#include <rime_api.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <unistd.h>

#include "interface.h"
#include "key_table.h"
#include "liberime-core.h"

#define XK_VoidSymbol 0xffffff

/**
 * Macro that defines a docstring for a function.
 * @param name The function name (without liberime_ prefix).
 * @param args The argument list as visible from Emacs (without parens).
 * @param docstring The rest of the documentation.
 */
#define DOCSTRING(name, args, docstring)                                       \
  const char *liberime_##name##__doc = (docstring "\n\n(fn " args ")")

#define DEFUN(ename, cname, min_nargs, max_nargs)                              \
  em_defun(env, (ename),                                                       \
           env->make_function(env, (min_nargs), (max_nargs), cname,            \
                              liberime_##cname##__doc, rime))

#define CONS_INT(key, integer)                                                 \
  em_cons(env, env->intern(env, key), env->make_integer(env, integer));
#define CONS_STRING(key, str)                                                  \
  em_cons(env, env->intern(env, key), env->make_string(env, str, strlen(str)))
#define CONS_NIL(key) em_cons(env, env->intern(env, key), em_nil)
#define CONS_VALUE(key, value) em_cons(env, env->intern(env, key), value)

#define CANDIDATE_MAXSTRLEN 1024
#define SCHEMA_MAXSTRLEN 1024
#define CONFIG_MAXSTRLEN 1024
#define INPUT_MAXSTRLEN 1024

#define NO_SESSION_ERR                                                         \
  "Cannot connect to librime session, make sure to run liberime-start first."

typedef struct _EmacsRime {
  RimeSessionId session_id;
  RimeApi *api;
  bool first_run;
} EmacsRime;

typedef struct _CandidateLinkedList {
  char *text;
  char *comment;
  struct _CandidateLinkedList *next;
} CandidateLinkedList;

typedef struct _EmacsRimeCandidates {
  size_t size;
  CandidateLinkedList *list;
} EmacsRimeCandidates;

typedef struct _CandidateExpansions {
  EmacsRimeCandidates full;
  EmacsRimeCandidates prefix;
  char *remainder;
} CandidateExpansions;

typedef struct _SessionStatus {
  char *schema_id;
  char *schema_name;
  bool is_disabled;
  bool is_composing;
  bool is_ascii_mode;
  bool is_full_shape;
  bool is_simplified;
  bool is_traditional;
  bool is_ascii_punct;
} SessionStatus;

void notification_handler(void *context, RimeSessionId session_id,
                          const char *message_type, const char *message_value) {
  /* EmacsRime *rime = (EmacsRime*) context; */
  /* emacs_env *env = rime->EmacsEnv; */
  /* char format[] = "[liberime] %s: %s"; */
  /* emacs_value args[3]; */
  /* args[0] = env->make_string(env, format, strnlen(format, SCHEMA_MAXSTRLEN));
   */
  /* args[1] = env->make_string(env, message_type, strnlen(message_type,
   * SCHEMA_MAXSTRLEN)); */
  /* args[2] = env->make_string(env, message_value, strnlen(message_value,
   * SCHEMA_MAXSTRLEN)); */
  /* env->funcall(env, env->intern (env, "message"), 3, args); */
}

// make sure session exists before operation
static bool _ensure_session(EmacsRime *rime) {
  if (!rime->api->find_session(rime->session_id)) {
    rime->session_id = rime->api->create_session();
    if (!rime->session_id) {
      // printf("cannot create rime session\n");
      return false;
    }
  }
  return true;
}

// Resolve an optional SESSION argument.  When SESSION_ARG is nil, the
// default session is used (creating it if necessary); otherwise the given
// session id must refer to a live session created by
// `liberime-session-create'.  On failure (*ok == false) an error has been
// signalled and the caller should return em_nil after releasing its own
// resources.
static RimeSessionId _resolve_session(emacs_env *env, EmacsRime *rime,
                                      emacs_value session_arg, bool *ok) {
  *ok = false;
  if (env->is_not_nil(env, session_arg)) {
    RimeSessionId id = (RimeSessionId)env->extract_integer(env, session_arg);
    if (!rime->api->find_session(id)) {
      em_signal_rimeerr(env, 1, NO_SESSION_ERR);
      return 0;
    }
    *ok = true;
    return id;
  }
  if (_ensure_session(rime)) {
    *ok = true;
    return rime->session_id;
  }
  em_signal_rimeerr(env, 1, NO_SESSION_ERR);
  return 0;
}

static char *_copy_string(const char *str) {
  if (str) {
    size_t size = strnlen(str, CANDIDATE_MAXSTRLEN);
    char *new_str = malloc(size + 1);
    strncpy(new_str, str, size);
    new_str[size] = '\0';
    return new_str;
  } else {
    return NULL;
  }
}

static void _candidates_append(EmacsRimeCandidates *cands,
                               CandidateLinkedList **tail, const char *text,
                               const char *comment) {
  CandidateLinkedList *node =
      (CandidateLinkedList *)malloc(sizeof(CandidateLinkedList));
  node->text = _copy_string(text);
  node->comment = _copy_string(comment);
  node->next = NULL;
  if (*tail) {
    (*tail)->next = node;
  } else {
    cands->list = node;
  }
  *tail = node;
  cands->size += 1;
}

EmacsRimeCandidates _get_candidates(EmacsRime *rime, RimeSessionId session_id,
                                    size_t index, size_t limit) {
  // calloc: the dummy head (and each node allocated inside the loop)
  // must have NULL text/comment so `free_candidate_list' can release
  // them even when the node was never filled (e.g. no candidates).
  EmacsRimeCandidates c = {
      .size = 0,
      .list = (CandidateLinkedList *)calloc(1, sizeof(CandidateLinkedList))};

  RimeCandidateListIterator iterator = {0};
  CandidateLinkedList *next = c.list;
  if (rime->api->candidate_list_from_index(session_id, &iterator, index)) {
    while (rime->api->candidate_list_next(&iterator)) {
      // If limit is set and we've reached the limit, stop
      if (limit > 0 && c.size >= limit) {
        break;
      }

      c.size += 1;

      next->text = _copy_string(iterator.candidate.text);
      next->comment = _copy_string(iterator.candidate.comment);

      next->next =
          (CandidateLinkedList *)calloc(1, sizeof(CandidateLinkedList));

      next = next->next;
    }
    next->next = NULL;
    rime->api->candidate_list_end(&iterator);
  }

  return c;
}

// bindings
DOCSTRING(start, "SHARED_DATA_DIR USER_DATA_DIR", "Start a rime session.");
static emacs_value start(emacs_env *env, ptrdiff_t nargs, emacs_value args[],
                         void *data) {
  EmacsRime *rime = (EmacsRime *)data;

  char *shared_data_dir = em_get_string(env, em_expand_file_name(env, args[0]));
  char *user_data_dir = em_get_string(env, em_expand_file_name(env, args[1]));

  RIME_STRUCT(RimeTraits, emacs_rime_traits);

  emacs_rime_traits.shared_data_dir = shared_data_dir;
  emacs_rime_traits.app_name = "rime.emacs-liberime";
  emacs_rime_traits.user_data_dir = user_data_dir;
  emacs_rime_traits.distribution_name = "Rime";
  emacs_rime_traits.distribution_code_name = "emacs-liberime";
  emacs_rime_traits.distribution_version = "0.1.0";
  if (rime->first_run) {
    rime->api->setup(&emacs_rime_traits);
    rime->first_run = false;
  }

  rime->api->initialize(&emacs_rime_traits);
  rime->api->set_notification_handler(notification_handler, rime);
  rime->api->start_maintenance(true);

  // wait for deploy
  rime->api->join_maintenance_thread();

  rime->session_id = rime->api->create_session();

  // Free allocated strings
  free(shared_data_dir);
  free(user_data_dir);

  return em_t;
}

DOCSTRING(finalize, "", "Finalize librime for redeploy.");
static emacs_value finalize(emacs_env *env, ptrdiff_t nargs, emacs_value args[],
                            void *data) {
  EmacsRime *rime = (EmacsRime *)data;
  if (rime->session_id) {
    rime->session_id = 0;
  }
  rime->api->finalize();
  return em_t;
}

void free_candidate_list(CandidateLinkedList *list) {
  CandidateLinkedList *next = list;
  while (next) {
    CandidateLinkedList *temp = next;
    next = temp->next;
    // `env->make_string' copies its input (see module_make_string in
    // Emacs's emacs-module.c), so the copies made by `_copy_string' can
    // safely be released here.
    free(temp->text);
    free(temp->comment);
    free(temp);
  }
}

/**
 * Build emacs list from candidates.
 */
static emacs_value _build_candidate_list(emacs_env *env,
                                         EmacsRimeCandidates candidates) {
  if (candidates.size == 0) {
    return em_nil;
  }

  emacs_value *array = malloc(sizeof(emacs_value) * candidates.size);
  CandidateLinkedList *next = candidates.list;
  int i = 0;
  while (next && i < candidates.size) {
    emacs_value value = env->make_string(env, next->text, strlen(next->text));
    if (next->comment) {
      emacs_value comment =
          env->make_string(env, next->comment, strlen(next->comment));
      value = em_propertize(env, value, ":comment", comment);
    }
    array[i++] = value;
    next = next->next;
  }

  emacs_value result = em_list(env, candidates.size, array);
  free(array);
  return result;
}

// Select SCHEMA_ID on a temporary SESSION for search.
//
// Selecting a schema persists var/previously_selected_schema and
// var/schema_access_time/<schema_id> (see Switcher::SetActiveSchema), which
// changes the schema of sessions created afterwards.  Save both values and
// restore them afterwards so that a temporary session leaves no trace in the
// user config.  Return false when the schema is unknown or selection fails.
static bool _select_temporary_schema(EmacsRime *rime,
                                     RimeSessionId session_id,
                                     const char *schema_id) {
  // Fast path: the session may already run the wanted schema.  Callers
  // reusing a persistent session hit this on every lookup after the
  // first.  select_schema rebuilds the whole engine pipeline and
  // persists user.yaml (Switcher::SetActiveSchema saves unconditionally)
  // even when the schema is unchanged, so the get_status round trip is
  // well spent.
  {
    RIME_STRUCT(RimeStatus, status);
    if (rime->api->get_status(session_id, &status)) {
      bool same = status.schema_id && strcmp(status.schema_id, schema_id) == 0;
      rime->api->free_status(&status);
      if (same) {
        return true;
      }
    }
  }
  char previous_schema[SCHEMA_MAXSTRLEN] = "";
  bool has_previous = false;
  int access_time = 0;
  bool has_access_time = false;
  char access_key[SCHEMA_MAXSTRLEN + 32];
  snprintf(access_key, sizeof(access_key), "var/schema_access_time/%s",
           schema_id);
  RimeConfig *user_cfg = malloc(sizeof(RimeConfig));
  if (rime->api->user_config_open("user", user_cfg)) {
    const char *previous = rime->api->config_get_cstring(
        user_cfg, "var/previously_selected_schema");
    if (previous && previous[0]) {
      strncpy(previous_schema, previous, SCHEMA_MAXSTRLEN - 1);
      previous_schema[SCHEMA_MAXSTRLEN - 1] = '\0';
      has_previous = true;
    }
    has_access_time =
        rime->api->config_get_int(user_cfg, access_key, &access_time);
    rime->api->config_close(user_cfg);
  }
  free(user_cfg);

  bool schema_found = false;
  RimeSchemaList schema_list;
  if (rime->api->get_schema_list(&schema_list)) {
    for (int i = 0; i < schema_list.size; i++) {
      if (strcmp(schema_list.list[i].schema_id, schema_id) == 0) {
        schema_found = true;
        break;
      }
    }
    rime->api->free_schema_list(&schema_list);
  }
  bool selected =
      schema_found && rime->api->select_schema(session_id, schema_id);

  // Restore the persisted values.  This runs even when SELECTED is false
  // as a defence in depth: librime may still have written the variables
  // internally before reporting failure.
  RimeConfig *restore_cfg = malloc(sizeof(RimeConfig));
  if (rime->api->user_config_open("user", restore_cfg)) {
    rime->api->config_set_string(restore_cfg,
                                 "var/previously_selected_schema",
                                 has_previous ? previous_schema : "");
    if (has_access_time) {
      rime->api->config_set_int(restore_cfg, access_key, access_time);
    } else {
      rime->api->config_clear(restore_cfg, access_key);
    }
    rime->api->config_close(restore_cfg);
  }
  free(restore_cfg);

  return selected;
}

// Walk the highlighted states of SESSION with XK_Down, collecting
// candidates that consume the complete input into EXPANSIONS->full and
// those that consume the shortest non-empty prefix into EXPANSIONS->prefix,
// storing the unconsumed suffix in EXPANSIONS->remainder.  At most LIMIT
// states are examined when LIMIT is non-zero.
static void _collect_candidate_expansions(EmacsRime *rime,
                                          RimeSessionId session_id,
                                          size_t input_end, size_t limit,
                                          CandidateExpansions *expansions) {
  CandidateLinkedList *full_tail = NULL;
  CandidateLinkedList *prefix_tail = NULL;
  size_t examined = 0;
  // One entry per examined state: the walk stops after LIMIT states
  // (or 4096 when LIMIT is zero).
  size_t seen_cap = (limit > 0 && limit < 4096) ? limit : 4096;
  size_t(*seen)[2] = malloc(sizeof(size_t[2]) * seen_cap);
  size_t seen_count = 0;
  if (!seen) {
    return;
  }

  for (;;) {
    RIME_STRUCT(RimeContext, ctx);
    if (!rime->api->get_context(session_id, &ctx)) {
      break;
    }
    int hindex = ctx.menu.highlighted_candidate_index;
    int page = ctx.menu.page_no;
    if (hindex < 0 || hindex >= ctx.menu.num_candidates) {
      rime->api->free_context(&ctx);
      break;
    }
    bool dup = false;
    for (size_t k = 0; k < seen_count; k++) {
      if (seen[k][0] == (size_t)page && seen[k][1] == (size_t)hindex) {
        dup = true;
        break;
      }
    }
    if (dup || seen_count >= seen_cap) {
      rime->api->free_context(&ctx);
      break;
    }
    seen[seen_count][0] = (size_t)page;
    seen[seen_count][1] = (size_t)hindex;
    seen_count++;
    RimeCandidate *candidate = &ctx.menu.candidates[hindex];
    size_t sel_end = ctx.composition.sel_end;
    char *preedit = ctx.composition.preedit;
    if (sel_end == input_end) {
      _candidates_append(&expansions->full, &full_tail, candidate->text,
                         candidate->comment);
    } else if (preedit && sel_end < strlen(preedit)) {
      const char *rest = preedit + sel_end;
      if (rest[0] != '\0') {
        if (expansions->remainder == NULL ||
            strlen(rest) > strlen(expansions->remainder)) {
          free_candidate_list(expansions->prefix.list);
          expansions->prefix.size = 0;
          prefix_tail = NULL;
          expansions->remainder = _copy_string(rest);
          _candidates_append(&expansions->prefix, &prefix_tail,
                             candidate->text, candidate->comment);
        } else if (strcmp(rest, expansions->remainder) == 0) {
          _candidates_append(&expansions->prefix, &prefix_tail,
                             candidate->text, candidate->comment);
        }
      }
    }
    rime->api->free_context(&ctx);
    examined++;
    if (limit > 0 && examined >= limit) {
      break;
    }
    // XK_Down moves the highlight, including across menu pages.
    if (!rime->api->process_key(session_id, 0xff54, 0)) {
      break;
    }
  }
  free(seen);
}

// Read the status of SESSION into STATUS.  The returned strings are
// heap-allocated; free them with `_free_session_status'.
static void _get_session_status(EmacsRime *rime, RimeSessionId session_id,
                                SessionStatus *status) {
  memset(status, 0, sizeof(*status));
  RIME_STRUCT(RimeStatus, rime_status);
  if (rime->api->get_status(session_id, &rime_status)) {
    status->schema_id = _copy_string(rime_status.schema_id);
    status->schema_name = _copy_string(rime_status.schema_name);
    status->is_disabled = rime_status.is_disabled;
    status->is_composing = rime_status.is_composing;
    status->is_ascii_mode = rime_status.is_ascii_mode;
    status->is_full_shape = rime_status.is_full_shape;
    status->is_simplified = rime_status.is_simplified;
    status->is_traditional = rime_status.is_traditional;
    status->is_ascii_punct = rime_status.is_ascii_punct;
  }
}

static void _free_session_status(SessionStatus *status) {
  free(status->schema_id);
  free(status->schema_name);
  memset(status, 0, sizeof(*status));
}

static void _plist_push(emacs_env *env, emacs_value plist[], int *pi,
                        const char *key, emacs_value value) {
  plist[(*pi)++] = env->intern(env, key);
  plist[(*pi)++] = value;
}

DOCSTRING(
    search, "STRING &optional LIMIT INDEX SCHEMA_ID FULL-CONTEXT SESSION",
    "Input STRING and return LIMIT number candidates starting from INDEX.\n"
    "When LIMIT is nil, return all candidates from INDEX.\n"
    "When INDEX is nil, start from 0.\n"
    "SCHEMA_ID, when non-nil, searches using the given schema.\n"
    "It only affects the temporary session, so the schema of the\n"
    "default session and global state are unchanged.\n"
    "This function always uses a separate session to avoid\n"
    "interfering with current input.\n"
    "When FULL-CONTEXT is non-nil, INDEX is ignored and the return\n"
    "value is a plist describing how candidates consume STR:\n"
    "\n"
    "  :commit          Text Rime committed automatically.  Shape-based\n"
    "                   schemas push out a completed word when the code\n"
    "                   grows too long, and that word no longer appears\n"
    "                   in the candidate menu; nil when nothing was\n"
    "                   committed.\n"
    "  :full            Candidates consuming STR completely, such as the\n"
    "                   word candidate of a complete code.\n"
    "  :prefix          Candidates consuming only the shortest non-empty\n"
    "                   prefix of STR.  Pinyin schemas offer single-\n"
    "                   character candidates while later syllables are\n"
    "                   still being typed, so these consume only part of\n"
    "                   the code.\n"
    "  :remainder       The code suffix left after the shortest prefix,\n"
    "                   i.e. the part of STR that :prefix candidates did\n"
    "                   not consume, ready for recursive expansion.\n"
    "  :remaining-input The full original input STR, as returned by\n"
    "                   get_input.  Unlike :remainder it is not tied to\n"
    "                   any prefix candidate; use it to decide whether\n"
    "                   :commit really consumed everything: a commit\n"
    "                   followed by unconsumed input and no candidates is\n"
    "                   only an automatically committed prefix.\n"
    "  :schema-id       The schema the temporary session actually used,\n"
    "                   together with :schema-name and the option flags\n"
    "                   :is-disabled, :is-composing, :is-ascii-mode,\n"
    "                   :is-full-shape, :is-simplified, :is-traditional\n"
    "                   and :is-ascii-punct.  This lets callers key caches\n"
    "                   on the state the candidates were produced with.\n"
    "\n"
    "For example, for STR \"nihaoshijie\" under a pinyin schema, :full\n"
    "could be a whole-phrase candidate while :prefix contains single-\n"
    "character candidates that consume only \"ni\", with :remainder\n"
    "\"haoshijie\".  LIMIT still bounds how many highlighted states are\n"
    "examined.\n"
    "SESSION, when non-nil, is a session id from `liberime-session-create'\n"
    "to search in; the session is reused instead of creating and destroying\n"
    "a temporary one.");
static emacs_value search(emacs_env *env, ptrdiff_t nargs, emacs_value args[],
                          void *data) {
  EmacsRime *rime = (EmacsRime *)data;
  char *string = em_get_string(env, args[0]);

  size_t limit = 0;
  if (nargs >= 2 && env->is_not_nil(env, args[1])) {
    limit = env->extract_integer(env, args[1]);
    // if limit set to 0 return nil immediately
    if (limit == 0) {
      free(string);
      return em_nil;
    }
  }

  size_t index = 0;
  if (nargs >= 3 && env->is_not_nil(env, args[2])) {
    index = env->extract_integer(env, args[2]);
  }

  char *schema_id = NULL;
  if (nargs >= 4 && env->is_not_nil(env, args[3])) {
    schema_id = em_get_string(env, args[3]);
  }

  bool full_context = false;
  if (nargs >= 5 && env->is_not_nil(env, args[4])) {
    full_context = true;
  }

  // When SESSION is given, reuse that session instead of creating and
  // destroying a temporary one.  The caller owns its lifetime.
  bool reuse_session = false;
  RimeSessionId session_id = 0;
  if (nargs >= 6 && env->is_not_nil(env, args[5])) {
    session_id = (RimeSessionId)env->extract_integer(env, args[5]);
    if (!rime->api->find_session(session_id)) {
      em_signal_rimeerr(env, 1, NO_SESSION_ERR);
      free(string);
      free(schema_id);
      return em_nil;
    }
    reuse_session = true;
  } else {
    // Always create a new session for search to avoid interfering with
    // the default session
    session_id = rime->api->create_session();
    if (!session_id) {
      em_signal_rimeerr(env, 1, "Cannot create session.");
      free(string);
      free(schema_id);
      return em_nil;
    }
  }

  if (schema_id && !_select_temporary_schema(rime, session_id, schema_id)) {
    free(schema_id);
    free(string);
    if (!reuse_session) {
      rime->api->destroy_session(session_id);
    }
    em_signal_rimeerr(env, 1, "Failed to select schema.");
    return em_nil;
  }
  free(schema_id);

  rime->api->clear_composition(session_id);
  rime->api->simulate_key_sequence(session_id, string);

  emacs_value result;
  if (full_context) {
    // Collect commit, full and shortest-prefix candidates plus the
    // unconsumed remainder by walking the highlighted states.
    char *commit_text = NULL;
    RIME_STRUCT(RimeCommit, commit);
    if (rime->api->get_commit(session_id, &commit)) {
      commit_text = _copy_string(commit.text);
      rime->api->free_commit(&commit);
    }

    size_t input_end = 0;
    RIME_STRUCT(RimeContext, ctx);
    if (rime->api->get_context(session_id, &ctx)) {
      input_end = ctx.composition.length;
      rime->api->free_context(&ctx);
    }

    CandidateExpansions expansions;
    memset(&expansions, 0, sizeof(expansions));
    _collect_candidate_expansions(rime, session_id, input_end, limit,
                                  &expansions);

    const char *remaining_input = rime->api->get_input(session_id);

    SessionStatus status;
    _get_session_status(rime, session_id, &status);

    // 5 plist pairs (:commit :full :prefix :remainder :remaining-input)
    // plus 9 status pairs, 28 slots in total; 32 leaves headroom.
    emacs_value plist[32];
    int pi = 0;
    _plist_push(env, plist, &pi, ":commit",
                commit_text
                    ? env->make_string(env, commit_text, strlen(commit_text))
                    : em_nil);
    _plist_push(env, plist, &pi, ":full",
                _build_candidate_list(env, expansions.full));
    _plist_push(env, plist, &pi, ":prefix",
                _build_candidate_list(env, expansions.prefix));
    _plist_push(env, plist, &pi, ":remainder",
                expansions.remainder
                    ? env->make_string(env, expansions.remainder,
                                       strlen(expansions.remainder))
                    : em_nil);
    _plist_push(env, plist, &pi, ":remaining-input",
                remaining_input
                    ? env->make_string(env, remaining_input,
                                       strlen(remaining_input))
                    : em_nil);
    _plist_push(env, plist, &pi, ":schema-id",
                status.schema_id
                    ? env->make_string(env, status.schema_id,
                                       strlen(status.schema_id))
                    : em_nil);
    _plist_push(env, plist, &pi, ":schema-name",
                status.schema_name
                    ? env->make_string(env, status.schema_name,
                                       strlen(status.schema_name))
                    : em_nil);
    _plist_push(env, plist, &pi, ":is-disabled",
                status.is_disabled ? em_t : em_nil);
    _plist_push(env, plist, &pi, ":is-composing",
                status.is_composing ? em_t : em_nil);
    _plist_push(env, plist, &pi, ":is-ascii-mode",
                status.is_ascii_mode ? em_t : em_nil);
    _plist_push(env, plist, &pi, ":is-full-shape",
                status.is_full_shape ? em_t : em_nil);
    _plist_push(env, plist, &pi, ":is-simplified",
                status.is_simplified ? em_t : em_nil);
    _plist_push(env, plist, &pi, ":is-traditional",
                status.is_traditional ? em_t : em_nil);
    _plist_push(env, plist, &pi, ":is-ascii-punct",
                status.is_ascii_punct ? em_t : em_nil);
    result = em_list(env, pi, plist);

    free_candidate_list(expansions.full.list);
    free_candidate_list(expansions.prefix.list);
    free(commit_text);
    free(expansions.remainder);
    _free_session_status(&status);
  } else {
    EmacsRimeCandidates candidates = _get_candidates(rime, session_id, index, limit);
    result = _build_candidate_list(env, candidates);
    free_candidate_list(candidates.list);
  }

  free(string);

  if (!reuse_session) {
    // Destroy the temporary session
    rime->api->destroy_session(session_id);
  }

  return result;
}
DOCSTRING(session_create, "&optional SCHEMA_ID",
          "Create a new temporary rime session and return its id.\n"
          "The session is independent of the default session and can be\n"
          "used with any liberime function that accepts a SESSION argument.\n"
          "Destroy it with `liberime-session-destroy'; stale sessions are\n"
          "also recycled by librime after five minutes of inactivity.\n"
          "SCHEMA_ID, when non-nil, selects that schema for the session\n"
          "without leaving any trace in the user config.");
static emacs_value session_create(emacs_env *env, ptrdiff_t nargs,
                                  emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;

  char *schema_id = NULL;
  if (nargs >= 1 && env->is_not_nil(env, args[0])) {
    schema_id = em_get_string(env, args[0]);
  }

  RimeSessionId session_id = rime->api->create_session();
  if (!session_id) {
    em_signal_rimeerr(env, 1, "Cannot create session.");
    free(schema_id);
    return em_nil;
  }

  if (schema_id && !_select_temporary_schema(rime, session_id, schema_id)) {
    free(schema_id);
    rime->api->destroy_session(session_id);
    em_signal_rimeerr(env, 1, "Failed to select schema.");
    return em_nil;
  }
  free(schema_id);

  return env->make_integer(env, (intmax_t)session_id);
}

DOCSTRING(session_destroy, "SESSION",
          "Destroy a session created by `liberime-session-create'.");
static emacs_value session_destroy(emacs_env *env, ptrdiff_t nargs,
                                   emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;

  RimeSessionId session_id = (RimeSessionId)env->extract_integer(env, args[0]);
  if (rime->api->destroy_session(session_id)) {
    return em_t;
  }
  return em_nil;
}


DOCSTRING(
    get_candidates, "&optional LIMIT INDEX SESSION",
    "Get current candidates from a rime session.\n"
    "LIMIT is max candidates to return, default all.\n"
    "INDEX is the starting position (0-based), default 0.\n"
    "SESSION is a session id from `liberime-session-create'; nil uses the\n"
    "default session.  Unlike search, this does NOT clear composition or\n"
    "simulate key sequence.");
static emacs_value get_candidates(emacs_env *env, ptrdiff_t nargs,
                                  emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;

  bool session_ok;
  RimeSessionId session_id = _resolve_session(
      env, rime, nargs >= 3 ? args[2] : em_nil, &session_ok);
  if (!session_ok) {
    return em_nil;
  }

  size_t limit = 0;
  if (nargs >= 1 && env->is_not_nil(env, args[0])) {
    limit = env->extract_integer(env, args[0]);
    if (limit == 0) {
      return em_nil;
    }
  }

  size_t index = 0;
  if (nargs >= 2 && env->is_not_nil(env, args[1])) {
    index = env->extract_integer(env, args[1]);
  }

  EmacsRimeCandidates candidates = _get_candidates(rime, session_id, index, limit);

  emacs_value result = _build_candidate_list(env, candidates);
  free_candidate_list(candidates.list);

  return result;
}

DOCSTRING(get_sync_dir, "", "Get rime sync directory.");
static emacs_value get_sync_dir(emacs_env *env, ptrdiff_t nargs,
                                emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;
  if (!_ensure_session(rime)) {
    em_signal_rimeerr(env, 1, NO_SESSION_ERR);
    return em_nil;
  }

  const char *sync_dir = rime->api->get_sync_dir();
  return env->make_string(env, sync_dir, strlen(sync_dir));
}

DOCSTRING(sync_user_data, "", "Sync rime user data.");
static emacs_value sync_user_data(emacs_env *env, ptrdiff_t nargs,
                                  emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;
  if (!_ensure_session(rime)) {
    em_signal_rimeerr(env, 1, NO_SESSION_ERR);
    return em_nil;
  }

  bool result = rime->api->sync_user_data();
  return result ? em_t : em_nil;
}

DOCSTRING(get_schema_list, "", "List all rime schema.");
static emacs_value get_schema_list(emacs_env *env, ptrdiff_t nargs,
                                   emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;
  if (!_ensure_session(rime)) {
    em_signal_rimeerr(env, 1, NO_SESSION_ERR);
    return em_nil;
  }

  RimeSchemaList schema_list;
  if (!rime->api->get_schema_list(&schema_list)) {
    em_signal_rimeerr(env, 1, "Get schema list form librime failed.");
    return em_nil;
  }

  emacs_value flist = env->intern(env, "list");
  emacs_value array[schema_list.size];
  for (int i = 0; i < schema_list.size; i++) {
    RimeSchemaListItem item = schema_list.list[i];
    emacs_value pair[2];
    pair[0] = env->make_string(env, item.schema_id,
                               strnlen(item.schema_id, SCHEMA_MAXSTRLEN));
    pair[1] =
        env->make_string(env, item.name, strnlen(item.name, SCHEMA_MAXSTRLEN));

    array[i] = env->funcall(env, flist, 2, pair);
  }

  emacs_value result = env->funcall(env, flist, schema_list.size, array);

  rime->api->free_schema_list(&schema_list);

  return result;
}

DOCSTRING(
    select_schema, "SCHEMA_ID",
    "Select a rime schema.\n"
    "SCHENA_ID should be a value returned from `liberime-get-schema-list'.");
static emacs_value select_schema(emacs_env *env, ptrdiff_t nargs,
                                 emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;
  const char *schema_id = em_get_string(env, args[0]);

  if (!_ensure_session(rime)) {
    em_signal_rimeerr(env, 1, NO_SESSION_ERR);
    free((char *)schema_id);
    return em_nil;
  }

  RimeSchemaList schema_list;
  if (!rime->api->get_schema_list(&schema_list)) {
    em_signal_rimeerr(env, 1, "Get schema list from librime failed.");
    free((char *)schema_id);
    return em_nil;
  }

  bool found = false;
  for (int i = 0; i < schema_list.size; i++) {
    if (strcmp(schema_list.list[i].schema_id, schema_id) == 0) {
      found = true;
      break;
    }
  }
  rime->api->free_schema_list(&schema_list);

  if (!found) {
    free((char *)schema_id);
    return em_nil;
  }

  if (rime->api->select_schema(rime->session_id, schema_id)) {
    free((char *)schema_id);
    return em_t;
  }
  free((char *)schema_id);
  return em_nil;
}

// input
DOCSTRING(process_key, "KEYCODE &optional MASK SESSION",
          "Send KEYCODE to rime session and process it.\n"
          "SESSION is a session id from `liberime-session-create'; nil\n"
          "uses the default session.");
static emacs_value process_key(emacs_env *env, ptrdiff_t nargs,
                               emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;

  int keycode = env->extract_integer(env, args[0]);
  int mask = 0;
  if (nargs >= 2 && env->is_not_nil(env, args[1])) {
    mask = env->extract_integer(env, args[1]);
  }

  bool session_ok;
  RimeSessionId session_id =
      _resolve_session(env, rime, nargs >= 3 ? args[2] : em_nil, &session_ok);
  if (!session_ok) {
    return em_nil;
  }

  if (rime->api->process_key(session_id, keycode, mask)) {
    return em_t;
  }
  return em_nil;
}

DOCSTRING(
    simulate_key_sequence, "STRING &optional SESSION",
    "Simulate a key sequence STRING to rime session.\n"
    "STRING follows librime's KeySequence format:\n"
    "  - Plain ASCII chars (except '{', '}'): e.g. \"a\", \"1\", \" \"\n"
    "  - Named keys in braces: \"{Left}\", \"{Return}\", \"{F1}\"\n"
    "  - With modifiers: \"{Control+a}\", \"{Shift+space}\", \"{Meta+F1}\"\n"
    "  - Multiple modifiers: \"{Control+Alt+Return}\"\n"
    "  - Braces themselves: \"{braceleft}\", \"{braceright}\"\n"
    "Multiple keys are concatenated: \"abc{space}{Return}\"\n"
    "See also `liberime-kbd-to-key-sequence'.");
static emacs_value simulate_key_sequence(emacs_env *env, ptrdiff_t nargs,
                                         emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;
  char *string = em_get_string(env, args[0]);

  bool session_ok;
  RimeSessionId session_id =
      _resolve_session(env, rime, nargs >= 2 ? args[1] : em_nil, &session_ok);
  if (!session_ok) {
    free(string);
    return em_nil;
  }

  rime->api->simulate_key_sequence(session_id, string);
  free(string);
  return em_t;
}

/* Convert Emacs EVENT to librime key sequence string.
   Returns a newly allocated string (caller must free), or NULL on failure. */
static char *_event_to_key_sequence(emacs_env *env, emacs_value event) {
  char result[256];
  result[0] = '\0';

  emacs_value type = env->type_of(env, event);

  if (env->eq(env, type, env->intern(env, "integer"))) {
    int ev = (int)env->extract_integer(env, event);
    if (emacs_int_event_to_key_sequence(ev, result, sizeof(result)) != 0) {
      return NULL;
    }
  } else if (env->eq(env, type, env->intern(env, "symbol"))) {
    /* Symbol event (function key like 'left, 'return, 'F1) */
    emacs_value symbol_name_result =
        env->funcall(env, env->intern(env, "symbol-name"), 1, &event);
    char *sym_name = em_get_string(env, symbol_name_result);

    if (sym_name) {
      if (emacs_symbol_to_key_sequence(sym_name, result, sizeof(result)) != 0) {
        free(sym_name);
        return NULL;
      }
      free(sym_name);
    } else {
      return NULL;
    }
  } else {
    /* Unsupported type */
    return NULL;
  }

  char *out = malloc(strlen(result) + 1);
  if (out) {
    strcpy(out, result);
  }
  return out;
}

DOCSTRING(liberime_event_to_key_sequence, "EVENT",
          "Convert Emacs EVENT to librime key sequence string.\n"
          "EVENT can be an integer (character with optional modifiers) or a "
          "symbol (function key like 'left, 'F1).\n"
          "Returns string like \"a\", \"{Control+a}\", \"{Left}\", "
          "\"{Control+Left}\".");
static emacs_value liberime_event_to_key_sequence(emacs_env *env,
                                                  ptrdiff_t nargs,
                                                  emacs_value args[],
                                                  void *data) {
  char *key_seq = _event_to_key_sequence(env, args[0]);
  if (!key_seq) {
    return em_nil;
  }
  emacs_value result = env->make_string(env, key_seq, strlen(key_seq));
  free(key_seq);
  return result;
}

DOCSTRING(liberime_process_event, "EVENT &optional SESSION",
          "Process Emacs EVENT by converting to key sequence and sending to "
          "librime.\n"
          "EVENT can be an integer (character with optional modifiers) or a "
          "symbol (function key).");
static emacs_value liberime_process_event(emacs_env *env, ptrdiff_t nargs,
                                          emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;

  char *key_seq = _event_to_key_sequence(env, args[0]);
  if (!key_seq) {
    return em_nil;
  }

  bool session_ok;
  RimeSessionId session_id =
      _resolve_session(env, rime, nargs >= 2 ? args[1] : em_nil, &session_ok);
  if (!session_ok) {
    free(key_seq);
    return em_nil;
  }

  bool success = rime->api->simulate_key_sequence(session_id, key_seq);
  free(key_seq);
  return success ? em_t : em_nil;
}

DOCSTRING(get_input, "&optional SESSION", "Get rime input.");
static emacs_value get_input(emacs_env *env, ptrdiff_t nargs,
                             emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;

  bool session_ok;
  RimeSessionId session_id =
      _resolve_session(env, rime, nargs >= 1 ? args[0] : em_nil, &session_ok);
  if (!session_ok) {
    return em_nil;
  }

  const char *input = rime->api->get_input(session_id);

  if (!input) {
    return em_nil;
  } else {
    return env->make_string(env, input, strnlen(input, INPUT_MAXSTRLEN));
  }
}

DOCSTRING(commit_composition, "&optional SESSION",
          "Commit rime composition.");
static emacs_value commit_composition(emacs_env *env, ptrdiff_t nargs,
                                      emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;

  bool session_ok;
  RimeSessionId session_id =
      _resolve_session(env, rime, nargs >= 1 ? args[0] : em_nil, &session_ok);
  if (!session_ok) {
    return em_nil;
  }

  if (rime->api->commit_composition(session_id)) {
    return em_t;
  }
  return em_nil;
}

DOCSTRING(clear_composition, "&optional SESSION", "Clear rime composition.");
static emacs_value clear_composition(emacs_env *env, ptrdiff_t nargs,
                                     emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;

  bool session_ok;
  RimeSessionId session_id =
      _resolve_session(env, rime, nargs >= 1 ? args[0] : em_nil, &session_ok);
  if (!session_ok) {
    return em_nil;
  }

  rime->api->clear_composition(session_id);
  return em_t;
}

DOCSTRING(select_candidate, "NUM &optional SESSION",
          "Select a rime candidate by NUM.");
static emacs_value select_candidate(emacs_env *env, ptrdiff_t nargs,
                                    emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;

  int index = env->extract_integer(env, args[0]);

  bool session_ok;
  RimeSessionId session_id =
      _resolve_session(env, rime, nargs >= 2 ? args[1] : em_nil, &session_ok);
  if (!session_ok) {
    return em_nil;
  }

  if (rime->api->select_candidate_on_current_page(session_id, index)) {
    return em_t;
  }
  return em_nil;
}

// output

DOCSTRING(get_commit, "&optional SESSION", "Get rime commit.");
static emacs_value get_commit(emacs_env *env, ptrdiff_t nargs,
                              emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;

  bool session_ok;
  RimeSessionId session_id =
      _resolve_session(env, rime, nargs >= 1 ? args[0] : em_nil, &session_ok);
  if (!session_ok) {
    return em_nil;
  }

  RIME_STRUCT(RimeCommit, commit);
  if (rime->api->get_commit(session_id, &commit)) {
    if (!commit.text) {
      return em_nil;
    }

    char *commit_str = _copy_string(commit.text);
    rime->api->free_commit(&commit);
    // printf("commit str is %s\n", commit_str);

    emacs_value result = env->make_string(env, commit_str, strlen(commit_str));
    free(commit_str);
    return result;
  }

  return em_nil;
}

DOCSTRING(get_context, "&optional SESSION", "Get rime context.");
static emacs_value get_context(emacs_env *env, ptrdiff_t nargs,
                               emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;

  bool session_ok;
  RimeSessionId session_id =
      _resolve_session(env, rime, nargs >= 1 ? args[0] : em_nil, &session_ok);
  if (!session_ok) {
    return em_nil;
  }

  RIME_STRUCT(RimeContext, context);
  if (!rime->api->get_context(session_id, &context)) {
    em_signal_rimeerr(env, 2, "Cannot get context.");
    return em_nil;
  }

  size_t result_size = 3;
  emacs_value result_array[result_size];

  // 0. context.commit_text_preview
  char *ctp_str = _copy_string(context.commit_text_preview);
  if (ctp_str)
    result_array[0] = CONS_STRING("commit-text-preview", ctp_str);
  else
    result_array[0] = CONS_NIL("commit-text-preview");

  // 2. context.composition
  size_t composition_size = 5;
  emacs_value composition_array[composition_size];
  composition_array[0] = CONS_INT("length", context.composition.length);
  composition_array[1] = CONS_INT("cursor-pos", context.composition.cursor_pos);
  composition_array[2] = CONS_INT("sel-start", context.composition.sel_start);
  composition_array[3] = CONS_INT("sel-end", context.composition.sel_end);

  char *preedit_str = _copy_string(context.composition.preedit);
  if (preedit_str)
    composition_array[4] = CONS_STRING("preedit", preedit_str);
  else {
    free(ctp_str);
    free(preedit_str);
    rime->api->free_context(&context);
    return em_nil;
  }
  /* composition_array[4] = CONS_NIL("preedit"); */

  emacs_value composition_value =
      em_list(env, composition_size, composition_array);
  result_array[1] = CONS_VALUE("composition", composition_value);

  // 3. context.menu
  if (context.menu.num_candidates) {
    size_t menu_size = 6;
    emacs_value menu_array[menu_size];
    menu_array[0] = CONS_INT("highlighted-candidate-index",
                             context.menu.highlighted_candidate_index);
    menu_array[1] =
        CONS_VALUE("last-page-p", context.menu.is_last_page ? em_t : em_nil);
    menu_array[2] = CONS_INT("num-candidates", context.menu.num_candidates);
    menu_array[3] = CONS_INT("page-no", context.menu.page_no);
    menu_array[4] = CONS_INT("page-size", context.menu.page_size);
    emacs_value carray[context.menu.num_candidates];
    // Build candidates
    for (int i = 0; i < context.menu.num_candidates; i++) {
      RimeCandidate candidate = context.menu.candidates[i];

      emacs_value value = em_string(env, candidate.text);
      if (candidate.comment) {
        emacs_value comment = em_string(env, candidate.comment);
        value = em_propertize(env, value, ":comment", comment);
      }

      carray[i] = value;
    }

    emacs_value candidates = em_list(env, context.menu.num_candidates, carray);
    menu_array[5] = CONS_VALUE("candidates", candidates);
    emacs_value menu = em_list(env, menu_size, menu_array);
    result_array[2] = CONS_VALUE("menu", menu);
  } else {
    result_array[2] = CONS_NIL("menu");
  }

  // build result
  emacs_value result = em_list(env, result_size, result_array);

  rime->api->free_context(&context);
  free(ctp_str);
  free(preedit_str);

  return result;
}

DOCSTRING(get_status, "&optional SESSION", "Get rime status.");
static emacs_value get_status(emacs_env *env, ptrdiff_t nargs,
                              emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;

  bool session_ok;
  RimeSessionId session_id =
      _resolve_session(env, rime, nargs >= 1 ? args[0] : em_nil, &session_ok);
  if (!session_ok) {
    return em_nil;
  }

  RIME_STRUCT(RimeStatus, status);
  if (!rime->api->get_status(session_id, &status)) {
    em_signal_rimeerr(env, 2, "Cannot get status.");
    return em_nil;
  }

  size_t result_size = 9;
  emacs_value result_array[result_size];

  char *schema_id = _copy_string(status.schema_id);
  if (schema_id)
    result_array[0] = CONS_STRING("schema_id", schema_id);
  else
    result_array[0] = CONS_NIL("schema_id");

  char *schema_name = _copy_string(status.schema_name);
  if (schema_name)
    result_array[1] = CONS_STRING("schema_name", schema_name);
  else
    result_array[1] = CONS_NIL("schema_name");

  result_array[2] =
      CONS_VALUE("is_disabled", status.is_disabled ? em_t : em_nil);
  result_array[3] =
      CONS_VALUE("is_composing", status.is_composing ? em_t : em_nil);
  result_array[4] =
      CONS_VALUE("is_ascii_mode", status.is_ascii_mode ? em_t : em_nil);
  result_array[5] =
      CONS_VALUE("is_full_shape", status.is_full_shape ? em_t : em_nil);
  result_array[6] =
      CONS_VALUE("is_simplified", status.is_simplified ? em_t : em_nil);
  result_array[7] =
      CONS_VALUE("is_traditional", status.is_traditional ? em_t : em_nil);
  result_array[8] =
      CONS_VALUE("is_ascii_punct", status.is_ascii_punct ? em_t : em_nil);

  // build result
  emacs_value result = em_list(env, result_size, result_array);

  rime->api->free_status(&status);
  free(schema_id);
  free(schema_name);

  return result;
}

DOCSTRING(get_option, "OPTION &optional SESSION",
          "Get the boolean state of rime OPTION.\n"
          "OPTION is a rime option name like \"simplification\", "
          "\"ascii_mode\", \"full_shape\", \"ascii_punct\" or\n"
          "\"extended_charset\".  Returns t when the option is on, nil "
          "when it is\n"
          "off or unknown.  With SESSION, operate on that session "
          "instead of the\n"
          "default session.");
static emacs_value get_option(emacs_env *env, ptrdiff_t nargs,
                              emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;

  bool session_ok;
  RimeSessionId session_id =
      _resolve_session(env, rime, nargs >= 2 ? args[1] : em_nil, &session_ok);
  if (!session_ok) {
    return em_nil;
  }

  const char *option = em_get_string(env, args[0]);
  Bool value = rime->api->get_option(session_id, option);
  free((char *)option);

  return value ? em_t : em_nil;
}

DOCSTRING(get_state_label, "OPTION STATE &optional SESSION",
          "Get the active schema's display label for rime OPTION in "
          "STATE.\n"
          "OPTION is a rime option name like \"simplification\"; STATE "
          "non-nil\n"
          "selects the on-label, nil the off-label.  Returns the label "
          "string,\n"
          "or nil when the schema defines no such switch (labels are "
          "schema-driven).\n"
          "With SESSION, operate on that session instead of the default "
          "session.");
static emacs_value get_state_label(emacs_env *env, ptrdiff_t nargs,
                                   emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;

  bool session_ok;
  RimeSessionId session_id =
      _resolve_session(env, rime, nargs >= 3 ? args[2] : em_nil, &session_ok);
  if (!session_ok) {
    return em_nil;
  }

  const char *option = em_get_string(env, args[0]);
  Bool state = env->is_not_nil(env, args[1]) ? True : False;
  const char *label = rime->api->get_state_label(session_id, option, state);
  emacs_value result =
      label ? env->make_string(env, label, strlen(label)) : em_nil;
  free((char *)option);

  return result;
}

DOCSTRING(get_switches, "&optional SCHEMA",
          "List the switches defined by SCHEMA's (default: active) config.\n"
          "Returns one entry per switch: (NAME STATES RESET OPTIONS).\n"
          "NAME is the option name for a toggle switch, or the first option\n"
          "of a radio group.  STATES is the list of display labels\n"
          "(states[i] labels entry i), or nil when the switch defines none.\n"
          "RESET is the initial state index from the schema, or nil.\n"
          "OPTIONS is the list of option names for a radio group\n"
          "(mutually exclusive), or nil for a plain toggle.  Labels and the\n"
          "switch list are fully schema-driven.");
static emacs_value get_switches(emacs_env *env, ptrdiff_t nargs,
                                emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;

  if (!_ensure_session(rime)) {
    em_signal_rimeerr(env, 1, NO_SESSION_ERR);
    return em_nil;
  }

  const int max_schema_length = 0xff;
  char *schema_id = (char *)malloc(max_schema_length * sizeof(char));
  memset(schema_id, 0, max_schema_length);
  if (nargs >= 1 && env->is_not_nil(env, args[0])) {
    char *arg0 = em_get_string(env, args[0]);
    if (arg0 == NULL || strlen(arg0) == 0 ||
        strlen(arg0) > max_schema_length - 1) {
      em_signal_rimeerr(env, 2, "Invalid schema id.");
      free(arg0);
      free(schema_id);
      return em_nil;
    }
    strcpy(schema_id, arg0);
    free(arg0);
  } else if (!rime->api->get_current_schema(rime->session_id, schema_id,
                                            max_schema_length)) {
    em_signal_rimeerr(env, 2, "Error get current schema.");
    free(schema_id);
    return em_nil;
  }

  RimeConfig *config = (RimeConfig *)malloc(sizeof(RimeConfig));
  if (!rime->api->schema_open(schema_id, config)) {
    free(schema_id);
    free(config);
    em_signal_rimeerr(env, 2, "Failed to open schema config file.");
    return em_nil;
  }
  free(schema_id);

  size_t cap = 8, count = 0;
  emacs_value *entries = (emacs_value *)malloc(cap * sizeof(emacs_value));

  RimeConfigIterator it;
  if (rime->api->config_begin_list(&it, config, "switches")) {
    do {
      char path[512];
      // Toggle switches define "name"; radio groups define "options".
      snprintf(path, sizeof(path), "%s/name", it.path);
      const char *name = rime->api->config_get_cstring(config, path);

      emacs_value options = em_nil;
      const char *first_option = NULL;
      {
        snprintf(path, sizeof(path), "%s/options", it.path);
        RimeConfigIterator oit;
        if (rime->api->config_begin_list(&oit, config, path)) {
          size_t ocap = 8, ocount = 0;
          emacs_value *opts =
              (emacs_value *)malloc(ocap * sizeof(emacs_value));
          do {
            const char *opt = rime->api->config_get_cstring(config, oit.path);
            if (opt) {
              if (!first_option) {
                first_option = opt;
              }
              if (ocount == ocap) {
                ocap *= 2;
                opts = (emacs_value *)realloc(
                    opts, ocap * sizeof(emacs_value));
              }
              opts[ocount++] =
                  env->make_string(env, opt, strlen(opt));
            }
          } while (rime->api->config_next(&oit));
          rime->api->config_end(&oit);
          if (ocount > 0) {
            options = em_list(env, (ptrdiff_t)ocount, opts);
          }
          free(opts);
        }
      }

      if (name == NULL && options == em_nil) {
        continue;  // malformed switch entry, skip
      }
      emacs_value name_value =
          name ? env->make_string(env, name, strlen(name))
               : /* radio group: NAME is the first option */
               (first_option
                    ? env->make_string(env, first_option,
                                       strlen(first_option))
                    : em_nil);

      emacs_value states = em_nil;
      {
        snprintf(path, sizeof(path), "%s/states", it.path);
        RimeConfigIterator sit;
        if (rime->api->config_begin_list(&sit, config, path)) {
          size_t scap = 8, scount = 0;
          emacs_value *labels =
              (emacs_value *)malloc(scap * sizeof(emacs_value));
          do {
            const char *label =
                rime->api->config_get_cstring(config, sit.path);
            if (label) {
              if (scount == scap) {
                scap *= 2;
                labels = (emacs_value *)realloc(
                    labels, scap * sizeof(emacs_value));
              }
              labels[scount++] =
                  env->make_string(env, label, strlen(label));
            }
          } while (rime->api->config_next(&sit));
          rime->api->config_end(&sit);
          if (scount > 0) {
            states = em_list(env, (ptrdiff_t)scount, labels);
          }
          free(labels);
        }
      }

      emacs_value reset = em_nil;
      {
        int reset_value;
        snprintf(path, sizeof(path), "%s/reset", it.path);
        if (rime->api->config_get_int(config, path, &reset_value)) {
          reset = env->make_integer(env, reset_value);
        }
      }

      emacs_value entry =
          em_cons(env, name_value,
                  em_cons(env, states, em_cons(env, reset,
                                               em_cons(env, options, em_nil))));
      if (count == cap) {
        cap *= 2;
        entries =
            (emacs_value *)realloc(entries, cap * sizeof(emacs_value));
      }
      entries[count++] = entry;
    } while (rime->api->config_next(&it));
    rime->api->config_end(&it);
  }

  rime->api->config_close(config);
  free(config);

  emacs_value result = em_list(env, (ptrdiff_t)count, entries);
  free(entries);
  return result;
}

DOCSTRING(set_option, "OPTION VALUE &optional SESSION",
          "Set rime OPTION to VALUE and return the resulting state.\n"
          "OPTION is a rime option name like \"simplification\", "
          "\"ascii_mode\", \"full_shape\", \"ascii_punct\" or\n"
          "\"extended_charset\"; any non-nil VALUE turns the option on. "
          "Returns\n"
          "the state read back after setting (librime creates unknown "
          "options,\n"
          "so the read-back reflects the value just set).  With SESSION, "
          "operate\n"
          "on that session instead of the default session.");
static emacs_value set_option(emacs_env *env, ptrdiff_t nargs,
                              emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;

  bool session_ok;
  RimeSessionId session_id =
      _resolve_session(env, rime, nargs >= 3 ? args[2] : em_nil, &session_ok);
  if (!session_ok) {
    return em_nil;
  }

  const char *option = em_get_string(env, args[0]);
  Bool value = env->is_not_nil(env, args[1]) ? True : False;
  rime->api->set_option(session_id, option, value);
  Bool result = rime->api->get_option(session_id, option);
  free((char *)option);

  return result ? em_t : em_nil;
}

DOCSTRING(get_user_config, "USER-CONFIG OPTION &optional RETURN-VALUE-TYPE",
          "Get OPTION of rime USER-CONFIG.\n"
          "The return value type can be set with RETURN-VALUE-TYPE.");
static emacs_value get_user_config(emacs_env *env, ptrdiff_t nargs,
                                   emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;

  if (!_ensure_session(rime)) {
    em_signal_rimeerr(env, 1, NO_SESSION_ERR);
    return em_nil;
  }

  if (nargs < 2) {
    em_signal_rimeerr(env, 2, "Invalid arguments.");
    return em_nil;
  }

  const char *config_id = em_get_string(env, args[0]);
  const char *config_key = em_get_string(env, args[1]);
  char *config_type = "cstring";
  if (nargs == 3) {
    config_type = em_get_string(env, args[2]);
  }

  RimeConfig *config = malloc(sizeof(RimeConfig));
  // 注意user_config_open是从user_data_dir下获取
  if (!rime->api->user_config_open(config_id, config)) {
    em_signal_rimeerr(env, 2, "Failed to open user config file.");
    free((char *)config_id);
    free((char *)config_key);
    if (nargs == 3) {
      free(config_type);
    }
    return em_nil;
  }

  bool success = false;
  emacs_value result;
  // printf("get %s for %s\n", config_key, config_type);
  if (strcmp("int", config_type) == 0) {
    int number = 0;
    success = rime->api->config_get_int(config, config_key, &number);
    result = env->make_integer(env, number);
  } else if (strcmp("double", config_type) == 0) {
    double number = 0.0;
    success = rime->api->config_get_double(config, config_key, &number);
    result = env->make_float(env, number);
  } else if (strcmp("bool", config_type) == 0) {
    Bool is_true = false;
    success = rime->api->config_get_bool(config, config_key, &is_true);
    result = is_true ? em_t : em_nil;
  } else {
    const char *string = rime->api->config_get_cstring(config, config_key);
    success = true;
    result = env->make_string(env, string, strnlen(string, CONFIG_MAXSTRLEN));
  }

  rime->api->config_close(config);
  free((char *)config_id);
  free((char *)config_key);
  if (nargs == 3) {
    free(config_type);
  }

  if (!success) {
    em_signal_rimeerr(env, 2, "Failed to get config.");
    return em_nil;
  }

  return result;
}

DOCSTRING(set_user_config, "USER-CONFIG OPTION VALUE &optional VALUE-TYPE",
          "Set rime USER-CONFIG OPTION to VALUE.\n"
          "When VALUE-TYPE is non-nil, VALUE will be converted to this type.");
static emacs_value set_user_config(emacs_env *env, ptrdiff_t nargs,
                                   emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;

  if (!_ensure_session(rime)) {
    em_signal_rimeerr(env, 1, NO_SESSION_ERR);
    return em_nil;
  }

  if (nargs < 3) {
    em_signal_rimeerr(env, 2, "Invalid arguments.");
    return em_nil;
  }

  const char *config_id = em_get_string(env, args[0]);
  const char *config_key = em_get_string(env, args[1]);
  char *config_type = "string";
  if (nargs == 4) {
    config_type = em_get_string(env, args[3]);
  }
  emacs_value value = args[2];

  RimeConfig *config = malloc(sizeof(RimeConfig));
  if (!rime->api->user_config_open(config_id, config)) {
    em_signal_rimeerr(env, 2, "Failed to open user config file.");
    free((char *)config_id);
    free((char *)config_key);
    if (nargs == 4) {
      free(config_type);
    }
    return em_nil;
  }

  if (strcmp("int", config_type) == 0) {
    int number = env->extract_integer(env, value);
    rime->api->config_set_int(config, config_key, number);
  } else if (strcmp("double", config_type) == 0) {
    double number = env->extract_float(env, value);
    rime->api->config_set_double(config, config_key, number);
  } else if (strcmp("bool", config_type) == 0) {
    bool is_true = env->is_not_nil(env, value);
    rime->api->config_set_bool(config, config_key, is_true);
  } else {
    const char *string = em_get_string(env, value);
    rime->api->config_set_string(config, config_key, string);
    free((char *)string);
  }

  rime->api->config_close(config);
  free(config);
  free((char *)config_id);
  free((char *)config_key);
  if (nargs == 4) {
    free(config_type);
  }

  return em_t;
}

DOCSTRING(get_schema_config, "SCHEMA-CONFIG OPTION &optional RETURN-VALUE-TYPE",
          "Get OPTION of rime SCHEMA-CONFIG.\n"
          "The return value type can be set with RETURN-VALUE-TYPE.");
static emacs_value get_schema_config(emacs_env *env, ptrdiff_t nargs,
                                     emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;

  if (!_ensure_session(rime)) {
    em_signal_rimeerr(env, 1, NO_SESSION_ERR);
    return em_nil;
  }

  if (nargs < 2) {
    em_signal_rimeerr(env, 2, "Invalid arguments.");
    return em_nil;
  }

  const char *arg0 = em_get_string(env, args[0]);
  const int max_schema_length = 0xff;
  char *schema_id = (char *)malloc(max_schema_length * sizeof(char));
  memset(schema_id, 0, max_schema_length);
  if (arg0 == NULL || strlen(arg0) == 0) {
    if (!rime->api->get_current_schema(rime->session_id, schema_id,
                                       max_schema_length)) {
      em_signal_rimeerr(env, 2, "error get current schema");
      free(schema_id);
      free((char *)arg0);
      return em_nil;
    }
  } else {
    if (strlen(arg0) > max_schema_length) {
      em_signal_rimeerr(env, 2, "Schema id too long.");
      free(schema_id);
      free((char *)arg0);
      return em_nil;
    }

    strcpy(schema_id, arg0);
  }

  free((char *)arg0);

  if (strlen(schema_id) == 0) {
    free(schema_id);
    em_signal_rimeerr(env, 2, "Error length of schema id.");
    return em_nil;
  }

  const char *config_key = em_get_string(env, args[1]);
  char *config_type = "cstring";
  if (nargs == 3) {
    config_type = em_get_string(env, args[2]);
  }

  RimeConfig *config = malloc(sizeof(RimeConfig));
  if (!rime->api->schema_open(schema_id, config)) {
    free(schema_id);
    free((char *)config_key);
    if (nargs == 3) {
      free(config_type);
    }
    em_signal_rimeerr(env, 2, "Failed to open schema config file.");
    return em_nil;
  }

  free(schema_id);

  bool success = false;
  emacs_value result;
  // printf("get %s for %s\n", schema_id, config_type);
  if (strcmp("int", config_type) == 0) {
    int number = 0;
    success = rime->api->config_get_int(config, config_key, &number);
    result = env->make_integer(env, number);
  } else if (strcmp("double", config_type) == 0) {
    double number = 0.0;
    success = rime->api->config_get_double(config, config_key, &number);
    result = env->make_float(env, number);
  } else if (strcmp("bool", config_type) == 0) {
    Bool is_true = false;
    success = rime->api->config_get_bool(config, config_key, &is_true);
    result = is_true ? em_t : em_nil;
  } else {
    const char *string = rime->api->config_get_cstring(config, config_key);
    success = true;
    result = env->make_string(env, string, strnlen(string, CONFIG_MAXSTRLEN));
  }

  rime->api->config_close(config);
  free((char *)config_key);
  if (nargs == 3) {
    free(config_type);
  }

  if (!success) {
    em_signal_rimeerr(env, 2, "Failed to get config.");
    return em_nil;
  }

  return result;
}

DOCSTRING(set_schema_config, "CONFIG OPTION VALUE &optional VALUE-TYPE",
          "Set rime SCHEMA-CONFIG OPTION to VALUE.\n"
          "When VALUE-TYPE is non-nil, VALUE will be converted to this type.");
static emacs_value set_schema_config(emacs_env *env, ptrdiff_t nargs,
                                     emacs_value args[], void *data) {
  EmacsRime *rime = (EmacsRime *)data;

  if (!_ensure_session(rime)) {
    em_signal_rimeerr(env, 1, NO_SESSION_ERR);
    return em_nil;
  }

  if (nargs < 3) {
    em_signal_rimeerr(env, 2, "Invalid arguments.");
    return em_nil;
  }

  const char *arg0 = em_get_string(env, args[0]);
  const int max_schema_length = 0xff;
  char *schema_id = (char *)malloc(max_schema_length * sizeof(char));
  memset(schema_id, 0, max_schema_length);
  if (arg0 == NULL || strlen(arg0) == 0) {
    if (!rime->api->get_current_schema(rime->session_id, schema_id,
                                       max_schema_length)) {
      em_signal_rimeerr(env, 2, "Error get current schema.");
      free(schema_id);
      free((char *)arg0);
      return em_nil;
    }
  } else {
    if (strlen(arg0) > max_schema_length) {
      em_signal_rimeerr(env, 2, "Schema id too long.");
      free(schema_id);
      free((char *)arg0);
      return em_nil;
    }

    strcpy(schema_id, arg0);
  }

  free((char *)arg0);

  if (strlen(schema_id) == 0) {
    free(schema_id);
    em_signal_rimeerr(env, 2, "Error length of schema id.");
    return em_nil;
  }

  const char *config_key = em_get_string(env, args[1]);
  emacs_value value = args[2];
  char *config_type = "string";
  if (nargs == 4) {
    config_type = em_get_string(env, args[3]);
  }

  RimeConfig *config = (RimeConfig *)malloc(sizeof(RimeConfig));
  if (!rime->api->schema_open(schema_id, config)) {
    free(schema_id);
    free((char *)config_key);
    if (nargs == 4) {
      free(config_type);
    }
    em_signal_rimeerr(env, 2, "Failed to open schema config file.");
    return em_nil;
  }

  free(schema_id);
  if (strcmp("int", config_type) == 0) {
    int number = env->extract_integer(env, value);
    rime->api->config_set_int(config, config_key, number);
  } else if (strcmp("double", config_type) == 0) {
    double number = env->extract_float(env, value);
    rime->api->config_set_double(config, config_key, number);
  } else if (strcmp("bool", config_type) == 0) {
    bool is_true = env->is_not_nil(env, value);
    rime->api->config_set_bool(config, config_key, is_true);
  } else {
    const char *string = em_get_string(env, value);
    rime->api->config_set_string(config, config_key, string);
    free((char *)string);
  }

  rime->api->config_close(config);
  free(config);
  free((char *)config_key);
  if (nargs == 4) {
    free(config_type);
  }

  return em_t;
}

void liberime_init(emacs_env *env) {
  // Name 'rime' is hardcode in DEFUN micro, so if you edit here,
  // you should edit DEFUN micro too.
  EmacsRime *rime = (EmacsRime *)malloc(sizeof(EmacsRime));

  rime->api = rime_get_api();
  rime->first_run = true; // not used yet

  if (!rime->api) {
    free(rime);
    em_signal_rimeerr(env, 1, "No librime found.");
    return;
  }

  DEFUN("liberime-start", start, 2, 2);
  DEFUN("liberime-search", search, 1, 6);
  DEFUN("liberime-get-candidates", get_candidates, 0, 3);
  DEFUN("liberime-session-create", session_create, 0, 1);
  DEFUN("liberime-session-destroy", session_destroy, 1, 1);
  DEFUN("liberime-select-schema", select_schema, 1, 1);
  DEFUN("liberime-get-schema-list", get_schema_list, 0, 0);

  // input
  DEFUN("liberime-process-key", process_key, 1, 3);
  DEFUN("liberime-simulate-key-sequence", simulate_key_sequence, 1, 2);
  DEFUN("liberime-event-to-key-sequence", liberime_event_to_key_sequence, 1, 1);
  DEFUN("liberime-process-event", liberime_process_event, 1, 2);
  DEFUN("liberime-commit-composition", commit_composition, 0, 1);
  DEFUN("liberime-clear-composition", clear_composition, 0, 1);
  DEFUN("liberime-select-candidate", select_candidate, 1, 2);
  DEFUN("liberime-get-input", get_input, 0, 1);

  // output
  DEFUN("liberime-get-commit", get_commit, 0, 1);
  DEFUN("liberime-get-context", get_context, 0, 1);

  // status
  DEFUN("liberime-get-status", get_status, 0, 1);
  DEFUN("liberime-get-option", get_option, 1, 2);
  DEFUN("liberime-set-option", set_option, 2, 3);
  DEFUN("liberime-get-state-label", get_state_label, 2, 3);
  DEFUN("liberime-get-switches", get_switches, 0, 1);

  // sync
  DEFUN("liberime-get-sync-dir", get_sync_dir, 0, 0);
  DEFUN("liberime-sync-user-data", sync_user_data, 0, 0);
  DEFUN("liberime-finalize", finalize, 0, 0);

  // user config
  DEFUN("liberime-get-user-config", get_user_config, 2, 3);
  DEFUN("liberime-set-user-config", set_user_config, 3, 4);

  // schema config
  // if schema id is nil/empty then get/set current schema config
  DEFUN("liberime-get-schema-config", get_schema_config, 2, 3);
  DEFUN("liberime-set-schema-config", set_schema_config, 3, 4);
}
