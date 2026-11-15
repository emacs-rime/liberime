;;; liberime.el --- Rime elisp binding    -*- lexical-binding: t; -*-

;; Author: A.I.
;; URL: https://github.com/merrickluo/liberime
;; Version: 0.0.12
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, Chinese, input-method, rime

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; A Emacs dynamic module provide librime bindings for Emacs.

;;; Code:
(require 'cl-lib)

(defcustom liberime-after-start-hook nil
  "List of functions to be called after liberime start."
  :group 'liberime
  :type 'hook)


(defcustom liberime-module-file nil
  "Liberime module file on the system.
When it is nil, librime will auto search module in many path."
  :group 'liberime
  :type 'file)

(defcustom liberime-shared-data-dir nil
  "Data directory on the system.

More info: https://github.com/rime/home/wiki/SharedData"
  :group 'liberime
  :type 'file)

(defcustom liberime-user-data-dir
  (locate-user-emacs-file "rime/")
  "Data directory on the user home directory."
  :group 'liberime
  :type 'file)

(defcustom liberime-auto-build nil
  "If set to t, try build when module file not found in the system."
  :group 'liberime
  :type 'boolean)

(defcustom liberime-load-on-require t
  "If non-nil, load the module and start rime when this file is loaded.
When nil, loading `liberime' only defines things; you then call
`liberime-load' yourself after setting `liberime-user-data-dir',
`liberime-module-file' and friends.  This helps when a third-party
package (e.g. pyim) issues the `require', so you cannot wrap it in a
`let' to bind those variables around load time."
  :group 'liberime
  :type 'boolean)

(defcustom liberime-verbose t
  "If non-nil, echo progress messages while starting rime."
  :group 'liberime
  :type 'boolean)

(defvar liberime-select-schema-timer nil
  "Timer used by `liberime-select-schema'.")

(defvar liberime-current-schema nil
  "The rime schema set by `liberime-select-schema'.")

(declare-function liberime-clear-composition "ext:src/liberime-core.c")
(declare-function liberime-commit-composition "ext:src/liberime-core.c")
(declare-function liberime-finalize "ext:src/liberime-core.c")
(declare-function liberime-get-commit "ext:src/liberime-core.c")
(declare-function liberime-get-context "ext:src/liberime-core.c")
(declare-function liberime-get-input "ext:src/liberime-core.c")
(declare-function liberime-get-schema-config "ext:src/liberime-core.c")
(declare-function liberime-get-schema-list "ext:src/liberime-core.c")
(declare-function liberime-get-option "ext:src/liberime-core.c")
(declare-function liberime-set-option "ext:src/liberime-core.c")
(declare-function liberime-get-state-label "ext:src/liberime-core.c")
(declare-function liberime-get-switches "ext:src/liberime-core.c")
(declare-function liberime-get-status "ext:src/liberime-core.c")
(declare-function liberime-get-sync-dir "ext:src/liberime-core.c")
(declare-function liberime-get-user-config "ext:src/liberime-core.c")
(declare-function liberime-process-key "ext:src/liberime-core.c")
(declare-function liberime-simulate-key-sequence "ext:src/liberime-core.c")
(declare-function liberime-event-to-key-sequence "ext:src/liberime-core.c")
(declare-function liberime-process-event "ext:src/liberime-core.c")
(declare-function liberime-search "ext:src/liberime-core.c")
(declare-function liberime-get-candidates "ext:src/liberime-core.c")
(declare-function liberime-select-candidate "ext:src/liberime-core.c")
(declare-function liberime-select-schema "ext:src/liberime-core.c")
(declare-function liberime-set-schema-config "ext:src/liberime-core.c")
(declare-function liberime-set-user-config "ext:src/liberime-core.c")
(declare-function liberime-start "ext:src/liberime-core.c")
(declare-function liberime-sync-user-data "ext:src/liberime-core.c")

(defun liberime-get-library-directory ()
  "Return the liberime package direcory."
  (let ((file (or (locate-library "liberime")
                  (locate-library "liberime-config"))))
    (when (and file (file-exists-p file))
      (file-name-directory file))))

(defun liberime-find-rime-data (parent-dirs &optional names)
  "Find directories listed in NAMES from PARENT-DIRS.

if NAMES is nil, \"rime-data\" as fallback."
  (cl-some (lambda (parent)
             (cl-some (lambda (name)
                        (let ((dir (expand-file-name name parent)))
                          (when (file-directory-p dir)
                            dir)))
                      (or names '("rime-data"))))
           (remove nil (if (fboundp 'xdg-data-dirs)
                           `(,@parent-dirs ,@(xdg-data-dirs))
                         parent-dirs))))

(defun liberime-get-shared-data-dir ()
  "Return user data directory."
  (or liberime-shared-data-dir
      ;; Guess
      (cl-case system-type
        (gnu/linux
         (liberime-find-rime-data
          '("/usr/share/local"
            "/usr/share"
            ;; GuixOS support
            "~/.guix-home/profile/share"
            "~/.guix-profile/share"
            "/run/current-system/profile/share")))
        (darwin
         "/Library/Input Methods/Squirrel.app/Contents/SharedSupport")
        (windows-nt
         (liberime-find-rime-data
          (list
           (let ((file (executable-find "emacs")))
             (when (and file (file-exists-p file))
               (expand-file-name
                (concat (file-name-directory file)
                        "../share"))))
           "c:/" "d:/" "e:/" "f:/" "g:/")
          '("rime-data"
            "msys32/mingw32/share/rime-data"
            "msys64/mingw64/share/rime-data"))))
      ;; Fallback to user data dir.
      (liberime-get-user-data-dir)))

(defun liberime-get-user-data-dir ()
  "Return user data directory, create it if necessary."
  (let ((directory (expand-file-name liberime-user-data-dir)))
    (ignore-errors
      (make-directory directory t)
      directory)))

(declare-function w32-shell-execute "w32fns")

(defun liberime-open-directory (directory)
  "Open DIRECTORY with external app."
  (let ((directory (expand-file-name directory)))
    (when (file-directory-p directory)
      (cond ((string-equal system-type "windows-nt")
             (w32-shell-execute "open" directory))
            ((string-equal system-type "darwin")
             (start-process "" nil "open" directory))
            ((string-equal system-type "gnu/linux")
             (let ((process-connection-type nil))
               (start-process "" nil "xdg-open" directory)))))))

;;;###autoload
(defun liberime-open-user-data-dir ()
  "Open user data dir with external app."
  (interactive)
  (let ((user-dir (liberime-get-user-data-dir)))
    (when user-dir
      (liberime-open-directory user-dir))))

;;;###autoload
(defun liberime-open-shared-data-dir ()
  "Open shared data dir with external app."
  (interactive)
  (let ((shared-dir (liberime-get-shared-data-dir)))
    (when shared-dir
      (liberime-open-directory shared-dir))))

;;;###autoload
(defun liberime-open-package-directory ()
  "Open liberime library directory with external app."
  (interactive)
  (let ((library-dir (liberime-get-library-directory)))
    (when library-dir
      (liberime-open-directory library-dir))))

;;;###autoload
(defun liberime-open-package-readme ()
  "Open liberime library README.org."
  (interactive)
  (let ((library-dir (liberime-get-library-directory)))
    (when library-dir
      (find-file (concat library-dir "README.org")))))

;;;###autoload
(defun liberime-build ()
  "Build liberime-core module."
  (interactive)
  (let ((buffer (get-buffer-create "*liberime build help*"))
        (dir (liberime-get-library-directory)))
    (if (not (and dir (file-directory-p dir)))
        (message "Liberime: library directory is not found.")
      (message "Liberime: start build liberime-core module ...")
      (with-current-buffer buffer
        (erase-buffer)
        (insert "* Liberime build help")
        (unless module-file-suffix
          (insert "** Your emacs do not support dynamic module.\n"))
        (unless (executable-find "gcc")
          (insert "** You should install gcc."))
        (unless (executable-find "make")
          (insert "** You should install make.")))
      (let ((default-directory dir)
            (makefile
             (concat
              (if (eq system-type 'windows-nt)
                  "LIBRIME = -llibrime\n"
                "LIBRIME = -lrime\n")
              (concat
               "CC = gcc\n"
               "LDFLAGS = -shared\n"
               "SRC = src\n"
               "SOURCES = $(wildcard $(SRC)/*.c)\n"
               "OBJS = $(patsubst %.c, %.o, $(SOURCES))\n")
              (format "TARGET = $(SRC)/liberime-core%s\n" (or module-file-suffix ".so"))
              (let* ((path (replace-regexp-in-string
                            "/share/emacs/.*" ""
                            (or (locate-library "files") "/usr")))
                     (include-dir (concat (file-name-as-directory path) "include/")))
                (if (file-exists-p (concat include-dir "emacs-module.h"))
                    (concat "CFLAGS = -fPIC -O2 -Wall -DHAVE_RIME_API -I " include-dir "\n")
                  (concat "CFLAGS = -fPIC -O2 -Wall -DHAVE_RIME_API -I emacs-module/" (number-to-string emacs-major-version) "\n")))
              (let ((p (getenv "RIME_PATH")))
                (if p
                    (concat "CFLAGS += -I " p "/src/\n"
                            "LDFLAGS += -L " p "/build/lib/ \n"
                            "LDFLAGS += -L " p "/build/lib/Release/\n"
                            "LDFLAGS += -L " p "/dist/lib\n"
                            "LDFLAGS += -Wl,-rpath," p "/build/lib/\n"
                            "LDFLAGS += -Wl,-rpath," p "/build/lib/Release\n"
                            "LDFLAGS += -Wl,-rpath," p "/dist/lib\n")
                  "\n"))
              (concat
               ".PHONY:all objs\n"
               "all:$(TARGET)\n"
               "objs:$(OBJS)\n"
               "$(TARGET):$(OBJS)\n"
               "	$(CC) $(OBJS) $(LDFLAGS) $(LIBRIME) $(LIBS) -o $@"))))
        (with-temp-buffer
          (insert makefile)
          (write-region (point-min) (point-max) (concat dir "Makefile-liberime-build") nil :silent))
        (set-process-sentinel
         (start-process "liberime-build" "*liberime build*"
                        "make" "liberime-build")
         (lambda (proc _event)
           (when (eq 'exit (process-status proc))
             (if (= 0 (process-exit-status proc))
                 (progn (liberime-load)
                        (message "Liberime: load liberime-core module successful."))
               (pop-to-buffer buffer)
               (error "Liberime: building failed with exit code %d" (process-exit-status proc))))))))))

(defun liberime-workable-p ()
  "Return t when liberime can work."
  (featurep 'liberime-core))

(defun liberime--start ()
  "Start liberime."
  (let ((shared-dir (liberime-get-shared-data-dir))
        (user-dir (liberime-get-user-data-dir)))
    (when (and shared-dir user-dir)
      (when liberime-verbose
        (message "Liberime: start with shared dir: %S" shared-dir)
        (message "Liberime: start with user dir: %S" user-dir))
      (liberime-start shared-dir user-dir)
      (when liberime-current-schema
        (liberime-try-select-schema liberime-current-schema))
      (run-hooks 'liberime-after-start-hook))))

;;;###autoload
(defun liberime-load ()
  "Load liberime-core module."
  (interactive)
  (when (and liberime-module-file
             (file-exists-p liberime-module-file)
             (not (featurep 'liberime-core)))
    (load-file liberime-module-file))
  (let* ((libdir (liberime-get-library-directory))
         (load-path
          (list libdir
                (concat libdir "src")
                (concat libdir "build"))))
    (require 'liberime-core nil t))
  (if (featurep 'liberime-core)
      (liberime--start)
    (if liberime-auto-build
        (liberime-build)
      (let ((buf (get-buffer-create "*liberime load*")))
        (with-current-buffer buf
          (erase-buffer)
          (insert "Liberime: Fail to load liberime-core module, try to run command: (liberime-build)")
          (goto-char (point-min)))
        (pop-to-buffer buf)))))

(when liberime-load-on-require
  (liberime-load))

(defun liberime-get-preedit ()
  "Get rime preedit."
  (let* ((context (liberime-get-context))
         (composition (alist-get 'composition context))
         (preedit (alist-get 'preedit composition)))
    preedit))

(defun liberime-get-page-size ()
  "Get rime page size from context."
  (let* ((context (liberime-get-context))
         (menu (alist-get 'menu context))
         (page-size (alist-get 'page-size menu)))
    page-size))

(defun liberime-select-candidate-crosspage (num)
  "Select rime candidate cross page.

This function is different from `liberime-select-candidate', When
NUM > page size, `liberime-select-candidate' do nothing, while
this function will go to proper page then select a candidate."
  (let ((page-size (liberime-get-page-size)))
    ;; No active composition/menu: nothing to select, avoid dividing by nil.
    (when page-size
      (let* ((position (- num 1))
             (page-n (/ position page-size))
             (n (% position page-size)))
        (liberime-process-key 65360) ;回退到第一页
        (dotimes (_ page-n)
          (liberime-process-key 65366)) ;发送翻页
        (liberime-select-candidate n)))))

(defun liberime-clear-commit ()
  "Clear the lastest rime commit."
  ;; NEED IMPROVE: Second run `liberime-get-commit' will clear commit.
  (liberime-get-commit))

(defun liberime-kbd-to-key-sequence (keys)
  "Convert Emacs key sequence KEYS to librime key sequence string.

KEYS is a key sequence (vector or string) as returned by `kbd', or a
plain string whose characters are treated as individual key events.
Each event is converted via `liberime-event-to-key-sequence' and the
results are concatenated.

See also `liberime-simulate-key-sequence'.

The output format follows librime's `KeySequence::Parse' convention
(see librime/src/rime/key_event.cc):
  - Plain printable ASCII (except `{` and `}`): output directly,
    e.g. \"a\", \"1\"
  - Named keys (Left, Return, F1, etc.): wrapped in braces,
    e.g. \"{Left}\", \"{F1}\"
  - Keys with modifiers: \"{Control+a}\", \"{Control+Left}\",
    \"{Meta+F1}\"
  - Braces `{` and `}` always use names: \"{braceleft}\",
    \"{braceright}\"

Examples:
  (liberime-kbd-to-key-sequence (kbd \"a\"))       => \"a\"
  (liberime-kbd-to-key-sequence (kbd \"C-a\"))     => \"{Control+a}\"
  (liberime-kbd-to-key-sequence (kbd \"C-M-a\"))   => \"{Control+Meta+a}\"
  (liberime-kbd-to-key-sequence (kbd \"<left>\"))  => \"{Left}\"
  (liberime-kbd-to-key-sequence (kbd \"C-<left>\")) => \"{Control+Left}\"
  (liberime-kbd-to-key-sequence (kbd \"C-<f1>\"))  => \"{Control+F1}\"
  (liberime-kbd-to-key-sequence (kbd \"{\") )      => \"{braceleft}\"
  (liberime-kbd-to-key-sequence (kbd \"C-M-<left>\"))
    => \"{Control+Meta+Left}\"

Multiple keys in a sequence are concatenated:
  (liberime-kbd-to-key-sequence \"abc\")            => \"abc\"
  (liberime-kbd-to-key-sequence (kbd \"C-a C-b\"))
    => \"{Control+a}{Control+b}\""
  (let ((sequences "")
        sequence)
    (dolist (event (listify-key-sequence keys))
      (setq sequence (liberime-event-to-key-sequence event))
      (setq sequences (concat sequences sequence)))
    sequences))

(defun liberime-process-keys (keys)
  "Process a sequence of KEYS by sending each event to librime.

KEYS is a key sequence (vector or string) as returned by `kbd', or a
plain string whose characters are treated as individual key events.
Each event is converted via `liberime-process-event' and sent to
librime in order.

This is the main entry point for feeding keystrokes to librime,
typically used in input method event handlers.

Examples:
  ;; Single keystroke
  (liberime-process-keys \"a\")

  ;; Multiple keystrokes
  (liberime-process-keys \"zhongwen\")

  ;; Control/meta combinations
  (liberime-process-keys (kbd \"C-<return>\"))
  (liberime-process-keys (kbd \"C-<SPC>\"))

  ;; Function keys
  (liberime-process-keys (kbd \"<f1>\"))"
  (dolist (event (listify-key-sequence keys))
    (liberime-process-event event)))

;;;###autoload
(defun liberime-deploy()
  "Deploy liberime to affect config file change."
  (interactive)
  (liberime-finalize)
  (liberime--start))

;;;###autoload
(defun liberime-set-page-size (page-size)
  "Set rime page-size to PAGE-SIZE or by default 10.
you also need to call `liberime-deploy' to make it take affect
you only need to do this once."
  (interactive "P")
  (liberime-set-user-config "default.custom" "patch/menu/page_size" (or page-size 10) "int"))

(defun liberime-try-select-schema (schema_id)
  "Try to select rime schema with SCHEMA_ID."
  (let ((n 1))
    (setq liberime-current-schema schema_id)
    (when (featurep 'liberime-core)
      (when liberime-select-schema-timer
        (cancel-timer liberime-select-schema-timer))
      (setq liberime-select-schema-timer
            (run-with-timer
             1 2
             (lambda ()
               (let ((id (alist-get 'schema_id (ignore-errors (liberime-get-status)))))
                 (cond ((or (equal id schema_id)
                            (> n 10))
                        (if (> n 10)
                            (message "Liberime: fail to select schema %S." schema_id)
                          (message "Liberime: success to select schema %S." schema_id))
                        (message "")
                        (cancel-timer liberime-select-schema-timer)
                        (setq liberime-select-schema-timer nil))
                       (t (message "Liberime: try (n=%s) to select schema %S ..." n schema_id)
                          (ignore-errors (liberime-select-schema schema_id))))
                 (setq n (+ n 1))))))
      t)))

;;;###autoload
(defun liberime-select-schema-interactive ()
  "Select a rime schema interactive."
  (interactive)
  (let ((schema-list
         (mapcar (lambda (x)
                   (cons (format "%s(%s)" (cadr x) (car x))
                         (car x)))
                 (ignore-errors (liberime-get-schema-list)))))
    (if schema-list
        (let* ((schema-name (completing-read "Rime schema: " schema-list))
               (schema (alist-get schema-name schema-list nil nil #'equal)))
          (liberime-try-select-schema schema))
      (message "Liberime: no schema has been found, ignore."))))

;;;###autoload
(defun liberime-option-menu ()
  "Select and toggle a rime switch interactively.
Switches are enumerated from the active schema's config via
`liberime-get-switches', so only switches the schema actually
defines are offered, with the schema's own state labels.  Each
candidate is shown as \"OPTION CURRENT -> TARGET\".  For toggle
switches selecting flips the option; for radio-group switches
selecting activates the next option and deactivates the others
(mirroring librime's own cycle behavior)."
  (interactive)
  (unless (fboundp 'liberime-get-switches)
    (user-error "Liberime: switch API not available (needs liberime > 0.0.11)"))
  (let ((entries nil))
    (dolist (sw (liberime-get-switches))
      (pcase-let ((`(,name ,states ,_reset ,options) sw))
        (if options
            ;; Radio group: offer cycling to the next option.
            (let* ((len (length options))
                   (cur-idx (or (cl-position-if #'liberime-get-option options)
                                0))
                   (nxt-idx (% (1+ cur-idx) len))
                   (label (lambda (i)
                            (or (nth i states) (nth i options)))))
              (push (cons (format "%s %s -> %s"
                                  (nth cur-idx options)
                                  (funcall label cur-idx)
                                  (funcall label nxt-idx))
                          (list :radio options (nth nxt-idx options)))
                    entries))
          ;; Plain toggle switch.
          (let* ((state (liberime-get-option name))
                 (off-label (or (nth 0 states) "off"))
                 (on-label (or (nth 1 states) "on"))
                 (current (if state on-label off-label))
                 (target (if state off-label on-label)))
            (push (cons (format "%s %s -> %s" name current target)
                        (list :toggle name (not state)))
                  entries)))))
    (setq entries (nreverse entries))
    (if (null entries)
        (user-error "Liberime: no switches defined in the active schema")
      (let* ((choice (completing-read "Rime option: " entries nil t))
             (entry (assoc choice entries)))
        (when entry
          (pcase-let ((`(,kind ,arg1 ,arg2) (cdr entry)))
            (pcase kind
              (:toggle (liberime-set-option arg1 arg2))
              (:radio
               (dolist (opt arg1)
                 (liberime-set-option opt (equal opt arg2)))))
            (message "%s" choice)))))))

;;;###autoload
(defun liberime-sync ()
  "Sync rime user data.
User should specify sync_dir in installation.yaml file of
`liberime-user-data-dir' directory."
  (interactive)
  (liberime-sync-user-data))

(defun liberime--finalize-on-exit ()
  "Finalize librime when Emacs is about to exit."
  (when (featurep 'liberime-core)
    (ignore-errors (liberime-finalize))))

(add-hook 'kill-emacs-hook #'liberime--finalize-on-exit)


(provide 'liberime)

;;; liberime.el ends here
