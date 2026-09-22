;;; liberime.el --- Rime elisp binding    -*- lexical-binding: t; -*-

;; Author: A.I.
;; URL: https://github.com/merrickluo/liberime
;; Version: 0.0.11
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
(require 'subr-x)

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

(defconst liberime--releases-url
  "https://github.com/emacs-rime/liberime/releases"
  "Base URL of the liberime GitHub releases.")

(defun liberime--release-tag ()
  "Return the release tag matching this liberime.el version.
The tag is derived from the Version header of liberime.el, so an
installed package downloads the binary cut for the same release."
  (require 'lisp-mnt)
  (declare-function lm-version "lisp-mnt")
  (let* ((file (or (locate-library "liberime")
                   (when (and load-file-name
                              (string-equal "liberime.el"
                                            (file-name-nondirectory load-file-name)))
                     load-file-name)
                   buffer-file-name))
         (version (and file (lm-version file))))
    (unless version
      (error "Liberime: cannot determine the package version; use C-u to pick a release tag"))
    (concat "v" version)))

(defun liberime--module-platform ()
  "Return the CI platform identifier for the current system.
This is the middle component of release archive names, e.g.
\"linux-x86_64\".  Return nil when the platform has no pre-built
archive."
  (let ((arch (car (split-string system-configuration "-"))))
    (cl-case system-type
      (gnu/linux (pcase arch
                   ((or "aarch64" "arm64") "linux-aarch64")
                   ((pred (lambda (a) (string-prefix-p "arm" a)))
                    "linux-armhf")
                   (_ "linux-x86_64")))
      (darwin (if (string-prefix-p "x86_64" arch)
                  "macos-x86_64"
                "macos-arm64"))
      (windows-nt (cond
                   ((string-prefix-p "x86_64" arch) "windows-x86_64")
                   ((or (string-prefix-p "aarch64" arch)
                        (string-prefix-p "arm64" arch))
                    "windows-clang-arm64"))))))

(defun liberime--module-asset-name (tag)
  "Return the release archive name for the current platform at TAG.
TAG is a git release tag like \"v0.0.11\".  On Windows the archive
bundled with librime and its dependencies is used.  Return nil when
the platform has no pre-built archive."
  (when-let* ((platform (liberime--module-platform)))
    (format "liberime-%s-%s%s.%s" tag platform
            (if (eq system-type 'windows-nt) "-with-deps" "")
            (if (eq system-type 'windows-nt) "zip" "tar.gz"))))

(defun liberime--module-download-url (asset tag)
  "Return the download URL for release archive ASSET at TAG."
  (format "%s/download/%s/%s" liberime--releases-url tag asset))

(defun liberime--download-file (url dest)
  "Download URL to DEST.  Return t on success, nil otherwise.
Writes to a sibling temp file and renames it into place once the
download completes, so a partial download never replaces an existing
module file."
  (require 'url-vars)
  (defvar url-request-method)
  (defvar url-show-status)
  (let* ((url-request-method "GET")
         (url-show-status nil)
         (tmp (make-temp-name (concat dest ".tmp.")))
         (done nil))
    (unwind-protect
        (let ((buf (url-retrieve-synchronously url t t 60)))
          (when buf
            (unwind-protect
                (with-current-buffer buf
                  (set-buffer-multibyte nil)
                  (goto-char (point-min))
                  (when (re-search-forward "^HTTP/[0-9.]+ 200" nil t)
                    (when (re-search-forward "\r?\n\r?\n" nil t)
                      (let ((coding-system-for-write 'binary))
                        (when (< (point) (point-max))
                          (write-region (point) (point-max) tmp nil 'silent)
                          (rename-file tmp dest t)
                          (setq done t))))))
              (when (buffer-live-p buf)
                (kill-buffer buf)))))
      (unless done
        (when (file-exists-p tmp)
          (ignore-errors (delete-file tmp)))))
    done))

(defun liberime--tar-octal (offset len)
  "Read an octal integer from the current buffer at OFFSET (0-based)."
  (string-to-number
   (buffer-substring-no-properties (1+ offset) (+ 1 offset len))
   8))

(defun liberime--untar-buffer (dir)
  "Extract the tar archive in the current unibyte buffer into DIR.
Regular members are written under DIR with their single top-level
directory stripped; other members are skipped.  Pure Elisp."
  (let ((pos (point-min))
        (max (point-max)))
    (while (< (+ pos 512) max)
      (let ((raw (buffer-substring-no-properties pos (min (+ pos 100) max))))
        (if (not (string-match "\\`\\([^\0]+\\)" raw))
            (setq pos max)                ; zero block: end of archive
          (let* ((name (match-string 1 raw))
                 (size (liberime--tar-octal (+ (1- pos) 124) 12))
                 (type (char-after (+ pos 156)))
                 (data-start (+ pos 512)))
            (when (and (memq type '(?0 ?\C-@))
                       (string-match "\\`[^/]+/\\(.+\\)\\'" name))
              (let ((file (expand-file-name (match-string 1 name) dir)))
                (make-directory (file-name-directory file) t)
                (let ((coding-system-for-write 'no-conversion))
                  (write-region data-start (+ data-start size) file nil 'silent))))
            (setq pos (+ data-start (* 512 (ceiling size 512))))))))))

(declare-function zlib-decompress-region "decompress.el")

(defun liberime--extract-tar-gz (archive dest)
  "Extract tar.gz ARCHIVE into DEST, stripping the top-level directory."
  (unless (fboundp 'zlib-decompress-region)
    (error "Liberime: this Emacs lacks zlib support, cannot extract %s" archive))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally archive)
    (zlib-decompress-region (point-min) (point-max))
    (liberime--untar-buffer (expand-file-name dest)))
  t)

(defsubst liberime--zip-u16 (base offset)
  "Read a little-endian u16 from the current buffer at BASE+OFFSET (0-based)."
  (+ (char-after (+ 1 base offset))
     (* 256 (char-after (+ 2 base offset)))))

(defsubst liberime--zip-u32 (base offset)
  "Read a little-endian u32 from the current buffer at BASE+OFFSET (0-based)."
  (+ (liberime--zip-u16 base offset)
     (* 65536 (liberime--zip-u16 base (+ offset 2)))))

(defun liberime--zip-member-data (dir name method lho csize)
  "Write the zip member NAME to DIR, from local header LHO with CSIZE bytes.
METHOD is the zip compression method (0 = stored, 8 = deflated).
Deflated members are decompressed with an external python3, because
Emacs's builtin `zlib-decompress-region' cannot process raw DEFLATE
streams.  Signals an error when the member cannot be extracted."
  (let* ((l-name (liberime--zip-u16 lho 26))
         (l-extra (liberime--zip-u16 lho 28))
         (data-start (+ lho 30 l-name l-extra))
         (data-end (+ data-start csize))
         (file (expand-file-name name dir)))
    (make-directory (file-name-directory file) t)
    (cond
     ((= method 0)
      (let ((coding-system-for-write 'no-conversion))
        (write-region (1+ data-start) (1+ data-end) file nil 'silent)))
     ((= method 8)
      (unless (executable-find "python3")
        (error "Liberime: python3 is required to extract deflated zip members"))
      ;; `zlib-decompress-region' cannot process raw DEFLATE streams,
      ;; so inflate through python3.  `call-process-region' with
      ;; no-conversion coding systems pipes the compressed bytes
      ;; unchanged; `process-file' cannot be fed buffer contents.
      (let ((out (generate-new-buffer " *liberime inflate*"))
            (coding-system-for-read 'no-conversion)
            (coding-system-for-write 'no-conversion))
        (unwind-protect
            (progn
              (unless (zerop (call-process-region
                              (1+ data-start) (1+ data-end)
                              "python3" nil out nil
                              "-c"
                              "import sys,zlib;sys.stdout.buffer.write(zlib.decompressobj(-15).decompress(sys.stdin.buffer.read()))"))
                (error "Failed to decompress zip member %s" name))
              (with-current-buffer out
                (let ((coding-system-for-write 'no-conversion))
                  (write-region (point-min) (point-max) file nil 'silent))))
          (kill-buffer out))))
     (t (error "Unsupported zip compression method %d" method)))))

(defun liberime--unzip-buffer (archive dir)
  "Extract the zip archive in the current unibyte buffer into DIR."
  (goto-char (point-max))
  (unless (re-search-backward "PK\5\6" nil t)
    (error "Not a zip archive: %s" archive))
  (let ((cd-offset (liberime--zip-u32 (1- (match-beginning 0)) 16)))
    (goto-char (1+ cd-offset))
    (while (looking-at "PK\1\2")
      (let* ((base (1- (point)))
             (method (liberime--zip-u16 base 10))
             (csize (liberime--zip-u32 base 20))
             (name-len (liberime--zip-u16 base 28))
             (extra-len (liberime--zip-u16 base 30))
             (comment-len (liberime--zip-u16 base 32))
             (lho (liberime--zip-u32 base 42))
             (name (buffer-substring-no-properties
                    (+ base 47) (+ base 47 name-len))))
        (unless (string-suffix-p "/" name)
          (liberime--zip-member-data dir name method lho csize))
        (goto-char (+ (point) 46 name-len extra-len comment-len)))))
  t)

(defun liberime--extract-zip (archive dest)
  "Extract zip ARCHIVE into DEST."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally archive)
    (liberime--unzip-buffer archive (expand-file-name dest)))
  t)

(defun liberime--extract-archive (archive dest)
  "Extract release ARCHIVE (tar.gz or zip) into DEST.
For tar.gz archives the single top-level directory is stripped."
  (if (string-suffix-p ".zip" archive)
      (liberime--extract-zip archive dest)
    (liberime--extract-tar-gz archive dest)))

(defun liberime--install-extracted (tmpdir package-dir)
  "Install the module files extracted under TMPDIR into PACKAGE-DIR.
The layout inside TMPDIR follows the release archives: lib/ holds the
module (bin/ on Windows), share/ holds auxiliary files.  A bin/
directory marks the archive as a Windows build.  The module is
installed as liberime-core`module-file-suffix' directly in
PACKAGE-DIR, where `liberime-load' finds it via `load-path'.  On
Windows the bundled dependency DLLs are installed next to it, and
share/rime-data becomes PACKAGE-DIR/rime-data."
  (let* ((windows (file-directory-p (expand-file-name "bin" tmpdir)))
         (module-dir (if windows "bin" "lib"))
         (suffixes (if windows
                       (list ".dll")
                     (list module-file-suffix ".dylib")))
         (source (cl-find-if
                  #'file-exists-p
                  (mapcar (lambda (suffix)
                            (expand-file-name
                             (concat module-dir "/liberime-core" suffix)
                             tmpdir))
                          suffixes))))
    (unless source
      (error "Archive does not contain a liberime-core module"))
    (copy-file source
               (expand-file-name
                (concat "liberime-core" (if windows ".dll" module-file-suffix))
                package-dir)
               t)
    (when windows
      (dolist (dll (directory-files (expand-file-name "bin" tmpdir)
                                    nil "\\`librime-.*\\.dll\\'"))
        (copy-file (expand-file-name (concat "bin/" dll) tmpdir)
                   (expand-file-name dll package-dir)
                   t))
      (let ((rime-data (expand-file-name "share/rime-data" tmpdir)))
        (when (file-directory-p rime-data)
          (let ((dest (expand-file-name "rime-data" package-dir)))
            (when (file-directory-p dest)
              (delete-directory dest t))
            (copy-directory rime-data dest)))))))

(defun liberime--download-and-install-module (package-dir tag)
  "Download the pre-built module at TAG and install it into PACKAGE-DIR.
The archive is downloaded and extracted in a temporary directory; the
package directory is only modified after the extracted module has been
verified, so a failed download leaves an existing module untouched.
Return non-nil on success."
  (let ((asset (liberime--module-asset-name tag)))
    (unless asset
      (user-error "Liberime: no pre-built module for platform %s (%s)"
                  system-type system-configuration))
    (let* ((url (liberime--module-download-url asset tag))
           (tmpdir (make-temp-file "liberime-download" t))
           (archive (expand-file-name asset tmpdir)))
      (unwind-protect
          (progn
            (message "Liberime: downloading %s ..." url)
            (unless (liberime--download-file url archive)
              (error "Download failed"))
            (message "Liberime: extracting %s ..." asset)
            (liberime--extract-archive archive tmpdir)
            (liberime--install-extracted tmpdir package-dir)
            (message "Liberime: module %s installed in %s" tag package-dir)
            t)
        (delete-directory tmpdir t)))))

;;;###autoload
(defun liberime-download-module (&optional prompt-for-version)
  "Download a pre-built liberime-core module from GitHub releases.
The module is installed as liberime-core`module-file-suffix' in the
liberime package directory, where `liberime-load' finds it via
`load-path'; no `liberime-module-file' setup is needed.  Note that
reinstalling or upgrading the liberime package may remove the
downloaded file; run this command again in that case.

The release tag defaults to the version of the installed liberime
package, so the binary always matches the elisp.  With prefix argument
PROMPT-FOR-VERSION, prompt for a release tag instead.

On Windows the archive bundled with librime and the rime schema data
is used; `liberime-get-shared-data-dir' picks up the installed
rime-data automatically."
  (interactive "P")
  (unless module-file-suffix
    (user-error "Liberime: this Emacs does not support dynamic modules"))
  (let* ((tag (if prompt-for-version
                  (let ((input (read-string
                                (format "Release tag (default %s): "
                                        (liberime--release-tag)))))
                    (if (string-empty-p input)
                        (liberime--release-tag)
                      input))
                (liberime--release-tag)))
         (package-dir (or (liberime-get-library-directory)
                          (user-error "Liberime: cannot locate the package directory")))
         (module (expand-file-name
                  (concat "liberime-core" module-file-suffix) package-dir)))
    (when (and (file-exists-p module)
               (not (yes-or-no-p
                     (format "Module already exists at %s.  Re-download? " module))))
      (user-error "Cancelled"))
    (if (not (liberime--download-and-install-module package-dir tag))
        (user-error "Liberime: download failed")
      (if (featurep 'liberime-core)
          (message "Liberime: module downloaded.  Restart Emacs to load the new version")
        (liberime-load)
        (when (featurep 'liberime-core)
          (message "Liberime: module downloaded and loaded successfully"))))))

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
      ;; Data from a downloaded "with-deps" archive.
      (when-let* ((libdir (liberime-get-library-directory))
                  (data-dir (expand-file-name "rime-data" libdir))
                  ((file-directory-p data-dir)))
        data-dir)
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
             (concat "open " (shell-quote-argument directory)))
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
          (insert "Liberime: Fail to load liberime-core module, try to run command:\n")
          (insert "  M-x liberime-download-module  - download a pre-built module\n")
          (insert "  M-x liberime-build            - build from source")
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
  (let* ((page-size (liberime-get-page-size))
         (position (- num 1))
         (page-n (/ position page-size))
         (n (% position page-size)))
    (liberime-process-key 65360) ;回退到第一页
    (dotimes (_ page-n)
      (liberime-process-key 65366)) ;发送翻页
    (liberime-select-candidate n)))

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
