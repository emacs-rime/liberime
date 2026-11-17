;;; liberime-download-test.el --- Unit tests for liberime module download -*- lexical-binding: t; -*-

;; Unit tests for the pre-built module download helpers in liberime.el.
;; These tests run without the compiled C module and without external
;; tools (no tar, unzip): fixtures are built and checked with Emacs
;; builtins (plus python3 where noted).
;;
;; Run with:
;;   emacs --batch -Q -L . -l ert -l test/liberime-download-test.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'liberime)

(defun liberime-download-test--make-tar-gz (file entries)
  "Write a tar.gz archive to FILE from ENTRIES.
ENTRIES is a list of (NAME . CONTENT) string pairs.  Uses python3 only
to construct the fixture; the code under test uses Emacs builtins."
  (let ((program
         (mapconcat
          #'identity
          (append
           (list "import tarfile, io"
                 (format "t = tarfile.open(%S, 'w:gz')" file))
           (mapcar (lambda (e)
                     (format "b = %S.encode(); i = tarfile.TarInfo(%S); i.size = len(b); t.addfile(i, io.BytesIO(b))"
                             (cdr e) (car e)))
                   entries)
           (list "t.close()"))
          "\n")))
    (with-temp-file "/tmp/liberime-fixture.py"
      (insert program))
    (should (zerop (process-file "python3" nil nil nil "/tmp/liberime-fixture.py"))))
  file)

(ert-deftest liberime-download-test-platform-linux ()
  "Linux platform identifiers map architectures onto CI archive names."
  (let ((system-type 'gnu/linux))
    (let ((system-configuration "x86_64-pc-linux-gnu"))
      (should (equal (liberime--module-platform) "linux-x86_64")))
    (let ((system-configuration "aarch64-unknown-linux-gnu"))
      (should (equal (liberime--module-platform) "linux-aarch64")))
    (let ((system-configuration "armv7l-unknown-linux-gnueabihf"))
      (should (equal (liberime--module-platform) "linux-armhf")))))

(ert-deftest liberime-download-test-platform-macos ()
  "macOS platform identifiers use the macos- prefix."
  (let ((system-type 'darwin))
    (let ((system-configuration "aarch64-apple-darwin23.0.0"))
      (should (equal (liberime--module-platform) "macos-arm64")))
    (let ((system-configuration "x86_64-apple-darwin23.0.0"))
      (should (equal (liberime--module-platform) "macos-x86_64")))))

(ert-deftest liberime-download-test-platform-windows ()
  "Windows x86_64 uses the mingw-w64 name; aarch64 uses clang-arm64."
  (let ((system-type 'windows-nt))
    (let ((system-configuration "x86_64-w64-mingw32"))
      (should (equal (liberime--module-platform) "windows-x86_64")))
    (let ((system-configuration "aarch64-w64-mingw32"))
      (should (equal (liberime--module-platform) "windows-clang-arm64")))))

(ert-deftest liberime-download-test-platform-unknown ()
  "Unknown platforms produce no platform identifier."
  (let ((system-type 'berkeley-unix)
        (system-configuration "x86_64-unknown-freebsd13.0"))
    (should (null (liberime--module-platform)))))

(ert-deftest liberime-download-test-asset-name ()
  "Asset names combine the tag, platform and archive extension."
  (let ((system-type 'gnu/linux)
        (system-configuration "x86_64-pc-linux-gnu"))
    (should (equal (liberime--module-asset-name "v0.0.11")
                   "liberime-v0.0.11-linux-x86_64.tar.gz")))
  (let ((system-type 'windows-nt)
        (system-configuration "x86_64-w64-mingw32"))
    (should (equal (liberime--module-asset-name "v0.0.11")
                   "liberime-v0.0.11-windows-x86_64-with-deps.zip"))))

(ert-deftest liberime-download-test-download-url ()
  "Download URLs point at the tagged release asset."
  (should
   (equal (liberime--module-download-url "liberime-v0.0.11-linux-x86_64.tar.gz"
                                         "v0.0.11")
          "https://github.com/emacs-rime/liberime/releases/download/v0.0.11/liberime-v0.0.11-linux-x86_64.tar.gz")))

(ert-deftest liberime-download-test-release-tag-from-header ()
  "The default tag derives from the Version header of liberime.el."
  (cl-letf (((symbol-function 'lm-version) (lambda (&optional _file) "0.0.11")))
    (should (equal (liberime--release-tag) "v0.0.11"))))

(ert-deftest liberime-download-test-release-tag-no-header ()
  "A missing Version header is an explicit error, not a silent guess."
  (cl-letf (((symbol-function 'lm-version) (lambda (&optional _file) nil)))
    (should-error (liberime--release-tag) :type 'error)))

(ert-deftest liberime-download-test-extract-tar-gz-strip-root ()
  "Extraction strips the single top-level release directory."
  (let* ((dir (make-temp-file "liberime-dl-strip" t))
         (archive (expand-file-name "release.tar.gz" dir))
         (dest (expand-file-name "out" dir)))
    (unwind-protect
        (progn
          (liberime-download-test--make-tar-gz
           archive
           '(("liberime-v0.0.11-linux-x86_64/lib/liberime-core.so" . "fake-so")
             ("liberime-v0.0.11-linux-x86_64/share/emacs/site-lisp/liberime.el"
              . "fake-el")))
          (make-directory dest)
          (liberime--extract-archive archive dest)
          (should (file-exists-p
                   (expand-file-name "lib/liberime-core.so" dest)))
          (should (equal (with-temp-buffer
                           (insert-file-contents
                            (expand-file-name "lib/liberime-core.so" dest))
                           (buffer-string))
                         "fake-so"))
          (should (file-exists-p
                   (expand-file-name "share/emacs/site-lisp/liberime.el" dest)))
          (should-not (file-exists-p
                       (expand-file-name "liberime-v0.0.11-linux-x86_64" dest))))
      (delete-directory dir t))))

(ert-deftest liberime-download-test-extract-zip ()
  "Extracting a zip release archive preserves bin/ and share/."
  (skip-unless (executable-find "python3"))
  (let* ((dir (make-temp-file "liberime-dl-zip" t))
         (archive (expand-file-name "release.zip" dir))
         (dest (expand-file-name "out" dir)))
    (unwind-protect
        (progn
          (should (zerop
                   (process-file
                    "python3" nil nil nil "-c"
                    (concat "import zipfile;"
                            "z=zipfile.ZipFile('" archive "','w');"
                            "z.writestr('bin/liberime-core.dll','fake-dll');"
                            "z.writestr('share/rime-data/default.yaml','schema');"
                            "z.close()"))))
          (make-directory dest)
          (liberime--extract-archive archive dest)
          (should (file-exists-p (expand-file-name "bin/liberime-core.dll" dest)))
          (should (file-exists-p
                   (expand-file-name "share/rime-data/default.yaml" dest))))
      (delete-directory dir t))))

(ert-deftest liberime-download-test-install-linux ()
  "Installing a linux archive drops liberime-core.so into the package dir."
  (let* ((dir (make-temp-file "liberime-dl-inst" t))
         (tmpdir (expand-file-name "tmp" dir))
         (pkgdir (expand-file-name "pkg" dir)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "lib" tmpdir) t)
          (let ((coding-system-for-write 'binary))
            (write-region "fake-so" nil
                          (expand-file-name "lib/liberime-core.so" tmpdir)
                          nil 'silent))
          (make-directory pkgdir t)
          (liberime--install-extracted tmpdir pkgdir)
          (should (file-exists-p
                   (expand-file-name
                    (concat "liberime-core" module-file-suffix) pkgdir))))
      (delete-directory dir t))))

(ert-deftest liberime-download-test-install-macos-renames-dylib ()
  "On macOS the .dylib is installed under `module-file-suffix' (.so)."
  (let* ((dir (make-temp-file "liberime-dl-inst-mac" t))
         (tmpdir (expand-file-name "tmp" dir))
         (pkgdir (expand-file-name "pkg" dir)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "lib" tmpdir) t)
          (let ((coding-system-for-write 'binary))
            (write-region "fake-dylib" nil
                          (expand-file-name "lib/liberime-core.dylib" tmpdir)
                          nil 'silent))
          (make-directory pkgdir t)
          (liberime--install-extracted tmpdir pkgdir)
          (should (file-exists-p
                   (expand-file-name "liberime-core.so" pkgdir))))
      (delete-directory dir t))))

(ert-deftest liberime-download-test-install-windows ()
  "Installing a windows archive installs bin/*.dll and share/rime-data."
  (let* ((dir (make-temp-file "liberime-dl-inst-win" t))
         (tmpdir (expand-file-name "tmp" dir))
         (pkgdir (expand-file-name "pkg" dir)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "bin" tmpdir) t)
          (make-directory (expand-file-name "share/rime-data" tmpdir) t)
          (let ((coding-system-for-write 'binary))
            (write-region "core" nil
                          (expand-file-name "bin/liberime-core.dll" tmpdir)
                          nil 'silent)
            (write-region "rime" nil
                          (expand-file-name "bin/librime-1.dll" tmpdir)
                          nil 'silent)
            (write-region "schema" nil
                          (expand-file-name "share/rime-data/default.yaml" tmpdir)
                          nil 'silent))
          (make-directory pkgdir t)
          (liberime--install-extracted tmpdir pkgdir)
          (should (file-exists-p
                   (expand-file-name "liberime-core.dll" pkgdir)))
          (should (file-exists-p
                   (expand-file-name "librime-1.dll" pkgdir)))
          (should (file-exists-p
                   (expand-file-name "rime-data/default.yaml" pkgdir))))
      (delete-directory dir t))))

(ert-deftest liberime-download-test-shared-data-dir-prefers-package ()
  "A rime-data directory in the package dir shadows system guesses."
  (let ((dir (make-temp-file "liberime-dl-data" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "rime-data" dir) t)
          (cl-letf (((symbol-function 'liberime-get-library-directory)
                     (lambda () (file-name-as-directory dir))))
            (let ((liberime-shared-data-dir nil))
              (should (equal (liberime-get-shared-data-dir)
                             (expand-file-name "rime-data" dir))))))
      (delete-directory dir t))))

(ert-deftest liberime-download-test-shared-data-dir-explicit-wins ()
  "An explicit `liberime-shared-data-dir' beats the package rime-data."
  (let ((dir (make-temp-file "liberime-dl-data2" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "rime-data" dir) t)
          (cl-letf (((symbol-function 'liberime-get-library-directory)
                     (lambda () (file-name-as-directory dir))))
            (let ((liberime-shared-data-dir "/usr/share/rime-data"))
              (should (equal (liberime-get-shared-data-dir)
                             "/usr/share/rime-data")))))
      (delete-directory dir t))))

(ert-deftest liberime-download-test-download-module-errors-without-platform ()
  "Downloading on an unsupported platform signals a user error."
  (let ((system-type 'berkeley-unix)
        (system-configuration "x86_64-unknown-freebsd13.0"))
    (should-error (liberime-download-module)
                  :type 'user-error)))

(provide 'liberime-download-test)
;;; liberime-download-test.el ends here
