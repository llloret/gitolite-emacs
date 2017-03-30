;;; gl-conf-test.el --- gl-conf-mode unit tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Unittest code for `gl-conf-mode'.

;;; Code:

(require 'gl-conf-init)
(require 'font-lock)
(require 'cl)


(defun gl-conf--face-p (pos faces)
  "Return non-nil if any of the faces at POS is present in FACES."
  (let ((f (or (get-text-property pos 'face) (list 'default))))
    (cl-intersection
     (if (not (listp f))
         (list f)
       f)
     (list faces))))


(defun gl-conf--location-test (locations faces)
  "Test if the nth point in LOCATIONS contain the nth face in FACES.

The nth point in LOCATION must contain the face at the nth face
in FACES for this test to pass."
  (cl-every #'identity (cl-mapcar #'gl-conf--face-p locations faces)))


(defun gl-conf--test-mode (buffer locations faces)
  "Test enabling `gl-conf-mode' in BUFFER.

The nth point in LOCATIONS is supposed to be matched with the nth
face in FACES when enabled.  When disabled, it checks that the
faces have been removed."
  (with-current-buffer buffer
    (global-font-lock-mode t)
    (font-lock-mode t)
    (gl-conf-mode)
    (font-lock-fontify-buffer)
    (gl-conf--location-test locations faces)))


(defun gl-conf--find-locations (buffer strings)
  "Return the `point' locations in BUFFER indicated by STRINGS.

STRINGS may contain any of:
- A plain string -> First occurrence of this string in the buffer.
- A list of a string and number N -> The NTH occurrence of the string.
- Anything else -> 0."
  (with-current-buffer buffer
    (let ((min (point-min))
          (max (point-max))
          (case-fold-search nil))
      (goto-char (point-min))
      (cl-loop for v in strings
               collect
               (save-excursion
                 (pcase v
                   ((pred stringp)
                    (search-forward v max t 1)
                    (match-beginning 0))
                   (`(,(and (pred stringp) s)
                      ,(and (pred numberp) n))
                    (search-forward s max t n)
                    (match-beginning 0))
                   (`(,_)
                    0)))))))


(ert-deftest gl-conf--test-font-lock-group ()
  (let* ((file (expand-file-name "gitolite.conf" gl-conf-test/test-path))
         (buf (find-file file))
         (grp-strings '("@invalid-grp" "@staff" "@interns"
                        ("@staff" 2) ("@staff" 3) ("@interns" 2)
                        ("@g" 1) ("@g" 2)))
         (faces `(font-lock-warning-face
                  ,@(make-list 6 font-lock-variable-name-face)
                  font-lock-function-name-face))
         (locations (gl-conf--find-locations buf grp-strings)))
    (should (gl-conf--test-mode buf locations faces))))


(ert-deftest gl-conf--test-font-lock-repo ()
  (let* ((file (expand-file-name "gitolite.conf" gl-conf-test/test-path))
         (buf (find-file file))
         (repo-strings (cl-loop for i from 1 to 10 collect (list "repo " i)))
         (faces `(,@(make-list 9 font-lock-keyword-face) default))
         (locations (gl-conf--find-locations buf repo-strings)))
    (should (gl-conf--test-mode buf locations faces))))


(ert-deftest gl-conf--test-font-lock-includes ()
  (let* ((file (expand-file-name "gitolite.conf" gl-conf-test/test-path))
         (buf (find-file file))
         (strings '(("subconf" 2)
                    ("include" 2)
                    ("include" 3)
                    ("include" 4)))
         (faces (make-list 4 font-lock-preprocessor-face))
         (locations (gl-conf--find-locations buf strings)))
    (should (gl-conf--test-mode buf locations faces))))


(ert-deftest gl-conf--test-list-groups ()
  (let* ((file-ans (expand-file-name "list-repos.out" gl-conf-test/test-path))
         (buf-ans  (find-file-literally file-ans))
         (file-tst (expand-file-name "gitolite.conf" gl-conf-test/test-path))
         (buf-tst  (find-file file-tst)))
    (with-current-buffer buf-tst
      (gl-conf-list-groups))
    (should (compare-buffer-substrings
             buf-ans nil nil
             buf-tst nil nil))))


(ert-deftest gl-conf--test-list-repos ()
  (let* ((file-ans (expand-file-name "list-repos.out" gl-conf-test/test-path))
         (buf-ans  (find-file-literally file-ans))
         (file-tst (expand-file-name "gitolite.conf" gl-conf-test/test-path))
         (buf-tst  (find-file file-tst)))
    (with-current-buffer buf-tst
      (gl-conf-list-repos))
    (should (compare-buffer-substrings
             buf-ans nil nil
             buf-tst nil nil))))


(ert-deftest gl-conf--test-visit-includes ()
  (let* ((file (expand-file-name "gitolite.conf" gl-conf-test/test-path))
         (buf  (find-file file))
         (ans0 (expand-file-name "foo.conf" gl-conf-test/test-path))
         (ans1 (expand-file-name "user-a.conf" gl-conf-test/test-path))
         (ans2 (expand-file-name "user-b.conf" gl-conf-test/test-path))
         visited
         includes)

    (with-current-buffer buf
      (search-forward "subconf \"foo.conf\"" nil t 1)
      (setq visited (gl-conf-visit-include))
      (dolist (b visited)
        (should (buffer-live-p b)))
      (should (string= (buffer-file-name) ans0)))

    (with-current-buffer buf
      (search-forward "include \"user-*.conf\"" nil t 1)
      (setq visited (gl-conf-visit-include))
      (dolist (b visited includes)
        (should (buffer-live-p b))
        (setq includes (append includes (list (buffer-file-name b)))))
      (should (cl-intersection includes (list ans1 ans2) :test #'string=)))))


(provide 'gl-conf-test)

;;; gl-conf-test.el ends here
