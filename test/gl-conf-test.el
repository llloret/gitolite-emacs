;;; gl-conf-test.el --- gl-conf-mode unit tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Unittest code for `gl-conf-mode'.

;;; Code:

(require 'gl-conf-init)
(require 'font-lock)
(require 'cl)


(defun gl-conf--face-p (pos faces)
  "Return non-nil if any of the faces at POS is present in FACES."
  (let ((f (get-text-property pos 'face)))
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


(defun gl-conf--test-mode (file mode locations faces)
  "Test enabling and disabling `gl-conf-mode' in FILE with major MODE.

The nth point in LOCATIONS is supposed to be matched with the nth
face in FACES when enabled.  When disabled, it checks that the
faces have been removed."
  (with-temp-buffer
    (insert-file-contents file)
    (global-font-lock-mode t)
    (font-lock-mode t)
    (funcall mode)
    (font-lock-fontify-buffer)
    (gl-conf--location-test locations faces)))


(ert-deftest gl-conf--conf-file-test ()
  (let* ((dir (file-name-as-directory gl-conf-test/test-path))
         (file (concat dir "gitolite.conf"))
         (locations '(42 96))
         (faces '(font-lock-variable-name-face
                  font-lock-keyword-face)))
    (should (gl-conf--test-mode file #'gl-conf-mode locations faces))))


(provide 'gl-conf-test)

;;; gl-conf-test.el ends here
