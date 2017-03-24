;;; gl-conf-init.el --- Test initialization code.

;;; Commentary:
;; Test setup code.

;;; Code:

(require 'undercover)
(undercover "gl-conf-mode.el"
            (:report-file "/tmp/coverage.json"))
(require 'f)
(require 'ert)

(defvar gl-conf-test/test-path
  (f-dirname (f-this-file)))

(defvar gl-conf-test/root-path
  (f-parent gl-conf-test/test-path))

(load (f-expand "gl-conf-mode" gl-conf-test/root-path))

(provide 'gl-conf-init)

;;; gl-conf-init.el ends here
