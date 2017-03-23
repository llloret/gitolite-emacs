;; gl-conf-mode.el --- Mode for editing gitolite config files -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Provides navigation utilities, syntax highlighting and indentation for
;; gitolite configuration files (gitolite.conf)
;;
;; Add this code to your .emacs file to use the mode (and automatically
;; open gitolite.conf files in this mode)
;;
;; (setq load-path (cons (expand-file-name "/directory/path/to/gitolite-conf-mode.el/") load-path))
;; (require 'gl-conf-mode)
;; (add-to-list 'auto-mode-alist '("gitolite\\.conf\\'" . gl-conf-mode))
;;
;; If the file you want to edit is not named gitolite.conf, use
;; M-x gl-conf-mode, after opening the file
;;
;; Automatic indentation for this mode is disabled by default.
;; If you want to enable it, go to Customization menu in Emacs,
;; then "Files", "Gitolite Config Files", and select the appropiate option.
;;
;; The interesting things you can do are:
;; - move to next repository definition: C-c C-n
;; - move to previous repository definition: C-c C-p
;; - go to the include file on the line where the cursor is: C-c C-v
;; - open a navigation window with all the repositories (hyperlink enabled): C-c C-l
;; - mark the current repository and body: C-c C-m
;; - open a navigation window with all the defined groups (hyperlink enabled): C-c C-g
;; - offer context sentitive help linking to the original web documentation: C-c C-h
;;
;; For the context sensitive help it can detect different positions, and will offer
;; help on that topic:
;;    - repo line
;;    - include line
;;    - permissions (R/RW/RWC/...)
;;    - refexes (branches, ...)
;;    - user or group permissions
;;    - groups
;;    - anything else (offer generic gitolite.conf help)

;; The help uses the main gitolite web documentation, linking directly into it
;; with a browser.
;; If the Emacs w3m module is available in the system, it will be used to open
;; the help inside Emacs, otherwise, the Emacs configured external browser will
;; be launched (Emacs variable `browse-url-browser-function')
;;
;;
;; Please note, that while it is not required by the license, I would
;; sincerely appreciate if you sent me enhancements / bugfixes you make
;; to integrate them in the master repo and make those changes accessible
;; to more people
;;
;; Now the MIT license block (this enables you to use this code in a
;; very liberal manner)
;;
;;
;; Copyright (c) 2011 Luis Lloret
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

(require 'thingatpt)


(defconst gl-conf-regex-repo "^[ \t]*repo[ \t]+")
(defconst gl-conf-regex-include "^[ \t]*include[ \t]+\"\\(.*\\)\"")

;;
;; Indentation logic
;;
(defun gl-conf-indent ()
  "Indent current line as a gitolite configuration file."
  (interactive)
  (when gl-conf-auto-indent-enable
	(if (bobp)
		(gl-conf-indent-line-to 0)
	  (let (cur-indent)
		(save-excursion
		  (beginning-of-line)
		  (let ((point-start (point))
				token)

			;; Search backwards (if there is a repo definition, we indent, otherwise, we don't)
			(if (re-search-backward gl-conf-regex-repo (point-min) t)
				(setq cur-indent tab-width) (setq cur-indent 0))

			(goto-char point-start)

			;; Set indentation to zero if this is a repo block
			(if (looking-at gl-conf-regex-repo)
				(setq cur-indent 0))))

		(gl-conf-indent-line-to cur-indent))))
  (unless gl-conf-auto-indent-enable
	(insert-tab))
  )

(defun gl-conf-point-in-indendation ()
  "Check if point is within a strip of whitespace used as indentation."
  (string-match "^[ \\t]*$" (buffer-substring (point-at-bol) (point))))

(defun gl-conf-indent-line-to (column)
  "Indent the current line to COLUMN."
  (if (gl-conf-point-in-indendation)
      (indent-line-to column)
    (save-excursion (indent-line-to column))))



(defun gl-conf--occur-clean ()
  "Reformat the `occur' output.

Remove the he header line showing the regular expression and the
buffer names."
  (let ((buf "*Occur*")
        (inhibit-read-only t))
    (if (get-buffer buf)
        (with-current-buffer buf
          (goto-char (point-min))
          (kill-line)
          (while (re-search-forward ".*in \\(buffer.*\\)" (point-max) t)
            (replace-match (match-string 1))))
      (message "There is no buffer named \"%s\"." buf))))


(defun gl-conf-find-next-repo ()
  "Moves the cursor to the next repo definition in the current file. Returns t if next repo, nil otherwise."
  (interactive)
  (push-mark)
  (let ((cur-point (point)))
	(end-of-line nil)
	(if (re-search-forward gl-conf-regex-repo nil t)
		(progn (beginning-of-line nil) t)
	  (message "No more repos")
	  (goto-char cur-point)
	  nil))
  )

(defun gl-conf-find-prev-repo ()
  "Moves the cursor to the previous repo definition on the current file. Returns t if previous repo, nil otherwise."
  (interactive)
  (push-mark)
  (let ((cur-point (point)))
	(if (re-search-backward gl-conf-regex-repo nil t)
		t
      (message "No previous repo")
	  (goto-char cur-point)
	  nil))
  )


(defun gl-conf--visit-all-includes ()
  "Visits all the include files recursively.

 Note that this function will follow wildcard file-names."
  (save-excursion
    ;; scan the file for include directives
    (let ((all)
          (buf)
          (wset (list (current-buffer))))
      (while wset
        (setq buf (pop wset))
        (push buf all)
        (with-current-buffer buf

          ;; Find all include statements in the current file.
          (goto-char (point-min))
          (while (re-search-forward gl-conf-regex-include (point-max) t)

            ;; Open the file and add it to the working set if not already seen.
            (cl-loop
             with r = (find-file-noselect (match-string 1) t nil t)
             for b in (if (listp r) r (list r)) do
             (unless (or (memq b all)
                         (memq b wset)) ; Avoid infinite recursion.
               (push b wset))))))
      all)))


(defun gl-conf-visit-include ()
  "Visit the include file that is on the current line. It follows wildcards. Opens the include(s) in gl-conf major mode."
  (interactive)
  (let (curpoint (point)
				 buffers)
    (beginning-of-line nil)
    (if (not (re-search-forward gl-conf-regex-include (point-at-eol) t))
		(message "Not a include line")
	  (setq buffers (find-file (match-string 1) t))
	  (if (listp buffers)
		  (dolist (buf buffers)
			(switch-to-buffer buf)
			(gl-conf-mode))
		(gl-conf-mode))))
  )


(defun gl-conf-list-common (regexp)
  "List all occurrences of a specified REGEXP with hyperlinks."
  (save-excursion
    ;; open the included files
    (let ((bufs (gl-conf--visit-all-includes)))
      ;; If multi-occur is not found fallback to occur.
      (if (fboundp #'multi-occur)
          (multi-occur bufs regexp)
        (occur regexp)))
    ;; Clean the navigation buffer that occur created.
    (gl-conf--occur-clean)))


(defun gl-conf-list-repos ()
  "Open a window with a list of all repos for the configuration file(s).

 The new window supports hyperlinking, so hitting RET on there
will take you to the occurrence.

In recent Emacs versions, it will use 'multi-occur', so it
navigates through the includes to find references in them as
well; Otherwise it will use `occur', which searches only in the
current file."
  (interactive)
  (gl-conf-list-common "^[ \t]*repo[ \t]+.*"))


(defun gl-conf-list-groups ()
  "Open a window with a list of all group definitions.

 The new window supports hyperlinking, so hitting RET on there
will take you to the occurrence.

In recent Emacs versions, it will use 'multi-occur', so it
navigates through the includes to find references in them as
well; Otherwise it will use 'occur', which searches only in the
current file."
  (interactive)
  (gl-conf-list-common "^[ \t]*@[A-Za-z0-9][A-Za-z0-9-_.]+"))


(defun gl-conf-mark-repo ()
  "Mark (select) the repo configuration line(s) based on where `point' is."
  (interactive)
  ;; Go to previous repo definition line, which is the one that contains the
  ;; cursor and mark the position
  (beginning-of-line)
  (when (or (looking-at gl-conf-regex-repo)
            (gl-conf-find-prev-repo))
    (set-mark (point))
    ;; Now look for the next repo or end of buffer...
    (if (not (gl-conf-find-next-repo))
        (goto-char (point-max)))
    ;; ...and move back while the line is empty...
    (if (not (eobp))
        (forward-line -1))
    (while (looking-at "^$")
      (forward-line -1))
    ;; ...and then go to the end of line to select it.
    (end-of-line)))


(defun gl-conf--open-url (url)
  "Open a URL with `w3m'."
  (let ((cur-buffer (get-buffer (buffer-name))))
    (if (fboundp 'w3m-goto-url)
        (progn
          (when (one-window-p t)
            (split-window)
            (set-window-buffer nil cur-buffer))
          (w3m-goto-url url))
      (browse-url url))))


(defun gl-conf-context-help ()
  "Offer context-sensitive help.

 Currently it needs `w3m' Emacs installed.  It would be nice if
 it could fall back to another mechanism, if this is not
 available."
  (interactive)

  ;; Ensure that this section is case-sensitive.
  (let ((cur-point (point))
        (case-fold-search nil))
    (save-excursion

      ;; Are we in a group?
      (if (and (word-at-point) (string-match "^@" (word-at-point)))
          (progn (gl-conf--open-url "http://gitolite.com/gitolite/conf/#group-definitions")
                 (message "Opened help for group definition"))
        (beginning-of-line)

        ;; Are we on the right side of an assignment with a permission at the
        ;; beginning (this means that we are in the users / groups part)?
        (cond
         ((re-search-forward "^[ \t]*\\(-\\|R\\|RW\\+?C?D?\\)[ \t]*=" (+ cur-point 1) t)
          (gl-conf--open-url "http://gitolite.com/gitolite/conf/#access-rules")
          (message "Opened help for access rules"))

         ;; Are we on a refex or right after it? (if there is a permission
         ;; before and we are looking at some word)
         ((re-search-forward "^[ \t]*\\(-\\|R\\|RW\\+?C?D?\\)[ \t]+\\w+" (+ cur-point 1)  t)
          (gl-conf--open-url "http://gitolite.com/gitolite/conf/#the-refex-field")
          (message "Opened help for refex definition"))

         ;; Are we in a permission code or right after it?
         ((re-search-forward "^[ \t]*\\(-\\|R\\|RW\\+?C?D?\\)" (+ cur-point 1) t)
          (gl-conf--open-url "http://gitolite.com/gitolite/conf-2/#access-control-rule-matching")
          (message "Opened help for access control rule matchings"))

         ;; Look for other things...
         ;; Are we on a repo line?
         ((looking-at "[ \t]*repo" )
          (gl-conf--open-url "http://gitolite.com/gitolite/basic-admin/#add-remove-and-rename-repos")
          (message "Opened help for repo"))

         ;; Are we in an include line?
         ((looking-at "[ \t]*include")
          (gl-conf--open-url "http://gitolite.com/gitolite/conf/#include-files")
          (message "Opened help for includes"))

         ;; Not found anything? Open generic help
         (t
          (gl-conf--open-url "http://gitolite.com/gitolite/conf/")
          (message "Not in any known context. Opened general help section")))))))


;;; Definition of constants for the font-lock functionality.

(defconst gl-conf--repo-rx
  "^[ \t]*\\(repo[ \t]+[A-Za-z0-9][A-Za-z0-9-/_.*]*\\)[ \t\n]"
  "Regular expression to match repository definitions.")

(defconst gl-conf--include-rx
  "^[ \t]*\\(include[ \t]+\\)"
  "Regular expression to match inclusion statements.")

(defconst gl-conf--permissions-rx
  "^[ \t]*\\(-\\|R\\|RW\\+?C?D?M?\\)[ \t].*="
  "Regular expression to match repository permissions.")

(defconst gl-conf--refex-rx
  "^[ \t]*\\(?:-\\|R\\|RW\\+?C?D?M?\\)[ \t]+\\(\\(?:\\w\\|/\\|\\[\\|\\]\\|-\\)+\\)[ \t]*="
  "Regular expression to match `refexes'.")

(defconst gl-conf--conf-rx
  "^[ \t]*\\(config\\).*="
  "Regular expression to match repository configuration.")

(defconst gl-conf--partial-conf-rx
  "^[ \t]*\\(config.*\\)"
  "Regular expression to match partial repository configuration.")

(defconst gl-conf--group-rx
  "^[ \t]*\\(@[A-Za-z0-9][A-Za-z0-9-_.]+\\)[ \t]*="
  "Regular expression to match group definitions.")

(defconst gl-conf--group-warn-rx
  "^[ \t]*\\(@[A-Za-z0-9][A-Za-z0-9-_.]+.*\\)"
  "Regular expression to detect incorrectly defined groups.")

(defconst gl-conf--group-use-rx
  "[= \t][ \t]*\\(@[A-Za-z0-9][A-Za-z0-9-_.]+\\)"
  "Regular expression to match usage of groups variables.")


(defconst gl-conf--font-lock-keywords
  `(((,gl-conf--repo-rx 1 font-lock-keyword-face)
     (,gl-conf--include-rx 1 font-lock-keyword-face)
     (,gl-conf--permissions-rx 1 font-lock-type-face)
     (,gl-conf--conf-rx 1 font-lock-reference-face)
     (,gl-conf--partial-conf-rx 1 font-lock-warning-face)
     (,gl-conf--group-rx 1 font-lock-variable-name-face)
     (,gl-conf--group-warn-rx 1 font-lock-warning-face)
     (,gl-conf--group-use-rx 1 font-lock-variable-name-face)
     (,gl-conf--refex-rx 1 font-lock-type-face)))
  "Syntax highlighting for gl-conf-mode.")


;;
;; Customization to enable or disable automatic indentation
;;
(defgroup gl-conf nil
  "Gitolite configuration editing."
  :tag "Gitolite config files"
  :prefix "gl-conf"
  :group 'files)

(defcustom gl-conf-auto-indent-enable nil
  "Enable automatic indentation for gl-conf mode."
  :type 'boolean
  :group 'gl-conf)

;;
;; gl-conf mode init function.
;;

(defconst gl-conf--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_  "w" table)
    (modify-syntax-entry ?@  "w" table)
    (modify-syntax-entry ?#  "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for `gl-conf-mode'.")


(defvar gl-conf-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") #'gl-conf-find-next-repo)
    (define-key map (kbd "C-c C-p") #'gl-conf-find-prev-repo)
    (define-key map (kbd "C-c C-v") #'gl-conf-visit-include)
    (define-key map (kbd "C-c C-l") #'gl-conf-list-repos)
    (define-key map (kbd "C-c C-m") #'gl-conf-mark-repo)
    (define-key map (kbd "C-c C-g") #'gl-conf-list-groups)
    (define-key map (kbd "C-c C-h") #'gl-conf-context-help)
    map)
  "Keymap for `gl-conf-mode'.")


;;;###autoload
(define-derived-mode gl-conf-mode prog-mode "gitolite-conf"
  "Major mode for editing gitolite config files.

Provides basic syntax highlighting (including detection of some
malformed constructs) and basic navigation.

\\{gl-conf-mode-map}"
  :group 'gl-conf
  :syntax-table gl-conf--syntax-table

  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local indent-line-function #'gl-conf-indent)
  (setq-local font-lock-defaults gl-conf--font-lock-keywords)
  (font-lock-flush))


(provide 'gl-conf-mode)

;;; gl-conf-mode.el ends here
