;;; latex-extra.el --- Adds several useful functionalities to LaTeX-mode.

;; Copyright (C) 2013 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>>
;; URL: http://github.com/BruceConnor/latex-extra
;; Version: 1.2.1
;; Keywords: tex
;; Package-Requires: ((auctex "11.86.1"))
;; 
;; Prefix: latex
;; Separator: /

;;; Commentary:
;; 
;; Defines extra commands and keys for LaTeX-mode. To activate (after
;; installing from melpa) just call
;; 
;;     (eval-after-load 'latex '(latex/setup-keybinds))
;;
;; The additions of this package fall into the following three
;; categories:
;; 
;; ## 1-Key Compilation ##
;; 
;; Tired of hitting `C-c C-c' 4 times (latex, bibtex, latex, view) for
;; the document to compile? This defines a much needed command that does
;; **everything** at once, and even handles compilation errors!
;; 
;; - C-c C-a **=>** `latex/compile-commands-until-done'
;; 
;; ## Navigation ##
;; 
;; Five new keybindings are defined for navigating between
;; sections/chapters. These are meant to be intuitive to people familiar
;; with `org-mode'.
;; 
;; - C-c C-n **=>** `latex/next-section'  
;; Goes forward to the next section-like command in the buffer (\part,
;; \chapter, \(sub)section, or \(sub)paragraph, whichever comes first).
;; - C-c C-u **=>** `latex/up-section'  
;; Goes backward to the previous section-like command containing this
;; one. For instance, if you're inside a subsection it goes up to the
;; section that contains it.
;; - C-c C-f **=>** `latex/next-section-same-level'  
;; Like next-section, except it skips anything that's "lower-level" then
;; the current one. For instance, if you're inside a subsection it finds
;; the next subsection (or higher), skipping any subsubsections or
;; paragraphs.
;; - C-M-f **=>** `latex/forward-environment'
;; Skip over the next environment, or exit the current one, whichever
;; comes first. 
;; - C-M-e **=>** `latex/end-of-environment'
;; Exit the current environment, and skip over some whitespace
;; afterwards. (Like `LaTeX-find-matching-end', but a little more useful.)
;; - C-M-b **=>** `latex/backward-environment'
;; - C-M-a **=>** `latex/beginning-of-environment'
;; - C-c C-p **=>** `latex/previous-section'  
;; - C-c C-b **=>** `latex/previous-section-same-level'  
;; Same as above, but go backward.
;; 
;; ## Whitespace Handling ##
;; 
;; `latex-extra.el' improves `auto-fill-mode' so that it only applies to
;; text, not equations. To use this improvement, just activate
;; `auto-fill-mode' as usual.
;; 
;; It also defines a new command:  
;; 
;; - C-c C-q **=>** `latex/clean-fill-indent-environment'  
;; Completely cleans up the entire current environment. This involves:
;; 
;; 1. Removing extraneous spaces and blank lines.
;; 2. Filling text (and only text, not equations).
;; 3. Indenting everything.

;;; Instructions:
;;
;; INSTALLATION
;;
;; If you install from melpa: just use (as described above)
;;
;;    (eval-after-load 'latex '(latex/setup-keybinds))
;;
;; If you install manually, first require it, then use the code above.
;;     (require 'latex-extra)

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 

;;; Change Log:
;; 1.2.1 - 20131011 - Fixed previous section
;; 1.2.1 - 20131011 - Rename latex-customize
;;; Code:
(eval-when-compile (require 'tex))
(eval-when-compile (require 'latex))
(eval-when-compile (require 'tex-buf))

(defconst latex-extra-version "1.2.1" "Version of the latex-extra.el package.")
(defconst latex-extra-version-int 4 "Version of the latex-extra.el package, as an integer.")
(defun latex-bug-report ()
  "Opens github issues page in a web browser. Please send me any bugs you find, and please include your Emacs and latex versions."
  (interactive)
  (message "Your latex-version is: %s, and your emacs version is: %s.\nPlease include this in your report!"
           latex-extra-version emacs-version)
  (browse-url "https://github.com/BruceConnor/latex-extra/issues/new"))
(defun latex-extra-customize ()
  "Open the customisation menu in the `latex-extra' group."
  (interactive)
  (customize-group 'latex-extra t))

;;; Implementation
(defun replace-regexp-everywhere (reg rep &optional start end)
  "Version of `replace-regexp' usable in lisp code."
  (goto-char (or start (point-min)))
  (while (re-search-forward reg end t)
    (replace-match rep nil nil)))
(defun always-t (&rest x) "Return t." t)

;;; Environment navigation
(defun latex//found-undesired-string (dir)
  "Decide whether the last search found the desired string."
  (if (> dir 0)
      (looking-back "begin")
    (looking-at "\\\\end")))

(defun latex//forward-arguments ()
  "Skip forward over the arguments."
  (when (looking-at "\\[") (forward-sexp 1))
  (when (looking-at "{") (forward-sexp 1)))

(defun latex/end-of-environment (&optional N nomark)
  "Move just past the end of the current latex environment.

Leaves point outside the environment.
Similar to `LaTeX-find-matching-end', but it accepts
numeric (prefix) argument N and skips some whitespace after the
closing \"\\end\"."
  (interactive "p")
  (unless (or nomark (region-active-p)) (push-mark))
  (let ((start (point))
        (count (abs N))
        (direction 1)
        (movement-function 'LaTeX-find-matching-end))
    (when (< N 0)
      (setq direction -1)
      (setq movement-function 'LaTeX-find-matching-begin))
    (while (and (> count 0) (funcall movement-function))
      (decf count))
    (when (> direction 0)    
      (latex//forward-arguments)
      (skip-chars-forward "[:blank:]")
      (when (looking-at "\n")
        (forward-char 1)
        (skip-chars-forward "[:blank:]")))
    ;; Return t or nil
    (case count     
      (0 t)
      (1 (message "Reached the end.") nil)
      (t (if (> direction 0)
             (error "Unclosed \\begin?")
           (error "Unopened \\end?"))))))

(defun latex/forward-environment (&optional N nomark)
  "Move to the \\end of the next \\begin, or to the \\end of the current environment (whichever comes first) N times.

Never goes into deeper environments."
  (interactive "p")
  (unless (or nomark (region-active-p)) (push-mark))
  (let ((start (point))
        (count (abs N))
        (direction (if (< N 0) -1 1)))
    (while (and (> count 0)
                (re-search-forward "\\\\\\(begin\\|end\\)\\b"
                                   nil t direction))
      (decf count)
      (if (latex//found-undesired-string direction)
          (unless (latex/end-of-environment direction t)
            (error "Unmatched \\begin?"))
        (latex//forward-arguments)))))

(defun latex/beginning-of-environment (&optional N nomark)
  "Move to the beginning of the current latex environment.

Leaves point outside the environment."
  (interactive "p")
  (latex/end-of-environment (- N) nomark))

(defun latex/backward-environment (&optional N nomark)
  "Move to the \\begin of the next \\end, or to the \\begin of the current environment (whichever comes first) N times.

Never goes into deeper environments."
  (interactive "p")
  (latex/forward-environment (- N) nomark))

;;; Section navigation
(defcustom latex/section-hierarchy '("\\headerbox"
                                     "\\subparagraph"
                                     "\\paragraph"
                                     "\\subsubsection"
                                     "\\subsection"
                                     "\\section"
                                     "\\chapter"
                                     "\\part"
                                     "\\maketitle"
                                     "\\documentclass")
  "List of strings which define what a section can be."
  :type '(repeat string)
  :group 'latex-extra
  :package-version '(latex-extra . "1.0"))

(defun latex/next-section (n)
  "Move N (or 1) headers forward.

Header stands for any string listed in `latex/section-hierarchy'.

Negative N goes backward."
  (interactive "p")
  (goto-char (latex//find-nth-section-with-predicate n 'always-t)))

(defun latex/previous-section (n)
  "Move N (or 1) headers backward.

Header stands for any string listed in `latex/section-hierarchy'."
  (interactive "p")
  (let ((sap (thing-at-point 'symbol)))
    (when (and (stringp sap) (string-match (latex/section-regexp) sap))
      (skip-chars-backward "[:alpha:]")
      (forward-char -2)))
  (latex/next-section (- n 1)))

(defun latex/up-section (n)
  "Move backward to the header that contains the current one.

Header stands for any string listed in `latex/section-hierarchy'.

With prefix argument N, goes that many headers up the hierarchy.
Negative N goes forward, but still goes \"up\" the hierarchy."
  (interactive "p")
  (goto-char (latex//find-nth-section-with-predicate (- n) 'latex/section<)))

(defun latex/next-section-same-level (n)
  "Move N (or 1) headers forward.

Header stands for any string listed in `latex/section-hierarchy'.

Negative N goes backward."
  (interactive "p")
  (goto-char (latex//find-nth-section-with-predicate n 'latex/section<=)))

(defun latex/previous-section-same-level (n)
  "Move N (or 1) headers backward.

Header stands for any string listed in `latex/section-hierarchy'."
  (interactive "p")
  (latex/next-section-same-level (- n)))

(defun latex//impl-previous-section ()
  "Find the previous header, avoiding dependencies and chaining.
Used for implementation."
  (let ((dest
         (save-match-data
           (save-excursion
             (when (looking-at "\\\\") (forward-char 1))
             (when (search-forward-regexp (latex/section-regexp) nil :noerror -1)
               (match-beginning 0))))))
    (if dest (goto-char dest) nil)))

(defun latex//find-nth-section-with-predicate (n pred)
  "Find Nth header satisfying predicate PRED, return the start of last match.

If this function fails, it returns original point position (so
you can just call it directly inside `goto-char').

PRED is the symbol to a function taking two strings.

Point will be moved up until the first header found. That is
taken as the \"previous-header\". Then, the following steps will
be repeated until PRED returns non-nil (abs N) times:

1. Point will move to the next header (in the direction
determined by the positivity of N.

2. PRED will be used to compare each this header with
\"previous-header\". It is run as:
  (PRED PREVIOUS-HEADER CURRENT-HEADER)

3. If PRED returned true, the current header is now taken as
\"previous-header\", otherwise it is ignored."
  (let* ((direction (if (> n 0) 1 -1))
         (amount (* n direction))
         (sap (thing-at-point 'symbol)) ;symbol at point
         (is-on-header-p (and (stringp sap)
                              (string-match (latex/section-regexp) sap)))
         (result
          (save-match-data
            (save-excursion
              (if (or is-on-header-p (latex//impl-previous-section))
                  (progn
                    (setq sap (thing-at-point 'symbol))
                    (when (looking-at "\\\\")
                      (unless (or (eobp) (= amount 0))
                        (forward-char 1)))
                    (while (and (> amount 0)
                                (search-forward-regexp (latex/section-regexp)
                                                       nil :noerror direction))
                      (save-match-data
                        (when (eval (list pred sap (thing-at-point 'symbol)))
                          (setq sap (thing-at-point 'symbol))
                          (decf amount))))
                    (if (= amount 0)
                        ;; Finished moving
                        (match-beginning 0)
                      ;; Didn't finish moving
                      (if (= amount n)
                          (message "No sections %s! (satisfying %S)"
                                   (if (> direction 0) "below" "above") pred)
                        (message "Reached the %s." (if (> direction 0) "bottom" "top")))))
                (message "Not inside a header."))))))
    (if (null (number-or-marker-p result))
        (point)
      (push-mark)
      result)))

(defun latex/section<= (x y)
  "Non-nil if Y comes after (or is equal to) X in `latex/section-hierarchy'."
  (member y (member x latex/section-hierarchy)))

(defun latex/section< (x y)
  "Non-nil if Y comes after X in `latex/section-hierarchy'."
  (member y (cdr-safe (member x latex/section-hierarchy))))

(defun latex/section-regexp ()
  "Return a regexp matching anything in `latex/section-hierarchy'."
  (mapconcat 'regexp-quote latex/section-hierarchy "\\|"))

(defun latex/beginning-of-line ()
  "Do `LaTeX-back-to-indentation' or `beginning-of-line'."
  (interactive)
  (let ((bef (point)))
    (LaTeX-back-to-indentation)
    (when (= bef (point))
      (beginning-of-line))))

;;; Autofilling
(defcustom latex/no-autofill-environments
  '("equation" "multline" "align" "aligned" "table" "split" "eqnarray")
  "A list of LaTeX environment names in which `auto-fill-mode' should be inhibited."
  :type '(choice (repeat string)
                 (const :tag "Never autofill." 'all))
  :group 'latex-extra
  :package-version '(latex-extra . "1.0"))

(defun latex/auto-fill-function ()
  "Perform auto-fill unless point is inside an unsuitable environment.

This function checks whether point is currently inside one of the
LaTeX environments listed in `latex/no-autofill-environments'. If
so, it inhibits automatic filling of the current paragraph."
  (when (latex/do-auto-fill-p)
    (do-auto-fill)))

(defun latex/do-auto-fill-p ()
  "Decide whether to auto-fill in current environment."
  (let ((do-auto-fill t)
        (current-environment "")
        (level 0))
    (while (and do-auto-fill (not (string= current-environment "document")))
      (setq level (1+ level)
            current-environment (LaTeX-current-environment level)
            do-auto-fill (not (member current-environment latex/no-autofill-environments))))
    do-auto-fill))

(defun latex/setup-auto-fill ()
  "Set the function used to fill a paragraph to `latex/auto-fill-function'."
  (interactive)
  (setq auto-fill-function 'latex/auto-fill-function))

;;; Whitespace cleaning
(defcustom latex/clean-up-whitespace t
  "Type of whitespace to be erased by `latex/clean-fill-indent-environment'.

Only excessive whitespace will be erased. That is, when there are
two or more consecutive blank lines they are turned into one, and
single blank lines are left untouched.

This variable has 4 possible values:
t:       Erases blank lines and spaces.
'lines:  Erases blank lines only.
'spaces: Erases spaces only.
nil:     Doesn't erase any whitespace."
  :type '(choice (const :tag "Erases blank lines and spaces." t)
                 (const :tag "Erases blank lines only." lines)
                 (const :tag "Erases spaces only." spaces)
                 (const :tag "Doesn't erase any whitespace." nil))
  :group 'latex-extra
  :package-version '(latex-extra . "1.0"))

(defun latex/clean-fill-indent-environment ()
  "Severely reorganise whitespace in current environment.

Performs the following actions (on current environment):
 1. Turn multiple new-lines and spaces into single new-lines and
    spaces, according to `latex/clean-up-whitespace'.
 2. Fill text, except inside environments given by
    `latex/no-autofill-environments'.
 3. Indent everything."
  (interactive)
  (when latex/clean-up-whitespace
    (save-excursion
      (message "Cleaning up...")
      (LaTeX-mark-environment)
      (let ((l (region-beginning))
            (r (region-end)))
        (unless (eq latex/clean-up-whitespace 'lines)  (replace-regexp-everywhere "  +" " "))
        (unless (eq latex/clean-up-whitespace 'spaces) (replace-regexp-everywhere "\n\n\n+" "\n\n")))))
  (unless (eq latex/no-autofill-environments 'all)
    (save-excursion
      (LaTeX-mark-environment)
      (let* ((l (region-beginning))
             (r (region-end))
             (size (number-to-string (length (number-to-string (line-number-at-pos r)))))
             (message-string (concat "Filling line %" size "s / %" size "s.")))
        (goto-char l)
        (forward-line 1)
        (while (and (<= (point) r) (not (eobp)))
          (if (latex/do-auto-fill-p)
              (LaTeX-fill-paragraph)
            (latex/end-of-environment 1)
            (forward-line -1))
          (forward-line 1)
          (message message-string (line-number-at-pos (point)) (line-number-at-pos r))))))
  (save-excursion
    (message "Indenting...")
    (LaTeX-mark-environment)
    (let ((l (region-beginning))
          (r (region-end)))
      (indent-region l r)))
  (message "Done."))

;;; Compilation
(defcustom latex/view-after-compile t
  "Start view-command at end of `latex/compile-commands-until-done'?"
  :type 'boolean
  :group 'latex-extra)

(defcustom latex/max-runs 10
  "Max number of times `TeX-command-master' can run.

If it goes beyond this, we decide something's wrong.

Used by `latex/compile-commands-until-done'."
  :type 'integer
  :group 'latex-extra)

(defcustom latex/view-skip-confirmation t
  "If non-nil `latex/compile-commands-until-done' will NOT ask for confirmation on the \"VIEW\" command."
  :type 'boolean
  :group 'latex-extra
  :package-version '(latex-extra . "1.0"))
(defvar latex/count-same-command 0)

(defun latex/command-default (name)
  "Next TeX command to use on file NAME."
  (cond ((if (string-equal name TeX-region)
             (TeX-check-files (concat name "." (TeX-output-extension))
                              (list name)
                              TeX-file-extensions)
           (TeX-save-document (TeX-master-file)))
         TeX-command-default)
        ((and (memq major-mode '(doctex-mode latex-mode))
              (TeX-check-files (concat name ".bbl")
                               (mapcar 'car
                                       (LaTeX-bibliography-list))
                               BibTeX-file-extensions))
         ;; We should check for bst files here as well.
         TeX-command-BibTeX)
        ((TeX-process-get-variable name
                                   'TeX-command-next
                                   TeX-command-Show))
        (TeX-command-Show)))

(defcustom latex/next-error-skip-confirmation nil
  "If non-nil `latex/compile-commands-until-done' calls `TeX-next-error' without confirmation (if there is an error, of course)."
  :type 'boolean
  :group 'latex-extra
  :package-version '(latex-extra . "1.0"))

(defun latex/compile-commands-until-done (clean-first)
  "Fully compile the current document, then view it.

If there are errors, call `TeX-next-error' instead of viewing.

With prefix argument CLEAN-FIRST, removes the output and
auxiliary files before starting (by running (TeX-clean t)). This
essentially runs the compilation on a clean slate.

This command repeatedly runs `TeX-command-master' until: (1) we
reach the VIEW command, (2) an error is found, or (3) the limit
defined in `latex/max-runs' is reached (which indicates something
is wrong).

`latex/next-error-skip-confirmation' and
`latex/view-skip-confirmation' can customize this command."
  (interactive "P")
  (when clean-first (TeX-clean t))
  (message "Compilation started.")
  (let* ((initial-buffer (buffer-name))
         (TeX-process-asynchronous nil)
         (master-file (TeX-master-file))
         (next-command (latex/command-default master-file))
         (counter 0))
    (while (and 
            (> counter -1)
            (not (equal next-command TeX-command-Show)))
      (when (> counter latex/max-runs)
        (error "Number of commands run exceeded %d (%S). Something is probably wrong"
               latex/max-runs 'latex/max-runs))
      (message "%d Doing: %s" (incf counter) next-command)
      (set-buffer initial-buffer)
      (TeX-command next-command 'TeX-master-file)
      (if (null (plist-get TeX-error-report-switches (intern master-file)))
          (setq next-command (latex/command-default master-file))
        (setq counter -1)
        (when (or latex/next-error-skip-confirmation
                  (y-or-n-p "Error found. Visit it? "))
          (TeX-next-error t))))
    (when (>= counter 0) ;; 
      (set-buffer initial-buffer)
      (if latex/view-skip-confirmation
          (TeX-view)
        (TeX-command TeX-command-Show 'TeX-master-file)))))

(defcustom latex/override-preview-map t
  "If non-nil, move the `preview-map' in LaTeX-mode from \"C-c C-p\" to \"C-c p\".

This this key is needed bind for `latex/previous-section'.

If you set this to nil, we won't bind the command
`latex/previous-section' to anything (it would be usually bound
to \"C-c C-p\"), so it will be up to you to bind it to something
else."
  :type 'boolean
  :group 'latex-extra
  :package-version '(latex-extra . "1.0"))

;;;###autoload
(defun latex/setup-keybinds ()
  "Define our key binds."
  (interactive)
  (add-hook 'LaTeX-mode-hook 'latex/setup-auto-fill)  
  (define-key LaTeX-mode-map "\C-\M-f" 'latex/forward-environment)
  (define-key LaTeX-mode-map "\C-\M-b" 'latex/backward-environment)
  (define-key LaTeX-mode-map "\C-\M-a" 'latex/beginning-of-environment)
  (define-key LaTeX-mode-map "\C-\M-e" 'latex/end-of-environment)
  (define-key LaTeX-mode-map ""   'latex/beginning-of-line)
  (define-key LaTeX-mode-map "" 'latex/compile-commands-until-done)
  (define-key LaTeX-mode-map "" 'latex/clean-fill-indent-environment)
  (define-key LaTeX-mode-map "" 'latex/up-section)
  (define-key LaTeX-mode-map "" 'latex/next-section)
  (define-key LaTeX-mode-map "" 'latex/next-section-same-level)
  (define-key LaTeX-mode-map "" 'latex/previous-section-same-level)
  (when latex/override-preview-map
    (define-key LaTeX-mode-map "" 'latex/previous-section)
    (define-key LaTeX-mode-map "p"  'preview-map))
  (add-hook 'LaTeX-mode-hook 'latex/setup-auto-fill)
  (defadvice LaTeX-preview-setup (after latex/after-LaTeX-preview-setup-advice () activate)
    "Move the preview map to \"p\" so that we free up \"\"."
    (when latex/override-preview-map
      (define-key LaTeX-mode-map "" 'latex/previous-section)
      (define-key LaTeX-mode-map "p"  'preview-map))))

(provide 'latex-extra)

;;; latex-extra.el ends here
