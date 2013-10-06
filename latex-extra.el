;;; latex-extra.el --- Adds several useful functionalities to LaTeX-mode.

;; Copyright (C) 2013 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>>
;; URL: http://github.com/BruceConnor/latex-extra
;; Version: 0.1a
;; Keywords: 
;; Prefix: latex
;; Separator: /

;;; Commentary:
;; 
;; Defines extra commands and keys for LaTeX-mode. The additions fall
;; into the following three categories:
;; 
;; # 1-Key Compilation #
;; 
;; Tired of hitting `C-c C-c' 4 times (latex, bibtex, latex, view) for
;; the document to compile? This defines a much needed command that does
;; **everything** at once, and even handles compilation errors!
;; 
;; - `C-c C-a' **=>** `latex/compile-commands-until-done'
;; 
;; # Navigating Sections #
;; 
;; Five new keybindings are defined for navigating between
;; sections/chapters. These are meant to be intuitive to people familiar
;; with `org-mode'.
;; 
;; - `C-c C-n' **=>** `latex/next-section'  
;; Goes forward to the next section-like command in the buffer (\part,
;; \chapter, \(sub)section, or \(sub)paragraph, whichever comes first).
;; - `C-c C-p' **=>** `latex/previous-section'  
;; Same, but goes backward.
;; - `C-c C-u' **=>** `latex/up-section'  
;; Goes backward to the previous section-like command containing this
;; one. For instance, if you're inside a subsection it goes up to the
;; section that contains it.
;; - `C-c C-f' **=>** `latex/next-section-same-level'  
;; Like next-section, except it skips anything that's "lower-level" then
;; the current one. For instance, if you're inside a subsection it finds
;; the next subsection (or higher), skipping any subsubsections or
;; paragraphs.
;; - `C-c C-b' **=>** `latex/previous-section-same-level'  
;; Same, but goes backward.
;; 
;; # Whitespace Handling #
;; 
;; `latex-extra.el' improves `auto-fill-mode' so that it only applies to
;; text, not equations. To use this improvement, just activate
;; `auto-fill-mode' as usual.
;; 
;; It also defines a new command:  
;; 
;; - `C-c C-q' **=>** `latex/clean-fill-indent-environment'  
;; Completely cleans up the entire current environment. This involves:
;; 
;; 1. Removing extraneous spaces and blank lines.
;; 2. Filling text (and only text, not equations).
;; 3. Indenting everything.
;; 

;;; Instructions:
;;
;; INSTALLATION
;;
;; If you install from melpa: nothing necessary, should work anywhere
;; you have latex-mode on.
;;
;; If you install manually:
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
;; 0.1a - 20131005 - Created File.
;;; Code:
(eval-when-compile (require 'tex))
(eval-when-compile (require 'latex))

(defconst latex-extra-version "0.1a" "Version of the latex-extra.el package.")
(defconst latex-extra-version-int 1 "Version of the latex-extra.el package, as an integer.")
(defun latex-bug-report ()
  "Opens github issues page in a web browser. Please send me any bugs you find, and please inclue your emacs and latex versions."
  (interactive)
  (message "Your latex-version is: %s, and your emacs version is: %s.\nPlease include this in your report!"
           latex-extra-version emacs-version)
  (browse-url "https://github.com/BruceConnor/latex-extra/issues/new"))
(defun latex-customize ()
  "Open the customization menu in the `latex-extra' group."
  (interactive)
  (customize-group 'latex-extra t))

;;; Implementation
(defun replace-regexp-everywhere (reg rep &optional start end)
  "Version of replace-regexp usable in lisp code."
  (goto-char (or start (point-min)))
  (while (re-search-forward reg end t)
    (replace-match rep nil nil)))

;;; Whitespace cleaning
(defcustom latex/clean-up-whitespace t
  "What type of whitespace will be erased by the function `latex/clean-fill-indent-environment'.

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
  :package-version '(latex-extra . "0.1a"))

(defun latex/clean-fill-indent-environment ()
  "Severely reorganize whitespace in current environment.

Performs the following actions (on current environment):
 1. Turn multiple new-lines and spaces into single new-lines and
    spaces, according to `latex/clean-up-whitespace'.
 2. Fill text, except inside evironments given by
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
            (end-of-environment)
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
  :group 'latex-functions
  :package-version '(latex-functions . ""))

(defun always-t (&rest x) "Return t." t)

(defun latex/next-section (n)
  "Move N (or 1) headers forward, where header stands for anything in the variable `latex/section-hierarchy'.

Negative N goes backward."
  (interactive "p")
  (goto-char (latex//find-nth-section-with-predicate n 'always-t)))

(defun latex/previous-section (n)
  "Move N (or 1) headers backward, where header stands for anything in the variable `latex/section-hierarchy'."
  (interactive "p")
  (latex/next-section (- n)))

(defun latex/up-section (n)
  "Move N (or 1) headers forward, where header stands for the current level of anything in the variable `latex/section-hierarchy'."
  (interactive "p")
  (goto-char (latex//find-nth-section-with-predicate (- n) 'latex/section<)))

(defun latex/next-section-same-level (n)
  "Move N (or 1) headers forward, where header stands for the current level of anything in the variable `latex/section-hierarchy'.

Negative N goes backward."
  (interactive "p")
  (goto-char (latex//find-nth-section-with-predicate n 'latex/section<=)))

(defun latex/previous-section-same-level (n)
  "Move N (or 1) headers backward, where header stands for the current level of anything in the variable `latex/section-hierarchy'."
  (interactive "p")
  (latex/next-section-same-level (- n)))

(defun latex//impl-previous-section ()
  (let ((dest
         (save-match-data
           (save-excursion
             (when (looking-at "\\\\") (forward-char 1))
             (when (search-forward-regexp (latex/section-regexp) nil :noerror -1)
               (match-beginning 0))))))
    (if dest (goto-char dest) nil)))

(defun latex//find-nth-section-with-predicate (n pred)
  "Find the next (or previous) N headers satisfying predicate PRED, return a marker at the start of the last match.

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
                      (unless (eobp) (forward-char 1)))
                    (while (and (> amount 0)
                                (search-forward-regexp (latex/section-regexp) nil :noerror direction))
                      (save-match-data
                        (when (eval (list pred sap (thing-at-point 'symbol)))
                          (setq sap (thing-at-point 'symbol))
                          (decf amount))))
                    (if (= amount n)
                        (message "No sections %s! (of the desired level)" (if (> direction 0) "below" "above"))
                      (unless (= amount 0) (message "Reached the %s." (if (> direction 0) "bottom" "top")))
                      (push-mark)
                      (match-beginning 0)))
                (message "Not inside a header."))))))
    (if (null (number-or-marker-p result)) (point)
      (push-mark)
      result)))

(defun latex/section<= (x y)
  "t if Y comes after (or is equal to) X in `latex/section-hierarchy'."
  (member y (member x latex/section-hierarchy)))

(defun latex/section< (x y)
  "t if Y comes after X in `latex/section-hierarchy'."
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
  :package-version '(latex-extra . "0.1a"))

(defun latex/auto-fill-function ()
  "Perform auto-fill unless point is inside one of the `latex/no-autofill-environments'.

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
  "This function sets the function used to fill a paragraph to `latex/auto-fill-function'."
  (setq auto-fill-function 'latex/auto-fill-function))

;;;###autoload
(add-hook 'LaTeX-mode-hook 'latex/setup-auto-fill)

;;; Compilation
(defcustom latex/view-after-compile t
  "Start view-command at end of `latex/compile-commands-until-done'?"
  :type 'boolean
  :group 'latex-extra)

(defcustom latex/max-runs 10
  "Max number of times `TeX-command-master' can run before we decide something's wrong.

Used by `latex/compile-commands-until-done'."
  :type 'integer
  :group 'latex-extra)

(defcustom latex/view-skip-confirmation t
  "If non-nil `latex/compile-commands-until-done' will NOT ask for confirmation on the \"VIEW\" command."
  :type 'boolean
  :group 'latex-extra
  :package-version '(latex-extra . "0.1a"))
(defvar latex/count-same-command 0)

(defun latex/command-default (name)
  "Next TeX command to use. Most of the code is stolen from `TeX-command-query'."
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
  :package-version '(latex-extra . "0.1a"))

(defun latex/compile-commands-until-done ()
  "Fully compile the current document, then view it. If there are errors, call `TeX-next-error' instead of viewing.

This command repeatedly runs `TeX-command-master' until: (1) we
reach the VIEW command, (2) an error is found, or (3) the limit
defined in `latex/max-runs' is reached (which indicates something
is wrong).

`latex/next-error-skip-confirmation' and
`latex/view-skip-confirmation' can customize this command."
  (interactive)
  (let* ((thebuffer (buffer-name))
         ;; (theplace (point))
         (TeX-process-asynchronous nil)
         (master-file (TeX-master-file))
         (nextCmd (latex/command-default master-file))
         (counter 0))
    (while (and ;; nextCmd
            (> counter -1)
            (not (equal nextCmd TeX-command-Show)))
      (when (> counter latex/max-runs)
        (error "Number of commands run exceeded %d (%S). Something is probably wrong."
               latex/max-runs 'latex/max-runs))
      (message "%d Doing: %s" (incf counter) nextCmd)
      (set-buffer thebuffer)
      (TeX-command nextCmd 'TeX-master-file)
      (if (null (plist-get TeX-error-report-switches (intern master-file)))
          (setq nextCmd (latex/command-default master-file))
        (setq counter -1)
        (when (or latex/next-error-skip-confirmation
                  (y-or-n-p "Error found. Visit it?"))
          (TeX-next-error t))))
    (when (>= counter 0) ;; 
      (set-buffer thebuffer)
      (if latex/view-skip-confirmation
          (TeX-view)
        (TeX-command TeX-command-Show 'TeX-master-file)))))

(defcustom latex/override-preview-map t
  "If this is non-nil, we'll move the `preview-map' in LaTeX-mode from \"\" to \"p\".

If you set this to nil, we won't bind the command
`latex/previous-section' to anything (it would be usually bound
to \"\"), so it will be up to you to bind it something else."
  :type 'boolean
  :group 'latex-extra
  :package-version '(latex-extra . "0.1a"))

;;;###autoload
(defadvice LaTeX-preview-setup (after latex/after-LaTeX-preview-setup-advice () activate)
  "Move the preview map to \"p\" so that we free up \"\"."
  (when latex/override-preview-map
    (define-key LaTeX-mode-map "" 'latex/previous-section)
    (define-key LaTeX-mode-map "p"  'preview-map)))

(defun latex/setup-keybinds ()
  "Define our keybinds."
  (interactive)
  (define-key LaTeX-mode-map ""   'latex/beginning-of-line)
  ;; (define-key LaTeX-mode-map "" 'latex/do-compile)
  (define-key LaTeX-mode-map "" 'latex/compile-commands-until-done)
  (define-key LaTeX-mode-map "" 'latex/clean-fill-indent-environment)
  (define-key LaTeX-mode-map "" 'latex/up-section)
  (define-key LaTeX-mode-map "" 'latex/next-section)
  (define-key LaTeX-mode-map "" 'latex/next-section-same-level)
  (define-key LaTeX-mode-map "" 'latex/previous-section-same-level)
  (when latex/override-preview-map
    (define-key LaTeX-mode-map "" 'latex/previous-section)
    (define-key LaTeX-mode-map "p"  'preview-map)))

;;;###autoload
(eval-after-load 'latex
  (latex/setup-keybinds))

(provide 'latex-extra)
;;; latex-extra.el ends here.

