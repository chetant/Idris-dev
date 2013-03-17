(eval-when-compile (require 'cl))
(eval-when-compile
  ;; Emacs 21 defines `values' as a (run-time) alias for list.
  ;; Don't maerge this with the pervious clause.
  (if (string-match "values"
		    (pp (byte-compile (lambda () (values t)))))
      (defsubst values (&rest values)
	values)))

;; Version of mode.
(defconst idris-version "v0.0.1"
  "`idris-mode' version number.")
(defun idris-version ()
  "Echo the current version of `idris-mode' in the minibuffer."
  (interactive)
  (message "Using idris-mode version %s" idris-version))

(defgroup idris nil
  "Major mode for editing Idris programs."
  :group 'languages
  :prefix "idris-")

;; Set load-path
;;;###autoload
(add-to-list 'load-path
   (or (file-name-directory load-file-name) (car load-path)))

(autoload 'turn-on-idris-indent "idris-indent"
  "Turn on Idris indentation." t)

(autoload 'idris-font-lock-choose-keywords "idris-font-lock")

;; Mode maps.
(defvar idris-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Bindings for the inferior idris process:
    (define-key map [?\C-c ?\C-l] 'inferior-idris-load-file)
    ;; (define-key map [?\C-c ?\C-b] 'switch-to-idris)

    ;; (define-key map [?\C-c ?\C-s] 'inferior-idris-start-process)
    ;; That's what M-; is for.
    ;; (define-key map "\C-c\C-c" 'comment-region)

    (define-key map (kbd "C-c C-t") 'inferior-idris-type)
    (define-key map (kbd "C-c C-i") 'inferior-idris-info)
    ;; (define-key map (kbd "C-c M-.") 'inferior-idris-find-definition)

    ;; (define-key map [?\C-c ?\C-v] 'idris-check)

    ;; (define-key map [remap delete-indentation] 'idris-delete-indentation)
    map)
  "Keymap used in Idris mode.")

(easy-menu-define idris-mode-menu idris-mode-map
  "Menu for the Idris major mode."
  `("Idris"
    ["Indent line" indent-according-to-mode]
    ["Indent region" indent-region mark-active]
    ["(Un)Comment region" comment-region mark-active]
    "---"
    ["Start interpreter" switch-to-idris]
    ["Load file" inferior-idris-load-file]
    "---"
    ,(if (default-boundp 'eldoc-documentation-function)
         ["Doc mode" eldoc-mode
          :style toggle :selected (bound-and-true-p eldoc-mode)]
       ["Doc mode" idris-doc-mode
        :style toggle :selected (and (boundp 'idris-doc-mode) idris-doc-mode)])
    ["Customize" (customize-group 'idris)]
    ))

;; Syntax table.
(defvar idris-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\  " " table)
    (modify-syntax-entry ?\t " " table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\'" table)
    (modify-syntax-entry ?_  "w" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[  "(]" table)
    (modify-syntax-entry ?\]  ")[" table)

    (cond ((featurep 'xemacs)
	   ;; I don't know whether this is equivalent to the below
	   ;; (modulo nesting).  -- fx
	   (modify-syntax-entry ?{  "(}5" table)
	   (modify-syntax-entry ?}  "){8" table)
	   (modify-syntax-entry ?-  "_ 1267" table))
	  (t
	   ;; In Emacs 21, the `n' indicates that they nest.
	   ;; The `b' annotation is actually ignored because it's only
	   ;; meaningful on the second char of a comment-starter, so
	   ;; on Emacs 20 and before we get wrong results.  --Stef
	   (modify-syntax-entry ?\{  "(}1nb" table)
	   (modify-syntax-entry ?\}  "){4nb" table)
	   (modify-syntax-entry ?-  "_ 123" table)))
    (modify-syntax-entry ?\n ">" table)

    (let (i lim)
      (map-char-table
       (lambda (k v)
	 (when (equal v '(1))
	   ;; The current Emacs 22 codebase can pass either a char
	   ;; or a char range.
	   (if (consp k)
	       (setq i (car k)
		     lim (cdr k))
	     (setq i k
		   lim k))
	   (while (<= i lim)
	     (when (> i 127)
	       (modify-syntax-entry i "_" table))
	     (setq i (1+ i)))))
       (standard-syntax-table)))
    
    (modify-syntax-entry ?\` "$`" table)
    (modify-syntax-entry ?\\ "\\" table)
    (mapc (lambda (x)
            (modify-syntax-entry x "_" table))
          ;; Some of these are actually OK by default.
          "!#$%&*+./:<=>?@^|~")
    (unless (featurep 'mule)
      ;; Non-ASCII syntax should be OK, at least in Emacs.
      (mapc (lambda (x)
              (modify-syntax-entry x "_" table))
            (concat "¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿"
                    "×÷"))
      (mapc (lambda (x)
              (modify-syntax-entry x "w" table))
            (concat "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ"
                    "ØÙÚÛÜÝÞß"
                    "àáâãäåæçèéêëìíîïðñòóôõö"
                    "øùúûüýþÿ")))
    table)
  "Syntax table used in Idris mode.")

(defun idris-ident-at-point ()
  "Return the identifier under point, or nil if none found.
May return a qualified name."
  (save-excursion
    (let ((case-fold-search nil))
      (multiple-value-bind (start end)
          (if (looking-at "\\s_")
              (values (progn (skip-syntax-backward "_") (point))
                      (progn (skip-syntax-forward "_") (point)))
            (values
             (progn (skip-syntax-backward "w'")
                    (skip-syntax-forward "'") (point))
             (progn (skip-syntax-forward "w'") (point))))
        ;; If we're looking at a module ID that qualifies further IDs, add
        ;; those IDs.
        (goto-char start)
        (while (and (looking-at "[[:upper:]]") (eq (char-after end) ?.)
                    ;; It's a module ID that qualifies further IDs.
                    (goto-char (1+ end))
                    (save-excursion
                      (when (not (zerop (skip-syntax-forward
                                         (if (looking-at "\\s_") "_" "w'"))))
                        (setq end (point))))))
        ;; If we're looking at an ID that's itself qualified by previous
        ;; module IDs, add those too.
        (goto-char start)
        (if (eq (char-after) ?.) (forward-char 1)) ;Special case for "."
        (while (and (eq (char-before) ?.)
                    (progn (forward-char -1)
                           (not (zerop (skip-syntax-backward "w'"))))
                    (skip-syntax-forward "'")
                    (looking-at "[[:upper:]]"))
          (setq start (point)))
        ;; This is it.
        (buffer-substring-no-properties start end)))))

(defun idris-delete-indentation (&optional arg)
  "Like `delete-indentation' but ignoring Bird-style \">\"."
  (interactive "*P")
  (let ((fill-prefix (or fill-prefix (if (eq idris-literate 'bird) ">"))))
    (delete-indentation arg)))

;; Various mode variables.

(defcustom idris-mode-hook nil
  "Hook run after entering Idris mode.
Do not select more than one of the three indentation modes."
  :type 'hook
  :group 'idris
  :options `(turn-on-idris-indent turn-on-idris-indentation
	     turn-on-font-lock
	     ,(if (boundp 'eldoc-documentation-function)
		  'turn-on-eldoc-mode
		'turn-on-idris-doc-mode) ; Emacs 21
	     turn-on-simple-indent turn-on-idris-doc-mode
	     imenu-add-menubar-index))

(defvar eldoc-print-current-symbol-info-function)

;; The main mode functions
;;;###autoload
(define-derived-mode idris-mode fundamental-mode "Idris"
  "Major mode for editing Idris programs.
Blank lines separate paragraphs, comments start with `-- '.
\\<idris-mode-map>
Module X is activated using the command `turn-on-X'.  For example,
`idris-indent' is activated using `turn-on-idris-indent'.
For more information on a module, see the help for its `X-mode'
function.  Some modules can be deactivated using `turn-off-X'.  (Note
that `idris-doc' is irregular in using `turn-(on/off)-idris-doc-mode'.)

Use `idris-version' to find out what version this is.

Invokes `idris-mode-hook'."
  (set (make-local-variable 'paragraph-start) (concat "^$\\|" page-delimiter))
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'fill-paragraph-function) 'idris-fill-paragraph)
  ;; (set (make-local-variable 'adaptive-fill-function) 'idris-adaptive-fill)
  (set (make-local-variable 'adaptive-fill-mode) nil)
  (set (make-local-variable 'comment-start) "-- ")
  (set (make-local-variable 'comment-padding) 0)
  (set (make-local-variable 'comment-start-skip) "[-{]-[ \t]*")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-end-skip) "[ \t]*\\(-}\\|\\s>\\)")
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  ;; Set things up for eldoc-mode.
  (set (make-local-variable 'eldoc-documentation-function)
       'idris-doc-current-info)
  ;; Set things up for imenu.
  (set (make-local-variable 'imenu-create-index-function)
       'idris-ds-create-imenu-index)
  ;; Set things up for font-lock.
  (set (make-local-variable 'font-lock-defaults)
       '(idris-font-lock-choose-keywords
	 nil nil ((?\' . "w") (?_  . "w")) nil
	 (font-lock-syntactic-keywords
	  . idris-font-lock-choose-syntactic-keywords)
	 (font-lock-syntactic-face-function
	  . idris-syntactic-face-function)
	 ;; Get help from font-lock-syntactic-keywords.
	 (parse-sexp-lookup-properties . t)))
  ;; Idris's layout rules mean that TABs have to be handled with extra care.
  ;; The safer option is to avoid TABs.  The second best is to make sure
  ;; TABs stops are 8 chars apart, as mandated by the Idris Report.  --Stef
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'tab-width) 8)
  (setq idris-literate nil))

(defun in-comment () (nth 4 (syntax-ppss)))

(defun idris-fill-paragraph (justify)
  (save-excursion
    ;; We don't want to reflow code.
    (unless (in-comment)
      (end-of-line)) ; Try to get inside a comment
    (if (in-comment) nil t)))

;;;###autoload
(define-derived-mode literate-idris-mode idris-mode "LitIdris"
  "As `idris-mode' but for literate scripts."
  (setq idris-literate
        (save-excursion
          (goto-char (point-min))
          (cond
           ((re-search-forward "^\\\\\\(begin\\|end\\){code}$" nil t) 'tex)
           ((re-search-forward "^>" nil t) 'bird)
           (t idris-literate-default))))
  (if (eq idris-literate 'bird)
      ;; fill-comment-paragraph isn't much use there, and even gets confused
      ;; by the syntax-table text-properties we add to mark the first char
      ;; of each line as a comment-starter.
      (set (make-local-variable 'fill-paragraph-handle-comment) nil))
  (set (make-local-variable 'mode-line-process)
       '("/" (:eval (symbol-name idris-literate)))))

;; (defvar haskell-saved-check-command nil
;;   "Internal use.")

;; ;; Like Python.  Should be abstracted, sigh.
;; (defun haskell-check (command)
;;   "Check a Haskell file (default current buffer's file).
;; Runs COMMAND, a shell command, as if by `compile'.
;; See `haskell-check-command' for the default."
;;   (interactive
;;    (list (read-string "Checker command: "
;; 		      (or haskell-saved-check-command
;; 			  (concat haskell-check-command " "
;; 				  (let ((name (buffer-file-name)))
;; 				    (if name
;; 					(file-name-nondirectory name))))))))
;;   (setq haskell-saved-check-command command)
;;   (require 'compile)
;;   (save-some-buffers (not compilation-ask-about-save) nil)
;;   (if (fboundp 'compilation-start)
;;       (compilation-start command)
;;     (compile-internal command "No more errors")))

;; (autoload 'flymake-init-create-temp-buffer-copy "flymake")

;; (defun haskell-flymake-init ()
;;   "Flymake init function for Haskell.
;; To be added to `flymake-init-create-temp-buffer-copy'."
;;   (let ((checker-elts (split-string haskell-saved-check-command)))
;;     (list (car checker-elts)
;; 	  (append (cdr checker-elts)
;; 		  (list (flymake-init-create-temp-buffer-copy
;; 			 'flymake-create-temp-inplace))))))

;; (eval-after-load "flymake"
;;   '(add-to-list 'flymake-allowed-file-name-masks
;; 		'("\\.l?hs\\'" haskell-flymake-init)))

;; Provide ourselves:

(provide 'idris-mode)
