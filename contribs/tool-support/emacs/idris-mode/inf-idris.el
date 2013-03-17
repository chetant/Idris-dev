(require 'comint)
(require 'shell)			;For directory tracking.
(require 'compile)
;; (require 'haskell-mode)
(eval-when-compile (require 'cl))

;; XEmacs compatibility.

(unless (fboundp 'subst-char-in-string)
  (defun subst-char-in-string (fromchar tochar string &optional inplace)
    ;; This is Idris-mode, we don't want no stinkin' `aset'.
    (apply 'string (mapcar (lambda (c) (if (eq c fromchar) tochar c)) string))))

(unless (fboundp 'make-temp-file)
  (defun make-temp-file (prefix &optional dir-flag)
    (catch 'done
      (while t
        (let ((f (make-temp-name (expand-file-name prefix (temp-directory)))))
          (condition-case ()
              (progn
                (if dir-flag (make-directory f)
                  (write-region "" nil f nil 'silent nil))
                (throw 'done f))
            (file-already-exists t)))))))

(unless (fboundp 'replace-regexp-in-string)
  (defun replace-regexp-in-string (regexp rep string)
    (replace-in-string string regexp rep)))

(defcustom idris-program-name
  (or (cond
       ((not (fboundp 'executable-find)) nil)
       ((executable-find "idris") "idris"))
      "idris")
  "The name of the command to start the inferior Idris process.
The command can include arguments."
  ;; Custom only supports the :options keyword for a few types, e.g. not
  ;; for string.
  ;; :options '("hugs \"+.\"" "ghci")
  :group 'idris
  :type '(choice string (repeat string)))

;; (defconst inferior-idris-info-xref-re
;;   "\t-- Defined at \\(.+\\):\\([0-9]+\\):\\([0-9]+\\)\\(?:-\\([0-9]+\\)\\)?$")

;; (defconst inferior-idris-module-re
;;   "\t-- Defined in \\(.+\\)$"
;;   "Regular expression for matching module names in :info.")

(defcustom inferior-idris-find-project-root t
  "If non-nil, try and find the project root directory of this file.
This will either look for a Cabal file or a \"module\" statement in the file."
  :group 'idris
  :type 'boolean)

(defconst inferior-idris-error-regexp-alist
  `(
    ("^user error (\\(.*?\\):\\([0-9]+\\):.*?column \\([0-9]+\\).*?)" 1 2 3)
    ("^user error (\\(.*?\\):\\([0-9]+\\):.*?)" 1 2)
    ("^\\(.*?\\):\\([0-9]+\\):.*?" 1 2)
    ))

(define-derived-mode inferior-idris-mode comint-mode "Inf-Idris"
  "Major mode for interacting with an inferior Idris process."
  (set (make-local-variable 'comint-prompt-regexp)
       "^\\*?.*> ")
  (set (make-local-variable 'comint-input-autoexpand) nil)

  ;; marks if we've seen the prompt yet
  (add-hook 'comint-output-filter-functions 'inferior-idris-spot-prompt nil t)

  ;; Setup directory tracking.
  (set (make-local-variable 'shell-cd-regexp) ":cd")

  (condition-case nil
      (shell-dirtrack-mode 1)
    (error      ;The minor mode function may not exist or not accept an arg.
     (set (make-local-variable 'shell-dirtrackp) t)
     (add-hook 'comint-input-filter-functions 'shell-directory-tracker
               nil 'local)))

  ;; Setup `compile' support so you can just use C-x ` and friends.
  (set (make-local-variable 'compilation-error-regexp-alist)
       inferior-idris-error-regexp-alist)
  (set (make-local-variable 'compilation-first-column) 0) ;GHCI counts from 0.
  (if (and (not (boundp 'minor-mode-overriding-map-alist))
           (fboundp 'compilation-shell-minor-mode))
      ;; If we can't remove compilation-minor-mode bindings, at least try to
      ;; use compilation-shell-minor-mode, so there are fewer
      ;; annoying bindings.
      (compilation-shell-minor-mode 1)
    ;; Else just use compilation-minor-mode but without its bindings because
    ;; things like mouse-2 are simply too annoying.
    (compilation-minor-mode 1)
    (let ((map (make-sparse-keymap)))
      (dolist (keys '([menu-bar] [follow-link]))
        ;; Preserve some of the bindings.
        (define-key map keys (lookup-key compilation-minor-mode-map keys)))
      (add-to-list 'minor-mode-overriding-map-alist
                   (cons 'compilation-minor-mode map)))))

(defun inferior-idris-string-to-strings (string)
  "Split the STRING into a list of strings."
  (let ((i (string-match "[\"]" string)))
    (if (null i) (split-string string)	; no quoting:  easy
      (append (unless (eq i 0) (split-string (substring string 0 i)))
	      (let ((rfs (read-from-string string i)))
		(cons (car rfs)
		      (inferior-idris-string-to-strings
		       (substring string (cdr rfs)))))))))

(defun inferior-idris-command (arg)
  (inferior-idris-string-to-strings
   (if (null arg) idris-program-name
     (read-string "Command to run idris: " idris-program-name))))

(defvar inferior-idris-buffer nil
  "The buffer in which the inferior process is running.")

(defun inferior-idris-start-process (command)
  "Start an inferior idris process.
With universal prefix \\[universal-argument], prompts for a COMMAND,
otherwise uses `idris-program-name'.
It runs the hook `inferior-idris-hook' after starting the process and
setting up the inferior-idris buffer."
  (interactive (list (inferior-idris-command current-prefix-arg)))
  (setq inferior-idris-buffer
	(apply 'make-comint "idris" (car command) nil (cdr command)))
  (with-current-buffer inferior-idris-buffer
    (inferior-idris-mode)
    (run-hooks 'inferior-idris-hook)))

(defun inferior-idris-process (&optional arg)
  (or (if (buffer-live-p inferior-idris-buffer)
	  (get-buffer-process inferior-idris-buffer))
      (progn
	(let ((current-prefix-arg arg))
	  (call-interactively 'inferior-idris-start-process))
	;; Try again.
	(inferior-idris-process arg))))

;;;###autoload
(defalias 'run-idris 'switch-to-idris)
;;;###autoload
(defun switch-to-idris (&optional arg)
  "Show the inferior-idris buffer.  Start the process if needed."
  (interactive "P")
  (let ((proc (inferior-idris-process arg)))
    (pop-to-buffer (process-buffer proc))))

(eval-when-compile
  (unless (fboundp 'with-selected-window)
    (defmacro with-selected-window (win &rest body)
      `(save-selected-window
         (select-window ,win)
         ,@body))))

(defcustom inferior-idris-wait-and-jump nil
  "If non-nil, wait for file loading to terminate and jump to the error."
  :type 'boolean
  :group 'idris)

(defvar inferior-idris-seen-prompt nil)
(make-variable-buffer-local 'inferior-idris-seen-prompt)

(defun inferior-idris-spot-prompt (string)
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (save-excursion
        (goto-char (process-mark proc))
        (if (re-search-backward comint-prompt-regexp
                                (line-beginning-position) t)
            (setq inferior-idris-seen-prompt t))))))

(defun inferior-idris-wait-for-prompt (proc &optional timeout)
  "Wait until PROC sends us a prompt.
The process PROC should be associated to a comint buffer."
  (with-current-buffer (process-buffer proc)
    (while (progn
             (goto-char comint-last-input-end)
             (not (or inferior-idris-seen-prompt
                      (setq inferior-idris-seen-prompt
                            (re-search-forward comint-prompt-regexp nil t))
                      (not (accept-process-output proc timeout))))))
    (unless inferior-idris-seen-prompt
      (error "Can't find the prompt"))))

(defun inferior-idris-find-project-root (buf)
  (with-current-buffer buf
          (save-excursion
            (goto-char (point-min))
            (let ((case-fold-search nil))
              (when (re-search-forward
                     "^module[ \t]+\\([^- \t\n]+\\.[^- \t\n]+\\)[ \t]+" nil t)
                (message "Found module string!")
                (let* ((dir default-directory)
                       (module (match-string 1))
                       (pos 0))
                  (while (string-match "\\." module pos)
                    (setq pos (match-end 0))
                    (setq dir (expand-file-name ".." dir)))
                  ;; Let's check that the module name matches the file name,
                  ;; otherwise the project root is probably not what we think.
                  (if (eq t (compare-strings
                             (file-name-sans-extension buffer-file-name)
                             nil nil
                             (expand-file-name
                              (replace-regexp-in-string "\\." "/" module)
                              dir)
                             nil nil t))
                      dir
                    ;; If they're not equal, it means the local directory
                    ;; hierarchy doesn't match the module name.  This seems
                    ;; odd, so let's warn the user about it.  May help us
                    ;; debug this code as well.
                    (message "Ignoring inconsistent `module' info: %s in %s"
                             module buffer-file-name)
                    nil)))))))

;;;###autoload
(defun inferior-idris-load-file (&optional reload)
  "Pass the current buffer's file to the inferior idris process.
If prefix arg \\[universal-argument] is given, just reload the previous file."
  (interactive "P")
  ;; Save first, so we're sure that `buffer-file-name' is non-nil afterward.
  (save-buffer)
  (let ((buf (current-buffer))
        (file buffer-file-name)
        (proc (inferior-idris-process)))
    (with-current-buffer (process-buffer proc)
      (compilation-forget-errors)
      (let ((parsing-end (marker-position (process-mark proc)))
            root)
    (if (and inferior-idris-find-project-root
             (setq root (inferior-idris-find-project-root buf)))
          ;; ;; Not sure if it's useful/needed and if it actually works.
          (unless (equal default-directory root)
            (setq default-directory root))
          (setq file (file-relative-name file)))
	(inferior-idris-send-command
         proc (if reload ":reload"
                (concat ":load "
                        ;; Espace the backslashes that may occur in file names.
                        (replace-regexp-in-string "[\\\"]" "\\\\\&" file))))
	;; Move the parsing-end marker *after* sending the command so
	;; that it doesn't point just to the insertion point.
	;; Otherwise insertion may move the marker (if done with
	;; insert-before-markers) and we'd then miss some errors.
	(if (boundp 'compilation-parsing-end)
	    (if (markerp compilation-parsing-end)
		(set-marker compilation-parsing-end parsing-end)
	      (setq compilation-parsing-end parsing-end))))
      (with-selected-window (display-buffer (current-buffer))
        (goto-char (point-max)))
      ;; Use compilation-auto-jump-to-first-error if available.
      ;; (if (and (boundp 'compilation-auto-jump-to-first-error)
      ;;          compilation-auto-jump-to-first-error
      ;;          (boundp 'compilation-auto-jump-to-next))
      ;;     (setq compilation-auto-jump-to-next t)
        (when inferior-idris-wait-and-jump
          (inferior-idris-wait-for-prompt proc)
          (ignore-errors                  ;Don't beep if there were no errors.
            (next-error)))))) ;; )

(defun inferior-idris-send-command (proc str)
  (setq str (concat str "\n"))
  (with-current-buffer (process-buffer proc)
    (inferior-idris-wait-for-prompt proc)
    (goto-char (process-mark proc))
    (insert-before-markers str)
    (move-marker comint-last-input-end (point))
    (setq inferior-idris-seen-prompt nil)
    (comint-send-string proc str)))

(defun inferior-idris-reload-file ()
  "Tell the inferior idris process to reread the current buffer's file."
  (interactive)
  (inferior-idris-load-file 'reload))

;;;###autoload
;;      (list (read-string (if (> (length sym) 0)
;;                             (format "Show type of (default %s): " sym)
;;                           "Show type of: ")
;;                         nil nil sym)
;;            current-prefix-arg)))
(defun inferior-idris-type (expr &optional insert-value)
  "Query the idris process for the type of the given expression.
If optional argument `insert-value' is non-nil, insert the type above point
in the buffer.  This can be done interactively with the \\[universal-argument] prefix.
The returned info is cached for reuse by `idris-doc-mode'."
  (interactive
   (let ((sym (idris-ident-at-point)))
     (list sym current-prefix-arg)))
  (if (string-match "\\`\\s_+\\'" expr) (setq expr (concat "(" expr ")")))
  (let* ((proc (inferior-idris-process))
         (type
          (with-current-buffer (process-buffer proc)
            (let ((parsing-end          ; Remember previous spot.
                   (marker-position (process-mark proc))))
              (inferior-idris-send-command proc (concat ":type " expr))
              ;; Find new point.
              (inferior-idris-wait-for-prompt proc)
              (goto-char (point-max))
              ;; Back up to the previous end-of-line.
              (end-of-line 0)
              ;; Extract the type output
              (buffer-substring-no-properties
               (save-excursion (goto-char parsing-end)
                               (line-beginning-position 2))
               (point))))))
    (if (not (string-match (concat "^\\(" (regexp-quote expr) "[ \t\n]+:[ \t\n]*\\(.\\|\n\\)*\\)")
                           type))
        (error "BOO No type info: %s" type)
      (progn
        (setf type (match-string 1 type))
      ;; ;; Cache for reuse by idris-doc.
      ;; (when (and (boundp 'idris-doc-mode) idris-doc-mode
      ;;            (boundp 'idris-doc-user-defined-ids)
      ;;            ;; Idris-doc only works for idents, not arbitrary expr.
      ;;            (string-match "\\`(?\\(\\s_+\\|\\(\\sw\\|\\s'\\)+\\)?[ \t]*::[ \t]*"
      ;;                          type))
      ;;   (let ((sym (match-string 1 type)))
      ;;     (setq idris-doc-user-defined-ids
      ;;           (cons (cons sym (substring type (match-end 0)))
      ;;                 (delq (assoc sym idris-doc-user-defined-ids)
      ;;                       idris-doc-user-defined-ids)))))

      (if (interactive-p) (message "%s" type))
      (when insert-value
        (beginning-of-line)
        (insert type "\n"))
        type))))

;;;###autoload
(defun inferior-idris-info (sym)
  "Query the idris process for the info of the given expression."
  (interactive
   (let ((sym (idris-ident-at-point)))
     (list sym)))
  (let ((proc (inferior-idris-process)))
    (with-current-buffer (process-buffer proc)
      (let ((parsing-end                ; Remember previous spot.
             (marker-position (process-mark proc))))
        (inferior-idris-send-command proc (concat ":info " sym))
        ;; Find new point.
        (inferior-idris-wait-for-prompt proc)
        (goto-char (point-max))
        ;; Move to previous end-of-line
        (end-of-line 0)
        (let ((result
               (buffer-substring-no-properties
                (save-excursion (goto-char parsing-end)
                                (line-beginning-position 2))
                (point))))
          ;; Move back to end of process buffer
          (goto-char (point-max))
          (if (interactive-p) (message "%s" result))
          result)))))

;; ;;;###autoload
;; (defun inferior-haskell-find-definition (sym)
;;   "Attempt to locate and jump to the definition of the given expression."
;;   (interactive
;;    (let ((sym (haskell-ident-at-point)))
;;      (list (read-string (if (> (length sym) 0)
;;                             (format "Find definition of (default %s): " sym)
;;                           "Find definition of: ")
;;                         nil nil sym))))
;;   (let ((info (inferior-haskell-info sym)))
;;     (if (not (string-match inferior-haskell-info-xref-re info))
;;         (error "No source information available")
;;       (let ((file (match-string-no-properties 1 info))
;;             (line (string-to-number
;;                    (match-string-no-properties 2 info)))
;;             (col (string-to-number
;;                   (match-string-no-properties 3 info))))
;;         (when file
;;           (with-current-buffer (process-buffer (inferior-haskell-process))
;;             ;; The file name is relative to the process's cwd.
;;             (setq file (expand-file-name file)))
;;           ;; Push current location marker on the ring used by `find-tag'
;;           (require 'etags)
;;           (ring-insert find-tag-marker-ring (point-marker))
;;           (pop-to-buffer (find-file-noselect file))
;;           (when line
;;             (goto-line line)
;;             (when col (move-to-column col))))))))

(provide 'inf-idris)
