(add-to-list 'load-path (or (file-name-directory load-file-name) (car load-path)))

(autoload 'idris-mode "idris-mode" "\
Major mode for editing Idris programs.
Blank lines separate paragraphs, comments start with `-- '.
\\<idris-mode-map>
Module X is activated using the command `turn-on-X'.  For example,
`idris-indent' is activated using `turn-on-idris-indent'.
For more information on a module, see the help for its `X-mode'
function.  Some modules can be deactivated using `turn-off-X'.  (Note
that `idris-doc' is irregular in using `turn-(on/off)-idris-doc-mode'.)

Use `idris-version' to find out what version this is.

Invokes `idris-mode-hook'.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.idr" . idris-mode))

;; (defalias 'run-idris 'switch-to-idris)

;; (autoload 'switch-to-idris "inf-idris" "\
;; Show the inferior-idris buffer.  Start the process if needed.

;; \(fn &optional ARG)" t nil)

(autoload 'inferior-idris-load-file "inf-idris" "\
Pass the current buffer's file to the inferior haskell process.
If prefix arg \\[universal-argument] is given, just reload the previous file.

\(fn &optional RELOAD)" t nil)

(autoload 'inferior-idris-type "inf-idris" "\
Query the idris process for the type of the given expression.
If optional argument `insert-value' is non-nil, insert the type above point
in the buffer.  This can be done interactively with the \\[universal-argument] prefix.
The returned info is cached for reuse by `idris-doc-mode'.

\(fn EXPR &optional INSERT-VALUE)" t nil)

(autoload 'inferior-idris-info "inf-idris" "\
Query the idris process for the info of the given expression.

\(fn SYM)" t nil)

;; (autoload 'inferior-haskell-find-definition "inf-haskell" "\
;; Attempt to locate and jump to the definition of the given expression.

;; \(fn SYM)" t nil)

(autoload 'idris-indent-mode "idris-indent" "\
``Intelligent'' Idris indentation mode.
This deals with the layout rule of Idris.
\\[idris-indent-cycle] starts the cycle which proposes new
possibilities as long as the TAB key is pressed.  Any other key
or mouse click terminates the cycle and is interpreted except for
RET which merely exits the cycle.
Other special keys are:
    \\[idris-indent-insert-equal]
      inserts an =
    \\[idris-indent-insert-guard]
      inserts an |
    \\[idris-indent-insert-otherwise]
      inserts an | otherwise =
these functions also align the guards and rhs of the current definition
    \\[idris-indent-insert-where]
      inserts a where keyword
    \\[idris-indent-align-guards-and-rhs]
      aligns the guards and rhs of the region
    \\[idris-indent-put-region-in-literate]
      makes the region a piece of literate code in a literate script

Invokes `idris-indent-hook' if not nil.

\(fn &optional ARG)" t nil)
