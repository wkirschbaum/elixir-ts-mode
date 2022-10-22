;;; elixir-mode.el --- Elixir support for Emacs -*- lexical-binding: t -*-

;; Examples of AST
;;https://github.com/elixir-lang/tree-sitter-elixir/blob/main/test/corpus/integration/function_definition.txt

(require 'cl-lib)
(require 'treesit)

(ignore-errors
  (unload-feature 'elixir-mode))

(defgroup elixir nil
  "Major mode for editing Elixir code."
  :tag "Elixir"
  :prefix "elixir-"
  :group 'languages)

(defcustom elixir-indent-level 2
  "Indentation of Elixir statements."
  :type 'integer
  :safe 'integerp)

(defcustom elixir-use-tree-sitter t
  "If non-nil, `elixir-mode' tries to use tree-sitter.
Currently `elixir-mode' uses tree-sitter for font-locking, imenu,
and movement functions."
  :type 'boolean
  :version "29.1")

;; Custom faces match highlights.scm as close as possible
;; to help with updates

(defface elixir-font-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "For use with @keyword tag.")

(defface elixir-font-comment-doc-face
  '((t (:inherit font-lock-doc-face)))
  "For use with @comment.doc tag.")

(defface elixir-font-comment-doc-identifier-face
  '((t (:inherit font-lock-doc-face)))
  "For use with @comment.doc tag.")

(defface elixir-font-comment-doc-attribute-face
  '((t (:inherit font-lock-doc-face)))
  "For use with @comment.doc.__attribute__ tag.")

(defface elixir-font-attribute-face
  '((t (:inherit font-lock-preprocessor-face)))
  "For use with @attribute tag.")

(defface elixir-font-operator-face
  '((t (:inherit default)))
  "For use with @operator tag.")

(defface elixir-font-constant-face
  '((t (:inherit font-lock-constant-face)))
  "For use with @constant tag.")

(defface elixir-font-number-face
  '((t (:inherit default)))
  "For use with @number tag.")

(defface elixir-font-module-face
  '((t (:inherit font-lock-type-face)))
  "For use with @module tag.")

(defface elixir-font-punctuation-face
  '((t (:inherit font-lock-keyword-face)))
  "For use with @punctuation tag.")

(defface elixir-font-punctuation-delimiter-face
  '((t (:inherit font-lock-keyword-face)))
  "For use with @punctuation.delimiter tag.")

(defface elixir-font-punctuation-bracket-face
  '((t (:inherit font-lock-keyword-face)))
  "For use with @punctuation.bracket.")

(defface elixir-font-punctuation-special-face
  '((t (:inherit font-lock-variable-name-face)))
  "For use with @punctuation.special tag.")

(defface elixir-font-embedded-face
  '((t (:inherit default)))
  "For use with @embedded tag.")

(defface elixir-font-string-face
  '((t (:inherit font-lock-string-face)))
  "For use with @string tag.")

(defface elixir-font-string-escape-face
  '((t (:inherit font-lock-regexp-grouping-backslash)))
  "For use with Reserved keywords.")

(defface elixir-font-string-regex-face
  '((t (:inherit font-lock-string-face)))
  "For use with @string.regex tag.")

(defface elixir-font-string-special-face
  '((t (:inherit font-lock-string-face)))
  "For use with @string.special tag.")

(defface elixir-font-string-special-symbol-face
  '((t (:inherit font-lock-builtin-face)))
  "For use with @string.special.symbol tag.")

(defface elixir-font-function-face
  '((t (:inherit font-lock-function-name-face)))
  "For use with @function tag.")

(defface elixir-font-sigil-name-face
  '((t (:inherit font-lock-string-face)))
  "For use with @__name__ tag.")

(defface elixir-font-variable-face
  '((t (:inherit default)))
  "For use with @variable tag.")

(defface elixir-font-constant-builtin-face
  '((t (:inherit font-lock-keyword-face)))
  "For use with @constant.builtin tag.")

(defface elixir-font-comment-face
  '((t (:inherit font-lock-comment-face)))
  "For use with @comment tag.")

(defface elixir-font-comment-unused-face
  '((t (:inherit font-lock-comment-face)))
  "For use with @comment.unused tag.")

(defface elixir-font-error-face
  '((t (:inherit error)))
  "For use with @comment.unused tag.")

;; Faces end

(defconst elixir--definition-keywords
  '("def" "defdelegate" "defexception" "defguard" "defguardp" "defimpl" "defmacro" "defmacrop" "defmodule" "defn" "defnp" "defoverridable" "defp" "defprotocol" "defstruct"))

(defconst elixir--definition-keywords-re
  (concat "^" (regexp-opt elixir--definition-keywords) "$"))

(defconst elixir--kernel-keywords
  '("alias" "case" "cond" "else" "for" "if" "import" "quote" "raise" "receive" "require" "reraise" "super" "throw" "try" "unless" "unquote" "unquote_splicing" "use" "with"))

(defconst elixir--kernel-keywords-re
  (concat "^" (regexp-opt elixir--kernel-keywords) "$"))

(defconst elixir--builtin-keywords
  '("__MODULE__" "__DIR__" "__ENV__" "__CALLER__" "__STACKTRACE__"))

(defconst elixir--builtin-keywords-re
  (concat "^" (regexp-opt elixir--builtin-keywords) "$"))


(defconst elixir--doc-keywords
  '("moduledoc" "typedoc" "doc"))

(defconst elixir--doc-keywords-re
  (concat "^" (regexp-opt elixir--doc-keywords) "$"))

(defconst elixir--reserved-keywords
  '("when" "and" "or" "not" "in"
    "not in" "fn" "do" "end" "catch" "rescue" "after" "else"))

(defconst elixir--reserved-keywords
  '("when" "and" "or" "not" "in"
    "not in" "fn" "do" "end" "catch" "rescue" "after" "else"))

(defconst elixir--reserved-keywords-re
  (concat "^" (regexp-opt elixir--reserved-keywords) "$"))

(defconst elixir--reserved-keywords-vector
  (apply #'vector elixir--reserved-keywords))

;; reference:
;; https://github.com/elixir-lang/tree-sitter-elixir/blob/main/queries/highlights.scm
(defvar elixir--treesit-font-lock-settings
  (treesit-font-lock-rules
   :language 'elixir
   :feature 'minimal
   `(
     (comment) @elixir-font-comment-face

     ;; (string
     ;;  [
     ;;   quoted_end: _ @elixir-font-string-face
     ;;   quoted_start: _ @elixir-font-string-face
     ;;  (quoted_content) @elixir-font-string-face
     ;;  (interpolation
     ;;   "#{"
     ;;   @elixir-font-string-escape-face
     ;;   "}" @elixir-font-string-escape-face)
     ;;  ])


     [(string) (charlist)] @font-lock-string-face
     (interpolation) @default ; color everything in substitution white
     (interpolation ["#{" "}"] @font-lock-constant-face)

     ,elixir--reserved-keywords-vector @elixir-font-keyword-face

     (unary_operator
      operator: "@" @elixir-font-comment-doc-attribute-face
      operand: (call
                target: (identifier) @elixir-font-comment-doc-identifier-face
                ;; arguments can be optional, but not sure how to specify
                ;; so adding another entry without arguments
                ;; if we don't handle then we don't apply font
                ;; and the non doc fortification query will take specify
                ;; a more specific font which takes precedence
                (arguments
                 [
                  ;; (string) @elixir-font-comment-doc-face
                  (charlist) @elixir-font-comment-doc-face
                  (sigil) @elixir-font-comment-doc-face
                  (boolean) @elixir-font-comment-doc-face
                  ]))
      (:match ,elixir--doc-keywords-re @elixir-font-comment-doc-identifier-face))

     (unary_operator
      operator: "@" @elixir-font-comment-doc-attribute-face
      operand: (call
                target: (identifier) @elixir-font-comment-doc-identifier-face)
      (:match ,elixir--doc-keywords-re @elixir-font-comment-doc-identifier-face))

     (unary_operator operator: "@" @elixir-font-attribute-face
                     operand: [
                               (identifier)  @elixir-font-attribute-face
                               (call target: (identifier)  @elixir-font-attribute-face)
                               (boolean)  @elixir-font-attribute-face
                               (nil)  @elixir-font-attribute-face
                               ])

     (unary_operator operator: "&") @elixir-font-function-face
     (operator_identifier) @elixir-font-operator-face

     ;; these are operators, should we mark them as keywords?
     (binary_operator
      operator: _ @elixir-font-keyword-face
      (:match ,elixir--reserved-keywords-re @elixir-font-keyword-face))

     (binary_operator operator: _ @elixir-font-operator-face)
     (dot operator: _ @elixir-font-operator-face)
     (stab_clause operator: _ @elixir-font-operator-face)

     [(boolean) (nil)] @elixir-font-constant-face
     [(integer) (float)] @elixir-font-number-face
     (alias) @elixir-font-module-face
     (call target: (dot left: (atom) @elixir-font-module-face))
     (char) @elixir-font-constant-face
     [(atom) (quoted_atom)] @elixir-font-module-face
     [(keyword) (quoted_keyword)] @elixir-font-string-special-symbol-face

     (call
      target: (identifier) @elixir-font-keyword-face
      (:match ,elixir--definition-keywords-re @elixir-font-keyword-face))
     (call
      target: (identifier) @elixir-font-keyword-face
      (:match ,elixir--kernel-keywords-re @elixir-font-keyword-face))
     (call
      target: [(identifier) @elixir-font-function-face
               (dot right: (identifier) @elixir-font-function-face)])
     (call
      target: (identifier) @elixir-font-keyword-face
      (arguments
       [
        (identifier) @elixir-font-function-face
        (binary_operator
         left: (identifier) @elixir-font-function-face
         operator: "when")
        ])
      (:match ,elixir--definition-keywords-re @elixir-font-keyword-face))
     (call
      target: (identifier) @elixir-font-keyword-face
      (arguments
       (binary_operator
        operator: "|>"
        right: (identifier) @elixir-font-variable-face))
      (:match ,elixir--definition-keywords-re @elixir-font-keyword-face))

     (binary_operator operator: "|>" right: (identifier) @elixir-font-function-face)
     ((identifier) @elixir-font-constant-builtin-face
      (:match ,elixir--builtin-keywords-re @elixir-font-constant-builtin-face))
     ((identifier) @elixir-font-comment-unused-face
      (:match "^_" @elixir-font-comment-unused-face))
     (identifier) @elixir-font-variable-face
     ["%"] @elixir-font-punctuation-face
     ["," ";"] @elixir-font-punctuation-delimiter-face
     ["(" ")" "[" "]" "{" "}" "<<" ">>"] @elixir-font-punctuation-bracket-face

     ;; (charlist
     ;;  [
     ;;   quoted_end: _ @elixir-font-string-face
     ;;   quoted_start: _ @elixir-font-string-face
     ;;  (quoted_content) @elixir-font-string-face
     ;;  (interpolation
     ;;   "#{"
     ;;   @elixir-font-string-escape-face
     ;;   "}" @elixir-font-string-escape-face)
     ;;  ])
     ;; (string
     ;;  [
     ;;   quoted_end: _ @elixir-font-string-face
     ;;   quoted_start: _ @elixir-font-string-face
     ;;  (quoted_content) @elixir-font-string-face
     ;;  (interpolation
     ;;   "#{"
     ;;   @elixir-font-string-escape-face
     ;;   "}" @elixir-font-string-escape-face)
     ;;  ])
     )
   :language 'elixir
   :feature 'moderate
   :override t
   `(
     (sigil
      (sigil_name) @elixir-font-sigil-name-face
      quoted_start: _ @elixir-font-string-special-face
      quoted_end: _ @elixir-font-string-special-face) @elixir-font-string-special-face
     (sigil
      (sigil_name) @elixir-font-sigil-name-face
      quoted_start: _ @elixir-font-string-face
      quoted_end: _ @elixir-font-string-face
      (:match "^[sS]$" @elixir-font-sigil-name-face)) @elixir-font-string-face
     (sigil
      (sigil_name) @elixir-font-sigil-name-face
      quoted_start: _ @elixir-font-string-regex-face
      quoted_end: _ @elixir-font-string-regex-face
      (:match "^[rR]$" @elixir-font-sigil-name-face)) @elixir-font-string-regex-face
     )
   :language 'elixir
   :feature 'full
   :override t
   `((escape_sequence) @elixir-font-string-escape-face)
   )
  "Tree-sitter font-lock settings.")

(defun elixir--treesit-capture-defun ()
  (interactive)
  (message
   "%s"
   (treesit-query-capture
    (treesit-buffer-root-node)
    "(call target: (identifier) @ident)")))

(defun elixir--treesit-find-parent-do-block (&optional node)
  (let ((node (or node (treesit-node-at (point))))
        (result nil))
    (while (and node (not result))
      (if (equal (treesit-node-type node) "do_block")
          (setq result node)
        (setq node (treesit-node-parent node))))
    result))

(defun elixir--treesit-backward-up-list ()
  (lambda (node _parent _bol &rest _)
    (let ((parent (elixir--treesit-find-parent-do-block node)))
      (if parent
          (save-excursion
            (goto-char (treesit-node-start parent))
            (back-to-indentation)
            (point))
        nil))))

(defvar elixir--treesit-indent-rules
  (let ((offset elixir-indent-level))
    `((elixir
       (no-node (elixir--treesit-backward-up-list) ,offset)
       ((node-is "}") parent-bol 0)
       ((node-is ")") parent-bol 0)
       ((node-is "]") parent-bol 0)
       ((node-is ">") parent-bol 0)
       ((node-is "]") parent-bol 0)
       ((node-is "|>") parent-bol 0)
       ((node-is "|") parent-bol 0)
       ((node-is "end") parent-bol 0)
       ((node-is "else_block") parent-bol 0)
       ((node-is "stab_clause") parent-bol ,offset)
       ((node-is "rescue_block") parent-bol 0)
       ((node-is ".") parent-bol ,offset)
       ((parent-is "body") parent-bol ,offset)
       ((parent-is "sigil") parent-bol 0)
       ((parent-is "string") parent-bol 0)
       ((parent-is "tuple") parent-bol ,offset)
       ((parent-is "do_block") parent-bol ,offset)
       ((parent-is "else_block") parent-bol ,offset)
       ((parent-is "stab_clause") parent-bol ,offset)
       ((parent-is "arguments") parent-bol ,offset)
       ((parent-is "list") parent-bol ,offset)
       ((parent-is "keywords") first-sibling 0)
       ((parent-is "binary_operator") parent ,offset)
       ))))

;; (defun elixir--treesit-current-defun (&optional include-type)
;;   "Find current Elixir function. Optional argument INCLUDE-TYPE indicates to include the type of the defun."
;;   (let ((node (treesit-node-at (point)))
;;         (name-list ()))
;;     (cl-loop while node
;;              if (pcase (treesit-node-type node)
;;                   ("call" (elixir--treesit-defun node)))
;;              do (push (elixir--treesit-defun node) name-list)
;;              do (setq node (treesit-node-parent node))
;;              finally return (concat (if include-type
;;                                         (format "%s " (treesit-node-type node))
;;                                       "")
;;                                     (string-join name-list ".")))))


(defun elixir--imenu-item-parent-label (type name)
  (cond
   ((equal type "defmodule") "Module")
   (t "")))

(defun elixir--imenu-item-label (type name)
  (format "%s %s" type name))

(defun elixir--imenu-jump-label (_type name)
  (format "%s" name))

(defun elixir--imenu-treesit-create-index (&optional node)
  "Return tree Imenu alist for the current Elixir buffer or NODE tree."
  (let* ((node (or node (treesit-buffer-root-node 'elixir)))
         (tree (treesit-induce-sparse-tree
                node
                (rx (seq bol (or "call") eol)))))
    (elixir--imenu-treesit-create-index-from-tree tree)))

(defun elixir--imenu-treesit-create-index-from-tree (node)
  (let* ((ts-node (car node))
         (children (cdr node))
         (subtrees (mapcan #'elixir--imenu-treesit-create-index-from-tree children))
         (type (elixir--treesit-defun-type ts-node))
         (name (when type (elixir--treesit-defun-name ts-node)))
         (marker (when ts-node
                   (set-marker (make-marker)
                               (treesit-node-start ts-node)))))
    (cond ((null ts-node) subtrees)
          ((null type) subtrees)
          (subtrees (let ((parent-label (funcall 'elixir--imenu-item-parent-label type name))
                          (jump-label (funcall 'elixir--imenu-jump-label type name)))
                      `((,parent-label ,(cons jump-label marker) ,@subtrees))))
          (t (let ((label (funcall 'elixir--imenu-item-label type name)))
               (list (cons label marker)))))))

(defvar elixir-query)
(setq elixir-query "(call target: (identifier) (arguments [(alias) @name (identifier) @name]))")

(defun elixir--treesit-query-defun ()
  "Elixir treesit function query."
  `(call
     target: (identifier) @type
     (arguments
      [
       (alias) @name
       (identifier) @name
       (call target: (identifier) @name)
       (binary_operator
        left: (call target: (identifier) @name)
        operator: "when")
       ])
     (:match ,elixir--definition-keywords-re @type)
     )
  )

(defun elixir--treesit-defun (node)
  "Get the module name from the NODE if exists."
  (let ((name
         (format
          "(%s)"
          (treesit-query-expand (elixir--treesit-query-defun)))))
    (treesit-query-capture node name)))

(defun elixir--treesit-defun-name (&optional node)
  "Get the module name from the NODE if exists."
  (let* ((node (or node (elixir--treesit-largest-node-at-point)))
        (name-node (alist-get 'name (elixir--treesit-defun node))))
    (when name-node (treesit-node-text name-node))))

(defun elixir--treesit-defun-type (&optional node)
  "Get the module name from the NODE if exists."
  (let* ((node (or node (elixir--treesit-largest-node-at-point)))
        (name-node (alist-get 'type (elixir--treesit-defun node))))
    (when name-node (treesit-node-text name-node))))


(defun elixir--treesit-largest-node-at-point ()
  (let* ((node-at-point (treesit-node-at (point)))
         (node-list
          (cl-loop for node = node-at-point
                   then (treesit-node-parent node)
                   while node
                   if (eq (treesit-node-start node)
                          (point))
                   collect node))
         (largest-node (car (last node-list))))
    (if (null largest-node)
        (treesit-node-at (point))
      largest-node)))

(defun elixir-backward-sexp (&optional arg)
  "Move backwards across expressions.  With ARG, do it that many times.  Negative arg -N means move forwards N times."
  (interactive "^p")
  (elixir-forward-sexp (- arg)))

(defun elixir--treesit-real-parent (&optional node)
  (let ((child (or node (treesit-node-at (point)))))
    (treesit-node-parent child)))

(defun elixir-parent-goto (&optional arg)
  (interactive "^P")
  (if (> (or arg 1) 0)
      (goto-char (treesit-node-start (elixir--treesit-real-parent)))
    (goto-char (treesit-node-end (elixir--treesit-real-parent)))))

(defun elixir-sibling-goto (&optional arg)
  (interactive)
  (message "%s" (treesit-node-index (elixir--treesit-largest-node-at-point)))
  (forward-comment (+ (point-max)))
  (let ((node (elixir--treesit-largest-node-at-point)))
    (goto-char (treesit-node-end node)))
  (forward-comment (+ (point-max))))

(defun elixir-next-sibling ()
  (interactive)
  (forward-comment (point-max))
  (goto-char
   (treesit-node-end
    (treesit-search-subtree
     (treesit-node-parent (elixir--treesit-largest-node-at-point))
     "call"))))

(defun elixir--treesit-beginning-of-defun (&optional arg)
  "Tree-sitter `beginning-of-defun' function.
ARG is the same as in `beginning-of-defun."
  (let ((arg (or arg 1)))
    (if (> arg 0)
        ;; Go backward.
        (while (and (> arg 0)
                    (treesit-search-forward-goto
                     (rx (or "call")) 'start nil t))
          (setq arg (1- arg)))
      ;; Go forward.
      (while (and (< arg 0)
                  (treesit-search-forward-goto
                   (rx (or "call")) 'start))
        (setq arg (1+ arg))))))

(defun elixir--treesit-end-of-defun (&optional arg)
  "Tree-sitter `end-of-defun' function.
ARG is the same as in `end-of-defun."
  (treesit-search-forward-goto (rx (or "call")) 'end))

(defun elixir-forward-sexp (&optional arg)
  "Move forward across expressions.  With ARG, do it that many times.  Negative arg -N means move backward N times."
  (interactive "^p")
  (if (> arg 0)
      (progn
        (forward-comment (point-max))
        (let ((next (elixir--treesit-largest-node-at-point)))
          (when next (goto-char (treesit-node-end next)))))
    (progn
      (forward-comment (point-max))
      (let ((largest-node (elixir--treesit-largest-node-at-point)))
        (goto-char
         (let ((prev-node (treesit-node-prev-sibling largest-node)))
           (if prev-node (treesit-node-start prev-node) (point))))))))

(defvar elixir-mode-syntax-table
  (let ((table (make-syntax-table)))

    ;; Note that ?_ might be better as class "_", but either seems to
    ;; work:
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?? "w" table)
    (modify-syntax-entry ?~ "w" table)
    (modify-syntax-entry ?! "_" table)
    (modify-syntax-entry ?' "\"'" table)
    (modify-syntax-entry ?\" "\"\"" table)
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?: "_" table)
    (modify-syntax-entry ?@ "_" table)
    table)
  "Elixir mode syntax table.")


;;;###autoload
(define-derived-mode elixir-mode prog-mode "Elixir"
  :group 'elixir

  ;; Treesit-mode.
  (setq-local treesit-mode-supported t)
  (setq-local treesit-required-languages '(elixir))
  (setq-local treesit-simple-indent-rules elixir--treesit-indent-rules)
  (setq-local treesit-font-lock-settings elixir--treesit-font-lock-settings)
  (setq-local treesit-font-lock-feature-list '((minimal) (moderate) (full)))

  ;; (setq-local treesit-defun-type-regexp (rx (or "call")))
  (setq-local beginning-of-defun-function 'elixir--treesit-beginning-of-defun)
  (setq-local end-of-defun-function 'elixir--treesit-end-of-defun)


  ;; Navigation

  (setq-local treesit-imenu-function #'elixir--imenu-treesit-create-index)

  (cond
   ((treesit-ready-p '(elixir))
    (treesit-mode))
   (t
    (message "Tree-sitter for Elixir isn't available")))

  ;; Comments
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local comment-end "")

  ;; TODO: check what impact parse-sexps have
  ;; (setq-local parse-sexp-lookup-properties t)
  ;; (setq-local parse-sexp-ignore-comments t)

  ;; (setq-local syntax-propertize-function elixir-syntax-propertize-function)

  ;; (if (and elixir-use-tree-sitter
  ;;          (treesit-can-enable-p))
  ;;     (progn
  ;;       ;; (treesit-parser-create 'elixir)

  ;;       (setq-local font-lock-keywords-only t) ; does not seem to work
  ;;       ;; so using no defaults for now
  ;;       ;; (setq-local font-lock-defaults '(nil t))

  ;;       (setq-local treesit-font-lock-feature-list '((minimal) (moderate) (full)))
  ;;       (setq-local treesit-font-lock-settings elixir--treesit-font-lock-settings)
  ;;       (treesit-font-lock-enable)


  ;;       (setq-local treesit-simple-indent-rules elixir--treesit-indent-rules)
  ;;       (setq-local indent-line-function #'treesit-indent)

  ;;       (setq-local beginning-of-defun-function #'elixir--treesit-beginning-of-defun)
  ;;       (setq-local end-of-defun-function #'elixir--treesit-end-of-defun)

  ;;       (setq-local forward-sexp-function #'elixir-forward-sexp)

  ;;       (setq-local imenu-create-index-function #'elixir--imenu-treesit-create-index)
  )


;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.elixir\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("mix\\.lock" . elixir-mode)))

(provide 'elixir-mode)
;;; elixir-custom-mode.el ends here





