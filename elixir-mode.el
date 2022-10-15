;;; elixir-mode.el --- Elixir support for Emacs -*- lexical-binding: t -*-

;; Examples of AST
;;https://github.com/elixir-lang/tree-sitter-elixir/blob/main/test/corpus/integration/function_definition.txt


(ignore-errors
    (unload-feature 'elixir-mode))


(require 'cl-lib)
(require 'treesit)

(defgroup elixir nil
  "Major mode for editing Elixir code."
  :tag "Elixir"
  :prefix "elixir-"
  :group 'languages)

(defcustom elixir-indent-level 2
  "Indentation of Elixir statements."
  :type 'integer
  :safe 'integerp)

;; TODO: This goes into infinite loop at eob and does not
;; handle multi-line def's
(defun elixir--treesit-beginning-of-defun (&optional arg)
  (let ((arg (or arg 1)))
    (if (> arg 0)
        ;; Go backward.
        (while (and (> arg 0)
                    (progn
                      (treesit-search-forward-goto "call" 'start nil t t)
                      (back-to-indentation)))
          (setq arg (1- arg)))
      ;; Go forward.
      (while (and (< arg 0)
                  (treesit-search-forward-goto "call" 'end nil nil t))
        (setq arg (1+ arg))))))

;; TODO: This goes into infinite loop at eob and does not
;; handle multi-line def's
(defun elixir--treesit-end-of-defun (&optional arg)
  (let ((arg (or arg 1)))
    (if (< arg 0)
        ;; Go backward.
        (while (and (< arg 0)
                    (treesit-search-forward-goto "call" 'end nil t t))
          (setq arg (1+ arg)))
      ;; Go forward.
      (while (and (> arg 0)
                  (progn
                      (treesit-search-forward-goto "call" 'start nil nil t)
                      (back-to-indentation)))
        (setq arg (1- arg))))))


;; Custom faces match highlights.scm as close as possible
;; to help with updates

(defface elixir-font-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "For use with @keyword tag.")

(defface elixir-font-comment-doc-face
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
  '((t (:inherit font-lock-string-face)))
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

(defface elixir-font-contant-builtin-face
  '((t (:inherit font-lock-keyword-face)))
  "For use with @constant.builtin tag.")

(defface elixir-font-comment-face
  '((t (:inherit font-lock-comment-face)))
  "For use with @comment tag.")

(defface elixir-font-comment-unused-face
  '((t (:inherit font-lock-comment-face)))
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

(defconst elixir--reserved-keywords-vector
  (apply #'vector elixir--reserved-keywords))

;; reference:
;; https://github.com/elixir-lang/tree-sitter-elixir/blob/main/queries/highlights.scm
(defvar elixir--treesit-font-lock-settings
  (treesit-font-lock-rules
   :language 'elixir
   `(
     (interpolation "#{" @elixir-font-punctuation-special-face "}" @elixir-font-punctuation-special-face)
     )
   :language 'elixir
   :override t
   `(
     ,elixir--reserved-keywords-vector @elixir-font-keyword-face
     (unary_operator operator: "&" operand: (integer) @elixir-font-operator-face)
     (operator_identifier) @elixir-font-operator-face
     (unary_operator operator: _ @elixir-font-operator-face)
     (binary_operator operator: _ @elixir-font-operator-face)
     (dot operator: _ @elixir-font-operator-face)
     (stab_clause operator: _ @elixir-font-operator-face)
     [(boolean) (nil)] @elixir-font-constant-face
     [(integer) (float)] @elixir-font-number-face
     (alias) @elixir-font-module-face
     (call target: (dot left: (atom) @elixir-font-module-face))
     (char) @elixir-font-constant-face
     (escape_sequence) @elixir-font-string-escape-face
     [
      (atom)
      (quoted_atom)
      (keyword)
      (quoted_keyword)
      ] @elixir-font-string-special-symbol-face
     [(string) (charlist)] @elixir-font-string-face
     (call
      target: (identifier) @elixir-font-keyword-face
      (:match ,elixir--definition-keywords-re @elixir-font-keyword-face))
     (call
      target: (identifier) @elixir-font-keyword-face
      (:match ,elixir--kernel-keywords-re @elixir-font-keyword-face))
     (call
      target: [
                                        ; local
               (identifier) @elixir-font-function-face
                                        ; remote
               (dot
                right: (identifier) @elixir-font-function-face)
               ])
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
     (comment) @elixir-font-comment-face
     ["%"] @elixir-font-punctuation-face
     ["," ";"] @elixir-font-punctuation-delimiter-face
     ["(" ")" "[" "]" "{" "}" "<<" ">>"] @elixir-font-punctuation-bracket-face
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
   :override t
   `(
     (unary_operator
      operator: "@" @elixir-font-comment-doc-attribute-face
      operand: (call
                target: (identifier) @elixir-font-comment-doc-attribute-face
                (arguments
                 [
                  (string) @elixir-font-comment-doc-face
                  (charlist) @elixir-font-comment-doc-face
                  (sigil
                   quoted_start: _ @elixir-font-comment-doc-face
                   quoted_end: _ @elixir-font-comment-doc-face)
                  @elixir-font-comment-doc-face
                  (boolean) @elixir-font-comment-doc-face
                  ]))
      (:match ,elixir--doc-keywords-re @elixir-font-comment-doc-attribute-face))
     (unary_operator operator: "@"  @elixir-font-attribute-face
                     operand: [
                               (identifier)  @elixir-font-attribute-face
                               (call target: (identifier)  @elixir-font-attribute-face)
                               (boolean)  @elixir-font-attribute-face
                               (nil)  @elixir-font-attribute-face
                               ])

     ))
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

(defun elixir--treesit-current-defun (&optional include-type)
  "Find current Elixir function. Optional argument INCLUDE-TYPE indicates to include the type of the defun."
  (let ((node (treesit-node-at (point)))
        (name-list ()))
    (cl-loop while node
             if (pcase (treesit-node-type node)
                  ("call" (elixir--imenu-node-type node)))
             do (push (elixir--imenu-node-name node) name-list)
             do (setq node (treesit-node-parent node))
             finally return (concat (if include-type
                                        (format "%s " (treesit-node-type node))
                                      "")
                                    (string-join name-list ".")))))


(defun elixir--imenu-item-parent-label (_type name)
  (format "%s" name))

(defun elixir--imenu-item-label (type name)
  (format "%s %s" type name))

(defun elixir--imenu-jump-label (_type _name)
  (format "..."))

(defun elixir--imenu-treesit-create-index (&optional node)
  "Return tree Imenu alist for the current Elixir buffer."
  (let* ((node (or node (treesit-buffer-root-node)))
         (tree (treesit-induce-sparse-tree
                node
                (rx (seq bol (or "call") eol)))))
    (elixir--imenu-treesit-create-index-from-tree tree)))

(defun elixir--imenu-treesit-create-index-from-tree (node)
  (let* ((ts-node (car node))
         (children (cdr node))
         (subtrees (mapcan #'elixir--imenu-treesit-create-index-from-tree children))
         (type (when ts-node (elixir--imenu-node-type ts-node)))
         (name (when type (elixir--imenu-node-name ts-node type)))
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

;; (setq elixir-query "(call target: (identifier) @type (arguments [(alias) @name (identifier) @name]) (.match? @type \"^(def|defmodule)$\")")

(defun elixir-treesit-up-sexp ()
  (interactive)
  (let ((largest-node (elixir--treesit-largest-node-at-point)))
    (goto-char (treesit-node-start (treesit-node-parent largest-node)))))

(defun elixir-treesit-next-sibling ()
  (interactive)
  (let* ((largest-node (elixir--treesit-largest-node-at-point))
         (parent (when largest-node (treesit-node-parent largest-node)))
         (index (when parent (treesit-node-index parent))))
    (message "%s" index)
    (goto-char
       (if largest-node
           (treesit-node-end largest-node)
         (treesit-node-end largest-node)))))

(defun elixir-treesit-prev-sibling ()
  (interactive)
  (let ((largest-node (elixir--treesit-largest-node-at-point)))
    (goto-char
     (treesit-node-start
      (let ((prev-node (treesit-node-prev-sibling
                        largest-node t)))
        (if prev-node prev-node largest-node))))))

(defun elixir-treesit-show-largest-node ()
  (interactive)
  (message
   "%s %s"
   (treesit-node-type (elixir--treesit-largest-node-at-point))
   (treesit-node-text (elixir--treesit-largest-node-at-point)))
  )

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

(defun elixir--imenu-node-name (node &optional type)
  ""
  ;; (let ((query "(call target: (identifier) @type (arguments [(alias) @name (identifier) @name]))"))
  ;;   ;; (message "%s"  (treesit-query-capture node query)))
  ;; (message "%s %s" (treesit-node-index node) (treesit-node-type node)))

  (pcase (or type (elixir--imenu-node-type node))
    ((or 'def 'defp) (treesit-node-text
           (treesit-search-subtree
            (treesit-search-subtree node "arguments") "identifier")))
    ((or 'test 'describe) (treesit-node-text
            (treesit-search-subtree node "string")))
    ('module (treesit-node-text
              (treesit-search-subtree node "alias")))))

(defun elixir--imenu-node-type (node)
  ""
  (pcase (treesit-node-text (treesit-search-subtree node "identifier"))
    ("def" 'def)
    ("defp" 'defp)
    ("test" 'test)
    ("describe" 'describe)
    ("defmodule" 'module)))

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

(defun elixir-prev-sibling ()
  (interactive)


)

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

;;;###autoload
(define-derived-mode elixir-mode prog-mode "Elixir"
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local comment-end "")

  (setq-local parse-sexp-lookup-properties t)
  (setq-local parse-sexp-ignore-comments t)


  (if (treesit-can-enable-p)
      (progn
        (treesit-parser-create 'heex)
        (treesit-parser-create 'elixir)

        (setq-local font-lock-defaults '(nil t))
        (setq-local treesit-font-lock-settings
                    elixir--treesit-font-lock-settings)
        (treesit-font-lock-enable)


        (setq-local treesit-simple-indent-rules elixir--treesit-indent-rules)
        (setq-local indent-line-function #'treesit-indent)

        (setq-local beginning-of-defun-function #'elixir--treesit-beginning-of-defun)
        (setq-local end-of-defun-function #'elixir--treesit-end-of-defun)

        (setq-local forward-sexp-function #'elixir-forward-sexp)

        (setq-local imenu-create-index-function #'elixir--imenu-treesit-create-index)
        (add-hook 'which-func-functions #'elixir--treesit-current-defun nil t))))


;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.elixir\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("mix\\.lock" . elixir-mode)))

(provide 'elixir-mode)
;;; elixir-custom-mode.el ends here


