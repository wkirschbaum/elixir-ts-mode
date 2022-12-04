;;; heex-ts-mode.el --- tree-sitter support for Elixir -*- coding: utf-8; lexical-binding: t; -*-

;; Author      : Wilhelm H Kirschbaum
;; Maintainer  : Wilhelm H Kirschbaum
;; Created     : November 2022
;; Keywords    : elixir languages tree-sitter

;;; Commentary:

;; Code:


(ignore-errors
  (unload-feature 'elixir-ts-mode))

(require 'treesit)
(eval-when-compile
  (require 'rx)
  (require 'cl-lib))

(defgroup elixir nil
  "Major mode for editing Elixir code."
  :tag "Elixir"
  :prefix "elixir-"
  :group 'languages)

(defcustom elixir-indent-level 2
  "Indentation of Elixir statements."
  :type 'integer
  :safe 'integerp)

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

(defconst elixir--definition-module
  '("defmodule" "defprotocol"))

(defconst elixir--definition-module-re
  (concat "^" (regexp-opt elixir--definition-module) "$"))

(defconst elixir--definition-function
  '("def" "defp" "defdelegate" "defguard" "defguardp" "defmacro" "defmacrop" "defn" "defnp"))

(defconst elixir--definition-function-re
  (concat "^" (regexp-opt elixir--definition-function) "$"))

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
   :feature 'comment
   '((comment) @elixir-font-comment-face)

   :language 'elixir
   :feature 'string
   :override t
   '([(string) (charlist)] @font-lock-string-face)

   :language 'elixir
   :feature 'string-interpolation
   :override t
   '((string
      [
       quoted_end: _ @elixir-font-string-face
       quoted_start: _ @elixir-font-string-face
       (quoted_content) @elixir-font-string-face
       (interpolation
        "#{" @elixir-font-string-escape-face "}" @elixir-font-string-escape-face
        )
       ])
     (charlist
      [
       quoted_end: _ @elixir-font-string-face
       quoted_start: _ @elixir-font-string-face
       (quoted_content) @elixir-font-string-face
       (interpolation
        "#{" @elixir-font-string-escape-face "}" @elixir-font-string-escape-face
        )
       ])
     )

   :language 'elixir
   :feature 'keyword
   ;; :override `prepend
   `(,elixir--reserved-keywords-vector @elixir-font-keyword-face
                                       ;; these are operators, should we mark them as keywords?
                                       (binary_operator
                                        operator: _ @elixir-font-keyword-face
                                        (:match ,elixir--reserved-keywords-re @elixir-font-keyword-face)))

   :language 'elixir
   :feature 'doc
   :override t
   `((unary_operator
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
                  (string) @elixir-font-comment-doc-face
                  (charlist) @elixir-font-comment-doc-face
                  (sigil) @elixir-font-comment-doc-face
                  (boolean) @elixir-font-comment-doc-face
                  ]))
      (:match ,elixir--doc-keywords-re @elixir-font-comment-doc-identifier-face))
     (unary_operator
      operator: "@" @elixir-font-comment-doc-attribute-face
      operand: (call
                target: (identifier) @elixir-font-comment-doc-identifier-face)
      (:match ,elixir--doc-keywords-re @elixir-font-comment-doc-identifier-face)))


   :language 'elixir
   :feature 'unary-operator
   `((unary_operator operator: "@" @elixir-font-attribute-face
                     operand: [
                               (identifier)  @elixir-font-attribute-face
                               (call target: (identifier)  @elixir-font-attribute-face)
                               (boolean)  @elixir-font-attribute-face
                               (nil)  @elixir-font-attribute-face
                               ])

     (unary_operator operator: "&") @elixir-font-function-face
     (operator_identifier) @elixir-font-operator-face
     )

   :language 'elixir
   :feature 'operator
   '((binary_operator operator: _ @elixir-font-operator-face)
     (dot operator: _ @elixir-font-operator-face)
     (stab_clause operator: _ @elixir-font-operator-face)

     [(boolean) (nil)] @elixir-font-constant-face
     [(integer) (float)] @elixir-font-number-face
     (alias) @elixir-font-module-face
     (call target: (dot left: (atom) @elixir-font-module-face))
     (char) @elixir-font-constant-face
     [(atom) (quoted_atom)] @elixir-font-module-face
     [(keyword) (quoted_keyword)] @elixir-font-string-special-symbol-face)

   :language 'elixir
   :feature 'call
   `((call
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
      (:match ,elixir--definition-keywords-re @elixir-font-keyword-face)))

   :language 'elixir
   :feature 'constant
   `((binary_operator operator: "|>" right: (identifier) @elixir-font-function-face)
     ((identifier) @elixir-font-constant-builtin-face
      (:match ,elixir--builtin-keywords-re @elixir-font-constant-builtin-face))
     ((identifier) @elixir-font-comment-unused-face
      (:match "^_" @elixir-font-comment-unused-face))
     (identifier) @elixir-font-variable-face
     ["%"] @elixir-font-punctuation-face
     ["," ";"] @elixir-font-punctuation-delimiter-face
     ["(" ")" "[" "]" "{" "}" "<<" ">>"] @elixir-font-punctuation-bracket-face)

   :language 'elixir
   :feature 'sigil
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
   :feature 'string-escape
   :override t
   `((escape_sequence) @elixir-font-string-escape-face)
   )
  "Tree-sitter font-lock settings.")

(defun elixir--treesit-find-parent-do-block (&optional node)
  (let ((node (or node (treesit-node-at (point))))
        (result nil))
    (while (and node (not result))
      (if (equal (treesit-node-type node) "do_block")
          (setq result node)
        (setq node (treesit-node-parent node))))
    result))

;; introducing custom queries like this makes things slow
;; so perhaps should be optional somehow
(defvar elixir--anonymous-function-end
  (treesit-query-compile 'elixir '((anonymous_function "end" @end))))

(defvar elixir--operator-parent
  (treesit-query-compile 'elixir '((binary_operator operator: _ @val))))

(defvar elixir--first-argument
  (treesit-query-compile 'elixir "(arguments . (_) @first-child) (tuple . (_) @first-child)"))

(defvar elixir--binary-operator-special
  (treesit-query-compile 'elixir '((binary_operator operator: "<>") @val)))

(defun elixir--indent-parent-bol-p (parent)
  (save-excursion
    (goto-char (treesit-node-start parent))
    (back-to-indentation)
    (and (eq (char-after) ?|) (eq (char-after (1+ (point))) ?>))))

(defvar elixir--treesit-indent-rules
  (let ((offset elixir-indent-level))
    `((elixir
       ((parent-is "source") parent-bol 0)
       ;; ensure we don't indent docs by setting no-indent on quoted_content
       ((parent-is "quoted_content") no-indent 0)
       (no-node parent-bol ,offset)
       ((node-is "|>") parent-bol 0)
       ((node-is "|") parent-bol 0)
       ((node-is "}") parent-bol 0)
       ((node-is ")") (lambda (_node parent &rest _)
          (if (elixir--indent-parent-bol-p parent)
              ;; parent-bol
              (save-excursion
                (goto-char (treesit-node-start parent))
                (back-to-indentation)
                (point))

            ;; grant-parent
            (treesit-node-start (treesit-node-parent parent))))
        0)
       ((node-is "]") parent-bol 0)

       ((node-is "else_block") grand-parent-bol 0)
       ((node-is "catch_block") grand-parent-bol 0)
       ((node-is "rescue_block") grand-parent-bol 0)
       ((node-is "stab_clause") parent-bol ,offset)

       ((query ,elixir--operator-parent) grand-parent 0)
       ((node-is "when") parent 0)

       ((node-is "keywords") parent-bol ,offset)

       ((parent-is "body") parent-bol ,offset)

       ((query ,elixir--first-argument)
        (lambda (_node parent &rest _)
          (if (elixir--indent-parent-bol-p parent)
              ;; parent-bol
              (save-excursion
                (goto-char (treesit-node-start parent))
                (back-to-indentation)
                (point))

            ;; grant-parent
            (treesit-node-start (treesit-node-parent parent))))
          ,offset)

       ((parent-is "arguments")
        (lambda (node parent &rest _)
          ;; grand-parent
          (treesit-node-start
           (treesit-node-child parent 0 t)))
        0)

       ;; ((query ,elixir--binary-operator-special) parent 0)

       ((parent-is "binary_operator") parent ,offset)

        ((node-is "pair") first-sibling 0)
        ((parent-is "tuple") (lambda (_n parent &rest _)
                               (treesit-node-start
                                (treesit-node-child parent 0 t))) 0)

        ((parent-is "list") parent-bol ,offset)
        ((parent-is "pair") parent ,offset)
        ((parent-is "map") parent-bol ,offset)

        ((query ,elixir--anonymous-function-end) parent-bol 0)

        ((node-is "end") grand-parent-bol 0)

        ((parent-is "do_block") grand-parent ,offset)

        ((parent-is "anonymous_function") grand-parent-bol ,offset)
        ((parent-is "else_block") parent ,offset)
        ((parent-is "rescue_block") parent ,offset)
        ((parent-is "catch_block") parent ,offset)
        ))))

(defun elixir--imenu-item-parent-label (_type name)
  (format "%s" name))

(defun elixir--imenu-item-label (type name)
  (format "%s %s" type name))

(defun elixir--imenu-jump-label (_type _name)
  (format "..."))

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

(defvar elixir--treesit-query-module-or-function
  (let ((query `((call
                  target: (identifier) @ignore
                  (arguments (alias) @name)
                  (:match ,elixir--definition-module-re @ignore))

                 (call
                  target: (identifier) @ignore
                  (arguments
                   [
                    (identifier) @name
                    (call target: (identifier) @name)
                    (binary_operator
                     left: (call target: (identifier) @name)
                     operator: "when")
                    ])
                  (:match ,elixir--definition-function-re @ignore)))))
    (treesit-query-compile 'elixir query)))

(defvar elixir--treesit-query-defun
  (let ((query `((call
                  target: (identifier) @type
                  (arguments
                   [
                    (alias) @name
                    (identifier) @name
                    (call target: (identifier)) @name
                    (binary_operator
                     left: (call target: (identifier)) @name
                     operator: "when")
                    ])
                  (:match ,elixir--definition-keywords-re @type)
                  ))))
    (treesit-query-compile 'elixir query)))



(defun elixir--treesit-defun (node)
  "Get the module name from the NODE if exists."
  (treesit-query-capture node elixir--treesit-query-defun))

(defun elixir--treesit-module-or-function (node)
  "Get the module name from the NODE if exists."
  (treesit-query-capture node elixir--treesit-query-module-or-function))


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


(defun elixir--forward-sexp ()
  "Move forward over the sexp."
  (goto-char
   (treesit-node-end
    (goto-char (treesit-node-end
                (treesit-search-forward (treesit-node-at (point)) (rx (or "call"))))))))

(defun elixir--treesit-forward-sexp ()
  "Elixir forward sexp."
  (cl-loop for cursor = (treesit-node-at (point))
           then (treesit-search-forward cursor (rx (or "call")))
           while cursor
           do (goto-char (treesit-node-end cursor))))


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

(defun elixir--treesit-goto-parent ()
  (interactive)
  (goto-char
   (treesit-node-start
    (treesit-node-parent (elixir--treesit-largest-node-at-point)))))

(defun elixir--treesit-defun-p (node)
  "Check if NODE is a defun."
  (elixir--treesit-defun node))


(defun elixir--treesit-find-first-parent-defun (node)
  "Return the top-level parent of NODE matching TYPE.
TYPE is a regexp, this function matches TYPE with each parent's
type."
  (cl-loop for cursor = (treesit-node-parent node)
           then (treesit-node-parent cursor)
           while cursor
           if (elixir--treesit-defun-p cursor)
           do (setq node cursor)
           finally return node))

(defun elixir--treesit-beginning-of-defun (&optional arg)
  "Tree-sitter `beginning-of-defun' function.
ARG is the same as in `beginning-of-defun."
  (let ((arg (or arg 1))
        (node (treesit-node-at (point))))
    (if (> arg 0)
        ;; Go backward.
        (while (and (> arg 0)
                    (setq node (treesit-search-forward-goto
                                node (rx (or "do_block")) t t)))
          (setq node (elixir--treesit-find-first-parent-defun node))
          (setq arg (1- arg)))
      ;; Go forward.
      (while (and (< arg 0)
                  (setq node (treesit-search-forward-goto
                              node (rx (or "call")) t t)))
        (setq node (elixir--treesit-find-first-parent-defun node ))
        (setq arg (1+ arg))))
    (goto-char (treesit-node-start node))))

(defun elixir--treesit-end-of-defun (&optional arg)
  "Tree-sitter `end-of-defun' function.
ARG is the same as in `end-of-defun."
  (elixir--treesit-beginning-of-defun (- (or arg 1))))

;; (defun elixir--treesit-forward-sexp (&optional arg)
;;   "Move forward across expressions.  With ARG, do it that many times.  Negative arg -N means move backward N times."
;;   (interactive "^p")
;;   (if (> arg 0)
;;       (progn
;;         (forward-comment (point-max))
;;         (let ((next (elixir--treesit-largest-node-at-point)))
;;           (when next (goto-char (treesit-node-end next)))))
;;     (progn
;;       (forward-comment (point-max))
;;       (let ((largest-node (elixir--treesit-largest-node-at-point)))
;;         (goto-char
;;          (let ((prev-node (treesit-node-prev-sibling largest-node)))
;;            (if prev-node (treesit-node-start prev-node) (point))))))))

(defvar elixir-ts-mode-syntax-table
  (let ((table (make-syntax-table)))
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
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?: "'" table)
    (modify-syntax-entry ?@ "'" table)
    table)
  "Elixir mode syntax table.")


(defvar elixir--treesit-range-rules
  (treesit-range-rules
   :embed 'heex
   :host 'elixir
   '((sigil
      (sigil_name) @name
      (quoted_content) @cap
      (:match "^H$" @name))
     )))


;;;###autoload
(define-derived-mode elixir-ts-mode prog-mode "Elixir"
  :group 'elixir
  :syntax-table elixir-ts-mode-syntax-table
  "Major mode for editing Elixir code.

\\{elixir-ts-mode-map}"

  (unless (treesit-ready-p 'elixir)
    (error "Tree-sitter for Elixir isn't available"))

  (treesit-parser-create 'elixir)

  ;; Comments
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local comment-end "")

    ;; Electric.
  (setq-local electric-indent-chars
              (append "]" ")" "}" "end" electric-indent-chars))

  ;; Treesit-mode.
  (setq-local treesit-mode-supported t)
  (setq-local treesit-required-languages '(elixir))
  (setq-local treesit-simple-indent-rules elixir--treesit-indent-rules)
  (setq-local treesit-font-lock-settings elixir--treesit-font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '(( comment string )
                ( keyword unary-operator operator doc)
                ( call constant )
                ( sigil string-escape)
                ( string-interpolation )))

  (setq-local treesit-defun-type-regexp (rx (or "do_block")))
  (setq-local beginning-of-defun-function 'elixir--treesit-beginning-of-defun)
  ;; (setq-local end-of-defun-function 'elixir--treesit-end-of-defun)
  ;; (setq-local forward-sexp-function 'elixir--treesit-forward-sexp)

  ;; Navigation

  (setq-local treesit-imenu-function #'elixir--imenu-treesit-create-index)

  (treesit-major-mode-setup))

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.elixir\\'" . elixir-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-ts-mode))
  (add-to-list 'auto-mode-alist '("mix\\.lock" . elixir-ts-mode)))

(provide 'elixir-ts-mode)
;;; elixir-ts-mode.el ends here

