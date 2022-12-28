;;; elixir-ts-mode.el --- tree-sitter support for Elixir -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Wilhelm H Kirschbaum

;; Author           : Wilhelm H Kirschbaum
;; Package-Requires : ((emacs "29"))
;; Created          : November 2022
;; Keywords         : elixir languages tree-sitter

;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Code:

(require 'treesit)
(require 'heex-ts-mode)
(eval-when-compile (require 'rx))

(defgroup elixir nil
  "Major mode for editing Elixir code."
  :group 'languages
  :version "29.1")

(defcustom elixir-ts-mode-indent-offset 2
  "Indentation of Elixir statements."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'elixir)

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

(defconst elixir-ts-mode--definition-keywords
  '("def" "defdelegate" "defexception" "defguard" "defguardp" "defimpl" "defmacro" "defmacrop" "defmodule" "defn" "defnp" "defoverridable" "defp" "defprotocol" "defstruct"))

(defconst elixir-ts-mode--definition-keywords-re
  (concat "^" (regexp-opt elixir-ts-mode--definition-keywords) "$"))

(defconst elixir-ts-mode--kernel-keywords
  '("alias" "case" "cond" "else" "for" "if" "import" "quote" "raise" "receive" "require" "reraise" "super" "throw" "try" "unless" "unquote" "unquote_splicing" "use" "with"))

(defconst elixir-ts-mode--kernel-keywords-re
  (concat "^" (regexp-opt elixir-ts-mode--kernel-keywords) "$"))

(defconst elixir-ts-mode--builtin-keywords
  '("__MODULE__" "__DIR__" "__ENV__" "__CALLER__" "__STACKTRACE__"))

(defconst elixir-ts-mode--builtin-keywords-re
  (concat "^" (regexp-opt elixir-ts-mode--builtin-keywords) "$"))

(defconst elixir-ts-mode--doc-keywords
  '("moduledoc" "typedoc" "doc"))

(defconst elixir-ts-mode--doc-keywords-re
  (concat "^" (regexp-opt elixir-ts-mode--doc-keywords) "$"))

(defconst elixir-ts-mode--reserved-keywords
  '("when" "and" "or" "not" "in"
    "not in" "fn" "do" "end" "catch" "rescue" "after" "else"))

(defconst elixir-ts-mode--reserved-keywords
  '("when" "and" "or" "not" "in"
    "not in" "fn" "do" "end" "catch" "rescue" "after" "else"))

(defconst elixir-ts-mode--reserved-keywords-re
  (concat "^" (regexp-opt elixir-ts-mode--reserved-keywords) "$"))

(defconst elixir-ts-mode--reserved-keywords-vector
  (apply #'vector elixir-ts-mode--reserved-keywords))

(defvar elixir-ts-mode--capture-anonymous-function-end
  (treesit-query-compile 'elixir '((anonymous_function "end" @end))))

(defvar elixir-ts-mode--capture-operator-parent
  (treesit-query-compile 'elixir '((binary_operator operator: _ @val))))

(defvar elixir-ts-mode--capture-first-argument
  (treesit-query-compile
   'elixir
   '((arguments :anchor (_) @first-child) (tuple :anchor (_) @first-child))))

(defvar elixir-ts-mode--syntax-table
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
  "Syntax table for `elixir-ts-mode.")

(defvar elixir-ts-mode--indent-rules
  (let ((offset elixir-ts-mode-indent-offset))
    `((elixir
       ((parent-is "source") parent-bol 0)
       ((parent-is "string") parent-bol 0)
       ((parent-is "quoted_content")
        (lambda (_n parent bol &rest _)
          (save-excursion
            (back-to-indentation)
            (if (bolp)
                (progn
                  (goto-char (treesit-node-start parent))
                  (back-to-indentation)
                  (point))
              (point)))) 0)
       ((node-is "|>") parent-bol 0)
       ((node-is "|") parent-bol 0)
       ((node-is "}") parent-bol 0)
       ((node-is ")") parent-bol 0)
       ((node-is "]") parent-bol 0)
       ((node-is "else_block") elixir-ts-mode--treesit-anchor-grand-parent-bol 0)
       ((node-is "catch_block") elixir-ts-mode--treesit-anchor-grand-parent-bol 0)
       ((node-is "rescue_block") elixir-ts-mode--treesit-anchor-grand-parent-bol 0)
       ((node-is "stab_clause") parent-bol ,offset)
       ((query ,elixir-ts-mode--capture-operator-parent) grand-parent 0)
       ((node-is "when") parent 0)
       ((node-is "keywords") parent-bol ,offset)
       ((parent-is "body") parent-bol ,offset)
       ((parent-is "arguments") parent-bol ,offset)
       ((parent-is "binary_operator") parent ,offset)
       ((node-is "pair") first-sibling 0)
       ((parent-is "tuple") (lambda (_n parent &rest _)
                              (treesit-node-start
                               (treesit-node-child parent 0 t))) 0)
       ((parent-is "list") parent-bol ,offset)
       ((parent-is "pair") parent ,offset)
       ((parent-is "map") parent-bol ,offset)
       ((query ,elixir-ts-mode--capture-anonymous-function-end) parent-bol 0)
       ((node-is "end") elixir-ts-mode--treesit-anchor-grand-parent-bol 0)
       ((parent-is "do_block") grand-parent ,offset)
       ((parent-is "anonymous_function")
        elixir-ts-mode--treesit-anchor-grand-parent-bol ,offset)
       ((parent-is "else_block") parent ,offset)
       ((parent-is "rescue_block") parent ,offset)
       ((parent-is "catch_block") parent ,offset)
       (no-node parent-bol ,offset)))))

;; reference:
;; https://github.com/elixir-lang/tree-sitter-elixir/blob/main/queries/highlights.scm
(defvar elixir-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'elixir
   :feature 'elixir-comment
   '((comment) @elixir-font-comment-face)

   :language 'elixir
   :feature 'elixir-string
   :override t
   '([(string) (charlist)] @font-lock-string-face)

   :language 'elixir
   :feature 'elixir-string-interpolation
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
   :feature 'elixir-keyword
   ;; :override `prepend
   `(,elixir-ts-mode--reserved-keywords-vector
     @elixir-font-keyword-face
     ;; these are operators, should we mark them as keywords?
     (binary_operator
      operator: _ @elixir-font-keyword-face
      (:match ,elixir-ts-mode--reserved-keywords-re @elixir-font-keyword-face)))

   :language 'elixir
   :feature 'elixir-doc
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
      (:match ,elixir-ts-mode--doc-keywords-re
              @elixir-font-comment-doc-identifier-face))
     (unary_operator
      operator: "@" @elixir-font-comment-doc-attribute-face
      operand: (call
                target: (identifier) @elixir-font-comment-doc-identifier-face)
      (:match ,elixir-ts-mode--doc-keywords-re @elixir-font-comment-doc-identifier-face)))

   :language 'elixir
   :feature 'elixir-unary-operator
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
   :feature 'elixir-operator
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
   :feature 'elixir-call
   `((call
      target: (identifier) @elixir-font-keyword-face
      (:match ,elixir-ts-mode--definition-keywords-re @elixir-font-keyword-face))
     (call
      target: (identifier) @elixir-font-keyword-face
      (:match ,elixir-ts-mode--kernel-keywords-re @elixir-font-keyword-face))
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
      (:match ,elixir-ts-mode--definition-keywords-re @elixir-font-keyword-face))
     (call
      target: (identifier) @elixir-font-keyword-face
      (arguments
       (binary_operator
        operator: "|>"
        right: (identifier) @elixir-font-variable-face))
      (:match ,elixir-ts-mode--definition-keywords-re @elixir-font-keyword-face)))

   :language 'elixir
   :feature 'elixir-constant
   `((binary_operator operator: "|>" right: (identifier) @elixir-font-function-face)
     ((identifier) @elixir-font-constant-builtin-face
      (:match ,elixir-ts-mode--builtin-keywords-re @elixir-font-constant-builtin-face))
     ((identifier) @elixir-font-comment-unused-face
      (:match "^_" @elixir-font-comment-unused-face))
     (identifier) @elixir-font-variable-face
     ["%"] @elixir-font-punctuation-face
     ["," ";"] @elixir-font-punctuation-delimiter-face
     ["(" ")" "[" "]" "{" "}" "<<" ">>"] @elixir-font-punctuation-bracket-face)

   :language 'elixir
   :feature 'elixir-sigil
   :override t
   `((sigil
      (sigil_name) @elixir-font-sigil-name-face
      quoted_start: _ @elixir-font-string-face
      quoted_end: _ @elixir-font-string-face
      (:match "^[sSwW]$" @elixir-font-sigil-name-face)) @elixir-font-string-face
      (sigil
       (sigil_name) @elixir-font-sigil-name-face
       quoted_start: _ @elixir-font-string-regex-face
       quoted_end: _ @elixir-font-string-regex-face
       (:match "^[rR]$" @elixir-font-sigil-name-face)) @elixir-font-string-regex-face
      (sigil
       "~" @elixir-font-string-special-face
       (sigil_name) @elixir-font-sigil-name-face
       quoted_start: _ @elixir-font-string-special-face
       quoted_end: _ @elixir-font-string-special-face
       (:match "^H$" @elixir-font-sigil-name-face)))

   :language 'elixir
   :feature 'elixir-string-escape
   :override t
   `((escape_sequence) @elixir-font-string-escape-face))
  "Tree-sitter font-lock settings.")

(defun elixir-ts-mode--indent-parent-bol-p (parent)
  (save-excursion
    (goto-char (treesit-node-start parent))
    (back-to-indentation)
    (and (eq (char-after) ?|) (eq (char-after (1+ (point))) ?>))))

(defun elixir-ts-mode--treesit-anchor-grand-parent-bol (_n parent &rest _)
  (save-excursion
    (goto-char (treesit-node-start (treesit-node-parent parent)))
    (back-to-indentation)
    (point)))

(defvar elixir-ts-mode--treesit-range-rules
  (treesit-range-rules
   :embed 'heex
   :host 'elixir
   '((sigil (sigil_name) @name (:match "^[H]$" @name) (quoted_content) @heex))))

(defun elixir-ts-mode--treesit-language-at-point (point)
  (let ((language-in-range
         (cl-loop
          for parser in (treesit-parser-list)
          do (setq range
                   (cl-loop
                    for range in (treesit-parser-included-ranges parser)
                    if (and (>= point (car range)) (<= point (cdr range)))
                    return parser))
          if range
          return (treesit-parser-language parser))))
    (if (null language-in-range)
        (when-let ((parser (car (treesit-parser-list))))
          (treesit-parser-language parser))
      language-in-range)))

(defun elixir-ts-mode--defun-p (node)
  (member (treesit-node-text
           (treesit-node-child-by-field-name node "target"))
          elixir-ts-mode--definition-keywords))

(defun elixir-ts-mode--defun-name (node)
  "Return the name of the defun NODE.
Return nil if NODE is not a defun node or doesn't have a name."
  (pcase (treesit-node-type node)
    ("call" (let ((node-child
                   (treesit-node-child (treesit-node-child node 1) 0)))
              (pcase (treesit-node-type node-child)
                ("alias" (treesit-node-text node-child t))
                ("call" (treesit-node-text
                         (treesit-node-child-by-field-name node-child "target") t))
                ("binary_operator"
                 (treesit-node-text
                  (treesit-node-child-by-field-name
                   (treesit-node-child-by-field-name node-child "left") "target") t))
                ("identifier"
                 (treesit-node-text node-child t))
                (_ nil))))
    (_ nil)))

;;;###autoload
(define-derived-mode elixir-ts-mode prog-mode "Elixir"
  "Major mode for editing Elixir, powered by tree-sitter."
  :group 'elixir
  :syntax-table elixir-ts-mode--syntax-table

  (when (and (treesit-ready-p 'elixir)
             (treesit-ready-p 'heex))

    (treesit-parser-create 'heex)
    (treesit-parser-create 'elixir)

    ;; Comments
    (setq-local comment-start "# ")
    (setq-local comment-start-skip
                (rx "#" (* (syntax whitespace))))

    (setq-local comment-end "")
    (setq-local comment-end-skip
                (rx (* (syntax whitespace))
                    (group (or (syntax comment-end) "\n"))))

    ;; Electric.
    (setq-local electric-indent-chars
                (append "]" ")" "}" "\"" "end" ">" electric-indent-chars))

    ;; heex embedding
    (setq-local treesit-language-at-point-function
                'elixir-ts-mode--treesit-language-at-point)

    (setq-local treesit-range-settings elixir-ts-mode--treesit-range-rules)
    (setq-local treesit-font-lock-settings
                (append elixir-ts-mode--font-lock-settings
                        heex-ts-mode--font-lock-settings))

    (setq-local treesit-simple-indent-rules
                (append elixir-ts-mode--indent-rules heex-ts-mode--indent-rules))

    (setq-local treesit-font-lock-level 6)

    (setq-local treesit-font-lock-feature-list
                '(( elixir-comment elixir-string elixir-call elixir-constant)
                  ( elixir-keyword elixir-unary-operator elixir-operator elixir-doc )
                  ( elixir-sigil elixir-string-escape elixir-string-interpolation)
                  ( heex-doctype heex-comment )
                  ( heex-component heex-tag heex-attribute heex-keyword heex-string )
                  ( heex-bracket )))

    (setq-local treesit-defun-name-function #'elixir-ts-mode--defun-name)

    ;; Imenu
    (setq-local treesit-simple-imenu-settings
                '((nil "\\`call\\'" elixir-ts-mode--defun-p nil)))

    ;; Navigation
    (setq-local treesit-defun-type-regexp
                '("call" . elixir-ts-mode--defun-p))

    (treesit-major-mode-setup)))

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.elixir\\'" . elixir-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-ts-mode))
  (add-to-list 'auto-mode-alist '("mix\\.lock" . elixir-ts-mode)))

(provide 'elixir-ts-mode)

;;; elixir-ts-mode.el ends here
