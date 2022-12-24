;;; heex-ts-mode.el --- tree-sitter support for Heex templates -*- coding: utf-8; lexical-binding: t; -*-

;; Author      : Wilhelm H Kirschbaum
;; Maintainer  : Wilhelm H Kirschbaum
;; Package-Requires: ((emacs "29"))
;; Created     : November 2022
;; Keywords    : heex elixir languages tree-sitter

;;; Commentary:

;; Known issues:
;;
;; Directive blocks are not represented in treesit-sitter-heex which makes it
;; tricky to implement indentation, so it does not work properly at the moment.
;; https://github.com/phoenixframework/tree-sitter-heex/issues/28

;; Code:

(require 'treesit)
(eval-when-compile (require 'rx))

(defcustom heex-ts-mode--indent-offset 2
  "Indentation of Elixir statements."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'heex)

(defface heex-font-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "For use with @keyword tag.")

(defface heex-font-bracket-face
  '((t (:inherit default)))
  "For use with @keyword tag.")

(defface heex-font-constant-face
  '((t (:inherit font-lock-doc-face)))
  "For use with @keyword tag.")

(defface heex-font-comment-face
  '((t (:inherit font-lock-comment-face)))
  "For use with @keyword tag.")

(defface heex-font-tag-face
  '((t (:inherit font-lock-function-name-face)))
  "For use with @tag tag.")

(defface heex-font-attribute-face
  '((t (:inherit font-lock-variable-name-face)))
  "For use with @keyword tag.")

(defface heex-font-string-face
  '((t (:inherit font-lock-constant-face)))
  "For use with @keyword tag.")

(defface heex-font-module-face
  '((t (:inherit font-lock-keyword-face)))
  "For use with @keyword tag.")

(defface heex-font-function-face
  '((t (:inherit font-lock-keyword-face)))
  "For use with @keyword tag.")

(defface heex-font-delimeter-face
  '((t (:inherit font-lock-keyword-face)))
  "For use with @keyword tag.")

(defconst heex-ts-mode--brackets
  '("%>" "--%>" "-->" "/>" "<!" "<!--" "<" "<%!--" "<%" "<%#"
    "<%%=" "<%=" "</" "</:" "<:" ">" "{" "}"))

(defconst heex-ts-mode--brackets-re
  (concat "^" (regexp-opt heex-ts-mode--brackets) "$"))

(defconst heex-ts-mode--brackets-vector
  (apply #'vector heex-ts-mode--brackets))

;; There seems to be no parent directive block
;; so we ignore it for until we learn how heex treesit
;; represents directive blocks
;; https://github.com/phoenixframework/tree-sitter-heex/issues/28
(defvar heex-ts-mode--indent-rules
  (let ((offset heex-ts-mode--indent-offset))
    `((heex
       ((parent-is "fragment") parent-bol 0)
       ((node-is "end_tag") parent-bol 0)
       ((node-is "end_component") parent-bol 0)
       ((node-is "end_slot") parent-bol 0)
       ((node-is ">") parent-bol 0)
       ((parent-is "component") parent-bol ,offset)
       ((parent-is "slot") parent-bol ,offset)
       ((parent-is "tag") parent-bol ,offset)))))

(defvar heex-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'heex
   :feature 'heex-doctype
   '((doctype) @heex-font-constant-face)

   :language 'heex
   :feature 'heex-comment
   '((comment) @heex-font-comment-face)

   :language 'heex
   :feature 'heex-bracket
   `(,heex-ts-mode--brackets-vector @heex-font-bracket-face)

   :language 'heex
   :feature 'heex-tag
   `([(tag_name) (slot_name)] @heex-font-tag-face)

   :language 'heex
   :feature 'heex-attribute
   `((attribute_name) @heex-font-attribute-face)

   :language 'heex
   :feature 'heex-keyword
   `((special_attribute_name) @heex-font-keyword-face)

   :language 'heex
   :feature 'heex-string
   `([(attribute_value) (quoted_attribute_value)] @heex-font-string-face)

   :language 'heex
   :feature 'heex-component
   `([
      (component_name) @heex-font-tag-face
      (module) @heex-font-module-face
      (function) @heex-font-function-face
      "." @heex-font-delimeter-face
      ])
   )
  "Tree-sitter font-lock settings.")

;; (defvar heex-ts-mode--range-rules
;;   (treesit-range-rules
;;    :embed 'elixir
;;    :host 'heex
;;    '((directive (partial_expression_value) @cap)
;;      (directive (expression_value) @cap)
;;      (expression (expression_value) @cap))))

;; Comment region is still very naive and might be easy pickings to
;; improve
(defun heex-ts-mode--comment-region (beg end &optional arg)
  (save-excursion
    (goto-char beg)
    (insert (concat comment-start " "))
    (goto-char end)
    (goto-char (pos-eol))
    (forward-comment (- (point-max)))
    (insert (concat " " comment-end))
    ))

(defvar heex-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?< "(>" table)
    (modify-syntax-entry ?> ")<" table)
    table)
  "Heex mode syntax table.")

;;;###autoload
(define-derived-mode heex-ts-mode prog-mode "Heex"
  "Major mode for editing Elixir, powered by tree-sitter."
  :group 'heex
  :syntax-table heex-ts-mode--syntax-table

  (when (treesit-ready-p 'heex)
    (treesit-parser-create 'heex))

  ;; Comments.
  (setq-local comment-start "<!-- ")
  (setq-local comment-start-skip (rx (or "<!--")
                                     (* (syntax whitespace))))
  (setq-local comment-end "-->")
  (setq-local comment-end-skip (rx (* (syntax whitespace))
                                   (group (or "-->"))))

  (setq-local comment-region-function 'heex-ts-mode--comment-region)

  ;; Electric.
  (setq-local electric-indent-chars
              (append ">" electric-indent-chars))

  ;; Navigation.
  (setq-local treesit-defun-type-regexp
              (rx bol (or "component" "tag" "slot") eol))

  ;; Treesit.
  (setq-local treesit-mode-supported t)

  (setq-local treesit-required-languages '(heex))
  (setq-local treesit-simple-indent-rules heex-ts-mode--indent-rules)
  (setq-local treesit-font-lock-settings heex-ts-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '(( heex-doctype heex-comment )
                ( heex-bracket heex-tag heex-attribute heex-keyword heex-string )
                ( heex-component )))

  (treesit-major-mode-setup))

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.[hl]?eex\\'" . heex-ts-mode)))

(provide 'heex-ts-mode)

;;; heex-ts-mode.el ends here
