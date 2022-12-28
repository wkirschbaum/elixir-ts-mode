;;; heex-ts-mode.el --- tree-sitter support for Heex -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Wilhelm H Kirschbaum

;; Author           : Wilhelm H Kirschbaum
;; Package-Requires : ((emacs "29"))
;; Created          : November 2022
;; Keywords         : heex languages tree-sitter

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

(defgroup heex nil
  "Major mode for editing Heex code."
  :group 'languages
  :version "29.1")

(defcustom heex-ts-mode--indent-offset 2
  "Indentation of Heex statements."
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

(defconst heex-ts-mode--brackets-vector
  (apply #'vector heex-ts-mode--brackets))

(defvar heex-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?< "(>" table)
    (modify-syntax-entry ?> ")<" table)
    table)
  "Heex mode syntax table.")

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
       ((parent-is "tag") parent-bol ,offset)
       (no-node parent-bol ,offset)))))

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

(defun heex-ts-mode--comment-region (beg end &optional arg)
  (save-excursion
    (goto-char beg)
    (insert (concat comment-start " "))
    (goto-char end)
    (goto-char (pos-eol))
    (forward-comment (- (point-max)))
    (insert (concat " " comment-end))
    ))

(defun heex-ts-mode--defun-name (node)
  "Return the name of the defun NODE.
Return nil if NODE is not a defun node or doesn't have a name."
  (pcase (treesit-node-type node)
    ((or "component" "slot" "tag")
     (string-trim (treesit-node-text
                   (treesit-node-child (treesit-node-child node 0) 1)
                   nil)))
    (_ nil)))

;;;###autoload
(define-derived-mode heex-ts-mode prog-mode "Heex"
  "Major mode for editing Heex, powered by tree-sitter."
  :group 'heex
  :syntax-table heex-ts-mode--syntax-table

  (when (and (treesit-ready-p 'heex))
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
  (setq-local treesit-defun-name-function #'heex-ts-mode--defun-name)

  ;; Imenu
  (setq-local treesit-simple-imenu-settings
              '(("Component" "\\`component\\'" nil nil)
                ("Slot" "\\`slot\\'" nil nil)
                ("Tag" "\\`tag\\'" nil nil)))

  (setq-local treesit-font-lock-settings heex-ts-mode--font-lock-settings)

  (setq-local treesit-simple-indent-rules heex-ts-mode--indent-rules)

  (setq-local treesit-font-lock-level 6)
  (setq-local treesit-font-lock-feature-list
              '(( heex-doctype heex-comment )
                ( heex-component heex-tag heex-attribute heex-keyword heex-string )
                ( heex-bracket )))

  (treesit-major-mode-setup))


;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.[hl]?eex\\'" . heex-ts-mode)))

(provide 'heex-ts-mode)
;;; heex-ts-mode.el ends here
