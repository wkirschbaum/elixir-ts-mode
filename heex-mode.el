;;; heex-mode.el --- major mode for editing heex templates -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Wilhelm H Kirschbaum

;;; Commentary:

;; Code:

(require 'treesit)

(eval-when-compile
  (require 'cl-lib))

(ignore-errors
  (unload-feature 'heex-mode))

(defgroup heex nil
  "Major mode for editing Heex code."
  :tag "Heex"
  :prefix "heex-"
  :group 'languages)

(defcustom heex-indent-level 2
  "Indentation of Heex statements."
  :type 'integer
  :safe 'integerp)

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

(defconst heex--brackets
  '("%>" "--%>" "-->" "/>" "<!" "<!--" "<" "<%!--" "<%" "<%#"
    "<%%=" "<%=" "</" "</:" "<:" ">" "{" "}"))

(defconst heex--brackets-re
  (concat "^" (regexp-opt heex--brackets) "$"))

(defconst heex--brackets-vector
  (apply #'vector heex--brackets))

;; There seems to be no parent directive block
;; so we ignore it for until we learn how heex treesit
;; represents directive blocks
;; https://github.com/phoenixframework/tree-sitter-heex/issues/28
(defvar heex--treesit-indent-rules
  (let ((offset heex-indent-level))
    `((heex
       ((node-is "end_tag") parent-bol 0)
       ((node-is "end_component") parent-bol 0)
       ((node-is "end_slot") parent-bol 0)
       ((parent-is "component") parent-bol ,offset)
       ((parent-is "slot") parent-bol ,offset)
       ((parent-is "tag") parent-bol ,offset)
       ))))

(defvar heex--treesit-font-lock-settings
  (treesit-font-lock-rules
   :language 'heex
   :feature 'doctype
   '((doctype) @heex-font-constant-face)

   :language 'heex
   :feature 'comment
   '((comment) @heex-font-comment-face)

   :language 'heex
   :feature 'bracket
   `(,heex--brackets-vector @heex-font-bracket-face)

   :language 'heex
   :feature 'tag
   `([(tag_name) (slot_name)] @heex-font-tag-face)

   :language 'heex
   :feature 'attribute
   `((attribute_name) @heex-font-attribute-face)

   :language 'heex
   :feature 'keyword
   `((special_attribute_name) @heex-font-keyword-face)

   :language 'heex
   :feature 'string
   `([(attribute_value) (quoted_attribute_value)] @heex-font-string-face)

   :language 'heex
   :feature 'component
   `([
      (component_name) @heex-font-tag-face
      (module) @heex-font-module-face
      (function) @heex-font-function-face
      "." @heex-font-delimeter-face
      ]))
  "Tree-sitter font-lock settings.")

(defun heex--treesit-largest-node-at-point (&optional node)
  "Find the largest node at point or from specified NODE."
  (save-excursion
    (forward-comment (point-max))
    (treesit-parent-while
     (or node (treesit-node-at (point)))
     (lambda (n)
       (and (eq (treesit-node-start n) (point))
            (not (equal (treesit-node-type n) "fragment")))))))

(defun heex--treesit-backward-sexp ()
  "Forward sexp for Heex using treesit."
  (let ((node
         (save-excursion
           (forward-comment (- (point-max)))
           (let ((bol (pos-bol))
                 (end-node (treesit-search-forward-goto
                            (treesit-node-at (point))
                            (rx (or "end_tag" "end_component" "end_slot")) t t)))
             (if (and end-node (> (treesit-node-end end-node) bol))
                 (treesit-node-start (treesit-node-parent end-node)))))))
    (when node (goto-char node))))

(defun heex--treesit-forward-sexp ()
  "Forward sexp for Heex using treesit."
  (let* ((node (heex--treesit-largest-node-at-point))
         (sibling (treesit-node-next-sibling node)))
    (if sibling
        (progn
          (goto-char (treesit-node-start sibling))
          (forward-comment (- (point-max))))
      (when node
        (pcase (treesit-node-type node)
          ((or "end_tag" "end_component" "end_slot") nil)
          (_ (goto-char (treesit-node-end node))))))))

(defun heex--forward-sexp (&optional arg)
  "Heex forward sexp with ARG."
  (let ((arg (or arg 1))
        (node (treesit-node-at (point))))
    (if (> arg 0)
        ;; Go forward.
        (while (and
                (> arg 0)
                (heex--treesit-forward-sexp))
          (setq arg (1- arg)))
      ;; Go backward.
      (while (and (< arg 0)
                  (heex--treesit-backward-sexp))
        (setq arg (1+ arg))))))

(defun heex--comment-region (beg end &optional arg)
  (save-excursion
    (goto-char beg)
    (insert (concat comment-start " "))
    (goto-char end)
    (goto-char (pos-eol))
    (forward-comment (- (point-max)))
    (insert (concat " " comment-end))
    ))


;;;###autoload
(define-derived-mode heex-mode prog-mode "Heex"
  :group 'heex
  "Major mode for editing Heex code.

\\{elixir-mode-map}"

  ;; Comments
  (setq-local comment-end "-->")
  ;; (setq-local comment-start-skip "<!--\\s-*")
  (setq-local comment-region-function 'heex--comment-region)
  (setq-local comment-start "<!--")

  ;; Treesit-mode.
  (setq-local treesit-mode-supported t)
  (setq-local treesit-required-languages '(heex))
  (setq-local treesit-simple-indent-rules heex--treesit-indent-rules)
  (setq-local treesit-font-lock-settings heex--treesit-font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '(( doctype comment )
                ( bracket tag attribute keyword string )
                ( component )))

  (setq-local treesit-defun-type-regexp
              (rx bol (or "start_component" "start_tag") eol))

  (setq-local forward-sexp-function 'heex--forward-sexp)

  (cond
   ((treesit-ready-p nil 'heex)
    (treesit-major-mode-setup))

   (t
    (message "Tree-sitter for Heex isn't available")))
  )

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.[hl]?eex\\'" . heex-mode)))

(provide 'heex-mode)

;;; heex-mode.el ends here
