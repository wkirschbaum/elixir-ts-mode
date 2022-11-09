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


(defvar heex--treesit-indent-rules
  (let ((offset heex-indent-level))
    `((heex
       ((parent-is "component") parent-bol ,offset)
       ((parent-is "slot") parent-bol ,offset)
       ((node-is "end_tag") parent-bol 0)
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

;;;###autoload
(define-derived-mode heex-mode prog-mode "Heex"
  :group 'heex
  "Major mode for editing Heex code.

\\{elixir-mode-map}"

  ;; Comments
  (setq-local comment-start "<!-- ")
  (setq-local comment-start-skip "<!--\\s-*")
  (setq-local comment-end " -->")

  ;; Treesit-mode.
  (setq-local treesit-mode-supported t)
  (setq-local treesit-required-languages '(heex))
  (setq-local treesit-simple-indent-rules heex--treesit-indent-rules)
  (setq-local treesit-font-lock-settings heex--treesit-font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '(( doctype comment )
                ( bracket tag attribute keyword string )
                ( component )))

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
;;; elixir-mode.el ends here
