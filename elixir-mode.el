;;; elixir-mode.el --- Elixir support for Emacs -*- lexical-binding: t -*-

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

(defvar elixir-attribute-face 'elixir-attribute-face)
(defface elixir-attribute-face
  '((t (:inherit font-lock-preprocessor-face)))
  "For use with module attribute tokens.")

(defvar elixir-atom-face 'elixir-atom-face)
(defface elixir-atom-face
  '((t (:inherit font-lock-builtin-face)))
  "For use with atoms & map keys.")

(defvar elixir-number-face 'elixir-number-face)
(defface elixir-number-face
  '((t (:inherit default)))
  "For use with numbers.")

(defun elixir--treesit-beginning-of-defun (&optional arg)
  (let ((arg (or arg 1)))
    (if (> arg 0)
        ;; Go backward.
        (while (and (> arg 0)
                    (progn
                      (treesit-search-forward-goto "do_block" 'start nil t)
                      (back-to-indentation)))
          (setq arg (1- arg)))
      ;; Go forward.
      (while (and (< arg 0)
                  (treesit-search-forward-goto "do_block" 'end))
        (setq arg (1+ arg))))))

(defun elixir--treesit-end-of-defun (&optional arg)
  (let ((arg (or arg 1)))
    (if (< arg 0)
        ;; Go backward.
        (while (and (< arg 0)
                    (treesit-search-forward-goto "do_block" 'end nil t))
          (setq arg (1+ arg)))
      ;; Go forward.
      (while (and (> arg 0)
                  (progn
                      (treesit-search-forward-goto "do_block" 'start)
                      (back-to-indentation)))
        (setq arg (1- arg))))))


(defvar elixir--treesit-font-lock-settings
  (treesit-font-lock-rules
   :language 'elixir
   `((call target: (identifier) @font-lock-keyword-face)
     (unary_operator) @elixir-attribute-face
     (call (arguments) @font-lock-type-face)
     (keyword) @elixir-atom-face
     (do_block ("do") @font-lock-keyword-face)
     (else_block ("else") @font-lock-keyword-face)
     (do_block ("end") @font-lock-keyword-face)
     (identifier) @font-lock-variable-name-face
     (comment) @font-lock-comment-face
     (string) @elixir--treesit-fontify-string
     (interpolation) @elixir-attribute-face
     ))
  "Tree-sitter font-lock settings.")

;; (defun elixir--treesit-backward-up-list ()
;;   (lambda (_node _parent _bol &rest _)
;;     (save-excursion
;;       (backward-up-list 1 nil t)
;;       (goto-char
;;        (treesit-node-start
;;         (treesit-node-at (point))))
;;       (back-to-indentation)
;;       (point))))


(defvar elixir--treesit-indent-rules
  (let ((offset elixir-indent-level))
    `((elixir
       ;; (no-node (elixir--treesit-backward-up-list) ,offset)
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
        (name-list ())
        (type 'def))
    ;; We have to find the do_block first and then
    ;; go up one level to find either an alias or call.
    ;; When finding a call, we need to get the first identifier.
    ;; This should work for all macros as well
    (cl-loop while node
             if (pcase (treesit-node-type node)
                  ("do_block" t)
                  (_ nil))
             do (push (elixir--treesit-node-block-name node) name-list)
             do (setq node (treesit-node-parent node))
             finally return (concat (if include-type
                                        (format "%s " type)
                                      "")
                                    (string-join name-list ".")))))


(defun elixir--imenu-item-parent-label (_type name)
  (format "%s" name))

(defun elixir--imenu-item-label (type name)
  (format "%s %s" type name))

(defun elixir--imenu-jump-label (_type _name)
  "...")

(defun elixir--treesit-node-block-name (node)
  (let* ((child (treesit-node-child (treesit-node-child (treesit-node-parent node) 1) 0)))
    (if (> (treesit-node-child-count child) 0)
        (treesit-node-text (treesit-node-child child 0))
      (treesit-node-text child))
    ))

(defun elixir--imenu-treesit-create-index (&optional node)
  "Return tree Imenu alist for the current Elixir buffer."
  (let* ((node (or node (treesit-buffer-root-node 'elixir)))
         (tree (treesit-induce-sparse-tree
                node
                (rx (seq bol (or "do_block") eol)))))
    (elixir--imenu-treesit-create-index-from-tree tree)))

(defun elixir--imenu-treesit-create-index-from-tree (node)
  (let* ((ts-node (car node))
         (children (cdr node))
         (subtrees (mapcan #'elixir--imenu-treesit-create-index-from-tree children))
         (type 'def)
         (name (when ts-node (elixir--treesit-node-block-name ts-node)))
         (marker (when ts-node
                   (set-marker (make-marker)
                               (treesit-node-start ts-node)))))
    (cond ((null ts-node) subtrees)
          (subtrees (let ((parent-label (funcall 'elixir--imenu-item-parent-label type name))
                          (jump-label (funcall 'elixir--imenu-jump-label type name)))
                      `((,parent-label ,(cons jump-label marker) ,@subtrees))))
          (t (let ((label (funcall 'elixir--imenu-item-label type name)))
               (list (cons label marker)))))))

;;;###autoload
(define-derived-mode elixir-mode prog-mode "Elixir"
  (treesit-parser-create 'elixir)


  ;; This turns off the syntax-based font-lock for comments and
  ;; strings.  So it doesn’t override tree-sitter’s fontification.
  (setq-local font-lock-keywords-only t)
  (setq-local treesit-font-lock-settings
              elixir--treesit-font-lock-settings)
  (treesit-font-lock-enable)


  (setq-local treesit-simple-indent-rules elixir--treesit-indent-rules)
  (setq-local indent-line-function #'treesit-indent)

  (setq-local beginning-of-defun-function #'elixir--treesit-beginning-of-defun)
  (setq-local end-of-defun-function #'elixir--treesit-end-of-defun)

  (setq-local imenu-create-index-function #'elixir--imenu-treesit-create-index)

  (add-hook 'which-func-functions #'elixir--treesit-current-defun nil t)

  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+ *"))


;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.elixir\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("mix\\.lock" . elixir-mode)))

(provide 'elixir-mode)
;;; elixir-custom-mode.el ends here


