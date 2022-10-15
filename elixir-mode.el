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


(defconst elixir--definition-keywords
  '("def" "defdelegate" "defexception" "defguard" "defguardp" "defimpl" "defmacro" "defmacrop" "defmodule" "defn" "defnp" "defoverridable" "defp" "defprotocol" "defstruct"))

(defconst elixir--definition-keywords-re
  (concat "^" (regexp-opt elixir--definition-keywords) "$"))

(defconst elixir--kernel-keywords
  '("alias" "case" "cond" "else" "for" "if" "import" "quote" "raise" "receive" "require" "reraise" "super" "throw" "try" "unless" "unquote" "unquote_splicing" "use" "with"))

(defconst elixir--kernel-keywords-re
  (concat "^" (regexp-opt elixir--kernel-keywords) "$"))

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
   :level 1
   `(
     ,elixir--reserved-keywords-vector @font-lock-keyword-face
     (call target: (identifier) @font-lock-keyword-face
           (:match ,elixir--definition-keywords-re @font-lock-keyword-face))
     (call
      target: (identifier) @font-lock-keyword-face
      (:match ,elixir--kernel-keywords-re @font-lock-keyword-face))
     (alias) @elixir-atom-face
     [(boolean) (nil)] @elixir-atom-face
     [(integer) (float)] @elixir-atom-face
     (call target: (dot left: (atom) @elixir-atom-face))
     (char) @elixir-atom-face
     (unary_operator
      operator: "@" @font-lock-variable-name-face
      operand: [
                (identifier) @font-lock-variable-name-face
                (call
                 target: (identifier) @font-lock-variable-name-face)
                (boolean) @font-lock-variable-name-face
                (nil) @font-lock-variable-name-face
                ])

     (interpolation
      "#{"
      @font-lock-variable-name-face
      "}" @font-lock-variable-name-face)
     ))
  "Tree-sitter font-lock settings.")

   ;; :language 'heex
   ;; :level 2
   ;; `(
   ;;   (doctype) @font-lock-keyword-face
   ;;   (ERROR) @error
   ;;   )

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


