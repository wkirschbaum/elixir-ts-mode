(ignore-errors
  (unload-feature 'elixir-mode))

(require 'treesit)
(treesit-parser-create 'elixir)


(defgroup elixir nil
  "Major mode for editing Elixir code."
  :prefix "elixir-"
  :group 'languages)

(defcustom elixir-indent-level 2
  "Indentation of Elixir statements."
  :type 'integer
  :safe 'integerp)

(defvar elixir-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Operators
    ;; (modify-syntax-entry ?< "." table)
    ;; (modify-syntax-entry ?> "." table)
    ;; (modify-syntax-entry ?& "." table)
    ;; (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?= "." table)
    ;; (modify-syntax-entry ?/ "." table)
    ;; (modify-syntax-entry ?+ "." table)
    ;; (modify-syntax-entry ?* "." table)
    ;; (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?: "'" table)

    ;; Comments
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)

    ;; Strings
    ;; (modify-syntax-entry ?\' "\"'" table)
    ;; (modify-syntax-entry ?\" "\"\"" table)

    ;; Symbol constituents
    ;; (modify-syntax-entry ?! "_" table)
    ;; (modify-syntax-entry ?? "_" table)
    ;; (modify-syntax-entry ?_ "_" table)
    ;; (modify-syntax-entry ?@ "_" table)

    ;; expressions
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)

    ;; escape character
    ;; (modify-syntax-entry ?# "\\" table)

    (modify-syntax-entry ?% "'" table)

    table)
  "Syntax table to use in Elixir mode.")

(defun whk/elixir-forward-token ()
  (interactive)
  (message (elixir-smie--forward-token)))

(defun whk/elixir-backward-token ()
  (interactive)
  (message (elixir-smie--backward-token)))

(defvar elixir-smie-grammar
  (smie-prec2->grammar
   (smie-merge-prec2s
    (smie-bnf->prec2
     '((id)
       (lines (line) (lines ";" lines))
       (line (exp)
             (line "=" line)
             ("quote" def-body "end")
             ("defmodule" def-body "end")
             ("defmacro" def-body "end")
             ("defimpl" id "," def-body "end")
             ("defprotocol" def-body "end")
             ("def" def-body "end")
             ("def-short" def-body-short)
             ("for-short" def-body-short)
             ("for" def-body "end")
             ("if-short" def-body-short)
             ("if" def-body "end")
             ("if" def-body "else" lines "end")
             ("case" def-body "end"))
       (def-body (exp "do" lines))
       (def-body-short (exp "," "do:" exp))
       (def-body-short (exp "," "do:" exp "," "else:" exp))
       ;; (case-matches (case-match) (case-matches "fun-split" case-matches))
       (fun-match (lines))
       (exp (id))
       )

     '((assoc ";") (assoc "=") (assoc "fun-split")))
    (smie-precs->prec2
     '()))))

(defun elixir-smie--in-list-p ()
  (nth 1 (syntax-ppss (point))))

(defun elixir-smie--print-token (kind token)
  (message "(%s) %s %s" smie--parent kind token))

(defun elixir-smie-rules (kind token)
  "Elixir indentation rules for KIND and TOKEN."
  (progn
    ;; (elixir-smie--print-token kind token)
    (pcase (cons kind token)
      ('(:elem . basic) 0)
      ('(:after . ",") (if (elixir-smie--in-list-p) nil (smie-rule-parent 2)))
      ('(:before . "=") 2)
      (`(:before . ,(or ";" "do"))
       (if (smie-rule-parent-p
            "[" "def" "defmodule" "defmacro" "quote" "defimpl" "defprotocol" "case")
           elixir-indent-level 0))
      ('(:after . ";") 0))))

;; TODO: backward token checks can be optimised, but keeping it simple for now
(defun elixir-smie--implicit-semi-p ()
  (save-excursion
    (skip-chars-backward " \t")
    (and (eolp)
         (not (memq (char-before)
                    '(?\; ?- ?+ ?* ?/ ?. ?, ?\\ ?& ?> ?< ?% ?~ ?^ ?= ??)))
         (not (member (smie-default-backward-token) '("do")))
         )))

(defun elixir-smie--search-token-on-line-p (&rest tokens)
  "Return t if TOKENS is found on the same line by skipping sexps."
  (save-excursion
    (let ((end-pos (line-end-position)) (found nil))
      (while (and
              (< (point) end-pos)
              (not found)
              (not (elixir-smie--in-list-p)))
        (let ((tok (smie-default-forward-token)))
          (cond
           ((member tok tokens) (setq found t))
           ((equal tok "") (smie-forward-sexp))
           (t nil))))
      found)))

(defun elixir-smie--fun-match-p ()
  "Return t if the current line is the start of a match block."
  (save-excursion
    (forward-comment (point-max))
    (if (smie-rule-bolp)
        (progn (beginning-of-line)
               ;; ensure that this is not the initial match by looking one token
               ;; back and all the tokens on the line for the same level
               (and (not (or
                          (member (save-excursion
                                    (smie-default-forward-token))
                                  '("fn" "do"))
                          (elixir-smie--search-token-on-line-p "fn" "do")))
                    (progn (end-of-line)
                           (equal (smie-default-backward-token) "->")))))))

(defun elixir-smie--next-end-p ()
  (save-excursion
    (letrec ((next-token (smie-default-forward-token))
             (result (equal next-token "end")))
      result)))

(defun elixir-smie--forward-token ()
  "Elixir forward token."
  (skip-chars-forward " \t")
  (cond
   ((and (eolp)
         (not (bolp))
         (elixir-smie--implicit-semi-p))
    (progn (forward-char 1)
           (forward-comment (point-max))
           (cond ((elixir-smie--fun-match-p) "fun-split")
                 (t ";"))))
   (t (let ((token (elixir-smie--to-alias (smie-default-forward-token))))
        (cond
         (t (elixir-smie--maybe-short-form token)))))))


(defun elixir-smie--backward-token ()
  "Elixir backward  token."
  (let ((pos (point)))
    (forward-comment (- (point)))
    (cond
     ((and (> pos (line-end-position))
           (elixir-smie--implicit-semi-p))
      (skip-chars-forward " \t")
      (cond ((elixir-smie--fun-match-p) "fun-split")
            (t ";")))
     (t (let ((token (elixir-smie--to-alias (smie-default-backward-token))))
          (cond
           (t (elixir-smie--maybe-short-form token))))))))

(defun elixir-smie--maybe-short-form (token)
  "Elixir return TOKEN short form if applicable."
  (if (and (member token '("def" "for" "if"))
           (save-excursion
             (end-of-line)
             (not (equal (smie-default-backward-token) "do"))))
      (concat token "-short")
    token))

(defun elixir-smie--to-alias (token)
  "Return a common name for TOKEN to simplify the grammar."
  (pcase token
    ("defp" "def")
    (_ token)))

;;;###autoload
(define-derived-mode elixir-mode prog-mode "Elixir"
  :syntax-table elixir-mode-syntax-table

  (smie-setup elixir-smie-grammar #'elixir-smie-rules
              :forward-token  #'elixir-smie--forward-token
              :backward-token #'elixir-smie--backward-token)

  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+ *")
  (setq-local parse-sexp-ignore-comments t)
  ;; (setq-local parse-sexp-lookup-properties t)
  ;; (setq-local paragraph-start (concat "$\\|" page-delimiter))
  ;; (setq-local paragraph-separate paragraph-start)
  ;; (setq-local paragraph-ignore-fill-prefix t)
  )


;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.elixir\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("mix\\.lock" . elixir-mode)))

(provide 'elixir-mode)
;;; elixir-custom-mode.el ends here


