;; -*- lexical-binding: t -*-
;;
;;; A test suite for Pyper's parser.el.

(require 'parser)
(require 'grammar)
(require 'token)

(ert-deftest test-parser-create-and-setup ()
  "Just make sure nothing falls apart"
  (let* ((grammar (grammar-load "source/grammar-2.7.8.sexp"))
         (parser (parser-create grammar)))
    ;; should not fail, that's enough here
    (parser-setup parser)))

(defun test-parser-test-yield (elem)
  (setq test-parser-token-list (cons elem test-parser-token-list)))
(defvar test-parser-token-list)

(defun test-parser-prepare-tokens (tokens)
  "Prepare token list for parsing.

Some tokens are not really meant for parsing (comments and
newlines), operators should be rewritten into a concrete operator
type. A raw token list should also be reversed. This function
does exactly that."
  (let (result)
    (cl-loop for (type value start end line-text)
             in tokens do
             (cond
              ;; Skip comments and meaningless newlines
              ((or (eq type token-COMMENT)
                   (eq type token-NL))
               t)
              ;; Rewrite a generic operator token to a concrete operator type
              ((eq type token-OP)
               (let ((type (gethash value grammar-opmap)))
                 (setq result (cons (list type value line-text) result))))
              ;; Add all the remaining tokens
              (t (setq result (cons (list type value line-text) result)))))
    result))

(defconst test-parser-assign-code
  "a = 1
b = 1
c = a + b
print")
(ert-deftest test-parser-classify-test ()
  "Just make sure nothing falls apart"
  (let* ((grammar (grammar-load "source/grammar-2.7.8.sexp"))
         (parser (parser-create grammar)))
    (parser-setup parser)
    (unwind-protect
        (with-temp-buffer
          (insert test-parser-assign-code)
          (goto-char (point-min))
          (generate-tokens 'test-parser-test-yield)
          (cl-loop for token in (test-parser-prepare-tokens test-parser-token-list) do
                   (let* ((type (nth 0 token))
                          (value (nth 1 token))
                          (ctx (nth 2 token))
                          (ilabel (parser-classify parser type value ctx)))
                     ;; (message "type, value, ctx = %s, %s, %s" type value ctx)
                     ;; (message "ilabel %s" ilabel)
                     ))
          (let ((used-names (parser-used-names parser)))
            (should (gethash "a" used-names))
            (should (gethash "b" used-names))
            (should (gethash "c" used-names))
            (should (gethash "print" used-names))))
      (setq test-parser-token-list nil))))
