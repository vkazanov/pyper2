;; -*- lexical-binding: t -*-
;;
;;; A test suite for Pyper's tokenizer.el.
;;
;; Incomplete, as the tests were written post-factum. Will grow along with
;; further development

(require 'tokenizer)
(require 'token)

;;; Helper functions

(defvar tokenizer-test-token-list nil)

(defun tokenizer-test-yield (elem)
  (setq tokenizer-test-token-list (cons elem tokenizer-test-token-list)))

;;; Operators

(defconst tokenizer-test-operator-list
  '("-" "+" "/" "*" "//" "**" "%"
    "-=" "+=" "/=" "//=" "%=" "=="))

(ert-deftest test-tokenizer-operators ()
  (dolist (operator-string tokenizer-test-operator-list)
          (unwind-protect
              (with-temp-buffer
                (insert (concat operator-string "\n"))
                (goto-char (point-min))
                (generate-tokens 'tokenizer-test-yield)
                (let* ((token (car (reverse tokenizer-test-token-list)))
                       (token-string (cadr token)))
                  (should (equal 3 (length tokenizer-test-token-list)))
                  (should (equal (car token) token-OP))
                  (should (equal token-string operator-string))))
            (setq tokenizer-test-token-list nil))))

;;; Strings

(defconst tokenizer-single-line-string-list
  '("\"simple\"" "r\"\raw\"" "u\"unicode\"" "ur\"raw/unicode\"" "b\"binary\""
    "'simple'" "r'\raw'" "u'unicode'" "ur'raw/unicode'" "b'binary'"
    "\"\"\"double quote triple\"\"\"" "'''single quote triple'''"))

(defconst tokenizer-multi-line-string-list
  '("\"first\\\n second\\\n third\"" "\"first\\\n second\\\n third\""
    "\"\"\"first\\\n second\\\n third\"\"\"" "'''first\\\n second\\\n third'''"))


(ert-deftest test-tokenizer-strings ()
  (dolist (test-string (append tokenizer-single-line-string-list
                               tokenizer-multi-line-string-list))
          (unwind-protect
              (with-temp-buffer
                (insert (concat test-string "\n"))
                (goto-char (point-min))
                (generate-tokens 'tokenizer-test-yield)
                (let* ((token (car (reverse tokenizer-test-token-list)))
                       (token-string (cadr token)))
                  (should (equal 3 (length tokenizer-test-token-list)))
                  (should (equal (car token) token-STRING))
                  (should (equal token-string test-string))))
            (setq tokenizer-test-token-list nil))))

;;; Names

(defconst tokenizer-name-string-list
  '("name" "Name" "NAME" "_name" "_1" "name1" "_name1" "_complex_name"))

(ert-deftest test-tokenizer-names ()
  (dolist (test-string tokenizer-name-string-list)
          (unwind-protect
              (with-temp-buffer
                (insert (concat test-string "\n"))
                (goto-char (point-min))
                (generate-tokens 'tokenizer-test-yield)
                (let* ((token (car (reverse tokenizer-test-token-list)))
                       (token-string (cadr token)))
                  (should (equal 3 (length tokenizer-test-token-list)))
                  (should (equal (car token) token-NAME))
                  (should (equal token-string test-string))))
            (setq tokenizer-test-token-list nil))))

;;; Numbers

(defconst tokenizer-number-string-list
  '("123" "123." ".123" "0.123" "0x123"))

(ert-deftest test-tokenizer-numbers ()
  (dolist (test-string tokenizer-number-string-list)
          (unwind-protect
              (with-temp-buffer
                (insert (concat test-string "\n"))
                (goto-char (point-min))
                (generate-tokens 'tokenizer-test-yield)
                (let* ((token (car (reverse tokenizer-test-token-list)))
                       (token-string (cadr token)))
                  (should (equal 3 (length tokenizer-test-token-list)))
                  (should (equal (car token) token-NUMBER))
                  (should (equal token-string test-string))))
            (setq tokenizer-test-token-list nil))))

;;; Newlines

(ert-deftest test-tokenizer-newline-and-ops ()
  (let ((test-string "__bla__ = ('one' 'two' 'three')\r\n")
        (test-token-string-pairs `((,token-NAME . "__bla__")
                                   (,token-OP . "=")
                                   (,token-OP . "(")
                                   (,token-STRING . "'one'")
                                   (,token-STRING . "'two'")
                                   (,token-STRING . "'three'")
                                   (,token-OP . ")")
                                   (,token-NEWLINE . "\r\n")
                                   (,token-ENDMARKER . ""))))
    (unwind-protect
        (with-temp-buffer
          (insert test-string)
          (goto-char (point-min))
          (generate-tokens 'tokenizer-test-yield)
          (let (token-build-pair)
            (fset 'token-build-pair (lambda (token) (cons (car token) (cadr token))))
            (let ((token-pair-list (reverse (mapcar 'token-build-pair tokenizer-test-token-list))))
              (should (equal token-pair-list test-token-string-pairs)))))
      (setq tokenizer-test-token-list nil))))

;;; Complex cases

(ert-deftest test-tokenizer-class-simple ()
  (let ((test-string "class\tClass:\r\n\tpass\r\n")
        (test-token-string-pairs `((,token-NAME . "class")
                                   (,token-NAME . "Class")
                                   (,token-OP . ":")
                                   (,token-NEWLINE . "\r\n")
                                   (,token-INDENT . "\t")
                                   (,token-NAME . "pass")
                                   (,token-NEWLINE . "\r\n")
                                   (,token-DEDENT . "")
                                   (,token-ENDMARKER . ""))))
    (unwind-protect
        (with-temp-buffer
          (insert test-string)
          (goto-char (point-min))
          (generate-tokens 'tokenizer-test-yield)
          (let (token-build-pair)
            (fset 'token-build-pair (lambda (token) (cons (car token) (cadr token))))
            (let ((token-pair-list (reverse (mapcar 'token-build-pair tokenizer-test-token-list))))
              (should (equal token-pair-list test-token-string-pairs)))))
      (setq tokenizer-test-token-list nil))))

(ert-deftest test-tokenizer-func ()
  (let ((test-string "def func(arg1, arg2, arg3):\r\n\tpass\r\n")
        (test-token-string-pairs `((,token-NAME . "def")
                                   (,token-NAME . "func")
                                   (,token-OP . "(")
                                   (,token-NAME . "arg1")
                                   (,token-OP . ",")
                                   (,token-NAME . "arg2")
                                   (,token-OP . ",")
                                   (,token-NAME . "arg3")
                                   (,token-OP . ")")
                                   (,token-OP . ":")
                                   (,token-NEWLINE . "\r\n")
                                   (,token-INDENT . "\t")
                                   (,token-NAME . "pass")
                                   (,token-NEWLINE . "\r\n")
                                   (,token-DEDENT . "")
                                   (,token-ENDMARKER . ""))))
    (unwind-protect
        (with-temp-buffer
          (insert test-string)
          (goto-char (point-min))
          (generate-tokens 'tokenizer-test-yield)
          (let (token-build-pair)
            (fset 'token-build-pair (lambda (token) (cons (car token) (cadr token))))
            (let ((token-pair-list (reverse (mapcar 'token-build-pair tokenizer-test-token-list))))
              (should (equal token-pair-list test-token-string-pairs)))))
      (setq tokenizer-test-token-list nil))))

(ert-deftest test-tokenizer-func-comment ()
  (let ((test-string "def func(arg):\r\n\t\"test\"\r\n\t# comment\r\n")
        (test-token-string-pairs `((,token-NAME . "def")
                                   (,token-NAME . "func")
                                   (,token-OP . "(")
                                   (,token-NAME . "arg")
                                   (,token-OP . ")")
                                   (,token-OP . ":")
                                   (,token-NEWLINE . "\r\n")
                                   (,token-INDENT . "\t")
                                   (,token-STRING . "\"test\"")
                                   (,token-NEWLINE . "\r\n")
                                   (,token-COMMENT . "# comment")
                                   (,token-NL . "\r\n")
                                   (,token-DEDENT . "")
                                   (,token-ENDMARKER . ""))))
    (unwind-protect
        (with-temp-buffer
          (insert test-string)
          (goto-char (point-min))
          (generate-tokens 'tokenizer-test-yield)
          (let (token-build-pair)
            (fset 'token-build-pair (lambda (token) (cons (car token) (cadr token))))
            (let ((token-pair-list (reverse (mapcar 'token-build-pair tokenizer-test-token-list))))
              (print token-pair-list)
              (should (equal token-pair-list test-token-string-pairs)))))
      (setq tokenizer-test-token-list nil))))

(ert-deftest test-tokenizer-expr ()
  (let ((test-string "1 + 22 + (13/9\r\n\t\t  * 2)\r\n")
        (test-token-string-pairs `((,token-NUMBER . "1")
                                   (,token-OP . "+")
                                   (,token-NUMBER . "22")
                                   (,token-OP . "+")
                                   (,token-OP . "(")
                                   (,token-NUMBER . "13")
                                   (,token-OP . "/")
                                   (,token-NUMBER . "9")
                                   (,token-NL . "\r\n")
                                   (,token-OP . "*")
                                   (,token-NUMBER . "2")
                                   (,token-OP . ")")
                                   (,token-NEWLINE . "\r\n")
                                   (,token-ENDMARKER . ""))))
    (unwind-protect
        (with-temp-buffer
          (insert test-string)
          (goto-char (point-min))
          (generate-tokens 'tokenizer-test-yield)
          (let (token-build-pair)
            (fset 'token-build-pair (lambda (token) (cons (car token) (cadr token))))
            (let ((token-pair-list (reverse (mapcar 'token-build-pair tokenizer-test-token-list))))
              (should (equal token-pair-list test-token-string-pairs)))))
      (setq tokenizer-test-token-list nil))))

(ert-deftest test-tokenizer-string-multiline ()
  (let ((test-string
         (concat "\"first\\\r\nsecond\\\r\nthird\"\r\n\r\n"
                 "\"\"\"first\r\nsecond\r\nthird\"\"\"\r\n"))
        (test-token-string-pairs `((,token-STRING . "\"first\\\r\nsecond\\\r\nthird\"")
                                   (,token-NEWLINE . "\r\n")
                                   (,token-NL . "\r\n")
                                   (,token-STRING . "\"\"\"first\r\nsecond\r\nthird\"\"\"")
                                   (,token-NEWLINE . "\r\n")
                                   (,token-ENDMARKER . ""))))
    (unwind-protect
        (with-temp-buffer
          (insert test-string)
          (goto-char (point-min))
          (generate-tokens 'tokenizer-test-yield)
          (let (token-build-pair)
            (fset 'token-build-pair (lambda (token) (cons (car token) (cadr token))))
            (let ((token-pair-list (reverse (mapcar 'token-build-pair tokenizer-test-token-list))))
              (should (equal token-pair-list test-token-string-pairs)))))
      (setq tokenizer-test-token-list nil))))
