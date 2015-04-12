;; -*- lexical-binding: t -*-
;;
;;; Grammar struct meant to be filled by the original pgen2 metaparser

(require 'ht)
(require 'token)
(eval-when-compile (require 'names))

(define-namespace grammar-

(defstruct (grammar (:constructor grammar--create))
  symbol2number
  number2symbol
  states
  dfas
  labels
  keywords
  tokens
  symbol2label
  start)

(defun create ()
  (grammar--create :symbol2number (make-hash-table :test 'equal)
                   :number2symbol (make-hash-table :test 'equal)
                   :dfas (make-hash-table :test 'equal)
                   :keywords (make-hash-table :test 'equal)
                   :tokens (make-hash-table :test 'equal)
                   :symbol2label  (make-hash-table :test 'equal)
                   :start 256))

(defun load (fname)
  (assert (file-exists-p fname))
  (let* (raw-grammar
         (grammar (grammar-create)))
    (with-temp-buffer
      (insert-file-contents fname)
      (setq raw-grammar (read (current-buffer))))
    (loop for (s val) in raw-grammar do
          ;; TODO: a cleanup would be nice
          (cond
           ((eq s 'symbol2number)
            (setf (grammar-symbol2number grammar) (ht<-plist val)))
           ((eq s 'number2symbol)
            (setf (grammar-number2symbol grammar) (ht<-plist val)))
           ((eq s 'states)
            (setf (grammar-states grammar) val))
           ((eq s 'dfas)
            ;; Convert into a suitable form (nested dicts)
            (let ((tmpdict (ht<-plist val)))
              (ht-each (lambda (key value)
                         (let* ((l (car value))
                                (d (ht<-plist (cadr value))))
                           (ht-set tmpdict key (list l d))))
                       tmpdict)
              (setf (grammar-dfas grammar) tmpdict)))
           ((eq s 'labels)
            (setf (grammar-labels grammar) val))
           ((eq s 'tokens)
            (setf (grammar-tokens grammar) (ht<-plist val)))
           ((eq s 'symbol2label)
            (setf (grammar-symbol2label grammar) (ht<-plist val)))
           ((eq s 'keywords)
            (setf (grammar-keywords grammar) (ht<-plist val)))
           ((eq s 'start)
            (setf (grammar-start grammar) val))))
    grammar))

;; TODO: make it a bit more representable
(defun report (g)
  (message "s2n")
  (maphash (lambda (key value) (message " '%s': %s" key value))
           (grammar-symbol2number g))
  (message "n2s")
  (maphash (lambda (key value) (message " %s: '%s'" key value))
           (grammar-number2symbol g))
  (message "states")
  (message " %s" (grammar-states g))
  (message "dfas")
  (maphash (lambda (key value)
             (message " %s: %s ->" key (car value))
             (maphash (lambda (key value) (message "   %s: %s" key value)) (cdr value)))
           (grammar-dfas g))
  (message "labels")
  (message " %s" (grammar-labels g))
  (message "start")
  (message " %s" (grammar-start g))
  (message "tokens")
  (message " %s" (grammar-tokens g))
  (message "keywords")
  (message " %s" (grammar-keywords g)))

(defconst opmap (make-hash-table :test 'equal))
(defconst opmap-raw "
\( LPAR
\) RPAR
\[ LSQB
\] RSQB
: COLON
, COMMA
; SEMI
+ PLUS
- MINUS
* STAR
/ SLASH
| VBAR
& AMPER
< LESS
> GREATER
= EQUAL
. DOT
% PERCENT
` BACKQUOTE
{ LBRACE
} RBRACE
@ AT
@= ATEQUAL
== EQEQUAL
!= NOTEQUAL
<> NOTEQUAL
<= LESSEQUAL
>= GREATEREQUAL
~ TILDE
^ CIRCUMFLEX
<< LEFTSHIFT
>> RIGHTSHIFT
** DOUBLESTAR
+= PLUSEQUAL
-= MINEQUAL
*= STAREQUAL
/= SLASHEQUAL
%= PERCENTEQUAL
&= AMPEREQUAL
|= VBAREQUAL
^= CIRCUMFLEXEQUAL
<<= LEFTSHIFTEQUAL
>>= RIGHTSHIFTEQUAL
**= DOUBLESTAREQUAL
// DOUBLESLASH
//= DOUBLESLASHEQUAL
-> RARROW
")
(defun opmap-initialize (raw)
  (cl-loop for pair-str
           in (split-string raw "[\\\n]" t) do
           (let* ((pair (split-string pair-str))
                  (op (car pair))
                  (token-name (concat "token-" (cadr pair)))
                  ;; should be defined in token.el already
                  (token-value (symbol-value (intern token-name))))
             (puthash op token-value opmap))))
(eval-when-compile (opmap-initialize opmap-raw))

) ; end of namespace grammar-


(provide 'grammar)
