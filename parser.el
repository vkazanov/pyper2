;; -*- lexical-binding: t -*-
;;
;;; Main parsing mechanism. Uses the grammar struct.

(require 'grammar)
(require 'token)
(eval-when-compile (require 'names))

(define-namespace parser-

(defstruct (parser (:constructor parser--create))
  grammar
  convert-function
  stack
  rootnode
  used-names)

(defun create (grammar &optional convert)
  (unless convert
    (setq convert (lambda (grammar node) node)))
  (parser--create :grammar grammar
                  :convert-function convert))

(defun setup (parser &optional start)
  (unless start
    (setq start (grammar-start (parser-grammar parser))))
  (let* ((grammar (parser-grammar parser))
         (newnode (list start nil nil nil))
         (stackentry (list (gethash start (grammar-dfas grammar))
                           0
                           newnode)))
    (setf (parser-stack parser) (list stackentry))
    (setf (parser-used-names parser) (make-hash-table :test 'equal))))

;; TODO:
(defun addtoken (parser type value context)
  ;; Add a token; return t iff this is the end of the program.
  (let ((ilabel (classify parser type value context)))
    ))

(defun classify (parser type value context)
  "Turn a token into a label.  \(Internal)"
  (catch 'return
    (let* (ilabel
           (grammar (parser-grammar parser))
           (used-names (parser-used-names parser))
           (keywords (grammar-keywords grammar))
           (tokens (grammar-tokens grammar)))
      (when (eq type token-NAME)
        ;; Keep a listing of all used names
        (puthash value 1 used-names)
        ;; Check for reserved words
        (setq ilabel (gethash value keywords))
        (when ilabel
          (throw 'return ilabel)))
      (setq ilabel (gethash type tokens))
      (unless ilabel
        (throw 'parse-error (list "bad token" type value context)))
      ilabel)))

;; TODO: test it somehow, don't really yet now how
(defun shift (parser type value newstate context)
  "Shift a token.  \(Internal)"
  (let* ((grammar (parser-grammar parser))
         (convert (parser-convert-function))
         (stack (parser-stack parser))
         (stack-entry (car stack))
         (setq stack (cdr stack))
         ;; original stackentry parts
         (dfa (nth 0 stack-entry))
         (state (nth 1 stack-entry))
         (node (nth 2 stack-entry))
         ;; a modified node
         newnode)
    (setq newnode (list type value context nil))
    (setq newnode (funcall convert grammar newnode))
    (when newnode
      (setf (elt node 3)
            (cons newnode (elt node 3))))
    (setf (parser-stack parser) (cons (list dfa newstate node)))))

;; TODO
(defun push (parser type newdfa newstate context)
  "Push a nonterminal.  \(Internal)"
  )

;; TODO
(defun pop (parser)
  "Pop a nonterminal.  \(Internal)"
  )

)


(provide 'parser)
