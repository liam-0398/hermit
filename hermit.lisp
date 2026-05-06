
; HERMIT
; Shell
;
(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
(ql:quickload :cl-readline)
(require :sb-posix)

(defvar *input* '())
(defvar *history* '())

(defun view-history() ; Interactive history viewer
  (remove 1 *history*)
  (format t "~{~a ~}~%" *history*)
  (let ((choice (parse-integer (read-line))))
    (setf choice (- choice 1))
       (setf *input* (nth choice *history*))
       (parser)))

(defun builtin-echo ()
    (print (rest *input*)))

(defun builtin-pwd ()
    (print (sb-posix:getcwd)))

(defun builtin-cd ()
  (handler-case
    (sb-posix:chdir (second *input*))
  (error (e)
	 (format t "Enter path"))))


(defun split-string (str) ; Tokenizer
  (let ((tokens '())
        (current ""))
      (do ((i 0 (1+ i))) ((>= i (length str)))
      (cond
          ((and (char= (char str i) #\\)(char= (char str (1+ i)) #\space)) ; Check for FILE\ DIR
              ; Continue
              (incf i 1)
              (setf current (concatenate 'string current (string (char str i)))))
          ((char= (char str i) #\Space) ; Create new token at space
                (push current tokens)  
                (setf current "")    
                )
          (t
              (setf current (concatenate 'string current (string (char str i)))))))

    (push current tokens)          ; grab the last token
    (reverse tokens)))

(defun parser () 
    (cond
      ((equal (first *input*) "cd") (builtin-cd))
      ((equal (first *input*) "pwd") (builtin-pwd))
      ((equal (first *input*) "echo") (builtin-echo))
      ((equal (first *input*) "###") (view-history))
      ((equal (first *input*) "!!!") (sb-impl::toplevel-repl nil)) ; Drop to REPL

     (t
        (handler-case
          (sb-ext:run-program (first *input*) (rest *input*) :output t :input t :wait t :search t)
        (error (e)
              (format t "~a~%" e))))))

(defun prompt ()
  (loop do
    (format t ">> ")
    (force-output)
    (setf *input* (split-string (cl-readline:readline :add-history t)))
    (unless (equal (first *input*) "###")
      (push *input* *history*))

    (when (equal (first *input*) "exit") (sb-ext:exit))
    (parser)))


(defun main () (prompt))

;(main)
(sb-ext:save-lisp-and-die "hermit" :toplevel #'main :executable t)
;comment main and uncomment line above then load in sbcl to compile to standalone executable
