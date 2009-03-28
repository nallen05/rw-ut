;
; http://github.com/nallen05/rw-ut
;
;  TODO
; 
;     -tests
;     -names of months and days of week
;     -12 hour clock, a.m./p.m.
;     -optimize


(defpackage :rw-ut
  (:use :cl)
  (:export ; using
	   :read-time-string
	   :write-time-string
           :*time-zone*

	   ; old [depricated] API
	   :r-ut
	   :w-ut

	   ; compiling fast readers or writers at runtime
	   :read-time-string-pattern->src
	   :write-time-string-pattern->src))

(in-package :rw-ut)

; special

(defvar *time-zone* 0) ; BEWARE: assumes GMT/UTC by default

(defconstant +default-ut-pattern+
  (if (boundp '+default-ut-pattern+) ; SBCL complains at normal DEFCONSTANT...
      (symbol-value '+default-ut-pattern+)
      "YYYY?/MM?/DD? hh?:mm?:ss"))

; util

(defun .get-match (string pattern-table)
"
   (.get-match \"01234\" '((\"0123\" . xx)))

   -> xx, 4

   (.get-match \"abcde\" '((\"0123\" . xx)))

   -> NIL
"
  (dolist (% pattern-table)
    (destructuring-bind (pattern . xx) %
      (let ((pattern-length (length pattern))
	    (mismatch (mismatch string pattern)))
	(if (or (null mismatch)
		(eql mismatch pattern-length))
	    (return-from .get-match (values xx pattern-length)))))))

(defun .parse-string (string pattern-table &optional (accum ""))
"
   (.parse-string \"jjj0123jjjjj012345abc\" '((\"01234\" . |1-4|) (\"0123\" . |1-3|) (\"abc\" . ABC))

   -> (\"jjj\" |1-3| \"jjjjj\" |1-4| \"5\" ABC)
"
  (if (zerop (length string))
      (if (not (zerop (length accum)))
	  (list accum))
      (let ((1st-char (char string 0)))
	(if (char= 1st-char #\\)
	    (.parse-string (subseq string 2)
			   pattern-table
			   (format nil "~A~A" accum (char string 1)))
	    (multiple-value-bind (fn pattern-length)
		(.get-match string pattern-table)
	      (if fn
		  (if (zerop (length accum))
		      (cons fn #1=(.parse-string (subseq string pattern-length)
						 pattern-table))
		      (list* accum fn #1#))
		  (.parse-string (subseq string 1) pattern-table (format nil "~A~A" accum (char string 0)))))))))

; READ-TIME-STRING

(defparameter *read-ut-pattern-table*
  `(("?"     . ?)
    ("YYYY" . year)
    ("YY"   . year)
    ("MM"   . month)
    ("M"    . month)
    ("DD"   . date)
    ("D"    . date)
    ("hh"   . hour)
    ("h"    . hour)
    ("mm"   . minute)
    ("m"    . minute)
    ("ss"   . second)
    ("s"    . second)))

(defun read-time-string-pattern->src (pattern)
  `(lambda (string)
    (let ((string-length (length string))
	  (n 0)
	  (year 0)
	  (hour 0)
	  (minute 0)
	  (second 0)
	  (date 1)
	  (month 1))
      (declare (ignorable string-length))
      ,@(labels ((rfn (%s)
		   (when %s
		     (destructuring-bind (1st% . rest%) %s
		       (etypecase 1st%
			 (string (cons `(incf n ,(length 1st%)) (rfn rest%)))
			 (symbol (case 1st%
				   (? (list `(when (> string-length n)
					       ,@(rfn rest%))))
				   (otherwise (cons `(multiple-value-setq (,1st% n)
						       (parse-integer string
								      :start n
								      :junk-allowed t))
						    (rfn rest%))))))))))
		(rfn (.parse-string pattern *read-ut-pattern-table*)))
      (encode-universal-time second minute hour date month year *time-zone*))))

(defun read-time-string (string &optional (pattern +default-ut-pattern+))
  "reads `STRING' according to the pattern string `PATTERN' and returns a universal time
like would be returned by ENCODE-UNIVERSAL-TIME"
  (funcall (coerce (read-time-string-pattern->src pattern) 'function)
	   string))

(define-compiler-macro read-time-string (&whole whole string &optional (pattern +default-ut-pattern+))
  (cond ((stringp pattern) `(funcall ,(read-time-string-pattern->src pattern) ,string))
	((and (symbolp pattern)
	      (constantp pattern)) `(funcall ,(read-time-string-pattern->src (symbol-value pattern)) ,string))
	(t (warn "<<<READ-TIME-STRING is a _lot_ faster if `PATTERN' is a constant or a string. see READ-TIME-STRING-PATTERN->SRC if you need to compile a fast reader at runtime>>>")
	   whole)))

; WRITE-TIME-STRING

(defparameter *write-ut-pattern-table*
  `(("?"    . ?)
    ("YYYY" year   n)
    ("YY"   year   2)
    ("MM"   month  2)
    ("M"    month  n)
    ("DD"   date   2)
    ("D"    date   n)
    ("hh"   hour   2)
    ("h"    hour   n)
    ("mm"   minute 2)
    ("m"    minute n)
    ("ss"   second 2)
    ("s"    second n)))

(defun .parsed-string->ut-parts (parsed-pattern)
"
   (.parsed-string->ut-parts '(\"jjj\" (year n) \"jjjjj\" (date 2) ? \"5\" (month 2)))

   -> (year date month)
"
  (mapcar 'first (remove-if-not 'listp parsed-pattern)))

(defun .ut-part->minimum-value (symbol)
  (ecase symbol
    ((second minute hour year) 0)
    ((date month) 1)))

(defun write-time-string-pattern->src (pattern)
  `(lambda (ut)
     (multiple-value-bind (second minute hour date month year)
	 (decode-universal-time ut *time-zone*)
       (declare (ignorable second minute hour date month year))
       (with-output-to-string (out)
	 ,@(labels ((rfn (%s)
		      (when %s
			(destructuring-bind (1st% . rest%) %s
			  (typecase 1st%
			    (string (cons `(write-string ,1st% out) (rfn rest%)))
			    (otherwise (case 1st%
					 (? (list `(unless (and ,@(mapcar (lambda (_) `(= ,_ ,(.ut-part->minimum-value _)))
									(.parsed-string->ut-parts rest%)))
						     ,@(rfn rest%))))
					 (otherwise (destructuring-bind (symbol digits) 1st%
						      (cons (ecase digits
							      (n `(princ ,symbol out))
							      (2 (case symbol
								   (year `(format out "~2,'0d" (mod year 100)))
								   (otherwise `(format out "~2,'0d" ,symbol)))))
							    (rfn rest%)))))))))))

		    (rfn (.parse-string pattern *write-ut-pattern-table*)))))))

(defun write-time-string (ut &optional (pattern +default-ut-pattern+))
  "write the universal `UT' as a string according to the pattern string `PATTERN'"
  (funcall (coerce (write-time-string-pattern->src pattern) 'function)
	   ut))

(define-compiler-macro write-time-string (&whole whole ut &optional (pattern +default-ut-pattern+))
  (cond ((stringp pattern) `(funcall ,(write-time-string-pattern->src pattern) ,ut))
	((and (symbolp pattern)
	      (constantp pattern)) `(funcall ,(write-time-string-pattern->src (symbol-value pattern)) ,ut))
	(t (warn "<<<WRITE-TIME-STRING is a _lot_ faster if `PATTERN' is a constant or a string. see WRITE-TIME-STRING-PATTERN->SRC if you need to compile a fast writer at runtime>>>")
	   whole)))

(defun write-time-string* (ut &rest pattern-and-format-args)
  "like WRITE-TIME-STRING except PATTERN can be followed by N format arguments [like FORMAT]
that are plugged in to PATTERN _after_ WRITE-TIME-STRING does it's magic"
  (apply #'format
	 nil
	 (funcall (coerce (write-time-string-pattern->src (first pattern-and-format-args)) 'function)
		  ut)
	 (rest pattern-and-format-args)))

(define-compiler-macro write-time-string* (&whole whole ut &rest pattern-and-format-args)
  (let ((pattern (or (first pattern-and-format-args)
		     +default-ut-pattern+))
	(args (rest pattern-and-format-args)))
    (cond ((stringp pattern) `(apply #'format
				     nil
				     (funcall ,(write-time-string-pattern->src pattern) ,ut)
				     ,@args))
	  ((and (symbolp pattern)
		(constantp pattern)) `(apply #'format
		nil
		(funcall ,(write-time-string-pattern->src (symbol-value pattern)) ,ut)
		,@args))
	  (t (warn "<<<WRITE-TIME-STRING* is a _lot_ faster if `PATTERN' is a constant or a string. see WRITE-TIME-STRING-PATTERN->SRC if you need to compile a fast writer at runtime>>>")
	     whole))))

; old [depricated] API

(defmacro r-ut (&rest args)
  (warn "use of the R-UT macro is depricated. use READ-TIME-STRING instead")
  `(read-time-string ,@args))

(defmacro w-ut (&rest args)
  (warn "use of the W-UT macro is depricated. use WRITE-TIME-STRING instead")
  `(write-time-string ,@args))

