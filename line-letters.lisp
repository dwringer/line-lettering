(let* ((alphabet  '((a . ("  __"
			  " /_/"
			  "/ / "))
		    (b . ("  __"
			  " /_/"
			  "/_/ "))
		    (c . ("  __"
			  " /  "
			  "/_  "))
		    (d . ("   "
			  " /)"
			  "/_/"))
		    (e . ("  __"
			  " /_ "
			  "/_  "))
		    (f . ("  __"
			  " /_ "
			  "/   "))
		    (g . ("  __"
			  " /_ "
			  "/_/ "))
		    (h . ("    "
			  " /_/"
			  "/ / "))
		    (i . ("  "
			  " /"
			  "/ "))
		    (j . ("    "
			  "   /"
			  "/_/ "))
		    (k . ("    "
			  " /_/"
			  "/ ( "))
		    (l . ("   "
			  " / "
			  "/_ "))
		    (m . ("    "
			  " ///"
			  "/ / "))
		    (n . ("     "
			  " // /"
			  "/ // "))
		    (o . ("  __"
			  " / /"
			  "/_/ "))
		    (p . ("  __"
			  " /_/"
			  "/   "))
		    (q . ("  __"
			  " / /"
			  "/_X "))
		    (r . ("  __"
			  " /_/"
			  "/ ( "))
		    (s . ("  __"
			  " /_ "
			  "__/ "))
		    (t . (" ____"
			  "  /  "
			  " /   "))
		    (u . ("    "
			  " / /"
			  "/_/ "))
		    (v . ("    "
			  " ( /"
			  " (/ "))
		    (w . ("    "
			  " / /"
			  "/// "))
		    (x . ("   "
			  " (/"
			  " /("))
		    (y . ("   "
			  " (/"
			  " / "))
		    (z . (" __"
			  "  /"
			  " /_"))
		    (_ . ("  "
			  "  "
			  "  "))))
       (char-widths (mapcar #'(lambda (xy) (cons (car xy) (length (cadr xy)))) alphabet)))

  (defun print-row (str &optional (destination t))
    (let ((row1 "") (row2 "") (row3 ""))
      (macrolet ((append-row (_r _s)
		   `(setf ,_r (concatenate 'string ,_r ,_s))))
	(dolist (ch (coerce str 'list))
	  (when (equal ch #\space)
	    (setf ch #\_))
	  (multiple-value-bind (r1 r2 r3)
	      (apply #'values (cdr (assoc (read-from-string (string ch)) alphabet)))
	    (progn (append-row row1 r1) (append-row row2 r2) (append-row row3 r3))))
	(format destination "~A~%~A~%~A~%" row1 row2 row3))))

  (defun print-string (str &optional (destination t) (terminal-row-length 80))
    (let (words word-lengths)
      ;; Collect words and their lengths:
      (do* ((chars (coerce str 'list) (cdr chars))
	    (char  (car chars)        (car chars))
	    (wlen  0)
	    word)
	   ((and (= wlen 0) (null char) (null chars)))
	(if (or (and (equal char #\space)
		     (not (null word)))
		(null char))
	    (progn
	      (push wlen word-lengths)
	      (push (coerce (reverse word) 'string) words)
	      (setf wlen 0
		    word nil))
	    (progn (incf wlen (cdr (assoc (read-from-string (string char)) char-widths)))
		   (push char word))))      ;; Gather rows into words:
      (do* (rows
	    row
	    (row-length        0)
	    (words-remaining   (reverse (copy-list words))        (cdr words-remaining))
	    (lengths-remaining (reverse (copy-list word-lengths)) (cdr lengths-remaining))
	    (next-word         (car words-remaining)              (car words-remaining))
	    (next-length       (car lengths-remaining)            (car lengths-remaining)))
	   ((and (= row-length 0) (null next-word) (null words-remaining))
	    (mapcar #'(lambda (row) (print-row row destination)) (reverse rows)))
	(when (not (null (cadr words-remaining)))
	  (incf next-length 2)
	  (setf next-word (format nil "~A " next-word)))
	(when (or (null words-remaining)
		  (< terminal-row-length (+ next-length row-length)))
	  (setf row-length 0)
	  (when (not (null row))
	    (push (apply #'concatenate (cons 'string (reverse row))) rows)
	    (setf row nil)))
	(when (not (null next-word))
	  (push next-word row)
	  (incf row-length next-length))))))
