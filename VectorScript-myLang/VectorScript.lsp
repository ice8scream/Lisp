(defun parse-string-to-floats (string)
    (setq first (position #\Space string))
    (setq second (position #\Space string :from-end t))
    (mapcar 'read-from-string
        (list 
            (subseq string 0 first) 
            (subseq string (1+ first) second) 
            (subseq string (1+ second))
        )
    )
)





(defmacro parse-vec (vec)
    `(
        (lambda()
            (parse-string-to-floats (subseq ,vec 1 (1- (length ,vec))))
        )
    )
)

(defmacro parse-num-pair(line start) 
    `(multiple-value-list (parse-integer ,line :start ,start :junk-allowed t)
    )
)

(defmacro get-operator(line)
    `(cond
        ((string= ,line "+")
                'vec-sum
            )
        ((string= ,line "-")
                'vec-diff
            )
        ((string= ,line "*")
                'scal-mul
            )
        ((string= ,line "/")
                'scal-div
            )
        ((string= ,line "dot")
                'dot-prod
            )
        ((string= ,line "cross")
                'cross-prod
            )
        ((string= ,line "=")
            'is-equal
        )
        ((string= ,line "len")
            'vec-length
        )
        ((string= ,line "norm")
            'vec-normalize
        )
        ((string= ,line "rev")
            'vec-reverse
        )
        (t line)
    )
)

(defmacro get-operation-elements (line)
    `(
        (lambda ()
            (setq first (1+ (position #\] ,line)))
            (if (= first (length ,line)) (setq first (position #\Space ,line)))
            (setq second (position #\[ ,line))
            (setq second-pos (position #\Space ,line :start (1+ first)))
            (if (= second 0) (setq second (cond (second-pos (1+ second-pos))(t (length ,line)))))
            (mapcar 
                (lambda(elem)
                    (cond
                        ((eq elem nil) nil)
                        ((find #\[ elem) (parse-vec elem))
                        ((parse-integer elem :junk-allowed t) (read-from-string elem))
                        (t (get-operator elem))
                    )
                )
            (list   
                    (subseq ,line (1+ first) (cond (second-pos (1- second)) (t (length ,line))))
                    (subseq ,line 0 first) 
                    (cond 
                       (second-pos (subseq ,line second))
                       (t nil)
                    )
                )
            )
        )
    )
)

(defun and-func(lst)
    (cond 
        ((null lst) t)
        (t (and (car lst) (and-func (cdr lst))))
    )
)

(defun vec-reverse (vec)
    (scal-mul vec -1)
)
(defun vec-length (vec)
    (sqrt (apply '+ (mapcar (lambda(x) (* x x)) vec)))
)

(defun vec-normalize (vec)
    (mapcar (lambda (dem) (/ dem (vec-length vec))) vec)
)

(defun is-equal (1op 2op)
    (and-func (mapcar 'eq 1op 2op))
)

(defun vec-sum (1op 2op)
    (mapcar '+ 1op 2op)
)
(defun vec-diff (1op 2op)
    (mapcar '- 1op 2op)
)
(defun scal-mul (1op 2op)
    (cond 
        ((atom 1op) (scal-mul 2op 1op))
        (t (mapcar (lambda(1op) (* 1op 2op)) 1op))
    )
)
(defun scal-div (1op 2op)
    (mapcar (lambda(1op) (/ 1op 2op)) 1op)
)
(defun cross-prod (1op 2op)   
        (apply
            (lambda (x1 x2 x3 y1 y2 y3)
            (list (- (* x2 y3) (* x3 y2)) (- (* x3 y1) (* x1 y3)) (- (* x1 y2) (* x2 y1)) )
            ) (append 1op 2op)
        )
)
(defun dot-prod (1op 2op)
    ( /
        (apply '+ (mapcar '* 1op 2op))
        (*  (vec-length 1op)
            (vec-length 2op)
        )
    )
)

(defun make-operation (operator 1op 2op)
    (cond
        (2op (funcall operator 1op 2op))
        (t (funcall operator 1op))
    )
)

(defmacro print-vec (vec)
    `(prog1
        (cond
            ((eq t ,vec) (princ ,vec))
            ((atom ,vec) (princ (float ,vec)))
            (t (princ (concatenate 'string "[" (string-trim "() " (format nil "~a" (mapcar 'float ,vec))) "]")))
        )
        (write-line "")
    )
)


(with-open-file (file #P"test.vs")
    (loop for i from 0
        for line = (read-line file nil nil)
        while line
        do 
            (princ (concatenate 'string line " = " ))
            (print-vec (apply 'make-operation (get-operation-elements line)))
    )
)
