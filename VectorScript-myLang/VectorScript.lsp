(defmacro parse-vec (vec)
    `(
        (lambda()
            (setq start 1 )
            (setq x-val (car (parse-num-pair ,vec start)))
            (setq start (cadr (parse-num-pair ,vec start)))
            (setq y-val (car (parse-num-pair ,vec start)))
            (setq start (cadr (parse-num-pair ,vec start)))
            (setq z-val (car (parse-num-pair ,vec start)))
            (setq start (cadr (parse-num-pair ,vec start)))

            (values-list (list (list x-val y-val z-val) start))
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
                        ((parse-integer elem :junk-allowed t) (parse-integer elem :junk-allowed t))
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
            ((atom ,vec) (princ ,vec))
            (t (princ (concatenate 'string "[" (string-trim "() " (format nil "~a" ,vec)) "]")))
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
