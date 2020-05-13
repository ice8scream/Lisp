
;7.Написать программу для нахождения минимального из чисел, являющихся
;  максимальными в каждой из строк заданной прямоуголоной матрицы. 
;  Использовать применяющие и/или отображающие функционалы.

(defun find-elem (lst op &optional (max (car lst)))
  (cond
      ((null lst) max)
      ((apply op (list (car lst) max)) (find-elem (cdr lst) op (car lst)))
      (t (find-elem (cdr lst) op max))
   )
)

(defun get-max(lst)
    (find-elem lst '>)
)

(defun get-min(lst)
    (find-elem lst '<)
    )

(defun get-min-of-max-strings(mx) 
        (get-min (mapcar 'get-max mx))
)

(print (get-min-of-max-strings `((1 2 10) (4 5 6) (7 8 9))))

;-------------------------------------------------------------

(defun print-matrix (matrix)
    (format t "~%~%/-----------~%~{~{~3,d~^ ~}~%~%~}\\--------------~%" matrix))

(defun sum-mtrx-rect (m &key row-from row-to col-from col-to)
    (apply '+ (mapcar #'(lambda (row) (apply '+ (subseq row col-from col-to))) (subseq m row-from row-to))))

(defun matrix ()
    (let ((m '((1))) (n 1))
         (lambda () (prog1
            (reverse (mapcar 'reverse m))
            (setf m ((lambda (tm) (cons (list (+ 1 (caar tm))) tm))
                     (mapcar #'(lambda (row) (cons (+ n (car row)) row)) m)))
            (incf n)))))


(setq m-gen (matrix))
(setq m-static (let ((res nil)) (dotimes (i 100 res) (setf res (funcall m-gen)))))


(print-matrix m-static)
(print (sum-mtrx-rect m-static :row-from 0 :row-to 10 :col-from 4 :col-to 5))