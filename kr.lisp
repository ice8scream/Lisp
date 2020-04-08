;4. Заданный числовой список разбить на подсписки из возрастающих 
;   подпоследовательностей максимальной длины рядом стоящих чисел. Так, например,
;   если исходный список состоял из чисел [2, 7, 10, 8, 3, 4, 9, 1, 2, 0, 8,
;   3, 2, 5], то результатом работы программы должен быть следующий список
;   списков: [[2, 7, 10], [8], [3, 4, 9], [1, 2], [0, 8], [3], [2, 5]].

(defun part-list(lst &OPTIONAL (part nil) )
    (cond
        ((null lst)     (list part))
        ((null part)    (part-list (cdr lst) (list (car lst))))
        ((> (car lst)   (last-elem part)) (part-list (cdr lst) (push-back part (car lst))))
        (t              (cons part (part-list lst)))
    )
)

(defun last-elem(lst)
    (cond 
        ((null (cdr lst))   (car lst))
        (t                  (last-elem (cdr lst)))
    )
)

(defun push-back(lst elem)
    (cond 
        ((null (cdr lst))   (list (car lst) elem))
        (t                  (cons (car lst) (push-back (cdr lst) elem)))
    )
)

;
;----------------------------------------------------------------
;

(print (part-list '(2 7 10 8 3 4 9 1 2 0 8 3 2 5)))
(print (part-list '(2 2 2 2)))
(print (part-list '(1 2 3)))
(print (part-list '(3 2 1)))
