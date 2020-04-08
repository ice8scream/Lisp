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

;6. Структура графа задана списками смежности номеров вершин, то есть списком, элементами которого являются пары, состоящие из номера вершины и
;   списка вершин, инцидентных ей.
;   type Graph = (Int, (Int, Int, ...))
;   Написать функцию, которая выдает длину кратчайшего маршрута между двумя заданными вершинами. Длиной считать количество вершин, встретившихся в данном маршруте. Если маршрута между вершинами не существует, то
;   функция должна выдавать ноль.


;  A   G
; / \   \
;B   C   H
; \ / \
;  D   E
;   \ /
;    F

(setq graph '(
    (A (B C))
    (B (A D))
    (C (A D E))
    (D (B C F))
    (E (C F))
    (F (D E))
    (G (H))
    (H (G))
))

(defun is-contains(lst elem)
    (cond
        ((null lst)             nil)
        ((eq (car lst) elem)     T)
        ( T                     (is-contains (cdr lst) elem))
    )
)

(defun get-hordes(lst node)
    (cond
        ((null lst)             nil)
        ((eq (caar lst) node)   (cadar lst))
        ( T                     (get-hordes (cdr lst) node))
    )
)

(defun min-val(lst &OPTIONAL (min nil))
    (cond 
        ((null lst)         min)
        ( T                 ((lambda (change-min keep-min)
                                (cond
                                        ((eq nil min)       change-min)
                                        ((eq nil (car lst)) keep-min)
                                        ((< (car lst) min)  change-min)
                                        ( T                 keep-min)
                                )                    
                                ) (min-val (cdr lst) (car lst)) (min-val (cdr lst) min))
        )
    )
)

(defun wfs(lst strt gol &OPTIONAL (used nil) )
    (cond 
        ((eq strt gol)              0)
        ((is-contains used strt)    nil)
        ( T 
            ((lambda(len)
            (cond
                ((eq len nil) nil)
                ( T         (+ 1 len))
             )
            ) (min-len lst (get-hordes lst strt) gol (cons strt used)))
         )
    )
)

(defun min-len(hordes lst gol used &OPTIONAL (lens nil))
    (cond
        ((null lst)     (min-val lens))
        ( T             
                            (min-len hordes (cdr lst) gol used (cons (wfs hordes (car lst) gol used) lens))
                       
        )
    )
)

(defun get-path(lst strt gol)
    ((lambda(path)
        (cond
            ((eq path nil)   0)
            ( T (+ 1 path))
        )
    ) (wfs lst strt gol))
)

;
;-------------------------------------------------------------------------
;

(print (get-path graph 'A 'F))
(print (get-path graph 'A 'D))
(print (get-path graph 'A 'C))
(print (get-path graph 'A 'A))
(print (get-path graph 'A 'H))

