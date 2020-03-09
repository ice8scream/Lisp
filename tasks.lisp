;10,11,14,18,22,26,27,29,46,47

;46. Предположим, что отец и мать некоторого лица, хранятся как значения соответствующих свойств у символа, обозначающего это лицо. Напишите функцию (РОДИТЕЛИ x), которая возвращает в качестве значения родителей, и предикат (СЕСТРЫ-БРАТЬЯ x1 x2), который истинен в случае, если x1 и x2 — сестры или братья, родные или с одним общим родителем.
;47. Определите функцию УДАЛИТЬ-ВСЕ-СВОЙСТВА, которая удаляет все свойства символа.

;------------------------------------------------------------------
;10.Определите функцию, осуществляющую удаление указанного 
;   количества последних элементов исходного списка.

(defun erase-last-elem(lst)
    (cond
        ((null (cdr lst))   nil)
        (t                  (cons
                                (car lst)
                                (erase-last-elem (cdr lst))
                            )
        )
    )
)

(defun erase-list-tail(lst num)
    (cond
        ((null lst)     nil)
        ((< num 1)      lst)
        (t              (erase-list-tail
                            (erase-last-elem lst)
                            (1- num)
                        )
        )
    )
)

;
;------------------------------------------------------------------
;

(print (erase-list-tail '() 5))
(print (erase-list-tail '(1 2 3) 4))
(print (erase-list-tail '(1 2 3 4 5 6 7 8 9 0) 5))
(print (erase-list-tail '(1 2 3 4 5 6 7 8 9 0) 1))
(print (erase-list-tail '(1 2 3 4 5 6 7 8 9 0) 0))
(print (erase-list-tail '(1 2 3 4 5 6 7 8 9 0) -5))

;
;------------------------------------------------------------------
;11.Определите функцию, осуществляющую разделение исходного списка 
;   на два подсписка. В первый из них должно попасть указанное
;   количество элементов с начала списка, во второй — оставшиеся 
;   элементы.

(defun divide-list (lst num)
    (cond
        ((null lst)     '(() ()))
        ((< num 1)      ( list () lst ))
        (t ((lambda(h div-lst)
                (cons
                    (cons
                        h
                        (car div-lst)
                    )
                    (
                        cdr div-lst
                    )
            )) (car lst) (divide-list (cdr lst) (1- num))
            )
        )
    )
)

;
;------------------------------------------------------------------
;

(print (divide-list '( 1 2 3 4 5 6 7 8 9 0 ) -1))
(print (divide-list '( 1 2 3 4 5 6 7 8 9 0 ) 2))
(print (divide-list '( 1 2 3 4 5 6 7 8 9 0 ) 7))
(print (divide-list '( 1 2 3 4 5 6 7 8 9 0 ) 12))
(print (divide-list '( 1 2 3 4 5 6 7 8 9 0 ) 0))
(print (divide-list '() 5))
(print (divide-list '() -1))

;
;------------------------------------------------------------------
;14.Определите функцию, осуществляющую перестановку двух элементов
;   списка с заданными номерами.

(defun get-elem-by-index(lst index)
    (cond
        ((null lst)     nil)
        ((< index 1)    nil)
        ((= index 1)    (car lst))
        (t              ( get-elem-by-index (cdr lst) (1- index) ))
    )
)

(defun set-elem-by-index(lst index value)
    (cond
        ((null lst)     nil)
        ((< index 1)    lst)
        ((= index 1)    (cons 
                            value 
                            (cdr lst)
                        ))
        (t              (cons 
                            (car lst) 
                            ( set-elem-by-index (cdr lst) (1- index) value)
                        ))
    )
)

(defun swap-elements-x-y(lst x y)
    (set-elem-by-index 
        (set-elem-by-index 
            lst 
            x 
            (get-elem-by-index lst y)
        )
        y
        (get-elem-by-index lst x)
    )
)

;
;------------------------------------------------------------------
;

(print (swap-elements-x-y '( 1 2 3 4 5 6 7 8 9 0 ) 1 8 ))
(print (swap-elements-x-y '( 1 2 3 4 5 6 7 8 9 0 ) 8 1 ))
(print (swap-elements-x-y '( 1 2 3 4 5 6 7 8 9 0 ) -1 8 ))
(print (swap-elements-x-y '( 1 2 3 4 5 6 7 8 9 0 ) 1 12 ))
(print (swap-elements-x-y '( 1 2 3 4 5 6 7 8 9 0 ) -1 12 ))

;
;------------------------------------------------------------------
;18.Определите предикат, проверяющий, является ли аргумент 
;   одноуровневым списком.

(defun is-list-single-level(lst)
    (cond
        ((null lst)         T)
        ((listp (car lst))  NIL)
        ( T                (is-list-single-level (cdr lst)))
    )
)

;
;------------------------------------------------------------------
;

(print (is-list-single-level '(()()())      ))
(print (is-list-single-level '()            ))
(print (is-list-single-level '(1 2 3 4)     ))
(print (is-list-single-level '(1 (2) 3 4)   ))

;
;------------------------------------------------------------------
;22.Определите функцию, которая обращает список (а b с) и разбивает 
;   его на уровни (((с) b) а).

(defun more-scopes(lst)
    (cond
        ((null (cdr lst))    lst)
        ( T                  (list ( more-scopes(cdr lst)) (car lst)))
    )
)

;
;------------------------------------------------------------------
;

(print (more-scopes '( 1 2 3 4 5 6 7 8 9 0 ) ))
(print (more-scopes '( 1 2 3 ) ))
(print (more-scopes '( ) ))

;
;------------------------------------------------------------------
;26.Определите функцию, разбивающую список (a b с d...) на пары 
;   ((а b) (с d)...).

(defun split-list(lst)
    (cond
        ((null lst)         lst)
        ((null (cdr lst))   (cons lst () ))
        ( T                 (cons   
                                (list (car lst) (cadr lst))
                                (split-list ( cddr lst )) 
                            ))
    )
)

;
;------------------------------------------------------------------
;

(print (split-list '( 1 2 3 4 5 6 7 8 9 0 ) ))
(print (split-list '( 1 2 3 4 5 6 7 8 9 ) ))
(print (split-list '( 1 ) ))

;
;------------------------------------------------------------------
;27.Определите функцию, которая, чередуя элементы списков (a b...) 
;   и (1 2...), образует новый список (a 1 b 2 ...).

(defun merge-append(left right)
    (cond
        ((null left)    right)
        ((null right)   left)
        ( T             (cons 
                            (car left) 
                            (cons 
                                (car right) 
                                (merge-append  
                                    (cdr left) 
                                    (cdr right)
                                )
                            )
                        )
        )
    )
)

;
;------------------------------------------------------------------
;

(print (merge-append '( 1 3 5 7 9 ) '( 2 4 6 8 0) ))
(print (merge-append '( 1 3 5 ) '( 2 4 6 7 8 9 0) ))
(print (merge-append '( 1 3 5 7 8 9 0) '( 2 4 6 ) ))

;
;------------------------------------------------------------------
;29.Определите функцию, вычисляющую глубину списка
;   (самой глубокой ветви).

(defun get-max-depth-list(lst)
    (cond
        ((null lst)     1)
        ((atom lst)     1)
        ((listp (car lst))      (cond
                                    ((> (1+ (get-max-depth-list (car lst))) (get-max-depth-list (cdr lst)))   (1+ (get-max-depth-list (car lst))))
                                    (T (get-max-depth-list (cdr lst)))
                                )
        )
        (T (get-max-depth-list (cdr lst)))
    )
)

;
;------------------------------------------------------------------
;

(print (get-max-depth-list '(1(2)(2)(2))                ))
(print (get-max-depth-list '(1(2(3))(2)(2))             ))
(print (get-max-depth-list '(1(2)1(2(3(4(5))))(2))      ))
(print (get-max-depth-list '(1(2)(2)(2(3(4(5)))))       ))
(print (get-max-depth-list '(1)                         ))



;; (defun get-list-by-range(lst l r)
;;     (cond
;;         ( (> l r)       (get-list-by-range lst r l ))
;;         ( (null lst)    nil)
;;         ( (< r 1)       nil)
;;         ( t     ((lambda(h l next-lst)
;;                         (cond
;;                             ( (< l 2)       (cons h next-lst ))
;;                             ( t             next-lst)
;;                         )
;;                 ) (car lst) l (get-list-by-range (cdr lst) (1- l) (1- r)) )
;;         )
;;     )
;; )