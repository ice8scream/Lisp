;1. Определите FUNCALL через функционал APPLY  --------------------------------

(defun my-funcall (func &rest args)
    (apply func args)
)

(defun unit-funcall (&rest args) (print args))

(print '(test 1))
(my-funcall 'unit-funcall 0 1 2)
(funcall 'unit-funcall 0 1 2)

;------------------------------------------------------------------------------
;3. Определите функционал (APL-APPLY f x), который применяет каждую функцию fi-
;   списка (f1 f2 ... fn) к соответствующему элементу списка x = (x1 x2 ... xn)
;   и возвращает список, сформированный из результатов.  ----------------------

(defun apl-apply (f x)
    (mapcar (lambda(f x) (funcall f x)) f x)
)

(print '(test 3))
(defun f1 (x) (* x 1))
(defun f2 (x) (* x 2))
(defun f3 (x) (* x 3))
(defun f4 (x) (* x 4))
(defun f5 (x) (* x 5))

(print (apl-apply '(f1 f2 f3 f4 f5) '(1 2 3 4 5)))

;------------------------------------------------------------------------------
;5. Определите функциональный предикат(НЕКОТОРЫй пред список),который истинен,-
;   когда, являющейся функциональным аргументом предикат пред истинен хотя бы -
;   для одного элемента списка список.  ---------------------------------------

(defun is-odd (x) 
    (cond
        ((= (mod x 2) 1) t)
        (t nil)
    )
)

(defun some-of-pred(pred lst)
    (not (null (mapcan pred lst)))
)

(print '(test 5))
(print (some-of-pred 'is-odd '(0 2 4 6 8)))
(print (some-of-pred 'is-odd '(0 2 4 6 8 9)))

;------------------------------------------------------------------------------
;7. Определите фильтр (УДАЛИТЬ-ЕСЛИ-НЕ пред список),удаляющий из списка список-
;   все элементы, которые не обладают свойством, наличие которого проверяет  --
;   предикат пред.  -----------------------------------------------------------

(defun is-member (lst elem)
    (cond 
        ((null (mapcan (lambda(x) (list (eq elem x))) lst)) nil)
        (t t)
    )
)

(defun is-has-property-p (x) 
    (is-member (symbol-plist x) 'p)
)

(defun my-delete-if-not (pred lst)
    (mapcan (lambda(x)
                   (cond
                       ((funcall pred x) (list x))
                       (t nil)
                   )
            ) lst)
)

(setf (get 'A 'p) t)
(setf (get 'B 'p) t)
(setf (get 'C 'p) t)
(setf (get 'D 'p) nil)

(print '(test 7))
(print (my-delete-if-not 'is-has-property-p '(A H B E C G D F)))

;------------------------------------------------------------------------------
;9. Напишите генератор порождения чисел Фибоначчи: 0, 1, 1, 2, 3, 5, ...  -----

(defun fib-gen()
    (let ((f 1) (s 0))
        (lambda () (setq 
                        temp s 
                        s (+ f s) 
                        f temp)
        
        )
    )
)

(setq gen-1 (fib-gen))
(setq gen-2 (fib-gen))

(print '(test 9))

(print '(gen-1))
(print (funcall gen-1))
(print (funcall gen-1))
(print (funcall gen-1))
(print (funcall gen-1))
(print (funcall gen-1))
(print (funcall gen-1))
(print '(gen-2))
(print (funcall gen-2))
(print (funcall gen-2))
(print (funcall gen-2))
(print (funcall gen-2))
(print (funcall gen-2))
(print (funcall gen-2))

;------------------------------------------------------------------------------
;11.Определите фукнционал МНОГОФУН, который использует функции, являющиеся  ---
;   аргументами, по следующей схеме:  -----------------------------------------
;   (МНОГОФУН ’(f g ... h) x) ⇔ (LIST (f x) (g x) ... (h x)).  ---------------

(defun mnogo-fun (fun-list x)
    (mapcar (lambda (f) (funcall f x)) fun-list)
)

(defun plus-2(x) (+ x 2))
(defun minus-2(x) (- x 2))
(defun umnog-na-2(x) (/ x 2))
(defun delit-na-2(x) (* x 2))

(print '(test 11))
(print (mnogo-fun '(plus-2  minus-2 umnog-na-2 delit-na-2) 2))

;------------------------------------------------------------------------------
;13.Определите функцию, которая возвращает в качестве значения свое определение
;   (лямбда-выражение)  -------------------------------------------------------

(setq self '( (lambda (x)
 (list x (list (quote quote) x)))
 (quote (lambda (x)
 (list x (list (quote quote) x))))))

(print '(test 13))
(print (eval self))
