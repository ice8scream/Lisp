;1. Определите макрос, который возвращает свой вызов.  ------------------------
(defmacro self () '(quote (self)))

(print '(macro 1))
(print (self))

;------------------------------------------------------------------------------
;2. Определите макрос (POP стек), который читает из стека верхний элемент и  --
;   меняет значение переменной стека.  ----------------------------------------
(defmacro pop-stack (stack)
  `(prog1
     (setq top (car ,stack))
     (setq ,stack (cdr ,stack))))

(setq my-stack '(1 2 3 5 6 7))

(print '(macro 2))

(print (pop-stack my-stack))
(print (pop-stack my-stack))
(print (pop-stack my-stack))
(print (pop-stack my-stack))
(print (pop-stack my-stack))
(print (pop-stack my-stack))

;------------------------------------------------------------------------------
;3. Определите лисповскую форму (IF условие p q) в виде макроса.  -------------
(defmacro my-if (condition p q)
  `(if ,condition ,p ,q)
)

(print '(macro 3))

(print (my-if (< 1 4) 'true 'false))
(print (my-if (< 4 1) 'true 'false))

;------------------------------------------------------------------------------
;4. Определите в виде макроса форму (FIF тест отр нуль полож).  ---------------
(defmacro FIF (test negative zero positive)
  `(cond
     ((< ,test 0) ,negative)
     ((= ,test 0) ,zero)
     (t ,positive)))

(print '(macro 4))

(print (FIF -5 'negative 0 'positive))
(print (FIF 10 'negative 0 'positive))
(print (FIF 0 'negative 0 'positive))

;------------------------------------------------------------------------------
;5. Определите в виде макроса форму (REPEAT e UNTIL p) паскалевского типа.  ---

(defmacro repeat (e until p)
  `(cond 
     (,p (prog1 ,e (repeat ,e until ,p)))
   )
)

(print '(macro 5))

(setq stack `(1 2 3 4 5 6 7))
(repeat (print (pop stack)) until stack)