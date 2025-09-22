#lang racket

;; ===========================================
;; 基础的面向对象系统实现
;; ===========================================

;; 消息传递机制 - OOP的核心
;; 对象就是一个能响应消息的过程

;; 创建一个简单的计数器类
(define (make-counter initial-value)
  (let ((count initial-value))
    (lambda (message . args)
      (cond
        ((eq? message 'get) count)
        ((eq? message 'set!) 
         (set! count (car args)))
        ((eq? message 'increment!) 
         (set! count (+ count 1)))
        ((eq? message 'decrement!) 
         (set! count (- count 1)))
        ((eq? message 'add!) 
         (set! count (+ count (car args))))
        (else (error "Unknown message:" message))))))

;; 使用计数器
(define counter1 (make-counter 0))
(define counter2 (make-counter 10))

(printf "=== 基础对象测试 ===\n")
(printf "counter1 初始值: ~a\n" (counter1 'get))
(printf "counter2 初始值: ~a\n" (counter2 'get))

(counter1 'increment!)
(counter2 'add! 5)

(printf "操作后 counter1: ~a\n" (counter1 'get))
(printf "操作后 counter2: ~a\n" (counter2 'get))

;; ===========================================
;; 更复杂的类系统 - 支持继承
;; ===========================================

;; 定义一个通用的类创建机制
(define (make-class superclass . methods)
  (lambda (message . args)
    (let ((method (assoc message methods)))
      (if method
          (apply (cdr method) args)
          (if superclass
              (apply superclass message args)
              (error "Unknown message:" message))))))

;; 基类：动物
(define animal-class
  (make-class #f
    (cons 'init (lambda (name)
                  (lambda (message . args)
                    (let ((self-name name))
                      (cond
                        ((eq? message 'get-name) self-name)
                        ((eq? message 'speak) "Some sound")
                        ((eq? message 'describe) 
                         (string-append "I am " self-name))
                        (else (error "Unknown message:" message)))))))))

;; 创建动物实例的工厂函数
(define (make-animal name)
  ((animal-class 'init name)))

;; 更高级的类系统实现
(define-syntax-rule (define-class class-name superclass (field ...) (method-name method-body) ...)
  (define (class-name . init-args)
    (let ((field #f) ...)  ; 初始化字段
      (letrec ((self (lambda (message . args)
                       (cond
                         ((eq? message 'method-name) method-body) ...
                         ((and superclass (not (eq? message 'method-name)) ...)
                          (apply superclass message args))
                         (else (error "Unknown message:" message))))))
        (when init-args (apply (self 'init) init-args))
        self))))

;; 使用更简洁的语法定义类
(define (make-person name age)
  (let ((my-name name)
        (my-age age))
    (lambda (message . args)
      (cond
        ((eq? message 'get-name) my-name)
        ((eq? message 'get-age) my-age)
        ((eq? message 'set-age!) (set! my-age (car args)))
        ((eq? message 'birthday!) (set! my-age (+ my-age 1)))
        ((eq? message 'greet) 
         (string-append "Hello, I'm " my-name 
                       ", I'm " (number->string my-age) " years old."))
        ((eq? message 'can-vote?) (>= my-age 18))
        (else (error "Unknown message:" message))))))

;; 学生类继承自人类
(define (make-student name age school)
  (let ((person (make-person name age))
        (my-school school)
        (grades '()))
    (lambda (message . args)
      (cond
        ((eq? message 'get-school) my-school)
        ((eq? message 'add-grade!) 
         (set! grades (cons (car args) grades)))
        ((eq? message 'get-grades) grades)
        ((eq? message 'average-grade)
         (if (null? grades)
             0
             (/ (apply + grades) (length grades))))
        ((eq? message 'greet)
         (string-append (person 'greet) 
                       " I study at " my-school "."))
        ;; 委托给父类
        (else (apply person message args))))))

;; 测试继承系统
(printf "\n=== 继承系统测试 ===\n")
(define alice (make-person "Alice" 25))
(define bob (make-student "Bob" 20 "MIT"))

(printf "Alice: ~a\n" (alice 'greet))
(printf "Bob: ~a\n" (bob 'greet))
(printf "Alice can vote: ~a\n" (alice 'can-vote?))
(printf "Bob can vote: ~a\n" (bob 'can-vote?))

(bob 'add-grade! 90)
(bob 'add-grade! 85)
(bob 'add-grade! 92)
(printf "Bob's grades: ~a\n" (bob 'get-grades))
(printf "Bob's average: ~a\n" (bob 'average-grade))

;; ===========================================
;; 支持多态的形状系统
;; ===========================================

;; 基础形状类
(define (make-shape color)
  (lambda (message . args)
    (cond
      ((eq? message 'get-color) color)
      ((eq? message 'set-color!) (set! color (car args)))
      ((eq? message 'area) 0)  ; 默认实现
      ((eq? message 'perimeter) 0)  ; 默认实现
      ((eq? message 'describe)
       (string-append "A " color " shape"))
      (else (error "Unknown message:" message)))))

;; 圆形类
(define (make-circle color radius)
  (let ((shape (make-shape color))
        (r radius))
    (lambda (message . args)
      (cond
        ((eq? message 'get-radius) r)
        ((eq? message 'set-radius!) (set! r (car args)))
        ((eq? message 'area) (* 3.14159 r r))
        ((eq? message 'perimeter) (* 2 3.14159 r))
        ((eq? message 'describe)
         (string-append "A " (shape 'get-color) 
                       " circle with radius " (number->string r)))
        ;; 委托给父类
        (else (apply shape message args))))))

;; 矩形类
(define (make-rectangle color width height)
  (let ((shape (make-shape color))
        (w width)
        (h height))
    (lambda (message . args)
      (cond
        ((eq? message 'get-width) w)
        ((eq? message 'get-height) h)
        ((eq? message 'set-width!) (set! w (car args)))
        ((eq? message 'set-height!) (set! h (car args)))
        ((eq? message 'area) (* w h))
        ((eq? message 'perimeter) (* 2 (+ w h)))
        ((eq? message 'describe)
         (string-append "A " (shape 'get-color) 
                       " rectangle " (number->string w) 
                       "x" (number->string h)))
        ;; 委托给父类
        (else (apply shape message args))))))

;; 多态函数 - 对不同类型的对象调用相同的方法
(define (print-shape-info shape)
  (printf "~a\n" (shape 'describe))
  (printf "  Area: ~a\n" (shape 'area))
  (printf "  Perimeter: ~a\n" (shape 'perimeter)))

;; 测试多态
(printf "\n=== 多态系统测试 ===\n")
(define shapes (list (make-circle "red" 5)
                     (make-rectangle "blue" 4 6)
                     (make-circle "green" 3)))

(for-each print-shape-info shapes)

;; ===========================================
;; 更高级的特性：方法查找和动态分发
;; ===========================================

;; 支持方法查找链的对象系统
(define (make-object methods parent)
  (lambda (message . args)
    (let ((method (assoc message methods)))
      (if method
          (apply (cdr method) args)
          (if parent
              (apply parent message args)
              (error "Method not found:" message))))))

;; 创建一个支持方法添加的可扩展对象
(define (make-extensible-object)
  (let ((methods '()))
    (lambda (message . args)
      (cond
        ((eq? message 'add-method!)
         (let ((method-name (car args))
               (method-proc (cadr args)))
           (set! methods (cons (cons method-name method-proc) methods))))
        ((eq? message 'list-methods)
         (map car methods))
        (else
         (let ((method (assoc message methods)))
           (if method
               (apply (cdr method) args)
               (error "Method not found:" message))))))))

;; 测试可扩展对象
(printf "\n=== 可扩展对象测试 ===\n")
(define obj (make-extensible-object))

(obj 'add-method! 'greet (lambda () "Hello!"))
(obj 'add-method! 'add (lambda (x y) (+ x y)))

(printf "Greeting: ~a\n" (obj 'greet))
(printf "Addition: ~a\n" (obj 'add 3 4))
(printf "Available methods: ~a\n" (obj 'list-methods))

;; ===========================================
;; 总结和思考
;; ===========================================

(printf "\n=== 系统特性总结 ===\n")
(printf "1. 封装：数据和方法封装在闭包中\n")
(printf "2. 继承：通过委托实现方法继承\n")
(printf "3. 多态：相同消息在不同对象上有不同行为\n")
(printf "4. 动态性：运行时可以添加方法\n")
(printf "5. 消息传递：统一的对象交互机制\n")