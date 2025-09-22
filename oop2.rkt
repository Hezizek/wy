#lang racket

;; ===========================================
;; 高级面向对象系统扩展
;; ===========================================

;; 1. 接口系统 - 定义对象契约
(define (make-interface name required-methods)
  (lambda (message . args)
    (cond
      ((eq? message 'get-name) name)
      ((eq? message 'get-methods) required-methods)
      ((eq? message 'implements?)
       (let ((obj (car args)))
         (andmap (lambda (method)
                   (with-handlers ([exn:fail? (lambda (e) #f)])
                     (obj method)
                     #t))
                 required-methods)))
      (else (error "Unknown interface message:" message)))))

;; 定义一个 Drawable 接口
(define drawable-interface 
  (make-interface "Drawable" '(draw get-bounds)))

;; 定义一个 Serializable 接口
(define serializable-interface
  (make-interface "Serializable" '(serialize deserialize)))

;; 2. 多重继承支持
(define (make-mixin-object parents methods)
  (lambda (message . args)
    (let ((method (assoc message methods)))
      (if method
          (apply (cdr method) args)
          ;; 在所有父对象中查找方法
          (let loop ((ps parents))
            (if (null? ps)
                (error "Method not found:" message)
                (with-handlers ([exn:fail? (lambda (e) (loop (cdr ps)))])
                  (apply (car ps) message args))))))))

;; 3. 访问控制系统
(define (make-protected-object public-methods private-methods)
  (lambda (message . args)
    (cond
      ;; 检查是否为公共方法
      ((assoc message public-methods)
       => (lambda (method) (apply (cdr method) args)))
      ;; 私有方法只能内部调用（通过特殊消息）
      ((and (pair? args) (eq? (car args) 'internal-call) (assoc message private-methods))
       => (lambda (method) (apply (cdr method) (cdr args))))
      (else (error "Access denied or method not found:" message)))))

;; 4. 元类系统 - 类也是对象
(define (make-metaclass name)
  (let ((instances '())
        (class-methods '()))
    (lambda (message . args)
      (cond
        ((eq? message 'get-name) name)
        ((eq? message 'add-instance!) 
         (set! instances (cons (car args) instances)))
        ((eq? message 'get-instances) instances)
        ((eq? message 'instance-count) (length instances))
        ((eq? message 'add-class-method!)
         (let ((method-name (car args))
               (method-proc (cadr args)))
           (set! class-methods (cons (cons method-name method-proc) class-methods))))
        ((assoc message class-methods)
         => (lambda (method) (apply (cdr method) args)))
        (else (error "Unknown metaclass message:" message))))))

;; 5. 观察者模式实现
(define (make-observable)
  (let ((observers '())
        (state #f))
    (lambda (message . args)
      (cond
        ((eq? message 'add-observer!)
         (set! observers (cons (car args) observers)))
        ((eq? message 'remove-observer!)
         (set! observers (remove (car args) observers)))
        ((eq? message 'notify!)
         (for-each (lambda (observer)
                     (observer 'update state))
                   observers))
        ((eq? message 'set-state!)
         (set! state (car args))
         ((lambda (self) (self 'notify!)) 
          (lambda (msg . as) (apply (eval 'message) (eval 'args)))))
        ((eq? message 'get-state) state)
        (else (error "Unknown observable message:" message))))))

;; 6. 高级形状系统 - 实现接口
(define (make-advanced-circle color radius)
  (let ((my-color color)
        (my-radius radius))
    (lambda (message . args)
      (cond
        ;; 基本属性
        ((eq? message 'get-color) my-color)
        ((eq? message 'get-radius) my-radius)
        ((eq? message 'set-color!) (set! my-color (car args)))
        ((eq? message 'set-radius!) (set! my-radius (car args)))
        
        ;; 计算方法
        ((eq? message 'area) (* 3.14159 my-radius my-radius))
        ((eq? message 'perimeter) (* 2 3.14159 my-radius))
        
        ;; 实现 Drawable 接口
        ((eq? message 'draw) 
         (string-append "Drawing a " my-color " circle with radius " 
                       (number->string my-radius)))
        ((eq? message 'get-bounds)
         (list (- my-radius) (- my-radius) (* 2 my-radius) (* 2 my-radius)))
        
        ;; 实现 Serializable 接口
        ((eq? message 'serialize)
         (list 'circle my-color my-radius))
        ((eq? message 'deserialize)
         (let ((data (car args)))
           (set! my-color (cadr data))
           (set! my-radius (caddr data))))
        
        ;; 类型检查
        ((eq? message 'type) 'circle)
        ((eq? message 'implements?)
         (let ((interface (car args)))
           (interface 'implements? 
                     (lambda (msg . as) 
                       (apply (eval 'message) (eval 'args))))))
        
        (else (error "Unknown message:" message))))))

;; 7. 工厂模式
(define shape-factory
  (let ((shape-types (make-hash)))
    ;; 注册形状类型
    (hash-set! shape-types 'circle make-advanced-circle)
    
    (lambda (message . args)
      (cond
        ((eq? message 'create)
         (let ((type (car args))
               (params (cdr args)))
           (let ((constructor (hash-ref shape-types type #f)))
             (if constructor
                 (apply constructor params)
                 (error "Unknown shape type:" type)))))
        ((eq? message 'register!)
         (let ((type (car args))
               (constructor (cadr args)))
           (hash-set! shape-types type constructor)))
        ((eq? message 'list-types)
         (hash-keys shape-types))
        (else (error "Unknown factory message:" message))))))

;; 8. 单例模式
(define (make-singleton-class constructor)
  (let ((instance #f))
    (lambda args
      (if instance
          instance
          (begin
            (set! instance (apply constructor args))
            instance)))))

;; 数据库连接单例示例
(define database-connection
  (make-singleton-class
   (lambda (host port)
     (let ((my-host host)
           (my-port port)
           (connected? #f))
       (lambda (message . args)
         (cond
           ((eq? message 'connect!)
            (set! connected? #t)
            (printf "Connected to ~a:~a\n" my-host my-port))
           ((eq? message 'disconnect!)
            (set! connected? #f)
            (printf "Disconnected from ~a:~a\n" my-host my-port))
           ((eq? message 'is-connected?) connected?)
           ((eq? message 'get-info)
            (list my-host my-port connected?))
           (else (error "Unknown database message:" message))))))))

;; ===========================================
;; 测试高级特性
;; ===========================================

(printf "\n=== 接口系统测试 ===\n")
(define circle1 (make-advanced-circle "red" 5))
(printf "Circle implements Drawable? ~a\n" 
        (drawable-interface 'implements? circle1))
(printf "Circle implements Serializable? ~a\n" 
        (serializable-interface 'implements? circle1))
(printf "Drawing: ~a\n" (circle1 'draw))
(printf "Bounds: ~a\n" (circle1 'get-bounds))
(printf "Serialized: ~a\n" (circle1 'serialize))

(printf "\n=== 工厂模式测试 ===\n")
(define factory-circle (shape-factory 'create 'circle "blue" 3))
(printf "Factory created circle: ~a\n" (factory-circle 'draw))
(printf "Available types: ~a\n" (shape-factory 'list-types))

(printf "\n=== 单例模式测试 ===\n")
(define db1 (database-connection "localhost" 5432))
(define db2 (database-connection "remote" 3306))
(printf "DB1 and DB2 are same instance? ~a\n" (eq? db1 db2))
(db1 'connect!)
(printf "DB2 connection status: ~a\n" (db2 'is-connected?))
(printf "DB info: ~a\n" (db1 'get-info))

;; 9. 方法链式调用（Fluent Interface）
(define (make-fluent-object)
  (let ((data (make-hash)))
    (letrec ((self (lambda (message . args)
                     (cond
                       ((eq? message 'set!)
                        (hash-set! data (car args) (cadr args))
                        self)  ; 返回自身支持链式调用
                       ((eq? message 'get)
                        (hash-ref data (car args) #f))
                       ((eq? message 'clear!)
                        (hash-clear! data)
                        self)
                       ((eq? message 'show)
                        (printf "Data: ~a\n" (hash->list data))
                        self)
                       ((eq? message 'build)
                        (hash-copy data))
                       (else (error "Unknown fluent message:" message))))))
      self)))

(printf "\n=== 链式调用测试 ===\n")
(define builder (make-fluent-object))
(define result ((((builder 'set! 'name "Alice") 'set! 'age 25) 'show) 'build))
(printf "Built object: ~a\n" result)

;; 10. 装饰器模式
(define (make-decorator base-object)
  (lambda (decorator-methods)
    (lambda (message . args)
      (let ((decorator-method (assoc message decorator-methods)))
        (if decorator-method
            (apply (cdr decorator-method) base-object args)
            (apply base-object message args))))))

;; 为圆形添加日志装饰器
(define (logging-decorator)
  (list (cons 'area (lambda (base-obj)
                      (printf "Computing area...\n")
                      (let ((result (base-obj 'area)))
                        (printf "Area computed: ~a\n" result)
                        result)))
        (cons 'draw (lambda (base-obj)
                      (printf "About to draw...\n")
                      (let ((result (base-obj 'draw)))
                        (printf "Drawing completed\n")
                        result)))))

(printf "\n=== 装饰器模式测试 ===\n")
(define plain-circle (make-advanced-circle "green" 4))
(define logged-circle ((make-decorator plain-circle) (logging-decorator)))

(printf "Plain circle area: ~a\n" (plain-circle 'area))
(printf "\nLogged circle operations:\n")
(logged-circle 'area)
(logged-circle 'draw)

(printf "\n=== 高级OOP特性总结 ===\n")
(printf "1. 接口系统：定义对象必须实现的方法契约\n")
(printf "2. 多重继承：支持从多个父对象继承方法\n")
(printf "3. 访问控制：区分公共和私有方法\n")
(printf "4. 元类系统：类本身也是可操作的对象\n")
(printf "5. 设计模式：工厂、单例、装饰器、观察者等\n")
(printf "6. 链式调用：支持流畅的方法调用语法\n")
(printf "7. 动态特性：运行时修改对象行为\n")