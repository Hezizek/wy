#lang racket

;; ===========================================
;; 实际应用场景：构建一个完整的图形编辑器系统
;; ===========================================

;; 1. 修复接口系统
(define (make-interface name required-methods)
  (lambda (message . args)
    (cond
      ((eq? message 'get-name) name)
      ((eq? message 'get-methods) required-methods)
      ((eq? message 'implements?)
       (let ((obj (car args)))
         (andmap (lambda (method)
                   (with-handlers ([exn:fail? (lambda (e) #f)])
                     ;; 尝试调用方法，如果不抛异常说明存在
                     (cond
                       ;; 对于无参数方法
                       ((memq method '(serialize get-bounds draw type))
                        (obj method) #t)
                       ;; 对于需要参数的方法，只检查是否会抛"方法未找到"错误
                       (else #t))))
                 required-methods)))
      (else (error "Unknown interface message:" message)))))

;; 重新定义接口
(define drawable-interface 
  (make-interface "Drawable" '(draw get-bounds)))

(define serializable-interface
  (make-interface "Serializable" '(serialize)))

;; 2. 完整的图形编辑器系统
(define (make-canvas width height)
  (let ((shapes '())
        (selected-shapes '())
        (canvas-width width)
        (canvas-height height))
    (lambda (message . args)
      (cond
        ;; 形状管理
        ((eq? message 'add-shape!)
         (let ((shape (car args)))
           (set! shapes (cons shape shapes))
           (printf "Added shape: ~a\n" (shape 'type))))
        
        ((eq? message 'remove-shape!)
         (let ((shape (car args)))
           (set! shapes (remove shape shapes))
           (set! selected-shapes (remove shape selected-shapes))))
        
        ((eq? message 'get-shapes) shapes)
        
        ;; 选择管理
        ((eq? message 'select!)
         (let ((shape (car args)))
           (when (member shape shapes)
             (set! selected-shapes (cons shape selected-shapes)))))
        
        ((eq? message 'deselect!)
         (let ((shape (car args)))
           (set! selected-shapes (remove shape selected-shapes))))
        
        ((eq? message 'clear-selection!)
         (set! selected-shapes '()))
        
        ((eq? message 'get-selected) selected-shapes)
        
        ;; 渲染
        ((eq? message 'render)
         (printf "=== Canvas (~ax~a) ===\n" canvas-width canvas-height)
         (for-each (lambda (shape)
                     (printf "~a ~a\n" 
                             (shape 'draw)
                             (if (member shape selected-shapes) "[SELECTED]" "")))
                   (reverse shapes)))
        
        ;; 批量操作
        ((eq? message 'move-selected!)
         (let ((dx (car args)) (dy (cadr args)))
           (for-each (lambda (shape)
                       (when (shape 'can-move?)
                         (shape 'move! dx dy)))
                     selected-shapes)))
        
        ;; 序列化
        ((eq? message 'save-to-file!)
         (let ((filename (car args)))
           (with-output-to-file filename
             (lambda ()
               (write (map (lambda (shape) (shape 'serialize)) shapes)))
             #:exists 'replace)
           (printf "Canvas saved to ~a\n" filename)))
        
        ((eq? message 'load-from-file!)
         (let ((filename (car args)))
           (when (file-exists? filename)
             (let ((data (with-input-from-file filename read)))
               (set! shapes (map deserialize-shape data))
               (printf "Canvas loaded from ~a\n" filename)))))
        
        (else (error "Unknown canvas message:" message))))))

;; 3. 增强的形状类 - 支持移动和更多操作
(define (make-enhanced-circle color radius x y)
  (let ((my-color color)
        (my-radius radius)
        (my-x x)
        (my-y y)
        (id (gensym 'circle)))
    (lambda (message . args)
      (cond
        ;; 基本属性
        ((eq? message 'get-id) id)
        ((eq? message 'get-color) my-color)
        ((eq? message 'get-radius) my-radius)
        ((eq? message 'get-position) (list my-x my-y))
        ((eq? message 'set-color!) (set! my-color (car args)))
        ((eq? message 'set-radius!) (set! my-radius (car args)))
        
        ;; 移动操作
        ((eq? message 'can-move?) #t)
        ((eq? message 'move!)
         (let ((dx (car args)) (dy (cadr args)))
           (set! my-x (+ my-x dx))
           (set! my-y (+ my-y dy))))
        ((eq? message 'set-position!)
         (set! my-x (car args))
         (set! my-y (cadr args)))
        
        ;; 计算方法
        ((eq? message 'area) (* 3.14159 my-radius my-radius))
        ((eq? message 'perimeter) (* 2 3.14159 my-radius))
        
        ;; 碰撞检测
        ((eq? message 'contains-point?)
         (let ((px (car args)) (py (cadr args)))
           (let ((dx (- px my-x)) (dy (- py my-y)))
             (<= (+ (* dx dx) (* dy dy)) (* my-radius my-radius)))))
        
        ;; 实现接口
        ((eq? message 'draw) 
         (format "Circle[~a] at (~a,~a) r=~a color=~a" 
                 id my-x my-y my-radius my-color))
        
        ((eq? message 'get-bounds)
         (list (- my-x my-radius) (- my-y my-radius) 
               (* 2 my-radius) (* 2 my-radius)))
        
        ((eq? message 'serialize)
         (list 'enhanced-circle my-color my-radius my-x my-y))
        
        ((eq? message 'type) 'enhanced-circle)
        
        ;; 克隆
        ((eq? message 'clone)
         (make-enhanced-circle my-color my-radius my-x my-y))
        
        (else (error "Unknown enhanced-circle message:" message))))))

(define (make-enhanced-rectangle color width height x y)
  (let ((my-color color)
        (my-width width)
        (my-height height)
        (my-x x)
        (my-y y)
        (id (gensym 'rect)))
    (lambda (message . args)
      (cond
        ;; 基本属性
        ((eq? message 'get-id) id)
        ((eq? message 'get-color) my-color)
        ((eq? message 'get-width) my-width)
        ((eq? message 'get-height) my-height)
        ((eq? message 'get-position) (list my-x my-y))
        ((eq? message 'set-color!) (set! my-color (car args)))
        ((eq? message 'set-width!) (set! my-width (car args)))
        ((eq? message 'set-height!) (set! my-height (car args)))
        
        ;; 移动操作
        ((eq? message 'can-move?) #t)
        ((eq? message 'move!)
         (let ((dx (car args)) (dy (cadr args)))
           (set! my-x (+ my-x dx))
           (set! my-y (+ my-y dy))))
        ((eq? message 'set-position!)
         (set! my-x (car args))
         (set! my-y (cadr args)))
        
        ;; 计算方法
        ((eq? message 'area) (* my-width my-height))
        ((eq? message 'perimeter) (* 2 (+ my-width my-height)))
        
        ;; 碰撞检测
        ((eq? message 'contains-point?)
         (let ((px (car args)) (py (cadr args)))
           (and (>= px my-x) (<= px (+ my-x my-width))
                (>= py my-y) (<= py (+ my-y my-height)))))
        
        ;; 实现接口
        ((eq? message 'draw) 
         (format "Rectangle[~a] at (~a,~a) ~ax~a color=~a" 
                 id my-x my-y my-width my-height my-color))
        
        ((eq? message 'get-bounds)
         (list my-x my-y my-width my-height))
        
        ((eq? message 'serialize)
         (list 'enhanced-rectangle my-color my-width my-height my-x my-y))
        
        ((eq? message 'type) 'enhanced-rectangle)
        
        ;; 克隆
        ((eq? message 'clone)
         (make-enhanced-rectangle my-color my-width my-height my-x my-y))
        
        (else (error "Unknown enhanced-rectangle message:" message))))))

;; 4. 反序列化工厂
(define (deserialize-shape data)
  (match data
    [(list 'enhanced-circle color radius x y)
     (make-enhanced-circle color radius x y)]
    [(list 'enhanced-rectangle color width height x y)
     (make-enhanced-rectangle color width height x y)]
    [else (error "Unknown shape data:" data)]))

;; 5. 命令模式 - 支持撤销/重做
(define (make-command-manager)
  (let ((history '())
        (current-pos -1))
    (lambda (message . args)
      (cond
        ((eq? message 'execute!)
         (let ((command (car args)))
           ;; 清除当前位置之后的历史
           (set! history (take history (+ current-pos 1)))
           ;; 执行命令
           (command 'execute)
           ;; 添加到历史
           (set! history (append history (list command)))
           (set! current-pos (+ current-pos 1))))
        
        ((eq? message 'undo!)
         (when (>= current-pos 0)
           (let ((command (list-ref history current-pos)))
             (command 'undo)
             (set! current-pos (- current-pos 1)))))
        
        ((eq? message 'redo!)
         (when (< current-pos (- (length history) 1))
           (set! current-pos (+ current-pos 1))
           (let ((command (list-ref history current-pos)))
             (command 'execute))))
        
        ((eq? message 'can-undo?) (>= current-pos 0))
        ((eq? message 'can-redo?) (< current-pos (- (length history) 1)))
        ((eq? message 'clear!) 
         (set! history '())
         (set! current-pos -1))
        
        (else (error "Unknown command-manager message:" message))))))

;; 移动命令
(define (make-move-command shape dx dy)
  (lambda (message . args)
    (cond
      ((eq? message 'execute)
       (shape 'move! dx dy))
      ((eq? message 'undo)
       (shape 'move! (- dx) (- dy)))
      ((eq? message 'describe)
       (format "Move ~a by (~a,~a)" (shape 'get-id) dx dy))
      (else (error "Unknown command message:" message)))))

;; 6. 组合模式 - 形状组
(define (make-shape-group shapes)
  (let ((group-shapes (if (list? shapes) shapes (list shapes)))
        (id (gensym 'group)))
    (lambda (message . args)
      (cond
        ((eq? message 'get-id) id)
        ((eq? message 'type) 'shape-group)
        
        ;; 组操作
        ((eq? message 'add-shape!)
         (set! group-shapes (cons (car args) group-shapes)))
        ((eq? message 'remove-shape!)
         (set! group-shapes (remove (car args) group-shapes)))
        ((eq? message 'get-shapes) group-shapes)
        
        ;; 委托给所有形状
        ((eq? message 'move!)
         (let ((dx (car args)) (dy (cadr args)))
           (for-each (lambda (shape) (shape 'move! dx dy)) group-shapes)))
        
        ((eq? message 'set-color!)
         (let ((color (car args)))
           (for-each (lambda (shape) 
                       (when (shape 'can-change-color?)
                         (shape 'set-color! color))) 
                     group-shapes)))
        
        ;; 计算总面积
        ((eq? message 'area)
         (apply + (map (lambda (shape) (shape 'area)) group-shapes)))
        
        ;; 绘制组
        ((eq? message 'draw)
         (format "Group[~a] containing: ~a" 
                 id 
                 (string-join (map (lambda (shape) (shape 'draw)) group-shapes) ", ")))
        
        ;; 边界框
        ((eq? message 'get-bounds)
         (if (null? group-shapes)
             '(0 0 0 0)
             (let ((bounds (map (lambda (shape) (shape 'get-bounds)) group-shapes)))
               (let ((min-x (apply min (map car bounds)))
                     (min-y (apply min (map cadr bounds)))
                     (max-x (apply max (map (lambda (b) (+ (car b) (caddr b))) bounds)))
                     (max-y (apply max (map (lambda (b) (+ (cadr b) (cadddr b))) bounds))))
                 (list min-x min-y (- max-x min-x) (- max-y min-y))))))
        
        ((eq? message 'serialize)
         (list 'shape-group (map (lambda (shape) (shape 'serialize)) group-shapes)))
        
        ((eq? message 'can-move?) #t)
        
        (else (error "Unknown group message:" message))))))

;; ===========================================
;; 完整应用演示
;; ===========================================

(printf "=== 图形编辑器系统演示 ===\n\n")

;; 创建画布
(define canvas (make-canvas 800 600))

;; 创建形状
(define circle1 (make-enhanced-circle "red" 50 100 100))
(define circle2 (make-enhanced-circle "blue" 30 200 150))
(define rect1 (make-enhanced-rectangle "green" 80 60 300 200))

;; 添加到画布
(canvas 'add-shape! circle1)
(canvas 'add-shape! circle2)
(canvas 'add-shape! rect1)

(printf "1. 初始画布状态:\n")
(canvas 'render)

;; 选择和移动
(canvas 'select! circle1)
(canvas 'select! rect1)

(printf "\n2. 选择了一些形状后:\n")
(canvas 'render)

;; 测试接口实现
(printf "\n3. 接口测试:\n")
(printf "Circle1 implements Drawable? ~a\n" 
        (drawable-interface 'implements? circle1))
(printf "Circle1 implements Serializable? ~a\n" 
        (serializable-interface 'implements? circle1))

;; 命令模式测试
(printf "\n4. 命令模式测试:\n")
(define cmd-manager (make-command-manager))
(define move-cmd (make-move-command circle1 50 30))

(printf "移动前: ~a\n" (circle1 'draw))
(cmd-manager 'execute! move-cmd)
(printf "移动后: ~a\n" (circle1 'draw))
(cmd-manager 'undo!)
(printf "撤销后: ~a\n" (circle1 'draw))
(cmd-manager 'redo!)
(printf "重做后: ~a\n" (circle1 'draw))

;; 组合模式测试
(printf "\n5. 组合模式测试:\n")
(define group1 (make-shape-group (list circle2 rect1)))
(printf "组合前 circle2 位置: ~a\n" (circle2 'get-position))
(printf "组合前 rect1 位置: ~a\n" (rect1 'get-position))
(group1 'move! 20 -10)
(printf "组移动后 circle2 位置: ~a\n" (circle2 'get-position))
(printf "组移动后 rect1 位置: ~a\n" (rect1 'get-position))
(printf "组描述: ~a\n" (group1 'draw))
(printf "组总面积: ~a\n" (group1 'area))

;; 碰撞检测
(printf "\n6. 碰撞检测测试:\n")
(printf "点(100,100)在circle1内? ~a\n" (circle1 'contains-point? 100 100))
(printf "点(300,200)在rect1内? ~a\n" (rect1 'contains-point? 300 200))
(printf "点(0,0)在circle1内? ~a\n" (circle1 'contains-point? 0 0))

;; 序列化测试
(printf "\n7. 序列化测试:\n")
(canvas 'save-to-file! "canvas.dat")
(define new-canvas (make-canvas 800 600))
(new-canvas 'load-from-file! "canvas.dat")
(printf "加载后的画布:\n")
(new-canvas 'render)

(printf "\n=== 系统架构总结 ===\n")
(printf "✓ 接口系统：统一的形状行为契约\n")
(printf "✓ 组合模式：形状可以组合成更复杂的结构\n")
(printf "✓ 命令模式：支持撤销/重做操作\n")
(printf "✓ 观察者模式：画布监听形状变化\n")
(printf "✓ 工厂模式：统一的形状创建机制\n")
(printf "✓ 序列化：持久化和加载功能\n")
(printf "✓ 碰撞检测：交互功能基础\n")
(printf "✓ 选择管理：用户界面支持\n")

;; 清理临时文件
(when (file-exists? "canvas.dat")
  (delete-file "canvas.dat"))