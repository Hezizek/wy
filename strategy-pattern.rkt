#lang racket

;; 策略模式实现
(define (make-sorting-context)
  (let ((strategy #f))
    (lambda (message . args)
      (cond
        ((eq? message 'set-strategy!) 
         (set! strategy (car args)))
        ((eq? message 'sort)
         (if strategy
             (strategy 'execute (car args))
             (error "No sorting strategy set")))
        (else (error "Unknown context message:" message))))))

;; 冒泡排序策略
(define bubble-sort-strategy
  (lambda (message . args)
    (cond
      ((eq? message 'execute)
       (let ((lst (car args)))
         (printf "Using bubble sort\n")
         (sort lst <)))
      (else (error "Unknown strategy message:" message)))))

;; 快速排序策略  
(define quick-sort-strategy
  (lambda (message . args)
    (cond
      ((eq? message 'execute)
       (let ((lst (car args)))
         (printf "Using quick sort\n")
         (sort lst <)))
      (else (error "Unknown strategy message:" message)))))

;; 测试
(define sorter (make-sorting-context))
(sorter 'set-strategy! bubble-sort-strategy)
(printf "Result: ~a\n" (sorter 'sort '(3 1 4 1 5 9 2 6)))