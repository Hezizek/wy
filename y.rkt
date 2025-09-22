#lang racket

;; Y Combinator定义
(define Y
  (lambda (f)
    ((lambda (x) (f (lambda (v) ((x x) v))))
     (lambda (x) (f (lambda (v) ((x x) v)))))))

;; 阶乘模板
(define factorial-template
  (lambda (f)
    (lambda (n)
      (if (= n 0)
          1
          (* n (f (- n 1)))))))

;; 斐波那契模板
(define fibonacci-template
  (lambda (f)
    (lambda (n)
      (cond
        [(= n 0) 0]
        [(= n 1) 1]
        [else (+ (f (- n 1)) (f (- n 2)))]))))

;; 生成递归函数
(define factorial (Y factorial-template))
(define fibonacci (Y fibonacci-template))

;; 测试
(displayln (factorial 5))   ; 120
(displayln (fibonacci 10))  ; 55

;; 甚至可以直接使用
(displayln ((Y (lambda (f)
                 (lambda (n)
                   (if (= n 1)
                       1
                       (* n (f (- n 1))))))) 6))  ; 720
