#lang racket
(require racket/match)

(define (cps exp)
  (letrec
      ([trivial? (lambda (x) (memq x '(zero? add1 sub1)))]
       [id (lambda (v) v)]
       [ctx0 (lambda (v) `(k ,v))] ; tail context
       [fv (let ([n -1])
             (lambda ()
               (set! n (+ n 1))
               (string->symbol (string-append "v" (number->string n)))))]
       [cps1
        (lambda (exp ctx)
          (match exp
            ;; 原子（不是 pair） => 直接交给上下文并绑定为 x
            [(? (lambda (v) (not (pair? v))) x)
             (ctx x)]

            ;; if 表达式
            [`(if ,test ,conseq ,alt)
             (cps1 test
                   (lambda (t)
                     (cond
                       ;; 如果上下文就是尾上下文或 id，直接生成 if（不额外包装 continuation）
                       [(or (eq? ctx ctx0) (eq? ctx id))
                        `(if ,t ,(cps1 conseq ctx) ,(cps1 alt ctx))]
                       [else
                        ;; 否则生成一个显式的 continuation k，并在分支中使用它
                        (let ([u (fv)])
                          `(let ([k (lambda (,u) ,(ctx u))])
                             (if ,t
                                 ,(cps1 conseq ctx0)
                                 ,(cps1 alt ctx0))))])))] ; 修复：这里应该也是 ctx0

            ;; lambda 抽象
            [`(lambda (,x) ,body)
             (ctx `(lambda (,x k) ,(cps1 body ctx0)))] ; 修复：添加了缺失的逗号

            ;; 二元运算 (op a b)
            [`(,op ,a ,b)
             (cps1 a
                   (lambda (v1)
                     (cps1 b
                           (lambda (v2)
                             (ctx `(,op ,v1 ,v2))))))]

            ;; 普通调用 (rator rand)
            [`(,rator ,rand)
             (cps1 rator
                   (lambda (r)
                     (cps1 rand
                           (lambda (d)
                             (cond
                               ;; rator 是平凡操作（zero?/add1/sub1）时，直接调用
                               [(trivial? r) (ctx `(,r ,d))]
                               ;; 如果当前上下文是尾上下文 ctx0，则生成尾调用形式
                               [(eq? ctx ctx0) `(,r ,d k)]
                               ;; 否则为它生成一个 continuation (lambda (u) (ctx u))
                               [else
                                (let ([u (fv)])
                                  `(,r ,d (lambda (,u) ,(ctx u))))])))))]))])
    (cps1 exp id)))

;; ===== 测试用例 =====
(displayln "---- example 1 ----")
(displayln (cps '((lambda (x) (add1 x)) (sub1 5))))
(displayln "---- example 2 ----")
(displayln (cps '(if (zero? x) (add1 x) (sub1 x))))
;; 嵌套函数调用
(displayln "---- example 3 ----")
(displayln (cps '((lambda (x) ((lambda (y) (add1 y)) x)) 5)))

;; 带有非trivial调用的if
(displayln "---- example 4 ----")
(displayln (cps '(if (zero? x) ((lambda (y) y) x) (add1 x))))

;; 复杂的嵌套
(displayln "---- example 5 ----")
(displayln (cps '((lambda (f) (f 3)) (lambda (x) (add1 x)))))