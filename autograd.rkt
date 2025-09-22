#lang racket

;; ===== 数据结构 =====
(struct Const (val) #:transparent)    ; 常量
(struct Var (name) #:transparent)     ; 变量
(struct Add (a b) #:transparent)      ; 加法
(struct Mul (a b) #:transparent)      ; 乘法
(struct Fun (param body) #:transparent) ; λ 抽象
(struct App (f arg) #:transparent)    ; 函数应用

;; ===== 环境查找 =====
(define (lookup env x)
  (cond [(null? env) (error "unbound variable" x)]
        [(eq? (caar env) x) (cdar env)]
        [else (lookup (cdr env) x)]))

;; ===== 解释器：求值表达式 =====
(define (eval-expr expr env)
  (match expr
    [(Const v) v]
    [(Var x) (lookup env x)]
    [(Add a b) (+ (eval-expr a env) (eval-expr b env))]
    [(Mul a b) (* (eval-expr a env) (eval-expr b env))]
    [(Fun x body) (lambda (v) (eval-expr body (cons (cons x v) env)))]
    [(App f a) ((eval-expr f env) (eval-expr a env))]))

;; ===== 自动微分：生成梯度表达式 =====
(define (diff expr var)
  (match expr
    [(Const v) (Const 0)]
    [(Var x) (if (eq? x var) (Const 1) (Const 0))]
    [(Add a b) (Add (diff a var) (diff b var))]
    [(Mul a b) (Add (Mul (diff a var) b)
                    (Mul a (diff b var)))]
    [(Fun x body) (Fun x (diff body var))]
    [(App f a) (error "diff of application not implemented fully")]))

;; ===== 工具函数 =====
(define (pretty expr)
  (match expr
    [(Const v) (number->string v)]
    [(Var x) (symbol->string x)]
    [(Add a b) (string-append "(" (pretty a) " + " (pretty b) ")")]
    [(Mul a b) (string-append "(" (pretty a) " * " (pretty b) ")")]
    [(Fun x body) (string-append "(λ" (symbol->string x) ". " (pretty body) ")")]
    [(App f a) (string-append "(" (pretty f) " " (pretty a) ")")]))

;; ===== 示例 =====
;; 表达式 f(x) = x * x + 2
(define f-expr (Add (Mul (Var 'x) (Var 'x)) (Const 2)))

;; f(3) = 11
(displayln (eval-expr f-expr '((x . 3))))

;; df/dx = 2x
(define df (diff f-expr 'x))
(displayln (pretty df))
(displayln (eval-expr df '((x . 3)))) ; df/dx at x=3 = 6

