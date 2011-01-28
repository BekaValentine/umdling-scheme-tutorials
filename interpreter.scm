; Constructs ================================================================================================

(define (self-evaluating? exp)
  (or (number? exp)
      (boolean? exp)
      (tagged? 'proc exp)
      (procedure? exp)))


(define (variable? exp)
  (symbol? exp))


(define (tagged? tag exp)
  (and (pair? exp)
       (eqv? tag (car exp))))


(define (quoted? exp)
  (tagged? 'quote exp))
(define (quoted-val exp)
  (cadr exp))


(define (cond? exp)
  (tagged? 'cond exp))
(define (clauses exp)
  (cdr exp))
(define (clause-test clause)
  (car clause))
(define (clause-result clause)
  (cadr clause))


(define (lambda? exp)
  (tagged? 'lambda exp))
(define (lambda-params exp)
  (cadr exp))
(define (lambda-body exp)
  (caddr exp))


(define (let? exp)
  (tagged? 'let exp))
(define (let-pairs exp)
  (cadr exp))
(define (let-body exp)
  (caddr exp))


(define (sequence? exp)
  (tagged? 'begin exp))
(define (sequence-exps exp)
  (cdr exp))


(define (definition? exp)
  (tagged? 'define exp))
(define (definition-var exp)
  (cadr exp))
(define (definition-val exp)
  (caddr exp))


(define (application-proc exp)
  (car exp))
(define (application-args exp)
  (cdr exp))


(define (proc? exp)
  (tagged? 'proc exp))
(define (proc-params exp)
  (cadr exp))
(define (proc-body exp)
  (caddr exp))
(define (proc-env exp)
  (cadddr exp))


; Internal ================================================================================================

(define (error? exp)
  (equal? 'error exp))

(define (pairup vars vals)
  (if (null? vars)
      '()
      (cons (list (car vars) (car vals))
            (pairup (cdr vars) (cdr vals)))))
(define empty-env '(()))
(define (extend env frame)
  (cons frame env))
(define (lookup var env)
  (if (null? env)
      'error
      (let ((var-in-first (lookup-frame var (car env))))
        (if (error? var-in-first)
            (lookup var (cdr env))
            var-in-first))))
(define (lookup-frame var pairs)
  (cond ((null? pairs) 'error)
        ((eqv? var (caar pairs))
         (cadar pairs))
        (else (lookup-frame var (cdr pairs)))))
(define (add-definition! var val env)
  (set-cdr! env (append env '()))
  (set-car! env (list (list var val))))


(define (any? p xs)
  (cond ((null? xs) #f)
        ((p (car xs)) #t)
        (else (any? p (cdr xs)))))
  

; Interpreter ================================================================================================

(define (interp exp env)
  (cond ((self-evaluating? exp)   exp)
        ((variable? exp)          (lookup exp env))
        ((quoted? exp)            (quoted-val exp))
        ((cond? exp)              (interp-cond-clauses (clauses exp) env))
        ((lambda? exp)            (interp-lambda exp env))
        ((let? exp)               (interp-let exp env))
        ((sequence? exp)          (interp-sequence-exps (sequence-exps exp) env))
        ((definition? exp)        (interp-definition exp env))
        ((pair? exp)              (interp-application exp env))
        (else                     'error)))


(define (interp-cond-clauses clauses env)
  (if (null? clauses)
      'error
      (let ((res (interp (clause-test (car clauses))
                         env)))
        (cond ((error? res) 'error)
              (res (interp (clause-result (car clauses))
                           env))
              (else (interp-cond-clauses (cdr clauses)
                                         env))))))


(define (interp-lambda exp env)
  (list 'proc
        (lambda-params exp)
        (lambda-body exp)
        env))


(define (interp-let exp env)
  (let ((new-vals (map (lambda (pair)
                         (list (car pair)
                               (interp (cadr pair) env)))
                       (let-pairs exp))))
    (if (any? (lambda (pair) (error? (cadr pair)))
              new-vals)
        'error
        (interp (let-body exp)
                (extend env new-vals)))))


(define (interp-sequence-exps exps env)
  (cond ((null? exps)
         'error)
        ((null? (cdr exps))
         (interp (car exps) env))
        (else (interp (car exps) env)
              (interp-sequence-exps (cdr exps) env))))


(define (interp-definition exp env)
  (if (not (variable? (definition-var exp)))
      'error
      (let ((val (interp (definition-val exp) env)))
        (if (error? val)
            'error
            (add-definition! (definition-var exp)
                             val
                             env)))))


(define (interp-application exp env)
  (let ((proc (interp (application-proc exp) env))
        (args (interp-args (application-args exp) env)))
    (cond ((error? proc) proc)
          ((any? error? args) 'error)
          (else (apply-proc proc args)))))


(define (interp-args args env)
  (map (lambda (arg) (interp arg env))
       args))


(define (apply-proc proc args)
  (cond ((primitive? proc)
         (apply-primitive proc args))
        ((proc? proc)
	     (apply-compound proc args))
        (else 'error)))


(define (apply-primitive proc args)
  (apply proc args))


(define (apply-compound proc args)
  (if (not (eqv? (length (proc-params proc))
                 (length args)))
      'error
      (interp (proc-body proc)
              (extend (proc-env proc)
                      (pairup (proc-params proc)
                              args)))))

(define (primitive? proc)
  (member proc (list + - * / > >= < <= = odd? even? cons list car cdr pair? list? null? equal? display)))

(define base-env
  (list (list
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '> >)
        (list '>= >=)
        (list '< <)
        (list '<= <=)
        (list '= =)
        (list 'odd? odd?)
        (list 'even? even?)
        (list 'cons cons)
        (list 'list list)
        (list 'car car)
        (list 'cdr cdr)
        (list 'pair? pair?)
        (list 'list? list?)
        (list 'null? null?)
        (list 'equal? equal?)
        (list 'display display))))