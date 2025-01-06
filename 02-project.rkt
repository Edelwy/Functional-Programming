#lang racket

;; Podatkovni tipi.
(struct int (n) #:transparent)
(struct true () #:transparent)
(struct false () #:transparent)
(struct exception (exn) #:transparent)
(struct triggered (e) #:transparent)
(struct empty () #:transparent)
(struct .. (e1 e2) #:transparent)

;; Nadzor toka.
(struct trigger (e) #:transparent)
(struct handle (e1 e2 e3) #:transparent)
(struct if-then-else (condition e1 e2) #:transparent)
(struct ?int (e) #:transparent)
(struct ?bool (e) #:transparent)
(struct ?.. (e) #:transparent)
(struct ?seq (e) #:transparent)
(struct ?empty (e) #:transparent)
(struct ?exception (e) #:transparent)
(struct add (e1 e2) #:transparent)
(struct mul (e1 e2) #:transparent)
(struct ?leq (e1 e2) #:transparent)
(struct ?= (e1 e2) #:transparent)
(struct head (e) #:transparent)
(struct tail (e) #:transparent)
(struct ~ (e) #:transparent)
(struct ?all (e) #:transparent)
(struct ?any (e) #:transparent)

;; Spremenljivke.
(struct vars (s e1 e2) #:transparent)
(struct valof (s) #:transparent)

;; Funkcije.
(struct fun (name farg body) #:transparent)
(struct proc (name body) #:transparent)
(struct closure (env f) #:transparent)
(struct call (e args) #:transparent)

;; Pomozne funkcije za lazjo berljivost.
(define (bool? value) 
    (cond 
        [(false? value) #t]
        [(true? value) #t]
        [else #f]
    )
)
    
(define (holds? value)
     (cond 
        [(true? value) #t]
        [else #f]
    )
)

(define (count lst)
    (cond
        [(empty? lst) 0]             
        [(..? lst) (+ 1 (count (..-e2 lst)))] 
        [else (triggered (exception "count: wrong argument type"))]
    )
)

(define (zip lst1 lst2) 
    (cond 
        [(and (list? lst1) (list? lst2)) (map cons lst1 lst2)]
        [else (list (cons lst1 lst2))]
    )
)

;; FR interpreter funkcija.
(define (fri expr env)
    ;;(printf "Evaluating: ~a\nEnvironment: ~a\n" expr env)
    (cond
        [(int? expr) expr]                                                  ;; Ce je izraz eno samo stevilo, vrnemo stevilo.                              
        [(true? expr) expr]                                                 ;; Ce je izraz samo true, vrnemo true. 
        [(false? expr) expr]                                                ;; Ce je izraz samo false, vrnemo false.
        [(exception? expr) expr]                                            ;; Ce je izraz izjema, vrnemo izjemo.
        [(triggered? expr) expr]                                             ;; Ce je izraz prozena izjema, vrnemo prozeno izjemo.
        [(empty? expr) expr]                                                ;; Ce je izraz prazen seznam, vrnemo prazen seznam.
        [(..? expr)                                                         ;; Ce je izraz sestavljen iz vecih izrazov jih evalviramo in vrnemo 
            (let ([e1 (fri (..-e1 expr) env)]                               ;; kot seznam, razen v primeru, ko je bila med evalvacijo prozena izjema.
                  [e2 (fri (..-e2 expr) env)])
                 (cond
                    [(triggered? e1) e1]
                    [(triggered? e2) e2]
                    [else (.. e1 e2)]
                 )
            )
        ]
        [(trigger? expr)                                                    ;; Proženje izjem v primeru, da se izraz evalvira v izjemo ali pa v že proženo izjemo.
            (let ([e (fri (trigger-e expr) env)])                           ;; V default primeru vrnemo "wrong argument type" izjemo.
                 (cond
                    [(exception? e) (triggered e)]
                    [(triggered? e) e]
                    [else (triggered (exception "trigger: wrong argument type"))]
                 )
            )
        ]
        [(handle? expr)                                                    ;; Funkcionalnost try-catch bloka. Prvi izraz definira izjemo, ki jo zelimo ujeti.          
            (let ([e1 (fri (handle-e1 expr) env)]                          ;; Drugi izraz je izraz, ki ga zelimo evalvirati.
                  [e2 (fri (handle-e2 expr) env)]
                  [e3 (fri (handle-e3 expr) env)])
                 (cond
                    [(triggered? e1) e1]
                    [(not (exception? e1)) (triggered (exception "handle: wrong argument type"))]
                    [(and (triggered? e2) (equal? (triggered-e e2) e1)) e3]
                    [else e2]
                 )
            )
        ]
        [(if-then-else? expr)                                             ;; Ce condition izraz ni resnicen vrnemo evalviran drugi izraz, v nasprotnem primeru prvi.               
            (let ([condition (fri (if-then-else-condition expr) env)])
                (cond
                    [(triggered? condition) condition]
                    [(false? condition) (fri (if-then-else-e2 expr) env)]
                    [else (fri (if-then-else-e1 expr) env)]
                )
            )
        ]
        [(?int? expr)                                                     ;; Type check za int, v primeru prozene izjeme vrnemo prozeno izjemo. 
            (let ([e (fri (?int-e expr) env)])
                (cond
                    [(triggered? e) e]
                    [else (if (int? e) (true) (false))]
                )
            )
        ]
        [(?bool? expr)                                                    ;; Type check za bool, v primeru prozene izjeme vrnemo prozeno izjemo.                                                  
            (let ([e (fri (?bool-e expr) env)])
                (cond
                    [(triggered? e) e]
                    [else (if (bool? e) (true) (false))]
                )
            )
        ]
        [(?..? expr)                                                      ;; Type check za seznam, v primeru prozene izjeme vrnemo prozeno izjemo. 
            (let ([e (fri (?..-e expr) env)])
                (cond
                    [(triggered? e) e]
                    [else (if (..? e) (true) (false))]
                )
            )
        ]
        [(?seq? expr)                                                     ;; Vrne true, ce se podano zaporedje konca z empty, razen v primeru izjeme. 
            (let ([e (fri (?seq-e expr) env)])
                (cond
                    [(triggered? e) e]
                    [(empty? e) (true)]
                    [(..? e) (fri (?seq (..-e2 e)) env)]
                    [else (false)]
                )
            )
        ]
        [(?empty? expr)                                                   ;; Type check za prazen seznam, v primeru prozene izjeme vrnemo prozeno izjemo. 
            (let ([e (fri (?empty-e expr) env)])
                (cond
                    [(triggered? e) e]
                    [else (if (empty? e) (true) (false))]
                )
            )
        ]
        [(?exception? expr)                                               ;; Type check za izjemo, v primeru prozene izjeme vrnemo prozeno izjemo. 
            (let ([e (fri (?exception-e expr) env)])
                (cond
                    [(triggered? e) e]
                    [else (if (exception e) (true) (false))]
                )
            )
        ]
        [(add? expr)                                                      ;; Ce sta izraza stevili ju sestejemo, za boolean vrednosti uporabimo disjunkcijo,     
            (let ([e1 (fri (add-e1 expr) env)]                            ;; ce sta seznama jih concat-amo, v nasprotnem primeru pa prozimo izjemo.
                  [e2 (fri (add-e2 expr) env)])
                (cond
                    [(triggered? e1) e1]
                    [(triggered? e2) e2]
                    [(and (bool? e1) (bool? e2)) (if (or (true? e1) (true? e2)) (true) (false))]
                    [(and (int? e1) (int? e2)) (int (+ (int-n e1) (int-n e2)))]
                    [(and (holds? (fri (?seq e1) env)) (holds? (fri (?seq e2) env))) 
                        (if (empty? e1) e2 (.. (fri (head e1) env) (fri (add (tail e1) e2) env)))
                    ]
                    [else (triggered (exception "add: wrong argument type"))]
                )
            )
        ] 
        [(mul? expr)                                                      ;; Ce sta izraza stevili ju mnozimo, za boolean vrednosti uporabimo konjungcijo,     
            (let ([e1 (fri (mul-e1 expr) env)]                            ;; v nasprotnem primeru pa prozimo izjemo.
                  [e2 (fri (mul-e2 expr) env)])
                (cond
                    [(triggered? e1) e1]
                    [(triggered? e2) e2]
                    [(and (bool? e1) (bool? e2)) (if (and (true? e1) (true? e2)) (true) (false))]
                    [(and (int? e1) (int? e2)) (int (* (int-n e1) (int-n e2))) ]
                    [else (triggered (exception "mul: wrong argument type"))]
                )
            )
        ]
        [(?leq? expr)                                                     ;; Ce sta izraza stevili ju primerjamo, za boolean vrednosti uporabimo implikacijo,     
            (let ([e1 (fri (?leq-e1 expr) env)]                           ;; ce sta seznama primerjamo stevilo elementov, v nasprotnem primeru pa prozimo izjemo.
                  [e2 (fri (?leq-e2 expr) env)])
                (cond
                    [(triggered? e1) e1]
                    [(triggered? e2) e2]
                    [(and (bool? e1) (bool? e2)) (if (and (true? e1) (false? e2)) (false) (true))]
                    [(and (int? e1) (int? e2)) (if (<= (int-n e1) (int-n e2)) (true) (false))]
                    [(and (holds? (fri (?seq e1) env)) (holds? (fri (?seq e2) env))) 
                        (if (equal? (count e1) (count e2)) (true) (false))
                    ] 
                    [else (triggered (exception "?leq: wrong argument type"))]
                )
            )
        ]
        [(?=? expr)                                                       ;; Primerjamo ekvivalenco izrazov. 
            (let ([e1 (fri (?=-e1 expr) env)]                           
                  [e2 (fri (?=-e2 expr) env)])
                (cond
                    [(triggered? e1) e1]
                    [(triggered? e2) e2]                                                  
                    [else (if (equal? e1 e2) (true) (false))]
                )
            )
        ]
        [(head? expr)                                                     ;; Vrne prvi element zaporedja, razen v primeru, da je izraz prazno zaporedje ali napacnega tipa.
            (let ([e (fri (head-e expr) env)])
                (cond
                    [(triggered? e) e]
                    [(..? e) (..-e1 e)]
                    [(empty? e) (triggered (exception "head: empty sequence"))]
                    [else (triggered (exception "head: wrong argument type"))]
                )
            )
        ]
        [(tail? expr)                                                     ;; Vrne preostanek zaporedja, razen v primeru, da je izraz prazno zaporedje ali napacnega tipa. 
            (let ([e (fri (tail-e expr) env)])
                (cond
                    [(triggered? e) e]
                    [(..? e) (..-e2 e)]
                    [(empty? e) (triggered (exception "tail: empty sequence"))]
                    [else (triggered (exception "tail: wrong argument type"))]
                )
            )
        ]
        [(~? expr)                                                        ;; Ce je izraz stevilo ga negiramo, za boolean vrednosti uporabimo logicno negacijo,                                                       
            (let ([e (fri (~-e expr) env)])                               ;; v nasprotnem primeru pa prozimo izjemo.
                (cond
                    [(triggered? e) e]
                    [(bool? e) (if (true? e)(false) (true))]
                    [(int? e) (int (- (int-n e)))]
                    [else (triggered (exception "~: wrong argument type"))]
                )
            )
        ]
        [(?all? expr)                                                     ;; Ce vsebuje seznam vrednost false vrnemo false, v nasprotenm primeru true.
            (let ([e (fri (?all-e expr) env)])
                (cond
                    [(triggered? e) e]
                    [(not (holds? (fri (?seq e) env))) (triggered (exception "all: wrong argument type"))]
                    [(empty? e) (true)]
                    [(holds? (fri (?= (..-e1 e) (false)) env)) (false)]
                    [(fri (?all (..-e2 e)) env)]
                )
            )
        ]                   
        [(?any? expr)                                                     ;; Ce vsebuje seznam vrednost, ki ni false vrnemo true, v nasprotenm primeru false. 
            (let ([e (fri (?any-e expr) env)])
                (cond
                    [(triggered? e) e]
                    [(not (holds? (fri (?seq e) env))) (triggered (exception "any: wrong argument type"))]
                    [(empty? e) (false)]
                    [(not (holds? (fri (?= (..-e1 e) (false)) env))) (true)]
                    [(fri (?any (..-e2 e)) env)]
                )
            )
        ]
        [(vars? expr)                                                    ;; Razsiri trenutno okolje z kljuci imen spremenljivk iz 's' ter vrednostmi iz 'e1'. Evalviramo izraz 'e2'.
            (let* ([s (if (list? (vars-s expr)) (vars-s expr) (list (vars-s expr)))]
                   [e1 (if (list? (vars-e1 expr)) (map (lambda (exp) (fri exp env)) (vars-e1 expr)) (list (fri (vars-e1 expr) env)))]
                   [errors (memf (lambda (exp) (triggered? exp)) e1)]
                   [envlst (zip s e1)])
                (cond
                    [errors (list-ref errors 0)]
                    [(check-duplicates s) (triggered (exception "vars: duplicate identifier"))]
                    [else (fri (vars-e2 expr) (append envlst env))]
                )
            )
        ]
        [(valof? expr)                                                    ;; Najde prvi kljuc v seznamu parov, ki jih imamo v environmentu, in vrne vrednost tega para.
            (let ([s (assoc (valof-s expr) env)])                         ;; Ce spremenljivka v okolju ne obstaja vrne prozeno izjemo.
                (if s (fri (cdr s) env) (triggered (exception "valof: undefined variable")))
            )
        ]
        [(fun? expr)                                                      ;; Evalvira funkcijo v funkcijsko ovojnico.
            (cond                                                         ;; Klic se zgodi sele, ko interpreter dobi 'call' command.
                [(triggered? (fun-body expr)) (fun-body expr)]
                [(check-duplicates (fun-farg expr)) (triggered (exception "fun: duplicate argument identifier"))]
                [else (closure env expr)]
            )
        ]
        [(proc? expr)                                                     ;; Vrne proceduro. Klic se zgodi sele, ko interpreter dobi 'call' command.
            (cond
                [(triggered? (proc-body expr)) (proc-body expr)]
                [else expr]
            )
        ]                                               
        [(closure? expr) expr]                                            ;; Vrne closure. Klic se zgodi sele, ko interpreter dobi 'call' command.
        [(call? expr)                                                     ;; Poklice ovojnico ali proceduro, funkcije imajo podane argumente procedure pa ne. 
            (let ([e (fri (call-e expr) env)]
                  [args (call-args expr)])
                (cond
                    [(triggered? e) e]
                    [(proc? e) (fri (proc-body e) (cons (cons (proc-name e) e) env))]
                    [(closure? e) 
                        (let* ([name (fun-name (closure-f e))]
                              [fargs (fun-farg (closure-f e))]
                              [body (fun-body (closure-f e))]
                              [newenv (zip fargs (map (lambda (exp) (fri exp env)) args))])
                            (cond
                                [(not (equal? (length args) (length fargs))) (triggered (exception "call: arity mismatch"))]
                                [else (fri body (append newenv (zip name (closure-f e)) (closure-env e)))]
                            )
                        )
                    ]
                    [else (triggered (exception "call: wrong argument type"))]
                )
            )
        ]
        [(triggered (exception "syntax not supported")) ]               
    )
)

;; Macros.
(define (greater e1 e2) 
    (~(?leq e1 e2))
)

(define (rev seq) (call 
    (fun "rev-macro" (list "seq")
        (if-then-else 
            (?empty (valof "seq"))
            (empty)
            (add (call (valof "rev-macro") (list (tail (valof "seq")))) (.. (head (valof "seq")) (empty)))
        ) 
    )
    (list seq))
)

(define (remainder e1) (call
    (fun "remainder-macro" (list "e1")
        (if-then-else 
            (?leq (valof "e1") (int 1))
            (valof "e1")
            (call (valof "remainder-macro") (list (add (valof "e1") (int -2))))
        )
    )
    (list e1))
)


(define (divisor e1) (call
    (fun "divisor-macro" (list "e1")
        (if-then-else 
            (?leq (valof "e1") (int 1))
            (int 0)
            (add (int 1) (call (valof "divisor-macro") (list (add (valof "e1") (int -2)))))
        )
    )
    (list e1))
)
 
(define (binary e1) (rev (call 
    (fun "binary-macro" (list "e1")
        (if-then-else 
            (?int (valof "e1"))
            (if-then-else
                (?= (divisor (valof "e1")) (int 0))
                (empty)
                (.. (remainder (valof "e1")) (call (valof "binary-macro") (list (divisor (valof "e1")))))
            )
            (trigger (exception "binary: expression not of int type"))
        ) 
    )
    (list e1)))
)

(define (mapping f seq) (call
    (fun "mapping-macro" (list "f" "seq")
        (if-then-else 
            (?empty (valof "seq"))
            (empty)
            (.. (call (valof "f") (list (head (valof "seq")))) (call (valof "mapping-macro") (list (valof "f") (tail (valof "seq")))))
        )
    )
    (list f seq))
)

(define (filtering f seq) (call
    (fun "filtering-macro" (list "f" "seq")
        (if-then-else 
            (?empty (valof "seq"))
            (empty)
            (if-then-else
                (call (valof "f") (list (head (valof "seq"))))
                (add (.. (head (valof "seq")) (empty)) (call (valof "filtering-macro") (list (valof "f") (tail (valof "seq")))))
                (call (valof "filtering-macro") (list (valof "f") (tail (valof "seq"))))
            )
        )
    )
    (list f seq))
)

(define (folding f acc seq) (call
    (fun "folding-macro" (list "f" "acc" "seq")
        (if-then-else 
            (?empty (valof "seq"))
            acc
            (call (valof "f") (list (head (valof "seq")) (call (valof "folding-macro") (list (valof "f") (valof "acc") (tail (valof "seq"))))))
        )
    )
    (list f acc seq))
)


