; Nicholas Marino
; EECS 345
; Programming Project Part 1
; Works under R5RS or "pretty big" from DrRacket
(load "simpleParser.scm")

(define (interpret prog)
  (interpret-prog prog '(() . ()))) ; initialize the state to a cons of two empty lists.
; the state will be represented as '((x y z) . (val_x val_y val_z))
; which prints as '((x z y) val_x val_y val_z)
; uninitialized variables have value '()

(define (interpret-prog prog state) ; figures out what the next line of the program wants to do and
  (cond ((eq? (caar prog) 'var)     ; executes it. This function should probably delegate more to
         (interpret-prog (cdr prog) ; others instead of doing some of the heavy lifting itself, but
                         (update-state (cadar prog) ; I'm a classical federalist.
                                       (if (eq? (cddar prog) '()) ; checking for (var x) 
                                           '()                    ; vs. (var x val)
                                           ((if (boolean-expr? (caddar prog) state)
                                                eval-bool
                                                eval-expr) (caddar prog) state))
                                       state))) ; ^^^ these if statements are peppered in because I
        ((eq? (caar prog) '=)                   ; originally thought variables and return statements
         (if (eq? (lookup (cadar prog) state)   ; couldn't be booleans. Upon rereading what I was
                  '__LOOKUP_ERROR)              ; supposed to do this seemed like the best solution.
             '__LOOKUP_ERROR ; <-- Apparently without this error propagation the interpreter will
             (interpret-prog (cdr prog) ; not error when assigning a variable before declaring.
                             (update-state (cadar prog)
                                           ((if (boolean-expr? (caddar prog) state)
                                                eval-bool
                                                eval-expr) (caddar prog) state)
                                           state))))
        ((eq? (caar prog) 'return)
         ((if (boolean-expr? (cadar prog) state)
              eval-bool
              eval-expr) (cadar prog) state))
        ((eq? (caar prog) 'if) ; eval-bool-helper returns #t or #f, so that's what we want here.
         (interpret-prog (if (eval-bool-helper (cadar prog) state) ; looks at value of conditional
                             (cons (caddar prog) (cdr prog))       ; then it puts the respective 
                             (if (eq? (cdddar prog) '())           ; statement on top of the rest
                                 (cdr prog)                        ; of the program, and discards
                                 (cons (cadddar prog) (cdr prog)))); the other.
                         state)) ; ^^^ have to check for if-then vs if-then-else
        ((eq? (caar prog) 'while)                                  
         (if (eval-bool-helper (cadar prog) state)            ; Similarly, if the while condition
             (interpret-prog (cons (caddar prog) prog) state) ; holds, put the while action on top
             (interpret-prog (cdr prog) state)))              ; of the program, followed by a copy
        (else '__ERROR))) ; how descriptive                   ; of the whole while loop.
                                                              ; if/when the while condition fails,
(define (cadddar a) (car (cdddar a))) ; apparently this one   ; the whole thing gets discarded.
                                      ; isn't built in
(define (update-state var val state) 
  (cond ((eq? (lookup var state) '__LOOKUP_ERROR) ; if lookup returns __LOOKUP_ERROR, the var
         (cons (cons var (car state))             ; isn't yet in the state.
               (cons val (cdr state))))           ; painfully inefficient, but it works.
        ((eq? var (caar state))
         (cons (car state)
               (cons val (cddr state))))
        (else (let ((newstate (update-state var val (cons (cdar state)
                                                          (cddr state)))))
                (cons (cons (caar state) (car newstate))      ; cons-car hell is why they stopped
                      (cons (cadr state) (cdr newstate))))))) ; using cons cells, but I guess I
                                                              ; didn't get the message.
(define (lookup var state)
  (cond ((null? (car state)) '__LOOKUP_ERROR)
        ((eq? (caar state) var) (cadr state))
        (else (lookup var (cons (cdar state)
                                (cddr state))))))

; symbols used in boolean expressions (as opposed to arithmetic expressions)
(define bool-exprs '(** || ! != == < > <= >=))

(define (boolean-expr? expr state)
  (cond ((number? expr) #f)
        ((or (eq? expr 'true) (eq? expr 'false)) #t)
        ((not (list? expr)) (or (eq? (lookup expr state) 'true)
                                     (eq? (lookup expr state) 'false)))
        ((memq (car expr) bool-exprs) #t)
        (else #f)))

(define (eval-expr expr state)                   ; kind of like m_value
  (cond ((number? expr) expr)
        ((not (list? expr)) (lookup expr state)) ; didn't want to figure out how to identify
        ((eq? (car expr) '+)                     ; variables, so they're !(number or list)
         (+ (eval-expr (cadr expr) state) (eval-expr (caddr expr) state)))
        ((eq? (car expr) '-)
         (if (null? (cddr expr)) ; distinguish between (- x) and (- x y)
             (- 0 (eval-expr (cadr expr) state))
             (- (eval-expr (cadr expr) state) (eval-expr (caddr expr) state))))
        ((eq? (car expr) '*)
         (* (eval-expr (cadr expr) state) (eval-expr (caddr expr) state)))
        ((eq? (car expr) '/)
         (/ (eval-expr (cadr expr) state) (eval-expr (caddr expr) state)))
        ((eq? (car expr) '%)
         (modulo (eval-expr (cadr expr) state) (eval-expr (caddr expr) state)))
        (else '__EVAL_ERROR)))

(define (eval-bool expr state)     ; so originally when I did this I was returning #t and #f
  (if (eval-bool-helper expr state) ; and when I reread the instructions I found that this was
      'true                        ; was the solution of least-rework.
      'false))                     ; it is arguably of poor functional style but at some point
                                   ; programming should be about pragmatics, so I'll take it.
(define (eval-bool-helper expr state)            ; kind of like m_boolean         
  (cond ((eq? expr 'true) #t)
        ((eq? expr 'false) #f)
        ((not (list? expr)) (eq? (lookup expr state) 'true))
        ((eq? (car expr) '==) ; comparison operators
         (= (eval-expr (cadr expr) state) (eval-expr (caddr expr) state)))
        ((eq? (car expr) '!=)
         (not (= (eval-expr (cadr expr) state) (eval-expr (caddr expr) state))))
        ((eq? (car expr) '<)
         (< (eval-expr (cadr expr) state) (eval-expr (caddr expr) state)))
        ((eq? (car expr) '>)
         (> (eval-expr (cadr expr) state) (eval-expr (caddr expr) state)))
        ((eq? (car expr) '<=)
         (<= (eval-expr (cadr expr) state) (eval-expr (caddr expr) state)))
        ((eq? (car expr) '>=)
         (>= (eval-expr (cadr expr) state) (eval-expr (caddr expr) state)))
        ((eq? (car expr) '!) ; logical operators
         (not (eval-bool-helper (cadr expr) state)))
        ((eq? (car expr) '&&)
         (and (eval-bool-helper (cadr expr) state) (eval-bool-helper (caddr expr) state)))
        ((eq? (car expr) '||)
         (or (eval-bool-helper (cadr expr) state) (eval-bool-helper (caddr expr) state)))
        (else '__BOOL_ERROR)))
      