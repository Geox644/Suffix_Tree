#lang racket
(require "suffix-tree.rkt")

(provide (all-defined-out))

; TODO 2
; Implementați o funcție care primește două cuvinte (liste
; de caractere) w1 și w2 și calculează cel mai lung prefix
; comun al acestora, împreună cu restul celor două cuvinte
; după eliminarea prefixului comun.
; ex:
; (longest-common-prefix '(#\w #\h #\y) '(#\w #\h #\e #\n))
; => '((#\w #\h) (#\y) (#\e #\n))
; Folosiți recursivitate pe coadă.

(define (longest-common-prefix w1 w2)
  (helper '() w1 w2))

(define (helper prefix wrd1 wrd2)
  (cond
    ((or (null? wrd1) (null? wrd2))
     (list (reverse prefix) wrd1 wrd2))
    ((equal? (car wrd1) (car wrd2))
     (helper (cons (car wrd1) prefix) (cdr wrd1) (cdr wrd2)))
    (else
     (list (reverse prefix) wrd1 wrd2))))


; TODO 3
; Implementați recursiv o funcție care primește o listă nevidă 
; de cuvinte care încep cu același caracter și calculează cel 
; mai lung prefix comun al acestora.
; Opriți căutarea (parcurgerea) în momentul în care aveți garanția 
; că prefixul comun curent este prefixul comun final.

(define (longest-common-prefix-of-list words)
   (prefix-list-helper (car words) (cdr words)))

(define (prefix-list-helper prefix words)
  (cond
    ((null? words) prefix)
    (else
     (prefix-list-helper (car (longest-common-prefix (car words) prefix)) (cdr words)))))



;; Următoarele două funcții sunt utile căutării unui șablon
;; (pattern) într-un text cu ajutorul arborelui de sufixe.
;; Ideea de căutare este următoarea:
;; - dacă șablonul există în text, atunci există un sufix care
;;   începe cu acest șablon, deci există o cale care începe din
;;   rădăcina arborelui care se potrivește cu șablonul
;; - vom căuta ramura a cărei etichetă începe cu prima literă
;;   din șablon
;; - dacă nu găsim această ramură, șablonul nu apare în text
;; - dacă șablonul este conținut integral în eticheta ramurii,
;;   atunci el apare în text
;; - dacă șablonul se potrivește cu eticheta dar nu este conținut
;;   în ea (de exemplu șablonul "nana$" se potrivește cu eticheta
;;   "na"), atunci continuăm căutarea în subarborele ramurii
;; - dacă șablonul nu se potrivește cu eticheta (de exemplu
;;   șablonul "numai" nu se potrivește cu eticheta "na"), atunci
;;   el nu apare în text (altfel, eticheta ar fi fost "n", nu
;;   "na", pentru că eticheta este cel mai lung prefix comun al
;;   sufixelor din subarborele său)


; TODO 4
; Implementați funcția match-pattern-with-label care primește un
; arbore de sufixe și un șablon nevid și realizează un singur pas 
; din procesul prezentat mai sus - identifică ramura arborelui a
; cărei etichetă începe cu prima literă din șablon, apoi
; determină cât de bine se potrivește șablonul cu eticheta,
; întorcând ca rezultat:
; - true, dacă șablonul este conținut integral în etichetă
; - lista (etichetă, nou pattern, subarbore), dacă șablonul se
;   potrivește cu eticheta dar nu este conținut în ea
;   (ex: ("na", "na$", subarborele de sub eticheta "na")
;   pentru șablonul inițial "nana$" și eticheta "na")
; - lista (false, cel mai lung prefix comun între etichetă și
;   șablon), dacă șablonul nu s-a potrivit cu eticheta sau nu
;   s-a găsit din start o etichetă care începe cu litera dorită
;   (ex1: (false, "n") pentru șablonul "numai" și eticheta "na")
;   (ex2: (false, "") pentru etichetă negăsită)
; Obs: deși exemplele folosesc stringuri pentru claritate, vă
; reamintim că în realitate lucrăm cu liste de caractere.
;; Funcție auxiliară pentru match-pattern-with-label



(define (match-pattern-with-label st pattern)
  (let* ((first-char (car pattern))
         (branch (get-ch-branch st first-char))) ;  ramura unui ST a carei eticheta incepe cu caracterul ch
    (if branch
        (let* ((label (get-branch-label branch))
               (prefix (car (longest-common-prefix label pattern)))
               (match-result (if (equal? prefix pattern)
                                 #t
                                 (if (< (length prefix) (length label))
                                     (list #f prefix)
                                     (list label (drop pattern (length label)) (get-branch-subtree branch))))))
                                     ; returneaza o lista ce contine eticheta, restul sablonului si subarborele ramurii
          match-result)
        (list #f '()))))



; TODO 5
; Implementați funcția st-has-pattern? care primește un
; arbore de sufixe și un șablon și întoarce true dacă șablonul
; apare în arbore, respectiv false în caz contrar.
(define (st-has-pattern? st pattern)
  (cond ((equal? (match-pattern-with-label st pattern) #t) #t)
        ((equal? (car (match-pattern-with-label st pattern)) #f) #f)
        (else (st-has-pattern? (third (match-pattern-with-label st pattern))
                                (second (match-pattern-with-label st pattern))))))

  