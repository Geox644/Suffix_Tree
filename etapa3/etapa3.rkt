#lang racket
(require "suffix-tree.rkt")
(require "etapa1.rkt")
(require "etapa2.rkt")

(provide (all-defined-out))

;; Această etapă este dedicată aplicațiilor 
;; arborelui de sufixe:
;; - găsirea unui șablon într-un text
;; - cel mai lung subșir comun a două texte
;; - găsirea unui subșir de lungime dată care se
;;   repetă în text
;; Conform convenției din etapele anterioare, un text
;; este întotdeauna reprezentat ca listă de caractere.
;; Rezultatele funcțiilor de mai jos sunt de asemenea
;; reprezentate ca liste de caractere.


; TODO 1
; Implementați funcția substring? care primește un text și
; un șablon nevid și întoarce true dacă șablonul apare în 
; text, respectiv false în caz contrar.
; Observație: ați implementat deja logica principală a
; acestei căutări în etapa 1, în funcția st-has-pattern?,
; care este un operator al tipului ST. Acum aveți toate
; uneltele necesare implementării operatorului corespunzător
; pentru tipul text (pentru că în etapa 2 ați implementat
; construcția arborelui de sufixe asociat unui text).


(define (substring? text pattern)
  (let ((st (text->cst text)))
      (st-has-pattern? st pattern)))


; TODO 2
; Implementați funcția longest-common-substring care primește
; două texte și determină cel mai lung subșir comun al
; acestora, folosind algoritmul următor:
; 1. Construiește arborele de sufixe ST1 pentru primul text.
; 2. Pentru fiecare sufix din al doilea text (de la cel mai
;    lung la cel mai scurt), găsește cea mai lungă potrivire 
;    cu sufixele din primul text, urmând căile relevante în ST1.
; 3. Rezultatul final este cel mai lung rezultat identificat
;    la pasul 2 (în caz de egalitate a lungimii, păstrăm
;    primul șir găsit).
; Folosiți named let pentru a parcurge sufixele.
; Observație: pentru sufixele din al doilea text nu dorim 
; marcajul de final $ pentru a nu crește artificial lungimea 
; șirului comun cu acest caracter.
; Hint: Revizitați funcția match-pattern-with-label (etapa 1).



(define (help st suffix)
  (let* ((ch (if (not (equal?  (car suffix) #\$)) (car suffix) suffix))
        (subtree (get-ch-branch st ch))) ; ramura carei eticheta incepe cu caracterul ch
    (if (equal? subtree #f)
        '()
        (let ((result (match-pattern-with-label st suffix)))
          (if (eq? result #t)
              (list suffix)
              (if (eq? (car result) #f) ;  Gasim un nod, dar nu este continut de sablon
                  (list (cadr result)) ; extrag sufixul partial
                  (cons (car result)
                        (help (caddr result) (cadr result)))))))))


(define (longest-common-substring text1 text2)
  (let* ((st (text->cst text1))
         (suffixes (get-suffixes text2)))
    (foldl (λ (suffix longest) ; sufixul curent din suffixes si cel mai lung subs com gasit pana acum
             (let ((check (help st suffix))) ; la fiecare pas obtin subsiruri comune si compara lungimea 
               (if (> (length (apply append check)) (length longest))
                   (apply append check)
                   longest)))
           '()
           suffixes)))


; TODO 3
; Implementați funcția repeated-substring-of-given-length
; care primește un text și un număr natural len și
; parcurge arborele de sufixe al textului până găsește un
; subșir de lungime len care se repetă în text.
; Dacă acest subșir nu există, funcția întoarce false.
; Obs: din felul în care este construit arborele de sufixe
; (pe baza alfabetului sortat), rezultatul va fi primul 
; asemenea subșir din punct de vedere alfabetic.
; Ideea este următoarea: orice cale în arborele de sufixe
; compact care se termină cu un nod intern (un nod care 
; are copii, nu este o frunză) reprezintă un subșir care
; se repetă, pentru că orice asemenea cale reprezintă un
; prefix comun pentru două sau mai multe sufixe ale textului.
; Folosiți interfața definită în fișierul suffix-tree
; atunci când manipulați arborele.

(define (explore-st st prefix len)
  (cond ((equal? (length prefix) len)
         prefix)
        ((st-empty? st) #f)
        ((> (length prefix) len)
         (take prefix len)) ; primele len caractere ale prefixului
        (else
         (let ((branch (if (equal? (first-branch st) '(#\$))
                           (first-branch (other-branches st))
                           (first-branch st))))
           (if (st-empty? branch)
               (explore-st (other-branches st) prefix len)
               (if (equal? (last (append prefix (get-branch-label branch))) #\$)
    (explore-st (other-branches st) prefix len)
    (let ((result (explore-st (get-branch-subtree branch) (append prefix (get-branch-label branch)) len)))
      (if result ; daca s a gasit un subsir repetat de lungime len
          result
          (explore-st (other-branches st) prefix len)))))))))

(define (repeated-substring-of-given-length text len)
  (let* ((st (text->cst text)))
    (explore-st st '() len)))
  

