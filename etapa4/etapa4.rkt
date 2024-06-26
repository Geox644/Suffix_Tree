#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")

(provide (all-defined-out))

;; Vom prelua toate funcțiile din etapele 1-3 (exceptând
;; longest-common-substring, care nu beneficiază de 
;; reprezentarea ca flux întrucât parcurge tot arborele)
;; și le vom adapta la noua reprezentare a unui ST.
;;
;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de 
;; abstractizare), ar trebui să alterăm doar funcția 
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi 
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".


; TODO
; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în 
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).


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

; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection


(define (longest-common-prefix-of-collection words)
   (prefix-list-helper (collection-first words) (collection-rest words)))

(define (prefix-list-helper prefix words)
  (cond
    ((collection-empty? words) prefix)
    (else
     (prefix-list-helper (collection-first (longest-common-prefix (collection-first words) prefix)) (collection-rest words)))))



(define (match-pattern-with-label st pattern)
  (let* ((first-char (car pattern))
         (branch (get-ch-branch st first-char)))
    (if branch
        (let* ((label (get-branch-label branch))
               (prefix (car (longest-common-prefix label pattern)))
               (match-result (if (equal? prefix pattern)
                                 #t
                                 (if (< (length prefix) (length label))
                                     (list #f prefix)
                                     (list label (drop pattern (length label)) (get-branch-subtree branch))))))
          match-result)
        (list #f '()))))



(define (st-has-pattern? st pattern)
  (cond ((equal? (match-pattern-with-label st pattern) #t) #t)
        ((equal? (car (match-pattern-with-label st pattern)) #f) #f)
        (else (st-has-pattern? (third (match-pattern-with-label st pattern))
                                (second (match-pattern-with-label st pattern))))))

  
(define (get-suffixes text)
  (if (null? (cdr text)) '((#\$)) (collection-cons text (get-suffixes (cdr text)))))



(define (get-ch-words words ch)
  (collection-filter (λ (word) (and (not (collection-empty? word)) (equal? (collection-first word) ch))) words))


(define (ast-func suffixes)
  (let ((first-ch (collection-first (collection-first suffixes))))
    (cons (list first-ch)
          (collection-map cdr suffixes))))



(define (cst-func suffixes)
  (let* ((cst-label (longest-common-prefix-of-collection suffixes)))
    (cons cst-label
          (collection-map (λ (suffix) (drop suffix (length cst-label)))
               suffixes))))


; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)


(define (suffixes->st labeling-func suffixes alphabet)
  (define (build-tree suffixes)
    (if (collection-empty? alphabet)
        '()
        (let* ((ch-alph-suff (collection-map (λ (ch) (get-ch-words suffixes ch)) alphabet))
               (non-empty-words (collection-filter (λ (x) (not (collection-empty? x))) ch-alph-suff)))
          (collection-map (λ (ch)
                 (let* ((label-and-suffixes (labeling-func ch))
                        (branch (car label-and-suffixes))
                        (new-suffixes (cdr label-and-suffixes)))
                   (cons branch (build-tree new-suffixes))))
               non-empty-words))))
  (build-tree suffixes))


; nu uitați să convertiți alfabetul într-un flux

(define text->st
  (lambda (labeling-func)
    (lambda (text)
      (let* ((nt (append text (list #\$)))
             (suffixes (get-suffixes nt))
             (alphabet (sort (remove-duplicates nt) char<?)))
        (suffixes->st labeling-func suffixes alphabet)))))


(define text->ast
  (text->st ast-func))

(define text->cst
  (text->st cst-func))


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.

(define (substring? text pattern)
  (let ((st (text->ast text)))
      (st-has-pattern? st pattern)))



; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.


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
           (if (null? branch)
               (explore-st (other-branches st) prefix len)
               (if (equal? (last (append prefix (get-branch-label branch))) #\$)
    (explore-st (other-branches st) prefix len)
    (let ((result (explore-st (get-branch-subtree branch) (append prefix (get-branch-label branch)) len)))
      (if result
          result
          (explore-st (other-branches st) prefix len))))))))) ; continui cu urmatoarele ramuri

(define (repeated-substring-of-given-length text len)
  (let* ((st (text->cst text)))
    (explore-st st '() len)))
