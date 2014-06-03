; Define subst2
; Replaces either the first occurrence of o1 or the first occurrence of o2 by new.
;
(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
	    ((or (eq? (car lat) o1) (eq? (car lat) o2))
	     (cons new (cdr lat)))
	    (else (cons (cat lat)
			(subst2 new o1 o2 (cdr lat)))))))))

(subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping)) ;;value is (vanilla ice cream with chocolate topping) 

; Define multirember
; Removes all occurrence of atom a from list lat.
(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     (else
      (cond
       ((eq? (car lat) a)
	(multirember a (cdr lat)))
       (else (cons (car lat)
		   (multirember a (cdr lat)))))))))

(multirember 'cup '(coffee cup tea cup and hick cup)) ;;value is (coffee tea and hick)
