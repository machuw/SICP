(define (average a b) (/ (+ a b) 2))
(define (close-enough? a b TOL)
  (< (abs (- a b)) TOL))
(define (negative? x) (< x 0))
(define (postive? x) (> x 0))

;; (define (search f neg-point pos-point TOL)
;;   (let ((midpoint (average neg-point pos-point)))
;;     ()))
;;     (if (close-enough? neg-point pos-point TOL) 
;;     	midpoint
;;     	(cond ((> (f midpoint) 0)
;;     	       (search f neg-point midpoint TOL))
;;     	      ((< (f midpoint) 0)
;;     	       (search f midpoint pos-point TOL))
;;     	      (else midpoint)))))

(define (search f neg-point pos-point TOL N)
  (search-iter f neg-point pos-point TOL 1 N))

(define (search-iter f neg-point pos-point TOL counter N)
  (let ((midpoint (average neg-point pos-point)))
    (cond ((close-enough? neg-point pos-point TOL) midpoint)
    	  ((> counter N) (error "Method failed after N iteration, N="N))
    	  (else
    	   (cond ((positive? (f midpoint)) (search-iter f neg-point midpoint TOL (+ counter 1) N))
    		 ((negative? (f midpoint)) (search-iter f midpoint pos-point TOL (+ counter 1) N))
    		 (else midpoint))))))

(define (half-interval-method f a b TOL N)
  (let ((a-value (f a))
	(b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
	   (search f a b TOL N))
	  ((and (negative? b-value) (positive? a-value))
	   (search f b a TOL N))
	  (else
	   (error "Values are not of opposite sign" a b)))))
