;;; predicates.scm -- various useful predicates, collected from other places
;;;
;;; author :  Sandra Loosemore
;;; date   :  19 Mar 1992
;;;


;;; Some predicates on patterns (used by CFN)

(define-integrable (var-or-wildcard-pat? p)
  (or (is-type? 'wildcard-pat p)
      (is-type? 'var-pat p)))

(define-integrable (irrefutable-pat? p)
  (or (is-type? 'wildcard-pat p)
      (is-type? 'var-pat p)
      (is-type? 'irr-pat p)))

