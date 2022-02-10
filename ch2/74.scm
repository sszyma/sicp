(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))


(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
	     (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record
		   (assoc key-2 (cdr subtable))))
	      (if record (cdr record) false))
	    false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
	     (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record
		   (assoc key-2 (cdr subtable))))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable
			    (cons (cons key-2 value)
				  (cdr subtable)))))
	    (set-cdr! local-table
		      (cons (list key-1 (cons key-2 value))
			    (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (error
	   "No method for these types: APPLY-GENERIC"
	   (list op type-tags))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Suppose that each division's personnel file is a set of records.
;; The structure of the set varies from division to division, and so
;; each division would mantain its own package installed in the lookup
;; table with internal procedures related to the particular data
;; structure used. For this reason each set of records is tagged with
;; the identifier of the corresponding division.

;; Furthermore, each employee's records is itself a set (structured from
;; division to division) that contains information keyed under identifiers
;; such as 'address' and 'salary'. Procedures that manipulate on records are
;; also included in each division's package.


;; (a)

;; It suffices that the identifying key, independent from division,
;; is assigned to each employee. Each division's package must contain
;; a procedure, tagged 'record-retrieve, that given an id-key retrieves
;; the corresponding record.

;; ID [Set-of Records] -> Record
;; retrivies a specified employee's record from a specified personnel file, and
;; tags it with the corresponding division's type for compatibility reasons.
(define (get-record id sor)
  (attach-tag (type-tag sor)
	      ((get 'retrieve-record (type-tag sor)) id (contents sor))))
  
;; (b))

;; Each division's package must contain a selector, tagged 'salary, that
;; retrieves the salary information from the given record.

;; Tagged-Record(s) -> Salary
;; Retrieves salary information
(define (get-salary record)
  (apply-generic 'salary record))

;; (c)

;; ID [List-of [Set-of Records]] -> [Maybe Record]
;; Searches through a list of each division's records for the record of a
;; given employee. To make it quicker, we require that the 'retrieve-record procedure
;; returns #false when no record was found for a specified ID in the [Set-of Records] file.
(define (find-employee-record id los)
  (if (null? los)
      (error "NO SUCH RECORD" "FIND-EMPLOYEE-RECORD" (list id los))
      (let ((potential-result
	     ((get 'retrieve-record (type-tag (car los))) id (contents (car los)))))
	(if potential-result
	    potential-result
	    (find-employee-record id (cdr los))))))

;; (d)
;; When the Insatiable takes over new company, then to incorporate new personnel information,
;; it suffices to install in the lookup table a package corresponding to the new division.
