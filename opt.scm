(import
  (scheme base)
  (scheme write)
  (scheme cyclone pretty-print)
)

    (define (udf:find-inlinable lis)
      (let loop ((l 0)
                 (candidates lis))
        ;(pretty-print `("candidate list" ,candidates))
        (if (= l (length candidates))
            candidates
            (loop 
              (length candidates) 
              (reduce-candidates candidates)))))

    ;; pair -> pair
    ;; Reduce candidate list and each candidate's call list,
    ;; rejecting those that may require CPS
    (define (reduce-candidates candidates)
      (foldl
        (lambda (c accum)
          (let ((new-call-list 
                  (reduce-call-list (cadr c) candidates)))
            (if new-call-list
                ;; Add candidate with new call list
                (cons (list (car c) new-call-list) accum)
                ;; No longer a candidate, do not add function
                accum)))
        '()
        candidates))

    ;; pair -> pair -> either pair boolean
    ;; Eliminate non-CPS calls from the list but
    ;; reject the list if there are any unknown calls, 
    ;; which are assumed to be CPS
    (define (reduce-call-list calls candidates)
      (foldl
        (lambda (call accum)
          (cond
            ;; CPS call, so we reject list
            ((not accum) #f)
            ;; non-CPS call so we don't need to track
            (else
              (let ((cndt (assoc call candidates)))
                ;(write `(debug ,call ,cndt))
                (cond
                  ;; Unknown call, assume CPS and reject the list
                  ((not cndt) #f)
                  ;; Called function does not CPS, no need to track it
                  ((null? (cadr cndt)) accum)
                  ;; Not sure yet, keep this call
                  (else (cons call accum)))))))
        '()
        calls))

(define candidates
#;((char-upcase (char-lower-case?))
  (char-downcase (char-upper-case?))
  (char-alphabetic?
    (char-lower-case? char-upper-case?))
  (char-upper-case? (char<=? char>=?))
  (char-lower-case? (char<=? char>=?))
  (char-numeric? ())
  (char-whitespace? ())
  (digit-value (char-numeric?))
  (string-upcase (string-map))
  (string-downcase (string-map))
  (string-foldcase (string-map)))
'((in-port:get-buf ())
 (in-port:get-lnum ())
 (in-port:get-cnum ())
 (in-port:read-buf! (in-port:set-buf!))
 (add-tok ())
 (get-toks (add-tok ->tok))
 (->tok (parse-atom reverse))
 (dotted? (reverse))
 (->dotted-list (->dotted-list))
 (parse/tok
   (add-tok
     ->tok
     in-port:set-buf!
     parse
     add-tok
     ->tok
     parse))
 (sign? ())
 (token-numeric?
   (k$605 k$605
          char-numeric?
          k$601
          k$601
          sign?
          char-numeric?
          char-numeric?))
 (read-block-comment
   (read-block-comment
     read-block-terminator
     get-next-char))
 (read-block-terminator
   (read-block-comment
     read-block-terminator
     get-next-char))
 (parse-literal-identifier (parse parse-li-rec))
 (parse-number
   (k$531 k$531
          hex-digit?
          k$531
          token-numeric?
          k$531
          parse-error
          in-port:get-cnum
          in-port:get-lnum
          parse
          tok->num$329
          parse-number-rec))
 (parse-number-rec
   (k$517 hex-digit?
          reverse
          in-port:set-buf!
          next$326
          k$513
          char>?
          k$511
          char>?
          k$511
          k$506
          parse-error
          in-port:get-cnum
          in-port:get-lnum
          next$326
          char-numeric?
          next$326
          next$326
          sign?
          parse-number-rec
          get-next-char))
 (hex-digit?
   (k$495 char<=? char>=? char<=? char>=?))
 (cyc-read
   (k$488 current-input-port parse reg-port)))
#;((call/cc (f))
 (Cyc-version ())
 (and (list rename$2167 rename$2167))
 (or (list list
           rename$2164
           rename$2164
           rename$2164
           rename$2164
           list
           list
           rename$2164
           rename$2164))
 (let (k$5792
       error
       k$5789
       error
       k$5785
       k$5785
       error
       k$5783
       list?
       error
       rename$2157
       rename$2157
       rename$2157
       rename$2157
       map
       map
       every
       k$5778
       k$5778
       k$5778))
 (let* (k$5731
        error
        k$5728
        error
        k$5717
        every
        k$5721
        k$5721
        k$5721
        list?
        error
        rename$2153
        rename$2153
        rename$2153))
 (letrec (append map map k$5683 map map list))
 (cond (compare$2093
        rename$2094
        k$5540
        list
        rename$2094
        list
        rename$2094
        rename$2094
        list
        rename$2094
        rename$2094
        list
        list
        list
        rename$2094
        rename$2094
        rename$2094
        list
        rename$2094
        rename$2094
        list
        rename$2094
        error
        compare$2093
        rename$2094))
 (when (k$5384 error k$5381 error))
 (unless (k$5360 error k$5357 error))
 (do (k$5280
      rename$2069
      rename$2069
      k$5280
      rename$2069
      rename$2069
      rename$2069
      rename$2069
      rename$2069
      map
      list
      rename$2069
      rename$2069
      append
      map
      k$5329
      k$5329
      error
      rename$2069
      rename$2069))
 (values (k$5163 k$5163))
 (dynamic-wind
   (after$2049 thunk$2050 before$2051))
 (call-with-port (proc$2046))
 (Cyc-bin-op (Cyc-bin-op cmp$2045))
 (Cyc-bin-op-char (Cyc-bin-op cmp$2040))
 (char=? (Cyc-bin-op-char))
 (char<? (Cyc-bin-op-char))
 (char>? (Cyc-bin-op-char))
 (char<=? (Cyc-bin-op-char))
 (char>=? (Cyc-bin-op-char))
 (string=? ())
 (string<? ())
 (string<=? ())
 (string>? ())
 (string>=? ())
 (member-helper (member-helper cmp-proc$2010))
 (member (member-helper member-helper))
 (assoc-helper (k$5061 cmp?$2004 assoc-helper))
 (assoc (assoc-helper assoc-helper))
 (foldl (foldl func$2000))
 (foldr (func$1997 foldr))
 (flush-output-port (current-output-port))
 (write-string (current-output-port))
 (write-char (current-output-port))
 (not ())
 (zero? ())
 (positive? ())
 (negative? ())
 (list ())
 (list-copy (foldr k$4934))
 (receive ())
 (car+cdr (values))
 (list-tail (list-tail zero?))
 (list-ref (list-tail))
 (reverse (foldl))
 (boolean=? (Cyc-obj=?))
 (symbol=? (Cyc-obj=?))
 (Cyc-obj=?
   (call/cc
     k$4503
     for-each
     return$1777
     k$4506
     type?$1776))
 (error (raise))
 (raise (list k$4144 r$4140))
 (raise-continuable (list k$4135 r$4131))
 (modulo (remainder))
 (odd? (modulo))
 (even? (modulo))
 (exact-integer-sqrt
   (k$4069
     k$4069
     not
     negative?
     exact?
     error
     k$4067
     values
     exact
     truncate
     sqrt))
 (exact-integer? (exact?))
 (inexact? (not exact?))
 (max (foldl k$4044 k$4044))
 (min (foldl k$4038 k$4038))
 (gcd/main (gcd/main modulo abs))
 (gcd/entry (foldl))
 (lcm/main (abs gcd/main))
 (lcm/entry (foldl))
 (denominator ())
 (numerator ())
 (quotient (truncate))
 (truncate/
   (values truncate-remainder truncate-quotient))
 (floor-quotient
   (k$3983 exact? exact? exact floor))
 (floor-remainder (floor-quotient))
 (floor/ (values floor-remainder floor-quotient))
 (square ())
 (identifier->symbol ())
 (find-tail (find-tail pred$1596))
 (find (find-tail))
 (cons-source ())
 (let*-values
   (k$3425
     k$3425
     k$3425
     cons-source
     rename$617$1383
     k$3383
     k$3383
     k$3383
     k$3383
     k$3383
     k$3383
     k$3383
     k$3383
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     rename$617$1383
     cons-source
     cons-source
     cons-source
     rename$617$1383
     k$3334
     k$3334
     k$3334
     k$3334
     k$3334
     k$3334
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     rename$617$1383
     rename$617$1383
     cons-source
     cons-source
     cons-source
     rename$617$1383
     rename$617$1383
     k$3329
     error
     k$3329
     k$3329
     k$3329))
 (let-values
   (k$3289
     k$3289
     k$3289
     k$3289
     k$3289
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     rename$675$1266
     rename$675$1266
     list?
     list?
     k$3263
     k$3263
     k$3263
     k$3263
     k$3263
     k$3263
     k$3263
     k$3263
     cons-source
     cons-source
     cons-source
     rename$675$1266
     k$3216
     k$3216
     k$3216
     k$3216
     k$3216
     k$3216
     k$3216
     k$3216
     k$3216
     k$3216
     k$3216
     k$3216
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     rename$675$1266
     list?
     k$3149
     k$3149
     k$3149
     k$3149
     k$3149
     k$3149
     k$3149
     k$3149
     k$3149
     k$3149
     k$3149
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     rename$675$1266
     rename$675$1266
     cons-source
     cons-source
     cons-source
     rename$675$1266
     rename$675$1266
     k$3077
     k$3077
     k$3077
     k$3077
     k$3077
     k$3077
     k$3077
     k$3077
     k$3077
     k$3077
     k$3077
     k$3077
     k$3077
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     append
     cons-source
     cons-source
     cons-source
     rename$675$1266
     map
     k$3138
     append
     cons-source
     rename$675$1266
     map
     k$3146
     rename$675$1266
     list?
     list?
     k$2989
     k$2989
     k$2989
     k$2989
     k$2989
     k$2989
     k$2989
     k$2989
     k$2989
     k$2989
     k$2989
     k$2989
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     append
     cons-source
     cons-source
     cons-source
     rename$675$1266
     map
     k$3058
     rename$675$1266
     append
     rename$675$1266
     map
     k$3064
     rename$675$1266
     cons-source
     cons-source
     cons-source
     rename$675$1266
     rename$675$1266
     list?
     list?
     k$2981
     error
     k$2981
     k$2981
     k$2981
     k$2981
     k$2981
     k$2981))
 (guard (k$2771
          k$2771
          k$2771
          k$2771
          k$2771
          k$2771
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          rename$836$1251
          rename$836$1251
          rename$836$1251
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          rename$836$1251
          rename$836$1251
          rename$836$1251
          rename$836$1251
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          cons-source
          rename$836$1251
          rename$836$1251
          rename$836$1251
          rename$836$1251
          rename$836$1251
          cons-source
          cons-source
          cons-source
          rename$836$1251
          rename$836$1251
          rename$836$1251
          rename$836$1251
          cons-source
          rename$836$1251
          rename$836$1251
          rename$836$1251
          cons-source
          rename$836$1251
          rename$836$1251
          rename$836$1251
          cons-source
          rename$836$1251
          rename$836$1251
          rename$836$1251
          list?
          list?
          k$2768
          error
          k$2768))
 (guard-aux
   (k$2739
     k$2739
     k$2739
     k$2739
     k$2739
     k$2739
     k$2739
     k$2739
     cons-source
     cons-source
     rename$863$1145
     list?
     compare$864$1144
     rename$863$1145
     k$2678
     k$2678
     k$2678
     k$2678
     k$2678
     k$2678
     k$2678
     k$2678
     k$2678
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     rename$863$1145
     rename$863$1145
     rename$863$1145
     cons-source
     cons-source
     cons-source
     rename$863$1145
     rename$863$1145
     compare$864$1144
     rename$863$1145
     k$2606
     k$2606
     k$2606
     k$2606
     k$2606
     k$2606
     k$2606
     k$2606
     k$2606
     k$2606
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     rename$863$1145
     cons-source
     cons-source
     rename$863$1145
     rename$863$1145
     rename$863$1145
     cons-source
     cons-source
     cons-source
     rename$863$1145
     rename$863$1145
     list?
     compare$864$1144
     rename$863$1145
     k$2584
     k$2584
     k$2584
     k$2584
     k$2584
     k$2584
     cons-source
     cons-source
     cons-source
     rename$863$1145
     k$2551
     k$2551
     k$2551
     k$2551
     k$2551
     k$2551
     k$2551
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     rename$863$1145
     rename$863$1145
     list?
     k$2518
     k$2518
     k$2518
     k$2518
     k$2518
     k$2518
     k$2518
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     rename$863$1145
     rename$863$1145
     list?
     k$2474
     k$2474
     k$2474
     k$2474
     k$2474
     k$2474
     k$2474
     k$2474
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     cons-source
     rename$863$1145
     cons-source
     cons-source
     rename$863$1145
     rename$863$1145
     list?
     list?
     k$2465
     error
     k$2465
     k$2465
     k$2465
     k$2465
     k$2465
     k$2465
     k$2465))
 (make-type-predicate (k$2446 k$2446 k$2446))
 (type-slot-offset (_list-index))
 (make-getter (k$2422))
 (_list-index (_list-index _list-index))
 (record? ()))
)
(pretty-print (udf:find-inlinable candidates))

;(let ((results (udf:find-inlinable candidates)))
;  (pretty-print
;    `(inlines: ,(car results)))
;  (pretty-print
;    `(candidates: ,(cdr results))))
;(write (reduce-call-list '(char-numeric? char-upper-case?) candidates))
;(newline)
;(write (reduce-call-list '(list char-numeric? char-upper-case?) candidates))
