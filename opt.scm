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
'((call/cc (k f))
 (Cyc-version (k$5836))
 (and (list rename$2167 rename$2167 k$5822 k$5822))
 (or (list list
           rename$2164
           rename$2164
           rename$2164
           rename$2164
           list
           list
           rename$2164
           rename$2164
           k$5797
           k$5797))
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
       k$5736
       rename$2157
       k$5736
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
        k$5691
        rename$2153
        rename$2153
        k$5691
        rename$2153))
 (letrec (k$5667 append map map k$5683 map map list))
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
        rename$2094
        k$5488))
 (when (k$5384 error k$5381 error k$5365))
 (unless (k$5360 error k$5357 error k$5341))
 (do (k$5280
      rename$2069
      rename$2069
      k$5280
      rename$2069
      rename$2069
      rename$2069
      rename$2069
      rename$2069
      k$5262
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
 (values (k$5163 k$5163 k$5158 k$5158))
 (dynamic-wind
   (k$5140 after$2049 thunk$2050 before$2051))
 (call-with-port (k$5135 proc$2046))
 (Cyc-bin-op (k$5127 Cyc-bin-op cmp$2045 k$5127))
 (Cyc-bin-op-char (Cyc-bin-op cmp$2040))
 (char=? (Cyc-bin-op-char))
 (char<? (Cyc-bin-op-char))
 (char>? (Cyc-bin-op-char))
 (char<=? (Cyc-bin-op-char))
 (char>=? (Cyc-bin-op-char))
 (string=? (k$5096))
 (string<? (k$5092))
 (string<=? (k$5088))
 (string>? (k$5084))
 (string>=? (k$5080))
 (member-helper
   (member-helper k$5073 cmp-proc$2010 k$5073))
 (member (member-helper member-helper))
 (assoc-helper
   (k$5061 cmp?$2004 assoc-helper k$5057 k$5057))
 (assoc (assoc-helper assoc-helper))
 (foldl (foldl func$2000 k$5045))
 (foldr (func$1997 foldr k$5038))
 (flush-output-port
   (k$5006 k$5006 current-output-port))
 (write-string
   (k$5000 k$5000 current-output-port))
 (write-char (k$4994 k$4994 current-output-port))
 (not (k$4987 k$4987))
 (zero? (k$4974))
 (positive? (k$4971))
 (negative? (k$4968))
 (list (k$4950))
 (list-copy (foldr k$4934))
 (receive (k$4912))
 (car+cdr (values))
 (list-tail (list-tail k$4833 zero?))
 (list-ref (k$4829 list-tail))
 (reverse (foldl))
 (boolean=? (Cyc-obj=?))
 (symbol=? (Cyc-obj=?))
 (Cyc-obj=?
   (k$4500
     call/cc
     k$4503
     for-each
     return$1777
     k$4506
     type?$1776))
 (error (raise))
 (raise (list k$4144 r$4140))
 (raise-continuable (list k$4135 r$4131))
 (modulo (k$4083 k$4083 k$4083 k$4083 remainder))
 (odd? (k$4079 modulo))
 (even? (k$4075 modulo))
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
 (exact-integer? (k$4056 k$4056 exact?))
 (inexact? (not exact?))
 (max (foldl k$4044 k$4044))
 (min (foldl k$4038 k$4038))
 (gcd/main (gcd/main modulo abs))
 (gcd/entry (foldl k$4020))
 (lcm/main (abs gcd/main))
 (lcm/entry (foldl k$4007))
 (denominator (k$4004))
 (numerator (k$4001))
 (quotient (truncate))
 (truncate/
   (values truncate-remainder truncate-quotient))
 (floor-quotient
   (k$3983 exact? exact? k$3980 exact floor))
 (floor-remainder (k$3975 floor-quotient))
 (floor/ (values floor-remainder floor-quotient))
 (square (k$3967))
 (identifier->symbol (k$3960))
 (find-tail (k$3953 find-tail k$3953 pred$1596))
 (find (k$3949 k$3949 find-tail))
 (cons-source (k$3946))
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
     k$3329
     k$3326))
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
     k$2981
     k$2978))
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
          k$2768
          k$2765))
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
     k$2465
     k$2462))
 (make-type-predicate
   (k$2446 k$2446 k$2446 k$2445))
 (type-slot-offset (_list-index))
 (make-getter (k$2422 k$2421))
 (_list-index
   (k$2405
     _list-index
     k$2405
     _list-index
     k$2405
     k$2405))
 (record? (k$2398 k$2398 k$2398)))
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
