(import 
  (scheme base)
  (srfi 60)
  (scheme cyclone test))

(define a #b1100)
(define b #b1010)
(define c #b1110)

(define (dec->bin x) (number->string x 2))

(define (test-commutative op)
  (test (op a b) (op b a)))

(define (test-associative op)
  (test (op a (op b c)) (op (op a b) c)))

(test-group
  "logand"
  (test "1000" (dec->bin (logand a b)))
  (test "1000" (dec->bin (bitwise-and a b)))
  (test-commutative logand)
  (test-commutative bitwise-and)
  (test-associative logand)
  (test-associative bitwise-and))

(test-group
  "logior"
  (test "1110" (dec->bin (logior a b)))
  (test "1110" (dec->bin (bitwise-ior a b)))
  (test-commutative logior)
  (test-commutative bitwise-ior)
  (test-associative logior)
  (test-associative bitwise-ior))

(test-group
  "logxor"
  (test "110" (dec->bin (logxor a b)))
  (test "110" (dec->bin (bitwise-xor a b)))
  (test-commutative logxor)
  (test-commutative bitwise-xor)
  (test-associative logxor)
  (test-associative bitwise-xor))

(test-group
  "lognot"
  (test "-10000001" (dec->bin (lognot #b10000000)))
  (test "-10000001" (dec->bin (bitwise-not #b10000000)))
  (test "-1" (dec->bin (lognot #b0)))
  (test "-1" (dec->bin (bitwise-not #b0))))

(test-group
  "logtest"
  (do
    ((i 1 (when (= i j) (+ 1 i)))
     (j 0) (if (= i j) 0 (+ 1 j)))
    ((= i 1024))
    (test (not (zero? (logand i j))) (logtest i j))
    (test (not (zero? (logand i j))) (any-bits-set? i j))))

(test-group
  "logcount"
  (test 4 (logcount #b10101010))
  (test 4 (bit-count #b10101010))
  (test 0 (logcount 0))
  (test 0 (bit-count 0))
  (test 1 (logcount -2))
  (test 1 (bit-count -2)))

(test-group
  "integer-length"
  (test 8 (integer-length #b10101010))
  (test 0 (integer-length 0))
  (test 4 (integer-length #b1111)))

(define fsb-results #u8(-1 0 1 0 2 0 1 0 3 0 1 0 2 0 1 0 4))

(test-group
  "log2-binary-factors"
  (do
    ((i 0 (+ i 1)))
    ((= i 17))
    (let ((res (bytevector-u8-ref fsb-results i)))
      (test res (log2-binary-factors i))
      (test res (first-set-bit i))
      (test res (log2-binary (- i)))
      (test res (first-set-bit (- i))))))

(test-group 
  "logbit"
  (test-assert (logbit? 0 #b1101))
  (test-assert (bit-set? 0 #b1101))
  (test-not (logbit? 1 #b1101))
  (test-not (bit-set? 0 #b1101))
  (test-assert (logbit? 2 #b1101))
  (test-assert (bit-set? 2 #b1101))
  (test-assert (logbit? 3 #b1101))
  (test-assert (bit-set? 3 #b1101))
  (test-not (logbit? 4 #b1101))
  (test-not (bit-set? 4 #b1101)))

(test-group
  "copy-bit"
  (test "1" (dec->bin (copy-bit 0 0 #t)))
  (test "100" (dec->bin (copy-bit 2 0 #t)))
  (test "1011" (dec->bin (copy-bit 2 #b1111 #f))))

(test-group
  "bit-field"
  (test "1010" (dec->bin (bit-field #b1101101010 0 4)))
  (test "10110" (dec->bin (bit-field #b1101101010 4 9))))

(test-group
  "copy-bit-field"
  (test "1101100000" (dec->bin (copy-bit-field #b1101101010 0 0 4)))
  (test "1101101111" (dec->bin (copy-bit-field #b1101101010 -1 0 4)))
  (test "110100111110000" (dec->bin (copy-bit-field #b110100100010000 -1 5 9))))

(test-group
  "ash"
  (test "1000" (dec->bin (ash #b1 3)))
  (test "1000" (dec->bin (arithmetic-shift #b1 3)))
  (test "101" (dec->bin (ash #b1010 -1)))
  (test "101" (dec->bin (arithmetic-shift #b1010 -1))))

(test-group
  "rotate-bit-field"
  (test "10" (dec->bin (rotate-bit-field #b0100 3 0 4)))
  (test "10" (dec->bin (rotate-bit-field #b0100 -1 0 4)))
  (test "110100010010000" (dec->bin (rotate-bit-field #b110100100010000
                                                      -1
                                                      5
                                                      9)))
  (test "110100000110000" (dec->bin (rotate-bit-field #b110100100010000
                                                      1
                                                      5
                                                      9))))

(test-group
  "reverse-bit-field"
  (test "e5" (number->string (reverse-bit-field #xa7 0 8) 16)))
