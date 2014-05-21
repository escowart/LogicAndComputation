; ****************** BEGIN INITIALIZATION FOR ACL2s MODE ****************** ;
; (Nothing to see here!  Your actual file is after this initialization code);

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading the TRACE* book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
; only load for interactive sessions: 
#+acl2s-startup (include-book "trace-star" :uncertified-okp nil :dir :acl2s-modes :ttags ((:acl2s-interaction)) :load-compiled-file nil);v4.0 change

#+acl2s-startup (assign evalable-printing-abstractions nil)

;arithmetic book
#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading arithmetic-5/top book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "arithmetic-5/top" :dir :system)

;basic thms/lemmas about lists
#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading coi/lists book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "coi/lists/basic" :dir :system)

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading ACL2's lexicographic-ordering-without-arithmetic book.~%This indicates that either your ACL2 installation is missing the standard books are they are not properly certified.") (value :invisible))
(include-book "ordinals/lexicographic-ordering-without-arithmetic" :dir :system)

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading the CCG book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "ccg" :uncertified-okp nil :dir :acl2s-modes :ttags ((:ccg)) :load-compiled-file nil);v4.0 change

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading ACL2s customizations book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "custom" :dir :acl2s-modes :uncertified-okp nil :load-compiled-file :comp :ttags :all)

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem setting up ACL2s mode.") (value :invisible))

;Settings common to all ACL2s modes
(acl2s-common-settings)

; Non-events:
(set-guard-checking :none)


; ******************* END INITIALIZATION FOR ACL2s MODE ******************* ;
;$ACL2s-SMode$;ACL2s
;Edwin Samuel Cowart
;Quest 1

(defdata natPair (list nat nat))
(defdata digit (oneof 0 1 2 3 4 5 6 7 8 9))
(defdata digitlist (listof digit))
(defdata digitlistPair (list digitlist digitlist))
    
; nat->digitlist : nat -> digitlist
; Converts a Natural Number into a Digit List.

(defunc nat->digitlist (n)
  :input-contract (natp n)
  :output-contract (digitlistp (nat->digitlist n))
  (if (< n 10) 
    (list n)
    (append
          (nat->digitlist (floor n 10))
          (list (mod n 10)))))

(check= (nat->digitlist 123456) '(1 2 3 4 5 6))
(check= (nat->digitlist 0000001) '(1))
(check= (nat->digitlist 1000000) '(1 0 0 0 0 0 0))
(check= (nat->digitlist 55) '(5 5))
(check= (nat->digitlist 0) '(0))

; digitlistzip : digitlist x digitlist -> digitlist
; Zips two Digit Lists of equal length together.
; (list x1 ... xn) x (list y1 ... yn) -> (list x1 y1 ... xn yn)

(defunc digitlistzip (a b)
  :input-contract (and (digitlistp a) 
                       (digitlistp b)
                       (equal (len a) (len b)))
  :output-contract (digitlistp (digitlistzip a b))
  (if (endp a)
    nil
    (cons (first a) 
          (cons (first b) 
                (digitlistzip (rest a) (rest b))))))

(check= (digitlistzip nil nil) nil)
(check= (digitlistzip '(3 4 5) '(0 0 0)) '(3 0 4 0 5 0))
(check= (digitlistzip '(0 0 0) '(3 4 5)) '(0 3 0 4 0 5))
(check= (digitlistzip '(0 0 0 0) '(0 3 4 5)) '(0 0 0 3 0 4 0 5))

; digitlist->nat : digitlist x nat -> nat
; Converts a Digit List into a Natural Number using an accumulator.

(defunc digitlist->nat (dl n)
  :input-contract (and (digitlistp dl) 
                       (natp n))
  :output-contract (natp (digitlist->nat dl n))
  (if (endp dl) n
    (digitlist->nat (rest dl)
                       (+ (* 10 n) (first dl)))))

(check= (digitlist->nat '(1 2 3 4 5 6) 0) 123456)
(check= (digitlist->nat '(9 9 9) 0) 999)
(check= (digitlist->nat '(9 9 9) 9) 9999)
(check= (digitlist->nat nil 10) 10)

; add-zeros : digitlist x nat -> digitlist
; Adds n number of 0 digits to the front of the Digit List

(defunc add-zeros (d n)
  :input-contract (and (digitlistp d) 
                       (integerp n))
  :output-contract (digitlistp (add-zeros d n))
  (if (<= n 0)
    d
    (add-zeros (cons 0 d) (- n 1))))

(check= (add-zeros '(1 2) 4) '(0 0 0 0 1 2))
(check= (add-zeros '(1 2) 0) '(1 2))
(check= (add-zeros '(1 2) -1) '(1 2))

:program

; even-dl-unzip : digitlist x digitlist x digitlist -> digitlistPair
; unzips an Even Digit List and conses them on to the two accumulators (leaf and right)
; (list x1 y1 ... xn yn) -> (list (list x1 ... xn) (list y1 ... yn))

(defunc even-dl-unzip (dl left right)
  :input-contract (and (digitlistp dl)
                       (natp (/ (len dl) 2))
                       (digitlistp left)
                       (digitlistp right)
                       (equal (len right) (len left)))
  :output-contract (digitlistPairp (even-dl-unzip dl left right))
  (if (endp dl)
    (list (revappend left nil)
          (revappend right nil))
    (even-dl-unzip (rest (rest dl))
                   (cons (first  dl) left)
                   (cons (second dl) right))))

(check= (even-dl-unzip '(1 2 3 4 5 6) nil nil) '((1 3 5) (2 4 6)))
(check= (even-dl-unzip nil '(0) '(1)) '((0) (1)))
(check= (even-dl-unzip '(0 1 2 3 4 5) '(8 3) '(1 3)) '((3 8 0 2 4) (3 1 1 3 5)))
(check= (even-dl-unzip nil nil nil) (list nil nil))

; dl-unzip : digitlist -> digitlistPair
; Unzips a Digit List into the two parts.
; (list x1 y1 ... xn yn)     -> (list (list x1 ... xn) (list y1 ... yn)) or
; (list y1 x1 ... x(n-1) yn) -> (list (list x1 ... x(n-1)) (list y1 ... yn))

(defunc dl-unzip (dl)
  :input-contract (digitlistp dl)
  :output-contract (digitlistPairp (dl-unzip dl))
  (if (natp (/ (len dl) 2))
    (even-dl-unzip dl nil nil)
    (even-dl-unzip (cons 0 dl) nil nil)))

(check= (dl-unzip '(1 1 2 2 3 4)) '((1 2 3) (1 2 4)))
(check= (dl-unzip '(1 2 2 3 4)) '((0 2 3) (1 2 4)))
(check= (dl-unzip nil) '(nil nil))

; w-w2-helper : digitlistPair -> natpair
; Convert both Digit List in a digitlistPair into their corresponding Natural Numbers

(defunc w-w2-helper (i)
  :input-contract (digitlistPairp i)
  :output-contract (natpairp (w-w2-helper i))
  (list (digitlist->nat (first i) 0) 
        (digitlist->nat (second i) 0)))

(check= (w-w2-helper nil) nil)
(check= (w-w2-helper '(() ())) '(0 0))
(check= (w-w2-helper '((1 3 5) (2 4 6))) '(135 246))
(check= (w-w2-helper '((1 1 1 1 0 0) (0))) '(111100 0))
(check= (w-w2-helper '((3 8 0 2 4) (3 1 1 3 5))) '(38024 31135))

; w-w2 : nat -> natpair
; Convert a unique Natural Number into a unique Pair of Natural Numbers

(defunc w-w2 (i)
  :input-contract (natp i)
  :output-contract (natpairp (w-w2 i))
  (w-w2-helper (dl-unzip (nat->digitlist i))))

(check= (w-w2 2000) (list 20 0))
(check= (w-w2 0) (list 0 0))
(check= (w-w2 1) (list 0 1))
(check= (w-w2 10) (list 1 0))
(check= (w-w2 11) (list 1 1))
(check= (w-w2 1000) (list 10 0))
(check= (w-w2 100) (list 0 10))

; w2-w-helper : digitlist x digitlist -> nat
; Converts two Digit Lists into a Natural Number

(defunc w2-w-helper (a b)
  :input-contract (and (digitlistp a) 
                       (digitlistp b))
  :output-contract (natp (w2-w-helper a b))
  (digitlist->nat
   (digitlistzip (add-zeros a (- (len b) (len a)))
                 (add-zeros b (- (len a) (len b)))) 0))


(check= (w2-w-helper '() '()) 0)
(check= (w2-w-helper '(0) '(0)) 0)
(check= (w2-w-helper '(1 2 3 4) '(5 0 0 0)) 15203040)

; w2-w : natpair -> nat
; Convert a unique Natural Number Pair into a unique Natural Numbers

(defunc w2-w (p)
  :input-contract (natpairp p)
  :output-contract (natp (w2-w p))
  (w2-w-helper (nat->digitlist (first p))
               (nat->digitlist (second p))))
                              

(check= (w2-w (list 20 0)) 2000)
(check= (w2-w (list 0 0)) 0)
(check= (w2-w (list 0 1)) 1)
(check= (w2-w (list 1 0)) 10)
(check= (w2-w (list 1 1)) 11)
(check= (w2-w (list 10 0)) 1000)
(check= (w2-w (list 0 10)) 100)

(acl2::acl2s-defaults :set acl2::num-trials 2500)

(acl2::test? 
 (implies (natp n)
          (equal (w2-w (w-w2 n))
                 n)))

(acl2::test?
 (implies (natpairp p)
          (equal (w-w2 (w2-w p))
                 p)))#|ACL2s-ToDo-Line|#


