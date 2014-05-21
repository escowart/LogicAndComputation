; **************** BEGIN INITIALIZATION FOR ACL2s B MODE ****************** ;
; (Nothing to see here!  Your actual file is after this initialization code);

#|
Pete Manolios
Fri Jan 27 09:39:00 EST 2012
----------------------------

Made changes for spring 2012.


Pete Manolios
Thu Jan 27 18:53:33 EST 2011
----------------------------

The Beginner level is the next level after Bare Bones level.

|#
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

;; #+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading DataDef+RandomTesting book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
;; (include-book "countereg-gen/top" :uncertified-okp nil :dir :system :load-compiled-file :comp)

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading ACL2s customizations book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "custom" :dir :acl2s-modes :uncertified-okp nil
                                         :load-compiled-file
                                         :comp :ttags :all)

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem setting up ACL2s Beginner mode.") (value :invisible))


;Settings common to all ACL2s modes
(acl2s-common-settings)

; Non-events:
(acl2::set-guard-checking :all)

(defconst *testing-upper-bound* 1000)  

(defun nth-small-pos-testing (n)
  (declare (xargs :guard (natp n)))
  (let ((n-small (mod n *testing-upper-bound*)))
    (nth-pos n-small)))

(defun nth-small-integer-testing (n)
  (declare (xargs :guard (natp n)))
  (let ((n-small (mod n *testing-upper-bound*)))
    (nth-integer n-small)))

(defun nth-small-nat-testing (n)
  (declare (xargs :guard (natp n)))
  (let ((n-small (mod n *testing-upper-bound*)))
    (nth-nat n-small)))

(defun nth-small-neg-testing (n)
  (declare (xargs :guard (natp n)))
  (let ((n-small (mod n *testing-upper-bound*)))
    (nth-neg n-small)))

(defun nth-small-positive-ratio-testing (n)
  (declare (xargs :guard (natp n)))
  (let ((n-small (mod n *testing-upper-bound*)))
    (nth-positive-ratio n-small)))

(defun nth-small-negative-ratio-testing (n)
  (declare (xargs :guard (natp n)))
  (let ((n-small (mod n *testing-upper-bound*)))
    (nth-negative-ratio n-small)))

(defun nth-small-rational-testing (n)
  (declare (xargs :guard (natp n)))
  (let ((n-small (mod n *testing-upper-bound*)))
    (nth-rational n-small)))

(defun nth-small-positive-rational-testing (n)
  (declare (xargs :guard (natp n)))
  (let ((n-small (mod n *testing-upper-bound*)))
    (nth-positive-rational n-small)))

(defun nth-small-negative-rational-testing (n)
  (declare (xargs :guard (natp n)))
  (let ((n-small (mod n *testing-upper-bound*)))
    (nth-negative-rational n-small)))

(defun nth-small-acl2-number-testing (n)
  (declare (xargs :guard (natp n)))
  (let ((n-small (mod n *testing-upper-bound*)))
    (nth-acl2-number n-small)))

(defun nth-small-complex-rational-testing (n)
  (declare (xargs :guard (natp n)))
  (let ((n-small (mod n *testing-upper-bound*)))
    (nth-complex-rational n-small)))

(defun nth-small-all (n)
  (declare (xargs ;:guard (natp n) ))
                  :verify-guards nil))
  (mv-let (choice seed) 
          (defdata::weighted-switch-nat 
            '(1  ;nil
              1  ;t
              1 ;0
              1 ;integer
              1  ;bool
              1  ;charlist
              1  ;sym
              1  ;string
              2  ;char
              1  ;acl2-num
              5 ;rational
              5 ;nat
              5 ;pos
              5  ;rational-list
              2  ;sym-list
              20 ;cons-atom
              5  ;nat-list
              10  ;cons-cons-atom
              1  ;stringlist
              10  ;atom-list
              ) n)
    (case choice
          (0 'nil)
          (1 't)
          (2 0)
          (3 (nth-small-integer-testing seed))
          (4 (nth (mod seed 2) *boolean-values*))
          (5 (nth-character-list seed))
          (6 (nth-symbol seed))
          (7 (nth-string seed))
          (8 (nth (mod seed (len *character-values*)) *character-values*))
          (9 (nth-small-acl2-number-testing seed))
          (10 (nth-small-rational-testing seed))
          (11 (nth-small-nat-testing seed))
          (12 (nth-small-pos-testing seed))
          (13 (nth-rational-list seed))
          (14 (nth-symbol-list seed))
          (15 (nth-cons-atom seed))
          (16 (nth-nat-list seed))
          (17 (nth-cons-ca-ca seed))
          (18 (nth-string-list seed))
          (19 (nth-atom-list seed))
          (t 'nil)))) ;this case should not come up


(defdata-testing pos :test-enumerator nth-small-pos-testing)
(defdata-testing integer :test-enumerator nth-small-integer-testing)
(defdata-testing nat :test-enumerator nth-small-nat-testing)
(defdata-testing neg :test-enumerator nth-small-neg-testing)
(defdata-testing positive-ratio :test-enumerator nth-small-positive-ratio-testing)
(defdata-testing negative-ratio :test-enumerator nth-small-negative-ratio-testing)
(defdata-testing rational :test-enumerator nth-small-rational-testing)
(defdata-testing positive-rational :test-enumerator nth-small-positive-rational-testing)
(defdata-testing negative-rational :test-enumerator nth-small-negative-rational-testing)
(defdata-testing acl2-number :test-enumerator nth-small-acl2-number-testing)
(defdata-testing complex-rational :test-enumerator nth-small-complex-rational-testing)
(defdata-testing all :test-enumerator nth-small-all)

(acl2s-defaults :set num-trials 50)

(defpkg "ACL2S B" ; beginner
  (union-eq '(t nil 
              ;if ; see macro below
              equal

              defun defunc ;for function definitions

              ; + * unary-- unary-/ < ; see definitions below
              numerator denominator
              rationalp integerp

              consp cons ; car cdr

              cond ; macro: explain
              list ; macro: explain

              lambda
              let let* ; macro: explain

              quote

              symbolp symbol-name symbol-package-name
              ;stringp
              ;charp

              check=

              and or iff implies not booleanp 
              ;+ * 
              / posp negp natp <= > >= zp - atom 
              ; true-listp 
              endp 
              ;caar cadr cdar cddr 
              ;caaar caadr cadar caddr cdaar cdadr cddar cdddr
              ;caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
              ;cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
              
              
              defdata nat string pos rational integer boolean all neg
              acl2-number true-list char symbol oneof listof enum record
              ;; i need them for defdata why?
              
              trace*

              defthm thm defconst in-package)
            '()))

(defthm natp-implies-acl2-numberp
  (implies (natp x)
           (acl2-numberp x))
  :rule-classes ((:rewrite)))

(defthm posp-implies-acl2-numberp
  (implies (posp x)
           (acl2-numberp x))
  :rule-classes ((:rewrite)))

(defthm integerp-implies-acl2-numberp
  (implies (integerp x)
           (acl2-numberp x))
  :rule-classes ((:rewrite)))

(defthm rationalp-implies-acl2-numberp2
  (implies (rationalp x)
           (acl2-numberp x))
  :rule-classes ((:rewrite)))

(defthm natp-implies-rationalp
  (implies (natp x)
           (rationalp x))
  :rule-classes ((:rewrite)))

(defthm posp-implies-rationalp
  (implies (posp x)
           (rationalp x))
  :rule-classes ((:rewrite)))

(defthm integerp-implies-rationalp
  (implies (integerp x)
           (rationalp x))
  :rule-classes ((:rewrite)))


(acl2::in-package "ACL2S B")

(defun acl2s-bb-identity-bool-guard (x)
  (acl2::declare (acl2::xargs :guard (acl2::booleanp x)))
  x)

(acl2::defmacro if (test tb fb)
  `(acl2::if (acl2s-bb-identity-bool-guard ,test) ,tb ,fb))

(acl2::defthm acl2s-bb-identity-bool-guard-backchain
  (acl2::implies (acl2::booleanp x)
                 (equal (acl2s-bb-identity-bool-guard x)
                        x)))

(acl2::defthm acl2s-bb-identity-bool-guard-equal
  (equal (acl2s-bb-identity-bool-guard (equal x y))
         (equal x y)))

(defunc first (x)
  :input-contract (consp x)
  :output-contract t
  (acl2::car x))

(defunc rest (x)
  :input-contract (consp x)
  :output-contract t
  (acl2::cdr x))

(defunc second (x)
  :input-contract (and (consp x) (consp (rest x)))
  :output-contract t
  (acl2::cadr x))

(defunc third (x)
  :input-contract (and (consp x) (consp (rest x)) (consp (rest (rest x))))
  :output-contract t
  (acl2::caddr x))

(defunc fourth (x)
  :input-contract (and (consp x) (consp (rest x)) 
                       (consp (rest (rest x)))
                       (consp (rest (rest (rest x)))))
  :output-contract t
  (acl2::cadddr x))

(defunc unary-- (x)
  :input-contract (rationalp x)
  :output-contract t
  (acl2::unary-- x))

(defunc unary-/ (x)
  :input-contract (acl2::and (rationalp x) (acl2::not (equal x 0)))
  :output-contract t
  (acl2::unary-/ x))

(defunc < (x y)
  :input-contract (acl2::and (rationalp x) (rationalp y))
  :output-contract (acl2::booleanp (< x y))
  (acl2::< x y))

(defunc + (x y)
  :input-contract (acl2::and (rationalp x) (rationalp y))
  :output-contract (rationalp (+ x y))
  (acl2::binary-+ x y))

(defunc * (x y)
  :input-contract (acl2::and (rationalp x) (rationalp y))
  :output-contract (rationalp (+ x y))
  (acl2::binary-* x y))

(defun my-preprocess (term wrld)
  (acl2::declare (acl2::ignore wrld))
  (acl2::cond ((acl2::and (consp term)
                          (acl2::or 
                           (equal (acl2::car term) 'acl2s-bb-identity-bool-guard)
                           (equal (acl2::car term) 'acl2s-bb-identity-consp-guard)
                           (equal (acl2::car term) 'acl2s-bb-identity-rationalp-guard)
                           (equal (acl2::car term) 'acl2s-bb-identity-rationalp-not-0-guard)))
               (acl2::cadr term))
              ((acl2::and (consp term)
                          (equal (acl2::car term) 'acl2::implies))
               (cons 'implies (acl2::cdr term)))
              ((acl2::and (consp term)
                          (equal (acl2::car term) 'acl2::/))
               (cons '/ (acl2::cdr term)))
              ((acl2::and (consp term)
                          (equal (acl2::car term) 'acl2::car))
               (cons 'first (acl2::cdr term)))
              ((acl2::and (consp term)
                          (equal (acl2::car term) 'acl2::cdr))
               (cons 'rest (acl2::cdr term)))
              ((acl2::and (consp term)
                          (consp (acl2::cdr term))
                          (equal (acl2::car term) 'acl2::not)
                          (equal (acl2::caadr term) 'acl2::>))
               (cons '<= (acl2::cdadr term)))
              ((acl2::and (consp term)
                          (equal (acl2::car term) 'acl2::not))
               (cons 'not (acl2::cdr term)))
              ((acl2::and (consp term)
                          (equal (acl2::car term) 'acl2::*))
               (cons '* (acl2::cdr term)))
              ((acl2::and (consp term)
                          (equal (acl2::car term) 'acl2::+))
               (cons '+ (acl2::cdr term)))
              ((acl2::and (consp term)
                          (equal (acl2::car term) 'acl2::/))
               (cons '/ (acl2::cdr term)))
              ((acl2::and (consp term)
                          (equal (acl2::car term) 'acl2::-))
               (cons '- (acl2::cdr term)))
              ((acl2::and (consp term)
                          (equal (acl2::car term) 'acl2::<))
               (cons '< (acl2::cdr term)))
              ((acl2::and (consp term)
                          (equal (acl2::car term) 'acl2::>))
               (cons '> (acl2::cdr term)))
              ((acl2::and (consp term)
                          (equal (acl2::car term) 'acl2::acl2-number))
               (cons 'acl2-number (acl2::cdr term)))
              (t nil)))

; A hack to help proofs go through in this mode.
(acl2::in-theory (acl2::enable rest))

(acl2::table acl2::user-defined-functions-table
             'acl2::untranslate-preprocess
             'my-preprocess)

(defunc len (a) 
  :input-contract t 
  :output-contract (natp (len a))
  (if (atom a)
      0
    (+ 1 (len (rest a)))))

(defthm intp-len 
  (integerp (len x))
  :rule-classes ((:type-prescription) (:rewrite)))

(acl2::defmacro listp (a)
  `(acl2::true-listp ,a))

(defunc append (a b) 
  :input-contract (and (listp a) (listp b))
  :output-contract (and (listp (append a b))
                        (equal (len (append a b)) (+ (len a) (len b))))
  (acl2::append a b))

(defthm append-length
  (equal (len (acl2::append a b))
         (+ (len a) (len b))))

(defunc app (a b) 
  :input-contract (and (listp a) (listp b))
  :output-contract (and (listp (app a b))
                        (equal (len (app a b)) (+ (len a) (len b))))
  (acl2::append a b))

(defunc rev (a) 
  :input-contract (listp a) 
  :output-contract (and (listp (rev a))
                        ;(equal (len (rev a)) (len a))
                        )
  (if (endp a)
      nil
    (append (rev (rest a)) (list (first a)))))

(defunc in (a X) 
  :input-contract (listp x)
  :output-contract (booleanp (in a X))
  (if (endp x)
      nil
    (or (equal a (first X))
        (in a (rest X)))))

(defunc remove-dups (a) 
  :input-contract (listp a) 
  :output-contract (listp (remove-dups a))
  (if (endp a)
      nil
    (if (in (first a) (rest a))
        (remove-dups (rest a))
      (cons (first a) (remove-dups (rest a))))))

(defunc nth (n l)
  :input-contract (and (natp n) (listp l))
  :output-contract t
  (if (endp l)
      nil
    (if (zp n)
        (first l)
      (nth (- n 1) (rest l)))))

(defunc nthrest (n l)
  :input-contract (and (natp n) (listp l))
  :output-contract (listp (nthrest n l))
  (if (endp l)
      nil
    (if (zp n)
        l
      (nthrest (- n 1) (rest l)))))

(defthm natp-acl2-len-tp 
  (natp (acl2::len x))
  :rule-classes ((:type-prescription) (:rewrite)))

(defthm integerp-acl2-len-tp 
  (integerp (acl2::len x))
  :rule-classes ((:type-prescription) (:rewrite)))

#|
(defunc string-len (l)
  :input-contract (stringp l)
  :output-contract (natp (string-len l))
  (acl2::length l))
|#


; harshrc 29 March 2012 -- added nth-list for Pete
(defun nth-list (n)
  (acl2::nth-true-list n))



;;Settings specific to this mode(copied from R&I mode)
(acl2::in-package "ACL2")
(set-backchain-limit '(50 100))
(set-rewrite-stack-limit 500)
(acl2s-defaults :set subgoal-timeout 60)
(acl2s-defaults :set defunc-timeout 200) 

(set-irrelevant-formals-ok :warn)
(set-bogus-mutual-recursion-ok :warn)
(set-ignore-ok :warn)

;for beginner users dont be strict in admitting defunc
;(acl2::acl2s-defaults :set acl2::defunc-strict 0)  
(acl2s-defaults :set num-trials 500)

;(assign evalable-ld-printingp t)
;(assign evalable-printing-abstractions '(list cons))
;(assign triple-print-prefix "; ")

(cw "~@0Beginner mode loaded.~%~@1"
    #+acl2s-startup "${NoMoReSnIp}$~%" #-acl2s-startup ""
    #+acl2s-startup "${SnIpMeHeRe}$~%" #-acl2s-startup "")


(acl2::in-package "ACL2S B")

; ***************** END INITIALIZATION FOR ACL2s B MODE ******************* ;
;$ACL2s-SMode$;Beginner
#|

CS 2800 Homework 11 - Spring 2013

Student names:

- For this homework, you are required to write induction proofs involving
  tail-recursive functions. This may involve defining functions that are
  admited by ACL2s.

- To turn in your solution, check out the directory at

  svn://dell-server-wahl-1.ccs.neu.edu/cs2800-2013-spring/11/<groupname>

  Place your file hw11.lisp in this directory, add it, and commit.

- Your entire solution file must be accepted by ACL2s, in BEGINNER mode. So
  enclose within comments as appropriate. If your solution to any question
  that is to be run in ACL2s is incomplete, comment out those portions so
  that the file is accepted by ACL2s.

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I. INDUCTION PROOFS INVOLVING TAIL-RECURSIVE FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; defining a list of natural numbers
(defdata natlist (listof nat))


;; The following theorems are needed for later proofs
(defthm natlist-app
  (implies (and (natlistp a)
                (natlistp b))
           (natlistp (acl2::append a b))))

(defthm rev-natlist
  (implies (natlistp l)
           (natlistp (rev l))))

;; 1. Define the function get-naturals, which takes a list l and returns
;; the list of natural numbers present in l.

(defunc get-naturals (l)
  :input-contract (listp l)
  :output-contract (natlistp (get-naturals l))
  (if (endp l)
    nil
    (if (natp (first l))
      (cons (first l) (get-naturals (rest l)))
      (get-naturals (rest l)))))

(check= (get-naturals '(a "abc" 1 aw 9)) '(1 9))
(check= (get-naturals '("hj" "abc" aw (1 2 3) 89 lk 1)) '(89 1))
(check= (get-naturals '(q "ab" lk ())) ())

;; 2. Define the function get-naturals-t, which is a tail-recursive version
;; of get-naturals with an accumulator:

(defunc get-naturals-t (l acc)
  :input-contract (and (listp l) (natlistp acc))
  :output-contract (natlistp (get-naturals-t l acc))
  (if (endp l)
    acc
    (get-naturals-t (rest l)
                    (if (natp (first l))
                      (cons (first l) acc)
                      acc))))
      

;; Write at least 3 check= tests
(check= (get-naturals-t '("five" "two" five 5 -1 0 3 5) nil) '(5 3 0 5))
(check= (get-naturals-t nil nil) nil)
(check= (get-naturals-t '(-1 -2 -3) '(0 1 2)) '(0 1 2))
(check= (get-naturals-t '(1 2 3) '(0 1 2)) '(3 2 1 0 1 2))
#|
  3. Define get-naturals*, a NON-RECURSIVE function that calls
  get-naturals-t and returns the same values as get-naturals.
  That is, the following must be a theorem:

  (listp l) => (get-naturals* l) = (get-naturals l)

  Note that get-naturals* has the same arguments and contracts as
  get-naturals.
|#

(defunc get-naturals* (l)
  :input-contract (listp l)
  :output-contract (natlistp (get-naturals* l))
  (rev (get-naturals-t l nil)))

#|

4. Identify a lemma that relates get-naturals-t to get-naturals. Remember 
   that this is a generalization step, i.e., all arguments to get-naturals-t
   are variables (no constants). The RHS should include acc.
   DO NOT prove this lemma at this point. (This will be done in step 6.)

  (listp l) /\ (listp acc) =>
    (get-naturals-t l acc) = (app (rev (get-naturals l)) acc)

  5. Assuming that lemma in 4 is true and using ONLY equational reasoning,
  prove the main theorem:

  (listp l) => (get-naturals* l) = (get-naturals l)
  
  Context:
  c1. (listp l)
  
  Proof:
  (get-naturals* l)
  = {def. get-naturals*}
  
  (rev (get-naturals-t l nil))
  = {lemma((l l) (acc nil))}
  
  (rev (app (rev (get-naturals l)) nil))
  = {L1((l (rev (get-naturals l))))}
  
  (rev (rev (get-naturals l)))
  = {L2((l (get-naturals l)))}
  
  (get-naturals l) = (get-naturals l)

  You may use the following lemmas without proof.

L1: (listp l) => (app l nil) = l
L2: (listp l) => (rev (rev l)) = l


6. Prove the lemma in 4. Use the induction scheme of get-naturals-t. In
   doing so, you may use the following lemma without proof:

   L3: (natp e) /\ (natlistp l) => (natlistp (cons e l))
   
   Phi = (listp l) /\ (listp acc) =>
          (get-naturals-t l acc) = (app (rev (get-naturals l)) acc)
   
   i)
   ~(listp l) \/ ~(listp acc) => Phi
   
   Context:
   c1. ~(listp l)
   c2. ~(listp acc)
   c3. (listp l)
   c4. (listp acc)
   
   Derived Contract:
   c5 nil {c1, c2, c3 ,c4}
   
   ii)
   (listp l) /\ (endp l) => Phi
   
   Context:
   c1. (listp l)
   c2. (endp l)
   c3. (listp acc)
   
   Derived Contract:
   c4. l = nil {c1, c2}
   
   Proof:
   (app (rev (get-naturals l)) acc)
   = {c4}
   
   (app (rev (get-naturals nil)) acc)
   = {def. get-naturals}
   
   (app (rev nil) acc)
   = {def. def. rev}
   
   (app nil acc)
   = {def. app}
   
   acc
   = {def. get-natural-t}
   
   (get-natural-t nil acc)
   = {c4}
   
   (get-natural-t l acc) = (get-natural-t l acc)
   
   iii)
   (listp l) /\ ~(endp l) /\ (natp (first l)) /\ Phi((l (rest l)) (acc (cons (first l) acc))) => Phi
   
   Context:
   c1. (listp l)
   c2. ~(endp l)
   c3. (natp (first l))
   c4. (listp (rest l)) /\ (listp acc) =>
          (get-naturals-t (rest l) acc) = (app (rev (get-naturals (rest l))) acc)
   c5. (listp acc)
   
   Derived Context:
   c6. (listp (rest l)) {c1, c2}
   c7. (get-naturals-t (rest l) (cons (first l) acc)) = (app (rev (get-naturals (rest l))) (cons (first l) acc)) {c4, c5, c6, MP}
   
   Proof:
   (app (rev (get-naturals l)) acc)
   = {def. get-naturals, c2, c3}
   
   (app (rev (cons (first l) (get-naturals l))) acc)
   = {def. rev}
   
   (app (app (rev (get-naturals l)) (list (first l))) acc)
   = {associativity-app-lemma((a (rev (get-naturals l))) (b (list (first l))) (c acc))}
   
   (app (rev (get-naturals l)) (app (list (first l)) acc))
   = {def. app}
   
   (app (rev (get-naturals l)) (cons (first l) (app nil acc)))
   = {def. app}
   
   (app (rev (get-naturals l)) (cons (first l) acc))
   = {c7}
   
   (get-naturals-t (rest l) (cons (first l) acc))
   = {def. get-naturals-t}
   
   (get-naturals-t l acc) = (get-naturals-t l acc)
   

7. Prove any remaining lemmas that you used but didn't prove. If none, say so.

associativity-app-lemma:
(listp a) /\ (listp b) /\ (listp c) => (app (app a b) c) = (app a (app b c))

i)
~(listp a) \/ ~(listp b) \/ ~(listp c) => Phi

Context:
c1. ~(listp a) \/ ~(listp b) \/ ~(listp c)
c2. (listp a)

Derived Context:
c3. l = nil {c1, c2}

ii)
(listp a) /\ (endp a) => Phi

Context:
c1. (listp a)
c2. (endp a)
c3. (listp b)
c4. (listp c)

Derived Context:
c5. a = nil {c1, c2}

Proof:
(app (app a b) c)
= {c5}

(app (app nil b) c)
= {def. app}

(app b c)
= {def. app}

(app nil (app b c))
= {c5}

(app a (app b c))

iii)

(listp a) /\ ~(endp a) /\ Phi((a (rest a))) => Phi

Context:
c1. (listp a)
c2. ~(endp a)
c3. (listp (rest a)) /\ (listp b) /\ (listp c) => 
       (app (app (rest a) b) c) = (app (rest a) (app b c))
c4. (listp b)
c5. (listp c)

Derived Context:
c6. (listp (rest a)) {c1, c2}
c7. (app (app (rest a) b) c) = (app (rest a) (app b c)) {c3, c4, c5, MP}

Proof:
(app (app a b) c)
= {def. app}

(app (cons (first a) (app (rest a) b)) c)
= {def. app}

(cons (first a) (app (app (rest a) b) c))
= {c7}

(cons (first a) (app (rest a) (app b c)))
= {def. app}

(app a (app b c)) = (app a (app b c))

|#

;; 8. Define the function how-many, which takes an element a and a list l
;; and returns the number of occurrences of a in l:

(defunc how-many (e l)
  :input-contract (listp l)
  :output-contract (natp (how-many e l))
  (if (endp l)
    0
    (+ (if (in e l) 1 0)
       (how-many e (rest l)))))

;;9. Write a function how-many-t, which is a tail-recursive version of how-many
;; with an accumulator:

(defunc how-many-t (e l acc)
  :input-contract (and (listp l) (natp acc))
  :output-contract (natp (how-many-t e l acc))
  (if (endp l)
    acc
    (how-many-t e (rest l)
                (+ (if (equal e (first l)) 1 0) acc))))

;; 10. Define how-many*, a NON-RECURSIVE function that calls how-many-t and
;;     returns the same values as how-many. That is, the following must be a
;;     theorem:

;;   (listp l) => (how-many* e l) = (how-many e l)

;;   Note that how-many* has the same arguments and contracts as how-many.

(defunc how-many* (e l)
  :input-contract (listp l)
  :output-contract (natp (how-many* e l))
  (how-many-t e l 0))

#|

11. Identify a lemma that relates how-many-t and how-many. Remember that
    this is a generalization step, i.e., all arguments to how-many-t are
    variables (no constants). The RHS should include acc.
how-many-lemma:
(listp l) => (how-many-t e l acc) = (+ acc (how-many e l))

12. Assuming that lemma in 11 is true and using ONLY equational reasoning,
    prove the main theorem:

  (listp l) => (how-many* e l) = (how-many e l)
  
  Context:
  c1. (listp l)
  
  Proof:
  (how-many* e l)
  = {def. how-many*}
  
  (how-many-t e l 0)
  = {how-many-lemma((e e) (l l) (acc 0))}
  
  (how-many e l) = (how-many e l)

13. Prove the lemma in 11. Use the induction scheme of how-many-t.

how-many-lemma:

i)
~(listp l) => Phi

Context:
c1. ~(listp l)
c2. (listp l)

Derived Context:
c3. nil {c1, c2}

ii)
(listp l) /\ (endp l) => Phi

Contest:
c1. (listp l)
c2. (endp l)

Derived Context:
c3. l = nil {c1, c2}

Proof:
(how-many-t e l acc)
= {c3}

(how-many-t e nil acc)
= {def. how-many-t}

acc
= {def. how-many}

(+ acc (how-many e nil))
= {c3}

(+ acc (how-many e l)) = (+ acc (how-many e l))

iii)
(listp l) /\ ~(endp l) /\ (equal e (first l)) /\ Phi((l (rest l)) (acc (+ 1 acc))) => Phi

Context:
c1. (listp l)
c2. ~(endp l)
c3. (listp acc)
c4. e = (first l)
c5. (listp (rest l)) /\ (listp (+ 1 acc)) => (how-many-t e (rest l) (+ 1 acc)) = (+ (+ 1 acc) (how-many e (rest l)))

Derived Context:
c6 (listp (rest l)) {c1, c2}
c7. (how-many-t e (rest l) (+ 1 acc)) = (+ (+ 1 acc) (how-many e (rest l))) {c5, c3, def nat, c4, c6}

Proof:
(how-many-t e l acc)
= {def. how-many-t}

(how-many-t e (rest l) (+ 1 acc))
= {c7}

(+ (+ 1 acc) (how-many e (rest l)))
= {arith, def. how-many, c4}

(+ acc (how-many e l)) = (+ acc (how-many e l))

iv)
(listp l) /\ ~(endp l) /\ ~(equal e (first l)) /\ Phi((l (rest l))) => Phi

Context:
c1. (listp l)
c2. ~(endp l)
c3. ~(equal e (first l))
c4. (listp (rest l)) => (how-many-t e (rest l) acc) = (+ acc (how-many e (rest l)))

Derived Context:
c5. (listp (rest l)) {c1, c2}
c6. (how-many-t e (rest l) acc) = (+ acc (how-many e (rest l)))

Proof:
(how-many-t e l acc)
= {def. how-many-t}

(how-many-t e (rest l) acc)
= {c6}

(+ acc (how-many e (rest l)))
= {c3, def. how-many}

(+ acc (how-many e l)) = (+ acc (how-many e l))

14. Prove any remaining lemmas that you used but didn't prove. If none, say so.

none.

|#

;; 15. Consider the function positions (x l), which determines the list of
;; positions in the list l where x occurs. It uses add-to-all (n l), which
;; adds the natural number n to every element of the list of naturals l:

(defunc add-to-all (n l)
  :input-contract (and (natp n) (natlistp l))
  :output-contract (natlistp (add-to-all n l))
  (if (endp l)
    ()
    (cons (+ (first l) n) (add-to-all n (rest l)))))

(check= (add-to-all 1 '(1 2 3)) '(2 3 4))

(defunc positions (x l)
  :input-contract (listp l)
  :output-contract (natlistp (positions x l))
  (cond ((endp l) ())
        ((equal (first l) x) (cons 0 (add-to-all 1 (positions x (rest l)))))
        (t                           (add-to-all 1 (positions x (rest l))))))

(check= (positions 3 '(3 1 9 7 3 67 34 3))  '(0 4 7))
(check= (positions 3 '(3 1 9 37 6 34 3 32)) '(0 6))
(check= (positions 1 '(3 1 9 7 3 67 34 1))  '(1 7))

;; Here is a tail-recursive version of positions, named positions-t:

(defunc positions-t (x l c acc)
  :input-contract (and (listp l) (natp c) (natlistp acc)) 
  :output-contract (natlistp (positions-t x l c acc))
  (cond ((endp l) acc)
        ((equal (first l) x) (positions-t x (rest l) (+ c 1) (cons c acc)))
        (t                   (positions-t x (rest l) (+ c 1)         acc))))

;; The tail-recursive function positions-t walks through the input list from
;; left to right. Suppose you find an occurrence of x. In order to know what
;; its position is in the original list, you have to keep track of the number
;; of elements already processed. That is the purpose of argument c of positions-t.

;; Now define positions*, a NON-RECURSIVE function that calls positions-t and
;; returns the same values as positions, i.e., this is a theorem:

;; (listp l) => (positions* x l) = (positions x l)

(defunc positions* (x l)
  :input-contract (listp l)
  :output-contract (natlistp (positions* x l))
  (rev (positions-t x l 0 nil)))

;; Note that positions* has the same arguments and contracts as positions.

#|

16. Identify a lemma that relates positions-t to positions. Remember that
this is a generalization step, i.e., all arguments to positions-t are
variables (no constants). The RHS should include c as well as acc.

Hint: this requires some thought. Evaluate positions-t on some examples to
find out what it should be. The lemma will involve several functions other
than positions-t and positions, including some functions defined earlier in
this homework. Consider the acl2::test? facility to sanity-check your
lemma.

(listp l) /\ (natp c) /\ (natlistp acc) => (positions-t x l c acc) = (app (rev (add-to-all c (positions x l))) acc)

17. Assuming that lemma in 16 is true and using ONLY equational reasoning,
    prove the main theorem:

  (listp l) => (positions* x l) = (positions x l)

(positions* x l)
= {def. positions*}

(rev (positions-t x l 0 nil))
= {l6}

(rev (app (rev (add-to-all 0 (positions x l))) nil))
= {app-list-nil-thm((l (rev (add-to-all 0 (positions x l)))))}

(rev (rev (add-to-all 0 (positions x l))))
= {rev-rev-thm((l (add-to-all 0 (positions x l))))}

(add-to-all 0 (positions x l))
= {lemma-ata-0((l (positions x l)))}

(positions x l) = (positions x l)


18. Prove the lemma in 16. Use the induction scheme of positions-t. In
    doing so, you may use the following lemmas without proof:

L3: (natp e) /\ (natlistp l) => (natlistp (cons e l))
L4: (listp x) /\ (listp y) => (app (rev x) (rev y)) = (rev (app y x))

There may be other lemmas that you find necessary. Put these on the todo
list, and prove them later.
(listp l) /\ (natp c) /\ (natlistp acc) => (positions-t x l c acc) = (app (rev (add-to-all c (positions x l))) acc)

i)
~(listp l) \/ ~(natp c) \/ ~(natlistp acc) => Phi

Context:
c1. ~(listp l) \/ ~(natp c) \/ ~(natlistp acc) 
c2. (listp l)
c3. (natp c)
c4. (natlistp acc)

Derived Context:
c5. nil {c1, c2, c3, c4}

ii)
(listp l) /\ (endp l) => Phi

Context:
c1. (listp l)
c2. (endp l)
c3. (natp c)
c4. (natlistp acc)

Derived Context:
c5. l = nil {c1, c2}

Proof:

LHS
(positions-t x l c acc)
= {c5}

(positions-t x nil c acc)
= {def. positions-t}

nil

RHS
(app (rev (add-to-all c (positions x l))) acc)
= {c5}

(app (rev (add-to-all c (positions x nil))) acc)
= {def. positions}

(app (rev (add-to-all c nil)) acc)
= {def. add-to-all}

(app (rev nil) acc)
= {def. rev}

(app nil acc)
= {def. app}

nil = nil

iii)
(listp l) /\ ~(endp l) /\ ((first l) = x) /\ Phi((l (rest l)) (c (+ c 1)) (acc (cons c acc))) => Phi

Context:
c1. (listp l)
c2. ~(endp l)
c3. (first l) = x
c4. (listp (rest l)) /\ (natp (+ c 1)) /\ (natlistp (cons c acc)) => 
      (positions-t x (rest l) (+ c 1) (cons c acc)) = (app (rev (add-to-all (+ c 1) (positions x (rest l)))) (cons c acc))
c5. (natp c)
c6. (natlistp acc)

Derived Context:
c7. (listp (rest l)) {c1, c2}
c8. (natp (+ c 1)) {c5, def. nat}
c9. (natlistp (cons c acc)) {c6, cons axiom}
c10. (positions-t x (rest l) (+ c 1) (cons c acc)) = (app (rev (add-to-all (+ c 1) (positions x (rest l)))) (cons c acc)) {c4, c7, c8, c9, MP}

Proof:
LHS
(positions-t x l c acc)
= {def. positions-t, c7, c3}

(positions-t x (rest l) (+ c 1) (cons c acc))
= {c10}

(app (rev (add-to-all (+ c 1) (positions x (rest l)))) (cons c acc))

RHS
(app (rev (add-to-all c (positions x l))) acc)
= {def. positions, c7, c3}

(app (rev (add-to-all c (cons 0 (add-to-all 1 (positions x (rest l)))))) acc)
= {def. add-to-all, cons axiom, arith}

(app (rev (cons c (add-to-all c (add-to-all 1 (positions x (rest l)))))) acc)
= {def. rev, cons axiom}

(app (app (rev (add-to-all c (add-to-all 1 (positions x (rest l))))) (list c)) acc)
= {associativity-app}

(app (rev (add-to-all c (add-to-all 1 (positions x (rest l))))) (app (list c) acc))
= {def. app}

(app (rev (add-to-all c (add-to-all 1 (positions x (rest l))))) (cons c (app nil acc)))
= {def. app}

(app (rev (add-to-all c (add-to-all 1 (positions x (rest l))))) (cons c acc))
= {add-to-all-lemma((a c) (b 1) (l (positions x (rest l))))

(app (rev (add-to-all (+ c 1) (positions x (rest l)))) (cons c acc))

(app (rev (add-to-all (+ c 1) (positions x (rest l)))) (cons c acc)) = (app (rev (add-to-all (+ c 1) (positions x (rest l)))) (cons c acc))

iv)

Context:
c1. (listp l)
c2. ~(endp l)
c3. ~((first l) = x)
c4. (listp (rest l)) /\ (natp (+ c 1)) /\ (natlistp acc) => 
      (positions-t x (rest l) (+ c 1) acc) = (app (rev (add-to-all (+ c 1) (positions x (rest l)))) acc)
c5. (natp c)
c6. (natlistp acc)

Derived Context:
c7. (listp (rest l)) {c1, c2}
c8. (natp (+ c 1)) {c5, def. nat}
c9. (positions-t x (rest l) (+ c 1) acc) = (app (rev (add-to-all (+ c 1) (positions x (rest l)))) acc) {c4, c6, c7, c8, MP}

Proof:
LHS
(positions-t x l c acc)
= {def. positions-t, c7, c3}

(positions-t x (rest l) (+ c 1) acc)
= {c9}

(app (rev (add-to-all (+ c 1) (positions x (rest l)))) acc)

RHS
(app (rev (add-to-all c (positions x l))) acc)
= {def. positions, c7, c3}

(app (rev (add-to-all c (add-to-all 1 (positions x (rest l))))) acc)
= {add-to-all-lemma((a c) (b 1) (l (positions x (rest l))))}

(app (rev (add-to-all (+ c 1) (positions x (rest l)))) acc)

(app (rev (add-to-all (+ c 1) (positions x (rest l)))) acc) = (app (rev (add-to-all (+ c 1) (positions x (rest l)))) acc)

19. Prove any remaining lemmas that you used but didn't prove. If none, say so.

lemma-ata-0:
(natlistp l) => (add-to-all 0 l) = l

i)
~(natlistp l) => Phi

Context:
c1. ~(natlistp l)
c2. (natlistp l)

Derived Context:
c3. nil {c1, c2}

ii)
(natlistp l) /\ (endp l) => Phi

Context:
c1. (natlistp l)
c2. (endp l)

Derived Context:
c3. l = nil {c1, c2}

Proof:
(add-to-all 0 l)
= {c3}

(add-to-all 0 nil)
= {def. add-to-all}

nil
= {c3}

l = l

iii)
(natlistp l) /\ ~(endp l) /\ Phi((l (rest l))) => Phi

Context:
c1. (natlistp l)
c2. ~(endp l)
c3. (natlistp (rest l)) => (add-to-all 0 (rest l)) = (rest l)

Derived Context:
c4. (natlistp (rest l)) {c1, c2}
c5. (add-to-all 0 (rest l)) = (rest l) {c3, c4}

Proof:
(add-to-all 0 l)
= {def. add-to-all}

(cons (+ (first l) 0) (add-to-all 0 (rest l)))
= {arith, c5}

(cons (first l) (rest l))
= {cons axiom}

l = l

add-to-all-lemma:
(natlistp l) /\ (natp a) /\ (natp b) => (add-to-all a (add-to-all b l)) = (add-to-all (+ a b) l)

i)
~(natlistp l) \/ ~(natp a) \/ ~(natp b) => Phi

Context:
c1. ~(natlistp l) \/ ~(natp a) \/ ~(natp b)
c2. (natlistp l)
c3. (natp a)
c4. (natp b)

Derived Context:
c5. nil {c1, c2, c3, c4}

ii)
(natlistp l) /\ (endp l) => Phi

Context:
c1. (natlistp l)
c2. (endp l)

Derived Context:
c3. l = nil {c1, c2}

Proof:
(add-to-all a (add-to-all b l))
= {c3}

(add-to-all a (add-to-all b nil))
= {def. add-to-all}

(add-to-all a nil)
= {def. add-to-all}

nil
= {def. add-to-all}

(add-to-all (+ a b) l) = (add-to-all (+ a b) l)

iii)
(natlistp l) /\ ~(endp l) /\ Phi((l (rest l))) => Phi

Context:
c1. (natlistp l)
c2. ~(endp l)
c3. (natlistp (rest l)) => (add-to-all a (add-to-all b (rest l))) = (add-to-all (+ a b) (rest l))

Derived Context:
c4. (natlistp (rest l)) {c1, c2}
c5. (add-to-all a (add-to-all b (rest l))) = (add-to-all (+ a b) (rest l))

Proof:
(add-to-all a (add-to-all b l))
= {def. add-to-all}

(add-to-all a (cons (+ b (first l)) (add-to-all b (rest l))))
= {def. add-to-all, cons axiom}

(cons (+ a (+ b (first l))) (add-to-all a (add-to-all b (rest l))))
= {c5}

(cons (+ a (+ b (first l))) (add-to-all (+ a b) (rest l)))
= {arith, def. add-to-all}

(add-to-all (+ a b) l) = (add-to-all (+ a b) l)

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; II. Fibonacci -- Made Efficient
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; First reduce the amount of testing done by ACL2 while it admits a function:
(acl2::acl2s-defaults :set acl2::subgoal-timeout 1)

;; Now recall the definition of the fib function:
(defunc fib (n)
  :input-contract (natp n)
  :output-contract (natp (fib n))
  (if (<= n 1)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

;; This is an elegant and very easy to read definition. But it is also very
;; slow. To see why, we trace the recursive calls to fib when processing
;; input. This is done as follows:

(acl2::trace! fib)
(acl2::set-guard-checking :none)

#|

(Hint: you can use (acl2::untrace$ ...) to stop tracing a function.)

Now try fib using some _small_ inputs. Start with n=1, n=2, to get a
feeling for the output produced by trace. A line of the form

 > (FIB 1)

indicates that a call (fib 1) was just encountered and is now being processes, while

 < (FIB 1)

indicates that the call (fib 1) was completed.

1. In the evaluation of (fib  5), how many times is fib called on argument 1 ? 5 times
   
   In the evaluation of (fib 10), how many times is fib called on argument 1 ? 55 times
   

Hint: you can use the Eclipse editor to count occurrences of certain text
strings, or you can copy the output of trace into your favorite alternative
editor.

Compare the above numbers with the values (fib 5) and (fib 10). What do you
find?

The number of calls of (fib 1) is equivalent to the value of the (fib n)

2. You saw how long the trace output of (fib 10) is -- for a fairly small
input of 10. Let's see whether we can make fib more efficient, but NOT
using tail-recursion (as we did in the last homework). Our fib-fast
function will be hard for ACL2 to prove terminating, so you are allowed to
write this function in program mode:

|#

:program

#|

The idea is as follows. First write a function fib-help that, for input n,
computes the _list_ of Fibonacci numbers
(fib n), (fib n-1), ..., 8,5,3,2,1,1,0 = (fib 0)
(descending order). See tests below, and also note the output contract,
which is provided for you. Provide 3 more tests.

To minimize the number of recursive calls required to evaluate (fib-help n),
you MUST use (let ...) whenever you need the result of a recursive call
several times. Your solution will be considered incorrect if your code
contains several calls to fib-help with the same arguments.

|#

(defunc fib-help (n)
  :input-contract (natp n)
  :output-contract (and (natlistp (fib-help n)) (equal (len (fib-help n)) (+ n 1)))
  (cond ((equal n 0) '(0))
        ((equal n 1) '(1 0))
        (t (let ((l (fib-help (- n 1))))
             (cons (+ (first l) (second l)) l)))))
    

(check= (fib-help 0) '(0))
(check= (fib-help 1) '(1 0))
(check= (fib-help 3) '(2 1 1 0))
(check= (fib-help 4) '(3 2 1 1 0))
(check= (fib-help 5) '(5 3 2 1 1 0))
(check= (fib-help 6) '(8 5 3 2 1 1 0))

;; Now write a non-recursive function fib-fast, with contracts as for the
;; (slow) fib function, which calls fib-help to compute (fib n).

(defunc fib-fast (n)
  :input-contract (natp n)
  :output-contract (natp (fib-fast n))
  (first (fib-help n)))

;; Now let's see whether fib-fast deserves that name. Turn on tracing for
;; the helper function (fib-fast itself is not recursive):

(acl2::trace! fib-help)

#|

In the evaluation of (fib-fast  5), how many times is fib-help called on argument 1 ? 1 time
In the evaluation of (fib-fast 10), how many times is fib-help called on argument 1 ? 1 time

Compare your results to those obtained with (fib n).

fib-fast is much faster than fib.

You can also try fib and fib-fast on input 100. Hint: try fib-fast first!

fib-fast took 100 calls then had to come back out and operate 100 more times while normal fib crashed my acl2s working envirnment.

|#

#|

Please fill out the following form:
https://docs.google.com/spreadsheet/viewform?formkey=dFI3Q3NYSlNwWUhacWplMFB2SjQyWGc6MA

Confirm here whom of the team members filled out the form:

Name 1 Edwin Cowart
Name 2 Elias Elgarten
(Name 3 ...)

Everyone who filled out the form gets credit worth 1 quiz.

The form is anonymous, to encourage you to write what you think about this
class. (If you want to stay anonymous, do not reveal your name on the
feedback form.) On the other hand, we must rely on your honesty in stating
whether you filled out the form or not, and in NOT filling out the form
several times (why would you do that??). We do not want to find
discrepancies in the number of entries on the form, and the number of
people claiming to have filled out the form.
|#