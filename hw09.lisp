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

CS 2800 Homework 9 - Spring 2013

Student names:

For this homework, you will be using the BEGINNER mode in ACL2s.

Instructions for using this file:

- Open this file in ACL2s as hw09.lisp. Note that only Part III. on
  TERMINATION actually requires ACL2s. Other questions are text that
  require answers as text, which must be written as ACL2s comments. This
  file already contains many comments, so you can see what the syntax is.

- Set the session mode to "BEGINNER".

- Insert your solutions into this file where indicated (for instance
  as "..."). For programming problems, this includes contracts, function
  bodies, and test cases. Make sure you provide as many test cases as
  required (in addition to the ones already provided, of course).

- Do not comment in or comment out any existing function definitions from
  this file. Also, do not change the order of any function definitions,
  whether provided by us or defined by you.

- Make sure the entire file is admitted by ACL2s in BEGINNER mode. This
  means:

  * there must be no "..." in the code. If you don't finish some
    problems, comment them out. The same is true for any English text
    that you may add.

  * all check= tests must be satisfied. If any of the provided tests fail,
    your function definition is wrong, and the file will not be admitted.
    If any of your own tests fail, it may be that your function is wrong,
    but it may also be that your check= specification is incorrect.

 - To turn in your solution, check out the directory at

  svn://dell-server-wahl-1.ccs.neu.edu/cs2800-2013-spring/09/<groupname>

  where groupname is the name of the group as it was emailed to you in the
  last homework. Your username and password remain as before.

  Place your file hw09.lisp in this directory, add it, and commit.

- Whenever convenient, we will use F and T to represent false and true, and
  the following character combinations to represent the Boolean
  connectives:

    NOT        ~

    AND        /\
    OR         \/

    IMPLIES    =>

    EQUIVALENT =
    XOR        <>

  The binding powers of these connectives are as in class: they are listed
  in the above table from highest to lowest. Within one group
  (no blank line), the binding power is the same.

|#

; I. EQUATIONAL REASONING and INDUCTION

#|

This part assumes the following definitions:

(defdata nat-list (listof nat))

(defunc scale (l n)
  :input-contract (and (nat-listp l) (natp n))
  :output-contract (nat-listp (scale l n))
  (if (endp l)
    nil
    (cons (* n (first l)) (scale (rest l) n))))

(defunc in2 (a l)
  :input-contract (listp l)
  :output-contract (booleanp (in2 a l))
  (if (endp l)
    nil
    (or (equal a (first l)) (in2 a (rest l)))))

(defunc subsetp (l1 l2)
  :input-contract (and (listp l1) (listp l2))
  :output-contract (booleanp (subsetp l1 l2))
  (if (endp l1)
    t
    (and (in2 (first l1) l2) (subsetp (rest l1) l2))))

1. Using equational reasoning, prove the following:

(and (implies (endp l)
              (equal (scale (scale l n1) n2)
                     (scale l (* n1 n2))))
     (implies (and (consp l)
                   (equal (scale (scale (rest l) n1) n2)
                          (scale (rest l) (* n1 n2))))
              (equal (scale (scale l n1) n2)
                     (scale l (* n1 n2)))))
                     
Phi = (listp l) /\ (natp n1) /\ (natp n2) => ((endp l) => (scale (scale l n1) n2) = (scale l (* n1 n2)))
                                             /\ ((consp l) /\ ((scale (scale (rest l) n1) n2) = (scale (rest l) (* n1 n2))) =>
                                                   (scale (scale l n1) n2) = (scale l (* n1 n2)))
                     
Phi =  Alpha /\ Beta

Alpha = (listp l) /\ (natp n1) /\ (natp n2) /\ (endp l) => (scale (scale l n1) n2) = (scale l (* n1 n2))

Context:
c1. (listp l)
c2. (natp n1)
c3. (natp n2)
c4. (endp l)

Derived Context:
c5. x = nil {c1, c4}

Proof:
(scale (scale l n1) n2)
= {c5, def. scale}

(scale nil n2)
= {def. scale}

nil
= {def. scale}

(scale l (* n1 n2)) = (scale l (* n1 n2))

Beta = (listp l) /\ (natp n1) /\ (natp n2) /\ (consp l) /\ ((scale (scale (rest l) n1) n2) = (scale (rest l) (* n1 n2))) =>
                                                   (scale (scale l n1) n2) = (scale l (* n1 n2))

Context:
c1. (listp l)
c2. (natp n1)
c3. (natp n2)
c4. (consp l)
c5. ((scale (scale (rest l) n1) n2) = (scale (rest l) (* n1 n2)))

Proof:
(scale (scale l n1) n2)
= {c1, c4, def. scale}

(scale (cons (* n1 (first l)) (scale (rest l) n1)) n2)
= {def. scale}

(cons (* n2 (* n1 (first l))) (scale (scale (rest l) n1) n2))
= {c5, cons axiom}

(cons (* n2 (* n1 (first l))) (scale (rest l) (* n1 n2)))
= {arith., def scale}

(scale l (* n1 n2)) = (scale l (* n1 n2))


2. Using equational reasoning, prove the following:

(and (implies (endp l)
              (subsetp l l))
     (implies (and (consp l)
                   (subsetp (rest l) (rest l)))
              (subsetp l l)))
              
Phi = (listp l) => ((endp l) => (subset l l)) /\ ((consp l) /\ (subset (rest l) (rest l)) => (subset l l))

Phi = Alpha /\ Beta

Alpha = (listp l) /\ (endp l) => (subset l l)

Context:
c1. (listp l)
c2. (endp l)

Derived Context
c3. l = nil {c1, c2}

(subset l l)
= {c3}

(subset nil nil)
= {def. subset}

t

Beta = (listp l) /\ (consp l) /\ (subset (rest l) (rest l)) => (subset l l)

Context:
c1. (listp l)
c2. (consp l)
c3. (subset (rest l) (rest l))
c4. (subset (rest l) (cons a (rest l))) {c2, c3, L1, MP}

(subset l l)
= {c1, c2, def. subset}

(in2 (first l) l) /\ (subset (rest l) l)
= {def. in2}

((equal (first l) (first l)) \/ (in2 (first l) (rest l))) /\ (subset (rest l) l)
= {prop. reason.}

(subset (rest l) l)
= {def. listp}

(subset (rest l) (cons (first l) (rest l)))
= {c4}

t

;; In doing so, you might require a lemma: in general we need to prove any
;; conjectures we make up, only then do they become lemmas. For this question,
;; you may assume the following lemma for free (that is you don't need to prove
;; it before using it).
L1:
(implies (and (listp l1)
              (listp l2)
              (subsetp l1 l2))
         (subsetp l1 (cons a l2)))

INDUCTION

Induction Schemes

Assume you are attempting to prove a formula phi using induction. Write the
proof obligations that the induction scheme of each of the following
functions gives rise to:

3. (defunc nind (n)
 :input-contract (natp n)
 :output-contract t
  (if (equal n 0)
   0
   (nind (- n 1))))
   
i)
(not (natp n)) => Phi

ii)
(natp n) /\ (n = 0) => Phi

iii)
(posp n) /\ Phi|((n n-1)) => Phi

4. (defunc del (a x)
 :input-contract (listp x)
 :output-contract (listp (del a x))
  (cond ((endp x)            nil)
        ((equal a (first x)) (del a (rest x)))
        (t                   (cons (first x) (del a (rest x))))))
        
i)
(not (listp x)) => Phi

ii)
(listp x) /\ (endp x) => Phi

iii)
(listp x) /\ (endp x) /\ Phi|((x (rest x))) => Phi

5. (defunc in (a x)
 :input-contract (listp x)
 :output-contract (booleanp (in a x))
   cond ((endp x)            nil)
        ((equal a (first x)) t)
        (t                   (in a (rest x))))
        
i)
(not (listp x)) => Phi

ii)
(listp x) /\ ((endp x) \/ (equal a (first x))) => Phi

iii)
(consp x) /\ ~(equal a (first x)) => Phi

6. (defunc new-sum (a b)
 :input-contract (and (natp a) (natp b))
 :output-contract (natp (new-sum a b))
     (if (equal a 0)
       0
      (+ a b)))
      
i)

~(natp a) \/ ~(natp b) => Phi

ii)

(natp a) /\ (natp b) => Phi

7. (defunc rev3 (x)
 :input-contract (listp x)
 :output-contract (listp (rev3 x))
   (if (endp x)
    nil
    (app (rev3 (rest x)) (list (first x)))))
    
i)
(not (listp x)) => Phi

ii)
(listp x) /\ (endp x) => Phi

iii)
(consp x) /\ Phi|((x (rest x)) => Phi


8. (defunc fib (n)
 :input-contract (natp n)
 :output-contract (natp (fib n))
    (cond ((equal n 0)
            1)
          ((equal n 1)
            1)
          (t (+ (fib (- n 1)) (fib (- n 2))))))
          
i)
(not (natp n)) => Phi

ii)
(natp n) /\ ((equal n 0) (equal n 1)) => Phi

iii)
(natp n) /\ ~(n = 0) /\ ~(n = 1) /\ Phi|((n n-1) (n n-2)) => Phi

Functions from induction schemes

9. Write a function such that its induction scheme gives rise to the below
proof obligations:

1. (not (listp l)) => phi
2. (listp l) /\ (endp l)  => phi
3. (listp l) /\ (not (endp l)) /\ phi|((l (rest l))) => phi

Is the above function unique (it is unique if there does not exist another
function that has the induction scheme above)? If yes, justify in English.
If no, write that function's definition in ACL2s syntax.

Proofs using induction

10a. Write a recursive function cube-n to compute the sum of cubes of the
first n positive natural numbers, that is the cubes of 1,...,n . Make your
function as simple and as concise as possible, as you will use this
definition in proofs below.

(defunc cube-n (n)
  :input-contract (natp n)
  :output-contract (natp (cube-n n))
  (if (equal n 0)
      0
      (+ (* n (* n n)) (cube-n (- n 1)))))

10b. Someone claims the following closed-form formula for the sum of cubes
of the first n natural numbers as 1^3 + ... + n^3 = n*n*(n+1)*(n+1)/4.

Prove this claim using induction. Formally, prove:

(natp n) => ((cube-n n)= (n*n*(n+1)*(n+1)/4)))

Hint: Use the induction scheme of nind. Prove each of the proof obligations
arising from the induction scheme of nind using Equational reasoning, where phi
is the conjecture.

Phi = ((cube-n n) = (n*n*(n+1)*(n+1)/4))

i) 
(not (natp n)) => Phi

ii)
(natp n) /\ (n = 0) => Phi
Context:
c1. (natp n)
c2. (n = 0)

(cube-n n)
= {def. cube-n}

0
= {arith.}

(n*n*(n+1)*(n+1)/4) = (n*n*(n+1)*(n+1)/4)

iii)
(natp n) /\ (n > 0) /\ Phi|((n n-1)) => Phi
Context:
c1. (natp n)
c2. (n > 0)
c3. ((cube-n n-1) = ((n-1)*(n-1)*n*n/4))

(cube-n n)
= {c2, def. cube-n}

(+ (* n (* n n)) (cube-n n-1))
= {c3}

(+ n^3 ((n-1)*(n-1)*n*n/4))
= {arith.}

n^3 + (1/4)n^4 + (-1/2)n^3 + (1/4)n^2
= {arith.}

(1/4)n^4 + (1/2)n^3 + (1/4)n^2

(n^2)/4 * n^2 + 2n + 1
= {arith.}

(n*n*(n+1)*(n+1)/4) = (n*n*(n+1)*(n+1)/4)

|#

; II. DEFINITIONAL PRINCIPLE

#|

 Review the Definitional Principle: course notes at
 http://www.ccs.neu.edu/course/cs2800/rapdt.pdf

For each of the definitions below, check whether it is admissible, i.e., it
satisfies the definitional principle. You can assume that f is a new
function symbol in each case, even though we use the same name f for all
functions.

If yes,

1. Provide a measure function that can be used to show termination.
2. Explain in English why the contract theorem holds.
3. Explain in English why the body contracts hold.

If no,

1. Identify which of the 6 conditions of the Definitional Principle are violated.
2. If the function is terminating, provide a measure function. Otherwise,
   provide an input that satisfies the input contract, but the function
   does not terminate on it.

1.
(defunc f (x l)
  :input-contract (and (integerp  x) (listp l))
  :output-contract (natp (f x l))
  (cond ((endp l) 0)
        ((> x 0)  (f (len l) (rest l)))
        (t        (+ (f x (rest l)) 1))))
Yes

1)   
(defunc m (x l)
  :input-contract (and (integerp  x) (listp l))
  :output-contract (natp (m x l))
  (len l))
  
2)
The contract theorem holds because:
The only termination case returns 0 which is a nat and the only case that stacks adds one to the answer which is the definition of a nat.

3)
The body contracts hold because:
l is a list so (len l) and (endp l) can be used.
x is an integer so (> x 0) can be used.
In the first recursive case, (len l) is always a integer and (rest l) is guarded by the (endp l) so it always a list.
In the second recursive case, x remains the game while (rest l) is called which is still guarded.

2.
(defunc f (a b)
  :input-contract  (and (natp a) (natp b))
  :output-contract (natp (f a b))
  (cond ((and (equal a 0) (equal b 0)) 0)
        ((< a b)                       (f b a))
        (t                             (f a (- b 1)))))

No

1)
#4 and #5 are violated because the function can recure on a negative integer which violates the input contract.

2)
Not terminating.
a = 1
b = 0
     
3.
(defunc f (a b)
  :input-contract  (and (natp a) (natp b))
  :output-contract (natp (f a b))
  (cond ((and (equal a 0) (equal b 0)) 0)
        ((< a b)                       (f a b))
        (t                             (f b (- a 1)))))
        
No

1)
#4 is violated because if a < b then (f a b) will always become (f a b) which will infinitely recure.
        
2)
Not terminating.
a = 0
b = 1

4.
(defunc f (x y)
  :input-contract (listp y)
  :output-contract (natp (f x y))
  (cond ((endp y)  0)
        (t         (f  (len y) (x (rest y))))))
        
1)
#3 is violated because x is an undefined function.

2)
Not terminating.
x = nil
y = (list nil)

5.
(defunc f (x y)
  :input-contract (and (natp x) (natp y))
  :output-contract (booleanp(f x y))
  (cond ((equal x y) t)
         ((> x y)    (f (- x 1) y))
         (t          (f x (- y 1)))))
         
1)
(defunc m (x y)
  :input-contract (and (natp x) (natp y))
  :output-contract (natp (m x y))
  (+ x y))

2)
The contract theorem holds because:
The only terminatio case returns t which is a boolean and no recursive case stacks.

3)
The body contracts hold because:
(equal x y) excepts anything as x and y and x and y are nats thus (> x y) will hold.
On both termination cases x or y are incremented by one. 
The greater one of the two will always decrease and if they are equal y decrease.
If x equals 0 then the function will decrease y til its also 0 and vice versa.


6.
(defunc f (a b a)
  :input-contract  (and (natp a) (natp b))
  :output-contract (natp (f a b))
  (cond
    ((and (equal a 0) (equal b 0))  0)
    ((< a b)                        (f a (- b 1)))
    (t                              (f b (- a 1)))))

No

1)
#2 is violated because a and b and a are not unique variable.

2)
Not terminating.
Cannot put inputs in because a and a are not unique.
    
7.
(defunc f (x y)
  :input-contract (and (natp x) (natp y))
  :output-contract (booleanp(f x y))
  (cond ((equal x y) t)
         ((> x y)    (f (- x 1) y))
         (t          (f y x))))

Yes

1)
(defunc m (x y)
  :input-contract (and (natp x) (natp y))
  :output-contract (natp (m x y))
  (if (> x y)
      x
      (+ y 1)))
      
2)
The contract theorem holds because:
The only termination case returns t which is always a boolean and no other case stacks onto the answer.

3)
The body contracts hold because:
(equal x y) can operate on any two inputs in the acl2 universe and (> x y) is fine because x and y are nats.
In the first recuresive case x must be greater than zero because 0 is the lower y can be thus substracting 1 is still a nat.
In the second recuresive case x and y flip and they are both nats so they will pass the input contract.

8.
(defunc f (x l)
  :input-contract (and (integerp  x) (listp l))
  :output-contract (natp (f x l))
  (cond ((endp l) 0)
        ((> x 0)  (f (len l) (rest l)))
        (t        (- (f x (rest l)) 1))))
        
No

1)
#6 is violate because if l is not empty and  x is less than or equal to 0 at any point the function will return a negative integer.

2)
(defunc m (x l)
  :input-contract (and (integerp  x) (listp l))
  :output-contract (natp (m x l))
  (len l))
      
9.
(defunc f (x y)
  :input-contract (and (natp x) (natp y))
  :output-contract (booleanp(f x y))
  (cond ((equal x y) t)
         ((> x y)    (f (- x 1) (+ y 1)))
         (t          (f (+ y 1) x))))

1)

(defunc m (x y)
  :input-contract (and (natp x) (natp y))
  :output-contract (natp (m x y))
  (if (evenp (+ x y))
      (+ (if (>= x y) 0 1) (abs (- x y)))
      (+ (if (> x y)  3 2) (abs (- x y)))))
  
  
          
(f 0 1) 3
(f 2 0) 2
(f 1 1) 0

(f 1 0) 4
(f 0 1) 3
(f 2 0) 2
(f 1 1) 0

2)
The contract theorem holds because:
The only termination case returns t which is always a boolean and no other case stacks onto the answer.

3)
The body contracts hold because:
(equal x y) can operate on any two inputs in the acl2 universe and (> x y) is fine because x and y are nats.
The first recuresive case will always have x and y as nats because x cannot be 0 because y is less thus you can subtract 1 and get a nat and you can always add one to a nat.
The second recuresive case will flip the terms and adds one to the new x which will always be a nat.

         
; III. A TERMINATION TESTER

The subject of this exercise is to study the problem of designing a
function that automatically checks whether another (recursively defined)
function terminates.

But wait -- we already know this problem is undecidable! That means there
is no ACL2 function that takes an arbitrary ACL2 function f as
input (encoded in some suitable way) and determines correctly whether f
terminates.

We will therefore only solve a simple version of this problem: our function
will determine whether a given function f terminates in a predetermined
number of "steps". We will see shortly what "steps" means.

We begin with an example.

1. Define a function f that takes a positive natural number and is defined
mathematically as follows:

         1        if n is 1
       /
f(n) = - f(n/2)   if n is even
       \
         f(3*n+1) if n is greater than 1 and odd.

Here is how to read that notation: first observe that the three cases on
the right are mutually exclusive, i.e. every positive natural number fits
into exactly one case. Given n, determine which case it fits in. The
expression associated with that case determines f(n).

Define this function in ACL2s, in :program mode. We will soon see why.

|#

:program

(defunc f (n)
  :input-contract (posp n)
  :output-contract (equal (f n) 1)
  (cond ((equal n 1) 1)
        ((posp (/ n 2)) (f (/ n 2)))
        (t (f (+ (* 3 n) 1)))))

; What does this function return, if anything?
; 1

; Write at least 3 check= tests that (should) confirm your conjecture.

(check= (f  8) 1)
(check= (f  5) 1)
(check= (f 40) 1)

#|

You can think of this function as generating a sequence of positive natural
numbers, namely the numbers that f is called on recursively. For example:

f(8) = f(4) = f(2) = f(1) = 1

To get a feel for f, write down the call sequences for the following
initial arguments, until the recursion ends:

f(10) = f(5) = f(16) = f(8) = f(4) = f(2) = f(1) = 1
f(7) = f(22) = f(11) = f(34) = f(17) = f(52) = f(26) = f(13) = f(40) = f(20) = f(10) = = f(5) = f(16) = f(8) = f(4) = f(2) = f(1) = 1

Hint: try out (acl2::trace! f)

The reason we have defined this function in :program mode is that ACL2
cannot prove its termination. In fact, nobody knows whether this function
always terminates! You can read the whole story on Wikipedia. Search for
Collatz Conjecture.

Think about why it is (apparently) difficult to define a measure function
for f. Try it! (No response is required in your homework file.)

2. Modify f into a function f-c that takes not only n but also two other
arguments, count and limit, which are natural numbers such that count <=
limit. The idea is that count counts the number of recursive calls we have
to make to evaluate (f n). If that number exceeds limit, we abort the
entire computation and return the symbol ! (exclamation mark). Otherwise we
proceed as in f. Think about what values to pass to count and limit in
recursive calls.

f-c : Pos x Nat x Nat -> Pos union {!}

The input contract MUST enforce the arithmetic relationship between count
and limit mentioned above. The output contract MUST state that f-c returns
a positive natural number OR the symbol ! .

|#

(defunc f-c (n count limit)
  :input-contract (and (posp n) (natp count) (natp limit))
  :output-contract (or (posp (f-c n count limit)) (equal (f-c n count limit) '!))
  (cond ((equal count limit) '!)
        ((equal n 1) 1)
        ((posp (/ n 2)) (f-c (/ n 2) (+ 1 count) limit))
        (t (f-c (+ (* 3 n) 1) (+ 1 count) limit))))


(check= (f-c 8 0 2) '!)
(check= (f-c 8 0 3) '!)
(check= (f-c 8 0 4)  1)

; Write at least 3 more tests.

(check= (f-c 7 0 20) 1)
(check= (f-c 2 0 3) 1)
(check= (f-c 3 0 4)  '!)

#|

3. Define a function f-terminates that takes two arguments: a positive
natural n and a natural number limit, and checks whether (f n) returns
after at most limit recursive calls. f-terminates returns t or nil.
Obviously, in the body of f-terminates use f-c instead of f .

f-terminates : Pos x Nat -> Bool

|#

(defunc f-terminates (n limit)
  :input-contract (and (posp n) (natp limit))
  :output-contract (booleanp (f-terminates n limit))
  (equal (f-c n 0 limit) 1))
    

(check= (f-terminates 8 2) nil)
(check= (f-terminates 8 3) nil)
(check= (f-terminates 8 4) t)

; Write at least 3 more check= tests

(check= (f-terminates 7 15) nil)
(check= (f-terminates 7 16) nil)
(check= (f-terminates 7 17) t)

#|

4. Find the number *limit* of recursive calls that it takes for f to
terminate on input

1267650600228229401496703205376

That is, the following two tests must pass with your number *limit*,
which you should enter in place of the ... below:

|#

(defconst *limit* 101)

(check= (f-terminates 1267650600228229401496703205376 (- *limit* 1)) nil)
(check= (f-terminates 1267650600228229401496703205376    *limit*   ) t  )

#|

Hint: to determine *limit*, consider the trace mechanism mentioned above,
or binary search for the optimal parameters to f-c.

What is the mathematical relationship between the (large) input to f shown
above, and the number *limit* you found? Answer and explain in English.

*limit* is ...

5. Remember that f may not terminate on all inputs: nobody has been able to
prove that is does. Now observe that function f-terminates itself always
does terminate, even if f does not! The reason is that, instead of f, we
are using f-c within f-terminates, and f-c only recurs a given number of
times.

We can therefore use ?-terminates to test termination of functions even if
we _know_ they do not terminate. Consider:

|#

(acl2::acl2s-defaults :set acl2::testing-enabled nil) ; this turns off contract testing

(defunc g (n)
  :input-contract (integerp n)
  :output-contract (integerp (g n))
  (if (equal n 0)
    0
    (+ 1 (g (- n 1)))))

#|

This function behaves like the identity on natural number inputs, and
diverges on all others. Try (g 10), (g 0), (g -1). What does ACL2 return on
the final example?

Now modify g into a function g-c that relates to g like f-c does to f. That
is, g-c counts the number of recursive calls we have to make to evaluate (g
n). If that number exceeds a given limit, we abort the entire computation and
return the symbol ! (exclamation mark).

g-c : Int x Nat x Nat -> Int union {!}

This function is a bit more tricky than f, as it is not tail-recursive!
'let' is your friend.

|#

(defunc g-c (n count limit)
  :input-contract (and (integerp n) (natp count) (natp limit))
  :output-contract (or (natp (g-c n count limit)) (equal '! (g-c n count limit)))
  (if (equal count limit)
    '!
    (if (equal n 0)
      count
      (g-c (- n 1) (+ 1 count) limit))))

#|

Now define a similar wrapper function g-terminates that tests termination
of g within a pre-specified number of recursive calls. In the body of
g-terminates use g-c instead of g .

g-terminates : Int x Nat -> Bool

|#

(defunc g-terminates (n limit)
  :input-contract (and (integerp n) (natp limit))
  :output-contract (booleanp (g-terminates n limit))
  (natp (g-c n 0 limit)))
    

(check= (g-terminates 10 10)  nil)
(check= (g-terminates 10 11)  t)
(check= (g-terminates -1 10)  nil) ; g does not terminate at all on this input
(check= (g-terminates -1 100) nil) ; g does not terminate at all on this input

; Write 2 more check= tests

(check= (g-terminates 7 7)  nil)
(check= (g-terminates 7 8)  t)

#|

6. How would you modify a function that has several recursive calls, like
fib shown below, in a manner analogous to what we did to f, with arguments
count and limit, such that evaluation is aborted when count reaches limit?

(defunc fib (n)
  :input-contract (natp n)
  :output-contract (natp (fib n))
  (if (<= n 1)
    1
    (+ (fib (- n 1)) (fib (- n 2)))))

Explain in plain English. You don't need to write function code.

The function would add one the count in both recuretions. 
Instead of a plus function it would be a !+ function that would add unless an arguement was '! and would returns '! otherwise it would do addition.

###################################################################################

Some final comments (no response required): In this exercise, for every
function f we wanted to termination-test, we had to write a new function
f-terminates. Wouldn't it be better to have a function terminatesp that
takes (the code of) another function as input and performs the steps above?
so we don't have to write another function ?-terminates every time, which
anyway differs from the others mostly in the name of the function that is
being tested?

Yes, that would be better. But there are a number of difficulties. An
obvious one is that the different functions we want to test may take
different numbers of arguments, and may return different types of values.
So how many arguments would terminatesp have? and what would be their
types? Not obvious.

Supporting such "generic" terminatesp functions requires quite heavy
programming language support, such as higher-order functions: functions
that take other functions as input and can run them on arbitrary arguments.
As we have often mentioned, ACL2 does not support higher-order functions,
since they make proving theorems much much harder.

Note that a true termination prover would have to be a higher-order
procedure, since it takes another procedure/function as input.

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Feedback
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

Please fill out the following form:

https://docs.google.com/spreadsheet/viewform?formkey=dDhMZjZpcGlRaFRveEV0UUlNdUJqMVE6MA

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
