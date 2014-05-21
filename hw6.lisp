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
CS 2800 Homework 6 - Spring 2013

Student names:

For this homework, you will be using the BEGINNER mode in ACL2s.

Instructions for using this file:

- Open this file in ACL2s as hw06.lisp .

- Set the session mode to "Beginner".

- Insert your solutions into this file where indicated (for instance
  as "..."). This includes contracts, function bodies, and test cases.
  Make sure you provide as many test cases as required
  (in addition to the ones already provided, of course).

- Do not comment in or comment out any existing function definitions
  from this file. Also, do not change the order of any function
  definitions, whether provided by us or defined by you.

- Make sure the entire file is admitted by ACL2s in Beginner
  mode. This means:
  
  * there must be no "..." in the code. If you don't finish some
    problems, comment them out. The same is true for any English text
    that you may add. This file already contains many comments, so you
    can see what the syntax is.
  
  * all check= tests must be satisfied. If any of the provided tests
    fail, your function definition is wrong, and the file will not be
    admitted.  If any of your own tests fail, it may be your function
    is wrong, but it may also be that your check= specification is
    incorrect.

  Sometimes it may take a while for your function to be admitted. Be
  patient: rest assured that, if your function is correct, the process
  will succeed. If you don't get a success message after 2-3
  mins (depending on your hardware), then probably your definition is
  too complicated or wrong.

- Save the file with your additions and turn it in. To do that, check
  out the directory at

  svn://dell-server-wahl-1.ccs.neu.edu/cs2800-2013-spring/06/<groupname>

  where groupname is the name of the group as it was emailed to
  you. Your username and password remain as before.

  Place your file hw06.lisp in this directory, add it, and commit. Do
  NOT submit the session file (ending in .a2s).

- For non-programming problems, include solutions as
  comments. Whenever convenient, we will use F and T to represent
  false and true, and the following character combinations to
  represent the Boolean connectives:

    NOT        ~

    AND        /\

    OR         \/

    IMPLIES    =>

    EQUIVALENT =

    XOR        <>

  The binding powers of these connectives are as in class: they are
  listed in the above table from highest to lowest. Within one
  group (no blank line), the binding power is the same.

|#

#| 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1. Substitutions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

In this part you will practice applying and recognizing substitutions.
Recall that a substitution is a list of pairs of the form (<var>
<term>). For an expression phi and a substitution sigma, we denote by
phi|sigma the expression obtained by applying the substitution sigma
to phi. Also remember that substitutions do not apply to quoted
variables.

[a]

Let

  phi = (+ x (* (f n) (+ 'n x)))

and

  sigma = ((x (+ n 2)) (n (+ m 1))) .

Determine phi|sigma .

   phi|sigma = (+ (+ n 2) (* (f (+ m 1)) (+ 'n (+ n 2))))

[b]

Let

  phi = (a 'x (a (a (b x) y) (b 'y)))

and

  sigma = ((x (+ z z)) (y (cons v w))) .

Determine phi|sigma.

   phi|sigma = (a 'x (a (a (b (+ z z)) (cons v w)) (b 'y)))

Determine (phi|sigma)|sigma .

   (phi|sigma)|sigma = (a 'x (a (a (b (+ z z)) (cons v w)) (b 'y)))

[c]

Let

  phi = (a (a 'x y) (a (a (a b a) 'a) (b 'y)))

and

  sigma = ((x (+ a b)) (b a) (a b) (y (a x y))) .

Determine phi|sigma.

   phi|sigma = (a (a 'x (a x y)) (a (a (a a b) 'a) (b 'y)))


Determine (phi|sigma)|sigma .

   (phi|sigma)|sigma = (a (a 'x (a (+ a b) (a x y))) (a (a (a b a) 'a) (b 'y)))

[d]

For any formula phi and any substitution sigma, (phi|sigma)|sigma =
phi|sigma . Is this claim true or false? If true, explain why. If
false, give a counterexample.

false

  phi = x
  
  sigma = ((x y) (y x))
  
  phi|sigma = y
  
  (phi|sigma)|sigma = x
  
  phi|sigma != (phi|sigma)|sigma
  
  y != x

[e]

Let

  phi = (- (* a b) (+ (* c d) (- d a)))

and

  psi = (- (* b a) (+ (* 71 (f b)) (- (f b) b))) .

Does there exist sigma such that phi|sigma = psi ? If your answer is
yes, give one. If your answer is no, explain why not.

  yes

  sigma = ((a b) (b a) (c 71) (d (f b)))

[f]

Let phi = (p (+ x y) (* (* 2 x) y)) . Then, for any two substitutions
sigma1 and sigma2, (phi|sigma1)|sigma2 = (phi|sigma2)|sigma1. Is this
claim true or false? If true, explain why. If false, give a
counterexample.

   false
   
   sigma1 = (x a)
   sigma2 = (x b)
   
   phi|sigma1 = (p (+ a y) (* (* 2 a) y))
   (phi|sigma1)|sigma2 = (p (+ a y) (* (* 2 a) y))
  
   phi|sigma2 = (p (+ b y) (* (* 2 b) y))
   (phi|sigma2)|sigma1 = (p (+ b y) (* (* 2 b) y))
   
   (phi|sigma1)|sigma2 != (phi|sigma2)|sigma1
   (p (+ a y) (* (* 2 a) y)) != (p (+ b y) (* (* 2 b) y))
  
|#

#|

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2. Contract Completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

For the following functions, we only know the signature:

f: Nat -> Nat
m: List -> List
c: Nat x List -> List
r: Nat x List -> Nat

[a]

Define functions f, m, c, and r with the above signatures. Your
functions should be as simple as possible! The actual outputs of the
functions are arbitrary, as long as they satisfy the output
contract. For example, for m, you may choose to simply return the
constant nil, which is a list.

|#

(defunc f (x)
  :input-contract (Natp x)
  :output-contract (Natp (f x))
  0)

(defunc m (a)
  :input-contract (listp a)
  :output-contract (listp (m a))
  nil)

(defunc c (x a)
  :input-contract (and (Natp x) (listp a))
  :output-contract (listp (c x a))
  nil)

(defunc r (x a)
  :input-contract (and (Natp x) (listp a))
  :output-contract (Natp (r x a))
  0)

#|

[b1]

We will now try to define more complex functions by reusing f, m, c,
and r. For each of the definitions, fill in the dots with contracts
such that ACL2 accepts the function.

The added contracts should be minimal. Therefore, do not add any
hypotheses that are enforced by the output contracts of the
participating functions. For instance, if your contract already
imposes the condition (listp a), and (m a) appears in a context where
a list is expected, you contract should not mention (listp (m a)) as
this is guaranteed by the output contract of m.

Sometimes there is no way to satisfy the input contracts of the
functions that appear in an expression. As an example, consider the
following definition:

  (defunc g0 (x)
    :input-contract ...
    :output-contract ...
    (c x x))

By instantiating the input contract of c with the substitution ((x
x) (a x)) , we get (and (natp x) (listp x)) . This formula is
equivalent to nil, because x cannot be a natural number and a list at
the same time! If you encounter such cases, then just comment out the
definition and write a comment explaining why the input contract is
nil.

Your output contracts should be as strong as possible. For example, t
is always a valid output contract, but it does not provide any useful
information.  Also, your output contracts should only depend on the
contracts of f, m, c, and r, and not on the concrete computation that
your implementations happen to perform. For example, consider the
following definition:

  (defunc g0 (x)
    :input-contract ...
    :output-contract ...
    (m x))

The input-contract should be (listp x) because of m's input
contract. The output contract should be (listp (g0 x)), even
though your implementation of m returns nil, because that is the
strongest contract that only depends on m's contract (i.e.  does
not depend on m's body).

|#

(defunc g1 (x y z)
  :input-contract (and
                   (natp x)
                   (natp y)
                   (listp z))
  :output-contract (natp (g1 x y z))
  (r (f x) (m (c y (m z)))))
(check= (g1 0 0 nil) 0)
#|
(defunc g2 (x y)
  :input-contract (and (natp x) (natp y) (listp y))
  ; simplifies to nil
  :output-contract (natp (g2 x y))
  (r x (r y (c x y))))
(check= (g2 0 nil) 0)
|#
(defunc g3 (x y)
  :input-contract (and (natp x) (listp y))
  :output-contract (booleanp (g3 x y))
  (equal (m (c x y))
         (c (f x) (m y))))
(check= (g3 0 nil) t)
#|
(defunc g4 (x y z)
  :input-contract (and (natp x) (natp y) (listp z))
  :output-contract (listp (g4 x y z))
  (c x (c y (r x z))))
  
  (r x z) = 0
  (c y 0) won't satifies c's input-contract
(check= (g4 0 nil) t)
|#
(defunc g5 (x y z w v)
  :input-contract (and (natp x)
                       (listp y)
                       (natp z)
                       (natp w)
                       (listp v))
  :output-contract (booleanp (g5 x y z w v))
  (implies (and (<= (r x (c x y))
                    (r z (c z y)))
                (<= (r z (c z v))
                    (r w (c w v))))
           (equal (c x (c z (c w v))) y)))
(check= (g5 0 nil 0 0 nil) t)
#|

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 3. Equational Reasoning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

The following questions ask for equational proofs about ACL2s
programs. Follow the steps introduced in class:

- Perform conjecture contract closure, adding hypotheses (if
  necessary).

- Use propositional reasoning to rewrite the conjecture into one or
  more implications with a conjunction of hypotheses in the antecedent
  and a single expression in the conclusion (if necessary).

- Extract the context (the hypotheses), and determine the derived
  context: anything that follows immediately from the context
  expressions. Remember that you cannot instantiate context
  expressions -- these are not theorems!

- Now write your equational reasoning proof. Be sure to justify each
  step in the style shown in class, e.g.,

      (len (list x))
  = { def. len, first/rest axioms, def list }
      1

  You can use basic arithmetic facts for free. In the justification,
  write "arithmetic", e.g.,

      (len (rest x)) + 2 + (first x) + 1
  = { Arithmetic }
      (first x) + (len (rest x)) + 3

  You can use the same format for proving inequalities. For example,
  you could prove that (len (cons a b)) > (len b) as follows:

      (len (cons a b))
  = { def. len, if axioms, def. atom, consp axioms }
      (+ 1 (len (rest (cons a b))))
  = { rest-cons axioms }
      (+ 1 (len b))
  > { arithmetic }
      (len b)

You can of course also use previously (in class or in homework) proved
theorems! In this case, cite the theorem in the justification, and
give the substitution that shows how you instantiated the theorem.

We will use the following function definitions:

(defunc atom (a) 
  :input-contract t
  :output-contract (booleanp (atom a))
  (not (consp a)))

(defunc len (a)
  :input-contract t
  :output-contract (natp (len a))
  (if (atom a)
      0
      (+ 1 (len (rest a)))))

(defunc app (a b)
  :input-contract (and (listp a) (listp b))
  :output-contract (and (listp (app a b))
                        (equal (len (app a b))
                               (+ (len a) (len b))))
  (if (endp a)
      b
      (cons (first a) (app (rest a) b))))
  
|#

(defunc list-of-t (n)
  :input-contract (natp n)
  :output-contract (listp (list-of-t n))
  (if (equal n 0)
      nil
      (cons t (list-of-t (- n 1)))))

(defunc factorial (n)
  :input-contract (natp n)
  :output-contract (natp (factorial n))
  (if (equal n 0)
      1
      (* n (factorial (- n 1)))))#|ACL2s-ToDo-Line|#


#|

Recall that for each of the defunc's above we get both a definitional
axiom (input contract => function application = function body), and a
contract theorem (input contract => output contract). Definitional
axioms and contract theorems are there for you to use, as ACL2s has
accepted these functions.

[a]

Prove the following conjecture using equational reasoning:

(implies (and (natp n)
              (> n 0)
              (equal (len (list-of-t (- n 1)))
                     (- n 1)))
         (equal (len (list-of-t n)) n))

; Base-Case

(implies (equal n 0)
         (equal (len (list-of-t n)) n))
         
Context:
         
c1. n = 0

Proof:

(len (list-of-t n))
=  {c1}

(len (list-of-t 0))
=  {def. list-of-t}
   
(len nil)
=  {def. len}
   
0
=  {arith., c1}
   
n = n

; Inductive-Case

(natp n) /\ (> n 0) /\ (len (list-of-t (- n 1))) = (- n 1))) => (len (list-of-t n)) = n

Context:

c1. (natp n)
c2. (> n 0)
c3. (natp (- n 1)) => (len (list-of-t (- n 1))) = (- n 1)

Extended Context:

c4. (natp (- n 1))
   {c1, c2}
c5. (len (list-of-t (- n 1))) = (- n 1)
   {c4, MP}
   
Proof:

(len (list-of-t n))
=  {c1, c4, def. list-of-t}

(len (cons t (list-of-t (- n 1))))
=  {def. len}
   
1 + (len (list-of-t (- n 1)))
=  {c4}

1 + (- n 1)
= {arith.}
  
n = n

|#

#|

[b1]

Prove the following conjecture using equational reasoning:

(implies (and (natp m)
              (natp n))
         (and (implies (equal m 0)
                       (equal (app (list-of-t m)
                                   (list-of-t n))
                              (list-of-t (+ m n))))
              (implies (and (> m 0)
                            (equal (app (list-of-t (- m 1))
                                        (list-of-t n))
                                   (list-of-t (+ (- m 1) n))))
                       (equal (app (list-of-t m)
                                   (list-of-t n))
                              (list-of-t (+ m n))))))

Hint: break the conjecture into two parts, and prove them
independently. For both parts, be careful with the context: e.g., you
have to take into account the hypotheses that the outer implication
enforces.

Alpha /\ Beta

Alpha = (natp m) /\ (natp n) /\ (equal m 0) => (app (list-of-t m) (list-of-t n)) = (list-of-t (+ m n))
                              
Beta = (natp m) /\ (natp n) /\ (> m 0) /\ (app (list-of-t (- m 1)) (list-of-t n)) = (list-of-t (+ (- m 1) n)) =>
          (app (list-of-t m) (list-of-t n)) = (list-of-t (+ m n))
                              


Alpha:

context:

c1. (natp m)
c2. (natp n)
c3. (equal m 0)

proof:

(app (list-of-t m) (list-of-t n))
=  {c3}                                      

(app (list-of-t 0) (list-of-t n))         
=  {def. list-of-t}                         
   
(app nil (list-of-t n))                   
=  {def. app}
   
(list-of-t n)  
=  {arith.}
   
(list-of-t (+ 0 n))
=  {c3}
   
(list-of-t (+ m n)) = (list-of-t (+ m n))

Beta:

Context:

c1. (natp m)
c2. (natp n)
c3. (> m 0)
c4. (natp (- m 1)) /\ (natp n) => (app (list-of-t (- m 1)) (list-of-t n)) = (list-of-t (+ (- m 1) n))
           
Extended Context

c5. (natp (- m 1))  
   {c1, c3}

c6. (app (list-of-t (- m 1)) (list-of-t n)) = (list-of-t (+ (- m 1) n)) 
   {c2, c4, c5, MP}

Proof:
           
(app (list-of-t m) (list-of-t n))                 
=  {def. list-of-t, c1, c3, c5}
   
(app (cons t (list-of-t (- m 1)) (list-of-t n))
=  {def. app}
   
(cons t (app (list-of-t (- m 1)) (list-of-t n)))
=  {c6}
   
(cons t (list-of-t (+ (- m 1) n)))
   {def. list-of-t}
   
(list-of-t (+ 1 (- m 1) n))
   {arith.}
   
(list-of-t (+ m n))) = (list-of-t (+ m n)))
   
   

[b2]

As we will see in class soon, the conjecture in part [b1], which
is now a theorem, corresponds to our proof obligation when
proving the following theorem by induction:


(implies (and (natp n)
              (natp m))
         (equal (app (list-of-t m)
                     (list-of-t n))
                (list-of-t (+ m n))))

Assume that you proved the theorem above. After all, you did all the
hard work! Prove the following conjecture using equational reasoning:

(implies (and (natp k)
              (natp m)
              (natp n))
         (equal (app (list-of-t k)
                     (app (list-of-t m)
                          (list-of-t n)))
                (list-of-t (+ k (+ m n)))))

(natp k) /\ (natp m) /\ (natp n) => (app (list-of-t k) (app (list-of-t m) (list-of-t n))) = (list-of-t (+ k (+ m n)))

Theorems:

thm1. (natp n) /\ (natp m) => (app (list-of-t m) (list-of-t n)) = (list-of-t (+ m n))

Context:

c1. (natp k)
c2. (natp m)
c3. (natp n)

Extended Context:

c4. (natp (+ m n))
   {c2, c3, def. nat, def. +}

Proof:

(app (list-of-t k) (app (list-of-t m)(list-of-t n)))
=  {thm1, c2, c3}
   
(app (list-of-t k) (list-of-t (+ m n))) 
=  {thm1, c1, c4}
   
(list-of-t (+ k (+ m n))) = (list-of-t (+ k (+ m n)))

[c]

Prove the following conjecture using equational reasoning:

(implies (and (natp n)
              (natp k))
         (and (implies (equal k 0)
                       (implies (and (> n 0)
                                     (> k 0))
                                (> (factorial (+ n k))
                                   (factorial n))))
              (implies (equal k 0)
                       (implies (implies (and (> n 0)
                                              (> k 0))
                                         (> (factorial (+ n k))
                                            (factorial n)))
                                (implies (and (> n 0)
                                              (> (+ k 1) 0))
                                         (> (factorial (+ n (+ k 1)))
                                            (factorial n)))))
              (implies (> k 0)
                       (implies (implies (and (> n 0)
                                              (> k 0))
                                         (> (factorial (+ n k))
                                            (factorial n)))
                                (implies (and (> n 0)
                                              (> (+ k 1) 0))
                                         (> (factorial (+ n (+ k 1)))
                                            (factorial n)))))))

For your information, the above conjecture corresponds to our
proof obligation when proving the following theorem by induction:

(implies (and (natp n)
              (natp k)
              (> n 0)
              (> k 0))
         (> (factorial (+ n k)) (factorial n)))
         
Alpha /\ Beta /\ Gamma

Alpha = (natp n) /\ (natp k) /\ (k = 0) => ((> n 0) /\ (> k 0) => (> (factorial (+ n k)) (factorial n)))

Context:

c1. (natp n)
c2. (natp k)
c3. k = 0

((> n 0) /\ (> k 0) => (> (factorial (+ n k)) (factorial n)))
   {c3}
   
((> n 0) /\ (> 0 0) => (> (factorial (+ n k)) (factorial n)))
   {arith.}
   
((> n 0) /\ nil => (> (factorial (+ n k)) (factorial n)))
   {arith.} 
   
(nil => (> (factorial (+ n k)) (factorial n)))
   {arith.}
   
t
   
Alpha : t for all cases
                             
Beta = (natp n) /\ (natp k) /\ (k = 0) =>
                (((> n 0) /\ (> k 0) => (> (factorial (+ n k)) (factorial n))) =>
                ((> n 0) /\ (> (+ k 1) 0) => (> (factorial (+ n (+ k 1))) (factorial n))))

Context:

c1. (natp n)
c2. (natp k)
c3. k = 0
c4. (> n 0)
c5. (> (+ k 1) 0)
   
Extended Context:

c6. (natp (+ n (+ k 1)))
=  {c1, c2, def. +, def. nat}

((> n 0) /\ (> k 0) => (> (factorial (+ n k)) (factorial n))) =>
                ((> n 0) /\ (> (+ k 1) 0) => (> (factorial (+ n (+ k 1))) (factorial n)))
= {c3}
   
((> n 0) /\ (> 0 0) => (> (factorial (+ n 0)) (factorial n))) =>
                ((> n 0) /\ (> (+ 0 1) 0) => (> (factorial (+ n (+ 0 1))) (factorial n)))
=  {arith.}

((> n 0) /\ nil => (> (factorial (+ n 0)) (factorial n))) =>
                ((> n 0) /\ (> (+ 0 1) 0) => (> (factorial (+ n (+ 0 1))) (factorial n)))
=  {arith.}

(nil => (> (factorial (+ n 0)) (factorial n))) =>
                ((> n 0) /\ (> (+ 0 1) 0) => (> (factorial (+ n (+ 0 1))) (factorial n)))
=  {arith.}

t => ((> n 0) /\ (> (+ 0 1) 0) => (> (factorial (+ n (+ 0 1))) (factorial n)))
=  {arith., MP}

(> n 0) /\ (> (+ 0 1) 0) => (> (factorial (+ n (+ 0 1))) (factorial n))
=  {arith.}
   
(factorial (+ n (+ 0 1)))
=  {arith.}
   
(factorial (+ n 1))
=  {def. factorial}
   
(* (+ n 1) (factorial (- (+ n 1) 1)))
=  {arith.}
   
(* (+ n 1) (factorial n))         >     (factorial n)
         / (factorial n)              / (factorial n)
=  {arith.}
     
(+ n 1) > 1
   {arith.}
   
n > 0
   {c4}


                                     
Gamma = (natp n) /\ (natp k) /\ (> k 0) =>
                 (((> n 0) /\ (> k 0) => (> (factorial (+ n k)) (factorial n))) =>
                 ((> n 0) /\ (> (+ k 1) 0) => (> (factorial (+ n (+ k 1))) (factorial n))))    

Context:

c1. (natp n)
c2. (natp k)
c3. (> k 0)
c4. (> n 0)
c5. (> n 0) /\ (> k 0) => (> (factorial (+ n k)) (factorial n))

Extended Context:

c6. (natp (+ n (+ k 1)))
   {c1, c2}
c7. (> (factorial (+ n k)) (factorial n))
   {c3, c4, MP}
   
(factorial (+ n (+ k 1))) > (factorial n))
= {arith.}
  
(factorial (+ n k 1)) 
= {def. factorial}
                     
(* (+ n k 1) (factorial (- (+ n k 1) 1)))
= {arith.}
  
(* (+ n k 1) (factorial (+ n k))) > (factorial n)
= {c3, c4, c7}

(* (+ n k 1) (factorial (+ n k))) > (factorial (+ n k) > (factorial n)

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Feedback
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

Please fill out the following feedback form, not as a team, but each team
member individually:

https://docs.google.com/spreadsheet/viewform?formkey=dFpfTDFKeUVOQ0x6RkJEUFhWOVJoanc6MA

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
