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
CS 2800 Homework 7 - Spring 2013

Student names:

For this homework, you will be using the BEGINNER mode in ACL2s.

Instructions for using this file:

- Open this file in ACL2s as hw07.lisp .

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
    admitted.  If any of your own tests fail, it may be that your
    function is wrong, but it may also be that your check=
    specification is incorrect.

  Sometimes it may take a while for your function to be admitted. Be
  patient: rest assured that, if your function is correct, the process
  will succeed. If you don't get a success message after 2-3
  mins (depending on your hardware), then probably your definition is
  too complicated or wrong.

  If you remain convinced that your definition is correct, just
  put :program somewhere towards the beginning of the file, e.g.,
  right here:

|#

; :program

#|

  It will turn off static verification of contracts and termination
  analysis.

- Save the file with your additions and turn it in. To do that, check
  out the directory at

  svn://dell-server-wahl-1.ccs.neu.edu/cs2800-2013-spring/07/<groupname>

  where groupname is the name of the group as it was emailed to
  you. Your username and password remain as before.

  Place your file hw07.lisp in this directory, add it, and commit. Do
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Preliminaries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; acl2::test?

#| 

acl2::test? takes a conjecture as input and runs a number of tests to
see whether it can find a counterexample. For example, let us
conjecture that, for positive natural numbers x and y, (x + y)
and (x * y) are different. You can test this conjecture like this:

  (acl2::test?
   (implies (and (posp x) (posp y))
            (not (equal (+ x y) (* x y)))))

acl2::test? may report that it succeeded: no counterexamples were
found. This does not mean you have proved the conjecture! It just
means test? hasn't come across a test that falsifies it. In fact, the
above conjecture is wrong. You can try to find more counterexamples by
increasing the number of tests run, like this:

|#
  
(acl2::acl2s-defaults :set acl2::num-trials 2500)

#|
Now try acl2::test? again!

In general, make sure to perform conjecture contract closure before
passing your conjecture to acl2::test? . For example,

  (acl2::test? (equal (append x y) (append y x)))

will waste time running many useless tests in which x and y are not
lists, and report contract violations. In contrast,

  (acl2::test?
   (implies (and (listp x) (listp y))
            (equal (append x y) (append y x))))

will return a counterexample using lists immediately.

|#

;; Tail-Recursion

#|

A function is said to be tail recursive, if there is nothing to do
after a recursive call occurs, except return its value.

Implementing a tail-recursive function usually amounts to adding an
extra "accumulator" argument that keeps track of the results so far.
For example, here is a tail-recursive implementation of the factorial
function:

|#

(defunc factorial-aux (n acc)
  :input-contract (and (natp n) (natp acc))
  :output-contract (natp (factorial-aux n acc))
  (if (equal n 0)
    acc
    (factorial-aux (- n 1) (* n acc))))

#|

As you can probably guess, to compute factorial of n we call
factorial-aux with acc = 1 . Here is the non-tail-recursive
definition from Homework 6:

|#

(defunc factorial (n)
  :input-contract (natp n)
  :output-contract (natp (factorial n))
  (if (equal n 0)
    1
    (* n (factorial (- n 1)))))

(acl2::test?
 (implies (natp n)
          (equal (factorial-aux n 1) (factorial n))))

#|

Notice that in factorial-aux, there is nothing to do once we return
from the recursive call, while in the HW6 definition we perform a
multiplication.

|#

;; Standard Definitions

#|

We will use the following definitions throughout this homework. 
(They're already defined in Beginner mode.)

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

(defunc in (a X)
  :input-contract (listp x)
  :output-contract (booleanp (in a X))
  (if (endp x)
      nil
    (or (equal a (first X))
        (in a (rest X)))))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1. Specification-Driven Programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

In this part of the homework, you will have to write programs that
meet given specifications. Specifications will come in two forms.
First, you will be given input and output contracts. Additionally, we
will use te acl2::test? mechanism.

Consider the following definition:

|#

(defunc count-list (a x)
  :input-contract (listp x)
  :output-contract (natp (count-list a x))
  (if (endp x)
    0
    (+ (if (equal (first x) a) 1 0)
       (count-list a (rest x)))))

#|

[a]

Provide a tail-recursive implementation of count-list:

|#

(defunc count-list-aux (a x acc)
  :input-contract (and (listp x) (natp acc))
  :output-contract (natp (count-list-aux a x acc))
  (if (endp x)
    acc
    (count-list-aux a 
                    (rest x)
                    (+ (if (equal (first x) a) 1 0) acc))))


#|

Make sure the following definitions and tests go through:

|#

(defunc count-list1 (a x)
  :input-contract (listp x)
  :output-contract (natp (count-list1 a x))
  (count-list-aux a x 0))

(acl2::test?
 (implies (and (listp x)
               (equal (count-list1 a x) 0))
          (not (in a x))))

(acl2::test?
 (implies (and (listp x)
               (listp y))
          (<= (count-list1 a x)
              (count-list1 a (app x y)))))

#|

In fact, we can have a very precise specification of
count-list1 (which just calls the tail-recursive function): it
should be equal to count-list ! Specifications are not always that
strict, as you will see later.

Make sure the following test goes through:

|#

(acl2::test?
 (implies (and (natp a)
               (listp x))
          (equal (count-list1 a x)
                 (count-list a x))))

#|

[b]

We are not really done with count-list1 ! It differs from count-list 
in a very important way. To see this, first define a tail-recursive
function f-aux that constructs a list of a given length.

We will use this function to provide "malicious" (i.e., really long)
inputs for count-list and count-list1 .

|#

(defunc f-aux (n acc)
  :input-contract (and (natp n) (listp acc))
  :output-contract (and (listp (f-aux n acc))
                        (equal (len (f-aux n acc))
                               (+ (len acc) n)))
  (if (equal n 0)
    acc
    (f-aux (- n 1) (cons t acc))))

(defunc f (n)
  :input-contract (natp n)
  :output-contract (equal (len (f n)) n)
  (f-aux n nil))

#|

For different natural numbers n , try

  (count-list 1 (f n))

in the ACL2s prompt. 

What is the largest number, x, for which the count-list expression above
can be evaluated? 

52599


Note: this can change on different runs so a number in the right
ballpark is OK.

Now try evaluating 

  (count-list1 1 (f n))

in the ACL2s prompt. 

Can you evaluate the count-list1 expression above with n=100x?

Yes

We tried 10000000000000000000000000000000000000000000

Explain what is going on. If you do not know what is going on, 
do some research on tail recursion and cite your sources in the answer below.

Stack Overflow will never happen because it is a tail recursition

|#

#|

[c]

In this part, we will deal with trees. A tree is a collection of
nodes, starting at a root node. Each node consists of a value (of any
type), along with a left and right node ("children"). A node can also
be nil. We use the terms "tree" and "node" interchangeably: a node "n"
can be viewed as the tree with "n" as the root. Here is the ACL2s
definition:

|#

(defdata tree
  (oneof nil
         (list tree all tree)))

#|

For example, the following tree:

      1
     / \
    3   5
   / \   \
  2   0   3

is represented in ACL2 as follows (defconst just gives a name to a
constant):

|#

(defconst *some-tree*
  (list (list (list nil
                    2
                    nil)
              3
              (list nil
                    0
                    nil))
        1
        (list nil
              5
              (list nil
                    3
                    nil))))
  
(acl2::test? (treep *some-tree*))

#|

To prove facts about trees, you can rely on the fact that if both
(treep r) and (consp r) hold, then r is a list with exactly three
elements. Also,

  (treep r) /\ (consp r) => (treep (first r))

and
 
  (treep r) /\ (consp r) => (treep (third r)) .

Finally,

  (treep r) => (listp r) .

Do not comment the theorems below. They will be required for
contract and termination proofs in what follows.

|#


(defthm consp-rest-tree
  (implies (and (treep r)
                (consp r))
           (consp (acl2::rest r)))
  :rule-classes (:type-prescription :forward-chaining))

(defthm consp-rest-rest-tree
  (implies (and (treep r)
                (consp r))
           (consp (acl2::rest (acl2::rest r))))
  :rule-classes (:type-prescription :forward-chaining))

(defthm tree-first
  (implies (and (treep r)
                (consp r))
           (treep (acl2::first r)))
  :rule-classes (:forward-chaining))

(defthm tree-third
  (implies (and (treep r)
                (consp r))
           (treep (acl2::third r)))
  :rule-classes (:forward-chaining))

(acl2::in-theory (acl2::disable acl2::integerp-minus-x))

#|

Here are a few helpful functions to deal with trees.

|#

(defunc tree-size (r)
  :input-contract (treep r)
  :output-contract (natp (tree-size r))
  (if (endp r)
    0
    (+ 1
       (+ (tree-size (first r))
          (tree-size (third r))))))

(defunc in-tree (a r)
  :input-contract (treep r)
  :output-contract (booleanp (in-tree a r))
  (if (endp r)
    nil
    (or (equal (second r) a)
        (in-tree a (first r))
        (in-tree a (third r)))))

(defunc count-tree (a r)
  :input-contract (treep r)
  :output-contract (natp (count-tree a r))
  (if (endp r)
    0
    (+ (if (equal (second r) a) 1 0)
       (+ (count-tree a (first r))
          (count-tree a (third r))))))

#|

There are multiple ways to walk through the nodes of a tree. Here is
a function that walks through a tree in some particular way, visits
each node once, and collects all values:

|#

(defunc tree-to-list (r)
  :input-contract (treep r)
  :output-contract (listp (tree-to-list r))
  (if (endp r)
    nil
    (app (cons (second r)
               (tree-to-list (first r)))
         (tree-to-list (third r)))))

#|

For the tree given above ( *some-tree* ), tree-to-list returns
(1 3 2 0 5 3) . Try it! This is not the only possible answer, e.g.,  
(2 3 0 1 5 3) would be perfectly fine. A value can (and should!)
appear multiple times in the output, if it appears in multiple
nodes of the tree. For instance, (2 3 0 1 5) would not be an
acceptable answer.

The formal properties below correspond to our informal specification
above.

|#

(acl2::test?
 (implies (and (treep r)
               (in-tree a r))
          (in a (tree-to-list r))))

(acl2::test?
  (implies (treep r)
           (equal (count-tree a r)
                  (count-list a (tree-to-list r)))))

#|

Provide another implementation of tree-to-list that visits
nodes in a different order. The new function should satisfy the
same properties as tree-to-list ; in other words, the tests below
the definition should pass!

|#

(defunc tree-to-list1 (r)
  :input-contract (treep r)
  :output-contract (listp (tree-to-list1 r))
  (if (endp r)
    nil
    (app (tree-to-list1 (first r))
         (cons (second r)
               (tree-to-list1 (third r))))))


(check= (equal (tree-to-list1 *some-tree*) 
               (tree-to-list *some-tree*))
        nil)

(acl2::test?
 (implies (and (treep r)
               (in-tree a r))
          (in a (tree-to-list1 r))))

(acl2::test?
  (implies (treep r)
           (equal (count-tree a r)
                  (count-list a (tree-to-list1 r)))))

#|

Now, provide a tail-recursive implementation.

Here is how to achieve this: one of your arguments will be a list
of subtrees to be visited (a list of trees). Another argument will
collect the naturals, just as before. Instead of recurring on
subtrees, you put them in the list. You recur on the list, until
it gets empty.

Fill in the definition of tree-to-list2-aux, and make sure all
definitions and tests below it work. 

|#

(defdata treelist (listof tree))

(defunc tree-to-list2-aux (l acc)
  :input-contract (and (treelistp l) (listp acc))
  :output-contract (listp (tree-to-list2-aux l acc))
  (if (endp l)
    acc
    (if (endp (first l))
      (tree-to-list2-aux (rest l) acc)
    (tree-to-list2-aux (cons (first (first l))
                             (cons (third (first l))
                                   (rest l)))
                       (cons (second (first l)) acc)))))

(defunc tree-to-list2 (r)
  :input-contract (treep r)
  :output-contract (listp (tree-to-list2 r))
  (tree-to-list2-aux (list r) nil))

(acl2::test?
 (implies (and (treep r)
               (in-tree a r))
          (in a (tree-to-list2 r))))

(acl2::test?
  (implies (treep r)
           (equal (count-tree x r)
                  (count-list x (tree-to-list2 r)))))

#|

All implementations should meet the properties given, but this
does not mean that they should provide the same answers! Fill in
the dots below, i.e., provide two different input trees on which
the implementations disagree.

|#

(acl2::test?
 (let ((r '((nil 1 nil) 2 nil)))
   (and (treep r)
        (not (equal (tree-to-list r)
                    (tree-to-list1 r))))))

(acl2::test?
 (let ((r '(nil 1 (nil 2 nil))))
   (and (treep r)
        (not (equal (tree-to-list r)
                    (tree-to-list2 r))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2. Conjecture Contract Completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

The conjectures in this Section are incomplete (irrespective of
whether they are true of false): they are lacking hypotheses that make
sure the functions involved are called only on arguments that satisfy
the functions' input contracts. Make the conjectures meaningful by
adding the necessary contract hypotheses. For example, given a
conjecture c , if you think hypotheses h1 and h2 (and nothing else)
are necessary to ensure the input contracts of all functions occurring
in c are satisfied, your answer should be

  (implies (and h1 h2) c)

Simplify the conjecture hypotheses as much as possible. In the above
example, suppose h1 => h2 is valid. Then the conjunction h1 /\ h2 is
equivalent to h1 ; thus, simplify your output to

  (implies h1 c)

Sometimes there is no way to satisfy the input contracts of the
functions that appear in an expression. For example, different
contracts may force some variable to be both a natural number and a
list at the same time. Of course, (natp x) /\ (listp x) is
unsatisfiable. If you encounter a conjecture where this happens, then
the conjecture is meaningless, since there is no way to satisfy its
hypotheses; just comment out the conjecture and write a comment
explaining why.

[a]

Add the required hypotheses to the following conjectures, by filling
in the dots. We re-use functions from Part 1.

1.

(implies (natp n)
         (equal (factorial-aux n 1)
                (factorial n)))

2.
                
(implies nil
         (equal (len (tree-to-list2-aux q (tree-size r)))
                (tree-size r)))
                
tree-size outputs a nat but the second arguement to tree-to-list2-aux  has to be a list

3.

(implies (and (treep a) (treep c))
         (equal (tree-size (list a b c))
                (+ (tree-size a)
                   (+ 1 (tree-size b)))))

4.

(implies (and (treep r) (rationalp k)
         (and (equal (len r) k)
              (> (count-tree a r) k)))

5.

(implies (treep r)
         (not (in r (tree-to-list r))))

6.

(implies nil
         (equal (tree-size (cons a b))
                (+ (tree-size a)
                   (tree-size b))))
                   
b has to be (cons any tree) for (tree-size (cons a b)) to work
and also has to be tree for (tree-size b)

7.

(implies (treep r)
         (> (count-tree a r) (count-list a r)))

8.

(implies (and (consp r) (treep r))
         (<= (+ (count-tree a (first r)) (count-tree a (third r)))
             (count-tree a r)))
                   
[b]

Test the conjectures you produced above, by using acl2::test? .
Ignore the non-meaningful conjectures, i.e., the ones that inevitably
lead to a contract violation. 

Which of the functions are candidate theorems? Provide counterexamples
for the rest.

|#

(acl2::test? (implies (natp n)
                      (equal (factorial-aux n 1)
                             (factorial n))))
#|
(acl2::test? (implies (and (treep a) (treep c))
                      (equal (tree-size (list a b c))
                             (+ (tree-size a)
                                (+ 1 (tree-size b))))))

 -- (A (((NIL NIL NIL) T NIL) 0 NIL)), (B 0) and 
(C ((((NIL NIL NIL) NIL (NIL NIL NIL))
     0 NIL)
    (#\a)
    ((NIL NIL NIL) NIL NIL)))

 -- 
(A (((((NIL NIL NIL) NIL NIL) T NIL)
     (#\a)
     (NIL T NIL))
    5 ((NIL NIL (NIL NIL NIL)) T NIL))),
(B "a") and (C ((NIL NIL NIL) NIL (NIL NIL NIL)))
 -- (A ((NIL NIL NIL) NIL NIL)), (B T) and 
(C ((((NIL NIL NIL) NIL (NIL NIL NIL)) T NIL) ACL2::|a| NIL))


(acl2::test? (implies (and (treep r) (rationalp k))
                           (and (equal (len r) k)
                                (> (count-tree a r) k))))

(R ((((NIL NIL NIL) T (NIL NIL NIL)) NIL NIL) "a" ((NIL NIL NIL) T NIL))),
(K -1/3) and (A (0 . 21))
 -- (R (((NIL NIL (NIL NIL NIL)) NIL ((NIL NIL NIL) NIL NIL)) T NIL)),
(K -4) and (A NIL)
 -- (R (((NIL NIL NIL) NIL (NIL NIL NIL)) T ((NIL NIL NIL) NIL NIL))),
(K -2) and (A T)


(acl2::test? (implies (and (treep r) (rationalp k)
                           (and (equal (len r) k)
                                (> (count-tree a r) k)))))

(R ((((NIL NIL NIL) T (NIL NIL NIL)) NIL NIL) "a" ((NIL NIL NIL) T NIL))),
(K -1/3) and (A (0 . 21))
 -- (R (((NIL NIL (NIL NIL NIL)) NIL ((NIL NIL NIL) NIL NIL)) T NIL)),
(K -4) and (A NIL)
 -- (R (((NIL NIL NIL) NIL (NIL NIL NIL)) T ((NIL NIL NIL) NIL NIL))),
(K -2) and (A T)
|#

(acl2::test? (implies (treep r)
                      (not (in r (tree-to-list r)))))

(acl2::test? (implies (and (consp r) (treep r))
                      (<= (+ (count-tree a (first r)) (count-tree a (third r)))
                          (count-tree a r))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 3. Equational Reasoning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

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

We will re-use definitions from parts 1 and 2 of this homework.

Recall that for each of the defunc's above we get both a definitional
axiom (input contract => function application = function body), and a
contract theorem (input contract => output contract). 

You can freely use both definitional axioms and contract theorems in 
what follows, as ACL2s has accepted these functions.

|#

; Theorem 1

(acl2::test? (implies (and (listp y)
                           (listp z))
                      (equal (app (cons x y) z) (cons x (app y z)))))

; Theorem 2

(acl2::test? (implies (and (listp x)
                           (listp y))
                      (equal (len (app x y)) (+ (len x) (len y)))))#|ACL2s-ToDo-Line|#


#|

[a]

Prove the following conjecture using equational reasoning:

(implies (treep r)
         (and (implies (and (consp r)
                            (equal (len (tree-to-list (first r)))
                                   (tree-size (first r)))
                            (equal (len (tree-to-list (third r)))
                                   (tree-size (third r))))
                       (equal (len (tree-to-list r))
                              (tree-size r)))
              (implies (endp r)
                       (equal (len (tree-to-list r))
                              (tree-size r)))))
                              
Hints:

- Use Theorem 3.1 from the lecture notes. We quote it for your
  convenience:

    (listp y) /\ (listp z) =>
    (app (cons x y) z) = (cons x (app y z))
  
- Exploit the output contract of app, which relates app and len .

Conjecture: A /\ B

A: (treep r) /\ (consp r) /\ ((len (tree-to-list (first r))) = (tree-size (first r))) /\ ((len (tree-to-list (third r))) = (tree-size (third r))) =>
      ((len (tree-to-list r)) = (tree-size r))
    
B: (treep r) /\ (endp r) => ((len (tree-to-list r)) = (tree-size r))

A:
Context:

c1. (treep r)
c2. (consp r)
c3. ((len (tree-to-list (first r))) = (tree-size (first r)))
c4. ((len (tree-to-list (third r))) = (tree-size (third r)))

Extended Context:

c5. (treep (first r))
c6. (treep (third r))

Proof:

(len (tree-to-list r))
=  {def. tree-to-list, c2}

(len (app (cons (second r) (tree-to-list (first r))) (tree-to-list (third r))))
=  {thm. 1, def. len}

1 + (len (app (tree-to-list (first r)) (tree-to-list (third r))))
=  {thm. 2}

1 + (len (tree-to-list (first r))) + (len (tree-to-list (third r)))
=  {c3,c4}

1 + (tree-size (first r)) + (tree-size (third r))
=  {def. tree-size}

(tree-size r) = (tree-size r)

B:

Context:

c1. (treep r)
c2. (endp r)

Extended Context:

c3. r = nil

Proof:

(len (tree-to-list r))
=  {c3, def. tree-to-list, def. len}

0
=  {def. tree-size}

(tree-size r) = (tree-size r)

|#


#|
    
[b]

Prove the following conjecture using equational reasoning:

(implies (and (listp x) (listp y))
         (and (implies (endp x)
                       (equal (count-list a (app x y))
                              (+ (count-list a x)
                                 (count-list a y))))
              (implies (and (consp x)
                            (equal (count-list a (app (rest x) y))
                                   (+ (count-list a (rest x))
                                      (count-list a y))))
                       (equal (count-list a (app x y))
                              (+ (count-list a x)
                                 (count-list a y))))))

A /\ B

A: (listp x) /\ (listp y) /\ (endp x) => ((count-list a (app x y)) = (+ (count-list a x) (count-list a y)))

B: (listp x) /\ (listp y) /\ (consp x) /\ ((count-list a (app (rest x) y)) = (+ (count-list a (rest x)) (count-list a y))) =>
      ((count-list a (app x y)) = (+ (count-list a x) (count-list a y)))
      
A:

Context:

c1. (listp x)
c2. (listp y)
c3. (endp x)

Extended Context:

c4. x = nil

Proof:

(count-list a (app x y))
=  {c4, def. app, arith}

0 + (count-list a y)
=  {def. count-list}

(+ (count-list a x) (count-list a y))) = (+ (count-list a x) (count-list a y)))

B:

Context:

c1. (listp x)
c2. (listp y)
c3. (consp x)
c4. (count-list a (app (rest x) y)) = (+ (count-list a (rest x)) (count-list a y))

Proof:

(count-list a (app x y))
=  {def. app}

(count-list a (cons (first x) (app (rest x) y))
=  {def. count-list, arith.}

(+ (equal (first x) a) (count-list a (app (rest x) y)))
=  {c4}

(+ (if (equal (first x) a) 1 0) (count-list a (rest x)) (count-list a y))
=  {def. count-list}

(+ (count-list a x) (count-list a y)) = (+ (count-list a x) (count-list a y))


|#

#|

[c]

Prove the following conjecture using equational reasoning:

(and
 (implies (and (treep r) (endp r))
          (iff (> (count-tree a r) 0)
               (in-tree a r)))
 (implies (and (treep r)
               (consp r)
               (iff (> (count-tree a (first r)) 0)
                    (in-tree a (first r)))
               (iff (> (count-tree a (third r)) 0)
                    (in-tree a (third r))))
          (iff (> (count-tree a r) 0)
               (in-tree a r))))
               
Hint: there are multiple ways to prove a bidirectional implication
(iff). You can start from one side and prove that it is equivalent
to the other, very similar to what we do for equalities.
Alternatively, you can prove the two directions of the implication
independently: to prove F <=> G , you need to show that F => G ,
and also that G => F . You could also start with the whole iff
expression, and prove that it is equal to t . You are free to apply
any of these strategies.

A /\ B

C: (> (count-tree a r) 0) <=> (in-tree a r)

A: (treep r) /\ (endp r) => C

B: (treep r) /\ (consp r) /\ ((> (count-tree a (first r)) 0) <=> (in-tree a (first r))) /\ ((> (count-tree a (third r)) 0) <=> (in-tree a (third r))) => C


A:

Context:

c1. (treep r)
c2. (endp r)

Extended Context:

c3. r = nil

Proof:

(> (count-tree a r) 0)
=  {c3, arith}

nil
=  {def. in-tree}

(in-tree a r) <=> (in-tree a r)


B:

Context:

c1. (treep r)
c2. (consp r)
c3. (> (count-tree a (first r)) 0) <=> (in-tree a (first r))
c4. (> (count-tree a (third r)) 0) <=> (in-tree a (third r))

Proof:

(> (count-tree a r) 0)
=  {def. count-tree}

(> (+ (if (equal a (second r)) 1 0) (count-tree a (first r)) (count-tree a (third r))) 0)
=  {arith., contract thm. count-tree}

(> (if (equal a (second r)) 1 0) 0) or (count-tree a (first r)) or (count-tree a (third r))
=  {c3, c4}

(> (if (equal a (second r)) 1 0) 0) or (in-tree a (first r)) or (in-tree a (third r))
=  {arith.}

(equal a (second r) or (in-tree a (first r)) or (in-tree a (third r))
=  {def. in-tree}

(in-tree r) <=> (in-tree r)

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Feedback
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

Please fill out the following form.

https://docs.google.com/spreadsheet/viewform?rm=full&formkey=dE80bmVQSkl5TmtydlF1SzRHNDFmVUE6MA

We do not keep track of who submitted what, so please be honest. Each
individual student should fill out the form, e.g., if there are two
people on a team, then each of these people should fill out the form.

Notice that you can actually fill out the form many times. Don't do
that because we have no idea who submitted what and can't identify
multiple submissions from the same person.

After you fill out the form, write your name below in this file, not
on the questionnaire. We have no way of checking if you submitted the
file, but by writing your name below you are claiming that you did,
and we'll take your word for it.

10 EXP points out of the total 100 for the homework will be awarded if
every member of your team fills out the form. If you cannot get a hold
of a team member, then indicate that and we'll give everyone who
claims to have filled out the questionnaire EXP.

Edwin Cowart
Elias Elgarten
|#
