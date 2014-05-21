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
CS 2800 Homework 5 - Spring 2013

For this homework, you will be using the BEGINNER mode in ACL2s.

Instructions for using this file:

- open this file in ACL2s as hw05.lisp

- set the session mode to "Beginner"

- insert your solutions into this file where indicated (for instance as
"..."). This includes contracts, the body of the function, and test cases.
Make sure you provide as many test cases as required (in addition to the
ones already provided, of course).

- do not comment in or comment out any existing function definitions from
this file. Also, do not change the order of any function definitions,
whether provided by us or defined by you.

- make sure the entire file is admitted by ACL2s in Beginner mode. This means:
  * there must be no "..." in the code. If you don't finish some problems,
    comment them out. The same is true for any English text that you may
    add. This file already contains many comments, so you can see what the
    syntax is.
  * all check= tests must be satisfied. If any of the provided tests fail,
    your function definition is wrong, and the file will not be admitted.
    If any of your own tests fail, it may be your function is wrong, but
    it may also be that your check= specification is incorrect.

  Sometimes it may take a while for your function to be admitted. Be
  patient: rest assured that, if your function is correct, the process will
  succeed. If you don't get a success message after 2-3 mins (depending on
  your hardware), then probably your definition is too complicated or
  wrong.

- save the file with your additions and turn it in. To do that, check out
the directory at

  svn://dell-server-wahl-1.ccs.neu.edu/cs2800-2013-spring/05/<groupname>

  where groupname is the name of the group as it was emailed to you. Your
  username and password remain as before.

  Place your file hw05.lisp in this directory, add it, and commit. Do NOT
  submit the session file (ending in .a2s).

|#

; In this homework we will design a simple SAT checker. It takes a
; propositional expression and prints a satisfying
; assignment if one exists. We then use the SAT checker to build a validity
; checker.

;; We first define functions to evaluate an expression. These functions will
;; be used in the sat solver to create a satisfying assignment incrementally
;; (one variable at a time).

;; Note that this time we work with a different set of binary connectives than
;; in the last homework.

;; First we define propositional expressions. We write
;; negation as '~ , disjunction as '+ , conjunction as '& , equivalence as '==
;; and implication as '=> .

(defdata BinOp (oneof '+ '& '== '=>))
(defdata UnaryOp '~)
(defdata PropEx (oneof boolean symbol
                       (list UnaryOp PropEx)
                       (list Binop PropEx PropEx))
  :type-lemmas t)

; The value type involves boolean and a special error value, which will be
; used as the evaluation result in case variables are not assigned.

(defdata value (oneof boolean 'error))

; Note: any function that you write that recurs on a propositional
; expression px must be based on its data definition. The base cases are
; (symbolp px) and (booleanp px).

; Recall the definitions of the following functions:

(defunc vand (a b)
  :input-contract (and (valuep a) (valuep b))
  :output-contract (valuep (and a b))
  (if (or (equal a 'error) 
          (equal b 'error)) 
      'error 
    (and a b)))

(defunc vor (a b)
  :input-contract (and (valuep a) (valuep b))
  :output-contract (valuep (vor a b))
  (if (or (equal a 'error) 
          (equal b 'error)) 
      'error 
    (or a b)))

(defunc vnot (a)
  :input-contract (valuep a)
  :output-contract (valuep (vnot a))
  (if (equal a 'error)
      'error 
    (not a)))

(defunc viff (a b)
  :input-contract (and (valuep a) (valuep b))
  :output-contract (valuep (viff a b))
  (if (or (equal a 'error) 
          (equal b 'error)) 
      'error 
    (iff a b)))

(defunc vimplies (a b)
  :input-contract (and (valuep a) (valuep b))
  :output-contract (valuep (vimplies a b))
  (if (or (equal a 'error) 
          (equal b 'error)) 
      'error 
    (if a b t)))

;; The following lemmas are required for proving the termination, contract
;; and the body contract of the functions. Please do not delete or comment
;; them out.

(defthm consp-propexp
  (implies (and (not (symbolp s))
                (PropExp s))
           (consp s))
  :rule-classes :forward-chaining)

(defthm consp-rest-propexp
  (implies (and (PropExp s)
                (not (symbolp s)))
           (consp (acl2::rest s)))
  :rule-classes (:type-prescription :forward-chaining))

(defthm consp-rest-rest-propexp
  (implies (and (PropExp s)
                (not (symbolp s))
                (not (equal (acl2::first s) '~)))
           (consp (acl2::rest (acl2::rest s))))
  :rule-classes (:type-prescription :forward-chaining))

(acl2::in-theory (acl2::disable valuep-definition-thm 
                                BinOpp-definition-thm))

; list of symbols
(defdata LoSymbol (listof symbol))

; add : symbol x LoSymbol

; (add a X): if a is not in X add symbol a to the front of X. Otherwise
; return X.
; Use the function 'in' to check if a given element is in X.
; Write at least five tests.

(defunc add (a X)
  :input-contract (and (symbolp a) (LoSymbolp X))
  :output-contract (LoSymbolp (add a X))
  (if (in a X) X
    (cons a X)))

(check= (add 'a nil) '(a))
(check= (add 'a '(a b)) '(a b))
(check= (add 'a '(b a)) '(b a))
(check= (add 'a '(a a)) '(a a))
(check= (add 'a '(b b c)) '(a b b c))
(check= (add 'a '(a)) '(a))
(check= (add 'a '(a a b a)) '(a a b a))
(check= (add 'a '(a a a a)) '(a a a a))
; get-vars : PropEx x LoSymbol -> LoSymbol

; (get-vars px acc) returns the list of variables appearing in the
;  expression `px`, including those in the provided accumulator `acc'.
;  Write at least five checks.

(defunc get-vars (px acc)
  :input-contract (and (PropExp px) (LoSymbolp acc))
  :output-contract (LoSymbolp (get-vars px acc))
  (cond ((booleanp px) acc)
        ((symbolp px) (add px acc))
        ((UnaryOpp (first px)) (get-vars (second px) acc))
        (t (get-vars (second px) (get-vars (third px) acc)))))

(check= (get-vars 'A '()) '(A))
; Whether the following tests pass depends on the order in which you return
; the variables (which is intentionally left unspecified).
; (check= (get-vars 'A '(B C)) '(A B C))
; (check= (get-vars '(& A B ) '()) '(B A))
; (check= (get-vars '(& (& A B) (+ C D) ) '()) '(D C B A))

(check= (get-vars 'A '(B C)) '(A B C))
(check= (get-vars '(& A B ) '()) '(A B))
(check= (get-vars '(& (& A B) (+ C D) ) '()) '(A B C D))
(check= (get-vars '(~ A) '()) '(A))
(check= (get-vars nil nil) nil)


; bval : PropEx -> value

; (bval px) evaluate boolean expression px. If px contains an unassigned
; variable, the function returns the symbol 'error. Use the function vand,
; vor, vimplies, vnot and viff.
; Write at least five checks.

(defunc bval (px)
  :input-contract (PropExp px)
  :output-contract (valuep (bval px))
  (cond ((booleanp px) px)
        ((symbolp px) 'error)
        ((UnaryOpp (first px)) (vnot (bval (second px))))
        ((equal (first px) '+) (vor (bval (second px)) (bval (third px))))
        ((equal (first px) '&) (vand (bval (second px)) (bval (third px))))
        ((equal (first px) '==) (viff (bval (second px)) (bval (third px))))
        (t (vimplies (bval (second px)) (bval (third px))))))


(check= (bval T) T)
(check= (bval NIL) NIL)
(check= (bval '(& T NIL)) NIL)
(check= (bval '(& T  T)) T)
(check= (bval '(+ T NIL )) T)
(check= (bval '(~ T)) NIL)
(check= (bval '(+ (& NIL  (~ NIL)) NIL)) NIL)
(check= (bval '(+ (& T (~ NIL)) T)) T)
(check= (bval '(+ (& NIL (~ NIL)) T)) T)
(check= (bval '(== T NIL)) NIL)
(check= (bval '(== T T)) T)
(check= (bval '(== NIL NIL)) T)
(check= (bval '(+ t x)) 'error)
(check= (bval '(+ (& NIL (~ x)) T)) 'error)

(check= (bval '(+ T T)) T)
(check= (bval '(+ (~ NIL) T)) T)
(check= (bval '(+ (~ NIL) (& T T))) T)
(check= (bval '(+ NIL (+ NIL (+ NIL (+ NIL (+ NIL (+ NIL T))))))) T)
(check= (bval '(~ (+ NIL (+ NIL (+ NIL (+ NIL (+ NIL (+ NIL T)))))))) NIL)



; Data definition for Assignment. The type Assign contains pairs of a
; symbol and the boolean value assigned to it.
(defdata Assign (list symbol boolean))
(defdata LoAssign (listof Assign))

;data definition for result of sat-solver : a list of
;assignments, 'unsat , 'valid , or 'error
(defdata SatisfyResult (oneof LoAssign 'unsat 'valid 'error))

; bind : PropEx x symbol x boolean -> PropEx

; (bind px name val) replaces the variable name in px with value val
; (that is, it binds the variable).
; Write at least five checks.

(defunc bind (px name val)
  :input-contract (and (PropExp px) (symbolp name) (booleanp val))
  :output-contract (PropExp (bind px name val))
  (cond ((booleanp px) px)
        ((and (symbolp px) (equal name px)) val)
        ((symbolp px) px)
        ((UnaryOpp (first px)) (list (first px) (bind (second px) name val)))
        (t (list (first px) (bind (second px) name val) (bind (third px) name val)))))
        
        
(check= (bind T 'A NIL) T)
(check= (bind NIL 'A T) NIL)
(check= (bind 'A 'B T) 'A)
(check= (bind 'A 'A NIL) NIL)
(check= (bind '(& (+ NIL  A)  (~ B) ) 'A T) '(& (+ NIL  T) (~ B)))
(check= (bind '(~ A) 'A NIL) '(~ NIL))
(check= (bind '(+ (~ A) (~ B)) 'A NIL) '(+ (~ NIL) (~ B)))
(check= (bind '(+ (~ A) (~ B)) 'C T) '(+ (~ A) (~ B)))
(check= (bind '(+ (~ A) (~ B)) 'B T) '(+ (~ A) (~ T)))
(check= (bind '(+ (~ A) (~ A)) 'A T) '(+ (~ T) (~ T)))


(acl2::in-theory (acl2::disable SatisfyResultp-definition-thm))

;   satisfy-remain : PropEx x LoSymbol x LoAssign -> SatisfyResult

;; Function satisfy-remain takes a propositional expression px, a list vars of
;; variables, and a list bindings of assignments to variables. The goal of
;; this function is to find assignments to all unbound variables in px such
;; that px evaluates to T. If such an assignment exists, it is returned. If no
;; such assignment exists, the function returns 'unsat.

;; This can be accomplished as follows. Let us assume that all variables
;; appearing in px are contained in vars; if we find during execution that
;; this assumption is violated, we return 'error. The function now tries all
;; possible assignments to the variables in vars (which, by the assumption,
;; includes all the variables unbound in px), binding the variables in px
;; according to these assignments, and checking whether any of the resulting
;; expressions evaluates to T. Specifically:

;; If vars is empty, we have no more variables to assign. Now evaluate px,
;; using one of the evaluation functions defined earlier. There are 3
;; different outcomes; for each of these decide what the return value of
;; satisfy-remain should be.

;; If vars is not empty, pick the first variable v in it (which we assume is
;; an unbound variable of px). Assign T to v in px, using one of the functions
;; defined above. We have now one unassigned variable fewer in px; use a
;; suitable recursive call to satisfy-remain to check for satisfiability of
;; the remaining expression (where v no longer occurs unbound).

;; If this succeeds, you are done. If this does not succeed, your only hope to
;; find a satisfying assignment is to try to assign F (nil) to v. Do that, and
;; recur, using a very similar recursive call as before.

;   Write at least ten checks.

(defunc satisfy-remain (px vars bindings)
  :input-contract (and (PropExp px) (LoSymbolp vars) (LoAssignp bindings))
  :output-contract (SatisfyResultp (satisfy-remain px vars bindings))
  (if (endp vars)
    (cond ((equal 'error (bval px))        'error)
          ((not (bval px))                 'unsat)
          (t                               bindings))
    (if (not (symbolp (satisfy-remain (bind px (first vars) t) 
                                      (rest vars) 
                                      (cons (list (first vars) t) bindings))))
      (satisfy-remain (bind px (first vars) t)(rest vars)(cons (list (first vars) t) bindings))
      (satisfy-remain (bind px (first vars) nil)(rest vars)(cons (list (first vars) nil) bindings)))))


                        


(check= (satisfy-remain T   '() '((A T))) '((A T)))
(check= (satisfy-remain NIL '() '((A T))) 'unsat)
(check= (satisfy-remain '(& A  B) '(A B) '()) '((B T) (A T)))
(check= (satisfy-remain '(& A  B) '(A) '()) 'error)

(check= (satisfy-remain '(~ (& A  B)) '(A B) '()) '((B nil) (A t)))
(check= (satisfy-remain '(~ (& A  B)) '() '()) 'error)
(check= (satisfy-remain '(~ T) '() '()) 'unsat)
(check= (satisfy-remain '(~ nil) '() '()) nil)
(check= (satisfy-remain '(~ nil) '() '((A T))) '((A T)))
(check= (satisfy-remain '(~ (== A (& A (+ B (=> C  D))))) '(A B C D) '()) '((D NIL) (C T) (B NIL) (A T)))
(check= (satisfy-remain nil '() '()) 'unsat)
(check= (satisfy-remain '(& B (& C (== A (~ A)))) '(A B C) '()) 'unsat)
(check= (satisfy-remain t '() '()) nil)
(check= (satisfy-remain '(~ (+ A (~ (+ A (~ (+ A (~ (+ A (~ (+ A A)))))))))) '(A) nil) '((A nil)))

; Function (satisfy px) is just a user-friendly wrapper around
; satisfy-remain that initializes satisfy-remain's arguments appropriately.
; The goal of satisfy is to find a satisfying assignment (a binding) to the
; variables in px if one exists, or return 'unsat.

; The following useful theorem should go through automatically.
(defthm loassignp-satisfyresultp
  (implies (loassignp x)
           (satisfyresultp x)))

; This is a one-liner! Use existing functions to gather all the variables in
; the expression 'px', and initialize the binding list of satisfy-remain as
; needed.

; Write at least five checks.

(defunc satisfy (px)
  :input-contract (PropExp px)
  :output-contract (SatisfyResultp (satisfy px))
  (satisfy-remain px (get-vars px nil) nil))

(check= (satisfy '(~ (& A  B))) '((B nil) (A t)))
(check= (satisfy '(~ T)) 'unsat)
(check= (satisfy '(~ nil)) nil)
(check= (satisfy '(~ (== A (& A (+ B (=> C  D)))))) '((D NIL) (C T) (B NIL) (A T)))
(check= (satisfy nil) 'unsat)


;  Notice that satisfy should never return 'error. Use acl2::test? to check
;  this conjecture. Remember to add appropriate hypothesis.

(acl2::test?
 (implies (PropExp px)
         (not (equal (satisfy px) 'error))))

;  Now use the satisfy function to find a satisfying assignment to the
;  Classification problems in the Homework 4. First write the expressions
;  in the problem in the form required by the data-definition of PropEx.
;  Then use the function 'satisfy' to find a satisfying assignment.

; isvalid : PropEx -> SatisfyResult

; (isvalid px) uses the above function satisfy to determine whether
; expression px is valid.
; This is a small non-recursive function. Do not write a validity checker
; from scratch!
; 
; Write at least five tests.

(defunc isvalid (px)
  :input-contract (PropExp px)
  :output-contract (SatisfyResultp (isvalid px))
  (if (equal 'unsat (satisfy (list '~ px))) 'valid
        (satisfy (list '~ px))))

(check= (isvalid '(== A A)) 'valid)
(check= (isvalid '(=> A A)) 'valid)
(check= (isvalid '(~ (& A (~ A)))) 'valid)
(check= (isvalid '(== A B))  '((B NIL) (A T)))
(check= (isvalid '(& A B))  '((B NIL) (A T)))
(check= (isvalid nil) nil)
(check= (isvalid t) 'valid)

;  Now use the function 'isvalid' to determine validity of the Word
;  problems in the Homework 4. First write the expressions in the problem
;  in the form required by the data-definition of PropEx. Then use the
;  function 'isvalid' to report 'valid or provide a falsifying assignment.

; a) ((S => (U <> R)) /\ S) => (R /\ U)

(check= (isvalid '(=> (& (=> S (~ (== U R))) S) (& R U))) '((U NIL) (R T) (S T)))

; b) ((S => K) /\ K) => S

(check= (isvalid '(=> (& (=> S K) K) S)) '((S NIL) (K T)))

; c) ((L => C) /\ (G /\ L)) => C

(check= (isvalid '(=> (& (=> L C) (& G L)) C)) 'valid)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Decimal to binary conversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; In the last homework we have seen how to build an adder for integers.
; Unfortunately, the integers had to be written in binary, for instance 8
; in the form 1000. Of course, this conversion is easy to automate, in both
; directions. And we can even use ACL2 to test that converting to and from
; binary notation gives us the original integer back.
; In fact, later in the class we will be able to _prove_ that, but you
; don't need to do that in this homework.

; We begin with a few simple data definitions.

(defdata digit (oneof 1 0))

; Write three tests (one negative, two positive) involving the recognizer
; for this datatype, digitp.

(check= (digitp 0) t)
(check= (digitp 1) t)
(check= (digitp 2) nil)



(defdata digitlist (listof digit))

; Write three tests (one negative, two positive) involving the recognizer
; for this datatype, digitlistp.

(check= (digitlistp '(0 1 0 1)) t)
(check= (digitlistp '(1 1 1 1)) t)
(check= (digitlistp '(2 5 3 2)) nil)



; The following theorems will help ACL2s reason about digitlists.

(defthm digitlist-append
  (implies (and (digitlistp l1)
                (digitlistp l2))
           (digitlistp (acl2::append l1 l2))))

(defthm digitlist-rev
  (implies (digitlistp l) 
           (digitlistp (rev l))))

(defthm digitlist-tlp
  (implies (digitlistp l) 
           (acl2::true-listp l))
  :rule-classes (:forward-chaining :rewrite))

; power : Nat x Nat -> Nat
; (power n e) computes n raised to power e
; Write 4 more tests.
(defunc power (n e)
  :input-contract (and (natp n) (natp e))
  :output-contract (natp (power n e))
  (if (equal e 0) 1 (* n (power n (- e 1)))))

(check= (power 0 0) 1)
(check= (power 3 1) 3)

(check= (power 0 1) 0)
(check= (power 2 10) 1024)
(check= (power 10 3) 1000)
(check= (power 5 4) 625)
(check= (power 1 2000) 1)

; Function evenp is a recognizer for even numbers.
; As a recognizer, evenp should take _anything_ as input (not just
; numbers), and return a boolean. It should return t if the input
; is an even integer and nil otherwise.
(defunc evenp (n)
  :input-contract t
  :output-contract (booleanp (evenp n))
  (and (integerp n) (integerp (/ n 2))))

(check= (evenp 0) t)
(check= (evenp 2) t)
(check= (evenp 1) nil)
(check= (evenp 3) nil)
(check= (evenp 9/1024) nil)

; Now our first conversion function:
; digitlist2dec : Digitlist -> Nat
; (digitlist2dec l) takes a list of digits (zeros and ones), interprets them as
; an integer in binary notation, and converts this integer into decimal notation.

; The empty digitlist should be converted to 0.

; Hint: process the digitlist from left to right, accumulating the final
; decimal number on the way.

; Add 5 more test cases.

(defunc digitlist2dec (l)
  :input-contract (Digitlistp l)
  :output-contract (natp (digitlist2dec l))
  (cond ((endp l) 0)
        (t (+ (* (first l) (power  2 (len (rest l))))
              (digitlist2dec (rest l))))))

(check= (digitlist2dec '()) 0)
(check= (digitlist2dec '(1 1 0 1)) 13)
(check= (digitlist2dec '(1 1 0 1 1 0 1 0 1)) 437)

(check= (digitlist2dec '(1 1 1 1 1 1 1 1 1 1 1)) 2047)
(check= (digitlist2dec '(0)) 0)
(check= (digitlist2dec '(1 0 1 1 1 0 0 0)) 184)
(check= (digitlist2dec '(1)) 1)
(check= (digitlist2dec '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 32767)

; Now the inverse conversion function:
; dec2digitlist : Nat -> Digitlist
; (dec2digitlist n) takes a natural number n (which, by the definition of
; the type of natural numbers, is interpreted by ACL2 in decimal notation), and
; converts it into binary notation.

; Number 0 should be converted into the empty digitlist.

; This function is slightly more tricky: let us first define a helper
; function dec2digitlist-helper that has the exact same signature as
; dec2digitlist. The helper function processes the digits of n from right
; to left, that is, starting with the lowest-order decimal digit. Decide
; whether this digit should be translated into a 0 or a 1. Then form a cons
; of the binary digit 0 or 1 you decided on, and a list obtained from a
; suitable recursive call of the helper function.

(defunc dec2digitlist-helper (n)
  :input-contract (natp n)
  :output-contract (Digitlistp (dec2digitlist-helper n))
  (if (equal n 0)
    nil
    (if (evenp n) 
      (cons 0 (dec2digitlist-helper (/ n 2)))
      (cons 1 (dec2digitlist-helper (/ (- n 1) 2))))))

; The above function pretty much does the job, except for one thing. What?
; Fix this "one thing" by defining the real dec2digitlist function, which
; calls dec2digitlist-helper and post-processes the result.

; Write 5 more test cases.


(defunc dec2digitlist (n)
  :input-contract (natp n)
  :output-contract (Digitlistp (dec2digitlist n))
  (rev (dec2digitlist-helper n)))

(check= (dec2digitlist  8) '(1 0 0 0))
(check= (dec2digitlist 26) '(1 1 0 1 0))

(check= (dec2digitlist  0) '())
(check= (dec2digitlist  1) '(1))
(check= (dec2digitlist  2) '(1 0))
(check= (dec2digitlist  4) '(1 0 0))
(check= (dec2digitlist  63) '(1 1 1 1 1 1))
(check= (dec2digitlist  64) '(1 0 0 0 0 0 0))

; Finally, use acl2::test? to test whether converting a digitlist to
; decimal and back gives us the original result. Your conjecture should take a
; digitlist l and a natural number n, perform conjecture contract checking,
; and state that if l is the dec2digitlist conversion of n, then ...
; [complete on your own].

(acl2::test?
 (implies (natp n)
          (equal n (digitlist2dec (dec2digitlist n)))))




; What do you find? Do you think there is a counterexample to your
; conjecture? If so state it (whether you have found it using test? or
; not).

; No there are no counter examples to our conjecture because it makes sense that
; it converts back to same number or it could be that both functions have the same error

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Conjecture contract checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

The following conjectures are incomplete (irrespective of whether they are
true or false): they lack hypotheses that make sure the functions
involved are called only on arguments that satisfy the functions' input
contracts. Make the conjectures meaningful by adding the necessary contract
hypotheses. For example, given a conjecture c, if you think hypotheses h1
and h2 (and nothing else) are necessary to ensure the input contracts of
all functions occurring in c are satisfied, your output should be

h1 & h2 => c

The added hypotheses should be minimal. Therefore:

- do not add any hypotheses that are enforced by the output contracts of
the participating functions. For instance, if the expression (f (g x))
appears in your conjecture, and g : type1 -> type2 and f : type2 -> type3,
then do not add (type2p (g x)) as a hypothesis: this is guaranteed by the
output contract of g.

- simplify the conjecture hypotheses as much as possible. In the above
example, suppose h1 => h2 is valid (say h1 = (equal x nil) and h2 = (listp x)).
Then the conjunction h1 & h2 is equivalent to h1 ; thus, simplify your
output to

h1 => c

The examples below may contain some unknown functions (whose signature will
be provided), and known functions such as arithmetic + . The input
contracts of _all_ functions need to be enforced by the hypotheses.

1. Given

foo: List x List -> List
bar:  All x List -> List

the conjecture c is

(foo (bar (/ x y) l) z)   =   (bar (/ y x) (foo l z))

Conjecture with hypotheses:

h1 is
(and (rationalp x) (rationalp y) (listp l) (listp z))

h1 => c

(implies (and (rationalp x) (rationalp y) (listp l) (listp z))
         (equal (foo (bar (/ x y) l) z) (bar (/ y x) (foo l z))))


2. Consider the following two definitions, the first of which gives rise to
a recognizer natlistp:

|#

(defdata natlist (listof nat))

(defunc evenlistp (l)
  :input-contract t
  :output-contract (booleanp (evenlistp l))
  (if (listp l)
      (integerp (/ (len l) 2))
    nil))

#|

For the following functions we only know the signature:

d: List -> All
m: Nat x List -> NatList
f: List -> Nat
s: EvenList -> List

the conjecture c is

(d (m x l)) = (f (s l))

Conjecture with hypotheses:

h1 is
(and (natp x) (Evenlistp l))

h1 => c

(implies (and (natp x) (Evenlistp l))
         (equal (d (m x l)) (f (s l))))

Now define four functions d, m, f, s (as simple as possible) with the above
signatures. The actual outputs of the functions are arbitrary, as long as
they satisfy the output contract. For example, for m, you may choose to
simply return the constant nil, which is a natlist.

|#

(defunc d (l)
  :input-contract (listp l)
  :output-contract t
  5)

(defunc m (n l)
  :input-contract (and (natp n) (listp l))
  :output-contract (NatListp (m n l))
  nil)

(defunc f (l)
  :input-contract (listp l)
  :output-contract (natp (f l))
  0)

(defunc s (l)
  :input-contract (EvenListp l)
  :output-contract (listp (s l))
  nil)#|ACL2s-ToDo-Line|#


#|

Write an acl2::test? that tests the conjecture for your definitions.
Remember to add appropriate hypotheses. Do the function definitions satisfy
the conjecture, or there is a witness that falsifies the conjecture?

3. Given function signatures for d, m, f, s as above in 2, the conjecture c
now is

(d (m x l)) = (f (s x))

Conjecture with hypotheses:

h1 is
(and (natp x) (Evenlistp x))

evaluates to:
nil

h1 => c

(implies (and (natp x) (Evenlistp x))
         (equal (d (m x l)) (f (s l))))
         
evaluates to:
t
    
Write an acl2::test? that tests the conjecture for your definitions.
Remember to add appropriate hypotheses. Do the function definitions satisfy
the conjecture, or there is a witness that falisifies the conjecture?
Explain.

(acl2::test? (implies (and (natp x) (Evenlistp l))
                      (equal (d (m x l)) (f (s l)))))
                      
Was falsified because

(equal (d (m x l)) (f (s l))))  becomes...
(equal (d nil) (f nil))         becomes...
(equal 5 0)                     becomes...
nil


(acl2::test? (implies (and (natp x) (Evenlistp x))
                      (equal (d (m x l)) (f (s l)))))

Was proven because...
(and (natp x) (Evenlistp x)) must evalute to...
nil

NIL => Anything evalutes to...
t


4. 

Come up with a conjecture that is not valid but for which you think finding
a counterexample is hard. You may only use the following functions in the
conjecture:

listp
endp
consp
first 
rest
len
equal
if 
and 
or
implies 
remove-dups
rev
append
in
PropExpp

Use acl2::test? to test your conjecture. Does ACl2s find a counterexample?

h is
(and A (consp B))

c is 
(in A (rest (rev (remove-dups (append B B))))) = (not (in A B))


(acl2::test? (implies (and A (consp B))
                      (equal (in A (rest (rev (remove-dups (append B B))))) (not (in A B)))))

|#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Feedback
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

Please fill out the following feedback form, not as a team, but each team
member individually:

https://docs.google.com/spreadsheet/viewform?formkey=dF9HMEpYX040SVYtTkExSkppRm1QSGc6MA

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
