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
CS 2800 Homework 4 - Spring 2013

For this homework, you will be using the BEGINNER mode in ACL2s.

Instructions for using this file:

- open this file in ACL2s as hw04.lisp

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

  svn://dell-server-wahl-1.ccs.neu.edu/cs2800-2013-spring/04/<groupname>

  where groupname is the name of the group as it was emailed to you. Your
  username and password remain as before.

  Place your file hw04.lisp in this directory, add it, and commit. Do NOT
  submit the session file (ending in .a2s).

Finally, add an ASCII text file "comments.txt" into the same directory
where you put hw04.lisp, with the following contents:

(i) a summary of how much time you spent on this homework, and how you
split up the work between the team members.

(ii) any feedback on the homework you may have, such as errors in it,
sources of misunderstanding, general difficulty, did you learn
something, etc.

- for non-programming problems, include solutions as comments. We will use
F and T to represent false and true, and the following character
combinations to represent the Boolean connectives:

  NOT        ~

  AND        /\
  OR         \/

  IMPLIES    =>

  EQUIVALENT =
  XOR        <>

  The binding powers of these connectives are as in class: they are listed
  in the above table from highest to lowest. Within one group (no blank line),
  the binding power is the same.

|#

#| 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1. Classification 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

For each of the following formulas, determine if they are valid,
satisfiable, unsatisfiable, or falsifiable. These labels can overlap (e.g.
a formula can be both satisfiable and valid), so indicate ALL of the four
characterizations that apply. You can use a truth table or a simplification
argument. For satisfiable or falsifiable formulas, you can also give
assignments that show satisfiability or falsifiability.

a) (p /\ q ) \/ r = (p /\ r) \/ q

-----------------------------------------
p = F

(F /\ q ) \/ r = (F /\ r) \/ q
F \/ r = F \/ q
r = q

-----------------------------------------
p = T
(T /\ q ) \/ r = (T /\ r) \/ q
T \/ r = T \/ q
T = T
T
-----------------------------------------

satisfiable and falsifiable


b) (p => q) /\ (r=>s) /\ (p\/r) => (q\/s)

-----------------------------------------
q = T

(p => T) /\ (r=>s) /\ (p\/r) => (T\/s)
(p => T) /\ (r=>s) /\ (p\/r) => T
T

-----------------------------------------
q = F

(p => F) /\ (r=>s) /\ (p\/r) => (F\/s)
~p /\ (r=>s) /\ (p \/ r) => s
~p /\ (~r \/ s) /\ (p \/ r) => s
~p /\ (~r \/ s) /\ (p \/ r) => s
(~r \/ s) /\ ((~p /\ p) \/ (~p /\ r)) => s
(~r \/ s) /\ (F \/ (~p /\ r)) => s
(~r \/ s) /\ (~p /\ r) => s
-----------------------------------------
s = T

(~r \/ T) /\ (~p /\ r) => T
T

-----------------------------------------
s = F

(~r \/ F) /\ (~p /\ r) => F
~r /\ (~p /\ r) => F
~r /\ ~p /\ r => F
(~r /\ r) /\ ~p  => F
F /\ ~p  => F
F => F
T

-----------------------------------------
-----------------------------------------

valid and satisfiable

c)  ~q /\ (p=>q) => q

-----------------------------------------
q = T

~T /\ (p => T) => T
T

-----------------------------------------
q = F

~F /\ (p => F) => F
T /\ (p => F) => F
(p => F) => F
~(p => F)
~(~p \/ F)
~~p
p

-----------------------------------------

satisfiable and falsifiable

d) ~p /\ (p => q) => ~q

-----------------------------------------
q = F

~p /\ (p => F) => ~F
~p /\ (p => F) => T
T

-----------------------------------------
q = T

~p /\ (p => T) => ~T
~p /\ T => F
~p => F
~~p \/ F
~~p
p
-----------------------------------------
satisfiable and falsifiable

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2. Word problems 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

The problems below consist of some assumptions followed by a conclusion.
Formalize and analyze the statements using propositional logic, as follows:
assign propositional variables to the atomic facts in the statements,
formalize the entire statement into a propositional logic formula, and then
determine whether the conclusion follows from the assumptions.

For example, suppose you were asked to formalize and analyze:

Mitt likes Jane if and only if Jane likes Mitt.
Jane likes Bill.
Therefore, Mitt does not like Jane.

Here's the kind of answer we expect. (Note that we have used "mnemonic"
[easy to remember] propositional variables to denote the facts in the
sentences. Do not use T or F as variables: these are reserved for true and
false, respectively.) :

Let mj denote "Mitt likes Jane"; let jm denote "Jane likes Mitt"; let jb
denote "Jane likes Bill". The first sentence can then be formalized as
mj = jm.  The second sentence is jb. The third sentence contains
the claim we are to analyze, which can be formalized as
(mj = jm) /\ jb => ~mj .

This is not a valid claim. A truth table shows that the claim is violated
by the assignment mj = jm = jb = T.

Problems: 

a) If Natasha is a spy, then exactly one of following holds: Natasha works
for USA or Natasha works for USSR. Natasha is a spy. Therefore, Natasha
works for USSR and Natasha works for USA.

Let S denote "Natasha is a spy"
Let U denote "Natasha works for USA"
Let R denote "Natasha works for USSR"

If S, then U xor R. = (S => (U <> R))
S. = S
Therefore, R and U = R /\ U

((S => (U <> R)) /\ S) => (R /\ U)


U = F
((S => (F <> R)) /\ S) => (R /\ F)
((S => R) /\ S) => F
~((S => R) /\ S) \/ F
~((S => R) /\ S)
~((~S \/ R) /\ S)
~((~S /\ S) \/ (R /\ S))
~(F \/ (R /\ S))
~(R /\ S)
~R \/ ~S

this is not valid due to the case R = S = T and U = F.

b) If Arthur pulled a sword from stone, then Arthur is King. Arthur is
King. Therefore, Arthur pulled a sword from stone.

Let S denote "Arthur pulled a sword from stone"
Let K denote Arthur is King

If S, then K =  S => K
K. = K
Therefore, S. = S

((S => K) /\ K) => S

S = F
((F=> K) /\ K) => F
~((F=> K) /\ K) \/ F
~((F=> K) /\ K)
~(T /\ K)
~(K)
~K

this is not valid due to the case S = F and K = T.

c) Tom takes the advanced course in Logic only if CS2800 is interesting.
Tom gets a good grade in CS2800 and Tom takes the advanced course in Logic.
Therefore, CS 2800 is interesting.

Let L denote "Tom takes the advanced course in Logic"
Let C denote "CS2800 is interesting"
Lem G denote "Tom gets a good grade in CS2800"

L only if C. = L => C
G and L. = G /\ L
Therefore, C. = C

((L => C) /\ (G /\ L)) => C
-----------------------------------------
C = T

((L => T) /\ (G /\ L)) => T
T

-----------------------------------------
C = F

((L => F) /\ (G /\ L)) => F
~((L => F) /\ (G /\ L)) \/ F
~((~L \/ F) /\ (G /\ L)) \/ F
~(~L /\ (G /\ L))
~(~L /\ L /\ G)
~(F /\ G)
~(F)
T
-----------------------------------------
this is valid

d) Find a non-CS and non-math book of your choice, such as a novel, a
history article, or a newspaper article on current affairs. Pick two
sentences whose formalization requires at least 3 and at most 6 variables.
Formalize the sentences and classify them as above. You can pick the two
sentences from two different books if you want. (Since you are quoting
these sentences from a book, remember to cite the source, i.e. the exact
book reference with page numbers!)








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 3. Decision Procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Given is a decision procedure UNSAT(h) that returns T if h is
unsatisfiable, F otherwise. Show how to construct a decision procedure for
satisfiability, one for falsifiability, and one for validity. Each of your
procedures should only involve UNSAT and elementary operations such as
tests, or negating answers. You are not allowed to build truth tables,
simplify input formulas, etc.

SAT(h) = ~UNSAT(h)
FAL(h) = ~UNSAT(~h)
VAL(h) = UNSAT(~h)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 4. Normal Forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Recall: a formula is in Conjunctive Normal Form (CNF) if it is a
conjunction of disjunctions of literals. Analogously, we defined
Disjunctive Normal Form (DNF) in the lecture.

Convert the following formulas to the normal form specified. You do not
have to show the intermediate steps, but state how you did it (e.g. via
truth table, via simplification, etc).

a) DNF for (x \/ ~y) /\ (y \/ ~z) => (~y => z)

 ~((x \/ ~y) /\ (y \/ ~z)) \/ (~y => z)
(~(x \/ ~y) \/ ~(y \/ ~z)) \/ (~y => z)
(~x /\ y) \/ (~y /\ z) \/ (~y => z)
(~x /\ y) \/ (~y /\ z) \/ (y \/ z)
(~x /\ y) \/ ((~y \/ (y \/ z)) /\ (z \/ (y \/ z)))
(~x /\ y) \/ ((~y \/ y \/ z) /\ (z \/ y \/ z))
(~x /\ y) \/ ((T \/ z) /\ (z \/ y))
(~x /\ y) \/ (T /\ (z \/ y))
(~x /\ y) \/ (z \/ y)

(~x /\ y) \/ z \/ y
simplification

b) CNF for (x => (y => z)) = (y => ~x)

(~x \/ (~y \/ z)) = (~y \/ ~x)
(~x \/ ~y \/ z) = (~x \/ ~y)
((~x \/ ~y \/ z) /\ (~x \/ ~y)) \/ (~(~x \/ ~y \/ z) /\ ~(~x \/ ~y))
((~x \/ ~y \/ z) /\ (~x \/ ~y)) \/ ((x /\ y /\ ~z) /\ (x /\ y))
(~x \/ ~y) \/ (x /\ y /\ ~z)
(x \/ ~x \/ ~y) /\ (y \/ ~x \/ ~y) /\ (~z \/ ~x \/ ~y)
T /\ T /\ (~z \/ ~x \/ ~y)

(~x \/ ~y \/ ~z)
simplification

c) DNF for (a \/ b) /\ (( a /\ c) \/ (a /\ b) \/ (a /\ ~c)  \/ b)
(a \/ b) /\ ((a /\ (c \/ b \/ ~c))  \/ b)
(a \/ b) /\ ((a /\ T)  \/ b)
(a \/ b) /\ (a  \/ b)
a \/ (b /\ b)

a \/ b
simplification
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; The goal of this exercise is to define an adder: a circuit that performs addition.
;; We begin by introducing a few binary operations.

(defdata PropOp (oneof 'and 'or 'xor))

; A propositional expression is either an atom (represented as a symbol)
; or (Op PropEx PropEx). Ignore the :type-lemmas directive in the data
; definition below.

(defdata PropEx (oneof symbol (list PropOp PropEx PropEx))
  :type-lemmas t)

(defdata listPropEx (listof PropEx))

; A bitvector is a list of of propositional atoms.
(defdata bv (listof symbol))

; A binding is a pair of the form (symbol,t) or (symbol,nil),
; which, intuitively, assigns t or nil to the atomic proposition
; specified by the symbol.
; (Note that t and nil are also symbols. To avoid confusion do not use
; them as propositional atoms.)

(defdata binding (list symbol boolean))

; an assignment is a list of bindings
(defdata assgn (listof binding))

; A value is the result of evaluating a propositional expression given the
; binding of the symbols that appear in the expression. If the
; assignment does not provide a binding of a symbol in the expression,
; its value is defined to be 'unbound.

(defdata value (oneof boolean 'unbound))

(defdata valuelist (listof value))

; 1.
; find2oc : symbol x bv -> booleanp

; (find2oc l) takes a symbol and list of symbols as input and returns t 
; if any symbol in the list equals the symbol.
(defunc find2oc (s l)
  :input-contract (and (symbolp s) (bvp l))
  :output-contract (booleanp (find2oc s l))
  (cond ((endp l) nil)
        ((equal s (first l)) t)
        (t (find2oc s (rest l)))))

(check= (find2oc 'o nil) nil)
(check= (find2oc 'o '(v b p)) nil)
(check= (find2oc 'o '(v b o p)) t)

; nodupsp : bv -> booleanp

; (find2oc l) takes list of symbols as input and returns false if any
; symbol occurs more than once; else it returns true.
(defunc nodupsp (l)
  :input-contract (bvp l)
  :output-contract (booleanp (nodupsp l))
  (cond ((endp l) t)
        ((find2oc (first l) (rest l)) nil)
        (t (nodupsp (rest l)))))

(check= (nodupsp nil) t)
(check= (nodupsp '(v b p)) t)
(check= (nodupsp '(o v b p o)) nil)

; 2.
; get-symbol-list : assgn -> list of symbols

; (get-symbol-list e) takes an assignment as input and returns the
; list of symbols that it binds, i.e. that are assigned values.
(defunc get-symbol-list (e)
  :input-contract (assgnp e)
  :output-contract (bvp (get-symbol-list e))
  (if (endp e) nil
    (cons (first (first e)) (get-symbol-list (rest e)))))

(check= (get-symbol-list nil) nil)
(check= (get-symbol-list (list (list 'a t) (list 'b nil) (list 'b t))) '(a b b))

; 3.
; nodupbindingp : assgn -> boolean

; (nodupbindingp e) takes an assignment as input and returns true if
; there are no duplicate bindings for any symbol.
(defunc nodupbindingp (e)
  :input-contract (assgnp e)
  :output-contract (booleanp (nodupbindingp e))
  (nodupsp (get-symbol-list e)))

(check= (nodupbindingp nil) t)
(check= (nodupbindingp (list (list 'a t) (list 'b nil) (list 'b t))) nil)
(check= (nodupbindingp (list (list 'a t) (list 'b nil) (list 'c t))) t)

; 4.
; lookup-assgn : symbol x assgn -> value 

; (lookup-assgn a e) finds the value that the symbol a is bound to in the
; assgn e. If there exists no binding in the assignment then return
; 'unbound.
(defunc lookup-assgn (a e)
  :input-contract (and (symbolp a) (assgnp e) (nodupbindingp e))
  :output-contract (valuep (lookup-assgn a e))
  (cond ((endp e) 'unbound)
        ((equal a (first (first e))) (second (first e)))
        (t (lookup-assgn a (rest e)))))

(check= (lookup-assgn 'a nil) 'unbound)
(check= (lookup-assgn 'b (list (list 'a t) (list 'b nil) (list 'e t))) nil)
(check= (lookup-assgn 'c (list (list 'a t) (list 'b nil) (list 'c t))) t)

; 5.
; vxor : value x value -> value 

; (vxor a b) is an extension of the known xor function. If the inputs
; are boolean it returns xor(a,b). In case any of the input is
; 'unbound then it returns 'unbound
(defunc vxor (a b)
  :input-contract (and (valuep a) (valuep b))
  :output-contract (valuep (vxor a b))
  (if (and (booleanp a) (booleanp b))
    (not (equal a b))
    'unbound))

(check= (vxor t nil)             t)
(check= (vxor t t)               nil)
(check= (vxor nil nil)           nil)
(check= (vxor nil t)             t)
(check= (vxor t 'unbound)        'unbound)
(check= (vxor 'unbound nil)      'unbound)
(check= (vxor 'unbound 'unbound) 'unbound)

; 6.
; vand : value x value -> value 

; (vand a b) is an extension of the known and function. If the inputs
; are boolean it returns a /\ b. In case any of the input is 'unbound
; then it returns 'unbound
(defunc vand (a b)
  :input-contract (and (valuep a) (valuep b))
  :output-contract (valuep (vand a b))
  (if (and (booleanp a) (booleanp b))
    (and a b)
    'unbound))

(check= (vand t nil)             nil)
(check= (vand t t)               t)
(check= (vand nil nil)           nil)
(check= (vand nil t)             nil)
(check= (vand t 'unbound)        'unbound)
(check= (vand 'unbound nil)      'unbound)
(check= (vand 'unbound 'unbound) 'unbound)

; 7.
; vor : value x value -> value 

; (vor a b) is an extension of the known or function. If the inputs are
; boolean it returns a \/ b. In case any of the input is 'unbound then
; it returns 'unbound
(defunc vor (a b)
  :input-contract (and (valuep a) (valuep b))
  :output-contract (valuep (vor a b))
  (if (and (booleanp a) (booleanp b))
    (or a b)
    'unbound))

(check= (vor t nil)             t)
(check= (vor t t)               t)
(check= (vor nil nil)           nil)
(check= (vor nil t)             t)
(check= (vor t 'unbound)        'unbound)
(check= (vor 'unbound nil)      'unbound)
(check= (vor 'unbound 'unbound) 'unbound)

; Please do not comment or delete the following. These theorems are
; required to admit some of the function definitions.

;  Begin
(defthm consp-propexp1
 (implies (and (PropExp s)
               (not (symbolp s)))
          (consp s))
 :rule-classes (:forward-chaining))

(defthm consp-propexp2
 (implies (and (PropExp s)
               (not (symbolp s)))
          (consp (rest s)))
 :rule-classes (:type-prescription))

(defthm consp-propexp3
 (implies (and (PropExp s)
               (not (symbolp s)))
          (consp (rest (rest s))))
 :rule-classes (:type-prescription))
; End

; 8. 
; evaluate-expr : PropEx x assgn -> value

; (evaluate-expr s e) takes a propositional expression s and an
; assignment e. The assignment tells the value a symbol appearing in
; the expression is bound to. Then it utilizes the propositional
; operations defined above to evaluate the expression.

; Write at least 5 checks. Cover the case when the assignment does not
; provide a binding of a symbol appearing in the expression.

(acl2::in-theory (acl2::disable valuep-definition-thm))

; NOTE : It may take a little while (up to a minute or two depending on
; the machine) to admit the definition. Please be patient.
(defunc evaluate-expr (s e)
  :input-contract (and (PropExp s) (assgnp e) (nodupbindingp e))
  :output-contract (valuep (evaluate-expr s e))
  (cond ((symbolp s) (lookup-assgn s e))
        ((equal 'or (first s)) 
         (vor (evaluate-expr (second s)  e) (evaluate-expr (third s) e)))
        ((equal 'xor (first s)) 
         (vxor (evaluate-expr (second s) e) (evaluate-expr (third s) e)))
        (t
         (vand (evaluate-expr (second s) e) (evaluate-expr (third s) e)))))

(check= (evaluate-expr 'x '((x t))) t)
(check= (evaluate-expr 'x nil) 'unbound)
(check= (evaluate-expr '(and x y) '((x t) (y t))) t)
(check= (evaluate-expr '(and x y) '((x t) (y t))) t)

; 9.
; evaluate-exprlist : listPropEx x assgn -> valuelist

; (evaluate-exprlist ls e) inputs a list of propositional expressions
; ls, and assignment e such that e has no duplicate bindings. It
; evaluates each expression in ls and returns a list of values that
; each expression evaluates to.

; Write at least 5 checks. 
(defunc evaluate-exprlist (ls e)
  :input-contract (and (listPropExp ls) (assgnp e) (nodupbindingp e))
  :output-contract (valuelistp (evaluate-exprlist ls e))
  (if (endp ls)
    nil
    (cons (evaluate-expr (first ls) e)
          (evaluate-exprlist (rest ls) e))))

(check= (evaluate-exprlist '((or x y)  (and x y)) '((x t) (y t)))     '(t t))
(check= (evaluate-exprlist '((xor x y) (and x y)) '((x t) (y t)))     '(nil t))
(check= (evaluate-exprlist '((or x y)  (and x y)) '((x nil) (y t)))   '(t nil))
(check= (evaluate-exprlist '((xor x y) (and x y)) '((x t) (y nil)))   '(t nil))
(check= (evaluate-exprlist '((xor x y) (and x y)) '((x nil) (y nil))) '(nil nil))
(check= (evaluate-exprlist '((or x y)  (and x y)) '((x t) (y t)))     '(t t))
(check= (evaluate-exprlist '((xor x y) (and x y)) '((x t) (y t)))     '(nil t))

; 10.
; Define FA-sum : symbol x symbol x PropEx --> PropEx

; FA-sum function implements the "sum" of the full-adder. It takes two
; atomic propositions, a and b, and PropEx c (PropEx for carry
; in). It returns the PropEx representing the sum component of the
; full-adder.

; In this and the following functions, the carry c is a PropEx, rather than
; a variable, as these functions will be later used to get a propositional
; expression for adding two bitvectors.

(defunc FA-sum (a b c)
  :input-contract  (and (symbolp a) (symbolp b) (PropExp c))
  :output-contract (PropExp (FA-sum a b c))
  (list 'or
        (list 'and a (list 'and b c))
        (list 'xor a (list 'xor b c))))
        
        

; The above function generates a propositional expression. We will
; provide bindings to the symbols and use evaluate-expr in order to
; test it. For example
(check= (evaluate-expr (FA-sum 'a 'b '(and x y)) '((a nil) (b t) (x t))) 'unbound)
(check= (evaluate-expr (FA-sum 'a 'b 'c) '((a nil) (b nil) (c nil))) nil)
(check= (evaluate-expr (FA-sum 'a 'b 'c) '((a nil) (b t) (c nil))) t)

; Now assume that c is symbol, and write the other 6 possible test cases for FA-sum
(check= (evaluate-expr (FA-sum 'a 'b 'c) '((a t)   (b t)     (c t)))   t)
(check= (evaluate-expr (FA-sum 'a 'b 'c) '((a t)   (b t)     (c nil))) nil)
(check= (evaluate-expr (FA-sum 'a 'b 'c) '((a t)   (b nil)   (c t)))   nil)
(check= (evaluate-expr (FA-sum 'a 'b 'c) '((a t)   (b nil)   (c nil))) t)
(check= (evaluate-expr (FA-sum 'a 'b 'c) '((a nil) (b t)     (c t)))   nil)
(check= (evaluate-expr (FA-sum 'a 'b 'c) '((a nil) (b nil)   (c t)))   t)
; 11.
; Define FA-carry : symbol x symbol x PropEx --> PropEx

; FA-carry function implements the "carry" of the full-adder. It takes two
; atomic propositions, a and b, and PropEx c (PropEx for carry
; in). It returns the PropEx representing the carry component of the
; full-adder.
(defunc FA-carry (a b c)
  :input-contract (and (symbolp a) (symbolp b) (PropExp c))
  :output-contract (PropExp (FA-carry a b c))
  (list 'or
        (list 'and a (list 'and b c))
        (list 'or
              (list 'and a (list 'xor b c))
              (list 'or
                    (list 'and b (list 'xor a c))
                    (list 'and c (list 'xor a b))))))

(check= (evaluate-expr (FA-carry 'a 'b 'c) '((a nil) (b nil) (c nil))) nil)
(check= (evaluate-expr (FA-carry 'a 'b 'c) '((a t) (b t) (c nil))) t)
(check= (evaluate-expr (FA-carry 'a 'b '(and x y)) '((a nil) (b t) (x t) (y nil))) nil)

; Now assume that c is symbol, and write the other 6 possible test cases for FA-carry
(check= (evaluate-expr (FA-carry 'a 'b 'c) '((a t)   (b t)     (c t)))   t)
(check= (evaluate-expr (FA-carry 'a 'b 'c) '((a t)   (b nil)   (c t)))   t)
(check= (evaluate-expr (FA-carry 'a 'b 'c) '((a t)   (b nil)   (c nil))) nil)
(check= (evaluate-expr (FA-carry 'a 'b 'c) '((a nil) (b t)     (c t)))   t)
(check= (evaluate-expr (FA-carry 'a 'b 'c) '((a nil) (b t)     (c nil))) nil)
(check= (evaluate-expr (FA-carry 'a 'b 'c) '((a nil) (b nil)   (c t)))   nil)
; 12.
; Define FA : symbol x symbol x PropEx --> ListPropEx

; FA function implements the "sum" and "carry" of the full-adder. It
; takes two atomic propositions, a and b, and PropEx c (PropEx for
; carry in). It returns the listPropEx of two elements. The first
; element is the "sum" component of the full-adder. The second element
; of the list is the "carry" component of the full-adder.
(defunc FA (a b c)
  :input-contract (and (symbolp a) (symbolp b) (PropExp c))
  :output-contract (listPropExp (FA a b c))
  (list (FA-sum a b c) (FA-carry a b c)))

(check= (evaluate-exprlist (FA 'a 'b '(and x y)) '((a nil) (b t) (x t) (y nil))) '(t nil))
(check= (evaluate-exprlist (FA 'a 'b 'c) '((a nil) (b t) (c nil))) '( t nil))
(check= (evaluate-exprlist (FA 'a 'b 'c) '((a t) (b t) (c nil))) '(nil t))

; Write all the possible test cases for one bit FA (assume c is a symbol)
(check= (evaluate-exprlist (FA 'a 'b 'c) '((a nil) (b nil) (c nil))) '(nil nil))
(check= (evaluate-exprlist (FA 'a 'b 'c) '((a nil) (b nil) (c t)))   '(t nil))
(check= (evaluate-exprlist (FA 'a 'b 'c) '((a nil) (b t)   (c nil))) '(t nil))
(check= (evaluate-exprlist (FA 'a 'b 'c) '((a nil) (b t)   (c t)))   '(nil t))
(check= (evaluate-exprlist (FA 'a 'b 'c) '((a t)   (b nil) (c nil))) '(t nil))
(check= (evaluate-exprlist (FA 'a 'b 'c) '((a t)   (b nil) (c t)))   '(nil t))
(check= (evaluate-exprlist (FA 'a 'b 'c) '((a t)   (b t)   (c nil))) '(nil t))
(check= (evaluate-exprlist (FA 'a 'b 'c) '((a t)   (b t)   (c t)))   '(t t))

; Please do not comment or delete the following. These theorems are
; required to admit some of the function definitions.

;  Begin
(defthm len-equal-consp
  (implies (and (bvp bv1)
                (bvp bv2)
                (equal (len bv1) (len bv2))
                (consp bv1))
           (consp bv2)))

; End

; 13.
; Define ripcarry-in: bitvector x bitvector x PropEx -> listPropEx

; Given are two equal-length bitvectors and a PropEx representing an
; expression for a carry bit. This function computes a list of
; PropEx's.  The ith element represents the function that computes the
; ith bit of the sum of the given bitvectors. The input is assumed to
; be given such that the least significant bit comes first, most
; significant bit last. The output should be in the same order. 

(defunc ripcarry-in (bv1 bv2 carry)
  :input-contract (and (PropExp carry) (bvp bv1) (bvp bv2) (equal (len bv1) (len bv2)))
  :output-contract (listPropExp (ripcarry-in bv1 bv2 carry))
  (if (endp bv1) (list carry)
    (cons (FA-sum (first bv1) (first bv2) carry)
          (ripcarry-in (rest bv1)
                       (rest bv2)
                       (FA-carry (first bv1) (first bv2) carry)))))

; We reserve the two symbols 'false and 'true for constants. Further, we
; will assume that we always have the following bindings for them in any
; assignment that we create:
; ('false nil) and ('true t)

; 14.
; Define ripcarry: bitvector x bitvector -> (list PropEx)

; Given are two equal-length bitvectors. This function computes a list
; of PropEx's. The ith element represents the function that computes
; the ith bit of the sum of the given bitvectors. The initial carry is
; assumed to be 0, represented by the constant function 'false.
(defunc ripcarry (bv1 bv2)
  :input-contract (and (bvp bv1) (bvp bv2) (equal (len bv1) (len bv2)))
  :output-contract (listPropExp (ripcarry bv1 bv2))
  (ripcarry-in bv1 bv2 'false))

; Write 5 checks for a 2 bit adder. Use the evaluate-exprlist and an
; assignment for the symbols as above. Remember to assign 'false to
; nil and 'true to t in the assignments

(check= (evaluate-exprlist (ripcarry nil nil) '((false nil) (true t))) '(nil))
(check= (evaluate-exprlist (ripcarry '(a b c d e) '(a b c d e))
                           '((a t) (b t) (c t) (d t) (e t) (false nil) (true t))) 
        '(nil t t t t t))
(check= (evaluate-exprlist (ripcarry '(b b) '(a a))
                           '((a nil) (b t) (false nil) (true t))) 
        '(t t nil))
(check= (evaluate-exprlist (ripcarry '(a b) '(b a))
                           '((a nil) (b t) (false nil) (true t))) 
        '(t t nil))
(check= (evaluate-exprlist (ripcarry '(b b ) '(a a))
                           '((a nil) (b nil) (false nil) (true t))) 
        '(nil nil nil))#|ACL2s-ToDo-Line|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Feedback
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

Please fill out the following feedback form, not as a team, but each team
member individually:

https://docs.google.com/spreadsheet/viewform?formkey=dFVINVZjamQyNlFabHFmQWk5c0hUM3c6MA

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