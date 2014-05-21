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

CS 2800 Homework 12 - Spring 2013

Student names:

- For this homework, you are going to use ACL2s to reason about
  tail-recursive function definitions.

- To turn in your solution, check out the directory at

  svn://dell-server-wahl-1.ccs.neu.edu/cs2800-2013-spring/12/<groupname>

  Place your file hw12.lisp in this directory, add it, and commit.

- Your entire solution file must be accepted by ACL2s, in
  BEGINNER mode. If your solution to any question that is to be
  run in ACL2s is incomplete, comment out those portions so that
  the file is accepted by ACL2s.

In this homework, we are going to use ACL2s as a workhorse to do
all the boring reasoning for us, e.g., equational reasoning. This
will free us to focus on the interesting part of proving
theorems, which is coming up with the key insights, encoded as
lemmas.

Here is a quick reminder of what we covered in class.

In ACL2s, if I want to prove a theorem, I do it with defthm. For
example, to ask ACL2s to prove the conjecture

(listp x) => (len (rev x)) = (len x)

I write this:

(defthm len-rev ; the name of the theorem ;
  (implies (listp x)
           (equal (len (rev x))
                  (len x))))

Theorems (like len-rev above) are used by ACL2s as rewrite
rules. What this means is that when ACL2s is reasoning and sees
an expression that matches the lhs (left-hand side) of len-rev,
it wil try to apply the rule by rewriting the lhs to the rhs of
the rule.

For example, suppose ACL2s see the expression

(len (rev (app a b)))

This expression "matches" the lhs of len-rev (see the lecture
notes for the details) and will get rewritten as per the rhs of
the rule to:

(len (app a b))

This was a good thing because the expression was simplified.

If we had written len-rev by swapping the lhs and rhs, we would
have gotten very different  behavior. The expression

(len (rev (app a b)))

would have been rewritten to 

(len (rev (rev (app a b))))

which is not a good thing. 

So the take-home message is to be careful how you write your
defthms. Make sure that the lhs is the more complex side.

|#

; Below are some basic lemmas that will be useful later.

(defthm len-consp
  (implies (and (equal (len x) (len y))
                (listp x) 
                (listp y) 
                (consp x))
           (consp y))
  :rule-classes :forward-chaining)

(defthm rev-tlp
  (implies (listp x)
           (acl2::true-listp (rev x)))
  :rule-classes ((:type-prescription) (:forward-chaining)))

(defthm app-rev
  (implies (and (listp x)
                (listp y))
           (equal (rev (acl2::append x y))
                  (acl2::append (rev y) (rev x)))))

(defthm rev-rev
  (implies (listp x)
           (equal (rev (rev x))
                  x)))

; Here is the definition of revt, the tail recursive version
; of rev that we defined in class.

(defunc revt (x acc) 
  :input-contract (and (listp x) (listp acc)) 
  :output-contract (listp (revt x acc)) 
  (if (endp x)
    acc 
    (revt (rest x) (cons (first x) acc))))

; Here is the definition of rev*, the function that uses revt to
; efficiently compute rev.

(defunc rev* (x) 
  :input-contract (listp x) 
  :output-contract (listp (rev* x)) 
  (revt x ()))

; Q1. Fill in the ...'s below so that both revt-lemma and
; rev*-rev, below, are theorems that ACL2s accepts. Notice that
; for theorems like this that relate the tail recursive version
; of a function with the simple recursive definition, we will
; always use rewrite rules that have the tail recursive function
; on the lhs so that when the rule applies, an expression
; containing the tail recursive function gets turned into an
; expression involving the simple recursive definition. Make sure
; to use the same approach for the rest of the questions in this
; homework.

(defthm revt-lemma 
  (implies (and (listp x) 
                (listp acc))
           (equal (revt x acc) 
                  (app (rev x) acc))))

(defthm rev*-rev
  (implies (listp x)
           (equal (rev* x)  
                  (rev x))))

; This is the data definition for a list of lists and
; accompanying theorems.

(defunc listlistp (l)
  :input-contract t
  :output-contract (booleanp (listlistp l))
  (if (atom l) 
    (equal l ())
    (and (listp (first l))
         (listlistp (rest l)))))

(defthm app-listlistp
  (implies (and (listlistp l)
                (listlistp x))
           (listlistp (acl2::append l x))))

(defthm listlistp-listp
  (implies (listlistp l)
           (acl2::true-listp l))
  :rule-classes :forward-chaining)

(defthm rev-listlistp
  (implies (listlistp l)
           (listlistp (rev l))))

; Here is a recursive definition for rev-all, a function that
; given a list of lists, reverses every element of the top-level
; list.

(defunc rev-all (l)
  :input-contract (listlistp l)
  :output-contract (listlistp (rev-all l))
  (if (endp l)
      ()
    (cons (rev (first l)) 
          (rev-all (rest l)))))

; A few lemmas

(defthm revall-listlistp
  (implies (listlistp x)
           (listlistp (rev-all x)))
  :rule-classes ((:type-prescription) (:forward-chaining)))

(defthm revall-tlp
  (implies (listlistp x)
           (acl2::true-listp (rev-all x)))
  :rule-classes ((:type-prescription) (:forward-chaining)))

; Here is the tail recursive definition. 

(defunc rev-all-t (l acc)
  :input-contract (and (listlistp l) (listlistp acc))
  :output-contract (listlistp (rev-all-t l acc))
  (if (endp l)
    acc
    (rev-all-t (rest l) (cons (rev (first l)) acc))))

(defthm revall-t-tlp
  (implies (and (listlistp l)
                (listlistp acc))
           (acl2::true-listp (rev-all-t l acc)))
  :rule-classes ((:type-prescription) (:forward-chaining)))

(defunc rev-all* (l)
  :input-contract (listlistp l)
  :output-contract (listlistp (rev-all* l))
  (rev (rev-all-t l ())))  

; Q2. Fill in the ...'s below so that ACL2s accepts
; rev-all-t-lemma and rev-all*-rev-all. See the comment before Q1 
; regarding how to write such lemmas.

(defthm rev-all-t-lemma
  (implies (and (listlistp l) (listlistp acc))
           (equal (rev-all-t l acc)
                  (app (rev (rev-all l)) acc))))

(defthm rev-all*-rev-all
  (implies (listlistp l)
           (equal (rev-all* l)
                  (rev-all l))))

(defdata natlist (listof nat))

(defthm natlistp-natp
  (implies (and (natp x)
                (natlistp y))
           (natlistp (cons x y))))

(defunc sum-of-squares (l)
  :input-contract (natlistp l)
  :output-contract (natp (sum-of-squares l))
  (if (endp l)
    0
    (+ (* (first l) (first l))
       (sum-of-squares (rest l)))))

(defunc sum-of-squares-t (l acc)
  :input-contract (and (natlistp l) (natp acc))
  :output-contract (natp (sum-of-squares-t l acc))
  (if (endp l)
    acc
    (sum-of-squares-t (rest l) (+ acc (* (first l) (first l))))))

(defunc sum-of-squares* (l)
  :input-contract (natlistp l)
  :output-contract (natp (sum-of-squares* l))
  (sum-of-squares-t l 0))

; Q3. Fill in the ...'s below so that ACL2s accepts
; sum-of-squares-t-lemma and sum-of-squares*-sos.

(defthm sum-of-squares-t-lemma
  (implies (and (natlistp l) (natp acc))
           (equal (sum-of-squares-t l acc)
                  (+ acc (sum-of-squares l)))))

(defthm sum-of-squares*-sos
  (implies (natlistp l) 
           (equal (sum-of-squares* l) 
                  (sum-of-squares l))))

(defunc delete (x l)
  :input-contract (listp l)
  :output-contract (listp (delete x l))
  (cond ((endp l) ())
        ((equal x (first l)) (delete x (rest l)))
        (t (cons (first l) (delete x (rest l))))))

(defunc delete-t (x l acc)
  :input-contract (and (listp l) (listp acc))
  :output-contract (listp (delete-t x l acc))
  (cond ((endp l) acc)
        ((equal x (first l)) (delete-t x (rest l) acc))
        (t (delete-t x (rest l) (app acc (list (first l)))))))

(defunc delete* (x l)
  :input-contract (listp l)
  :output-contract (listp (delete* x l))
  (delete-t x l ()))

; Q4. Fill in the ...'s below so that ACL2s accepts
; delete-t-lemma and delete*-delete.

(defthm delete-t-lemma
  (implies (and (listp l) (listp acc))
           (equal (delete-t x l acc)
                  (app acc (delete x l)))))

(defthm delete*-delete
  (implies (listp l) 
           (equal (delete* x l) 
                  (delete x l))))


(defunc how-many (e l)
  :input-contract (listp l)
  :output-contract (natp (how-many e l))
  (if (endp l)
    0
    (+ (if (equal e (first l)) 1 0)
       (how-many e (rest l)))))

(defunc how-many-t (e l acc)
  :input-contract (and (listp l) (natp acc))
  :output-contract (natp (how-many-t e l acc))
  (if (endp l)
    acc
    (how-many-t e (rest l) (+ (if (equal e (first l)) 1 0) acc))))

(defunc how-many* (e l)
  :input-contract (listp l)
  :output-contract (natp (how-many* e l))
  (how-many-t e l 0))

; Q5. Fill in the ...'s below so that ACL2s accepts
; how-many-t-lemma and how-many*-how-many.

(defthm how-many-t-lemma
  (implies (and (listp l) (natp acc))
           (equal (how-many-t e l acc)
                  (+ acc (how-many e l)))))

(defthm how-many*-how-many
  (implies (listp l) 
           (equal (how-many* x l) 
                  (how-many x l))))

(defunc ! (n)
  :input-contract (natp n)
  :output-contract (posp (! n))
  (if (equal n 0)
    1
    (* n (! (- n 1)))))

(defunc !-t (n acc)
  :input-contract (and (natp n) (posp acc))
  :output-contract (posp (!-t n acc))
  (if (equal n 0)
    acc
    (!-t (- n 1) (* n acc))))

; Q6. Fill in the ...'s in the next *two* forms below so that
; ACL2s accepts !*, !-t-lemma, !*-!.

(defunc !* (n)
  :input-contract (natp n)
  :output-contract (posp (! n))
  (!-t n 1))

(defthm !-t-lemma
   (implies (and (natp n) (posp acc))
            (equal (!-t n acc)
                   (* acc (! n)))))

(defthm !*-!
  (implies (natp n)
           (equal (!* n) 
                  (! n))))

(defunc pair (x y)
  :input-contract (and (listp x) (listp y) 
                       (equal (len x) (len y)))
  :output-contract (listp (pair x y))
  (if (endp x)
    ()
    (cons (list (first x) (first y)) (pair (rest x) (rest y)))))

(defunc pair-t (x y acc)
  :input-contract (and (listp x) (listp y) (listp acc) 
                       (equal (len x) (len y)))
  :output-contract (listp (pair-t x y acc))
  (if (endp x)
    acc
    (pair-t (rest x) (rest y) (cons (list (first x) (first y)) acc))))

; Q7. Fill in the ...'s in the next *two* forms below so that
; ACL2s accepts pair*, pair-t-lemma, and pair*-pair.

(defunc pair* (x y)
  :input-contract (and (listp x) (listp y) 
                       (equal (len x) (len y)))
  :output-contract (listp (pair* x y))
  (rev (pair-t x y nil)))


(defthm pair-t-lemma
  (implies (and (listp x) (listp y) (listp acc) 
                       (equal (len x) (len y)))
           (equal (pair-t x y acc)
                  (app (rev (pair x y)) acc))))

(defthm pair*-pair
  (implies (and (listp x) (listp y) 
                (equal (len x) (len y)))
           (equal (pair* x y)
                  (pair x y))))

(defunc merge-lists (x y)
  :input-contract (and (natlistp x) (natlistp y))
  :output-contract t
  (cond  ((endp x) y)
         ((endp y) x)
         ((< (first x) (first y))
          (cons (first x) (merge-lists (rest x) y)))
         (t (cons (first y) (merge-lists x (rest y))))))

(defthm merge-lists-output
  (implies (and (natlistp x) (natlistp y))
           (natlistp (merge-lists x y)))
  :hints (("goal" :induct (merge-lists-induction-scheme-from-definition x y))))

; Q8. Fill in the ...'s in the next *three* forms below so that
; ACL2s accepts merge-lists-t, merge-lists*, merge-lists-t-lemma,
; and merge-lists*-merge-lists.

(defunc merge-lists-t (x y acc)
  :input-contract (and (natlistp x) (natlistp y) (natlistp acc))
  :output-contract (natlistp (merge-lists-t x y acc))
  (cond  ((and (endp x) (endp y)) acc)
         ((endp x) (app (rev y) acc))
         ((endp y) (app (rev x) acc))
         ((< (first x) (first y))
          (merge-lists-t (rest x) y (cons (first x) acc)))
         (t (merge-lists-t x (rest y) (cons (first y) acc)))))


(defunc merge-lists* (x y)
  :input-contract (and (natlistp x) (natlistp y))
  :output-contract (natlistp (merge-lists x y))
  (rev (merge-lists-t x y ())))

(defthm merge-lists-t-lemma
  (implies (and (natlistp x) 
                (natlistp y)
                (natlistp acc))
           (equal (merge-lists-t x y acc)
                  (app (rev (merge-lists x y)) acc))))

(defthm merge-lists*-merge-lists 
  (implies (and (natlistp x) (natlistp y))
           (equal (merge-lists* x y)
                  (merge-lists x y))))#|ACL2s-ToDo-Line|#


#|

Please fill out the following form:
https://docs.google.com/spreadsheet/viewform?formkey=dDYzR1dLZmduamhaU19HSWt0dGp5T0E6MA

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