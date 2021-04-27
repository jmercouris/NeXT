;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(plan nil)

;; Let k={0,1,2,...}, and f^k[i] url-list be a notation for the
;; following sexp (nth i (nyxt::recursive-links k url-list)).

;; The following properties hold:

;; 1) f^k[i] url-list = f^i[i] url-list
;; 2) f^k[k] url-list = f^k-i[k-i] . f^k[i] url-list

;; Test cases:

(defvar *url-a* (quri:uri "a")
  "Mock URL of a web page A. ")

(defvar *url-b* (quri:uri "b")
  "Mock URL of a web page B.")

;; Take the following case of a page A that loops to itself, i.e. it has
;; a single link to itself.  This is a one-cycle.

;; A -links-to-> A (...)

(defun one-cycle (url)
  (list url))

(defun recursive-links-one-cycle (depth url-list)
  "Same as `recursive-links', but using `one-cycle' to compute the links
of URLs."
  (nyxt::recursive-links depth url-list #'one-cycle))

;; The following must hold:

;; f^k   '()  = nil
;; f^0   '(a) = '('(a))
;; f^k+1 '(a) = '('(a) nil)

(is (recursive-links-one-cycle (random 10000) nil)
    nil)
(is (recursive-links-one-cycle 0 (list *url-a*))
    (list (list *url-a*)))
(is (recursive-links-one-cycle (+ (random 10000) 1) (list *url-a*))
    (list (list *url-a*) nil))

;; Take the following case of two web pages, A and B, such that each is
;; a link to the other.  This is a two-cycle.

;; A -links-to-> B -links-to-> A (...)

(defun two-cycle (url)
  (if (quri:uri= url *url-a*)
      (list *url-b*)
      (list *url-a*)))

(defun recursive-links-two-cycle (depth url-list)
  "Same as `recursive-links', but using `two-cycle' to compute the links
of URLs."
  (nyxt::recursive-links depth url-list #'two-cycle))

;; The following must hold:

;; f^k   '()    = nil
;; f^0   '(a)   = '('(a))
;; f^1   '(a)   = '('(a) '(b))
;; f^k+2 '(a)   = '('(a) '(b) nil)
;; f^k+2 '(b)   = '('(b) '(a) nil)
;; f^k+2 '(a b) = '('(a b) nil)

(is (recursive-links-two-cycle (random 10000) nil)
    nil)
(is (recursive-links-two-cycle 0 (list *url-a*))
    (list (list *url-a*)))
(is (recursive-links-two-cycle 1 (list *url-a*))
    (list (list *url-a*) (list *url-b*)))
(is (recursive-links-two-cycle (+ (random 10000) 2) (list *url-a*))
    (list (list *url-a*) (list *url-b*) nil))
(is (recursive-links-two-cycle (+ (random 10000) 2) (list *url-b*))
    (list (list *url-b*) (list *url-a*) nil))
(is (recursive-links-two-cycle (+ (random 10000) 2) (list *url-a* *url-b*))
    (list (list *url-a* *url-b*) nil))

(finalize)

;; TODO
;; write tests that test the properties 1 and 2 above.
;; test if the links for any given url are computed once and only once.
