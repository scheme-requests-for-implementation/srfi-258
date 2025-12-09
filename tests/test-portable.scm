;;; SPDX-FileCopyrightText: 2025 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

;;; Portable SRFI 64 test suite for SRFI 258 Uninterned symbols.

;;; Test runner

;; The SRFI 64 implementation used by most Schemes has a very basic
;; default test runner. This is slightly more helpful on failures.

(define (my-test-runner-factory)
  (letrec*
   ((runner (test-runner-null))
    (test-end
     (lambda (runner)
       (case (test-result-kind runner)
         ((pass)
          (display "Pass: ")
          (display (test-runner-test-name runner))
          (newline))
         ((fail)
          (display "FAIL: ")
          (display (test-runner-test-name runner))
          (display ". Expected ")
          (display (test-result-ref runner 'expected-value))
          (display ", got ")
          (display (test-result-ref runner 'actual-value))
          (display ".\n")))))
    (test-final
     (lambda (runner)
       (display "===============================\n")
       (display "Total passes: ")
       (display (test-runner-pass-count runner))
       (newline)
       (display "Total failures: ")
       (display (test-runner-fail-count runner))
       (newline))))

    (test-runner-on-test-end! runner test-end)
    (test-runner-on-final! runner test-final)
    runner))

(test-runner-factory my-test-runner-factory)

;;; Test suite

(test-begin "SRFI 258 Uninterned symbols")

(test-assert "Usym is symbol? (S->US)"
  (symbol? (string->uninterned-symbol "x")))

(test-assert "Usym is symbol? (GUS)"
  (symbol? (generate-uninterned-symbol)))

(test-assert "Usym is unique (S->US)"
  (not (eqv? (string->uninterned-symbol "x")
             (string->uninterned-symbol "x"))))

(test-assert "Usym is unique (GUS)"
  (not (eqv? (generate-uninterned-symbol)
             (generate-uninterned-symbol))))

(test-assert "Usym is eqv? to itself (S->US)"
  (let ((x (string->uninterned-symbol "x")))
    (eqv? x x)))

(test-assert "Usym is eqv? to itself (GUS)"
  (let ((x (generate-uninterned-symbol)))
    (eqv? x x)))

(test-assert "Usym is not symbol-interned? (S->US)"
  (not (symbol-interned? (string->uninterned-symbol "x"))))

(test-assert "Usym is not symbol-interned? (GUS)"
  (not (symbol-interned? (generate-uninterned-symbol))))

(test-assert "generate-uninterned-symbol with string prefix"
  (let* ((prefix "perfection")
         (g (generate-uninterned-symbol prefix)))
    (equal? prefix
            (substring (symbol->string g)
                       0
                       (string-length prefix)))))

(test-assert "generate-uninterned-symbol with symbol prefix"
  (let* ((prefix 'perfection)
         (ps (symbol->string prefix))
         (g (generate-uninterned-symbol prefix)))
    (equal? ps
            (substring (symbol->string g)
                       0
                       (string-length ps)))))

(test-end)
