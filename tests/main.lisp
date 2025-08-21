(defpackage iiscv/tests/main
  (:use :cl
        :iiscv
        :rove))
(in-package :iiscv/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :iiscv)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
