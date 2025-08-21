(in-package #:audits)

(rove:deftest basic-rove-test
  "A basic test to confirm Rove is working correctly."
  (rove:ok (= 1 1) "Confirming that 1 equals 1."))

(rove:run-suite *package*)
