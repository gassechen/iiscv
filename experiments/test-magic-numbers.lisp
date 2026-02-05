;;; test-magic-numbers.lisp
;;; Test Suite for Magic Number Detection

(in-package :iiscv)

;; Test 1: Basic Magic Numbers (already tested but verify)
(make-atomic-commit '(defun basic-magic-test (x)
                      (let ((result (+ x 10)))
                        (* result 2))))

;; Test 2: Magic Numbers in Constants (local constants - should NOT detect)
(make-atomic-commit '(defun local-constants-test (radius)
                      (let ((pi 3.14159)
                            (tau (* 2 pi)))
                        (* pi radius radius))))

;; Test 3: Magic Numbers in Parameters (should NOT detect)
(make-atomic-commit '(defun parameters-test (base multiplier)
                      (let ((result (* base multiplier)))
                        (+ result 100))))

;; Test 4: Magic Numbers in Mathematical Constants (should NOT detect)
(make-atomic-commit '(defun math-constants-test (x)
                      (let ((e 2.71828)
                            (golden-ratio 1.61803)
                            (sqrt2 1.41421))
                        (+ x e golden-ratio sqrt2))))

;; Test 5: Magic Numbers in Common Constants (should NOT detect)
(make-atomic-commit '(defun common-constants-test (seconds)
                      (let ((minutes (* seconds 60))
                            (hours (* minutes 60))
                            (days (* hours 24)))
                        days)))

;; Test 6: Magic Numbers in Scientific Constants (should NOT detect)
(make-atomic-commit '(defun scientific-constants-test (mass)
                      (let ((g 9.81) ; gravity
                            (c 299792458) ; speed of light
                            (h 6.62607015e-34)) ; Planck constant
                        (* mass g))))

;; Test 7: Magic Numbers in Financial Constants (should NOT detect)
(make-atomic-commit '(defun financial-constants-test (amount)
                      (let ((tax-rate 0.21)
                            (conversion-rate 1.12))
                        (* amount tax-rate))))

;; Test 8: Magic Numbers in Status Codes (should NOT detect)
(make-atomic-commit '(defun status-codes-test (code)
                      (case code
                        (200 "OK")
                        (404 "Not Found")
                        (500 "Server Error")
                        (t "Unknown"))))

;; Test 9: Magic Numbers in Error Codes (should NOT detect)
(make-atomic-commit '(defun error-codes-test (error)
                      (case error
                        (1 "File not found")
                        (2 "Permission denied")
                        (3 "Out of memory")
                        (t "Unknown error"))))

;; Test 10: Magic Numbers in Configuration (should NOT detect)
(make-atomic-commit '(defun config-test (setting)
                      (case setting
                        (:debug t)
                        (:verbose 2)
                        (:quiet 0)
                        (t :normal))))

;; Test 11: Magic Numbers in Array Sizes (should NOT detect)
(make-atomic-commit '(defun array-test (n)
                      (let ((array (make-array 10)))
                        (dotimes (i n)
                          (setf (aref array i) i))
                        array)))

;; Test 12: Magic Numbers in Loop Bounds (should NOT detect)
(make-atomic-commit '(defun loop-test (n)
                      (loop for i from 1 to 100
                            do (format t "Iteration: ~A~%" i))
                      n)))

;; Test 13: Magic Numbers in Mathematical Operations (should NOT detect)
(make-atomic-commit '(defun math-test (x)
                      (let ((result (+ (* x 2) 10)))
                        (if (> result 100)
                            (values result "High")
                            (values result "Low")))))

;; Test 14: Magic Numbers in Conditional Checks (should NOT detect)
(make-atomic-commit '(defun conditional-test (value)
                      (if (< value 10)
                          "Low"
                          (if (< value 100)
                              "Medium"
                              "High"))))

;; Test 15: Magic Numbers in String Operations (should NOT detect)
(make-atomic-commit '(defun string-test (text)
                      (let ((len (length text)))
                        (if (< len 5)
                            "Short"
                            (if (< len 20)
                                "Medium"
                                "Long")))))

;; Test 16: Magic Numbers in Time Operations (should NOT detect)
(make-atomic-commit '(defun time-test (timestamp)
                      (let ((now (get-universal-time))
                            (one-hour (* 60 60))
                            (one-day (* one-hour 24)))
                        (- now timestamp))))

;; Test 17: Magic Numbers in File Operations (should NOT detect)
(make-atomic-commit '(defun file-test (filename)
                      (with-open-file (stream filename
                                            :direction :output
                                            :if-exists :supersede
                                            :element-type 'character)
                        (format stream "File content"))))

;; Test 18: Magic Numbers in System Operations (should NOT detect)
(make-atomic-commit '(defun system-test ()
                      (let ((max-retries 3)
                            (timeout 30))
                        (loop for i from 1 to max-retries
                              do (format t "Attempt ~A~%" i)
                                 (sleep timeout))
                        t)))

;; Test 19: Magic Numbers in Data Processing (should NOT detect)
(make-atomic-commit '(defun data-test (data)
                      (let ((threshold 0.5)
                            (max-items 100))
                        (remove-if-not #'numberp data :count max-items :key #'abs :test #'> threshold))))

;; Test 20: Magic Numbers in Complex Calculations (should NOT detect)
(make-atomic-commit '(defun complex-test (x y z)
                      (let ((a (+ x y))
                            (b (* y z))
                            (c (- a b)))
                        (/ (+ a b c) 3))))

(format t "~%~%=== MAGIC NUMBERS TESTS COMPLETED ===~%")
(format t "Total magic number tests: 20~%")
(format t "Run (show-atomic-commit) to view the commits~%")
(format t "Run (make-human-commit \"Magic number detection tests\") to consolidate~%")