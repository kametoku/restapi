(defpackage restapi/tests/main
  (:use :cl
        :restapi
        :rove))
(in-package :restapi/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :restapi)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
