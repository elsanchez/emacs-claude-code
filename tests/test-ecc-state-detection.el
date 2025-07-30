;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-28 07:55:56>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-state-detection.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'ert)
(require 'ecc-state-detection)

(ert-deftest test-ecc-state-detection-feature-loads-without-error ()
  "Test that ecc-state-detection feature loads successfully without errors."
  (should (featurep 'ecc-state-detection)))

(ert-deftest test-ecc-state-detection-patterns-is-list ()
  "Test that state detection patterns is a list."
  (should (listp --ecc-state-detection-patterns)))

(ert-deftest test-ecc-state-detection-patterns-not-empty ()
  "Test that state detection patterns list is not empty."
  (should (> (length --ecc-state-detection-patterns) 0)))

(ert-deftest test-ecc-state-detection-buffer-size-is-integer ()
  "Test that buffer size is an integer."
  (should (integerp --ecc-state-detection-buffer-size)))

(ert-deftest test-ecc-state-detection-buffer-size-positive ()
  "Test that buffer size is positive."
  (should (> --ecc-state-detection-buffer-size 0)))

(ert-deftest test-ecc-state-detection-yn-pattern ()
  "Test Y/N state detection."
  (with-temp-buffer
    (insert "Some text ❯ 1. Yes more text")
    (should (eq (--ecc-state-detection-detect) :y/n))))

(ert-deftest test-ecc-state-detection-yyn-pattern ()
  "Test Y/Y/N state detection."
  (with-temp-buffer
    (insert "Some text ❯ 1. Yes\n 2. Yes, and more text")
    (should (eq (--ecc-state-detection-detect) :y/y/n))))


(ert-deftest test-ecc-state-detection-no-match ()
  "Test that nil is returned when no pattern matches."
  (with-temp-buffer
    (insert "Just regular text without any patterns")
    (should-not (--ecc-state-detection-detect))))

(ert-deftest test-ecc-state-detection-get-name-yn ()
  "Test that :y/n state converts to 'Y/N' name."
  (should (string= (--ecc-state-detection-get-name :y/n) "Y/N")))

(ert-deftest test-ecc-state-detection-get-name-waiting ()
  "Test that :waiting state converts to 'Continue' name."
  (should (string= (--ecc-state-detection-get-name :waiting) "Continue")))

(ert-deftest test-ecc-state-detection-error-pattern ()
  "Test error state detection."
  (with-temp-buffer
    (insert "Some text with Error: Something went wrong")
    (should (eq (--ecc-state-detection-detect) :error-state))))

(ert-deftest test-ecc-state-detection-timeout-pattern ()
  "Test timeout state detection."
  (with-temp-buffer
    (insert "Request timed out after 30 seconds")
    (should (eq (--ecc-state-detection-detect) :timeout))))

(ert-deftest test-ecc-state-detection-thinking-pattern ()
  "Test thinking state detection."
  (with-temp-buffer
    (insert "Claude is thinking about your request...")
    (should (eq (--ecc-state-detection-detect) :thinking))))

(ert-deftest test-ecc-state-detection-flexible-yn-pattern ()
  "Test flexible Y/N state detection."
  (with-temp-buffer
    (insert "❯ Choose: Yes or No")
    (should (eq (--ecc-state-detection-detect) :y/n))))

(ert-deftest test-ecc-state-detection-flexible-waiting-pattern ()
  "Test flexible waiting state detection."
  (with-temp-buffer
    (insert "│ > Type your message here")
    (should (eq (--ecc-state-detection-detect) :waiting))))

(ert-deftest test-ecc-state-detection-get-name-new-states ()
  "Test that new states convert to proper names."
  (should (string= (--ecc-state-detection-get-name :thinking) "Thinking"))
  (should (string= (--ecc-state-detection-get-name :error-state) "Error"))
  (should (string= (--ecc-state-detection-get-name :timeout) "Timeout")))

(ert-deftest test-ecc-state-detection-adaptive-buffer-size-config ()
  "Test that adaptive buffer size configuration variables exist."
  (should (boundp '--ecc-state-detection-adaptive-buffer-size))
  (should (boundp '--ecc-state-detection-max-buffer-size)))

(ert-deftest test-ecc-state-detection-optimal-buffer-size-small-buffer ()
  "Test optimal buffer size for small buffers."
  (with-temp-buffer
    (insert "small content")
    (should (<= (--ecc-state-detection--get-optimal-buffer-size) 512))))

(ert-deftest test-ecc-state-detection-optimal-buffer-size-disabled ()
  "Test optimal buffer size when adaptive sizing is disabled."
  (let ((--ecc-state-detection-adaptive-buffer-size nil))
    (with-temp-buffer
      (insert "content")
      (should (= (--ecc-state-detection--get-optimal-buffer-size) 
                 --ecc-state-detection-buffer-size)))))


(provide 'test-ecc-state-detection)

(when
    (not load-file-name)
  (message "test-ecc-state-detection.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))