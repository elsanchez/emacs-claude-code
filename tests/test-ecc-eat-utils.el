;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-07-23 00:00:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-eat-utils.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'ecc-eat-utils)

;;; Tests for ecc-eat-utils

(ert-deftest test--ecc-eat-detect-extension ()
  "Test extension detection based on content patterns."
  (should (string= "py" (--ecc-eat-detect-extension "import os\ndef main():")))
  (should (string= "js" (--ecc-eat-detect-extension "function test() {")))
  (should (string= "el" (--ecc-eat-detect-extension "(defun test-func ()")))
  (should (string= "sh" (--ecc-eat-detect-extension "#!/bin/bash\nfunction test() {")))
  (should (string= "html" (--ecc-eat-detect-extension "<html><body>")))
  (should (string= "css" (--ecc-eat-detect-extension ".main-content {")))
  (should (string= "txt" (--ecc-eat-detect-extension "Just some plain text"))))

(ert-deftest test--ecc-eat-buffer-p ()
  "Test eat buffer detection."
  ;; Test with current buffer (should be nil since we're not in eat-mode)
  (should-not (--ecc-eat-buffer-p))
  
  ;; Test with explicit buffer
  (with-temp-buffer
    (should-not (--ecc-eat-buffer-p (current-buffer)))))

(ert-deftest test--ecc-eat-process-live-p ()
  "Test eat process detection."
  ;; Should return nil when not in eat-mode
  (should-not (--ecc-eat-process-live-p)))

(ert-deftest test--ecc-eat-get-buffer-content ()
  "Test getting buffer content."
  ;; Should return nil when not in eat-mode
  (should-not (--ecc-eat-get-buffer-content)))

(ert-deftest test--ecc-eat-send-functions ()
  "Test eat send functions don't error when not in eat-mode."
  ;; These should not error even when not in eat-mode
  (should-not (--ecc-eat-send-command "test"))
  (should-not (--ecc-eat-send-string "test"))
  (should-not (--ecc-eat-send-return)))

(ert-deftest test-ecc-eat-mode-hook ()
  "Test eat mode hook function."
  ;; Should not error when called
  (should-not (ecc-eat-mode-hook)))

(ert-deftest test--ecc-eat-optimize-scrolling ()
  "Test scrolling optimization function."
  ;; Should not error when called outside eat-mode
  (should-not (--ecc-eat-optimize-scrolling)))

(ert-deftest test--ecc-eat-setup-buffer ()
  "Test buffer setup function."
  ;; Should not error when called outside eat-mode
  (should-not (--ecc-eat-setup-buffer)))

;;; Mock tests for eat-mode functionality
;;; Note: These tests simulate eat-mode behavior since we can't easily
;;; create real eat buffers in test environment

(ert-deftest test-eat-mode-simulation ()
  "Test functions with simulated eat-mode."
  (with-temp-buffer
    ;; Mock derived-mode-p to return true for eat-mode
    (cl-letf (((symbol-function 'derived-mode-p)
               (lambda (mode) (eq mode 'eat-mode))))
      
      ;; Test buffer detection
      (should (--ecc-eat-buffer-p))
      
      ;; Test content retrieval
      (insert "test content")
      (should (string= "test content" (--ecc-eat-get-buffer-content)))
      
      ;; Test scrolling optimization (should not error)
      (should-not (--ecc-eat-optimize-scrolling))
      
      ;; Test buffer setup (should not error)
      (should-not (--ecc-eat-setup-buffer)))))

(provide 'test-ecc-eat-utils)