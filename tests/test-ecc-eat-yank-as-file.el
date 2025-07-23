;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-07-23 00:00:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/tests/test-ecc-eat-yank-as-file.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'ert)
(require 'ecc-eat-yank-as-file)

;;; Tests for ecc-eat-yank-as-file

(ert-deftest test--ecc-get-kill-ring-content ()
  "Test getting content from kill ring."
  (let ((kill-ring '("test content" "older content")))
    (should (string= "test content" (--ecc-get-kill-ring-content))))
  
  (let ((kill-ring nil))
    (should-not (--ecc-get-kill-ring-content))))

(ert-deftest test--ecc-create-temp-file ()
  "Test temporary file creation."
  (let ((temp-file (--ecc-create-temp-file)))
    (should (stringp temp-file))
    (should (string-match "kill-ring-.*\\.tmp$" temp-file))
    (should (file-name-absolute-p temp-file))))

(ert-deftest test--ecc-write-content-to-file ()
  "Test writing content to file."
  (let ((temp-file (make-temp-file "ecc-test-" nil ".tmp"))
        (test-content "This is test content"))
    (unwind-protect
        (progn
          (--ecc-write-content-to-file test-content temp-file)
          (should (file-exists-p temp-file))
          (with-temp-buffer
            (insert-file-contents temp-file)
            (should (string= test-content (buffer-string)))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest test--ecc-send-read-command-eat ()
  "Test sending read command to eat."
  (let ((test-file "/tmp/test.txt")
        (sent-commands nil))
    
    ;; Mock eat functions
    (cl-letf (((symbol-function 'derived-mode-p)
               (lambda (mode) (eq mode 'eat-mode)))
              ((symbol-function 'eat-term-send-string)
               (lambda (terminal string)
                 (push string sent-commands))))
      
      ;; Mock eat-terminal variable
      (let ((eat-terminal t))
        (--ecc-send-read-command-eat test-file)
        
        ;; Should have sent command and return
        (should (= 2 (length sent-commands)))
        (should (string= "Read /tmp/test.txt" (nth 1 sent-commands)))))))

(ert-deftest test-emacs-claude-code-eat-yank-as-file-not-eat-mode ()
  "Test yank-as-file when not in eat-mode."
  (cl-letf (((symbol-function 'derived-mode-p)
             (lambda (mode) nil)))
    
    ;; Should show message about eat-mode requirement
    (should-not (emacs-claude-code-eat-yank-as-file))))

(ert-deftest test-emacs-claude-code-eat-yank-as-file-empty-kill-ring ()
  "Test yank-as-file with empty kill ring."
  (let ((kill-ring nil))
    (cl-letf (((symbol-function 'derived-mode-p)
               (lambda (mode) (eq mode 'eat-mode))))
      
      ;; Should show message about empty kill ring
      (should-not (emacs-claude-code-eat-yank-as-file)))))

(ert-deftest test-alias-backward-compatibility ()
  "Test that alias works for backward compatibility."
  (should (fboundp 'ecc-eat-yank-as-file))
  (should (eq (symbol-function 'ecc-eat-yank-as-file)
              (symbol-function 'emacs-claude-code-eat-yank-as-file))))

(ert-deftest test-localhost-detection ()
  "Test localhost detection in remote-info."
  (let ((kill-ring '("test content"))
        (created-files nil))
    
    (cl-letf (((symbol-function 'derived-mode-p)
               (lambda (mode) (eq mode 'eat-mode)))
              ((symbol-function '--ecc-create-temp-file)
               (lambda (&optional use-default-dir)
                 (let ((file "/tmp/test-file.tmp"))
                   (push file created-files)
                   file)))
              ((symbol-function '--ecc-write-content-to-file)
               (lambda (content file) t))
              ((symbol-function '--ecc-send-read-command-eat)
               (lambda (file) t)))
      
      ;; Test localhost
      (emacs-claude-code-eat-yank-as-file '((host . "localhost")))
      (should (= 1 (length created-files)))
      
      ;; Test 127.0.0.1
      (setq created-files nil)
      (emacs-claude-code-eat-yank-as-file '((host . "127.0.0.1")))
      (should (= 1 (length created-files))))))

(provide 'test-ecc-eat-yank-as-file)