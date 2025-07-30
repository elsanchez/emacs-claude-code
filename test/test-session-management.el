;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: Claude Code (Phase 3 Milestone 3)
;;; Timestamp: <2025-07-18 12:15:00>
;;; File: test-session-management.el

;;; Copyright (C) 2025 Claude Code Enhancement

;; Test suite for Smart Session Management features

(require 'ert)
(require 'ecc-session-manager)
(require 'ecc-context-manager)
(require 'ecc-search-history)
(require 'ecc-project-memory)
(require 'ecc-decision-tracker)

;; Test fixtures
(defvar test-session-data nil)
(defvar test-project-root nil)
(defvar test-temp-dir nil)

(defun setup-test-environment ()
  "Set up test environment."
  (setq test-temp-dir (make-temp-file "ecc-test-" t))
  (setq test-project-root test-temp-dir)
  (setq ecc-session-save-directory (expand-file-name "sessions/" test-temp-dir))
  (setq ecc-session-use-memory-bank nil) ; Use local storage for tests
  (setq test-session-data
        `(:id "test-session-001"
          :created ,(current-time)
          :last-updated ,(current-time)
          :project-root ,test-project-root
          :buffers ((:buffer-name "test-buffer"
                     :content "test content"
                     :timestamp ,(current-time)))
          :conversation-history ((:type user-input
                                  :content "test user input"
                                  :timestamp ,(current-time))
                                 (:type claude-response
                                  :content "test claude response"
                                  :timestamp ,(current-time))))))

(defun teardown-test-environment ()
  "Clean up test environment."
  (when (and test-temp-dir (file-directory-p test-temp-dir))
    (delete-directory test-temp-dir t))
  (setq test-temp-dir nil)
  (setq test-project-root nil)
  (setq ecc-session--current-session nil)
  (setq ecc-decision--current-decisions nil)
  (clrhash ecc-project-memory--cache))

;; Session Manager Tests
;; ----------------------------------------

(ert-deftest test-session-id-generation ()
  "Test session ID generation."
  (setup-test-environment)
  (unwind-protect
      (let ((id1 (ecc-session-generate-id))
            (id2 (ecc-session-generate-id)))
        (should (stringp id1))
        (should (stringp id2))
        (should (not (string= id1 id2)))
        (should (string-match-p "^session-[0-9]\\{8\\}-[0-9]\\{6\\}-[0-9]+$" id1)))
    (teardown-test-environment)))

(ert-deftest test-session-creation ()
  "Test session creation."
  (setup-test-environment)
  (unwind-protect
      (let ((session-id (ecc-session-create-session)))
        (should (stringp session-id))
        (should ecc-session--current-session)
        (should (string= session-id (plist-get ecc-session--current-session :id)))
        (should (plist-get ecc-session--current-session :created))
        (should (plist-get ecc-session--current-session :project-root)))
    (teardown-test-environment)))

(ert-deftest test-session-persistence ()
  "Test session save and load."
  (setup-test-environment)
  (unwind-protect
      (let ((session test-session-data)
            (filename "test-session.json"))
        ;; Test save
        (should (ecc-session-save-to-file session filename))
        (let ((filepath (expand-file-name filename ecc-session-save-directory)))
          (should (file-exists-p filepath)))
        
        ;; Test load
        (let ((loaded-session (ecc-session-load-from-file filename)))
          (should loaded-session)
          (should (string= (plist-get loaded-session :id) 
                          (plist-get session :id)))
          (should (plist-get loaded-session :project-root))))
    (teardown-test-environment)))

(ert-deftest test-conversation-history ()
  "Test conversation history management."
  (setup-test-environment)
  (unwind-protect
      (progn
        (ecc-session-create-session)
        (ecc-session-add-conversation-entry 'user-input "Test user input")
        (ecc-session-add-conversation-entry 'claude-response "Test response")
        
        (let ((history (plist-get ecc-session--current-session :conversation-history)))
          (should (= (length history) 2))
          (should (eq (plist-get (car history) :type) 'claude-response))
          (should (eq (plist-get (cadr history) :type) 'user-input))))
    (teardown-test-environment)))

;; Context Manager Tests
;; ----------------------------------------

(ert-deftest test-context-capture ()
  "Test context capture."
  (setup-test-environment)
  (unwind-protect
      (let ((context (ecc-context-capture-current-state)))
        (should (plist-get context :timestamp))
        (should (plist-get context :emacs-version))
        (should (plist-get context :working-directory))
        (should (plist-get context :vterm-buffers)))
    (teardown-test-environment)))

(ert-deftest test-context-persistence ()
  "Test context save and load."
  (setup-test-environment)
  (unwind-protect
      (let ((context `(:timestamp ,(current-time)
                      :test-data "test-value"
                      :working-directory ,test-project-root)))
        ;; Test save
        (should (ecc-context-save-to-file context))
        
        ;; Test load
        (let ((loaded-context (ecc-context-load-from-file)))
          (should loaded-context)
          (should (string= (plist-get loaded-context :test-data) "test-value"))
          (should (plist-get loaded-context :timestamp))))
    (teardown-test-environment)))

;; Search History Tests
;; ----------------------------------------

(ert-deftest test-search-functionality ()
  "Test search functionality."
  (setup-test-environment)
  (unwind-protect
      (progn
        ;; Create test session with conversation
        (ecc-session-create-session)
        (ecc-session-add-conversation-entry 'user-input "How to implement feature X")
        (ecc-session-add-conversation-entry 'claude-response "Feature X can be implemented using...")
        
        ;; Test search
        (let ((results (ecc-search-conversations "feature")))
          (should (> (length results) 0))
          (should (cl-some (lambda (r) (string-match-p "feature" 
                                                      (downcase (plist-get r :content))))
                          results))))
    (teardown-test-environment)))

(ert-deftest test-search-by-type ()
  "Test search by conversation type."
  (setup-test-environment)
  (unwind-protect
      (progn
        (ecc-session-create-session)
        (ecc-session-add-conversation-entry 'user-input "user query")
        (ecc-session-add-conversation-entry 'claude-response "claude answer")
        
        ;; Test search user inputs only
        (let ((results (ecc-search-by-type "query" 'user-input)))
          (should (> (length results) 0))
          (should (cl-every (lambda (r) (eq (plist-get r :entry-type) 'user-input))
                           results))))
    (teardown-test-environment)))

;; Project Memory Tests
;; ----------------------------------------

(ert-deftest test-project-detection ()
  "Test project detection functionality."
  (setup-test-environment)
  (unwind-protect
      (let ((project-root (ecc-project-memory--get-project-root)))
        (should (stringp project-root))
        (should (file-directory-p project-root))
        
        (let ((project-name (ecc-project-memory--get-project-name project-root)))
          (should (stringp project-name))
          (should (> (length project-name) 0)))
        
        (let ((project-key (ecc-project-memory--get-project-key project-root)))
          (should (stringp project-key))
          (should (= (length project-key) 32)))) ; MD5 hash length
    (teardown-test-environment)))

(ert-deftest test-tech-stack-detection ()
  "Test technology stack detection."
  (setup-test-environment)
  (unwind-protect
      (progn
        ;; Create test files
        (write-region "" nil (expand-file-name "package.json" test-project-root))
        (write-region "" nil (expand-file-name "requirements.txt" test-project-root))
        
        (let ((tech-stack (ecc-project-memory--detect-tech-stack test-project-root)))
          (should (member "Node.js/JavaScript" tech-stack))
          (should (member "Python" tech-stack))))
    (teardown-test-environment)))

(ert-deftest test-project-memory-persistence ()
  "Test project memory save and load."
  (setup-test-environment)
  (unwind-protect
      (progn
        (let ((memory (ecc-project-memory--create-memory-structure test-project-root)))
          (should (plist-get memory :project-root))
          (should (plist-get memory :project-name))
          (should (plist-get memory :created))
          
          ;; Test save
          (should (ecc-project-memory--save-to-storage test-project-root memory))
          
          ;; Test load
          (let ((loaded-memory (ecc-project-memory--load-from-storage test-project-root)))
            (should loaded-memory)
            (should (string= (plist-get loaded-memory :project-root) 
                            (plist-get memory :project-root))))))
    (teardown-test-environment)))

;; Decision Tracker Tests
;; ----------------------------------------

(ert-deftest test-decision-creation ()
  "Test decision creation."
  (setup-test-environment)
  (unwind-protect
      (let ((decision (ecc-decision-create-entry "Test Decision" 
                                                  "This is a test decision"
                                                  'implementation 
                                                  'high)))
        (should (plist-get decision :id))
        (should (string= (plist-get decision :title) "Test Decision"))
        (should (string= (plist-get decision :description) "This is a test decision"))
        (should (eq (plist-get decision :category) 'implementation))
        (should (eq (plist-get decision :importance) 'high))
        (should (plist-get decision :timestamp))
        (should (plist-get decision :author)))
    (teardown-test-environment)))

(ert-deftest test-decision-auto-categorization ()
  "Test automatic decision categorization."
  (setup-test-environment)
  (unwind-protect
      (progn
        (should (eq (ecc-decision-auto-categorize "Fix bug in parser" "Error handling issue")
                   'bug-fix))
        (should (eq (ecc-decision-auto-categorize "Add new feature" "Implement user authentication")
                   'feature))
        (should (eq (ecc-decision-auto-categorize "Refactor code" "Clean up old functions")
                   'refactoring))
        (should (eq (ecc-decision-auto-categorize "Optimize performance" "Speed up database queries")
                   'optimization)))
    (teardown-test-environment)))

(ert-deftest test-decision-management ()
  "Test decision management operations."
  (setup-test-environment)
  (unwind-protect
      (progn
        ;; Add decision
        (let ((decision (ecc-decision-add-decision "Test Decision" "Test description")))
          (should decision)
          (should (member decision ecc-decision--current-decisions))
          
          ;; Find decision
          (let ((found (ecc-decision-find-by-id (plist-get decision :id))))
            (should found)
            (should (string= (plist-get found :title) "Test Decision")))
          
          ;; Update decision
          (let ((updated (ecc-decision-update-decision (plist-get decision :id)
                                                       '(:status implemented))))
            (should updated)
            (should (eq (plist-get updated :status) 'implemented)))))
    (teardown-test-environment)))

(ert-deftest test-decision-persistence ()
  "Test decision persistence."
  (setup-test-environment)
  (unwind-protect
      (progn
        ;; Add test decisions
        (ecc-decision-add-decision "Decision 1" "Description 1")
        (ecc-decision-add-decision "Decision 2" "Description 2")
        
        ;; Save decisions
        (ecc-decision-save-decisions)
        
        ;; Clear current decisions
        (setq ecc-decision--current-decisions nil)
        
        ;; Load decisions
        (ecc-decision-load-decisions)
        
        ;; Verify loaded decisions
        (should (= (length ecc-decision--current-decisions) 2))
        (should (cl-some (lambda (d) (string= (plist-get d :title) "Decision 1"))
                        ecc-decision--current-decisions)))
    (teardown-test-environment)))

;; Integration Tests
;; ----------------------------------------

(ert-deftest test-session-integration ()
  "Test integration between session components."
  (setup-test-environment)
  (unwind-protect
      (progn
        ;; Create session
        (ecc-session-create-session)
        
        ;; Add conversation
        (ecc-session-add-conversation-entry 'user-input "Test integration")
        
        ;; Add decision
        (ecc-decision-add-decision "Integration Decision" "Test decision")
        
        ;; Capture context
        (let ((context (ecc-context-capture-current-state)))
          (should (plist-get context :session-id))
          (should (string= (plist-get context :session-id)
                          (plist-get ecc-session--current-session :id))))
        
        ;; Search should find the conversation
        (let ((results (ecc-search-conversations "integration")))
          (should (> (length results) 0))))
    (teardown-test-environment)))

(ert-deftest test-memory-bank-fallback ()
  "Test fallback to local storage when Memory Bank fails."
  (setup-test-environment)
  (unwind-protect
      (progn
        ;; Mock Memory Bank failure
        (setq ecc-session-use-memory-bank nil)
        
        ;; Create and save session
        (ecc-session-create-session)
        (ecc-session-save-current)
        
        ;; Verify local file exists
        (let ((files (directory-files ecc-session-save-directory nil "\\.json$")))
          (should (> (length files) 0))))
    (teardown-test-environment)))

;; Performance Tests
;; ----------------------------------------

(ert-deftest test-search-performance ()
  "Test search performance with large dataset."
  (setup-test-environment)
  (unwind-protect
      (progn
        ;; Create large dataset
        (ecc-session-create-session)
        (dotimes (i 100)
          (ecc-session-add-conversation-entry 'user-input (format "User input %d" i))
          (ecc-session-add-conversation-entry 'claude-response (format "Response %d" i)))
        
        ;; Measure search time
        (let ((start-time (current-time)))
          (ecc-search-conversations "input")
          (let ((elapsed (float-time (time-subtract (current-time) start-time))))
            (should (< elapsed 1.0))))) ; Should complete within 1 second
    (teardown-test-environment)))

;; Error Handling Tests
;; ----------------------------------------

(ert-deftest test-error-handling ()
  "Test error handling in session management."
  (setup-test-environment)
  (unwind-protect
      (progn
        ;; Test with invalid session ID
        (should-not (ecc-session-load-from-file "nonexistent-session.json"))
        
        ;; Test with invalid decision ID
        (should-not (ecc-decision-find-by-id "nonexistent-decision"))
        
        ;; Test with invalid project root
        (should-not (ecc-project-memory--load-from-storage "/nonexistent/path")))
    (teardown-test-environment)))

;; Utility function to run all tests
(defun run-session-management-tests ()
  "Run all session management tests."
  (interactive)
  (ert "test-session-\\|test-context-\\|test-search-\\|test-project-\\|test-decision-"))

(provide 'test-session-management)

;;; test-session-management.el ends here