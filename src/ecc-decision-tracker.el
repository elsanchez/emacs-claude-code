;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: Claude Code (Phase 3 Milestone 3)
;;; Timestamp: <2025-07-18 12:00:00>
;;; File: ecc-decision-tracker.el

;;; Copyright (C) 2025 Claude Code Enhancement

;; 1. Dependencies
;; ----------------------------------------
(require 'ecc-debug)
(require 'ecc-session-manager)
(require 'ecc-project-memory)
(require 'cl-lib)

;; 2. Configuration
;; ----------------------------------------

(defcustom ecc-decision-auto-save t
  "Whether to automatically save decision tracking data."
  :type 'boolean
  :group 'ecc)

(defcustom ecc-decision-max-entries 500
  "Maximum number of decision entries to keep."
  :type 'number
  :group 'ecc)

(defcustom ecc-decision-importance-levels '(critical high medium low)
  "Available importance levels for decisions."
  :type '(repeat symbol)
  :group 'ecc)

(defcustom ecc-decision-categories '(architecture design implementation refactoring bug-fix feature optimization)
  "Available categories for decisions."
  :type '(repeat symbol)
  :group 'ecc)

;; 3. Variables
;; ----------------------------------------

(defvar ecc-decision--global-cache (make-hash-table :test 'equal)
  "Global cache for decision tracking data.")

(defvar ecc-decision--current-decisions nil
  "Current session's decisions.")

(defvar ecc-decision--auto-categorize-patterns
  '(("bug\\|fix\\|error\\|issue" . bug-fix)
    ("feature\\|new\\|add\\|implement" . feature)
    ("refactor\\|clean\\|reorganize" . refactoring)
    ("optimize\\|performance\\|speed" . optimization)
    ("architecture\\|structure\\|design" . architecture)
    ("ui\\|interface\\|frontend" . design))
  "Patterns to automatically categorize decisions.")

;; 4. Core Decision Functions
;; ----------------------------------------

(defun ecc-decision-generate-id ()
  "Generate a unique decision ID."
  (format "decision-%s-%s" 
          (format-time-string "%Y%m%d-%H%M%S")
          (format "%04x" (random 65536))))

(defun ecc-decision-auto-categorize (title description)
  "Automatically categorize decision based on TITLE and DESCRIPTION."
  (let ((text (downcase (concat title " " description))))
    (or (cl-loop for (pattern . category) in ecc-decision--auto-categorize-patterns
                 when (string-match-p pattern text)
                 return category)
        'implementation)))

(defun ecc-decision-create-entry (title description &optional category importance)
  "Create a new decision entry."
  (let ((decision-id (ecc-decision-generate-id))
        (auto-category (ecc-decision-auto-categorize title description)))
    `(:id ,decision-id
      :title ,title
      :description ,description
      :category ,(or category auto-category)
      :importance ,(or importance 'medium)
      :timestamp ,(current-time)
      :author ,(user-login-name)
      :session-id ,(when ecc-session--current-session
                     (plist-get ecc-session--current-session :id))
      :project-root ,(or (when (fboundp 'projectile-project-root)
                           (projectile-project-root))
                         default-directory)
      :buffer ,(buffer-name (current-buffer))
      :file ,(buffer-file-name (current-buffer))
      :line ,(line-number-at-pos)
      :context ,(ecc-decision--capture-context)
      :rationale ""
      :alternatives ()
      :outcomes ()
      :related-decisions ()
      :status 'active
      :tags ())))

(defun ecc-decision--capture-context ()
  "Capture contextual information for decision."
  (let ((context '()))
    ;; Current function/class context
    (when (buffer-file-name)
      (save-excursion
        (let ((func-name (which-function)))
          (when func-name
            (push `(:type function :name ,func-name) context)))))
    
    ;; Git context
    (when (and (fboundp 'magit-get-current-branch)
               (magit-get-current-branch))
      (push `(:type git-branch :value ,(magit-get-current-branch)) context))
    
    ;; Recent changes context
    (when (buffer-modified-p)
      (push `(:type buffer-modified :value t) context))
    
    context))

;; 5. Decision Management
;; ----------------------------------------

(defun ecc-decision-add-decision (title description &optional category importance)
  "Add a new decision to tracking."
  (interactive 
   (list (read-string "Decision title: ")
         (read-string "Description: ")
         (when current-prefix-arg
           (intern (completing-read "Category: " 
                                   (mapcar 'symbol-name ecc-decision-categories))))
         (when current-prefix-arg
           (intern (completing-read "Importance: " 
                                   (mapcar 'symbol-name ecc-decision-importance-levels))))))
  
  (let ((decision (ecc-decision-create-entry title description category importance)))
    ;; Add to current session
    (push decision ecc-decision--current-decisions)
    
    ;; Add to project memory
    (condition-case err
        (ecc-project-memory-add-decision title description)
      (error
       (ecc-debug-message "Failed to add decision to project memory: %s" 
                          (error-message-string err))))
    
    ;; Save to storage
    (when ecc-decision-auto-save
      (ecc-decision-save-decisions))
    
    (message "Decision '%s' added and categorized as '%s'" 
             title (plist-get decision :category))
    
    decision))

(defun ecc-decision-update-decision (decision-id updates)
  "Update decision with DECISION-ID using UPDATES plist."
  (let ((decision (ecc-decision-find-by-id decision-id)))
    (when decision
      ;; Apply updates
      (cl-loop for (key value) on updates by 'cddr
               do (plist-put decision key value))
      
      ;; Update timestamp
      (plist-put decision :last-updated (current-time))
      
      ;; Save changes
      (when ecc-decision-auto-save
        (ecc-decision-save-decisions))
      
      (message "Decision '%s' updated" (plist-get decision :title))
      decision)))

(defun ecc-decision-find-by-id (decision-id)
  "Find decision by DECISION-ID."
  (cl-find-if (lambda (d) (string= (plist-get d :id) decision-id))
              ecc-decision--current-decisions))

(defun ecc-decision-add-outcome (decision-id outcome)
  "Add OUTCOME to decision with DECISION-ID."
  (interactive 
   (list (completing-read "Decision ID: " 
                         (mapcar (lambda (d) (plist-get d :id)) 
                                ecc-decision--current-decisions))
         (read-string "Outcome: ")))
  
  (let ((decision (ecc-decision-find-by-id decision-id)))
    (when decision
      (let ((outcomes (plist-get decision :outcomes))
            (new-outcome `(:outcome ,outcome
                          :timestamp ,(current-time)
                          :session-id ,(when ecc-session--current-session
                                         (plist-get ecc-session--current-session :id)))))
        (plist-put decision :outcomes (cons new-outcome outcomes))
        (when ecc-decision-auto-save
          (ecc-decision-save-decisions))
        (message "Outcome added to decision '%s'" (plist-get decision :title))))))

(defun ecc-decision-set-status (decision-id status)
  "Set STATUS for decision with DECISION-ID."
  (interactive 
   (list (completing-read "Decision ID: " 
                         (mapcar (lambda (d) (plist-get d :id)) 
                                ecc-decision--current-decisions))
         (intern (completing-read "Status: " 
                                '("active" "implemented" "deprecated" "cancelled")))))
  
  (ecc-decision-update-decision decision-id `(:status ,status)))

;; 6. Decision Analysis
;; ----------------------------------------

(defun ecc-decision-analyze-patterns ()
  "Analyze decision patterns and provide insights."
  (interactive)
  (let ((decisions ecc-decision--current-decisions)
        (category-counts (make-hash-table :test 'eq))
        (importance-counts (make-hash-table :test 'eq))
        (author-counts (make-hash-table :test 'equal))
        (monthly-counts (make-hash-table :test 'equal)))
    
    ;; Analyze decisions
    (dolist (decision decisions)
      (let ((category (plist-get decision :category))
            (importance (plist-get decision :importance))
            (author (plist-get decision :author))
            (timestamp (plist-get decision :timestamp)))
        
        ;; Count by category
        (cl-incf (gethash category category-counts 0))
        
        ;; Count by importance
        (cl-incf (gethash importance importance-counts 0))
        
        ;; Count by author
        (cl-incf (gethash author author-counts 0))
        
        ;; Count by month
        (let ((month (format-time-string "%Y-%m" timestamp)))
          (cl-incf (gethash month monthly-counts 0)))))
    
    ;; Display analysis
    (with-current-buffer (get-buffer-create "*Decision Analysis*")
      (erase-buffer)
      (insert "Decision Tracking Analysis\n")
      (insert "===========================\n\n")
      
      (insert (format "Total decisions: %d\n\n" (length decisions)))
      
      ;; Category analysis
      (insert "Decisions by Category:\n")
      (maphash (lambda (category count)
                 (insert (format "  %s: %d (%.1f%%)\n" 
                               category count 
                               (* 100.0 (/ count (float (length decisions)))))))
               category-counts)
      (insert "\n")
      
      ;; Importance analysis
      (insert "Decisions by Importance:\n")
      (maphash (lambda (importance count)
                 (insert (format "  %s: %d (%.1f%%)\n" 
                               importance count 
                               (* 100.0 (/ count (float (length decisions)))))))
               importance-counts)
      (insert "\n")
      
      ;; Author analysis
      (insert "Decisions by Author:\n")
      (maphash (lambda (author count)
                 (insert (format "  %s: %d\n" author count)))
               author-counts)
      (insert "\n")
      
      ;; Monthly trend
      (insert "Monthly Decision Trend:\n")
      (let ((sorted-months (sort (hash-table-keys monthly-counts) 'string<)))
        (dolist (month sorted-months)
          (insert (format "  %s: %d\n" month (gethash month monthly-counts)))))
      
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun ecc-decision-find-related (decision-id)
  "Find decisions related to DECISION-ID."
  (let ((decision (ecc-decision-find-by-id decision-id))
        (related '()))
    (when decision
      (let ((category (plist-get decision :category))
            (project-root (plist-get decision :project-root))
            (file (plist-get decision :file)))
        
        ;; Find by same category
        (dolist (other-decision ecc-decision--current-decisions)
          (when (and (not (string= (plist-get other-decision :id) decision-id))
                     (or (eq (plist-get other-decision :category) category)
                         (string= (plist-get other-decision :project-root) project-root)
                         (string= (plist-get other-decision :file) file)))
            (push other-decision related))))
      
      related)))

;; 7. Storage Functions
;; ----------------------------------------

(defun ecc-decision-save-decisions ()
  "Save decision tracking data to storage."
  (let ((data `(:decisions ,ecc-decision--current-decisions
                :last-updated ,(current-time)
                :version "1.0")))
    
    ;; Save to Memory Bank
    (condition-case err
        (let ((content (json-encode data)))
          (mcp__allpepper-memory-bank__memory_bank_write
           "emacs-claude-code" "decisions.json" content)
          (ecc-debug-message "Decisions saved to Memory Bank"))
      (error
       (ecc-debug-message "Failed to save decisions to Memory Bank: %s" 
                          (error-message-string err))))
    
    ;; Save to local file as backup
    (condition-case err
        (let ((filepath (expand-file-name "decisions.json" ecc-session-save-directory)))
          (ecc-session-ensure-directory)
          (with-temp-file filepath
            (insert (json-encode data)))
          (ecc-debug-message "Decisions saved to file: %s" filepath))
      (error
       (ecc-debug-message "Failed to save decisions to file: %s" 
                          (error-message-string err))))))

(defun ecc-decision-load-decisions ()
  "Load decision tracking data from storage."
  (let ((data nil))
    ;; Try Memory Bank first
    (setq data (condition-case err
                   (let ((content (mcp__allpepper-memory-bank__memory_bank_read
                                   "emacs-claude-code" "decisions.json")))
                     (when content
                       (json-read-from-string content)))
                 (error
                  (ecc-debug-message "Failed to load decisions from Memory Bank: %s" 
                                     (error-message-string err))
                  nil)))
    
    ;; Try local file if Memory Bank failed
    (unless data
      (setq data (condition-case err
                     (let ((filepath (expand-file-name "decisions.json" ecc-session-save-directory)))
                       (when (file-exists-p filepath)
                         (with-temp-buffer
                           (insert-file-contents filepath)
                           (json-read))))
                   (error
                    (ecc-debug-message "Failed to load decisions from file: %s" 
                                       (error-message-string err))
                    nil))))
    
    ;; Extract decisions
    (when data
      (setq ecc-decision--current-decisions (plist-get data :decisions))
      (ecc-debug-message "Loaded %d decisions" (length ecc-decision--current-decisions)))
    
    data))

;; 8. User Interface
;; ----------------------------------------

(defun ecc-decision-list-decisions ()
  "List all tracked decisions."
  (interactive)
  (let ((decisions ecc-decision--current-decisions))
    (if decisions
        (with-current-buffer (get-buffer-create "*Decision List*")
          (erase-buffer)
          (insert "Decision Tracking List\n")
          (insert "======================\n\n")
          
          ;; Group by category
          (let ((categories (make-hash-table :test 'eq)))
            (dolist (decision decisions)
              (let ((category (plist-get decision :category)))
                (push decision (gethash category categories))))
            
            (maphash (lambda (category category-decisions)
                       (insert (format "\n%s (%d decisions):\n" 
                                     (upcase (symbol-name category))
                                     (length category-decisions)))
                       (insert (make-string (+ 4 (length (symbol-name category))) ?-))
                       (insert "\n")
                       
                       (dolist (decision (sort category-decisions 
                                             (lambda (a b) 
                                               (time-less-p (plist-get b :timestamp)
                                                          (plist-get a :timestamp)))))
                         (insert (format "  [%s] %s\n"
                                       (upcase (symbol-name (plist-get decision :importance)))
                                       (plist-get decision :title)))
                         (insert (format "      %s\n" (plist-get decision :description)))
                         (insert (format "      Author: %s | %s | Status: %s\n"
                                       (plist-get decision :author)
                                       (format-time-string "%Y-%m-%d %H:%M" 
                                                         (plist-get decision :timestamp))
                                       (plist-get decision :status)))
                         (when (plist-get decision :file)
                           (insert (format "      File: %s:%s\n" 
                                         (plist-get decision :file)
                                         (plist-get decision :line))))
                         (insert "\n")))
                     categories))
          
          (goto-char (point-min))
          (display-buffer (current-buffer)))
      (message "No decisions tracked yet"))))

(defun ecc-decision-show-decision (decision-id)
  "Show detailed information for DECISION-ID."
  (interactive 
   (list (completing-read "Decision ID: " 
                         (mapcar (lambda (d) (plist-get d :id)) 
                                ecc-decision--current-decisions))))
  
  (let ((decision (ecc-decision-find-by-id decision-id)))
    (if decision
        (with-current-buffer (get-buffer-create "*Decision Details*")
          (erase-buffer)
          (insert (format "Decision: %s\n" (plist-get decision :title)))
          (insert (make-string (+ 10 (length (plist-get decision :title))) ?=))
          (insert "\n\n")
          
          (insert (format "ID: %s\n" (plist-get decision :id)))
          (insert (format "Category: %s\n" (plist-get decision :category)))
          (insert (format "Importance: %s\n" (plist-get decision :importance)))
          (insert (format "Status: %s\n" (plist-get decision :status)))
          (insert (format "Author: %s\n" (plist-get decision :author)))
          (insert (format "Created: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S" 
                                                             (plist-get decision :timestamp))))
          (when (plist-get decision :last-updated)
            (insert (format "Updated: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S" 
                                                               (plist-get decision :last-updated)))))
          (insert "\n")
          
          (insert (format "Description:\n%s\n\n" (plist-get decision :description)))
          
          (when (plist-get decision :rationale)
            (insert (format "Rationale:\n%s\n\n" (plist-get decision :rationale))))
          
          (let ((outcomes (plist-get decision :outcomes)))
            (when outcomes
              (insert "Outcomes:\n")
              (dolist (outcome outcomes)
                (insert (format "  - %s (%s)\n" 
                              (plist-get outcome :outcome)
                              (format-time-string "%Y-%m-%d" (plist-get outcome :timestamp)))))
              (insert "\n")))
          
          (when (plist-get decision :file)
            (insert (format "Location: %s:%s\n" 
                          (plist-get decision :file)
                          (plist-get decision :line))))
          
          (let ((related (ecc-decision-find-related decision-id)))
            (when related
              (insert "\nRelated Decisions:\n")
              (dolist (rel related)
                (insert (format "  - %s (%s)\n" 
                              (plist-get rel :title)
                              (plist-get rel :id))))))
          
          (goto-char (point-min))
          (display-buffer (current-buffer)))
      (message "Decision not found"))))

(defun ecc-decision-export-decisions (filename)
  "Export decisions to FILENAME."
  (interactive "FExport decisions to: ")
  (let ((data `(:decisions ,ecc-decision--current-decisions
                :exported-at ,(current-time)
                :exported-by ,(user-login-name))))
    (with-temp-file filename
      (insert (json-encode data)))
    (message "Decisions exported to %s" filename)))

;; 9. Integration with Session Manager
;; ----------------------------------------

(defun ecc-decision-on-session-save ()
  "Hook called when session is saved."
  (when ecc-decision-auto-save
    (ecc-decision-save-decisions)))

(defun ecc-decision-on-session-load ()
  "Hook called when session is loaded."
  (ecc-decision-load-decisions))

;; Add hooks
(add-hook 'ecc-session-save-hook 'ecc-decision-on-session-save)
(add-hook 'ecc-session-load-hook 'ecc-decision-on-session-load)

;; 10. Initialization
;; ----------------------------------------

(defun ecc-decision-initialize ()
  "Initialize decision tracking."
  (ecc-decision-load-decisions)
  (ecc-debug-message "Decision tracking initialized with %d decisions" 
                     (length ecc-decision--current-decisions)))

;; Auto-initialize
(add-hook 'emacs-startup-hook 'ecc-decision-initialize)

(provide 'ecc-decision-tracker)

;;; ecc-decision-tracker.el ends here