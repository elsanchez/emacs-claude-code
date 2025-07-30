;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: Claude Code (Phase 3 Milestone 3)
;;; Timestamp: <2025-07-17 11:30:00>
;;; File: ecc-search-history.el

;;; Copyright (C) 2025 Claude Code Enhancement

;; 1. Dependencies
;; ----------------------------------------
(require 'ecc-debug)
(require 'ecc-session-manager)
(require 'cl-lib)

;; 2. Configuration
;; ----------------------------------------

(defcustom ecc-search-max-results 50
  "Maximum number of search results to show."
  :type 'number
  :group 'ecc)

(defcustom ecc-search-context-lines 3
  "Number of context lines to show around search matches."
  :type 'number
  :group 'ecc)

(defcustom ecc-search-case-sensitive nil
  "Whether search should be case sensitive."
  :type 'boolean
  :group 'ecc)

(defcustom ecc-search-use-regex nil
  "Whether to use regular expressions in search by default."
  :type 'boolean
  :group 'ecc)

;; 3. Variables
;; ----------------------------------------

(defvar ecc-search--last-query ""
  "Last search query used.")

(defvar ecc-search--last-results nil
  "Last search results.")

(defvar ecc-search--results-buffer "*Claude Search Results*"
  "Buffer name for search results.")

;; 4. Core Search Functions
;; ----------------------------------------

(defun ecc-search-get-all-sessions ()
  "Get all available sessions for searching."
  (let ((sessions '()))
    ;; Get from local files
    (when (file-directory-p ecc-session-save-directory)
      (dolist (file (directory-files ecc-session-save-directory nil "\\.json$"))
        (let ((session (ecc-session-load-from-file file)))
          (when session
            (push session sessions)))))
    
    ;; Get from Memory Bank
    (when ecc-session-use-memory-bank
      (condition-case err
          (let ((files (mcp__allpepper-memory-bank__list_project_files "emacs-claude-code")))
            (dolist (file files)
              (when (string-match "session-.*\\.json$" file)
                (let ((session (ecc-session-load-from-memory-bank 
                                (file-name-sans-extension file))))
                  (when session
                    (push session sessions))))))
        (error
         (ecc-debug-message "Failed to load Memory Bank sessions: %s" (error-message-string err)))))
    
    ;; Include current session
    (when ecc-session--current-session
      (push ecc-session--current-session sessions))
    
    ;; Remove duplicates by session ID
    (cl-remove-duplicates sessions 
                          :test (lambda (a b) 
                                  (string= (plist-get a :id) (plist-get b :id))))))

(defun ecc-search-in-session (session query &optional regex-p)
  "Search for QUERY in SESSION. Use regex if REGEX-P is non-nil."
  (let ((results '())
        (session-id (plist-get session :id))
        (created (plist-get session :created))
        (project-root (plist-get session :project-root))
        (conversation-history (plist-get session :conversation-history)))
    
    (dolist (entry conversation-history)
      (let ((content (plist-get entry :content))
            (type (plist-get entry :type))
            (timestamp (plist-get entry :timestamp))
            (buffer (plist-get entry :buffer)))
        
        (when (and content 
                   (if regex-p
                       (string-match-p query content)
                     (string-match-p (if ecc-search-case-sensitive
                                         query
                                       (downcase query))
                                     (if ecc-search-case-sensitive
                                         content
                                       (downcase content)))))
          (push `(:session-id ,session-id
                  :session-created ,created
                  :project-root ,project-root
                  :entry-type ,type
                  :entry-timestamp ,timestamp
                  :entry-buffer ,buffer
                  :content ,content
                  :match-preview ,(ecc-search--get-match-preview content query regex-p))
                results))))
    
    results))

(defun ecc-search--get-match-preview (content query regex-p)
  "Get a preview of the match in CONTENT for QUERY."
  (let ((match-pos (if regex-p
                       (string-match query content)
                     (string-match (if ecc-search-case-sensitive
                                       query
                                     (downcase query))
                                   (if ecc-search-case-sensitive
                                       content
                                     (downcase content))))))
    (when match-pos
      (let* ((start (max 0 (- match-pos 50)))
             (end (min (length content) (+ match-pos (length query) 50)))
             (preview (substring content start end)))
        (if (> start 0) (setq preview (concat "..." preview)))
        (if (< end (length content)) (setq preview (concat preview "...")))
        preview))))

(defun ecc-search-conversations (query &optional regex-p)
  "Search for QUERY in conversation history. Use regex if REGEX-P is non-nil."
  (let ((all-results '())
        (sessions (ecc-search-get-all-sessions)))
    
    (dolist (session sessions)
      (let ((session-results (ecc-search-in-session session query regex-p)))
        (setq all-results (append all-results session-results))))
    
    ;; Sort by timestamp (newest first)
    (setq all-results (sort all-results 
                            (lambda (a b)
                              (time-less-p (plist-get b :entry-timestamp)
                                           (plist-get a :entry-timestamp)))))
    
    ;; Limit results
    (when (> (length all-results) ecc-search-max-results)
      (setq all-results (cl-subseq all-results 0 ecc-search-max-results)))
    
    (setq ecc-search--last-query query)
    (setq ecc-search--last-results all-results)
    
    all-results))

;; 5. Search by Type and Context
;; ----------------------------------------

(defun ecc-search-by-type (query type)
  "Search for QUERY in conversations of specific TYPE."
  (let ((results (ecc-search-conversations query)))
    (cl-remove-if-not (lambda (result)
                        (eq (plist-get result :entry-type) type))
                      results)))

(defun ecc-search-user-inputs (query)
  "Search in user inputs only."
  (interactive "sSearch user inputs: ")
  (let ((results (ecc-search-by-type query 'user-input)))
    (ecc-search--display-results results (format "User inputs matching '%s'" query))))

(defun ecc-search-claude-responses (query)
  "Search in Claude responses only."
  (interactive "sSearch Claude responses: ")
  (let ((results (ecc-search-by-type query 'claude-response)))
    (ecc-search--display-results results (format "Claude responses matching '%s'" query))))

(defun ecc-search-by-project (query project-path)
  "Search for QUERY in conversations from specific PROJECT-PATH."
  (let ((results (ecc-search-conversations query)))
    (cl-remove-if-not (lambda (result)
                        (and (plist-get result :project-root)
                             (string-prefix-p project-path (plist-get result :project-root))))
                      results)))

(defun ecc-search-current-project (query)
  "Search for QUERY in current project's conversations."
  (interactive "sSearch in current project: ")
  (let* ((project-root (or (when (fboundp 'projectile-project-root)
                             (projectile-project-root))
                           default-directory))
         (results (ecc-search-by-project query project-root)))
    (ecc-search--display-results results 
                                 (format "Results in project '%s' for '%s'" 
                                         (file-name-nondirectory (directory-file-name project-root))
                                         query))))

;; 6. Time-based Search
;; ----------------------------------------

(defun ecc-search-recent (query &optional hours)
  "Search for QUERY in recent conversations (last HOURS hours, default 24)."
  (interactive "sSearch recent conversations: ")
  (let* ((hours (or hours 24))
         (cutoff-time (time-subtract (current-time) (seconds-to-time (* hours 3600))))
         (results (ecc-search-conversations query))
         (recent-results (cl-remove-if-not 
                          (lambda (result)
                            (time-less-p cutoff-time (plist-get result :entry-timestamp)))
                          results)))
    (ecc-search--display-results recent-results 
                                 (format "Recent results (last %d hours) for '%s'" hours query))))

(defun ecc-search-today (query)
  "Search for QUERY in today's conversations."
  (interactive "sSearch today's conversations: ")
  (ecc-search-recent query 24))

;; 7. Results Display
;; ----------------------------------------

(defun ecc-search--display-results (results title)
  "Display RESULTS in a buffer with TITLE."
  (with-current-buffer (get-buffer-create ecc-search--results-buffer)
    (erase-buffer)
    (insert (format "%s\n" title))
    (insert (make-string (length title) ?=))
    (insert "\n\n")
    
    (if (null results)
        (insert "No results found.\n")
      (insert (format "Found %d result(s):\n\n" (length results)))
      
      (dolist (result results)
        (let ((session-id (plist-get result :session-id))
              (timestamp (plist-get result :entry-timestamp))
              (type (plist-get result :entry-type))
              (buffer (plist-get result :entry-buffer))
              (project (plist-get result :project-root))
              (preview (plist-get result :match-preview)))
          
          (insert (format "Session: %s\n" session-id))
          (insert (format "Time: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S" timestamp)))
          (insert (format "Type: %s\n" type))
          (when buffer
            (insert (format "Buffer: %s\n" buffer)))
          (when project
            (insert (format "Project: %s\n" (file-name-nondirectory (directory-file-name project)))))
          (insert (format "Preview: %s\n" (or preview "No preview available")))
          (insert "\n")
          (insert (make-string 50 ?-))
          (insert "\n\n"))))
    
    (goto-char (point-min))
    (display-buffer (current-buffer))))

;; 8. Interactive Search Interface
;; ----------------------------------------

(defun ecc-search-interactive ()
  "Interactive search interface."
  (interactive)
  (let ((query (read-string "Search query: " ecc-search--last-query)))
    (when (not (string-empty-p query))
      (let ((results (ecc-search-conversations query)))
        (ecc-search--display-results results (format "Search results for '%s'" query))))))

(defun ecc-search-regex ()
  "Search using regular expressions."
  (interactive)
  (let ((query (read-string "Regex search: " ecc-search--last-query)))
    (when (not (string-empty-p query))
      (let ((results (ecc-search-conversations query t)))
        (ecc-search--display-results results (format "Regex search results for '%s'" query))))))

(defun ecc-search-repeat-last ()
  "Repeat last search."
  (interactive)
  (if (string-empty-p ecc-search--last-query)
      (message "No previous search to repeat")
    (let ((results (ecc-search-conversations ecc-search--last-query)))
      (ecc-search--display-results results (format "Search results for '%s'" ecc-search--last-query)))))

;; 9. Advanced Search Features
;; ----------------------------------------

(defun ecc-search-statistics ()
  "Show search statistics."
  (interactive)
  (let ((sessions (ecc-search-get-all-sessions))
        (total-conversations 0)
        (total-user-inputs 0)
        (total-claude-responses 0)
        (oldest-session nil)
        (newest-session nil))
    
    (dolist (session sessions)
      (let ((history (plist-get session :conversation-history))
            (created (plist-get session :created)))
        (setq total-conversations (+ total-conversations (length history)))
        (dolist (entry history)
          (let ((type (plist-get entry :entry-type)))
            (cond 
             ((eq type 'user-input) (incf total-user-inputs))
             ((eq type 'claude-response) (incf total-claude-responses)))))
        
        (when (or (null oldest-session) (time-less-p created oldest-session))
          (setq oldest-session created))
        (when (or (null newest-session) (time-less-p newest-session created))
          (setq newest-session created))))
    
    (with-current-buffer (get-buffer-create "*Claude Search Statistics*")
      (erase-buffer)
      (insert "Claude Code Search Statistics\n")
      (insert "===============================\n\n")
      (insert (format "Total sessions: %d\n" (length sessions)))
      (insert (format "Total conversations: %d\n" total-conversations))
      (insert (format "User inputs: %d\n" total-user-inputs))
      (insert (format "Claude responses: %d\n" total-claude-responses))
      (when oldest-session
        (insert (format "Oldest session: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S" oldest-session))))
      (when newest-session
        (insert (format "Newest session: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S" newest-session))))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun ecc-search-export-results (filename)
  "Export last search results to FILENAME."
  (interactive "FExport results to file: ")
  (if ecc-search--last-results
      (with-temp-file filename
        (insert (format "Search results for: %s\n" ecc-search--last-query))
        (insert (format "Generated: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
        (insert (json-encode ecc-search--last-results))
        (message "Results exported to %s" filename))
    (message "No search results to export")))

(provide 'ecc-search-history)

;;; ecc-search-history.el ends here