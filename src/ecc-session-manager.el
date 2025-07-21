;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: Claude Code (Phase 3 Milestone 3)
;;; Timestamp: <2025-07-17 11:00:00>
;;; File: ecc-session-manager.el

;;; Copyright (C) 2025 Claude Code Enhancement

;; 1. Dependencies
;; ----------------------------------------
(require 'ecc-debug)
(require 'ecc-vterm-utils)
(require 'json)

;; 2. Configuration
;; ----------------------------------------

(defcustom ecc-session-save-directory "~/.emacs-claude-code/sessions/"
  "Directory to save Claude Code sessions."
  :type 'string
  :group 'ecc)

(defcustom ecc-session-auto-save-interval 300
  "Interval in seconds for automatic session saving (0 to disable)."
  :type 'number
  :group 'ecc)

(defcustom ecc-session-max-history 100
  "Maximum number of sessions to keep in history."
  :type 'number
  :group 'ecc)

(defcustom ecc-session-use-memory-bank t
  "Whether to use Memory Bank MCP for session persistence."
  :type 'boolean
  :group 'ecc)

;; 3. Variables
;; ----------------------------------------

(defvar ecc-session--current-session nil
  "Current session data.")

(defvar ecc-session--auto-save-timer nil
  "Timer for automatic session saving.")

(defvar ecc-session--session-counter 0
  "Counter for generating unique session IDs.")

;; 4. Core Session Management
;; ----------------------------------------

(defun ecc-session-generate-id ()
  "Generate a unique session ID."
  (setq ecc-session--session-counter (1+ ecc-session--session-counter))
  (format "session-%s-%d" 
          (format-time-string "%Y%m%d-%H%M%S")
          ecc-session--session-counter))

(defun ecc-session-create-session ()
  "Create a new session."
  (let ((session-id (ecc-session-generate-id)))
    (setq ecc-session--current-session
          `(:id ,session-id
            :created ,(current-time)
            :last-updated ,(current-time)
            :project-root ,(or (projectile-project-root) default-directory)
            :buffers ()
            :context ""
            :conversation-history ()
            :states-detected ()
            :user-actions ()))
    (ecc-debug-message "Created new session: %s" session-id)
    session-id))

(defun ecc-session-get-current-session ()
  "Get current session, creating one if needed."
  (unless ecc-session--current-session
    (ecc-session-create-session))
  ecc-session--current-session)

(defun ecc-session-capture-buffer-content (buffer)
  "Capture relevant content from BUFFER for session."
  (with-current-buffer buffer
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      `(:buffer-name ,(buffer-name buffer)
        :buffer-file ,(buffer-file-name buffer)
        :content-length ,(length content)
        :content-preview ,(substring content 0 (min 500 (length content)))
        :point ,(point)
        :mark ,(when (mark) (mark))
        :modified ,(buffer-modified-p)
        :timestamp ,(current-time)))))

(defun ecc-session-update-buffer-info (buffer)
  "Update session with current buffer information."
  (let ((session (ecc-session-get-current-session))
        (buffer-info (ecc-session-capture-buffer-content buffer)))
    (setq ecc-session--current-session
          (plist-put session :buffers
                     (cons buffer-info 
                           (cl-remove-if (lambda (b) 
                                           (string= (plist-get b :buffer-name)
                                                    (plist-get buffer-info :buffer-name)))
                                         (plist-get session :buffers)))))
    (plist-put ecc-session--current-session :last-updated (current-time))))

(defun ecc-session-add-conversation-entry (type content)
  "Add a conversation entry to current session."
  (let ((session (ecc-session-get-current-session))
        (entry `(:type ,type
                 :content ,content
                 :timestamp ,(current-time)
                 :buffer ,(buffer-name (current-buffer)))))
    (setq ecc-session--current-session
          (plist-put session :conversation-history
                     (cons entry (plist-get session :conversation-history))))
    (plist-put ecc-session--current-session :last-updated (current-time))))

;; 5. Session Persistence
;; ----------------------------------------

(defun ecc-session-ensure-directory ()
  "Ensure session directory exists."
  (unless (file-directory-p ecc-session-save-directory)
    (make-directory ecc-session-save-directory t)))

(defun ecc-session-save-to-file (session filename)
  "Save SESSION to FILENAME."
  (ecc-session-ensure-directory)
  (let ((filepath (expand-file-name filename ecc-session-save-directory)))
    (condition-case err
        (with-temp-file filepath
          (insert (json-encode session))
          (ecc-debug-message "Session saved to file: %s" filepath)
          t)
      (error
       (ecc-debug-message "Failed to save session to file: %s" (error-message-string err))
       nil))))

(defun ecc-session-load-from-file (filename)
  "Load session from FILENAME."
  (let ((filepath (expand-file-name filename ecc-session-save-directory)))
    (condition-case err
        (when (file-exists-p filepath)
          (with-temp-buffer
            (insert-file-contents filepath)
            (let ((session (json-read)))
              (ecc-debug-message "Session loaded from file: %s" filepath)
              session)))
      (error
       (ecc-debug-message "Failed to load session from file: %s" (error-message-string err))
       nil))))

(defun ecc-session-save-to-memory-bank (session)
  "Save SESSION to Memory Bank MCP."
  (when (and ecc-session-use-memory-bank
             (fboundp 'mcp__allpepper-memory-bank__memory_bank_write))
    (condition-case err
        (let* ((session-id (plist-get session :id))
               (filename (format "%s.json" session-id))
               (content (json-encode session)))
          (mcp__allpepper-memory-bank__memory_bank_write
           "emacs-claude-code" filename content)
          (ecc-debug-message "Session saved to Memory Bank: %s" session-id)
          t)
      (error
       (ecc-debug-message "Failed to save session to Memory Bank: %s" (error-message-string err))
       nil))))

(defun ecc-session-load-from-memory-bank (session-id)
  "Load session from Memory Bank MCP."
  (when (and ecc-session-use-memory-bank
             (fboundp 'mcp__allpepper-memory-bank__memory_bank_read))
    (condition-case err
        (let ((filename (format "%s.json" session-id)))
          (let ((content (mcp__allpepper-memory-bank__memory_bank_read
                          "emacs-claude-code" filename)))
            (when content
              (let ((session (json-read-from-string content)))
                (ecc-debug-message "Session loaded from Memory Bank: %s" session-id)
                session))))
      (error
       (ecc-debug-message "Failed to load session from Memory Bank: %s" (error-message-string err))
       nil))))

;; 6. User Interface Functions
;; ----------------------------------------

(defun ecc-session-save-current ()
  "Save current session."
  (interactive)
  (let ((session (ecc-session-get-current-session)))
    (when session
      (let ((session-id (plist-get session :id)))
        (let ((saved-file (ecc-session-save-to-file session (format "%s.json" session-id)))
              (saved-memory (ecc-session-save-to-memory-bank session)))
          (message "Session '%s' saved%s%s" 
                   session-id
                   (if saved-file " (file)" "")
                   (if saved-memory " (memory-bank)" "")))))))

(defun ecc-session-list-sessions ()
  "List all available sessions."
  (interactive)
  (let ((sessions '()))
    ;; Get sessions from files
    (when (file-directory-p ecc-session-save-directory)
      (dolist (file (directory-files ecc-session-save-directory nil "\\.json$"))
        (let ((session-id (file-name-sans-extension file)))
          (push `(:id ,session-id :source "file" :filename ,file) sessions))))
    
    ;; Get sessions from Memory Bank
    (when ecc-session-use-memory-bank
      (condition-case err
          (let ((files (mcp__allpepper-memory-bank__list_project_files "emacs-claude-code")))
            (dolist (file files)
              (when (string-match "session-.*\\.json$" file)
                (let ((session-id (file-name-sans-extension file)))
                  (unless (assoc session-id sessions)
                    (push `(:id ,session-id :source "memory-bank" :filename ,file) sessions))))))
        (error
         (ecc-debug-message "Failed to list Memory Bank sessions: %s" (error-message-string err)))))
    
    (if sessions
        (with-current-buffer (get-buffer-create "*Claude Sessions*")
          (erase-buffer)
          (insert "Available Claude Code Sessions:\n")
          (insert "================================\n\n")
          (dolist (session sessions)
            (insert (format "ID: %s\n" (plist-get session :id)))
            (insert (format "Source: %s\n" (plist-get session :source)))
            (insert (format "File: %s\n\n" (plist-get session :filename))))
          (goto-char (point-min))
          (display-buffer (current-buffer)))
      (message "No sessions found"))))

(defun ecc-session-load-session (session-id)
  "Load session by SESSION-ID."
  (interactive "sEnter session ID: ")
  (let ((session (or (ecc-session-load-from-memory-bank session-id)
                     (ecc-session-load-from-file (format "%s.json" session-id)))))
    (if session
        (progn
          (setq ecc-session--current-session session)
          (message "Session '%s' loaded successfully" session-id)
          ;; TODO: Restore buffer states, context, etc.
          )
      (message "Session '%s' not found" session-id))))

;; 7. Auto-save functionality
;; ----------------------------------------

(defun ecc-session-auto-save ()
  "Automatically save current session."
  (when (and ecc-session--current-session
             (> ecc-session-auto-save-interval 0))
    (ecc-session-save-current)))

(defun ecc-session-start-auto-save ()
  "Start automatic session saving."
  (when (and (> ecc-session-auto-save-interval 0)
             (not ecc-session--auto-save-timer))
    (setq ecc-session--auto-save-timer
          (run-with-timer ecc-session-auto-save-interval
                          ecc-session-auto-save-interval
                          'ecc-session-auto-save))
    (ecc-debug-message "Auto-save started (interval: %d seconds)" ecc-session-auto-save-interval)))

(defun ecc-session-stop-auto-save ()
  "Stop automatic session saving."
  (when ecc-session--auto-save-timer
    (cancel-timer ecc-session--auto-save-timer)
    (setq ecc-session--auto-save-timer nil)
    (ecc-debug-message "Auto-save stopped")))

;; 8. Integration hooks
;; ----------------------------------------

(defun ecc-session-on-buffer-change ()
  "Hook function called when buffer changes."
  (when (and (buffer-name (current-buffer))
             (string-match-p "\\*vterm\\*" (buffer-name (current-buffer))))
    (ecc-session-update-buffer-info (current-buffer))))

(defun ecc-session-on-claude-response (response)
  "Hook function called when Claude responds."
  (ecc-session-add-conversation-entry 'claude-response response))

(defun ecc-session-on-user-input (input)
  "Hook function called when user inputs."
  (ecc-session-add-conversation-entry 'user-input input))

;; 9. Initialization
;; ----------------------------------------

(defun ecc-session-initialize ()
  "Initialize session management."
  (ecc-session-ensure-directory)
  (ecc-session-create-session)
  (ecc-session-start-auto-save)
  (ecc-debug-message "Session management initialized"))

;; Enable session management on load
(add-hook 'ecc-mode-hook 'ecc-session-initialize)

(provide 'ecc-session-manager)

;;; ecc-session-manager.el ends here