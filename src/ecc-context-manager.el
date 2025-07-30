;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: Claude Code (Phase 3 Milestone 3)
;;; Timestamp: <2025-07-17 11:15:00>
;;; File: ecc-context-manager.el

;;; Copyright (C) 2025 Claude Code Enhancement

;; 1. Dependencies
;; ----------------------------------------
(require 'ecc-debug)
(require 'ecc-session-manager)
(require 'ecc-state-detection)

;; 2. Configuration
;; ----------------------------------------

(defcustom ecc-context-auto-restore t
  "Whether to automatically restore context on startup."
  :type 'boolean
  :group 'ecc)

(defcustom ecc-context-save-on-exit t
  "Whether to save context when exiting Emacs."
  :type 'boolean
  :group 'ecc)

(defcustom ecc-context-max-buffer-history 50
  "Maximum number of buffer states to keep in context."
  :type 'number
  :group 'ecc)

(defcustom ecc-context-restore-delay 2.0
  "Delay in seconds before restoring context after startup."
  :type 'float
  :group 'ecc)

;; 3. Variables
;; ----------------------------------------

(defvar ecc-context--last-session-id nil
  "ID of the last active session.")

(defvar ecc-context--restoration-timer nil
  "Timer for delayed context restoration.")

(defvar ecc-context--current-context nil
  "Current context information.")

;; 4. Context Capture
;; ----------------------------------------

(defun ecc-context-capture-current-state ()
  "Capture current application state for context continuity."
  (let ((context `(:timestamp ,(current-time)
                   :emacs-version ,emacs-version
                   :working-directory ,default-directory
                   :project-root ,(when (fboundp 'projectile-project-root)
                                    (projectile-project-root))
                   :active-buffer ,(when (buffer-name (current-buffer))
                                     (buffer-name (current-buffer)))
                   :vterm-buffers ,(ecc-context--get-vterm-buffers)
                   :auto-response-enabled ,(when (boundp '--ecc-auto-response--enabled)
                                             --ecc-auto-response--enabled)
                   :debug-mode-enabled ,(when (boundp '--ecc-debug-enabled)
                                          --ecc-debug-enabled)
                   :notification-enabled ,(when (boundp '--ecc-notification-enabled)
                                            --ecc-notification-enabled)
                   :last-detected-state ,(when (boundp '--ecc-state-detection--last-state)
                                           --ecc-state-detection--last-state)
                   :session-id ,(when ecc-session--current-session
                                 (plist-get ecc-session--current-session :id)))))
    (setq ecc-context--current-context context)
    context))

(defun ecc-context--get-vterm-buffers ()
  "Get information about active vterm buffers."
  (let ((vterm-buffers '()))
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (string-match-p "\\*vterm\\*" (buffer-name buffer)))
        (with-current-buffer buffer
          (push `(:name ,(buffer-name buffer)
                  :directory ,default-directory
                  :process-alive ,(when (get-buffer-process buffer)
                                    (process-live-p (get-buffer-process buffer)))
                  :last-command ,(ecc-context--get-last-command buffer))
                vterm-buffers))))
    vterm-buffers))

(defun ecc-context--get-last-command (buffer)
  "Get the last command executed in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (let ((content (buffer-substring-no-properties 
                      (max (point-min) (- (point-max) 500))
                      (point-max))))
        ;; Extract last command - simple heuristic
        (when (string-match "\\$ \\([^\n]+\\)" content)
          (match-string 1 content))))))

;; 5. Context Persistence
;; ----------------------------------------

(defun ecc-context-save-to-memory-bank (context)
  "Save CONTEXT to Memory Bank MCP."
  (condition-case err
      (let ((content (json-encode context))
            (filename "last-context.json"))
        (mcp__allpepper-memory-bank__memory_bank_write
         "emacs-claude-code" filename content)
        (ecc-debug-message "Context saved to Memory Bank")
        t)
    (error
     (ecc-debug-message "Failed to save context to Memory Bank: %s" (error-message-string err))
     nil)))

(defun ecc-context-load-from-memory-bank ()
  "Load context from Memory Bank MCP."
  (condition-case err
      (let ((content (mcp__allpepper-memory-bank__memory_bank_read
                      "emacs-claude-code" "last-context.json")))
        (when content
          (let ((context (json-read-from-string content)))
            (ecc-debug-message "Context loaded from Memory Bank")
            context)))
    (error
     (ecc-debug-message "Failed to load context from Memory Bank: %s" (error-message-string err))
     nil)))

(defun ecc-context-save-to-file (context)
  "Save CONTEXT to local file."
  (let ((filepath (expand-file-name "last-context.json" ecc-session-save-directory)))
    (condition-case err
        (progn
          (ecc-session-ensure-directory)
          (with-temp-file filepath
            (insert (json-encode context)))
          (ecc-debug-message "Context saved to file: %s" filepath)
          t)
      (error
       (ecc-debug-message "Failed to save context to file: %s" (error-message-string err))
       nil))))

(defun ecc-context-load-from-file ()
  "Load context from local file."
  (let ((filepath (expand-file-name "last-context.json" ecc-session-save-directory)))
    (condition-case err
        (when (file-exists-p filepath)
          (with-temp-buffer
            (insert-file-contents filepath)
            (let ((context (json-read)))
              (ecc-debug-message "Context loaded from file: %s" filepath)
              context)))
      (error
       (ecc-debug-message "Failed to load context from file: %s" (error-message-string err))
       nil))))

;; 6. Context Restoration
;; ----------------------------------------

(defun ecc-context-restore-state (context)
  "Restore application state from CONTEXT."
  (when context
    (ecc-debug-message "Restoring context from: %s" 
                       (format-time-string "%Y-%m-%d %H:%M:%S"
                                           (plist-get context :timestamp)))
    
    ;; Restore working directory
    (let ((working-dir (plist-get context :working-directory)))
      (when (and working-dir (file-directory-p working-dir))
        (setq default-directory working-dir)
        (ecc-debug-message "Restored working directory: %s" working-dir)))
    
    ;; Restore project context
    (let ((project-root (plist-get context :project-root)))
      (when (and project-root (file-directory-p project-root))
        (cd project-root)
        (ecc-debug-message "Restored project root: %s" project-root)))
    
    ;; Restore auto-response state
    (let ((auto-response (plist-get context :auto-response-enabled)))
      (when (and (boundp '--ecc-auto-response--enabled) auto-response)
        (setq --ecc-auto-response--enabled auto-response)
        (ecc-debug-message "Restored auto-response state: %s" auto-response)))
    
    ;; Restore debug mode
    (let ((debug-mode (plist-get context :debug-mode-enabled)))
      (when (and (boundp '--ecc-debug-enabled) debug-mode)
        (setq --ecc-debug-enabled debug-mode)
        (ecc-debug-message "Restored debug mode: %s" debug-mode)))
    
    ;; Restore notification state
    (let ((notifications (plist-get context :notification-enabled)))
      (when (and (boundp '--ecc-notification-enabled) notifications)
        (setq --ecc-notification-enabled notifications)
        (ecc-debug-message "Restored notifications: %s" notifications)))
    
    ;; Restore last session if available
    (let ((session-id (plist-get context :session-id)))
      (when session-id
        (setq ecc-context--last-session-id session-id)
        (ecc-debug-message "Found last session ID: %s" session-id)))
    
    ;; Restore vterm buffers (delayed)
    (let ((vterm-buffers (plist-get context :vterm-buffers)))
      (when vterm-buffers
        (run-with-timer 1.0 nil 'ecc-context--restore-vterm-buffers vterm-buffers)))
    
    (message "Context restored successfully")))

(defun ecc-context--restore-vterm-buffers (vterm-buffers)
  "Restore vterm buffers from VTERM-BUFFERS."
  (dolist (buffer-info vterm-buffers)
    (let ((buffer-name (plist-get buffer-info :name))
          (directory (plist-get buffer-info :directory)))
      (when (and buffer-name directory (file-directory-p directory))
        (condition-case err
            (progn
              (let ((default-directory directory))
                (unless (get-buffer buffer-name)
                  (vterm buffer-name)
                  (ecc-debug-message "Restored vterm buffer: %s in %s" 
                                     buffer-name directory))))
          (error
           (ecc-debug-message "Failed to restore vterm buffer %s: %s" 
                              buffer-name (error-message-string err))))))))

;; 7. Session Integration
;; ----------------------------------------

(defun ecc-context-restore-last-session ()
  "Restore the last active session."
  (when ecc-context--last-session-id
    (condition-case err
        (progn
          (ecc-session-load-session ecc-context--last-session-id)
          (ecc-debug-message "Restored last session: %s" ecc-context--last-session-id))
      (error
       (ecc-debug-message "Failed to restore last session: %s" (error-message-string err))))))

;; 8. User Interface
;; ----------------------------------------

(defun ecc-context-save-current ()
  "Save current context."
  (interactive)
  (let ((context (ecc-context-capture-current-state)))
    (let ((saved-file (ecc-context-save-to-file context))
          (saved-memory (ecc-context-save-to-memory-bank context)))
      (message "Context saved%s%s" 
               (if saved-file " (file)" "")
               (if saved-memory " (memory-bank)" "")))))

(defun ecc-context-restore-last ()
  "Restore last saved context."
  (interactive)
  (let ((context (or (ecc-context-load-from-memory-bank)
                     (ecc-context-load-from-file))))
    (if context
        (progn
          (ecc-context-restore-state context)
          (ecc-context-restore-last-session))
      (message "No saved context found"))))

(defun ecc-context-show-current ()
  "Show current context information."
  (interactive)
  (let ((context (ecc-context-capture-current-state)))
    (with-current-buffer (get-buffer-create "*Claude Context*")
      (erase-buffer)
      (insert "Current Claude Code Context:\n")
      (insert "============================\n\n")
      (insert (json-encode context))
      (json-pretty-print-buffer)
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;; 9. Automatic Context Management
;; ----------------------------------------

(defun ecc-context-auto-save ()
  "Automatically save context."
  (when ecc-context-save-on-exit
    (ecc-context-save-current)))

(defun ecc-context-auto-restore ()
  "Automatically restore context after startup."
  (when ecc-context-auto-restore
    (setq ecc-context--restoration-timer
          (run-with-timer ecc-context-restore-delay nil
                          'ecc-context-restore-last))))

;; 10. Hooks and Initialization
;; ----------------------------------------

(defun ecc-context-initialize ()
  "Initialize context management."
  (ecc-debug-message "Context management initialized")
  (ecc-context-auto-restore)
  
  ;; Add hooks for automatic context saving
  (add-hook 'kill-emacs-hook 'ecc-context-auto-save)
  (add-hook 'ecc-session-save-hook 'ecc-context-save-current))

;; Initialize context management
(add-hook 'emacs-startup-hook 'ecc-context-initialize)

(provide 'ecc-context-manager)

;;; ecc-context-manager.el ends here