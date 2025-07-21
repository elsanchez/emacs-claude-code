;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: Claude Code (Phase 3 Milestone 3 - Simplified)
;;; File: ecc-session-simple.el

;;; Simple Session Management for immediate testing

(defvar ecc-session-simple-directory (expand-file-name "~/.emacs-claude-code/sessions/")
  "Directory for simple session storage.")

(defvar ecc-session-simple-current nil
  "Current simple session data.")

(defun ecc-session-simple-ensure-directory ()
  "Ensure session directory exists."
  (unless (file-directory-p ecc-session-simple-directory)
    (make-directory ecc-session-simple-directory t)))

(defun ecc-session-simple-save-current ()
  "Save current session (simplified version)."
  (interactive)
  (ecc-session-simple-ensure-directory)
  (let* ((session-id (format "session-%s" (format-time-string "%Y%m%d-%H%M%S")))
         (session-data `(:id ,session-id
                        :created ,(current-time)
                        :buffer ,(buffer-name)
                        :directory ,default-directory))
         (filename (expand-file-name (format "%s.json" session-id) ecc-session-simple-directory)))
    
    (setq ecc-session-simple-current session-data)
    
    (with-temp-file filename
      (insert (json-encode session-data)))
    
    (message "✅ Simple session saved: %s" session-id)
    session-id))

(defun ecc-session-simple-list ()
  "List available sessions."
  (interactive)
  (ecc-session-simple-ensure-directory)
  (let ((sessions (directory-files ecc-session-simple-directory nil "\\.json$")))
    (if sessions
        (progn
          (message "📋 Available sessions:")
          (dolist (session sessions)
            (message "  - %s" (file-name-sans-extension session))))
      (message "❌ No sessions found"))))

(defun ecc-session-simple-load (session-id)
  "Load a session by SESSION-ID."
  (interactive 
   (list (completing-read "Session ID: " 
                         (mapcar 'file-name-sans-extension
                                (directory-files ecc-session-simple-directory nil "\\.json$")))))
  
  (let ((filename (expand-file-name (format "%s.json" session-id) ecc-session-simple-directory)))
    (if (file-exists-p filename)
        (let ((session-data (with-temp-buffer
                             (insert-file-contents filename)
                             (json-read))))
          (setq ecc-session-simple-current session-data)
          (message "✅ Session loaded: %s" session-id)
          session-data)
      (message "❌ Session not found: %s" session-id))))

(defun ecc-session-simple-show-current ()
  "Show current session info."
  (interactive)
  (if ecc-session-simple-current
      (message "📋 Current session: %s (created: %s)" 
               (plist-get ecc-session-simple-current :id)
               (format-time-string "%Y-%m-%d %H:%M:%S" 
                                 (plist-get ecc-session-simple-current :created)))
    (message "❌ No current session")))

;; Quick decision tracking
(defvar ecc-decisions-simple nil
  "Simple decisions list.")

(defun ecc-decision-simple-add (title description)
  "Add a simple decision."
  (interactive 
   (list (read-string "Decision title: ")
         (read-string "Description: ")))
  
  (let ((decision `(:title ,title
                   :description ,description
                   :timestamp ,(current-time)
                   :buffer ,(buffer-name)
                   :file ,(buffer-file-name))))
    (push decision ecc-decisions-simple)
    (message "✅ Decision added: %s" title)
    decision))

(defun ecc-decision-simple-list ()
  "List simple decisions."
  (interactive)
  (if ecc-decisions-simple
      (progn
        (message "📋 Decisions:")
        (dolist (decision ecc-decisions-simple)
          (message "  - %s: %s" 
                   (plist-get decision :title)
                   (plist-get decision :description))))
    (message "❌ No decisions recorded")))

;; Quick keybindings for testing
(defun ecc-simple-setup-keys ()
  "Setup simple keybindings for testing."
  (interactive)
  (global-set-key (kbd "C-c s s") 'ecc-session-simple-save-current)
  (global-set-key (kbd "C-c s l") 'ecc-session-simple-list)
  (global-set-key (kbd "C-c s o") 'ecc-session-simple-load)
  (global-set-key (kbd "C-c s c") 'ecc-session-simple-show-current)
  (global-set-key (kbd "C-c d a") 'ecc-decision-simple-add)
  (global-set-key (kbd "C-c d l") 'ecc-decision-simple-list)
  (message "✅ Simple session management keys setup:
  C-c s s - Save session
  C-c s l - List sessions  
  C-c s o - Load session
  C-c s c - Show current
  C-c d a - Add decision
  C-c d l - List decisions"))

(provide 'ecc-session-simple)

;;; ecc-session-simple.el ends here