;;; ecc-multi-agent.el --- Multiple Claude Agent Management -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience, terminals, claude

;;; Commentary:

;; This module provides functions to manage multiple Claude agents
;; using EAT terminals for different projects and workflows.

;;; Code:

(require 'eat nil t)
(require 'ecc-debug)

;; 1. Configuration
;; ----------------------------------------

(defcustom ecc-multi-agent-default-agents
  '(("claude-main" . "~/")
    ("claude-secondary" . "~/")
    ("claude-project" . "~/"))
  "Default agent configurations as (NAME . DIRECTORY) pairs."
  :type '(alist :key-type string :value-type directory)
  :group 'emacs-claude-code)

(defcustom ecc-multi-agent-window-layout 'vertical
  "Default window layout for multiple agents.
Options: 'vertical, 'horizontal, 'grid, 'tabs"
  :type '(choice (const vertical)
                 (const horizontal)
                 (const grid)
                 (const tabs))
  :group 'emacs-claude-code)

(defcustom ecc-multi-agent-auto-start-claude t
  "Whether to automatically start claude-code command in new agent terminals."
  :type 'boolean
  :group 'emacs-claude-code)

;; 2. Core Functions
;; ----------------------------------------

(defun ecc-multi-agent-create-agent (name &optional directory)
  "Create a new Claude agent with NAME in DIRECTORY.
If DIRECTORY is nil, use current directory."
  (interactive
   (list (read-string "Agent name: " "claude-agent")
         (read-directory-name "Working directory: " default-directory)))
  (let* ((buffer-name (format "*%s*" name))
         (dir (or directory default-directory)))
    (when (get-buffer buffer-name)
      (user-error "Agent buffer '%s' already exists" buffer-name))
    
    ;; Create EAT terminal
    (eat buffer-name)
    (with-current-buffer buffer-name
      ;; Change to specified directory
      (when (and dir (file-directory-p dir))
        (eat-term-send-string eat-terminal (format "cd %s" (shell-quote-argument dir)))
        (eat-term-send-string eat-terminal (kbd "RET")))
      
      ;; Auto-start claude if enabled
      (when ecc-multi-agent-auto-start-claude
        (sit-for 0.5) ; Wait a moment for directory change
        (eat-term-send-string eat-terminal "claude")
        (eat-term-send-string eat-terminal (kbd "RET"))))
    
    (ecc-debug-message "Created Claude agent: %s in %s" name dir)
    buffer-name))

(defun ecc-multi-agent-setup-general ()
  "Set up multiple Claude agents for general development work."
  (interactive)
  (let ((window-config (current-window-configuration)))
    (condition-case err
        (progn
          ;; Clear current window layout
          (delete-other-windows)
          
          ;; Create main agent
          (ecc-multi-agent-create-agent "claude-main" default-directory)
          
          ;; Split and create secondary agent
          (split-window-below)
          (other-window 1)
          (ecc-multi-agent-create-agent "claude-secondary" default-directory)
          
          ;; Split again for third agent
          (split-window-right)
          (other-window 1)
          (ecc-multi-agent-create-agent "claude-project" default-directory)
          
          ;; Return to main agent
          (other-window 1)
          (switch-to-buffer "*claude-main*")
          
          (message "Multi-agent setup complete! Agents: claude-main, claude-secondary, claude-project"))
      (error
       (set-window-configuration window-config)
       (error "Failed to setup multi-agent environment: %s" (error-message-string err))))))

(defun ecc-multi-agent-list-agents ()
  "List all active Claude agent buffers."
  (interactive)
  (let ((agents (cl-remove-if-not 
                 (lambda (buf)
                   (and (buffer-live-p buf)
                        (string-match-p "^\\*claude-" (buffer-name buf))))
                 (buffer-list))))
    (if agents
        (message "Active Claude agents: %s" 
                 (mapconcat (lambda (buf) (buffer-name buf)) agents ", "))
      (message "No active Claude agents found"))))

(defun ecc-multi-agent-switch-to-agent ()
  "Switch to a specific Claude agent buffer."
  (interactive)
  (let* ((agents (cl-remove-if-not 
                  (lambda (buf)
                    (and (buffer-live-p buf)
                         (string-match-p "^\\*claude-" (buffer-name buf))))
                  (buffer-list)))
         (agent-names (mapcar #'buffer-name agents)))
    (if agent-names
        (let ((selected (completing-read "Switch to agent: " agent-names)))
          (switch-to-buffer selected))
      (message "No active Claude agents found"))))

(defun ecc-multi-agent-kill-agent (name)
  "Kill a specific Claude agent by NAME."
  (interactive
   (let* ((agents (cl-remove-if-not 
                   (lambda (buf)
                     (and (buffer-live-p buf)
                          (string-match-p "^\\*claude-" (buffer-name buf))))
                   (buffer-list)))
          (agent-names (mapcar #'buffer-name agents)))
     (list (completing-read "Kill agent: " agent-names))))
  (let ((buffer (get-buffer name)))
    (if buffer
        (progn
          (kill-buffer buffer)
          (message "Killed Claude agent: %s" name))
      (message "Agent not found: %s" name))))

(defun ecc-multi-agent-kill-all-agents ()
  "Kill all Claude agent buffers."
  (interactive)
  (let ((agents (cl-remove-if-not 
                 (lambda (buf)
                   (and (buffer-live-p buf)
                        (string-match-p "^\\*claude-" (buffer-name buf))))
                 (buffer-list))))
    (if agents
        (when (y-or-n-p (format "Kill %d Claude agents? " (length agents)))
          (dolist (agent agents)
            (kill-buffer agent))
          (message "Killed %d Claude agents" (length agents)))
      (message "No active Claude agents to kill"))))

;; 3. Project-Specific Functions
;; ----------------------------------------

(defun ecc-multi-agent-setup-ecc-tracker ()
  "Set up agents specifically for emacs-claude-code + tracker workflow."
  (interactive)
  (let ((window-config (current-window-configuration))
        (ecc-dir "~/.config/doom/lisp/emacs-claude-code")
        (tracker-dir "~/repo/tracker"))
    (condition-case err
        (progn
          ;; Verify directories exist
          (unless (file-directory-p (expand-file-name ecc-dir))
            (error "ECC directory not found: %s" ecc-dir))
          (unless (file-directory-p (expand-file-name tracker-dir))
            (error "Tracker directory not found: %s" tracker-dir))
          
          ;; Clear current window layout
          (delete-other-windows)
          
          ;; Create ECC agent (left side)
          (ecc-multi-agent-create-agent "claude-ecc" ecc-dir)
          
          ;; Split vertically and create tracker agent (right side)
          (split-window-right)
          (other-window 1)
          (ecc-multi-agent-create-agent "claude-tracker" tracker-dir)
          
          ;; Return to ECC agent
          (other-window 1)
          
          (message "ECC + Tracker setup complete! Use C-x b to switch between agents"))
      (error
       (set-window-configuration window-config)
       (error "Failed to setup ECC + Tracker environment: %s" (error-message-string err))))))

(defun ecc-multi-agent-setup-custom-projects ()
  "Set up agents for custom project directories."
  (interactive)
  (let ((projects '())
        (continue t))
    ;; Collect project directories
    (while continue
      (let ((dir (read-directory-name 
                  (format "Project directory %d (empty to finish): " 
                          (1+ (length projects)))
                  "~/")))
        (if (string= dir "~/")
            (setq continue nil)
          (let ((name (read-string 
                       (format "Agent name for %s: " 
                               (file-name-nondirectory (directory-file-name dir)))
                       (format "claude-%s" 
                               (file-name-nondirectory (directory-file-name dir))))))
            (push (cons name dir) projects)))))
    
    (if (null projects)
        (message "No projects specified")
      ;; Setup window layout based on number of projects
      (delete-other-windows)
      (let ((project-count (length projects)))
        (dolist (project (reverse projects))
          (let ((name (car project))
                (dir (cdr project)))
            (ecc-multi-agent-create-agent name dir)
            (when (> project-count 1)
              (if (= (length projects) project-count)
                  (split-window-right)
                (split-window-below))
              (other-window 1)
              (setq project-count (1- project-count)))))
        
        (message "Custom project setup complete! Agents: %s" 
                 (mapconcat #'car (reverse projects) ", "))))))

;; 4. Utility Functions
;; ----------------------------------------

(defun ecc-multi-agent-send-to-all (command)
  "Send COMMAND to all active Claude agents."
  (interactive "sCommand to send to all agents: ")
  (let ((agents (cl-remove-if-not 
                 (lambda (buf)
                   (and (buffer-live-p buf)
                        (string-match-p "^\\*claude-" (buffer-name buf))
                        (with-current-buffer buf
                          (and (bound-and-true-p eat-terminal)
                               eat-terminal))))
                 (buffer-list))))
    (if agents
        (progn
          (dolist (agent agents)
            (with-current-buffer agent
              (eat-term-send-string eat-terminal command)
              (eat-term-send-string eat-terminal (kbd "RET"))))
          (message "Sent command to %d agents: %s" (length agents) command))
      (message "No active Claude agents found"))))

;; 5. Interactive Commands
;; ----------------------------------------

;;;###autoload
(defun ecc-multi-agent-menu ()
  "Show interactive menu for multi-agent management."
  (interactive)
  (let ((choice (completing-read
                 "Multi-Agent Action: "
                 '("Setup General (3 agents)"
                   "Setup ECC + Tracker" 
                   "Setup Custom Projects"
                   "Create Single Agent"
                   "List Active Agents"
                   "Switch to Agent"
                   "Send Command to All"
                   "Kill Agent"
                   "Kill All Agents")
                 nil t)))
    (cond
     ((string= choice "Setup General (3 agents)")
      (ecc-multi-agent-setup-general))
     ((string= choice "Setup ECC + Tracker")
      (ecc-multi-agent-setup-ecc-tracker))
     ((string= choice "Setup Custom Projects")
      (ecc-multi-agent-setup-custom-projects))
     ((string= choice "Create Single Agent")
      (call-interactively #'ecc-multi-agent-create-agent))
     ((string= choice "List Active Agents")
      (ecc-multi-agent-list-agents))
     ((string= choice "Switch to Agent")
      (ecc-multi-agent-switch-to-agent))
     ((string= choice "Send Command to All")
      (call-interactively #'ecc-multi-agent-send-to-all))
     ((string= choice "Kill Agent")
      (call-interactively #'ecc-multi-agent-kill-agent))
     ((string= choice "Kill All Agents")
      (ecc-multi-agent-kill-all-agents)))))

(provide 'ecc-multi-agent)

;;; ecc-multi-agent.el ends here