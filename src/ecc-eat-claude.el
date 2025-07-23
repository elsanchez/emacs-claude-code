;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-07-23 00:00:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-eat-claude.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(require 'eat nil t)
(require 'ecc-eat-utils)

;; 1. Configuration
;; ----------------------------------------

(defcustom ecc-claude-program "claude"
  "The name or path of the claude-code program."
  :type 'string
  :group 'ecc)

(defcustom ecc-claude-program-switches nil
  "List of command line switches to pass to the Claude program.
These are passed as SWITCHES parameters to `eat-make`.
E.g, `\'(\"--verbose\" \"--dangerously-skip-permissions\")'."
  :type '(repeat string)
  :group 'ecc)

(defcustom ecc-claude-switch-to-buffer-on-create t
  "Whether to switch to the Claude buffer when creating a new session.
If non-nil, automatically switch to the Claude buffer after starting.
If nil, create the session but don't switch focus to it."
  :type 'boolean
  :group 'ecc)

;; 2. Variables
;; ----------------------------------------

(defvar ecc-claude--cwd nil
  "Working directory for the current Claude session.")

;; 3. Buffer Management
;; ----------------------------------------

(defun ecc-claude--get-buffer-name ()
  "Get the buffer name for Claude session."
  (format "*ecc-claude-%s*" (or (ecc-claude--project-root) "default")))

(defun ecc-claude--get-buffer ()
  "Get the Claude buffer if it exists."
  (get-buffer (ecc-claude--get-buffer-name)))

(defun ecc-claude--project-root ()
  "Get the project root directory."
  (or (when (fboundp 'project-root)
        (when-let ((project (project-current)))
          (project-root project)))
      (when (fboundp 'vc-git-root)
        (vc-git-root default-directory))
      default-directory))

(defun ecc-claude--switch-to-buffer ()
  "Switch to existing Claude buffer if it exists."
  (when-let ((buffer (ecc-claude--get-buffer)))
    (let ((window (display-buffer buffer)))
      (when ecc-claude-switch-to-buffer-on-create
        (select-window window))
      t)))

;; 4. Core Functions
;; ----------------------------------------

(defun ecc-claude--start (work-dir &rest args)
  "Start Claude Code in WORK-DIR with ARGS."
  (require 'eat)
  (let* ((default-directory work-dir)
         (buffer-name (ecc-claude--get-buffer-name))
         (buffer (get-buffer-create buffer-name))
         (process-environment
          (append '("TERM=xterm-256color")
                  process-environment)))
    (with-current-buffer buffer
      (cd work-dir)
      (setq-local eat-term-name "xterm-256color")
      (let ((process-adaptive-read-buffering nil)
            (switches (remove nil (append args ecc-claude-program-switches))))
        (apply #'eat-make (substring buffer-name 1 -1) ecc-claude-program nil switches))
      
      ;; Set buffer-local variables after eat-make to ensure they persist
      (setq-local ecc-claude--cwd work-dir)
      
      ;; Apply eat optimizations
      (--ecc-eat-optimize-scrolling)
      
      ;; Set up custom key mappings after eat initialization
      (run-with-timer 0.1 nil
                      (lambda ()
                        (ecc-claude--setup-eat-integration buffer))))
    
    (let ((window (display-buffer buffer)))
      (when ecc-claude-switch-to-buffer-on-create
        (select-window window)))))

(defun ecc-claude--setup-eat-integration (buffer)
  "Set up eat integration for Claude buffer."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      ;; Setup custom keybindings
      (when (derived-mode-p 'eat-mode)
        (local-set-key (kbd "C-g") #'ecc-claude--send-escape)))))

(defun ecc-claude--send-escape ()
  "Send escape to Claude to cancel current input."
  (interactive)
  (when (and (derived-mode-p 'eat-mode)
             (bound-and-true-p eat-terminal))
    (eat-term-send-string eat-terminal (kbd "ESC"))))

(defun ecc-claude--run-with-args (&optional arg &rest args)
  "Start Claude Code with ARGS or switch to existing session.
With prefix ARG, prompt for the project directory."
  (let* ((explicit-dir (when arg (read-directory-name "Project directory: ")))
         (work-dir (or explicit-dir (ecc-claude--project-root))))
    (unless (ecc-claude--switch-to-buffer)
      (apply #'ecc-claude--start work-dir args))))

;; 5. Interactive Commands
;; ----------------------------------------

;;;###autoload
(defun ecc-claude-run (&optional arg)
  "Start Claude Code in eat or switch to existing session.
With prefix ARG, prompt for the project directory."
  (interactive "P")
  (ecc-claude--run-with-args arg))

;;;###autoload
(defun ecc-claude-resume (&optional arg)
  "Start Claude Code with resume in eat or switch to existing session.
With prefix ARG, prompt for the project directory."
  (interactive "P")
  (let ((ecc-claude-switch-to-buffer-on-create t))
    (ecc-claude--run-with-args arg "--resume")))

;;;###autoload
(defun ecc-claude-kill ()
  "Kill Claude process and close its window."
  (interactive)
  (if-let* ((claude-buffer (ecc-claude--get-buffer)))
      (progn
        (with-current-buffer claude-buffer
          (when (and (bound-and-true-p eat-terminal)
                     eat-terminal)
            (let ((process (eat-term-parameter eat-terminal 'eat--process)))
              (when (and process (process-live-p process))
                (kill-process process))))
          (kill-buffer claude-buffer))
        (message "Claude session killed"))
    (error "There is no Claude session in this workspace or project")))

;;;###autoload
(defun ecc-claude-send-message (message)
  "Send MESSAGE to the active Claude session."
  (interactive "sMessage: ")
  (if-let* ((claude-buffer (ecc-claude--get-buffer)))
      (with-current-buffer claude-buffer
        (when (and (derived-mode-p 'eat-mode)
                   (bound-and-true-p eat-terminal))
          (eat-term-send-string eat-terminal message)
          (eat-term-send-string eat-terminal (kbd "RET"))
          (display-buffer claude-buffer)))
    (error "No Claude session is active")))

;;;###autoload
(defun ecc-claude-add-file (file)
  "Add FILE to Claude context using @ syntax."
  (interactive "fFile: ")
  (let ((relative-file (file-relative-name file (ecc-claude--project-root))))
    (ecc-claude-send-message (format "@%s" relative-file))))

;;;###autoload
(defun ecc-claude-add-current-file ()
  "Add current file to Claude context."
  (interactive)
  (if (buffer-file-name)
      (ecc-claude-add-file (buffer-file-name))
    (error "Buffer is not visiting a file")))

;; 6. Keybindings
;; ----------------------------------------

(defvar ecc-claude-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c c r") #'ecc-claude-run)
    (define-key map (kbd "C-c c R") #'ecc-claude-resume)
    (define-key map (kbd "C-c c k") #'ecc-claude-kill)
    (define-key map (kbd "C-c c s") #'ecc-claude-send-message)
    (define-key map (kbd "C-c c f") #'ecc-claude-add-file)
    (define-key map (kbd "C-c c F") #'ecc-claude-add-current-file)
    map)
  "Keymap for ecc-claude commands.")

;;;###autoload
(define-minor-mode ecc-claude-mode
  "Minor mode for Claude Code integration with eat."
  :lighter " Claude"
  :keymap ecc-claude-mode-map
  :group 'ecc)

;;;###autoload
(define-globalized-minor-mode global-ecc-claude-mode ecc-claude-mode
  (lambda () (ecc-claude-mode 1)))

(provide 'ecc-eat-claude)

(when (not load-file-name)
  (message "ecc-eat-claude.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))