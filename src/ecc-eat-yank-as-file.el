;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-07-23 00:00:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-eat-yank-as-file.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'eat nil t)

;; 1. Main entry point
;; ----------------------------------------

(defcustom ecc-directory-for-yank-as-file "~/.emacs-claude-code/"
  "Default directory for yank-as-file operations, both local and remote.
This directory will be used for storing temporary files from kill ring content."
  :type 'string
  :group 'ecc)

(defun emacs-claude-code-eat-yank-as-file (&optional remote-info)
  "Create a temporary file with the latest kill-ring content and send path to eat.

When REMOTE-INFO is provided, uses that specific server.
When REMOTE-INFO is nil, prompts user to select host.
Special case: when host is 'localhost' or '127.0.0.1', creates local file.

REMOTE-INFO should be an alist with keys: user, host, port
Example: '((user . \"username\") (host . \"hostname\") (port . \"22\"))

Example usage:
  ;; Prompt for host selection
  (ecc-eat-yank-as-file)
  ;; Use specific remote host
  (ecc-eat-yank-as-file '((user . \"user\") (host . \"example.com\") (port . \"22\")))
  ;; Use localhost (creates local file)
  (ecc-eat-yank-as-file '((user . \"user\") (host . \"localhost\") (port . \"22\")))
  ;; Sends \"Read /path/to/file.tmp\" to eat"
  (interactive)
  (if (not (derived-mode-p 'eat-mode))
      (message "This command only works in eat-mode")
    (let ((content (--ecc-get-kill-ring-content)))
      (if (not content)
          (message "Kill ring is empty")
        (if remote-info
            ;; remote-info provided - check if localhost or remote
            (if (let ((host (cdr (assoc 'host remote-info))))
                  (or (equal host "localhost")
                      (equal host "127.0.0.1")
                      (equal host "")))
                ;; Local file creation (localhost case)
                (let ((temp-file (--ecc-create-temp-file t)))
                  (--ecc-write-content-to-file content temp-file)
                  (--ecc-send-read-command-eat temp-file))
              ;; Remote file creation
              (if (require 'ecc-remote nil t)
                  (let ((remote-file (--ecc-yank-to-remote-with-ssh-info content remote-info)))
                    (when remote-file
                      (--ecc-send-read-command-eat remote-file)))
                (message "ecc-remote not available")))
          ;; No remote-info provided - prompt user
          (if (require 'ecc-remote nil t)
              (let ((ssh-info (--ecc-get-ssh-info-from-selection)))
                (if ssh-info
                    ;; Remote host selected
                    (let ((remote-file (--ecc-yank-to-remote-with-ssh-info content ssh-info)))
                      (when remote-file
                        (--ecc-send-read-command-eat remote-file)))
                  ;; nil returned (localhost/cancelled) - create local file
                  (let ((temp-file (--ecc-create-temp-file t)))
                    (--ecc-write-content-to-file content
                                                       temp-file)
                    (--ecc-send-read-command-eat temp-file))))
            (message "ecc-remote not available")))))))

;; Alias for backward compatibility
(defalias 'ecc-eat-yank-as-file 'emacs-claude-code-eat-yank-as-file)

;; 2. Core functions
;; ----------------------------------------

(defun --ecc-create-temp-file (&optional use-default-dir)
  "Create a unique temporary file in the current or default directory.

If USE-DEFAULT-DIR is non-nil, creates file in default directory (unified if ecc-remote loaded).
Returns the absolute path of the created file."
  (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (filename (format "kill-ring-%s.tmp" timestamp))
         (directory (if use-default-dir
                        (if (fboundp '--ecc-get-yank-directory)
                            (--ecc-get-yank-directory)
                          (expand-file-name
                           ecc-directory-for-yank-as-file))
                      default-directory))
         (filepath (expand-file-name filename directory)))
    ;; Ensure directory exists when using default directory
    (when use-default-dir
      (unless (file-exists-p directory)
        (make-directory directory t)))
    filepath))

(defun --ecc-get-kill-ring-content ()
  "Get the latest content from the kill-ring.

Returns the content as a string, or nil if kill-ring is empty."
  (when kill-ring
    (car kill-ring)))

(defun --ecc-write-content-to-file (content filepath)
  "Write CONTENT to FILEPATH."
  (with-temp-file filepath
    (insert content))
  (message "Created temporary file: %s" filepath))

(defun --ecc-send-read-command-eat (filepath)
  "Send a Read command with FILEPATH to the eat buffer."
  (let ((command (format "Read %s" filepath)))
    (if (and (derived-mode-p 'eat-mode)
             (bound-and-true-p eat-terminal))
        (progn
          (eat-term-send-string eat-terminal command)
          (eat-term-send-string eat-terminal (kbd "RET")))
      (message "eat terminal not available, command would be: %s" command))))

(defun --ecc-yank-to-remote-with-ssh-info (content ssh-info)
  "Helper function to yank CONTENT to remote file using SSH-INFO.
Returns local file path on remote host on success, nil on failure."
  (let* ((local-file (--ecc-create-temp-file t))
         (target-dir (if (fboundp '--ecc-get-yank-directory)
                         (--ecc-get-yank-directory t)  ; Get remote directory
                       ecc-directory-for-yank-as-file))
         (remote-local-path
          (concat target-dir (file-name-nondirectory local-file))))
    (--ecc-write-content-to-file content local-file)
    (if (and (require 'ecc-remote nil t)
             (fboundp '--ecc-transfer-file-to-remote))
        (if (--ecc-transfer-file-to-remote local-file ssh-info target-dir)
            remote-local-path ; Return local path on remote host
          (progn
            (message "Failed to transfer file to remote server")
            nil))
      (progn
        (message "Remote transfer functionality not available")
        nil))))

;; ;; 3. (Optional) Keybinding setup
;; ;; ----------------------------------------

;; (defun ecc-eat-yank-as-file-setup-keybinding ()
;;   "Set up keybinding for ecc-eat-yank-as-file in eat-mode."
;;   (when (fboundp 'eat-mode-map)
;;     (define-key eat-mode-map (kbd "C-c C-y") 'ecc-eat-yank-as-file)))

;; ;; Set up keybinding when eat is loaded
;; (with-eval-after-load 'eat
;;   (ecc-eat-yank-as-file-setup-keybinding))


(provide 'ecc-eat-yank-as-file)

(when
    (not load-file-name)
  (message "ecc-eat-yank-as-file.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))