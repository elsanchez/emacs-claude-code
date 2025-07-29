;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-07-23 00:00:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-claude-code/src/ecc-eat-utils.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;; 1. Dependencies
;; ----------------------------------------
(require 'ecc-debug)

;; 2. Configuration
;; ----------------------------------------

(defcustom --ecc-eat-yank-extension-patterns
  '(("sh" . "^#!.*sh")
    ("py" . "^\\(import\\|from\\|def\\|class\\|if __name__ ==\\)")
    ("js"
     . "\\(function\\|const\\|let\\|var\\|=>\\|import\\|export\\)")
    ("html" . "\\(<html\\|<!DOCTYPE\\|<body\\|<div\\|<script\\)")
    ("css" . "\\([.#]?[a-zA-Z0-9_-]+\\s-*{\\)")
    ("el"
     . "\\((defun\\|(defvar\\|(defcustom\\|(require\\|(provide\\)")
    ("txt" . ".*"))
  "Alist mapping file extensions to regex patterns for content detection."
  :type '(alist :key-type string :value-type regexp)
  :group 'ecc)

(defcustom --ecc-eat-yank-as-file-prompt t
  "Whether to prompt before yanking as file."
  :type 'boolean
  :group 'ecc)

(defcustom --ecc-eat-yank-as-file-message-format "See <%s>"
  "Format string for the message sent to Claude. %s is replaced with the file path."
  :type 'string
  :group 'ecc)

;; 3. Variables
;; ----------------------------------------

(defvar --ecc-eat-yank-history nil
  "History of filenames used for yanking eat content.")

(defcustom --ecc-eat-yank-as-file-enabled nil
  "Whether to enable automatic yank-as-file prompt on eat-yank."
  :type 'boolean
  :group 'ecc)

;; 4. Core Functions
;; ----------------------------------------

(defun --ecc-eat-send-command (command)
  "Send COMMAND to the current eat terminal buffer."
  (when (derived-mode-p 'eat-mode)
    (when (bound-and-true-p eat-terminal)
      (eat-term-send-string eat-terminal command)
      (eat-term-send-string eat-terminal (kbd "RET")))))

(defun --ecc-eat-send-string (string)
  "Send STRING to the current eat terminal buffer without return."
  (when (derived-mode-p 'eat-mode)
    (when (bound-and-true-p eat-terminal)
      (eat-term-send-string eat-terminal string))))

(defun --ecc-eat-send-return ()
  "Send return/enter to the current eat terminal buffer."
  (when (derived-mode-p 'eat-mode)
    (when (bound-and-true-p eat-terminal)
      (eat-term-send-string eat-terminal (kbd "RET")))))

(defun --ecc-eat-buffer-p (&optional buffer)
  "Check if BUFFER (or current buffer) is an eat-mode buffer."
  (with-current-buffer (or buffer (current-buffer))
    (derived-mode-p 'eat-mode)))

(defun --ecc-eat-process-live-p ()
  "Check if the eat terminal process is alive."
  (when (and (bound-and-true-p eat-terminal)
             eat-terminal)
    (let ((process (eat-term-parameter eat-terminal 'eat--process)))
      (and process (process-live-p process)))))

(defun --ecc-eat-get-buffer-content ()
  "Get the visible content of the eat buffer."
  (when (derived-mode-p 'eat-mode)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun --ecc-eat-detect-extension (content)
  "Detect file extension based on CONTENT using patterns."
  (catch 'found
    (dolist (pattern-entry --ecc-eat-yank-extension-patterns)
      (when (string-match (cdr pattern-entry) content)
        (throw 'found (car pattern-entry))))
    "txt"))

(defun --ecc-eat-optimize-scrolling ()
  "Apply eat-specific scrolling optimizations based on claudemacs approach."
  (when (derived-mode-p 'eat-mode)
    ;; Optimize scrolling for terminal input - allows text to go to bottom
    (setq-local scroll-conservatively 10000)  ; Never recenter
    (setq-local scroll-margin 0)              ; No margin so text goes to edge
    (setq-local maximum-scroll-margin 0)      ; No maximum margin
    (setq-local scroll-preserve-screen-position t)  ; Preserve position during scrolling
    
    ;; Additional stabilization for blinking character height changes
    (setq-local auto-window-vscroll nil)      ; Disable automatic scrolling adjustments
    (setq-local scroll-step 1)                ; Scroll one line at a time
    (setq-local hscroll-step 1)               ; Horizontal scroll one column at a time
    (setq-local hscroll-margin 0)             ; No horizontal scroll margin
    
    ;; Force consistent line spacing to prevent height fluctuations
    (setq-local line-spacing 0)               ; No extra line spacing
    
    ;; Disable eat's text blinking to reduce display changes
    (when (bound-and-true-p eat-enable-blinking-text)
      (setq-local eat-enable-blinking-text nil))
    
    ;; Force consistent character metrics for blinking symbols
    (setq-local vertical-scroll-bar nil)      ; Disable scroll bar
    (setq-local fringe-mode 0)                ; Disable fringes that can cause reflow
    
    ;; Replace problematic blinking character with consistent asterisk
    (let ((display-table (make-display-table)))
      (aset display-table #x23fa [?✽])  ; Replace ⏺ (U+23FA) with ✽
      (setq-local buffer-display-table display-table))
    
    ;; Return nil explicitly for tests
    nil))

(defun --ecc-eat-setup-buffer ()
  "Set up eat buffer with optimizations and configurations."
  (when (derived-mode-p 'eat-mode)
    (--ecc-eat-optimize-scrolling)
    ;; Additional eat-specific setup can go here
    ))

;; 5. Integration Functions
;; ----------------------------------------

(defun ecc-eat-mode-hook ()
  "Hook function to set up eat buffers for emacs-claude-code."
  (when (derived-mode-p 'eat-mode)
    (--ecc-eat-setup-buffer)))

;; Add hook for eat mode
(add-hook 'eat-mode-hook 'ecc-eat-mode-hook)

(provide 'ecc-eat-utils)

(when
    (not load-file-name)
  (message "ecc-eat-utils.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))