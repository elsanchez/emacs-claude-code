;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: Claude Code (Phase 3 Milestone 3)
;;; Timestamp: <2025-07-17 11:45:00>
;;; File: ecc-project-memory.el

;;; Copyright (C) 2025 Claude Code Enhancement

;; 1. Dependencies
;; ----------------------------------------
(require 'ecc-debug)
(require 'ecc-session-manager)
(require 'cl-lib)

;; 2. Configuration
;; ----------------------------------------

(defcustom ecc-project-memory-auto-save t
  "Whether to automatically save project memory on changes."
  :type 'boolean
  :group 'ecc)

(defcustom ecc-project-memory-max-entries 1000
  "Maximum number of memory entries per project."
  :type 'number
  :group 'ecc)

(defcustom ecc-project-memory-include-file-patterns '("\\.md$" "\\.txt$" "\\.org$" "\\.el$" "\\.py$" "\\.js$" "\\.ts$")
  "File patterns to include in project memory."
  :type '(repeat string)
  :group 'ecc)

;; 3. Variables
;; ----------------------------------------

(defvar ecc-project-memory--cache (make-hash-table :test 'equal)
  "Cache for project memory data.")

(defvar ecc-project-memory--current-project nil
  "Current project root directory.")

;; 4. Project Detection
;; ----------------------------------------

(defun ecc-project-memory--get-project-root ()
  "Get the current project root directory."
  (or (when (fboundp 'projectile-project-root)
        (projectile-project-root))
      (when (fboundp 'project-root)
        (project-root (project-current)))
      default-directory))

(defun ecc-project-memory--get-project-name (project-root)
  "Get a human-readable project name from PROJECT-ROOT."
  (file-name-nondirectory (directory-file-name project-root)))

(defun ecc-project-memory--get-project-key (project-root)
  "Get a unique key for PROJECT-ROOT."
  (secure-hash 'md5 (expand-file-name project-root)))

;; 5. Memory Structure
;; ----------------------------------------

(defun ecc-project-memory--create-memory-structure (project-root)
  "Create a new memory structure for PROJECT-ROOT."
  `(:project-root ,project-root
    :project-name ,(ecc-project-memory--get-project-name project-root)
    :created ,(current-time)
    :last-updated ,(current-time)
    :file-structure ()
    :dependencies ()
    :key-decisions ()
    :conventions ()
    :common-patterns ()
    :session-history ()
    :context-notes ()
    :tech-stack ()
    :build-system ()
    :deployment-info ()
    :team-members ()
    :project-goals ()))

(defun ecc-project-memory--get-memory (project-root)
  "Get memory for PROJECT-ROOT, creating if necessary."
  (let ((project-key (ecc-project-memory--get-project-key project-root)))
    (or (gethash project-key ecc-project-memory--cache)
        (let ((memory (or (ecc-project-memory--load-from-storage project-root)
                          (ecc-project-memory--create-memory-structure project-root))))
          (puthash project-key memory ecc-project-memory--cache)
          memory))))

;; 6. File Structure Analysis
;; ----------------------------------------

(defun ecc-project-memory--analyze-file-structure (project-root)
  "Analyze the file structure of PROJECT-ROOT."
  (let ((files '())
        (directories '())
        (total-size 0))
    
    (when (file-directory-p project-root)
      (dolist (file (directory-files-recursively project-root "." t))
        (let ((relative-path (file-relative-name file project-root)))
          (cond
           ((file-directory-p file)
            (push relative-path directories))
           ((file-regular-p file)
            (let ((size (file-attribute-size (file-attributes file))))
              (setq total-size (+ total-size size))
              (when (cl-some (lambda (pattern)
                               (string-match-p pattern relative-path))
                             ecc-project-memory-include-file-patterns)
                (push `(:path ,relative-path
                        :size ,size
                        :modified ,(file-attribute-modification-time (file-attributes file)))
                      files))))))))
    
    `(:files ,files
      :directories ,directories
      :total-size ,total-size
      :analyzed-at ,(current-time))))

(defun ecc-project-memory--detect-tech-stack (project-root)
  "Detect the technology stack of PROJECT-ROOT."
  (let ((tech-stack '())
        (indicators '(("package.json" . "Node.js/JavaScript")
                      ("requirements.txt" . "Python")
                      ("Pipfile" . "Python/Pipenv")
                      ("pyproject.toml" . "Python/Poetry")
                      ("Cargo.toml" . "Rust")
                      ("pom.xml" . "Java/Maven")
                      ("build.gradle" . "Java/Gradle")
                      ("composer.json" . "PHP")
                      ("Gemfile" . "Ruby")
                      ("go.mod" . "Go")
                      ("project.clj" . "Clojure")
                      ("mix.exs" . "Elixir")
                      ("pubspec.yaml" . "Dart/Flutter")
                      ("*.el" . "Emacs Lisp")
                      ("Makefile" . "Make")
                      ("CMakeLists.txt" . "CMake")
                      ("Dockerfile" . "Docker")
                      ("docker-compose.yml" . "Docker Compose")
                      (".gitignore" . "Git"))))
    
    (dolist (indicator indicators)
      (let ((pattern (car indicator))
            (tech (cdr indicator)))
        (when (or (file-exists-p (expand-file-name pattern project-root))
                  (and (string-match "\\*\\." pattern)
                       (directory-files project-root nil 
                                        (concat "\\." (substring pattern 2) "$"))))
          (push tech tech-stack))))
    
    tech-stack))

(defun ecc-project-memory--detect-build-system (project-root)
  "Detect the build system of PROJECT-ROOT."
  (let ((build-systems '()))
    (cond
     ((file-exists-p (expand-file-name "package.json" project-root))
      (push "npm/yarn" build-systems))
     ((file-exists-p (expand-file-name "Makefile" project-root))
      (push "Make" build-systems))
     ((file-exists-p (expand-file-name "CMakeLists.txt" project-root))
      (push "CMake" build-systems))
     ((file-exists-p (expand-file-name "build.gradle" project-root))
      (push "Gradle" build-systems))
     ((file-exists-p (expand-file-name "pom.xml" project-root))
      (push "Maven" build-systems)))
    
    build-systems))

;; 7. Memory Updating
;; ----------------------------------------

(defun ecc-project-memory--update-memory (project-root)
  "Update memory for PROJECT-ROOT with current analysis."
  (let ((memory (ecc-project-memory--get-memory project-root)))
    (plist-put memory :last-updated (current-time))
    (plist-put memory :file-structure (ecc-project-memory--analyze-file-structure project-root))
    (plist-put memory :tech-stack (ecc-project-memory--detect-tech-stack project-root))
    (plist-put memory :build-system (ecc-project-memory--detect-build-system project-root))
    
    (when ecc-project-memory-auto-save
      (ecc-project-memory--save-to-storage project-root memory))
    
    memory))

(defun ecc-project-memory-add-decision (decision description)
  "Add a key decision to current project memory."
  (interactive "sDecision: \nsDescription: ")
  (let* ((project-root (ecc-project-memory--get-project-root))
         (memory (ecc-project-memory--get-memory project-root))
         (decisions (plist-get memory :key-decisions))
         (new-decision `(:decision ,decision
                         :description ,description
                         :timestamp ,(current-time)
                         :author ,(user-login-name))))
    
    (setq decisions (cons new-decision decisions))
    (plist-put memory :key-decisions decisions)
    (plist-put memory :last-updated (current-time))
    
    (when ecc-project-memory-auto-save
      (ecc-project-memory--save-to-storage project-root memory))
    
    (message "Decision added to project memory")))

(defun ecc-project-memory-add-convention (convention description)
  "Add a coding convention to current project memory."
  (interactive "sConvention: \nsDescription: ")
  (let* ((project-root (ecc-project-memory--get-project-root))
         (memory (ecc-project-memory--get-memory project-root))
         (conventions (plist-get memory :conventions))
         (new-convention `(:convention ,convention
                           :description ,description
                           :timestamp ,(current-time))))
    
    (setq conventions (cons new-convention conventions))
    (plist-put memory :conventions conventions)
    (plist-put memory :last-updated (current-time))
    
    (when ecc-project-memory-auto-save
      (ecc-project-memory--save-to-storage project-root memory))
    
    (message "Convention added to project memory")))

(defun ecc-project-memory-add-note (note)
  "Add a context note to current project memory."
  (interactive "sNote: ")
  (let* ((project-root (ecc-project-memory--get-project-root))
         (memory (ecc-project-memory--get-memory project-root))
         (notes (plist-get memory :context-notes))
         (new-note `(:note ,note
                     :timestamp ,(current-time)
                     :buffer ,(buffer-name (current-buffer))
                     :file ,(buffer-file-name (current-buffer)))))
    
    (setq notes (cons new-note notes))
    (plist-put memory :context-notes notes)
    (plist-put memory :last-updated (current-time))
    
    (when ecc-project-memory-auto-save
      (ecc-project-memory--save-to-storage project-root memory))
    
    (message "Note added to project memory")))

;; 8. Storage Functions
;; ----------------------------------------

(defun ecc-project-memory--save-to-storage (project-root memory)
  "Save project memory to storage."
  (let ((project-name (ecc-project-memory--get-project-name project-root))
        (filename (format "project-memory-%s.json" 
                          (ecc-project-memory--get-project-key project-root))))
    
    ;; Save to Memory Bank
    (condition-case err
        (let ((content (json-encode memory)))
          (mcp__allpepper-memory-bank__memory_bank_write
           "emacs-claude-code" filename content)
          (ecc-debug-message "Project memory saved to Memory Bank: %s" project-name))
      (error
       (ecc-debug-message "Failed to save project memory: %s" (error-message-string err))))
    
    ;; Save to local file as backup
    (condition-case err
        (let ((filepath (expand-file-name filename ecc-session-save-directory)))
          (ecc-session-ensure-directory)
          (with-temp-file filepath
            (insert (json-encode memory)))
          (ecc-debug-message "Project memory saved to file: %s" filepath))
      (error
       (ecc-debug-message "Failed to save project memory to file: %s" (error-message-string err))))))

(defun ecc-project-memory--load-from-storage (project-root)
  "Load project memory from storage."
  (let ((filename (format "project-memory-%s.json" 
                          (ecc-project-memory--get-project-key project-root))))
    
    ;; Try Memory Bank first
    (or (condition-case err
            (let ((content (mcp__allpepper-memory-bank__memory_bank_read
                            "emacs-claude-code" filename)))
              (when content
                (json-read-from-string content)))
          (error
           (ecc-debug-message "Failed to load from Memory Bank: %s" (error-message-string err))
           nil))
        
        ;; Try local file
        (condition-case err
            (let ((filepath (expand-file-name filename ecc-session-save-directory)))
              (when (file-exists-p filepath)
                (with-temp-buffer
                  (insert-file-contents filepath)
                  (json-read))))
          (error
           (ecc-debug-message "Failed to load from file: %s" (error-message-string err))
           nil)))))

;; 9. User Interface
;; ----------------------------------------

(defun ecc-project-memory-show-current ()
  "Show current project memory."
  (interactive)
  (let* ((project-root (ecc-project-memory--get-project-root))
         (memory (ecc-project-memory--get-memory project-root))
         (project-name (plist-get memory :project-name)))
    
    (with-current-buffer (get-buffer-create "*Claude Project Memory*")
      (erase-buffer)
      (insert (format "Project Memory: %s\n" project-name))
      (insert (make-string (+ 16 (length project-name)) ?=))
      (insert "\n\n")
      
      (insert (format "Project Root: %s\n" project-root))
      (insert (format "Created: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S" (plist-get memory :created))))
      (insert (format "Last Updated: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S" (plist-get memory :last-updated))))
      
      ;; Tech Stack
      (let ((tech-stack (plist-get memory :tech-stack)))
        (when tech-stack
          (insert "Technology Stack:\n")
          (dolist (tech tech-stack)
            (insert (format "  - %s\n" tech)))
          (insert "\n")))
      
      ;; Build System
      (let ((build-system (plist-get memory :build-system)))
        (when build-system
          (insert "Build System:\n")
          (dolist (build build-system)
            (insert (format "  - %s\n" build)))
          (insert "\n")))
      
      ;; Key Decisions
      (let ((decisions (plist-get memory :key-decisions)))
        (when decisions
          (insert "Key Decisions:\n")
          (dolist (decision decisions)
            (insert (format "  - %s: %s\n" 
                            (plist-get decision :decision)
                            (plist-get decision :description))))
          (insert "\n")))
      
      ;; Conventions
      (let ((conventions (plist-get memory :conventions)))
        (when conventions
          (insert "Conventions:\n")
          (dolist (convention conventions)
            (insert (format "  - %s: %s\n" 
                            (plist-get convention :convention)
                            (plist-get convention :description))))
          (insert "\n")))
      
      ;; Context Notes
      (let ((notes (plist-get memory :context-notes)))
        (when notes
          (insert "Context Notes:\n")
          (dolist (note (cl-subseq notes 0 (min 10 (length notes))))
            (insert (format "  - %s (%s)\n" 
                            (plist-get note :note)
                            (format-time-string "%Y-%m-%d %H:%M" (plist-get note :timestamp)))))
          (insert "\n")))
      
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun ecc-project-memory-refresh ()
  "Refresh current project memory by re-analyzing the project."
  (interactive)
  (let* ((project-root (ecc-project-memory--get-project-root))
         (memory (ecc-project-memory--update-memory project-root))
         (project-name (plist-get memory :project-name)))
    (message "Project memory refreshed for %s" project-name)))

(defun ecc-project-memory-switch-project (project-root)
  "Switch to project memory for PROJECT-ROOT."
  (interactive "DProject root: ")
  (setq ecc-project-memory--current-project project-root)
  (ecc-project-memory-refresh)
  (message "Switched to project: %s" (ecc-project-memory--get-project-name project-root)))

;; 10. Integration with Session Manager
;; ----------------------------------------

(defun ecc-project-memory--on-session-change ()
  "Hook called when session changes."
  (when ecc-session--current-session
    (let ((project-root (plist-get ecc-session--current-session :project-root)))
      (when project-root
        (ecc-project-memory--update-memory project-root)))))

;; Add hook to session manager
(add-hook 'ecc-session-save-hook 'ecc-project-memory--on-session-change)

(provide 'ecc-project-memory)

;;; ecc-project-memory.el ends here