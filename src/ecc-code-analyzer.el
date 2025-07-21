;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: Claude Code (Phase 4 Milestone 1)
;;; Timestamp: <2025-07-18 12:35:00>
;;; File: ecc-code-analyzer.el

;;; Copyright (C) 2025 Claude Code Enhancement - Phase 4

;; Semantic Code Analysis for Intelligent Context
;; This module provides deep code understanding capabilities for emacs-claude-code

;; 1. Dependencies
;; ----------------------------------------
(require 'ecc-debug)
(require 'tree-sitter nil t)
(require 'lsp-mode nil t)
(require 'projectile nil t)
(require 'json)
(require 'cl-lib)

;; 2. Configuration
;; ----------------------------------------

(defcustom ecc-code-analyzer-enabled t
  "Whether code analysis is globally enabled."
  :type 'boolean
  :group 'ecc)

(defcustom ecc-code-analyzer-cache-size 100
  "Maximum number of files to keep in analysis cache."
  :type 'number
  :group 'ecc)

(defcustom ecc-code-analyzer-auto-analyze t
  "Whether to automatically analyze files when opened."
  :type 'boolean
  :group 'ecc)

(defcustom ecc-code-analyzer-languages
  '(emacs-lisp python javascript typescript go rust java c cpp)
  "Languages supported for semantic analysis."
  :type '(repeat symbol)
  :group 'ecc)

;; 3. Variables
;; ----------------------------------------

(defvar ecc-code-analyzer--cache (make-hash-table :test 'equal)
  "Cache for analyzed file data.")

(defvar ecc-code-analyzer--last-analysis-time (make-hash-table :test 'equal)
  "Timestamps of last analysis for each file.")

(defvar ecc-code-analyzer--project-cache (make-hash-table :test 'equal)
  "Cache for project-level analysis data.")

;; 4. Core Analysis Functions
;; ----------------------------------------

(defun ecc-code-analyzer-get-language (&optional buffer)
  "Determine the programming language of BUFFER (default current buffer)."
  (with-current-buffer (or buffer (current-buffer))
    (let ((mode-name (symbol-name major-mode)))
      (cond
       ((string-match-p "emacs-lisp\\|elisp\\|lisp-interaction" mode-name) 'emacs-lisp)
       ((string-match-p "python" mode-name) 'python)
       ((string-match-p "js\\|javascript" mode-name) 'javascript)
       ((string-match-p "typescript\\|ts" mode-name) 'typescript)
       ((string-match-p "go" mode-name) 'go)
       ((string-match-p "rust\\|rs" mode-name) 'rust)
       ((string-match-p "java" mode-name) 'java)
       ((string-match-p "c-mode\\|cc-mode" mode-name) 'c)
       ((string-match-p "c++" mode-name) 'cpp)
       (t 'unknown)))))

(defun ecc-code-analyzer-analyze-structure (&optional buffer)
  "Analyze the structure of BUFFER and return semantic information."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((language (ecc-code-analyzer-get-language))
           (file-path (buffer-file-name))
           (content (buffer-string))
           (analysis-result `(:file-path ,file-path
                             :language ,language
                             :timestamp ,(current-time)
                             :buffer-name ,(buffer-name)
                             :size ,(buffer-size))))
      
      ;; Language-specific analysis
      (setf analysis-result 
            (plist-put analysis-result :functions 
                       (ecc-code-analyzer--extract-functions language content)))
      
      (setf analysis-result 
            (plist-put analysis-result :classes 
                       (ecc-code-analyzer--extract-classes language content)))
      
      (setf analysis-result 
            (plist-put analysis-result :imports 
                       (ecc-code-analyzer--extract-imports language content)))
      
      (setf analysis-result 
            (plist-put analysis-result :variables 
                       (ecc-code-analyzer--extract-variables language content)))
      
      ;; Project context
      (when (and (fboundp 'projectile-project-root) (projectile-project-root))
        (setf analysis-result 
              (plist-put analysis-result :project-root (projectile-project-root))))
      
      ;; LSP integration if available
      (when (and (fboundp 'lsp-mode) (bound-and-true-p lsp-mode))
        (setf analysis-result 
              (plist-put analysis-result :lsp-info 
                         (ecc-code-analyzer--get-lsp-info))))
      
      ;; Cache the result
      (when file-path
        (puthash file-path analysis-result ecc-code-analyzer--cache)
        (puthash file-path (current-time) ecc-code-analyzer--last-analysis-time))
      
      (ecc-debug-message "Code analysis completed for %s (%s)" 
                         (buffer-name) language)
      
      analysis-result)))

;; 5. Language-Specific Extractors
;; ----------------------------------------

(defun ecc-code-analyzer--extract-functions (language content)
  "Extract function definitions from CONTENT based on LANGUAGE."
  (let ((functions '()))
    (cond
     ((eq language 'emacs-lisp)
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        (while (re-search-forward "(defun\\s-+\\([[:word:]-]+\\)" nil t)
          (let* ((func-name (match-string 1))
                 (line-number (line-number-at-pos (match-beginning 0)))
                 (doc-string (ecc-code-analyzer--extract-elisp-docstring)))
            (push `(:name ,func-name
                   :line ,line-number
                   :type function
                   :doc-string ,doc-string
                   :language emacs-lisp) functions)))))
     
     ((eq language 'python)
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        (while (re-search-forward "^\\s-*def\\s-+\\([[:word:]_]+\\)\\s-*(" nil t)
          (let* ((func-name (match-string 1))
                 (line-number (line-number-at-pos (match-beginning 0))))
            (push `(:name ,func-name
                   :line ,line-number
                   :type function
                   :language python) functions)))))
     
     ((memq language '(javascript typescript))
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        (while (re-search-forward "\\(function\\s-+\\([[:word:]_]+\\)\\|\\([[:word:]_]+\\)\\s-*[=:]\\s-*function\\|\\([[:word:]_]+\\)\\s-*=>\\)" nil t)
          (let* ((func-name (or (match-string 2) (match-string 3) (match-string 4)))
                 (line-number (line-number-at-pos (match-beginning 0))))
            (when func-name
              (push `(:name ,func-name
                     :line ,line-number
                     :type function
                     :language ,language) functions))))))
     
     ((eq language 'go)
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        (while (re-search-forward "^func\\s-+\\([[:word:]_]+\\)\\s-*(" nil t)
          (let* ((func-name (match-string 1))
                 (line-number (line-number-at-pos (match-beginning 0))))
            (push `(:name ,func-name
                   :line ,line-number
                   :type function
                   :language go) functions)))))
     
     (t
      (ecc-debug-message "Function extraction not implemented for %s" language)))
    
    (nreverse functions)))

(defun ecc-code-analyzer--extract-classes (language content)
  "Extract class definitions from CONTENT based on LANGUAGE."
  (let ((classes '()))
    (cond
     ((eq language 'python)
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        (while (re-search-forward "^\\s-*class\\s-+\\([[:word:]_]+\\)" nil t)
          (let* ((class-name (match-string 1))
                 (line-number (line-number-at-pos (match-beginning 0))))
            (push `(:name ,class-name
                   :line ,line-number
                   :type class
                   :language python) classes)))))
     
     ((memq language '(javascript typescript))
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        (while (re-search-forward "^\\s-*class\\s-+\\([[:word:]_]+\\)" nil t)
          (let* ((class-name (match-string 1))
                 (line-number (line-number-at-pos (match-beginning 0))))
            (push `(:name ,class-name
                   :line ,line-number
                   :type class
                   :language ,language) classes)))))
     
     ((eq language 'java)
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        (while (re-search-forward "\\(public\\|private\\|protected\\)?\\s-*class\\s-+\\([[:word:]_]+\\)" nil t)
          (let* ((class-name (match-string 2))
                 (line-number (line-number-at-pos (match-beginning 0))))
            (push `(:name ,class-name
                   :line ,line-number
                   :type class
                   :language java) classes)))))
     
     (t
      (ecc-debug-message "Class extraction not implemented for %s" language)))
    
    (nreverse classes)))

(defun ecc-code-analyzer--extract-imports (language content)
  "Extract import/require statements from CONTENT based on LANGUAGE."
  (let ((imports '()))
    (cond
     ((eq language 'emacs-lisp)
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        (while (re-search-forward "(require\\s-+'\\([[:word:]-]+\\)" nil t)
          (let* ((module-name (match-string 1))
                 (line-number (line-number-at-pos (match-beginning 0))))
            (push `(:module ,module-name
                   :line ,line-number
                   :type require
                   :language emacs-lisp) imports)))))
     
     ((eq language 'python)
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        (while (re-search-forward "^\\s-*\\(import\\|from\\)\\s-+\\([[:word:]_.]+\\)" nil t)
          (let* ((import-type (match-string 1))
                 (module-name (match-string 2))
                 (line-number (line-number-at-pos (match-beginning 0))))
            (push `(:module ,module-name
                   :line ,line-number
                   :type ,(intern import-type)
                   :language python) imports)))))
     
     ((memq language '(javascript typescript))
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        (while (re-search-forward "\\(import\\|require\\).*['\"]\\([^'\"]+\\)['\"]" nil t)
          (let* ((import-type (match-string 1))
                 (module-name (match-string 2))
                 (line-number (line-number-at-pos (match-beginning 0))))
            (push `(:module ,module-name
                   :line ,line-number
                   :type ,(intern import-type)
                   :language ,language) imports)))))
     
     (t
      (ecc-debug-message "Import extraction not implemented for %s" language)))
    
    (nreverse imports)))

(defun ecc-code-analyzer--extract-variables (language content)
  "Extract variable definitions from CONTENT based on LANGUAGE."
  (let ((variables '()))
    (cond
     ((eq language 'emacs-lisp)
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        (while (re-search-forward "(def\\(var\\|custom\\)\\s-+\\([[:word:]-]+\\)" nil t)
          (let* ((var-type (match-string 1))
                 (var-name (match-string 2))
                 (line-number (line-number-at-pos (match-beginning 0))))
            (push `(:name ,var-name
                   :line ,line-number
                   :type ,(intern var-type)
                   :language emacs-lisp) variables)))))
     
     ((eq language 'python)
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        (while (re-search-forward "^\\s-*\\([[:word:]_]+\\)\\s-*=" nil t)
          (let* ((var-name (match-string 1))
                 (line-number (line-number-at-pos (match-beginning 0))))
            (push `(:name ,var-name
                   :line ,line-number
                   :type variable
                   :language python) variables)))))
     
     (t
      (ecc-debug-message "Variable extraction not implemented for %s" language)))
    
    (nreverse variables)))

;; 6. Helper Functions
;; ----------------------------------------

(defun ecc-code-analyzer--extract-elisp-docstring ()
  "Extract docstring for current elisp function."
  (save-excursion
    (forward-line 1)
    (skip-chars-forward " \t\n")
    (when (looking-at "\"")
      (let ((start (point)))
        (forward-sexp 1)
        (buffer-substring-no-properties (1+ start) (1- (point)))))))

(defun ecc-code-analyzer--get-lsp-info ()
  "Get LSP-specific information for current buffer."
  (when (and (fboundp 'lsp-mode) (bound-and-true-p lsp-mode))
    `(:server ,(when (fboundp 'lsp--cur-workspace)
                 (lsp--workspace-server-id (car (lsp--cur-workspace))))
      :diagnostics ,(when (fboundp 'lsp-diagnostics-stats-for)
                      (lsp-diagnostics-stats-for (current-buffer)))
      :symbols ,(when (fboundp 'lsp-document-symbols)
                  (condition-case nil
                      (length (lsp-document-symbols))
                    (error 0))))))

;; 7. Project Analysis
;; ----------------------------------------

(defun ecc-code-analyzer-analyze-project (&optional project-root)
  "Analyze the entire project structure."
  (let* ((root (or project-root 
                   (when (fboundp 'projectile-project-root)
                     (projectile-project-root))
                   default-directory))
         (project-key (md5 root))
         (cached-analysis (gethash project-key ecc-code-analyzer--project-cache)))
    
    ;; Return cached analysis if recent
    (if (and cached-analysis
             (< (float-time (time-subtract (current-time) 
                                         (plist-get cached-analysis :timestamp))) 
                300)) ; 5 minutes
        cached-analysis
      
      ;; Perform new analysis
      (let* ((files (ecc-code-analyzer--get-project-files root))
             (file-analyses '())
             (total-functions 0)
             (total-classes 0)
             (languages (make-hash-table :test 'eq)))
        
        ;; Analyze each file
        (dolist (file files)
          (when (file-exists-p file)
            (with-temp-buffer
              (insert-file-contents file)
              (let* ((temp-buffer (current-buffer))
                     (analysis (ecc-code-analyzer-analyze-structure temp-buffer))
                     (lang (plist-get analysis :language))
                     (funcs (length (plist-get analysis :functions)))
                     (classes (length (plist-get analysis :classes))))
                
                (push analysis file-analyses)
                (setq total-functions (+ total-functions funcs))
                (setq total-classes (+ total-classes classes))
                (puthash lang (1+ (gethash lang languages 0)) languages)))))
        
        ;; Create project analysis summary
        (let ((project-analysis 
               `(:project-root ,root
                 :timestamp ,(current-time)
                 :total-files ,(length files)
                 :total-functions ,total-functions
                 :total-classes ,total-classes
                 :languages ,(hash-table-keys languages)
                 :language-distribution ,languages
                 :files ,file-analyses
                 :dependencies ,(ecc-code-analyzer--analyze-project-dependencies root))))
          
          ;; Cache the result
          (puthash project-key project-analysis ecc-code-analyzer--project-cache)
          (ecc-debug-message "Project analysis completed for %s (%d files)" 
                             root (length files))
          
          project-analysis)))))

(defun ecc-code-analyzer--get-project-files (root)
  "Get all source files in project ROOT."
  (let ((extensions '(".el" ".py" ".js" ".ts" ".go" ".rs" ".java" ".c" ".cpp" ".h"))
        (files '()))
    (dolist (ext extensions)
      (setq files (append files 
                          (directory-files-recursively root 
                                                       (concat "\\" ext "$")))))
    ;; Filter out common ignore patterns
    (cl-remove-if (lambda (file)
                    (string-match-p "\\(node_modules\\|__pycache__\\|\\.git\\|target/debug\\|target/release\\)" file))
                  files)))

(defun ecc-code-analyzer--analyze-project-dependencies (root)
  "Analyze project dependencies from various manifest files."
  (let ((deps '()))
    ;; Package.json (Node.js)
    (let ((package-json (expand-file-name "package.json" root)))
      (when (file-exists-p package-json)
        (condition-case nil
            (let* ((json-object-type 'plist)
                   (json-array-type 'list)
                   (data (json-read-file package-json))
                   (dependencies (plist-get data :dependencies))
                   (dev-deps (plist-get data :devDependencies)))
              (push `(:type npm :file "package.json" 
                     :dependencies ,dependencies
                     :dev-dependencies ,dev-deps) deps))
          (error nil))))
    
    ;; requirements.txt (Python)
    (let ((requirements (expand-file-name "requirements.txt" root)))
      (when (file-exists-p requirements)
        (with-temp-buffer
          (insert-file-contents requirements)
          (let ((python-deps '()))
            (while (not (eobp))
              (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                (when (and (not (string-empty-p line)) 
                           (not (string-prefix-p "#" line)))
                  (push line python-deps)))
              (forward-line 1))
            (when python-deps
              (push `(:type python :file "requirements.txt" 
                     :dependencies ,(nreverse python-deps)) deps))))))
    
    ;; Cargo.toml (Rust)
    (let ((cargo-toml (expand-file-name "Cargo.toml" root)))
      (when (file-exists-p cargo-toml)
        (push `(:type rust :file "Cargo.toml") deps)))
    
    deps))

;; 8. User Interface Functions
;; ----------------------------------------

(defun ecc-code-analyzer-show-current-analysis ()
  "Show analysis for current buffer."
  (interactive)
  (let ((analysis (ecc-code-analyzer-analyze-structure)))
    (with-current-buffer (get-buffer-create "*Code Analysis*")
      (erase-buffer)
      (insert "Code Analysis Report\n")
      (insert "===================\n\n")
      
      (insert (format "File: %s\n" (plist-get analysis :file-path)))
      (insert (format "Language: %s\n" (plist-get analysis :language)))
      (insert (format "Buffer: %s\n" (plist-get analysis :buffer-name)))
      (insert (format "Size: %d characters\n\n" (plist-get analysis :size)))
      
      ;; Functions
      (let ((functions (plist-get analysis :functions)))
        (insert (format "Functions (%d):\n" (length functions)))
        (dolist (func functions)
          (insert (format "  - %s (line %d)\n" 
                         (plist-get func :name)
                         (plist-get func :line))))
        (insert "\n"))
      
      ;; Classes
      (let ((classes (plist-get analysis :classes)))
        (insert (format "Classes (%d):\n" (length classes)))
        (dolist (class classes)
          (insert (format "  - %s (line %d)\n" 
                         (plist-get class :name)
                         (plist-get class :line))))
        (insert "\n"))
      
      ;; Imports
      (let ((imports (plist-get analysis :imports)))
        (insert (format "Imports (%d):\n" (length imports)))
        (dolist (import imports)
          (insert (format "  - %s (line %d)\n" 
                         (plist-get import :module)
                         (plist-get import :line))))
        (insert "\n"))
      
      ;; Project info
      (when (plist-get analysis :project-root)
        (insert (format "Project: %s\n" (plist-get analysis :project-root))))
      
      ;; LSP info
      (when (plist-get analysis :lsp-info)
        (let ((lsp-info (plist-get analysis :lsp-info)))
          (insert (format "LSP Server: %s\n" (plist-get lsp-info :server)))
          (insert (format "LSP Symbols: %s\n" (plist-get lsp-info :symbols)))))
      
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun ecc-code-analyzer-show-project-analysis ()
  "Show analysis for current project."
  (interactive)
  (let ((analysis (ecc-code-analyzer-analyze-project)))
    (with-current-buffer (get-buffer-create "*Project Analysis*")
      (erase-buffer)
      (insert "Project Analysis Report\n")
      (insert "=======================\n\n")
      
      (insert (format "Project Root: %s\n" (plist-get analysis :project-root)))
      (insert (format "Total Files: %d\n" (plist-get analysis :total-files)))
      (insert (format "Total Functions: %d\n" (plist-get analysis :total-functions)))
      (insert (format "Total Classes: %d\n" (plist-get analysis :total-classes)))
      (insert (format "Languages: %s\n\n" (plist-get analysis :languages)))
      
      ;; Language distribution
      (insert "Language Distribution:\n")
      (let ((lang-dist (plist-get analysis :language-distribution)))
        (maphash (lambda (lang count)
                   (insert (format "  %s: %d files\n" lang count)))
                 lang-dist))
      (insert "\n")
      
      ;; Dependencies
      (let ((deps (plist-get analysis :dependencies)))
        (when deps
          (insert "Dependencies:\n")
          (dolist (dep deps)
            (insert (format "  %s (%s)\n" 
                           (plist-get dep :type)
                           (plist-get dep :file))))
          (insert "\n")))
      
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;; 9. Integration Hooks
;; ----------------------------------------

(defun ecc-code-analyzer-on-file-opened ()
  "Hook to run when a file is opened."
  (when (and ecc-code-analyzer-enabled
             ecc-code-analyzer-auto-analyze
             (buffer-file-name)
             (memq (ecc-code-analyzer-get-language) ecc-code-analyzer-languages))
    (run-with-idle-timer 1 nil #'ecc-code-analyzer-analyze-structure)))

(defun ecc-code-analyzer-on-file-saved ()
  "Hook to run when a file is saved."
  (when (and ecc-code-analyzer-enabled
             (buffer-file-name)
             (memq (ecc-code-analyzer-get-language) ecc-code-analyzer-languages))
    (ecc-code-analyzer-analyze-structure)))

;; Add hooks
(add-hook 'find-file-hook #'ecc-code-analyzer-on-file-opened)
(add-hook 'after-save-hook #'ecc-code-analyzer-on-file-saved)

;; 10. Initialization
;; ----------------------------------------

(defun ecc-code-analyzer-initialize ()
  "Initialize the code analyzer."
  (when ecc-code-analyzer-enabled
    (ecc-debug-message "Code analyzer initialized")
    ;; Clear old cache entries
    (when (> (hash-table-count ecc-code-analyzer--cache) ecc-code-analyzer-cache-size)
      (clrhash ecc-code-analyzer--cache)
      (clrhash ecc-code-analyzer--last-analysis-time))))

;; Auto-initialize
(add-hook 'emacs-startup-hook #'ecc-code-analyzer-initialize)

(provide 'ecc-code-analyzer)

;;; ecc-code-analyzer.el ends here