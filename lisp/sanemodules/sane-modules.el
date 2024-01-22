;;; sane-modules.el --- Emacs Lisp package for SANE modules -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                         V A R I A B L E S                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup sane-modules nil
  "Customization options for sane-modules package."
  :group 'convenience
  :prefix "sane-module-")

(defcustom sane-module-base-directory "~/sane-modules/"
  "Base directory for SANE modules."
  :type 'string
  :group 'sane-modules)

(defcustom sane-module-module-list '()
  "List of SANE modules to load."
  :type '(repeat symbol)
  :group 'sane-modules)

(defcustom sane-module-config-list '()
  "List of SANE module configurations."
  :type '(repeat string)
  :group 'sane-modules)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                             C O D E                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl-lib)

;;;###autoload
(defun sane-modules-expand-paths (base-directory module-list)
  "Expand each symbol in MODULE-LIST to a subdirectory path within BASE-DIRECTORY.
Validate the existence of each path and return a list of the valid paths."
  (cl-loop for module in module-list
           for subdirectory = (expand-file-name (symbol-name module) base-directory)
           when (file-directory-p subdirectory)
           collect subdirectory))

;;;###autoload
(defun sane-modules-find-config-files (directory-list)
  "Find 'config.el' files within each directory in DIRECTORY-LIST.
Validate the existence of the file in each directory and return a list
of fully expanded file paths for the valid 'config.el' files."
  (let ((valid-paths '()))
    (dolist (directory directory-list valid-paths)
      (let ((config-file (expand-file-name "config.el" directory)))
        (when (file-exists-p config-file)
          (push config-file valid-paths))))))

(defun sane-modules-load-files (file-list timing-flag)
  "Load files from FILE-LIST.
Validate the existence of each file and load it. If TIMING-FLAG is non-nil,
report the load time for each file; otherwise, load the file without reporting load times."
  (let ((loaded-files '()))
    (cl-loop for file in file-list
             unless (file-exists-p file)
             do (message "File does not exist: %s" file)
             else do (if timing-flag
                         (let ((load-time (benchmark-run (load file))))
                           (message "Loaded file: %s, Load time: %.6f seconds" file (car load-time)))
                       (load file))
             finally return (nreverse loaded-files))))

;;;###autoload
(defun sane-modules-load-modules (base-directory module-list &optional timing-flag)
  "Load configuration files for modules in MODULE-LIST under BASE-DIRECTORY.
If TIMING-FLAG is non-nil, report the load time for each file; otherwise,
load the file without reporting load times."
  ;; (setq sane-module-paths (sane-modules-expand-paths base-directory module-list) timing-flag)
  ;; (setq sane-module-configs (sane-modules-find-config-files sane-module-paths))
  ;; (sane-modules-load-files sane-module-configs timing-flag)
  (sane-modules-load-files
   (sane-modules-find-config-files
    (sane-modules-expand-paths base-directory module-list)) timing-flag))

(provide 'sane-modules)

;;; sane-modules.el ends here
