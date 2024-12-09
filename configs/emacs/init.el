;;; init.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Initialize Emacs.

;;; Code:

;; temporarily disable check for file handlers
(defvar default--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; restore settings
(defun config/reset-file-handler-alist-h ()
  "Restore `file-name-handler-alist`."
  (dolist (handler file-name-handler-alist)
    (add-to-list 'default--file-name-handler-alist handler))
  (setq file-name-handler-alist default--file-name-handler-alist
        gc-cons-threshold 16777216
        gc-cons-percentage 0.1))
(add-hook 'emacs-startup-hook #'config/reset-file-handler-alist-h)

;; don't use custom file
(setopt custom-file (expand-file-name "custom.el" user-emacs-directory))

;; configure use-package
(setopt use-package-always-ensure t
        use-package-enable-imenu-support t
        use-package-always-defer t
        use-package-expand-minimally t)

;; configure package management
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "var/elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; install use-package support
(elpaca elpaca-use-package
        (elpaca-use-package-mode))

;; add custom path
(push (expand-file-name "lisp/" user-emacs-directory) load-path)

;; set font
(set-frame-font "Inconsolata LGC Nerd Font 14" t t)

;; load configuration
(let ((file-name-handler-alist nil))
  (if (file-exists-p (expand-file-name "config.elc" user-emacs-directory))
      (load-file (expand-file-name "config.elc" user-emacs-directory))
    (load-file (expand-file-name "config.el" user-emacs-directory))))

;;; init.el ends here
