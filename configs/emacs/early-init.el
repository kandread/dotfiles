;;; early-init.el -*- lexical-binding: t; -*-

;;; Commentary:

;; The file called when Emacs is first started.

;;; Code:

;; disable package management at startup
(setopt package-enable-at-startup nil)

;; defer garbage collection to decrease the load and compile time
(setopt gc-cons-threshold most-positive-fixnum)

;; increase gc threshold
(setopt gc-cons-percentage 0.6)

;; silence compiler warnings as they can be pretty disruptive
(setopt native-comp-async-report-warnings-errors nil)

;; change path for native compilation cache directory
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))

;; prefer newer compiled files
(setopt load-prefer-newer noninteractive)

;; inhibit frame resizing at this early stage
(setopt frame-inhibit-implied-resize t)

;; don't warn me unless it's serious
(setopt warning-minimum-level :emergency)

;; inhibit splash screen, menus etc
(setopt inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-buffer-menu t)

;; no menu bar, toolbar, scroll bar
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;;; early-init.el ends here
