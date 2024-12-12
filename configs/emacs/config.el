;;; config.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Configuration file for Emacs

;;; Code:

;; general emacs settings
(use-package emacs
  :ensure nil
  :custom
  (user-full-name "Kostas Andreadis")
  (user-mail-address "kandread@umass.edu")
  (visible-bell nil)
  (visible-cursor nil)
  (ring-bell-function #'ignore)
  (use-short-answers t)
  (enable-recursive-minibuffers t)
  (create-lockfiles nil)
  (cursor-in-non-selected-windows nil)
  (use-dialog-box nil)
  (use-file-dialog nil)
  (bidi-paragraph-direction 'left-to-right)
  (bidi-inhibit-bpa t)
  (inhibit-default-init t)
  (initial-scratch-message nil)
  (help-window-select t)
  :init
  (set-language-environment "UTF-8"))

;; no littering
(use-package no-littering
  :custom
  (auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; recent files
(use-package recentf
  :ensure nil
  :hook (elpaca-after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 100)
  (recentf-max-menu-items 0)
  (recentf-filename-handlers '(file-truename))
  (recentf-auto-cleanup 'never)
  (recentf-exclude '("Mail/umass" "/nix/store" "/tmp" "/private" "./elpaca.")))

;; history
(use-package savehist
  :ensure nil
  :hook (elpaca-after-init . savehist-mode))

(use-package save-place
  :ensure nil
  :hook (elpaca-after-init . save-place-mode))

;; file settings
(use-package files
  :ensure nil
  :custom
  (auto-save-default nil)
  (backup-by-copying t)
  (delete-old-versions t)
  (version-control t)
  (kept-new-versions 1)
  (kept-old-versions 1)
  (safe-local-variable-values '((buffer-read-only . 1)))
  (backup-directory-alist `((".*" . ,temporary-file-directory)))
  (auto-save-file-name-transforms `((".*" ,temporary-file-directory t))))

;; buffers
(use-package buffers
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :custom
  (uniquify-buffer-name-style 'forward))

;; scrolling
(use-package pixel-scroll
  :ensure nil
  :custom
  (auto-window-vscroll nil)
  (fast-but-imprecise-scrolling t)
  (scroll-conservatively 101)
  (scroll-margin 0)
  (scroll-preserve-screen-position t)
  :hook (elpaca-after-init . pixel-scroll-precision-mode))

;; windows
(use-package window
  :ensure nil
  :hook (elpaca-after-init . winner-mode)
  :bind
  ("C-x -" . split-window-vertically)
  ("C-x |" . split-window-horizontally)
  ("C-x =" . resize-window)
  :config
  (windmove-default-keybindings '(ctrl shift))
  (defun resize-window (&optional arg)
    "Resize window interactively."
    (interactive "P")
    (if (one-window-p) (error "Cannot resize sole window"))
    (setq arg (if arg (prefix-numeric-value arg) 1))
    (let (c)
      (catch 'done
        (while t
          (message
           "C-n=heighten, C-p=shrink, C-f=widen, C-b=narrow (by %d);  0-9=unit, <RET>=quit"
           arg)
          (setq c (read-char))
          (condition-case ()
              (cond
               ((= c ?\^N) (enlarge-window arg))
               ((= c ?\^P) (shrink-window arg))
               ((= c ?\^F) (enlarge-window-horizontally arg))
               ((= c ?\^B) (shrink-window-horizontally arg))
               ((= c ?\^G) (keyboard-quit))
               ((= c ?\^M) (throw 'done t))
               ((= c ?0) (setq arg 10))
               ((and (> c ?0) (<= c ?9)) (setq arg (- c ?0)))
               (t (beep)))
            (error (beep)))))
      (message "Done.")))
  (defun quit-window-a ()
    "When running `quit-window', always kill the buffer."
    (ad-set-arg 0 t))
  (advice-add 'quit-window-always-kill :before #'quit-window))

;; easily switch windows
(use-package ace-window
  :bind ("M-o" . ace-window)
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?r ?s ?t ?n ?e ?i ?o)))

(use-package transpose-frame)

;; flash current line after movement
(use-package pulsar
  :hook (elpaca-after-init . pulsar-global-mode))

;; dired
(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-golh --group-directories-first")
  (dired-use-ls-dired nil)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-auto-revert-buffer t)
  (dired-kill-when-opening-new-dired-buffer nil)
  (dired-hide-details-hide-symlink-targets nil))

(use-package dired-launch
  :hook (dired-load . dired-launch-mode)
  :bind (:map dired-mode-map
              ("J" . dired-launch-command)
              ("K" . dired-launch-command-with-prompt)))

(use-package dired-narrow
  :after dired
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-remove)))

;; bookmarks
(use-package bookmark
  :ensure nil
  :custom
  (bookmark-set-fringe-mark nil))

;; completion
(use-package vertico
  :hook (elpaca-after-init . vertico-mode)
  :bind (:map vertico-map
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :custom
  (vertico-cycle t)
  (vertico-resize nil))

;; fuzzy matching
(use-package orderless
  :demand
  :custom
  (completion-category-defaults nil)
  (completion-styles '(orderless))
  (completion-category-overrides '((file (styles . (basic-remote partial-completion)))))
  :config
  (defun basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))
  (defun basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))
  (add-to-list 'completion-styles-alist
               '(basic-remote basic-remote-try-completion basic-remote-all-completions nil)))

;; templates
(use-package tempel
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert))
  :custom
  (tempel-path (expand-file-name "templates" user-emacs-directory))
  :config
  (defun tempel-setup-capf ()
    "Add tempel capf to completion-at-point-functions"
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  :hook ((prog-mode text-mode) . tempel-setup-capf))

;; completion popups
(use-package corfu
  :hook ((prog-mode org-mode) . corfu-mode)
  :custom
  (tab-always-indent 'complete)
  (completion-cycle-threshold 3)
  (corfu-quit-at-boundary nil)
  (corfu-cycle t))

;; consult completing-read commands
(use-package consult
  :defer 1
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :config
  (require '+consult)
  :bind
  ("M-C" . capitalize-word)
  (:prefix-map consult-map
               :prefix "M-c"
               ("g" . consult-ripgrep)
               ("i" . consult-imenu)
               ("k" . consult-global-mark)
               ("r" . consult-recent-file)
               ("t" . consult-theme)
               ("l" . consult-line)
               ("h" . consult-org-heading)
               ("b" . consult-buffer)
               ("f" . consult-flymake)
               ("y" . consult-yank-pop)
               ("a" . consult-org-agenda)))

(use-package consult-dir
  :after consult
  :bind
  (:map consult-map ("d" . consult-dir))
  (:map vertico-map ("C-x C-d" . consult-dir)))

;; theming
(use-package nerd-icons)

(use-package modus-themes
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui nil)
  (modus-themes-custom-auto-reload t)
  (modus-themes-prompts '(intense bold))
  (modus-themes-completions '((matches . (extrabold))
                              (selection . (semibold accented))
                              (popup . (accented intense))))
  (modus-themes-org-blocks 'gray-background))

(use-package ef-themes
  :custom
  (ef-themes-mixed-fonts t)
  (ef-themes-variable-pitch-ui t)
  (ef-themes-region '(intense no-extend neutral)))

(use-package standard-themes
  :custom
  (standard-themes-bold-constructs t)
  (standard-themes-italic-constructs t)
  (standard-themes-disable-other-themes t)
  (standard-themes-mixed-fonts t)
  (standard-themes-variable-pitch-ui t)
  (standard-themes-prompts '(extrabold italic)))

(use-package doom-themes)

;; transient commands
(use-package transient)

;; version control
(use-package autorevert
  :ensure nil
  :custom
  (auto-revert-check-vcinfo t))

(use-package magit
  :bind ("C-x g" . magit-status)
  :custom
  (magit-diff-refine-hunk t)
  (magit-save-repository-buffers nil)
  (magit-revision-insert-related-refs nil))

(use-package git-auto-commit-mode
  :custom
  (gac-automatically-push-p t))

;; contextual actions
(use-package embark
  :bind ("C-." . embark-act)
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :demand
  :after embark)

;; minibuffer annotations
(use-package marginalia
  :hook (elpaca-after-init . marginalia-mode)
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle)))

;; editing
(use-package wgrep
  :defer 2)

(use-package simple
  :ensure nil
  :config
  (delete-selection-mode)
  :custom
  (indent-tabs-mode nil)
  (kill-do-not-save-duplicates t)
  (sentence-end-double-space nil)
  :bind
  ([remap upcase-word] . upcase-dwim)
  ([remap downcase-word] . downcase-dwim))

(use-package so-long
  :ensure nil
  :hook (elpaca-after-init . global-so-long-mode))

(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup))

(use-package show-paren
  :ensure nil
  :hook (elpaca-after-init . show-paren-mode))

(use-package electric-pair
  :ensure nil
  :hook (elpaca-after-init . electric-pair-mode))

(use-package visual-line
  :ensure nil
  :hook ((prog-mode text-mode) . visual-line-mode))

;; wrap region
(use-package wrap-region
  :hook ((prog-mode org-mode) . wrap-region-mode)
  :config
  (wrap-region-add-wrappers
   '(("/" "/" nil org-mode)
     ("*" "*" nil org-mode))))

;; indent aggressively
(use-package aggressive-indent
  :hook (elpaca-after-init . global-aggressive-indent-mode)
  :config
  (dolist (mode '(python-mode haskell-mode))
    (add-to-list 'aggressive-indent-excluded-modes mode)))

;; useful extension for Emacs
(use-package crux
  :bind
  ([remap kill-line] . crux-smart-kill-line)
  ("C-o" . crux-smart-open-line)
  ("C-S-o" . crux-smart-open-line-above)
  ("M-<tab>" . crux-other-window-or-switch-buffer))

;; kill and mark things easily
(use-package easy-kill
  :bind
  ([remap kill-ring-save] . easy-kill)
  ([remap mark-sexp] . easy-mark))

;; spelling
(use-package ispell
  :ensure nil
  :custom
  (ispell-program-name "hunspell")
  (ispell-dictionary "en_US")
  :config
  (dolist (item '((":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:")
                  ("#\\+BEGIN_SRC" . "#\\+END_SRC")
                  ("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE")))
    (add-to-list 'ispell-skip-region-alist item)))

(use-package jinx
  :ensure nil
  :hook ((text-mode prog-mode) . jinx-mode)
  :bind
  ("C-;" . jinx-correct-nearest)
  ("C-M-;" . jinx-correct-all))

;; jump to things
(use-package avy
  :custom
  (avy-timeout-seconds 0.5)
  :bind
  ("M-j" . avy-goto-char-timer)
  ("M-g l" . avy-goto-line))

(use-package link-hint
  :bind
  ("C-x l" . link-hint-open-link)
  ("C-x M-l" . link-hint-copy-link))

(use-package goto-last-change
  :bind ("C-x C-'" . goto-last-change))

;; undo
(use-package undo-fu
  :bind
  ("C-z" . undo-fu-only-undo)
  ("C-S-z" . undo-fu-only-redo))

(use-package vundo
  :bind ("C-M-z" . vundo))

;; substitute targets
(use-package substitute
  :custom
  (substitute-highlights t)
  :bind-keymap ("C-c s" . substitute-prefix-map)
  :bind (:map substitute-prefix-map
              ("s" . substitute-target-below-point)
              ("r" . substitute-target-above-point)
              ("d" . substitute-target-in-defun)
              ("b" . substitute-target-in-buffer))
  :hook (substitute-post-replace . substitute-report-operation))

;; replace built-in comment-dwim command
(use-package comment-dwim-2
  :bind
  ([remap comment-dwim] . comment-dwim-2)
  (:map org-mode-map ("M-;" . org-comment-dwim-2)))

;; programming languages
(use-package nix-mode)

(use-package haskell-mode)

;; plantUML
(use-package plantuml-mode
  :custom
  (org-plantuml-jar-path (string-replace "bin/plantuml" "lib/plantuml.jar" (string-trim-right (shell-command-to-string "readlink $(which plantuml)"))))
  (plantuml-executable-path (executable-find "plantuml"))
  (plantuml-default-exec-mode 'executable)
  (plantuml-indent-level 2))

;; graphviz
(use-package graphviz-dot-mode
  :custom (graphviz-dot-indent-width 2))

;; highlight keywords
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

;; rainbow colors
(use-package rainbow-mode)

;; the mighty org mode
(use-package org
  :ensure org-contrib
  :demand
  :hook
  (org-mode . org/prettify-checkbox)
  (org-checkbox-statistics . org/checkbox-list-complete)
  (org-after-todo-statistics . org/org-summary-todo)
  (org-after-todo-state-change . org/remove-schedule-when-waiting)
  (org-tab-after-check-for-table . org-end-of-line)
  :bind
  ("C-c l" . org-store-link)
  (:map org-mode-map
        ("C-c 0" . org/schedule-today)
        ("C-c 1" . org/schedule-tomorrow))
  :custom
  (org-modules '())
  (org-directory "~/Documents/Org")
  (org-startup-folded t)
  (org-adapt-indentation t)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-hide-emphasis-markers t)
  (org-insert-heading-respect-content t)
  (org-tags-column 0)
  (org-auto-align-tags nil)
  (org-special-ctrl-a/e t)
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts nil)
  (org-ellipsis "…")
  (org-log-into-drawer t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-enforce-todo-dependencies t)
  (org-archive-tag "archive")
  (org-archive-location (concat org-directory "/archive/%s::"))
  (org-archive-save-context-info '(time olpath itags))
  (org-link-email-description-format "%s")
  (org-id-link-to-org-use-id 'create-if-interactive)
  (org-default-notes-file (expand-file-name "notes.org" org-directory))
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                       (sequence "WAITING(w@/!)" "|" "SOMEDAY(o)" "CANCELED(c@)")))
  (org-todo-keyword-faces
   '(("NEXT" :foreground "#4682b4")
     ("DONE" :foreground "#2e8b57")
     ("TODO" :foreground "#ff5f59")
     ("WAITING" :foreground "#bc8f8f")
     ("SOMEDAY" :foreground "#8470ff")
     ("CANCELED" :foreground "#dda0dd")))
  :config
  (require '+org)
  (setopt org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs)))

(use-package org-indent
  :ensure nil
  :hook (org-mode . org-indent-mode))

(use-package org-refile
  :ensure nil
  :custom
  (org-outline-path-complete-in-steps nil)
  (org-refile-use-outline-path 'file)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-target-verify-function #'org/verify-refile-target)
  (org-refile-targets '(("proposals.org" :maxlevel . 1)
                        ("projects.org" :maxlevel . 2)
                        ("teaching.org" :maxlevel . 2)
                        ("tasks.org" :maxlevel . 9)))
  :config
  (advice-add 'org-refile :after
              (lambda (&rest _)
                (org-save-all-org-buffers))))

(use-package org-clock
  :ensure nil
  :custom
  (org-clock-persist-query-resume nil)
  (org-clock-auto-clock-resolution 'when-no-clock-is-running))

(use-package org-keys
  :ensure nil
  :custom
  (org-return-follows-link t)
  (org-use-speed-commands t))

(use-package org-src
  :ensure nil
  :custom
  (org-src-window-setup 'reorganize-frame)
  (org-confirm-babel-evaluate nil)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (latex . t)
     (plantuml . t)
     (python . t)
     (octave . t)
     ;; (jupyter . t)
     ))
  (add-to-list 'org-src-lang-modes '("ipython" . python)))

(use-package ox
  :ensure nil
  :custom
  (org-export-allow-bind-keywords t)
  (org-html-html5-fancy t)
  (org-html-doctype "html5")
  (org-latex-prefer-user-labels t)
  (org-latex-with-hyperref nil)
  (org-cite-csl-styles-dir "~/Documents/Papers/styles")
  :config
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))

(use-package org-agenda
  :ensure nil
  :bind ("C-x ." . org-agenda)
  (:map org-agenda-mode-map
        ("0" . org/agenda-schedule-today)
        ("1" . org/agenda-schedule-tomorrow))
  :hook (org-agenda-finalize . org/cleanup-replied-emails)
  :custom
  (org-agenda-files '("~/Documents/Org"))
  (org-agenda-include-diary nil)
  (org-agenda-window-setup 'only-window)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-dim-blocked-tasks 'invisible)
  (org-agenda-text-search-extra-files '(agenda-archives))
  (org-agenda-tag-filter-preset '("-archive"))
  (org-agenda-skip-deadline-prewarning-if-scheduled t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-stuck-projects '("+project+LEVEL=1/-SOMEDAY-DONE" ("NEXT" "TODO") ("info") ""))
  (org-agenda-hide-tags-regexp "project\\|calendar")
  (org-agenda-tags-todo-honor-ignore-options t)
  (org-agenda-tags-column 'auto)
  (org-agenda-time-grid '((daily today require-timed remove-match)
                          (900 1000 1100 1200 1300 1400 1500 1600 1700 1800)
                          "......"
                          "----------------"))
  (org-agenda-custom-commands
   '(("w" "Weekly schedule"
      ((agenda "" ((org-agenda-overriding-header "")
                   (org-agenda-start-on-weekday nil)
                   (org-agenda-start-day (org-read-date nil nil "++1" nil (org-read-date nil t "-sun")))
                   (org-agenda-span 7)
                   (org-agenda-time-grid nil)
                   (org-agenda-files `(,(expand-file-name "calendar.org" org-directory)))))))
     ("g" "Get Things Done"
      ((agenda "" ((org-agenda-span 'day)
                   (org-deadline-warning-days 0)
                   (org-agenda-skip-function
                    '(org/agenda-skip-without-match "-email"))))
       (tags-todo "+email" ((org-agenda-overriding-header "Emails")))
       (todo "NEXT" ((org-agenda-overriding-header "Next/Active Tasks")
                     (org-agenda-skip-function #'(lambda () (org/agenda-maybe-skip-if-today nil)))
                     (org-agenda-sorting-strategy '(deadline-up scheduled-up priority-down category-up))))
       (tags-todo "-email-TODO=\"NEXT\"" ((org-agenda-overriding-header "Due Soon")
                                          (org-agenda-skip-function
                                           #'(lambda () (or (org-agenda-skip-entry-if 'notdeadline)
                                                            (org/agenda-maybe-skip-if-today nil))))
                                          (org-agenda-prefix-format '((tags . " %-8:c %(org/deadline-days) ")))
                                          (org-agenda-sorting-strategy '(deadline-up))
                                          (org-agenda-todo-ignore-deadlines 'far)
                                          (org-deadline-warning-days 15))))))))

(use-package org-capture
  :ensure nil
  :after org
  :demand
  :bind ("C-x c" . org-capture)
  :custom
  (org-capture-templates
   `(("r" "respond" entry
      (file ,(expand-file-name "email.org" org-directory))
      "* TODO %a %? \nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))" :immediate-finish t)
     ("t" "todo" entry
      (file ,(expand-file-name "inbox.org" org-directory))
      "* TODO %?\n%a\n")
     ("n" "note" entry
      (file ,(expand-file-name "notes.org" org-directory))
      "* %? :note:\n%U\n%a\n")
     ("p" "project todo" plain
      (file+function ,(expand-file-name "projects.org" org-directory)
                     (lambda () (goto-char (point-min)) (search-forward (denote-retrieve-title-value (buffer-file-name (org-capture-get :original-buffer)) 'org)) (forward-line)))
      "** TODO %?\n")
     ("j" "jhm" entry
      (file ,(expand-file-name "jhm.org" org-directory))
      (file ,(expand-file-name "templates/jhm.org" org-directory)))))
  :hook (org-capture-after-finalize . org/kill-calendar))

(use-package org-habit
  :ensure nil
  :custom
  (org-habit-graph-column 80)
  (org-habit-scheduled-past-days 7))

;; modernize org-mode
(use-package org-modern
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)
  :custom
  (org-modern-todo-faces '(("WAITING" :background "#bc8f8f" :foreground "black")
                           ("TODO" :background "#ff5f59" :foreground "white")
                           ("DONE" :background "#2e8b57" :foreground "white")
                           ("NEXT" :background "#4682b4" :foreground "white")
                           ("SOMEDAY" :background "#8470ff" :foreground "white")
                           ("CANCELED" :background "#dda0dd" :foreground "black")))
  (org-modern-star 'replace)
  (org-modern-checkbox nil)
  ;; (org-modern-checkbox '((88 . "󰄲") (45 . "󰡖") (32 . "󰄱")))
  :config
  (add-to-list 'org-modern-tag-faces '("someday" . (:background "#8470ff" :foreground "white"))))

;; toggle visibility of org elements
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-inside-latex nil))

;; automatically toggle LaTeX fragments in org mode
(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

(use-package org-preview
  :elpaca (org-preview :host github :repo "karthink/org-preview")
  :hook (org-mode . org-preview-mode))

;; pomodoro for org
(use-package org-pomodoro
  :bind
  (:map org-mode-map ("C-c C-x m" . org-pomodoro))
  (:map org-agenda-mode-map ("M" . org-pomodoro)))

;; org dependencies and actions
(use-package org-edna
  :hook (org-mode . org-edna-mode))

;; time blocking with org
(use-package org-timeblock
  :commands org-timeblock
  :custom
  (org-timeblock-files '("~/Documents/Org/calendar.org"
                         "~/Documents/Org/tasks.org"
                         "~/Documents/Org/projects.org"
                         "~/Documents/Org/inbox.org"
                         "~/Documents/Org/proposals.org"
                         "~/Documents/Org/teaching.org")))

;; exporting from org-mode
(use-package org-re-reveal
  :after org
  :demand
  :custom
  (org-re-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
  (org-re-reveal-revealjs-version "4"))

(use-package htmlize)

(use-package ox-ipynb
  :elpaca (ox-ipynb :host github :repo "jkitchin/ox-ipynb")
  :after org
  :demand)

;; calendars
(use-package khalel
  :commands (khalel-run-vdirsyncer khalel-import-events)
  :demand
  :after org-capture
  :custom
  (khalel-import-org-file (concat org-directory "/calendar.org"))
  (khalel-import-end-date "+30d")
  (khalel-import-org-file-confirm-overwrite nil)
  (khalel-default-calendar "calendar")
  :config
  (khalel-add-capture-template))


;; sending emails
(use-package message
  :ensure nil
  :custom
  (message-kill-buffer-on-exit t)
  (smtpmail-smtp-user "kandread@umass.edu")
  (message-send-mail-function #'message-send-mail-with-sendmail)
  (sendmail-program "msmtp")
  (smtpmail-queue-dir "~/Mail/queued")
  (message-sendmail-envelope-from nil)
  (message-cite-style 'message-cite-style-outlook)
  (message-make-forward-subject-function #'message-forward-subject-fwd)
  ;; (message-signature nil)
  (message-signature (concat "Kostas Andreadis (he/him)\n"
                             "Assistant Professor, UMass Amherst\n"
                             "https://hydro-umass.github.io"))
  )

;; notmuch email
(use-package notmuch
  :disabled
  :bind ("C-x m" . notmuch)
  (:map notmuch-search-mode-map
        ("d" . notmuch/search-delete)
        ("@" . notmuch/search-spam)
        ("c z" . notmuch/search-thread-in-tree))
  (:map notmuch-tree-mode-map
        ("d" . notmuch/tree-delete)
        ("@" . notmuch/tree-spam))
  (:map notmuch-show-mode-map
        ("d" . notmuch/show-delete)
        ("@" . notmuch/show-spam)
        ("W" . notmuch-show-save-attachments)
        ("o" . notmuch/show-view-attachment)
        ("W" . notmuch-show-save-attachments)
        ("w" . notmuch/show-save-attachment))
  :custom
  (notmuch-show-logo nil)
  (mail-user-agent 'notmuch-user-agent)
  (notmuch-hello-sections '(notmuch-hello-insert-header
                            notmuch-hello-insert-saved-searches
                            notmuch-hello-insert-search
                            notmuch-hello-insert-alltags
                            notmuch-hello-insert-footer))
  (notmuch-draft-folder "umass/Drafts")
  (notmuch-fcc-dirs "umass/Sent")
  (notmuch-multipart/alternative-discouraged '("text/plain" "text/html"))
  (mm-default-directory "~/Downloads/")
  :config
  (require '+notmuch)
  (add-hook 'notmuch-message-mode-hook #'notmuch/setup-message-mode)
  (add-to-list 'notmuch-saved-searches '(:name "today" :query "date:today" :key "y")))

(use-package ol-notmuch
  :disabled
  :after notmuch)

(use-package  messages-are-flowing
  :disabled
  :hook (notmuch-message-mode . messages-are-flowing-use-and-mark-hard-newlines)
  :custom (fill-flowed-encode-column 998))

(use-package org-msg
  :bind (:map org-msg-edit-mode-map
              ("C-c f c" . message-goto-cc)
              ("C-c f b" . message-goto-bcc)
              ("C-c f t" . message-goto-to)
              ("C-c f f" . message-goto-from)
              ("C-c f s" . message-goto-subject))
  :config
  (defun org-msg/kill-ascii-export-buffer-a ()
    "Kill redundant Org ASCII export buffer after sending message."
    (with-current-buffer "*Org ASCII Export*" (kill-buffer-and-window)))
  (advice-add 'org-msg-ctrl-c-ctrl-c :after #'org-msg/kill-ascii-export-buffer-a)
  ;; (defun email/org-msg-notmuch-tag-replied ()
  ;;   "Tag messages as replied when using org-msg."
  ;;   (when (eq major-mode 'org-msg-edit-mode)
  ;;     (if-let* ((in-reply-to (org-msg-message-fetch-field "in-reply-to"))
  ;;               (id (and (string-match "<\\(.+\\)>" in-reply-to) (match-string 1 in-reply-to))))
  ;;         (notmuch-tag (notmuch-id-to-query id) notmuch-message-replied-tags))))
  ;; (add-hook 'org-ctrl-c-ctrl-c-hook #'email/org-msg-notmuch-tag-replied)
  :custom
  (org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t")
  (org-msg-startup "hidestars indent inlineimages")
  (org-msg-greeting-fmt "\nHi%s,\n\n")
  (org-msg-greeting-name-limit 1)
  (org-msg-default-alternatives '((new . (text html))
                                  (reply-to-html . (text html))
                                  (reply-to-text . (text))))
  (org-msg-convert-citation t)
  (org-msg-recipient-names '(("kandread@umass.edu" . "Kostas")
                             ("cdguzman@umass.edu" . "Christian")
                             ("rpalmer@engin.umass.edu" . "Rick")
                             ("tlanghorst@umass.edu" . "Ted")
                             ("coss.31@osu.edu" . "Steve")
                             ("tminear@colorado.edu" . "Toby")
                             ("o.wing@fathom.global". "Ollie")
                             ("durand.8@osu.edu" . "Mike")
                             ("ksouth@umass.edu" . "Katie")
                             ("frederick.s.policelli@nasa.gov" . "Fritz")
                             ("arhollan@umass.edu" . "Addie Rose")))
  (org-msg-signature "
Best,

Kostas

#+begin_signature
--
Kostas Andreadis (he/him)
Assistant Professor, UMass Amherst
https://hydro-umass.github.io
#+end_signature"))

;; notes
(use-package denote
  :bind
  ("C-x C-\\" . set-goal-column)
  ("C-x C-n" . denote)
  :custom
  (denote-directory (expand-file-name "~/Documents/Notes"))
  (denote-known-keywords '("project" "literature" "fleeting" "slipbox"))
  (denote-infer-keywords nil)
  (denote-sort-keywords t)
  (denote-prompts '(keyword title))
  (denote-date-prompt-use-org-read-date t)
  (denote-allow-multi-word-keywords nil)
  (denote-rename-no-confirm t)
  (denote-file-name-slug-functions '((title . denote-sluggify-title)
                                     (signature . denote-sluggify-signature)
                                     (keyword . identity)))
  (denote-link-fontify-backlinks t)
  (denote-backlinks-show-context t)
  (denote-dired-directories (list denote-directory))
  (denote-journal-extras-title-format 'day-date-month-year)
  (denote-templates `((journal . ,(concat "* Daily Goals\n"
                                          "- [ ] \n"
                                          "* Daybook\n"
                                          "** Notes\n\n"
                                          "** Tasks\n"
                                          "#+begin: my-org-ql :query (and (todo) (planning :to today) (not (tags \"email\"))) :scope \"org-agenda-files\"\n\n#+end:\n"
                                          "* Time-blocker\n"
                                          "** Morning\n\n"
                                          "** Afternoon\n\n")))))

(use-package consult-notes
  :after consult
  :bind (:map consult-map ("n" . consult-notes))
  :config
  (consult-notes-denote-mode))

;; view PDF documents
(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :hook (pdf-view-mode . auto-revert-mode)
  :custom
  (pdf-view-display-size 'fit-page)
  (doc-view-pdf->png-converter-function 'ignore)
  :config
  (pdf-tools-install)
  :init
  (defun pdf/suppress-large-files-prompt-a (fn size op-type filename &optional offer-raw)
    "Silence File *.pdf is large (X MiB), really open? prompts for pdfs"
    (unless (string-match-p "\\.pdf\\'" filename)
      (funcall fn size op-type filename offer-raw)))
  (advice-add 'abort-if-file-too-large :around #'pdf/suppress-large-files-prompt-a))

;; LaTeX documents
(use-package tex
  :elpaca (auctex :repo "https://git.savannah.gnu.org/git/auctex.git" :branch "main"
                  :pre-build (("make" "elpa"))
                  :build (:not elpaca--compile-info) ;; Make will take care of this step
                  :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
                  :version (lambda (_) (require 'tex-site) AUCTeX-version))
  :custom
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (TeX-source-correlate-start-server t)
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-method (quote synctex))
  ;; (TeX-engine 'luatex)
  (TeX-electric-sub-and-superscript t)
  (TeX-master t)
  (TeX-save-query nil)
  (TeX-parse-self t)
  (TeX-auto-save t)
  (TeX-command-extra-options "--synctex=1")
  (reftex-plug-into-AUCTeX t)
  (LaTeX-beamer-item-overlay-flag nil)
  :hook (TeX-after-compilation-finished-functions . TeX-revert-document-buffer))

(use-package auctex-latexmk
  :custom (auctex-latexmk-inherit-TeX-PDF-mode t)
  :hook (LaTeX-mode . auctex-latexmk-setup))

(use-package cdlatex)

;; manage bibliography
(use-package bibtex
  :ensure nil
  :custom
  (bibtex-autokey-year-length 4)
  (bibtex-autokey-name-year-separator "")
  (bibtex-autokey-year-title-separator "")
  (bibtex-autokey-titleword-separator "")
  (bibtex-autokey-titlewords 3)
  (bibtex-autokey-titlewords-stretch 2)
  (bibtex-autokey-titleword-length nil)
  (bibtex-autokey-titleword-case-convert-function #'capitalize)
  (bibtex-align-at-equal-sign t))

(use-package citar
  :hook ((LaTeX-mode org-mode) . citar-capf-setup)
  :custom
  (citar-bibliography '("~/Documents/Papers/library.bib"))
  (org-cite-global-bibliography '("~/Documents/Papers/library.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-symbol-separator "  ")
  :config
  (if (featurep 'nerd-icons)
      (progn
        (defvar citar-indicator-files-icons
          (citar-indicator-create
           :symbol (nerd-icons-faicon "nf-fa-file_o" :face 'nerd-icons-green :v-adjust -0.1)
           :function #'citar-has-files
           :padding "  "
           :tag "has:files"))
        (defvar citar-indicator-links-icons
          (citar-indicator-create
           :symbol (nerd-icons-faicon "nf-fa-link" :face 'nerd-icons-orange :v-adjust 0.01)
           :function #'citar-has-links
           :padding "  "
           :tag "has:links"))
        (defvar citar-indicator-notes-icons
          (citar-indicator-create
           :symbol (nerd-icons-codicon "nf-cod-note" :face 'nerd-icons-blue :v-adjust -0.3)
           :function #'citar-has-notes
           :padding "    "
           :tag "has:notes"))
        (defvar citar-indicator-cited-icons
          (citar-indicator-create
           :symbol (nerd-icons-faicon "nf-fa-circle_o" :face 'nerd-icon-green)
           :function #'citar-is-cited
           :padding "  "
           :tag "is:cited"))
        (setq citar-indicators
              (list citar-indicator-files-icons
                    citar-indicator-links-icons
                    citar-indicator-notes-icons
                    citar-indicator-cited-icons))))
  :bind
  (:map org-mode-map :package org ("C-c b" . org-cite-insert))
  (:map LaTeX-mode-map :package latex ("C-c b" . citar-insert-citation)))

(use-package citar-embark
  :after (citar embark)
  :demand
  :config
  (citar-embark-mode))

(use-package citar-denote
  :after (citar denote)
  :custom
  (citar-denote-title-format nil)
  (citar-denote-keyword "literature"))

;; take notes on pdfs
(use-package org-noter
  :commands org-noter
  :custom
  (org-noter-always-create-frame nil))

;; markdown
(use-package markdown-mode
  :custom
  (markdown-enable-math t)
  (markdown-enable-wiki-links t)
  (markdown-italic-underscore t)
  (markdown-asymmetric-header t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-gfm-additional-languages '("sh" "julia"))
  (markdown-make-gfm-checkboxes-buttons t)
  (markdown-nested-imenu-heading-index (not (ignore-errors (native-comp-available-p)))))

;; images
(use-package image-mode
  :ensure nil
  :custom (image-auto-resize 'fit-window))

;; terminals
(use-package vterm
  :custom
  (vterm-environment '("LD_LIBRARY_PATH=/run/opengl-driver/lib"))
  (vterm-shell "zsh"))

;; remote connections
(use-package tramp
  :ensure nil
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; load local environments
(use-package envrc
  :hook (elpaca-after-init . envrc-global-mode)
  :custom (envrc-none-lighter ""))

;; julia
(use-package julia-mode)

(use-package eglot-jl
  :hook (julia-mode . eglot-jl-init)
  :custom (eglot-connect-timeout 300))

(use-package julia-repl
  :hook (julia-mode . julia-repl-mode)
  :config
  (julia-repl-set-terminal-backend 'vterm))

;; Jupyter notebooks
(use-package jupyter
  :ensure nil)

(use-package code-cells
  :bind (:map code-cells-mode-map
              ("M-n" . code-cells-forward-cell)
              ("M-p" . code-cells-backward-cell)
              ("C-c <down>" . code-cells-move-cell-down)
              ("C-c <up>" . code-cells-move-cell-up)
              ("M-<return>" . code-cells-eval)))

;; inherit buffer-local environment variables
(use-package inheritenv
  :elpaca (inheritenv :host github :repo "purcell/inheritenv")
  :demand
  :config
  (inheritenv-add-advice 'jupyter-run-repl))

;; language server
(use-package eglot-hierarchy
  :elpaca (eglot-hierarchy :host github :repo "dolmens/eglot-hierarchy"))

;; tab-bar
;; with customizations from https://christiantietze.de/posts/2022/02/emacs-tab-bar-numbered-tabs/
(use-package tab-bar
  :ensure nil
  :config
  (defvar tabs/circle-numbers-alist
    '((1 . "󰲠")
      (2 . "󰲢")
      (3 . "󰲤")
      (4 . "󰲦")
      (5 . "󰲨")
      (6 . "󰲪")
      (7 . "󰲬")
      (8 . "󰲮")
      (9 . "󰲰"))
    "Alist of integers to strings of circled unicode numbers.")
  (defun tabs/tab-bar-tab-name-format-default (tab i)
    (let ((current-p (eq (car tab) 'current-tab))
          (tab-num (if (and tab-bar-tab-hints (< i 10))
                       (alist-get i tabs/circle-numbers-alist) "")))
      (propertize
       (concat tab-num
               " "
               (alist-get 'name tab)
               (or (and tab-bar-close-button-show
                        (not (eq tab-bar-close-button-show
                                 (if current-p 'non-selected 'selected)))
                        tab-bar-close-button)
                   "")
               " ")
       'face (funcall tab-bar-tab-face-function tab))))
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-select-tab-modifiers '(meta control))
  (tab-bar-tab-hints t)
  (tab-bar-tab-name-format-function #'tabs/tab-bar-tab-name-format-default)
  (tab-bar-show 1)
  (tab-bar-new-tab-choice "*scratch*"))

;; windows/tab history
(use-package tab-bar-history
  :ensure nil
  ;; :hook (elpaca-after-init . tab-bar-history-mode)
  :custom (tab-bar-format '(tab-bar-format-tabs tab-bar-separator tab-bar-format-add-tab)))

;; a butler for buffers
(use-package bufler
  :custom
  (bufler-reverse nil)
  (bufler-list-group-separators '((0 . "\n")))
  (bufler-initial-face-depth 1)
  (bufler-filter-buffer-modes '(bufler-list-mode
                                special-mode
                                elpaca-log-mode
                                messages-buffer-mode
                                emacs-lisp-compilation-mode
                                fundamental-mode))
  (bufler-groups (bufler-defgroups
                   (group (group-or "Mail"
                                    (mode-match "*notmuch*" (rx bos "notmuch-"))))
                   (auto-mode)
                   (auto-directory))))

;; hide minor modes
(use-package minions
  :hook (elpaca-after-init . minions-mode)
  :custom
  (minions-prominent-modes '(auto-revert-mode envrc-mode flymake-mode)))

;; query org files
(use-package org-ql
  :custom
  (org-ql-ask-unsafe-queries nil)
  :config
  (require '+org-ql))

;; get environment variables from shell
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :defer 2
  :custom
  (exec-path-from-shell-arguments nil)
  :config
  (exec-path-from-shell-initialize))

;; zap to char using avy
(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

;; notmuch queries with consult
(use-package consult-notmuch
  :disabled
  :bind (:map consult-map
              ("m m" . consult-notmuch)
              ("m a" . consult-notmuch-address)))

;; show fonts
(use-package show-font)

;; isolate buffer per frame
(use-package beframe
  :custom
  (beframe-global-buffers '("*scratch*" "*Messages*"))
  (beframe-create-frame-scratch-buffer nil)
  :hook (elpaca-after-init . beframe-mode))

;; frame and tab local buffers
;; (use-package bufferlo
;;   :hook (elpaca-after-init . bufferlo-mode)
;;   :bind
;;   ([remap switch-to-buffer] . bufferlo-switch-to-buffer))

;; LLM with Emacs
(use-package gptel
  :commands gptel
  :config
  (setq
   gptel-model 'incept5/llama3.1-claude:latest
   gptel-backend (gptel-make-ollama "Ollama"
                   :host "localhost:11434"
                   :stream t
                   :models '(incept5/llama3.1-claude:latest
                             deepseek-coder:latest
                             codellama:latest))))

;; mu4e for emails
(use-package mu4e
  :ensure nil
  :bind ("C-x m" . mu4e)
  ;; :commands mu4e
  :bind
  (:map mu4e-main-mode-map ("u" . mu4e-update-index))
  (:map mu4e-headers-mode-map
        ("@" . mu4e-headers-mark-for-spam)
        ("i" . mu4e-headers-mark-for-refile))
  (:map mu4e-compose-minor-mode-map
        ("r" . mu4e-compose-reply)
        ("R" . mu4e-compose-wide-reply))
  (:map mu4e-view-mode-map
        ("i" . mu4e-view-mark-for-refile))
  :custom
  (mu4e-spam-folder "/umass/Junk")
  (mu4e-attachment-dir "~/Downloads/")
  (mu4e-update-interval nil)
  (mu4e-hide-index-messages t)
  (mu4e-context-policy 'pick-first)
  (mu4e-compose-context-policy 'ask-if-none)
  (mu4e-compose-format-flowed t)
  (fill-flowed-encode-column 998)
  (mu4e-change-filenames-when-moving t)
  (mu4e-completing-read-function #'completing-read)
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-confirm-quit nil)
  (mu4e-sent-folder "/umass/Sent")
  (mu4e-drafts-folder "/umass/Drafts")
  (mu4e-trash-folder "/umass/Trash")
  (mu4e-search-sort-direction 'ascending)
  (mu4e-search-include-related nil)
  (mu4e-headers-thread-single-orphan-prefix '("─>" . "─▶"))
  (mu4e-headers-thread-orphan-prefix        '("┬>" . "┬▶ "))
  (mu4e-headers-thread-connection-prefix    '("│ " . "│ "))
  (mu4e-headers-thread-first-child-prefix   '("├>" . "├▶"))
  (mu4e-headers-thread-child-prefix         '("├>" . "├▶"))
  (mu4e-headers-thread-last-child-prefix    '("└>" . "╰▶"))
  (mail-user-agent 'mu4e-user-agent)
  :config
  (require 'mu4e-icalendar)
  (mu4e-icalendar-setup)
  ;; automatically apply marks before refreshing search
  (defun mu4e/mark-execute-all-no-confirm-a ()
    "Execute mu4e marks with no confirmation"
    (when (> (mu4e-mark-marks-num) 0)
      (mu4e-mark-execute-all t)))
  (advice-add 'mu4e-search-rerun :before #'mu4e/mark-execute-all-no-confirm-a)
  ;; handle spam messages
  ;; (defvar mu4e-spam-folder "/junk" "Folder for spam messages, relative to the root maildir.")
  (add-to-list 'mu4e-marks
               '(spam
                 :char       "S"
                 :prompt     "Spam"
                 :show-target (lambda (target) mu4e-spam-folder)
                 :action      (lambda (docid msg target)
                                (mu4e--server-move docid mu4e-spam-folder "+S-u-N"))))
  (mu4e~headers-defun-mark-for spam))

(use-package mu4e-column-faces
  :hook (mu4e-main-mode . mu4e-column-faces-mode))

(use-package consult-mu
  :elpaca (consult-mu :host github :repo "armindarvish/consult-mu" :files (:defaults "extras/*.el"))
  :after (mu4e consult))

(use-package mu4e-thread-folding
  :elpaca (mu4e-thread-folding :host github :repo "rougier/mu4e-thread-folding")
  :after mu4e)

;; try lsp-bridge
(use-package lsp-bridge
  :ensure nil)

;; load theme
(add-hook 'elpaca-after-init-hook (lambda () (load-theme 'ef-owl t)))

;;; config.el ends here
