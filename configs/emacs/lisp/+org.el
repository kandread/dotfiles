;;; +org.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Additional functions for Org-mode

;;; Code:

(defun org/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(defun org/checkbox-list-complete ()
  "Change TODO state when checkboxes are complete."
  (let ((todo-state (org-get-todo-state)) beg end)
    (unless (not todo-state)
      (save-excursion
        (org-back-to-heading t)
        (setq beg (point))
        (end-of-line)
        (setq end (point))
        (goto-char beg)
        (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                               end t)
            (if (match-end 1)
                (if (equal (match-string 1) "100%")
                    (unless (string-equal todo-state "DONE")
                      (org-todo "DONE"))
                  (unless (string-equal todo-state todo-state)
                    (org-todo todo-state)))
              (if (and (> (match-end 2) (match-beginning 2))
                       (equal (match-string 2) (match-string 3)))
                  (unless (string-equal todo-state "DONE")
                    (org-todo "DONE"))
                (unless (string-equal todo-state todo-state)
                  (org-todo todo-state)))))))))

(defun org/org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-todo-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(defun org/remove-schedule-when-waiting ()
  "Remove scheduled property when state is WAITING."
  (save-excursion
    (org-back-to-heading)
    (if (equal (org-get-todo-state) "WAITING")
        (let ((current-prefix-arg '(4)))
          (call-interactively #'org-schedule)))))

(defun org/email-replied-p ()
  "Return nil if message at heading has not been replied to."
  (interactive)
  (let* ((heading (nth 4 (org-heading-components)))
         (id (and (string-match "mu4e:\\(msgid:[[:alnum:][:punct:]]+\\)\\]\[[[:alnum:][:space:][:punct:]]+\]" heading) (match-string 1 heading)))
         (replied? (call-process "mu" nil nil nil "find" (concat id " and flag:replied"))))
    (= replied? 0)))
;; (defun org/email-replied-p ()
;;   "Return nil if message at heading has not been replied to."
;;   (interactive)
;;   (when (featurep 'notmuch)
;;     (let* ((heading (nth 4 (org-heading-components)))
;;         (id (and (string-match "notmuch:\\(id:[[:alnum:][:punct:]]+\\)\\]\[[[:alnum:][:space:][:punct:]]+\]" heading) (match-string 1 heading)))
;;         (tags (format "%s" (notmuch-call-notmuch-sexp "search" "--output=tags" "--format=sexp" id))))
;;       (string-match-p (regexp-quote "replied") tags))))

(defun org/delete-all-done-entries ()
  "Delete all entries marked DONE."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (while (outline-previous-heading)
      (when (or (org-entry-is-done-p) (org/email-replied-p))
        (org-cut-subtree)))))

(defun org/cleanup-replied-emails ()
  "Clean up email tasks that have been replied (i.e. marked done)."
  (interactive)
  (org-map-entries #'org/delete-all-done-entries "email" (list (expand-file-name "email.org" org-directory)))
  (save-some-buffers 'no-confirm (equal buffer-file-name "email.org")))

(defun org/kill-calendar ()
  "Kill *Calendar* buffer if open."
  (when (get-buffer "*Calendar*")
    (kill-buffer "*Calendar*")))

(defun org/match-at-point-p (match)
  "Return non-nil if headline at point matches MATCH.
Here MATCH is a match string of the same format used by
`org-tags-view'."
  (funcall (cdr (org-make-tags-matcher match))
           (org-get-todo-state)
           (org-get-tags-at)
           (org-reduced-level (org-current-level))))

(defun org/agenda-skip-without-match (match)
  "Skip current headline unless it matches MATCH.

Return nil if headline containing point matches MATCH (which
should be a match string of the same format used by
`org-tags-view').  If headline does not match, return the
position of the next headline in current buffer.

Intended for use with `org-agenda-skip-function', where this will
skip exactly those headlines that do not match."
  (save-excursion
    (unless (org-at-heading-p) (org-back-to-heading))
    (let ((next-headline (save-excursion
                           (or (outline-next-heading) (point-max)))))
      (if (org/match-at-point-p match) nil next-headline))))

(defun org/agenda-maybe-skip-if-today (skip-today)
  "If skip-today is t, skip entries that are not scheduled or do not have a deadline today. Otherwise, skip entries that are scheduled or have a deadline in the future."
  (ignore-errors
    (let* ((scheduled (org-entry-get nil "SCHEDULED"))
           (deadline (org-entry-get nil "DEADLINE"))
           (now (time-to-days (current-time)))
           (today (or (and deadline (<= (time-to-days (org-time-string-to-time deadline)) now))
                      (and scheduled (<= (time-to-days (org-time-string-to-time scheduled)) now))))
           (skip? (if skip-today today (not today)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if skip?
          nil
        next-headline))))

(defun org/agenda-schedule-today ()
  "Schedule entry at point for today."
  (org-agenda-schedule 1 "0d"))

(defun org/agenda-schedule-tomorrow ()
  "Schedule entry at point for tomorrow"
  (org-agenda-schedule 1 "+1d"))

(defun org/schedule-today ()
  "Schedule entry at point for today."
  (org-schedule 1 "0d"))

(defun org/schedule-tomorrow ()
  "Schedule entry at point for tomorrow"
  (org-schedule 1 "+1d"))

(defun jhm/manuscript-id ()
  "Extract manuscript id from stored link."
  (let ((link (plist-get org-store-link-plist :annotation)))
    (if (string-match "\\(JHM-D-[0-9]+-[0-9]+\\)" link)
        (match-string 1 link) "")))

(defun jhm/replace-headers ()
  "Replace to address when replying to JHM emails."
  (when (string-match "andreadis.jhm@ametsoc.org" (message-fetch-field "To"))
    (message-replace-header "To" "Hayley Charney <hcharney@ametsoc.org>")
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "Hi Konstantinos" nil t)
        (replace-match "Hi Hayley")))))

(defun org/archive-done-tasks ()
  "Archive all done entries in the file."
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'file))

(defun org/agenda-delete-empty-blocks ()
  "Remove empty agenda blocks.
  A block is identified as empty if there are fewer than 2
  non-empty lines in the block (excluding the line with
  `org-agenda-block-separator' characters)."
  (when org-agenda-compact-blocks
    (user-error "Cannot delete empty compact blocks"))
  (setq buffer-read-only nil)
  (save-excursion
    (goto-char (point-min))
    (let* ((blank-line-re "^\\s-*$")
           (content-line-count (if (looking-at-p blank-line-re) 0 1))
           (start-pos (point))
           (block-re (format "%c\\{10,\\}" org-agenda-block-separator)))
      (while (and (not (eobp)) (forward-line))
        (cond
         ((looking-at-p block-re)
          (when (< content-line-count 2)
            (delete-region start-pos (1+ (point-at-bol))))
          (setq start-pos (point))
          (forward-line)
          (setq content-line-count (if (looking-at-p blank-line-re) 0 1)))
         ((not (looking-at-p blank-line-re))
          (setq content-line-count (1+ content-line-count)))))
      (when (< content-line-count 2)
        (delete-region start-pos (point-max)))
      (goto-char (point-min))
      ;; The above strategy can leave a separator line at the beginning
      ;; of the buffer.
      (when (looking-at-p block-re)
        (delete-region (point) (1+ (point-at-eol))))))
  (setq buffer-read-only t))

(defun org/deadline-days ()
  "Return date string for deadline."
  (let* ((today (org-today))
         (deadline-entry (org-entry-get nil "DEADLINE"))
         (deadline (if deadline-entry (org-time-string-to-absolute deadline-entry) nil)))
    (if deadline
        (propertize (format "In %d d." (- deadline today)) 'face 'org-imminent-deadline))))

(defun org/prettify-checkbox ()
  "Replace checkboxes with glyphs."
  (push '("[ ]" .  "🞎") prettify-symbols-alist)
  (push '("[X]" . "🗷" ) prettify-symbols-alist)
  (push '("[-]" . "◫" ) prettify-symbols-alist)
  (prettify-symbols-mode))


(provide '+org)

;;; +org.el ends here
