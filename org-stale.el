;;; org-stale.el --- Archive stale Org entries -*- lexical-binding: t; -*-

;; Meant to be run unattended via:
;;   emacs --batch -l org-stale.el
;; Loading this file has a side effect: the very last line calls
;; my/org-archive-stale-entries, which archives entries and saves buffers.
;; Don't load it inside a normal interactive session unless that's what you want.

(require 'org)
(setq org-agenda-files (list "/mnt/dietpi_userdata/syncthing/notes/org"))
(defvar org-archive-file "/mnt/dietpi_userdata/syncthing/notes/org/arkiv.org"
  "Path to the single file all stale entries are archived into.")

(defun my/is-time-past (time)
  "Non-nil if TIME's day is strictly before today."
  (< (time-to-days time) (time-to-days (current-time))))

(defun my/entry-has-past-plain-timestamp-p ()
  "Non-nil if the entry at point contains a plain timestamp before today.
Looks at bare timestamps in the entry body (e.g. from the \"Calendar
event\" capture template), not SCHEDULED/DEADLINE properties."
  (save-excursion
    ;; Both args to org-end-of-subtree must be t here: this jumps to the
    ;; start of the *next* heading (or end of buffer), which is always a
    ;; safe position at-or-after everything in this entry. With the second
    ;; arg left at its default, Org backs up over trailing blank lines,
    ;; which can land `end' earlier than where forward-line puts point
    ;; below, especially on short/empty entries, causing a
    ;; "wrong side of point" search error.
    (let ((end (save-excursion (org-end-of-subtree t t) (point))))
      (org-back-to-heading t)
      (forward-line 1) ; skip past the heading line itself
      (when (re-search-forward org-ts-regexp-both end t)
	(my/is-time-past (org-time-string-to-time (match-string 0)))))))

(defun my/get-stale-org-entry-with-reason ()
  "Examine the entry at point and return (REASON HEADING MARKER).
REASON is nil if the entry isn't stale, otherwise a short string
describing why (\"DONE\", \"overdue scheduled\", etc)."
  (let ((entry-todo-state (org-get-todo-state))
	(entry-scheduled-time (org-get-scheduled-time (point)))
	(entry-deadline-time (org-get-deadline-time (point)))
	(entry-marker (point-marker)))
    (let ((reason (cond
		   ((string= "DONE" entry-todo-state) "DONE")
		   ((and entry-scheduled-time (my/is-time-past entry-scheduled-time)) "overdue scheduled")
		   ((and entry-deadline-time (my/is-time-past entry-deadline-time)) "overdue deadline")
		   ;; Only check for a bare past timestamp on entries with no
		   ;; TODO state at all, i.e. plain calendar-style events.
		   ;; Otherwise a TODO whose body happens to mention some
		   ;; unrelated past date would get wrongly flagged.
		   ((and (not entry-todo-state) (my/entry-has-past-plain-timestamp-p)) "past event"))))
      (list reason (org-get-heading t t t t) entry-marker))))

(defun my/list-stale-org-entries ()
  "Return a list of (REASON HEADING MARKER) for every stale entry
across `org-agenda-files'."
  (let ((results '())
	;; Exclude the archive file itself from the scan. Without this,
	;; DONE entries already sitting in the archive get re-detected as
	;; stale on every run and re-archived into themselves, so the
	;; count never goes down (found via idempotency testing).
	(files (seq-remove (lambda (f) (file-equal-p f org-archive-file))
			   (org-agenda-files))))
    (dolist (file files)
      (setq results (append results (with-current-buffer (find-file-noselect file)
				      (seq-filter (lambda (entry) (car entry)) (org-map-entries (lambda () (my/get-stale-org-entry-with-reason))))))))
    results))

(defun my/org-archive-entry (marker)
  "Archive the entry at MARKER to `org-archive-file'."
  (org-with-point-at marker
    (let ((org-archive-location (concat org-archive-file "::")))
      (org-archive-subtree))))

(defun my/org-archive-stale-entries ()
  "Find and archive every stale entry across `org-agenda-files'.
Archives one entry at a time so a single malformed entry doesn't
abort the whole run; failures are logged with `message' and skipped.
Saves all modified buffers afterward."
  (let ((success-counter 0))
    (dolist (entry (my/list-stale-org-entries))
      (condition-case err
	  (progn
	    (my/org-archive-entry (caddr entry))
	    (cl-incf success-counter))
	(error (message "Error: %s %s" err (cadr entry)))))
    (message "Archived %d entries." success-counter)
    (save-some-buffers t)))

(my/org-archive-stale-entries)
