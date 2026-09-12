;;; org-stale.el --- Archive stale Org entries -*- lexical-binding: t; -*-

;; Meant to be run unattended via:
;;   emacs --batch -l org-stale.el [ORG-DIR]
;; ORG-DIR is optional and defaults to the Linux path below, so
;; on machines where the notes live somewhere else, e.g. on Windows:
;;   emacs.exe --batch -l org-stale.el "C:/Users/you/notes/org"
;; Loading this file has a side effect: the very last line calls
;; my/archive-stale-org-entries, which archives entries and saves buffers.
;; Don't load it inside a normal interactive session unless that's what you want.

(require 'org)
(defvar my/org-stale-directory
  (or (car command-line-args-left) "/mnt/dietpi_userdata/syncthing/notes/org")
  "Directory scanned for stale entries. See file header for how to override.")
(setq org-agenda-files (list my/org-stale-directory))
(defvar org-archive-file (expand-file-name "arkiv.org" my/org-stale-directory)
  "Path to the single file all stale entries are archived into.")
;; temporary-file-directory resolves correctly on both Linux (/tmp)
;; and Windows (%TEMP%), unlike a hardcoded /tmp.
(setq backup-directory-alist `(("." . ,temporary-file-directory)))

(defun my/is-time-older-than (time days)
  "Non-nil if TIME's day is older than DAYS days ago."
  (< (time-to-days time) (time-to-days (time-subtract (current-time) (days-to-time days)))))

(defun my/timestamp-end-time (timestamp-string)
  "Return the time value for the end of a range in TIMESTAMP-STRING,
or the time value for TIMESTAMP-STRING itself if it isn't a range.
A range looks like \"<2026-01-01>--<2026-01-05>\"; only the part
after -- is used, so a still-ongoing range isn't treated as past
just because its start date has passed."
  (if (string-match "--" timestamp-string)
      (org-time-string-to-time (substring timestamp-string (match-end 0)))
    (org-time-string-to-time timestamp-string)))

(defun my/org-entry-has-past-plain-timestamp-p ()
  "Non-nil if the entry at point contains a plain timestamp before today.
Looks at bare timestamps in the entry body (e.g. from the \"Calendar
event\" capture template), not SCHEDULED/DEADLINE properties. If the
timestamp is a range, only a range whose END has passed counts."
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
	(let* ((first-match (match-string 0))
	       ;; Check whether a "--<timestamp>" immediately follows,
	       ;; meaning this is a range, not a single date.
	       (full-text
		(if (looking-at (concat "--" org-ts-regexp-both))
		    (concat first-match (match-string 0))
		  first-match)))
	  (my/is-time-older-than (my/timestamp-end-time full-text) 0))))))

(defun my/get-stale-org-entry-with-reason ()
  "Examine the entry at point and return (REASON HEADING MARKER).
REASON is nil if the entry isn't stale, otherwise a short string
describing why (\"DONE\" or \"past event\")."
  (let* ((entry-todo-state (org-get-todo-state))
	 ;; Read SCHEDULED/DEADLINE as raw text rather than via
	 ;; org-get-scheduled-time/org-get-deadline-time, since those
	 ;; don't expose range information (they'd silently return just
	 ;; the start of a multi-day range).
	 (entry-scheduled-raw (org-entry-get (point) "SCHEDULED"))
	 (entry-deadline-raw (org-entry-get (point) "DEADLINE"))
	 (entry-scheduled-time (and entry-scheduled-raw (my/timestamp-end-time entry-scheduled-raw)))
	 (entry-deadline-time (and entry-deadline-raw (my/timestamp-end-time entry-deadline-raw)))
	 (entry-marker (point-marker)))
    (let ((reason (cond
		   ((and (string= "DONE" entry-todo-state)
			 (or (not entry-scheduled-time) (my/is-time-older-than entry-scheduled-time 0))
			 (or (not entry-deadline-time) (my/is-time-older-than entry-deadline-time 0)))
		    "DONE")
		   ;; Only check for a bare past timestamp on entries with no
		   ;; TODO state at all, i.e. plain calendar-style events.
		   ;; Otherwise a TODO whose body happens to mention some
		   ;; unrelated past date would get wrongly flagged.
		   ((and (not entry-todo-state) (my/org-entry-has-past-plain-timestamp-p)) "past event"))))
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

(defun my/archive-org-entry (marker)
  "Archive the entry at MARKER to `org-archive-file'."
  (org-with-point-at marker
    (let ((org-archive-location (concat org-archive-file "::")))
      (org-archive-subtree))))

(defun my/archive-stale-org-entries ()
  "Find and archive every stale entry across `org-agenda-files'.
Archives one entry at a time so a single malformed entry doesn't
abort the whole run; failures are logged with `message' and skipped.
Saves all modified buffers afterward."
  (let ((success-counter 0))
    (dolist (entry (my/list-stale-org-entries))
      (condition-case err
	  (progn
	    (my/archive-org-entry (caddr entry))
	    (cl-incf success-counter))
	(error (message "Error: %s %s" err (cadr entry)))))
    (message "Archived %d entries." success-counter)
    (save-some-buffers t)))

(defun my/archived-entry-too-old-p ()
  "Non-nil if the entry at point has an ARCHIVE_TIME older than 180 days."
  (let ((archive-time (org-entry-get (point) "ARCHIVE_TIME")))
    (and archive-time
	 (my/is-time-older-than (org-time-string-to-time archive-time) 180))))

(defun my/delete-org-entry (marker)
  "Delete the entry at MARKER."
  (org-with-point-at marker
    (org-cut-subtree)))

(defun my/clean-up-archive ()
  "Delete entries in `org-archive-location' that were archived more than six months ago."
  (let ((success-counter 0))
    (dolist (entry (seq-filter #'identity
			       (with-current-buffer (find-file-noselect org-archive-file)
				 (org-map-entries
				  (lambda ()
				    (when (my/archived-entry-too-old-p)
				      (point-marker)))))))
      (condition-case err
	  (progn
	    (my/delete-org-entry entry)
	    (cl-incf success-counter))
	(error (message "Error: %s %s" err (marker-buffer entry)))))
    (message "Deleted %d entries." success-counter)
    (save-some-buffers t)))

(my/archive-stale-org-entries)
(my/clean-up-archive)
