;;; lsdb.el --- the Lovely Sister Database

;; Copyright (C) 2002 Daiki Ueno

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Keywords: adress book

;; This file is part of the Lovely Sister Database.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; For Semi-gnus:
;;; (autoload 'lsdb-gnus-insinuate "lsdb")
;;; (autoload 'lsdb-gnus-insinuate-message "lsdb")
;;; (add-hook 'gnus-startup-hook 'lsdb-gnus-insinuate)
;;; (add-hook 'message-setup-hook
;;;           (lambda ()
;;;             (define-key message-mode-map "\M-\t" 'lsdb-complete-name)))

;;; For Wanderlust, put the following lines into your ~/.wl:
;;; (require 'lsdb)
;;; (lsdb-wl-insinuate)
;;; (add-hook 'wl-draft-mode-hook
;;;           (lambda ()
;;;             (define-key wl-draft-mode-map "\M-\t" 'lsdb-complete-name)))

;;; Code:

(require 'poem)
(require 'mime)

;;;_* USER CUSTOMIZATION VARIABLES:
(defgroup lsdb nil
  "The Lovely Sister Database."
  :group 'news
  :group 'mail)
  
(defcustom lsdb-file (expand-file-name "~/.lsdb")
  "The name of the Lovely Sister Database file."
  :group 'lsdb
  :type 'file)

(defcustom lsdb-file-coding-system 'iso-2022-jp
  "Coding system for `lsdb-file'."
  :group 'lsdb
  :type 'symbol)

(defcustom lsdb-sender-headers
  "From\\|Resent-From"
  "List of headers to search for senders."
  :group 'lsdb
  :type 'list)

(defcustom lsdb-recipients-headers
  "Resent-To\\|Resent-Cc\\|Reply-To\\|To\\|Cc\\|Bcc"
  "List of headers to search for recipients."
  :group 'lsdb
  :type 'list)

(defcustom lsdb-interesting-header-alist
  '(("Organization" nil organization)
    ("\\(X-\\)?User-Agent\\|X-Mailer" nil user-agent)
    ("\\(X-\\)?ML-Name" nil mailing-list)
    ("X-Attribution\\|X-cite-me" nil attribution))
  "Alist of headers we are interested in.
The format of elements of this list should be
     (FIELD-NAME REGEXP ENTRY STRING)
where the last three elements are optional."
  :group 'lsdb
  :type 'list)

(defcustom lsdb-entry-type-alist
  '((net 5 ?,)
    (creation-date 2)
    (last-modified 2)
    (mailing-list 3 ?,)
    (attribution 3 ?.)
    (organization 3)
    (score -1))
  "Alist of entries to display.
The format of elements of this list should be
     (ENTRY SCORE CLASS)
where the last element is optional."
  :group 'lsdb
  :type 'list)

(defcustom lsdb-decode-field-body-function #'lsdb-decode-field-body
  "Field body decoder."
  :group 'lsdb
  :type 'function)

(defcustom lsdb-canonicalize-full-name-function
  #'lsdb-canonicalize-spaces-and-dots
  "Way to canonicalize full name."
  :group 'lsdb
  :type 'function)

(defcustom lsdb-print-record-function
  #'lsdb-print-record
  "Function to print LSDB record."
  :group 'lsdb
  :type 'function)

(defcustom lsdb-window-max-height 7
  "Maximum number of lines used to display LSDB record."
  :group 'lsdb
  :type 'integer)

;;;_. Faces
(defface lsdb-header-face
  '((t (:underline t)))
  "Face for the file header line in `lsdb-mode'."
  :group 'lsdb)
(defvar lsdb-header-face 'lsdb-header-face)

(defface lsdb-field-name-face
  '((((class color) (background dark))
     (:foreground "PaleTurquoise" :bold t))
    (t (:bold t)))
  "Face for the message header line in `lsdb-mode'."
  :group 'lsdb)
(defvar lsdb-field-name-face 'lsdb-field-name-face)

(defface lsdb-field-body-face
  '((((class color) (background dark))
     (:foreground "turquoise" :italic t))
    (t (:italic t)))
  "Face for the message header line in `lsdb-mode'."
  :group 'lsdb)
(defvar lsdb-field-body-face 'lsdb-field-body-face)

(defconst lsdb-font-lock-keywords
  '(("^\\sw.*$"
     (0 lsdb-header-face))
    ("^\t\t.*$"
     (0 lsdb-field-body-face))
    ("^\t\\([^\t:]+:\\)[ \t]*\\(.*\\)$"
     (1 lsdb-field-name-face)
     (2 lsdb-field-body-face))))

(put 'lsdb-mode 'font-lock-defaults '(lsdb-font-lock-keywords t))

;;;_* CODE - no user customizations below
(defvar lsdb-hash-table nil
  "Internal hash table to hold LSDB records.")

(defvar lsdb-buffer-name "*LSDB*"
  "Buffer name to display LSDB record.")

(defvar lsdb-hash-table-is-dirty nil
  "Flag to indicate whether the hash table needs to be saved.")

;;;_. Hash Table Emulation
(if (fboundp 'make-hash-table)
    (progn
      (defalias 'lsdb-puthash 'puthash)
      (defalias 'lsdb-gethash 'gethash)
      (defalias 'lsdb-remhash 'remhash)
      (defalias 'lsdb-maphash 'maphash)
      (defalias 'lsdb-hash-table-size 'hash-table-size)
      (defalias 'lsdb-hash-table-count 'hash-table-count)
      (defalias 'lsdb-make-hash-table 'make-hash-table))
  (defun lsdb-puthash (key value hash-table)
    "Hash KEY to VALUE in HASH-TABLE."
    ;; Obarray is regarded as an open hash table, as a matter of
    ;; fact, rehashing doesn't make sense.
    (let (new-obarray)
      (when (> (car hash-table)
	       (* (length (nth 1 hash-table)) 0.7))
	(setq new-obarray (make-vector (* (length (nth 1 hash-table)) 2) 0))
	(mapatoms
	 (lambda (symbol)
	   (set (intern (symbol-name symbol) new-obarray)
		(symbol-value symbol)))
	 (nth 1 hash-table))
	(setcdr hash-table (list new-obarray)))
      (set (intern key (nth 1 hash-table)) value)
      (setcar hash-table (1+ (car hash-table)))))
  (defun lsdb-gethash (key hash-table &optional default)
    "Find hash value for KEY in HASH-TABLE.
If there is no corresponding value, return DEFAULT (which defaults to nil)."
    (let ((symbol (intern-soft key (nth 1 hash-table))))
      (if symbol
	  (symbol-value symbol)
	default)))
  (defun lsdb-remhash (key hash-table)
    "Remove the entry for KEY from HASH-TABLE.
Do nothing if there is no entry for KEY in HASH-TABLE."
    (unintern key (nth 1 hash-table))
    (setcar hash-table (1- (car hash-table))))
  (defun lsdb-maphash (function hash-table)
    "Map FUNCTION over entries in HASH-TABLE, calling it with two args,
each key and value in HASH-TABLE.

FUNCTION may not modify HASH-TABLE, with the one exception that FUNCTION
may remhash or puthash the entry currently being processed by FUNCTION."
    (mapatoms
     (lambda (symbol)
       (funcall function (symbol-name symbol) (symbol-value symbol)))
     hash-table))
  (defun lsdb-hash-table-size (hash-table)
    "Return the size of HASH-TABLE.
This is the current number of slots in HASH-TABLE, whether occupied or not."
    (length (nth 1 hash-table)))
  (defalias 'lsdb-hash-table-count 'car)
  (defun lsdb-make-hash-table (&rest args)
    "Return a new empty hash table object."
    (list 0 (make-vector (or (plist-get args :size) 29) 0))))

;;;_. Hash Table Reader/Writer
(eval-and-compile
  (condition-case nil
      (progn
	;; In XEmacs, hash tables can also be created by the lisp reader
	;; using structure syntax.
	(read-from-string "#s(hash-table)")
	(defun lsdb-load-file (file)
	  "Read the contents of FILE into a hash table."
	  (save-excursion
	    (set-buffer (find-file-noselect file))
	    (re-search-forward "^#s")
	    (beginning-of-line)
	    (read (point-min-marker)))))
    (invalid-read-syntax
    (defun lsdb-load-file (file)
      "Read the contents of FILE into a hash table."
      (let* ((plist
	      (with-temp-buffer
		(insert-file-contents file)
		(save-excursion
		  (re-search-forward "^#s")
		  (replace-match "")
		  (beginning-of-line)
		  (cdr (read (point-marker))))))
	     (size (plist-get plist 'size))
	     (data (plist-get plist 'data))
	     (hash-table (lsdb-make-hash-table :size size :test 'equal)))
	(while data
	  (lsdb-puthash (pop data) (pop data) hash-table))
	hash-table)))))

(defun lsdb-save-file (file hash-table)
  "Write the entries within HASH-TABLE into FILE."
  (let ((coding-system-for-write lsdb-file-coding-system))
    (with-temp-file file
      (if (and (or (featurep 'mule)
		   (featurep 'file-coding))
	       lsdb-file-coding-system)
	  (insert ";;; -*- coding: "
		  (if (symbolp lsdb-file-coding-system)
		      (symbol-name lsdb-file-coding-system)
		    ;; XEmacs
		    (coding-system-name lsdb-file-coding-system))
		  " -*-\n"))
      (insert "#s(hash-table size "
	      (number-to-string (lsdb-hash-table-size hash-table))
	      " test equal data (")
      (lsdb-maphash
       (lambda (key value)
	 (insert (prin1-to-string key) " " (prin1-to-string value) " "))
       hash-table)
      (insert "))"))))

(defun lsdb-offer-save ()
  (if (and lsdb-hash-table-is-dirty
	   (y-or-n-p "Save the LSDB now?"))
      (lsdb-save-file lsdb-file lsdb-hash-table)))

;;;_. Mail Header Extraction
(defun lsdb-fetch-field-bodies (regexp)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
	  field-bodies)
      (while (re-search-forward (concat "^\\(" regexp "\\):[ \t]*")
				nil t)
	(push (funcall lsdb-decode-field-body-function
			     (buffer-substring (point) (std11-field-end))
			     (match-string 1))
		    field-bodies))
      (nreverse field-bodies))))

(defun lsdb-canonicalize-spaces-and-dots (string)
  (while (string-match "  +\\|[\f\t\n\r\v]+\\|\\." string)
    (setq string (replace-match " " nil t string)))
  string)

(defun lsdb-extract-address-components (string)
  (let ((components (std11-extract-address-components string)))
    (if (nth 1 components)
	(if (car components)
	    (list (nth 1 components)
		  (funcall lsdb-canonicalize-full-name-function
			   (car components)))
	  (list (nth 1 components) (nth 1 components))))))

;; stolen (and renamed) from nnheader.el
(defun lsdb-decode-field-body (field-body field-name
					  &optional mode max-column)
  (let ((multibyte enable-multibyte-characters))
    (unwind-protect
	(progn
	  (set-buffer-multibyte t)
	  (mime-decode-field-body field-body
				  (if (stringp field-name)
				      (intern (capitalize field-name))
				    field-name)
				  mode max-column))
      (set-buffer-multibyte multibyte))))

;;;_. Record Management
(defun lsdb-maybe-load-file ()
  (unless lsdb-hash-table
    (if (file-exists-p lsdb-file)
	(setq lsdb-hash-table (lsdb-load-file lsdb-file))
      (setq lsdb-hash-table (lsdb-make-hash-table :test 'equal)))))

(defun lsdb-update-record (sender &optional interesting)
  (let ((old (lsdb-gethash (nth 1 sender) lsdb-hash-table))
	(new (cons (cons 'net (list (car sender)))
		   interesting))
	merged
	record)
    (unless old
      (setq new (cons (cons 'creation-date (format-time-string "%Y-%m-%d"))
		      new)))
    (setq merged (lsdb-merge-record-entries old new)
	  record (cons (nth 1 sender) merged))
    (unless (equal merged old)
      (let ((entry (assq 'last-modified (cdr record)))
	    (last-modified (format-time-string "%Y-%m-%d")))
	(if entry
	    (setcdr entry last-modified)
	  (setcdr record (cons (cons 'last-modified last-modified)
			       (cdr record)))))
      (lsdb-puthash (car record) (copy-sequence (cdr record))
		    lsdb-hash-table)
      (setq lsdb-hash-table-is-dirty t))
    record))

(defun lsdb-update-records ()
  (lsdb-maybe-load-file)
  (let (senders recipients interesting alist records bodies entry)
    (save-restriction
      (std11-narrow-to-header)
      (setq senders
	    (delq nil (mapcar #'lsdb-extract-address-components
			      (lsdb-fetch-field-bodies
			       lsdb-sender-headers)))
	    recipients
	    (delq nil (mapcar #'lsdb-extract-address-components
			      (lsdb-fetch-field-bodies
			       lsdb-recipients-headers))))
      (setq alist lsdb-interesting-header-alist)
      (while alist
	(setq bodies
	      (mapcar
	       (lambda (field-body)
		 (if (and (nth 1 (car alist))
			  (string-match (nth 1 (car alist)) field-body))
		     (replace-match (nth 3 (car alist)) nil nil field-body)
		   field-body))
	       (lsdb-fetch-field-bodies (car (car alist)))))
	(when bodies
	  (setq entry (or (nth 2 (car alist))
			  'notes))
	  (push (cons entry
		      (if (eq ?. (nth 2 (assq entry lsdb-entry-type-alist)))
			  (car bodies)
			bodies))
		interesting))
	(setq alist (cdr alist))))
    (if senders
	(setq records (list (lsdb-update-record (pop senders) interesting))))
    (setq alist (nconc senders recipients))
    (while alist
      (setq records (cons (lsdb-update-record (pop alist)) records)))
    (nreverse records)))

(defun lsdb-merge-record-entries (old new)
  (setq old (copy-sequence old))
  (while new
    (let ((entry (assq (car (car new)) old))
	  list pointer)
      (if (null entry)
	  (setq old (nconc old (list (car new))))
	(if (listp (cdr entry))
	    (progn
	      (setq list (cdr (car new)) pointer list)
	      (while pointer
		(if (member (car pointer) (cdr entry))
		    (setq list (delq (car pointer) list)))
		(setq pointer (cdr pointer)))
	      (setcdr entry (nconc (cdr entry) list)))
	  (setcdr entry (cdr (car new))))))
    (setq new (cdr new)))
  old)

;;;_. Display Management
(defun lsdb-temp-buffer-show-function (buffer)
  (save-selected-window
    (let ((window (or (get-buffer-window lsdb-buffer-name)
		      (progn
			(select-window (get-largest-window))
			(split-window-vertically))))
	  height)
      (set-window-buffer window buffer)
      (select-window window)
      (unless (pos-visible-in-window-p (point-max))
	(enlarge-window (- lsdb-window-max-height (window-height))))
      (shrink-window-if-larger-than-buffer)
      (if (> (setq height (window-height))
	     lsdb-window-max-height)
	  (shrink-window (- height lsdb-window-max-height))
	  (shrink-window-if-larger-than-buffer)))))

(defun lsdb-display-record (record)
  (let ((temp-buffer-show-function
	 (function lsdb-temp-buffer-show-function)))
    (with-output-to-temp-buffer lsdb-buffer-name
      (set-buffer standard-output)
      (funcall lsdb-print-record-function record)
      (lsdb-mode))))

(defun lsdb-print-record (record)
  (insert (car record) "\n")
  (let ((entries
	 (sort (cdr record)
	       (lambda (entry1 entry2)
		 (> (or (nth 1 (assq (car entry1) lsdb-entry-type-alist))
			0)
		    (or (nth 1 (assq (car entry2) lsdb-entry-type-alist))
			0))))))
    (while entries
      (insert "\t" (capitalize (symbol-name (car (car entries)))) ": "
	      (if (listp (cdr (car entries)))
		  (mapconcat #'identity (cdr (car entries))
			     (if (eq ?, (nth 2 (assq (car (car entries))
						     lsdb-entry-type-alist)))
				 ", "
			       "\n\t\t"))
		(cdr (car entries)))
	      "\n")
      (setq entries (cdr entries)))))

;;;_. Completion
(defvar lsdb-last-completion nil)
(defvar lsdb-last-candidates nil)
(defvar lsdb-last-candidates-pointer nil)

(defun lsdb-complete-name ()
  "Complete the user full-name or net-address before point"
  (interactive)
  (let* ((start
	  (save-excursion
	    (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
	    (goto-char (match-end 0))
	    (point)))
	 pattern
	 (case-fold-search t)
	 (completion-ignore-case t))
    (unless (eq last-command this-command)
      (setq lsdb-last-candidates nil
	    lsdb-last-candidates-pointer nil
	    lsdb-last-completion (buffer-substring start (point))
	    pattern (concat "\\<" lsdb-last-completion))
      (lsdb-maphash
       (lambda (key value)
	 (let ((net (cdr (assq 'net value))))
	   (if (string-match pattern key)
	       (setq lsdb-last-candidates
		     (nconc lsdb-last-candidates
			    (mapcar (lambda (address)
				      (if (equal key address)
					  key
					(concat key " <" address ">")))
				    net)))
	     (while net
	       (if (string-match pattern (car net))
		   (push (car net) lsdb-last-candidates))
	       (setq net (cdr net))))))
       lsdb-hash-table))
    (unless lsdb-last-candidates-pointer
      (setq lsdb-last-candidates-pointer lsdb-last-candidates))
    (when lsdb-last-candidates-pointer
      (delete-region start (point))
      (insert (pop lsdb-last-candidates-pointer)))))

;;;_. Major Mode (`lsdb-mode') Implementation
(define-derived-mode lsdb-mode fundamental-mode "LSDB"
  "Major mode for browsing LSDB records."
  (setq buffer-read-only t)
  (if (featurep 'xemacs)
      ;; In XEmacs, setting `font-lock-defaults' only affects on
      ;; `find-file-hooks'.
      (font-lock-set-defaults)
    (set (make-local-variable 'font-lock-defaults)
	 '(lsdb-font-lock-keywords t))))

;;;_. Interface to Semi-gnus
;;;###autoload
(defun lsdb-gnus-insinuate ()
  "Call this function to hook LSDB into Semi-gnus."
  (add-hook 'gnus-article-prepare-hook 'lsdb-gnus-update-record)
  (add-hook 'gnus-save-newsrc-hook 'lsdb-offer-save))

(defvar gnus-current-headers)
(defun lsdb-gnus-update-record ()
  (let ((entity gnus-current-headers)
	records)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (buffer-disable-undo)
      (mime-insert-entity entity)
      (setq records (lsdb-update-records))
      (when records
	(lsdb-display-record (car records))))))

;;;_. Interface to Wanderlust
(defun lsdb-wl-insinuate ()
  "Call this function to hook LSDB into Wanderlust."
  (add-hook 'wl-message-redisplay-hook 'lsdb-wl-update-record)
  (add-hook 'wl-exit-hook 'lsdb-offer-save))

(defun lsdb-wl-update-record ()
  (save-excursion
    (set-buffer (wl-message-get-original-buffer))
    (let ((records (lsdb-update-records)))
      (when records
	(lsdb-display-record (car records))))))

(provide 'lsdb)

;;;_* Local emacs vars.
;;; The following `outline-layout' local variable setting:
;;;  - closes all topics from the first topic to just before the third-to-last,
;;;  - shows the children of the third to last (config vars)
;;;  - and the second to last (code section),
;;;  - and closes the last topic (this local-variables section).
;;;Local variables:
;;;outline-layout: (0 : -1 -1 0)
;;;End:

;;; lsdb.el ends here
