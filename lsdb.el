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
(require 'pces)
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

(defcustom lsdb-file-coding-system (find-coding-system 'iso-2022-jp)
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
    ("\\(X-URL\\|X-URI\\)" nil www)
    ("X-Attribution\\|X-cite-me" nil attribution)
    ("X-Face" nil x-face))
  "Alist of headers we are interested in.
The format of elements of this list should be
     (FIELD-NAME REGEXP ENTRY STRING)
where the last three elements are optional."
  :group 'lsdb
  :type 'list)

(defcustom lsdb-entry-type-alist
  '((net 5 ?,)
    (creation-date 2)
    (last-modified 3)
    (mailing-list 4 ?,)
    (attribution 4 ?.)
    (organization 4)
    (www 1)
    (score -1)
    (x-face -1))
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

(defcustom lsdb-window-max-height 7
  "Maximum number of lines used to display LSDB record."
  :group 'lsdb
  :type 'integer)

(defcustom lsdb-insert-x-face-function
  (if (and (>= emacs-major-version 21)
	   (locate-library "x-face-e21"))
      #'lsdb-insert-x-face-with-x-face-e21
    (if (and (featurep 'xemacs)
	     (memq 'xface (image-instantiator-format-list)))
	#'lsdb-insert-x-face-with-xemacs-glyph))
  "Function to display X-Face."
  :group 'lsdb
  :type 'function)

(defcustom lsdb-display-record-hook
  (if lsdb-insert-x-face-function
      #'lsdb-expose-x-face)
  "A hook called after a record is displayed."
  :group 'lsdb
  :type 'hook)

(defgroup lsdb-edit-form nil
  "A mode for editing forms."
  :group 'lsdb)

(defcustom lsdb-edit-form-mode-hook nil
  "Hook run in `lsdb-edit-form-mode' buffers."
  :group 'lsdb-edit-form
  :type 'hook)

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
  '(("^\\sw[^\r\n]*"
     (0 lsdb-header-face))
    ("^\t\t.*$"
     (0 lsdb-field-body-face))
    ("^\t\\([^\t:]+:\\)[ \t]*\\(.*\\)$"
     (1 lsdb-field-name-face)
     (2 lsdb-field-body-face))))

(put 'lsdb-mode 'font-lock-defaults '(lsdb-font-lock-keywords t))

;;;_* CODE - no user customizations below
;;;_. Internal Variables
(defvar lsdb-hash-table nil
  "Internal hash table to hold LSDB records.")

(defvar lsdb-buffer-name "*LSDB*"
  "Buffer name to display LSDB record.")

(defvar lsdb-hash-table-is-dirty nil
  "Flag to indicate whether the hash table needs to be saved.")

(defvar lsdb-known-entry-names
  (make-vector 29 0)
  "An obarray used to complete an entry name.")

;;;_. Hash Table Emulation
(if (and (fboundp 'make-hash-table)
	 (subrp (symbol-function 'make-hash-table)))
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
     (nth 1 hash-table)))
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
	  (let ((buffer (find-file-noselect file)))
	    (unwind-protect
		(save-excursion
		  (set-buffer buffer)
		  (re-search-forward "^#s")
		  (beginning-of-line)
		  (read (point-min-marker)))
	      (kill-buffer buffer)))))
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
		    (symbol-name (coding-system-name lsdb-file-coding-system)))
		  " -*-\n"))
      (insert "#s(hash-table size "
	      (number-to-string (lsdb-hash-table-size hash-table))
	      " test equal data (")
      (lsdb-maphash
       (lambda (key value)
	 (insert (prin1-to-string key) " " (prin1-to-string value) " "))
       hash-table)
      (insert "))"))))

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
      (lsdb-puthash (car record) (cdr record)
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
	  (shrink-window (- height lsdb-window-max-height)))
      (set-window-start window (point-min)))))

(defun lsdb-display-record (record)
  "Display only one RECORD, then shrink the window as possible."
  (let ((temp-buffer-show-function
	 (function lsdb-temp-buffer-show-function)))
    (lsdb-display-records (list record))))

(defun lsdb-display-records (records)
  (with-output-to-temp-buffer lsdb-buffer-name
    (set-buffer standard-output)
    (while records
      (save-restriction
	(narrow-to-region (point) (point))
	(lsdb-print-record (car records))
	(add-text-properties (point-min) (point-max)
			     (list 'lsdb-record (car records)))
	(run-hooks 'lsdb-display-record-hook))
      (goto-char (point-max))
      (setq records (cdr records)))
    (lsdb-mode)))

(defsubst lsdb-entry-score (entry)
  (or (nth 1 (assq (car entry) lsdb-entry-type-alist)) 0))

(defun lsdb-insert-entry (entry)
  (let ((entry-name (capitalize (symbol-name (car entry)))))
    (intern entry-name lsdb-known-entry-names)
    (insert "\t" entry-name ": "
	    (if (listp (cdr entry))
		(mapconcat
		 #'identity (cdr entry)
		 (if (eq ?, (nth 2 (assq (car entry) lsdb-entry-type-alist)))
		     ", "
		   "\n\t\t"))
	      (cdr entry))
	    "\n")))

(defun lsdb-print-record (record)
  (insert (car record) "\n")
  (let ((entries
	 (sort (copy-sequence (cdr record))
	       (lambda (entry1 entry2)
		 (> (lsdb-entry-score entry1) (lsdb-entry-score entry2))))))
    (while entries
      (if (>= (lsdb-entry-score (car entries)) 0)
	  (lsdb-insert-entry (car entries)))
      (setq entries (cdr entries)))))

;;;_. Completion
(defvar lsdb-last-completion nil)
(defvar lsdb-last-candidates nil)
(defvar lsdb-last-candidates-pointer nil)

(defun lsdb-complete-name ()
  "Complete the user full-name or net-address before point"
  (interactive)
  (lsdb-maybe-load-file)
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
(defvar lsdb-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "a" 'lsdb-mode-add-entry)
    (define-key keymap "d" 'lsdb-mode-delete-entry)
    (define-key keymap "e" 'lsdb-mode-edit-entry)
    (define-key keymap "s" 'lsdb-mode-save)
    (define-key keymap "q" 'lsdb-mode-quit-window)
    (define-key keymap "g" 'lsdb-mode-lookup)
    (define-key keymap "p" 'lsdb-mode-previous-record)
    (define-key keymap "n" 'lsdb-mode-next-record)
    (define-key keymap " " 'scroll-up)
    (define-key keymap [delete] 'scroll-down)
    (define-key keymap "\177" 'scroll-down)
    (define-key keymap [backspace] 'scroll-down)
    keymap)
  "LSDB's keymap.")

(define-derived-mode lsdb-mode fundamental-mode "LSDB"
  "Major mode for browsing LSDB records."
  (setq buffer-read-only t)
  (if (featurep 'xemacs)
      ;; In XEmacs, setting `font-lock-defaults' only affects on
      ;; `find-file-hooks'.
      (font-lock-set-defaults)
    (set (make-local-variable 'font-lock-defaults)
	 '(lsdb-font-lock-keywords t))))

(defun lsdb-narrow-to-record ()
  (narrow-to-region
   (previous-single-property-change (point) 'lsdb-record nil (point-min))
   (next-single-property-change (point) 'lsdb-record nil (point-max)))
  (goto-char (point-min)))

(defun lsdb-current-record ()
  (let ((record (get-text-property (point) 'lsdb-record)))
    (unless record
      (error "There is nothing to follow here"))
    record))

(defun lsdb-current-entry ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^[^\t]")
	(let ((record (lsdb-current-record))
	      (completion-ignore-case t))
	  (completing-read
	   "Which entry to modify: "
	   (mapcar (lambda (entry)
		     (list (capitalize (symbol-name (car entry)))))
		   (cdr record))))
      (end-of-line)
      (re-search-backward "^\t\\([^\t][^:]+\\):")
      (match-string 1))))

(defun lsdb-mode-add-entry (entry-name)
  "Add an entry on the current line."
  (interactive
   (let ((completion-ignore-case t))
     (list (completing-read "Entry name: " lsdb-known-entry-names))))
  (beginning-of-line)
  (unless (symbolp entry-name)
    (setq entry-name (intern (downcase entry-name))))
  (when (assq entry-name (cdr (lsdb-current-record)))
    (error "The entry already exists"))
  (let ((marker (point-marker)))
    (lsdb-edit-form
     nil "Editing the entry."
     `(lambda (form)
	(when form
	  (save-excursion
	    (set-buffer lsdb-buffer-name)
	    (goto-char ,marker)
	    (let ((record (lsdb-current-record))
		  (inhibit-read-only t)
		  buffer-read-only)
	      (setcdr record (cons (cons ',entry-name form) (cdr record)))
	      (lsdb-puthash (car record) (cdr record)
			    lsdb-hash-table)
	      (setq lsdb-hash-table-is-dirty t)
	      (beginning-of-line 2)
	      (add-text-properties
	       (point)
	       (progn
		 (lsdb-insert-entry (cons ',entry-name form))
		 (point))
	       (list 'lsdb-record record)))))))))

(defun lsdb-mode-delete-entry (&optional entry-name dont-update)
  "Delete the entry on the current line."
  (interactive)
  (let ((record (lsdb-current-record))
	entry)
    (or entry-name
	(setq entry-name (lsdb-current-entry)))
    (setq entry (assq (intern (downcase entry-name)) (cdr record)))
    (when (and entry
	       (not dont-update))
      (setcdr record (delq entry (cdr record)))
      (lsdb-puthash (car record) (cdr record)
		    lsdb-hash-table)
      (setq lsdb-hash-table-is-dirty t))
    (save-restriction
      (lsdb-narrow-to-record)
      (let ((case-fold-search t)
	    (inhibit-read-only t)
	    buffer-read-only)
	(goto-char (point-min))
	(if (re-search-forward
	     (concat "^\t" (or entry-name
			       (lsdb-current-entry))
		     ":")
	     nil t)
	    (delete-region (match-beginning 0)
			   (if (re-search-forward
				"^\t[^\t][^:]+:" nil t)
			       (match-beginning 0)
			     (point-max))))))))

(defun lsdb-mode-edit-entry ()
  "Edit the entry on the current line."
  (interactive)
  (let* ((record (lsdb-current-record))
	 (entry-name (intern (downcase (lsdb-current-entry))))
	 (entry (assq entry-name (cdr record)))
	 (marker (point-marker)))
    (lsdb-edit-form
     (cdr entry) "Editing the entry."
     `(lambda (form)
	(unless (equal form ',entry-name)
	  (save-excursion
	    (set-buffer lsdb-buffer-name)
	    (goto-char ,marker)
	    (let* ((record (lsdb-current-record))
		   (entry (assq ',entry-name (cdr record)))
		   (inhibit-read-only t)
		   buffer-read-only)
	      (setcdr entry form)
	      (setq lsdb-hash-table-is-dirty t)
	      (lsdb-mode-delete-entry (symbol-name ',entry-name) t)
	      (beginning-of-line)
	      (add-text-properties
	       (point)
	       (progn
		 (lsdb-insert-entry (cons ',entry-name form))
		 (point))
	       (list 'lsdb-record record)))))))))

(defun lsdb-mode-save ()
  "Save LSDB hash table into `lsdb-file'."
  (interactive)
  (if (not lsdb-hash-table-is-dirty)
      (message "(No changes need to be saved)")
    (when (or (interactive-p)
	      (y-or-n-p "Save the LSDB now?"))
      (lsdb-save-file lsdb-file lsdb-hash-table)
      (setq lsdb-hash-table-is-dirty nil)
      (message "The LSDB was saved successfully."))))

(if (commandp 'quit-window)
    (defalias 'lsdb-mode-quit-window 'quit-window)
  (defun lsdb-mode-quit-window ()
    "Quit the current buffer."
    (interactive)
    (if (one-window-p)
	(bury-buffer)
      (delete-window))))

(defun lsdb-lookup-records (regexp &optional entry-name)
  (let (records)
    (lsdb-maphash
     (if entry-name
	 (progn
	   (unless (symbolp entry-name)
	     (setq entry-name (intern (downcase entry-name))))
	   (lambda (key value)
	     (let ((entry (cdr (assq entry-name value)))
		   found)
	       (unless (listp entry)
		 (setq entry (list entry)))
	       (while (and (not found) entry)
		 (if (string-match regexp (pop entry))
		     (setq found t)))
	       (if found
		   (push (cons key value) records)))))
       (lambda (key value)
	 (if (string-match regexp key)
	     (push (cons key value) records))))
     lsdb-hash-table)
    records))

(defvar lsdb-mode-lookup-history nil)

(defun lsdb-mode-lookup (regexp &optional entry-name)
  "Display all entries in the LSDB matching the REGEXP."
  (interactive
   (let* ((completion-ignore-case t)
	  (entry-name
	   (if current-prefix-arg
	       (completing-read "Entry name: "
				lsdb-known-entry-names))))
     (list
      (read-from-minibuffer
       (if entry-name
	   (format "Search records `%s' regexp: " entry-name)
	 "Search records regexp: ")
       nil nil nil 'lsdb-mode-lookup-history)
      entry-name)))
  (lsdb-maybe-load-file)
  (let ((records (lsdb-lookup-records regexp entry-name)))
    (if records
	(lsdb-display-records records))))

;;;###autoload
(defalias 'lsdb 'lsdb-mode-lookup)

(defun lsdb-mode-next-record (&optional arg)
  "Go to the next record."
  (interactive "p")
  (unless arg				;called noninteractively?
    (setq arg 1))
  (if (< arg 0)
      (lsdb-mode-previous-record (- arg))
    (while (> arg 0)
      (goto-char (next-single-property-change
		  (point) 'lsdb-record nil (point-max)))
      (setq arg (1- arg)))))

(defun lsdb-mode-previous-record (&optional arg)
  "Go to the previous record."
  (interactive "p")
  (unless arg				;called noninteractively?
    (setq arg 1))
  (if (< arg 0)
      (lsdb-mode-next-record (- arg))
    (while (> arg 0)
      (goto-char (previous-single-property-change
		  (point) 'lsdb-record nil (point-min)))
      (setq arg (1- arg)))))

;;;_ : Edit Forms -- stolen (and renamed) from gnus-eform.el
(defvar lsdb-edit-form-buffer "*LSDB edit form*")
(defvar lsdb-edit-form-done-function nil)
(defvar lsdb-previous-window-configuration nil)

(defvar lsdb-edit-form-mode-map
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap emacs-lisp-mode-map)
    (define-key keymap "\C-c\C-c" 'lsdb-edit-form-done)
    (define-key keymap "\C-c\C-k" 'lsdb-edit-form-exit)
    keymap)
  "Edit form's keymap.")

(defun lsdb-edit-form-mode ()
  "Major mode for editing forms.
It is a slightly enhanced emacs-lisp-mode.

\\{lsdb-edit-form-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'lsdb-edit-form-mode
	mode-name "LSDB Edit Form")
  (use-local-map lsdb-edit-form-mode-map)
  (make-local-variable 'lsdb-edit-form-done-function)
  (make-local-variable 'lsdb-previous-window-configuration)
  (run-hooks 'lsdb-edit-form-mode-hook))

(defun lsdb-edit-form (form documentation exit-func)
  "Edit FORM in a new buffer.
Call EXIT-FUNC on exit.  Display DOCUMENTATION in the beginning
of the buffer."
  (let ((window-configuration
	 (current-window-configuration)))
    (switch-to-buffer (get-buffer-create lsdb-edit-form-buffer))
    (lsdb-edit-form-mode)
    (setq lsdb-previous-window-configuration window-configuration
	  lsdb-edit-form-done-function exit-func)
    (erase-buffer)
    (insert documentation)
    (unless (bolp)
      (insert "\n"))
    (goto-char (point-min))
    (while (not (eobp))
      (insert ";;; ")
      (forward-line 1))
    (insert ";; Type `C-c C-c' after you've finished editing.\n")
    (insert "\n")
    (let ((p (point)))
      (pp form (current-buffer))
      (insert "\n")
      (goto-char p))))

(defun lsdb-edit-form-done ()
  "Update changes and kill the current buffer."
  (interactive)
  (goto-char (point-min))
  (let ((form (condition-case nil
		  (read (current-buffer))
		(end-of-file nil)))
	(func lsdb-edit-form-done-function))
    (lsdb-edit-form-exit)
    (funcall func form)))

(defun lsdb-edit-form-exit ()
  "Kill the current buffer."
  (interactive)
  (let ((window-configuration lsdb-previous-window-configuration))
    (kill-buffer (current-buffer))
    (set-window-configuration window-configuration)))

;;;_. Interface to Semi-gnus
;;;###autoload
(defun lsdb-gnus-insinuate ()
  "Call this function to hook LSDB into Semi-gnus."
  (add-hook 'gnus-article-prepare-hook 'lsdb-gnus-update-record)
  (add-hook 'gnus-save-newsrc-hook 'lsdb-mode-save))

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
;;;###autoload
(defun lsdb-wl-insinuate ()
  "Call this function to hook LSDB into Wanderlust."
  (add-hook 'wl-message-redisplay-hook 'lsdb-wl-update-record)
  (add-hook 'wl-summary-exit-hook 'lsdb-wl-hide-buffer)
  (add-hook 'wl-exit-hook 'lsdb-mode-save))

(defun lsdb-wl-update-record ()
  (save-excursion
    (set-buffer (wl-message-get-original-buffer))
    (let ((records (lsdb-update-records)))
      (when records
	(lsdb-display-record (car records))))))

(defun lsdb-wl-hide-buffer ()
  (let ((window (get-buffer-window lsdb-buffer-name)))
    (if window
	(delete-window window))))

;;;_. Interface to MU-CITE
(defun lsdb-mu-attribution (address)
  "Extract attribute information from LSDB."
  (let ((records
	 (lsdb-lookup-records (concat "\\<" address "\\>") 'net)))
    (if records
	(cdr (assq 'attribution (cdr (car records)))))))

(defun lsdb-mu-set-attribution (attribution address)
  "Add attribute information to LSDB."
  (let ((records
	 (lsdb-lookup-records (concat "\\<" address "\\>") 'net))
	entry)
    (when records
      (setq entry (assq 'attribution (cdr (car records))))
      (if entry
	  (setcdr entry attribution)
	(setcdr (car records) (cons (cons 'attribution attribution)
				    (cdr (car records))))
	(lsdb-puthash (car (car records)) (cdr (car records))
		      lsdb-hash-table)
	(setq lsdb-hash-table-is-dirty t)))))

(defun lsdb-mu-get-prefix-method ()
  "A mu-cite method to return a prefix from LSDB or \">\".
If an `attribution' value is found in LSDB, the value is returned.
Otherwise \">\" is returned."
  (or (lsdb-mu-attribution (mu-cite-get-value 'address))
      ">"))

(defvar minibuffer-allow-text-properties)

(defvar lsdb-mu-history nil)

(defun lsdb-mu-get-prefix-register-method ()
  "A mu-cite method to return a prefix from LSDB or register it.
If an `attribution' value is found in LSDB, the value is returned.
Otherwise the function requests a prefix from a user.  The prefix will
be registered to LSDB if the user wants it."
  (let ((address (mu-cite-get-value 'address)))
    (or (lsdb-mu-attribution address)
	(let* (minibuffer-allow-text-properties
	       (result (read-string "Citation name? "
				    (or (mu-cite-get-value 'x-attribution)
					(mu-cite-get-value 'full-name))
				    'lsdb-mu-history)))
	  (if (and (not (string-equal result ""))
		   (y-or-n-p (format "Register \"%s\"? " result)))
	      (lsdb-mu-set-attribution result address))
	  result))))

(defun lsdb-mu-get-prefix-register-verbose-method ()
  "A mu-cite method to return a prefix using LSDB.

In this method, a user must specify a prefix unconditionally.  If an
`attribution' value is found in LSDB, the value is used as a initial
value to input the prefix.  The prefix will be registered to LSDB if
the user wants it."
  (let* ((address (mu-cite-get-value 'address))
	 (attribution (lsdb-mu-attribution address))
	 minibuffer-allow-text-properties
	 (result (read-string "Citation name? "
			      (or attribution
				  (mu-cite-get-value 'x-attribution)
				  (mu-cite-get-value 'full-name))
			      'lsdb-mu-history)))
    (if (and (not (string-equal result ""))
	     (not (string-equal result attribution))
	     (y-or-n-p (format "Register \"%s\"? " result)))
	(lsdb-mu-set-attribution result address))
    result))

(defvar mu-cite-methods-alist)
;;;###autoload
(defun lsdb-mu-insinuate ()
  (add-hook 'mu-cite-instantiation-hook
	    (lambda ()
	      (setq mu-cite-methods-alist
		    (nconc
		     mu-cite-methods-alist
		     (list
		      (cons 'lsdb-prefix
			    #'lsdb-mu-get-prefix-method)
		      (cons 'lsdb-prefix-register
			    #'lsdb-mu-get-prefix-register-method)
		      (cons 'lsdb-prefix-register-verbose
			    #'lsdb-mu-get-prefix-register-verbose-method)))))))

;;;_. X-Face Rendering
(defun lsdb-expose-x-face ()
  (let* ((record (get-text-property (point-min) 'lsdb-record))
	 (x-face (cdr (assq 'x-face (cdr record))))
	 (limit "\r"))
    (when (and lsdb-insert-x-face-function
	       x-face)
      (goto-char (point-min))
      (end-of-line)
      (if (fboundp 'propertize)
	  (insert (propertize limit 'invisible t) " ")
	(put-text-property 0 1 'invisible t limit)
	(insert limit " "))
      (while x-face
	(funcall lsdb-insert-x-face-function (pop x-face))))))

;; stolen (and renamed) from gnus-summary-x-face.el written by Akihiro Arisawa.
(defvar lsdb-x-face-scale-factor 0.5
  "A number of scale factor used to scale down X-face image.
See also `x-face-scale-factor'.")

(defun lsdb-insert-x-face-with-x-face-e21 (x-face)
  (require 'x-face-e21)
  (insert-image (x-face-create-image
		 x-face :scale-factor lsdb-x-face-scale-factor)))

(defun lsdb-insert-x-face-with-xemacs-glyph (x-face)
  (let ((glyph
	 (make-glyph
	  (vector 'xface :data (concat "X-Face: " x-face)))))
    (if glyph
	(set-extent-end-glyph
	 (make-extent (point) (point))
	 glyph))))

(require 'product)
(provide 'lsdb)

(product-provide 'lsdb
  (product-define "LSDB" nil '(0 1)))

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
