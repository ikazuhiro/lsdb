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
;;; (add-hook 'gnus-summary-mode-hook
;;;           (lambda ()
;;;             (define-key gnus-summary-mode-map ":" 'lsdb-toggle-buffer)))

;;; For Wanderlust, put the following lines into your ~/.wl:
;;; (require 'lsdb)
;;; (lsdb-wl-insinuate)
;;; (add-hook 'wl-draft-mode-hook
;;;           (lambda ()
;;;             (define-key wl-draft-mode-map "\M-\t" 'lsdb-complete-name)))
;;; (add-hook 'wl-summary-mode-hook
;;;           (lambda ()
;;;             (define-key wl-summary-mode-map ":" 'lsdb-wl-toggle-buffer)))

;;; For Mew, put the following lines into your ~/.mew:
;;; (autoload 'lsdb-mew-insinuate "lsdb")
;;; (add-hook 'mew-init-hook 'lsdb-mew-insinuate)
;;; (add-hook 'mew-draft-mode-hook
;;;           (lambda ()
;;;             (define-key mew-draft-header-map "\M-I" 'lsdb-complete-name)))
;;; (add-hook 'mew-summary-mode-hook
;;;           (lambda ()
;;;             (define-key mew-summary-mode-map ":" 'lsdb-toggle-buffer)))

;;; Code:

(require 'poem)
(require 'pces)
(require 'mime)
(require 'static)

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
    ("\\(X-\\)?User-Agent\\|X-Mailer\\|X-Newsreader" nil user-agent)
    ("\\(X-\\)?ML-Name" nil mailing-list)
    ("List-Id" "\\(.*\\)[ \t]+<[^>]+>\\'" mailing-list "\\1")
    ("X-Sequence" "\\(.*\\)[ \t]+[0-9]+\\'" mailing-list "\\1")
    ("Delivered-To" "mailing list[ \t]+\\([^@]+\\)@.*" mailing-list "\\1")
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
    (creation-date 2 ?. t)
    (last-modified 3 ?. t)
    (mailing-list 4 ?,)
    (attribution 4 ?.)
    (organization 4)
    (www 4)
    (aka 4 ?,)
    (score -1)
    (x-face -1))
  "Alist of entry types for presentation.
The format of elements of this list should be
     (ENTRY SCORE [CLASS READ-ONLY])
where the last two elements are optional.
Possible values for CLASS are `?.' and '?,'.  If CLASS is `?.', the
entry takes a unique value which is overridden by newly assigned one
by `lsdb-mode-edit-entry' or such a command.  If CLASS is `?,', the
entry can have multiple values separated by commas.
If the fourth element READ-ONLY is non-nil, it is assumed that the
entry cannot be modified."
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

(defcustom lsdb-lookup-full-name-functions
  '(lsdb-lookup-full-name-from-address-cache)
  "List of functions to pick up the existing full-name of the sender.
The sender is passed to each function as the argument."
  :group 'lsdb
  :type 'hook)

(defcustom lsdb-update-record-functions
  '(lsdb-update-address-cache)
  "List of functions called after a record is updated.
The updated record is passed to each function as the argument."
  :group 'lsdb
  :type 'hook)

(defcustom lsdb-secondary-hash-tables
  '(lsdb-address-cache)
  "List of the hash tables for reverse lookup"
  :group 'lsdb
  :type 'list)

(defcustom lsdb-window-max-height 7
  "Maximum number of lines used to display LSDB record."
  :group 'lsdb
  :type 'integer)

(defcustom lsdb-x-face-command-alist
  '((pbm "{ echo '/* Width=48, Height=48 */'; uncompface; } | icontopbm | pnmscale 0.5")
    (xpm "{ echo '/* Width=48, Height=48 */'; uncompface; } | icontopbm | pnmscale 0.5 | ppmtoxpm"))
  "An alist from an image type to a command to be executed to display an X-Face header.
The command will be executed in a sub-shell asynchronously.
The compressed face will be piped to this command."
  :group 'lsdb
  :type 'list)

(defcustom lsdb-insert-x-face-function
  (if (static-if (featurep 'xemacs)
	  (featurep 'xpm)
	(and (>= emacs-major-version 21)
	     (fboundp 'image-type-available-p)
	     (or (image-type-available-p 'pbm)
		 (image-type-available-p 'xpm))))
      #'lsdb-insert-x-face-asynchronously)
  "Function to display X-Face."
  :group 'lsdb
  :type 'function)

(defcustom lsdb-print-record-hook '(lsdb-expose-x-face)
  "A hook called after a record is displayed."
  :group 'lsdb
  :type 'hook)

(defcustom lsdb-display-records-sort-predicate nil
  "A predicate to sort records."
  :group 'lsdb
  :type 'function)

(defcustom lsdb-pop-up-windows t
  "Non-nil means LSDB should make new windows to display records."
  :group 'lsdb
  :type 'boolean)

(defgroup lsdb-edit-form nil
  "A mode for editing forms."
  :group 'lsdb)

(defcustom lsdb-edit-form-mode-hook nil
  "Hook run in `lsdb-edit-form-mode' buffers."
  :group 'lsdb-edit-form
  :type 'hook)

(defcustom lsdb-shell-file-name "/bin/sh"
  "File name to load inferior shells from.
Bourne shell or its equivalent \(not tcsh) is needed for \"2>\"."
  :group 'lsdb
  :type 'string)

(defcustom lsdb-shell-command-switch "-c"
  "Switch used to have the shell execute its command line argument."
  :group 'lsdb
  :type 'string)

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

(defvar lsdb-address-cache nil
  "The reverse lookup table for `lsdb-hash-table'.
It represents address to full-name mapping.")

(defvar lsdb-buffer-name "*LSDB*"
  "Buffer name to display LSDB record.")

(defvar lsdb-hash-tables-are-dirty nil
  "Flag to indicate whether the internal hash tables need to be saved.")

(defvar lsdb-known-entry-names
  (make-vector 29 0)
  "An obarray used to complete an entry name.")

(defvar lsdb-temp-buffer-show-function
  #'lsdb-temp-buffer-show-function
  "Non-nil means call as function to display a help buffer.
The function is called with one argument, the buffer to be displayed.
Overrides `temp-buffer-show-function'.")

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
(defconst lsdb-secondary-hash-table-start-format
  ";;; %S\n")

(defsubst lsdb-secondary-hash-table-start (hash-table)
  (format lsdb-secondary-hash-table-start-format hash-table))

(eval-and-compile
  (condition-case nil
      (progn
	;; In XEmacs, hash tables can also be created by the lisp reader
	;; using structure syntax.
	(read-from-string "#s(hash-table)")
	(defalias 'lsdb-read 'read))
    (invalid-read-syntax
     (defun lsdb-read (&optional marker)
       "Read one Lisp expression as text from MARKER, return as Lisp object."
       (save-excursion
	 (goto-char marker)
	 (if (looking-at "^#s(")
	     (with-temp-buffer
	       (buffer-disable-undo)
	       (insert-buffer-substring (marker-buffer marker) marker)
	       (goto-char (point-min))
	       (delete-char 2)
	       (let ((object (read (current-buffer)))
		     hash-table data)
		 (if (eq 'hash-table (car object))
		     (progn
		       (setq hash-table
			     (lsdb-make-hash-table
			      :size (plist-get (cdr object) 'size)
			      :test 'equal)
			     data (plist-get (cdr object) 'data))
		       (while data
			 (lsdb-puthash (pop data) (pop data) hash-table))
		       hash-table)
		   object)))))))))

(defun lsdb-load-hash-tables ()
  "Read the contents of `lsdb-file' into the internal hash tables."
  (let ((buffer (find-file-noselect lsdb-file))
	tables)
    (unwind-protect
	(save-excursion
	  (set-buffer buffer)
	  (goto-char (point-min))
	  (re-search-forward "^#s(")
	  (goto-char (match-beginning 0))
	  (setq lsdb-hash-table (lsdb-read (point-marker)))
	  ;; Load the secondary hash tables following.
	  (setq tables lsdb-secondary-hash-tables)
	  (while tables
	    (if (re-search-forward
		 (concat "^" (lsdb-secondary-hash-table-start
			      (car tables)))
		 nil t)
		(set (car tables) (lsdb-read (point-marker))))
	    (setq tables (cdr tables))))
      (kill-buffer buffer))))

(defun lsdb-insert-hash-table (hash-table)
  (insert "#s(hash-table size "
	  ;; Reduce the actual size of the close hash table, because
	  ;; XEmacs doesn't have a distinction between index-size and
	  ;; hash-table-size.
	  (number-to-string (lsdb-hash-table-count hash-table))
	  " test equal data (")
  (lsdb-maphash
   (lambda (key value)
     (insert (prin1-to-string key) " " (prin1-to-string value) " "))
   hash-table)
  (insert "))"))

(defun lsdb-save-hash-tables ()
  "Write the records within the internal hash tables into `lsdb-file'."
  (let ((coding-system-for-write lsdb-file-coding-system)
	tables)
    (with-temp-file lsdb-file
      (if (and (or (featurep 'mule)
		   (featurep 'file-coding))
	       lsdb-file-coding-system)
	  (let ((coding-system-name
		 (if (symbolp lsdb-file-coding-system)
		     (symbol-name lsdb-file-coding-system)
		   ;; XEmacs
		   (static-if (featurep 'xemacs)
		       (symbol-name (coding-system-name
				     lsdb-file-coding-system))))))
	    (if coding-system-name
		(insert ";;; -*- coding: " coding-system-name " -*-\n"))))
      (lsdb-insert-hash-table lsdb-hash-table)
      ;; Save the secondary hash tables following.
      (setq tables lsdb-secondary-hash-tables)
      (while tables
	(insert "\n" (lsdb-secondary-hash-table-start
		      (car tables)))
	(lsdb-insert-hash-table (symbol-value (car tables)))
	(setq tables (cdr tables))))))

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
    (if (and (nth 1 components)
	     ;; When parsing a group address,
	     ;; std11-extract-address-components is likely to return
	     ;; the ("GROUP" "") form.
	     (not (equal (nth 1 components) "")))
	(if (car components)
	    (list (funcall lsdb-canonicalize-full-name-function
			   (car components))
		  (nth 1 components))
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
(defun lsdb-maybe-load-secondary-hash-tables ()
  (let ((tables lsdb-secondary-hash-tables))
    (while tables
      (unless (symbol-value (car tables))
	(set (car tables) (lsdb-make-hash-table :test 'equal))
	(lsdb-maphash
	 (lambda (key value)
	   (run-hook-with-args
	    'lsdb-update-record-functions
	    (cons key value)))
	 lsdb-hash-table)
	(setq lsdb-hash-tables-are-dirty t))
      (setq tables (cdr tables)))))

(defun lsdb-maybe-load-hash-tables ()
  (unless lsdb-hash-table
    (if (file-exists-p lsdb-file)
	(lsdb-load-hash-tables)
      (setq lsdb-hash-table (lsdb-make-hash-table :test 'equal)))
    (lsdb-maybe-load-secondary-hash-tables)))

;;;_ : Fallback Lookup Functions
;;;_  , #1 Address Cache
(defun lsdb-lookup-full-name-from-address-cache (sender)
  (lsdb-gethash (nth 1 sender) lsdb-address-cache))

(defun lsdb-update-address-cache (record)
  (let ((net (cdr (assq 'net record))))
    (while net
      (lsdb-puthash (pop net) (car record) lsdb-address-cache))))

;;;_  , #2 Iterate on the All Records (very slow)
(defun lsdb-lookup-full-name-by-fuzzy-matching (sender)
  (let ((names
	 (if (string-match
	      "\\`\\(.+\\)[ \t]+\\(/[ \t]+\\|(\\([^)]+\\))\\)"
	      (car sender))
	     (if (match-beginning 3)
		 (list (match-string 1 (car sender))
		       (match-string 3 (car sender)))
	       (list (match-string 1 (car sender))
		     (substring (car sender) (match-end 0))))
	   (list (car sender))))
	(case-fold-search t))
    (catch 'found
      (lsdb-maphash
       (lambda (key value)
	 (while names
	   (if (or (string-match
		    (concat "\\<" (regexp-quote (car names)) "\\>")
		    key)
		   (string-match
		    (concat
		     "\\<"
		     (regexp-quote
		      (mapconcat #'identity
				 (nreverse (split-string (car names)))
				 " "))
		     "\\>")
		    key)
		   ;; Don't assume that we are using address cache.
		   (member (nth 1 sender) (cdr (assq 'net value))))
	       (throw 'found key))
	   (setq names (cdr names))))
       lsdb-hash-table))))

;;;_ : Update Records
(defun lsdb-update-record (sender &optional interesting)
  (let ((old (lsdb-gethash (car sender) lsdb-hash-table))
	(new (cons (cons 'net (list (nth 1 sender)))
		   interesting))
	merged
	record
	full-name)
    ;; Look for the existing record from the reverse hash table.
    ;; If it is found, regsiter the current full-name as AKA.
    (unless old
      (setq full-name
	    (run-hook-with-args-until-success
	     'lsdb-lookup-full-name-functions
	     sender))
      (when full-name
	(setq old (lsdb-gethash full-name lsdb-hash-table)
	      new (cons (list 'aka (car sender)) new))
	(setcar sender full-name)))
    (unless old
      (setq new (cons (cons 'creation-date (format-time-string "%Y-%m-%d"))
		      new)))
    (setq merged (lsdb-merge-record-entries old new)
	  record (cons (car sender) merged))
    (unless (equal merged old)
      (let ((entry (assq 'last-modified (cdr record)))
	    (last-modified (format-time-string "%Y-%m-%d")))
	(if entry
	    (setcdr entry last-modified)
	  (setcdr record (cons (cons 'last-modified last-modified)
			       (cdr record)))))
      (lsdb-puthash (car record) (cdr record)
		    lsdb-hash-table)
      (run-hook-with-args 'lsdb-update-record-functions record)
      (setq lsdb-hash-tables-are-dirty t))
    record))

(defun lsdb-update-records ()
  (lsdb-maybe-load-hash-tables)
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
	      (delq nil (mapcar
			 (lambda (field-body)
			   (if (nth 1 (car alist))
			       (and (string-match (nth 1 (car alist))
						  field-body)
				    (replace-match (nth 3 (car alist))
						   nil nil field-body))
			     field-body))
			 (lsdb-fetch-field-bodies (car (car alist))))))
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
(defun lsdb-fit-window-to-buffer (&optional window)
  (save-selected-window
    (if window
	(select-window window))
    (unless (pos-visible-in-window-p (point-max))
      (enlarge-window (- lsdb-window-max-height (window-height))))
    (shrink-window-if-larger-than-buffer)
    (let ((height (window-height)))
      (if (> height lsdb-window-max-height)
	  (shrink-window (- height lsdb-window-max-height)))
      (set-window-start window (point-min)))))

(defun lsdb-temp-buffer-show-function (buffer)
  (when lsdb-pop-up-windows
    (save-selected-window
      (let ((window (or (get-buffer-window lsdb-buffer-name)
			(progn
			  (select-window (get-largest-window))
			  (split-window-vertically)))))
	(set-window-buffer window buffer)
	(lsdb-fit-window-to-buffer window)))))

(defun lsdb-display-record (record)
  "Display only one RECORD, then shrink the window as possible."
  (let ((temp-buffer-show-function lsdb-temp-buffer-show-function))
    (lsdb-display-records (list record))))

(defun lsdb-display-records (records)
  (with-output-to-temp-buffer lsdb-buffer-name
    (set-buffer standard-output)
    (setq records
	  (sort (copy-sequence records)
		(or lsdb-display-records-sort-predicate
		    (lambda (record1 record2)
		      (string-lessp (car record1) (car record2))))))
    (while records
      (save-restriction
	(narrow-to-region (point) (point))
	(lsdb-print-record (car records)))
      (goto-char (point-max))
      (setq records (cdr records)))
    (lsdb-mode)))

(defsubst lsdb-entry-score (entry)
  (or (nth 1 (assq (car entry) lsdb-entry-type-alist)) 0))

(defun lsdb-insert-entry (entry)
  (let ((entry-name (capitalize (symbol-name (car entry)))))
    (intern entry-name lsdb-known-entry-names)
    (if (>= (lsdb-entry-score entry) 0)
	(insert "\t" entry-name ": "
		(if (listp (cdr entry))
		    (mapconcat
		     #'identity (cdr entry)
		     (if (eq ?, (nth 2 (assq (car entry)
					     lsdb-entry-type-alist)))
			 ", "
		       "\n\t\t"))
		  (cdr entry))
		"\n"))))

(defun lsdb-print-record (record)
  (insert (car record) "\n")
  (let ((entries
	 (sort (copy-sequence (cdr record))
	       (lambda (entry1 entry2)
		 (> (lsdb-entry-score entry1) (lsdb-entry-score entry2))))))
    (while entries
      (lsdb-insert-entry (car entries))
      (setq entries (cdr entries))))
  (add-text-properties (point-min) (point-max)
		       (list 'lsdb-record record))
  (run-hooks 'lsdb-print-record-hook))

;;;_. Completion
(defvar lsdb-last-completion nil)
(defvar lsdb-last-candidates nil)
(defvar lsdb-last-candidates-pointer nil)

;;;_ : Matching Highlight
(defvar lsdb-last-highlight-overlay nil)

(defun lsdb-complete-name-highlight (start end)
  (make-local-hook 'pre-command-hook)
  (add-hook 'pre-command-hook 'lsdb-complete-name-highlight-update nil t)
  (save-excursion
    (goto-char start)
    (search-forward lsdb-last-completion end)
    (setq lsdb-last-highlight-overlay
	  (make-overlay (match-beginning 0) (match-end 0)))
    (overlay-put lsdb-last-highlight-overlay 'face
		 (or (find-face 'isearch-secondary)
		     (find-face 'isearch-lazy-highlight-face)
		     'underline))))

(defun lsdb-complete-name-highlight-update ()
  (unless (eq 'this-command 'lsdb-complete-name)
    (if lsdb-last-highlight-overlay
	(delete-overlay lsdb-last-highlight-overlay))
    (remove-hook 'pre-command-hook
		 'lsdb-complete-name-highlight-update t)))

;;;_ : Name Completion
(defun lsdb-complete-name ()
  "Complete the user full-name or net-address before point"
  (interactive)
  (lsdb-maybe-load-hash-tables)
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
	    pattern (concat "\\<" (regexp-quote lsdb-last-completion)))
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
       lsdb-hash-table)
      ;; Sort candidates by the position where the pattern occurred.
      (setq lsdb-last-candidates
	    (sort lsdb-last-candidates
		  (lambda (cand1 cand2)
		    (< (if (string-match pattern cand1)
			   (match-beginning 0))
		       (if (string-match pattern cand2)
			   (match-beginning 0)))))))
    (unless lsdb-last-candidates-pointer
      (setq lsdb-last-candidates-pointer lsdb-last-candidates))
    (when lsdb-last-candidates-pointer
      (delete-region start (point))
      (insert (pop lsdb-last-candidates-pointer))
      (lsdb-complete-name-highlight start (point)))))

;;;_. Major Mode (`lsdb-mode') Implementation
;;;_ : Modeline Buffer Identification
(defconst lsdb-pointer-xpm
  "/* XPM */
static char * lsdb_pointer_xpm[] = {
\"14 14 5 1\",
\" 	c None\",
\"+	c #FF9696\",
\"@	c #FF0000\",
\"#	c #FF7575\",
\"$	c #FF5959\",
\"              \",
\"  +++   @@@   \",
\" +++## @@@@@  \",
\" ++### @@@@@  \",
\" +#####@@@@@  \",
\" +###$$@@@@@  \",
\" +###$$@@@@@  \",
\"  ##$$$@@@@   \",
\"   #$$$@@@    \",
\"    $$@@@     \",
\"     $@@      \",
\"      @       \",
\"              \",
\"              \"};")

(static-if (featurep 'xemacs)
    (progn
      (defvar lsdb-xemacs-modeline-left-extent
	(copy-extent modeline-buffer-id-left-extent))

      (defvar lsdb-xemacs-modeline-right-extent
	(copy-extent modeline-buffer-id-right-extent))

      (defun lsdb-modeline-buffer-identification (line)
	"Decorate 1st element of `mode-line-buffer-identification' LINE.
Modify whole identification by side effect."
	(let ((id (car line)) chopped)
	  (if (and (stringp id) (string-match "^LSDB:" id))
	      (progn
		(setq chopped (substring id 0 (match-end 0))
		      id (substring id (match-end 0)))
		(nconc
		 (list
		  (let ((glyph
			 (make-glyph
			  (nconc
			   (if (featurep 'xpm)
			       (list (vector 'xpm :data lsdb-pointer-xpm)))
			   (list (vector 'string :data chopped))))))
		    (set-glyph-face glyph 'modeline-buffer-id)
		    (cons lsdb-xemacs-modeline-left-extent glyph))
		  (cons lsdb-xemacs-modeline-right-extent id))
		 (cdr line)))
	    line))))
  (condition-case nil
      (progn
	(require 'image)
	(defun lsdb-modeline-buffer-identification (line)
	  "Decorate 1st element of `mode-line-buffer-identification' LINE.
Modify whole identification by side effect."
	  (let ((id (copy-sequence (car line)))
		(image
		 (if (image-type-available-p 'xpm)
		     (create-image lsdb-pointer-xpm 'xpm t :ascent 'center))))
	    (when (and image
		       (stringp id) (string-match "^LSDB:" id))
	      (add-text-properties 0 (length id)
				   (list 'display image
					 'rear-nonsticky (list 'display))
				   id)
	      (setcar line id))
	    line)))
    (error
     (defalias 'lsdb-modeline-buffer-identification 'identity))))

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

(defvar lsdb-modeline-string "")

(define-derived-mode lsdb-mode fundamental-mode "LSDB"
  "Major mode for browsing LSDB records."
  (setq buffer-read-only t)
  (static-if (featurep 'xemacs)
      ;; In XEmacs, setting `font-lock-defaults' only affects on
      ;; `find-file-hooks'.
      (font-lock-set-defaults)
    (set (make-local-variable 'font-lock-defaults)
	 '(lsdb-font-lock-keywords t)))
  (make-local-hook 'post-command-hook)
  (add-hook 'post-command-hook 'lsdb-modeline-update nil t)
  (make-local-variable 'lsdb-modeline-string)
  (setq mode-line-buffer-identification
	(lsdb-modeline-buffer-identification
	 '("LSDB: " lsdb-modeline-string)))
  (lsdb-modeline-update)
  (force-mode-line-update))

(defun lsdb-modeline-update ()
  (let ((record
	 (get-text-property (if (eobp) (point-min) (point)) 'lsdb-record))
	net)
    (if record
	(progn
	  (setq net (car (cdr (assq 'net (cdr record)))))
	  (if (equal net (car record))
	      (setq lsdb-modeline-string net)
	    (setq lsdb-modeline-string (concat (car record) " <" net ">"))))
      (setq lsdb-modeline-string ""))))

(defun lsdb-narrow-to-record ()
  "Narrow to the current record."
  (let ((end (next-single-property-change (point) 'lsdb-record nil
					  (point-max))))
    (narrow-to-region
     (previous-single-property-change (point) 'lsdb-record nil (point-min))
     end)
    (goto-char (point-min))))

(defun lsdb-current-record ()
  "Return the current record name."
  (let ((record (get-text-property (point) 'lsdb-record)))
    (unless record
      (error "There is nothing to follow here"))
    record))

(defun lsdb-current-entry ()
  "Return the current entry name.
If the point is not on a entry line, it prompts to select a entry in
the current record."
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
	      (run-hook-with-args 'lsdb-update-record-functions record)
	      (setq lsdb-hash-tables-are-dirty t)
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
      (run-hook-with-args 'lsdb-update-record-functions record)
      (setq lsdb-hash-tables-are-dirty t))
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
	(unless (equal form ',(cdr entry))
	  (save-excursion
	    (set-buffer lsdb-buffer-name)
	    (goto-char ,marker)
	    (let* ((record (lsdb-current-record))
		   (entry (assq ',entry-name (cdr record)))
		   (inhibit-read-only t)
		   buffer-read-only)
	      (setcdr entry form)
	      (run-hook-with-args 'lsdb-update-record-functions record)
	      (setq lsdb-hash-tables-are-dirty t)
	      (lsdb-mode-delete-entry (symbol-name ',entry-name) t)
	      (beginning-of-line)
	      (add-text-properties
	       (point)
	       (progn
		 (lsdb-insert-entry (cons ',entry-name form))
		 (point))
	       (list 'lsdb-record record)))))))))

(defun lsdb-mode-save (&optional dont-ask)
  "Save LSDB hash table into `lsdb-file'."
  (interactive)
  (if (not lsdb-hash-tables-are-dirty)
      (message "(No changes need to be saved)")
    (when (or (interactive-p)
	      dont-ask
	      (y-or-n-p "Save the LSDB now?"))
      (lsdb-save-hash-tables)
      (setq lsdb-hash-tables-are-dirty nil)
      (message "The LSDB was saved successfully."))))

(defun lsdb-mode-quit-window (&optional kill window)
  "Quit the current buffer.
It partially emulates the GNU Emacs' of `quit-window'."
  (interactive "P")
  (unless window
    (setq window (selected-window)))
  (let ((buffer (window-buffer window)))
    (unless (save-selected-window
	      (select-window window)
	      (one-window-p))
      (delete-window window))
    (if kill
	(kill-buffer buffer)
      (bury-buffer buffer))))

(defun lsdb-hide-buffer ()
  "Hide the LSDB window."
  (let ((window (get-buffer-window lsdb-buffer-name)))
    (if window
	(lsdb-mode-quit-window nil window))))

(defun lsdb-show-buffer ()
  "Show the LSDB window."
  (if (get-buffer lsdb-buffer-name)
      (if lsdb-temp-buffer-show-function
	  (let ((lsdb-pop-up-windows t))
	    (funcall lsdb-temp-buffer-show-function lsdb-buffer-name))
	(pop-to-buffer lsdb-buffer-name))))

(defun lsdb-toggle-buffer (&optional arg)
  "Toggle hiding of the LSDB window.
If given a negative prefix, always show; if given a positive prefix,
always hide."
  (interactive
   (list (if current-prefix-arg
	     (prefix-numeric-value current-prefix-arg)
	   0)))
  (unless arg				;called noninteractively?
    (setq arg 0))
  (cond
   ((or (< arg 0)
	(and (zerop arg)
	     (not (get-buffer-window lsdb-buffer-name))))
    (lsdb-show-buffer))
   ((or (> arg 0)
	(and (zerop arg)
	     (get-buffer-window lsdb-buffer-name)))
    (lsdb-hide-buffer))))

(defun lsdb-lookup-records (regexp &optional entry-name)
  "Return the all records in the LSDB matching the REGEXP.
If the optional 2nd argument ENTRY-NAME is given, matching only
performed against the entry field."
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
  "Display the all records in the LSDB matching the REGEXP.
If the optional 2nd argument ENTRY-NAME is given, matching only
performed against the entry field."
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
  (lsdb-maybe-load-hash-tables)
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
  (add-hook 'wl-summary-exit-hook 'lsdb-hide-buffer)
  (add-hook 'wl-summary-toggle-disp-off-hook 'lsdb-hide-buffer)
  (add-hook 'wl-summary-toggle-disp-folder-on-hook 'lsdb-hide-buffer)
  (add-hook 'wl-summary-toggle-disp-folder-off-hook 'lsdb-hide-buffer)
  (add-hook 'wl-summary-toggle-disp-folder-message-resumed-hook
	    'lsdb-wl-show-buffer)
  (add-hook 'wl-exit-hook 'lsdb-mode-save)
  (add-hook 'wl-save-hook 'lsdb-mode-save))

(eval-when-compile
  (autoload 'wl-message-get-original-buffer "wl-message"))
(defun lsdb-wl-update-record ()
  (save-excursion
    (set-buffer (wl-message-get-original-buffer))
    (let ((records (lsdb-update-records)))
      (when records
	(let ((lsdb-temp-buffer-show-function
	       #'lsdb-wl-temp-buffer-show-function))
	  (lsdb-display-record (car records)))))))

(defun lsdb-wl-toggle-buffer (&optional arg)
  "Toggle hiding of the LSDB window for Wanderlust.
If given a negative prefix, always show; if given a positive prefix,
always hide."
  (interactive
   (list (if current-prefix-arg
	     (prefix-numeric-value current-prefix-arg)
	   0)))
  (let ((lsdb-temp-buffer-show-function
	 #'lsdb-wl-temp-buffer-show-function))
    (lsdb-toggle-buffer arg)))

(defun lsdb-wl-show-buffer ()
  (when lsdb-pop-up-windows
    (let ((lsdb-temp-buffer-show-function
	   #'lsdb-wl-temp-buffer-show-function))
      (lsdb-show-buffer))))

(defvar wl-current-summary-buffer)
(defvar wl-message-buffer)
(defun lsdb-wl-temp-buffer-show-function (buffer)
  (when lsdb-pop-up-windows
    (save-selected-window
      (let ((window (or (get-buffer-window lsdb-buffer-name)
			(progn
			  (select-window 
			   (or (save-excursion
				 (if (buffer-live-p wl-current-summary-buffer)
				     (set-buffer wl-current-summary-buffer))
				 (get-buffer-window wl-message-buffer))
			       (get-largest-window)))
			  (split-window-vertically)))))
	(set-window-buffer window buffer)
	(lsdb-fit-window-to-buffer window)))))

;;;_. Interface to Mew written by Hideyuki SHIRAI <shirai@rdmg.mgcs.mei.co.jp>
(eval-when-compile
  (autoload 'mew-sinfo-get-disp-msg "mew")
  (autoload 'mew-current-get-fld "mew")
  (autoload 'mew-current-get-msg "mew")
  (autoload 'mew-frame-id "mew")
  (autoload 'mew-cache-hit "mew"))

;;;###autoload
(defun lsdb-mew-insinuate ()
  "Call this function to hook LSDB into Mew."
  (add-hook 'mew-message-hook 'lsdb-mew-update-record)
  (add-hook 'mew-summary-toggle-disp-msg-hook
	    (lambda ()
	      (unless (mew-sinfo-get-disp-msg)
		(lsdb-hide-buffer))))
  (add-hook 'mew-suspend-hook 'lsdb-hide-buffer)
  (add-hook 'mew-quit-hook 'lsdb-mode-save)
  (add-hook 'kill-emacs-hook 'lsdb-mode-save))

(defun lsdb-mew-update-record ()
  (let* ((fld (mew-current-get-fld (mew-frame-id)))
	 (msg (mew-current-get-msg (mew-frame-id)))
	 (cache (mew-cache-hit fld msg 'must-hit))
	 records)
    (save-excursion
      (set-buffer cache)
      (make-local-variable 'lsdb-decode-field-body-function)
      (setq lsdb-decode-field-body-function
	    (lambda (body name)
	      (set-text-properties 0 (length body) nil body)
	      body))
      (when (setq records (lsdb-update-records))
	(lsdb-display-record (car records))))))

;;;_. Interface to MU-CITE
(eval-when-compile
  (autoload 'mu-cite-get-value "mu-cite"))

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
	(run-hook-with-args 'lsdb-update-record-functions (car records))
	(setq lsdb-hash-tables-are-dirty t)))))

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
(defvar lsdb-x-face-cache
  (lsdb-make-hash-table :test 'equal))

(defun lsdb-x-face-available-image-type ()
  (static-if (featurep 'xemacs)
      (if (featurep 'xpm)
	  'xpm)
    (and (>= emacs-major-version 21)
	 (fboundp 'image-type-available-p)
	 (if (image-type-available-p 'pbm)
	     'pbm
	   (if (image-type-available-p 'xpm)
	       'xpm)))))

(defun lsdb-expose-x-face ()
  (let* ((record (get-text-property (point-min) 'lsdb-record))
	 (x-face (cdr (assq 'x-face (cdr record))))
	 (delimiter "\r "))
    (when (and lsdb-insert-x-face-function
	       x-face)
      (goto-char (point-min))
      (end-of-line)
      (put-text-property 0 1 'invisible t delimiter) ;hide "\r"
      (put-text-property
       (point)
       (progn
	 (insert delimiter)
	 (while x-face
	   (funcall lsdb-insert-x-face-function (pop x-face)))
	 (point))
       'lsdb-record record))))

(defun lsdb-insert-x-face-image (data type marker)
  (static-if (featurep 'xemacs)
      (save-excursion
	(set-buffer (marker-buffer marker))
	(goto-char marker)
	(let* ((inhibit-read-only t)
	       buffer-read-only
	       (glyph (make-glyph (vector type :data data))))
	  (set-extent-begin-glyph
	   (make-extent (point) (point))
	   glyph)))
    (save-excursion
      (set-buffer (marker-buffer marker))
      (goto-char marker)
      (let* ((inhibit-read-only t)
	     buffer-read-only
	     (image (create-image data type t :ascent 'center))
	     (record (get-text-property (point) 'lsdb-record)))
	(put-text-property (point) (progn
				     (insert-image image)
				     (point))
			   'lsdb-record record)))))

(defun lsdb-insert-x-face-asynchronously (x-face)
  (let* ((type (lsdb-x-face-available-image-type))
	 (shell-file-name lsdb-shell-file-name)
	 (shell-command-switch lsdb-shell-command-switch)
	 (process-connection-type nil)
	 (cached (cdr (assq type (lsdb-gethash x-face lsdb-x-face-cache))))
	 (marker (point-marker))
	 process)
    (if cached
	(lsdb-insert-x-face-image cached type marker)
      (setq process
	    (start-process-shell-command
	     "lsdb-x-face-command" (generate-new-buffer " *lsdb work*")
	     (concat "{ "
		     (nth 1 (assq type lsdb-x-face-command-alist))
		     "; } 2> /dev/null")))
      (process-send-string process (concat x-face "\n"))
      (process-send-eof process)
      (set-process-sentinel
       process
       `(lambda (process string)
	  (unwind-protect
	      (when (and (buffer-live-p (marker-buffer ,marker))
			 (equal string "finished\n"))
		(let ((data
		       (with-current-buffer (process-buffer process)
			 (set-buffer-multibyte nil)
			 (buffer-string))))
		  (lsdb-insert-x-face-image data ',type ,marker)
		  (lsdb-puthash ,x-face (list (cons ',type data))
				lsdb-x-face-cache)))
	    (kill-buffer (process-buffer process))))))))

(require 'product)
(provide 'lsdb)

(product-provide 'lsdb
  (product-define "LSDB" nil '(0 5)))

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
