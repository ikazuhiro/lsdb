;;; xbm-thumb.el --- create XBM thumbnail under Emacs.
;; Copyright (C) 2000 Daiki Ueno

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Created: 2000-02-26
;; Keywords: xbm, image

;; This file is not part of any package.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:
;; 

;;; Code:

(defvar xbm-thumb-dot-threshold 1)

(defun xbm-thumb-fold-left (function accu sequence)
  (if (null sequence) accu
    (xbm-thumb-fold-left
     function (funcall function accu (car sequence))
     (cdr sequence))))

(defun xbm-thumb-aggregate-block (b1 b2)
  (let ((idx 128) (result 0))
    (while (> idx 1)
      (setq result
	    (logior (lsh result 1)
		    (if (< xbm-thumb-dot-threshold
			   (xbm-thumb-fold-left
			    #'+ 0 (list
				   (logand b1 idx)
				   (logand b1 (lsh idx -1))
				   (logand b2 idx)
				   (logand b2 (lsh idx -1)))))
			1 0))
	    idx (lsh idx -2)))
    result))

(defun xbm-thumb-aggregate-row (row)
  (let ((len (/ (length row) 2))
	(result "")
	(i 0))
    (while (< i len)
      (setq result
	    (format "%s\\x%02x" result
		    (logior
		     (lsh (xbm-thumb-aggregate-block
			   (aref row (1+ i)) (aref row (+ i 1 len))) 4)
		     (xbm-thumb-aggregate-block
		      (aref row i) (aref row (+ i len)))))
	    i (+ i 2)))
    result))

;;;###autoload
(defun xbm-make-thumbnail (data)
  "Create XBM thumbnail."
  (let* ((string (nth 2 data))
	 (len (length string))
	 (width (/ (car data) 8))
	 (result "")
	 (i 0))
    (while (< i len)
      (setq result
	    (concat result
		    (xbm-thumb-aggregate-row
		     (substring string i (setq i (+ i (* 2 width))))))))
    (list
     (/ (car data) 2) (/ (nth 1 data) 2)
     (car (read-from-string (concat "\"" result "\""))))))

(provide 'xbm-thumb)

;;; xbm-thumb.el ends here
