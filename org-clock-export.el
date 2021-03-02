;;; org-clock-export.el --- Export org-clock data as CSV -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Jeff Filipovits

;; Author: Jeff Filipovits <jrfilipovits@gmail.com>
;; Url: https://github.com/legalnonsense/org-clock-export
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1") (org "9.0") (org-ql "0.5-pre")
;; Keywords: Org, clock, CSV

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Export org-clock data into a CSV file.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;;; Installation

;; Install these required packages:

;; + org-ql

;; Then put this file in your load-path, and put this in your init
;; file. See the README for a use-package declaration. 


;;;; Usage


;;;; Tips



;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;; Requirements

(require 'org-ql)
(require 'cl-lib)
(require 'rx)

;;;; Customization

(defgroup org-clock-export nil
  "org-clock-export options"
  :tag " org-clock-export"
  :group 'org
  :group 'org-clock-export
  :prefix "org-clock-export-")

(defcustom org-clock-export-buffer "*ORG-CLOCK-EXPORT CSV*"
  "Buffer used to export CSV data."
  :type 'string)

(defcustom org-clock-export-export-file-name (concat user-emacs-directory "clock-export.csv")
  "File to export data to."
  :type 'file)

(defcustom org-clock-export-delimiter ","
  "Delimiter used in the CSV output. Default: \",\""
  :type 'string)

(defcustom org-clock-export-org-ql-query nil
  "Additional query used by `org-ql-select' to determine which headings to 
check.  For example, if you want to narrow the results to headings tagged with
\"Customer1\" then set this to '(tags \"Customer1\").  See `org-ql' documentation 
for more information.  

All queries are automatically limited to headings with clock lines.  You do not 
need to include '(clocked), however you can add additional limitations, e.g.,
'(clocked :on today)."
  :type 'sexp)

(defcustom org-clock-export-files nil
  "If nil, use `org-agenda-files'.  Otherwise, specify a file
or list of files.  Default: nil."
  :type '(choice file list string))

(defcustom org-clock-export-data
  '( "date" (concat start-month "/" start-day "/" start-year)
     "hours" total-hours
     "minutes" total-minutes
     "description" (org-entry-get (point) "ITEM")
     "hourly rate" (or (org-entry-get (point) "HOURLY-RATE") "325"))
  "The following variables are let-bound when the functions are called, 
based on the values in the clock line:
start-year
start-month
start-day
start-dow
start-hour
start-minute
end-year
end-month
end-day
end-dow
end-hour
end-minute
total-hours
total-minutes.

Any other sexp is evaluated at the first point of each heading with a clock line.  
Hence, you can use `org-entry-get' to retrieve property values, or any other 
method to gather data from the heading.  The only rules are that it must return a 
string, and it must keep the point at the heading when finsihed (i.e., you should
 `save-excursion' if you move the point when looking for data.

If there are multiple clock lines in a heading, this returns a line of CSV data for each one."
  :type '(repeat (list symbol sexp)))

;;;; Constants

(defconst org-clock-export--clock-re (rx (seq bol
					      (zero-or-more (any "	 "))
					      "CLOCK: ")
					 (seq "["
					      ;; start-year
					      (group-n 1 (= 4 digit))
					      "-"
					      ;; start month
					      (group-n 2 (= 2 digit))
					      "-"
					      ;; start day
					      (group-n 3 (= 2 digit))
					      (one-or-more not-newline)
					      ;; start DOW
					      (group-n 4 (= 3 alpha))
					      (one-or-more not-newline)
					      ;; start hour
					      (group-n 5 (= 2 digit))
					      ":"
					      ;; start minute
					      (group-n 6 (= 2 digit))
					      "]")
					 (seq "--")
					 (seq "["
					      ;; end-year
					      (group-n 7 (= 4 digit))
					      "-"
					      ;; end month
					      (group-n 8 (= 2 digit))
					      "-"
					      ;; end day
					      (group-n 9 (= 2 digit))
					      (one-or-more not-newline)
					      ;; end DOW
					      (group-n 10 (= 3 alpha))
					      (one-or-more not-newline)
					      ;; end hour
					      (group-n 11 (= 2 digit))
					      ":"
					      ;; end minute
					      (group-n 12 (= 2 digit))
					      "]")
					 (seq (one-or-more space)
					      "=>"
					      (one-or-more space))
					 ;; total hours
					 (seq (group-n 13 (one-or-more digit))
					      ":"
					      ;; total minutes
					      (group-n 14 (one-or-more digit))))
  "Clock line RE.  The groups are explained in the comments.")

;;;; Functions

(defmacro org-clock-export--parse-clock-lines-in-heading (arg)
  "Return data based on `org-clock-export-data'.  Used by 
`org-clock-export--run-org-ql' as the `org-ql-select' action."
  `(save-excursion
     (cl-flet ((get-limit () (or (save-excursion
				   (end-of-line)
				   (re-search-forward org-heading-regexp nil t))
				 (point-max))))
       (cl-loop while
		(re-search-forward org-clock-export--clock-re (get-limit) 'no-error)
		collect
		(list ,@(cl-loop for
				 x from 1 to (1- (length (symbol-value arg))) by 2
				 collect
				 `(cl-flet ((get-match (num) (org-no-properties (match-string num))))
				    (let ((start-year (get-match 1))
					  (start-month (get-match 2))
					  (start-day (get-match 3))
					  (start-dow (get-match 4))
					  (start-hour (get-match 5))
					  (start-minute (get-match 6))
					  (end-year (get-match 7))
					  (end-month (get-match 8))
					  (end-day (get-match 9))
					  (end-dow (get-match 10))
					  (end-hour (get-match 11))
					  (end-minute (get-match 12))
					  (total-hours (get-match 13))
					  (total-minutes (get-match 14)))
				      ,(nth x (symbol-value arg))))))))))

(defun org-clock-export--run-org-ql ()
  "Run org-ql to process all headings in `org-clock-export-files' and
return a list with an element for each clock line."
  (cl-loop for each in
	   (org-ql-select (or org-clock-export-files
			      (org-agenda-files))
	     `(and (clocked)
		   ,org-clock-export-org-ql-query)
	     :action '(org-clock-export--parse-clock-lines-in-heading
		       org-clock-export-data))
	   append each))

;;;; Commands

(defun org-clock-export (&optional prefix)
  "With no prefix, export to buffer.
With one prefix, export to file.
With two prefixes, prompt for file."
  (interactive "p")
  (with-current-buffer (get-buffer-create org-clock-export-buffer)
    (erase-buffer)
    (cl-loop for
	     x from 0 to (1- (length org-clock-export-data)) by 2
	     do
	     (insert  (nth x org-clock-export-data)
		      org-clock-export-delimiter)
	     finally
	     (progn (delete-char (* -1 (length org-clock-export-delimiter)))
		    (insert "\n")))
    (cl-loop for
	     entry in (org-clock-export--run-org-ql)
	     do
	     (cl-loop for
		      data in entry
		      do
		      (insert data org-clock-export-delimiter)
		      finally
		      (progn (delete-char (* -1 (length org-clock-export-delimiter)))
			     (insert "\n"))))
    (pcase prefix
      (`4 (write-region (point-min) (point-max) org-clock-export-export-file-name))
      (`16 (write-region (point-min) (point-max) (read-file-name
						  "File name to export CSV data:"))))))

;;;; Footer

(provide 'org-clock-export)

;;; org-clock-export.el ends here
