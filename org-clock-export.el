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

;; See the README.

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

;; I have no idea if I am using the :type keyword correctly!

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

(defcustom org-clock-export-data-format
  '( "date" (concat start-month "/" start-day "/" start-year)
     "hours" total-hours
     "minutes" total-minutes
     "description" (org-entry-get (point) "ITEM")
     "hourly rate" (or (org-entry-get (point) "HOURLY-RATE") "325"))
  "The following variables are let-bound when the sexps are called, 
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
total-minutes
total-time

Any other sexp is evaluated at the first point of each heading with a clock 
line.  Hence, you can use `org-entry-get' to retrieve property values, or any
other method to gather data from the heading.  The only rules are that it must
return a string, and it must keep the point at the heading when finsihed (i.e.,
you should `save-excursion' if you move the point when looking for data.

If there are multiple clock lines in a heading, this returns a line of CSV data
for each one."
  :type '(repeat (list string sexp)))

;;;; Constants

(defconst org-clock-export--clock-re
  (rx (seq bol
	   (zero-or-more (any "	 "))
	   "CLOCK: ")
      (seq "["
	   (group-n 1 (= 4 digit)) ;; start year
	   "-"
	   (group-n 2 (= 2 digit)) ;; start month
	   "-"
	   (group-n 3 (= 2 digit)) ;; start day
	   (one-or-more space)
	   (group-n 4 (= 3 alpha)) ;; start DOW
	   (one-or-more space)
	   (group-n 5 (= 2 digit)) ;; start hour
	   ":"
	   (group-n 6 (= 2 digit)) ;; start minute
	   "]")
      "--"
      (seq "["
	   (group-n 7 (= 4 digit)) ;; end year
	   "-"
	   (group-n 8 (= 2 digit)) ;; end month
	   "-"
	   (group-n 9 (= 2 digit)) ;; end day
	   (one-or-more space)
	   (group-n 10 (= 3 alpha)) ;; end DOW
	   (one-or-more space)
	   (group-n 11 (= 2 digit)) ;; end hour
	   ":"
	   (group-n 12 (= 2 digit)) ;; end minute
	   "]")
      (seq (one-or-more space)
	   "=>"
	   (one-or-more space))
      (seq (group-n 15 ;; total time (hh:mm format)
		    (group-n 13 (one-or-more digit)) ;; total hours
		    ":"
		    (group-n 14 (one-or-more digit))))) ;; total minutes
  "Clock line RE.  The groups are explained in the comments.")

;;;; Functions

(defmacro org-clock-export--parse-clock-lines-in-heading (arg)
  "Return data based on `org-clock-export-data-format'.  Used by 
`org-clock-export--run-org-ql' as the `org-ql-select' action."
  `(save-excursion
     (cl-flet ((get-limit () (or (save-excursion
				   (end-of-line)
				   (re-search-forward org-heading-regexp nil t))
				 (point-max))))
       (cl-loop while
		(re-search-forward org-clock-export--clock-re (get-limit) 'no-error)
		collect
		(cl-flet ((get-match (num) (org-no-properties (match-string num))))
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
			(total-minutes (get-match 14))
			(total-time (get-match 15)))
		    (list
		     ,@(cl-loop
			for
			x from 1 to (1- (length arg)) by 2
			collect
			(nth x arg)))))))))

(defun org-clock-export--run-org-ql (&optional query files export-data)
  "Run org-ql to process all headings in `org-clock-export-files' and
return a list with an element for each clock line."
  ;; cl-loop is necessary to flatten the results, since each heading
  ;; might have more than one clock line
  (cl-loop for each in
	   (org-ql-select (or files
			      org-clock-export-files
			      (org-agenda-files))
	     `(and (clocked)
		   ,query)
	     :action `(org-clock-export--parse-clock-lines-in-heading
		       ,export-data))
	   append each))         

(defun org-clock-export--csv-quote (entry delimiter)
  "Escape entries to be CSV-safe, by quoting the field if it contains a delimiter, newline or double-quote."
  (if (string-match-p (rx (or "\"" "\n" (literal delimiter))) entry)
      (concat "\"" (string-replace "\"" "\"\"" entry) "\"")
    entry))

;;;; Commands

(cl-defun org-clock-export (&key (org-files org-clock-export-files)
				 (org-ql-query org-clock-export-org-ql-query)
				 (csv-data-format org-clock-export-data-format)
				 (output-file org-clock-export-file-name)
				 (delimiter org-clock-export-delimiter)				    
				 (output-buffer org-clock-export-buffer))
  (with-current-buffer (get-buffer-create output-buffer)
    (erase-buffer)
    (cl-flet ((clean-up ()
			(delete-char (* -1 (length delimiter)))
			(insert "\n")))
      (cl-loop for
	       x from 0 to (1- (length csv-data-format)) by 2
	       do
	       (insert  (org-clock-export--csv-quote (nth x csv-data-format) delimiter)
			delimiter)
	       finally
	       (clean-up))
      (cl-loop for
	       entry in (org-clock-export--run-org-ql org-ql-query org-files csv-data-format)
	       do
	       (cl-loop for
			data in entry
			do
			(insert (org-clock-export--csv-quote data delimiter) delimiter)
			finally
			(clean-up))))
    (when output-file
      (write-region (point-min) (point-max)
		    output-file))))

;;;; Footer

(provide 'org-clock-export)

;;; org-clock-export.el ends here
