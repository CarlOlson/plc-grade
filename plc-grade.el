
(require 'cl-lib) ;; cl-remove-if-not

(defun plc-grade ()
  "Grades file in current buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*PLC Agda Grade*"))
	(file-name (buffer-file-name))
	(need-save-p (buffer-modified-p))
	(has-goals-p (agda2-has-goals-p))
	(has-grading-p (file-directory-p
			(concat "./grading/" (file-name-base (buffer-file-name))))))
    (with-current-buffer buffer
      (delete-region (point-max) (point-min))
      (cond
       ((not has-grading-p) (insert "./grading directory not found.\n"))
       (need-save-p (insert "File must be saved for changes to be graded.\n"))
       (has-goals-p (progn ;; warning: agda2-has-goals-p doesn't reload file
		     (insert "File must have no holes to grade. This is a limitation of Agda.\n")
		     (insert "If this is a mistake reload the file. (C-c C-l)\n")))
       (t (plc-grade-helper file-name))))
    (display-buffer buffer))
  )

(defun plc-grade-helper (file-name)
  "Evaluates tests. Prints errors, score, and warnings."
  (let* ((results
	  (plc-check-tests
	   (directory-files (concat "./grading/" (file-name-base file-name))
			    t
			    "\.agda$")
	   'plc-check-file))
	 (score (apply '+ (cl-remove-if-not 'numberp results)))
	 (messages (apply 'concat (cl-remove-if-not 'stringp results))))
    (insert messages)
    (insert (format "Total score: \t%d\n\n" score))
    (if (<= score 0)
	(insert "Warning: \tFile must have no holes to grade. This is a limitation of Agda.\n"))
    ;; (insert (if (string= "" (agda2-eval-file file-name "-v 0 --safe"))
    ;; 	    "File passed safe mode.\n" "File failed safe mode.\n"))
    )
  )

(defun plc-check-tests (files processor)
  "Checks agda files with processor given."
  (if files
      (let* ((test-file (car files))
	     (point-file (plc-test-to-point-file test-file))
	     (rest (cdr files)))
	(cons
	 (if (file-exists-p point-file)
	     (funcall processor test-file (plc-extract-points point-file))
	   (concat "File doesn't exist: \t" point-file "\n"))
	 (plc-check-tests rest processor))
	)
    '()
    )
  )

(defun plc-test-to-point-file (test-file)
  "Converts a file `X.agda' to `X.points'."
  (if (s-chop-suffix ".agda" test-file)
      (concat (substring test-file 0 -5) ".points")
    nil)
  )

(defun plc-extract-points (point-file)
  "Returns the number contained within `point-file'."
  (with-temp-buffer
    (insert-file-contents point-file)
    (string-to-number (buffer-substring (point-min) (point-max))))
  )

(defun plc-check-file (file points)
  "Checks agda file. Returns `points' or error string."
  (if (string= ""
	       (agda2-eval-file file "-v 0"))
      points 
    (concat "File failed: \t" (file-name-nondirectory file) "\n"))
  )

(defun agda2-eval-file (file &optional args)
  "Evaluates adga file with `agda2-include-dirs' added to path. Optional `args' are appended."
  (with-output-to-string
    (apply 'call-process agda2-program-name nil standard-output nil
	   (split-string (concat "-i " (mapconcat (lambda (a) a) agda2-include-dirs " -i ")
				 " -i " (file-name-directory file)
				 " " args " "
				 file))))
  )

(defun agda2-has-goals-p ()
  "Determins if current file has active goal. File must be evaluated with `agda2-load'."
  (save-excursion
    (goto-char (point-min))
    (agda2-next-goal)
    (not (eq (point) (point-min))))
  )

;; defined in emacs 24.4
(defun file-name-base (&optional filename)
  "Return the base name of the FILENAME: no directory, no extension.
FILENAME defaults to `buffer-file-name'."
  (file-name-sans-extension
   (file-name-nondirectory (or filename (buffer-file-name)))))

;; defined in s.el library
;; https://github.com/magnars/s.el/blob/master/s.el
(defun s-chop-suffix (suffix s)
  "Remove SUFFIX if it is at end of S."
  (let ((pos (- (length suffix))))
    (if (and (>= (length s) (length suffix))
	     (string= suffix (substring s pos)))
	(substring s 0 pos)
      s)))
