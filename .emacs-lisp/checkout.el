(provide 'checkout)

;; Comment given for last checkout command
(setq last-co-comment "")
;; Comment given for last checkin command
(setq last-ci-comment "")

(setq v-name "")
(defun checkout-curr (comment)
    "Checkout version in current buffer with COMMENT."
    (interactive (list (read-string "Comment: " last-co-comment)))
    (setq last-co-comment comment)
    (setq pname (file-name-nondirectory (buffer-file-name)))
    (setq option "-unreserved")
    (setq choice
	  (read-string "Reserved: r or Unreserved: u: " ))
    (cond
     ((equal choice "u")
      (setq option "-unreserved"))
     ((equal choice "r")
      (setq option "-reserved"))
     (t
      (message (concat "Unrecognized choice: using default " option "."))
      )
     )
    (shell-command-verbose
     (concat "cleartool checkout -c " (quote-string comment) " " option "  " pname))
    (find-curr-file-again nil)
)

(defun uncheckout-curr ()
    "Uncheckout version in current buffer and remove private data."
    (interactive)
    (setq pname (file-name-nondirectory (buffer-file-name)))
    (shell-command-verbose (concat "cleartool uncheckout " pname))
    (find-curr-file-again t)
)

(defun checkin-curr ()
    "Checkin version in current buffer."
    (interactive)
    (setq pname (file-name-nondirectory (buffer-file-name)))
    (setq option nil)
    (while (not option)
        (setq choice
	      (read-string "Comment: s (same) or n (new) or l (list): " "s"))
	(cond
	    ((equal choice "s")
		(setq option "-nc"))
	    ((equal choice "n")
	        (setq comment (read-string "Comment: " last-ci-comment))
		(setq last-ci-comment comment)
		(setq option (concat "-c " (quote-string comment))))
	    ((equal choice "l")
		(shell-command-verbose
	            (concat "cleartool lscheckout " pname)))
	    (t
		(message (concat "Unrecognized choice: " choice "."))
		(sleep-for 2))
	)
    )
    (shell-command-verbose
	(concat "cleartool checkin " option " " pname))
    (find-curr-file-again t)
)

(defun lscheckout-curr ()
    "List checkout for the current buffer."
    (interactive)
    (setq pname (file-name-nondirectory (buffer-file-name)))
    (shell-command-verbose (concat "cleartool lscheckout -l " pname))
)

(defun lscheckout-curr-dir ()
    "List checkouts for the current directory."
    (interactive)
    (shell-command-verbose "cleartool lscheckout")
)

(defun shell-command-verbose (command)
    "Execute COMMAND in shell with message."
    (interactive "sCommand: \n")
    (message (concat "Executing: " command " ..."))
    (shell-command command)
    (message "Done")
)

(defun find-curr-file-again (read-only)
    "Read in the current file again, READONLY (t) or not (nil)."
    (setq pname (buffer-file-name))
    (setq linenum (1+ (count-lines 1 (point))))
    (kill-buffer (buffer-name))
    (if read-only
	(find-file-read-only pname)
        (find-file pname))
    (goto-line linenum)
)

(defun quote-string (string)
    "Enclose STRING in single or double quotes."
    (setq has-double (string-match "\"" string))
    (setq has-single (string-match "'" string))
    (cond
	((or (and (not has-single) (not has-double))
	     (and has-double (not has-single)))
	    (concat "'" string "'"))
	((and has-single (not has-double))
	    (concat "\"" string "\""))
	(t
	    (message (concat "Can't quote string correctly: " string))
	    (sleep-for 3)
	    (concat "\"" string "\""))
    )
)
