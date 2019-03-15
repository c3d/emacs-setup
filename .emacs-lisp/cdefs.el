(provide 'cdefs)

;;;
;;;
;;;  C-mode defaults
;;;
;;;

;;; Style definitions 
(c-add-style
 "ddd"
 '((c-basic-offset . 4)
   (c-comment-only-line-offset . 0)
   (c-offsets-alist . ((statement-block-intro . +)
                       (knr-argdecl-intro . +)
                       (substatement-open . 0)
                       (substatement-label . 0)
                       (label . 0)
                       (statement-cont . +)
                       (inline-open . 0)
                       (inexpr-class . 0)
                       (innamespace . 0)
                       ))))

;;; Style-independent settings
(setq c-tab-always-indent t)
(setq c-auto-newline nil)

;;; Reindent the right part of a C #define
(defun c-indent-define ()
  "For a #define line, indent right part on the comment column."
  (interactive "*")
  (beginning-of-line 1)
  (if (null comment-start)
      (error "No comment syntax defined")
    (let* ((eolpos (save-excursion (end-of-line) (point)))
           cpos indent begpos)
      (if (re-search-forward 
           "#[ \t]*define[ \t]+[A-Za-z0-9_]+\\([ \t]+\\).+"
           eolpos 'move)
	  (progn (setq cpos (point-marker))
		 ;; Find the start of the comment delimiter.
		 ;; If there were paren-pairs in comment-start-skip,
		 ;; position at the end of the first pair.
		 (if (match-end 1)
		     (goto-char (match-end 1))
		   ;; If comment-start-skip matched a string with
		   ;; internal whitespace (not final whitespace) then
		   ;; the delimiter start at the end of that
		   ;; whitespace.  Otherwise, it starts at the
		   ;; beginning of what was matched.
                   (error "#define has no body or malformed body")))
        (error "This does not look like a #define"))
      (setq begpos (point))
      ;; Compute desired indent.
      ;; Do not call comment-indent-function but simply comment-column
      ;; because we don't want auto-alignment on previous comment...
      (if (= (current-column)
	     (setq indent comment-column))
	  (goto-char begpos)
	;; If that's different from current, change it.
	(skip-chars-backward " \t")
	(delete-region (point) begpos)
	(indent-to indent)))))


;;; Allow interactive or direct change of style
; (defun set-c-style (style &optional global)
;   "Set C-mode variables to use one of several different indentation styles.
; The arguments are a string representing the desired style
; and a flag which, if non-nil, means to set the style globally.
; \(Interactively, the flag comes from the prefix argument.)
; Available styles are GNU, K&R, BSD and Whitesmith."
;   (interactive (list (let ((completion-ignore-case t))
; 		       (completing-read "Use which C indentation style? "
; 					c-style-alist nil t))
; 		     current-prefix-arg))
;   (let ((vars (cdr (assoc style c-style-alist))))
;     (or vars
; 	(error "Invalid C indentation style `%s'" style))
;     (while vars
;       (or global
; 	  (make-local-variable (car (car vars))))
;       (set (car (car vars)) (cdr (car vars)))
;       (setq vars (cdr vars)))))

(defun set-tab-width-to-2 ()
  "Set tab width for current buffer to 2"
  (interactive)
  (setq tab-width 2))
(defun set-tab-width-to-3 ()
  "Set tab width for current buffer to 3"
  (interactive)
  (setq tab-width 3))
(defun set-tab-width-to-4 ()
  "Set tab width for current buffer to 4"
  (interactive)0
  (setq tab-width 4))
(defun set-tab-width-to-8 ()
  "Set tab width for current buffer to 8"
  (interactive)
  (setq tab-width 8))

;;; Use cplus-md1 to have a mode that is compatible with both C and C++
;; (autoload 'c++-c-mode "cplus-md1")
;;(autoload 'c++-mode "cplus-md")
;;(autoload 'c-mode "c-mode")

