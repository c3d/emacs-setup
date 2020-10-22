(provide 'defaults)

(setq load-path (append '("~/.emacs-lisp") load-path))

(require 'keydefs)
(require 'findgrep)
(require 'comments)
;;(require 'xl)

;;; Use DDD's c-style
(require 'cdefs)
;; (c-set-style "user")

;;; Take advantage of some nice Emacs-19 features such as :
;;; auto-load hilite and paren
(progn
  (load-library "paren")
  (standard-display-8bit 128 255)
  (line-number-mode t))

;;; We are grown-ups now ...
(put 'eval-expression 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq dired-listing-switches "-lot")

(setq ps-lpr-command "/usr/bin/lp")
(setq ps-lpr-switches '("-opostscript" "-dljmx3"))

; Keep more than on backup file ... but don't keep every file
(setq version-control t)
(setq kept-old-versions 2)
(setq kept-new-versions 2)
(setq delete-old-versions t)

;;; Insert only spaces when indenting (best for the PC)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)
(setq indent-tabs-mode nil)

;;; Some of us are sometimes lasy ...
(setq require-final-newline t)

;;; This team uses CVS ...
(setq vc-initial-comment t)

;;; Mapped on C-c p :
(defun marque-precedente ()
  (interactive)
  (pop-mark)
  (exchange-point-and-mark))

(setq fortran-continuation-char ?|)
(setq fortran-do-indent 2)
(setq fortran-if-indent 2)
(setq fortran-continuation-indent 2)

(setq fortran-comment-line-column 1)
(setq fortran-comment-indent-style ())
(setq fortran-comment-indent-char ? )
(setq fortran-comment-region "C | ")

; We might still have to work with that bloody shit ...
(autoload 'wave-mode "wave"
  "Major mode for edition of WAVE .pro files" t)

(setq wave-mode-hook
      (function (lambda ()
                  (setq wave-block-indent 2
                        wave-continuation-indent 4
                        wave-minimum-statement-indent 0
                        wave-newline-and-indent t
                        wave-surround-by-blank t))))

;;; Load modes
(setq auto-mode-alist
      (append '(("\\.C$"    . c++-mode)
		("\\.cc$"   . c++-mode)
		("\\.cpp$"  . c++-mode)
		("\\.cp$"   . c++-mode)
		("\\.c$"    . c++-mode)
		("\\.h$"    . c++-mode)
		("\\.y$"    . c++-mode)
		("\\.tbl$"  . c++-mode)
                ("\\.adb"   . ada-mode)
                ("\\.ads"   . ada-mode)
                ("\\.pro$"  . wave-mode)
                ("\\.fs$"  . c-mode)
                ("\\.vs$"  . c-mode)
                ("\\.ax$"   . lisp-mode)
                ("\\.s$"  . ia64-asm-mode)
                ("\\.S$"  . ia64-asm-mode)
                ("\\.syntax$" . xl-mode)
                ("\\.bytecode$" . xl-mode)
                ("\\.stylesheet$" . xl-mode)
                ("\\.pro$" . shell-script-mode)
                ("\\.ddd$" . xl-mode)
                ("\\.dds$" . xl-mode)
                ("\\.ddt$" . xl-mode)
                ("Makefile.new" . makefile-mode))
	      auto-mode-alist))

;;; Add some fun
(setq colors-list '(("Black" "#C0C0DA")
                    ("Black" "#E0E5FF")
                    ("Black" "#D0C0EE")
                    ("Black" "#F0EEDA")
                    ("Black" "#E0EEC0")
                    ("Black" "#C0DAEE")
                    ("Black" "#B0DAC0")
                    ("Black" "AntiqueWhite")
                    ("Black" "LightSkyBlue")
                    ("Black" "PaleGreen1")
                    ("Black" "LightPink")
                    ("Black" "SeaGreen1")
                    ("Black" "LightSalmon")
                    ("Black" "LightSeaGreen")
                    ("Black" "LightSteelBlue")
                    ("Black" "FloralWhite")
                    ("Black" "BlanchedAlmond")
                    ("Black" "LightGoldenrod")
                    ("Black" "LightCyan")
                    ("Black" "LavenderBlush")
                    ("Black" "LightYellow")
                    ("Black" "PapayaWhip")
                    ("Black" "PeachPuff")
                    ("Black" "White")))

(defun cycle-colors ()
  (interactive)
  (set-foreground-color (car (car colors-list)))
  (set-background-color (car (cdr (car colors-list))))
  (setq colors-list (append (cdr colors-list)
                            (list (car colors-list)))))
