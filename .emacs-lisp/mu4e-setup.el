;==============================================================================
;
;  mu4e configuration
;
;==============================================================================

(require 'mu4e)
(require 'mu4e-actions)
(setq mu4e-maildir "~/Maildir/")
(setq mu4e-drafts-folder "/GMail/[Gmail].Drafts")
(setq mu4e-sent-folder   "/GMail/[Gmail].Sent Mail")
;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)
;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; shortcuts
(setq mu4e-maildir-shortcuts
    '( ("/INBOX"               . ?i)
       ("/[Gmail].All Mail"    . ?a)
       ("/[Gmail].Sent Mail"   . ?s)))

;; something about ourselves
(setq
   user-mail-address "christophe@dinechin.org"
   user-full-name  "Christophe de Dinechin"
   mu4e-compose-signature
    (concat
      "Cheers,\n"
      "Christophe de Dinechin (IRC c3d)\n"))

;; show images
(setq mu4e-show-images t)

;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; convert html emails properly
;; Possible options:
;;   - html2text -utf8 -width 72
;;   - textutil -stdin -format html -convert txt -stdout
;;   - html2markdown | grep -v '&nbsp_place_holder;' (Requires html2text pypi)
;;   - w3m -dump -cols 80 -T text/html
;;   - view in browser (provided below)
(setq mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout")

;; spell check
(add-hook 'mu4e-compose-mode-hook
        (defun my-do-compose-stuff ()
           "My settings for message composition."
           (set-fill-column 72)
           (flyspell-mode)))

;; add option to view html message in a browser
;; `aV` in view to activate
(add-to-list 'mu4e-view-actions
  '("ViewInBrowser" . mu4e-action-view-in-browser) t)
(add-to-list 'mu4e-view-actions
  '("Patch" . mu4e-action-git-apply-mbox) t)

;; define 'P' (the first letter of the description) as the shortcut
;; the 't' argument to add-to-list puts it at the end of the list
(add-to-list 'mu4e-headers-actions
  '("Patch" . mu4e-action-git-apply-mbox) t)


;; fetch mail every 5 minutes
(setq mu4e-update-interval 300)

;; No stinking duplicates
(setq mu4e-headers-skip-duplicates t)


;------------------------------------------------------------------------------
;; configuration for sending mail
;------------------------------------------------------------------------------
(setq mu4e-sent-folder "/GMail/[Gmail].Sent Mail"
      mu4e-drafts-folder "/GMail/[Gmail].Drafts"
      message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

(defvar mu4e-account-alist
  '(("GMail"
     (mu4e-sent-folder "/GMail/[Gmail].Sent Mail")
     (mu4e-drafts-folder "/GMail/[Gmail].Drafts")
     (user-mail-address "christophe@dinechin.org")
     (smtpmail-smtp-user "christophe.de.dinechin")
     (smtpmail-default-smtp-server "smtp.gmail.com")
     (smtpmail-smtp-server "smtp.gmail.com")
     (smtpmail-stream-type starttls)
     (smtpmail-smtp-service 587))
    ("RedHat"
     (mu4e-sent-folder "/RedHat/Sent")
     (mu4e-drafts-folder "/RedHat/Drafts")
     (user-mail-address "dinechin@redhat.com")
     (smtpmail-smtp-user "cdupontd")
     (smtpmail-default-smtp-server "smtp.redhat.com")
     (smtpmail-smtp-server "smtp.redhat.com")
     (smtpmail-stream-type starttls)
     (smtpmail-smtp-service 587))))

(defun mu4e-set-account ()
  "Set the account for composing a message."
  (interactive)
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) mu4e-account-alist)
                             nil t nil nil (caar mu4e-account-alist))))
         (account-vars (cdr (assoc account mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))

(add-hook 'mu4e-compose-pre-hook 'mu4e-set-account)

(add-to-list 'mu4e-bookmarks
  (make-mu4e-bookmark
    :name  "spice"
    :query "list:spice.*"
    :key ?s))
(add-to-list 'mu4e-bookmarks
  (make-mu4e-bookmark
    :name  "kvm"
    :query "list:kvm.*"
    :key ?k))
(add-to-list 'mu4e-bookmarks
  (make-mu4e-bookmark
    :name  "Flagged"
    :query "flag:flagged"
    :key ?f))
(add-to-list 'mu4e-bookmarks
  (make-mu4e-bookmark
    :name  "Just me"
    :query "NOT flag:list AND NOT flag:draft"
    :key ?j))
