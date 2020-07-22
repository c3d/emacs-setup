;==============================================================================
;
;  mu4e configuration
;
;==============================================================================

(setq load-path (append '("~/Work/mu/mu4e") load-path))

(require 'mu4e)
(require 'mu4e-actions)
(setq mu4e-maildir "~/Maildir/")
(setq mu4e-drafts-folder "/gmail/Drafts")
(setq mu4e-sent-folder   "/gmail/Sent")
;; don't save message to Sent Messages, Gmail/IMAP takes care of this
;; (setq mu4e-sent-messages-behavior 'delete)
;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "mbsync -q all")
(setq  message-citation-line-format "On %Y-%m-%d at %R %Z, %f wrote..."
       message-citation-line-function 'message-insert-formatted-citation-line)


;; shortcuts
(setq mu4e-maildir-shortcuts
    '( ("/gmail/Inbox"                  . ?I)
       ("/gmail/Sent"                   . ?S)
       ("/rh/Act"                       . ?a)
       ("/rh/Act/Act Now"               . ?n)
       ("/rh/Act/Keep"                  . ?k)
       ("/rh/Act/Review"                . ?r)
       ("/rh/Act/Travel"                . ?t)
       ("/rh/Inbox"                     . ?i)
       ("/rh/Sent"                      . ?s)
       ("/rh/Bugzillas"                 . ?b)
       ("/rh/Dev/Fedora notifications"  . ?f)
       ("/rh/Dev/virt-devel"            . ?v)
       ("/rh/Dev/qemu-devel"            . ?q)
       ("/rh/WIP/Patches"               . ?p)
       ("/rh/WIP/Pull"                  . ?l)
       ("/rh/WIP/RFC"                   . ?c)
))

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
;; (setq mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout")
(setq mu4e-html2text-command
      "w3m -dump -T text/html -O utf8 -cols 80 -o display_link_number=true")

;; spell check
(add-hook 'mu4e-compose-mode-hook
        (defun ddd-mu4e-compose-stuff ()
           "My settings for message composition."
           (set-fill-column 76)
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

;; Various common settings
(setq
 mu4e-update-interval           421     ;; Roughly 7 minutes
 mu4e-headers-skip-duplicates   t       ;; Don't record duplicates
 mu4e-index-cleanup             nil     ;; Don't do a full cleanup check
 mu4e-index-lazy-check          t)      ;; Don't consider up-to-date dirs


;------------------------------------------------------------------------------
;; configuration for sending mail
;------------------------------------------------------------------------------
(setq mu4e-sent-folder "/gmail/Sent"
      mu4e-drafts-folder "/gmail/Drafts"
      message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

(defvar mu4e-account-alist
  '(("gmail"
     (mu4e-sent-folder "/gmail/Sent")
     (mu4e-drafts-folder "/gmail/Drafts")
     (user-mail-address "christophe@dinechin.org")
     (smtpmail-smtp-user "christophe.de.dinechin")
     (smtpmail-default-smtp-server "smtp.gmail.com")
     (smtpmail-smtp-server "smtp.gmail.com")
     (smtpmail-stream-type starttls)
     (smtpmail-smtp-service 587))
    ("rh"
     (mu4e-sent-folder "/rh/Sent")
     (mu4e-drafts-folder "/rh/Drafts")
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
    :name  "Hot topics"
    :query "subject:kata OR list:kvm-containers-list OR maildir:/Act"
    :key ?h)
  (make-mu4e-bookmark
    :name  "kvm"
    :query "list:kvm*"
    :key ?k))
(add-to-list 'mu4e-bookmarks
  (make-mu4e-bookmark
    :name  "qemu"
    :query "list:qemu*"
    :key ?q))
(add-to-list 'mu4e-bookmarks
  (make-mu4e-bookmark
    :name  "Flagged"
    :query "flag:flagged"
    :key ?f))
(add-to-list 'mu4e-bookmarks
  (make-mu4e-bookmark
    :name  "Virt"
    :query "list:virt*"
    :key ?v))
(add-to-list 'mu4e-bookmarks
  (make-mu4e-bookmark
    :name  "Work"
    :query "list:virt* OR list:qemu* OR list:kvm* OR list:kata* OR subject:kata OR subject:qemu OR maildir:/Act"
    :key ?o))
(add-to-list 'mu4e-bookmarks
  (make-mu4e-bookmark
    :name  "Just me"
    :query "(to:dinechin@redhat.com OR to:cdupontd@redhat.com OR to:christophe@dinechin.org) AND NOT flag:draft"
    :key ?j))

(setq mu4e-view-fields '(:from :to  :cc :subject :flags :date :maildir
                               :mailing-list :tags :attachments
                               :signature :decryption :message-id)
      mu4e-headers-fields '((:flags         . 8)
                            (:human-date    . 12)
                                        ;(:acctshortname . 4)
                            (:mailing-list  . 15)
                            (:from-or-to    . 25)
                                        ;(:size          . 6)
                            (:subject       . nil)))
