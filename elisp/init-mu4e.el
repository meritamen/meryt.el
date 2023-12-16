;;; init-mu4e.el -*- lexical-binding: t -*-

;; Copyright (c) 2023 Meritamen <meritamen@sdf.org>

;; Author: Meritamen <meritamen@sdf.org>
;; URL: https://github.com/meritamen/meritaemacs

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; As i using `borg` as package manager, emacs complained when i built
;; packages encountering `erc-sasl`, thus i disable byte-compile and it
;; eventually worked :)

;;; Code:

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e/")

(require 'mu4e)
(require 'smtpmail)

(setq mu4e-mu-binary (executable-find "mu")
      mu4e-maildir "~/.maildir"
      mu4e-get-mail-command (concat (executable-find "mbsync") " -a")
      mu4e-update-interval 300
      mu4e-attachment-dir "~/Desktop"
      mu4e-change-filenames-when-moving t
      mu4e-user-mail-address-list '("meritamen@sdf.org")
      mu4e-contexts
      `(,(make-mu4e-context
          :name "sdf"
          :enter-func
          (lambda () (mu4e-message "Enter meritamen@sdf.org context"))
          :leave-func
          (lambda () (mu4e-message "Leave meritamen@sdf.org context"))
          :match-func
          (lambda (msg)
            (when msg
              (mu4e-message-contact-field-matches msg
                                                  :to "meritamen@sdf.org")))
          :vars '((user-mail-address . "meritamen@sdf.org")
                  (user-full-name . "meritamen")
                  (mu4e-drafts-folder . "/sdf/Drafts")
                  (mu4e-refile-folder . "/sdf/Archive")
                  (mu4e-sent-folder . "/sdf/Sent")
                  (mu4e-trash-folder . "/sdf/Trash"))))
      mu4e-context-policy 'pick-first ;; start with the first (default) context;
      mu4e-compose-context-policy 'ask ;; ask for context if no context matches;
      smtpmail-smtp-server "mx.sdf.org"
      smtpmail-stream-type 'starttls
      smtpmail-smtp-service 587
      sendmail-program "/usr/local/bin/msmtp"
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'message-send-mail-with-sendmail)

(provide 'init-mu4e)
;;; init-mu4e.el ends here
