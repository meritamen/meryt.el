;;; init-irc.el -*- lexical-binding: t -*-

;; Copyright (c) 2023 Meritamen <meritamen@sdf.org>

;; Author: Meritamen <meritamen@sdf.org>
;; URL: https://github.com/meritamen/.emacs.d

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

(require 'erc)

(setq erc-nick "meritamen"
      erc-nick-uniquifier "_"
      erc-server-auto-reconnect t
      erc-server-reconnect-timeout 15
      erc-autojoin-channels-alist
      '(("libera.chat" "#emacs" "#commonlisp")))

(require 'erc-sasl)

;; Add SASL server to list of SASL servers (start a new list, if it did not exist)
(add-to-list 'erc-sasl-server-regexp-list "irc\\.libera\\.chat")

;; Redefine/Override the erc-login() function from the erc package, so that
;; it now uses SASL
(defun erc-login ()
  "Perform user authentication at the IRC server. (PATCHED)"
  (erc-log (format "login: nick: %s, user: %s %s %s :%s"
           (erc-current-nick)
           (user-login-name)
           (or erc-system-name (system-name))
           erc-session-server
           erc-session-user-full-name))
  (if erc-session-password
      (erc-server-send (format "PASS %s" erc-session-password))
    (message "Logging in without password"))
  (when (and (featurep 'erc-sasl) (erc-sasl-use-sasl-p))
    (erc-server-send "CAP REQ :sasl"))
  (erc-server-send (format "NICK %s" (erc-current-nick)))
  (erc-server-send
   (format "USER %s %s %s :%s"
       ;; hacked - S.B.
       (if erc-anonymous-login erc-email-userid (user-login-name))
       "0" "*"
       erc-session-user-full-name))
  (erc-update-mode-line))

;; Save logs when leaving a channel.
(add-to-list 'erc-modules 'log)

(setq erc-save-buffer-on-part t
      erc-log-channels-directory "~/.erc/logs")

(erc-update-modules)

(provide 'init-irc)
;;; init-irc.el ends here
