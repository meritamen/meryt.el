;;; init-langs.el -*- lexical-binding: t -*-

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

;;; Code:

(defvar inferior-lisp-program "sbcl")

(require 'sly)

(setq sly-mrepl-history-file-name "~/.sly-mrepl-history"
      sly-kill-without-query-p t
      sly-net-coding-system 'utf-8-unix)

;;; source: https://github.com/doomemacs/doomemacs/blob/master/modules/lang/common-lisp/config.el#60L
(defun +common-lisp--cleanup-sly-maybe-h ()
    "Kill processes and leftover buffers when killing the last sly buffer."
    (unless (cl-loop for buf in (delq (current-buffer) (buffer-list))
                     if (and (buffer-local-value 'sly-mode buf)
                             (get-buffer-window buf))
                     return t)
      (dolist (conn (sly--purge-connections))
        (sly-quit-lisp-internal conn 'sly-quit-sentinel t))
      (let (kill-buffer-hook kill-buffer-query-functions)
        (mapc #'kill-buffer
              (cl-loop for buf in (delq (current-buffer) (buffer-list))
                       if (buffer-local-value 'sly-mode buf)
                       collect buf)))))

(defun merita-temp-buffer-p (buf)
  "Returns non-nil if BUF is temporary."
  (equal (substring (buffer-name buf) 0 1) " "))

(defun +common-lisp-init-sly-h ()
      "Attempt to auto-start sly when opening a lisp buffer."
      (cond ((or (merita-temp-buffer-p (current-buffer))
                 (sly-connected-p)))
            ((executable-find (car (split-string inferior-lisp-program)))
             (let ((sly-auto-start 'always))
               (sly-auto-start)
               (add-hook 'kill-buffer-hook #'+common-lisp--cleanup-sly-maybe-h nil t)))
            ((message "WARNING: Couldn't find `inferior-lisp-program' (%s)"
                      inferior-lisp-program))))

(add-hook 'sly-mode-hook #'+common-lisp-init-sly-h)

(require 'paredit)

(add-hook 'lisp-mode-hook #'paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'common-lisp-mode-hook #'paredit-mode)

(provide 'init-langs)
;;; init-langs.el ends here
