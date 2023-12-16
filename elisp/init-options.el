;;; init-options.el -*- lexical-binding: t -*-

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

;; Space-based indentation, define tab character breadth.
(setq-default indent-tabs-mode nil
              tab-width 2
              tab-always-indent 'complete)

(setq custom-file (concat user-emacs-directory "custom.el")
      ring-bell-function 'ignore
      make-backup-files nil
      auto-save-default nil
      create-lockfiles nil
      hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 10
      scroll-margin 0
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2
      use-short-answers t
      enable-recursive-minibuffers t
      mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none
      url-proxy-services
            '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
              ("http" . "127.0.0.1:7890")
              ("https" . "127.0.0.1:7890"))
      find-file-visit-truename t
      vc-follow-symlinks t
      confirm-nonexistent-file-or-buffer nil
      enable-recursive-minibuffers t
      dired-use-ls-dired nil)

(provide 'init-options)
;;; init-options.el ends here
