;;; init.el -*- lexical-binding: t -*-

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

(defun load-all-environment-variables ()
    "Load all environment variables from the user's shell."
    (let ((shell-env (shell-command-to-string "env")))
      (dolist (var (split-string shell-env "\n"))
        (when (string-match "\\([^=]+\\)=\\(.*\\)" var)
          (let ((name (match-string 1 var))
                (value (match-string 2 var)))
            (setenv name value))))))

(add-hook 'after-init-hook #'load-all-environment-variables)

(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

(eval-and-compile ; `borg'
  (add-to-list 'load-path (expand-file-name "site-lisp/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize))

(require 'gcmh)
(setq gcmh-idle-delay 'auto
      gcmh-auto-idle-delay-factor 10
      gcmh-high-cons-threshold #x64000000)

(add-hook 'after-init-hook #'gcmh-mode)

(require 'savehist)
(add-hook 'after-init-hook #'savehist-mode)
(setq history-length 1000
      savehist-additional-variables
        '(mark-ring
          global-mark-ring
          search-ring
          regexp-search-ring
          extended-command-history)
      savehist-autosave-interval 300)

(require 'saveplace)
(add-hook 'after-init-hook #'save-place-mode)

(require 'tramp)
(setq tramp-default-method "ssh"
      tramp-auto-save-directory (expand-file-name "tramp-autosaves/" user-emacs-directory)
      tramp-backup-directory-alist backup-directory-alist
      remote-file-name-inhibit-cache 60)

(require 'hl-line)
(add-hook 'after-init-hook #'global-hl-line-mode)

(require 'paren)
(add-hook 'after-init-hook #'show-paren-mode)

(setq show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

(with-no-warnings
    ;; Display matching line for off-screen paren.
    (defun display-line-overlay (pos str &optional face)
      "Display line at POS as STR with FACE.

     FACE defaults to inheriting from default and highlight."
      (let ((ol (save-excursion
                  (goto-char pos)
                  (make-overlay (line-beginning-position)
                                (line-end-position)))))
        (overlay-put ol 'display str)
        (overlay-put ol 'face
                     (or face '(:inherit highlight)))
        ol))

    (defvar-local show-paren--off-screen-overlay nil)
    (defun show-paren-off-screen (&rest _args)
      "Display matching line for off-screen paren."
      (when (overlayp show-paren--off-screen-overlay)
        (delete-overlay show-paren--off-screen-overlay))
      ;; Check if it's appropriate to show match info,
      (when (and (overlay-buffer show-paren--overlay)
                 (not (or cursor-in-echo-area
                          executing-kbd-macro
                          noninteractive
                          (minibufferp)
                          this-command))
                 (and (not (bobp))
                      (memq (char-syntax (char-before)) '(?\) ?\$)))
                 (= 1 (logand 1 (- (point)
                                   (save-excursion
                                     (forward-char -1)
                                     (skip-syntax-backward "/\\")
                                     (point))))))
        ;; Rebind `minibuffer-message' called by `blink-matching-open'
        ;; to handle the overlay display.
        (cl-letf (((symbol-function #'minibuffer-message)
                   (lambda (msg &rest args)
                     (let ((msg (apply #'format-message msg args)))
                       (setq show-paren--off-screen-overlay
                             (display-line-overlay
                              (window-start) msg))))))
          (blink-matching-open))))
    (advice-add #'show-paren-function :after #'show-paren-off-screen))

(require 'whitespace)
(add-hook 'before-save-hook #'whitespace-cleanup)
(add-hook 'text-mode-hook #'visual-line-mode)

(require 'dired)
(setq dired-kill-when-opening-new-dired-buffer t
      dired-auto-revert-buffer #'dired-buffer-stale-p
      dired-recursive-copies 'top
      dired-recursive-deletes 'always)

(nconc
 auto-mode-alist
 '(("/LICENSE\\'" . text-mode)
   ("\\.log\\'" . text-mode)
   ("rc\\'" . conf-mode)
   ("\\.\\(?:hex\\|nes\\)\\'" . hexl-mode)))

(require 'init-edit)
(require 'init-hydra)
(require 'init-irc)
(require 'init-langs)
(require 'init-minibuffer)
(require 'init-mu4e)
(require 'init-options)
(require 'init-vc)

(let ((custom (expand-file-name "custom.el" user-emacs-directory)))
      (when (file-exists-p custom)
        (load custom)))

;;; init.el ends here
