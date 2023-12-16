;;; init-vc.el -*- lexical-binding: t -*-

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

(require 'magit)

(add-hook 'magit-process-mode-hook #'goto-address-mode)

(require 'fullframe)

(with-eval-after-load 'magit
  (fullframe magit-status magit-mode-quit-window))

(setq magit-diff-refine-hunk t
      ;; Show word-granularity differences within diff hunks.
      magit-save-repository-buffers nil
      ;; Don't autosave repo buffers. This is too magical.
      magit-revision-insert-related-refs nil
      ;; Don't display parent/related refs in commit buffers; they are rarely helpful and only add to runtime costs.
      )

;; Exterminate Magit buffers
(defun +magit-kill-buffers (&rest _)
    "Restore window configuration and kill all Magit buffers."
    (interactive)
    (magit-restore-window-configuration)
    (let ((buffers (magit-mode-get-buffers)))
      (when (eq major-mode 'magit-status-mode)
        (mapc (lambda (buf)
                (with-current-buffer buf
                  (if (and magit-this-process
                           (eq (process-status magit-this-process) 'run))
                      (bury-buffer buf)
                    (kill-buffer buf))))
              buffers))))

(setq magit-bury-buffer-function #'+magit-kill-buffers)

(provide 'init-vc)
