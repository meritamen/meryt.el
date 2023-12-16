;;; init-minibuffer.el -*- lexical-binding: t -*-

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

(require 'marginalia)

(setq marginalia-max-relative-age 0
      marginalia-align 'right)

(define-key minibuffer-local-map (kbd "M-A") 'marginalia-cycle)
(add-hook 'after-init-hook #'marginalia-mode)

(require 'vertico)

(add-to-list 'load-path (expand-file-name "site-lisp/vertico/extensions" user-emacs-directory))

(require 'vertico-indexed)
(require 'vertico-flat)
(require 'vertico-grid)
(require 'vertico-mouse)
(require 'vertico-quick)
(require 'vertico-buffer)
(require 'vertico-repeat)
(require 'vertico-reverse)
(require 'vertico-directory)
(require 'vertico-multiform)
(require 'vertico-unobtrusive)

(add-hook 'rfn-eshadow-update-overlay #'vertico-directory-tidy)
(add-hook 'minibuffer-setup #'vertico-repeat-save)

(setq vertico-count 13
      vertico-resize t
      vertico-cycle t
      ;; Extensions
      vertico-grid-separator "       "
      vertico-grid-lookahead 50
      vertico-buffer-display-action '(display-buffer-reuse-window)
      vertico-multiform-categories
       '((file reverse)
         (consult-grep buffer)
         (consult-location)
         (imenu buffer)
         (library reverse indexed)
         (org-roam-node reverse indexed)
         (t reverse))
      vertico-multiform-commands
       '(("flyspell-correct-*" grid reverse)
         (org-refile grid reverse indexed)
         (consult-yank-pop indexed)
         (consult-flycheck)
         (consult-lsp-diagnostics)))

(defun merita/vertico-multiform-flat-toggle ()
  "Toggle between flat and reverse."
  (interactive)
  (vertico-multiform--display-toggle 'vertico-flat-mode)
  (if vertico-flat-mode
      (vertico-multiform--temporary-mode 'vertico-reverse-mode -1)
    (vertico-multiform--temporary-mode 'vertico-reverse-mode 1)))

(defun merita/vertico-quick-embark (&optional arg)
  "Embark on candidate using quick keys."
  (interactive)
  (when (vertico-quick-jump)
    (embark-act arg)))

;; Workaround for problem with `tramp' hostname completions. This overrides
;; the completion style specifically for remote files! See
;; https://github.com/minad/vertico#tramp-hostname-completion
(defun merita/basic-remote-try-completion (string table pred point)
  (and (vertico--remote-p string)
       (completion-basic-try-completion string table pred point)))

(defun merita/basic-remote-all-completions (string table pred point)
  (and (vertico--remote-p string)
       (completion-basic-all-completions string table pred point)))

(add-to-list 'completion-styles-alist
             '(basic-remote           ; Name of `completion-style'
               merita/basic-remote-try-completion merita/basic-remote-all-completions nil))

(add-hook 'after-init-hook #'vertico-mode)

;; Extensions
(add-hook 'after-init-hook #'vertico-multiform-mode)
;; Tidy shadowed file names
(add-hook 'rfn-eshadow-update-overlay 'vertico-directory-tidy)

(define-key vertico-map (kbd "RET") 'vertico-directory-enter)
(define-key vertico-map (kbd "DEL") 'vertico-directory-delete-char)
(define-key vertico-map (kbd "M-DEL") 'vertico-directory-delete-word)

;; Prefix the current candidate with “» ”. From
;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
(advice-add #'vertico--format-candidate :around
            (lambda (orig cand prefix suffix index _start)
              (setq cand (funcall orig cand prefix suffix index _start))
              (concat
               (if (= vertico--index index)
                   (propertize "» " 'face 'vertico-current)
                 "  ")
               cand)))

(require 'orderless)

(setq completion-styles '(orderless)
      completion-category-defaults nil    ; I want to be in control!
      completion-category-overrides
        '((file (styles basic-remote ; For `tramp' hostname completion with `vertico'
                        orderless)))
      orderless-component-separator 'orderless-escapable-split-on-space
      orderless-matching-styles
        '(orderless-literal
          orderless-prefixes
          orderless-initialism
          orderless-regexp)
          ;; orderless-flex
          ;; orderless-strict-leading-initialism
          ;; orderless-strict-initialism
          ;; orderless-strict-full-initialism
          ;; orderless-without-literal          ; Recommended for dispatches instead

      orderless-style-dispatchers
        '(merita/orderless-literal-dispatcher
          merita/orderless-strict-initialism-dispatcher
          merita/orderless-flex-dispatcher))

(defun orderless--strict-*-initialism (component &optional anchored)
  "Match a COMPONENT as a strict initialism, optionally ANCHORED.
   The characters in COMPONENT must occur in the candidate in that
   order at the beginning of subsequent words comprised of letters.
   Only non-letters can be in between the words that start with the
   initials.

   If ANCHORED is `start' require that the first initial appear in
   the first word of the candidate.  If ANCHORED is `both' require
   that the first and last initials appear in the first and last
   words of the candidate, respectively."
  (orderless--separated-by
   '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)))
   (cl-loop for char across component collect `(seq word-start ,char))
   (when anchored '(seq (group buffer-start) (zero-or-more (not alpha))))
   (when (eq anchored 'both)
     '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)) eol))))

(defun orderless-strict-initialism (component)
  "Match a COMPONENT as a strict initialism.
   This means the characters in COMPONENT must occur in the
   candidate in that order at the beginning of subsequent words
   comprised of letters.  Only non-letters can be in between the
   words that start with the initials."
  (orderless--strict-*-initialism component))

(defun merita/orderless-literal-dispatcher (pattern _index _total)
  "Literal style dispatcher using the equals sign as a suffix.
   It matches PATTERN _INDEX and _TOTAL according to how Orderless
   parses its input."
  (when (string-suffix-p "=" pattern)
    `(orderless-literal . ,(substring pattern 0 -1))))

(defun merita/orderless-strict-initialism-dispatcher (pattern _index _total)
  "Leading initialism  dispatcher using the comma suffix.
   It matches PATTERN _INDEX and _TOTAL according to how Orderless
   parses its input."
  (when (string-suffix-p "," pattern)
    `(orderless-strict-initialism . ,(substring pattern 0 -1))))

(defun merita/orderless-flex-dispatcher (pattern _index _total)
  "Flex  dispatcher using the tilde suffix.
   It matches PATTERN _INDEX and _TOTAL according to how Orderless
   parses its input."
  (when (string-suffix-p "." pattern)
    `(orderless-flex . ,(substring pattern 0 -1))))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
