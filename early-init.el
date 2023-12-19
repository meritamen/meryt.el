;;; early-init.el -*- lexical-binding: t -*-

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

;; This file is loaded before the package system and GUI is initialized, so
;; in it you can customize variables that affect the package initialization process.

;;; Code:

(setq ;; Don't make installed packages available before loading the init.el file.
      package-enable-at-startup nil
      ;; Adjust garbage collection thresholds and maximize portion of the heap
      ;; used for allocationduring Emacs startup for perf.
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1.0
      ;; Increase how much is read from processes in a single chunk (default is 4kb).
      ;; This is further increased elsewhere, where needed (like our LSP module).
      read-process-output-max (* 64 1024)  ; 64kb
      ;; In noninteractive sessions, prioritize .el file. It saves IO time.
      load-prefer-newer noninteractive

      ;; Disable mouse scrolling.
      mouse-wheel-down-event nil
      mouse-wheel-up-event nil)

;; early-init.el ends here
