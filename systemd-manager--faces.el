;;; systemd-manager--faces.el --- Systemd faces and colorization -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Face definitions and colorization functions for systemd package.

;;; Code:

(defface systemd-manager--active-face
  '((t :foreground "medium spring green"))
  "Face for active systemd units.")

(defface systemd-manager--inactive-face
  '((t :foreground "gray"))
  "Face for inactive systemd units.")

(defface systemd-manager--failed-face
  '((t :foreground "orange red" :weight bold))
  "Face for failed systemd units.")

(defface systemd-manager--activating-face
  '((t :foreground "yellow"))
  "Face for activating systemd units.")

(defface systemd-manager--deactivating-face
  '((t :foreground "orange"))
  "Face for deactivating systemd units.")

(defface systemd-manager--reloading-face
  '((t :foreground "cyan"))
  "Face for reloading systemd units.")

(defun systemd-manager--colorize-active-state (state)
  "Apply color coding to systemd active STATE."
  (cond
   ((string= state "active")
    (propertize state 'face 'systemd-manager--active-face))
   ((string= state "inactive")
    (propertize state 'face 'systemd-manager--inactive-face))
   ((string= state "failed")
    (propertize state 'face 'systemd-manager--failed-face))
   ((string= state "activating")
    (propertize state 'face 'systemd-manager--activating-face))
   ((string= state "deactivating")
    (propertize state 'face 'systemd-manager--deactivating-face))
   ((string= state "reloading")
    (propertize state 'face 'systemd-manager--reloading-face))
   (t state)))

(defface systemd-manager--loaded-face
  '((t :foreground "deep sky blue"))
  "Face for loaded systemd units.")

(defface systemd-manager--not-loaded-face
  '((t :foreground "gray"))
  "Face for not-loaded systemd units.")

(defun systemd-manager--colorize-load-state (state)
  "Apply color coding to systemd load STATE."
  (cond
   ((string= state "loaded")
    (propertize state 'face 'systemd-manager--loaded-face))
   ((string= state "not-loaded")
    (propertize state 'face 'systemd-manager--not-loaded-face))
   (t state)))

(provide 'systemd-manager--faces)
;;; systemd-manager--faces.el ends here
