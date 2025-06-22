;;; systemd-manager--actions.el --- Systemd unit actions -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Unit control actions for systemd package including start, stop,
;; restart, enable, disable operations.

;;; Code:

(require 'systemd-manager--core)

(defun systemd-manager--start-unit ()
  "Start the systemd unit at point."
  (interactive)
  (let ((unit (systemd-manager--get-unit-at-point)))
    (if unit
        (systemd-manager--execute-action "start" unit)
      (message "No unit at point"))))

(defun systemd-manager--stop-unit ()
  "Stop the systemd unit at point."
  (interactive)
  (let ((unit (systemd-manager--get-unit-at-point)))
    (if unit
        (systemd-manager--execute-action "stop" unit)
      (message "No unit at point"))))

(defun systemd-manager--restart-unit ()
  "Restart the systemd unit at point."
  (interactive)
  (let ((unit (systemd-manager--get-unit-at-point)))
    (if unit
        (systemd-manager--execute-action "restart" unit)
      (message "No unit at point"))))

(defun systemd-manager--enable-unit ()
  "Enable the systemd unit at point."
  (interactive)
  (let ((unit (systemd-manager--get-unit-at-point)))
    (if unit
        (systemd-manager--execute-action "enable" unit)
      (message "No unit at point"))))

(defun systemd-manager--disable-unit ()
  "Disable the systemd unit at point."
  (interactive)
  (let ((unit (systemd-manager--get-unit-at-point)))
    (if unit
        (systemd-manager--execute-action "disable" unit)
      (message "No unit at point"))))

(defun systemd-manager--open-unit-file ()
  "Open the configuration file for the systemd unit at point."
  (interactive)
  (let ((unit (systemd-manager--get-unit-at-point)))
    (if unit
        (let ((file-path (string-trim
                          (systemd-manager--execute-command
                           (format "show %s --property=FragmentPath --value" unit)))))
          (if (and file-path (not (string-empty-p file-path)) (file-exists-p file-path))
              (find-file-other-window file-path)
            (message "Unit file not found or unit has no fragment path: %s" unit)))
      (message "No unit at point"))))

(provide 'systemd-manager--actions)
;;; systemd-manager--actions.el ends here
