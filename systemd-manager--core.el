;;; systemd-manager--core.el --- Core systemd functionality -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Core functionality for systemd package including command execution
;; and data fetching.

;;; Code:

(require 'json)

;; Variables
(defvar-local systemd-manager--user-mode nil
  "Whether to show user units instead of system units.")

(defvar systemd-manager--auto-refresh-delay 2
  "Delay in seconds before auto-refreshing buffer after an action.")

(defvar-local systemd-manager--last-units-data nil
  "Cache of the last units data to avoid unnecessary refreshes.")

;; Command building and execution
(defun systemd-manager--build-command (action &optional unit)
  "Build a systemctl command string for ACTION and optional UNIT."
  (let ((base-cmd (if systemd-manager--user-mode
                      "systemctl --user"
                    "systemctl"))
        (sudo-prefix (if (and (not systemd-manager--user-mode)
                              (member action '("start" "stop" "restart" "enable" "disable")))
                         "sudo " "")))
    (if unit
        (format "%s%s %s %s" sudo-prefix base-cmd action unit)
      (format "%s%s %s" sudo-prefix base-cmd action))))

(defun systemd-manager--execute-command (action &optional unit)
  "Execute a systemctl command for ACTION and optional UNIT.
Returns the command output as a string."
  (let ((cmd (systemd-manager--build-command action unit)))
    (shell-command-to-string cmd)))

(defun systemd-manager--execute-action (action unit &optional success-message)
  "Execute ACTION on UNIT and schedule refresh.
Shows SUCCESS-MESSAGE if provided, otherwise a default message."
  (let ((cmd (systemd-manager--build-command action unit)))
    (shell-command cmd)
    (message (or success-message (format "%s unit: %s" (capitalize action) unit)))
    (systemd-manager--schedule-refresh)))

(defun systemd-manager--schedule-refresh ()
  "Schedule a refresh of the current buffer after a delay."
  (run-with-timer systemd-manager--auto-refresh-delay nil
                  (lambda ()
                    (when (get-buffer "*Systemd Units*")
                      (with-current-buffer "*Systemd Units*"
                        (let ((units (get-units)))
                          (when units
                            (systemd-manager--refresh-units units t))))))))

(defun systemd-manager--merge-unit-data (unit-files unit-status)
  "Merge unit file data with runtime status."
  (let ((status-table (make-hash-table :test 'equal)))
    ;; Build hash table of runtime status
    (dolist (unit unit-status)
      (puthash (alist-get 'unit unit) unit status-table))

    ;; Merge data, prioritizing unit-files as the complete list
    (mapcar (lambda (unit-file)
              (let* ((unit-name (alist-get 'unit_file unit-file))
                     (status (gethash unit-name status-table)))
                `((unit . ,unit-name)
                  (load . ,(or (and status (alist-get 'load status)) "not-loaded"))
                  (active . ,(or (and status (alist-get 'active status)) "inactive"))
                  (description . ,(or (and status (alist-get 'description status)) "")))))
            unit-files)))

(provide 'systemd-manager--core)
;;; systemd-manager--core.el ends here
