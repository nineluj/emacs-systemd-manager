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

(require 'tramp)
(require 'json)

;; Variables
(defvar-local systemd-manager--user-mode nil
  "Whether to show user units instead of system units.")

(defvar systemd-manager--auto-refresh-delay 2
  "Delay in seconds before auto-refreshing buffer after an action.")

(defvar-local systemd-manager--last-units-data nil
  "Cache of the last units data to avoid unnecessary refreshes.")

(defvar-local systemd-manager--remote-host nil
  "Remote host for TRAMP connections. If nil, commands run locally.")

;; Helper functions for TRAMP support
(defun systemd-manager--get-default-directory ()
  "Get the appropriate default-directory for command execution."
  (if systemd-manager--remote-host
      (format "/%s:%s:" tramp-default-method systemd-manager--remote-host)
    default-directory))

(defun systemd-manager--build-command (action &optional unit)
  "Build a systemctl command string for ACTION and optional UNIT."
  (let ((base-cmd (if systemd-manager--user-mode
                      "systemctl --user"
                    "systemctl"))
        (sudo-prefix (if (and (not systemd-manager--user-mode)
                              (not systemd-manager--remote-host) ; Don't use sudo with TRAMP
                              (member action '("start" "stop" "restart" "enable" "disable")))
                         "sudo " "")))
    (if unit
        (format "%s%s %s %s" sudo-prefix base-cmd action unit)
      (format "%s%s %s" sudo-prefix base-cmd action))))

(defun systemd-manager--execute-command (action &optional unit)
  "Execute a systemctl command for ACTION and optional UNIT.
Returns the command output as a string."
  (let ((cmd (systemd-manager--build-command action unit))
        (default-directory (systemd-manager--get-default-directory)))
    (shell-command-to-string cmd)))

(defun systemd-manager--execute-action (action unit &optional success-message)
  "Execute ACTION on UNIT and schedule refresh.
Shows SUCCESS-MESSAGE if provided, otherwise a default message."
  (let ((cmd (systemd-manager--build-command action unit))
        (default-directory (systemd-manager--get-default-directory)))
    (shell-command cmd)
    (let ((host-info (if systemd-manager--remote-host 
                         (format " on %s" systemd-manager--remote-host) 
                       "")))
      (message (or success-message
                   (format "%s unit: %s%s" (capitalize action) unit host-info))))
    (systemd-manager--schedule-refresh)))

(defun systemd-manager--set-remote-host (host)
  "Set the remote host for systemd operations.
HOST should be in the format 'user@hostname' or just 'hostname'.
Set to nil to operate on local system."
  (interactive "sRemote host (user@hostname or empty for local): ")
  (setq systemd-manager--remote-host 
        (if (string-empty-p host) nil host))
  (message (if systemd-manager--remote-host
               "Systemd operations will be performed on: %s"
             "Systemd operations will be performed locally")
           systemd-manager--remote-host))

(defun systemd-manager--connect-to-host (host)
  "Connect to a remote HOST for systemd management.
HOST can be:
- 'hostname' (uses current user)
- 'user@hostname' 
- empty string or nil for local operations"
  (interactive "sConnect to host (user@hostname, hostname, or empty for local): ")
  (when (string-empty-p host)
    (setq host nil))
  (setq systemd-manager--remote-host host)
  (when (get-buffer "*Systemd Units*")
    (with-current-buffer "*Systemd Units*"
      (systemd-manager--schedule-refresh)))
  (message "Connected to: %s" (or host "local system")))

(defun systemd-manager--get-connection-info ()
  "Get current connection information as a string."
  (if systemd-manager--remote-host
      (format "Remote: %s%s" 
              systemd-manager--remote-host
              (if systemd-manager--user-mode " (user)" " (system)"))
    (format "Local%s" 
            (if systemd-manager--user-mode " (user)" " (system)"))))

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
