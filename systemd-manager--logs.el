;;; systemd-manager--logs.el --- Systemd log viewing -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Log viewing functionality for systemd package.

;;; Code:

(require 'systemd-manager--core)

(defun systemd-manager--build-log-command (unit)
  "Build a journalctl command string for UNIT."
  (let ((base-cmd (if systemd-manager--user-mode
                      (format "journalctl --user -u %s --no-pager" unit)
                    (format "journalctl -u %s --no-pager" unit)))
        (sudo-prefix (if (and (not systemd-manager--user-mode)
                              (not systemd-manager--remote-host)) ; Don't use sudo with TRAMP
                         "sudo " "")))
    (format "%s%s" sudo-prefix base-cmd)))

(defun systemd-manager--execute-log-command (unit)
  "Execute a journalctl command for UNIT.
Returns the command output as a string."
  (let ((cmd (systemd-manager--build-log-command unit))
        (default-directory (systemd-manager--get-default-directory)))
    (shell-command-to-string cmd)))

(defun systemd-manager--show-logs ()
  "Show logs for the systemd unit at point."
  (interactive)
  (let ((unit (systemd-manager--get-unit-at-point)))
    (if unit
        (let* ((host-suffix (if systemd-manager--remote-host 
                                (format " (%s)" systemd-manager--remote-host) 
                              ""))
               (buffer (get-buffer-create (format "*Systemd Logs: %s%s*" unit host-suffix))))
          (with-current-buffer buffer
            (erase-buffer)
            (insert (systemd-manager--execute-log-command unit))
            (goto-char (point-max))
            (view-mode)
            ;; Set up local variables for refresh
            (setq systemd-manager--user-mode (with-current-buffer "*Systemd Units*" systemd-manager--user-mode))
            (setq systemd-manager--remote-host (with-current-buffer "*Systemd Units*" systemd-manager--remote-host))
            ;; Set up keybindings for logs buffer
            (local-set-key (kbd "a") 'systemd-manager--toggle-auto-refresh)
            (local-set-key (kbd "g") 'systemd-manager--refresh-logs-buffer))
          (pop-to-buffer buffer))
      (message "No unit at point"))))

(defun systemd-manager--refresh-logs-buffer ()
  "Refresh the current logs buffer."
  (interactive)
  (when (string-match "\\*Systemd Logs: \\([^*]+\\)\\*" (buffer-name))
    (let* ((full-name (match-string 1 (buffer-name)))
           ;; Extract unit name by removing host suffix if present
           (unit (if (string-match "\\(.+\\) (.*)" full-name)
                     (match-string 1 full-name)
                   full-name))
           (point-before (point)))
      (erase-buffer)
      (insert (systemd-manager--execute-log-command unit))
      (goto-char point-before)
      (message "Logs refreshed for %s%s" 
               unit 
               (if systemd-manager--remote-host 
                   (format " on %s" systemd-manager--remote-host) 
                 "")))))
