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

(defun systemd-manager--show-logs ()
  "Show logs for the systemd unit at point."
  (interactive)
  (let ((unit (systemd-manager--get-unit-at-point)))
    (if unit
        (let* ((cmd (if systemd-manager--user-mode
                        (format "journalctl --user -u %s --no-pager" unit)
                      (format "journalctl -u %s --no-pager" unit)))
               (buffer (get-buffer-create (format "*Systemd Logs: %s*" unit))))
          (with-current-buffer buffer
            (erase-buffer)
            (insert (shell-command-to-string cmd))
            (goto-char (point-max))
            (view-mode)
            ;; Set up local variables for refresh
            (setq systemd-manager--user-mode (with-current-buffer "*Systemd Units*" systemd-manager--user-mode))
            ;; Set up keybindings for logs buffer
            (local-set-key (kbd "a") 'systemd-manager--toggle-auto-refresh)
            (local-set-key (kbd "g") 'systemd-manager--refresh-current-buffer))
          (pop-to-buffer buffer))
      (message "No unit at point"))))

(provide 'systemd-manager--logs)
;;; systemd-manager--logs.el ends here
