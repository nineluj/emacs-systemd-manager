;;; systemd-manager--ui.el --- Systemd user interface -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; User interface components for systemd package including tabulated
;; list display and keybindings.

;;; Code:

(require 'tabulated-list)
(require 'transient)
(require 'systemd-manager--core)
(require 'systemd-manager--faces)

(defun systemd-manager--get-unit-files ()
  "Get unit files with their enabled/disabled status."
  (json-parse-string (systemd-manager--execute-command "list-unit-files -o json")
                     :object-type 'alist
                     :array-type 'list
                     :null-object nil))

(defun systemd-manager--get-unit-status ()
  "Get units with their loaded and active status."
  (json-parse-string (systemd-manager--execute-command "list-units --all -o json")
                     :object-type 'alist
                     :array-type 'list
                     :null-object nil))

(defun get-units ()
  "Get systemd units with their status and descriptions."
  (condition-case err
      (let* ((unit-files (systemd-manager--get-unit-files))
             (unit-status (systemd-manager--get-unit-status)))
        (systemd-manager--merge-unit-data unit-files unit-status))
    (json-error
     (message "Failed to parse systemctl JSON output: %s" err)
     nil)))

(defun systemd-manager--show-units ()
  "Show systemd units in a table."
  (interactive "P")
  (let ((units (get-units)))
    (if units
        (systemd-manager--units-tabulated units)
      (message "No units found or failed to retrieve units"))))

(defun systemd-manager--get-unit-at-point ()
  "Get the unit name at the current line."
  (when (derived-mode-p 'tabulated-list-mode)
    (let ((entry (tabulated-list-get-entry)))
      (when entry
        (aref entry 0)))))

(defun systemd-manager--setup-table (units)
  "Set up the tabulated list format and entries for systemd UNITS."
  (setq tabulated-list-format
        (vector
         (list (if systemd-manager--user-mode "User Unit" "Unit") 50 t)
         '("Load" 15 t)       ; loaded/not-loaded/etc
         '("Active" 15 t)     ; active/inactive/failed/etc
         '("Description" 40 t)))
  (setq tabulated-list-entries
        (mapcar (lambda (unit)
                  (list (alist-get 'unit unit)
                        (vector
                         (alist-get 'unit unit)
                         (systemd-manager--colorize-load-state (or (alist-get 'load unit) "not-loaded"))
                         (systemd-manager--colorize-active-state (or (alist-get 'active unit) "inactive"))
                         (or (alist-get 'description unit) ""))))
                ;; Sort units by name
                (sort units (lambda (a b)
                              (string< (alist-get 'unit a)
                                       (alist-get 'unit b)))))))

(defun systemd-manager--refresh-units (units &optional silent)
  "Refresh the units table with new data.
If SILENT is non-nil, don't show refresh message.
Only refreshes if the data has actually changed."
  ;; Compare new data with cached data
  (unless (equal units systemd-manager--last-units-data)
    (let ((current-unit (systemd-manager--get-unit-at-point))  ; Save current unit
          (current-column (current-column)))           ; Save current column
      (systemd-manager--setup-table units)
      (tabulated-list-init-header)
      (tabulated-list-print)
      ;; Cache the new data
      (setq systemd-manager--last-units-data units)
      ;; Try to restore position to the same unit
      (if current-unit
          (progn
            (goto-char (point-min))
            (while (and (not (eobp))
                        (not (string= (systemd-manager--get-unit-at-point) current-unit)))
              (forward-line 1))
            ;; Restore horizontal position
            (move-to-column current-column))
        (goto-char (point-min)))
      (unless silent
        (message "Refreshed %s units" (if systemd-manager--user-mode "user" "system"))))
    ;; If data is the same, show a message only if not silent
    (unless silent
      (message "No changes detected - display unchanged"))))

(defun systemd-manager--units-tabulated (units)
  "Display systemd UNITS in a tabulated list."
  (let ((buffer (get-buffer-create "*Systemd Units*")))
    (with-current-buffer buffer
      (tabulated-list-mode)
      (setq systemd-manager--user-mode nil) ; Start with system units
      (setq systemd-manager--last-units-data nil) ; force refresh if re-opened
      (systemd-manager--setup-table units)
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (systemd-manager--setup-keybindings))
    (pop-to-buffer buffer)))

(defun systemd-manager--setup-keybindings ()
  "Set up key bindings for systemd units buffer."
  (local-set-key (kbd "s") 'systemd-manager--start-unit)
  (local-set-key (kbd "S") 'systemd-manager--stop-unit)
  (local-set-key (kbd "r") 'systemd-manager--restart-unit)
  (local-set-key (kbd "l") 'systemd-manager--show-logs)
  (local-set-key (kbd "f") 'systemd-manager--open-unit-file)
  (local-set-key (kbd "u") 'systemd-manager--toggle-user-mode)
  (local-set-key (kbd "a") 'systemd-manager--toggle-auto-refresh)
  (local-set-key (kbd "g") 'systemd-manager--refresh-current-buffer)
  (local-set-key (kbd "?") 'systemd-manager--unit-menu))

(defun systemd-manager--toggle-user-mode ()
  "Toggle between system and user units."
  (interactive)
  (setq systemd-manager--user-mode (not systemd-manager--user-mode))
  ;; clear cache since we get new data
  (setq systemd-manager--last-units-data nil)
  (let ((units (get-units)))
    (if units
        (systemd-manager--refresh-units units)
      (message "No units found or failed to retrieve units"))))

(defun systemd-manager--refresh-current-buffer ()
  "Refresh the current systemd buffer based on its type."
  (interactive)
  (cond
   ;; Units table buffer
   ((string-match "\\*Systemd Units\\*" (buffer-name))
    (let ((units (get-units)))
      (when units
        (systemd-manager--refresh-units units))))
   ;; Logs buffer
   ((string-match "\\*Systemd Logs:" (buffer-name))
    (systemd-manager--refresh-logs-buffer))))

(defun systemd-manager--refresh-logs-buffer ()
  "Refresh the current logs buffer."
  (when (string-match "\\*Systemd Logs: \\(.+\\)\\*" (buffer-name))
    (let* ((unit (match-string 1 (buffer-name)))
           (cmd (if systemd-manager--user-mode
                    (format "journalctl --user -u %s --no-pager" unit)
                  (format "journalctl -u %s --no-pager" unit)))
           (point-pos (point)))
      (erase-buffer)
      (insert (shell-command-to-string cmd))
      (goto-char (min point-pos (point-max)))
      (view-mode))))

(provide 'systemd-manager--ui)
;;; systemd-manager--ui.el ends here
