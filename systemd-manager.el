;;; systemd-manager.el --- Systemd unit management interface -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: nineluj <code@nineluj.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, systemd, system
;; URL: https://github.com/yourusername/systemd.el

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides an interface for managing systemd units from within
;; Emacs.  It allows you to:
;;
;; - View system and user units in a tabulated list
;; - Start, stop, restart, enable, and disable units
;; - View unit logs
;; - Open unit configuration files
;; - Toggle between system and user units
;;
;; Usage:
;;   M-x systemd-manager

;;; Code:

(require 'systemd-manager--core)
(require 'systemd-manager--actions)
(require 'systemd-manager--transient)
(require 'systemd-manager--logs)
(require 'systemd-manager--faces)
(require 'systemd-manager--ui)

;;;###autoload
(defun systemd-manager ()
  "Show systemd units interface."
  (interactive)
  (systemd-manager--show-units))

(provide 'systemd-manager)
;;; systemd.el ends here
