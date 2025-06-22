;;; systemd-manager--transient.el --- Systemd manager transient -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Code:

(require 'transient)

(transient-define-prefix systemd-manager--unit-menu ()
  "Systemd unit actions menu."
  ["Unit Actions"
   ("s" "Start unit" systemd-manager--start-unit)
   ("S" "Stop unit" systemd-manager--stop-unit)
   ("r" "Restart unit" systemd-manager--restart-unit)
   ("l" "Show logs" systemd-manager--show-logs)
   ("f" "Open unit file" systemd-manager--open-unit-file)
   ("u" "Toggle user mode" systemd-manager--toggle-user-mode)]
  ["Refresh"
   ("g" "Refresh now" systemd-manager--refresh-current-buffer)])

(provide 'systemd-manager--transient)
;;; systemd-manager--transient.el ends here
