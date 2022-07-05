;;; activate.el --- Test activation  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh
;; Created date 2022-05-08 12:50:37

;; This file is NOT part of GNU Emacs.

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
;;
;; Test activation
;;

;;; Code:

(eask-pkg-init)

(require 'lsp-mode)

(lsp-install-server t 'grammarly-ls)  ; Start installation

(defconst timeout 180
  "Timeout in seconds.")

(defvar timer 0)

(defun get-lsp-install-buffer ()
  "Get lsp-insall buffer."
  (nth 0
       (cl-remove-if-not (lambda (buf)
                           (string-prefix-p "*lsp-install:" (buffer-name buf)))
                         (buffer-list))))

(with-current-buffer (get-lsp-install-buffer)
  (while (not (string-match-p "^Comint" (thing-at-point 'line)))
    (goto-char (point-max))
    (forward-line -1)
    (sit-for 5)
    (cl-incf timer 5)
    (message "%s" (buffer-string))
    (message "Waited %s..." timer)))

;;; activate.el ends here
