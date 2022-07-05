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

(cl-defun lsp--npm-dependency-install (callback error-callback &key package &allow-other-keys)
  (if-let ((npm-binary (executable-find "npm")))
      (progn
        ;; Explicitly `make-directory' to work around NPM bug in
        ;; versions 7.0.0 through 7.4.1. See
        ;; https://github.com/emacs-lsp/lsp-mode/issues/2364 for
        ;; discussion.
        (make-directory (f-join lsp-server-install-dir "npm" package "lib") 'parents)
        (lsp-async-start-process (lambda ()
                                   (if (string-empty-p
                                        (string-trim (shell-command-to-string
                                                      (mapconcat #'shell-quote-argument `(,npm-binary "view" ,package "peerDependencies") " "))))
                                       callback
                                     (let ((default-directory (f-dirname (car (last (directory-files-recursively (f-join lsp-server-install-dir "npm" package) "package.json"))))))
                                       (when (f-dir-p default-directory)
                                         (lsp-async-start-process callback
                                                                  error-callback
                                                                  (executable-find "npx")
                                                                  "npm-install-peers")))))
                                 error-callback
                                 npm-binary
                                 "-g"
                                 "--prefix"
                                 (f-join lsp-server-install-dir "npm" package)
                                 "install"
                                 package))
    (lsp-log "Unable to install %s via `npm' because it is not present" package)
    nil))

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
    (message "Waited %s..." timer)))

(defconst server-install-path (lsp-package-path 'grammarly-ls)
  "The server install location.")

(unless (file-exists-p server-install-path)
  (error "Failed to install server: %s" server-install-path)
  (kill-emacs 1))

(message "Testing with a file...")

(find-file "README.md")  ; start lsp

;;; activate.el ends here
