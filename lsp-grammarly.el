;;; lsp-grammarly.el --- LSP Clients for Grammarly  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh
;; Created date 2021-02-19 23:48:37

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: LSP Clients for Grammarly.
;; Keyword:
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/emacs-grammarly/lsp-grammarly

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
;; LSP server implementation for Grammarly
;;

;;; Code:

(require 'lsp-mode)

(defgroup lsp-grammarly nil
  "Settings for the Grammarly Language Server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/znck/grammarly"))

(defcustom lsp-grammarly-auto-activate t
  "Enable Grammarly service when a supported document is opened."
  :type 'boolean
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-audience "knowledgeable"
  "Sets the default audience for every document."
  :type '(choice (const "general")
                 (const "knowledgeable")
                 (const "expert"))
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-dialect "american"
  "Sets the default audience for every document."
  :type '(choice (const "american")
                 (const "australian")
                 (const "british")
                 (const "canadian"))
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-domain "general"
  "Sets the default audience for every document."
  :type '(choice (const "academic")
                 (const "business")
                 (const "general")
                 (const "technical")
                 (const "casual")
                 (const "creative"))
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-emotions '()
  "Experimental: How do you want to sound."
  :type 'list
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-goals '()
  "Experimental: What are you trying to do."
  :type 'list
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-user-words '()
  "A list of words as a local dictionary."
  :type 'list
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-override '()
  "Per document override for audience, dialect, domain, emotions and goals."
  :type 'list
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-diagnostics '()
  "Language-specific rules to ignore unnecessary diagnostics."
  :type 'list
  :group 'lsp-grammarly)

;; TODO: Map severity to Flycheck and Flymake.
(defcustom lsp-grammarly-severity '()
  "A mapping from Grammarly alert categories to Emacs Flycheck diagnostics
severity."
  :type 'string
  :group 'lsp-grammarly)

(defun lsp-grammarly--server-command ()
  "Generate startup command for Grammarly language server."
  (list (lsp-package-path 'grammarly-ls) "--stdio"))

(lsp-register-custom-settings
 '(("grammarly.autoActivate" lsp-grammarly-auto-activate t)
   ("grammarly.audience" lsp-grammarly-audience)
   ("grammarly.dialect" lsp-grammarly-dialect)
   ("grammarly.domain" lsp-grammarly-domain)
   ("grammarly.emotions" lsp-grammarly-emotions)
   ("grammarly.goals" lsp-grammarly-goals)
   ("grammarly.userWords" lsp-grammarly-user-words)
   ("grammarly.overrides" lsp-grammarly-override)
   ;;("grammarly.diagnostics" lsp-grammarly-diagnostics)
   ;;("grammarly.severity" lsp-grammarly-severity)
   ))

(lsp-dependency 'grammarly-ls
                '(:system "grammarly-ls")
                '(:npm :package "unofficial-grammarly-language-server-2"
                       :path "unofficial-grammarly-language-server-2"))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-grammarly--server-command)
  :major-modes '(text-mode latex-mode org-mode markdown-mode)
  :priority -1
  :server-id 'grammarly-ls
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'grammarly-ls callback error-callback))))

(provide 'lsp-grammarly)
;;; lsp-grammarly.el ends here
