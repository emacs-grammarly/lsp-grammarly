;;; lsp-grammarly.el --- LSP Clients for Grammarly  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh
;; Created date 2021-02-19 23:48:37

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: LSP Clients for Grammarly.
;; Keyword: lsp grammarly checker
;; Version: 0.1.1
;; Package-Requires: ((emacs "24.3") (lsp-mode "6.1") (s "1.12.0") (ht "2.3"))
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
(require 's)
(require 'ht)
(require 'json)

(defgroup lsp-grammarly nil
  "Settings for the Grammarly Language Server.

Link: https://github.com/znck/grammarly"
  :group 'lsp-mode
  :link '(url-link "https://github.com/emacs-grammarly/lsp-grammarly"))

(defcustom lsp-grammarly-server-path nil
  "Path points for Grammarly LSP.

This is only for development use."
  :type 'string
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-modes
  '(text-mode latex-mode org-mode markdown-mode)
  "List of major mode that work with Grammarly."
  :type 'list
  :group 'lsp-grammarly)

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

;;
;; (@* "Util" )
;;

(defun lsp-grammarly--scale-100 (score)
  "Convert SCORE to the scale of 100 instead of scale of 1."
  (* score 100))

(defun lsp-grammarly--username ()
  "Return the currently login username."
  (when lsp-grammarly--password
    (cdr (assoc 'username lsp-grammarly--password))))

;;
;; (@* "Login" )
;;

(defconst lsp-grammarly--account "default"
  "Key that Grammarly LSP default to.")

(defconst lsp-grammarly--cookie-key "vscode-grammarly-cookie"
  "Key to store credentials.")

(defvar lsp-grammarly--password-string ""
  "Encrypted password in string.")

(defvar lsp-grammarly--password ""
  "Encrypted password in alist.")

(defvar lsp-grammarly--token nil)

(defun lsp-grammarly--get-credentials (_workspace uri &rest _)
  ""
  (message "\f")
  (message ">>> [get-credentials]:")
  (message "╘[TL] 1: %s" (type-of _workspace))
  (message "╘[TL] 2: %s" (ht-keys uri))
  nil)

(defun lsp-grammarly--get-token (_workspace uri &rest _)
  ""
  (message "\f")
  (message ">>> [get-token]:")
  (message "╘[TL] 1: %s" (type-of _workspace))
  ;;(message "╘[TL] 2: %s" (ht-keys uri))
  lsp-grammarly--password-string)

(defun lsp-grammarly--store-token (_workspace uri &rest _)
  ""
  ;; TODO: ..
  )

(defun lsp-grammarly--show-error (_workspace uri &rest _)
  ""
  ;; TODO: ..
  )

(defun lsp-grammarly--update-document-state (_workspace uri &rest _)
  ""
  ;; TODO: ..
  )

(defun lsp-grammarly--init (&rest _)
  "Get Grammarly API ready."
  (setq lsp-grammarly--password-string nil
        lsp-grammarly--password nil)
  (let ((cookie (shell-command-to-string (format "keytar find %s" lsp-grammarly--cookie-key)))
        lines)
    (when (and (stringp cookie)
               (string-match-p "account:" cookie) (string-match-p "password:" cookie)
               (string-match-p "default" cookie))
      (setq lines (split-string cookie "\n"))
      (setq pass (nth 3 lines)
            pass (s-replace "password:" "" pass)
            pass (s-replace "'" "" pass)
            pass (string-trim pass))
      (setq lsp-grammarly--password-string pass
            lsp-grammarly--password (ignore-errors (json-read-from-string pass)))))
  (when lsp-grammarly--password
    (message "Logged in as %s" (lsp-grammarly--username))))

(defun lsp-grammarly--login ()
  "Login to Grammarly.com."
  ;; TODO: ..
  )

;;
;; (@* "Server" )
;;

(defun lsp-grammarly--server-command ()
  "Generate startup command for Grammarly language server."
  (or (and lsp-grammarly-server-path
           (list lsp-grammarly-server-path "--stdio"))
      (list (lsp-package-path 'grammarly-ls) "--stdio")))

(lsp-register-custom-settings
 '(("grammarly.autoActivate" lsp-grammarly-auto-activate t)
   ("grammarly.audience" lsp-grammarly-audience)
   ("grammarly.dialect" lsp-grammarly-dialect)
   ("grammarly.domain" lsp-grammarly-domain)
   ("grammarly.emotions" lsp-grammarly-emotions)
   ("grammarly.goals" lsp-grammarly-goals)
   ("grammarly.userWords" lsp-grammarly-user-words)
   ("grammarly.overrides" lsp-grammarly-override)))

(lsp-dependency 'grammarly-ls
                '(:system "grammarly-ls")
                '(:npm :package "unofficial-grammarly-language-server-2"
                       :path "unofficial-grammarly-language-server-2"))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-grammarly--server-command)
  :major-modes lsp-grammarly-modes
  :priority -1
  :server-id 'grammarly-ls
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'grammarly-ls callback error-callback))
  :after-open-fn #'lsp-grammarly--init
  :async-request-handlers
  (ht ("$/getCredentials" #'lsp-grammarly--get-credentials)
      ("$/getToken" #'lsp-grammarly--get-token)
      ("$/storeToken" #'lsp-grammarly--store-token)
      ("$/showError" #'lsp-grammarly--show-error)
      ("$/updateDocumentState" #'lsp-grammarly--update-document-state))))


;;
;; (@* "Commands" )
;;

(defun lsp-grammarly-check-grammar ()
  "Start the Grammarly checker."
  (interactive)
  (lsp-request-async
   "$/checkGrammar" `(:uri ,(lsp--buffer-uri))
   (lambda (_) (message "Start Grammarly checker..."))))

(defun lsp-grammarly-stop ()
  "Stop the Grammarly checker."
  (interactive)
  (lsp-request-async
   "$/stop" `(:uri ,(lsp--buffer-uri))
   (lambda (_) (message "Stop Grammarly checker..."))))

(defun lsp-grammarly-get-document-state ()
  "Return document state."
  (interactive)
  (lsp-request-async
   "$/getDocumentState" `(:uri ,(lsp--buffer-uri))
   (lambda (state)
     (let* ((user (ht-get state "user"))
            (is-premium (ht-get user "isPremium"))
            (_is-anonymous (ht-get user "isAnonymous"))
            (username (ht-get user "username"))
            (text-info (ht-get state "textInfo"))
            (chars-count (ht-get text-info "charsCount"))
            (words-count (ht-get text-info "wordsCount"))
            (readability-score (ht-get text-info "readabilityScore"))
            (score (ht-get state "score"))
            (scores (ht-get state "scores"))
            (clarity (ht-get scores "Clarity"))
            (tone (ht-get scores "Tone"))
            (correctness (ht-get scores "Correctness"))
            (general-score (ht-get scores "GeneralScore"))
            (engagement (ht-get scores "Engagement")))
       (message
        "[User] %s (%s)
[Text-Info] Readability: %s, C: %s, W: %s
[Text Score] %s out of 100
Clarity: %s, Tone: %s, Correctness: %s, GeneralScore: %s, Engagement: %s"
        username (if is-premium "Premium" "Free")
        readability-score chars-count words-count
        score
        (lsp-grammarly--scale-100 clarity)
        (lsp-grammarly--scale-100 tone)
        (lsp-grammarly--scale-100 correctness)
        (lsp-grammarly--scale-100 general-score)
        (lsp-grammarly--scale-100 engagement))))))

(provide 'lsp-grammarly)
;;; lsp-grammarly.el ends here
