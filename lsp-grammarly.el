;;; lsp-grammarly.el --- LSP Clients for Grammarly  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh
;; Created date 2021-02-19 23:48:37

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: LSP Clients for Grammarly.
;; Keyword: lsp grammarly checker
;; Version: 0.2.1
;; Package-Requires: ((emacs "27.1") (lsp-mode "6.1") (grammarly "0.3.0") (request "0.3.0") (s "1.12.0") (ht "2.3"))
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
(require 'grammarly)
(require 'request)
(require 's)
(require 'ht)
(require 'json)

(unless (require 'keytar nil t)
  (warn "`Keytar' is required for login into Grammarly account."))

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

(defvar lsp-grammarly--show-debug-message nil
  "Flag to see if we show debug messages.")

;;
;; (@* "Util" )
;;

(defun lsp-grammarly--message (fmt &rest args)
  "Debug message like function `message' with same argument FMT and ARGS."
  (when lsp-grammarly--show-debug-message (apply 'message fmt args)))

(defun lsp-grammarly--scale-100 (score)
  "Convert SCORE to the scale of 100 instead of scale of 1."
  (* score 100))

(defun lsp-grammarly--random-bytes (n)
  "Return random bytes up to N."
  (let* ((charset "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
         (baseCount (length charset)))
    (with-temp-buffer
      (dotimes (_ n) (insert (elt charset (random baseCount))))
      (buffer-string))))

(defun lsp-grammarly--json-encode (obj)
  "Wrap function `json-encode' to convert OBJ for keytar password."
  (s-replace "\"" "\\\"" (json-encode obj)))

;;
;; (@* "Login" )
;;

(defconst lsp-grammarly--cookie-key "vscode-grammarly-cookie"
  "Key to store credentials.")

(defconst lsp-grammarly--account "default"
  "Key that Grammarly LSP default to.")

(defvar lsp-grammarly--password-string nil
  "Encrypted password in string.")

(defvar lsp-grammarly--password nil
  "Encrypted password in alist.")

(defun lsp-grammarly-login-p ()
  "Return non-nil if currently logged in to Grammarly.com."
  lsp-grammarly--password)

(defun lsp-grammarly--username ()
  "Return the currently login username."
  (when lsp-grammarly--password
    (or (ignore-errors (cdr (assoc 'username lsp-grammarly--password)))
        (ignore-errors (ht-get lsp-grammarly--password "username")))))

(defun lsp-grammarly--get-credentials (_workspace _uri callback &rest _)
  "Return the credentials from native password manager.

For argument CALLBACK, see object `lsp--client' description."
  (funcall callback nil))

(defun lsp-grammarly--get-token (_workspace _uri callback &rest _)
  "Return the token from variable `lsp-grammarly--password'.

For argument CALLBACK, see object `lsp--client' description."
  (funcall callback lsp-grammarly--password))

(defun lsp-grammarly--store-token (_workspace _uri _callback &rest _)
  "Save the token once."
  (keytar-set-password
   lsp-grammarly--cookie-key lsp-grammarly--account lsp-grammarly--password-string))

(defun lsp-grammarly--init (&rest _)
  "Get Grammarly API ready."
  (unless (lsp-grammarly-login-p)
    (let ((pass (ignore-errors
                  (keytar-get-password lsp-grammarly--cookie-key lsp-grammarly--account))))
      (when pass
        (setq lsp-grammarly--password-string pass
              lsp-grammarly--password (ignore-errors (json-read-from-string pass)))))
    (if (lsp-grammarly-login-p)
        (message "[INFO] Logged in as, %s" (lsp-grammarly--username))
      (message "[INFO] Visited as, anonymous"))))

(defun lsp-grammarly--show-error (_workspace _uri callback &rest _)
  "Show error from language server.

For argument CALLBACK, see object `lsp--client' description."
  ;; TODO: This only shows credentials error but we have it resolve
  ;; on our side.
  (funcall callback))

(defun lsp-grammarly--update-document-state (_workspace _uri _callback &rest _)
  "Update the document status from current document."
  ;; TODO: this is where you get the document state to update modeline
  ;; information for this plugin.
  ;;
  ;; Currently, this does nothing.
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
                '(:npm :package "@emacs-grammarly/unofficial-grammarly-language-server"
                       :path "unofficial-grammarly-language-server"))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-grammarly--server-command)
  :activation-fn (lambda (&rest _) (apply #'derived-mode-p lsp-grammarly-modes))
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

;;
;; (@* "Login" )
;;

(defvar lsp-grammarly--code-verifier nil "Login information, code verifier.")
(defvar lsp-grammarly--challenge nil "Login information, challenge.")

(defconst lsp-grammarly-client-id "extensionVSCode"
  "Key for URI scheme.")

(defun lsp-grammarly--resolve-uri (uri)
  "Handle URI for authentication."
  (let ((prefix "vscode://znck.grammarly/auth/callback?") query)
    (if (not (string-prefix-p prefix uri))
        (user-error "[WARNING] An URL should start with prefix: %s" prefix)
      (setq uri (s-replace prefix "" uri)
            query (url-parse-query-string uri))
      (nth 1 (assoc "code" query)))))

(defun lsp-grammarly--update-cookie ()
  "Refresh the Grammarly.com cookie once."
  (grammarly--form-cookie)
  `((csrf-token . ,(grammarly--get-cookie-by-name "csrf-token"))
    (grauth . ,(grammarly--get-cookie-by-name "grauth"))
    (gnar-containerId . ,(grammarly--get-cookie-by-name "gnar_containerId"))
    (tdi . ,(grammarly--get-cookie-by-name "tdi"))))

(defun lsp-grammarly--uri-callback ()
  "Callback after resolving URI.

Argument CODE is the query string from URI."
  (let* ((uri (read-string "[Grammarly Authentication] code: "))
         (code (lsp-grammarly--resolve-uri uri))
         cookie csrf-token grauth gnar-containerId tdi)
    (request
      (format "https://auth.grammarly.com/v3/user/oranonymous?app=%s" lsp-grammarly-client-id)
      :type "GET"
      :headers
      `(("x-client-type". ,lsp-grammarly-client-id)
        ("x-client-version" . "0.0.0"))
      :success
      (cl-function
       (lambda (&key _response _data &allow-other-keys)
         (setq cookie (lsp-grammarly--update-cookie)
               csrf-token (cdr (assoc 'csrf-token cookie))
               grauth (cdr (assoc 'grauth cookie))
               gnar-containerId (cdr (assoc 'gnar-containerId cookie)))
         (request
           "https://auth.grammarly.com/v3/api/unified-login/code/exchange"
           :type "POST"
           :headers
           `(("Accept" . "application/json")
             ("Content-Type" . "application/json")
             ("x-client-type" . ,lsp-grammarly-client-id)
             ("x-client-version" . "0.0.0")
             ("x-csrf-token" . ,csrf-token)
             ("x-container-id" . ,gnar-containerId)
             ("cookie" . ,(format "grauth=%s; csrf-token=%s" grauth csrf-token)))
           :data
           (json-encode
            `(("client_id" . ,lsp-grammarly-client-id)
              ("code" . ,code)
              ("code_verifier" . ,lsp-grammarly--code-verifier)))
           :success
           (cl-function
            (lambda (&key _response data &allow-other-keys)
              (setq cookie (lsp-grammarly--update-cookie)
                    csrf-token (cdr (assoc 'csrf-token cookie))
                    grauth (cdr (assoc 'grauth cookie))
                    gnar-containerId (cdr (assoc 'gnar-containerId cookie))
                    tdi (cdr (assoc 'tdi cookie)))
              (let* ((all-data (json-read-from-string data))
                     (user (nth 0 all-data))
                     (premium (string= "Premium" (cdr (assoc 'type user))))
                     (name (cdr (assoc 'name user)))
                     (email (cdr (assoc 'email user)))
                     (token (format "grauth=%s;csrf-token=%s;tdi=%s;" grauth csrf-token tdi))
                     (auth-info `(("isAnonymous" . :json-false)
                                  ("isPremium" . ,premium)
                                  ("token" . ,token)
                                  ("username" . ,email))))
                (keytar-set-password
                 lsp-grammarly--cookie-key lsp-grammarly--account
                 (lsp-grammarly--json-encode auth-info))
                ;; TODO: This is slow, need to improve the performance for better
                ;; user experience.
                (ignore-errors (lsp-workspace-restart nil))
                (message "[INFO] Logged in as `%s`" name))))
           :error
           (cl-function
            (lambda (&rest args &key _error-thrown &allow-other-keys)
              (lsp-grammarly--message "[ERROR] Error while authenticating login: %s" args))))))
      :error
      (cl-function
       (lambda (&rest args &key _error-thrown &allow-other-keys)
         (lsp-grammarly--message "[ERROR] Error while getting cookie: %s" args))))))

(defun lsp-grammarly-login ()
  "Login to Grammarly.com."
  (interactive)
  (keytar--ckeck)
  (if (lsp-grammarly-login-p)
      (message "[INFO] You are already logged in with `%s`" (lsp-grammarly--username))
    (setq lsp-grammarly--code-verifier
          (base64url-encode-string (lsp-grammarly--random-bytes 96) t)
          lsp-grammarly--challenge
          (base64url-encode-string (secure-hash 'sha256 lsp-grammarly--code-verifier nil nil t) t))
    (browse-url (format
                 "https://grammarly.com/signin/app?client_id=%s&code_challenge=%s"
                 lsp-grammarly-client-id lsp-grammarly--challenge))
    (lsp-grammarly--uri-callback)))

(defun lsp-grammarly-logout ()
  "Logout from Grammarly.com."
  (interactive)
  (keytar--ckeck)
  (if (not (lsp-grammarly-login-p))
      (message "[INFO] You are already logout from Grammarly.com")
    (if (keytar-delete-password lsp-grammarly--cookie-key lsp-grammarly--account)
        (progn
          (setq lsp-grammarly--password nil
                lsp-grammarly--password-string nil)
          (lsp-workspace-restart nil)
          (message "[INFO] Logged out of Grammarly.com"))
      (message "[ERROR] Failed to logout from Grammarly.com"))))

(provide 'lsp-grammarly)
;;; lsp-grammarly.el ends here
