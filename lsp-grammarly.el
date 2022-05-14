;;; lsp-grammarly.el --- LSP Clients for Grammarly  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022  Shen, Jen-Chieh
;; Created date 2021-02-19 23:48:37

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: LSP Clients for Grammarly.
;; Keyword: lsp grammarly checker
;; Version: 0.3.0
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

(require 'subr-x)

(require 'lsp-mode)
(require 'grammarly)
(require 'request)
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

(define-obsolete-variable-alias
  'lsp-grammarly-modes 'lsp-grammarly-active-modes "0.2.1")

(defcustom lsp-grammarly-active-modes
  '(text-mode latex-mode org-mode markdown-mode)
  "List of major mode that work with Grammarly."
  :type 'list
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-patterns
  ["**/**.md"
   "**/*.txt"]
  "A glob pattern, like `*.{md,txt}` for file scheme."
  :type 'vector
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-selectors
  []
  "Filter documents to be checked with Grammarly."
  :type 'vector
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-audience "knowledgeable"
  "Sets the default audience for every document."
  :type '(choice (const "general")
                 (const "knowledgeable")
                 (const "expert"))
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-dialect "american"
  "Sets the default dialect for every document."
  :type '(choice (const "american")
                 (const "australian")
                 (const "british")
                 (const "canadian")
                 (const "auto-text"))
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-domain "general"
  "Sets the default domain for every document."
  :type '(choice (const "academic")
                 (const "business")
                 (const "general")
                 (const "mail")
                 (const "casual")
                 (const "creative"))
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-suggestions-conjunction-at-start-of-sentence
  nil
  "Flags use of conjunctions such as 'but' and 'and' at the beginning of
sentences."
  :type 'boolean
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-suggestions-fluency
  t
  "Suggests ways to sound more natural and fluent."
  :type 'boolean
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-suggestions-informal-pronouns-academic
  nil
  "Flags use of personal pronouns such as 'I' and 'you' in academic writing."
  :type 'boolean
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-suggestions-missing-spaces
  t
  "Suggests adding missing spacing after a numeral when writing times."
  :type 'boolean
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-suggestions-noun-strings
  t
  "Flags a series of nouns that modify a final noun."
  :type 'boolean
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-suggestions-numbers-beginning-sentences
  t
  "Suggests spelling out numbers at the beginning of sentences."
  :type 'boolean
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-suggestions-numbers-zero-through-ten
  t
  "Suggests spelling out numbers zero through ten."
  :type 'boolean
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-suggestions-oxford-comma
  nil
  "Suggests adding the Oxford comma after the second-to-last item in a list of
things."
  :type 'boolean
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-suggestions-passive-voice
  nil
  "Flags use of passive voice."
  :type 'boolean
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-suggestions-person-first-language
  t
  "Suggests using person-first language to refer respectfully to an individual
with a disability."
  :type 'boolean
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-suggestions-possibly-biased-language-age-related
  t
  "Suggests alternatives to potentially biased language related to older adults."
  :type 'boolean
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-suggestions-possibly-biased-language-disability-related
  t
  "Suggests alternatives to potentially ableist language."
  :type 'boolean
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-suggestions-possibly-biased-language-family-related
  t
  "Suggests alternatives to potentially biased language related to parenting and
family systems."
  :type 'boolean
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-suggestions-possibly-biased-language-gender-related
  t
  "Suggests alternatives to potentially gender-biased and non-inclusive phrasing."
  :type 'boolean
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-suggestions-possibly-biased-language-human-rights
  t
  "Suggests alternatives to language related to human slavery."
  :type 'boolean
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-suggestions-possibly-biased-language-human-rights-related
  t
  "Suggests alternatives to terms with origins in the institution of slavery."
  :type 'boolean
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-suggestions-possibly-biased-language-lgbtqia-related
  t
  "Flags LGBTQIA+-related terms that may be seen as biased, outdated, or
disrespectful in some contexts."
  :type 'boolean
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-suggestions-possibly-biased-language-race-ethnicity-related
  t
  "Suggests alternatives to potentially biased language related to race and
ethnicity."
  :type 'boolean
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-suggestions-possibly-politically-incorrect-language
  t
  "Suggests alternatives to language that may be considered politically
incorrect."
  :type 'boolean
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-suggestions-preposition-at-the-end-of-sentence
  nil
  "Flags use of prepositions such as 'with' and 'in' at the end of sentences."
  :type 'boolean
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-suggestions-punctuation-with-quotation
  t
  "Suggests placing punctuation before closing quotation marks."
  :type 'boolean
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-suggestions-readability-fillerwords
  t
  "Flags long, complicated sentences that could potentially confuse your reader."
  :type 'boolean
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-suggestions-readability-transforms
  t
  "Suggests splitting long, complicated sentences that could potentially confuse
your reader."
  :type 'boolean
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-suggestions-sentence-variety
  t
  "Flags series of sentences that follow the same pattern."
  :type 'boolean
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-suggestions-spaces-surrounding-slash
  t
  "Suggests removing extra spaces surrounding a slash."
  :type 'boolean
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-suggestions-split-infinitive
  t
  "Suggests rewriting split infinitives so that an adverb doesn't come between
'to' and the verb."
  :type 'boolean
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-suggestions-stylistic-fragments
  nil
  "Suggests completing all incomplete sentences, including stylistic sentence
fragments that may be intentional."
  :type 'boolean
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-suggestions-unnecessary-ellipses
  nil
  "Flags unnecessary use of ellipses (...)."
  :type 'boolean
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-suggestions-variety
  t
  "Suggests alternatives to words that occur frequently in the same paragraph."
  :type 'boolean
  :group 'lsp-grammarly)

(defcustom lsp-grammarly-suggestions-vocabulary
  t
  "Suggests alternatives to bland and overused words such as 'good' and 'nice'."
  :type 'boolean
  :group 'lsp-grammarly)

(defvar lsp-grammarly--show-debug-message nil
  "Flag to see if we show debug messages.")

;;
;; (@* "Obsolete" )
;;

(defcustom lsp-grammarly-auto-activate t
  "Enable Grammarly service when a supported document is opened."
  :type 'boolean
  :group 'lsp-grammarly)
(make-obsolete-variable 'lsp-grammarly-auto-activate nil "0.2.2")

(defcustom lsp-grammarly-emotions '()
  "Experimental: How do you want to sound."
  :type 'list
  :group 'lsp-grammarly)
(make-obsolete-variable 'lsp-grammarly-emotions nil "0.3.0")

(defcustom lsp-grammarly-goals '()
  "Experimental: What are you trying to do."
  :type 'list
  :group 'lsp-grammarly)
(make-obsolete-variable 'lsp-grammarly-goal nil "0.3.0")

(defcustom lsp-grammarly-user-words '()
  "A list of words as a local dictionary."
  :type 'list
  :group 'lsp-grammarly)
(make-obsolete-variable 'lsp-grammarly-user-words nil "0.3.0")

(defcustom lsp-grammarly-override '()
  "Per document override for audience, dialect, domain, emotions and goals."
  :type 'list
  :group 'lsp-grammarly)
(make-obsolete-variable 'lsp-grammarly-override nil "0.3.0")

;;
;; (@* "Util" )
;;

(defun lsp-grammarly--message (fmt &rest args)
  "Debug message like function `message' with same argument FMT and ARGS."
  (when lsp-grammarly--show-debug-message (apply 'message fmt args)))

(defun lsp-grammarly--scale-100 (score)
  "Convert SCORE to the scale of 100 instead of scale of 1."
  (ignore-errors (* score 100)))

(defun lsp-grammarly--random-bytes (n)
  "Return random bytes up to N."
  (let* ((charset "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
         (baseCount (length charset)))
    (with-temp-buffer
      (dotimes (_ n) (insert (elt charset (random baseCount))))
      (buffer-string))))

(defun lsp-grammarly--json-read (string)
  "Ensure read JSON STRING avoid bad string format."
  (let ((output (or (ignore-errors (json-read-from-string string))
                    (ignore-errors (json-read-from-string (concat "\"" string "\""))))))
    ;; After avoiding bad string format, we need to read json once again.
    (when (stringp output)
      (setq output (ignore-errors (json-read-from-string output))))
    output))

;;
;; (@* "Login" )
;;

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
  )

(defun lsp-grammarly--init (&rest _)
  "Get Grammarly API ready."
  ;; TODO: wait for the server side implementation
  )

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

(defconst lsp-grammarly-client-id "client_BaDkMgx4X19X9UxxYRCXZo"
  "Client ID is required for language server's activation.")

(defun lsp-grammarly--server-command ()
  "Generate startup command for Grammarly language server."
  (or (and lsp-grammarly-server-path
           (list lsp-grammarly-server-path "--stdio"))
      (list (lsp-package-path 'grammarly-ls) "--stdio")))

(lsp-register-custom-settings
 '(("grammarly.patterns" lsp-grammarly-patterns)
   ("grammarly.selectors" lsp-grammarly-selectors)
   ("grammarly.config.documentDialect" lsp-grammarly-dialect)
   ("grammarly.config.documentDomain" lsp-grammarly-domain)
   ("grammarly.config.suggestions.ConjunctionAtStartOfSentence" lsp-grammarly-suggestions-conjunction-at-start-of-sentence)
   ("grammarly.config.suggestions.Fluency" lsp-grammarly-suggestions-fluency)
   ("grammarly.config.suggestions.InformalPronounsAcademic" lsp-grammarly-suggestions-informal-pronouns-academic)
   ("grammarly.config.suggestions.MissingSpaces" lsp-grammarly-suggestions-missing-spaces)
   ("grammarly.config.suggestions.NounStrings" lsp-grammarly-suggestions-noun-strings)
   ("grammarly.config.suggestions.NumbersBeginningSentences" lsp-grammarly-suggestions-numbers-beginning-sentences)
   ("grammarly.config.suggestions.NumbersZeroThroughTen" lsp-grammarly-suggestions-numbers-zero-through-ten)
   ("grammarly.config.suggestions.OxfordComma" lsp-grammarly-suggestions-oxford-comma)
   ("grammarly.config.suggestions.PassiveVoice" lsp-grammarly-suggestions-passive-voice)
   ("grammarly.config.suggestions.PersonFirstLanguage" lsp-grammarly-suggestions-person-first-language)
   ("grammarly.config.suggestions.PossiblyBiasedLanguageAgeRelated" lsp-grammarly-suggestions-possibly-biased-language-age-related)
   ("grammarly.config.suggestions.PossiblyBiasedLanguageDisabilityRelated" lsp-grammarly-suggestions-possibly-biased-language-disability-related)
   ("grammarly.config.suggestions.PossiblyBiasedLanguageFamilyRelated" lsp-grammarly-suggestions-possibly-biased-language-family-related)
   ("grammarly.config.suggestions.PossiblyBiasedLanguageGenderRelated" lsp-grammarly-suggestions-possibly-biased-language-gender-related)
   ("grammarly.config.suggestions.PossiblyBiasedLanguageHumanRights" lsp-grammarly-suggestions-possibly-biased-language-human-rights)
   ("grammarly.config.suggestions.PossiblyBiasedLanguageHumanRightsRelated" lsp-grammarly-suggestions-possibly-biased-language-human-rights-related)
   ("grammarly.config.suggestions.PossiblyBiasedLanguageLgbtqiaRelated" lsp-grammarly-suggestions-possibly-biased-language-lgbtqia-related)
   ("grammarly.config.suggestions.PossiblyBiasedLanguageRaceEthnicityRelated" lsp-grammarly-suggestions-possibly-biased-language-race-ethnicity-related)
   ("grammarly.config.suggestions.PossiblyPoliticallyIncorrectLanguage" lsp-grammarly-suggestions-possibly-politically-incorrect-language)
   ("grammarly.config.suggestions.PrepositionAtTheEndOfSentence" lsp-grammarly-suggestions-preposition-at-the-end-of-sentence)
   ("grammarly.config.suggestions.PunctuationWithQuotation" lsp-grammarly-suggestions-punctuation-with-quotation)
   ("grammarly.config.suggestions.ReadabilityFillerwords" lsp-grammarly-suggestions-readability-fillerwords)
   ("grammarly.config.suggestions.ReadabilityTransforms" lsp-grammarly-suggestions-readability-transforms)
   ("grammarly.config.suggestions.SentenceVariety" lsp-grammarly-suggestions-sentence-variety)
   ("grammarly.config.suggestions.SpacesSurroundingSlash" lsp-grammarly-suggestions-spaces-surrounding-slash)
   ("grammarly.config.suggestions.SplitInfinitive" lsp-grammarly-suggestions-split-infinitive)
   ("grammarly.config.suggestions.StylisticFragments" lsp-grammarly-suggestions-stylistic-fragments)
   ("grammarly.config.suggestions.UnnecessaryEllipses" lsp-grammarly-suggestions-unnecessary-ellipses)
   ("grammarly.config.suggestions.Variety" lsp-grammarly-suggestions-variety)
   ("grammarly.config.suggestions.Vocabulary" lsp-grammarly-suggestions-vocabulary)))

(lsp-dependency 'grammarly-ls
                '(:system "grammarly-ls")
                '(:npm :package "@emacs-grammarly/grammarly-languageserver"
                       :path "grammarly-languageserver"))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-grammarly--server-command)
  :initialization-options
  `((clientId . ,lsp-grammarly-client-id)
    (name . "Grammarly"))
  :activation-fn (lambda (&rest _) (apply #'derived-mode-p lsp-grammarly-active-modes))
  :priority -1
  :add-on? t
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
  (user-error "[INFO] This command is currently disabled, and it will be added back in the later version"))

(defun lsp-grammarly-stop ()
  "Stop the Grammarly checker."
  (interactive)
  (user-error "[INFO] This command is currently disabled, and it will be added back in the later version"))

(defun lsp-grammarly-stats ()
  "Return document state."
  (interactive)
  (user-error "[INFO] This command is currently disabled, and it will be added back in the later version"))

;;
;; (@* "Login" )
;;

(defun lsp-grammarly-connected-p (&optional callback)
  "Print account connectivity."
  (interactive)
  (lsp-request-async
   "$/isUserAccountConnected" nil
   (lambda (connected &rest _)
     (when callback (funcall callback connected)))))

(defun lsp-grammarly--get-oauth-url (redirect-uri callback)
  "Get OAuth url and execute CALLBACK."
  (lsp-request-async
   "$/getOAuthUrl" `(:oauthRedirectUri ,redirect-uri)
   callback))

(defun lsp-grammarly--resolve-uri (uri)
  "Handle URI for authentication."
  (let ((prefix "vscode://znck.grammarly/auth/callback?") query)
    (if (not (string-prefix-p prefix uri))
        (user-error "[WARNING] An URL should start with prefix: %s" prefix)
      (setq uri (s-replace prefix "" uri)
            query (url-parse-query-string uri))
      (nth 1 (assoc "code" query)))))

(defun lsp-grammarly--uri-callback ()
  "Callback after resolving URI.

Argument CODE is the query string from URI."
  (let* ((uri (read-string "[Grammarly Authentication] code: "))
         (code (lsp-grammarly--resolve-uri uri)))
    (lsp-request-async
     "$/handleOAuthCallbackUri" `(:url ,code)
     (lambda (&rest _)
       (lsp-grammarly-connected-p
        (lambda (connected &rest _)
          (if connected
              (message "Account connected.")
            (message "Unexpected URI: %s" uri))))))))

(defun lsp-grammarly-login ()
  "Login to Grammarly.com."
  (interactive)
  (lsp-grammarly-connected-p
   (lambda (connected)
     (if connected (message "[INFO] You are already logged in")
       (let* ((internal-redirect-uri "vscode://znck.grammarly/auth/callback")
              (external-redirect-uri "vscode://znck.grammarly/auth/callback")
              (redirect-uri
               (if (string= internal-redirect-uri external-redirect-uri)
                   internal-redirect-uri
                 "https://vscode-extension-grammarly.netlify.app/.netlify/functions/redirect")))
         (lsp-grammarly--get-oauth-url
          redirect-uri
          (lambda (url &rest _)
            (let ((to-base64-url (base64url-encode-string external-redirect-uri t)))
              (setq url (s-replace-regexp "state=[^&]*"
                                          (concat "state=" to-base64-url)
                                          url)))
            (browse-url url)
            (lsp-grammarly--uri-callback))))))))

(defun lsp-grammarly-logout ()
  "Logout from Grammarly.com."
  (interactive)
  (lsp-request-async
   "$/logout" nil
   (lambda (&rest _) (message "Logged out."))))

(provide 'lsp-grammarly)
;;; lsp-grammarly.el ends here
