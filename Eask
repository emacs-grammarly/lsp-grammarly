;; -*- mode: eask; lexical-binding: t -*-

(package "lsp-grammarly"
         "0.3.0"
         "LSP Clients for Grammarly")

(website-url "https://github.com/emacs-grammarly/lsp-grammarly")
(keywords "convenience" "lsp" "grammarly" "checker")

(package-file "lsp-grammarly.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "28.1")
(depends-on "lsp-mode")
(depends-on "grammarly")
(depends-on "request")
(depends-on "s")
(depends-on "ht")

(development
 (depends-on "markdown-mode"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
