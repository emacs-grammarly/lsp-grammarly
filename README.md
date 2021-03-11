[![Build Status](https://travis-ci.com/emacs-grammarly/lsp-grammarly.svg?branch=master)](https://travis-ci.com/emacs-grammarly/lsp-grammarly)
[![MELPA](https://melpa.org/packages/lsp-grammarly-badge.svg)](https://melpa.org/#/lsp-grammarly)
[![MELPA Stable](https://stable.melpa.org/packages/lsp-grammarly-badge.svg)](https://stable.melpa.org/#/lsp-grammarly)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# lsp-grammarly

`lsp-mode` client leveraging [unofficial-grammarly-language-server](https://github.com/znck/grammarly).

![](./etc/screenshot.png)

## :floppy_disk: Quickstart

```el
(use-package lsp-grammarly
  :ensure t
  :hook (text-mode . (lambda ()
                       (require 'lsp-grammarly)
                       (lsp))))  ; or lsp-deferred
```

## :mag: Commands

List of commands interact with `language server` and `Grammarly.com`.

| Commands                         | Description                                                            |
|:---------------------------------|:-----------------------------------------------------------------------|
| lsp-grammarly-login              | Login to Grammarly.com                                                 |
| lsp-grammarly-logout             | Logout from Grammarly.com                                              |
| lsp-grammarly-check-grammar      | Start grammarly check for currnet document (buffer)                    |
| lsp-grammarly-stop               | Stop grammarly checker from current document (buffer)                  |
| lsp-grammarly-get-document-state | Log out current document status, score, readability, words count, etc. |

## :wrench: Configuration

`lsp-grammarly` supports following configuration. Each configuration is described in
detail in [Grammarly Settings](https://github.com/znck/grammarly#extension-settings).

* `grammarly.autoActivate` via `lsp-grammarly-auto-activate`
* `grammarly.audience` via `lsp-grammarly-audience`
* `grammarly.dialect` via `lsp-grammarly-dialect`
* `grammarly.domain` via `lsp-grammarly-domain`
* `grammarly.emotions` via `lsp-grammarly-emotions`
* `grammarly.goals` via `lsp-grammarly-goals`
* `grammarly.userWords` via `lsp-grammarly-user-words`
* `grammarly.overrides` via `lsp-grammarly-override`

## :pencil: Roadmap

List of todos, but I have not got time to implement these features.

- [ ] Create another package that displays information from Grammarly.com.
Like, `score`, `readability`, `word counts`, etc.

## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
