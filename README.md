[![Build Status](https://travis-ci.com/emacs-grammarly/grammarly.svg?branch=master)](https://travis-ci.com/emacs-grammarly/grammarly)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# lsp-grammarly

`lsp-mode` client leveraging [unofficial-grammarly-language-server](https://github.com/znck/grammarly).

![](./etc/screenshot.png)

## Quickstart

```el
(use-package lsp-grammarly
  :ensure t
  :hook (text-mode . (lambda ()
                       (require 'lsp-grammarly)
                       (lsp))))  ; or lsp-deferred
```

## Configuration

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

## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
