[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/lsp-grammarly-badge.svg)](https://melpa.org/#/lsp-grammarly)
[![MELPA Stable](https://stable.melpa.org/packages/lsp-grammarly-badge.svg)](https://stable.melpa.org/#/lsp-grammarly)

# lsp-grammarly

[![CI](https://github.com/emacs-grammarly/lsp-grammarly/actions/workflows/test.yml/badge.svg)](https://github.com/emacs-grammarly/lsp-grammarly/actions/workflows/test.yml)

`lsp-mode` client leveraging [grammarly-language-server](https://github.com/znck/grammarly).

<p align="center"><img src="./etc/screenshot.png"/></p>

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [lsp-grammarly](#lsp-grammarly)
    - [💾 Quickstart](#💾-quickstart)
    - [📇 Commands](#📇-commands)
    - [🔧 Configuration](#🔧-configuration)
    - [📝 Roadmap](#📝-roadmap)
    - [Contribute](#contribute)

<!-- markdown-toc end -->

## 💾 Quickstart

```el
(use-package lsp-grammarly
  :ensure t
  :hook (text-mode . (lambda ()
                       (require 'lsp-grammarly)
                       (lsp))))  ; or lsp-deferred
```

## 📇 Commands

List of commands interact with `language server` and `Grammarly.com`.

| Commands                    | Description                                                                  |
|:----------------------------|:-----------------------------------------------------------------------------|
| lsp-grammarly-login         | Login to Grammarly.com                                                       |
| lsp-grammarly-logout        | Logout from Grammarly.com                                                    |
| lsp-grammarly-check-grammar | Start grammarly check for currnet document (buffer)                          |
| lsp-grammarly-stop          | Stop grammarly checker from current document (buffer)                        |
| lsp-grammarly-stats         | Log out current document status, `score`, `readability`, `words count`, etc. |

## 🔧 Configuration

`lsp-grammarly` supports following configuration. Each configuration is described in
detail in [Grammarly Settings](https://github.com/znck/grammarly#extension-settings).

* `grammarly.patterns` via `lsp-grammarly-patterns`
* `grammarly.selectors` via `lsp-grammarly-selectors`
* `grammarly.config.documentDialect` via `lsp-grammarly-dialect`
* `grammarly.config.documentDomain` via `lsp-grammarly-domain`
* `grammarly.config.suggestions.ConjunctionAtStartOfSentence` via `lsp-grammarly-suggestions.conjunction-at-start-of-sentence`
* `` via ``

## 📝 Roadmap

List of todos, but I have not got time to implement these features.

- [ ] Create another package that displays information from [Grammarly.com](https://www.grammarly.com/)
(To display useful information, `score`, `readability`, `word counts`, etc).

## Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
