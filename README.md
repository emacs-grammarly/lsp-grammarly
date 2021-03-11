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

## :money_with_wings: Using a Paid Grammarly Account

You can either login with [vscode-grammarly](https://marketplace.visualstudio.com/items?itemName=znck.grammarly)
using VSCode or hit `M-x lsp-grammarly-login`. They both share the same credentials
so you can login with either side.

### Method 1: Login with VSCode (easier)

Install VSCode and install extension [vscode-grammarly](https://marketplace.visualstudio.com/items?itemName=znck.grammarly)
from the extension panel.

![](./etc/login/vscode-grammarly-extension.png)

Then call command palette (default to <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>p</kbd>)
and type to search `grammarly login` command.

You should see [Grammarly Website](#authentication-from-grammarly-website) and
login with your Grammarly account.

![](./etc/login/open-app-vscode.png)

:tada: Make sure you click on the button `Open Visual Studio Code`. Done! You
can now close VSCode and go back to Emacs!

### Method 2: Login with Emacs (a bit complicated)

Hit `M-x lsp-grammarly-login` and you should see the Grammarly's website pop out
from your favorite browser. See below [screenshot](#authentication-from-grammarly-website),

After login, click the button `Open URL:vscode`, If you have VSCode install, then
this will be `Open Visual Studio Code` yet it doesn't matter.

![](./etc/login/open-url-vscode.png)

Then click <kbd>F12</kbd> to open the DevTool window. You should able to see
an URI like the following

![](./etc/login/external-handler.png)

Copy and paste the URI back to Emacs and hit return.

![](./etc/login/emacs-paste.png)

:tada: Done! Now you should be loggin!

### Authentication from Grammarly website

Login with your Grammarly account (This step does not require VSCode to be
installed)!

[](./etc/login/vscode-grammarly-login.png)

## :mag: Commands

List of commands interact with `language server` and `Grammarly.com`.

| Commands                         | Description                                                            |
|:---------------------------------|:-----------------------------------------------------------------------|
| lsp-grammarly-login              | Login to Grammarly.com                                                 |
| lsp-grammarly-logout             | Logout from Grammarly.com                                              |
| lsp-grammarly-check-grammar      | Start grammarly check for currnet document (buffer)                    |
| lsp-grammarly-stop               | Stop grammarly checker from current document (buffer)                  |
| lsp-grammarly-get-document-state | Log out current document status, `score`, `readability`, `words count`, etc. |

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

- [ ] Create another package that displays information from [Grammarly.com]().
Like, `score`, `readability`, `word counts`, etc.

## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
