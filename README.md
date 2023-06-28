[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/lsp-grammarly-badge.svg)](https://melpa.org/#/lsp-grammarly)
[![MELPA Stable](https://stable.melpa.org/packages/lsp-grammarly-badge.svg)](https://stable.melpa.org/#/lsp-grammarly)

# lsp-grammarly
> LSP Clients for Grammarly

[![CI](https://github.com/emacs-grammarly/lsp-grammarly/actions/workflows/test.yml/badge.svg)](https://github.com/emacs-grammarly/lsp-grammarly/actions/workflows/test.yml)
[![Activate](https://github.com/emacs-grammarly/lsp-grammarly/actions/workflows/activate.yml/badge.svg)](https://github.com/emacs-grammarly/lsp-grammarly/actions/workflows/activate.yml)

`lsp-mode` client leveraging [grammarly-language-server](https://github.com/emacs-grammarly/grammarly-language-server).

<p align="center"><img src="./etc/screenshot.png"/></p>

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [lsp-grammarly](#lsp-grammarly)
    - [💾 Quickstart](#💾-quickstart)
    - [📇 Commands](#📇-commands)
    - [🔧 Configuration](#🔧-configuration)
    - [📝 Roadmap](#📝-roadmap)
    - [💸 Using a Paid Grammarly Account](#💸-using-a-paid-grammarly-account)
        - [🔍 Method 1: Login with VSCode (easier)](#🔍-method-1-login-with-vscode-easier)
        - [🔍 Method 2: Login with Emacs (a bit complicated)](#🔍-method-2-login-with-emacs-a-bit-complicated)
        - [Authentication from Grammarly website](#authentication-from-grammarly-website)
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

The `use-package` can be installed via MELPA if missing.

## 📇 Commands

> ⚠️ Warning
> 
> We are moving to the newer grammarly-language-server that uses the official
> API. Some features are disabled for now, but we will later add them back.

List of commands interact with `language server` and `Grammarly.com`.

| Commands             | Description                                                                  |
|:---------------------|:-----------------------------------------------------------------------------|
| lsp-grammarly-login  | Login to Grammarly.com                                                       |
| lsp-grammarly-logout | Logout from Grammarly.com                                                    |
| lsp-grammarly-resume | Start grammarly check for currnet document (buffer)                          |
| lsp-grammarly-pause  | Stop grammarly checker from current document (buffer)                        |
| lsp-grammarly-stats  | Log out current document status, `score`, `readability`, `words count`, etc. |

## 🔧 Configuration

`lsp-grammarly` supports following configuration. Each configuration is described in
detail in [Grammarly Extension Settings](https://github.com/emacs-grammarly/grammarly-language-server/blob/main/extension/package.json).

* `grammarly.patterns` via `lsp-grammarly-patterns`
* `grammarly.selectors` via `lsp-grammarly-selectors`
* `grammarly.config.documentDialect` via `lsp-grammarly-dialect`
* `grammarly.config.documentDomain` via `lsp-grammarly-domain`
* `grammarly.config.suggestions.ConjunctionAtStartOfSentence` via `lsp-grammarly-suggestions-conjunction-at-start-of-sentence`
* `grammarly.config.suggestions.Fluency` via `lsp-grammarly-suggestions-fluency`
* `grammarly.config.suggestions.InformalPronounsAcademic` via `lsp-grammarly-suggestions-informal-pronouns-academic`
* `grammarly.config.suggestions.MissingSpaces` via `lsp-grammarly-suggestions-missing-spaces`
* `grammarly.config.suggestions.NounStrings` via `lsp-grammarly-suggestions-noun-strings`
* `grammarly.config.suggestions.NumbersBeginningSentences` via `lsp-grammarly-suggestions-numbers-beginning-sentences`
* `grammarly.config.suggestions.NumbersZeroThroughTen` via `lsp-grammarly-suggestions-numbers-zero-through-ten`
* `grammarly.config.suggestions.OxfordComma` via `lsp-grammarly-suggestions-oxford-comma`
* `grammarly.config.suggestions.PassiveVoice` via `lsp-grammarly-suggestions-passive-voice`
* `grammarly.config.suggestions.PersonFirstLanguage` via `lsp-grammarly-suggestions-person-first-language`
* `grammarly.config.suggestions.PossiblyBiasedLanguageAgeRelated` via `lsp-grammarly-suggestions-possibly-biased-language-age-related`
* `grammarly.config.suggestions.PossiblyBiasedLanguageDisabilityRelated` via `lsp-grammarly-suggestions-possibly-biased-language-disability-related`
* `grammarly.config.suggestions.PossiblyBiasedLanguageFamilyRelated` via `lsp-grammarly-suggestions-possibly-biased-language-family-related`
* `grammarly.config.suggestions.PossiblyBiasedLanguageGenderRelated` via `lsp-grammarly-suggestions-possibly-biased-language-gender-related`
* `grammarly.config.suggestions.PossiblyBiasedLanguageHumanRights` via `lsp-grammarly-suggestions-possibly-biased-language-human-rights`
* `grammarly.config.suggestions.PossiblyBiasedLanguageHumanRightsRelated` via `lsp-grammarly-suggestions-possibly-biased-language-human-rights-related`
* `grammarly.config.suggestions.PossiblyBiasedLanguageLgbtqiaRelated` via `lsp-grammarly-suggestions-possibly-biased-language-lgbtqia-related`
* `grammarly.config.suggestions.PossiblyBiasedLanguageRaceEthnicityRelated` via `lsp-grammarly-suggestions-possibly-biased-language-race-ethnicity-related`
* `grammarly.config.suggestions.PossiblyPoliticallyIncorrectLanguage` via `lsp-grammarly-suggestions-possibly-politically-incorrect-language`
* `grammarly.config.suggestions.PrepositionAtTheEndOfSentence` via `lsp-grammarly-suggestions-preposition-at-the-end-of-sentence`
* `grammarly.config.suggestions.PunctuationWithQuotation` via `lsp-grammarly-suggestions-punctuation-with-quotation`
* `grammarly.config.suggestions.ReadabilityFillerwords` via `lsp-grammarly-suggestions-readability-fillerwords`
* `grammarly.config.suggestions.ReadabilityTransforms` via `lsp-grammarly-suggestions-readability-transforms`
* `grammarly.config.suggestions.SentenceVariety` via `lsp-grammarly-suggestions-sentence-variety`
* `grammarly.config.suggestions.SpacesSurroundingSlash` via `lsp-grammarly-suggestions-spaces-surrounding-slash`
* `grammarly.config.suggestions.SplitInfinitive` via `lsp-grammarly-suggestions-split-infinitive`
* `grammarly.config.suggestions.StylisticFragments` via `lsp-grammarly-suggestions-stylistic-fragments`
* `grammarly.config.suggestions.UnnecessaryEllipses` via `lsp-grammarly-suggestions-unnecessary-ellipses`
* `grammarly.config.suggestions.Variety` via `lsp-grammarly-suggestions-variety`
* `grammarly.config.suggestions.Vocabulary` via `lsp-grammarly-suggestions-vocabulary`

## 📝 Roadmap

List of todos, but I have not got time to implement these features.

- [ ] Create another package that displays information from [Grammarly.com](https://www.grammarly.com/)
(To display useful information, `score`, `readability`, `word counts`, etc).

## 💸 Using a Paid Grammarly Account

> 📢 Note that nothing needs to be done for using the free version of grammarly.

You can either login with [vscode-grammarly][]
using VSCode or hit `M-x lsp-grammarly-login`. They both share the same credentials
so you can login with either side.

### 🔍 Method 1: Login with VSCode (easier)

Install VSCode and, from the extension panel, select [vscode-grammarly][]
click on the drop-down arrow and select "Install pre-release version". (There
is an [issue](https://github.com/znck/grammarly/issues/351) with the release
version that causes authentication to fail, but this issue does not appear to
affect the pre-release version. Alternatively, you may try version 0.18.0,
which users [report](https://github.com/znck/grammarly/issues/350#issuecomment-1483848906)
as working fine.)

<p align="center"><img src="./etc/login/vscode-grammarly-extension.png"/></p>

Then call command palette (default to <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>p</kbd>)
and type to search `grammarly login` command.

<p align="center"><img src="./etc/login/vscode-grammarly-login.png"/></p>

You should see [Grammarly Website](#authentication-from-grammarly-website) and
login with your Grammarly account.

<p align="center"><img src="./etc/login/open-app-vscode.png"/></p>

🎉 Make sure you click on the button `Open Visual Studio Code`. Done! You
can now close VSCode and go back to Emacs!

### 🔍 Method 2: Login with Emacs (a bit complicated)

> ⚠️ Warning
> 
> This method is no longer working with the new language server and currently
> WIP, please login with VSCode!

Hit `M-x lsp-grammarly-login` and you should see the Grammarly's website pop out
from your favorite browser. See below [screenshot](#authentication-from-grammarly-website),

After login, click the button `Open URL:vscode`, If you have VSCode installed, then
this button would be `Open Visual Studio Code` instead yet it doesn't matter.

<p align="center"><img src="./etc/login/open-url-vscode.png"/></p>

Then click <kbd>F12</kbd> to open the DevTool window. You should able to see
an URI like the following

<p align="center"><img src="./etc/login/external-handler.png"/></p>

Copy and paste the URI back to Emacs and hit return.

<p align="center"><img src="./etc/login/emacs-paste.png"/></p>


🎉 Done! Now you should be loggin!

### Authentication from Grammarly website

Login with your Grammarly account (This step does not require VSCode to be
installed)!

<p align="center"><img src="./etc/login/grammarly-website.png"/></p>

## Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)
[![Become a patron](https://img.shields.io/badge/patreon-become%20a%20patron-orange.svg?logo=patreon)](https://www.patreon.com/jcs090218)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!

[vscode-grammarly]: https://marketplace.visualstudio.com/items?itemName=znck.grammarly
