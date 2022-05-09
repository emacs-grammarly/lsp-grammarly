[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/lsp-grammarly-badge.svg)](https://melpa.org/#/lsp-grammarly)
[![MELPA Stable](https://stable.melpa.org/packages/lsp-grammarly-badge.svg)](https://stable.melpa.org/#/lsp-grammarly)

# lsp-grammarly

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

> ⚠️ We are moving to the newer grammarly-language-server that uses the official
> API. Some features are disabled for now, but we will later add them back.

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

## Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
