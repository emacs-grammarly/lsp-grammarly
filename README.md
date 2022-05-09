[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/lsp-grammarly-badge.svg)](https://melpa.org/#/lsp-grammarly)
[![MELPA Stable](https://stable.melpa.org/packages/lsp-grammarly-badge.svg)](https://stable.melpa.org/#/lsp-grammarly)

# lsp-grammarly

[![CI](https://github.com/emacs-grammarly/lsp-grammarly/actions/workflows/test.yml/badge.svg)](https://github.com/emacs-grammarly/lsp-grammarly/actions/workflows/test.yml)
[![Activate](https://github.com/emacs-grammarly/lsp-grammarly/actions/workflows/activate.yml/badge.svg)](https://github.com/emacs-grammarly/lsp-grammarly/actions/workflows/activate.yml)

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
detail in [Grammarly Settings](https://github.com/znck/grammarly#extension-settings).

| Properties                                                                | Variables                                                                   |
|---------------------------------------------------------------------------|-----------------------------------------------------------------------------|
| `grammarly.patterns`                                                      | `lsp-grammarly-patterns`                                                    |
| `grammarly.selectors`                                                     | `lsp-grammarly-selectors`                                                   |
| `grammarly.config.documentDialect`                                        | `lsp-grammarly-dialect`                                                     |
| `grammarly.config.documentDomain`                                         | `lsp-grammarly-domain`                                                      |
| `grammarly.config.suggestions.ConjunctionAtStartOfSentence`               | `lsp-grammarly-suggestions-conjunction-at-start-of-sentence`                |
| `grammarly.config.suggestions.Fluency`                                    | `lsp-grammarly-suggestions-fluency`                                         |
| `grammarly.config.suggestions.InformalPronounsAcademic`                   | `lsp-grammarly-suggestions-informal-pronouns-academic`                      |
| `grammarly.config.suggestions.MissingSpaces`                              | `lsp-grammarly-suggestions-missing-spaces`                                  |
| `grammarly.config.suggestions.NounStrings`                                | `lsp-grammarly-suggestions-noun-strings`                                    |
| `grammarly.config.suggestions.NumbersBeginningSentences`                  | `lsp-grammarly-suggestions-numbers-beginning-sentences`                     |
| `grammarly.config.suggestions.NumbersZeroThroughTen`                      | `lsp-grammarly-suggestions-numbers-zero-through-ten`                        |
| `grammarly.config.suggestions.OxfordComma`                                | `lsp-grammarly-suggestions-oxford-comma`                                    |
| `grammarly.config.suggestions.PassiveVoice`                               | `lsp-grammarly-suggestions-passive-voice`                                   |
| `grammarly.config.suggestions.PersonFirstLanguage`                        | `lsp-grammarly-suggestions-person-first-language`                           |
| `grammarly.config.suggestions.PossiblyBiasedLanguageAgeRelated`           | `lsp-grammarly-suggestions-possibly-biased-language-age-related`            |
| `grammarly.config.suggestions.PossiblyBiasedLanguageDisabilityRelated`    | `lsp-grammarly-suggestions-possibly-biased-language-disability-related`     |
| `grammarly.config.suggestions.PossiblyBiasedLanguageFamilyRelated`        | `lsp-grammarly-suggestions-possibly-biased-language-family-related`         |
| `grammarly.config.suggestions.PossiblyBiasedLanguageGenderRelated`        | `lsp-grammarly-suggestions-possibly-biased-language-gender-related`         |
| `grammarly.config.suggestions.PossiblyBiasedLanguageHumanRights`          | `lsp-grammarly-suggestions-possibly-biased-language-human-rights`           |
| `grammarly.config.suggestions.PossiblyBiasedLanguageHumanRightsRelated`   | `lsp-grammarly-suggestions-possibly-biased-language-human-rights-related`   |
| `grammarly.config.suggestions.PossiblyBiasedLanguageLgbtqiaRelated`       | `lsp-grammarly-suggestions-possibly-biased-language-lgbtqia-related`        |
| `grammarly.config.suggestions.PossiblyBiasedLanguageRaceEthnicityRelated` | `lsp-grammarly-suggestions-possibly-biased-language-race-ethnicity-related` |
| `grammarly.config.suggestions.PossiblyPoliticallyIncorrectLanguage`       | `lsp-grammarly-suggestions-possibly-politically-incorrect-language`         |
| `grammarly.config.suggestions.PrepositionAtTheEndOfSentence`              | `lsp-grammarly-suggestions-preposition-at-the-end-of-sentence`              |
| `grammarly.config.suggestions.PunctuationWithQuotation`                   | `lsp-grammarly-suggestions-punctuation-with-quotation`                      |
| `grammarly.config.suggestions.ReadabilityFillerwords`                     | `lsp-grammarly-suggestions-readability-fillerwords`                         |
| `grammarly.config.suggestions.ReadabilityTransforms`                      | `lsp-grammarly-suggestions-readability-transforms`                          |
| `grammarly.config.suggestions.SentenceVariety`                            | `lsp-grammarly-suggestions-sentence-variety`                                |
| `grammarly.config.suggestions.SpacesSurroundingSlash`                     | `lsp-grammarly-suggestions-spaces-surrounding-slash`                        |
| `grammarly.config.suggestions.SplitInfinitive`                            | `lsp-grammarly-suggestions-split-infinitive`                                |
| `grammarly.config.suggestions.StylisticFragments`                         | `lsp-grammarly-suggestions-stylistic-fragments`                             |
| `grammarly.config.suggestions.UnnecessaryEllipses`                        | `lsp-grammarly-suggestions-unnecessary-ellipses`                            |
| `grammarly.config.suggestions.Variety`                                    | `lsp-grammarly-suggestions-variety`                                         |
| `grammarly.config.suggestions.Vocabulary`                                 | `lsp-grammarly-suggestions-vocabulary`                                      |

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
