;;; test-grammarly.el --- Test moulde for lsp-grammarly  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh
;; Created date 2021-03-07 19:52:20

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
;; Test moulde for `unofficial-grammarly-language-server'
;;
;; See
;;   - https://github.com/znck/grammarly
;;   - https://github.com/emacs-grammarly/unofficial-grammarly-language-server
;;

;;; Code:

(setq lsp-grammarly-server-path
      (concat
       "c:/Users/JenChieh/Downloads/workspace/emacs-grammarly/"
       "unofficial-grammarly-language-server/unofficial-grammarly-language-server-2.cmd"))

;;; test-grammarly.el ends here
