;;; init.el --- Initialization file for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; Package --- Summary
;;; Code:

(add-to-list 'load-path "~/.config/emacs/modules")
(require 'my-defaults)
(require 'my-evil)
(require 'my-themes)
(require 'my-org-mode)
(require 'my-completion)
(require 'my-prog-mode)
(require 'my-vc)
(require 'my-latex)
(require 'my-mode-line)
;; (require 'my-mail-setup)
;; (require 'my-elfeed)
