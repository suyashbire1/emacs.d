;;; early-init.el --- Initialization file for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; This file is loaded before the package system and GUI is initialized,
;;; so in it you can customize variables that affect frame appearance as
;;; well as the package initialization process
;;; Package --- Summary
;;; Code:

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq package-enable-at-startup nil)
(setq straight-use-package-by-default t) ;; have use-package use straight.el by default.

(defvar bootstrap-version)
(let ((bootstrap-file
 (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
(bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
  (url-retrieve-synchronously
   "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
   'silent 'inhibit-cookies)
(goto-char (point-max))
(eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package) ;; install use-package via straight

;;; early-init ends here
