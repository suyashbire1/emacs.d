;;; my-org-mode.el --- org-mode customizations for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs Startup File --- org-mode for Emacs
;;; Package --- Summary
;;; Code:

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :init
  (defun my-org-mode-hooks ()
    (visual-line-mode)
    (display-line-numbers-mode t)
    (flyspell-mode)
    (outline-minor-mode)
    (electric-pair-mode))
  (add-hook 'org-mode-hook 'my-org-mode-hooks)
  :general
  (leader
   "oa"  'org-agenda
   "ot"  'org-todo-list)
  :config
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t)

  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

  (defun org-babel-execute-and-next ()
    (interactive)
    (progn (org-babel-execute-src-block)
           (org-babel-next-src-block)))
  (setq org-highlight-latex-and-related '(entities script latex)
        org-tags-column 90)

  (local-leader org-mode-map
    ;; :keymaps 'org-mode-map
    "e"   'org-export-dispatch
    "t"   'org-hide-block-toggle
    "x"   'org-babel-execute-src-block
    "X"   'org-babel-execute-and-next
    "i"   'org-toggle-inline-images
    "d"   'org-babel-remove-result
    "o"   'org-open-at-point)
)

(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

(use-package org-re-reveal
  :hook (org-mode . load-org-re-reveal)
  :config
  (defun load-org-re-reveal ()
    (require 'org-re-reveal))
  (setq org-re-reveal-root "file:///some_loc/reveal.js")) ;; set_this

(provide 'my-org-mode)
;;; my-org-mode ends here
