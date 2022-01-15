;;; my-vc.el --- My vc for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs Startup File --- my vc for Emacs
;;; Package --- Summary
;;; Code:

(use-package magit
  :commands (magit-status)
  :general
  (leader
   "g"   '(:ignore t :which-key "git")
   "gs"  'magit-status))

(provide 'my-vc)
;;; my-vc ends here
