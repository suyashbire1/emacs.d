;;; my-defaults.el --- My defaults for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs Startup File --- my defaults for Emacs
;;; Package --- Summary
;;; Code:

(use-package emacs
  :init

  ;; sigh
  (defalias 'yes-or-no-p 'y-or-n-p)

  ; which directory to put backup files
  (setq backup-directory-alist `(("." . "~/.config/emacs/backups")))

  ;transform backups file name
  (setq auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list/" t)))

  ; toggle wrapping text at the 80th character
  (setq fill-column 80)

  ; redisplay never recenters cursor
  (setq scroll-conservatively 101)

  ; spell checker
  (setq ispell-program-name "hunspell")

  ;; help window modifications
  (setq help-window-select t)
  (customize-set-variable
        'display-buffer-alist
        '(("\\*Help\\*" display-buffer-below-selected)))

  ;; relative line numbers
  (with-eval-after-load 'display-line-numbers
    (setq display-line-numbers-type 'relative
          display-line-numbers-width-start t))

  ;; tabs are evil
  (setq indent-tabs-mode nil)

  ;; always follow symlinks in git dirs
  (setq vc-follow-symlinks t)

  ;; whitespace
  (setq whitespace-style '(face trailing))

  ;; utf8 in every nook and cranny
  (set-charset-priority 'unicode)
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

  ;; persist a custom file
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
  (load custom-file))

  ; fix =defvar= warnings
  (setq enable-local-variables :all)

  ;; use trash-cli rather than rm when deleting files.
  (setq delete-by-moving-to-trash t)

  ;; less noise when compiling elisp
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
  (setq native-comp-async-report-warnings-errors nil)
  (setq load-prefer-newer t)


  ;; font!
  (add-to-list 'default-frame-alist '(font . "Noto Sans Mono-12"))

  ; default modes
  (global-hl-line-mode 1)
  (blink-cursor-mode 0)
  (recentf-mode 1)
  (show-paren-mode t)

  ;; set_this
  ;; (setq initial-scratch-message
  ;;       (concat
  ;;        (shell-command-to-string
  ;;         "fortune calvin| cowsay -f calvin") "emacs-init-time: " (emacs-init-time)))
  )

(provide 'my-defaults)
;;; my-defaults ends here
