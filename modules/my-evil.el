;;; my-evil.el --- evil customizations for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs Startup File --- evil for Emacs
;;; Package --- Summary
;;; Code:

(use-package which-key
  :config (which-key-mode 1))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :custom
  (evil-want-C-u-scroll t) ;; allow scroll up with 'C-u'
  (evil-want-C-d-scroll t) ;; allow scroll down with 'C-d'
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  ;; :custom
  ;; (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode) ;; globally enable evil-commentary
  )

(use-package general
  :after evil
  :config
  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))
  ;; (general-override-mode 1)

  ;; leader key SPC similar to spacemacs
  (general-create-definer leader
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "C-SPC" ;; access leader in insert and mode
    )

  ;; local leader key SPC similar to spacemacs
  (general-create-definer local-leader
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC m"
    :non-normal-prefix "C-SPC m" ;; access local leader in insert mode
    )

  ;; some useful functions
  (defun find-user-init-file ()
    "Edit the `user-init-file', in same window."
    (interactive)
    (find-file user-init-file))

  (defun load-user-init-file ()
    "Load the `user-init-file', in same window."
    (interactive)
    (load-file user-init-file))

  ;;Taken from http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/
  (defun rename-file-and-buffer ()
    "Rename the current buffer and file it is visiting."
    (interactive)
    (let ((filename (buffer-file-name)))
      (if (not (and filename (file-exists-p filename)))
          (message "Buffer is not visiting a file!")
        (let ((new-name (read-file-name "New name: " filename)))
          (cond
           ((vc-backend filename) (vc-rename-file filename new-name))
           (t
            (rename-file filename new-name t)
            (set-visited-file-name new-name t t)))))))


  (general-def
    "C-x x" 'eval-defun)

  (leader

    ""     nil
    "c"   (general-simulate-key "C-c")
    "h"   (general-simulate-key "C-h")
    "x"   (general-simulate-key "C-x")
    "u"   '(universal-argument :wk "C-u")

    ;; Theme operations
    "t"   '(:ignore t :which-key "themes")
    "tn"  'my/cycle-theme
    "tt"  'load-theme
    "tl"  'load-leuven-theme
    "td"  'load-dichromacy-theme

    ;; Quit operations
    "q"	  '(:ignore t :which-key "quit emacs")
    "qz"  'kill-emacs
    "qq"  'delete-frame

    ;; Buffer operations
    "b"   '(:ignore t :which-key "buffer")
    "bb"  'mode-line-other-buffer
    "bd"  'kill-this-buffer
    "b]"  'next-buffer
    "b["  'previous-buffer
    "bq"  'kill-buffer-and-window
    "bR"  'rename-file-and-buffer
    "br"  'revert-buffer
    "bm"  'switch-to-buffer

    ;; Window operations
    "w"   '(:ignore t :which-key "window")
    "wn"  'evil-window-vnew
    "w>"  'evil-window-increase-width
    "w<"  'evil-window-decrease-width
    "w+"  'evil-window-increase-height
    "w-"  'evil-window-increase-height
    "w/"  'evil-window-vsplit
    "wv"  'evil-window-split
    "ww"  'evil-window-next
    "wd"  'evil-window-delete
    "wD"  'delete-other-windows

    ;; File operations
    "f"   '(:ignore t :which-key "files")
    "fc"  'write-file
    "fe"  '(:ignore t :which-key "emacs")
    "fed" 'find-user-init-file
    "feR" 'load-user-init-file
    "fj"  'dired-jump
    "fl"  'find-file-literally
    "fR"  'rename-file-and-buffer
    "fs"  'save-buffer

    ;; Applications
    "a"   '(:ignore t :which-key "Applications")
    "ad"  'dired
    ":"   'shell-command
    ";"   'eval-expression
    "ac"  'calendar

    "wh"  'evil-window-left
    "wl"  'evil-window-right
    "wj"  'evil-window-down
    "wk"  'evil-window-up
    "bN"  'evil-buffer-new
    "fd"  'evil-save-and-close
    )
  )

(use-package undo-fu
  :general
  ('normal "C-r" 'undo-fu-only-redo))

(provide 'my-evil)
;;; my-evil ends here
