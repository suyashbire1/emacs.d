;;; init.el --- Initialization file for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; Package --- Summary
;;; Code:
(eval-and-compile
  (setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6))

(defvar temp--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ; which directory to put backups file
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ;transform backups file name
      fill-column 80   ; toggle wrapping text at the 80th character
      scroll-conservatively 101         ;
      ispell-program-name "aspell")

(with-eval-after-load 'tramp
  (setq tramp-default-method "ssh"))

(with-eval-after-load 'display-line-numbers
  (setq display-line-numbers-type 'relative
        display-line-numbers-width-start t))

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq-default indent-tabs-mode nil)
(global-hl-line-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode 0)
(winner-mode 1)
(put 'narrow-to-region 'disabled nil)

;;;We’re going to set the load-path ourselves and avoid calling (package-initilize) (for performance reasons) so we need to set package--init-file-ensured to true to tell package.el to not automatically call it on our behalf. Additionally we’re setting package-enable-at-startup to nil so that packages will not automatically be loaded for us since use-package will be handling that.
(eval-and-compile
  (setq load-prefer-newer t
        package-user-dir "~/.emacs.d/elpa"
        package--init-file-ensured t
        package-enable-at-startup nil)

  (unless (file-directory-p package-user-dir)
    (make-directory package-user-dir t))

  (setq load-path (append load-path (directory-files package-user-dir t "^[^.]" t))))


(eval-when-compile
  (require 'package)
  ;; tells emacs not to load any packages before starting up
  ;; the following lines tell emacs where on the internet to look up
  ;; for new packages.
  (setq package-archives '(("melpa"     . "https://melpa.org/packages/")
                           ("elpa"      . "https://elpa.gnu.org/packages/")
                           ("repo-org"  . "https://orgmode.org/elpa/")))
  ;; (package-initialize)
  (unless package--initialized (package-initialize t))

  ;; Bootstrap `use-package'
  (unless (package-installed-p 'use-package) ; unless it is already installed
    (package-refresh-contents) ; updage packages archive
    (package-install 'use-package)) ; and install the most recent version of use-package

  (require 'use-package)
  (setq use-package-always-ensure t))

(use-package which-key
  :config (which-key-mode 1))

(use-package general
  :after which-key
  :config
  (general-override-mode 1)

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


  (defun disable-all-themes ()
    "disable all active themes."
    (dolist (i custom-enabled-themes)
      (disable-theme i)))

  (defadvice load-theme (before disable-themes-first activate)
    (disable-all-themes))

  ;; Following lines to cycle through themes adapted from ivan's answer on
  ;; https://emacs.stackexchange.com/questions/24088/make-a-function-to-toggle-themes
  (setq my/themes (custom-available-themes))
  (setq my/themes-index 0)

  (defun my/cycle-theme ()
    "Cycles through my themes."
    (interactive)
    (setq my/themes-index (% (1+ my/themes-index) (length my/themes)))
    (my/load-indexed-theme))

  (defun my/load-indexed-theme ()
    (load-theme (nth my/themes-index my/themes)))

  (defun load-leuven-theme ()
    "Loads `leuven' theme"
    (interactive)
    (load-theme 'leuven))

  (defun load-dichromacy-theme ()
    "Loads `dichromacy' theme"
    (interactive)
    (load-theme 'dichromacy))

  (general-create-definer tyrant-def
    :states '(normal visual insert motion emacs)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")

  (general-create-definer despot-def
    :states '(normal insert)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")

  (general-define-key
    :keymaps 'key-translation-map
    "ESC" (kbd "C-g"))

  (general-def
    "C-x x" 'eval-defun)

  (tyrant-def

    ""     nil
    "c"   (general-simulate-key "C-c")
    "h"   (general-simulate-key "C-h")
    "u"   (general-simulate-key "C-u")
    "x"   (general-simulate-key "C-x")

    ;; Package manager
    "lp"  'list-packages

    ;; Theme operations
    "t"   '(:ignore t :which-key "themes")
    "tn"  'my/cycle-theme
    "tt"  'load-theme
    "tl"  'load-leuven-theme
    "td"  'load-dichromacy-theme

    ;; Quit operations
    "q"	  '(:ignore t :which-key "quit emacs")
    "qq"  'kill-emacs
    "qz"  'delete-frame

    ;; Buffer operations
    "b"   '(:ignore t :which-key "buffer")
    "bb"  'mode-line-other-buffer
    "bd"  'kill-this-buffer
    "b]"  'next-buffer
    "b["  'previous-buffer
    "bq"  'kill-buffer-and-window
    "bR"  'rename-file-and-buffer
    "br"  'revert-buffer

    ;; Window operations
    "w"   '(:ignore t :which-key "window")
    "wm"  'maximize-window
    "w/"  'split-window-horizontally
    "wv"  'split-window-vertically
    "wm"  'maximize-window
    "wu"  'winner-undo
    "ww"  'other-window
    "wd"  'delete-window
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
    "oa"  'org-agenda)

  (general-def 'normal doc-view-mode-map
    "j"   'doc-view-next-line-or-next-page
    "k"   'doc-view-previous-line-or-previous-page
    "gg"  'doc-view-first-page
    "G"   'doc-view-last-page
    "C-d" 'doc-view-scroll-up-or-next-page
    "C-f" 'doc-view-scroll-up-or-next-page
    "C-b" 'doc-view-scroll-down-or-previous-page)

  (general-def '(normal visual) outline-minor-mode-map
    "zn"  'outline-next-visible-heading
    "zp"  'outline-previous-visible-heading
    "zf"  'outline-forward-same-level
    "zB"  'outline-backward-same-level)

  (general-def 'normal package-menu-mode-map
    "i"   'package-menu-mark-install
    "U"   'package-menu-mark-upgrades
    "d"   'package-menu-mark-delete
    "u"   'package-menu-mark-unmark
    "x"   'package-menu-execute
    "q"   'quit-window)

  (general-def 'normal calendar-mode-map
    "h"   'calendar-backward-day
    "j"   'calendar-forward-week
    "k"   'calendar-backward-week
    "l"   'calendar-forward-day
    "0"   'calendar-beginning-of-week
    "^"   'calendar-beginning-of-week
    "$"   'calendar-end-of-week
    "["   'calendar-backward-year
    "]"   'calendar-forward-year
    "("   'calendar-beginning-of-month
    ")"   'calendar-end-of-month
    "SPC" 'scroll-other-window
    "S-SPC" 'scroll-other-window-down
    "<delete>" 'scroll-other-window-down
    "<"   'calendar-scroll-right
    ">"   'calendar-scroll-left
    "C-b" 'calendar-scroll-right-three-months
    "C-f" 'calendar-scroll-left-three-months
    "{"   'calendar-backward-month
    "}"   'calendar-forward-month
    "C-k" 'calendar-backward-month
    "C-j" 'calendar-forward-month
    "gk"  'calendar-backward-month
    "gj"  'calendar-forward-month
    "v"   'calendar-set-mark
    "."   'calendar-goto-today
    "q"   'calendar-exit))

(use-package suggest
  :general (tyrant-def "as" 'suggest))

(use-package ranger
  :hook (after-init . ranger-override-dired-mode)
  :general (tyrant-def "ar" 'ranger))

(use-package solarized-theme
  :init
  (setq solarized-scale-org-headlines nil)
  (custom-set-faces '(mode-line ((t (:background "gray10"
                                     :foreground "dark gray"
                                     :box nil
                                     :weight normal
                                     :height 1.0
                                     :width normal
                                     :underline nil
                                     :overline nil))))
                    '(mode-line-inactive ((t (:background "gray10"
                                              :foreground "dark gray"
                                              :box nil
                                              :weight normal
                                              :height 1.0
                                              :width normal
                                              :underline nil
                                              :overline nil)))))
  :hook (after-init . load-solarized-dark)
  :config
  (defun load-solarized-dark ()
      "Load the `solarized-dark' theme."
      (interactive)
      (load-theme 'solarized-dark))
  (defun load-solarized-light ()
      "Load the `solarized-light' theme."
      (interactive)
      (load-theme 'solarized-light))
  :general
  (tyrant-def "ts"  '(:ignore t :which-key "solarized")
              "tsl" 'load-solarized-light
              "tsd" 'load-solarized-dark))

(use-package evil
  :hook (after-init . evil-mode)
  :config
  (evil-set-initial-state 'shell-mode 'normal)
  (evil-set-initial-state 'doc-view-mode 'normal)
  (evil-set-initial-state 'package-menu-mode 'normal)
  (evil-set-initial-state 'biblio-selection-mode 'motion)
  (setq doc-view-continuous t)
  :general
  (tyrant-def
    "wh"  'evil-window-left
    "wl"  'evil-window-right
    "wj"  'evil-window-down
    "wk"  'evil-window-up
    "bN"  'evil-buffer-new
    "fd"  'evil-save-and-close)
  ('motion override-global-map
    "]b"  'evil-next-buffer
    "[b"  'evil-prev-buffer))

(use-package evil-numbers
  :after evil
  :general
  ('normal "C-=" 'evil-numbers/inc-at-pt
           "C--" 'evil-numbers/dec-at-pt))

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

(use-package evil-easymotion
  :after evil
  :config (evilem-default-keybindings "gs"))

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode 1)
  :general
  ('normal override-global-map
    "gc"  'evil-commentary
    "gC" 'evil-commentary-line))

(use-package evil-visualstar
  :after evil
  :config
  (setq evilmi-always-simple-jump t)
  (global-evil-visualstar-mode 1))

(use-package company
  :hook (after-init . global-company-mode)
  :config
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") #'company-select-previous-or-abort)
  (setq company-frontends '(company-echo-metadata-frontend
                            company-pseudo-tooltip-unless-just-one-frontend
                            company-preview-frontend))
  (setq company-backends '((company-capf
                            company-files)
                           (company-dabbrev-code company-keywords)
                            company-dabbrev company-yasnippet)))

(use-package company-quickhelp
  :defer 5
  :config (company-quickhelp-mode))

(use-package company-statistics
  :defer 5
  :config (company-statistics-mode))

(use-package projectile)

(defvar narrowing-system "ivy"
  "Sets the narrowing system to use - helm or ivy")

(use-package ivy
    :if (equal narrowing-system "ivy")
    :hook (after-init . ivy-mode)
    :config (setq ivy-use-virtual-buffers t
                ivy-count-format "(%d/%d) "
                ivy-initial-inputs-alist nil
                ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
    :commands (ivy-switch-buffer)
    :general
    (tyrant-def "bm"  'ivy-switch-buffer))

(use-package smex
  :if (equal narrowing-system "ivy"))

(use-package counsel
  :after (ivy)
  :general
  (tyrant-def
    "SPC" 'counsel-M-x
    "ff"  'counsel-find-file
    "fr"  'counsel-recentf
    "fL"  'counsel-locate))

(use-package flyspell-correct-ivy
  :if (equal narrowing-system "ivy")
  :commands (flyspell-correct-word-generic)
  :general
   (:keymaps '(flyspell-mode-map)
    :states '(normal visual)
    "zs" 'flyspell-correct-word-generic
    "z=" 'flyspell-buffer))

(use-package counsel-projectile
  :after (projectile ivy)
  :general
  (tyrant-def
   "p"   '(:ignore t :which-key "projectile")
   "pd"  'counsel-projectile-dired-find-dir
   "po"  'counsel-projectile-find-other-file
   "pf"  'counsel-projectile-find-file
   "fp"  'counsel-projectile-find-file
   "pb"  'counsel-projectile-switch-to-buffer
   "bp"  'counsel-projectile-switch-to-buffer))


(use-package helm
  :if (equal narrowing-system "helm")
  :hook (after-init . helm-mode)
  :config (require 'helm-config)
  :commands (helm-mini
             helm-find-files
             helm-recentf
             helm-locate
             helm-M-x
             helm-flyspell-correct)
  :general
  (tyrant-def
   "SPC" 'helm-M-x
   "bm"  'helm-mini
   "ff"  'helm-find-files
   "fr"  'helm-recentf
   "fL"  'helm-locate))

(use-package helm-flyspell
  :if (equal narrowing-system "helm")
  :commands (helm-flyspell-correct)
  :general
   (:keymaps '(flyspell-mode-map)
    :states '(normal visual)
    "zs" 'helm-flyspell-correct
    "z=" 'flyspell-buffer))

(use-package helm-projectile
  :after (projectile helm)
  :general
  (tyrant-def
   "p"   '(:ignore t :which-key "projectile")
   "pd"  'helm-projectile-dired-find-dir
   "po"  'helm-projectile-find-other-file
   "pf"  'helm-projectile-find-file
   "fp"  'helm-projectile-find-file
   "pb"  'helm-projectile-switch-to-buffer
   "bp"  'helm-projectile-switch-to-buffer))


(use-package flycheck
  :commands (flycheck-mode)
  :general
  (tyrant-def
   "e"   '(:ignore t :which-key "Errors")
   "en"  'flycheck-next-error
   "ep"  'flycheck-previous-error))

(use-package magit
  :commands (magit-status)
  :general
  (tyrant-def
   "g"   '(:ignore t :which-key "git")
   "gs"  'magit-status))

(use-package evil-magit
  :hook (magit-mode . evil-magit-init))

(use-package company-jedi
  :hook (python-mode . my-python-mode-hook)
  :config
  (defun my-python-mode-hook ()
    (setq-local company-backends '(company-jedi)))
  (if (eq system-type 'darwin)
    (setq python-shell-exec-path "/usr/local/miniconda3/bin"
          python-shell-interpreter "/usr/local/miniconda3/bin/python")
    (setq python-shell-interpreter "python3"))
  :general
   ('(normal visual) python-mode-map
    "]]"  'python-nav-forward-defun
    "[["  'python-nav-backward-defun
    "gj"  'python-nav-forward-block
    "gk"  'python-nav-backward-block)
  (despot-def python-mode-map
   ""      nil
   "mg"   'jedi:goto-definition
   "mb"   'jedi:goto-definition-pop-marker))

(use-package yapfify
  :hook (python-mode . yapf-mode))

(use-package sphinx-doc
  :hook (python-mode . sphinx-doc-mode)
  :general
  (despot-def python-mode-map
   "ms"   'sphinx-doc))

(use-package yasnippet
  :hook ((prog-mode org-mode) . yas-minor-mode)
  :general
  (tyrant-def
   "y"   '(:ignore t :which-key "yasnippet")
   "yi"  'yas-insert-snippet
   "yv"  'yas-visit-snippet-file
   "yn"  'yas-new-snippet))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package org
  :defer t
  :mode ("\\.org\\'" . org-mode)
  :ensure org-plus-contrib
  :init
  (defun my-org-mode-hooks ()
    (visual-line-mode)
    (display-line-numbers-mode t)
    (flyspell-mode)
    (outline-minor-mode)
    (electric-pair-mode))
  (add-hook 'org-mode-hook 'my-org-mode-hooks)
  :general
  (despot-def org-mode-map
    "me"   'org-export-dispatch
    "mt"   'org-hide-block-toggle
    "mx"   'org-babel-execute-src-block
    "mX"   'org-babel-execute-and-next
    "md"   'org-babel-remove-result)
  :config
  (if (not (featurep 'ox-bibtex))
      (require 'ox-bibtex))
  (defun org-babel-execute-and-next ()
    (interactive)
    (progn (org-babel-execute-src-block)
           (org-babel-next-src-block)))
  (setq org-highlight-latex-and-related '(entities script latex)
        org-tags-column 90)
  (add-to-list 'org-structure-template-alist
               '("<ip" "#+BEGIN_SRC ipython :session ? :results raw
  drawer\n\n#+END_SRC"
                 "<src lang=\"?\">\n\n</src>")))

(use-package ob-ipython
  :hook (org-mode . my-ob-ipython-hook)
  :config
  (defun my-ob-ipython-hook ()
    (with-eval-after-load 'org-babel
      (progn
        (require 'ob-ipython)
        (setq ob-ipython-suppress-execution-count t)
        (add-to-list 'company-backends 'company-ob-ipython))))

  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages
           '((python  . t)
             (ipython . t))))
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        ob-ipython-suppress-execution-count t)
  
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images
            'append)
  :general
  (tyrant-def org-mode-map
    "mb"   (general-simulate-key "C-c C-v")))

;; (use-package org-ref
;;   :hook (org-mode . load-org-ref)
;;   :config
;;   (defun load-org-ref ()
;;     (require 'org-ref))
;;   (setq org-ref-default-bibliography '("~/Zotero/papers.bib")
;;         org-ref-pdf-directory "~/gdrve2/pdfs2/"
;;         org-ref-bibliography-notes "~/Zotero/pdfs/notes.org"
;;         org-ref-default-citation-link "citet")
;;   :general
;;   (despot-def org-mode-map
;;     "mc"   'org-ref-helm-insert-cite-link
;;     "mr"   'org-ref-helm-insert-ref-link
;;     "ml"   'org-ref-helm-insert-label-link))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package org-pomodoro
  :general
  (despot-def org-mode-map
   "mps"  'org-pomodoro))

(use-package ox-reveal
  :hook (org-mode . load-org-reveal)
  :config
  (defun load-org-reveal ()
    (if (not (featurep 'ox-reveal))
        (require 'ox-reveal))))

(use-package tex
  :defer t
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :ensure auctex
  :init
  (add-hook 'LaTeX-mode-hook 'my-LaTeX-mode-hooks)
  (defun my-LaTeX-mode-hooks ()
    (whitespace-mode)
    (show-paren-mode)
    (visual-line-mode)
    (flyspell-mode)
    (outline-minor-mode)
    (display-line-numbers-mode t)
    (TeX-source-correlate-mode t))
  :config
  (setq TeX-auto-save t
        TeX-source-correlate-start-server 'synctex)
  (defun insert-file-name-base (file)
    "Read file name and insert it at point.
    With a prefix argument, insert only the non-directory part."
    (interactive "FFile:")
    (insert (file-name-base file)))
  :general
  (despot-def TeX-mode-map
    "mb"   'TeX-command-master
    "ma"   'TeX-command-run-all
    "mv"   'TeX-view
    "mc"   'reftex-citation
    "mr"   'reftex-reference
    "mf"   'insert-file-name-base))

(use-package reftex
  :hook (LaTeX-mode . turn-on-reftex)
  :config
  (setq reftex-plug-into-AUCTeX t))

(use-package cdlatex
  :hook
  (LaTeX-mode . turn-on-cdlatex)
  (org-mode   . turn-on-org-cdlatex)
  :config
  (setq cdlatex-command-alist
        '(("ct" "Insert \\citet" "\\citet{?}" cdlatex-position-cursor nil t nil)
          ("cp" "Insert \\citep" "\\citep{?}" cdlatex-position-cursor nil t nil)
          ("eref" "Insert \\eqref" "\\eqref{eq?}" cdlatex-position-cursor nil t nil)
          ("fref" "Insert \\ref" "\\ref{fig?}" cdlatex-position-cursor nil t nil)
          ("sref" "Insert \\ref" "\\ref{sec?}" cdlatex-position-cursor nil t nil))))

(use-package auctex-latexmk
  :hook (LaTeX-mode . auctex-latexmk-setup)
  :config
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

(use-package company-reftex
  :after company
  :hook (reftex-mode . load-company-reftex)
  :config
  (defun load-company-reftex ()
    (add-to-list 'company-backends
                 '(company-reftex-citations
                   company-reftex-labels))))

(use-package company-bibtex
  :after company
  :hook (org-mode . load-company-bibtex)
  :config
  (defun load-company-bibtex ()
    (add-to-list 'company-backends 'company-bibtex))

  (if (eq system-type 'darwin)
    (setq company-bibtex-bibliography
          '("~/Documents/bib_file/papers.bib"
            "~/Documents/bib_file/selfpapers.bib"))
    (setq company-bibtex-bibliography
          '("~/bibtex/papers.bib"
            "~/bibtex/selfpapers.bib")))
  (setq company-bibtex-org-citation-regex (regexp-opt '("cite:" "\\cite{"))))

(use-package ivy-bibtex
  :after (ivy)
  :defines bibtex-completion-bibliography
  :config
  (set-bibtex-config)
  :general
  (tyrant-def "ab" 'ivy-bibtex))

(use-package helm-bibtex
  :after (helm)
  :defines bibtex-completion-bibliography
  :config
  (set-bibtex-config)
  :general
  (tyrant-def "ab" 'helm-bibtex))

(defun set-bibtex-config ()
  (setq bibtex-completion-bibliography
        '("~/bibtex/papers.bib"
          "~/bibtex/selfpapers.bib")
        bibtex-completion-library-path '("~/gdrive2/bibtex/pdfs/")
        bibtex-completion-notes-path "~/bibtex/notes.org"
        bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-pdf-symbol "⌘"
        bibtex-completion-notes-symbol "✎"
        bibtex-autokey-titlewords 2
        bibtex-autokey-titlewords-stretch 0
        bibtex-autokey-names-stretch 1)
  (tyrant-def bibtex-mode-map
    "mi" 'doi-insert-bibtex
    "mc" 'bibtex-clean-entry)
  (general-def 'normal biblio-selection-mode-map
    "j" 'biblio--selection-next
    "k" 'biblio--selection-previous))


(use-package shell-pop
  :commands (shell-pop)
  :config (setq shell-pop-shell-type '("shell"
                                       "*shell*"
                                       (lambda nil (shell))))
  :general
  (tyrant-def "'" 'shell-pop))

(setq whitespace-style '(face trailing))

(defun my-prog-mode-hook ()
  (auto-fill-mode)
  (show-paren-mode)
  (whitespace-mode)
  (electric-pair-mode)
  (flycheck-mode)
  (display-line-numbers-mode))

(add-hook 'prog-mode-hook 'my-prog-mode-hook)
(setq before-save-hook 'nil)
;(add-hook 'before-save-hook 'delete-trailing-whitespace)

;(use-package pdf-tools
;  :defer 5
;  :config
;  (pdf-tools-install)
;  :general
;  (general-def 'normal pdf-view-mode-map
;    "j"   'pdf-tools-next-line-or-next-page
;    "k"   'pdf-tools-previous-line-or-previous-page
;    "gg"  'pdf-tools-first-page
;    "G"   'pdf-tools-last-page
;    "C-d" 'pdf-tools-scroll-up-or-next-page
;    "C-f" 'pdf-tools-scroll-up-or-next-page
;    "C-b" 'pdf-tools-scroll-down-or-previous-page))

(use-package telephone-line
  :config
  ; (setq telephone-line-primary-left-separator 'telephone-line-abs-left
  ; telephone-line-primary-right-separator 'telephone-line-abs-right)

  (telephone-line-defsegment my-vc-info ()
  (when vc-mode
  (cond
  ((string-match "Git[:-]" vc-mode)
  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
  (concat "" (format " %s" branch))))
  ((string-match "SVN-" vc-mode)
  (let ((revision (cadr (split-string vc-mode "-"))))
  (concat "" (format "SVN-%s" revision))))
  (t (format "%s" vc-mode)))))

  (telephone-line-defsegment* my-airline-position-segment (&optional lines columns)
    (let* ((l (number-to-string (if lines lines 1)))
           (c (number-to-string (if columns columns 2))))
      (if (eq major-mode 'paradox-menu-mode)
          (telephone-line-raw mode-line-front-space t)
          (concat " " "%" l "l:%" c "c"))))

  (setq telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment))
          (accent . (my-vc-info
                     telephone-line-process-segment))
          (nil    . (telephone-line-buffer-segment
                     telephone-line-projectile-segment))))
  (setq telephone-line-rhs
        '((nil    . (telephone-line-flycheck-segment
                     telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))
          (nil    . (telephone-line-hud-segment
                     my-airline-position-segment))))

  (setq display-time-format "%b %d %a %R")
  (setq display-time-default-load-average nil)
  (setq display-time-use-mail-icon t)
  (setq display-time-mail-file t)
  (display-time-mode t)

  (telephone-line-mode 1))


(eval-when-compile
(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
(load custom-file)))

(eval-and-compile
(add-hook 'emacs-startup-hook '(lambda ()
                (setq gc-cons-threshold 16777216
                        gc-cons-percentage 0.1
                        file-name-handler-alist temp--file-name-handler-alist))))
(setq initial-scratch-message (concat "Startup time: " (emacs-init-time)))
(provide 'init)
;;; init ends here
