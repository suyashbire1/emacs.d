;;; my-prog-mode.el --- My prog-mode for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs Startup File --- my prog-mode for Emacs
;;; Package --- Summary
;;; Code:

;; programming mode hook
(defun my-prog-mode-hook ()
  ;; (auto-fill-mode)
  (show-paren-mode)
  (whitespace-mode)
  (electric-pair-mode)
  (flymake-mode)
  (display-line-numbers-mode))
(add-hook 'prog-mode-hook 'my-prog-mode-hook)

(use-package yapfify
  :hook (python-mode . yapf-mode))

(use-package sphinx-doc
  :hook (python-mode . sphinx-doc-mode)
  :general
  (local-leader python-mode-map
   "ms"   'sphinx-doc))

;; Requires emacs built with module support
(use-package jupyter
  :hook (org-mode . my-jupyter-hook)
  :config
  (defun my-jupyter-hook ()
    (add-to-list 'org-structure-template-alist
                 '("jp" "#+BEGIN_SRC jupyter-python :session ? :async yes\n\n#+END_SRC"
                        "<src lang=\"?\">\n\n</src>"))
    (require 'jupyter)
    (jupyter-org-interaction-mode))
  (org-babel-do-load-languages 'org-babel-load-languages
    (append org-babel-load-languages
            '((jupyter  . t))))

  (local-leader org-mode-map
    "b"   (general-simulate-key "C-c C-v"))
  (general-def '(motion insert) jupyter-org-interaction-mode-map
   "C-RET" 'org-babel-execute-src-block
   "S-RET" 'org-babel-execute-and-next))

(use-package julia-mode
  :mode ("\\.jl\\'" . julia-mode))

(use-package web-mode
  :mode ("\\.html\\'" . web-mode)
  :config
  (setq web-mode-enable-current-column-highlight t))

(use-package emmet-mode
  :hook (web-mode  . emmet-mode)
        (css-mode  . emmet-mode))

(use-package markdown-mode
  :defer t
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(provide 'my-prog-mode)
;;; my-prog-mode ends here
