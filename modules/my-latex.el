;;; my-latex.el --- My latex for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs Startup File --- my latex for Emacs
;;; Package --- Summary
;;; Code:

(use-package auctex
  :no-require t
  :mode ("\\.tex\\'" . LaTeX-mode)
  :init
  (setq TeX-parse-self t ; parse on load
        TeX-auto-save t  ; parse on save
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        TeX-source-correlate-start-server nil
        TeX-electric-sub-and-superscript t
        TeX-engine 'xetex ;; use xelatex by default
        TeX-save-query nil))

(use-package tex
  :straight auctex
  :config
  (defun my-LaTeX-mode-hooks ()
    (whitespace-mode)
    (show-paren-mode)
    (visual-line-mode)
    (flyspell-mode)
    (outline-minor-mode)
    (display-line-numbers-mode t)
    (TeX-source-correlate-mode t)
    (prettify-symbols-mode))
  (add-hook 'LaTeX-mode-hook 'my-LaTeX-mode-hooks)
  (add-hook 'TeX-after-compilation-finished-functions
              #'TeX-revert-document-buffer)
  (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))

  (local-leader LaTeX-mode-map
    "p" 'preview-at-point
    "b" 'TeX-command-master
    "a" 'TeX-command-run-all
    "v" 'TeX-view
    )
)

(use-package evil-tex
  :hook (LaTeX-mode . evil-tex-mode))

(use-package aas
  :hook (LaTeX-mode . aas-activate-for-major-mode)
  :hook (org-mode . aas-activate-for-major-mode))

(use-package laas
  :hook (LaTeX-mode . laas-mode)
  :config ; do whatever here
  (aas-set-snippets 'laas-mode
                    ;; ;; set condition!
                    ;; :cond #'texmathp ; expand only while in math
                    ;; "supp" "\\supp"
                    ;; "On" "O(n)"
                    ;; "O1" "O(1)"
                    ;; "Olog" "O(\\log n)"
                    ;; "Olon" "O(n \\log n)"
                    ;; ;; bind to functions!
                    ;; "Sum" (lambda () (interactive)
                    ;;         (yas-expand-snippet "\\sum_{$1}^{$2} $0"))
                    ;; "Span" (lambda () (interactive)
                    ;;          (yas-expand-snippet "\\Span($1)$0"))

                    ;; add accent snippets
                    :cond #'laas-object-on-left-condition
                    "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))))

;; (use-package cdlatex
;;   :hook
;;   (LaTeX-mode . turn-on-cdlatex)
;;   (org-mode   . turn-on-org-cdlatex)
;;   :config
;;   (setq cdlatex-command-alist
;;         '(("deg" "Insert ^{\\circ}" "^{\\circ}?" cdlatex-position-cursor nil t nil))))

(require 'my-bibtex)

(use-package reftex
  :straight (:type built-in)
  :config
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
  (setq reftex-plug-into-AUCTeX t
      reftex-default-bibliography (symbol-value 'my-bib-files))
  ;; (local-leader LaTeX-mode-map
  ;;     "r"   'reftex-reference)
)

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
)

(provide 'my-latex)
;;; my-latex ends here
