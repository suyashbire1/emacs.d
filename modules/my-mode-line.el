;;; my-mode-line.el --- Mode line for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs Startup File --- mode line for Emacs
;;; Package --- Summary
;;; Code:
;; (use-package all-the-icons)

(use-package hide-mode-line
  :config
  (add-hook 'help-mode-hook #'hide-mode-line-mode))

(use-package telephone-line
 :hook (after-init . telephone-line-mode)
 :config
 (setq telephone-line-primary-left-separator 'telephone-line-nil
       telephone-line-primary-right-separator 'telephone-line-nil
       telephone-line-secondary-left-separator 'telephone-line-nil
       telephone-line-secondary-right-separator 'telephone-line-nil)

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
         (concat "  " "%" l "l:%" c "c"))))

 (setq telephone-line-lhs
       '((evil   . (telephone-line-evil-tag-segment))
         (accent . (telephone-line-buffer-modified-segment))
         (nil    . (telephone-line-projectile-buffer-segment
                    telephone-line-process-segment
		    telephone-line-minor-mode-segment))))
 (setq telephone-line-rhs
       '((nil    . (telephone-line-misc-info-segment))
         (nil    . (my-vc-info))
         (nil    . (telephone-line-hud-segment
                    my-airline-position-segment))))

 ;; (setq display-time-format "%b %d %a %R")
 ;; (setq display-time-default-load-average nil)
 ;; (setq display-time-use-mail-icon t)
 ;; (setq display-time-mail-file t)
 ;; (display-time-mode t)
 )


;; (use-package doom-modeline
;;   :init (doom-modeline-mode 1)
;;   :config
;;   (setq doom-modeline-height 25)
;;   (setq doom-modeline-buffer-file-name-style 'truncate-except-project)
;;   (setq doom-modeline-mu4e t)
;;   (setq inhibit-compacting-font-caches t)
;;   (setq doom-modeline-icon t)
;;   (setq doom-modeline-modal-icon nil)
;;   )

(provide 'my-mode-line)
;;; my-mode-line.el ends here
