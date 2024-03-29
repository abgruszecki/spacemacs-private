;;; packages.el --- bespoke layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Aleksander Boruch-Gruszecki <gruszecki@Alex>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst bespoke-packages
  '(;; local
    (evil-org :location local)
    (bespoke-org-ref :location local)
    ;; post-inits
    auctex
    magit
    helm-projectile
    vterm
    )
)


(defun bespoke/init-evil-org ()
  (use-package evil-org
    :defer t
    :commands
    (evil-org-mode)
    :init
    (progn
      (add-hook 'org-mode-hook 'spacemacs//evil-org-mode)
      (setq evil-org-use-additional-insert t
            evil-org-key-theme `(textobjects
                                 navigation
                                 additional
                                 ,@(when org-want-todo-bindings '(todo)))))
    :config
    (spacemacs|hide-lighter evil-org-mode))
  )

(defun bespoke/init-bespoke-org-ref ()
  (use-package bespoke-org-ref
    :after org-ref
    :commands
    (bespoke-org-ref/top-helm-bibtex)
    )
  )

(defun bespoke/pre-init-magit ()
  (defhydra my-magit/smerge-hydra (:color red)
    "Hydra for in-buffer merge conflict resolution"
    ("q" nil "quit")
    ("p" #'smerge-prev)
    ("n" #'smerge-next)
    ("w" #'smerge-keep-upper)
    ("s" #'smerge-keep-lower)
    ("c" #'smerge-keep-current))

  (spacemacs|use-package-add-hook magit
    :post-config
    (spacemacs/set-leader-keys
      "gR" #'my-magit/smerge-hydra/body))
  )

(defun bespoke/pre-init-helm-projectile ()
  (spacemacs|use-package-add-hook helm-projectile
    :post-config
    (helm-add-action-to-source "Run rg in directory"
                               #'spacemacs/helm-files-do-rg
                               helm-source-projectile-directories-list
                               3)))

(defun bespoke/pre-init-vterm ()
  (spacemacs|use-package-add-hook vterm
    :post-config
    (define-key vterm-mode-map (kbd "<f1>") nil)
    (define-key vterm-mode-map (kbd "<f2>") nil)
    (define-key vterm-mode-map (kbd "<f3>") nil)
    (define-key vterm-mode-map (kbd "<f4>") nil)
    (define-key vterm-mode-map (kbd "<f5>") nil)
    (define-key vterm-mode-map (kbd "<f6>") nil)
    (define-key vterm-mode-map (kbd "<f7>") nil)
    (define-key vterm-mode-map (kbd "<f8>") nil)

    (define-key vterm-mode-map (kbd "<prior>") nil)
    (define-key vterm-mode-map (kbd "<next>") nil)

    (evil-define-key 'insert vterm-mode-map
      (kbd "<C-left>") #'vterm--self-insert
      (kbd "<C-right>") #'vterm--self-insert
      (kbd "C-w") #'vterm--self-insert
      (kbd "C-k") #'vterm--self-insert
      (kbd "C-u") #'vterm--self-insert
      (kbd "C-t") #'vterm--self-insert
      ))
  )

(defun bespoke/pre-init-auctex ()
  (spacemacs|use-package-add-hook auctex
    :post-init
    (remove-hook 'LaTeX-mode-hook 'LaTeX-math-mode)))

;;; packages.el ends here
