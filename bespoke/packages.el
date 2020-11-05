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
  '(
    (evil-org :location local)
    ;; post-inits
    magit
    vterm
    )
  "The list of Lisp packages required by the bespoke layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

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

;;; packages.el ends here
