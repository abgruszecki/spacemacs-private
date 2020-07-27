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

(defun bespoke/pre-init-scala-mode ()
  )


;;; packages.el ends here
