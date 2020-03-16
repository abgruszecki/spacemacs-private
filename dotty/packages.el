;;; packages.el --- dotty layer packages file for Spacemacs.
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

(defconst dotty-packages
  '((dotty :location local)
    scala-mode
    lsp-mode

    (origami :toggle t
             :excluded nil)
    )
  "The list of Lisp packages required by the dotty layer.

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

(defun dotty/init-dotty ()
  (use-package dotty
    ;; :defer t
    :after scala-mode))

(defun dotty/init-scala-mode ()
  (use-package scala-mode
    :defer t))

(defun dotty/init-origami ()
  (use-package origami
    :defer t))

(defun dotty/pre-init-lsp-mode ()
  (spacemacs|use-package-add-hook lsp-mode
    :post-config (lsp-register-client
                  (make-lsp--client
                   :new-connection (lsp-stdio-connection (lambda ()
                                                           (dotty//start-dotty-lsp)))
                   :major-modes '(scala-mode)
                   :server-id 'dotty-lsp)))
  )

;;; packages.el ends here
