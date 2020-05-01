;;; packages.el --- bespoke-scala-mode layer packages file for Spacemacs.
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

(defconst bespoke-scala-mode-packages
  '(
    (scala-mode :location local)
    ))

(defun bespoke-scala-mode/init-scala-mode ()
  (use-package scala-mode
    :defer t
    :commands
    (scala-mode))

  (progn
    (add-to-list 'auto-mode-alist
                 '("\\.\\(scala\\|sbt\\)\\'" . scala-mode))
    (modify-coding-system-alist 'file "\\.\\(scala\\|sbt\\)\\'" 'utf-8)))

;;; packages.el ends here
