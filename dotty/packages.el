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
    ))

(defun dotty/init-dotty ()
  (use-package dotty
    :after scala-mode
    :init
    (progn
      (evil-define-key 'normal dotty-trace-mode-map
        (kbd "RET") #'dotty-trace/hs-forward-toggle-node
        (kbd "<tab>") (lambda () (interactive) (re-search-forward "^#"))
        (kbd "<backtab>") (lambda () (interactive) (re-search-backward "^#"))
        (kbd "<C-tab>") (lambda () (interactive) (re-search-forward "^#!"))
        (kbd "<C-iso-lefttab>") (lambda () (interactive) (re-search-backward "^#!"))
        "g%" 'dotty/jump-to-matching-trace-header
        )

      (spacemacs/set-leader-keys-for-major-mode 'dotty-trace-mode
        "p" #'dotty/trace/peek-header
        "P" #'dotty/trace/preview-header
        "n" #'dotty/trace/narrow-header
        "<SPC>" #'dotty/jump-to-matching-trace-header
        "[" #'dotty-trace/jump-to-opening-header
        "]" #'dotty-trace/jump-to-closing-header
        "{" #'dotty-trace/jump-backwards-to-sibling-header
        "}" #'dotty-trace/jump-forwards-to-sibling-header
        )
      )))

;;; packages.el ends here
