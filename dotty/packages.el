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
    lsp-mode
    ))

(defun dotty/init-dotty ()
  (use-package dotty
    :after scala-mode))

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
