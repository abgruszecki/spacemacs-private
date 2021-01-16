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
    sbt-mode
    (scala-mode :location local)
    ))

(defun bespoke-scala-mode/init-sbt-mode ()
  (use-package sbt-mode
    :defer t
    :config
    ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
    ;; allows for using SPACE in the minibuffer
    (substitute-key-definition
     'minibuffer-complete-word
     'self-insert-command
     minibuffer-local-completion-map)
    ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
    (setq sbt:program-options '("-Dsbt.supershell=false"))
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'scala-mode "mb" "sbt")
      (spacemacs/declare-prefix-for-mode 'scala-mode "mg" "goto")
      (spacemacs/set-leader-keys-for-major-mode 'scala-mode
        "b <tab>" #'sbt-start
        "b." #'sbt-hydra
        "bb" #'sbt-command
        "bC" #'sbt-do-clean
        "bc" #'sbt-do-compile
        "bt" #'sbt-do-test
        "br" #'sbt-do-run
        "bR" #'my-sbt/sbt-do-reload
        ))))

(defun bespoke-scala-mode/init-scala-mode ()
  (use-package scala-mode
    :defer t
    :commands (scala-mode)
    :config
    (progn
      ;; TODO are these better as custom bindings for o/O?
      (advice-add #'evil-open-below :after #'my-scala//handle-multiline-comment)
      (advice-add #'evil-open-above :after #'my-scala//handle-multiline-comment)
      (add-hook 'scala-mode-hook #'my-scala/init)
      (evil-define-key 'insert scala-mode-map
        (kbd "RET") #'my-scala/newline-and-indent-with-asterisk)

      (evil-define-key '(normal insert) scala-mode-map
        (kbd "<backtab>") #'bespoke-scala/ws-indent-backwards)

      (evil-define-key 'normal scala-mode-map
        "J" #'my-scala/scala-join-line)
      (spacemacs/set-leader-keys-for-major-mode 'scala-mode
        "<f9> `" #'my-scala/set-indent
        "<f9> TAB" #'bespoke-scala/toggle-indent)
      ))

  ;; TODO is this necessary if scala-mode has similar lines?
  (progn
    (add-to-list 'auto-mode-alist
                 '("\\.\\(scala\\|sbt\\|sc\\)\\'" . scala-mode))
    (modify-coding-system-alist 'file "\\.\\(scala\\|sbt\\|sc\\)\\'" 'utf-8)))

;;; packages.el ends here
