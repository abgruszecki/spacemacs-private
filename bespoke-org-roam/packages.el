;;; packages.el --- bespoke-org-roam layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Aleksander Boruch-Gruszecki <gruszecki@Alex>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `bespoke-org-roam-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `bespoke-org-roam/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `bespoke-org-roam/pre-init-PACKAGE' and/or
;;   `bespoke-org-roam/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst bespoke-org-roam-packages
  '(org-roam
    org-roam-bibtex
    )
  "The list of Lisp packages required by the bespoke-org-roam layer.

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

(defun bespoke-org-roam/pre-init-org-roam ()
  ;; TODO why was this recommended?
  ;; (defconst org-roam-packages
  ;;   '(org-roam org-roam-bibtex))

  (defvar my-spacemacs/org-roam-prefix-map (make-sparse-keymap)
    "Prefix map for org-roam")
  (bind-keys :map my-spacemacs/org-roam-prefix-map
             ((kbd "<f2>")  . org-roam-node-find)
             ((kbd "<f3>")  . org-roam-ref-find)
             ((kbd "<tab>") . org-roam-buffer-display-dedicated)
             ((kbd "i")     . org-roam-insert))
  (spacemacs/set-leader-keys "<f2>" my-spacemacs/org-roam-prefix-map)

  (spacemacs|use-package-add-hook org-roam
    :post-config
    (setq org-roam-directory "~/org/roam"
          org-roam-db-location "~/.cache/org-roam/org-roam.db")

    (add-to-list 'org-roam-capture-templates
                 '("z" "zasób" plain #'org-roam-capture--get-point "%?"
                   :file-name "%<%Y%m%d%H%M%S>-${slug}"
                   :head "#+title: ${title}\n#+roam_tags: @zasób\n"
                   :unnarrowed t))

    (add-to-list 'org-roam-capture-templates
                 '("d" "domena" plain #'org-roam-capture--get-point "%?"
                   :file-name "%<%Y%m%d%H%M%S>-${slug}"
                   :head "#+title: ${title}\n#+roam_tags: @domena\n"
                   :unnarrowed t))

    (add-to-list 'org-roam-capture-templates
                 '("p" "pracka" plain #'org-roam-capture--get-point "%?"
                   :file-name "%<%Y%m%d%H%M%S>-${slug}"
                   :head "#+title: ${title}\n#+roam_key:\n#+roam_tags: @zasób\n"
                   :unnarrowed t))))

(defun bespoke-org-roam/init-org-roam ()
  (use-package org-roam
    :defer t
    ;; :hook (after-init . org-roam-mode)
    :commands (org-roam-setup)
    :init
    (progn
      (spacemacs/declare-prefix "aor" "org-roam")
      (spacemacs/declare-prefix "aord" "org-roam-dailies")
      (spacemacs/declare-prefix "aort" "org-roam-tags")
      (spacemacs/set-leader-keys
        "aordy" 'org-roam-dailies-goto-yesterday
        "aordt" 'org-roam-dailies-goto-today
        "aordT" 'org-roam-dailies-goto-tomorrow
        "aordd" 'org-roam-dailies-goto-date
        "aorc" 'org-roam-capture
        "aorf" 'org-roam-node-find
        "aorg" 'org-roam-graph
        "aori" 'org-roam-node-insert
        "aorl" 'org-roam-buffer-toggle
        "aorta" 'org-roam-tag-add
        "aortr" 'org-roam-tag-remove
        "aora" 'org-roam-alias-add)

      (spacemacs/declare-prefix-for-mode 'org-mode "mr" "org-roam")
      (spacemacs/declare-prefix-for-mode 'org-mode "mrd" "org-roam-dailies")
      (spacemacs/declare-prefix-for-mode 'org-mode "mrt" "org-roam-tags")
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "rdy" 'org-roam-dailies-goto-yesterday
        "rdt" 'org-roam-dailies-goto-today
        "rdT" 'org-roam-dailies-goto-tomorrow
        "rdd" 'org-roam-dailies-goto-date
        "rc" 'org-roam-capture
        "rf" 'org-roam-node-find
        "rg" 'org-roam-graph
        "ri" 'org-roam-node-insert
        "rl" 'org-roam-buffer-toggle
        "rta" 'org-roam-tag-add
        "rtr" 'org-roam-tag-remove
        "ra" 'org-roam-alias-add))
    :config
    (progn
      (spacemacs|hide-lighter org-roam-mode))))

(defun bespoke-org-roam/init-org-roam-bibtex ()
  (use-package org-roam-bibtex
    :after org-roam
    :hook (org-roam-mode . org-roam-bibtex-mode)
    :config
    (spacemacs|hide-lighter org-roam-bibtex-mode)))

;;; packages.el ends here
