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
    (bespoke-org-roam-bibtex :location local)
    ))

(defvar bspk//capture-registers '())
(defun bspk//capture-register (key)
  (if-let* ((cell (plist-member bspk//capture-registers key))
            (entry (when cell (cadr cell))))
      entry
    (format "<Empty: %s>" key)))

(defun bespoke-org-roam/pre-init-org-roam ()
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

    (setq org-roam-node-display-template "${title:120} ${tags:30}")

    (setq org-roam-capture-templates
          `(("z" "zasób" plain "%?"
             :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n#+filetags: :@zasób:\n")
             :unnarrowed t)

            ("l" "log")
            ("ll" "log/doktorat" entry "* %^{Verb|Read|Seen|Re-read}: [[%(bspk//capture-register :url)][%(bspk//capture-register :title)]]  %(org-set-tags \":log:\")%?"
             :if-new (file+olp "~/org/roam/doktorat.org" ("Notes"))
             :prepend t
             :prepare-finalize ,(lambda ()
                                  (beginning-of-buffer)
                                  (org-align-tags))
             )

            ("p" "pracka" plain "%?"
             :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n#+filetags: :@pracka:@zasób:\n\n* ZTK\n* Wiedza")
             :unnarrowed t)

            ("s" "standalone" plain "%?"
             :if-new (file+head "standalone/%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n#+filetags: :@standalone:\n")
             :unnarrowed t)

            ("d" "domena" plain "%?"
             :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n#+filetags: :@domena:\n")
             :unnarrowed t)
            ))
    ))

(defun bespoke-org-roam/init-org-roam ()
  (use-package org-roam
    ;; :defer t ;; temporarily load eagerly to make patching easier
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
      (spacemacs|hide-lighter org-roam-mode)))

  (cl-defun org-roam-node-insert-section (&key source-node point properties)
    "Insert section for a link from SOURCE-NODE to some other node.
The other node is normally `org-roam-buffer-current-node'.

SOURCE-NODE is an `org-roam-node' that links or references with
the other node.

POINT is a character position where the link is located in
SOURCE-NODE's file.

PROPERTIES (a plist) contains additional information about the
link.

Despite the name, this function actually inserts 2 sections at
the same time:

1. `org-roam-node-section' for a heading that describes
   SOURCE-NODE. Acts as a parent section of the following one.

2. `org-roam-preview-section' for a preview content that comes
   from SOURCE-NODE's file for the link (that references the
   other node) at POINT. Acts a child section of the previous
   one."
    (magit-insert-section section (org-roam-node-section)
      (let ((outline (if-let ((outline (plist-get properties :outline)))
                         (mapconcat #'org-link-display-format outline " > ")
                       "Top")))
        (insert (concat (propertize (org-roam-node-title source-node)
                                    'font-lock-face 'org-roam-title)
                        (format " (%s)"
                                (propertize outline 'font-lock-face 'org-roam-olp)))))
      (magit-insert-heading)
      (oset section node source-node)
      (magit-insert-section section (org-roam-preview-section)
        (insert (save-excursion
                  (org-roam-fontify-like-in-org-mode
                   (org-roam-preview-get-contents (org-roam-node-file source-node) point)))
                "\n")
        (oset section file (org-roam-node-file source-node))
        (oset section point point)
        (insert ?\n)))))

(defun bespoke-org-roam/init-org-roam-bibtex ()
  ;; (use-package org-roam-bibtex
  ;;   :after org-roam
  ;;   :hook (org-roam-mode . org-roam-bibtex-mode)
  ;;   :config
  ;;   (spacemacs|hide-lighter org-roam-bibtex-mode))
  )

(defun bespoke-org-roam/init-bespoke-org-roam-bibtex ()
  ;; (message "Entered %s" 'bespoke-org-roam/init-bespoke-org-roam-bibtex)
  ;; (cl-macrolet ((check-loaded (pkg-sym)
  ;;                             `(message "Loaded %s? ==> %s" ,pkg-sym (not (eq nil (memq ,pkg-sym package-activated-list))))))
  ;;   (check-loaded 'org-mode)
  ;;   (check-loaded 'org-roam)
  ;;   (check-loaded 'org-roam-bibtex))

  (use-package bespoke-org-roam-bibtex
    ;; :after org-roam-bibtex
    )
  )

;;; packages.el ends here
