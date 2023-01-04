(require 'org-roam-bibtex)

(defun bsp-orb/cite-title ()
  (interactive)
  (bibtex-completion-init)
  (let* ((link (org-element-context))
         citekey entry)
    (unless (eq (org-element-type link) 'link)
      (user-error "Point not at a link!"))
    (setf citekey (org-element-property :path link))
    (when (s-starts-with? "&" citekey)
      (setf citekey (seq-drop citekey 1)))
    (setf entry (bibtex-completion-get-entry citekey))
    (unless entry
      (user-error "No BibTeX entry for key `%s'!" citekey))
    (let ((result (bibtex-completion-clean-string (bibtex-completion-get-value "title" entry))))
      (if (called-interactively-p t)
          (kill-new result)
        result))))

(defun bsp-orb/eldoc-documentation-function (callback)
  (when (eq (get-text-property (point) 'face) 'org-ref-cite-face)
    (funcall callback (bsp-orb/cite-title))))

(defun bsp-orb//init ()
  (add-hook 'eldoc-documentation-functions #'bsp-orb/eldoc-documentation-function 0 t))

(add-hook 'org-mode-hook #'bsp-orb//init)

(provide 'bespoke-org-roam-bibtex)
