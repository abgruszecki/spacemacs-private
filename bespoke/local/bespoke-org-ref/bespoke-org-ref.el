;;; bespoke-org-ref.el --- Bespoke adjustments to org-ref.  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Aleksander Boruch-Gruszecki

;; Author: Aleksander Boruch-Gruszecki <gruszecki@Alex>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'org-ref)
(require 'org-ref-helm)
(require 'helm-bibtex)

(defvar bespoke-org-ref//helm-source-bibtex
  (helm-build-sync-source "BibTeX entries"
    :header-name (lambda (name)
                   (format "%s: " name))
    :candidates 'helm-bibtex-candidates
    :filtered-candidate-transformer 'helm-bibtex-candidates-formatter
    :action (helm-make-actions
             ;; "Insert citation"            'helm-bibtex-insert-citation
             ;; "Insert reference"           'helm-bibtex-insert-reference
             ;; "Insert BibTeX key"          'helm-bibtex-insert-key
             ;; "Insert BibTeX entry"        'helm-bibtex-insert-bibtex
             ;; "Attach PDF to email"        'helm-bibtex-add-PDF-attachment
             "Edit notes"                 'helm-bibtex-edit-notes
             "Show entry"                 'helm-bibtex-show-entry
             "Open PDF, URL or DOI"       'helm-bibtex-open-any
             "Open URL or DOI in browser" 'helm-bibtex-open-url-or-doi
             "Add PDF to library"         'helm-bibtex-add-pdf-to-library))
  "Copied from [[helm-source-bibtex]]")

;; TODO is this necessary?
(setq bibtex-completion-notes-path "~/org/roam/")

;;;###autoload
(defun bespoke-org-ref/top-helm-bibtex ()
  "Copied from [[org-ref-helm-insert-cite-link]]."
  (interactive)
  (org-ref-save-all-bibtex-buffers)
  (let ((original-bibtex-completion-bibliography bibtex-completion-bibliography)
        (original-helm-source-bibtex helm-source-bibtex))
    (setf bibtex-completion-bibliography (org-ref-find-bibliography)
          helm-source-bibtex bespoke-org-ref//helm-source-bibtex)
    (unwind-protect
        (helm-bibtex)
      (setf bibtex-completion-bibliography original-bibtex-completion-bibliography
            helm-source-bibtex original-helm-source-bibtex))))

(org-roam-bibtex-mode 1)

(provide 'bespoke-org-ref)
;;; bespoke-org-ref.el ends here
