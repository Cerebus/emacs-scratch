;;; my-zettel.el --- Local Zettelkasten configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  cerebus2

;; Author: cerebus2 <cerebus2@gmail.com>
;; Keywords: local, bib, docs, extensions, hypermedia

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

;; Create and maintain a local Zettelkasten using denote, org-cite,
;; biblio, and bibtex.

;;; Code:
(unless (package-installed-p 'denote)
  (package-install 'denote))

(unless (package-installed-p 'biblio)
  (package-install 'biblio))

(require 'org-protocol)

(customize-set-variable 'bibtex-dialect 'biblatex)
(customize-set-variable 'denote-directory "~/org/zettelkasten")
(customize-set-variable 'org-cite-global-bibliography
			(expand-file-name "~/org/zettelkasten/references.bib"))
(customize-set-variable 'biblio-synchronous t)

(defun my-open-zettel-bibliography ()
  "Visit the Zettelkasten bibliography."
  (interactive)
  (find-file org-cite-global-bibliography))

(bind-key (kbd "C-c z c") #'org-capture)
(bind-key (kbd "C-c z n") #'denote-open-or-create)
(bind-key (kbd "C-c z o") #'my-open-zettel-bibliography)

(with-eval-after-load 'biblio
  (defun my-biblio--selection-add-to-global-bibliography-callback (bibtex entry)
    "Add BIBTEX (from ENTRY) to end of the Zettelkasten bibliography."
    (with-current-buffer (find-file-noselect org-cite-global-bibliography)
      (goto-char (point-max))
      (insert bibtex)
      (save-buffer))
    (message "Added bibtex entry for %S."
	     (biblio--prepare-title (biblio-alist-get 'title entry))))

  (defun my-selection-add-to-global-bibliography ()
    "Add BibTex of current entry at the end of the Zettelkasten bibliography."
    (interactive)
    (biblio--selection-forward-bibtex #'my-biblio--selection-add-to-global-bibliography-callback))

  (bind-key (kbd "I") #'my-selection-add-to-global-bibliography 'biblio-selection-mode-map))

(with-eval-after-load 'org-capture
  (add-to-list 'org-capture-templates
               '("n" "New note (with Denote)" plain
                 (file denote-last-path)
                 #'denote-org-capture
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t))
  (add-to-list 'org-capture-templates
	       '("w" "New reference (with org-protocol)" plain
	       (file org-cite-global-bibliography)
	       "@misc{,\n\ttitle = \"%:description\",\n\tauthor = \"%?\",\n\turl = \"%:link\",\n\tyear = %<%Y>,\n\tnote = \"Online; Accessed %<%d %B %Y>\"\n}"
	       :empty-lines 1 :prepare-finalize bibtex-clean-entry)))

(provide 'my-zettel)
;;; my-zettel.el ends here
