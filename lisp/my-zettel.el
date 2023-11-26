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

;; Package dependencies.
(unless (package-installed-p 'denote)
  (package-install 'denote))

(unless (package-installed-p 'biblio)
  (package-install 'biblio))

;; Process dependencies.
(server-start)
(require 'org-protocol)

;; Variables.
(defgroup my-zettelkasten nil
  "A Zettelkasten manager.")

(defcustom my-zettelkasten-dir (expand-file-name "~/org/zettelkasten")
  "Directory to store your Zettels."
  :type '(directory)
  :group 'my-zettelkasten
  :set #'(lambda (var value)
	   (unless (file-exists-p value)
	     (make-directory value t))
	   (set-default-toplevel-value var value)))

(defcustom my-zettelkasten-bibliography
  (concat
   (file-name-as-directory my-zettelkasten-dir) "references.bib")
  "BibTeX file to store references for your Zettel.

This value will be used for `org-cite-global-bibliography'"
  :type '(file)
  :group 'my-zettelkasten
  :set-after '(my-zettelkasten-dir))

(customize-set-variable 'bibtex-dialect 'biblatex)
(customize-set-variable 'denote-directory my-zettelkasten-dir)
(customize-set-variable 'org-cite-global-bibliography my-zettelkasten-bibliography)

;; Methods.
(defun my-zettelkasten-open-bibliography ()
  "Visit the Zettelkasten bibliography."
  (interactive)
  (find-file my-zettelkasten-bibliography))

;; Define biblio actions to add to the Zettelkasten reference file.
(with-eval-after-load 'biblio-core
  (defun my-biblio--selection-add-to-zettelkasten-bibliography-callback (bibtex entry)
    "Add BIBTEX (from ENTRY) to the Zettelkasten bibliography."
    (with-current-buffer (find-file-noselect my-zettelkasten-bibliography)
      (goto-char (point-max))
      (insert bibtex)
      (save-buffer))
    (message "Added bibtex entry for %S."
	     (biblio--prepare-title (biblio-alist-get 'title entry))))

  (defun my-selection-add-to-zettelkasten-bibliography-quit ()
    "Add BibTeX of current entry to Zettelkasten and close reesults."
    (interactive)
    (biblio--selection-forward-bibtex #'my-biblio--selection-add-to-zettelkasten-bibliography-callback t))

  (defun my-selection-add-to-zettelkasten-bibliography ()
    "Add BibTeX of current entry to Zettelkastens."
    (interactive)
    (biblio--selection-forward-bibtex #'my-biblio--selection-add-to-zettelkasten-bibliography-callback)))

;; Keyindings.
(bind-key (kbd "C-c z c") #'org-capture)
(bind-key (kbd "C-c z n") #'denote-open-or-create)
(bind-key (kbd "C-c z o") #'my-zettelkasten-open-bibliography)
(with-eval-after-load 'biblio-core
  ;; Redefine biblio's insert actions to add to the Zettelkasten reference file.
  (bind-key (kbd "I") #'my-selection-add-to-zettelkasten-bibliography-quit 'biblio-selection-mode-map)
  (bind-key (kbd "C-y") #'my-selection-add-to-zettelkasten-bibliography-quit 'biblio-selection-mode-map)
  (bind-key (kbd "i") #'my-selection-add-to-zettelkasten-bibliography 'biblio-selection-mode-map))

;; Capture templates.
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
		 (file my-zettelkasten-bibliography)
		 "@misc{,\n\ttitle = \"%:description\",\n\tauthor = \"%?\",\n\turl = \"%:link\",\n\tyear = %<%Y>,\n\tnote = \"Online; Accessed %<%d %B %Y>\"\n}"
		 :empty-lines 1 :prepare-finalize bibtex-clean-entry)))

;; Package
(provide 'my-zettel)
;;; my-zettel.el ends here
