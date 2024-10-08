;;; my-tex.el --- TeX-mode stuff                     -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Timothy J. Miller

;; Author: Timothy J. Miller <tmiller@mitre.org>
;; Keywords: convenience

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

;;

;;; Code:

(unless (package-installed-p 'auctex)
  (package-install 'auctex))

(setq TeX-auto-save t)
(setq TeX-parse-self t)

;; (load "auctex.el" nil t t)  ; Not supposed to be needed

(provide 'my-tex)
;;; my-tex.el ends here
