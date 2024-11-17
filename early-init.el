;;; early-init.el --- Early customization            -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Timothy Miller

;; Author: Timothy Miller <cerebus2@gmail.com>
;; Keywords: local

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

;; Things that have to happen absolutely earliest.

;;; Code:
(add-to-list 'default-frame-alist '(undecorated-round . t))

(provide 'early-init)
;;; early-init.el ends here
