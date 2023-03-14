;;; my-auth.el --- local security related things            -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Timothy J. Miller

;; Author: Timothy J. Miller <tmiller@mitre.org>
;; Keywords: convenience, local

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

;; Local security issues

;;; Code:

;; Integration with the ~pass~ command
(require 'auth-source-pass)
(auth-source-pass-enable)

;; Local trust anchors, if any
(with-eval-after-load 'gnutls
  (if (file-exists-p "~/.trust/certs.pem")
      (add-to-list 'gnutls-trustfiles "~/.trust/certs.pem")
    )
  )

(provide 'my-auth)
;;; my-auth.el ends here
