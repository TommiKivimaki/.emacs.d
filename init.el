;;; package --- init.el

;; Author: Tommi Kivimäi <tommi@tommikivimaki.com>
;; Keywords: lisp, configuration

;;; Commentary:

;; Init file to load theconfiguration from 'configuration.org'

;;; Code:

(package-initialize)
(require 'org)

(defun load-config()
  "Running configuration in literate 'org-mode'."
  (interactive)
(org-babel-load-file
 (expand-file-name "configuration.org" user-emacs-directory)))

(load-config)

;;; init.el ends here
