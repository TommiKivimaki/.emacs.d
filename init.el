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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (which-key web-mode swift-mode markdown-mode magit flycheck dired-narrow beacon))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
