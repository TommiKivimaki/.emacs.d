;;(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;;'(package-selected-packages (quote (markdown-mode swift-mode))))
;;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;;)

;;(require 'package)
;;(add-to-list 'package-archives
;;             '("melpa-stable" . "https://stable.melpa.org/packages/"))
;;(package-initialize)





(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; Packages to be installed
(defvar my-packages '(markdown-mode
		      swift-mode))

;; Install packages
(package-initialize)
(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-refresh-contents)
    (package-install p))
  (add-to-list 'package-selected-packages p))









;; Set default theme
(load-theme 'tango-dark)

;; Display line numbers
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))


;; Set default window size
(if (display-graphic-p)
   (progn
     (setq initial-frame-alist
	    '(
	      (tool-bar-lines . 0)
	      (width . 95)
	      (height . 92)))
     (setq default-frame-alist
	    '(
	      (tool-bar-lines . 0)
	      (width . 95)
	      (height . 92))))
 (progn
   (setq initial-frame-alist '((tool-bar-lines . 0)))
   (setq default-frame-alist '((tool-bar-lines . 0)))))



;; Set garbage collection to 20MB instead of 0.76MB default.
;; Speeds up certain operations
(setq gc-cons-threshold 20000000)

;; Direct auto save files
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Set back-up directory
 (setq backup-directory-alist `(("." . ,(expand-file-name
 					(concat user-emacs-directory "backups")))))

;; Set how many backups are stored
(setq make-backup-files t
     backup-by-copying t
     delete-old-versions t
     kept-new-versions 2
     kept-old-versions 3
     version-control t)

;; Allow following symlinks
(setq vc-follow-symlinks t)



;; Set indentation for CSS
;;(setq css-indent-offset 2)

;; Enable using right side ALT as a modifier and not META key
(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

;; Get rid of start-up screen
;;(setq inhibit-startup-screen t)
