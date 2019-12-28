(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")))

;; Packages to be installed
(defvar my-packages '(dired-narrow
		      markdown-mode
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
(setq css-indent-offset 2)

;; Enable using right side ALT as a modifier and not META key
(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

;; Get rid of start-up screen
(setq inhibit-startup-screen t)


;;
;; DIRED
;;
;; Use 'a' to visit a directory without creating a new buffer
(put 'dired-find-alternate-file 'disabled nil)
;; ls switches
;;(setq-default dired-listing-switches "-alh")
;; On C copy recursively by default
(setq dired-recursive-copies 'always)
;; dired-narrow starts fussy search with /
;;(http://pragmaticemacs.com/emacs/dynamically-filter-directory-listing-with-dired-narrow/)
(require 'dired)
(define-key dired-mode-map (kbd "/") 'dired-narrow-fuzzy)


;; Change font size
(defun zoom-in ()
  (interactive)
  (let ((x (+ (face-attribute 'default :height)
	      10)))
    (set-face-attribute 'default nil :height x)))

(defun zoom-out ()
  (interactive)
  (let ((x (- (face-attribute 'default :height)
	      10)))
    (set-face-attribute 'default nil :height x)))

(define-key global-map (kbd "C-1") 'zoom-in)
(define-key global-map (kbd "C-0") 'zoom-out)


;; Disable scroll bar
;;(scroll-bar-mode -1)
