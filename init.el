(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")))

;; Packages to be installed
(defvar my-packages '(beacon
		      dired-narrow
		      flycheck
		      magit
		      markdown-mode
		      swift-mode
		      which-key))

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


;; Remember cursor position when reopening files
(setq save-place-file (expand-file-name "places" user-emacs-directory))
(setq save-place-forget-unreadable-files nil)
(save-place-mode 1)


;; Enable narrow-to-region mode. It hides the lines which are not in a region
(put 'narrow-to-region 'disabled nil)

;; Use visible feedback and not the system bell
(setq visible-bell t)


;;
;; MAGIT
;; https://github.com/magit/magit/wiki/Cheatsheet
(global-set-key (kbd "C-x g") 'magit-status)


;;
;; BEACON
;; https://github.com/Malabarba/beacon
;; Visual cursor position when scrolling and jumping
(beacon-mode 1)
(setq beacon-blink-duration 0.8)


;;
;; WHICH-KEY
;; https://github.com/justbur/emacs-which-key
(require 'which-key)
;; I could use hooks to enable which-key only for specific modes
(which-key-mode)
;; Show VIM keys too
(setq which-key-allow-evil-operators t)
(setq which-key-show-operator-stage-maps t)


;;
;; FLYCHECK
;; http://www.flycheck.org/en/latest/
;; install syntax checking tools like 'npm install eslint' for JavaScript
(add-hook 'after-init-hook #'global-flycheck-mode)


;;
;; ORG MODE
;; Add automatic line breaks to make text look nice in org-mode
(setq org-directory "~/Documents/org/")

;; Setup all the files to be used
(setq org-agenda-files (list "~/Documents/org/todo.org"
			     "~/Documents/org/notes.org"))

;; "M-x notes" to quickly open notes
(defun notes()
  (interactive)
  (find-file "~/Documents/org/notes.org"))

;; Adds a timestamp when a todo is marked as DONE
(setq org-log-done t)

(add-hook 'org-mode-hook 'auto-fill-mode)

;; Store link with "C-c l". Paste it with "C-c C-l"
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
;; Capture todo template (Create a todo: "C-c c t")
(define-key global-map (kbd "C-c c") 'org-capture)
;;(global-set-key (kbd "C-b") 'org-switchb)

;; Open agande in the current window
(setq org-agenda-window-setup (quote current-window))

(setq org-capture-templates
      '(("t" "todo" entry (file+headline "~/Documents/org/todo.org" "Todos")
	 "* TODO [#B] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n")))


;; Configuring TODO states. These can be configured at the top of a file too.
(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
;; Set faces for todos
(setq org-todo-keyword-faces '(("TODO" . (:foreground "#C2222D" :weight bold))
		      ("IN-PROGRESS" . (:foreground "#FFBF00" :weight bold))
		      ("WAITING" . (:foreground "#DE8ED5" :weight bold))
		      ("DONE" . (:foreground "#73D115" :weight bold))))

;; Set priorities
(setq org-highest-priority ?A)
(setq org-lowest-priority ?C)
(setq org-default-priority ?B)
;; Set priority faces
(setq org-priority-faces '((?A . (:foreground "#C2222D" :weight bold))
			   (?B . (:foreground "#FFBF00"))
			   (?C . (:foreground "#238823"))))
