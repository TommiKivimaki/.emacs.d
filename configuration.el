(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")))

(defvar my-packages '(beacon
		      dired-narrow
		      flycheck
		      magit
		      markdown-mode
		      swift-mode
		      which-key))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-refresh-contents)
    (package-install p))
  (add-to-list 'package-selected-packages p))

(load-theme 'tango-dark)

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

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

;;(scroll-bar-mode -1)

(setq save-place-file (expand-file-name "places" user-emacs-directory))
(setq save-place-forget-unreadable-files nil)
(save-place-mode 1)

(put 'narrow-to-region 'disabled nil)

(setq visible-bell t)

(beacon-mode 1)
(setq beacon-blink-duration 0.8)

(setq gc-cons-threshold 20000000)

(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq backup-directory-alist `(("." . ,(expand-file-name
 					(concat user-emacs-directory "backups")))))

(setq make-backup-files t
     backup-by-copying t
     delete-old-versions t
     kept-new-versions 2
     kept-old-versions 3
     version-control t)

(setq vc-follow-symlinks t)

(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

(setq inhibit-startup-screen t)

(add-hook 'markdown-mode-hook 'visual-line-mode)

(put 'dired-find-alternate-file 'disabled nil)

(setq dired-recursive-copies 'always)

(require 'dired)
(define-key dired-mode-map (kbd "/") 'dired-narrow-fuzzy)

(global-set-key (kbd "C-x g") 'magit-status)

(require 'which-key)
(which-key-mode)
;; Show VIM keys too
(setq which-key-allow-evil-operators t)
(setq which-key-show-operator-stage-maps t)

(add-hook 'after-init-hook #'global-flycheck-mode)

(setq org-directory "~/Documents/org/")

;; Setup all the agenda files
(setq org-agenda-files (list "~/Documents/org/todo.org"
			     "~/Documents/org/notes.org"
			     "~/Documents/org/projects.org"))

;; "M-x notes" to quickly open notes
(defun notes()
  (interactive)
  (find-file "~/Documents/org/notes.org"))

;; "M-x projects" to quickly open projects
(defun projects()
  (interactive)
  (find-file "~/Documents/org/projects.org"))

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
			   (?C . (:foreground "#"))))

;; Set tag alignment for org-mode and org-agenda-mode 
(setq org-tags-column (- 14 (window-body-width)))
(setq org-agenda-tags-column (- 14 (window-body-width)))
;; Align tags with "C-c C-c"
;;(add-hook 'org-ctrl-c-ctrl-c-hook 'org-align-all-tags)
