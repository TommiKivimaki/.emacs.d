(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")))

(defvar my-packages '(beacon
          company
		      dired-narrow
          dockerfile-mode
          htmlize
          epresent
		      flycheck
          ido-vertical-mode
          js2-mode
		      magit
		      markdown-mode
          org-bullets
          ox-reveal
          ox-gfm
          pdf-tools
          restclient
		      swift-mode
		      web-mode
          which-key
          yaml-mode))

;; To get pdf-tools installed
;;; pdf-tools installed first via homebrew
;;; brew install --HEAD dunn/homebrew-emacs/pdf-tools
;;; Then mactex installed
;;; brew cask install mactex

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
	      (width . 120)
	      (height . 92)))
     (setq default-frame-alist
	    '(
	      (tool-bar-lines . 0)
	      (width . 120)
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

(defalias 'list-buffers 'ibuffer-other-window)
  (global-set-key (kbd "C-x C-b") 'ibuffer)

;; Define a filter group
  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ("dired" (mode . dired-mode))
                 ("org" (name . "^.*org$"))
                 ("Magit" (mode . magit-mode))
                 ("web" (or
                 (mode . web-mode)
                 (mode . js2-mode)
                 (mode . css-mode)))
                 ("shell" (or
                 (mode . eshell-mode)
                 (mode . shell-mode)))
                 ("programming" (or
                                 (mode . python-mode)
                                 (mode . swift-mode)
                                 (mode . c++-mode)))

                 ("emacs" (or
                           (name . "^\\*scratch\\*$")
                           (name . "^\\*Messages\\*$")))
                 ))))

  (add-hook 'ibuffer-mode-hook
            (lambda ()
            ;; Automatically keep buffer list updated
            (ibuffer-auto-mode 1)
            ;; Load the defined filter group
            (ibuffer-switch-to-saved-filter-groups "default")))

  ;; Don't show filter groups if there are no buffers in that group
  (setq ibuffer-show-empty-filter-groups nil)

  ;; Don't ask for confirmation to delete marked buffers
  ;; (setq ibuffer-expert t)

(defun kill-other-buffers ()
  "Kill all other buffers, but not the current one"
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(windmove-default-keybindings)

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (shell . t)
    (emacs-lisp . t)
    (js . t)
    (ditaa . t)
))

(eval-after-load "org"
  '(require 'ox-gfm nil t))

(fset 'yes-or-no-p 'y-or-n-p)

(setq org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.11.0_1/libexec/ditaa-0.11.0-standalone.jar")
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
(setq org-confirm-babel-evaluate nil)

(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)

(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
(setq ido-vertical-show-count t)

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

(setq org-directory "/Volumes/Promise RAID/VersionControlled/org")

;; Setup all the agenda files
(setq org-agenda-files (list "/Volumes/Promise RAID/VersionControlled/org/todo.org"
			     "/Volumes/Promise RAID/VersionControlled/org/notes.org"
			     "/Volumes/Promise RAID/VersionControlled/org/projects.org"
			     "/Volumes/Promise RAID/VersionControlled/org/content.org"
           "/Volumes/Promise RAID/VersionControlled/org/ukulele.org"
           "/Volumes/Promise RAID/VersionControlled/org/trade.org"))

;; "M-x notes" to quickly open notes
(defun notes()
  (interactive)
  (find-file "/Volumes/Promise RAID/VersionControlled/org/notes.org"))

;; "M-x projects" to quickly open projects
(defun proj()
  (interactive)
  (find-file "/Volumes/Promise RAID/VersionControlled/org/projects.org"))

;; "M-x content" to quickly open content
(defun content()
  (interactive)
  (find-file "/Volumes/Promise RAID/VersionControlled/org/content.org"))

;; "M-x content" to quickly open ukulele stuff
(defun uke()
  (interactive)
  (find-file "/Volumes/Promise RAID/VersionControlled/org/ukulele.org"))

;; "M-x trade" to quickly open trading stuff
(defun trade()
  (interactive)
  (find-file "/Volumes/Promise RAID/VersionControlled/org/trade.org"))

;; Starts to bullets
(add-hook 'org-mode-hook (
   lambda()
     (org-bullets-mode 1)))

;; Adds a timestamp when a todo is marked as DONE
(setq org-log-done t)

(add-hook 'org-mode-hook 'auto-fill-mode)

;; Store link with "C-c l". Paste it with "C-c C-l"
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
;; Capture todo template (Create a todo: "C-c c t")
(define-key global-map (kbd "C-c c") 'org-capture)
;; Shortcut to structure templates in org mode
(add-hook 'org-mode-hook (
          lambda()
          (local-set-key (kbd "C-c s") 'org-insert-structure-template)))

;;(global-set-key (kbd "C-b") 'org-switchb)

;; Open agande in the current window
(setq org-agenda-window-setup (quote current-window))

(setq org-capture-templates
      '(("t" "todo" entry (file+headline "/Volumes/Promise RAID/VersionControlled/org/todo.org" "Todos")
      "* TODO [#B] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n")
      ("j" "Journal" entry (file+datetree "/Volumes/Promise RAID/VersionControlled/org/todo.org")
         "* %?\nDate %U\n  %i\n  %a")))


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
;; Enable shift+arrow key to switch between windows if cursor is not in a special
;; place inside org window
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; Hide the emphasis markers
(setq org-hide-emphasis-markers t)

;; Color emphasis for text
;;(setq org-emphasis-alist (quote (("*" bold)
;;("/" italic)
;;("_" underline)
;;("=" org-verbatim verbatim)
;;("~" org-code verbatim)
;;("?" (:foreground "#C2222D"))
;;("+"
;;(:strike-through t)))))

(load-library "ox-reveal")
;; (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
(setq org-reveal-root "file:///Users/tommi/Documents/reveal.js")
;; Disable automatic title slide generation
(setq org-reveal-title-slide nil)

(add-hook 'after-init-hook 'global-company-mode)
;; No delay in showing suggestions.
(setq company-idle-delay 1)
;; Show suggestions after entering 2 character2.
(setq company-minimum-prefix-length 2)
(setq company-selection-wrap-around t)
; Use tab key to cycle through suggestions.
; ('tng' means 'tab and go')
(company-tng-configure-default)

(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; Tab width to 2
(setq-default tab-width 2)
;; Use spaces instead of a tab
(setq-default tab-width 2 indent-tabs-mode nil)
;; Indentation cannot insert tabs
(setq-default indent-tabs-mode nil)

(setq js-indent-level 2)

(setq python-indent 2)

(setq css-indent-offset 2)

(add-hook 'sh-mode-hook
          (lambda ()
            (setq sh-basic-offset 2
                  sh-indentation 2)))

(setq web-mode-markup-indent-offset 2)

(add-hook 'before-save-hook '(lambda()
  (when (not (or (derived-mode-p 'markdown-mode)))
    (delete-trailing-whitespace))))

(defun enable-code-folding ()
  (hs-minor-mode)
  (local-set-key (kbd "C-z C-f") 'hs-hide-all)
  (local-set-key (kbd "C-z C-u") 'hs-show-all)
  (local-set-key (kbd "C-z C-b C-f") 'hs-hide-block)
  (local-set-key (kbd "C-z C-b C-u") 'hs-show-block))

(add-hook 'prog-mode-hook 'enable-code-folding)

(defun web-mode-config ()
  ;; HTML indent
  (setq web-mode-markup-indent-offset 2)
  ;; CSS indent
  (setq web-mode-css-indent-offset 2)
  ;; JS indent
  (setq web-mode-script-padding 2)
  ;; Highlight current HTML element
  (setq web-mode-enable-current-element-highlight t)
  ;; Current column highlight
  (setq web-mode-enable-current-column-highlight t)
  ;; Enable auto pairing
  (setq web-mode-enable-auto-pairing t)
)

(add-hook 'web-mode-hook 'web-mode-config)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs?\\'" . web-mode))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq js2-highlight-level 3)
;; Do not warn about missing semicolons
(setq js2-strict-missing-semi-warning nil)
