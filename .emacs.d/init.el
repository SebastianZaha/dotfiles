;; Manage packages. Install the following if they do not exist. Handy on fresh installs.
(require 'package)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit 
                      starter-kit-ruby starter-kit-js starter-kit-bindings 
                      zenburn-theme)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; Paths
(setq shell-file-name "bash")
(setq shell-command-switch "-ic")

;; Executables
(setq exec-path (append exec-path '("/opt/local/bin/")))
(setq exec-path (append exec-path '("/usr/local/bin/")))

;; Remove all bells
(setq ring-bell-function 'ignore)
(setq initial-scratch-message nil)

;; Backups and auto-saves should be stored in /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;; File encoding
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Romanian special characters. Although defined by the OSX keymap, they're ignored in emacs.
(global-set-key (kbd "M-s") (kbd "ș"))
(global-set-key (kbd "M-t") (kbd "ț"))
(global-set-key (kbd "M-i") (kbd "i"))
(global-set-key (kbd "M-a") (kbd "ă"))
(global-set-key (kbd "M-q") (kbd "â"))
(global-set-key (kbd "M-S") (kbd "Ș"))
(global-set-key (kbd "M-T") (kbd "Ț"))
(global-set-key (kbd "M-I") (kbd "Î"))
(global-set-key (kbd "M-A") (kbd "Ă"))
(global-set-key (kbd "M-Q") (kbd "Â"))

;; Turn on pending delete (when a region is selected, typing replaces it)
(delete-selection-mode t)

;; Scrolling
;; one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;; don't accelerate scrolling
(setq mouse-wheel-progressive-speed nil)
;; scroll window under mouse
(setq mouse-wheel-follow-mouse 't)
;; keyboard scroll one line at a time
(setq scroll-step 1)

;; Color
(load-theme 'zenburn t)

;; Server
(require 'server)
(unless (server-running-p)
  (server-start))

(setq ns-pop-up-frames nil)


;; Don't auto wrap
(setq-default fill-column 100)
(setq auto-fill-mode 1)

;; Window size
(if (window-system)
    (progn
      (set-frame-height (selected-frame) 56)
      (set-frame-width (selected-frame) 100)))

;; Project management
(projectile-global-mode)
(define-key projectile-mode-map [?\s-f] 'projectile-find-file)
(define-key projectile-mode-map [?\s-d] 'projectile-find-dir)
(define-key projectile-mode-map [?\s-p] 'projectile-switch-project)
(define-key projectile-mode-map [?\s-g] 'projectile-grep)

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;; Custom bindings

(defun volatile-kill-buffer ()
   "Kill current buffer unconditionally."
   (interactive)
   (let ((buffer-modified-p nil))
     (kill-buffer-and-window )))
;; Unconditionally kill unmodified buffers.
(global-set-key (kbd "C-x k") 'volatile-kill-buffer)

;; Documentation, dash support
(global-set-key "\C-cd" 'dash-at-point)
(global-set-key "\C-ce" 'dash-at-point-with-docset)
;; (add-to-list 'dash-at-point-mode-alist '(perl-mode . "perl"))
;; (add-hook 'rinari-minor-mode-hook
;;          (lambda () (setq dash-at-point-docset "rails")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming

;; Cosmetics
(set-default 'tab-width 4)
(set-default 'c-basic-offset 4)

;; Editing
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-M-h") 'backward-kill-word)
(global-set-key (kbd "C-c q") 'join-line)
(global-set-key (kbd "s-/") 'comment-or-uncomment-region)

;; Text
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; Coding tools
(global-set-key (kbd "M-/") 'hippie-expand)

;; Jump to a column on current line. Useful when debugging some of those obnoxious js stack traces
(defun goto-column-number (number)
  "Untabify, and go to a column number within the current line (1 is beginning of the line)."
  (interactive "nColumn number (<nr> - 1 == C) ? ")
  (beginning-of-line)
  (untabify (point-min) (point-max))
  (while (> number 1)
    (if (eolp)
        (insert ? )
      (forward-char))
    (setq number (1- number))))


;; PHP
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-hook 'php-mode-hook
          (lambda ()
            (c-set-style "bsd")
            (setq c-indent-level 4)
            (setq c-continued-statement-offset 4)
            (setq c-brace-offset -4)
            (setq c-argdecl-indent 0)
            (setq c-label-offset -4)
            (setq c-basic-offset 4)
            (setq tab-width 4)
            (setq indent-tabs-mode nil)
            (c-set-offset 'case-label '+)
            (c-set-offset 'arglist-close 'c-lineup-arglist-operators)
            (c-set-offset 'arglist-intro '+)
            (c-set-offset 'arglist-close '0)
            (c-set-offset 'arglist-cont-nonempty 'c-lineup-math)))


;; Javascript
(setq js-indent-level 4)


;; Scheme config
;; Enable Quack mode
;; The binary of your interpreter
(setq scheme-program-name "mit-scheme")
;; This hook lets you use your theme colours instead of quack's ones.
(defun scheme-mode-quack-hook ()
  (require 'quack)
  (setq quack-fontify-style 'emacs))
(add-hook 'scheme-mode-hook 'scheme-mode-quack-hook)

;; Rainbow delimiters
(require 'rainbow-delimiters)
(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)

(require 'smartparens-config)
(smartparens-global-mode t)


;; Ruby
(defun inf-ruby-set-pry-hook ()
  (setq inf-ruby-default-implementation "pry"))
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
(add-hook 'after-init-hook 'inf-ruby-switch-setup)
(add-hook 'inf-ruby-mode-hook 'inf-ruby-set-pry-hook)


;; TODO
;; Navigation
(global-set-key (kbd "s-t") 'ffip)

;; grep (ack) in project

;; jump to definition

;; svn / git integration - browse history, review changes before commit
