;; Manage packages. Install the following if they do not exist. Handy on fresh installs.
(require 'package)

;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(;;starter-kit
                      ;;starter-kit-ruby starter-kit-js starter-kit-bindings 
                      zenburn-theme 
                      flx-ido rainbow-delimiters smartparens 
                      markdown-mode
                      )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; Paths
(setq shell-file-name "bash")
(setq shell-command-switch "-ic")

;; Executables
(setq exec-path (append exec-path '("/usr/local/bin/")))

;; Remove all bells
(setq ring-bell-function 'ignore)
(setq initial-scratch-message nil)

;; Backups, inspired from http://stackoverflow.com/a/18330742/1306453
(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
        (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
)

;; File encoding
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Romanian special characters. Although defined by the OSX keymap, they're ignored in emacs.
(global-set-key (kbd "M-s") (kbd "ș"))
(global-set-key (kbd "M-t") (kbd "ț"))
(global-set-key (kbd "M-i") (kbd "î"))
(global-set-key (kbd "M-a") (kbd "ă"))
(global-set-key (kbd "M-q") (kbd "â"))
(global-set-key (kbd "M-S") (kbd "Ș"))
(global-set-key (kbd "M-T") (kbd "Ț"))
(global-set-key (kbd "M-I") (kbd "Î"))
(global-set-key (kbd "M-A") (kbd "Ă"))
(global-set-key (kbd "M-Q") (kbd "Â"))
;; German
(global-set-key (kbd "M-§") (kbd "ä"))
(global-set-key (kbd "M-u") (kbd "ü"))
(global-set-key (kbd "M-o") (kbd "ö"))


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
      (set-frame-height (selected-frame) 51)
      (set-frame-width (selected-frame) 100)))

;; Project management
;; (projectile-global-mode)
;; (define-key projectile-mode-map [?\s-f] 'projectile-find-file)
;; (define-key projectile-mode-map [?\s-d] 'projectile-find-dir)
;; (define-key projectile-mode-map [?\s-p] 'projectile-switch-project)
;; (define-key projectile-mode-map [?\s-g] 'projectile-grep)

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
(add-to-list 'auto-mode-alist '("\\.md.txt$" . markdown-mode))

;; Coding tools
(global-set-key (kbd "M-/") 'hippie-expand)


;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)


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


;; Go

;; TODO
;; Navigation
(global-set-key (kbd "s-t") 'ffip)

;; grep (ack) in project

;; jump to definition

;; svn / git integration - browse history, review changes before commit
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
	(zenburn-theme toml-mode starter-kit-ruby starter-kit-js starter-kit-bindings smartparens rust-mode rainbow-delimiters quack php-mode markdown-mode lua-mode go-mode go-autocomplete flycheck flx-ido company arduino-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
