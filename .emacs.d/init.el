(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(defvar rc/package-contents-refreshed nil)

(defun rc/package-refresh-contents-once ()
  (when (not rc/package-contents-refreshed)
    (setq rc/package-contents-refreshed t)
    (package-refresh-contents)))

(defun rc/require-one-package (package)
  (when (not (package-installed-p package))
    (rc/package-refresh-contents-once)
    (package-install package)))

(defun rc/require (&rest packages)
  (dolist (package packages)
    (rc/require-one-package package)))

(rc/require 'smex 'ido-completing-read+ 'zenburn-theme 'magit)

(require 'ido-completing-read+)

(global-set-key (kbd "M-x") 'smex)

;; Paths
(setq shell-file-name "bash")
(setq shell-command-switch "-ic")

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

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(global-display-line-numbers-mode 0)

;; Color
(load-theme 'zenburn t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 130 :family "Jetbrains Mono")))))

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

(ido-mode 1)
(ido-everywhere 1)

;; Custom bindings

(defun volatile-kill-buffer ()
   "Kill current buffer unconditionally."
   (interactive)
   (let ((buffer-modified-p nil))
     (kill-buffer-and-window )))
;; Unconditionally kill unmodified buffers.
(global-set-key (kbd "C-x k") 'volatile-kill-buffer)

;; Documentation, dash support
;; (global-set-key "\C-cd" 'dash-at-point)
;; (global-set-key "\C-ce" 'dash-at-point-with-docset)
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
(global-set-key (kbd "s-/") 'comment-or-uncomment-region)
(defun top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))
(global-set-key (kbd "C-S-j") 'top-join-line) ;; intellij binding

;; Text
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md.txt$" . markdown-mode))

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

;; rainbow delimiters
(require 'rainbow-delimiters)
(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(smex magit zenburn-theme toml-mode starter-kit-ruby starter-kit-js starter-kit-bindings smartparens rust-mode rainbow-delimiters quack php-mode markdown-mode lua-mode go-mode go-autocomplete flycheck flx-ido company arduino-mode))
 '(xref-search-program 'ripgrep))

