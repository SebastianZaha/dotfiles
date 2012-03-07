;; Manage packages. Install the following if they do not exist. Handy
;; on fresh installs.
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit starter-kit-ruby starter-kit-js starter-kit-bindings zenburn-theme)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; remove all bells
(setq ring-bell-function 'ignore)
(setq initial-scratch-message nil)

; encoding
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; turn on pending delete (when a region is selected, typing replaces it)
(delete-selection-mode t)

;; Color
(load-theme 'zenburn)

;; Server
(server-start)

;; Programming

(set-default 'tab-width 4)
(set-default 'c-basic-offset 4)

;; Don't auto wrap
(setq-default fill-column 160)
(setq auto-fill-mode 0)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("5727ad01be0a0d371f6e26c72f2ef2bafdc483063de26c88eaceea0674deb3d9" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
