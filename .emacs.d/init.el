;; Manage packages. Install the following if they do not exist. Handy on fresh installs.
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

;; Paths
(setq shell-file-name "bash")
(setq shell-command-switch "-ic")
;; For macports
(setq exec-path (append exec-path '("/opt/local/bin/")))

;; Remove all bells
(setq ring-bell-function 'ignore)
(setq initial-scratch-message nil)

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

;; turn on pending delete (when a region is selected, typing replaces it)
(delete-selection-mode t)

;; Color
(load-theme 'zenburn)

;; Server
(server-start)

;; Don't auto wrap
(setq-default fill-column 160)
(setq auto-fill-mode 0)

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


;; TODO
;; Navigation
(global-set-key (kbd "s-t") 'ffip)
(global-set-key (kbd "C-c a") 'ack)

;; grep (ack) in project

;; jump to definition

;; svn / git integration - browse history, review changes before commit


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("b7553781f4a831d5af6545f7a5967eb002c8daeee688c5cbf33bf27936ec18b3" "5727ad01be0a0d371f6e26c72f2ef2bafdc483063de26c88eaceea0674deb3d9" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
