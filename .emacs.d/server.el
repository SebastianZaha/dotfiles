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

(defun volatile-kill-buffer ()
   "Kill current buffer unconditionally."
   (interactive)
   (let ((buffer-modified-p nil))
     (kill-buffer-and-window )))
;; Unconditionally kill unmodified buffers.
(global-set-key (kbd "C-x k") 'volatile-kill-buffer)

;; Cosmetics
(set-default 'tab-width 4)
(set-default 'c-basic-offset 4)

;; Editing
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-M-h") 'backward-kill-word)
(global-set-key (kbd "C-c q") 'join-line)
(global-set-key (kbd "s-/") 'comment-or-uncomment-region)

;; Do not auto-indent on RET, it messes up pasting into terminal                                                                                                                  (electric-indent-mode 0)
