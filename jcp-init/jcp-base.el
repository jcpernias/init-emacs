;; Disable the scrollbar, the toolbar and the menu bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'horizontal-scroll-mode) (horizontal-scroll-mode -1))

;; Ask for confirmation when killing emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; Do not show start up screen:
(setq inhibit-startup-screen t)

;; Do not show initial message in scratch buffer:
(setq initial-scratch-message nil)

;; Visible bell: modeline flash
(defun jcp/flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

(setq visible-bell nil
      ring-bell-function 'jcp/flash-mode-line)

;; Set default font
(set-face-attribute 'default nil
                    :family "Source Code Pro" :height 150 :weight 'normal)

;; Set default frame dimensions
(setq default-frame-alist
      '((width . 90) (height . 52)
	(vertical-scroll-bars . nil)
	(horizontal-scroll-bars . nil)))

;; Add a newline at the end of files if there is not already one there
(setq require-final-newline t)

;; Delete trailing whitespace before save
(setq delte-trailing-lines t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Use visual line mode in text buffers
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Do not insert tabs
(setq-default indent-tabs-mode nil)

;; Tab stops every 4 columns
(setq default-tab-width 4)

;; Inserted text replaces the selected region
(delete-selection-mode 1)

;; Disable non selected window highlight
(setq-default cursor-in-non-selected-windows nil
      highlight-nonselected-windows nil)

;; Fringes
(setq fringes-outside-margins t)

;; Using the clipboard
(setq select-enable-clipboard t
      x-select-enable-clipboard-manager t
      select-enable-primary t
      save-interprogram-paste-before-kill t)

;; Mouse-2 does not move point; it inserts the text at
;; point, regardless of where you clicked or even which
;; of the frame’s windows you clicked on
(setq mouse-yank-at-point t)

;; Do not clutter init.el with customize settings:
(setq custom-file
      (expand-file-name (concat user-emacs-directory "custom.el")))
(if (file-exists-p custom-file)
    (load custom-file))

;; Backup files
(setq make-backup-files t)
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; Save bookmarks every time a bookmark is made or deleted
(setq bookmark-save-flag 1)

;; Enable show-paren mode
(show-paren-mode 1)

;; Line and column numbers in the modeline
(setq line-number-mode t)
(setq column-number-mode t)

;; shell
(setq explicit-shell-file-name "bash")

;; Compile
(setq compilation-scroll-output t)

;; hippie expand
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
      try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill
      try-complete-file-name-partially try-complete-file-name
      try-expand-all-abbrevs try-expand-list try-expand-line
      try-complete-lisp-symbol-partially
      try-complete-lisp-symbol))

;; apropos
(setq apropos-do-all t)

;; save-place
(save-place-mode 1)
(setq save-place-forget-unreadable-files nil)

;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally
      ediff-diff-options "-w")


(provide 'jcp-base)