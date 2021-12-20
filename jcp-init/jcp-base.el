;; Disable the scrollbar, the toolbar and the menu bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
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

;; Set default font and default frame dimensions
(let ((sname (system-name)))
  (cond
   ((string-equal sname "Lola")
    (progn
      (set-face-attribute
       'default nil
       :family "Source Code Pro" :height 100 :weight 'normal)
      (setq default-frame-alist
            '((width . 85) (height . 40)
              (vertical-scroll-bars . nil)
              (horizontal-scroll-bars . nil)))))
   ((string-equal sname "Victoria.local")
    (progn
      (set-face-attribute
       'default nil
       :family "Source Code Pro" :height 160 :weight 'normal)
      (setq my-top (- (x-display-pixel-height) 1272)
            my-left (/ (- (x-display-pixel-width) 1730) 2))
      (add-to-list 'initial-frame-alist (cons 'top my-top))
      (add-to-list 'initial-frame-alist (cons 'left my-left))
      (setq default-frame-alist
            '((width . 190) (height . 60)
              (vertical-scroll-bars . nil)
              (horizontal-scroll-bars . nil)))))
   ((string-equal sname "Duke.local")
    (progn
      (set-face-attribute
       'default nil
       :family "Source Code Pro" :height 150 :weight 'normal)
      (setq my-top (- (x-display-pixel-height) 1020)
            width (nth 3 (assq 'geometry (car (display-monitor-attributes-list))))
            my-left (/ (-  width 1637) 2))
      (add-to-list 'initial-frame-alist (cons 'top my-top))
      (add-to-list 'initial-frame-alist (cons 'left my-left))
      (setq default-frame-alist
            '((width . 180) (height . 52)
              (vertical-scroll-bars . nil)
              (horizontal-scroll-bars . nil)))))
   (t
    (progn
      (set-face-attribute
       'default nil
       :family "Source Code Pro" :height 150 :weight 'normal)
      (setq default-frame-alist
            '((width . 90) (height . 52)
              (vertical-scroll-bars . nil)
              (horizontal-scroll-bars . nil)))))))

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

;; Append the directory names when visiting two files
;; with the same name
(setq uniquify-buffer-name-style 'forward)

;; case insensitive sort in dired
(require 'ls-lisp)
(setq ls-lisp-use-insert-directory-program nil)
(setq ls-lisp-ignore-case 't)
(setq ls-lisp-use-string-collate nil)

;; Dired-x
(add-hook
 'dired-load-hook
 (lambda ()
   (load "dired-x")
   ;; Set dired-x global variables here.  For example:
   ;; (setq dired-guess-shell-gnutar "gtar")
   (setq dired-x-hands-off-my-keys nil)
   (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.[^.]")
   (when (eq system-type 'darwin)
     (setq dired-guess-shell-alist-user '(("\\.pdf\\'" "open -a Skim"))))))
(add-hook
 'dired-mode-hook
 (lambda ()
   ;; Set dired-x buffer-local variables here.
   (dired-omit-mode 1)))

(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)

(autoload 'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (dired-jump) but in other window." t)

;; Using the clipboard
(setq select-enable-clipboard t
      x-select-enable-clipboard-manager t
      select-enable-primary t
      save-interprogram-paste-before-kill t)

;; Mouse-2 does not move point; it inserts the text at
;; point, regardless of where you clicked or even which
;; of the frame’s windows you clicked on
(setq mouse-yank-at-point t)

;; Use aspell
(setq ispell-program-name "aspell")

;; bind flyspell context menu to mouse-3:
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

;; abbrevs
(setq save-abbrevs t)


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

;; electric pair mode
(electric-pair-mode 1)
(add-function
 :before-until electric-pair-inhibit-predicate
 (lambda (c)
   (and (eq major-mode 'org-mode)
        (eq c ?<))))

;; Line and column numbers in the modeline
(setq line-number-mode t)
(setq column-number-mode t)

;; shell
(when (eq system-type 'darwin)
  (setq explicit-shell-file-name "zsh"))

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

;; recent files
(add-hook 'after-init-hook 'recentf-mode)

(provide 'jcp-base)
