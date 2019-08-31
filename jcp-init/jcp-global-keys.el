;; avy
(global-set-key (kbd "C-.") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "M-g g") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "C-c C-j") 'avy-resume)

;; multiple cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; another binding for just-one-space
(global-set-key (kbd "s-SPC") 'just-one-space)

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Disable C-z in graphical environments
(when (display-graphic-p)
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z")))

;; Bind zap-up-to-char instead of zap-to-char
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)
(global-set-key (kbd "C-c z") 'zap-up-to-char)


;; undo tree mode
(global-set-key (kbd "M-z") 'undo-tree-undo)
(global-set-key (kbd "M-Z") 'undo-tree-redo)

;; projectile
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


(global-set-key (kbd "C-c C-z") 'restart-emacs)

(global-set-key (kbd "M-/") 'hippie-expand)

;; Dired-x
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x 4 C-j") 'dired-jump-other-window)

;; Flyspell
(global-set-key (kbd "<f8>") 'flyspell-mode)
(global-set-key (kbd "M-<f8>") 'flyspell-buffer)

;; ivy, swiper, counsel
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-c p") 'counsel-compile)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-h l") 'counsel-find-library)
(global-set-key (kbd "C-h i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "C-h u") 'counsel-unicode-char)
(global-set-key (kbd "C-s") 'counsel-grep-or-swiper)
(global-set-key (kbd "C-r") 'counsel-grep-or-swiper-backward)
(global-set-key (kbd "C-x B") 'ivy-switch-buffer-other-window)

(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-x B") 'ivy-switch-buffer-other-window)
(global-set-key (kbd "C-x B") 'ivy-switch-buffer-other-window)
(global-set-key (kbd "C-x B") 'ivy-switch-buffer-other-window)

;; flyspell correct ivy
(global-set-key (kbd "C-+") 'flyspell-correct-wrapper)

;; Magit
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c C-g") 'magit-dispatch)

;; Org
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-switchb)


(provide 'jcp-global-keys)
