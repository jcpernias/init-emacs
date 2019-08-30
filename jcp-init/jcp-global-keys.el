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
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; C-c p is bound to counsel-compile
;; (global-set-key (kbd "C-c p") 'compile)
(global-set-key (kbd "C-c r") 'recompile)
(global-set-key (kbd "C-c k") 'kill-compilation)


(global-set-key (kbd "M-/") 'hippie-expand)



(provide 'jcp-global-keys)
