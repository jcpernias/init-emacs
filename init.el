;;; Main init file -*- lexical-binding: t -*-

(add-to-list 'load-path (concat user-emacs-directory "jcp-init"))

;; Adjust garbage collection thresholds during startup and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
	    (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(require 'jcp-functions)

;; mac key bindings
(when (eq system-type 'darwin)
  (require 'jcp-darwin))

(require 'jcp-base)
(require 'jcp-extensions)
(require 'jcp-global-keys)

(provide 'init)
