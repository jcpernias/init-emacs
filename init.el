;; Disable the scrollbar, the toolbar and the menu bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'horizontal-scroll-mode) (horizontal-scroll-mode -1))

;; Do not show start up screen:
(setq inhibit-startup-screen t)

;; Do not show initial message in scratch buffer:
(setq initial-scratch-message nil)

;; Set default font
(set-face-attribute 'default nil
                    :family "Source Code Pro" :height 150 :weight 'normal)

;; Set default frame dimensions
(setq default-frame-alist
      '((width . 90) (height . 52)
	(vertical-scroll-bars . nil)
	(horizontal-scroll-bars . nil)))

;; Visible bell: modeline flash  
(setq visible-bell nil
      ring-bell-function 'flash-mode-line)
(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))



;; Do not clutter init.el with customize settings:
(setq custom-file
      (expand-file-name (concat user-emacs-directory "custom.el")))
(if (file-exists-p custom-file)
    (load custom-file))


;; Set backup directory
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; Enable show-paren mode
(show-paren-mode 1)

;; Line and column numbers in the modeline
(setq line-number-mode t)
(setq column-number-mode t)

;; Bind zap-up-to-char instead of zap-to-char
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)
(global-set-key (kbd "M-z") 'zap-up-to-char)


;; shell
(setq explicit-shell-file-name "bash")



;; set up the Emacs package system
;; (see http://emacs.stackexchange.com/a/5888)	 
(require 'package)
(setq package-enable-at-startup nil)

;; package repositories
(setq package-archives
      '(("gnu"         . "https://elpa.gnu.org/packages/")
	("melpa"       . "https://melpa.org/packages/")
	("org"         . "http://orgmode.org/elpa/")))

;; Initialize Emacs package system:
(unless package--initialized
  (package-initialize))


;; Install use-package (https://github.com/jwiegley/use-package).
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)


;; macOS: Use right option key to type \, |, @, and # characters
(setq ns-right-alternate-modifier (quote none))

;; macOS: Set environment variables as they are set in the shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (add-to-list 'exec-path-from-shell-variables "TEXMFHOME")
    (exec-path-from-shell-initialize)))


;; Load hc-zenburn-theme, a higher contrast version
;; of the zenburn theme.

(use-package hc-zenburn-theme
  :ensure t
  :config
  (load-theme 'hc-zenburn t t))
