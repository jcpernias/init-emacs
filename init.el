;; Disable the scrollbar, the toolbar and the menu bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))


;; Do not clutter init.el with customize settings:
(setq custom-file
      (expand-file-name (concat user-emacs-directory "custom.el")))
(if (file-exists-p custom-file)
    (load custom-file))


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

