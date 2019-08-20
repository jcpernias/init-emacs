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


;; Add a newline at the end of files if there is not already one there
(setq require-final-newline t)

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp) 

;; Disable C-z in graphical environments
(when (display-graphic-p)
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z")))


;; Use visual line mode in text buffers
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Do not insert tabs
(setq-default indent-tabs-mode nil)

;; Tab stops every 4 columns
(setq default-tab-width 4)

;; Inserted text replaces the selected region
(delete-selection-mode 1)

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


;; Workaround for a bug in Emacs: https://debbugs.gnu.org/34341
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

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

;; use system trash

(when (eq system-type 'darwin)
  (use-package osx-trash
    :ensure t
    :config
    (osx-trash-setup)))
(setq delete-by-moving-to-trash t)



;; Load hc-zenburn-theme, a higher contrast version
;; of the zenburn theme.

(use-package hc-zenburn-theme
  :ensure t
  :config
  (load-theme 'hc-zenburn t t))


;; Ivy: see https://www.reddit.com/r/emacs/comments/910pga/tip_how_to_use_ivy_and_its_utilities_in_your/?utm_source=share&utm_medium=web2x

(use-package counsel
  :ensure t
  :after swiper
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x l" . counsel-locate)
         ("C-c p" . counsel-compile)
	 ("C-h f" . counsel-describe-function)
	 ("C-h v" . counsel-describe-variable)
	 ("C-h l" . counsel-find-library)
	 ("C-h i" . counsel-info-lookup-symbol)
	 ("C-h u" . counsel-unicode-char)
         ("C-s" . counsel-grep-or-swiper)
         ("C-r" . counsel-grep-or-swiper-backward)
	 ("C-x B" . ivy-switch-buffer-other-window))
  :config (counsel-mode))

(use-package ivy
  :ensure t
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window)
	 ("C-x B" . ivy-switch-buffer-other-window)
	 ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :init
  (setq ivy-use-selectable-prompt t)
  :config
  (ivy-mode))

(use-package ivy-rich
  :ensure t
  :after counsel
  :init
  (setq ivy-virtual-abbreviate 'full
	ivy-rich-path-style 'abbrev)
  :config
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package swiper
  :ensure t
  :after ivy
  :bind ())


;; Compile
(setq compilation-scroll-output t)
;; C-c p is bound to counsel-compile
;; (global-set-key (kbd "C-c p") 'compile)
(global-set-key (kbd "C-c r") 'recompile)
(global-set-key (kbd "C-c k") 'kill-compilation)

;; hippie expand
(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
      try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill
      try-complete-file-name-partially try-complete-file-name
      try-expand-all-abbrevs try-expand-list try-expand-line
      try-complete-lisp-symbol-partially
      try-complete-lisp-symbol))

;; apropos
(setq apropos-do-all t)

;; Flyspell

;; bind context menu to mouse-3:
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

;; Add some useful keybindings:
(global-set-key (kbd "<f8>") 'flyspell-mode)
(global-set-key (kbd "M-<f8>") 'flyspell-buffer)

;; ivy interface for flyspell-correct package
(use-package flyspell-correct-ivy
  :ensure t
  :bind ("C-+" . flyspell-correct-wrapper)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

;; Magit
(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status)
         ("C-c C-g" . magit-dispatch-popup))
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes)
  (setq magit-push-always-verify nil)
  ;; Disable diff before commit
  (setq vc-handled-backends (delq 'Git vc-handled-backends))
  ;; Disable VC for Git
  (setq vc-handled-backends (delq 'Git vc-handled-backends))
  (global-magit-file-mode))

;; htmlize exports the contents of an Emacs buffer to HTML
;; preserving display properties such as colors, fonts, underlining,
;; etc.
(use-package htmlize
  :ensure t)

;; Org mode
(use-package org
  :ensure org-plus-contrib
  :mode ("\\.org$" . org-mode)
  :init
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c b") 'org-switchb)
  (setq org-export-backends '(ascii html md beamer))

  :config
  ;; Org directory
  (let ((dir "~/.org"))
    (make-directory dir :parents)
    (setq org-directory dir))

  ;; Org notes file
  (setq org-default-notes-file (concat org-directory "/notes.org"))

  ;; Org agenda file list
  (setq org-agenda-files (concat org-directory "/agenda-files"))


  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-list-allow-alphabetical t)
  (setq org-use-speed-commands t)

  ;; Htmlize with css. See the documentation of this variable:
  (setq org-html-htmlize-output-type 'css)
  (add-to-list 'safe-local-variable-values
               '(org-html-head-include-scripts . nil))

  ;; active Babel languages
  (require 'ob-R)
  (require 'ob-latex)
  (add-to-list 'org-babel-noweb-error-langs "latex")

  (defun my-org-confirm-babel-evaluate (lang body)
    (not (string-match "^\\(R\\|emacs-lisp\\)$" lang)))
  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

  (setq org-file-apps
        '((auto-mode . emacs)
          ("\\.mm\\'" . default)
          ("\\.x?html?\\'" . default)
          ("\\.pdf\\'" . "open -a Preview %s")
          ;; ("\\.pdf\\'" . "evince %s")
          ))
  ;; Do not center latex images by default
  (setq org-latex-images-centered nil)
  ;; Use texi2dvi to compile latex files
  (setq org-latex-pdf-process (quote ("texi2dvi -p -b -V %f"))))

;; RefTeX
(use-package reftex
  :commands (reftex-mode turn-on-reftex)
  :config
  (setq reftex-enable-partial-scans t)
  (setq reftex-save-parse-info t)
  (setq reftex-use-multiple-selection-buffers t)

  ;; Make TeX and RefTex aware of Snw and Rnw files
  (setq reftex-file-extensions
        '(("Snw" "Rnw" "nw" "tex" ".tex" ".ltx") ("bib" ".bib")))
  (setq reftex-plug-into-AUCTeX t))

;; AUCTeX
(use-package tex-site
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)

  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
  (add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-bib-cite)
  (setq TeX-PDF-mode t)
  (setq ess-swv-plug-into-AUCTeX-p t)

  (setq TeX-auto-global "~/local/var/auctex/")
  (setq TeX-auto-local ".Auto/")
  (setq TeX-electric-sub-and-superscript t)

  ;; Default pdf viewer
  (setq TeX-view-program-list
        '(("Preview" "open -a Preview %o")))
  (setq TeX-view-program-selection '((output-pdf "Preview")))

  ;; Turn on source correlation
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (setq TeX-source-correlate-start-server t)

  ;; Make TeX and RefTex aware of Snw and Rnw files
  (setq TeX-file-extensions
        '("Snw" "Rnw" "nw" "tex" "sty" "cls" "ltx" "texi" "texinfo"))
  )

;; ESS
(use-package ess-site
  :ensure ess
  :defer t
  :commands (R ess-version)
  :mode (("\\.R\\'" . R-mode)
         ("\\.[RS]nw\\'" . Rnw-mode))
  :config
  (ess-toggle-underscore nil)
  (setq ess-use-ido t)
  (setq ess-ask-for-ess-directory nil)
  (setq ess-local-process-name "R")
  (setq ess-help-own-frame 'one)
  ;; (setq ess-swv-processor "knitr")  

  (add-hook 'ess-mode-hook
            (lambda ()
              (ess-set-style 'GNU 'quiet)
              ;; Because
              ;;                                 DEF GNU BSD K&R C++
              ;; ess-indent-level                  2   2   8   5   4
              ;; ess-continued-statement-offset    2   2   8   5   4
              ;; ess-brace-offset                  0   0  -8  -5  -4
              ;; ess-arg-function-offset           2   4   0   0   0
              ;; ess-expression-offset             4   2   8   5   4
              ;; ess-else-offset                   0   0   0   0   0
              ;; ess-close-brace-offset            0   0   0   0   0
              (local-set-key [(shift return)] 'my-ess-eval)
              (add-hook 'local-write-file-hooks
                        (lambda ()
                          (ess-nuke-trailing-whitespace)))))

  (add-hook 'inferior-ess-mode-hook
            '(lambda()
               (setq comint-scroll-to-bottom-on-input t)
               (setq comint-scroll-to-bottom-on-output t)
               (setq comint-move-point-for-output t)
               (local-set-key [C-up] 'comint-previous-input)
               (local-set-key [C-down] 'comint-next-input)))

  (add-hook 'Rnw-mode-hook '(lambda()
               (local-set-key [(shift return)] 'my-ess-eval))))
