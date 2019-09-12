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
(setq use-package-always-ensure t)

;; Load hc-zenburn-theme, a higher contrast version
;; of the zenburn theme.
(use-package hc-zenburn-theme
  :config
  (load-theme 'hc-zenburn t t))

(use-package tango-plus-theme
  :defer t)

(use-package solarized-theme
  :defer t)

(use-package delight
  :config
  (delight '((abbrev-mode " Abv" abbrev)
             (eldoc-mode nil "eldoc")
             (overwrite-mode " Ov" t)
             (flyspell-mode " Spell" flyspell)
             (auto-revert-mode nil autorevert)
             (lisp-interaction-mode "li" :major)
             (emacs-lisp-mode "el" :major))))

;; Ivy: see https://www.reddit.com/r/emacs/comments/910pga/tip_how_to_use_ivy_and_its_utilities_in_your/?utm_source=share&utm_medium=web2x
(use-package counsel
  :delight
  :after swiper
  :config (counsel-mode))

(use-package ivy
  :defer 0.1
  :delight
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :init
  (setq ivy-use-selectable-prompt t)
  :config
  (ivy-mode))

(use-package ivy-rich
  :after counsel
  :init
  (setq ivy-virtual-abbreviate 'full
	ivy-rich-path-style 'abbrev)
  :config
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package swiper
  :after ivy)

;; projectile
(use-package projectile
  :delight '(:eval (format " [%s]" (projectile-project-name)))
  :init
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action #'projectile-dired)
  :config
  (projectile-global-mode 1))

(use-package counsel-projectile
  :config
  (counsel-projectile-modify-action
   'counsel-projectile-switch-project-action
   '((default counsel-projectile-switch-project-action-dired)))
  ;;(setq projectile-completion-system 'ivy)
  (counsel-projectile-mode))

;; which key
(use-package which-key
  :delight
  :config
  (which-key-mode 1))

;; move around the visible buffer
(use-package avy
  :config
  (avy-setup-default))

;; assign number to windows and use M-1 to M-0 to navigate
(use-package window-numbering
  :config
  (window-numbering-mode))

;; undo tree mode
(use-package undo-tree
  :delight
  :config
  (global-undo-tree-mode 1))

;; multiple cursors
(use-package multiple-cursors)

;; company is a text completion framework for Emacs.
(use-package company)

;; flycheck
(use-package flycheck)

;; ivy interface for flyspell-correct package
(use-package flyspell-correct-ivy
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

;; yasnnipet
(use-package yasnippet
  :delight (yas-minor-mode " Y")
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

;; Magit
(use-package magit
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes)
  (setq magit-push-always-verify nil)
  ;; Disable diff before commit
  (setq vc-handled-backends (delq 'Git vc-handled-backends))
  (global-magit-file-mode))

;; htmlize exports the contents of an Emacs buffer to HTML
;; preserving display properties such as colors, fonts, underlining,
;; etc.
(use-package htmlize)

;; Org mode
(use-package org
  :ensure org-plus-contrib
  :mode ("\\.org$" . org-mode)
  :init
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


  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-list-allow-alphabetical t
        org-use-speed-commands t)

  ;; Htmlize with css. See the documentation of this variable:
  (setq org-html-htmlize-output-type 'css)
  (add-to-list 'safe-local-variable-values
               '(org-html-head-include-scripts . nil))

  ;; activate block snippets
  (require 'org-tempo)

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

(use-package org-bullets
  :ensure t
  :hook
  (org-mode . org-bullets-mode))

(use-package org-projectile
  :config
  (org-projectile-per-project)
  (setq org-projectile-per-project-filepath "TODO.org")
  (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
  (push (org-projectile-project-todo-entry) org-capture-templates))

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
  :hook
  (((LaTeX-mode latex-mode) . turn-on-reftex)
   (LaTeX-mode . auto-fill-mode)
   (LaTeX-mode . visual-line-mode)
   (LaTeX-mode . LaTeX-math-mode)
   (LaTeX-mode . turn-on-bib-cite))
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
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

;; restart emacs

;; With a single universal-argument (C-u) Emacs is restarted with
;; --debug-init flag; with two universal-argument Emacs is restarted
;; with -Q flag; with three universal-argument the user is prompted
;; for the arguments
(use-package restart-emacs)

(provide 'jcp-extensions)
