;; Workaround for a bug in Emacs: https://debbugs.gnu.org/34341
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; set up the Emacs package system
;; (see http://emacs.stackexchange.com/a/5888)
(require 'package)
(setq package-enable-at-startup nil)

;; package repositories
(setq package-archives
      '(("gnu"         . "https://elpa.gnu.org/packages/")
	("melpa"       . "https://melpa.org/packages/")))

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
  (load-theme 'hc-zenburn t))

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

;; Enable Fira Code ligatures. See instructions for downloading Fira
;; Code Symbol [[https://github.com/jming422/fira-code-mode][here]].
(use-package fira-code-mode
  :custom (fira-code-mode-disabled-ligatures
           '("[]" "#{" "#(" "#_" "#_(" "{-" "x")) ;; List of ligatures to turn off
  :config (global-fira-code-mode))


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
  (setq projectile-project-search-path '(("~/Projects" . 1)))
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
;; (use-package undo-tree
;;   :delight
;;   :init
;;   (setq jcp/undo-tree-dir (concat user-emacs-directory "undo-tree"))
;;   (unless (file-exists-p jcp/undo-tree-dir)
;;     (make-directory jcp/undo-tree-dir t))
;;   (setq undo-tree-history-directory-alist (list (cons "."  jcp/undo-tree-dir)))
;;   :config
;;   (global-undo-tree-mode 1)
;;   ;; Do not save text properties in undo history:
;;   ;; see https://emacs.stackexchange.com/a/31130
;;   (defun nadvice/undo-tree-ignore-text-properties (old-fun &rest args)
;;     (dolist (item buffer-undo-list)
;;       (and (consp item)
;;            (stringp (car item))
;;            (setcar item (substring-no-properties (car item)))))
;;     (apply old-fun args))

;;   (advice-add 'undo-list-transfer-to-tree :around
;;               #'nadvice/undo-tree-ignore-text-properties))

;; multiple cursors
(use-package multiple-cursors)

;; company is a text completion framework for Emacs.
(use-package company)

;; flycheck
(use-package flycheck
  :ensure t)
;  :init (global-flycheck-mode))

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
  ;; (global-magit-file-mode)
  (setq vc-handled-backends (delq 'Git vc-handled-backends)))

;; htmlize exports the contents of an Emacs buffer to HTML
;; preserving display properties such as colors, fonts, underlining,
;; etc.
(use-package htmlize)

;; Org mode
(use-package org
  :ensure t
  :mode ("\\.org$" . org-mode)
  :init
  (setq org-export-backends '(ascii html md beamer))

  :config
  ;; Org directory
  (let ((dir "~/Documents/org"))
    (make-directory dir :parents)
    (setq org-directory dir))

  ;; Org notes file
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (ensure-file org-default-notes-file)

  ;; Org agenda file list
  (setq org-agenda-files
        (mapcar (lambda (x) (concat org-directory x))
                '("/notes.org"
                  "/work.org"
                  "/teaching.org"
                  "/home.org")))

  (dolist (path org-agenda-files)
    (ensure-file path))

  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-list-allow-alphabetical t
        org-use-speed-commands t
        org-log-done t)

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

  ;; Non-nil means ask for confirmation before executing Emacs Lisp
  ;; links.  Elisp links can be dangerous. Therefore we advise against
  ;; setting this variable to nil.  Just change it to ‘y-or-n-p’ if
  ;; you want to confirm with a single keystroke rather than having to
  ;; type "yes".
  (setq org-confirm-elisp-link-function 'y-or-n-p)

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
  (setq org-latex-pdf-process (quote ("LATEX=%latex texi2dvi -b -V %f"))))

(use-package org-bullets
  :ensure t
  :hook
  (org-mode . org-bullets-mode))

;; [[https://github.com/IvanMalison/org-projectile][org-projectile]]
;; provides functions for the creation of org-mode TODOs that are
;; associated with projectile projects.

(use-package org-projectile
  :ensure t
  :bind (("C-c n p" . org-projectile-project-todo-completing-read))
  :commands (org-projectile-open-project)
  :config
  (progn
    (setq org-projectile-projects-file
          (concat org-directory "/projects.org"))
    (ensure-file org-projectile-projects-file)
    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates)))


(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (;; ("README\\.md\\'" . gfm-mode)
         ;; ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config
  (setq markdown-asymmetric-header t)
  (setq markdown-code-block-braces t)
  (setq markdown-command
        (concat
         "/usr/local/bin/pandoc"
         " --from=markdown --to=html"
         " --standalone --mathjax --highlight-style=pygments")))

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
  (setq TeX-style-private (expand-file-name "~/.emacs.d/auctex-styles"))
  (setq TeX-electric-sub-and-superscript t)

  ;; Fontification of macros
  (setq font-latex-match-reference-keywords
        '(
          ;; changes
          ("added" "[{")
          ("deleted" "[{")
          ("replaced" "[{{")
          ("comment" "[{")
          ("highlight" "[{")
          ))

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

  :init
  (setq ess-ask-for-ess-directory nil)
  (setq ess-help-own-frame 'one)
  ;; uses braces around code block language strings:
  (setq markdown-code-block-braces t)

  :config
  (ess-toggle-underscore nil)

  (add-hook 'ess-mode-hook
            (lambda ()
              (ess-set-style 'Rstudio-)
              ;; (local-set-key [(shift return)] 'my-ess-eval)
              (add-hook 'local-write-file-hooks
                        (lambda ()
                          (ess-nuke-trailing-whitespace)))))

  (add-hook 'inferior-ess-mode-hook
            (lambda()
               (setq comint-scroll-to-bottom-on-input t)
               (setq comint-scroll-to-bottom-on-output t)
               (setq comint-move-point-for-output t)
               (local-set-key [C-up] 'comint-previous-input)
               (local-set-key [C-down] 'comint-next-input))))


(use-package poly-markdown
  :ensure t)

(use-package poly-R
  :ensure t)

(use-package polymode
  :diminish (poly-org-mode
             poly-gfm+r-mode
             poly-markdown-mode
             poly-noweb+r-mode
             poly-markdown+r-mode
             poly-rapport-mode
             poly-html+r-mode
             poly-brew+r-mode
             poly-r+c++-mode
             poly-c++r-mode)
  :mode (("\\.[rR]md\\'" . poly-gfm+r-mode)
         ("\\.[RS]nw\\'" . poly-noweb+R-mode)
         ("\\.md$" . poly-gfm-mode)
         ("\\.Rcpp$" . poly-r+c++-mode)
         ("\\.cppR$" . poly-c++r-mode))
  :init
  (require 'poly-R)
  (require 'poly-markdown))

;; Python

(setq python-shell-interpreter "python3")
(setq python-shell-interpreter-args "-i")


(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (setq elpy-rpc-python-command
        (concat (getenv "HOMEBREW_PREFIX") "/bin/python3"))
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))

;; (when (load "flycheck" t t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))

;; restart emacs

;; With a single universal-argument (C-u) Emacs is restarted with
;; --debug-init flag; with two universal-argument Emacs is restarted
;; with -Q flag; with three universal-argument the user is prompted
;; for the arguments
(use-package restart-emacs)


;; web-mode
(use-package emmet-mode
  :ensure t
  :hook web-mode)

(use-package rainbow-mode
  :ensure t
  :hook web-mode)

(use-package js2-mode
  :ensure t)
  ;; :hook (web-mode . js2-minor-mode))

(use-package prettier-js
  :ensure t)
  ;; :hook (web-mode . prettier-js-mode))

(use-package add-node-modules-path
  :ensure t
  :hook web-mode)

(use-package web-mode
  :ensure t
  :mode (("\\.html$" . web-mode)
         ("\\.jsx?\\'" . web-mode)))

;; JSON files
(use-package json-mode)

;; YAML
(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'"
  :config
  (add-hook 'yaml-mode-hook
      (lambda ()
         (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

;; Moodle Gift files
(use-package gift-mode
  :ensure t
  :mode "\\.gift\\'")

;; csv files
;; CSV mode config
(use-package csv-mode
  :ensure t
  :mode "\\.csv$")

(provide 'jcp-extensions)

;; quarto mode
(use-package quarto-mode)
