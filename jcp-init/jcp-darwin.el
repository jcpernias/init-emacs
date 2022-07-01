(setq mac-function-modifier 'super)
(setq mac-option-modifier 'none)
(setq mac-command-modifier 'meta)

;; macOS: Set environment variables as they are set in the shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (add-to-list 'exec-path-from-shell-variables "TEXMFHOME")
    (add-to-list 'exec-path-from-shell-variables "HOMEBREW_PREFIX")
    (exec-path-from-shell-initialize)))

;; use system trash

(use-package osx-trash
  :ensure t
  :init
  (setq delete-by-moving-to-trash t)
  :config
  (osx-trash-setup))


;; unset keybinding for printing
(global-unset-key (kbd "s-p"))

(setenv "LANG" "es_ES.UTF-8")

(provide 'jcp-darwin)
