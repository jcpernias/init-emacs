(setq mac-function-modifier 'super)
(setq mac-option-modifier 'none)
(setq mac-command-modifier 'meta)

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

(provide 'jcp-darwin)
