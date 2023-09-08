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

;; Package osx-trash is no longer maintained. The code below is taken
;; from Christian Tietze:
;; https://christiantietze.de/posts/2021/06/emacs-trash-file-macos/
(setq delete-by-moving-to-trash t)

(defun system-move-file-to-trash (path)
  "Moves file at PATH to the macOS trash.

Relies on the command-line utility 'trash' (http://hasseg.org/trash/)."
  (shell-command (concat "trash -vF \"" path "\""
                         "| sed -e 's/^/Trashed: /'")
                 nil ;; Name of output buffer
                 "*Trash Error Buffer*"))


;; unset keybinding for printing
(global-unset-key (kbd "s-p"))

(setenv "LANG" "es_ES.UTF-8")

(provide 'jcp-darwin)
