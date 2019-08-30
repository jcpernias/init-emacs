;;; Main init file

(add-to-list 'load-path (concat user-emacs-directory "jcp-init"))

(require 'jcp-base)
(require 'jcp-extensions)
(require 'jcp-functions)
(require 'jcp-global-keys)

;; mac key bindings
(when (eq system-type 'darwin)
  (require 'jcp-darwin))
