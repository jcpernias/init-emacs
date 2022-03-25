;;; Main init file

(add-to-list 'load-path (concat user-emacs-directory "jcp-init"))

(require 'jcp-functions)

;; mac key bindings
(when (eq system-type 'darwin)
  (require 'jcp-darwin))

(require 'jcp-base)
(require 'jcp-extensions)
(require 'jcp-global-keys)
