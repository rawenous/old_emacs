(require 'package)
(package-initialize)

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

(if (package-installed-p 'use-package)
    t
    (package-install 'use-package))

(require 'cl)

(defvar user-emacs-leader-key "M-m"
  "The leader key accessible for all emacs commands")
(defvar user-default-map (make-sparse-keymap)
  "Base keymap for all user key commands.")

(defun user/set-leader-keys (key def &rest bindings)
  "Add KEY and DEF as key bindings under
`dotspacemacs-leader-key' and `dotspacemacs-emacs-leader-key'.
KEY should be a string suitable for passing to `kbd', and it
should not include the leaders. DEF is most likely a quoted
command. See `define-key' for more information about the possible
choices for DEF. This function simply uses `define-key' to add
the bindings.

For convenience, this function will accept additional KEY DEF
pairs. For example,

\(spacemacs/set-leader-keys
   \"a\" 'command1
   \"C-c\" 'command2
   \"bb\" 'command3\)"
  (while key
    (define-key user-default-map (kbd key) def)
    (setq key (pop bindings) def (pop bindings))))

(use-package diminish
  :ensure t)

(global-set-key (kbd user-emacs-leader-key) user-default-map)

;; add your modules path
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'setup-editing)
(require 'setup-constants)
(require 'setup-conveniance)
(require 'setup-files)
(require 'setup-helm)
(require 'setup-communication)
(require 'setup-programming)
(require 'setup-environment)
(require 'setup-faces-and-ui)
(require 'setup-keybindings)
