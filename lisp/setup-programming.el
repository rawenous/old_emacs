(provide 'setup-programming)
;; GROUP: Programming -> Languages -> C

;; Available C style:
;; “gnu”: The default style for GNU projects
;; “k&r”: What Kernighan and Ritchie, the authors of C used in their book
;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; “stroustrup”: What Stroustrup, the author of C++ used in his book
;; “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,” Erik Nyquist and Mats Henricson, Ellemtel
;; “linux”: What the Linux developers use for kernel development
;; “python”: What Python developers use for extension modules
;; “java”: The default style for java-mode (see below)
;; “user”: When you want to define your own style
(setq c-default-style "linux" ; set style to "linux"
      c-basic-offset 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUP: Programming -> Tools -> Gdb ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq gdb-many-windows t        ; use gdb-many-windows by default
      gdb-show-main t)          ; Non-nil means display source file containing the main routine at startup

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUP: Programming -> Tools -> Compilation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation from Emacs
(defun prelude-colorize-compilation-buffer ()
  "Colorize a compilation mode buffer."
  (interactive)
  ;; we don't want to mess with child modes such as grep-mode, ack, ag, etc
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

;; setup compilation-mode used by `compile' command
(require 'compile)
(setq compilation-ask-about-save nil          ; Just save before compiling
      compilation-always-kill t               ; Just kill old compile processes before starting the new one
      compilation-scroll-output 'first-error) ; Automatically scroll to first
(global-set-key (kbd "<f5>") 'compile)

;; GROUP: Programming -> Tools -> Makefile
;; takenn from prelude-c.el:48: https://github.com/bbatsov/prelude/blob/master/modules/prelude-c.el
(defun prelude-makefile-mode-defaults ()
  (whitespace-toggle-options '(tabs))
  (setq indent-tabs-mode t ))

(setq prelude-makefile-mode-hook 'prelude-makefile-mode-defaults)

(add-hook 'makefile-mode-hook (lambda ()
                                (run-hooks 'prelude-makefile-mode-hook)))

;; GROUP: Programming -> Tools -> Ediff
(setq ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)


;;;;;;;;;;;;;;
;; PACKAGES ;;
;;;;;;;;;;;;;;

(use-package diff-hl-mode
  :diminish diff-hl-mode)

(use-package magit
  :ensure t
  :config
  (add-hook 'magit-mode-hook 'magit-load-config-extensions)
  (user/set-leader-keys "gh" 'magit-log)
  (user/set-leader-keys "gf" 'magit-file-log)
  (user/set-leader-keys "gb" 'magit-blame-mode)
  (user/set-leader-keys "gm" 'magit-branch-manager)
  (user/set-leader-keys "gc" 'magit-branch)
  (user/set-leader-keys "gs" 'magit-status)
  (user/set-leader-keys "gr" 'magit-reflog)
  (user/set-leader-keys "gt" 'magit-tag)
  )

(use-package helm-gtags
  :ensure t
  :config
  (user/set-leader-keys  "lc" 'helm-gtags-create-tags)
  (user/set-leader-keys  "ld" 'helm-gtags-find-tag)
  (user/set-leader-keys  "lf" 'helm-gtags-select-path)
  (user/set-leader-keys  "lj" 'helm-gtags-dwim)
  (user/set-leader-keys  "lG" 'helm-gtags-dwim-other-window)
  (user/set-leader-keys  "li" 'helm-gtags-tags-in-this-function)
  (user/set-leader-keys  "ll" 'helm-gtags-parse-file)
  (user/set-leader-keys  "ln" 'helm-gtags-next-history)
  (user/set-leader-keys  "lp" 'helm-gtags-previous-history)
  (user/set-leader-keys  "lr" 'helm-gtags-find-rtag)
  (user/set-leader-keys  "lR" 'helm-gtags-resume)
  (user/set-leader-keys  "ls" 'helm-gtags-select)
  (user/set-leader-keys  "lS" 'helm-gtags-show-stack)
  (user/set-leader-keys  "lu" 'helm-gtags-update-tags)
  )
