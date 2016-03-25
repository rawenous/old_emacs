(provide 'setup-faces-and-ui)

;; you won't need any of the bar thingies
;; turn it off to save screen estate
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(size-indication-mode t)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
;; taken from prelude-ui.el
(setq frame-title-format
      '("" invocation-name " - " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))

;; ;; change font to Inconsolata for better looking text
;; ;; remember to install the font Inconsolata first
;; (setq default-frame-alist '((font . "Inconsolata-11")))
;; ;; set italic font for italic face, since Emacs does not set italic
;; ;; face automatically
;; (set-face-attribute 'italic nil
;;                     :family "Inconsolata-Italic")


;;;;;;;;;;;;;;
;; PACKAGES ;;
;;;;;;;;;;;;;;

(use-package highlight-numbers
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))


(use-package solarized-theme
  :ensure t
  :if window-system
  :init
  (progn
    (setq solarized-use-less-bold t
          solarized-use-more-italic t
          solarized-emphasize-indicators nil
          solarized-distinct-fringe-background nil
          solarized-high-contrast-mode-line nil))
  :config
  (progn
    (load "solarized-theme-autoloads" nil t)
    )
  (load-theme 'solarized-dark t)
  )

(use-package zenburn-theme
  :ensure t
  :if (not window-system)
  :config
  (progn
    (load "zenburn-theme-autoloads" nil t)))

(use-package monokai-theme
  :ensure t
  :if (not window-system)
  :config)

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme)
  )
