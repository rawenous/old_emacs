(provide 'setup-conveniance)

;; GROUP: Convenience -> Revert

;; update any change made on file to the current buffer
;; (global-auto-revert-mode)

;; GROUP: Convenience -> Hippe Expand
;; hippie-expand is a better version of dabbrev-expand.
;; While dabbrev-expand searches for words you already types, in current;; buffers and other buffers, hippie-expand includes more sources,
;; such as filenames, klll ring...
(setq
 hippie-expand-try-functions-list
 '(try-expand-dabbrev ;; Try to expand word "dynamically", searching the current buffer.
   try-expand-dabbrev-all-buffers ;; Try to expand word "dynamically", searching all other buffers.
   try-expand-dabbrev-from-kill ;; Try to expand word "dynamically", searching the kill ring.
   try-complete-file-name-partially ;; Try to complete text as a file name, as many characters as unique.
   try-complete-file-name ;; Try to complete text as a file name.
   try-expand-all-abbrevs ;; Try to expand word before point according to all abbrev tables.
   try-expand-list ;; Try to complete the current line to an entire line in the buffer.
   try-expand-line ;; Try to complete the current line to an entire line in the buffer.
   try-complete-lisp-symbol-partially ;; Try to complete as an Emacs Lisp symbol, as many characters as unique.
   try-complete-lisp-symbol) ;; Try to complete word as an Emacs Lisp symbol.
 )

;; GROUP: Convenience -> HL Line
(global-hl-line-mode)

;; GROUP: Convenience -> Ibuffer
(setq ibuffer-use-other-window t) ;; always display ibuffer in another window

;; GROUP: Convenience -> Linum
(add-hook 'prog-mode-hook 'linum-mode) ;; enable linum only in programming modes

;; whenever you create useless whitespace, the whitespace is highlighted
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; GROUP: Convenience -> Windmove
;; easier window navigation
(windmove-default-keybindings)

;; Rebind yes-no to y-n
(defalias 'yes-or-no-p 'y-or-n-p)

;;;;;;;;;;;;;;;;;
;; KEYBINDINGS ;;
;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-/") 'hippie-expand) ; replace dabbrev-expand
(user/set-leader-keys "Tw" 'whitespace-mode) ; activate whitespace-mode to view all whitespace characters

;;;;;;;;;;;;;;
;; PACKAGES ;;
;;;;;;;;;;;;;;

;; Company is a text completion framework for Emacs. The name stands for "complete anything".
(use-package company
  :ensure t
  :config
  (global-company-mode)
  :diminish company-mode)

;; allows you to select text objects incrementally.
(use-package expand-region
  :ensure t
  :config
  ;; (global-set-key (kbd "M-m") 'er/expand-region)
  )

;; Group your buffers by their parent vc root directory
(use-package ibuffer-vc
  :ensure t
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic))))

  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                filename-and-process))))

;; Like guide-key but better. Shows the available commands when you type a prefix like C-x
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  :diminish which-key-mode)

(use-package projectile
  :diminish projectile-mode
  :ensure t
  :config
  (setq projectile-enable-caching t
        ;; Projectile cache store.
        projectile-cache-file (path-join *user-cache-directory* "projectile")
        projectile-known-projects-file (path-join *user-data-directory* "projectile-bookmarks.eld")
        projectile-completion-system 'default
        projectile-indexing-method 'alien
        )
  (projectile-global-mode)
  (user/set-leader-keys "p" 'projectile-command-map)

  )
