(provide 'setup-communication)

;; go-to-address-mode
(add-hook 'prog-mode-hook 'goto-address-mode)
(add-hook 'text-mode-hook 'goto-address-mode)

;; TRAMP CONFIG
(use-package tramp
  :ensure t
  :init
  (setq-default
   ;; Persistency files.
   tramp-persistency-file-name (path-join *user-cache-directory* "tramp")
   ;; Auto save storage.
   tramp-auto-save-directory (path-join *user-auto-save-directory* "tramp"))

  :config
  (setq-default
   ;; Default file transfer method.
   tramp-default-method "ssh"
   ;; Cache passwords.
   password-cache t
   password-cache-expiry 1000
   remote-file-name-inhibit-cache nil ; dont cache remote file attributes. This should speed up tramp
   ;; SSH is properly configured to share connections.
   tramp-use-ssh-controlmaster-options t
   ;; Dont allow reading dir-local on remote system since this slows down tramp by factor of 3.
   enable-remote-dir-locals nil
   )
  (tramp-set-completion-function
   "ssh" (-map
          (lambda (x) (list 'tramp-parse-sconfig x))
          (-remove
           (lambda (x) (not (file-exists-p x)))
           `(,(path-join "/" "etc" "ssh_config")
             ,(path-join "/" "etc" "ssh" "ssh_config")
             ,(path-join *user-home-directory* ".ssh" "config")))))

  ;; Debugging tramp
  ;; (setq tramp-debug-buffer t)
  ;; (setq tramp-verbose 6)


  ;; Preserve PATH on remote host.
  (setq tramp-remote-path (delete 'tramp-default-remote-path tramp-remote-path))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))
