(provide 'setup-keybindings)

;; Shamelessly stolen from spacemacs
(defun user/declare-prefix (prefix name &optional long-name)
  "Declare a prefix PREFIX. PREFIX is a string describing a key
sequence. NAME is a string used as the prefix command.
LONG-NAME if given is stored in `spacemacs/prefix-titles'."
  (let* ((command name)
         (full-prefix (concat user-emacs-leader-key " " prefix))
         (full-prefix-lst (listify-key-sequence (kbd full-prefix)))
         )

    ;; define the prefix command only if it does not already exist
    (unless long-name (setq long-name name))
    (which-key-declare-prefixes
      full-prefix (cons name long-name))))

(setq user/key-binding-prefixes '(("T"   "UI toggles/themes")
                                  ("g"   "git")
                                  ("/"   "git-grep")
                                  ("l"   "locate(tags)")
                                  ("p"   "projectile")
                                  ))
(mapc (lambda (x) (apply #'user/declare-prefix x))
      user/key-binding-prefixes)

(which-key-add-key-based-replacements  "M-m /" "git-grep") ; rename prefix from helm-grep-do-git-grep to git-grep
