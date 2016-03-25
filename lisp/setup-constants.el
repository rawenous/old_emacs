(defun path-join (root &rest dirs)
  "Join paths together starting at ROOT and proceeding with DIRS.
Ex: (path-join \"/tmp\" \"a\" \"b\" \"c\") => /tmp/a/b/c"
  (if (not dirs)
      root
    (apply 'path-join
	   (expand-file-name (car dirs) root)
	   (cdr dirs))))

(defun getenv-or (env value)
  "Fetch the value of ENV or, if it is not set, return VALUE."
  (if (getenv env)
      (getenv env)
    value))

  ;;; (Directories) ;;;
(defconst *user-home-directory*
  (getenv-or "HOME" (concat (expand-file-name "~") "/"))
  "Path to user home directory.")
(defconst *user-local-directory*
  (if (getenv "XDG_DATA_HOME")
      (path-dirname (getenv "XDG_DATA_HOME"))
    (path-join *user-home-directory* ".local"))
  "Path to user's local store.")
(defconst *user-config-directory*
  (path-join (getenv-or "XDG_CONFIG_HOME"
			(path-join *user-home-directory* ".config"))
	     "emacs")
  "Path to user's local cache store.")
(defconst *user-data-directory*
  (path-join (getenv-or "XDG_DATA_HOME"
			(path-join *user-local-directory* "share"))
	     "emacs")
  "Path to user's local data store.")
(defconst *user-cache-directory*
  (path-join (getenv-or "XDG_CACHE_HOME"
			(path-join *user-home-directory* ".cache"))
	     "emacs")
  "Path to user's local cache store.")
(defconst *user-documents-directory*
  (path-join *user-home-directory* "Documents")
  "Path to user's documents directory.")

(defconst *user-el-get-directory*
  (path-join user-emacs-directory "el-get")
  "Path to user's el-get store.")
(defconst *user-local-init*
  (path-join *user-home-directory* ".emacs.local.el")
  "Path to user's machine-local configuration file.")

;; Set up the autosaves directory.
(defconst *user-auto-save-directory* (path-join *user-cache-directory* "auto-saves"))
;; Emacs will create the backup dir automatically, but not the autosaves dir.
(make-directory *user-auto-save-directory* t)


(provide 'setup-constants)
