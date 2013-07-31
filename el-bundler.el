;; (el-bundler:initialize)
;; (el-bundler:install)
;; (el-bundler:update)
;; (el-bundler:update-all)
;; (el-bundler:remove)
;; (el-bundler:clear)

(eval-when-compile (require 'cl))
(require 'dired)
(require 'deferred-ext)

(defvar el-bundler-directory "~/.emacs.d/elisp-bundle")

(defvar el-bundler-process-num 3)
(defvar el-bundler-buffer-init "*el-bundler:initialize*")
(defvar el-bundler-buffer-cmd  "*el-bundler:command*")

(defvar el-bundler-display-function 'display-buffer)
(defvar el-bundler-show-result t)

(defconst el-bundler-packages nil)
(defconst el-bundler-method-table (make-hash-table))
(defconst el-bundler-github-url-format "https://github.com/%s.git")

(defvar el-bundler-counter (cons 0 0))
(defvar el-bundler-progress-length 40)
(defvar el-bundler-progress-mark "=")


(defun el-bundler:dir ()
  (file-name-as-directory el-bundler-directory))

(defun el-bundler:plist-get (list label default)
  (if (plist-member list label) (plist-get list label) default))

(defun el-bundler:basename (url)
  (file-name-nondirectory (directory-file-name url)))

(defun el-bundler:github-url (name)
  (format el-bundler-github-url-format name))

(defun el-bundler:add-lood-path (path)
  (add-to-list 'load-path path)
  (el-bundler:init-message "load-path: %S" path))

(defun* el-bundler:configure (&key (dir el-bundler-directory)
                                   (process-num el-bundler-process-num))
  (setq el-bundler-directory dir
        el-bundler-process-num process-num))


;; Buffer
(defun el-bundler:init-message (&rest string)
  (el-bundler:message el-bundler-buffer-init (apply 'format string)))

(defun el-bundler:cmd-message (&rest string)
  (el-bundler:message el-bundler-buffer-cmd (apply 'format string)))

(defun el-bundler:clear-buffer ()
  (with-current-buffer (get-buffer-create el-bundler-buffer-cmd)
    (erase-buffer)))

(defun el-bundler:message (buffer-name string)
  (with-current-buffer (get-buffer-create buffer-name)
    (save-excursion
      (goto-char (point-max))
      (insert (concat string "\n")))))

(defun el-bundler:display-function ()
  (when el-bundler-show-result
    (funcall el-bundler-display-function el-bundler-buffer-cmd)))


;; Progress
(defun el-bundler:progress-set (denom)
  (setq el-bundler-counter (cons 0 denom))
  (el-bundler:progress-update))

(defun el-bundler:progress-inc ()
  (incf (car el-bundler-counter))
  (el-bundler:progress-update))

(defun el-bundler:progress-update ()
  (let* ((numer (car el-bundler-counter))
         (denom (cdr el-bundler-counter))
         (title (if (= numer denom) "Complete" "Running"))
         (bar (el-bundler:progress-calc-bar numer denom)))
    (el-bundler:progress-message "%s: %s %s/%s" title bar numer denom)))

(defun el-bundler:progress-calc-bar (numer denom)
  (format (concat "|%-" (number-to-string el-bundler-progress-length) "s|")
          (make-string (round (* el-bundler-progress-length (/ (float numer) (float denom))))
                       (string-to-char el-bundler-progress-mark))))

(defun el-bundler:progress-message (&rest string)
  (with-current-buffer (get-buffer-create el-bundler-buffer-cmd)
    (save-excursion
      (goto-char (point-min))
      (unless (= 1 (point-max)) (kill-whole-line))
      (insert (concat (apply 'format string) "\n")))))


;; Package
(defmacro el-bundler:packages (&rest packages)
  `(setq el-bundler-packages ',packages))

(defun el-bundler:package-path (package-name)
  (file-name-as-directory (concat (el-bundler:dir) package-name)))

(defun el-bundler:package-exists-p (package-name)
  (file-exists-p (el-bundler:package-path package-name)))

(defun el-bundler:package-name (url_or_path)
  (replace-regexp-in-string "\\.git" "" (el-bundler:basename url_or_path)))

(defun el-bundler:get-package-name (package)
  (let ((name (plist-get package :name))
        (url  (plist-get package :url)))
    (el-bundler:package-name (or name url))))

(defun el-bundler:package-name-list ()
  (loop for package in el-bundler-packages
        collect (el-bundler:get-package-name package)))

(defun el-bundler:package-from-name (package-name)
  (loop for package in el-bundler-packages
        for _name = (file-name-nondirectory (plist-get package :name))
        if (string= package-name _name) return package))

(defun el-bundler:package-exec (package action)
  (let* ((type (plist-get package :type))
         (method (el-bundler:select-method type action)))
    (funcall method package)))

(defun el-bundler:unnecessary-packages ()
  (loop for filename in (directory-files el-bundler-directory t)
        for basename = (file-name-nondirectory filename)
        unless (string-match dired-trivial-filenames basename)
        unless (member basename (el-bundler:package-name-list))
        collect basename))

(defun el-bundler:remove-package (package-name)
  (el-bundler:remove-packages (list package-name)))

(defun el-bundler:remove-packages (packages)
  (loop for package in packages
        for path = (el-bundler:package-path package)
        if (file-exists-p path)
        do (dired-delete-file path 'always)
           (el-bundler:cmd-message "[OK] Package: %S was Deleted." path)
        else
        do (el-bundler:cmd-message "[NG] Package: %S was not Exists." path)
        end
        do (el-bundler:progress-inc)))

(defun el-bundler:load-path-list ()
  (loop for package in el-bundler-packages
        for name = (el-bundler:get-package-name package)
        for dir  = (el-bundler:package-path name)
        if (file-directory-p dir)
        collect dir))

(defun* el-bundler:regist-type (type &key install update)
  (let (methods)
    (loop for action in '(install update)
          do (setq methods (plist-put methods (intern (format ":%s" action))
                                      (symbol-value action))))
    (puthash type methods el-bundler-method-table)))

(defun el-bundler:collect-package-task (action)
  (loop for package in el-bundler-packages
        for type   = (plist-get package :type)
        for method = (el-bundler:select-method type action)
        collect `(lambda () (,method ',package))))

(defun el-bundler:select-method (type action)
  (plist-get (gethash (intern (format ":%s" type))
                      el-bundler-method-table) action))


(defun el-bundler:async-byte-compile (package-dir)
  (el-bundler:async-exec "el-bundler:byte-compile" package-dir))

(defun el-bundler:async-exec (func-name default-dir)
  (lexical-let ((el-bundler-directory (symbol-file 'el-bundler:async-exec)))
    (deferred:process:ext default-dir
      "emacs" "-batch"
      "--eval" (format "(setq default-directory %S)" default-dir)
      "--eval" (format "(setq load-path (append '%S load-path)))" load-path)
      "-L" (file-name-directory el-bundler-directory)
      "-l" (file-name-sans-extension el-bundler-directory)
      "-f" func-name)))

(defun el-bundler:byte-compile ()
  (let (byte-compile-warnings emacs-lisp-mode-hook)
    (add-to-list 'load-path default-directory)
    (loop for filename in (directory-files default-directory t)
          for basename = (file-name-nondirectory filename)
          unless (string-match dired-trivial-filenames basename)
          if (file-directory-p filename)
          do (byte-recompile-directory filename 0)
          else
          do (and (string-match "\\.el$" basename) (byte-compile-file filename)))))


(defun el-bundler:github-clone (package)
  (let* ((name (plist-get package :name))
         (url  (el-bundler:github-url name)))
    (el-bundler:git-clone (append package (list :url url)))))

(defun el-bundler:github-pull (package)
  (let* ((name (plist-get package :name))
         (url  (el-bundler:github-url name)))
    (el-bundler:git-pull (append package (list :url url)))))

(el-bundler:regist-type :github
 :install #'el-bundler:github-clone
 :update  #'el-bundler:github-pull)


(defun el-bundler:git-clone (package)
  (lexical-let* ((url    (plist-get package :url))
                 (name   (el-bundler:package-name url))
                 (dir    (el-bundler:package-path name))
                 (branch (el-bundler:plist-get package :branch "master")))
    (cond
     ((file-exists-p dir)
      (el-bundler:progress-inc)
      (el-bundler:cmd-message "[-] Package %s:%s Exist." name branch))
     (t
      (deferred:$
        (deferred:process:ext (el-bundler:dir)
          "git" "--no-pager" "clone" "--recursive" "-b" branch url)
        (deferred:nextc it
          (lambda () (el-bundler:async-byte-compile dir)))
        (deferred:nextc it
          (lambda () (el-bundler:cmd-message "[OK] Package %s:%s Installed." name branch)))
        (deferred:error it
          (lambda (err) (el-bundler:cmd-message "[NG] Package %s:%s Install Failure.\n %s" name branch err)))
        (deferred:watch it
          (lambda () (el-bundler:progress-inc))))))))

(defun el-bundler:git-pull (package)
  (lexical-let* ((url    (plist-get package :url))
                 (name   (el-bundler:package-name url))
                 (dir    (el-bundler:package-path name))
                 (branch (el-bundler:plist-get package :branch "master")))
    (deferred:$
      (deferred:process:ext dir
        "git" "--no-pager" "pull" "--recurse-submodules")
      (deferred:nextc it
        (lambda () (el-bundler:async-byte-compile dir)))
      (deferred:nextc it
        (lambda () (el-bundler:cmd-message "[OK] Package %s:%s Updated." name branch)))
      (deferred:error it
        (lambda (err) (el-bundler:cmd-message "[NG] Package %s:%s Updated Failuer.\n %s" name branch err)))
      (deferred:watch it
        (lambda () (el-bundler:progress-inc))))))

(el-bundler:regist-type :git
 :install #'el-bundler:git-clone
 :update  #'el-bundler:git-pull)


(defun el-bundler:initialize ()
  (interactive)
  (loop for path in (el-bundler:load-path-list)
        for fullpath = (file-name-directory (expand-file-name path))
        do (el-bundler:add-lood-path fullpath)))

(defun el-bundler:install ()
  (interactive)
  (let ((semaphore (cc:semaphore-create el-bundler-process-num))
        (task-list (el-bundler:collect-package-task :install)))
    (el-bundler:clear-buffer)
    (el-bundler:progress-set (length task-list))
    (loop for task in task-list
          do (cc:semaphore-with semaphore task)))
  (el-bundler:display-function))

(defun el-bundler:update ()
  (interactive)
  (let* ((minibuffer-history (el-bundler:package-name-list))
         (name (read-string "package: "))
         (package (el-bundler:package-from-name name)))
    (el-bundler:clear-buffer)
    (el-bundler:progress-set 1)
    (if (el-bundler:package-exists-p name)
        (el-bundler:package-exec package :update)
      (el-bundler:progress-inc)
      (el-bundler:cmd-message "[NG] Package: %s is not found." name)))
  (el-bundler:display-function))

(defun el-bundler:update-all ()
  (interactive)
  (let ((semaphore (cc:semaphore-create el-bundler-process-num))
        (task-list (el-bundler:collect-package-task :update)))
    (el-bundler:clear-buffer)
    (el-bundler:progress-set (length task-list))
    (loop for task in task-list
          do (cc:semaphore-with semaphore task)))
  (el-bundler:display-function))

(defun el-bundler:remove ()
  (interactive)
  (let* ((minibuffer-history (el-bundler:package-name-list))
         (name (read-string "package: "))
         (package (el-bundler:package-from-name name)))
    (el-bundler:clear-buffer)
    (el-bundler:progress-set 1)
    (if package
        (el-bundler:remove-packages (list name))
      (el-bundler:progress-inc)
      (el-bundler:cmd-message "[NG] Package: %s is not found." name)))
  (el-bundler:display-function))

(defun el-bundler:clear ()
  (interactive)
  (let ((unnecessary-packages (el-bundler:unnecessary-packages)))
    (when unnecessary-packages
      (el-bundler:clear-buffer)
      (el-bundler:progress-set (length unnecessary-packages))
      (el-bundler:remove-packages unnecessary-packages))))

(defun el-bundler:install! ()
  (interactive)
  (el-bundler:clear)
  (el-bundler:install))

(provide 'el-bundler)
