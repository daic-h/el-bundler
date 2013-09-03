;;; deferred-ext.el

;; Copyright (C) 2013  Daichi Hirata

;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(add-to-list 'load-path (expand-file-name
                         "lib/emacs-deferred"
                         (file-name-directory (or load-file-name buffer-file-name))))
(require 'concurrent)

(defun deferred:process:ext (proc-dir command &rest args)
  (deferred:process-gen:ext 'start-process proc-dir command args))

(defun deferred:process-gen:ext (f proc-dir command args)
  (lexical-let ((pd (deferred:process-buffer-gen:ext f proc-dir command args)) d)
    (setq d (deferred:nextc pd
              (lambda (buf)
                (prog1
                    (with-current-buffer buf (buffer-string))
                  (kill-buffer buf)))))
    (setf (deferred-cancel d)
          (lambda (x)
            (deferred:default-cancel d)
            (deferred:default-cancel pd)))
    d))

(defun deferred:process-buffer-gen:ext (f proc-dir command args)
  (let ((d (deferred:next)) (uid (deferred:uid)))
    (lexical-let
        ((f f) (proc-dir proc-dir) (command command) (args args)
         (proc-name (format "*deferred:*%s*:%s" command uid))
         (buf-name (format " *deferred:*%s*:%s" command uid))
         (nd (deferred:new)) proc-buf proc)
      (deferred:nextc d
        (lambda (x)
          (let ((default-directory proc-dir))
            (setq proc-buf (get-buffer-create buf-name))
            (condition-case err
                (progn
                  (setq proc
                        (if (null (car args))
                            (apply f proc-name buf-name command nil)
                          (apply f proc-name buf-name command args)))
                  (set-process-sentinel
                   proc
                   (lambda (proc event)
                     (cond
                      ((string-match "exited abnormally" event)
                       (let ((msg (if (buffer-live-p proc-buf)
                                      (deferred:buffer-string
                                        (format "Process [%s] exited abnormally : %%s"
                                                command) proc-buf)
                                    (concat "Process exited abnormally: " proc-name))))
                         (kill-buffer proc-buf)
                         (deferred:post-task nd 'ng msg)))
                      ((equal event "finished\n")
                       (deferred:post-task nd 'ok proc-buf)))))
                  (setf (deferred-cancel nd)
                        (lambda (x) (deferred:default-cancel x)
                          (when proc
                            (kill-process proc)
                            (kill-buffer proc-buf)))))
            (error (deferred:post-task nd 'ng (error-message-string err))))
            nil)))
        nd)))

(defmacro deferred:processc:ext (d proc-dir command &rest args)
  "Process chain of `deferred:process'."
  `(deferred:nextc ,d
     (lambda (,(gensym)) (deferred:process:ext ,proc-dir ,command ,@args))))

(provide 'deferred-ext)
