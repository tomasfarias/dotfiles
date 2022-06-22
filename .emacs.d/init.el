(defconst emacs-start-time (current-time))

;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)

;; Configure straight
(straight-use-package 'use-package)
(straight-use-package 'el-patch)

;; Configure use-package to use straight.el by default
(use-package straight
             :custom (straight-use-package-by-default t))

(let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
  (message "Loaded straight in %.3fs" elapsed))

;; Load custom configuration
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; Load org configuration
(use-package org
  :ensure t)

(org-babel-load-file
 (expand-file-name "Emacs.org" user-emacs-directory))

;; Message how long it took to load everything
(let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
  (message "Loaded packages in %.3fs" elapsed))
