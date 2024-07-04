;;; Emacs
;;;
;;; My own init.el

;;; Contents:
;;;
;;;  - Package management
;;;  - Basic settings
;;;  - Environment variables
;;;  - Load configuration files

;; For time tracking, record start time
(defconst emacs-start-time (current-time))

(defun tomas/report-time-elapsed-for-execution (task since)
  "Report time it took since SINCE to do TASK."
  (let ((elapsed (float-time (time-subtract (current-time) since))))
    (message "Completed %s in %.3fs" task elapsed)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Package management
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Bootstrap straight.el
;;
;; This bootstrap code will look for a bootstrap.el file if it always cloned to
;; the local file-system. If it's not available, it will be downloaded from
;; straight.el's GitHub repository.
;;
;; Afterwards, use-package.el is installed and the straight.el integration is
;; enabled. This allows the usage of use-package.el syntax to install packages
;; without any extra settings, for example:
;;
;; (use-package org)

(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))

  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))

  (load bootstrap-file nil 'nomessage))

;; Install use-package.el
(straight-use-package 'use-package)
;; Use straight.el integration with use-package.el by default
(setopt straight-use-package-by-default t)

;; Progress report after initializing straight.el
(tomas/report-time-elapsed-for-execution "straight.el initialization" emacs-start-time)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Basic settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The startup screen displays important information about Emacs basic usage and
;; copyleft license. However, I am already familiar with its contents, to the point
;; that I do not need to see it all the time
(setopt inhibit-startup-screen t)

;; Set default major mode for *scratch* buffer. This buffer will be displayed to
;; a user who has uncommented the previous line, unless overridden with
;; initial-buffer-choice
(setopt initial-major-mode 'fundamental-mode)

;; Prefer UTF-8 encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setopt default-buffer-file-coding-system 'utf-8)

;; File backups
;;
;; By default, Emacs will backup files in the same directory as the file we are
;; editing. This can quickly become annoying, so it's better to send backups to
;; the system's temporary file directory. We achieve this by setting
;; backup-directory-alist to the temporary file directory ("/tmp" is commonly
;; used)
(setopt backup-directory-alist `((".*" . ,temporary-file-directory)))
(setopt backup-by-copying t
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)

;; Disable tabs indenting
(setopt indent-tabs-mode nil)

;; Revert file buffers when on-disk file changes
(global-auto-revert-mode 1)

;; Automatically delete trailing white space
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Not using a typewriter
(setopt sentence-end-double-space nil)

;; GPG auth sources
(setopt auth-sources '("~/.authinfo.gpg"))

;; Misc. UI tweaks
(blink-cursor-mode -1)
(menu-bar-mode -1) ; Useful to re-enable occasionally
(tool-bar-mode -1)
(tooltip-mode -1)
(pixel-scroll-precision-mode)
(pixel-scroll-mode 1)

(setopt mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
        mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
        mouse-wheel-follow-mouse 't ;; scroll window under mouse
        scroll-step 0
        scroll-conservatively 1000) ;; keyboard scroll one line at a time

(when (display-graphic-p)
  (scroll-bar-mode t)
  (mouse-wheel-mode t))

;; Progress report after basic settings
(tomas/report-time-elapsed-for-execution "loading basic settings" emacs-start-time)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Environment variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fetch environment variables from shell
;;
;; I run Emacs as a systemd daemon. For this reason, Emacs doesn't have certain
;; environment variables set, like $PATH. I use exec-path-from-shell to fix this.
(use-package exec-path-from-shell)
(setopt exec-path-from-shell-shell-name "/usr/bin/fish")

(dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
  (add-to-list 'exec-path-from-shell-variables var))

(when (memq window-system '(mac ns x pgtk))
  (exec-path-from-shell-initialize))
(when (daemonp)
  (exec-path-from-shell-initialize))

;; Progress report after fetching environment variables
(tomas/report-time-elapsed-for-execution "fetching environment variables from shell" emacs-start-time)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Load configuration files
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Install org-mode
;;
;; All my configuration files are org files. Org-mode allows for notebook-style
;; literal programming, which is great for documenting configuration. Org files
;; are loaded using org-babel-load-file.
;;
;; First, install a basic version of org-mode.
(use-package org)

(defun tomas/org-babel-load-file (file &optional compile)
  "Load Emacs Lisp source code blocks from an Org FILE.
After FILE is loaded, clean-up the tangled Emacs Lisp (.el) file.
COMPILE prefix argument is passed to underlying `org-babel-load-file'."
  (interactive "fFile to load: \nP")
  (let ((tangled-file (concat (file-name-sans-extension file) ".el"))
        (start-time (current-time)))

    (org-babel-load-file file compile)
    (when (file-exists-p tangled-file) (delete-file tangled-file))
    (tomas/report-time-elapsed-for-execution (concat "loading " file) start-time)))

;; Load base configuration file
;;
;; This file contains settings and packages that provide UI/UX enhancements,
;; provide theming support, and other basic enhancements.
(tomas/org-babel-load-file
 (expand-file-name "configs/base.org" user-emacs-directory))

;; Load org configuration file
;;
;; This file contains settings and packages that setup the org-mode environment
;; beyond ensuring org is installed to load the configuration files.
(tomas/org-babel-load-file
 (expand-file-name "configs/org.org" user-emacs-directory))

;; Load development configuration file
;;
;; This file contains settings and packages that make Emacs a fully-fledged IDE.
;; This includes programming major modes, formatting tools, Magit, project management,
;; and more!
(tomas/org-babel-load-file
 (expand-file-name "configs/development.org" user-emacs-directory))

;; Load RSS configuration file
;;
;; This configuration file setus up Elfeed, the Emacs RSS feed reader.
(tomas/org-babel-load-file
 (expand-file-name "configs/rss.org" user-emacs-directory))

;; Load IRC configuration file
;;
;; This configuration file setus up ERC, the Emacs IRC client.
(tomas/org-babel-load-file
 (expand-file-name "configs/irc.org" user-emacs-directory))

;; Message how long it took to load everything
(tomas/report-time-elapsed-for-execution "init.el" emacs-start-time)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("ffafb0e9f63935183713b204c11d22225008559fa62133a69848835f4f4a758c" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
