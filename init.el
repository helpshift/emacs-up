;;; init.el --- Make Emacs useful!
;;; Author: Vedang Manerikar
;;; Created on: 10 Jul 2016
;;; Commentary:

;; This file is a bare minimum configuration file to enable working
;; with Emacs for Helpshift newcomers.

;;; Code:

(when (version< emacs-version "27")
  (error "Unsupported Emacs Version! Please upgrade to a newer Emacs.  Emacs installation instructions: https://www.gnu.org/software/emacs/download.html"))

(defvar emacs-up--version "v3.4.1"
  "The current version of the Emacs Up Starter Kit.")

(defun emacs-up-version ()
  "Return the current version of the Emacs Up Starter Kit."
  (interactive)
  (message "Emacs Up %s" emacs-up--version))

;; Set a directory for temporary/state related files.
(defvar dotfiles-dirname
  (file-name-directory (or load-file-name
                           (buffer-file-name)))
  "The directory where this code is running from.
Ideally, this will be ~/.emacs.d.")
(defvar tempfiles-dirname
  (concat dotfiles-dirname "temp-files/")
  "A sub-directory to hold temporary files generated by Emacs.")

;; Create the temp-files folder if necessary.
(make-directory tempfiles-dirname t)

;;; El-Get for great good.
(defvar el-get-dir
  (concat dotfiles-dirname "el-get/")
  "The sub-directory where el-get packages are installed.")

(defvar el-get-user-package-directory
  (concat dotfiles-dirname "el-get-config/")
  "The sub-directory where optional user-configuration for various packages, and user-defined recipes live.")

(defvar el-get-my-recipes
  (concat el-get-user-package-directory "personal-recipes/")
  "The sub-directory where user-defined recipes live, if the user needs to define and install his/her own recipes.")

;; Make the el-get directories if required
(make-directory el-get-dir t)
(make-directory el-get-my-recipes t)

;; Add el-get to the load-path. From this point onward, we're plugged
;; into the el-get package management system.
(add-to-list 'load-path (concat el-get-dir "el-get"))

;; Install el-get if it isn't already present
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch
          el-get-install-skip-emacswiki-recipes)
      (goto-char (point-max))
      (eval-print-last-sexp))))

;; Add our personal recipes to el-get's recipe path
(add-to-list 'el-get-recipe-path el-get-my-recipes)

;;; Load packaging info for clojure
(defvar clj-packages-file
  (concat dotfiles-dirname "hs-clj-packages.el")
  "Information about packages to be installed for Clojure development.

Also contains along with versions and other config.")

(load clj-packages-file)

;;; This is the order in which the packages are loaded. Changing this
;;; order can sometimes lead to nasty surprises: eg: when you are
;;; overshadowing some in-built libraries or when you expect a package
;;; to already be loaded in order to fix system paths (*cough*
;;; `exec-path-from-shell' *cough*)

(setq el-get-sources
      (append

       (when (and (eq system-type 'darwin)
                  (eq window-system 'ns))
         ;; Emacs plugin for dynamic PATH loading - Fix Emacs's
         ;; understanding of the the Path var on Mac.
         '((:name exec-path-from-shell
                  :after (progn (exec-path-from-shell-initialize)))))

       '(;; Jump to things in Emacs tree-style.
         (:name avy
                :after (progn (global-set-key (kbd "M-g g") 'avy-goto-line)
                              (global-set-key (kbd "M-g SPC") 'avy-goto-word-1)
                              (avy-setup-default)))

         ;; Fixing weird quirks and poor defaults
         (:name better-defaults)
         (:name plantuml-mode
                :after (progn (setq plantuml-default-exec-mode 'jar)
                              (add-to-list 'auto-mode-alist
                                           '("\\.puml$" . plantuml-mode))))

         ;; A low contrast color theme for Emacs.
         (:name color-theme-zenburn)

         ;; Modular in-buffer completion framework for Emacs
         (:name company-mode
                :after (progn (require 'company)
                              (add-hook 'after-init-hook 'global-company-mode)
                              (setq-default company-lighter " cmp")
                              (define-key company-active-map
                                [tab] 'company-complete)
                              (define-key company-active-map
                                (kbd "TAB") 'company-complete)))

         ;; an Emacs jump to definition package for 40+ languages
         (:name dumb-jump
                :after (progn (dumb-jump-mode)
                              (define-key dumb-jump-mode-map (kbd "C-c d g")
                                'dumb-jump-go)
                              (define-key dumb-jump-mode-map (kbd "C-c d b")
                                'dumb-jump-back)
                              (define-key dumb-jump-mode-map (kbd "C-c d q")
                                'dumb-jump-quick-look)
                              ;; Don't mess with the default
                              ;; indentation keybinding
                              (define-key dumb-jump-mode-map (kbd "C-M-q")
                                nil)))

         ;; On-the-fly syntax checking
         (:name flycheck
                :after (progn (setq flycheck-global-modes '(not org-mode))
                              (global-flycheck-mode)))

         ;; Emacs incremental completion and narrowing framework
         (:name helm
                :after (progn ;; Explicitly turn off global `helm-mode'.
                         ;; Only use it where required. Prefer `ido'
                         ;; globally.
                         (helm-mode -1)
                         ;; Various useful key-bindings (other than Helm Defaults)
                         ;; Useful Helm Defaults: C-x c i, C-x c I
                         ;; unset this because I plan to use it as a prefix key.
                         (global-set-key (kbd "C-x c r") nil)
                         (global-set-key (kbd "C-x c r b") 'helm-filtered-bookmarks)
                         (global-set-key (kbd "C-x c r r") 'helm-regexp)
                         (global-set-key (kbd "C-x c C-b") 'helm-mini)
                         (global-set-key (kbd "M-y") 'helm-show-kill-ring)
                         (global-set-key (kbd "C-x c SPC") 'helm-all-mark-rings)
                         (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
                         (global-set-key (kbd "C-x c r i") 'helm-register)))
         (:name helm-ag
                :after (progn (setq helm-ag-insert-at-point 'symbol
                                    helm-ag-fuzzy-match t)
                              (global-set-key (kbd "C-x c g a") 'helm-do-ag-project-root)
                              (global-set-key (kbd "C-x c g s") 'helm-do-ag)
                              ;; Move old behaviour to a new key
                              (global-set-key (kbd "C-x c g g") 'helm-do-grep-ag)))
         (:name helm-projectile
                :before (progn (setq projectile-keymap-prefix (kbd "C-x c p")))
                :after (progn (require 'helm-projectile)
                              (projectile-mode)
                              (setq projectile-completion-system 'helm
                                    projectile-switch-project-action 'helm-projectile
                                    projectile-enable-caching t
                                    projectile-cache-file (concat tempfiles-dirname "projectile.cache")
                                    projectile-known-projects-file (concat tempfiles-dirname "projectile-bookmarks.eld")
                                    projectile-mode-line '(:eval (if (file-remote-p default-directory)
                                                                     " "
                                                                   (format " Ptl[%s]"
                                                                           (projectile-project-name)))))
                              ;; I want to use <C-x c p> for helm-projectile
                              (global-set-key (kbd "C-x c P") 'helm-list-emacs-process)
                              (helm-projectile-on)))

         ;; Use ido (nearly) everywhere
         ;; settings for this package are loaded below in the ido section.
         (:name ido-completing-read-plus)

         ;; It's Magit! An Emacs mode for Git.
         (:name magit
                :after (progn (global-set-key (kbd "C-x g") 'magit-status)
                              (setq magit-completing-read-function 'magit-ido-completing-read)))

         ;; Minor mode for editing parentheses
         (:name paredit
                :after (progn (eval-after-load 'paredit
                                '(progn
                                   (define-key paredit-mode-map (kbd "C-o") 'paredit-open-round)))
                              (add-hook 'emacs-lisp-mode-hook
                                        'enable-paredit-mode)))

         ;; Major mode for plantUML
         (:name plantuml-mode
                :after (progn (setq plantuml-default-exec-mode 'jar)
                              (add-to-list 'auto-mode-alist
                                           '("\\.puml$" . plantuml-mode))))

         ;; Format JS, JSX files on save event.
         ;; Prerequisite: npm install -g prettier`
         (:name prettier-js
                :after (add-hook 'rjsx-mode-hook 'Prettier-Js-mode))

         ;; Major mode for JSX and JS files
         (:name rjsx-mode
                :after (progn (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
                              (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
                              (setq js2-basic-offset 2
                                    js-switch-indent-offset 2)))

         ;; M-x interface with Ido-style fuzzy matching.
         (:name smex
                :after (progn (smex-initialize)
                              (global-set-key (kbd "M-x") 'smex)))

         ;; A collection of snippets for repetitive stuff
         (:name yasnippet
                :after (progn (yas-global-mode 1)
                              (add-to-list 'hippie-expand-try-functions-list
                                           'yas-hippie-try-expand)))
         (:name yasnippet-snippets))

       (cond
        ;; Set up recipes to support development against older
        ;; Clojure versions
        ((equal "clj16-" clj-version)
         (progn (hs-cleanup-previous-install-if-necessary)
                (hs-clojure16-env)))
        ;; Set up recipes to support development against Clojure
        ;; version 1.7
        ((equal "clj17" clj-version)
         (progn (hs-cleanup-previous-install-if-necessary)
                (hs-clojure17-env)))
        ;; Latest Clojure
        ((equal "clj18+" clj-version)
         (progn (hs-cleanup-previous-install-if-necessary)
                (hs-latest-stable-clojure-env))))))

(el-get 'sync
        (mapcar 'el-get-source-name el-get-sources))

(hs-store-clojure-env-ver)

;; Modify the CMD key to be my Meta key
(setq mac-command-modifier 'meta)

;;; Recentf settings
;; Use recentf via helm, invoke it with <C-x c C-c f>
(require 'recentf)
(setq recentf-exclude (list (concat tempfiles-dirname "*"))
      recentf-save-file (concat tempfiles-dirname "recentf")
      recentf-max-saved-items 1000
      recentf-max-menu-items 1000)
(recentf-mode)

;;; Interactively Do Things
;; Ido settings
(require 'ido)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t
      ido-create-new-buffer 'always
      ido-save-directory-list-file (concat tempfiles-dirname "ido.last"))
(ido-mode t)
(ido-everywhere)
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)
(add-hook 'ido-make-buffer-list-hook 'ido-summary-buffers-to-end)

;;; Saveplace Settings
(require 'saveplace)
(setq save-place-file (concat tempfiles-dirname "places"))
(save-place-mode)

;;; Desktop Settings
(require 'desktop)
(add-to-list 'desktop-path tempfiles-dirname)

;; Move Emacs state into the temp folder we've created.
(setq  backup-directory-alist `(("." . ,(concat tempfiles-dirname "backups"))))
;; Turn off the anoying bell sound
(setq visible-bell t)

;; Theme and Look
;; This should load after `custom-safe-themes' to avoid Emacs
;; panicking about whether it is safe or not.
(load-theme 'zenburn t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(package-initialize)

(provide 'init)
;;; init.el ends here
