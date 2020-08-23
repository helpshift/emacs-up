;;; hs-clj-packages.el --- Functions to enable development against
;;; older versions of Clojure, if required.
;;; Author: Vedang Manerikar
;;; Created on: 10 Jul 2016
;;; Commentary:

;; This file depends on functionality provided by El-Get and should be
;; loaded after el-get has been initialized.

;;; Code:

(defvar clj-version "clj18+"
  "Variable to indicate the version of Clojure that you work on.

The default value is `clj18+'. Other possible values are `clj17'
and `clj16-'. Supporting development for Clojure 1.6 or below
requires installing and setting up older versions of `cider',
`clojure-mode' and `clj-refactor'. Set this variable to 'clj17'
if you are working against Clojure 1.7. Set this variable to
'clj16-' if you are working against Clojure 1.6 or older.")

(defvar installed-clj-packages-version "clj18+"
  "Variable to indicate the current version of installed clj-packages.

This variable is automatically set/reset by code based on the
value of `clj-version'.")

(defvar clj-packages-install-file
  (concat tempfiles-dirname ".clj-packages")
  "A file to store whether we are currently using old or new clj-packages.")

;; Reset the `installed-clj-packages-version' var to it's actual value:
(load clj-packages-install-file 'noerror)

(defun hs-store-clojure-env-ver ()
  "Store the Clojure env version that we've installed."
  (with-temp-file clj-packages-install-file
    (print `(setq installed-clj-packages-version ,clj-version)
           (current-buffer))))

(defun cider-repl-prompt-on-newline (ns)
  "Return a prompt string with newline.
NS is the namespace information passed into the function by
cider."
  (concat ns ">\n"))

(defun load-cider-config ()
  "Configuration for CIDER."
  (eval-after-load 'clojure-mode
    '(progn (add-hook 'clojure-mode-hook 'enable-paredit-mode)))
  (eval-after-load 'cider-repl
    '(progn (add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
            (define-key cider-repl-mode-map (kbd "C-M-q") 'prog-indent-sexp)
            (define-key cider-repl-mode-map (kbd "C-c M-o") 'cider-repl-clear-buffer)))
  (eval-after-load 'cider-mode
    '(progn
       (setq cider-repl-history-file
             (concat tempfiles-dirname "cider-history.txt")
             cider-repl-history-size most-positive-fixnum
             cider-repl-wrap-history t
             cider-repl-prompt-function 'cider-repl-prompt-on-newline
             cider-annotate-completion-candidates t
             cider-completion-annotations-include-ns 'always
             cider-show-error-buffer 'always
             cider-prompt-for-symbol nil
             cider-auto-jump-to-error 'errors-only
             cider-apropos-actions
             '(("find-def" . cider--find-var)
               ("display-doc" . cider-doc-lookup)
               ("lookup-on-grimoire" . cider-grimoire-lookup)))

       (add-hook 'cider-mode-hook 'eldoc-mode))))

(defun turn-on-clj-refactor ()
  "Helper function to add as a hook to `clojure-mode'."
  (clj-refactor-mode +1)
  (cljr-add-keybindings-with-prefix "C-c m"))

(defun turn-on-cljstyle ()
  "Utility function to turn on `cljstyle-mode' and auto-formatting."
  (if (executable-find "cljstyle")
      (cljstyle-mode +1)
    (message "Could not find `cljstyle' on $PATH. Please ensure you have installed it correctly.")))

(defun load-clj-refactor-config ()
  "Configuration for `clj-refactor-mode'."
  (setq cljr-favor-prefix-notation nil
        cljr-eagerly-build-asts-on-startup t
        cljr-warn-on-eval nil
        ;; don't stop on analyzer failures. examples: not finding a
        ;; data-reader.
        cljr-ignore-analyzer-errors t)

  (eval-after-load 'clojure-mode
    '(progn
       (add-hook 'clojure-mode-hook 'turn-on-clj-refactor))))

(defvar hs--clojure16-env
  '((:name clojure-mode
           :checkout "5.1.0")
    (:name cider
           :checkout "v0.10.2"
           :after (progn (load-cider-config)))
    (:name clj-refactor
           :checkout "1.1.0"
           :after (progn (load-clj-refactor-config))))
  "Return a list of `el-get-sources' for development against Clojure 1.6 and below.")

(defvar hs--clojure17-env
  '((:name clojure-mode
           :checkout "5.6.0")
    (:name cider
           :checkout "v0.17.0"
           :after (progn (load-cider-config)))
    (:name clj-refactor
           :checkout "2.3.1"
           :after (progn (load-clj-refactor-config))))
  "Return a list of `el-get-sources' for development against Clojure 1.7.")

(defvar hs--latest-stable-clojure-env
  '(;; Clojure Interactive Development Environment that Rocks
    (:name cider
           :checkout "v0.26.1"
           :after (progn (load-cider-config)))
    ;; A collection of simple clojure refactoring functions
    (:name clj-refactor
           :checkout "v2.5.0"
           :after (progn (load-clj-refactor-config)))
    ;; On the fly syntax checking for Clojure, via clj-kondo. You
    ;; need to install clj-kondo by following installation
    ;; instructions at
    ;; https://github.com/borkdude/clj-kondo/blob/master/doc/install.md
    (:name flycheck-clj-kondo)
    ;; Add formatting via `cljstyle' as a minor-mode, so that Clojure
    ;; files are always well-formatted. Note: Turning cljstyle on by
    ;; default is disabled, because it breaks marks and jumping. I
    ;; will remove it / fix it in a future version.
    (:name cljstyle
           ;; :after (progn (add-hook 'clojure-mode-hook 'turn-on-cljstyle))
           ))
  "Return a list of stable `el-get-sources' for development against the latest Clojure.")

(defvar hs--common-env
  '(;; HELM interface to CIDER.
    (:name helm-cider
           :after (progn (eval-after-load 'cider-mode
                           '(progn (helm-cider-mode 1)
                                   (setq helm-cider-apropos-actions
                                         '(("Find definition" lambda
                                            (candidate)
                                            (cider-find-var nil candidate))
                                           ("CiderDoc" . cider-doc-lookup)
                                           ("Find on Grimoire" . cider-grimoire-lookup)))
                                   ;; define keys for apropos that follow helm conventions
                                   (define-key cider-mode-map (kbd "C-x c d n") 'cider-browse-ns)
                                   (define-key cider-mode-map (kbd "C-x c d a") 'cider-apropos)
                                   (define-key cider-mode-map (kbd "C-x c d e") 'cider-apropos-documentation))))))
  "Return a list of stable `el-get-sources' for development against Clojure.

Handles both latest as well as older versions of Clojure.")

(defun hs-clojure16-env ()
  "Return a list of `el-get-sources' for development against Clojure 1.6 or less."
  (append hs--clojure16-env hs--common-env))

(defun hs-clojure16-env ()
  "Return a list of `el-get-sources' for development against Clojure 1.6 or less."
  (append hs--clojure17-env hs--common-env))

(defun hs-latest-stable-clojure-env ()
  "Return a list of stable `el-get-sources' for development against the latest Clojure."
  (append hs--latest-stable-clojure-env hs--common-env))

(defun hs-cleanup-previous-install-if-necessary ()
  "Cleanup Emacs packages installed for Clojure development, if required."
  (cond ((and (equal "clj16-" clj-version)
              (not (equal "clj16-" installed-clj-packages-version)))
         ;; Remove whatever may be installed by `hs-clojure16-env' (so
         ;; that it will be cleanly installed again).
         (mapcar 'el-get-remove
                 (mapcar 'el-get-source-name (hs-clojure16-env))))

        ((and (equal "clj17" clj-version)
              (not (equal "clj17" installed-clj-packages-version)))
         ;; Remove whatever may be installed by `hs-clojure17-env' (so
         ;; that it will be cleanly installed again).
         (mapcar 'el-get-remove
                 (mapcar 'el-get-source-name (hs-clojure17-env))))

        ((and (equal "clj18+" clj-version)
              (not (equal "clj18+" installed-clj-packages-version)))
         (mapcar 'el-get-remove
                 (mapcar 'el-get-source-name (hs-latest-stable-clojure-env))))))


(provide 'hs-clj-packages)
;;; hs-clj-packages.el ends here
