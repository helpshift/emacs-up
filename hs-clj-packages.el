;;; hs-clj-packages.el --- Functions to enable development against
;;; older versions of Clojure, if required.
;;; Author: Vedang Manerikar
;;; Created on: 10 Jul 2016
;;; Commentary:

;; This file depends on functionality provided by El-Get and should be
;; loaded after el-get has been initialized.

;;; Code:

(defvar use-older-clj-versions nil
  "Variable to indicate that you work on Clojure 1.6 or below.
Supporting development for Clojure 1.6 or below requires
installing and setting up older versions of `cider',
`clojure-mode' and `clj-refactor'.  Set this variable to 't' if
you are working against older Clojure versions.")

(defvar use-older-clj-packages nil
  "Variable to indicate whether you are currently using older or
newer versions of `cider', `clojure-mode' and `clj-refactor'.
Set this variable to 't' if you are working against older Clojure
versions.")

(defvar clj-packages-install-file
  (concat tempfiles-dirname ".clj-packages")
  "A file to store whether we are currently using old or new clj-packages.")

;; Reset the `use-older-clj-packages' var to it's actual value:
(load clj-packages-install-file 'noerror)

(defun hs-store-clojure-env-ver (is-older-env-installed)
  "Store the Clojure env version that we've installed.
The variable IS-OLDER-ENV-INSTALLED is a boolean."
  (with-temp-file clj-packages-install-file
    (print `(setq use-older-clj-packages ,is-older-env-installed)
           (current-buffer))))

(defun cider-repl-prompt-on-newline (ns)
  "Return a prompt string with newline.
NS is the namespace information passed into the function by
cider."
  (concat ns ">\n"))

(defun load-cider-config ()
  "Configuration for CIDER."
  (eval-after-load 'clojure-mode
    '(progn (add-hook 'clojure-mode-hook
                      'enable-paredit-mode)))
  (eval-after-load 'cider-repl
    '(progn (add-hook 'cider-repl-mode-hook
                      'enable-paredit-mode)
            (define-key cider-repl-mode-map
              (kbd "C-M-q")
              'prog-indent-sexp)
            (define-key cider-repl-mode-map
              (kbd "C-c M-o")
              'cider-repl-clear-buffer)))
  (eval-after-load 'cider-mode
    '(progn
       (setq cider-repl-history-file
             (concat tempfiles-dirname "cider-history.txt")
             cider-repl-history-size most-positive-fixnum
             cider-repl-wrap-history t
             cider-repl-prompt-function 'cider-repl-prompt-on-newline
             nrepl-buffer-name-separator "-"
             nrepl-buffer-name-show-port t
             nrepl-log-messages t
             cider-annotate-completion-candidates t
             cider-completion-annotations-include-ns 'always
             cider-show-error-buffer 'always
             cider-prompt-for-symbol nil
             cider-apropos-actions
             '(("find-def" . cider--find-var)
               ("display-doc" . cider-doc-lookup)
               ("lookup-on-grimoire" . cider-grimoire-lookup)))

       (add-hook 'cider-mode-hook 'eldoc-mode))))

(defun turn-on-clj-refactor ()
  (clj-refactor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c m"))

(defun load-clj-refactor-config ()
  "Configuration for clj-refactor."
  (setq cljr-favor-prefix-notation nil
        ;; stops cljr from running tests when
        ;; we connect to the repl
        cljr-eagerly-build-asts-on-startup nil)

  (eval-after-load 'clojure-mode
    '(progn
       (add-hook 'clojure-mode-hook
                 'turn-on-clj-refactor))))

(defvar hs--clojure16-env
  '(;; Clojure Interactive Development Environment that Rocks
    ;; First Change the dependencies to older versions
    (:name clojure-mode
           :checkout "5.1.0")
    (:name cider
           :checkout "v0.10.2"
           :after (progn (load-cider-config)))
    (:name clj-refactor
           :checkout "1.1.0"
           :after (progn (load-clj-refactor-config))))
  "Return a list of `el-get-sources' for development against Clojure 1.6.")

(defvar hs--latest-stable-clojure-env
  '(;; Clojure Interactive Development Environment that Rocks
    (:name cider
           :checkout "v0.14.0"
           :after (progn (load-cider-config)))
    ;; A collection of simple clojure refactoring functions
    (:name clj-refactor
           :checkout "2.2.0"
           :after (progn (load-clj-refactor-config))))
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
  "Return a list of stable `el-get-sources' for development against Clojure (both latest as well as older versions of Clojure)")

(defun hs-clojure16-env ()
  "Return a list of `el-get-sources' for development against Clojure 1.6."
  (append hs--clojure16-env hs--common-env))

(defun hs-latest-stable-clojure-env ()
  "Return a list of stable `el-get-sources' for development against the latest Clojure."
  (append hs--latest-stable-clojure-env hs--common-env))

(defun hs-cleanup-previous-install-if-necessary ()
  "If Emacs packages have been installed for Clojure development,
  check if they are compatible with the Clojure we plan to work
  with."
  (cond ((and use-older-clj-versions
              (not use-older-clj-packages))
         (mapcar 'el-get-remove
                 (mapcar 'el-get-source-name hs-clojure16-env)))

        ((and (not use-older-clj-versions)
              use-older-clj-packages)
         (mapcar 'el-get-remove
                 (mapcar 'el-get-source-name hs-clojure16-env)))))


(provide 'hs-clj-packages)
;;; hs-clj-packages.el ends here
