;;;;; emacs -q -l ~/.orgmode.el

;;;; 1) Variables
(defvar *use-home* (concat (expand-file-name "~") "/"))
(defvar *plugin-path* (concat *use-home* ".emacs.d/plugins/"))

(defvar *python* "/path/to/python")
(defvar *ipython-args* "-i /path/to/ipython-script.py")

;;; getwd() / setwd()
(defvar *org-path* "/path/to/orgnotes/")
(setq default-directory *org-path*)

;;;; 2) Emacs default setup
;; Normally, we do not keep backup files
(setq make-backup-files nil)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-message t)
(column-number-mode t)
(line-number-mode t)
(show-paren-mode t)
(delete-selection-mode t)
;; highlight text selection
(transient-mark-mode t)
;; turn on syntax highlighting
(global-font-lock-mode t)
;; Mode line: we want `buffer-name` + `default-directory`...
(setq-default mode-line-buffer-identification
              '("%b:" default-directory))
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;;; Editing
(set-fringe-mode '(0 . 0))
(setq-default show-trailing-whitespace t)
;; M-x delete-trailing-whitespace
(set-face-background 'trailing-whitespace "red")
(setq default-buffer-file-coding-system 'utf-8)

;;; Emacs setup under windows
(setq frame-title-format (concat "%b | " default-directory))
; (set-default-font "Monaco-12")
; (set-face-attribute 'default nil :height 120)
(set-default-font "Consolas-13")
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset
                    (font-spec :family "SimSun" :size 16)))
(set-background-color "dark slate gray")
; (set-background-color "black")
(set-foreground-color "grey")
; (set-foreground-color "wheat")
(set-cursor-color "white")
(set-face-background 'region "blue")
(setq w32-get-true-file-attributes nil)

;; Maximize Emacs
(if (eq system-type 'windows-nt)
    (w32-send-sys-command 61488))

(setq x-select-enable-clipboard nil)
(defconst pc-behaviour-level 1)
(setq mouse-drag-copy-region t)

;;;; 3) Emacs plugins
(setq load-path (append (list *plugin-path*) load-path))

(require 'sr-speedbar)
(setq sr-speedbar-right-side nil)
(setq sr-speedbar-skip-other-window-p t)
(setq sr-speedbar-auto-refresh t)
(setq sr-speedbar-max-width 30)
(setq speedbar-show-unknown-files t)
(setq speedbar-tag-hierarchy-method '(speedbar-prefix-group-tag-hierarchy))
(setq speedbar-sort-tags t)

;;;; 4) Org-mode
(setq org-startup-indented t)

(add-hook 'org-mode-hook
          (lambda ()
            (setq system-time-locale "C")
            (setq truncate-lines nil)
            (setq org-log-done t)))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

(setq org-agenda-files
      (list (concat *org-path* "work.org")
            (concat *org-path* "home.org")))

;;;; 5) Dirty python scripting under windows
;;; <C-c C-z>: python-shell-switch-to-shell
;;; <M-x> comment-region
(setq python-shell-interpreter *python*
      python-shell-interpreter-args *ipython-args*
      python-shell-prompt-regexp "In \\[[0-9]+\\]: "
      python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
      python-shell-completion-setup-code
      "from IPython.core.completerlib import module_completion"
      python-shell-completion-module-string-code
      "';'.join(module_completion('''%s'''))\n"
      python-shell-completion-string-code
      "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
(add-hook 'inferior-python-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))
