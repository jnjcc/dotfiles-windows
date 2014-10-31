;;;; 1) Variables
(defvar *use-home* (concat (expand-file-name "~") "/"))
(defvar *plugin-path* (concat *use-home* ".emacs.d/plugins/"))

(defvar *ess-path* (concat *use-home* ".emacs.d/ess/lisp/"))
;;; R
(defvar *r-start-args* "--quiet --no-restore-history --no-save ")
(defvar *r-bin* "/path/to/Rterm")

;;; getwd() / setwd()
(setq default-directory "/path/to/R/workspace/")

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

;; Mode line: display time
; (display-time-mode t)
; (setq display-time-24hr-format t)
; (setq display-time-day-and-date t)

;;; Editing
(setq-default show-trailing-whitespace t)
;; M-x delete-trailing-whitespace
(set-face-background 'trailing-whitespace "red")
(setq default-buffer-file-coding-system 'utf-8)

;;; Emacs setup under windows
(setq frame-title-format (concat "%b | " default-directory))
(set-default-font "Monaco-12")
; (set-face-attribute 'default nil :height 120)
(set-background-color "dark slate gray")
; (set-background-color "black")
(set-foreground-color "wheat")
(set-cursor-color "white")
(set-face-background 'region "blue")

;; Maximize Emacs
(if (eq system-type 'windows-nt)
    (w32-send-sys-command 61488))

(setq x-select-enable-clipboard nil)
(defconst pc-behaviour-level 1)
(setq mouse-drag-copy-region t)

;;;; 3) Emacs plugins
;; linum
; (global-linum-mode 1)

(setq load-path (append (list *plugin-path*) load-path))

;; M-x visit-tags-table; M-x taglist
;; cedet & ecb does this better, but their size really freaks me out
(require 'taglist)

(require 'sr-speedbar)
(setq sr-speedbar-right-side nil)
(setq sr-speedbar-skip-other-window-p t)
(setq sr-speedbar-auto-refresh t)
(setq sr-speedbar-max-width 30)
(setq speedbar-show-unknown-files t)
(setq speedbar-tag-hierarchy-method '(speedbar-prefix-group-tag-hierarchy))
(setq speedbar-sort-tags t)

;;;; 4) R, ESS, etc
(setq auto-mode-alist
      (append '(("\\.R$" . R-mode))
              auto-mode-alist))

;;; ESS
(setq load-path (append (list *ess-path*) load-path))
(setq-default inferior-R-args *r-start-args*)
(setq inferior-R-program-name *r-bin*)
(require 'ess-site)

(autoload 'R-mode "ess-site.el" "ESS" t)

(setq ess-tab-complete-in-script t)
; (setq ess-first-tab-never-complete t)
;; start R in current working directory, don't ask
(setq ess-ask-for-ess-directory nil)
(setq ess-eval-visibly-p nil)
(require 'ess-eldoc)
;; ESS converts underscores to "<-" by default
(ess-toggle-underscore nil)

;;; multiple window on startup
;;; top-left: reserved for R script editing
;;; bottom-left: R console
;;; right: reserved for R help
;; turns out we don't need the help window that much...
; (split-window-horizontally)
(split-window-vertically)
(other-window 1)
(R)
(other-window -1)
;; reload last open buffers
(desktop-save-mode 1)

;;; M-x clipboard-yank
(dolist (k '([mouse-1] [down-mouse-1] [drag-mouse-1] [double-mouse-1]
             [triple-mouse-1]
             [mouse-2] [down-mouse-2] [drag-mouse-2] [double-mouse-2]
             [triple-mouse-2]
             [mouse-3] [down-mouse-3] [drag-mouse-3] [double-mouse-3]
             [triple-mouse-3]))
  (global-unset-key k))
