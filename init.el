; Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

; I don't like Customize customizing my init.el
(setq custom-file "~/.emacs.d/lisp/custom.el")

; don't display startup message
(setq inhibit-startup-message t)

; use srgb
(setq ns-use-srgb-colorspace t)

; use a different ispell
(setq-default ispell-program-name "aspell")

; Turn off mouse interface early in startup to avoid momentary display
;; You really don't need these; trust me.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

; default font
(set-default-font "Inconsolata-13")

; sbcl
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/local/bin/sbcl")

; sources
(require 'package)
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ;("melpa"     . "http://melpa.milkbox.net/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
                         ("tromey"    . "http://tromey.com/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

; cask
(require 'cask "/usr/local/Cellar/cask/0.7.0/cask.el")
(cask-initialize)

; custom init files
(load "~/.emacs.d/emacs_init_packages.el")
(load "~/.emacs.d/emacs_init_customization.el")
(load "~/.emacs.d/emacs_init_ecb.el")
(load "~/.emacs.d/emacs_init_utility.el")
(load "~/.emacs.d/emacs_init_keymaps.el")

;; load files depending on hostname
(cond ((string= system-name "Galadriel.local") (load "~/.emacs.d/emacs_init_galadriel.local.el"))
	  (t (load "~/.emacs.d/emacs_init_minas.tirith.el")))

(load custom-file)
(put 'erase-buffer 'disabled nil)
