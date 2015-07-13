;: OSX keybindings
(setq mac-command-modifier 'super)
(setq ns-function-modifier 'hyper)
(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

; Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

; I don't like Customize customizing my init.el
(setq custom-file "~/.emacs.d/lisp/custom.el")

; don't display startup message
(setq inhibit-startup-message t)

; use srgb
(setq ns-use-srgb-colorspace t)

; use a different ispell
(setq-default ispell-program-name "/usr/local/bin/aspell")

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
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("melpa"     . "http://melpa.org/packages/")
                         ("tromey"    . "http://tromey.com/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(ac-cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(projectile . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(powerline . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(hydra . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(anzu . "melpa-stable") t)
;(add-to-list 'package-pinned-packages '(helm . "melpa-stable") t)

(package-initialize)

(load "~/.emacs.d/emacs_init_use_package.el")

; custom init files
(load "~/.emacs.d/emacs_init_customization.el")
(load "~/.emacs.d/emacs_init_utility.el")
(load "~/.emacs.d/emacs_init_keymaps.el")

;; load files depending on hostname
(cond ((string= (substring system-name 0 9) "Galadriel")
       (load "~/.emacs.d/emacs_init_galadriel.local.el"))
      (t (load "~/.emacs.d/emacs_init_minas.tirith.el")))

(load custom-file)
(put 'erase-buffer 'disabled nil)

; Set exec path from shell variables
(exec-path-from-shell-initialize)
(moe-theme-random-color)
