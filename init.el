; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode
         '(tool-bar-mode                ; No toolbars, more room for text.
           scroll-bar-mode              ; No scroll bars either.
           tool-bar-mode))
  (funcall mode 0))

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

; default font
(set-default-font "Inconsolata-13")

; sources
(require 'package)
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("melpa"     . "http://melpa.org/packages/")
                         ("tromey"    . "http://tromey.com/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(clj-refactor . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(cljr-helm . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(ac-cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(projectile . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(hydra . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(anzu . "melpa-stable") t)

(package-initialize)

(load "~/.emacs.d/emacs_init_use_package.el")

; custom init files
(load "~/.emacs.d/emacs_init_customization.el")
(load "~/.emacs.d/emacs_init_utility.el")
(load "~/.emacs.d/emacs_init_keymaps.el")

;; load files depending on hostname
(if (string= (downcase (substring system-name 0 9)) "galadriel")
    (load "~/.emacs.d/emacs_init_galadriel.local.el")
  (load "~/.emacs.d/emacs_init_minas.tirith.el"))

(load custom-file)
(put 'erase-buffer 'disabled nil)

; Set exec path from shell variables
(exec-path-from-shell-initialize)

(add-hook 'whitespace-mode-hook '(lambda () (diminish 'whitespace-mode)))

(set-fontset-font
 t 'symbol
 (font-spec :family "Apple Color Emoji") nil 'prepend)

(defun recompile-init ()
  "Recompile init files"
  (interactive)
  (byte-recompile-directory (expand-file-name "~/.emacs.d") 0))
