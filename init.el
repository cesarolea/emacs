; Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

; The all important load path
(add-to-list 'load-path "~/.emacs.d/")

; don't display startup message
(setq inhibit-startup-message t)

; Turn off mouse interface early in startup to avoid momentary display
;; You really don't need these; trust me.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

; default font
(set-default-font "Inconsolata-13")

; default theme
(load-theme 'wombat t)

; sbcl
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/local/bin/sbcl")

(load "~/.emacs.d/emacs_init_packages.el")
(load "~/.emacs.d/emacs_init_customization.el")
(load "~/.emacs.d/emacs_init_ecb.el")
(load "~/.emacs.d/emacs_init_utility.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-layout-window-sizes (quote (("left8" (ecb-directories-buffer-name 0.13 . 0.2923076923076923) (ecb-sources-buffer-name 0.13 . 0.23076923076923078) (ecb-methods-buffer-name 0.13 . 0.2923076923076923) (ecb-history-buffer-name 0.13 . 0.16923076923076924)))))
 '(ecb-options-version "2.40"))
