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
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-safe-themes (quote ("7df5b36ef661649550614a15e9afb9d3e785706be6a577058f1b440dff1b03e3" "e58e9052d5380946ef7397f0248563ccd69a467362f513765d4afd2c2f5aafe6" "94d66281c0398118afd3fdb921d8b813401a36748ce4541e7ad6b1533a557a9f" "427234e4b45350b4159575f1ac72860c32dce79bb57a29a196b9cfb9dd3554d9" default)))
 '(ecb-layout-window-sizes (quote (("left8" (ecb-directories-buffer-name 0.13 . 0.2923076923076923) (ecb-sources-buffer-name 0.13 . 0.23076923076923078) (ecb-methods-buffer-name 0.13 . 0.2923076923076923) (ecb-history-buffer-name 0.13 . 0.16923076923076924)))))
 '(ecb-options-version "2.40"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
