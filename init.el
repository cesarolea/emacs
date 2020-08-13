; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode
         '(tool-bar-mode                ; No toolbars, more room for text.
           scroll-bar-mode              ; No scroll bars either.
           tool-bar-mode
           menu-bar-mode))
  (funcall mode 0))

(setq create-lockfiles nil)

; Avoid garbage collection during startup
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; Always load newest byte code
(setq load-prefer-newer t)

;; Prevent resizing frame when setting new font (adds 1s to startup)
(setq frame-inhibit-implied-resize t)

;; Prevent loading text mode at startup
(setq initial-major-mode 'fundamental-mode)

;; Welcome
(setq initial-scratch-message (concat "# Welcome " (user-login-name) "!\n# Happy Hacking...\n\n"))

(defvar doom--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

; Package.el initialization is expensive, so disable it! package.el is sneaky though,
; it will initialize itself if you’re not careful. Not on my watch, criminal scum!
(setq package-enable-at-startup nil ; don't auto-initialize!
      ;; don't add that `custom-set-variables' block to my initl!
      package--init-file-ensured t)

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
;(cond
; ((executable-find "aspell")
;  (setq-default ispell-program-name "aspell")))

; custom font size depending on resolution
(defun fontify-frame (frame)
  (interactive)
  (set-frame-parameter frame 'font "IBM Plex Mono 14"))

;; Fontify any future frames
(push 'fontify-frame after-make-frame-functions)

;; elpa.gnu.org uses TLS1.2, not TLS1.3
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; sources
(package-initialize)
; sources
(setq package-archives '(("org"          . "https://orgmode.org/elpa/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa"        . "https://melpa.org/packages/")))



; execution path so homebrew binaries work
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

(load "~/.emacs.d/emacs_init_use_package.el")

; custom init files
(load "~/.emacs.d/emacs_init_customization.el")
(load "~/.emacs.d/emacs_init_utility.el")
(load "~/.emacs.d/emacs_init_keymaps.el")

(load custom-file)
(put 'erase-buffer 'disabled nil)

(add-hook 'whitespace-mode-hook '(lambda () (diminish 'whitespace-mode)))

(put 'dired-find-alternate-file 'disabled nil)

;; Fontify current frame
(fontify-frame nil)

; Reset GC as late as possible;
(add-hook 'emacs-startup-hook
          '(lambda ()
             (setq gc-cons-threshold 50000000)
             (setq gc-cons-percentage 0.1)
             (setq file-name-handler-alist doom--file-name-handler-alist)))
