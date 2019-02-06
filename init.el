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
(setq-default ispell-program-name "/usr/local/bin/aspell")

; custom font size depending on resolution
(defun fontify-frame (frame)
  (interactive)
  (if window-system
      (progn
        (if (> (x-display-pixel-width) 5000)
            (set-frame-parameter frame 'font "IBM Plex Mono 18") ;; 4K display
          (set-frame-parameter frame 'font "IBM Plex Mono 12")))))

;; Fontify any future frames
(push 'fontify-frame after-make-frame-functions)

;; sources
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 4))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

; execution path so homebrew binaries work
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

(load "~/.emacs.d/emacs_init_straight.el")
(load "~/.emacs.d/emacs_init_use_package.el")

; custom init files
(load "~/.emacs.d/emacs_init_customization.el")
(load "~/.emacs.d/emacs_init_utility.el")
(load "~/.emacs.d/emacs_init_keymaps.el")

(load custom-file)
(put 'erase-buffer 'disabled nil)

(add-hook 'whitespace-mode-hook '(lambda () (diminish 'whitespace-mode)))

(set-fontset-font
 t 'symbol
 (font-spec :family "Apple Color Emoji") nil 'prepend)

(put 'dired-find-alternate-file 'disabled nil)

;; Fontify current frame
(fontify-frame nil)

; Reset GC as late as possible;
(add-hook 'emacs-startup-hook
          '(lambda ()
             (setq gc-cons-threshold 50000000)
             (setq gc-cons-percentage 0.1)
             (setq file-name-handler-alist doom--file-name-handler-alist)))
