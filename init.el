; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode
         '(tool-bar-mode                ; No toolbars, more room for text.
           scroll-bar-mode              ; No scroll bars either.
           tool-bar-mode
           menu-bar-mode))
  (funcall mode 0))

(setq create-lockfiles nil                    ; disable the creation of lockfiles
      auto-save-default nil                   ; don't create autosave files
      load-prefer-newer t                     ; always load newest bytecode
      initial-major-mode 'fundamental-mode    ; prevent loading text mode at startup
      initial-scratch-message (concat "# Welcome " (user-login-name) "!\n# Happy Hacking...\n\n") ; welcome
      mac-command-modifier 'super             ; OSX keybindings
      ns-function-modifier 'hyper
      mac-option-key-is-meta t
      mac-right-option-modifier nil
      custom-file "~/.emacs.d/lisp/custom.el" ; avoid adding to init.el
      inhibit-startup-message t               ; don't display startup message
      ns-use-srgb-colorspace t                ; use srgb
      gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3" ; elpa.gnu.org uses TLS1.2, not TLS1.3
      package-archives '(("org"          . "https://orgmode.org/elpa/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa"        . "https://melpa.org/packages/"))
      exec-path (append exec-path '("/usr/local/bin"))
      confirm-kill-processes nil              ; don't prompt for killing processes
      ns-use-proxy-icon nil
      recenter-positions '(top middle bottom) ; enable saveplace
      next-line-add-newlines t                ; add newlines at the end of line with C-n
      ns-pop-up-frames nil                    ; force new frames into existing window
      ring-bell-function 'ignore              ; no bell
      standard-indent 2
      next-line-add-newlines nil              ; no newlines past EOF
      confirm-nonexistent-file-or-buffer nil  ; no confirm opening non-existant files/buffers
      large-file-warning-threshold 100000000  ; warn on opening files bigger than 100MB
      ido-create-new-buffer 'always           ; no prompt for new buffer creation in ido
      kill-buffer-query-functions (remq 'process-kill-buffer-query-function
					kill-buffer-query-functions)
      doc-view-continuous t                   ; scroll PDFs with the mouse wheel
      doc-view-resolution 300                 ; so PDFs don't hurt my eyes
      suggest-key-bindings nil                ; stop telling me the menu command key
      display-line-numbers-grow-only t
      display-line-numbers-type "relative"
      whitespace-line-column 100              ; limit line length
      whitespace-style '(face tabs empty trailing lines-tail tab-mark)
      whitespace-display-mappings '((trailing-mark 32 [183] [46])
                                    (newline-mark 10 [182 10])
                                    (tab-mark 9 [9655 9] [92 9]))
      default-major-mode 'text-mode
      x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      dired-listing-switches "-alh"
      search-default-mode #'char-fold-to-regexp
      gc-cons-threshold (* 100 1024 1024)
      jit-lock-defer-time nil
      jit-lock-stealth-nice 0.1
      jit-lock-stealth-time 0.2
      jit-lock-stealth-verbose nil
      auto-window-vscroll nil
      scroll-margin 10
      scroll-conservatively 100000
      scroll-preserve-screen-position 1
      frame-title-format '((:eval (if (buffer-file-name)
				      (abbreviate-file-name (buffer-file-name))
				    "%b")))
      require-final-newline nil
      dired-recursive-deletes 'always
      dired-recursive-copies 'always
      dired-dwim-target t
      )

; themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

; default spell
(setq-default ispell-program-name "aspell")

; custom font size depending on resolution
; doesn't work for emacs daemon instances
(defun fontify-frame (frame)
  (interactive)
  (set-frame-parameter frame 'font "IBM Plex Mono 14"))

; Fontify any future frames
(push 'fontify-frame after-make-frame-functions)

; sources
(package-initialize)

; execution path so homebrew binaries work
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))

; fontify current frame
(fontify-frame nil)

(load "~/.emacs.d/packages.el")
(load "~/.emacs.d/customization.el")
(load "~/.emacs.d/utility.el")
(load "~/.emacs.d/keymaps.el")
