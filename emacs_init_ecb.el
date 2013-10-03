;;; emacs_init_packages.el --- Summary

;;; Commentary:

;;; Code:

;;; init ecb
(require 'ecb)
(require 'ecb-autoloads)

;;; the layout to use
(setq ecb-layout-name "left8")

;;; disable the tip of the day
(setq ecb-tip-of-the-day nil)

;;; show source files in directories buffer
(setq ecb-show-sources-in-directories-buffer 'always)

;;; keep a consistent compile window
; (setq ecb-compile-window-height 1)

;;; quick navigation between ecb windows
(global-set-key (kbd "C-c C-c") 'ecb-goto-window-edit1)
(global-set-key (kbd "C-c C-1") 'ecb-goto-window-directories)
(global-set-key (kbd "C-c C-2") 'ecb-goto-window-sources)
(global-set-key (kbd "C-c C-3") 'ecb-goto-window-methods)
(global-set-key (kbd "C-c C-4") 'ecb-goto-window-history)
(global-set-key (kbd "C-c C-0") 'ecb-goto-window-compilation)

;;; emacs_init_ecb.el ends here
