(global-set-key "\M-/" 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
	try-complete-file-name-partially
	try-complete-file-name
	try-expand-all-abbrevs
	try-expand-list
	try-expand-line
	try-complete-lisp-symbol-partially
	try-complete-lisp-symbol))

; execution path so homebrew binaries work
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))

;; so we can use thing at point
;(require 'thingatpt)

;; Add newlines at the end of line with C-n
(setq next-line-add-newlines t)

;; stop blinking cursor
(blink-cursor-mode 0)

;; force new frames into existing window
(setq ns-pop-up-frames nil)

;; no bell
(setq ring-bell-function 'ignore)

;; Move to the previous window
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))

;; Erase region on insert
(delete-selection-mode 1)
(global-set-key (kbd "C-c d") 'c-hungry-delete-forward)

;; tabs and indentation
(setq standard-indent 2)
(setq-default tab-width 2)
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(setq-default c-default-style "linux");

;; encoding
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; show column number in bar
(column-number-mode t)

;; highlight incremental search
(defconst search-highlight t)

;; no newlines past EOF
(setq next-line-add-newlines nil)

;; wrap lines in a tasteful way
(global-visual-line-mode 1)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; pick up changes to files on disk automatically (ie, after git pull)
(global-auto-revert-mode t)

;; don't confirm opening non-existant files/buffers
(setq confirm-nonexistent-file-or-buffer nil)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; don't prompt for new buffer creation in ido
(setq ido-create-new-buffer 'always)

;; for commenting lines or blocks
(global-set-key "\C-c\ -" 'comment-region)
(global-set-key "\C-c\ +" 'uncomment-region)

;; yes, I want to kill buffers with processes attached
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Do not ask about running processes when exiting.
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

;; use finder database instead of locate
(setq locate-command "mdfind")

;; Anwsering y/n is faster than yes/no.
(fset 'yes-or-no-p 'y-or-n-p)

;; scroll PDFs with the mouse wheel
(setq doc-view-continuous t)

;; stop telling me the menu command key
(setq suggest-key-bindings nil)

;; which buffers should have lines (all major modes for programming)
(add-hook 'prog-mode-hook (lambda ()
                            (linum-mode 1)
                            (flycheck-mode 1)
                            (electric-pair-mode 1)
                            (rainbow-mode 1)
                            (flyspell-mode 1)
                            (projectile-mode 1)))

;; but only lisps should have rainbow delimiters
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

;; default major mode is text mode instead of fundamental mode
(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook (lambda ()
			    (electric-pair-mode 0)
			    (linum-mode 0)
			    (flycheck-mode 0)))

;; which function
(which-function-mode)

;; move naturally between open windows
(windmove-default-keybindings)

;; move text lines or regions
(global-set-key [M-S-up] 'move-text-up)
(global-set-key [M-S-down] 'move-text-down)

;; drag and drop arrangement
(defun th/swap-window-buffers-by-dnd (drag-event)
  "Swaps the buffers displayed in the DRAG-EVENT's start and end
window."
  (interactive "e")
  (let ((start-win (cl-caadr drag-event))
        (end-win   (cl-caaddr drag-event)))
    (when (and (windowp start-win)
               (windowp end-win)
               (not (eq start-win end-win))
               (not (memq (minibuffer-window)
                          (list start-win end-win))))
      (let ((bs (window-buffer start-win))
            (be (window-buffer end-win)))
        (unless (eq bs be)
          (set-window-buffer start-win be)
          (set-window-buffer end-win bs))))))

(global-set-key (kbd "<C-S-drag-mouse-1>") #'th/swap-window-buffers-by-dnd)
