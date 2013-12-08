; execution path so homebrew binaries work
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

; so we can use thing at point
(require 'thingatpt)

; Add newlines at the end of line with C-n
(setq next-line-add-newlines t)

; blink cursor
(blink-cursor-mode t)

; force new frames into existing window
(setq ns-pop-up-frames nil)

; no bell
(setq ring-bell-function 'ignore)

;; Move to the previous window
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))

;; Erase region on insert
(delete-selection-mode +1)

; tabs and indentation
(setq standard-indent 2)
(setq-default tab-width 2)
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(setq-default c-default-style "linux");

; encoding
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

; show column number in bar
(column-number-mode t)

; highlight matching parens with smartparens
(show-smartparens-global-mode +1)

; highlight incremental search
(defconst search-highlight t)

; no newlines past EOF
(setq next-line-add-newlines nil)

; wrap lines in a tasteful way
(global-visual-line-mode 1)

; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

; pick up changes to files on disk automatically (ie, after git pull)
(global-auto-revert-mode t)

; don't confirm opening non-existant files/buffers
(setq confirm-nonexistent-file-or-buffer nil)

; don't prompt for new buffer creation in ido
(setq ido-create-new-buffer 'always)

; for commenting lines or blocks
(global-set-key "\C-c\ -" 'comment-region)
(global-set-key "\C-c\ +" 'uncomment-region)

(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

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

;; which function model
(which-function-mode 1)

;; stop telling me the menu command key
(setq suggest-key-bindings nil)

;; initial frame position
(if (window-system) (progn
					  (cond
					   ((string= system-name "Galadriel.local") (set-frame-size (selected-frame) 150 50))
					   ((string= system-name "Minas-Tirith") (set-frame-size (selected-frame) 200 65))
					   )))

;; the C style
(setq c-default-style "linux" c-basic-offset 4)
(setq-default tab-width 4 indent-tabs-mode t)

;; ido
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

;; org-mode
(require 'org-install)
(require 'org-latex)
;; so when completing tasks the timestamp is set
(setq org-log-done t)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(add-hook 'org-mode-hook (lambda ()
						   (flyspell-mode 1)
                           (linum-mode 0)
                           (electric-pair-mode 0)))

; org-export
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell 
      (replace-regexp-in-string "[[:space:]\n]*$" "" 
        (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when (equal system-type 'darwin) (set-exec-path-from-shell-PATH))

;; recentf is loaded in packages
(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))
(global-set-key (kbd "C-x C-r") 'recentf-ido-find-file)

; which buffers should have lines (all major modes for programming)
(add-hook 'prog-mode-hook (lambda ()
                            (linum-mode 1)
							(flycheck-mode 1)
							(electric-pair-mode 1)
							(rainbow-mode 1)))

; default major mode is text mode instead of fundamental mode
(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook (lambda ()
								   (electric-pair-mode 0)
								   (linum-mode 0)
								   (flycheck-mode 0)))

; hippie expand
(global-set-key (kbd "M-/") 'hippie-expand)
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

(require 'hippie-expand-slime)
(add-hook 'slime-mode-hook 'set-up-slime-hippie-expand)
(add-hook 'slime-repl-mode-hook 'set-up-slime-hippie-expand)

; paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
    (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
    (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
    (add-hook 'scheme-mode-hook           #'enable-paredit-mode)

; paredit + slime, sitting on a tree...
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
; Stop SLIME's REPL from grabbing DEL,
; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
	(read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)
