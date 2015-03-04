(require 'use-package)

(use-package powerline :ensure t)
(use-package moe-theme :ensure t
  :init (powerline-moe-theme))

(use-package git-gutter :ensure t
  :idle (progn
	  (global-git-gutter-mode t)
	  (git-gutter:linum-setup)))

(use-package recentf :ensure t
  :init (progn
	  (recentf-mode 1)
	  (setq recentf-max-menu-items 25))
  :bind ("\C-x\ \C-r" . recentf-open-files))

(use-package saveplace :ensure t
  :init (progn
	  (setq save-place-file (concat user-emacs-directory "saveplace.el"))
	  (setq-default save-place t)))

(use-package hippie-expand-slime :ensure t
  :init (progn
	  (add-hook 'slime-mode-hook 'set-up-slime-hippie-expand)
	  (add-hook 'slime-repl-mode-hook 'set-up-slime-hippie-expand)))

(use-package yasnippet :ensure t
  :init (progn
	  (setq yas-snippet-dirs
		'("~/.emacs.d/snippets"))
	  (yas-global-mode 1)
	  
	  (defun company-yasnippet-or-completion ()
	    (interactive)
	    (if (yas/expansion-at-point)
		(progn (company-abort)
		       (yas/expand))
	      (company-complete-common)))
	  
	  (defun yas/expansion-at-point ()
	    "Tested with v0.6.1. Extracted from `yas/expand-1'"
	    (yas--templates-for-key-at-point))))

(use-package company :ensure t
  :idle (progn
	  (add-to-list 'company-backends 'company-yasnippet t)
	  (define-key company-active-map (kbd "TAB") 'company-yasnippet-or-completion)
	  (define-key company-active-map [tab] 'company-yasnippet-or-completion)
	  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)))

(use-package js2-mode :ensure t
  :idle (progn
	  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
	  (add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))))

(use-package js2-refactor :ensure t
  :idle (js2r-add-keybindings-with-prefix "C-c C-r"))

(use-package js-comint :ensure t
  :idle (cond ((eq system-type 'darwin)
	       (progn
		 (setq inferior-js-program-command "/usr/local/bin/node -i")
		 (add-hook 'inferior-js-mode-hook
			   (lambda ()
			     ;; We like nice colors
			     (ansi-color-for-comint-mode-on)
			     ;; Deal with some prompt nonsense
			     (add-to-list
			      'comint-preoutput-filter-functions
			      (lambda (output)
				(replace-regexp-in-string "\033\\[[0-9]+[GKJ]" "" output)))))))))

(use-package smex :ensure t
  :init (progn
	  (smex-initialize)
	  (global-set-key (kbd "M-x") 'smex)
	  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
	  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
	  (setf smex-key-advice-ignore-menu-bar 1)))

(use-package ido :ensure t
  :init (progn
	  (ido-mode 1)
	  (setq ido-everywhere t)
	  (setq ido-use-faces t)

	  (defun recentf-ido-find-file ()
	    "Find a recent file using ido."
	    (interactive)
	    (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
	      (when file
		(find-file file))))
	  
	  (global-set-key (kbd "C-x C-r") 'recentf-ido-find-file)))

(use-package ido-vertical-mode :ensure t
  :init (progn
	  (ido-mode 1)
	  (ido-vertical-mode 1)))

(use-package flx-ido :ensure t
  :init (progn
	  (flx-ido-mode 1)
	  (setq ido-enable-flex-matching t)
	  (setq ido-use-faces nil)))

(use-package projectile :ensure t :pin melpa-stable)

(use-package helm :ensure t
  :init (progn
	  (require 'helm-files)
	  (setq helm-idle-delay 0.1)
	  (setq helm-input-idle-delay 0.1)
	  (setq helm-c-locate-command "locate-with-mdfind %.0s %s")
	  (setq helm-for-files-preferred-list
		'(helm-source-buffers-list
		  helm-source-recentf
		  helm-source-bookmarks
		  helm-source-file-cache
		  helm-source-files-in-current-dir
		  helm-source-mac-spotlight))
	  (global-set-key "\C-x\ a" 'helm-for-files)
	  (global-set-key (kbd "C-c y") 'helm-show-kill-ring)
    ;; replace M-x with helm's version
    (global-set-key (kbd "M-x") 'helm-M-x)
    ;; find files with helm
    (global-set-key (kbd "C-x C-f") 'helm-find-files)
    ;; replace C-x b with helm's version
    (global-set-key "\C-x\ b" 'helm-mini)))

(use-package helm-projectile :ensure t
  :disabled t
  :init (helm-projectile-on))

(use-package flycheck :ensure t
  :init (progn
	  (add-hook 'after-init-hook #'global-flycheck-mode)
	  (provide 'emacs_init_packages)))

(use-package flyspell :ensure t
  :bind ("C-c C-SPC" . ispell-word))

(use-package highlight-symbol :ensure t
  :init (progn
	  (global-set-key (kbd "<f13>") 'highlight-symbol-at-point)
	  (global-set-key (kbd "<f14>") 'highlight-symbol-prev)
	  (global-set-key (kbd "<f15>") 'highlight-symbol-next)
	  (global-set-key (kbd "<f16>") 'highlight-symbol-query-replace)))

(use-package highlight-current-line :ensure t)
(use-package auto-highlight-symbol :ensure t
  :init (progn
	  (add-hook 'prog-mode-hook (lambda ()
				      (highlight-current-line-minor-mode t)
				      (auto-highlight-symbol-mode t)
				      (flyspell-prog-mode)))))

(use-package smartparens :ensure t :pin melpa-stable
  :init (show-smartparens-global-mode +1))

(use-package eyebrowse :ensure t
  :init (progn
          ;(setq eyebrowse-keymap-prefix (kbd "H-w"))
          (eyebrowse-mode t)))

(use-package undo-tree :ensure t
  :init (progn
	  (global-undo-tree-mode 1)
	  (defalias 'redo 'undo-tree-redo)
	  (global-set-key (kbd "s-z") 'undo)
	  (global-set-key (kbd "M-z") 'redo)))

(use-package web-mode :ensure t
  :init (progn
	  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
	  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
	  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
	  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
	  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
	  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
	  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
	  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

	  (setq web-mode-markup-indent-offset 4)
	  (setq web-mode-code-indent-offset 4)))

(use-package ace-window :ensure t)
(use-package ace-jump-mode :ensure t
  :init (progn
	  (define-key global-map (kbd "C-' SPC") 'ace-jump-mode)
	  (define-key global-map (kbd "C-' C-u SPC") 'ace-jump-char-mode)
	  (define-key global-map (kbd "C-' C-u C-u SPC") 'ace-jump-line-mode)
	  (define-key global-map (kbd "M-'") 'ace-window)
	  (define-key global-map (kbd "C-x o") nil)))

(use-package rainbow-mode :ensure t)
(use-package rainbow-delimiters :ensure t)

(use-package popwin :ensure t
  :init (progn
	  (global-set-key (kbd "M-z") popwin:keymap)
	  (popwin-mode 1)))

(use-package expand-region :ensure t
  :bind ("C-=" . er/expand-region))

(use-package org
  :init (progn
	  (global-set-key "\C-cl" 'org-store-link)
	  (global-set-key "\C-cc" 'org-capture)
	  (global-set-key "\C-ca" 'org-agenda)
	  (global-set-key "\C-cb" 'org-iswitchb)

	  (setq org-log-done t)

	  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
	  (add-hook 'org-mode-hook (lambda ()
				     (flyspell-mode 1)
				     (linum-mode 0)
				     (electric-pair-mode 0)))

	  (defun set-exec-path-from-shell-PATH ()
	    (let ((path-from-shell 
		   (replace-regexp-in-string "[[:space:]\n]*$" "" 
					     (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
	      (setenv "PATH" path-from-shell)
	      (setq exec-path (split-string path-from-shell path-separator))))
	  (when (equal system-type 'darwin) (set-exec-path-from-shell-PATH))


	  (setq org-default-notes-file "~/Sync/Org/refile.org")
	  (setq org-capture-templates
		(quote (("t" "todo" entry (file "~/Sync/Org/refile.org")
			 "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
			("n" "note" entry (file "~/Sync/Org/refile.org")
			 "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t))))))

(use-package paredit :ensure t
  :init (progn
	  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
	  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
	  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
	  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
	  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
	  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
	  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
	  (add-hook 'clojure-mode-hook          #'enable-paredit-mode)
	  (add-hook 'cider-repl-mode-hook       #'enable-paredit-mode)

	  (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))

	  (defun override-slime-repl-bindings-with-paredit ()
	    (define-key slime-repl-mode-map
	      (read-kbd-macro paredit-backward-delete-key) nil))
	  (add-hook 'slime-repl-mode-hook
              'override-slime-repl-bindings-with-paredit t)))

(use-package cider :ensure t :pin melpa-stable
  :init (progn
	  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
	  (add-hook 'cider-mode-hook (lambda () (popwin-mode nil)))
    (add-hook 'cider-repl-mode-hook #'company-mode)
    (add-hook 'cider-mode-hook #'company-mode)
    (add-hook 'clojure-mode-hook #'company-mode)

	  (setq nrepl-hide-special-buffers t)
	  (setq cider-show-error-buffer nil)))

(use-package ac-cider :ensure t :pin melpa-stable)

(use-package discover-my-major :ensure t
  :bind ("C-h C-m" . discover-my-major))

(use-package dash :ensure t)
(use-package exec-path-from-shell :ensure t)
(use-package magit :ensure t)
(use-package multiple-cursors :ensure t)
(use-package move-text :ensure t)
