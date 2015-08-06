(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(diminish 'visual-line-mode)
(diminish 'abbrev-mode)

(use-package nlinum :ensure t
  :config (progn
            (add-hook 'prog-mode-hook 'nlinum-mode)))

(use-package powerline :ensure t)
(use-package moe-theme :ensure t
  :config (progn
            (powerline-moe-theme)
            (moe-theme-set-color 'blue)
            (setq show-paren-style 'expression)
            (setq powerline-display-buffer-size nil)))

(use-package popwin :ensure t
  :config (progn
           (popwin-mode 1)))

(use-package git-gutter :ensure t
  :config (progn
           (global-git-gutter-mode t)
           (git-gutter:linum-setup))
  :diminish git-gutter-mode)

(use-package recentf :ensure t
  :config (progn
            (recentf-mode 1)
            (setq recentf-max-menu-items 25))
  :bind ("\C-x\ \C-r" . recentf-open-files))

(use-package saveplace :ensure t
  :init (progn
          (setq save-place-file (concat user-emacs-directory "saveplace.el"))
          (setq-default save-place t)))

(use-package hippie-expand-slime :ensure t
  :config (progn
            (add-hook 'slime-mode-hook 'set-up-slime-hippie-expand)
            (add-hook 'slime-repl-mode-hook 'set-up-slime-hippie-expand)))

(use-package company :ensure t
  :config (progn
           (add-to-list 'company-backends 'company-yasnippet t)
           (define-key company-active-map (kbd "TAB") 'company-yasnippet-or-completion)
           (define-key company-active-map [tab] 'company-yasnippet-or-completion)
           (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand))
  :diminish company-mode)

(use-package js2-mode :ensure t
  :config (progn
           (add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
           ))

(use-package js-comint :ensure t
  :config (cond ((eq system-type 'darwin)
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
  :config (progn
            (smex-initialize)
            (global-set-key (kbd "M-x") 'smex)
            (global-set-key (kbd "M-X") 'smex-major-mode-commands)
            (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
            (setf smex-key-advice-ignore-menu-bar 1)))

(use-package ido :ensure t
  :config (progn
            (ido-mode 1)
            (setq ido-everywhere t)
            (setq ido-use-faces t)
            (setq ido-use-filename-at-point 'guess)
            (setq ido-use-url-at-point nil)

            (defun recentf-ido-find-file ()
              "Find a recent file using ido."
              (interactive)
              (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
                (when file
                  (find-file file))))
            
            (global-set-key (kbd "C-x C-r") 'recentf-ido-find-file)))

(use-package ido-vertical-mode :ensure t
  :config (progn
            (ido-mode 1)
            (ido-vertical-mode 1)))

(use-package flx-ido :ensure t
  :config (progn
            (flx-ido-mode 1)
            (setq ido-enable-flex-matching t)
            (setq ido-use-faces nil)))

(use-package projectile :ensure t
  :config (progn
            (setq projectile-require-project-root nil)
            (setq projectile-mode-line '(:eval (format " P[%s]" (projectile-project-name))))
            (projectile-global-mode t)))

(use-package helm :ensure t
  :config (progn
            (require 'helm-files)

            ;; so helm adapts to your usage
            (helm-adaptive-mode 1)

            ;; window management
            (push '("^\*helm.+\*$" :regexp t) popwin:special-display-config)
            (add-hook 'helm-after-initialize-hook (lambda ()
                                                    (popwin:display-buffer helm-buffer t)
                                                    (popwin-mode -1)))

            ;;  Restore popwin-mode after a Helm session finishes.
            (add-hook 'helm-cleanup-hook (lambda () (popwin-mode 1)))
            
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
            ;; disabling for now, using ido for now
            ;; (global-set-key (kbd "C-x C-f") 'helm-find-files)
            ;; replace C-x b with helm's version
            (global-set-key "\C-x\ b" 'helm-mini)
            (global-set-key "\C-x\ \C-r" 'helm-recentf)))

(use-package helm-projectile :ensure t
  :config (helm-projectile-on))

(use-package flycheck :ensure t
  :config (progn
            (add-hook 'after-init-hook #'global-flycheck-mode)
            (provide 'emacs_init_packages)))

(use-package flyspell :ensure t
  :bind ("C-c C-SPC" . ispell-word)
  :diminish flyspell-mode)

(use-package highlight-symbol :ensure t
  :config (progn
            (global-set-key (kbd "<f13>") 'highlight-symbol-at-point)
            (global-set-key (kbd "<f14>") 'highlight-symbol-prev)
            (global-set-key (kbd "<f15>") 'highlight-symbol-next)
            (global-set-key (kbd "<f16>") 'highlight-symbol-query-replace)))

(use-package highlight-current-line :ensure t
  :diminish highlight-current-line-minor-mode)

(use-package auto-highlight-symbol :ensure t
  :config (progn
            (add-hook 'prog-mode-hook (lambda ()
                                        (highlight-current-line-minor-mode t)
                                        (auto-highlight-symbol-mode t)
                                        (flyspell-prog-mode))))
  :diminish auto-highlight-symbol-mode)

(use-package smartparens :ensure t :pin melpa-stable
  :config (show-smartparens-global-mode +1))

(use-package eyebrowse :ensure t
  :init (progn
          (setq eyebrowse-wrap-around t
                eyebrowse-new-workspace t)
          (eyebrowse-mode 1)
          (eyebrowse-switch-to-window-config-0))
  :diminish eyebrowse-mode)

(use-package undo-tree :ensure t
  :config (progn
            (global-undo-tree-mode 1)
            (defalias 'redo 'undo-tree-redo)
            (global-set-key (kbd "s-z") 'undo)
            (global-set-key (kbd "M-z") 'redo))
  :diminish undo-tree-mode)

(use-package web-mode :ensure t
  :config (progn
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

(use-package ace-window :ensure t
  :init (progn
          (define-key global-map (kbd "M-'") 'ace-window)
          (define-key global-map (kbd "C-M-'") 'aw-flip-window)
          (define-key global-map (kbd "C-x o") nil)))

(use-package rainbow-mode :ensure t
  :diminish rainbow-mode)

(use-package rainbow-delimiters :ensure t)

(use-package expand-region :ensure t
  :config (progn
            (global-set-key (kbd "C-=") 'er/expand-region)
            (global-set-key (kbd "C-M-=") 'er/contract-region)))

(use-package org
  :config (progn
            (global-set-key "\C-cl" 'org-store-link)
            (global-set-key "\C-cc" 'org-capture)
            (global-set-key "\C-ca" 'org-agenda)
            (global-set-key "\C-cb" 'org-iswitchb)

            (setq org-log-done t)

            (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
            (add-hook 'org-mode-hook (lambda ()
                                       (flyspell-mode 1)
                                       (nlinum-mode 0)
                                       (electric-pair-mode 0)))

            (defun set-exec-path-from-shell-PATH ()
              (let ((path-from-shell 
                     (replace-regexp-in-string "[[:space:]\n]*$" "" 
                                               (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
                (setenv "PATH" path-from-shell)
                (setq exec-path (split-string path-from-shell path-separator))))
            (when (equal system-type 'darwin) (set-exec-path-from-shell-PATH))

            (setq org-clock-persist 'history)
            (org-clock-persistence-insinuate)

            (define-key org-mode-map (kbd "s-u") #'org-goto)
            (define-key org-mode-map (kbd "s-U") #'org-mark-ring-goto)))

(use-package paredit :ensure t
  :config (progn
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
                      'override-slime-repl-bindings-with-paredit t))
  :diminish paredit-mode)

(use-package cider :ensure t :pin melpa-stable
  :config (progn
            (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)          
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
(use-package magit :ensure t
  :config (progn
            (setq magit-last-seen-setup-instructions "1.4.0")
            (global-set-key (kbd "<f10>") 'magit-status)
            (add-to-list 'magit-no-confirm 'stage-all-changes)
            (setq magit-push-always-verify nil)
            (if (bound-and-true-p magit-auto-revert-mode)
                (diminish 'magit-auto-revert-mode))))

(use-package multiple-cursors :ensure t)
(use-package move-text :ensure t)

(use-package hydra :ensure t :pin melpa-stable
  :config (load "~/.emacs.d/hydras.el"))

(use-package gist :ensure t :pin melpa-stable)

(use-package markdown-mode :ensure t
  :config (progn
            (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
            (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))))

;; (use-package org-bullets :ensure t
;;   :config (progn
;;             (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))))

(use-package clean-aindent-mode :ensure t
  :config (progn
            (clean-aindent-mode 1)
            (setq clean-aindent-is-simple-indent t)
            (define-key global-map (kbd "RET") 'newline-and-indent)))

(use-package reveal-in-finder :ensure t
  :bind ("C-c C-f" . reveal-in-finder))

(use-package anzu :ensure t :pin melpa-stable
  :config (progn
            (global-anzu-mode)
            (set-face-attribute 'anzu-mode-line nil
                                :foreground "white" :weight 'bold))
  :bind ("M-%" . anzu-query-replace)
  :diminish anzu-mode)

(use-package bm :ensure t
  :config (progn
            (define-fringe-bitmap 'bm-marker-left [#xF8
                                                   #xFC
                                                   #xFE
                                                   #x0F
                                                   #x0F
                                                   #xFE
                                                   #xFC
                                                   #xF8])
            (setq bm-highlight-style 'bm-highlight-only-fringe)
            (setq-default bm-buffer-persistence t)
            (add-hook' after-init-hook 'bm-repository-load)
            (add-hook 'find-file-hooks 'bm-buffer-restore)
            (add-hook 'kill-buffer-hook 'bm-buffer-save)
            (add-hook 'kill-emacs-hook '(lambda nil
                                          (bm-buffer-save-all)
                                          (bm-repository-save)))))

(use-package impatient-mode :ensure t
  :config (progn
            (setq httpd-port 8181)))
