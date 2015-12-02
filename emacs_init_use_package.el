(eval-when-compile
  (require 'use-package))

(require 'diminish)
(require 'bind-key)

(diminish 'visual-line-mode)
(diminish 'abbrev-mode)

(use-package powerline :ensure t)
(use-package moe-theme :ensure t
  :config (progn
            (load-theme 'moe-dark t)
            (powerline-moe-theme)
            (moe-theme-set-color 'blue)
            (setq show-paren-style 'expression)
            (setq powerline-display-buffer-size nil)))

(use-package highlight-current-line :ensure t
  :diminish highlight-current-line-minor-mode)

(use-package popwin :ensure t
  :config (progn
           (popwin-mode 1)))

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

(use-package yasnippet
  :ensure t
  :config (progn
            (setq yas-prompt-functions '(yas-ido-prompt))))

(use-package clojure-snippets :ensure t)

(use-package company :ensure t
  :config (progn
            (defvar company-mode/enable-yas t
              "Enable yasnippet for all backends.")
            
            (defun company-mode/backend-with-yas (backend)
              (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
                  backend
                (append (if (consp backend) backend (list backend))
                        '(:with company-yasnippet))))

            (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

            (add-hook 'emacs-lisp-mode-hook 'company-mode)
            (global-set-key (kbd "C-'") 'company-complete))
  :diminish company-mode)

(use-package js2-mode :ensure t
  :config (progn
           (add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))))

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

(use-package ido :ensure t
  :config (progn
            (ido-mode 1)
            (setq ido-everywhere t)
            (setq ido-use-faces t)
            (setq ido-use-filename-at-point 'guess)
            (setq ido-use-url-at-point nil)
            (setq ido-enable-flex-matching t)
            ;; restrict to current directory
            (setq ido-auto-merge-work-directories-length -1)

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

(use-package helm-flx :ensure t :config (helm-flx-mode +1))

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

            (defun helm-buffer-ace-window (buffer)
              "Use ‘ace-window’ to select a window to display BUFFER."
              (ace-select-window)
              (switch-to-buffer buffer))

            (add-to-list 'helm-type-buffer-actions
                         '("Switch to buffer in Ace window ‘C-c C-e'" . helm-buffer-ace-window)
                         :append)
            
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
            (global-set-key "\C-x\ \C-r" 'helm-recentf)
            (global-set-key (kbd "<f9>") 'helm-bookmarks)))

(use-package helm-projectile :ensure t
  :config (progn
            (defun helm-find-ace-window (file)
              "Use ‘ace-window' to select a window to display FILE."
              (ace-select-window)
              (find-file file))

            (add-to-list 'helm-find-files-actions
                         '("Find File in Ace window" . helm-find-ace-window)
                         :append)

            (defun helm-file-run-ace-window ()
              (interactive)
              (with-helm-alive-p
                (helm-exit-and-execute-action 'helm-file-ace-window)))

            ;;; For `helm-find-files'
            (define-key helm-find-files-map (kbd "C-c C-e")
              #'helm-file-run-ace-window)

            ;; For file commands in `helm-projectile'
            (define-key helm-projectile-find-file-map (kbd "C-c C-e")
              #'helm-file-run-ace-window)

            (helm-projectile-on)))

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
                                       (electric-pair-mode 1)))
                                       
            (defun set-exec-path-from-shell-PATH ()
              (let ((path-from-shell 
                     (replace-regexp-in-string "[[:space:]\n]*$" "" 
                                               (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
                (setenv "PATH" path-from-shell)
                (setq exec-path (split-string path-from-shell path-separator))))
            (when (equal system-type 'darwin) (set-exec-path-from-shell-PATH))

            (setq org-clock-persist 'history)
            (org-clock-persistence-insinuate)

            ;; when evaluating, reinsert and preserve indentation
            (setq org-src-preserve-indentation t)
            ;; preserve native color scheme for target source code
            (setq org-src-fontify-natively t)

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

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))
  :init
  (add-hook 'clojure-mode-hook (lambda () (progn
                                            (subword-mode t)
                                            (diminish 'subword-mode))))
  (add-hook 'clojure-mode-hook #'eldoc-mode)
  (diminish 'eldoc-mode))

(use-package cider :ensure t :pin melpa-stable
  :config (progn
            (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)          
            (add-hook 'cider-repl-mode-hook #'company-mode)
            (add-hook 'cider-mode-hook #'company-mode)
            (add-hook 'clojure-mode-hook #'company-mode)

            (setq nrepl-hide-special-buffers t ; hide nrepl buffers from menu
                  cider-show-error-buffer nil ; don't show error buffers
                  nrepl-log-messages t ; useful for debugging
                  cider-repl-use-clojure-font-lock t ; syntax highlighting in REPL
                  cider-prompt-save-file-on-load 'always-save ;  just always save when loading buffer
                  cider-font-lock-dynamically '(macro core function var) ; syntax highlight all namespaces
                  cider-overlays-use-font-lock t ; syntax highlight evaluation overlays
                  cider-repl-toggle-pretty-printing t)) ; REPL always pretty-prints results
)

(use-package clj-refactor
  :ensure t
  :pin melpa-stable
  :config (progn
            (defun refactor-mode-hook ()
              (clj-refactor-mode 1)
              (yas-minor-mode 1)
              (cljr-add-keybindings-with-prefix "C-c C-m")
              (diminish 'clj-refactor-mode)
              (diminish 'yas-minor-mode))
            (add-hook 'clojure-mode-hook #'refactor-mode-hook)))

(use-package ac-cider :ensure t :pin melpa-stable)

(use-package align-cljlet
  :ensure t
  :bind ("C-c C-a" . align-cljlet))

(use-package discover-my-major :ensure t
  :bind ("C-h C-m" . discover-my-major))

(use-package dash :ensure t)
(use-package exec-path-from-shell :ensure t)

(use-package magit
  :ensure t
  :bind ("<f10>" . magit-status)
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-push-always-verify nil)
  (if (bound-and-true-p magit-auto-revert-mode)
      (diminish 'magit-auto-revert-mode)))

(use-package multiple-cursors :ensure t)
(use-package move-text :ensure t)

(use-package hydra :ensure t :pin melpa-stable
  :config (load "~/.emacs.d/hydras.el"))

(use-package gist :ensure t :pin melpa-stable)

(use-package markdown-mode :ensure t
  :config (progn
            (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
            (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))))

(use-package clean-aindent-mode :ensure t
  :config (progn
            (clean-aindent-mode 1)
            (setq clean-aindent-is-simple-indent t)
            (define-key global-map (kbd "RET") 'newline-and-indent)))

(use-package reveal-in-osx-finder :ensure t
  :bind ("C-c C-f" . reveal-in-osx-finder))

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
            (add-hook 'after-init-hook 'bm-repository-load)
            (add-hook 'find-file-hooks 'bm-buffer-restore)
            (add-hook 'kill-buffer-hook 'bm-buffer-save)
            (add-hook 'kill-emacs-hook '(lambda nil
                                          (bm-buffer-save-all)
                                          (bm-repository-save)))))

(use-package impatient-mode :ensure t
  :config (progn
            (setq httpd-port 8181)))

(use-package shrink-whitespace
  :ensure t
  :bind ("M-SPC" . shrink-whitespace))

(use-package calfw
  :ensure t
  :config (progn
            (require 'calfw-org)
            (require 'calfw-ical)

            (defun my-calendar ()
              (interactive)
              (cfw:open-calendar-buffer
               :contents-sources
               (list
                (cfw:org-create-source "green3")
                (cfw:ical-create-source "gcal" "https://www.google.com/calendar/ical/cesarolea%40gmail.com/private-608dbfd9a769792574aeab0fab06af1f/basic.ics" "IndianRed"))))))

(use-package diff-hl
  :ensure t
  :config (progn
            (add-hook 'prog-mode-hook (lambda ()
                                        (diff-hl-mode 1)))))

(use-package company-emoji
  :ensure t
  :config (progn
            ;; enable in org mode buffers
            (add-hook 'org-mode-hook 'company-mode)
            (add-hook 'org-mode-hook 'company-emoji-init)
            ;; enable in git commit log buffers
            (add-hook 'git-commit-mode-hook 'company-mode)
            (add-hook 'git-commit-mode-hook 'company-emoji-init)))

(use-package beacon
  :ensure t
  :config (progn
            (beacon-mode 1)
            (setq beacon-push-mark 35)
            (setq beacon-color "#666600"))
  :diminish beacon-mode)

(use-package buffer-flip
  :ensure t
  :config (progn
            (key-chord-mode t)
            (buffer-flip-mode)))

(use-package fill-column-indicator
  :ensure t
  :pin melpa-stable
  :config (progn
            (setq fci-rule-column 79)))
