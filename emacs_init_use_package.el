(require 'package)

(setq use-package-always-pin "melpa-stable")
(setq use-package-always-ensure t)

(defconst cesaro-savefile-dir (expand-file-name "savefile" user-emacs-directory))

(use-package use-package-chords :ensure t :config (key-chord-mode 1))

(use-package diminish :ensure t)

(eval-after-load "visual-line" '(diminish 'visual-line-mode))
(eval-after-load "abbrev" '(diminish 'abbrev-mode))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))
(eval-after-load "autorevert" '(diminish 'auto-revert-mode))

(use-package use-package-ensure-system-package :ensure t)

(use-package flycheck
  :ensure t
  :hook ((prog-mode . flycheck-mode)
         (org-mode  . flycheck-mode))
  :diminish flycheck-mode)

(use-package magit
  :ensure t
  :bind ("<f10>" . magit-status)
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-push-always-verify nil)
  (setq magit-branch-read-upstream-first t)
  (if (bound-and-true-p magit-auto-revert-mode)
      (diminish 'magit-auto-revert-mode))
  :diminish magit-mode)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (defun enable-doom-modeline-icons (_frame)
    (setq doom-modeline-icon t))

  (add-hook 'after-make-frame-functions
            #'enable-doom-modeline-icons))

(use-package doom-themes :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  (global-hl-line-mode 1))

(use-package exec-path-from-shell :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package rainbow-mode :diminish rainbow-mode :ensure t :pin gnu)

(use-package popwin :config (popwin-mode 1) :ensure t)

(use-package recentf :ensure t
  :hook (after-init . recentf-mode)
  :bind ("\C-x\ \C-r" . recentf-open-files)
  :config
  (recentf-mode 1)
  (add-to-list 'recentf-exclude (format "%s/\\.emacs\\.d/elpa/.*" (getenv "HOME")))
  (add-to-list 'recentf-exclude (format "%s/\\.emacs\\.d/ido.last" (getenv "HOME")))
  (setq recentf-max-menu-items 25
        recentf-max-saved-items 20000000))

(use-package savehist :ensure t
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" cesaro-savefile-dir))
  (savehist-mode 1))

(use-package windmove :ensure t
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings))

(use-package company :ensure t
  :hook (prog-mode . company-mode)
  :config
  (setq company-tooltip-align-annotations t
        company-minimum-prefix-length 1
        company-idle-delay 0.0)
  (global-set-key (kbd "C-'") 'company-complete)
  (global-company-mode)
  :diminish company-mode)

(use-package company-lsp :ensure t)

(use-package js2-mode :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-basic-offset 2
        js-indent-level 2))

(use-package ido :ensure t
  :config
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

  (global-set-key (kbd "C-x C-r") 'recentf-ido-find-file))

(use-package ido-vertical-mode :ensure t
  :config
  (ido-mode 1)
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right))

(use-package flx-ido :ensure t
  :config
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t
        ido-use-faces t
        ido-use-filename-at-point t))

(use-package projectile :ensure t
  :init
  (custom-set-variables '(projectile-keymap-prefix (kbd "C-c p")))
  :config
  (projectile-mode t)
  (setq projectile-project-search-path '("~/workspace/")
        projectile-mode-line-function '(lambda () (format " P[%s]" (projectile-project-name)))
        projectile-indexing-method 'alien))

(use-package helm-flx :ensure t :pin melpa
  :config (helm-flx-mode +1))

(use-package helm :ensure t
  :config (progn
            ;; (require 'helm-files)

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
            (setq helm-follow-mode-persistent t)
            (setq helm-c-locate-command "locate-with-mdfind %.0s %s")
            (setq helm-for-files-preferred-list
                  '(helm-source-buffers-list
                    helm-source-recentf
                    helm-source-bookmarks
                    helm-source-file-cache
                    helm-source-files-in-current-dir
                    helm-source-locate))
            (global-set-key "\C-x\ a" 'helm-for-files)
            (global-set-key (kbd "C-c y") 'helm-show-kill-ring)
            ;; replace M-x with helm's version
            (global-set-key (kbd "M-x") 'helm-M-x)
            ;; replace C-x b with helm's version
            (global-set-key "\C-x\ b" 'helm-mini)
            (global-set-key "\C-x\ \C-r" 'helm-recentf)
            (global-set-key (kbd "<f9>") 'helm-bookmarks)))

(use-package helm-projectile :ensure t
  :config
  (defun contextual-helm-projectile ()
    (if (and (buffer-file-name)
             (projectile-project-p))
        (progn
          (global-unset-key "\C-x\ a")
          (global-set-key "\C-x\ a" 'helm-projectile)
          )
      (progn
        (global-unset-key "\C-x\ a")
        (global-set-key "\C-x\ a" 'helm-for-files)
        )))
  (contextual-helm-projectile)
  (add-hook 'window-configuration-change-hook #'contextual-helm-projectile)
                                        ;(setq projectile-enable-caching t) ;; fix slow invocations of helm-projectile-find-file
  (helm-projectile-on))

(use-package helm-ag :ensure t
  :config
  (setq helm-ag-fuzzy-match t)
  (defun helm-ag-projectile-root (&optional ARG)
    "Search from projectile-project-root` which defaults to current directory if no project."
    (interactive)
    (helm-ag (projectile-project-root)))
  (defun helm-do-ag-projectile-root (&optional ARG)
    "Search from projectile-project-root` which defaults to current directory if no project."
    (interactive)
    (helm-do-ag (projectile-project-root))))

(use-package flyspell :ensure t
  :bind ("C-c C-SPC" . ispell-word)
  :diminish flyspell-mode)

(use-package highlight-symbol :ensure t
  :config (progn
            (global-set-key (kbd "<f13>") 'highlight-symbol-at-point)
            (global-set-key (kbd "<f14>") 'highlight-symbol-prev)
            (global-set-key (kbd "<f15>") 'highlight-symbol-next)
            (global-set-key (kbd "<f16>") 'highlight-symbol-query-replace)))

(use-package auto-highlight-symbol :ensure t :pin melpa
  :config (progn
            (add-hook 'prog-mode-hook (lambda ()
                                        (auto-highlight-symbol-mode t))))
  :diminish auto-highlight-symbol-mode)

(use-package smartparens :ensure t)

(use-package eyebrowse :ensure t
  :init (progn
          (setq eyebrowse-wrap-around t
                eyebrowse-new-workspace t)
          (eyebrowse-mode 1)
          (eyebrowse-switch-to-window-config-0))
  :diminish eyebrowse-mode)

(use-package undo-tree :ensure t :pin gnu
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-history-dir (let ((dir (concat user-emacs-directory
                                                 "undo-tree-history/")))
                                (make-directory dir :parents)
                                dir))
  (setq undo-tree-history-directory-alist `(("." . ,undo-tree-history-dir)))
  (defalias 'redo 'undo-tree-redo)
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

(use-package rainbow-delimiters :ensure t)

(use-package expand-region :ensure t
  :config (progn
            (global-set-key (kbd "C-=") 'er/expand-region)
            (global-set-key (kbd "C-M-=") 'er/contract-region)))

(use-package ox-reveal :ensure t :pin melpa
  :config (progn (setq org-reveal-root "file:///Users/cesarolea/workspace/reveal.js")))

(use-package org :ensure t
  :config
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb)

  (setq org-default-notes-file "~/Sync/Org/refile.org")
  (setq org-log-done t)

  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (add-hook 'org-mode-hook (lambda ()
                             (flyspell-mode 1)
                             (electric-pair-mode 1)
                             (delete '("\\.pdf\\'" . default) org-file-apps)
                             (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s"))))

  (defun set-exec-path-from-shell-PATH ()
    (let ((path-from-shell
           (replace-regexp-in-string "[[:space:]\n]*$" ""
                                     (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
      (setenv "PATH" path-from-shell)
      (setq exec-path (split-string path-from-shell path-separator))))
  (when (equal system-type 'darwin) (set-exec-path-from-shell-PATH))

  ;; (defun company-add-ispell ()
  ;;   (when (boundp 'company-backends)
  ;;     (make-local-variable 'company-backends)
  ;;     ;; add ispell
  ;;     (add-to-list 'company-backends 'company-ispell)))
  ;; (add-hook 'text-mode-hook 'company-add-ispell)

  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  ;; when evaluating, reinsert and preserve indentation
  (setq org-src-preserve-indentation t)
  ;; preserve native color scheme for target source code
  (setq org-src-fontify-natively t)
  ;; smart quotes on export
  (setq org-export-with-smart-quotes t)

  (define-key org-mode-map (kbd "s-u") #'org-goto)
  (define-key org-mode-map (kbd "s-U") #'org-mark-ring-goto)

  (add-hook 'org-mode-hook #'auto-fill-mode)

  ;; exporters
  (require 'ox-md)     ; markdown
  (require 'ox-reveal) ; nice presentations
  )

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

(use-package clojure-mode :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))
  :config (require 'flycheck-clj-kondo)
  :init
  (add-hook 'clojure-mode-hook (lambda () (progn
                                            (subword-mode t)
                                            (diminish 'subword-mode))))
  (add-hook 'clojure-mode-hook #'eldoc-mode)
  (diminish 'eldoc-mode))

(use-package cider :ensure t
  :config
  (defun company-remove-ispell ()
    (when (boundp 'company-backends)
      (make-local-variable 'company-backends)
      ;; remove ispell
      (setq company-backends (delete 'company-dabbrev company-backends))))
  (add-hook 'prog-mode-hook 'company-remove-ispell)

  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'clojure-mode-hook #'company-mode)

  (setq nrepl-hide-special-buffers t
        cider-repl-use-clojure-font-lock t ; syntax highlighting in REPL
        cider-overlays-use-font-lock t ; syntax highlight evaluation overlays
        cider-repl-toggle-pretty-printing t ; REPL always pretty-prints results
        cider-repl-display-help-banner nil ; don't display start banner
        nrepl-prompt-to-kill-server-buffer-on-quit nil ; don't prompt to kill server buffers on quit
        cider-repl-wrap-history t ; wrap around history when end is reached
        cider-save-file-on-load t ; don't prompt when eval, just save
        cider-font-lock-dynamically '(macro core function var) ; font lock from all namespaces
        )

  (define-key cider-repl-mode-map (kbd "C-c M-o") #'cider-repl-clear-buffer))

(use-package helm-cider :pin melpa
  :config (helm-cider-mode 1))

(use-package move-text :ensure t)

(use-package hydra :ensure t
  :config (load "~/.emacs.d/hydras.el"))


(use-package reveal-in-osx-finder :ensure t
  :bind ("C-c C-f" . reveal-in-osx-finder))

(use-package anzu :ensure t
  :config
  (global-anzu-mode)
  (set-face-attribute 'anzu-mode-line nil :foreground "white" :weight 'bold)
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
  :config
  (setq httpd-port 8181))

(use-package shrink-whitespace :ensure t
  :bind ("M-SPC" . shrink-whitespace))

(use-package diff-hl :ensure t
  :config (progn
            (add-hook 'prog-mode-hook (lambda ()
                                        (diff-hl-mode 1)))))

(use-package buffer-flip
  :ensure t
  :bind  (("M-<tab>" . buffer-flip)
          :map buffer-flip-map
          ( "M-<tab>" .   buffer-flip-forward)
          ( "<M-iso-lefttab>" . buffer-flip-backward)
          ( "M-ESC" .     buffer-flip-abort))
  :config
  (setq buffer-flip-skip-patterns
        '("^\\*helm\\b"
          "^\\*swiper\\*$")))

(use-package smooth-scroll :ensure t
  :config (progn
            (smooth-scroll-mode 1)
            (setq smooth-scroll-margin 5)
            ;; (setq smooth-scroll/vscroll-step-size 5
            ;;       smooth-scroll-margin 5)
            )
  :diminish smooth-scroll-mode)

(use-package origami :ensure t :pin melpa
  :config (progn
            (add-hook 'prog-mode-hook 'origami-mode)
            (global-set-key (kbd "<f5>") 'origami-recursively-toggle-node)))

(use-package git-timemachine :ensure t)

(use-package swiper :ensure t)

(use-package swiper-helm :ensure t
  :config (progn (global-set-key "\C-s" 'swiper)
                 (global-set-key "\C-r" 'swiper)))

(use-package crux :ensure t
  :config (progn (global-set-key "\M-m" 'crux-move-beginning-of-line)))

(use-package fireplace :ensure t)

(use-package restclient :ensure t :pin melpa)

(use-package company-restclient :ensure t :pin melpa
  :config (progn
            (add-hook 'restclient-mode-hook #'company-mode)
            (add-to-list 'company-backends 'company-restclient)))

(use-package restclient-helm :ensure t :pin melpa)

(use-package dumb-jump
  :pin melpa
  :ensure t
  :config
  (dumb-jump-mode t)
  (global-set-key (kbd "<f12>") 'dumb-jump-go)
  (setq dumb-jump-selector 'helm))

(use-package tramp :ensure t
  :config
  ;; Turn off auto-save for tramp files
  (defun tramp-set-auto-save ()
    (auto-save-mode -1))
  (with-eval-after-load 'tramp-cache
    (setq tramp-persistency-file-name "~/.emacs.d/tramp"))
  (setq tramp-default-method "ssh"
        tramp-default-user-alist '(("\\`su\\(do\\)?\\'" nil "root"))
        tramp-adb-program "adb"
        tramp-auto-save-directory "~/.emacs.d/tramp-autosave"
        tramp-verbose 6
        ;; use the settings in ~/.ssh/config instead of Tramp's
        tramp-use-ssh-controlmaster-options nil
        backup-enable-predicate
        (lambda (name)
          (and (normal-backup-enable-predicate name)
               (not (let ((method (file-remote-p name 'method)))
                      (when (stringp method)
                        (member method '("su" "sudo")))))))))

(use-package atomic-chrome :ensure t
  :pin melpa
  :config
  (setq atomic-chrome-default-major-mode  'text-mode)
  (setq atomic-chrome-buffer-open-style   'frame
        atomic-chrome-buffer-frame-width  100
        atomic-chrome-buffer-frame-height 25)
  ;; (setq atomic-chrome-url-major-mode-alist
  ;;       '(("flotiya\\.local" . js2-mode)
  ;;         ("phabricator" . text-mode)))
  (atomic-chrome-start-server)
  :diminish AtomicChrome)

(use-package dockerfile-mode :ensure t
  :diminish Dockerfile)

(use-package super-save :ensure t
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t
        auto-save-default nil)
  :diminish super-save-mode)

;; temporarily highlight changes from yanking, etc
(use-package volatile-highlights :ensure t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

(use-package yaml-mode :ensure t)

(use-package all-the-icons :ensure t)

(use-package neotree :ensure t
  :config
  (global-set-key [f7] 'neotree-toggle)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)
        neo-smart-open t
        neo-autorefresh nil
        neo-window-width (if (> (x-display-pixel-width) 5000) 40 35)
        projectile-switch-project-action 'neotree-projectile-action)
  (defun text-scale-once ()
    (interactive)(progn(text-scale-adjust 0)(text-scale-decrease 1)))
  (add-hook 'neo-after-create-hook (lambda (_)(call-interactively 'text-scale-once))))

(use-package s3ed :ensure t :pin melpa
  :config
  (global-set-key (kbd "C-c s f") 's3ed-find-file)
  (global-set-key (kbd "C-c s s") 's3ed-save-file))

(use-package php-mode :ensure t)

(use-package fontawesome :ensure t)

(use-package emojify :ensure t
  :config
  (setq
   emojify-prog-contexts           "comments"
   emojify-point-entered-behaviour "echo")
  (when (member "EmojiOne Color" (font-family-list))
    (set-fontset-font t 'unicode "EmojiOne Color" nil 'prepend))
  (add-hook 'after-init-hook #'global-emojify-mode))

(use-package lsp-mode :ensure t
  :commands lsp
  :diminish lsp-mode
  :config
  (setq lsp-enable-snippet nil)
  (require 'lsp-clients))

(use-package rust-mode :ensure t
  :config
  (setq rust-format-on-save t)
  :hook (rust-mode . lsp-deferred))

(use-package flycheck-rust :ensure t
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package flycheck-clj-kondo :ensure t)
