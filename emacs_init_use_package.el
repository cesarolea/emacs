;; use straight.el for retrieving packages
;(let ((bootstrap-file (concat user-emacs-directory "straight/bootstrap.el"))
;      (bootstrap-version 2))
;  (unless (file-exists-p bootstrap-file)
;    (with-current-buffer
;        (url-retrieve-synchronously
;         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;         'silent 'inhibit-cookies)
;      (goto-char (point-max))
;      (eval-print-last-sexp)))
;  (load bootstrap-file nil 'nomessage))

;; install use-package with straight.el
;(straight-use-package 'use-package)

;; Now use-package will use straight.el to automatically install missing packages if you provide :ensure t
(defconst cesaro-savefile-dir (expand-file-name "savefile" user-emacs-directory))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'diminish)
(require 'bind-key)

(diminish 'visual-line-mode)
(diminish 'abbrev-mode)

(eval-after-load "eldoc"
  '(diminish 'eldoc-mode))

(eval-after-load "autorevert"
  '(diminish 'auto-revert-mode))

(use-package use-package-ensure-system-package
  :ensure t)

(use-package use-package-chords
  :ensure t :pin melpa
  :config (key-chord-mode 1))

(use-package auto-package-update
  :ensure t :pin melpa-stable
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package zerodark-theme :ensure t :pin melpa-stable
  :config
  (load-theme 'zerodark t)
  (zerodark-setup-modeline-format)
  (global-hl-line-mode 1))

(use-package exec-path-from-shell :ensure t :pin melpa-stable
  :config (exec-path-from-shell-initialize))

(use-package popwin :ensure t :pin melpa-stable
  :config (popwin-mode 1))

(use-package recentf :ensure t :pin melpa-stable
  :defer 5
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 25)
  :bind ("\C-x\ \C-r" . recentf-open-files))

(use-package saveplace :ensure t :pin melpa-stable
  :init
  (setq save-place-file (concat user-emacs-directory "saveplace.el"))
  (setq-default save-place t))

(use-package savehist :ensure t :pin melpa-stable
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" cesaro-savefile-dir))
  (savehist-mode +1))

(use-package windmove :ensure t :pin melpa-stable
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings))

(use-package dired
  :config
  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)

  ;; enable some really cool extensions like C-x C-j(dired-jump)
  (require 'dired-x))

(use-package company :ensure t :pin melpa-stable
  :config
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  (global-set-key (kbd "C-'") 'company-complete)
  (global-company-mode)
  :diminish company-mode)

(use-package js2-mode :ensure t :pin melpa-stable
  :config (progn
            (add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))))

(use-package ido :ensure t :pin melpa-stable
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

(use-package ido-vertical-mode :ensure t :pin melpa-stable
  :config (progn
            (ido-mode 1)
            (ido-vertical-mode 1)
            (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)))

(use-package flx-ido :ensure t :pin melpa-stable
  :config (progn
            (flx-ido-mode 1)
            (setq ido-enable-flex-matching t
                  ido-use-faces t
                  ido-use-filename-at-point t)))

(use-package projectile :ensure t :pin melpa-stable
  :config (progn
            (setq projectile-require-project-root nil)
            (setq projectile-mode-line '(:eval (format " P[%s]" (projectile-project-name))))
            (projectile-global-mode t)))

(use-package helm-flx :ensure t
  :config (helm-flx-mode +1))

(use-package helm :ensure t :pin melpa-stable
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
                    helm-source-mac-spotlight))
            (global-set-key "\C-x\ a" 'helm-for-files)
            (global-set-key (kbd "C-c y") 'helm-show-kill-ring)
            ;; replace M-x with helm's version
            (global-set-key (kbd "M-x") 'helm-M-x)
            ;; replace C-x b with helm's version
            (global-set-key "\C-x\ b" 'helm-mini)
            (global-set-key "\C-x\ \C-r" 'helm-recentf)
            (global-set-key (kbd "<f9>") 'helm-bookmarks)))

(use-package helm-projectile :ensure t :pin melpa-stable
  :config (progn
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
            (setq projectile-enable-caching t) ;; fix slow invocations of helm-projectile-find-file
            (helm-projectile-on)))

(use-package helm-ag
  :ensure t :pin melpa-stable
  :config (progn (setq helm-ag-fuzzy-match t)
                 (defun helm-ag-projectile-root (&optional ARG)
                   "Search from projectile-project-root` which defaults to current directory if no project."
                   (interactive)
                   (helm-ag (projectile-project-root)))
                 (defun helm-do-ag-projectile-root (&optional ARG)
                   "Search from projectile-project-root` which defaults to current directory if no project."
                   (interactive)
                   (helm-do-ag (projectile-project-root)))))

(use-package helm-descbinds :ensure t :pin melpa-stable)

(use-package flycheck :ensure t :pin melpa-stable
  :defer 5
  :config (progn
            (add-hook 'after-init-hook #'global-flycheck-mode)
            (provide 'emacs_init_packages))
  :diminish flycheck-mode)

(use-package flyspell :ensure t :pin melpa-stable
  :defer 5
  :bind ("C-c C-SPC" . ispell-word)
  :diminish flyspell-mode)

(use-package highlight-symbol :ensure t :pin melpa-stable
  :defer 5
  :config (progn
            (global-set-key (kbd "<f13>") 'highlight-symbol-at-point)
            (global-set-key (kbd "<f14>") 'highlight-symbol-prev)
            (global-set-key (kbd "<f15>") 'highlight-symbol-next)
            (global-set-key (kbd "<f16>") 'highlight-symbol-query-replace)))

(use-package auto-highlight-symbol :ensure t :pin melpa
  :config (progn
            (add-hook 'prog-mode-hook (lambda ()
                                        (auto-highlight-symbol-mode t)
                                        (flyspell-prog-mode))))
  :diminish auto-highlight-symbol-mode)

(use-package smartparens :ensure t :pin melpa-stable)

(use-package eyebrowse :ensure t :pin melpa-stable
  :init (progn
          (setq eyebrowse-wrap-around t
                eyebrowse-new-workspace t)
          (eyebrowse-mode 1)
          (eyebrowse-switch-to-window-config-0))
  :diminish eyebrowse-mode)

(use-package undo-tree
  :ensure t :pin melpa
  :config
  (global-undo-tree-mode 1)
  (defalias 'redo 'undo-tree-redo)
  (global-set-key (kbd "s-z") 'undo)
  (global-set-key (kbd "M-z") 'redo)
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  :diminish undo-tree-mode)

(use-package web-mode :ensure t :pin melpa-stable
  :defer 5
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

(use-package ace-window :ensure t :pin melpa-stable
  :init (progn
          (define-key global-map (kbd "M-'") 'ace-window)
          (define-key global-map (kbd "C-M-'") 'aw-flip-window)
          (define-key global-map (kbd "C-x o") nil)))

(use-package rainbow-mode :ensure t :pin melpa
 :defer 5
 :diminish rainbow-mode)

(use-package rainbow-delimiters :ensure t :pin melpa-stable)

(use-package expand-region :ensure t :pin melpa-stable
  :config (progn
            (global-set-key (kbd "C-=") 'er/expand-region)
            (global-set-key (kbd "C-M-=") 'er/contract-region)))

(use-package ox-reveal
  :ensure t :pin melpa
  :config (progn (setq org-reveal-root "file:///Users/cesarolea/workspace/reveal.js")))

(use-package org
  :ensure t :pin melpa-stable
  :config (progn
            (global-set-key "\C-cl" 'org-store-link)
            (global-set-key "\C-cc" 'org-capture)
            (global-set-key "\C-ca" 'org-agenda)
            (global-set-key "\C-cb" 'org-iswitchb)

            (setq org-default-notes-file "~/Sync/Org/refile.org")
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
            ;; smart quotes on export
            (setq org-export-with-smart-quotes t)

            (define-key org-mode-map (kbd "s-u") #'org-goto)
            (define-key org-mode-map (kbd "s-U") #'org-mark-ring-goto)

            (add-hook 'org-mode #'auto-fill-mode)

            ;; exporters
            (require 'ox-md)     ; markdown
            (require 'ox-reveal) ; nice presentations
            ))

(use-package paredit :ensure t :pin melpa-stable
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

(use-package clojure-mode :ensure t :pin melpa-stable
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))
  :init
  (add-hook 'clojure-mode-hook (lambda () (progn
                                            (subword-mode t)
                                            (diminish 'subword-mode)
                                            (cider-hydra-mode 1))))
  (add-hook 'clojure-mode-hook #'eldoc-mode)
  (diminish 'eldoc-mode))

(use-package cider :pin melpa-stable
  :config (progn
	    (add-hook 'cider-mode-hook 'eldoc-mode)
      (add-hook 'cider-repl-mode-hook #'eldoc-mode)
	    (add-hook 'cider-repl-mode-hook #'company-mode)
	    (add-hook 'cider-mode-hook #'company-mode)
	    (add-hook 'clojure-mode-hook #'company-mode)

	    (setq nrepl-log-messages t ; useful for debugging
		  cider-repl-use-clojure-font-lock t ; syntax highlighting in REPL
		  cider-prompt-save-file-on-load 'always-save ;  just always save when loading buffer
		  cider-font-lock-dynamically '(macro core function var) ; syntax highlight all namespaces
		  cider-overlays-use-font-lock t ; syntax highlight evaluation overlays
		  cider-repl-toggle-pretty-printing t ; REPL always pretty-prints results
      cider-repl-display-help-banner nil ; don't display start banner
      nrepl-prompt-to-kill-server-buffer-on-quit nil ; don't prompt to kill server buffers on quit
      )

	    (define-key cider-repl-mode-map (kbd "C-c M-o") #'cider-repl-clear-buffer)))

(use-package cider-hydra :ensure t)

(use-package helm-cider
  :ensure t :pin melpa-stable
  :config (helm-cider-mode 1))

(use-package magit
  :ensure t :pin melpa-stable
  :bind ("<f10>" . magit-status)
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-push-always-verify nil)
  (setq magit-branch-read-upstream-first t)
  (if (bound-and-true-p magit-auto-revert-mode)
      (diminish 'magit-auto-revert-mode))
  :diminish magit-mode)

(use-package move-text :ensure t :pin melpa-stable)

(use-package hydra :ensure t :pin melpa-stable
  :config (load "~/.emacs.d/hydras.el"))


(use-package reveal-in-osx-finder :ensure t :pin melpa-stable
  :defer 5
  :bind ("C-c C-f" . reveal-in-osx-finder))

(use-package anzu :ensure t :pin melpa-stable
  :config
  (global-anzu-mode)
  (set-face-attribute 'anzu-mode-line nil :foreground "white" :weight 'bold)
  :bind ("M-%" . anzu-query-replace)
  :diminish anzu-mode)

(use-package bm :ensure t :pin melpa-stable
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

(use-package impatient-mode :ensure t :pin melpa-stable
  :defer 5
  :config
  (setq httpd-port 8181))

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook))
    (add-hook hook #'whitespace-mode))
  ;(add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 100) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

(use-package shrink-whitespace
  :ensure t :pin melpa-stable
  :bind ("M-SPC" . shrink-whitespace))

(use-package diff-hl
  :ensure t :pin melpa-stable
  :config (progn
            (add-hook 'prog-mode-hook (lambda ()
                                        (diff-hl-mode 1)))))

; (use-package company-emoji
;   :ensure t
;   :config (progn
;             ;; enable in org mode buffers
;             (add-hook 'org-mode-hook 'company-mode)
;             (add-hook 'org-mode-hook 'company-emoji-init)
;             ;; enable in git commit log buffers
;             (add-hook 'git-commit-mode-hook 'company-mode)
;             (add-hook 'git-commit-mode-hook 'company-emoji-init)))

(use-package buffer-flip
  :ensure t :pin melpa-stable
  :chords (("u8" . buffer-flip))
  :bind (:map buffer-flip-map
              ( "8" .   buffer-flip-forward)
              ( "*" .   buffer-flip-backward)
              ( "C-g" . buffer-flip-abort)))

(use-package smooth-scroll
  :ensure t :pin melpa-stable
  :config (progn
            (smooth-scroll-mode 1)
            (setq smooth-scroll/vscroll-step-size 5))
  :diminish smooth-scroll-mode)

(use-package org-bullets
  :ensure t :pin melpa-stable
  :config (progn
            (setq org-bullets-face-name (quote org-bullet-face))
            (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
            (setq org-bullets-bullet-list '("✙" "♱" "♰" "☥" "✞" "✟" "✝" "†" "✠" "✚" "✜" "✛" "✢" "✣" "✤" "✥"))))

(use-package origami
  :defer 5
  :ensure t :pin melpa
  :config (progn
            (add-hook 'prog-mode-hook 'origami-mode)
            (global-set-key (kbd "<f5>") 'origami-recursively-toggle-node)))

(use-package git-timemachine
  :ensure t :pin melpa-stable
  :defer 5)

(use-package swiper :ensure t :pin melpa-stable)

(use-package swiper-helm
  :ensure t :pin melpa-stable
  :config (progn (global-set-key "\C-s" 'swiper)
                 (global-set-key "\C-r" 'swiper)))

(use-package crux :ensure t :pin melpa-stable
  :config (progn (global-set-key "\M-m" 'crux-move-beginning-of-line)))

(use-package fireplace :ensure t :pin melpa-stable :defer 10)

(use-package restclient :ensure t :pin melpa)

(use-package company-restclient
  :ensure t :pin melpa-stable
  :defer 5
  :config (progn
            (add-hook 'restclient-mode-hook #'company-mode)
            (add-to-list 'company-backends 'company-restclient)))

(use-package restclient-helm :ensure t :defer 10)

(use-package terraform-mode :ensure t :pin melpa-stable :defer 10)

(use-package dumb-jump
  :ensure t :pin melpa-stable
  :config (progn
            (dumb-jump-mode t)
            (global-set-key (kbd "<f12>") 'dumb-jump-go)
            (setq dumb-jump-selector 'helm)))

(use-package tramp
  :defer 5
  :config
  ;; Turn off auto-save for tramp files
  (defun tramp-set-auto-save ()
    (auto-save-mode -1))
  (with-eval-after-load 'tramp-cache
    (setq tramp-persistency-file-name "~/.emacs.d/tramp"))
  (setq tramp-default-method "ssh"
        tramp-default-user-alist '(("\\`su\\(do\\)?\\'" nil "root"))
        tramp-adb-program "adb"
        ;; use the settings in ~/.ssh/config instead of Tramp's
        tramp-use-ssh-controlmaster-options nil
        backup-enable-predicate
        (lambda (name)
          (and (normal-backup-enable-predicate name)
               (not (let ((method (file-remote-p name 'method)))
                      (when (stringp method)
                        (member method '("su" "sudo")))))))))

(use-package atomic-chrome :ensure t :pin melpa-stable
  :defer 10
  :config
  (setq atomic-chrome-default-major-mode 'text-mode)
  (setq atomic-chrome-url-major-mode-alist
        '(("flotiya\\.local" . js2-mode)
          ("phabricator" . text-mode)))
  (atomic-chrome-start-server)
  :diminish AtomicChrome)

(use-package dockerfile-mode :ensure t :pin melpa-stable
  :defer 10
  :diminish Dockerfile)

(use-package platformio-mode :ensure t :pin melpa-stable
  :defer 10
  :config
  ;; Enable irony for all c++ files, and platformio-mode only
  ;; when needed (platformio.ini present in project root).
  (add-hook 'c++-mode-hook (lambda () (platformio-conditionally-enable))))

(use-package super-save
  :ensure t :pin melpa-stable
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t
        auto-save-default nil)
  :diminish super-save-mode)

;; temporarily highlight changes from yanking, etc
(use-package volatile-highlights
  :ensure t :pin melpa-stable
  :config
  (volatile-highlights-mode +1)
  :diminish volatile-highlights-mode)

(use-package dockerfile-mode :ensure t :pin melpa-stable :defer 10)

(use-package yaml-mode :ensure t :pin melpa-stable :defer 10)

(use-package all-the-icons :ensure t :pin melpa-stable)

(use-package neotree :ensure t :pin melpa-stable
  :config
  (global-set-key [f7] 'neotree-toggle)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)
        neo-smart-open t
        neo-autorefresh t
        neo-window-width (if (> (x-display-pixel-width) 5000) 40 35)))
