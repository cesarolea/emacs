;; default theme
(require 'moe-theme)
(moe-dark)

(require 'powerline)
(powerline-moe-theme)

;; git-gutter
(require 'git-gutter)
(global-git-gutter-mode t)
(git-gutter:linum-setup)

;; flyspell
(global-set-key (kbd "C-c C-SPC") 'ispell-word)

; recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

; autocomplete
(require 'auto-complete-config)
(ac-config-default)

; js2-mode
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

; js2-refactor
(require 'js2-refactor)
(js2r-add-keybindings-with-prefix "C-c C-r")

; js-comint
(require 'js-comint)
(cond ((eq system-type 'darwin) (progn
                                  (setq inferior-js-program-command "/usr/local/bin/node -i")
                                  (add-hook 'inferior-js-mode-hook
                                            (lambda ()
                                             ;; We like nice colors
                                              (ansi-color-for-comint-mode-on)
                                              ;; Deal with some prompt nonsense
                                              (add-to-list
                                               'comint-preoutput-filter-functions
                                               (lambda (output)
                                                 (replace-regexp-in-string "\033\\[[0-9]+[GKJ]" "" output))))))))

; smex - ido for modes
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(setf smex-key-advice-ignore-menu-bar 1)

; ido vertical mode
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)

; helm
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

; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(provide 'emacs_init_packages)

(require 'highlight-symbol)
(global-set-key (kbd "<f13>") 'highlight-symbol-at-point)
(global-set-key (kbd "<f14>") 'highlight-symbol-prev)
(global-set-key (kbd "<f15>") 'highlight-symbol-next)
(global-set-key (kbd "<f16>") 'highlight-symbol-query-replace)

; highlight current line
(require 'highlight-current-line)

; auto-highlight symbol
(require 'auto-highlight-symbol)

(add-hook 'prog-mode-hook (lambda ()
							(highlight-current-line-minor-mode t)
							(auto-highlight-symbol-mode t)
							(flyspell-prog-mode)))

; highlight matching parens with smartparens
(show-smartparens-global-mode +1)

; eyebrowse
(setq eyebrowse-keymap-prefix (kbd "H-w"))
(eyebrowse-mode t)

; undo tree
(global-undo-tree-mode 1)

; command is mapped to super (lowercase s)
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "M-z") 'redo)

; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

; web-mode indentation
(setq web-mode-markup-indent-offset 4)
(setq web-mode-code-indent-offset 4)

(require 'ace-jump-mode)
(define-key global-map (kbd "C-' SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-' C-u SPC") 'ace-jump-char-mode)
(define-key global-map (kbd "C-' C-u C-u SPC") 'ace-jump-line-mode)
(define-key global-map (kbd "M-'") 'ace-window)

; unset C-x o for other window
(define-key global-map (kbd "C-x o") nil)

; so we don't get lost in lisp
(require 'rainbow-delimiters)

; configure autocomplete for clojure
(require 'ac-nrepl)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

; popwin
(require 'popwin)
(global-set-key (kbd "M-z") popwin:keymap)
(popwin-mode 1)

;; expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; visual column indicator
(require 'fill-column-indicator)
