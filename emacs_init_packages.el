; sources
(require 'package)
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "http://melpa.milkbox.net/packages/")
                         ("tromey"    . "http://tromey.com/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

; default theme
(load-theme 'moe-light t)

; flyspell
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

; helm
(require 'helm-files)
(setq helm-idle-delay 0.1)
(setq helm-input-idle-delay 0.1)
(setq helm-c-locate-command "locate-with-mdfind %.0s %s")
(global-set-key "\C-x\ a" 'helm-for-files)

; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(provide 'emacs_init_packages)

(require 'highlight-symbol)
(global-set-key (kbd "<f13>") 'highlight-symbol-at-point)
(global-set-key (kbd "<f14>") 'highlight-symbol-prev)
(global-set-key (kbd "<f15>") 'highlight-symbol-next)
(global-set-key (kbd "<f16>") 'highlight-symbol-query-replace)

; real auto save
;(require 'real-auto-save)
;(add-hook 'org-mode-hook 'turn-on-real-auto-save)
;(setq real-auto-save-interval 30) ;;in seconds

; highlight current line
(require 'highlight-current-line)
(set-face-background 'highlight-current-line-face "LightYellow2")

; auto-highlight symbol
(require 'auto-highlight-symbol)

(add-hook 'prog-mode-hook (lambda ()
							(highlight-current-line-minor-mode t)
							(auto-highlight-symbol-mode t)))

; highlight matching parens with smartparens
(show-smartparens-global-mode +1)
