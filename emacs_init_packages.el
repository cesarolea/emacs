; sources
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

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

; helm
(require 'helm-files)
(setq helm-idle-delay 0.1)
(setq helm-input-idle-delay 0.1)
(setq helm-c-locate-command "locate-with-mdfind %.0s %s")
(global-set-key "\C-x\ a" 'helm-for-files)

; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(provide 'emacs_init_packages)
