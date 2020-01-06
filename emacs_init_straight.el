;; install use-package
(straight-use-package 'use-package)

;; set proper path
(when (memq window-system '(mac ns))
  (straight-use-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; use straight.el by default
(setq straight-use-package-by-default t)
