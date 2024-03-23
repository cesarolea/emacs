;; disable package.el
(setq package-enable-at-startup nil)

;; no titlebar - rounded corners in macOS
(add-to-list 'default-frame-alist '(undecorated-round . t))

(defvar default-gc-cons-threshold 16777216 ; 16mb
  "my default desired value of `gc-cons-threshold'
during normal emacs operations.")

;; make garbage collector less invasive
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; (add-hook
;;  'emacs-startup-hook
;;  (lambda (&rest _)
;;    (setq gc-cons-threshold default-gc-cons-threshold)
;;    (setq gc-cons-percentage 0.1)))

;; Local Variables:
;; no-byte-compile: t
;; End:
