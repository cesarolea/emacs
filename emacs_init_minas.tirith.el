; minas-tirith customization

(load-theme 'moe-dark t)

;; set frame position
(set-frame-size (selected-frame) 200 65)

;; set theme
;; (load-theme 'moe-dark t)

;; set highlight line
(set-face-background 'highlight-current-line-face "gray17")

;; Font also, for emacsclient
(set-default-font "Inconsolata-13")

;; refile target in minas
(setq org-default-notes-file "/Volumes/Tomb/Sync/Org/refile.org")
