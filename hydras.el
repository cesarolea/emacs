(defhydra hydra-major (:color blue)
  "major mode"
  ("q" sql-mode "SQL")
  ("o" org-mode "org")
  ("t" text-mode "text")
  ("w" web-mode "web")
  ("j" js2-mode "JavaScript"))
(global-set-key (kbd "C-c m") 'hydra-major/body)

(defhydra hydra-window (:color red :hint nil)
  "
 Split: _v_ert _x_:horz
Delete: _o_ther ace-_d_elete
  Move: _s_wap
  Misc: _a_ce-window
"
  ("v" split-window-right)
  ("x" split-window-below)
  ("A" hydra-move-splitter-left)
  ("S" hydra-move-splitter-down)
  ("W" hydra-move-splitter-up)
  ("D" hydra-move-splitter-right)
  ("s" ace-swap-window)
  ("d" ace-delete-window)
  ("|" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right)) "Split right and move")
  ("_" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)) "Split below and move")
  ("o" delete-other-windows "Delete other windows" :exit t)
  ("a" ace-window "Ace window" :exit t))
(global-set-key (kbd "C-c w") 'hydra-window/body)

(defhydra hydra-movement (:color blue)
  "movement"
  ("c" avi-goto-char-2 "Go to char")
  ("l" avi-goto-line "Go to line")
  ("w" avi-goto-word-1 "Go to word"))
(global-set-key (kbd "C-c g") 'hydra-movement/body)

(defhydra hydra-gist (:color blue)
  "gists"
  ("l" gist-list "List gists")
  ("g" gitst-region-or-buffer "Gist region or buffer")
  ("P" gist-region-or-buffer-private "Gist region or buffer private")
  ("r" gist-region "Gist region")
  ("R" gist-region-private "Private gist")
  ("b" gist-buffer "Gist buffer")
  ("B" gist-buffer-private "Gist buffer private"))
(global-set-key (kbd "C-c s") 'hydra-gist/body)
