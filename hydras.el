(defhydra hydra-major (:color blue)
  "major mode"
  ("q" sql-mode "SQL")
  ("o" org-mode "org")
  ("t" text-mode "text")
  ("w" web-mode "web")
  ("j" js2-mode "JavaScript")
  ("m" markdown-mode "Markdown"))
(global-set-key (kbd "C-c m") 'hydra-major/body)

(defhydra hydra-window (:color red :hint nil)
  "
 Split: _v_ert _x_:horz
Delete: _o_ther ace-_d_elete
Resize: _H_:splitter left  _J_:splitter down  _K_:splitter up  _L_:splitter right
  Move: _s_wap
  Misc: _a_ce-window _+_:text increase _-_:text decrease _=_:text adjust
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
  ("H" hydra-move-splitter-left)
  ("J" hydra-move-splitter-down)
  ("K" hydra-move-splitter-up)
  ("L" hydra-move-splitter-right)
  ("a" ace-window "Ace window" :exit t)
  ("+" text-scale-increase)
  ("-" text-scale-decrease)
  ("=" text-scale-adjust :exit t))
(global-set-key (kbd "C-c w") 'hydra-window/body)

(defhydra hydra-movement (:color blue)
  "movement"
  ("c" avy-goto-char-2 "Go to char")
  ("l" avy-goto-line "Go to line")
  ("L" goto-line "Go to line number")
  ("w" avy-goto-word-1 "Go to word"))
(global-set-key (kbd "C-c g") 'hydra-movement/body)

(global-set-key (kbd "M-.") 'avy-goto-char-2)

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

(defhydra hydra-org (:color red :hint nil)
  "
Capture^       ^Navigation^
-----------------------------------------------------------
capture         _j_ next heading
last capture    _k_ prev heading
                _h_ next heading (same level)
                _l_ prev heading (same level)
                _u_p higher heading
                _t_oggle
                _g_o to
"
  ("j" outline-next-visible-heading)
  ("k" outline-previous-visible-heading)
  ("h" org-forward-heading-same-level)
  ("l" org-backward-heading-same-level)
  ("u" outline-up-heading)
  ("t" org-cycle)
  ("g" org-goto :exit t))
(global-set-key (kbd "C-c o") 'hydra-org/body)

(defhydra hydra-utility (:color blue :hint nil)
  "
URL^             ^Format^  ^Misc^
--------------------------------------------------------
_h_umanify        _j_son    _c_opy filename to clipboard
_d_ecode region   _x_ml     _s_how filename of buffer
                        _i_nsert filename to buffer
                        _t_oggle letter case"
  ("h" url-humanify)
  ("d" url-decode-region)
  ("j" json-pretty-print)
  ("x" xml-format)
  ("c" copy-file-name-to-clipboard)
  ("s" show-file-name-of-current-buffer)
  ("t" toggle-letter-case :color red)
  ("i" bjm/insert-file-name))
(global-set-key (kbd "C-c u") 'hydra-utility/body)

(defhydra hydra-eyebrowse (:color blue :hint nil)
  "
Workspace^                     ^Navigation^
------------------------------------------------
Workspace _1_   Workspace _6_   _n_ext workspace
Workspace _2_   Workspace _7_   _p_rev workspace
Workspace _3_   Workspace _8_   _l_ast workspace
Workspace _4_   Workspace _9_   _c_lose workspace
Workspace _5_   Workspace _0_
"
  ("1" eyebrowse-switch-to-window-config-0)
  ("2" eyebrowse-switch-to-window-config-2)
  ("3" eyebrowse-switch-to-window-config-3)
  ("4" eyebrowse-switch-to-window-config-4)
  ("5" eyebrowse-switch-to-window-config-5)
  ("6" eyebrowse-switch-to-window-config-6)
  ("7" eyebrowse-switch-to-window-config-7)
  ("8" eyebrowse-switch-to-window-config-8)
  ("9" eyebrowse-switch-to-window-config-9)
  ("0" eyebrowse-switch-to-window-config-0)
  ("n" eyebrowse-next-window-config)
  ("p" eyebrowse-prev-window-config)
  ("l" eyebrowse-last-window-config)
  ("c" eyebrowse-close-window-config))
(global-set-key (kbd "C-c e") 'hydra-eyebrowse/body)

(defhydra hydra-flycheck (:color red :hint nil)
  "
Navigation^  ^Buffer^
------------------
_j_ Next      _C_lear
_k_ Prev      _B_uffer
_h_ First     _D_isable
_l_ List      _S_etup
"
  ("j" flycheck-next-error)
  ("k" flycheck-previous-error)
  ("h" flycheck-first-error)
  ("l" flycheck-list-errors :color blue)
  ("C" flycheck-clear)
  ("B" flycheck-buffer)
  ("D" flycheck-disable-checker :color blue)
  ("S" flycheck-verify-setup :color blue))
(global-set-key (kbd "C-c k") 'hydra-flycheck/body)

(defhydra hydra-bm (:color red :hint nil :idle 1.0)
  "Bookmarks"
  ("t" bm-toggle "Toggle")
  ("T" bm-toggle "Toggle" :color blue)
  ("j" bm-next "Next")
  ("k" bm-previous "Previous")
  ("l" bm-show "Show local")
  ("A" bm-show-all "Show all")
  ("x" bm-remove-all-current-buffer :color blue)
  ("X" bm-remove-all-all-buffers :color blue))
(global-set-key (kbd "C-c b") 'hydra-bm/body)

(defhydra hydra-origami (:color red :hint nil :timeout)
    "
Code Folds^       ^Navigation^
---------------------------------
_t_ Toggle       _j_ Move to next
_T_ Toggle All   _k_ Move to previous
_u_ undo
_r_ redo
_R_ Reset
"
  ("t" origami-recursively-toggle-node)
  ("T" origami-toggle-all-nodes)
  ("u" origami-undo)
  ("r" origami-redo)
  ("j" origami-next-fold)
  ("k" origami-previous-fold)
  ("R" origami-reset))
(global-set-key (kbd "C-c f") 'hydra-origami/body)
