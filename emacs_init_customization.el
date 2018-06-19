(defvar root-dir "~/.emacs.d/")

(global-set-key "\M-/" 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
	try-complete-file-name-partially
	try-complete-file-name
	try-expand-all-abbrevs
	try-expand-list
	try-expand-line
	try-complete-lisp-symbol-partially
	try-complete-lisp-symbol))

; execution path so homebrew binaries work
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Add newlines at the end of line with C-n
(setq next-line-add-newlines t)

;; stop blinking cursor
(blink-cursor-mode -1)

;; force new frames into existing window
(setq ns-pop-up-frames nil)

;; no bell
(setq ring-bell-function 'ignore)

;; Move to the previous window
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))

;; Erase region on insert
(delete-selection-mode 1)
(global-set-key (kbd "C-c d") 'c-hungry-delete-forward)

;; tabs and indentation
(setq standard-indent 2)
(setq-default tab-width 2)
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(setq-default c-default-style "linux");

;; encoding
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; show column number in bar
(column-number-mode t)

;; highlight incremental search
(defconst search-highlight t)

;; no newlines past EOF
(setq next-line-add-newlines nil)

;; wrap lines in a tasteful way
(global-visual-line-mode 1)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; pick up changes to files on disk automatically (ie, after git pull)
(global-auto-revert-mode t)

;; don't confirm opening non-existant files/buffers
(setq confirm-nonexistent-file-or-buffer nil)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; don't prompt for new buffer creation in ido
(setq ido-create-new-buffer 'always)

;; for commenting lines or blocks
(global-set-key "\C-c\ -" 'comment-region)
(global-set-key "\C-c\ +" 'uncomment-region)

;; yes, I want to kill buffers with processes attached
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Do not ask about running processes when exiting.
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-letf ((process-list ())) ad-do-it))

;; use finder database instead of locate
(setq locate-command "mdfind")

;; Anwsering y/n is faster than yes/no.
(fset 'yes-or-no-p 'y-or-n-p)

;; scroll PDFs with the mouse wheel
(setq doc-view-continuous t)

;; so PDFs don't hurt my eyes
(setq doc-view-resolution 300)

;; stop telling me the menu command key
(setq suggest-key-bindings nil)

(defun buffer-too-big-p ()
  (or (> (buffer-size) (* 5000 80))
      (> (line-number-at-pos (point-max)) 5000)))

;; line number configuration
(setq display-line-numbers-grow-only t
      display-line-numbers-type "relative")

(global-set-key [f6] 'display-line-numbers-mode)

;; prog mode setup
(add-hook 'prog-mode-hook (lambda ()
                            (flycheck-mode 1)
                            (electric-pair-mode 1)
                            (rainbow-mode 1)
                            (flyspell-mode 1)
                            (visual-line-mode 0)
                            (toggle-truncate-lines 1)
                            (show-paren-mode t)
                            (lambda ()
                              ;; turn off `display-line-numbers-mode' when there are more than 5000 lines
                              (if (buffer-too-big-p) (display-line-numbers-mode -1)))
                            (whitespace-mode 1)
                            (lambda ()
                              (local-set-key (kbd "C-M-;") #'comment-or-uncomment-sexp))))

;; but only lisps should have rainbow delimiters
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

;; default major mode is text mode instead of fundamental mode
(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook (lambda ()
			    (electric-pair-mode 0)
			    (flycheck-mode 0)
          ;(set-input-method "spanish-prefix")
          ))

;; which function
(which-function-mode)

;; move naturally between open windows
(windmove-default-keybindings)

;; move text lines or regions
(global-set-key [M-S-up] 'move-text-up)
(global-set-key [M-S-down] 'move-text-down)

;; drag and drop arrangement
(defun th/swap-window-buffers-by-dnd (drag-event)
  "Swaps the buffers displayed in the DRAG-EVENT's start and end
window."
  (interactive "e")
  (let ((start-win (cl-caadr drag-event))
        (end-win   (cl-caaddr drag-event)))
    (when (and (windowp start-win)
               (windowp end-win)
               (not (eq start-win end-win))
               (not (memq (minibuffer-window)
                          (list start-win end-win))))
      (let ((bs (window-buffer start-win))
            (be (window-buffer end-win)))
        (unless (eq bs be)
          (set-window-buffer start-win be)
          (set-window-buffer end-win bs))))))
(global-set-key (kbd "<C-S-drag-mouse-1>") #'th/swap-window-buffers-by-dnd)

;; automatically indenting yanked text if in programming-modes
(defvar yank-indent-modes '(prog-mode)
  "Modes in which to indent regions that are yanked (or yank-popped)")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (member major-mode yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (member major-mode yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

;; replace ls for gls from coreutils (for dired)
(setq insert-directory-program (executable-find "gls"))

(defun xah-cut-line-or-region ()
  "Cut current line, or text selection.
When `universal-argument' is called first, cut whole buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2015-05-06"
  (interactive)
  (let (ξp1 ξp2)
    (if current-prefix-arg
        (progn (setq ξp1 (point-min))
               (setq ξp2 (point-max)))
      (progn (if (use-region-p)
                 (progn (setq ξp1 (region-beginning))
                        (setq ξp2 (region-end)))
               (progn (setq ξp1 (line-beginning-position))
                      (setq ξp2 (line-beginning-position 2))))))
    (kill-region ξp1 ξp2)))
(global-set-key (kbd "C-w") 'xah-cut-line-or-region)

(defun xah-copy-line-or-region ()
  "Copy current line, or text selection.
When `universal-argument' is called first, copy whole buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2015-05-06"
  (interactive)
  (let (ξp1 ξp2)
    (if current-prefix-arg
        (progn (setq ξp1 (point-min))
               (setq ξp2 (point-max)))
      (progn (if (use-region-p)
                 (progn (setq ξp1 (region-beginning))
                        (setq ξp2 (region-end)))
               (progn (setq ξp1 (line-beginning-position))
                      (setq ξp2 (line-end-position))))))
    (kill-ring-save ξp1 ξp2)
    (if current-prefix-arg
        (message "buffer text copied")
      (message "text copied"))))
(global-set-key (kbd "M-w") 'xah-copy-line-or-region)

;; Allow clipboard from outside emacs
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)

(defun uncomment-sexp (&optional n)
  "Uncomment a sexp around point."
  (interactive "P")
  (let* ((initial-point (point-marker))
         (p)
         (end (save-excursion
                (when (elt (syntax-ppss) 4)
                  (re-search-backward comment-start-skip
                                      (line-beginning-position)
                                      t))
                (setq p (point-marker))
                (comment-forward (point-max))
                (point-marker)))
         (beg (save-excursion
                (forward-line 0)
                (while (= end (save-excursion
                                (comment-forward (point-max))
                                (point)))
                  (forward-line -1))
                (goto-char (line-end-position))
                (re-search-backward comment-start-skip
                                    (line-beginning-position)
                                    t)
                (while (looking-at-p comment-start-skip)
                  (forward-char -1))
                (point-marker))))
    (unless (= beg end)
      (uncomment-region beg end)
      (goto-char p)
      ;; Indentify the "top-level" sexp inside the comment.
      (while (and (ignore-errors (backward-up-list) t)
                  (>= (point) beg))
        (skip-chars-backward (rx (syntax expression-prefix)))
        (setq p (point-marker)))
      ;; Re-comment everything before it.
      (ignore-errors
        (comment-region beg p))
      ;; And everything after it.
      (goto-char p)
      (forward-sexp (or n 1))
      (skip-chars-forward "\r\n[:blank:]")
      (if (< (point) end)
          (ignore-errors
            (comment-region (point) end))
        ;; If this is a closing delimiter, pull it up.
        (goto-char end)
        (skip-chars-forward "\r\n[:blank:]")
        (when (= 5 (car (syntax-after (point))))
          (delete-indentation))))
    ;; Without a prefix, it's more useful to leave point where
    ;; it was.
    (unless n
      (goto-char initial-point))))

(defun comment-sexp--raw ()
  "Comment the sexp at point or ahead of point."
  (pcase (or (bounds-of-thing-at-point 'sexp)
             (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (bounds-of-thing-at-point 'sexp)))
    (`(,l . ,r)
     (goto-char r)
     (skip-chars-forward "\r\n[:blank:]")
     (comment-region l r)
     (skip-chars-forward "\r\n[:blank:]"))))

(defun comment-or-uncomment-sexp (&optional n)
  "Comment the sexp at point and move past it.
If already inside (or before) a comment, uncomment instead.
With a prefix argument N, (un)comment that many sexps."
  (interactive "P")
  (if (or (elt (syntax-ppss) 4)
          (< (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (point))
             (save-excursion
               (comment-forward 1)
               (point))))
      (uncomment-sexp n)
    (dotimes (_ (or n 1))
      (comment-sexp--raw))))

;; dired sane file sizes
(setq dired-listing-switches "-alh")

;; switch between two most recent buffers
(defun switch-to-previous-buffer ()
  "Switch to most recent buffer. Repeated calls toggle back and forth between the most recent two buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; set key binding
(global-set-key (kbd "C-+") 'switch-to-previous-buffer)

;; so minibuffer history is saved
(savehist-mode 1)

;; better ediff
(customize-set-variable 'ediff-window-setup-function 'ediff-setup-windows-plain)
(customize-set-variable 'ediff-split-window-function 'split-window-horizontally)
(customize-set-variable 'ediff-diff-options "-w")

(defun ora-ediff-hook ()
  (ediff-setup-keymap)
  (define-key ediff-mode-map "j" 'ediff-next-difference)
  (define-key ediff-mode-map "k" 'ediff-previous-difference))

(add-hook 'ediff-mode-hook 'ora-ediff-hook)

;; When popping the mark, continue popping until the cursor
;; actually moves
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

;; quickly pop the mark several times by typing C-u C-SPC C-SPC
(setq set-mark-command-repeat-pop t)

;; recompile emacs config
(defun recompile-emacs-config (&optional FORCE)
  "Recompile Emacs configuration."
  (interactive)
  (byte-recompile-directory root-dir 0 FORCE))

;; If all you use is magit anyway, this is not really a loss
(remove-hook 'find-file-hooks 'vc-find-file-hook)

(defun modi/switch-to-scratch-and-back (arg)
  "Toggle between *scratch-MODE* buffer and the current buffer.
If a scratch buffer does not exist, create it with the major mode set to that
of the buffer from where this function is called.

        COMMAND -> Open/switch to a scratch buffer in the current buffer's major mode
    C-0 COMMAND -> Open/switch to a scratch buffer in `fundamental-mode'
    C-u COMMAND -> Open/switch to a scratch buffer in `org-mode'
C-u C-u COMMAND -> Open/switch to a scratch buffer in `emacs-elisp-mode'"
  (interactive "p")
  (if (and (= arg 1) ; no prefix
           (string-match-p "\\*scratch" (buffer-name)))
      (switch-to-buffer (other-buffer))
    (let ((mode-str (cl-case arg
                      (0  "fundamental-mode") ; C-0
                      (4  "org-mode") ; C-u
                      (16 "emacs-lisp-mode") ; C-u C-u
                      (t  (format "%s" major-mode))))) ; no prefix
      (switch-to-buffer (get-buffer-create
                         (concat "*scratch-" mode-str "*")))
      (funcall (intern mode-str)))))
(global-set-key (kbd "<f8>") 'modi/switch-to-scratch-and-back)

;; Search for equivalent unicode characters when searching
;; for ascii chars
(setq search-default-mode #'char-fold-to-regexp)

;; increase garbage collection threshold
(setq gc-cons-threshold (* 100 1024 1024)) ;; 100 mb

;; Allow font-lock-mode to do background parsing
(setq jit-lock-defer-time nil
      jit-lock-stealth-nice 0.1
      jit-lock-stealth-time 0.2
      jit-lock-stealth-verbose nil)

;; If you change buffer, or focus, disable the current buffer’s mark
(transient-mark-mode 1)

;; reduce lag when scrolling down
(setq auto-window-vscroll nil)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Newline at end of file
(setq require-final-newline -1)
