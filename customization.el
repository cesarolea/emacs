;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(save-place-mode 1)

;; stop blinking cursor
(blink-cursor-mode -1)

;; Move to the previous window
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))

;; Erase region on insert
(delete-selection-mode 1)
(global-set-key (kbd "C-c d") 'c-hungry-delete-forward)

(setq-default tab-width 2
	      c-basic-offset 4
	      indent-tabs-mode nil
	      c-default-style "linux")

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

;; wrap lines in a tasteful way
(global-visual-line-mode 1)

;; pick up changes to files on disk automatically (ie, after git pull)
(global-auto-revert-mode t)

;; for commenting lines or blocks
(global-set-key "\C-c\ -" 'comment-region)
(global-set-key "\C-c\ +" 'uncomment-region)

;; Anwsering y/n is faster than yes/no.
(fset 'yes-or-no-p 'y-or-n-p)

(defun buffer-too-big-p ()
  (or (> (buffer-size) (* 5000 80))
      (> (line-number-at-pos (point-max)) 5000)))

(global-set-key [f6] 'display-line-numbers-mode)

;; cleanup whitespace
(global-set-key [f2] 'whitespace-cleanup)

;; prog mode setup
(add-hook 'prog-mode-hook (lambda ()
                            (electric-pair-mode 1)
                            (rainbow-mode 1)
                            (visual-line-mode 0)
                            (toggle-truncate-lines 1)
                            (show-paren-mode t)
                            (add-hook 'before-save-hook #'whitespace-cleanup nil 'make-it-local)
                            (local-set-key (kbd "C-M-;") #'comment-or-uncomment-sexp)))

;; but only lisps should have rainbow delimiters
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

(add-hook 'clojure-mode-hook 'whitespace-mode)
(add-hook 'emacs-lisp-mode-hook 'whitespace-mode)

(add-hook 'text-mode-hook (lambda ()
                            (electric-pair-mode 0)
                            (flycheck-mode 0)))

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

;; If you change buffer, or focus, disable the current buffer’s mark
(transient-mark-mode 1)

;;scroll window up/down by one line
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))

;; Dired setup
;; reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; enable some really cool extensions like C-x C-j(dired-jump)
(require 'dired-x)

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
(define-key global-map "\M-Q" 'unfill-paragraph)

(defun joaot/delete-process-at-point ()
  (interactive)
  (let ((process (get-text-property (point) 'tabulated-list-id)))
    (cond ((and process
                (processp process))
           (delete-process process)
           (revert-buffer))
          (t
           (error "no process at point!")))))

(define-key process-menu-mode-map (kbd "C-k") 'joaot/delete-process-at-point)