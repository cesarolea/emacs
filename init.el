(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

(use-package org
  :config
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-ca" 'org-agenda)

  (define-key org-mode-map (kbd "C-c r") #'org-list-repair)
  (define-key org-mode-map (kbd "s-U") #'org-mark-ring-goto)
  (define-key org-mode-map (kbd "s-l") #'org-toggle-link-display)
  (define-key org-mode-map (kbd "s-i") #'org-toggle-inline-images)
  (define-key org-mode-map (kbd "C-c C-,") #'org-insert-structure-template)

  (require 'org-protocol)
  (require 'org-capture)
  ;; (require 'org-contacts)

  (defadvice org-capture
      (after make-full-window-frame activate)
    "Advise capture to be the only window when used as a popup"
    (if (equal "emacs-capture-win" (frame-parameter nil 'name))
        (delete-other-windows)))

  (defadvice org-capture-finalize
      (after delete-capture-frame activate)
    "Advise capture-finalize to close the frame"
    (if (equal "emacs-capture-win" (frame-parameter nil 'name))
        (delete-frame)))

  (setq org-capture-templates
        '(("w" "Work Workflow")
          ("wt" "Task" entry (file+olp "~/Dropbox/org/work.org.gpg" "To Do")
           "* TODO %?\n  ADDED: %t\n\n%i" :empty-lines 1)
          ("wr" "Refile" entry (file+olp "~/Dropbox/org/work.org.gpg" "Refile")
           "* TODO %?\n  ADDED: %t\n\n%i" :empty-lines 1)
          ("wm" "Monday" entry (file "~/Dropbox/org/Monday.org.gpg")
           "* TODO %?\n  :PROPERTIES:\n  :orgtrello_users: cesarolea7\n  :END:\n" :empty-lines 1)
          ;; these are for mu4e configured below
          ("m" "Email Workflow")
          ("mf" "Follow Up" entry (file+olp "~/Dropbox/org/Mail.org.gpg" "Follow Up")
           "* TODO Follow up with %:fromname on %a %(org-set-tags \"mail\")\nADDED:%t\n\n%i"
           :immediate-finish t)
          ("mr" "Read Later" entry (file+olp "~/Dropbox/org/Mail.org.gpg" "Read Later")
           "* TODO Read %a from %:fromname %(org-set-tags \"mail\")\nADDED:%t\n\n%i"
           :immediate-finish t)
          ("ms" "Schedule" entry (file+olp "~/Dropbox/org/Mail.org.gpg" "Schedule")
           "* TODO Follow up with %:fromname on %a %(org-set-tags \"mail\")\nSCHEDULED:%t DEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%i"
           :immediate-finish t)
          ;; these are for contacts
          ("c" "Contacts")
          ("cw" "Work" entry (file+olp "~/Dropbox/org/contacts.org.gpg" "LoanPro")
           "* %^{Name}
:PROPERTIES:
:EMAIL: %^{EMAIL}
:GROUP: Work
:COMPANY: %^{COMPANY}
:MESSAGES: [[mu4e:query:from:%\\2 AND flag:unread AND NOT flag:trashed][Unread emails]], [[mu4e:query:from:%\\2 AND NOT flag:trashed][All emails]]
:END:" :empty-lines 1 :immediate-finish t)
          ("cp" "Personal" entry (file+olp "~/Dropbox/org/contacts.org.gpg" "Personal")
           "* %^{Name}
:PROPERTIES:
:EMAIL: %^{EMAIL}
:GROUP: %^{GROUP}
:MESSAGES: [[mu4e:query:from:%\\2 AND flag:unread AND NOT flag:trashed][Unread emails]]
:END:" :empty-lines 1 :immediate-finish t)
          ;; these are for org-drill
          ("d" "Drill")
          ("dv" "Vocabolario" entry (file+olp "~/Dropbox/org/italiano.org.gpg" "Vocabolario")
           "** Parole   :drill:
   :PROPERTIES:
   :DRILL_CARD_TYPE: twosided
   :ADDED: %U
   :END:

   Tradurre la parola.

*** Italiano

    %^{Parole}

*** Español

    %^{Palabra}

*** Frasi di esempli

    %?" :empty-lines 1 :immediate-finish t)))

  ;; Agenda list
  ;; Ignores TODO items with a scheduled and/or deadline date that
  ;; occur in the future. The idea is that those items have already
  ;; been "taken care of" until it's time to start working on them
  (setq org-agenda-todo-ignore-scheduled  'future
        org-agenda-todo-ignore-deadlines  'future
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done  t)

  ;; Save underlying org files when changing status of agenda items
  (defmacro η (fnc)
    "Return function that ignores its arguments and invokes FNC."
    `(lambda (&rest _rest)
       (funcall ,fnc)))

  (advice-add 'org-deadline       :after (η #'org-save-all-org-buffers))
  (advice-add 'org-schedule       :after (η #'org-save-all-org-buffers))
  (advice-add 'org-store-log-note :after (η #'org-save-all-org-buffers))
  (advice-add 'org-todo           :after (η #'org-save-all-org-buffers))

  ;; for habits, when marking as done the buffer still changes
  ;; this fixes it
  (add-hook 'org-trigger-hook 'save-buffer)

  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  ;; modules
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit t)

  ;; where to put captured notes
  (setq org-default-notes-file "~/Dropbox/org/refile.org"
        org-agenda-files '("~/Dropbox/org/life.org.gpg"
                           "~/Dropbox/org/Mail.org.gpg"
                           "~/Dropbox/org/Monday.org.gpg"
                           "~/Dropbox/org/work.org.gpg"
                           "~/Dropbox/org/weekly-check-in.org.gpg")
        ;; so that you can refile to any file tracked by agenda
        ;; plus a nesting level of 3
        org-refile-targets '((org-agenda-files :maxlevel . 3))
        org-directory "~/Sync/Org"
        org-ellipsis " ↷" ;; other option is ↴

        ;; capture timestamps and notes when TODO state
        ;; changes to DONE
        org-log-done t

        ;; show plain text links by default
        ;; org-descriptive-links nil

        ;; when clocking time for tasks, persist history across
        ;; emacs sessions. Used together with
        ;; (org-clock-persistence-insinuate)
        org-clock-persist 'history

        ;; Default is nil. Source code is indented. This indentation
        ;; applies during export or tangling, and depending on the
        ;; context, may alter leading spaces and tabs. When non-nil,
        ;; source code is aligned with the leftmost column. No lines
        ;; are modified during export or tangling, which is very
        ;; useful for white-space sensitive languages, such as Python.
        ;;
        ;; Local variables can be used to set this to true on specific
        ;; buffers only:
        ;; M-x add-file-local-variable RET org-src-preserve-indentation RET t
        ;; and press C-c on the header arguments
        org-src-preserve-indentation nil
        org-edit-src-content-indentation 0

        ;; preserve native color scheme for target source code
        org-src-fontify-natively t

        org-src-tab-acts-natively t

        org-startup-indented t

        org-hide-leading-stars t

        ;; smart quotes on export
        org-export-with-smart-quotes t

        org-habit-show-all-today nil

        ;; hides * for bold, / for italics, etc
        org-hide-emphasis-markers t

        ;; so when hitting enter after a heading, it keeps proper indentation
        org-adapt-indentation t

        org-tag-alist-for-agenda '(("work"     . ?w)
                                   ("personal" . ?p)
                                   ("home"     . ?h)
                                   ("sp"       . ?s)
                                   ("mail"     . ?m)
                                   ("loanpro"  . ?l)
                                   ("cto"      . ?c))
        org-tag-alist '(("work"     . ?w)
                        ("personal" . ?p)
                        ("home"     . ?h)
                        ("sp"       . ?s)
                        ("mail"     . ?m)
                        ("loanpro"  . ?l)
                        ("cto"      . ?c)
                        ("drill"    . ?d))
        )

  (add-hook 'org-mode-hook (lambda ()
                             (flyspell-mode 1)
                             (hl-line-mode 1)
                             (auto-fill-mode 0)
                             (electric-pair-mode 1)
                             (visual-line-mode 0)
                             (toggle-truncate-lines 1)
                             (variable-pitch-mode 1)
                             (delete '("\\.pdf\\'" . default) org-file-apps)
                             (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s"))))

  (defun set-exec-path-from-shell-PATH ()
    (let ((path-from-shell
           (replace-regexp-in-string "[[:space:]\n]*$" ""
                                     (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
      (setenv "PATH" path-from-shell)
      (setq exec-path (split-string path-from-shell path-separator))))
  (when (equal system-type 'darwin) (set-exec-path-from-shell-PATH))

  ;; see org-clock-persist above
  (org-clock-persistence-insinuate)

  ;; exporters
  (require 'ox-md)          ; markdown
  ;; (require 'ox-reveal)      ; nice presentations
  ;; (require 'ox-hugo)        ; blogging
  (require 'ox-koma-letter) ; letters

  (with-eval-after-load 'ox-latex
    (add-to-list 'org-latex-classes
                 '("org-plain-latex" "\\documentclass{article}
                [NO-DEFAULT-PACKAGES]
                [PACKAGES]
                [EXTRA]"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subpparagraph{%s}" . "\\subparagraph*{%s}"))))
  )

(org-babel-load-file (concat user-emacs-directory "README.org"))
(put 'narrow-to-region 'disabled nil)
