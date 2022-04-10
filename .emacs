;; -*- mode: elisp -*-

;; Disable the splash screen (to enable it again, replace the t with 0)
(setq inhibit-splash-screen t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

;; Disable toolbar
(tool-bar-mode -1)

;; Disable menu bar
(menu-bar-mode -1)

;; Disable scroll bar
(scroll-bar-mode -1)

;; Enable line highlight
(global-hl-line-mode 0)

;; Enable line number
(line-number-mode t)

;; Enable transient mark mode
(transient-mark-mode 1)

;; Return follows link
(setq org-return-follows-link 1)

;; TODO Keywords
(setq org-todo-keywords
      '((sequence "NEXT(n)" "TODO(t)" "|" "DONE(d)")
	(sequence "CHCK(c)" "WAIT(w)" "|" "CNCL(x)")))

;; TODO keyword colors
(setq org-todo-keywords-faces
      '(("NEXT" . "cornflower blue") ("WAIT" . "dark goldenrod") ("CHCK" . "dark goldenrod")))

;; Disable lisp evaluation, enable C++ evaluation
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . nil)
   (C . t)))

;; "Activation" from orgmode Introduction section of The Guide
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;;;;Org mode configuration
;; Enable Org mode
(require 'org)
;; Make Org mode work with files ending in .org
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; The above is the default in recent emacsen
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files '("C:/Users/Hunter/Dropbox/org"))
 '(org-capture-templates
   '(("d" "Doc" entry
      (file+headline org-default-notes-file "Docs")
      "* [[%c][Doc:%?]]")
     ("j" "Journal Entry" entry
      (file+olp+datetree "~/org/journal.org")
      "* %?" :empty-lines 1)
     ("l" "Link" entry
      (file+headline org-default-notes-file "Links")
      "* [[%c][%?]]")
     ("n" "Note" item
      (file+headline org-default-notes-file "Notes")
      "")
     ("t" "Task" entry
      (file+headline org-default-notes-file "Inbox")
      "* TODO %?")
     ("a" "Article" entry
      (file+headline org-default-notes-file "Articles")
      "* %?%i")))
 '(org-refile-targets '((org-agenda-files :maxlevel . 6)))
 '(package-selected-packages '(impatient-mode which-key org-roam)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Org Capture
(setq org-directory "C:/Users/Hunter/Dropbox/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))

;; Word wrap functionality
(global-visual-line-mode t)

;; Enable C-z undo
(global-unset-key "\C-z")
(global-set-key (kbd "C-z")
		(lambda () (interactive) (undo)))

;; 'C-x o' alias 'C-<tab>'
(global-set-key (kbd "C-<tab>")
		(lambda () (interactive) (other-window 1)))

;; Enable C-return eval-print-last-sexp
(global-set-key (kbd "<C-return>") 'eval-last-sexp)

;; Make frame fullscreen (disabled)
;(toggle-frame-maximized)

;; Org file navigation shortcuts
(global-set-key (kbd "C-c h")
		(lambda () (interactive) (org-roam-find-file "Home" nil nil t)))

(global-set-key (kbd "C-c j")
		(lambda () (interactive) (find-file "~/journal.org")))

(global-set-key (kbd "C-c n")
		(lambda () (interactive) (find-file org-default-notes-file)))

(global-set-key (kbd "C-c e")
		(lambda () (interactive) (find-file "~/.emacs")))

(defun home ()
  (interactive)
  (org-roam-find-file "Home" nil nil t))

;; enable ido-mode
;(setq ido-enable-flex-matching t)
;(setq ido-everywhere t)
;(ido-mode 1)

;; [[https://stackoverflow.com/questions/17590784/how-to-let-org-mode-open-a-link-like-file-file-org-in-current-window-inste][stackoverflow]]
(defun org-force-open-current-window ()
  (interactive)
  (let ((org-link-frame-setup (quote
                               ((vm . vm-visit-folder)
                                (vm-imap . vm-visit-imap-folder)
                                (gnus . gnus)
                                (file . find-file)
                                (wl . wl)))
                              ))
    (org-open-at-point)))
;; Depending on universal argument try opening link
(defun org-open-maybe (&optional arg)
  (interactive "P")
  (if arg
      (org-open-at-point)
    (org-force-open-current-window)
    )
  )
;; Redefine file opening without clobbering universal argument
(define-key org-mode-map "\C-c\C-o" 'org-open-maybe)

;; Blind Writer Program -- Version 2.0
(defun blind-writer ()
  "Activates blind writing in current buffer"
  (interactive)
  (defun undo () (insert (pop history)))
  (defun write ()
    (progn
      (push input history)
      (set-buffer blindBuffer)
      (erase-buffer)
      (insert input)
      (set-buffer targetBuffer)
      (insert-before-markers input " ")
      (set-buffer blindBuffer)))
  (let ((targetBuffer (current-buffer))
	(blindBuffer (generate-new-buffer "Blind Buffer"))
	(input "")
	(history ()))
    (switch-to-buffer blindBuffer)
    (with-current-buffer blindBuffer
      (visual-line-mode t))
    (while (not (string= input ":eof"))
      (setq input (read-string ": "))
      (when (not (string= input ":eof"))
	(when (string= input ":eop")
	  (progn
	    (set-buffer targetBuffer)
	    (insert-before-markers "\n\n")
	    (set-buffer blindBuffer)))
	(when (string= input ":undo")
	  (when (not (eq history nil))
	    (minibuffer-with-setup-hook
		'undo
	      (progn
		(set-buffer targetBuffer)
		(delete-char (- 0 (length (nth 0 history)) 1))
		(set-buffer blindBuffer)
		(erase-buffer) ;; Comment these two lines
		(insert (nth 1 history)) ;; to remove undo display functionality
		(setq input (read-string ": "))))))
	(when (not (string= (substring input 0 1) ":"))
	  (write))))
    (switch-to-buffer targetBuffer)
    (kill-buffer blindBuffer)))

(show-paren-mode t)
(electric-pair-mode t)

;; Enables white space
(setq org-cycle-separator-lines 2)

;; Enable global star hiding in org mode
;; (setq org-hide-leading-stars t)

;; Enable line numbers globally
;;(setq global-linum-mode nil)

;; Enable line numbers in Python mode
(add-hook 'python-mode-hook 'linum-mode)

;; Desktop save mode disable
(desktop-save-mode 0)

;; Disable scratch buffer message
(setq initial-scratch-message nil)

;; Easy recompile
(global-set-key (kbd "C-;")
		(lambda () (interactive) (recompile)))

;; Add flyspell
;;(dolist (hook '(text-mode-hook))
;;  (add-hook hook (lambda () (flyspell-mode 1))))

;; Make Org link MACRO
(fset 'Make\ Org\ Link
      (kmacro-lambda-form [?\C-w ?\C-x ?\C-f ?\C-y ?. ?o ?r ?g return ?\C-x ?\C-s ?\C-x ?k return ?\C-c ?\C-l ?~ ?\\ ?o ?r ?g ?\\ ?\C-y ?. ?o ?r ?g return ?\C-y return] 0 "%d"))

;; Clock Settings
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; Org mode continuous clocking
(setq org-clock-continuously nil)
(setq org-clock-mode-line-total 'today)

;; Org mode TODO logging
; (setq org-log-done 'time) ;; Log time when TODO item marked DONE
;; (setq org-log-done 'note) '' Prompt for note when TODO item marked DONE
; (setq org-closed-keep-when-no-todo 't) ;; Keep time log when DONE tag removed

;; Save Macro
(defun save-macro (name)
  "save a macro. Take a name as argument
   and save save the last defined macro under
   this name under the end of your .emacs"
  (interactive "SName of the macro: ")
  (kmacro-name-last-macro name)
  (find-file user-init-file)
  (goto-char (point-max))
  (newline)
  (switch-to-buffer nil))

;; CUA Mode
(cua-mode t)

;; Melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Evil
;;(require 'evil)
;;(evil-mode 0)

;; Agenda Formats
(setq oap-format-a
      '((agenda . " %t")
	(todo   . " %?-12 s")
	(tags   . " ")
	(search . " %i %-12:c")))

(setq oap-format-b
      '((agenda . " %40b %t")
	(todo   . " %40b ")
	(tags   . " ")
	(search . " %i %-12:c")))

;; Breadcrumbs Format
(setq org-agenda-breadcrumbs-separator "/")

;; Dependency settings
(setq org-enforce-todo-dependencies t)
(setq org-agenda-dim-blocked-tasks 'invisible)

;; Breadcrumb Program
(setq org-agenda-prefix-format oap-format-a)
(defun breadcrumbs_toggle ()
  "Trigger agenda mode breadcrumbs."
  (interactive)
  (if (equal org-agenda-prefix-format oap-format-b)
      (progn
	(setq org-agenda-prefix-format
	      oap-format-a))
    (progn
      (setq org-agenda-prefix-format
	    oap-format-b)))
  (org-agenda-redo))
(add-hook 'org-agenda-mode-hook
	  (lambda () (local-set-key (key "C-c b") 'breadcrumbs_toggle)))

;; Only shows unscheduled tasks in org todo agenda
(setq org-agenda-todo-ignore-scheduled nil)
(defun toggle-ignore-scheduled ()
  "Toggles display of scheduled tasks in org-agenda todo display."
  (interactive)
  (setq org-agenda-todo-ignore-scheduled (if (euql org-agenda-todo-ignore-scheduled 'all) nil 'all))
  (org-agenda-redo))
(add-hook 'org-agenda-mode-hook
	  (lambda () (local-set-key (kbd "C-c s") 'toggle-ignore-scheduled)))

;; Note: org-file-apps controls what program opens attachments

;; Removing unused org-agenda-mode keybindings
(add-hook 'org-agenda-mode-hook
	  (lambda () (local-set-key (kbd "t") nil)))
(add-hook 'org-agenda-mode-hook
	  (lambda () (local-set-key (kbd "e") nil)))
(add-hook 'org-agenda-mode-hook
	  (lambda () (local-set-key (kbd "a") nil)))

(setq org-todo-repeat-to-state t)

;; Org Priorities
(setq org-priority-highest 1)
(setq org-priority-lowest 20)
(setq org-priority-default 18)

;; Removes priority upon task completion
;(defun remove-priority-when-complete ()
;  "Removes priority when task DONE"
;  (interactive)
;  (when (equal org-state "DONE")
;    (org-priority ?\s)))
;
;(add-hook 'org-after-todo-state-change-hook 'remove-priority-when-complete)

(defun org-set-content-level ()
  (interactive)
  (org-content 2))
(global-set-key (kbd "C-c r")
		(lambda () (interactive) (org-set-content-level)))

(fset 'agenda-todo-chck
      (kmacro-lambda-form [?= ?T ?O ?D ?O ?\\ ?| ?C ?H ?C ?K return] 0 "%d"))

(add-hook 'org-agenda-mode-hook
	  (lambda () (local-set-key (kbd "C-=") 'agenda-todo-chck)))

;; Ctrl-Tab to swap between windows
(global-set-key [C-S-tab] 
    (lambda ()
      (interactive)
      (other-window -1)))

;; r13 Shortcut
(defun r13 ()
  (interactive)
  (save-excursion
    (progn
      (beginning-of-line)
      (skip-chars-forward "\s*")
      (re-search-forward "TODO\s" (+ (point-marker) 5) t)
      (push-mark (point-marker))
      (end-of-line)
      (re-search-forward "\s\s" (end-of-line) t -1)
      (rot13-region (mark-marker) (point-marker))
      (pop-mark))))

;; Org formatting, star settings
(setq org-startup-indented t
      org-hide-leading-stars t)

;; Inline Images
(setq org-startup-with-inline-images t)

;; Aspell
(setq-default ispell-program-name "aspell")
