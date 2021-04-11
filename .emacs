;; -*- mode: elisp -*-

;; Disable the splash screen (to enable it again, replace the t with 0)
(setq inhibit-splash-screen t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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
 '(org-agenda-files
   (quote
    ("~/org-roam/20210117124139-home.org" "~/org/notes.org")))
 '(org-capture-templates
   (quote
    (("d" "Doc" entry
      (file+headline org-default-notes-file "Docs")
      "* [[%c][Doc:%?]]")
     ("j" "Journal Entry" entry
      (file+olp+datetree "~/journal.org")
      "* %?" :empty-lines 1)
     ("l" "Link" entry
      (file+headline org-default-notes-file "Links")
      "* [[%c][%?]]")
     ("n" "Note" item
      (file+headline org-default-notes-file "Notes")
      "")
     ("t" "Task" entry
      (file+headline org-default-notes-file "Tasks")
      "* TODO %?")
     ("a" "Article" entry
      (file+headline org-default-notes-file "Articles")
      "* %?%i"))))
 '(org-refile-targets (quote ((org-agenda-files :maxlevel . 6))))
 '(package-selected-packages (quote (which-key org-roam))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Org Capture
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
		(lambda () (interactive) (find-file "~/org/notes.org")))

(global-set-key (kbd "C-c e")
		(lambda () (interactive) (find-file "~/.emacs"01)))

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

;; ;; Blind writer program -- Version 1.0
;; (defun blind-writer ()
;;   "Activates blind writing for current buffer"
;;   (interactive)
;;   (let ((targetBuffer (current-buffer))
;; 	(input "")
;; 	(blindBuffer (generate-new-buffer "Blind Buffer")))
;;     (switch-to-buffer blindBuffer)
;;     (while (not (string= input ":eof"))
;;       (setq input (read-string ": "))
;;       (if (not (string= input ":eof"))
;; 	  (if (string= input ":eop")
;; 	      (progn
;; 		(set-buffer targetBuffer)
;; 		(insert-before-markers "\n\n")
;; 		(set-buffer blindBuffer))
;; 	    (progn
;; 	      (erase-buffer)
;; 	      (insert input)
;; 	      (set-buffer targetBuffer)
;; 	      (insert-before-markers input " ")
;; 	      (set-buffer blindBuffer)))))
;;     (switch-to-buffer targetBuffer)
;;     (kill-buffer blindBuffer)))

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
(setq org-hide-leading-stars t)

;; Enable line numbers globally
;;(setq global-linum-mode nil)

;; Enable line numbers in Python mode
(add-hook 'python-mode-hook 'linum-mode)

;; Desktop save mode disable
(desktop-save-mode 0)

;; Disable scratch buffer message
(setq initial-scratch-message nil)
