(load-theme 'tango-dark)

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			 ("melpa" . "http://melpa.org/packages/")
			 ))
;; ("marmalade" . "http://marmalade-repo.org/packages/")

(package-initialize)

(defun require-package (package)
  (setq-default highlight-tabs t)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

;; Most important
(require-package 'evil)
(evil-mode 1)

(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

(require-package 'evil-magit)

(define-key evil-normal-state-map (kbd ";") 'evil-ex)
(define-key evil-normal-state-map (kbd ":") 'evil-repeat-find-char)

(define-key evil-visual-state-map (kbd ";") 'evil-ex)
(define-key evil-visual-state-map (kbd ":") 'evil-repeat-find-char)
(define-key evil-normal-state-map (kbd "zj") 'evil-insert-line-below)

(defun evil-insert-line-below (n)
  (interactive "sVal: ")
  "Insert blank line below"
  (evil-open-below n)
  )

(defun evil-insert-line-above ()
  (interactive "")
  "Insert blank line above"
  (internal-show-cursor nil nil)
  (evil-open-above 1)
  (evil-normal-state)
  (evil-next-line)
  (internal-show-cursor nil 1)
  )


(defun evil-insert-line-below ()
  (interactive "")
  "Insert blank line below"
  (internal-show-cursor nil nil)
  (evil-open-below 1)
  (evil-normal-state)
  (evil-previous-line)
  (internal-show-cursor nil 1)
  )

(define-key evil-normal-state-map (kbd "zj") 'evil-insert-line-below)
(define-key evil-normal-state-map (kbd "zk") 'evil-insert-line-above)

(define-key evil-normal-state-map (kbd "'") 'evil-goto-mark)
(define-key evil-normal-state-map (kbd "`") 'evil-goto-mark-line)
(define-key evil-visual-state-map (kbd "'") 'evil-goto-mark)
(define-key evil-visual-state-map (kbd "`") 'evil-goto-mark-line)

(require 'org-agenda)
(global-set-key "\C-ca" 'org-agenda)
;; (global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)


;; Almost as important
(require-package 'org)
(require-package 'evil-org)

;; Other evil-tweaks
(require-package 'evil-surround)
(global-evil-surround-mode 1)

;; Get out of insert mode with kj
(require-package 'evil-escape)
(evil-escape-mode 1)
(setq-default evil-escape-key-sequence "kj")

;; (require-package 'evil-numbers)
;; (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
;; (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)



;; Other important org-things
(require-package 'org-trello)
;; org-trello major mode for all .trello files
(add-to-list 'auto-mode-alist '("\\.trello$" . org-mode))

;; add a hook function to check if this is trello file, then activate the org-trello minor mode.
(add-hook 'org-mode-hook
	  (lambda ()
	    (let ((filename (buffer-file-name (current-buffer))))
	      (when (and filename (string= "trello" (file-name-extension filename)))
		(org-trello-mode)))))

;; (require-package 'org-pomodoro)

(define-key global-map "\C-cl" 'org-store-link)

;; Set to the location of your Org files on your local system
(setq org-directory "~/Dropbox/org")
;; Set to the name of the file where new notes will be stored
;; (setq org-mobile-inbox-for-pull "~/Dropbox/org/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
;; (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")


(require-package 'yasnippet)
(yas-global-mode 1)

(add-to-list 'yas-snippet-dirs "/Users/zingbretsen/.emacs.d/yasnippet-snippets")


;; (require 'xclip)
;; (xclip-mode 1)

(define-key org-agenda-mode-map "j" 'evil-next-line)
(define-key org-agenda-mode-map "k" 'evil-previous-line)
(define-key org-agenda-mode-map "G" 'evil-goto-line )
;(define-key org-agenda-mode-map "gg" 'evil-goto-first-line)

(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t)


;; helm settings (TAB in helm window for actions over selected items,
;; C-SPC to select items)
(require-package 'helm)
(require 'helm-config)
(require 'helm-misc)

(require-package 'projectile)
(projectile-global-mode)

(require-package 'helm-projectile)
(require-package 'helm-chrome)
(require-package 'helm-firefox)
(require-package 'helm-fuzzier)
(require-package 'helm-google)
(require-package 'helm-gtags)
(require-package 'helm-package)
(require-package 'helm-swoop)
(require-package 'helm-css-scss)
(require 'helm-locate)
(setq helm-quick-update t)
;; (setq helm-bookmark-show-location t)
(setq helm-buffers-fuzzy-matching t)

(global-set-key (kbd "M-x") 'helm-M-x)

(defun helm-my-buffers ()
  (interactive)
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm-other-buffer '(helm-c-source-buffers-list
			 helm-c-source-elscreen
			 helm-c-source-projectile-files-list
			 helm-c-source-ctags
			 helm-c-source-recentf
			 helm-c-source-locate)
		       "*helm-my-buffers*")))

(require-package 'spotify)


(setq org-agenda-include-diary t)


;; (require-package 'flycheck)
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; (autoload 'php-mode "php-mode" "Major mode for editing php code." t)
;; (add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
;; (add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))


					;(require-package 'spray)
					;(require-package 'pandoc-mode)
;; (global-set-key (kbd "j") 'spray-slower)
;; (global-set-key (kbd "k") 'spray-faster)

(filesets-init)
(projectile-mode)

(require-package 'dired+)

(setq org-directory "~/Dropbox/org")
;; (setq org-default-notes-file "~/Dropbox/org/refile.org")
;; (define-key global-map "\C-cc" 'org-capture)

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
;; (setq org-capture-templates
;;       (quote (("t" "todo" entry (file "~/Dropbox/org/refile.org")
;;                "* TODO %?\n%U\n%a\n" )
;;               ("r" "respond" entry (file "~/Dropbox/org/refile.org")
;;                "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" )
;;               ("n" "note" entry (file "~/Dropbox/org/refile.org")
;;                "* %? :NOTE:\n%U\n%a\n" )
;;               ("j" "Journal" entry (file+datetree "~/Dropbox/org/diary.org")
;;                "* %?\n%U\n" )
;;               ("m" "Meeting" entry (file "~/Dropbox/org/refile.org")
;;                "* MEETING with %? :MEETING:\n%U" )
;;               ("p" "Phone call" entry (file "~/Dropbox/org/refile.org")
;;                "* PHONE %? :PHONE:\n%U" )
;;               )))


;; ; Targets include this file and any file contributing to the agenda - up to 9 levels deep
;; (setq org-refile-targets (quote ((nil :maxlevel . 9)
;;                                  (org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets - we file directly with IDO
;; (setq org-refile-use-outline-path t)

;; ; Targets complete directly with IDO
;; (setq org-outline-path-complete-in-steps nil)

;; ; Allow refile to create parent tasks with confirmation
;; (setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))
; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

;;;; Refile settings
; Exclude DONE state tasks from refile targets
;; (defun bh/verify-refile-target ()
;;   "Exclude todo keywords with a done state from refile targets"
;;   (not (member (nth 2 (org-heading-components)) org-done-keywords)))

;; (setq org-refile-target-verify-function 'bh/verify-refile-target)

;; (require 'ess-site)
(setq initial-buffer-choice "~/Dropbox/org/trello.trello")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (wombat)))
 '(elpy-rpc-python-command "/Users/zingbretsen/anaconda/bin/python")
 '(elpy-syntax-check-command "/Users/zingbretsen/anaconda/bin/flake8")
 '(initial-buffer-choice "~/Dropbox/org/trello.trello")
 '(org-agenda-files
   (quote
    ("~/Dropbox/org/home.org" "~/Dropbox/org/learn.org" "~/Dropbox/org/veronica.trello" "~/Dropbox/org/trello.trello")))
 '(org-trello-current-prefix-keybinding "C-c o" nil (org-trello))
 '(python-shell-interpreter "/Users/zingbretsen/anaconda/bin/ipython")
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.gmail.com" t)
 '(smtpmail-smtp-service 25 t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; (require-package 'polymode)
;; (require 'poly-R)
;; (require 'poly-markdown)
;; (require-package 'r-autoyas)
;; (add-hook 'ess-mode-hook 'r-autoyas-ess-activate)

(add-to-list 'load-path "~/Documents/software/emacs-ipython-notebook-0.12.1")
(require-package 'ein)

(require-package 'auto-complete)
(require-package 'smartrep)
(setq ein:use-auto-complete t)
(setq ein:use-smartrep t)

;;;;;;;;;;;;;;;;;;;;;
;; New for testing ;;
;;;;;;;;;;;;;;;;;;;;;
;; (require-package 'auto-yasnippet)

;;Use gcc to comment out a line (takes a count), gc to comment out the target of a motion (for example, gcap to comment out a paragraph), gc in visual mode to comment out the selection.
(require-package 'evil-commentary)

;; Use gx motion; gx motion
;; (or visual)
(require-package 'evil-exchange)
(evil-exchange-install)

;; (require-package 'evil-cleverparens)
(require-package 'evil-numbers)

(defhydra hydra-increment (global-map "C-a")
  "increment/decrement number"
  ("k" evil-numbers/inc-at-pt  "up")
  ("j" evil-numbers/dec-at-pt "down"))

(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("k" text-scale-increase "in")
  ("j" text-scale-decrease "out"))

(require-package 'evil-leader)
(global-evil-leader-mode)
(setq evil-leader/in-all-states 1)
(evil-leader/set-leader ",")

(evil-leader/set-key
  "d" 'drink
  "e" 'find-file
  "f" 'find-file
  "b" 'switch-to-buffer
  "k" 'kill-buffer
  "m" 'magit-status
  "n" 'linum-mode
  "c" 'comment-or-uncomment-region
  ;; "p" 'spotify-playpause
  ;; "n" 'spotify-next
  "j" 'org-journal-new-entry
  "a" 'align-regexp
  "g" 'green-tea
  "t" 'black-tea
  "v" 'visual-line-mode
  "x" 'helm-M-x
  "," 'evil-normal-state
  )

;;Testing
(require-package 'json)
(defvar logurl "https://zingbretsen.com/health/log.php")
(defun zingquery (key val)
      (let ((url-request-method "GET")
	    ))
        (url-retrieve (concat
	logurl
	(concat "?data={%22" key "%22:")
	(concat "%22" val "%22}")
	)
		      (lambda (status) ())))

(defun log-health (key val)
  "Interactively log to health DB"
  (interactive "sKey: \nsVal: ")
  (zingquery key val)
  )

;; (require-package 'fold-dwim)
;; (require-package 'hideshow-org)
(defvar terminal-notifier-command '/usr/local/bin/terminal-notifier)

(defun sayit (msg)
  "say a thing"
  (interactive "sSay a thing")
  (start-process "say" "*say*" "/usr/bin/say" msg)
  )
(start-process "terminal-notifier" "terminal-notifier" "/usr/local/bin/terminal-notifier" "-message hi")

(defun timed-notification(time msg)
  (interactive "sNotification when (e.g: 2 minutes, 60 seconds, 3 days): \nsMessage: ")
  (run-at-time time nil (lambda (msg) (sayit  msg)) msg))

(defun black-tea ()
  "4 minute timer"
  (interactive)
   (timed-notification "4 minutes" "Tea time!")
   (zingquery "Drink" "Black Tea")
   )

(defun green-tea ()
  "1.5 minute timer"
  (interactive)
  (timed-notification "1.5 minutes" "Tea time!")
  (zingquery "Drink" "Green Tea")
  )

(defun drink (drink)
  "Log drink to health DB"
  (interactive "sDrink: ")
  (zingquery "Drink" drink)
  )

;; (setq
;;  send-mail-function 'smtpmail-send-it
;;  message-send-mail-function 'smtpmail-send-it
;;  user-mail-address "zach.ingbretsen@gmail.com"
;;  smtpmail-starttls-credentials '(("smtp.gmail.com" "587" nil nil))
;;  smtpmail-auth-credentials (expand-file-name "~/.authinfo")
;;  smtpmail-default-smtp-server "smtp.gmail.com"
;;  smtpmail-smtp-server "smtp.gmail.com"
;;  smtpmail-smtp-service 587
;;  smtpmail-debug-info t
;;  starttls-extra-arguments nil
;;  starttls-gnutls-program "/usr/local/bin/gnutls-cli"
;;  starttls-extra-arguments nil
;;  starttls-use-gnutls t
;;  )

;;(require 'org-drill)

(defun fix-special-chars ()
  (interactive)
  (replace-string "’" "'")
  (replace-string "“" "\"")
  (replace-string "”" "\"")
  (replace-string "é" "&eacute;")
  (replace-string "—" "-")
  )

;; Testing

;; (package-initialize)
;; (elpy-enable)

(add-hook 'js-mode-hook 'linum-mode)
(require-package 'doremi-cmd)
(require-package 'doremi-frm)

;; To run shell inside emacs
(require-package 'multi-term)
(setq multi-term-program "/usr/local/bin/zsh")

;(require-package 'smartparens)
;(require-package 'smartparens-config)
;(add-hook 'js-mode-hook #'smartparens-mode)
