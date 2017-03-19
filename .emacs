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

(require-package 'yasnippet)
(yas-global-mode 1)

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


;; Use gx motion; gx motion
;; (or visual)
(require-package 'evil-exchange)
(evil-exchange-install)

;;;;;;;;;;;;;;;;;;;;;
;; New for testing ;;
;;;;;;;;;;;;;;;;;;;;;

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


;;Use gcc to comment out a line (takes a count), gc to comment out the target of a motion (for example, gcap to comment out a paragraph), gc in visual mode to comment out the selection.
(require-package 'evil-commentary)

;; (require-package 'evil-cleverparens)
(require-package 'evil-numbers)

(require-package 'hydra)
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
  "e" 'find-file
  "f" 'find-file
  "b" 'switch-to-buffer
  "k" 'kill-buffer
  "m" 'magit-status
  "n" 'linum-mode
  "c" 'comment-or-uncomment-region
  "a" 'align-regexp
  "v" 'visual-line-mode
  "x" 'helm-M-x
  "," 'evil-normal-state
  )

(defun fix-special-chars ()
  (interactive)
  (replace-string "’" "'")
  (replace-string "“" "\"")
  (replace-string "”" "\"")
  (replace-string "é" "&eacute;")
  (replace-string "—" "-")
  )

(add-hook 'js-mode-hook 'linum-mode)
(require-package 'doremi-cmd)
(require-package 'doremi-frm)
