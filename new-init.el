;; Automatically reread from disk if the underlying file changes
(savehist-mode)
(add-to-list 'image-types 'svg)
;;Use the minibuffer while in the minibuffer
;;; (setq enable-recursive-minibuffers t)
;;Tab cycles through completion candidates
;; (setq completion-cycle-threshold 3)
;;Show annotations
;; (setq completions-detailed t)
;;Try to complete when you hit tab, otherwise indent
;; (setq tab-always-indent 'complete)

;;Show completions in a vertical interactive list
;; (fido-vertical-mode 1)
;;Different styles to match input on completions
;; (setq completion-styles '(basic initial substring))
;;Tab acts like it does in the console
;; (define-key minibuffer-mode-map (kbd "TAB") 'minibuffer-complete)

;;Mode line information
;;(setq x-underline-at-descent-line nil)
(setq switch-to-buffer-obey-display-actions t)
(setq-default show-trailing-whitespace nil)
(setq-default indicate-buffer-boundaries 'left)

(add-function :after after-focus-change-function
	      (lambda ()
		(unless (frame-focus-state)
		  (garbage-collect))))

(blink-cursor-mode -1)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;; (setq tab-bar-show 0)

;; Add the time to the tab-bar, if visible
;;(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
;;(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(setq display-time-format "%a %F %T")
(setq display-time-interval 1)
(display-time-mode)

(load-theme 'modus-vivendi)
;; (load-theme 'deeper-blue t)
;;(unless (package-installed-p 'use-package)
(package-refresh-contents)
(package-install 'use-package)

(eval-when-compile (require 'package))
(setq use-package-always-ensure t)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("elpa" . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(unless package-archive-contents
  (package-refresh-contents))

(use-package emacs
  :bind
  ("C-w" . backward-kill-word)
  ("C-x C-k" . kill-region)
  ("C-c C-k" . kill-region)
  ("C-x r" . query-replace-regexp)
  ("C-x l" . goto-line)
  ("C-c b" . bury-buffer)
  ("C-c f" . revert-buffer)
  ("C-x f" . find-file)
  ("C-M-l" . execute-extended-command)
  ("C-M-j" . switch-to-buffer)
  ("C-;" . comment-line)
  :config
  (setq inhibit-startup-message t)
  (setq user-full-name "Jeff Gonis")
  (setq user-mail-address "jeffgonis@fastmail.com")
  (setq-default truncate-lines t)
  (setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
	backup-by-copying t
	version-control   t
	delete-old-versions t
	kept-new-versions 20
	kept-old-versions 5)
  (fset 'yes-or-no-p 'y-or-n-p)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (show-paren-mode 1)
  (setq line-number-mode t)
  (setq column-number-mode t)
  (setq auto-revert-interval 3)
  (setq auto-revert-check-vc-info t)
  (global-auto-revert-mode)
  (global-subword-mode)
  (global-prettify-symbols-mode)
  (global-hl-line-mode)
  (global-unset-key "\C-z")
  (set-charset-priority 'unicode)
  (set-default-coding-systems 'utf-8)
  (recentf-mode t)
  (setq create-lockfiles nil)
  ;; (setq scheme-program-name "/Users/jeffgonis/code/gauche/gauche/bin/gosh -i -r7 -I /Users/jeffgonis/code/sicp/concabs")
  (setq scheme-program-name "/Users/jeffgonis/code/gauche/gauche/bin/gosh -i -r7 -I /Users/jeffgonis/code/sicp/seasonedSchemer")
  )


(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))

(use-package dired
  :config
  (setq dired-create-destination-dirs 'ask
	dired-kill-when-opening-new-dired-buffer t
	dired-do-revert-buffer t
	dired-mark-region t))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :after (dired all-the-icons)
  :hook (dired-mode . all-the-icons-dired-mode))
(use-package all-the-icons-completion
  :after (all-the-icons dired)
  :hook (dired-mode . all-the-icons-dired-mode)
  :init
  (all-the-icons-completion-mode))

(use-package doom-themes
  :init (load-theme 'doom-opera t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package paredit
  :commands (paredit-mode)
  :hook (emacs-lisp-mode . paredit-mode)
  :hook (lisp-mode . paredit-mode)
  ;; :hook (sly-mrepl-mode . paredit-mode)
  :hook (lisp-interaction-mode . paredit-mode))

(use-package yaml-mode
  :commands (yaml-mode)
  :init
  (add-hook 'yaml-mode-hook 'flycheck-mode)
  (add-hook 'yaml-mode-hook 'flyspell-prog-mode))

(use-package flycheck-yamllint
  :after (yaml-mode)
  :commands (yaml-mode)
  :init
  (add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))

(use-package json-mode
  :commands (json-mode)
  :init
  (add-hook 'json-mode-hook #'flycheck-mode))

(use-package ibuffer
  :commands (ibuffer)
  :init
  (bind-key "C-x C-b" 'ibuffer))

(use-package ace-window
  :bind ("C-x o" . ace-window)
  :config
  (ace-window-display-mode))

(use-package flyspell
  :hook (text-mode . turn-on-flyspell)
  :init
  (bind-key "C-c cw" 'flyspell-auto-correct-word)
  (setq ispell-program-name "aspell"
	ispell-dictionary "en_CA"
	flyspell-prog-text-faces '(font-lock-comment-face font-lock-doc-face)))

(use-package rg
  :init
  (rg-enable-default-bindings)
  (setq rg-command-lie-flags '("--hidden")))

(use-package vterm)

(use-package magit
  :commands (magit-status magit-dispatch-popup)
  :bind (("C-x g" . magit-status)
	 ("C-x M-g" . magit-dispatch-popup))
  :hook (git-commit-setup . git-commit-turn-on-flyspell)
  :config
  (setq magit-save-repository-buffers 'dontask
	magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1
	git-commit-major-mode 'git-commit-mode))

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
	ediff-split-window-function 'split-window-horizontally))

(use-package windmove
  :init
  (windmove-default-keybindings))

(use-package helpful
  :after (counsel)
  :custom (counsel-describe-variable-function #'helpful-variable)
  :bind
  ("C-h f" . helpful-function)
  ("C-c C-d" . helpful-at-point)
  ([remap descibe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package format-all
  :init
  :hook ((prog-mode . format-all-mode)
	 (prog-mode . format-all-ensure-formatter)
	 (yaml-mode . format-all-mode)
	 (yaml-mode . format-all-ensure-formatter)))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package sly)
(use-package sly-asdf
  :after (sly))
(use-package sly-quicklisp
  :after (sly))
(use-package sly-repl-ansi-color
  :after (sly))

(with-eval-after-load 'sly
  (require 'sly-quicklisp)
  (require 'sly-repl-ansi-color)
  (require 'sly-asdf))

;; (use-package geiser)
;; (use-package geiser-gauche
;;   :after geiser
;;   :init (add-to-list 'geiser-active-implementations 'gauche))
(use-package quack)

(require 'speedbar)
(setq-default speedbar-update-flag t)
(setq-default speedbar-use-images nil)
(setq-default speedbar-frame-parameters
              '((name . "speedbar")
                (title . "speedbar")
                (minibuffer . nil)
                (border-width . 2)
                (menu-bar-lines . 0)
                (tool-bar-lines . 0)
                (unsplittable . t)
                (left-fringe . 10)))

(use-package eglot)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-switch-buffer-kill))
  :init
  (ivy-mode 1)
  ;; (ivy-mode 1))
  )

(use-package swiper
  :after (ivy))

(use-package counsel
  :after (ivy)
  :hook (after-init . counsel-mode)
  :bind (("M-x" . counsel-M-x)
	 ("C-M-l" . counsel-M-x)
	 ("C-M-j" . counsel-switch-buffer)
	 ("C-x C-f" . counsel-find-file))
  :init (progn
	  (setq ivy-initial-inputs-alist nil)
	  (counsel-mode)))

(use-package ivy-prescient
  :after (ivy counsel)
  :init
  (ivy-prescient-mode 1))

(use-package ivy-rich
  :after (ivy counsel)
  :init (ivy-rich-mode 1))

(use-package all-the-icons-ivy
  :after  (ivy counsel))

(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode)
  (setq company-minimum-prefix-length 3
	company-selection-wraparound t
	company-tooltip-minimum-width 15
	company-tooltip-align-annotations t
	company-idle-delay 0.5))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package olivetti)

;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
;; (use-package slime)
;; (use-package slime-company
;; :after (slime company)
;; :config (setq slime-company-completion 'fuzzy
;; slime-company-after-completion 'slime-company-just-one-space))
;; (slime-setup '(slime-fancy slime-company))
(use-package lispy
  :hook ((emacs-lisp-mode . lispy-mode)
	 (scheme-mode . lispy-mode)))

(use-package neotree)
(neotree)

(use-package chess)

(use-package go-mode
  :mode "\\.go\\'")

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  ;; (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(setq inferior-lisp-program "/opt/homebrew/bin/sbcl")
;; (setq geiser-gauche-binary "/Users/jeffgonis/code/gauche/bin/gosh")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(connection-local-criteria-alist
   '(((:application tramp :machine "localhost")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp :machine "jeffgonis-mba")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))))
 '(custom-safe-themes
   '("da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350" "90a6f96a4665a6a56e36dec873a15cbedf761c51ec08dd993d6604e32dd45940" "f149d9986497e8877e0bd1981d1bef8c8a6d35be7d82cba193ad7e46f0989f6a" default))
 '(display-time-mode t)
 '(fill-column 80)
 '(neo-window-fixed-size nil)
 '(neo-window-width 35)
 '(package-selected-packages
   '(kubernetes-helm which-key go-mode chess auto-package-update olivetti all-the-icons use-package))
 '(quack-fontify-style 'emacs)
 '(quack-programs
   '("/Users/jeffgonis/code/gauche/gauche/bin/gosh -i -r7 -I /Users/jeffgonis/code/sicp/seasonedSchemer" "/Users/jeffgonis/code/gauche/bin/gosh -i -r7 -I /Users/jeffgonis/code/sicp/concabs" "/Users/jeffgonis/code/gauche/gauche/bin/gosh -i -r7 -I /Users/jeffgonis/code/sicp/concabs" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi"))
 '(tool-bar-mode nil))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :extend nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :foundry "nil" :family "Roboto Mono")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Roboto Mono" :foundry "nil" :slant normal :weight regular :height 140 :width normal)))))
