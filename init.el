;;Start of config
(setq inhibit-startup-message t)
(set-fringe-mode 10)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq visible-bell t)
;;(set-face-attribute 'default nil :font "Latin Modern Mono" :height 136)
(set-face-attribute 'default nil :font "Roboto Mono" :height 110)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;;Initialize package sources
(require 'package)
(setq package-archives '(("melpa" .
			  "https://melpa.org/packages/")
			 ("elpa" .
			  "http://elpa.gnu.org/packages/")
			 ("org" .
			  "https://orgmode.org/elpa/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;;Initialize use-package on non-linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;;Bring in use-package and make sure it always tries to bring
;;in the package specified by the use-package form.
(require 'use-package)
(setq use-package-always-ensure t)

;;Turn on column numbering mode in the modeline
(column-number-mode)
;;Turn on line numbers globally
(global-display-line-numbers-mode t)
;;Turn off line numbers for modes where it doesn't make sense
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook
  (add-hook mode (lambda () (display-line-numbers-mode 0))))))

;;Allow the diminish command in use-package to work
(use-package diminish)

;;Allow command logging to be installed and then turn it
;;on everywhere
(use-package  command-log-mode
  :diminish)
(global-command-log-mode)

;;Ivy provides autocomplete
(use-package ivy
  :diminish
  ;;Set bindings so we can navigate the buffer
  ;;autocomplete list using home row keys
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-switch-buffer-kill))
  :config
  (ivy-mode 1))

;;The doom-modeline package relies on icon fonts being
;;installed which are included in this all-the-icons
;;package. On a fresh install you also need to run
;;M-x all-the-icons-install-fonts to get the icos
;;fonts needed installed
(use-package all-the-icons)

;;Use a default configuration to get a more modern looking
;;emacs modeline installed
(use-package doom-modeline
  :config
  (doom-modeline-mode 1))

(use-package doom-themes
  :init (load-theme 'doom-opera t))

;Get multicolored parentheses and also make them match.
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
(show-paren-mode 1)

;;Pop up a minibuffer after you type in a key to show
;;what key commands are associated with. Ie type C-x
;;and which-key will show all the keys you could type
;;after and what commands they will execute.
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.5))

;Have Richer autocomplete information
(use-package ivy-rich
  :init (ivy-rich-mode 1))

;;Counsel is a set of enhanced commands for emacs
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-M-l" . counsel-M-x)
	 ("C-M-j" . counsel-switch-buffer)
	 ("C-x C-f" . counsel-find-file))
  
  :config
  (setq ivy-initial-inputs-alist nil))

;;Setup a better help system by rebinding the system
;;describe commands to call the helpful equivalents.
;;Customize counsel commands to delegate to the helpful
;;commands so that you get the rich autocomplete of counsel
;;and the good docs of helpful.
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;;Package use to create general keybindings. I've chosed
;;to set aside C-t as my own user defined keyspace, where
;;I can place my own keybindings to various commands as
;;I want.  The create-definer is what sets aside a specific
;;key stroke as being the prefix leading into other keybindings
(use-package general)
(general-create-definer jgon/leader-keys
  :prefix "C-;")
(jgon/leader-keys
  ;;"C-t" '(:ignore t :which-key "toggles")
  "t" '(counsel-load-theme :which-key "choose theme"))


;;Projectile provides useful project management functionality
;;for emacs, bind C-c p as a jumping off point to all the things
;;it can do, and setup my ~/Code folder as the folder where I store
;;all my coding projects.
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Code")
    (setq projectile-project-search-path '("~/Code"))))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package yasnippet)

(use-package emaps)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "<C-return>")
  (setq lsp-enable-snippet nil)
  :config (lsp-enable-which-key-integration t))

(use-package lsp-ui)

(use-package company
  :config (global-company-mode t)
  (setq company-idle-delay 0))

(use-package lispy
  :hook ((emacs-lisp-mode . lispy-mode)
         (scheme-mode . lispy-mode)))

(use-package sly
  :mode "\\.lisp\\'")

(use-package slime
  :disabled
  :mode "\\.lisp\\'")

;; Include .sld library definition files
(use-package quack
  :mode "\\.sld\\'")



(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp)))
  :config (setq ccls-executable "/home/jeff.gonis/Code/ccls/Release/ccls"))

