;; Basic config
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq ring-bell-function 'ignore)
(setq default-frame-alist '((font . "JetBrainsMono Nerd Font-14")))
(setq modus-themes-region '(bg-only))
(load-theme 'modus-operandi)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

;; Store backups & auto-saves in a directory
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-saves/" user-emacs-directory) t)))
(setq backup-directory-alist
      `((".*" . ,(expand-file-name "backups/" user-emacs-directory))))
(setq create-lockfiles nil)
(setq make-backup-files t
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-old-versions 6
      kept-new-versions 9)
(setq auto-save-timeout 20
      auto-save-interval 200)

(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 100)
(setq recentf-save-file (expand-file-name "recentf" user-emacs-directory))

;; C format
(setq c-default-style "k&r"
      c-basic-offset 4)

;; Packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(setq use-package-always-ensure t)

(use-package posframe)
(use-package vertico-posframe
  :after vertico
  :init (vertico-posframe-mode))
(use-package doom-modeline
  :init (doom-modeline-mode 1))
(use-package nerd-icons)
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))
(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode))
(use-package eglot
  :hook
  ((c-mode c++-mode) . eglot-ensure)
  :config
  (add-to-list
   'eglot-server-programs
   '((c-mode c++-mode) . ("clangd"
			  "--background-index"
			  "--clang-tidy"
			  "--compile-commands-dir=build"))))
(use-package eldoc
  :ensure nil
  :hook (eglot-managed-mode . eldoc-box-hover-mode))
(use-package magit)
(use-package dashboard
  :init
  (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  (setq dashboard-items '((recents . 5)
			   (bookmarks . 5)
			   (projects . 5)))
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  :hook
  (dashboard-mode . (lambda () (display-line-numbers-mode -1)))
  :config
  (dashboard-setup-startup-hook))

(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-preselect 'first)
  (corfu-quit-no-match 'separator)
  (corfu-scroll-margin 4))
(use-package yasnippet
  :config (yas-global-mode 1))
(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode))
(use-package tree-sitter-langs)
(use-package which-key
  :config (which-key-mode))
(use-package vertico
  :init (vertico-mode))
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))
(use-package consult
  :bind
  ("M-i" . consult-imenu)
  ("C-s" . consult-line)
  ("C-x b" . consult-buffer)
  ("C-x C-r" . consult-recent-file)
  ("C-x r b" . consult-bookmark)
  ("M-g g" . consult-goto-line)
  ("C-x p b" . consult-project-buffer))
(use-package avy
  :bind ("C-;" . avy-goto-char-in-line))
(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :init
  (setq markdown-command "pandoc"))
(use-package projectile
  :init
  (setq projectile-project-search-path '("~/projects"))
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (keymap-global-set "C-c p" 'projectile-command-map)
  (projectile-mode +1))

(defun my/cmake-pick-target ()
  "Parse cmake --build . --target help and let user pick a clean target to build."
  (interactive)
  (let* ((project-root (project-root (project-current t)))
         (build-dir (expand-file-name "build/" project-root))
         (default-directory build-dir)
         (raw-output (shell-command-to-string "cmake --build . --target help"))
         (lines (split-string raw-output "\n" t))
         (targets
          (seq-filter
           (lambda (line)
             (and (string-prefix-p "... " line)
                  (let* ((trimmed (string-trim (substring line 4)))
                         (target (car (split-string trimmed " "))))
                    (not (string-match-p "\\." target)))))
           lines))
         ;; Clean off ...  and trim descriptions 
         (cleaned (mapcar (lambda (line)
                            (car (split-string (string-trim (substring line 4)) " ")))
                          targets))
         (target (completing-read "CMake target: " cleaned)))
    (compile (format "cmake --build %s --target %s" build-dir target))))

(keymap-global-set "C-c m" 'my/cmake-pick-target)
(keymap-global-set "M-z" 'zap-up-to-char)
(keymap-global-set "C-c l" 'org-store-link)
(keymap-global-set "C-c a" 'org-agenda)
(keymap-global-set "C-c c" 'org-capture)
(keymap-global-set "C-x C-b" 'ibuffer)

(defvar flymake-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'flymake-goto-next-error)
    (define-key map (kbd "p") #'flymake-goto-next-previous)
    (define-key map (kbd "l") #'flymake-show-buffer-diagnostics)
    map))

(define-key global-map (kbd "C-c !") flymake-prefix-map)
