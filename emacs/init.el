(setq inihibit-startup-message t)
(setq initial-scratch-message "")
(setq ring-bell-function 'ignore)

(when (display-graphic-p)
  (set-frame-font "JetBrainsMono Nerd Font-14")
  (load-theme 'modus-operandi)
  (scroll-bar-mode 0)
  (menu-bar-mode 0)
  (tool-bar-mode 0))

;; C format
(setq c-default-style "k&r"
      c-basic-offset 4)

;; Store backups & auto-saves in a directory
(let ((backup-dir (expand-file-name "backups/" user-emacs-directory))
      (autosave-dir (expand-file-name "auto-saves/" user-emacs-directory)))
  (setq backup-directory-alist `(("." . ,backup-dir)))
  (setq auto-save-file-name-transforms `((".*" ,autosave-dir t))))

;; Line numbers
(global-display-line-numbers-mode t)



;; Packages
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(setq use-package-always-ensure t)

(use-package nerd-icons)
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))
(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode))
(use-package eglot
  :hook
  ((c-mode c++-mode python-mode) . eglot-ensure)
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
(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-preselect 'first)
  (corfu-quit-no-match 'separator)
  (corfu-scroll-margin 4))
;(use-package nerd-icons-corfu
;  :after corfu
;  :custom
;  (nerd-icons-corfu-font "JetBrainsMono Nerd Font")
;  :hook (corfu-mode . nerd-icons-corfu-mode))
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
  ("C-x b" . consult-buffer)
  ("C-x r b" . consult-bookmark)
  ("M-g g" . consult-goto-line))
(use-package avy
  :bind ("C-;" . avy-goto-word-1))

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

(keymap-global-set "C-c m" #'my/cmake-pick-target)
(keymap-global-set "M-z" 'zap-up-to-char)

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-x C-b") #'ibuffer)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633" "3e200d49451ec4b8baa068c989e7fba2a97646091fd555eca0ee5a1386d56077" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "57a29645c35ae5ce1660d5987d3da5869b048477a7801ce7ab57bfb25ce12d3e" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" default))
 '(gdb-many-windows t)
 '(org-babel-load-languages '((C . t) (emacs-lisp . t)))
 '(package-selected-packages
   '(nerd-icons-ibuffer nerd-icons-corfu corfu nerd-icons-dired diredfl avy exwm eldoc-box smartparens consult magit orderless vertico projectile which-key yasnippet tree-sitter-langs tree-sitter treesit-auto solarized-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
