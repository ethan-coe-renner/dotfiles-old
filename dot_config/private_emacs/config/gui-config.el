;; GUI Settings and packages

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)
(setq frame-resize-pixelwise t)


(set-face-attribute 'default nil :font "Source Code Pro" :height 120)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Source Code Pro" :height 120)

(setq custom-file "~/.config/emacs-config/custom.el")
(load custom-file)
(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-nord t)
  )

(use-package smart-mode-line
  :straight t
  :config
  (sml/setup))


;; Line number setup
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook
                rust-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

(recentf-mode t)
(setq recentf-max-saved-items 100)
