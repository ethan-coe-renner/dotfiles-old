;; set up modes for various languages

(use-package toml-mode :straight t)
(use-package yaml-mode :straight t
  :mode "\\.yml\\'")
(use-package rustic :straight t)
(use-package nix-mode :straight t)
(use-package json-mode :straight t)

(use-package gnuplot :straight t
  :init
  (autoload 'gnuplot-mode "gnuplot" "Gnuplot major mode" t)
  (autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot-mode" t)
  (setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist)))

(use-package kbd-mode
  :straight (kbd-mode :type git :host github :repo "kmonad/kbd-mode"))


(use-package flycheck :straight t
  :init (global-flycheck-mode)
  )
