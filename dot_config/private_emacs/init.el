(defmacro load-config (name)
  (list 'load-file (concat "~/.config/emacs/config/" name "-config.el")))

(load-config "straight")
(load-config "general")
(load-config "gui")
(load-config "dired")
(load-config "mini-buffer")
(load-config "evil")
(load-config "navigation")
(load-config "formatting")
(load-config "project-management")
(load-config "language-modes")
(load-config "completion")
(load-config "lsp")
(load-config "eshell")
(load-config "help")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("461c4943e6ff399975c0424b895b6f5227b421c547ec8c333803d3e69ac69d64" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
