;; set up version control settings and packages
(use-package magit
  :straight t
  :general
  (leader-key-def
    "g" 'magit-status
    ))

(use-package projectile
  :straight t
  :config (projectile-mode)
  :init
  (setq projectile-switch-project-action #'projectile-dired)
  :general
  (leader-key-def
    "p" 'projectile-command-map
    ))
