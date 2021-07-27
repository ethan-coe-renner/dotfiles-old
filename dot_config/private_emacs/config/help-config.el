;; Set up packages for helping

(use-package which-key
  :straight t
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(use-package helpful
  :straight t
  :general
  (leader-key-def
    "h" 'helpful-at-point
    )
  )

(use-package define-word
  :straight t
  :general
  (leader-key-def
    "d" 'define-word-at-point
    "D" 'define-word
    ))
