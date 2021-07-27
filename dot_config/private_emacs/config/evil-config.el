;; Set up Evil Mode

(use-package evil
  :straight t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-tree)
  (setq evil-want-integration t)
  (setq evil-respect-visual-line-mode t)
  (setq evil-want-C-u-scroll t)
  (setq evil-cros-lines t)

  :hook (evil-mode . visual-line-mode)

  :config
  (evil-mode 1)

  :general
  ("C-M-u" 'universal-argument)
  (:states '(normal motion)
           "j" 'evil-next-visual-line
           "k" 'evil-previous-visual-line
           )
  )

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :straight t
  :config
  (evil-commentary-mode 1))

(use-package evil-goggles
  :straight t
  :config
  (evil-goggles-mode 1))

(use-package evil-snipe
  :straight t
  :general
  (:states '(normal motion)
           "s" 'evil-snipe-s
           "S" 'evil-snipe-S)
  )

(use-package evil-multiedit
  :straight t
  :config
  (evil-multiedit-default-keybinds)
  )
(use-package undo-tree
  :straight t
  :config
  (global-undo-tree-mode 1)
  )

(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode 1))
