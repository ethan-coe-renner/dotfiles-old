;; Install and basic setup for general.el.
;; Most keybindings are with their associated packages

(use-package general
  :straight t
  :config
  (general-auto-unbind-keys)
  (general-evil-setup)

  (general-create-definer leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  )
