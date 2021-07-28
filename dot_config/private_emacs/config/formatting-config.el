;; setup for packages to help with formatting
(use-package aggressive-indent
  :straight t
  :hook (prog-mode . aggressive-indent-mode)
  )

;; (use-package format-all
;;   :straight t
;;   :config
;;   (format-all-mode 1))
