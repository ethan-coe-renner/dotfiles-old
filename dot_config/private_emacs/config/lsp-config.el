
(use-package lsp-mode
  :straight t
  :init
  (setq gc-cons-threshold 100000000) ;; set per the lsp-doctor recommendation
  (setq read-process-output-max (* 1024 1024)) ;; same reason ^
  (setq lsp-keymap-prefix "C-c l")
  :hook (
         (rustic-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui :straight t :commands lsp-ui-mode)
(use-package lsp-treemacs :straight t :commands lsp-treemacs-errors-list)
