;; Dired configuration
(use-package dired
  :config
  (setq dired-listing-switches "-agho --group-directories-first")
  :general
  (general-def
    :states 'normal
    :keymaps 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file)
  )

(use-package all-the-icons-dired
  :straight t
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package dired-x
  :general
  (leader-key-def
    "<SPC>" 'dired-jump
    ))

;; Revert Dired and other buffers
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
