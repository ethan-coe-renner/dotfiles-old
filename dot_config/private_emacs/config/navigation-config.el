;; set up navigation helpers
(use-package avy
  :straight
  :general
  ("C-s" 'avy-goto-char-timer)
  )

(general-def
  "C-w m" 'maximize-window)

(use-package winum
  :straight t
  :general
  (
   "M-1" 'winum-select-window-1
   "M-2" 'winum-select-window-2
   "M-3" 'winum-select-window-3
   "M-4" 'winum-select-window-4
   "M-5" 'winum-select-window-5
   "M-6" 'winum-select-window-6
   "M-7" 'winum-select-window-7
   "M-8" 'winum-select-window-8
   )
  :config
  (winum-mode t))

(use-package smartscan
  :straight t
  :hook (prog-mode . smartscan-mode))

(use-package rg
  :straight t
  :config
  (rg-enable-default-bindings))
