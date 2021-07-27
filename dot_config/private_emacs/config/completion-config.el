;; set up in buffer completion
(use-package company
  :straight t
  :init
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0) ;; default is 0.2
  )
