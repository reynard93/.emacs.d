;;; init-writing.el -*- lexical-binding: t no-byte-compile: t -*-

;; Copyright (C) 2021-2023 zilongshanren

;; Author: zilongshanren <guanghui8827@gmail.com>
;; URL: https://github.com/zilongshanren/emacs.d

(use-package ispell-minor-mode
  :ensure nil
  :config
  (advice-add 'ispell-lookup-words :around
              (lambda (orig &rest args)
                (shut-up (apply orig args)))))

(use-package flyspell-correct
  :ensure t
  :init

  )

(use-package ispell
  :ensure nil
  :init
  (if sys/win32p
      (setq ispell-program-name "aspell"))
  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))
  (ispell-change-dictionary "american" t))


(use-package ox-hugo
  :ensure t                             ;Auto-install the package from Melpa
  :pin melpa ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
  :after ox
  :commands org-hugo-export-to-md)

(provide 'init-writing)
