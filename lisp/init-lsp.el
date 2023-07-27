;;; init-lsp.el -*- lexical-binding: t no-byte-compile: t -*-
(defun my-eglot-keybindgs ()
  (define-key evil-motion-state-map "gR" #'eglot-rename)
  (define-key evil-motion-state-map "gr" #'xref-find-references)
  (define-key evil-normal-state-map "gi" #'eglot-find-implementation)
  (define-key evil-motion-state-map "gh" #'eldoc)
  (define-key evil-normal-state-map "ga" #'eglot-code-actions))

(use-package eglot
  :ensure
  :init
  (define-derived-mode vue-mode web-mode "Vue"
    "A major mode derived from web-mode, for editing .vue files with LSP support.")
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (advice-add 'eglot-ensure :after 'my-eglot-keybindgs)
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l r" . eglot-rename)
              ("C-c l o" . eglot-code-action-organize-imports)
              ("C-c l f" . eglot-format)
              ("C-c l d" . eldoc)
              ("s-<return>" . eglot-code-actions))
  :hook
  (eglot-managed-mode . me/flymake-eslint-enable-maybe)
  (css-mode . eglot-ensure)
  (js2-mode . eglot-ensure)
  (js-mode . eglot-ensure)
  ;; (web-mode . eglot-ensure)
  (vue-mode . eglot-ensure)
  (rust-mode . eglot-ensure)
  (elixir-mode . eglot-ensure)
  (c++-mode . eglot-ensure)
  ;; disable for performance issue, specially for peek framework definition
  ;; (dart-mode . eglot-ensure)
  :config
  (setq eglot-send-changes-idle-time 0.2)
  (add-to-list 'eglot-server-programs `(vue-mode . ("vue-language-server" "--stdio" :initializationOptions (
                         :typescript (:tsdk
                        "/Users/reynardlee/.nvm/versions/node/v18.9.1/lib/node_modules/typescript/lib")
                         :languageFeatures (:completion
                                            (:defaultTagNameCase "both"
                                                                 :defaultAttrNameCase "kebabCase"
                                                                 :getDocumentNameCasesRequest nil
                                                                 :getDocumentSelectionRequest nil)
                                            :diagnostics
                                            (:getDocumentVersionRequest nil))
                         :documentFeatures (:documentFormatting
                                            (:defaultPrintWidth 100
                                                                :getDocumentPrintWidthRequest nil)
                                            :documentSymbol t
                                            :documentColor t)))))
  (add-to-list 'eglot-server-programs '(rust-mode "rust-analyzer"))
  (add-to-list 'eglot-server-programs '(c++-mode . ("clangd" "--enable-config")))
  ;;(add-to-list 'eglot-server-programs '(web-mode . ("vscode-html-language-server" "--stdio")))

  (setq read-process-output-max (* 1024 1024))
  (push :documentHighlightProvider eglot-ignored-server-capabilities)
  (setq eldoc-echo-area-use-multiline-p nil))

  (cl-defmacro eglot-org-babel-enable (lang)
    "Support LANG in org source code block."
    (cl-check-type lang string)
    (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
           (intern-pre (intern (format "eglot--%s" (symbol-name edit-pre)))))
      `(progn
         (defun ,intern-pre (info)
           (let ((file-name (->> info caddr (alist-get :file))))
             (unless file-name
               (setq file-name (concat default-directory (if (string= ,lang "C") "org-src-babel.c" "org-src-babel.cpp")))
               (write-region (point-min) (point-max) file-name))
             (setq buffer-file-name file-name)
             (eglot-ensure)))
         (put ',intern-pre 'function-documentation
              (format "Enable lsp-bridge-mode in the buffer of org source block (%s)."
                      (upcase ,lang)))
         (if (fboundp ',edit-pre)
             (advice-add ',edit-pre :after ',intern-pre)
           (progn
             (defun ,edit-pre (info)
               (,intern-pre info))
             (put ',edit-pre 'function-documentation
                  (format "Prepare local buffer environment for org source block (%s)."
                          (upcase ,lang))))))))

  (with-eval-after-load 'org
    (dolist (lang '("C" "C++"))
      (eval `(eglot-org-babel-enable ,lang))))

(use-package consult-eglot
  :ensure t
  :defer t)

(use-package dumb-jump
  :ensure t
  :config (setq dumb-jump-selector 'completion-read))

(with-eval-after-load 'xref
  (setq xref-search-program 'ripgrep)     ;project-find-regexp
  (when (functionp 'xref-show-definitions-completing-read)
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
    (setq xref-show-xrefs-function #'xref-show-definitions-completing-read)))




(provide 'init-lsp)
