;;; init-web.el -*- lexical-binding: t no-byte-compile: t -*-

;; Copyright (C) 2021-2023 zilongshanren

;; Author: zilongshanren <guanghui8827@gmail.com>
;; URL: https://github.com/zilongshanren/emacs.d


;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;; config js2-mode for js files
(setq auto-mode-alist
      (append
       '(
         ("\\.js\\'" . js-mode)
         ("\\.vue\\'" . vue-mode)
         ("\\.html\\'" . web-mode)
         ("\\.html.eex\\'" . web-mode)
         )
       auto-mode-alist))

;; config for web mode
(defun my-web-mode-indent-setup ()
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-style-padding 0)
  (setq web-mode-script-padding 0) ; web-mode, vue sfc no padding in the script section
  (setq web-mode-markup-indent-offset 2) ; web-mode, html tag in html file, default is 4
  (setq web-mode-css-indent-offset 2)    ; web-mode, css in html file
  (setq web-mode-code-indent-offset 2)   ; web-mode, js code in html file
  )

(use-package typescript-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :config
  (my-web-mode-indent-setup)
  :custom-face
  (web-mode-current-element-highlight-face
   ((t (:background "#d9dbd7" :foreground "#2d3428"))))
  ;; light color for highlighting the current HTML element's column
  (web-mode-current-column-highlight-face
   ((t (:background "#d9dbd7"))))
)

(use-package emmet-mode
  :ensure t)


(defun my-toggle-web-indent ()
  (interactive)
  ;; web development
  (if (or (eq major-mode 'js-mode) (eq major-mode 'js2-mode))
      (progn
	(setq js-indent-level (if (= js-indent-level 2) 4 2))
	(setq js2-basic-offset (if (= js2-basic-offset 2) 4 2))))

  (if (eq major-mode 'web-mode)
      (progn (setq web-mode-markup-indent-offset (if (= web-mode-markup-indent-offset 2) 4 2))
	     (setq web-mode-css-indent-offset (if (= web-mode-css-indent-offset 2) 4 2))
	     (setq web-mode-code-indent-offset (if (= web-mode-code-indent-offset 2) 4 2))))
  (if (eq major-mode 'css-mode)
      (setq css-indent-offset (if (= css-indent-offset 2) 4 2)))
)


(use-package yaml-mode
  :ensure t
  )

(use-package dtrt-indent
  :diminish
  :hook (prog-mode . dtrt-indent-mode))


(provide 'init-web)
