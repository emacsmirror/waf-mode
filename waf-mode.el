;;; waf-mode.el --- Waf integration for Emacs

;; Author: Denys Valchuk <dvalchuk@gmail.com>
;; URL: https://bitbucket.org/dvalchuk/waf-mode
;; Version: 0.1.0
;; Package-Requires: ((projectile "20151130.1039"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; TODO Add commentary.
;;
;;; Code:

(require 'projectile)

;;; Customization
(defgroup waf nil
  "Waf integration for Emacs"
  :prefix "waf-" :group 'tools
  :link '(url-link :tag "Waf Documentation" "https://github.com/waf-project/waf")
  :link '(url-link :tag "Submit Waf Issue" "https://github.com/waf-project/waf/issues"))

(defcustom waf-mode-keymap-prefix (kbd "C-c w")
  "Waf-mode keymap prefix."
  :group 'waf
  :type 'string)


;;; User setup functions
(defun waf-setup-compile-buffer ()
  "Enables ansi-colors and scrolling in the compilation buffer."
  (require 'ansi-color)
  (add-hook 'compilation-filter-hook
            (lambda ()
              (when (eq major-mode 'compilation-mode)
                (ansi-color-apply-on-region compilation-filter-start (point-max))
                )
              )
            )
  (setq compilation-scroll-output t)
  )


;;; Internal functions
(defun waf--run-make-target (cmd)
  "Call `waf CMD' in the root of the project."
  (let* ((default-directory (projectile-project-root))
         (cmd (concat "waf " cmd)))
    (save-some-buffers (not compilation-ask-about-save)
                       (lambda ()
                         (projectile-project-buffer-p (current-buffer)
                                                      default-directory)))
    (compilation-start cmd)))


;;; User commands
(defun waf-build ()
  "Build Waf project."
  (interactive)
  (waf--run-make-target "build")
  )

(defun waf-clean ()
  "Clean Waf project."
  (interactive)
  (waf--run-make-target "clean")
  )

(defun waf-configure ()
  "Configure Waf project."
  (interactive)
  (waf--run-make-target "configure")
  )

(defun waf-reconfigure ()
  "Reconfigure Waf project."
  (interactive)
  (waf--run-make-target "distclean configure")
  )

(defun waf-rebuild ()
  "Rebuild Waf project."
  (interactive)
  (waf--run-make-target "clean build")
  )

(defun waf-rebuild-all ()
  "Reconfigure and Rebuild Waf project."
  (interactive)
  (waf--run-make-target "distclean configure build")
  )

;;; Minor mode
(defvar waf-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "b") #'waf-build)
    (define-key map (kbd "c") #'waf-clean)
    (define-key map (kbd "C") #'waf-configure)
    (define-key map (kbd "r") #'waf-rebuild)
    (define-key map (kbd "R") #'waf-reconfigure)
    (define-key map (kbd "B") #'waf-rebuild-all)
    map)
  "Keymap for Waf mode commands after `waf-mode-keymap-prefix'.")
(fset 'waf-command-map waf-command-map)

(defvar waf-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map waf-mode-keymap-prefix 'waf-command-map)
    map)
  "Keymap for Waf mode.")

(easy-menu-change
 '("Tools") "Waf"
 '(["Build Project" waf-build]
   ["Rebuild Project" waf-rebuild]
   ["Configure Project" waf-configure]
   ["Reconfigure Project" waf-reconfigure]
   ["Reconfigure and Rebuild Project" waf-rebuild-all]
   "---"
   ["Clean Project" waf-clean]
   )
 )

(defun waf-conditionally-enable ()
  "Enable `waf-mode' only when a `wscript' file is present in project root."
  (condition-case nil
      (when (projectile-verify-file "wscript")
        (waf-mode 1)
        )
    (error nil)
    )
  )


;;;###autoload
(define-minor-mode waf-mode
  "Waf integration for Emacs."
  :lighter " Waf"
  :keymap waf-mode-map
  :group 'waf
  :require 'waf
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("wscript\\'" . python-mode))

(provide 'waf-mode)
;;; waf-mode.el ends here
