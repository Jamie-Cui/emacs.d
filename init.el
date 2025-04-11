;;     Copyright (C) 2025 Jamie Cui

;;     This program is free software: you can redistribute it and/or modify
;;     it under the terms of the GNU General Public License as published by
;;     the Free Software Foundation, either version 3 of the License, or
;;     (at your option) any later version.

;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;     GNU General Public License for more details.

;;     You should have received a copy of the GNU General Public License
;;     along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Also add information on how to contact you by electronic and paper mail.

;;   If the program does terminal interaction, make it output a short
;; notice like this when it starts in an interactive mode:

;;     <program>  Copyright (C) <year>  <name of author>
;;     This program comes with ABSOLUTELY NO WARRANTY; for details type `show w'.
;;     This is free software, and you are welcome to redistribute it
;;     under certain conditions; type `show c' for details.

;; The hypothetical commands `show w' and `show c' should show the appropriate
;; parts of the General Public License.  Of course, your program's commands
;; might be different; for a GUI interface, you would use an "about box".

;;   You should also get your employer (if you work as a programmer) or school,
;; if any, to sign a "copyright disclaimer" for the program, if necessary.
;; For more information on this, and how to apply and follow the GNU GPL, see
;; <http://www.gnu.org/licenses/>.

;;   The GNU General Public License does not permit incorporating your program
;; into proprietary programs.  If your program is a subroutine library, you
;; may consider it more useful to permit linking proprietary applications with
;; the library.  If this is what you want to do, use the GNU Lesser General
;; Public License instead of this License.  But first, please read
;; <http://www.gnu.org/philosophy/why-not-lgpl.html>.

;; -----------------------------------------------------------
;; Emacs native configurations
;; -----------------------------------------------------------

;; disable certain things
(menu-bar-mode 0)
(tool-bar-mode 0)

;; -----------------------------------------------------------
;; (my) emacs core thirdparty configurations
;; -----------------------------------------------------------

;; Enable package
(require 'package)
(setq package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
                         ("nongnu"   . "http://elpa.nongnu.org/nongnu/")
                         ("org"   . "http://orgmode.org/elpa/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)


;;防止反复调用 package-refresh-contents 会影响加载速度
(when (not package-archive-contents)
  (package-refresh-contents))

;;modeline上显示我的所有的按键和执行的命令
(unless (package-installed-p 'keycast)
  (package-install 'keycast))

(unless (package-installed-p 'evil-collection)
  (package-install 'evil-collection))

(unless (package-installed-p 'undo-tree)
  (package-install 'undo-tree))

(unless (package-installed-p 'undo-fu)
  (package-install 'undo-fu))

(unless (package-installed-p 'which-key)
  (package-install 'which-key))

(unless (package-installed-p 'evil)
  (package-install 'evil))

(unless (package-installed-p 'vertico)
  (package-install 'vertico))

(unless (package-installed-p 'projectile)
  (package-install 'projectile))

;; keycast
(use-package keycast
  :config
  (keycast-mode-line-mode 1))

;; which-key
(use-package which-key
  :config
  (which-key-setup-minibuffer)
  (which-key-mode 1))

;; vertico
(use-package vertico
  :init
  (vertico-mode)
  )

;; evil
(use-package undo-tree)
(use-package undo-fu)

(use-package evil
  :ensure t
  :after (:and undo-tree undo-fu)
  :init
  (setq evil-overriding-maps nil)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)

  ;; Leader
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'visual (kbd "SPC"))
  (evil-set-leader 'motion (kbd "SPC"))
  (evil-set-leader 'insert (kbd "M-SPC"))

  ;; window-related key bindings
  (evil-define-key nil 'global
    (kbd "<leader>wh")     #'evil-window-left
    (kbd "<leader>wj")     #'evil-window-down
    (kbd "<leader>wk")     #'evil-window-up
    (kbd "<leader>wl")     #'evil-window-right
    (kbd "<leader>ww")     #'other-window
    (kbd "<leader>wd")     #'evil-window-delete
    (kbd "<leader>ws")     #'evil-window-split
    (kbd "<leader>wv")     #'evil-window-vsplit
    )

  ;; buffer-related key bindings
  (evil-define-key nil 'global
    (kbd "<leader>bb")     #'vertico-
    (kbd "<leader>bj")     #'evil-window-down
    (kbd "<leader>wk")     #'evil-window-up
    (kbd "<leader>wl")     #'evil-window-right
    (kbd "<leader>ww")     #'other-window
    (kbd "<leader>wd")     #'evil-window-delete
    (kbd "<leader>ws")     #'evil-window-split
    (kbd "<leader>wv")     #'evil-window-vsplit
    )
  )

;;; evil-collection
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))
