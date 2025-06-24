;; fancy eshell 
;; see: https://lambdaland.org/posts/2024-08-19_fancy_eshell_prompt/
(setopt eshell-prompt-function #'+eshell/fancy-shell)
(setopt eshell-prompt-regexp "^[^#$\n]* [$#] ")
(setopt eshell-highlight-prompt nil)

(defun +eshell/fancy-shell ()
  "A pretty shell with git status"
  (let* ((cwd (abbreviate-file-name (eshell/pwd)))
         (x-stat eshell-last-command-status))
    (propertize
     (format "%s %s $ "
             (if (< 0 x-stat) (format (propertize "!%s" 'font-lock-face '(:foreground "red")) x-stat)
               (propertize "➤" 'font-lock-face (list :foreground (if (< 0 x-stat) "red" "green"))))
             (propertize cwd 'font-lock-face '(:foreground "#45babf")))
     'read-only t
     'front-sticky   '(font-lock-face read-only)
     'rear-nonsticky '(font-lock-face read-only)))
  )
