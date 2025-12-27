;;; init-completion.el --- completion framework configuration -*- lexical-binding: t -*-
;;; Commentary:
;;
;; 配置现代化的补全框架,包括:
;; - Vertico: 垂直补全 UI
;; - Corfu: 文本内补全
;; - Consult: 增强的搜索和导航
;; - Orderless: 模糊匹配
;; - Marginalia: 补全候选项注释
;;
;; Dependencies:
;; - ripgrep (rg): 用于 consult-ripgrep
;;

;;; Code:

;; -----------------------------------------------------------
;; Vertico - 垂直补全 UI
;; -----------------------------------------------------------

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-preselect 'first)
  (vertico-count 17)
  (vertico-multiform-categories '((embark-keybinding grid)))
  :config
  (vertico-multiform-mode))

(use-package vertico-posframe
  :ensure t
  :after vertico
  :config
  (vertico-posframe-mode 1))

;; -----------------------------------------------------------
;; Corfu - 文本内补全
;; -----------------------------------------------------------

(use-package corfu
  :ensure t
  :custom
  (corfu-auto nil)
  (corfu-cycle t)
  (corfu-preview-current 'nil) ; 不预选插入
  (corfu-preselect 'nil)       ; 不预先选择
  (corfu-quit-no-match 'separator)
  :config
  (global-corfu-mode)
  ;; 在 eshell 中使用 corfu
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-auto nil))))

(use-package corfu-terminal
  :ensure t
  :defer t
  :config
  (when (version< emacs-version "31")
    (corfu-terminal-mode +1)))

;; -----------------------------------------------------------
;; Orderless - 模糊匹配
;; -----------------------------------------------------------

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides
        '((file (styles . (partial-completion))))))

;; -----------------------------------------------------------
;; Marginalia - 补全候选项注释
;; -----------------------------------------------------------

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; -----------------------------------------------------------
;; Consult - 增强的搜索和导航
;; -----------------------------------------------------------

(use-package consult
  :ensure t
  :custom
  (consult-preview-max-count 17)
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 1 any))
  :config
  (setq xref-show-xrefs-function       #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; -----------------------------------------------------------
;; Custom Functions
;; -----------------------------------------------------------

;;;###autoload
(cl-defun +vertico/file-search
    (&key query in all-files (recursive t) prompt args)
  "Conduct a file search using ripgrep.

:query STRING
  Determines the initial input to search for.
:in PATH
  Sets what directory to base the search out of. Defaults to the current project's root.
:recursive BOOL
  Whether or not to search files recursively from the base directory.
:args LIST
  Arguments to be appended to `consult-ripgrep-args'."
  (declare (indent defun))
  (unless (executable-find "rg")
    (user-error "Couldn't find ripgrep in your PATH"))
  (require 'consult)
  (setq deactivate-mark t)
  (let* ((project-root (or (projectile-project-root) default-directory))
         (directory (or in project-root))
         (consult-ripgrep-args
          (concat "rg "
                  (if all-files "-uu ")
                  (unless recursive "--maxdepth 1 ")
                  "--null --line-buffered --color=never --max-columns=1000 "
                  "--smart-case --no-heading "
                  "--with-filename --line-number --search-zip "
                  "--hidden -g !.git -g !.svn -g !.hg "
                  (mapconcat #'identity args " ")))
         (prompt (if (stringp prompt) (string-trim prompt) "Search"))
         (query query)
         (consult-async-split-style consult-async-split-style)
         (consult-async-split-styles-alist consult-async-split-styles-alist))
    ;; Change the split style if the initial query contains the separator.
    (when query
      (cl-destructuring-bind (&key type separator initial _function)
          (alist-get consult-async-split-style consult-async-split-styles-alist)
        (pcase type
          (`separator
           (replace-regexp-in-string (regexp-quote (char-to-string separator))
                                     (concat "\\" (char-to-string separator))
                                     query t t))
          (`perl
           (when (string-match-p initial query)
             (setf (alist-get 'perlalt consult-async-split-styles-alist)
                   `(:initial ,(or (cl-loop for char in (list "%" "@" "!" "&" "/" ";")
                                            unless (string-match-p char query)
                                            return char)
                                   "%")
                              :type perl)
                   consult-async-split-style 'perlalt))))))
    (consult--grep prompt #'consult--ripgrep-make-builder directory query)))

;;;###autoload
(defun +vertico/project-search
    (&optional arg initial-query directory)
  "Performs a live project search from the project root using ripgrep.
If ARG (universal argument), include all files, even hidden or compressed ones,
in the search."
  (interactive "P")
  (+vertico/file-search :query initial-query :in directory :all-files arg))

(provide 'init-completion)
