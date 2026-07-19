;;; llm.el --- LLM and agent integrations -*- lexical-binding: t -*-
;;; Commentary:
;; LLM and agent integrations: agent-switch, gptel, agent-shell, magent and
;; magit-gptel.
;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; -----------------------------------------------------------
;; DONE llm
;;
;; agent-switch
;; agent-shell
;; gptel
;; magit-gptel
;; -----------------------------------------------------------

(use-package agent-switch
  :vc (:url "https://github.com/Jamie-Cui/agent-switch.el" :rev "main")
  :ensure t
  :commands agent-switch
  :custom
  (agent-switch-authinfo-file (expand-file-name "~/.authinfo"))
  )

;; (use-package gptel-agent
;;   :ensure t
;;   :after gptel
;;   :config
;;   (require 'gptel-agent-tools)
;;   (add-to-list 'gptel-agent-dirs (concat +emacs/repo-directory "/agents"))
;;   (gptel-agent-update)
;;   ;; this package automatically add presets to gptel
;;   ;; @gptel-agent
;;   ;; @gptel-plan
;;   )

(use-package agent-shell
  :ensure t
  :custom
  (agent-shell-display-action 'display-buffer-other-window)
  ;; (agent-shell-display-action
  ;;  '((display-buffer-reuse-window
  ;;     display-buffer-use-some-window
  ;;     display-buffer-pop-up-window)
  ;;    (inhibit-same-window . t)))
  (agent-shell-show-welcome-message nil)
  (agent-shell-header-style 'text)
  (agent-shell-show-config-icons nil)
  :config

  (defun +agent-shell/bind-return-in-action-keymap-a (map)
    "Bind GUI <return> in agent-shell action keymaps."
    (when (keymapp map)
      (when-let* ((action (lookup-key map (kbd "RET"))))
        (define-key map (kbd "<return>") action)))
    map)

  (with-eval-after-load 'agent-shell-ui
    (advice-remove 'agent-shell-ui-make-action-keymap
                   #'+agent-shell/bind-return-in-action-keymap-a)
    (advice-add 'agent-shell-ui-make-action-keymap
                :filter-return #'+agent-shell/bind-return-in-action-keymap-a))

  ;; HACK using +emacs/proxy for codex-acp
  (with-eval-after-load 'agent-shell-openai
    (let ((proxy (concat "http://" +emacs/proxy)))
      (setq agent-shell-openai-codex-environment
            (list
             (format "http_proxy=%s" proxy)
             (format "https_proxy=%s" proxy)
             (format "HTTP_PROXY=%s" proxy)
             (format "HTTPS_PROXY=%s" proxy)
             (format "all_proxy=%s" proxy)
             (format "ALL_PROXY=%s" proxy)
             "no_proxy=localhost,127.0.0.1,::1"
             "NO_PROXY=localhost,127.0.0.1,::1"))))

  (with-eval-after-load 'agent-shell-anthropic
    (let ((proxy (concat "http://" +emacs/proxy)))
      (setq agent-shell-anthropic-claude-environment
            (list
             (format "http_proxy=%s" proxy)
             (format "https_proxy=%s" proxy)
             (format "HTTP_PROXY=%s" proxy)
             (format "HTTPS_PROXY=%s" proxy)
             (format "all_proxy=%s" proxy)
             (format "ALL_PROXY=%s" proxy)
             "no_proxy=localhost,127.0.0.1,::1"
             "NO_PROXY=localhost,127.0.0.1,::1"))))

  ;; HACK using sssaicode api key

  ;; (defun +agent-shell/sss-api-key ()
  ;;   "Return the SSS API key used by Codex."
  ;;   (or (getenv "SSS_API_KEY")
  ;;       (user-error
  ;;        "SSS_API_KEY is not available in Emacs; restart Emacs or run `exec-path-from-shell-copy-env'")))

  ;; (with-eval-after-load 'agent-shell-openai
  ;;   (setq agent-shell-openai-authentication
  ;;         (agent-shell-openai-make-authentication
  ;;          :api-key #'+agent-shell/sss-api-key)
  ;;         agent-shell-openai-codex-acp-command
  ;;         '("codex-acp"
  ;;           "-c" "model_provider=\"sss\""
  ;;           "-c" "preferred_auth_method=\"apikey\"")))
  )

(use-package agent-shell-permission-transient
  :load-path (lambda () (concat +emacs/repo-directory "/site-lisp/"))
  :after agent-shell
  :demand t
  :bind (:map agent-shell-mode-map
              ("C-c C-p" . agent-shell-permission-transient-menu))
  :config
  (agent-shell-permission-transient-mode +1))

(use-package gptel
  :ensure t
  :custom
  (gptel-rewrite-default-action 'merge)
  (gptel-default-mode 'org-mode)
  (gptel-org-branching-context t)
  (gptel-log-level 'info)
  (gptel-proxy +emacs/proxy)
  ;; re-bind key
  :bind (:map gptel-mode-map
              ("C-c C-c" . #'gptel-send)
              ("C-c RET" . #'gptel-menu))
  :config
  ;; Display gptel buffers outside the current window.
  (setq gptel-display-buffer-action
        '((display-buffer-reuse-window
           display-buffer-use-some-window
           display-buffer-pop-up-window)
          (inhibit-same-window . t)))

  ;; register deepseek backend
  (defvar +llm/deepseek
    (gptel-make-deepseek "DeepSeek"
      :key (auth-source-pick-first-password :host "deepseek")
      :stream t))

  ;; register gemini backend
  (defvar +llm/gemini
    (gptel-make-gemini "Gemini"
      :key (auth-source-pick-first-password :host "gemini")
      :stream t))

  ;; register aliyun backend
  (defvar +llm/aliyun
    (gptel-make-deepseek "Aliyun"
      :host "dashscope.aliyuncs.com/compatible-mode/v1"
      :endpoint "/chat/completions"
      :stream t
      :key (auth-source-pick-first-password :host "aliyun")
      :models '(
                qwen3.7-plus
                (qwen3.7-max :request-params (:enable_thinking t))
                )))

  (defvar +llm/sssaicode
    (gptel-make-openai "SssAiCode"
      :host "https://codex1.sssaicode.com/api/v1"
      :endpoint "/chat/completions"
      :stream t
      :key (auth-source-pick-first-password :host "sssaicode")
      :models '(gpt-5.4)))

  ;; register zhipu backend
  (defvar +llm/zhipu
    (gptel-make-deepseek "Zhipu"
      :host "open.bigmodel.cn/api/coding/paas/v4"
      :endpoint "/chat/completions"
      :stream t
      :key (auth-source-pick-first-password :host "zhipu")
      :models '(glm-4.7)))

  ;; register local backend
  ;; NOTE to make ollama work through LAN, on its server
  ;; see: https://github.com/ollama/ollama/blob/main/docs/faq.md
  ;; (defvar +gptel/local-backend
  ;;   (gptel-make-openai "OpenWebUI"
  ;;     :host "localhost:8080"
  ;;     :protocol "http"
  ;;     :endpoint "/api/chat/completions"
  ;;     :stream t
  ;;     :key "sk-02bdf77754894f87b8988711c7d15b67"
  ;;     :models '(qwen2.5-coder:latest)))

  ;; set default values
  (setopt gptel-backend +llm/deepseek)
  (setopt gptel-model 'deepseek-v4-pro)

  ;; set context
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "=@Jamie=\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "=@AI=\n")

  ;; set hook
  (add-hook 'gptel-mode-hook
            (lambda () (insert "* Default Context\n=@Jamie=")))

  (defun +llm/remove-headings (beg end)
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (goto-char beg)
        (while (re-search-forward org-heading-regexp end t)
          (forward-line 0)
          (delete-char (1+ (length (match-string 1))))
          (insert-and-inherit "*")
          (end-of-line)
          (skip-chars-backward " \t\r")
          (insert-and-inherit "*")))))

  (add-hook 'gptel-post-response-functions #'+llm/remove-headings)

  (use-package magit-gptel
    :load-path (lambda () (concat +emacs/repo-directory "/site-lisp/"))
    :after (gptel magit)
    :demand t
    :custom
    (magit-gptel-model 'deepseek-v4-flash)
    (magit-gptel-request-params '(:thinking (:type "disabled")
                                            :temperature 0.1))
    :config
    ;; Reset from the fixed package default so reloading this module never
    ;; appends another copy of the commit-message requirements.
    (setopt magit-gptel-commit-prompt
            magit-gptel--default-commit-prompt))

  ;; -----------------------------------------------------------
  ;; PlantUML Beautification (using gptel-rewrite)
  ;; -----------------------------------------------------------

  (defvar +llm/beautify-plantuml-directive
    "You are a PlantUML expert. Beautify and improve the PlantUML diagram while preserving its semantic meaning. Improve layout, add appropriate styling/colors, organize elements logically, add skinparams for professional appearance. Return ONLY the improved PlantUML code without any explanations or markdown formatting."
    "Rewrite directive for PlantUML beautification.")

  (defun +llm/beautify-plantuml ()
    "Beautify PlantUML source block at point using gptel-rewrite.
This selects the PlantUML code region and invokes gptel's rewrite
functionality, allowing you to diff/ediff/merge the changes."
    (interactive)
    (require 'gptel-rewrite)
    ;; 1. Validate we're in org-mode
    (unless (derived-mode-p 'org-mode)
      (user-error "Not in org-mode"))

    ;; 2. Validate we're in a PlantUML source block
    (let* ((info (org-babel-get-src-block-info))
           (lang (car info)))
      (unless info
        (user-error "Not in a source block"))
      (unless (string= lang "plantuml")
        (user-error "Not in a PlantUML block (current: %s)" lang))

      ;; 3. Select the code region
      (let ((code-start (save-excursion
                          (org-babel-goto-src-block-head)
                          (forward-line 1)
                          (point)))
            (code-end (save-excursion
                        (org-babel-goto-src-block-head)
                        (re-search-forward "^[ \t]*#\\+end_src")
                        (match-beginning 0))))
        ;; 4. Set region and invoke gptel-rewrite
        (goto-char code-start)
        (push-mark code-end t t)
        (let ((gptel--rewrite-directive +llm/beautify-plantuml-directive))
          (gptel--suffix-rewrite)))))
  )

(use-package magent
  :vc (:url "https://github.com/Jamie-Cui/magent" :rev "master")
  :ensure t
  :after (agent-shell gptel)
  :demand t
  :custom
  ;; (magent-bypass-permission t)
  (magent-default-effort 'xhigh)
  :config
  (add-to-list 'magent-skill-directories
               (expand-file-name "skills" +emacs/repo-directory) t)
  (magent-agent-shell-ensure-config)

  (magent-command-register
   "finish-work"
   :description "Create a branch, commit and push all changes, then open a PR."
   :title "Finish work as a pull request"
   :session-policy 'isolated
   :source-layer 'user
   :requires-project t
   :required-tools '(bash)
   :handler
   (lambda (invocation)
     (let* ((root
             (when-let* ((scope
                          (magent-command-invocation-origin-scope invocation)))
               (and (stringp scope)
                    (file-name-as-directory (expand-file-name scope)))))
            (context (magent-command-invocation-argument invocation))
            (current-branch
             (condition-case nil
                 (car (process-lines "git" "-C" root
                                     "branch" "--show-current"))
               (error nil)))
            (reuse-branch-p
             (and (stringp current-branch)
                  (string-prefix-p "finish-work/" current-branch)))
            (branch
             (if reuse-branch-p
                 current-branch
               (format "finish-work/%s"
                       (format-time-string "%Y%m%d-%H%M%S"))))
            process
            process-buffer
            commit
            subject)
       (unless root
         (user-error "/finish-work requires a filesystem-backed project"))
       (cl-labels
           ((cleanup ()
              (let ((owned-process process)
                    (owned-buffer process-buffer))
                (setq process nil
                      process-buffer nil)
                (when (process-live-p owned-process)
                  (delete-process owned-process))
                (when (buffer-live-p owned-buffer)
                  (kill-buffer owned-buffer))))
            (fail (message)
              (when (eq (magent-command-invocation-status invocation) 'active)
                (cleanup)
                (magent-command-set-cancel-function invocation nil)
                (magent-command-fail
                 invocation
                 (format
                  "Finish work stopped: %s\n\nBranch: %s\nCommit: %s"
                  message branch (or commit "not created")))))
            (run (label command callback)
              (when (eq (magent-command-invocation-status invocation) 'active)
                (magent-command-progress invocation (concat label "..."))
                (let* ((buffer (generate-new-buffer " *magent-finish-work*"))
                       (default-directory root)
                       (process-environment
                        (cons "GIT_TERMINAL_PROMPT=0" process-environment))
                       child
                       sentinel)
                  (setq
                   sentinel
                   (lambda (finished _event)
                     (when (and
                            (memq (process-status finished) '(exit signal))
                            (eq finished process)
                            (not (process-get finished 'finish-work-done)))
                       (process-put finished 'finish-work-done t)
                       (let ((status (process-exit-status finished))
                             (output
                              (when (buffer-live-p buffer)
                                (with-current-buffer buffer
                                  (buffer-substring-no-properties
                                   (point-min) (point-max))))))
                         (setq process nil
                               process-buffer nil)
                         (when (buffer-live-p buffer)
                           (kill-buffer buffer))
                         (when (eq (magent-command-invocation-status invocation)
                                   'active)
                           (if (zerop status)
                               (condition-case err
                                   (funcall callback (or output ""))
                                 ((error quit)
                                  (fail (error-message-string err))))
                             (fail
                              (format
                               "%s failed (%s): %s"
                               (mapconcat #'shell-quote-argument command " ")
                               status
                               (string-trim (or output ""))))))))))
                  (condition-case err
                      (progn
                        (setq child
                              (make-process
                               :name "magent-finish-work"
                               :buffer buffer
                               :command command
                               :connection-type 'pipe
                               :coding 'utf-8-unix
                               :noquery t
                               :sentinel #'ignore)
                              process child
                              process-buffer buffer)
                        (set-process-sentinel child sentinel)
                        (when (memq (process-status child) '(exit signal))
                          (funcall sentinel child "finished\n")))
                    ((error quit)
                     (when (buffer-live-p buffer)
                       (kill-buffer buffer))
                     (fail (error-message-string err))))))))
         (magent-command-defer invocation)
         (magent-command-set-cancel-function
          invocation (lambda () (cleanup)))
         (run
          "Resolving repository"
          (list "git" "-C" root "rev-parse" "--show-toplevel")
          (lambda (root-output)
            (setq root
                  (file-name-as-directory
                   (file-truename (string-trim root-output))))
            (run
             "Checking worktree"
             (list "git" "-C" root "status" "--porcelain=v1"
                   "--untracked-files=all")
             (lambda (status-output)
               (when (string-empty-p status-output)
                 (user-error "The worktree has no changes to finish"))
               (run
                (if reuse-branch-p "Reusing branch" "Creating branch")
                (if reuse-branch-p
                    (list "git" "-C" root "switch" branch)
                  (list "git" "-C" root "switch" "-c" branch))
                (lambda (_switch-output)
                  (run
                   "Staging changes"
                   (list "git" "-C" root "add" "-A")
                   (lambda (_add-output)
                     (run
                      "Checking staged changes"
                      (list "git" "-C" root "diff" "--cached"
                            "--name-only")
                      (lambda (paths)
                        (when (string-empty-p (string-trim paths))
                          (user-error "No staged changes remain to commit"))
                        (magent-command-progress
                         invocation "Writing commit message...")
                        (magent-command-submit-step
                         invocation
                         (format
                          (concat
                           "Write one Conventional Commit subject for the exact "
                           "staged changes in %s on branch %s. Inspect them with "
                           "git diff --cached as needed. Use only read-only Git "
                           "inspection; do not edit files, alter Git state, push, "
                           "or create a pull request.\n\n"
                           "Optional context: %s\n\n"
                           "Return only the subject line. Do not use Markdown or "
                           "a code fence.")
                          root branch
                          (if (string-empty-p context) "(none)" context))
                         (lambda (status result)
                           (if (not (eq status 'completed))
                               (fail
                                (format
                                 "Commit-message step ended with %s: %s"
                                 status
                                 (magent-agent-result-content-string result)))
                             (let* ((subject-regexp
                                     (concat
                                      "\\`\\(?:feat\\|fix\\|refactor\\|docs"
                                      "\\|test\\|chore\\|build\\|ci\\|perf"
                                      "\\|style\\)"
                                      "\\(?:([a-z0-9][a-z0-9_.-]*?)\\)?!?"
                                      ": [^[:cntrl:]]+\\'"))
                                    (raw-subject
                                     (magent-agent-result-content-string result))
                                    (candidate
                                     (cl-find-if
                                      (lambda (line)
                                        (and (<= (length line) 120)
                                             (string-match-p subject-regexp line)))
                                      (split-string raw-subject
                                                    "[\n\r]+" t "[ \t]+"))))
                               (unless
                                   candidate
                                 (error
                                  "No valid Conventional Commit subject in: %S"
                                  raw-subject))
                               (setq subject candidate)
                               (run
                                "Creating commit"
                                (list "git" "-C" root "commit" "-m" subject)
                                (lambda (_commit-output)
                                  (run
                                   "Reading commit"
                                   (list "git" "-C" root "rev-parse" "HEAD")
                                   (lambda (head-output)
                                     (setq commit (string-trim head-output))
                                     (run
                                      "Pushing branch"
                                      (list "git" "-C" root "push"
                                            "--set-upstream" "origin" branch)
                                      (lambda (_push-output)
                                        (magent-command-progress
                                         invocation "Writing pull request...")
                                        (magent-command-submit-step
                                         invocation
                                         (format
                                          (concat
                                           "Write the pull request for commit %s "
                                           "on branch %s in %s. Inspect the committed "
                                           "diff as needed. Use only read-only Git "
                                           "inspection; do not edit files, alter Git "
                                           "state, push, or create the pull request "
                                           "yourself. The body should have a concise "
                                           "Summary section.\n\nOptional context: %s\n\n"
                                           "Return only the Markdown body and no "
                                           "surrounding prose or code fence.")
                                          commit branch root
                                          (if (string-empty-p context)
                                              "(none)"
                                            context))
                                         (lambda (pr-status pr-result)
                                           (if (not (eq pr-status 'completed))
                                               (fail
                                                (format
                                                 "PR-writing step ended with %s: %s"
                                                 pr-status
                                                 (magent-agent-result-content-string
                                                  pr-result)))
                                             (let* ((body
                                                     (string-trim
                                                      (magent-agent-result-content-string
                                                       pr-result)))
                                                    (opening-end
                                                     (and
                                                      (string-prefix-p "```" body)
                                                      (string-match "\n" body))))
                                               (when (and opening-end
                                                          (string-suffix-p "```" body))
                                                 (setq body
                                                       (string-trim
                                                        (substring body
                                                                   (1+ opening-end)
                                                                   -3))))
                                               (when (string-empty-p body)
                                                 (error "Pull request body is empty"))
                                               (run
                                                "Creating pull request"
                                                (list "gh" "pr" "create"
                                                      "--head" branch
                                                      "--title" subject
                                                      "--body" body)
                                                (lambda (pr-output)
                                                  (unless
                                                      (string-match
                                                       "https?://[^[:space:]]+"
                                                       pr-output)
                                                    (user-error
                                                     "gh succeeded but returned no PR URL"))
                                                  (let* ((url (match-string
                                                               0 pr-output))
                                                         (report
                                                          (format
                                                           (concat
                                                            "Finish work completed.\n\n"
                                                            "Branch: %s\n"
                                                            "Commit: %s\n"
                                                            "Pull request: %s")
                                                           branch commit url)))
                                                    (magent-command-set-cancel-function
                                                     invocation nil)
                                                    (magent-command-respond
                                                     invocation report)
                                                    (magent-command-complete
                                                     invocation report))))))))))))))))))))))))))))))))
  )


(provide 'init-llm)
;;; llm.el ends here
