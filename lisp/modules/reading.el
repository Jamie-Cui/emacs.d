;;; reading.el --- feeds and reading -*- lexical-binding: t -*-
;;; Commentary:
;; Feeds and reading: elfeed, elfeed-goodies and the dashboard elfeed section.
;;; Code:


;; ------------------------------------------------------------------
;; (optional) elfeed
;; ------------------------------------------------------------------

(use-package elfeed
  :ensure t
  :custom
  (elfeed-db-directory (expand-file-name "elfeed/db/" user-emacs-directory))
  (elfeed-enclosure-default-dir (expand-file-name "elfeed/enclosure/" user-emacs-directory))
  (elfeed-search-filter "") ; startup with no filter
  (elfeed-feeds '(
                  ;;
                  ;; ------ Security ------
                  ;; crypto paper feeds
                  ;; ("https://jamie-cui.github.io/paper-feeds/feed.xml")
                  ("https://jamie-cui.github.io/paper-feeds/feed-arxiv.xml" summary)
                  ("https://jamie-cui.github.io/paper-feeds/feed-iacr.xml" summary)
                  ;; iacr paper
                  ("https://eprint.iacr.org/rss/rss.xml")
                  ;; Linux Security
                  ("https://linuxsecurity.com/linuxsecurity_hybrid.xml")
                  ;; Feisty Duck's Security and Cryptography newsletter
                  ("https://www.feistyduck.com/newsletter/feed")
                  ;; crypto stack exchange
                  ("https://crypto.meta.stackexchange.com/feeds")
                  ;;
                  ;; ------ General ------
                  ;; null program
                  ("https://nullprogram.com/feed/")
                  ;; Linux Do
                  ("https://linux.do/top.rss")
                  ;; Linux Weekly News
                  ("http://lwn.net/headlines/rss")
                  ;; Hacker News Best for top vote getters from the past few days
                  ("https://hnrss.org/best")
                  ;; 架构师之路
                  ("https://plink.anyfeeder.com/weixin/gh_10a6b96351a9")
                  ;;
                  ;; ------ Theory of CS ------
                  ("https://theory.report/rss20.xml")
                  ;;
                  ;; ------ Emacs ------
                  ;; Emacs life
                  ("https://planet.emacslife.com/atom.xml")
                  ;; Emacs China
                  ("https://emacs-china.org/latest.rss")
                  ;;
                  ;; ------ AI ------
                  ;; 机器之心
                  ("https://plink.anyfeeder.com/weixin/almosthuman2014")
                  ;; 新智元
                  ("https://plink.anyfeeder.com/weixin/AI_era")
                  ))
  :config
  (elfeed-db-ensure))

(use-package elfeed-goodies
  :ensure t
  :after elfeed
  :config
  (elfeed-db-ensure)
  (elfeed-goodies/setup)
  ;; Keep elfeed-goodies' entry rendering, but use the current window
  ;; instead of its popwin split-pane view.
  (setq elfeed-show-entry-switch #'switch-to-buffer
        elfeed-show-entry-delete #'elfeed-kill-buffer))

(use-package dashboard-elfeed
  :after dashboard
  :demand t
  :load-path (lambda () (concat +emacs/repo-directory "/site-lisp/"))
  :config
  (dashboard-elfeed-setup))



;; ---------------------------------------------------------------------------
;; Reference: commented-out reading-related packages (kept for future use).
;; ---------------------------------------------------------------------------

;; (use-package telega
;;   :ensure t
;;   :config
;;   (let* ((parts (split-string +emacs/proxy ":"))
;;          (server (car parts))
;;          (port (string-to-number (cadr parts))))
;;     (setq telega-proxies
;;           (list `(:server ,server
;;                           :port ,port
;;                           :enable t
;;                           :type (:@type "proxyTypeSocks5"))))))

;; (setq package-vc-allow-build-commands t)
;; (use-package reader
;;   ;; The upstream `all' target regenerates reader-autoloads.el without
;;   ;; package.el's load-path prelude, so only build the native module here.
;;   :preface
;;   (add-to-list 'load-path (expand-file-name "reader" package-user-dir))
;;   :vc (:url "https://github.com/Jamie-Cui/emacs-reader"
;;             :rev "master"
;;             :make "render-core.so"))

(provide 'init-reading)
;;; reading.el ends here
