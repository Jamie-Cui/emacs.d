;;; magent-skills.el --- Claude Code skill loader  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui

;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Keywords: tools, ai
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Loader for Claude Code skills (SKILL.md format).
;; Scans agent-skills/ directory and makes skills available to magent agents.

;;; Code:

(require 'cl-lib)

(declare-function magent-log "magent-config")

;;; Skill data structure

(cl-defstruct magent-skill
  "Represents a loaded Claude Code skill."
  name                ; Skill name (string)
  description         ; Brief description
  tools               ; Required tools (e.g., "Bash")
  disable-invocation  ; Whether to disable model invocation
  body                ; Markdown body content
  dir)                ; Directory path

;;; Skill registry

(defvar magent-skills--registry nil
  "Alist of (skill-name . magent-skill) for loaded skills.")

(defun magent-skills-get (name)
  "Get skill by NAME from registry."
  (cdr (assoc name magent-skills--registry)))

(defun magent-skills-list ()
  "Return list of all loaded skill names."
  (mapcar #'car magent-skills--registry))

(defun magent-skills-clear ()
  "Clear skill registry."
  (setq magent-skills--registry nil))

;;; YAML frontmatter parsing

(defun magent-skills--parse-yaml-frontmatter (file)
  "Parse YAML frontmatter from FILE.
Returns a plist with :name, :description, :tools, :disable-model-invocation, :body."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))

    ;; Check for YAML frontmatter (--- at start)
    (unless (looking-at "^---$")
      (error "No YAML frontmatter found in %s" file))

    (forward-line 1)
    (let ((yaml-start (point))
          (yaml-end nil)
          (body-start nil))

      ;; Find end of frontmatter (second ---)
      (unless (re-search-forward "^---$" nil t)
        (error "Unterminated YAML frontmatter in %s" file))

      (setq yaml-end (match-beginning 0))
      (setq body-start (match-end 0))

      ;; Parse YAML fields
      (let ((yaml-text (buffer-substring-no-properties yaml-start yaml-end))
            (body (string-trim (buffer-substring-no-properties body-start (point-max))))
            (name nil)
            (description nil)
            (tools nil)
            (disable-invocation nil))

        ;; Simple YAML parser (only handles simple key: value pairs)
        (with-temp-buffer
          (insert yaml-text)
          (goto-char (point-min))

          ;; Parse name
          (when (re-search-forward "^name:\\s-*\\(.+\\)$" nil t)
            (setq name (match-string 1)))

          ;; Parse description (may be quoted)
          (goto-char (point-min))
          (when (re-search-forward "^description:\\s-*['\"]?\\(.+?\\)['\"]?$" nil t)
            (setq description (match-string 1)))

          ;; Parse tools
          (goto-char (point-min))
          (when (re-search-forward "^tools:\\s-*\\(.+\\)$" nil t)
            (setq tools (match-string 1)))

          ;; Parse disable-model-invocation
          (goto-char (point-min))
          (when (re-search-forward "^disable-model-invocation:\\s-*\\(true\\|false\\)$" nil t)
            (setq disable-invocation (string= (match-string 1) "true"))))

        (list :name name
              :description description
              :tools tools
              :disable-model-invocation disable-invocation
              :body body)))))

;;; Skill loading

(defun magent-skills--load-skill (skill-dir)
  "Load skill from SKILL-DIR containing SKILL.md.
Returns a magent-skill struct or nil if loading fails."
  (let ((skill-md (expand-file-name "SKILL.md" skill-dir)))
    (unless (file-exists-p skill-md)
      (magent-log "WARN no SKILL.md in %s, skipping" skill-dir)
      (cl-return-from magent-skills--load-skill nil))

    (condition-case err
        (let ((parsed (magent-skills--parse-yaml-frontmatter skill-md)))
          (unless (plist-get parsed :name)
            (magent-log "ERROR no name field in %s" skill-md)
            (cl-return-from magent-skills--load-skill nil))

          (magent-skill-create
           :name (plist-get parsed :name)
           :description (plist-get parsed :description)
           :tools (plist-get parsed :tools)
           :disable-invocation (plist-get parsed :disable-model-invocation)
           :body (plist-get parsed :body)
           :dir skill-dir))
      (error
       (magent-log "ERROR loading skill from %s: %s" skill-dir (error-message-string err))
       nil))))

(defun magent-skills-load-all (&optional skills-dir)
  "Load all skills from SKILLS-DIR (defaults to agent-skills/ in +emacs/repo-directory).
Populates `magent-skills--registry'."
  (interactive)

  (let* ((base-dir (or skills-dir
                       (expand-file-name "agent-skills" +emacs/repo-directory)))
         (skill-dirs (when (file-directory-p base-dir)
                       (directory-files base-dir t "^[^.]" t))))

    (unless skill-dirs
      (magent-log "WARN no skills directory found at %s" base-dir)
      (cl-return-from magent-skills-load-all))

    (magent-log "INFO loading skills from %s" base-dir)

    (let ((loaded 0))
      (dolist (dir skill-dirs)
        (when (file-directory-p dir)
          (let ((skill (magent-skills--load-skill dir)))
            (when skill
              (push (cons (magent-skill-name skill) skill)
                    magent-skills--registry)
              (cl-incf loaded)
              (magent-log "INFO loaded skill: %s" (magent-skill-name skill))))))

      (magent-log "INFO loaded %d skill(s) total" loaded))))

;;; Skill invocation helpers

(defun magent-skills--find-impl-file (skill-dir skill-name)
  "Find implementation .el file for SKILL-NAME in SKILL-DIR.
Looks for agent-skills-<name>.el or agent-skills/<name>.el."
  (let ((patterns (list
                   (format "agent-skills-%s.el" skill-name)
                   (format "agent-skills/%s.el" skill-name))))
    (cl-loop for pattern in patterns
             for path = (expand-file-name pattern skill-dir)
             when (file-exists-p path)
             return path)))

(provide 'magent-skills)

;;; magent-skills.el ends here
