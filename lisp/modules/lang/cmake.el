;;; cmake.el --- CMake support and font-locking -*- lexical-binding: t -*-
;;; Commentary:
;; CMake support and font-locking.
;;; Code:

;; ------------------------------------------------------------------
;; NOTE fix cmake-ts-mode syntax table
;; ------------------------------------------------------------------

(defun +cmake-ts-mode/fix-syntax-table ()
  (modify-syntax-entry ?/ "-"))

(defconst +cmake-ts-mode/boolean-constants
  '("ON" "OFF" "TRUE" "FALSE" "YES" "NO" "Y" "N"
    "On" "Off" "True" "False" "Yes" "No")
  "Boolean-like CMake constants to highlight.")

(defconst +cmake-ts-mode/command-options
  '("ALIAS" "ALL" "AND" "APPEND" "ARGS" "AUTHOR_WARNING" "BEFORE" "BOOL" "CACHE"
    "COMMAND" "COMMANDS" "COMMENT" "COMPONENT" "COMPONENTS" "CONFIG"
    "CONFIGS" "CONFIGURE_DEPENDS" "DEFINED" "DEPENDS" "DESCRIPTION"
    "DESTINATION" "DIRECTORY" "EQUAL" "ERROR_QUIET" "EXCLUDE_FROM_ALL"
    "EXISTS" "EXPORT" "FATAL_ERROR" "FILE" "FILEPATH" "FILES"
    "FILES_MATCHING" "FORCE"
    "GLOB" "GLOB_RECURSE" "GREATER" "GREATER_EQUAL" "HOMEPAGE_URL" "IMPORTED"
    "IN" "INCLUDES" "INTERFACE" "INTERNAL" "ITEMS" "LANGUAGE" "LANGUAGES" "LESS"
    "LESS_EQUAL" "LIBRARY" "LISTS" "MATCHES" "MODULE" "NAME" "NAMES"
    "NAMESPACE" "NO_DEFAULT_PATH" "NOT" "NOTICE" "OBJECT" "OPTIONAL"
    "OPTIONAL_COMPONENTS" "OR" "OUTPUT" "PATH" "PATH_EQUAL" "PATTERN" "PERMISSIONS"
    "POLICY" "PRIVATE" "PROGRAMS" "PROPERTIES" "PROPERTY" "PUBLIC" "QUIET"
    "RANGE" "REQUIRED" "RESULT_VARIABLE" "RUNTIME" "SEND_ERROR" "SHARED"
    "STATIC" "STATUS" "STRING" "STREQUAL" "STRGREATER" "STRGREATER_EQUAL" "STRINGS"
    "STRLESS" "STRLESS_EQUAL" "SYSTEM" "TARGET" "TARGETS" "TYPE" "VALUE"
    "VERBATIM" "VERSION" "VERSION_EQUAL" "VERSION_GREATER"
    "VERSION_GREATER_EQUAL" "VERSION_LESS" "VERSION_LESS_EQUAL" "WARNING"
    "WORKING_DIRECTORY" "ZIP_LISTS")
  "Common CMake command option words to highlight.")

(defconst +cmake-ts-mode/boolean-constant-regexp
  (concat "\\`" (regexp-opt +cmake-ts-mode/boolean-constants) "\\'"))

(defconst +cmake-ts-mode/command-option-regexp
  (concat "\\`" (regexp-opt +cmake-ts-mode/command-options) "\\'"))

(defun +cmake-ts-mode/add-font-lock-feature (feature)
  "Add FEATURE to the highest `treesit-font-lock-feature-list' level."
  (unless (memq feature (apply #'append treesit-font-lock-feature-list))
    (setq-local treesit-font-lock-feature-list
                (append (butlast treesit-font-lock-feature-list)
                        (list (append (car (last treesit-font-lock-feature-list))
                                      (list feature)))))))

(defun +cmake-ts-mode/set-proper-font-lock-level ()
  (setq-local treesit-font-lock-level 4)
  (treesit-font-lock-recompute-features))

(defun +cmake-ts-mode/fontify-command-options ()
  "Add extra font-lock rules for CMake option arguments."
  (+cmake-ts-mode/add-font-lock-feature 'cmake-boolean-constant)
  (+cmake-ts-mode/add-font-lock-feature 'cmake-command-option)
  (treesit-add-font-lock-rules
   (treesit-font-lock-rules
    :language 'cmake
    :feature 'cmake-boolean-constant
    :override 'keep
    `(((unquoted_argument) @font-lock-constant-face
       (:match ,+cmake-ts-mode/boolean-constant-regexp
               @font-lock-constant-face)))

    :language 'cmake
    :feature 'cmake-command-option
    :override 'keep
    `(((unquoted_argument) @font-lock-keyword-face
       (:match ,+cmake-ts-mode/command-option-regexp
               @font-lock-keyword-face))))
   :after 'keyword)
  (font-lock-flush))

(add-hook 'cmake-ts-mode-hook #'+cmake-ts-mode/fix-syntax-table)
(add-hook 'cmake-ts-mode-hook #'+cmake-ts-mode/set-proper-font-lock-level)
(add-hook 'cmake-ts-mode-hook #'+cmake-ts-mode/fontify-command-options)

(use-package eldoc-cmake
  :ensure t
  :hook (cmake-ts-mode . eldoc-cmake-enable))

(provide 'init-lang-cmake)
;;; cmake.el ends here
