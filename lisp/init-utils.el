;;; init-utils.el --- utility functions and macros -*- lexical-binding: t -*-
;;; Commentary:
;;; Core utility functions and macros used across configuration
;;; Code:

(defvar-local +syntax-ppss-memo-last-point nil)
(defvar-local +syntax-ppss-memo-last-result nil)

(defun +wc/non-ascii (&optional start end)
  (interactive)
  (let ((start (if mark-active (region-beginning) (point-min)))
        (end (if mark-active (region-end) (point-max))))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char start)
        (message "lines: %3d non ascii words: %3d chars: %3d"
                 (count-lines start end)
                 (count-matches "[^[:ascii:]]")
                 (- end start))))))

(defun +syntax-ppss-memo-reset-h (&rest _ignored)
  "Reset memoization as a safety precaution.

IGNORED is a dummy argument used to eat up arguments passed from
the hook where this is executed."
  (setq +syntax-ppss-memo-last-point nil
        +syntax-ppss-memo-last-result nil))

;;;###autoload
(defun +syntax-ppss (&optional p)
  "Memoize the last result of `syntax-ppss'.

P is the point at which we run `syntax-ppss'"
  (let ((p (or p (point)))
        (mem-p +syntax-ppss-memo-last-point))
    (if (and (eq p (nth 0 mem-p))
             (eq (point-min) (nth 1 mem-p))
             (eq (point-max) (nth 2 mem-p)))
        +syntax-ppss-memo-last-result
      ;; Add hook to reset memoization if necessary
      (unless +syntax-ppss-memo-last-point
        (add-hook 'before-change-functions #'+syntax-ppss-memo-reset-h t t))
      (setq +syntax-ppss-memo-last-point (list p (point-min) (point-max))
            +syntax-ppss-memo-last-result (syntax-ppss p)))))

;;;###autoload
(defun +surrounded-by-pairs-p (pair &optional inline balanced)
  "Returns t if point is surrounded by a brace delimiter: {[(

If INLINE is non-nil, only returns t if braces are on the same line, and
whitespace is balanced on either side of the cursor.

If INLINE is nil, returns t if the opening and closing braces are on adjacent
lines, above and below, with only whitespace in between."
  (when pair
    (let ((beg (plist-get pair :beg))
          (end (plist-get pair :end))
          (pt (point)))
      (when (and (> pt beg) (< pt end))
        (when-let* ((cl (plist-get pair :cl))
                    (op (plist-get pair :op)))
          (and (not (string= op ""))
               (not (string= cl ""))
               (let ((nbeg (+ (length op) beg))
                     (nend (- end (length cl))))
                 (let ((content (buffer-substring-no-properties nbeg nend)))
                   (and (string-match-p (format "[ %s]*" (if inline "" "\n")) content)
                        (or (not balanced)
                            (= (- pt nbeg) (- nend pt))))))))))))

;;;###autoload
(defun +point-in-comment-p (&optional pt)
  "Return non-nil if point is in a comment.
PT defaults to the current position."
  (let ((pt (or pt (point))))
    (ignore-errors
      (save-excursion
        ;; We cannot be in a comment if we are inside a string
        (unless (nth 3 (+syntax-ppss pt))
          (or (nth 4 (+syntax-ppss pt))
              ;; this also test opening and closing comment delimiters... we
              ;; need to chack that it is not newline, which is in "comment
              ;; ender" class in elisp-mode, but we just want it to be treated
              ;; as whitespace
              (and (< pt (point-max))
                   (memq (char-syntax (char-after pt)) '(?< ?>))
                   (not (eq (char-after pt) ?\n)))
              ;; we also need to test the special syntax flag for comment
              ;; starters and enders, because `syntax-ppss' does not yet know if
              ;; we are inside a comment or not (e.g. / can be a division or
              ;; comment starter...).
              (when-let ((s (car (syntax-after pt))))
                (or (and (/= 0 (logand (ash 1 16) s))
                         (nth 4 (syntax-ppss (+ pt 2))))
                    (and (/= 0 (logand (ash 1 17) s))
                         (nth 4 (syntax-ppss (+ pt 1))))
                    (and (/= 0 (logand (ash 1 18) s))
                         (nth 4 (syntax-ppss (- pt 1))))
                    (and (/= 0 (logand (ash 1 19) s))
                         (nth 4 (syntax-ppss (- pt 2))))))))))))

;;;###autoload
(defun +point-in-string-p (&optional pt)
  "Return non-nil if point is inside string.

This function actually returns the 3rd element of `syntax-ppss'
which can be a number if the string is delimited by that
character or t if the string is delimited by general string
fences.

If optional argument PT is present test this instead of point."
  (ignore-errors
    (save-excursion
      (nth 3 (+syntax-ppss pt)))))

;;;###autoload
(defun +point-in-string-or-comment-p (&optional pos)
  "Return non-nil if POS is in a string or comment."
  (or (+point-in-string-p pos)
      (+point-in-comment-p pos)))

;;;###autoload
(defun +region-active-p ()
  "Return non-nil if selection is active.
Detects evil visual mode as well."
  (declare (side-effect-free t))
  (or (use-region-p)
      (and (bound-and-true-p evil-local-mode)
           (evil-visual-state-p))))

;;;###autoload
(defun +region-beginning ()
  "Return beginning position of selection.
Uses `evil-visual-beginning' if available."
  (declare (side-effect-free t))
  (or (and (bound-and-true-p evil-local-mode)
           (evil-visual-state-p)
           (markerp evil-visual-beginning)
           (marker-position evil-visual-beginning))
      (region-beginning)))

;;;###autoload
(defun +region-end ()
  "Return end position of selection.
Uses `evil-visual-end' if available."
  (declare (side-effect-free t))
  (or (and (bound-and-true-p evil-local-mode)
           (evil-visual-state-p)
           (markerp evil-visual-end)
           (marker-position evil-visual-end))
      (region-end)))

;;;###autoload
(defun +region-bounds (&optional as-list)
  "Return the bounds of the current seelction.

If AS-LIST is non-nil, returns (BEG END). Otherwise returns a cons cell (BEG .
END)."
  (let* ((active (+region-active-p))
         (beg (if active (+region-beginning)))
         (end (if active (+region-end))))
    (if as-list
        (list beg end)
      (cons beg end))))

;;;###autoload
(defun +thing-at-point-or-region (&optional thing prompt)
  "Grab the current selection, THING at point, or xref identifier at point.

Returns THING if it is a string. Otherwise, if nothing is found at point and
PROMPT is non-nil, prompt for a string (if PROMPT is a string it'll be used as
the prompting string). Returns nil if all else fails.

NOTE: Don't use THING for grabbing symbol-at-point. The xref fallback is smarter
in some cases."
  (declare (side-effect-free t))
  (cond ((stringp thing)
         thing)
        ((+region-active-p)
         (buffer-substring-no-properties
          (+region-beginning)
          (+region-end)))
        (thing
         (thing-at-point thing t))
        ((require 'xref nil t)
         ;; Eglot, nox (a fork of eglot), and elpy implementations for
         ;; `xref-backend-identifier-at-point' betray the documented purpose of
         ;; the interface. Eglot/nox return a hardcoded string and elpy prepends
         ;; the line number to the symbol.
         (if (memq (xref-find-backend) '(eglot elpy nox))
             (thing-at-point 'symbol t)
           ;; A little smarter than using `symbol-at-point', though in most
           ;; cases, xref ends up using `symbol-at-point' anyway.
           (xref-backend-identifier-at-point (xref-find-backend))))
        (prompt
         (read-string (if (stringp prompt) prompt "")))))


;;
;;; Commands

(defun +bol-bot-eot-eol (&optional pos)
  (save-mark-and-excursion
    (when pos
      (goto-char pos))
    (let* ((bol (if visual-line-mode
                    (save-excursion
                      (beginning-of-visual-line)
                      (point))
                  (line-beginning-position)))
           (bot (save-excursion
                  (goto-char bol)
                  (skip-chars-forward " \t\r")
                  (point)))
           (eol (if visual-line-mode
                    (save-excursion (end-of-visual-line) (point))
                  (line-end-position)))
           (eot (or (save-excursion
                      (if (not comment-use-syntax)
                          (progn
                            (goto-char bol)
                            (when (re-search-forward comment-start-skip eol t)
                              (or (match-end 1) (match-beginning 0))))
                        (goto-char eol)
                        (while (and (+point-in-comment-p)
                                    (> (point) bol))
                          (backward-char))
                        (skip-chars-backward " " bol)
                        (or (eq (char-after) 32)
                            (eolp)
                            (bolp)
                            (forward-char))
                        (point)))
                    eol)))
      (list bol bot eot eol))))

(defvar +last-backward-pt nil)
;;;###autoload
(defun +backward-to-bol-or-indent (&optional point)
  "Jump between the indentation column (first non-whitespace character) and the
beginning of the line. The opposite of
`+forward-to-last-non-comment-or-eol'."
  (interactive "^d")
  (let ((pt (or point (point))))
    (cl-destructuring-bind (bol bot _eot _eol)
        (+bol-bot-eot-eol pt)
      (cond ((> pt bot)
             (goto-char bot))
            ((= pt bol)
             (or (and +last-backward-pt
                      (= (line-number-at-pos +last-backward-pt)
                         (line-number-at-pos pt)))
                 (setq +last-backward-pt nil))
             (goto-char (or +last-backward-pt bot))
             (setq +last-backward-pt nil))
            ((<= pt bot)
             (setq +last-backward-pt pt)
             (goto-char bol))))))

(defvar +last-forward-pt nil)
;;;###autoload
(defun +forward-to-last-non-comment-or-eol (&optional point)
  "Jumps between the last non-blank, non-comment character in the line and the
true end of the line. The opposite of `+backward-to-bol-or-indent'."
  (interactive "^d")
  (let ((pt (or point (point))))
    (cl-destructuring-bind (_bol _bot eot eol)
        (+bol-bot-eot-eol pt)
      (cond ((< pt eot)
             (goto-char eot))
            ((= pt eol)
             (goto-char (or +last-forward-pt eot))
             (setq +last-forward-pt nil))
            ((>= pt eot)
             (setq +last-backward-pt pt)
             (goto-char eol))))))

;;;###autoload
(defun +backward-kill-to-bol-and-indent ()
  "Kill line to the first non-blank character. If invoked again afterwards, kill
line to beginning of line. Same as `evil-delete-back-to-indentation'."
  (interactive)
  (let ((empty-line-p (save-excursion (beginning-of-line)
                                      (looking-at-p "[ \t]*$"))))
    (funcall (if (fboundp 'evil-delete)
                 #'evil-delete
               #'delete-region)
             (line-beginning-position) (point))
    (unless empty-line-p
      (indent-according-to-mode))))

;;;###autoload
(defun +delete-backward-word (arg)
  "Like `backward-kill-word', but doesn't affect the kill-ring."
  (interactive "p")
  (let ((kill-ring nil) (kill-ring-yank-pointer nil))
    (ignore-errors (backward-kill-word arg))))

;;;###autoload
(defun +dumb-indent ()
  "Inserts a tab character (or spaces x tab-width)."
  (interactive)
  (if indent-tabs-mode
      (insert "\t")
    (let* ((movement (% (current-column) tab-width))
           (spaces (if (= 0 movement) tab-width (- tab-width movement))))
      (insert (make-string spaces ? )))))

;;;###autoload
(defun +dumb-dedent ()
  "Dedents the current line."
  (interactive)
  (if indent-tabs-mode
      (call-interactively #'backward-delete-char)
    (unless (bolp)
      (save-excursion
        (when (> (current-column) (current-indentation))
          (back-to-indentation))
        (let ((movement (% (current-column) tab-width)))
          (delete-char
           (- (if (= 0 movement)
                  tab-width
                (- tab-width movement)))))))))

;;;###autoload
(defun +retab (arg &optional beg end)
  "Converts tabs-to-spaces or spaces-to-tabs within BEG and END (defaults to
buffer start and end, to make indentation consistent. Which it does depends on
the value of `indent-tab-mode'.

If ARG (universal argument) is non-nil, retab the current buffer using the
opposite indentation style."
  (interactive "P\nr")
  (unless (and beg end)
    (setq beg (point-min)
          end (point-max)))
  (let ((indent-tabs-mode (if arg (not indent-tabs-mode) indent-tabs-mode)))
    (if indent-tabs-mode
        (tabify beg end)
      (untabify beg end))))

;;;###autoload
(defun +delete-trailing-newlines ()
  "Trim trailing newlines.

Respects `require-final-newline'."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (delete-blank-lines)))

;;;###autoload
(defun +dos2unix ()
  "Convert the current buffer to a Unix file encoding."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

;;;###autoload
(defun +unix2dos ()
  "Convert the current buffer to a DOS file encoding."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

;;;###autoload
(defun +toggle-indent-style ()
  "Switch between tabs and spaces indentation style in the current buffer."
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (message "Indent style changed to %s" (if indent-tabs-mode "tabs" "spaces")))

(defvar editorconfig-lisp-use-default-indent)
;;;###autoload
(defun +set-indent-width (width)
  "Change the indentation size to WIDTH of the current buffer.

The effectiveness of this command is significantly improved if you have
editorconfig or dtrt-indent installed."
  (interactive
   (list (if (integerp current-prefix-arg)
             current-prefix-arg
           (read-number "New indent size: "))))
  (setq tab-width width)
  (setq-local standard-indent width)
  (when (boundp 'evil-shift-width)
    (setq evil-shift-width width))
  ;; NOTE Only use `editorconfig' once we drop 29.x support.
  (cond ((let ((load-path (get 'load-path 'initial-value)))
           ;; A built-in `editorconfig' package was added in Emacs 30.x, but
           ;; with a different API. Since it's built in, prefer it over the
           ;; upstream one, but we still need to adapt:
           (if (require 'editorconfig nil t)
               (fboundp #'editorconfig--default-indent-size-function)))
         (pcase-dolist (`(,var . ,val) (editorconfig--default-indent-size-function width))
           (set (make-local-variable var) val)))
        ((require 'editorconfig nil t)
         (let (editorconfig-lisp-use-default-indent)
           (editorconfig-set-indentation nil width)))
        ((require 'dtrt-indent nil t)
         (when-let (vars (nth 2 (assq major-mode dtrt-indent-hook-mapping-list)))
           (dolist (var (ensure-list vars))
             (message "Updated %s = %d" var width)
             (set var width)))))
  (message "Changed buffer's indent-size to %d" width))


;;
;;; Hooks

;;;###autoload
(defun +enable-delete-trailing-whitespace-h ()
  "Enables the automatic deletion of trailing whitespaces upon file save.

i.e. enables `ws-butler-mode' in the current buffer."
  (ws-butler-mode +1))

;;;###autoload
(defun +disable-delete-trailing-whitespace-h ()
  "Disables the automatic deletion of trailing whitespaces upon file save.

i.e. disables `ws-butler-mode' in the current buffer."
  (ws-butler-mode -1))

;;;###autoload
(defun +enable-show-trailing-whitespace-h ()
  "Enable `show-trailing-whitespace' in the current buffer."
  (setq-local show-trailing-whitespace t))

;;;###autoload
(defun +disable-show-trailing-whitespace-h ()
  "Disable `show-trailing-whitespace' in the current buffer."
  (setq-local show-trailing-whitespace nil))


;; NOTE Since I'm using evil, i want an additional space when inserting
;;###autoload
(defun +evil/smart-insert ()
  "Enter insert mode smartly: if current char is whitespace, append after word."
  (interactive)
  (let ((c (char-after (point)))
        (eow (and (not (eobp)) (looking-at "\\b"))))
    (message "current char: %c" c)
    (when (not eow)
      (message "moving to the end of word")
      (call-interactively 'evil-forward-word-end))
    (call-interactively 'evil-append) ;; append
    ))

(provide 'init-utils)
