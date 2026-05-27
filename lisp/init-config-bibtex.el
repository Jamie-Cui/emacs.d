;;; init-config-bibtex.el --- BibTeX configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'bibtex)

;; Configure uniquification (appends a/b/c for duplicates)
(setopt bibtex-autokey-name-case-convert-function #'upcase)
(setopt bibtex-autokey-names 4)
(setopt bibtex-autokey-additional-names "+")
(setopt bibtex-autokey-name-stretch 0)
(setopt bibtex-autokey-name-length 1)
(setopt bibtex-autokey-year-length 4)
(setopt bibtex-autokey-titlewords 0)
(setopt bibtex-autokey-titlewords-stretch 0)
(setopt bibtex-autokey-name-year-separator "")

;; HACK override this function from built-in
;; maybe report upstream?
(defun bibtex-autokey-abbrev (string len)
  "Return an abbreviation of STRING with at least LEN characters.
If LEN is positive the abbreviation is terminated only after a consonant
or at the word end.  If LEN is negative the abbreviation is strictly
enforced using abs (LEN) characters.  If LEN is not a number, STRING
is returned unchanged."
  (cond ((or (not (numberp len))
             (<= (length string) (abs len)))
         string)
        ((equal len 0)
         "")
        ((equal len 1)
         (substring string 0 1))
        ((< len 0)
         (substring string 0 (abs len)))
        (t (let* ((case-fold-search t)
                  (abort-char (string-match "[^aeiou]" string (1- len))))
             (message "%s" abort-char)
             (if abort-char
                 (substring string 0 (1+ abort-char))
               string)))))

(provide 'init-config-bibtex)
;;; init-config-bibtex.el ends here
