(require 'cl-lib)

(cl-defun agent-skills/list-buffers ()
  "Return a list with all the buffer names."
  (mapcar #'buffer-name (buffer-list)))

(cl-defun agent-skills/read-buffer (buffer-name)
  "Return the content of BUFFER-NAME as a string."
  (let ((buf (get-buffer buffer-name)))
    (unless buf
      (error "Buffer %s does not exist" buffer-name))
    (with-current-buffer buf
      (buffer-substring-no-properties (point-min) (point-max)))))

(provide 'agent-skills/buffer)
