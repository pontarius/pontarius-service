#!/usr/bin/emacs --script
(package-initialize)
(require 'nxml-mode)

(defun format-xml ()
  (interactive)
  (nxml-mode)
  (goto-char (point-min))
  (while (search-forward "><" nil t)
    (replace-match ">\n<"))
  (goto-char (point-min))
  (indent-region (point-min) (point-max)))
