(defun compile ()
  (interactive)
  (load-library "ox-reveal")
  (let ((org-html-inline-images t))
    (org-reveal-export-to-html)))

(defun sql-default-ansi ()
  (sql-highlight-ansi-keywords))

(require 'sql)
(add-to-list 'sql-mode-hook 'sql-default-ansi)

(setq org-export-filter-final-output-functions
      '((lambda (f b x)
          (with-temp-buffer
            (insert f)

            (goto-char (point-min))
            (if (re-search-forward "<li>.*Thank.*</li>" nil t)
                (replace-match "" nil nil))

            (goto-char (point-min))
            (if (re-search-forward "<li>.*About me.*</li>" nil t)
                (replace-match "" nil nil))

            (buffer-string)))))

(compile)
