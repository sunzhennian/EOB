(require 'ox-publish)
(require 'eob-utils)


(defun eob-generate-archive(base-dir site-sub-dir)
  (interactive)
  (let* ((sub-dir (file-name-as-directory
                   (expand-file-name site-sub-dir base-dir)))
         (absolute-org-files (eob-get-base-files sub-dir)))
    (setq path-title-mtime-org-files
          (mapcar (lambda (filename)
                    (list (concat site-sub-dir
                                  "/"
                                  (s-chop-prefix sub-dir filename))
                          (org-org-get-file-title filename)
                          (org-org-get-file-mtime filename)))
                  absolute-org-files))

    (setq path-title-mtime-org-files
          (sort path-title-mtime-org-files
                #'(lambda (path-title-mtime1 path-title-mtime2)
                    (not (time-less-p (nth 2 path-title-mtime1)
                                      (nth 2 path-title-mtime2))))))
   (setq archives (ht-create))
   (dolist (org-file path-title-mtime-org-files)
     (setq date (format-time-string "%Y-%m-%d" (caddr org-file)))
     (setq year (car (split-string date "-")))
      (setq ht-year (ht-get archives year))
      (ht-set archives year (cons org-file ht-year))
      )
   (setq archive-list (list()))
   (setq archive-keys (ht-keys archives))
   (dolist (archive-year archive-keys)
     (setq archive-this-year (list()))
     (dolist (org-file (ht-get archives archive-year))
       (setq path (car org-file))
       (setq filenameHTML (concat (file-name-sans-extension path) ".html" ))
       (setq post-title (cadr org-file))
       (add-to-list 'archive-this-year (ht ("archive-date" (format-time-string "%Y-%m-%d" (caddr org-file))) ("archive-url" (concat "/" filenameHTML)) ("archive-title" post-title)))
       )
     (add-to-list 'archive-list (ht ("archive-year-list" (butlast archive-this-year)) ("archive-year" archive-year)))
     )
   (setq fileHTMLSave (expand-file-name "archive/index.html" eob-publish-directory) )
   (make-directory (file-name-directory fileHTMLSave) t)
   (setq content-all (ht-create))
    (ht-set content-all "post-title" "Archives")
    (ht-set content-all "site-title" eob-title)
    (ht-set content-all "site-author" eob-author-name)
    (ht-set  content-all "archive-list" (butlast archive-list))
    (ht-set content-all "head" (eob-generate-head nil "Archives"))
    (ht-set content-all "navigation" (eob-generate-navigation))
   (with-temp-buffer
         (insert (eob-render "archive.html" content-all))
	 (when (file-writable-p fileHTMLSave)
	   (write-region (point-min)
                      (point-max)
		      fileHTMLSave)))
    )
)
  

(provide 'eob-generator-archive)
