(require 'ox-publish)
(require 'eob-utils)


(defun eob-generate-category(base-dir site-sub-dir)
  (interactive)
    "Generate necessary \"categories.org\" for posts."
    (let* ((sub-dir (file-name-as-directory
                     (expand-file-name site-sub-dir base-dir)))
           (categories-file (expand-file-name "categories.org" base-dir))
           (absolute-org-files (eob-get-base-files sub-dir))
           (categories (ht-create)))
      (dolist (org-file absolute-org-files)
        (let* ((path-title-pair
                (cons (concat site-sub-dir
                              "/"
                              (s-chop-prefix sub-dir org-file))
                      (org-org-get-file-title org-file)))
               (category (org-org-get-file-category org-file))
               (ht-category (ht-get categories category)))
          (if category
              (ht-set categories category (cons path-title-pair ht-category)))))
       (setq category-keys-origin (ht-keys categories))
       (setq category-keys (sort category-keys-origin (lambda (a b) (string< (downcase a) (downcase b)))))
       (setq category-list-ht (list ()))
       (setq category-entry-ht (list()))
       (dolist (category category-keys)
	 (add-to-list 'category-list-ht (ht ("category-name" category) ("category-number"  (length (ht-get categories category)))))
	 (setq category-this-list (list()))
	 (dolist (path-title-pair (ht-get categories category))
	   (setq path (car path-title-pair))
	   (setq filenameHTML (concat (file-name-sans-extension path) ".html" ))
	   (add-to-list 'category-this-list (ht ("category-url" (concat "/" filenameHTML)) ("category-title" (cdr path-title-pair))))
	 )
	 (add-to-list 'category-entry-ht (ht ("category-name" category) ("category-entry-list" (reverse (butlast category-this-list)))))
       )
    (setq content-all (ht-create))
    (ht-set content-all "post-title" "Categories")
    (ht-set content-all "site-title" eob-title)
    (ht-set content-all "site-author" eob-author-name)
    (ht-set  content-all "category-entry" (reverse (butlast category-entry-ht)))
    (ht-set  content-all "category-list" (reverse (butlast category-list-ht)))
    (ht-set content-all "head" (eob-generate-head nil "Categories"))
    (ht-set content-all "navigation" (eob-generate-navigation))
    (setq fileHTMLSave (expand-file-name "category/index.html" eob-publish-directory) )
    (make-directory (file-name-directory fileHTMLSave) t)
       (with-temp-buffer
	 (insert (eob-render "category.html" content-all))
	 (when (file-writable-p fileHTMLSave)
	   (write-region (point-min)
                      (point-max)
		      fileHTMLSave)))
   )
)
  

(provide 'eob-generator-category)
