(require 'ht)
(require 'mustache)
(require 'eob-vars)

(defun file-to-string (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun mustache-file-render (file context)
  (mustache-render (file-to-string file) context))

(defun org-html-get-body (org-html)
  (let ((body-regexp "<body>\\(.*\\|\n\\)*</body>"))
    (car (s-match body-regexp org-html))))

(defun org-org-get-file-title (org-file)
     (with-temp-buffer
      (insert-file-contents org-file)
      (setq opt-list (org-export--get-inbuffer-options)))
      (car(plist-get opt-list :title)))

(defun org-html-get-body-toc (org-html)
  (let ((org-html-body (org-html-get-body org-html))
        (toc-regexp "<div id=\"table-of-.*\">\\(.\\|\n\\)*?</div>\n</div>"))
    (car (s-match toc-regexp org-html-body))))

(defun eob-get-entry (org-file)
  (interactive)
  (let ((file-all (file-to-string org-file))
        (more-regexp "#\\+Title.*\\(.\\|\n\\)*?<!-- more -->"))
    (setq result (car (s-match more-regexp file-all)))
    (if (equal result nil)
     (with-temp-buffer
      (insert-file-contents org-file nil 0 500)
      (org-html-export-as-html nil nil nil t nil)
      (setq org-html (substring-no-properties (car kill-ring)))
      (setq content (org-html-get-content-without-toc org-html))
     )
     (with-temp-buffer
      (insert result)
      (org-html-export-as-html nil nil nil t nil)
      (setq org-html (substring-no-properties (car kill-ring)))
      (setq content (org-html-get-content-without-toc org-html))
     )
    )
    )
)

(defun org-html-get-body-content (org-html)
  (let ((org-html-body (org-html-get-body org-html))
        (body-regexp "</?body?>")
        (title-regexp "<h1 class=\"title\">.*?</h1>")
        (postamble-regexp "<div id=\"postamble\" class=\"status\">\\(.\\|\n\\)*?</div>")
        (toc-regexp "<div id=\"table-of-.*\">\\(.\\|\n\\)*?</div>")
        (text-toc-regexp "<div id=\"text-table-of-.*\">\\(.\\|\n\\)*?</div>"))
    (s-trim
     (reduce
      #'(lambda (regexp string)
          (replace-regexp-in-string regexp "" string))
      (list body-regexp title-regexp toc-regexp text-toc-regexp postamble-regexp)
      :initial-value org-html-body
      :from-end t))))

(defun org-html-get-content-without-toc (org-html)
 (let ((toc-regexp "<div id=\"table-of-.*\">\\(.\\|\n\\)*?</div>")
        (text-toc-regexp "<div id=\"text-table-of-.*\">\\(.\\|\n\\)*?</div>"))
    (s-trim
     (reduce
      #'(lambda (regexp string)
          (replace-regexp-in-string regexp "" string))
      (list toc-regexp text-toc-regexp)
      :initial-value org-html
      :from-end t)))
  )


(defun org-org-get-file-properties (org-file)
  (let ((org-file-string-list
         (s-lines (file-to-string org-file)))
        (prop-regexp "^#\\+\\(.*?\\):[ \t]+\\(.*\\)")
        (prop-dict (ht-create)))
    (dolist (line org-file-string-list)
      (setq match-data (s-match prop-regexp line))
      (setq prop-key (nth 1 match-data))
      (setq prop-value (nth 2 match-data))
      (ht-set prop-dict prop-key prop-value))
    (ht-remove prop-dict nil)
    prop-dict))


(defun org-org-get-file-mtime (org-file)
  (let* ((attrs (file-attributes org-file))
         (mtime (nth 5 attrs))
         (org-file-prop-dict (org-org-get-file-properties org-file))
         (org-file-date (ht-get org-file-prop-dict "DATE")))
    (if org-file-date
        (setq mtime
              (apply #'encode-time
                     (org-parse-time-string org-file-date))))
    mtime))

(defun org-org-get-file-category (org-file)
   (let ((CATEGORY (ht-get (org-org-get-file-properties org-file)
                          "CATEGORY")))
    (if CATEGORY
        (s-trim CATEGORY)
      nil)))

(defun eob-new-project (&optional project-directory)
   (interactive "GProject directory: ")
  (unless project-directory
    (setq project-directory default-directory))
  (unless (file-exists-p project-directory)
    (make-directory project-directory))
  (setq old-default-directory default-directory)
  (unwind-protect
      (progn
        (cd project-directory)
        (make-directory "post")
	(make-directory "images")
	(make-directory "sourcecodes")
        (copy-file (expand-file-name "eob-config.el"
                                     eob-load-directory)
                   project-directory)
        (copy-directory (expand-file-name "themes"
                    eob-load-directory)
                   project-directory)
        (cd old-default-directory))))

(defun eob-load-project (&optional project-directory)
   (interactive
   (list (read-directory-name "Project directory: " eob-project-directory)))
  (unless project-directory
    (setq project-directory default-directory))
  (setq old-default-directory default-directory)
  (unwind-protect
      (progn
        (cd project-directory)
        (load-file "eob-config.el"))
    (cd old-default-directory))
  (setq eob-project-directory project-directory)
  (setq eob-publish-directory (file-name-as-directory (expand-file-name eob-html-publish-dir eob-project-directory))))

(defun eob-load-template (theme template)
  (expand-file-name
   (format "themes/%s/template/%s" theme template) eob-project-directory))


(defun eob-new-org-file (org-file &optional view-org-file)
  (if (file-exists-p org-file)
      (error "File already exists, please type a new file."))
  (let ((buffer (find-file-noselect org-file)))
    (set-buffer buffer)
    ;(org-export-insert-default-template default)
    (insert "#+Title: \n")
    (insert "#+DATE: " (format-time-string "%Y-%m-%d"))
    (insert "\n")
    (insert "#+OPTIONS: toc:nil num:nil\n")
    (insert "#+CATEGORY:\n")
    (insert "#+TAGS:\n\n\n")
    (insert "# <!-- more -->\n")
    (save-buffer)
    (if view-org-file
        (switch-to-buffer buffer)
      (kill-buffer buffer))))

 (defun eob-new-post (org-file)
    (interactive
     (list (read-file-name
            "file name: "
             (concat eob-project-directory (concat "post/" (format-time-string "%Y-%m-%d-"))
                               ))))
    (eob-new-org-file org-file t))

(defun eob-render (template context)
  (mustache-file-render
   (eob-load-template eob-theme template)
   context))

(defun eob-generate-footer ()
  (let ((context
         (ht-from-plist
          `("author-email" ,eob-author-email
            "author-name" ,eob-author-name))))
    (eob-render "footer.html" context)))


(defun eob-generate-head (&optional org-file post-title)
    (setq context (ht-create))
    (ht-set context "site-title" eob-title)
    (if post-title (ht-set context "post-title" post-title) (ht-set context "post-title" (org-org-get-file-title org-file)) )
    (eob-render "head.html" context))

(defun eob-generate-navigation ()
  (let ((context
         (ht-from-plist
          `("site-title" ,eob-title))))
    (eob-render "navigation.html" context)))

(defun eob-get-base-files (base-dir &optional extension)
  (interactive)
  (let ((org-publish-sitemap-requested nil)
        (org-publish-temp-files nil)
        (match (concat "^[^\\.].*\\.\\("
                       (if extension extension "org")
                       "\\)$")))
    (org-publish-get-base-files-1 base-dir t match "index" nil)
    org-publish-temp-files))

(defun eob-get-posts-sorted-time(base-dir site-sub-dir)
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
    )
    (setq post-list-ht (list ()))
    (dotimes (i (length path-title-mtime-org-files))
      (add-to-list 'post-list-ht (ht ("org-file" (car (nth i path-title-mtime-org-files))) ("id" (number-to-string i))))
      )
    (setq results (reverse (butlast post-list-ht)))
)


(defun eob-get-posts-with-properties(base-dir site-sub-dir)
  (interactive)
  (let ((post-list-sorted-time (eob-get-posts-sorted-time base-dir site-sub-dir)))
  (setq post-list-p (list()))
  (dotimes (i (length post-list-sorted-time))
    (setq org-file-full-path (expand-file-name (ht-get (nth i post-list-sorted-time) "org-file") eob-project-directory))
    (setq org-inner-properties (org-org-get-file-properties org-file-full-path))
    (setq post-ht (ht-merge  org-inner-properties  (nth i post-list-sorted-time)))
    (ht-set post-ht "org-file-full-path" org-file-full-path)
    (ht-set post-ht "post-url" (concat "/" (concat (file-name-sans-extension (ht-get post-ht "org-file")) ".html" )))
    (ht-set post-ht "DATE" (format-time-string "%Y-%m-%d" (org-org-get-file-mtime (ht-get post-ht "org-file-full-path"))))
    (ht-set post-ht "Title"  (org-org-get-file-title (ht-get post-ht "org-file-full-path")))
    (add-to-list 'post-list-p post-ht)
    ))
   (setq post-list-p (reverse (butlast post-list-p)))
  (setq post-list-p-all (list()))
  (dotimes (i (length post-list-p))
    (setq temp (nth i post-list-p))
    (if (= i 0) (ht-set temp "post-pre" nil) (ht-set temp "post-pre" (nth (1- i) post-list-p)))
    (if (= i (1- (length post-list-p))) (ht-set temp "post-next" nil) (ht-set temp "post-next" (nth (1+ i) post-list-p)))
    (add-to-list 'post-list-p-all temp)
    )
  (setq results (reverse (butlast post-list-p-all)))
)


(defun eob-get-common-info()
    (interactive)
    (let ((common-info (ht-create)))
	  (ht-set common-info "site-title" eob-title)
	  (ht-set common-info "site-author" eob-author-name)
	  (ht-set common-info  "duoshuo-shortname" "sunzhennian")
    (setq result common-info))
    )

(defun eob-deploy()
  (interactive)
  (progn
    (shell "deploy")
    (set-buffer "deploy")
    (insert (format "cd %s && git add -A . && git commit -m \"Site-update at %s\" && git push " eob-publish-directory (format-time-string "%Y-%m-%d %T"))) 
  )  
)

(provide 'eob-utils)
