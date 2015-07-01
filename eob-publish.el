(require 's)
(require 'ox-publish)
(require 'eob-utils)
(require 'eob-generator-index)
(require 'eob-generator-category)
(require 'eob-generator-archive)
(require 'eob-generator-post)



(defun eob-pre-publish (base-dir)
  (eob-generate-index base-dir "post")
  (eob-generate-archive base-dir "post")
  (eob-generate-category base-dir "post")
  (eob-generate-about base-dir)
  (eob-generate-post base-dir "post")
)

(defun eob-post-publish (base-dir)
  (copy-directory  (expand-file-name (format "themes/%s/static" eob-theme) eob-project-directory) eob-publish-directory)
  (copy-directory (expand-file-name "images" eob-project-directory) eob-publish-directory)
  (copy-directory (expand-file-name "sourcecodes" eob-project-directory) eob-publish-directory)
)

(defun eob-publish (project-dir &optional republish localhost)
(interactive
   (list (read-directory-name "Project directory: " eob-project-directory)
         (y-or-n-p "Force republish all? ")
         (unless (s-matches? "localhost" eob-url)
           (y-or-n-p "Force publish in localhost mode? "))))
(eob-pre-publish project-dir)
(eob-post-publish project-dir)
)

(provide 'eob-publish)

