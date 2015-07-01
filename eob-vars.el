
(defgroup eob nil
  "Options for eob, a site generator based on Emacs org-mode."
  :tag "Emacs org-mode based site generator"
  :group 'org)

(defconst eob-load-directory
  (file-name-directory load-file-name))

(defconst eob-project-directory nil)
(defconst eob-publish-directory nil)

(provide 'eob-vars)

