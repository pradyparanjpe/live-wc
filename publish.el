;;; .publish.el --- gitlab-ci pages -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Publish all *.org files to *.html for gitlab-pages export
;;
;;; Code:

(message "Install Emacs Packages")
(when (getenv "CI_PAGES_URL")
  (require 'package)
  (package-initialize)
  (add-to-list
   'package-archives '("elpa" . "https://elpa.gnu.org/packages/" ) t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (unless (package-installed-p 'htmlize)
    (package-refresh-contents)
    (package-install 'htmlize))
  (setq user-full-name nil))

;; org mode
(message "Require Org: x-html, x-publish")
(require 'org)
(require 'ox-publish)
(require 'ox-html)

(message "Add Read The Org header.")
(setq org-confirm-babel-evaluate nil)
(setq org-html-head-include-default-style nil)
(setq org-html-head "
<link rel=\"stylesheet//fniessen.github.io/org-html-themes/src/readtheorg_theme/css/htmlize.css\"/>
<link rel=\"stylesheet\" type=\"text/css\" href=\"https://fniessen.github.io/org-html-themes/src/readtheorg_theme/css/readtheorg.css\"/>

<script src=\"https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js\"></script>
<script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js\"></script>
<script type=\"text/javascript\" src=\"https://fniessen.github.io/org-html-themes/src/lib/js/jquery.stickytableheaders.min.js\"></script>
<script type=\"text/javascript\" src=\"https://fniessen.github.io/org-html-themes/src/readtheorg_theme/js/readtheorg.js\"></script>

<style>pre.src{background:#343131;color:white;} </style>

<script src=\"https://cdnjs.cloudflare.com/ajax/libs/jquery/3.5.1/jquery.min.js\"></script>
<script src=\"https://cdn.datatables.net/1.10.22/js/jquery.dataTables.min.js\"></script>
<script> $(\"table\").DataTable(); </script>
")

(message "Define publish sections")
(setq org-publish-project-alist
      '(("docs"
         :base-directory "doc/"
         :base-extension "org"
         :publishing-directory "doc/"
         :publishing-function org-html-publish-to-html
         :recursive t
         :auto-sitemap nil)
        ("org" :components ("docs"))))

(defun live-wc-publish--all ()
  "Publish everything."
  (mkdir "doc/" t)
  (message "Require Package")
  (add-to-list 'load-path (file-name-as-directory (expand-file-name ".")))
  (require 'live-wc)
  (message "Publish")
  (org-publish-all t nil))

(unless (getenv "CI_PAGES_URL")
  (live-wc-publish--all))

;;; publish.el ends here
