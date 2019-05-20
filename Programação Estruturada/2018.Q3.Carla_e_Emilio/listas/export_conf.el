;; Use package preamble
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa"     . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu"       . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("org"       . "http://orgmode.org/elpa/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;;causes the package(s) to be installed automatically if not already present
(setq use-package-always-ensure t)
(setq use-package-always-defer t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(use-package org
  :config
  (use-package org-plus-contrib)
  (require 'ox-latex)
  (use-package gnuplot)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (latex . t)
     (gnuplot . t)
     ))


  ;; Adiciona funcionalidade de NÃO exportar uma coluna
  (defun dfeich/org-export-delete-commented-cols (back-end)
    "Delete columns $2 to $> marked as `<#>' on a row with `/' in $1.
    If you want a non-empty column $1 to be deleted make it $2 by
    inserting an empty column before and adding `/' in $1."
    (while (re-search-forward "^[ \t]*| +/ +|\\(.*|\\)? +\\(<#>\\) *|" nil t)
      (goto-char (match-beginning 2))
      (org-table-delete-column)
      (beginning-of-line)))
  (add-hook 'org-export-before-processing-hook #'dfeich/org-export-delete-commented-cols)

  (defun emilio/reenable-whitespace-mode ()
    (unless (bound-and-true-p my-global-whitespace-mode)
      (call-interactively 'my-global-whitespace-mode)))

  (defun emilio/temporarily-disable-whitespace-mode ()
    (when (bound-and-true-p my-global-whitespace-mode)
      (progn
        (call-interactively 'my-global-whitespace-mode)
        (run-with-idle-timer 10 nil 'emilio/reenable-whitespace-mode))))

  (defun emilio/org-disable-modes-before-export (backend)
    "Disable modes that would interfere in export"
    (emilio/temporarily-disable-whitespace-mode))
  (add-hook 'org-export-before-processing-hook #'emilio/org-disable-modes-before-export)

  (setq org-export-coding-system 'utf-8)
  (setq org-confirm-babel-evaluate nil)
  (setq org-html-validation-link nil)
  (setq org-list-allow-alphabetical t)
  (setq org-latex-compiler "xelatex")
  (setq org-latex-pdf-process
        '("latexmk -xelatex -shell-escape -f -output-directory=%o %f"))
  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("cachedir=/tmp/minted-\jobname" "minted" t))
  (add-to-list 'org-latex-packages-alist '("" "xcolor" nil))
  (add-to-list 'org-latex-packages-alist '("" "fontspec" t ("xelatex")))
  )
