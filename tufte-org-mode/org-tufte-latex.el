;;; org-tufte-latex.el --- Org export engine Tufte-LaTeX backend -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Sets up minor mode `org-tufte-latex-minor-mode' to support exporting Org
;; files to Tufte-LaTeX

;; This loads ox-tufte-latex.el that adds a new Tufte-LaTeX Org export backend
;; as well as sets up helper functions and some buffer-local LaTeX defaults,
;; e.g., the default LaTeX class while `org-tufte-latex-minor-mode' is active
;; is "tufte-handout" and macros are created for Tufte-LaTeX specific commands
;; like \newthought, \sidenote and \marginnote

;; Additionally, there is ebib citation support, setting up a link type
;; "tufte-ebib" for `ebib-insert-citation' that on exports to LaTeX expands
;; "[[tufte-ebib:key][Preamble text::Pre-note::Post-note]]" to
;; "Preamble text\autocite[Pre-note][Post-note]{key}"

;; Enable with `org-tufte-latex-minor-mode' or add the following to an Org file:
;; =====
;; * Local variables :noexport:
;;   #+begin_src org
;;   Local Variables:
;;   eval: (org-tufte-latex-minor-mode 1)
;;   End:
;;   #+end_src
;; =====

;; Requires ox-extra.el (in the org-plus-contrib ELPA package) and
;; ox-tufte-latex.el (download from https://github.com/tsdye/tufte-org-mode)

;; Also requires these CTAN packages be installed (e.g. via tlmgr):
;;   changepage fancyhdr geometry hyperref natbib
;;   paralist placeins ragged2e setspace textcase
;;   titlesec xcolor xifthen bera psnfss oberdiek
;;   iftex microtype mathpazo soul etex etexcmds
;;   biblatex booktabs graphics hyphenat marginfix
;;   amsmath morefloats l3packages xpatch hycolor
;;   pdfescape letltxmacro ltxcmds kvsetkeys
;;   kvdefinekeys bigintcalc intcalc atbegshi
;;   atveryend bitset rerunfilecheck epstopdf-pkg
;;   uniquecounter refcount gettitlestring
;;   hardwrap xltxtra realscripts
;;   imakeidx fbb tufte-latex

;;; Code:

(require 'cl-extra)
(require 'cl-seq)
(require 'subr-x)

(require 'org)

;; nodes with :ignore: tag are skipped but not their children when exporting
(require 'ox-extra)
(ox-extras-activate '(latex-header-blocks ignore-headlines))

(defun tufte-latex-org-kwds ()
  "Parse the buffer and return a cons list of (property . value)
from lines like: #+PROPERTY: value"
  (org-element-map (org-element-parse-buffer 'element) '(keyword node-property)
    (lambda (keyword) (cons (org-element-property :key keyword)
                            (org-element-property :value keyword)))))

(defun tufte-latex-org-kwd (KEYWORD)
  "Get the value of a KEYWORD in the form of \"#+KEYWORD: value\""
  (or (cdr (assoc KEYWORD (tufte-latex-org-kwds))) ""))

(defun insert-tufte-ebib-org-mode-cite-command (lst)
  "Check if LST is an `org-mode' cite command list,
and if so returns a modified list with the tufte-ebib cite command."
  (if (eq 'org-mode (car lst))
      (cons (car lst)
            (list (cl-remove-duplicates
                   (cons '("tufte-ebib" "[[tufte-ebib:%K][%D]]")
                         (car (cdr lst)))
                   :test (lambda (x y) (string= (car x) (car y)))
                   :from-end t)))
    lst))

(defun org-tufte-latex-ebib-export (path desc format)
  "Export an ebib link.
See `org-link-parameters' for details about PATH, DESC and FORMAT."
  (let* ((link-desc (or desc ""))
         (desc-parts (split-string link-desc "::"))
         (desc-name (car desc-parts))
         (desc-pre-note (or (nth 1 desc-parts) ""))
         (desc-post-note (mapconcat 'identity (nthcdr 2 desc-parts) "::")))
    (cond
     ((eq format 'latex)
      (if desc
          (format "%s\\autocite%s%s{%s}"
                  (concat desc-name " ")
                  (if (string= "" desc-pre-note) "" (format "[%s]" desc-pre-note))
                  (if (string= "" desc-post-note) "" (format "[%s]" desc-post-note))
                  path)
        (format "\\autocite{%s}" path))))))

(defun org-tufte-latex-minor-mode--setup ()
  "Setup code for Org Tufte-LaTeX minor mode."
  ;; load necessary Emacs packages
  (require 'ox-tufte-latex)
  (require 'ebib)
  ;; change default LaTeX class for Org documents
  (setq-local org-latex-default-class "tufte-handout")
  ;; add Tufte-LaTeX book and handout classes
  (setq-local
    my-org-tufte-latex-latex-class-base-packages
    "\\usepackage[p,osf]{fbb}
     \\usepackage{booktabs,graphicx,microtype,hyphenat,amsmath}
     \\geometry{paperheight=10.5in,paperwidth=8.5in,textwidth=4.375in}
     \\titleformat{\\part}[display]{\\relax\\itshape\\huge}{}{0pt}{\\huge\\rmfamily\\itshape}[]
     \\setmarginnotefont{\\itshape\\footnotesize}
     \\usepackage[backend=biber,style=verbose-trad1]{biblatex}
     % Use imakeidx to create an index
     \\usepackage{imakeidx}
     \\makeindex[intoc=true]
     [NO-DEFAULT-PACKAGES]")
  (setq-local org-latex-classes (copy-tree org-latex-classes))
  (add-to-list
   'org-latex-classes
   `("tufte-book" ;; chapters grouped into parts
     ,(concat "\\documentclass[twoside,nobib]{tufte-book}"
              "\n"
              my-org-tufte-latex-latex-class-base-packages)
     ("\\part{%s}" . "\\part*{%s}") ;; comment this line if book has no parts
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")))
  (add-to-list
   'org-latex-classes
   `("tufte-book-noparts" ;; chapters not grouped into parts
     ,(concat "\\documentclass[twoside,nobib]{tufte-book}"
              "\n"
              my-org-tufte-latex-latex-class-base-packages)
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")))
  (add-to-list
   'org-latex-classes
   `("tufte-handout"
     ,(concat "\\documentclass[twoside,nobib]{tufte-handout}"
              "\n"
              my-org-tufte-latex-latex-class-base-packages)
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")))
  (kill-local-variable 'my-org-tufte-latex-latex-class-base-packages)
  ;; make sure local LaTeX compile process uses pdflatex and biber
  (setq-local org-latex-pdf-process
              '("pdflatex -interaction nonstopmode -output-directory %o %f"
                "biber %b"
                "pdflatex -interaction nonstopmode -output-directory %o %f"
                "pdflatex -interaction nonstopmode -output-directory %o %f"))
  ;; no table of contents
  (setq-local org-export-with-toc nil)
  ;; ebib support
  (setq-local ebib-citation-commands
              (mapcar 'insert-tufte-ebib-org-mode-cite-command
                      ebib-citation-commands))
  ;; update org-link-parameters to add ebib support
  (setq-local org-link-parameters (copy-tree org-link-parameters))
  (org-link-set-parameters "tufte-ebib"
                           :follow #'org-ebib-open
                           :store #'org-ebib-store-link
                           :export #'org-tufte-latex-ebib-export)
  ;; Tufte-LaTeX macros with default behavior when exporting via non-Tufte-LaTeX backend
  ;; See https://github.com/fniessen/org-macros for other useful macros
  ;; "{{{newthought}}}" expands to "\newthought{$1}"
  ;; "{{{sidenote(p1,p2,p3)}}}" expands to "\sidenote[p3][p2]{p1}"
  ;; "{{{marginnote(p1,p2)}}}" expands to "\marginnote[p2]{p1}"
  ;; "{{{index(p1)}}}" expands to "\index{p1}"
  (setq-local
   org-export-global-macros
   '(("newthought" . "(eval (if (org-export-derived-backend-p org-export-current-backend 'tufte-latex) (concat \"\\\\newthought{\" $1 \"}\") (upcase $1)))")
     ("sidenote" . "(eval (if (org-export-derived-backend-p org-export-current-backend 'tufte-latex) (concat \"\\\\sidenote[\" $3 \"][\" $2 \"]{\" $1 \"}\") (concat \"[fn::\" $1 \"]\")))")
     ("marginnote" . "(eval (if (org-export-derived-backend-p org-export-current-backend 'tufte-latex) (concat \"\\\\marginnote[\" $2 \"]{\" $1 \"}\") (concat \"[fn::\" $1 \"]\")))")
     ;; Indexing macro, see https://orgmode.org/manual/Quoting-LaTeX-code.html
     ("index" . "@@latex:\index{$1}@@"))))

(defun org-tufte-latex-minor-mode--teardown ()
  "Teardown code for Org Tufte-LaTeX minor mode."
  (kill-local-variable 'ebib-citation-commands)
  (kill-local-variable 'org-export-global-macros)
  (kill-local-variable 'org-export-with-toc)
  (kill-local-variable 'org-latex-default-class)
  (kill-local-variable 'org-latex-pdf-process)
  (kill-local-variable 'org-latex-classes)
  (kill-local-variable 'org-link-parameters))

(define-minor-mode org-tufte-latex-minor-mode
  "For editing Org files aimed at generating Tufte-LaTeX files."
  ;; initial value
  nil
  ;; mode line indicator
  " OrgTL"
  ;; minor mode bindings
  nil
  ;; startup code
  (if (symbol-value org-tufte-latex-minor-mode)
      (org-tufte-latex-minor-mode--setup)
    (org-tufte-latex-minor-mode--teardown)))

(defun org-tufte-latex-make-copyright-page (copyrightdate rightholder publisher website printordinal printdate)
    "Creates a Tufte-LaTeX book copyright page.
It will have the COPYRIGHTDATE, RIGHTHOLDER, PUBLISHER, WEBSITE,
PRINTORDINAL (e.g. \"First edition, second printing\") and
the PRINTDATE. Typically it is used within an emacs-lisp source block.
Example usage within an Org document:
#+begin_export latex
(org-tufte-latex-make-copyright-page \"1970\"
                               \"Author Or Right Holder Name\"
                               \"Some Publisher Group\"
                               \"http://this.is.not.a.real.site\"
                               \"First edition, second printing\"
                               \"January 1970\")
#+end_export"
    (format "
#+begin_export LATEX
%% \\newpage
\\begin{fullwidth}
~\\vfill
\\thispagestyle{empty}
\\setlength{\\parindent}{0pt}
\\setlength{\\parskip}{\\baselineskip}
%% Work around no \copyright in the T1 encoding using lmr font OT1 encoding
\\renewcommand*\\copyright{{\\usefont{OT1}{lmr}{m}{n}\\textcopyright}}
Copyright \\copyright\\ %s %s

\\par\\smallcaps{Published by %s}

\\par\\smallcaps{%s}

\\par\\textit{%s, %s}
\\end{fullwidth}
#+end_export" copyrightdate rightholder publisher website printordinal printdate))

(provide 'org-tufte-latex)

;;; org-tufte-latex.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
