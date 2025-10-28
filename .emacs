;; -*- lexical-binding: t; -*-

;; Configure load-path.
(push (expand-file-name "~/lib/emacs") load-path)

;; screen
(load "screen" t)

;; Swap BS and DEL.
(keyboard-translate ?\C-h ?\C-?)

;; Global keybindings
(global-set-key "\C-z\C-z" 'shell)
(global-set-key "\M-h" 'help-for-help)
;(global-set-key [mouse-4] 'previous-line)
;(global-set-key [mouse-5] 'next-line)
;(global-set-key [mouse-6] (lambda () (interactive)))

;; (my--define-fontset "test" "terminus 26" "kyokasho 24" t)
(defun my--define-fontset (fontset ascii-font unicode-font &optional activate)
  (let ((fontset-name (concat "fontset-" fontset)))
    (when (eq window-system 'x)
      (create-fontset-from-ascii-font ascii-font nil fontset)
      (set-fontset-font fontset-name 'unicode unicode-font nil 'append)
      (when activate
	(add-to-list 'default-frame-alist (cons 'font fontset-name))))))

(setq initial-buffer-choice t)


;;; Major mode
;; flylint
(autoload 'flylint-mode "flylint" nil t)

;; emacs-lisp
(global-eldoc-mode -1)
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (linum-mode 1)
	    (flylint-mode 1)))

;; c/c++
(setq c-default-style "gnu")
(add-hook 'c-mode-hook
  	  (lambda ()
	    (linum-mode 1)
	    (flylint-mode 1)))

;; perl
(add-hook 'perl-mode-hook
	  (lambda ()
	    (require 'perl-compl)
	    (local-set-key "\M-\t" 'complete-perl-symbol)
	    (local-set-key "\M-." 'find-perl-tag)
	    (local-set-key "\C-c\C-\M-\\" 'perltidy-buffer)
	    (linum-mode 1)
	    (flylint-mode 1)))

; python
(autoload 'py-mode "py-mode" nil t)
(autoload 'inf-py "py-mode" nil t)
(global-set-key "\C-cy" 'inf-py)
(add-to-list 'auto-mode-alist '("\\.py$" . py-mode))
(add-to-list 'interpreter-mode-alist (cons "python[0-9.]*" 'py-mode))
(add-hook 'py-mode-hook
  	  (lambda ()
	    (require 'py-compl)
	    (local-set-key "\M-\t" 'py-complete-symbol)
	    (local-set-key "\M-." 'py-find-tag)
	    (local-set-key "\C-c\C-\M-\\" 'py-format-buffer)
	    (linum-mode 1)
	    (flylint-mode 1)))

;; math
(autoload 'math-mode "math-mode" nil t)
(autoload 'inf-math "math-mode" nil t)
(global-set-key "\C-c," 'inf-math)
(add-to-list 'auto-mode-alist '("\\.m$" . math-mode))
(add-hook 'math-mode-hook
	  (lambda ()
	     (linum-mode 1)))

;; bibtex
(setq bibtex-autokey-name-case-convert 'capitalize)
(setq bibtex-autokey-titleword-case-convert 'capitalize)
(setq bibtex-autokey-titleword-separator "")
(setq bibtex-autokey-titleword-length 'infinity)
(setq bibtex-autokey-titlewords 1)
(setq bibtex-autokey-year-title-separator ":")

;; doc-view
;(setq doc-view-scale-internally nil)
(setq doc-view-image-width 1280)
(setq doc-view-resolution 300)
(setq doc-view-ghostscript-options
      '("-dSAFER" "-dNOPAUSE" "-sDEVICE=png16m" "-dTextAlphaBits=4" "-dBATCH"
	"-dGraphicsAlphaBits=4" "-dQUIET"
	"-dMaxBitmap=500000000" "-dAlignToPixels=0" "-dGridFitTT=2"))
(add-hook 'doc-view-mode-hook
  	  (lambda ()
	    (local-set-key "c" 'doc-annotate-add-annotation)
	    (local-set-key [mouse-1] 'doc-annotate-add-annotation)))

;; doc-annotate
(autoload 'doc-annotate-mode "doc-annotate")
(autoload 'doc-annotate-add-annotation "doc-annotate")
(add-to-list 'auto-mode-alist '("\\.ant$" . doc-annotate-mode))
(add-hook 'doc-annotate-mode-hook
  	  (lambda ()
	    (auto-fill-mode 1)))

;; pytrans
(autoload 'pytrans-translate "pytrans")
(global-set-key "\C-cT" 'pytrans-translate)

;; zf
(require 'zf)
;(zf-mode 1)

;; org
;; This must be loaded *before* org-mode initialization.
(load "config-org")

;; package
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.org/packages/")
	("org" . "http://orgmode.org/elpa/")))
(package-initialize)


;;; X11 specific.
(when (eq window-system 'x)
  (scroll-bar-mode -1)
  (my--define-fontset
   "hiro18"
   "-hiro-maru-medium-r-normal--18-170-75-75-c-90-iso8859-1"
   "-hiro-maru-medium-r-normal--18-170-75-75-c-170-jisx0208.1990-0"
   'activate)
  ;; Face.
  (dolist (elem '((bold "LightGoldenrod")
		  (underline "PaleGreen")
		  (mode-line "black" "PaleGreen3" bold)
		  (mode-line-inactive "PaleGreen" "black")
		  (link "PaleGreen")
		  (link-visited "salmon")
		  (font-lock-builtin-face "aquamarine1")
		  (font-lock-keyword-face "aquamarine1" nil bold)
		  (font-lock-constant-face "aquamarine2")
		  (font-lock-comment-face "orange")))
    (set-face-attribute (car elem) nil
			:foreground (nth 1 elem)
			:background (nth 2 elem)
			:weight (or (nth 3 elem) 'normal)))
  ;; hl-line mode
  (global-hl-line-mode t)
  (set-face-background 'hl-line "DarkSlateGray")
  ;; misc
  (setq mouse-yank-at-point t))

;(my--define-fontset
; "terminus"
; "terminus-18"
; "VL Gothic-17")


;; 追記-------------------------------------------------------
;; --- quelpa bootstrap ---
(unless (require 'quelpa nil 'noerror)
  (with-temp-buffer
    (url-insert-file-contents
     "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)))

;; --- dired-narrow install (if missing) ---
(unless (require 'dired-narrow nil 'noerror)
  (quelpa '(dired-narrow :fetcher github :repo "Fuco1/dired-hacks"))
  (require 'dired-narrow))  ;; ここで読み込んでおく

;; 好みでキーバインド
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "/") #'dired-narrow)
  (define-key dired-mode-map (kbd "C-c C-n") #'dired-narrow-fuzzy))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(dired-narrow dired-hacks-utils dash)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; org-mode LaTeX export設定
;; (require 'ox-latex) は config-org.el で既に読み込まれているのでコメントアウト

;; platexをデフォルトのLaTeXコンパイラに設定
(setq org-latex-compiler "platex")

;; PDFビルドプロセスの設定（platex -> pbibtex -> dvipdfmx）
;; BibTeXを使う場合はこちらを使用
(setq org-latex-pdf-process
      '("platex -interaction=nonstopmode -output-directory=%o %f"
        "pbibtex %b"
        "platex -interaction=nonstopmode -output-directory=%o %f"
        "platex -interaction=nonstopmode -output-directory=%o %f"
        "dvipdfmx %b.dvi"))

;; BibTeXを使わない場合のプロセス（コメントアウトしておく）
;; (setq org-latex-pdf-process
;;       '("platex -interaction=nonstopmode -output-directory=%o %f"
;;         "platex -interaction=nonstopmode -output-directory=%o %f"
;;         "dvipdfmx %b.dvi"))

;; ieicej document classの定義
;; [NO-DEFAULT-PACKAGES]を使うことで、hyperrefなどのデフォルトパッケージを除外
(add-to-list 'org-latex-classes
             '("ieicej"
               "\\documentclass[technicalreport,dvipdfmx]{ieicej}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; ieicejsp document classの定義（学生ポスターセッション用）
(add-to-list 'org-latex-classes
             '("ieicejsp"
               "\\documentclass[twocolumn,a4paper,dvipdfmx]{ieicejsp}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; \date{}、\title{}、\hypersetupを出力しない設定（ieicejクラス使用時のみ）
;; org-latex-template関数をアドバイスして不要なコマンドを削除
(defun my-org-latex-remove-date-title (contents)
  "Remove \\date{}, \\title{}, and \\hypersetup{} from LaTeX output for ieicej class."
  (let ((new-contents contents))
    ;; ieicejクラスを使っている場合のみ処理
    (when (string-match "\\\\documentclass.*{ieicej}" new-contents)
      (setq new-contents (replace-regexp-in-string "\\\\date{[^}]*}\n" "" new-contents))
      ;; (setq new-contents (replace-regexp-in-string "\\\\title{[^}]*}\n" "" new-contents))
      ;; \hypersetup{...}を削除（複数行にまたがる場合も対応）
      (setq new-contents (replace-regexp-in-string "\\\\hypersetup{\\(\n\\|.\\)*?}\n" "" new-contents)))
    new-contents))

(advice-add 'org-latex-template :filter-return #'my-org-latex-remove-date-title)

;; org-modeでの画像表示設定
(setq org-image-actual-width nil)

;; org-modeでのLaTeX数式プレビュー設定
(setq org-latex-create-formula-image-program 'dvipng)

;; ソースコードのシンタックスハイライト
(setq org-src-fontify-natively t)

;; exportで\maketitleを自動挿入しない（手動で配置するため）
(setq org-latex-title-command "")


;; scratch-bufferを呼び出すコマンド
(defun switch-to-scratch-buffer ()
    "Switch to *scratch* buffer, creating it if it doesn't exist."
    (interactive)
    (let ((scratch-buffer (get-buffer "*scratch*")))
      (if scratch-buffer
          (switch-to-buffer scratch-buffer)
        (switch-to-buffer (get-buffer-create "*scratch*"))
        (lisp-interaction-mode))))

  ;; キーバインドの例: C-c s で*scratch*バッファに切り替え
  (global-set-key (kbd "C-c s") 'switch-to-scratch-buffer)

;; 前のバッファに戻るコマンド
(defun switch-to-previous-buffer ()
  "Switch to the previously visited buffer in the buffer list."
  (interactive)
  (bury-buffer))

;; 次のバッファに進むコマンド
(defun switch-to-next-buffer ()
  "Switch to the next buffer in the buffer list."
  (interactive)
  (switch-to-buffer (car (last (buffer-list)))))

;; キーバインド: C-<tab> で前のバッファに切り替え
(global-set-key (kbd "C-<tab>") 'switch-to-previous-buffer)
;; キーバインド: C-<iso-lefttab> で次のバッファに切り替え
(global-set-key (kbd "C-<iso-lefttab>") 'switch-to-next-buffer)
