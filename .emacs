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
;; --- dired-narrow setup ---
;; dired-narrowのload-pathを追加
(add-to-list 'load-path "~/.emacs.d/quelpa/build/dired-narrow")
(add-to-list 'load-path "~/.emacs.d/quelpa/build/dash")
(require 'dired-narrow nil 'noerror)

;; 好みでキーバインド
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "/") #'dired-narrow)
  (define-key dired-mode-map (kbd "C-c C-n") #'dired-narrow-fuzzy))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(counsel ivy dired-narrow dired-hacks-utils dash)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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



;; org-mode LaTeX export設定
;; (require 'ox-latex) は config-org.el で既に読み込まれているのでコメントアウト

(setq org-latex-compiler "platex")

;; PDFビルドプロセスの設定（platex -> pbibtex -> dvipdfmx）
;; BibTeXを使う場合はこちらを使用
(setq org-latex-pdf-process
      '("platex -interaction=nonstopmode -output-directory=%o %f"
        "pbibtex %b"
        "platex -interaction=nonstopmode -output-directory=%o %f"
        "platex -interaction=nonstopmode -output-directory=%o %f"
        "dvipdfmx %b.dvi"))

;; pdflatex用のPDFビルドプロセス（IEEEtran等で使用）
(setq org-latex-pdf-process-pdflatex
      '("pdflatex -interaction=nonstopmode -output-directory=%o %f"
        "bibtex %b"
        "pdflatex -interaction=nonstopmode -output-directory=%o %f"
        "pdflatex -interaction=nonstopmode -output-directory=%o %f"))


;; IEEEtran document classの定義（IEEE国際会議用）
(add-to-list 'org-latex-classes
             '("IEEEtran"
               "\\documentclass[conference]{IEEEtran}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; \date{}、\title{}、\hypersetupを出力しない設定（ieicejクラスおよびIEEEtranクラス使用時）
;; org-latex-template関数をアドバイスして不要なコマンドを削除
(defun my-org-latex-remove-date-title (contents)
  "Remove \\date{}, \\title{}, and \\hypersetup{} from LaTeX output for ieicej and IEEEtran classes."
  (let ((new-contents contents))
    ;; ieicejクラスまたはIEEEtranクラスを使っている場合のみ処理
    (when (or (string-match "\\\\documentclass.*{ieicej}" new-contents)
              (string-match "\\\\documentclass.*{IEEEtran}" new-contents))
      (setq new-contents (replace-regexp-in-string "\\\\date{[^}]*}\n" "" new-contents))
      (setq new-contents (replace-regexp-in-string "\\\\title{}\n" "" new-contents))
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

;; Beamer用のタイトルページ自動生成設定
(with-eval-after-load 'ox-beamer
  (defun my-org-beamer-title-command (contents info)
    "Beamerエクスポート時にタイトルページフレームを生成"
    (let ((title (org-export-data (plist-get info :title) info)))
      (if (org-string-nw-p title)
          "\\begin{frame}[plain]\n\\titlepage\n\\end{frame}\n\n"
        "")))

  ;; org-latex-title-commandをBeamerの場合のみオーバーライド
  (defun my-org-beamer-template-advice (orig-fun contents info)
    (let ((org-latex-title-command (my-org-beamer-title-command contents info)))
      (funcall orig-fun contents info)))

  (advice-add 'org-beamer-template :around #'my-org-beamer-template-advice))

;; IEEEtranクラス使用時はBeamerエクスポートを無効化
(defun my-org-export-disable-beamer-for-ieeetran (backend)
  "Disable Beamer export when using IEEEtran class."
  (when (and (eq backend 'beamer)
             (let ((class (org-export-data (plist-get (org-export-get-environment) :latex-class) nil)))
               (string= class "IEEEtran")))
    (user-error "IEEEtran class should use LaTeX export (C-c C-e l l), not Beamer export")))

;; org-latex-classesに登録されているクラスがBeamerテンプレートを使わないようにする
(with-eval-after-load 'ox-latex
  ;; LaTeXエクスポート時にBeamerのフックが呼ばれないようにする
  (when (boundp 'org-export-before-parsing-functions)
    (setq org-export-before-parsing-functions
          (remove 'org-beamer-p org-export-before-parsing-functions))))



;; BibTeXを使わない場合のプロセス（コメントアウトしておく）
;; (setq org-latex-pdf-process
;;       '("platex -interaction=nonstopmode -output-directory=%o %f"
;;         "platex -interaction=nonstopmode -output-directory=%o %f"
;;         "dvipdfmx %b.dvi"))

;; ;; ieicej document classの定義
;; ;; [NO-DEFAULT-PACKAGES]を使うことで、hyperrefなどのデフォルトパッケージを除外
;; (add-to-list 'org-latex-classes
;;              '("ieicej"
;;                "\\documentclass[technicalreport,dvipdfmx]{ieicej}
;; [NO-DEFAULT-PACKAGES]
;; [PACKAGES]
;; [EXTRA]"
;;                ("\\section{%s}" . "\\section*{%s}")
;;                ("\\subsection{%s}" . "\\subsection*{%s}")
;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;                ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; ;; ieicejsp document classの定義（学生ポスターセッション用）
;; (add-to-list 'org-latex-classes
;;              '("ieicejsp"
;;                "\\documentclass[twocolumn,a4paper,dvipdfmx]{ieicejsp}
;; [NO-DEFAULT-PACKAGES]
;; [PACKAGES]
;; [EXTRA]"
;;                ("\\section{%s}" . "\\section*{%s}")
;;                ("\\subsection{%s}" . "\\subsection*{%s}")
;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;                ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))




;; paperi.org用の進捗度自動更新（texファイル直接書き換え方式）
;; (defun my-update-progress-in-tex (orig-fun &rest args)
;;   "org-latex-compile実行前にtexファイルの進捗度を更新する"
;;   (let ((tex-file (car args)))
;;     (when (and tex-file
;; p               (file-exists-p tex-file)
;;                (string-match "paperi\\.tex$" tex-file))
;;       (let* ((script-path (concat (file-name-directory tex-file) "count-progress.sh"))
;;              (output (shell-command-to-string
;;                       (format "bash %s %s" script-path tex-file))))
;;         (when (string-match "\\\\progress{\\([0-9]+\\)}{\\([0-9]+\\)}{\\([0-9]+\\)}" output)
;;           (let ((jp-count (match-string 1 output))
;;                 (en-count (match-string 2 output))
;;                 (fig-count (match-string 3 output)))
;;             ;; texファイルを直接書き換え
;;             (with-temp-file tex-file
;;               (insert-file-contents tex-file)
;;               (goto-char (point-min))
;;               (when (re-search-forward "\\\\progress{[^}]*}{[^}]*}{[^}]*}" nil t)
;;                 (replace-match (format "\\\\progress{%s}{%s}{%s}"
;;                                        jp-count en-count fig-count))))
;;             (message "進捗度を自動更新しました: %s pt (日本語:%s 英語:%s 図:%s)"
;;                      (+ (* (string-to-number jp-count) 1.7)
;;                         (string-to-number en-count)
;;                         (* (string-to-number fig-count) 1500))
;;                      jp-count en-count fig-count))))))
;;   ;; 元の関数を実行（PDF生成）
;;   (apply orig-fun args))

;; ;; org-latex-compileにadviceを追加
;; (advice-add 'org-latex-compile :around #'my-update-progress-in-tex)
;; chatgpt-el の設定
(add-to-list 'load-path "/home/shun/chatgpt-el")
(autoload 'chatgpt-query "chatgpt" nil t)
(autoload 'chatgpt-query-api "chatgpt" nil t)
(autoload 'chatgpt-insert-reply "chatgpt" nil t)
(autoload 'chatgpt-fill "chatgpt" nil t)
(autoload 'chatgpt-select-engine "chatgpt" nil t)
(global-set-key "\C-cb" 'chatgpt-query)
(global-set-key "\C-cQ" 'chatgpt-insert-reply)
(global-set-key "\C-cf" 'chatgpt-fill)
(global-set-key "\C-cE" 'chatgpt-select-engine)
(setq chatgpt-engine "ChatGPT")
(setq chatgpt-prog "/home/shun/chatgpt-el/chatgpt-cdp")


;;find-fileの強化版

;; Ivy（補完システム）
(use-package ivy
  :ensure t
  :diminish
  :bind (("C-s" . swiper))  ;; 検索も強化される
  :config
  (ivy-mode 1))

;; Counsel（Ivyを拡張して find-file や検索を強化）
(use-package counsel
  :ensure t
  :bind (("C-x C-f" . counsel-find-file)  ;; ★ここで find-file を強化
         ("M-x" . counsel-M-x)            ;; M-x も強化される
         ("C-x C-r" . counsel-recentf))
  :config
  (counsel-mode 1))

(require 'ox)
(require 'ox-md)
(setq org-md-headline-style 'atx)  ;; # 見出し形式
(setq org-export-with-section-numbers nil)
(setq org-export-with-toc nil)
