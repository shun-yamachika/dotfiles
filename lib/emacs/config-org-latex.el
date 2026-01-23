; -*- Emacs-Lisp -*-
;;
;; Org-mode LaTeX Export Configuration
;;
;; このファイルはorg-modeからLaTeXへのエクスポート設定を管理します
;; 新しいドキュメントクラスを追加する場合は、セクション2を参照してください
;;

;;; ==========================================================================
;;; 1. 基本設定
;;; ==========================================================================

;; LaTeXコンパイラのデフォルト設定（platex使用）
;; 注意：個別のorgファイルで #+LATEX_COMPILER: pdflatex などで上書き可能
(setq org-latex-compiler "platex")

;; デフォルトのPDFビルドプロセス（platex + pbibtex + dvipdfmx）
;; 注意：個別のorgファイルで #+BIND: org-latex-pdf-process (...) で上書き可能
(setq org-latex-pdf-process
      '("platex -interaction=nonstopmode -output-directory=%o %f"
        "pbibtex %b"
        "platex -interaction=nonstopmode -output-directory=%o %f"
        "platex -interaction=nonstopmode -output-directory=%o %f"
        "dvipdfmx %b.dvi"))

;; 参考：pdflatex用のプロセス（必要に応じて使用）
;; #+BIND: org-latex-pdf-process で以下を指定することも可能
(setq org-latex-pdf-process-pdflatex
      '("pdflatex -interaction=nonstopmode -output-directory=%o %f"
        "bibtex %b"
        "pdflatex -interaction=nonstopmode -output-directory=%o %f"
        "pdflatex -interaction=nonstopmode -output-directory=%o %f"))

;; BibTeXを使わない場合のプロセス（参考）
;; (setq org-latex-pdf-process-no-bib
;;       '("platex -interaction=nonstopmode -output-directory=%o %f"
;;         "platex -interaction=nonstopmode -output-directory=%o %f"
;;         "dvipdfmx %b.dvi"))

;; org-mode内での画像表示設定
(setq org-image-actual-width nil)

;; LaTeX数式プレビュー設定
(setq org-latex-create-formula-image-program 'dvipng)

;; ソースコードのシンタックスハイライト有効化
(setq org-src-fontify-natively t)

;; exportで\maketitleを自動挿入しない（手動で配置するため）
(setq org-latex-title-command "")


;;; ==========================================================================
;;; 2. ドキュメントクラス定義
;;; ==========================================================================
;;
;; 新しいドキュメントクラスを追加する場合の手順：
;;
;; 1. 以下のテンプレートをコピーして、適切な位置に追加
;; 2. クラス名、documentclassオプション、セクション構造を編集
;; 3. 必要に応じて、セクション3のフックを追加（\date{}, \title{}削除など）
;;
;; テンプレート：
;; ----------------------------------------
;; ;; <クラス名>クラス（<用途の説明>）
;; (add-to-list 'org-latex-classes
;;              '("<クラス名>"
;;                "\\documentclass[<オプション>]{<クラス名>}
;; [NO-DEFAULT-PACKAGES]
;; [PACKAGES]
;; [EXTRA]"
;;                ("\\section{%s}" . "\\section*{%s}")
;;                ("\\subsection{%s}" . "\\subsection*{%s}")
;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;                ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
;; ----------------------------------------
;;
;; [NO-DEFAULT-PACKAGES] の説明：
;;   hyperrefなどのデフォルトパッケージを除外します
;;   clsファイルとパッケージが競合する場合に使用してください
;;

;; IEEEtranクラス（IEEE国際会議論文用）
;; 使用例: #+LATEX_CLASS: IEEEtran
;;        #+LATEX_CLASS_OPTIONS: [conference]
;;        #+LATEX_COMPILER: platex
;;        #+BIND: org-latex-pdf-process ("platex ..." "pbibtex %b" ...)
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

;; コメントアウト例：ieicejクラス（IEICE技術研究会用）
;; 必要に応じてコメントを外して使用
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

;; コメントアウト例：ieicejspクラス（IEICE学生ポスターセッション用）
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

;; bachelorクラス（卒業論文用）
;; 使用例: #+LATEX_CLASS: bachelor
;; 注意：macroディレクトリのパスを環境変数TEXINPUTS等で設定する必要があります
;;       例: export TEXINPUTS=.:~/latexsample/paper-templates/bachelor/macro:
(add-to-list 'org-latex-classes
             '("bachelor"
               "\\documentclass[12pt,a4paper,titlepage,dvipdfmx]{jarticle}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


;;; ==========================================================================
;;; 3. ドキュメントクラス固有のフック・アドバイス
;;; ==========================================================================

;; \date{}, \title{}, \hypersetup{}を出力しない設定
;; （ieicejクラスおよびIEEEtranクラス使用時）
;;
;; これらのクラスでは、タイトルや日付を独自のコマンドで管理するため
;; org-modeが自動生成する\date{}, \title{}などが不要です
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

;; 新しいクラスで同様の処理が必要な場合：
;; 上記の関数内の (string-match ...) 部分に、新しいクラス名を追加してください
;; 例：
;;   (when (or (string-match "\\\\documentclass.*{ieicej}" new-contents)
;;             (string-match "\\\\documentclass.*{IEEEtran}" new-contents)
;;             (string-match "\\\\documentclass.*{新しいクラス名}" new-contents))


;;; ==========================================================================
;;; 4. Beamer関連設定
;;; ==========================================================================

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


;;; ==========================================================================
;;; 5. その他の実験的設定（コメントアウト）
;;; ==========================================================================

;; paperi.org用の進捗度自動更新（texファイル直接書き換え方式）
;; 現在は使用していないため、コメントアウト
;; (defun my-update-progress-in-tex (orig-fun &rest args)
;;   "org-latex-compile実行前にtexファイルの進捗度を更新する"
;;   (let ((tex-file (car args)))
;;     (when (and tex-file
;;                (file-exists-p tex-file)
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
;;
;; ;; org-latex-compileにadviceを追加
;; (advice-add 'org-latex-compile :around #'my-update-progress-in-tex)

;;; config-org-latex.el ends here
