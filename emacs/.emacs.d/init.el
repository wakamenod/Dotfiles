;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; ======= Basic Setup ===========
(setq ring-bell-function 'ignore
      echo-keystrokes 0.5 ; shows which-key popup quickly
      dired-dwim-target t
      truncate-lines t
      truncate-partial-width-windows t
      mode-require-final-newline t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(desktop-save-mode t)
(save-place-mode 1)
(show-paren-mode t)
(column-number-mode t)
(winner-mode t)
(delete-selection-mode 1)
(setq-default indent-tabs-mode nil)   ; use space

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'alt)
(setq mac-control-modifier 'control)
(setq ns-function-modifier 'hyper)

(set-language-environment "Japanese")
(setenv "LANG" "ja_JP.UTF-8")

(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
(global-hl-line-mode t) ; high-light current line

(set-default-coding-systems 'utf-8) ; utf-8 にする
(prefer-coding-system 'utf-8)
(set-locale-environment "utf-8")

(let ((gls "/usr/local/bin/gls"))
  (if (file-exists-p gls) (setq insert-directory-program gls)))

;; ======= backup-files =========
;; http://yohshiy.blog.fc2.com/blog-entry-319.html
(setq backup-directory-alist '((".*" . "~/.ehist")))
;; 番号付けによる複数保存
(setq version-control     t)  ;; 実行の有無
(setq kept-new-versions   5)  ;; 最新の保持数
(setq kept-old-versions   1)  ;; 最古の保持数
(setq delete-old-versions t)  ;; 範囲外を削除

(setq auto-save-list-file-prefix nil)
(setq create-lockfiles nil)

;; from zenburn-with-color-variables
(defmacro with-my-color-variables (&rest body)
  "`let' bind all colors defined in `my-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  (let ((my-colors-alist '(
                           ("zenburn-fg-1"     . "#656555")

                           ("gruvbox-dark0_hard"   .   "#1d2021")
                           ("gruvbox-dark0"        .   "#282828")
                           ("gruvbox-dark0_soft"   .   "#32302f")
                           ("gruvbox-dark1"        .   "#3c3836")
                           ("gruvbox-dark2"        .   "#504945")
                           ("gruvbox-dark3"        .   "#665c54")
                           ("gruvbox-dark4"        .   "#7c6f64")

                           ("gruvbox-gray"         .   "#928374")

                           ("gruvbox-light0_hard"  .   "#ffffc8")
                           ("gruvbox-light0"       .   "#fdf4c1")
                           ("gruvbox-light0_soft"  .   "#f4e8ba")
                           ("gruvbox-light1"       .   "#ebdbb2")
                           ("gruvbox-light2"       .   "#d5c4a1")
                           ("gruvbox-light3"       .   "#bdae93")
                           ("gruvbox-light4"       .   "#a89984")
                           ("gruvbox-neutral_red"   .  "#fb4934")
                           ("gruvbox-neutral_green" .  "#b8bb26")
                           ("gruvbox-neutral_yellow".  "#fabd2f")
                           ("gruvbox-neutral_blue"  .  "#83a598")
                           ("gruvbox-neutral_purple".  "#d3869b")
                           ("gruvbox-neutral_aqua"  .  "#8ec07c")
                           ("gruvbox-neutral_orange".  "#fe8019")
                           ("gruvbox-bright_red"     .  "#fb4933" )
                           ("gruvbox-bright_green"   .  "#b8bb26" )
                           ("gruvbox-bright_yellow"  .  "#fabd2f" )
                           ("gruvbox-bright_blue"    .  "#83a598" )
                           ("gruvbox-bright_purple"  .  "#d3869b" )
                           ("gruvbox-bright_aqua"    .  "#8ec07c" )
                           ("gruvbox-bright_orange"  .  "#fe8019" )

                           ("gruvbox-faded_red"      . "#9d0006" )
                           ("gruvbox-faded_green"    . "#79740e" )
                           ("gruvbox-faded_yellow"   . "#b57614" )
                           ("gruvbox-faded_blue"     . "#076678" )
                           ("gruvbox-faded_purple"   . "#8f3f71" )
                           ("gruvbox-faded_aqua"     . "#427b58" )
                           ("gruvbox-faded_orange"   . "#af3a03" )
                           )))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   my-colors-alist))
     ,@body)))

;; ======= straight.el (package-manager) ======
(eval-and-compile
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
  (straight-use-package 'use-package))
; 以下が効けば:straight tは不要のはずだが上手くいかない時がある
; (setq straight-use-package-by-defaul t)

(use-package use-package-hydra :straight t)

;; ======== scroll =========
;; (use-package smooth-scroll
;;   :straight t
;;   :config
;;   (smooth-scroll-mode t)
;;   ;; 縦方向のスクロール行数を変更する。
;;   (setq smooth-scroll/vscroll-step-size 10)
;;   ;; 横方向のスクロール行数を変更する。
;;   (setq smooth-scroll/hscroll-step-size 10))

;; (use-package smooth-scrolling
;;   :straight t
;;   :config
;;   (smooth-scrolling-mode 1)
;;   )

;; (setq scroll-preserve-screen-position t)
(setq scroll-preserve-screen-position 'always)
;; https://with-emacs.com/posts/ui-hacks/keep-scrollin-scrollin-scrollin/
(autoload 'View-scroll-half-page-forward "view")
(autoload 'View-scroll-half-page-backward "view")
(global-set-key (kbd "C-v") 'View-scroll-half-page-forward)
(global-set-key (kbd "M-v") 'View-scroll-half-page-backward)

;; (setq  mouse-wheel-scroll-amount '(1)
;;       mouse-wheel-progressive-speed nil
;;       scroll-conservatively 101)

;; ======== Dired =========
(use-package dired-x
  :config
  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map (kbd "^")
                (lambda () (interactive) (find-alternate-file "..")))))
  (progn
    (put 'dired-find-alternate-file 'disabled nil)
    (setq dired-omit-verbose nil)
    ;; toggle `dired-omit-mode' with C-x M-o
    (add-hook 'dired-mode-hook #'dired-omit-mode)
    (setq dired-omit-files
          (concat dired-omit-files "\\|^.DS_STORE$\\|^.projectile$|^~*"))))

;; ========= LSP ==========
(setq read-process-output-max (* 1024 1024))
(setq gc-cons-threshold 100000000)
(setq lsp-prefer-capf t)

;; https://glassonion.hatenablog.com/entry/2020/02/17/144008
(use-package company
  :straight t
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  (completion-ignore-case t)
  (company-dabbrev-downcase nil)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotation t)
  (company-frontends '(company-pseudo-tooltip-frontend)
                     (company-echo-metadata-frontend))
  :config
  (setq company-backends '(company-capf company-dabbrev))
  (global-company-mode)
  (setq company-dabbrev-char-regexp "\\(\\sw\\|\\s_\\|_\\|-\\)")
  (setq company-backends '((company-capf company-dabbrev)
                           ;;company-bbdb
                           ;;company-eclim
                           company-semantic
                           ;;company-clang
                           ;;company-xcode
                           ;;company-cmake
                           company-files
                           (company-dabbrev-code company-gtags
                                                 company-etags company-keywords))))
  ;; :bind (:map company-active-map
  ;;       ("C-n" . company-select-next)
  ;;       ("C-p" . company-select-previous)))

(use-package hover
  :straight t
  :after dart-mode
  :config
  (setq hover-hot-reload-on-save t))

(use-package lsp-ui
  :straight t
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-peek-list-width 60
        lsp-ui-peek-fontify 'always))

(use-package company-lsp
  :straight t
  :commands company-lsp)


;; TODO remove this workaround func once elogt's issues are resolved
;; https://github.com/hlissner/doom-emacs/issues/3269
;; https://github.com/joaotavora/eglot/issues/491
(defun project-root (project)
    (car (project-roots project)))

(use-package eglot
  :straight t
  :bind (:map eglot-mode-map
              ("C-c C-d" . eglot-help-at-point)
              ("C-c C-r" . eglot-code-actions))
  :config
  ;; https://github.com/joaotavora/eglot/issues/220
  ;; https://github.com/golang/go/issues/32572
  (setq eglot-xref-lessp-function
        (lambda (a b)
          (< (xref-location-line (xref-item-location a))
             (xref-location-line (xref-item-location b)))))
  (add-to-list 'eglot-server-programs '(dart-mode . ("danal")))
  (setq-default eglot-workspace-configuration
                '((:gopls . (:usePlaceholders t))))
  (setq eglot-sync-connect 1)
  (setq eglot-ignored-server-capabilites '(:documentHighlightProvider))
  (add-to-list 'eglot-server-programs '(go-mode . ("gopls")))

  (add-to-list 'display-buffer-alist
               '("\\*sqls\\*"
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.3)))

  (defclass eglot-sqls (eglot-lsp-server) () :documentation "SQL's Language Server")
  (add-to-list 'eglot-server-programs '(sql-mode . (eglot-sqls "sqls")))
  (cl-defmethod eglot-execute-command
    ((server eglot-sqls) (command (eql executeQuery)) arguments)
    "For executeQuery."
    ;; (ignore-errors
    (let* ((beg (eglot--pos-to-lsp-position (if (use-region-p) (region-beginning) (point-min))))
           (end (eglot--pos-to-lsp-position (if (use-region-p) (region-end) (point-max))))
           (res (jsonrpc-request server :workspace/executeCommand
                                 `(:command ,(format "%s" command) :arguments ,arguments
                                            :timeout 0.5 :range (:start ,beg :end ,end))))
           (buffer (generate-new-buffer "*sqls*")))
      (with-current-buffer buffer
        (eglot--apply-text-edits `[
                                   (:range
                                    (:start
                                     (:line 0 :character 0)
                                     :end
                                     (:line 0 :character 0))
                                    :newText ,res)
                                   ]
                                 )
        (org-mode))
      (pop-to-buffer buffer))
    )
  (cl-defmethod eglot-execute-command
    ((server eglot-sqls) (_cmd (eql switchDatabase)) arguments)
    "For switchDatabase."
    (let* ((res (jsonrpc-request server :workspace/executeCommand
                                 `(:command "showDatabases" :arguments ,arguments :timeout 0.5)))
           (menu-items (split-string res "\n"))
           (menu `("Eglot code actions:" ("dummy" ,@menu-items)))
           (db (if (listp last-nonmenu-event)
                   (x-popup-menu last-nonmenu-event menu)
                 (completing-read "[eglot] Pick an database: "
                                  menu-items nil t
                                  nil nil (car menu-items))
                 ))
           )
      (jsonrpc-request server :workspace/executeCommand
                       `(:command "switchDatabase" :arguments [,db] :timeout 0.5))
      ))

  :hook
  ((c-mode . eglot-ensure)
   ;; (dart-mode . eglot-ensure)
   (go-mode . eglot-ensure)
   (sql-mode . eglot-ensure)
   (kotlin-mode . eglot-ensure)))


;; ========= Golang =========
;; (let ((envs '("GOROOT" "GOPATH")))
;;   (exec-path-from-shell-copy-envs envs))
(use-package go-mode
  :straight t
  :commands go-mode
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))
;; (setenv "GOPATH"
;;         (concat))


;; ========= Ivy ===========
(use-package ivy
  :straight t
  :init
  (setq ivy-initial-input-alist nil)
  :custom
  (ivy-use-virtual-buffer t)
  :config
  ;; mac-change-language-to-usを使うには日本語対応用のpatchを当ててビルドする必要がある
  ;; https://qiita.com/takaxp/items/e07bb286d80fa9dd8e05
  (add-hook 'minibuffer-setup-hook #'mac-change-language-to-us)
  (add-to-list
   'ivy-completing-read-handlers-alist
   '(org-capture-refile . completing-read-default))
  (ivy-mode)
  (use-package ivy-hydra
    :straight t
    :defer t)
  (use-package flx
    :straight t))

;; (use-package swiper
;;   :straight t
;;   :after ivy
;;   :config ;; https://taipapamotohus.com/post/swiper_migemo/
;;   (defun isearch-forward-or-swiper (use-swiper)
;;     (interactive "p")
;;     ;; (interactive "P") ;; 大文字のPだと，C-u C-sでないと効かない
;;     (let (current-prefix-arg)
;;       (call-interactively (if use-swiper 'swiper 'isearch-forward))))
;;   (global-set-key (kbd "C-s") 'isearch-forward-or-swiper))

;; https://taipapamotohus.com/post/swiper_migemo/
(use-package migemo
  :straight t
  :config
  ;; C/Migemo を使う場合は次のような設定を .emacs に加えます．
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs" "-i" "\a"))
  (setq migemo-dictionary "/usr/local/Cellar/cmigemo/20110227/share/migemo/utf-8/migemo-dict")  ;; 各自の辞書の在り処を指示
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  ;; charset encoding
  (setq migemo-coding-system 'utf-8-unix))

;; 上の設定だけでは使えなかった
;; https://www.yewton.net/2020/05/21/migemo-ivy/
(defun ytn-ivy-migemo-re-builder (str)
  (let* ((sep " \\|\\^\\|\\/\\|\\.\\|\\*") ;検索にスラッシュが入るとエラーになるのでここにスラッシュを追加した
         (splitted (--map (s-join "" it)
                          (--partition-by (s-matches-p " \\|\\^\\|\\.\\|\\*" it)
                                          (s-split "" str t)))))
    (s-join "" (--map (cond ((s-equals? it " ") ".*?")
                            ((s-matches? sep it) it)
                            (t (migemo-get-pattern it)))
                      splitted))))
;(setq ivy-re-builders-alist '((t . ivy--regex-plus)
;                              (swiper . ytn-ivy-migemo-re-builder)))
;; さらにrg(ripgrep)でもmigemoを使える
;; https://www.yewton.net/2020/05/25/counsel-rg-migemo/
(setq migemo-options '("--quiet" "--nonewline" "--emacs"))
(setq ivy-re-builders-alist '((t . ivy--regex-plus)
                              (counsel-rg . ytn-ivy-migemo-re-builder)
                              (swiper . ytn-ivy-migemo-re-builder)))
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "M-s") 'counsel-rg)))

;; (use-package find-file-in-project
;;   :straight t
;;   :config
;;   (global-set-key (kbd "C-M-f") 'find-file-in-project))

(use-package counsel
  :straight t
  :bind
  ("C-x C-r" . counsel-recentf)
  ("C-M-s" . counsel-rg)
  ("C-x b" . counsel-switch-buffer)
  :config
  (counsel-mode)
  (setq ivy-initial-inputs-alist '((org-agenda-refile . "^"))))

(use-package all-the-icons-ivy-rich
  :straight t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :straight t
  :init (ivy-rich-mode 1))

(use-package prescient
  :straight t
  :config
  ;; ivy インターフェイスでコマンドを実行するたびに，キャッシュをファイル保存
  (setq prescient-aggressive-file-save t)
    ;; ファイルの保存先
  (setq prescient-save-file
        (expand-file-name "~/.emacs.d/prescient-save.el"))
    ;; アクティベート
  (prescient-persist-mode 1))

(use-package ivy-xref
  :straight t
  :ensure t
  :init
  ;; xref initialization is different in Emacs 27 - there are two different
  ;; variables which can be set rather than just one
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
  ;; commands other than xref-find-definitions (e.g. project-find-regexp)
  ;; as well
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package ivy-prescient
  :straight t
  :hook (ivy-mode . ivy-prescient-mode)
  :hook (swiper . ivy-prescient-mode)
  :hook (ivy-prescient-mode . prescient-persist-mode)
  :after (prescient counsel)
  :config
  (setq ivy-prescient-sort-commands
        '(:not swiper swiper-isearch ivy-switch-buffer counsel-grep
               counsel-git-grep counsel-ag counsel-rg counsel-imenu
               counsel-recentf counsel-yank-pop)
        ivy-prescient-retain-classic-highlighting t)
  )

;; 入れるだけでcounsel-M-xがソートされる
(use-package amx
  :straight t)
(use-package company-prescient
  :straight t
  :after ivy-prescient
  :config

  ;; 以下設定するとswiperの並び順がおかしくなるのでコメントアウト
  ;; コマンドを追加
  ;; (dolist (command '(counsel-world-clock ;; Merged!
  ;;                    counsel-app)) ;; add :caller
  ;;   (add-to-list 'ivy-prescient-sort-commands command))

    ;; フィルタの影響範囲を限定する．以下の3つは順番が重要．
  ;; (1) マイナーモードの有効化
  (ivy-prescient-mode 1)
  ;; (2) =counsel-M-x= をイニシャル入力対応にする
  (setf (alist-get 'counsel-M-x ivy-re-builders-alist)
        #'ivy-prescient-re-builder)
  ;; (3) デフォルトのイニシャル入力を上書きする
  (setf (alist-get t ivy-re-builders-alist) #'ivy--regex-ignore-order))
(use-package selectrum-prescient :straight t)

;; (use-package ivy-posframe
;;   :straight t
;;   :after ivy
;;   :config
;;   (use-package posframe
;;     :straight t)
;;   (setq ivy-posframe-display-functions-alist
;;       '((swiper          . nil)
;;         (swiper-avy      . nil)
;;         (swiper-isearch  . nil)
;;         (complete-symbol . ivy-posframe-display-at-point)
;;         (counsel-M-x     . ivy-posframe-display-at-point)
;;         (flyspell-correct-ivy . ivy-posframe-display-at-point)
;;         (counsel-recentf . ivy-posframe-display-at-frame-center)
;;         (t               . ivy-posframe-display)
;;         ))
;;   (ivy-posframe-enable))

;; ======== Flycheck =======
(use-package flycheck
  :straight t
  :commands flycheck-mode
  :init
  (global-flycheck-mode)
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++14"))))


;; ======= recentf ======
(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :bind ("C-x C-r" . counsel-recentf)
  :config
  (setq recentf-filename-handlers
        '(substring-no-properties    ; strip out lingering text properties
          abbreviate-file-name))      ; replace $HOME with ~
  :custom
  (recentf-save-file "~/.emacs.d/.recentf")
  (recentf-auto-cleanup 'never)  ;; 存在しないファイルは消さない
  (recentf-max-saved-items 20000000))
;; recentf拡張
(use-package recentf-ext
  :straight t)

;; ========= themes ========
;; (use-package doom-themes
;;   :straight t
;;   :custom
;;   (doom-themes-enable-italic t)
;;   (doom-themes-enable-bold t)
;;   :custom-face
;;   (doom-modeline-bar ((t (:background "#6272a4"))))
;;   :config
;;   (setq doom-one-brighter-modeline t)
;;   (load-theme 'doom-vibrant t)
;;   (doom-themes-neotree-config)
;;   (doom-themes-org-config))

(use-package doom-modeline
  :straight t
  :defer t
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-icon t)

  (doom-modeline-major-mode-icon nil)
  :hook
  (after-init . doom-modeline-mode)
  :config
  (line-number-mode 0)
  (column-number-mode 0)
  (setq doom-modeline-minor-modes t)
  (minions-mode))
  ;; (doom-modeline-def-modeline 'main
  ;;             '(bar window-number matches buffer-info remote-host buffer-position selection-info)
  ;;             '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker)))

(use-package minions
  :straight t
  :custom
  (minions-mode-line-lighter "⚙")
  (minions-mode-line-delimiters nil)
  (minions-direct '(vlf-mode flycheck-mode projectile-mode))
  :config
  (minions-mode))

(use-package all-the-icons
  :straight t
  :custom
  (all-the-icons-scale-factor 1.0)
  :config
  ;; all-the-icons doesn't work without font-lock+
  ;; And font-lock+ doesn't have autoloads
  (use-package font-lock+
    :straight (:host github :repo "emacsmirror/font-lock-plus")
    :config (require 'font-lock+)))

(use-package all-the-icons-dired
  :straight t
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

;; ======= Git ============
(use-package magit
  :straight t
  :commands (magit-status)
  :config
  (setq magit-display-buffer-function
        (lambda (buffer)
          (display-buffer
           buffer (if (and (derived-mode-p 'magit-mode)
                           (memq (with-current-buffer buffer major-mode)
                                 '(magit-process-mode
                                   magit-revision-mode
                                   magit-diff-mode
                                   magit-stash-mode
                                   magit-status-mode)))
                      nil
                    '(display-buffer-same-window)))))
  :bind (("C-x g" . magit-status)))

(use-package diff-hl
  :straight t
  :hook ((dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :custom (diff-hl-flydiff-delay 0.5)
  :config (global-diff-hl-mode t))

(use-package git-gutter
  :straight t
  :defer t
  :custom
  (git-gutter:modified-sign "~")
  (git-gutter:added-sign    "+")
  (git-gutter:deleted-sign  "-")
  :custom-face
  (git-gutter:modified ((t (:background "#f1fa8c"))))
  (git-gutter:added    ((t (:background "#50fa7b"))))
  (git-gutter:staged    ((t (:background "#50fa7b"))))
  (git-gutter:deleted  ((t (:background "#ff79c6"))))
  :hydra
  (hydra-git-gutter nil
                    "git hunk"
                    ("p" git-gutter:previous-hunk "previous")
                    ("n" git-gutter:next-hunk "next")
                    ("s" git-gutter:stage-hunk "stage")
                    ("r" git-gutter:revert-hunk "revert")
                    ("SPC" git-gutter:toggle-popup-hunk "toggle diffinfo"))
  :bind
  ("C-c g" . hydra-git-gutter/body)
  :config
  (global-git-gutter-mode +1))

;; https://nukosuke.hatenablog.jp/entry/git-gutter-hydra
(use-package hydra :straight t)
(defun git-gutter:toggle-popup-hunk ()
  "Toggle git-gutter hunk window."
  (interactive)
  (if (window-live-p (git-gutter:popup-buffer-window))
      (delete-window (git-gutter:popup-buffer-window))
    (git-gutter:popup-hunk)))

;; ======== Yasnippet =======
(use-package yasnippet
  :straight t
  :config
  (yas-global-mode t)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "C-'") #'yas-expand)
  (add-to-list #'yas-snippet-dirs "/Users/wakamenod/.doom.d/snippets")
  (yas-reload-all)
  (setq yas-prompt-functions '(yas-ido-prompt))
  (yas-global-mode)
  :diminish yas-minor-mode)

; YASnippet のスニペットを候補に表示するための設定
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")
(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
(defun set-yas-as-company-backend ()
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))
(add-hook 'company-mode-hook 'set-yas-as-company-backend)

;; ======== Shell ===========
(setq eshell-save-history-on-exit t
      eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")

;; (use-package popwin
;;   :straight t
;;   :config
;;   (popwin-mode t)
;;   ;; (setq display-buffer-alist 'popwin:display-buffer)
;;   (push '("*Completions*" :noselect t :height 0.4) popwin:special-display-config)
;;   ;; (push '("*xref*" :noselect t :height 0.4) popwin:special-display-config)
;;   ;; (push '("vterm" :noselect t :height 0.4) popwin:special-display-config)
;;   (push '("*compilation*" :noselect t :height 0.4) popwin:special-display-config))

;; (use-package shell-pop
;;   :straight t
;;   :defer t
;;   ; :bind ("M-t" . shell-pop)
;;   :custom
;;   (shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda () (eshell)))))
;;   ; (shell-pop-universal-key "M-t")
;;   (shell-pop-window-size 50)
;;   (shell-pop-full-span t)
;;   (shell-pop-window-position "bottom")
;;   :commands (shell-pop)
;;   :config
;;   (setq eshell-history-size 1000000)
;;   (setq eshell-read-history "~/.bash_history")
;;   (add-hook 'eshell-exit-hook 'shell-pop--kill-and-delete-window t t)
;;   (add-hook 'eshell-mode-hook
;;             (lambda ()
;;               (define-key eshell-mode-map (kbd "C-s") 'counsel-esh-history)))
;;   (setq eshell-command-aliases-list
;;         (append
;;          (list
;;           (list "ll" "ls -alF")
;;           (list "d" "dired .")
;;     (list "ojd" "~/Projects/ShellScriptProjects/misc/oj_all.sh -u $1")
;;     (list "ojt" "~/Projects/ShellScriptProjects/misc/oj_all.sh")))))

(define-advice vterm--set-title (:filter-args (&rest args) qqqqq)
  (let ((title (car (car args))))
    (setq title (last (split-string title "[ \\/]")))
    title
    ))

(defun my/vterm-open()
  (interactive)
  (vterm)
  (delete-window)
  (switch-to-buffer nil))

(use-package vterm
  :straight t
  :bind(:map vterm-mode-map
             ("C-y" . vterm-yank)
             ("M-t" . my/vterm-open))
  :custom
  (vterm-buffer-name-string "vterm %s")
  (vterm-max-scrollback 100000)
  (vterm-kill-buffer-on-exit t))

(use-package vterm-toggle
  :straight t
  :config
  (global-set-key (kbd "M-t") 'vterm-toggle)
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 ;;(display-buffer-reuse-window display-buffer-in-direction)
                 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                 ;;(direction . bottom)
                 ;;(dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3)))

  (setq vterm-toggle-cd-auto-create-buffer nil)

  ;; (setq centaur-tabs-buffer-groups-function 'vmacs-awesome-tab-buffer-groups)
  ;; (defun vmacs-awesome-tab-buffer-groups ()
  ;;   "`vmacs-awesome-tab-buffer-groups' control buffers' group rules. "
  ;;   (list
  ;;    (cond
  ;;     ((string-match-p (rx (or
  ;;                           "\*jdtls::stderr\*"
  ;;                           "\*jdtls\*"
  ;;                           ))
  ;;                      (buffer-name))
  ;;      "Others")
  ;;     ((and (derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode)
  ;;          (not (string-match-p (rx (or "\*jdtls::stderr\*")) (buffer-name))))
  ;;      "Term")
  ;;     ((string-match-p (rx (or
  ;;                           "\*Helm"
  ;;                           "\*helm"
  ;;                           "\*tramp"
  ;;                           "\*Completions\*"
  ;;                           "\*sdcv\*"
  ;;                           "\*Messages\*"
  ;;                           "\*Ido Completions\*"
  ;;                           ))
  ;;                      (buffer-name))
  ;;      "Emacs")
  ;;     (t "Common"))))

  (setq vterm-toggle--vterm-buffer-p-function 'vmacs-term-mode-p)
  (defun vmacs-term-mode-p(&optional args)
    (derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode))
  )

;; (define-advice centaur-tabs-projectile-buffer-groups (:override () my/centaur-tabs-projectile-buffer-groups)
;;   "Return the list of group names BUFFER belongs to."
;;   (if centaur-tabs-projectile-buffer-group-calc
;;       (symbol-value 'centaur-tabs-projectile-buffer-group-calc)
;;     (set (make-local-variable 'centaur-tabs-projectile-buffer-group-calc)

;; 	 (cond
;;           ((string-equal "*" (substring (buffer-name) 0 1)) '("Misc"))
;; 	  ((or (get-buffer-process (current-buffer)) (memq major-mode '(comint-mode compilation-mode))) '("Term"))
;; 	  ((condition-case _err
;; 	       (projectile-project-root)
;; 	     (error nil)) (list (projectile-project-name)))
;; 	  ((memq major-mode '(emacs-lisp-mode python-mode emacs-lisp-mode c-mode
;; 					      c++-mode javascript-mode js-mode
;; 					      js2-mode makefile-mode
;; 					      lua-mode vala-mode)) '("Coding"))
;; 	  ((memq major-mode '(nxhtml-mode html-mode
;; 					  mhtml-mode css-mode)) '("HTML"))
;; 	  ((memq major-mode '(org-mode calendar-mode diary-mode)) '("Org"))
;; 	  ((memq major-mode '(dired-mode)) '("Dir"))
;; 	  (t '("Other"))))
;;     (symbol-value 'centaur-tabs-projectile-buffer-group-calc)))


;; (use-package centaur-tabs
;;   :straight t
;;   :demand
;;   :hook
;;   (dired-mode . centaur-tabs-local-mode)
;;   :config
;;   (setq centaur-tabs-style "bar"
;;         centaur-tabs-set-icons t
;;         centaur-tabs-gray-out-icons 'buffer
;;         centaur-tabs-bar-height 24
;;         centaur-tabs-set-bar 'over
;;         centaur-tabs-set-modified-marker t
;;         centaur-tabs-modified-marker "◎"
;;         centaur-tabs-cycle-scope 'tabs
;;         )
;;   (centaur-tabs-mode t)
;;   (centaur-tabs-group-by-projectile-project)
;;   :bind
;;   ("C-A-h" . centaur-tabs-backward)
;;   ("A-C-l" . centaur-tabs-forward))

;; (use-package linum-off
;;   :straight t
;;   :config
;;   (setq linum-disabled-modes-list '(vterm-mode)))


;; ===== Typescript ========
(use-package typescript-mode
  :straight t
  :mode (("\\.ts\\'" . typescript-mode)))
;; (use-package tide
;;   :straight t
;;   :commands (tide-mode)
;;   :hook
;;   (typescript-mode . tide-mode)
;;   :config
;;   (tide-setup)
;;   (flycheck-mode t)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (eldoc-mode t)
;;   (company-mode-on))

;; ======= PDF =============
(use-package pdf-tools
  :straight t
  :config
  ;; initialise
  ;; (pdf-tools-install)
  ;; PDF Tools does not work well together with linum-mode
  ;; (add-hook 'pdf-view-mode-hook (lambda() (nlinum-mode -1)))
  ;; open pdfs scaled to fit page
  ;; (setq-default pdf-view-display-size 'fit-page)
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t)
  ;; use normal isearch
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  ;; more fine-grained zooming
  (setq pdf-view-resize-factor 1.1))

;; ====== Dart ============
;; Assuming usage with dart-mode
(use-package dart-mode
  :straight t
  :custom
  (dart-format-on-save 1)
  (dart-sdk-path "~/.local/share/flutter/bin/cache/dart-sdk/")
  ;; Optional
  :hook (dart-mode . flutter-test-mode))

(use-package flutter
  :straight t
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload))
  ;; :hook
  ;; (dart-mode . (lambda ()
  ;; 		      (add-hook 'after-save-hook #'flutter-run-or-hot-reload nil t)))
  :custom
  (flutter-sdk-path "~/.local/share/flutter"))

;; Optional
(use-package flutter-l10n-flycheck
  :straight t
  :after flutter
  :config
  (flutter-l10n-flycheck-setup))

(use-package groovy-mode
  :straight t
  :init
  (add-to-list 'auto-mode-alist (cons "\\.gradle\\'" 'groovy-mode)))

(defun my/counsel-rg-with-extention (extention)
  "Execute counsel-rg with on cursor files EXTENTION."
  (let ((match-extention extention))
    (string-match "^[A-Za-z0-9_]+\.\\([A-Za-z0-9_\.]+\\):" match-extention)
    (counsel-rg (concat "-g'*." (match-string 1 match-extention) "' -- "))))
(ivy-set-actions
 'counsel-rg
 '(("e" my/counsel-rg-with-extention "with-extention")
   ))


;; ======== Others ========
;; (define-advice counsel--format-ag-command (:filter-return (&rest args) aaaaaaabbbb)
;;   (let ((ret (car args)))
;;     (princ "\n")
;;     (princ ret)
;;     ret
;;     )
;;   )
(defun my/rg-current-file()
  "Grep for a INPUT in the current file using rg."
  (interactive)
  ;; -M specifies max colums for printing results. Since dealing with log files, set this aggressively.
  ;; -- is to denote that anything following it should be interpreted as a positional argument
  ;; https://github.com/BurntSushi/ripgrep/issues/1077
  ;; Can check the resulting command string in above advice
  (counsel-rg "" nil (concat "-M 1000 -- " (buffer-file-name) ) nil)
  )

(use-package bm :straight t)
(use-package vlf
  :straight t
  :init
  (require 'vlf-setup)
  :bind
  ("A-." . hydra-vlf-menu/body)
  :hydra
  (hydra-vlf-menu (:color pink :hint nil)
                  "
^Main^              ^Jump^        ^Misc^        ^Search^         ^Bookmark
^^^^^^^^----------------------------------------------------------------
_n_: next           _[_: begin    _f_: follow   _s_: forward     _t_: toggle
_p_: prev           _]_: end      ^ ^           _r_: backward    _N_: next
_SPC_: next-point   _j_: batch    ^ ^           _o_: occur       _P_: prev
^ ^                 _l_: line     ^ ^           _g_: rg          _L_: list
"
                  ("n"   vlf-next-batch)
                  ("p"   vlf-prev-batch)
                  ("SPC" vlf-next-batch-from-point)
                  ("["   vlf-beginning-of-file)
                  ("]"   vlf-end-of-file)
                  ("j"   vlf-jump-to-chunk)
                  ("l"   vlf-goto-line)
                  ("s"   vlf-re-search-forward)
                  ("r"   vlf-re-search-backward)
                  ("o"   vlf-occur)
                  ("f"   vlf-toggle-follow)
                  ("g"   my/rg-current-file)
                  ("t"   bm-toggle)
                  ("N"   bm-next)
                  ("P"   bm-previous)
                  ("L"   bm-show)
                  ("q" nil "quit" :color blue))
  )

(use-package wgrep
  :straight t
  )


(use-package expand-region
  :straight t
  :defer t
  ;; :bind
  ;; (("C-=" . er/expand-region))
  :config
  (define-key region-bindings-mode-map (kbd "+") 'er/expand-regione)
  (define-key region-bindings-mode-map (kbd "-") 'er/contract-regione)
  :commands
  (er/expand-region er/contract-region))

;; https://tottoto.net/emacs-first-settings/
(use-package exec-path-from-shell
  :straight t
  :config
  (exec-path-from-shell-initialize))

(use-package which-key
  :straight t
  :init (which-key-mode))

(use-package smartparens
  :straight t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode))

(use-package smart-jump
 :straight t
 :config
 (smart-jump-setup-default-registers))

(use-package docker
  :straight t
  :defer t
  :bind ("C-c d" . docker))

(use-package dockerfile-mode
  :straight t
  :mode (("Dockerfile\\'" . dockerfile-mode)))

;; (use-package whole-line-or-region
;;   :straight t
;;   :config
;;   (whole-line-or-region-global-mode t))

(use-package easy-kill
  :straight t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))

(use-package undo-fu
  :straight t
  :config
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-M-z") 'undo-fu-only-redo))

(use-package multiple-cursors
  :straight t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package lsp-mode
  :straight t
  :hook (php-mode . lsp)
  :custom
  (lsp-enable-file-watchers nil)
  :config
   (setq lsp-ui-doc-enable nil)
   :init (add-to-list 'company-backends 'company-capf)
   :commands lsp
  )
(use-package lsp-dart
  :straight t
  :hook (dart-mode . lsp))

 (use-package kotlin-mode
   :straight t)
;; (use-package lsp-kotlin
;;   :straight (:host github :repo "whily/lsp-kotlin"))

(use-package yaml-mode
  :straight t
  :defer t
  :init
  (add-to-list 'auto-mode-alist (cons "\\.y[a]?ml\\'" 'yaml-mode)))



;; ========= Key maps ===========
(global-set-key (kbd "C-A-h") 'windmove-left)
(global-set-key (kbd "C-A-j") 'windmove-down)
(global-set-key (kbd "C-A-k") 'windmove-up)
(global-set-key (kbd "C-A-l") 'windmove-right)
(defun my/split-windmove-right ()
  "Vsplit window and move right."
  (interactive)
  (split-window-right)

  (windmove-right))
(global-set-key (kbd "C-x 3") 'my/split-windmove-right)
;;(global-set-key (kbd "C-M-o") 'delete-other-windows)
;;(global-set-key (kbd "C-M-k") 'delete-window)
;;(global-set-key (kbd "M-k") 'kill-this-buffer)
;; (defun counsel-recentf-other ()
;;   "Find a file on `recentf-list'."
;;   (interactive)
;;   (require 'recentf)
;;   (recentf-mode)
;;   (ivy-read "Recentf other: " (mapcar #'substring-no-properties recentf-list)
;;             :action (lambda (f)
;;                       (with-ivy-window
;;                         (split-window-horizontally))
;;                       (other-window 1)
;;                       (find-file f)
;;                       (balance-windows))
;;             :require-match t
;;             :caller 'counsel-recentf-other))
;;(global-set-key (kbd "C-x M-r") 'counsel-recentf-other)
;; (defun counsel-find-file-other-action (x)
;;   "Find file X."
;;   (with-ivy-window
;;     (split-window-horizontally))
;;   (other-window 1)
;;   (find-file x)
;;   (balance-windows))
;; (defun counsel-find-file-other (&optional initial-input)
;;   "Forward to `find-file'.
;; When INITIAL-INPUT is non-nil, use it in the minibuffer during completion."
;;   (interactive)
;;   (counsel--find-file-1
;;    "Find file other: " initial-input
;;    #'counsel-find-file-other-action
;;    'counsel-find-file-other))
;;(global-set-key (kbd "C-x M-f") 'counsel-find-file-other)
;; my/macro is recorded macro. see how to record: https://kb.iu.edu/d/aaxw
;; what this macros does is M-w w, which is easy-kill's save word at point
;;(global-set-key (kbd "M-d") (fset 'my/macro "\367w"))
;;(global-set-key (kbd "M-n") (fset 'my/kbd-macro-yyp "\C-a\367\C-y"))
;;(global-set-key (kbd "C-M-d") (fset 'my/kbd-macro-viw "\200w"))
;;(global-set-key (kbd "M-c") (fset 'my/kbd-macro-ciw "\367w\C-w"))
;;(global-set-key (kbd "C-M-y") (fset 'my/kbd-macro-newline-yank (kmacro-lambda-form [?\C-o ?\C-y backspace] 0 "%d")))

;;(global-set-key (kbd "C-M-s") 'counsel-rg)
;;(global-set-key (kbd "M-l") 'indent-region)
;;(global-set-key (kbd "C-x =") 'balance-windows)
;; (global-set-key (kbd "C-x +") 'text-scale-increase)
;; (global-set-key (kbd "C-x -") 'text-scale-decrease)
;; (global-set-key (kbd "C-M-g") 'goto-line)
;; (global-set-key (kbd "M-s s") 'swiper-isearch-thing-at-point)
;;(global-set-key (kbd "C-x b") 'counsel-switch-buffer)

(define-key global-map [165] [92]) ;; ¥の代わりにバックスラッシュを入力する

;; ======== window ========
(use-package buffer-move
  :straight t
  :config
  (global-set-key (kbd "A-k")  'buf-move-up)
  (global-set-key (kbd "A-j")  'buf-move-down)
  (global-set-key (kbd "A-h")  'buf-move-left)
  (global-set-key (kbd "A-l")  'buf-move-right))

;; (defun my/ace-window (arg)
;;   "Disable japanese input then ace-window."
;;   (interactive "p")
;;   (mac-change-language-to-us)
;;   (ace-window arg))
;; (use-package ace-window
;;   :straight t
;;   :config
;;   (global-set-key (kbd "M-o") 'my/ace-window)
;;   ;; (setq aw-dispatch-always t)
;;   (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
;;   :custom-face
;;   (aw-leading-char-face ((t (:height 4.0 :foreground "#f1fa8c"))))
;;   )


;; ========= OpenSCAD ==========
(use-package scad-preview
  :straight t
  )
(use-package scad-mode
  :straight t
  :mode (("\\.scad$'" . scad-mode))
  :custom
  (scad-command "/Applications/OpenSCAD.app/Contents/MacOS/OpenSCAD")
  )


;; ======= anzu =========
;; (use-package anzu
;;   :defer t
;;   :straight t
;;   :bind
;;   (("M-r" . anzu-query-replace)
;;    ("C-M-r" . anzu-query-replace-regexp))
;;   :custom
;;   (anzu-mode-lighter "")
;;   (anzu-search-threshold 1000)
;;   (anzu-replace-to-string-separator " => ")
;;   :commands (anzu-mode anzu-query-replace anzu-query-replace-regexp)
;;   :hook
;;   (after-init . global-anzu-mode))


;; ======= on region =========
;; http://blog.fujimisakari.com/elisp_useful_for_programming/
(defun region-to-single-quote ()
  (interactive)
  (quote-formater "'%s'" "^\\(\"\\).*" ".*\\(\"\\)$"))

(defun region-to-double-quote ()
  (interactive)
  (quote-formater "\"%s\"" "^\\('\\).*" ".*\\('\\)$"))

(defun region-to-bracket ()
  (interactive)
  (quote-formater "\(%s\)" "^\\(\\[\\).*" ".*\\(\\]\\)$"))

(defun region-to-square-bracket ()
  (interactive)
  (quote-formater "\[%s\]" "^\\(\(\\).*" ".*\\(\)\\)$"))

(defun quote-formater (quote-format re-prefix re-suffix)
  (if mark-active
      (let* ((region-text (buffer-substring-no-properties (region-beginning) (region-end)))
             (replace-func (lambda (re target-text)(replace-regexp-in-string re "" target-text nil nil 1)))
             (text (funcall replace-func re-suffix (funcall replace-func re-prefix region-text))))
        (delete-region (region-beginning) (region-end))
        (insert (format quote-format text)))
    (error "Not Region selection")))

(use-package region-bindings-mode
  :straight t
  :config
  (region-bindings-mode-enable)
  (define-key region-bindings-mode-map (kbd "M-4") 'region-to-single-quote)
  (define-key region-bindings-mode-map (kbd "M-2") 'region-to-double-quote)
  (define-key region-bindings-mode-map (kbd "M-9") 'region-to-bracket)
  (define-key region-bindings-mode-map (kbd "M-i") 'region-to-square-bracket))

;; ======= Stolen from DOOM ========
;; Create missing directories when we open a file that doesn't exist under a
;; directory tree that may not exist.
(add-hook 'find-file-not-found-functions
  (defun doom-create-missing-directories-h ()
    "Automatically create missing directories when creating new files."
    (unless (file-remote-p buffer-file-name)
      (let ((parent-directory (file-name-directory buffer-file-name)))
        (and (not (file-directory-p parent-directory))
             (y-or-n-p (format "Directory `%s' does not exist! Create it? "
                               parent-directory))
             (progn (make-directory parent-directory 'parents)
                    t))))))


;; ======= open next line ========
;; https://www.emacswiki.org/emacs/OpenNextLine
;; Behave like vi's o command
;; (defun open-next-line (arg)
;;   "Move to the next line and then opens a line.
;;     See also `newline-and-indent'."
;;   (interactive "p")
;;   (end-of-line)
;;   (open-line arg)
;;   (forward-line 1)
;;   (when newline-and-indent
;;     (indent-according-to-mode)))
;; Behave like vi's O command
;; (defun open-previous-line (arg)
;;   "Open a new line before the current one. 
;;      See also `newline-and-indent'."
;;   (interactive "p")
;;   (beginning-of-line)
;;   (open-line arg)
;;   (when newline-and-indent
;;     (indent-according-to-mode)))
;;(global-set-key (kbd "C-o") 'open-next-line)
;; (global-set-key (kbd "C-M-o") 'open-previous-line)
;; Autoindent open-*-lines
;; (defvar newline-and-indent t
;;   "Modify the behavior of the open-*-line functions to cause them to autoindent.")

;; ;; yank時にindentする
;; (defun yank-and-indent ()
;;   "Yank and then indent the newly formed region according to mode."
;;   (interactive)
;;   (yank)
;;   (call-interactively 'indent-region))
;; (global-set-key (kbd "C-y") 'yank-and-indent)
;; (define-key region-bindings-mode-map (kbd "C-y") 'yank)


;; ========= Makefile =======
(setq compilation-scroll-output t)

;; ========= Projectile =======
(use-package projectile
  :straight t
  :config
  (defun my-projectile-project-find-function (dir)
    (let ((root (projectile-project-root dir)))
      (and root (cons 'transient root))))
  (add-to-list 'project-find-functions 'my-projectile-project-find-function)
  (global-set-key (kbd "C-c m") 'projectile-compile-project))

(use-package counsel-projectile
  :straight t
  :after (projectile)
  :config
  (setq projectile-completion-system 'ivy)
  (setq counsel-projectile-sort-files t) ;; 当該プロジェクト内リストをソート
  (setq counsel-projectile-sort-projects t) ;; プロジェクトリストをソート
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (counsel-projectile-mode 1))


;; ======== Html ========
(use-package web-mode
  :straight t
  :config
  (setq web-mode-enable-auto-indentation nil)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))


;; ========= Fonts ==========
;;; CamingoCode + Ricty Diminished
;;;   下記のようなサイズの組み合わせだと全角文字と半角文字のバランスがよい.
;;;   他の組み合わせだとズレるので注意.
;;;     + CamingoCode:size=13, Ricty Diminished:size=14
;;;     + CamingoCode:size=15, Ricty Diminished:size=16
;;;     + CamingoCode:size=17, Ricty Diminished:size=18
;;; http://boiled-mag.hatenablog.jp/entry/2018/05/22/192536
(create-fontset-from-ascii-font "CamingoCode:size=15:weight=normal:slant=normal"
                                nil
                                "CamingoCode_Cica")
(set-fontset-font "fontset-CamingoCode_Cica"
                  'unicode
                  (font-spec :family "Cica" :size 16) nil  'append)
;;; 上記で作成したフォントセットをデフォルトに設定する.
(add-to-list 'default-frame-alist '(font . "fontset-CamingoCode_Cica"))


(create-fontset-from-ascii-font "CamingoCode:size=26:weight=normal:slant=normal"
                                nil
                                "CamingoCode_Cica_L")
(set-fontset-font "fontset-CamingoCode_Cica_L"
                  'unicode
                  (font-spec :family "Cica" :size 27) nil  'append)

(create-fontset-from-ascii-font "CamingoCode:size=21:weight=normal:slant=normal"
                                nil
                                "CamingoCode_Cica_M")
(set-fontset-font "fontset-CamingoCode_Cica_M"
                  'unicode
                  (font-spec :family "Cica" :size 22) nil  'append)

(create-fontset-from-ascii-font "CamingoCode:size=18:weight=normal:slant=normal"
                                nil
                                "CamingoCode_Cica_S")
(set-fontset-font "fontset-CamingoCode_Cica_S"
                  'unicode
                  (font-spec :family "Cica" :size 19) nil  'append)

(create-fontset-from-ascii-font "HackGenNerd:size=18:weight=normal:slant=normal"
                                nil
                                "HackGenNerd")
(set-fontset-font "fontset-HackGenNerd"
                  'unicode
                  (font-spec :family "HackGenNerd" :size 13) nil  'append)

;; ======= Themes ======
;; (load-theme 'tsdh-dark t)
;; (use-package color-theme-sanityinc-tomorrow
;;   :straight t )

;; (load-theme 'sanityinc-tomorrow-bright t)
;; (load-theme 'sanityinc-tomorrow-night t)
;; (load-theme 'sanityinc-tomorrow-eighties t)
;; (load-theme 'sanityinc-tomorrow-blue t)
;; (load-theme 'sanityinc-tomorrow-day t)

;; (use-package immaterial-theme
;;   :straight t
;;   :config
;;    (load-theme 'immaterial-dark t))

 ;; (use-package cyberpunk-2019-theme
 ;;   :straight t
 ;;   :config
 ;;     (load-theme 'cyberpunk-2019 t))

(use-package cyberpunk-theme
  :straight t
  :config
    (load-theme 'cyberpunk t))

 ;; (use-package hc-zenburn-theme
 ;;   :straight t
 ;;   :config
 ;;   (load-theme 'hc-zenburn t))

;; ========= White Space ==========
;; https://qiita.com/itiut@github/items/4d74da2412a29ef59c3a
(setq whitespace-style '(face           ; faceで可視化
                         trailing       ; 行末
                         tabs           ; タブ
                         spaces         ; スペース
                         empty          ; 先頭/末尾の空行
                         space-mark     ; 表示のマッピング
                         tab-mark
                         ))

(setq whitespace-display-mappings
      '((space-mark ?\u3000 [?\u25a1])
        ;; WARNING: the mapping below has a problem.
        ;; When a TAB occupies exactly one column, it will display the
        ;; character ?\xBB at that column followed by a TAB which goes to
        ;; the next TAB column.
        ;; If this is a problem for you, please, comment the line below.
        (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))

;; スペースは全角のみを可視化
(setq whitespace-space-regexp "\\(\u3000+\\)")

;; 保存前に自動でクリーンアップ
;; (setq whitespace-action '(auto-cleanup))

(global-whitespace-mode 1)
(setq whitespace-global-modes '(not dired-mode magit-mode magit-log-mode vterm-mode))

(defvar my/bg-color "#232323")
(set-face-attribute 'whitespace-trailing nil
                    :background my/bg-color
                    :foreground "DeepPink"
                    :underline t)
(set-face-attribute 'whitespace-tab nil
                    :background my/bg-color
                    :foreground "LightSkyBlue"
                    :underline t)
(set-face-attribute 'whitespace-space nil
                    :background my/bg-color
                    :foreground "GreenYellow"
                    :weight 'bold)
(set-face-attribute 'whitespace-empty nil
                    :background my/bg-color)

(defun setup-org-prettify-symbols ()
  "Setpu org prettify symbols."
  (push '(":music:" . "♬" ) prettify-symbols-alist)
  (push '(":shopping:" . "🎁" ) prettify-symbols-alist)
  (prettify-symbols-mode)
  )

;; ========= Org ===========
(use-package org
  :hook
  (org-mode . (lambda ()
                (setq buffer-face-mode-face '(:foreground "#fdf4c1"))
                (buffer-face-mode)))
  (org-mode . setup-org-prettify-symbols)
  :config
  (setq org-agenda-files '("~/Dropbox/Org"))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-startup-with-inline-images t)

  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)
  (setq org-todo-keywords
        '((sequence "TODO" "|" "DONE(!)" "SLEEP(!)")))
  (setq org-log-done 'time)

  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  (add-to-list 'org-file-apps '(directory . emacs))
  (setq org-hide-emphasis-markers t
        org-startup-indented t
        org-pretty-entities t
        org-list-indent-offset 4
        org-use-speed-commands t
        org-return-follows-link t
        org-link-frame-setup '((file . find-file))
        org-use-sub-superscripts '{}
        org-agenda-tags-column -1
        org-lowest-priority ?D
        org-src-tab-acts-natively t)

  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-completion-use-ido nil)

  (let* ((variable-tuple  '(:fontset "CamingoCode_Cica_L"))
         (base-font-color (face-foreground 'default nil 'default))
         (headline        `(:inherit default :weight bold :foreground ,base-font-color))
         )

    (with-my-color-variables
      (custom-theme-set-faces
       'user
       `(org-table ((t (:foreground ,gruvbox-light0))))
       `(org-tag ((t (:foreground ,gruvbox-bright_orange :family "HackGenNerd"))))
       `(org-link ((t (:foreground ,gruvbox-faded_aqua))))
       `(org-date ((t (:foreground ,gruvbox-bright_blue))))
       `(org-verbatim ((t (:foreground ,gruvbox-bright_red :background ,gruvbox-dark0))))
       `(org-level-2 ((t (,@headline ,@variable-tuple :foreground ,gruvbox-light0))))
       `(org-level-1 ((t (,@headline ,@variable-tuple :foreground ,gruvbox-bright_blue))))
       `(org-document-title ((t (,@headline ,@variable-tuple :foreground ,gruvbox-bright_blue )))))))
  (set-face-attribute 'org-table nil :fontset "fontset-HackGenNerd")
  (set-face-attribute 'org-link nil  :fontset "fontset-HackGenNerd")
  (set-face-attribute 'org-date nil  :fontset "fontset-HackGenNerd")
  (set-face-attribute 'org-level-2 nil :fontset "fontset-CamingoCode_Cica_M" :height 1.2)
  (set-face-attribute 'org-level-1 nil :fontset "fontset-CamingoCode_Cica_L" :height 2.0)
  (set-face-attribute 'org-document-title nil :fontset "fontset-CamingoCode_Cica_L" :height 2.0)
  )

;; (use-package emojify
;;   :straight t
;;   :config
;;   (emojify-set-emoji-styles '(github unicode ascii))
;;   (setq emojify-user-emojis '((":shopping:" . (("name" . "Shopping Trolley")
;;                                                 ;; ("image" . "~/.emacs.d/emojis/trollface.png")
;;                                                ("github" . "🛒")
;;                                                 ("style" . "unicode")))))
;;   )

(define-advice org-agenda-fix-displayed-tags (:filter-return (&rest args)  my-fix-displayed-org)
  "Force fontify ageda item. (hack)"
  (let ((txt (car args)))
    (with-temp-buffer
      (org-mode)
      (insert txt)
      (font-lock-fontify-buffer)
      (goto-char (point-min))
      (looking-at "^.*$")
      (setq txt (match-string 0) )
      txt
      )
    )
  )

(use-package el-patch :straight t)

(use-package org-agenda
  :config/el-patch
  (el-patch-feature org-agenda)
  (el-patch-defun org-agenda-align-tags(&optional line)
    "Align all tags in agenda items to `org-agenda-tags-column'.
When optional argument LINE is non-nil, align tags only on the
current line."
    (let ((inhibit-read-only t)
          (org-agenda-tags-column (if (eq 'auto org-agenda-tags-column)
                                      (- (window-text-width))
                                    org-agenda-tags-column))
          (end (and line (line-end-position)))
          l c)
      (save-excursion
        (goto-char (if line (line-beginning-position) (point-min)))
        (while (re-search-forward org-tag-group-re end t)
          (add-text-properties
           (match-beginning 1) (match-end 1)
           (list 'face (delq nil (let ((prop (get-text-property
                                              (match-beginning 1) 'face)))
                                   (or (listp prop) (setq prop (list prop)))
                                   (if (memq 'org-tag prop)
                                       prop
                                     (cons 'org-tag prop))))))
          (setq l (string-width (match-string 1))
                c (if (< org-agenda-tags-column 0)
                      (- (abs org-agenda-tags-column) l)
                    org-agenda-tags-column))
          (goto-char (match-beginning 1))
          (delete-region (save-excursion (skip-chars-backward " \t") (point))
                         (point))
          (el-patch-swap
            (insert (org-add-props
                        (make-string (max 1 (- c (current-column))) ?\s)
                        (plist-put (copy-sequence (text-properties-at (point)))
                                   'face nil)))
            (insert (make-string (max 1 (- c (current-column))) ?\s)))
          )

        (goto-char (point-min))
        (org-font-lock-add-tag-faces (point-max))))
    )
  :config
  (defun my/revert-repeat-task-done ()
    (org-map-entries
     (lambda() (let ((element (org-element-at-point)))
                 (and (member "repeat" (org-element-property :tags element))
                      (eq (org-element-property :todo-type element) 'done)
                      (let* ((closed (org-element-property :closed element))
                             (should-revert-todo
                              (cond ((not closed) t)
                                    (t (not (string= (format-time-string "%Y%m%d" (current-time))
                                                     (format "%d%02d%02d"
                                                             (org-element-property :year-start closed)
                                                             (org-element-property :month-start closed)
                                                             (org-element-property :day-start closed))
                                                     ))))))
                        (when should-revert-todo
                          (org-todo "TODO")
                          )))))
     nil
     (org-agenda-files)
     )
    )
  (add-hook 'org-agenda-mode-hook 'my/revert-repeat-task-done)

  :bind (:map org-agenda-mode-map
              ("w" . org-agenda-refile))
  )

(use-package org-download
  :straight t
  :config
  (add-hook 'dired-mode-hook 'org-download-enable))

(use-package org-bullets
  :straight t
  :custom
  (org-ellipsis "⤵")
  :hook (org-mode . org-bullets-mode))

(use-package org-fancy-priorities
  :straight t
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))

;; (use-package org-pretty-tags
;;   :straight t
;;   :config
;;   (setq org-pretty-tags-surrogate-strings
;;         (quote
;;          (("TOPIC" . "☆")
;;           ("PROJEKT" . "💡")
;;           ("SERVICE" . "✍")
;;           ("Blog" . "✍")
;;           ;; ("music" . "♬")
;;           ("security" . "🔥"))))
;;   (org-pretty-tags-global-mode))

(defun xlfd-at (pos)
  "Return X logical font description (XLFD) of the font at POS in the current buffer."
  (if (not (display-graphic-p))
      (message "Display is not graphic. So font is not used.")
    (font-xlfd-name (font-at pos))))

(defun xlfd-cursor-position (pos)
  "Return X logical font description (XLFD) of the font at the point."
  (interactive "d")
  (message (xlfd-at pos)))

;; From Doom Emacs
(setq org-indirect-buffer-display 'current-window
      org-eldoc-breadcrumb-separator " → "
      org-enforce-todo-dependencies t
      org-hide-emphasis-markers t
      org-entities-user
      '(("flat"  "\\flat" nil "" "" "266D" "♭")
        ("sharp" "\\sharp" nil "" "" "266F" "♯"))
      org-fontify-quote-and-verse-blocks t
      org-fontify-whole-heading-line t
      org-footnote-auto-label 'plain
      org-hide-leading-stars t
      org-image-actual-width nil
      org-priority-faces
      '((?A . error)
        (?B . warning)
        (?C . success))
      org-startup-indented t
      org-tags-column 0
      org-use-sub-superscripts '{})

(setq org-directory "~/org")
(setq org-default-notes-file "notes.org")
(define-key global-map "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("n" "Note" entry (file "~/org/notes.org")
         "* TODO [#B] %?\nEntered on %U")));; %aはデフォルトでorg-store-link

(defun org/note-right-now (content)
  (interactive "sContent: ")
  (org-capture nil "n")
  (insert content)
  (org-capture-finalize))
(global-set-key (kbd "C-M-c") 'org/note-right-now)


; https://qiita.com/takaxp/items/0b717ad1d0488b74429d
(defun show-org-buffer (file)
  "Show an org-file FILE on the current buffer."
  (interactive)
  (if (get-buffer file)
      (let ((buffer (get-buffer file)))
        (switch-to-buffer buffer)
        (message "%s" file))
    (find-file (concat "~/Dropbox/Org/" file))))
;;(global-set-key (kbd "C-M-a") '(lambda () (interactive)
;;                                 (show-org-buffer "notes.org")))
(global-set-key (kbd "C-M-a") 'org-todo-list)

;; ====== Tab ========
;; https://qiita.com/ballforest/items/ed2ffc8cb2c474a82b91
;; (defun my-iflipb-buffer-list ()
;;   "Returns list of buffers whose major-mode is the same as current buffer's one."
;;   (let ((cur-buf-list (buffer-list (selected-frame)))
;;         (same-major-mode-buflist nil)
;;         (currbuf-major-mode
;;          (buffer-local-value 'major-mode (current-buffer))))
;;      (dolist (buffer cur-buf-list)
;;       (if (eq (buffer-local-value 'major-mode buffer) currbuf-major-mode)
;;           (add-to-list 'same-major-mode-buflist buffer)))
;;      (nreverse same-major-mode-buflist)))
;; (use-package iflipb
;;   :straight t
;;   :config
;;   (setq iflipb-wrap-around t)
;;   (setq iflipb-ignore-buffers (list "^[*]"))
;;   (global-set-key (kbd "A-C->") 'iflipb-next-buffer)
;;   (global-set-key (kbd "A-C-<") 'iflipb-previous-buffer)
;;   (setq iflipb-buffer-list-function 'my-iflipb-buffer-list))


;; ====== PHP ========
(use-package php-mode
  :straight t
  :mode
  ("\\.php\\’" . php-mode))

(use-package phpunit
  :straight t)

;; ====== Java ========
(use-package lsp-java
  :straight t
  :config (add-hook 'java-mode-hook 'lsp))

;; ====== Ediff ======
;; コントロール用のバッファを同一フレーム内に表示
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; diffのバッファを上下ではなく左右に並べる
(setq ediff-split-window-function 'split-window-horizontally)

;; ====== package =====
(defun package-menu-find-marks ()
  "Find packages marked for action in *Packages*."
  (interactive)
  (occur "^[A-Z]"))

(define-key package-menu-mode-map "s" #'package-menu-filter-by-keyword)
(define-key package-menu-mode-map "a" #'package-menu-find-marks)

(use-package auto-package-update
  :straight t
  :config
  (setq auto-package-update-delete-old-versions t)
  (add-hook 'auto-package-update-before-hook
            (lambda () (message "I will update packages now")))
  (save-window-excursion
    (auto-package-update-now)))

;; ====== Dashboard ====
;; (use-package dashboard
;;   :defer t
;;   :straight t
;;   :delight
;;   :custom-face
;;   (dashboard-text-banner ((t (:foreground "#feff8f" :weight bold))))
;;   :custom
;;   (dashboard-center-content t)
;;   (dashboard-startup-banner 4)
;;   (dashboard-items '((recents . 15)
;;                      (projects . 5)
;;                      (bookmarks . 5)))
;;   (dashboard-set-heading-icons t)
;;   (dashboard-set-file-icons t)
;;   :hook
;;   (after-init . dashboard-setup-startup-hook))
  ;; :config
  ;; (defun my-banner-path (&rest _)
  ;;   "Return the full path to banner."
  ;;   (expand-file-name "banner.txt" user-emacs-directory))
  ;; (advice-add #'dashboard-get-banner-path :override #'my-banner-path))

;; see https://emacs.stackexchange.com/questions/19506/suppress-warning-assignment-to-free-variable-and-others
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
