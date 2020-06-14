;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; ======= Basic Setup ===========
(setq ring-bell-function 'ignore
      echo-keystrokes 0.5 ; shows which-key popup quickly
      dired-dwim-target t
      mode-require-final-newline t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(desktop-save-mode t)
(save-place-mode 1)
(show-paren-mode t)
(column-number-mode t)
(winner-mode t)

(setq mac-command-modifier 'meta)

(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
(global-hl-line-mode t) ; high-light current line

(set-default-coding-systems 'utf-8) ; utf-8 にする
(prefer-coding-system 'utf-8)

(let ((gls "/usr/local/bin/gls"))
  (if (file-exists-p gls) (setq insert-directory-program gls)))

;; ======== scroll =========
;; (use-package smooth-scrolling
;;   :config
;;   (smooth-scrolling-mode t))
(setq mouse-wheel-scroll-amount '(1)
      mouse-wheel-progressive-speed nil
      scroll-conservatively 101)

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
  ;; 	      ("C-n" . company-select-next)
  ;; 	      ("C-p" . company-select-previous)))

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
   (add-to-list 'eglot-server-programs '(dart-mode . ("danal")))
   (setq eglot-sync-connect 1)
   (setq eglot-ignored-server-capabilites '(:documentHighlightProvider))
  :hook
  ((c-mode . eglot-ensure)
   ;; (dart-mode . eglot-ensure)
   (kotlin-mode . eglot-ensure)))

;; ========= Ivy ===========
(use-package ivy
  :straight t
  :init
  (setq ivy-initial-input-alist nil)
  :custom
  (ivy-use-virtual-buffer t)
  :config
  (use-package ivy-hydra
    :straight t
    :defer t)
  (use-package flx
    :straight t))

(use-package swiper
  :straight t
  :after ivy
  :config ;; https://taipapamotohus.com/post/swiper_migemo/
  (defun isearch-forward-or-swiper (use-swiper)
    (interactive "p")
    ;; (interactive "P") ;; 大文字のPだと，C-u C-sでないと効かない
    (let (current-prefix-arg)
      (call-interactively (if use-swiper 'swiper 'isearch-forward))))
  (global-set-key (kbd "C-s") 'isearch-forward-or-swiper))

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
  (let* ((sep " \\|\\^\\|\\.\\|\\*")
         (splitted (--map (s-join "" it)
                          (--partition-by (s-matches-p " \\|\\^\\|\\.\\|\\*" it)
                                          (s-split "" str t)))))
    (s-join "" (--map (cond ((s-equals? it " ") ".*?")
                            ((s-matches? sep it) it)
                            (t (migemo-get-pattern it)))
                      splitted))))
(setq ivy-re-builders-alist '((t . ivy--regex-plus)
                              (swiper . ytn-ivy-migemo-re-builder)))
;; さらにrg(ripgrep)でもmigemoを使える
;; https://www.yewton.net/2020/05/25/counsel-rg-migemo/
(setq migemo-options '("--quiet" "--nonewline" "--emacs"))
(setq ivy-re-builders-alist '((t . ivy--regex-plus)
                              (counsel-rg . ytn-ivy-migemo-re-builder)
                              (swiper . ytn-ivy-migemo-re-builder)))
(add-hook 'dired-mode-hook
	  (lambda ()
	    (define-key dired-mode-map (kbd "M-s") 'counsel-rg)))


(use-package counsel
  :straight t
  :bind
  ("C-x C-r" . counsel-recentf)
  :config
  (counsel-mode)
  (setq ivy-initial-inputs-alist nil))

(use-package all-the-icons-ivy-rich
  :straight t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :straight t
  :init (ivy-rich-mode 1))

(use-package prescient :straight t)
(use-package ivy-prescient :straight t)
(use-package company-prescient :straight t)
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
  (doom-modeline-minor-modes nil)
  :hook
  (after-init . doom-modeline-mode)
  :config
  (line-number-mode 0)
  (column-number-mode 0)
  (doom-modeline-def-modeline 'main
			      '(bar window-number matches buffer-info remote-host buffer-position parrot selection-info)
			      '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker)))

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
  :bind (("C-x g" . magit-status)))

(use-package diff-hl
  :straight t
  :hook ((dired-mode . diff-hl-dired-mode)
	 (magit-post-refresh . diff-hl-magit-post-refresh))
  :custom (diff-hl-flydiff-delay 0.5)
  :config (global-diff-hl-mode t))


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
(use-package popwin
  :straight t
  :defer t
  :commands (popwin-mode)
  :config
  (setq popwin:special-display-config
        '(("*Completions*" :noselect t :height 0.4))))

(use-package shell-pop
  :straight t
  :defer t
  :bind ("M-p" . shell-pop)
  :custom
  (shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda () (eshell)))))
  (shell-pop-universal-key "M-p")
  (shell-pop-window-size 50)
  (shell-pop-full-span t)
  (shell-pop-window-position "bottom")
  :commands (shell-pop)
  :config
  (setq eshell-command-aliases-list
        (append
         (list
          (list "ll" "ls -alF")
	  (list "ojt" "~/Projects/ShellScriptProjects/misc/oj_all.sh")
	  (list "ojd" "~/Projects/ShellScriptProjects/misc/oj_all.sh -u $1")))))


;; ===== Typescript ========
(use-package typescript-mode
  :straight t
  :mode (("\\.ts\\'" . typescript-mode)))
(use-package tide
  :straight t
  :commands (tide-mode)
  :hook
  (typescript-mode . tide-mode)
  :config
  (tide-setup)
  (flycheck-mode t)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode t)
  (company-mode-on))

;; ======= PDF =============
(use-package pdf-tools
  :straight t
  :config
  ;; initialise
  (pdf-tools-install)
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


;; ======== Others ========
(use-package spotify
  :straight (:host github :repo "danielfm/spotify.el")
  :config
  ;; (setq counsel-spotify-client-secret "9e4fa4b354894875878b9b0e497083f7")
  ;; (setq counsel-spotify-client-id "bd53c5e86a6746c08b8649bb1ac2735a")
  ;; (setq counsel-spotify-service-name "spotifyd")
  ;; (setq counsel-spotify-use-system-bus-p t))
  (setq spotify-oauth2-client-secret "9e4fa4b354894875878b9b0e497083f7>")
  (setq spotify-oauth2-client-id "bd53c5e86a6746c08b8649bb1ac2735a")
  (setq spotify-transport 'connect)
  (define-key spotify-mode-map (kbd "C-c .") 'spotify-command-map))





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



;; ========= Key maps ===========
;; option + shift hjkl
(global-set-key (kbd "C-Ó") 'windmove-left)
(global-set-key (kbd "C-Ô") 'windmove-down)
(global-set-key (kbd "C-") 'windmove-up)
(global-set-key (kbd "C-Ò") 'windmove-right)
(defun my/split-windmove-right ()
  "Vsplit window and move right."
  (interactive)
  (split-window-right)
  (windmove-right))
(global-set-key (kbd "C-∏") 'my/split-windmove-right)
(global-set-key (kbd "C-Ø") 'delete-other-windows)
(global-set-key (kbd "C-¨") 'delete-window)
(global-set-key (kbd "M-k") 'kill-this-buffer)
(defun counsel-recentf-other ()
  "Find a file on `recentf-list'."
  (interactive)
  (require 'recentf)
  (recentf-mode)
  (ivy-read "Recentf other: " (mapcar #'substring-no-properties recentf-list)
            :action (lambda (f)
                      (with-ivy-window
                        (split-window-horizontally))
                      (other-window 1)
                      (find-file f)
                      (balance-windows))
            :require-match t
            :caller 'counsel-recentf-other))
(global-set-key (kbd "C-x M-r") 'counsel-recentf-other)
(defun counsel-find-file-other-action (x)
  "Find file X."
  (with-ivy-window
    (split-window-horizontally))
  (other-window 1)
  (find-file x)
  (balance-windows))
(defun counsel-find-file-other (&optional initial-input)
  "Forward to `find-file'.
When INITIAL-INPUT is non-nil, use it in the minibuffer during completion."
  (interactive)
  (counsel--find-file-1
   "Find file other: " initial-input
   #'counsel-find-file-other-action
   'counsel-find-file-other))
(global-set-key (kbd "C-x M-f") 'counsel-find-file-other)
;; my/macro is recorded macro. see how to record: https://kb.iu.edu/d/aaxw
;; what this macros does is M-w w, which is easy-kill's save word at point
(global-set-key (kbd "M-d") (fset 'my/macro "\367w"))
(global-set-key (kbd "M-l") (fset 'my/kbd-macro-yyp "\C-a\367\C-y"))
(global-set-key (kbd "C-M-d") (fset 'my/kbd-macro-viw "\200w"))
(global-set-key (kbd "M-c") (fset 'my/kbd-macro-ciw "\367w\C-w"))


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
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (forward-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))
;; Behave like vi's O command
(defun open-previous-line (arg)
  "Open a new line before the current one. 
     See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))
(global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "C-M-o") 'open-previous-line)
;; Autoindent open-*-lines
(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions to cause them to autoindent.")


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Cica" :foundry "nil" :slant normal :weight normal :height 171 :width normal))))
 '(doom-modeline-bar ((t (:background "#6272a4"))))
 '(mode-line ((t (:background "light slate gray" :foreground "black" :box (:line-width -1 :style released-button))))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; see https://emacs.stackexchange.com/questions/19506/suppress-warning-assignment-to-free-variable-and-others
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
