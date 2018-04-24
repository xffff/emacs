(setq extensions "~/emacs-setup")

;;; package stuff
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(mapc
 (lambda (package)
   (unless (package-installed-p package)
     (progn (message "installing %s" package)
            (package-refresh-contents)
            (package-install package))))
 '(elpy
   helm
   helm-projectile
   helm-company
   helm-spotify-plus
   yasnippet
   vlf
   magit
   exec-path-from-shell
   color-theme
   color-theme-sanityinc-solarized
   color-theme-monokai
   csv-mode
   company
   go-mode
   docker
   docker-compose-mode
   dockerfile-mode
   atomic-chrome
   plantuml-mode))


;;;; mac stuff
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (global-set-key (kbd "s-3") '(lambda () (interactive) (insert "#")))
  (custom-set-variables
   '(gnutls-trustfiles
     (quote
      ("/etc/ssl/certs/ca-certificates.crt"
       "/etc/pki/tls/certs/ca-bundle.crt"
       "/etc/ssl/ca-bundle.pem"
       "/usr/ssl/certs/ca-bundle.crt"
       "/usr/local/share/certs/ca-root-nss.crt"
       "/private/etc/ssl/cert.pem"))))
  (when (memq window-system '(mac ns))
    ;; get the path back... need package
    (exec-path-from-shell-initialize)))


;;;; enable packages etc
(require 'helm)
(require 'helm-config)
(require 'yasnippet)
(require 'vlf)
(require 'color-theme)
(require 'atomic-chrome)
(elpy-enable)

(atomic-chrome-start-server)
(yas/initialize)
(yas-global-mode t)
(helm-mode 1)
(helm-projectile-on)
(semantic-mode 1)
(show-paren-mode t)
(column-number-mode t)
(projectile-global-mode t)
(global-subword-mode t)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode -1)
(windmove-default-keybindings)


(setq browse-url-browser-function 'eww-browse-url)
(setq initial-buffer-choice t)    
(setq initial-scratch-message nil)
(setq wrap-region-mode t)
(setq vc-handled-backends (delq 'Git vc-handled-backends))
(setq indent-tabs-mode nil)
(setq vc-annotate-background nil)
(setq vc-annotate-very-old-color nil)
(setq indent-tabs-mode nil)
(setq global-auto-revert-mode t)
(setq electric-pair-mode t)
(setq display-time-mode t)
(setq case-replace nil)
(setq display-time t)
(setq windmove-wrap-around t)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)


(yas-load-directory (format "%s/%s" extensions "/sfemacs/apex-snippets"))
(color-theme-initialize)
(color-theme-monokai)
;; (color-theme-dark-laptop)

;;;; semantic stuff
(global-semantic-decoration-mode 1)
(global-semantic-idle-summary-mode 1)
(global-semantic-highlight-func-mode 1)
(global-semantic-stickyfunc-mode 1)
(add-hook 'speedbar-load-hook (lambda () (require 'semantic/sb)))


;;;; helm options
(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))

(setq helm-split-window-in-side-p t       ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp t    ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount 8                ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
(setq helm-M-x-fuzzy-match t) 

(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match t)
(setq helm-apropos-fuzzy-match t)



;;;; projectile stuff
(setq projectile-indexing-method 'alien)
(setq projectile-enable-caching t)
(setq projectile-remember-window-configs t)
(setq projectile-completion-system 'helm)



;;;; magit options
(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-save-repository-buffers nil)


;;; nicer backward kill word
;;; http://david.rothlis.net/emacs/ergonomics.html
(defun kill-region-or-backward-kill-word (&optional arg region)
  "`kill-region' if the region is active, otherwise `backward-kill-word'"
  (interactive
   (list (prefix-numeric-value current-prefix-arg) (use-region-p)))
  (if region
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))



;;;; keybindings
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>")) ; ergonomic backspace
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(define-key global-map "\C-c c" 'org-capture)
(global-set-key (kbd "C-w") 'kill-region-or-backward-kill-word)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-Y") 'helm-show-kill-ring)
(global-set-key (kbd "M-M") 'helm-all-mark-rings)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-c h g") 'helm-google-suggest)
(global-unset-key  (kbd "C-x C-c"))
(global-set-key (kbd "C-x C-q") 'save-buffers-kill-terminal) ; no more fat fingers


(define-key minibuffer-local-map
  (kbd "C-w")
  'kill-region-or-backward-kill-word)

;;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-/") 'helm-company)


;;; company mode options
(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))
(setq company-backends
      (quote
       ((company-semantic
         company-dabbrev-code
         company-dabbrev
         company-keywords
         company-files)
        company-capf
        company-yasnippet)))
(setq completion-styles (quote
                         (partial-completion
                          substring)))
(setq company-dabbrev-downcase nil)
(setq company-idle-delay 0.5)


;;; file extension hooks
(add-to-list 'auto-mode-alist '("\\.cls\\'" . apex-mode))
(add-to-list 'auto-mode-alist '("\\.trigger\\'" . apex-mode))
(add-to-list 'auto-mode-alist '("\\.page\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.cmp\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.evt\\'" . nxml-mode))

(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
(add-to-list 'save-some-buffers-action-alist
             `(?r ,(lambda (buf) (revert-buffer buf))
                  ,(purecopy "revert the buffer")))
(add-to-list 'save-some-buffers-action-alist
             `(?k ,(lambda (buf) (kill-buffer buf))
                  ,(purecopy "kill the buffer")))



;;;; java stuff
(add-hook 'prog-mode-hook (lambda ()
			    (setq c-basic-offset 4
				  tab-width 4
				  indent-tabs-mode nil)
                            (electric-pair-mode t)))


;;; plantuml stuff
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))


;;;; python stuff
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(defun run-python-once ()
  (remove-hook 'python-mode-hook 'run-python-once)
  (run-python))
(add-hook 'python-mode-hook 'run-python-once)

;; https://emacs.stackexchange.com/questions/30082/your-python-shell-interpreter-doesn-t-seem-to-support-readline
(with-eval-after-load 'python
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_"))))



;;;; org-stuff
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-agenda-custom-commands
      '(("c" "Calendar" agenda ""
         ((org-agenda-ndays 7)                          ;; [1]
          (org-agenda-start-on-weekday 0)               ;; [2]
          (org-agenda-time-grid nil)
          (org-agenda-repeating-timestamp-show-all t)   ;; [3]
          (org-agenda-entry-types '(:timestamp :sexp))))  ;; [4]
        ;; other commands go here
        ))
(setq org-tags-column -100)
(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
        ("STARTED" . "yellow")
        ("CANCELED" . (:foreground "blue" :weight bold))
        ("WAITING" . (:foreground "purple" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))))
(setq org-todo-keywords
      '("TODO"
        "STARTED"
        "CANCELED"
        "WAITING(w@/!)"
        "DONE"))
(setq org-startup-with-inline-images t)
(add-hook 'org-mode-hook 'iimage-mode)

;;; Use different font in org mode
(defun org-mode-buffer-face ()
  "Make org mode more readable"
  (interactive)
  (setq buffer-face-mode-face
        '(:family "Menlo" :height 150 :width semi-condensed))
  (toggle-truncate-lines)
  (toggle-word-wrap)
  (visual-line-mode)
  (buffer-face-mode))

(setq org-blank-before-new-entry
      '((heading . always)
       (plain-list-item . nil)))

(add-hook 'org-mode-hook 'org-mode-buffer-face)


;;;; custom-stuff
(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-char start)
            (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))

(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))

(defun which-active-modes ()
  "Give a message of which minor modes are enabled in the current buffer."
  (interactive)
  (let ((active-modes))
    (mapc (lambda (mode) (condition-case nil
                             (if (and (symbolp mode) (symbol-value mode))
                                 (add-to-list 'active-modes mode))
                           (error nil) ))
          minor-mode-list)
    (message "Active modes are %s" active-modes)))

(defun remove-newlines-in-region (s e)
  (interactive "r")
  (save-restriction
    (narrow-to-region s e)
    (goto-char (point-min))
    (while (search-forward "
" nil t) (replace-match " " nil t))))

(defun insert-braces ()
  (interactive)
  (if (region-active-p)
      (insert-pair 1 ?{ ?})
    (insert "{}")
    (backward-char)))

(defun insert-quotations (&optional arg)
  "Enclose following ARG sexps in quotation marks.
Leave point after open-paren."
  (interactive "*P")
  (insert-pair arg ?\' ?\'))

(defun insert-quotes (&optional arg)
  "Enclose following ARG sexps in quotes.
Leave point after open-quote."
  (interactive "*P")
  (insert-pair arg ?\" ?\"))

(defun insert-backquote (&optional arg)
  "Enclose following ARG sexps in quotations with backquote.
Leave point after open-quotation."
  (interactive "*P")
  (insert-pair arg ?\` ?\'))




;;; cant remember why I have it stuff
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)


;; make backup to a designated dir, mirroring the full path
;; http://ergoemacs.org/emacs/emacs_set_backup_into_a_directory.html
(defun my-backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir "~/.emacs.d/emacs-backup/")
         (backupFilePath
          (replace-regexp-in-string "//" "/" (concat backupRootDir fpath "~"))))
    (make-directory
     (file-name-directory backupFilePath)
     (file-name-directory backupFilePath))
    backupFilePath
  )
)

(setq make-backup-file-name-function 'my-backup-file-name)

;; https://stackoverflow.com/questions/8674912/how-to-collapse-whitespaces-in-a-region
(defun just-one-space-in-region (beg end)
  "replace all whitespace in the region with single spaces"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "\\s-+" nil t)
        (replace-match " ")))))
