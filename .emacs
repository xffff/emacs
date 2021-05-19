(setq extensions "~/Documents/emacs-setup")

;;; package stuff
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
; (package-initialize)

(mapc
 (lambda (package)
   (unless (package-installed-p package)
     (progn (message "installing %s" package)
            (package-refresh-contents)
            (package-install package))))
 '(atomic-chrome
   company
   csv-mode
   docker
   docker-compose-mode
   dockerfile-mode
   elpy
   exec-path-from-shell
   go-mode
   flycheck
   json-mode
   js2-mode
   magit
   multiple-cursors
   plantuml-mode
   powerline
   ob-http
   org-alert
   org-mind-map
   org-bullets
   org-roam
   org-roam-server
   ox-reveal
   oauth2
   shx
   solarized-theme
   vlf
   yasnippet
   web-mode
   which-key))


;;;; mac stuff
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)

                                        ; why did I get a DE keyboard
  (global-set-key "\M-l" '(lambda () (interactive) (insert "@")))
  (global-set-key "\M-5" '(lambda () (interactive) (insert "[")))
  (global-set-key "\M-6" '(lambda () (interactive) (insert "]")))
  (global-set-key "\M-7" '(lambda () (interactive) (insert "|")))
  (global-set-key "\M-/" '(lambda () (interactive) (insert "\\")))
  (global-set-key "\M-8" '(lambda () (interactive) (insert "{")))
  (global-set-key "\M-9" '(lambda () (interactive) (insert "}")))
  (global-set-key "\M-n" '(lambda () (interactive) (insert "~")))

  (define-key minibuffer-local-map "\M-n" "~")
  
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

(server-start)

;;;; enable packages etc
(require 'yasnippet)
(require 'vlf)
(require 'atomic-chrome)
(require 'multiple-cursors)
(require 'powerline)
(require 'flycheck)
(require 'org-roam)
(require 'ox-reveal)

(elpy-enable)
(powerline-default-theme)
(atomic-chrome-start-server)
(yas/initialize)
(yas-global-mode t)
(semantic-mode 1)
(show-paren-mode t)
(column-number-mode t)
(global-subword-mode t)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode -1)
(windmove-default-keybindings)
(which-key-mode)

;(setq browse-url-browser-function 'eww-browse-url)
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
(setq shell-command-switch "-ic")
(setq which-key-popup-type 'minibuffer)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(load-file (format "%s/%s" extensions "/sfemacs/apex-mode/apex-mode.el"))
(yas-load-directory (format "%s/%s" extensions "/sfemacs/apex-snippets"))

;;;; semantic stuff
(global-semantic-decoration-mode 1)
(global-semantic-idle-summary-mode 1)
(global-semantic-highlight-func-mode 1)
(global-semantic-stickyfunc-mode 1)
(add-hook 'speedbar-load-hook (lambda () (require 'semantic/sb)))


;;;; alert stuff
(setq alert-default-style 'osx-notifier)

;;;; flycheck stuff

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; use web-mode for .js files
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;;;; node modules
;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)


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

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;;; keybindings
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>")) ; ergonomic backspace

(global-set-key (kbd "C-w") 'kill-region-or-backward-kill-word)

;; no more fat fingers
(global-unset-key (kbd "C-x C-c"))
(global-set-key (kbd "C-x C-q") 'save-buffers-kill-terminal)

;; multi cursor stuff
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(define-key minibuffer-local-map
  (kbd "C-w")
  'kill-region-or-backward-kill-word)

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
(setq plantuml-output-type "png")
(setq plantuml-default-exec-mode "jar")

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
(setq python-shell-interpreter "python3")


;;;; org-stuff

;; This is an Emacs package that creates graphviz directed graphs from
(require 'ox-org)



(setq org-mind-map-engine "dot")       ; Default. Directed Graph
;; (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
;; (setq org-mind-map-engine "twopi")  ; Radial Layout
;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
;; (setq org-mind-map-engine "sfdp")   ; Multiscale version of fdp for the layout of large graphs
;; (setq org-mind-map-engine "twopi")  ; Radial layouts
;; (setq org-mind-map-engine "circo")  ; Circular Layout

(add-hook 'org-mode-hook (lambda() (org-bullets-mode 1)))

(setq org-hide-emphasis-markers t)
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))


(setq org-image-actual-width 100)
(setq org-reveal-root "file:///Applications/reveal.js/")
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
      '((sequence "TODO" "STARTED" "WAITING(w@/!)" "|")
        (sequence "|" "DONE" "CANCELED")))


;; reload org
(org-reload)

(setq org-startup-with-inline-images nil)


;; stop these annoying keybindings that interfere with windmove
(define-key org-mode-map (kbd "<S-up>") nil)
(define-key org-mode-map (kbd "<S-down>") nil)
(define-key org-mode-map (kbd "<S-left>") nil)
(define-key org-mode-map (kbd "<S-right>") nil)

(add-to-list 'org-src-lang-modes '("http" . ob-http))
(add-to-list 'org-src-lang-modes '("python" . python))
(add-to-list 'org-babel-load-languages '(http . t))
(add-to-list 'org-babel-load-languages '(shell . t))
(add-to-list 'org-babel-load-languages '(python . t))

(setq org-confirm-babel-evaluate nil)

(require 'ob-js)
(add-to-list 'org-babel-load-languages '(js . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-to-list 'org-babel-tangle-lang-exts '("js" . "js"))


(add-hook 'org-mode-hook 'iimage-mode)

(defun org-config-fill-prefix ()
  "Set `fill-prefix' to the empty string."
  (setq fill-prefix ""))

(add-hook 'org-mode-hook #'org-config-fill-prefix)

(setq org-alert-headline-regexp "\\(Sched.+:.+TODO.+\\|Deadline:.+TODO.+\\)")

;;; Use different font in org mode
(defun org-mode-buffer-face ()
  "Make org mode more readable"
  (interactive)
  (toggle-truncate-lines)
  (toggle-word-wrap)
  (visual-line-mode)
  (buffer-face-mode))

(let* ((variable-tuple
        (cond ((x-list-fonts "Menlo")         '(:font "Menlo"))
              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
              ((x-list-fonts "Verdana")         '(:font "Verdana"))
              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
              (nil (warn "Cannot find a Sans Serif Font"))))))

(custom-theme-set-faces
 'user
 `(org-level-4 ((t (:height 1))))
 `(org-level-3 ((t (:height 1.1))))
 `(org-level-2 ((t (:height 1.2))))
 `(org-level-1 ((t (:height 1.3))))
 `(org-document-title ((t (:height 2.0 :underline nil)))))

(setq org-blank-before-new-entry
      '((heading . always)
       (plain-list-item . nil)))

(add-hook 'org-mode-hook 'org-mode-buffer-face)

(setq org-columns-default-format
      "%CATEGORY %25ITEM %TODO %3PRIORITY %SCHEDULED %DEADLINE %EFFORT %ALLTAGS")

(add-to-list 'org-agenda-custom-commands
             '("t" "List of all TODO entries"
               alltodo ""
               ((org-agenda-view-columns-initially t))))

;; org roam stuff
(global-set-key (kbd "C-c c") 'org-roam-dailies-find-today)
(add-hook 'after-init-hook 'org-roam-mode)
(setq org-roam-directory "~/Documents/admin/org/org-roam"
      org-roam-tag-sources '(prop last-directory)
      org-roam-graph-filetype "png"
      org-roam-dailies-directory "dailies/"
      org-roam-dailies-capture-templates
      '(("d" "default" entry
         #'org-roam-capture--get-point
         "* %?"
         :file-name "dailies/%<%Y-%m-%d>"
         :head "#+title: %<%Y-%m-%d>\n\n")))

(setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 31338
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20)


;;;; custom-stuff
(defun my/uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-char start)
            (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))

(defun my/uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))

(defun my/which-active-modes ()
  "Give a message of which minor modes are enabled in the current buffer."
  (interactive)
  (let ((active-modes))
    (mapc (lambda (mode) (condition-case nil
                             (if (and (symbolp mode) (symbol-value mode))
                                 (add-to-list 'active-modes mode))
                           (error nil) ))
          minor-mode-list)
    (message "Active modes are %s" active-modes)))

(defun my/remove-newlines-in-region (s e)
  (interactive "r")
  (save-restriction
    (narrow-to-region s e)
    (goto-char (point-min))
    (while (search-forward "
" nil t) (replace-match " " nil t))))

(defun my/insert-braces ()
  (interactive)
  (if (region-active-p)
      (insert-pair 1 ?{ ?})
    (insert "{}")
    (backward-char)))

(defun my/insert-quotations (&optional arg)
  "Enclose following ARG sexps in quotation marks.
Leave point after open-paren."
  (interactive "*P")
  (insert-pair arg ?\' ?\'))

(defun my/insert-quotes (&optional arg)
  "Enclose following ARG sexps in quotes.
Leave point after open-quote."
  (interactive "*P")
  (insert-pair arg ?\" ?\"))

(defun my/insert-backquote (&optional arg)
  "Enclose following ARG sexps in quotations with backquote.
Leave point after open-quotation."
  (interactive "*P")
  (insert-pair arg ?\` ?\'))


;;; cant remember why I have it stuff
(defun my/minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my/minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my/minibuffer-exit-hook)


;; make backup to a designated dir, mirroring the full path
;; http://ergoemacs.org/emacs/emacs_set_backup_into_a_directory.html
(defun my/backup-file-name (fpath)
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

(setq make-backup-file-name-function 'my/backup-file-name)

;; https://stackoverflow.com/questions/8674912/how-to-collapse-whitespaces-in-a-region
(defun my/just-one-space-in-region (beg end)
  "replace all whitespace in the region with single spaces"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "\\s-+" nil t)
        (replace-match " ")))))

;;; shx stuff
(defun my/shx-cmd-dx (args)
  "open dx"
  (shx-send (concat "sfdx " args)))

(load-theme 'wombat)
(load-theme 'org-beautify)
