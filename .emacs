;; .emacs


;;^^^^^^^^^^^^^^^^^^^^^^^^^    APPEARENCE     ^^^^^^^^^^^^^^^^^^^^

;; Set the default font
;(set-face-attribute 'default nil :font "-*-terminus-*-*-*-*-22-*-*-*-*-*-iso8859-*")
;(set-frame-font "-*-terminus-*-*-*-*-22-*-*-*-*-*-iso8859-*")
;; default frame properties
(setq default-frame-alist
      '((font . "-*-terminus-*-*-*-*-24-*-*-*-*-*-iso8859-*")))


;(require 'color-theme)
;(color-theme-initialize)
;(color-theme-charcoal-black)
(load-theme 'wheatgrass t)


;; Control Fringes
;(setq left-fringe-width 1)
;(setq right-fringe-width 0)
; (fringe-mode '(3 . 1))
; (set-face-attribute 'fringe nil :background "darkslategray")

;; Scroll Bars
(set-scroll-bar-mode 'right)

;; no splash screen, please
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)

;; and no menu-bar or tool-bar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; default to better frame titles
(setq frame-title-format
      '((:eval (if (buffer-file-name)
		   (abbreviate-file-name (buffer-file-name))
		 "%b")) "   %n (%I)"))
;;      (concat  "%b"));@" system-name))

;; show tooltips in the echo area only.
; (setq tooltip-delay 1)
; (setq tooltip-hide-delay 1)
; (setq tooltip-use-echo-area t)


;;***********************  END OF APPEARENCE  ***************************


;;************************  SESSIONS   *************************

;; no backups
(setq backup-inhibited t)

;;; uncomment this line to disable loading of "default.el" at startup
;;(setq inhibit-default-init t)


;;************************  END OF SESSIONS  ***********************


;;***********************  VARIABLES  **********************
; (setq-default fill-column 77)

;; setting the default major mode to text mode. Turns on auto-fill minor mode
;; also.
(setq-default major-mode 'text-mode)
;;(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; set the spell checking program to be aspell, which is far better than
;; ispell
; (setq-default ispell-program-name "aspell")

(setq european-calendar-style t)

(setq inferior-lisp-program "/usr/bin/sbcl")

(setq scheme-program-name "/usr/bin/guile")

(setq woman-cache-filename "~/.woman-cache")

;; enable more than one simultaneous active minibuffers
(setq enable-recursive-minibuffers t)

;; append a newline to each file that doesn't already end with one
(setq require-final-newline t)

;; never enter the debugger if not explicitly told to do so.
(setq eval-expression-debug-on-error nil)


;; sentences end with a single space.
(setq sentence-end-double-space nil)

; (setq sentence-end "[.?!][]\"')]*\\($\\|\t\\| \\)[ \t\n]*")

(setq latex-run-command "perltex --latex=pdflatex")


; No prompting for compile commands
(setq compilation-read-command nil)

;; Use mark only when active
(setq mark-even-if-inactive nil)

;; Kill read only is ok
(setq kill-read-only-ok t)

;; RE-Builder
; (setq reb-re-syntax 'string)


;; Trailing whitespace and newlines
; (setq-default show-trailing-whitespace t)
;(setq-default indicate-empty-lines t)


;; Ido mode
;; (setq confirm-nonexistent-file-or-buffer nil)
;; (setq ido-enable-flex-matching t)
;; (setq ido-enable-last-directory-history nil)
;; (setq ido-confirm-unique-completion nil)
;; (setq ido-create-new-buffer 'always)


;; Org-mode
;; Agenda
;; (if (file-directory-p "~/org")
;;     (setq org-agenda-files (remove-if-not #'file-regular-p (directory-files "~/org" t "\.org$" t))))

;; I don't want to type yes/no 
(fset 'yes-or-no-p 'y-or-n-p)	   	;From MasteringEmacs


;; Don't ask confirmation before killing a buffer associated with a process
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function kill-buffer-query-functions))



;; disabling restrictions
(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(setq-default indent-tabs-mode nil)

;;********************  END OF VARIABLES  *******************


;;*********************** KEY BINDINGS   ************************

;; pressing f2 will make quick switching easy
(global-set-key [f2] 'quick-switch-to-last-buffer)

;; unsetting the insert key
(global-unset-key [insert])

;; set \C-c d to delete-region, which can delete text without copying it
;; to the kill-ring
; (global-set-key "\C-cd" 'delete-region)

;; help function is already bound to F1. So C-h can be assigned to backspace.
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'delete-backward-word)
;; (global-set-key (kbd "C-d") 'delete-forward-word)
;; (global-set-key (kbd "M-d") 'delete-char)




;; C-S-k for delete whole line
(global-set-key (kbd "C-S-k") 'kill-whole-line)

(global-set-key (kbd "M-f") 'forward-to-word)
(global-set-key (kbd "M-v") 'backward-to-word)
;; (global-set-key (kbd "M-f") 'forward-char)
(global-set-key (kbd "C-v") 'backward-char)
(global-set-key (kbd "C-b") 'scroll-up)
(global-set-key (kbd "M-b") 'scroll-down)

(global-set-key "\C-\M-h" 'backward-kill-word)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x \\") 'align-regexp)
;; Font size

;(define-key global-map (kbd "C-+") 'text-scale-increase)
; (define-key global-map (kbd "C--") 'text-scale-decrease)

;; Jump to a definition in the current file. (This is awesome.)
;(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x C-M-f") 'find-file-in-project)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c e") 'eval-and-replace)


;C-xC-n = next window, C-x C-p = previous windows
(global-set-key "\C-x\C-p" 'other-window)


(global-set-key "\C-x\C-n" 'other-window-backward)

;; Window resize
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Next/Previous user buffers
(global-set-key (kbd "<f5>") 'prev-user-buffer)
(global-set-key (kbd "<f6>") 'prev-user-buffer)

;; Web search
(global-set-key (kbd "<f9>") 'dict-lookup)
(global-set-key (kbd "C-<f9>") 'google-lookup)

;; Requires smart-compile
(global-set-key (kbd "C-x C-;") 'smart-compile)


;; For org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(global-set-key (kbd "<f12>") 'popup-popdown-shell)

;; Evil Numbers
(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)

;; Goto-char like in vim
(global-set-key (kbd "M-i") 'search-forward-char)
(global-set-key (kbd "M-I") 'search-backward-char)

(global-set-key (kbd "M-n") 'search-forward-char-again)
(global-set-key (kbd "M-p") 'search-backward-char-again)

;; Acer Jump
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(define-key global-map (kbd "C-c k") 'copy-line)

;; open line as in vim
(define-key global-map (kbd "C-o") 'vim-open-line-below)
(define-key global-map (kbd "C-S-o") 'vim-open-line-above)



;;*********************  END OF KEY BINDINGS  ********************


;; ^^^^^^^^^^^^^^^^^^^^ Functions ^^^^^^^^^^^^^^^^^^^^

(defun other-window-backward ()
  "select the previous window"
  (interactive)
  (other-window -1))


;; defines quick-switch-last-buffer
(defun quick-switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun next-user-buffer (n)
  "Switch to the next user buffer in cyclic order."
  (interactive "p")
  (let (pos newpos (len 0)
	(bufs  (remove-if (lambda (buf) (string-match "^\\s-*\\*" (buffer-name buf))) (buffer-list))))

    (when bufs				;Proceed only if there is at least one user buffer.
      (setq pos (position (current-buffer)  bufs)
	    len (length bufs))
      ;; newpos = (current pos + n)%len if current buffer is a user buffer. Otherwise, 0
      (setq newpos (if pos (mod (+ pos n) len) 0))
      (switch-to-buffer (nth newpos bufs)))))

(defun prev-user-buffer (n)
  "Switch to the previous user buffer in cyclic order."
  (interactive "p")
  (next-user-buffer (- n)))

;; Adapted from Xah Lee
(defun web-lookup (site)
  "Look up for a word or phrase in the site given as argument"
;  (interactive)
  (let (curword)
    (setq curword
	  (if (region-active-p)
	      (buffer-substring-no-properties (region-beginning) (region-end))
	    (thing-at-point 'symbol)))
    (unless (or (null curword) (string-match "^\\s-*$" curword)) ;don't proceed if the string is empty
      (setq curword (replace-regexp-in-string " " "%20" curword)) 
      (browse-url (concat site curword)))))

(defun dict-lookup ()
  "Look up a word of phrase in www.tfd.com"
  (interactive)
  (web-lookup "http://www.tfd.com/"))

(defun google-lookup ()
  "Search for a word or phrase in google."
  (interactive)
  (web-lookup "http://www.google.com/search?q="))


;; Filesystem Coding
(defun to-unix-encoding ()
  "Set the coding system of the current buffer to unix"
  (interactive)
  (set-buffer-file-coding-system 'unix))

(defun to-dos-encoding ()
  "Set the coding system of the current buffer to dos"
  (interactive)
  (set-buffer-file-coding-system 'dos))


;; Define aliases for the above two functions
(defalias 'dos2unix 'to-unix-encoding)
(defalias 'unix2dos 'to-dos-encoding)

;; banish the mouse pointer
;(mouse-avoidance-mode 'banish)


(defun delete-forward-word (n)
  "Delete the word right to the point.
If the point is in the middle of a word, delete the region from the point to the end of the word.
Does not affect the kill-ring"
  (interactive "p")
  (delete-region (point)
		 (save-excursion
		   (forward-word n)
		   (point))))


(defun delete-backward-word (n)
  "Delete the word left to the point.
If the point is in the middle of a word, delete the region from the point to the beginning of the word.
Does not affect the kill-ring"
  (interactive "p")
  (delete-forward-word (- n)))

;; Popup shell
;; (defvar th-shell-popup-buffer nil)

;; (defun th-shell-popup ()
;;   "Toggle a shell popup buffer with the current file's directory as cwd."
;;   (interactive)
;;   (unless (buffer-live-p th-shell-popup-buffer)
;;     (save-window-excursion (shell "*Popup Shell*"))
;;     (setq th-shell-popup-buffer (get-buffer "*Popup Shell*")))
;;   (let ((win (get-buffer-window th-shell-popup-buffer))
;; 	(dir (file-name-directory (or (buffer-file-name)
;; 				      ;; dired
;; 				      dired-directory
;; 				      ;; use HOME
;; 				      "~/"))))
;;     (if win
;; 	(delete-window win)
;;       (pop-to-buffer th-shell-popup-buffer nil t)
;;       (comint-send-string nil (concat "cd " dir "\n")))))


(defun popup-popdown-shell ()
  "Toggles an eshell popup buffer and changes its working directory to current file's.
If there is already a visible eshell window, this command deletes
it. Otherwise, it will select/create an eshell buffer, create a
small window in the current frame, and will pop to the eshell
buffer in that window. The working directory is the working
directory of the buffer from which the command is invoked. If the
buffer was not visiting a file, the wd is taken to be the home
directory"

  (interactive)
  (let ((buf (get-buffer "*eshell*"))   ;buf = the eshell buffer if there is one.
                                        ;otherwise, nil
        (dir (file-name-directory (or (buffer-file-name)
                                   "~/")))
        (win-height (/ (frame-height) 3))
                                   
        win)
    (unless buf                         ;if there is no eshell session open, create one in the background
      (save-window-excursion (eshell))
      (setq buf (get-buffer "*eshell*")))
    (setq win (get-buffer-window buf))  ;win = visible window which displays eshell if any
    (if win                             ;if yes delete it
        (delete-window win)
      ;; Create a new window and go to it
      (select-window (split-window-below (- win-height)))
      ;; otherwise, pop to the buffer
      (pop-to-buffer buf nil t)
      ;; Now change the working directory

      ;; Insert the cd dir
      (eshell-kill-input)               ;clean up the current line so that the next command doesn't
                                        ;get added to that.
      (insert "cd " "\"" dir "\"")
      (eshell-send-input))))

;; eval-and-replace from emacs-starter-kit
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
	     (current-buffer))
    (error (message "Invalid expression")
	   (insert (current-kill 0)))))


;; From Emacs Wiki
(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
		  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))



(defvar search-string nil "Search String for use with search-forward-char-next/prev")

(defun search-forward-char (ch arg)
  "Search forward for the arg'th occurrence of character ch"
  (interactive "cGoto Char: \np")
  (let ((pt (point)))
    (setq search-string 		;If ch is a char, convert it into a quoted string and save; otherwise, just
					;save
	  (if (stringp ch) ch
	    (regexp-quote (char-to-string ch))))

    (if (and (> arg 0) (looking-at search-string)) ;If arg is positive and we are at a search-string, search-forward
					;would consider the string at point as one match. We don't need that.
					;So, in that situation we increment arg by one.
	(setq arg (1+ arg)))

    (search-forward-regexp  search-string nil nil arg)

    (unless (or (= pt (point)) (< arg 0)) ;Unless search was a fail or if arg is a negative, move back one char
					;so as to keep the match-string at point.
      (backward-char))))

(defun search-backward-char (ch arg)
  "Search backward for the arg'th occurrence of character ch"
  (interactive "cGoto Char: \np")
  (search-forward-char ch (- arg)))

(defun search-forward-char-again (arg)
  "Search forward for the arg'th occurrence of the search string"
  (interactive "p")
  (search-forward-char search-string arg))

(defun search-backward-char-again (arg)
  "Search backward for the arg'th occurrence of the search string"
  (interactive "p")
  (search-forward-char search-string (- arg)))


;; Open Line
(defun vim-open-line-below (arg)
  (interactive "p")
  (if (> arg 0)
      (move-end-of-line 1)		;if arg is positive, move to the end of line
					;otherwise, go to the beginning of the line
    (move-beginning-of-line 1))
  (newline (abs arg))			;insert |arg| newlines
  (if (> arg 0) (setq arg (1- arg)))	;if arg is +ve, we're already at one level down, so decrement arg
					;accordingly
  (previous-line (abs arg)))


(defun vim-open-line-above (arg)
  (interactive "p")
  (vim-open-line-below (- arg)))



(defun sum-up-numbers ()
  "Sum all the numbers in the region if there is one or the entire buffer."
  (interactive)
  (save-excursion
  (let ((sum 0) (start-pos (point-min))
        (end-pos (point-max)))
    (if (region-active-p)
        (setq start-pos (region-beginning) end-pos (region-end)))
    (goto-char start-pos)
    (while (re-search-forward "\\([0-9]+\\(\\.[0-9]*\\)?\\)" end-pos t)
      (incf sum (string-to-number (match-string 1))))
    (message "Total: %s" sum))))


(defun test-replace (s1 s2)
  "Test for replacing s1 with s2"
  (interactive)
  (save-excursion
    (let ((start-pos (point-min))
          (end-pos (point-max)))
      (if (region-active-p)
          (setq start-pos (region-beginning) end-pos (region-end)))
      (while (search-forward-regexp s1 end-pos t)
        (if (string-match-p s1 (match-string 0)) (replace-match s2))))))


(defun exchange-strings (s1 s2)
  "Exchange s1 and s2"

  (interactive "sExchange this string: 
sWith this: ")
                                        ;  (save-excursion
  (let ((start-pos (point))
        (end-pos (point-max))
        (qs1 (regexp-quote s1))
        (qs2 (regexp-quote s2))
        (user-input ?y)
        match-data-backup)
    (if (region-active-p)
        (setq start-pos (region-beginning) end-pos (region-end)))
    (goto-char start-pos)
    (while (search-forward-regexp (format "%s\\|%s" qs1 qs2) end-pos t)
                                        ; (message (format "startWhile: %s, %s" (match-beginning 0) (match-end 0)))
                                        ;        (setq match-data-backup (match-data))
      (if (not (char-equal user-input ?!))
          (setq user-input (read-char "Replace? ")))
      (unless (char-equal user-input ?n)
                                        ;          (set-match-data match-data-backup)
                                        ;    (message (format "%s, %s" (match-beginning 0) (match-end 0))))))))
        (if (string-match-p qs1 (match-string 0)) (replace-match qs2) (replace-match qs1))))))



;; ******************** END OF FUNCTIONS ********************



;;********************* EXTRA SERVICES  ************************

;; display time
(display-time)

;; compress and uncompress automatically
; (auto-compression-mode t)

;; I don't want the C-x/C-c/C-v for Cut/Copy/Paste crap. But I would like to have CUA-mode's selection facilities
(cua-selection-mode t)


(which-function-mode 1)


;;*****************  END OF EXTRA SERVICES  *******************



;;*****************  MODES  *******************

;; maxima
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(setq auto-mode-alist (cons '("\\.max" . maxima-mode) auto-mode-alist))

;; pari/gp
(autoload 'gp-mode "/usr/share/doc/pari-gp/emacs/pari" nil t)
(autoload 'gp-script-mode "/usr/share/doc/pari-gp/emacs/pari" nil t)
(autoload 'gp "/usr/share/doc/pari-gp/emacs/pari" nil t)
(autoload 'gpman "/usr/share/doc/pari-gp/emacs/pari" nil t)
(setq auto-mode-alist (cons '("\\.gp$" . gp-script-mode)
			      auto-mode-alist))


;; Perl
;; cperl-mode is made the default mode for perl source
(defalias 'perl-mode 'cperl-mode)



;; Graphviz
; (load-file "~/softies/graphviz-dot-mode.el")


;; sawfish
(setq auto-mode-alist
      (append
       '(("\\.sawfishrc\\'" . lisp-mode)
	("\\.jl\\'" . lisp-mode))
       auto-mode-alist))

;; malayalam latex file
(setq auto-mode-alist
      (append
       '(("\\.mal\\'" . latex-mode))
       auto-mode-alist))

;; Lua
(setq auto-mode-alist (cons '("\.lua$" . lua-mode) auto-mode-alist))
    (autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;; php
(autoload 'php-mode "php-mode.el" "Php mode." t)
(setq auto-mode-alist (append '(("/*.\.php[345]?$" . php-mode)) auto-mode-alist))

;; Python
;; (autoload 'python-mode "python-mode.el" "Python mode." t)
;; (setq auto-mode-alist (append '(("/*.\.py$" . python-mode)) auto-mode-alist))
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-m" 'newline-and-indent)))
(global-set-key (kbd "C-m") 'newline-and-indent)


;; SLIME
; (add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")
; (require 'slime)
; (slime-setup)

;; Haskell
(setq auto-mode-alist
      (append auto-mode-alist
	      '(("\\.[hg]sc\?$" . haskell-mode)
		("\\.hi$" . haskell-mode)
		("\\.l[hg]s" . literate-haskell-mode))))


(autoload 'haskell-mode "haskell-mode"
  "Major mode for Haskell" t)
(autoload 'literate-haskell-mode "haskell-mode"
  "Major mode for literate Haskell" t)


(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(require 'inf-haskell)

;; Prolog
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)





;; Octave
(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

(add-hook 'octave-mode-hook
	  (lambda ()
	    (abbrev-mode 1)
	    (auto-fill-mode 1)
	    (if (eq window-system 'x)
		(font-lock-mode 1))))



;; CEDET
; (load-file "/usr/share/emacs/site-lisp/cedet/common/cedet.el")

;; ECB
; (add-to-list 'load-path "/usr/share/emacs/site-lisp/ecb")
; (require 'ecb)
;To load ecb first after starting it by ecb-activate:
;(require 'ecb-autoloads)

;; org-mode
(require 'org-install)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (haskell . t)
   (perl . t)
   (python . t)
   (sh . t)))


;; AucTeX
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
;(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; Scala
;; (add-to-list 'load-path "/usr/share/ensime/elisp")
;; (add-to-list 'exec-path "/usr/share/ensime")
;; (require 'ensime)
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)



;;*****************  END OF MODES  *******************


;; ******************** PACKAGES ********************

;; Loadpath

(add-to-list 'load-path "~/.emacs.d/lib")

(require 'misc)

; (require 'rainbow-delimiters)

(require 'smart-compile)

;; (require 'ido)
;; (ido-mode 1)
;; (ido-everywhere 1)

;; Icicles
(setq load-path (cons "/usr/share/emacs/site-lisp/icicles" load-path))
(require 'icicles)
(eval-after-load "ring" '(progn (require 'ring+)))
(icy-mode 1)

(require 'evil-numbers)



;; Auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/lib/ac-dict")
(ac-config-default)
(setq ac-auto-start 5)

(define-key ac-mode-map (kbd "M-<f1>") 'auto-complete)


(require 'ac-math)

(add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of {{{latex-mode}}}

(defun ac-latex-mode-setup ()         ; add ac-sources to default ac-sources
  (setq ac-sources
     (append '(ac-source-math-latex ac-source-latex-commands  ac-source-math-unicode)
	       ac-sources)))

(add-hook 'LaTeX-mode-hook 'ac-latex-mode-setup)


;; W3M
;(require 'w3m)


(require 'iy-go-to-char)



;; Contol-lock
(require 'control-lock)
(control-lock-keys)

(require 'ace-jump-mode)


(add-to-list 'load-path "/usr/share/emacs/scala-mode")
(require 'scala-mode-auto)


;; ******************** END OF PACKAGES ********************


;; ********************  Hooks ********************

(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)


;; When forward isearch exits, I want the point to be at the beginning of the match, and not at the end
;; From EmacsWiki
(add-hook 'isearch-mode-end-hook 'goto-match-beginning)
(defun goto-match-beginning ()
  (when (and isearch-forward (not isearch-mode-end-hook-quit))
    (goto-char isearch-other-end)))

;; (setq magic-mode-alist '(("/bin/zsh" . shell-script-mode)))

;; (add-hook 'c-mode-common-hook (lambda () (define-key c++-mode-map "\C-c\C-e" 'smart-compile)))


;; ******************** END OF HOOKS ********************



;; Utilities

;; (require 'emms-setup)
;; (emms-standard)
;; (emms-default-players)


