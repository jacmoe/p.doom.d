;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Variables                                                                        ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my-dark-theme 'catppuccin)    ; catppuccin, misterioso, uwu
(defvar my-light-theme 'tsdh-light)
(defvar my-main-theme my-dark-theme)
(defvar my-theme-shade "dark") ; can be light or dark. Used to color the Boon-mode cursor

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Personal Information                                                             ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq user-full-name "Jacob Moena"
      user-mail-address "jacmoe.dk@gmail.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; General settings                                                                 ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(save-place-mode 1)                                    ; Remember and restore the last cursor location of opened files
(setq confirm-kill-emacs nil)                          ; Yes, I really want to quit.
(setq custom-file (make-temp-file "emacs-custom"))     ; prevent custom from preserving state
(setq inhibit-compacting-font-caches t)                ; for performance reasons
(setq bookmark-save-flag 1)                            ; Save bookmarks each time it changes, not only on exit
(require 'zone)                                        ; Emacs "screensaver"
(zone-when-idle 300)                                   ; Zone out when idle for five minutes.
(setq enable-local-eval t)                             ; Define safe local variables
;; Mouse-avoidance makes the frame "jump" on Windows...
(unless (eq system-type 'windows-nt)
        (if (display-mouse-p) (mouse-avoidance-mode 'banish)))  ; Shove the mouse pointer out of  the way
;; tell Undo-fu to only store linear history, not the full history
(setq undo-fu-session-linear t)
(map! "C-;" nil)                                         ; Don't steal my C-; !

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Visual settings                                                                  ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convince Emacs to trust themes so that we can install them
(setq custom-safe-themes t)
;; Remove all but the first menu entry on the splash screen
(setq +doom-dashboard-menu-sections (cl-subseq +doom-dashboard-menu-sections 0 1))

;; Fonts - ordinary and variable pitch
(setq doom-font (font-spec :family "Overpass Mono" :size 20)
      doom-variable-pitch-font (font-spec :family "Alegreya" :size 28))

;; Theme
(setq doom-theme my-main-theme)

;; Make the modified file name in the modeline orange instead of red
(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))

;; Setting initial size and position of frame
;; It is a necessary hack because Doom doesn't seem to
;; care about my frame size when restoring sessions ...
(if (eq system-type 'windows-nt)
    (setq initial-frame-alist '((top . 38) (left . 66) (width . 124) (height . 32)))
  (setq initial-frame-alist '((top . 38) (left . 76) (width . 130) (height . 38))))
  

;; Misc settings
;; (setq display-line-numbers-type nil)                        ; Do not show line numbers
(display-time-mode 1)                                       ; Display time in modeline
(fringe-mode '(80 . 80))                                    ; Show vertical fringes
(blink-cursor-mode t)                                       ; The cursor should blink
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode) ; Do not highlight current line
(setq global-page-break-lines-mode t)                       ; Pretty page breaks everywhere
(setq confirm-kill-processes nil)                           ; Don't ask to kill running processes when exiting Emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Global keybindings                                                               ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(map! "<f9>" #'doom-big-font-mode)   ; Toggle big font mode
(map!"C-<down>" #'enlarge-window)
(map!"C-<up>" #'shrink-window)
(map!"C-<left>" #'enlarge-window-horizontally)
(map!"C-<right>" #'shrink-window-horizontally)
(map! "C-`" #'diff-buffer-with-file) ; view what is modified
(map! "C-c t d" #'switch-theme) ; switch theme light/dark

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Features                                                                         ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Boon
;; CtrlF
;; Miscellaneous

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Boon                                                                             ;;
;;                                                                                  ;;
;; https://github.com/jyp/boon                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; An Ergonomic Command Mode for Emacs
;; Run tutorial with M-x boon-tutorial
(use-package! boon
  :init
  (require 'boon-qwerty)
  (require 'boon-tutorial)
  :config
  (boon-mode)
  (setq boon-insert-cursor-color "orange")
  (if (equal my-theme-shade "dark")
      (progn
        (setq boon-default-cursor-color "white"))
        
    (setq boon-default-cursor-color "black"))
    
  (define-key boon-command-map "L" 'forward-sentence)
  (define-key boon-command-map "K" 'backward-sentence)
  (define-key boon-command-map "s" 'prot/scroll-center-cursor-mode)
  (define-key boon-command-map "n" 'org-narrow-to-subtree)
  (define-key boon-command-map "N" 'widen)
  (define-key boon-command-map "M" 'doom-modeline-mode)
  (define-key boon-command-map "w" 'org-tracktable-status)
  (define-key boon-command-map "æ" 'boon-smarter-forward)
  (add-hook 'lexic-mode-hook 'turn-off-boon-mode)
  (add-hook 'ibuffer-hook 'turn-off-boon-mode)
  (add-hook 'doom-dashboard-mode 'turn-off-boon-mode)
  (add-hook 'org-capture-mode-hook 'turn-off-boon-mode)
  :bind
  ("<f6>" . turn-on-boon-mode)
  ("<f7>" . turn-off-boon-mode)
  ("C-;" . boon-set-command-state); used to quit insert mode
  ("C-æ" . boon-set-command-state)); used to quit insert mode - Danish version
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; CtrlF                                                                            ;;
;;                                                                                  ;;
;; https://github.com/raxod502/ctrlf                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! ctrlf
  :config
  (ctrlf-mode t))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Miscellaneous                                                                    ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; kill current buffer, without confirmation
(defun delete-current-buffer ()
                                        ; deletes the current buffer
  (interactive)
  (kill-buffer (current-buffer)))
(map! "C-x k" #'delete-current-buffer)

;; align comments
(defun my-align-comments (beginning end)
  "Align comments within marked region."
  (interactive "*r")
  (let (indent-tabs-mode align-to-tab-stop)
    (align-regexp beginning end (concat "\\(\\s-*\\)"
                                        (regexp-quote comment-start)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; better comment box                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jcs-comment-box (b e)
  "Draw a box comment around the region but arrange for the region
to extend to at least the fill column. Place the point after the
comment box."
  (interactive "r")
  (let ((e (copy-marker e t)))
    (goto-char b)
    (end-of-line)
    (insert-char ?  (- fill-column (current-column)))
    (comment-box b e 1)
    (goto-char e)
    (set-marker e nil)))

;; turn on dark theme
(defun go-dark-theme ()
  (interactive)
  (setq boon-default-cursor-color "white")
  (setq my-theme-shade "dark")
  (load-theme my-dark-theme t))
  

;; turn on light theme
(defun go-light-theme ()
  (interactive)
  (setq boon-default-cursor-color "black")
  (setq my-theme-shade "light")
  (load-theme my-light-theme))
  

;; switch between light and dark theme
(defun switch-theme ()
  (interactive)
  (if (equal my-theme-shade "light")
      (go-dark-theme)
    (go-light-theme)))
    
  
