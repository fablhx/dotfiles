;;; early-init.el --- Settings that must precede the init file  -*- lexical-binding: t; -*-

;;; Commentary:
;; Symlinked as ~/.emacs.d/early-init.el.  Emacs reads it before it activates
;; packages and before it creates the first frame, which is the only reason
;; anything here is not in ~/.emacs.

;;; Code:

;; Emacs starts with the GC threshold at 800 kB and the file name handlers
;; armed, which buys a collection every few files loaded and a regexp sweep per
;; `load' - for the hundreds of files package activation and ~/.emacs go on to
;; load.  Neither pays off before the first frame exists.  The originals are
;; restored from `emacs-startup-hook', which Emacs runs even when a startup
;; file signals partway through.
(defconst my-startup-file-name-handler-alist file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

(defun my-restore-startup-defaults ()
  "Undo the startup-only performance settings made at the top of this file."
  ;; Not the 800 kB default: collecting that often costs more in a long-lived
  ;; session, with LSP and flycheck allocating, than the pauses it avoids.
  (setq gc-cons-threshold (* 32 1024 1024)
        gc-cons-percentage 0.1
        file-name-handler-alist my-startup-file-name-handler-alist))

(add-hook 'emacs-startup-hook #'my-restore-startup-defaults)

;; One preloaded file of autoloads for every installed package, instead of one
;; `load' plus a package descriptor read per package.  package.el rewrites it
;; whenever a package is installed or deleted; `M-x package-quickstart-refresh'
;; writes the first one.
(setq package-quickstart t)

;; The initial frame is created between this file and ~/.emacs, so a tool bar
;; or scroll bar switched off there exists for as long as it takes ~/.emacs to
;; get to it: built, then destroyed, with the frame resized around both.
;; Asking for a frame that never has them skips all of it.
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; What is on screen now agrees with the frame parameters, but the modes that
;; own those parameters still read as enabled, so `M-x tool-bar-mode' would
;; turn a tool bar off before it could turn one on.
(setq tool-bar-mode nil
      scroll-bar-mode nil)

;;; early-init.el ends here
