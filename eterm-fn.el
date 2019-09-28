;;; eterm-fn.el --- Function (F1--F12) keys for term -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Bruno Félix Rezende Ribeiro <oitofelix@gnu.org>

;; Author: Bruno Félix Rezende Ribeiro <oitofelix@gnu.org>
;; Keywords: terminals
;; Package: eterm-fn
;; Homepage: https://github.com/oitofelix/eterm-fn
;; Version: 0
;; Package-Requires: ((emacs "25"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides the ‘eterm-fn-mode’: a global minor mode that
;; makes term mode capable of handling the keyboard function keys
;; (F1--F12).  This consists of detecting their presses and sending
;; their respective escape codes to the underlying process and also
;; providing a terminfo database to export such capabilities to
;; ncurses-based applications.  The X11R6 xterm’s escape codes are
;; used.

;; Both standard 16 colors and extended 256 colors terminals are
;; supported.  The latter is provided by package ‘eterm-256color’,
;; which is automatically detected in case it’s present.

;; Customize the variable ‘eterm-fn-mode’ to enable this mode globally.

;;; Code:


(require 'term)
(eval-when-compile (require 'subr-x))


(defconst eterm-fn-base (file-name-directory load-file-name))

;;;###autoload
(define-minor-mode eterm-fn-mode
  "Toggle Eterm-Fn mode on or off.
With a prefix argument ARG, enable Eterm-Fn mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is ‘toggle’.

This global minor mode makes term mode capable of handling the
keyboard function keys (F1--F12).  This consists of detecting
their presses and sending their respective escape codes to the
underlying process and also providing a terminfo database to
export such capabilities to ncurses-based applications.  The
X11R6 xterm’s escape codes are used.

Both standard 16 colors and extended 256 colors terminals are
supported.  The latter is provided by package ‘eterm-256color’,
which is automatically detected in case it’s present.

When enabled the variable ‘term-term-name’ is defined to
\"eterm-fn-color\" or \"eterm-fn-256color\" as appropriate.  The
required terminfo capability database is automatically compiled
and installed (if needed --- as well as its dependencies).

Customize the variable ‘eterm-fn-mode’ to enable this mode globally."
  :group 'term
  :global t
  :require 'eterm-fn
  (if eterm-fn-mode
      (eterm-fn--enable)
    (eterm-fn--disable)))

(defun eterm-fn--enable ()
  "Enable ‘eterm-fn-mode’.

Meant to be invoke by command ‘eterm-fn-mode’."
  (setq term-term-name
        (or (and (featurep 'eterm-256color) "eterm-fn-256color")
            "eterm-fn-color"))
  (apply #'eterm-fn--install-tic-from-src-dir
         term-term-name eterm-fn-base
         (or (and (featurep 'eterm-256color)
                  '("eterm-256color" "eterm-color"))
             '("eterm-color")))
  (dotimes (i 12)
    (define-key term-raw-map
      (vector (intern (concat "f" (int-to-string (1+ i)))))
      #'eterm-fn-send-key)))

(defun eterm-fn--disable ()
  "Disable ‘eterm-fn-mode’.

Meant to be invoked by command ‘eterm-fn-mode’ and by
‘unload-feature’ when unloading feature ‘eterm-fn’."
  (setq term-term-name
        (or (and (featurep 'eterm-256color) "eterm-256color")
            "eterm-color"))
  (substitute-key-definition #'eterm-fn-send-key nil term-raw-map))

(defun eterm-fn-unload-function ()
  "Disable ‘eterm-fn-mode’ just before unloading feature ‘eterm-fn’."
  (eterm-fn-mode -1))

(defun eterm-fn-send-key ()
  "Send last pressed function key escape code to terminal sub-process.
In case the last key press is not a function key, send nothing.
Use X11R6 xterm’s escape code.

The global minor mode command ‘eterm-fn-mode’ bounds this
function in the map ‘term-raw-map’ to the function keys F1--F12
so their pressing in a term buffer send their respective escape
codes to the buffer’s sub-process."
  (interactive)
  (term-send-raw-string
   (alist-get last-input-event
	      '((f1 . "[11~")
		(f2 . "[12~")
		(f3 . "[13~")
		(f4 . "[14~")
		(f5 . "[15~")
		(f6 . "[17~")
		(f7 . "[18~")
		(f8 . "[19~")
		(f9 . "[20~")
		(f10 . "[21~")
		(f11 . "[22~")
		(f12 . "[23~"))
              "")))

(cl-defun eterm-fn--install-tic-from-src-dir
    (name dir &rest deps &aux
	  (ti-src-file-name (expand-file-name (format "%s.ti" name) dir))
	  (tic-src-file-name (expand-file-name name dir))
	  (tic-dest-file-name
	   (expand-file-name name
			     (expand-file-name (substring name 0 1)
					       "~/.terminfo"))))
  "Install compiled terminfo capability database NAME located in DIR.
If it is not compiled, first compile it and its dependencies
DEPS (in case there is any) recursively.  If it is already
installed do nothing.

Parameter DEPS is the same list of dependencies accepted by
‘eterm-fn--compile-ti’.

This is used by command ‘eterm-fn-mode’ to install the
appropriate compiled terminfo databases describing the function
keys capabilities."
  (unless (file-exists-p tic-dest-file-name)
    (cond
     ((file-exists-p tic-src-file-name)
      (mkdir (file-name-directory tic-dest-file-name) 'parents)
      (copy-file tic-src-file-name tic-dest-file-name))
     ((file-exists-p ti-src-file-name)
      (apply #'eterm-fn--compile-ti ti-src-file-name deps))
     (t (error "Terminfo description for eterm-fn not found")))))

(cl-defun eterm-fn--compile-ti
    (file-name
     &rest deps
     &aux
     (path `(,@load-path
             ,(expand-file-name (substring (car deps) 0 1) data-directory)))
     (dep-tic (and deps (locate-file (car deps) path)))
     (dep-ti (and deps (locate-file (concat (car deps) ".ti") path)))
     (dep-dir
      (and dep-tic
	   (if (string= (file-name-nondirectory
			 (directory-file-name (file-name-directory dep-tic)))
			(substring (car deps) 0 1))
	       (expand-file-name (format "%s/../" (file-name-directory dep-tic)))
	     (let ((tic-dest-dir (expand-file-name (substring (car deps) 0 1)
						   "~/.terminfo")))
	       (mkdir (file-name-directory tic-dest-dir) 'parents)
	       (copy-file dep-tic tic-dest-dir)
	       "~/.terminfo")))))
  "Compile terminfo capability database FILE-NAME.
In case the immediate element of the list of dependencies DEPS is
not compiled, first compile it and its subsequent dependencies
recursively, as needed.

It uses the ‘tic’ terminfo entry-description compiler from
ncurses and output the compiled databases to ‘~/.terminfo’.

This is used by function ‘eterm-fn--install-tic-from-src-dir’ to
compile terminfo databases for installation."
  (when (and deps (not dep-tic) (not dep-ti))
    (error "Terminfo description ‘%s’ depends on ‘%s’ which was not found"
	   (file-name-sans-extension (file-name-nondirectory file-name))
	   (car deps)))
  (when (and (not dep-tic) dep-ti)
    (apply #'eterm-fn--compile-ti dep-ti (cdr deps)))
  (setq dep-dir (or dep-dir "~/.terminfo"))
  (let ((process-environment
	 (cons (format "TERMINFO=%s" dep-dir) process-environment))
	(tic-buf-name " *tic*"))
    (unless (executable-find "tic")
      (error "Terminfo entry-description compiler ‘tic’ not found"))
    (when (get-buffer tic-buf-name) (kill-buffer tic-buf-name))
    (with-current-buffer (get-buffer-create tic-buf-name)
      (unless (eq 0 (call-process "tic" nil `(,(current-buffer) t) nil
				  "-o" (expand-file-name "~/.terminfo")
                                  file-name))
	(error "Error compiling terminfo description: %s (TERMINFO=\"%s\")"
	       (string-trim-right (buffer-string) "[[:blank:]\n]*$") dep-dir)))))


(provide 'eterm-fn)

;;; eterm-fn.el ends here
