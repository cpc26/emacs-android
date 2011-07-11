;;; emacs-android.el --- Android Development with GNU/Emacs

;; Copyright (C) 2011 Yoni Rabkin
;; Author: Yoni Rabkin <yonirabkin@member.fsf.org>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Setup:
;;
;; To use font-locking and automatically enable auto-revert-tail-mode
;; on the logcat file, first redirect the logcat output into such a
;; file, in this case I call the file "logcat". For instance, I run
;; something like this in a Screen session:
;;
;;         $ adb -s emulator-5554 logcat > /some/place/logcat
;;
;; Then have Emacs run `android-logcat-mode' when you open the file:
;;
;; (add-to-list 'auto-mode-alist '("logcat" . android-logcat-mode))
;;
;; Load the package when Emacs starts:
;;
;; (require 'emacs-android)
;;
;; Finally, visit the logcat file. If you did everything right then
;; the logcat buffer should be in "(Android-Logcat Tail)" mode.
;;
;; `emacs-android.el' can also find errors in the log file and jump to
;; the file and line number in question. Currently the file with the
;; error needs to be open in an existing buffer (this will probably be
;; changed in future).
;;
;; Set the package name for the package (for example: my.program.com)
;; you want stuff to be found:
;;
;; (setq android-logcat-java-package "my.program.com")

;;; Use:
;;
;;

;;; Code:

(defvar android-logcat-buffer nil
  "Buffer for adb logcat output.")

(defvar android-logcat-error-file-and-line-regexp
  "(\\(.+\..+\\):\\([0-9]+\\))"
  "Regular expression for a logcat error file and line number.")

(defvar android-logcat-error-line-start-regexp
  "^E/"
  "Regular expression for the start of a line with an error.")

(defvar android-logcat-java-package nil
  "Package name for looking through logs.")

;;; ------------------------------------------------------------------
;;; Logcat
;;; ------------------------------------------------------------------

(defvar android-logcat-font-lock-keywords
  (list '("^V/.*$" . font-lock-comment-face)
	'("^W/.*$" . font-lock-constant-face)
	'("^E/.*$" . font-lock-warning-face)))

(define-derived-mode android-logcat-mode text-mode "Android-Logcat"
  "Major mode for looking at Android logcat output."
  (interactive)
  (auto-revert-tail-mode)
  (goto-char (point-max))
  (set (make-local-variable 'font-lock-defaults)
       '(android-logcat-font-lock-keywords))
  (setq android-logcat-buffer (buffer-name))
  (font-lock-mode 1))

(defconst android-logcat-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "n") 'android-logcat-find-next-error)
    (define-key map (kbd "p") 'android-logcat-find-previous-error)
    (define-key map (kbd "N") 'android-logcat-visit-next-error)
    (define-key map (kbd "P") 'android-logcat-visit-previous-error)
    (define-key map (kbd "RET") 'android-logcat-visit-this-error)
    map)
  "Keymap for `android-logcat-mode'.")

(defun android-logcat-assert-setup ()
  "Assert that some critical variables are set."
  (when (not (get-buffer android-logcat-buffer))
    (error "Cannot find ADB logcat buffer"))
  (when (not android-logcat-java-package)
    (error "`android-logcat-java-package not set")))

(defun android-logcat-find-next-error-start-line ()
  "Find the next error start line."
  (when (not
	 (re-search-forward android-logcat-error-line-start-regexp
			    (point-max) t))
    (error "Past last error")))

(defun android-logcat-find-previous-error-start-line ()
  "Find the previous error start line."
  (when (not
	 (re-search-backward android-logcat-error-line-start-regexp
			     (point-min) t))
    (error "Past first error")))

(defun android-logcat-find-error (forward &optional no-visit)
  "Find error line with our package and file:line-number pair.

If FORWARD is non-nil then search forward in the buffer,
otherwise search backward. If NO-VISIT is non-nil then do not
visit the buffer and line-number referenced in the error line."
  (android-logcat-assert-setup)
  (switch-to-buffer android-logcat-buffer)
  ;; Keep looking until we find a line that has both our package name
  ;; a file:line-number pair.
  (while (not (re-search-forward
	       (concat
		android-logcat-java-package
		".+"
		android-logcat-error-file-and-line-regexp)
	       (point-at-eol) t))
    (if forward
	(android-logcat-find-next-error-start-line)
      (android-logcat-find-previous-error-start-line)))
  (if no-visit
      (goto-char (point-at-bol))
    (let ((file (match-string 1))
	  (line-number (parse-integer (match-string 2))))
      (switch-to-buffer file)
      (goto-line line-number))))

(defun android-logcat-find-next-error ()
  "Find the next error in the logcat buffer."
  (interactive)
  (goto-char (point-at-eol))
  (android-logcat-find-error t t))

(defun android-logcat-find-previous-error ()
  "Find the previous error in the logcat buffer."
  (interactive)
  (forward-line -1)
  (android-logcat-find-error nil t))

(defun android-logcat-visit-next-error ()
  "Visit the next error in the logcat buffer."
  (interactive)
  (goto-char (point-at-eol))
  (android-logcat-find-error t))

(defun android-logcat-visit-previous-error ()
  "Visit the previous error in the logcat buffer."
  (interactive)
  (forward-line -1)
  (android-logcat-find-error nil))

(defun android-logcat-visit-this-error ()
  "Visit the error on this line in the logcat buffer.

If there is no error on this line then visit the next error."
  (interactive)
  (goto-char (point-at-bol))
  (android-logcat-find-error t))

;;; ------------------------------------------------------------------
;;; Documentation
;;; ------------------------------------------------------------------

(defun android-search ()
  "Search the Android developers site for the word at point."
  (interactive)
  (let ((w (word-at-point)))
    (if w
	(browse-url
	 (concat "http://developer.android.com/search.html#q="
		 (url-hexify-string w)
		 "&t=0"))
      (error "no word at point"))))

(provide 'emacs-android)

;;; emacs-mode.el ends here.
