;;; popup-gtags.el --- GNU GLOBAL popup interface

;; Copyright (C) 2012  tabi
;; Author: tabi <koko1000ban@gmail.com>
;; Keywords: popup, gtags

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; * `tb:gtags-find-tag' is `popup' interface of `gtags-find-tag'.

;;; Requirement:
;; * gtags.el http://www.emacswiki.org/emacs/GnuGlobal
;; * popup.el   http://github.com/m2ym/auto-complete
;; * deferred.el http://github.com/kiwanami/emacs-deferred

;;; Installation:

;; drop requirements and this file into a directory in your `load-path',
;; and put these lines into your .emacs file.

;; (require 'tb:popup-gtags)
;; (define-key gtags-mode-map "\M-." 'tb:gtags-find-tag)
;; (define-key gtags-mode-map "\M-," 'gtags-pop-stack)
;; (define-key gtags-mode-map "\M-r" 'tb:gtags-find-rtag)

;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `tb:gtags-use-elscreen'
;;    *If non-nil, use elscreen find file
;;    default = t

;;; ChangeLog:
;; * 0.0.1:
;;   Initial version.

;;; Code:

(require 'popup)
(require 'deferred)

;; lexical-let
(eval-when-compile
  (require 'cl))

;;; Variables:

(defcustom tb:gtags-use-elscreen t
  "Non-nil means use elscreen when open tag buffer."
  :type 'boolean
  :group 'tb:gtags)

(defconst tb:gtags-candidate-pattern
  "\\(^[^ ]+\\)\s+\\([0-9]+\\)\s+\\([^ ]+\\)\s\\(.+\\)")

(defconst tb:gtags-disp-format "%s %s %s %s")

(defconst tb:gtags-command "global")

(defconst tb:gtags-linum-property 'tb:gtags-linum)

(defconst tb:gtags-path-property 'tb:gtags-path)

;;; Functions:

(defun tb:gtags-parse-output ()
  (goto-char (point-min))
  (let (sym linum path rest disp candidates)
    (while (re-search-forward tb:gtags-candidate-pattern nil t)
      (setq sym (match-string-no-properties 1)
            linum (string-to-number (match-string-no-properties 2))
            path (gtags-decode-pathname (match-string-no-properties 3))
            rest (match-string-no-properties 4))
      ;; (message "sym:%s linum:%s path:%s rest:%s" sym linum path rest)
      (setq disp 
            (if rest
                (format tb:gtags-disp-format sym linum path rest)
              sym))
      (setq disp (propertize disp tb:gtags-linum-property linum))
      (setq disp (propertize disp tb:gtags-path-property path))
      (push disp candidates))
    candidates))

(defun tb:gtags-popup-tag (flag)
  (lexical-let ((tagname (thing-at-point 'symbol)))
    (when tagname
      (deferred:$
        (deferred:process-buffer tb:gtags-command (format "-xa%s" flag) tagname)
        (deferred:nextc it
          (lambda (buf)
            (with-current-buffer buf
              (tb:gtags-parse-output))))
        (deferred:nextc it
          (lambda (x)
            (if x
                (popup-menu* x)
              (error tagname))))
        (deferred:nextc it
          (lambda (select)
            (if select
                (let ((linum (get-text-property 0 tb:gtags-linum-property select))
                      (path (get-text-property 0 tb:gtags-path-property select))
                      (prev-buffer (current-buffer)))
                  
                  ;; (message "line:%s path:%s" linum path)
                  ;; (message "%s" (current-buffer))

                  (gtags-push-context)
                  (if tb:gtags-use-elscreen
                      (if gtags-read-only 
                          (elscreen-find-file-read-only path)
                        (elscreen-find-file path))
                    (if gtags-read-only 
                        (find-file-read-only path)
                      (find-file path)))
                  (setq gtags-current-buffer (current-buffer))
                  (goto-line linum)
                  (gtags-mode 1))
              (message "tag: %s not found." tagname))))
        (deferred:error it
          (lambda (err)
            (message "%s: tag not found." err)))))))

(defun tb:gtags-find-tag ()
  (interactive)
  (tb:gtags-popup-tag ""))
  
(defun tb:gtags-find-rtag ()
  (interactive)
  (tb:gtags-popup-tag "r"))

(provide 'popup-gtags)

;;; popup-gtags.el ends here