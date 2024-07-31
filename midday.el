;;; midday.el --- Clean up unneeded buffers -*- lexical-binding: t -*-

;; Copyright (C) 2024  Federico Tedin

;; Author: Federico Tedin <federicotedin@gmail.com>
;; Maintainer: Federico Tedin <federicotedin@gmail.com>
;; Homepage: https://github.com/federicotdn/midday
;; Keywords: tools
;; Package-Version: 1.0.0
;; Package-Requires: ((emacs "26.3"))

;; This file is NOT part of GNU Emacs.

;; midday is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; midday is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with midday.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; Midday allows you to quickly and easily clean (kill) any buffers
;; which you might not need anymore, based on a set of user-defined
;; predicates.

;;; Code:

(defgroup midday nil
  "A tool for quickly cleaning up unneeded buffers."
  :prefix "midday-"
  :group 'tools)

(defcustom midday-confirm-kill 'y-or-n-p
  "How to ask for confirmation when killing buffers.
If set to nil, midday will not ask for confirmation.  If the value is
non-nil, it should be set to a predicate function, like `y-or-n-p'."
  :type '(choice (const :tag "No confirmation" nil)
                 (function :tag "Confirmation function")))

(cl-defstruct midday--predicate function type name)

(defalias 'midday-predicate #'make-midday--predicate)

(defvar midday-predicates nil)

(defvar midday-debug nil)

(defvar-local midday--protected nil)

(defun midday-protected-matcher ()
  (lambda (buf)
    (buffer-local-value 'midday--protected buf)))

(defun midday-buffer-displayed-matcher ()
  (lambda (buf)
    (get-buffer-window buf)))

(defun midday-empty-buffer-matcher ()
  (lambda (buf)
    (zerop (buffer-size buf))))

(defun midday-process-matcher ()
  (lambda (buf)
    (get-buffer-process buf)))

(defun midday-mark-ring-matcher ()
  (lambda (buf)
    (with-current-buffer buf
      mark-ring)))

(defun midday-undo-list-matcher ()
  (lambda (buf)
    (with-current-buffer buf
      (consp buffer-undo-list))))

(defun midday-empty-undo-list-matcher ()
  (lambda (buf)
    (with-current-buffer buf
      (null buffer-undo-list))))

(defun midday-buffer-modified-matcher ()
  (lambda (buf)
    (with-current-buffer buf
      (buffer-modified-p))))

(defun midday-major-mode-matcher (mode)
  (let ((buf-mode mode))
    (lambda (buf)
      (with-current-buffer buf
        (eq major-mode buf-mode)))))

(defun midday-buffer-name-matcher (name)
  (let ((buf-name name))
    (lambda (buf)
      (string= (buffer-name buf) buf-name))))

(defun midday-buffer-name-regexp-matcher (name-regexp)
  (let ((buf-name-regexp name-regexp))
    (lambda (buf)
      (string-match buf-name-regexp (buffer-name buf)))))

(defun midday-buffer-local-variable-bound-and-set (symbol)
  (lambda (buf)
    (when (buffer-local-boundp symbol buf)
      (buffer-local-value symbol buf))))

(setq midday-predicates
      (list
       (midday-predicate :function (midday-empty-buffer-matcher)
                         :type 'kill
                         :name "Kill empty")
       (midday-predicate :function (midday-major-mode-matcher 'dired-mode)
                         :type 'kill
                         :name "Kill dired-mode")
       (midday-predicate :function (midday-major-mode-matcher 'magit-log-mode)
                         :type 'kill
                         :name "Kill magit-log-mode")
       (midday-predicate :function (midday-major-mode-matcher 'magit-diff-mode)
                         :type 'kill
                         :name "Kill magit-diff-mode")
       (midday-predicate :function (midday-major-mode-matcher 'magit-revision-mode)
                         :type 'kill
                         :name "Kill magit-revision-mode")
       (midday-predicate :function (midday-major-mode-matcher 'magit-process-mode)
                         :type 'kill
                         :name "Kill magit-process-mode")
       (midday-predicate :function (midday-buffer-name-matcher "init.el")
                         :type 'kill
                         :name "Kill init.el")
       (midday-predicate :function (midday-empty-undo-list-matcher)
                         :type 'kill
                         :name "Kill empty undo list")
       (midday-predicate :function (midday-buffer-name-regexp-matcher "^\\s-*\\*")
                         :type 'keep
                         :name "Keep special")
       (midday-predicate :function (midday-buffer-modified-matcher)
                         :type 'keep
                         :name "Keep modified")
       (midday-predicate :function (midday-buffer-displayed-matcher)
                         :type 'keep
                         :name "Keep displayed")
       (midday-predicate :function (midday-undo-list-matcher)
                         :type 'keep
                         :name "Keep undo list nonempty")
       (midday-predicate :function (midday-process-matcher)
                         :type 'keep
                         :name "Keep processes")
       (midday-predicate :function (midday-major-mode-matcher 'org-mode)
                         :type 'keep
                         :name "Keep org-mode")
       (midday-predicate :function (midday-protected-matcher)
                         :type 'keep
                         :name "Keep protected")
       (midday-predicate :function (midday-buffer-local-variable-bound-and-set 'bufmoji--original-name)
                         :type 'keep
                         :name "Keep bufmojified")
       ))

(defun midday--debug (format-string &rest args)
  "Call `message' with FORMAT-STRING and ARGS if `midday-debug' is non-nil."
  (when midday-debug
    (apply #'message (concat "Midday: " format-string) args)))

(defun midday--any-predicate (type buf)
  "Return the first predicate of TYPE matching BUF in `midday-predicates'."
  (catch 'end
    (dolist (pred midday-predicates)
      (unless pred
        (user-error "Nil predicate found in `midday-predicates'"))
      (when (and (eq (midday--predicate-type pred) type)
                 (funcall (midday--predicate-function pred) buf))
        (throw 'end pred)))))

(defun midday ()
  "Clean up unneeded buffers according to predicates in `midday-predicates'."
  (interactive)
  (let (kill-list prompt)
    (dolist (buf (buffer-list))
      (let ((kill-match (midday--any-predicate 'kill buf))
            (keep-match (midday--any-predicate 'keep buf)))
        (if (and kill-match (not keep-match))
            (progn
              (midday--debug "Killing buffer: %s" (buffer-name buf))
              (midday--debug " Reason: matched kill predicate: '%s'"
                             (midday--predicate-name kill-match))
              (push buf kill-list))
          (midday--debug "Keep buffer: %s" (buffer-name buf))
          (if keep-match
              (midday--debug " Reason: matched keep predicate: '%s'"
                             (midday--predicate-name keep-match))
            (midday--debug " Reason: no kill predicate matched")))))
    (setq prompt (concat (format "Kill %s buffer(s)? " (length kill-list))
                         (string-join (mapcar (lambda (b)
                                                (buffer-name b))
                                              kill-list)
                                      ", ")))
    (if kill-list
        (when (or (not midday-confirm-kill)
                  (funcall midday-confirm-kill prompt))
          (dolist (buf kill-list)
            (kill-buffer buf))
          (message "Killed %s buffer(s)" (length kill-list)))
      (message "No buffers to kill"))))

(defun midday-recover ()
  "TODO")

(defun midday-protect ()
  "Manually mark the current buffer as protected.
Use the `midday-protected-matcher' predicate as `keep' to prevent
midday from killing protected buffers."
  (interactive)
  (setq midday--protected t))

;;; midday.el ends here
