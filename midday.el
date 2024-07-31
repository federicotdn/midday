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

(defcustom midday-predicates nil
  "TODO"
  :type '(repeat (restricted-sexp
                  :match-alternatives
                  (midday-predicate-item-p))))

(cl-defstruct midday-predicate-item
  "Structure to hold predicate functions and their type.."
  function type name negate)

(defalias 'midday-predicate #'make-midday-predicate-item
  "Alias to allow easier creation of predicate items.")

(defvar midday-debug nil
  "Set to non-nil to enable debug messages.")

(defvar-local midday-protected nil
  "Set to non-nil to mark a buffer as protected.
This variable is read by the \"Keep protected\" standard predicate.")

(defun midday-standard-predicates ()
  "Return a list of generally useful predicates for midday.
Setting `midday-predicates' to the result of calling this function
will give you a useful starting point for using midday.  You can then
freely add and remove predicates as needed."
  (list
   (midday-predicate :function (midday-matcher-empty-buffer)
                     :type 'kill
                     :name "Kill empty")
   (midday-predicate :function (midday-matcher-major-mode 'dired-mode)
                     :type 'kill
                     :name "Kill dired-mode")
   (midday-predicate :function (midday-matcher-major-mode 'magit-log-mode)
                     :type 'kill
                     :name "Kill magit-log-mode")
   (midday-predicate :function (midday-matcher-major-mode 'magit-diff-mode)
                     :type 'kill
                     :name "Kill magit-diff-mode")
   (midday-predicate :function (midday-matcher-major-mode 'magit-revision-mode)
                     :type 'kill
                     :name "Kill magit-revision-mode")
   (midday-predicate :function (midday-matcher-major-mode 'magit-process-mode)
                     :type 'kill
                     :name "Kill magit-process-mode")
   (midday-predicate :function (midday-matcher-buffer-name "init.el")
                     :type 'kill
                     :name "Kill init.el")
   (midday-predicate :function (midday-matcher-undo-list)
                     :negate t
                     :type 'kill
                     :name "Kill empty undo list")
   (midday-predicate :function (midday-matcher-buffer-name-regexp "^\\s-*\\*")
                     :type 'keep
                     :name "Keep special")
   (midday-predicate :function (midday-matcher-buffer-modified)
                     :type 'keep
                     :name "Keep modified")
   (midday-predicate :function (midday-matcher-buffer-displayed)
                     :type 'keep
                     :name "Keep displayed")
   (midday-predicate :function (midday-matcher-undo-list)
                     :type 'keep
                     :name "Keep undo list nonempty")
   (midday-predicate :function (midday-matcher-process)
                     :type 'keep
                     :name "Keep processes")
   (midday-predicate :function (midday-matcher-major-mode 'org-mode)
                     :type 'keep
                     :name "Keep org-mode")
   (midday-predicate :function (midday-matcher-buffer-local-value 'midday-protected)
                     :type 'keep
                     :name "Keep protected")
   (midday-predicate :function (midday-matcher-buffer-local-value 'bufmoji--original-name)
                     :type 'keep
                     :name "Keep bufmojified")))

(defun midday-matcher-buffer-displayed ()
  (lambda (buf)
    (get-buffer-window buf)))

(defun midday-matcher-empty-buffer ()
  (lambda (buf)
    (zerop (buffer-size buf))))

(defun midday-matcher-process ()
  (lambda (buf)
    (get-buffer-process buf)))

(defun midday-matcher-undo-list ()
  (lambda (buf)
    (with-current-buffer buf
      (consp buffer-undo-list))))

(defun midday-matcher-buffer-modified ()
  (lambda (buf)
    (with-current-buffer buf
      (buffer-modified-p))))

(defun midday-matcher-major-mode (mode)
  (lambda (buf)
    (with-current-buffer buf
      (eq major-mode mode))))

(defun midday-matcher-buffer-name (name)
  (lambda (buf)
    (string= (buffer-name buf) name)))

(defun midday-matcher-buffer-name-regexp (name-regexp)
  (lambda (buf)
    (string-match name-regexp (buffer-name buf))))

(defun midday-matcher-buffer-local-value (symbol)
  (lambda (buf)
    (when (buffer-local-boundp symbol buf)
      (buffer-local-value symbol buf))))

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
      (when (eq (midday-predicate-item-type pred) type)
        ;; Evaluate the predicate with BUF.
        (let ((result (funcall (midday-predicate-item-function pred) buf))
              (negate (midday-predicate-item-negate pred)))
          (when (or (and result (not negate))
                    (and (not result) negate)))
          (throw 'end pred))))))

(defun midday ()
  "Clean up unneeded buffers according to predicates in `midday-predicates'."
  (interactive)
  (unless midday-predicates
    (user-error
     "No predicates found, see documentation for `midday-predicates'"))
  (let (kill-list prompt)
    (midday--debug "Evaluating %s buffer(s)" (length (buffer-list)))
    (dolist (buf (buffer-list))
      (let ((kill-match (midday--any-predicate 'kill buf))
            (keep-match (midday--any-predicate 'keep buf)))
        (if (and kill-match (not keep-match))
            (progn
              (midday--debug "Killing buffer: %s" (buffer-name buf))
              (midday--debug " Reason: matched kill predicate: '%s'"
                             (midday-predicate-item-name kill-match))
              (push buf kill-list))
          (midday--debug "Keep buffer: %s" (buffer-name buf))
          (if keep-match
              (midday--debug " Reason: matched keep predicate: '%s'"
                             (midday-predicate-item-name keep-match))
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
Use the `midday-buffer-local-variable-bound-and-set' predicate as
`keep' with `midday-protect' to prevent midday from killing protected
buffers."
  (interactive)
  (setq midday-protected t))

;;; midday.el ends here
