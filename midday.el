;;; midday.el --- Clean up unused buffers -*- lexical-binding: t -*-

(cl-defstruct midday--predicate function type name)

(defalias 'midday-predicate #'make-midday--predicate)

(defvar midday-predicates nil)

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
       (midday-predicate :function (midday-buffer-name-regexp-matcher "^ ?\\*")
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
       ))

(defun midday--any-predicate (predicate-type val)
  "TODO"
  (catch 'end
    (dolist (pred midday-predicates)
      (when (and (eq (midday--predicate-type pred) predicate-type)
                 (funcall (midday--predicate-function pred) val))
        (message "matched %s %s" val (midday--predicate-name pred))
        (throw 'end t)))))

(defun midday ()
  "Clean up buffer list."
  (interactive)
  (let ((kill-count 0))
    (dolist (buf (buffer-list))
      (message "process %s" buf)
      (when (and (midday--any-predicate 'kill buf)
                 (not (midday--any-predicate 'keep buf)))
        (message "Killing buffer %s" (buffer-name buf))
        (kill-buffer buf)
        (setq kill-count (1+ kill-count))))
    (message "Killed %s buffers" kill-count)))

(defun midday-recover ()
  "TODO")

(defun midday-protect ()
  "TODO"
  (interactive)
  (setq midday--protected t))
