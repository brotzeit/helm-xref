;;; helm-xref.el --- Helm interface for xref results -*- lexical-binding: t -*-

;; Copyright (C) 2017  Fritz Stelzer <brotzeitmacher@gmail.com>

;; Author: Fritz Stelzer <brotzeitmacher@gmail.com>
;; URL: https://github.com/brotzeitmacher/helm-xref
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (helm "1.9.4"))

;;; License:
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Code:

(require 'helm)
(require 'xref)
(require 'cl-seq)

(defvar helm-xref-alist nil
  "Holds helm candidates.")

(defun helm-xref-candidates (xrefs)
  "Convert XREF-ALIST items to helm candidates and add them to `helm-xref-alist'."
  (dolist (xref xrefs)
    (with-slots (summary location) xref
      (let* ((line (xref-location-line location))
             (prefix
              (if line
                  line ""))
             (marker (xref-location-marker location))
             (file (xref-location-group location ))
             candidate)
        (setq candidate
              (concat
               (propertize (car (reverse (split-string file "\\/")))
                           'font-lock-face '(:foreground "cyan"))
               ":"
               (when (string= "integer" (type-of line))
                 (propertize (int-to-string line)
                             'font-lock-face 'compilation-line-number))
               ":"
               summary))
        (push `(,candidate . ,marker) helm-xref-alist)))))

(defun helm-xref-goto-location (location func)
  "Set buffer and point according to xref-location LOCATION.

Use FUNC to display buffer."
  (let ((buf (marker-buffer location))
        (offset (marker-position location)))
    (with-current-buffer buf
      (goto-char offset)
      (funcall func buf))))

(defun helm-xref-source ()
  "Return a `helm' source for xref results."
  (helm-build-sync-source "Helm Xref"
    :candidates (lambda ()
                  helm-xref-alist)
    :persistent-action (lambda (candidate)
                         (helm-xref-goto-location candidate 'display-buffer))
    :action (lambda (candidate)
              (helm-xref-goto-location candidate 'switch-to-buffer))
    :candidate-transformer (lambda (candidates)
                             (let (group
                                   result)
                               (dolist (xref (reverse (cl-sort candidates #'string-lessp :key #'car)))
                                 (cond
                                  ((or (= (length group) 0)
                                       (string= (nth 0 (split-string (car xref) ":"))
                                                (nth 0 (split-string (car (nth -1 group)) ":"))))
                                   (push xref group))
                                  (t
                                   (dolist (x (cl-sort group #'> :key #'cdr))
                                     (push x result))
                                   (setq group nil)
                                   (push xref group))))
                               result))
    :candidate-number-limit 9999))

(defun helm-xref-show-xrefs (xrefs _alist)
  "Function to display XREFS.

Needs to be set the value of `xref-show-xrefs-function'."
  (setq helm-xref-alist nil)
  (helm-xref-candidates xrefs)
  (helm :sources (helm-xref-source)
        :truncate-lines t
        :buffer "*helm-xref*"))

(provide 'helm-xref)
;;; helm-xref.el ends here
