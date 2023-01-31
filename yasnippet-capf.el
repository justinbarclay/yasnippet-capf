;;; yasnippet-capf.el --- An interface for the parinfer-rust library -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2021  Justin Barclay

;; Author: Justin Barclay <github@justinbarclay.ca>
;; URL: https://github.com/justinbarclay/yasnippet-capf
;; Version: 0.0.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: capf

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

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
(defvar-local yas-capf--candidate-cache nil "List of all candidates for the current buffer")

(defun yas-annotate-candidates (tables)
  "Adds the YAS name, table-name, and snippet defintion as text properties to all candidates

 Properties:
 - `yas-mode' is the table-name the candidate was found in
 - `yas-template' is the snippet template for current candidate
 - `yas-name' is the snippet name for the current candidate"
  (setq-local yas-capf--candidate-cache
              (cl-mapcan (lambda (table)
                           (let ((table-name (yas--table-name table))
                                 (tablehash (yas--table-hash table))
                                 candidates)
                             (maphash (lambda (key namehash)
                                        (push (propertize key
                                                          'yas-mode table-name
                                                          'yas-template (car (hash-table-values namehash))
                                                          'yas-name (car (hash-table-keys namehash)))
                                              candidates))
                                      tablehash)
                             candidates))
                         tables)))

(defun yas-annotate-completion (&optional candidate)
  "Annotates current `candidate' with the snippets table name"
  (format "%s[yas]" (get-text-property 0 'yas-mode candidate)))

(defun yas-affixate-completion (candidates)
  "Annotates current `candidate' with the snippets table name"
  (mapcar (lambda (candidate)
            (list candidate
                  (kind-icon-formatted 'snippet)
                  (yas-annotate-completion candidate)))
          candidates))

(defun yas-exit-function (candidate status)
  (when (eq status 'finished)
    (yas-expand)))

(defun yas-help-buffer (&optional cand)
  "Displays the snippet's extended name and expanded body in a help buffer "
  (with-current-buffer (get-buffer-create "*yas-documentation*")
    (erase-buffer)
    (prog-mode)
    (funcall (intern-soft (get-text-property 0 'yas-mode cand)))
    (yas-minor-mode 1)
    (save-excursion
      (insert "Title: " (get-text-property 0 'yas-name cand) "\n")
      (insert "Snippet Body:\n")
      (yas-expand-snippet (yas--template-content (get-text-property 0 'yas-template cand))))
    (current-buffer)))

(defun yas-complete-snippet-at-point ()
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (list (car bounds)
            (cdr bounds)
            (or yas-capf--candidate-cache
                (yas-annotate-candidates (yas--get-snippet-tables)))
            :exclusive 'no
            :annotation-function 'yas-annotate-completion
            :affixation-function 'yas-affixate-completion
            :company-doc-buffer 'yas-help-buffer
            :exit-function 'yas-exit-function
            :category 'yasnippets))))

(define-minor-mode yas-capf-minor-mode
  "Adds completion-at-point support for yasnippets"
  :init-value nil
  :lighter "yas-capf"
  (add-to-list 'completion-at-point-functions #'yas-complete-snippet-at-point))

;;; yasnippet-capf.el ends here
