;;; find-trace-paths.el --- Scan buffer for clickable trace paths 

;;; Copyright (C) 2020 Tatu Lahtela
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: Tatu Lahtela
;; Package-Requires:
;; URL: https://github.com/ration/find-trace-paths
;; Version: 0.0.1

;;; Commentary:


;;; Code:

(defcustom find-trace-paths-regex "[A-Za-z\.\/]+:[0-9]+\\(:[0-9]+\\)?"
  "REGEX to use for matching traces")

(defvar find-trace-paths--goto-map (make-sparse-keymap))
(define-key find-trace-paths--goto-map (kbd "<C-return>")  #'find-trace-paths--mark-and-goto)
(define-key find-trace-paths--goto-map (kbd "C-.")  #'find-trace-paths--search-backward)
(define-key find-trace-paths--goto-map (kbd "C-g")  #'find-trace-paths--abort)


(defun find-file-with-line (filename)
    "When FILENAME in format /path/foo.bar:3:4 find-file and go to line 3 column 4"
  (save-match-data
    (let* ((matched (string-match "(?\\(.*?\\):\\(.*?\\):\\(.*?\\))?$" filename))
           (line-number (and matched
                             (match-string 2 filename)
                             (string-to-number (match-string 2 filename))))
           (char-number (and matched
                             (match-string 3 filename)
                             (string-to-number (match-string 3 filename))))

           (file-path (if matched (match-string 1 filename) filename)))
      (find-file-other-window file-path)
      (when line-number
        (goto-char (point-min))
        (forward-line (1- line-number))
        (when char-number
          (move-to-column (- char-number 1)))))))

(defun find-trace-paths--mark-and-goto ()
    "Mark the current search string and exit the search."
    (interactive)
    (find-trace-paths-mode -1)
    (let ((selection (buffer-substring (point) (match-end 0))))
      (find-file-with-line selection)))

(defun find-trace-paths--abort ()
  (interactive)
  (find-trace-paths-mode -1)
  (keyboard-quit))

(defun find-trace-paths--search-backward ()
  (interactive)
  (search-backward-regexp find-trace-paths-regex))

(define-minor-mode find-trace-paths-mode
"Find errors that contain filenames with line numbers from buffer and scroll through them"
:lighter " Find-Error "
:keymap find-trace-paths--goto-map
(if find-trace-paths-mode (find-trace-paths--search-backward)
  (find-trace-paths--search-backward)))

(defun find-trace-paths ()
  (interactive)
  (if find-trace-paths-mode (find-trace-paths--abort)
    (find-trace-paths-mode)))

(provide 'find-trace-paths)
