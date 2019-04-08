;;; package-requires.el --- Utilities for Package-Requires header -*- lexical-binding: t -*-

;; Copyright (C) 2019 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (dash "2.10"))
;; Keywords: languages tools
;; URL: https://github.com/akirak/package-requires.el

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a collection of utilities related to Package-Requires
;; header in Emacs Lisp files to help Emacs package developers create
;; new packages more efficiently.
;;
;; At present, it only contains commands to analyse dependencies of
;; Emacs Lisp files in a directory.

;;; Code:

(require 'dash)
(require 'cl-lib)

(autoload 'helm "helm")

(declare-function 'helm "helm")
(declare-function 'helm-build-sync-source "helm-source")
(declare-function 'helm-marked-candidates "helm")

(defconst package-requires-buffer "*elisp deps*")

(defvar package-requires-emacs-version nil)
(defvar package-requires-required-packages nil)

;;;###autoload
(defcustom package-requires-file-selector
  (cond
   ((featurep 'helm) 'helm)
   (t nil))
  "Completion backend used for selecting files,"
  :group 'package-requires
  :type '(or (const :tag "Default" nil)
             (const :tag "Helm" helm)))

(defconst package-requires-emacs-versions
  '("24.4" "24.5" "25.1" "25.2" "25.3" "26.1"
    "snapshot")
  "List of all recent Emacs versions in ascending order.")

(defun package-requires--read-header (&optional allow-empty)
  "Extract a list of dependencies from the library header."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (rx bol ";;" (* space) "Package-Requires:")
                           nil t)
        (let ((start (point))
              (end (progn (end-of-line) (point))))
          (car (read-from-string
                (buffer-substring-no-properties start end))))
      (unless allow-empty
        (user-error "No Package-Requires header in the buffer")))))

(defun package-requires--parse-files (files)
  "Parse \"Package-Requires\" header in FILES.

Returns a cons cell of the minimal Emacs version and a list of
names of dependencies packages."
  (-let (((xs ys)
          (->> (--map (with-current-buffer (or (find-buffer-visiting it)
                                               (find-file-noselect it))
                        (package-requires--read-header))
                      files)
               (-concat)
               (-flatten-n 1)
               (-separate (lambda (cell) (eq (car cell) 'emacs))))))
    (cons (-max-by (-compose #'not #'version<) (--map (nth 1 it) xs))
          (-distinct (-map (-compose #'symbol-name #'car) ys)))))

(defun package-requires--elisp-file-predicate (fname)
  "Return non-nil if FNAME is an elisp file."
  (and (equal ".el" (file-name-extension fname))
       (not (string-match-p "^\\."
                            (file-name-nondirectory fname)))))

(defun package-requires--glob-elisp-files (&optional dir)
  "Generate a list of elisp files in DIR."
  (directory-files (or dir default-directory) nil "^[^.]+\\.el\\'"))

(defun package-requires--select-files-helm (prompt)
  "Select files using helm.

PROMPT is used as the prompt."
  (helm :prompt prompt
        :sources
        (helm-build-sync-source (format "Files in %s" default-directory)
          :action (lambda (_) (helm-marked-candidates))
          :candidates (package-requires--glob-elisp-files))))

(defun package-requires--select-files-default (prompt)
  "Select files using `read-file-name'.

PROMPT is used as the prompt."
  (list (read-file-name prompt nil nil
                        nil nil
                        #'package-requires--elisp-file-predicate)))

(defun package-requires--select-files (prompt)
  "Select files using a preferred interface.

PROMPT is used as the prompt."
  (cl-case package-requires-file-selector
    ('helm (if (require 'helm nil t)
               (package-requires--select-files-helm prompt)
             (package-requires--select-files-default prompt)))
    (otherwise (package-requires--select-files-default prompt))))

;;;###autoload
(defun package-requires-show-file-deps (files)
  "Display the dependencies of FILES in a buffer."
  (interactive (list (package-requires--select-files "Elisp file: ")))
  (let* ((deps (--map (cons it
                            (package-requires--parse-files (list it)))
                      files)))
    (setq package-requires-emacs-version (-max-by (-compose #'not #'version<)
                                                  (--map (nth 1 it) deps))
          package-requires-packages (-distinct (apply '-concat (--map (-drop 2 it) deps))))
    (with-current-buffer (get-buffer-create package-requires-buffer)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert "Dependencies for elisp files in "
              (abbreviate-file-name default-directory)
              "\n\nEmacs version
-------------\n"
              (mapconcat (lambda (info)
                           (format "%-4s %s" (nth 1 info) (nth 0 info)))
                         deps "\n")
              "\n=> " package-requires-emacs-version
              "\n\nPackages
-------------\n"
              (string-join package-requires-packages " ")
              "\n")
      (setq buffer-read-only t)
      (local-set-key "q" #'quit-window))
    (display-buffer package-requires-buffer)))

;;;###autoload
(defun package-requires-redisplay ()
  "Display the last dependency information."
  (interactive)
  (if (get-buffer package-requires-buffer)
      (display-buffer package-requires-buffer)
    (message "No previous result is available. Creating a new one for the directory")
    (package-requires-show-dir-deps)))

;;;###autoload
(defun package-requires-show-dir-deps ()
  "Display the dependencies of all files in the directory."
  (interactive)
  (package-requires-show-file-deps (package-requires--glob-elisp-files)))

;;;###autoload
(defun package-requires-emacs-version ()
  "Return the required Emacs version.

Intended for use in a snippet."
  (or package-requires-emacs-version
      (car (package-requires--parse-files
            (package-requires--glob-elisp-files)))))

;;;###autoload
(defun package-requires-format-required-packages ()
  "Return a list of required packages separated by space.

Intended for use in a snippet."
  (string-join (or package-requires-required-packages
                   (cdr (package-requires--parse-files
                         (package-requires--select-files
                          "Select elisp files: "))))
               " "))

;;;###autoload
(defun package-requires-travis-matrix ()
  "Produce a string to be used in env section of .travis.yml.

Intended for use in a snippet."
  (let ((versions (cl-member (package-requires-emacs-version)
                             package-requires-emacs-versions
                             :test #'string-equal)))
    (concat "  matrix:\n"
            (mapconcat (lambda (ver) (format "  - EMACS_VERSION=%s" ver))
                       versions
                       "\n"))))

(provide 'package-requires)
;;; package-requires.el ends here
