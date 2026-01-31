;;; ekg-org.el --- Letting ekg act as a source of data for org  -*- lexical-binding: t -*-

;; Copyright (c) 2026  Andrew Hyatt <ahyatt@gmail.com>

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/ekg
;; Keywords: outlines, hypermedia
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This module allows ekg to act as a source of tasks for org-mode.

(require 'org-element)
(require 'seq)
(require 'ekg)

;; TODO: this may not be necessary if we have schema support for states, we can
;; look up by the state which is a property.
(defconst ekg-org-state-tag "org/state/"
  "Prefix for EKG tags representing Org TODO states.")

;; TODO: this may not be necessary if we have schema support for tasks, we can
;; just look up by type.
(defconst ekg-org-task-tag "org/task"
  "Tag used to identify EKG notes that should be treated as Org tasks.")

(defconst ekg-org-archive-tag "org/archive"
  "Tag used to identify EKG notes that should be treated as archived Org tasks.")

;; TODO: once we have schema support we can look up by parent ID (or nil to get
;; the top-level items)
(defun ekg-org-get-tasks (&optional archive)
  "Fetch tasks from ekg and return as Org formatted string.

If ARCHIVE is non-nil, fetch archived tasks instead. If nil, fetch
active, unarchived, tasks."
  ;; Replace this with actual DB fetching logic
  (seq-filter
   (lambda (note)
     (and
      (plist-get (ekg-note-properties note) :titled/title)
      (not (and archive
                (not (member ekg-org-archive-tag (ekg-note-tags note)))))))
   (ekg-get-notes-with-parent-tag ekg-org-task-tag)))

(defun ekg-org-get-child-notes-of-id (id)
  "Fetch child notes of a given note ID."

  ;; TODO: will need actual schema support for this.
  nil
  )

(defun ekg-org-task-to-element (note parent)
  "Convert an EKG NOTE to an org-element node.

PARENT is the parent org-element node."
  (let* ((title (plist-get (ekg-note-properties note) :titled/title))
         (id (number-to-string (ekg-note-id note)))
         (state (upcase
                 ;; Use the suffix of the first org/state/ tag found
                 (string-replace ekg-org-state-tag ""
                                 (car (seq-filter
                                       (lambda (tag) (string-prefix-p ekg-org-state-tag tag))
                                       (ekg-note-tags note)))))))
    (let ((element (org-element-create
                    'headline
                    `(:level ,(+ 1 (or (org-element-property :level parent) 0))
                             :parent ,parent
                             :pre-blank 1
                             :title ,title
                             :tags ,(ekg-note-tags note)
                             :todo-keyword ,state)
                    (org-element-create
                     'node-property
                     `(:key "EKG_ID"
                            :value ,id)))))
      ;; Add contents of note
      (org-element-set-contents
       element
       ;; Let's add a link to the actual EKG entry if users want to navigate to it
       (concat
        (org-element-interpret-data
         (org-element-create
          'paragraph
          `(:parent ,element
                    :contents-begin 0
                    :contents-end 0
                    :post-blank 1)
          (format "EKG Entry: [[ekg-note:%d][View in EKG]]"
                  (ekg-note-id note))))
        (ekg-display-note-text note)))
      element)))

(defun ekg-org-task-to-string (note)
  "Turn NOTE and all its children into an org-mode string."
  (org-element-interpret-data
   (ekg-org-task-to-element note nil)))

(defun ekg-org-generate-org-content (&optional archive)
  "Generate Org formatted content from EKG tasks.

If ARCHIVE is nil, use active tasks, if non-nil, use archived tasks."
  ;; TODO: We just want the top-level tasks here
  (with-temp-buffer
    (let ((tasks (ekg-org-get-tasks archive)))
      (dolist (task tasks)
        (insert (ekg-org-task-to-string task)
                "\n"))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun ekg-org-fs-handler (operation &rest args)
  "Fake our ekg data as a file."
  (let ((filename (car args)))
    (cond
     ;; 1. Emacs asks: "Does this file exist?" -> YES
     ((eq operation 'file-exists-p) t)

     ;; 2. Emacs asks: "Is it readable?" -> YES
     ((eq operation 'file-readable-p) t)

     ;; 3. Emacs asks: "What is the real path?" -> Just return the input
     ((eq operation 'file-truename) filename)

     ;; 4. Emacs asks: "What are the attributes?" (Size, ModTime, etc) Org
     ;; Agenda checks this. We use the db's file, so when it updates, the agenda
     ;; will see that it has updated.
     ((eq operation 'file-attributes)
      ;; (t/nil nlinks uid gid atime mtime ctime size modes ...)
      (file-attributes ekg-db-file))

     ;; 5. Emacs asks: "Read the file!" -> WE GENERATE IT
     ((eq operation 'insert-file-contents)
      (let ((content (ekg-org-generate-org-content
                      (string-match ".*archive" filename))))
        (setq-local buffer-file-name filename)
        (insert content)
        ;; Return value must be (filename size)
        (list filename (length content))))

     ;; 6. Emacs asks: "Expand this path" (resolve ./ or ~/)
     ;; We just ensure the prefix stays intact.
     ((eq operation 'expand-file-name)
      (if (file-name-absolute-p filename)
          filename
        (concat "/ekg:" filename)))

     ;; 7. Catch-all: If we didn't handle it, fail gracefully or pass through.
     (t (let ((inhibit-file-name-handlers
               (cons 'ekg-org-fs-handler
                     (and (eq inhibit-file-name-operation operation)
                          inhibit-file-name-handlers)))
              (inhibit-file-name-operation operation))
          (apply operation args))))))

(add-to-list 'file-name-handler-alist '("\\`/ekg:" . ekg-org-fs-handler))

;; Add the fake file to Org Agenda
(add-to-list 'org-agenda-files "/ekg:tasks.org")

;; We need archive to open up as org, and it doesn't by default, which is odd.
;; But without this, we get an error.
(add-to-list 'auto-mode-alist '("\\.org_archive" . org-mode))

(provide 'ekg-org)
