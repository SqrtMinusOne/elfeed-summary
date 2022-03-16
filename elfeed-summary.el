;;; elfeed-summary.el --- TODO -*- lexical-binding: t -*-

;; Copyright (C) 2022 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (magit-section "3.3.0") (elfeed "3.4.1"))
;; Homepage: https://github.com/SqrtMinusOne/elfeed-summary.el

;; This file is NOT part of GNU Emacs.

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

;;; Commentary:
;; TODO

;;; Code:
(require 'elfeed)
(require 'elfeed-db)
(require 'widget)
(require 'seq)

(define-widget 'elfeed-summary-query 'lazy
  "A query to extract a subset of elfeed feeds."
  :offset 4
  :tag "Extract subset of elfeed feed list"
  :type '(choice (symbol :tag "One tag")
                 (const :tag "All" :all)
                 (cons :tag "Match title"
                       (const :tag "Title" title)
                       (choice (string :tag "String")
                               (sexp :tag "Lisp expression")))
                 (cons :tag "Match author"
                       (const :tag "Author" author)
                       (choice (string :tag "String")
                               (sexp :tag "Lisp expression")))
                 (cons :tag "Match URL"
                       (const :tag "URL" url)
                       (choice (string :tag "String")
                               (sexp :tag "Lisp expression")))
                 (cons :tag "AND"
                       (const :tag "AND" and)
                       (repeat elfeed-summary-query))
                 (cons :tag "NOT"
                       (const :tag "NOT" not)
                       elfeed-summary-query)
                 (repeat :tag "OR (Implicit)" elfeed-summary-query)
                 (cons :tag "OR"
                       (const :tag "OR" or)
                       (repeat elfeed-summary-query))))

(define-widget 'elfeed-summary-group 'lazy
  "A group of `elfeed-summary-query'"
  :offset 4
  :tag "Group"
  :type '(repeat
          (choice
           (cons :tag "Group"
                 (const group)
                 (list :tag "Group params"
                       (cons
                        (const :tag "Title" :title)
                        (string :tag "Title"))
                       (cons
                        (const :tag "Sort function" :sort-fn)
                        (choice
                         function
                         (const :tag "None" nil)))
                       (cons
                        (const :tag "Elements" :elements)
                        elfeed-summary-group)))
           elfeed-summary-query)))

(defgroup elfeed-summary ()
  "Feed summary inteface for elfeed."
  :group 'elfeed)

(defcustom elfeed-summary-settings '((group (:title . "All feeds")
                                            (:sort-fn . string-lessp)
                                            (:elements :all)))
  "Elfeed summary buffer settings.

This is a list of these possible items:
- group
- query
- a few special forms

Groups are used to group queries under collapsible sections.

A group is a cons cell like (group . <params>), where params are an
alist with the following attributes:
- `:title' (mandatory)
- `:elements' (mandatory) - also a list of groups and queries
- `:sort-fn' - function used to sort titles of feeds, found by queries
  in `:elements'.  E.g. `string-greaterp' for alphabetical order.

Query is a form that extract a subset of elfeed feeds based on
some criteria.  In the summary buffer, each feed found by the
query will be represented as a line.

Query can be:
- A symbol of a tag.
  A feed will be matched if it has that tag.
- `:all'.  Will match anything.
- `(title . \"string\")' or `(title . <form>)'
  Match feed title with `string-match-p'.  <form> makes sense if you
  want to pass something like `rx'.
- `(author . \"string\")' or `(author . <form>)'
- `(url . \"string\")' or `(url . <form>)'
- `(and <q-1> <q-2> ... <q-n>)'
  Match if all the conditions 1, 2, ..., n match.
- `(or <q-1> <q-2> ... <q-n>)' or `(<q-1> <q-2> ... <q-n>)'
  Match if any of the conditions 1, 2, ..., n match.
- `(not <query>)'

Feed tags are taken from `elfeed-feeds'.

Query examples:
- `(emacs lisp)'
  Return all feeds that have either \"emacs\" or \"lisp\" tags.
- `(and emacs lisp)'
  Return all feeds that have both \"emacs\" and \"lisp\" tags.
- `(and (title . \"Emacs\") (not planets))
  Return all feeds that have \"Emacs\" in their title and don't have
  the \"planets\" tag.

Available special forms:
- `:misc' - print out feeds, not found by any query above.
- `:unread' - a special feed of all unread entries."
  :group 'elfeed-summary
  :type 'elfeed-summary-group)

(defcustom elfeed-summary-look-back (* 60 60 24 180)
  "TODO"
  :group 'elfeed-summary
  :type 'integer)

(defcustom elfeed-summary-unread-tag 'unread
  "Unread tag"
  :group 'elfeed-summary
  :type 'symbol)

(defface elfeed-summary-group-face
  '((t (:inherit magit-section-heading)))
  "Default face for the elfeed-summary group."
  :group 'elfeed-summary)

(cl-defun elfeed-summary--match-tag (query &key tags title url author title-meta)
  "Check if attributes of elfeed feed match QUERY.

QUERY is a form as described in `elfeed-summary-settings'.

TAGS is a list of tags from `elfeed-feeds', TITLE, URL, AUTHOR
and TITLE-META are attributes of the `elfeed-db-feed'."
  (cond
   ;; `:all'
   ((equal query :all) t)
   ;; symbol
   ((symbolp query) (member query tags))
   ;; (title . "Title")
   ;; (title . (rx "Title"))
   ((eq (car query) 'title)
    (or (and title
             (string-match-p
              (if (stringp (cdr query))
                  (cdr query)
                (eval (cdr query)))
              title))
        (and title-meta
             (string-match-p
              (if (stringp (cdr query))
                  (cdr query)
                (eval (cdr query)))
              title-meta))))
   ;; (author . "Author")
   ;; (author . (rx "Author"))
   ((eq (car query) 'author)
    (and author
         (string-match-p
          (if (stringp (cdr query))
              (cdr query)
            (eval (cdr query)))
          author)))
   ;; (url . "URL")
   ;; (url . (rx "URL"))
   ((eq (car query) 'url)
    (and url
         (string-match-p
          (if (stringp (cdr query))
              (cdr query)
            (eval (cdr query)))
          url)))
   ;; (and <query-1> <query-2> ... <query-n>)
   ((eq (car query) 'and)
    (seq-every-p
     (lambda (query-elem)
       (elfeed-summary--match-tag
        query-elem
        :tags tags
        :title title
        :title-meta title-meta
        :url url
        :author author))
     (cdr query)))
   ;; (not <query>)
   ((eq (car query) 'not)
    (not
     (elfeed-summary--match-tag
      (cdr query)
      :tags tags
      :title title
      :title-meta title-meta
      :url url
      :author author)))
   ;; (or <query-1> <query-2> ... <query-n>)
   ;; (<query-1> <query-2> ... <query-n>)
   (t (seq-some
       (lambda (query-elem)
         (elfeed-summary--match-tag
          query-elem
          :tags tags
          :title title
          :title-meta title-meta
          :url url
          :author author))
       (if (eq (car query) 'or)
           (cdr query)
         query)))))

(defun elfeed-summary--get-feeds (query)
  "Get elfeed feeds that match QUERY.

QUERY is described in `elfeed-summary-settings'."
  (cl-loop for feed in elfeed-feeds
           for url = (car feed)
           for tags = (cdr feed)
           for feed = (elfeed-db-get-feed url)
           if (elfeed-summary--match-tag
               query
               :tags tags
               :title (elfeed-feed-title feed)
               :title-meta (plist-get (elfeed-feed-meta feed) :title)
               :url url
               :author (plist-get (car (elfeed-feed-author feed)) :name))
           collect feed))

(defun elfeed-summary--extract-feeds (params)
  (cl-loop for param in params
           if (and (listp param) (eq (car param) 'group))
           append (elfeed-summary--extract-feeds
                   (cdr (assoc :elements (cdr param))))
           else append (elfeed-summary--get-feeds param)))

(defun elfeed-summary--build-tree-feed (feed unread-count total-count)
  (let* ((unread (or (gethash (elfeed-feed-id feed) unread-count) 0))
         (tags (alist-get (elfeed-feed-id feed) elfeed-feeds
                          nil nil #'equal))
         (all-tags (if (< 0 unread)
                       (cons elfeed-summary-unread-tag tags)
                     tags)))
    `((feed . ,feed)
      (unread . ,unread)
      (total . ,(or (gethash (elfeed-feed-id feed) total-count) 0))
      (faces . ,(elfeed-search--faces all-tags))
      (tags . ,all-tags))))

(defun elfeed-summary--build-tree-unread (unread-count)
  (let ((unread (apply #'+ (maphash (lambda (k v) v) unread-count))))
    `((feed . ,(elfeed-feed--create
                :id unread
                :title "Unread"))
      (unread . ,unread)
      (total . ,unread))))

(defun elfeed-summary--build-tree (params unread-count total-count misc-feeds)
  (cl-loop for param in params
           if (and (listp param) (eq (car param) 'group))
           collect `(,param
                     (children . ,(elfeed-summary--build-tree
                                   (cdr (assoc :elements (cdr param)))
                                   unread-count total-count misc-feeds)))
           else if (eq param :misc)
           append (cl-loop for feed in misc-feeds
                           collect (elfeed-summary--build-tree-feed
                                    feed unread-count total-count))
           else if (eq param :unread)
           collect (elfeed-summary--build-tree-unread unread-count)
           else
           append (cl-loop for feed in (elfeed-summary--get-feeds param)
                           collect (elfeed-summary--build-tree-feed
                                    feed unread-count total-count))))

(defun elfeed-summary--get-data ()
  (let* ((feeds (elfeed-summary--extract-feeds
                 elfeed-summary-settings))
         (all-feeds (mapcar #'car elfeed-feeds))
         (misc-feeds
          (thread-last feeds
                       (mapcar #'elfeed-feed-id)
                       (seq-difference all-feeds)
                       (mapcar #'elfeed-db-get-feed)))
         (unread-count (make-hash-table :test #'equal))
         (total-count (make-hash-table :test #'equal)))
    (with-elfeed-db-visit (entry feed)
      (puthash (elfeed-feed-id feed)
               (1+ (or (gethash (elfeed-feed-id feed) total-count) 0))
               total-count)
      (when (member elfeed-summary-unread-tag (elfeed-entry-tags entry))
        (puthash (elfeed-feed-id feed)
                 (1+ (or (gethash (elfeed-feed-id feed) unread-count) 0))
                 unread-count))
      (when (> (- (time-convert nil 'integer)
                  elfeed-summary-look-back)
               (elfeed-entry-date entry))
        (elfeed-db-return)))
    (elfeed-summary--build-tree elfeed-summary-settings
                                unread-count total-count misc-feeds)))

(provide 'elfeed-summary)
;;; elfeed-summary.el ends here
