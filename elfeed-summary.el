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
           (list :tag "Group"
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
                  elfeed-summary-group))
           elfeed-summary-query)))

(defgroup elfeed-summary ()
  "Feed summary inteface for elfeed."
  :group 'elfeed)

(defcustom elfeed-summary-settings '(((:title . "All feeds")
                                      (:sort-fn . string-lessp)
                                      (:elements nil)))
  "Elfeed summary buffer settings."
  :group 'elfeed-summary
  :type 'elfeed-summary-group)

(cl-defun elfeed-summary--match-tag (query &key tags title url author title-meta)
  "Check if attributes of elfeed feed match QUERY.

QUERY is a form as described in TODO.

TAGS is a list of tags from `elfeed-feeds', TITLE, URL, AUTHOR
and TITLE-META are attributes of the `elfeed-db-feed'."
  (cond
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

QUERY is described in TODO."
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

(provide 'elfeed-summary)
;;; elfeed-summary.el ends here
