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
(require 'cl-lib)
(require 'elfeed)
(require 'elfeed-db)
(require 'elfeed-search)
(require 'magit-section)
(require 'seq)
(require 'widget)

;; XXX I want to have the compatibility with evil-mode without
;; requiring it, so I check whether this function is bound later in
;; the code.
(declare-function evil-define-key* "evil-core")


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

(define-widget 'elfeed-summary-setting-elements 'lazy
  "Type widget for `elfeed-summary-settings'"
  :offset 4
  :tag "Settings list"
  :type '(repeat
          (choice
           (cons :tag "Group"
                 (const group)
                 (repeat :tag "Group params"
                         (choice
                          (cons
                           (const :tag "Title" :title)
                           (string :tag "Title"))
                          (cons
                           (const :tag "Sort function" :sort-fn)
                           (choice
                            function
                            (const :tag "None" nil)))
                          (cons
                           (const :tag "Face" :face)
                           (face :tag "Face"))
                          (cons
                           (const :tag "Elements" :elements)
                           elfeed-summary-setting-elements))))
           (cons :tag "Query"
                 (const query)
                 elfeed-summary-query)
           (cons :tag "Search"
                 (const search)
                 (repeat :tag "Search params"
                         (choice
                          (cons :tag "Filter"
                                (const :tag "Filter" :filter)
                                (string :tag "Filter string"))
                          (cons :tag "Title"
                                (const :tag "Title" :title)
                                (string :tag "Filter title"))
                          (cons :tag "Tags"
                                (const :tag "Tags" :tags)
                                (repeat symbol)))))
           (const :tag "Misc feeds" :misc))))

(defgroup elfeed-summary ()
  "Feed summary inteface for elfeed."
  :group 'elfeed)

(defcustom elfeed-summary-settings
  '((group (:title . "All feeds")
           (:sort-fn . string-lessp)
           (:elements (query . :all)))
    (group (:title . "Searches")
           (:elements (search
                       (:filter . "@7-days-ago +unread")
                       (:title . "Unread entries this week"))
                      (search
                       (:filter . "@6-months-ago emacs")
                       (:title . "Something about Emacs")))))
  "Elfeed summary buffer settings.

This is a list of these possible items:
- Group `(group . <group-params>)'
  Groups are used to group elements under collapsible sections.
- Query `(query . <query-params>)'
  Query extracts a subset of elfeed feeds based on the given criteria.
  Each found feed will be represented as a line.
- Search `(search . <search-params>)'
  Elfeed search, as defined by `elfeed-search-set-filter'.
- a few special forms

`<group-params>' is an alist with the following keys:
- `:title' (mandatory)
- `:elements' (mandatory) - also a list of groups and queries
- `:sort-fn' - function used to sort titles of feeds, found by queries
  in `:elements'.  E.g. `string-greaterp' for alphabetical order.
- `:face' - group face.  The default face if `elfeed-summary-group-face'.

`<query-params>' can be:
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

Feed tags for query are taken from `elfeed-feeds'.

Query examples:
- `(emacs lisp)'
  Return all feeds that have either \"emacs\" or \"lisp\" tags.
- `(and emacs lisp)'
  Return all feeds that have both \"emacs\" and \"lisp\" tags.
- `(and (title . \"Emacs\") (not planets))'
  Return all feeds that have \"Emacs\" in their title and don't have
  the \"planets\" tag.

`<search-params>` is an alist with the following keys:
- `:filter' (mandatory) filter string, as defined by
  `elfeed-search-set-filter'
- `:title' (mandatory) title.
- `:tags' - list of tags to get the face of the entry.

Available special forms:
- `:misc' - print out feeds, not found by any query above."
  :group 'elfeed-summary
  :type 'elfeed-summary-setting-elements)

(defcustom elfeed-summary-look-back (* 60 60 24 180)
  "TODO"
  :group 'elfeed-summary
  :type 'integer)

(defcustom elfeed-summary-unread-tag 'unread
  "Unread tag"
  :group 'elfeed-summary
  :type 'symbol)

(defcustom elfeed-summary-feed-face-fn #'elfeed-summary--feed-face-fn
  "Function to get the face of the feed.

Accepts two arguments:
- The corresponding instance of `elfeed-feed'.
- List of tags from `elfeed-feeds'.

The default implementation, `elfeed-summary--feed-face-fn', calls
`elfeed-search--faces'."
  :group 'elfeed-summary
  :type 'function)

(defcustom elfeed-summary-search-face-fn #'elfeed-summary--search-face-fn
  "Function to get the face of the search.

Accepts two-arguments:
- `<search-params>', as described in `elfeed-summary-settings'.
- The number of found items.

The default implementation, `elfeed-summary--search-face-fn', calls
`elfeed-search--faces' with the contents of `:tags' of
`<search-params>' plus `unread' if the number of found items is
greater than zero."
  :group 'elfeed-summary
  :type 'function)

(defconst elfeed-summary-buffer "*elfeed-summary*"
  "Elfeed summary buffer name")

(defface elfeed-summary-group-face
  '((t (:inherit magit-section-heading)))
  "Default face for the elfeed-summary group."
  :group 'elfeed-summary)

;;; Logic
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
           else if (and (listp param) (eq (car param) 'query))
           append (elfeed-summary--get-feeds (cdr param))))

(defun elfeed-summary--feed-face-fn (_feed tags)
  (elfeed-search--faces tags))

(defun elfeed-summary--build-tree-feed (feed unread-count total-count)
  (let* ((unread (or (gethash (elfeed-feed-id feed) unread-count) 0))
         (tags (alist-get (elfeed-feed-id feed) elfeed-feeds
                          nil nil #'equal))
         (all-tags (if (< 0 unread)
                       (cons elfeed-summary-unread-tag tags)
                     tags)))
    `(feed . ((feed . ,feed)
              (unread . ,unread)
              (total . ,(or (gethash (elfeed-feed-id feed) total-count) 0))
              (faces . ,(funcall elfeed-summary-feed-face-fn feed all-tags))
              (tags . ,all-tags)))))

(defun elfeed-summary--search-face-fn (search count)
  (let ((tags (append
               (alist-get :tags search)
               (when (< 0 count)
                 '(unread)))))
    (elfeed-search--faces tags)))

(defun elfeed-summary--build-search (search)
  "TODO

Implented the same way as `elfeed-search--update-list'."
  (let* ((filter (elfeed-search-parse-filter (alist-get :filter search)))
         (head (list nil))
         (tail head)
         (count 0))
    (if elfeed-search-compile-filter
        ;; Force lexical bindings regardless of the current
        ;; buffer-local value. Lexical scope uses the faster
        ;; stack-ref opcode instead of the traditional varref opcode.
        (let ((lexical-binding t)
              (func (byte-compile (elfeed-search-compile-filter filter))))
          (with-elfeed-db-visit (entry feed)
            (when (funcall func entry feed count)
              (setf (cdr tail) (list entry)
                    tail (cdr tail)
                    count (1+ count)))))
      (with-elfeed-db-visit (entry feed)
        (when (elfeed-search-filter filter entry feed count)
          (setf (cdr tail) (list entry)
                tail (cdr tail)
                count (1+ count)))))
    `(search . ((params . ,(cdr search))
                (faces . ,(funcall elfeed-summary-search-face-fn (cdr search) count))
                (count . ,count)))))

(defun elfeed-summary--build-tree (params unread-count total-count misc-feeds)
  (cl-loop for param in params
           if (and (listp param) (eq (car param) 'group))
           collect `(group . ((params . ,(cdr param))
                              (face . ,(or (alist-get :face (cdr param))
                                           'elfeed-summary-group-face))
                              (children . ,(elfeed-summary--build-tree
                                            (cdr (assoc :elements (cdr param)))
                                            unread-count total-count misc-feeds))))
           else if (and (listp param) (eq (car param) 'search))
           collect (elfeed-summary--build-search param)
           else if (and (listp param) (eq (car param) 'query))
           append (cl-loop for feed in (elfeed-summary--get-feeds (cdr param))
                           collect (elfeed-summary--build-tree-feed
                                    feed unread-count total-count))
           else if (eq param :misc)
           append (cl-loop for feed in misc-feeds
                           collect (elfeed-summary--build-tree-feed
                                    feed unread-count total-count))
           else do (error "Can't parse: %s" (prin1-to-string param))))

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
    (elfeed-db-ensure)
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

;;; View
(defvar-local elfeed-summary--tree nil
  "TODO")

(defvar elfeed-summary-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "RET") #'widget-button-press)
    (define-key map (kbd "q") (lambda ()
                                (interactive)
                                (quit-window t)))
    (when (fboundp #'evil-define-key*)
      (evil-define-key* 'normal map
        (kbd "<tab>") #'magit-section-toggle
        "q" (lambda ()
              (interactive)
              (quit-window t))))
    map)
  "A keymap for `elfeed-summary-mode-map'.")

(define-derived-mode elfeed-summary-mode magit-section "Elfeed Summary"
  "TODO"
  :group 'org-journal-tags
  (setq-local buffer-read-only t))

(defclass elfeed-summary-group-section (magit-section)
  ((group :initform nil)))

(defun elfeed-summary--render-feed (data)
  (let* ((feed (alist-get 'feed data))
         (title (or (plist-get (elfeed-feed-meta feed) :title)
                    (elfeed-feed-title feed)
                    (elfeed-feed-id feed))))
    (insert (propertize title 'face (alist-get 'faces data)))
    (insert "\n")))

(defun elfeed-summary--render-search (data)
  (let ((search-data (alist-get 'params data)))
    (insert (propertize
             (alist-get :title search-data)
             'face
             (alist-get :face search-data)))
    (insert "\n")))

(defun elfeed-summary--render-group (data)
  (let ((group-data (alist-get 'params data)))
    (magit-insert-section group (elfeed-summary-group-section)
      (insert (propertize
               (alist-get :title group-data)
               'face
               (alist-get :face group-data)))
      (insert "\n")
      (magit-insert-heading)
      (oset group group data)
      (cl-loop for child in (alist-get 'children data)
               do (elfeed-summary--render-item child)))))

(defun elfeed-summary--render-item (item)
  (let ((data (cdr item)))
    (pcase (car item)
      ('group
       (elfeed-summary--render-group data))
      ('feed
       (elfeed-summary--render-feed data))
      ('search
       (elfeed-summary--render-search data))
      (_ (error "Unknown tree item: %s" (prin1-to-string (car item)))))))

(defun elfeed-summary--render (tree)
  "TODO"
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq-local elfeed-summary--tree tree)
    (unless (eq major-mode 'elfeed-summary-mode)
      (elfeed-summary-mode))
    (mapc #'elfeed-summary--render-item tree)))

(defun elfeed-summary ()
  "TODO"
  (interactive)
  (when-let ((buffer (get-buffer elfeed-summary-buffer)))
    (kill-buffer buffer))
  (let ((buffer (get-buffer-create elfeed-summary-buffer)))
    (with-current-buffer buffer
      (elfeed-summary--render
       (elfeed-summary--get-data)))
    (switch-to-buffer buffer)))

(provide 'elfeed-summary)
;;; elfeed-summary.el ends here
