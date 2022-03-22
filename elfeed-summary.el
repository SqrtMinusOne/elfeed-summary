;;; elfeed-summary.el --- Feed summary interface for elfeed -*- lexical-binding: t -*-

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
;; An extension for elfeed that provides a feed summary interface,
;; inspired by newsboat but tree-based.
;;
;; `elfeed-summary' pops up the summary buffer.  The buffer shows
;; individual feeds and searches, combined into groups.  This is
;; determined by the `elfeed-summary-settings' variable.
;;
;; Also take a look at the package README at
;; <https://github.com/SqrtMinusOne/elfeed-summary> for more
;; information.

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

;; XXX Compatibility with elfeed-org.  If this function is bound, then
;; `elfeed-org' is available
(declare-function rmh-elfeed-org-process "elfeed-org")

(define-widget 'elfeed-summary-query 'lazy
  "Type widget for the query part of `elfeed-summary-settings'."
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
                           (const :tag "Face" :face)
                           (face :tag "Face"))
                          (cons
                           (const :tag "Hide" :hide)
                           (boolean :tag "Hide"))
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
           (:elements (query . :all)))
    (group (:title . "Searches")
           (:elements (search
                       (:filter . "@7-days-ago +unread")
                       (:title . "Unread entries this week"))
                      (search
                       (:filter . "@6-months-ago emacs")
                       (:title . "Something about Emacs")))))
  "Elfeed summary settings.

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
  queries
  in `:elements'.  E.g. `string-greaterp' for alphabetical order.
- `:face' - group face.  The default face if `elfeed-summary-group-face'.
- `:hide' - if non-nil, collapse by default.

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
  "Timespan for which to count entries in the feed list.

The default value is 180 days, which means that only entries less than
180 days old will be counted.

This has to be set up for efficiency because the elfeed database is
time-based, so this allows queirying only the most recent part of the
database.

The value is in seconds."
  :group 'elfeed-summary
  :type 'integer)

(defcustom elfeed-summary-default-filter "@6-months-ago "
  "Default filter when switching to search buffer for a feed."
  :group 'elfeed-summary
  :type 'integer)

(defcustom elfeed-summary-unread-tag 'unread
  "Tag with which to consider the entry unread."
  :group 'elfeed-summary
  :type 'symbol)

(defcustom elfeed-summary-feed-face-fn #'elfeed-summary--feed-face-fn
  "Function to get the face of the feed entry.

Accepts two arguments:
- The corresponding instance of `elfeed-feed'.
- List of tags from `elfeed-feeds'.

The default implementation, `elfeed-summary--feed-face-fn', calls
`elfeed-search--faces'."
  :group 'elfeed-summary
  :type 'function)

(defcustom elfeed-summary-search-face-fn #'elfeed-summary--search-face-fn
  "Function to get the face of the search entry.

Accepts the following arguments:
- `<search-params>', as described in `elfeed-summary-settings'.
- The number of found unread items.
- The number of found items.

The default implementation, `elfeed-summary--search-face-fn', calls
`elfeed-search--faces' with the contents of `:tags' of
`<search-params>' plus `unread' if the number of found items is
greater than zero."
  :group 'elfeed-summary
  :type 'function)

(defcustom elfeed-summary-feed-sort-fn #'elfeed-summary--feed-sort-fn
  "Function to sort feeds in query.

Receives two instances of `elfeed-feed'."
  :group 'elfeed-summary
  :type 'function)

(defcustom elfeed-summary-refresh-on-each-update nil
  "Whether to refresh the elfeed summary buffer after each update.

This significantly slows down the `elfeed-update' command."
  :group 'elfeed-summary
  :type 'boolean)

(defcustom elfeed-summary-confirm-mark-read t
  "Whether to confirm marking the feed as read."
  :group 'elfeed-summary
  :type 'boolean)

(defconst elfeed-summary-buffer "*elfeed-summary*"
  "Elfeed summary buffer name.")

(defface elfeed-summary-group-face
  '((t (:inherit magit-section-heading)))
  "Default face for the elfeed summary group."
  :group 'elfeed-summary)

(defface elfeed-summary-count-face
  '((t (:inherit elfeed-search-title-face)))
  "Face for the number of entries of a read feed or search."
  :group 'elfeed-summary)

(defface elfeed-summary-count-face-unread
  '((t (:inherit elfeed-search-unread-title-face)))
  "Face for the number of entries of an unread feed or search."
  :group 'elfeed-summary)


;;; Logic
(cl-defun elfeed-summary--match-tag (query &key tags title url author title-meta)
  "Check if attributes of elfeed feed match QUERY.

QUERY is a `<query-params>' form as described in
`elfeed-summary-settings'.

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

(defun elfeed-summary--feed-sort-fn (feed-1 feed-2)
  "The default implementation of a feed sorting function.

FEED-1 and FEED-2 are instances of `elfeed-feed'."
  (string-lessp
   (downcase
    (or (plist-get (elfeed-feed-meta feed-1) :title)
        (elfeed-feed-title feed-1)
        (elfeed-feed-id feed-1)))
   (downcase
    (or (plist-get (elfeed-feed-meta feed-2) :title)
        (elfeed-feed-title feed-2)
        (elfeed-feed-id feed-2)))))

(defun elfeed-summary--get-feeds (query)
  "Get elfeed feeds that match QUERY.

QUERY is described in `elfeed-summary-settings'."
  (seq-sort
   elfeed-summary-feed-sort-fn
   (cl-loop for entry in elfeed-feeds
            for url = (car entry)
            for tags = (cdr entry)
            for feed = (elfeed-db-get-feed url)
            if (elfeed-summary--match-tag
                query
                :tags tags
                :title (elfeed-feed-title feed)
                :title-meta (plist-get (elfeed-feed-meta feed) :title)
                :url url
                :author (plist-get (car (elfeed-feed-author feed)) :name))
            collect feed)))

(defun elfeed-summary--feed-face-fn (_feed tags)
  "The default implementation of the feed face function.

FEED is an instance of `elfeed-feed', TAGS is a list of tags from
`elfeed-feeds'."
  (elfeed-search--faces tags))

(defun elfeed-summary--build-tree-feed (feed unread-count total-count)
  "Create a feed entry for the summary details tree.

FEED is an instance of `elfeed-feed'.  UNREAD-COUNT and TOTAL-COUNT
are hashmaps with feed ids as keys and corresponding numbers of
entries as values."
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

(defun elfeed-summary--search-face-fn (search unread _total)
  "The default implementation of the search entry face function.

SEARCH is a `<search-params>' form as described in
`elfeed-summary-settings'.

UNREAD is the number of unread entries, TOTAL is the total number of
entries."
  (let ((tags (append
               (alist-get :tags search)
               (when (< 0 unread)
                 '(unread)))))
    (elfeed-search--faces tags)))

(defun elfeed-summary--build-search (search)
  "Create a search entry for the summary details tree.

SEARCH is a `<search-params>' form as described in
`elfeed-summary-settings'.

Implented the same way as `elfeed-search--update-list'."
  (let* ((filter (elfeed-search-parse-filter (alist-get :filter search)))
         (head (list nil))
         (tail head)
         (unread 0)
         (total 0))
    (if elfeed-search-compile-filter
        ;; Force lexical bindings regardless of the current
        ;; buffer-local value. Lexical scope uses the faster
        ;; stack-ref opcode instead of the traditional varref opcode.
        (let ((lexical-binding t)
              (func (byte-compile (elfeed-search-compile-filter filter))))
          (with-elfeed-db-visit (entry feed)
            (when (funcall func entry feed total)
              (setf (cdr tail) (list entry)
                    tail (cdr tail)
                    total (1+ total))
              (when (member elfeed-summary-unread-tag (elfeed-entry-tags entry))
                (setq unread (1+ unread))))))
      (with-elfeed-db-visit (entry feed)
        (when (elfeed-search-filter filter entry feed total)
          (setf (cdr tail) (list entry)
                tail (cdr tail)
                total (1+ total))
          (when (member elfeed-summary-unread-tag (elfeed-entry-tags entry))
            (setq unread (1+ unread))))))
    `(search . ((params . ,(cdr search))
                (faces . ,(funcall elfeed-summary-search-face-fn
                                   (cdr search) unread total))
                (unread . ,unread)
                (total . ,total)))))

(defun elfeed-summary--build-tree (params unread-count total-count misc-feeds)
  "Recursively create the summary details tree.

PARAMS is a form as described in `elfeed-summary-settings'.

UNREAD-COUNT and TOTAL-COUNT are hashmaps with feed ids as keys and
corresponding numbers of entries as values.

MISC-FEEDS is a list of feeds that was not used in PARAMS.

The resulting form is described in `elfeed-summary--get-data'."
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

(defun elfeed-summary--extract-feeds (params)
  "Extract feeds from PARAMS.

PARAMS is a form as described in `elfeed-summary-settings'."
  (cl-loop for param in params
           if (and (listp param) (eq (car param) 'group))
           append (elfeed-summary--extract-feeds
                   (cdr (assoc :elements (cdr param))))
           else if (and (listp param) (eq (car param) 'query))
           append (elfeed-summary--get-feeds (cdr param))))

(defun elfeed-summary--ensure ()
  "Ensure that eleed database is loaded and feeds are set up."
  (elfeed-db-ensure)
  (when (and (not elfeed-feeds)
             (fboundp #'rmh-elfeed-org-process)
             ;; To shut up the byte compiler
             (boundp 'rmh-elfeed-org-files)
             (boundp 'rmh-elfeed-org-tree-id))
    (rmh-elfeed-org-process rmh-elfeed-org-files rmh-elfeed-org-tree-id)))

(defun elfeed-summary--get-data ()
  "Create the summary details tree from scratch.

The summary tree is created by extending `elfeed-summary-settings'
with the data from the elfeed database.

The return value is a list of alists of the following elements:
- `(group . <tree-group-params>)'
- `(feed . <feed-group-params>)'
- `(search . <search-group-params>)'

`<tree-group-params>' is an alist with the following keys:
- `params' - `<group-params>' as described in
  `elfeed-summary-settings'.
- `face' - face for the group.
- `children' - list of children, same structure as the root form.

`<feed-group-params>' is an alist with the following keys:
- `feed' - instance of `elfeed-feed'.
- `tags' - feed tags.
- `faces' - list of faces for the search entry.
- `unread' - number of unread entries in the feed.
- `total' - total number of entries in the feed.

`<search-group-params>' is an alist with the following keys:
- `params' - `<search-params>' as described in
  `elfeed-summary-settings'.
- `faces' - list of faces for the search entry.
- `unread' - number of unread entries in the search results.
- `total' - total number of entries in the search results."
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

;;; View
(defvar-local elfeed-summary--tree nil
  "The current value of the elfeed summary tree.")

(defvar elfeed-summary--unread-padding 3
  "Padding for the unread column in the elfeed summary buffer.")

(defvar elfeed-summary--total-padding 3
  "Padding for the total column in the elfeed summary buffer.")

(defvar elfeed-summary--only-unread nil
  "Only show items with unread entries in the elfeed summary buffer.")

(defvar elfeed-summary--search-show-read nil
  "Do not filter +unread when switching to the elfeed search buffer.")

(defvar elfeed-summary--search-mark-read nil
  "If t, mark the feed as read instead of switching to it.")

(defvar elfeed-summary-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "RET") #'elfeed-summary--action)
    (define-key map (kbd "M-RET") #'elfeed-summary--action-show-read)
    (define-key map (kbd "q") (lambda ()
                                (interactive)
                                (quit-window t)))
    (define-key map (kbd "r") #'elfeed-summary--refresh)
    (define-key map (kbd "R") #'elfeed-update)
    (define-key map (kbd "u") #'elfeed-summary-toggle-only-unread)
    (define-key map (kbd "U") #'elfeed-summary--action-mark-read)
    (when (fboundp #'evil-define-key*)
      (evil-define-key* 'normal map
        (kbd "<tab>") #'magit-section-toggle
        "r" #'elfeed-summary--refresh
        "R" #'elfeed-update
        "u" #'elfeed-summary-toggle-only-unread
        (kbd "RET") #'elfeed-summary--action
        "M-RET" #'elfeed-summary--action-show-read
        "U" #'elfeed-summary--action-mark-read
        "q" (lambda ()
              (interactive)
              (quit-window t))))
    map)
  "A keymap for `elfeed-summary-mode-map'.")

(define-derived-mode elfeed-summary-mode magit-section "Elfeed Summary"
  "A major mode to display the elfeed summary data."
  :group 'org-journal-tags
  (setq-local buffer-read-only t))

(defclass elfeed-summary-group-section (magit-section)
  ((group :initform nil)))

(defun elfeed-summary--widget-press-show-read (pos &optional event)
  "Press a button with `elfeed-summary--search-show-read' set to t.

POS and EVENT are forwarded to `widget-button-press'."
  (interactive "@d")
  (let ((elfeed-summary--search-show-read t))
    (widget-button-press pos event)))

(defun elfeed-summary--widget-press-mark-read (pos &optional event)
  "Press a button with `elfeed-summary--search-mark-read' set to t.

POS and EVENT are forwarded to `widget-button-press'."
  (interactive "@d")
  (let ((elfeed-summary--search-mark-read t))
    (widget-button-press pos event)))

(defun elfeed-summary--mark-read (feed)
  "Mark all items in the FEED as read.

FEED is an instance of `elfeed-feed'."
  (when (or (not elfeed-summary-confirm-mark-read)
            (y-or-n-p "Mark all entries in feed as read? "))
    (with-elfeed-db-visit (entry feed-2)
      (when (equal feed feed-2)
        (when (member elfeed-summary-unread-tag (elfeed-entry-tags entry))
          (setf (elfeed-entry-tags entry)
                (seq-filter (lambda (tag) (not (eq elfeed-summary-unread-tag tag)))
                            (elfeed-entry-tags entry))))))
    (elfeed-summary--refresh)))

(defun elfeed-summary--goto-feed (feed show-read)
  "Open the FEED in a elfeed search buffer.

FEED is an instance `elfeed-feed'.  If SHOW-READ is t, also show read
items."
  (elfeed)
  (elfeed-search-set-filter
   (concat
    elfeed-summary-default-filter
    (unless (or elfeed-summary--search-show-read
                show-read)
      "+unread ")
    "="
    (replace-regexp-in-string
     (rx "?" (* not-newline) eos)
     ""
     (elfeed-feed-url feed)))))

(defun elfeed-summary--search-feed-notify (widget &rest _)
  "A function to run in `:notify' in a feed widget button.

WIDGET is an instance of the pressed widget."
  (cond
   (elfeed-summary--search-mark-read
    (elfeed-summary--mark-read (list (widget-get widget :feed))))
   (_ (elfeed-summary--goto-feed
       (widget-get widget :feed) (widget-get widget :only-read)))))

(defun elfeed-summary--group-extract-feeds (group)
  "Extract feeds from GROUP.

GROUP is a `<tree-group-params>' as described in
`elfeed-summary--get-data'."
  (cl-loop for child in (alist-get 'children group)
           if (eq (car child) 'group)
           append (elfeed-summary--group-extract-feeds child)
           else if (eq (car child) 'feed)
           collect (alist-get 'feed (cdr child))))

(defun elfeed-summary--open-section (section)
  "Open section under cursor.

SECTION is an instance of `magit-section'."
  (let ((feeds (elfeed-summary--group-extract-feeds
                (oref section group))))
    (unless feeds
      (user-error "No feeds in section!"))
    (cond
     (elfeed-summary--search-mark-read
      (elfeed-summary--mark-read feeds))
     (t (progn
          (elfeed)
          (elfeed-search-set-filter
           (concat
            elfeed-summary-default-filter
            (unless elfeed-summary--search-show-read
              "+unread ")
            (mapconcat
             (lambda (feed)
               (format "=%s" (replace-regexp-in-string
                              (rx "?" (* not-newline) eos)
                              ""
                              (elfeed-feed-url feed))))
             feeds
             " "))))))))

(defun elfeed-summary--render-feed (data)
  "Render a feed item for the elfeed summary buffer.

DATA is a `<feed-group-params>' form as described in
`elfeed-summary--get-data'."
  (let* ((feed (alist-get 'feed data))
         (title (or (plist-get (elfeed-feed-meta feed) :title)
                    (elfeed-feed-title feed)
                    (elfeed-feed-id feed)))
         (text-format-string
          (concat "%" (number-to-string elfeed-summary--unread-padding)
                  "d / %-" (number-to-string elfeed-summary--total-padding)
                  "d "))
         (text (concat
                (propertize
                 (format text-format-string
                         (alist-get 'unread data) (alist-get 'total data))
                 'face (if (< 0 (alist-get 'unread data))
                           'elfeed-summary-count-face-unread
                         'elfeed-summary-count-face))
                (propertize title 'face (alist-get 'faces data)))))
    (widget-create 'push-button
                   :notify #'elfeed-summary--search-feed-notify
                   :feed feed
                   :only-read (= 0 (alist-get 'unread data))
                   text)
    (insert "\n")))

(defun elfeed-summary--render-search (data)
  "Render a search item for the elfeed summary buffer.

DATA is a `<search-group-params>' form as described in the
`elfeed-summary--get-data'."
  (let* ((search-data (alist-get 'params data))
         (text-format-string
          (concat "%" (number-to-string elfeed-summary--unread-padding)
                  "d / %-" (number-to-string elfeed-summary--total-padding)
                  "d "))
         (text (concat
                (propertize
                 (format text-format-string
                         (alist-get 'unread data) (alist-get 'total data))
                 'face
                 (if (< 0 (alist-get 'unread data))
                     'elfeed-summary-count-face-unread
                   'elfeed-summary-count-face))
                (propertize
                 (alist-get :title search-data)
                 'face
                 (alist-get 'faces data)))))
    (widget-create 'push-button
                   :notify (lambda (widget &rest _)
                             (elfeed)
                             (elfeed-search-set-filter
                              (widget-get widget :filter)))
                   :filter (alist-get :filter search-data)
                   text)
    (widget-insert "\n")))

(defun elfeed-summary--render-group (data)
  "Render a group item for the elfeed summary buffer.

DATA is a `<tree-group-params>' from as described in
`elfeed-summary-get-data'."
  (let ((group-data (alist-get 'params data)))
    (magit-insert-section group (elfeed-summary-group-section
                                 nil (alist-get :hide group-data))
      (insert (propertize
               (alist-get :title group-data)
               'face
               (alist-get 'face data)))
      (insert "\n")
      (magit-insert-heading)
      (oset group group data)
      (cl-loop for child in (alist-get 'children data)
               do (elfeed-summary--render-item child)))))

(defun elfeed-summary--render-item (item)
  "Render one item for the elfeed summary buffer.

ITEM is one alist as returned by `elfeed-summary--get-data'."
  (let ((data (cdr item)))
    (pcase (car item)
      ('group
       (elfeed-summary--render-group data))
      ('feed
       (elfeed-summary--render-feed data))
      ('search
       (elfeed-summary--render-search data))
      (_ (error "Unknown tree item: %s" (prin1-to-string (car item)))))))

(defun elfeed-summary--render-params (tree &optional max-unread max-total)
  "Get rendering parameters from the summary tree.

TREE is a form such as returned by `elfeed-summary--get-data'.

MAX-UNREAD and MAX-TOTAL are paramenters for the recursive descent."
  (unless max-unread
    (setq max-unread 0
          max-total 0))
  (cl-loop for item in tree
           for type = (car item)
           if (eq type 'group)
           do (let ((data (elfeed-summary--render-params
                           (alist-get 'children (cdr item))
                           max-unread max-total)))
                (setq max-unread (max max-unread (nth 0 data))
                      max-total (max max-total (nth 1 data))))
           else if (or (eq type 'feed) (eq type 'search))
           do (setq max-unread
                    (max max-unread (alist-get 'unread (cdr item)))
                    max-total
                    (max max-total (alist-get 'total (cdr item)))))
  (list max-unread max-total))

(defun elfeed-summary--leave-only-unread (tree)
  "Leave only items that have unread elfeed entries in them.

TREE is a form such as returned by `elfeed-summary--get-data'."
  (cl-loop for item in tree
           for type = (car item)
           if (and (eq type 'group)
                   (let ((children (elfeed-summary--leave-only-unread
                                    (alist-get 'children (cdr item)))))
                     (setf (alist-get 'children (cdr item))
                           children)
                     (< 0 (length children))))
           collect item
           else if (and (or (eq type 'feed) (eq type 'search))
                        (< 0 (alist-get 'unread (cdr item))))
           collect item))

(defun elfeed-summary--render (tree)
  "Render the elfeed summary tree.

TREE is a form such as returned by `elfeed-summary--get-data'."
  (when elfeed-summary--only-unread
    (setq tree (elfeed-summary--leave-only-unread tree)))
  (setq-local widget-push-button-prefix "")
  (setq-local widget-push-button-suffix "")
  (setq-local elfeed-search-filter-active t)
  (let* ((inhibit-read-only t)
         (render-data (elfeed-summary--render-params tree))
         (elfeed-summary--unread-padding
          (length (number-to-string (nth 0 render-data))))
         (elfeed-summary--total-padding
          (length (number-to-string (nth 1 render-data)))))
    (erase-buffer)
    (setq-local elfeed-summary--tree tree)
    (unless (eq major-mode 'elfeed-summary-mode)
      (elfeed-summary-mode))
    (insert (elfeed-search--header) "\n\n")
    (magit-insert-section _
      (magit-insert-heading)
      (unless tree
        (insert "No items found."))
      (mapc #'elfeed-summary--render-item tree))
    (widget-setup)))

(defun elfeed-summary--get-folding-state (&optional section folding-state parent-hidden)
  "Get the folding state of elfeed summary groups.

SECTION is an instance of `magit-section', FOLDING-STATE is a hash
map.  PARENT-HIDDEN shows whether the parent section is hidden.

If SECTION has the `group' slot, it is presumed to hold an instance of
`<tree-group-params>' as described in `elfeed-summary--get-data'.  The
resulting hash map will have `<group-params>' as keys and values of
the corresponding `hidden' slots as values."
  (unless section
    (setq section magit-root-section))
  (unless folding-state
    (setq folding-state (make-hash-table :test #'equal)))
  (when (and (slot-exists-p section 'group)
             (slot-boundp section 'group))
    (puthash (alist-get 'params (oref section group))
             (or parent-hidden (oref section hidden))
             folding-state))
  (cl-loop for child in (oref section children)
           do (elfeed-summary--get-folding-state
               child folding-state (oref section hidden)))
  folding-state)

(defun elfeed-summary--restore-folding-state (folding-state &optional section)
  "Restore the folding state of elfeed summary groups.

FOLDING-STATE is a hash map as returned by
`elfeed-summary--get-folding-state'.

SECTION is an instance of `magit-section', used for recursive
descent."
  (unless section
    (setq section magit-root-section))
  (when (and (slot-exists-p section 'group)
             (slot-boundp section 'group)
             (not (eq (gethash (alist-get 'params (oref section group) 'null)
                               folding-state)
                      'null)))
    (if (gethash (alist-get 'params (oref section group)) folding-state)
        (magit-section-hide section)
      (magit-section-show section)))
  (cl-loop for child in (oref section children)
           do (elfeed-summary--restore-folding-state folding-state child)))

(defun elfeed-summary--refresh ()
  "Refresh the elfeed summary tree."
  (interactive)
  (when (equal (buffer-name) elfeed-summary-buffer)
    ;; XXX this should've been `save-excursion, but somehow it doesn't
    ;; work.  And it is also necessary to preserve the folding state.
    (let ((inhibit-read-only t)
          (line (line-number-at-pos))
          (window-start (window-start))
          (folding-state (elfeed-summary--get-folding-state)))
      (erase-buffer)
      (elfeed-summary--render
       (elfeed-summary--get-data))
      (elfeed-summary--restore-folding-state folding-state)
      (goto-char (point-min))
      (forward-line (1- line))
      (set-window-start (selected-window) window-start))))

(defun elfeed-summary-toggle-only-unread ()
  "Toggle displaying only items with unread elfeed entries."
  (interactive)
  (setq-local elfeed-summary--only-unread
              (not elfeed-summary--only-unread))
  (elfeed-summary--refresh))

(defun elfeed-summary--on-feed-update (&rest _)
  "Message elfeed search header if the buffer is elfeed summary."
  (when elfeed-summary-refresh-on-each-update
    (elfeed-summary--refresh-if-exists)))

(defun elfeed-summary--refresh-if-exists ()
  "Refresh the elfeed summary buffer if it exists."
  (when-let (buffer (get-buffer elfeed-summary-buffer))
    (with-current-buffer buffer
      (elfeed-summary--refresh))))

(defun elfeed-summary ()
  "Display a feed summary for elfeed.

The buffer displays a list of feeds, as set up by the
`elfeed-summary-settings' variable."
  (interactive)
  (elfeed-summary--ensure)
  (add-hook 'elfeed-update-hooks #'elfeed-summary--on-feed-update)
  (add-hook 'elfeed-update-init-hooks #'elfeed-summary--refresh-if-exists)
  (when-let ((buffer (get-buffer elfeed-summary-buffer)))
    (kill-buffer buffer))
  (let ((buffer (get-buffer-create elfeed-summary-buffer)))
    (with-current-buffer buffer
      (elfeed-summary--render
       (elfeed-summary--get-data)))
    (switch-to-buffer buffer)
    (goto-char (point-min))))

(provide 'elfeed-summary)
;;; elfeed-summary.el ends here
