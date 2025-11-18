(in-package :file-server)

;;
;; BEGIN JWT Token
;;

(defun issue-jwt (user-id &optional (expiration-seconds 3600))
  "Issue a JWT for a user."
  (let* ((claims `(("sub" . ,user-id)
                    ("exp" . ,(+ (u:universal-time-to-unix-time)
                                expiration-seconds)))))
    (j:encode :hs256 *jwt-secret* claims)))

(defun validate-jwt (token)
  "Validate a JWT token. Returns username if JWT token validates and user
exists. Otherwise, logs a message and returns NIL."
  (handler-case
    (multiple-value-bind (claims headers sig)
      (jose:decode :hs256 *jwt-secret* token)
      (declare (ignore headers sig))
      (when claims
        (let ((user-id (cdr (assoc "sub" claims :test #'string=))))
          (if user-id
            (let ((user (handler-case
                          (a:get-value *rbac* "users" "username"
                            "id" user-id)
                          (error (e)
                            (u:log-it
                              :warn
                              "JWT has invalid user ID: ~a" e)
                            nil))))
              (if user
                user
                (progn
                  (u:log-it :warn "User with ID ~a not found" user-id)
                  nil)))
            (progn
              (u:log-it :warn "User ID not found in JWT")
              nil)))))
    (error (e)
      (u:log-it :warn "Invalid JWT: ~a" e)
      nil)))

;;
;; END JWT Token
;;

;;
;; BEGIN Misc
;;

(defun invert-hex-color (hex-string)
  "Return the inverse color of the color represented by HEX-STRING. HEX-STRING
is a 3-digit or 6-digit hexadecimal value that is optionally prefixed by the
# symbol. Examples of valid values for HEX-STRING: #00FF33, #FFF, AA77CE, 123.
This function returns the inverted value using the same number of hexadecimal
digits provided in HEX-STRING, but always prefixes the return value with the
# symbol. If HEX-STRING is invalid, this function raises an error."
  (let* ((color (string-trim "# " hex-string))
          (short (= (length color) 3))
          (full-color (if short
                        (loop for char across color
                          append (list char char) into full
                          finally (return (map 'string 'identity full)))
                        color))
          (components (loop for a from 0 to 4 by 2
                        for b = (+ a 2)
                        collect
                        (parse-integer (subseq full-color a b) :radix 16)))
          (inverse-components (loop for component in components
                                collect (- 255 component)))
          (hex-components (loop for value in inverse-components
                            collect (format nil "~2,'0x" value)))
          (final-components (if short
                              (mapcar
                                (lambda (c) (subseq c 0 1))
                                hex-components)
                              hex-components)))
    (format nil "#~{~a~}" final-components)))

(defun alist-to-hash-table (alist)
  "Converts a ALIST, which is a Common Lisp a-list, into a hash table, using
ALIST keys as the hash table keys and ALIST values as the hash table
values. When ALIST contains a repeated key, the hash table value associated with
the repeated key becomes an array of the ALIST values."
  (loop with h = (make-hash-table :test 'equal)
    for (key . value) in alist
    for existing = (gethash key h)
    when (null existing) do
    (let ((array (setf (gethash key h)
                   (make-array 100 :adjustable t :fill-pointer 0))))
      (vector-push-extend value array))
    else do
    (vector-push-extend value existing)
    finally (return h)))

(defgeneric has (reference-list thing)
  (:documentation "Returns T if REFERENCE-LIST contains THING. If THING is a
string, this function checks for that string in REFERENCE-LIST. If THING is a
list, this function checks that all elements of THING are in REFERENCE-LIST.")
  (:method ((reference-list list) (thing string))
    (when (member thing reference-list :test 'equal) t))
  (:method ((reference-list list) (thing list))
    (when
      (every (lambda (s) (member s reference-list :test 'equal)) thing)
      t)))

(defun has-some (reference-list query-list)
  "Returns T if REFERENCE-LIST contains any of the elements in QUERY-LIST."
  (when
    (some (lambda (s) (member s reference-list :test 'equal)) query-list)
    t))

(defgeneric exclude (reference-list exclude)
  (:documentation "Returns a list containing the elements of REFERENCE-LIST
that are not in EXCLUDE. If EXCLUDE is a list, this function excludes all
elements in EXCLUDE from REFERENCE-LIST. If EXCLUDE is a string, this function
excludes the string from REFERENCE-LIST")
  (:method ((reference-list list) (exclude string))
    "Returns a list containing the elements of REFERENCE-LIST excluding
the one that is equal to EXCLUDE."
    (remove-if (lambda (s) (equal s exclude)) reference-list))
  (:method ((reference-list list) (exclude list))
    "Returns a list containing the elements of REFERENCE-LIST excluding
all elements in EXCLUDE."
    (remove-if
      (lambda (s) (member s exclude :test 'equal))
      reference-list)))

(defun exclude-regex (reference-list exclude)
  "Returns a list of the elements of REFERENCE-LIST that don't match the
EXCLUDE regular expression."
  (remove-if (lambda (s) (re:scan exclude s)) reference-list))

(defun exclude-regex (reference-list exclude &optional exceptions)
  "Returns a list of the elements of REFERENCE-LIST that that don't match the
EXCLUDE regular expression. However, elements that match EXCEPTIONS are not
excluded, even if they match EXCLUDE."
  (remove-if (lambda (s)
               (and
                 (not (member s exceptions :test 'equal))
                 (re:scan exclude s)))
    reference-list))

(defgeneric exclude-except (reference-list exclude exceptions)
  (:documentation "Returns a list containing the elements of REFERENCE-LIST
that don't match the EXCLUDE regular expression and that don't match
EXCEPTIONS.")
  (:method ((reference-list list) (exclude string) (exceptions list))
    "Returns a list containing the elements of REFERENCE-LIST that don't match
EXCLUDE and are not among EXCEPTIONS."
    (remove-if
      (lambda (s)
        (and (re:scan exclude s)
          (not (member s exceptions :test 'equal))))
      reference-list)))

(defun exclusive-role-for (username)
  "Returns the exclusive role for USERNAME."
  (format nil "~a:exclusive" username))

(defun additional-text (count-actual count-listed count-total)
  "Returns a string indicating, in fuzzy terms, how many additional items, beyond
COUNT-LISTED, exist and are not being shown. COUNT-ACTUAL is the actual number
of items being considered. COUNT-TOTAL is the maximum number of items taht will
be pulled from the data source. COUNT-LISTED is the maximum number of items that
can be shown. If COUNT-ACTUAL is less than or equal to COUNT-LISTED, this
function returns an empty string, because no notice of additional items is
needed. If COUNT-ACTUAL is greater than COUNT-LISTED, but less than COUNT-TOTAL,
this function returns a string indicating the number of items that are not being
shown. If COUNT-ACTUAL is equal or greater than COUNT-TOTAL, this function
returns a string indicating that there are many more items not being shown,
without specifying how many."
  (cond
    ((<= count-actual count-listed) nil)
    ((< count-actual count-total)
      (format nil "and ~d more"
        (- count-total count-actual)))
    (t "and many more")))

;;
;; END Misc
;;

;;
;; BEGIN Web/HTML
;;

(defun readable-timestamp (universal-time)
  "Return UNIVERSAL-TIME formatted as a timestamp that reads like this:
YYYY-MM-DD HH:MM. If UNIVERSAL-TIME is NIL or :NULL, this function returns an
empty string."
  (if (and universal-time (not (eql universal-time :null)))
    (let ((ts (u:timestamp-string :universal-time universal-time)))
      (subseq (re:regex-replace "T" ts " ") 0 16))
    ""))

(defun join-html (list &optional (new-lines t))
  (let ((format-string (if new-lines "~{~a~%~}" "~{~a~}"))
         (list-no-nulls (remove-if-not #'identity list)))
    (format nil format-string list-no-nulls)))

(defun add-to-url-query (path &rest pairs)
  (when path
    (loop
      for key in pairs by #'cddr
      for value in (cdr pairs) by #'cddr
      for beg = path then url
      for url = (if (and key value)
                  (let ((format-string (cond
                                         ((re:scan "[?]$" beg) "~a~(~a~)=~a")
                                         ((re:scan "[?]" beg) "~a&~(~a~)=~a")
                                         (t "~a?~(~a~)=~a"))))
                    (format nil format-string
                      beg key
                      (h:url-encode (format nil "~a" value))))
                  (or beg url))
      finally (return url))))

(defun form-text (text &key (class "form-text"))
  (s:with-html-string
    (:div :class "form-group"
      (:span :class class text))))

(defun add-to-class (existing-class new-class)
  (cond
    ((and existing-class new-class)
      (format nil "~a ~a" existing-class new-class))
    (existing-class existing-class)
    (new-class new-class)
    (t nil)))

(defun label-to-name (label)
  "Convert LABEL to a name suitable for use as an HTML form element name or ID."
  (u:trim
    (re:regex-replace-all "[^a-z0-9]+" (format nil "~(~a~)" label)  "-")
    "^-|-$"))

(defun name-to-id (name)
  "Convert NAME to an ID suitable for use as an HTML form element ID."
  (format nil "~a-~a" name (u:random-hex-number 4)))

(defun input-text (label &key
                    (name (label-to-name label))
                    required
                    is-password
                    placeholder
                    class)
  (s:with-html-string
    (:div :class "form-group"
      (:label :for name label)
      (:input :type (if is-password "password" "text")
        :id (name-to-id name)
        :name name
        :class (add-to-class class "text-input")
        :required required
        :placeholder placeholder))))

(defun input-password (&key required class)
  (s:with-html-string
    (:div :class "password-group"
      (:raw (input-text "Password: "
              :required required
              :is-password t
              :class (add-to-class "password" class)))
      (:raw (input-text "Confirm Password: "
              :required required
              :is-password t
              :class (add-to-class "confirm-password" class))))))

(defun input-hidden (name value)
  (s:with-html-string
    (:input :type "hidden" :name name :value value)))

(defun input-checkbox (label &key
                        (name (label-to-name label))
                        checked
                        value
                        disabled
                        class)
  (s:with-html-string
    (:div :class "form-group"
      (:label
        (:input :type "checkbox" :name name :checked checked :value value
          :class (add-to-class class "input-checkbox")
          :disabled disabled)
        label))))

(defun input-checkbox-list (label values &key
                             class
                             (name (label-to-name label))
                             (checked (mapcar (constantly nil) values))
                             (disabled (mapcar (constantly nil) values)))
  (unless (= (length checked) (length values))
    (error "Length of CHECKED list must equal length of VALUES list"))
  (unless (= (length disabled) (length values))
    (error "Length of DISABLED list must equal length of VALUES list"))
  (let ((checkboxes (loop
                      for value in values
                      for check in checked
                      for disable in disabled
                      for checkbox = (s:with-html-string
                                       (:label
                                         (:input :type "checkbox"
                                           :name name
                                           :checked check
                                           :disabled disable
                                           :value value)
                                         value))
                      collect checkbox into html
                      finally (return (join-html html)))))
    (s:with-html-string
      (:div :class "form-group"
        (:label label)
        (:div :class (add-to-class class "input-checkbox-list")
          (:raw checkboxes))))))

(defun input-form (id class action method &rest fields)
  (s:with-html-string
    (:form :id id :class class :action action :method method
      (:raw (join-html fields)))))

(defun input-submit-button (display &key name value (class "submit-button"))
  (s:with-html-string
    (:div :class "button-container"
      (:button :type "submit"
        :class class
        :name name
        :value value
        display))))

(defun render-pager (url current-page page-size element-count
                      &optional (link-count 5))
  (u:log-it-pairs :debug :in "render-pager"
    :url url
    :current-page current-page
    :page-size page-size
    :element-count element-count
    :link-count link-count)
  (loop
    with page-count = (ceiling element-count page-size)
    with link-count-half = (floor link-count 2)
    with first-page = (max 1 (- current-page link-count-half))
    with last-page = (min page-count (+ current-page link-count-half))
    with page-1 = (when (> first-page 1) 1)
    with page-n = (when (< last-page page-count) page-count)
    with check-current-page = (when (or
                                      (< current-page 1)
                                      (> current-page page-count))
                                (u:log-it-pairs :error
                                  :in "render-pager"
                                  :status "current-page is out of bounds"
                                  :current-page current-page
                                  :first-page 1
                                  :last-page page-count)
                                (setf current-page first-page))
    with pages = (remove-if-not
                   #'identity
                   (append
                     (list page-1)
                     (loop for page from first-page to last-page collect page)
                     (list page-n)))
    for page in pages
    for next-page in (cdr (append pages (list 0)))
    for index from 1 to (length pages)
    collect (s:with-html-string
              (:a :class (if (= page current-page) "current-page" "page")
                :href (add-to-url-query url "page" page)
                (format nil "~d" page))
              (:span :class "page-separator"
                (if (> next-page (1+ page)) "..." " ")))
    into pager
    finally (return
              (if (> (length pages) 1)
                (s:with-html-string
                  (:comment "Pager")
                  (:div :class "pager"
                    (:span :class "title" "Page: ")
                    (:span :class "pages" (:raw (format nil "~{~a~}" pager)))))
                ""))))

;;
;; END WEb/HTML
;;
