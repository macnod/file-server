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

(defun join-html (list &optional new-lines)
  (let ((format-string (if new-lines "~{~a~%~}" "~{~a~}")))
    (format nil format-string list)))

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

(defun input-hidden (name value)
  (s:with-html-string
    (:input :type "hidden" :name name :value value)))

(defun input-text (name label &optional required password)
  (s:with-html-string
    (:div :class "form-group"
      (:label :for name label)
      (:input :type (if password "password" "text")
        :id name 
        :class "textinput"
        :name name
        :required required))))

(defun input-checkbox (name display &key checked value disabled)
  (s:with-html-string
    (:div :class "checkbox"
      (:label
        (:input :type "checkbox" :name name :checked checked :value value
          :disabled disabled)
        display))))

(defun input-checkbox-list (name label values &key checked-states)
  (let ((checkboxes (loop with states = (or checked-states 
                                          (mapcar (constantly nil) values))
                      for value in values
                      for checked in states
                      for checkbox = (input-checkbox name value
                                       :checked checked :value value)
                      collect checkbox into html
                      finally (return (join-html html)))))
    (s:with-html-string
      (:div :class "form-group"
        (:label label)
        (:div :class "checkbox-group"
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
  (u:log-it-pairs :debug :detail "render-pager"
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
                                (error
                                  (u:log-it-pairs :error
                                    :details "CURRENT-PAGE is out of bounds"
                                    :current-page curent-page
                                    :first-page 1
                                    :last-page page-count)))
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

