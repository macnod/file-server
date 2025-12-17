(require :asdf)
(require :fiveam)
(require :cl-ppcre)
(require :uiop)
(require :rbac)
(require :dc-ds)
(require :dc-time)
(require :dc-eclectic)

(push (uiop:getcwd) asdf:*central-registry*)
(ql:register-local-projects)
(asdf:load-system :file-server)

(defpackage :file-server-test
  (:use :cl :fiveam :file-server)
  (:local-nicknames
    (:a :rbac)
    (:re :cl-ppcre)
    (:ds :dc-ds)
    (:dt :dc-time)
    (:u :dc-eclectic)))

(in-package :file-server-test)

(def-suite utilities-suite
  :description "FiveAM tests for file-server utilities")

(in-suite utilities-suite)

(test invert-hex-color
  (is (equal (invert-hex-color "#fff") "#000") "invert-hex-color #fff => #000")
  (is (equal (invert-hex-color "#101") "#EFE") "invert-hex-color #101 => #EFE"))

(test has-test
  (is-true (has '("one" "two" "three") "one"))
  (is-false (has '("one" "two" "three") "four"))
  (is-true (has '("one" "two" "three") '("one" "two")))
  (is-false (has '("one" "two" "three") '("three" "four"))))

(test has-some
  (is-true (has-some '("one" "two" "three") '("one" "two")))
  (is-true (has-some '("one" "two" "three") '("three")))
  (is-true (has-some '("one" "two" "three") '("three" "four")))
  (is-false (has-some '("one" "two" "three") '("four" "five")))
  (is-false (has-some nil '("four" "five")))
  (is-true (has-some '("one" "two" "three") nil)))

(test exclude
  (is (equal (exclude '("one" "two" "three") '("two" "four")) '("one" "three")))
  (is (equal (exclude '("one" "two" "three") nil) '("one" "two" "three")))
  (is-false (exclude nil '("two" "four")))
  (is (equal (exclude nil nil) nil))
  (is (equal (exclude '("one" "two" "three") '("one" "two" "three")) nil))
  (is (equal (exclude '("one" "two" "three" "two") '("two")) '("one" "three")))
  (is (equal (exclude '("one" "two" "three") '("four" "five"))
        '("one" "two" "three")))
  (is (equal (exclude '("one" "two" "three" "four" "five") '("two" "four"))
        '("one" "three" "five")))
  (is (equal (exclude '("one" "two" "three") "two") '("one" "three"))))

(test exclude-regex
  (is (equal (exclude-regex '("apple" "banana" "cherry" "date") "a.*e")
             '("banana" "cherry")))
  (is (equal (exclude-regex '("apple" "banana" "cherry" "date") "b.*a")
             '("apple" "cherry" "date")))
  (is (equal (exclude-regex '("apple" "banana" "cherry" "date") "z.*")
             '("apple" "banana" "cherry" "date")))
  (is (equal (exclude-regex nil "a.*e") nil))
  (is (equal (exclude-regex '("apple" "banana" "cherry" "date") nil)
             '("apple" "banana" "cherry" "date")))
  (is (equal (exclude-regex '("apple" "banana" "cherry" "date") "a" '("date"))
              '("cherry" "date"))))

(test exclusive-role-for
  (is (equal (exclusive-role-for "admin") "admin:exclusive"))
  (is (equal (exclusive-role-for "user-01") "user-01:exclusive")))

(test additional-text
  (is-false (additional-text 3 5 4))
  (is-false (additional-text 5 5 5))
  (is (equal (additional-text 20 10 200) "and 180 more"))
  (is (equal (additional-text 20 10 15) "and many more")))

(test readable-timestamp
  (let* ((ut (get-universal-time))
          (ts (dt:timestamp-string :universal-time ut))
          (readable (subseq (re:regex-replace "T" ts " ") 0 16)))
    (is (string= (file-server::readable-timestamp ut) readable))))

(test join-html
  (is (string=
        (join-html '("<li>Item 1</li>" "<li>Item 2</li>" "<li>Item 3</li>"))
        (format nil "~{~a~%~}"
          (list "<li>Item 1</li>" "<li>Item 2</li>" "<li>Item 3</li>"))))
  (is (string= (join-html nil) ""))
  (is (string=
        (join-html '("<ul>" ("<li>Item A</li>" "<li>Item B</li>") "</ul>"))
        (format nil "~{~a~%~}"
          (u:flatten
            '("<ul>" ("<li>Item A</li>" "<li>Item B</li>") "</ul>"))))))

(test html-list
  (is (string=
        (html-list '("Apple" "Banana" "Cherry"))
        (format nil "<ul class=html-list>
 <li>Apple
 <li>Banana
 <li>Cherry
</ul>")))
  (is (string=
        (html-list '("Apple" "Banana" "Cherry") :class "test")
        (format nil "<ul class=\"test html-list\">
 <li>Apple
 <li>Banana
 <li>Cherry
</ul>")))
  (is-false (html-list nil)))

(test add-to-url-query
  (is (string= (add-to-url-query "/api" "a" 1 "b" 2 "c" 3) "/api?a=1&b=2&c=3"))
  (is (string= (add-to-url-query "/api" "a" "hello & good-bye")
        "/api?a=hello%20%26%20good-bye"))
  (is (string= (add-to-url-query "/api?existing=1" "new" 2)
        "/api?existing=1&new=2"))
  (is (string= (add-to-url-query "/api?existing=1" "existing" 2)
        "/api?existing=1&existing=2")))


(test form-title
  (is (string= (form-title "Test Form")
        "<div class=form-title>
 <span class=form-title-text>Test Form</span>
</div>")))

(test form-text
  (is (string= (form-text "hello")
        "<div class=form-group>
 <div class=form-text>
  hello
 </div>
</div>")))

(test add-to-class
  (is (string= (add-to-class "class-1" "class-2") "class-1 class-2"))
  (is (string= (add-to-class "class-1 class-2" "class-3")
        "class-1 class-2 class-3"))
  (is (string= (add-to-class nil "class-1") "class-1"))
  (is (string= (add-to-class "class-1" nil) "class-1")))

(test label-to-name
  (is (string= (label-to-name "User Name") "user-name"))
  (is (string= (label-to-name "  Leading and Trailing  ") "leading-and-trailing"))
  (is (string= (label-to-name "Special!@#Chars$$%") "special-chars"))
  (is (string= (label-to-name "Mixed CASE Label") "mixed-case-label")))

(test name-to-id
  (is (re:scan "^abc-[0-9a-f]{4}$" (name-to-id "abc"))))

(test input-text
  (is (string= (input-text "name") "<div class=form-group>
 <label for=name>name</label>
 <input class=text-input type=text name=name
        autocomplete=new-password>
</div>")))

(test input-password
  (is (string= (input-password) "<div class=password-group><div class=form-group>
  <label for=password>Password: </label>
  <input class=\"password text-input\" type=password
         name=password autocomplete=new-password></div><div class=form-group>
  <label for=confirm-password>Confirm Password: </label>
  <input class=\"confirm-password text-input\"
         type=password name=confirm-password
         autocomplete=new-password></div>
</div>")))

(test input-hidden
  (is (string= (input-hidden "session-token" "abc123")
        "<input type=hidden name=session-token
       value=abc123>")))

(test input-checkbox
  (is (string= (input-checkbox "dark mode") "<div class=form-group>
 <label>
  <input class=input-checkbox type=checkbox
         name=dark-mode>dark mode</label>
</div>")))

(test input-checkbox-pre
  (is (string= (input-checkbox-pre "select")
"<div class=form-group>
 <label for=select>select</label>
 <input class=input-checkbox-pre type=checkbox
        name=select>
</div>")))

(test input-checkbox-list
  (is (string= (input-checkbox-list "colors" '("red" "green" "blue" "x")
               :checked '(t nil t nil)
               :disabled '(nil nil nil t))
"<div class=form-group>
 <label>colors</label>
 <div class=input-checkbox-list><label>
 <input type=checkbox name=colors checked
        value=red>red</label>
<label>
 <input type=checkbox name=colors value=green>green</label>
<label>
 <input type=checkbox name=colors checked
        value=blue>blue</label>
<label>
 <input type=checkbox name=colors disabled
        value=x>x</label>

 </div>
</div>")))

(test input-form
  (is (string= (input-form "login" "/do-login" "POST"
               (input-text "name")
               (input-password))
"<form class=\"standard-form login\"
      action=/do-login method=POST><div class=form-group>
 <label for=name>name</label>
 <input class=text-input type=text name=name
        autocomplete=new-password>
</div>
<div class=password-group><div class=form-group>
  <label for=password>Password: </label>
  <input class=\"password text-input\" type=password
         name=password autocomplete=new-password></div><div class=form-group>
  <label for=confirm-password>Confirm Password: </label>
  <input class=\"confirm-password text-input\"
         type=password name=confirm-password
         autocomplete=new-password></div>
</div>

</form>")))

(test input-form
  (is (string= (upload-form "upload" "/do-upload" "POST"
               (form-text "Select the file to upload")
               (input-file "file"))
"<form
      class=\"multipart-form standard-form upload\"
      action=/do-upload method=POST
      enctype=multipart/form-data><div class=form-group>
 <div class=form-text>
  Select the file to upload
 </div>
</div>
<div class=form-group>
 <label for=file>file</label>
 <input type=file name=file>
</div>

</form>")))

(test render-pager
  (is (string= (render-pager "/list" 2 20 189)
"<!-- Pager -->
<div class=pager>
 <span class=title>Page: </span><span class=pages><a class=page href=\"/list?page=1\">1</a><span class=page-separator> </span><a class=current-page href=\"/list?page=2\">2</a><span class=page-separator> </span><a class=page href=\"/list?page=3\">3</a><span class=page-separator> </span><a class=page href=\"/list?page=4\">4</a><span class=page-separator>...</span><a class=page href=\"/list?page=10\">10</a><span class=page-separator> </span></span>
</div>")))








(def-suite database-suite
  :description "FiveAM tests for file-server database interactions")

(in-suite database-suite)

(defun is-uuid (id)
  (when (re:scan
          "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"
          id)
    t))

(defun clear-document-root ()
  (let ((files-dir (file-server::absolute-directory-path "/")))
    (when (uiop:directory-exists-p files-dir)
      (uiop:delete-directory-tree files-dir :validate #'identity)
      (ensure-directories-exist files-dir))))

(defun clear-files ()
  (loop for resource in (file-server::resource-names)
    unless (string= resource "/")
    do (a:d-remove-resource file-server::*rbac* resource))
  (clear-document-root))

(file-server::init-database)

(test document-root
  (is-true (re:scan "/tests/files/$" file-server::*document-root*)
    "Expected document root to end with /tests/files/"))

(test db-directory-id
  (is-true (is-uuid (file-server::db-directory-id "/"))))

(test db-user-id
  (is-true (is-uuid (file-server::db-user-id
                      file-server::*admin*
                      file-server::*admin-password*))))

(test db-list-roles
  (is (file-server::db-list-roles "admin" t)
    '("admin" "admin:exclusive" "logged-in" "public")))

(test db-add-user
  (file-server::db-add-user "user-01" "password-01" "no-email" nil)
  (is-true (is-uuid (a:get-id file-server::*rbac* "users" "user-01")))
  (is (equal (file-server::db-list-roles "user-01" t)
        '("logged-in" "public" "user-01:exclusive")))
  (a:d-add-role file-server::*rbac* "role-a")
  (a:d-add-role file-server::*rbac* "role-b")
  (a:d-add-role-user file-server::*rbac* "role-a" "user-01")
  (is (equal (file-server::db-list-roles "user-01") '("role-a")))
  (file-server::db-add-user "user-02" "password-02" "user-02@sinistercode.com"
    '("role-a" "role-b"))
  (is (a:list-role-usernames file-server::*rbac* "role-a")
    '("user-01" "user-02"))
  (is (a:list-role-usernames file-server::*rbac* "role-b")
    '("user-02")))

(test db-add-resource
  (clear-files)
  (let ((new-resource "/one/"))
    (file-server::ensure-directories-exist
      (file-server::absolute-directory-path new-resource))
    (file-server::db-add-resource new-resource :roles '("role-a" "role-b"))
    (is-true (is-uuid (file-server::db-directory-id new-resource)))
    (is (file-server:has
          (a:list-resource-role-names file-server::*rbac* new-resource)
          '("role-a" "role-b")))
    (let ((dirs (file-server::fs-list-directories)))
      (is-true (file-server::has dirs new-resource)
        "Expected to find ~a in ~{~a~^, ~}" new-resource dirs))))

(test fs-list-directories
  (loop with new-resources = '("/two/" "/three/")
    for new-resource in new-resources
    do
    (file-server::ensure-directories-exist
      (file-server::absolute-directory-path new-resource))
    (file-server::db-add-resource new-resource
      :roles '("admin" "admin:exclusive" "public"))
    finally
    (let ((dirs (file-server::fs-list-directories)))
      (is-true (file-server::has dirs new-resources)
        "expected ~{~a~^, ~}; got ~{~a~^, ~}" new-resources dirs))))

(test hash-directory-list
  (is (file-server::hash-directory-list '("/one/" "/two/" "/three/"))
    (format nil "~{~a~}"
      (list
        "dcf0ab435d499ce5a782c7fb5ceed3b0b675a7761260d4642684f7e07c80850c"
        "52810a9bc407ae9570fa983ae372cdf0f2168de8eb41657a0ee7ac558bc40869"))))

(test sync-directories
  (clear-files)
  (loop with new-dirs = '("/four/" "/five/" "/six/")
    for new-dir in new-dirs
    do (file-server::ensure-directories-exist
         (file-server::absolute-directory-path new-dir))
      finally
      (file-server::sync-directories)
      (let ((resources (file-server::resource-names)))
        (is (equal resources (cons "/" (sort (copy-seq new-dirs) #'string<)))
          "expected ~{~a~^, ~}; got ~{~a~^, ~}" new-dirs resources))))

(test clean-path
  (clear-files)
  (loop with dirs = '("/one/" "/two/" "/three/")
    for dir in dirs
    for name-1 = (u:random-string 4 (u:ascii-alpha))
    for name-2 = (u:replace-extension
                   (u:random-string 4 (u:ascii-alpha)) ".txt")
    for file-1 = (file-server::absolute-file-path dir name-1)
    for file-2 = (file-server::absolute-file-path dir name-2)
    for rel-1 = (format nil "~a~a" dir name-1)
    for rel-2 = (format nil "~a~a" dir name-2)
    do
    (file-server::ensure-directories-exist
      (file-server::absolute-directory-path dir))
    (u:spew name-1 file-1)
    (u:spew name-2 file-2)
    collect rel-1 into relative-paths
    collect rel-2 into relative-paths
    finally
    (loop for file in relative-paths
      for expected = (re:regex-replace "/[^/]+$" file "/")
      for got = (file-server::clean-path file)
      do (is (string= expected got) "expected ~a; got ~a" expected got))
    (loop for expected in dirs
      for test-dir = (subseq expected 0 (1- (length expected)))
      for got = (file-server::clean-path test-dir)
      do (is (string= expected got) "expected ~a: got ~a" expected got))
    ;; The directory /four does not exist, so clean-path is going to think
    ;; it's a file and return / as the clean path to the file
    (is-false (string= (file-server::clean-path "/four") "/four/"))))

;;; Run tests
(unless (run-all-tests)
  (sb-ext:quit :unix-status 1))
