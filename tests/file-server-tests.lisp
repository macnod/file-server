(require :asdf)
(require :fiveam)
(require :cl-ppcre)
(require :uiop)
(require :rbac)
(require :p-log)
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
    (:pl :p-log)
    (:u :dc-eclectic)))

(in-package :file-server-test)

(pl:make-log-stream "tests" file-server::*log-file*
  :append nil :severity-threshold :info)

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

(defun clear-users-and-roles ()
  (loop for user in (file-server::exclude
                      (a:list-usernames file-server::*rbac*)
                      (list "admin" "guest" "system"))
    do (a:d-remove-user file-server::*rbac* user))
  (loop for role in (file-server::exclude
                      (a:list-role-names file-server::*rbac*)
                      (list
                        "admin" "admin:exclusive"
                        "system" "system:exclusive"
                        "logged-in" "public" "guest:exclusive"))
    do (a:d-remove-role file-server::*rbac* role)))

(defun clear-everything ()
  (clear-files)
  (clear-users-and-roles))

(file-server::init-database)

(test logger-version
  (is (equal (file-server::logger-version) "0.1")
    "Expected file-server logger version to be 0.1")
  (is (equal (pl:pversion) "0.1")
    "Expected file-server-test logger version to be 0.1"))

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

(test db-add-directory
  (clear-everything)
  (let* ((dir "/one/")
          (dir-abs (file-server::absolute-directory-path dir))
          (roles '("role-01" "role-02"))
          (all-roles (u:safe-sort
                       (append
                         (list file-server::*admin-role*
                           file-server::*system-role*)
                         roles))))
    (loop for role in roles
      do (a:d-add-role file-server::*rbac* role))
    (is-true (file-server::db-add-directory dir :roles roles)
      "expected to successfully add directory ~a to database" dir)
    (is-true (u:directory-exists-p dir-abs)
      "expected directory ~a to exist in the file system" dir-abs)
    (is-true (is-uuid (file-server::db-directory-id dir))
      "expected directory ~a to have a database ID" dir)
    (is (equal all-roles (a:list-resource-role-names file-server::*rbac* dir))
      "expected directory ~a to have roles ~{~a~^, ~}; got ~{~a~^, ~}"
      dir all-roles (a:list-resource-role-names file-server::*rbac* dir))))

(test fs-list-directories
  (loop with new-resources = '("/two/" "/three/")
    for new-resource in new-resources
    do (file-server::db-add-directory new-resource
         :roles '("admin" "admin-:exclusive" "public"))
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
        (is (equal resources (cons "/" (u:safe-sort new-dirs)))
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

(test access
  (a:d-add-role file-server::*rbac* "role-c" :permissions '("read"))
  (a:d-add-role file-server::*rbac* "role-d")
  (file-server::db-add-user "user-03" "password-03" "no-email" '("role-c"))
  (file-server::db-add-user "user-04" "password-04" "no-email" '("role-d"))
  (file-server::db-add-resource "/seven/" :roles '("role-c"))
  (file-server::db-add-resource "/eight/" :roles '("role-d"))
  (is-true (file-server::has-read-access "user-03" "/seven/")
    "expected user-03 to have read access to /seven/")
  (is-false (file-server::has-update-access "user-03" "/seven/")
    "expected user-03 to lack update access to /seven/")
  (is-false (file-server::has-read-access "user-03" "/eight/")
    "expected user-03 to lack read access to /eight/")
  (is-false (file-server::has-update-access "user-03" "/eight")
    "expected user-03 to lack update access to /eight/")
  (is-false (file-server::has-read-access "user-04" "/seven/")
    "expected user-04 to lack read access to /seven/")
  (is-true (file-server::has-read-access "user-04" "/eight/")
    "expected user-04 to have read access to /eight/")
  (is-true (file-server::has-update-access "user-04" "/eight/")
    "expected user-04 to have update access to /eight/"))

(test list-files
  (loop
    with dir = "/one/"
    with dir-abs = (file-server::absolute-directory-path dir)
    initially
    (clear-files)
    (ensure-directories-exist dir-abs)
    for a from 1 to 3
    for name = (format nil "file-~d.txt" a)
    for rel-path = (u:join-paths dir name)
    for abs-path = (file-server::absolute-file-path dir name)
    do  (u:spew "bogus data" abs-path)
    collect rel-path into expected-files
    finally
    (let ((got-files (u:safe-sort (file-server::list-files dir-abs))))
      (is (equal expected-files got-files)
        "expected ~{~a~^, ~}; got ~{~a~^, ~}"
        expected-files got-files))))

(test rdl-subdirectories
  (let ((dir-1 "/one/")
         (dir-2 "/two/")
         (dir-3 "/one/a/")
         (dir-4 "/one/b/")
         (dir-5 "/two/c/")
         (dir-6 "/two/d/")
         (files '("/one/file-1.txt" "/one/a/file-2.txt" "/one/b/file-3.txt"
                   "/two/file-4.txt" "/two/c/file-5.txt" "/two/d/file-6.txt"))
         (user "user-05")
         (user-password "password-05")
         (role "role-e"))
    (clear-everything)
    (a:d-add-role file-server::*rbac* role :permissions '("read"))
    (file-server::db-add-user user user-password "no-email" (list role))
    ;; Add directories. Directories under /one/ have role-e; those under
    ;; /two/ have admin role. Since user-05 only has role-e, directories
    ;; under /two/ should not be visible to user-05.
    (loop for dir in (list dir-1 dir-2 dir-3 dir-4 dir-5 dir-6)
      for roles = (if (re:scan "^/one/" dir) (list role) (list "admin"))
      do
      (ensure-directories-exist (file-server::absolute-directory-path dir))
      (a:d-add-resource file-server::*rbac* dir :roles roles))
    ;; Put some files in the directories for good measure
    (loop for file in files
      for abs-path = (file-server::absolute-file-path
                       (file-server::clean-path file)
                       (u:filename-only file))
      do (u:spew (format nil "bogus data for file ~a" file) abs-path))
    ;; Now check what subdirectories user-05 can see
    (let ((got-1 (file-server::rdl-subdirectories
                   user
                   (file-server::absolute-directory-path dir-1)))
           (got-2 (file-server::rdl-subdirectories
                    user
                    (file-server::absolute-directory-path dir-2))))
      (is (equal (list dir-3 dir-4) got-1)
        "expected ~{~a,~^, ~}; got ~{~a~^, ~}" (list dir-3 dir-4) got-1)
      (is-false got-2
        "expected no subdirectories; got ~{~a~^, ~}" got-2))))

(test directory-roles
  (let ((dir "/one/")
         (roles-1 '("role-01"))
         (roles-2 (mapcar (lambda (n) (format nil "role-~2,'0d" n))
                    (u:range 2 4)))
         (roles-3 '("role-05"))
         (roles-4 (mapcar (lambda (n) (format nil "role-~2,'0d" n))
                    (u:range 6 15)))
         (roles-5 (mapcar (lambda (n) (format nil "role-~2,'0d" n))
                    (u:range 16 19)))
         (roles-6 (mapcar (lambda (n) (format nil "role-~2,'0d" n))
                    (u:range 20 30))))
    (clear-everything)
    (loop for role in (append roles-1 roles-2 roles-3 roles-4 roles-5 roles-6)
      do (a:d-add-role file-server::*rbac* role))
    (file-server::db-add-directory dir :roles roles-1)
    (is (equal roles-1 (file-server::directory-roles dir))
      "expected role-01; got ~{~a~^, ~}" (file-server::directory-roles dir))
    (loop for role in roles-2
      do (a:d-add-resource-role file-server::*rbac* dir role))
    (is (equal (append roles-1 roles-2)
          (sort (file-server::directory-roles dir) #'string<))
      "expected ~{~a~^, ~}; got ~{~a~^, ~}"
      (append roles-1 roles-2)
      (sort (file-server::directory-roles dir) #'string<))
    (loop for role in roles-3
      do (a:d-add-resource-role file-server::*rbac* dir role))
    (is (equal (append roles-1 roles-2 roles-3)
          (sort (file-server::directory-roles dir) #'string<))
      "expected ~{~a~^, ~}; got ~{~a~^, ~}"
      (append roles-1 roles-2 roles-3)
      (sort (file-server::directory-roles dir) #'string<))
    (loop for role in roles-4
      do (a:d-add-resource-role file-server::*rbac* dir role))
    (is (equal (cons "and 5 more" (append roles-1 roles-2 roles-3))
          (sort (file-server::directory-roles dir) #'string<))
      "expected ~{~a~^, ~}; got ~{~a~^, ~}"
      (cons "and 5 more" (append roles-1 roles-2 roles-3))
      (sort (file-server::directory-roles dir) #'string<))
    (loop for role in roles-5
      do (a:d-add-resource-role file-server::*rbac* dir role))
    (is (equal (cons "and 1 more" (append roles-1 roles-2 roles-3))
          (sort (file-server::directory-roles dir) #'string<))
      "expected ~{~a~^, ~}; got ~{~a~^, ~}"
      (cons "and 1 more" (append roles-1 roles-2 roles-3))
      (sort (file-server::directory-roles dir) #'string<))
    (loop for role in roles-6
      do (a:d-add-resource-role file-server::*rbac* dir role))
    (is (equal (cons "and many more" (append roles-1 roles-2 roles-3))
          (sort (file-server::directory-roles dir) #'string<))
      "expected ~{~a~^, ~}; got ~{~a~^, ~}"
      (cons "and many more" (append roles-1 roles-2 roles-3))
      (sort (file-server::directory-roles dir) #'string<))))

(test user-list-user-roles
  (clear-everything)
  (let ((user-1 "user-10")
         (user-2 "user-11")
         (user-3 "user-12"))
    (a:d-add-role file-server::*rbac* "role-1")
    (a:d-add-role file-server::*rbac* "role-2")
    (file-server::db-add-user user-1 "password-01" "no-email" '("role-1"))
    (file-server::db-add-user user-2 "password-02" "no-email" '("role-2"))
    (file-server::db-add-user user-3 "password-03" "no-email"
      '("role-1" "role-2"))
    (is (equal (file-server::user-list-user-roles "admin") '("admin"))
      "expected admin to have single role 'admin'")
    (is (equal (file-server::user-list-user-roles "guest") '("public"))
      "expected guest to have single role 'public'")
    (is (equal (file-server::user-list-user-roles user-1) '("role-1"))
      "expected user-01 to have single role 'role-1'")
    (is (equal (file-server::user-list-user-roles user-2) '("role-2"))
      "expected user-02 to have single role 'role-2")
    (is (equal (file-server::user-list-user-roles user-3) '("role-1" "role-2"))
      "expected user-03 to have roles 'role-1' and 'role-2'")))

;;; Run tests
(let ((test-results (run-all-tests)))
  (pl:close-log-stream "tests")
  (unless test-results
    (sb-ext:quit :unix-status 1)))
