(in-package :file-server)

(defun maybe-invert (color user)
  (if (user-setting user "dark mode")
    (invert-hex-color color)
    color))

(defun generate-css (user)
  (let ((page-background-color (maybe-invert "#fff" user))
         (navbar-background-color (maybe-invert "#eee" user))
         (table-stripe-color (maybe-invert "#f2f2f2" user))
         (navbar-font-family "mono")
         (nav-menu-color (maybe-invert "#222" user))
         (nav-menu-after-color (maybe-invert "#ddd" user))
         (nav-menu-hover-color (maybe-invert "#0c0" user))
         (nav-menu-active-color (maybe-invert "#fff" user))
         (font-color (maybe-invert "#000" user))
         (cancel-button-color (maybe-invert "#f00" user))
         (confirm-button-color (maybe-invert "#0c0" user))
         (link-color (maybe-invert "#06c" user))
         (link-visited-color (maybe-invert "#551a8b" user))
         (link-hover-color (maybe-invert "#05a" user))
         (link-active-color (maybe-invert "#f00" user)))
    (l:compile-and-write
      `(body :background-color ,page-background-color
         (.main-page
           :color ,font-color
           :background-color ,page-background-color
           :max-width "60rem"
           :margin "0 auto"
           :padding "1rem"
           (a :color ,link-color)
           ("a:visited" :color ,link-visited-color)
           ("a:hover" 
             :color ,link-hover-color
             :font-size "1.1rem")
           ("a:active" :color ,link-active-color)
           (input :color ,font-color :background-color ,page-background-color)
           (.title :font-family "mono" :font-size "2rem" :text-align "center")
           (.listing
             :list-style-type none
             :display "block"
             :width "100%"
             (li
               :display "flex"
               :align-items "center"
               :width "100%"
               (a :display "flex" :align-items "center"
                 (img :margin-right "8px"))
               (span
                 :display "inline"
                 :margin-left "10px"
                 :font-size "small"
                 :white-space "nowrap")
               (.edit-roles-link
                 :margin-left "10px")))
           (.breadcrumb
             :display "flex"
             :align-items "center"
             :gap "8px"
             (img :vertical-align "middle")
             (div :display "inline" :font-size "24px"))
           (.access-list :margin-left "32px" :display "flex"
             (img :vertical-align "middle")
             (span :margin-left "4px" :font-size "14px" :margin-top "-2px"))

           (.navbar
             :background-color ,navbar-background-color
             :font-family ,navbar-font-family
             :font-size "1.2rem"
             :padding "0 2rem"
             :box-shadow "0 2px 5px rgba(0, 0, 0, 0.1)"
             (.nav-menu
               :display "flex"
               :list-style "none"
               :justify-content "center"
               (li :margin "0"
                 (a
                   :display "block"
                   :color ,nav-menu-color
                   :text-decoration "none"
                   :padding "1rem 1.5rem"
                   :font-weight "500"
                   :transition "all 0.3s ease"
                   :position "relative"
                   (.user :padding-right "0.2rem")
                   (.login :font-size "0.75rem" :vertical-align "sub"))
                 ("a::after"
                   :content ""
                   :position "absolute"
                   :width "0"
                   :height "3px"
                   :bottom "0"
                   :left "50%"
                   :background-color ,nav-menu-after-color
                   :transition "all 0.3s ease"
                   :transform "translateX(-50%)")
                 ("a:hover"
                   :color ,nav-menu-hover-color
                   :background-color ,page-background-color)
                 ("a:hover::after" :width "70%")
                 ("a.active"
                   :color ,nav-menu-active-color
                   :font-weight "600")
                 ("a.active::after" :width "70%")
                 (img :width "18px" :height "18px" :margin-right "4px"))))

           (.status-line
             :position "fixed"
             :left "50%"
             :bottom "20px"
             :transform "translateX(-50%)"
             :text-align "center"
             :font-family "mono"
             :font-size "0.8rem"
             :display "flex"
             :gap "4rem")

           ((:or .user-list .roles-list .users-list .roles-list .role-users-list)
             :width "100%"
             :align-items "center"
             (table
               :width "100%"
               :margin-bottom "1.5rem"
               :border-spacing "0"
               (th :text-align "left"
                 :border-bottom "1px solid black")
               (td :text-align "left")
               ("tr:nth-child(even)" :background-color ,table-stripe-color)
               (.user-roles-cell
                 (span :font-size "0.90rem")
                 (a :text-decoration "none"
                   (img :margin-left "10px"))))

             (.pager
               :text-align "center"
               :display "flex"
               :justify-content "center"
               :align-items "center"
               :gap "0.5rem"
               :font-size "0.95rem"))

           ((:or
              .add-user
              .add-role
              .add-role-user
              .add-directory
              .edit-directory-roles
              .edit-user-roles
              .settings-form
              .login-form)
             :display "grid"
             :align-items "start"
             :margin-top "1rem"
             :padding "1.5rem"
             :gap "0.5rem"
             (.form-title
               :grid-column "1 / -1"
               :text-align "center"
               :margin-bottom "1.0rem"
               :font-size "1.3rem"
               (.form-title-text
                 :display "inline-block"))
             (.form-group
               :display "flex"
               :align-items "right"
               (label
                 :grid-column "1"
                 :width "20rem"
                 :text-align "right"
                 :margin-right "0.5em")
               (.text-input
                 :grid-column "2"
                 :width "15rem"))
             (.password-group
               :display "flex"
               :flex-direction "column"
               :gap "0.50rem")
             (.input-checkbox-list
               :display "flex"
               :flex-direction "column"
               :align-items "flex-start"
               :grid-column "2"
               (label
                 :display "flex"
                 :align-items "center"
                 :margin "0"
                 :gap "0.25rem"
                 :width "auto"))
             (.button-container
               :grid-column "1 / -1"
               :justify-self "center"
               (.submit-button
                 :margin-top "1rem"))
             (.logout-link
               :grid-column "1 / -1"
               :margin-top "2.0em"
               :font-size "1.5rem"
               :justify-self "center"))

           ((:or .delete-users-form .delete-roles-form)
             :width "100%"
             (.button-container
               :display "flex"
               :justify-content "right"))

           (.confirmation
             :width "100%"
             :display "flex"
             :flex-direction "column"
             (.confirmation-form
               :display "flex"
               (.button-container
                 :margin-right "1rem"
                 (.cancel-button
                   :color ,cancel-button-color
                   :font-size "1.2rem")
                 (.confirm-button
                   :color ,confirm-button-color
                   :font-size "1.1rem"))))

           (.bogus-class :end "end"))))))
