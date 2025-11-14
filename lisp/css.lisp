(in-package :file-server)

(defun generate-css ()
  (let ((page-background-color "#fff")
         (navbar-background-color "#eee")
         (navbar-font-family "mono")
         (nav-menu-color "#222")
         (nav-menu-after-color "#ddd")
         (nav-menu-hover-color "#0c0")
         (nav-menu-active-color "#fff"))
    (l:compile-and-write
      `(.main-page
         :background-color ,page-background-color
         :max-width "60rem"
         :margin "0 auto"
         :padding "1rem"
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
                 :background-color "rgba(255, 255, 255, 0.1)")
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
           :font-size "0.8rem")

         ((:or .user-list .roles-list .users-list .roles-list)
           :width "100%"
           :align-items "center"
           (table
             :width "100%"
             :margin-bottom "1.5rem"
             :border-spacing "0"
             (th :text-align "left"
               :border-bottom "1px solid black")
             (td :text-align "left")
             ("tr:nth-child(even)" :background-color "#f2f2f2")
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

         ((:or .add-user .add-role .add-directory .edit-directory-roles .edit-user-roles)
           :display "grid"
           :align-items "start"
           :margin-top "1rem"
           :padding "1.5rem"
           :gap "0.5rem"
           (.form-group
             :display "flex"
             :align-items "right"
             (label
               :grid-column "1"
               :width "20rem"
               :text-align "right"
               :margin-right "0.5em")
             (.textinput
               :grid-column "2"
               :width "50%"))
           (.checkbox-group
             :grid-column "2")
           (.button-container
             :grid-column "1 / -1"
             :justify-self "center"
             (.submit-button
               :margin-top "1rem")))

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
                 :color "#f00"
                 :font-size "1.1rem")
               (.confirm-button
                 :color "#0c0"
                 :font-size "1.1rem"))))

         (.bogus-class :end "end")))))
