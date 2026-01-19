(defsystem "metamusic"
  :description ""
  :serial t
  :depends-on (:quri :dexador :jonathan)
  :components ((:file "utils")
               (:file "main")))