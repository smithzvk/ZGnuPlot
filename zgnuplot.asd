
(asdf:defsystem :zgnuplot
  :author "Zach Smith"
  :license "GPLv3 or later. See file COPYING"
  :depends-on (:cffi :toolbox :defclass-star :iterate :cgn :osicat)
  :components ((:file "package")
               (:file "zgnuplot"))
  :serial t )