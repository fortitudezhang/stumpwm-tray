(cl:defpackage :simpletray
  (:use :cl :bordeaux-threads :log5)
  (:export 
      :create
      :destroy))
