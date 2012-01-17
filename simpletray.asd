(asdf:defsystem :simpletray
  :description "Simple tray implementation for use with Stumpwm."
  :author "Lucas Pandolfo & fortitude.zhang"
  :licence "LGPL"
  :version "0.1"
  :depends-on (:clx :bordeaux-threads :log5)
  :components ((:file "package")
	       (:file "tray-log")
	       (:file "tray")))

