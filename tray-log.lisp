(in-package :simpletray)

(require 'log5)

;; set this variable to false to disable tray log
(defparameter *tray-log-enable* t)

;; Log code flow
(log5:defcategory flow-log)

(log5:start-sender 'log-file-sender
		   (log5:stream-sender :location "~/stumptray.log")  
		   :category-spec '(flow-log)  
		   :output-spec '(log5:time log5:message log5:category)) 

(defmacro log-flow-to-file (log-string &rest log-items)
  `(if *tray-log-enable*
      ;; slice the log-items
      (log5:log-for (flow-log) ,log-string ,@log-items)))
