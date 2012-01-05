(in-package :simpletray)
;; no need use stumpwm currently
;;(require 'stumpwm)

(require 'bordeaux-threads)

(defparameter *tray-thread* nil)
(defparameter *tray-display* nil)
(defparameter *tray-window* nil)
(defparameter *root-window* nil)
(defparameter *client-windows* nil)
(defparameter *tray-icon-width* 16)
(defparameter *tray-icon-height* 16)
(defparameter *tray-background-color* "green")

(defconstant +xembed-protocol-version+ 1)

(defconstant +xembed-embedded-notify+          0)
(defconstant +xembed-window-activate+          1)
(defconstant +xembed-window-deactivate+        2)
(defconstant +xembed-request-focus+            3)
(defconstant +xembed-focus-in+                 4)
(defconstant +xembed-focus-out+                5)
(defconstant +xembed-focus-next+               6)
(defconstant +xembed-focus-prev+               7)
;; 8-9 were used for xembed-grab-key/xembed-ungrab-key
(defconstant +xembed-modality-on+              10)
(defconstant +xembed-modality-off+             11)
(defconstant +xembed-register-accelerator+     12)
(defconstant +xembed-unregister-accelerator+   13)
(defconstant +xembed-activate-accelerator+     14)

;; details for  xembed-focus-in: 
(defconstant +xembed-focus-current+            0)
(defconstant +xembed-focus-first+              1)
(defconstant +xembed-focus-last+               2)

(defmacro make-reconfigure-mask (&rest keywords)
  (reduce #'logior 
	  (mapcar (lambda (x) (case x
				(:x 1)
				(:y 2)
				(:width 4)
				(:height 8)
				(:border-width 16)
				(:sibling 32)
				(:stack-mode 64)
				(t (error "~a is not a valid keyword in the reconfigure mask" x))))
		  keywords)))

(defun get-timestamp (window)
  (xlib:change-property window :WM_CLASS () :STRING 8 :mode :append) 
  (xlib:event-case ((xlib:window-display window) :force-output-p t
			    :discard-p nil)
    (:property-notify (time)
		      (format t "Timestamp: ~a~%" time)
		      time)
    (t () nil)))

(defun get-root-window (window)
  (xlib:drawable-root window))

(defun get-parent-window (window)
  (multiple-value-bind (children parent root)
      (xlib:query-tree window)
    (declare (ignore children root))
    parent))

(defun acquire-selection (window selection-name)
  (setf (xlib:selection-owner (xlib:window-display window) selection-name (get-timestamp window)) window)
  (if (xlib:window-equal window (xlib:selection-owner (xlib:window-display window) selection-name))
      t
      nil))

(defun get-window-name (window)
  (xlib:get-property window :_NET_WM_NAME :type :UTF8_STRING :result-type 'string :transform #'code-char))

(defun xembed-message-extra-data (window message-type)
  (let ((data
	 (if (eq message-type +xembed-embedded-notify+)
	     (list (xlib:window-id window) 1)
	     nil)))
    (format t "data: ~a~%" data)
    data))

(defun send-xembed-message (window dst-window message-type &optional (subtype 0))
  (xlib:send-event dst-window
		   :client-message #xfff :propagate-p nil :display (xlib:window-display window)
		   :window dst-window :format 32
		   :type :_XEMBED :event-window dst-window
		   :data (append (list (get-timestamp window) message-type subtype)
				 (xembed-message-extra-data window message-type)))
  (xlib:display-finish-output (xlib:window-display window)))

(defun remove-client (window)
  (setf *client-windows* (remove window *client-windows* :test #'xlib:window-equal)))

(defun visibility-changed (window state)
  (format t "Visibility for window ~a changed to ~a~%" window state))

(defun resize-window (window width height)
  (xlib:send-event (get-root-window window) :configure-request #xfff 
		   :parent (get-root-window window) :window window 
		   :width width :height height 
		   :value-mask (make-reconfigure-mask :width :height))
  (xlib:display-finish-output (xlib:window-display window)))

(defun resize-drawable (drawable width height)
  (log-flow-to-file "resizing drawable to ~a ~a ~%" width height)
  (unless (or (eq width 0) (eq height 0))
    (setf (xlib:drawable-height drawable) height)
    (setf (xlib:drawable-width drawable) width)))

(defun resize-icon (window width height)
  (resize-drawable window
		   (min width *tray-icon-width*)
		   (min height *tray-icon-height*)))

(defun position-window (window x y)
  (xlib:send-event (get-parent-window window) :configure-request #xfff 
		   :parent (get-parent-window window) :window window 
		   :x x :y y :value-mask (make-reconfigure-mask :x :y))
  (xlib:display-finish-output (xlib:window-display window)))

(defun position-subwindow (parent window x y)
  (log-flow-to-file "positioning window at ~a ~a ~%" x y)
  (xlib:reparent-window window parent x y)
  (xlib:display-finish-output (xlib:window-display window)))

(defun get-window-max-dimensions (window)
  (let ((hints (xlib:get-property window :WM_NORMAL_HINTS :type nil)))
    (format t "Dimensions: ~a~%" hints)
    (list (nth 5 hints) (nth 6 hints))))

(defun get-window-max-width (window)
  (car (get-window-max-dimensions window)))

(defun get-window-max-height (window)
  (cadr (get-window-max-dimensions window)))

(defun reorganize-icons (window clients)
  (loop for i in clients
     do
       (position-subwindow window i 
			   (round 
			    (+ total-width 
			       (/ (- *tray-icon-width*
				     (xlib:drawable-width i)) 2)))
			   (round 
			    (/ (- *tray-icon-height* 
				  (xlib:drawable-height i)) 2)))
     summing *tray-icon-width* into total-width
     finally
       (log-flow-to-file "Ancho = ~a Alto = ~a ~%" total-width *tray-icon-height*)
       (if (> total-width 0)
	   (resize-window window total-width *tray-icon-height*)
	   ;; always make it one icon size which make it looks pretty.
	   (resize-window window *tray-icon-width* *tray-icon-height*)))
  nil)

(defgeneric process-client-message (window type format data)
  (:documentation "Process client messages"))

(defmethod process-client-message (window (type (eql :_XEMBED)) format data)
  (format t "_XEMBED message received: format:  ~a data: ~a ~%" format data))

(defmethod process-client-message (window (type (eql :_NET_SYSTEM_TRAY_OPCODE)) format data)
  (format t "Event type _NET_SYSTEM_TRAY_OPCODE~%")
  (log-flow-to-file "Event type _NET_SYSTEM_TRAY_OPCODE with window ~A ~%" window)
  (case (aref data 1)
    (0 (log-flow-to-file "Window ~a requests dock~%" (aref data 2))
       (let* ((client-window (xlib::make-window :id (aref data 2) :display (xlib:window-display window)))
	      ;;(client-protocol-version (xlib:get-property  client-window "_XEMBED_INFO" :type nil)))
	      )
	 ;; FIXME: workaround for make-window cannot generate correct window type
	 (if (xlib:window-p client-window)
	     (progn
	       (push client-window *client-windows*)
	       (xlib:reparent-window client-window window 0 0)
	       (log-flow-to-file "Window reparented~%")
	       ;;(format t "Client XEMBED protocol version: ~a~%" client-protocol-version)
	       (send-xembed-message window client-window +xembed-embedded-notify+)
	       (log-flow-to-file "XEMBED embedded notify message sent to ~a ~%" (get-window-name client-window))
	       (resize-drawable client-window
				*tray-icon-width*
				*tray-icon-height*)
	       (reorganize-icons window *client-windows*)
	       (xlib:map-window client-window))
	     (log-flow-to-file "Cannnot generate window type~%"))))
    (1 (log-flow-to-file "Begin Message event~%"))
    (2 (log-flow-to-file "End Message event~%"))
    (otherwise (log-flow-to-file "Unknown message received.")))
  nil)

(defun alloc-color (screen color)
  (xlib:alloc-color (xlib:screen-default-colormap screen) color))

(defun tray (&optional (host ""))
  (log-flow-to-file "tray thread enter ~%")
  (let* ((display (xlib:open-display host))
	 (screen (first (xlib:display-roots display)))
	 (black (xlib:screen-black-pixel screen))
	 (root-window (xlib:screen-root screen))
	 (my-window (xlib:create-window
		     :parent root-window
		     :x -1
		     :y -1
		     ;;		     :depth 24
		     :width *tray-icon-width*
		     :height *tray-icon-height*
		     ;;		     :background black
		     ;; here we directly use the non-export symbol of stumpwm
		     :background (alloc-color screen *tray-background-color*)
					;:override-redirect :ON
		     :event-mask (xlib:make-event-mask :exposure :property-change 
						       :key-press :visibility-change 
						       :substructure-notify :substructure-redirect))))
    (setf *root-window* root-window)
    (setf *tray-display* display)
    (setf *tray-window* my-window)
    (setf *client-windows* nil)

    (xlib:change-property my-window 
			  :WM_TRANSIENT_FOR (list (xlib:window-id root-window))
			  :WINDOW 32)
    
    (xlib:change-property *tray-window* 
			  :WM_NAME (coerce "Stumpwm system tray" 'list) 
			  :STRING 8 :transform #'char-code)

    (xlib:change-property *tray-window* 
			  :_NET_WM_NAME (coerce "Stumpwm system tray" 'list) 
			  :UTF8_STRING 8 :transform #'char-code)

    (xlib:map-window my-window)

    ;; FIXME, change S0 with Sn(n is calculated by aquire screen number)
    (when (acquire-selection my-window :_NET_SYSTEM_TRAY_S0)
      (format t "Selection acquired~%")
      ;; Loop until we die!
      (catch 'destroy
	(loop
	   (xlib:event-case (display :force-output-p t
				     :discard-p t)
	     (:selection-request () (format t "Selection request event~%"))
	     (:selection-notify () (format t "Selection notify event~%"))
	     (:selection-clear () (format t "Selection clear event~%") t)
	     (:property-notify (atom state time window) (format t "Property notify (~a) ~a ~a ~a~%" window atom state time))
					;(:key-press () (format t "Key press event~%") (process-click) nil)
	     (:visibility-notify (state) (visibility-changed my-window state) nil)
	     (:configure-request (width height x y window) 
				 (format t "W: ~a H: ~a X: ~a Y: ~a from window: ~a~%" width height x y window) 
				 (resize-icon window width height)
				 (reorganize-icons my-window *client-windows*)
				 nil)
	     (:destroy-notify (window) (format t "Window ~a destroyed~%" window) 
			      (remove-client window) 
			      (reorganize-icons my-window *client-windows*)
			      throw 'destroy)
	     (:client-message (type format data)
			      (process-client-message my-window type format data)))))
      (xlib:close-display display))))


(defun create ()
  (setf *tray-thread* (bordeaux-threads:make-thread 'tray :name "Tray: main loop thread")))

(defun destroy ()
  ;; destroy the thread
  (if (bordeaux-threads:threadp *tray-thread*)
      (bordeaux-threads:destroy-thread  *tray-thread*))
  ;; once we have closed the display,X related resources will be freed automatically by X server.
  (if (xlib:display-p *tray-display*)
      (xlib:close-display *tray-display*)))
