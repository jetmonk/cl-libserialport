
(in-package libserialport)


(defstruct event-set
  event-set-ptr
  port-names  ;; the ports added to this event set
  ;; a vector #(T) if this event-set is alive 
  (alive (make-array 1 :initial-contents '(T))))


;; mark event-set as un-alive 
(defun %set-event-set-unalive (event-set)
  (declare (type event-set event-set))
  (setf (aref (event-set-alive event-set) 0) nil))

(defun event-set-alive-p (event-set)
  (declare (type event-set event-set))
  (and (aref (event-set-alive event-set) 0)
       (event-set-event-set-ptr event-set)))

(defun create-event-set ()
  "Create a new EVENT-SET for allowing simultaneous waiting on serial ports."
  (cffi:with-foreign-object (es-ptr-ptr :pointer)
    (let ((retval (sp-new-event-set es-ptr-ptr)))
      (when (not (eq retval :sp-ok))
	(error "Error ~A in sp-new-event-set" retval))
      (make-event-set
       :event-set-ptr (cffi:mem-ref es-ptr-ptr :pointer)))))

(defun destroy-event-set (event-set)
  "Destroy an EVENT-SET, unallocating foreign memory."
  (declare (type event-set event-set))
  (when (event-set-alive-p event-set)
    (%set-event-set-unalive event-set)
    (sp-free-event-set (event-set-event-set-ptr event-set))))


  
(defun add-serial-port-to-event-set (serial-port event-set
						 &key
						 (rx-ready t)
						 (tx-ready t)
						 (error    t))
  "Add an PORT to an EVENT-SET, with the events being waited defined by keywords
:RX-READY, :TX-READ, :ERROR."
  (declare (type serial-port serial-port)
	   (type event-set event-set))
  (assert (serial-port-alive-p serial-port))
  (assert (event-set-alive-p event-set))
  (let ((retval (sp-add-port-events (event-set-event-set-ptr event-set)
				    (serial-port-port-ptr serial-port)
				    (logior
				     (if rx-ready (cffi:foreign-enum-value 'sp-event :sp-event-rx-ready) 0)
				     (if tx-ready (cffi:foreign-enum-value 'sp-event :sp-event-tx-ready) 0)
				     (if error    (cffi:foreign-enum-value 'sp-event :sp-event-error) 0)))))
    (when (not (eq retval :sp-ok))
      (error "ERROR ~A in sp-add-port-events" retval))
    (pushnew (serial-port-name serial-port)
	     (event-set-port-names event-set)
	     :test 'equal)))


(defun event-wait (event-set &key (timeout 1000))
  (declare (type event-set event-set)
	   (type (unsigned-byte 32) timeout))
  "Wait on events in EVENT-SET for TIMEOUT in milliseconds (0 means forever).
The serial ports can then be checked with SERIAL-INPUT-WAITING, SERIAL-OUTPUT-WAITING."
  (assert (event-set-alive-p event-set))
  (let ((retval (sp-wait (event-set-event-set-ptr event-set) timeout)))
    (when (not (eq retval :sp-ok))
      (error "ERROR ~A in sp-wait" retval))))
    
    
  
					
	  
  
