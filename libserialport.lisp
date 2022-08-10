
(in-package libserialport)


(defconstant +serial-buff-size+ 256) ;; size in butes of fixed buffer in serial port

(defstruct serial-port
  (name nil)
  (port-ptr nil)
  ;; foreign buffer of size +serial-buff-size+
  (fbuf nil)
  ;; a vector #(T) if this port is alive - may be wrapped in a lambda
  ;; for possible finalization, because a finalizer lambda can't close
  ;; around SERIAL-PORT object.  Currently, there is no finalization.
  (alive (make-array 1 :initial-contents '(T)))
  (baud nil)
  (bits nil)
  (stopbits nil)
  (parity nil)
  (rts nil)
  (cts nil)
  (dtr nil)
  (dsr nil)
  (xonxoff nil)
  (flowcontrol nil)
  (transport nil))


(defstruct serial-port-description
  name
  description
  transport ;; "description"
  ;; usb details
  usb-bus usb-address
  usb-vendor-id usb-product-id
  usb-manufacturer
  usb-product
  usb-serial-number
  bluetooth-mac-address)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note used serial ports in *SERIAL-PORT-HASH* when they are opened,
;; and remove them when shut down.  This is the current alternative to
;; finalization, because it is really user's job to keep track of, and
;; cleanly shut down, serial ports rather than trusting the GC.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *serial-port-hash*
  (make-hash-table :test 'equal))
					    			 
(defvar *serial-port-lock* (bordeaux-threads:make-lock "serial-port-hash-lock"))

(defun %note-serial-port-as-open (serial-port)
  (declare (type serial-port serial-port))
  (bordeaux-threads:with-lock-held (*serial-port-lock*)
    (setf (gethash (serial-port-name serial-port) *serial-port-hash*)
	  serial-port)))

(defun %note-serial-port-as-closed (serial-port)
  (declare (type serial-port serial-port))
  (bordeaux-threads:with-lock-held (*serial-port-lock*)
    (remhash (serial-port-name serial-port) *serial-port-hash*)))

(defun %serial-port-open-p (location-or-serial-port)
  (declare (type (or string serial-port) location-or-serial-port))
  (let ((key (cond ((stringp location-or-serial-port) location-or-serial-port)
		   (t (serial-port-name location-or-serial-port)))))
    (bordeaux-threads:with-lock-held (*serial-port-lock*)
      (gethash key *serial-port-hash*))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mark serial port as un-alive 
(defun %set-serial-port-unalive (serial-port)
  (declare (type serial-port serial-port))
  (setf (aref (serial-port-alive serial-port) 0) nil))

(defun serial-port-alive-p (serial-port)
  (declare (type serial-port serial-port))
  (and (aref (serial-port-alive serial-port) 0)
       (serial-port-port-ptr serial-port)))

(defun build-serial-port-description-for-ptr (port-ptr &key (include-errors nil))
  "This returns a SERIAL-PORT-DESCRIPTION or NULL if an error occurs.

If INCLUDE-ERRORS is set, then error responses (as keywords) are
returned in the slots instead of NIL.  This works only for USB
qualities bus, address, vendor-id, product-id."
  (let ((name (sp-get-port-name port-ptr))
	(description (sp-get-port-description port-ptr))
	(transport (sp-get-port-transport port-ptr))
	usb-manufacturer
	usb-product 
	usb-serial-number 
	usb-bus usb-address
	usb-vendor-id usb-product-id
	bluetooth-mac-address)

    ;; it seems that even objects with transport=sp-transport-native
    ;; have USB qualities.
    (cffi:with-foreign-objects
	((usb-bus-ptr :int)
	 (usb-addr-ptr :int))
      (let ((retval
	      (sp-get-port-usb-bus-address port-ptr usb-bus-ptr usb-addr-ptr)))
	(cond ((eq retval :sp-ok)
	       (setf usb-bus (cffi:mem-aref usb-bus-ptr :int))
	       (setf usb-address (cffi:mem-aref usb-addr-ptr :int)))
	      (include-errors
	       (setf usb-bus retval)
	       (setf usb-address retval)))))
	       

     (cffi:with-foreign-objects
	((usb-vid-ptr :int)
	 (usb-pid-ptr :int))
      (let ((retval
	      (sp-get-port-usb-vid-pid port-ptr usb-vid-ptr usb-pid-ptr)))
	(cond ((eq retval :sp-ok)
	       (setf usb-vendor-id (cffi:mem-aref usb-vid-ptr :int))
	       (setf usb-product-id (cffi:mem-aref usb-pid-ptr :int)))
	      (include-errors
	       (setf usb-vendor-id retval)
	       (setf usb-product-id retval)))))

    (setf usb-manufacturer
	  (sp-get-port-usb-manufacturer port-ptr))
    (setf usb-product (sp-get-port-usb-product port-ptr))
    (setf usb-serial-number (sp-get-port-usb-serial port-ptr))
    (setf bluetooth-mac-address
	  (sp-get-port-bluetooth-address port-ptr))

    (make-serial-port-description
     :name name
     :description description 
     :usb-manufacturer usb-manufacturer
     :usb-product usb-product
     :usb-serial-number usb-serial-number
     :usb-vendor-id usb-vendor-id
     :usb-product-id usb-product-id 
     :transport transport
     :usb-bus usb-bus
     :usb-address usb-address
     :bluetooth-mac-address bluetooth-mac-address)))

(defun build-serial-port-description (serial-port)
  "Create a SERIAL-PORT-DESCRIPTION for a SERIAL-PORT"
  (declare (type serial-port serial-port))
  (assert (serial-port-alive-p serial-port))
  (build-serial-port-description (serial-port-port-ptr serial-port)))

(defun list-serial-ports (&key (include-errors nil))
  "List available (though not necessarily connected) serial ports,
returning a list of SERIAL-PORT-DESCRIPTION structures.

If INCLUDE-ERRORS is set, then error responses (as keywords) are
returned in the slots instead of NIL.  This works only for USB
qualities bus, address, vendor-id, product-id."
  (cffi:with-foreign-object (port-ptr-ptr-ptr :pointer)
    (let ((retval (sp-list-ports port-ptr-ptr-ptr)))
      (when (not (eq retval :sp-ok))
	(error "Error listing ports: ~A" retval))
      (loop
        with port-ptr-ptr = (cffi:mem-ref port-ptr-ptr-ptr :pointer)
	for i from 0
	for port-ptr = (cffi:mem-aref port-ptr-ptr :pointer i)
	until (cffi:null-pointer-p  port-ptr)
	collect  (build-serial-port-description-for-ptr
		  port-ptr
		  :include-errors include-errors)
	finally
	   (sp-free-port-list port-ptr-ptr)))))

(defun shutdown-serial-port (serial-port)
  "Shut down a serial port, returning T."
  (declare (type serial-port serial-port))
  (when (serial-port-alive-p serial-port)
    (let ((retval (sp-close (serial-port-port-ptr serial-port))))
      (when (not (eq retval :sp-ok))
	(error "Error ~A closing serial port ~A"
	       retval (serial-port-name serial-port))))
    (sp-free-port  (serial-port-port-ptr serial-port))
    (when (serial-port-fbuf serial-port)
      (cffi:foreign-free (serial-port-fbuf serial-port))
      (setf (serial-port-fbuf serial-port) nil))
    (%set-serial-port-unalive serial-port)
    (%note-serial-port-as-closed serial-port)
    (setf (serial-port-port-ptr serial-port) nil)
    t))

(defun shutdown-all-serial-ports ()
  "Shut down all serial ports in *SERIAL-PORT-HASH*.
Returns list of the form ((NAME1 .  NIL-OR-ERROR) (NAME2 . NIL-OR-ERROR))
where NIL-OR-ERROR is NIL on success, or an error object on the closing."
  (let ((pairs nil))
    (maphash (lambda (key val)
	       (multiple-value-bind (ret err)
		   (ignore-errors (shutdown-serial-port val))
		 (declare (ignore ret))
		 (push (cons key err) pairs)))
	     *serial-port-hash*)
    pairs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the next two macros bind buffers to variables, using pre-allocated
;; buffers if possible to minimize consing/malloc

;; macro analogous to cffi:with-foreign-object that binds varname to
;; a foreign pointer containing at least nbytes.   But if nbytes
;; is <+serial-buff-size+ it uses the buffer in serial port.  Useful for reading
;; char by char beause there won't be a malloc with every char.
#+nil
(defmacro with-serial-port-buffer ((varname serial-port nbytes) &body body)
  (let ((body-func-name (gensym "WITH-SERIAL-PORT-BUFFER-BODY"))
	(tmpbufname (gensym "WITH-SERIAL-PORT-BUFFER-TMP"))
	(nbytes-name (gensym "NBYTES")))
    ;; otherwise create two runtime branches
    `(let ((,nbytes-name ,nbytes))
       (flet ((,body-func-name (,varname)
		,@body))
	 (if (<= ,nbytes-name +serial-buff-size+) 
	     (,body-func-name (serial-port-fbuf ,serial-port))
	     (cffi:with-foreign-object (,tmpbufname :unsigned-char ,nbytes-name)
	       (,body-func-name ,tmpbufname)))))))

;; newer version that allocates the foreign buffer on the heap rather than the C-stack
(defmacro with-serial-port-buffer ((varname serial-port nbytes) &body body)
  (let ((body-func-name (gensym "WITH-SERIAL-PORT-BUFFER-BODY"))
	(tmpbufname (gensym "WITH-SERIAL-PORT-BUFFER-TMP"))
	(nbytes-name (gensym "NBYTES")))
    ;; use fixed buffer, or a larger buffer if needed
    `(let ((,nbytes-name ,nbytes))
       (flet ((,body-func-name (,varname)
		,@body))
	 (if (<= ,nbytes-name +serial-buff-size+)
	     (,body-func-name (serial-port-fbuf ,serial-port))
	     (progn ;; nbytes too big, so allocate on foreign heap
	       (let ((,tmpbufname nil))
		 (unwind-protect
		      (progn
			(setf ,tmpbufname (cffi:foreign-alloc :unsigned-char :count ,nbytes-name))
			(,body-func-name ,tmpbufname))
		   (progn
		     (cffi:foreign-free ,tmpbufname))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		 


(defun set-serial-port-qualities (serial-port
				  &key
				    baud 
				    bits
				    parity
				    stopbits
				    xonxoff
				    flowcontrol
				    rts
				    cts
				    dtr
				    dsr
				    ;;
				    (shutdown-on-failure nil))
  "Set a serial port's qualities.   See documentation of OPEN-SERIAL-PORT.

Any of the following can also be NULL, in which case the quality will be
unchanged.

BITS can be 7 or 8.
PARITY can be :SP-PARITY-{NONE,ODD,EVEN,MARK,SPACE}
STOPBITS can be 1 or 2
XONXOFF can be :SP-XONOXOFF-{DISABLED,IN,OUT,INOUT}
FLOWCONTROL can be :SP-FLOWCONTROL-{NONE,XONXOFF,RTSCTS,DTRDSR}
RTS can be :SP-RTS-{OFF,ON,FLOW-CONTROL}
CTS can be :SP-CTS-{IGNORE,FLOW-CONTROL}
DTR can be :SP-DTR-{OFF,ON,FLOW-CONTROL}
DSR can be :SP-DSR-{IGNORE,FLOW-CONTROL}

If SHUTDOWN-ON-FAILURE is set, then any error closes the serial port, and
deallocates the underlying C liberserialport structure."
  (declare
   (type serial-port serial-port)
   (type (member nil 7 8) bits)
   (type (member nil :sp-parity-none :sp-parity-even :sp-parity-odd
				 :sp-parity-mark :sp-parity-space)
	 parity)
   (type (member nil 1 2) stopbits)
   (type (member nil :sp-xonxoff-disabled :sp-xonxoff-in :sp-xonxoff-out
					  :sp-xonxoff-inout)
	 xonxoff)
   (type (member nil :sp-flowcontrol-none :sp-flowcontrol-xonxoff
		 :sp-flowcontrol-dtrdsr :sp-flowcontrol-rtscts)
	 flowcontrol)
   (type (member nil :sp-rts-off :sp-rts-on :sp-rts-flow-control) rts)
   (type (member nil :sp-cts-ignore :sp-cts-flow-control) cts)
   (type (member nil :sp-dsr-ignore :sp-dsr-flow-control) dsr)
   (type (member nil :sp-dtr-off :sp-dtr-on :sp-dtr-flow-control) dtr))

  (let ((port-ptr (serial-port-port-ptr serial-port))
	(config-ptr nil)
	(failed nil))
    (unwind-protect
	 (progn
	   ;; allocate a new config
	   (cffi:with-foreign-object (ptrvec :pointer)
	     (let ((retval (sp-new-config ptrvec)))
	       (when (not (eq retval :sp-ok))
		 (error "Error ~A in sp-new-config" retval))
	       (setf config-ptr (cffi:mem-aref ptrvec :pointer))))
	   ;;
	   ;; copy the config from the port
	   (let ((retval (sp-get-config port-ptr config-ptr)))
	     (when (not (eq retval :sp-ok))
	       (setf failed t)
	       (error "Error ~A in sp-get-config" retval)))
	   ;;
	   ;;
	   (when baud
	     (let ((retval (sp-set-config-baudrate config-ptr baud)))
	       (when (not (eq retval :sp-ok))
		 (setf failed t)
		 (error "Error ~A in sp-set-config-baudrate." retval))
	       (setf (serial-port-baud serial-port) baud)))
	   ;;
	   (when bits
	     (let ((retval (sp-set-config-bits config-ptr bits)))
	       (when (not (eq retval :sp-ok))
		 (setf failed t)
		 (error "Error ~A in sp-set-config-bits" retval)))
	     (setf (serial-port-bits serial-port) bits))
	   ;;
	   (when parity
	     (let ((retval (sp-set-config-parity config-ptr parity)))
	       (when (not (eq retval :sp-ok))
		 (setf failed t)
		 (error "Error ~A in sp-set-config-parity" retval)))
	     (setf (serial-port-parity serial-port) parity))
	   ;;
	   (when stopbits
	     (let ((retval (sp-set-config-stopbits config-ptr stopbits)))
	       (when (not (eq retval :sp-ok))
		 (setf failed t)
		 (error "Error ~A in sp-set-config-stopbits" retval)))
	     (setf (serial-port-stopbits serial-port) stopbits))
	   ;;
	   (when xonxoff
	     (let ((retval (sp-set-config-xon-xoff config-ptr xonxoff)))
	       (when (not (eq retval :sp-ok))
		 (setf failed t)
		 (error "Error ~A in sp-set-config-xon-xoff" retval)))
	     (setf (serial-port-xonxoff serial-port) xonxoff))
	   ;;
	   (when flowcontrol
	     (let ((retval (sp-set-config-flowcontrol config-ptr flowcontrol)))
	       (when (not (eq retval :sp-ok))
		 (setf failed t)
		 (error "Error ~A in sp-set-config-flowcontrol" retval)))
	     (setf (serial-port-flowcontrol serial-port) flowcontrol))
	   ;;
	   (when rts
	     (let ((retval (sp-set-config-rts config-ptr rts)))
	       (when (not (eq retval :sp-ok))
		 (setf failed t)
		 (error "Error ~A in sp-set-config-rts" retval)))
	     (setf (serial-port-rts serial-port) rts))
	   ;;
	   (when cts
	     (let ((retval (sp-set-config-cts config-ptr cts)))
	       (when (not (eq retval :sp-ok))
		 (setf failed t)
		 (error "Error ~A in sp-set-config-cts" retval)))
	     (setf (serial-port-cts serial-port) cts))
	   ;;
	   (when dtr
	     (let ((retval (sp-set-config-dtr config-ptr dtr)))
	       (when (not (eq retval :sp-ok))
		 (setf failed t)
		 (error "Error ~A in sp-set-config-dtr" retval)))
	     (setf (serial-port-dtr serial-port) dtr))
	   ;;
	   (when dsr
	     (let ((retval (sp-set-config-dsr config-ptr dsr)))
	       (when (not (eq retval :sp-ok))
		 (setf failed t)
		 (error "Error ~A in sp-set-config-dsr" retval)))
	     (setf (serial-port-dsr serial-port) dsr))
	   ;;
	   ;; transfer the config back to the port
	   (let ((retval (sp-set-config port-ptr config-ptr)))
	     (when (not (eq retval :sp-ok))
	       (setf failed t)
	       (error "Error ~A in sp-set-config" retval)))
	   ;; dealloc will occur in unwind-protected form
	   )

      (progn ;; unwind protected form
	(when config-ptr (sp-free-config config-ptr))
	(when (and failed shutdown-on-failure)
	  (shutdown-serial-port serial-port)))
    
    )))

(defun open-serial-port (port-name
			 &key
			   (mode :sp-mode-read-write)
			   (baud 9600)
			   (bits 8)
			   (parity :sp-parity-none)
			   (stopbits 1)
			   (xonxoff :sp-xonxoff-disabled)
			   (flowcontrol :sp-flowcontrol-none)
			   (rts :sp-rts-off)
			   (cts :sp-cts-ignore)
			   (dtr :sp-dtr-off)
			   (dsr :sp-dsr-ignore))
  "Open a serial port, named by PORT-NAME, typically the path
to the file.

BITS can be 7 or 8.
MODE can be :SP-MODE-{READ,WRITE,READWRITE}
PARITY can be :SP-PARITY-{NONE,ODD,EVEN,MARK,SPACE}
STOPBITS can be 1 or 2
XONXOFF can be :SP-XONOXOFF-{DISABLED,IN,OUT,INOUT}
FLOWCONTROL can be :SP-FLOWCONTROL-{NONE,XONXOFF,RTSCTS,DTRDSR}
RTS can be :SP-RTS-{OFF,ON,FLOW-CONTROL}
CTS can be :SP-CTS-{IGNORE,FLOW-CONTROL}
DTR can be :SP-DTR-{OFF,ON,FLOW-CONTROL}
DSR can be :SP-DSR-{IGNORE,FLOW-CONTROL}
"
  (declare
   (type (or pathname string) port-name)
   (type (member 7 8) bits)
   (type (member :sp-mode-read :sp-mode-write :sp-mode-read-write) mode)
   (type (member :sp-parity-none :sp-parity-even :sp-parity-odd
				 :sp-parity-mark :sp-parity-space)
	 parity)
   (type (member 1 2) stopbits)
   (type (member :sp-xonxoff-disabled :sp-xonxoff-in :sp-xonxoff-out
				      :sp-xonxoff-inout)
	 xonxoff)
   (type (member :sp-flowcontrol-none :sp-flowcontrol-xonxoff
		 :sp-flowcontrol-dtrdsr :sp-flowcontrol-rtscts)
	 flowcontrol)
   (type (member :sp-rts-off :sp-rts-on :sp-rts-flow-control) rts)
   (type (member :sp-cts-ignore :sp-cts-flow-control) cts)
   (type (member :sp-dsr-ignore :sp-dsr-flow-control) dsr)
   (type (member :sp-dtr-off :sp-dtr-on :sp-dtr-flow-control) dtr))


  (let* ((port-string (if (pathnamep port-name)
			  (namestring port-name) port-name))
	 port-ptr)

    (when (%serial-port-open-p port-string)
      (error "Serial port ~A is already open (in *SERIAL-PORT-HASH*).  Cannot open again."
	     port-string))
    

    ;; find the port by name and set PORT-PTR
    (cffi:with-foreign-object (ptrvec :pointer)
      (let ((retval (sp-get-port-by-name port-string ptrvec)))
	(when (not (eq retval :sp-ok))
	  (error "Error ~A getting serial port <~A>" retval port-string))
	(setf port-ptr (cffi:mem-ref ptrvec :pointer))))

    ;; open the port
    (let ((retval (sp-open port-ptr mode)))
      (when (not (eq retval :sp-ok))
	(sp-free-port port-ptr)
	(error "Error ~A opening serial port <~A>" retval port-string)))

    (let ((serial-port (make-serial-port
			:name port-string
			:port-ptr port-ptr
			:fbuf (cffi:foreign-alloc :unsigned-char
						  :count +serial-buff-size+))))
      (set-serial-port-qualities
       serial-port
       :baud baud :bits bits :parity parity :stopbits stopbits
       :xonxoff xonxoff :flowcontrol flowcontrol :rts rts :cts cts
       :dtr dtr :dsr dsr
       :shutdown-on-failure t)
      (%note-serial-port-as-open serial-port)
      serial-port)))
			


  
				   
