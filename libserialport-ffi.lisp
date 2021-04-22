
#|

 This is a full CFFI export of the libserialport library

 https://sigrok.org/wiki/Libserialport

 All ENUMS are turned into symbols, and functions should return these symbols.

|#

(in-package libserialport)

;; FIXME
(cffi:load-foreign-library "./lib/libserialport.dylib")

(cffi:defctype enum :int)

(cffi:defctype size_t #.(cond ((= 4 (cffi:foreign-type-size :pointer))
			       :uint32)
			      ((= 8 (cffi:foreign-type-size :pointer))
			       :uint64)
			      (t (error "Failed to deterine type size_t"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some functions return errors, but also an integer count on success,
;; so these allow undeclared enum values to pass through
(cffi:defcenum (sp-return :int :allow-undeclared-values t)
  (:sp-ok 0)
  (:sp-err-arg -1)
  (:sp-err-fail -2)
  (:sp-err-mem -3)
  (:sp-err-supp -4))

(cffi:defcenum sp-mode
  (:sp-mode-read 1)
  (:sp-mode-write 2)
  (:sp-mode-read-write 3))

(cffi:defcenum sp-event
  (:sp-event-rx-ready 1)
  (:sp-event-tx-ready 2)
  (:sp-event-error 4))

(cffi:defcenum sp-buffer
  (:sp-buf-input 1)
  (:sp-buf-output 2)
  (:sp-buf-both 3))

(cffi:defcenum sp-parity
  (:sp-parity-invalid -1)
  (:sp-parity-none 0)
  (:sp-parity-odd 1)
  (:sp-parity-even 2)
  (:sp-parity-mark 3)
  (:sp-parity-space 4))

(cffi:defcenum sp-rts
  (:sp-rts-invalid -1)
  (:sp-rts-off 0)
  (:sp-rts-on 1)
  (:sp-rts-flow-control 2))

(cffi:defcenum sp-cts
  (:sp-cts-invalid -1)
  (:sp-cts-ignore 0)
  (:sp-cts-flow-control 1))

(cffi:defcenum sp-dtr
  (:sp-dtr-invalid -1)
  (:sp-dtr-off 0)
  (:sp-dtr-on 1)
  (:sp-dtr-flow-control 2))

(cffi:defcenum sp-dsr
  (:sp-dsr-invalid -1)
  (:sp-dsr-ignore 0)
  (:sp-dsr-flow-control 1))

(cffi:defcenum sp-xonxoff
  (:sp-xonxoff-invalid -1)
  (:sp-xonxoff-disabled 0)
  (:sp-xonxoff-in 1)
  (:sp-xonxoff-out 2)
  (:sp-xonxoff-inout 3))

(cffi:defcenum sp-flowcontrol
  (:sp-flowcontrol-none 0)
  (:sp-flowcontrol-xonxoff 1)
  (:sp-flowcontrol-rtscts 2)
  (:sp-flowcontrol-dtrdsr 3))

(cffi:defcenum sp-signal
  (:sp-sig-cts 1)
  (:sp-sig-dsr 2)
  (:sp-sig-dcd 4)
  (:sp-sig-ri  8))

(cffi:defcenum sp-transport
  :sp-transport-native
  :sp-transport-usb
  :sp-transport-bluetooth)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cffi:defcstruct sp-event-set
  ;; array of OS-specific handles
  (handles (:pointer :void))
  ;; array of masks
  (masks (:pointer sp-event))
  (count :int))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cffi:defcfun "sp_get_port_by_name"
    sp-return
  (portname :string)
  (port-ptr-ptr :pointer))

(cffi:defcfun "sp_free_port"
    :void
  (port-ptr :pointer))

(cffi:defcfun "sp_list_ports"
    sp-return
  (port-ptr-ptr-ptr :pointer))


(cffi:defcfun "sp_copy_port"
    sp-return
  (port-ptr :pointer)
  (portc-copy-ptr-ptr :pointer))


(cffi:defcfun "sp_free_port_list"
    :void
  (port-ptr-ptr :pointer))

(cffi:defcfun "sp_open" 
    sp-return
  (port-ptr :pointer)
  (flags sp-mode))

(cffi:defcfun "sp_close" 
    sp-return
  (port-ptr :pointer))

(cffi:defcfun "sp_get_port_name" 
    :string
  (port-ptr :pointer))

(cffi:defcfun "sp_get_port_description" 
    :string
  (port-ptr :pointer))

(cffi:defcfun "sp_get_port_transport" 
    sp-transport
  (port-ptr :pointer))


(cffi:defcfun "sp_get_port_usb_bus_address"
    sp-return
  (port-ptr :pointer)
  (usb-bus-ptr (:pointer :int))
  (usb-address-ptr (:pointer :int)))


(cffi:defcfun "sp_get_port_usb_vid_pid"
    sp-return
  (port-ptr :pointer)
  (usb-vid-ptr (:pointer :int))
  (usb-pid-ptr (:pointer :int)))


(cffi:defcfun "sp_get_port_usb_manufacturer"
    :string
  (port-ptr :pointer))


(cffi:defcfun "sp_get_port_usb_product"
    :string
  (port-ptr :pointer))

(cffi:defcfun "sp_get_port_usb_serial"
    :string
  (port-ptr :pointer))


(cffi:defcfun "sp_get_port_bluetooth_address"
    :string
  (port-ptr :pointer))

(cffi:defcfun "sp_get_port_handle"
    sp-return
  (port-ptr :pointer)
  (result-ptr (:pointer :void)))


(cffi:defcfun "sp_new_config"
    sp-return
  (port-config-ptr-ptr :pointer))


(cffi:defcfun "sp_free_config"
    :void
  (port-config-ptr :pointer))

(cffi:defcfun "sp_get_config"
    sp-return
  (port-ptr :pointer)
  (port-config-ptr :pointer))

(cffi:defcfun "sp_set_config"
    sp-return
  (port-ptr :pointer)
  (port-config-ptr :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cffi:defcfun "sp_set_baudrate"
    sp-return
  (port-ptr :pointer)
  (baudrate :int))
;;
(cffi:defcfun "sp_get_config_baudrate"
    sp-return
  (config-ptr :pointer)
  (baudrate-ptr (:pointer :int)))
;;
(cffi:defcfun "sp_set_config_baudrate"
    sp-return
  (config-ptr :pointer)
  (baudrate :int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cffi:defcfun "sp_set_bits"
    sp-return
  (port-ptr :pointer)
  (bits :int))
;;
(cffi:defcfun "sp_get_config_bits"
    sp-return
  (config-ptr :pointer)
  (bits-ptr (:pointer :int)))
;;
(cffi:defcfun "sp_set_config_bits"
    sp-return
  (config-ptr :pointer)
  (bits :int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cffi:defcfun "sp_set_parity"
    sp-return
  (port-ptr :pointer)
  (parity sp-parity))
;;
;; have to work to turn a pointer into an enum keyword
(cffi:defcfun ("sp_get_config_parity" %sp-get-config-parity)
    sp-return
  (config-ptr :pointer)
  (parity-ptr (:pointer sp-parity)))
;; this is a wrapper to convert a returned ENUM in a int[1] to the keyword
(defun sp-get-config-parity (config-ptr)
  (cffi:with-foreign-object (nptr 'sp-parity)
    (let* ((retval (%sp-get-config-parity config-ptr nptr))
	   (ekeyword (cffi:mem-ref nptr 'sp-parity)))
      (values retval ekeyword))))
;;
(cffi:defcfun "sp_set_config_parity"
    sp-return
  (config-ptr :pointer)
  (parity sp-parity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cffi:defcfun "sp_set_stopbits"
    sp-return
  (port-ptr :pointer)
  (stopbits :int))
;;
(cffi:defcfun ("sp_get_config_stopbits" %sp-get-config-stopbits)
    sp-return
  (config-ptr :pointer)
  (stopbits-ptr (:pointer :int)))
;;
(defun get-config-stopbits (config-ptr)
  (cffi:with-foreign-object (nptr :int)
    (let* ((retval (%sp-get-config-stopbits config-ptr nptr))
	   (nstopbits (cffi:mem-ref nptr :int)))
      (values retval nstopbits))))
;;
(cffi:defcfun "sp_set_config_stopbits"
    sp-return
  (config-ptr :pointer)
  (stopbits :int))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cffi:defcfun "sp_set_rts"
    sp-return
  (port-ptr :pointer)
  (rts sp-rts))
;;
(cffi:defcfun ("sp_get_config_rts" %sp-get-config-rts)
    sp-return
  (config-ptr :pointer)
  (rts-ptr (:pointer sp-rts)))
;;
(defun sp-get-config-rts (config-ptr)
  (cffi:with-foreign-object (nptr 'sp-rts)
    (let* ((retval (%sp-get-config-rts config-ptr nptr))
	   (ekeyword (cffi:mem-ref nptr 'sp-rts)))
      (values retval ekeyword))))
;;
(cffi:defcfun "sp_set_config_rts"
    sp-return
  (config-ptr :pointer)
  (rts sp-rts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cffi:defcfun "sp_set_cts"
    sp-return
  (port-ptr :pointer)
  (cts sp-cts))
;;
(cffi:defcfun ("sp_get_config_cts" %sp-get-config-cts)
    sp-return
  (config-ptr :pointer)
  (cts-ptr (:pointer sp-cts)))
;;
(defun sp-get-config-cts (config-ptr)
  (cffi:with-foreign-object (nptr 'sp-cts)
    (let* ((retval (%sp-get-config-cts config-ptr nptr))
	   (ekeyword (cffi:mem-ref nptr 'sp-cts)))
      (values retval ekeyword))))
;;
(cffi:defcfun "sp_set_config_cts"
    sp-return
  (config-ptr :pointer)
  (cts sp-cts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cffi:defcfun "sp_set_dtr"
    sp-return
  (port-ptr :pointer)
  (dtr sp-dtr))
;;
(cffi:defcfun ("sp_get_config_dtr" %sp-get-config-dtr)
    sp-return
  (config-ptr :pointer)
  (dtr-ptr (:pointer sp-dtr)))
;;
(defun sp-get-config-dtr (config-ptr)
  (cffi:with-foreign-object (nptr 'sp-dtr)
    (let* ((retval (%sp-get-config-dtr config-ptr nptr))
	   (ekeyword (cffi:mem-ref nptr 'sp-dtr)))
      (values retval ekeyword))))
;;
(cffi:defcfun "sp_set_config_dtr"
    sp-return
  (config-ptr :pointer)
  (dtr sp-dtr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cffi:defcfun "sp_set_dsr"
    sp-return
  (port-ptr :pointer)
  (dsr sp-dsr))
;;
(cffi:defcfun ("sp_get_config_dsr" %sp-get-config-dsr)
    sp-return
  (config-ptr :pointer)
  (dsr-ptr (:pointer sp-dsr)))
;;
(defun sp-get-config-dsr (config-ptr)
  (cffi:with-foreign-object (nptr 'sp-dsr)
    (let* ((retval (%sp-get-config-dsr config-ptr nptr))
	   (ekeyword (cffi:mem-ref nptr 'sp-dsr)))
      (values retval ekeyword))))
;;
(cffi:defcfun "sp_set_config_dsr"
    sp-return
  (config-ptr :pointer)
  (dsr sp-dsr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cffi:defcfun "sp_set_xon_xoff"
    sp-return
  (port-ptr :pointer)
  (xon-xoff sp-xonxoff))
;;
(cffi:defcfun ("sp_get_config_xon_xoff" %sp-get-config-xon-xoff)
    sp-return
  (config-ptr :pointer)
  (xon-xoff-ptr (:pointer sp-xonxoff)))
;;
(defun sp-get-config-xon-xoff (config-ptr)
  (cffi:with-foreign-object (nptr 'sp-xonxoff)
    (let* ((retval (%sp-get-config-xon-xoff config-ptr nptr))
	   (ekeyword (cffi:mem-ref nptr 'sp-xonxoff)))
      (values retval ekeyword))))
;;
(cffi:defcfun "sp_set_config_xon_xoff"
    sp-return
  (config-ptr :pointer)
  (xon-xoff sp-xonxoff))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cffi:defcfun "sp_set_flowcontrol"
    sp-return
  (port-ptr :pointer)
  (flowcontrol sp-flowcontrol))
;;
(cffi:defcfun ("sp_get_config_flowcontrol" %sp-get-config-flowcontrol)
    sp-return
  (config-ptr :pointer)
  (flowcontrol-ptr (:pointer sp-flowcontrol)))
;;
(defun sp-get-config-flowcontrol (config-ptr)
  (cffi:with-foreign-object (nptr 'sp-flowcontrol)
    (let* ((retval (%sp-get-config-flowcontrol config-ptr nptr))
	   (ekeyword (cffi:mem-ref nptr 'sp-flowcontrol)))
      (values retval ekeyword))))
;;
(cffi:defcfun "sp_set_config_flowcontrol"
    sp-return
  (config-ptr :pointer)
  (flowcontrol sp-flowcontrol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; as soon as any data is available
(cffi:defcfun "sp_blocking_read"
    sp-return
  (port-ptr :pointer)
  (buf (:pointer :void))
  (count size_t)
  (timeout_ms :uint))

;; until complete (count?)
(cffi:defcfun "sp_blocking_read_next"
    sp-return
  (port-ptr :pointer)
  (buf (:pointer :void))
  (count size_t)
  (timeout_ms :uint))


(cffi:defcfun "sp_nonblocking_read"
    sp-return
  (port-ptr :pointer)
  (buf (:pointer :void))
  (count size_t))


(cffi:defcfun "sp_blocking_write"
    sp-return
  (port-ptr :pointer)
  (buf (:pointer :void))
  (count size_t)
  (timeout_ms :uint))

(cffi:defcfun "sp_nonblocking_write"
    sp-return
  (port-ptr :pointer)
  (buf (:pointer :void))
  (count size_t))



(cffi:defcfun "sp_input_waiting"
    sp-return
  (port-ptr :pointer))

(cffi:defcfun "sp_output_waiting"
    sp-return
  (port-ptr :pointer))


(cffi:defcfun "sp_flush"
    sp-return
  (port-ptr :pointer)
  (buffers sp-buffer))

(cffi:defcfun "sp_drain"
    sp-return
  (port-ptr :pointer))


(cffi:defcfun "sp_new_event_set"
    sp-return
  (event-set-ptr-ptr :pointer))

(cffi:defcfun "sp_add_port_events"
    sp-return
  (event-set-ptr :pointer)
  (port-ptr :pointer)
  (mask sp-event))

(cffi:defcfun "sp_wait"
    sp-return
  (event-set-ptr :pointer)
  (timeout :int))


(cffi:defcfun "sp_free_event_set"
    :void
  (event-set-ptr :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cffi:defcfun ("sp_get_signals" %sp-get-signals)
    sp-return
  (port-ptr :pointer)
  (sp-signal-ptr (:pointer sp-signal)))
;;
(defun sp-get-signals (port-ptr)
  (cffi:with-foreign-object (nptr 'sp-signal)
    (let* ((retval (%sp-get-signals port-ptr nptr))
	   (keyword (cffi:mem-ref nptr 'sp-signal)))
      (values retval keyword))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cffi:defcfun "sp_start_break"
    sp-return
  (port-ptr :pointer))

(cffi:defcfun "sp_end_break"
    sp-return
  (port-ptr :pointer))


(cffi:defcfun "sp_last_error_code"
    :int)

(cffi:defcfun "sp_free_error_message"
    :void
  (message (:pointer :char)))

(cffi:defcfun ("sp_last_error_message" %sp-last-error-message)
    (:pointer :char)) ;; not :STRING because it needs to be freed
(defun sp-last-error-message ()
  (let* ((ptr (%sp-last-error-message))
	 (error-message (cffi:foreign-string-to-lisp ptr)))
    (sp-free-error-message ptr)
    error-message))


(cffi:defcfun "sp_set_debug_handler"
    :void
  (handler (:pointer :void))) ;; void (*handler)(const char *format, ...)

(cffi:defcfun "sp_default_debug_handler"
    :void
  (format-string :string)
  &rest)


(cffi:defcfun "sp_get_major_package_version" :int)
(cffi:defcfun "sp_get_minor_package_version" :int)
(cffi:defcfun "sp_get_package_version_string" :string)
(cffi:defcfun "sp_get_current_lib_version" :int)
(cffi:defcfun "sp_get_revision_lib_version" :int)
(cffi:defcfun "sp_get_age_lib_version" :int)
(cffi:defcfun "sp_get_lib_version_string" :string)


  































  



  
