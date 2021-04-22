
cl-libserialport - a Common Lisp interface to the libserialport C library.

libserialport is a "is a minimal, cross-platform shared library
written in C that is intended to take care of the OS-specific details
when writing software that uses serial ports."

See https://sigrok.org/wiki/Libserialport

It is nice because it is easy to use, cross-platform, and provides
straightforward access to traditional, bluetooth, and USB serial
devices.

Depends on: BABEL, CFFI

This Lisp package provides an interface to all exported functions
of libserialport.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DOCUMENTATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

All functions have documentation strings.  Qualities are described
by keywords in libserialport-ffi.lisp, taken from original C enums.

;; Lisp object, containing a pointer to the low level
;;   serial port object
SERIAL-PORT 

;; Lisp object describing a serial port, created by LIST-SERIAL-PORTS
;; and BUILD-SERIAL-PORT-DESCRIPTION
SERIAL-PORT-DESCRIPTION 


;; list serial ports on a computer, as SERIAL-PORT-DESCRIPTION objects
(LIST-SERIAL-PORTS)  

;; create a SERIAL-PORT-DESCRIPTION  from a SERIAL-PORT
(BUILD-SERIAL-PORT-DESCRIPTION SERIAL-PORT) 

;;  shut down open serial port
(SHUTDOWN-SERIAL-PORT SERIAL-PORT) 

;;  open  a named serial port, with various qualities given by keywords.
(OPEN-SERIAL-PORT "/dev/tty.serial"
   &KEY
  :MODE     :SP-MODE-READ-WRITE (etc)
  :BAUD     XXXX
  :BITS     1
  :PARITY   :SP-PARITY-NONE (etc)
  :STOPBITS 1
  :XONXOFF  :SP-XONXOFF-DISABLED
  :FLOWCONTROL
  :RTS rts :CTS cts :DTR dtr :DSR dsr);; see keywords in libserialport-ffi.lisp
  r 


(SET-SERIAL-PORT-QUALITIES :SERIAL-PORT :BAUD ...) - set a serial port's
    qualities


;; Write DATA (an octet, octet-vector, or string)
;; Returns the number of octets written.
(SERIAL-WRITE-DATA SERIAL-PORT DATA
  :ENCODING :LATIN-1 ;; Babel encoding for ougoing octets, if DATA is a string
  :BLOCKING NIL ;; use blocking IO with timeout
  :TIMEOUT 0    ;; milliseconds, if BLOCKING is true; 0 means forever
  :LINE-END NIL) ;; or :cr, :lf, :crlf if we want to end this as a line


;; Read a vector of octets of length COUNT, or a single
;; bare octet if COUNT=-1
;; Returns (VALUES OCTETS NUM-OCTETS-READ)
(SERIAL-READ-OCTETS SERIAL-PORT COUNT
  :BLOCKING NIL
  :TIMEOUT 1000
  :OCTET-BUF NIL ;; optional (unsigned-byte 8) vector with fill-pointer
  :APPEND NIL)   ;; append at fill pointer if true


;; read one octet, or NIL if timeout
(SERIAL-READ-OCTET SERIAL-PORT
  :BLOCKING NIL
  :TIMEOUT 1000)

;; read one char between 0-255
(SERIAL-READ-CHAR8 SERIAL-PORT
  :BLOCKING NIL
  :TIMEOUT 1000)

;; read octets until it hits FINAL-OCTET, which is not returned
(SERIAL-READ-OCTETS-UNTIL SERIAL-PORT FINAL-OCTAT
  :BLOCKING NIL
  :TIMEOUT 1000
  :MAX-LENGTH 65536
  :OCTET-BUF NIL)  ;; optional (unsigned-byte 8) vector with fill-pointer

;; read a line (string), converting octets to chars using Babel
(SERIAL-READ-LINE SERIAL-PORT
  :BLOCKING NIL
  :TIMEOUT 1000
  :MAX-LENGTH 65536 ;; octets, not chars
  :LINE-TERMINATION-CHAR #\Linefeed
  :IGNORE-FINAL-CARRIAGE-RETURN T ;; ignore #\cr before #\lf
  :ENCODING :LATIN-1) ;; Babel encoding

;; return number of bytes waiting on serial port
(SERIAL-INPUT-WAITING SERIAL-PORT)
(SERIAL-OUTPUT-WAITING SERIAL-PORT)


;; flush buffers, dumping their data
(SERIAL-FLUSH-BUFFER SERIAL-PORT
   :FLUSH-INPUT-BUFFER T
   :FLUSH-OUTPUT-BUFFER T)

;; wait until buffers drain - no timeout possible
(SERIAL-DRAIN SERIAL-PORT)


;; lisp object representing an event-set, an obect attached to one
;; or more SERIAL-PORTS, waiting for an event (select mechanism)
EVENT-SET

;; make a new event set
(CREATE-EVENT-SET)

;; delete and unallocate an event set
(DESTROY-EVENT-SET EVENT-SET)

;; add a serial port to an event set, so the event set can wait
;; on it
(ADD-SERIAL-PORT-TO-EVENT-SET SERIAL-PORT EVENT-SET
  :RX-READY T    ;; wait for incoming data
  :TX-READY T    ;; wait for ready to write
  :ERROR    T)   ;; wait for error

;; wait on an event set, returning after timeout, or after one
;; of the connect serial ports has a pending event.  Then
;;  SERIAL-INPUT-WAITING, SERIAL-OUTPUT-WAITING can be applied to
;;  the SERIAL-PORTS
(EVENT-WAIT EVENT-SET :TIMEOUT TIMEOUT)
