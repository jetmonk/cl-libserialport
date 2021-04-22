
;; functions to read/write

(in-package libserialport)

(defun serial-write-data (serial-port data
				      &key
					(encoding :latin-1) ;; for string only
					(blocking nil)
					(timeout 1000)
					(line-end nil))

  "Write DATA to SERIAL-PORT.  DATA can be a string, unsigned byte8 vector, a 
number from 0-255, or a character.   Unicode data are translated to octets
using babel package, using ENCODING.

The number of octets written is returned.

BLOCKING indicates whether blocking output should be used.  

TIMEOUT is the timeout in milliseconds if BLOCKING is true.  LINE-END
is a line termination to append; it can be NIL (none),:CR,:LF,:CRLF"
				      
  (declare (type serial-port serial-port)
	   (type (member nil :cr :lf :crlf) line-end)
	   (type (unsigned-byte 24) timeout)
	   (type (or string
		     (unsigned-byte 8)
		     character
		     (simple-array (unsigned-byte 8) (*)))
		 data))
  
  (assert (serial-port-alive-p serial-port))
  
  (let* ((bytes ;; a byte array, if we're writing a sequence
	   (cond
	     ((stringp data)
	      (babel:string-to-octets data :encoding encoding))
	     ((typep data '(simple-array (unsigned-byte 8) (*))) ;; unsigned byte 8
	      data)
	     ;; out of range char so turn into encoded string
	     ((and (characterp data) 
		   (> (char-code data) 255))
	      (babel:string-to-octets
	       (make-string 1 :initial-element data)
	       :encoding encoding))
	     (t ;; an integer or char<255
	      nil)))
	 ;; just one byte
	 (one-byte (if (not bytes)
		       (if (integerp data)
			   data
			   (char-code data))))
	 ;; number of bytes for line-end
	 (nend (cond ((not line-end) 0)
		     ((eq line-end :crlf) 2)
		     (t 1))) ;; :CR or :LF
	 (nbytes
	   (+ nend
	      (cond (bytes (length bytes))
		    (t 1))))) ;; a single char or ingeter

    ;; set up a foreign BUF with one more data bytes
    (with-serial-port-buffer (buf serial-port  nbytes)
      (let ((nlast 0))
	(cond
	  (bytes ;; case of a sequence of data
	   (loop for i of-type fixnum below (length bytes)
		 do
		    (incf nlast)
		    (setf (cffi:mem-ref buf :unsigned-char i) (aref bytes i))))
	  ;; 
	  (one-byte ;; else a single dataum [0-255]
	   (incf nlast)
	   (setf (cffi:mem-ref buf :unsigned-char 0) one-byte)))
	
	;; add a line termination if requested
	(cond ((eq line-end :cr)
	       (setf (cffi:mem-ref buf :unsigned-char nlast) #.(char-code #\cr)))
	      ((eq line-end :lf)
	       (setf (cffi:mem-ref buf :unsigned-char nlast) #.(char-code #\lf)))
	      ((eq line-end :crlf)
	       (setf (cffi:mem-ref buf :unsigned-char nlast) #.(char-code #\cr))
	       (incf nlast)
	       (setf (cffi:mem-ref buf :unsigned-char nlast) #.(char-code #\lf)))))
      ;;
      (cond
	(blocking
	 (let ((retval
		 (sp-blocking-write (serial-port-port-ptr serial-port) buf nbytes timeout)))
	   (when (symbolp retval) ;; an error code
	     (error "ERROR ~A blocking writing bytes to serial port." retval))
	   retval)) ;; number of bytes written
	;;
	((not blocking)
	 (let ((retval (sp-nonblocking-write (serial-port-port-ptr serial-port) buf nbytes)))
	   (when (symbolp retval) ;; an error code
	     (error "ERROR ~A non-blocking writing bytes to serial port." retval))
	   retval))))))  ;; number of bytes written
	   
      



(defun serial-read-octets (serial-port count
			   &key
			     (blocking nil)
			     (timeout 1000)
			     (octet-buf nil)
			     (append nil))
  "Read at most COUNT octets from SERIAL-PORT.   

BLOCKING indicates blocking or no-blocking output.
TIMEOUT is the timeout in milliseconds for blocking mode (0 means no timeout)
OCTET-BUF is an optional output array.  If given, it must be an adjustable
   (unsigned-byte 8) array with a fill pointer.  
APPEND means to append the new output to the end of OCTET-BUF at the
  fill pointer, instead of starting with an empty vector.

The special value of COUNT=-1 means to return a single octet or NIL, instead
of a vector. 

Returns (VALUES OCTETS NUM-OCTETS-READ)."

  (declare (type serial-port serial-port)
	   (type (or (member -1) unsigned-byte) count)
	   (type (or null (array (unsigned-byte 8) (*))) octet-buf))

  (when (zerop count) (error "Cannot read zero octets."))
  (when octet-buf
    (when (not (and (adjustable-array-p octet-buf)
		    (array-has-fill-pointer-p octet-buf)))
      (error "OCTET-BUF is not a (unsigned-byte 8) array that is adjustable and has a fill pointer."))
    (when (not append)
      (setf (fill-pointer octet-buf) 0)))
  
  ;; allocate foreign and Lisp storage, trying to use buffers inside
  ;; serial-port
  (block retblock
    (with-serial-port-buffer (fbuf serial-port count)
      (let ((acount (abs count))
	    (nbytes-read nil))
	
	(cond (blocking
	       (let ((retval
		       (sp-blocking-read
			(serial-port-port-ptr serial-port)
			fbuf acount timeout)))
		 (when (and (symbolp retval) ;; an error code
			    (not (eq retval :sp-ok)))
		   (error "ERROR ~A blocking readingbytes to serial port."
			  retval))
		 (setf nbytes-read retval)))
	      ((not blocking)
	       (let ((retval
		       (sp-nonblocking-read
			(serial-port-port-ptr serial-port)
			fbuf acount)))
		 (when (and (symbolp retval) ;; an error code
			    (not (eq retval :sp-ok)))
		   (error "ERROR ~A non-blocking reading bytes to serial port."
			  retval))
		 (setf nbytes-read retval))))

	;; zero-byte successful read
	(if (eq nbytes-read :sp-ok)
	    (setf nbytes-read 0))
	
	
	(if (= count -1)
	    ;; special case of count=1 ==> (values nil-or-char nbytes-read)
	    (return-from retblock
	      (values
	       (if (plusp nbytes-read)  (cffi:mem-ref fbuf :unsigned-char 0) nil)
	       nbytes-read))
	    ;; else grow the actual output buf
	    (let ((obuf 
		    (or
		     ;; use the passed buffer if given
		     octet-buf
		     ;; otherwise allocate a new vector 
		     (make-array 0 :element-type '(unsigned-byte 8)
				   :adjustable t
				   :fill-pointer 0))))
	      
	      (loop for i of-type fixnum below nbytes-read
		    do (vector-push-extend
			(cffi:mem-ref fbuf :unsigned-char i)
			obuf))
	      (return-from retblock (values fbuf nbytes-read))))))))



(defun serial-read-octet (serial-port &key (blocking nil) (timeout 1000))
  "Read one octet from SERIAL-PORT, or NIL if none."
  (multiple-value-bind (octet nbytes)
      (serial-read-octets serial-port -1 :blocking blocking :timeout timeout)
    (values octet nbytes)))


(defun serial-read-char8 (serial-port &key (blocking nil) (timeout 1000))
  "Read one char (with char-code < 255) from SERIAL-PORT, or NIL if none."
  (let ((byte (serial-read-octet serial-port :blocking blocking :timeout timeout)))
    (when byte (code-char byte))))


(defun serial-read-octets-until (serial-port final-octet
				 &key
				   (blocking nil)
				   (timeout 1000)
				   (max-length 65536)
				   (octet-buf nil))
  "Read a vector of octets from SERIAL-PORT until FINAL-OCTET is read,
returning an octet array, without the final octet.  MAX-LENGTH is the
maximum permitted length.

Returns (VALUES OCTET-VECTOR FINAL-OCTET-READ).  If FINAL-OCTET-READ
is not FINAL-OCTET the it will be NIL, which probably means the
operation timed out.

The TIMEOUT is the individual octet read timeout, not the total timeout.

OCTET-BUF is an optional output array.  If given, it must be an adjustable
   (unsigned-byte 8) array with a fill pointer."

  (when octet-buf
    (when (not (and (adjustable-array-p octet-buf)
		    (typep octet-buf '(array (unsigned-byte 8) (*)))
		    (array-has-fill-pointer-p octet-buf)))
      (error "OCTET-BUF is not a (unsigned-byte 8) array that is adjutable and has a fill pointer.")))
  
  (let ((ovec (or octet-buf
		  (make-array 32 :element-type '(unsigned-byte 8)
				 :adjustable t :fill-pointer 0))))
    (loop with nbytes-read = 0
	  for byte = (serial-read-octet serial-port
					:blocking blocking :timeout timeout)
	  do (cond ((or (eql byte final-octet) (eql byte nil))
		    (return (values ovec byte)))
		   (t
		    (incf nbytes-read)
		    (when (> nbytes-read max-length)
		      (error "Number of bytes read exceeds MAX-LENGTH=~A" max-length))
		    (vector-push-extend byte ovec))))))

(defun serial-read-line (serial-port
			 &key
			   (blocking nil)
			   (timeout 1000)
			   (max-length 65536)
			   (line-termination-char #\lf)
			   (ignore-final-carriage-return t)
			   (encoding :latin-1))
  "Read a line from SERIAL-PORT.  MAX-LENGTH is the maxinum number of
octets (not chars), and the line ends with LINE-TERMINATION-CHAR (by
default #\linefeed), and by default a final #\Return char is stripped
to allow DOS lines to be read.

TIMEOUT is the timeout of an individual octet read, not the entire
call.

Returns (VALUES STRING FINISHED-LINE-P DECODING-ERROR OCTETS) where
FINISHED-LINE-P is true if LINE-TERMINATION-CHAR was reached;
otherwise there may have been a timeout.  STRING can be NIL if there
is an error in Babel decoding the string using the encoding given, in
which case DECODING-ERROR is non-NIL.  OCTETS is the vector of raw
octets, possibly minus the #\Return."
  (multiple-value-bind (ovec final-byte)
      (serial-read-octets-until serial-port (char-code line-termination-char)
				:blocking blocking :timeout timeout :max-length max-length)
    ;; get rid of final #\Return
    (when (and ignore-final-carriage-return
	       (plusp (length ovec))
	       (= (aref ovec (1- (length ovec))) #.(char-code #\cr)))
      (vector-pop ovec))
    ;;
    (multiple-value-bind (string decoding-error)
	(babel:octets-to-string ovec :encoding encoding)
      (values string
	      (eql final-byte (char-code line-termination-char))
	      decoding-error
	      ovec))))



(defun serial-input-waiting (serial-port)
  "Returns the number of input octets waiting in the SERIAL-PORT.
Can also return a non-negtive number or a keyword indicating an error."
  (assert (serial-port-alive-p serial-port))
  (sp-input-waiting (serial-port-port-ptr serial-port)))
  

(defun serial-output-waiting (serial-port)
  "Returns the number of input octets waiting in the SERIAL-PORT.
Can also return a non-negtive number or a keyword indicating an error."
  (assert (serial-port-alive-p serial-port))
  (sp-output-waiting (serial-port-port-ptr serial-port)))


(defun serial-flush-buffer (serial-port &key (flush-input-buffer t) (flush-output-buffer t))
  "Flush serial port buffers, discarding data."
  (assert (serial-port-alive-p serial-port))
  (when (or flush-input-buffer flush-output-buffer)
    (sp-flush (serial-port-port-ptr serial-port)
	      (cond ((and flush-input-buffer flush-output-buffer)
		     :sp-buf-both)
		    (flush-input-buffer
		     :sp-buf-input)
		    (flush-output-buffer
		     :sp-buf-output)))))

(defun serial-drain-buffer (serial-port)
  "Wait until output of SERIAL-PORT have been transmited - no timeout option"
  (assert (serial-port-alive-p serial-port))
  (sp-drain (serial-port-port-ptr serial-port)))

  
		    
	

  
  
  
  
