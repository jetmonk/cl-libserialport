

(asdf:defsystem libserialport
  :depends-on (cffi babel)
  :components
  ((:file "libserialport-package")
   (:file  "libserialport-ffi" :depends-on ("libserialport-package"))
   (:file  "libserialport"
    :depends-on ("libserialport-package" "libserialport-ffi"))
   (:file  "libserialport-io"
    :depends-on ("libserialport-package" "libserialport-ffi"))
   (:file  "event-sets"
    :depends-on ("libserialport-package" "libserialport-ffi"
		 "libserialport"))
   
   ))
