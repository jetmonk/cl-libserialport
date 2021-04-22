
(defpackage libserialport
  (:use #:cl)
  (:export

   ;; libserialport.lisp
   
   #:sp-last-error-message
   #:sp-last-error-code

   #:serial-port #:serial-port-p #:serial-port-alive #:serial-port-baud
   #:serial-port-bits #:serial-port-stopbits #:serial-port-parity
   #:serial-port-rts #:serial-port-cts #:serial-port-dtr #:serial-port-dsr
   #:serial-port-xonxoff #:serial-port-flowcontrol #:serial-port-transport
   
   #:serial-port-description #:serial-port-description-p
   #:serial-port-description-name
   #:serial-port-description-description
   #:serial-port-description-transport
   #:serial-port-description-usb-bus
   #:serial-port-description-usb-address
   #:serial-port-description-usb-vendor-id
   #:serial-port-description-usb-product-id
   #:serial-port-description-usb-manufacturer
   #:serial-port-description-usb-product
   #:serial-port-description-usb-serial-number
   #:serial-port-description-bluetooth-mac-address

   #:list-serial-ports
   #:build-serial-port-description

   #:serial-port-alive-p
   #:shutdown-serial-port
   #:set-serial-port-qualities
   #:open-serial-port

   ;; libserialport-io.lisp

   #:serial-write-data
   #:serial-read-octets 
   #:serial-read-octet
   #:serial-read-char8
   #:serial-read-octets-until
   #:serial-read-line

   #:serial-input-waiting
   #:serial-output-waiting
   #:serial-flush-buffer
   #:serial-drain-buffer

   ;; event-set.lisp
   #:event-set #:event-set-p
   #:event-set-port-names
   #:event-set-alive-p
   #:create-event-set
   #:destroy-event-set
   #:add-serial-port-to-event-set
   
   

   ))
