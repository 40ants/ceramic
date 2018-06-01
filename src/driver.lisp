(in-package :cl-user)
(defpackage ceramic.driver
  (:use :cl)
  (:import-from :alexandria
                :if-let
                :assoc-value)
  (:import-from :ceramic.log
                :*logging*
                :log-message)
  (:import-from :ceramic.runtime
                :*releasep*
                :executable-relative-pathname)
  (:import-from :ceramic.file
                :release-directory)
  (:import-from :ceramic.os
                :*operating-system*)
  (:import-from :electron-tools
                :app-directory
                :binary-pathname)
  (:export :driver
           :*driver*
           :start
           :stop
           :js
   :sync-js
   :driver-running)
  (:documentation "The Ceramic driver interface."))
(in-package :ceramic.driver)

;;; Classes

(defclass driver ()
  ((process :accessor driver-process
            :documentation "The Electron process.")
   (context :accessor driver-context
            :type remote-js:buffered-context
            :documentation "The remote-js object.")
   (js-lock :accessor driver-js-lock
            :initform (bt:make-lock "ceramic-js-sync")
            :documentation "A lock object for js sync.")
   (js-cond :accessor driver-js-cond
            :initform (bt:make-condition-variable)
            :documentation "A condition variable for js sync.")
   (responses :accessor driver-responses
              :initform (make-hash-table :test #'equal)
              :type hash-table
              :documentation "A hash table of message IDs to evaluation results.")
   (running-p :reader driver-running
              :initform nil
              :documentation "A flag which is true when Electron app is running and nil when it is stopped."))
  (:documentation "The Ceramic driver."))

;; TODO: remove this comment if everything find 
;; Driver should not be created at the top level,
;; because in this case, port for websocket will not
;; be selected automatically when bundled application
;; started
(defvar *driver* (make-instance 'driver)
  "The global driver object.")

;;; Interface

(defgeneric start (driver)
  (:documentation "Start the Electron process and the remote-js server.")

  (:method ((driver driver))
    (log-message "Starting server...")
    (start-remote-js driver)
    (log-message "Starting Electron process...")
    (start-electron driver)
    (log-message "Waiting for startup...")
    (wait-for-client driver)
    (log-message "Electron started")
    
    (setf (slot-value driver 'running-p)
          t)))

(defgeneric stop (driver)
  (:documentation "Stop the Electron process and the remote-js server.")

  (:method ((driver driver))
    (log-message "Stopping Electron process...")
    (stop-electron driver)
    (log-message "Stopping server...")
    (stop-remote-js driver)

    (setf (slot-value driver 'running-p)
          nil)))

(defgeneric js (driver js)
  (:documentation "Evaluate a string of JavaScript in the Electron process.")

  (:method ((driver driver) js)
    (declare (type string js))
    (log:info "Sending js to browser"
              js)
    (remote-js:eval (driver-context driver) js)))

(defgeneric sync-js (driver js)
  (:documentation "Synchronously evaluate JavaScript in the Electron process,
  returning a string containing the result of the evaluation.")

  ;; TODO разобраться как обрабатывать события от электрона
  (:method ((driver driver) js)
    (let* ((message-id (uuid:format-as-urn nil (uuid:make-v4-uuid)))
           (full-js (format nil "Ceramic.syncEval(~S, (function() { ~A }))"
                            message-id
                            js)))
      ;; Send the message
      (js driver full-js)

      (log:info "Waiting for response from the browser")

      (with-slots (responses js-lock js-cond) driver
        (bt:with-lock-held (js-lock)
          (loop
            (multiple-value-bind (response found)
                (gethash message-id responses)
              (if found
                  ;; We got a reply
                  (progn
                    (log:info "Message from JS process was received"
                              message-id
                              response)
                    (remhash message-id responses)
                    (return-from sync-js response))
                  ;; Not yet
                  (bt:condition-wait js-cond js-lock)))))))))

(defgeneric port (driver)
  (:documentation "Return the port the WebSockets server is running on.")

  (:method ((driver driver))
    (remote-js:context-port (driver-context driver))))

(defgeneric address (driver)
  (:documentation "Return the address the WebSockets server is running on.")

  (:method ((driver driver))
    (remote-js:context-address (driver-context driver))))

;;; IPC

(defgeneric on-message (driver message)
  (:documentation "Receive a message from a WebSockets client.")

  (:method ((driver driver) message)
    (declare (type string message))
    (log:info "Message from JavaScript was received" message)
    
    (let* ((data (cl-json:decode-json-from-string message))
           (type-str (assoc-value data :type))
           (type (intern (string-upcase type-str)
                         'keyword)))
      (on-message-type driver type data))))

(defgeneric on-message-type (driver type message)
  (:documentation "Processes decoded message of particular type.")

  (:method ((driver driver) type message)
    (log:error "Unknown message type"
               type
               message)))

(defmethod on-message-type ((driver driver)
                            (type (eql :response))
                            message)
  (log:info "Processing response from JavaScript."
            message)
  
  (with-slots (responses js-lock js-cond) driver
    (bt:with-lock-held (js-lock)
      (setf (gethash (assoc-value message :id) responses)
            (assoc-value message :result))
      (bt:condition-notify js-cond))))


(defmethod on-message-type ((driver driver)
                            (type (eql :quit))
                            message)
  (log:info "Stopping because all windows were closed.")
  (stop driver))

;;; Internals

(defmethod start-electron ((driver driver))
  "Start the Electron process."
  (log:info "Trying to log with log4cl" *releasep*)
  
  (let* ((directory (if *releasep*
                        (executable-relative-pathname #p"./")
                        (release-directory)))
         (app-directory
           (app-directory directory
                          :operating-system ceramic.os:*operating-system*))
         (path (binary-pathname directory
                                :operating-system ceramic.os:*operating-system*))
         (args (list app-directory
                     (address driver)
                     (write-to-string (port driver)))))
    
    (with-slots (process) driver

      (log:info "Starting Electron"
                path
                args
                directory
                app-directory
                *logging*
                *releasep*)

      (setf process
            (external-program:start path
                                    args
                                    :output (when *logging*
                                              *standard-output*)
                                    :error :output))
      (multiple-value-bind (status code)
          (external-program:process-status process)
        (sleep 5)
        (log:info "Program was started" status code)
        (when (and (not (eql status
                             :running))
                   (not (zerop code)))
          (error "Program ~A exited with ~A code."
                 path
                 code)))))
  (values))

(defmethod stop-electron ((driver driver))
  "Stop the Electron process."
  (with-slots (process) driver
    (handler-case
        (js driver "Ceramic.quit()")
      (t ()
        (warn "Error quitting the Electron process. Forcing shutdown...")
        (external-program:signal-process process :killed))))
  (values))

(defmethod start-remote-js ((driver driver))
  "Start the remote-js server."
  (with-slots (context) driver
    (setf context
          (remote-js:make-buffered-context :address "localhost"
                                           :callback
                                           #'(lambda (message)
                                               (on-message driver message))))
    (remote-js:start context))
  (values))

(defmethod wait-for-client ((driver driver))
  "Wait for the client to connect to the WebSockets server."
  (loop until (remote-js:context-connected-p (driver-context driver))))

(defmethod stop-remote-js ((driver driver))
  "Stop the remote-js server."
  (with-slots (context) driver
    (remote-js:stop context))
  (slot-makunbound driver 'context)
  (values))
