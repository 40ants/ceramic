(defpackage ceramic
  (:use :cl)
  (:import-from :ceramic.driver
                :*driver*
                :driver-running)
  (:import-from :ceramic.setup
                :setup)
  (:import-from :ceramic.runtime
                :*releasep*)
  (:import-from :ceramic.crashreporter
                :start-crash-reporter)
  #+quicklisp
  (:import-from :ceramic.bundler
                :bundle)
  (:import-from :ceramic.resource
                :define-resources
                :resource-directory
                :resource)
  (:import-from :ceramic.window
                :window
                :window-id
                :make-window
                ;; Predicates
                :loadingp
                :crashedp
                ;; Accessors
                :title
                :url
                ;; Operations
                :show
                :hide
                :center
                :reload
                :stop-loading
                :back
                :forward
                :undo
                :redo
                :cut
                :copy
                :paste
                :select-all
                :unselect
                :open-dev-tools
                :close-dev-tools)
  (:import-from #:alexandria
                #:ensure-symbol)
  (:shadowing-import-from :ceramic.window
                          :close)
  (:export :window
           :window-id
           :make-window
           ;; Predicates
           :loadingp
           :crashedp
           ;; Accessors
           :title
           :url
           ;; Operations
           :show
           :hide
           :close
           :center
           :reload
           :stop-loading
           :back
           :forward
           :undo
           :redo
           :cut
           :copy
           :paste
           :select-all
           :unselect
           :open-dev-tools
           :close-dev-tools)
  (:export :start-crash-reporter)
  (:export :bundle)
  (:export :define-resources
           :resource-directory
           :resource)
  (:export :setup
           :start
           :stop
           :quit
           :define-entry-point)
  (:documentation "The main interface."))
(in-package :ceramic)

;;; FIXME move this someone more semantic

#-quicklisp
(defun bundle ()
  nil)

;;; Lifecycle

(defun start ()
  "Start the Electron process."

  (unless *driver*
    (setf *driver*
          (make-instance 'ceramic.driver:driver)))
  
  (ceramic.driver:start *driver*))

(defun stop ()
  "Stop the Electron process."
  (ceramic.driver:stop *driver*))

;;; Entry point for released applications

(defpackage ceramic-entry
  (:use :cl))

(defmacro define-entry-point (system-name () &body body)
  "Define the application's entry point."
  (let ((entry-point (ensure-symbol system-name
                                    :ceramic-entry)))
    `(defun ,entry-point (&key releasep)
       "Starts Ceramic application.

Set 'releasep' argument to nil if you start it from the REPL.
Otherwise, Ceramic will search Electron binary in the same
directory where executable file resides."
       
       (let ((*releasep* releasep)
             (swank-port (find-port:find-port :min 4005))
             (swank-started nil)
             (swank-package (find-package :swank)))
         
         (when swank-package
           ;; We'll start swank only of application was started not
           ;; from the slime's repl.
           (unless (ignore-errors
                    (funcall (intern "CONNECTION-INFO" swank-package)))
             
             (log:info "Starting swank server on port"
                       swank-port)
             
             (funcall (intern "CREATE-SERVER" swank-package)
                      :dont-close t
                      :port swank-port)
             (setf swank-started t)))

         ;; Start Ceramic and Electron
         (start)
         (handler-case
             (progn
               ,@body
               (loop while (driver-running *driver*)
                     do (sleep 1)))
           
           (t (condition)
             (log:info "Exception caught" condition)
             (when swank-started
               (funcall (intern "STOP-SERVER" swank-package)
                        swank-port))
             ;; (uiop:print-condition-backtrace condition)
             (log:info "Quitting")
             (quit)))))))
