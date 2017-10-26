(in-package :cl-user)
(defpackage ceramic.log
  (:use :cl)
  (:export :*logging*
           :*logging-stream*
           :log-message)
  (:documentation "Logging."))
(in-package :ceramic.log)

(defvar *logging* t
  "Whether or not to log info.")

(defvar *logging-stream* *standard-output*
  "The logging stream.")

(defun log-message (message &rest args)
  (log:debug message args))
