(defpackage ceramic.setup
  (:use :cl)
  (:import-from :ceramic.log
                :log-message)
  (:import-from :ceramic.file
                :*ceramic-directory*
                :release-directory)
  (:import-from :ceramic.os
                :*operating-system*
                :*architecture*)
  (:import-from :electron-tools
                :app-directory
                :binary-pathname
                :get-release)
  (:import-from #:ceramic.os
                #:copy-directory)
  (:export :setup)
  (:documentation "Set up everything needed to develop with Ceramic."))
(in-package :ceramic.setup)

;;; Dealing with Electron releases

(defparameter +main-javascript+
  (asdf:system-relative-pathname :ceramic #p"src/main.js")
  "Pathname to the JavaScript file for the main process.")

(defparameter +ws-module+
  (asdf:system-relative-pathname :ceramic #p"node_modules/ws/"))

(defun clean-release (app-directory)
  "Clean up default files from an Electron release."
  (let ((app-files (list #p"main.js"
                         #p"default_app.js"
                         #p"index.html"
                         #p"package.json")))
    (loop for file in app-files do
      (let ((pathname (merge-pathnames file
                                       app-directory)))
        (when (probe-file pathname)
          (delete-file pathname))))))

(defun insert-javascript (app-directory)
  "Insert the main process JavaScript into an Electron release."
  (uiop:copy-file +main-javascript+
                  (merge-pathnames #p"main.js"
                                   app-directory)))

(defun insert-package-definition (app-directory)
  "Insert the package.json into an Electron release."
  (with-open-file (output-stream (merge-pathnames #p"package.json"
                                                  app-directory)
                                 :direction :output
                                 :if-does-not-exist :create)
    (write-string (format nil "{ \"name\": ~S, \"version\": ~S, \"main\": \"main.js\" }"
                          "Ceramic/Electron"
                          (asdf:component-version
                           (asdf:find-system :ceramic)))
                  output-stream)))

(defun copy-ws-module (app-directory)
  "Copy the WebSockets module."
  (copy-directory +ws-module+
                  (merge-pathnames #p"node_modules/ws/"
                                   app-directory)))

(defun prepare-release (&key release-directory
                          operating-system
                          app-directory)
  "Prepare an Electron release.
   app-directory is a final directory where Node.js part should be copied."
  (unless (or app-directory
              release-directory)
    (error "One of :app-directory or :release-directory should be specified"))
  
  (unless app-directory
    (setf app-directory
          (app-directory release-directory
                         :operating-system operating-system)))
  
  (ensure-directories-exist app-directory)
  (clean-release app-directory)
  (insert-javascript app-directory)
  (insert-package-definition app-directory)
  (copy-ws-module app-directory)
  (values))

;;; Main

(defparameter *electron-version* "1.2.7"
  "The version of Electron to use.")

(defun setup (&key force)
  "Set up everything needed to start developing."
  (log-message "Creating Ceramic directories...")
  (ensure-directories-exist (release-directory))
  (if (or (uiop:emptyp (uiop:directory-files (release-directory)))
          force)
      (progn
        (log-message "Downloading a copy of Electron...")
        (ensure-directories-exist (release-directory))
        (get-release (release-directory)
                     :operating-system *operating-system*
                     :architecture *architecture*
                     :version *electron-version*))
      (log-message "Already downloaded. Use :force t to force download."))
  (log-message "Preparing the files...")
  (prepare-release :release-directory (release-directory)
                   :operating-system *operating-system*)
  (values))
