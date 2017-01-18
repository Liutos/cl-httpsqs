(in-package #:cl-httpsqs)

(defclass <httpsqs-queue> ()
  ((auth
    :initarg :auth
    :reader queue-auth
    :type string)
   (host
    :initarg :host
    :reader queue-host
    :type string)
   (port
    :initarg :port
    :reader queue-port
    :type integer)))

(defun http-request (method name parameters queue &optional content)
  "Send HTTP request to QUEUE with CONTENT and PARAMETERS."
  (check-type content (or string null))
  (check-type method keyword)
  (check-type parameters list)
  (check-type queue <httpsqs-queue>)
  (let ((auth (queue-auth queue))
        (uri (make-queue-uri queue)))
    (when auth
      (push `("auth" . ,auth) parameters))
    (push `("name" . ,name) parameters)
    (let ((args (list :external-format-in :utf-8
                      :method method
                      :parameters parameters)))
      (when (and (eq method :post) content)
        (push content args)
        (push :content args))
      (apply #'drakma:http-request uri args))))

(defun make-queue-uri (queue)
  (check-type queue <httpsqs-queue>)
  (format nil "http://~A:~A/"
          (queue-host queue)
          (queue-port queue)))

;; export

(defun dequeue (name queue &optional (charset "utf-8"))
  "Returns a string dequeued from a httpsqs queue, or NIL when the queue is empty."
  (check-type name string)
  (check-type queue <httpsqs-queue>)
  (let ((parameters `(("charset" . ,charset)
                      ("name" . ,name)
                      ("opt" . "get"))))
    (multiple-value-bind (content code headers)
        (http-request :get parameters queue)
      (declare (ignorable code))
      (if (and (string= content "HTTPSQS_GET_END")
               (null (assoc :pos headers)))
          nil
          content))))

(defun enqueue (data name queue &optional (charset "utf-8"))
  "Append DATA into QUEUE named NAME."
  (check-type data string)
  (check-type name string)
  (check-type queue <httpsqs-queue>)
  (let ((parameters `(("charset" . ,charset)
                      ("opt" . "put"))))
    (http-request :post name parameters queue data)))

(defun fetch-json-status (name queue)
  "Returns a JSON string of the QUEUE's status."
  (check-type name string)
  (check-type queue <httpsqs-queue>)
  (let ((parameters `(("opt" . "status_json"))))
    (http-request :get name parameters queue)))

(defun make-queue (host port &optional auth)
  (make-instance '<httpsqs-queue>
                 :auth auth
                 :host host
                 :port port))

(defun print-status (name queue)
  "Display the current status of QUEUE."
  (check-type name string)
  (check-type queue <httpsqs-queue>)
  (let ((parameters `(("opt" . "status"))))
    (let ((status (http-request :get name parameters queue)))
      (print status))))

(defun reset (name queue)
  "Reset the QUEUE"
  (check-type name string)
  (check-type queue <httpsqs-queue>)
  (let ((parameters `(("opt" . "reset"))))
    (http-request :get name parameters queue)))

(defun set-max (name num queue)
  "Set the max number of elements in QUEUE."
  (check-type num integer)
  (check-type name string)
  (check-type queue <httpsqs-queue>)
  (let ((parameters `(("num" . ,(format nil "~A" num))
                      ("opt" . "maxqueue"))))
    (http-request :get name parameters queue)))

(defun set-sync-time (name queue seconds)
  "Set interval in seconds for auto flush of QUEUE."
  (check-type name string)
  (check-type queue <httpsqs-queue>)
  (check-type seconds integer)
  (let ((parameters `(("num" . ,(format nil "~A" seconds))
                      ("opt" . "maxqueue"))))
    (http-request :get name parameters queue)))

(defun view (name pos queue)
  "View the element at POS in QUEUE."
  (check-type name string)
  (check-type pos integer)
  (check-type queue <httpsqs-queue>)
  (let ((parameters `(("opt" . "view")
                      ("pos" . ,(format nil "~A" pos)))))
    (http-request :get name parameters queue)))
