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

(defun http-request (method parameters queue &optional content)
  "Send HTTP request to QUEUE with CONTENT and PARAMETERS."
  (when content
    (check-type content string))
  (check-type method keyword)
  (check-type parameters list)
  (check-type queue <httpsqs-queue>)
  (let ((auth (queue-auth queue))
        (uri (make-queue-uri queue)))
    (when auth
      (push `("auth" . ,auth) parameters))
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

(defun dequeue (name queue)
  "Returns a string dequeued from a httpsqs queue, or NIL when the queue is empty."
  (check-type name string)
  (check-type queue <httpsqs-queue>)
  (let ((parameters `(("name" . ,name)
                      ("opt" . "get"))))
    (multiple-value-bind (content code headers)
        (http-request :get parameters queue)
      (declare (ignorable code))
      (if (and (string= content "HTTPSQS_GET_END")
               (null (assoc :pos headers)))
          nil
          content))))

(defun enqueue (data name queue)
  "Append DATA into QUEUE named NAME."
  (check-type data string)
  (check-type name string)
  (check-type queue <httpsqs-queue>)
  (let ((parameters `(("name" . ,name)
                      ("opt" . "put"))))
    (http-request :post parameters queue data)))

(defun fetch-json-status (name queue)
  "Returns a JSON string of the QUEUE's status."
  (check-type name string)
  (check-type queue <httpsqs-queue>)
  (let ((parameters `(("name" . ,name)
                      ("opt" . "status_json"))))
    (http-request :get parameters queue)))

(defun make-queue (host port &optional auth)
  (make-instance '<httpsqs-queue>
                 :auth auth
                 :host host
                 :port port))

(defun print-status (name queue)
  "Display the current status of QUEUE."
  (check-type name string)
  (check-type queue <httpsqs-queue>)
  (let ((parameters `(("name" . ,name)
                      ("opt" . "status"))))
    (let ((status (http-request :get parameters queue)))
      (print status))))