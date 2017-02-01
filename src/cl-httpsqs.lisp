(in-package #:cl-httpsqs)

(defclass <httpsqs> ()
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

(define-condition httpsqs-error (error)
  ())

(define-condition httpsqs-auth-failed (httpsqs-error)
  ()
  (:report (lambda (c stream)
             (declare (ignorable c))
             (format stream "Invalid authentication"))))

(define-condition httpsqs-enqueue-error (httpsqs-error)
  ()
  (:report (lambda (c stream)
             (declare (ignorable c))
             (format stream "An error occured when enqueueing"))))

(define-condition httpsqs-reset-error (httpsqs-error)
  ()
  (:report (lambda (c stream)
             (declare (ignorable c))
             (format stream "An error occured when reseting"))))

(defun http-request (method name parameters queue &optional content)
  "Send HTTP request to QUEUE with CONTENT and PARAMETERS."
  (check-type content (or string null))
  (check-type method keyword)
  (check-type parameters list)
  (check-type queue <httpsqs>)
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
      (let* ((values (multiple-value-list (apply #'drakma:http-request uri args)))
             (response (first values)))
        (when (string= response "HTTPSQS_AUTH_FAILED")
          (error 'httpsqs-auth-failed))
        (when (string= response "HTTPSQS_ERROR")
          (error 'httpsqs-error))
        (apply #'values values)))))

(defun make-queue-uri (queue)
  (check-type queue <httpsqs>)
  (format nil "http://~A:~A/"
          (queue-host queue)
          (queue-port queue)))

;; export

(defun dequeue (name queue &optional (charset "utf-8"))
  "Returns a string dequeued from a httpsqs queue, or NIL when the queue is empty."
  (check-type name string)
  (check-type queue <httpsqs>)
  (let ((parameters `(("charset" . ,charset)
                      ("opt" . "get"))))
    (multiple-value-bind (content code headers)
        (http-request :get name parameters queue)
      (declare (ignorable code))
      (if (and (string= content "HTTPSQS_GET_END")
               (null (assoc :pos headers)))
          nil
          content))))

(defun enqueue (data name queue &optional (charset "utf-8"))
  "Append DATA into QUEUE named NAME."
  (check-type data string)
  (check-type name string)
  (check-type queue <httpsqs>)
  (let ((parameters `(("charset" . ,charset)
                      ("opt" . "put"))))
    (let ((response (http-request :post name parameters queue data)))
      (when (string= response "HTTPSQS_PUT_ERROR")
        (error 'httpsqs-enqueue-error))
      response)))

(defun fetch-json-status (name queue)
  "Returns a JSON string of the QUEUE's status."
  (check-type name string)
  (check-type queue <httpsqs>)
  (let ((parameters `(("opt" . "status_json"))))
    (http-request :get name parameters queue)))

(defun make-queue (host port &optional auth)
  (make-instance '<httpsqs>
                 :auth auth
                 :host host
                 :port port))

(defun print-status (name queue)
  "Display the current status of QUEUE."
  (check-type name string)
  (check-type queue <httpsqs>)
  (let ((parameters `(("opt" . "status"))))
    (let ((status (http-request :get name parameters queue)))
      (print status))))

(defun reset (name queue)
  "Reset the QUEUE."
  (check-type name string)
  (check-type queue <httpsqs>)
  (let ((parameters `(("opt" . "reset"))))
    (let ((response (http-request :get name parameters queue)))
      (when (string= response "HTTPSQS_RESET_ERROR")
        (error 'httpsqs-reset-error))
      response)))

(defun set-max (name num queue)
  "Set the max number of elements in QUEUE."
  (check-type num integer)
  (check-type name string)
  (check-type queue <httpsqs>)
  (let ((parameters `(("num" . ,(format nil "~A" num))
                      ("opt" . "maxqueue"))))
    (http-request :get name parameters queue)))

(defun set-sync-time (name queue seconds)
  "Set interval in seconds for auto flush of QUEUE."
  (check-type name string)
  (check-type queue <httpsqs>)
  (check-type seconds integer)
  (let ((parameters `(("num" . ,(format nil "~A" seconds))
                      ("opt" . "maxqueue"))))
    (http-request :get name parameters queue)))

(defun view (name pos queue)
  "View the element at POS in QUEUE."
  (check-type name string)
  (check-type pos integer)
  (check-type queue <httpsqs>)
  (let ((parameters `(("opt" . "view")
                      ("pos" . ,(format nil "~A" pos)))))
    (http-request :get name parameters queue)))
