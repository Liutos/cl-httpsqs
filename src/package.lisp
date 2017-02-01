(in-package #:cl-user)

(defpackage #:cl-httpsqs
  (:use #:cl)
  (:export #:dequeue
           #:enqueue
           #:fetch-json-status
           #:make-queue
           #:print-status
           #:reset
           #:set-max
           #:set-sync-time
           #:view))
