# CL-HTTPSQS - A client library for accessing HTTPSQS

## Overview

[HTTPSQS](http://zyan.cc/httpsqs/) is a HTTP-based message queue, and this lib is used to interact with it.f

## Usage

Assume the HTTPSQS is started and listened on port 1218 at localhost, after loading this lib, the following functions could be used

### CL-HTTPSQS:MAKE-QUEUE

This is a function and it's used to create an instance of class `CL-HTTPSQS:<HTTPSQS>`

```lisp
(defvar *queue* (cl-httpsqs:make-queue "127.0.0.1" 1218))
```

### CL-HTTPSQS:ENQUEUE

After creating the instance, it's able to push an item into the queue by means of the function `CL-HTTPSQS:ENQUEUE`

```lisp
(cl-httpsqs:enqueue "Hello, world!" "test" *queue*)
```

### CL-HTTPSQS:PRINT-STATUS

To see the current status of a queue, use the `CL-HTTPSQS:PRINT-STATUS`

```lisp
(cl-httpsqs:print-status "test" *queue*)
```

### CL-HTTPSQS:DEQUEUE

Now there is one item in the queue, and it's able to ask the queue to output it

```lisp
(cl-httpsqs:dequeue "test" *queue*)
```
