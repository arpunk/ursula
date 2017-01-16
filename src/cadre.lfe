#|
@doc
  ursula cadre API
@end
|#

(defmodule cadre

  ;; API
  (export (register 3)
          (find 1)))

(include-lib "dns/include/dns_records.hrl")

;;; API

(defun register (service-id node port)
  (cadre-storage:insert 'services `#(,service-id ,node ,port)))

(defun find (service-id)
  (cadre-storage:select 'services service-id))