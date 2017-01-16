#|
@doc
  ursula DNS delegate resolver
@end
|#

(defmodule ursula-delegate
  (behaviour erldns_resolver)

  ;; resolver callbacks
  (export (get_records_by_name 1)))

(include-lib "dns/include/dns_records.hrl")

;;; API

(defun get_records_by_name (qname)
  (let ((service-id (get-service-id qname)))
    (case (cadre:find service-id)
      (`#(,_ ,target ,port) `[,(make-dns_rr name qname
                                            type (DNS_TYPE_SRV)
                                            ttl 0
                                            data (make-dns_rrdata_srv priority 1
                                                                      weight 1
                                                                      port port
                                                                      target target))])
      ('notfound '[]))))

;;; Internal functions

(defun get-service-id (qname)
  (let* ((`#(ok ,domain0) (application:get_env 'ursula 'domain))
         (`#(ok ,cluster0) (application:get_env 'ursula 'cluster))
         (domain (list_to_binary domain0))
         (cluster (list_to_binary cluster0))
         (`[,service-id ,#"service" ,_ ,_] (binary:split qname #"." `(global))))
    service-id))