#|
@doc
  ursula public API
@end
|#

(defmodule ursula-app
  (behaviour application)

  ;; Application callbacks
  (export (start 2)
          (stop 1)))

(include-lib "dns/include/dns_records.hrl")

;;; API

(defun start (_type _args)
  (initialize-domain)
  (ursula-sup:start_link))

(defun stop (_state)
  'ok)

;;; Internal functions

(defun initialize-domain ()
  (let* ((`#(ok ,domain) (application:get_env 'ursula 'domain))
         (`#(ok ,cluster) (application:get_env 'ursula 'cluster))
         (zone (binary "service."
                       ((list_to_binary cluster) binary)
                       "."
                       ((list_to_binary domain) binary)))
         (soa (make-dns_rrdata_soa mname (binary "ns1." (zone binary))
                                   rname (binary "admin." (zone binary))
                                   serial 2013022001
                                   refresh 86400
                                   retry 7200
                                   expire 604800
                                   minimum 300)))
    (erldns_zone_cache:put_zone `#(,zone () (,(make-dns_rr name zone
                                                           type (DNS_TYPE_SOA)
                                                           ttl 3600
                                                           data soa))))))