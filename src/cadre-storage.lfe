#|
@doc
   cadre storage backend
@end
|#

(defmodule cadre-storage

  ;; API
  (export (insert 2)
          (delete 2)
          (select 2)))

;;; API

(defun insert (table value)
  (case (riak_ensemble_client:kover (node) table (element 1 value) value 10000)
    (`#(ok ,_) 'ok)
    (err (progn
           (error_logger:error_msg "error=~p~n" `(,err))
           `#(error ,err)))))

(defun delete (table key)
  (case (riak_ensemble_client:kover (node) table key 'notfound 10000)
    (`#(ok ,_) 'ok)
    (err (progn
           (error_logger:error_msg "error=~p~n" `(,err))
           `#(error ,err)))))

(defun select (table key)
  (case (riak_ensemble_client:kget (node) table key 10000)
    (`#(ok ,obj) (ursula-ensemble-backend:obj_value obj))
    (err (progn
           (error_logger:error_msg "error=~p~n" `(,err))
           'notfound))))