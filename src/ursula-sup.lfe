#|
@doc
  ursula top level supervisor
@end
|#

(defmodule ursula-sup
  (behaviour supervisor)

  ;; API
  (export (start_link 0))

  ;; supervisor callbacks
  (export (init 1)))

;;; API

(defun start_link ()
  (supervisor:start_link
    `#(local ,(server-name)) (MODULE) '()))

;;; Supervisor callbacks

(defun init (_args)
  (let* ((data-root (application:get_env 'riak_ensemble 'data_root "./data"))
         (listen-port (application:get_env 'ursula 'http_port 8080))
         (ensemble `#(riak_ensemble_sup
                      #(riak_ensemble_sup start_link
                                          (,(filename:join data-root
                                                           (atom_to_list (node)))))
                      permanent 20000 supervisor (riak_ensemble_sup)))
         (cluster #(ursula-cluster
                    #(ursula-cluster start_link
                                     ())
                    permanent 20000 worker (ursula-cluster))))
    `#(ok #(#(one_for_one 0 1) (,ensemble ,cluster)))))

;;; Internal functions

(defun server-name ()
  'ursula-sup)
