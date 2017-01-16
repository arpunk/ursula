#|
@doc
  ursula cluster API
@end
|#

(defmodule ursula-cluster
  (behaviour gen_server)

  ;; API
  (export (start_link 0)
          (add-nodes 0)
          (create 0)
          (create-ensembles 1)
          (update-ensembles 1))

  ;; gen_server callbacks
  (export (init 1)
          (handle_call 3)
          (handle_cast 2)
          (handle_info 2)
          (terminate 2)
          (code_change 3)))

;;; API

(defun add-nodes ()
  (gen_server:call (MODULE) 'add_nodes 20000))

(defun create ()
  (gen_server:call (MODULE) 'create 20000))

(defun update-ensembles (ensembles)
  (lc ((<- ensemble-id ensembles))
    (let* (((= `#(_ ,node) peer) (riak_ensemble_manager:get_leader ensemble-id))
           (pid (rpc:call node 'riak_ensemble_manager 'get_peer_id `(,ensemble-id ,peer))))
      (riak_ensemble_peer:update_members pid `(#(add #(,ensemble-id ,(node)))) 5000))))

(defun create-ensembles (ensembles)
  (lc ((<- ensemble-id ensembles))
    (riak_ensemble_manager:create_ensemble ensemble-id
                                           `#(,ensemble-id ,(node))
                                           'ursula-ensemble-backend
                                           '())))

(defun start_link ()
  (gen_server:start_link `#(local ,(MODULE)) (MODULE) '() '()))

;;; gen_server callbacks

(defun init (_)
  `#(ok ,#M()))

(defun handle_call
  (('add_nodes _from state)
   (progn (join-cluster (nodes))
          `#(reply ok ,state)))
  (('create _from state)
    (progn (riak_ensemble_manager:enable)
           (wait-stable)
           (create-ensembles (ensembles))
           `#(reply ok ,state)))
  ((_ _from state) `#(reply ok ,state)))

(defun handle_cast (message state)
  (error_logger:error_msg "unhandled msg: ~p~n" `(,message))
  `#(noreply ,state))

(defun handle_info (message state)
  (error_logger:error_msg "unhandled msg: ~p~n" `(,message))
  `#(noreply ,state))

(defun terminate (_reason _state)
  'ok)

(defun code_change (_old_vsn state _extra)
  `#(ok ,state))

;;; Internal functions

(defun wait-stable ()
  (case (check-stable)
    ('true 'ok)
    ('false (wait-stable))))

(defun check-stable ()
  (case (riak_ensemble_manager:check_quorum 'root 1000)
    ('true (case (riak_ensemble_peer:stable_views 'root 1000)
             (`#(ok true) 'true)
             (_ 'false)))
    ('false 'false)))

(defun join-cluster
  (((list)) 'ok)
  (((cons h t))
   (case (riak_ensemble_manager:join h (node))
     ('ok (progn (wait-stable)
                 (update-ensembles (ensembles))
                 'ok))
     ('already_enabled (progn (wait-stable)
                              (update-ensembles (ensembles))))
     (`#(error same_node) 'ok)
     (_ (if (=:= h (node))
          (join-cluster t)
          (join-cluster `(,h)))))))

(defun ensembles ()
  '(services routers))