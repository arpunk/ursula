#|
@doc
  ursula ensemble backend
@end
|#

(defmodule ursula-ensemble-backend
  (behaviour riak_ensemble_backend)

  ;; API
  (export (init 3)
          (new_obj 4)
          (obj_epoch 1)
          (obj_seq 1)
          (obj_key 1)
          (obj_value 1))
  (export (set_obj_epoch 2)
          (set_obj_seq 2)
          (set_obj_value 2))
  (export (get 3)
          (put 4)
          (tick 5)
          (ping 2)
          (ready_to_start 0))
  (export (synctree_path 2)
          (handle_down 4)))

(include-lib "riak_ensemble_ng/include/riak_ensemble_types.hrl")

(defrecord obj
  (epoch (epoch))
  (seq (seq))
  (key (term))
  (value (term)))

(defrecord state
  savefile
  id
  tid)

;;; API

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init
  ((ensemble id '())
   (let* (((binary (hash integer (size 160))) (make-sha ensemble id))
          (name (integer_to_list hash))
          (`#(ok ,root) (application:get_env 'riak_ensemble 'data_root))
          (file (filename:join `(,root "ensembles" ,(++ name "_kv"))))
          (tid (reload-data file)))
     (make-state savefile file
                 tid tid
                 id id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun new_obj (epoch seq key value)
  (make-obj epoch epoch
            seq seq
            key key
            value value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun obj_epoch (obj)
  (obj-epoch obj))

(defun obj_seq (obj)
  (obj-seq obj))

(defun obj_key (obj)
  (obj-key obj))

(defun obj_value (obj)
  (obj-value obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set_obj_epoch (epoch obj)
  (set-obj-epoch obj epoch))

(defun set_obj_seq (seq obj)
  (set-obj-seq obj seq))

(defun set_obj_value (value obj)
  (set-obj-value obj value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get
  ((key from (= (match-state tid tid) state))
   (let ((reply (case (ets:lookup tid key)
                  ([`#(,key ,value)] value)
                  ([] 'notfound))))
     (riak_ensemble_backend:reply from reply)
     state)))

(defun put
  ((key obj from (= (match-state tid tid savefile file) state))
   (ets:insert tid `#(,key ,obj))
   (save-data file tid)
   (riak_ensemble_backend:reply from obj)
   state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tick (_epoch _seq _leader _views state)
  state)

(defun ping (_from state)
  `#(ok ,state))

(defun ready_to_start ()
  'true)

(defun synctree_path (_ensemble _id)
  'default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handle_down (_ref _pid _reason _state)
  'false)

;;; Internal functions

(defun reload-data (file)
  (case (load-saved-data file)
    (`#(ok ,tid) tid)
    ('not_found (ets:new 'cadre `[ordered_set
                                  private
                                  #(read_concurrency true)]))))

(defun load-saved-data (file)
  (case (filelib:is_regular file)
    ('true (ets:file2tab file `[#(verify true)]))
    (_ 'not_found)))

(defun save-data (file tid)
  (let (('ok (filelib:ensure_dir file)))
    (ets:tab2file tid file `[#(extended_info [md5sum object_count])
                             #(sync true)])))

(defun make-sha (ensemble id)
  (riak_ensemble_util:sha (term_to_binary `#(,ensemble ,id))))