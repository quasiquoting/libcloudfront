;;; ====================================================== [ libcloudfront.lfe ]

(defmodule libcloudfront
  ;; API
  (export (credentials 3) (to_cookie 1) (to_params 1))
  ;; Config helper functions
  (export (get_env 1) (key_pair_id 1) (private_key 1))
  ;; Expiry functions
  (export (from_now 1) (from_now 2)))

(include-lib "lfe/include/clj.lfe")

;;; ==================================================================== [ API ]

(defun credentials (resource expiry args)
  (let ((raw-policy (policy resource expiry)))
    `[#(policy      ,(safe-base64 raw-policy))
      #(signature   ,(sign raw-policy (private_key args)))
      #(key_pair_id ,(key_pair_id args))]))

(defun to_cookie (cred)
  (lists:map
    (match-lambda
      ([`#(policy      ,value)] `#(#"CloudFront-Policy"      ,value))
      ([`#(signature   ,value)] `#(#"CloudFront-Signature"   ,value))
      ([`#(key_pair_id ,value)] `#(#"CloudFront-Key-Pair-Id" ,value)))
    cred))

(defun to_params (cred)
  (-> (match-lambda
        ([`#(policy      ,value)] `#(#"Policy"      ,value))
        ([`#(signature   ,value)] `#(#"Signature"   ,value))
        ([`#(key_pair_id ,value)] `#(#"Key-Pair-Id" ,value)))
      (lists:map cred)))

;;; ================================================ [ Config helper functions ]

(defun get_env (app)
  "Given an `app` name, return a property list with keys,
  `` 'key_pair_id `` and `` 'private_key ``.

  If either are missing, throw an error.

  `key_pair_id` must be present in `elli_cloudfront`'s env and `private_key`
  is the contents `{{key_pair_id}}.key` in `app`'s `priv` directory.

  If the `.key` file cannot be found, throw an error."
  (->> (application:get_env (MODULE) 'key_pair_id 'undefined)
       (tuple 'key_pair_id) (list)
       (get_env (priv_dir app))))

(defun key_pair_id (args)
  "Given a proplist of `args`, return the `` 'key_pair_id ``.

  If it is missing, throw `#(error #(missing key_pair_id))`."
  (case (proplists:get_value 'key_pair_id args)
    ('undefined (error #(missing key_pair_id) (list args)))
    (value      (assert-binary 'key_pair_id value))))

(defun private_key (args)
  "Given a proplist of `args`, return the `` 'private_key ``.

  If it is missing, throw `#(error #(missing private_key))`."
  (case (proplists:get_value 'private_key args)
    ('undefined (error #(missing private_key) (list args)))
    (value      (->> value (assert-binary 'private_key)))))

;;; ======================================================= [ Expiry functions ]

(defun from_now
  "Return the the number of seconds the Unix epoch `n` `unit`s from now."
  ([0 _unit]   (now))
  ([1 'day]    (from_now 1 'days))
  ([1 'hour]   (from_now 1 'hours))
  ([1 'minute] (from_now 1 'minutes))
  ([1 'second] (from_now 1 'seconds))
  ([n 'days] (when (is_integer n) (> n 0))
   (from_now (days n)))
  ([n 'hours] (when (is_integer n) (> n 0))
   (from_now (hours n)))
  ([n 'minutes] (when (is_integer n) (> n 0))
   (from_now (minutes n)))
  ([n 'seconds] (when (is_integer n) (> n 0))
   (from_now n))
  ([n unit] (error 'invalid_time_diff (list n unit))))

(defun from_now
  "Equivalent to [[from_now/2]] but with a single argument, `` `#(,n ,unit) ``.

  [[from_now/1]] will also accept a non-negative integer, `n`,
  and treat it as `` `#(,n seconds) ``."
  ;; n seconds
  ([0] (now))
  ([n] (when (is_integer n) (> n 0))
   (+ n (now)))
  ;; `#(,n ,unit)
  ([`#(0 ,_unit)] (now))
  ([#(1 day)]     (from_now #(1 days)))
  ([#(1 hour)]    (from_now #(1 hours)))
  ([#(1 minute)]  (from_now #(1 minutes)))
  ([#(1 second)]  (from_now #(1 seconds)))
  ([`#(,n days)]    (when (is_integer n) (> n 0)) (from_now (days n)))
  ([`#(,n hours)]   (when (is_integer n) (> n 0)) (from_now (hours n)))
  ([`#(,n minutes)] (when (is_integer n) (> n 0)) (from_now (minutes n)))
  ([`#(,n seconds)] (when (is_integer n) (> n 0)) (from_now n))
  ([x] (error 'invalid_time_diff (list x))))


;;; ===================================================== [ Internal functions ]

(defun policy
  ([url epoch-time] (when (integer? epoch-time))
   (let* ((condition `[#(#"DateLessThan" [#(#"AWS:EpochTime" ,epoch-time)])])
          (statement `[[#(#"Resource" ,url) #(#"Condition" ,condition)]]))
     (jsx:encode `[#(#"Statement" ,statement)])))
  ([url expiry]
   (policy url (from_now expiry))))

(defun sign (data pem-bin)
  (->> (pem->key pem-bin)
       (public_key:sign data 'sha)
       (safe-base64)))

(defun pem->key (pem-bin)
  (let ((`[,rsa-entry] (public_key:pem_decode pem-bin)))
    (public_key:pem_entry_decode rsa-entry)))

(defun safe-base64 (data)
  (fold-replace (base64:encode data)
    '[#("\\+" "-") #("=" "_") #("/" "~")]))

(defun get_env (priv-dir args)
  (let ((`#(ok ,private_key) (->> (-> (key_pair_id args) ; Validate key_pair_id
                                      (binary_to_list)
                                      (++ ".key"))
                                  (filename:join priv-dir)
                                  (file:read_file))))
    (doto (lists:keystore 'private_key 1 args `#(private_key ,private_key))
      (private_key))))                  ; Validate private_key

(defun priv_dir (app)
  (case (code:priv_dir app)
    (#(error bad_name) (error 'bad_name (list app)))
    (priv_dir          priv_dir)))

(defun now ()
  "Return the number of seconds since the Unix epoch."
  (erlang:system_time 'seconds))

(defun days
  "Return the number of seconds in `n` days."
  ([0]     0)
  ([1] 86400)
  ([n] (when (is_integer n) (> n 0))
   (* n (days 1))))

(defun hours
  "Return the number of seconds in `n` hours."
  ([0]    0)
  ([1] 3600)
  ([n] (when (is_integer n) (> n 0))
   (* n (hours 1))))

(defun minutes
  "Return the number of seconds in `n` minutes."
  ([0]  0)
  ([1] 60)
  ([n] (when (is_integer n) (> n 0))
   (* n (minutes 1))))

(defun assert-binary (k v) (if (is_binary v) v (error 'non_binary (list k v))))

(defun fold-replace (string pairs) (fold-replace string pairs 'binary))

(defun fold-replace (string pairs return-type)
  (lists:foldl
    (match-lambda
      ([`#(,patt ,replacement) acc]
       (re:replace acc patt replacement `[global #(return ,return-type)])))
    string pairs))

;;; ==================================================================== [ EOF ]
