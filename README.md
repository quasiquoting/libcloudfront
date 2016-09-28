# libcloudfront

[![Build Status][Travis badge]][Travis link]

[Travis badge]: https://travis-ci.org/quasiquoting/libcloudfront.svg?branch=master
[Travis link]: https://travis-ci.org/quasiquoting/libcloudfront

*CloudFront Cookie/URL signing library.*


## Use

*my_app/config/sys.config*:

```erlang
{my_app, [{key_pair_id, <<"AKEXAMPLE123">>}]}.
```

*my_app/priv/AKEXAMPLE123.key*:

```
-----BEGIN RSA PRIVATE KEY-----
...
-----END RSA PRIVATE KEY-----
```

```erlang
%% application:start(my_app),
KeyPairId = <<"AKEXAMPLE123">>,

Resource = <<"http://example.com/path/to/resource">>,
%% Resource = <<"http://example.com/path/*">>,
%% Can have wildcards according to CloudFront spec

Expiry = {2, days}, % from now
%% ... alternatively,
%% EpochTime = 1475039777,

%% Must include {key_pair_id, KeyPairId} where KeyPairId is a binary
%% and <<"priv/" KeyPairId ".key">> exists is and is the key file.
Args = libcloudfront:get_env(my_app),

Credentials = libcloudfront:credentials(Resource, Expiry, Args),
%% Credentials are a proplist with the following keys:
[
 {policy, Policy},
 {signature, Signature},
 {key_pair_id, KeyPairId}
] = Credentials,

%% For signed cookies
CookieKeyVals = libcloudfront:to_cookie(Credentials),

%% For signed URLs
QueryParams = libcloudfront:to_params(Credentials).
```

```erlang
Expiry   :: {0,   _Unit}
          | {1,    Singular}
          | {2..,  Plural},
Singular :: day  | hour  | minute  | second.
Plural   :: days | hours | minutes | seconds.
```


## Build

```fish
rebar3 compile
```


## Test

```fish
rebar3 eunit -m libcloudfront-tests
```
