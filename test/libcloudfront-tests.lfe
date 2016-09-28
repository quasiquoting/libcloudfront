(defmodule libcloudfront-tests)

(include-lib "ltest/include/ltest-macros.lfe")

(deftest example
  (ensure-loaded 'libcloudfront)
  (let* ((resource    #"http://example.com/*")
         (epoch-time  1475039777)
         (args        (libcloudfront:get_env 'libcloudfront))
         (policy      #b("eyJTdGF0ZW1lbnQiOlt7IlJlc291cmNlIjoiaHR0cDovL2V4YW1"
                         "wbGUuY29tLyoiLCJDb25kaXRpb24iOnsiRGF0ZUxlc3NUaGFuIj"
                         "p7IkFXUzpFcG9jaFRpbWUiOjE0NzUwMzk3Nzd9fX1dfQ__"))
         (signature   #b("KefD2qnlTxJL5yN5ehy81s0EWJGZAPPLhmTjoIFdMoIU2a3gAGK"
                         "kcNnRfwDRj5DwbZf~Kq2W8mch0tTmcVo1~XV6SmVPjIJU05T418"
                         "PRjuNxhU1I-3D1ExYC9WdNeTIUb6aj~NG3nArNf2ViadiN8M1io"
                         "4wiHSjJEGrDPEzry6dcOlbhUnA~jp7BEoKpiDWEhXFmuoQqkBJ3"
                         "sxouu3r0z2sokcPfmB0djCIyP3CipKmA96XeJ7FZv0WqileNeZX"
                         "4AhBV853Z6mPV0DQLpN8GnLYUQERs2Jz~G4CKwy97g3IHURKZs~"
                         "5KQr-gb7U9A2lb856WBg4oCtK0wKRQmCHdUg__"))
         (key_pair_id #"AKEXAMPLE123"))
    (let ((credentials (libcloudfront:credentials resource epoch-time args)))
      (value-equal policy      'policy      credentials)
      (value-equal signature   'signature   credentials)
      (value-equal key_pair_id 'key_pair_id credentials)
      (let ((cookie (libcloudfront:to_cookie credentials)))
        (value-equal policy      #"CloudFront-Policy"      cookie)
        (value-equal signature   #"CloudFront-Signature"   cookie)
        (value-equal key_pair_id #"CloudFront-Key-Pair-Id" cookie))
      (let ((params (libcloudfront:to_params credentials)))
        (value-equal policy      #"Policy"      params)
        (value-equal signature   #"Signature"   params)
        (value-equal key_pair_id #"Key-Pair-Id" params)))))

(defun ensure-loaded (app)
  (orelse (lists:keymember app 1 (application:loaded_applications))
          (let (('ok (application:load app)))))
  'ok)

(defun value-equal (expected key list)
  (is-equal expected (proplists:get_value key list)))
