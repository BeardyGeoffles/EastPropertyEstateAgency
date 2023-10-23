       01 set-bit-pairs pic 9(2) comp-x value 1.
       01 user-key-control.
         03 user-key-setting pic 9(2) comp-x.
         03 filler pic x value "1".
         03 first-user-key pic 9(2) comp-x.
         03 number-of-keys pic 9(2) comp-x.
       01 key-status.
         03 key-type pic x.
         03 key-code-1 pic 9(2) comp-x.
         03 key-code-2 pic 9(2) comp-x.
