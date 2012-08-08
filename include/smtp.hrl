-define(MAX_RECORDS, 10000).
-record(smtp_state,{
     user  = undefined,
     email = undefined,
     host  = undefined,
     from  = undefined,
     rcpt  = [],
     mail  = undefined}).
