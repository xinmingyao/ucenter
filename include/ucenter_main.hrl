-define(LOGIN_REQ,1).
-define(LOGIN_REP,2).
-define(CREATE_SHARE_REQ,3).
-define(CREATE_REP,4).
-define(GET_SHARES_REQ,5).
-define(GET_SHARES_REP,6).

-record(user,{name,pwd}).
-record(share,{name,ip,port,type}).


