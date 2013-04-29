-record(hostevent, {host, event}).
-type hostevent() :: #hostevent{}.

-define(SS_QUEUE, "/topic/rd_host_startup_shutdown").