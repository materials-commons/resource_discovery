
-record(resource, {host, type, name, attrs}).
-type resource() :: #resource{}.

-record(hostevent, {host, event}).
-type hostevent() :: #hostevent{}.
