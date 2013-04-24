
-record(resource, {host, type, name, attrs, event}).
-type resource() :: #resource{}.

-record(hostevent, {host, event}).
-type hostevent() :: #hostevent{}.
