-record(methods, {get, post, put, delete}).

-record(resource, {
          is_id,
          id,
          methods = #methods{},
          verbs = [],
          sub_resources = [],
          hop
         }).
