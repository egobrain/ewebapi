-record(methods, {get, post, put, delete}).

-record(resource, {
          is_id,
          id,
          methods = #methods{},
          verbs = [],
          sub_resources = [],
          ctp,
          cta,
          hop,
          init_handler,
          error_handler
         }).
