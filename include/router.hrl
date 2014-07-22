-record(ewebapi_router, {
          prefix :: [binary()],
          ctp,
          cta,
          resources = [] :: [{binary(), ewebapi:resource()}],
          init_handler,
          error_handler
         }).
