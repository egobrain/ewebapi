-record(ewebapi_router, {
          prefix :: [binary()],
          resources = [] :: [{binary(), ewebapi:resource()}]
         }).
