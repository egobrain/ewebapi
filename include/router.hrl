-record(ewebapi_router, {
          prefix :: [binary()],
          ctp,
          cta,
          resources = [] :: [{binary(), ewebapi:resource()}]
         }).
