-record(resource, {
          is_id,
          id,
          converter,
          methods = #{},
          verbs = #{},
          sub_resources = [],
          ctp,
          cta,
          hop,
          init_handler,
          error_handler
         }).
