
-record(pmap, {key_t  :: aeb_aevm_data:type(),
               val_t  :: aeb_aevm_data:type(),
               parent :: none | non_neg_integer(),
               size   = 0 :: non_neg_integer(),
               data   :: #{aeb_heap:binary_value() => aeb_heap:binary_value() | tombstone}
                       | stored}).

-record(maps, { maps    = #{} :: #{ non_neg_integer() => #pmap{} }
              , next_id = 0   :: non_neg_integer() }).

-record(heap, { maps   :: #maps{},
                offset :: aeb_heap:offset(),
                heap   :: binary() | #{non_neg_integer() => non_neg_integer()} }).

