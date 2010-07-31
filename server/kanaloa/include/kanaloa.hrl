
%% @type kanaloa_connection_state() = {Owner::pid(), Guid::binary(), Pending::[iolist()]}
-record(kanaloa_connection_state, {owner, id, pending=[]}).

%% @hidden
-record(kanaloa_settings, {handler, http_content_type, parse_jsonrpc, batch_interval, batch_check_interval, batch_count, connection_message_timeout}).
