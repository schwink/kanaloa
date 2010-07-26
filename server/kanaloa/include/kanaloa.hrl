%% @author Stephen Schwink <thruster@schwink.net>
%% @copyright 2010 Stephen Schwink.

%% @type kanaloa_settings() = {Junction::junction_id(), Message::binary()}
%% @hidden
-record(kanaloa_settings, {handler, http_content_type, parse_jsonrpc, batch_interval, batch_check_interval, batch_count}).
