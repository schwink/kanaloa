
%% @hidden
-record(kanaloa_settings, {handler,
			   http_content_type,
			   parse_jsonrpc,
			   batch_interval,
			   batch_check_interval,
			   batch_count,
			   connection_message_timeout,
			   message_size,
			   batch_size_cutoff
			  }).
