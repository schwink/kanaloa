{application, kanaloa_test_app,
 [{description, "kanaloa_test_app"},
  {vsn, "0.01"},
  {modules, [
    kanaloa_test_app,
    kanaloa_test_app_app,
    kanaloa,
    kanaloa_connection,
    kanaloa_rpc,
    kanaloa_state_server,
    kanaloa_sup,
    kanaloa_utils,
    kanaloa_web,
    mochiweb_http
  ]},
  {registered, []},
  {mod, {kanaloa_test_app_app, []}},
  {env, [
    {port, 8001}
  ]},
  {applications, [kernel, stdlib, crypto]}]}.
