{application, test_app,
 [{description, "test_app"},
  {vsn, "0.01"},
  {modules, [
    test_app,
    test_app_app,
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
  {mod, {test_app_app, []}},
  {env, [
    {port, 8001}
  ]},
  {applications, [kernel, stdlib, crypto]}]}.
