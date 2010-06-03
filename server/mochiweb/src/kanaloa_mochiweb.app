{application, kanaloa_mochiweb,
 [{description, "kanaloa_mochiweb"},
  {vsn, "0.04"},
  {modules, [
    kanaloa_mochiweb,
    kanaloa_mochiweb_app,
    kanaloa_mochiweb_sup,
    kanaloa_mochiweb_web,
    kanaloa_mochiweb_deps
  ]},
  {registered, []},
  {mod, {kanaloa_mochiweb_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
