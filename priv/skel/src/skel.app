{application, skel,
 [{description, "skel"},
  {vsn, "reftrans"},
  {modules, [
    skel,
    skel_app,
    skel_sup,
    skel_deps,
    skel_resource
  ]},
  {registered, []},
  {mod, {skel_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
