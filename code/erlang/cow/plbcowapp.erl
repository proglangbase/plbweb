-module(plbcowapp).
-behaviour(application).
-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/[...]", cowboy_static, {priv_dir, proglangbase, "", [
                {mimetypes, cow_mimetypes, all},
                {dir_handler, directory_h}
            ]}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
        env => #{dispatch => Dispatch},
        middlewares => [cowboy_router, directory_lister, cowboy_handler]
    }),
    plbcowsup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http).
