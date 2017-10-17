%% -------------------------------------------------------------------
%%
%% Copyright (c) 2017 Carlos Gonzalez Florido.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(nkprometheus_svc).
-export([start_services/1]).

start_services(_) ->
    Exporter = make_service_spec(),
    start_exporter(Exporter).

start_exporter(#{ id := SrvId } = Spec) ->
    case nkservice:start(SrvId, Spec) of
        {ok, _} ->
            lager:info("Started prometheus exporter ~p", [SrvId]),
            ok;
        {error, already_started} ->
            ok;
        {error, Error} ->
            lager:warning("Error starting prometheus exporter ~p: ~p", [SrvId, Error]),
            error(service_start_error)
    end.

app_config(Key) ->
    case application:get_env(nkprometheus, Key) of
        {ok, Value} -> Value;
        _ -> undefined
    end.

make_service_spec() ->
    Config = #{ listen_path => app_config(listen_ip),
                listen_port => app_config(listen_port),
                listen_path => app_config(listen_path),
                listen_secure => app_config(listen_secure) },

    io:format("config: ~p~n", [Config]),
    case nkprometheus_util:parse_exporter(Config) of 
        {ok, Exporter} ->
            #{ id => nkprometheus,
               callback => nkprometheus_callbacks,
               rest_url => rest_url(Exporter),
               debug => []};
        {error, Error} ->
            lager:warning("Invalid prometheus exporter configuration: ~p", [Error]),
            error(invalid_service_syntax)
    end.
    
rest_url(#{ listen_ip := Host, 
            listen_port := Port, 
            listen_path := Path, 
            listen_secure := Secure}) ->
    BinPort = nklib_util:to_binary(Port),
    Http1 = case Secure of true -> <<"https">>; false -> <<"http">> end,
    <<Http1/binary, "://", Host/binary, ":", BinPort/binary, Path/binary>>.
