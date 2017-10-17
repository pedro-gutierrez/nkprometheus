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
-define(SRV, nkprometheus).

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

make_service_spec() ->
    #{ id => ?SRV, 
       callback => nkprometheus_callbacks,
       rest_url => rest_url(nkprometheus_app:get(listen_ip, <<"0.0.0.0">>),
                            nkprometheus_app:get(listen_port, 8081),
                            nkprometheus_app:get(listen_path, <<"/metrics">>),
                            nkprometheus_app:get(listen_secure, false)),
       debug => []}.

rest_url(Host, Port, Path, Secure) ->
    BinPort = nklib_util:to_binary(Port),
    Http1 = case Secure of true -> <<"https">>; false -> <<"http">> end,
    <<Http1/binary, "://", Host/binary, ":", BinPort/binary, Path/binary>>.
