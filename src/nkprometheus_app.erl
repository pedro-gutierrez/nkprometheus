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
-module(nkprometheus_app).
-behaviour(application).
-export([start/2, stop/1]).
-export([get/1, get/2, get_srv/2, put/2]).
-define(APP, nkprometheus).

start(_StartType, _StartArgs) ->
    Syntax = nkprometheus_util:endpoints_syntax(),
    case nklib_config:load_env(?APP, Syntax) of
        {ok, _} ->
            {ok, Pid} = nkprometheus_sup:start_link(),
            {ok, Vsn} = application:get_key(?APP, vsn),
            lager:info("NkPrometheus v~s has started.", [Vsn]),
            {ok, Pid};
        {error, Error} ->
            lager:error("Error parsing config: ~p", [Error]),
            error(Error)
    end.

stop(_State) ->
    ok.


get(Key) ->
    nklib_config:get(?APP, Key).

get(Key, Default) ->
    nklib_config:get(?APP, Key, Default).

get_srv(Class, Key) ->
    nklib_config:get_domain(?APP, Class, Key).

put(Key, Val) ->
    nklib_config:put(?APP, Key, Val).

