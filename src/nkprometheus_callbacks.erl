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
-module(nkprometheus_callbacks).
-export([plugin_deps/0, nkservice_rest_http/3]).

plugin_deps() -> [nkservice_rest].

nkservice_rest_http(get, Path, Req) ->
    lager:debug("NkPrometheus Rest handler Path: ~p, Req: ~p", [Path, Req]),
    {http, 200, [{<<"content-type">>, <<"text/plain">>}], <<"nkprometheus test">>};

nkservice_rest_http(_Method, _Path, _Req) ->
    continue.
