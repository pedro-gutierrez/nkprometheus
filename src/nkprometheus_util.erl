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
-module(nkprometheus_util).
-export([exporter_syntax/0, 
        parse_exporter/1]).

exporter_syntax() ->
    #{ listen_ip => host,
       listen_port => {integer, 1, 65535},
       listen_path => basepath,
       listen_secure => boolean,
       '__defaults' => #{
         listen_ip => <<"127.0.0.1">>,
         listen_port => 8081,
         listen_path => <<"/metrics">>,
         listen_secure => false 
        }
     }.

parse_exporter(Data) ->
    case nklib_syntax:parse(Data, exporter_syntax()) of
        {ok, Exporter, _} ->
            {ok, Exporter};
        {error, Error} ->
            {error, Error}
    end.
