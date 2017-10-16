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
-module(nkprometheus).
-export([register_metric/3,
         register_metric/4,
         record_value/3,
         record_value/4]).

register_metric(gauge, Name, Help) ->
    prometheus_gauge:new([{name, Name}, {help, Help}]);

register_metric(_, _, _) ->
    {error, invalid_type}.

register_metric(gauge, Name, Labels, Help) ->
    prometheus_gauge:new([{name, Name}, {labels, Labels}, {help, Help}]);

register_metric(_, _, _, _) ->
    {error, invalid_type}.

record_value(gauge, Name, Value) ->
    prometheus_gauge:set(Name, Value);

record_value(_, _, _) ->
    {error, invalid_type}.

record_value(gauge, Name, Labels, Value) ->
    prometheus_gauge:set(Name, Labels, Value);

record_value(_, _, _, _) ->
    {error, invalid_type}.
