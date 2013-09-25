%% @copyright 2013 Basho Technologies
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.

-module(webmachine_config).

-export([set_max_request_body_throwaway_bytes/1,
         get_max_request_body_throwaway_bytes/0]).

%% 64MB in bytes
-define(DEFAULT_MAX_REQUEST_BODY_THROWAWAY_BYTES, 67108864).

-spec set_max_request_body_throwaway_bytes(integer()) -> ok.
set_max_request_body_throwaway_bytes(NumBytes) when is_integer(NumBytes) ->
    application:set_env(webmachine, max_request_body_throwaway_bytes, NumBytes).

-spec get_max_request_body_throwaway_bytes() -> integer().
get_max_request_body_throwaway_bytes() ->
    application:get_env(webmachine, max_request_body_throwaway_bytes,
                        ?DEFAULT_MAX_REQUEST_BODY_THROWAWAY_BYTES).
