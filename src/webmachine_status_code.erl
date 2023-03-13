%% Wrapper around Webmachine allowing the response status code to be
%% specified as a bare numerical code, or as a tuple of
%% `{NumericalCode, ReasonPhrase}`. This is to support use of
%% extension status codes not defined in RFC 2616.

-module(webmachine_status_code).

-export([status_code/1, reason_phrase/1]).

-type status_code() :: pos_integer().
-type reason_phrase() :: string().
-type status_code_with_phrase() ::
        {pos_integer(), reason_phrase() | undefined}.
-type status_code_optional_phrase() ::
        status_code() | status_code_with_phrase().

-export_type(
    [status_code_optional_phrase/0, status_code_with_phrase/0, status_code/0]).

%% Extract just the numerical code of the status.
-spec status_code(status_code_optional_phrase()) -> status_code().
status_code({Code, _}) when is_integer(Code) ->
    Code;
status_code(Code) when is_integer(Code) ->
    Code.

%% Get the phrase to be included with the status code.
%%
%% This function abstracts the phrase-replacement functionality
%% offered by `{halt, {Code, Phrase}}` returns, and also patches over
%% an oddity in httpd_util:reason_phrase (see comment).
-spec reason_phrase(status_code_optional_phrase()) -> reason_phrase().
reason_phrase({Code, Phrase}) when is_integer(Code), is_list(Phrase) ->
    Phrase;
reason_phrase({Code, undefined}) ->
    reason_phrase(Code);
reason_phrase(418) -> % RFC 2324
    "I'm a teapot";
reason_phrase(428) -> % RFC 6585
    "Precondition Required";
reason_phrase(429) -> % RFC 6585
    "Too Many Requests";
reason_phrase(431) -> % RFC 6585
    "Request Header Fields Too Large";
reason_phrase(511) -> % RFC 6585
    "Network Authentication Required";
reason_phrase(Code) when is_integer(Code), Code >= 100, Code =< 599 ->
    case httpd_util:reason_phrase(Code) of
        "Internal Server Error" when Code =/= 500 ->
            %% httpd_util:reason_phrase/1 returns "Internal Server
            %% Error" for any code not defined in RFC 2616, but it
            %% seems more appropriate (according to 6.1.1) to return
            %% the x00 phrase for extension codes: "applications MUST
            %% ... treat any unrecognized response as being equivalent
            %% to the x00 status code of that class"
            httpd_util:reason_phrase(Code - (Code rem 100));
        Phrase ->
            Phrase
    end.
