-module(webmachine_headers).

-type t() :: gb_trees:tree(name(), value()).
-type name() :: 'Cache-Control' % erlang:decode_packet/3
                     | 'Connection'
                     | 'Date'
                     | 'Pragma'
                     | 'Transfer-Encoding'
                     | 'Upgrade'
                     | 'Via'
                     | 'Accept'
                     | 'Accept-Charset'
                     | 'Accept-Encoding'
                     | 'Accept-Language'
                     | 'Authorization'
                     | 'From'
                     | 'Host'
                     | 'If-Modified-Since'
                     | 'If-Match'
                     | 'If-None-Match'
                     | 'If-Range'
                     | 'If-Unmodified-Since'
                     | 'Max-Forwards'
                     | 'Proxy-Authorization'
                     | 'Range'
                     | 'Referer'
                     | 'User-Agent'
                     | 'Age'
                     | 'Location'
                     | 'Proxy-Authenticate'
                     | 'Public'
                     | 'Retry-After'
                     | 'Server'
                     | 'Vary'
                     | 'Warning'
                     |'Www-Authenticate'
                     | 'Allow'
                     | 'Content-Base'
                     | 'Content-Encoding'
                     | 'Content-Language'
                     | 'Content-Length'
                     | 'Content-Location'
                     | 'Content-Md5'
                     | 'Content-Range'
                     | 'Content-Type'
                     | 'Etag'
                     | 'Expires'
                     | 'Last-Modified'
                     | 'Accept-Ranges'
                     | 'Set-Cookie'
                     | 'Set-Cookie2'
                     | 'X-Forwarded-For'
                     | 'Cookie'
                     | 'Keep-Alive'
                     | 'Proxy-Connection'
                     | string() | binary().

-type value() :: string() | binary().
-export_type([t/0, name/0, value/0]).

-export([
         empty/0
        ]).

-spec empty() -> t().
empty() ->
    gb_trees:empty().
