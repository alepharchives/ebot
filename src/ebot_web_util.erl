%% EBOT, an erlang web crawler.
%% Copyright (C) 2010 ~ matteo DOT redaelli AT libero DOT it
%%                      http://www.redaelli.org/matteo/
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%
%%%-------------------------------------------------------------------
%%% File    : ebot_web_util.erl
%%% Author  : matteo <<matteo.redaelli@@libero.it>
%%% Description : 
%%%
%%% Created :  4 Oct 2009 by matteo <matteo@redaelli.org>
%%%-------------------------------------------------------------------
-module(ebot_web_util).
-author("matteo.redaelli@libero.it").

-include("ebot.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([
	 fetch_url_with_only_html_body/1,
	 fetch_url_links/1
	]).

%%--------------------------------------------------------------------
%% Functions:  
%%
%% Description: Handling call messages
%%--------------------------------------------------------------------

fetch_url_with_only_html_body(Url) ->
    Result = fetch_url(Url, head),
    case Result of 
	{error, Reason} ->
	    {error, Reason};   
	{ok, Url1, {_Status, Headers, _}} ->
	    case is_text_html_mime_url(Headers) of
		true ->
		    fetch_url(Url1, get);
		false ->
		    Result
	    end
    end.

fetch_url_links(URL) ->
    case fetch_url(URL, get) of
	{error, Reason} ->
	    {error, Reason};
	{ok, _, {_Status, _Headers, Body}} -> 
	    Links = ebot_html_util:get_links(Body, URL),
	    {ok, Links}
    end.
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

fetch_url(Url, Method) when is_binary(Url) ->
    fetch_url(binary_to_list(Url), Method);
fetch_url(Url, Method) ->
    {ok, ReqHeaders} = ebot_util:get_env(web_http_header),
    {ok, HttpRequestOptions} = ebot_util:get_env(web_http_request_options),
    {ok, ReqTimeout} = ebot_util:get_env(web_http_timeout_ms),
    %% ibrowse is used instead of httpc, because httpc has issues. For example, chunked transfer encoding is not fully supported.
    %% ibrowse cannot do autoredirects, so to use wrapper req_autoredirect/6.
    try req_autoredirect(Url, ReqHeaders, Method, [{response_format, binary}|HttpRequestOptions], ReqTimeout, 0) of

	%% Url1 maybe or may not be equal to Url. Url1 differs from Url if one or more redirections done inside req_autoredirect/6
	{ok, _Url1, _Response} = Result ->
	    %% Do not decompress body right here. It will save memory on RabbitMQ-server-side.
	    Result;

	{error, Reason} = Result ->
	    error_logger:error_report({?MODULE, ?LINE, {fetch_url, Url, error, Reason}}),
	    Result

    catch
	_:Reason -> 
	    error_logger:error_report({?MODULE, ?LINE, {fetch_url, Url, cannot_fetch_url, Reason}}),
	    {error, Reason}
    end.


%% wrapper around ibrowse to redirect automatically
req_autoredirect(Url, ReqHeaders, Method, Options, ReqTimeout, N) when N < 8 ->
    %% ibrowse:send_req(Url, ReqHeaders, Method::atom(), ReqBody, Options, Timeout) 
    %%	  -> {ok, Code::string, RespHeaders, RespBody} | {error, req_timeout} | {error, Reason}
    case ibrowse:send_req(Url, ReqHeaders, Method, [], Options, ReqTimeout) of
	{ok, Code, RespHeaders, RespBody} ->
	    case proplists:get_value("Location", RespHeaders) of
		undefined ->
		    %% No more redirects. Remove camel-case from response headers.
		    RespHeaders1 = [{string:to_lower(K), V} || {K,V} <- RespHeaders],
		    {ok, Url, {Code, RespHeaders1, RespBody}};

		LocationUrl ->
		    error_logger:error_report({?MODULE, ?LINE, {autoredirect, Code, Url, LocationUrl}}),
		    req_autoredirect(LocationUrl, ReqHeaders, Method, Options, ReqTimeout, N+1)
	    end;

	E -> E
    end;
req_autoredirect(_, _, _, _, _, _) ->
    {error, too_many_redirects}.


is_text_html_mime_url(Headers) ->
    Contenttype = proplists:get_value("content-type", Headers),
    case Contenttype of
	undefined ->
	    false;
	Contenttype ->
	    case re:run(Contenttype, "^text/html",[{capture, none},caseless] ) of
		match ->
		    true;
		nomatch ->
		    false
	    end
    end.


-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

ebot_web_util_test() ->
    Url =  <<"http://www.redaelli.org/matteo/ebot_test/">>,

    H1 = [{"connection","Keep-Alive"},
	  {"content-type","text/html"},
	  {"keep-alive","timeout=15, max=99"}],

    H2 = [{"connection","Keep-Alive"},
	  {"content-length","725"},
	  {"content-type","text/txt"}],

    ?assertEqual(true, is_text_html_mime_url(H1)),
    ?assertEqual(false, is_text_html_mime_url(H2)),

    {ok, Url, {_Status, Headers, _Body}} = fetch_url_with_only_html_body(Url),
    ?assertEqual(true, is_text_html_mime_url(Headers)),

    ExpectedUrlLinks = [<<"http://code.google.com/p/oreste/">>,
			     <<"http://github.com/matteoredaelli/ebot">>,
			     <<"http://www.redaelli.org/">>,
			     <<"http://www.redaelli.org/matteo">>,
			     <<"http://www.redaelli.org/matteo/ebot_test/dir1">>
		       ],
    UrlLinks = fetch_url_links(Url),
    ?assertEqual( {ok, ExpectedUrlLinks}, UrlLinks),
    ExpectedExternalLinks = [<<"http://code.google.com/p/oreste/">>,
			     <<"http://github.com/matteoredaelli/ebot">>],
    ?assertEqual(ExpectedExternalLinks,  
		 ebot_url_util:filter_external_links(Url, ExpectedUrlLinks)).

-endif.
