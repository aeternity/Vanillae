#!/usr/bin/env escript

%% This is a script to print out the base64 representation of a sha384 hash of
%% input bytes. This exists because browser extensions are stupid.  In order to
%% load a script into a page context it needs the sha384 hash encoded in base64
%% of the script. This script exists to just print that easily

-mode(compile).

%% either read from file or read stdin
main(["-e", FileName]) ->
    {ok, Bytes} = file:read_file(FileName),
    main_bytes(ext, Bytes);
main([FileName]) ->
    {ok, Bytes} = file:read_file(FileName),
    main_bytes(normal, Bytes);
main(["-e"]) ->
    main_bytes(ext, read_stdin());
main([]) ->
    main_bytes(normal, read_stdin()).


%% take sha384 of bytes and print in base64
%% print in manifest extension format
main_bytes(ext, Bytes) ->
    Printing = main_bytes(Bytes),
    io:format("'sha384-~ts'", [Printing]);
main_bytes(normal, Bytes) ->
    Printing = main_bytes(Bytes),
    io:format("~ts~n", [Printing]).

main_bytes(Bytes) ->
    Sha384 = crypto:hash(sha384, Bytes),
    base64:encode(Sha384).


read_stdin() ->
    read_stdin(<<>>).

read_stdin(Acc) ->
    ThisChar = io:get_chars(standard_io, "", 1),
    case ThisChar of
        eof ->
            unicode:characters_to_binary(Acc);
        {error, Reason} ->
            error(Reason);
        Char ->
            read_stdin([Acc, Char])
    end.
