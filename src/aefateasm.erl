-module(aefateasm).

-export([main/1]).

-define(OPT_SPEC,
    [ {src_file, undefined, undefined, string, "Fate assembler code file"}
    , {verbose, $v, "verbose", undefined, "Verbose output"}
    , {help, $h, "help", undefined, "Show this message"}
    , {outfile, $o, "out", string, "Output file (experimental)"} ]).

usage() ->
    getopt:usage(?OPT_SPEC, "aefateasm").

main(Args) ->
    case getopt:parse(?OPT_SPEC, Args) of
        {ok, {Opts, []}} ->
            case proplists:get_value(help, Opts, false) of
                false ->
                    assemble(Opts);
                true ->
                    usage()
            end;

        {ok, {_, NonOpts}} ->
            io:format("Can't understand ~p\n\n", [NonOpts]),
            usage();

        {error, {Reason, Data}} ->
            io:format("Error: ~s ~p\n\n", [Reason, Data]),
            usage()
    end.

assemble(Opts) ->
    case proplists:get_value(src_file, Opts, undefined) of
        undefined ->
            io:format("Error: no input source file\n\n"),
            usage();
        File ->
            assemble(File, Opts)
    end.

assemble(File, Opts) ->
    Verbose = proplists:get_value(verbose, Opts, false),
    case proplists:get_value(outfile, Opts, undefined) of
        undefined ->
            Asm = aefa_asm:read_file(File),
            {Env, BC} = aefa_asm:asm_to_bytecode(Asm, Opts),
            case Verbose of 
                true ->
                    io:format("Env: ~0p~n", [Env]);
                false -> ok
            end,
            io:format("Code: ~0p~n", [BC]);
        OutFile ->
            aefa_asm:assemble_file(File, OutFile, Opts)
    end.
    
    
