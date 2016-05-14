-module(entry_manager).
-export([entries/3, entries/4, entries/5]).

files(DataDirectory, Project, Year) ->
  Directory = io_lib:format("~s/~s/~p", [DataDirectory, Project, Year]),
  Files = case file:list_dir(list_to_binary(Directory)) of
    {ok, Filenames} -> Filenames;
    {error, _Reason} -> []
  end,
  {Directory, Files}.

content(DataDirectory, Files) ->
  Content = lists:map(fun(File) -> {ok, Lines} = file:read_file(DataDirectory ++ "/" ++ File), Lines end, Files),
  lists:flatten(
    lists:map(fun(Lines) -> binary:split(Lines, <<"\n">>, [global, trim_all]) end, Content)
  ).

entries(DataDirectory, Project, Year) ->
  {Directory, Files} = files(DataDirectory, Project, Year),
  content(Directory, Files).

entries(DataDirectory, Project, Year, Month) ->
  Prefix = binary_to_list(list_to_binary(io_lib:format("~p-~2..0B", [Year, Month]))),
  {Directory, AllFiles} = files(DataDirectory, Project, Year),
  MonthFiles = lists:filter(fun(Filename) -> lists:prefix(Prefix, Filename) end, AllFiles),
  content(Directory, MonthFiles).

entries(DataDirectory, Project, Year, Month, Date) ->
  DateFile = binary_to_list(list_to_binary(io_lib:format("~s/~s/~p/~p-~2..0B-~2..0B", [DataDirectory, Project, Year, Year, Month, Date]))),
  case file:read_file(DateFile) of
    {ok, Lines} -> binary:split(Lines, <<"\n">>, [global, trim_all]);
    {error, _Reason} -> []
  end.
