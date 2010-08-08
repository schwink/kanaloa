%% @author Stephen Schwink <thruster@schwink.net>%% @copyright 2010 Stephen Schwink.

%% @doc Useful common functions.

-module(kanaloa_utils).
-author('Stephen Schwink <kanaloa@schwink.net>').

-export([now_utc_ms/0]).

%% @spec now_utc_ms() -> integer()
%% @doc Returns an integer representing the current time, as the number of milliseconds since the start of the UNIX epoch.
%% This can be used to represent a Date in JavaScript.
%% This depends on the server timezone being correctly configured.
now_utc_ms() ->
    Now = now(),
    {_MegaSeconds, _Seconds, MicroSeconds} = Now,
    DateTime = calendar:now_to_universal_time(Now),
    
    Seconds = calendar:datetime_to_gregorian_seconds(DateTime),
    % Seconds between Gregorian and Unix epochs = 62167219200
    UnixSeconds = Seconds - 62167219200,
    
    Milliseconds = (UnixSeconds * 1000) + (MicroSeconds / 1000),
    trunc(Milliseconds).
