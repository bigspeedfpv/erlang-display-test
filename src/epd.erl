-module(epd).

-export([start_link/1]).

%% public API %%
-export([clear/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2]).

-type config() :: {{pins, [{dc | reset | busy, non_neg_integer()}]}, {spi, spi:params()}}.
-export_type([config/0]).

-record(state, {
    spi :: spi:spi(), dc_pin :: gpio:pin(), reset_pin :: gpio:pin(), busy_pin :: gpio:pin()
}).

-spec start_link(config()) -> {ok, pid()}.
start_link(EpdSettings) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, EpdSettings, []).

clear() ->
    gen_server:cast(epd, clear).

%% GEN_SERVER FUNCTIONALITY %%

-spec init(config()) -> {ok, #state{}}.
init({{pins, Pins}, SpiSettings}) ->
    DcPin = proplists:get_value(dc, Pins),
    ResetPin = proplists:get_value(reset, Pins),
    BusyPin = proplists:get_value(busy, Pins),
    State2 = hw_init(SpiSettings, #state{dc_pin = DcPin, reset_pin = ResetPin, busy_pin = BusyPin}),
    {ok, State2}.

handle_call(_Msg, _From, State) ->
    io:format("got call ~n"),
    {reply, 0, State}.

handle_cast(clear, State) ->
    epd_clear(State),
    {noreply, State};
handle_cast(_Msg, State) ->
    io:format("got cast~n"),
    {noreply, State}.

%% HARDWARE FUNCTIONALITY %%

-include("luts.hrl").

hw_reset(#state{reset_pin = ResetPin}) ->
    io:format("hardware reset~n"),
    gpio:digital_write(ResetPin, high),
    timer:sleep(20),
    gpio:digital_write(ResetPin, low),
    timer:sleep(2),
    gpio:digital_write(ResetPin, high),
    timer:sleep(20).

-spec hw_init({spi, SpiSettings :: spi:params()}, State :: #state{}) -> #state{}.
hw_init({spi, SpiSettings}, #state{dc_pin = DcPin, reset_pin = ResetPin, busy_pin = BusyPin} = State) ->
    io:format("initializing epd~n"),
    erlang:display(gpio:set_pin_mode(DcPin, output)),
    erlang:display(gpio:digital_write(DcPin, low)),
    erlang:display(gpio:set_pin_mode(ResetPin, output)),
    erlang:display(gpio:digital_write(DcPin, high)),
    erlang:display(gpio:set_pin_mode(BusyPin, input)),
    erlang:display(gpio:set_pin_pull(BusyPin, down)),
    Spi = spi:open(SpiSettings),

    erlang:display(State),
    State2 = State#state{spi = Spi},

    hw_reset(State2),

    send_command(State2, 16#12),
    timer:sleep(300),

    io:format("filling memory~n"),
    send_command(State2, 16#46),
    send_data(State2, <<16#F7>>),
    io:format("waiting for not busy~n"),
    wait_for_low(BusyPin),
    send_command(State2, 16#47),
    send_data(State2, <<16#F7>>),
    io:format("waiting for not busy~n"),
    wait_for_low(BusyPin),

    io:format("setting output control~n"),
    send_command(State2, 16#01),
    send_data(State2, <<16#DF, 16#01, 16#00>>),

    io:format("setting gate level~n"),
    send_command(State2, 16#03),
    send_data(State2, <<0>>),

    io:format("setting source level~n"),
    send_command(State2, 16#04),
    send_data(State2, <<16#41, 16#A8, 16#32>>),

    io:format("setting data entry settings~n"),
    send_command(State2, 16#11),
    send_data(State2, <<16#03>>),

    io:format("setting border~n"),
    send_command(State2, 16#3C),
    send_data(State2, <<0>>),

    io:format("setting softstart~n"),
    send_command(State2, 16#0C),
    send_data(State2, <<16#AE, 16#C7, 16#C3, 16#C0, 16#C0>>),

    io:format("setting temp sensor~n"),
    send_command(State2, 16#18),
    send_data(State2, <<16#80>>),

    io:format("setting vcom~n"),
    send_command(State2, 16#2C),
    send_data(State2, <<16#44>>),

    io:format("setting display mode~n"),
    send_command(State2, 16#37),
    send_data(State2, <<16#00, 16#FF, 16#FF, 16#FF, 16#FF, 16#4F, 16#FF, 16#FF, 16#FF, 16#FF>>),

    io:format("setting x pos~n"),
    send_command(State2, 16#44),
    send_data(State2, <<16#00, 16#00, 16#17, 16#01>>),
    io:format("setting y pos~n"),
    send_command(State2, 16#45),
    send_data(State2, <<16#00, 16#00, 16#DF, 16#01>>),

    io:format("Loading LUT~n"),
    send_command(State2, 16#22),
    send_data(State2, <<16#CF>>),

    State2.

-spec epd_clear(State :: #state{}) -> ok.
epd_clear(#state{busy_pin = BusyPin} = State) ->
    io:format("clearing~n"),
    send_command(State, 16#4E),
    send_data(State, <<0, 0>>),
    send_command(State, 16#4F),
    send_data(State, <<0, 0>>),

    io:format("writing blank image~n"),
    send_command(State, 16#24),
    send_data(State, ?EMPTY_SCREEN),

    send_command(State, 16#32),
    send_data(State, ?LUT_GRAY1_GC),

    send_command(State, 16#20),
    wait_for_low(BusyPin),
    ok.

send_command(#state{spi = Spi, dc_pin = DcPin}, Cmd) ->
    gpio:digital_write(DcPin, low),
    % TODO: maybe command would work here? idk
    spi:write(Spi, epd_hw, #{write_data => <<Cmd>>}).

send_data(#state{spi = Spi, dc_pin = DcPin}, Data) ->
    gpio:digital_write(DcPin, high),
    spi:write(Spi, epd_hw, #{write_data => Data}).

% FIXME: this should probably use interrupts and message passing?
wait_for_low(Pin) ->
    case gpio:digital_read(Pin) of
        low ->
            ok;
        _ ->
            timer:sleep(20),
            wait_for_low(Pin)
    end.
