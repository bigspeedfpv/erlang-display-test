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
    {ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, EpdSettings, []),
    gen_server:cast(Pid, {init, EpdSettings}),
    {ok, Pid}.

clear() ->
    gen_server:cast(epd, clear).

%% GEN_SERVER FUNCTIONALITY %%

-spec init(config()) -> {ok, #state{}}.
init({{pins, Pins}, _SpiSettings}) ->
    DcPin = proplists:get_value(dc, Pins),
    ResetPin = proplists:get_value(reset, Pins),
    BusyPin = proplists:get_value(busy, Pins),
    {ok, #state{dc_pin = DcPin, reset_pin = ResetPin, busy_pin = BusyPin}}.

handle_call(_Msg, _From, State) ->
    io:format("got call ~n"),
    {reply, 0, State}.

handle_cast(
    {init, {_, {spi, SpiSettings}}}, #state{dc_pin = DcPin, reset_pin = ResetPin, busy_pin = BusyPin} = State
) ->
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

    send_command(State2, 16#46),
    send_data(State2, <<16#F7>>),

    {noreply, State2};
handle_cast(_Msg, State) ->
    io:format("got cast~n"),
    {noreply, State}.

%% HARDWARE FUNCTIONALITY %%

hw_reset(#state{reset_pin = ResetPin}) ->
    gpio:digital_write(ResetPin, high),
    timer:sleep(20),
    gpio:digital_write(ResetPin, low),
    timer:sleep(2),
    gpio:digital_write(ResetPin, high),
    timer:sleep(20).

send_command(#state{spi = Spi, dc_pin = DcPin}, Cmd) ->
    gpio:digital_write(DcPin, low),
    % TODO: maybe command would work here? idk
    spi:write(Spi, epd_hw, #{write_data => <<Cmd>>}).

send_data(#state{spi = Spi, dc_pin = DcPin}, Data) ->
    gpio:digital_write(DcPin, high),
    spi:write(Spi, epd_hw, #{write_data => Data}).
