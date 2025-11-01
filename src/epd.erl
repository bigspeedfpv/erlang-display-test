-module(epd).

-export([start_link/1]).

%% public API %%
-export([coords_for_index/1, clear/0, checkerboard/1, sleep/0, hw_reset/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2]).

-type config() :: {{pins, [{dc | reset | busy, non_neg_integer()}]}, {spi, spi:params()}}.
-export_type([config/0]).

-record(state, {
    spi :: spi:spi(), dc_pin :: gpio:pin(), reset_pin :: gpio:pin(), busy_pin :: gpio:pin()
}).

-define(DISPLAY_HEIGHT, 480).
-define(DISPLAY_WIDTH, 280).
-define(PIXEL_COUNT, ?DISPLAY_WIDTH * ?DISPLAY_HEIGHT).
-define(BW_BITSTRING_LEN, ?PIXEL_COUNT / 8).

-include("luts.hrl").

%% PUBLIC API %%

-spec start_link(config()) -> {ok, pid()}.
start_link(EpdSettings) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, EpdSettings, []).

-spec coords_for_index(integer()) -> {integer(), integer()}.
coords_for_index(Index) ->
    {Index rem ?DISPLAY_WIDTH, Index div ?DISPLAY_WIDTH}.

clear() ->
    gen_server:cast(epd, {show, ?EMPTY_SCREEN}).

checkerboard(Size) ->
    Checkerboard = build_checkerboard(?PIXEL_COUNT, Size),
    gen_server:cast(epd, {show, Checkerboard}).

sleep() ->
    gen_server:cast(epd, sleep).

hw_reset() ->
    gen_server:cast(epd, hw_reset).

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

handle_cast({show, Image}, State) ->
    epd_show(State, Image),
    {noreply, State};
handle_cast(sleep, State) ->
    epd_sleep(State),
    {noreply, State};
handle_cast(hw_reset, State) ->
    hw_reset(State),
    {noreply, State};
handle_cast(_Msg, State) ->
    io:format("got cast~n"),
    {noreply, State}.

%% HARDWARE FUNCTIONALITY %%

hw_reset(#state{reset_pin = ResetPin}) ->
    io:format("hardware reset~n"),
    gpio:digital_write(ResetPin, high),
    timer:sleep(20),
    gpio:digital_write(ResetPin, low),
    timer:sleep(2),
    gpio:digital_write(ResetPin, high),
    timer:sleep(20).

-spec hw_init({spi, SpiSettings :: spi:params()}, State :: #state{}) -> #state{}.
hw_init(
    {spi, SpiSettings}, #state{dc_pin = DcPin, reset_pin = ResetPin, busy_pin = BusyPin} = State
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

-spec epd_show(State :: #state{}, Image :: binary()) -> ok.
epd_show(#state{busy_pin = BusyPin} = State, Image) ->
    io:format("clearing~n"),
    send_command(State, 16#4E),
    send_data(State, <<0, 0>>),
    send_command(State, 16#4F),
    send_data(State, <<0, 0>>),

    io:format("writing image~n"),
    send_command(State, 16#24),
    send_data(State, Image),

    io:format("writing lut~n"),
    send_command(State, 16#32),
    send_data(State, ?LUT_GRAY1_GC),

    send_command(State, 16#20),
    wait_for_low(BusyPin),
    ok.

send_command(#state{spi = Spi, dc_pin = DcPin}, Cmd) ->
    gpio:digital_write(DcPin, low),
    % TODO: maybe command would work here? idk
    spi:write(Spi, epd_hw, #{write_data => <<Cmd>>}).

% ESP32 has an SPI transfer limit of ~4092 bytes, so we chunk if more.
send_data(#state{spi = Spi, dc_pin = DcPin}, Data) when byte_size(Data) < 4092 ->
    gpio:digital_write(DcPin, high),
    spi:write(Spi, epd_hw, #{write_data => Data});
send_data(#state{spi = Spi, dc_pin = DcPin}, Data) ->
    gpio:digital_write(DcPin, high),
    write_chunked(Spi, Data, 0, byte_size(Data)).

-define(CHUNK_SIZE, 4000).

write_chunked(_Spi, _Data, Offset, TotalLen) when Offset >= TotalLen -> ok;
write_chunked(Spi, Data, Offset, TotalLen) ->
    ChunkLen = min(?CHUNK_SIZE, TotalLen - Offset),
    io:format("writing chunked data from offset ~p with length ~p (total ~p bytes)~n", [
        Offset, ChunkLen, TotalLen
    ]),
    Chunk = binary:part(Data, Offset, ChunkLen),
    case spi:write(Spi, epd_hw, #{write_data => Chunk}) of
        ok ->
            write_chunked(Spi, Data, Offset + ?CHUNK_SIZE, TotalLen);
        {error, Why} ->
            io:format("error writing SPI chunk: ~p~n", [Why]),
            {error, Why}
    end.

% FIXME: this should probably use interrupts and message passing?
wait_for_low(Pin) ->
    case gpio:digital_read(Pin) of
        low ->
            ok;
        _ ->
            timer:sleep(20),
            wait_for_low(Pin)
    end.

epd_sleep(State) ->
    send_command(State, 16#10).

% NOTES SO I CAN STOP ALT TABBING vvvvvv
% The EPD expects WIDTH*HEIGHT bits of info for b&w, with each bit being one pixel.
% Pixels are row-major. If byte encoding (which we are here) they should go MSB->LSB left to right.
% Since AtomVM doesn't support bitstrings, we use binaries, so the actual length of the binary
% should just be W*H/8. Then, getting the value of an indexed pixel using array notation
% (because i'm bad at erlang) should be pixels[i div 8] band (1 bsl (7 - (i rem 8))).
% Rationale: floor div will give us the containing byte, (i rem 8) will give us "bit N" in
% the byte, and we need to index from "left to right" so we subtract from 7
% holy yap :(

-spec build_checkerboard(TotalPixels :: pos_integer(), SquareSize :: pos_integer()) -> bitstring().
build_checkerboard(TotalPixels, SquareSize) ->
    io:format("building checkerboard for ~p total pixels~n", [TotalPixels]),
    % i wish binaries were mutable.
    % we step through pixel indices in chunks of 8 and do precise pixels in this function!
    list_to_binary([
        [(fill_row(Row, 0, ?DISPLAY_WIDTH, SquareSize, []))]
     || Row <- lists:seq(0, ?DISPLAY_HEIGHT)
    ]).

-spec fill_row(
    Row :: non_neg_integer(),
    Col :: non_neg_integer(),
    Width :: pos_integer(),
    SquareSize :: pos_integer(),
    Acc :: list(integer())
) -> binary().
fill_row(_, Col, Width, _, Acc) when Col >= Width ->
    list_to_binary(lists:reverse(Acc));
fill_row(Row, Col, Width, SquareSize, Acc) ->
    Byte = fill_byte(SquareSize, Width, Row, Col, 0, 0),
    fill_row(Row, Col + 8, Width, SquareSize, [Byte | Acc]).

-spec fill_byte(
    SquareSize :: pos_integer(),
    Width :: pos_integer(),
    Row :: non_neg_integer(),
    Col :: non_neg_integer(),
    Offset :: non_neg_integer(),
    Acc :: integer()
) -> integer().
fill_byte(_, _, _, _, 8, Acc) ->
    Acc;
fill_byte(SquareSize, Width, Row, Col, Offset, Acc) ->
    {SquareX, SquareY} = {(Col + Offset) div SquareSize, Row div SquareSize},
    % if the square position is even this should be a white square (set to 1)
    Acc2 =
        case (SquareX + SquareY) rem 2 of
            0 -> Acc;
            % see above note for why we start from 7
            1 -> Acc bor (1 bsl (7 - Offset))
        end,
    fill_byte(SquareSize, Width, Row, Col, Offset + 1, Acc2).
