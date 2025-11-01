-module(displaytest_app).

-export([start/0]).

-define(EPD_SETTINGS, {
    {pins, [
        {dc, 26},
        {reset, 25},
        {busy, 34}
    ]},
    {spi, [
        {
            bus_config, [
                {mosi, 19},
                {sclk, 5},
                {peripheral, "spi2"}
            ]
        },
        {device_config, [
            {epd_hw, [
                {clock_speed_hz, 2000000},
                {mode, 0},
                {cs, 4},
                {address_len_bits, 0}
            ]}
        ]}
    ]}
}).

start() ->
    io:format("helloooooo~n"),
    gpio:open(),
    displaytest_sup:start_link(?EPD_SETTINGS),
    epd:clear(),
    epd:checkerboard(abs(atomvm:random()) rem 128),
    epd:sleep(),
    wait_forever().

wait_forever() ->
    receive
        X -> X
    end.
