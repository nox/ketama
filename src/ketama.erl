%% Copyright (c) 2012, Anthony Ramine <n.oxyde@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(ketama).

-record(table, {
    name      :: atom(),
    continuum :: ketama_continuum:continuum()
}).

-opaque table() :: #table{}.
-type memcache() :: {inet:hostname() | inet:ip_address(), inet:port_number()}.

-export_type([table/0]).
-export_type([memcache/0]).

-export([memcache_table/2]).
-export([table/2]).
-export([table/3]).
-export([lookup/2]).
-export([add/2]).
-export([remove/2]).

-spec memcache_table(atom(), ketama_continuum:weight_relation(memcache())) ->
                        table().
memcache_table(Name, Servers) ->
    table(Name, fun memcache_key/1, Servers).

-spec table(atom(), ketama_continuum:weight_relation(iodata())) -> table().
table(Name, Weights) ->
    table(Name, identity, Weights).

-spec table(atom(), ketama_continuum:key_fun(T),
            ketama_continuum:weight_relation(T)) -> table().
table(Name, F, Weights) ->
    Name      = ets_table(Name),
    Continuum = ketama_continuum:new(F, Weights),
    Points    = ketama_continuum:points(Continuum),
    ets:insert(Name, Points),
    #table{name=Name,continuum=Continuum}.

-spec lookup(atom(), iodata()) -> term().
lookup(Name, Value) ->
    Point = point(Value),
    Key   = case ets:next(Name, Point-1) of
                '$end_of_table' -> ets:first(Name);
                Result          -> Result
            end,
    ets:lookup_element(Name, Key, 2).

add(Weights, Table) ->
    update(Weights, Table, fun ketama_continuum:add/2).

remove(Values, Table) ->
    update(Values, Table, fun ketama_continuum:remove/2).

-type update_fun(Term) :: fun((Term, ketama_continuum:continuum()) ->
                                  ketama_continuum:continuum()).

-spec update(Term, table(), update_fun(Term)) -> table().
update(Term, Table=#table{name=Name,continuum=OldContinuum}, F) ->
    OldPoints       = ketama_continuum:points(OldContinuum),
    NewContinuum    = F(Term, OldContinuum),
    NewPoints       = ketama_continuum:points(NewContinuum),
    OldValueRel     = sofs:relation(OldPoints, [{point,value}]),
    NewValueRel     = sofs:relation(NewPoints, [{point,value}]),
    AddedValueRel   = sofs:difference(NewValueRel, OldValueRel),
    OldPointSet     = sofs:domain(OldValueRel),
    NewPointSet     = sofs:domain(NewValueRel),
    RemovedPointSet = sofs:difference(OldPointSet, NewPointSet),
    ets:insert(Name, sofs:to_external(AddedValueRel)),
    _ = [ets:delete(Name, P) || P <- sofs:to_external(RemovedPointSet)],
    Table#table{continuum=NewContinuum}.

-spec ets_table(atom()) -> atom().
ets_table(Name) ->
    ets:new(Name, [ordered_set,named_table,{read_concurrency,true}]).

-spec point(iodata()) -> ketama_continuum:point().
point(Value) ->
    <<Point:32/little, _/binary>> = erlang:md5(Value),
    Point.

-spec memcache_key(memcache()) -> iodata().
memcache_key({Hostname,Port}) when is_atom(Hostname) ->
    memcache_key({atom_to_list(Hostname),Port});
memcache_key({Hostname,Port}) when is_list(Hostname) ->
    [Hostname,$:|integer_to_list(Port)];
memcache_key({Hostname,Port}) when is_tuple(Hostname) ->
    memcache_key({inet_parse:ntoa(Hostname),Port}).
