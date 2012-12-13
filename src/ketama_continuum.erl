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

-module(ketama_continuum).

-type weight_relation(T) :: relation(T, weight()).
-type weight()           :: pos_integer().

-type point_set() :: ordsets:ordset(point()).
-type point()     :: 0..(1 bsl 32 - 1).

%% @doc A function from values to keys.
-type key_fun(T) :: fun((T) -> iodata()) | identity.

-type relation(Domain, Range) :: [{Domain,Range}].

-record(continuum, {
    value_rel,
    weight_fun,
    key_fun = identity
}).

-opaque continuum() :: #continuum{}.

-export_type([weight_relation/1]).
-export_type([point/0]).
-export_type([key_fun/1]).
-export_type([continuum/0]).

-export([new/1]).
-export([new/2]).
-export([points/1]).
-export([add/2]).
-export([remove/2]).

-spec new(weight_relation(iodata())) -> continuum().
new(WeightRel) ->
    WeightFun = weight_relation_to_function(WeightRel),
    PointFam  = weight_range_to_point_set_range(WeightFun),
    ValueRel  = point_family_to_value_relation(PointFam),
    #continuum{value_rel=ValueRel,weight_fun=WeightFun}.

-spec new(key_fun(T), weight_relation(T)) -> continuum().
new(F, WeightRel) when is_function(F, 1) ->
    WeightFun0 = weight_relation_to_function(WeightRel),
    {WeightFun1,KeyFun} = value_domain_to_key_domain(F, WeightFun0),
    PointFam0 = weight_range_to_point_set_range(WeightFun1),
    PointFam1 = key_domain_to_value_domain(KeyFun, PointFam0),
    ValueRel  = point_family_to_value_relation(PointFam1),
    #continuum{value_rel=ValueRel,weight_fun=WeightFun0};
new(identity, WeightRel) ->
    new(WeightRel).

-spec points(continuum()) -> relation(point(), term()).
points(#continuum{value_rel=Rel}) ->
    drop_redundant_points(Rel).

-spec add(weight_relation(term()), continuum()) -> continuum().
add(WeightRel, #continuum{weight_fun=WeightFun,key_fun=F}) ->
    new(F, WeightRel ++ WeightFun).

-spec remove([term()], continuum()) -> continuum().
remove(ExtValueSet, #continuum{weight_fun=ExtWeightFun,key_fun=F}) ->
    ValueSet   = sofs:set(ExtValueSet, [value]),
    WeightFun0 = sofs:a_function(ExtWeightFun, [{value,weight}]),
    WeightFun1 = sofs:drestriction(WeightFun0, ValueSet),
    new(F, sofs:to_external(WeightFun1)).

-spec weight_relation_to_function(weight_relation(T)) -> weight_relation(T).
weight_relation_to_function(ExtWeightRel) ->
    Rel = sofs:relation(ExtWeightRel, [{value,weight}]),
    Fam = sofs:relation_to_family(Rel),
    orddict:map(fun (_, Ws) -> max(lists:sum(Ws), 0) end,
                sofs:to_external(Fam)).

-spec weight_range_to_point_set_range(weight_relation(iodata())) ->
    relation(iodata(), point_set()).
weight_range_to_point_set_range(Rel) ->
    Count  = length(Rel),
    TotalW = orddict:fold(fun (_, W, Sum) -> Sum + W end, 0, Rel),
    orddict:map(fun (K, W) -> key_point_set(K, 40 * Count * W div TotalW) end,
                Rel).

-spec key_point_set(iodata(), non_neg_integer()) -> point_set().
key_point_set(K, N) when N >= 0 ->
    Ctx = key_hash_context(K),
    seqfoldr(fun (I, Acc) ->
                     ordsets:union(nth_key_point_set(I - 1, Ctx), Acc)
             end,
             orddict:new(), N).

-spec nth_key_point_set(non_neg_integer(), binary()) -> point_set().
nth_key_point_set(I, Ctx) ->
    <<P0:32/little,P1:32/little,
      P2:32/little,P3:32/little>> = nth_key_hash(I, Ctx),
    ordsets:from_list([P0,P1,P2,P3]).

-spec key_hash_context(binary()) -> binary().
key_hash_context(K) ->
    Ctx = erlang:md5_init(),
    erlang:md5_update(Ctx, [K,$-]).

-spec nth_key_hash(non_neg_integer(), binary()) -> binary().
nth_key_hash(I, Ctx0) ->
    Ctx1 = erlang:md5_update(Ctx0, integer_to_list(I)),
    erlang:md5_final(Ctx1).

-spec point_family_to_value_relation(relation(T, point_set())) ->
                                        relation(point(), T).
point_family_to_value_relation(ExtPointFam) ->
    Fam  = sofs:family(ExtPointFam, [{key,[point]}]),
    Rel0 = sofs:family_to_relation(Fam),
    Rel1 = sofs:converse(Rel0),
    sofs:to_external(Rel1).

-spec value_domain_to_key_domain(key_fun(T), relation(T, Range)) ->
                                    {relation(iodata(), Range),
                                     relation(T, iodata())}.
value_domain_to_key_domain(F, ExtWeightFun) ->
    ExtKeyFun  = lists:map(fun ({V,_}) -> {V,F(V)} end, ExtWeightFun),
    KeyFun     = sofs:a_function(ExtKeyFun, [{value,key}]),
    ValueFun   = sofs:converse(KeyFun),
    WeightFun0 = sofs:a_function(ExtWeightFun, [{value,weight}]),
    WeightFun1 = sofs:composite(ValueFun, WeightFun0),
    KeyFun     = sofs:converse(ValueFun),
    {sofs:to_external(WeightFun1),ExtKeyFun}.

-spec key_domain_to_value_domain(relation(Value, iodata()),
                                 relation(iodata(), Range)) ->
                                    relation(Value, Range).
key_domain_to_value_domain(ExtKeyFun, ExtFunFromKey) ->
    KeyFun       = sofs:a_function(ExtKeyFun, [{value,key}]),
    FunFromKey   = sofs:a_function(ExtFunFromKey, [{key,range}]),
    FunFromValue = sofs:composite(KeyFun, FunFromKey),
    sofs:to_external(FunFromValue).

-spec drop_redundant_points(orddict:orddict()) -> orddict:orddict().
drop_redundant_points([{_,V0}=P|Ps0]) ->
    Ps1 = lists:dropwhile(fun ({_,V1}) -> V0 =:= V1 end, Ps0),
    [P|drop_redundant_points(Ps1)];
drop_redundant_points([]) ->
    [].

-spec seqfoldr(fun((pos_integer(), T) -> T), T, non_neg_integer()) -> T.
seqfoldr(F, Acc, N) when is_integer(N), N > 0 ->
    seqfoldr(F, F(N, Acc), N - 1);
seqfoldr(F, Acc, 0) when is_function(F, 2) ->
    Acc.
