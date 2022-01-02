-module(my_bank).
-behaviour(gen_server).

%% API
-export([start/1, stop/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([new_account/1, deposit/2, withdraw/2]).

start(Name) ->
    start_link(Name).

stop(Name) ->
    gen_server:call(Name, stop).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

init(_Args) ->
    {ok, ets:new(?MODULE, [])}.

handle_call({new, Who}, _From, Tab) ->
    Reply = case ets:lookup(Tab, Who) of 
        [] -> ets:insert(Tab, {Who, 0}),
            {welcome, Who};
        [_] -> {Who, 'You already are a customer'}
        end,    
    {reply, Reply, Tab};

handle_call({add, Who, X}, _From, Tab) ->
    Reply = case ets:lookup(Tab, Who) of 
        [] -> not_a_customer;
        [{Who, Balance}] -> 
            NewBlance = Balance + X,
            ets:insert(Tab, {Who, NewBlance}),
            {thanks, Who, your_banlance_is, NewBlance}
        end,
    {reply, Reply, Tab};

handle_call({remove, Who, X}, _From, Tab) ->
    Reply = case ets:lookup(Tab, Who) of 
        [] -> not_a_customer;
        [{Who, Balance}] when X =< Balance -> 
            NewBlance = Balance - X,
            ets:insert(Tab, {Who, NewBlance}),
            {thanks, Who, your_banlance_is, NewBlance};
        [{Who, Balance}] -> {sorry, Who, you_only_have, Balance, in_the_bank}
    end,
    {reply, Reply, Tab};
	
handle_call(stop, _From, Tab) ->
    {stop, normal, stopped, Tab}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

new_account(Who) -> gen_server:call(?MODULE, {new, Who}).
deposit(Who, Account) -> gen_server:call(?MODULE, {add, Who, Account}).
withdraw(Who, Account) -> gen_server:call(?MODULE, {remove, Who, Account}).