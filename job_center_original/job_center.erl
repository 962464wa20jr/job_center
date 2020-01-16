-module(job_center). 
-behaviour(gen_server). 
-export([start_link/0,job_getStatistic/0,stop/0,init/1,work_wanted/0,add_job/2,cancel/0,handle_cast/2,handle_call/3,handle_info/2,terminate/2,code_change/3]). 
-import(gen_server,[start_link/4,call/2]). 

start_link() ->
	start_link({local,?MODULE},?MODULE,[],[]). 
	
stop() ->
	call(?MODULE,stop). 

init([]) ->
	State = #{1 => {washing_clothes,5000,not_done},2 => {sweep_the_floor,5000,doing},3 => {washing_dishes,5000,not_done},4 => {throwing_garbage,1000,doing}},
	{ok,State}. 

% user api	
work_wanted() ->
	call(?MODULE,{work_wanted}). 
	
add_job(Fun,Time) ->
	call(?MODULE,{add_job,Fun,Time}). 
	
job_getStatistic() ->
	call(?MODULE,{job_getStatistic}). 
	
cancel() ->
	call(?MODULE,{cancel}). 

% callbakc function

handle_call({work_wanted},_From,State) ->
	List = maps:to_list(State),
	NewList = lists:filter(fun(X) -> 
		{_,{_,_,Status}} = X,
		Status =:= not_done end,
		List),
	io:format("newList:~w~n",[NewList]),
	case NewList of
		[] -> 
			Index = -1,
			Fun = void,
			Time = 0,
			{reply,{not_job_now,Index,Fun,Time},State};
		[First|_Tail] ->
			{Index,{Fun,Time,_}} = First,
			io:format("index:~w,Fun:~w~n",[Index,Fun]),
			State2 = State#{ Index := {Fun,Time,doing}},
			{reply,{get_job_success,Index,Fun,Time},State2}
	end;
handle_call({add_job,Fun,Time},_From,State) ->
	Size =  maps:size(State),
	State2 = State#{Size+1 => {Fun,Time,not_done}},
	{reply,add_job_success,State2};
handle_call({job_getStatistic},_From,State) ->
	List = maps:to_list(State),
	Executing = lists:filter(fun(X) ->
		{_,{_,_,Status}} = X,
		Status =:= doing end,
		List),
	io:format("正在进行的任务:~p~n",[Executing]),
	{reply,show_doing_job,State};
handle_call({cancel},_From,State) ->
	List = maps:to_list(State),
	Executing = lists:filter(fun(X) ->
		{_,{_,_,Status}} = X,
		Status =:= doing end,
		List),
	case Executing of
		[] -> {reply,no_executing_job,State};
		_ ->
		%io:format("list:~w~n",[Executing]),
			Record = for(Executing,fun(X,Y) -> {Index,{Fun,Time,_}} = X,Y#{Index := {Fun,Time,not_done}} end,State),
			[State2|_T] = lists:reverse(Record),
			{reply,cancel_doing_job,State2}
	end;
handle_call(stop,_From,State)->
    io:format("good bye!~n"),
    {stop,normal,stopped,State}. 
	
handle_cast(_Msg,State) ->
	{noreply,State}. 
	
handle_info(_Info,State) ->
	{noreply,State}. 

terminate(_Reason,_State) -> ok. 

code_change(_OldVsn,State,_Extra) -> {ok,State}. 

for([],_F,_State) ->
	[];
for([H|T],F,State) ->
	[F(H,State)|for(T,F,F(H,State))]. 
			
	


	
	
