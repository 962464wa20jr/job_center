-module(job_center_file). 
-behaviour(gen_server). 
-export([start_link/0,job_getStatistic/0,stop/0,init/1,work_wanted/0,add_job/2,job_done/1,cancel/0,handle_cast/2,handle_call/3,handle_info/2,terminate/2,code_change/3]). 
-import(gen_server,[start_link/4,call/2]). 


start_link() ->
	start_link({local,?MODULE},?MODULE,[],[]). 
	
stop() ->
	call(?MODULE,stop). 
	
init([]) ->
	Tab = ?MODULE,
	{ok,Tab} = dets:open_file(Tab,{file,atom_to_list(Tab) ++ ".data"}),
	State = ets:new(?MODULE,[public,named_table]),
	ets:insert(State,{job_index,0}),
	State = dets:to_ets(Tab,State),
	% ets:insert(State,[{1,washing_clothes,5000,not_done,{}},{2,sweeping_floor,5000,not_done,{}},{3,throwing_gabage,1000,not_done,{}},{4,washing_dishes,5000,not_done,{}}]),
	{ok,State}. 
	
% user api

work_wanted() ->
	call(?MODULE,{work_wanted}). 

add_job(Fun,Time) ->
	call(?MODULE,{add_job,Fun,Time}). 
	
job_done(JobNumber) ->
	call(?MODULE,{job_done,JobNumber}). 
	
job_getStatistic() ->
	call(?MODULE,{job_getStatistic}). 
	
cancel() ->
	call(?MODULE,{cancel}). 
	
% callback functions

handle_call({work_wanted},_From,State) ->
	Match_Ret = ets:match(State,{'$1','$2','$3',not_done,'_'},1),
	case Match_Ret of
		'$end_of_table' -> 
			Fun = void,
			Index = -1,
			Time = 0,
			{reply,{not_job_now,Index,Fun,Time},State};
		{[[Index,_Fun,_Time]],_} ->
			StartTime = calendar:now_to_local_time(os:timestamp()),
			ets:update_element(State,Index,{5,StartTime}),
			ets:update_element(State,Index,{4,doing}),
			ets:to_dets(State,?MODULE),
			{reply,get_job_success,State}
	end;
handle_call({add_job,Fun,Time},_From,State) ->
	[{job_index,Index}] =  ets:lookup(State,job_index),
	ets:insert(State,[{job_index,Index+1},{Index,Fun,Time,not_done,{}}]),
	ets:to_dets(State,?MODULE),
	{reply,add_job_success,State};
handle_call({job_getStatistic},_From,State) ->
	Match_doing = ets:match(State,{'$1','$2','$3',doing,'_'}),
	io:format("Match_Ret:~w~n",[Match_doing]),
	io:format("正在进行的任务数量是:~p~n",[length(Match_doing)]),
	{reply,show_doing_job,State};
handle_call({cancel},_From,State) ->
	Match_Ret = ets:match(State,{'$1','$2','$3',doing,'_'}),
	case Match_Ret of
		[] -> {reply,no_doing_job_now,State};
		_ ->
			lists:map(fun(X) ->
					[Index,_Fun,_Time] = X,
					ets:update_element(State,Index,{4,not_done}) end,Match_Ret),
					ets:to_dets(State,?MODULE),
			{reply,cancel_doing_job,State}
	end;
handle_call({job_done,JobNumber},_From,State) ->
	case ets:member(State,JobNumber) of
		true ->
			Status_change_before = ets:lookup_element(State,JobNumber,4),
			case Status_change_before of
				not_done ->
					Reply = did_not_got_yet;
				done ->
					Reply = already_done;
				doing ->
					Reply = good_job,
					ets:update_element(State,JobNumber,{4,done}),
					ets:to_dets(State,?MODULE),
					{{SY,SM,SD},{SHH,SMM,SSS}} = ets:lookup_element(State,JobNumber,5),
					% io:format("~w~n",[Time]),
					{{Y,M,D},{HH,MM,SS}} = calendar:now_to_local_time(os:timestamp()),
					{ok,S} = file:open("done_job.data",[append]),
					io:format(S,"~w~n",[ets:lookup(State,JobNumber)]),
					io:format(S,"~s~w~s~w~s~w~s~w~s~w~s~w~n",["Start_time:",SY,"-",SM,"-",SD," ",SHH,":",SMM,":",SSS]),
					io:format(S,"~s~w~s~w~s~w~s~w~s~w~s~w~n",["End_time:",Y,"-",M,"-",D," ",HH,":",MM,":",SS])
			end,
			{reply,Reply,State};
		false ->
			{reply,job_not_found,State}
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
	