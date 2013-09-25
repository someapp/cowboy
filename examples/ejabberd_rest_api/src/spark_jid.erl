-module(spark_jid).
-include("ejabberd.hrl").
-include("jlib.hrl").


-export([split_jid/1, 
		 raw_split_jid/1,
		 get_server_name/1]).
-export([reconstruct_spark_jid/2]).

get_server_name(Jid) -> 
	{User, Server} = raw_split_jid(Jid),
	Server0 = remove_resource(Server),
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server0),
    {LUser, LServer}.

raw_split_jid(Jid) -> 
	[U, S] = re:split(Jid, "@"),
	{U, S}.

remove_resource(Server) ->
	Server0 
	= case re:split(Server, "/") of
		[S, R] -> S;
		Else -> Else	 
	end,
	Server0.
	
raw_split_jid2(Jid, Token) -> re:split(Jid, Token).
split_jid(Jid) ->  
   case raw_split_jid2(Jid,"#") of 
     [AAJid, RealJid, TokenComId] -> 
   	 	[Token, CommunityId] = re:split(TokenComId, "-"),
	    ?INFO_MSG("Found AAJid ~p MemberJid ~p Token ~p CommunityId ~p",
	    			[AAJid, RealJid,Token,CommunityId]),
	    [AAJid, RealJid,Token,CommunityId]; 
   
   
   	 [RealJid, TokenComId] -> 
   	 	[Token, CommunityId] = re:split(TokenComId, "-"),
	    ?INFO_MSG("Found MemberJid ~p Token ~p CommunityId ~p",
	    			[RealJid,Token,CommunityId]),
	    [RealJid,Token,CommunityId];
	    
	 OldJid-> 
	 	case re:split(OldJid, "-") of
	 		 [A,B] -> 
	 		 		?INFO_MSG("Found MemberJid ~p, CommunityId ~p", [A,B]),		  
	 		 		[A,B];
	 		 E->E
	    end;
	 _ -> 
	   Jid
   end.


reconstruct_spark_jid(A,C) when is_binary(A) ; is_binary(C)->
  Ret = <<A/binary,<<"-">>/binary,C/binary>>,
  erlang:binary_to_list(Ret);
reconstruct_spark_jid(A,C) when is_list(A) ; is_list(C)->
  lists:concat([A,"-",C]).
