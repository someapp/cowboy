-ifndef(USER_WEBPRESENCE_HRL).
-define(USER_WEBPRESENCE_HRL, true).

-record(user_webpresence, {
	jid :: string(),
%	brandId :: string(),
	presence :: string(),
	time_stamp :: string()
}).

-record(social_presence, {
	memberId :: bitstring(),
	brandId :: bitstring(),
	presenceType :: bitstring(),
	since ::	bitstring(),
	lastseend :: bitstring()
}).

-endif.
