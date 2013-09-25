-ifndef(USER_WEBPRESENCE_HRL).
-define(USER_WEBPRESENCE_HRL, true).

-record(user_webpresence, {
	memberId :: string(),
%	brandId :: string(),
	presence :: string(),
	token :: integer()
}).

-record(social_presence, {
	memberId :: bitstring(),
	brandId :: bitstring(),
	presenceType :: bitstring(),
	since ::	bitstring(),
	lastseend :: bitstring()
}).

-endif.
