% Deandra Spike-Madden, 500946801, Section 8
% Thuy Van Hoang, 500949895, Section 6
% Shahzeb Nizam, 500694152, Section 2

% cps721 assignment 4 natural language processing

% Database

% flights

flight(aa300,americanAirlines,ny,austin). flight(aa333,americanAirlines,toronto,london).
flight(ua69,unitedAirlines,ny,miami).flight(ua95,unitedAirlines,chicago,toronto).
flight(ua96,unitedAirlines,toronto,chicago).flight(ua99,unitedAirlines,chicago,ny).
flight(ac987,airCanada,edmonton,montreal). 
flight(ac900,airCanada,montreal,miami).
flight(ac125,airCanada,toronto,montreal). flight(ac406,airCanada,toronto,montreal).
flight(ac021,airCanada,toronto,losAngeles). flight(ac783,airCanada,toronto,losAngeles).
flight(ac128,airCanada,toronto,vancouver). flight(ac999,airCanada,toronto,hongKong).
flight(ac087,airCanada,vancouver,shanghai). flight(ac349,airCanada,vancouver,london).

% departure and arrival times

dtime(aa300,ny,1200). atime(aa300,austin,1600).
dtime(aa333,toronto,1230). atime(aa333,london,2100).
dtime(ua69,ny,1500). atime(ua69,miami,1900).
dtime(ua95,chicago,1215). atime(ua95,toronto,1330).
dtime(ua96,toronto,1515). atime(ua96,chicago,1630).
dtime(ua99,chicago,1100). atime(ua99,ny,1330).
dtime(ac987,edmonton,1130). atime(ac987,montreal,1645).
dtime(ac900,montreal,0800). atime(ac900,miami,1200).
dtime(ac021,toronto,0715). atime(ac021,losAngeles,1320).
dtime(ac783,toronto,1400). atime(ac783,losAngeles,2000).
dtime(ac125,toronto,1125). atime(ac125,montreal,1350).
dtime(ac406,toronto,1000). atime(ac406,montreal,1115).  
dtime(ac128,toronto,1400). atime(ac128,vancouver,1800). 
dtime(ac999,toronto,0100). atime(ac999,hongKong,1600). 
dtime(ac087,vancouver,1300). atime(ac087,shanghai,2300).
dtime(ac349,vancouver,0900). atime(ac349,london,1820).

% locations

location(toronto,canada). location(edmonton,canada). 
location(montreal,canada). location(vancouver,canada).
location(austin,usa). location(chicago,usa).
location(ny,usa). location(losAngeles,usa).
location(miami,usa).
location(shanghai,china). location(hongKong,china).
location(london,uk).

% Lexicon

article(a). article(an). article(the). article(any).

common_noun(flight,X) :- flight(X,From_city,To_city,Departure_time).
common_noun(city,X) :- location(X, Country).
common_noun(country,X) :- location(City,X).
% common_noun(time, X).

adjective(canadian,City) :-  location(City,canada).
adjective(american,City) :-  location(City,usa).
adjective(chinese,City) :-  location(City,china).
% adjective(morning,T).
% adjective(afternoon,T).
% adjective(day,T).
% adjective(evening,T).
% adjective(international,X).
% adjective(domestic,X).
% adjective(long,X).
% adjective(short,X).
% adjective(arrival, X).
% adjective(departure, X).

% preposition(to,X,Y).
% preposition(from,X,Y).
% preposition(in,X,Y).
% preposition(with,X,Y).
% preposition(between,X,Y).

% proper_noun(toronto).

% Parser 

what(Words, Ref) :- np(Words, Ref).

% Noun phrase can be a proper name or can start with an article 

np([Name],Name) :- proper_noun(Name).
np([Art|Rest], What) :- article(Art), np2(Rest, What).

% If a noun phrase starts with an article, then it must be followed
% by another noun phrase that starts either with an adjective
% or with a common noun

np2([Adj|Rest],What) :- adjective(Adj,What), np2(Rest, What).
np2([Noun|Rest], What) :- common_noun(Noun, What), mods(Rest,What).

% Modifiers provide an additional specific info about nouns
% Modifier can be a prepositional phrase followed by none, one or more
% additional modifiers

mods([], _).
mods(Words, What) :-
	appendLists(Start, End, Words),
	prepPhrase(Start, What),	mods(End, What).

prepPhrase([Prep|Rest], What) :-
	preposition(Prep, What, Ref), np(Rest, Ref).

appendLists([], L, L).
appendLists([H|L1], L2, [H|L3]) :-  appendLists(L1, L2, L3).