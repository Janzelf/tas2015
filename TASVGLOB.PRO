/*****************************************************************************

		Copyright (c) 1989 - 1998 De Lint Associates

 Project:  TASVPI
 FileName: TASVGLOB.PRO
 Purpose: Algemeen gebruikte predicates
 Written by: JdL
 Comments:
******************************************************************************/

include "tasvp1.inc"
include "tasvp1.con"
include "hlptopic.con"


database - alg
single waarzo(integer)
alspeler(integer SpelerNo)
single belangrijkeEventCounter(integer) 

PREDICATES

verh_str1(tijd, tijd, string)
procedure score(integer,wedscat,string,tijd LogTijd, string Noti)
betld(janee,string,janee,string,indicator,BOOLEAN beperkt)
was_op(janee, integer, wedscat, string, string, string, integer TMDag)
%add_baan(janee, string, string)
%  get_dag(tijd, integer)
  get_tijd1(tijd)
  volgende_weds1(persno,integer,wedscat,wl,wl,tijd,integer)
  volgende_weds(persno,wl,wl,wl,tijd)
  gewaarschuwd(lr,janee,janee,janee,tgnst,tgnst)
  partij(integer, wedscat, tgnst, integerlist)

%  eenOnbekend(integer, wedscat, tgnst, tgnst)
%  copyNaw1(BOOLEAN Compat, WINDOW Progress, string InFile, string OutFile)
  behandelFout(integer Foutcode)
  copyTextFile1(string InFile, string OutFile)
  afkorten(BOOLEAN, string Voornaam, string Voorletter)
  addAfgebr(string, string)
%  proef11a(string Rev, integer Pos, integer TussenSom)
%  check_nax(integer, nax, nax, integer SpNo, string Tel) - (i, i, o, o, o)
%  aggr_min(nax, nax, persno, string Tel) - (i, o, o, o)
procedure  rsoort2str(wedscat, string)
nondeterm invalidEmailChar(char)	
%determ emailadresFout(string Adres)
%  eenbye(tgnst, tgnst)
nondeterm pouleSpeler(wedscat Cat, integer Speler)
procedure rdubbels(integerlist, integerlist, integerlist) - (i, i, o)
%procedure geplaatstePoule(wedscat, string, integer) - (i, o, i)
%nondeterm geplaatsteOpgave(wedscat, string)
procedure distr(string Club, string District)
procedure zonderInhoud(string, string)
procedure kolommen(string BondsNr, string Landcode, string StD1, string GebDatum, string Club, string Distr, 
                   string Rating, string LuckyLooser, string RangLijst, kolomtypel, slist In, slist Uit)
procedure uitgesloten(string In, integer Uitgesloten, string Uit)
procedure dagmstr(integer Selectie, string Perdag, integer PerDagen)
nondeterm wd_planned2(wedid, tijd Tot, integer WNo, wedscat Cat, integer LinksRechts)
determ eenBekend(tgnst, tgnst)
determ compsexe1(sexlist, sexlist)
determ alDoorgekoppeld(wedscat, integer Speler)
procedure voornaam(string, string)
determ tweeOnbekenden(tgnst, tgnst)
procedure filterVolgende(wl, boolean, wl)
procedure landcode(boolean, string, string, string)
procedure landcodeRepr(string, string Repr)
procedure achternaamPlus(boolean, string, string, string)
procedure viaOpg(wedscat, tgnst, string)
procedure koppelVerliezer(wedscat Cat, wedscat Verl)- (i,o)

CLAUSES

waarzo(0).

%%% vpi_getUnicodeEnabled(b_False).

geplaatsteOpgave(Cat, PltsStr,Plts) :-
  opg(_,Cat,_,Plts,_,_,_,_,_,_,_),
  Plts > 0,
  str_int(PltsStr, Plts).

geplaatstePoule(Cat, StringO, 1) :-
  pos(Cat, _, _, poel), % voor kwalificatierondes niet.
  findall(Plts, geplaatsteOpgave(Cat,Plts,_), List),  
  not(List = []),
  sortstring_c(List, 1),
  list_to_string(List, ",", String), 
  format(StringO, "(%s)", String), !.  
geplaatstePoule(_, "", _).

proef11(Str) :-
  %trap(term_str(ulong, _, Str),_,fail),
  str_len(Str, 8),
  reversestr(Str, "", Rev),
  proef11a(Rev, 1, 0), !.
proef11(_Str) :-
  dlg_MessageBox( "KNLTB nummer", "nummer is ongeldig.", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ).

proef11det(Str) :-
  %trap(term_str(ulong, _, Str),_,fail),
  str_len(Str, 8),
  not(Str = "00000000"),	% 10.1.2015
  reversestr(Str, "", Rev),
  proef11a(Rev, 1, 0), !.

proef11a(String, Pos, Som) :-
  frontstr(1,String,StartString,RestString), !,
  %write("\n ", StartString),
  str_int(StartString, Int), 
  Som1 = Som + Int * Pos,
  Pos1 = Pos + 1,
  proef11a(RestString, Pos1, Som1).
proef11a(_X, _, Som) :-
  Y = Som mod 11,
  Y = 0.

searchLaatste(String, Char, BeginPos, LaatstePos) :-
  searchchar(String,Char,FoundPos),
  frontstr(FoundPos,String,_,RestString), !,
  In1 = BeginPos + FoundPos,
  searchLaatste(RestString, Char, In1, LaatstePos).
searchLaatste(_String, _Char, Pos, Pos) :-
  Pos > 0, !.		% hij moet iets gevonden hebben, anders fail
/*
bankproef("") :- !.
bankproef(String) :-
  fronttoken(String, "P", Rest),
  fronttoken(Rest, Gironummer,_),
*/
invalidEmailChar(' ').	
invalidEmailChar('/').
invalidEmailChar(':').
invalidEmailChar(',').
invalidEmailChar(';').

emailproef("") :- !.	% Als blanco OK
emailproef(Adres) :-
  not(emailadresFout(Adres)), !.
  
emailadresFout(Adres) :-
  fronttoken(Adres, Token, _),
  upper_lower(Token, "www"), !.
emailadresFout(Adres) :-
  invalidEmailChar(Char),
  searchchar(Adres,Char,_), !.			% er is een invalid character
emailadresFout(Adres) :-
  not(searchchar(Adres,'@',_)), !.		% geen @
emailadresFout(Adres) :-
  searchchar(Adres,'@',FoundPos),
  searchLaatste(Adres, '@', 0, Laatste),
  Laatste <> FoundPos, !.			% er zijn twee @-en
emailadresFout(Adres) :-
  searchchar(Adres,'@',FoundPos),
  frontstr(FoundPos,Adres,_,RestString),
  not(searchchar(RestString,'.',_)), !.		% er is geen punt na de @
/*emailadresFout(Adres) :-
  searchchar(Adres,'@',FoundPos),
  frontstr(FoundPos,Adres,_,RestString),
  searchchar(RestString,'.',Pos),
  searchLaatste(RestString, '.', 0, Laatste),
  Laatste <> Pos, !.				% er zijn twee punten */
emailadresFout(Adres) :-
  searchchar(Adres,'@',FoundPos),
  frontstr(FoundPos,Adres,_,RestString),
  searchchar(RestString,'.',PuntPos),
  str_len(RestString, Len),
  Len - PuntPos < 2, !.				% minmaal twee characters na de punt
  

numb(1).
numb(X) :- numb(A) , X=A+1.
 
member(X,[X|_]).
member(X,[_|Tail]) :- member(X,Tail).

verhl_strl(Verhinderingen,H) :-
  periode_member(Periode,Verhinderingen),
  Periode = per(K1,K2),
  verh_str1(K1, K2, H).

verh_str1(K1, K2, H) :-
  tijd_kwart(D1,HV,MV,K1),
  dag(D1,DS1,_), 
  tijd_kwart(_D2,HT,MT,K2),
  %dt_dateoffset_to_str(D2, "%YL %MD %DD", DStr),
  Van = HV * 4 + MV,
  Van <= 1,
  Tot = HT * 4 + MT,
  %write("\n", Tot),
  %Tot >= 94, !,
  Tot >= 125, !,	% 23.4.2012
  %write("\n- ",HT," ",MT),
  format(H, "% afwezig", DS1).
verh_str1(K1, K2, H) :-
  tijd_kwart(D1,U,Kw,K1),
  U = 0,
  Kw = 0,
  tijd_kwart(D2,_,_,K2),
  D2 = D1,
  dag(D2,DS2,_), 
  tijd_str(K2,TS3,_), !,
  format(H, "% tot %", DS2, TS3).
verh_str1(K1, K2, H) :-
  tijd_kwart(D1,_,_,K1),
  dag(D1,DS1,_), 
  tijd_str(K1,KS1,_),
  tijd_str(K2,TS3,_), !,
  format(H, "% % tot %", DS1, KS1, TS3).
verh_str1(K1, K2, H) :-
  tijd_kwart(D1,_,_,K1),
  dt_dateoffset_to_str(D1, "%Dn %Dd/%Md", DS1),
  tijd_str(K1,KS1,_),
  tijd_str(K2,TS3,_), !,
  format(H, "% % tot %", DS1, KS1, TS3).

periode_member(H,[H|_]).
periode_member(P,[_|Trail]) :- 
  periode_member(P,Trail).

tijd_kwart(Dag,Uur,Min,Kwart) :-
  roosterinterval(15),
  free(Dag),
  free(Uur),
  free(Min),
  bound(Kwart), !,
  bitright(Kwart,7,Dag),
  bitright(Kwart,2,T),
  bitand(T,$1F,Uur),
  bitand(Kwart,$3,M1),
  Min = M1 * 15.
tijd_kwart(Dag,Uur,Min,Kwart) :-
  roosterinterval(N),
  N <> 15, 
  free(Dag),
  free(Uur),
  free(Min),
  bound(Kwart), !,
  bitright(Kwart,7,Dag),
  bitand(Kwart, $7F, Intervals),
  Minuten = Intervals * N,
  Uur = Minuten div 60,
  Min = Minuten mod 60.
tijd_kwart(Dag,Uur,Min,Kwart) :-
  roosterinterval(15), 
  bound(Dag),             /* dag code */
  bound(Uur),
  bound(Min),
  free(Kwart), !,
  M = Min div 15,
  bitleft(Uur,2,U1),
  bitleft(Dag,7,D1),
  bitor(M,U1,M1),    
  bitor(M1,D1,Kwart).
tijd_kwart(Dag,Uur,Min,Kwart) :-
  roosterinterval(N),
  %N <> 15,
  bound(Dag),             /* dag code */
  bound(Uur),
  bound(Min),
  free(Kwart),
  Intervals = (Uur * 60 + Min) div N,
  bitleft(Dag,7,D1),
  bitor(Intervals,D1,Kwart).
 
tijd_str(K1,Str,OutDag) :-          /* zorg dat het een beetje goed oogt */  
  tijd_kwart(Dag,U1,M1,K1),
  helestr_int(0,UM1,M1),
  format(Str, "%2d:%s", U1, UM1),
  dt_dateoffset_to_str(Dag,"%YL-%MD-%DD",OutDag).


helestr_int(0,Str,Int) :-  /* voor minuten */
  Int < 10, !,
  str_int(S,Int),
  concat("0",S,Str).
helestr_int(_,Str,Int) :-
  str_int(Str,Int).

sp2opg(SpNo,Cat,OpgNo,0,Plts,s(SpNo)) :-      		/****** zoek opgave bij speler   */
  opg(OpgNo,Cat,s(SpNo),Plts,_,_,_,_,_,_,_).      		/* enkelspel                     */
sp2opg(SpNo,Cat,OpgNo,-1,Plts,pw(SpNo)) :-      		/****** zoek opgave bij speler   */
  opg(OpgNo,Cat,pw(SpNo),Plts,_,_,_,_,_,_,_).      		/* enkelspel                     */
sp2opg(SpNo,Cat,OpgNo,MaatNo,Plts,p(SpNo,MaatNo)) :-	/*      en dubbelspel            */
  opg(OpgNo,Cat,p(SpNo,MaatNo),Plts,_,_,_,_,_,_,_).
sp2opg(SpNo,Cat,OpgNo,MaatNo,Plts,p(MaatNo,SpNo)) :-
  opg(OpgNo,Cat,p(MaatNo,SpNo),Plts,_,_,_,_,_,_,_).

append([],X,X).
append([H|L],L1,[H|L2]) :-
  append(L,L1,L2).

grootste(I1, I2, I1) :-
  I1 > I2, !.
grootste(_, I2, I2).

kleinste(I1, I2, I1) :-
  I1 < I2, !.
kleinste(_, I2, I2).


sp_opgaven(SKeus, String) :-
  sp2opg(SKeus,CatN,Opg,MaatNo,_,_),
  met_maat(MaatNo, Maat),
  opg(Opg,CatN,_,Plts,Rnd,Uitsl,_,_,_,_,_),
  wc(CatN, _, _, _, _, Str, _, _, _, _, _, _, _, _, __),
  %wc(CatN,_,_,Str,_,_,_),
  uitgesloten(Str, Uitsl, Str1),
  write_maat(Str1,Maat,Plts,Rnd,String).

uitgesloten(In, 0, In) :- !.
uitgesloten(In, _, Uit) :-
  format(Uit, "U:%s", In).

met_maat(0, "") :- !.
met_maat(-1, "zoekt partner") :- !.	% 15.11.2005
met_maat(MaatNo, Maatnaam) :-
  sp2(MaatNo,_,Maat,_,_,_,_RD2,_Bet,_, _Club,_Ad, _Wnpl, _BondsNr2, _ES, _StD2O, _Gebdatum2, _, _, _, _,_,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_), !,
  %sp1(MaatNo,_,Maat,_,_,_,_,_,_Clubno,_Adr,_Wnpl,_Knltb,_Strks,_Strkd,_Geb,_Tel2,_Tel3,_Email,_Res),!,
  format(Maatnaam, "met %s", Maat).
%  trim_c(Maatnaam).

%write_uitsl(0) :- write("\n "), !.
%write_uitsl(_) :- write("\nx").

write_maat(Wc,"",0,0, Out) :- !,
  concat(Wc, "\t", Out).
write_maat(Wc,Maat,0,0, Str) :- !,
  format(Str, "%s\t%-20",Wc,Maat).
write_maat(Wc,"",X,0, Str) :- !,
  format(Str,"%s\tp:%",Wc,X).
write_maat(Wc,Maat,X,0, Str) :- !,
  format(Str,"%s\t%-18 p:%",Wc,Maat,X).
write_maat(Wc,Maat,X,Y, Str) :-
  Y1 = Y + 1,
  format(Str,"%s\t%-18 p:% r:%",Wc,Maat,X,Y1).

afkorten(b_True, Voorletters, Out) :-
  fronttoken(Voorletters, First, Rest),
  str_len(First, Len),
  Len > 1,
  frontstr(1, First, Out1, _),
  trim_c(Out1),
  trim_c(Rest),
  format(Out, " %s %s", Out1, Rest), !.
  %concat(Out1, Rest, Out), !.
afkorten(_, Out, Out).

voornaam(In, Front) :-
  searchchar(In,' ',FPos),
  FP = FPos - 1,
  trap(frontstr(FP, In, Front, _),_,true), !.
voornaam(In, In).

landcode(Voorltr, Achternaam, AchternaamU, Landcode) :-	% achternaam met/zonder Landcode
  str_len(Achternaam, Len),
  Len > 3,
  Pos = Len - 3,
  subchar(Achternaam,Pos,Char),
  Char = ' ',			% voorafgegaan door een spatie
  frontstr(Pos,Achternaam,AchternaamMin,LC),
  trim_c(AchternaamMin),
  upper_lower(Landcode, LC),
  LC = Landcode,
  achternaamPlus(Voorltr, Achternaam, AchternaamMin, AchternaamU), !.
landcode(_, Achternaam, Achternaam, "").

achternaamPlus(b_true, _, AchternaamMin, AchternaamMin) :- !.
achternaamPlus(_, Achternaam, _, Achternaam).

isLeeg("", "&nbsp;") :- !.
isLeeg(In, In). 

naamscan(nee, _, In, In, Voornaam, "") :-	% niet omdraaien
  searchchar(In, ',', Pos),
  Pos > 2,
  str_len(In, Len),
  %L1 = Pos - 1,
  L2 = Len - Pos,
  P1 = Pos + 1,
  %substring(In,1,L1,Achternaam),
  %trim_c(Achternaam),
  substring(In,P1,L2, Voorletters), 
  trim_c(Voorletters),
  voornaam(Voorletters, Voornaam), !.
naamscan(_, Voorltr, In, Out, Voornaam, LandCode) :-	% boolean Voorltr
  searchchar(In, ',', Pos),
  Pos > 2,
  str_len(In, Len),
  L1 = Pos - 1,
  L2 = Len - Pos,
  P1 = Pos + 1,
  substring(In,1,L1,Achternaam),
  trim_c(Achternaam),
  landcode(Voorltr, Achternaam, AchternaamU, LandCode),
  substring(In,P1,L2, Voorletters),
  trim_c(Voorletters),
  voornaam(Voorletters, Voornaam), 
  afkorten(Voorltr, Voorletters, VoorlettersOut),
  trim_c(VoorlettersOut),
  format(Out, "% %", VoorlettersOut, AchternaamU), !.
naamscan(_, Voorltr, In, Out, Voornaam, "") :-
  searchchar(In, ';', Pos),			% deze is speciaal voor Arnoldus
  Pos > 2,
  str_len(In, Len),
  L1 = Pos - 1,
  L2 = Len - Pos,
  P1 = Pos + 1,
  substring(In,1,L1,Achternaam),
  trim_c(Achternaam),
  substring(In,P1,L2, Voorletters),
  trim_c(Voorletters),
  voornaam(Voorletters, Voornaam),
  afkorten(Voorltr, Voorletters, VoorlettersOut),
  trim_c(VoorlettersOut),
  %frontchar(Voorletters, Chars, _),
  format(Out, "% %", VoorlettersOut, Achternaam), !.
naamscan(_, _, A,A, "", "").

addAfgebr(In, Uit) :-
  fronttoken(In, _, _), !,
  format(Uit, "(%s)", In).  
addAfgebr(_, "").

rsoort2str(Cat, "schema") :-
  pos(Cat, _, _, afval), !.
rsoort2str(Cat, "poule") :-
  pos(Cat, _, _, _), !.
rsoort2str(_, "").

repc(Omdraaien,Haakje,IBaan,Afz,Bet,Off,Dim1,Dim2,No,Cat,Tg,PlanTm,StrT,Text,TijdLog,Naam1,Naam2, Nr1, Nr2)  :-
  repa(Omdraaien,Haakje,IBaan,Afz,Bet,Off,Dim1,Dim2,No,Cat, Tg,PlanTm,StrT,Text,TijdLog,b_True, b_False,_,_,Naam1,Naam2, Nr1, Nr2,_).
%  rep(Haakje,IBaan,Afz,Bet,Off,Dim1,Dim2,No,Cat, Tg,PlanTm,StrT,Text,TijdLog,b_False, b_True,_,_).

repb(Omdraaien,Haakje,IBaan,Afz,Bet,Off,Dim1,Dim2,No,Cat,Tg,PlanTm,StrT,Text,TijdLog,Voorltr,Anchor)  :- % tascomm
  repa(Omdraaien,Haakje,IBaan,Afz,Bet,Off,Dim1,Dim2,No,Cat, Tg,PlanTm,StrT,Text,TijdLog,Voorltr, b_False,_,Anchor,_,_,_,_,_).

repa(Omdraaien,_,_,_,BetSig,_,Dim,Dim,_,_,s(No),_PlanTm,Naam2,"",0,_Voorltr,Beperkt, Club,"",Naam2, "",No,0, "")  :-% report singles
  sp2(No,_,Naam,_,_,_,_RD2,Bet,_, Club,_Ad, _Wnpl, _BondsNr2, _ES, _StD2O, _Gebdatum2, _, _, _Email, _,_,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_), !,
  naamscan(Omdraaien,b_False,Naam, Naam1,_, _LC),
  betld(BetSig,Naam1,Bet,Naam2,1,Beperkt).
repa(Omdraaien,_,_,_,BetSig,Off,_,_,No1,Cat,s(No),_PlanTm,NaamO,Score,TijdLog,Voorltr,Beperkt,Club,Anchor,NaamO,"",No,0,Noti) :-  % report singles
  bitleft(No1,1,N), N1 = N + Off, 
  score(N1,Cat,Score,TijdLog,Noti),
  sp2(No,_,Naam,_,_,_,_RD2,Bet,_, Club,_Ad, _Wnpl, _BondsNr2, _ES, _StD2O, _Gebdatum2, _, _, _Email1, _,_,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_), !,
  naamscan(Omdraaien,Voorltr, Naam, Naam1,_,_LC),
  betld(BetSig,Naam1,Bet,NaamO,1,Beperkt),
  format(Anchor,"<A name=T%ux%u></A>",N1, Cat).
repa(Omdraaien,_,_,_,BetSig,_,Dim,Dim,_,_,pw(No),_PlanTm,Naamt,"",0,_Voorltr,Beperkt, Club,"",Naam2, "zoekt partner",No,0,"")  :-% report partner wanted
  sp2(No,_,Naam,_,_,_,_RD2,Bet,_, Club,_Ad, _Wnpl, _BondsNr2, _ES, _StD2O, _Gebdatum2, _, _, _Email, _,_,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_), !,
  naamscan(Omdraaien,b_False,Naam, Naam1,_,_LC),
  betld(BetSig,Naam1,Bet,Naam2,1,Beperkt),
  format(Naamt, "%s zoekt partner", Naam2).
repa(Omdraaien,_,_,_,BetSig,Off,_,_,No1,Cat,pw(No),_PlanTm,Naamt,Score,TijdLog,Voorltr,Beperkt,Club,"",NaamO,"zoekt partner",No,0,Noti) :-
  bitleft(No1,1,N), N1 = N + Off, 
  score(N1,Cat,Score,TijdLog,Noti),
  sp2(No,_,Naam,_,_,_,_RD2,Bet,_, Club,_Ad, _Wnpl, _BondsNr2, _ES, _StD2O, _Gebdatum2, _, _, _Email1, _,_,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_), !,
  naamscan(Omdraaien,Voorltr, Naam, Naam1,_, _LC),
  betld(BetSig,Naam1,Bet,NaamO,1,Beperkt),
  format(Naamt, "%s zoekt partner", NaamO).
repa(Omdraaien,_,_,_,BetSig,_,Dim,Dim,_,_,p(No,No1),_PlanTm,Naamt,"",0,_Voorltr,Beperkt,Clubs,"",Naama,Naamb,No,No1,"") :-	% report doubles
  sp2(No,_,Naam,_,_,_,_RD2,Bet,_, Club,_Ad, _Wnpl, _BondsNr2, _ES, _StD2O, _Gebdatum2, _, _, _Email1, _,_,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_),
  %sp1(No,_,Naam,_,_,_,Bet,_,Club,_,_,_,_,_,_,_,_,_,_),						% eerste ronde
  %trim_c(Naam),
  naamscan(Omdraaien,b_False, Naam, Naam2, _, _Landcode1),
  sp2(No1,_,Naam1,_,_,_,_,Bet1,_, Club1,_, _, _, _, _, _, _, _, _Email2, _,_,_,_,_,_,_,_), !,
  %sp1(No1,_,Naam1,_,_,_,Bet1,_,Club1,_,_,_,_,_,_,_,_,_,_), !,
  %trim_c(Naam1),
  naamscan(Omdraaien,b_False, Naam1, Naam3,_,_Landcode2),
  betld(BetSig,Naam2,Bet,Naama,0,Beperkt),
  betld(BetSig,Naam3,Bet1,Naamb,1,Beperkt),
  format(Naamt, "%s+%s", Naama,Naamb),
  format(Clubs, "%s/%s", Club, Club1).
repa(Omdraaien,_,_,_,BetSig,Off,_,_,WNo,Cat,p(No,No1),_PlanTm,Naamt,Score,TijdLog,Voorltr,Beperkt,Clubs,Anchor,Naama,Naamb,No,No1,Noti) :-	% report doubles
  bitleft(WNo,1,N), NW = N + Off, 
  score(NW,Cat,Score,TijdLog,Noti),
  sp2(No,_,Naam,_,_,_,_RD2,Bet,_, Club,_Ad, _Wnpl, _BondsNr2, _ES, _StD2O, _Gebdatum2, _, _, _Email1, _,_,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_),
  %sp1(No,_,Naam,_,_,_,Bet,_,Club,_,_,_,_,_,_,_,_,_,_),
  trim_c(Naam),
  naamscan(Omdraaien,Voorltr, Naam, Naam2,_,_Landcode1),
  sp2(No1,_,Naam1,_,_,_,_,Bet1,_, Club1,_, _, _, _, _, _, _, _, _Email2, _,_,_,_,_,_,_,_), !,
  %sp1(No1,_,Naam1,_,_,_,Bet1,_,Club1,_,_,_,_,_,_,_,_,_,_), !,
  trim_c(Naam1),
  naamscan(Omdraaien,Voorltr, Naam1, Naam3,_, _Landcode2),
  betld(BetSig,Naam2,Bet,Naama,0,Beperkt),
  betld(BetSig,Naam3,Bet1,Naamb,1,Beperkt),
  format(Naamt, "%s+%s", Naama, Naamb),
  format(Clubs, "%s/%s", Club, Club1),
  format(Anchor,"<A name=T%ux%u></A>",NW, Cat).
repa(_,_,_,_,_,_,_,_,_,_,bye,_PlanTm,"--bye--","",0,_Voorltr,_,"","","--bye--","",0,0,"")     :- !.		% report byes
repa(_,_,_,_,_,Off,Dim,Dim,WNo,Cat,onbekend,_PlanTm,Tekst,"",0,_Voorltr,_,"","",Tekst,"",0,0,"")  :-	% report koppeling
  kopl(CatVan, Pl, Cat, plaats(No)),	% 14.7.2007 down???
  bitleft(WNo, 1, No1),
  No = No1 + Off,
  					% naar hier ??
  wc(CatVan, _, _, _, _, Afkort, _, _, _, _, _, _, _, _, __),
  %wc(CatVan,_/*Naam*/,_,AfKort,_,_,_), 
  %geplaatstePoule(CatVan, Plaats, Pl),
  rsoort2str(CatVan, RStr), !,
  nr2tekst(Pl, PlT),
  format(Tekst, "nr %s van %s %s", PlT, RStr, Afkort).
repa(_,_,_,_,_,_,Dim,Dim,_,_,onbekend,_PlanTm,"  -- open plaats --","",0,_Voorltr,_,"","","  -- open plaats --","",0,0,"")  :- !.
%  Dim > 0, !./* !!!   */
%repa(_,Haakje,IBaan,Afz,_,Off,_,_,No,Cat,onbekend,PlanTmDag,StrT,Text,TijdLog,_Voorltr,_Beperkt,"",Anchor,StrT,"",0,0)  :-     % ga naar voorgaande
repa(Omdraaien,Haakje,IBaan,Afz,_,Off,_,_,No,Cat,onbekend,PlanTmDag,StrT,Text,TijdLog,Voorltr,_Beperkt,"",Anchor,Naama,Naamb,0,0,Noti)  :-     % ga naar voorgaande
  bitleft(No,1,HNo), 
  HNo1 = HNo + Off,                          % wedstrijd voor geplande
  w2(HNo1,Cat,Tijd,_,_,F,TijdLog,Baan),      % tijd
  wd(HNo1,Cat,T1,T2,_Score,_,_),
  tgnst_naam(Omdraaien, Voorltr, Cat, T1, Naama, _),
  tgnst_naam(Omdraaien, Voorltr, Cat, T2, Naamb, _),
  %PlanTm1 = PlanTm + 1,
  %bitleft(PlanTm, 7, TijdTm1),
  %write("\nTijd:",Tijd," PlanTm1:",PlanTm1," TijdTm1:",TijdTm1),
  bitright(Tijd, 7, Dag),
  Dag <= PlanTmDag,
  wd(HNo1,Cat,_,_,Afgebr,_,Noti),
  addAfgebr(Afgebr, AfgebrO),
  was_op(Afz,HNo1,Cat,AfgebrO,"",Text,PlanTmDag),
  %tijd_kwart(Dag,_,_,Tijd),
  dag(Dag,DStr,_), !,
  add_baan(IBaan, Baan, BaanUit),
  tijd_str(Tijd,Str1,_),
  trim_c(DStr),
  format(StrT, "%c%c %s %s %s", Haakje, F, DStr, Str1, BaanUit),
  trim_c(StrT),
  format(Anchor,"<A name=T%ux%u></A>",HNo1, Cat).
repa(_,_,_,Afz,_,Off,_,_,No,Cat,onbekend,PlanTmDag,Text,AfgebrO,0,_Voorltr,_Beperkt,"","",Text,"",0,0,Noti) :-
  bitleft(No,1,HNo), 
  HNo1 = HNo + Off,                       /* wedstrijd voor geplande  */
  wd(HNo1,Cat,_,_,Afgebr,_,Noti), !,
  addAfgebr(Afgebr, AfgebrO),
  was_op(Afz,HNo1,Cat,"","eerder:", Text, PlanTMDag).
repa(_,_,_,_,_,Off,_,_,WNo,Cat,gelijk_spel,_PlanTm,"Gelijk spel:",Score,TijdLog,_,_Beperkt,"","","Gelijk spel:","",0,0,Noti) :-
  bitleft(WNo,1,N), NW = N + Off, 
  score(NW,Cat,Score,TijdLog,Noti), !.

repa(_,_,_,_,_,_,_,_,_,CatT,openplaats(No1),_PlanTm,Tekst,"",0,_Voorltr,_,"","",Tekst,"",0,0, "")  :-	% report koppeling
  kopl(CatV, rang(Nr), CatT, plaats(No1)),	% 24.2.2012
  wc(CatV, _, _, _, _, Afkort, _, _, _, _, _, _, _, _, __),!,
  %wc(CatV,_/*Naam*/,_,AfKort,_,_,_),  !,
  rsoort2str(CatV, RStr),
  format(Tekst, "nr %u van %s %s", Nr, RStr, AfKort).

repa(_,_,_,_,_,Off,_,1,WNo,Cat,onbekend,_PlanTm,Tekst,"",0,_Voorltr,_,"","",Tekst,"",0,0, "")  :-	% report koppeling
  kopl(CatVan, Pl, Cat, plaats(No)),	% 12.8.2006 ipv stippeltjes
  bitleft(WNo, 1, No1),
  No = No1 + Off,
  wc(CatVan, _, _, _, _, AfKort, _, _, _, _, _, _, _, _, __),
  %wc(CatVan,_/*Naam*/,_,AfKort,_,_,_), 
  %geplaatstePoule(CatVan, Plaats, Pl),
  rsoort2str(CatVan, RStr), !,
  nr2tekst(Pl, PlT),
  format(Tekst, "nr %s van %s %s", PlT, RStr, Afkort).
repa(_,_,_,_,_,_,_,_,_,_,_,_PlanTm,"......","",0,_,_,"","","......","",0,0,"noti").

zonderInhoud(In, In) :-
  trim_c(In),
  not(In = ""), !.
zonderInhoud(_, "&nbsp;").

distr(Club, Distr) :-
  club(Club, _ , District),
  concat(District, "   ", D1),
  frontstr(3,D1,Distr,_),
  trim_c(Distr),
  not(Distr=""),!.
distr(_, "&nbsp;").

kolommen(BondsNr, LandCode, StD1, GebDatum, Club, Distr, Rating, LL, RangL, [bondsnummer|Rest], In, [BondsNr1|Uit]) :- !,
  kolommen(BondsNr, Landcode, StD1, GebDatum, Club, Distr, Rating, LL, RangL, Rest, In, Uit),
  zonderInhoud(BondsNr, BondsNr1).
kolommen(BondsNr, LandCode, StD1, GebDatum, Club, Distr, Rating, LL, RangL, [landcode|Rest], In, [Landcode1|Uit]) :- !,
  kolommen(BondsNr, Landcode, StD1, GebDatum, Club, Distr, Rating, LL, RangL, Rest, In, Uit),
  zonderInhoud(Landcode, Landcode1).
kolommen(BondsNr, LandCode, St, GebDatum, Club, Distr, Rating, LL, RangL, [speelsterkte|Rest], In, [St1|Uit]) :- !,
  kolommen(BondsNr, LandCode, St, GebDatum, Club, Distr, Rating, LL, RangL, Rest, In, Uit),
  zonderInhoud(St, St1).
kolommen(BondsNr, LandCode, St, GebDatum, Club, Distr, Rating, LL, RangL, [gebdatum|Rest], In, [GebDatum1|Uit]) :- !,
  kolommen(BondsNr, LandCode, St, GebDatum, Club, Distr, Rating, LL, RangL, Rest, In, Uit),
  zonderInhoud(GebDatum, GebDatum1).
kolommen(BondsNr, LandCode, St, GebDatum, Club, Distr, Rating, LL, RangL, [club|Rest], In, [Club1|Uit]) :- !,
  kolommen(BondsNr, LandCode, St, GebDatum, Club, Distr, Rating, LL, RangL, Rest, In, Uit),
  zonderInhoud(Club, Club1).
kolommen(BondsNr, LandCode, St, GebDatum, Club, Distr, Rating, LL, RangL, [districtk|Rest], In, [Distr1|Uit]) :- !,
  kolommen(BondsNr, LandCode, St, GebDatum, Club, Distr, Rating, LL, RangL, Rest, In, Uit),
  zonderInhoud(Distr, Distr1).
kolommen(BondsNr, LandCode, St, GebDatum, Club, Distr, Rating, LL, RangL, [rating|Rest], In, [Rating1|Uit]) :- !,
  kolommen(BondsNr, LandCode, St, GebDatum, Club, Distr, Rating, LL, RangL, Rest, In, Uit),
  zonderInhoud(Rating, Rating1).
kolommen(BondsNr, LandCode, St, GebDatum, Club, Distr, Rating, LL, RangL, [wildcard|Rest], In, [LL1|Uit]) :- !,
  kolommen(BondsNr, LandCode, St, GebDatum, Club, Distr, Rating, LL, RangL, Rest, In, Uit),
  zonderInhoud(LL, LL1).
kolommen(BondsNr, LandCode, St, GebDatum, Club, Distr, Rating, LL, RangL, [ranglijst|Rest], In, [RangL1|Uit]) :- !,
  kolommen(BondsNr, LandCode, St, GebDatum, Club, Distr, Rating, LL, RangL, Rest, In, Uit),
  zonderInhoud(RangL, RangL1).
/*kolommen(BondsNr, St, GebDatum, Club, Distr, Rating, LL, [_|Rest], In, Uit) :- !,		% rangorde en wildcard
  kolommen(BondsNr, St, GebDatum, Club, Distr, Rating, LL, Rest, In, Uit).*/
kolommen(_BondsNr, _LandCode, _St, _GebDatum, _Club, _Distr, _, _, _, _, Uit, Uit).

landcodeRepr(LC, Repr) :-
  fronttoken(LC, _, _),
  schemadoel(afdruk),
  syspath(HomeDir, _),
  format(VlagFile, "%sVlaggen\\%s.png", Homedir, LC),
  existfile(VlagFile),
  format(Repr, "<IMG src=\"file:///%s\" alt=\"%s\"/ >", VlagFile, LC), !.
landcodeRepr(LC, Repr) :-
  fronttoken(LC, _, _),
  schemadoel(upload),
  format(Repr, "<IMG src=\"Vlaggen/%s.png\" alt=\"%s\"/>", LC, LC), !.
landcodeRepr(LC, LC) :-
  fronttoken(LC, _, _), !.
landcodeRepr(_, "&nbsp;").

viaOpg(Cat,Tgnst,LL) :-
  opg(_, Cat,Tgnst,_,_,_,_,LL,_,_,_), !.
viaOpg(Cat,Tgnst,"") :-
  checkAlOpgave(Cat, Tgnst).  
    
repRowH(_,Cat,_,s(No), St, Naam1, ja, KolomValues, Bondsnr11, Gebdatum11, Distr1, "", "&nbsp;", nee, [], "", "", "", "", KolomTypeL) :-
  sp2(No,_,Naam,_,_,RE,_,_,_, Club1,_Ad, _Wnpl, BondsNr1, St, _StD2O, Gebdatum1, _, _, _, _,_,RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_),
  viaOpg(Cat,s(No),LL),
  zonderInhoud(LL, LL1),			% hier zijn ze waarschijnlijk dubbel
  zonderInhoud(BondsNr1, BondsNr11),
  zonderInhoud(Gebdatum1, Gebdatum11),
  distr(Club1,Distr1), 
  zonderInhoud(RE, RE1),
  ranglijstGeldt(Cat, Gebdatum1, RangPosE, RangordeG),
  naamscan(ja,b_False, Naam, Naam1,_, LC),
  landcodeRepr(LC, Landcode),
  kolommen(BondsNr1, Landcode, St, GebDatum11, Club1, Distr1, RE1, LL1, RangordeG, KolomTypeL, [], KolomValues), !.
repRowH(_,Cat,_,pw(No), St, Naam1, ja, KolomValues, Bondsnr11, Gebdatum11, Distr1, "", "zoekt partner", ja, [], "", "", "", "<BR>", KolomTypeL) :-
  sp2(No,_,Naam,_,_,RE,_,_,_, Club1,_Ad, _Wnpl, BondsNr1, St, _StD2O, Gebdatum1, _, _, _, _,_,RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_),
  viaOpg(Cat,pw(No),LL),
  zonderInhoud(LL, LL1),			% hier zijn ze waarschijnlijk dubbel
  zonderInhoud(BondsNr1, BondsNr11),
  zonderInhoud(Gebdatum1, Gebdatum11),
  distr(Club1,Distr1), 
  zonderInhoud(RE, RE1),
  ranglijstGeldt(Cat, Gebdatum1, RangPosE, RangordeG),
  naamscan(ja,b_False, Naam, Naam1,_,LC),
  landcodeRepr(LC, Landcode),
  kolommen(BondsNr1, Landcode, St, GebDatum11, Club1, Distr1, RE1, LL1, RangordeG, KolomTypeL, [], KolomValues),  !.
repRowH(_,Cat,_,p(No,No1), StD2, Naam2, ja, KolomValues2, Bondsnr21, Gebdatum21, Distr2, StD1, Naam1, ja, Kolomvalues1, Bondsnr11, Gebdatum11, Distr1,  "<BR>", KolomTypeL) :-% report double
  sp2(No,_,Naamt,_,_,_,RD1,_,_, Club1,_Ad1, _Wnpl1, BondsNr1, _ES1, StD1, Gebdatum1, _, _, _, _,_,_RangPosE1,RangPosD1,_Incasso1,_CheckGeg1,_Log1,_),
  viaOpg(Cat,p(No,No1),LL),
  zonderInhoud(LL, LL1),
  zonderInhoud(BondsNr1, BondsNr11),
  zonderInhoud(Gebdatum1, Gebdatum11),
  distr(Club1,Distr1), 
  zonderInhoud(RD1, RD11),
  ranglijstGeldt(Cat, Gebdatum1, RangPosD1, RangordeG1),
  naamscan(ja,b_False, Naamt, Naam1,_, LC1),
  landcodeRepr(LC1, Landcode1),
  kolommen(BondsNr11, Landcode1, StD1, GebDatum11, Club1, Distr1, RD11, LL1, RangOrdeG1, KolomTypeL, [], KolomValues1), 
  sp2(No1,_,Naamt1,_,_,_,RD2,_,_, Club2,_Ad, _Wnpl, BondsNr2, _ES, StD2, Gebdatum2, _, _, _, _,_,_RangPosE,RangPosD,_Incasso,_CheckGeg,_Log,_),
  zonderInhoud(BondsNr2, BondsNr21),
  zonderInhoud(Gebdatum2, Gebdatum21),
  distr(Club2,Distr2), 
  zonderInhoud(RD2, RD21),
  ranglijstGeldt(Cat, Gebdatum2, RangPosD, RangordeG),
  naamscan(ja,b_False, Naamt1, Naam2,_, LC2),
  landcodeRepr(LC2, Landcode2),
  kolommen(BondsNr21, Landcode2, StD2, GebDatum21, Club2, Distr2, RD21, "&nbsp;", RangOrdeG, KolomTypeL, [], KolomValues2), !.
repRowH(_,_,_,bye,"", "bye", nee, [], "&nbsp;", "&nbsp;","&nbsp;","", "&nbsp;", nee, KolomValues, "&nbsp;","&nbsp;","&nbsp;", "", KolomTypeL) :- !,
  kolommen("", "", "", "", "", "", "", "", "", KolomTypeL, [], KolomValues).
repRowH(WNo, Cat, Off, onbekend,"",Tekst,nee,[], "&nbsp;","&nbsp;","&nbsp;","", "&nbsp;",nee,[], "&nbsp;","&nbsp;","&nbsp;", "", _KolomTypeL) :-
  bitleft(WNo, 1, No1),
  No = No1 + Off,
  w2(No,Cat,Tijd,_,_,_F,_TijdLog,_Baan),      % tijd
  bitright(Tijd, 7, Dag),
  prplan(PlanSel),
  plantm(DagNumP),
  DagTM = PlanSel * DagNumP,
  Dag <= DagTM,
  dag(Dag,DStr,_), !,
  %dt_dateoffset_to_str(Dag, "%Dn&nbsp;%Dd/%Md", DStr),
  tijd_str(Tijd,Str1,_),
  trim_c(DStr),
  format(Tekst, "%s %s", DStr, Str1), !.
repRowH(WNo, Cat, Off, onbekend,"",Tekst,nee,[], "&nbsp;","&nbsp;","&nbsp;","", "&nbsp;",nee,KolomValues, "&nbsp;","&nbsp;","&nbsp;", "", KolomTypeL) :-
  kopl(CatVan, Pl, Cat, plaats(No)),
  bitleft(WNo, 1, No1),
  No = No1 + Off,
  wc(CatVan, _, _, _, _, AfKort, _, _, _, _, _, _, _, _, __),
  %wc(CatVan,_Naam,_,AfKort,_,_,_), 
  rsoort2str(CatVan, RStr),
  nr2tekst(Pl, PlT), !,
  format(Tekst, "nr %s van %s %s", PlT, RStr, Afkort),
  kolommen("", "", "", "", "", "", "", "", "", KolomTypeL, [], KolomValues).
repRowH(_WNo, Cat, _Off, openplaats(No),"open",Tekst,nee,[], "&nbsp;","&nbsp;","&nbsp;","&nbsp;", "&nbsp;",nee,Kolomvalues, "&nbsp;","&nbsp;","&nbsp;", "", KolomTypeL) :-
  kopl(CatVan, Pl, Cat, plaats(No)),
  wc(CatVan, _, _, _, _, AfKort, _, _, _, _, _, _, _, _, __),
  %wc(CatVan,_/*Naam*/,_,AfKort,_,_,_), 
  rsoort2str(CatVan, RStr), 
  nr2tekst(Pl, PlT), !,
  format(Tekst, "nr %s van %s %s", PlT, RStr, Afkort),
  kolommen("", "", "", "", "", "", "", "", "", KolomTypeL, [], KolomValues).
repRowH(_WNo, _Cat, _Off, openplaats(No),"open",Tekst,nee,[], "&nbsp;","&nbsp;","&nbsp;","&nbsp;", "&nbsp;",nee,Kolomvalues, "&nbsp;","&nbsp;","&nbsp;", "", KolomTypeL) :-
  format(Tekst, "Open plaats %u", No), !,
  kolommen("", "", "", "", "", "", "", "", "", KolomTypeL, [], KolomValues).
repRowH(_,_,_,_,"","&nbsp;",nee, [], "&nbsp;","","","", "&nbsp;",nee,[],"&nbsp","","", "",_). 

card(0,1) :- !.    % determine size of grid in rows
card(1,1) :- !.
card(N,Card) :- bitright(N,1,N1),card(N1,C1),bitleft(C1,1,Card).
 
dim(0,0) :- !.     % determine size of grid in columns
dim(1,0) :- !.  
dim(N,Dim) :- bitright(N,1,N1),dim(N1,D1),Dim = D1 + 1.
  
score(No, Cat, Score,TijdLog,Noti) :-
  w3(No, Cat, _S1, _P1, _P2, _Stat, TijdLog), 
  wd(No,Cat,_,_,Score,_,Noti), !.
score(No, Cat, Score1,0,Noti) :-
  wd(No,Cat,_,_,Score,_,Noti), 
  trim_c(Score),
  not(Score = ""), !,
  format(Score1, "(%s)", Score).
score(_, _, "",0,"").

betld(ja,Naam,nee,Naam2,0,b_True) :- !,
  format(Naam2, "%-2.18s$",Naam).
betld(ja,Naam,nee,Naam2,1,b_True) :- !,
  format(Naam2, "$%-2.18s",Naam).
betld(ja,Naam,omheteven,Naam2,0,b_True) :- !,
  format(Naam2, "%-2.18s$",Naam).
betld(ja,Naam,omheteven,Naam2,1,b_True) :- !,
  format(Naam2, "$%-2.18s",Naam).
betld(_,Naam,_,Naam1,_,b_True) :-
  format(Naam1, "%-2.18s",Naam).
betld(ja,Naam,nee,Naam2,0,b_False) :- !,
  format(Naam2, "%s$",Naam).
betld(ja,Naam,nee,Naam2,1,b_False) :- !,
  format(Naam2, "$%s",Naam).
betld(ja,Naam,omheteven,Naam2,0,b_False) :- !,
  format(Naam2, "%s$",Naam).
betld(ja,Naam,omheteven,Naam2,1,b_False) :- !,
  format(Naam2, "$%s",Naam).
betld(_,Naam,_,Naam,_,b_False).

add_baan(_, "", "") :- !.
add_baan(nee,_,"") :- !.
add_baan(_, In, Uit) :-
  format(Uit, "[%s]", In).

was_op(ja,HNo1,Cat,Str,In,Out, PlanTMDag) :-
  ws(HNo1, Cat, _, Tijd),
  bitright(Tijd, 7, Dag),
  Dag <= PlanTmDag,
  tijd_kwart(D1,U1,M1,Tijd),
  dag(D1,DS2,_),
  fronttoken(DS2, T1, _),
  helestr_int(0,MS1,M1),
  format(Out, "%s (%s%s %.%)", Str, In, T1, U1, MS1), !.
was_op(_,_, _, X, _, X, _).
   
count([_|T],I,N) :- I1 = I + 1, count(T,I1,N), !.     
count(_,N,N).

telop([H|T], In, Uit) :-
  In1 = In + H, !,
  telop(T, In1, Uit).
telop(_, In, In).

get_dag1(DagTijd, D1) :-
  date(Year, Month, Day),
  dt_date_to_offset(Year, Month, Day, D1),
  bitleft(D1, 7, DagTijd), !.

get_tijd1(Time) :-
  time(Hours, Minutes, _, _),
  bitleft(Hours, 2, H),
  roosterinterval(RI),
  M = Minutes div RI,
  bitor(H, M, Time).

time_stamp1(Tijd, Dag, Hoelaat) :-  
  get_dag1(DagTijd, Dag),		% als dag dan ook 
  get_tijd1(HoeLaat), 			% echte tijd
  bitor(DagTijd, HoeLaat, Tijd), !.

time_stamp_next(Tijd, Dag, Hoelaat) :-
  huidig(D, Hoelaatx),  % D = ok
  bitleft(D, 7, DagTijd),
  bitor(DagTijd, HoeLaatx, T),
  findall(T1, bn(T1,_),Tijden),
  sorttijd_c(Tijden),
  member(Tijd, Tijden),
  Tijd >= T, !,			% eerstvolgende baantijd
  bitright(Tijd, 7, Dag),
  bitand(Tijd, $7F, Hoelaat).
time_stamp_next(T1,Dag,Hoelaat) :-
  bn(T1, _), !,			% dan maar het eerste tijdstip
  bitright(T1, 7, Dag),
  bitand(T1, $7F, Hoelaat).
time_stamp_next(0,0,0).

huidig(Dag, Tijd) :-		% gesynchroniseerd
  time_stamp1(Stamp, _DagTijd, Tijd),
  bitright(Stamp, 7, Dag),
  findall(Dx, dag(Dx,_,_), DxL),
  sorttijd_c(DxL),
  DxL = [N1|_],
  Dag >= N1,
  reverse(DxL, [], DxLRev),
  DxLRev = [N2|_],
  Dag <= N2,  !.		% tussen eerste en laatste toernooidag?
/*huidig(Dag, T) :-		% dan maar eerste planningdag!
  findall(Tijd, w2(_,_,Tijd,_,_,_,_,_), Tijden), 	% 16.10.2009 niet meer in 2009.2F
  sorttijd_c(Tijden),
  Tijden = [T|_], !,
  bitright(T, 7, Dag). */
huidig(Dag, 0) :-		% dan maar eerste kalenderdag
  findall(Dg, dag(Dg, _, _), Dagen),
  sorttijd_c(Dagen),
  Dagen = [Dag | _], !.

huidigetijdG(HTijd) :-	% niet gesynchroniseerd
  time_stamp1(HTijd, _, _).
%  date(Year, Month, Day),
%  time(Hours, Minutes, _, _),
%  dt_min_to_offset(Year,Month,Day,Hours,Minutes,HTijd).  

dagIndex(0, _, _, 0) :- !.
dagIndex(_, [], _, 0) :- !.
dagIndex(Dag, [DagS|_], In, In) :-
  dag(Dag1, DagS, _), 
  Dag <= Dag1, !.
dagIndex(Dag, [_|Dagen], In, Uit) :-
  In1 = In + 1,
  dagIndex(Dag, Dagen, In1, Uit).

wd_planned(SpNo,WEList,Totdat,VervolgWaarsch) :- 
  findall(WId,wd_planned1X(WId,SpNo,Totdat/*$7FFF*/),WList),
  %write("\nSpno:",SpNo," WList:",WList),
  volgende_weds(SpNo,WList,[],WEList1,Totdat),
  filterVolgende(WEList1, VervolgWaarsch, WEList).

filterVolgende([A,B|WList], b_False, Uit) :-
  A = w(_,Cat,TijdA,_,_,_,_,_),
  B = w(_,Cat,TijdB,_,_,_,_,_),
  TijdB > TijdA, !,
  filterVolgende([A|WList], b_False, Uit).
filterVolgende([A,B|WList], b_False, Uit) :-
  A = w(_,Cat,TijdA,_,_,_,_,_),
  B = w(_,Cat,TijdB,_,_,_,_,_),
  TijdA > TijdB, !,
  filterVolgende([B|WList], b_False, Uit).
filterVolgende([A|WList], b_False, [A|Uit]) :- !,
  filterVolgende(WList, b_False, Uit).
filterVolgende(WList, _, WList).
  
volgende_weds(_,[],Y,Y,_) :- !.
volgende_weds(SpNo,[Weds|Rest],X,Y,Totdat) :-
  Weds = w(W, Cat, _, _, _, _, _, _),
  bitright(W,1,W1),
  bitand(W,1,L_R),
  volgende_weds1(SpNo,W1,Cat,[Weds],Volg,Totdat,L_R),
  append(X,Volg,X1),
  volgende_weds(SpNo,Rest,X1,Y,Totdat).
    
volgende_weds1(_,0,_,Out,Out,_,_) :- !.
volgende_weds1(_,_,Cat,Out,Out,_,_) :- 
  pos(Cat, _, _, Soort),
  not(Soort = afval), !.
volgende_weds1(SpNo,W,Cat,In,[w(W,Cat,Tijd,Gewaarschuwd,L_R,Tgnst, b_True, BaanPark)|Out],Totdat,L_R) :-
  wd(W,Cat,T1,T2,_,_,_),
  w2(W,Cat,Tijd,G1,G2,_,_,BaanPark), 
  Tijd < Totdat, !,
  lr(Tgnst,T1,T2,L_R),
  gewaarschuwd(L_R, G1, G2, Gewaarschuwd, T1, T2),
  bitright(W,1,W1),
  bitand(W,1,L_R1),
  volgende_weds1(SpNo,W1,Cat,In,Out,Totdat,L_R1).
volgende_weds1(SpNo,W,Cat,In,Out,Totdat,_) :-
  bitright(W,1,W1),
  bitand(W,1,L_R1),
  volgende_weds1(SpNo,W1,Cat,In,Out,Totdat,L_R1).
    
gewaarschuwd(0,X,_,X,_,_) :- !.
gewaarschuwd(1,_,Y,Y,_,_) :- !.

wd_planned1X(WId,SpNo,Tot) :-
  %write("\n"),
  pos(Cat, _, _, _),	% zelfde cats bij elkaar voor filteren
  wedstrijd(WNo,Cat,SpNo,LinksRechts,_Zelf,Andere),
  not(Andere = bye),
  wd_planned2(WId, Tot, WNo, Cat, LinksRechts).

wd_planned2(WId, Tot, WNo, Cat, LinksRechts) :-
  w2(WNo,Cat,Tijd,G1,G2,_,_,BaanPark),
  wd(WNo,Cat,T1,T2,_,_,_),
  lr(Tgnst,T1,T2,LinksRechts),
  Tijd < Tot,
  gewaarschuwd(LinksRechts,G1,G2,Gewaarschuwd,T1,T2),
  WId = w(WNo,Cat,Tijd,Gewaarschuwd,LinksRechts,Tgnst,b_False,BaanPark).   
wd_planned2(WId, Tot, WNo, Cat, _XLinksRechts) :-
  WNo > 1,
  pos(Cat, _, _, afval),
  not(w2(WNo,Cat,_,_,_,_,_,_)),
  not(w3(WNo,Cat,_,_, _, _, _)),
  bitright(WNo, 1, WNo1),
  bitand(WNo, 1, LinksRechts),
  wd_planned2(WId, Tot, WNo1, Cat, LinksRechts).

wedstrijd(WNum,Cat,SpelerNum,0,p(SpelerNum,P),Andere) :-     % Links = 0; Rechts = 1
  wd(WNum,Cat,p(SpelerNum,P),Andere,_,_,_).
wedstrijd(WNum,Cat,SpelerNum,1,p(SpelerNum,P),Andere) :-
  wd(WNum,Cat,Andere,p(SpelerNum,P),_,_,_).
wedstrijd(WNum,Cat,SpelerNum,0,p(P,SpelerNum),Andere) :-
  wd(WNum,Cat,p(P,SpelerNum),Andere,_,_,_).
wedstrijd(WNum,Cat,SpelerNum,1,p(P,SpelerNum),Andere) :-
  wd(WNum,Cat,Andere,p(P,SpelerNum),_,_,_).
wedstrijd(WNum,Cat,SpelerNum,0,s(SpelerNum),Andere) :-
  wd(WNum,Cat,s(SpelerNum),Andere,_,_,_).
wedstrijd(WNum,Cat,SpelerNum,1,s(SpelerNum),Andere) :-
  wd(WNum,Cat,Andere,s(SpelerNum),_,_,_).
wedstrijd(WNum,Cat,SpelerNum,0,pw(SpelerNum),Andere) :-
  wd(WNum,Cat,pw(SpelerNum),Andere,_,_,_).
wedstrijd(WNum,Cat,SpelerNum,1,pw(SpelerNum),Andere) :-
  wd(WNum,Cat,Andere,pw(SpelerNum),_,_,_).

%____________________________________________________________________________
 
sp_for_c(Persno,Sex,Naam,Tel,Verhl,-1,Betaald,UitKant) :-
  sp2(PersNo,Sex,Naam,Tel,Verhl,_RatingE,_RatingD,Betaald,UitKant, _Cl,_Ad, _Wnpl, _BondsNr, _ES, _DS, _Geb, _Tel2, _Tel3, _EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Res).

sp_for_c_tel(Persno,Sex,Naam,Tel,Tel2,Tel3,Verhl,-1,Betaald,UitKant,Leeftijd) :-
  sp2(PersNo,Sex,Naam,Tel,Verhl,_RatingE,_RatingD,Betaald,UitKant, _Cl,_Ad, _Wnpl, _BondsNr, _ES, _DS, Geb, Tel2, Tel3, _EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Res),
  leeftijd(Geb, Leeftijd, 0, _),
  Leeftijd > 0, !.
sp_for_c_tel(Persno,Sex,Naam,Tel,Tel2,Tel3,Verhl,-1,Betaald,UitKant,99) :-	% leeftijd niet herkenbaar
  sp2(PersNo,Sex,Naam,Tel,Verhl,_RatingE,_RatingD,Betaald,UitKant, _Cl,_Ad, _Wnpl, _BondsNr, _ES, _DS, _Geb, Tel2, Tel3, _EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Res), !.

spsrt_for_c(SrtNo,SrtNaam,ManVr, LftL, LftU, StrkL, StrkU, Verhl) :- 
  spsrt(SrtNo, SrtNaam, ManVr, LftL, LftU, StrkL, StrkU, Verhl). 

opg_for_c(Nummer,Cat,Tgnst,Plaats,Ronde,Ongebr,Ongebr1) :-
  opg(Nummer,Cat,Tgnst,Plaats,Ronde,Ongebr,Ongebr1,_,_TeamId,_Keur,_Melding) .

wc_for_c(Wedscat,Naam,Type,AfKort,Catlist,Contr,VerlRonde) :-
  wc(Wedscat, _, _, Naam, Type, AfKort, CatList/*??*/, _SpStrk, _LftV, _LftTM, _HDG, _Schemasoort, _Keuring, _KNLTB, Contr),
  koppelVerliezer(Wedscat, VerlRonde).

koppelVerliezer(Cat, VCat) :-
  kopl(VCat, verliezer, Cat, _), !.
koppelVerliezer(_Cat, 0).

wd_for_c(WedsNo,Cat,Tgnst1,Tgnst2,Uitslag,Vrij) :-
  wd(WedsNo,Cat,Tgnst1,Tgnst2,Uitslag,Vrij,_).

w2_for_c(WedsNo,Cat,Tijd,Gewaarsh1,Gewaarsch2,Fixed,Vrij) :-
  w2(WedsNo,Cat,Tijd,Gewaarsh1,Gewaarsch2,Fixed,Vrij,_). % zonder baanno!

w3_for_c(WedsNo,Cat,Winnaar,Vrij) :-
  w3(WedsNo,Cat,Winnaar,_PW, _PV, _Stat, Vrij).

ws_for_c(WedsNo, Cat, Partij, Tijd) :-
  ws(WedsNo, Cat, Partij, Tijd).

bn_for_c(Tijd,Aantal) :-
  bn(Tijd,Aantal).
bn_for_c(Tijd, 0) :-
  bn_t(Tijd).
  
pos_for_c(WedsCat, Card, Pos, Poel_of_Afval) :-
  pos(WedsCat, Card, Pos, Poel_of_Afval).

dag_for_c(DagNo,Naam,Werkdag) :-
  dag(DagNo,Naam,Werkdag).

betaal_adm_for_c(JaNee) :- 
  betaal_adm(JaNee).

oproepen_for_c(JaNee) :-
  oproepen(Janee).

sleuteltekst_for_c(String) :- 
  sleuteltekst(String1), 
  reversestr(String1, "", String), !.

naam_for_c(Naam, 0, Datum) :- naam(Naam, _Nummer, Datum, _, _, _).

blok_for_c(Cat, BlokLijst) :- bl(Cat, BlokLijst).

blLft_for_c(Leeftijd, BlokLijst) :-		% leeftijdsverhindering
  blLft(Van, Tot, BlokLijst),
  Leeftijd >= Van,
  Leeftijd <= Tot, !.

%           integer Separatie, integer Duur, integer Enkel, integer DagMax) - (o, o, o, o, o, o, o, o, o) language stdcall
planparam_c(EersteDag, Aantal, PerWat, PerWatStr, PerDagen, MinSeparatie, MaxSeparatie, Duur, MaxEnkels, DagMax, DVEbool) :-  	% 15.8.2016  MaxSeparatie nieuw!
  dag_max_o(Aantal, PerWat),
  dagmstr(PerWat, PerWatStr, PerDagen),
  dag(DagNo,_Naam,_Werkdag),
  EersteDag = DagNo, 			% 2.12.2002 eerst maar eenvoudig
  separatie(MinSeparatie),
  maxseparatie(MaxSeparatie),
  duur(Duur),
  singles(MaxEnkels),
  dag_max(DagMax),
  dubbelvoorEnkel(DVE),
  janee2bool(DVE, DVEbool), !.
  %dt_offset_to_date(DagNo, _, _, _, _, DagVandeWeek),
  %EersteDag = DagNo - DagVandeWeek + 1, !.
  
dagmstr(1, "per 2 dagen", 2) :- !.
dagmstr(2, "per week", 7) :- !.
dagmstr(3, "per twee weken", 14) :- !.
dagmstr(4, "per vier weken", 28) :- !.
dagmstr(5, "per vijf weken", 35) :- !.	
dagmstr(_,"per dag", 1).		% = 0
   
rdubbels([H | Rest], In, Uit) :-
  alspeler(H), !,
  rdubbels(Rest, In, Uit).
rdubbels([H | Rest], In, [H | Uit]) :-
  assert(alspeler(H)), !,
  rdubbels(Rest, In, Uit).
rdubbels(_, In, In) :-
  retractall(alspeler(_)).
  
de_spelers(Num, Cat, T1, T2, Uit) :-
  bitleft(Num, 1, N0),
  partij(N0, Cat, T1, Part1),		% partij boven
  N1 = N0 + 1,
  partij(N1, Cat, T2, Part2),		% partij beneden
  append(Part1, Part2, Spelers),	% voeg samen
  rdubbels(Spelers, [], Uit).

partij(_, _, s(P1), [P1]) :- !.
partij(_, _, pw(P1), [P1]) :- !.
partij(_, _, p(P1, P2), [P1, P2]) :- !.
partij(Num, Cat, onbekend, Spelers) :-	% ga dan een terug
  not(pos(Cat, _, _, poel)),		% maar niet als poule
  wd(Num, Cat, T1, T2,_,_,_), !,		% als die bestaat
  de_spelers(Num, Cat, T1, T2, Spelers).% neem daar de spelers van
partij(_Num, Cat, onbekend, Spelers) :-	% ga dan een terug
  not(pos(Cat, _, _, poel)),		% maar niet als poule
  kopl(CatV, pl3en4(_), Cat, _),	% pl 3/4 onderdeel + koppeling?
  wd(2, CatV, T1, T2,_,_,_),		% als die bestaat
  de_spelers(2, CatV, T1, T2, Spelers1),% neem daar de spelers van
  wd(3, CatV, T3, T4,_,_,_), !,		% als die bestaat
  de_spelers(3, CatV, T3, T4, Spelers2),% neem ook daar de spelers van
  append(Spelers1,Spelers2,Spelers).    % en voeg samen
partij(Num, Cat, onbekend, Spelers) :-	% ga dan een terug	% 5.12.2001
  kopl(CatV, _, Cat, plaats(Num)),	% koppeling (niet uitgevoerd)
  pos(CatV, _, _, poel), !,		% naar poule?
  findall(Speler, pouleSpeler(CatV, Speler), Spelers).	% neem daar de spelers van
partij(_Num, Cat, openplaats(X), Spelers) :-	% open plaats in een poule
  kopl(CatV, _, Cat, plaats(X)),	% koppeling (niet uitgevoerd)
  pos(CatV, _, _, poel), !,		% terug naar poule?
  findall(Speler, pouleSpeler(CatV, Speler), Spelers).	% neem daar de spelers van
partij(_, _, _, []). 

pouleSpeler(CatV, Speler) :-
  wd(Num, CatV, T1, T2,_,_,_), 		% als die bestaat
  %w2(Num,CatV,_,_,_,_,_,_),  		% 19.9.2002 niet gepland
  de_spelers(Num, CatV, T1, T2, Spelers),
  member(Speler, Spelers),
  not(alDoorgekoppeld(CatV, Speler)).	% 9.9.2004

aldoorgekoppeld(CatV, Speler) :-
  kopl(CatV, _, Cat, IPV),	% koppeling
  koplUitgevoerd(Cat, IPV, Tgnst),
  tgnstNum(Tgnst, SpelerK),
  Speler = SpelerK, !.
    

tweeOnbekenden(onbekend, onbekend).

/*invoerbaar(Num, Cat, b_True) :-
  wd(Num,Cat,S1,S2,_,_), 
  not(S1 = bye),
  not(S2 = bye),
  not(S1 = onbekend),
  not(S2 = onbekend), 
  not(S1 = openplaats(_)),
  not(S2 = openplaats(_)), !.*/
invoerbaar(Num, Cat, b_True) :-
  wd(Num,Cat,S1,S2,_,_,_), 
  not(S1 = bye),
  not(S2 = bye),
  not(tweeOnbekenden(S1, S2)),
  not(S1 = openplaats(_)),
  not(S2 = openplaats(_)), !.
invoerbaar(_Num, _Cat, b_False).

corrigeerbaar(Num, Cat, b_True) :-
  w3(Num,Cat,_,_,_,_,_), !.
corrigeerbaar(_, _, b_False).

planbaar(Num, Cat, b_True) :-
  wd(Num, Cat, T1, T2, _, _, _), 
  not(T1 = bye),
  not(T2 = bye),
  %not(eenOnbekend(Num, Cat, T1, T2)),	% NB en ongepland
  not(w3(Num,Cat,_,_,_,_,_)), !.
planbaar(_, _, b_False).


planMeteen( b_True) :-
  wd(Num, Cat, T1, T2, _, _, _), 
  not(T1 = bye),
  not(T2 = bye),
  not(w2(Num,Cat,_,_,_,_,_,_)),
  eenBekend(T1, T2),
  not(w3(Num,Cat,_,_,_,_,_)), !.
planMeteen(b_False).

eenBekend(p(_,_), _) :- !.
eenBekend(_, p(_,_)) :- !.
eenBekend(s(_), _) :- !.
eenBekend(_, s(_)) :- !.
eenBekend(pw(_), _) :- !.
eenBekend(_, pw(_)).

beeldBaar(Num, Cat, b_True) :-
  pos(Cat, Top, _, _),
  Num < 2 * Top, 
  wd(Num,Cat,T1, T2,_,_, _),
  not(T1 = bye),
  not(T2 = bye),
  not(w3(Num,Cat,_,_,_,_,_)), !.  
beeldBaar(_Num, _Cat, b_False).

baanbaar(Num, Cat, b_True) :-
  baan_adm(ja),
  w2(Num,Cat,_Tijd,_,_,_Fix,_,_Baan), !.
baanbaar(_Num, _Cat, b_False).


wisplanbaar(Num, Cat, b_True) :-
  w2(Num,Cat,_Tijd,_,_,_Fix,_,_Baan), !.
wisplanbaar(_, _, b_False).

jaNee2Bool(ja, b_True) :- !.
jaNee2Bool(nee, b_False).

strepen(0,_) :- !.
strepen(N,Char) :-
  write(Char),
  N1 = N - 1,
  strepen(N1,Char).

selectGeen(ListWin) :-
  %ListWin = win_GetCtlHandle(_Win, idca_dagen),
  Aantal = lbox_CountAll(ListWin),
  Aantal > 0,
  lbox_Suspend(ListWin),
  numb(N),
  N1 = N - 1,
  lbox_SetSel(ListWin, N1, b_False),
  N >= Aantal, 
  lbox_Resume(ListWin), !.
selectGeen(_).

selectAlle(ListWin) :-
  Aantal = lbox_CountAll(ListWin),
  Aantal > 0,
  lbox_Suspend(ListWin),
  numb(N),
  N1 = N - 1,
  lbox_SetSel(ListWin, N1, b_True),
  N >= Aantal, 
  lbox_Resume(ListWin), !.
selectAlle(_).

getWcDisp(Catno, Size) :-
  wcDisp(CatNo, Size), !.
getWcDisp(_CatNo, Size) :-
  schemaFontSize(Size), !.
  
updateWcDisp(CatNo, Size) :-
  retractall(wcDisp(Catno, _)),
  assert(wcDisp(CatNo, Size)), !.
updateWcDisp(_, _).

reverse([H|T], In, Out) :- !,
  reverse(T, [H|In], Out).
reverse(_, In, In).
  
reversestr(InS, In, Out) :-
  frontstr(1, InS, CS, InS1),
  concat(CS, In, In1), !,
  reversestr(InS1, In1, Out).
reversestr(_, In, In) :- !.  

compsexes(L1, L2) :-
  compsexe1(L1, L2),
  compsexe1(L2, L1).

compsexe1([H|T], List2) :-
  member(H, List2), !,
  compsexe1(T, List2).
compsexe1([], _List2).
  
behandelfout(7030) :-
  closefile(work),
  closefile(nawf), 
  dlg_MessageBox( "", "Fout bij het lezen.\nOperatie afgebroken.", mesbox_iconerror, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  !.
behandelFout(Ret) :-
  closefile(work),
  closefile(nawf), 
  exit(Ret).

copyTextFile(INFileNaam, UitFileNaam) :-
  trap(copyTextFile1(InFileNaam, UitFileNaam), Ret, behandelFout(Ret)), !.
copyTextFile(_, _) :-
  closefile(nawf),	% 28.5.2012
  closefile(work).

copyTextFile1(In, Uit) :-
  %writedevice(screen),
  %write("\nCopyT"),
  myOpenread(0, nawf, In),
  %filePos(nawf,1,2),
  %filePos(nawf, Len, 0),
  %filePos(nawf, 0, 0),
  closefile(work),
  %maakRW(Uit),
  openwrite(work, Uit),
  readdevice(nawf),
  writedevice(work),
  repeat_f(nawf),
  readln(Record),
  %filePos(nawf, Current, 0),
  write(Record, '\n'),
  % flush(work),		Toch maar niet!
  %progress_bar_set_value(ProgressWin, Len, Current),
  vpi_ProcessEvents(b_True ),
  fail.
/*copyTextFile1(_, _) :-
  closefile(nawf),
  closefile(work). */


%BEGIN_WIN Editor
/**************************************************************************
	Creation and event handling for window: "Editor"
**************************************************************************/

database - editeur
  editeurwindow(window)

constants

%BEGIN Editor, CreateParms, 15:42:38-19.4.1999, Code automatically updated!
  win_editor_WinType = w_TopLevel
  win_editor_Flags = [wsf_SizeBorder,wsf_TitleBar,wsf_Maximize,wsf_Minimize,wsf_Close,wsf_ClipSiblings]
  win_editor_RCT = rct(100,80,764,525)
  win_editor_Menu = no_menu
  %win_editor_Title = "Editor"
  win_editor_Help = idh_inhoud
%END Editor, CreateParms

predicates

  win_editor_eh : EHANDLER

clauses

close_editorwin() :-
  editeurwindow(Win),
  win_Destroy(Win), !.
close_editorwin().

  win_editor_Create(_Parent, Text, Titel):-
	close_editorwin(),
ifdef use_editor
	%Text = "",
	Font = font_Create(ff_Fixed,[],10),
	ReadOnly = b_false, Indent = b_true, InitPos = 1,
	edit_Create(win_editor_WinType,win_editor_RCT,Titel,
		 win_editor_Menu,_Parent,win_editor_Flags,Font,ReadOnly,
		 Indent,Text,InitPos,win_editor_eh),
enddef
	true.

%BEGIN Editor, e_Create
  win_editor_eh(_Win,e_Create(_),0):-!,
  vensterpositie(editorvenster, _Win),
  assert(editeurwindow(_Win)),
%BEGIN Editor, InitControls, 15:42:38-19.4.1999, Code automatically updated!
%END Editor, InitControls
%BEGIN Editor, ToolbarCreate, 15:42:38-19.4.1999, Code automatically updated!
%END Editor, ToolbarCreate
	!.
%END Editor, e_Create

%BEGIN Editor, e_Menu, Editor default popup menu 
  win_editor_eh(_Win,e_Menu(ID,_CAS),0):-
	ID >= edit_MenuUndo, ID <= edit_MenuFont, 
	!,fail.
%END Editor, e_Menu, Editor default popup menu
%MARK Editor, new events

%BEGIN Editor, e_Destroy
  win_editor_eh(_Win,e_Destroy,0):-!,
	updatevensterpositie(editorvenster, _Win),
	retractall(editeurwindow(_)),
	!.
%END Editor, e_Destroy

%BEGIN Editor, e_Size
  win_editor_eh(_Win,e_Size(_Width,_Height),0):-!,
ifdef use_tbar
	toolbar_Resize(_Win),
enddef
	!.
%END Editor, e_Size

%BEGIN Editor, e_Menu, Parent window 
  win_editor_eh(Win,e_Menu(ID,CAS),0):-!,
	PARENT = win_GetParent(Win),
	win_SendEvent(PARENT,e_Menu(ID,CAS)),
	!.
%END Editor, e_Menu, Parent window

%END_WIN Editor


%determ checkhasp(WINDOW, integer, integer)
%_______________________________________________________________

/*checkhaspt(_,-100) :-  retract(fullfunc()), !.
checkhaspt(_,-101) :-  retract(fullfunc()), !.
checkhaspt(_,-102) :-  retract(fullfunc()), !.
checkhaspt(0,_)    :-  retract(fullfunc()), !.
checkhaspt(_,_).

checkhaspf(_, 100) :-  !, fail.
checkhaspf(_,-101) :-  !, fail.
checkhaspf(_,-102) :-  !, fail.
checkhaspf(0,_)    :-  !, fail.
checkhaspf(_,_). */

%______________speeltijden______________________________________________

database - rondes
  algeweestW(wedscat)

predicates
  splitwt(wt,wtl,wtl,wtl)
  splitw(wedid,wl,wl,wl)
  prt_gewaarsch1(janee,char,janee,integer)
procedure prt_wasop(integer Num, wedscat Cat, tgnst, string, char Verzet)
procedure prt_wasop1(integer Num, wedscat Cat, tgnst, string)
  nondeterm afzeggenS(persno, string)
  nondeterm te_plannenS(persno, string)
  vOfQ(char, char, char)
  saveWorkIndex(integer LR, wedid, integer Index)
  vervolgWeds(integer Num, wedscat Cat, string, slist, integer Delta)
  maxPlan(wedscat, tijd)
  procedure metLocatie(string, string)
%procedure xSpelsoort1(string , wedsex)	% ook enkel kan gemengd zijn
procedure wedstypConvert(wedstyp,string,wedstyp)	% alleen nog maar enkel en dubbel
nondeterm voorrondeA(wedscat Cat, wedscat VoorrondeX)
determ getNth1(integer, integer, catl, wedscat)

clauses

splitw(_,[],[],[]).
splitw(w(N,C,T,W,LR,P,Verv,BP),[w(N1,C1,T1,W1,LR1,P1,Verv1,BP1)|X],[w(N1,C1,T1,W1,LR1,P1,Verv1,BP1)|Y],Z) :-
  T > T1, !,
  splitw(w(N,C,T,W,LR,P,Verv,BP),X,Y,Z).
splitw(H,[A|X],Y,[A|Z]) :- !,
  splitw(H,X,Y,Z).

qsortw([],[]).
qsortw([H|T],S) :-
  splitw(H,T,A,B),
  qsortw(A,A1),
  qsortw(B,B1),
  append(A1,[H|B1],S), !.

splitwt(_,[],[],[]).
splitwt(wt(S,T,G,B,BW, Was),[wt(S1,T1,G1,B1,BW1, Was1)|X],[wt(S1,T1,G1,B1,BW1, Was1)|Y],Z) :-
  T > T1, !,
  splitwt(wt(S,T,G,B,BW, Was),X,Y,Z).
splitwt(H,[A|X],Y,[A|Z]) :- !,
  splitwt(H,X,Y,Z).

qsortwt([],[]).
qsortwt([H|T],S) :-
  splitwt(H,T,A,B),
  qsortwt(A,A1),
  qsortwt(B,B1),
  append(A1,[H|B1],S), !.

speeltijden1([],[]) :- !.   
speeltijden1([w(No,Cat,WedsTijd,Gewaarsch, _, Tgnst, BW, BaanPark)|Rest],
				[wt(Wedstyp,WedsTijd,Gewaarsch,BaanPark,BW, WasOp)|Out]) :-
  prt_wasop1(No,Cat,Tgnst,WasOp),
  wc(Cat, _, _, _, _, WedsTyp, _, _, _, _, _, _, _, _, _), !,
  %wc(Cat,_,_,WedsTyp,_,_,_), !,
  speeltijden1(Rest,Out).
speeltijden1([_|Rest],Out) :-
  speeltijden1(Rest,Out).

prt_wasop(Num,Cat,Tgnst,Str,'v') :-
  oproepen(ja),
  ws(Num,Cat,Tgnst,Tijd),
  tijd_kwart(D1,U1,M1,Tijd),
  dag(D1,DS1,_), 
  helestr_int(0,MS1,M1), !,
  format(Str,"(%-7 %2\46%2)",DS1,U1,MS1).
prt_wasop(_Num,_Cat,_Tgnst, "",' ').

prt_wasop1(Num,Cat,Tgnst,Str) :-
  oproepen(ja),
  ws(Num,Cat,Tgnst,Tijd),
  tijd_kwart(D1,U1,M1,Tijd),
  dag(D1,DS1,_), 
  helestr_int(0,MS1,M1), !,
  format(Str,"dus niet: %-7 %2\46%2!",DS1,U1,MS1).
prt_wasop1(_Num,_Cat,_Tgnst, "").

bijWinst(b_True, "bij winst", "bw") :- !.
bijWinst(_, "", "").

metLocatie(_Baan, "") :-
  baan_adm(nee), !.
metLocatie(Baan, BStr) :-
  format(BStr, " [%s]", Baan).

speeltSList(_,[],X,X,_,S,S,A,A) :- !.
speeltSList(LR,[w(Num,Cat,Tijd,Gewaarsch,LRInWeds,T,Verv,BaanPark)|Rest],In,Uit,Vraagteken,InS,UitS1,Ain,Auit) :-
  wc(Cat, _, _, _, _, Type, _, _, _, _, _, _, _, _, _),
  %wc(Cat,_,_,Type,_,_,_), 
  tijd_kwart(D1,U1,M1,Tijd),
  dag(D1,DS1,_), !,
  helestr_int(0,MS1,M1),
  prt_gewaarsch1(Gewaarsch,Gstring,Vraagteken,A),
  prt_wasop(Num,Cat,T,WasOpStr,Verzet),
  vOfQ(Gstring, Verzet, Vout),
  Ain1 = Ain + A,
  bijWinst(Verv, BijWinst, _),
  metLocatie(BaanPark, BaanPStr),
  format(Str,"%c\t%-4 %-7 %2\46%2 %1 % %",Vout,Type,DS1,U1,MS1,WasOpStr, BijWinst, BaanPStr),
  saveWorkIndex(LR, w(Num,Cat,Tijd,Gewaarsch, LRInWeds, T,Verv,BaanPark), In),
  bitright(Num, 1, Num1),
  vervolgWeds(Num1, Cat, Str, StrPlusVervolg, Indelta),
  In1 = In + 1 + Indelta,
  speeltSList(LR,Rest,In1,Uit,Vraagteken,InS,UitS, Ain1,Auit),
  append(StrPlusVervolg, UitS, UitS1).

speeltSListAfdr(_,[],X,X,_,S,S,A,A) :- !.
speeltSListAfdr(LR,[w(Num,Cat,Tijd,Gewaarsch,_LRInWeds,T,Verv,BaanPark)|Rest],In,Uit,Vraagteken,InS,UitS1,Ain,Auit) :-
  wc(Cat, _, _, _, _, Type, _, _, _, _, _, _, _, _, _),
  %wc(Cat,_,_,Type,_,_,_), 
  tijd_kwart(D1,U1,M1,Tijd),
  dag(D1,DS1,_), !,
  helestr_int(0,MS1,M1),
  prt_gewaarsch1(Gewaarsch,_Gstring,Vraagteken,A),
  prt_wasop(Num,Cat,T,WasOpStr,_Verzet),
  Ain1 = Ain + A,
  bijWinst(Verv, BijWinst, _),
  metLocatie(BaanPark, BaanPStr),
  format(Str,"%-4 %-7 %2\46%2 %1 %s%s",Type,DS1,U1,MS1,WasOpStr, BijWinst, BaanPStr),
  trim_c(Str),
  %saveWorkIndex(LR, w(Num,Cat,Tijd,Gewaarsch, LRInWeds, T,Verv,BaanPark), In),
  bitright(Num, 1, Num1),
  vervolgWeds(Num1, Cat, Str, StrPlusVervolg, Indelta),
  In1 = In + 1 + Indelta,
  speeltSListAfdr(LR,Rest,In1,Uit,Vraagteken,InS,UitS, Ain1,Auit),
  append(StrPlusVervolg, UitS, UitS1).

vervolgWeds(Num, Cat, In, [In, Verv], 1) :-
  w2(Num,Cat,Tijd,_,_,_F,_TijdLog,Baan),      % tijd
  tijd_kwart(Dag,_,_,Tijd),
  dag(Dag,DStr,_), !,
  tijd_str(Tijd,Str1,_),
  metLocatie(Baan, BaanPStr),
  format(Verv, "  bij winst: %s %s%s", DStr, Str1,BaanPStr), !.
vervolgWeds(_Num, _Cat, In, [In], 0).

saveWorkIndex(LR, w(Num,Cat,_Tijd,nee, LRInWeds, T, Verv, _), Index) :-
  assert(oproepItem(LR, Index, mijnoproepen, Num, Cat, T, LRInWeds, Verv)), !.
saveWorkIndex(_, _, _).

vOfQ(_, 'v', 'v') :- !.
vOfQ(X,   _, X).

prt_gewaarsch1(nee,'?',ja,1) :-
  oproepen(ja), !.
prt_gewaarsch1(_,' ',_,1).	% 29.8.2002

prt_speelt([],X,X,_,_,_,S,S,A,A) :- !.
prt_speelt([wt(Type,Tijd,Gewaarsch,BaanPark,BW, WasOp)|Rest],In,Uit,Vraagteken,1,Kol,InS,[Str|UitS],Ain,Auit) :-
  tijd_kwart(D1,U1,M1,Tijd), !,
  %dag(D1,DS1,_), !,
  %dt_dateoffset_to_str(Dag, "%Dd-%Md-%YL", DatumN),
  dt_dateoffset_to_str(D1, "%Dn %DD-%MD-%YL", Repr0),
  helestr_int(0,MS1,M1),
  prt_gewaarsch1(Gewaarsch,Gstring,Vraagteken,A),
  Ain1 = Ain + A,
  bijwinst(BW, Tekst, _),
  %format(Str,"%-4 %-7 %2\46%2 %c %s %s %s",Type,DS1,U1,MS1,Gstring,BaanPark,Tekst, WasOp),
  format(Str,"%-4 %s %2:%2 %c %s %s %s",Type,Repr0,U1,MS1,Gstring,BaanPark,Tekst, WasOp),	% 11.10.2017
  In1 = In + 1,
  prt_speelt(Rest,In1,Uit,Vraagteken,1,Kol,InS,UitS, Ain1,Auit).

te_plannen(PersNo, WNo, Wedscat, LR) :-
  wc(Wedscat, _, _, _, _, _, _, __, _, _, _, _, _, _, _),
  %wc(Wedscat,_,_,_,_,_,_),
  getbacktrack(Here),
  wedstrijd(WNo,Wedscat,PersNo,LR,_Zelf,Andere),     % Links = 0; Rechts = 1
  not(w3(WNo,Wedscat,_,_,_,_,_)),
  not(w2(WNo,Wedscat,_,_,_,_,_,_)),
  not(Andere = bye),
  %wd(WNo, Wedscat, T1, T2, _, _),
  %echt(T1, T2),
  cutbacktrack(Here).
  
lastdir(In, Body, Out) :-
  str_len(In, Len),
  L1 = Len - 1,
  frontstr(L1,In,In1,Tok),
  %lasttoken(In, In1, Tok),
  Tok = "\\", !,
  lastdir(In1, "", Body, Out).
lastdir(In, Body, Out) :-
  lastdir(In, "", Body, Out).

lastdir(In, Body, Body, In) :-
  not(searchchar(In,'\\',_)), !.
lastdir(In, Bin, Bout, Out) :-
  searchchar(In,'\\',FoundPos),
  %FoundPos > 0, !,
  P1 = FoundPos,
  frontstr(P1,In,StartString,Rest),
  fronttoken(Rest, _, _),  !,
  concat(Bin, StartString, Bin1),
  lastdir(Rest, Bin1, Bout, Out).

lr(T,T,_,0) :- !.
lr(T,_,T,1).

spTelPlus(s(SpNo), Tel, Naam) :-
  sp2(SpNo,_,Naam,Tel1,_,_,_,_,_,_,_,_,_,_,_,_,Tel2, Tel3, _,_,_,_,_,_,_,_,_), !,
  format(Tel, "%s %s %s", Tel1, Tel2, Tel3),
  trim_c(Tel).
spTelPlus(pw(SpNo), Tel, Naam) :-
  sp2(SpNo,_,Naam,Tel1,_,_,_,_,_,_,_,_,_,_,_,_,Tel2, Tel3, _,_,_,_,_,_,_,_,_), !,
  format(Tel, "%s %s %s", Tel1, Tel2, Tel3),
  trim_c(Tel).
spTelPlus(p(S1,S2), Tel, Namen) :-
  sp2(S1,_,Naam1,Tel11,_,_,_,_,_,_,_,_,_,_,_,_,Tel21, Tel31, _,_,_,_,_,_,_,_,_),
  sp2(S2,_,Naam2,Tel12,_,_,_,_,_,_,_,_,_,_,_,_,Tel22, Tel32, _,_,_,_,_,_,_,_,_), !,
  format(Tel,"%s %s %s\n%s %s %s",Tel11,Tel21,Tel31,Tel12,Tel22,Tel32),
  trim_c(Tel),
  format(Namen, "%s+%s",Naam1,Naam2).
spTelPlus(_, "", "").

tgnst_num(s(Num),Num) :- !.
tgnst_num(pw(Num),Num) :- !.
tgnst_num(p(Num,_),Num) :- !.
tgnst_num(p(_,Num),Num).

afzeggenSList(Pers, ListIn, ListUit, Ain, Auit) :-
  findall(S, afzeggenS(Pers,S), AList),
  count(AList, Ain, Auit),
  append(ListIn, AList, ListUit).

afzeggenS(Pers, Str) :-
  ws(Num,Cat,T,Tijd),		% er was eens een planning
  tgnst_num(T, Pers),
  not(w2(Num,Cat,_,_,_,_,_,_)),	% nu geen planning
  wc(Cat, _, _, _, _, Type, _, _, _, _, _, _, _, _, _),
  %wc(Cat,_,_,Type,_,_,_), 
  tijd_kwart(D1,U1,M1,Tijd),
  %dag(D1,DS1,_), 
  dt_dateoffset_to_str(D1, "%Dn %DD-%MD-%YL", Repr0),
  helestr_int(0,MS1,M1),
  format(Str, "x\t%-5 %-7 %2-%2",Type,Repr0,U1,MS1).
  %format(Str, "x\t%-5 %-7 %2\46%2",Type,DS1,U1,MS1).

te_plannenList(SpNo, TePlanList) :-
  findall(TePlan, te_plannenS(SpNo, TePlan), TePlanList).
  
te_plannenS(SpNo, Str) :-
  te_plannen(SpNo, _WNo, Cat, _LR),		% geeft er max één per cat
  wc(Cat, _, _, _, _, Type, _, _, _, _, _, _, _, _, _),
  %wc(Cat,_,_,Type,_,_,_),
  format(Str, "Nog te plannen %s", Type).

gekoppeld(BeginT, Num, Cat, onbekend, onbekend, BeginTPlus) :-
  gekoppeld4c(BeginT, Num, Cat, onbekend, onbekend, BeginTPlus), !.
gekoppeld(BeginT, _Num, _Cat, _, _, BeginT).

gekoppeld4c(BeginT, Num, Cat, onbekend, onbekend, BeginTPlus) :-
  kopl(CatIn1, _, Cat, plaats(PosUit1)),
  bitright(PosUit1, 1, P1),
  bitand(PosUit1, 1, A1),  
  P1 = Num,
  A1 = 0,
  kopl(CatIn2, _, Cat, plaats(PosUit2)),
  bitright(PosUit2, 1, P2),
  bitand(PosUit2, 1, A2),  
  P2 = Num,
  A2 = 1,
  maxPlan(CatIn1, MaxTijd1),
  maxPlan(CatIn2, MaxTijd2),
  grootste(MaxTijd1, MaxTijd2, MaxTijd),
  grootste(BeginT, MaxTijd, BeginTPlus), !.
gekoppeld4c(BeginT, Num, Cat, onbekend, _, BeginTPlus) :-
  kopl(CatIn, _, Cat, plaats(PosUit)),
  bitright(PosUit, 1, P1),
  P1 = Num,
  maxPlan(CatIn, MaxTijd),
  grootste(BeginT, MaxTijd, BeginTPlus), !.
gekoppeld4c(BeginT, Num, Cat, _, onbekend, BeginTPlus) :-
  kopl(CatIn, _, Cat, plaats(PosUit)),
  bitright(PosUit, 1, P1),
  P1 = Num, !,
  maxPlan(CatIn, MaxTijd),
  grootste(BeginT, MaxTijd, BeginTPlus).

maxPlan(Cat, TijdM1) :-
  findall(Tijd, w2(_,Cat,Tijd,_,_,_,_,_),TijdL),
  sorttijd_c(TijdL),
  reverse(TijdL, [], TijdLR),
  TijdLR = [TijdM|_],
  separatie(Sep),
  TijdM1 = TijdM + Sep,  !.
maxPlan(_, 0).

%wochar('W').
%wochar('w').
%wochar('P').
%wochar('p').

dialoogpositie(VensterID, Win) :-
  Xt = vpi_GetAttrVal(attr_screen_width),	% 6.8.2008
  XtUiterste = Xt - 300,
  Yt = vpi_GetAttrVal(attr_screen_height),	% 6.8.2008
  YtUiterste = Yt - 300,
  vensterRCT(VensterID, RCT),
  RCT = rct(L0, T0, _, _),	% zoek linksboven
  kleinste(L0, XtUiterste, L00),
  kleinste(T0, YtUiterste, T00),
  OuterRct = win_GetOuterRect(Win),
  Wsflags = win_GetState(Win),
  OldRCT = rect_GetClient(Wsflags,b_False,OuterRct),
  OldRCT = rct(L1, T1, _, _),
  OffsX = L00 - L1,
  OffsY = T00 - T1,
  RCTNew = rect_Offset(OldRCT, OffsX, OffsY),
  win_Move(Win, RCTNew), !.
dialoogpositie(_VensterID, _Win).

vensterpositie(VensterID, Win) :-	% als dialoogpositie maar incl afm
%  not(VensterID = onderdeelselectvenster),
  not(VensterID = oproepcorvenster),
  not(VensterID = browsevenster),
  not(VensterID = rapportvenster),
  vensterRCT(VensterID, RCT),
  win_Move(Win, RCT), !.
vensterpositie(onderdeelselectvenster, Win) :-
  TWin = vpi_GetTaskWin(),
/*  Trct = win_GetOuterRect(TWin),
  WsflagsT = win_GetState(TWin),
  Trct1 = rect_GetClient(WsflagsT,b_True,Trct),*/
  Trct1 = win_GetClientRect(TWin ),
  Trct1 = rct(_,_,B,H),
  OuterRct = win_GetOuterRect(Win),
  Wsflags = win_GetState(Win),
  OldRCT = rect_GetClient(Wsflags,b_False,OuterRct),
  OldRCT = rct(_, _, BO, HO),
  Offx = B - BO - 200,
  Offy = H - HO - 200,
  RCT = rect_Offset(OldRCT, Offx, Offy),
  win_Move(Win, RCT), !.
vensterpositie(rapportvenster, Win) :-
/*  TWin = vpi_GetTaskWin(),
  Trct1 = win_GetClientRect(TWin ),
  Trct1 = rct(_,_,BO,HO),*/
  vensterRCT(rapportvenster, OldRCT),
  random(40,Randomx),
  random(40,Randomy),
  X = Randomx - 20,
  Y = Randomy - 20,
  RCT = rect_Offset(OldRCT, X, Y),
  win_Move(Win, RCT), !.
vensterpositie(oproepcorvenster, Win) :-
  OuterRct = win_GetOuterRect(Win),
  Wsflags = win_GetState(Win),
  OldRCT = rect_GetClient(Wsflags,b_False,OuterRct),
  RCT = rect_Offset(OldRCT, 10, 10),
  win_Move(Win, RCT), !.
vensterpositie(browsevenster, Win) :-
  TWin = vpi_GetTaskWin(),
  OuterRct = win_GetOuterRect(TWin),
  Wsflags = win_GetState(TWin),
  Trct1 = rect_GetClient(Wsflags, b_True, OuterRct),
  %Trct1 = win_GetClientRect(TWin ),
  Trct1 = rct(_,_,B,H),
  B1 = B - 40,
  H1 = H - 50,
  X1 =  10,
  Y1 =  30,
  RCT = rct(X1, Y1, B1, H1),
/*
  OuterRct = win_GetOuterRect(Win),
  Wsflags = win_GetState(Win),
  OldRCT = rect_GetClient(Wsflags,b_False,OuterRct),
  RCT = rect_Offset(OldRCT, -20, -10), */
  win_Move(Win, RCT), !.
vensterpositie(_, _Win).
  
updateVensterpositie(VensterId, Win) :-	% geldt voor beide
  OuterRct = win_GetOuterRect(Win),
  Wsflags = win_GetState(Win),
  not(member(wsf_Maximized, WsFlags)),
  not(member(wsf_Minimized, WsFlags)),
  RCT = rect_GetClient(Wsflags,b_False,OuterRct),
  RCT = rct(L,T,R,B),
  R - L > 60,
  B - T > 60,
  retractall(vensterRCT(VensterId, _)),
  assert(vensterRCT(VensterId, RCT)), !.
updateVensterpositie(_, _).

/*speelsterkteOudNieuw(Oud, Nieuw) :-
  searchchar(Oud,'/',FoundPos),
  F1 = FoundPos - 1,
  frontstr(F1,Oud,Nieuw,_RestString), !.
speelsterkteOudNieuw(Oud, Nieuw) :-
  upper_lower(Oud1, Oud),
  speelsterkteOudNieuw1(Oud1, Nieuw), !.
speelsterkteOudNieuw(Nieuw, Nieuw).
*/
removeDubl([H1,H1|T], In, Uit) :-
  removeDubl([H1|T], In, UIt), !.
removeDubl([H|T], In, [H|Uit]) :-
  removeDubl(T, In, Uit), !.
removeDubl(_, In, In).

posP(Cat, Cat, Card, Pos, RSoort) :-
  pos(Cat, Card, Pos, RSoort), !. 
posP(Cat, CatNr, Card, Pos, RSoort) :-
  vink(Cat, CatNr),
  pos(Cat, Card, Pos, RSoort), !. 
posP(Cat, CatNr, 0, 0, nietingedeeld) :-
  vink(Cat, CatNr),
  not(pos(Cat, _, _, _)), !. 
posP(0, _Cat0, 0, 0, nietingedeeld).

getNth(N, List, Elem) :-
  getNth1(0, N, List, Elem).
  
getNth1(N, N, [E|_], E) :- !.
getNth1(A, N, [_|Tail], E) :-
  A1 = A + 1,
  getNth1(A1, N, Tail, E).

xSpelsoort(Cat, MVS, HDG, WTs, WTd) :-
  %dlg_note("", "hier"),
  wc(Cat, _, _, _, WedsTyp, _, _, _, _, _, HDG, _, _, _, _),
  %wc(Cat,_,WedsTyp,_,SexList,_,_),
  %xml_wedsex(HDG, MVs),
  xSpelsoort1(MVs, HDG),
  wedstypConvert(WedsTyp, WTs, WTd), !.

%  --> xml_wedsex
xSpelsoort1("G", gemengd) :- !.	% ook enkel kan gemengd zijn
xSpelsoort1("V", dames) :- !.   % NB anders dan de KNLTB letters!!
xSpelsoort1("M", heren) :- !.
xSpelsoort1("N", hdnvt) :- !.

wedstypConvert(e,"E",e) :- !.	% alleen nog maar enkel en dubbel
wedstypConvert(_,"D",d).

isOnderCat(Cat) :-
  koplv(Cat, _CatT), !.

bovencat(Cat, CatBoven) :-		% zoek bovenste
  koplv(Cat, CatB), !,
  %Rank < 3, !,	% 1 of 2 gaan door
  bovenCat(CatB, CatBoven).
bovencat(Cat, Cat).

bovencat_nd(Cat, CatBoven) :-		% zoek bovensten
  koplv(Cat, CatB),
  %Rank < 3, !,	% 1 of 2 gaan door
  bovenCat_nd(CatB, CatBoven).
bovencat_nd(Cat, Cat).

voorronde(_Cat, _Voorronde) :-
  retractall(algeweestW(_)),
  fail.
voorronde(Cat, VoorrondeX) :-
  getbacktrack(Here1),
  koplv(_, Cat),
  cutbacktrack(Here1),		% als er één is, dan allemaal in volgorde
  wc(Voorronde, _, _, _, _, _, _, __, _, _, _, _, _, _, _),
  %wc(Voorronde, _Naam, _, _Shortp,_,_,_),
  getbacktrack(Here),
  koplv(Voorronde, Cat),
  cutbacktrack(Here),
  voorrondeA(Voorronde, VoorrondeX),
  not(algeweestW(VoorrondeX)),
  assert(algeweestW(VoorrondeX)).
voorronde(_Cat, _Voorronde) :-
  retractall(algeweestW(_)),
  fail.

voorrondeA(Cat, Cat).
voorrondeA(Cat, VoorrondeX) :-
  getbacktrack(Here1),
  koplv(_, Cat),
  cutbacktrack(Here1),		% als er één is, dan allemaal in volgorde
  wc(Voorronde, _, _, _, _, _, _, __, _, _, _, _, _, _, _),
  getbacktrack(Here),
  koplv(Voorronde, Cat),
  cutbacktrack(Here),
  voorrondeA(Voorronde, VoorrondeX).

voorrondes(Cat,b_True) :-	% determ
  kopl(CatV, _, Cat, _),
  not(kopl(CatV, pl3en4(_), Cat, _)), !.
%  koplv(_, Cat), !.
voorrondes(_Cat,b_False).

koplv(CatV, Cat) :-
  kopl(CatV, _, Cat, _),
  not(kopl(CatV, pl3en4(_), Cat, _)).
koplv(CatV, Cat) :-			% 3 en 4 is helaas andersom
  kopl(Cat, pl3en4(3), CatV, _).	% komen altijd paarwijze voor

stelsel(Cat, Voorrondes) :-
  findall(VCat, voorronde(Cat, VCat), Voorrondes), !.

tgnstNum(p(No,_),No).	% 12.11.2003 itt tgnst_num
tgnstNum(p(_,No),No).
tgnstNum(s(No),No).
tgnstNum(pw(No),No).

metatag("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">",
        "<META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=iso-8859-1\">").

metTekst(String) :-
  frontchar(String, Teken, _),
  Teken >= 'A',
  Teken <= 'Z', !.
metTekst(String) :-
  frontchar(String, Teken, _),
  Teken >= 'a',
  Teken <= 'z', !.
metTekst(String) :-
  frontchar(String, _, Rest), !,
  metTekst(Rest).
clauses

get_Webmap(Map1) :-
  %disk(OSPath),
  getfolder(TMap, _Toernooien, _, _),
  %get_ToernooiDir(TMap, _, _, _),
  format(Map, "%s\\Webmap", TMap),
  trap(mkdir(Map),_,true),
  concat(Map, "\\", Map1), !.
get_Webmap(Map) :-		% als ie niet gemaakt kan worden dan is ie er al
  getfolder(TMap, _Toernooien, _, _),
  %get_ToernooiDir(TMap, _, _, _),
  format(Map, "%s\\Webmap\\", TMap), !. 

get_tempDir(TempDir, TempFile) :-
  envsymbol("TMP", TempDir),
  filenamePath(TempFile, TempDir, tmpconstx), !.
get_tempDir(TempDir, TempFile) :-
  envsymbol("TEMP", TempDir),
  filenamePath(TempFile, TempDir, tmpconstx), !.
get_tempDir(Dir, TempFile) :-
  disk(Dir),
  filenamePath(TempFile, Dir, tmpconstx).

rapport_CSS(CSSContent) :-
  tasversie(Versie),
  format(S1, "/*Toernooi Assistent versie %s stylesheet voor Rapporten */\n", Versie),
  format(S2, "%s@page {size: auto; margin 0cm}\nSPAN.rood {color:#FF0000}\nSPAN.groen {color:#32CD32}\nSPAN.rood {color:red; font-weight : bold;}\nPRE {font-size: 10pt;}\n", S1),
  format(S3, "%sBODY {background-color: white; font-family: Verdana, Arial, Helvetica; font-size: 9pt; color: black; font-weight: normal;}\n",S2),
  format(S4, "%sTABLE {background-color: white; font-family: Verdana, Arial, Helvetica; font-size: 9pt; color: black; font-weight: normal;\n",S3),
  format(S5, "%s border-style: solid; border-color: lightgrey; border-right-width: 0; border-top-width: 1; border-left-width: 1; border-bottom-width: 0;}\n",S4),
  format(S51, "%sTABLE.nieuw {page-break-before:auto;}\n", S5),
  format(S6, "%sTD {border-style: solid; border-color: lightgrey; border-right-width: 1; border-top-width: 0; border-left-width: 0; border-bottom-width: 1;}\n",S51),
  format(S7, "%sTD.middle {text-align:center}\n",S6),
  format(S8, "%sTD.rechts {text-align:right}\n",S7),
  format(S81, "%sTR.uitsl {color: blue}\n",S8),
  format(S82, "%sTR.plaats {color: red}\n",S81),
  format(S9, "%sA:LINK, A:VISITED { text-decoration : none; font-weight : bold; color : #3366BB; }\n",S82),
  format(S10, "%sA:HOVER { font-weight : bold; color : #CC0000; }\n", S9),
%  format(S9, "%sTD.nobottom {border-bottom-wdth: 0}\n",S8),
  format(CSSContent, "%sTH {border-style: solid; border-color: lightgrey; border-right-width: 1; border-top-width: 0; border-left-width: 0; border-bottom-width: 1;}",S10).

namenX(SpList, txt(ID, Naam, 0, b_True, 0, [])) :-
  member(No1, SpList),
  sp2(No1,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  %sp1(No1,_,Naam,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),
  ID = id_menufuzz + No1.

emailSturen(ToList,CCList,BCCList,Titel,Tekst,Bestanden) :-
  disk(Current),
  %sMAPI_SendMailMessage(["toernooi.assistent@hccnet.nl"],[],[], "Klaar..","Over"),
  form_Recips( [], ToList, sMAPI_TO, Lst1 ),
  form_Recips( Lst1, CCList, sMAPI_CC, Lst2 ),
  form_Recips( Lst2, BCCList, sMAPI_BCC, Recips_list),
  sMAPI_ConvRecipDesc_Array( Recips_list, Recips_ptr, Count ),
  NULL_originator = cast( MAPIRECIPDESC_ARRAY, 0 ),
  sMAPI_ConvFileDesc_Array(Bestanden, M_Files, FCount),
  _RC = sMAPI_SendMail( 0, 0, mapimessage(0, Titel, Tekst, "", "", "", 0, NULL_originator,Count,Recips_ptr,FCount,M_files), [ sMAPI_Logon_UI ] ),
%  write("\nRC=",RC),
  disk(Current), !.

loclist(List) :-
  baan_adm(ja),
  findlokaties(List), !.
  %findall(Loc,  w2(_,_Cat,_Tijd,_J1,_J2,_Geforceerd,_,Loc), LocList),
  %sortstring_c(LocList, 1),
  %removeDublenBlank(LocList, [], List), !.
loclist([]).

predicates

%rsoort2bitmap1(rsoort Afval, wcorigine Ond, teamstatus Opg,   cTreeView::bitmapId)
%rsoort2bitmap2(wcorigine Ond, teamstatus Opg,  cTreeView::bitmapId)
%opgKeur(catl, teamstatus)
%switchKeur(wcorigine, wcorigine)
%procedure toernooionderdeel_TT(wedscat Cat, keurdom CatKeur)

clauses

afgekeurdeopgaven(Cat,No) :-
  %wc(Cat, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
  %wc(Cat,_,_,_,_,_, _),
  opg(No,Cat,_Tgnst,_,_,_,_,_,_,msg(_,_,_,_,_,_),_).

nietgekeurdeopgaven(Cat, No) :-
  opg(No,Cat,_Tgnst,_,_,_,_,_,_,na,_).
/*  %wc(Cat, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
  %wc(Cat,_,_,_,_,_, _),
  %bovenCat(Cat, Catb),	% 16.6.2007
  %opg(No,Cat,Tgnst,_,_,_,_,_),
  %team_t_algemeen(_, Catb, Tgnst, nietgekeurd, _).
nietgekeurdeopgaven(Cat, No) :-
  wc(Cat, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
  %wc(Cat,_,_,_,_,_, _),
  bovenCat(Cat, Catb),	% 16.6.2007
  opg(No,Cat,Tgnst,_,_,_,_,_),
  not(team_t_algemeen(_, Catb, Tgnst, _, _)).*/

opgKeur(CatL, msg("F",K1,T1,S2,K2,T2)) :-
  member(Cat, CatL),
  %findall(Sx, opg(_,Cat,_,_,_,_,_,_,_,msg("F",_,_,_,_,_),_),SLix),
  opg(_No,Cat,_Tgnst,_,_,_,_,_,_,msg("F",K1,T1,S2,K2,T2),_), !.
opgKeur(CatL, msg("W",K1,T1,S2,K2,T2)) :-
  member(Cat, CatL),
  opg(_No,Cat,_Tgnst,_,_,_,_,_,_,msg("W",K1,T1,S2,K2,T2),_), !.
opgKeur(CatL, msg("I",K1,T1,S2,K2,T2)) :-
  member(Cat, CatL),
  opg(_No,Cat,_Tgnst,_,_,_,_,_,_,msg("I",K1,T1,S2,K2,T2),_), !.
opgKeur(CatL, msg("W","","","","","")) :-
  member(Cat, CatL),
  lokaalOnderdeelProbleem(Cat), !.
opgKeur(CatL, geaccepteerd) :- 
  member(Cat, CatL),
  opg(_No,Cat,_Tgnst,_,_,_,_,_,_,geaccepteerd,_), !.
  %bovencat(Cat, CatB),	% 16.6.2007
  %nietgekeurdeopgaven(CatB, _), !.
opgKeur(_, na).

switchKeur(reserve(_), vanknltb) :- !.
switchkeur(X, X).

rsoort2bitmap(Cat, resId(idb_plaats_34)) :-
  kopl(_CatNo, pl3en4(_), Cat, _), !.
rsoort2bitmap(Cat, resId(idb_troostronde)) :-
  kopl(Cat,verliezer,_,_), !.
  %wc(_, _, _, _,_,_,Cat), 
  %Cat <> 0, !.	% verliezersronde
rsoort2bitmap(Cat, BitMap) :-
  pos(Cat, _, _, RSoort), % ingedeeld
  bovenCat(Cat, TopCat),
  wc(TopCat, _, _, _, __, _, _, _, _, _, _, _, CatKeur, _, _),
  switchKeur(CatKeur, CatKeur1),
  %toernooionderdeel_tt(TopCat, CatKeur),
  findall(Voorr,voorronde(Cat, Voorr),Voorrondes),
  opgKeur([Cat|Voorrondes], OpgKeur),
  %write("\n",OpgKeur),
  rsoort2bitmap1(RSoort, CatKeur1, OpgKeur, BitMap), !. 
rsoort2bitmap(Cat, BitMap) :-
  bovenCat(Cat, TopCat),  % niet ingedeeld
  wc(TopCat, _, _, _, __, _, _, _, _, _, _, _, CatKeur, _, _),
  %toernooionderdeel_TT(TopCat, CatKeur),
  findall(Voorr,voorronde(Cat, Voorr),Voorrondes),
  opgKeur([Cat|Voorrondes], OpgKeur),
  rsoort2bitmap2(CatKeur, OpgKeur, BitMap), !. 
rsoort2bitmap(_Cat, resId(idb_geenindeling)).

/*toernooionderdeel_TT(TopCat, CatKeur) :-
  toernooionderdeel_t(TopCat, _, _, _, _, _, _, CatKeur, _), !.
toernooionderdeel_TT(_Cat, nietgekeurd).*/

rsoort2bitmap1(afval, vanknltb, nietgeaccepteerd,   resId(idb_afvalmetao)) :- !.    % groen rood
rsoort2bitmap1(afval, vanknltb, msg(_,_,_,_,_,_),   resId(idb_afval)) :- !.    % groen rood
rsoort2bitmap1(afval, vanknltb, na, resId(idb_afvalmetno)) :- !. 	% groen grijs
rsoort2bitmap1(afval, vanknltb, geaccepteerd, resId(idb_afval)) :- !.         % groen groen
rsoort2bitmap1(afval, _          , _          , resId(idb_afvalnietgek)) :- !.  % grijs
rsoort2bitmap1(poel,  vanknltb,   nietgeaccepteerd, resId(idb_pouleafggoed)) :- !.  % groen rood
rsoort2bitmap1(poel,  vanknltb, msg(_,_,_,_,_,_),   resId(idb_poulemetao)) :- !.    % groen rood
%rsoort2bitmap1(poel,  afgekeurd,   afgekeurd,   resId(idb_pouleafgek)) :- !.    % rood rood
rsoort2bitmap1(poel,  vanknltb, na, resId(idb_poulemetno)) :- !. 	% groen grijs
rsoort2bitmap1(poel,  vanknltb, geaccepteerd, resId(idb_poule)) :- !.         % groen groen
rsoort2bitmap1(poel,  _          , _          , resId(idb_poulenietgek)) :- !.  % grijs

rsoort2bitmap2(vanknltb, msg(_,_,_,_,_,_), resId(idb_geenindelingmetao)) :- !.  % dg lbl
rsoort2bitmap2(reserve(_), msg(_,_,_,_,_,_), resId(idb_geenindelingmetao)) :- !.  % dg lbl
rsoort2bitmap2(vanknltb, na, resId(idb_geenindelinggoedgek)) :- !.  % dg dg
rsoort2bitmap2(reserve(_), na, resId(idb_geenindelinggoedgek)) :- !.  % dg dg
rsoort2bitmap2(vanknltb, nietgeaccepteerd,   resId(idb_geenindelingmetao))   :- !.  % groen rood
rsoort2bitmap2(reserve(_), nietgeaccepteerd,   resId(idb_geenindelingmetao))   :- !.  % groen rood
%rsoort2bitmap2(afgekeurd,   afgekeurd,   resId(idb_geenindelingafgek))   :- !.  % rood rood
%rsoort2bitmap2(vankbnltb, geaccepteerd, vanknltb, resId(idb_geenindelingmetno))   :- !. 	% groen grijs
rsoort2bitmap2(vanknltb, geaccepteerd, resId(idb_geenindelinggoedgek)) :- !.  % groen groen
rsoort2bitmap2( _         , _, resId(idb_geenindeling))        :- !.  % grijs


predicates

%urlEncode(string In, string In, string Uit) - (i, i, o)
encodeU(char, string)
encodeC(integer, char)

clauses

urlencode(In, In1, Uit) :-
  frontchar(In,Char,Rest),
  encodeU(Char, Code),
  concat(In1, Code, In2), !,
  urlencode(Rest, In2, Uit).
urlencode(_, Uit, Uit).

encodeU(' ', "+") :- !.
encodeU(Char, US) :-  Char >= 'A', Char <= 'Z', str_char(US,Char), !.
encodeU(Char, US) :-  Char >= 'a', Char <= 'z', str_char(US,Char), !.
encodeU(Char, US) :-  Char >= '0', Char <= '9', str_char(US,Char), !.
encodeU('-', "-") :- !.
encodeU('_', "_") :- !.
encodeU('.', ".") :- !.
encodeU('*', "*") :- !.
encodeU(Char, US) :-
  char_int(Char, I),
  bitand(I, 0x000F, I1),
  encodeC(I1, C1),
  bitright(I, 4, I2),
  bitand(I2, 0x000F, I3),
  encodeC(I3, C2),
  format(US, "%%%c%c", C2, C1).
  
encodeC(0,'0') :- !. 
encodeC(1,'1') :- !. 
encodeC(2,'2') :- !. 
encodeC(3,'3') :- !. 
encodeC(4,'4') :- !. 
encodeC(5,'5') :- !. 
encodeC(6,'6') :- !. 
encodeC(7,'7') :- !. 
encodeC(8,'8') :- !. 
encodeC(9,'9') :- !. 
encodeC(10,'A') :- !. 
encodeC(11,'B') :- !. 
encodeC(12,'C') :- !. 
encodeC(13,'D') :- !. 
encodeC(14,'E') :- !. 
encodeC(_,'F'). 

%_____________ drukte & urgentie 	nieuw 26.7.2003

database - urgentie
 single zekerAfval(integer)
 single zekerPoule(integer)
 single bijWinstA(integer)
 single plaatsingV(integer)
 single onderdelen(string)
 single onderdelenLengte(integer)
 single ranking(integer)
 single minslots(integer)
 single drukte(integer)
 single druksteOnderdeel(wedscat)
% spelerdrukte(integer SpNo, string Onderdelen, integer Afval, integer Poule, integer BijWinst, integer Plaatsing, integer Rank, integer slots, wedscat DruksteOnderdeel)
 beschikbaarOp(integer Tijdstip)
 wedurg(integer WdNo, wedscat Cat, integer MaxDrukte)
 %pouleVoorRng(integer WdNo, wedscat)
 
predicates
determ gevorderdInSchema(wedscat Cat, tgnst Tgnst, integer NogTeSpelen, integer BijWinst)
determ persoonMoetNogSpelen(wedscat Cat, tgnst Tgnst, integer Plaatsing, integer ZekerAfval,
               integer Zekerpoule, integer Bijwinst, integer Plaatsingsbonus, rsoort AfvalPoule)
nondeterm nogInPoule(integer WdNo, tgnst Tgnst, wedscat Cat)
nondeterm nogInPoule(integer WdNo, wedscat Cat)
nondeterm nogInPouleNietGeplaatst(integer WdNo, wedscat Cat)
procedure vervolg(wedscat Cat, integer Rondes)
nondeterm druktestring(string)
determ verloren(integer WdNo, wedscat Cat, tgnst Tgnst)
%procedure timeGrid(integerlist)
%procedure slots(verhl, integer UitKantoor, integer Slots)
procedure plaatsingsbonus(integer, integer, integer)
% procedure plaatsingsbonusPoule(integer, integer)
%procedure parseTabs(string, slist)	% string met tabs naar stringlist
nondeterm bnVanaf(integer Vanaf, integer Tijd)
%nondeterm wedsurg(catl, integer, wedscat, integer Urg)
nondeterm wedSpelersDrukte(integerlist DeSpelers, integer Drukte, wedscat Catzelf)
procedure eigenCatBonus(wedscat, wedscat, integer)
nondeterm wedsurgStr(string Str)
nondeterm chop(slist, integer N, integer M, boolean TwaalfProcent, string Str)
determ    nognietgepland(boolean AlleenNogNietgepland, integer WdNo, wedscat Cat)
%procedure accumulatie(rsoort AfValPoule, integer Dr, integer Drukte, integer AccuDrukte)
procedure pouleVoorrang()
procedure echtewedstrijd(wedscat Cat, integer WedNo, integer Card, integer Card2)

clauses

zekerAfval(0).
zekerPoule(0).
bijWinstA(0).
plaatsingV(0).
onderdelen("").
onderdelenLengte(0). % om kolombreedte te bepalen
ranking(1).
%maxslots(0).
minslots(100).
drukte(0).
druksteOnderdeel(0).

eigenCatBonus(Cat, Cat, 1) :- !.
eigenCatBonus(_, _, 0).

timeGrid([H|T]) :-
  assert(beschikbaarOp(H)), !,
  timeGrid(T).
timeGrid(_).

slots([per(Van, Tot)|_], _, _Slots) :-	% delete eerst alle tijdstippen van verhindering
  duur(Duur),
  beschikbaarOp(Tijd),
  Tijd > Van - Duur,
  Tijd < Tot,
  retractall(beschikbaarOp(Tijd)),
  fail.
slots(_, UitKant, _Slots) :-		% delete alle tijdstippen voor uitkantoor
  beschikbaarOp(X),
  bitright(X, 7, Dag),
  dag(Dag, _, w),			% als het een werdag is
  bitand(X, $7F, X1),
  X1 < UitKant,
  retractall(beschikbaarOp(X)),
  fail.
slots([_|Rest], Uitkant, Slots) :- !,
  slots(Rest, Uitkant, Slots).
slots(_, _, _Slots) :-
  duur(Duur),
  beschikbaarOp(Tijd),
  beschikbaarOp(X),
  X > Tijd,
  X < Tijd + Duur,
  retractall(beschikbaarOp(X)),
  fail.
slots(_, _, Slots) :-
  findall(X, beschikbaarOp(X), List),
  count(List, 0, Slots),
  minslots(Mi),
  kleinste(Mi, Slots, Mi1),
  assert(minslots(Mi1)),
  retractall(beschikbaarOp(_)).

drukteString(String) :-
  spelerdrukte(SpNo, Onderdelen, Afval, Poule, BijWinst, Plaatsingsbonus, _, SlotsVerhinderd, _),
  Drukte = (Afval + Poule) * 2 + BijWinst + Plaatsingsbonus + SlotsVerhinderd,
  format(String, "%5d\t%2d\t%u", Drukte, Poule, SpNo),
  str_len(Onderdelen, Len1),
  onderdelenLengte(Len),
  grootste(Len, Len1, LenX),
  assert(onderdelenlengte(LenX)).

parsetabs(In, [H|Out]) :-
  searchchar(In, '\t', Nr1),
  Nr1a = Nr1 - 1,
  frontstr(Nr1a,In,H,_),
  frontstr(Nr1,In, _, Rest), !,
  parseTabs(Rest, Out).
parseTabs(In, [In]) :-
  fronttoken(In, _, _), !.
parseTabs(_, []).

bnVanaf(Vanaf, Tijd) :-
  bn(Tijd, _),
  Tijd >= Vanaf.

drukteRapport(_) :-		% altijd
  assert(minslots(100)),
  fail.
drukteRapport(1) :-		% 1 = igv rapport
  assert(onderdelenlengte(0)),
  closefile(work),
  get_TempDir(_, TempFile),
  openwrite(work,TempFile),
  writedevice(work),
  fail.
drukteRapport(_) :-
  time_stamp_next(Huidig, _Dag, _HoeLaat),
  findall(T, bnVanaf(Huidig, T), Tijden),
  sortint_c(Tijden),
  timeGrid(Tijden),
  slots([], 0, MaxSlots),
  sp2(SpNo,_,_,_,Verh,_,_,_,UitKant,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
    persoonMoetNogTotaal(SpNo, Onderdelen, Afval, Poule, Bijwinst, Plaatsing, DruksteOnderdeel),
    Afval + Poule + BijWinst > 0,
    timeGrid(Tijden),
    slots(Verh, Uitkant, Slots),
    SlotsVerhinderd = MaxSlots - Slots,
    assert(spelerdrukte(SpNo, Onderdelen, Afval, Poule, BijWinst, Plaatsing, 0, SlotsVerhinderd, DruksteOnderdeel)),
  fail.
drukteRapport(1) :-
  findall(Str, druktestring(Str), List0),
  onderdelenlengte(MaxLen),
  Fudgefaktor = MaxLen - 10,		% voor presentatie
  grootste(0, Fudgefaktor, FudgeNetto),
  str_len(Fudge, FudgeNetto),
  grootste(MaxLen, 9, MaxLen1),
  time_stamp_next(_Huidig, Dag, HoeLaat),
  dag(Dag, DagTekst, _),
  tijd_str(Hoelaat,HoelStr,_),
  write("De spelerbezetting (\"Waarde\") wordt als volgt uitgerekend:\n    Een zeker te spelen wedstrijd telt voor twee punten.\n"),
  write("    Een bij winst-wedstrijd telt voor 1 punt.\n"),
  write("    Geplaatste spelers (1-4) krijgen bonuspunten voor de onderdelen waar ze nog\n    in zitten en met bij-winstpartijen.\n"),
  write("    Verhinderingen (gerekend vanaf: ",DagTekst, " ", HoelStr,") geven ook punten.\n"),
  writef("Hoogste \"Waarde\"-s staan bovenaan.              %sPlaats Afval Poule Bijwinst Verh\n", Fudge),
  writef("Rank  Waarde  Naam                    Nog in:   %sbonus  wedst wedst  wedst   inder\n", Fudge),
  assert(ranking(1)),
  format(FormaatStr, "%%4u. %%4u %%-25.25s  %%-%us %%4d  %%4d  %%4d  %%4d    %%4d", MaxLen1),
  sortstring_c(List0, 1),    % alleen om te sorteren
  reverse(List0, [], List),
  member(StrD, List),
    parseTabs(StrD, StrDList),
    StrDList = [DrS, _, SpNoStr|_],
    str_int(DrS, Dr),
    str_int(SpNoStr, SpNo),
    sp2(SpNo,_, Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
    %sp1(SpNo,_,Naam,_,_Verh,_,_Bet,_UitKant,_Club,_,_,_,_,_,_,_,_,_,_),
    retract(spelerdrukte(SpNo, Onderdelen, Afval, Poule, BijWinst, Plaatsing, 0, SlotsVerhinderd, DruksteCat)),
    trim_c(Onderdelen),
    ranking(Rank),
    R1 = Rank + 1,
    assert(ranking(R1)),
    assertz(spelerdrukte(SpNo, Onderdelen, Afval, Poule, BijWinst, Plaatsing, Rank, SlotsVerhinderd, DruksteCat)),
    format(Record, FormaatStr, Rank, Dr, Naam, Onderdelen, Plaatsing, Afval, Poule, BijWinst, SlotsVerhinderd),
    write(Record, "\n"),
  fail.
drukteRapport(0) :-		% drukte ipv rank
  findall(Str, druktestring(Str), List0),
  assert(ranking(1)),
  sortstring_c(List0, 1),    % alleen om te sorteren
  reverse(List0, [], List),
  member(StrD, List),
    parseTabs(StrD, StrDList),
    StrDList = [_DrS, _, SpNoStr|_],
    %str_int(DrS, Dr),
    str_int(SpNoStr, SpNo),
    retract(spelerdrukte(SpNo, Onderdelen, Afval, Poule, BijWinst, Plaatsingsbonus, 0, SlotsVerhinderd, DruksteCat)),
    ranking(Rank),
    R1 = Rank + 1,
    assert(ranking(R1)),
%    Drukte = (Afval + Poule) * 2 + BijWinst + Plaatsingsbonus + SlotsVerhinderd,	% Drukte > 0 !
    assertz(spelerdrukte(SpNo, Onderdelen, Afval, Poule, BijWinst, Plaatsingsbonus, Rank, SlotsVerhinderd, DruksteCat)),
  fail.
drukteRapport(1) :-
  closefile(work),
  retractall(spelerdrukte(_,_,_,_,_,_,_,_,_)),
  Parent = vpi_GetTaskWin(),
  dlg_rapport_Create(Parent, "Spelerbezetting","Drukte",b_False),
  fail.
drukteRapport(_):-
  assert(onderdelen("")).

persoonMoetNogTotaal(SpNo, _Onderdelen, _ZekerAfvalT, _ZekerPouleT, _BijwinstT, _Plaatsingen, _DruksteOnderdeel) :-
  assert(zekerAfval(0)),
  assert(zekerPoule(0)),
  assert(bijWinstA(0)),
  assert(plaatsingV(0)),
  assert(onderdelen("")),
  assert(drukte(0)),
  assert(druksteOnderdeel(0)),
  sp2opg(SPNo,Cat,_Opg,_MaatNo,Plts,Tgnst),
    persoonMoetNogSpelen(Cat, Tgnst, Plts, ZekerAfval, ZekerPoule, Bijwinst, PlaatsingsBonus, _AfvalPoule),
    wc(Cat, _, _, _, _, StrC, _, _, _, _, _, _, _, _, _),
    %wc(Cat,_,_,StrC,_,_,_),		% als poule -> plaatsingsBonus = < 1!
    zekerAfval(ZA),
    ZA1 = ZA + ZekerAfval,
    assert(zekerAfval(ZA1)),
    zekerPoule(ZP),
    ZP1 = ZP + ZekerPoule,
    assert(zekerPoule(ZP1)),
    bijWinstA(BW),
    BW1 = BW + Bijwinst,
    assertA(bijWinstA(BW1)),
    onderdelen(Ostr),
    format(OStr1, "%s %s",OStr, StrC),
    assert(onderdelen(OStr1)),
    plaatsingV(Pl),
%    plaatsingsbonus(Plts, PlF),
    Pl1 = Pl + PlaatsingsBonus,
    assert(plaatsingV(Pl1)),
    drukte(Dr),
    Drukte = (ZekerAfval + ZekerPoule) * 2 + BijWinst + Plaatsingsbonus,
    %accumulatie(AfValPoule, Dr, Drukte, AccuDrukte),
    Drukte > Dr,
    assert(drukte(Drukte)),
    assert(druksteOnderdeel(Cat)),
  fail.
persoonMoetNogTotaal(_SpNo, Onderdelen, ZekerAfvalT, ZekerPouleT, BijwinstT, Plaatsingen, DruksteOnderdeel) :-
  zekerAfval(ZekerAfvalT),
  zekerPoule(ZekerPouleT),
  bijWinstA(BijwinstT),
  onderdelen(Onderdelen),
  plaatsingV(Plaatsingen),
  druksteOnderdeel(DruksteOnderdeel).

persoonMoetNogSpelen(Cat, Tgnst, Plts, ZekerAfval, 0, Bijwinst, Plaatsingsbonus, afval) :-
  pos(Cat, _, _, afval),
  gevorderdInSchema(Cat, Tgnst, ZekerAfval, BijWinst1),
  vervolg(Cat, Rondes),
  BijWinst = BijWinst1 + Rondes,
  plaatsingsbonus(Plts, Bijwinst, Plaatsingsbonus),
  !.
persoonMoetNogSpelen(Cat, Tgnst, Plts, 0, ZekerPoule, Bijwinst, Plaatsingsbonus, poel) :-
  pos(Cat, _, _, poel),
  findall(WdNo, nogInPoule(WdNo, Tgnst, Cat), WL),
  count(WL, 0, ZekerPoule),
  ZekerPoule > 0,
  vervolg(Cat, Rondes),
  BijWinst = Rondes,
  plaatsingsbonus(Plts, BijWinst, Plaatsingsbonus),
  !.

plaatsingsbonus(0, _, 0) :- !.
plaatsingsbonus(1, Bijwinst, 2) :-
  Bijwinst > 2, !.
plaatsingsbonus(1, Bijwinst, 1) :-
  Bijwinst > 0, !.
plaatsingsbonus(2, Bijwinst, 2) :-
  Bijwinst > 2, !.
plaatsingsbonus(2, Bijwinst, 1) :-
  Bijwinst > 0, !.
plaatsingsbonus(3, Bijwinst, 1) :-
  Bijwinst > 2, !.
plaatsingsbonus(4, Bijwinst, 1) :-
  Bijwinst > 2, !.
plaatsingsbonus(_, _, 0).

vervolg(Cat, R1) :-
  kopl(Cat, _, CatEind, _IPV),
  pos(Cateind, Card, _, afval),
%  term_str(ipvdom, _IPV, Msg),
%  _Answer = dlg_MessageBox( "Debug", Msg, mesbox_iconinformation, mesbox_buttonsokcancel, mesbox_defaultfirst, mesbox_suspendapplication ),
  _IPV = plaats(WedNo),
  bitright(WedNo, 1, WedNo1),
  echtewedstrijd(CatEind, WedNo1, Card, CardNetto), 
  dim(CardNetto, Rondes), 
  R1 = Rondes + 1, !.
vervolg(Cat, R1) :-
  kopl(Cat, _, CatEind, _IPV),	% 14.8.2010 algemene link
  pos(Cateind, Card, _, afval),
  dim(Card, Rondes), 
  R1 = Rondes + 1, !.
vervolg(Cat, Rondes) :-
  kopl(Cat, _, CatEind, plaats(X)),
  pos(Cateind, _, _, poel), !,
  findall(WdNo, nogInPoule(WdNo, openplaats(X), CatEind), WL),
  count(WL, 0, Rondes).
vervolg(_Cat, 0).

echtewedstrijd(Cat, WedNo, Card, Card2) :-
  wd(WedNo, Cat, bye, _, _, _, _),
  Card1 = Card - 1,
  bitright(WedNo, 1, WedNo1),
  echtewedstrijd(Cat, WedNo1, Card1, Card2), !.
echtewedstrijd(Cat, WedNo, Card, Card2) :-
  wd(WedNo, Cat, _, bye, _, _, _),
  Card1 = Card - 1, 
  bitright(WedNo, 1, WedNo1),
  echtewedstrijd(Cat, WedNo1, Card1, Card2), !.
echtewedstrijd(_, _, Card, Card).

gevorderdInSchema(Cat, Tgnst, 1, BijWinst) :-	% failt als niet meer in schema
  findall(Num1, wd(Num1,Cat,Tgnst,_,_,_,_), Num1L), 
  findall(Num2, wd(Num2,Cat,_,Tgnst,_,_,_), Num2L), 
  append(Num1L, Num2L, Nums),
  sortint_c(Nums),
  Nums = [WdNo|_],
  not(verloren(WdNo, Cat, Tgnst)),
  dim(WdNo, NogTeSpelen),
  %NogTespelen > 0,
  BijWinst = NogTeSpelen. % - 1.

verloren(WdNo, Cat, Tgnst) :-
  wd(WdNo,Cat,T2,Tgnst,_,_,_),
  w3(WdNo,Cat,T2,_, _, _, _), !.
verloren(WdNo, Cat, Tgnst) :-
  wd(WdNo,Cat,Tgnst,T2,_,_,_),
  w3(WdNo,Cat,T2,_, _, _, _), !.

nogInPoule(WdNo, Tgnst, Cat) :-
  wd(WdNo,Cat,Tgnst,_,_,_,_),
  not(w3(WdNo,Cat,_,_, _, _, _)).
nogInPoule(WdNo, Tgnst, Cat) :-
  wd(WdNo,Cat,_,Tgnst,_,_,_),
  not(w3(WdNo,Cat,_,_, _, _, _)).

nogInPoule(WdNo, Cat) :-
  wd(WdNo,Cat,_,_,_,_,_),
  not(w3(WdNo,Cat,_,_, _, _, _)).

nogInPouleNietGeplaatst(WdNo, Cat) :-
  wd(WdNo,Cat,Tgnst1,Tgnst2,_,_,_),
  not(w3(WdNo,Cat,_,_, _, _, _)),
  opg(_,Cat,Tgnst1,0,_,_,_,_,_,_,_),
  opg(_,Cat,Tgnst2,0,_,_,_,_,_,_,_).

wedSpelersDrukte(DeSpelers, Drukte, CatZelf) :-
  member(Speler, DeSpelers),
    spelerdrukte(Speler, _Onderdelen, Afval, Poule, BijWinst, Plaatsing, _Drukte0, SlotsVerhinderd, DruksteCat),
    Drukte0 = (Afval + Poule) * 2 + BijWinst + Plaatsing + SlotsVerhinderd,	% Drukte > 0 !
    eigenCatBonus(CatZelf, DruksteCat, Bonus),
    Drukte = Drukte0 + Bonus. 

%allebeionbekend(onbekekend, onbekend).

sortwed_c3(_AlleenNogNietgepland, _CatList, _TwaalfProcent, _No, _Cat, _Urg, _Aantal) :-
  drukterapport(0),	% hier wordt de spelerdrukte gezet
  fail.
sortwed_c3(AlleenNogNietgepland, CatList, _TwaalfProcent, _No, _Cat, _Urg, _Aantal) :-
  retractall(wedurg(_, _, _)),
  member(Cat, CatList),
    wd(WdNo,Cat,T1,T2,_,_,_),
      eenbekend(T1, T2),
      not(T1 = bye),
      not(T2 = bye),
      %not(T1 = onbekend),	% 4.11.2012 !!
      %not(T2 = onbekend),
      not(w3(WdNo,Cat,_,_, _, _, _)),
      nognietgepland(AlleenNogNietGepland, WdNo, Cat),
      de_spelers(WdNo, Cat, T1, T2, DeSpelers),
      findall(Drukte, wedSpelersDrukte(DeSpelers, Drukte,Cat), DList),
      sortint_c(DList),
      reverse(DList, [], DListR),
      DListR = [MaxDrukte|_],
      assert(wedurg(WdNo, Cat, MaxDrukte)),
  fail.
sortwed_c3(_AlleenNogNietgepland, _CatList, TwaalfProcent, No, Cat, Urg, Aantal) :-
  %retractall(spelerdrukte(_,_,_,_,_,_,_,_,_)),
  pouleVoorrang(), 
  findall(US, wedsurgStr(US), USList),
  sortstring_c(USList, 1),
  count(USList, 0, Aantal),
  %bitright(Aantal, 4, GewensteAantal),
  Gedeelte = Aantal div 8,
  %write("\n", Aantal, "  ", GewensteAantal),
  chop(USList, 1, Gedeelte, TwaalfProcent, Str),
    parseTabs(Str, SList),
    SList = [US, CS, WS | _],
    str_int(WS, No1),
    No = 9999 - No1,
    str_int(CS, Cat),
    str_int(US, Urg1),
    Urg = 9999 - Urg1.

pouleVoorrang() :-
  pos(Cat, _, _, poel),
  getbacktrack(Here),
  opg(_,Cat,_,Plts,_,_,_,_,_,_,_),
  Plts > 0,			% er is een geplaatste
  cutbacktrack(Here),
  %wc(Cat,Lang,_,_,_,_,_),
  %write("\n", Lang),
  findall(WdNoX, nogInPoule(WdNoX, Cat), WDXList),
  count(WDXList, 0, Count),
  Count > 2,		% alleen poules van 3 of 4
  Count < 7,
  nogInPouleNietGeplaatst(No, Cat),
  %nogNietGepland(b_True, No, Cat),
  getbacktrack(Here1),
  retract(wedurg(No, Cat, _Oud)),
  cutbacktrack(Here1),
  assert(wedurg(No, Cat, 1000)),	% zet vooraan..
  %write(" ", _Oud, " ", 1000), 
  fail.
pouleVoorrang().

chop([Str | _USList], N, M, b_True, Str) :-
  N <= M.
chop([Str | _USList], N, M, b_True, Str) :-
  N > M, !, fail.
chop([Str | _USList], _N, _M, b_False, Str).
chop([_ | USList], N, M, TwaalfProcent, Str) :-
  N1 = N + 1,
  chop(USList, N1, M, TwaalfProcent, Str).

nognietgepland(b_True, _WdNo, _Cat) :- !.	% voor comapat
nognietgepland(_, WdNo, Cat) :-
  not(w2(WdNo,Cat,_,_,_,_,_,_)).      % tijd

wedsurgStr(Str) :-
  retract(wedurg(WdNo, Cat, Urg)),
  Urg1  = 9999 - Urg, 
  WdNo1 = 9999 - WdNo,
  format(Str, "%4u\t%3u\t%4u", Urg1, Cat, WdNo1).	% right justified


database - bookmarks
  bookmark(wps, string, integer)
  single teller(integer)
  single kort(string)
single crit(integer Minleeftijd, integer Maxleeftijd, integer MaxSpeelsterkte, boolean Enkelspel, boolean Dubbelspel, boolean GemengdDubbel, boolean ActueleRating)
 spelerVindex(integer)
 jsonattribuut(string)
 jsonobject(string)

predicates
nondeterm  gespeeldop(integer Dag, integer No, wedscat, tijd)
nondeterm  opdebaanOp(integer Dag, integer No, wedscat Cat, integer B, tijd)
nondeterm  geplandOp(integer Dag, wps, integer Baan)
nondeterm  geplandOpX(integer Dag, wps)
%nondeterm  debanen(string Baan)
nondeterm  debanen1(integer, integer, integer)
%procedure  baanNaamL(integer Len)
%nondeterm  baanNaamL1(integer)
nondeterm  spRankInLijst(integerlist, integer Rank)
procedure  janee2tekst(janee, string)
/*  prt_wedst(integer, wedscat)
  prt_wedst1(integer, wedscat)
  check_wedstrijd(integer, wedscat) */
procedure dagstatusfile(string) 
procedure baannaamGepland(string, string, string)
determ    rep_wed2a(string Voorvoegsel, integer, wedscat, string Html) - (i, i, i, o) 
procedure aanwezigSp(integer, string, string, string)
isBookmark (wps, string)
procedure markeer(wpl, integer)
procedure kortIsVeranderd(string, string)
procedure rondetekst(integer Dim, rsoort Soort, string)
procedure voegStatusNtoe(slist, slist)
procedure usb2usb(slist, slist)
procedure map(string)
%procedure string visBEHEERid(string) - (i)
procedure sleuteltekstP(string)
procedure eenmalig()
%procedure alleDagen(string)
nondeterm alleDag(string)
procedure findcategoriecrits(string)
procedure minL(integer, integer, integer)
procedure maxL(integer, integer, integer)
procedure maxSp(integer, string, integer)
procedure ookEnk(boolean, wedstyp, boolean)
procedure ookDubb(boolean, wedstyp, boolean)
procedure ookGD(boolean, wedsex, boolean)
procedure ookAR(boolean, string Schemasoort, boolean)
nondeterm categorie(wedstyp, string GMV, string NaamKort)
procedure slist alsAlleenlezen()
procedure metBG(janee, string BGNaam, string BGEmail, string BGMaamM, string BGEmeilM)
procedure getTid(slist, string)
procedure writeToJournaal1(string Msg, janee)
nondeterm toertype(string) 
procedure wdViewJson(integer, wedscat, string BaanNaam) % produceert de hele wedstrijdJson
procedure wdJson(integer, wedscat)
procedure w2Json(integer,wedscat)
procedure w3Json(integer,wedscat)
procedure appendToOutputW(string)
procedure appendToOutputS(string)
procedure welkeSpelers(tgnst,string Suffix)
procedure afrondenOutput
procedure toAttrS(string, string)
procedure toAttrU(string, integer)
%procedure escapeJSON(string In, string Resultaat, string Uit)
%procedure escapeJSON1(char In, string Res, string Res1)
%procedure dagrapport1(string)
nondeterm dagmetwedstrijd(string)
determ dagmetwedstrijd1(integer)
determ eenOfTwee(integer WdNo, wedscat, tgnst Winnaar, string) % tgnst1 of tgnst2 is winnaar
procedure bOfN(string Bondsnr, integer SpNr, string Uit)
%procedure newtkal(string Prov, string BeheerId, string Session)

clauses
debanen(Baan) :-
  getMaxBanen(Max),
  debanen1(1, Max, Baan).
  
debanen1(Max, Max, Max) :- !.
debanen1(A, _Max, A).
debanen1(A, Max, Uit) :-
  A1 = A + 1,
  debanen1(A1, Max, Uit).

gespeeldOp(Dag, No, Cat, TijdLog) :-
%  wc(Cat,_,_,_,_,_,_),
  w3(No, Cat, _S1, _P1, _P2, _Stat, TijdLog),
  tijd_kwart(Dag1,_Uur,_Min,TijdLog),
  Dag = Dag1.

opdebaanOp(Dag, No, Cat, B, Tijd) :-
  debanen(B),
  baantoew(No, Cat, baan(B, _)),
  w2(No,Cat,Tijd,_,_,_,_,_),      % tijd
  tijd_kwart(Dag1,_Uur,_Min,Tijd),
  Dag = Dag1.

isBookmark(WP, BM) :-
  retract(bookmark(WP, Tekst, Mark)),
  format(BM, "<a id='%s' name='%s'><span class='bmvisible'>[%u]</span></a>", Tekst, Tekst, Mark), !.
%isBookmark(_,"&nbsp;").
isBookmark(_,"").

teller(0).
kort("").

markeer(WPL, Aantal) :-
  retractall(bookmark(_, _, _)),
  assert(teller(0)),
  member(WPm, WPL),
  teller(X),
  X1 = X + 1,
  assert(teller(X1)),
  YY = X1 mod Aantal,
  YY = 0,
  Nr = X1 div Aantal,
  format(BM, "bladwijzer%u", Nr),
  assert(bookmark(WPm, BM, Nr)),
  fail.
markeer(WPL, _Aantal) :-
  reverse(WPL, [], WPLr),
  WPLr = [WPm | _], !,
  retractall(bookmark(WPm, _, _)),
  assert(bookmark(WPm, "laatste", 100)).
markeer(_WPL, _Aantal).

geplandOpX(Dag, wp(No, Cat, TijdLog)) :-
  wc(Cat, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
  %wc(Cat,_Lang,_,_Kort,_,_,_),
  w3(No, Cat, _S1, _P1, _P2, _Stat, TijdLog),
  tijd_kwart(Dag1,_Uur,_Min,TijdLog),
  Dag = Dag1.
geplandOpX(Dag, wp(No, Cat, Tijd)) :-
  debanen(B),
  baantoew(No, Cat, baan(B, _)),
  w2(No,Cat,Tijd,_,_,_,_,_),      % tijd
  tijd_kwart(Dag1,_Uur,_Min,Tijd),
  Dag = Dag1.
geplandOpX(Dag, wp(No, Cat, Tijd)) :-
  findall(T, bn(T, _), TList),
  sortint_c(TList),
  member(Tijd, TList),
  tijd_kwart(Dag1,_Uur,_Min,Tijd),
  Dag = Dag1,
  geplandOp(Dag, wp(No, Cat, Tijd), _).

geplandOp(Dag, wp(No, Cat, Tijd), B) :-		% en nog niet op de baan
  debanen(B),
  baantoew(No, Cat, reserveren(B, _)),
  w2(No,Cat,Tijd,_,_,_,_,_),      % tijd
  tijd_kwart(Dag1,_Uur,_Min,Tijd),
  Dag = Dag1.
geplandOp(Dag, wp(No, Cat, Tijd), 99) :-		% en nog niet op de baan
  wc(Cat, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
  w2(No,Cat,Tijd,_,_,_,_,_),      % tijd
  not(baantoew(No, Cat, baan(_, _))),
  not(baantoew(No, Cat, reserveren(_, _))),	% 6.7.2007 ook niet gereserveerd
  tijd_kwart(Dag1,_Uur,_Min,Tijd),
  Dag = Dag1.

dagstatusFile(File) :-
  get_Webmap(Dir),
  filenamepath(File, Dir, "dagstatus.html").

aanwezigSp(Sp, Naam, NaamUit, "ja") :- 
  spelerAanwezig(Sp, _Tijd, _, _),
  format(NaamUit, "<span style='background-color:yellow'>%s</span>", Naam), !.
aanwezigSp(_Sp, NaamUit, NaamUit, "nee"). 

rep_wed2a(Voorvoeg, WdNo,Cat, Html) :-
  wd(WdNo,Cat,T1,T2,_Uitsl,_,_),
  repa(ja,'}',nee, nee,nee,0,2,1,WdNo,Cat,T1,alleplanning,_TS1,_,_,b_False, b_False,_,_, Naam1,Naam2, Nr1, Nr2,_),
  repa(ja,'}',nee, nee,nee,1,2,1,WdNo,Cat,T2,alleplanning,_TS2,_,_,b_False, b_False,_,_,Naam3,Naam4, Nr3, Nr4,_), !,
  aanwezigSp(Nr1, Naam1, TS1, _),
  aanwezigSp(Nr2, Naam2, TS2, _),
  aanwezigSp(Nr3, Naam3, TS3, _),
  aanwezigSp(Nr4, Naam4, TS4, _),
  pos(Cat, _Card, _, Soort), !,
  dim(WdNo, Dim),
  rondetekst(Dim, Soort, Ronde),
  format(Html, "<TR>%s<TD>%s<BR>%s</TD><TD>%s<BR>%s</TD><TD>%s</TD></TR>\n", Voorvoeg, TS1, TS2, TS3, TS4, Ronde).

rondetekst(_, poel, "poule") :- !.
rondetekst(0, _, "finale") :- !.
rondetekst(1, _, "1/2 f") :- !.
rondetekst(2, _, "1/4 f") :- !.
rondetekst(_, _, "&nbsp;").

kortIsVeranderd(KortIn, "&nbsp;") :-
  kort(KortIn), !. 
kortIsVeranderd(KortIn, KortIn) :-
  assert(kort(KortIn)).

wdJson(WdNo,Cat) :-
  %comline(LineBuffer),
  %upper_lower(LineBuffer, LBL),
  %searchstring(LBL, "/x", _),
  wd(WdNo,Cat,T1,T2,Uitsl,Spoor,UserNote), 
  wc(Cat, _, _, _NaamLang, _, NaamKort, _, _, _, _, _, _, _, _, _),
  toAttrS("catnaamkort",NaamKort),
  welkespelers(T1,"1"),
  welkespelers(T2,"2"), 
  toAttrU("wnr",WdNo),
  toAttrU("catnr",Cat),
  toAttrS("uitsl",Uitsl),
  toAttrU("spoor",Spoor),
  toAttrS("usernote",UserNote),
  pos(Cat, _Card, _, Soort), 
  dim(WdNo, Dim),
  rondetekst(Dim, Soort, Ronde), !,
  toAttrs("rondetekst", Ronde).
wdJson(_,_).

w2Json(WdNo,Cat) :-   % als er planning is..
  %comline(LineBuffer),
  %upper_lower(LineBuffer, LBL),
  %searchstring(LBL, "/x", _),
  w2(WdNo,Cat,Tijd,_Gewaarsh1,_Gewaarsch2,_Fixed,_Vrij,Baan),
  toAttrU("plantijd",Tijd),
  toAttrS("planbaan",Baan),
  tijd_str(Tijd,Str,Dag),
  toAttrS("geplandom",Str),
  toAttrS("geplandop", Dag),
  !.
w2Json(_WdNo,_Cat).

eenOfTwee(WdNo, Cat, T1, "tgnst1") :-
  wd(WdNo,Cat,T1,_,_,_,_), !.
eenOfTwee(WdNo, Cat, T2, "tgnst2") :-
  wd(WdNo,Cat,_,T2,_,_,_), !.

w3Json(WdNo,Cat) :-   % als ie gespeeld is..
  %comline(LineBuffer),
  %upper_lower(LineBuffer, LBL),
  %searchstring(LBL, "/x", _),
  w3(WdNo, Cat, Tgnst, P1, P2, Stat, TijdLog),
  eenOfTwee(WdNo, Cat, Tgnst, Winnaar),
  %welkespelers(Winnaar, "W"),
  toAttrS("winnaar",Winnaar),
  toAttrU("punten1",P1),
  toAttrU("punten2",P2),
  term_str(uitslstat,Stat,StatS),
  toAttrS("status",StatS),
  toAttrU("tijdlog",TijdLog),
  tijd_str(TijdLog,Str,Dag),
  toAttrS("gespeeldom",Str),
  toAttrS("gespeeldop", Dag),
 !.
w3Json(_,_).
 
wdViewJson(WdNo, Cat, BaanNaam) :-
  %comline(LineBuffer),
  %upper_lower(LineBuffer, LBL),
  %searchstring(LBL, "/x", _),
  %teller(N),
  %N1 = N + 1,
  %assert(teller(N1)),
  wdJson(WdNo,Cat),
  w2Json(WdNo,Cat),
  w3Json(WdNo,Cat),
  toAttrS("baannaam",BaanNaam),
  %format(Key,"\"%u*%u\"",WdNo,Cat),
  findall(Attr, jsonattribuut(Attr), Alist),
  retractall(jsonattribuut(_)),
  list_to_string(Alist,",",Attrs),
  %format(Str, "\"%u\":{%s}",N,Attrs),
  format(Str, "{%s}",Attrs),
  assert(jsonobject(Str)), !.
wdViewJson(_No, _Cat, _).

welkespelers(p(N1, N2), Suffix) :-
  sp2(N1,_,_,_,_,_,_,_,_, _,_, _, BondsNr1, _, _, _, _, _, _, _,_,_,_,_,_,_,_), 
  bOfN(BondsNr1,N1,NS1),
  sp2(N2,_,_,_,_,_,_,_,_, _,_, _, BondsNr2, _, _, _, _, _, _, _,_,_,_,_,_,_,_),
  bOfN(BondsNr2,N2,NS2), 
  retractall(spelerVindex(N1)),
  assert(spelerVindex(N1)),
  retractall(spelerVindex(N2)),
  assert(spelerVindex(N2)),
  format(Str, "\"tgnst%s\":[\"%s\",\"%s\"]",Suffix,NS1,NS2),
  assert(jsonattribuut(Str)), !.
welkespelers(s(N1), Suffix) :-
  sp2(N1,_,_,_,_,_,_,_,_, _,_, _, BondsNr1, _, _, _, _, _, _, _,_,_,_,_,_,_,_), 
  bOfN(BondsNr1,N1,NS1),
  retractall(spelerVindex(N1)),
  assert(spelerVindex(N1)), 
  format(Str,"\"tgnst%s\":[\"%s\"]",Suffix, NS1),
  assert(jsonattribuut(Str)), !.
welkespelers(onbekend, _) :- !.
welkespelers(X, Suf) :-
  term_str(tgnst,X,Str),
  format(T, "%s %s",Str, Suf),
  dlg_note("tasvglob ", T).

toAttrS(Keyword, Value) :-
  fronttoken(Value, _,_),
  not(Value = "nee"), 
  not(Value = "&nbsp;"),
  term_str(string,Value,ValueEsc), !, % oneliner ... zelfde escape chars
% escapeJSON(Value, "", ValueEsc), !,
  format(Attr, "\"%s\":%s", Keyword, ValueEsc),
  assert(jsonattribuut(Attr)).
toAttrS(_Keyword, _Value).

toAttrU(Keyword, Value) :-
  Value <> 0, !,
  format(Attr, "\"%s\":\"%u\"", Keyword, Value),
  assert(jsonattribuut(Attr)).
toAttrU(_Keyword, _Value).

bOfN(BondsNr,_Nr,BondsNr) :-
  fronttoken(BondsNr, _, _), !.
bOfN(_, Nr, Uit) :-
  str_int(Uit,Nr).
 
afrondenOutput() :-
  findall(Obj, jsonobject(Obj), OList), % de wedviews in dit geval
  list_to_string(OList,",\n",OLstr),
  %format(Str, "\n%s}}\n",OLStr),
  format(Str, "\n%s]}\n",OLStr),
  appendtooutputW(Str),
  retractall(jsonobject(_)),
  retractall(jsonattribuut(_)),
  retract(spelerVindex(SpNo)),
  sp2(SpNo,_,Naam,_,_,_,_RD2,_Bet,_, _Club,_Ad, _Wnpl, BondsNr, _ES, _StD2O, _Gebdatum2, _, _, _Email, _,_,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_), 
  naamscan(ja,b_False,Naam, Naam1,_, _LC),
  aanwezigSp(SpNo, "", _, Aanwezig),
  toAttrU("spnr",SpNo),
  toAttrS("bondsnr",BondsNr),
  toAttrS("naam",Naam1),
  toAttrS("aanwezig",Aanwezig),
  findall(Attr, jsonAttribuut(Attr),AList),
  list_to_string(AList,",",Astring),
  retractall(jsonattribuut(_)),
  bOfN(BondsNr, SpNo, NrStr),
  format(SStr,"\"%s\":{%s}",NrStr,Astring),
  assert(jsonobject(SStr)),
  fail.
afrondenOutput() :-
 findall(Obj, jsonobject(Obj), OList),
 list_to_string(OList,",\n",OStr),
 format(Str, "{\n%s}",OStr),
 retractall(jsonobject(_)),
 appendToOutputS(Str), !.
afrondenoutput().

appendtoOutputW(Str) :-
  retract(dagstatusExport(Ex, Sp)), !,
  concat(Ex, Str, ExNw),
  assert(dagstatusExport(ExNw,Sp)).
appendtoOutputW(Str) :-
  assert(dagstatusExport(Str, "")).

appendtoOutputS(Str) :-
  retract(dagstatusExport(Ex, Sp)), !,
  concat(Sp, Str, SpNw),
  assert(dagstatusExport(Ex,SpNw)).
appendtoOutputS(Str) :-
  assert(dagstatusExport("", Str)).

dagmetwedstrijd(Dx) :-
  dag(D, Dx, _),
  dagmetwedstrijd1(D).  

dagmetwedstrijd1(Dag) :-
  findall(Noy, gespeeldOp(Dag, Noy, _Cat, _), NoyList),  % gespeeld ++++++++++++++++++++++++++++
  not(NoyList = []), !.
dagmetwedstrijd1(Dag) :-
  findall(T, bn(T, _), TList),
  sortint_c(TList),
  member(Tijd, TList),
  findall(Nox, geplandOp(Dag, wp(Nox, _, Tijd), _), NoxList),
  not(NoxList = []), !.

dagrapport2(Dag) :-
  %dag(Dag, Dsel, _V1), !,
  retractall(dagstatusQueued(_,_)),
  assert(dagstatusQueued("", Dag)), !.
  %dagstatusFile(DFile),
  %dagrapport1(DSel).

dagrapport(DFile) :-
  findall(Dx, dagmetwedstrijd(Dx),DxList),
  dlg_ListSelect("Welke toernooidag?",DxList,1,DSel,_Index),
  dagrapport1(DSel),
  dagstatusFile(DFile).
  
dagrapport1(DSel) :-
  retractall(dagstatusExport(_,_)),
  retractall(spelerVindex(_)),
  retractall(jsonattribuut(_)),
  retractall(jsonobject(_)),
  closefile(nawf),
  dagstatusFile(Dagbestand),
  retractall(dagstatusQueued(_,_)),
  openwrite(nawf, Dagbestand),
  writedevice(nawf),
  metaTag(Eerst, Tag),
  write(Eerst),
  write("\n<HTML>\n<HEAD>", Tag, "<TITLE>Dagstatus</TITLE>\n"),
  write("<STYLE type='text/css'>"),
  FontGr = "10pt",
  Kleur = "black",
  dagstatusinst(FGr, _),
  write("\nBODY {background-color: white; font-family: Arial; font-size:",FontGr,"; color:",Kleur,"};"),
  write("\nTABLE {background-color: white; font-family: Arial; font-size:",FGr,"; color:",Kleur,"; font-weight: bold;}"),
  %write("\nH5 {font-family: Arial}"),
  write("\nSPAN.bmvisible {display : none;}"), 
  write("\nSPAN.instel {font-size : 8pt;}"), 
  write("\n#insteldiv {border : thin solid red; margin: 1em; padding: 1em}"),
  write("\n</STYLE>"),
  write("\n</HEAD>\n"),
  write("\n<BODY>"),
  %vandaagOfMorgen(VandaagOfMorgen, Dag, _),
  dag(Dag, Dsel, _V1),
  dt_dateoffset_to_str(Dag, "%YL-%MD-%DD",Datum),  % soort van iso
  format(VStr,"{\"%s\":[",Datum),
  %VStr = "[",
  %format(VStr,"{\"nr\":%u,\"tekst\":\"%s\",\"datum\":\"%s\",\n",Dag,DSel,Datum),
  appendToOutputW(VStr),
  findall(WP, geplandOpX(Dag, WP), WPL),
  assert(kort("")),
  markeer(WPL, 10),
  assert(teller(0)),
  write("\n<TABLE border=0 cellpadding=2 cellspacing=2>"),
  findall(Noy, gespeeldOp(Dag, Noy, _Cat, _), NoyList),  % gespeeld ++++++++++++++++++++++++++++
  not(NoyList = []),
  write("\n<TR><TH><a name=\"1\"></a><a name=\"gespeeld\" id=\"gespeeld\"></a>\n", DSel, "</TH><TH colspan=3>UITSLAGEN</TH><td>&nbsp;<!--instel--></td></TR>"),
  wc(Cat, _, _, _, _, Kort0, _, __, _, _, _, _, _, _, _),
    zonderHekje(Kort0,"",Kort),
    findall(Nox, gespeeldOp(Dag, Nox, Cat, _), NoxList),
    not(NoxList = []),
    gespeeldOp(Dag, No, Cat, Tijd),
    isBookmark(wp(No, Cat, Tijd), BM),
    kortIsVeranderd(Kort, Kort1),
      format(KortS, "<TD>&nbsp;%s</TD><TD style='white-space:nowrap'>%s</TD>",BM, Kort1),
      wdViewJson(No, Cat, ""),
      rep_wed2(KortS, b_False, No, Cat, _Plat, Html),
      write(Html),
  fail.
dagrapport1(DSel) :- 
  %vandaagOfMorgen(VandaagOfMorgen, Dag, _),
  dag(Dag, DSel, _),
  findall(Noy, opdebaanOp(Dag, Noy, _, _, _), NoyList),
  not(NoyList = []),
  write("\n<TR style='font-size:1pt'><TD colspan=5 style='font-size:3pt;background-color:#00FF7F'>&nbsp;</TD></TR>"),
  write("\n<TR><TH><a name=\"2\"></a><a name=\"opdebaan\" id=\"opdebaan\"></a>\n", DSel, "</TH><TH colspan=3>PARTIJEN OP DE BAAN</TH><td>&nbsp;<!--instel--></td></TR>"),
  findall(Nox, opdebaanOp(Dag, Nox, Cat, _, _), NoxList),
  not(NoxList = []),
  %write("\n", Lang),
  opdebaanOp(Dag, No, Cat, Baan, Tijd),
    wc(Cat, _, _, _, _, Kort0, _, _, _, _, _, _, _, _, _),
    zonderHekje(Kort0,"",Kort),
    baannaamP(Baan, _Beschikbaar, _ParkToekomst, BaanNaam),
    wdViewJson(No, Cat, BaanNaam),
    isBookmark(wp(No, Cat, Tijd),BM), 
    format(Voorvoeg, "<TD style='background-color:#FFE4E1; font-weight:bold; text-align:right'>%s%s&nbsp;&nbsp;&nbsp;&nbsp;</TD><TD style='white-space:nowrap'>%s</TD>", BaanNaam, BM, Kort),
    rep_wed2a(Voorvoeg, No, Cat, Html),
    write(Html),
  fail.
dagrapport1(Tekst) :- 
  %vandaagOfMorgen(VandaagOfMorgen, Dag, _),
  findall(WP, geplandOp(Dag, WP, _), WPList),
  not(WPList=[]), 		% gepland ++++++++++++++++++++++++++++++++
  dag(Dag, Tekst, _),
  write("\n<TR style='font-size:1pt'><TD colspan=5 style='font-size:3pt;background-color:#00FF7F'>&nbsp;</TD></TR>"),
  write("\n<a name=\"3\"></a><a name=\"gepland\" id=\"gepland\"></a>\n<TR><TH>",Tekst,"</TH><TH colspan=3>GEPLANDE PARTIJEN</TH><td>&nbsp;<!--instel--></td></TR>"),
  findall(T, bn(T, _), TList),
  sortint_c(TList),
  member(Tijd, TList),
  findall(Nox, geplandOp(Dag, wp(Nox, _, Tijd), _), NoxList),
  not(NoxList = []),
  tijd_str(Tijd, TStr,_),
  writef("\n<TR><TH style=\"text-align: right\">%s</TH><TD colspan=3>&nbsp;<TD></TR>", TStr),
  wc(Cat, _, _, _, _, Kort0, _, _, _, _, _, _, _, _, _), 
    zonderHekje(Kort0,"",Kort),
    findall(Noz, geplandOp(Dag, wp(Noz, Cat, Tijd), _), NozList),
    not(NozList = []),
    geplandOp(Dag, wp(No, Cat, Tijd), Baan),
      baannaamP(Baan, _Beschikbaar, _ParkToekomst, BaanNaam),
      isBookmark(wp(No, Cat, Tijd),BM), 
      baannaamGepland(BaanNaam, BM, BaanNaamO),
      wdViewJson(No, Cat, BaanNaam),
      format(Voorvoeg, "%s<TD style='white-space:nowrap'>%s</TD>", BaanNaamO, Kort), 
      rep_wed2a(Voorvoeg, No, Cat, Html),
    write(Html),
  fail.
dagrapport1(DSel) :- 
  afrondenOutput(),
  dag(Dag, DSel, _),
  %vandaagOfMorgen(VandaagOfMorgen, Dag, _),
  findall(Noy, geplandOp(Dag, wp(Noy, _Cat, _), _), NoyList),
  not(NoyList = []),
  write("\n</TABLE>"),
  dag(Dag, Tekst, _),
  time(Hours,Minutes,_Seconds,_Hundredths),
  helestr_int(0, MStr, Minutes),
  write("\n<H5>Stand van de wedstrijden op ", Tekst, " om ", Hours, ":", MStr, "</H5>" ),
  fail.
dagrapport1(DSel) :-	% rapport van de dag hand
  write("\n</BODY></HTML>"),
  closefile(nawf),
  dag(Dag, DSel, _),
  dagstatusfile(File),
  retractall(dagstatusQueued(_,_)),
  assert(dagstatusQueued(File, Dag)),
  fail.
%dagrapport1(_) :-
%  dagstatusexport(W,_S), !,
%  write(W).
dagrapport1(_).

baannaamGepland(In, BM, Uit) :-
  fronttoken(In, _, _),
  format(Uit, "<TD style='background-color:#FFFF00; font-weight:bold; text-align:right'>%s%s&nbsp;&nbsp;&nbsp;&nbsp;</TD>", BM, In), !.
baannaamGepland(_, BM, Uit) :-
  format(Uit, "<TD>&nbsp;%s</TD>", BM).

spRankInLijst(Lijst, Rank) :-
  member(SpNo, Lijst),
  spelerdrukte(SpNo, _Onderdelen, _Afval, _Poule, _BijWinst, _Plaatsing, Rank, _SlotsVerhinderd, _DruksteCat).

knelpuntenRapport() :-
  closefile(work),
  get_TempDir(_, TempFile),
  openwrite(work,TempFile),
  findall(Catn,pos(Catn, _, _, _),Catl),
  time_stamp_next(BeginTijd,  _DagNo, _Tijd),
  %assert(found_vlag()),		% altijd... anders krijg je boodschap
  %Eind1 = BeginTijd + optimconst, % periode waarover geoptimaliseerd gaat worden
				  % zeven dagen. . . . .
  writedevice(work),
  tijd_kwart(D1,U1,M1,BeginTijd),
  dag(D1,DS1,_), 
  helestr_int(0,MS1,M1),
  writef("Knelpunt-wedstrijden in volgorde van urgentie (per % %.%).",  DS1,U1,MS1),
  write("\nDe spelerbezetting komt overeen met Onderdeel, Plan, Spelerbezetting,\ndaar kun je ook de rank terugvinden."),
  sortwed_c3(1, Catl, b_True, WdNo, Cat, _Urg, _Aantal),	% 12,5% van alle
  findall(Str, druktestring(Str), _List0),
  onderdelenlengte(MaxLen),
  Fudgefaktor = MaxLen - 10,		% voor presentatie
  grootste(0, Fudgefaktor, FudgeNetto),
  str_len(Fudge, FudgeNetto),
  grootste(MaxLen, 9, MaxLen1),
  format(FormaatStr, "\n%%4u. %%4u %%-25.25s  %%-%us %%4d  %%4d  %%4d  %%4d    %%4d", MaxLen1),
  wc(Cat, _, _, _, _, S, _, _, _, _, _, _, _, _, _), 
  %wc(Cat,_,_,S,_,_,_),
  wd(WdNo,Cat,T1,T2,_,_,_),
  write("\n+...................................................................................+"),
  gepland(WdNo,Cat,_J1,_J2,Geforceerd), 	% titel van de wedstrijd
  baan_adm(IBaan),
  repa(ja,'}',IBaan, nee,nee,0,2,1,WdNo,Cat,T1,alleplanning,TS1,_Score, _TtijdChanged,b_True,b_False,_Clubs, _An,_,_,_,_,_),
  repa(ja,'}',IBaan, nee,nee,1,2,1,WdNo,Cat,T2,alleplanning,TS2,_,_,b_True,b_False,_Clubs2, _An2,_,_,_,_,_Noti),
  format(Str,"\n    %s %c %s - % ",S,Geforceerd,TS1,TS2),
  persoonMoetNogSpelen(Cat, T1, _, _, _, BijwinstX, _, _),
  %dim(WdNo, Dim),
  writef("%60.60s, bij winst nog %u ronde(s).",Str,BijWinstX),
  de_spelers(WdNo, Cat,T1, T2, Lijst),
  writef("\n                                                %sPlaats Afval Poule Bijwinst Verh", Fudge),
  writef("\nRank  Waarde  Naam                    Nog in:   %sbonus  wedst wedst  wedst   inder", Fudge),
  findall(Rankx, spRankInLijst(Lijst, Rankx),RankL),
  sortint_c(RankL),
  member(Rank, RankL),
  spelerdrukte(SpNo, Onderdelen, Afval, Poule, BijWinst, Plaatsing, Rank, SlotsVerhinderd, _DruksteCat),
  sp2(SpNo,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  %sp1(SpNo,_,Naam,_,_Verh,_,_Bet,_UitKant,_Club,_,_,_,_,_,_,_,_,_,_),
  Dr = (Afval + Poule) * 2 + BijWinst + Plaatsing + SlotsVerhinderd,	% Drukte > 0 !
  format(Record, FormaatStr, Rank, Dr, Naam, Onderdelen, Plaatsing, Afval, Poule, BijWinst, SlotsVerhinderd),
  write(Record),
  %rep_wed1(Num, Cat, BeginTijd, Urg, ja),
  fail.
knelpuntenRapport() :-
  retractall(spelerdrukte(_,_,_,_,_,_,_,_,_)),  %!!
  closefile(work),  
  Parent = vpi_GetTaskWin(),
  dlg_rapport_Create(Parent, "Knelpunten","Knelpunten",b_False), !.
knelpuntenRapport.

misser(Line, Error) :-
  searchstring(Line, "found",_),
  format(Msg, "%u %s (in ServIt)", Error, Line), !,
  writetojournaal(Msg, ja).
misser(Line, _Error) :-
  searchstring(Line, "beschik",_),
  vpi_Alarm(mesbox_iconinformation),
  TaskWin = vpi_GetTaskWin(),
  toolbar_SetValue(TaskWin, idt_toolbarmsg, text_value("Acceptatieresultaat nog niet beschikbaar!")),
  fail.
misser(Line, Error) :-
  format(Msg, "%u %s", Error, Line),
  writetojournaal(Msg).

writeToJournaal(String) :-
  writeToJournaal(String, nee).

writeToJournaal(String, JaNee) :-
  writedevice(X),
  writeToJournaal1(String, JaNee),
  writedevice(X).

writeToJournaal1(String, ja) :-
  logclose(),
  dlg_MessageBox( "Journaal", String, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.
writeToJournaal1(String, _) :-
  closefile(journaal),
  getFolder(_Dir, Toernooien, _, _),
  filenamepath(FullName, Toernooien, jourconst),
  %format(FullName, "%s\\%s", Dir, jourconst),
  %maakRW(FullName),
  trap(openappend(journaal, Fullname),_,fail),
  writedevice(X),
  writedevice(journaal),
  time(Hours,Minutes,_Seconds,_Hundredths),
  helestr_int(0, MinStr, Minutes),
  date(Jaar, Maand, Dag),
  format(Msg,"\n%d-%d-%d %d:%s %s", Dag, Maand, Jaar, Hours, MinStr, String),
  write(Msg),
  closefile(journaal),
  writedevice(X),
  comline(LineBuffer),
  upper_lower(LineBuffer, LBL),
  searchstring(LBL, "/x", _), !,		% in TEST
  writedevice(screen),
  write("\n### to journaal: ",String),
  writedevice(X).
writeToJournaal1(_String, _).

%eenmalig :-
%  geweest, !.
eenmalig :-
  S2 = "NB De veiligheidsinstellingen van uw browser moeten ook javascript toestaan (Internetexplorer).",
  %S3 = "Deze aanwijzing wordt slechts eenmaal getoond.",
  format(Msg, "TK beheer opent hierna in uw standaard browser.\n\n%s\n\n", S2),
  _Answer = dlg_MessageBox( "Beheer", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ).
  %assert(geweest).

/*
newtkal(_Prov, BeheerId, Session) :-
  comline(LineBuffer),
  upper_lower(LineBuffer, LBL),
  searchstring(LBL, "/x", _), !,		% in TEST
  format(Session,"https://www.toernooiklapper.nl/tkal/tkal2016.php?%s", BeheerId), !.  % stap 2 isset(BEHEER)
newtkal(Prov, BeheerId, Session) :-
  format(Session,"http://%s/tkal2009.php?%s", Prov, BeheerId), !.  % stap 2 isset(BEHEER)
*/
  
tkalrequest1(Anchor) :-	% alleen gebruikt bij opgeven voor de kalender
  naam(ToernooiNaam, _, _Datum, _, _, _),
  fronttoken(ToernooiNaam, _, _),
  not(toernooiNaamWebOngelijk(b_False)),
  providerX(Prov, _PN, "", _),
  %format(Titel, "Controle paneel voor ToernooiKlapper (%s)", Anchor),
  Vars = aanmeldenVars(),
  format(Volgend,"http://%s/tkal2009.php?anchor=%s", Prov, Anchor),
  get_Tempdir(_Tdir, TempBestand),
  filenameext(Tempbestand, Name, _),
  filenameext(HetFrame, Name, ".html"),
  deletefile(HetFrame),
  %write("\n#### ", Volgend),
  downloadData(Volgend, HetFrame, Vars, _HttpRetcode, 0), % stap 1 isset(vrs) isset(anchor)
  file_str(HetFrame, Strr),
  write("\n",Strr),
  BeheerId = visBEHEERid(HetFrame),
  %newtkal(Prov, BeheerId, Session),
  format(Session,"https://www.toernooiklapper.nl/tkal/tkal2016.php?%s", BeheerId),  % stap 2 isset(BEHEER)
  %format(Session,"http://%s/tkal2009.php?%s", Prov, BeheerId),  % stap 2 isset(BEHEER)
  write("\n#### ", Session, "\n"),
  %dlg_browser_Create(Win, Session), 
  eenmalig(),
  Null = cast(api_hwnd,0),
  api_ShellExecute(Null,"open",Session,"","",1),!.
tkalrequest1(_Anchor) :-	% alleen gebruikt bij opgeven voor de kalender
  naam(ToernooiNaam, _, _Datum, _, _, _),
  not(fronttoken(ToernooiNaam, _, _)),
  _Answer = dlg_MessageBox( "Beheer", "U moet eerst een toernooimap openen!", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ).
  

visBEHEERid(Bestand, Id) :-
  file_str(Bestand, String),
  %write("\n",String),
  searchstring(String,"BEHEER=",Pos),
  Posa = Pos - 1,
  frontstr(Posa,String,_,Rest),
  searchstring(Rest, "\"", Pos1),
  Pos2 = Pos1 -1,
  frontstr(Pos2, Rest, Id, _), !.
visBEHEERid(_Bestand, "").

voegStatusNtoe(In, ["totN", String | In]) :-
  status(SL),
  term_str(sl, SL, String), !.
voegStatusNtoe(In,["totN", "geen" | In]).

usb2usb(V6, ["usb", "yes" | V6]) :-
  usb(ja), !.
usb2usb(V6, V6).

map(Map) :-
  webdir(Map, inet, _), !.
map(Map) :-
  toernooidir(Dir, _),
  lastdir(Dir, _, Map), !.
map("null").

sleuteltekstP(Id) :-
  sleuteltekst(Id), !.
sleuteltekstP(Id) :-
  status(SL),
  ontsleutel(SL, Id), !.
sleuteltekstP("is er niet!").

crit(200,0,11,b_False,b_False,b_False,b_False).

%  wc(Cat, _, _, _, _, Kort, _, _, _, _, _, _, _, _, _), 


categorie(EDG, Gesl, NaamKort) :-
  wc(CatNo, _, _, _, EDG, NaamKort, _SexList, _SpStrk, _LftV, _LftTM, HDGN, _Schemasoort, _Keuring, _KNLTB, _Geld),
  %wc(CatNo,_Naam,EDG,NaamKort,Sexlist,_Geld,_Verl),
  not(isondercat(CatNo)),
  xml_wedsex(HDGN, Geslacht, _GeslachtS),
  Geslacht = Gesl.

findcategoriecrits(_) :-
  assert(crit(200,0,11,b_False,b_False,b_False,b_False)),
  wc(_CatNo, _, _, _, EDG, _NaamKort, _SexList, SpeelSterkte, leeftijd(_,LeeftV), leeftijd(_,LeeftTM), HDG, Schemasoort, _Keuring, _KNLTB, _Geld),
  %wc(CatNo,_Naam,EDG,_NaamKort,_Sexlist,_Geld,_Verl),
  %not(isondercat(CatNo)),	% 5.9.2013 was al
  %wcrit(CatNo, Speelsterkte, LeeftV, LeeftTM, _Reserve),
  crit(MinLIn, MaxLIn, MaxSpIn, OokEnkelIn, OokDubbelIn, OokGDIn, ARin),
  minL(MinLIn, LeeftV, MinLOut),
  maxL(MaxLIn, LeeftTM, MaxLOut),
  maxSp(MaxSpIn, Speelsterkte, SpStOut),
  ookEnk(OokEnkelIn, EDG, OokEnkelOut),
  ookDubb(OokDubbelIn, EDG, OokDubbelOut),
  ookGD(OokGDIn, HDG, OokGDOut),
  ookAR(ARin, Schemasoort, ARuit),
  assert(crit(MinLOut, MaxLOut, SpStOut, OokEnkelOut, OokDubbelOut, OokGDOut,ARuit)),
  fail.
findcategoriecrits(CatLS) :-
  crit(MinL, MaxL, MaxSp, OokEnkel, OokDubbel, OokGD,AR),
  format(CatLS, "%u,%u,%u,%u,%u,%u,%u", MinL, MaxL, MaxSp, OokEnkel, OokDubbel, OokGD,AR).

minL(MinL, 0, MinL) :- !.
minL(MinL, X, MinOut) :-
  kleinste(MinL, X, MinOut), !.
  
maxL(MaxL, 0, MaxL) :- !.
maxL(MaxL, X, MaxOut) :-
  grootste(MaxL, X, MaxOut).
  
maxSp(MaxSp, X, MaxSp) :-
  not(fronttoken(X, _, _)), !.
maxSP(MaxSp, X, MaxOut) :-
  str_int(X, M),
  kleinste(MaxSp, M, MaxOut) , !.
maxSP(MaxSp, _X, MaxSp).

ookEnk(_OokEnkelIn, e, b_True) :- !.
ookEnk(OokEnkelIn, _, OokEnkelIn).

ookDubb(_OokDubbelIn, d, b_True) :- !.
ookDubb(_OokDubbelIn, g, b_True) :- !.
ookDubb(OokDubbelIn, _, OokDubbelIn).

ookGD(_OokGDIn, gemengd, b_True) :- !.
ookGD(OokGDIn, _, OokGDIn).

ookAR(_, Schemasoort, b_True) :-
  Schemasoort = "A", !.
ookAR(ARin, _Schemasoort, ARin).

%alsAlleenlezen(["alleenlezen","ja"]) :- !. % ################################ test
alsAlleenlezen(["alleenlezen","ja"]) :-
  alleenLezen1(), !.
alsAlleenlezen([]).

alledag(Out) :-
  dag(Dag, _, _),
  dt_dateoffset_to_str(Dag,"%YL-%MD-%DD",Out).

functionarisP(Ftype, Tid, Naam, Email, Tel1, Tel2, Tel3) :-
  functionaris(Ftype, Tid, Naam, Email,Tel1, Tel2, Tel3), !.
functionarisP(wedstrijdleider, _, Aanhef, Email, Tel, "", "") :-
  vereniging1(_VNum, _Club, Aanhef, _Straat, _Huisnr, _Plaats, _PC, Tel, Email, _Website), !.
functionarisP(_, _, "", "", "", "", "").

metBG(ja, BGNaam, BGEmail, BGNaam, BGEmail) :- !.
metBG(_, _BGNaam, _BGEmail, "", "").

getTid(Tids, Tid) :-
  Tids = [Tid|_], !.
getTid(_Tids, "").

toerType(TType) :-
  toernooiK(Nr, Type),
  term_str(toernooisoort, Type, TS), 
  format(TType, "%s*%s", Nr, TS).

aanmeldenVars(VarList) :-	% functie die VarList teruggeeft!
  status(L),
  L = [CRC|Status],
  status2get(Status, "", GetString),
  str_int(CRCS, CRC),
  V00 = alsAlleenLezen(),
  V1 = ["nr",CRCS | V00],
  form(_CatL, Uiterste, _Retour, _Mededeling, _MaxOnd, _InschrGeld, _GeenVerh, _MaxVerhS, _UitKant, _UitKantMax, _VerhWijze, _Voorweekend, _SpelerGeg, _PartnerGeg),
  dt_timeoffset_to_str(Uiterste, "%YL-%MD-%DD", UitersteStr), 
%vereniging1(IDC_TOERNOOIEXPORT_ORGVER_VALUE, IDCE_VERENIGING_VALUE, IDC_AANHEF_VALUE, IDC_ADRESVER_VALUE,IDC_HUISNR_VALUE,IDCE_PLAATS_VALUE,IDC_POSTCODE_VALUE,IDC_TOERNTELIN_VALUE,IDC_EMAILIN_VALUE,IDC_WEBSITET_VALUE),
  vereniging1(VNum, Club, Aanhef, _Straat, _Huisnr, Plaats, _PC, Tel, Email, Website),
  %vereniging1(VNum, Club, _Aanhef, Club, Plaats, _EMail, _BGNaam, _BGTel, Email, Website),
  naam(ToernooiNaam, Tids, _Datum, TicketI, District, _),
  getTid(Tids, Tid),
  %functionarisP(wedstrijdleider, Tid, WL, Email, WLtelef, _, _), 
  V2 = ["inschrijftm", UitersteStr, "resultaatemail", Email | V1],
  voegStatusNtoe(V2, V2a),
  functionarisP(bondsgedelegeerde, Tid, BGNaam, BGEmail, _, _, _), 
  maketicket(TicketI, Ticket),
  V3 = ["vrs", versie, "toernooinaam", Toernooinaam | V2a],
  %V3 = ["vrs", versie, "stap", "twee", "toernooinaam", Toernooinaam | V2a], 	% 15.2.2012 'stap' was nodig voor logic
  sleuteltekstP(Id),
  openbaar(Openb),
  metBG(Openb, BGNaam, BGEmail, BGNaamM, BGEmailM),
  V4 = ["wlt", Tel, "wl", Aanhef, "lic", Id, "tot", GetString, "bg", BGNaamM, "bgemail", BGEmailM | V3],
  V5 = ["club", Club, "plaats", Plaats | V4],
  findcategoriecrits(CatLS),
  findall(CatKortam, categorie(e, "M", CatKortam), CatKortListEM),
  findall(CatKortav, categorie(e, "V", CatKortav), CatKortListEV),
  findall(CatKortbm, categorie(d, "M", CatKortbm), CatKortListDM),
  findall(CatKortbv, categorie(d, "V", CatKortbv), CatKortListDV),
  findall(CatKortc, categorie(d, "G", CatKortc), CatKortListG),
  list_to_string(CatKortListEM, ",", CatKortListEMS),
  list_to_string(CatKortListEV, ",", CatKortListEVS),
  list_to_string(CatKortListDM, ",", CatKortListDMS),
  list_to_string(CatKortListDV, ",", CatKortListDVS),
  list_to_string(CatKortListG, ",", CatKortListGS),
  format(CatKortListS, "%s[]%s[]%s[]%s[]%s",CatKortListEMS,CatKortListEVS, CatKortListDMS,CatKortListDVS,CatKortListGS),
  V6 = ["wemail", EMail, "catstats", CatLS, "catskort", CatKortListS | V5],  
  findall(S, alledag(S), AlleDagen),
  list_to_string(AlleDagen, "#", AlleDagenS),
  janee2tekst(Openb, OpenbT),
  %write("\nplaatsbewijs=", Ticket),
  emailadresZelfP(Afzender, _), !,	% 23.10.2006
  map(Map),
  usb2usb(V6, V7),
  api_getComputerName(PCname),
  findall(TTs, toerType(TTs), TTSL),
  list_to_string(TTSL, "#", TTList),
  %term_str(slist, Tids, NrList),
  VarList = ["district", District, "vnum", VNum, "tdat", AlleDagenS, "tnr", TTList, "website", WebSite, "plaatsbewijs", Ticket, "open",
    OpenbT, "afzender", Afzender, "map", Map, "pcname", PCname | V7],
  !.
%  findall(TTs, toerType(TTs), TTSL),
%  list_to_string(TTSL, "#", TTList),
%  VarList = ["district", District, "vnum", VNum, "tdat", AlleDagenS, "tnr", NrList, "website", WebSite, "plaatsbewijs", Ticket, "open",
%    OpenbT, "afzender", Afzender, "map", Map, "pcname", PCname, "toertypes", TTList | V7],
%  !.

aanmeldenVars(["lic", Lic, "vrs", versie, "totN", String, "nr", CRCS, "tot", GetString, "map", Map, "pcname", PCName]) :-	% voor het opstarten
  map(Map),
  status(SL),
  ontsleutel(SL, Id), 
  reversestr(Id, "", Lic),
  term_str(sl, SL, String),
  SL = [CRC|Status],
  status2get(Status, "", GetString),
  str_int(CRCS, CRC),
  api_getComputerName(PCName), !.
aanmeldenVars(["vrs", versie, "totN", "geen", "map", Map, "pcname", PCName ]):-
  map(Map),
  api_getComputerName(PCName),  !.

janee2tekst(ja, "1") :- !.
janee2tekst(_, "0").

toernooidag(Diff) :-
  date(Year,Month,Day),
  dt_date_to_offset(Year, Month, Day, Off2),
  dag(Dag1, _,_),
  Diff = Off2 - Dag1, !.
toernooidag(0).

belangrijkeEventCounter(0).

resetBelangrijkeEventCounter() :-
  assert(belangrijkeEventCounter(0)).

belangrijkeEventsP(N) :-
  belangrijkeEventCounter(N), !.
belangrijkeEventsP(0).
  
incrBelangrijkeEventCounter() :-
  belangrijkeEventsP(N),
  N1 = N + 1,
  %retractall(belangrijkeEvents(_)),
  assert(belangrijkeEventCounter(N1)), !.

database - opgvis
  opgcode(string)
  
clauses

vergelijkTicket(T, T) :- !.
vergelijkTicket(_, TicketN) :-
  not(fronttoken(TicketN, _, _)), !.
vergelijkTicket(_, TicketN) :-
  naam(ToernooiNaam, Nr, Datum, _, Res1, Res2),
  logf('A', naam(ToernooiNaam, Nr, Datum, TicketN, Res1, Res2),nee), !.
%vergelijkTicket(Ticket, TicketN) :-
%  format(Msg, "Nieuwe internetID:\nnieuw: %s\nvorig: %s.",TicketN, Ticket),
%  provider(_, PN),
%  format(MID, "InternetID op %s.", PN),
%  dlg_MessageBox( MID, Msg, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
%  naam(ToernooiNaam, Nr, Datum, _, Res1, Res2),
%  logf('A', naam(ToernooiNaam, Nr, Datum, TicketN, Res1, Res2),nee), !.

synchKlapper(R, Bestand, b_False) :-
  R <> 0,
  not(existfile(Bestand)),
  writef("\nBestand %s bestaat niet en/of returncode = %u.", Bestand, R), !.
synchKlapper(0, Bestand, _) :-
  openread(work, Bestand),
  readdevice(work),
  repeat_f(work),
  readln(Line),
  searchstring(Line,"<!--#",FP1),
  FP1 = 1, 
  frontstr(5,Line,_,TicketN),
  fronttoken(TicketN, Fr, _),
  %write("visTicketUit: ",Fr, "\n"),
  naam(_ToernooiNaam, _Nr, _Datum, Ticket, _Res1, _Res2),
  vergelijkTicket(Ticket, Fr),
  fail.
synchKlapper(0, _Bestand, b_True) :-
  filepos(work, 0, 0),
  repeat_f(work),
  readln(Line),
  searchstring(Line, "FTPnotOK", _), !,
  closefile(work). 
synchKlapper(_, _, b_False) :-
  closefile(work).

isNRt(Cat) :-		% NB geldt voor hoofdrondes
  wc(Cat, _, _, _, _, _NaamKort, _SexList, _SpSt, _LftV, _LftTM, _HDG, _Schemasoort, _Keuring, Tid, _Geld),
  toernooiK(Tid,nrt), !.

isKwalificatie(Cat, b_True) :-		% b_True = sectie
  kopl(Cat, _, CatNrt, _),
  isNrt(CatNrt),
  pos(Cat, _Card, _, afval), !.
isKwalificatie(Cat, b_False) :-		% b_False = nog niet gesplitst
  kopl(Cat, _, CatNrt, _),
  isNrt(CatNrt),
  not(pos(Cat, _, _, _)), !.

vertaaldistrict("MBR", "CBR") :- !.
vertaaldistrict("HER", "CBR") :- !.
vertaaldistrict("AMS", "IJM") :- !.
vertaaldistrict("HLM", "IJM") :- !.
vertaaldistrict("GRO", "GDR") :- !.
vertaaldistrict("DRE", "GDR") :- !.
vertaaldistrict(X, X).

district1(0, "", "-- Selecteer --").
district1(1, "BRO", "Brabant Oost").
district1(2, "CBR", "Centraal Brabant").
district1(3, "WBR", "West Brabant").
district1(4, "DHG", "Den Haag").
district1(5, "FRI", "Friesland").
district1(6, "GEL", "Gelderland").
district1(7, "GDR", "Groningen/Drenthe").
district1(8, "IJM", "IJmond").
district1(9, "INT", "Internationaal").
district1(10, "UTR", "Utrecht").
district1(11, "LEI", "Leiden").
district1(12, "LIM", "Limburg").
district1(13, "NHN", "N-Holland N").
district1(14, "OVE", "Overijssel").
district1(15, "RDM", "Rotterdam").
district1(16, "ZEE", "Zeeland").

predicates
procedure  id2letters(string, string, string)
%procedure  stripSuffix(string, string)
  erIsEenRest(slist Rest, integer)
%  zelfdeweek(integer, integer)
procedure ifNoMList(slist, string)

clauses

/*makeTicket(_, NewTicket) :-  	% 19.4.2010 nog geen ticket vooraan
  comline(LineBuffer),
  upper_lower(LineBuffer, LBL),
  searchstring(LBL, "/x", _),		% in TEST
  status(L),
  L = [CRC|_],
  str_int(CRCS, CRC),
  dag(Date, _, _),
  dt_dateoffset_to_str(Date, "%WD", WeekNr), 
  id2Letters(CRCS, "", Letters),
  format(NewTicket, "%s%s", Letters, WeekNr), !.*/
makeTicket("", NewTicket) :-  	% 19.4.2010 nog geen ticket vooraan
  status(L),
  L = [CRC|_],
  str_int(CRCS, CRC),
  dag(Date, _, _),
  dt_dateoffset_to_str(Date, "%WD", WeekNr),
  %zoekMaandag(_, _, WeekNr, _),
  id2Letters(CRCS, "", Letters),
  format(NewTicket, "%s%s", Letters, WeekNr), !.
/*makeTicket(Ticket, Ticket) :-
  %zoekMaandag(_, No, _, _),
  getbacktrack(Here),
  dag(No, _, _),
  cutbacktrack(Here),	% alleen de eerste dag
  dt_week_year_to_offset(_WeekNr,Year, No),
  date(Jaar,_Month,_Day),
  Year <> Jaar, !.			% ander jaar -> doe niets!
makeTicket(Ticket, NewTicket) :-
  %comline(LineBuffer),
  %upper_lower(LineBuffer, LBL),
  %not(searchstring(LBL, "/ta", _)),		% in TEST
  stripSuffix(Ticket, Ticket1),
  str_len(Ticket1, Len),
  L1 = Len - 2,
  frontstr(L1, Ticket1, _, WkNr1), 
  toernooiWeekJaar(_, WeekNr, _),
  trap(str_int(WkNr1, W1), _, fail), 	% 18.4.2010 wees voorzichtig
  not(zelfdeweek(W1, WeekNr)),
  %trap(str_int(WeekNr, W),_, fail),
  %WeekNr <> WkNr1,
  WeekNr > W1 + 2,
  WeekNr < W1 - 2,
  status(L),
  L = [CRC|_],
  str_int(CRCS, CRC),
  id2Letters(CRCS, "", Letters),
  format(NewTicket, "%s%s", Letters, WeekNr), !.*/
makeTicket(Ticket, Ticket).

/*
zelfdeweek(53, 01).	% 3.1.2009

stripSuffix(Ticket, Uit) :-
  reversestr(Ticket, "", Tekcit),
  frontchar(Tekcit, Char, Tekcit1),
  Char < '0', !,
  reversestr(Tekcit1, "", Ticket2),
  stripSuffix(Ticket2, Uit).   
stripSuffix(Ticket, Uit) :-
  reversestr(Ticket, "", Tekcit),
  frontchar(Tekcit, Char, Tekcit1),
  Char > '9', !,
  reversestr(Tekcit1, "", Ticket2),
  stripSuffix(Ticket2, Uit).   
stripSuffix(Ticket, Ticket) .
*/
id2letters(Id, In, Uit) :-
  frontchar(Id,Char,Rest),
  Char = '-', !,
  concat(In, "Z", In1),
  id2letters(Rest, In1, Uit).
id2letters(Id, In, Uit) :-
  frontchar(Id,Char,Rest),
  char_int(Char, CharI), !,
  char_int('0', C),
  char_int('A', Base),
  NCharI = Base + CharI - C,
  char_int(NChar, NCharI),
  format(In1, "%s%c", In, NChar),
  id2letters(Rest, In1, Uit).
id2letters(_, Uit, Uit).

erIsEenRest([], _) :- !.
erIsEenRest([M1 | T], N) :-
  N1 = N + 1,
  checkvoortgang(N, M1),
  erIsEenRest(T, N1).

ifNoMList([], "geen response\nfirewall??") :- !.
ifNoMlist(MList, String) :-
  list_to_string(MList, "\n", String).

licentiecheck(XX, Reststring) :-	% FTP (tascomm) OPG (tasverh) EIND (tasprnt) etc.
  ParmsList = aanmeldenVars(),	% NB er is ook de internetid check!
  providerX(Provider, _, "", _),		% BIJ voor bijwerken vh ledenbestand
  NParms = ["gevraagd", XX | ParmsList],
  format(URL, "http://%s/inetcheck.php", Provider),
  get_tempdir(TDir, _),
  %get_Toernooidir(TDir, _, _, _), 
  filenamepath(FeedbackXML, TDir, "feedback.xml"),
  deletefile(FeedbackXML),
  0 = downloadData(URL, FeedbackXML, NParms, _, 0),
  file_str(FeedbackXML, Tekst),
  searchstring(Tekst,"is akkoord",_FP1),
  parsemsg(Tekst, MList, Reststring),
  MList = [_Akkoord | Rest],
  erIsEenRest(Rest, 2), !,
  trap(deletefile(FeedbackXML),_,true).
licentiecheck(_, _) :-
  get_tempdir(TDir, _),
  %get_Toernooidir(TDir, _, _, _), 
  filenamepath(FeedbackXML, TDir, "feedback.xml"),
  file_str(FeedbackXML, Tekst),
  trap(deletefile(FeedbackXML),_,true),
  parseMsg(Tekst, MList, _),
  ifNoMlist(MList, String),
  %list_to_string(MList, "\n", String),
  dlg_MessageBox( "Licentie/versie", String, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  checkvoortgang(99, "??"),	% met delay
  fail.

providerX("test.toernooiklapper.nl", "ToernooiAssistent", In, OL) :-
  comline(LineBuffer),
  upper_lower(LineBuffer, LBL),
  searchstring(LBL, "/ta", _),
  format(OL, "http://test.toernooiklapper.nl/Htabin/%s", In), !.
providerX("tas.toernooiklapper.nl", "ToernooiKlapper", In, OL) :- % echt !
  format(OL, "http://tas.toernooiklapper.nl/Htabin/%s", In).

visSession(File, Id, Session) :-  % moet op \n eindigen!
  file_str(File, Str),
  write(Str),
  searchstring(Str,Id,FP),
  FP1 = FP-1,
  frontstr(Fp1, Str, _, Rest),
  searchstring(Rest, "\n", FP2),
  FP2a = FP2 - 1, 
  frontstr(FP2a, Rest, Session, _), 
  write("session = ", Session, "\n"), !.
visSession(_File, _Id, "").

toernooiTypeP(Nummer, Type) :-
  toernooiK(Nummer, Type), !.
toernooiTypeP(_, anders).
  

database - htimer
single hTime(integer Hours, integer, integer)

predicates
procedure verstreken(string, integer, integer, integer, integer, integer, integer)

clauses
htime(0, 0, 0).

htmlTimer(Tekst) :-
  writedevice(X),
  comline(LineBuffer),
  upper_lower(LineBuffer, LBL),
  searchstring(LBL, "/x", _), 		% alleen in TEST
  time(HoursI,MinutesI,SecondsI,_),
  hTime(HoursS, MinutesS, SecondsS),
  writedevice(screen),
  verstreken(Tekst, HoursI, HoursS, MinutesI, MinutesS, SecondsI, SecondsS),
  writedevice(X),
  assert(hTime(HoursI, MinutesI, SecondsI)), !.
htmlTimer(_Tekst).
  
verstreken(Tekst, _HoursI, _HoursS, _MinutesI, _, _SecondsI, _0) :-
  searchstring(Tekst,"egin",_FoundPos),
  write("\n", Tekst), !.
verstreken(Tekst, _HoursI, _HoursS, MinutesI, MinutesS, SecondsI, SecondsS) :-
  TijdI = MinutesI * 60 + SecondsI,
  TijdS = MinutesS * 60 + SecondsS,
  V = TijdI - TijdS,
  writef("\n%s verstreken %u", Tekst, V).

/*
facts - pp
ppRonde(wedscat, integer Ronde)

predicates
  procedure isPoulePouleA(wedscat, integer)
  procedure zoekPProndes(wedscat Cat, integer MaxRondes, integer N) 
  procedure getFolder1(string, string)

clauses
isPoulePoule(Cat, Diepte) :-
  not(pos(Cat, _, _, _)), 
  isPoulePouleA(Cat, 0),
  findall(X, ppRonde(_, X), NL),
  sortint_c(NL),
  reverse(NL, [], NLR),
  NLR = [Diepte|_],
  Diepte > 1,
  findall(Y, ppRonde(Y, 0), YL),
  count(YL, 0, AantY),
  AantY > 8,
  retractall(ppRonde(_, _)),  !.
isPoulePoule(_, _) :-
  retractall(ppRonde(_,_)),
  fail.

isPoulePouleA(Cat, N) :-
  kopl(CatV, _, Cat, _),
  not(ppRonde(CatV, _)),
  assert(ppRonde(CatV, N)),
  N1 = N + 1,
  isPoulePouleA(CatV, N1),
  fail.
isPoulePouleA(_Cat, _N).

zoekPProndes(Cat, MaxRondes, N) :-
  kopl(CatR, _, Cat, _),
  not(ppRonde(CatR, _)),
  Ronde = MaxRondes - N + 2,
  assert(ppRonde(CatR, Ronde)),
  N1 = N + 1,
  zoekPProndes(CatR, MaxRondes, N1),
  fail.
zoekPProndes(_, _, _).

ppRondeCats_nd(Cat, N, CatPP) :-
  isPoulePoule(Cat, Diepte),	% om de diepte te bepalen
  zoekPProndes(Cat, Diepte, 1),
  wc(CatPP, _, _, _, _, _, _, _, _, _, _, _, _, _, _), 
  %wc(CatPP,_CatNaam,_,_,_,_,_),	% juiste volgorde
  ppRonde(CatPP, N).
ppRondeCats_nd(_, _, _) :-
  retractall(ppRonde(_,_)),
  fail.
*/
domains
shGetFolderPath = procedure(integer,integer,integer,integer,string)-(i,i,i,i,i) language stdcall

clauses
getFolder(X1, Toernooien, Gedeeld, Backups) :-
  usb(ja),
  syspath(ExeStartupPath,_ProgName),
  lastdir(ExeStartupPath, Path, _),
  met_bslash(Path, X1), 
  concat(X1, "Toernooien", Toernooien),
  concat(X1, "Toernooien\\Gedeeld", Gedeeld),
  concat(X1, "Toernooien\\Backups", Backups), !.
getFolder(X, Toernooien, Gedeeld, Backups) :-
  trap(DllHandle = vpi_LoadDll("SHFolder.dll"),_,fail),
  NameFolder=cast(shGetFolderPath,vpi_GetDllProc(DllHandle,"SHGetFolderPathA")),
  str_len(X,1000),
  NameFolder(0,0x0005,0,0,X),
  %dlg_note(X),
  vpi_FreeDll(DllHandle),
  %getFolder1(X, X1),
  %concat(X, "\\", X1).
  concat(X, "\\Toernooien", Toernooien),
  concat(X, "\\Toernooien\\Gedeeld", Gedeeld),
  concat(X, "\\Toernooien\\Backups", Backups), !.
  %write("\n### ", Toernooien), !.
getFolder("", "", "","").  

/*
getfolder1(_, PathMet) :-
  usb(ja),
  syspath(ExeStartupPath,_ProgName),
  lastdir(ExeStartupPath, Path, _),
  met_bslash(Path, PathMet), !.
getFolder1(X, X1) :-
  concat(X, "\\", X1).
*/
verbindingMislukt(0) :- !, fail.
verbindingMislukt(_) :- 
  dlg_MessageBox( "Verbinding", "Verbinding is mislukt.\nProbeer nog een keer als er internetverbinding is.", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ).

domains 
  resultaat = resultok(string);resultnok(string)

predicates
procedure onlinebackupResultaat(integer, resultaat)
determ reportErr(string)
veranderLetter(char Letter, string TDir, string Uit)
okChars(char)

clauses
onlinebackup(Reden) :-
  cursor_SetWait(),
  webdir(_, inet, _),
  not(geenServer(_)),
  providerX(_Prov, _, "backupsingle.php", URL),
  VarList = aanmeldenVars(),
  VarList1 = ["reden", Reden | VarList],
  get_TempDir(Map, _),
  filenamePath(FullName, Map, "onlinebackupresultaat.txt"),
  deletefile(FullName),
  %Return = getbackup(URL, FullName, VarList1),
  Return = downloadData(URL, FullName, VarList1, _ResponseX, 0),
  file_str(FullName, Str),
  %write("\n",Str),
  trap(term_str(resultaat, Term, Str), _, reportErr(Str)),
  onlinebackupResultaat(Return, Term), !.
onlinebackup(_Reden).
  
onlinebackupResultaat(0, resultok(String)) :-
  _Answer = dlg_MessageBox( "On-line backup", String, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), !.
onlinebackupResultaat(0, resultnok(String)) :-
  _Answer = dlg_MessageBox( "On-line backup", String, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), !.
onlinebackupResultaat(X, _) :-
  format(Msg, "Verbinding mislukt.\nRC=%u.\nProbeer nog een keer?", X),
  _Answer = dlg_MessageBox( "On-line backup", Msg, mesbox_iconerror, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), !.
  
reportErr(Str) :-	% 15.11.2010
  not(fronttoken(Str, _, _)), !.
reportErr(Str) :-
  writeToJournaal(Str, ja),
  fail.

okChars('\\').
okChars('C').
okChars('c').
okChars('D').
okChars('d').

veranderLetter(Letter, TDir, Uit) :-
  frontchar(TDir, Fchar, TDirR),
  not(okChars(Fchar)),
  frontchar(Uit, Letter, TDirR), !.
veranderLetter(_, TDir, TDir).
  
getToernooiDirs(Tdir) :-
  getfolder(_, Dir, _, _),
  met_bslash(Dir, Tdir). 
getToernooiDirs(Tdir) :-
  not(usb(ja)),
  toernooiDirs(TDir).
getToernooiDirs(TdirUit) :-
  usb(ja),
  syspath(ExeStartupPath,_ProgName),
  frontchar(ExeStartupPath, Letter, _),
  toernooiDirs(TDirIn),
  veranderLetter(Letter, TDirIn, TDirUit).
  
voegtoeaantoernooidirs(_Dir) :-
  alleenlezen1, !.
voegtoeaantoernooidirs(Dir) :-
  getToernooidirs(Dirx),  	% verwijder hem als ie bestaat
  met_bslash(Dir, Dir1),
  upper_lower(Dir1, Dirx), 
  %write("\n verwijder: ",DirX),
  retract(toernooidirs(Dirx)),
  fail.
voegtoeaantoernooidirs(Dir) :-
  met_bslash(Dir, Dir1),	% en voeg hem vooraan weer toe
  getfolder(_, DirF, _, _),
  met_bslash(DirF, DirFM),
  not(upper_lower(DirFM, Dir1)),
  assertA(toernooidirs(Dir1)),
  format(Msg, "### toegevoegd aan toernooidirs: %s", Dir1),
  writetojournaal(Msg),
  findall(X, toernooidirs(X), XL),
  count(XL, 0, N),
  N > 3,
  reverse(XL, [], XLR),
  XLR = [XR | _],
  retract(toernooidirs(XR)), !.  % en verwijder de laatste als meer dan drie
voegtoeaantoernooidirs(_Dir).

toBR(In, Uit) :-
  searchstring(In,"\n",FoundPos),
  FoundPos > 0,
  F0 = FoundPos - 1,
  frontstr(F0, In, Begin, Rest),
  %F1 = Len + 1,
  frontstr(1, Rest, _, Rest1),
  format(In1, "%s<br>%s", Begin, Rest1), !,
  toBR(In1, Uit).
toBR(Uit, Uit).

checkServer(0) :-
  retractall(geenServer(_)), !.
checkServer(Status) :-
  not(geenServer(_)),
  format(Msg, "De internetverbinding met ToernooiKlapper werkt niet of hikt.\nPubliceren en gedeeld niet mogelijk! (%d)\n\nOpnieuw opstarten kan helpen.", Status),
  _Answer = dlg_MessageBox( "Verbinding", Msg, mesbox_iconerror, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  writetojournaal(Msg, nee),
  retractall(geenServer(_)),
  assert(geenServer(Status)),
  fail.
  