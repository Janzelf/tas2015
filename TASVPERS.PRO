/*****************************************************************************

		Copyright (c) 1989 - 1998 De Lint Associates

 Project:  TASVPI
 FileName: TASVPERS.PRO
 Purpose: Display scherm met alle persoonsgegevens
 Written by: JdL
 Comments:
******************************************************************************/

include "tasvp1.inc"
include "tasvp1.con"
include "hlptopic.con"


DATABASE - persoon

%persoon(WINDOW, integer Persno, string Naam, verhl Verhinderingen, sex Spsrt) ook in tasverh!
%single resultaatP(integer)
single gegevensS(dbasedom)
%determ timerid1(long)

PREDICATES

club2string(string, string)
string2club(string, string)
voorkeus(integer Keus, integer Index, integer Waarde, SLIST Waardes)
%sterkteKeus
remVerh(verhl In, integer Index, verhl)
spsrtSel(sex, SLIST, integer, integer)
procedure safestr_int(string, integer)
betaald2gezien(janee, boolean Gezien, boolean Betaald) - (i,o,o) 
gezien2betaald(janee, boolean Gezien, boolean Betaald) - (o,i,i)
enableWis(WINDOW, boolean)
%gevuld(slist, BOOLEAN)
  clubveld(WINDOW ctrlwin, string)
  nondeterm teken(char)
  scheidingsteken(string)
%nondeterm  persoonsItems(string, string, string, MENU_ITEM)
  gelijk(dbasedom)
  getsp(WINDOW, dbasedom)
  setsp(WINDOW, dbasedom)
procedure enableWaarsch(integer, BOOLEAN)
procedure fillOpgList(WINDOW, integer PersNo, slist OpgList)
%procedure startGezienTimer(WINDOW, janee Gewaarschuwd)
%procedure timer_zwijg()
  scheidingstekenN(string Naam)
  nondeterm tekenN(char)
%determ ratingNogMaagdelijk(slist, slist)
procedure setUitKant(window Win, integer UitKantN)
procedure getUitKantoor(window, integer)
procedure loadBankNr(window Win, janee Banknummers, string BankNr, string Akkoord)
procedure speler_extra(integer SKeus, string CheckGeg, string Log, slist Import)
procedure checked2akkoord(boolean, string)
procedure akkoord2checked(string, boolean)
%procedure addNote(integer SpNo)
procedure minutensel(integer, slist, slist)
%procedure addpaspoort(string Geb, boolean Pasp, string Uit)
%procedure splitGeb(string, string, boolean, paspoort)

CLAUSES


gegevensS(sp2(0,0,"","",[],"","",nee,0, "", "", "", "", "", "", "", "", "", "", "",nee,"","","","","",[])).

close_Formulier(SKeus) :-
  persoon(Win, SKeus, _, _, _, _, _, _),
  win_Destroy(Win), !.
close_Formulier(_).

close_Formulier() :-
  persoon(Win, _SKeus, _, _, _, _, _, _),
  win_Destroy(Win), !.
close_Formulier().

betaald2gezien(omheteven, b_False, b_False):- !.
betaald2gezien(nee,       b_True,  b_False):- !.
betaald2gezien(ja,        b_True,  b_True) :- !.

gezien2betaald(omheteven, b_False, b_False):- !.	% (o,i,i) ...
gezien2betaald(nee,       b_True,  b_False):- !.
gezien2betaald(ja,        b_True,  b_True) :- !.
gezien2betaald(ja,        b_False, b_True) :- !.

safestr_int(Str, Int) :- 
  str_int(Str, Int), !.
safestr_int(_, 0).

voorkeus(0, _, 0, _) :- !.
voorkeus(Int, Int, Waarde, [Str|_]) :-
  str_int(Str, Waarde), !.
voorkeus(Int, N, Waarde, [_|Rest]) :-
  N1 = N + 1,
  voorkeus(Int, N1, Waarde, Rest), !.
voorkeus(0, _, _, _).

club2string(Club, String) :-
  club(Club, String, _), !.
club2string(X,X).

string2club(X, X) :-
  trim_c(X),
  X = "", !.
string2club(String, Club) :-
  trim_c(String),
  club(Club, String, _), !.
string2club(String, Club) :-
  str_len(String, Len),
  Len > 5, !,
  frontstr(5,String,Club,_).
string2club(X, X).

remVerh([], _, []) :-!.
remVerh([_|Rest], 0, Rest) :- !.
remVerh([A|Rest], Index, [A|Out]) :-
  I1 = Index - 1,
  remVerh(Rest, I1, Out).

spsrtSel(Sex, [SexStr|_], Uit, Uit) :-
  spsrt(Sex, SexStr, _Sexe, _, _, _, _, _), !.
spsrtSel(Sex, [_|SPSRTLIST], In, Uit) :-
  In1 = In + 1,
  spsrtSel(Sex, SPSRTLIST, In1, Uit).

enableWis(Win, b_False) :-
  persoon(Win, Sp,_,_,_,_,_, _),
  sp2opg(Sp,_,_,_,_,_), !.
enableWis(_Win, b_True).

sterkteSel(Sterkte, [H|_], N, N) :-
  fronttoken(H, I, _),
  Sterkte = I, !.
sterkteSel(Sterkte, [H|_], N, N) :-
  fronttoken(H, _I, R),
  fronttoken(R, _, R1),
  upper_lower(St, Sterkte),
  St = R1, !.
sterkteSel(Sterkte, [_|T], N, Nu) :-
  N1 = N + 1, !,
  sterkteSel(Sterkte, T, N1, Nu).
sterkteSel(_, _, _, 0).		% 7.11.2001

spstItem2SpSterkte(StIn, Spsterkte) :-
  fronttoken(StIn, Spsterkte,_), !.
spstItem2SpSterkte(_StIn, "").

getUitKantoor(Win, UitKantN) :-
  UitKWin = win_GetCtlHandle(Win, idc_pers_uitkantoor),
  Uitkantoor = win_GetText(UitKWin),
  safestr_int(UitKantoor, Uur),
  MinWin = win_GetCtlHandle(Win, idc_pers_min),
  Minuten = win_GetText(MinWin),
  safestr_int(Minuten, Min),
  tijd_kwart(0,Uur,Min,UitkantN).
  
minutensel(Laatste, In, In) :- 
  roosterinterval(N),
  Laatste >= 60 - N, !.
minutensel(Van, In, [H | Uit]) :-
  roosterinterval(RI),
  Tijd1 = Van + RI,
  str_int(H, Tijd1),
  minutensel(Tijd1, In, Uit).

setUitKant(Win, UitKant) :-
  tijd_kwart(_Dag,Uur,Min,Uitkant), 
  UitKantUurWin = win_GetCtlHandle(Win, idc_pers_uitkantoor),
  Uren = ["  "," 9","10","11","12","13","14","15","16","17","18","19","20","21"],
  lbox_Clear(UitKantUurWin),
  lbox_Add(UitKantUurWin, Uren),
  voorkeus(U, 0, Uur, Uren),
  lbox_SetSel(UitKantUurWin, U, b_True),
  UitKantMinWin = win_GetCtlHandle(Win, idc_pers_min),
  minutensel(0, [""], Minuten),
  %Minuten = ["","00","15","30","45"],
  lbox_Clear(UitKantMinWin),
  lbox_Add(UitKantMinWin, Minuten),
  voorkeus(M, 1, Min, Minuten),  	% 28.5.2013
  lbox_SetSel(UitKantMinWin, M, b_True), !.
setUitKant(_Win, _UitKant).

splitGeb(Geb, Geb, b_False, geen) :-  % haal de P er vanaf
  not(searchchar(Geb,'P',_)), !.
splitGeb(Geb, GebU, b_True, groen) :-
  frontchar(Geb,_FrontChar,GebU), !.
splitGeb(Geb, Geb, b_False, geen).
  
setsp(Win, SpelerIn) :-
  SpelerIn = sp2(SKeus,Sex,Naam,Tel,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, LidNr, Sterkte, SterkD, Geb, Tel2, Tel3, EMail, _Opm,Gezien,RangE,RangD,Incasso,_CheckGeg,Akkoord,_Import),
%  	SpelerIn = speler(SKeus, Sex, Naam, Tel, Verh, Misc, _Betaald, Uitkant,
%		    Club,Adres,Woonpl,LidNr, Sterkte,SterkD,Geb,Tel2,Tel3,Email,_Opm,_),
	loadBankNr(Win, ja, Incasso, Akkoord),
	REWin = win_GetCtlHandle(Win, idct_ratinge),
	win_SetText(REWin, RatingE),
	RDWin = win_GetCtlHandle(Win, idct_ratingd),
	win_SetText(RDWin, RatingD),
	NaamWin = win_GetCtlHandle(Win, idc_naame),
	win_SetText(NaamWin, Naam),
	SpSrtWin = win_GetCtlHandle(Win, idc_perslb_spsrt),
	findall(SpSoorten, spsrt(_, SpSoorten, _Sexe, _, _, _, _, _),SoortList),
	lbox_Clear(SpSrtWin),
	lbox_Add(SpSrtWin, SoortList),
	spsrtSel(Sex, SoortList, 0, SoortSelectie),
	lbox_SetSel(SpSrtWin, SoortSelectie, b_True),
	findall(VerhStr, verhl_strl(Verh, VerhStr), VerhinderdList),  % !!!
	VerhWin = win_GetCtlHandle(Win, idc_persoonsgegevens_verhinderd),
	lbox_Clear(VerhWin),
	lbox_Add(VerhWin, VerhinderdList),	% 22.6.2013
	DeelnameWin = win_GetCtlHandle(Win, idc_persoonsgegevens_deelname),
	fillOpgList(Win, SKeus, OpgList),
	lbox_Clear(DeelnameWin),
	lbox_Add(DeelnameWin, OpgList),	% 22.6.2013
	AdresWin = win_GetCtlHandle(Win, idc_persoonsgegevens_adres),
	win_SetText(AdresWin, Adres),
	WoonplWin = win_GetCtlHandle(Win, idc_persoonsgegevens_woonpl),
	win_SetText(WoonplWin, Woonpl),
	KnltbWin = win_GetCtlHandle(Win, idc_persoonsgegevens_knltb),
	win_SetText(KnltbWin, LidNr),
	%
	Sterktes = ["", "1", "2", "3", "4", "5", "6", "7", "8", "9"],
	EnkelWin = win_GetCtlHandle(Win, idc_edit_enkel1),
	lbox_Clear(EnkelWin),
	lbox_Add(EnkelWin, Sterktes),
	sterkteSel(Sterkte, Sterktes, 0, Ne),
	lbox_SetSel(EnkelWin, Ne, b_True),
	DubbelWin = win_GetCtlHandle(Win, idc_persoonsgegevens_dubbele1),
	lbox_Clear(DubbelWin),
	lbox_Add(DubbelWin, Sterktes),
	sterkteSel(SterkD, Sterktes, 0, Nd),
	lbox_SetSel(DubbelWin, Nd, b_True),
	%
	splitGeb(Geb, Gebd, Pasp, _),
	GebWin = win_GetCtlHandle(Win, idc_persoonsgegevens_gebdatum),
	win_SetText(GebWin, Gebd),
	PaspWin = win_GetCtlHandle(Win, idc_pasp),
	win_Check(PaspWin, Pasp),
	club2string(Club, CLUB_DEFAULT),
	ClubWin = win_GetCtlHandle(Win, idc_persoonsgegevens_club),
	lbox_Clear(ClubWin),
	win_SetText(ClubWin, CLUB_DEFAULT),
	findall(_Club2,club(_Club1,_Club2,_), ClubList),
	lbox_Add(ClubWin, ClubList),
	setUitKant(Win, UitKant),
	janee2Bool(Gezien, IGezien),
	janee2Bool(Betaald, IBetaald),
	BetaaldWin = win_GetCtlHandle(Win, idc_persbetaald),
	win_Check(BetaaldWin, IBetaald),
	GezienWin = win_GetCtlHandle(Win, idc_gezien),
	win_Check(GezienWin, IGezien),
	Tel1Win = win_GetCtlHandle(Win, idc_persoonsgegevens_telefoon),
	win_SetText(Tel1Win, Tel),
	Tel2Win = win_GetCtlHandle(Win, idc_persoonsgegevens_telef2),
	win_SetText(Tel2Win, Tel2),
	Tel3Win = win_GetCtlHandle(Win, idc_persoonsgegevens_telef3),
	win_SetText(Tel3Win, Tel3),
	EmailWin = win_GetCtlHandle(Win, idc_email),
	win_SetText(EmailWin, Email),
	RaEWin = win_GetCtlHandle(Win, idc_range),
	win_SetText(RaEWin, RangE),
	RaDWin = win_GetCtlHandle(Win, idc_rangd),
	win_SetText(RaDWin, RangD),
	SpNoWin = win_GetCtlHandle(Win, idct_spno),
	str_int(SKS, SKeus),
	win_setText(SpNoWin, SKS), 
	win_PostEvent(Win,e_User(upd_toolb, 0)),
	!.

loadBankNr(Win, ja, Incasso, Akkoord) :-
  akkoord2checked(Akkoord, BOKChecked),
  BOKWin = win_GetCtlHandle(Win, idc_incasso_akkoord),
  win_Check(BOKWin, BOKChecked),
  win_SetState(BOKWin, [wsf_enabled, wsf_visible]),
  BWin = win_GetCtlHandle(Win, idc_banknr),
  win_SetState(BWin, [wsf_enabled, wsf_visible]),
  %banknr(SKeus, LidNr, BankNummer),
  win_SetText(BWin, Incasso), !.
loadBankNr(Win, _, Incasso, Akkoord) :-
  BWin = win_GetCtlHandle(Win, idc_banknr),
  win_SetState(BWin, [wsf_disabled, wsf_invisible]),
  win_SetText(BWin, Incasso),
  BOKWin = win_GetCtlHandle(Win, idc_incasso_akkoord),
  win_SetState(BOKWin, [wsf_disabled, wsf_invisible]),
  akkoord2checked(Akkoord, BOKChecked),
  win_Check(BOKWin, BOKChecked), !.

checked2Akkoord(b_True, "akkoord") :- !.
checked2Akkoord(_, "") :- !.

akkoord2checked("akkoord", b_True) :- !.
akkoord2checked(_, b_False).

addPaspoort(Geb, b_True, Uit) :-
  not(frontchar(Geb,'P',_)), !,
  concat("P",Geb, Uit).
addPaspoort(Geb, _, Geb).

getsp(Win, SpelerUit) :-
	REWin = win_GetCtlHandle(Win, idct_ratinge),
	RatingE = win_GetText(REWin),
	trim_c(RatingE),
	RDWin = win_GetCtlHandle(Win, idct_ratingd),
	RatingD = win_GetText(RDWin),
	trim_c(RatingD),
	AdresWin = win_GetCtlHandle(Win, idc_persoonsgegevens_adres),
	Adres = win_GetText(AdresWin),
	NaamWin = win_GetCtlHandle(Win, idc_naame),
	NaamN = win_GetText(NaamWin),
	trim_c(NaamN),
	WoonplWin = win_GetCtlHandle(Win, idc_persoonsgegevens_woonpl),
	Woonpl = win_GetText(WoonplWin),
	KnltbWin = win_GetCtlHandle(Win, idc_persoonsgegevens_knltb),
	LidNr = win_GetText(KnltbWin),
	trim_c(LidNr),
	EnkelWin = win_GetCtlHandle(Win, idc_edit_enkel1),
	SterkeX = win_GetText(EnkelWin),
	spstItem2SpSterkte(SterkeX, Sterke),
	DubbelWin = win_GetCtlHandle(Win, idc_persoonsgegevens_dubbele1),
	SterkDX = win_GetText(DubbelWin),
	spstItem2SpSterkte(SterkDX, SterkD),
	getUitKantoor(Win, UitKantN),
	/*UitKWin = win_GetCtlHandle(Win, idc_pers_uitkantoor),
	Uitkantoor = win_GetText(UitKWin),
	safestr_int(UitKantoor, Uur),
	MinWin = win_GetCtlHandle(Win, idc_pers_min),
	Minuten = win_GetText(MinWin),
	safestr_int(Minuten, Min),
	tijd_kwart(0,Uur,Min,UitkantN), */
	GebWin = win_GetCtlHandle(Win, idc_persoonsgegevens_gebdatum),
	Geb = win_GetText(GebWin),
	trim_c(Geb),
	PaspWin = win_GetCtlHandle(Win, idc_pasp),
	Paspoort = win_IsChecked(PaspWin),
	addPaspoort(Geb, PasPoort, GebP),
	T1Win = win_GetCtlHandle(Win, idc_persoonsgegevens_telefoon),
	TelN = win_GetText(T1Win),
	T2Win = win_GetCtlHandle(Win, idc_persoonsgegevens_telef2),
	Tel1 = win_GetText(T2Win),
	T3Win = win_GetCtlHandle(Win, idc_persoonsgegevens_telef3),
	Tel2 = win_GetText(T3Win),
	ClubWin = win_GetCtlHandle(Win, idc_persoonsgegevens_club),
	ClubDef = win_GetText(ClubWin),
	string2club(ClubDef, Club),
	SpsrtWin = win_GetCtlHandle(Win, idc_perslb_spsrt),
	SpSrtIn = win_GetText(SpSrtWin),
	spsrt(SexN, SpSrtIn, _, _, _, _, _, _),
	GezienWin = win_GetCtlHandle(Win, idc_gezien),
	Gezien = win_IsChecked(GezienWin),
	janee2bool(GezienN, Gezien),
	BetaaldWin = win_GetCtlHandle(Win, idc_persbetaald),
	Betaald = win_IsChecked(BetaaldWin),
	janee2bool(BetaaldN, Betaald),
	%gezien2betaald(BetaaldN,Gezien, Betaald),
	EmailWin = win_GetCtlHandle(Win, idc_email),
	EMail = win_GetText(EmailWin), 
	retract(persoon(_, SKeus, _, VerhN, _, _, _,_)), 
	upper_lower(SterkDU, SterkD),
	upper_lower(SterkEU, SterkE),
	%editcontroltext(idc_edit_control,Opm,_,_,_),
        EWin = win_GetCtlHandle(Win, idc_edit_control),
        Opm = edit_GetText(EWin),
        %updateRating(RatingE, RatingD, Misc, MiscN),
	assert(persoon(Win, SKeus, NaamN, VerhN, SexN, SterkE, SterkD, UitKantN)),
	%banknummers(BankNummers),
	%janee2bool(Banknummers, BanknummersB),
	BWin = win_GetCtlHandle(Win, idc_banknr),
	Incasso = win_GetText(BWin),
	trim_c(Incasso),
	BOKWin = win_GetCtlHandle(Win, idc_incasso_akkoord),
	BOK = win_IsChecked(BOKWin),
	checked2Akkoord(BOK, BAkkoord),
	RaEWin = win_GetCtlHandle(Win, idc_range),
	RangE = win_GetText(RaEWin),
	trim_c(RangE),
	RaDWin = win_GetCtlHandle(Win, idc_rangd),
	RangD = win_GetText(RaDWin),
	trim_c(RangD),
        %sp2(SKeus,_,_,_,_,_,_,_,_,_,_,_, _, _, _,_,_,_,_,_,_,_,_,_,_CheckGeg,_Log,_Import),
	speler_extra(SKeus, _CheckGeg, _Log, _Import),
	SpelerUit = sp2(SKeus,SexN,NaamN,TelN,VerhN,RatingE,RatingD,BetaaldN,UitKantN, Club, Adres, Woonpl, LidNr, SterkeU, SterkDU, GebP, Tel1, Tel2, EMail, Opm,GezienN,RangE,RangD,Incasso,_CheckGeg,Bakkoord,_Import), !.
	%saveBankNr(Win, BankNummersB, SKeus, LidNr),

speler_extra(SKeus, CheckGeg, Log, Import) :-
        sp2(SKeus,_,_,_,_,_,_,_,_,_,_,_, _, _, _,_,_,_,_,_,_,_,_,_,CheckGeg,Log,Import), !.
speler_extra(_SKeus, "", "", []).


clubveld(_Win, "") :- !.
clubveld(_Win, Text) :-
  club(Text, _, _), !.
clubveld(_Win, Text) :-
  upper_lower(Text1, Text),
  club(Text1, _, _), !.
clubveld(_Win, Text) :-
  club(_, Text, _), !.
clubveld(_Win, Text) :-
  concat(Text, "     ", Text1),
  frontstr(5, Text1, Text2, _),
  trim_c(Text2),
  %upper_lower(Text3, Text2),
  nieuwe_club(_Win, Text2).

teken('/').
teken('-').
teken('.').

scheidingsteken(Gebdat) :-
  teken(Scheid),
  searchchar(Gebdat,Scheid,FoundPos),
  FoundPos > 0, !.

/*
persoonsItems(_, _, _, separator).
persoonsItems(Tel1, _, _, txt(idr_opbellen1,Tel1,0,b_True,0,[])):-
  Tel1 <> "".
persoonsItems(_ ,Tel2, _, txt(idr_opbellen2,Tel2,0,b_True,0,[])):-
  Tel2 <> "".
persoonsItems(_ , _, Tel3, txt(idr_opbellen3,Tel3,0,b_True,0,[])):-
  Tel3 <> "".
persoonsItems(_, _, _, separator).
*/
gelijk(sp2(_,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)) :-
  not(fronttoken(Naam, _, _)), !.	% doe dan ook niets
gelijk(sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, _ /*Club*/, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import)) :-
  sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, _ /* Club*/, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import), !.
gelijk(Sp1):- 
  comline(LineBuffer),
  upper_lower(LineBuffer, LineBufferL),
  searchstring(LineBufferL,"/x",_),
  Sp1 =  sp2(SKeus,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),	% bestaat ie wel
  sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
  Sp2 = sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
  write("\n1:", Sp1),
  write("\n2:", Sp2),
  fail.
gelijk(Sp1):- 
  comline(LineBuffer),
  upper_lower(LineBuffer, LineBufferL),
  searchstring(LineBufferL,"/x",_),
  Sp1 = sp2(SKeus,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), 
  not(sp2(SKeus,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)),	% bestaat ie niet
  write("\n1:", Sp1),
  write("\n2: bestond nog niet"),
  fail.

update_sp(Speler, Speler) :- !.
update_sp(_, sp2(_,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)) :-
  fronttoken(Naam, _, N1),
  fronttoken(N1, _, _),		% minstens twee tokens
  not(scheidingstekenN(Naam)),
  dlg_MessageBox( "Naam", "Schrijf de naam in telefoonboekformaat:\nAchternaam, Voornaam/voorletters voorvoegsel\nVoorbeeld: Pokkevak, K. van\nhet voorvoegsel in kleine letters", 
             mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.
update_sp(_, sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import)) :-
  sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
   !.
update_sp(_, sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import)) :-
  logf('A', sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),nee),
  dlg_opgaven_refresh(1), !.


enableWaarsch(0, b_False) :- !.
enableWaarsch(_, b_True) :- !.

fillOpgList(Win, SKeus, OnderdelenUI) :-
  sp2(SKeus,_Sex,_Naam,_Tel,_Verh,_RatingE,_RatingD,_Betaald,_UitKant, _Club, _Adres, _Woonpl, _LidNr, _Sterkte, _SterkD, _Geb, _Tel2, _Tel3, _EMail, _Opm,_Gezien,
       _RangE,_RangD,_Incasso,_CheckGeg,_Akkoord,Imports),
  findall(Onderdeel, sp_opgaven(SKeus,Onderdeel), Onderdelen),
  sortstring_c(Onderdelen,2),
  wd_planned(SKeus,WedstrijdList,alleplanning,b_True),
  speeltijden1(WedstrijdList,Speeltdn),
  qsortwt(Speeltdn,SpeeltdnS),
  prt_speelt(SpeeltdnS,0,_,ja,1,1,Onderdelen,OnderdelenU,0,X),
  append(OnderdelenU, Imports, OnderdelenUI),	       % 20.12.2009  voeg de imports toe
  enableWaarsch(X, WEnable),
  toolbar_SetValue(Win, id_Speler_Oproepen_lijst, ctrl_value(Wenable,1)), !.
fillOpgList(_Win, _SKeus, []).

/*startGezienTimer(_Win, omheteven) :-
  formGezien(b_True, Tijd),
  Tijd1 = Tijd * 1000,
  TimerId = timer_Set(_Win, Tijd1),	% seconden
  assert(timerid1(TimerId)), !.
startGezienTimer(_Win, _).*/

tekenN(',').
tekenN(';').

scheidingstekenN(Naam) :-
  tekenN(Teken),
  searchchar(Naam,Teken,_FoundPos), !.

%BEGIN_DLG Persoonsgegevens
/**************************************************************************
	Creation and event handling for dialog: Persoonsgegevens
**************************************************************************/

constants

%BEGIN Persoonsgegevens, CreateParms, 14:01:48-6.10.2015, Code automatically updated!
  dlg_persoonsgegevens_ResID = idd_persoonsgegevens
  dlg_persoonsgegevens_DlgType = wd_Modeless
  dlg_persoonsgegevens_Help = idh_persoonsgegevens
%END Persoonsgegevens, CreateParms

predicates

  dlg_persoonsgegevens_eh : EHANDLER
  scrollhandler_eh : EHANDLER
  save_speler1(WINDOW)
  matchCatShort(string Elem, wedscat Cat)
procedure stripU(string NaamKort, string NaamKortS)
procedure spelerProfiel(string, boolean)
determ knltbnummerNietUniek(integer SKeus, string LidNr, string NaamVanDeAnder)
determ partner(integer SKeus, wedscat Cat, integer Partner)

clauses

scrollhandler_eh(_Win,e_Keydown('P',c_Control),0):-
	ParentWin = win_GetParent(_Win),
	win_SendEvent(ParentWin,e_Menu(id_Onderdeel_print,0)),
  !.
scrollhandler_eh(_Win,e_Keydown(k_next,c_Nothing),0):-
	ParentWin = win_GetParent(_Win),
	win_SendEvent(ParentWin,e_Menu(idt_neer,0)),
  !.
scrollhandler_eh(_Win,e_Vscroll(sc_Linedown,_),0):-
	ParentWin = win_GetParent(_Win),
	win_SendEvent(ParentWin,e_Menu(idt_neer,0)),
  !.
scrollhandler_eh(_Win,e_Keydown(k_prev,c_Nothing),0):-
	ParentWin = win_GetParent(_Win),
	win_SendEvent(ParentWin,e_Menu(idt_op,0)),
  !.
scrollhandler_eh(_Win,e_Vscroll(sc_LineUp,_),0):-
	ParentWin = win_GetParent(_Win),
	win_SendEvent(ParentWin,e_Menu(idt_op,0)),
  !.

upd_formulier() :-	% zit in bij update_display
  persoon(_Win, _PersNo, _, _, _, _, _, _),
  win_PostEvent(_Win, e_User(upd_opglist, 0)), !.
upd_formulier().

/*
bankNrFout(BankNr, ResStr) :-
  frontchar(BankNr, Char, _),
  Char <= '9',
  Char >= '1',
  reversestr(BankNr, "", Rev),
  not(proef11a(Rev, 1, 0)),
  ResStr = vpi_GetResStr(st_ongeldig_baknummer), !.
bankNrFout(BankNr, ResStr) :-
  frontchar(BankNr, Char, _),
  Char > '9',
  not(Char = 'P'),
  not(Char = 'G'),
  ResStr = vpi_GetResStr(st_ongeldig_baknummer), !.
bankNrFout(BankNr, ResStr) :-
  frontchar(BankNr, Char, _),
  Char < '1',
  not(Char = 'P'),
  not(Char = 'G'), 
  ResStr = vpi_GetResStr(st_ongeldig_baknummer), !.
*/

knltbnummerNietUniek(SKeus, LidNr, NaamVanDeAnder) :-
  sp2(PersNoX,_,NaamVanDeAnder,_,_,_,_,_,_,_,_,_,LidNr,_,_,_,_,_,_,_,_,_,_,_,_,_,_),
  not(PersnoX = SKeus), !.

/*preservePG("P", Rest, "P", Rest) :- !.
preservePG("G", Rest, "P", Rest) :- !.
preservePG(X, Rest, "", Uit) :-
  concat(X, Rest, Uit). */

/*save_speler1(Win) :-	% 20.10.2003
  banknummers(ja),
  BWin = win_GetCtlHandle(Win, idc_banknr),
  BankNr = win_GetText(BWin),
  NaamWin = win_GetCtlHandle(Win, idc_naame),
  NaamS = win_GetText(NaamWin),
  trim_c(NaamS),
  not(NaamS = ""),
  trim_c(BankNr),
  fronttoken(BankNr, _, _),
  %frontstr(1, BankNr,FC,RestString),
  %preservePG(FC, Reststring, PG, BankNr1),
  %incassoNr(NaamS, BankNr1, BankNr2),
  %concat(PG, BankNr2, BankNr3),
  bankNrFout(BankNr, Msg),
  dlg_MessageBox( "Incassonummer", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  win_SetFocus(BWin), !.*/
save_speler1(_Win) :-
  persoon(_Win, PersNo, _, _, _, _, _, _),
  NaamWin = win_GetCtlHandle(_Win, idc_naame),
  NaamS = win_GetText(NaamWin),
  trim_c(NaamS),
  not(NaamS = ""),
  sp2(PersNoX,_,NaamS,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), 
  PersnoX <> PersNo,		% er bestaat er een met ander nummer
  _A = dlg_MessageBox( "Speler", "Naam niet uniek.", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  win_SetFocus(NaamWin), !.
save_speler1(_Win) :-
  SrtWin = win_GetCtlHandle(_Win, idc_perslb_spsrt),
  SrtSelected = win_GetText(SrtWin),
  StWin = win_GetCtlHandle(_Win, idc_edit_enkel1),
  Sterkte = win_GetText(StWin),
  trim_c(Sterkte),
  trim_c(SrtSelected),
  spsrt(_SexN, Srt, _, _, _, S1, S2, _),
  trim_c(Srt),
  Srt = SrtSelected,
  %speelsterkteOudNieuw(Sterkte, SterkteN),
  %speelsterkteOudNieuw(S1, S1N),
  %speelsterkteOudNieuw(S2, S2N),
  not(check_sterk(S1, S2, Sterkte)),
  format(Msg, "Speelsterkte niet tussen %s en %s\nvan deze spelersoort.", S1, S2),
  2 = dlg_MessageBox( "Speler", Msg, mesbox_iconexclamation, mesbox_buttonsokcancel, mesbox_defaultsecond, mesbox_suspendapplication ),
  win_SetFocus(StWin),
  !.
save_speler1(_Win) :-
  openbaar(ja),
  KnltbWin = win_GetCtlHandle(_Win, idc_persoonsgegevens_knltb),
  LidNr = win_GetText(KnltbWin),
  trim_c(LidNr),
  LidNr = "",
  getbacktrack(Here),
  persoon(_Win, SKeus, _, _, _, _, _, _),
  sp2opg(SKeus,_,_,_,_,_),
  cutbacktrack(Here),
  1 = dlg_MessageBox( "Speler", "Knltbnummer ontbreekt.\nInvullen?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
  win_SetFocus(KnltbWin),
  !.  
save_speler1(_Win) :-
  openbaar(ja),
  KnltbWin = win_GetCtlHandle(_Win, idc_persoonsgegevens_knltb),
  LidNr = win_GetText(KnltbWin),
  trim_c(LidNr),
  not(LidNr = ""),
  persoon(_Win, SKeus, _, _, _, _, _, _),
  knltbnummerNietUniek(SKeus, LidNr, NaamVanDeAnder),
  format(Msg, "Bondsnummer niet uniek!\n%s heeft hetzelfde nummer!\nNB Dat kan een synoniem zijn.", NaamVanDeAnder),
  1 = dlg_MessageBox( "Speler", Msg, mesbox_iconerror, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  win_SetFocus(KnltbWin),
  !.  
save_speler1(_Win) :-
  persoon(_Win, SKeus, _, _, _, _, _, _),
  sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
  SpelerIn = sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
  getsp(_Win, SpelerN),
  update_sp(SpelerIn, SpelerN),
  SpelerN = sp2(_,_,NaamN,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  update_betalingen(),
  update_waarsch(),
  not(NaamN = Naam),
  spelerselRefresh(),	% naam veranderd
  update_display(),
  fail.
save_speler1(_Win) :-
  persoon(_Win, SKeus, _, _, _, _, _, _),
  not(sp2(SKeus,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)), 	% dus nieuwe speler
  getsp(_Win, SpelerN),
  logf('A', SpelerN, nee),
  spelerselRefresh(),	% lijst veranderd
  fail.
save_speler1(_Win) :-
  persoon(_Win, SKeus, _, _, _, _, _, _),
  not(sp2(SKeus,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)),		% wis eventuele opgaven van niet bestaande speler
  sp2opg(SKeus,Cat,Opg,_,_,_),
  opg(Opg,Cat,Tgnst,Plts,Rnd,Opgesteld,Tijd,LL,TeamId,Keur,Melding),
  logf('R',opg(Opg,Cat,Tgnst,Plts,Rnd,Opgesteld,Tijd,LL,TeamId,Keur,Melding),nee),
  fail.


matchCatShort(Elem, Cat) :-
  stripU(Elem, Elem1),
  fronttoken(Elem1, CatShort, _),
  wc(Cat, _, _, _, _, CatShort, _, _, _, _, _, _, _, _, _), !.
  %wc(Cat,_,_,CatShort,_,_,_), !.
matchCatShort(Elem, Cat) :-
  stripU(Elem, Elem1),
  searchchar(Elem1,'\t',FoundPos),
  Len = FoundPos - 1,
  frontstr(Len,Elem1,CatShort,_),
  %trim_c(CatShort),
  wc(Cat, _, _, _, _, CatShort, _, _, _, _, _, _, _, _, _), !.
  %wc(Cat,_,_,CatShort,_,_,_), !.


spelerProfiel(KNLTB, b_True) :-
  proef11det(KNLTB), !.
spelerProfiel(_, b_False).

partner(SKeus, Cat, Partner) :-
  opg(_Opg,Cat,p(SKeus, Partner),_,_,_,_,_,_,_,_), !.
partner(SKeus, Cat, Partner) :-
  opg(_Opg,Cat,p(Partner, SKeus),_,_,_,_,_,_,_,_), !.

addNote(SKeus, EofS) :-
  persoon(Win, SKeus, _, _, _, _, _, _),
  EditWin = win_GetCtlHandle(Win, idc_edit_control), 
  date(_Year,Month,Day),
  time(Hours,Minutes,_Seconds,_Hundredths),
  helestr_int(0, MinStr, Minutes),
  format(Msg, "%s op %U/%U %U.%s.", EofS, Day, Month,Hours,MinStr),
  trap(Text2 = edit_GetText(EditWin),_, true),
  extraSpatie(Msg, Text2, Tekst1),
  edit_AppendStr(EditWin, Tekst1), !.  

mess([], "Naam en gegevens verwijderen?") :- !.
mess(_, "Er zijn inschrijvingen die hiernaar verwijzen.\nAls je deze persoon verwijdert\nbestaat de kans dat hij opnieuw ingevoerd wordt!\nToch verwijderen?"). 

  

 dlg_persoonsgegevens_Create(_Parent, sp2(SKeus,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)):-
	persoon(Win, SKeus, _, _, _, _, _, _), !,	% zelfde speler
	trap(win_SetFocus(Win),_,true).
 dlg_persoonsgegevens_Create(_Parent, sp2(Sp,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)):-
	persoon(_Win, Sp1, _, _, _, _, _, _),
	Sp <> Sp1,				% andere speler
	getsp(_Win, SpelerGeg),
	not(gelijk(SpelerGeg)),
	1 = dlg_MessageBox( "Veranderingen", "De gegevens zijn veranderd.\nBewaren?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
	save_speler1(_Win), !.
 dlg_persoonsgegevens_Create(_Parent, sp2(Sp,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)):-
	persoon(_Win, SKeus, _, _, _, _, _, _),
	Sp <> SKeus,
	not(sp2(SKeus,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)),		% wis eventuele opgaven van niet bestaande speler
	sp2opg(SKeus,Cat,Opg,_,_,_),
	opg(Opg,Cat,Tgnst,Plts,Rnd,Opgesteld,Tijd,LL,TeamId,Keur,Melding),
	logf('R',opg(Opg,Cat,Tgnst,Plts,Rnd,Opgesteld,Tijd,LL,TeamId,Keur,Melding),nee),
	fail.
 dlg_persoonsgegevens_Create(_Parent, sp2(No,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import)):-
	persoon(_Win, SKeus, _, _, _, _, _, _),
	No <> SKeus,
	loglock(lees, "dlg_persoonsgegevens0"),
	logclose(),
	SpelerIn = sp2(No,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
%  	SpelerIn = speler(No, Sex, Naam, Tel, Verh, Misc, Betaald, Uitkant,
%		    Club, Adr, Wnpl, Knltb, StS, StD, Geb, Tel1, Tel2, EMail,Opm,BankNr),
	assert(gegevensS(SpelerIn)),
	retractall(persoon(_Win, _, _, _, _, _, _, _)),
        retractall(editcontroltext(idc_edit_control,_,_,_,_,_,_)),
	assert(persoon(_Win, No, Naam, Verh, Sex, ES, DS, UitKant)),
	assert(editcontroltext(idc_edit_control,Opm,b_True,ff_Helvetica,b_False,9,1)),
	EWin = win_GetCtlHandle(_Win, idc_edit_control),
	edit_pastestr(EWin, Opm),
        setsp(_Win, SpelerIn), !.
  dlg_persoonsgegevens_Create(Parent, SpelerIn):-
	loglock(lees, "dlg_persoonsgegevens"),
	logclose(),
	assert(gegevensS(SpelerIn)),
	NulWin = cast(WINDOW, 0),
        SpelerIn = sp2(SKeus,Sex,Naam,_,Verh,RatingE, RatingD,_,UitKant, _, _,_,_,_,_,_,_,_,_,Opm,_,_,_,_,_,_,_),
  	%SpelerIn = speler(SKeus, Sex, NAAM, _Tel, Verh, Misc, _Betaald, Uitkant,
	%	    _Club,_Adres,_Woonpl,_LidNr, _Sterkte,_SterkD,_Geb,_Tel2,_Tel3,_IDC_EMAIL,Opm,_),
	assert(persoon(NulWin, SKeus, NAAM, Verh, Sex, RatingE, RatingD, UitKant)),
	assert(editcontroltext(idc_edit_control,Opm,b_True,ff_Helvetica,b_False,9,1)),
	win_CreateResDialog(Parent,dlg_persoonsgegevens_DlgType,dlg_persoonsgegevens_ResID,dlg_persoonsgegevens_eh,0).

%BEGIN Persoonsgegevens, idc_ok _CtlInfo
  dlg_persoonsgegevens_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	NaamWin = win_GetCtlHandle(_Win, idc_naame),
	NaamS = win_GetText(NaamWin),
	trim_c(NaamS),
	not(NaamS = ""),
	save_speler1(_Win), !.
  dlg_persoonsgegevens_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	win_Destroy(_Win),
	!.
%END Persoonsgegevens, idc_ok _CtlInfo
%MARK Persoonsgegevens, new events

%BEGIN Persoonsgegevens, idc_persbetaald _CtlInfo
  dlg_persoonsgegevens_eh(_Win,e_Control(idc_persbetaald,_CtrlType,CtrlWin,_CtlInfo),0):-
	%bevestigen(),
	b_True = win_IsChecked(CtrlWin),
	persoon(_Win, SpNo, _, _, _, _, _, _),
	contributie(SpNo, Bedrag, String, []),
	format(Msg, "%s\ntotaal %7.2f euro\nBetaald?", String, Bedrag),
	1 = dlg_MessageBox( "Betaling", Msg, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
	!.
/*  dlg_persoonsgegevens_eh(_Win,e_Control(idc_persbetaald,_CtrlType,CtrlWin,_CtlInfo),0):-
	%bevestigen(),
	b_True = win_IsChecked(CtrlWin),
	persoon(_Win, SpNo, _, _, _, _, _, _),
	not(contributie(SpNo, _, _, [])),
	%format(Msg, "%s\nBedrag nog onbekend\nBetaald?", String, Bedrag),
	1 = dlg_MessageBox( "Betaling", "Bedrag nog onbekend\nBetaald?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
	!.*/
  dlg_persoonsgegevens_eh(_Win,e_Control(idc_persbetaald,_CtrlType,CtrlWin,_CtlInfo),0):-
	%bevestigen(),
	b_True = win_IsChecked(CtrlWin),
	win_Check(CtrlWin, b_False), !.
%END Persoonsgegevens, idc_persbetaald _CtlInfo

%BEGIN Persoonsgegevens, idc_pers_min selchanged
  dlg_persoonsgegevens_eh(_Win,e_Control(idc_pers_min,_CtrlType,_CtrlWin,selchanged),0):-!,
	getUitKantoor(_Win, UitKantN),
	retract(persoon(_Win, Persno, Naam, Verhl, Sex, ES, DS, _UitKant)),
	assert(persoon(_Win, Persno, Naam, Verhl, Sex, ES, DS, UitKantN)),
	!.
%END Persoonsgegevens, idc_pers_min selchanged

%BEGIN Persoonsgegevens, idc_pers_uitkantoor selchanged
  dlg_persoonsgegevens_eh(_Win,e_Control(idc_pers_uitkantoor,_CtrlType,_CtrlWin,selchanged),0):-!,
	getUitKantoor(_Win, UitKantN),
	retract(persoon(_Win, Persno, Naam, Verhl, Sex, ES, DS, _UitKant)),
	assert(persoon(_Win, Persno, Naam, Verhl, Sex, ES, DS, UitKantN)),
	!.
%END Persoonsgegevens, idc_pers_uitkantoor selchanged

%BEGIN Persoonsgegevens, idc_persoonsgegevens_knltb modified
  dlg_persoonsgegevens_eh(_Win,e_Control(idc_persoonsgegevens_knltb,_CtrlType,_CtrlWin,modified),0):-!,
	win_PostEvent(_Win,e_User(upd_toolb, 0)),
	!.
%END Persoonsgegevens, idc_persoonsgegevens_knltb modified

%BEGIN Persoonsgegevens, idc_persoonsgegevens_deelname activated
  dlg_persoonsgegevens_eh(_Win,e_Control(idc_persoonsgegevens_deelname,_CtrlType,_CtrlWin,activated),0):-
	persoon(_Win, SKeus, _Naam, _VerhlN, _Sex, _, _, _),
	WWin = win_GetCtlHandle(_Win, idc_persoonsgegevens_deelname),
	lbox_GetSel(WWin,ItemList,_IndexList),
	ItemList = [Elem|_],
	matchCatShort(Elem, Cat),
	wc(Cat, _, _, _, WT, _, _, _, _, _, _, _, _, _, _),
	%wc(Cat,_,WT,_,_,_,_),
	not(WT = e),
	searchstring(Elem,"met ",_FoundPos),
	partner(SKeus, Cat, Partner), !,
        sp2(Partner,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
        SpelerIn = sp2(Partner,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
	ParentWin = win_GetParent(_Win),
	dlg_persoonsgegevens_Create(ParentWin, SpelerIn ),
	!.
  dlg_persoonsgegevens_eh(_Win,e_Control(idc_persoonsgegevens_deelname,_CtrlType,_CtrlWin,activated),0):-
	persoon(_Win, SKeus, _Naam, _VerhlN, _Sex, _, _, _),
	WWin = win_GetCtlHandle(_Win, idc_persoonsgegevens_deelname),
	lbox_GetSel(WWin,ItemList,_IndexList),
	ItemList = [Elem|_],
        sp2(SKeus,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,Imports),
	member(Elem, Imports), !,
	getInschrijf(Imports),
	fail.
  dlg_persoonsgegevens_eh(_Win,e_Control(idc_persoonsgegevens_deelname,_CtrlType,_CtrlWin,activated),0):-
	persoon(_Win, SKeus, _Naam, _VerhlN, _Sex, _, _, _),
	WWin = win_GetCtlHandle(_Win, idc_persoonsgegevens_deelname),
	lbox_GetSel(WWin,ItemList,_IndexList),
	ItemList = [Elem|_],
	matchCatShort(Elem, Cat),
	zoekOp1(SKeus, Cat),
	trap(CW = win_GetCtlHandle(_Win, idc_cancel),_,fail),
	win_SendEvent(_Win,e_Control(idc_cancel,wc_PushButton,CW, activated())),
	!.
  dlg_persoonsgegevens_eh(_Win,e_Control(idc_persoonsgegevens_deelname,_CtrlType,_CtrlWin,activated),0):-
	persoon(_Win, SKeus, Naam, _VerhlN, _Sex, _, _, _),
	zoekOp(SKeus, Naam),
	trap(CW = win_GetCtlHandle(_Win, idc_cancel),_,fail),
	win_SendEvent(_Win,e_Control(idc_cancel,wc_PushButton,CW, activated())),
	!.
	
%END Persoonsgegevens, idc_persoonsgegevens_deelname activated

%BEGIN Persoonsgegevens, idc_email losefocus
  dlg_persoonsgegevens_eh(_Win,e_Control(idc_email,_CtrlType,_CtrlWin,losefocus),0):-
	Str = win_GetText(_CtrlWin),
	emailproef(Str),
	win_PostEvent(_Win,e_User(upd_toolb, 0)),
	!.
  dlg_persoonsgegevens_eh(_Win,e_Control(idc_email,_CtrlType,_CtrlWin,losefocus),0):-!,
	dlg_MessageBox( "Emailadres", "Ongeldig emailadres!", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	!.
%END Persoonsgegevens, idc_email losefocus



%BEGIN Persoonsgegevens, e_Destroy
  dlg_persoonsgegevens_eh(_Win,e_Destroy,0):-
	updateVensterpositie(spelervenster, _Win),
        retractall(editcontroltext(idc_edit_control,_,_,_,_,_,_)),
	retract(persoon(_Win, _SKeus, _NaamN, _, _, _, _, _)),
	!.
%END Persoonsgegevens, e_Destroy

%BEGIN Persoonsgegevens, idc_persoonsgegevens_knltb losefocus
  dlg_persoonsgegevens_eh(_Win,e_Control(idc_persoonsgegevens_knltb,_CtrlType,_CtrlWin,losefocus),0):-
	openbaar(ja),
	Str = win_GetText(_CtrlWin),
	trim_c(Str),
	proef11(Str),
	!.
  dlg_persoonsgegevens_eh(_Win,e_Control(idc_persoonsgegevens_knltb,_CtrlType,_CtrlWin,losefocus),0):- !.
%END Persoonsgegevens, idc_persoonsgegevens_knltb losefocus

%BEGIN Persoonsgegevens, e_User
  dlg_persoonsgegevens_eh(_Win,e_User(upd_verhinderingen,_Ptr),0):-!,
	persoon(_Win, _PersNo, _NaamN, VerhlN, _Sex, _, _, UitKantN),
	LboxWin = win_GetCtlHandle(_Win, idc_persoonsgegevens_verhinderd),
	lbox_Clear(LboxWin),
	findall(VerhStr, verhl_strl(VerhlN, VerhStr), SList),
	lbox_Add(LboxWin, -1, SList),
	setUitKant(_Win, UitKantN),
	!.
  dlg_persoonsgegevens_eh(_Win,e_User(upd_opglist,_Ptr),0):-
	persoon(_Win, PersNo, _Naam, _VerhlN, _Sex, _, _, _),
	OpgegWin = win_GetCtlHandle(_Win, idc_persoonsgegevens_deelname),
	OpgList0 = lbox_GetAll(OpgegWin),
	fillOpgList(_Win, PersNo, OpgegList),
	not(OpgList0 = OpgegList),
	lbox_Clear(OpgegWin),
	lbox_Add(OpgegWin, OpgegList),
	!.
  dlg_persoonsgegevens_eh(_Win,e_User(upd_toolb,_Ptr),0):-
	toolbar_SetValue(_Win, id_Speler_Oproepen_lijst, ctrl_value(b_False,1)),
	persoon(_Win, SKeus, _Naam, _VerhlN, _Sex, _, _, _),
	getbacktrack(Here),
	wd_planned(SKeus,WedstrijdList,alleplanning,b_True),
	member(Wed, WedstrijdList),
	Wed = w(_Num,_Cat,_Tijd,Gewaarsch,_LRInWeds,_T,_Verv,_BaanPark),
	Gewaarsch = nee,
	cutbacktrack(Here),
	toolbar_SetValue(_Win, id_Speler_Oproepen_lijst, ctrl_value(b_True,1)),
	fail.
  dlg_persoonsgegevens_eh(_Win,e_User(upd_toolb,_Ptr),0):-
	toolbar_SetValue(_Win, id_Email1, ctrl_value(b_False,1)),
	EWin = win_GetCtlHandle(_Win, idc_email),
	T = win_GetText(EWin),
	fronttoken(T, _, _),
	toolbar_SetValue(_Win, id_Email1, ctrl_value(b_True,1)),
	fail.
  dlg_persoonsgegevens_eh(_Win,e_User(upd_toolb,_Ptr),0):-
	toolbar_SetValue(_Win, idb_sms, ctrl_value(b_False,1)),
	persoon(_Win, Nr, _, _, _, _, _, _),
	sp2(Nr,_Sex,_Naam,Telthuis,_Verh,_RatingE,_RatingD,_Betaald,_UitKant, _Club, _Adres, _Woonpl, _BondsNummer, _ES, _DS, _Gebdatum, Telwerk, Telmobiel, _EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Import),
	 _MobielNr = selectMobielNr(Telthuis, Telwerk, TelMobiel, smsbulk),
	toolbar_SetValue(_Win, idb_sms, ctrl_value(b_True,1)),
	fail.
  dlg_persoonsgegevens_eh(_Win,e_User(upd_toolb,_Ptr),0):-
	formulierVolgend(_, _, Volgend),
	toolbar_SetValue(_Win, idt_neer, ctrl_value(Volgend, 1)),
	formulierVorig(_, _, Vorig),
	toolbar_SetValue(_Win, idt_op, ctrl_value(Vorig, 1)),
	enableWis(_Win, WisKan),
	toolbar_SetValue(_Win, idb_Wis, ctrl_value(WisKan,1)),
	KWin = win_GetCtlHandle(_Win, idc_persoonsgegevens_knltb),
	KNLTB = win_GetText(KWin),
	spelerProfiel(KNLTB, ProfielGeldig),
	toolbar_SetValue(_Win, id_Speler_checken, ctrl_value(ProfielGeldig,1)),
	toolbar_SetValue(_Win, id_Speler_wedstrijden, ctrl_value(b_False,1)),
	persoon(_Win, SpNo, _Naam, _VerhlN, _Sex, _, _, _),
	sp2opg(SpNo, _, _, _, _, _),
	wedstrijd(_,_,SpNo,_,_,_),
	toolbar_SetValue(_Win, id_Speler_wedstrijden, ctrl_value(b_True,1)),
	!.
%END Persoonsgegevens, e_User
  dlg_persoonsgegevens_eh(_Win,e_Menu(id_Speler_checken,_CAS),0):-
	KWin = win_GetCtlHandle(_Win, idc_persoonsgegevens_knltb),
	KNLTB = win_GetText(KWin),
	providerX(Prov, _, "", _),
	format(Intermed, "http://%s/profielKNLTB.php?bnr=%s", Prov, KNLTB),
	Null = cast(api_hwnd,0),
  	api_ShellExecute(Null,"open",Intermed,"","",1),		% open schema direct in browser
	!.
  dlg_persoonsgegevens_eh(_Win,e_Menu(id_Onderdeel_print,_CAS),0):-
        getsp(_Win, Speler),
	persoon(_Win, PersNo, _Naam, _VerhlN, _Sex, _, _, _),
	spelerdata(PersNo, Speler, Data),
	TaskWin = vpi_getTaskwin(),
	get_TempDir(_, TempFile),
	file_str(TempFile, Data),
	dlg_rapport_Create(TaskWin, "Persoonsformulier", "Formulier", b_False), !.
  dlg_persoonsgegevens_eh(_Win,e_Menu(id_Email1,_CAS),0):-
        getsp(_Win, Speler),
	persoon(_Win, PersNo, Naam, _VerhlN, _Sex, _, _, _),
	Speler = sp2(PersNo,_,_NaamN,_,_ ,_,_,_,_,_,_, _,Bondsnummer,_,_,_, _,_,EMail,_,_,_,_,_,_,_,_),
	%spelerdata(PersNo, Speler, Data),
	%naam(Toer, _, _,_,_,_),
	emailuitvoerEnkel(_Win, Persno, Naam, Email, Bondsnummer),
        !.
  dlg_persoonsgegevens_eh(_Win,e_Menu(id_Speler_Oproepen_lijst,_CAS),0):-
	1 <> dlg_MessageBox( "Gewaarschuwd", "Speler als gewaarschuwd boeken?\nSpeler moet ZELF eventuele partner(s) informeren!", mesbox_iconquestion, mesbox_buttonsokcancel, mesbox_defaultsecond, mesbox_suspendapplication ),
	!.
  dlg_persoonsgegevens_eh(_Win,e_Menu(id_Speler_Oproepen_lijst,_CAS),0):-
	persoon(_Win, SKeus, _Naam, _VerhlN, _Sex, _, _, _),
	wd_planned(SKeus,WedstrijdList,alleplanning,b_True),
	member(Wed, WedstrijdList),
	Wed = w(Num,Cat,Tijd,Gewaarsch,LRInWeds,_T,_Verv,_BaanPark),
	Gewaarsch = nee,
	wd(Num,Cat,T1,T2,_,_,_),
	getbacktrack(Here),
	w2(Num,Cat,Tijd,G1,G2,Fix,Tijd2,Bn), 
	cutbacktrack(Here),
	handleLRGew(G1, G2, Gew1, Gew2,T1,T2,Num,Cat,LRInWeds,_),
	logf('A',w2(Num,Cat,Tijd,Gew1,Gew2,Fix,Tijd2,Bn),ja),
	fail.
  dlg_persoonsgegevens_eh(_Win,e_Menu(id_Speler_Oproepen_lijst,_CAS),0):-
	logclose(),
	persoon(_Win, PersNo, _Naam, _VerhlN, _Sex, _, _, _),
	OpgegWin = win_GetCtlHandle(_Win, idc_persoonsgegevens_deelname),
	lbox_Clear(OpgegWin),
	fillOpgList(_Win, PersNo, OpgList),
	lbox_Add(OpgegWin, OpgList),
	update_waarsch(),
	win_PostEvent(_Win,e_User(upd_toolb, 0)),
	!.
  dlg_persoonsgegevens_eh(_Win,e_Menu(id_Speler_wedstrijden,_CAS),0):-
	persoon(_Win, SKeus, _Naam, _VerhlN, _Sex, _, _, _),
	WWin = win_GetCtlHandle(_Win, idc_persoonsgegevens_deelname),
	lbox_GetSel(WWin,ItemList,_IndexList),
	ItemList = [Elem|_],
	matchCatShort(Elem, Cat),
	zoekOp1(SKeus, Cat),
	trap(CW = win_GetCtlHandle(_Win, idc_cancel),_,fail),
	win_SendEvent(_Win,e_Control(idc_cancel,wc_PushButton,CW, activated())),
	!.
  dlg_persoonsgegevens_eh(_Win,e_Menu(id_Speler_wedstrijden,_CAS),0):-!,
	persoon(_Win, SKeus, Naam, _VerhlN, _Sex, _, _, _),
	zoekOp(SKeus, Naam),
	trap(CW = win_GetCtlHandle(_Win, idc_cancel),_,fail),
	win_SendEvent(_Win,e_Control(idc_cancel,wc_PushButton,CW, activated())),
	!.
/*  dlg_persoonsgegevens_eh(_Win,e_Menu(idts_help,_CAS),0):-
	project_ShowHelpContext("Persoonsgegevens"),
	!. */
  dlg_persoonsgegevens_eh(_Win,e_Menu(idt_downloadm,_CAS),0):-
	persoon(_Win, SKeus, _Naam, _VerhlN, _Sex, _, _, _),
  	getSpelerOproepen(SKeus), !.
 dlg_persoonsgegevens_eh(_Win,e_Menu(idb_sms,_CAS),0):-
	persoon(_Win, Persoon, _, _, _, _, _, _),
        stuurSMS(_Win, Persoon), !.
 dlg_persoonsgegevens_eh(_Win,e_Menu(idt_op,_CAS),0):-
	getsp(_Win, Speler),
	not(gelijk(Speler)),
	1 = dlg_MessageBox( "Veranderingen", "De gegevens zijn veranderd.\nBewaren?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
	save_speler1(_Win), !.
 dlg_persoonsgegevens_eh(_Win,e_Menu(idt_op,_CAS),0):-
	persoon(_Win, SKeus, _, _, _, _, _, _),
	not(sp2(SKeus,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)),		% wis eventuele opgaven van niet bestaande speler
	sp2opg(SKeus,Cat,Opg,_,_,_),
	opg(Opg,Cat,Tgnst,Plts,Rnd,Opgesteld,Tijd,LL,TeamId,Keur,Melding),
	logf('R',opg(Opg,Cat,Tgnst,Plts,Rnd,Opgesteld,Tijd,LL,TeamId,Keur,Melding),nee),
	fail.
 dlg_persoonsgegevens_eh(_Win,e_Menu(idt_op,_CAS),0):-
	formulierVorig(No, Index, _),
	formulierSelect(Index),
	loglock(lees, "spelerop"),
	logclose(),
	sp2(No,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
	SpelerIn = sp2(No,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
	assert(gegevensS(SpelerIn)),
	retractall(persoon(_Win, _, _, _, _,_, _, _)),
        retractall(editcontroltext(idc_edit_control,_,_,_,_,_,_)),
	assert(persoon(_Win, No, Naam, Verh, Sex,ES, DS, UitKant)),
	assert(editcontroltext(idc_edit_control,Opm,b_True,ff_Helvetica,b_False,9,1)),
	EWin = win_GetCtlHandle(_Win, idc_edit_control),
	edit_pastestr(EWin, Opm),
        setsp(_Win, SpelerIn), !.
 dlg_persoonsgegevens_eh(_Win,e_Menu(idt_neer,_CAS),0):-
	getsp(_Win, SpelerIn),
	not(gelijk(SpelerIn)),
	1 = dlg_MessageBox( "Veranderingen", "De gegevens zijn veranderd.\nBewaren?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
	save_speler1(_Win), !.
 dlg_persoonsgegevens_eh(_Win,e_Menu(idt_neer,_CAS),0):-
	persoon(_Win, SKeus, _, _, _, _, _, _),
	not(sp2(SKeus,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)),
	sp2opg(SKeus,Cat,Opg,_,_,_),
	opg(Opg,Cat,Tgnst,Plts,Rnd,Opgesteld,Tijd,LL,TeamId,Keur,Melding),
	logf('R',opg(Opg,Cat,Tgnst,Plts,Rnd,Opgesteld,Tijd,LL,TeamId,Keur,Melding),nee),
	fail.
 dlg_persoonsgegevens_eh(_Win,e_Menu(idt_neer,_CAS),0):-
	formulierVolgend(No, Index, _),
	formulierSelect(Index),
	loglock(lees, "spelerneer"),
	logclose(),
	sp2(No,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
	SpelerIn = sp2(No,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
	assert(gegevensS(SpelerIn)),
	retractall(persoon(_Win, _, _, _, _, _, _, _)),
        retractall(editcontroltext(idc_edit_control,_,_,_,_,_,_)),
	assert(persoon(_Win, No, Naam, Verh, Sex,ES, DS, UitKant)),
	assert(editcontroltext(idc_edit_control,Opm,b_True,ff_Helvetica,b_False,9,1)),
	EWin = win_GetCtlHandle(_Win, idc_edit_control),
	edit_pastestr(EWin, Opm),
        setsp(_Win, SpelerIn), !.
  dlg_persoonsgegevens_eh(_Win,e_Menu(idb_wis,_CAS),0):-
	persoon(_Win, Persno, _, _, _, _, _, _),
	sp2(PersNo,_,_,_,_,_,_,_,_, _, _, _, _, _, _, _, _, _, _, _,_,_,_,_,_,_,Import),
	mess(Import, Message),
	2 = dlg_MessageBox( "Persoon", Message, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
	!.
  dlg_persoonsgegevens_eh(_Win,e_Menu(idb_wis,_CAS),0):-
	persoon(_Win, Persno, _, _, _, _, _, _),
	sp2(PersNo,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
	SpelerIn = sp2(PersNo,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
	logf('R',SpelerIn,nee), 
	sp2opg(Persno,Cat,Opg,_,_,_),
	opg(Opg,Cat,Tgnst,Plts,Rnd,Opgesteld,Tijd,LL,TeamId,Keur,Melding),
	logf('R',opg(Opg,Cat,Tgnst,Plts,Rnd,Opgesteld,Tijd,LL,TeamId,Keur,Melding),nee),
	fail.
  dlg_persoonsgegevens_eh(_Win,e_Menu(idb_wis,_CAS),0):-
	spelerselRefresh(),
	win_Destroy(_Win),
	!.


%BEGIN Persoonsgegevens, idc_persoonsgegevens_gebdatum losefocus
  dlg_persoonsgegevens_eh(_Win,e_Control(idc_persoonsgegevens_gebdatum,_CtrlType,_CtrlWin,losefocus),0):-
	SrtWin = win_GetCtlHandle(_Win, idc_perslb_spsrt),
	SrtSel = win_GetText(SrtWin),
	%dialog_GetListButton(_Win,idc_perslb_spsrt,_IDC_SPSRTLIST,_IDC_SPSRTLBSELECTIE),
	Geb = win_GetText(_CtrlWin),
	trim_c(Geb),
	trim_c(SrtSel),
	spsrt(_SexN, Srt, _, L1, L2, _, _, _),
	trim_c(Srt),
	Srt = SrtSel,
	not(check_leeft(L1, L2, Geb)),
	Waars = "Leeftijd stemt niet overeen met\nde leeftijdsgrenzen van spelersoort!",
	dlg_MessageBox( "Leeftijd", Waars, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	!.
  dlg_persoonsgegevens_eh(_Win,e_Control(idc_persoonsgegevens_gebdatum,_CtrlType,_CtrlWin,losefocus),0):-
	Geb = win_GetText(_CtrlWin),
	trim_c(Geb),
	Geb <> "",
	not(scheidingsteken(Geb)),
	dlg_MessageBox( "Geboortedatum", "formaat dd-mm-jjjj\n(inclusief de streepjes)", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	!.
  dlg_persoonsgegevens_eh(_Win,e_Control(idc_persoonsgegevens_gebdatum,_CtrlType,_CtrlWin,losefocus),0) :- !.
%END Persoonsgegevens, idc_persoonsgegevens_gebdatum losefocus

%BEGIN Persoonsgegevens, idc_persoonsgegevens_club losefocus
  dlg_persoonsgegevens_eh(_Win,e_Control(idc_persoonsgegevens_club,_CtrlType,_CtrlWin,losefocus),0):-
	openbaar(ja),
	Text = win_GetText(_CtrlWin),
	trim_c(Text),
	clubveld(_Win, Text),
	!.
%END Persoonsgegevens, idc_persoonsgegevens_club losefocus

%BEGIN Persoonsgegevens, e_CloseRequest
  dlg_persoonsgegevens_eh(_Win,e_CloseRequest,0):-
	getsp(_Win, Speler),
	not(gelijk(Speler)),
	1 = dlg_MessageBox( "Veranderingen", "De gegevens zijn veranderd.\nBewaren?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
	trap(CW = win_GetCtlHandle(_Win, idc_ok),_,fail),
	win_SendEvent(_Win,e_Control(idc_ok,wc_PushButton,CW, activated())), !.
  dlg_persoonsgegevens_eh(_Win,e_CloseRequest,0):-
	persoon(_Win, SKeus, _, _, _, _, _, _),
	not(sp2(SKeus,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)),
	sp2opg(SKeus,Cat,Opg,_,_,_),
	opg(Opg,Cat,Tgnst,Plts,Rnd,Opgesteld,Tijd,LL,TeamId,Keur,Melding),
	logf('R',opg(Opg,Cat,Tgnst,Plts,Rnd,Opgesteld,Tijd,LL,TeamId,Keur,Melding),nee),
	fail.
%END Persoonsgegevens, e_CloseRequest

%BEGIN Persoonsgegevens, idc_help _CtlInfo
  dlg_persoonsgegevens_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Persoonsgegevens"),
	!.
%END Persoonsgegevens, idc_help _CtlInfo

%BEGIN Persoonsgegevens, idc_cancel _CtlInfo
  dlg_persoonsgegevens_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-
	getsp(_Win, sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import)),
	not(gelijk(sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import))),
	1 = dlg_MessageBox( "Veranderingen", "De gegevens zijn veranderd.\nBewaren?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
	trap(CW = win_GetCtlHandle(_Win, idc_ok),_,fail),
	win_SendEvent(_Win,e_Control(idc_ok,wc_PushButton,CW, activated())), !.
  dlg_persoonsgegevens_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-
	persoon(_Win, SKeus, _, _, _, _, _, _),
	not(sp2(SKeus,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)),
	sp2opg(SKeus,Cat,Opg,_,_,_),
	opg(Opg,Cat,Tgnst,Plts,Rnd,Opgesteld,Tijd,LL,TeamId,Keur,Melding),
	logf('R',opg(Opg,Cat,Tgnst,Plts,Rnd,Opgesteld,Tijd,LL,TeamId,Keur,Melding),nee),
	fail.
  dlg_persoonsgegevens_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-
	win_Destroy(_Win),
	!.
%END Persoonsgegevens, idc_cancel _CtlInfo

%BEGIN Persoonsgegevens, idc_pbverhaf _CtlInfo
  dlg_persoonsgegevens_eh(_Win,e_Control(idc_pbverhaf,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idc_persoonsgegevens_verhinderd),
	Index = lbox_GetSelIndex(LboxWin),
	retract(persoon(_Win,PersNo, Naam, Verhl, Sex, ES, DS, UitKant)),
	remVerh(Verhl, Index, VerhlN),
	assert(persoon(_Win, PersNo, Naam, VerhlN, Sex, ES, DS, UitKant)),
	lbox_Delete(LboxWin, Index),
	lbox_SetSel(LboxWin, Index, b_True),
	%dialog_SetState(_Win, [enable(idc_pbverhaf, b_False)]),
	!.
%END Persoonsgegevens, idc_pbverhaf _CtlInfo

%BEGIN Persoonsgegevens, e_Create
  dlg_persoonsgegevens_eh(_Win,e_Create(_CreationData),0):-!,
	dialoogpositie(spelervenster, _Win),	% als bewaard van de vorige keer
	%OpgWin = win_GetCtlHandle(_Win, idc_opgavenpartner),
	%RCT = win_GetOuterRect( OpgWin ),
	%RCT = rct(_L, _T, R, _B),
	%R1 = R + 5,
	%tb_formulierbar_Create(_Win, pnt(R1, T)), 
	tb_formulierbar_Create(_Win), 
	gegevensS(SpelerIn),
        setsp(_Win, SpelerIn),
	NulWin = cast(WINDOW, 0),
	retract(persoon(NulWin, Persno, Naam, Verhl, Sex, ES, DS, UitKant)),
	assert(persoon(_Win, Persno, Naam, Verhl, Sex, ES, DS, UitKant)),
	List = win_getCtlHandle(_Win, idc_persoonsgegevens_verhinderd),
	Font = font_create(ff_Fixed, [], 10),
	win_SetFont(List, Font),
	%enableWis(_Win),
	%openbaar(Open),
	%janee2bool(Open, Open1),
	%dialog_SetState(_Win, [enable(idc_persoonsgegevens_club, Open1), enable(idct_club, Open1),show(idc_persoonsgegevens_club, Open1), show(idct_club, Open1)]), 
	%modem_enabled(Modem, _Comport, _Voorkiezen, _Speed, _Toonpuls, _Geluid), % com#, dialinit, speed, init
	%toolbar_SetValue(_Win, idr_bel, ctrl_value(Modem,1)),
	NaamWin = win_GetCtlHandle(_Win, idc_naame),
	win_SetSubclassHandler(NaamWin, scrollhandler_eh, b_False ),
	DeelWin = win_GetCtlHandle(_Win, idc_persoonsgegevens_deelname),
	lbox_SetTabStops(DeelWin, [40, 45]),
	
	!.
%END Persoonsgegevens, e_Create

%BEGIN Persoonsgegevens, idc_opgavenpartner _CtlInfo
  dlg_persoonsgegevens_eh(_Win,e_Control(idc_opgavenpartner,_CtrlType,_CtrlWin,_CtlInfo),0):-
	%GezienWin = win_GetCtlHandle(_Win, idc_gezien),
	%win_Check(GezienWin, b_True),
	getbacktrack(Here),
	persoon(_Win, Persno, _, _, _, _, _, _),
	cutbacktrack(Here),
	% update met spsrt!!
	LboxWin = win_GetCtlHandle(_Win, idc_perslb_spsrt),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	spsrt(SexN, Item, _, _, _, _, _, _),
	NaamWin = win_GetCtlHandle(_Win, idc_naame),
	NaamN = win_GetText(NaamWin),
	retract(persoon(_Win, Persno, _Naam, Verhl, _Sex, ES, DS, UitKant)),
	assert(persoon(_Win, PersNo, NaamN, Verhl, SexN, ES, DS, UitKant)),
	dlg_opgeven_Create(_Win),
	trap(OpgegWin = win_GetCtlHandle(_Win, idc_persoonsgegevens_deelname), _, fail),
	lbox_Clear(OpgegWin),
	fillOpgList(_Win, PersNo, OpgegList),
	lbox_Add(OpgegWin, OpgegList),
	win_PostEvent(_Win,e_User(upd_toolb, 0)),
	%enableWis(_Win),
	!.
%END Persoonsgegevens, idc_opgavenpartner _CtlInfo

%BEGIN Persoonsgegevens, idc_persoonsgegevens_verhinderd selchanged
  dlg_persoonsgegevens_eh(_Win,e_Control(idc_persoonsgegevens_verhinderd,_CtrlType,_LboxWin,selchanged),0):-!,
	dialog_SetState(_Win, [enable(idc_pbverhaf, b_True)]),
	!.
%END Persoonsgegevens, idc_persoonsgegevens_verhinderd selchanged

%BEGIN Persoonsgegevens, idc_verh_nieuw _CtlInfo
  dlg_persoonsgegevens_eh(_Win,e_Control(idc_verh_nieuw,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	%persoon(_Win, Persno, _, Verhl, Sex),
	retract(persoon(_Win, PersNo,_,Verhl, Sex, ES, DS, UitKant)),
	NaamWin = win_GetCtlHandle(_Win, idc_naame),
	NaamN = win_GetText(NaamWin),
	assert(persoon(_Win, PersNo, NaamN, Verhl, Sex, ES, DS, UitKant)),
	dlg_verhindernieuw_Create(_Win, PersNo, Verhl, _VerhlN),
	%LboxWin = win_GetCtlHandle(_Win, idc_persoonsgegevens_verhinderd),
	%lbox_Clear(LboxWin),
	%findall(VerhStr, verhl_strl(VerhlN, VerhStr), SList),
	%lbox_Add(LboxWin, -1, SList),
	!.
%END Persoonsgegevens, idc_verh_nieuw _CtlInfo

  dlg_persoonsgegevens_eh(_,_,_):-!,fail.

%END_DLG Persoonsgegevens


PREDICATES

database - opgavent1

  /*  opgtijd    type          pltsng  ronde   uitsl  indelingsnum */
  opg_t1(integer,wedscat,tgnst,integer,integer,integer,integer,string,string,teamstatus,string)

predicates


  %vrij_opg(integer, wedscat)
procedure metPartner(WINDOW, WINDOW Lbox, integer Index, wedscat, wedsex, wedstyp, integer PersNo)
  withPartner(WINDOW, integer SKeus, wedscat CatN, wedsex, wedstyp Type, tgnst Tgnst) 
  withPartner1(integer SKeus, wedscat, string , tgnst)

nondeterm
  cat_vrij_speler(string NaamMetChar,integer SpelerZelf,wedscat,wedsex,sexe)
  al_opg(integer Spelerno,wedscat,string Naam,string NaamMetChar)
filterSex(sexlist, sexe EigenSex, wedsex, sexlist) - (i,i,i,o)
nondeterm sp_opgavent(WINDOW, integer, wedscat, string, tgnst, BOOLEAN) - (i, i, o, o, o, i) (i, i, i, o, o, i)
nondeterm sp2opgt(integer,wedscat,integer,persno,integer,tgnst)   -  (i,o,o,o,o,o) 
			  (i,i,o,o,o,o) %(i,o,o,o,o,o) language pascal
determ write_maat1(string,string Tijdstip,string,integer,integer,string) - (i, i, i, i, i, o)
%determ   partnerCritFout1(wedscat, string Naam,string, string, string, BOOLEAN Messages)
procedure critMsg(BOOLEAN, string Naam, string Item, string Msg)
  persnoCritFout(WINDOW Parent, wedscat CatN, string Naam, BOOLEAN Msg)
procedure persnoCritFoutCheck(WINDOW Parent, wedscat CatN, integer PersNo, BOOLEAN Msg) % haalt gegevens uit window
procedure metUitsluit(string, integer, string)

CLAUSES

critMsg(b_False, _, _, _) :- !.
critMsg(_, Naam, Item, Msg) :-
  format(Msg1, "%s\n%s", Naam, Msg),
  dlg_MessageBox( Item, Msg1, mesbox_iconexclamation, mesbox_buttonsOK, mesbox_defaultfirst, mesbox_suspendapplication ), !.

partnerCritFout(CatN, PersNo, IMsg, _) :-
  PersNo > 0,
  sp2(PersNo,SexC,NaamS,_,_,_,_,_,_,_,_,_,_, StS, StD, Geb, _, _,_,_,_,_,_,_,_,_,_),
  partnerCritFout1(CatN, NaamS, SexC, Geb, StS, StD, IMsg,_), !.
partnerCritFout(_CatN, _, _IMsg, b_False).
  
partnerCritFout1(CatN, NaamS, Sex, _Geb, _StS, _StD, IMsg,Msg) :-
  wc(CatN, _, _, Naam, _, _CatShort, SL, _, _, _, _HDG, _, _, _, _),
  not(member(Sex,SL)),
  format(Msg, "spelersoort klopt niet!"),
  critMsg(IMsg, NaamS, Naam, Msg), !. 
partnerCritFout1(CatN, NaamS, _Sex, Geb, _StS, _StD, IMsg,Msg) :-
  wc(CatN, _, _, Naam, _, _CatShort, _, _, leeftijd(_,LeeftVan), _, _, _, _, _, _),
  leeftijd(Geb, Leeft, CatN, _),
  Leeft > 0,
  Leeft < LeeftVan,
  format(Msg, "% jaar.\nJonger dan %u!", Leeft, LeeftVan),  
  critMsg(IMsg, NaamS, Naam, Msg), !. 
partnerCritFout1(CatN, NaamS, _Sex, Geb, _StS, _StD, IMsg, Msg) :-
  wc(CatN, _, _, Naam, _, _CatShort, _, _, _, leeftijd(_,LeeftTM), _, _, _, _, _),
  LeeftTM > 0,
  leeftijd(Geb, Leeft, CatN, _),
  Leeft > 0,
  Leeft > LeeftTM,
  format(Msg, "% jaar.\nOuder dan %u!", Leeft, LeeftTM),  
  critMsg(IMsg, NaamS, Naam, Msg), !. 
partnerCritFout1(CatN, NaamS, _Sex, _Geb, Enk, _StD, IMsg, Msg) :-
  wc(CatN, _, _, Naam, e, _CatShort, _, SpSt, _, _, _, _, _, _, _),
  trim_c(Enk),
  not(Enk = ""),
  Enk < Spst,
  format(Msg, "Enkelspelspeelsterkte %.\nHoger dan %s!", Enk, Spst),  
  critMsg(IMsg, NaamS, Naam, Msg), !. 
partnerCritFout1(CatN, NaamS, _Sex, _Geb, _StS, Dub, IMsg, Msg) :-
  wc(CatN, _, _, Naam, EDG, _CatShort, _, SpSt, _, _, _, _, _, _, _),
  not(EDG = e),
  trim_c(Dub),
  not(Dub = ""),
  Dub < Spst,
  format(Msg, "Dubbelspelspeelsterkte %.\nHoger dan %s!", Dub, Spst),  
  critMsg(IMsg, NaamS, Naam, Msg), !. 

metPartner(_Win, Lbox, Index, CatN, WedSex, Type, PersNo) :-
  withPartner(_Win, Persno, CatN, WedSex, Type, Tgnst), 
  getbacktrack(Here),
  date(Year,Month,Day),
  time(Hours,Minutes,_Seconds,_Hundredths),
  dt_min_to_offset(Year, Month, Day, Hours, Minutes, Nr),
  numb(N),     
  Opg = Nr + N - 1,
  not(opg(Opg,CatN,_,_,_,_,_,_,_,_,_)),
  cutbacktrack(Here),
  assert(opg_t1(Opg,CatN,Tgnst,0,0,0,0,"","",na,"")),
  sp_opgavent(_Win, Persno, CatN, Onderdeel, _, b_False),
  UitWin = win_GetCtlHandle(_Win, idc_opgegevenlb),
  lbox_Delete(Lbox, Index),
  lbox_Add(UitWin, Onderdeel), !.
metPartner(_Win, Lbox, Index, _, _, _, _PersNo) :- 
  lbox_SetSel(Lbox, Index, b_False).
	
al_opg(SpNo,Cat,Name,NameN) :-
  sp2opg(SpNo,Cat,_,_,_,Tg),
  Tg = pw(_), !,
  concat("!\t",Name,NameN).
al_opg(SpNo,Cat,Name,NameN) :-
  sp2opg(SpNo,Cat,_,_,_,_), !,
  concat("¬\t",Name,NameN).
al_opg(_, _,Name,NameN) :-
  concat(" \t",Name,NameN).

cat_vrij_speler(NameN,SKeus,Cat,gemengd,Sexe) :-  /* gemengd dubbels */
  %andereSexe(b_True),			% noodzakelijk andere sexe
  wc(Cat, _, _, _, _, _, SexList, _, _, _, _, _, _, _, _),
  %getbacktrack(Here),
  member(Sex,SexList),
  %cutbacktrack(Here),
  %write("\n",Sexe),
  not(spsrt(Sex,_,Sexe,_,_,_,_,_)), % moet iemand van het andere geslacht zijn 
  sp2(SpNo,Sex,Name,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  SpNo <> SKeus,
  not(partnerCritFout(Cat, SpNo, b_False, b_True)), 
  al_opg(SpNo,Cat,Name,NameN). /* signaleer als persoon al in onderdeel */
cat_vrij_speler(NameN,SKeus,Cat,Wedsex,_) :-      /* gewone dubbels */
  not(Wedsex = gemengd),
  %not(Wedsex = hdnvt),
  wc(Cat, _, _, _, _, _, SexList, _, _, _, _, _, _, _, _),
  member(Sex,SexList),
  sp2(SpNo,Sex,Name,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  SpNo <> SKeus, 
  not(partnerCritFout(Cat, SpNo, b_False, b_True)), 
  al_opg(SpNo,Cat,Name,NameN).

filterSex(List, _, Typ, List) :- 
  not(Typ = gemengd),
  not(Typ = hdnvt), !.
filterSex([S|Rest], Eigen, Typ, [S|Uit]) :-
  spsrt(S,_,Sexe,_,_,_,_,_),
  not(Sexe = Eigen), !,
  filterSex(Rest, Eigen, Typ, Uit).
filterSex([_|Rest], Eigen, Typ, Uit) :- !,
  filterSex(Rest, Eigen, Typ, Uit).
filterSex([], _, _, []).

withPartner(_, SKeus, _CatN, _Wedsex, e, s(SKeus)) :- !. % geen partner
withPartner(Win, SKeus, Cat, WedSex, _Mixed, Resultpartner) :- 
  wc(Cat, _, _, Catnaam, _Mixed, _, SexList, _, _, _, HDGN, _, _, _, _),
  ParentWin = win_GetParent(Win),
  persoon(ParentWin,SKeus, _Naam, _Verhl, SpCat, _ES, _DS, _Uitkant),
  spsrt(SpCat,_,MV,_,_,_,_,_), !,
  findall(Namey,cat_vrij_speler(Namey,SKeus,Cat,WedSex,MV),NaamList),
  %list_to_string(NaamList, "\n", Lijst),
  %write(Lijst),
  filterSex(SexList, MV, HDGN, SexListF),
  sortstring_c(NaamList, 2),
  format(Titel, "Partner voor %s:", CatNaam),
  dlg_partnerselectie_Create(Win, Titel, NaamList, SexListF, NaamSel),
  withPartner1(SKeus, Cat, NaamSel, ResultPartner).

withPartner1(SKeus, _, "<wanted>", pw(SKeus)) :- !.
withPartner1(SKeus, Cat, NaamSel, Uit) :- 
  frontstr(2,NaamSel,_,NaamSel1),
  sp2(Partner,_S1,NaamSel1,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !,
  not(partnerCritFout(Cat, Partner, b_True, b_True)),
  orderedTgnst(p(SKeus, Partner), Uit).
  %orde(Naam,NaamSel1,SKeus,Partner, S1, S2).
	
orde(_Nam1,_Nam2,SKeus,Partner, SKeus, Partner) :-	% altijd vrouw eerst 
  sp2(SKeus,S1,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  spsrt(S1,_,v,_,_,_,_,_),
  sp2(Partner,S2,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  spsrt(S2,_,m,_,_,_,_,_), !.	% 21.11.2007 tenzij partner ook vrouw!
orde(_Nam1,_Nam2,SKeus,Partner, Partner, SKeus) :-
  sp2(Partner,S1,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  spsrt(S1,_,v,_,_,_,_,_),
  sp2(SKeus,S2,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  spsrt(S2,_,m,_,_,_,_,_), !.	% 21.11.2007 tenzij partner ook vrouw!
orde(Nam,Nam1,SKeus,Partner, SKeus, Partner) :-	% anders alfabetisch
  upper_lower(Nam,Naam),
  upper_lower(Nam1,Naam1),
  Naam < Naam1, !.
orde(_Nam,_Nam1,SKeus,Partner,Partner, SKeus).
  
sp_opgavent(_Win, SKeus, CatN, String1, Tg, b_False) :-
  sp2opgt(SKeus,CatN,Opg,MaatNo,_,Tg),
  met_maat(MaatNo, Maat),
  opg_t1(Opg,CatN,Tg,Plts,Rnd,Uitsl,_,_,_,_,_),
  wc(CatN, _, _, _, _, Str, _, _, _, _, _, _, _, _, _),
  dt_minoffset_to_str(Opg, "%DD/%MD %HH:%MM", Tijd),
  write_maat1(Str,Tijd,Maat,Plts,Rnd,String),
  metUitsluit(String, Uitsl, String1).
sp_opgavent(PWin, SKeus, CatN, String1, Tg, b_True) :-		% true = check
  sp2opgt(SKeus,CatN,Opg,MaatNo,_,Tg),
  met_maat(MaatNo, Maat),
  persnoCritFoutCheck(PWin, CatN, SKeus, b_True),		% true = messages
  partnerCritFout(CatN, MaatNo, b_True, b_False),		% messages en niet_fail.
  opg_t1(Opg,CatN,Tg,Plts,Rnd,Uitsl,_,_,_,_,_),
  wc(CatN, _, _, _, _, Str, _, _, _, _, _, _, _, _, _),
  dt_minoffset_to_str(Opg, "%DD/%MD %HH:%MM", Tijd),
  write_maat1(Str,Tijd, Maat,Plts,Rnd,String),
  metUitsluit(String, Uitsl, String1).

metUitsluit(In, 0, In) :- !.
metUitsluit(In, _, Uit) :-
  concat("U:", In, Uit), !.

sp2opgt(SpNo,Cat,OpgNo,0,Plts,s(SpNo)) :-      		/****** zoek opgave bij speler   */
  opg_t1(OpgNo,Cat,s(SpNo),Plts,_,_,_,_,_,_,_).      		/* enkelspel                     */
sp2opgt(SpNo,Cat,OpgNo,-1,Plts,pw(SpNo)) :-      		/****** zoek opgave bij speler   */
  opg_t1(OpgNo,Cat,pw(SpNo),Plts,_,_,_,_,_,_,_).      		/* partner wanted                     */
sp2opgt(SpNo,Cat,OpgNo,MaatNo,Plts,p(SpNo,MaatNo)) :-	/*      en dubbelspel            */
  opg_t1(OpgNo,Cat,p(SpNo,MaatNo),Plts,_,_,_,_,_,_,_).
sp2opgt(SpNo,Cat,OpgNo,MaatNo,Plts,p(MaatNo,SpNo)) :-
  opg_t1(OpgNo,Cat,p(MaatNo,SpNo),Plts,_,_,_,_,_,_,_).

write_maat1(Wc,Tijd, "",0,0, Wc1) :- !,
  format(Wc1, "%s\t%s\t", Wc, Tijd). 
write_maat1(Wc,Tijd, "",-1,0, Wc1) :- !,
  format(Wc1, "%s\t%s\tzoekt partner.", Wc, Tijd). 
write_maat1(Wc,Tijd,Maat,0,0, Str) :- !,
  format(Str, "%s\t%s\t%-20",Wc,Tijd, Maat).
write_maat1(Wc,Tijd,"",X,0, Str) :- !,
  format(Str,"%s\t%s\tp:%",Wc,Tijd,X).
write_maat1(Wc,Tijd,Maat,X,0, Str) :- !,
  format(Str,"%s\t%s\t%-18 p:%",Wc,Tijd,Maat,X).
write_maat1(Wc,Tijd,Maat,X,Y, Str) :-
  Y1 = Y + 1,
  format(Str,"%s\t%s\t%-18 p:% r:%",Wc,Tijd,Maat,X,Y1).

%BEGIN_DLG Opgeven
/**************************************************************************
	Creation and event handling for dialog: Opgeven
**************************************************************************/

constants

%BEGIN Opgeven, CreateParms, 16:17:15-23.11.2013, Code automatically updated!
  dlg_opgeven_DlgType = wd_Modal
  dlg_opgeven_Title = "Opgeven"
  dlg_opgeven_RCT = rct(38,32,397,190)
  dlg_opgeven_Flags = [wsf_Close,wsf_TitleBar]
  dlg_opgeven_Help = idh_opgeven
  dlg_opgeven_Font = "Arial Narrow"
  dlg_opgeven_FSize = 10
%END Opgeven, CreateParms

predicates

  dlg_opgeven_eh : EHANDLER
  initopg(integer Persno)
  nondeterm sex_cat(WINDOW, integer,sex,string)
  sp_catI(integer,wedscat)
procedure upd_opg(integer Persno)
nondeterm  in_wedstrijd(wedscat,integer,tgnst,integer) 
alsIngedeeld(wedscat)
  upd_schema(integer, integer, wedscat, tgnst, rsoort) % was upd_wd
  volg1(integer,wedscat,tgnst,rsoort) 
  uitslag(tgnst, tgnst, tgnst)
procedure  planning_upd(integer, wedscat, tgnst)
nondeterm selectLaatste(wedscat, integerlist)
procedure eersteOpgave(tgnst, integer)				% zorg dat andere speler in forulier
procedure uitsl2Text(integer, string Toelaten)
determ partnerW(integer Speler, tgnst, integer Partner)
procedure veranderTekst(window, integer)
determ ondanks(integer, integer, wedscat)
%determ schemasoortAR(string Schemasoort)  

clauses

upd_schema(_, WNum, Cat, bye, _) :-  			% haal eventuele 
  getbacktrack(Here),
  wd(Wnum,Cat,T1,T2,_,Parm1,Nota), 
  cutbacktrack(Here),
  logf('Z',wd(Wnum,Cat,T1,T2,"",Parm1,Nota),ja),
  w3(WNum,Cat,W,PW,PV,Stat,T),				%  uitslag weg
  logf('R',w3(WNum,Cat,W,PW,PV,Stat,T),ja),
  fail.
upd_schema(1, WNum, Cat, TgnstKeus, Schema) :-  	% 1 = links
  wd(Wnum,Cat,Tx,T2,Score,Parm1,Nota),
  not(TgnstKeus = Tx), !,
  logf('Z',wd(Wnum,Cat,TgnstKeus,T2,Score,Parm1,Nota),ja),
  %select(Wnum, Cat),
  uitslag(TgnstKeus, T2, Winnaar),
  volg1(Wnum,Cat,Winnaar,Schema).
upd_schema(2, WNum, Cat, TgnstKeus, Schema) :-		% 2 = rechts
  wd(Wnum,Cat,T1,Tx,Score,Parm1,Nota),
  not(TgnstKeus = Tx), !,
  logf('Z',wd(Wnum,Cat,T1,TgnstKeus,Score,Parm1,Nota),ja),
  %select(Wnum, Cat),
  uitslag(T1, TgnstKeus, Winnaar),
  volg1(Wnum,Cat,Winnaar,Schema).
upd_schema(_, WNum, Cat, _, Schema) :-		% ga door om planning
  wd(Wnum,Cat,T1,T2,_,_,_),
  %select(Wnum, Cat),
  uitslag(T1, T2, Winnaar), !,
  volg1(Wnum,Cat,Winnaar,Schema).

uitslag(bye, bye, bye) :- !.
uitslag(T1, bye, T1)   :- !.
uitslag(bye, T2, T2)   :- !.
uitslag(_, _, onbekend).

volg1(No, Cat, Winnaar, Schema) :-
  planning_upd(No, Cat, Winnaar),
  bitright(No, 1, VolgNo),
  bitand(No, 1, LR),
  wd(VolgNo,Cat,_,_,_,_,_), 		% bestaat ie?
  LofR = LR + 1,
  update_display(),
  upd_schema(LofR, VolgNo, Cat, Winnaar, Schema), !.   % schema bestaat, ga door
volg1(_, _, _, _).

ondanks(0, WNum, Cat) :-
  w2(WNum,Cat,Tijd,J1,J2,_X,Y,Bn),
  logf('Z',w2(WNum,Cat,Tijd,J1,J2,'f',Y,Bn),ja), !,
  fail.	% fixeer dan
ondanks(_, WNum, Cat) :-
  w2(WNum,Cat,Tijd,J1,J2,X,Y,Bn), !,
  logf('R',w2(WNum,Cat,Tijd,J1,J2,X,Y,Bn),ja).

planning_upd(WNum, Cat, _) :-
  ws(WNum, Cat, Tgnst, Tijd),
  logf('R', ws(WNum, Cat, Tgnst, Tijd),ja),
  fail.
planning_upd(WNum, Cat, onbekend) :-  			% haal eventuele 
  w2(WNum,Cat,Tijd,_J1,_J2,_X,_Y,_Bn),
  check_forceer_c(0, [Tijd], Wnum, Cat, _, Result,_Bxx,_TT),
  Result <> 0,
  check_forceer_c(1, [Tijd], Wnum, Cat, _, _, Bezwaren,_), 
  Parent = vpi_GetTaskWin(),
  logclose(),
  dlg_bezwaren_Create(Parent, Wnum, Cat, Bezwaren, Result1),
  loglock(mdi, "planning_upd"),
  %Result1 <> 0,
  ondanks(Result1, WNum, Cat), !.
  %logf('R',w2(WNum,Cat,Tijd,J1,J2,X,Y,Bn),ja), !.
planning_upd(WNum, Cat, Tgnst) :-  			% haal eventuele 
  not(Tgnst = onbekend),
  w2(WNum,Cat,Tijd,J1,J2,X,Y,Bn),			%  planning weg
  logf('R',w2(WNum,Cat,Tijd,J1,J2,X,Y,Bn),ja), !.
planning_upd(_, _, _).

initopg(Persno) :-
  sp2opg(Persno,Cat,Opg,_,_,_), 
  opg(Opg,Cat,Tgnst,Plaats,Ronde,Uitsl,Indelingsnr,LL,TeamId,Keur,Melding),
  assert(opg_t1(Opg,Cat,Tgnst,Plaats,Ronde,Uitsl,Indelingsnr,LL,TeamId,Keur,Melding)),
  fail.
initopg(_).

selectLaatste(_, _).
selectLaatste(CatN, [Wd|_]) :- 	% select tijdens backtrack
  select(CatN, Wd),
  fail.

partnerW(Speler, p(Speler, Partner), Partner) :- !.
partnerW(Speler, p(Partner, Speler), Partner).

upd_opg(_Persno) :-
  loglock(mdi, "upd_opg"),
  fail.
upd_opg(Persno) :-					% opgave met pw-partner
  opg_t1(_Opg,Cat,Tgnst,_Plaats,_Ronde,_Uitsl,_Indelingsnr,_LL,_TeamId,_Keur,_Melding),
  partnerW(PersNo, Tgnst, Partner),
  sp2opg(Partner, Cat,OpgX, _, _, Tgx),		% er is een opgave van die partner
  not(Tgx = Tgnst),
  opg(Opgx,Cat,Tgx,Plaatsx,Rondex,Uitslx,Indelingsnrx,LLx,TeamIdx,Keurx,Meldingx),
  logf('R',opg(Opgx,Cat,Tgx,Plaatsx,Rondex,Uitslx,Indelingsnrx,LLx,TeamIdx,Keurx,Meldingx),ja),
  pos(Cat, Card, _, Schema),		% in een ingedeeld schema
  in_wedstrijd(Cat, WedV, Tgx, LR), 
  WedV >= Card,
  close_alles(),
  select(Cat, WedV),
  upd_schema(LR, WedV, Cat, bye, Schema),		% haal uit schema
  update_display(),
  fail.
upd_opg(Persno) :-					% opg niet meer
  sp2opg(Persno,Cat,OpgNo,_,_,_), 
  not(opg_t1(OpgNo,Cat,_,_,_,_,_,_,_,_,_)),
  opg(OpgNo,Cat,Tgnst,Plaats,Ronde,Uitsl,Indelingsnr,LL,TeamId,Keur,Melding),
  logf('R',opg(OpgNo,Cat,Tgnst,Plaats,Ronde,Uitsl,Indelingsnr,LL,TeamId,Keur,Melding),ja),
  fail.
upd_opg(Persno) :-
  sp2opg(Persno,CatN,OpgNo,_,_,_),
  opg(OpgNo,CatN,Tgnst,_XPlaats,_XRonde,_XUitsl,_XIndelingsnr,_XLL,_XTeamId,_XKeur,_XMelding),
  opg_t1(OpgNo,CatN,TgnstN,Plaats,Ronde,Uitsl1,Indelingsnr,LL,TeamId,Keur,Melding),
  not(TgnstN = Tgnst),					% andere tegenst
  pos(CatN, Card, _, Schema),
  findall(WdX, in_wedstrijd(CatN, WdX, Tgnst, _), WdXList),
  sortint_c(WdXList),
  reverse(WdXList, [], WdXRev),
  selectLaatste(CatN, WdXList),
  member(WedV, WdXRev),
  WedV >= Card,		% voor de zekerheid
  in_wedstrijd(CatN, WedV, Tgnst, LR), 
  close_schemas(),
  close_poules(),
  upd_schema(LR, WedV, CatN, TgnstN, Schema),		% update schema
  eersteOpgave(TgnstN, PersNo),
  retract(opg_t1(OpgNo,CatN,_,_,_,_,_,_,_,_,_)),
  logf('Z',opg(OpgNo,CatN,TgnstN,Plaats,Ronde,Uitsl1,Indelingsnr,LL,TeamId,Keur,Melding),ja),
  fail.
upd_opg(Persno) :-
  sp2opg(Persno,Cat,_,_,_,_),
  opg(Opg,Cat,Tgnst,Plaats,Ronde,Uitsl,Indelingsnr,LL,TeamId,Keur,Melding),	% opg zelfde gebleven
  retract(opg_t1(Opg,Cat,Tgnst,Plaats,Ronde,Uitsl,Indelingsnr,LL,TeamId,Keur,Melding)),
  fail.
upd_opg(Persno) :-						% opg alleen maar veranderd
  retract(opg_t1(Opg,Cat,Tgnst,Plaats,Ronde,Uitsl,Indelingsnr,LL,TeamId,Keur,Melding)),
  eersteOpgave(Tgnst, Persno),
  logf('Z',opg(Opg,Cat,Tgnst,Plaats,Ronde,Uitsl,Indelingsnr,LL,TeamId,Keur,Melding),ja),
  opg(OpgO,Cat,Tgnst,PlaatsX,RondeX,UitslX,IndelingsnrX,LLX,TeamIdX,KeurX,MeldingX),
  OpgO <> Opg,							% opgavetijdstip is ook veranderd
  logf('R',opg(OpgO,Cat,Tgnst,PlaatsX,RondeX,UitslX,IndelingsnrX,LLX,TeamIdX,KeurX,MeldingX),ja),
  fail.
upd_opg(_) :-
  logclose().

eersteOpgave(s(_), _) :- !.
eersteOpgave(p(Sp,_), PersNo) :-
  Sp <> PersNo,
  not(sp2opg(Sp,_,_,_,_,_)),	% nog geen opgave
  sp2(Sp,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,ja,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
  SpelerIn = sp2(Sp,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,ja,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
  logf('Z', SpelerIn, ja), !.
eersteOpgave(p(_,Sp), PersNo) :-
  Sp <> PersNo,
  not(sp2opg(Sp,_,_,_,_,_)),	% nog geen opgave
  sp2(Sp,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,ja,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
  SpelerIn = sp2(Sp,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,ja,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
  %sp(Sp, Sex, Naam, Tel, Verh, Offset, _, Uitkant), 
  logf('Z', SpelerIn, ja), !.
eersteOpgave(_, _) :- 
  vpi_Alarm(mesbox_iconexclamation).


schemasoortAR(Wcat,Oid) :-
  %wc(WCat, Tid, _, _, _, _, _, _, _, _, _, _, _, _Tid, _),
  fronttoken(Oid,_,_),
  kopl(WCat, _, WH, _),
  wc(WH, OidH, _, _, _, _, _, _, _, _, _, _, _, _Tidx, _),
  fronttoken(OidH,_, _), !.
    
  
/*
sex_cat(PWin, SpNo,Sex,NaamKort) :-
  wc(WCat, _, _, _, _, NaamKort, _SexList, _, _, _, hdnvt, _, _, _Tid, _),
  %not(schemasoortAR(Schemasoort)),
  %not(toernooiK(Tid,nrt)),
  %not(toernooiK(Tid,jrt)),
  %member(Sex,SexList),
  not(sp_catI(SpNo,WCat)),
  not(persnoCritFout(PWin, WCat, "", b_False)).	% geen messages dus ook geen naam
*/
sex_cat(PWin, SpNo,Sex,NaamKort) :-
  wc(WCat, Oid, _, _, _, NaamKort, SexList, _, _, _, _, _Schemasoort, _, Tid, _),
  not(schemasoortAR(Wcat,Oid)),
  not(toernooiK(Tid,nrt)),
  not(toernooiK(Tid,jrt)),
  member(Sex,SexList),
  not(sp_catI(SpNo,WCat)),
  not(persnoCritFout(PWin, WCat, "", b_False)).	% geen messages dus ook geen naam
sex_cat(PWin, SpNo,Sex,NaamKort) :-
  wc(WCat, _, _, _, _, NaamKort, SexList, _, _, _, _, _, _, Tid, _),
  toernooiK(Tid,jrt),
  member(Sex,SexList),
  not(sp_catI(SpNo,WCat)),
  not(persnoCritFout(PWin, WCat, "", b_False)).
sex_cat(PWin, SpNo,Sex,NaamKort) :-
  wc(WCat, _, _, _, _, NaamKort, SexList, _, _, _, _, _, _, Tid, _),
  toernooiK(Tid,nrt),
  member(Sex,SexList),
  not(sp_catI(SpNo,WCat)),
  not(persnoCritFout(PWin, WCat, "", b_False)).

sp_catI(SpNo,Cat) :-
  opg_t1(_,Cat,s(SpNo),_,_,_,_,_,_,_,_), !.
sp_catI(SpNo,Cat) :-
  opg_t1(_,Cat,p(SpNo,_),_,_,_,_,_,_,_,_), !.
sp_catI(SpNo,Cat) :-
  opg_t1(_,Cat,p(_,SpNo),_,_,_,_,_,_,_,_), !.

alsIngedeeld(CatN) :-
  opg_t1(_, CatN, Tgnst1, _, _, _, _, _, _, _, _), 
  pos(CatN, _Card, _, afval),
  getbacktrack(Here),                   % test of al ingedeeld
  in_wedstrijd(CatN, _WdNo, Tgnst1, _), !,
  %WdNo >= Card,
  cutbacktrack(Here),
  Answ = dlg_Ask("Opgave verwijderen",
  		"Is al ingedeeld...\nOmzetten in bye?",
  		["&Ja","&Cancel"]),
  Answ = resp_Default.
alsIngedeeld(CatN) :-
  opg_t1(_, CatN, Tgnst1, _, _, _, _, _, _, _, _), 
  pos(CatN, _Card, _, poel),
  getbacktrack(Here),                   % test of al ingedeeld
  in_wedstrijd(CatN, _WdNo, Tgnst1, _), !,
  %WdNo >= Card,
  cutbacktrack(Here),
  dlg_MessageBox( "Kan niet verwijderen", "Eerst pouleopstelling wijzigen", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.
alsIngedeeld(_).

in_wedstrijd(Cat,WdNo,Tgnst,1):- 
  wd(WdNo,Cat,Tgnst,_,_,_,_).
in_wedstrijd(Cat,WdNo,Tgnst,2):- 
  wd(WdNo,Cat,_,Tgnst,_,_,_).

persnoCritFoutCheck(PWin, CatN, _PersNo, IMsg) :-
  NaamWin = win_GetCtlHandle(PWin, idc_naame),
  Naam = win_GetText(NaamWin),
  persnoCritFout(PWin, CatN, Naam, IMsg), !.
persnoCritFoutCheck(_PWin, _CatN, _PersNo, _IMsg).

persnoCritFout(PWin, CatN, NaamS, IMsg) :-
  wc(CatN, _, _, Naam, _, _, _, _, leeftijd(_,LeeftVan), _, _, _, _, _, _),
  %wc(CatN,Naam,_,_NaamKort,_SpCatL,_,_),
  GebWin = win_GetCtlHandle(PWin, idc_persoonsgegevens_gebdatum),
  Geb = win_GetText(GebWin),
  %wcritP(CatN,  _Spst, LeeftVan, _LeeftTM, _),
  leeftijd(Geb, Leeft, CatN, _),
  Leeft > 0,
  Leeft < LeeftVan,
  format(Msg, "% jaar.\nJonger dan %u!", Leeft, LeeftVan),  
  critMsg(IMsg, NaamS, Naam, Msg), !. 
  %2 = dlg_MessageBox( "Leeftijd", Msg, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ), !.
persnoCritFout(PWin, CatN, NaamS, IMsg) :-
  wc(CatN, _, _, Naam, _, _, _, _, _, leeftijd(_,LeeftTM), _, _, _, _, _),
  %wc(CatN,Naam,_,_NaamKort,_SpCatL,_,_),
  %wcritP(CatN,  _Spst, _LeeftVan, LeeftTM, _),
  LeeftTM > 0,
  GebWin = win_GetCtlHandle(PWin, idc_persoonsgegevens_gebdatum),
  Geb = win_GetText(GebWin),
  leeftijd(Geb, Leeft, CatN, _),
  Leeft > 0,
  Leeft > LeeftTM, 
  format(Msg, "% jaar.\nOuder dan %u!", Leeft, LeeftTM),  
  critMsg(IMsg, NaamS, Naam, Msg), !. 
  %2 = dlg_MessageBox( "Leeftijd", Msg, mesbox_iconexclamation, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ), !.
persnoCritFout(PWin, CatN, NaamS, IMsg) :-
  wc(CatN, _, _, Naam, e, _, _, SpSt, _, _, _, _, _, _, _),	% 29.7.2016
  %wc(CatN,Naam,e,_NaamKort,_SpCatL,_,_),
  %wcritP(CatN,  Spst, _LeeftVan, _LeeftTM, _),
  EWin = win_GetCtlHandle(PWin, idc_edit_enkel1),
  Enk = win_GetText(EWin),
  trim_c(Enk),
  not(Enk = ""),
  fronttoken(Enk, EnkN, _),
  EnkN < Spst,
  format(Msg, "Enkelspelspeelsterkte %.\nHoger dan %s!", Enk, Spst),  
  critMsg(IMsg, NaamS, Naam, Msg), !. 
persnoCritFout(PWin, CatN, NaamS, IMsg) :-
  wc(CatN, _, _, Naam, EDG, _, _, SpSt, _, _, _, _, _, _, _),
  %wc(CatN,Naam,EDG,_NaamKort,_SpCatL,_,_),
  not(EDG = e),
  %wcritP(CatN,  Spst, _LeeftVan, _LeeftTM, _),
  DWin = win_GetCtlHandle(PWin, idc_persoonsgegevens_dubbele1),
  Dub = win_GetText(DWin),
  trim_c(Dub),
  not(Dub = ""),
  fronttoken(Dub, DubN, _),
  DubN < Spst,
  format(Msg, "Dubbelspelspeelsterkte %.\nHoger dan %s!", Dub, Spst),  
  critMsg(IMsg, NaamS, Naam, Msg), !. 
  %2 = dlg_MessageBox( "Leeftijd", Msg, mesbox_iconexclamation, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ), !.

stripU(NaamKort, NaamKortS1) :-
  fronttoken(NaamKort, "U", NaamKortS), 
  frontchar(NaamKortS,_FrontChar,NaamKortS1), !.
stripU(NaamKort, NaamKort).

alIngedeeld(Tgnst, Cat, 1) :-
  in_weds(Cat, Tgnst),
  dlg_MessageBox( "Uitsluiten", "Stond(en) al ingedeeld.\nVergeet niet speler(s) uit het schema te verwijderen met\nOnderdeel, Indelen, Wijzig opstelling of door opnieuw in te delen.", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  !.
alIngedeeld(_Tgnst, _Cat, _).

uitsl2Text(1, "Uitsluiten") :- !.
uitsl2Text(_, "Toelaten").

veranderTekst(CWin, -1) :- !,
  win_SetText(CWin, "Geef partner").
veranderTekst(CWin, _) :- !,
  win_SetText(CWin, "&Andere partner").

  dlg_opgeven_Create(Parent):-
	loglock(lees, "dlg_opgeven"),
	logclose(),
	win_CreateDynDialog(Parent,
		[
%BEGIN Opgeven, WinDefList, 16:17:15-23.11.2013, Code automatically updated!
		 dlg_font(wdef(dlg_opgeven_DlgType,dlg_opgeven_RCT,dlg_opgeven_Title,u_DlgBase),
		 	  dlg_opgeven_Font,dlg_opgeven_FSize,dlg_opgeven_Flags),
		 ctl(wdef(wc_LBox,rct(1,15,105,150),"",u_DlgBase),idc_opgeven_lbox,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_MultiSelect]),
		 ctl(wdef(wc_LBox,rct(127,15,357,127),"",u_DlgBase),idc_opgegevenlb,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_UseTabStops]),
		 ctl(wdef(wc_PushButton,rct(106,73,124,83),"&Bij >",u_DlgBase),idc_opgevenbij,[wsf_Group,wsf_TabStop,wsf_Disabled]),
		 ctl(wdef(wc_PushButton,rct(106,92,124,102),"< &Af",u_DlgBase),idc_opgevenaf,[wsf_Group,wsf_TabStop,wsf_Disabled]),
		 ctl(wdef(wc_PushButton,rct(152,127,204,139),"&Andere partner",u_DlgBase),idc_verander_partner,[wsf_Group,wsf_TabStop,wsf_Disabled]),
		 ctl(wdef(wc_PushButton,rct(210,127,258,139),"&Uitsluiten",u_DlgBase),idc_uitsluiten,[wsf_Group,wsf_TabStop,wsf_Disabled]),
		 ctl(wdef(wc_PushButton,rct(264,127,312,139),"&Details",u_DlgBase),idc_details,[wsf_Group,wsf_TabStop,wsf_Disabled]),
		 ctl(wdef(wc_PushButton,rct(325,135,357,156),"&OK",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(257,146,287,156),"&Cancel",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(291,146,321,156),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_Text,rct(4,3,76,13),"In aanmerking komen:",u_DlgBase),idct_opgin_aanmerking_komen,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(131,3,195,13),"Ingeschreven voor:",u_DlgBase),idct_ingeschreven_voor,[wsf_AlignLeft])
%END Opgeven, WinDefList
		],dlg_opgeven_eh,0).

%BEGIN Opgeven, idc_ok _CtlInfo
  dlg_opgeven_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-!,
	Parent = win_GetParent(_Win),
	persoon(Parent, Persno, _Naam, _Verhinderingen, _Sex, _, _, _),
	upd_opg(Persno),
	dlg_plaatsing_refresh(),
	dlg_opgaven_refresh(0), !,
	trap(win_Destroy(_Win), _, fail).
%END Opgeven, idc_ok _CtlInfo
%MARK Opgeven, new events

%BEGIN Opgeven, idc_details _CtlInfo
  dlg_opgeven_eh(_Win,e_Control(idc_details,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	Lbox  = win_GetCtlHandle(_Win, idc_opgegevenlb),
	Index = lbox_GetSelIndex(Lbox),
	Item  = lbox_GetItem(Lbox, Index),
	searchchar(Item,'\t',Pos),
	P1 = Pos - 1,
	frontstr(P1, Item, NaamKort, _),
	stripU(NaamKort, NaamKortS),
	wc(Cat, _, _, _Naam, _, NaamKortS, _, _, _, _, _, _, _, _, _),
	%wc(Cat,_,_Type,NaamKortS,_SpCatL,_,_),
	%retract(opg_t1(Opg,CatN,Tgnst,A,B,Uitsl,C,D)),
	opg_t1(Num,Cat,Tgnst,Plaats,Ronde,Uitsl,TijdKK,LL,TeamId,Keur,Melding), 
	dlg_plaatsing1_Create(_Win, Num, Cat, Tgnst,Plaats,Ronde,Uitsl,TijdKK,LL,NumX ,PlaatsX,RondeX,UitslX,TijdX,LLX,Result,1),
	Result = idc_ok,
	retract(opg_t1(Num,Cat,Tgnst,Plaats,Ronde,Uitsl,TijdKK,LL,TeamId,Keur,Melding)),
	assert(opg_t1(NumX,Cat,Tgnst,PlaatsX,RondeX,UitslX,TijdX,LLX,TeamId,Keur,Melding)),
	Parent = win_GetParent(_Win),
	persoon(Parent, Persno, _Naamy, _Verhinderingeny, _Sexy, _, _, _),
	findall(Onderdeel, sp_opgavent(_Win, Persno,_, Onderdeel, _, b_False), Onderdelen),
	%UitWin = win_GetCtlHandle(_Win, idc_opgegevenlb),
	lbox_Clear(Lbox),
	lbox_Add(Lbox, Onderdelen),
	lbox_SetSel(Lbox, Index, b_True),
	uitsl2Text(UitslX, KnopText),
	UWin = win_GetCtlHandle(_Win, idc_uitsluiten),
	win_SetText(UWin, KnopText),
	!.
%END Opgeven, idc_details _CtlInfo

%BEGIN Opgeven, idc_uitsluiten _CtlInfo
  dlg_opgeven_eh(_Win,e_Control(idc_uitsluiten,_CtrlType,_CtrlWin,_CtlInfo),0):-
	Lbox  = win_GetCtlHandle(_Win, idc_opgegevenlb),
	Index = lbox_GetSelIndex(Lbox),
	Item  = lbox_GetItem(Lbox, Index),
	searchchar(Item,'\t',Pos),
	P1 = Pos - 1,
	frontstr(P1, Item, NaamKort, _),
	stripU(NaamKort, NaamKortS),
	wc(CatN, _, _, _Naam, _, NaamKortS, _, _, _, _, _, _, _, _, _),
	%wc(CatN,_,_Type,NaamKortS,_SpCatL,_,_),
	retract(opg_t1(Opg,CatN,Tgnst,A,B,Uitsl,C,D,TeamId,Keur,Melding)),
	bitxor(Uitsl, 1, Uitsl1),
	assert(opg_t1(Opg,CatN,Tgnst,A,B,Uitsl1,C,D,TeamId,Keur,Melding)),
	Parent = win_GetParent(_Win),
	persoon(Parent, Persno, _Naamy, _Verhinderingeny, _Sexy, _, _, _),
	findall(Onderdeel, sp_opgavent(_Win, Persno,_, Onderdeel, _, b_False), Onderdelen),
	%UitWin = win_GetCtlHandle(_Win, idc_opgegevenlb),
	lbox_Clear(Lbox),
	lbox_Add(Lbox, Onderdelen),
	lbox_SetSel(Lbox, Index, b_True),
	uitsl2Text(Uitsl, KnopText),
	UWin = win_GetCtlHandle(_Win, idc_uitsluiten),
	win_SetText(UWin, KnopText),
	alIngedeeld(Tgnst, CatN, Uitsl1),
	!.
%END Opgeven, idc_uitsluiten _CtlInfo

%BEGIN Opgeven, idc_opgeven_lbox activated
  dlg_opgeven_eh(_Win,e_Control(idc_opgeven_lbox,_CtrlType,Lbox,activated),0):-
	lbox_GetSel(Lbox,_ItemL1,IndexL1),
	IndexL1 = [Index|_],
	CatNaamKort = lbox_GetItem(Lbox, Index),
	wc(CatN, _, _, _Naam, Type, CatNaamKort, _, _, _, _, _, _, _, _, _),
	%wc(CatN,_Item,Type,CatNaamKort,_SpCatL,_,_),
	ParentWin = win_GetParent(_Win),
	persoon(ParentWin, Persno, _NaamX, _Verhinderingen,_Sex, _, _, _),
	%not(persnoCritFout(_Win, CatN, PersNo)),
	xSpelsoort(CatN, _MVs, MVdom, _WTs, _WTd),
	metPartner(_Win, Lbox, Index, CatN, MVdom, Type, PersNo),
	dialog_SetState(_Win, [enable(idc_opgevenbij, b_False),enable(idc_opgevenaf, b_False)]),
	!.
%END Opgeven, idc_opgeven_lbox activated

%BEGIN Opgeven, idc_help _CtlInfo
  dlg_opgeven_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Opgeven"),
	!.
%END Opgeven, idc_help _CtlInfo

%BEGIN Opgeven, idc_verander_partner _CtlInfo
  dlg_opgeven_eh(_Win,e_Control(idc_verander_partner,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	Lbox  = win_GetCtlHandle(_Win, idc_opgegevenlb),
	Index = lbox_GetSelIndex(Lbox),
	Item  = lbox_GetItem(Lbox, Index),
	searchchar(Item,'\t',Pos),
	P1 = Pos - 1,
	frontstr(P1, Item, NaamKort, _),
	stripU(NaamKort, NaamKortS),
	wc(CatN, _, _, _, Type, NaamKortS, _, _, _, _, _, _, _, _, _),
	%wc(CatN,_,Type,NaamKortS,_SpCatL,_,_),
	opg_t1(Opg,CatN,_TgnstX,_,_,_,_,_,_,_,_),
	Parent = win_GetParent(_Win),
	persoon(Parent, Persno, _Naamy, _Verhinderingeny, _Sexy, _, _, _),
	%ParentWin = win_GetParent(_Win),
	%checkPersnoCrit(ParentWin, CatN, PersNo),
	xSpelsoort(CatN, _MVsv, WedSex, _WTsv, _WTdv),
	withPartner(_Win, Persno, CatN, WedSex, Type, Tgnst),
	retract(opg_t1(Opg,CatN,_,_,_,_,_,_,_,_,_)),
	assert(opg_t1(Opg,CatN,Tgnst,0,0,0,0,"","",na,"")),
	findall(Onderdeel, sp_opgavent(_Win, Persno,_, Onderdeel, _, b_False), Onderdelen),
	%UitWin = win_GetCtlHandle(_Win, idc_opgegevenlb),
	lbox_Clear(Lbox),
	lbox_Add(Lbox, Onderdelen),
	lbox_SetSel(Lbox, Index, b_True),
	!.
%END Opgeven, idc_verander_partner _CtlInfo

%BEGIN Opgeven, e_Destroy
  dlg_opgeven_eh(_Win,e_Destroy,0):-!,
	retractall(opg_t1(_,_,_,_,_,_,_,_,_,_,_)),
	!.
%END Opgeven, e_Destroy

%BEGIN Opgeven, idc_opgegevenlb selchanged
  dlg_opgeven_eh(_Win,e_Control(idc_opgegevenlb,_CtrlType,LboxWin,selchanged),0):-
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	fronttoken(Item, "U", _),
	UWin = win_GetCtlHandle(_Win, idc_uitsluiten),
	win_SetState(UWin, [wsf_enabled]),
	win_SetText(UWin, "Toelaten"),
	fail.
  dlg_opgeven_eh(_Win,e_Control(idc_opgegevenlb,_CtrlType,LboxWin,selchanged),0):-
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	not(fronttoken(Item, "U", _)),
	UWin = win_GetCtlHandle(_Win, idc_uitsluiten),
	win_SetState(UWin, [wsf_enabled]),
	win_SetText(UWin, "Uitsluiten"),
	fail.
  dlg_opgeven_eh(_Win,e_Control(idc_opgegevenlb,_CtrlType,LboxWin,selchanged),0):-
	dialog_SetState(_Win, [enable(idc_opgevenaf, b_True),enable(idc_details, b_True)]),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	searchchar(Item,'\t',Pos),
	P1 = Pos - 1,
	frontstr(P1, Item, NaamKort, _),
	stripU(NaamKort, NaamKortS),
	wc(CatN, _, _, _Naam, Type, NaamKortS, _, _, _, _, _, _, _, _, _),
	%wc(CatN,_Naam,Type,NaamKortS,_SpCatL,_,_),
	not(Type = e), 
	dialog_SetState(_Win,[enable(idc_verander_partner, b_True)]),
	Parent = win_GetParent(_Win),
	persoon(Parent, SpNo, _Naamy, _Verhinderingeny, _Sex, _, _, _),
	sp2opgt(SpNo,CatN,_OpgNo,MaatNo,_,_),
	CtrlWin = win_GetCtlHandle(_Win, idc_verander_partner),
	veranderTekst(CtrlWin, MaatNo),
	!.
  dlg_opgeven_eh(_Win,e_Control(idc_opgegevenlb,_CtrlType,_LboxWin,selchanged),0):-
	dialog_SetState(_Win,[enable(idc_verander_partner, b_False)]),
	!.
%END Opgeven, idc_opgegevenlb selchanged

%BEGIN Opgeven, idc_opgeven_lbox selchanged
  dlg_opgeven_eh(_Win,e_Control(idc_opgeven_lbox,_CtrlType,LboxWin,selchanged),0):-
	lbox_GetSelIndex(LboxWin),
	dialog_SetState(_Win, [enable(idc_opgevenbij, b_True)]),
	!.
  dlg_opgeven_eh(_Win,e_Control(idc_opgeven_lbox,_CtrlType,_LboxWin,selchanged),0):-
	dialog_SetState(_Win, [enable(idc_opgevenbij, b_False)]),
	!.
%END Opgeven, idc_opgeven_lbox selchanged

%BEGIN Opgeven, idc_opgevenaf _CtlInfo
  dlg_opgeven_eh(_Win,e_Control(idc_opgevenaf,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	Lbox  = win_GetCtlHandle(_Win, idc_opgegevenlb),
	Index = lbox_GetSelIndex(Lbox),
	Item  = lbox_GetItem(Lbox, Index),
	Parent = win_GetParent(_Win),
	persoon(Parent, Persno, _Naamy, _Verhinderingeny, Sex, _, _, _),
	sp_opgavent(_Win, Persno, CatN, Onderdeel, Tgnst, b_False),
	Item = Onderdeel,
	alsIngedeeld(CatN),	% check of het kan ....
	retractall(opg_t1(_, CatN, Tgnst, _, _, _, _, _, _, _, _)),
	findall(OnderdeelXX, sp_opgavent(_Win, Persno,_, OnderdeelXX, _, b_False), Onderdelen),
	lbox_Clear(Lbox),
	lbox_Add(Lbox, Onderdelen),
	InWin = win_GetCtlHandle(_Win, idc_opgeven_lbox),
	PWin = win_GetParent(_Win),
	findall(Cat1,sex_cat(PWin, Persno,Sex,Cat1),INAANMERKINGLIJST),
	lbox_Clear(InWin),
	lbox_Add(InWin, INAANMERKINGLIJST),
	dialog_SetState(_Win, [enable(idc_opgevenbij, b_False),
		enable(idc_opgevenaf, b_False),enable(idc_verander_partner,b_False),
		enable(idc_uitsluiten,b_False),enable(idc_details,b_False)]),
	!.

%END Opgeven, idc_opgevenaf _CtlInfo

%BEGIN Opgeven, idc_opgevenbij _CtlInfo
  dlg_opgeven_eh(_Win,e_Control(idc_opgevenbij,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	Lbox  = win_GetCtlHandle(_Win, idc_opgeven_lbox),
	lbox_GetSel(Lbox,_,L1),
	not(L1 = []),
	repeat,
	  lbox_GetSel(Lbox,_ItemL1,IndexL1),
	  IndexL1 = [Index|_],
	  NaamKort = lbox_GetItem(Lbox, Index),
	  wc(CatN, _, _, _, Type, NaamKort, _, _, _, _, _, _, _, _, _),
	  ParentWin = win_GetParent(_Win),
	  persoon(ParentWin, Persno, _Naam, _Verhinderingen,_Sex, _, _, _),
	  xSpelsoort(CatN, _MVsv, WedSex, _WTsv, _WTdv),
	  metPartner(_Win, Lbox, Index, CatN, WedSex, Type, PersNo),
	  lbox_GetSel(Lbox,_,IndexList),	% opnieuw kijken of er nog "bij"-s zijn
	IndexList = [],
	dialog_SetState(_Win, [enable(idc_opgevenbij, b_False),enable(idc_opgevenaf, b_False)]),
	!.
%END Opgeven, idc_opgevenbij _CtlInfo

%BEGIN Opgeven, idc_cancel _CtlInfo
  dlg_opgeven_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	win_Destroy(_Win),
	!.
%END Opgeven, idc_cancel _CtlInfo

%BEGIN Opgeven, e_Create
  dlg_opgeven_eh(_Win,e_Create(_CreationData),0):-!,
	Parent = win_GetParent(_Win),
	persoon(Parent, Persno, Naam, _Verhinderingen, Sex, _, _, _),
	%sp(Persno, _Sexll, Naam, _Tel, _Verh, _Offset, _, _Uitkant), 
	initopg(Persno),
	OuterRCT = win_GetOuterRect(_Win),
	ClientRCT = rect_GetClient(dlg_opgeven_Flags, b_False, OuterRCT),
	RCT = rect_Offset(ClientRCT, -40, -40),
	win_Move(_Win, RCT),
	PWin = win_GetParent(_Win),
	findall(Onderdeel, sp_opgavent(PWin, Persno,_, Onderdeel, _, b_True), Onderdelen),
	LboxWin = win_GetCtlHandle(_Win, idc_opgegevenlb),
	lbox_SetTabStops(LboxWin, [50, 60, 80, 90]),
	lbox_Add(LboxWin, Onderdelen),
	%NaamWin = win_GetCtlHandle(_Win, idct_opgpersoonsnaam),
	format(Naam1, "Deelname van %s", Naam),
	%win_SetText(NaamWin, Naam),
	win_SetText(_Win, Naam1),
	PWin = win_GetParent(_Win),
	findall(Cat1,sex_cat(PWin, Persno,Sex,Cat1),INAANMERKINGLIJST),
	InWin = win_GetCtlHandle(_Win, idc_opgeven_lbox),
	lbox_SetTabStops(InWin, [40]),
	lbox_Add(InWin, INAANMERKINGLIJST),
	!.
%END Opgeven, e_Create


  dlg_opgeven_eh(_,_,_):-!,fail.

%END_DLG Opgeven


%BEGIN_TLB Formulierbar, 21:46:36-16.6.2012, Code automatically updated!
/**************************************************************************
	Creation of toolbar: Formulierbar
**************************************************************************/

clauses

  tb_formulierbar_Create(_Parent):-
ifdef use_tbar
	toolbar_create(tb_bottom,0xC0C0C0,_Parent,
		[tb_ctrl(idt_op,pushb,idb_ow,idb_opd,idb_grijs,"Op;Op",1,1),
		 tb_ctrl(idt_neer,pushb,idb_neerw,idb_neerp,idb_grijs,"Neer;Neer",1,1),
		 separator,
		 tb_ctrl(id_Speler_Oproepen_lijst,pushb,idb_waarsw,idb_waarswd,idb_grijs,"Roep op;Spelers oproepen (informeren)",1,1),
		 tb_ctrl(id_Speler_wedstrijden,pushb,idb_zoekopw,idb_zoekopp,idb_grijs,"Zoek speler (F3);Wijs speler aan in schema",1,1),
		 tb_ctrl(id_email1,pushb,idb_emailw,idb_emaild,idb_grijs,"Email;Stuur mail",1,1),
		 tb_ctrl(idt_downloadm,pushb,idb_downloadm,idb_downloadd,idb_grijs,"Bekiijk verzonden emails;Bekijk verzonden emails",1,1),
		 tb_ctrl(id_Onderdeel_print,pushb,idb_printw,idb_printdown,idb_nieuwe_speler_disabled,"Afdruk;Spelergegevens afdrukken",1,1),
		 tb_ctrl(id_Speler_checken,pushb,idb_knltb,idb_knltb,idb_grijs,"Spelerprofiel;Spelerprofiel",0,1),
		 tb_ctrl(idb_wis,pushb,idb_wisw,idb_wispressed,idb_grijs,"Persoon wissen;Persoon wissen tenzij deelnemer",1,1)]),
enddef
	true.
%END_TLB Formulierbar


%BEGIN_DLG Tidsynchronisatie
/**************************************************************************
	Creation and event handling for dialog: Tidsynchronisatie
**************************************************************************/

constants

%BEGIN Tidsynchronisatie, CreateParms, 11:14:15-23.11.2014, Code automatically updated!
  dlg_tidsynchronisatie_ResID = idd_tidreflectie
  dlg_tidsynchronisatie_DlgType = wd_Modeless
  dlg_tidsynchronisatie_Help = idh_afvalschema
%END Tidsynchronisatie, CreateParms

predicates

  dlg_tidsynchronisatie_eh : EHANDLER
  dlg_tidsynchronisatie_update(DIALOG_VAL_LIST)
  determ checkPWresult(string, string)
  nondeterm toernooinummerPlus(string)
  determ checkCert(string Tid, string Cert, string PW)
  procedure certIsGeldig(integer Verschil, string Formatstring)

clauses
checkPWresult(In, _Wachtwoord) :-
  not(searchstring(In,"password?",_)), !.
checkPWresult(_In, Wachtwoord) :-
  format(Msg, "Ongeldig wachtwoord: %s\nNB Iedere keer dat u het certificaat downloadt hoort daar een nieuw wachtwoord bij!",Wachtwoord),
  _A = dlg_MessageBox( "Wachtwoord?", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.

certIsGeldig(Verschil, "%s - %s toernooi - CERTIFICAAT VERDACHT!! ") :-
  Verschil < 100,
  _Answer = dlg_MessageBox( "Certificaat", "Certificaat is verdacht!\nVoer een vers certificaat in met \"(opnieuw) Koppelen\"" , mesbox_iconerror, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), !.
certIsGeldig(Verschil,FStr) :-
  format(FStr,"%%s - %%s toernooi - certificaat is nog %d dagen geldig", Verschil).

toernooinummerPlus(TidPlus) :-
  providerX(Prov,_,"",_),
  format(URL,"http://%s/knltbproxy2015/IsJuisteCertificaatEr.php",Prov),
  get_TempDir(_,TempFile),
  toernooiK(Tid, Srt),
  deletefile(TempFile),
  Vars = aanmeldenVars(),
  _Return = downloadData(URL, TempFile, ["certcheck", Tid | Vars], _HttpResponsecode, 0),
  file_str(TempFile, GeldigTM),
  upper_lower(GeldigTM, Lower),
  not(searchstring(Lower,"niet",_)),
  trim_c(GeldigTM),
  dt_datestr_to_offset(GeldigTM, "%Dd-%Md-%YL", Offset),
  %write("\n", Offset),
  date(Year,Month,Day),
  dt_date_to_offset(Year,Month,Day, Huidig),
  Verschil = Offset - Huidig,
  certIsGeldig(Verschil, FormatString),
  term_str(toernooisoort, Srt, Str),
  format(TidPlus, Formatstring, Tid, Str).

 checkCert(Tid, Cert, PW) :-
  fronttoken(Cert, _,_),
  fronttoken(PW, _, _),
  fronttoken(Tid, _, _),
  providerX(Prov,_,"",_),
  format(URL,"http://%s/knltbproxy2015/CertificaatImport1.php",Prov),
  trim_c(Pw),
  trim_c(Tid),
  Vars = aanmeldenVars(),
  Report = "certificaatupload.txt",
  _ReturnK = fileupload(URL, Cert, Report, ["ww",Pw, "tid", Tid | Vars]),
  write("\ncertificaatimport1"),
  write("\n",_ReturnK),
  file_str(Report, Str),
  write("\n",Str),
  checkPWresult(Str, Pw),
  searchstring(Str, "Geluk", _), !. 
checkCert(Tid, Cert, PW) :-
  not(fronttoken(Cert, _,_)),
  not(fronttoken(PW,_,_)),
  fronttoken(Tid, _, _),
  providerX(Prov,_,"",_),
  format(URL,"http://%s/knltbproxy2015/IsJuisteCertificaatEr.php",Prov),
  get_TempDir(_,TempFile),
  deletefile(TempFile),
  Vars = aanmeldenVars(),
  _Return = downloadData(URL, TempFile, ["certcheck", Tid | Vars], _HttpResponsecode, 0),
  file_str(TempFile, GeldigTM),
  upper_lower(GeldigTM, Lower), 
  not(searchstring(Lower,"niet",_)), !.
checkCert(_Tid,_Cert, _Pw) :-
  not(fronttoken(_Tid,_,_)), !,
  Msg = "voer het juiste toernooinummer in!",
  _A = dlg_MessageBox( "Certificaat?", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.
checkCert(_,Cert, _Pw) :-
  not(existfile(Cert)), !,
  Msg = "wijs een geldig certificaat aan op uw PC!",
  _A = dlg_MessageBox( "Certificaat?", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.
checkCert(_Tid,_Cert, _Pw) :-
  Msg = "voer een geldig certificaat/wachtwoord in!",
  _A = dlg_MessageBox( "Certificaat?", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.
  
  dlg_tidsynchronisatie_Create(Parent):-
	findall(TidPlus, toernooinummerPlus(TidPlus), TidList),
	%list_to_string(TidList, "\n", Nrs),
	IDC_TID_VALUE = "",
	%IDCTST_CERTIFICAATFILE_TITLE = "certificaatfile",
	IDCST_WACHTWOORD_VALUE = "",
	IDC_CERTIFICAATBESTAND_VALUE = "certificaat is nog niet aangewezen",
	IDCT_BESCHIKBARE_CERTS_ITEMLIST = TidList,
	%S1 = "NB Richt altijd het toernooi zo vroeg mogelijk in!\n",
	%S2 = "Selecteer een regel en Update definities",
	%concat(S1,S2,S3),
	IDCT_ONDERSTAANDE_IN_TITLE = "",
%MARK Tidsynchronisatie, new variables

	dialog_CreateModeless(Parent,dlg_tidsynchronisatie_ResID,"",
  		[
%BEGIN Tidsynchronisatie, ControlList, 11:14:15-23.11.2014, Code automatically updated!
		df(idct_beschikbare_certs,listbox(IDCT_BESCHIKBARE_CERTS_ITEMLIST,[0]),nopr),
		df(idc_tid,editstr(IDC_TID_VALUE,[]),nopr),
		df(idc_certificaatbestand,editstr(IDC_CERTIFICAATBESTAND_VALUE,[]),nopr),
		df(idcst_wachtwoord,editstr(IDCST_WACHTWOORD_VALUE,[]),nopr),
		df(idct_onderstaande_in,statictext(IDCT_ONDERSTAANDE_IN_TITLE),nopr)
%END Tidsynchronisatie, ControlList
		],
		dlg_tidsynchronisatie_eh,0).

  dlg_tidsynchronisatie_update(_VALLIST):-
%BEGIN Tidsynchronisatie, Update controls, 11:14:15-23.11.2014, Code automatically updated!
	_IDC_TID_VALUE = dialog_VLGetstr(idc_tid,_VALLIST),
	_IDCST_WACHTWOORD_VALUE = dialog_VLGetstr(idcst_wachtwoord,_VALLIST),
	_IDC_CERTIFICAATBESTAND_VALUE = dialog_VLGetstr(idc_certificaatbestand,_VALLIST),
	dialog_VLGetListBox(idct_beschikbare_certs,_VALLIST,_IDCT_BESCHIKBARE_CERTS_ITEMLIST,_IDCT_BESCHIKBARE_CERTS_SELECT),
%END Tidsynchronisatie, Update controls
	checkCert(_IDC_TID_VALUE, _IDC_CERTIFICAATBESTAND_VALUE, _IDCST_WACHTWOORD_VALUE),
	stopdbrefresh(),
	getKNLTBdefinities(_IDC_TID_VALUE,nee,b_True,_),
        compress(update),
        startDbRefresh(),
	fail.
  dlg_tidsynchronisatie_update(_VALLIST):-
	startdbrefresh().
	
%BEGIN Tidsynchronisatie, idc_ok _CtlInfo
  dlg_tidsynchronisatie_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-!,
	VALLIST = dialog_GetValues(_Win),
	dlg_tidsynchronisatie_update(VALLIST),
	!,fail.
%END Tidsynchronisatie, idc_ok _CtlInfo
%MARK Tidsynchronisatie, new events

%BEGIN Tidsynchronisatie, e_Create
  dlg_tidsynchronisatie_eh(_Win,e_Create(_CreationData),0):-!,
	dialog_GetListBox(_Win, idct_beschikbare_certs,_IDCT_BESCHIKBARE_CERTS_ITEMLIST,_IDCT_BESCHIKBARE_CERTS_SELECT),
	_IDCT_BESCHIKBARE_CERTS_ITEMLIST = [Regel | _],
	fronttoken(Regel, Tid, _),
	dialog_SetStr(_Win, idc_tid, Tid),
	dialog_SetState(_Win, [enable(idc_update_definities, b_True), enable(idc_verwijder, b_True)]),
	!.
%END Tidsynchronisatie, e_Create

%BEGIN Tidsynchronisatie, idc_verwijder _CtlInfo
  dlg_tidsynchronisatie_eh(_Win,e_Control(idc_verwijder,_CtrlType,_CtrlWin,_CtlInfo),0):-
	dialog_GetListBox(_Win, idct_beschikbare_certs,_IDCT_BESCHIKBARE_CERTS_ITEMLIST,_IDCT_BESCHIKBARE_CERTS_SELECT),
	_IDCT_BESCHIKBARE_CERTS_ITEMLIST = [Regel | _],
	fronttoken(Regel, Tid, _),
	format(Msg,"Je kunt de KNLTB voorgeprogrammeerde definities loskoppelen van toernooinummer %s.\nEr wordt eerst een herstelpunt (=back-up) gemaakt\nDoorgaan?", Tid),
	1 = dlg_MessageBox( "Loskoppelen onderdeeldefinities", Msg, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
        backup("email", ja),
	stopdbrefresh(),
	findall(WCx, wc(WCx, _, _, _, _, _, _, _, _, _, _, _, _, Tid,_), WCL),
	member(WC, WCL),
	getbacktrack(Here),
	retract(wc(WC, _, N, Naam, EDG, Kort, SexL, SpStrk, LftV, LftTM, HDG, Schemasoort, _Origine, _,Geld)),
	cutbacktrack(Here),
	edgHDGconvert(EDG, HDG, EDG1, HDG1),
	assertz(wc(WC, "", N, Naam, EDG1, Kort, SexL, SpStrk, LftV, LftTM, HDG1, Schemasoort, vanzelf, "",Geld)),
	fail. 
%END Tidsynchronisatie, idc_verwijder _CtlInfo
  dlg_tidsynchronisatie_eh(_Win,e_Control(idc_verwijder,_CtrlType,_CtrlWin,_CtlInfo),0):-
	compress(update),
	startdbrefresh(),
	_Answer = dlg_MessageBox( "Losmaken", "Het toernooibestand is losgekoppeld van het toernooinummer.\nHet certificaat blijft beschikbaar om weer opnieuw te koppelen.", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	win_destroy(_Win), !.

%BEGIN Tidsynchronisatie, idc_update_definities _CtlInfo
  dlg_tidsynchronisatie_eh(_Win,e_Control(idc_update_definities,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	dialog_GetListBox(_Win, idct_beschikbare_certs,_IDCT_BESCHIKBARE_CERTS_ITEMLIST,_IDCT_BESCHIKBARE_CERTS_SELECT),
	_IDCT_BESCHIKBARE_CERTS_ITEMLIST = [Regel | _],
	fronttoken(Regel, Tid, _),
	%write("", Tid),
	stopdbrefresh(),
	getKNLTBdefinities(Tid,nee,b_False,_OK),
	compress(update),
	startdbrefresh(),
	win_destroy(_Win),
	!.
%END Tidsynchronisatie, idc_update_definities _CtlInfo

%BEGIN Tidsynchronisatie, idct_beschikbare_certs selchanged
  dlg_tidsynchronisatie_eh(_Win,e_Control(idct_beschikbare_certs,_CtrlType,_CtrlWin,selchanged),0):-
	dialog_GetListBox(_Win, idct_beschikbare_certs,_IDCT_BESCHIKBARE_CERTS_ITEMLIST,_IDCT_BESCHIKBARE_CERTS_SELECT),
	_IDCT_BESCHIKBARE_CERTS_ITEMLIST = [Regel | _],
	fronttoken(Regel, Tid, _),
	dialog_SetStr(_Win, idc_tid, Tid),
	dialog_SetState(_Win, [enable(idc_update_definities, b_True), enable(idc_verwijder, b_True)]),
	!.
%END Tidsynchronisatie, idct_beschikbare_certs selchanged

%BEGIN Tidsynchronisatie, idct_beschikbare_certs activated
  dlg_tidsynchronisatie_eh(_Win,e_Control(idct_beschikbare_certs,_CtrlType,_CtrlWin,activated),0):-
	dialog_GetListBox(_Win, idct_beschikbare_certs,_IDCT_BESCHIKBARE_CERTS_ITEMLIST,_IDCT_BESCHIKBARE_CERTS_SELECT),
	_IDCT_BESCHIKBARE_CERTS_ITEMLIST = [Regel | _],
	fronttoken(Regel, Tid, _),
	stopdbrefresh(),
	getKNLTBdefinities(Tid,nee,b_True,_OK),
	compress(update),
	startdbrefresh(),
	win_destroy(_Win),
	!.
%END Tidsynchronisatie, idct_beschikbare_certs activated

%BEGIN Tidsynchronisatie, idcst_selecteer _CtlInfo
  dlg_tidsynchronisatie_eh(_Win,e_Control(idcst_selecteer,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	FN = dlg_GetFileName("*.pfx",["pfx","*.pfx"],"Selecteer certificaatbestand",[],"",_OutListFiles),
	dialog_SetStr(_Win, idc_certificaatbestand, FN),
	dialog_setState(_Win, [enable(idcst_selecteer, b_False)]),
	!.
%END Tidsynchronisatie, idcst_selecteer _CtlInfo

  dlg_tidsynchronisatie_eh(_,_,_):-!,fail.

%END_DLG Tidsynchronisatie

