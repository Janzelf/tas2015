/*****************************************************************************

		Copyright (c) 1989 - 1998 De Lint Associates

 Project:  TASVPI
 FileName: TASUITSL.PRO
 Purpose: Verwerk de uitslagen
 Written by: JdL
 Comments:
******************************************************************************/

include "tasvp1.inc"
include "tasvp1.con"
include "hlptopic.con"

include "error.con"
include "datecust\\date_cc.pre"


constants 
  %baanctlbase = 12000
  updatedagstatus = 13	
  %id_DoeNiks  = 11900
  id_Vrijgeven = 11901
  id_Terugtrekken = 11902
  id_Terugtrekken1 = 11903
  id_Banentoolreset = 11904
  id_Uitbreiding = 11905
  %id_Aantalbanen = 11906
  
database - uitslagen
  itemAfmU(WINDOW, integer, integer, integer, integer, integer, integer, integer)
  single ictr(integer Index)
  single maxilen(integer)
  planSelect(window, integer, wedscat Poule)	% 0 = te plannen 1 = alle 2 = uitslagen 3 = per poule 4 = agendamode
  urg(window, integer, wedscat, integer Urgentie)
  verzet(window, integer, wedscat, string Verzet, color, integer Ruilbaar)
  teveelWeds(tijd, integer AantalTeveel)
  %single  tePlannenL(integer HuidigNo, wedscat HuidigWC, integer Tijd, integer NaarNo, wedscat NaarCat, string Guidance, string Aktief)
  single boventekens(integer HuidigeNo, wedscat HuidigeWC)
  single ondertekens(integer AanbevolenTijdstip, integer WNo, integer Wedscat, string Msg) % oordspronkelijke selectie
  bezwColor(integer Tijd , integer Result)
  single huidigeTijd(integer)
  single cursorrect(integer)
  determ timerbezig(window, integer Id)
  hotSpelerSpot(window, integer, wedscat, integer SpNo, rct Area)
  hotBaanSpot(window, integer, wedscat, rct Area)
  single menucontext(integer, wedscat)
  single maxbanenM(integer)
  single maxbaanicoonbreedte(integer, integer)

database - bezw
determ  bezwarenwinTekst(window, integer, wedscat, string TijdStr, string Bezwaren, string Opmerkingen, string OpmVerpl, string OpmerkingenAnder, boolean Changed)	% zie verderop in de code
determ  extratijdstip(window)
determ  notaVeld


predicates

nondeterm   wedstr_in_volg1(window, string, integer Mode) - (i,o,i)
%nondeterm   wdid_wdname(wl,string,integer,wedscat) - (i,o,o,o)
  wedRepr(integer Mode, string Str, string Naam, string Naam2, integer No, wedscat Cat, string D,
      string T, string BaanPark, tijd Tijd, 
      string NaamA1, integer A1, string NaamA2, integer A2, string NaamB1, integer B1, string NaamB2, integer B2)
procedure oproepInd(janee, janee, string)
  inLijst : zoekFunc
  nondeterm letters(tgnst, tgnst, char)
  nondeterm spLetr(tgnst, char)
  gepland1(integer, wedscat, string, integer Mode)
procedure geplandeTijd(string, string Dag, string Tijdstip)
procedure w2proc(integer No, wedscat Cat, janee G1, janee G2, string BaanPark)
%  nondeterm wedsInVolgorde(wedscat, string)
  nondeterm wedsInVolgorde1(integer, wedscat)
  nondeterm catSel(string)
  cat1(rsoort, string)
 % haspisert(integer, integer)
 % haspiserf(integer, integer)
 % emailfout()
nondeterm prog_wedst(wedid, tijd)
nondeterm wedstrijdenOpTijdstip(wl, tijd, integer Aantal, integer , string Str)
procedure indexAgenda(integer No, integer Wedscat, integer Tijd, slist LboxL, integer N0, integer N1)
procedure kleurenBRead(color, color)
determ kleurenBUpdate(color, color)
procedure nieuweIndex(integer Mode, window, slist, integer)
procedure comparevolg(window, slist Nieuw, slist Oud, integer) - (i, i, i, i)
determ compareVolg1(window LWin, tijd Tijd1, slist, tijd Tijd2, slist, integer Index) - (i,i,i,i,i,i)
procedure mode2oproep(integer, janee, janee)
nondeterm wedsUitslInVolg(string Str)
procedure voorvoegsel(integer Tijd, integer No, wedscat Cat, string BaanPark, integer Ix, string Str)
procedure alignAanwezig(window LboxWin)
%determ    baanstadiumUit(string Letter, integer, integer, integer, integer, RESID wit)
procedure doorschuiven(window, integer BaanNr)
procedure terugschuiven(integer BaanNr)
nondeterm doorschuifvolgorde(integer Baan, string Key)
  maxbaanBreedte(Window, integer Breedte, integer Hoogte)
determ spelerblijft(integer, integer, wedscat, boolean MetAanwReg)
%procedure spelersblijven(integer, wedscat, boolean MetAanwReg)
determ zelfdeweds(integer, integer, wedscat, wedscat)
determ laatzien(tijd, tijd)
determ allebeiOpenplaats(tgnst,tgnst)
procedure upd_baantoew(dbasedom)

clauses

getMaxBanen(Max) :-
  maxBanenM(Max).

zelfdeweds(N, N, C, C).


spelersblijven(Num, Cat, MetAanwReg) :-
  banentool(ja, _),
  wd(Num,Cat,T1,T2,_,_,_),
  de_spelers(Num, Cat, T1, T2, Spelers),
  member(Sp, Spelers),
  spelerblijft(Sp, Num, Cat, MetAanwReg),
  fail.
spelersblijven(_Num, _Cat, _).

spelerBlijft(SpNo, No, Cat, _) :-
  spelerAanwezig(SpNo, Tijd, No, Cat),
  logf('R',spelerAanwezig(SpNo, Tijd, No, Cat),nee),
  fail.
spelerBlijft(SpNo, Noz, Catz, b_True) :-
  getbacktrack(Here),
  huidig(DagNu, _),
  %time_stamp_next(_, DagNu, _),
  findall(Tx, bn(Tx, _), TL),
  sorttijd_c(TL),
  member(Tijd, TL),
  w2(No,Cat,Tijd,_,_,_,_,_),
  not(zelfdeweds(No, Noz, Cat, Catz)),
  tijd_kwart(Dag, _Uur, _Kwartier, Tijd),
  Dag = DagNu,
  wd(No,Cat,T1,T2,_U,_TT,_),
  de_spelers(No, Cat, T1, T2, Spelers),
  member(SpNo, Spelers),
  cutbacktrack(Here), 
  sp2(SpNo,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  tijd_str(Tijd, TStr,_),
  wc(Cat, _, _, _, _, CStr, _, _, _, _, _, _, _, _, _),
  format(Msg, "Blijft %s \nnu op het park voor de volgende wedstrijd,\neen %s om %s?", Naam, CStr, TStr),
  1 = dlg_MessageBox( "Vervolg?", Msg, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
  logf('Z',spelerAanwezig(SpNo, Tijd, No, Cat),nee),
  !.  
%spelerBlijft(SpNo, _Noz, _Catz) :-
%  spelerAanwezig(SpNo, Tijd, No, Cat),
%  logf('R', spelerAanwezig(SpNo, Tijd, No, Cat),nee), !.

zetaanwezig(Speler) :-
  not(spelerAanwezig(Speler, _, _, _)),
  huidig(DagNu, _),
  findall(Tx, bn(Tx, _), TL),
  sorttijd_c(TL),
  member(Tijd, TL),
  w2(No,Cat,Tijd,_,_,_,_,_),
  tijd_kwart(Dag, _Uur, _Kwartier, Tijd),
  Dag = DagNu,
  wd(No,Cat,T1,T2,_U,_TT,_),
  de_spelers(No, Cat, T1, T2, Spelers),
  member(Speler, Spelers),
  not(speleraanwezig(Speler, Tijd, No, Cat)),
  logf('Z',spelerAanwezig(Speler, Tijd, No, Cat),nee),
  !.
zetaanwezig(_Speler).

alignAanwezig(_) :-
  not(banentool(ja, _)), !.
alignAanwezig(_Win) :-
  baantoew(No, Cat, Baantoew),
  not(w2(No,Cat,_,_,_,_,_,_)),
  logf('R',baantoew(No, Cat, Baantoew), ja),
  Baantoew = baan(I, _),			% er is een baan vrij gekomen
  doorschuiven(_Win, I),
  fail.
alignAanwezig(LboxWin) :-
  hotbaanspot(LboxWin, No, Cat, _),
  not(w2(No,Cat,_,_,_,_,_,_)),
  retractall(hotbaanspot(LboxWin, No, Cat, _)),
  baantoew(No, Cat, Baantoew),
  logf('R',baantoew(No, Cat, Baantoew), ja),
  fail.
alignAanwezig(_Win) :-
  logclose().

doorschuiven(_, I) :-	% naar baan I
  not(baantoew(_, _, baan(I, _))),
  findall(Tijd, doorschuifvolgorde(I, Tijd), Tijden),
  sortstring_c(Tijden, 1),
  Tijden = [TijdFirst|_], 
  parseTabs(TijdFirst, StrList),
  StrList = [_, NoS, CatS | _],
  str_int(NoS, No),
  str_int(CatS, Cat),
  retract(baantoew(No, Cat, reserveren(I, T))),
  upd_baantoew(baantoew(No, Cat, baan(I, T))), !.
  %logf('A',baantoew(No, Cat, baan(I, T)), nee), !.
doorschuiven(_, _I).

terugschuiven(I) :-  % van baan I
  baantoew(No, Cat, baan(I, T)), !,
  logf('A', baantoew(No, Cat, reserveren(I, T)), nee).
terugschuiven(_I).  % van baan I

maxbanenM(1).
ictr(0).
maxilen(0).
%tePlannenL(0, 0, 0, 0, 0, "", "").
boventekens(0, 0).
ondertekens(0, 0, 0, "").
huidigeTijd(0).
cursorrect(0).

mode2oproep(4, _, nee) :- !.
mode2oproep(2, _, nee) :- !.
mode2oproep(0, _, nee) :- !.
mode2oproep(_, Oproep, Oproep).

kleurenBRead(A, B) :-
  kleurenB(A, B), !.
kleurenBRead(color_Yellow, color_cyan).

kleurenBUpdate(A, B) :-
  retractall(kleurenB(_, _)),
  assert(kleurenB(A, B)).

wedsInVolgorde1(Num, Cat) :-
  pos(Cat, Card, _, _),
  wd(Num,Cat,_Winnaar,_T2,_U,_TT,_),
  Num >= Card.

catSel(CatStr) :-         
  wc(CatNo, _, _, Cat, _, _, _, _, _, _, _, _, _, _, _),
  %wc(CatNo,Cat,_WT,_,_,_,_),
  getbacktrack(Here),
  pos(CatNo,_,_,RSoort),
  RSoort = poel,		% alleen de ingedeelde poeltjes
  cutbacktrack(Here),
  cat1(RSoort, Str),
%  concat(Str, Cat, CatStr).
  findall(OpgaveNum, opg(OpgaveNum,CatNo,_,_,_,_,_,_,_,_,_),OpgL),
  count(OpgL, 0, Aant),
  format(CatStr, "%s\t%3d\t%s", Str, Aant, Cat).

cat1(afval, " ") :- !.
cat1(poel, "p").

planOndSeq(Win) :-			% plan per poule
  findall(Ond,   catSel(Ond), OndList),
  dlg_onderdeel_select_Create(Win, "Selecteer onderdeel",OndList, 0, Selectie, _),
  Selectie = [CurrOnd|_],
  %dlg_onderdeelplan_Create(Win, CurrONd), !.
  searchchar(CurrOnd, '\t', P1),
  frontstr(P1,CurrOnd,_,S1),
  searchchar(S1,'\t',P2),
  frontstr(P2,S1,_,Onderdeel),
  wc(Cat, _, _, Onderdeel, _, _, _, _, _, _, _, _, _, _, _),
  %wc(Cat,Onderdeel,_,_CatNam,_,_,_), 
  pos(Cat, _, Card, _), 
  select(Cat, Card),
  win_uitslageninvoer_Create(Win, 3, Cat), !.
  
uitsl_verwrk(Parent) :-
  %not(nonetw()),
  getbacktrack(Here),
  w2(_No,_Cat,_Tijd1,_,_,_,_,_), 
  cutbacktrack(Here),
  win_uitslageninvoer_Create(Parent, 2, 0), !.
uitsl_verwrk(_Parent) :-
  dlg_MessageBox( "Uitslagen invoeren", "Er zijn geen of nog geen geplande wedstrijden.\nAls u toch uitslagen wilt invoeren, klik dan met de rechtermuisknop op een wedstrijd.", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  !, fail.

gepland1(No, Cat, "x", _) :-
  w3(No,Cat,_,_,_,_,_), !.			% gespeeld
gepland1(No, Cat, "?", _) :-
  not(w2(No,Cat,_,_,_,_,_, _)),  !.

%  baantoew = gespeeldop(integer) baan(integer) ; open; aanwezig(string)
%  baantoew( integer, wedscat, baantoew)
voorvoegsel(Tijd, No, Cat, _Baan, _Ix, Str) :-
  banentool(ja, _),
  str_int(S, Tijd),
  str_len(S, Len),
  str_Len(X, Len),
  baantoew(No, Cat, gespeeldop(I, _)),
  %helestr_int(0, StrI, I),
  format(Str, "%s%s        ", X, I), !. 
voorvoegsel(Tijd, No, Cat, _Baan, _Ix, Str) :-
  banentool(ja, _),
  baantoew(No, Cat, baan(I, _)),
  %helestr_int(0, StrI, I),
  format(Str, "%u99%s      ", Tijd, I), !. 
voorvoegsel(Tijd, No, Cat, _Baan, _Ix, Str) :-
  banentool(ja, _),
  baantoew(No, Cat, reserveren(I,T)),	% baan, tijdstip
  %helestr_int(0, StrI, I),
  format(Str, "%u99%s%s", Tijd, I, T), !. 
voorvoegsel(Tijd, No, Cat, _Baan, _Ix, Str) :-
  banentool(ja, _),
  baantoew(No, Cat, aanwezig(T)),
  format(Str, "%u99Z%s", Tijd, T), !.
voorvoegsel(Tijd, _No, _Cat, Baan, Ix, Str) :-
  baan_adm(ja), !,
  format(Str, "%u%-9s%3uZZZZ", Tijd, Baan, Ix).
%  format(Str, "%uZZZZZZZZZZZZZZZZZ", Tijd, Baan).
voorvoegsel(Tijd, _No, _Cat, _Baan, Ix, Str) :-
  format(Str, "%uZZZZZZZZZ %3uZZZZ", Tijd, Ix).

allebeiOpenplaats(openplaats(_),openplaats(_)).

wedsUitslInVolg(Str) :-	% 24.2.2012
  assert(ictr(0)),
  wc(Cat, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
  w2(No,Cat,Tijd1,_,_,_,_,BaanPark),
  wd(No,Cat,T1,T2,_,_,_),
  not(allebeiOpenplaats(T1, T2)),
  %not(T1 = openplaats(_)),
  %not(T2 = openPlaats(_)),
  ictr(I),
  I1 = I  + 1,
  assert(ictr(I1)),
  voorvoegsel(Tijd1, No, Cat, BaanPark, I, VV),
  format(Str, "%s\t%\t%\t%\t\t", VV, No, Cat, Tijd1).
%write("\n", Str).

wedstr_in_volg1(_Win, Str, 2) :-		% 2 = uitslagen
  findall(S0, wedsUitslInVolg(S0), SL),
  sortstring_c(SL, 1),
  member(S1, SL),
  parseTabs(S1, EList),
  EList = [_, No, Cat, Tijd1 | _],
  format(Str, "%\t%\t%\t\t", No, Cat, Tijd1). 
wedstr_in_volg1(_Win, Str, Mode) :-		% 0 = te plannen 1 = alle
  Mode < 2,
  %time_stamp_next(BeginTijd,  _DagNo, _Tijd),
  %Eind = BeginTijd + optimconst,
  findall(Catn,pos(Catn, _, _, _),Catl),	% 1 = alle
  sortwed_c3(1, CatL, b_False/*	 28.9.2003*/, Num, Cat, _Urg, _Aantal),% nondeterm
  retractall(spelerdrukte(_,_,_,_,_,_,_,_,_)),
  gepland1(Num, Cat, Plan, Mode),
  format(Str, "%\t%\t%\t%\t%", Num, Cat, Plan, _Urg, _Aantal).
wedstr_in_volg1(Win, Str, 3) :-		% 3 =  ??
  planSelect(Win, 3, Cat),
  findall(Num, wedsInVolgorde1(Num, Cat), List),
  sortint_c(List),
  member(WedsNum, List),
  gepland1(WedsNum, Cat, Plan, 1),
  format(Str, "%\t%\t%\t\t", WedsNum, Cat, Plan).
wedstr_in_volg1(Win, Str, 4) :-		% 4 = volgens agenda
  time_stamp_next(Huidig, _, _),
  assert(huidigeTijd(Huidig)),
  planSelect(Win, 4, _),
  findall(Tijdx, bn(Tijdx, _),Tijden),
  sortint_c(Tijden),
  %Tijden = [Tijdeerste | _],
  %write("\n", Huidig, " ", Tijdeerste),
  member(Tijd, Tijden),
  laatzien(Huidig, Tijd),
  bn(Tijd, Aantal),
  findall(WedId, prog_wedst(WedId, Tijd), Wedstrijden),
  count(Wedstrijden, 0, AantalWeds),
  wedstrijdenOpTijdstip(Wedstrijden, Tijd, Aantal, AantalWeds, Str).

%laatzien(_Huidig, _Tijd) :- !.
laatzien(Huidig, Tijd) :-
  Tijd >= Huidig, !.
laatzien(_Huidig, Tijd) :-
  w2(_,_,Tijd,_,_,_,_,_), !.  % er staat nog een oude geplande wedstrijd op dat tijdstip

wedstrijdenOpTijdstip(_Wedstrijden, Tijd, 0, 0, Str) :-
  format(Str, "%\t%\t%\t%\t%", 0, 0, Tijd, 0, 0).	% 30.12.2002
wedstrijdenOpTijdstip(_Wedstrijden, Tijd, AantalBesch, AantalBezet, Str) :-
  AantalBezet < AantalBesch,
  Vrij = AantalBesch - AantalBezet,
  format(Str, "%\t%\t%\t%\t%", 0, 0, Tijd, Vrij, AantalBezet).	% 1.11.2002
wedstrijdenOpTijdstip(_Wedstrijden, Tijd, AantalBesch, AantalBezet, "") :-
  AantalBezet > AantalBesch,
  OverLoop = AantalBezet - AantalBesch,
  assert(teveelWeds(Tijd, Overloop)),
  fail.
wedstrijdenOpTijdstip(Wedstrijden, Tijd, _Aantal, _, Str) :-
  member(Weds, Wedstrijden),
  Weds = w(WedNo,Cat,_Kwart,_Janee,_,_Onbekend,_, _),
  format(Str, "%\t%\t%\t\t", WedNo, Cat, Tijd).

compareVolg(_LWin, [], [], _) :- !.
compareVolg(LWin, [Str1|Rest1], [Str2|Rest2], N) :-
  not(Str1 = Str2),
  %write("\n", Str1, " N:",N, " ", Str2),
  fronttoken(Str1, _, R1),
  fronttoken(Str2, _, R2),
  fronttoken(R1, _Cat1, R11),
  fronttoken(R2, _Cat2, R21),
  fronttoken(R11, Tijd1S, _),
  fronttoken(R21, Tijd2S, _),
  str_int(Tijd1S, Tijd1),
  str_int(Tijd2S, Tijd2),
  compareVolg1(LWin, Tijd1, [Str1|Rest1], Tijd2, [Str2|Rest2], N), !.
compareVolg(LWin, [_|Rest1], [_|Rest2], N) :-
  N1 = N + 1, !,
  compareVolg(LWin, Rest1, Rest2, N1).
compareVolg(LWin, [Str1|Rest1], [], N) :-
  lbox_Add(LWin, -1, Str1),
  N1 = N + 1, !,
  compareVolg(LWin, Rest1, [], N1).
compareVolg(LWin, [], [_|Rest2], N) :-
  lbox_Delete(LWin, N), !,
  compareVolg(LWin, [], Rest2, N).
compareVolg(_, _L1, _L2, _).

compareVolg1(LWin, Tijd1, [Str1|Rest1], Tijd2, Rest2, N) :-
  Tijd1 < Tijd2, !,
  lbox_Add(LWin, N, Str1),
  N1 = N + 1,
  compareVolg(LWin, Rest1, Rest2, N1).
compareVolg1(LWin, Tijd1, [Str1|Rest1], Tijd2, [_|Rest2], N) :-
  Tijd1 = Tijd2, !,
  lbox_Delete(LWin, N),
  %write("\ndelete:", N), 
  lbox_Add(LWin, N, Str1),
  %write("\nadd:", N, " ", Str1),
  N1 = N + 1,
  compareVolg(LWin, Rest1, Rest2, N1).
compareVolg1(LWin, _Tijd1, Str1, _Tijd2, [_|Rest2], N) :-
  %Tijd1 > Tijd2, !,	impliciet
  lbox_Delete(LWin, N),
  compareVolg(LWin, Str1, Rest2, N).
   
prog_wedst(w(WedNo,Cat,Kwart,nee,0,onbekend,b_False, "BaanPark"),Kwart) :-
  wc(Cat, _, _, _, _, _, _, _, _, _, _, _, _, _, _),	% in volgorde van categorie 
  w2(WedNo,Cat,Kwart,_,_,_,_,_).

oproepInd(ja, nee, "?") :- !.
oproepInd(_, _, "").

geplandeTijd(TijdRS, DStr, TStr1) :-
  str_int(TijdRS, Tijd),
  tijd_kwart(Dag,_,_,Tijd),
  dag(Dag,DStr,_), 
  tijd_str(Tijd,TStr1,_), !.
geplandeTijd("x", "gespeeld", "") :- !.
geplandeTijd(_, "??", "").

w2proc(No, Cat, G1, G2, BaanPark) :-
  w2(No,Cat,_,G1,G2,_,_,BaanPark), !.
w2proc(_No, _Cat, ja, ja, "").

wedRepr(_, Str, "uitwijktijdstip", "", 0, 0, DStr, TStr, "", Tijd, "uitwijktijdstip", 0, "", 0, "", 0, "", 0) :-
  fronttoken(Str, NoS, R),
  NoS = "0",
  fronttoken(R, CatS, R1),
  CatS = "0", 
  fronttoken(R1, TijdRS, R2),
  %frontstr(Pos2,R2,_,R3),
  geplandeTijd(TijdRS, DStr, TStr),
  str_intP(TijdRS, Tijd),
  %searchchar(R2,'\t',Pos2),
  fronttoken(R2, Plan, R3),
  fronttoken(R3, Aantal, _),
  Plan   = "0",
  Aantal = "0", !.
wedRepr(_, Str, Naam, "", 0, 0, DStr, TStr, "", Tijd, Naam, 0, "", 0, "", 0, "", 0) :-
  fronttoken(Str, NoS, R),
  NoS = "0",
  fronttoken(R, CatS, R1),
  CatS = "0", 
  fronttoken(R1, TijdRS, R2),
  %frontstr(Pos2,R2,_,R3),
  geplandeTijd(TijdRS, DStr, TStr),
  str_intP(TijdRS, Tijd),
  %searchchar(R2,'\t',Pos2),
  fronttoken(R2, Plan, R3),
  fronttoken(R3, Aantal, _),
  Aantal = "0", !,
  format(Naam, "% vrij", Plan).
wedRepr(_, Str, Naam, "", 0, 0, DStr, TStr, "", Tijd, Naam, 0, "", 0, "", 0, "", 0) :-
  fronttoken(Str, NoS, R),
  NoS = "0",
  fronttoken(R, CatS, R1),
  CatS = "0", !,
  fronttoken(R1, TijdRS, R2),
  %frontstr(Pos2,R2,_,R3),
  geplandeTijd(TijdRS, DStr, TStr),
  str_intP(TijdRS, Tijd),
  %searchchar(R2,'\t',Pos2),
  fronttoken(R2, Plan, R3),
  fronttoken(R3, Aantal, _),
  format(Naam, "% vrij, % bezet", Plan, Aantal).
wedRepr(Mode, Str, NaamX1, Naam2, No, Cat, DStr, TStr, BaanPark, Tijd, NaamA1, NrA1, NaamA2, NrA2, NaamB1, NrB1, NaamB2, NrB2) :-
  %parseTabs(Str, List),
  %List = [NoS,CatS,TijdRS|_],
  searchchar(Str,'\t',FoundPos),
  frontstr(FoundPos,Str,_NoS1,R1),
  fronttoken(Str, NoS, _),
  searchchar(R1,'\t',Pos1),
  frontstr(Pos1,R1,_CatS1,R2),
  fronttoken(R1, CatS, _),
  %searchchar(R2,'\t',Pos2),
  fronttoken(R2, TijdRS, _),
  str_int(NoS, No),
  str_int(CatS, Cat),
  geplandeTijd(TijdRS, DStr, TStr),
  str_intP(TijdRS, Tijd),
  wd(No,Cat,A,B,_,_,_),
  w2proc(No,Cat,G1,G2,BaanPark),
  %tijd_kwart(Dag,_,_,Tijd),
  %dag(Dag,DStr,_), !,
  %tijd_str(Tijd,TStr1),
  betaal_adm(BetSig),
  oproepen(Oproep),
  mode2oproep(Mode, Oproep, Oproep1),
  mode2oproep(Mode,BetSig, BetSig1),
  %rep('}',nee,Oproep,nee,0,1,2,No,Cat,A,alleplanning, Naam,_Score,_TijdLog),
  pos(Cat, Top, _, _ ),  		% 
  dim(Top, TopDim),
  dim(No,NoDim),                  	% bepaal de ronde
  repc(nee,'}',nee,Oproep1,BetSig1,0,TopDim,NoDim,No,Cat,A,alleplanning, Naam,_Score,_TijdLog,NaamA1,NaamA2,NrA1,NrA2),
  oproepInd(Oproep1, G1, I1),
  concat(Naam, I1, NaamX),
  concat(NaamX, " ", NaamX1),
  repc(nee,'}',nee,Oproep1,BetSig1,1,TopDim,NoDim,No,Cat,B,alleplanning, Naam1,_Score1,_TijdLog1,NaamB1,NaamB2,NrB1, NrB2),
  oproepInd(Oproep1, G2, I2),
  concat(I2, Naam1, Naam2), !.
wedRepr(_Mode, Str, _NaamX1, _Naam2, No, Cat, _DStr, _TStr, _BaanPark, Tijd, _NaamA1, _NrA1, _NaamA2, _NrA2, _NaamB1, _NrB1, _NaamB2, _NrB2) :-
  parseTabs(Str, List),
  List = [NoS,CatS,_TijdRS|_],
  str_int(NoS, No),
  str_int(CatS, Cat),
  w2(No,Cat,Tijd,B,C,D,E,F),
  not(wd(No,Cat,_,_,_,_,_)),
  write("\nwel w2 niet wd ",No, " ", Cat),
  %retract(w2(No,Cat,Tijd,B,C,D,E,F)),
  logf('R', w2(No,Cat,Tijd,B,C,D,E,F), nee), 
  fail.
  

indexAgenda(0, 0, Tijd, [Tijd1|_], N, N) :-
  fronttoken(Tijd1, NoS, R1),
  str_int(NoS, 0),
  fronttoken(R1, CatS, R2),
  str_int(CatS, 0),
  fronttoken(R2, TijdS, _),
  str_int(TijdS, Tijd), !.			% als ze allebei nul waren is de tijd belangrijk
indexAgenda(No, Cat, Tijd, [Tijd1|_], N, N) :-
  No  > 0,
  Cat > 0,
  fronttoken(Tijd1, NoS, R1),
  str_int(NoS, No),
  fronttoken(R1, CatS, R2),
  str_int(CatS, Cat),		% anders is wedid belangrijk
  fronttoken(R2, TijdS, _),
  str_int(TijdS, Tijd), !.
indexAgenda(No, Cat, Tijd, [_|Rest], N, Uit) :-
  N1 = N + 1, !,
  indexAgenda(No, Cat, Tijd, Rest, N1, Uit).
indexAgenda(_, _, _Tijd, _, _N, 0).

nieuweIndex(4, LboxWin, WedsL1, Index) :-
  %boventekens(No, Cat),
  ondertekens(Tijd, No, Cat, Msg),
  indexAgenda(No, Cat, Tijd, WedsL1, 0, Index),
  Win = win_GetParent(LboxWin),
  toolbar_SetValue(Win, idt_3, text_value(Msg)), !.
nieuweIndex(_, _LboxWin, _WedsL1, 0).

inLijst(Str, Letter) :-
  fronttoken(Str, NoS, R1),
  fronttoken(R1, CatS, _R2),
  str_int(NoS, No),
  str_int(CatS, Cat),
  wd(No,Cat,A,B,_,_,_),
  letters(A, B, Letter).

letters(A, _, Letter) :-
  spLetr(A, Letter).
letters(_, B, Letter) :-
  spLetr(B, Letter).

spLetr(s(SpNo), Letter) :-
  sp2(SpNo,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !,
  %sp1(SpNo,_,Naam,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), !,
  frontchar(Naam, Letter,_).
spLetr(pw(SpNo), Letter) :-
  sp2(SpNo,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !,
  %sp1(SpNo,_,Naam,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), !,
  frontchar(Naam, Letter,_).
spLetr(p(SpNo, _), Letter) :-
  sp2(SpNo,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  %sp1(SpNo,_,Naam,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), 
  frontchar(Naam, Letter,_).
spLetr(p(_, SpNo), Letter) :-
  sp2(SpNo,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !,
  %sp1(SpNo,_,Naam,_,_, _,_,_,_,_, _,_,_,_,_, _,_,_,_), !,
  frontchar(Naam, Letter,_).

%BEGIN_WIN Uitslageninvoer
/**************************************************************************
        Creation and event handling for window: Uitslageninvoer
**************************************************************************/

constants
%BEGIN Uitslageninvoer, CreateParms, 21:58:09-2.11.2002, Code automatically updated!
  win_uitslageninvoer_WinType = w_TopLevel
  win_uitslageninvoer_Flags = [wsf_SizeBorder,wsf_TitleBar,wsf_Close,wsf_ClipSiblings,wsf_Maximize]
  win_uitslageninvoer_RCT = rct(100,80,691,399)
  win_uitslageninvoer_Menu = no_menu
  win_uitslageninvoer_Title = "Invoer uitslagen"
  win_uitslageninvoer_Help = idh_uitslagen_invoeren_f8
%END Uitslageninvoer, CreateParms
%id_Wedstrijd_uitslag_invoeren = 612
%id_Knelpunten = 613
%id_Voortgang = 614
id_Kleur1 = 615
id_Kleur2 = 616
id_KleurR = 617

predicates

  win_uitslageninvoer_eh : EHANDLER
  subclassu_eh : EHANDLER  
  maxDatL(WINDOW, integer Max)
  maxOndL(WINDOW, integer Max)
  maxOndLshort(WINDOW, integer Mode, integer Max)
procedure  maxParkL(WINDOW, integer Max, integer Extraspace, integer ExtraspaceO)
procedure  ownerdraw1(WINDOW, WINDOW, RCT, integer ItemId, COLOR BaanPark, boolean Selected)
procedure   titelU(integer Mode, string Titel, wedscat Poule)
  popUpU(WINDOW, PNT)
procedure checkBaanParkL(WINDOW, WINDOW, string BaanPark, integer TabBaanPark)
procedure get_urg(window Win, integer Mode, window LboxWin, integer T, integer B, integer Tab5, integer No, wedscat Cat, integer Rechts, color)
procedure vensterpositieL(window)
procedure getGradatie1(window Win, tijd Tijd)
procedure getverschuiving(window, integer Mode, integer No1, wedscat Cat1, tijd Tijd)
  create_toolbars(window, integer Mode)
  listbox_Resize(window Win, window LboxWin, integer Mode)
nondeterm  planselectNot4(window Win, integer Mode, integer Poule)
determ   rectAfm(window Win, integer T, integer B, integer R, rct RectDag, rct RectTijd, rct RectPark, rct RectCat, integer TUrg, integer TGradatie, rct RectNaam1, rct RectNaam2, integer ParkL)
  bepaalKolommen(window Win)
procedure  opm(integerlist, string, string) % opmerkingen van spelers
procedure nogeenPunt(string In, string Uit)
procedure opmMinEmail(string Opm, string Uit)
%procedure opmMinImport(string Opm, string Uit)
procedure opmMinWas(string Opm, string Uit)
procedure baanNietVrij(integer Tijd, integer Result, integer VNo1, wedscat VCat1, string Tekst, string Opmerkingen2) % als er geen baan vrij is, kijk naar ander(en) 0x08
 planDetails(window Win, integer Mode, integer Aangewezen, wedscat, tijd, integer Betreffende, wedscat Cat1)
procedure eerstvolgendeVoorDeAndere(tijd, integer No, wedscat Cat, integerlist SpelerS, string Tekst)
procedure verschuiving(integer No, wedscat Cat, string, color Reserve, integer Ruilbaar)
procedure negatief(integer Diff, string, color)
procedure kleuren(window Win, boolean Selected, integer No, wedscat Cat, integer Tijd, color Pen, color Fore, color Back1, color Back2, color Brush, color Park)
nondeterm dag_tijd(integer, integer)
determ gefixeerdAlsBezwaren(window, integer, wedscat, integer Result, char Fix)
procedure teveelWedstrijden(window LboxWin, integer Tijd, color BackColor, color Uit)
procedure toggleFixeren(char Fixeren, char FixerenUit)
procedure streepjedag(window LboxWin, integer ItemId, integer Tijd, rct RectDag, rct RectTijd)
procedure streepjeTijd(window, integer Tijd, integer TijdN, rct RctTijd)
procedure zonnetje(window, integer, integer)
procedure schort_op2(integer,wedscat,schl) - (i, i, o)
procedure herstel(schl)
procedure isTeRuilen(integer No, wedscat Cat,schl SchortList, string RuilTekst, integer Result)
nondeterm  transactie(integer No,wedscat Cat,integer OplTijd,char Markeer, integer TT, janee G1, janee G2, integer OudeTijd)
nondeterm  transactie1(dbasedom)
procedure update_urg(window)
 update_top(window)
procedure update_bottom(window)
procedure herstelselectie(window LboxWin, slist Vorig)
determ selectNieuw(window LboxWin, string N, slist, integer I)
procedure update_top1(string CatNam, string Naam, string Naam1, string Tekst)
procedure ookDeTePlannen(integerlist SpelersList, integer Num, wedscat Cat, integerlist SpelersListU)
procedure zetVensterFont(integer, window LboxWin)
procedure drawNamenInRect(integer Mode2, janee Banentool, integer, wedscat, window LboxWin, rct Area, string Namen,
                 string NaamA1, integer NrA1, string NaamA2, integer NrA2, integer RechtsLinks,
                  color BackGround, color Foreground, boolean Selected)	 % 0 = rechts 2= alleen uitslagen
procedure aanstrepen(integer Mode, janee Banentool, integer, wedscat, integer NrA1, color BackGround, color BackGround1, color Foreground, color Foreground1, boolean Selected)
procedure aanwezigNaam(string NaamA1, integer SpNo, string NaamA1a)	% spatie voor en achter als aanwezig
procedure drawBaanSpot(integer Mode, janee Banentool, window LboxWin, integer No, wedscat Cat, rct, rct UitBreedte)	% banentoolicoon
procedure baanKandidaat(integer No1, wedscat Cat1)
determ    spelersAanwezig(tgnst, tgnst)
procedure drawMyIcon(Window, string Text, integer L, integer T, color OpKleur, color LetterKleur, color RandKleur, RCT RectUit)

determ    aanwezig(integer)
procedure verwijderFantoomSpots(window LboxWin, integer Mode, janee BanenTool, integer T, integer B)
procedure banenPopup(integer Max, integer N, MENU_ITEM_LIST)
procedure selectIcon(integer BaanNr, color VrijBezetKlaar, color Tekstkleur)
procedure te_betalen(integer)
procedure setHotSpelerSpot(integer Mode, janee Banentool, window LboxWin, integer No, wedscat Cat1, integer SpelerNo, rct)
procedure metKNLTBnummer(integer SpNo, janee Openbaar)
procedure overplanning(tijd, string)
procedure cleanSpelerAanwezig()
procedure    betalingMsg(janee Betaaladm, janee Betaald, janee BetaaldUit, string Naam, string Bondsnummer, integer SpNo)
procedure tshirtmelding(janee, janee,string)
%procedure eerstAanwezig(janee Algemeld, string Msg)
procedure changeMelding(integer SKeus, janee Betaald, janee Gemeld)
determ changed(janee, janee, janee, janee) 
determ completepartij(window)
nondeterm uitslsp(window, slist, integer Sp)
clauses

menucontext(0, 0).

checkcompleet() :-
  planselect(Win, 2, _), 
  win_PostEvent(Win,e_User(11,0)), !.
checkcompleet().

titelU(0, "Overzicht van de nog te plannen wedstrijden", _) :- !.
%titelU(1, "Overzicht van alle komende wedstrijden (gepland en ongepland)", _) :- !.	% 5.12.2002
titelU(2,  "Aanwezigheidsregistratie, baanuitgifte en invoer uitslagen (F8)",_) :- 
  banentool(ja, _), !.
titelU(2, "Uitslagen invoeren (F8)", _) :- !.
titelU(3, Titel, Poule) :-
  wc(Poule, _, _, Naam, _, _, _, _, _, _, _, _, _, _, _),
  format(Titel, "Plan poule - %s", Naam), !.
titelU(4, "Toernooiagenda", _) :- 
  boventekens(0,0), !.
titelU(4, "Plan een wedstrijd", _) :-  !.
titelU(_, "??", _).

update_uitslagen(_XY) :-          % ########## kan aangezet worden #################################################
  upd_formulier(),
  update_planinvoer(),		
  planSelect(LboxWin, _, _), % is een fact
  alignaanwezig(LboxWin),   % ###########################################################
  win_Invalidate(LboxWin),
  banentool_ververs(),
  update_betalingen(), !.
update_uitslagen(_).

update_planinvoer() :-
  planSelect(_LboxWin, 4, _),
  retract(timerBezig(_Win, TimerId)),
  timer_Kill(TimerId),
  fail.
update_planinvoer() :-
  planSelect(LboxWin, 4, _),
  win_Invalidate(LboxWin), !.
update_planinvoer().

uitslagen_top() :-
  planSelect(Win, 2, _),
  LboxWin = win_GetCtlHandle(Win, idci_lbagenda),
  lbox_SetSel(LboxWin, 0, b_True), !.
uitslagen_top().
  
maxDatL(Win, _) :-
  assert(maxiLen(0)),
  dag(_, DagN, _),
  win_GetTextExtent(Win, DagN, -1, Width, _Height),
  maxiLen(CurMaxL),
  Width > CurMaxL,
  assert(maxiLen(Width)),
  fail.
maxDatL(_, MaxL1) :-
  maxiLen(MaxL),
  MaxL1 = MaxL. % + 5.

maxOndL(Win, _) :-
  assert(maxiLen(0)),
  wc(_, _, _, CatNam, _, _, _, _, _, _, _, _, _, _, _),
  win_GetTextExtent(Win, CatNam, -1, Width, _Height),
  maxiLen(CurMaxL),
  Width > CurMaxL,
  assert(maxiLen(Width)),
  fail.
maxOndL(_, MaxL1) :-
  maxiLen(MaxL),
  MaxL1 = MaxL.

maxOndLshort(Win, Mode, _) :-
  assert(maxiLen(0)),
  wedstr_in_volg1(Win, Str, Mode),
  parsetabs(Str, List),
  List = [_, Cat | _],
  str_int(Cat, CatNr), 
  wc(CatNr, _, _, _, _, CatNam, _, _, _, _, _, _, _, _, _),
  trim_c(CatNam),
  win_GetTextExtent(Win, CatNam, -1, Width, _Height),
  maxiLen(CurMaxL),
  Width > CurMaxL,
  assert(maxiLen(Width)),
  %writedevice(X),writedevice(screen),write("\n",CatNr,"\t  ",CatNam, "\t", Width), writedevice(X),
  fail.
maxOndLshort(_, _Mode, MaxL1) :-
  maxiLen(MaxL),
  %writedevice(X),writedevice(screen),write("\nMode: ",Mode,"MaxL: \t", MaxL), writedevice(X),
  MaxL1 = MaxL.

maxParkL(_Win, 0, _, 0) :-
  baan_adm(nee), !.  % = lokatieplanning!
maxParkL(_Win, _, _, _) :-
  assert(maxiLen(0)),
  w2(_,_,_,_,_,_,_,BaanPark),
  win_GetTextExtent(_Win, BaanPark, -1, Width, _Height),
  maxiLen(CurMaxL),
  Width > CurMaxL,
  assert(maxiLen(Width)),
  fail.
maxParkL(_, MaxL, Bspace, Bspace) :-
  maxiLen(MaxL),
  !.

banenPopup(Max, N,  []) :- 
  N > Max, !.
banenPopup(Max, BaanNr,    [ownerdraw(BaanNr,111,b_False,0,[])|Rest]) :-
  baannaam(BaanNr, b_False, _, _, _),
  N1 = BaanNr + 1, 
  banenPopup(Max, N1, Rest), !.
banenPopup(Max, BaanNr,    [ownerdraw(BaanNr,111,b_True,0,[])|Rest]) :-
  baantoew(_No, _Cat, baan(BaanNr, _)),		% mag wel gereserveerd worden
  N1 = BaanNr + 1, 
  banenPopup(Max, N1, Rest), !.
banenPopup(Max, N,    [ownerdraw(N,111,b_True,0,[])|Rest]) :-
  N1 = N + 1, 
  banenPopup(Max, N1, Rest), !.
banenPopup(_Max, _,  []).

selectIcon(BaanNr, color_LtGray, color_Gray) :-
  baannaam(BaanNr, b_False, _, _, _), !.
selectIcon(BaanNr, color_magenta, color_Black) :-
  baantoew(_No, _Cat, baan(BaanNr, _)), !.
selectIcon(_BaanNr, color_green, color_Black).

doorschuifvolgorde(I, String) :-
  baantoew(No, Cat, reserveren(I, AanwezigTijd)),
  w2(No,Cat,PlanTijd,_,_,_,_,_), 
  tijd_str(PlanTijd, Str,_),
  format(String, "%s%s\t%u\t%u",Str, AanwezigTijd, No, Cat).

%  baantoew(No, Cat, baan(N)),
%  baanStadiumUit(N, _, TxtId, idb_baanaanwezig1gn, idb_baanaanwezig1rd),
subclassu_eh(LWin,e_OwnerDraw(_CtlType,_TxtId,BaanNr,_ItemAction,ItemState,_LboxWin,RectItem,_ItemData),0) :-  % van de uitslaginvoer
  own_member1(ItemState, odstate_Selected),   % was selected
  RectItem = rct(L,T,_,_),
  baannaamP(BaanNr, _, _, BNaam),
  selectIcon(BaanNr, OpKleur, Tekstkleur),
  drawMyIcon(LWin, BNaam, L, T, OpKleur, Tekstkleur, color_black, rct(_,_,R1,_)),
  Arrow = pict_GetFromRes(idb_baanarrow), 
  pict_Draw(LWin, Arrow, pnt(R1, T), rop_SrcCopy), !.
subclassu_eh(LWin,e_OwnerDraw(_CtlType,_TxtId,BaanNr,_ItemAction,_ItemState,_LboxWin,RectItem,_ItemData),0) :-
  RectItem = rct(L,T,R,B),
  baannaamP(BaanNr, _, _, BNaam),
  selectIcon(BaanNr, OpKleur, TekstKleur),
  drawMyIcon(LWin, BNaam, L, T, OpKleur, Tekstkleur, color_black, rct(_,_,R1,_)),
  Kleur = vpi_GetAttrVal(attr_color_btnface),
  win_SetPen(LWin,pen(1, ps_solid, Kleur)),
  win_SetBrush(LWin,brush(pat_solid,Kleur)),
  draw_Rect(LWin,rct(R1,T,R,B)), !.
subclassu_eh(_Win,e_OwnerMeasureItem(_CtlType,_CtlId,_ItemId,_Data),Size):-
  maxBaanBreedte(_Win,Lengte, Hoogte),
  %win_GetTextExtent(_Win, "JansenenTilanusg", -1, Width, _He),
  Width = 5 + Lengte,
  Height = Hoogte + 1, %He,
  bitleft(Height, 16, H1),
  Size = Width + H1,!.
subclassu_eh(_LWin,e_Char(k_f5,_),0):-!,		% ???
  Parent=win_GetParent(_LWin),
  planSelect(Parent, 4, _),
  win_SendEvent(Parent,e_Control(idci_lbagenda,wc_Lbox,_LWin,activated)),  !.
subclassu_eh(_LWin,e_Char(k_f9,_),0):-
  Parent=win_GetParent(_LWin),
  win_PostEvent(Parent,e_Menu(id_Wedstrijd_uitslag_invoeren, 0)), !.
subclassu_eh(_LWin,e_Char(k_f5,_),0):-
  Parent=win_GetParent(_LWin),
  win_PostEvent(Parent,e_Menu(id_Wedstrijd_plan, 0)), !.
subclassu_eh(_LWin,e_Char(k_f6,_),0):-
  Parent=win_GetParent(_LWin),
  win_PostEvent(Parent,e_Menu(id_Speler_Oproepen_lijst, 0)), !.
subclassu_eh(_LWin,e_Char('\27',_ShiftCtlAlt),0):-!,
  Parent=win_GetParent(_LWin),
  win_Destroy(Parent).
subclassu_eh(_LWin,e_Char(Kar,_ShiftCtlAlt),0):-!,
  Parent=win_GetParent(_LWin),
  win_SendEvent(Parent,e_Char(Kar,_ShiftCtlAlt)), !.
subclassu_eh(LboxWin,e_mouseDown(PNT,c_Nothing,mouse_button_left),0):-	% allemaal aanwezig
  Win = win_GetParent(LboxWin),
  planSelect(Win, 2, _Poule),	% 0 = te plannen 1 = alle 2 = uitslagen 3 = per poule 4 = agendamode
  hotBaanSpot(LboxWin, No, Cat, RCT),  
  rect_PntInside(RCT,PNT),
  baantoew(No, Cat, aanwezig(_)),
  assert(menucontext(No, Cat)),
  maxbanenM(Max),
  banenPopup(Max,1, Menu),
  menu_PopUp(LboxWin,dyn_menu(Menu), PNT, align_Left), !.
subclassu_eh(LboxWin,e_mouseDown(PNT,c_Nothing,mouse_button_left),0):-	% gespeeld op (vrijgegeven)
  Win = win_GetParent(LboxWin),
  planSelect(Win, 2, _Poule),	% 0 = te plannen 1 = alle 2 = uitslagen 3 = per poule 4 = agendamode
  hotBaanSpot(LboxWin, No, Cat, RCT),  
  rect_PntInside(RCT,PNT),
  baantoew(No, Cat, gespeeldop(_B, _)),
  assert(menucontext(No, Cat)),
  %baannaamP(B, _, _, BNaam),
  %format(GaTerug, "&Toch niet klaar op baan %s", BNaam),
  GaTerug = "&Undo",
  Menu = dyn_menu([
    %txt(id_DoeNiks,DoeNiks,0,b_True,0,[]),
    txt(id_Wedstrijd_uitslag_invoeren,"&Uitslag invoeren",0,b_True,0,[]),
    txt(id_Terugtrekken, GaTerug, 0, b_True,0, [])
    ]),
  menu_PopUp(LboxWin,Menu, PNT, align_Left), !.		
subclassu_eh(LboxWin,e_mouseDown(PNT,c_Nothing,mouse_button_left),0):- % alle andere
  hotBaanSpot(LboxWin, No, Cat, RCT),  
  rect_PntInside(RCT,PNT),
  baantoew(No, Cat, baan(_B, _)),
  assert(menucontext(No, Cat)),
  Menu = dyn_menu([
    txt(id_Wedstrijd_uitslag_invoeren,"&Uitslag invoeren",0,b_True,0,[]),
    txt(id_Vrijgeven,"&Vrij geven (uitslag straks invoeren)",0,b_True,0,[]),
    txt(id_Terugtrekken,"&Undo",0,b_True,0,[])
    %txt(id_Terugtrekken,"&Terugtrekken",0,b_True,0,[])
    ]),
  menu_PopUp(LboxWin,Menu, PNT, align_Left), !.
subclassu_eh(LboxWin,e_mouseDown(PNT,c_Nothing,mouse_button_left),0):-
  Win = win_GetParent(LboxWin),
  planSelect(Win, 2, _Poule),	% 0 = te plannen 1 = alle 2 = uitslagen 3 = per poule 4 = agendamode
  hotBaanSpot(LboxWin, No, Cat, RCT),  
  rect_PntInside(RCT,PNT),
  baantoew(No, Cat, reserveren(_B, _)),
  assert(menucontext(No, Cat)),
  Menu = dyn_menu([
    txt(id_Terugtrekken1,"&Undo",0,b_True,0,[])
    %txt(id_Terugtrekken1,"&Reservering terugtrekken",0,b_True,0,[])
    ]),
  menu_PopUp(LboxWin,Menu, PNT, align_Left), !.
subclassu_eh(LWin,e_mouseDown(PNT,c_Nothing,mouse_button_left),0):-
  Win = win_GetParent(LWin),
  planSelect(Win, 2, _Poule),	% 0 = te plannen 1 = alle 2 = uitslagen 3 = per poule 4 = agendamode
  hotSpelerSpot(LWin, No, Cat, Sp, RCT),  
  rect_PntInside(RCT,PNT),
  switchSpelerAanwezig(Sp, No, Cat), 
  win_PostEvent(Win,e_User(updatedagstatus,0)),				% update dagstatus
  hotSpelerSpot(LWin, _, _, Sp, RCTx),  
  RCTx = rct(_L,T,R,B),
  %L1 = L - 10,
  R1 = R + 100,		% veld is misschien breder geworden
  win_Invalidate(LWin, rct(0,T,R1,B)),
  fail.  %, RCT).
subclassu_eh(LWin,e_mouseDown(PNT,c_Nothing,mouse_button_right),0):-	% formulier!
  Win = win_GetParent(LWin),
  planSelect(Win, 2, _Poule),	% 0 = te plannen 1 = alle 2 = uitslagen 3 = per poule 4 = agendamode
  hotSpelerSpot(LWin, _No, _Cat, SKeus, RCT),  
  rect_PntInside(RCT,PNT),
  sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Res),
  SpelerIn = sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Res),
  TaskWin = vpi_GetTaskWin(),
  dlg_persoonsgegevens_Create(TaskWin, SpelerIn ), !.
subclassu_eh(LWin,e_mouseDown(_PNT,c_Nothing,mouse_button_right),0):-
  ParentWin = win_GetParent(LWin),
  win_PostEvent(ParentWin,e_Menu(idts_popup,0)),
  !.
subclassu_eh(LWin,e_Menu(BaanNr, _X),0):- 
  Win = win_GetParent(LWin),
  planSelect(Win, 2, _Poule),	% 0 = te plannen 1 = alle 2 = uitslagen 3 = per poule 4 = agendamode
  %baanStadiumUit(BaanNr, _Tid, Id, _, _, _),
  not(baantoew(_, _, baan(BaanNr, _))),				% Nr niet bezet
  menucontext(No, Cat),
  baantoew(No, Cat, aanwezig(T)),			% en aanwezig
  %logf('A',baantoew(No, Cat, baan(BaanNr, T)), nee),		% wijs de wedstrijd toe aan die baan
  upd_baantoew(baantoew(No, Cat, baan(BaanNr, T))),
  update_uitslagen("e_menu baantoew"), 
  win_PostEvent(Win,e_User(updatedagstatus,0)),				% update dagstatus
  !.
subclassu_eh(LWin,e_Menu(BaanNr, _X),0):- 
  Win = win_GetParent(LWin),
  planSelect(Win, 2, _Poule),	% 0 = te plannen 1 = alle 2 = uitslagen 3 = per poule 4 = agendamode
  menucontext(No, Cat),
  baantoew(_, _, baan(BaanNr, _)),				% Nr wel bezet
  baantoew(No, Cat, aanwezig(T)),
  upd_baantoew(baantoew(No, Cat, reserveren(BaanNr, T))),
  %logf('A',baantoew(No, Cat, reserveren(BaanNr, T)),nee),	% zet in de wacht voor die baan
  update_uitslagen("e_menu baantoew"), 
  win_PostEvent(Win,e_User(updatedagstatus,0)),				% update dagstatus
  !.
subclassu_eh(LboxWin,e_Menu(id_Wedstrijd_uitslag_invoeren, _),0):- 
  Parent = win_GetParent(LboxWin),
  menucontext(No, Cat),
  dlg_uitslaginvoer1_Create(Parent, No, Cat, b_True),
  update_uitslagen("id_uitslag_invoeren"),
  trap(win_BringToTop(Parent),_,true),
  win_PostEvent(Parent,e_User(updatedagstatus,0)),				% update dagstatus
  !.
subclassu_eh(LboxWin,e_Menu(id_Vrijgeven, _),0):- 
  Win = win_GetParent(LboxWin),
  menucontext(No, Cat),
  retract(baantoew(No, Cat, baan(I, T))), !,			% vrijgeven
  logf('A',baantoew(No, Cat, gespeeldop(I, T)),nee),		% 'gespeeld op'
  doorschuiven(LboxWin, I),
  update_uitslagen("id_vrijgeven"),
  win_PostEvent(Win,e_User(updatedagstatus,0)), !.				% update dagstatus
subclassu_eh(LboxWin,e_Menu(id_Vrijgeven, _),0):- 	% 1.8.2004
  Win = win_GetParent(LboxWin),
  menucontext(No, Cat),
  retract(baantoew(No, Cat, reserveren(I, T))), !,			% vrijgeven
  logf('A',baantoew(No, Cat, gespeeldop(I, T)),nee),		% 'gespeeld op'
  doorschuiven(LboxWin, I),
  update_uitslagen("id_vrijgeven"),
  win_PostEvent(Win,e_User(updatedagstatus,0)),				% update dagstatus
   !.
subclassu_eh(_LboxWin,e_Menu(id_Terugtrekken, _),0):- 
  menucontext(No, Cat),
  baantoew(No, Cat, gespeeldop(I, T)), !,
  terugschuiven(I),
  logf('A', baantoew(No, Cat, baan(I, T)), nee),
  update_uitslagen("id_terugtrekken").
subclassu_eh(LboxWin,e_Menu(id_Terugtrekken, _),0):- 
  menucontext(No, Cat),
  baantoew(No, Cat, baan(I, T)), !,
  %logf('A',baantoew(No, Cat, aanwezig(T)),nee),
  upd_baantoew(baantoew(No, Cat, aanwezig(T))),
  doorschuiven(LboxWin, I),
  update_uitslagen("id_terugtrekken"), !.
subclassu_eh(_LboxWin,e_Menu(id_Terugtrekken1, _),0):- 
  menucontext(No, Cat),
  baantoew(No, Cat, reserveren(_I, Tijd)), !,
  upd_baantoew(baantoew(No, Cat, aanwezig(Tijd))),
  %logf('A',baantoew(No, Cat, aanwezig(Tijd)),nee),
  update_uitslagen("id_terugtrekken"), !.
subclassu_eh(LboxWin,e_mouseDown(PNT,c_Nothing,mouse_button_left),0) :-
  hotBaanSpot(LboxWin, _No, _Cat, RCT),  
  rect_PntInside(RCT,PNT), !.
subclassu_eh(LboxWin,e_mouseDown(PNT,c_Nothing,mouse_button_left),0) :-
  hotSpelerSpot(LboxWin, _No, _Cat, _, RCT),  
  rect_PntInside(RCT,PNT), !.

baannaamP(BaanNr, Beschikbaar, Park, BaanNaam) :-
  baannaam(BaanNr, Beschikbaar, Park,BaanNaam, _), !.
baannaamP(BaanNr, b_True, "", BaanNaam) :-
  BaanNr <> 99,
  not(baannaam(BaanNr, _, _, _, _)),
  format(BaanNaam, "%u", BaanNr), !.
baannaamP(_, b_True, "", "").

switchSpelerAanwezig(Sp, _, _) :-
  spelerAanwezig(Sp, Tijd, Noxx, Catxx),
  sp2(Sp,_Sex,Naam,_,_,_,_,_Betaald,_, _, _, _, _BondsNr, _, _, _, _, _, _, _,_,_,_,_,_,_,_), 
  format(Msg, "%s afmelden?", Naam),
  1 = dlg_MessageBox( "Speler", Msg, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
  logf('R',spelerAanwezig(Sp, Tijd, Noxx, Catxx),nee),
  update_betalingen(),
  checkcompleet(), !.
  %baanKandidaat(No, Cat), !.
switchSpelerAanwezig(Sp, _No, _Cat) :-
  not(spelerAanwezig(Sp, _, _, _)),
  zetaanwezig(Sp),
  update_betalingen(),
  openbaar(Open),
  metKNLTBnummer(Sp, Open),
  te_informeren(Sp),
  te_betalen(Sp),
  checkcompleet(),  !.
switchSpelerAanwezig(_, _, _).
  %fail.

metKNLTBnummer(SKeus, ja) :-
  getbacktrack(Here),
  toernooiK(_,_),
  cutbackTrack(Here),
  sp2(SKeus,_Sex,Naam,_,_,_,_,_Betaald,_, _, _, _, BondsNr, _, _, _, _, _, _, _,_,_,_,_,_,_,_), 
  not(fronttoken(BondsNr, _, _)),
  format(Msg, "Speler %s mist bondsnummer!\nInvullen?\nKlik met rechter muisknop om het formulier te openen.", Naam),
  dlg_MessageBox( "Bondsnummer", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  !.
metKNLTBnummer(_, _).

te_informeren(SKeus) :-
  oproepen(ja),  
  getbacktrack(Here),
  wd_planned(SKeus,WedstrijdList,alleplanning,b_True),
  member(Wed, WedstrijdList),
  Wed = w(Num,Cat,_Tijd,Gewaarsch,_LRInWeds,Tgnst,_Verv,_BaanPark),
  Gewaarsch = nee,
  cutbacktrack(Here),
  TaskWin = vpi_GetTaskWin(),
  dlg_plangegevens_Create(mijnoproepen,TaskWin, "Nog te informeren.", Tgnst, Num, Cat, _Result, b_False), !.
te_informeren(_Sp).

te_betalen(SKeus) :-
  betaal_adm(Betadm),
  sp2(SKeus,_Sex,Naam,_Telthuis,_Verh,_RatingE,_RatingD,Betaald,_UitKant, _Club, _Adres, _Woonpl, BondsNr, _ES, _DS, _Gebdatum, _Telwerk, _Telmobiel, _EMail, _Opm,Tshirt,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Res),
  %SpIn = sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,NNM,RangPosE,RangPosD,Incasso,CheckGeg,Log,Res),
  %write("betalingsmelding: ", BetAdm, " NNM: ", NNM, " Naam: ", Naam, " SKeus: ", SKeus, " Bondsnr: ", Bondsnr),
  tshirtmelding(Tshirt, TshirtOut,Naam), 
  betalingMsg(Betadm, Betaald, BetaaldUit, Naam,Bondsnr,SKeus),
  changeMelding(SKeus, BetaaldUit, TshirtOut),
  !.
te_betalen(_SKeus).

% tshirtmelding algemeld meldadm resultaat
tshirtmelding(ja, ja, _) :- !.
tshirtmelding(X, X, _) :-
  tshirt(b_False, _), !.
tshirtmelding(nee, ja, Naam) :- 
  tshirt(_, Melding),  
  format(M1, "%s\n%s",Naam, Melding),
  1 = dlg_MessageBox( "Speler", M1, mesbox_iconinformation, mesbox_buttonsokcancel, mesbox_defaultfirst, mesbox_suspendapplication ), !.
tshirtmelding(_, nee,_). 
  
betalingMsg(ja, nee,ja, Naam,BondsNr,SpNo) :-
  contributie(SpNo, Bedrag, String, []),
  format(Msg, "%s - Bondsnummer: %s\n%7.2f  eur%s\nVoldaan?", Naam, BondsNr, Bedrag, String),
  1 = dlg_MessageBox( "Deze speler", Msg, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
  !.
betalingMsg(_, X,X,_,_,_).

changed(Betaald, Betaald, NNM, NNM) :- !, fail.
changed(_,_,_,_).

changeMelding(SKeus, Betaald, NNM) :-
  sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,BetOud,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
  changed(Betaald,BetOud, Gezien,NNM),
  SpelerIn = sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,NNM,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
  logf('A',SpelerIn,nee), !,
  %checkKNLTB(BondsNr),
  update_uitslagen("changemelding"),
  update_waarsch.
changeMelding(_SKeus, _, _).

own_member1([X|_],X).
own_member1([_|T],X):-own_member1(T,X).

close_uitslaginv() :-
  itemAfmU(Win, _Tab1, _Tab2, _Tab3, _Tab4, _Tab5, _Tab5a, _Tab6),
  win_Destroy(Win),
  fail.
close_uitslaginv().

ookDeTePlannen(SpelersList, Num, Cat, SpelersListU) :-
  wd(Num,Cat,T1, T2,_,_,_), 
  de_Spelers(Num, Cat, T1, T2, SpelersList1),
  append(SpelersList, SpelersList1, SpelersListU), !.
ookDeTePlannen(SpelersList, _, _, SpelersList).

popUpU(_Win, PNT) :-
  planSelect(_Win, Mode, _),
  Mode <> 4,
  %not(planSelect(_Win, 4, _)),
  LboxWin = win_GetCtlHandle(_Win, idci_lbagenda),
  Index = lbox_GetSelIndex(LboxWin),
  Item = lbox_GetItem(LboxWin, Index),
  wedRepr(Mode, Item, _Naam, _Naam2, Num, Cat, _, _, _, _, _, _, _, _, _, _, _, _),
  planbaar(Num, Cat, PlanB),
  invoerbaar(Num, Cat, Inv),
  wisplanbaar(Num, Cat, PlanW),
  beeldbaar(Num, Cat, Beeld),
  baanbaar(Num, Cat, Bnb),
  Menu = dyn_menu([
    txt(id_Wedstrijd_plan,"&Plan\tF5",0,PlanB,0,[]),
    txt(id_Wedstrijd_wis_plan1,"&Wis plan",0,PlanW,0,[]),
    txt(id_Wedstrijd_uitslag_invoeren,"&Uitslag invoeren\tF9",0,Inv,0,[]),
    txt(idr_baannummer,"&Locatietoewijzing", 0, BnB, 0, []),
    separator,
    txt(idts_overzicht,"Wedstrijd&overzicht\tF7",0,Beeld,0,[]),
    txt(idr_schemaeen,"Schema",0,Beeld,0,[])
    ]), !,
  menu_PopUp(_Win,Menu, PNT, align_Left), !.
popUpU(_Win, PNT) :-
  planSelect(_Win, 4, _),
  LboxWin = win_GetCtlHandle(_Win, idci_lbagenda),
  Index = lbox_GetSelIndex(LboxWin),
  Item = lbox_GetItem(LboxWin, Index),
  wedRepr(4, Item, _Naam, _Naam2, Num, Cat, _, _, _, _, _, _, _, _, _, _, _, _),
  wd(Num,Cat,T1, T2,_,_,_), 
  de_Spelers(Num, Cat, T1, T2, SpelersList),
  bovenTekens(NoP, CatP),
  ookDeTePlannen(SpelersList, NoP, CatP, SpelersListPlus), 
  findall(MItem, namenX(SpelersListPlus, MItem), MenuSpelerItems),
  planbaar(Num, Cat, PlanB),
  invoerbaar(Num, Cat, Inv),
  wisplanbaar(Num, Cat, PlanW),
  beeldbaar(Num, Cat, Beeld),
  baanbaar(Num, Cat, Bnb),
  Menu = dyn_menu([
    txt(id_Wedstrijd_plan,"&Plan\tF5",0,PlanB,0,[]),
    txt(idt_set_fixeer,"(ont-)&Markeer (gefixeerd)",0,PlanB,0,[]),
    txt(idt_tijdstip,"&Extra tijdstip",0,b_True,0,[]),
    txt(id_Wedstrijd_wis_plan1,"&Wis plan",0,PlanW,0,[]),
    txt(id_Wedstrijd_uitslag_invoeren,"&Uitslag invoeren\tF9",0,Inv,0,[]),
    txt(idr_baannummer,"&Locatietoewijzing", 0, BnB, 0, []),
    txt(idr_baannummer,"&Deelnemers", 0, b_True, 0, MenuSpelerItems),
    separator,
    txt(idts_overzicht,"Wedstrijd&overzicht\tF7",0,Beeld,0,[]),
    txt(idr_schemaeen,"Schema",0,Beeld,0,[]),
    separator,
    txt(0,"Achtergrondkleuren", 0, PlanB,0,[
      txt(id_Kleur2,"&Betreffende", 0, PlanB,0,[]),
      txt(id_Kleur1,"&Onderliggende", 0, PlanB,0,[]),
      txt(id_KleurR,"&Reset", 0, PlanB,0,[])
      ])
    ]), !,
  menu_PopUp(_Win,Menu, PNT, align_Left), !.

save_UitslWin() :-
  planSelect(Win, 4, _),
  updateVensterpositie(plannenvenster, Win),
  fail.
save_UitslWin() :-
  planSelect(Win, Mode, _),
  not(Mode = 4),
  updateVensterpositie(uitslagenvenster, Win), !.
save_UitslWin().

vensterpositieL(Win) :-	
  planSelect(Win, 4, _),
%  not(VensterID = onderdeelselectvenster),
  vensterRCT(plannenvenster, RCT),
  win_Move(Win, RCT), !.
vensterpositieL(Win) :-	
  planSelect(Win, M, _),
  M <> 4,
  vensterRCT(uitslagenvenster, RCT),
  win_Move(Win, RCT), !.
vensterpositieL(Win1) :-
  planSelect(Win1, 4, _),
  TaskWin = vpi_GetTaskWin(),
  TaskRct = win_GetClientRect( TaskWin ),
  RCT = win_GetOuterRect(Win1),
  WsFlags = win_GetState(Win1),
  ClRCT = rect_GetClient(WsFlags, b_False, RCT),
  TaskRct = rct(_, _, Xt, _),
  ClRCT = rct(_, _, Xc, _),
  OffsX = Xt - Xc - 54,
  NewClient = rect_Offset(ClRCT, OffsX, 0), 
  win_Move(Win1, NewClient), !.
vensterpositieL(_Win).

planMetAgenda() :-			% groene taakbalkicoon 'A'
  not(w2(_,_,_,_,_,_,_,_)), !.
planMetAgenda() :-			% groene taakbalkicoon 'A'
  pos(Cat, _, No, _),
  w2(No,Cat,Tijd,_,_,_,_,_),
  assert(boventekens(0, 0)),
  assert(ondertekens(Tijd, No, Cat, "---")),
  TaskWin = vpi_GetTaskWin(),
  win_uitslageninvoer_Create(TaskWin, 4, 0), !.
planMetAgenda() :-
  boventekens(0, 0),
  TaskWin = vpi_GetTaskWin(),
  win_uitslageninvoer_Create(TaskWin, 4, 0), !.
planMetAgenda() :-
  time_stamp_next(Huidig, _Dag, _HoeLaat),
  assert(boventekens(0, 0)),
  assert(ondertekens(Huidig, 0, 0, "---")),
  TaskWin = vpi_GetTaskWin(),
  win_uitslageninvoer_Create(TaskWin, 4, 0), !.

planMetAgenda(No, Cat) :-		% echt plannen 'P'
  planbaar(No, Cat, PlanB),
  Planb = b_False, !.			% voor de zekerheid
planMetAgenda(No, Cat) :-
  not(w2(No,Cat,_,_,_,_,_,_)),
  TaskWin = vpi_GetTaskWin(),
  wd(No,Cat,T1,T2,_,_,_),               /* kijk wie er meespelen            */
  time_stamp_next(Huidig, _Dag, _HoeLaat),
  de_spelers(No,Cat,T1,T2,SpelerS),
  na_vorige(No,Cat,T1,T2,BeginT1),
  grootste(Huidig,BeginT1,BeginTijd),
  scan_planning_c(1, BeginTijd, Spelers, No, Cat, AdviesTijd, _Over),
  assert(boventekens(No, Cat)),
  assert(ondertekens(AdviesTijd, 0, 0, "Eerstbeschikbare tijdstip geselecteerd")),
  win_uitslageninvoer_Create(TaskWin, 4, 0), !.
planMetAgenda(No, Cat) :-
  not(w2(No,Cat,_,_,_,_,_,_)),
  TaskWin = vpi_GetTaskWin(),
  time_stamp_next(Huidig, _Dag, _HoeLaat),
  na_vorige(No,Cat,bye,bye, BeginT1),
  grootste(Huidig,BeginT1,BeginTijd),
  assert(boventekens(No, Cat)),
  assert(ondertekens(BeginTijd, 0, 0, "Moeilijke te plannen!")),
  win_uitslageninvoer_Create(TaskWin, 4, 0),
  !.
planMetAgenda(No, Cat) :-
  w2(No,Cat,Tijd,_,_,_,_,_), !,
  TaskWin = vpi_GetTaskWin(),
  assert(boventekens(No, Cat)),
  assert(ondertekens(Tijd, No, Cat, "Huidige plan geselecteerd!")),
  win_uitslageninvoer_Create(TaskWin, 4, 0).

planselectNot4(Win, Mode, Poule) :-
  planSelect(Win, Mode, Poule),
  Mode <> 4, !.

opmZuiver(Opm, Uit) :-
  %opmMinImport(Opm, O1),
  opmMinWas(Opm, O2),
  opmMinEmail(O2, Uit),
  trim_c(Uit).


opmMinWas(Opm, Uit) :-
  searchstring(Opm,"(was:",Pos),
  P1 = Pos - 1,
  frontstr(P1, Opm, Voor, Achter),
  searchchar(Achter, ')', P2),
  P2 - P1 < 32,			% 25.12.2002 voor de zekerheid
  frontstr(P2, Achter, _, A1), !,
  concat(Voor, A1, In1),
  opmMinWas(In1, Uit).
opmMinWas(Opm, Opm).

opmMinEmail(Opm, Uit) :-
  searchstring(Opm,"Email op",Pos),
  P1 = Pos - 1,
  frontstr(P1, Opm, Voor, Achter),
  searchchar(Achter, '.', P2),
  frontstr(P2, Achter, _, A1), !,
  nogeenPunt(A1, A2),
  concat(Voor, A2, In1),
  opmMinEmail(In1, Uit).
opmMinEmail(Opm, Opm).

nogeenPunt(In, Uit) :-
  searchchar(In, '.', Pos),
  Pos < 4,
  frontstr(Pos, In, _, In1), !,
  nogeenPunt(In1, Uit).
nogeenPunt(In, In).

opm([Sp|SpL], In, OpmUit) :-
  fronttoken(In, _, _),			% extra linefeed
  sp2(Sp,_,NaamS,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,Opm,_,_,_,_,_,_,_),
  fronttoken(Opm, _, _), 
  %opmMinEmail(Opm, Opm1),
  %opmMinImport(Opm1, Opm2),
  %opmMinWas(Opm2, Opm3),
  Opm3 = opmZuiver(Opm),
  fronttoken(Opm3, _, _), !,
  %trim_c(Opm3),
  formatOpm(Opm3, "", "", Uit, " ", 45),
  format(In1, "%s\n%s: %s",In, NaamS, Uit),
  opm(SpL, In1, OpmUit).
opm([Sp|SpL], _In, OpmUit) :-
  sp2(Sp,_,NaamS,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,Opm,_,_,_,_,_,_,_),
  fronttoken(Opm, _, _), 
  %opmMinEmail(Opm, Opm1),
  %opmMinImport(Opm1, Opm2),
  %opmMinWas(Opm2, Opm3),
  Opm3 = opmZuiver(Opm),
  fronttoken(Opm3, _, _), !,
  %trim_c(Opm3),
  formatOpm(Opm3, "", "", Uit, " ", 45),
  %format(In1, "%s\n%s: %s",In, NaamS, Uit),
  format(In1, "%s: %s",NaamS, Uit),
  opm(SpL, In1, OpmUit).
opm([_|SpL], In, OpmUit) :- !,
  opm(SpL, In, OpmUit).
opm(_, In, In).

cleanSpelerAanwezig() :-
  date(Year, Month, Day),
  dt_date_to_offset(Year, Month, Day, D1),
  spelerAanwezig(Sp, Tijd, Noxx, Catxx),
  tijd_kwart(Dag,_,_,Tijd),
  Dag <> D1,
  not(w2(Noxx,Catxx,Tijd,_,_,_,_,_)),
  logf('R',spelerAanwezig(Sp, Tijd, Noxx, Catxx), ja),
  fail.
cleanSpelerAanwezig() :-
  logclose().

create_toolbars(_Win, _) :-
  toolbar_remove(_Win),
  fail.
create_toolbars(_Win, 4) :-		% 2 toolbars
  win_SetIcon(_Win, idi_planning),
  not(boventekens(0,0)),
  tb_planning_Create(_Win),
  tb_planbottom_Create(_Win), !.
create_toolbars(_Win, 4) :-		% 12 toolbar
  win_SetIcon(_Win, idi_agenda),
  boventekens(0,0),
  tb_planbottom_Create(_Win), !.
create_toolbars(_Win, 2) :-
  win_SetIcon(_Win, idi_uitslagen),
  cleanSpelerAanwezig(),
  tb_uitslagen_Create(_Win), !.
create_toolbars(_Win, _) :-
  win_SetIcon(_Win, idi_uitslagen),
  tb_uitslagen_Create(_Win), !.

listbox_Resize(Win, LboxWin, 4) :-	% twee toolbars
  WRCT = win_GetClientRect( Win ),
  WRCT = rct(_,_,Width, Height),
  not(boventekens(0,0)),
  toolbar_GetRect(Win,tb_bottom,RCT),
  toolbar_GetRect(Win,tb_top,RCT1),
  RCT = rct(_,T,_,B),
  RCT1 = rct(_,_,_,B1),
  H = Height - B + T,
  win_Move(LboxWin,rct(0,B1,Width,H)),
  !.
listbox_Resize(Win, LboxWin, _) :-	% één toolbar
  WRCT = win_GetClientRect( Win ),
  WRCT = rct(_,_,Width, Height),
  toolbar_GetRect(Win,tb_bottom,RCT),
  RCT = rct(_,T,_,B),
  H = Height - B + T,
  win_Move(LboxWin,rct(0,0,Width,H)),
  !.
listbox_Resize(_Win, _LboxWin, _).

getverschuiving(_Win, 4, No, Cat, _Tijd) :-
  not(verzet(_Win, No, Cat, _, _, _)),
  verschuiving(No, Cat, Tekst, Kleur, Ruilbaar),
  assert(verzet(_Win, No, Cat, Tekst, Kleur, Ruilbaar)), !.
getverschuiving(_, _, _, _, _).

overplanning(Tijd, Tekst) :-
  teveelweds(Tijd, Overloop),
  Overloop > 1, !,
  format(Tekst, "\nEr staan %u banen teveel gepland.", Overloop).
overplanning(Tijd, Tekst) :-
  teveelweds(Tijd, Overloop),
  Overloop = 1, !,
  format(Tekst, "\nEr staat 1 baan teveel gepland.").
overplanning(_, "").

eerstvolgendeVoorDeAndere(Tijd, No, Cat, SpelerS, Tekst) :-		% bepaalt de ruiltekst voor aangewezen weds
  time_stamp_next(Huidig, _Dag, _HoeLaat),
  na_vorige(No,Cat,bye,bye,BeginT1),
  grootste(Huidig,BeginT1,BeginTijd),
  boventekens(BetreffendeNo, BetreffendeCat),
  schort_op2(BetreffendeNo, BetreffendeCat, SchortList),
  undoSchortOp(SchortList),
  scan_planning_c(1, BeginTijd, Spelers, No, Cat, AdviesTijd, _Over),
  isTeRuilen(No, Cat, SchortList, RuilTekst, _),
  herstel(SchortList),
  overplanning(Tijd, OTekst),
  tijd_str(AdviesTijd,TStr1,_),
  bitright(AdviesTijd, 7, Dag),
  dag(Dag,DStr1,_),
  format(Tekst, "Onderliggende partij kan naar: %s %s%s%s",  DStr1 ,TStr1, RuilTekst, OTekst), !.
eerstvolgendeVoorDeAndere(_, _No, _Cat, _SpelerS, "Onderliggende partij moeilijk te verschuiven!").

isTeRuilen(No, Cat, [s(_,_,BTijd,_,_,_,_,_)], "\nDeze wedstrijden zijn te ruilen.", 1) :-
  check_forceer_c(1, [BTijd], No, Cat, _, Result,_Bezwaren,_),
  bitand(Result, 0xFEFF, Rc1),
  Rc1 = 0, !.
isTeRuilen(_No, _Cat, [s(_,_,_BTijd,_,_,_,_,_)], "\nRuilen is moeilijk!", 0) :- !.
isTeRuilen(_No, _Cat, _, "", 0) :- !.

verschuiving(No, Cat, Tekst, Kleur, Ruilbaar) :-		% wordt voor alle regels uitgerekend
  wd(No,Cat,T1,T2,_,_,_),
  de_spelers(No,Cat,T1,T2,SpL),
  w2(No,Cat,HTijd,_,_,_,_,_),
  time_stamp_next(Huidig, _Dag, _HoeLaat),
  na_vorige(No,Cat,T1,T2,BeginT1),
  grootste(Huidig,BeginT1,BeginTijd),
  boventekens(BetreffendeNo, BetreffendeCat),
  schort_op2(BetreffendeNo, BetreffendeCat, SchortList),
  undoSchortOp(SchortList),
  scan_planning_c(1, BeginTijd, SpL, No, Cat, AdviesTijd, _Over),
  isTeRuilen(No, Cat, SchortList, _RuilTekst, Ruilbaar),
  herstel(SchortList),
  bitright(HTijd, 7, HDag),
  bitright(AdviesTijd, 7, ADag),
  Diff = ADag - Hdag,
  negatief(Diff, Teken, Kleur),
  format(Tekst, "%s%d", Teken, Diff), !.
verschuiving(0, 0, "", color_Black, 0) :- !.
verschuiving(_No, _Cat, "?!", color_Red, 0).
  
negatief(Diff, "", color_Black) :-
  Diff <= 0, !.
negatief(_Diff, "", color_Red).
  
vorig_plan(Num, Cat, G1, G2, OudeTijd) :-
  w2(Num,Cat,_T,J1,J2,_,_,_),
  wd(Num,Cat,T1,T2,_,_,_),              
  afzU(G1, J1, Num, Cat, OudeTijd, T1, nee),
  afzU(G2, J2, Num, Cat, OudeTijd, T2, nee), !.
vorig_plan(_Num, _Cat, _, _, _).

afzU(ja, nee, Pos, Cat, _Tijd, Tgnst, _) :-
  ws(Pos, Cat, Tgnst, _), !.		% was al gewaarschuwd
afzU(ja, nee, Pos, Cat, Tijd, Tgnst, LogOpen) :-
  not(Tgnst = onbekend),
  logf('A', ws(Pos, Cat, Tgnst, Tijd), LogOpen), !,
  update_waarsch().
afzU(_, ja, Pos, Cat, _, Tgnst, _):-
  ws(Pos, Cat, Tgnst, Tijd),
  logf('R', ws(Pos, Cat, Tgnst, Tijd),nee), !.
afzU(_, _, _, _, _, _, _).

afz(ja, Pos, Cat, _Tijd, Tgnst, _) :-
  ws(Pos, Cat, Tgnst, _), !.		% was al
afz(ja, Pos, Cat, Tijd, Tgnst, LogOpen) :-
  not(Tgnst = onbekend),
  logf('A', ws(Pos, Cat, Tgnst, Tijd), LogOpen), !,
  update_waarsch().
afz(_, _, _, _, _, _).

schort_op1(Num,Cat,InL,InL) :-           
  bitright(Num,1,N1),
  not(wd(N1,Cat,_,_,_,_,_)), !. /* einde van het rooster, doe niets meer */
schort_op1(Num,Cat,InL,[Opschort|SList]) :-
  bitright(Num,1,N1),
  w2(N1,Cat,Tijd,B,C,D,E,F), 		/* schort deze op              */
  retract(w2(N1,Cat,Tijd,B,C,D,E,F)), !,
  Opschort = s(N1,Cat,Tijd,B,C,D,E,F),
  schort_op1(N1,Cat,InL,SList).         /* en bekijk de volgende       */
schort_op1(Num,Cat,InL,OutL) :-    
  bitright(Num,1,N1),                   /* bekijk de volgende, ook als */
  schort_op1(N1,Cat,InL,OutL).          /* niets op te schorten viel   */

schort_op2(No, Cat, [s(No,Cat,Tijd,B,C,D,E,F)]) :-
  retract(w2(No,Cat,Tijd,B,C,D,E,F)), !.
schort_op2(_No, _Cat, []).

herstel([s(BetreffendeNo,BetreffendeCat,BTijd,BB,BC,BD,BE,BF)]) :-
  assert(w2(BetreffendeNo,BetreffendeCat,BTijd,BB,BC,BD,BE,BF)), !.	% en weer terug
herstel(_).

undoSchortOp(_).
undoSchortOp(List) :-
  member(Opschort,List),
  Opschort = s(N1,Cat,Tijd,B,C,D,E,F),
  not(w2(N1,Cat,_,_,_,_,_,_)),
  assertz(w2(N1,Cat,Tijd,B,C,D,E,F)),
  fail.

qsort([],[]).          /* sorteer wedstrijden op tijdsvolgorde */
qsort([H|T],S) :-
  split(H,T,A,B),
  qsort(A,A1),
  qsort(B,B1),
  append(A1,[H|B1],S), !.

split(_,[],[],[]).
split(s(Na,Cata,Tijda,Ba,Ca,Da,Ea,Fa),
               [s(N1,Cat,Tijd,B,C,D,E,F)|X],[s(N1,Cat,Tijd,B,C,D,E,F)|Y],Z) :-
  Tijda > Tijd,
  split(s(Na,Cata,Tijda,Ba,Ca,Da,Ea,Fa),X,Y,Z).
split(H,[A|X],Y,[A|Z]) :- !,
  split(H,X,Y,Z).

herplan(_,[],_) :- !.
herplan(Win,[Opschort|SList],LogOpen) :-
  Opschort = s(Num,Cat,Tijd,B,C,Forc,Stamp,Baan),
  select(Cat, Num),
  na_vorige(Num,Cat,bye,bye,BeginT1),
  Tijd >= BeginT1,
  check_forceer_c(0, [Tijd], Num, Cat, _, Result,_,_), 
  bitand(Result, 0xFEFF,Result1), % kijk niet naar voorafgaande wedstrijd nog niet gepland
  Result1 = 0, !,       			% herplannen gelukt !!!!!! 
  logf('A',w2(Num,Cat,Tijd,B,C,Forc,Stamp,Baan),nee),
  bitright(Num,1,_N1),
  select(Cat, Num),
  herplan(Win,SList,LogOpen).
herplan(Win,[Opschort|SList],LogOpen) :-	% ga anders al of niet forceren
  Opschort = s(Num,Cat,Tijd,B,C,_,Stamp,Baan),
  logclose(),
  tijd_str(Tijd,TStr1,_),
  bitright(Tijd,7,Dag),
  dag(Dag,DStr1,_),
  format(Intro, "Tijdstip: %s %s",  DStr1 ,TStr1),
  %	na_vorige(Pos,Cat,TijdVorige),
  check_forceer_c(1, [Tijd], Num, Cat, _, Result,Bezwaren,_),
  bitand(Result, 0xFEFF,Result1), % 2.6.2011 kijk niet naar voorafgaande wedstrijd nog niet gepland
  Result1 > 0,
  concat(Intro, Bezwaren, Bezwaren1),
  dlg_bezwaren_Create(Win, Num, Cat, Bezwaren1, Keus),
  Keus = 0, !,
  logf('A',w2(Num,Cat,Tijd,B,C,'f',Stamp,Baan),nee),
  bitright(Num,1,_N1),
  select(Cat,Num),
  herplan(Win,SList,LogOpen).
herplan(Win,[Opschort|SList],LogOpen) :-
  Opschort = s(Num,Cat,Tijd,J1,J2,Forc,Stamp,Baan),
  select(Cat,Num),
  tijd_str(Tijd,Str,_),
  tijd_kwart(D2,_,_,Tijd), 
  dag(D2,DStr2,_), 
  wd(Num, Cat, T1, T2, _, _, _), 
  afz(J1, Num, Cat, Tijd, T1, LogOpen),
  afz(J2, Num, Cat, Tijd, T2, LogOpen),
  logf('R', w2(Num,Cat,Tijd,J1,J2,Forc,Stamp,Baan),LogOpen),
  logclose(),
  format(S4," deze wedstrijd op % % moet verzet worden!!", DStr2, Str),
  dlg_MessageBox( "Planning", S4, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  bitright(Num,1,_N1), !,
  select(Cat,Num), !,
  herplan(Win,SList,LogOpen).

/*check_forceer(integer, tijdl, integer, wedscat, 
			integer, integer Duur, integer Enkel, integer, integer, string, integer, string Outstring, integer Tijdstip) - 
               (i, i, i, i, i, i, i, i, i, o, o, o, o) language stdcall %as "_check_forceer_c_0" was c
*/

gefixeerdAlsBezwaren(Win, NoOnder, CatOnder, Result, ' ') :-
  wd(NoOnder,CatOnder,_,_,_,_,_),
  w2(NoOnder,CatOnder,Oudetijd,J1,J2,_,_,_),
  boventekens(No, Cat),
  w2(No,Cat,Ruiltijd,_,_,_,_,_),
  wd(No,Cat,_,_,_,_,_),
  schort_op2(No, Cat, SchortList),
  undoSchortOp(SchortList),
  check_forceer_c(0, [Ruiltijd], NoOnder, CatOnder, _Outstring, ResultOnder, _, _TijdXX),
  bitand(ResultOnder, 0xFEFF, RcO),
  herstel(SchortList),
  RcO = 0,
  Result =  0x08,
  tijd_str(RuilTijd,Str,_),
  tijd_kwart(D2,_,_,RuilTijd), 
  dag(D2,DStr2,_), 
  format(Tekst, "Wedstrijden ruilen?\nDe onderliggende wedstrijd wordt verplaatst naar % %.", DStr2, Str),
  1 = dlg_MessageBox( "Ruilen", Tekst, mesbox_iconexclamation, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
  time_stamp1(TT, _, _),
  loglock(mdi, "gefixeerdAlsBezwaren"),
  check_forceer_c(0, [Ruiltijd], NoOnder, CatOnder, _, ResultCheck, _, _),
  ResultCheck = 0x08,
  logf('Z', w2(NoOnder,CatOnder,RuilTijd,nee,nee,' ',TT,""),nee),
	dlg_vervolgpartij_Create(ja, Win, NoOnder, CatOnder, ">> Onderliggende partij <<",nee,0,_Ret,b_True),
  vorig_plan(NoOnder, CatOnder, J1, J2, Oudetijd ), !.
gefixeerdAlsBezwaren(Win, NoOnder, CatOnder, Result, ' ') :-
  wd(NoOnder,CatOnder,T1,T2,_,_,_),
  w2(NoOnder,CatOnder,OudeTijd,J1,J2,_,_,_),
  time_stamp_next(Huidig, _Dag, _HoeLaat),
  de_spelers(NoOnder,CatOnder,T1,T2,SpelerS),
  na_vorige(NoOnder,CatOnder,T1, T2,BeginT1),
  grootste(Huidig,BeginT1,BeginTijd),
  scan_planning_c(1, BeginTijd, SpelerS, NoOnder, CatOnder, AdviesTijd, _Over),
  Result =  0x08, 
  tijd_str(AdviesTijd,Str,_),
  tijd_kwart(D2,_,_,AdviesTijd), 
  dag(D2,DStr2,_), 
  format(Tekst, "Onderliggende partij verplaatsen?\nDe onderliggende wedstrijd wordt verplaatst naar % %.", DStr2, Str),
  1 = dlg_MessageBox( "In plaats van?", Tekst, mesbox_iconexclamation, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
  time_stamp1(TT, _, _),
  loglock(mdi, "gefixeerdAlsBezwaren"),
  scan_planning_c(1, BeginTijd, SpelerS, NoOnder, CatOnder, AdviesTijd1, _),
  Adviestijd1 = Adviestijd,		% nog steeds!
  logf('Z', w2(NoOnder,CatOnder,AdviesTijd,nee,nee,' ',TT,""),nee),
  dlg_vervolgpartij_Create(ja, Win, NoOnder, CatOnder, " >> Onderliggende partij <<",nee,0,_Ret,b_True),
  vorig_plan(NoOnder, CatOnder, J1, J2, Oudetijd ), !.
gefixeerdAlsBezwaren(_, _NoOnder, _CatOnder, 0, ' ') :- !.
gefixeerdAlsBezwaren(_, _NoOnder, _CatOnder, _, 'f') :-
  1 = dlg_MessageBox( "Bezwaren", "==> Er zijn bezwaren! <==\nToch op dit tijdstip plannen?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ).

teveelWedstrijden(LboxWin, Tijd, _BackKleur, color_ltgray) :-
  teveelWeds(Tijd, _),
  win_SetForeColor(LboxWin, color_yellow), !.
teveelWedstrijden(_LboxWin, _Tijd, BackKleur, BackKleur).

toggleFixeren('f', ' ') :-
  dlg_MessageBox( "Markering", "Markering wordt verwijderd", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  !.
togglefixeren(_, 'f') :-
  dlg_MessageBox( "Markering", "Markering (F) wordt aangebracht", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  !.

transactie(No,Cat,OplTijd,Fixeren, TT, G1, G2, OTijd) :-			% als op cancel gedrukt wordt
  retract(w2(No,Cat,OTijd,G1,G2,OFixeren,OTT,OBaanNo)),	% er is oude planning
  assert(w2(No,Cat,OplTijd,nee,nee,Fixeren,TT,"")), !,	% vervang door nieuwe
  transactie1(w2(No,Cat,OTijd,G1,G2,OFixeren,OTT,OBaanNo)).
transactie(No,Cat,OplTijd,Fixeren, TT, nee, nee, 0) :-			% 
  assert(w2(No,Cat,OplTijd,nee,nee,Fixeren,TT,"")).	% nieuwe, er was geen oude planning
transactie(No,Cat,_OplTijd,_, _TT, _, _, _) :-			% als op cancel gedrukt wordt
  retractall(w2(No,Cat,_,_,_,_,_,_)),
  fail.

transactie1(_).
transactie1(w2(No,Cat,OTijd,G1,G2,Fixeren,OTT,BaanNo)) :-
  retractall(w2(No,Cat,_,_,_,_,_,_)),
  assert(w2(No,Cat,OTijd,G1,G2,Fixeren,OTT,BaanNo)),		% oude planning weer terug
  fail.

update_urg(_Win) :-
  findall(Catn,pos(Catn, _, _, _),CatL),
  %time_stamp_next(BeginTijd,  _DagNo, _Tijd),
  %Eind1 = BeginTijd + optimconst, % periode waarover geoptimaliseerd gaat worden
  sortwed_c3(1, CatL, b_True, Num, Cat, Urg, _Aantal), % 20 = 12,5%, van 1 = alle planbare
  assert(urg(_Win, Num, Cat, Urg)),
  fail.
update_urg(_) :-
  retractall(spelerdrukte(_,_,_,_,_,_,_,_,_)).

update_top(_Win) :-
  boventekens(0,0), !.
  %toolbar_SetValue(_Win, idt_2, text_valueExt(". . .",color_Black,color_White)), !.
update_top(_Win) :-
  boventekens(No, Cat),
  wc(Cat, _, _, _, _, CatNam, _, _, _, _, _, _, _, _, _),
  wd(No,Cat,A,B,_,_,_),
  not(w3(No,Cat,_,_,_,_,_)),		% gespeeld
  BetSig = nee(),
  Oproep = nee(),
  pos(Cat, Top, _, _ ),  		% 
  dim(Top, TopDim),
  dim(No,NoDim),                  	% bepaal de ronde
  repc(nee,'}',nee,Oproep,BetSig,0,TopDim,NoDim,No,Cat,A,alleplanning, Naam,_Score,_TijdLog,_,_,_,_),
  repc(nee,'}',nee,Oproep,BetSig,1,TopDim,NoDim,No,Cat,B,alleplanning, Naam1,_Score1,_TijdLog1,_,_,_,_),
  update_top1(CatNam, Naam, Naam1, Tekst),
  toolbar_SetValue(_Win, idt_2, text_valueExt(Tekst,color_Black,0xFFF0F0)), !.

update_top1(CatNam, Naam, Naam1, Tekst) :-
  concat(Naam, Naam1, Totaal),
  not(fronttoken(Totaal, _, _)),
  format(Tekst,  "zie betreffende schema ... %s", CatNam), !.
update_top1(CatNam, Naam, Naam1, Tekst) :-
  format(Tekst, "%s    %s - %s", CatNam, Naam, Naam1).

update_bottom(Win) :-
  ondertekens(_, _, _, Tekst),
  toolbar_SetValue(Win, idt_3, text_value(Tekst)), !.
  
herstelselectie(LboxWin, _IndexSListVorig) :-
  _Index = lbox_GetSelIndex(LboxWin), !.	% al geselecteerd
herstelselectie(LboxWin, [N|_]) :-
  LboxList = lbox_GetAll(LboxWin),
  selectNieuw(LboxWin, N, LboxList, 0), !.	% weer gevonden
herstelselectie(_LboxWin, _).			% niets aan te doen?

selectNieuw(LboxWin, N, [N|_], I) :-
  lbox_SetSel(LboxWin, I, b_True), !.
selectNieuw(LboxWin, N, [_|Rest], I) :-
  I1 = I + 1,
  selectNieuw(LboxWin, N, Rest, I1).

planDetails(Win, 4, VNo1, VCat1, Tijd, No1, Cat1) :-
  not(Cat1 = 0),  % te plannen
  tijd_str(Tijd,TStr1,_),
  bitright(Tijd, 7, Dag),
  dag(Dag,DStr1,_),
  format(Intro, "Tijdstip: %s %s",  DStr1 ,TStr1),
  check_forceer_c(1, [Tijd], No1, Cat1, _, Result,Bezwaren,_),
  %writedevice(X),
  %writedevice(screen),
  %write("\nBezwaren",Bezwaren),
  %writedevice(X),
  wd(No1,Cat1,A,B,_,_,_),
  de_spelers(No1, Cat1, A, B, SpL),
  opm(SpL, "", Opmerkingen1),
  baanNietVrij(Tijd, Result, VNo1, VCat1, Opmerkingen2, Opmerkingen3), % als er geen baan vrij is, kijk naar ander(en) 0x08
  win_bezwarenwin_Create(Win, Intro, Bezwaren, Result, Opmerkingen1, Opmerkingen2,Opmerkingen3),	% met bezwaren en opmerkingen.
  !.
planDetails(Win, _, _VNo1, _VCat1, _Tijd, _No1, _Cat1) :-
  win_bezwarenwin_Create(Win, "", "", 0, "", "",""), !.

baanNietVrij(Tijd, Result, No, Cat, Tekst, Opmerkingen) :- % als er geen baan vrij is, kijk naar ander(en) 0x08
  bitand(Result, 0x08, R1),
  R1 > 0,
  wd(No,Cat,T1,T2,_,_,_),               /* kijk wie er meespelen            */
  de_spelers(No,Cat,T1,T2,SpL),
  eerstvolgendeVoorDeAndere(Tijd, No, Cat, SpL, Tekst),
  opm(SpL, "", Opmerkingen), !.
baanNietVrij(_Tijd, _Result, _No, _Cat, "", "").

zetVensterFont(4, LboxWin) :-
  Font = font_Create(ff_Helvetica, [], 9),
  win_SetFont(LboxWin, Font), !.
zetVensterFont(_, LboxWin) :-
  Font = font_Create(ff_Helvetica, [], 9),
  win_SetFont(LboxWin, Font), !.

completepartij(_Win) :-
  LboxWin = win_GetCtlHandle(_Win, idci_lbagenda),
  LboxList = lbox_GetAll(LboxWin),
  member(Item, LboxList),
  wedRepr(1,Item, _Naam, _Naam2, No, Cat, _, _, _, _, _, _, _, _, _, _, _, _),
  No <> 0,
  Cat <> 0,
  baanKandidaat(No, Cat),
  fail.
%completepartij(_).

/*  Font = win_GetFont(_Win),
  _FontName = font_GetAttrs(Font, Style, Size),
  %write("\n", FontName),
  Siz1 = Size - 2,
  NewFont = font_SetAttrs(Font, Style, Siz1),
  win_SetFont(_Win, NewFont),*/

planningBezig() :-
	planselect(_Win, 4, _), 
	not(boventekens(0, 0)), !.

uitslsp(Win, Items, Sp) :-
  planSelect(Win, Mode, _),
  %str_int(SM, Mode),
  %dlg_note(SM),
  member(Str, Items),
  wedRepr(Mode,Str, _Naam, _Naam2, No, Cat, _, _, _, _, _, _, _, _, _, _, _, _),
  wd(No,Cat,T1,T2,_,_,_),
  de_spelers(No, Cat, T1, T2, Uit),
  member(Sp, Uit).


  win_uitslageninvoer_Create(_, 4, _Poule):-
	planselect(Win, 4, _),
	boventekens(0,0),			% was bezig met algemene agenda (of planning was afgelopen)
	win_SetState(Win, [wsf_Restored]), 
	trap(win_SetFocus(Win),_,true), !.
  win_uitslageninvoer_Create(_, 4, Poule):-
	planselect(Win, 4, _),
	create_toolbars(Win, 4),
	titelU(4, Titel, Poule),		% planning (opnieuw even bepalen wie)
	win_SetText(Win, Titel),
	LboxWin = win_GetCtlHandle(Win, idci_lbagenda),
        listbox_Resize(Win, LboxWin, 4),
	update_top(Win),
	update_bottom(Win),
	update_planinvoer(),
	WedsL1 = lbox_GetAll(LboxWin),
	nieuweIndex(4, LboxWin, WedsL1, Index),
	I1 = Index + 5,
	lbox_SetSel(LboxWin, I1, b_True),
	lbox_SetSel(LboxWin, Index, b_True),
	trap(win_BringToTop(Win),_, true),
	boventekens(No1, Cat1),
	ondertekens(Tijd, No, Cat, _Msg),
	planDetails(Win, 4, No, Cat, Tijd, No1, Cat1),
	!.
  win_uitslageninvoer_Create(_, Mode, _Poule):-
	Mode <> 4,
	assert(boventekens(0,0)),
	planselectNot4(Win, Mode1, _),
	Mode <> Mode1,
	win_Destroy(Win),
	fail.
  win_uitslageninvoer_Create(_, Mode, _):-
	Mode <> 4,
	planselectNot4(Win, Mode, _),
	%itemAfmU(Win, _Tab1, _Tab2, _Tab3, _Tab4, _Tab5, _Tab5a, _Tab6),
	win_Invalidate(Win),
	win_SetState(Win, [wsf_Restored]), 
	win_SetFocus(Win), !.
  win_uitslageninvoer_Create(_Parent, Mode, Poule):-
	NullWin = cast(window, 0),
	%writedevice(X),writedevice(screen),write("mode: ",Mode),writedevice(X),
	assert(planSelect(NullWin, Mode, Poule)),
	win_Create(win_uitslageninvoer_WinType,win_uitslageninvoer_RCT,win_uitslageninvoer_Title,
		   win_uitslageninvoer_Menu,_Parent,win_uitslageninvoer_Flags,win_uitslageninvoer_eh,0).


%BEGIN Uitslageninvoer, e_Create
  win_uitslageninvoer_eh(_Win,e_Create(_),0):-!,
        %write("\ne_create: ", _Win),
	NullWin = cast(window, 0),
	retract(planSelect(Nullwin, Mode, Poule)),
	assertz(planselect(_Win, Mode, Poule)),
	bepaalKolommen(_Win),
	win_SetIcon(_Win, idi_uitslagen),
	loglock(mdi,"uitslageninvoer"),
	logclose(),
%BEGIN Uitslageninvoer, InitControls, 21:58:09-2.11.2002, Code automatically updated!
	win_CreateControl(wc_LBox,rct(10,15,576,251),"",_Win,[wsf_OwnerDraw,wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_HasStrings,wsf_NoBorder],idci_lbagenda),
%END Uitslageninvoer, InitControls
%BEGIN Uitslageninvoer, ToolbarCreate, 21:58:09-2.11.2002, Code automatically updated!
%END Uitslageninvoer, ToolbarCreate
	vensterpositieL(_Win),	% als bewaard van de vorige keer
	create_toolbars(_Win, Mode),
	titelU(Mode, Titel, Poule),
	win_SetText(_Win, Titel),
	findall(WedidStr,wedstr_in_volg1(_Win,WedIdStr,Mode),WedsL1),
	LboxWin = win_GetCtlHandle(_Win, idci_lbagenda),
	zetVensterFont(Mode, LboxWin),
	lbox_Add(LboxWin, WedsL1),
	update_top(_Win),
	update_bottom(_Win),
	nieuweIndex(Mode, LboxWin, WedsL1, Index),
	I1 = Index + 5,
	lbox_SetSel(LboxWin, I1, b_True),
	lbox_SetSel(LboxWin, Index, b_True),
	boventekens(No1, Cat1),
	ondertekens(Tijd, No, Cat, _Msg),
	planDetails(_Win, Mode, No, Cat, Tijd, No1, Cat1),
	win_SetSubclassHandler(LboxWin,subclassu_eh,b_false),	% 11.12.2002 ???
	!.
%END Uitslageninvoer, e_Create

%MARK Uitslageninvoer, new events

%BEGIN Uitslageninvoer, idt_spelers
  win_uitslageninvoer_eh(_Win,e_Menu(idt_spelers,_ShiftCtlAlt),0):-
	LboxWin = win_GetCtlHandle(_Win, idci_lbagenda),
	Items = lbox_GetAll(LboxWin),
	findall(Sp, uitslsp(_Win, Items, Sp), SpList),
	setSelCrit([],SpList),
	TaskWin = vpi_GetTaskWin(),
	win_spelersel_Create(TaskWin, 16),
	spelerSelRefresh(),
	!.
%END Uitslageninvoer, idt_spelers

%BEGIN Uitslageninvoer, idt_aanwijs
  win_uitslageninvoer_eh(_Win,e_Menu(idt_aanwijs,_ShiftCtlAlt),0):-
  	boventekens(No, Cat),
  	select(Cat,No),
	!.
%END Uitslageninvoer, idt_aanwijs

%BEGIN Uitslageninvoer, idt_synchro
  win_uitslageninvoer_eh(_Win,e_Menu(idt_synchro,_ShiftCtlAlt),0):-
	uitslagendownload(b_true),
	!.
%END Uitslageninvoer, idt_synchro

%BEGIN Uitslageninvoer, id_Speler_gegevens
  win_uitslageninvoer_eh(_Win,e_Menu(id_Speler_gegevens,_ShiftCtlAlt),0):-!,
	boventekens(No, Cat), 
	wd(No, Cat, Tg1, Tg2, _, _, _),
	de_spelers(No, Cat, Tg1, Tg2, Spelers),
	setSelCrit([],Spelers),
	TaskWin = vpi_GetTaskWin(),
	win_spelersel_Create(TaskWin, 16),
	spelerSelRefresh(),
	%write("\n!!"),
	!.
%END Uitslageninvoer, id_Speler_gegevens

%BEGIN Uitslageninvoer, e_Timer
  win_uitslageninvoer_eh(_Win,e_Timer(TimerId),0):-
	retract(timerBezig(_Win, _)),
	timer_Kill(TimerId),
	!.
%END Uitslageninvoer, e_Timer

%BEGIN Uitslageninvoer, idts_overzichttop
  win_uitslageninvoer_eh(_Win,e_Menu(idts_overzichttop,_ShiftCtlAlt),0):-!,
        boventekens(No, Cat),
	TaskWin = vpi_GetTaskWin(),
	situatie(TaskWin, No, Cat, 0), 
	!.
%END Uitslageninvoer, idts_overzichttop

%BEGIN Uitslageninvoer, id_Wedstrijd_wis_plan1
  win_uitslageninvoer_eh(_Win,e_Menu(id_Wedstrijd_wis_plan1,_ShiftCtlAlt),0):-!,
	%1 = dlg_MessageBox( "Wis Plan", "Planning voor deze wedstrijd wissen?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
	LboxWin = win_GetCtlHandle(_Win, idci_lbagenda),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	wedRepr(1,Item, _Naam, _Naam2, Num, Cat, _, _, _, _, _, _, _, _, _, _, _, _),
	afgelasten(_Win,Num,Cat),
	lbox_Delete(LboxWin, Index),
	win_Invalidate(_Win),
	update_display(),
	update_situatie(),
	!.
%END Uitslageninvoer, id_Wedstrijd_wis_plan1

%BEGIN Uitslageninvoer, idt_tijdstip
  win_uitslageninvoer_eh(_Win,e_Menu(idt_tijdstip,_ShiftCtlAlt),0):-
	LboxWin = win_GetCtlHandle(_Win, idci_lbagenda),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	wedRepr(1,Item, _Naam, _Naam2, No1, Cat1, _, _, _, Tijd, _, _, _, _, _, _, _, _),
	bn(Tijd, N),
	No1 + Cat1 + N > 0,	% één van de drie > 0
	win_tijdstipextra_Create(_Win),
	!.
  win_uitslageninvoer_eh(_Win,e_Menu(idt_tijdstip,_ShiftCtlAlt),0):-
	LboxWin = win_GetCtlHandle(_Win, idci_lbagenda),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	wedRepr(1,Item, _Naam, _Naam2, No1, Cat1, _, _, _, Tijd, _, _, _, _, _, _, _, _),
	No1 = 0,
	Cat1 = 0,
	bn(Tijd, 0),
	1 = dlg_MessageBox( "Uitwijktijdstip", "Wissen?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
	logf('R', bn(Tijd, 0), nee),
	lbox_Delete(LboxWin, Index),
	win_Invalidate(_Win),
	!.
%END Uitslageninvoer, idt_tijdstip

%BEGIN Uitslageninvoer, idt_set_fixeer
  win_uitslageninvoer_eh(_Win,e_Menu(idt_set_fixeer,_ShiftCtlAlt),0):-
	planSelect(_Win,4, _),
	LboxWin = win_GetCtlHandle(_Win, idci_lbagenda),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	wedRepr(1,Item, _Naam, _Naam2, No1, Cat1, _, _, _, _, _, _, _, _, _, _, _, _),
	w2(No1,Cat1,OplTijd,G1,G2,Fixeren,TT,BaanNo),
	toggleFixeren(Fixeren, FixerenUit),
	logf('A', w2(No1,Cat1,OplTijd,G1,G2,FixerenUit,TT,BaanNo), nee),
	win_Invalidate(_Win),
	!.
%END Uitslageninvoer, idt_set_fixeer

%BEGIN Uitslageninvoer, idt_plan
  win_uitslageninvoer_eh(_Win,e_Menu(idt_plan,_ShiftCtlAlt),0):-	% alleen mode 4
	planSelect(_Win, 4, _),
	LboxWin = win_GetCtlHandle(_Win, idci_lbagenda),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	wedRepr(4,Item, _Naam, _Naam2, NoOnderliggend, CatOnderliggend, _, _, _, OplTijd, _, _, _, _, _, _, _, _),
        boventekens(No, Cat),
	not(w2(No,Cat,OplTijd,_,_,_,_,_)),	% 31.3.2003 mag niet al op dit tijdstip
	time_stamp1(TT, _, _),
	check_forceer_c(0, [OplTijd], No, Cat, _Outstring, Result, _, _TijdXX),
	bitand(Result, 0xFEFF, Rc1),
	gefixeerdAlsBezwaren(_Win, NoOnderliggend, CatOnderliggend, Rc1, Fixeren),
	logclose(),
	schort_op1(No,Cat,[],SchortList),
	undoSchortOp(SchortList),
	qsort(SchortList,SchList),
	transactie(No, Cat, OplTijd, Fixeren, TT, G1, G2, OudeTijd),	% als fail hierna zet terug
	dlg_vervolgpartij_Create(ja, _Win, No, Cat, "Vaststellen (nieuw) tijdstip",nee,0,Ret,b_True),
	Ret = idc_ok,
	herplan(_Win,SchList,ja),
	logclose(),
	vorig_plan(No, Cat, G1, G2, OudeTijd),					% update gewaarschuwd zijn
	assert(boventekens(0, 0)),
	create_toolbars(_Win, 4),
        listbox_Resize(_Win, LboxWin, 4),
	update_proj_toolbar(),
	update_waarsch(),
	update_situatie(),
	update_uitslagen("idt_plan"),
	select(Cat, No),
	win_Destroy(_Win),
  %assert(boventekens(0, 0)),
  assert(ondertekens(OplTijd, No, Cat, "---")),
  TaskWin = vpi_GetTaskWin(),
  win_uitslageninvoer_Create(TaskWin, 4, 0),
	%win_BringToTop(_Win),
	%planMetAgenda(),
	!.
  win_uitslageninvoer_eh(_Win,e_Menu(idt_plan,_ShiftCtlAlt),0):-
	planSelect(_Win, 4, _),
	boventekens(0,0),
	LboxWin = win_GetCtlHandle(_Win, idci_lbagenda),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	wedRepr(4,Item, _Naam, _Naam2, No1, Cat1, _, _, _, _, _, _, _, _, _, _, _, _),
	select(Cat1, No1),
	planMetAgenda(No1, Cat1),
	!.
  win_uitslageninvoer_eh(_Win,e_Menu(idt_plan,_ShiftCtlAlt),0):-
	planSelect(_Win, N, _),
	N <> 4,
	LboxWin = win_GetCtlHandle(_Win, idci_lbagenda),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	wedRepr(N,Item, _Naam, _Naam2, No1, Cat1, _, _, _, _, _, _, _, _, _, _, _, _),
	select(Cat1, No1),
	planMetAgenda(No1, Cat1),
	!.
  win_uitslageninvoer_eh(_Win,e_Menu(idt_plan,_ShiftCtlAlt),0):-
	!.
%END Uitslageninvoer, idt_plan

%BEGIN Uitslageninvoer, idr_schemaplan
  win_uitslageninvoer_eh(_Win,e_Menu(idr_schemaplan,_ShiftCtlAlt),0):-	% van top-toolbar (mode 4) only
        boventekens(No, Cat),
	select(Cat, No),
	!.
%END Uitslageninvoer, idr_schemaplan

%BEGIN Uitslageninvoer, e_GetFocus
  win_uitslageninvoer_eh(_Win,e_GetFocus,0):-
	LboxWin = win_GetCtlHandle(_Win, idci_lbagenda),
	win_SetFocus(LboxWin),
	!.
%END Uitslageninvoer, e_GetFocus

%BEGIN Uitslageninvoer, idci_lbagenda selchanged
  win_uitslageninvoer_eh(_Win,e_Control(idci_lbagenda,_CtrlType,_CtrlWin,selchanged),0):-
	win_PostEvent(_Win,e_User(upd_toolb, 0)),
	not(planSelect(_Win, 4, _)),
	!.
  win_uitslageninvoer_eh(_Win,e_Control(idci_lbagenda,_CtrlType,LboxWin,selchanged),0):-
	planSelect(_Win, 4, _),
	win_tijdstipextra_Update(),			% laat het met de dag meelopen
	LboxWin = win_GetCtlHandle(_Win, idci_lbagenda),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	wedRepr(4,Item, _Naam, _Naam2, VNo1, VCat1, _, _, _, Tijd, _, _, _, _, _, _, _, _),
	%write("\nSelect: ", VCat1, "No ", VNo1),
	select(VCat1, VNo1),	% 25.8.2004
	%win_SetFocus(LboxWin),
	boventekens(No1, Cat1),
	planDetails(_Win, 4, VNo1, VCat1, Tijd, No1, Cat1),
	toolbar_SetValue(_Win, idt_3, text_value("")),
	trap(win_BringToTop(_Win),_,true),
	!.
  win_uitslageninvoer_eh(_Win,e_Control(idci_lbagenda,_CtrlType,_LboxWin,selchanged),0):-
	win_bezwarenwin_Create(_Win, "", "", 0, "", "", ""),	% haal eventueel bezwarenwin weg
	boventekens(0, 0),
	toolbar_SetValue(_Win, idt_2, text_value("")), !.% planning top = ""
%END Uitslageninvoer, idci_lbagenda selchanged

%BEGIN Uitslageninvoer, idr_schemaeen
  win_uitslageninvoer_eh(_Win,e_Menu(idr_schemaeen,_ShiftCtlAlt),0):-
	LboxWin = win_GetCtlHandle(_Win, idci_lbagenda),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	wedRepr(1,Item, _Naam, _Naam2, No1, Cat1, _, _, _, _, _, _, _, _, _, _, _, _),
	select(Cat1, No1),
	update_waarsch(),
	update_situatie(),
	update_uitslagen("idr_schemaeen"), !.
%END Uitslageninvoer, idr_schemaeen

%BEGIN Uitslageninvoer, id_Wedstrijd_uitslag_invoeren
  win_uitslageninvoer_eh(_Win,e_Menu(id_Wedstrijd_uitslag_invoeren,_ShiftCtlAlt),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idci_lbagenda),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	wedRepr(1,Item, _Naam, _Naam2, No1, Cat1, _, _, _, _, _, _, _, _, _, _, _, _),
	invoerbaar(No1, Cat1, Inv),
	Inv = b_True,
	select(Cat1, No1),
	dlg_uitslaginvoer1_Create(_Win, No1, Cat1, b_False),
	win_BringToTop(_Win),
	w3(No1,Cat1,_,_,_,_,_),		% er is een uitslag ingevoerd
	lbox_SetSel(LboxWin, 0, b_True),
	%geenWedstrijdGeselecteerd(),
	!.
%END Uitslageninvoer, id_Wedstrijd_uitslag_invoeren

%BEGIN Uitslageninvoer, e_MouseDown
  win_uitslageninvoer_eh(_Win,e_MouseDown(_XPNT,_ShiftCtlAlt,mouse_button_right),0):-
	not(planSelect(_Win, 2, _)),
	win_PostEvent(_Win,e_Menu(idts_popup,0)),
	!.
%END Uitslageninvoer, e_MouseDown

  win_uitslageninvoer_eh(_Win,e_Menu(id_Kleur1,_ShiftCtlAlt),0):-
	kleurenBRead(InitColor, B),
	%InitColor=color_Red,
	NewCOLOR = dlg_ChooseColor(_Win, InitCOLOR),
	kleurenBUpdate(NewColor, B),
	win_Invalidate(_Win), 
	!.
  win_uitslageninvoer_eh(_Win,e_Menu(id_Kleur2,_ShiftCtlAlt),0):-
	kleurenBRead(A, InitColor),
	%InitColor=color_Red,
	NewCOLOR = dlg_ChooseColor(_Win, InitCOLOR),
	kleurenBUpdate(A, NewColor),
	win_Invalidate(_Win), 
	!.
  win_uitslageninvoer_eh(_Win,e_Menu(id_KleurR,_ShiftCtlAlt),0):-
	1 = dlg_MessageBox( "Planningkleuren", "Weer terugzetten naar standaardwaarde?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
	retractall(kleurenB(_, _)),
	win_Invalidate(_Win), 
	!.
  win_uitslageninvoer_eh(_Win,e_Menu(id_Wedstrijd_plan,_ShiftCtlAlt),0):-
	LboxWin = win_GetCtlHandle(_Win, idci_lbagenda),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	wedRepr(1,Item, _Naam, _Naam2, No1, Cat1, _, _, _, _, _, _, _, _, _, _, _, _),
	TaskWin = vpi_GetTaskWin(),
	planAangewezenWeds(TaskWin, No1, Cat1),
	update_waarsch(),
	update_situatie(),
	update_uitslagen("id_weds_plan"), !.
  win_uitslageninvoer_eh(_Win,e_Menu(idr_baannummer,_ShiftCtlAlt),0):-
	LboxWin = win_GetCtlHandle(_Win, idci_lbagenda),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	wedRepr(1,Item, _Naam, _Naam2, No1, Cat1, _, _, _, _, _, _, _, _, _, _, _, _),
	%TaskWin = vpi_GetTaskWin(),
	%situatie(TaskWin, No1, Cat1, 0), 
	dlg_baannummer_Create(_Win, No1, Cat1),
	win_Invalidate(_Win), !.
  win_uitslageninvoer_eh(_Win,e_Menu(ID,_ShiftCtlAlt),0):-
	SKeus = ID - id_menufuzz,
	SKeus > 0,
	sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Res),
	SpelerIn = sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Res), !,
	dlg_persoonsgegevens_Create(_Win, SpelerIn ).


%BEGIN Uitslageninvoer, idts_popup
  win_uitslageninvoer_eh(_Win,e_Menu(idts_popup,_ShiftCtlAlt),0):-!,
	cursorrect(V),
	LboxWin = win_GetCtlHandle(_Win, idci_lbagenda),
	RCT = win_GetOuterRect(LboxWin),
	RCT = rct(L, _T, R, _B),
	X = (L + R) div 2,
	PNT = pnt(X, V),
	%PNT = cursor_GetPos(_Win),
	popUpU(_Win, PNT),
	!.
%END Uitslageninvoer, idts_popup

%BEGIN Uitslageninvoer, id_Onderdeel_print
  win_uitslageninvoer_eh(_Win,e_Menu(id_Onderdeel_print,_ShiftCtlAlt),0):-
	%write("hier"),
	planSelect(_Win,2, _),
	dagrapport(File),
	format(DagBestand, "file:\\\\%s", File),
	Null = cast(api_hwnd,0),
	api_ShellExecute(Null,"open",Dagbestand,"","",1), !.		% open in browser
  win_uitslageninvoer_eh(_Win,e_Menu(id_Onderdeel_print,_ShiftCtlAlt),0):-!,
	dlg_agenda_Create(_Win, 0),
	!.
%END Uitslageninvoer, id_Onderdeel_print

%BEGIN Uitslageninvoer, idts_overzicht
  win_uitslageninvoer_eh(_Win,e_Menu(idts_overzicht,_ShiftCtlAlt),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idci_lbagenda),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	wedRepr(1,Item, _Naam, _Naam2, No1, Cat1, _, _, _, _, _, _, _, _, _, _, _, _),
	TaskWin = vpi_GetTaskWin(),
	%write("\n### situatie!!"), 
	situatie(TaskWin, No1, Cat1, 0),
	!.
%END Uitslageninvoer, idts_overzicht


%BEGIN Uitslageninvoer, e_Char
  win_uitslageninvoer_eh(_Win,e_Char(k_f9,_ShiftCtlAlt),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idci_lbagenda),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	wedRepr(1,Item, _Naam, _Naam2, No1, Cat1, _, _, _, _, _, _, _, _, _, _, _, _),
	select(Cat1, No1),
	dlg_uitslaginvoer1_Create(_Win, No1, Cat1, b_False),
	win_Invalidate(LboxWin),
	!.
  win_uitslageninvoer_eh(_Win,e_Char(k_f7,_ShiftCtlAlt),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idci_lbagenda),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	wedRepr(1,Item, _Naam, _Naam2, No1, Cat1, _, _, _, _, _, _, _, _, _, _, _, _),
	TaskWin = vpi_GetTaskWin(),
	situatie(TaskWin, No1, Cat1, 0), 
	!.
  win_uitslageninvoer_eh(_Win,e_Char(Getal,_ShiftCtlAlt),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idci_lbagenda),
	LboxList = lbox_GetAll(LboxWin),
	NoOfItems = lbox_CountAll(LboxWin),
	Teken = cast(char, Getal),
	zoekTeken(_Win, Teken, LboxList, NoOfItems, NewPos, inLijst),
	lbox_SetSel(LboxWin, NewPos, b_True),
	!.
  win_uitslageninvoer_eh(_Win,e_Char(k_f5,_ShiftCtlAlt),0):-
	win_PostEvent(_Win,e_Menu(id_Wedstrijd_plan, 0)), !.
  win_uitslageninvoer_eh(_Win,e_Char(k_f6,_ShiftCtlAlt),0):-
	win_PostEvent(_Win,e_Menu(id_Wedstrijd_fixeer, 0)), !.
%END Uitslageninvoer, e_Char

%BEGIN Uitslageninvoer, e_Update
  win_uitslageninvoer_eh(_Win,e_Update(_UpdateRct),0):-!,
	win_PostEvent(_Win,e_User(3,0)),
	!.
%END Uitslageninvoer, e_Update

%BEGIN Uitslageninvoer, e_Destroy
  win_uitslageninvoer_eh(_Win,e_Destroy,0):-
	planSelect(_Win, 2, _),	% uitslageninvoer
	CtrlWin = win_GetCtlHandle(_Win, idci_lbagenda),
	retractall(hotSpelerSpot(CtrlWin, _, _, _, _)),
	retractall(hotBaanSpot(CtrlWin, _, _, _)),
        toolbar_GetCtlHandle(_Win,tb_moveable("",pnt(0,0)),TB_Win),  % wordt die nog gebruikt? ##################
	RCT = win_GetOuterRect(TB_Win),
	WSFlags = win_GetState(TB_Win),
	ClientRCT = rect_GetClient(WSFlags,b_False, RCT),
        retractall(vensterRCT(banentoolvenster, _)),
        assert(vensterRCT(banentoolvenster, ClientRCT)),
	fail.
  win_uitslageninvoer_eh(_Win,e_Destroy,0):-
	%retractall(bezwarenwinTekst(_, _, _, _, _, _, _, _, _)),	% 23.11.2010 voor de zekerheid
        retractall(editcontroltext(idc_planning, _, _, _,_,_,_)),
        retractall(notaVeld),
	retract(timerBezig(_Win, Id)),
	timer_Kill(Id),
	fail.
  win_uitslageninvoer_eh(_Win,e_Destroy,0):-!,
	save_UitslWin(),
	retract(itemAfmU(_Win, _Tab1, _Tab2, _Tab3, _Tab4, _Tab5, _Tab5a, _Tab6)),
	retractall(urg(_Win, _, _, _)),
	retractall(verzet(_Win, _, _, _, _, _)),
	retractall(teveelWeds(_,_)),
	retractall(planSelect(_Win, _, _)),	% met de bijbehorende mode
	!.
%END Uitslageninvoer, e_Destroy

%BEGIN Uitslageninvoer, e_User
  win_uitslageninvoer_eh(_Win,e_User(1,_Ptr),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idci_lbagenda),
	win_SetFocus(LboxWin),
	!.
  win_uitslageninvoer_eh(_Win,e_User(3,_),0):-		% invalidate/e_update
	planSelect(_Win,4, _Poule),			% helemaal
	not(timerBezig(_Win, _)),
	retractall(urg(_Win, _, _, _)),
	retractall(verzet(_Win, _, _, _, _, _)),
	retractall(teveelWeds(_,_)),
	retractall(bezwColor(_,_)),
	update_urg(_Win),
	NewTimerId = timer_Set(_Win, 30000),
	assert(timerBezig(_Win, NewTimerId)),		% niet opnieuw binnen twee seconden
	fail.
  win_uitslageninvoer_eh(_Win,e_User(3,_),0):-		% invalidate/e_update
	planSelect(_Win,4, _Poule),
	%planSelect(_Win,Mode, _Poule),
	findall(WedidStr,wedstr_in_volg1(_Win,WedIdStr,4),WedsL1),
	LboxWin = win_GetCtlHandle(_Win, idci_lbagenda),
	lbox_GetSel(LboxWin,IndexSListVorig,_),
	LboxList = lbox_GetAll(LboxWin),
	lbox_Suspend(LboxWin),
	comparevolg(LboxWin, WedsL1, LboxList, 0),
	lbox_Resume(LboxWin),
	herstelselectie(LboxWin, IndexSListVorig),
	win_PostEvent(_Win,e_User(upd_toolb, 0)), !.
  win_uitslageninvoer_eh(_Win,e_User(3,_),0):-		% invalidate/e_update
	planSelect(_Win,2, _Poule),		%  2 = uitslagen 
	%maxBaanBreedte(_Win, _Breedte),
	win_banentool_Create(_Win),
	LboxWin = win_GetCtlHandle(_Win, idci_lbagenda),
	%Parent = win_GetParent(_Win),
	alignAanwezig(LboxWin),
	findall(WedidStr,wedstr_in_volg1(_Win,WedIdStr,2),WedsL1),
	lbox_GetSel(LboxWin,IndexSListVorig,_),
	LboxList = lbox_GetAll(LboxWin),
	comparevolg(LboxWin, WedsL1, LboxList, 0),
	herstelselectie(LboxWin, IndexSListVorig),
	%comparevolg(LboxWin, WedsL1, LboxList, 0),
	%herstelselectie(LboxWin, IndexSListVorig),
	win_PostEvent(_Win,e_User(upd_toolb, 0)), !.
  win_uitslageninvoer_eh(_Win,e_User(3,_),0):-		% invalidate/e_update
	planSelect(_Win,Mode, _Poule),		% 0 = te plannen 1 = alle 2 = uitslagen 3 = per poule 4 = agenda+
	Mode <> 4,
	Mode <> 2,
	findall(WedidStr,wedstr_in_volg1(_Win,WedIdStr,Mode),WedsL1),
	LboxWin = win_GetCtlHandle(_Win, idci_lbagenda),
	lbox_Suspend(LboxWin),
	lbox_Clear(LboxWin),
	lbox_Add(LboxWin, WedsL1),
	lbox_Resume(LboxWin),
	%win_SetSubclassHandler(LboxWin,subclassu_eh,b_false),
	win_PostEvent(_Win,e_User(upd_toolb, 0)),
	lbox_SetSel(LboxWin, 0, b_True), !.
  win_uitslageninvoer_eh(_Win,e_User(11,_),0):-			% kijk of er een complete partij bijgekomen is
  	completepartij(_Win), !.
	%LboxWin = win_GetCtlHandle(_Win, idci_lbagenda),
	%LboxList = lbox_GetAll(LboxWin),
	%member(Item, LboxList),
	%wedRepr(1,Item, _Naam, _Naam2, No, Cat, _, _, _, _, _, _, _, _, _, _, _, _),
	%No <> 0,
	%Cat <> 0,
	%baanKandidaat(No, Cat),
	%fail.
  %win_uitslageninvoer_eh(_Win,e_User(11,_),0):-	!.
  win_uitslageninvoer_eh(_Win,e_User(updatedagstatus,_Ptr),0):-
 	huidig(Dag,_),
        dagrapport2(Dag),
	!.
  win_uitslageninvoer_eh(_Win,e_User(upd_toolb,_Index),0):- 	% upd_toolb  uitslag
	LboxWin = win_GetCtlHandle(_Win, idci_lbagenda),
	toolbar_SetValue(_Win, id_Wedstrijd_uitslag_invoeren, ctrl_value(0,1)),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	wedRepr(1,Item, _Naam, _Naam2, No1, Cat1, _, _, _, _, _, _, _, _, _, _, _, _),
	invoerbaar(No1, Cat1, Inv),
	Inv = b_True,
	toolbar_SetValue(_Win, id_Wedstrijd_uitslag_invoeren, ctrl_value(1,1)),
	fail.
  win_uitslageninvoer_eh(_Win,e_User(upd_toolb,_Index),0):- % upd_toolb  plan
	planSelect(_Win,4, _Poule),
	LboxWin = win_GetCtlHandle(_Win, idci_lbagenda),
	toolbar_SetValue(_Win, idt_plan, ctrl_value(1,1)),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	wedRepr(1,Item, _Naam, _Naam2, _, _, _, _, _, Tijd1, _, _, _, _, _, _, _, _),
	boventekens(No1, Cat1),
	w2(No1,Cat1,Tijd1,_,_,_,_,_),				% zelfde tijdstip!
	toolbar_SetValue(_Win, idt_plan, ctrl_value(0,1)),
	fail.
  win_uitslageninvoer_eh(_Win,e_User(upd_toolb,_Index),0):-
	LboxWin = win_GetCtlHandle(_Win, idci_lbagenda),
	toolbar_SetValue(_Win, idr_schemaeen, ctrl_value(0,1)),		% schema
	toolbar_SetValue(_Win, idts_overzicht, ctrl_value(0,1)),	% situatie
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	wedRepr(1,Item, _Naam, _Naam2, No1, Cat1, _, _, _, _, _, _, _, _, _, _, _, _),
	No1 <> 0,
	Cat1 <> 0,
	toolbar_SetValue(_Win, idr_schemaeen, ctrl_value(1,1)),
	toolbar_SetValue(_Win, idts_overzicht, ctrl_value(1,1)),
	fail.
  win_uitslageninvoer_eh(_Win,e_User(upd_toolb,_Index),0):-
	LboxWin = win_GetCtlHandle(_Win, idci_lbagenda),
	toolbar_SetValue(_Win, idt_set_fixeer, ctrl_value(0,1)),	% fixeer/markeer
	toolbar_SetValue(_Win, id_Wedstrijd_wis_plan1, ctrl_value(0,1)),% wis plan
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	wedRepr(1,Item, _Naam, _Naam2, _No1, Cat1, _, _, _, _, _, _, _, _, _, _, _, _),
	Cat1 > 0,
	toolbar_SetValue(_Win, idt_set_fixeer, ctrl_value(1,1)),
	toolbar_SetValue(_Win, id_Wedstrijd_wis_plan1, ctrl_value(1,1)),
	fail.
  win_uitslageninvoer_eh(_Win,e_User(upd_toolb,_Index),0):-
	!.
%END Uitslageninvoer, e_User

%BEGIN Uitslageninvoer, idci_lbagenda activated
  win_uitslageninvoer_eh(_Win,e_Control(idci_lbagenda,_CtrlType,LboxWin,activated),0):-
	planSelect(_Win,2, _),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	wedRepr(1,Item, _Naam, _Naam2, No1, Cat1, _, _, _, _, _, _, _, _, _, _, _, _),
	invoerbaar(No1, Cat1, Inv),
	Inv = b_True,
	select(Cat1, No1),
	dlg_uitslaginvoer1_Create(_Win, No1, Cat1, b_False),
	win_Invalidate(_Win),
	!.
  win_uitslageninvoer_eh(_Win,e_Control(idci_lbagenda,_CtrlType,_LboxWin,activated),0):-
	planSelect(_Win,4, _),
	win_PostEvent(_Win,e_menu(idt_plan, 0)),
	!.
  win_uitslageninvoer_eh(_Win,e_Control(idci_lbagenda,_CtrlType,LboxWin,activated),0):-
	planSelect(_Win,X, _),
	X <> 2,
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	wedRepr(1,Item, _Naam, _Naam2, No1, Cat1, _, _, _, _, _, _, _, _, _, _, _, _),
	select(Cat1, No1),
	win_BringToTop(_Win),
	win_PostEvent(_Win,e_Menu(idts_popup,0)),
	!.
  win_uitslageninvoer_eh(_Win,e_Control(idci_lbagenda,_CtrlType,LboxWin,activated),0):-
	planSelect(_Win,2, _), 
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	wedRepr(1,Item, _Naam, _Naam2, No1, Cat1, _, _, _, _, _, _, _, _, _, _, _, _),
	select(Cat1, No1), !.
%END Uitslageninvoer, idci_lbagenda activated

%BEGIN Uitslageninvoer, e_OwnerMeasureItem
  win_uitslageninvoer_eh(_Win,e_OwnerMeasureItem(_CtlType,_CtlId,_ItemId,_Data),Size):-
	win_GetTextExtent(_Win, "JansenenTilanusg", -1, Width, He),
	Height = He,
	bitleft(Height, 16, H1),
	Size = Width + H1,
	%write("\nOwnerdrawMeasure uitslagen!"),
	!.
%END Uitslageninvoer, e_OwnerMeasureItem

%BEGIN Uitslageninvoer, e_OwnerDraw
  win_uitslageninvoer_eh(_Win,e_OwnerDraw(_CtlType,_CtlId,_ItemId,_ItemAction,
		ItemState,LboxWin,RectItem,_ItemData),0):-
		own_member1(ItemState, odstate_Selected), !,  % was selected
	RectItem = rct(_, T, _, _B),
	assert(cursorrect(T)),
	ownerdraw1(_Win, LboxWin, RectItem, _ItemId, color_White, b_True),
	!.
  win_uitslageninvoer_eh(_Win,e_OwnerDraw(_CtlType,_CtlId,_ItemId,_ItemAction,
		_ItemState,LboxWin,RectItem,_ItemData),0):-
	_Itemstate = [],
	ownerdraw1(_Win, LboxWin, RectItem, _ItemId, color_LtGray, b_False),
	!.
%END Uitslageninvoer, e_OwnerDraw

%BEGIN Uitslageninvoer, e_Size
  win_uitslageninvoer_eh(_Win,e_Size(Width,_Height),0):-
ifdef use_tbar
	toolbar_Resize(_Win),
enddef
	planSelect(_Win, Mode, _),
	LboxWin = win_GetCtlHandle(_Win, idci_lbagenda),
	listbox_Resize(_Win, LboxWin, Mode),
	banenToolMove(Width),
	win_Invalidate(_Win),
	!.
%END Uitslageninvoer, e_Size

%BEGIN Uitslageninvoer, e_Menu, Parent window 
  win_uitslageninvoer_eh(Win,e_Menu(ID,CAS),0):-!,
	PARENT = win_GetParent(Win),
	win_SendEvent(PARENT,e_Menu(ID,CAS)),
	!.
%END Uitslageninvoer, e_Menu, Parent window

%END_WIN Uitslageninvoer

%kleuren(Win, Selected, No, Cat, Tijd, color Pen, color Fore, color Back, color Brush, color Park)
kleuren(_Win, b_True, _No, _Cat, _Tijd, color_Blue, color_White, color_Blue, color_Blue, color_Blue, color_White) :- !.
%kleuren(_Win, _, No, Cat, color_LtGray, color_Black, color_LtGray, color_LtGray, color_White) :- 
%  tePlannenL(No, Cat, _, _, _, _), !.		% geen bezwarenkolom
kleuren(_Win, _, 0, 0, Tijd, color_White, color_Black, 0xA6FFD2, color_White, color_White, 0x408000 /* parkkleur! */) :-
  planSelect(_Win, 4, _),				% voor planning
  bezwColor(Tijd, Retcode), 
  bitand(Retcode, 0xFEFF, Rc1),
  Rc1 = 0, !.
kleuren(_Win, _, No, Cat, Tijd, color_White, color_Black, 0xDDFFFF, color_White, color_White, 0x408000) :-
  planSelect(_Win, 4, _),				% voor planning
  bezwColor(Tijd, Result),
  bitand(Result, 0xFEFF, Rc1),
  Rc1 = 0x08,	% alleen bezwaar: geen baan beschikbaar!
  verzet(_Win, No, Cat, _Tekst, _Kleur, 1), !.	% ruilbaar
kleuren(_Win, _, _No, _Cat, Tijd, color_White, color_Black, 0xFDFDD5/*0x9CE2E4*/, color_White, color_White, 0x408000) :-
  planSelect(_Win, 4, _),				% voor planning alleen
  bezwColor(Tijd, Result),
  Result = 0x08, !.
kleuren(_Win, _, No, Cat, Tijd, color_Blue, color_Black, color_White, color_White, color_White, 0x408000) :- 
  planSelect(_Win, 4, _),				% voor planning alleen
  boventekens(No, Cat),
  ondertekens(Tijd,No, Cat, _), !.
kleuren(_Win, _, No, Cat, Tijd, color_LtGray, color_Black, color_White, color_White, color_White, 0x408000) :- 
  planSelect(_Win, 4, _),				% voor planning alleen
  %boventekens(0, 0),
  ondertekens(Tijd,No, Cat, _), !.
kleuren(_Win, _, _No, _Cat, _Tijd, color_White, color_Black, color_White, color_White, color_White, 0x408000).

bepaalKolommen(Win) :-
  retractall(itemAfmU(Win, _, _, _, _, _, _, _)),
  planSelect(Win, 4, _),				% voor planning
  win_GetTextExtent(Win, " ", -1, Tblank, _),
  maxDatL(Win, Tab1),
  Tab1a = Tab1, % + Tblank,
  win_GetTextExtent(Win, "23u25", -1, T3, _),
  Tab2 = Tab1a + T3 + Tblank,
  maxParkL(Win, ParkL, Tblank, T4Delta),
  Tab3 = Tab2 + ParkL + T4Delta + Tblank,
  Tab4 = Tab3 + 70 + Tblank,	% 26.2.2017
  maxOndLShort(Win, 4, OndL),
  Tab5 = Tab4 + OndL + Tblank,
  assert(itemAfmU(Win, Tab1a, Tab2, Tab3, Tab4, Tab5, Tblank, ParkL)), !.
bepaalKolommen(Win) :-
  planSelect(Win, 0, _),				% F4 nog te plannen
  win_GetTextExtent(Win, " ", -1, Tblank, _),
  Tab1a = 0, % + Tblank,
  Tab2 = 0,
  Tab3 = 0,
  Tab4 =  Tblank,
  ParkL = 0,
  maxOndLshort(Win, 0, OndLen),
  Tab5 = Tab4 + OndLen + Tblank,
  %str_int(Note, Tab5),
  %dlg_note("planselect 0", Note),
  assert(itemAfmU(Win, Tab1a, Tab2, Tab3, Tab4, Tab5, Tblank, ParkL)), !.
bepaalKolommen(Win) :-
  planselect(Win, Mode, _),
  win_GetTextExtent(Win, " ", -1, Tblank, _),
  maxDatL(Win, Tab1),
  Tab1a = Tab1, % + Tblank,
  win_GetTextExtent(Win, "23u25", -1, T3, _),
  Tab2 = Tab1a + T3 + Tblank,
  maxParkL(Win, ParkL, Tblank, T4Delta),
  Tab3 = Tab2 + ParkL + T4Delta + Tblank,
  Tab4 = Tab3 + 15 + Tblank,
  maxOndLshort(Win, Mode, OndL),
  Tab5 = Tab4 + OndL + Tblank,
  %writedevice(X), writedevice(screen),write("\nTab4: ", Tab4,", tab5: ",Tab5, ", OndL: ",OndL),writedevice(X),
  assert(itemAfmU(Win, Tab1a, Tab2, Tab3, Tab4, Tab5, Tblank, ParkL)), !.

get_urg(_Win, 4, LboxWin, T, B, Tab5, _No, _Cat, Rechts, Kleur) :-	% zet eerst op groen
  X2 = Tab5 + Rechts + 2,
  %planSelect(Win, 4, _),
  win_SetBrush(LboxWin,brush(pat_Solid, Kleur)),	% 16.3.2005
  win_SetPen(LboxWin,pen(1,ps_Solid,Kleur)),
  draw_Rect(LboxWin,rct(Tab5, T, X2, B)),
  fail.
get_urg(Win, 4, LboxWin, T, B, Tab5, Num, Cat, Rechts, Kleur) :-
  X1 = Tab5,
  X2 = Tab5 + Rechts,
  urg(Win, Num, Cat, _Urg),				% het is een urgente wedstrijd ==> rood
  win_SetForeColor(LboxWin, color_Red),
  win_SetBackColor(LboxWin, Kleur),
  draw_TextInRect(LboxWin,rct(X1,T,X2,B), "urg", -1,[dtext_right]),
  fail.
get_urg(_Win, 4, LboxWin, T, B, Tab5, No, Cat, Rechts, _) :-
  w2(No,Cat,_Tijd,ja,_,_,_,_),				% de ene helft is al gewaarschuwd ==> zwart
  X1 = Tab5 + Rechts,
  X2 = X1 + 1,
  win_SetBrush(LboxWin,brush(pat_Solid, color_Black)),
  win_SetPen(LboxWin,pen(1,ps_Solid,color_Black)),
  draw_Rect(LboxWin,rct(X1, T, X2, B)),
  fail.
get_urg(_Win, 4, LboxWin, T, B, Tab5, No, Cat, Rechts, _) :-
  w2(No,Cat,_Tijd,_,ja,_,_,_),				% de andere helft is gewaarschuwd ==> zwart
  X1 = Tab5 + Rechts + 1,
  X2 = X1 + 1,
  win_SetBrush(LboxWin,brush(pat_Solid, color_Black)),
  win_SetPen(LboxWin,pen(1,ps_Solid,color_Black)),
  draw_Rect(LboxWin,rct(X1, T, X2, B)),
  fail.
get_urg(_Win, 4, LboxWin, T, B, Tab5, No, Cat, Rechts, Kleur) :-
  %planSelect(Win, 4, _),
  win_GetTextExtent(LboxWin, "F", -1, Width, _Height),	% geef gefixeerd weer
  X1 = Tab5 + Rechts + 2,
  X2 = X1 + Width,
  win_SetBrush(LboxWin,brush(pat_Solid, Kleur)),
  win_SetPen(LboxWin,pen(1,ps_Solid,Kleur)),
  draw_Rect(LboxWin,rct(X1, T, X2, B)),
  win_SetBackColor(LboxWin, Kleur),
  win_SetForeColor(LboxWin, color_Black),
  getbacktrack(Here),
  w2(No,Cat,_,_,_,F,_,_),
  cutbacktrack(Here),
  upper_lower(FU, F),
  format(F1, "%", FU),
  draw_Rect(LboxWin,rct(X1, T, X2, B)),
  draw_TextInRect(LboxWin,rct(X1,T,X2,B), F1, -1,[dtext_right]),
  fail.
get_urg(_Win, 4, LboxWin, T, B, Tab5, No, Cat, Rechts, _) :-
  win_GetTextExtent(LboxWin, "F", -1, Width, _),
  Font = font_Create(ff_Times, [], 8),
  win_SetFont(LboxWin, Font),
  X1 = Tab5 + Rechts + 2 + Width,
  win_GetTextExtent(_Win, "11", -1, Width1, _),
  X2 = X1 + Width1,
  verzet(_Win, No, Cat, Tekst, Kleur, _),
  win_SetForeColor(LboxWin, Kleur),
  draw_Rect(LboxWin,rct(X1, T, X2, B)),			% zet eerst alles groen
  draw_TextInRect(LboxWin,rct(X1,T,X2,B), Tekst, -1,[dtext_right]),
  !.
get_urg(_Win, _, _LboxWin, _T, _B, _Tab5, _No, _Cat, _, _).

rectAfm(Win, T, B, R, RectDag, RectTijd, RectPark, RectCat, TUrg, TGradatie, RectNaam1, RectNaam2, BaanParkL) :-
  itemAfmU(Win, Tab1, Tab2, Tab3, Tab4, Tab5, Tblank, BaanParkL), !,
  RectDag = rct(0, T, Tab1, B),
  RectTijd = rct(Tab1, T, Tab2, B),
  TPark = Tab2 + Tblank,
  RectPark = rct(Tpark, T, Tab3, B),
  TGradatie = Tab3,
  TUrg = (Tab3 + Tab4) div 2 - Tblank - Tblank - Tblank,
  RectCat = rct(Tab4, T, Tab5, B),
  TNaamL = (R + Tab5) div 2 - 4 - Tblank,
  RectNaam1 = rct(Tab5, T, TNaamL, B),
  TNaamR = TNaamL + 8,
  %writedevice(Xw),  writedevice(screen),  format(Nota, "tab4: %d tab5: %d tnaamL: %d rectpark(L:%d,T:%d,R:%d,B:%d) \trectCat(L:%d,T:%d,R:%d,B:%d)\trectNaam1: (L:%d,T:%d,R:%d,B:%d)",Tab4, Tab5, TnaamL, Tpark, T, Tab3, B, Tab4, T, Tab5,B, Tab5, T, TnaamL,B),
  %write("\n",Nota), writedevice(Xw),  
  RectNaam2 = rct(TNaamR, T, R, B).

streepjedag(LboxWin, ItemId, Tijd, rct(DL, _DT, DR, DB), RctTijd) :-
  ItemIdNext = ItemId + 1,
  ItemNext = lbox_GetItem(LboxWin, ItemIdNext),	% get the string
  fronttoken(ItemNext, _R1, R2),
  fronttoken(R2, _, R3),
  fronttoken(R3, TijdNextStr, _),		% derde token is tijd
  str_int(TijdNextStr, TijdN),
  huidigeTijd(TijdHuidig),
  zonnetje(LboxWin, Tijd, TijdHuidig),
  streepjeTijd(LboxWin, Tijd, TijdN, RctTijd), 
  bitright(TijdN, 7, DagN),
  bitright(Tijd, 7, Dag),
  Dag <> DagN,
  DB1 = DB - 1,
  draw_Line(LboxWin,pnt(DL,DB1),pnt(DR, DB1)), !.
streepjedag(_LboxWin, _ItemId, _Tijd, _, _).

zonnetje(LboxWin, TijdHuidig, TijdHuidig) :-
  win_SetPen(LboxWin,pen(2,ps_Solid,color_yellow)), !.	% zet een geel streepje
zonnetje(LboxWin, _, _) :-
  win_SetPen(LboxWin,pen(1,ps_Solid,color_black)).
 
streepjeTijd(LboxWin, Tijd, TijdN, rct(TL, _TT, TR, TB)) :-
  Tijd <> TijdN, !,
  TB1 = TB - 1,
  draw_Line(LboxWin,pnt(TL,TB1),pnt(TR, TB1)).
streepjeTijd(_, _Tijd, _TijdN, _RctTijd).

aanstrepen(2, ja, _No, _Cat, SpNo, BackGround, BackGround, _, color_Yellow,b_True) :-
  spelerAanwezig(SpNo, _Tijd, _, _), !.
aanstrepen(2, ja, No, Cat, SpNo, _BackGround, color_Yellow, ForeGround, ForeGround,b_False) :-
  spelerAanwezig(SpNo, _Tijd, No, Cat), !.
aanstrepen(2, ja, No, Cat, SpNo, _BackGround, 0xFFD7EB/*0xC1FFFF*/, ForeGround, ForeGround,b_False) :-
  spelerAanwezig(SpNo, _, _, _),
  w2(No,Cat,Tijd,_,_,_,_,_),
  tijd_kwart(Dag, _Uur, _Kwartier, Tijd),
  huidig(D1, _),
  Dag = D1, !.
aanstrepen(_, _, _, _, _, BackGround,BackGround, ForeGround, ForeGround,_).

aanwezigNaam(NaamA1, SpNo, NaamA1a) :-	% spatie voor en achter
  spelerAanwezig(SpNo, _Tijd, _, _),
  format(NaamA1a, " %s ",NaamA1),  !.
aanwezigNaam(NaamA1, _SpNo, NaamA1).

verwijderFantoomSpots(LboxWin, 2, ja, T, B) :-
  retractall(hotSpelerSpot(LboxWin, _, _, _, rct(_, T, _, B))), !.
verwijderFantoomSpots(_LboxWin, _, _, _T, _B).

drawNamenInRect(Mode, Banentool, No1, Cat1, LboxWin, rct(L,T,R,B), _Namen1, NaamA1, NrA1, "", _NrA2, 0, BackGround, Foreground, Selected) :-	 % 0 = rechts 2= alleen uitslagen
  NrA1 > 0,		% linker kolom
  aanwezigNaam(NaamA1, NrA1, NaamA1a),
  drawBaanSpot(Mode, Banentool, LboxWin, No1, Cat1, rct(L,T,R,B),rct(_,_,RI,_)),
  aanstrepen(Mode, Banentool, No1, Cat1, NrA1, BackGround, BackGround1,Foreground,ForeGround1, Selected),
  win_SetBackColor(LboxWin, BackGround1),
  win_SetForeColor(LboxWin, ForeGround1),
  win_GetTextExtent(LboxWin, NaamA1a, -1, WA1, _H1),
  L1 = R - WA1,
  grootste(L1, RI/*L*/, L1a),
  setHotSpelerSpot(Mode, Banentool, LboxWin, No1, Cat1, NrA1, rct(L1a, T, R, B)),
  draw_TextInRect(LboxWin,rct(L1a,T,R,B), NaamA1a, -1, [dtext_left]), !.
drawNamenInRect(Mode, Banentool, No1, Cat1, LboxWin, rct(L0,T,R,B), _Namen1, NaamA1, NrA1, NaamA2, NrA2, 0, BackGround, Foreground, Selected) :-	 % 0 = rechts 2= alleen uitslagen
  NrA1 > 0,		% linker kolom
  NrA2 > 0,
  aanwezigNaam(NaamA1, NrA1, NaamA1a),
  aanwezigNaam(NaamA2, NrA2, NaamA2a),
  win_GetTextExtent(LboxWin, NaamA2a, -1, WA2, _H2),
  win_GetTextExtent(LboxWin, NaamA1a, -1, WA1, _H1),
  win_GetTextExtent(LboxWin, "+", -1, WPlus, _),	% eigenlijk alleen om een beetje afstand te houden
  drawBaanSpot(Mode, Banentool, LboxWin, No1, Cat1, rct(L0,T,R,B), rct(_,_,L,_)), 	% het eventuele icoon
  L2 = R - WA2,
  aanstrepen(Mode, Banentool, No1, Cat1, NrA2, BackGround, BackGround2,Foreground,ForeGround2, Selected),
  R1 = L2 - WPlus,
  L1 = R1 - WA1,
  grootste(L, L1, LG),
  Delta = WA2 * (LG - L1) div (WA2 + WA1),	% proportioneel
  R1a = R1 + Delta,
  aanstrepen(Mode, Banentool, No1, Cat1, NrA1, BackGround, BackGround1,Foreground,ForeGround1, Selected),
  L2a1 = R1a + WPlus,
  A2Rect = rct(L2a1, T, R, B),
  setHotSpelerSpot(Mode, Banentool, LboxWin, No1, Cat1, NrA2, A2Rect),
  win_SetBackColor(LboxWin, BackGround2),
  win_SetForeColor(LboxWin, ForeGround2),
  draw_TextInRect(LboxWin,A2Rect, NaamA2a, -1, [dtext_left]),
  % ga A1 schrijven (meest linkse)
  A1Rect = rct(LG, T, R1a, B),
  setHotSpelerSpot(Mode, Banentool, LboxWin, No1, Cat1, NrA1, A1Rect),
  win_SetBackColor(LboxWin, BackGround1),
  win_SetForeColor(LboxWin, ForeGround1),
  draw_TextInRect(LboxWin,A1Rect, NaamA1a, -1, [dtext_left]),
  win_SetBackColor(LboxWin, BackGround),
  win_SetForeColor(LboxWin, ForeGround),
  draw_TextInrect(LboxWin,rct(R1a,T,L2a1,B), "+", -1, [dtext_center]), !.
drawNamenInRect(Mode, Banentool, No1, Cat1, LboxWin, rct(L,T,R,B), _Namen1, NaamA1, NrA1, "", _NrA2, 1, BackGround, Foreground, Selected) :-	 % 1 = links 2= uitslaginvoer
  NrA1 > 0,
  aanwezigNaam(NaamA1, NrA1, NaamA1a),
  aanstrepen(Mode, Banentool, No1, Cat1, NrA1, BackGround, BackGround1,Foreground,ForeGround1,Selected),
  win_SetBackColor(LboxWin, BackGround1),
  win_SetForeColor(LboxWin, ForeGround1),
  win_GetTextExtent(LboxWin, NaamA1a, -1, WA1, _H1),
  R1 = L + WA1,
  kleinste(R1, R, R1a),
  setHotSpelerSpot(Mode, Banentool, LboxWin, No1, Cat1, NrA1, rct(L,T,R1a,B)),
  draw_TextInRect(LboxWin,rct(L,T,R1a,B), NaamA1a, -1, [dtext_left]), !.
drawNamenInRect(Mode, Banentool, No1, Cat1, LboxWin, rct(L,T,R,B), _Namen1, NaamA1, NrA1, NaamA2, NrA2, 1, BackGround, Foreground, Selected) :-	 % 1 = links 2= alleen uitslaginvoer
  NrA1 > 0,	% rechter kolom!
  NrA2 > 0,
  aanwezigNaam(NaamA1, NrA1, NaamA1a),
  aanwezigNaam(NaamA2, NrA2, NaamA2a),
  aanstrepen(Mode, Banentool, No1, Cat1, NrA1, BackGround, BackGround1,Foreground,ForeGround1,Selected),
  win_GetTextExtent(LboxWin, NaamA2a, -1, WA2, _H2),
  win_GetTextExtent(LboxWin, NaamA1a, -1, WA1, _H1),
  win_GetTextExtent(LboxWin, "+", -1, WPlus, _),
  R1 = L + WA1,
  kleinste(R1, R, R1a),
  L2 = R1a + WPlus,
  R2 = L2 + WA2,
  kleinste(R, R2, R2a),
  aanstrepen(Mode, Banentool, No1, Cat1, NrA2, BackGround, BackGround2,Foreground,ForeGround2,Selected),
  Delta = WA2 * (R2 - R2a) div (WA2 + WA1),
  R1a1 = R1a - Delta,
  L2a1 = L2 - Delta,
  % A1 schrijven
  A1Rect = rct(L, T, R1a1, B),
  setHotSpelerSpot(Mode, Banentool, LboxWin, No1, Cat1, NrA1, A1Rect),
  win_SetBackColor(LboxWin, BackGround1),
  win_SetForeColor(LboxWin, ForeGround1),
  draw_TextInRect(LboxWin,A1Rect, NaamA1a, -1, [dtext_left]),
  % A2 schrijven
  A2Rect = rct(L2a1, T, R2a, B),
  setHotSpelerSpot(Mode, Banentool, LboxWin, No1, Cat1, NrA2, A2Rect),
  win_SetBackColor(LboxWin, BackGround2),
  win_SetForeColor(LboxWin, ForeGround2),
  draw_TextInRect(LboxWin,A2Rect, NaamA2a, -1, [dtext_left]),
  win_SetBackColor(LboxWin, BackGround),
  win_SetForeColor(LboxWin, ForeGround),
  draw_TextInrect(LboxWin,rct(R1a1,T,L2a1,B), "+", -1, [dtext_center]), !.
drawNamenInRect(_, _, _, _, LboxWin, RectNaam1, Namen1, _NaamA1, _NrA1, _NaamA2, _NrA2, 0, _, _, _) :-	 % 0 = rechts <> 2 = geen uitslagen
  draw_TextInRect(LboxWin,RectNaam1, Namen1, -1, [dtext_right]), !.
drawNamenInRect(_, _, _, _, LboxWin, RectNaam2, Namen2, _NaamB1, _NrB1, _NaamB2, _NrB2, _, _, _, _) :-	 % 1 = links  <> 2 = geen uitslagen
  draw_TextInRect(LboxWin,RectNaam2, Namen2, -1, [dtext_left]). 

setHotSpelerSpot(2, ja, LboxWin, No1, Cat1, NrA2, A2Rect) :-
  retractall(hotSpelerSpot(LboxWin, No1, Cat1, NrA2, _)),
  asserta(hotSpelerSpot(LboxWin, No1, Cat1, NrA2, A2Rect)), !.
setHotSpelerSpot(_, _, _LboxWin, _No1, _Cat1, _NrA2, _A2Rect).

drawMyIcon(LboxWin, Text, L, T, OpKleur, LetterKleur, RandKleur, RectUit) :-
  maxbaanbreedte(LboxWin, Width, H),
  R = L + Width,
  B = T + H - 1,
  Tx = T - 1,
  RectUit = rct(L, Tx, R, B), 
  win_SetPen(LboxWin,pen(1, ps_solid, RandKleur)),
  win_SetBrush(LboxWin,brush(pat_solid,OpKleur)),
  draw_RoundRect(LboxWin,RectUit, 2, 2),
  win_SetBackMode(LboxWin,bk_Transparent),
  win_SetForeColor(LboxWin, LetterKleur),
  draw_TextInRect(LboxWin,RectUit, Text, -1,[dtext_Center, dtext_Vcenter,dtext_Noprefix]), !,
  win_SetBackMode(LboxWin,bk_Opaque).
drawMyIcon(_, _, L, T, _, _, _, rct(L,T,L,T)).

drawBaanSpot(2, ja, LboxWin, No, Cat, rct(L,T,__,_),RectUit1) :-
  baantoew(No, Cat, aanwezig(_X)),
  T1 = T, % + 1,
  L1 = L + 1,
  drawMyIcon(LboxWin, "?", L1, T1, color_yellow, color_black, color_black, RectUit1),
  retractall(hotBaanSpot(LboxWin, No, Cat, _)), !,
  asserta(hotBaanSpot(LboxWin, No, Cat, RectUit1)).
drawBaanSpot(2, ja, LboxWin, No, Cat, rct(L,T,_,_),RectUit1) :-
  baantoew(No, Cat, reserveren(Baan, _Tijd)),
  T1 = T, % + 1,
  L1 = L + 1,
  baanNaamP(Baan, _, _, BaanNaam),		% baan in bedrijf
  drawMyIcon(LboxWin, BaanNaam, L1, T1, color_yellow, color_black, color_black, RectUit1), !,
  retractall(hotBaanSpot(LboxWin, No, Cat, _)),
  asserta(hotBaanSpot(LboxWin, No, Cat, RectUit1)).
drawBaanSpot(2, ja, LboxWin, No, Cat, rct(L,T,_,_), RectUit2) :-
  baantoew(No, Cat, baan(BaanNr, _)),
  T1 = T, % + 1,
  L1 = L + 1,
  baanNaamP(BaanNr, _, _, BaanNaam),		% baan in bedrijf
  drawMyIcon(LboxWin, BaanNaam, L1, T1, color_magenta, color_black, color_black, RectUit2), !,
  retractall(hotBaanSpot(LboxWin, No, Cat, _)),
  asserta(hotBaanSpot(LboxWin, No, Cat, RectUit2)).
drawBaanSpot(2, ja, LboxWin, No, Cat, rct(L,T,__,_), RectUit2) :-
  baantoew(No, Cat, gespeeldop(BaanNr, _Tijd)),
  T1 = T, % + 1,
  L1 = L + 1,
  baanNaamP(BaanNr, _, _, BaanNaam),		% baan in bedrijf
  drawMyIcon(LboxWin, BaanNaam, L1, T1, color_white, color_black, color_black, RectUit2), !,
  retractall(hotBaanSpot(LboxWin, No, Cat, _)),
  asserta(hotBaanSpot(LboxWin, No, Cat, RectUit2)).
drawBaanSpot(_, _, _LboxWin, _No, _Cat, rct(L,T,_,_), rct(L,T,L,T)).
  
aanwezig(0) :- !.
aanwezig(Nr) :-
  spelerAanwezig(Nr, Tijd, _, _),
  tijd_kwart(D1, _, _, Tijd),
  huidig(Dag, _),
  Dag = D1, !.

spelersAanwezig(p(Nr1, Nr2), p(Nr3, Nr4)) :-
  aanwezig(Nr1),
  aanwezig(Nr2),
  aanwezig(Nr3),
  aanwezig(Nr4), !.
spelersAanwezig(pw(Nr1), p(Nr3, Nr4)) :-
  aanwezig(Nr1),
  aanwezig(Nr3),
  aanwezig(Nr4), !.
spelersAanwezig(p(Nr1, Nr2), pw(Nr3)) :-
  aanwezig(Nr1),
  aanwezig(Nr2),
  aanwezig(Nr3), !.
spelersAanwezig(pw(Nr1), pw(Nr4)) :-
  aanwezig(Nr1),
  aanwezig(Nr4), !.
spelersAanwezig(s(Nr1), s(Nr3)) :-
  aanwezig(Nr1),
  aanwezig(Nr3), !.

upd_baantoew(baantoew(No, Cat, X)) :-
  baantoew(No,Cat,X), !.
upd_baantoew(baantoew(No, Cat, X)) :-
  logf('A',baantoew(No, Cat, X),nee), !.
upd_baantoew(_).

baanKandidaat(No, Cat) :-
  w2(No,Cat,Tijd,_,_,_,_,_),
  tijd_kwart(Dag, _, _, Tijd),
  huidig(Dag1, _),
  Dag = Dag1,
  wd(No,Cat,A,B,_,_,_),
  spelersAanwezig(A, B),
  not(baantoew(No, Cat, _)),
  time(H,Min,Sec,_),
  helestr_int(0, HS, H),
  helestr_int(0, MS, Min),
  helestr_int(0, SS, Sec),
  format(Str, "%s%s%s", HS, MS, SS),
  w2(No,Cat,Tijd,_,_,_,_,_),
  upd_baantoew(baantoew(No, Cat, aanwezig(Str))),
  update_uitslagen("baankand"), !.
baanKandidaat(No, Cat) :-
  wd(No,Cat,A,B,_,_,_),
  not(spelersAanwezig(A, B)),
  baantoew(No, Cat, aanwezig(I)), !,
  logf('R', baantoew(No, Cat, aanwezig(I)), nee),
  update_uitslagen("baankand").
baanKandidaat(_No, _Cat).

ownerdraw1(Win, LboxWin, RectItem, ItemId, _Color, Selected) :-
  Item = lbox_GetItem(LboxWin, ItemId),	% get the string
  planSelect(Win, Mode, _),
  wedRepr(Mode,Item, Namen1, Namen2, No1, Cat1, DStr, TStr, BaanPark, Tijd, NaamA1, NrA1, NaamA2, NrA2, NaamB1, NrB1, NaamB2, NrB2),
  getGradatie1(Win, Tijd),	% bezwaren met kleurtjes
  getverschuiving(Win, Mode, No1, Cat1, Tijd),
  wc(Cat1, _, _, _, _, CatNam, _, _, _, _, _, _, _, _, _),
  kleuren(Win, Selected, No1, Cat1, Tijd, PenKleur, ForeKleur, BackKleur1, BackKleur2, BrushKleur,ParkKl),
  win_SetPen(LboxWin,pen(1,ps_Solid,PenKleur)),
  win_SetForeColor(LboxWin, ForeKleur),
  RectItem = rct(_L, T, R, B),
  rectAfm(Win, T, B, R, RectDag, RectTijd, RectPark, RectCat, TUrg, _TGradatie, RectNaam1, RectNaam2, BaanParkL),
  checkBaanParkL(Win, LboxWin, BaanPark, BaanParkL), 			% maak baanpark groter na bijv. refresh
  win_SetBrush(LboxWin,brush(pat_Solid, BrushKleur)),
  draw_Rect(LboxWin,RectItem),
  win_SetBrush(LboxWin,brush(pat_Solid, BackKleur1)),
  draw_Rect(LboxWin,RectCat),
  win_SetBackColor(LboxWin, BackKleur1),
  draw_TextInRect(LboxWin,RectCat, CatNam, -1, [dtext_left]),
  win_SetBackColor(LboxWin, BackKleur2),
  draw_TextInRect(LboxWin,RectDag, DStr, -1, [dtext_right]),
  teveelWedstrijden(LboxWin, Tijd, BackKleur2, BackColorB), 		% signaleer teveel wedstrijden
  win_SetBackColor(LboxWin,BackColorB),
  draw_TextInRect(LboxWin,RectTijd, TStr, -1, [dtext_right]),
  win_SetBrush(LboxWin,brush(pat_Hollow, BrushKleur)),
  draw_Rect(LboxWin,RectItem),
  win_SetBackColor(LboxWin, BackKleur2),
  win_SetForeColor(LboxWin, ForeKleur),					% zet weer terug
  banentool(BanenTool, _),
  verwijderFantoomSpots(LboxWin, Mode, BanenTool, T, B),
  drawNamenInRect(Mode, BanenTool, No1, Cat1, LboxWin, RectNaam1, Namen1, NaamA1, NrA1, NaamA2, NrA2, 0, BackKleur2,ForeKleur,Selected), % 0 = rechts
  win_SetBackColor(LboxWin, BackKleur2),
  win_SetForeColor(LboxWin, ForeKleur),
  drawNamenInRect(Mode, BanenTool, No1, Cat1, LboxWin, RectNaam2, Namen2, NaamB1, NrB1, NaamB2, NrB2, 1, BackKleur2,ForeKleur,Selected), % 1 = links
  win_SetBackColor(LboxWin, BackKleur2),
  win_SetForeColor(LboxWin, ParkKl),
  draw_TextInRect(LboxWin,RectPark, BaanPark, -1, [dtext_left]),
  Font = font_Create(ff_Helvetica, [fs_bold], 8),
  win_SetFont(LboxWin, Font),
  win_GetTextExtent(LboxWin, "urg", -1, Rechts, _Height),	% geef gefixeerd weer
  Achtergrond1 = color_White,
  get_urg(Win, Mode, LboxWin, T, B, TUrg, No1, Cat1, Rechts,Achtergrond1),
  streepjedag(LboxWin, ItemId, Tijd, RectDag, RectTijd),
  !.
ownerdraw1(Win, LboxWin, RectItem, ItemId, _Color, Selected) :-	% als Cat niet bestaat
  Item = lbox_GetItem(LboxWin, ItemId),	% get the string
  planSelect(Win, Mode, _),
  wedRepr(Mode,Item, Namen1, _Namen2, No1, Cat1, DStr, TStr, _BaanPark, Tijd, NaamA1, NrA1, NaamA2, NrA2, _NaamB1, _NrB1, _NaamB2, _NrB2),
  getGradatie1(Win, Tijd),	% bezwaren met kleurtjes
  kleuren(Win, Selected, No1, Cat1, Tijd, PenKleur, ForeKleur, BackKleur1, BackKleur2, BrushKleur,_ParkKl),
  win_SetPen(LboxWin,pen(1,ps_Solid,PenKleur)),
  win_SetForeColor(LboxWin, ForeKleur),
  win_SetBackColor(LboxWin, BackKleur2),
  win_SetBrush(LboxWin,brush(pat_Solid, BrushKleur)),
  draw_Rect(LboxWin,RectItem),
  RectItem = rct(_L, T, R, B),
  rectAfm(Win, T, B, R, RectDag, RectTijd, _RectPark, RectCat,_TUrg, _TGradatie, _RectNaam1, RectNaam2, _),
  RectNaam = rect_Union(RectCat, RectNaam2),
  draw_TextInRect(LboxWin,RectDag, DStr, -1, [dtext_right]),
  teveelWedstrijden(LboxWin, Tijd, BackKleur2, _), 
  draw_TextInRect(LboxWin,RectTijd, TStr, -1, [dtext_right]),
  win_SetForeColor(LboxWin, ForeKleur),
  banenTool(BanenTool, _),
  verwijderFantoomSpots(LboxWin, Mode, BanenTool, T, B),
  win_SetBackColor(LboxWin, BackKleur1),
  drawNamenInRect(Mode, BanenTool, No1, Cat1,LboxWin, RectNaam, Namen1, NaamA1, NrA1, NaamA2, NrA2, 1, BackKleur2,ForeKleur,Selected), % 1 = links
  win_SetForeColor(LboxWin, ForeKleur),
  Font = font_Create(ff_Helvetica, [fs_bold], 8),
  win_SetFont(LboxWin, Font),
  streepjedag(LboxWin, ItemId, Tijd, RectDag, RectTijd),
  !.
ownerdraw1(_Win, _LboxWin, _RectItem, _ItemId, _Color, _Selected) :-
  beep() , !.

dag_tijd(Dag,Tijd) :-
  bn(Tijd,_),
  bitright(Tijd,7,D),
  D = Dag.

getGradatie1(Win,  _Tijd) :-
  planSelectNot4(Win, _, _), !. 
getGradatie1(_Win, _Tijd) :-
  boventekens(0, 0), !.		% geen bezwarenkolom
getGradatie1(_Win, TijdQ) :-
  boventekens(No, Cat),
  not(bezwColor(TijdQ, _)),
  bitright(TijdQ, 7, Dag),				% met een hele dag tegelijk
  findall(T1,dag_tijd(Dag,T1),Tijdstippen),
  sorttijd_c(Tijdstippen),
  check_forceer_c(0, Tijdstippen, No, Cat, _Outstring, Result, _, Tijd),
  assert(bezwColor(Tijd, Result)),
  fail.  
getGradatie1(_Win, _Tijd).

checkBaanParkL(Win, LboxWin, BaanPark, BaanParkL) :-
  baan_adm(ja),
  win_GetTextExtent(LboxWin, BaanPark, -1, Width, _Height),
  Width > BaanParkL,
  bepaalKolommen(Win),		% opnieuw
  win_Invalidate(Win), !.
checkBaanParkL(_Win, _LboxWin, _BaanPark, _TabBaanPark).

%BEGIN_DLG Vervolgpartij
/**************************************************************************
	Creation and event handling for dialog: Vervolgpartij
**************************************************************************/

constants

%BEGIN Vervolgpartij, CreateParms, 09:06:56-2.7.2012, Code automatically updated!
  dlg_vervolgpartij_ResID = idd_vervolgpartij
  dlg_vervolgpartij_DlgType = wd_Modal
  dlg_vervolgpartij_Help = idh_oproepenafzeggen
%END Vervolgpartij, CreateParms

database - uitsl
  uitslWedsId(WINDOW, integer, wedscat, string Titel, janee TempGew, integer LR)
  single gelijkgebleven(integer,boolean Slave )
  single lijnen(integer, integer, integer, integer)

predicates

  dlg_vervolgpartij_eh : EHANDLER
  dlg_vervolgpartij_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_vervolgpartij_update(DIALOG_VAL_LIST)
  oproepenAdm1(janee, janee, WINDOW, janee Gewaarschuwd, integer LR, janee TempGew)
  oproepenAdm2(janee, janee, WINDOW, janee Gewaarschuwd, integer LR, janee TempGew)
  gedrukt(janee,dialog_control_id,dialog_control_id,dialog_control_id)
  partijBekend(tgnst, janee)
  lrTgnst(WINDOW, integer)
  procedure handleWas(janee, janee, tgnst, integer, wedscat) - (i,i,i,i,i)
  %procedure upd_w2(dbasedom) - (i)

clauses
gelijkgebleven(idc_cancel,b_False).
lijnen(0,0,0,0).

oproepenAdm1(ja, ja, Win, _Gew, 1, TempGew) :-	% oproepen, bekend, de ene partij
  gedrukt(TempGew, idcv_ja1, idcv_nee1, Kontrol1),
  dialog_SetRadiobutton(Win,idcv_ja1,Kontrol1),!.
oproepenAdm1(ja, ja, Win, Gew, _, _) :-
  gedrukt(Gew, idcv_ja1, idcv_nee1, Kontrol1),
  dialog_SetRadiobutton(Win,idcv_ja1,Kontrol1),!.
oproepenAdm1(_, _, Win, Gew, _, _) :- !,
  gedrukt(Gew, idcv_ja1, idcv_nee1, Kontrol1),
  dialog_SetRadiobutton(Win,idcv_ja1,Kontrol1),
  dialog_SetState(Win, [show(idcv_ja1,b_False),show(idcv_nee1,b_False),
  		show(idcv_gegevens1, b_False), enable(idcv_gegevens1,b_False),
  		show(idcv_gewwarschuwd1,b_False)]).

oproepenAdm2(ja, ja, Win, _Gew, 2, TempGew) :-
  gedrukt(TempGew, idcv_ja2, idcv_nee2, Kontrol2),
  dialog_SetRadiobutton(Win,idcv_ja2,Kontrol2), !.
oproepenAdm2(ja, ja, Win, Gew, _, _) :-	
  gedrukt(Gew, idcv_ja2, idcv_nee2, Kontrol2),
  dialog_SetRadiobutton(Win,idcv_ja2,Kontrol2), !.
oproepenAdm2(_, _, Win, Gew, _, _) :- !,
  gedrukt(Gew, idcv_ja2, idcv_nee2, Kontrol2),
  dialog_SetRadiobutton(Win,idcv_ja2,Kontrol2), 
  dialog_SetState(Win, [show(idcv_ja2,b_False),show(idcv_nee2,b_False),
  		show(idcv_gegevens2, b_False), enable(idcv_gegevens2,b_False),
  		show(idcv_gewaarschuwd2,b_False)]). 

lrTgnst(Win, 1) :- 
  dialog_SetState(Win, [enable(idcv_ja2,b_False),enable(idcv_nee2,b_False),
  			enable(idcv_gewaarschuwd2,b_False),
  			enable(idctv_speler2,b_False),enable(idcv_sp2tel,b_False),
  			enable(idcv_gegevens2,b_False),show(idcv_gegevens2,b_False)]), !. 
lrTgnst(Win, 2) :- 
  dialog_SetState(Win, [enable(idcv_ja1,b_False),enable(idcv_nee1,b_False),
  			enable(idcv_gewwarschuwd1,b_False), 
  			enable(idctv_speler1,b_False),enable(idctv_sp1tel,b_False),
  			enable(idcv_gegevens1,b_False),show(idcv_gegevens1,b_False)]), !.
lrTgnst(_,_).

partijBekend(p(_,_),ja) :- !.
partijBekend(s(_),ja)   :- !.
partijBekend(pw(_),ja)   :- !.
partijBekend(_, nee).

gedrukt(ja, X, _, X) :- !.
gedrukt(nee,_, X, X).

handleWas(_,  ja, T, Num, Cat) :-	% 2.8.1999
  ws(Num, Cat, T, Tijd),
  logf('R', ws(Num, Cat, T, Tijd),ja),
  fail. 
handleWas(_, _, _T, _Num, _Cat).


  dlg_vervolgpartij_Create(nee, _, _, _, _, _, _, idc_cancel,_):- !.
  dlg_vervolgpartij_Create(ja, _Parent, Num, Cat, _, _, _, idc_cancel,_):-
	not(w2(Num,Cat,_,_,_,_,_,_)),
	dlg_note("Vervolgpartij","nog niet gepland"), !.		% zou niet moeten voorkomen
  dlg_vervolgpartij_Create(ja, Parent, Num, Cat, Titel,TempGew,LR, Ret,Slave):-
	NulWin = cast(WINDOW, 0),
	assert(uitslWedsId(NulWin, Num, Cat, Titel, TempGew,LR)),
	assert(gelijkgebleven(idc_cancel,Slave)),
	RB2 = idcv_ja2,
	RB  = idcv_ja1,
	locList(IDCV_LOCATIE_ITEMLIST),
	IDCV_LOCATIE_DEFAULT = "",
%MARK Vervolgpartij, new variables

	dialog_CreateModal(Parent,dlg_vervolgpartij_ResID,"",
  		[
%BEGIN Vervolgpartij, ControlList, 09:06:56-2.7.2012, Code automatically updated!
		df(idcv_locatie,listedit(IDCV_LOCATIE_ITEMLIST,IDCV_LOCATIE_DEFAULT),nopr),
		df(RB2,radiobuttongroup([idcv_ja2,idcv_nee2]),nopr),
		df(RB,radiobuttongroup([idcv_ja1,idcv_nee1]),nopr)
%END Vervolgpartij, ControlList
		],
		dlg_vervolgpartij_eh,0,VALLIST,ANSWER),
	dlg_vervolgpartij_handle_answer(ANSWER,VALLIST),
	gelijkgebleven(Ret,_).

  dlg_vervolgpartij_handle_answer(idc_ok,VALLIST):-!,
	dlg_vervolgpartij_update(VALLIST).
  dlg_vervolgpartij_handle_answer(idc_cancel,_):-!.  % Handle Esc and Cancel here
  dlg_vervolgpartij_handle_answer(_,_):-
	errorexit().

  dlg_vervolgpartij_update(_VALLIST):-
%BEGIN Vervolgpartij, Update controls, 09:06:56-2.7.2012, Code automatically updated!
	_IDCV_LOCATIE_DEFAULT = dialog_VLGetListEdit(idcv_locatie,_VALLIST),
	_RB2 = dialog_VLGetRadiobutton(idcv_ja2,_VALLIST),
	_RB = dialog_VLGetRadiobutton(idcv_ja1,_VALLIST),
%END Vervolgpartij, Update controls
	true.

%MARK Vervolgpartij, new events

%BEGIN Vervolgpartij, idcv_gegevens2 _CtlInfo
  dlg_vervolgpartij_eh(_Win,e_Control(idcv_gegevens2,_CtrlType,_CtrlWin,_CtlInfo),0):-
	uitslWedsId(_Win, Num, Cat,_Titel,_TempGew,_LR),
	wd(Num,Cat,_A, Tgnst,_,_,_),
	dlg_plangegevens_Create(mijnoproepen, _Win, "gegevens2", Tgnst, Num, Cat, Result, b_True),
	Result = 1,
	RJa = win_GetCtlHandle(_Win, idcv_ja2),
	RNee = win_GetCtlHandle(_Win, idcv_nee2),
	win_CheckRadioButton(RJa, [RJa, RNee]),
	gelijkgebleven(_,Slave),
	assert(gelijkgebleven(idc_ok,Slave)), !.
	%win_Destroy(_Win).
%END Vervolgpartij, idcv_gegevens2 _CtlInfo

%BEGIN Vervolgpartij, idcv_gegevens1 _CtlInfo
  dlg_vervolgpartij_eh(_Win,e_Control(idcv_gegevens1,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	uitslWedsId(_Win, Num, Cat,_Titel,_TempGew,_LR),
	wd(Num,Cat,Tgnst,_B,_,_,_),
	dlg_plangegevens_Create(mijnoproepen, _Win, "gegevens1", Tgnst, Num, Cat, Result, b_True),
	Result = 1,
	RJa = win_GetCtlHandle(_Win, idcv_ja1),
	RNee = win_GetCtlHandle(_Win, idcv_nee1),
	win_CheckRadioButton(RJa, [RJa, RNee]),
	gelijkgebleven(_,Slave),
	assert(gelijkgebleven(idc_ok,Slave)),
	!.
%END Vervolgpartij, idcv_gegevens1 _CtlInfo

%BEGIN Vervolgpartij, idc_help _CtlInfo
  dlg_vervolgpartij_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Oproepenafzeggen"),
	!.
%END Vervolgpartij, idc_help _CtlInfo

%BEGIN Vervolgpartij, e_Update
  dlg_vervolgpartij_eh(_Win,e_Update(_UpdateRct),0):-!,
	CtrlWin = win_GetCtlHandle(_Win, idctv_onderdeel),
	uitslWedsId(_Win, Num, Cat,_Titel,_TempGew,_LR),
	wc(Cat, _, _, CNaam, _, _, _, _, _, _, _, _, _, _, _),
	win_SetText(CtrlWin, CNaam),
	bitright(Num, 1, Num1),
	bitand(Num, 1, OE),
	oproepen(Oproep),
	pos(Cat, Top, _, _ ),  		% 
	dim(Top, TopDim),
	dim(Num1,NoDim),                  	% bepaal de ronde
	repc(nee,' ',nee,Oproep,nee,OE,TopDim,NoDim,Num1,Cat,onbekend,alleplanning, Plan,Was,_TijdLog2,_,_,_,_),
	OpWin  = win_GetCtlHandle(_Win, idctv_op),
	format(PlanPlus, "%s %s\n%s", Plan, Was),
	win_SetText(OpWin, PlanPlus),
	lijnen(L1, L2, L3, R),
	draw_Line(_Win,pnt(0,L1),pnt(R, L1)),
	draw_Line(_Win,pnt(0,L2),pnt(R, L2)),
	draw_Line(_Win,pnt(0,L3),pnt(R, L3)),
	!.
%END Vervolgpartij, e_Update

%BEGIN Vervolgpartij, idc_ok _CtlInfo
%BEGIN Vervolgpartij, idc_ok _CtlInfo
  dlg_vervolgpartij_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	baan_adm(ja),
	BC = win_GetCtlHandle(_Win, idcv_locatie),
	Bn = win_GetText(BC),
	trim_c(Bn),
	Bn = "",
	Answer = dlg_MessageBox( "Locatie", "Locatie invullen?", mesbox_iconquestion, mesbox_buttonsyesnocancel, mesbox_defaultsecond, mesbox_suspendapplication ),
 	Answer <> 2, !.
  dlg_vervolgpartij_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	uitslWedsId(_Win, Num, Cat,_Titel,_TempGew,_LR),
	Kontrol1 = win_GetCtlHandle(_Win,idcv_ja1),
	Checked1 = win_IsChecked(Kontrol1),
	janee2bool(Gew1, Checked1),
	Kontrol2 = win_GetCtlHandle(_Win,idcv_ja2),
	Checked2 = win_IsChecked(Kontrol2),
	janee2bool(Gew2, Checked2),
	loglock(mdi, "dlg_vervolgpartij"),
	w2(Num,Cat,Tijd1,G1,G2,Fix,Tijd2,_), 
	BC = win_GetCtlHandle(_Win, idcv_locatie),
	Bn = win_GetText(BC),
	gelijkgebleven(_,Slave),
	assert(gelijkgebleven(idc_ok,Slave)),
	wd(Num,Cat,T1,T2,_,_,_),
	handleWas(G1, Gew1, T1, Num, Cat),
	handleWas(G2, Gew2, T2, Num, Cat),
        logf('Z',w2(Num,Cat,Tijd1,Gew1,Gew2,Fix,Tijd2,Bn), nee),
	%upd_w2(w2(Num,Cat,Tijd1,Gew1,Gew2,Fix,Tijd2,Bn)),
	update_waarsch(),
	update_situatie(),
	update_uitslagen("vervolgp"),
	win_Destroy(_Win), !.
  dlg_vervolgpartij_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):- !,
	logclose(),
	fail.
%END Vervolgpartij, idc_ok _CtlInfo

%BEGIN Vervolgpartij, idc_cancel _CtlInfo
  dlg_vervolgpartij_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	win_Destroy(_Win),
	!.
%END Vervolgpartij, idc_cancel _CtlInfo

%BEGIN Vervolgpartij, e_Destroy
  dlg_vervolgpartij_eh(_Win,e_Destroy,0):-!,
	retractall(uitslWedsId(_Win, _, _, _, _, _)),
	!.
%END Vervolgpartij, e_Destroy

%BEGIN Vervolgpartij, e_Create
  dlg_vervolgpartij_eh(_Win,e_Create(_CreationData),0):-!,
	NulWin = cast(WINDOW, 0),
	retract(uitslWedsId(NulWin, Num, Cat, Titel,TempGew, LR)),
	assert(uitslWedsId(_Win, Num, Cat,Titel,TempGew,LR)),
	LijnWin = win_GetCtlHandle(_Win, idcv_lijn),
	Lrect = win_GetOuterRect(LijnWin),
	Lrect = rct(_, _, _, LB),
	Rect = win_GetClientRect(_Win),
	Rect = rct(_, _, R, _),
	RectOO = win_GetOuterRect(_Win),
	WsFlags = win_GetState(_Win),
	RectO = rect_GetClient(WsFlags, b_False, RectOO),
	RectO = rct(LO, _, _, _),
	ParentWin = win_GetParent(_Win),
	RectP = win_GetOuterRect(ParentWin),
	RectP = rct(LP, _, _, _),
	Dh = LP - LO + 40,
	RectONew =rect_Offset(RectO, Dh, 0),
	win_Move(_Win,RectONew),
	win_Destroy(LijnWin),
	%draw_Line(_Win,pnt(0,LB),pnt(R, LB)),
	TegenWin = win_GetCtlHandle(_Win, idcv_grouptegen),
	Trect = win_GetOuterRect(TegenWin),
	Trect = rct(_, TT, _, TB),
	TMiddle = (TT + TB) div 2,
	win_Destroy(TegenWin),
	OWin = win_GetCtlHandle(_Win, idcv_groupbox),
	Orect = win_GetOuterRect(OWin),
	Orect = rct(_, OT, _, OB),
	OMiddle = (OT + OB) div 2,
	win_Destroy(OWin),
	assert(lijnen(LB, TMiddle, OMiddle, R)),
	%draw_Line(_Win,pnt(0,TMiddle),pnt(R, TMiddle)),
	win_SetText(_Win, Titel),
	wd(Num,Cat,A,B,_,_,_),
	pos(Cat, Top, _, _ ),  		% 
	dim(Top, TopDim),
	dim(Num,NoDim),                  	% bepaal de ronde
	repc(nee,'}',nee,nee,nee,0,TopDim,NoDim,Num,Cat,A,alleplanning, Naam1,_Score,_TijdLog,_,_,_,_),
	repc(nee,'}',nee,nee,nee,1,TopDim,NoDim,Num,Cat,B,alleplanning, Naam2,_Score1,_TijdLog1,_,_,_,_),
	CBWin1 = win_GetCtlHandle(_Win, idctv_speler1),
	win_SetText(CBWin1, Naam1),
	CBWin2 = win_GetCtlHandle(_Win, idctv_speler2),
	win_SetText(CBWin2, Naam2),
	spTelPlus(A, Tel1, _),
	spTelPlus(B, Tel2, _),
	Tel1Win = win_GetCtlHandle(_Win, idctv_sp1tel),
	Tel2Win = win_GetCtlHandle(_Win, idcv_sp2tel),
	win_SetText(Tel1Win, Tel1),
	win_SetText(Tel2Win, Tel2),
	w2(Num,Cat,_Tijd,GewA,GewB,_Fix,_Spoor, BNo),
	oproepen(Oproepen),
	partijBekend(A, BekendA),			% is de persoon bekend?
	oproepenAdm1(Oproepen, BekendA, _Win, GewA, LR, TempGew), % set gew. button
	partijBekend(B, BekendB),
	oproepenAdm2(Oproepen, BekendB, _Win, GewB, LR, TempGew),
	lrTgnst(_Win, LR),
	baan_adm(BA),
	LocWin = win_GetCtlHandle(_Win, idcv_locatie),
	win_SetText(LocWin, BNo),
	janee2bool(BA, BAbool),
	dialog_SetState(_Win, [enable(idctv_plaats, BAbool), enable(idcv_locatie, BAbool)]),
	!.
%END Vervolgpartij, e_Create

  dlg_vervolgpartij_eh(_,_,_):-!,fail.

%END_DLG Vervolgpartij


%BEGIN_WIN Waarschuwinvoer
/**************************************************************************
	Creation and event handling for window: Waarschuwinvoer
**************************************************************************/

constants
%BEGIN Waarschuwinvoer, CreateParms, 23:21:05-1.7.2012, Code automatically updated!
  win_waarschuwinvoer_WinType = w_TopLevel
  win_waarschuwinvoer_Flags = [wsf_SizeBorder,wsf_TitleBar,wsf_Close,wsf_ClipSiblings]
  win_waarschuwinvoer_RCT = rct(100,80,563,581)
  win_waarschuwinvoer_Menu = no_menu
  win_waarschuwinvoer_Title = "Waarschuwinvoer"
  win_waarschuwinvoer_Help = idh_inhoud
%END Waarschuwinvoer, CreateParms


domains
  wedspot = wedspot(integer, wedscat, rct)
  mailspot = mailspot(integer SpNo, rct)
  spelerspot = spelerspot(integer SpNo, rct)

database - waarschuwen
  determ soort(WINDOW, ilist)
  single algesynchroniseerd(janee)
  itemAfmW(WINDOW, integer, integer, integer, integer, integer, integer, integer, integer)
  currentTeken(WINDOW, char, integer)
  single currentTPos(integer)
  hotWaarschuwspot(window LboxWin, integer ItemId, rct InfoSpot, wedspot WS, mailspot M1, mailspot M2, spelerspot S1, spelerspot S2) 

predicates

  win_waarschuwinvoer_eh : EHANDLER
  subclassw_eh : EHANDLER
  nondeterm oproepInVolgorde(wedscat, string)
  nondeterm oproepAfzeg(wedscat, tijd, string) - (i,i,o)
  nondeterm oproepAfzeg1(wedscat, tijd, string) - (i,i,o)
  nondeterm nietGew(janee, janee, integer, oprafztype)
  lr1(tgnst, tgnst, tgnst)
  nondeterm lr2(tgnst,tgnst,tgnst,integer, janee, janee, janee)
  titel(oprafztype, string, string Short, integer Venstertype, string Popuptitel)
  wrepr(string, string CDT, integer, wedscat, tijd, oprafztype, tgnst)
  draw_Item(WINDOW, integer, RCT, COLOR)
  soortAfhandeling(WINDOW, integer Num, wedscat Cat, string Naam, tgnst, oprafztype, integer Answer) - (i,i,i,i,i,i,o)
  trepr : zoekFunc
  procedure erepr(tgnst, integer Bitmap, integer Offset, integer Bitmap, integer Offset, integer, wedscat, integer Sp1, integer Sp2)
  procedure completion(string)
  nondeterm complItem(wedscat, oprafztype)
  procedure infostatus(window, rct, string)
  procedure draw_picture(window LboxWin, integer ItemId, pnt, integer Bitmap, integer OffsetB, integer SpNo, rct)
  procedure draw_pictures(window LboxWin, integer ItemId, pnt, tgnst, integer, wedscat, integer Sp1, rct, integer Sp2, rct)
  procedure verstuurd(tgnst T, integer SpNo, integer No, wedscat Cat, integer Bitmap) 
  procedure synchroniseer(window)
  procedure tgnst_namen(window LboxWin, tgnst, integer SpNo1, string Naam1, integer Len1, integer SpNo2, string Naam2, integer Len2) 
  procedure selectW(integer Index)

clauses

currentTPos(0).
algesynchroniseerd(nee).

titel(mijnoproepen, "Oproepen/afzeggen (F6):", "", oproepvenster, "&Informeren") :- !.
titel(mijnafzeggen, "Afzeggen", "afzeggen", oproepvenster, "&Informeren") :- !.
titel(mijnverzet, "Verzet", "verzet", oproepvenster, "&Informeren") :- !.
titel(mijncorrectie, "Oproep ongedaan maken","OK", oproepcorvenster, "&Niet geïnformeerd").

lr1(T, T, _) :- !.
lr1(T, _, T).

lr2(A, A, _, 0, Gew, _, Gew).
lr2(B, _, B, 1, _, Gew, Gew).

nietGew(nee, _, 0, mijnoproepen).
nietGew(_, nee, 1, mijnoproepen).
nietGew(ja, _, 0, mijncorrectie).
nietGew(_, ja, 1, mijncorrectie).

oproepafzeg(Cat, _Tijd, _Str) :-
  ws(Num,Cat,T,_Tijd1),		% er was eens een planning
  w2(Num,Cat,_Tijd2,G1,G2,_,_,_),% en een nieuwe
  wd(Num,Cat,A,B,_,_,_),		
  lr2(T1,A,B,_, G1, G2, ja),	% 30.6.2007 maar daar zijn ze al voor gewaarschuwd		
  T1 = T,			% zelfde persoon?
  %write("X"),
  logf('R', ws(Num,Cat,T,_Tijd1), nee),
  fail.
oproepafzeg(Cat, Tijd, Str) :-
  ws(Num,Cat,T,Tijd1),		% er was eens een planning
  w2(Num,Cat,Tijd2,G1,G2,_,_,_),% en een nieuwe
  wd(Num,Cat,A,B,_,_,_),		
  lr2(T1,A,B,_, G1, G2, nee),	% 30.6.2007		
  T1 = T,			% zelfde persoon?
  term_str(tgnst, T, TStr), 
  kleinste(Tijd1, Tijd2, TijdA),
  TijdA = Tijd,
  term_str(oprafztype, mijnverzet, SoortS),
  format(Str, "%\t%\t%\t%\t%",SoortS, Num, Cat, Tijd, TStr).
oproepAfzeg(Cat, Tijd, Str) :-
  w2(No,Cat,Tijd,G1,G2,_,_,_),
  wd(No,Cat,A,B,_,_,_),		
  nietGew(G1,G2,LR,Soort),	% oproep / correctie
  Soort = mijnoproepen,
  lr(T,A,B,LR),			% bepaal links of rechts
  not(ws(No,Cat,T,_)),		% was hiervoor al
  not(T = onbekend),
  not(T = openplaats(_)),
  term_str(tgnst, T, TStr), 
  term_str(oprafztype, Soort, SoortS),
  format(Str, "%\t%\t%\t%\t%",SoortS, No, Cat, Tijd, TStr).
oproepAfzeg(Cat, Tijd, Str) :-
  ws(Num,Cat,T,Tijd),		% er was eens een planning
  not(w2(Num,Cat,_,_,_,_,_,_)),	% nu geen planning
  term_str(tgnst, T, TStr), 
  term_str(oprafztype, mijnafzeggen, SoortS),
  format(Str, "%\t%\t%\t%\t%",SoortS, Num, Cat, Tijd, TStr).
oproepAfzeg(Cat, Tijd, Str) :-	% 10.6.2000
  ws(Num,Cat,T,Tijd),		% er was eens een planning
  w2(Num,Cat,_Tijd2,_G1,_G2,_,_,_),% en een nieuwe
  wd(Num,Cat,A,B,_,_,_),		% dus afzeggen
  not(lr1(T,A,B)),		% komt niet meer voor
  term_str(tgnst, T, TStr), 
  term_str(oprafztype, mijnafzeggen, SoortS),
  format(Str, "%\t%\t%\t%\t%\t%",SoortS, Num, Cat, Tijd, TStr).

oproepAfzeg1(Cat, Tijd, Str) :-
  w2(No,Cat,Tijd,G1,G2,_,_,_),
  wd(No,Cat,A,B,_,_,_),		
  nietGew(G1,G2,LR,Soort),	% oproep / correctie
  not(Soort = mijnoproepen),
  lr(T,A,B,LR),			% bepaal links of rechts
  not(ws(No,Cat,T,_)),		% was hiervoor al
  not(T = onbekend),
  term_str(tgnst, T, TStr), 
  term_str(oprafztype, Soort, SoortS),
  format(Str, "%\t%\t%\t%\t%",SoortS, No, Cat, Tijd, TStr).

oproepInVolgorde(Cat, Str) :-
  findall(Tijd,bn(Tijd, _),Tijden),
  sorttijd_c(Tijden),
  member(Tijd1,Tijden),
  wc(Cat, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
  oproepAfzeg(Cat, Tijd1,Str).
oproepInVolgorde(Cat, Str) :-
  findall(Tijd,bn(Tijd, _),Tijden),
  sorttijd_c(Tijden),
  member(Tijd1,Tijden),
  wc(Cat, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
  oproepAfzeg1(Cat, Tijd1,Str).

zoekTeken(Win, Teken, List, Aantal, NewPos, Zoeker) :-		% ander teken
  not(currentTeken(Win, _, _)),
  assert(currentTeken(Win, Teken, -1)), !,
  zoekTeken(Win, Teken, List, Aantal, NewPos, Zoeker).
zoekTeken(Win, Teken, List, Aantal, NewPos, Zoeker) :-		% ander teken
  currentTeken(Win, TekenC, _),
  TekenC <> Teken,
  retract(currentTeken(Win, _, _)),
  assert(currentTeken(Win, Teken, -1)), !,
  zoekTeken(Win, Teken, List, Aantal, NewPos, Zoeker).
zoekTeken(Win, Teken, List, _, FoundPos, Zoeker) :-		% zoek
  currentTeken(Win, _, Pos),
  assert(currentTPos(-1)),
  member(Str, List),						% ga de hele lijst langs
  currentTPos(N),
  FoundPos = N + 1,
  assert(currentTPos(FoundPos)),
  FoundPos > Pos,
  Zoeker(Str, Teken1),
  upper_lower(Teken,Teken1),
  retract(currentTeken(Win, _, _)),
  assert(currentTeken(Win, Teken, FoundPos)), !.
zoekTeken(Win, Teken, List, NoOfItems, NewPos, Zoeker) :-	% voorbij einde van de lijst
  currentTPos(N),
  currentTeken(Win, _, Pos),
  Pos > -1,
  N >= NoOfItems - 1,
  retract(currentTeken(Win, _, _)),
  assert(currentTeken(Win, Teken, -1)), !,		% begin van voren af aan
  zoekTeken(Win, Teken, List, NoOfItems, NewPos, Zoeker).
  
trepr(Str, Teken) :-			% uitslag
  parsetabs(Str, SList),
  SList = [_Tok, NoS, CatS, _, LRS, _ | _],
  str_int(NoS,No),
  str_int(CatS,Cat),
  wd(No,Cat,A,B,_,_,_),
  str_int(LRS, LR),
  lr(T,A,B,LR),
  spLetr(T, Teken).
trepr(Str, Teken) :-			% waarschuw
  parsetabs(Str, SList),
  SList = [_Tok, _NoS, _CatS, _, TStr],
  term_str(tgnst, T, TStr),
  spLetr(T, Teken).

tgnst_namen(LboxWin, s(No), No, Naam1, Len1, 0, "", 0) :- 
  sp2(No,_,Naam1,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  win_GetTextExtent(LboxWin, Naam1, -1, Len1, _Height), !.
tgnst_namen(LboxWin, pw(No), No, Naam1, Len1, 0, "", 0) :- 
  sp2(No,_,Naam1,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  win_GetTextExtent(LboxWin, Naam1, -1, Len1, _Height), !.
tgnst_namen(LboxWin, p(No1,No2), No1, Naam1a, Len1, No2, Naam2, Len2) :- 
  sp2(No1,_,Naam1,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  concat(Naam1, "+", Naam1a),
  win_GetTextExtent(LboxWin, Naam1a, -1, Len1, _Height),
  sp2(No2,_,Naam2,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  win_GetTextExtent(LboxWin, Naam2, -1, Len2, _), !.
tgnst_namen(_, _, 0, "???", 40, 0, "", 0). 

wrepr(Str, CDT, No, Cat, Tijd, Soort, T) :-
  parsetabs(Str, SList),
  SList = [Tok, NoS, CatS, TijdS, TStr | _],
  term_str(oprafztype, Soort, Tok), 
  %str_int(Tok, Soort),
  str_int(NoS,No),
  str_int(CatS,Cat),
  str_int(TijdS, Tijd),
  term_str(tgnst, T, TStr),
  bitright(Tijd,7,Dag),
  dag(Dag,DStr,_),
  tijd_str(Tijd,TStr1,_),
  wc(Cat, _, _, _, _, CatNam, _, _, _, _, _, _, _, _, _), !,
  format(CDT, "% %s %s", CatNam, DStr, TStr1).
wrepr(Str, CDT, No, Cat, Tijd, Soort, T) :-
  comline(LineBuffer),
  trim_c(LineBuffer),
  upper_lower(LineBuffer, LinebufferL), % alleen diagnostisch!
  searchstring(LinebufferL, "/x", _),
  parsetabs(Str, SList),
  SList = [Tok, NoS, CatS, TijdS, LRS, TStr | _],
  term_str(oprafztype, Soort, Tok), 
  str_int(NoS,No),
  str_int(CatS,Cat),
  str_int(TijdS, Tijd),
  str_int(LRS, LR),
  LR = 99,
  term_str(tgnst, T, TStr),
  ws(No, Cat, T, Tijd),
  bitright(Tijd,7,Dag),
  dag(Dag,DStr,_),
  tijd_str(Tijd,TStr1,_),
  %tgnst_naam(nee, b_False, Cat, T,  Naam, _Tel),
  wc(Cat, _, _, _, _, CatNam, _, _, _, _, _, _, _, _, _), !,
  format(CDT, "% %s %s", CatNam, DStr, TStr1).

draw_pictures(LboxWin, ItemId, Pnt, Tgnst, No, Cat, Sp1, Rect1, Sp2, Rect2) :-
  %retractall(hotMailSpot(LboxWin, Itemid, _, _)),
  erepr(Tgnst, Bitmap1, Offs1, Bitmap2, Offs2, No, Cat, Sp1, Sp2),
  draw_picture(LboxWin, ItemId, Pnt, Bitmap1, Offs1, Sp1, Rect1),
  draw_picture(Lboxwin, ItemId, Pnt, Bitmap2, Offs2, Sp2, Rect2), !.
  
draw_picture(_, _,  _, 0, _, _, rct(0,0,0,0)) :- !.
draw_picture(LboxWin, _ItemId, pnt(T, Tab6), Bitmap, OffsetB, _Sp, Rect) :-
  Picture1 = pict_GetFromRes(Bitmap),
  T1 = T + 3,
  H6 = Tab6 + OffsetB,
  R = H6 + 15,
  B = T1 + 10,
  pict_Draw(LboxWin, Picture1, pnt(H6, T1), rop_SrcCopy),
  Rect = rct(H6, T1, R, B), !.
  %asserta(hotMailSpot(LboxWin, ItemId, Sp, Rect)), !.

%w2e(No,Wc,T,Tijd,[mt(email,"",0)],[])

verstuurd(T, SpNo, No, Cat, idb_emeilkleinB) :-	% bevestigd 
  w2e(No,Cat,T,SpNo, _, _Tijd,_,Bev),
  not(Bev = []), !.
verstuurd(T, SpNo, No, Cat, idb_emailklein) :- 
  w2e(No,Cat,T,SpNo, _, _Tijd,_,_), !.
verstuurd(_T, _, _No, _Cat, idb_emailkleinD). 

erepr(s(No), Bitmap, 7, 0, 0, Num, Cat, No, 0) :-
  sp2(No,_,_Naam1,_,_,_,_,_Bet1,_, _Club1,_, _, _, _, _, _, _, _, Email, _,_,_,_,_,_,_,_),
  fronttoken(Email, _, _), !,
  verstuurd(s(No), No, Num, Cat, Bitmap). 
erepr(pw(No), Bitmap, 7, 0, 0, Num, Cat, No, 0) :-
  sp2(No,_,_Naam1,_,_,_,_,_Bet1,_, _Club1,_, _, _, _, _, _, _, _, Email, _,_,_,_,_,_,_,_),
  fronttoken(Email, _, _), !,
  verstuurd(pw(No), No, Num, Cat, Bitmap). 
erepr(p(No1,No2), Bitmap1, 0, Bitmap2, 15, No, Cat, No1, No2) :-
  sp2(No1,_,_Naam1,_,_,_,_,_Bet1,_, _Club1,_, _, _, _, _, _, _, _, Email1, _,_,_,_,_,_,_,_),
  fronttoken(Email1, _, _),
  sp2(No2,_,_Naam2,_,_,_,_,_Bet2,_, _Club2,_, _, _, _, _, _, _, _, Email2, _,_,_,_,_,_,_,_),
  fronttoken(Email2, _, _), !,
  verstuurd(p(No1,No2), No1, No, Cat, Bitmap1),
  verstuurd(p(No1,No2), No2, No, Cat, Bitmap2).
erepr(p(No1,No2), Bitmap, 0, 0, 0, No, Cat, No1, 0) :-
  sp2(No1,_,_Naam1,_,_,_,_,_Bet1,_, _Club1,_, _, _, _, _, _, _, _, Email1, _,_,_,_,_,_,_,_),
  fronttoken(Email1, _, _), !,
  verstuurd(p(No1,No2), No1, No, Cat, Bitmap). 
erepr(p(No1,No2), 0, 0, Bitmap, 15, No, Cat, No2, 0) :-
  sp2(No2,_,_Naam2,_,_,_,_,_Bet2,_, _Club2,_, _, _, _, _, _, _, _, Email2, _,_,_,_,_,_,_,_),
  fronttoken(Email2, _, _), !,
  verstuurd(p(No1,No2), No2, No, Cat, Bitmap). 
%erepr(p(_,_), idb_emailLRD, 0) :- !.
erepr(_, 0, 0, 0, 0, _, _, 0, 0).

infostatus(Win, rct(L,T,_R,_B), "OK") :-
  T1 = T + 3,
  H1 = L + 3,
  Picture1 = pict_GetFromRes(idb_waarsOK1),
  pict_Draw(Win, Picture1, pnt(H1, T1), rop_SrcCopy), !.
infostatus(Win, RCT, Short) :-
  RCT = rct(L,T,R,B),
  T1 = T + 3,
  H1 = L + 3,
  H2 = H1 + 15,
  Rct1 = rct(H2,T,R,B),
  Picture1 = pict_GetFromRes(idb_waarsNOG),
  pict_Draw(Win, Picture1, pnt(H1, T1), rop_SrcCopy),
  draw_TextInRect(Win,RCT1, Short, -1, [dtext_center]).



draw_Item(LBoxWin, ItemId, RectItem, Color) :-
  RectItem = rct(_L, T, R, B),
  T1 = T - 1, T2 = T + 1,
  retractall(hotWaarschuwspot(LboxWin, _, rct(_, T, _, _), _, _, _, _, _)), 
  retractall(hotWaarschuwspot(LboxWin, _, rct(_, T1, _, _), _, _, _, _, _)), 
  retractall(hotWaarschuwspot(LboxWin, _, rct(_, T2, _, _), _, _, _, _, _)), 

  Item = lbox_GetItem(LboxWin, ItemId),	% get the string
  wrepr(Item, _CDT, No, Cat, Tijd, Soort, Tgnst),
  %not(Naam = "Diagnostisch"),
  %not(LR = 99),
  titel(Soort,_,Short,_,_),
  itemAfmW(_Win, Tab0, Tab1, Tab2, Tab3, Tab4, Tab5, Tab6, Tab7),
  Rect0 = rct(0,T,Tab0,B),
  infostatus(Lboxwin, Rect0, Short),
  Rect1 = rct(Tab0, T, Tab1, B),
  wc(Cat, _, _, _, _, CatNam, _, _, _, _, _, _, _, _, _),
  trim_c(CatNam), 
  draw_TextInRect(LboxWin,Rect1, CatNam, -1, [dtext_left]),
  Rect2 = rct(Tab1, T, Tab2, B),
  bitright(Tijd,7,Dag),
  dag(Dag,DStr,_),
  trim_c(DStr),
  draw_TextInRect(LboxWin,Rect2, DStr, -1, [dtext_right]),
  Rect3 = rct(Tab2, T, Tab3, B),
  tijd_str(Tijd,TStr1,_),
  draw_TextInRect(LboxWin,Rect3, TStr1, -1, [dtext_right]),
  Rect4 = rct(Tab4, T, Tab5, B),
  %Rect5 = rct(Tab7, T, R, B),
  draw_pictures(LboxWin, ItemId, pnt(T, Tab6), Tgnst,No,Cat, Sp1, RectSp1, Sp2, RectSp2),
  tgnst_namen(LboxWin, Tgnst, No1, Naam1, Len1, No2, Naam2, _Len2),
  R1 = Tab7 + Len1,
  RectN1 = rct(Tab7, T, R1, B), 
  draw_TextInRect(LboxWin, RectN1, Naam1, -1, [dtext_left]),
  RectN2 = rct(R1, T, R, B),
  draw_TextInRect(LboxWin, RectN2, Naam2, -1, [dtext_left]),
  win_SetForeColor(LboxWin, Color),
  retractall(hotWaarschuwspot(LboxWin, ItemId, _, _, _, _, _, _)), 
  asserta(hotWaarschuwspot(LboxWin, ItemId, Rect0, wedspot(No, Cat, Rect1), mailspot(Sp1, RectSp1), mailspot(Sp2, RectSp2), spelerspot(No1, RectN1), spelerspot(No2, RectN2))), 
  w2(No,Cat,_,_,_,_,_,BaanPark), !,
  draw_TextInRect(LboxWin,Rect4, BaanPark, -1, [dtext_left]).


close_waarsch() :-
  soort(Win, _),
  win_Destroy(Win),
  fail.
close_waarsch().

update_waarsch() :-
  soort(Win, _),
  win_PostEvent(Win,e_User(4,0)),	% refresh
  trap(win_Invalidate(Win),_,true),
  fail.
update_waarsch().

soortAfhandeling(Win, Num, Cat,  Naam, Tgnst, mijnoproepen, Answer) :-
  format(Str, "%s %s", "Oproepen", Naam),
  dlg_plangegevens_Create(mijnoproepen, Win, Str, Tgnst, Num, Cat, Answer, b_False), !.
soortAfhandeling(Win, Num, Cat,  Naam, Tgnst, mijnverzet, Answer) :-
  format(Str, "%s %s", "Verzetten", Naam),
  dlg_plangegevens_Create(mijnoproepen, Win, Str, Tgnst, Num, Cat, Answer, b_False), !. % eigenlijk mijnverzet
soortAfhandeling(_Win, Num, Cat,  Naam, Tgnst, mijnafzeggen, Answer) :-
  format(Str, "%s %s", "Afzeggen", Naam),
  dlg_plangegevens_Create(mijnoproepen, _Win, Str, Tgnst, Num, Cat, Answer, b_False), !. 	% 22.8.2009
%  format(Str, "Is/zijn %s afgezegd en dus op de hoogte?", Naam),
%  1 = dlg_MessageBox( "Afzeggen", Str, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
%  ws(Num,Cat,Tgnst,Tijd1),		% er was eens een planning
%  logf('R', ws(Num,Cat,Tgnst,Tijd1), nee), !.
soortAfhandeling(Win, Num, Cat,  Naam, T, mijncorrectie, Answer) :-
  format(Str, "%s %s", "Toch niet geïnformeerd", Naam),
  wd(Num,Cat,A,B,_,_,_),
  lr(T,A,B,LR),			% bepaal links of rechts
  LR1 = LR + 1,
  dlg_vervolgpartij_Create(ja, Win, Num, Cat, Str,nee, LR1, Answer,b_False), !.
soortAfhandeling(_Win, _Num, _Cat, _Naam, _Tgnst, mijnafzeggen, idc_cancel).

save_WaarschWin() :-
  itemAfmW(Win, _Tab0, _Tab1, _Tab2, _Tab3, _, _, _, _),
  updatevensterpositie(oproepvenster, Win), !.
save_WaarschWin().


subclassw_eh(LboxWin,e_MouseDown(PNT,_ShiftCtlAlt,_Button),0):-
  hotWaarschuwspot(LboxWin, Index, Rect, _, _, _, _, _), 
  %hotInformeerspot(LboxWin, Index, Rect),
  rect_PntInside(Rect,PNT),
  getbacktrack(Here),
  Item = lbox_GetItem(LboxWin, Index),
  wrepr(Item, CDT, Num, Cat, _Tijd,SoortAfh, Tgnst),
  tgnst_naam(nee, b_False, Cat, Tgnst,NaamX,_),
  format(Naam, "% %", CDT, NaamX), !,
  %select(Cat, Num),
  TaskWin = vpi_GetTaskWin(),
  cutbacktrack(Here),
  selectW(Index), 
  Parent = win_GetParent(LBoxWin),
  soortAfhandeling(Parent, Num, Cat,  Naam, Tgnst, SoortAfh, Answer), 
  %win_Invalidate(Parent),
  %win_SetFocus(TaskWin),
  %lbox_SetSel(LboxWin, Index, b_True),
  win_waarschuwinvoer_Create(Taskwin, []),
  selectW(Index), 
  %itemAfmW(Parent, _Tab0, _Tab1, Tab2, _Tab3, _Tab4, _Tab5, _Tab6, _Tab7),
  %PNT = pnt(X, Y), X1 = X + Tab2 + 5, PNT1 = pnt(X1, Y),
  %win_PostEvent(LboxWin,e_MouseDown(PNT1,_ShiftCtlAlt,Button)),
  trap(win_destroy(Parent), _, fail),
  Answer = idc_ok,
	update_proj_toolbar(),
	update_situatie(),
	update_uitslagen("tasuitsl wwaarsch"),!.
subclassw_eh(_LWin,e_Char('\27',_ShiftCtlAlt),0):-!,
  Parent=win_GetParent(_LWin),
  win_Destroy(Parent).
subclassw_eh(_LWin,e_Char(Kar,_ShiftCtlAlt),0):-
  Parent=win_GetParent(_LWin),
  win_SendEvent(Parent,e_Char(Kar,_ShiftCtlAlt)), !.
%subclassw_eh(LboxWin,e_MouseDown(PNT,_ShiftCtlAlt,mouse_button_right),0):-
subclassw_eh(LboxWin,e_MouseDown(PNT,_ShiftCtlAlt,_),0):-
  hotWaarschuwspot(LboxWin, _, _, wedspot(No, Cat, Rect), _, _, _, _), 
  %hotWedstrijdSpot(LboxWin, _, No, Cat, Rect),
  rect_PntInside(Rect,PNT),
  %write("\nselected: ", No, Cat),
  select(Cat, No), !,
  fail.
subclassw_eh(LboxWin,e_MouseDown(PNT,_ShiftCtlAlt,_),0):-
  hotWaarschuwspot(LboxWin, _ItemId, _, _, mailspot(SpNo, Rect), _, _, _), 
  %hotMailspot(LboxWin, _Itemid, Spno, Rct),
  %lbox_SetSel(LboxWin, Itemid, b_True),  
  rect_PntInside(Rect,PNT),
  getspeleroproepen(SpNo), !,
  fail.
subclassw_eh(LboxWin,e_MouseDown(PNT,_ShiftCtlAlt,_),0):-
  hotWaarschuwspot(LboxWin, _ItemId, _, _, _, mailspot(SpNo, Rect), _, _), 
  %hotMailspot(LboxWin, _Itemid, Spno, Rct),
  %lbox_SetSel(LboxWin, Itemid, b_True),  
  rect_PntInside(Rect,PNT),
  getspeleroproepen(SpNo), !,
  fail.
subclassw_eh(LboxWin,e_MouseDown(PNT,_ShiftCtlAlt,_),0):-
  hotWaarschuwspot(LboxWin, _, _, _, _, _, spelerspot(SKeus, RCT), _), 
  rect_PntInside(RCT,PNT),
  sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
  SpelerIn = sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
  TaskWin = vpi_GetTaskWin(),
  dlg_persoonsgegevens_Create(TaskWin, SpelerIn ), !, 
  fail.
subclassw_eh(LboxWin,e_MouseDown(PNT,_ShiftCtlAlt,_),0):-
  hotWaarschuwspot(LboxWin, _, _, _, _, _, _, spelerspot(SKeus, RCT)), 
  rect_PntInside(RCT,PNT),
  sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
  SpelerIn = sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
  TaskWin = vpi_GetTaskWin(),
  dlg_persoonsgegevens_Create(TaskWin, SpelerIn ), !, 
  fail.
   
allemaalgewaarschuwd() :-
  wc(Cat, _, _, _, _, _CatNam, _, _, _, _, _, _, _, _, _),
  findall(Nr, w2(Nr,Cat,_,_,_,_,_,_), NrList),
  member(Num, NrList),
  w2(Num,Cat,Tijd1,_G1,_G2,Fix,Tijd2,Bn),
  not(w2(Num,Cat,_,ja,ja,_,_,_)),
  cursor_SetWait(),
  logf('A',  w2(Num,Cat,Tijd1,ja,ja,Fix,Tijd2,Bn), ja),
  fail.
allemaalgewaarschuwd() :-
  ws(Num,Cat,T,Tijd1),
  logf('R', ws(Num,Cat,T,Tijd1), ja),
  fail.
allemaalgewaarschuwd() :-
  logclose(),
  update_display(),
  update_waarsch(),
  update_situatie(),
  update_uitslagen("allemaalgewaarschuwd"),
  update_proj_toolbar().

nognietgewaarschuwd() :-
  1 = dlg_MessageBox( "Oproepen resetten", "Moet de server ook de verstuurde oproepmails vergeten?\n(Alle envelopjes worden dan weer grijs)", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
  providerX(Prov, _, "", _),
  format(URL,"http://%s/oproepcurl3.php", Prov), 
  get_TempDir(_, TempFile),
  deletefile(TempFile),
  AanmeldenVars = aanmeldenvars(),
  _Return = downloadData(URL, TempFile, ["type", "deleteOproepen"| AanmeldenVars], _HttpResponsecode, 0),
  
  fail.
nognietgewaarschuwd() :-
  wc(Cat, _, _, _, _, _CatNam, _, _, _, _, _, _, _, _, _),
  findall(Nr, w2(Nr,Cat,_,_,_,_,_,_), NrList),
  member(Num, NrList),
  w2(Num,Cat,Tijd1,_G1,_G2,Fix,Tijd2,Bn),
  not(w2(Num,Cat,_,nee,nee,_,_,_)),
  cursor_SetWait(),
  logf('A',  w2(Num,Cat,Tijd1,nee,nee,Fix,Tijd2,Bn), ja),
  fail.
nognietgewaarschuwd() :-
  logclose(),
  update_display(),
  update_waarsch(),
  update_situatie(),
  update_uitslagen("nog niet gewaarschuwd"),
  update_proj_toolbar().

complItem(Cat, mijnoproepen) :-
  w2(No,Cat,_Tijd,G1,G2,_,_,_),
  nietGew(G1,G2,LR,mijnoproepen),	% oproep / correctie
  wd(No,Cat,A,B,_,_,_),		% dus afzeggen
  lr(T,A,B,LR),			% bepaal links of rechts
  not(ws(No,Cat,T,_)),		% niet ook afzeggen??
  not(T = onbekend).
complItem(Cat, mijncorrectie) :-
  w2(No,Cat,_Tijd,ja,_,_,_,_),
  wd(No,Cat,A,_,_,_,_),		
  not(A = onbekend).
complItem(Cat, mijncorrectie) :-
  w2(No,Cat,_Tijd,_,ja,_,_,_),
  wd(No,Cat,_,B,_,_,_),		
  not(B = onbekend).

completion(Text) :-
  findall(C, complItem(C, mijnoproepen), List1),
  count(List1, 0, Aant1),
  findall(Wc2, complItem(Wc2, mijncorrectie), List2),
  count(List2, 0, Aant2),
  Aant1 + Aant2 > 0,
  Procent = Aant2 * 100 div (Aant1 + Aant2),
  format(Text, "Percentage bevestigd: %u%%", Procent), !.
completion("--").

synchroniseer(Win) :-
  win_SendEvent(Win,e_Menu(idt_downloadbevestig,c_Nothing)),
  algesynchroniseerd(nee),
  assert(algesynchroniseerd(ja)),
  dlg_MessageBox( "Oproepen/afzeggen", "Dit scherm bevat hyperlinks!\n\nDe reeds gewaarschuwde spelers/koppels worden onderaan de lijst weergegeven!\n\nOproepen na wijzigingen kan herhaald worden\nzonder dat iedereen weer een mail krijgt!", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  !.
synchroniseer(_).

getSpelerOproepen(SKeus) :-
  sp2(SKeus,_,_NaamN,_,_ ,_,_,_,_,_,_, _,_Bondsnummer,_,_,_, _,_,EMail,_,_,_,_,_,_,_,_),
  providerX(Prov, _, "", _),
  format(URL,"http://%s/showmail.php", Prov),
  _Result = vpi_ProcessEvents(),
  get_TempDir(_, TempFile),
  filenameext(TempFile, Name, _Ext),
  filenameext(TempfileH, Name, ".html"),
  deletefile(TempFileH),
  AanmeldenVars = aanmeldenvars(),
  %naam(_ToernooiNaam, _Nr, _Datum, TicketI, _District, _),
  %maketicket(TicketI, Ticket),
  str_int(SKeusS, SKeus),
  _Return = downloadData(URL, TempFileH, ["type", "mails", "mailadres", Email, "speler", SkeusS | AanmeldenVars], _HttpResponsecode, 0),
  file_str(TempFileH, SStr),
  write("\n", TempFileH, "\n", SSTR),
  BeheerId = visSession(TempFileH, "BEHEER="),
  format(URL1,"http://%s/showmail.php?%s&stap=twee", Prov, BeheerId),
  write(URL1),
  Null = cast(api_hwnd,0),
  api_ShellExecute(Null,"open",URL1,"","",1), !.		% open in browser
getSpelerOproepen(_Speler).

updateWaarschuwVoorselectie(Voorselectie) :-
  soort(_, Voorselectie), !.
updateWaarschuwVoorselectie(Voorselectie) :-
  retract(soort(Win, _)),
  assert(soort(Win, Voorselectie)), !.
updateWaarschuwVoorselectie(_).

selectW(Index) :-
  soort(Win, _),
  win_PostEvent(Win,e_User(6,Index)), !.
selectW(_Index).

  win_waarschuwinvoer_Create(_, Voorselectie):-
	soort(Win, _),
	retractall(soort(Win, _)),
	assert(soort(Win, Voorselectie)),
	win_SetState(Win, [wsf_Restored]),
	trap(win_SetFocus(Win), _, true), !.
  win_waarschuwinvoer_Create(_Parent, Voorselectie):-
	NulWin = cast(WINDOW, 0),
	assert(soort(NulWin, Voorselectie)),	% alleen nog nodig voor het window.
	win_CreateDyn(
		[
%BEGIN Waarschuwinvoer, WinDefList, 23:21:05-1.7.2012, Code automatically updated!
		 topwin(wdef(win_waarschuwinvoer_WinType,win_waarschuwinvoer_RCT,win_waarschuwinvoer_Title,u_Pixels),
			win_waarschuwinvoer_Menu,win_waarschuwinvoer_Flags),
		 ctl(wdef(wc_LBox,rct(26,42,373,233),"",u_Pixels),idcw_lb,[wsf_OwnerDraw,wsf_Group,wsf_TabStop,wsf_VScroll,wsf_HasStrings,wsf_NoBorder])
%END Waarschuwinvoer, WinDefList
		],_Parent,win_waarschuwinvoer_eh,0).

%BEGIN Waarschuwinvoer, e_Create
  win_waarschuwinvoer_eh(_Win,e_Create(_),0):-!,
        NewFont = font_Create(ff_Helvetica, [], 10),
  	win_SetFont(_Win, NewFont),
	NulWin = cast(WINDOW, 0),
	retract(soort(NulWin, Voorselectie)),
	assert(soort(_Win, Voorselectie)),
	win_PostEvent(_Win,e_User(1,0)),
	titel(mijnoproepen, Titel,_, Venstertype,_),
	win_SetText(_Win, Titel),
	win_GetTextExtent(_Win, " ", -1, Bspace, _),
	maxDatL(_Win, DatL),
	maxOndL(_Win, Tab11),
	win_GetTextExtent(_Win, "afzeggen  ", -1, Taba, _),
	Tab0 = Taba + 18,
	Tab1 = Tab0 + Tab11, 
	%win_GetTextExtent(_Win, "ma 27/12 ", -1, T2, _),
	Tab2 = Tab1 + DatL, 
	win_GetTextExtent(_Win, "23u25", -1, T3, _),
	Tab3 = Tab2 + T3,
	maxParkL(_Win, ParkL, Bspace, Extra),
	Tab4 = Tab3 + Bspace,
	Tab5 = Tab4 + ParkL,
	Tab6 = Tab5 + Extra,
	Tab7 = Tab6 + 32,
	vensterpositie(Venstertype, _Win),
%BEGIN Waarschuwinvoer, ToolbarCreate, 23:21:05-1.7.2012, Code automatically updated!
	tb_informeren_Create(_Win),
%END Waarschuwinvoer, ToolbarCreate
	assert(itemAfmW(_Win, Tab0, Tab1, Tab2, Tab3, Tab4, Tab5, Tab6, Tab7)),
	!.
%END Waarschuwinvoer, e_Create
%MARK Waarschuwinvoer, new events


%BEGIN Waarschuwinvoer, idt_downloadbevestig
  win_waarschuwinvoer_eh(_Win,e_Menu(idt_downloadbevestig,_ShiftCtlAlt),0):-
	getbevestigingen(),
	!.
%END Waarschuwinvoer, idt_downloadbevestig

%BEGIN Waarschuwinvoer, id_email_oproepen
  win_waarschuwinvoer_eh(_Win,e_Menu(id_email_oproepen,_ShiftCtlAlt),0):-
	soort(_, []),
	stuurBerichtenOproep(_Win, bulk),	% er is al verbinding
	win_PostEvent(_Win,e_User(4,0)),	% refresh
	!.
  win_waarschuwinvoer_eh(_Win,e_Menu(id_email_oproepen,_ShiftCtlAlt),0):-
	soort(_Win, Lijst),
	stuurBerichtenOproep(_Win, collectie(Lijst)),	% er is al verbinding
	win_PostEvent(_Win,e_User(4,0)),	% refresh
	!.
%END Waarschuwinvoer, id_email_oproepen



%BEGIN Waarschuwinvoer, e_GetFocus
  win_waarschuwinvoer_eh(_Win,e_GetFocus,0):-!,
	win_PostEvent(_Win,e_User(3,0)),	% geef focus door
	!.
%END Waarschuwinvoer, e_GetFocus

%BEGIN Waarschuwinvoer, e_Char
  win_waarschuwinvoer_eh(_Win,e_Char('\27',_ShiftCtlAlt),0):-
	win_Destroy(_Win),
	!.
  win_waarschuwinvoer_eh(_Win,e_Char(k_f3,_ShiftCtlAlt),0):-
	LboxWin = win_GetCtlHandle(_Win, idcw_lb),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	wrepr(Item, _, Num, Cat, _Tijd, _SoortAfh, _T),
	select(Cat, Num),	
	!.
  win_waarschuwinvoer_eh(_Win,e_Char(k_f7,_ShiftCtlAlt),0):-
	LboxWin = win_GetCtlHandle(_Win, idcw_lb),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	wrepr(Item, _, Num, Cat, _Tijd,_SoortAfh, _T),
	TaskWin = vpi_GetTaskWin(),
	situatie(TaskWin, Num, Cat, 0),
	!.
  win_waarschuwinvoer_eh(_Win,e_Char(Getal,_ShiftCtlAlt),0):-
	LboxWin = win_GetCtlHandle(_Win, idcw_lb),
	LboxList = lbox_GetAll(LboxWin),
	NoOfItems = lbox_CountAll(LboxWin),
	Teken = cast(char, Getal),
	zoekTeken(_Win, Teken, LboxList, NoOfItems, NewPos, trepr),
	lbox_SetSel(LboxWin, NewPos, b_True),
	!.
%END Waarschuwinvoer, e_Char


%BEGIN Waarschuwinvoer, e_Destroy
  win_waarschuwinvoer_eh(_Win,e_Destroy,0):-!,
	soort(_Win, _Voorselectie),
	titel(mijnoproepen, _Titel,_, Venstertype,_),
	updatevensterpositie(Venstertype, _Win),
	retract(soort(_Win, _)),
	retract(itemAfmW(_Win, _, _, _, _, _, _, _, _)),
	LboxWin = win_GetCtlHandle(_Win, idcw_lb),
	retractall(hotWaarschuwspot(LboxWin, _, _, _, _, _, _, _)), 
	!.
%END Waarschuwinvoer, e_Destroy

%BEGIN Waarschuwinvoer, idcw_lb activated
  win_waarschuwinvoer_eh(_Win,e_Control(idcw_lb,_CtrlType,LboxWin,activated),0):-
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	wrepr(Item, CDT, Num, Cat, _Tijd,SoortAfh, Tgnst),
	tgnst_naam(nee, b_False, Cat, Tgnst,NaamX,_),
	format(Naam, "% %", CDT, NaamX),
	select(Cat, Num),
	soortAfhandeling(_Win, Num, Cat,  Naam, Tgnst, SoortAfh, Answer),
	Answer = idc_ok,
	select(Cat, Num),
	win_PostEvent(_Win,e_User(4,0)),	% refresh
	lbox_SetSel(LboxWin, Index, b_True),
	win_PostEvent(_Win,e_User(5,0)),	% set focus
	update_proj_toolbar(),
	update_situatie(),
	update_uitslagen("waarsch_invoer activated"),
	!.
  win_waarschuwinvoer_eh(_Win,e_Control(idcw_lb,_CtrlType,LboxWin,activated),0):-
	win_SetFocus(LboxWin), !.
%END Waarschuwinvoer, idcw_lb activated

%BEGIN Waarschuwinvoer, e_User
  win_waarschuwinvoer_eh(_Win,e_User(1,_Ptr),0):-	% create
	LboxWin = win_GetCtlHandle(_Win, idcw_lb),
	retractall(hotWaarschuwspot(LboxWin, _, _, _, _, _, _, _)), 
        NewFont = font_Create(ff_Helvetica, [], 10),
  	win_SetFont(LboxWin, NewFont),
	completion(Completion),
	toolbar_Setvalue(_Win, idti_gereed, text_value(Completion)),
	soort(_Win, _Voorselectie),
	LboxWin = win_GetCtlHandle(_Win, idcw_lb),
	RCT = win_GetClientRect(_Win),
	win_Move(LboxWin,RCT),
	soort(_Win, _Voorselectie),
	findall(Wedid,oproepInVolgorde(_,Wedid),WedsL1),
	lbox_Suspend(LboxWin),
	lbox_Clear(LboxWin),
	lbox_Add(LboxWin, WedsL1),
	lbox_Resume(LboxWin),
	lbox_SetSel(LboxWin, 0, b_True),
	win_SetFocus(LboxWin),
	win_SetSubclassHandler(LboxWin,subclassw_eh,b_false),
	synchroniseer(_Win),
	!.
  win_waarschuwinvoer_eh(_Win,e_User(2,_Ptr),0):-	% resize
	%write("\nresize.."),
	LboxWin = win_GetCtlHandle(_Win, idcw_lb),
	RCT0 = win_GetClientRect( _Win ),
	itemAfmW(_Win, _Tab0, _Tab1, _Tab2, _Tab3, _Tab4, _, _, _),
	toolbar_GetRect(_Win,tb_bottom,TbRCT),
	TbRCT = rct(_, T, _, B),
	RCT0 = rct(L, T0, R, H0),
	H1 = H0 - B + T,	% adjust voor toolbar
	win_Move(LboxWin,rct(L, T0, R, H1)),
	win_Invalidate(_Win),
	!.
  win_waarschuwinvoer_eh(_Win,e_User(3,_Ptr),0):-
	LboxWin = win_GetCtlHandle(_Win, idcw_lb),
	trap(win_SetFocus(LboxWin),_, true),
	!.
  win_waarschuwinvoer_eh(_Win,e_User(4,_Ptr),0):-	% refresh
	completion(Completion),
	toolbar_Setvalue(_Win, idti_gereed, text_value(Completion)),
	soort(_Win, _Voorselectie),
	LboxWin = win_GetCtlHandle(_Win, idcw_lb),
	findall(Wedid,oproepInVolgorde(_,Wedid),WedsL1),
	LboxList = lbox_GetAll(LboxWin),
	not(LBoxList = WedsL1),				% niet hetzelfde gebleven
	lbox_GetSel(LboxWin,_ItemList,IndexList),
	lbox_Suspend(LboxWin),
	lbox_Clear(LboxWin),
	lbox_Add(LboxWin, WedsL1),
	lbox_Resume(LboxWin),
	IndexList = [SelIndex|_],
	lbox_SetSel(LboxWin, SelIndex, b_True),
	!.
  win_waarschuwinvoer_eh(_Win,e_User(5,_Ptr),0):-
	trap(win_BringToTop(_Win),_,true),
	!.
  win_waarschuwinvoer_eh(_Win,e_User(6,SelIndex),0):-
	LboxWin = win_GetCtlHandle(_Win, idcw_lb),
	lbox_SetSel(LboxWin, SelIndex, b_True),
	!.
%END Waarschuwinvoer, e_User

%BEGIN Waarschuwinvoer, e_OwnerMeasureItem
  win_waarschuwinvoer_eh(_Win,e_OwnerMeasureItem(_CtlType,_CtlId,_ItemId,_Data),Size):-!,
	win_GetTextExtent(_Win, "JansenenTilanusg", -1, Width, He),
	Height = He,
	bitleft(Height, 16, H1),
	Size = Width + H1,
	!.
%END Waarschuwinvoer, e_OwnerMeasureItem

%BEGIN Waarschuwinvoer, e_OwnerDraw
  win_waarschuwinvoer_eh(_Win,e_OwnerDraw(_CtlType,_CtlId,_ItemId,_ItemAction,
		ItemState,LboxWin,RectItem,_ItemData),0):-
	own_member1(ItemState, odstate_Selected), !,  % was selected
	win_SetPen(LboxWin,pen(1,ps_Solid,color_Blue)),
	win_SetForeColor(LboxWin, color_White),
	win_SetBackColor(LboxWin, color_Blue),
	win_SetBrush(LboxWin,brush(pat_Solid, color_Blue)),
	draw_Rect(LboxWin,RectItem),
	draw_Item(LBoxWin, _ItemId, RectItem, color_White),
	!.
  win_waarschuwinvoer_eh(_Win,e_OwnerDraw(_CtlType,_CtlId,_ItemId,_ItemAction,
		_ItemState,LboxWin,RectItem,_ItemData),0):-!,
	_Itemstate = [],
	win_SetPen(LboxWin,pen(1,ps_Solid,color_White)),
	draw_Rect(LboxWin,RectItem),
	win_SetForeColor(LboxWin, color_Black),
	win_SetBackColor(LboxWin, color_White),
	draw_Item(LBoxWin, _ItemId, RectItem, color_LtGray),
	!.
%END Waarschuwinvoer, e_OwnerDraw

%BEGIN Waarschuwinvoer, e_Size
  win_waarschuwinvoer_eh(_Win,e_Size(_Width,_Height),0):-
ifdef use_tbar
	toolbar_Resize(_Win),
enddef
	win_PostEvent(_Win,e_User(2,0)),		% resize listbox
	!.
%END Waarschuwinvoer, e_Size

  win_waarschuwinvoer_eh(_Win,e_Menu(id_Wedstrijd_plan,_ShiftCtlAlt),0):-
	LboxWin = win_GetCtlHandle(_Win, idcw_lb),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	wrepr(Item, _, Num, Cat, _Tijd, _SoortAfh, _),
	TaskWin = vpi_GetTaskWin(),
	planAangewezenWeds(TaskWin, Num, Cat),
	update_display(),
	update_waarsch(),
	update_situatie(),
	update_uitslagen("wedstrijd_plan"), !.


%BEGIN Waarschuwinvoer, e_Menu, Parent window 
  win_waarschuwinvoer_eh(Win,e_Menu(ID,CAS),0):-!,
	PARENT = win_GetParent(Win),
	win_SendEvent(PARENT,e_Menu(ID,CAS)),
	!.
%END Waarschuwinvoer, e_Menu, Parent window

%END_WIN Waarschuwinvoer

%______________________ plaatsen, uitsluiten, etc.

%domains 
%keurdom = goedgekeurd; nietgekeurd; afgekeurd

constants
 %nakijken = 27
 teplaatsen = 28
 prefresh = 29

database - topgaven
             /*  opg#    type          pltsng  ronde   uitsl  indelingsnum LL Keur */
  opg_t(WINDOW, integer,wedscat,tgnst,integer,integer,integer,integer,string, string, teamstatus, string)
  winCat(WINDOW, wedscat)
  single maxAfmeting(integer)
  %single aantalGekopieerd(integer)
  %single aantalVerhuisd1(integer)
  single sorteerVeld(integer)
  single plaatsingVeranderd(BOOLEAN)
  single iafm(integer Status, integer Datum, integer Vrijveld, integer Plts, integer Sterkte, integer Rating, integer RatingDelta, 
          integer Uitsl, integer Rangorde, integer RangordeDelta)
  single eersteIndex(integer Num)
  determ selectedOpgave(integer Num, wedscat, integer EersteFocus)
  determ heeftfocus
  single nietBeklijven(boolean)

predicates
  copyOpg(WINDOW, wedscat)
  saveOpg(WINDOW)
  compOpg(WINDOW)
  nondeterm   opgListBase(WINDOW Win, wedscat Cat, string SorteerVeld, integer Volgorde)
  determ      opgListBase1(integer No, wedscat, tgnst, integer Plts, string SorteerVeld, integer Volgorde)
  opg_repr(WINDOW Win, string StrIn, integer No, wedscat Cat, string Tijd, string WCVrij, string Plts, 
  	string Sterkte, string RatingStr, string RatingDelta, color RatingDeltaKleur, string Uitsluit,
  	 string Naam, string LL, string Keur, color KeurKl, string RangOrde, string RangDelta, color) 
     - (i, i, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o)
  procedure str_int_special(string, integer)
  procedure str_int_uitsl(string, integer, color) - (o, i, o)
  procedure str_int_ronde(string, integer) - (o, i)
  procedure str_int_plaats(string, integer) - (o, i)
  procedure str_int_rang(string, integer Rang) - (i, o)
  in_wedstrijd(wedscat, tgnst)
  check_sex(sexlist, sexlist)
  check_spelers(wedscat, integerlist, string)
  checkDeelInButtons(WINDOW)
  sp2opgt(integer, wedscat)
procedure zoekIndex(integer NoI, wedscat CatI, slist, integer In, integer Uit)
%  procedure keur2tekst(wedscat, ilist Tegenstanders, teamstatus,string Kort, string Tekst,color) - (i,i,i,o,o,o)
procedure blancoAchteraan(string, string)
procedure reprrang(integer, string)
procedure reprrangDubbel(string In1, string In2, string Rang, string RangDelta, string RangSort)
nondeterm gep(window Win, wedscat Cat, integer)
completeerElem : xml_EndElementHandler
procedure string amember(slist, slist Keyws) - (i,i)
procedure ratingDeltaColor(real, color)
determ voorNa(window Win, wedscat Cat, wedscat ErNa, integer Eigencount, integer ErNaCount) - (i,i,o,o,o)
determ erna(wedscat Cat, catl, wedscat Erna) -  (i,i,o)
procedure count2enabled(integer, integer)
determ typematch(wedstyp,wedstyp)

clauses
nietBeklijven(b_True).
completeerelem(_, _) .

maxAfmeting(0).
plaatsingVeranderd(b_False).
iafm(0, 0, 0, 0, 0, 0, 0, 0, 0, 0).
eersteIndex(0).

amember([Keyw1, Value | _], KeywL, ValueA) :-
  member(Keyw1, KeywL),
  utf_8_to_ansi(Value,ValueA), !.
amember([ _, _ | Rest], KeywL, Value) :-
  Value = amember(Rest, KeywL), !.
amember(_, _, "").

copyOpg(Win, Cat) :-
  loglock(mdi, "copyOpg"),
  logclose(),
  opg(Opg,Cat,Tgnst,Plaats,Ronde,Uitsl,Tijd,LL,TID,Keur,MT),
  assert(opg_t(Win,Opg,Cat,Tgnst,Plaats,Ronde,Uitsl,Tijd,LL,TID,Keur,MT)),
  fail.
copyOpg(Win, Cat) :-	% ook van de Cat erna igv ratingtoernooi
  voorNa(Win, Cat, ErNa, _Eigencount, _ErNaCount),
  opg(Opg,Erna,Tgnst,Plaats,Ronde,Uitsl,Tijd,LL,TID,Keur,MT),
  assert(opg_t(Win,Opg,Erna,Tgnst,Plaats,Ronde,Uitsl,Tijd,LL,TID,Keur,MT)),
  fail.
copyOpg(Win, Cat) :-   % en sorteer die dan ook nog eens een keer
  voorNa(Win, Cat, ErNa, _Eigencount,_ErNaCount),
  sorteerVeld(GewensteSorteervolgorde),
  findall(Veld, opgListBase(Win, Erna, Veld, GewensteSorteervolgorde), OpgBaseL),  % daarom moet opg_t al bestaan
  sortstring_c(OpgBaseL, 1),
  member(Item,OpgBaseL),
  opg_repr(Win, Item, Num, _Cat, _Tijd, _LL, _Plts, _Sterkte, _RStr, _RD, _RDColor, _Uitsl, _Naam, _Naam1, _, _, _, _, _),
  getbacktrack(Here),
  retract(opg_t(Win,Num,Erna,Tgnst,Plaats,Ronde,Reserve,Tijd,LL,TID,Keur,MT)),
  cutbacktrack(Here),
  assertz(opg_t(Win,Num,Erna,Tgnst,Plaats,Ronde,Reserve,Tijd,LL,TID,Keur,MT)),
  fail.
copyOpg(_Win, _Cat).

formatrating(RD, RD4) :-	% 17.7.2016
  frontstr(2,RD, Fr, _),
  Fr = "10",
  str_len(RD, Len),
  Len > 4,
  concat("009", RD, RD11),
  str_len(RD, Len),
  L1 = Len - 5,
  frontstr(L1, RD11, _, RD4), !.
formatrating(RD, RD4) :-
  str_len(RD, Len),
  Len > 4,
  concat("00", RD, RD11),
  str_len(RD, Len),
  L1 = Len - 5,
  frontstr(L1, RD11, _, RD4), !.
formatrating(RD, RD).

opgListBase(Win, Cat, SorteerVeld, Volgorde) :-
  opg_t(Win,No,Cat,Tgnst,Plts,_,_,_,_,_,_,_),
  opgListBase1(No, Cat, Tgnst, Plts, SorteerVeld, Volgorde).

noodRating(Rating, _Speelsterkte, Rating1) :-
  fronttoken(Rating, _, _),
  trim_c(Rating),
  frontstr(6,Rating,Rating1,_), !.
noodRating(_, Speelsterkte, NoodRating) :-
  fronttoken(Speelsterkte,_,_), !,
  concat(Speelsterkte, ",0011", NoodRating). 
noodrating(_, _, "9,9999"). 

/*
opgListBase1(_No, _Cat, X, _Plts, _SorteerVeld, _A) :-
  write("\nopglistbase ", X, " ",idcp_rating),
  fail.
*/
opgListBase1(No, Cat, s(SpNo), Plts, SorteerVeld, idcp_plaatsing1) :-
  sp2(SpNo,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !,
  get_st(s(SpNo),Sterk,_,_,_,b_False),
  str_int_special(PStr, Plts),
  format(SorteerVeld, "%2.2s%-3.3s%s\t%u\t%U", PStr, Sterk, Naam, Cat, No).
opgListBase1(No, Cat, pw(SpNo), Plts, SorteerVeld, idcp_plaatsing1) :-
  sp2(SpNo,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !,
  get_st(s(SpNo),Sterk,_,_,_,b_False),
  str_int_special(PStr, Plts),
  format(SorteerVeld, "%2.2s%-3.3s%s\t%u\t%U", PStr, Sterk, Naam, Cat, No).
opgListBase1(No, Cat, p(SpNo,Sp1), Plts, SorteerVeld, idcp_plaatsing1) :-
  sp2(SpNo,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !,
  get_st(p(SpNo,Sp1),Sterk,_,_,_,b_False),
  str_int_special(PStr, Plts),
  format(SorteerVeld, "%2.2s%-3.3s%s\t%u\t%U", PStr, Sterk, Naam, Cat, No).
opgListBase1(No, Cat, _, _, SorteerVeld, idcp_datum) :-
  %opg_t(Win,No,Cat,_,_Plts,_Ronde,_Uitsl,_,_,_,_,_),
  str_int(Tijd, No), !,					% 21.7.2002
  format(SorteerVeld, "%s\t%u\t%U", Tijd, Cat, No).
opgListBase1(No, Cat, s(SpNo), _, SorteerVeld, idcp_alfabetisch) :-
  %opg_t(Win,No,Cat,s(SpNo),_Plts,_Ronde,_Uitsl,_,_,_,_,_),
  sp2(SpNo,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !,
  format(SorteerVeld, "%s\t%u\t%U", Naam, Cat, No).
opgListBase1(No, Cat, pw(SpNo), _, SorteerVeld, idcp_alfabetisch) :-
  %opg_t(Win,No,Cat,pw(SpNo),_Plts,_Ronde,_Uitsl,_,_,_,_,_),
  sp2(SpNo,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !,
  format(SorteerVeld, "%s\t%u\t%U", Naam, Cat, No).
opgListBase1(No, Cat, p(SpNo,_), _, SorteerVeld, idcp_alfabetisch) :-
  %opg_t(Win,No,Cat,p(SpNo,_),_Plts,_Ronde,_Uitsl,_,_,_,_,_),
  sp2(SpNo,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !,
  format(SorteerVeld, "%s\t%u\t%U", Naam, Cat, No).
opgListBase1(No, Cat, s(SpNo), _, SorteerVeld, idcp_speelsterkte) :-
  %opg_t(Win,No,Cat,s(SpNo),_Plts,_Ronde,_Uitsl,_,_,_,_,_),
  sp2(SpNo,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !,
  get_st(s(SpNo),Sterk,_,_,_,b_False),
  format(SorteerVeld, "%2.2s%-3.3s%s\t%u\t%U", "", Sterk, Naam, Cat, No).
opgListBase1(No, Cat, pw(SpNo), _, SorteerVeld, idcp_speelsterkte) :-
  %opg_t(Win,No,Cat,pw(SpNo),_Plts,_Ronde,_Uitsl,_,_,_,_,_),
  sp2(SpNo,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !,
  get_st(s(SpNo),Sterk,_,_,_,b_False),
  format(SorteerVeld, "%2.2s%-3.3s%s\t%u\t%U", "", Sterk, Naam, Cat, No).
opgListBase1(No, Cat, p(SpNo,Sp1), _, SorteerVeld, idcp_speelsterkte) :-
  %opg_t(Win,No,Cat,p(SpNo,Sp1),_Plts,_Ronde,_Uitsl,_,_,_,_,_),
  sp2(SpNo,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !,
  get_st(p(SpNo,Sp1),Sterk,_,_,_,b_False),
  format(SorteerVeld, "%2.2s%-3.3s%s\t%u\t%U", "", Sterk, Naam, Cat, No).
opgListBase1(No, Cat, s(SpNo), _, SorteerVeld, idcp_rating) :-
  sp2(SpNo,_,Naam,_,_,RE1,_RD,_,_, _, _, _, _, ES1, _SPSt, _, _, _, _, _,_,_RangPosE1,_RangPosD1,_,_CheckGeg1,_Log1,_Res1), !,
  noodRating(RE1,ES1,RE),
  formatRating(RE, RE4),
  format(SorteerVeld, "%-9s%s\t%u\t%U", RE4, Naam, Cat, No).
opgListBase1(No, Cat, pw(SpNo), _, SorteerVeld, idcp_rating) :-
  sp2(SpNo,_,Naam,_,_,RE1,_RD,_,_, _, _, _, _, ES1, _SPSt, _, _, _, _, _,_,_RangPosE1,_RangPosD1,_,_CheckGeg1,_Log1,_Res1), !,
  noodRating(RE1,ES1,RE),
  formatRating(RE, RE4),
  format(SorteerVeld, "%-8s%s\t%u\t%U", RE4, Naam, Cat, No).
opgListBase1(No, Cat, p(SpNo,Sp1), _, SorteerVeld, idcp_rating) :-
  sp2(SpNo,_,Naam,_,_,_RE1,RD1,_,_, _, _, _, _, _ES1, DS1, _, _, _, _, _,_,_RangPosE1,_RangPosD1,_,_CheckGeg1,_Log1,_Res1), 
  sp2(Sp1,_,_N,_,_,_RE21,RD2,_,_, _, _, _, _, _ES2, DS2, _, _, _, _, _,_,_RangPosE2,_RangPosD2,_,_CheckGeg2,_Log2,_Res2), !,
  noodRating(RD1, DS1, RD11),
  noodRating(RD2, DS2, RD21),
  str_real_Rating(RD11, R1),
  str_real_Rating(RD21, R2),
  dubbelRating(R1, R2, RD, _, _, _),
  formatrating(RD, RD4),
  format(SorteerVeld, "%-8s%s\t%u\t%U", RD4, Naam, Cat, No).
opgListBase1(No, Cat, p(SpNo,Sp1), _, SorteerVeld, idcp_rangorde) :-
  %opg_t(Win,No,Cat,p(SpNo,Sp1),_Plts,_Ronde,_Uitsl,_,_,_,_,_),
  sp2(SpNo,_,_Naam,_,_,_RE1,_RD1,_,_, _, _, _, _, _ES1, _SPSt, GebD1, _, _, _, _,_,_RangPosE1,RPD1,_,_CheckGeg1,_Log1,_Res1), 
  sp2(Sp1,_,_N,_,_,_RE21,_RD2,_,_, _, _, _, _, _ES2, _SPSt2, GebD2, _, _, _, _,_,_RangPosE2,RPD2,_,_CheckGeg2,_Log2,_Res2), !,
  ranglijstGeldt(Cat, GebD1, RPD1, RPD1g),
  ranglijstGeldt(Cat, GebD2, RPD2, RPD2g),
  reprrangDubbel(RPD1g, RPD2g, _Rang, _RangDelta, RangSort),
  format(SorteerVeld, "%4.4s%4s%6s\t%u\t%U", "", RangSort, "", Cat, No).
opgListBase1(No, Cat, s(SpNo), _, SorteerVeld, idcp_rangorde) :-
  %opg_t(Win,No,Cat,s(SpNo),_Plts,_Ronde,_Uitsl,_,_,_,_,_),
  sp2(SpNo,_,_Naam,_,_,_RatE1,_RD1,_,_, _, _, _, _, _ES1, _SPSt, Gebdat, _, _, _, _,_,RE1,_RPD1,_,_CheckGeg1,_Log1,_Res1), !,
  ranglijstGeldt(Cat, Gebdat, RE1, RE1g),
  blancoAchteraan(RE1g, RE1a),
  format(SorteerVeld, "%4.4s%4s%6s\t%u\t%U", "", RE1a, "", Cat, No).
opgListBase1(No, Cat, pw(SpNo), _, SorteerVeld, idcp_rangorde) :-
  %opg_t(Win,No,Cat,pw(SpNo),_Plts,_Ronde,_Uitsl,_,_,_,_,_),
  sp2(SpNo,_,_Naam,_,_,_RatE1,_RD1,_,_, _, _, _, _, _ES1, _SPSt, Gebdat, _, _, _, _,_,RE1,_RPD1,_,_CheckGeg1,_Log1,_Res1), !,
  ranglijstGeldt(Cat, Gebdat, RE1, RE1g),
  blancoAchteraan(RE1g, RE1a),
  format(SorteerVeld, "%4.4s%4s%6s\t%u\t%U", "", RE1a, "", Cat, No).

ranglijstGeldt(Cat, Gebdat, _In, "") :-
  leeftijd(Gebdat, Leeft, 0, _),
  wc(Cat, _, _, _, _, _, _, _, _LftV, leeftijd(_, LeeftTM), _, _, _, _, _),
  LeeftTM < 17,
  LeeftTM > 12,				% kleiner dus niet
  Leeft <= LeeftTM - 2, !.		% uit jongere klasse (jeugd)
ranglijstGeldt(Cat, Gebdat, _In, "") :-
  leeftijd(Gebdat, Leeft, Cat, _),
  wc(Cat, _, _, _, _, _, _, _, leeftijd(_, LeefTV), _, _, _, _, _, _),
  LeeftV > 30,
  Leeft >= LeeftV + 5, !.		% uit oudere klasse (veteranen)
ranglijstGeldt(_Cat, _Gebdat, In, In).

blancoAchteraan(In, In) :-
  fronttoken(In, _, _), !.
blancoAchteraan(_, "9999").

str_real_Rating(Rating, Real) :-
  searchchar(Rating, ',', _),
  fronttoken(Rating, Cijfer, R1),
  fronttoken(R1, _, R2),
  format(RealS, "%s.%s", Cijfer, R2),
  str_real(RealS, Real), !.
str_real_Rating(Rating, Real) :-
  str_real(Rating, Real), !.
str_real_Rating(_Rating, 0.0).

dubbelRating(0.0, 0.0, "", "", color_black, 0) :- !.
dubbelRating(R1, 0.0, RUit, "-", color_black, 0) :- 
  format(RUit, "%-6.4f", R1), !.
dubbelRating(0.0, R1, RUit, "-", color_black, 0) :- 
  format(RUit, "%-6.4f", R1), !.
dubbelRating(R1, R2, RUit, RDelta, Color, DR) :- 
  %R1 < R2,
  R = (R1 + R2) / 2,
  format(RUit, "%-6.4f", R), 
  D = abs(R1 - R2),
  ratingDeltaColor(D, Color),
  format(RDelta, "%-3.1f", D),
  DR = val(integer, D * 10000),   !.
/*dubbelRating(R1, R2, RUit, RDelta, Color, DR) :- 
  R = (R1 + R2 * ratingfact()) / (ratingfact() + 1),
  format(RUit, "%-6.4f", R), 
  D = abs(R1 - R2),
  ratingDeltaColor(D, Color),
  format(RDelta, "%-3.1f", D),
  DR = val(integer, D * 10000),   !.*/

ratingDeltaColor(D, color_black) :-
  D <= 2, !.
ratingDeltaColor(_, color_red). 

/*ratingfact(2) :-  	% 13.3.2012
  naam(_, _, _, _, Distr, _),
  Distr = "RDM", !.
ratingfact(1) */

opg_repr(Win, StrIn, No, Cat, Tijd, LL, Plaats, Sterkte, Rating, "", color_black, UStr, Naam, "", KeurS, KeurKleur,RangordeU, "", VoorgrKleur) :-
  parseTabs(StrIn, SList),
  SList = [_, CatNrS, NrS | _],
  str_int(CatNrS, Cat),
  str_int(NrS, No),		% (long)
  opg_t(Win,No,Cat,s(SpNo),Plts,_Ronde,Uitsl,_,LL,_,Keur,_),
  dt_minoffset_to_str(No, "%DD/%MD %HH:%MM", Tijd),
  sp2(SpNo,_,Naam,_,_ ,Rating,_,_,_,_,_, _,_,_,_,Gebdat, _,_,_,_,_,Rangorde,_,_,_,_,_),
  ranglijstGeldt(Cat, Gebdat, Rangorde, RangordeG),
  str_int_rang(RangordeG, Rang),
  reprrang(Rang, RangordeU), 
  get_st(s(SpNo),Sterkte,_,_,_,b_False),
  str_int_uitsl(UStr, Uitsl, VoorgrKleur), 
  str_int_plaats(Plaats, Plts),
  keur2Tekst(Cat,[SpNo],Keur, KeurS, _, KeurKleur),  !.
opg_repr(Win, StrIn, No, Cat, Tijd, LL, Plaats, Sterkte, Rating, RatingDelta, RatingColor, UStr, Naam, "ZOEKT PARTNER", KeurS, KeurKleur, Rang, RangDelta, VoorgrKleur) :-
  parseTabs(StrIn, SList),
  SList = [_, CatNrS, NrS | _],
  str_int(CatNrS, Cat),
  str_int(NrS, No),		% (long)
  opg_t(Win,No,Cat,pw(SpNo),Plts,_Ronde,Uitsl,_,LL,_,Keur,_),
  dt_minoffset_to_str(No, "%DD/%MD %HH:%MM", Tijd),
  sp2(SpNo,_,Naam,_,_ ,_,Rating1,_,_,_,_, _,_,_,_,Geb1, _,_,_,_,_,_,Rangorde1S,_,_,_,_),
  str_Real_Rating(Rating1, RR1),
  str_Real_Rating("", RR2),
  dubbelRating(RR1, RR2, Rating, RatingDelta, RatingColor, _),
  get_st(pw(SpNo),Sterkte,_,_,_,b_False),
  str_int_uitsl(UStr, Uitsl, VoorgrKleur),
  str_int_plaats(Plaats, Plts),
  ranglijstGeldt(Cat, Geb1, Rangorde1S, Rangorde1G),
  reprrangDubbel(RangOrde1G, "", Rang, RangDelta, _RangSort),
  keur2Tekst(Cat,[SpNo],Keur, KeurS, _, KeurKleur),  !.
opg_repr(Win, StrIn, No, Cat, Tijd, LL, Plaats, Sterkte, Rating, RatingDelta, RatingColor, UStr, Naam, Naam1, KeurS, KeurKleur, Rang, RangDelta, VoorgrKleur) :-
  parseTabs(StrIn, SList),
  SList = [_, CatNrS, NrS | _],
  str_int(CatNrS, Cat),
  str_int(NrS, No),		% (long)
  opg_t(Win,No,Cat,p(SpNo,Sp1),Plts,_Ronde,Uitsl,_,LL,_,Keur,_),
  dt_minoffset_to_str(No, "%DD/%MD %HH:%MM", Tijd),
  sp2(SpNo,_,Naam,_,_ ,_,Rating1,_,_,_,_, _,_,_,_,Geb1, _,_,_,_,_,_,Rangorde1S,_,_,_,_),
  sp2(Sp1,_,Naam1,_,_ ,_,Rating2,_,_,_,_, _,_,_,_,Geb2, _,_,_,_,_,_,Rangorde2S,_,_,_,_),
  str_Real_Rating(Rating1, RR1),
  str_Real_Rating(Rating2, RR2),
  dubbelRating(RR1, RR2, Rating, RatingDelta, RatingColor, _),
  get_st(p(SpNo,Sp1),Sterkte,_,_,_,b_False),
  str_int_uitsl(UStr, Uitsl, VoorgrKleur),
  str_int_plaats(Plaats, Plts),
  ranglijstGeldt(Cat, Geb1, Rangorde1S, Rangorde1G),
  ranglijstGeldt(Cat, Geb2, Rangorde2S, Rangorde2G),
  reprrangDubbel(RangOrde1G, Rangorde2G, Rang, RangDelta, _RangSort),
  keur2Tekst(Cat,[SpNo,Sp1],Keur, KeurS, _, KeurKleur),  !.

reprrangDubbel(In1, In2, Rang, RangDelta, Rang) :-
  str_int(In1, In1I),
  str_int(In2, In2I),
  RangDeltaI = abs(In1I - In2I),
  str_intP(RangDelta, RangDeltaI),
  RangI = In1I + In2I,
  str_intP(Rang, RangI), !.  
reprrangDubbel(_In1, _In2, "-", "-", "999999").

reprrang(In, "-") :-
  In >= 9999, !.
reprrang(0, "-") :- !.
reprrang(In, Uit) :-
  str_int(Uit, In).

str_int_rang(Str, Rang) :-
  str_int(Str, Rang), !.
str_int_rang(_, 9999) :- !.

str_int_uitsl(" ", 0, color_black) :- !.
str_int_uitsl("u", _, color_blue).

str_int_ronde("  ", 0) :- !.
str_int_ronde("2e", 1) :- !.
str_int_ronde("3e", 2) :- !.
str_int_ronde("??", _).

str_int_plaats(Str, P1) :-
  P1 > 0,
  str_int(Str, P1), !.
str_int_plaats("", _) :- !.

str_int_special(XS, X) :-
  X > 0, 
  str_int(XS, X), !.
str_int_special("99", _) :- !.

typeMatch(d,g) :- !.
typematch(g,d) :-  !.
typematch(X,X).

passende_categorie(Cat, CatS1, Personen, SpCatL) :-
  wc(Cat, _, _, _, Typ, _, _, _, _, _, _, _, _, _, _), 
  %wc(CatNum,CatS,Typ1, _,SpCatL1,_,_),
  wc(CatNum, _, _, CatS, Typ1, _, SpCatL1, _, _, _, _, _, _, _, _),
  typematch(Typ1,Typ),
  CatNum <> Cat,
  check_sex(SpCatL, SpCatL1),
  check_spelers(CatNum, Personen, Merk),
  concat(Merk, CatS, CatS1).

check_sex(SpCat, SpCatL) :-			% moeten elkaar volledig dekken!
  member(Cat, SpCat),
  not(member(Cat, SpCatL)), !, fail.
check_sex(_, _).	
check_spelers(Cat, Personen, "~") :-
  member(Pers, Personen),
  sp2opg(Pers, Cat, _, _,_,_), !.
check_spelers(Cat, Personen, "~") :-		% niet twee keer kopiëren
  member(Pers, Personen),
  sp2opgt(Pers, Cat), !.
check_spelers(_, _, "").

sp2opgt(SpNo,Cat) :-      			% al bij een t-opg?
  opg_t(_,_Opg1,Cat,s(SpNo),_,_,_,_,_,_,_,_), !.
sp2opgt(SpNo,Cat) :-      			% al bij een t-opg?
  opg_t(_,_Opg1,Cat,pw(SpNo),_,_,_,_,_,_,_,_), !.
sp2opgt(SpNo,Cat) :-
  opg_t(_,_Opg1,Cat,p(SpNo,_),_,_,_,_,_,_,_,_), !.
sp2opgt(SpNo,Cat) :-
  opg_t(_,_Opg1,Cat,p(_,SpNo),_,_,_,_,_,_,_,_), !.

in_wedstrijd(Cat,Tgnst):- 
  wd(_,Cat,Tgnst,_,_,_,_), !.
in_wedstrijd(Cat,Tgnst):- 
  wd(_,Cat,_,Tgnst,_,_,_), !.

personen(s(X), [X], [C]) :- 
  sp2(X,C,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !.
personen(pw(X), [X], [C]) :- 
  sp2(X,C,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !.
personen(p(X1,X2), [X1, X2], [C1, C2]) :-
  sp2(X1,C1,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  sp2(X2,C2,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !.

gep(Win, Cat, Plaats) :-
  opg_t(Win, _Opg,Cat,_Tgnst,Plaats,_Ronde,_Uitsl,_Tijd,_LL,_,_,_),
  Plaats <> 0.

saveOpg(Win) :-
  winCat(Win, Cat),
  findall(Plaats, gep(Win, Cat, Plaats), PlList),
  sortint_c(PlList),
  reverse(PlList, [], PlListRev),
  PlListRev = [PlMax | _], 
  checkPlaatsing1(PlList, 1, PlMax, Missende),
  signaleerMissende(Missende),
  fail.
saveOpg(Win) :-
  loglock(mdi, "saveOpg"),
  winCat(Win, Cat),
  opg(Opg,Cat,Tgnst,Pl,Ronde,Uitsl,Tijd,LL,TID,Stat,MT),			% komt niet meer voor
  not(opg_t(Win, Opg,Cat,_,_,_,_,_,_,_,_,_)),				% (verhuisd)
  logf('R', opg(Opg,Cat,Tgnst,Pl,Ronde,Uitsl,Tijd,LL,TID,Stat,MT),ja),	% NB mag niet 'omheteven' worden
  fail.
saveOpg(Win) :-
  winCat(Win, Cat),
  voorNa(Win, Cat,ErNa,_Eigencount,_NaCount),
  opg(Opg,Erna,Tgnst,Pl,Ronde,Uitsl,Tijd,LL,TID,Stat,MT),
  not(opg_t(Win, Erna,Cat,_,_,_,_,_,_,_,_,_)),
  logf('R', opg(Opg,Erna,Tgnst,Pl,Ronde,Uitsl,Tijd,LL,TID,Stat,MT),ja),
  fail.
saveOpg(Win) :-
  opg(Opg,Cat,Tgnst,Plaats,Ronde,Uitsl,Tijd,LL,TID,Stat,MT),
  retract(opg_t(Win, Opg,Cat,Tgnst,Plaats,Ronde,Uitsl,Tijd,LL,TID,Stat,MT)),	% gelijk gebleven
  fail.
saveOpg(Win) :-
  assert(plaatsingVeranderd(b_False)),				% vlag
  opg_t(Win, Opg,Cat,_,Plaats,Ronde,Uitsl,Tijd,LL,TID,Stat,MT),		% veranderd	
  getbacktrack(Here),
  opg(Opg,Cat,Tgnst,PlaatsVorig,_,_,_,_,_,_,_),
  cutbacktrack(Here),
  logf('Z',opg(Opg,Cat,Tgnst,Plaats,Ronde,Uitsl,Tijd,LL,TID,Stat,MT), ja),
  retract(opg_t(Win, Opg,Cat,_,_,_,_,_,_,_,_,_)),
  PlaatsVorig <> Plaats,
  assert(plaatsingVeranderd(b_True)),
  fail.
saveOpg(Win) :-
  retract(opg_t(Win, Opg,Cat,Tgnst,Plaats,Ronde,Uitsl,Tijd,_,TID,Stat,MT)),	% nieuw (verhuisd)
  logf('Z',opg(Opg,Cat,Tgnst,Plaats,Ronde,Uitsl,Tijd,"",TID,Stat,MT), ja),
  fail.
saveOpg(_Win) :-
  logclose(),
  plaatsingVeranderd(b_True),
  update_display(),
  fail.
saveOpg(_Win).

compOpg(Win) :-
  winCat(Win, Cat),
  %voorNa(Win, Cat, ErNa, NaCount),
  opg(Opg,Cat,Tgnst,Pl,Ronde,Uitsl,Tijd,LL,_TID,_Stat,_MT),			% komt niet meer voor
  not(opg_t(Win, Opg,Cat,Tgnst,Pl,Ronde,Uitsl,Tijd,LL,_,_,_)),	% (verhuisd)
  !.								% ongelijk
compOpg(Win) :-
  %winCat(Win, Cat),
  opg_t(Win, Opg,Cat,Tgnst,Pl,Ronde,Uitsl,Tijd,LL,_,_,_),		% 
  not(opg(Opg,Cat,Tgnst,Pl,Ronde,Uitsl,Tijd,LL,_,_,_)),		% veranderd of nieuw
  !.								% ongelijk
/*
compOpg(Win) :-
  winCat(Win, Cat),
  opg_t(Win, _Opg,Cat1,_Tgnst,_Pl,_Ronde,_Uitsl,_Tijd,_LL,_,_,_),	% 
  Cat1 <> Cat,							% nieuwe categorie
  !.*/								% kopie of verhuisd

checkDeelInButtons(_Win) :-   % de rating knopjes
  winCat(_Win, Cat),
  voorNa(_Win, Cat,_ErNa,EigenCount, NaCount),
  FWin = win_GetCtlHandle(_Win, idcp_rating_up),
  win_BringToTop(FWin),
  GWin = win_GetCtlHandle(_Win, idcp_rating_dn),
  win_BringToTop(GWin),
  count2enabled(Eigencount,EnDis),
  win_SetState(FWin,[EnDis]),
  count2enabled(Nacount, EnDis1),
  win_SetState(FWin,[EnDis1]),
  fail.
checkDeelInButtons(Win) :-
  win_PostEvent(Win,e_User(teplaatsen, 0)),   % dit gaat om het aantal te plaatsen spelers
  dialog_SetState(Win, [enable(idcp_pbverhuis,b_False),enable(idcp_pbdoornaar,b_False),
  	enable(idcp_selecteer,b_False), enable(idcp_wis, b_False)]),
  LboxWin = win_GetCtlHandle(Win, idcp_plaatsing),
  win_SetFocus(LboxWin),
  Index = lbox_GetSelIndex(LboxWin),
  dialog_SetState(Win, [enable(idcp_pbdoornaar,b_True),enable(idcp_selecteer,b_True)]),	% er is er een geselecteerd
  Item = lbox_GetItem(LboxWin, Index),
  opg_repr(Win, Item, Num, Cat, _Tijd, _LL, _Plts, _Sterkte, _RStr, _RD, _RDColor, _Uitsl, _Naam, _Naam1, _, _, _, _, _),
  opg_t(Win,Num,Cat,Tgnst,_Plaats1,_Ronde1,_Reserve1,_Tijd1,_,_,_,_),
  not(in_wedstrijd(Cat, Tgnst)),			% niet opgesteld in schema dus mag verwijderd
  dialog_SetState(Win, [enable(idcp_pbverhuis,b_True), enable(idcp_wis,b_True)]),
  win_SetFocus(LboxWin),
  !.
checkDeelInButtons(_Win).

zoekIndex(NoI, CatI, [H|_], In, In) :-
  searchchar(H, '\t', P0),
  frontstr(P0,H,_,R0),
  fronttoken(R0,CatNrS,R1),
  str_int(CatNrS, Cat),
  Cat = CatI,
  fronttoken(R1,NrS,_),
  str_int(NrS, No),		% (long) gevonden
  No = NoI, !.
zoekIndex(NoI, CatI, [_|T], In, Uit) :-
  In1 = In + 1,
  zoekIndex(NoI, CatI, T, In1, Uit), !.
zoekIndex(_NoI, _CatI, _, _, 0).

%BEGIN_DLG Plaatsing
/**************************************************************************
	Creation and event handling for dialog: Plaatsing
**************************************************************************/

constants

%BEGIN Plaatsing, CreateParms, 09:58:13-28.12.2013, Code automatically updated!
  dlg_plaatsing_ResID = idd_plaatsing
  dlg_plaatsing_DlgType = wd_Modal
  dlg_plaatsing_Help = idh_plaatsing
%END Plaatsing, CreateParms

predicates

  dlg_plaatsing_eh : EHANDLER
  dlg_plaatsing_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_plaatsing_update(DIALOG_VAL_LIST)
procedure  setSorteerVeld(WINDOW, wedscat)
  myOwnerdraw2(window Parent, window LboxWin, integer ItemId, RCT RectItem, COLOR Voorgrond, COLOR Achtergrond, boolean Selected)
procedure checkPartnerButton(window , wedstyp)
procedure metDelta(window, integer Factor)
procedure addRangorde(integer, window, integer L2, integer WRAngorde, integer WRangordeDelta, integer T, integer B)

procedure metRangorde(window Win, boolean)
procedure maxWCveld(window, integer Max)
procedure addRangOrde(window, integer Top, integer Bottom, integer XIn, integer WRangOrde1,
          integer WRangorde2, string Rangorde, string Rangorde2, integer XUit)
volgordeOpg(window Win, integer Selectie)
nondeterm uitgesloteT(window Win, integer Num)
  uitgeslotenT(window Win, integer Aantal)
  geplaatstenT(window Win, integer Aantal)
nondeterm geplaatsteT(window Win, integer Num)
ifselected(boolean, color, color, color)
noHilite(color, color, color)
determ verplaatsingMogelijk(wedscat, tgnst)

clauses

sorteerVeld(idcp_rating).

setSorteerVeld(_Win, Cat) :-
  wc(Cat, _, _, _, _, _CStr, _, _, _, _, _, SSrt, _, _, _),
  not(SSrt = ""),
  assert(sorteerVeld(idcp_rating)),
  fail.
setSorteerVeld(Win, _Cat) :-
  sorteerVeld(A_P),
  RB1Win = win_GetCtlHandle(Win, idcp_alfabetisch),
  RB2Win = win_GetCtlHandle(Win, idcp_speelsterkte),
  RB3Win = win_GetCtlHandle(Win, idcp_rating),
  RB4Win = win_GetCtlHandle(Win, idcp_datum),
  RB5Win = win_GetCtlHandle(Win, idcp_plaatsing1),
  RB6Win = win_GetCtlHandle(Win, idcp_rangorde),
  CheckWin = win_GetCtlHandle(Win, A_P),
  win_CheckRadioButton(CheckWin, [RB1Win, RB2Win, RB3Win,RB4Win, RB5Win,RB6Win]), !.

addRangorde(_LboxWin, _T, _B, L2, 0, 0, _RangOrde, _RangDelta, L2) :- !.	% geen ranglijst
addRangorde(LboxWin, T, B, L2, Wrangorde, 0, RangOrde, _RangDelta, L3) :-
  L3 = L2 + Wrangorde,					% ranglijst enkel
  Rct2a = rct(L2, T, L3, B),
  draw_TextInRect(LboxWin, Rct2a, Rangorde, -1, [dtext_right]),  !.	% 11.5.2004
addRangorde(LboxWin, T, B, L1, Wrangorde, WRangordeDelta, RangOrde, RangDelta, L4) :-
  L2 = L1 + Wrangorde - 5,					% ranglijst dubbel
  Rct2 = rct(L1, T, L2, B),
  draw_TextInRect(LboxWin, Rct2, Rangorde, -1, [dtext_right]),
  L3 = L2 + WrangordeDelta - 7,
  Rct3 = rct(L2, T, L3, B),
  draw_TextInRect(LboxWin, Rct3, RangDelta, -1, [dtext_right]), 
  L4 = L3 + 12, !.

ifSelected(b_True, _, X, X) :- !.
ifSelected(_, X, _, X).

noHilite(color_black, Voorgrond, Voorgrond) :- !.
noHilite(Color, _, Color).

myOwnerdraw2(Win, LboxWin, ItemId, RectItem, Voorgrond, Achtergrond, Selected) :-
  win_SetPen(LboxWin,pen(1,ps_Solid,Achtergrond)),
  draw_Rect(LboxWin, RectItem),
  iafm(Wstatus, WDatum, WCVrij, WPlts, Wstrk, WRating, WRatingDelta, Wuitsl, Wrangorde, WrangordeDelta),
  Item = lbox_GetItem(LboxWin, ItemId),	% get the string
  RectItem = rct(_L, T, R, B0),
  B = B0 + 1,
  opg_repr(Win, Item, _No, _Cat, Tijd, LL, Plts, Sterkte, Rating, RDelta, RDColor, Uitsl, Naam, Naam1, KeurS, KeurKl, Rangorde, RangDelta, VoorgrKleur),
  win_SetForeColor(LboxWin, KeurKl),
  win_SetBackColor(LboxWin, color_white),
  Rct0 = rct(2, T, Wstatus, B),
  win_SetBrush(LboxWin,brush(pat_Solid, color_White)),
  win_SetPen(LboxWin,pen(1,ps_Solid,Achtergrond)),
  draw_Rect(LboxWin, rct(0,T,Wstatus,B0)),
  Font=win_GetFont(Win),
  NewFont = font_SetAttrs(Font,[fs_Bold],0),
  win_setFont(Win, NewFont),
  draw_TextInRect(LboxWin,Rct0, KeurS, -1, [dtext_left,dtext_Vcenter]),
  win_SetBrush(LboxWin,brush(pat_Hollow, color_White)),
  win_SetPen(LboxWin,pen(1,ps_Solid,Achtergrond)),
  draw_Rect(LboxWin, rct(0,T,Wstatus,B0)),
  ifSelected(Selected, VoorgrKleur, Voorgrond, VoorgrondX),
  win_SetForeColor(LboxWin, VoorgrondX),
  win_SetBackColor(LboxWin, Achtergrond),
  L1 = Wstatus + WDatum,
  Rct1 = rct(Wstatus, T, L1, B),
  draw_TextInRect(LboxWin,Rct1, Tijd, -1, [dtext_left]),
  %
  L1a = L1 + WCVrij,
  Rct2L = rct(L1, T, L1a, B),
  draw_TextInRect(LboxWin,Rct2L, LL, -1, [dtext_center]),
  %
  L2 = L1a + Wplts,
  Rct2 = rct(L1a, T, L2, B),
  draw_TextInRect(LboxWin,Rct2, Plts, -1, [dtext_center]),
  %
  addRangorde(LboxWin, T, B, L2, Wrangorde, WRangordeDelta, RangOrde, RangDelta, L3a),
  %
  L3 = L3a + Wstrk - 5,
  Rct3 = rct(L3a, T, L3, B),
  draw_TextInRect(LboxWin,Rct3, Sterkte, -1, [dtext_center]),
  %
  L4 = L3 + Wrating,
  Rct4 = rct(L3, T, L4, B),
  draw_TextInRect(LboxWin,Rct4, Rating, -1, [dtext_right]),	%---
  %
  L4a = L4 + WRatingDelta,
  Rct4a = rct(L4, T, L4a, B), 
  noHilite(RDColor, Voorgrond, RDColor1),
  win_SetForeColor(LboxWin, RDColor1),
  draw_TextInRect(LboxWin,Rct4a, RDelta, -1, [dtext_right]),	%---
  %
  L5 = L4a + Wuitsl + 5,
  Rct5 = rct(L4a, T, L5, B),
  %concat(Uitroep, Uitsl, Uitsl1),
  win_SetForeColor(LboxWin, color_red),
  win_SetBackColor(LboxWin, color_white),
  draw_TextInRect(LboxWin,Rct5, Uitsl, -1, [dtext_center]),
  L6 = (R + L5) div 2,
  Rct6 = rct(L5, T, L6, B),
  win_SetForeColor(LboxWin, VoorgrondX),
  win_SetBackColor(LboxWin, Achtergrond),
  draw_TextInRect(LboxWin,Rct6, Naam, -1, [dtext_left]),
  Rct7 = rct(L6, T, R,  B),
  draw_TextInRect(LboxWin,Rct7, Naam1, -1, [dtext_left]), !.

checkPartnerButton(_Win, e) :- 
  PartnerWin = win_GetCtlHandle(_Win, idcp_partner),
	win_SetState(PartnerWin, [wsf_Disabled]), !.
checkPartnerButton(_Win, _).

maxWCveld(Win, _) :-
  assert(maxAfmeting(0)),
  opg_t(Win, _,_,_,_,_,_,_,WC, _, _, _),
  fronttoken(WC, _, _),
  win_GetTextExtent(Win, WC, -1, Width, _Height),
  maxAfmeting(CurMaxL),
  Width > CurMaxL,
  assert(maxAfmeting(Width)),
  fail.
maxWCveld(_Win, MaxWC) :-
  maxAfmeting(MaxWC), !.

metRangorde(_Win, b_True) :-
  winCat(_, Cat),
  opg(_Opg,Cat,p(SpNo,_Sp1),_,_,_,_,_,_,_,_),
  sp2(SpNo,_,_Naam,_,_,_RE1,_RD1,_,_, _, _, _, _, _ES1, _SPSt, _, _, _, _, _,_,_RangPosE1,RangPosD1,_,_CheckGeg1,_Log1,_Res1), 
  fronttoken(RangPosD1, _, _), !.
metRangorde(_Win, b_True) :-
  winCat(_, Cat),
  opg(_Opg,Cat,p(_SpNo,Sp1),_,_,_,_,_,_,_,_),
  sp2(Sp1,_,_Naam,_,_,_RE1,_RD1,_,_, _, _, _, _, _ES1, _SPSt, _, _, _, _, _,_,_RangPosE1,RangPosD1,_,_CheckGeg1,_Log1,_Res1), 
  fronttoken(RangPosD1, _, _), !.
metRangorde(_Win, b_True) :-
  winCat(_, Cat),
  opg(_Opg,Cat,s(SpNo),_,_,_,_,_,_,_,_),
  sp2(SpNo,_,_Naam,_,_,_RE1,_RD1,_,_, _, _, _, _, _ES1, _SPSt, _, _, _, _, _,_,RangPosE1,_RangPosD1,_,_CheckGeg1,_Log1,_Res1), 
  fronttoken(RangPosE1, _, _), !.
metRangorde(_Win, b_False).

metDelta(_Win, 0) :-
  winCat(_, Cat),
  wc(Cat, _, _, _, e, _, _, _, _, _, _, _, _, _, _), !.
metDelta(_Win, 1).

addRangorde(1, Win, L2, WRAngorde, _WRangordeDelta, T, B) :-
  R = L2 + WRangorde,
  V1 = rct(L2, T, R, B),
  draw_TextInRect(Win, V1, "Rng", -1, [dtext_center]), !.
addRangorde(2, Win, L2, WRAngorde, WRangordeDelta, T, B) :-
  R = L2 + WRangorde,
  V1 = rct(L2, T, R, B),
  draw_TextInRect(Win, V1, "Som", -1, [dtext_right]),	% 11.5.2004
  L3 = R + WRangordeDelta,
  V2 = rct(R, T, L3, B),
  draw_TextInRect(Win, V2, "Delt", -1, [dtext_center]), !.
addRangorde(_, Win, L2, WRAngorde, WRangordeDelta, T, B) :-
  R = L2 + WRangorde + WRangordedelta,
  T1 = 2 * T - B,
  V1 = rct(L2, T1, R, T),
  draw_TextInRect(Win, V1, "Ranglijst", -1, [dtext_center]), !.

volgordeOpg(_Win, SorteerVolgorde) :-
  LboxWin = win_GetCtlHandle(_Win, idcp_plaatsing),
  Index = lbox_GetSelIndex(LboxWin),
  Item = lbox_GetItem(LboxWin, Index),
  opg_repr(_Win, Item, Num, Cat, _Tijd, _LL, _Plts, _Sterkte, _RStr, _RDelta, _RDColor, _Uitsl, _Naam, _Naam1, _, _, _, _, _),
  assert(sorteerVeld(Sorteervolgorde)),
  findall(Veld, opgListBase(_Win, Cat, Veld, Sorteervolgorde), OpgBaseL), 
  sortstring_c(OpgBaseL, 1),
  lbox_Clear(LboxWin),
  lbox_Add(LboxWin, OpgBaseL),
  zoekindex(Num, Cat, OpgBaseL, 0, NewIndex),
  lbox_SetSel(LboxWin, NewIndex, b_True),
  checkDeelInButtons(_Win), !.

geplaatstenT(_Win, Aantal) :-
  findall(Num, geplaatsteT(_Win, Num), NumL),
  count(NumL, 0, Aantal).

geplaatsteT(Win, Num) :-
  winCat(Win, Cat),
  opg_t(Win,Num,Cat,_Tgnst,Plaats,_Ronde,_Uitsl,_TijdKK,_LL,_Keur,_,_),
  Plaats <> 0.
  
uitgeslotenT(_Win, Aantal) :-
  findall(Num, uitgesloteT(_Win, Num), NumL),
  count(NumL, 0, Aantal).

uitgesloteT(Win, Num) :-
  winCat(Win, Cat),
  opg_t(Win,Num,Cat,_Tgnst,_Plaats,_Ronde,Uitsl,_TijdKK,_LL,_Keur,_,_),
  Uitsl <> 0.

dlg_plaatsing_refresh() :-
  winCat(Win, Cat),
  LboxWin = win_GetCtlHandle(Win, idcp_plaatsing),
  Index = lbox_GetSelIndex(LboxWin),
  Item = lbox_GetItem(LboxWin, Index),
  opg_repr(Win, Item, Num, _, _Tijd, _LL, _Plts, _Stxx, _RStrx, _RDelta,  _RDColor, _Uitslx, _Naam, _Naam1x, _, _, _, _, _),
  not(opg(Num,Cat,_,_,_,_,_,_,_,_,_)),
  retract(opg_t(Win,Num,Cat,_,_,_,_,_,_,_,_,_)),
  sorteerVeld(Sorteer),
  findall(Veld, opgListBase(_Win, Cat, Veld, Sorteer), OpgBaseL), 
  sortstring_c(OpgBaseL, 1),
  lbox_Clear(LboxWin),
  lbox_Add(LboxWin, OpgBaseL),
  lbox_SetSel(LboxWin, 0, b_True),
  checkDeelInButtons(Win),
  win_Invalidate(Win), !.
dlg_plaatsing_refresh().

erna(Cat, [Cat,Erna|_], Erna) :- !.
erna(Cat, [_|Rest], Erna) :-
  erna(Cat,Rest,Erna).

voorNa(Win, Cat,ErNa,Eigencount, NaCount) :-
  findall(Numx,opg_t(Win,Numx,Cat,_,_,_,_,_,_,_,_,_),NumXL),
  count(NumxL,0,Eigencount),
  kopl(Cat, algemeen, Master, algemeen),
  not(isNRT(Master)),  !,
  %findall(Peer, kopl(Peer, algemeen, Master, algemeen), Peers),
  findall(Peer, koplinVolgorde(Master,Peer), Peers),
  erna(Cat,Peers,Erna),
  findall(NumNa,opg_t(Win,NumNa,Erna,_,_,_,_,_,_,_,_,_),NumNaL),
  count(NumNaL,0,NACount).

count2Enabled(0, wsf_Disabled) :- !.
count2Enabled(_, wsf_enabled).

verplaatsingMogelijk(Cat, Tgnst) :-
  not(in_wedstrijd(Cat, Tgnst)), !.			% niet opgesteld in schema dus mag verwijderd
verplaatsingMogelijk(_Cat, _Tgnst) :-
  dlg_MessageBox( "Indelen", "verplaatsing niet mogelijk!\nPersonen staan al in schema opgesteld!", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.

  dlg_plaatsing_Create(_Parent, Cat, _Index):-
	opg(NumX,Cat,Tgnst,A,B,C,D,E,TID,Stat,MT),			% clean-up voor de zekerheid
	tgnstnum(Tgnst, TNum),
	not(sp2(TNum,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)),
	logf('R', opg(NumX,Cat,Tgnst,A,B,C,D,E,TID,Stat,MT),ja),
	fail.
  dlg_plaatsing_Create(Parent, Cat, Index):-
	Parm = cast(long, Cat),
	%write("\nParm: ", Parm),
	assert(eersteIndex(Index)),
	assert(winCat(~0, Cat)),

%MARK Plaatsing, new variables

	dialog_CreateModal(Parent,dlg_plaatsing_ResID,"",
  		[
%BEGIN Plaatsing, ControlList, 09:58:13-28.12.2013, Code automatically updated!
		df(idcp_plaatsing,listbox([],[0]),nopr)
%END Plaatsing, ControlList
		],
		dlg_plaatsing_eh,Parm,VALLIST,ANSWER),
	dlg_plaatsing_handle_answer(ANSWER,VALLIST).

  dlg_plaatsing_handle_answer(idc_ok,VALLIST):-!,
	dlg_plaatsing_update(VALLIST).
  dlg_plaatsing_handle_answer(idc_cancel,_):-!.  % Handle Esc and Cancel here
  dlg_plaatsing_handle_answer(_,_):-
	errorexit().

  dlg_plaatsing_update(_VALLIST):-
%BEGIN Plaatsing, Update controls, 09:58:13-28.12.2013, Code automatically updated!
	dialog_VLGetListBox(idcp_plaatsing,_VALLIST,_IDCP_PLAATSING_ITEMLIST,_IDCP_PLAATSING_SELECT),
%END Plaatsing, Update controls
	true.

%MARK Plaatsing, new events

%BEGIN Plaatsing, idcp_uitsluit _CtlInfo
  dlg_plaatsing_eh(_Win,e_Control(idcp_uitsluit,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idcp_plaatsing),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
        opg_repr(_Win, Item, Num, Cat, _, _, _, _Sterkte, _RStr, _, _, _, _, _, _, _, _, _, _),
	opg_t(_Win,Num,Cat,Tgnst,Plaats,Ronde,Uitsl,TijdKK,LL,Keur,TID,MT),
	bitxor(Uitsl,1,Uitsl1),  
	retract(opg_t(_Win,Num,Cat,_,_,_,_,_,_,_,_,_)),
	assert(opg_t(_Win,Num,Cat,Tgnst,Plaats,Ronde,Uitsl1,TijdKK,LL,Keur,TID,MT)),
	win_Invalidate(_Win),
	!.
%END Plaatsing, idcp_uitsluit _CtlInfo


%BEGIN Plaatsing, idcp_rating_up _CtlInfo
  dlg_plaatsing_eh(_Win,e_Control(idcp_rating_up,_CtrlType,_CtrlWin,_CtlInfo),0):-
	Pwin = win_GetCtlHandle(_Win, idcp_plaats),
	win_SetState(PWin, [wsf_Disabled]),
	LboxWin = win_GetCtlHandle(_Win, idcp_plaatsing),
	winCat(_Win, Cat),
	voorNa(_Win, Cat, ErNa, _EigenCount, _ErNaCount),
	getbacktrack(HereX),
	opg_t(_Win,Num,Erna,Tgnst1,Plaats,Ronde,Uitsl,Tijd1,LLXX,Tid,Keur,MT),
	cutbacktrack(HereX),
	verplaatsingMogelijk(Erna,Tgnst1),
	retract(opg_t(_Win,Num,Erna,_,_,_,_,_,_,_,_,_)), % de voorste van erna
	getbacktrack(Here),
	numb(N),     
	OpgNum = Num + N - 1,
	not(opg(OpgNum,Cat,_,_,_,_,_,_,_,_,_)),
	cutbacktrack(Here),
	assertz(opg_t(_Win,OpgNum,Cat,Tgnst1,Plaats,Ronde,Uitsl,Tijd1,LLXX,Tid,Keur,MT)),
	opglistbase1(OpgNum, Cat, Tgnst1, Plaats, Item, idcp_rating),
	lbox_Add(LboxWin, Item),
	NoOfItems = lbox_CountAll(LboxWin),
	Laatste = NoOfItems - 1,
	lbox_SetSel(LboxWin, Laatste, b_True),
	win_SetFocus(LboxWin),
	checkDeelInButtons(_Win),
	!.
%END Plaatsing, idcp_rating_up _CtlInfo

%BEGIN Plaatsing, idcp_rating_dn _CtlInfo
  dlg_plaatsing_eh(_Win,e_Control(idcp_rating_dn,_CtrlType,_CtrlWin,_CtlInfo),0):-
	Pwin = win_GetCtlHandle(_Win, idcp_plaats),
	win_SetState(PWin, [wsf_Disabled]),
	LboxWin = win_GetCtlHandle(_Win, idcp_plaatsing),
	NoOfItems = lbox_CountAll(LboxWin),
	Laatste = NoOfItems - 1,
	lbox_SetSel(LboxWin, Laatste, b_True),
	Item = lbox_GetItem(LboxWin, Laatste),
        opg_repr(_Win, Item, Num, Cat, _Tijd, _LL, _Plts, _Sterkte, _RStr, _RDelta, _RDcolor, _Uitsl, _Naam, _Naam1, _, _, _, _, _),
	win_SetFocus(_Win),
	voorNa(_Win, Cat, ErNa, _EigenCount, _ErNaCount),
	opg_t(_Win,Num,Cat,Tgnst1,Plaats,Ronde,Uitsl,Tijd1,LLXX,Tid,Keur,MT),
	verplaatsingMogelijk(Cat,Tgnst1),
	retract(opg_t(_Win,Num,Cat,_,_,_,_,_,_,_,_,_)), 
	getbacktrack(Here),
	numb(N),     
	OpgNum = Num + N - 1,
	not(opg(OpgNum,Erna,_,_,_,_,_,_,_,_,_)),
	cutbacktrack(Here),
	asserta(opg_t(_Win,OpgNum,Erna,Tgnst1,Plaats,Ronde,Uitsl,Tijd1,LLXX,Tid,Keur,MT)), 
	lbox_Delete(LboxWin, Laatste),
	Laatste1 = Laatste - 1,
	lbox_SetSel(LboxWin, Laatste1, b_True),
	checkDeelInButtons(_Win),
	!.
%END Plaatsing, idcp_rating_dn _CtlInfo

%BEGIN Plaatsing, idcp_plaats _CtlInfo
  dlg_plaatsing_eh(_Win,e_Control(idcp_plaats,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
  	sorteerveld(Volgorde),
	dlg_plaatsvoorstel_Create(_Win, Volgorde),
	!.
%END Plaatsing, idcp_plaats _CtlInfo

%BEGIN Plaatsing, idcp_uitloothulp _CtlInfo
  dlg_plaatsing_eh(_Win,e_Control(idcp_uitloothulp,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	winCat(_Win, Cat),
	dlg_uitloten_Create(_Win, Cat),
	!.
%END Plaatsing, idcp_uitloothulp _CtlInfo

%BEGIN Plaatsing, idcp_plaatsing1 _CtlInfo
  dlg_plaatsing_eh(_Win,e_Control(idcp_plaatsing1,_CtrlType,_CtrlWin,_CtlInfo),0):-
	volgordeOpg(_Win, idcp_plaatsing1),
	!.
%END Plaatsing, idcp_plaatsing1 _CtlInfo

%BEGIN Plaatsing, idcp_rangorde _CtlInfo
  dlg_plaatsing_eh(_Win,e_Control(idcp_rangorde,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	volgordeOpg(_Win, idcp_rangorde),
	!.
%END Plaatsing, idcp_rangorde _CtlInfo

%BEGIN Plaatsing, idcp_rating _CtlInfo
  dlg_plaatsing_eh(_Win,e_Control(idcp_rating,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	volgordeOpg(_Win, idcp_rating),
	!.
%END Plaatsing, idcp_rating _CtlInfo

%BEGIN Plaatsing, idcp_plaatsing losefocus
  dlg_plaatsing_eh(_Win,e_Control(idcp_plaatsing,_CtrlType,_CtrlWin,losefocus),0):-!,
	retractall(heeftfocus),
	win_Invalidate(_Win),
	!.
%END Plaatsing, idcp_plaatsing losefocus

%BEGIN Plaatsing, idcp_plaatsing getfocus
  dlg_plaatsing_eh(_Win,e_Control(idcp_plaatsing,_CtrlType,_CtrlWin,getfocus),0):-!,
	retractall(heeftfocus),
	assert(heeftfocus),
	win_Invalidate(_Win),
	!.
%END Plaatsing, idcp_plaatsing getfocus

%BEGIN Plaatsing, idcp_naam _CtlInfo
  dlg_plaatsing_eh(_Win,e_Control(idcp_naam,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idcp_plaatsing),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	%write("\n",Item), 
        opg_repr(_Win, Item, Num, Cat, _Tijd, _LL, _Plts, _Stxx, _RStrx, _RDelta,  _RDcolor, _Uitslx, Naam, _Naam1x, _, _, _, _, _),
        sp2(SKeus,Sex,Naam,_Tthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, LidNr, Sterkte, SterkD, Geb, Tel1, Tel2, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Res),
        SpelerIn = sp2(SKeus,Sex,Naam,_Tthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, LidNr, Sterkte, SterkD, Geb, Tel1, Tel2, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Res),
        %write("\n", SpelerIn),
        dlg_persoonsgegevens_Create(_Win, SpelerIn),
	sorteerVeld(Sorteer),
	findall(Veld, opgListBase(_Win, Cat, Veld, Sorteer), OpgBaseL), 
	sortstring_c(OpgBaseL, 1),
	lbox_Suspend(LboxWin),
	lbox_Clear(LboxWin),
	lbox_Add(LboxWin, OpgBaseL),
	lbox_Resume(LboxWin),
	zoekindex(Num, Cat, OpgBaseL, 0, NewIndex),
	lbox_SetSel(LboxWin, NewIndex, b_True),
	!.
%END Plaatsing, idcp_naam _CtlInfo

%BEGIN Plaatsing, idcp_partner _CtlInfo
  dlg_plaatsing_eh(_Win,e_Control(idcp_partner,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idcp_plaatsing),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
        opg_repr(_Win, Item, Num, Cat, _Tijd, _LL, _Plts, _Sterkte, _RStr, _RDelta, _RDcolor, _Uitsl, _Naam, Naam1, _, _, _, _, _),
        sp2(SKeus,Sex,Naam1,Tthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, LidNr, Sterkte, SterkD, Geb, Tel1, Tel2, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Res),
        SpelerIn = sp2(SKeus,Sex,Naam1,Tthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, LidNr, Sterkte, SterkD, Geb, Tel1, Tel2, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Res),
        dlg_persoonsgegevens_Create(_Win, SpelerIn),
	sorteerVeld(Sorteer),
	findall(Veld, opgListBase(_Win, Cat, Veld, Sorteer), OpgBaseL), 
	sortstring_c(OpgBaseL, 1),
	lbox_Suspend(LboxWin),
	lbox_Clear(LboxWin),
	lbox_Add(LboxWin, OpgBaseL),
	lbox_Resume(LboxWin),
	zoekindex(Num, Cat, OpgBaseL, 0, NewIndex),
	lbox_SetSel(LboxWin, NewIndex, b_True),
	!.
%END Plaatsing, idcp_partner _CtlInfo


%BEGIN Plaatsing, e_Update
  dlg_plaatsing_eh(_Win,e_Update(_UpdateRct),0):-
	win_GetTextExtent(_Win, "goedgek ", -1, Wstatus, _),
	win_GetTextExtent(_Win, " 31/12 22:22", -1, WDatum, _),
	win_GetTextExtent(_Win, " Plts", -1, WPlts, _),
	win_GetTextExtent(_Win, " Sterk", -1, Wstrk, _),
	win_GetTextExtent(_Win, " 999  ", -1, Wrangorde0, _),
	win_GetTextExtent(_Win, "5,5555 ", -1, Wrating, _),
	metDelta(_Win, DeltaFact),
	win_GetTextExtent(_Win, "  1,6", -1, WratingDelta, _),
	WRatingDelta1 = WRatingDelta * DeltaFact,
	metRangorde(_Win, RangFact),
	WRangorde = WRangorde0 * RangFact,
	WrangordeDelta = Wrangorde * DeltaFact,			% al of niet rangorde!!
	win_GetTextExtent(_Win, "Uitsl", -1, Wuitsl, _),
	maxWCveld(_Win, WCVrij),
	assert(iafm(Wstatus, WDatum, WCVrij, WPlts, Wstrk, WRating, WRatingDelta1, Wuitsl, Wrangorde, WrangordeDelta)),
	fail.
  dlg_plaatsing_eh(_Win,e_Update(_UpdateRct),0):-
	LboxWin = win_GetCtlHandle(_Win, idcp_plaatsing),
	D1 = win_GetCtlHandle(_Win, idcp_status),
	DRCT = win_GetOuterRect(D1),
	DRct = rct(_, T, _R, B),	% 5.1.2003
	LRct = win_GetOuterRect(LboxWin),
	Lrct = rct(Begin, _, Breedte0 , _),
	Breedte = Breedte0 - Begin,
	iafm(Wstatus, WDatum, WCVrij, WPlts, Wstrk, Wrating, WRatingDelta, Wuitsl, WRangorde, WRangordeDelta),
	L1 = WStatus,
	%L1 = 1,
	Rxx = Wstatus,
	R1 = L1 + WDatum,
	V1 = rct(Rxx, T, R1, B),
	draw_TextInRect(_Win, V1, "Datum", -1, [dtext_center]), 
	%
	R1a = R1 + WCVrij,
	L2 = R1 + WPlts + WCVrij,
	V1a = rct(R1a, T, L2, B),
	draw_TextInRect(_Win, V1a, "Plts", -1, [dtext_center]), 
	%%				de rangorde als ie er is
	L5 = L2 + WRangorde + Wrangordedelta + Wstrk + WRating + WratingDelta,
	%%
	L6 = L5 + Wuitsl,
	V6 = rct(L5, T, L6, B),
	draw_TextInRect(_Win, V6, "Uitl", -1, [dtext_center]), 
	%
	L7 = (L6 + Breedte) div 2,
	V7 = rct(L6, T, L7, B),
	draw_TextInRect(_Win, V7, "Speler", -1, [dtext_left]), 
	%
	V8 = rct(L7, T, Breedte, B), 
	draw_TextInRect(_Win, V8, "Partner", -1, [dtext_left]), 
	fail.
  dlg_plaatsing_eh(_Win,e_Update(_UpdateRct),0):-
	iafm(Wstatus, WDatum, WCVrij, WPlts, Wstrk, Wrating, 0, _Wuitsl, WRangorde, _WRangordeDelta),
	D1 = win_GetCtlHandle(_Win, idcp_status),
	DRCT = win_GetOuterRect(D1),
	DRct = rct(_, T, _R, B),	% 5.1.2003
	L2 = WStatus + WDAtum + WPlts+ WCVrij,
	%%				de rangorde als ie er is
	addRangorde(1, _Win, L2, WRAngorde, 0, T, B),
	% addRangorde single
	L3 = L2 + WRangorde,
	%%
	L4 = L3 + Wstrk,
	V3 = rct(L3, T, L4, B),
	draw_TextInRect(_Win, V3, "Spst", -1, [dtext_center]), 
	%
	L5 = L4 + Wrating,
	V5 = rct(L4, T, L5, B),
	draw_TextInRect(_Win, V5, "Rating", -1, [dtext_center]), 
	%
	fail.
  dlg_plaatsing_eh(_Win,e_Update(_UpdateRct),0):-	% onderste lijn dubbel
	iafm(Wstatus, WDatum, WCVrij, WPlts, Wstrk, Wrating, WRatingDelta, _Wuitsl, WRangorde, WRangordeDelta),
	WRatingDelta > 0,
	D1 = win_GetCtlHandle(_Win, idcp_status),
	DRCT = win_GetOuterRect(D1),
	DRct = rct(_, T, _R, B),	% 5.1.2003
	L2 = WStatus + WDAtum + WPlts + WCVrij,
	%%				de rangorde als ie er is
	addRangorde(2, _Win, L2, WRAngorde, WRangordeDelta, T, B),
	% addRangorde dubbelonder
	L3 = L2 + WRangorde + WRangOrdeDelta,
	%%
	L4 = L3 + Wstrk,
	V3 = rct(L3, T, L4, B),
	draw_TextInRect(_Win, V3, "Gem.", -1, [dtext_center]), 
	%
	L5 = L4 + Wrating,
	Picture = pict_GetFromRes(idb_delta),
	pict_GetSize(Picture, WI, _Y, _Size),
	%win_GetTextExtent(_Win, "Gem.", -1, WR, _),
	V5 = rct(L4, T, L5, B),
	draw_TextInRect(_Win, V5, "Gem.", -1, [dtext_center]), 
	L6 = L5 + (WRatingDelta - WI) div 2,
	T2 = T + 3,
	pict_Draw(_Win, Picture, pnt(L6, T2), rop_SrcCopy),
	%
	fail.
  dlg_plaatsing_eh(_Win,e_Update(_UpdateRct),0):-	% bovenste lijn dubbel
	iafm(Wstatus, WDatum, WCVrij, WPlts, Wstrk, Wrating, WRatingDelta, _Wuitsl, WRangorde, WRangordeDelta),
	WRatingDelta > 0,
	D1 = win_GetCtlHandle(_Win, idcp_status),
	DRCT = win_GetOuterRect(D1),
	DRct = rct(_, T, _R, B),	% 5.1.2003
	Tbovenste = 2 * T - B, 
	L2 = WStatus + WDAtum + WPlts + WCVrij,
	%%				de rangorde als ie er is
	addRangorde(3, _Win, L2, WRAngorde, WRangordeDelta, T, B),
	% addRangorde dubbel boven
	L3 = L2 + WRangorde + WRangordedelta,
	%%
	L4 = L3 + Wstrk,
	V3 = rct(L3, Tbovenste, L4, T),
	draw_TextInRect(_Win, V3, "Spst", -1, [dtext_center]), 
	%
	L5 = L4 + Wrating + WRatingDelta,
	V5 = rct(L4, Tbovenste, L5, T),
	draw_TextInRect(_Win, V5, "Rating", -1, [dtext_center]), 
	%
	fail.
  dlg_plaatsing_eh(_Win,e_Update(_UpdateRct),0):- !.	% klaar  hè hè
%END Plaatsing, e_Update

%BEGIN Plaatsing, e_User
  dlg_plaatsing_eh(_Win,e_User(prefresh,_Ptr),0):-
        dlg_plaatsing_refresh(),
        trap(win_Setfocus(_Win), _, true), !.
  dlg_plaatsing_eh(_Win,e_User(teplaatsen,_Ptr),0):-
	winCat(_Win, Cat),
	isKwalificatie(Cat, IsSectie),
	IsSectie = b_False, 		% moet nog gesplitst worden 
	uitgeslotenT(_Win, AantalU),
	geplaatstenT(_Win, AantalG),
        findall(Num, opg_t(_Win,Num,_Cat,_Tgnst,_Plaats,_Ronde,_Uitsl,_TijdKK,_LL,_Keur,_,_), NumL),
        count(NumL, 0, AantalT),
	format(Msg, "Aantal opgaven: %u. Aantal uitgesloten: %u.\nGeplaatst: %u van 4 of 8 (kwalificatie!).",
	      AantalT, AantalU, AantalG), 
	MWin = win_GetCtlHandle(_Win, idctp_totaal_te_plaatsen),
        win_SetText(MWin, Msg), !.
  dlg_plaatsing_eh(_Win,e_User(teplaatsen,_Ptr),0):-
	winCat(_Win, Cat),
	uitgeslotenT(_Win, AantalU),
	geplaatstenT(_Win, AantalG),
        findall(Num, opg_t(_Win,Num,Cat,_Tgnst,_Plaats,_Ronde,_Uitsl,_TijdKK,_LL,_Keur,_,_), NumL),
        count(NumL, 0, AantalT),
        A = AantalT - AantalU,
	findall(ASl, juistAantalGeplaatsten(Cat, A, ASl, _), ASList),
	list_to_string(ASList, " of ", ASString),
	format(Msg, "Aantal opgaven: %u. Aantal uitgesloten: %u.\nGeplaatst: %u van %s (afval).",
	      AantalT, AantalU, AantalG, ASString), 
	MWin = win_GetCtlHandle(_Win, idctp_totaal_te_plaatsen),
        win_SetText(MWin, Msg), !.
%END Plaatsing, e_User

%BEGIN Plaatsing, idcp_datum _CtlInfo
  dlg_plaatsing_eh(_Win,e_Control(idcp_datum,_CtrlType,_CtrlWin,_CtlInfo),0):-
	volgordeOpg(_Win, idcp_datum),
	!.
%END Plaatsing, idcp_datum _CtlInfo

%BEGIN Plaatsing, e_OwnerMeasureItem
  dlg_plaatsing_eh(_Win,e_OwnerMeasureItem(_CtlType,_CtlId,_ItemId,_Data),Size):-
	win_GetTextExtent(_Win, "goedgek", -1, Wstatus, Height),
	H = Height + 1,
	bitleft(H, 16, H1),
	Size = Wstatus + H1,
	!.
%END Plaatsing, e_OwnerMeasureItem

%BEGIN Plaatsing, e_OwnerDraw
  dlg_plaatsing_eh(_Win,e_OwnerDraw(_CtlType,_CtlId,_ItemId,_ItemAction,
		ItemState,LboxWin,RectItem,_ItemData),0):-
	own_member1(ItemState, odstate_Selected),   % was selected
	heeftfocus(),
	win_SetBrush(LboxWin,brush(pat_Solid, color_Blue)),
	myOwnerdraw2(_Win, LboxWin, _ItemId, RectItem, color_white, color_blue, b_True),
	!.
  dlg_plaatsing_eh(_Win,e_OwnerDraw(_CtlType,_CtlId,_ItemId,_ItemAction,
		ItemState,LboxWin,RectItem,_ItemData),0):-
	own_member1(ItemState, odstate_Selected), !,  % was selected
	win_SetBrush(LboxWin,brush(pat_Solid, color_ltGray)),
	myOwnerdraw2(_Win, LboxWin, _ItemId, RectItem, color_white, color_ltgray, b_True),
	!.
  dlg_plaatsing_eh(_Win,e_OwnerDraw(_CtlType,_CtlId,_ItemId,_ItemAction,
		_ItemState,LboxWin,RectItem,_ItemData),0):-
	_Itemstate = [],
	win_SetPen(LboxWin,pen(1,ps_Solid,color_White)),
	myOwnerdraw2(_Win, LboxWin, _ItemId, RectItem, color_black, color_white, b_False),
	!.
%END Plaatsing, e_OwnerDraw

%BEGIN Plaatsing, idcp_wis _CtlInfo
  dlg_plaatsing_eh(_Win,e_Control(idcp_wis,_CtrlType,_CtrlWin,_CtlInfo),0):-
	%winCat(_Win, Cat),
	LboxWin = win_GetCtlHandle(_Win, idcp_plaatsing),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
        opg_repr(_Win, Item, Num, Cat, _Tijd, _LL, _Plts, _Sterkte, _RStr, _RDelta, _RDcolor, _Uitsl, _Naam, _Naam1, _, _, _, _, _),
	opg_t(_Win,Num,Cat,Tgnst1,_,_,_,Tijd1,_,_,_,_),
	retract(opg_t(_Win,Num,Cat,Tgnst1,_,_,_,Tijd1,_,_,_,_)), 
	lbox_Delete(LboxWin, Index),
	lbox_SetSel(LboxWin, Index, b_True),
	checkDeelInButtons(_Win),
	!.
%END Plaatsing, idcp_wis _CtlInfo

%BEGIN Plaatsing, idc_ok _CtlInfo
  dlg_plaatsing_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	saveOpg(_Win),
	win_Destroy(_Win),
	!.
%END Plaatsing, idc_ok _CtlInfo

%BEGIN Plaatsing, e_CloseRequest
  dlg_plaatsing_eh(_Win,e_CloseRequest,0):-!,
	compOpg(_Win),
	Ans = dlg_Ask("","Veranderingen bewaren?",["&Ja","&Nee"]),
	Ans = resp_default,
	saveOpg(_Win),
	win_Destroy(_Win),
	!.
%END Plaatsing, e_CloseRequest

%BEGIN Plaatsing, idc_help _CtlInfo
  dlg_plaatsing_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-
	project_ShowHelpContext("Plaatsing"),
	!.
%END Plaatsing, idc_help _CtlInfo

%BEGIN Plaatsing, idcp_speelsterkte _CtlInfo
  dlg_plaatsing_eh(_Win,e_Control(idcp_speelsterkte,_CtrlType,_CtrlWin,_CtlInfo),0):-
	volgordeOpg(_Win, idcp_speelsterkte),
	!.
%END Plaatsing, idcp_speelsterkte _CtlInfo

%BEGIN Plaatsing, idcp_alfabetisch _CtlInfo
  dlg_plaatsing_eh(_Win,e_Control(idcp_alfabetisch,_CtrlType,_CtrlWin,_CtlInfo),0):-
	volgordeOpg(_Win, idcp_alfabetisch),
	!.
%END Plaatsing, idcp_alfabetisch _CtlInfo

%BEGIN Plaatsing, idcp_plaatsing selchanged
  dlg_plaatsing_eh(_Win,e_Control(idcp_plaatsing,_CtrlType,_CtrlWin,selchanged),0):-
	checkDeelInButtons(_Win),
	!.
%END Plaatsing, idcp_plaatsing selchanged

%BEGIN Plaatsing, idcp_pbdoornaar _CtlInfo
  dlg_plaatsing_eh(_Win,e_Control(idcp_pbdoornaar,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idcp_plaatsing),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
        opg_repr(_Win, Item, Num, Cat, _Tijd, _LL, _Plts, _Sterkte, _RStr, _RDelta, _RDcolor, _Uitsl, _Naam, _Naam1, _, _, _, _, _),
	opg_t(_Win,Num,Cat,Tgnst1,_,_,UitslXX,_,LLXX,Keuring,TID,MT),
	personen(Tgnst1, Personen, SpCatL),
	findall(CatO, passende_categorie(Cat, CatO, Personen, SpCatL), P_Cats),
	dlg_onderdeel_select_Create(_Win, "Door naar onderdeel", P_Cats, 0, Selectie, _),
	win_SetFocus(_Win),
	Selectie = [CatS|_],
	wc(CatNum, _, _, CatS, _, _, _, _, _, _, _, _, _, _, _),
	getbacktrack(Here),
	numb(N),     
	OpgN = Num + N - 1,
	not(opg(OpgN,CatNum,_,_,_,_,_,_,_,_,_)),
	cutbacktrack(Here),
	assert(opg_t(_Win,OpgN,CatNum,Tgnst1,0,0,UitslXX,0,LLXX,Keuring,TID,MT)), 
	!.
%END Plaatsing, idcp_pbdoornaar _CtlInfo

%BEGIN Plaatsing, idcp_pbverhuis _CtlInfo
  dlg_plaatsing_eh(_Win,e_Control(idcp_pbverhuis,_CtrlType,_CtrlWin,_CtlInfo),0):-
	%winCat(_Win, Cat),
	LboxWin = win_GetCtlHandle(_Win, idcp_plaatsing),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
        opg_repr(_Win, Item, Num, Cat, _Tijd, _LL, _Plts, _Sterkte, _RStr, _RDelta, _RDcolor, _Uitsl, _Naam, _Naam1, _, _, _, _, _),
	opg_t(_Win,Num,Cat,Tgnst1,_,_,UitslXX,Tijd1,_,_,_,_),
	personen(Tgnst1, Personen, SpCatL),
	findall(CatO, passende_categorie(Cat, CatO, Personen, SpCatL), P_Cats),
	dlg_onderdeel_select_Create(_Win, "Te verhuizen naar:", P_Cats, 0, Selectie, _),
	win_SetFocus(_Win),
	Selectie = [CatS|_],
	wc(CatNum, _, _, CatS, _, _, _, _, _, _, _, _, _, _, _),
	retract(opg_t(_Win,Num,Cat,Tgnst1,_,_,_,Tijd1,LLXX,Keuring,TID,MT)), 
	getbacktrack(Here),
	numb(N),     
	OpgN = Num + N - 1,
	not(opg(OpgN,CatNum,_,_,_,_,_,_,_,_,_)),
	cutbacktrack(Here),
	assert(opg_t(_Win,OpgN,CatNum,Tgnst1,0,0,UitslXX,0,LLXX,Keuring,TID,MT)), 
	lbox_Delete(LboxWin, Index),
	lbox_SetSel(LboxWin, Index, b_True),
	checkDeelInButtons(_Win),
	!.
%END Plaatsing, idcp_pbverhuis _CtlInfo

%BEGIN Plaatsing, e_Destroy
  dlg_plaatsing_eh(_Win,e_Destroy,0):-!,
	assert(iafm(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)),
	retractall(opg_t(_Win,_,_,_,_,_,_,_,_,_,_,_)),
	retractall(winCat(_Win, _)),		% als laatste !
	assert(eersteIndex(0)),
	!.
%END Plaatsing, e_Destroy

%BEGIN Plaatsing, idcp_selecteer _CtlInfo
  dlg_plaatsing_eh(_Win,e_Control(idcp_selecteer,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idcp_plaatsing),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
        opg_repr(_Win, Item, Num, Cat, _, _, _, _Sterkte, _RStr, _, _, _, _, _, _, _, _, _, _),
	opg_t(_Win,Num,Cat,Tgnst,Plaats,Ronde,Uitsl,TijdKK,LL,Keur,TID,MT), 
	dlg_plaatsing1_Create(_Win, Num, Cat, Tgnst,Plaats,Ronde,Uitsl,TijdKK,LL,NumX ,PlaatsX,RondeX,UitslX,TijdX,LLX,Result,0),
	Result = idc_ok,
	retract(opg_t(_Win,Num,Cat,_,_,_,_,_,_,_,_,_)),
	assert(opg_t(_Win,NumX,Cat,Tgnst,PlaatsX,RondeX,UitslX,TijdX,LLX,Keur,TID,MT)),
	alIngedeeld(Tgnst, Cat, UitslX),
	sorteerVeld(Sorteer),
	findall(Veld, opgListBase(_Win, Cat, Veld, Sorteer), OpgBaseL), 
	sortstring_c(OpgBaseL, 1),
	lbox_Clear(LboxWin),
	lbox_Add(LboxWin, OpgBaseL),
	zoekindex(Num, Cat, OpgBaseL, 0, NewIndex),
	lbox_SetSel(LboxWin, NewIndex, b_True),
	checkDeelInButtons(_Win),
	win_Invalidate(_Win),
	!.
%END Plaatsing, idcp_selecteer _CtlInfo

%BEGIN Plaatsing, idcp_plaatsing activated
  dlg_plaatsing_eh(_Win,e_Control(idcp_plaatsing,_CtrlType,LboxWin,activated),0):-
	%winCat(_Win, Cat),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
        opg_repr(_Win, Item, Num, Cat, _, _, _, _Sterkte, _RStr, _RDelta, _RDcolor, _, _, _, _, _, _, _, _),
	opg_t(_Win,Num,Cat,Tgnst,Plaats,Ronde,Uitsl,TijdKK,LL,Keur,TID,MT), 
	dlg_plaatsing1_Create(_Win, Num, Cat, Tgnst,Plaats,Ronde,Uitsl,TijdKK,LL,NumX ,PlaatsX,RondeX,UitslX,TijdX,LLX,Result,0),
	Result = idc_ok,
	retract(opg_t(_Win,Num,Cat,Tgnst,_,_,_,_,_,_,_,_)),
	assert(opg_t(_Win,NumX,Cat,Tgnst,PlaatsX,RondeX,UitslX,TijdX,LLX,Keur,TID,MT)),
	sorteerVeld(Sorteer),
	findall(Veld, opgListBase(_Win, Cat, Veld, Sorteer), OpgBaseL), 
	sortstring_c(OpgBaseL, 1),
	lbox_Clear(LboxWin),
	lbox_Add(LboxWin, OpgBaseL),
	zoekindex(Num, Cat, OpgBaseL, 0, NewIndex),
	lbox_SetSel(LboxWin, NewIndex, b_True),
	checkDeelInButtons(_Win),
	!.
%END Plaatsing, idcp_plaatsing activated

%BEGIN Plaatsing, idc_cancel _CtlInfo
  dlg_plaatsing_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-
	compOpg(_Win),
	Ans = dlg_Ask("","Veranderingen bewaren?",["&Ja","&Nee"]),
	Ans = resp_default,
	saveOpg(_Win),
	win_Destroy(_Win),
	!.
  dlg_plaatsing_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	win_Destroy(_Win),
	!.
%END Plaatsing, idc_cancel _CtlInfo

%BEGIN Plaatsing, e_Create
  dlg_plaatsing_eh(_Win,e_Create(_CreationData),0):-!,
	retractall(winCat(~0, _)),
	retractall(heeftfocus()),
	assert(heeftfocus()),
	Cat = cast(wedscat, _CreationData),
	assert(winCat(_Win, Cat)),
	copyOpg(_Win, Cat),
	wc(Cat, _, _, CatS, WTyp, _, _, _, _, _, _, _, _, _, _),
	TitWin = win_GetCtlHandle(_Win, idctp_onderdeel),
	win_SetText(TitWin, CatS),
	LboxWin = win_GetCtlHandle(_Win, idcp_plaatsing),
	setSorteerVeld(_Win, Cat),
	sorteerVeld(Sorteer),
	findall(Veld, opgListBase(_Win, Cat, Veld, Sorteer), OpgBaseL),
	sortstring_c(OpgBaseL, 1),
	lbox_Add(LboxWin, OpgBaseL),
	eersteIndex(Num),
	zoekindex(Num, Cat, OpgBaseL, 0, NewIndex),
	lbox_SetSel(LboxWin, NewIndex, b_True),
	checkDeelInButtons(_Win),
	checkPartnerButton(_Win, WTyp),
	voorNa(_Win, Cat,_ErNa,_EigenCount, _NaCount),
	FWin = win_GetCtlHandle(_Win, idcp_rating_up),
	win_BringToTop(FWin),
	GWin = win_GetCtlHandle(_Win, idcp_rating_dn),
	win_BringToTop(GWin), !.
%END Plaatsing, e_Create

  dlg_plaatsing_eh(_Win,e_Control(_,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idcp_plaatsing),
	win_SetFocus(LboxWin),
	fail.
  dlg_plaatsing_eh(_,_,_):-!,fail.

%END_DLG Plaatsing


%BEGIN_DLG Plaatsing1
/**************************************************************************
	Creation and event handling for dialog: Plaatsing1
**************************************************************************/

constants

%BEGIN Plaatsing1, CreateParms, 19:41:48-24.1.2014, Code automatically updated!
  dlg_plaatsing1_DlgType = wd_Modal
  dlg_plaatsing1_Title = "Opgave/plaatsingsdetails"
  dlg_plaatsing1_RCT = rct(40,32,397,183)
  dlg_plaatsing1_Flags = [wsf_Close,wsf_TitleBar]
  dlg_plaatsing1_Help = idh_opgavedetails
  dlg_plaatsing1_Font = "Arial"
  dlg_plaatsing1_FSize = 9
%END Plaatsing1, CreateParms
  date_cc_class = "date_cc_class"

predicates

  dlg_plaatsing1_eh : EHANDLER
  dlg_plaatsing1_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST,integer,wedscat,integer,integer,integer,integer,integer,string)
  dlg_plaatsing1_update(DIALOG_VAL_LIST,integer,wedscat,integer,integer,integer,integer,integer,string)
  int2DialogInt(integer, DIALOG_INT)
  opgNamen(tgnst, string, string, string, string, string, string)
  date_cc_class_handlerP : EHANDLER
  opgaveTijdstipUniek(ulong)
nondeterm lokaalProbleem(wedscat, ilist, string Msg)
determ bondsnrfout(string)
procedure  zeefblanks(string, integer, string) - (i,i,o)

clauses

zeefBlanks(In, Ctr, Uit) :-
  frontstr(2, In, S, R),
  S = "  ", !,
  concat(" ", R, R1),
  zeefBlanks(R1, Ctr, Uit), !.
zeefBlanks(In, Ctr, Uit) :-
  frontstr(1, In, S, R),
  S = " ",
  Ctr > 70,  !,
  concat("\n", R, R1),
  zeefBlanks(R1, 0, Uit), !.
zeefBlanks(In, Cntr, Uit) :-
  frontstr(1,In,S,R), !,
  Cntr1 = Cntr + 1,
  zeefBlanks(R, Cntr1, Uit1),
  concat(S, Uit1, Uit). 
zeefBlanks(_, _Cntr, "").


int2DialogInt(0, void()):- !.
int2DialogInt(X, i(X)) :- !.

opgNamen(s(SpNo), Naam, SpSt, RE, "", "", "") :-
  sp2(SpNo,_,Naam,_,_,RE,_RD,_,_, _, _, _, _, SpSt, _DS, _, _, _, _, _,_,_RangPosE,_RangPosD,_,_CheckGeg,_Log,_Res), !.
opgNamen(pw(SpNo), Naam, SpSt, RE, "zoekt partner", "", "") :-
  sp2(SpNo,_,Naam,_,_,RE,_RD,_,_, _, _, _, _, SpSt, _DS, _, _, _, _, _,_,_RangPosE,_RangPosD,_,_CheckGeg,_Log,_Res), !.
opgNamen(p(SpNo,Sp1), Naam, SpSt, RD1, Naam1, SpSt2, RD2) :-
  sp2(SpNo,_,Naam,_,_,_RE1,RD1,_,_, _, _, _, _, _ES1, SPSt, _, _, _, _, _,_,_RangPosE1,_RangPosD1,_,_CheckGeg1,_Log1,_Res1), 
  sp2(Sp1,_,Naam1,_,_,_RE2,RD2,_,_, _, _, _, _, _ES2, SpSt2, _, _, _, _, _,_,_RangPosE2,_RangPosD2,_,_CheckGeg2,_Log2,_Res2), !.

date_cc_class_handlerP(Win,e_Create(_),0):-!,		% CALL date_cc_Init ON
  selectedOpgave(InitValue, _, _),
  date_cc_Init(Win, InitValue, "%DD-%MD-%YL %HH:%MM"),
  Font = font_Create(ff_Helvetica, [], 8),
  win_SetFont(Win, Font).
date_cc_class_handlerP(Win, EVENT, 0):-			% CALL date_cc_HandleEvent
  date_cc_handleEvent(Win, Event), !.			% ON ANY OTHER EVENT
date_cc_class_handlerP(Win, EVENT, 0):-!,			% CALL date_cc_HandleEvent
  dlg_plaatsing1_eh(Win, Event).			% ON ANY OTHER EVENT

opgaveTijdstipUniek(MinOffset) :-
  selectedOpgave(MinOffset, _, _), !.
opgaveTijdstipUniek(MinOffset) :-
  opg_t(_,MinOffset,_Cat,_T,_Plts,_Ronde,_Uitsl,_,_,_,_,_),
  MinPlus = MinOffset + 1,
  opgaveTijdstipUniek(MinPlus), !.
opgaveTijdstipUniek(MinOffset) :-
  opg(MinOffset,_Cat,_T,_Plts,_Ronde,_Uitsl,_,_,_,_,_),
  MinPlus = MinOffset + 1,
  opgaveTijdstipUniek(MinPlus), !.
opgaveTijdstipUniek(MinOffset) :-
  retract(selectedOpgave(_, Cat, EersteFocus)),
  assert(selectedOpgave(MinOffset, Cat, EersteFocus)), !.

lokaalOnderdeelProbleem(Cat) :-   	% global!!
  opg(_,Cat,Tgnst,_,_,_,_,_,_,_,_),
  tgnstnum(Tgnst, TNum),
  sp2(Tnum,SexC,NaamS,_,_,_,_,_,_,_,_,_,_, StS, StD, Geb, _, _,_,_,_,_,_,_,_,_,_),
  partnerCritFout1(Cat, NaamS, SexC, Geb, StS, StD, b_False, _Msg), !.

bondsnrfout("") :- !.
bondsnrfout(BN) :-
  str_int(BN, 0), !.
bondsnrfout(BN) :-
  not(proef11det(BN)).

lokaalProbleem(Cat, TL, Msg) :-
  bovencat(Cat, CatB),
  wc(CatB, _, _, _, _, _, _, _, _, _, _, _, Origine, _, _),
  not(Origine = vanzelf),
  member(PersNo, TL),
  sp2(PersNo,_SexC,NaamS,_,_,_,_,_,_,_,_,_,BondsNr, _, _, _, _, _,_,_,_,_,_,_,_,_,_),
  bondsnrFout(BondsNr),
  format(Msg, "Speler %s heeft een verkeerd of ontbrekend bondsnummer %s",NaamS,BondsNr).
lokaalProbleem(Cat, TL, Msg) :-
  member(PersNo, TL),
  sp2(PersNo,SexC,NaamS,_,_,_,_,_,_,_,_,_,_, StS, StD, Geb, _, _,_,_,_,_,_,_,_,_,_),
  partnerCritFout1(Cat, NaamS, SexC, Geb, StS, StD, b_False, Msg).

keur2tekst(_, _,msg(Sev,Nr,Msg,Sev1,Nr1,Msg1), Sev,Tekst, color_blue) :-
  Sev = "W",
  format(Tekst, "%s %s %s\n%s %s %s",Sev,Nr,Msg,Sev1,Nr1,Msg1), !.
keur2tekst(_,_,msg(Sev,Nr,Msg,Sev1,Nr1,Msg1), Sev,Tekst, color_red) :-
  Sev = "F",
  format(Tekst, "%s %s %s\n%s %s %s",Sev,Nr,Msg,Sev1,Nr1,Msg1), !.
keur2tekst(_,_,msg(Sev,Nr,Msg,Sev1,Nr1,Msg1), Sev,Tekst, color_green) :-
  Sev = "I",
  format(Tekst, "%s %s %s\n%s %s %s",Sev,Nr,Msg,Sev1,Nr1,Msg1), !.
keur2tekst(_,_,geaccepteerd, "accept", "speler is geaccepteerd", color_green) :- !.
keur2tekst(_,_,nietgeaccepteerd, "niet geacc", "speler is niet geaccepteerd", color_cyan) :- !.
keur2tekst(_,_,reserve, "reserve", "speler is reserve", color_LtGray) :- !.
keur2tekst(Cat,TL,_, "L !","", color_cyan) :-
  findall(Msg, lokaalProbleem(Cat,TL,Msg), MList),
  not(MList = []), !.
  %list_to_string(Mlist,"\n",Tekst), !. wordt bij details aangevuld
keur2tekst(_,_,na, "niet gek", "", color_blue) :- 
  toernooiK(_,_), !.
keur2tekst(_,_,_, "","", color_black).

  dlg_plaatsing1_Create(Parent, Num, Cat,Tgnst, Plaats,Ronde,Uitsl,Tijd,LL,NumX ,PlaatsX,RondeX,UitslX,Tijd,LLX,Answer,EersteFocus):-
	assert(selectedOpgave(Num, Cat, EersteFocus)),
	class_Create(date_cc_class,date_cc_class_handlerP),
	int2DialogInt(Plaats, IDCP_PLAATSINGSNO_VALUE), 
	IDCP_STATUS_VALUE = LL,
	IDCP_UITSLUITEN_CHECKED = Uitsl,
	LISTBUT_SELECT = Ronde,
	opgNamen(Tgnst, IDCTP_NAAM1_TITLE, IDCTP_SPST1TXT, IDCTP_RANKING1TXT,
	                IDCP_NAAM2_TITLE, IDCTP_SPST2TXT, IDCTP_RANKING2TXT),
%MARK Plaatsing1, new variables

	dialog_Create(Parent,
		[
%BEGIN Plaatsing1, WinDefList, 19:41:48-24.1.2014, Code automatically updated!
		 dlg_font(wdef(dlg_plaatsing1_DlgType,dlg_plaatsing1_RCT,dlg_plaatsing1_Title,u_DlgBase),
		 	  dlg_plaatsing1_Font,dlg_plaatsing1_FSize,dlg_plaatsing1_Flags),
		 ctl(wdef(wc_Edit,rct(54,38,68,49),"",u_DlgBase),idcp_plaatsingsno,[wsf_Group,wsf_TabStop,wsf_AutoHScroll,wsf_AlignLeft]),
		 ctl(wdef(wc_CheckBox,rct(54,51,78,61),"",u_DlgBase),idcp_uitsluiten,[wsf_Auto,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_LBoxButton,rct(54,64,77,146),"",u_DlgBase),listbut,[wsf_Group,wsf_TabStop,wsf_VScroll]),
		 ctl(wdef(wc_Edit,rct(54,78,86,89),"ST",u_DlgBase),idcp_status,[wsf_Group,wsf_TabStop,wsf_AutoHScroll,wsf_AlignLeft]),
		 customctl(wdef(wc_Custom,rct(47,138,114,148),"Boekdatum",u_DlgBase),"date_cc_class",idc_boekdatum,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(323,134,355,149),"&OK",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(245,136,277,146),"&Cancel",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(283,136,315,146),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_Edit,rct(89,45,356,113),"",u_DlgBase),idcp_edit,[wsf_AlignLeft,wsf_Group,wsf_TabStop,wsf_AutoHScroll,wsf_MultiLine,wsf_WantReturn]),
		 ctl(wdef(wc_Text,rct(3,13,127,23),"Naam1",u_DlgBase),idctp_naam1,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(3,24,127,34),"Naam2",u_DlgBase),idcp_naam2,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(3,38,40,48),"Plaatsing:",u_DlgBase),idctp_plaatsing,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(3,65,53,75),"Direct in ronde:",u_DlgBase),idctp_direct_in_ronde,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(3,138,44,148),"Boektijdstip:",u_DlgBase),idct_boekdatum,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(116,138,202,148),"(gebruik de pijltjestoetsen)",u_DlgBase),idctp_aanwijzng,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(4,79,35,89),"Status:",u_DlgBase),idctp_status,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(11,88,59,98),"(WC, LL, etc.)",u_DlgBase),idctp_atoernooien,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(136,13,153,23),"Spst1",u_DlgBase),idctp_spst1,[wsf_AlignCenter]),
		 ctl(wdef(wc_Text,rct(166,13,214,23),"Ranking1",u_DlgBase),idctp_ranking1,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(136,24,152,34),"Spst2",u_DlgBase),idctp_spst2,[wsf_AlignCenter]),
		 ctl(wdef(wc_Text,rct(166,24,214,34),"Ranking2",u_DlgBase),idctp_ranking2,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(89,35,285,45),"Meldingen na de inschrijfcheck:",u_DlgBase),idctp_opmerkingen_inschrijfcheck,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(4,2,125,12),"Naam",u_DlgBase),idctp_naam,[wsf_AlignCenter]),
		 ctl(wdef(wc_Text,rct(127,2,204,12),"Speelsterkte Rating",u_DlgBase),idctp_speelsterkte,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(3,51,45,61),"Uitsluiten:",u_DlgBase),idctp_uitsluiten,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(90,113,324,124),"NB Sommige meldingen kunnen verdwijnen als u nog een check doet!",u_DlgBase),idctp_static_text,[wsf_AlignLeft])
%END Plaatsing1, WinDefList
		],
  		[
%BEGIN Plaatsing1, ControlList, 19:41:48-24.1.2014, Code automatically updated!
		df(idcp_plaatsingsno,editint(IDCP_PLAATSINGSNO_VALUE,[maximum(99)]),nopr),
		df(idcp_uitsluiten,checkbox(IDCP_UITSLUITEN_CHECKED),nopr),
		df(listbut,listbutton(["1","2","3"],LISTBUT_SELECT),nopr),
		df(idcp_status,editstr(IDCP_STATUS_VALUE,[]),nopr),
		df(idcp_edit,editstr("",[]),nopr),
		df(idctp_naam1,statictext(IDCTP_NAAM1_TITLE),nopr),
		df(idcp_naam2,statictext(IDCP_NAAM2_TITLE),nopr),
		df(idctp_spst1,statictext(IDCTP_SPST1TXT),nopr),
		df(idctp_ranking1,statictext(IDCTP_RANKING1TXT),nopr),
		df(idctp_spst2,statictext(IDCTP_SPST2TXT),nopr),
		df(idctp_ranking2,statictext(IDCTP_RANKING2TXT),nopr)
%END Plaatsing1, ControlList
		],
		dlg_plaatsing1_eh,0,VALLIST,ANSWER),
	dlg_plaatsing1_handle_answer(ANSWER,VALLIST,Num,Cat,NumX ,PlaatsX,RondeX,UitslX,_TijdX,LLX),
	class_destroy(date_cc_class),
	retractall(selectedOpgave(_, _, _)),
	!.

  dlg_plaatsing1_handle_answer(idc_ok,_VALLIST,Num,Cat,NumX ,PlaatsX,RondeX,UitslX,TijdX,LLX):-!,
	dlg_plaatsing1_update(_VALLIST,Num,Cat,NumX ,PlaatsX,RondeX,UitslX,TijdX,LLX).
  dlg_plaatsing1_handle_answer(idc_cancel,_,_,_,0 ,0,0, 0, 0, ""):-!.  % Handle Esc and Cancel here
  dlg_plaatsing1_handle_answer(_,_,_,_,_,_,_,_,_,_):-
	errorexit().

  dlg_plaatsing1_update(_VALLIST,_Num,_Cat,NumNw ,PlNew,_LISTBUT_SELECT,_IDCP_UITSLUITEN_CHECKED,0,_IDCP_STATUS_VALUE):-
%BEGIN Plaatsing1, Update controls, 19:41:48-24.1.2014, Code automatically updated!
	_IDCP_PLAATSINGSNO_VALUE = dialog_VLGetint(idcp_plaatsingsno,_VALLIST),
	_IDCP_UITSLUITEN_CHECKED = dialog_VLGetCheck(idcp_uitsluiten,_VALLIST),
	dialog_VLGetListButton(listbut,_VALLIST,_LISTBUT_ITEMLIST,_LISTBUT_SELECT),
	_IDCP_STATUS_VALUE = dialog_VLGetstr(idcp_status,_VALLIST),
	_IDCP_EDIT_VALUE = dialog_VLGetstr(idcp_edit,_VALLIST),
%END Plaatsing1, Update controls
	int2dialogInt(PlNew,_IDCP_PLAATSINGSNO_VALUE),
	selectedOpgave(NumNw, _Cat, _),
	true.

%MARK Plaatsing1, new events


%BEGIN Plaatsing1, e_User
  dlg_plaatsing1_eh(_Win,e_User(88,_Ptr),0):-
	selectedOpgave(_NumNw, _Cat, 1),
	BWin = win_GetCtlHandle(_Win, idc_boekdatum),
	win_SetFocus(BWin),
	!.
%END Plaatsing1, e_User

%BEGIN Plaatsing1, idc_ok _CtlInfo
  dlg_plaatsing1_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	trap(DateCCWin = win_GetCtlHandle(_Win,idc_boekdatum),_,fail),
	MinOffset = date_cc_GetValue(DateCCWin),
	opgaveTijdstipUniek(MinOffset),
	fail.
%END Plaatsing1, idc_ok _CtlInfo

%BEGIN Plaatsing1, idc_help _CtlInfo
  dlg_plaatsing1_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Opgavedetails"),
	!.
%END Plaatsing1, idc_help _CtlInfo

%BEGIN Plaatsing1, idcp_plaatsingsno modified
  dlg_plaatsing1_eh(_Win,e_Control(idcp_plaatsingsno,_CtrlType,PltsWin,modified),0):-!,
	dialog_SetState(_Win, [enable(listbut,b_False)]),
	Text = win_GetText(PltsWin),
	trim_c(Text),
	Text <> "",
	dialog_SetState(_Win, [enable(listbut,b_True)]),
	!.
%END Plaatsing1, idcp_plaatsingsno modified

%BEGIN Plaatsing1, e_Create
  dlg_plaatsing1_eh(_Win,e_Create(_CreationData),0):-
	selectedOpgave(Num, Cat, _EersteFocus),
	opg(Num,Cat,Tgnst,_,_,_,_,_,_TeamId,Keur,_),
	findall(Numx, tgnstnum(Tgnst,Numx), NumxList),
        keur2Tekst(Cat,NumxList,Keur, _, KeurS, _KeurKleur),
        findall(Msg, lokaalProbleem(Cat,NumxList,Msg), MList),
	list_to_string(Mlist,"\n",Tekst), 
	format(KeurSS, "%s\n%s",KeurS,Tekst),
	zeefblanks(KeurSS,10,KeurSS1),
	KWin = win_GetCtlHandle(_Win, idcp_edit),
	win_SetText(KWin, KeurSS1),
	fail.
  dlg_plaatsing1_eh(_Win,e_Create(_CreationData),0):-
	dialog_SetState(_Win, [enable(listbut,b_False)]),
	win_PostEvent(_Win,e_user(88,0)),		% focus boekdatum
	PltsWin = win_GetCtlHandle(_Win, idcp_plaatsingsno),
	Text = win_GetText(PltsWin),
	trim_c(Text),
	Text <> "",
	dialog_SetState(_Win, [enable(listbut,b_True)]),
	!.
%END Plaatsing1, e_Create

  dlg_plaatsing1_eh(_,_,_):-!,fail.

%END_DLG Plaatsing1


%BEGIN_DLG Uitslaginvoer1
/**************************************************************************
	Creation and event handling for dialog: Uitslaginvoer1
**************************************************************************/

constants

%BEGIN Uitslaginvoer1, CreateParms, 16:35:45-16.11.2016, Code automatically updated!
  dlg_uitslaginvoer1_DlgType = wd_Modal
  dlg_uitslaginvoer1_Title = "Uitslaginvoer"
  dlg_uitslaginvoer1_RCT = rct(40,32,281,189)
  dlg_uitslaginvoer1_Flags = [wsf_Close,wsf_TitleBar]
  dlg_uitslaginvoer1_Help = idh_uitslagen_invoeren_f8
  dlg_uitslaginvoer1_Font = "Arial"
  dlg_uitslaginvoer1_FSize = 10
%END Uitslaginvoer1, CreateParms
u_links = 0
u_rechts = 1
setfocus = 22

database - uitslag
single verliezergezet(boolean)	% verliezer gaat door al automatisch gezet
single eersteKeer(boolean)
single metAanwezigheidsRegistratie(boolean)

predicates

  dlg_uitslaginvoer1_eh : EHANDLER
  date_cc_class_handler : EHANDLER
  enableGelijkspel1(WINDOW, rsoort)
  enableOK(WINDOW)		% main entry point
  enableOK(WINDOW, string)
  zetGew(integer OE, janee, janee, janee, janee)
  %procedure verwijderPlanning(integer, wedscat, tijd, janee, janee, tgnst) - (i,i,o,o,o,i)
  verwijderUitslag(integer, wedscat)
  verwerk(WINDOW, integer, wedscat) - (i,i,i)
  uitsl_v11(integer,wedscat,tgnst,tgnst,integer,tijd,string)
  eerste_wedstrijd(wedscat, integer, integer)
  uitsl_numeriek(string)
  geen_wedstrijd(string,tgnst,tgnst)
  volg1(integer,wedscat,tgnst)
  volg1(integer,wedscat,integer,tgnst)
  uitslag_volg(integer,wedscat)
  determ  volg1Extern(integer,wedscat,tgnst) - (i,i,i)
  determ  volg1Extern(integer,wedscat,integer,tgnst) - (i,i,i,i)
  nondeterm plaats(wedscat, integer Card, integer Num)
  openPlaats(wedscat Verliezerscat, integer GevondenLR, integer Gevonden, string Naam)
  procedure uitslagWO(janee Oproepen, string Uitsl, integer Num, wedscat Cat, tijd, tgnst Winnaar, boolean WalkOver) -
            (i,i,i,i,i,i,i)
  procedure uitslagWO(integer Num, wedscat Cat, tijd, tgnst Winnaar)
  volgkoppeling(WINDOW, wedscat, vandom)
procedure boekdatum(WINDOW, MINUT_OFFSET)
procedure enablePunten(window, integer, rsoort) - (i, i, i)
  uitslagverw(window Win, integer Num, wedscat Cat, string Uitsl)
  enableOngedaan(window Win, integer Num, wedscat Cat)
  determ winnende(window, tgnst T1, tgnst T2, tgnst Win, Tgnst Verl, integer Vorige)
  procedure iniUitslForm(window Win, integer No, wedscat Cat, tgnst T, tgnst T2)
  procedure getStatus(window Win, uitslstat, wedscat)
  procedure opgevenAls(wedscat VerlCat, tgnst Verliezer, teamstatus)
  procedure opgesteldAls(tgnst Verliezer, wedscat VerlCat)
  procedure afgebrokenOngedaan(window, string Uitslag)
  procedure zetpunten(window Win, boolean EersteKeer)
  procedure zetPunten(window Win, integer Pouletelling, string P1, string P2)
  determ puntenNodig(window)
  procedure verwijder(string, string Uitsl, string)
  determ eenWedstrijdNietKlaar()
  procedure disableCheckbox(window Win, integer PT, integer EenTwee)
  procedure getStatusPouleMessage(wedscat Cat)
  procedure skiptoken1(string In, string Out)
  procedure setsplusmin(string In, integer SetsIn, integer SetsUit, boolean Verkort)
  determ isUitslagNietPlausibel(window UitslWin, wedscat, integer SetsaldoWinnaar, boolean Verkort)
  procedure setIsVerkort(integer A, boolean Verkort)
  determ eenonbekende(tgnst, tgnst)
  procedure woInvoeren(integer OK, integer, wedscat)
  procedure woOngedaan(integer OK, integer, wedscat)
  procedure boekdag(integer, wedscat, integer)
  procedure verwijderPlanningWO(integer Num, wedscat Cat, integer LinksRechts)
  procedure alTweePuntenToegekend(string)
clauses

verliezergezet(b_False).
eersteKeer(b_True).
metAanwezigheidsRegistratie(b_True).

%+++++++++++++++++++++++ eigenlijk 'silent' uitslagverwerking +++++++++++++++++++++++++++++++++++++++++
uitslag_volgExtern(_, Cat) :-
  pos(Cat, _, _, poel), !.
uitslag_volgExtern(No,Cat) :-      
  w3(No,Cat,WinNo,_, _, _, _), !,	% er is een winnaar
  volg1Extern(No,Cat,WinNo).
uitslag_volgExtern(No,Cat) :-		% allebei zijn bye
  wd(No,Cat,bye,bye,_,_,_), !,
  volg1Extern(No,Cat,bye).
uitslag_volgExtern(No,Cat) :-		% één van de twee is bye
  wd(No,Cat,bye,WinNo,_,_,_), !,
  volg1Extern(No,Cat,WinNo).
uitslag_volgExtern(No,Cat) :-		% de ander is bye
  wd(No,Cat,WinNo,bye,_,_,_), !,
  volg1Extern(No,Cat,WinNo).
uitslag_volgExtern(No,Cat) :-
  wd(No,Cat,_,_,_,_,_), !,
  volg1Extern(No,Cat,onbekend).

volg1Extern(0,_,_,_) :- !.                    /* voor de finale is er geen */
volg1Extern(No,Cat,1,Winnaar) :-              /* volgende ronde !          */
  wd(No,Cat,_,Winnaar,_,_,_), !.          /* al ingevuld               */  
volg1Extern(No,Cat,0,Winnaar) :-              /* volgende ronde !          */
  wd(No,Cat,Winnaar,_,_,_,_), !.          /* al ingevuld               */  
%---------------------------------------------------------------------------
volg1Extern(No,Cat,1,Winnaar) :-              /* volgende ronde !          */
  wd(No,Cat,Opg1,_,Tekst,_,Nota),
  w3(No,Cat,onbekend,_, _,Stat, Sta), !,	% andere heeft al opgegeven
  retractall(wd(No, Cat, _,_,_,_,_)),
  assertZ(wd(No,Cat,Opg1,Winnaar,Tekst,Sta,Nota)), 
%  Term1 = wd(No,Cat,Opg1,Winnaar,Tekst,Sta,Nota),
%  write("\nZ1 ", Term1),
  retractall(w3(No,Cat,_, _, _,_, _)),
  assertZ(w3(No,Cat,Winnaar, 0, 0,Stat, Sta)),
%  Term2 = w3(No,Cat,Winnaar, 0, 0,Stat, Sta),
%  write("\nZ2 ", Term2),
  %verwijderPlanningExtern(No, Cat, _, _, _, onbekend),
  select(Cat, No),
  uitslag_volgExtern(No,Cat).                /* ga door */
volg1Extern(No,Cat,0,Winnaar) :-              /* volgende ronde !          */
  wd(No,Cat,_,Opg1,Tekst,_,Nota),
  w3(No,Cat,onbekend,_, _,Stat, Sta), !,	% andere heeft al opgegeven
  retractall(wd(No, Cat, _,_,_,_,_)),
  assertZ(wd(No,Cat,Winnaar,Opg1,Tekst,Sta,Nota)), 
%  Term1 = wd(No,Cat,Winnaar,Opg1,Tekst,Sta,Nota),
%  write("\nZ3 ", Term1),
  retractall(w3(No,Cat,_, _, _,_, _)),
  assertZ(w3(No,Cat,Winnaar, 0, 0,Stat, Sta)),
%  Term2 = w3(No,Cat,Winnaar, 0, 0,Stat, Sta),
%  write("\nZ4 ", Term2),
  %verwijderPlanningExtern(No, Cat, _, _, _, onbekend),
  select(Cat, No),
  uitslag_volgExtern(No,Cat).                /* ga door */
%------------------------------------------------------------------------ 11.7.2006
volg1Extern(No,Cat,1,Winnaar) :-              /* volgende ronde !          */
  wd(No,Cat,Opg1,_,_,_,Nota),
  retractall(wd(No, Cat, _,_,_,_,_)),
  assertZ(wd(No,Cat,Opg1,Winnaar,"",0,Nota)), !,
%  Term1 = wd(No,Cat,Opg1,Winnaar,"",0,Nota),
%  write("\nZ5 ", Term1),
  select(Cat, No),
  uitslag_volgExtern(No,Cat).                /* ga door */
volg1Extern(No,Cat,0,Winnaar) :-
  wd(No,Cat,_,Opg1,_,_,Nota),
  retractall(wd(No, Cat, _,_,_,_,_)),
  assertZ(wd(No,Cat,Winnaar,Opg1,"",0,Nota)), !,
%  Term1 = wd(No,Cat,Winnaar,Opg1,"",0,Nota),
%  write("\nZ6 ", Term1),
  select(Cat, No),
  uitslag_volgExtern(No,Cat).
volg1Extern(No,Cat,1,Winnaar) :-
  !, 
  retractall(wd(No, Cat, _,_,_,_,_)),
  assertZ(wd(No,Cat,onbekend,Winnaar,"",0,"")),
%  Term1 = wd(No,Cat,onbekend,Winnaar,"",0,""),
%  write("\nZ7 ", Term1),
  select(Cat, No),
  uitslag_volgExtern(No,Cat).
volg1Extern(No,Cat,0,Winnaar) :-
  !, 
  retractall(wd(No, Cat, _,_,_,_,_)),
  assertZ(wd(No,Cat,Winnaar,onbekend,"",0,"")),
%  Term2 = wd(No,Cat,Winnaar,onbekend,"",0,""),
%  write("\nZ8 ", Term2),
  select(Cat, No),
  uitslag_volgExtern(No,Cat).

volg1Extern(No, Cat, Tgnst) :-
  %write("\nVolgextern volgende ronde", Tgnst),
  bitright(No,1,VolgNo),
  bitand(No,1,A),
  volg1Extern(VolgNo,Cat,A,Tgnst).

eenWedstrijdNietKlaar() :-
  wd(Num1,Cat,T1,T2,_U,_,_),
  not(w3(Num1, Cat, _, _, _, _, _)),
  not(T1 = bye),
  not(T2 = bye), !.


%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
uitslag_volg(_, _) :-
  toernooiK(_,Type),
  not(Type = nrt),
  not(eenWedstrijdNietKlaar()),
  Msg = "Laatste uitslag ingevoerd! Stuur een export naar de KNLTB!\nZie Toernooi, Rapporteer einduitslag, Uitvoer voor de KNLTB.\n\nToernooiklapper wordt eerst updated.",
  dlg_MessageBox( "==> You made it! <==", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  compress(replace),
  startDBrefresh(),
  TaskWin = vpi_GetTaskWin(),
  win_PostEvent(TaskWin,e_menu(id_Onderdeel_Webpaginas_uploaden,0)),
  fail.
uitslag_volg(_, _) :-
  not(toernooiK(_, _)),
  not(eenWedstrijdNietKlaar()),
  Msg = "Laatste uitslag ingevoerd!\n\nUpdate het Web!",
  dlg_MessageBox( "You made it!", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  compress(replace),
  startDBrefresh(),
  TaskWin = vpi_GetTaskWin(),
  win_PostEvent(TaskWin,e_menu(id_Onderdeel_Webpaginas_uploaden,0)),
  fail.
uitslag_volg(_, Cat) :-
  pos(Cat, _, _, poel), !.
uitslag_volg(No,Cat) :-      
  w3(No,Cat,WinNo,_, _, _, _), !,	% er is een winnaar
  volg1(No,Cat,WinNo).
uitslag_volg(No,Cat) :-		% allebei zijn bye
  wd(No,Cat,bye,bye,_,_,_), !,
  volg1(No,Cat,bye).
uitslag_volg(No,Cat) :-		% één van de twee is bye
  wd(No,Cat,bye,WinNo,_,_,_), !,
  volg1(No,Cat,WinNo).
uitslag_volg(No,Cat) :-		% de ander is bye
  wd(No,Cat,WinNo,bye,_,_,_), !,
  volg1(No,Cat,WinNo).
uitslag_volg(No,Cat) :-
  wd(No,Cat,_,_,_,_,_), !,
  volg1(No,Cat,onbekend).

volg1(No, Cat, Tgnst) :-
  bitright(No,1,VolgNo),
  bitand(No,1,A),
  volg1(VolgNo,Cat,A,Tgnst).

verwijderPlanningWO(Num, Cat, _LR) :-
  ws(Num,Cat,T,Tijd),
  logf('R',ws(Num,Cat,T,Tijd),ja),
  fail.
verwijderPlanningWO(Num, Cat, LR) :-
  w2(Num,Cat,Tijd,G1,G2,Fix,Spoor, BNo),
  logf('R',w2(Num,Cat,Tijd,G1,G2,Fix,Spoor, BNo),ja),
  bitleft(Num, 1, Num1),
  Num2 = Num1 + LR,
  wd(Num2,Cat,TX1,TX2,_Tekst,_,_),
  1 = dlg_MessageBox( "Informeren", "Spelers van de voorgaande ronde informeren?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
  logf('Z',ws(Num,Cat,TX1,Tijd),ja),
  logf('Z',ws(Num,Cat,TX2,Tijd),ja), !.
verwijderPlanningWO(_Num, _Cat, _).

volg1(0,_,_,_) :- !.                    /* voor de finale is er geen */
volg1(No,Cat,1,Winnaar) :-              /* volgende ronde !          */
  wd(No,Cat,_,Winnaar,_,_,_), !.          /* al ingevuld               */  
volg1(No,Cat,0,Winnaar) :-              /* volgende ronde !          */
  wd(No,Cat,Winnaar,_,_,_,_), !.          /* al ingevuld               */  
%---------------------------------------------------------------------------
volg1(No,Cat,1,Winnaar) :-              /* volgende ronde !          */
  wd(No,Cat,Opg1,_,Tekst,_,Nota),
  w3(No,Cat,onbekend,_, _,Stat, Sta), !,	% andere heeft al opgegeven
  logf('Z',wd(No,Cat,Opg1,Winnaar,Tekst,Sta,Nota),nee), 
  logf('Z', w3(No,Cat,Winnaar, 0, 0,Stat, Sta), nee),
  verwijderPlanning(No, Cat, _, _, _, onbekend),
  select(Cat, No),
  uitslag_volg(No,Cat).                /* ga door */
volg1(No,Cat,0,Winnaar) :-              /* volgende ronde !          */
  wd(No,Cat,_,Opg1,Tekst,_,Nota),
  w3(No,Cat,onbekend,_, _,Stat, Sta), !,	% andere heeft al opgegeven
  logf('Z',wd(No,Cat,Winnaar,Opg1,Tekst,Sta,Nota),nee), 
  logf('Z', w3(No,Cat,Winnaar, 0, 0,Stat, Sta), nee),
  verwijderPlanning(No, Cat, _, _, _, onbekend),
  select(Cat, No),
  uitslag_volg(No,Cat).                /* ga door */
%------------------------------------------------------------------------ 11.7.2006
volg1(No,Cat,1,Winnaar) :-              /* volgende ronde !          */
  wd(No,Cat,Opg1,_,_,_,Nota),
  logf('Z',wd(No,Cat,Opg1,Winnaar,"",0,Nota),nee), !,
  select(Cat, No),
  uitslag_volg(No,Cat).                /* ga door */
volg1(No,Cat,0,Winnaar) :-
  wd(No,Cat,_,Opg1,_,_,Nota),
  logf('Z',wd(No,Cat,Winnaar,Opg1,"",0,Nota),nee), !,
  select(Cat, No),
  uitslag_volg(No,Cat).
volg1(No,Cat,1,Winnaar) :-
  !, 
  logf('Z',wd(No,Cat,onbekend,Winnaar,"",0,""),nee),
  select(Cat, No),
  uitslag_volg(No,Cat).
volg1(No,Cat,0,Winnaar) :-
  !, 
  logf('Z',wd(No,Cat,Winnaar,onbekend,"",0,""),nee),
  select(Cat, No),
  uitslag_volg(No,Cat).

uitsl_numeriek(NUitsl) :-
  fronttoken(NUitsl, Token,_),
  not(str_int(Token, _)), !, fail.
uitsl_numeriek(_).

plaats(Cat, Card, Num) :-
  wd(No,Cat,onbekend,_,_,_,_), 
  No >= Card,
  Num = 2 * No.
plaats(Cat, Card, Num) :-
  wd(No,Cat,_,onbekend,_,_,_), 
  No >= Card,
  Num = 2 * No + 1.

openPlaats(VerlCat, _LR, _Num1, Naam) :-
  not(pos(VerlCat, _, _, _)),
  wc(VerlCat, _, _, VCatNaam, _, _, _, _, _, _, _, _, _, _, _),
  %wc(VerlCat,VCatNaam,_,_,_,_,_), 
  format(Msg, "%s\nGenoteerd voor verliezersronde.", Naam),
  dlg_MessageBox( VCatNaam, Msg, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.
openPlaats(Cat, LR, Num1, _) :-
  pos(Cat, Card, _, afval),
  findall(Num, plaats(Cat, Card, Num), NList),
  not(NList = []),
  sortint_c(NList),
  NList = [Num|_], !,
  bitright(Num, 1, Num1),
  bitand(Num, 1, LR0),
  LR = LR0 + 1.
openPlaats(Cat, 1, 1, Naam) :-
  pos(Cat, _Card, _, afval),
  wc(Cat, _, _, VCatNaam, _, _, _, _, _, _, _, _, _, _, _),
  %wc(Cat,VCatNaam,_,_,_,_,_), 
  format(Msg, "Geen open plaatsen.\n%s wel genoteerd (candidaat).", Naam),
  dlg_MessageBox( VCatNaam, Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.
   
uitsl_v11(_No,Cat,_VerlVorig,Verli,_,_,_) :-  
  kopl(VerlCat, verliezer, Cat, _),
  %wc(Cat,_,_,_,_,_,VerlCat),			% nu de winnaar, misschien eerst verl
  in_wedstrijd(VerlCat, Verli),
  spTelPlus(Verli, _, Naam),
  format(Msg, "%s\nAl in verliezersronde!", Naam),
  dlg_MessageBox( "", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), !.
uitsl_v11(_No,Cat,VerlVorig,_Verli,_,_,_) :-  
  %wc(Cat,_,_,_,_,_,VerlCat),			% nu de winnaar, misschien eerst verl
  kopl(VerlCat, verliezer, Cat, _),
  not(in_wedstrijd(Cat, Verlvorig)),
  opg(Num,VerlCat,VerlVorig,A,B,C,D,LL,TID,Stat,MT),		% en in verliezersronde
  logf('R',opg(Num,VerlCat,VerlVorig,A,B,C,D,LL,TID,Stat,MT),ja),
  fail.
uitsl_v11(_No,Cat,VerlVorig,_,_,_,_) :-  
  kopl(VerlCat, verliezer, Cat, _),
  %wc(Cat,_,_,_,_,_,VerlCat),			% nu de winnaar, misschien eerst verl
  wc(VerlCat, _, _, CatNaam, _, _, _, _, _, _, _, _, _, _, _),
  %wc(VerlCat,CatNaam,_,_,_,_,_),		% nu de winnaar, misschien eerst verl
  in_wedstrijd(VerlCat, Verlvorig),
  spTelPlus(VerlVorig, _, Naam),
  format(Msg, "%s\nZelf met Indelen, Wijzig opstelling corrigeren!", Naam),
  dlg_MessageBox( CatNaam, Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.
uitsl_v11(No,Cat,_TgnSt,Verliezer,Vorige,_LogTijd,NUitsl) :-
  pos(Cat, _, _, afval),			% 3.3.2002 alleen voor afval.
  bitleft(No, 1, No1),				% voor de zekerheid ook de voorafgaande weds...
  verwijderPlanning(No1, Cat, _, _, _, onbekend),
  No2 = No1 + 1,
  verwijderPlanning(No2, Cat, _, _, _, onbekend),
  kopl(VerlCat, verliezer, Cat, _),
  uitsl_numeriek(Nuitsl),
  loglock(mdi, "uitsl_v11"),
  eerste_wedstrijd(Cat, No, Vorige),
  opg(_,Cat,Verliezer,_,_,_,_,_,_,Keur,_),
  opgevenAls(VerlCat, Verliezer,Keur),	% 18.8.2015 neem de keur over
  opgesteldAls(Verliezer, VerlCat),
  fail.
uitsl_v11(_,_,_,_,_,_,_) :-
  logclose().

opgevenAls(VerlCat, Verliezer, Keur) :-
  not(opg(_,VerlCat,Verliezer,_,_,_,_,_,_,_,_)),	% staat niet al opgegeven!
  date(Year,Month,Day),
  time(Hours,Minutes,_Seconds,_Hundredths),
  dt_min_to_offset(Year, Month, Day, Hours, Minutes, Numr),
  getbacktrack(Here),
  numb(N),     
  OpgN = Numr + N - 1,
  not(opg(OpgN,VerlCat,_,_,_,_,_,_,_,_,_)),
  cutbacktrack(Here),
  logf('Z',opg(OpgN,VerlCat,Verliezer,0,0,0,0,"","",Keur,""),nee), !.
opgevenAls(_VerlCat, _Verliezer, _).

opgesteldAls(Verliezer, VerlCat) :-
  spTelPlus(Verliezer, _, Naam),
  openPlaats(VerlCat, LR, Num, Naam), 
  upd_wd(LR, Num, VerlCat, Verliezer, afval), 
  update_display(), !,
  logclose(),
  format(Msg, "%s\nOpgesteld in de verliezersronde.", Naam),
  dlg_MessageBox( "", Msg, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ).
opgesteldAls(_Verliezer, _VerlCat).

eerste_wedstrijd(Cat, No, Vorige) :-
  bitleft(No,1,No1),
  No2 = No1 + Vorige,
  wd(No2,Cat,A,B,Uitsl,_,_), !,	% als niet bestaat is het goed
  geen_wedstrijd(Uitsl,A,B).	% als hij wel bestaat moet het een 'onechte'
eerste_wedstrijd(_, _, _).	% zijn

geen_wedstrijd(_,bye,_) :- !.
geen_wedstrijd(_,_,bye) :- !.
geen_wedstrijd(Uitsl, _, _) :-
  not(uitsl_numeriek(Uitsl)).

nieuwPlan(nee,  _Win, _Num, _Cat):- !. 		% nee
nieuwPlan(ja,  _Win, Num, Cat) :- 		% niet planbaar
  wd(Num,Cat,bye,_,_,_,_), !.
nieuwPlan(ja,  _Win, Num, Cat) :- 		% niet planbaar
  wd(Num,Cat,_,bye,_,_,_), !.
nieuwPlan(_,  _Win, Num, Cat) :- 		% al plan
  w2(Num,Cat,_Tijd,_G1,_G2,_Fix,_Spoor, _BNo), !.
nieuwplan(_, _, _, _) :-
  planningBezig,
  dlg_MessageBox( "Planning informatie", "Er is al een planning bezig! Kan niet twee tegelijk.\nDaarom wordt deze te plannen wedstrijd toegevoegd\naan het 'te plannen'-scherm (P_).", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), !.
nieuwPlan(ja,  _Win, Num, _Cat) :- 
  Num < 4,					% (halve) finale, misschien niet
  A = dlg_MessageBox( "Doorplannen", "De vervolgpartij is een (halve) finale.\nPlannen?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
  A <> 1, !.
nieuwPlan(ja, _Win, Num, Cat) :-			% wèl plannen
  planMetAgenda(Num, Cat).

koplUitgevoerd(Cat, plaats(Regel), Nieuwe) :-		% global
  bitright(Regel, 1, P1),			% bij een afval
  bitand(Regel, 1, H1),
  wd(P1,Cat,T1,T2,_,_,_),
  lr(Nieuwe, T1, T2, H1),
  not(Nieuwe = onbekend), !.  
koplUitgevoerd(_Cat, ipv(Tgnst), Tgnst).	% bij een poule

nr2tekst(rang(N), Uit) :-
  str_int(Uit, N), !.
nr2tekst(pl3en4(_), "3/4").

volgkoppeling(_Win, Cat, rang(1)) :-
  wc(Cat, _, _, _, _, _, _, _SpeelSterkte, _, _, _, _, _, Tid, _),
  toernooiK(Tid, nrt),
  kopl(Cat, algemeen, CatEind, _),
  vanPoelAfval(afval, Cat, rang(1), T1, _, NK),	% de aangewezen winnaar of runner-up als die er al is
  NK = "",					% NK = niet klaar
  not(opg(_,CatEind,T1,_,_,_,_,_,_,_,_)),
  wc(CatEind, _, _, NaamEind, _, _, _, _, _, _, _, _, _, _, _),
  %wc(CatEind,NaamEind,_,_,_,_,_),			% door naar verliezersronde 
  repc(nee,' ',nee,nee,nee,0,0,1,0,Cat,T1,alleplanning,Naam1,_,_Log,_,_,_,_),% en bepaal de tekst ervoor 
  format(Msg, "%s\n wordt als kwalifier opgegeven voor %s!", Naam1, NaamEind),
  dlg_MessageBox( "Kwalifier", Msg, mesbox_iconinformation, mesbox_buttonsOK, mesbox_defaultfirst, mesbox_suspendapplication ),
  date(Year,Month,Day),
  time(Hours,Minutes,_Seconds,_Hundredths),
  dt_min_to_offset(Year, Month, Day, Hours, Minutes, Numr),
  getbacktrack(Here),
  numb(N),     
  OpgN = Numr + N - 1,
  not(opg(OpgN,CatEind,_,_,_,_,_,_,_,_,_)),
  cutbacktrack(Here),
  logf('Z',opg(OpgN,CatEind,T1,0,0,0,0,"KW","",na,""),nee), !.
volgkoppeling(Win, Cat, Nr) :-
  kopl(Cat, Nr, CatNo1, plaats(PosUit)),
  not(koplUitgevoerd(CatNo1, plaats(PosUit),_)),
  pos(Cat, _, _, Type),
  wc(CatNo1, _, _, NaamV, _, _, _, _, _, _, _, _, _, _, _),
  %wc(CatNo1,NaamV,_,_,_,_,_),			% door naar verliezersronde 
  vanPoelAfval(Type, Cat, Nr, T1, _, NK),	% de aangewezen winnaar of runner-up als die er al is
  NK = "",					% NK = niet klaar
  repc(nee,' ',nee,nee,nee,0,0,1,0,Cat,T1,alleplanning,Naam1,_,_Log,_,_,_,_),% en bepaal de tekst ervoor 
  nr2tekst(Nr, NrTekst),
  format(Msg, "%s\nis/zijn nr %s geworden?\nDoor naar %s?\n\nNB dat kan ongedaan gemaakt worden\nmet het 'koppelingen' icoontje.", Naam1, NrTekst,NaamV),
  1 = dlg_MessageBox( "Plaatsen in eindronde of 3/4 plaats", Msg, mesbox_iconquestion, mesbox_buttonsYesNo, mesbox_defaultfirst, mesbox_suspendapplication ),
  koppelingEchtUitvoeren(Win, Cat, CatNo1, PosUit, T1, onbekend),
  !.
% --------------
volgkoppeling(Win, Cat, Nr) :-
  kopl(Cat, Nr, CatNo1, plaats(PosUit)),
  koplUitgevoerd(CatNo1, plaats(PosUit),NTgnst),
  pos(Cat, _, _, Type),
  wc(CatNo1, _, _, NaamV, _, _, _, _, _, _, _, _, _, _, _),
  %wc(CatNo1,NaamV,_,_,_,_,_),			% door naar verliezersronde 
  vanPoelAfval(Type, Cat, Nr, T1, _, NK),	% de aangewezen winnaar of runner-up als die er al is
  NK = "",					% NK = niet klaar
  not(NTgnst = T1),
  repc(nee,' ',nee,nee,nee,0,0,1,0,Cat,T1,alleplanning,Naam1,_,_Log,_,_,_,_),% en bepaal de tekst ervoor 
  repc(nee,' ',nee,nee,nee,0,0,1,0,CatNo1,NTgnst,alleplanning,NaamN,_,_LogN,_,_,_,_),% en bepaal de tekst ervoor 
  %nr2tekst(Nr, NrTekst),
  format(Msg, "%s in plaats van %s doorschuiven naar %s?", Naam1, NaamN, NaamV),
  1 = dlg_MessageBox( "Koppeling volgen", Msg, mesbox_iconquestion, mesbox_buttonsYesNo, mesbox_defaultfirst, mesbox_suspendapplication ),
  koppelingEchtUitvoeren(Win, Cat, CatNo1, PosUit, T1, NTgnst),
  !.
% ---------------
volgkoppeling(_, _Cat, _Nr).

verwerk(Win, 2, Cat) :-
  pos(Cat, _, _, afval), 	% afval
  volgkoppeling(Win, Cat, pl3en4(3)),	% bovenste helft
  fail.
verwerk(Win, 3, Cat) :-
  pos(Cat, _, _, afval), 	% afval
  volgkoppeling(Win, Cat, pl3en4(4)),	% benedenste helft
  fail.
verwerk(Win, 1, Cat) :-
  pos(Cat, _, _, afval), 	% afval
  volgkoppeling(Win, Cat, rang(1)),
  volgkoppeling(Win, Cat, rang(2)), !.
verwerk(Win,_Num, Cat) :-
  pos(Cat, _, _, poel), 	% poule
  volgkoppeling(Win, Cat, rang(1)),
  volgkoppeling(Win, Cat, rang(2)),
  volgkoppeling(Win, Cat, rang(3)),
  volgkoppeling(Win, Cat, rang(4)), !.
verwerk(_Win,Num, Cat) :-
  w2(Num,Cat,_,_,_,_,_,_), !.	% planning is er al
verwerk(Win,Num, Cat) :-
  bitright(Num, 1, Num1),
  wd(Num1,Cat,T1,T2,_U,_,_),
  not(w3(Num1, Cat, _, _, _, _, _)),
  not(T1 = bye),
  not(T2 = bye),
  door_plannen(Doorpl),
  nieuwPlan(Doorpl, Win,Num1,Cat),
  !.
verwerk(_, _Num, _Cat).		% zal bijv poule geweest zijn

verwijderPlanning(Num, Cat, _, _, _, Twin) :-
  ws(Num,Cat,T,Tijd),
  not(T = Twin),				% blijft bestaan om eventueel af te zeggen
  logf('R',ws(Num,Cat,T,Tijd),ja),
  fail.
verwijderPlanning(Num, Cat, Tijd, G1, G2, _) :-
  w2(Num,Cat,Tijd,G1,G2,Fix,Spoor, BNo),
  logf('R',w2(Num,Cat,Tijd,G1,G2,Fix,Spoor, BNo),nee), !.
verwijderPlanning(_Num, _Cat, Tijd, nee, nee, _) :-
  time_stamp_next(Tijd, _, _),		% wilekeurig bestaand tijdstip
  logclose(), !.
verwijderPlanning(_Num, _Cat, 0, nee, nee, _) :-
  logclose().

verwijderUitslag(Num, Cat) :-
  w3(Num, Cat, T2, P1, P2, Status, TT),
  logf('R',w3(Num, Cat, T2, P1, P2, Status, TT),nee), !.
verwijderUitslag(_, _).

zetGew(0, _, G2, ja, G2) :- !.
zetGew(1, G1, _, G1, ja).

vervolgMelding(_, _, _, Num, Cat, _) :-
  not(w2(Num,Cat,_,_,_,_,_,_)),
  !.
vervolgMelding(Parent, Winnaars, ja, Num, Cat, OE) :-
  select(Cat, Num),
  dlg_gewaarschuwd_Create(Parent, Winnaars, Num, Cat, OE), !.
vervolgMelding(_, _, nee, Num, Cat, _) :-
  bitright(Num, 1, Num1),
  bitand(Num, 1, OE),
  baan_adm(BaanAdm),
  repc(nee,' ',BaanAdm,nee,nee,OE,1,2,Num1,Cat,onbekend,alleplanning, Plan,_Score2,_TijdLog2,_,_,_,_),
  format(Tekst, "Vervolg op %s.", Plan),
  dlg_MessageBox( "Volgende ronde", Tekst, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  !.
vervolgMelding(_, _, _, _, _, _).  
	
enableOK(Win) :-
  EF1 = win_GetCtlHandle(Win, idc_uitslag),
  Stand = win_GetText(EF1),
  enableOK(Win, Stand), !.
enableOK(_Win).

enableOK(Win, _Stand) :-		% enable gestaakt en walkover
  AW = win_GetCtlHandle(Win, idct_aanwijzing),
  win_SetText(AW, "(als gezien vanuit de winnaar)"),
  dialog_SetState(Win, [enable(idc_walkover, b_True),show(idc_walkover,b_True),
  	enable(idc_gestaakt, b_True),show(idc_gestaakt,b_True)]),	
  RBG = win_GetCtlHandle(Win, idc_gelijk_spel),
  GC  = win_IsChecked(RBG),
  RBA = win_GetCtlHandle(Win, idc_afgebroken_partij),
  AC = win_IsChecked(RBA),
  AC + GC = 1,
  win_SetText(AW, "(als gezien vanuit bovenste)"),
  CBG = win_GetCtlHandle(Win, idc_gestaakt),
  win_Check(CBG, b_False),
  CBW = win_GetCtlHandle(Win, idc_walkover),
  win_Check(CBW, b_False),
  dialog_SetState(Win, [enable(idc_walkover, b_False),show(idc_walkover,b_False),
  	enable(idc_gestaakt, b_False),show(idc_gestaakt,b_False)]),	
  fail.
enableOK(Win, _Stand) :-		% enable verliezer
  dialog_SetState(Win, [enable(idc_verliezer, b_False),show(idc_verliezer,b_False)]),	% begin met disabelen
  RB1 = win_GetCtlHandle(Win, idc_winnaar1),
  W1C = win_IsChecked(RB1),
  RB2 = win_GetCtlHandle(Win, idc_winnaar2),
  W2C = win_IsChecked(RB2),
  W1C + W2C > 0,
  Vorige = W1C,					% truuk, het gaat om de verliezer!
  uitslWedsId(Win,No,Cat,_,_,_),
  not(w3(No, Cat, _, _, _, _, _)),		% al een keer geweest
  kopl(VerlCat, verliezer, Cat, _),
  %wc(Cat,_,_,_,_,_,VerlCat),
  wc(VerlCat, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
  %wc(VerlCat,_,_,_,_,_,_),			% door naar verliezersronde 
  pos(Cat, _, _, afval),
  eerste_wedstrijd(Cat, No, Vorige),
  dialog_SetState(Win, [enable(idc_verliezer, b_True),show(idc_verliezer,b_True)]),	% begin met enabelen
  verliezergezet(b_False),
  assert(verliezergezet(b_True)),
  VW = win_GetCtlHandle(Win, idc_verliezer),
  win_Check(VW, b_True),
  fail.
enableOK(Win, Stand) :-			% enable OK zelf
  dialog_SetState(Win, [enable(idc_ok, b_True)]),	% begin met enabelen
  fronttoken(Stand, _, _),				% stand ingevuld
  RBG = win_GetCtlHandle(Win, idc_gelijk_spel),
  GC  = win_IsChecked(RBG),
  RB1 = win_GetCtlHandle(Win, idc_winnaar1),
  W1C = win_IsChecked(RB1),
  RB2 = win_GetCtlHandle(Win, idc_winnaar2),
  W2C = win_IsChecked(RB2),
  RBO = win_GetCtlHandle(Win, idc_ongedaan),
  OC = win_IsChecked(RBO),
  RBA = win_GetCtlHandle(Win, idc_afgebroken_partij),
  AC = win_IsChecked(RBA),
  GC + W1C + W2C + OC + AC > 0,	!.			% een selectie gemaakt
enableOK(Win, _Stand) :-			% ook OK enables igv afgebroken partij	7.10.2011
  RBA = win_GetCtlHandle(Win, idc_afgebroken_partij),
  b_True = win_IsChecked(RBA), !.
enableOK(Win, _Stand) :-			% ook OK enables igv afgebroken partij
  RBA = win_GetCtlHandle(Win, idc_walkover),
  b_True = win_IsChecked(RBA), !.	% geen stand nodig	% 25.12.2002
enableOK(Win, _Stand) :-		% en van wissen
  RBA = win_GetCtlHandle(Win, idc_ongedaan),
  b_True = win_IsChecked(RBA), !.
enableOK(Win, _) :-
  dialog_SetState(Win, [enable(idc_ok, b_False)]). 

enableOngedaan(Win, Num, Cat) :-
  w3(Num, Cat, _B, _, _, _, _Tijd),
  dialog_SetState(Win, [enable(idc_ongedaan, b_True)]), !.
enableOngedaan(Win, Num, Cat) :-
  wd(Num, Cat, _B, _, Uitsl, _, _Tijd),
  fronttoken(Uitsl, _, _),
  dialog_SetState(Win, [enable(idc_ongedaan, b_True)]), !.
enableOngedaan(Win, _Num, _Cat) :-
  dialog_SetState(Win, [enable(idc_ongedaan, b_False)]), !.

enableGelijkspel1(Win, poel) :-
  %pouletelling(0),
  RBWin = win_GetCtlHandle(Win, idc_gelijk_spel),
  win_SetState(RBWin, [wsf_Enabled]),
  !.
enableGelijkspel1(Win, _) :-
  RBWin = win_GetCtlHandle(Win, idc_gelijk_spel),
  win_SetState(RBWin, [wsf_Disabled]).

uitslagWO(ja, _Uitsl1, Num, Cat, Tijd, Winnaar, b_True) :-		% 30.8.2007 niet meer al gewaarschuwd!
  logclose(),
  %2 = dlg_MessageBox( "W.O.", "Is de tegenpartij geïnformeerd?", mesbox_iconquestion, mesbox_buttonsYesNo, mesbox_defaultfirst, mesbox_suspendapplication ),	% 22.2.2009
  uitslagWO(Num, Cat, Tijd, Winnaar),
  !.
uitslagWO(_, _, Num, Cat, _, _, _) :-		% ruim alle ws-en op
  ws(Num,Cat,Winnaar,Tijd),
  logf('R',ws(Num,Cat,Winnaar,Tijd),nee), !.
uitslagWO(_, _, _, _, _, _, _).
  
uitslagWO(Num, Cat, Tijd, Winnaar) :-
  not(ws(Num,Cat,Winnaar,Tijd)),		% als niet al afgezegd moet worden
  logf('Z',ws(Num,Cat,Winnaar,Tijd),nee),!.
uitslagWO(_Num, _Cat, _Tijd, _Winnaar).

boekdatum(Win, Value) :-
  uitslWedsId(Win, No, Cat, _,_, _),		% window bestaat al
  beep(),
  w3(No, Cat, _, _, _, _, Tijd),
  bitright(Tijd, 7, Dag),
  dt_mins_date_time(Value, Dag, 0), !.
boekdatum(_Win, Value) :-			% window nog in wording
  NulWin = cast(WINDOW, 0),
  uitslWedsId(NulWin, No, Cat, _,_, _),
  w3(No, Cat, _, _, _, _, Tijd),
  bitright(Tijd, 7, Dag),
  dt_mins_date_time(Value, Dag, 0), !.
boekdatum(_Win, Value) :-			% nog geen vorige boekdatum, wel plandatum
  NulWin = cast(WINDOW, 0),
  uitslWedsId(NulWin, No, Cat, _,_, _),
  w2(No,Cat,Tijd,_,_,_,_,_), 
  bitright(Tijd, 7, Dag),
  dt_mins_date_time(Value, Dag, 0), !.
boekdatum(_Win, Value) :-			% nog geen vorige boekdatum
  date(Year,Month,Day),
  dt_min_to_offset(Year, Month, Day, 0,0, Value), !.

date_cc_class_handler(Win,e_Create(_),0):-!,		% CALL date_cc_Init ON
  boekdatum(Win, InitValue),
  date_cc_Init(Win, InitValue, "%DD-%MD-%YL").
date_cc_class_handler(Win, EVENT, 0):-			% CALL date_cc_HandleEvent
  date_cc_handleEvent(Win, Event), !.			% ON ANY OTHER EVENT
date_cc_class_handler(Win, EVENT, 0):-!,			% CALL date_cc_HandleEvent
  dlg_uitslaginvoer1_eh(Win, Event).			% ON ANY OTHER EVENT

enablePunten(Win, _Punten, afval) :-
  PWin = win_GetCtlHandle(Win, idc_punten),
  win_SetState(PWin, [wsf_Invisible]),
  P1Win = win_GetCtlHandle(Win, idc_punt1),
  win_SetState(P1Win, [wsf_Enabled, wsf_Invisible]),
  P2Win = win_GetCtlHandle(Win, idc_punt2),
  win_SetState(P2Win, [wsf_Enabled, wsf_Invisible]), !.
enablePunten(_Win, _, _) :-	!.		% straks pouletelling = 2 : geef zelf punten

uitslagverw(_Win, Num, Cat, Uitsl) :-
  getbacktrack(Here),
  wd(Num,Cat,T1,T2,_,_,Nota),
  cutbacktrack(Here),
  verwijderPlanning(Num, Cat, _PLTijd, _, _, onbekend),
  verwijderUitslag(Num, Cat),
  trap(DateCCWin = win_GetCtlHandle(_Win,idc_boekdatum),_,fail),
  MinOffset = date_cc_GetValue(DateCCWin),
  dt_mins_date_time(MinOffset, Datume, _),
  bitleft(Datume,7,UTijd),
  logf('Z',wd(Num,Cat,T1,T2,Uitsl,UTijd,Nota),nee), !.

winnende(Win, T1, T2, T1, T2, 1) :-
  RB = win_GetCtlHandle(Win, idc_winnaar1),
  b_True = win_IsChecked(RB), !.
winnende(Win, T1, T2, T2, T1, 0) :-
  RB = win_GetCtlHandle(Win, idc_winnaar2),
  b_True = win_IsChecked(RB), !.

afgebrokenOngedaan(Win, Uitslag) :-
  uitslWedsId(Win,Num,Cat,_,_,_),
  uitslagverw(Win, Num, Cat, Uitslag),
  uitslag_volg(Num, Cat),
  door_plannen(ja),
  1 = dlg_MessageBox( "Planning", "Wedstrijd opnieuw plannen?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
  nieuwPlan(ja, Win,Num,Cat),
  fail.
afgebrokenOngedaan(Win, _Uitslag) :-
  update_uitslagen("afgebroken ongedaan"),
  update_situatie(),
  update_waarsch(),
  uitslagen_top(),
  win_Destroy(Win).

iniUitslForm(Win, No, Cat, _T1, _T2) :-		% vul de punten in
  w3(No, Cat, _, P1P, P2P, _Status, _Tijd),
  P1 = win_GetCtlHandle(Win, idc_punt1),
  str_int(P1S, P1P),
  win_SetText(P1, P1S),
  P2 = win_GetCtlHandle(Win, idc_punt2),
  str_int(P2S, P2P),
  win_SetText(P2, P2S),
  fail.
/*iniUitslForm(Win, No, Cat, _T1, _T2) :-		% w.o.
  w3(No, Cat, _, _P1P, _P2P, walkovernr, _Tijd),
  RB = win_GetCtlHandle(Win, idc_walkover),
  win_Check(RB, b_True),
  CNR = win_GetCtlHandle(Win, idc_niet_reglementair),
  win_SetState(CNR, [wsf_Visible]),
  win_check(CNR, b_False),
  fail.*/
iniUitslForm(Win, No, Cat, _T1, _T2) :-		% w.o.
  w3(No, Cat, _, _P1P, _P2P, walkover, _Tijd),
  RB = win_GetCtlHandle(Win, idc_walkover),
  win_Check(RB, b_True),
  %CNR = win_GetCtlHandle(Win, idc_niet_reglementair),
  %win_SetState(CNR, [wsf_Visible]),
  %win_check(CNR, b_False),
  fail.
iniUitslForm(Win, No, Cat, _T1, _T2) :-		% opgave
  w3(No, Cat, _, _P1P, _P2P, gestaakt, _Tijd),
  RB = win_GetCtlHandle(Win, idc_gestaakt),
  win_Check(RB, b_True),
  fail.
iniUitslForm(Win, No, Cat, T, _T2) :-		% preselect de winnaar
  w3(No, Cat, T, _, _, _, _Tijd),
  RB = win_GetCtlHandle(Win, idc_winnaar1),
  win_Check(RB, b_True),
  pouletelling(PT),
  disableCheckbox(Win, PT, 1), !.
iniUitslForm(Win, No, Cat, _T1, T) :-
  w3(No, Cat, T, _, _, _, _Tijd),
  RB = win_GetCtlHandle(Win, idc_winnaar2),
  win_Check(RB, b_True),
  pouletelling(PT),
  disableCheckbox(Win, PT, 2), !.
iniUitslForm(Win, No, Cat, _T1, _T2) :-
  w3(No, Cat, gelijk_spel, _, _, _, _Tijd),
  RB = win_GetCtlHandle(Win, idc_gelijk_spel),
  win_Check(RB, b_True), !.
iniUitslForm(_Win, _No, _Cat, _T1, _T).

zetpunten(Win, b_True) :-
  P1 = win_GetCtlHandle(Win, idc_punt1),
  Inhoud1 = win_GetText(P1),
  P2 = win_GetCtlHandle(Win, idc_punt2),
  Inhoud2 = win_GetText(P2),
  concat(Inhoud1, Inhoud2, Inhoud),
  fronttoken(Inhoud, _, _), !.
zetpunten(Win, _) :-
  uitslWedsId(Win,_,Cat,_,_,_),
  pos(Cat, _,  _, poel),
  pouleTelling(PT),
  zetPunten(Win, PT, PTS1, _PTS2),
  P1 = win_GetCtlHandle(Win, idc_punt1),
  win_SetText(P1, PTS1),
  fail.
zetpunten(Win, _) :-
  uitslWedsId(Win,_,Cat,_,_,_),
  pos(Cat, _,  _, poel),
  pouleTelling(PT),
  zetPunten(Win, PT, _PTS1, PTS2),
  P2 = win_GetCtlHandle(Win, idc_punt2),
  win_SetText(P2, PTS2), !.
zetpunten(_, _).

zetPunten(Win, 5, "2", "0") :-			% 5 = nieuw met 2 en 1 punten
  EF1 = win_GetCtlHandle(Win, idc_uitslag),
  Uitslag = win_GetText(EF1),
  fronttoken(Uitslag, _, _),
  RB = win_GetCtlHandle(Win, idc_winnaar1),
  b_True = win_IsChecked(RB), !.
zetPunten(Win, 5, "0", "2") :-
  EF1 = win_GetCtlHandle(Win, idc_uitslag),
  Uitslag = win_GetText(EF1),
  fronttoken(Uitslag, _, _),
  RB = win_GetCtlHandle(Win, idc_winnaar2),
  b_True = win_IsChecked(RB), !.
zetPunten(Win, 5, "1", "1") :-
  EF1 = win_GetCtlHandle(Win, idc_uitslag),
  Uitslag = win_GetText(EF1),
  fronttoken(Uitslag, _, _),
  RB = win_GetCtlHandle(Win, idc_gelijk_spel),
  b_True = win_IsChecked(RB), !.
zetPunten(Win, 0, PntS, "0") :-			% 0 = normale telling (1 en 1)
  EF1 = win_GetCtlHandle(Win, idc_uitslag),
  Uitslag = win_GetText(EF1),
  fronttoken(Uitslag, _, _),
  RB = win_GetCtlHandle(Win, idc_winnaar1),
  b_True = win_IsChecked(RB),
  alTweePuntenToegekend(PntS), !.
zetPunten(Win, 0, "0", PntS) :-
  EF1 = win_GetCtlHandle(Win, idc_uitslag),
  Uitslag = win_GetText(EF1),
  fronttoken(Uitslag, _, _),
  RB = win_GetCtlHandle(Win, idc_winnaar2),
  b_True = win_IsChecked(RB),
  alTweePuntenToegekend(PntS), !.
zetPunten(Win, 0, "1", "1") :-
  EF1 = win_GetCtlHandle(Win, idc_uitslag),
  Uitslag = win_GetText(EF1),
  fronttoken(Uitslag, _, _),
  RB = win_GetCtlHandle(Win, idc_gelijk_spel),
  b_True = win_IsChecked(RB), !.
zetPunten(Win, 1, P1, P2) :-
  RB = win_GetCtlHandle(Win, idc_winnaar1),
  b_True = win_IsChecked(RB),
  EF1 = win_GetCtlHandle(Win, idc_uitslag),
  Uitslag = win_GetText(EF1),
  fronttoken(Uitslag, _, _),
  uitsl_games(Uitslag, GamesT, GamesV, _/*SetsT*/, _/*SetsV*/),
  Games1 = GamesT - GamesV, 
  punten(1, Games1, Punten1),
  str_int(P1, Punten1),
  Games2 = GamesV - GamesT, 
  punten(0, Games2, Punten2),
  str_int(P2, Punten2), !.
zetPunten(Win, 1, P2, P1) :-			% omgedraaid
  RB = win_GetCtlHandle(Win, idc_winnaar2),
  b_True = win_IsChecked(RB),
  EF1 = win_GetCtlHandle(Win, idc_uitslag),
  Uitslag = win_GetText(EF1),
  fronttoken(Uitslag, _, _),
  uitsl_games(Uitslag, GamesT, GamesV, _/*SetsT*/, _/*SetsV*/),
  Games1 = GamesT - GamesV, 
  punten(1, Games1, Punten1),
  str_int(P1, Punten1),
  Games2 = GamesV - GamesT, 
  punten(1, Games2, Punten2),
  str_int(P2, Punten2), !.
zetPunten(Win, 4, P1, P2) :-
  RB = win_GetCtlHandle(Win, idc_winnaar1),
  b_True = win_IsChecked(RB),
  EF1 = win_GetCtlHandle(Win, idc_uitslag),
  Uitslag = win_GetText(EF1),
  fronttoken(Uitslag, _, _),
  uitsl_games(Uitslag, GamesT, GamesV, _/*SetsT*/, _/*SetsV*/),
  Games1 = GamesT - GamesV, 
  punten(4, Games1, Punten1),
  str_int(P1, Punten1),
  Games2 = GamesV - GamesT, 
  punten(4, Games2, Punten2),
  str_int(P2, Punten2), !.
zetPunten(Win, 4, P2, P1) :-			% omgedraaid
  RB = win_GetCtlHandle(Win, idc_winnaar2),
  b_True = win_IsChecked(RB),
  EF1 = win_GetCtlHandle(Win, idc_uitslag),
  Uitslag = win_GetText(EF1),
  fronttoken(Uitslag, _, _),
  uitsl_games(Uitslag, GamesT, GamesV, _/*SetsT*/, _/*SetsV*/),
  Games1 = GamesT - GamesV, 
  punten(4, Games1, Punten1),
  str_int(P1, Punten1),
  Games2 = GamesV - GamesT, 
  punten(4, Games2, Punten2),
  str_int(P2, Punten2), !.
zetPunten(Win, _, "", "") :-
  CBA = win_GetCtlHandle(Win, idc_afgebroken_partij),
  b_True = win_IsChecked(CBA), !.
zetPunten(Win, 3, "", "0") :-
  EF1 = win_GetCtlHandle(Win, idc_uitslag),
  Uitslag = win_GetText(EF1),
  fronttoken(Uitslag, _, _),
  RB = win_GetCtlHandle(Win, idc_winnaar1),
  b_True = win_IsChecked(RB), !.
zetPunten(Win, 3, "0", "") :-
  EF1 = win_GetCtlHandle(Win, idc_uitslag),
  Uitslag = win_GetText(EF1),
  fronttoken(Uitslag, _, _),
  RB = win_GetCtlHandle(Win, idc_winnaar2),
  b_True = win_IsChecked(RB), !.
zetPunten(_Win, _, "", "").

/*getStatus(Win, walkovernr, Cat) :-
  RB = win_GetCtlHandle(Win, idc_walkover),
  b_True = win_IsChecked(RB),
  CNR = win_GetCtlHandle(Win, idc_niet_reglementair),
  b_True = win_IsChecked(CNR), !,
  getStatusPouleMessage(Cat). */
getStatus(Win, walkover, Cat) :-
  RB = win_GetCtlHandle(Win, idc_walkover),
  b_True = win_IsChecked(RB), !,
  getStatusPouleMessage(Cat).
getStatus(Win, gestaakt, Cat) :-
  RB = win_GetCtlHandle(Win, idc_gestaakt),
  b_True = win_IsChecked(RB), !,
  getStatusPouleMessage(Cat).
getStatus(_Win, geen, _).

alTweePuntenToegekend("2") :-
  w3(_,_, _, P, _, _, _),
  P > 1, !.
alTweePuntenToegekend("2") :-
  w3(_,_, _, _, P, _, _),
  P > 1, !.
alTweePuntenToegekend("1").

getStatusPouleMessage(Cat) :-
  pos(Cat, _, _, poel),
  pouletelling(0),			% normaal
  dlg_MessageBox( "W.O./Opgave", "Bij een poule tellen géén van de door\nde opgever(s) gespeelde partijen\n meer mee voor de puntentelling!", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  !.
getStatusPouleMessage(_Cat).

str_intP("", In) :-
  bound(In),
  In = 0, !.
str_intP(S, I) :-
  str_int(S, I), !.
str_intP("", In) :-
  bound(In), !.
str_intP(In, 0) :-
  bound(In). 

puntenNodig(_Win) :-
  PF1 = win_GetCtlHandle(_Win, idc_punt1),
  TP1 = win_GetText(PF1),
  not(str_int(TP1, _)),  
  dlg_MessageBox( "Punten", "vul punten in!", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  win_PostEvent(_Win,e_User( setfocus, 1)), !. 
puntenNodig(_Win) :-
  PF2 = win_GetCtlHandle(_Win, idc_punt2),
  TP2 = win_GetText(PF2),
  not(str_int(TP2, _)),  
  dlg_MessageBox( "Punten", "vul punten in!", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  win_PostEvent(_Win,e_User( setfocus, 2)), !. 

%	searchstring(Uitsl,"gestaakt",FoundPos),
verwijder(String, Uitsl, NUitsl) :-
  searchstring(Uitsl,String,FoundPos),
  str_len(String, Len),
  L1 = FoundPos - 1,
  frontstr(L1,Uitsl,S1,_),
  L2 = FoundPos + Len - 1,
  frontstr(L2,Uitsl,_,S2),
  concat(S1, S2, NUitsl), !.
verwijder(_, Uitsl, Uitsl).

disableCheckbox(_Win, 0, 1) :-
  Win2 = win_GetCtlHandle(_Win, idc_punt2),
  win_SetState(Win2, [wsf_disabled]),
  Win1 = win_GetCtlHandle(_Win, idc_punt1),
  win_SetState(Win1, [wsf_enabled]), !.
disableCheckbox(_Win, 0, 2) :-
  Win2 = win_GetCtlHandle(_Win, idc_punt2),
  win_SetState(Win2, [wsf_enabled]),
  Win1 = win_GetCtlHandle(_Win, idc_punt1),
  win_SetState(Win1, [wsf_disabled]), !.
disableCheckbox(_Win, _PT, _) :-
  Win2 = win_GetCtlHandle(_Win, idc_punt2),
  win_SetState(Win2, [wsf_enabled]),
  Win1 = win_GetCtlHandle(_Win, idc_punt1),
  win_SetState(Win1, [wsf_enabled]), !.


setIsVerkort(A, b_True) :- 
  A < 6, !.
setIsVerkort(_, b_False).

setsplusmin(In, SetsIn, SetsUit, VerkortUit) :-
  skiptoken1(In, In1),
  fronttoken(In1, T1, Rest),
  str_int(T1, A1),
  skiptoken1(Rest, R1),
  fronttoken(R1, T3, R2),
  str_int(T3, A2),
  grootste(A1, A2, AGr),
  setIsVerkort(AGr, Verkort1),
  %A2 > A1,
  A1 <> A2,		% anders bingo
  SetsIn1 = SetsIn + ((A1 - A2) div abs(A1 - A2)), 
   !,
  setsplusmin(R2, SetsIn1, SetsUit, Verkort2),
  bitor(Verkort1, Verkort2, VerkortUit).
setsplusmin(_, In, In, b_False).

skiptoken1(In, Out2) :-
  fronttoken(In, Token, Out1),
  Token = "(",
  searchchar(Out1,')',FPos),
  frontstr(FPos,Out1,_StartString,Out2), !.
skiptoken1(In, Out) :-
  fronttoken(In, Token, Out1),
  not(str_int(Token,_)), 
  skiptoken1(Out1, Out), !.
skiptoken1(In, In).

%isUitslagNietPlausibel(_Win, Cat, Getal, Verkort) :-
%  write("\nGetal: ",Getal," Wedscat: ",Cat," Verkort: ",Verkort),
%  fail.
isUitslagNietPlausibel(Win, _Cat, Getal, _Verkort) :-
  Getal < 1,
  1 <> dlg_MessageBox( "Typefout?", "Uitslag vanuit de winnaar gezien?\nIs deze uitslag juist ingevoerd?", mesbox_iconquestion, mesbox_buttonsYesNo, mesbox_defaultfirst, mesbox_suspendapplication ),
  win_SetFocus(Win), !.
isUitslagNietPlausibel(_Win, Cat, _Getal, b_True) :-
  wc(Cat, _OnderdeelId, _, _, _, _CStr, _, _, _, _, _, _, _, Tid, _),
  toernooiK(Tid, nrt),
  dlg_MessageBox( "Uitslag", "Verkorte partijen zijn niet toegestaan bij NRt!", mesbox_iconexclamation, mesbox_buttonsOK, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.

eenOnbekende(onbekend, _) :- !.
eenOnbekende(_, onbekend).

boekdag(No, Cat, Tijd) :-			% nog geen vorige boekdatum, wel plandatum
  w2(No,Cat,Tijd,_,_,_,_,_), !.
boekdag(_, _, UTijd) :-			% nog geen vorige boekdatum
  date(Year,Month,Day),
  dt_min_to_offset(Year, Month, Day, 0,0, MinOffset),
  dt_mins_date_time(MinOffset, Datume, _),
  bitleft(Datume,7,UTijd).

woInvoeren(1, No, Cat) :-	% alleen als de wo invoeren bij onbekende tegenstander
  wd(No,Cat,T1,onbekend,_,_,Nota),	% de ander is de verliezer
  verwijderplanningWO(No, Cat, 1),
  boekdag(No, Cat, BDag),  
  logf('Z', w3(No, Cat, onbekend, 0, 0, walkover, BDag),ja),
  logf('Z',wd(No,Cat,T1,onbekend,"w.o.",BDag,Nota),nee),
  update_display(),
  uitslag_volg(No, Cat), !.
woInvoeren(1, No, Cat) :-	% alleen als de wo invoeren bij onbekende tegenstander
  wd(No,Cat,onbekend,T2, _,_,Nota),	% de ander is de verliezer
  verwijderplanningWO(No, Cat, 0),
  boekdag(No, Cat, BDag),  
  logf('Z', w3(No, Cat, onbekend, 0, 0, walkover, BDag),ja),
  logf('Z',wd(No,Cat,onbekend,T2,"w.o.",BDag,Nota),nee),
  update_display(),
  uitslag_volg(No, Cat), !.
woInvoeren(_, _, _).

woOngedaan(1, No, Cat) :-
  w3(No, Cat, onbekend, G1, G2, Status, BDag),
  logf('R', w3(No, Cat, onbekend, G1, G2, Status, BDag), nee),
  fail.
woOngedaan(1, No, Cat) :-
  wd(No,Cat,T1,T2,_,BDag,Nota),
  logf('Z', wd(No,Cat,T1,T2,"",BDag,Nota), nee), !.
woOngedaan(_, _No, _Cat).
  
  dlg_uitslaginvoer1_Create(_Parentxx, No, Cat, _):-
	wd(No,Cat,S1,S2,_,_,_),
	eenOnbekende(S1, S2),
	not(w3(No,Cat,_,_,_,_,_)),
	OK = dlg_MessageBox( "Uitslag invoeren", "Nog niet alles spelers zijn bekend.\nWil je een walk over (W.O.) invoeren?", mesbox_iconquestion, mesbox_buttonsokcancel, mesbox_defaultfirst, mesbox_suspendapplication ),
	woInvoeren(OK, No, Cat),
	update_waarsch(),
	update_situatie(),
	update_uitslagen("uitslag_invoeren_create w.o."),
	logclose(),
	!.
  dlg_uitslaginvoer1_Create(_Parentxx, No, Cat, _):-
	wd(No,Cat,S1,S2,_,_,_),
	eenOnbekende(S1, S2),
	w3(No,Cat,onbekend,_,_,_,_),
	OK = dlg_MessageBox( "Uitslag invoeren", "Wil je de walk over (W.O.) ongedaan maken?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
	woOngedaan(OK, No, Cat),
	update_waarsch(),
	update_situatie(),
	update_uitslagen("uitslaginvoer_create w.o."),
	logclose(),
	!.
  dlg_uitslaginvoer1_Create(_Parentxx, No, Cat, MetaanW):-
	Parent = vpi_GetTaskWin(),
	invoerbaar(No, Cat, b_True),	% 1.1.2005 voor de zekerheid
	class_Create(date_cc_class,date_cc_class_handler),
	assert(verliezergezet(b_False)),
	assert(eersteKeer(b_True)),
	assert(metAanwezigheidsRegistratie(MetAanw)),
	NulWin = cast(WINDOW, 0),
	assert(uitslWedsId(NulWin, No, Cat, "Voer uitslag in",nee, 0)),
	win_CreateDynDialog(Parent,
		[
%BEGIN Uitslaginvoer1, WinDefList, 16:35:45-16.11.2016, Code automatically updated!
		 dlg_font(wdef(dlg_uitslaginvoer1_DlgType,dlg_uitslaginvoer1_RCT,dlg_uitslaginvoer1_Title,u_DlgBase),
		 	  dlg_uitslaginvoer1_Font,dlg_uitslaginvoer1_FSize,dlg_uitslaginvoer1_Flags),
		 ctl(wdef(wc_Edit,rct(11,14,26,25),"",u_DlgBase),idc_punt1,[wsf_AlignCenter,wsf_Group,wsf_TabStop,wsf_AutoHScroll]),
		 ctl(wdef(wc_Edit,rct(11,26,26,37),"",u_DlgBase),idc_punt2,[wsf_AlignCenter,wsf_Group,wsf_TabStop,wsf_AutoHScroll]),
		 ctl(wdef(wc_RadioButton,rct(35,15,207,25),"Winnaar1",u_DlgBase),idc_winnaar1,[wsf_Auto,wsf_TabStop,wsf_Group]),
		 ctl(wdef(wc_RadioButton,rct(35,28,207,38),"Winnaar2",u_DlgBase),idc_winnaar2,[wsf_Auto,wsf_TabStop]),
		 ctl(wdef(wc_RadioButton,rct(35,41,207,51),"&Voorlopige stand (partij bijvoorbeeld afgebroken)",u_DlgBase),idc_afgebroken_partij,[wsf_Auto,wsf_TabStop]),
		 ctl(wdef(wc_RadioButton,rct(35,54,207,64),"Gelijk spel",u_DlgBase),idc_gelijk_spel,[wsf_Auto,wsf_TabStop]),
		 ctl(wdef(wc_RadioButton,rct(35,67,207,77),"Uitslag wissen",u_DlgBase),idc_ongedaan,[wsf_Auto,wsf_TabStop]),
		 ctl(wdef(wc_Edit,rct(32,82,127,94),"",u_DlgBase),idc_uitslag,[wsf_AlignLeft,wsf_Group,wsf_TabStop,wsf_AutoHScroll]),
		 ctl(wdef(wc_CheckBox,rct(32,96,131,106),"&Walk-over (zonder spelen)",u_DlgBase),idc_walkover,[wsf_Auto,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_CheckBox,rct(134,96,186,106),"&Opgegeven",u_DlgBase),idc_gestaakt,[wsf_Auto,wsf_TabStop,wsf_ClipSiblings]),
		 ctl(wdef(wc_CheckBox,rct(32,119,148,129),"&Verliezer naar verliezersronde",u_DlgBase),idc_verliezer,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 customctl(wdef(wc_Custom,rct(46,139,101,150),"Boekdatum",u_DlgBase),"date_cc_class",idc_boekdatum,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(135,139,167,150),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(168,139,200,150),"&Cancel",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(206,133,240,154),"OK",u_DlgBase),idc_ok,[wsf_Group,wsf_TabStop,wsf_Default]),
		 ctl(wdef(wc_Text,rct(6,139,45,149),"Boekdatum",u_DlgBase),idct_boekdatum,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(127,80,191,96),"(als gezien vanuit de winnaar)",u_DlgBase),idct_aanwijzing,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(6,83,31,93),"Uitslag",u_DlgBase),idct_uitslag,[wsf_AlignLeft]),
		 icon(wdef(wc_Icon,rct(10,55,26,71),"",u_DlgBase),idc_uitslaginvoer1_1,idi_uitslagen,[]),
		 ctl(wdef(wc_GroupBox,rct(32,4,209,79),"Wie is winnaar?",u_DlgBase),idc_uitslag1,[]),
		 ctl(wdef(wc_GroupBox,rct(4,4,33,46),"Punten",u_DlgBase),idc_punten,[])
%END Uitslaginvoer1, WinDefList
		],dlg_uitslaginvoer1_eh,0),
	class_destroy(date_cc_class), !.

%MARK Uitslaginvoer1, new events

%BEGIN Uitslaginvoer1, idc_uitslag losefocus
  dlg_uitslaginvoer1_eh(_Win,e_Control(idc_uitslag,_CtrlType,_CtrlWin,losefocus),0):-
	enableOK(_Win),
	zetPunten(_Win, b_True), !.
%END Uitslaginvoer1, idc_uitslag losefocus

%BEGIN Uitslaginvoer1, e_User
  dlg_uitslaginvoer1_eh(_Win,e_User(setfocus,1),0):-
	PW1 = win_GetCtlHandle(_Win, idc_punt1),
	win_SetFocus(PW1),
	!.
  dlg_uitslaginvoer1_eh(_Win,e_User(setfocus,2),0):-
	PW2 = win_GetCtlHandle(_Win, idc_punt2),
	win_SetFocus(PW2),
	!.
  dlg_uitslaginvoer1_eh(_Win,e_User(setfocus,3),0):-
	UW = win_GetCtlHandle(_Win, idc_uitslag),
	win_SetFocus(UW),
	!.
%END Uitslaginvoer1, e_User

%BEGIN Uitslaginvoer1, idc_ongedaan _CtlInfo
  dlg_uitslaginvoer1_eh(_Win,e_Control(idc_ongedaan,_CtrlType,_CtrlWin,_CtlInfo),0):-
	enableOK(_Win),
	eersteKeer(EersteKeer),
	zetpunten(_Win, EersteKeer),
	assert(eersteKeer(b_False)),
	pouletelling(PT),
        disableCheckbox(_Win, PT, 0),
	!.
%END Uitslaginvoer1, idc_ongedaan _CtlInfo

%BEGIN Uitslaginvoer1, idc_afgebroken_partij _CtlInfo
  dlg_uitslaginvoer1_eh(_Win,e_Control(idc_afgebroken_partij,_CtrlType,_CtrlWin,_CtlInfo),0):-
	enableOK(_Win),
	eersteKeer(EersteKeer),
	zetpunten(_Win, EersteKeer),
	assert(eersteKeer(b_False)),
	pouletelling(PT),
        disableCheckbox(_Win, PT, 0),
	!.
%END Uitslaginvoer1, idc_afgebroken_partij _CtlInfo

%BEGIN Uitslaginvoer1, idc_gelijk_spel _CtlInfo
  dlg_uitslaginvoer1_eh(_Win,e_Control(idc_gelijk_spel,_CtrlType,_CtrlWin,_CtlInfo),0):-
	enableOK(_Win),
	eersteKeer(EersteKeer),
	zetpunten(_Win,EersteKeer),
	assert(eersteKeer(b_False)),
	pouletelling(PT),
        disableCheckbox(_Win, PT, 0),
	!.
%END Uitslaginvoer1, idc_gelijk_spel _CtlInfo

%BEGIN Uitslaginvoer1, idc_winnaar2 _CtlInfo
  dlg_uitslaginvoer1_eh(_Win,e_Control(idc_winnaar2,_CtrlType,_CtrlWin,_CtlInfo),0):-
	enableOK(_Win),
	eersteKeer(EersteKeer),
	zetpunten(_Win,EersteKeer),
	assert(eersteKeer(b_False)),
	pouletelling(PT),
        disableCheckbox(_Win, PT, 2),
	!.
%END Uitslaginvoer1, idc_winnaar2 _CtlInfo

%BEGIN Uitslaginvoer1, idc_winnaar1 _CtlInfo
  dlg_uitslaginvoer1_eh(_Win,e_Control(idc_winnaar1,_CtrlType,_CtrlWin,_CtlInfo),0):-
	enableOK(_Win),
	eersteKeer(EersteKeer),
	zetpunten(_Win,EersteKeer),
	assert(eersteKeer(b_False)),
	pouletelling(PT),
        disableCheckbox(_Win, PT, 1),
	!.
%END Uitslaginvoer1, idc_winnaar1 _CtlInfo

%BEGIN Uitslaginvoer1, idc_gestaakt _CtlInfo
  dlg_uitslaginvoer1_eh(_Win,e_Control(idc_gestaakt,_CtrlType,CBG,_CtlInfo),0):-
	b_True = win_IsChecked(CBG),		% gestaakt en walk-over nooit allebei aan
	CBW = win_GetCtlHandle(_Win, idc_walkover),
	win_Check(CBW, b_False),
	enableOK(_Win),
	EF1 = win_GetCtlHandle(_Win, idc_uitslag),
	Uitsl = win_GetText(EF1),
	concat(Uitsl, " opgegeven", NUitsl),
	verwijder(" w.o.", NUitsl, NUitsl1),
	win_SetText(EF1, NUitsl1),
	!.
  dlg_uitslaginvoer1_eh(_Win,e_Control(idc_gestaakt,_CtrlType,CBG,_CtlInfo),0):-
	b_False = win_IsChecked(CBG),
	EF1 = win_GetCtlHandle(_Win, idc_uitslag),
	Uitsl = win_GetText(EF1),
	verwijder(" opgegeven", Uitsl, NUitsl),
	win_SetText(EF1, NUitsl), !.
  dlg_uitslaginvoer1_eh(_Win,e_Control(idc_gestaakt,_CtrlType,_CBG,_CtlInfo),0):- !.
%END Uitslaginvoer1, idc_gestaakt _CtlInfo

%BEGIN Uitslaginvoer1, idc_walkover _CtlInfo
  dlg_uitslaginvoer1_eh(_Win,e_Control(idc_walkover,_CtrlType,CBW,_CtlInfo),0):-
	b_True = win_IsChecked(CBW),
	%CNR = win_GetCtlHandle(_Win, idc_niet_reglementair),
	%win_SetState(CNR, [wsf_Visible]),
	CBG = win_GetCtlHandle(_Win, idc_gestaakt),
	win_Check(CBG, b_False),
	enableOK(_Win),
	EF1 = win_GetCtlHandle(_Win, idc_uitslag),
	Uitsl = win_GetText(EF1),
	concat(Uitsl, " w.o.", NUitsl),
	verwijder(" opgegeven", NUitsl, NUitsl1),
	win_SetText(EF1, NUitsl1),
	P1win = win_GetCtlHandle(_Win,idc_punt1),
	P2win = win_GetCtlHandle(_Win,idc_punt2),
	win_SetText(P1Win, ""),
	win_SetText(P2Win, ""),
	!.

  dlg_uitslaginvoer1_eh(_Win,e_Control(idc_walkover,_CtrlType,_CtrlWin,_CtlInfo),0):-!.
%END Uitslaginvoer1, idc_walkover _CtlInfo

%BEGIN Uitslaginvoer1, idc_ok _CtlInfo
  dlg_uitslaginvoer1_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	RB1 = win_GetCtlHandle(_Win, idc_winnaar1),	% set is verkort
	RB1V = win_IsChecked(RB1),
	RB2 = win_GetCtlHandle(_Win, idc_winnaar2),
	RB2V = win_IsChecked(RB2),
	RB1V + RB2V > 0,
	uitslWedsId(_Win,_Num,Cat,_,_,_),
	RBWO = win_GetCtlHandle(_Win, idc_walkover),
	not(b_True = win_IsChecked(RBWO)),
	RBOP = win_GetCtlHandle(_Win, idc_gestaakt),
	not(b_True = win_IsChecked(RBOP)),
	EF1 = win_GetCtlHandle(_Win, idc_uitslag),
	Uitsl = win_GetText(EF1),
	setsplusmin(Uitsl, 0, Getal, Verkort),
	isUitslagNietPlausibel(EF1, Cat, Getal, Verkort),
	!.
  dlg_uitslaginvoer1_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	RB1 = win_GetCtlHandle(_Win, idc_winnaar1),
	RB1V = win_IsChecked(RB1),
	RB2 = win_GetCtlHandle(_Win, idc_winnaar2),
	RB2V = win_IsChecked(RB2),
	RBG = win_GetCtlHandle(_Win, idc_gelijk_spel),
	RBGV = win_IsChecked(RBG),
	RB1V + RB2V + RBGV > 0,
	uitslWedsId(_Win,_Num,Cat,_,_,_),
	pos(Cat, _, _, poel),			% er zijn punten vereist
	RBWO = win_GetCtlHandle(_Win, idc_walkover),
	not(b_True = win_IsChecked(RBWO)),
	RBOP = win_GetCtlHandle(_Win, idc_gestaakt),
	not(b_True = win_IsChecked(RBOP)),
	puntenNodig(_Win), !.
  dlg_uitslaginvoer1_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	uitslWedsId(_Win,Num,Cat,_,_,_),
	wd(Num,Cat,T1,T2,_,_,Nota),
	winnende(_Win, T1, T2, Tw, Tv, Vorige),		% winnaar1 OF winnaar2
	EF1 = win_GetCtlHandle(_Win, idc_uitslag),
	Uitsl = win_GetText(EF1),
	%uitslWedsId(_Win,Num,Cat,_,_,_),
	loglock(mdi, "dlg_uitslaginvoer1"),
	verwijderPlanning(Num, Cat, PlTijd, _, _G, Tw),
	trap(DateCCWin = win_GetCtlHandle(_Win,idc_boekdatum),_,fail),
	MinOffset = date_cc_GetValue(DateCCWin),
	dt_mins_date_time(MinOffset, Datume, _),
	bitleft(Datume,7,UTijd),
	getStatus(_Win, Status, Cat),
	PF1 = win_GetCtlHandle(_Win, idc_punt1),
	TP1 = win_GetText(PF1),
	str_intP(TP1, P1),  
	PF2 = win_GetCtlHandle(_Win, idc_punt2),
	TP2 = win_GetText(PF2),
	str_intP(TP2, P2),  
	logf('Z', w3(Num, Cat, Tw, P1, P2, Status, UTijd),ja),
	oproepen(Oproep),
	CBW = win_GetCtlHandle(_Win, idc_walkover),
	WalkOver = win_IsChecked(CBW),
	uitslagWO(Oproep, Uitsl, Num, Cat, PlTijd, Tw, WalkOver),
	logf('Z',wd(Num,Cat,T1,T2,Uitsl,UTijd,Nota),nee),
	update_display(),
	uitslag_volg(Num, Cat),
	win_Destroy(_Win),
	TaskWin = vpi_GetTaskWin(),
	bitright(Num, 1, Num1),
	bitand(Num, 1, OE),
	verwerk(TaskWin, Num, Cat), 
	tgnst_naam(nee, b_False, Cat, Tw,WNaam,_),
	vervolgMelding(TaskWin, WNaam ,Oproep, Num1, Cat, OE),
	metAanwezigheidsRegistratie(MetAanw),
	write("\nMaw: ", MetAanw),
	spelersBlijven(Num, Cat, MetAanw),
	update_situatie(),
	update_waarsch(),
	update_uitslagen("idc_ok"),
	uitslagen_top(),
	uitsl_v11(Num,Cat,Tw,Tv,Vorige,UTijd,Uitsl),  
	!.
  dlg_uitslaginvoer1_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	winnende(_Win, onbekend, onbekend, _, _, _),		% winnaar1 en winnaar2
	win_Destroy(_Win),
	logclose(),
	!.
  dlg_uitslaginvoer1_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	RB = win_GetCtlHandle(_Win, idc_gelijk_spel),
	b_True = win_IsChecked(RB),
	EF1 = win_GetCtlHandle(_Win, idc_uitslag),
	Uitsl = win_GetText(EF1),
	uitslWedsId(_Win,Num,Cat,_,_,_),
	wd(Num,Cat,T1,T2,_,_,Nota),
	verwijderPlanning(Num, Cat, _PLTijd, _, _, onbekend), 
	trap(DateCCWin = win_GetCtlHandle(_Win,idc_boekdatum),_,fail),
	MinOffset = date_cc_GetValue(DateCCWin),
	dt_mins_date_time(MinOffset, Datume, _),
	bitleft(Datume,7,UTijd),
	getStatus(_Win, Status, Cat),				% 13.4.2003 
	PF1 = win_GetCtlHandle(_Win, idc_punt1),
	TP1 = win_GetText(PF1),
	str_intP(TP1, P1),  
	PF2 = win_GetCtlHandle(_Win, idc_punt2),
	TP2 = win_GetText(PF2),
	str_intP(TP2, P2),  
	logf('Z', w3(Num, Cat, gelijk_spel, P1, P2, Status, UTijd),ja),
	logf('Z',wd(Num,Cat,T1,T2,Uitsl,UTijd,Nota),nee),
	metAanwezigheidsRegistratie(MetAanw),
	write("\nMaw: ", MetAanw),
	spelersBlijven(Num, Cat, MetAanw), 
	select(Cat, Num),
	update_situatie(),
	update_waarsch(),
	update_uitslagen("uitslaginvoer -idc_ok"),
	uitslagen_top(),
	win_Destroy(_Win),
	TaskWin = vpi_GetTaskWin(),
	verwerk(TaskWin, Num, Cat), 
	!.
  dlg_uitslaginvoer1_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	RB = win_GetCtlHandle(_Win, idc_afgebroken_partij),
	b_True = win_IsChecked(RB),
	EF1 = win_GetCtlHandle(_Win, idc_uitslag),
	Uitslag = win_GetText(EF1),
	afgebrokenOngedaan(_Win, Uitslag),
	!.
  dlg_uitslaginvoer1_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	RB = win_GetCtlHandle(_Win, idc_ongedaan),
	b_True = win_IsChecked(RB),
	afgebrokenOngedaan(_Win, ""),
	!.
%END Uitslaginvoer1, idc_ok _CtlInfo

%BEGIN Uitslaginvoer1, idc_help _CtlInfo
  dlg_uitslaginvoer1_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Uitslageninvoerenf8"),
	!.
%END Uitslaginvoer1, idc_help _CtlInfo

%BEGIN Uitslaginvoer1, e_Destroy
  dlg_uitslaginvoer1_eh(_Win,e_Destroy,0):-!,
	retractall(uitslWedsId(_Win, _, _, _,_,_)),
	logclose(),
	!.
%END Uitslaginvoer1, e_Destroy

%BEGIN Uitslaginvoer1, e_Create
  dlg_uitslaginvoer1_eh(_Win,e_Create(_CreationData),0):-!,
	NulWin = cast(WINDOW, 0),
	retract(uitslWedsId(NulWin, Num, Cat, _, LR, TGew)),
	wc(Cat, _, _, Titel, _, _, _, _, _, _, _, _, _, _, _),
	%wc(Cat,Titel,_,_,_,_,_),
	win_SetText(_Win, Titel),
	assert(uitslWedsId(_Win, Num, Cat, Titel, LR, TGew)),
	wd(Num,Cat,A,B,Stand,_,_),
	UB = win_GetCtlHandle(_Win, idc_uitslag),
	win_SetText(UB, Stand),
	repc(nee,'}',nee,nee,nee,0,1,1,Num,Cat,A,alleplanning, Naam1,_Score,_TijdLog,_,_,_,_),
	repc(nee,'}',nee,nee,nee,1,1,1,Num,Cat,B,alleplanning, Naam2,_Score1,_TijdLog1,_,_,_,_),
	concat(" ", Naam1, Naama),
	concat(" ", Naam2, Naamb),
	Pb1 = win_GetCtlHandle(_Win, idc_winnaar1),
	Pb2 = win_GetCtlHandle(_Win, idc_winnaar2),
	win_SetText(Pb1, Naama),
	win_SetText(Pb2, Naamb),
	pos(Cat, _, _, AfvPoel),
	enableGelijkspel1(_Win, AfvPoel),
        enableOngedaan(_Win, Num, Cat),
	pouletelling(PT),
	enablePunten(_Win, PT, AfvPoel), 
	iniUitslForm(_Win, Num, Cat, A, B),
	enableOK(_Win),
	win_PostEvent(_Win,e_User( setfocus, 3)),
	!.
%END Uitslaginvoer1, e_Create


%BEGIN Uitslaginvoer1, idc_cancel _CtlInfo
  dlg_uitslaginvoer1_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	win_Destroy(_Win),
	!.
%END Uitslaginvoer1, idc_cancel _CtlInfo

  dlg_uitslaginvoer1_eh(_,_,_):-!,fail.

%END_DLG Uitslaginvoer1


%BEGIN_TLB Informeren, 11:57:35-1.7.2012, Code automatically updated!
/**************************************************************************
	Creation of toolbar: Informeren
**************************************************************************/

clauses

  tb_informeren_Create(_Parent):-
ifdef use_tbar
	toolbar_create(tb_bottom,0xC0C0C0,_Parent,
		[separator,
		 tb_ctrl(id_email_oproepen,pushb,idb_emailw,idb_emaild,idb_grijs,"Emails;Email naar alle op te roepen personen uit de lijst die een emailadres hebben",1,1),
		 tb_ctrl(idt_downloadbevestig,pushb,idb_synchro,idb_synchrod,idb_synchrod,"Synchroniseren;Synchroniseren met Internet",1,1),
		 tb_ctrl(id_Speler_Oproep_print,pushb,idb_printw,idb_printdown,idb_printdown,"Afdruk (lijst);Lijst afdrukken",1,1),
		 tb_text(idti_gereed,tb_static,164,0,3,10,0x0," percentage bevestigd: 100% ")]),
enddef
	true.
%END_TLB Informeren






%BEGIN_DLG Plangegevens
/**************************************************************************
	Creation and event handling for dialog: Plangegevens
**************************************************************************/

constants

%BEGIN Plangegevens, CreateParms, 12:03:43-22.3.2013, Code automatically updated!
  dlg_plangegevens_DlgType = wd_Modal
  dlg_plangegevens_Title = "Planningsgegevens"
  dlg_plangegevens_RCT = rct(40,32,346,262)
  dlg_plangegevens_Flags = [wsf_Close,wsf_TitleBar,wsf_ClipSiblings]
  dlg_plangegevens_Help = idh_planninggegevens
  dlg_plangegevens_Font = "Arial"
  dlg_plangegevens_FSize = 10
%END Plangegevens, CreateParms

getStartupText = 1000

database - plangeg
  single personen1(oprafztype, tgnst, integer, wedscat, boolean Slave)
  single resultaat1(integer)
  single hulpindex(integer)
  determ  oprpersoon(integer)

predicates

  dlg_plangegevens_eh : EHANDLER
  enableGew(integer,WINDOW,integer)
  procedure telButton(WINDOW, string Tel, string Tel1, string Tel2, integer Button1, integer Button2, integer Button3) - (i,i,i,i,i,i,i)
  procedure telButton(WINDOW, string Tel, integer Button, string TypeHWM)
  zetIncl(WINDOW, integer Control, integer Gaant)
  zetOpGew(integer LRE, oprafztype, persno, BOOLEAN Inclusief, integer, wedscat, boolean Slave)
  saveAfzegIndexes(integer LR, integer Iin, persno Pers)
  initOpm(tgnst)
procedure  updateText(tgnst, string Text1, string TextO1, string Text2, string TextO2) - (i, i, i, i, i)
procedure  updateText(integer, string, string)
procedure  beginTijd(integer, string, string)
procedure zetBericht(string, BOOLEAN)
determ slaaf(boolean, integer Num, wedscat Cat, integer Num, wedscat Cat)
determ eenling(tgnst, integer)
procedure smsmogelijk(integer, boolean)

clauses

personen1(mijnoproepen, onbekend, 0, 0, b_False).
resultaat1(0).
hulpindex(0).

updateText(s(Sp), Text1, TextO1, _, _) :-
  updateText(Sp,Text1, TextO1), !.
updateText(pw(Sp), Text1, TextO1, _, _) :-
  updateText(Sp,Text1, TextO1), !.
updateText(p(S1, S2), Text1, TextO1, Text2, TextO2) :-
  updateText(S1,Text1, TextO1),
  updateText(S2,Text2, TextO2), !.
updateText(_, _, _, _, _).
  
updateText(_Sp, TextOut, TextOut) :- !.
updateText(Sp, TextOut, _TextIn) :-
  sp2(Sp,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, _Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Res),
  SpelerIn = sp2(Sp,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, TextOut,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Res),
  format(Msg, "%s\nVeranderingen in de opmerkingen bewaren?", Naam),
  1 = dlg_MessageBox( "Opmerkingenveld", Msg, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
  logf('Z',  SpelerIn, nee), !.
updateText(_, _, _).
  
handleLRGew(_, G2, ja, G2, T1, _, Num, Cat, 0, T1) :-
  handleWas(nee,  ja, T1, Num, Cat), !.
handleLRGew(G1, _, G1, ja, _, T2, Num, Cat, 1, T2) :-
  handleWas(nee,  ja, T2, Num, Cat), !.

enableGew(Sp,Win,Knop) :-
  oproepen(ja),
  wd_planned1X(Wed,SP,alleplanning),
  Wed = w(_, _, _, nee, _, _, _, _),
  dialog_SetState(Win, [enable(Knop, b_True)]), !.
enableGew(_Sp,_Win,_Knop).
telButton(_Win, Tel1, Tel2, Tel3, OpBut1, OpBut2, OpBut3) :-
  dialog_SetState(_Win, [enable(OpBut1, b_False), enable(Opbut2, b_False), enable(Opbut3, b_False),
  			show(OpBut1, b_False), show(Opbut2, b_False), show(Opbut3, b_False)]),
  telButton(_Win, Tel1, OpBut1, "H"),
  telButton(_Win, Tel2, OpBut2, "W"),
  telButton(_Win, Tel3, OpBut3, "M"), !.
telButton(_, _, _, _, _, _, _).

telButton(_Win, Tel, Button, Type) :-
  CtrlWin1 = win_GetCtlHandle(_Win, Button),
  win_SetText(CtrlWin1, ""),
  Tel <> "",
  dialog_SetState(_Win, [enable(Button, b_True),show(Button, b_True)]),
  format(Opdruk, "%s: %s", Type, Tel),
  win_SetText(CtrlWin1, Opdruk), !.
telButton(_Win, _, _Button, _Type).

zetIncl(Win, Ctl, Gaant) :-
  Gaant > 1,
  dialog_SetState(Win,[show(Ctl, b_True), enable(Ctl, b_True)]), !.
zetIncl(Win, Ctl, _Gaant) :-
  dialog_SetState(Win,[show(Ctl, b_False), enable(Ctl, b_False)]), !.

slaaf(b_True, Num, Cat, Num, Cat).

zetOpGew(LR, _, _Sp, b_True, NM, CatM, Slave ) :-			% allemaal
  oproepItem(LR, _Index, mijnoproepen, Num, Cat, _T, LRInWeds, _BW),
  not(slaaf(Slave, NM, CatM, Num, Cat)),
  wd(Num,Cat,T1,T2,_,_,_),
  getbacktrack(Here),
  w2(Num,Cat,Tijd1,G1,G2,Fix,Tijd2,Bn), 
  cutbacktrack(Here),
  handleLRGew(G1, G2, Gew1, Gew2,T1,T2,Num,Cat,LRInWeds,_),
  logf('A',w2(Num,Cat,Tijd1,Gew1,Gew2,Fix,Tijd2,Bn),ja),
  fail.
zetOpGew(LR, _, _Sp, b_True, NM, CatM, Slave) :-			% allemaal
  oproepItem(LR, _Index, mijnoproepen, Num, Cat, Tg, _LRInWeds, _BW),	% 22.8.2009
  not(slaaf(Slave, NM, CatM, Num, Cat)),
  ws(Num,Cat,Tg,Tijd),		% er was eens een planning
  not(w2(Num,Cat,_,_,_,_,_,_)),	% nu geen planning
  logf('R', ws(Num,Cat,Tg,Tijd), ja),
  fail.
zetOpGew(LR, mijnoproepen, _Sp, b_False, Num, Cat, b_False) :-	% eentje en niet slave
  oproepItem(LR, _Index, mijnoproepen, Num, Cat, _T, LRInWeds, _BW),
  wd(Num,Cat,T1,T2,_,_,_),
  getbacktrack(Here),
  w2(Num,Cat,Tijd1,G1,G2,Fix,Tijd2,Bn), 
  cutbacktrack(Here),
  handleLRGew(G1, G2, Gew1, Gew2,T1,T2,Num,Cat,LRInWeds,_),
  logf('A',w2(Num,Cat,Tijd1,Gew1,Gew2,Fix,Tijd2,Bn),ja),
  fail.
zetOpGew(LR, mijnoproepen, _Sp, b_False, Num, Cat, b_False) :-	% eentje en niet slave
  oproepItem(LR, _Index, mijnoproepen, Num, Cat, Tg, _LRInWeds, _),	% 22.8.2009
  ws(Num,Cat,Tg,Tijd),		% er was eens een planning
  not(w2(Num,Cat,_,_,_,_,_,_)),	% nu geen planning
  logf('R', ws(Num,Cat,Tg,Tijd), ja),
  fail.
zetOpGew(_LR, _, _Sp, _, _, _, _).

saveAfzegIndexes(LR, Iin, Pers) :-
  assert(hulpindex(Iin)),
  ws(Num,Cat,T,_Tijd),		% er was eens een planning
  tgnst_num(T, Pers),
  not(w2(Num,Cat,_,_,_,_,_,_)),	% nu geen planning
  hulpindex(Index),
  assert(oproepItem(LR, Index, mijnoproepen, Num, Cat, T, 0, b_False)),	% 22.8.2009
  I2 = Index + 1,
  assert(hulpindex(I2)),
  fail.
saveAfzegIndexes(_, _, _).

extraSpatie(T, "", T) :- !.
extraSpatie(T1, _, T2) :-
  concat(" ", T1, T2).

initOpm(s(Sp)) :-
  sp2(Sp,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,Opm,_,_,_,_,_,_,_),
  Opm1 = opmZuiver(Opm),
  asserta(editcontroltext(idcp_custom1, Opm1, b_True, ff_Helvetica,b_False,9,1)),
  asserta(editcontroltext(idcp_custom2, "x!x", b_True, ff_Helvetica,b_False,9,1)), !.
initOpm(pw(Sp)) :-
  sp2(Sp,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,Opm,_,_,_,_,_,_,_),
  Opm1 = opmZuiver(Opm),
  asserta(editcontroltext(idcp_custom1, Opm1, b_True, ff_Helvetica,b_False,9,1)),
  asserta(editcontroltext(idcp_custom2, "x!x", b_True, ff_Helvetica,b_False,9,1)), !.
initOpm(p(Sp1,Sp2)) :-
  sp2(Sp1,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,Opm1,_,_,_,_,_,_,_),
  Opm1Z = opmZuiver(Opm1),
  asserta(editcontroltext(idcp_custom1, Opm1Z, b_True, ff_Helvetica,b_False,9,1)),
  sp2(Sp2,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,Opm2,_,_,_,_,_,_,_),
  Opm2Z = opmZuiver(Opm2),
  asserta(editcontroltext(idcp_custom2, Opm2Z, b_True, ff_Helvetica,b_False,9,1)), !.

zetBericht(IN, b_True) :-
  fronttoken(In,_,_), !.
zetBericht(_, b_False).

eenling(s(Sp), Sp) :- !.
eenling(pw(Sp), Sp).

smsmogelijk(Nr, b_True) :-
  sp2(Nr,_Sex,_Naam,Telthuis,_Verh,_RatingE,_RatingD,_Betaald,_UitKant, _Club, _Adres, _Woonpl, _BondsNummer, _ES, _DS, _Gebdatum, Telwerk, Telmobiel, _EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Res),
  _MobielNr = selectMobielNr(Telthuis, Telwerk, TelMobiel, smsbulk), !.
smsmogelijk(_Nr, b_False).

  dlg_plangegevens_Create(Handeling, Parent, Titel, Tgnst, Num, Cat, Result, Slave):-
	retractall(oproepItem(_,_,_,_,_,_,_,_)),
	initOpm(Tgnst),
	Parm = cast(long, Titel),
	assert(personen1(Handeling, Tgnst, Num, Cat, Slave)),
	assert(resultaat1(0)),
	win_CreateDynDialog(Parent,
		[
%BEGIN Plangegevens, WinDefList, 12:03:43-22.3.2013, Code automatically updated!
		 dlg_font(wdef(dlg_plangegevens_DlgType,dlg_plangegevens_RCT,dlg_plangegevens_Title,u_DlgBase),
		 	  dlg_plangegevens_Font,dlg_plangegevens_FSize,dlg_plangegevens_Flags),
		 customctl(wdef(wc_Custom,rct(1,99,150,133),"Custom1",u_DlgBase),"pdcedit",idcp_custom1,[wsf_Group,wsf_TabStop]),
		 customctl(wdef(wc_Custom,rct(152,99,306,133),"Custom2",u_DlgBase),"pdcedit",idcp_custom2,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_CheckBox,rct(19,134,104,144),"&Inclusief alle overige",u_DlgBase),idcp_voor_alles,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_CheckBox,rct(206,134,289,144),"Inclusief alle &overige",u_DlgBase),idcp_voor_alles1,[wsf_Auto,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(46,144,94,156),"Em&ail",u_DlgBase),idcp_stuur_email1,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(46,172,94,184),"&Afdrukken",u_DlgBase),idcp_afdrukken,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(206,144,254,156),"&Email",u_DlgBase),idcp_stuuremail2,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(206,172,254,184),"A&fdrukken",u_DlgBase),idcp_afdrukken1,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(109,135,194,152),"&Geïnformeerd",u_DlgBase),idc_gewaarschuwd,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(109,152,194,169),"nog &Niet geïnformeerd",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(3,202,35,219),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(270,202,302,219),"&Cancel",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(46,158,94,170),"&Sms",u_DlgBase),idcp_plangegevens_sms,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(206,158,254,170),"S&ms",u_DlgBase),idcp_plangegevens_smsr,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_LBox,rct(1,11,150,90),"",u_DlgBase),idcp_deelname,[wsf_Group,wsf_VScroll,wsf_NoIntegralHeight,wsf_UseTabStops,wsf_MultiSelect]),
		 ctl(wdef(wc_Text,rct(6,0,145,12),"Persoon1",u_DlgBase),idctp_persoon1,[wsf_AlignCenter]),
		 ctl(wdef(wc_LBox,rct(152,11,306,90),"",u_DlgBase),idcp_deelname2,[wsf_Group,wsf_VScroll,wsf_NoIntegralHeight,wsf_UseTabStops,wsf_MultiSelect]),
		 ctl(wdef(wc_Text,rct(153,0,301,12),"Persoon2",u_DlgBase),idcp_persoon2,[wsf_AlignCenter]),
		 ctl(wdef(wc_Text,rct(128,191,170,202),"Telefoon",u_DlgBase),idctp_telefoon,[wsf_AlignCenter]),
		 ctl(wdef(wc_Text,rct(3,160,40,170),"x=afgelast",u_DlgBase),idctp_xafgelast,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(3,186,40,195),"v=verzet",u_DlgBase),idctp_vverzet,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(3,151,40,161),"?=gepland",u_DlgBase),idcp_plangegevens_19,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(1,90,51,98),"Opmerkingen:",u_DlgBase),idcp_plangegevens_24,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(50,189,112,199),"opbellen1",u_DlgBase),idcp_opbellen1,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(50,202,112,212),"opbellen2",u_DlgBase),idcp_opbellen2,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(51,215,113,225),"opbellen5",u_DlgBase),idcp_opbellen5,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(176,190,238,200),"opbellen3",u_DlgBase),idcp_opbellen3,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(176,202,238,212),"opbellen4",u_DlgBase),idcp_opbellen4,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(176,215,238,225),"opbellen6",u_DlgBase),idcp_opbellen6,[wsf_AlignLeft]),
		 ctl(wdef(wc_GroupBox,rct(48,184,255,229),"",u_DlgBase),idcp_groupbox,[])
%END Plangegevens, WinDefList
		],dlg_plangegevens_eh,Parm),
	resultaat1(Result).

%BEGIN Plangegevens, idc_ok _CtlInfo
  dlg_plangegevens_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-!,
	personen1(_Handeling, Tgnst, _Num, _Cat, _),	
	EditWin1 = win_GetCtlHandle(_Win, idcp_custom1),
	retract(editcontroltext(idcp_custom1, TextO1, _, _, _, _,_)),
	Text1 = edit_GetText(EditWin1),
	EditWin2 = win_GetCtlHandle(_Win, idcp_custom2),
	retract(editcontroltext(idcp_custom2, TextO2, _, _, _, _,_)),
	Text2 = edit_GetText(EditWin2),
	updateText(Tgnst, Text1, TextO1, Text2, TextO2),	% 22.8.2009 wordt niets mee gedaan
	win_Destroy(_Win),
	!.
%END Plangegevens, idc_ok _CtlInfo
%MARK Plangegevens, new events

%BEGIN Plangegevens, idcp_plangegevens_smsr _CtlInfo
  dlg_plangegevens_eh(_Win,e_Control(idcp_plangegevens_smsr,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	personen1(_, p(_, Persoon), _Num, _Cat, _), 
        stuurSMS(_Win, Persoon),
	!.
%END Plangegevens, idcp_plangegevens_smsr _CtlInfo

%BEGIN Plangegevens, idcp_plangegevens_sms _CtlInfo
  dlg_plangegevens_eh(_Win,e_Control(idcp_plangegevens_sms,_CtrlType,_CtrlWin,_CtlInfo),0):-
	personen1(_, p(Persoon, _), _Num, _Cat, _), 
        stuurSMS(_Win, Persoon),
	!.
  dlg_plangegevens_eh(_Win,e_Control(idcp_plangegevens_sms,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	personen1(_, s(Persoon), _Num, _Cat, _), 
        stuurSMS(_Win, Persoon),
	!.
%END Plangegevens, idcp_plangegevens_sms _CtlInfo

%BEGIN Plangegevens, idcp_afdrukken1 _CtlInfo
  dlg_plangegevens_eh(_Win,e_Control(idcp_afdrukken1,_CtrlType,_CtrlWin,_CtlInfo),0):-
	personen1(_, p(_, Persoon), _Num, _Cat, _), 
	stuurOproepBericht(Persoon, Onderwerp, Bericht), 
	closefile(work),
	get_TempDir(_, TempFile),
	file_str(TempFile, Bericht),
	dlg_rapport_Create(_Win, Onderwerp,"Oproep",b_False), 
	!.
  dlg_plangegevens_eh(_Win,e_Control(idcp_afdrukken1,_CtrlType,_CtrlWin,_CtlInfo),0):- !.
%END Plangegevens, idcp_afdrukken1 _CtlInfo

%BEGIN Plangegevens, idcp_afdrukken _CtlInfo
  dlg_plangegevens_eh(_Win,e_Control(idcp_afdrukken,_CtrlType,_CtrlWin,_CtlInfo),0):-
	personen1(_, p(Persoon, _), _Num, _Cat, _), 
	stuurOproepBericht(Persoon, Onderwerp, Bericht), 
	closefile(work),
	get_TempDir(_, TempFile),
	file_str(TempFile, Bericht),
	dlg_rapport_Create(_Win, Onderwerp,"Oproep",b_False), 
	!.
  dlg_plangegevens_eh(_Win,e_Control(idcp_afdrukken,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	personen1(_, Tg, _Num, _Cat, _), 
	eenling(Tg, Persoon),
	stuurOproepBericht(Persoon, Onderwerp, Bericht), 
	closefile(work),
	get_TempDir(_, TempFile),
	file_str(TempFile, Bericht),
	dlg_rapport_Create(_Win, Onderwerp,"Oproep",b_False), 
	!.
%END Plangegevens, idcp_afdrukken _CtlInfo

%BEGIN Plangegevens, idcp_stuuremail2 _CtlInfo
  dlg_plangegevens_eh(_Win,e_Control(idcp_stuuremail2,_CtrlType,_CtrlWin,_CtlInfo),0):-
	personen1(_, p(_, Persoon), _Num, _Cat, _), 
        stuurBerichtenOproep(_Win, collectie([Persoon])),
	!.
  dlg_plangegevens_eh(_Win,e_Control(idcp_stuuremail2,_CtrlType,_CtrlWin,_CtlInfo),0) :- !.
%END Plangegevens, idcp_stuuremail2 _CtlInfo

%BEGIN Plangegevens, idcp_stuur_email1 _CtlInfo
  dlg_plangegevens_eh(_Win,e_Control(idcp_stuur_email1,_CtrlType,_CtrlWin,_CtlInfo),0):-
	personen1(_, p(Persoon, _), _Num, _Cat, _), 
        stuurBerichtenOproep(_Win, collectie([Persoon])),
	!.
  dlg_plangegevens_eh(_Win,e_Control(idcp_stuur_email1,_CtrlType,_CtrlWin,_CtlInfo),0):-
	personen1(_, s(Persoon), _Num, _Cat, _), 
        stuurBerichtenOproep(_Win, collectie([Persoon])),
	!.
  dlg_plangegevens_eh(_Win,e_Control(idcp_stuur_email1,_CtrlType,_CtrlWin,_CtlInfo),0):-!.
%END Plangegevens, idcp_stuur_email1 _CtlInfo


%BEGIN Plangegevens, idcp_deelname2 selchanged
  dlg_plangegevens_eh(_Win,e_Control(idcp_deelname2,_CtrlType,_CtrlWin,selchanged),0):-
	selectGeen(_CtrlWin),
	trap(CW = win_GetCtlHandle(_Win, idcp_voor_alles1),_,fail),
	win_SendEvent(_Win,e_Control(idcp_voor_alles1,wc_CheckBox,CW, activated())),
	!.
%END Plangegevens, idcp_deelname2 selchanged

%BEGIN Plangegevens, idcp_deelname selchanged
  dlg_plangegevens_eh(_Win,e_Control(idcp_deelname,_CtrlType,_CtrlWin,selchanged),0):-
	selectGeen(_CtrlWin),
	trap(CW = win_GetCtlHandle(_Win, idcp_voor_alles),_,fail),
	win_SendEvent(_Win,e_Control(idcp_voor_alles,wc_CheckBox,CW, activated())),
	!.
%END Plangegevens, idcp_deelname selchanged

%BEGIN Plangegevens, e_Destroy
  dlg_plangegevens_eh(_Win,e_Destroy,0):-
	%ParentWin = win_GetParent(_Win),
	retractall(oproepItem(_,_,_,_,_,_,_,_)),
	retractall(oprPersoon(_)),
	retractall(editcontroltext(idcp_custom1, _, _, _, _, _, _)),
	retractall(editcontroltext(idcp_custom2, _, _, _, _, _, _)),
	%trap(win_BringToTop(ParentWin),_,true),
	!.
%END Plangegevens, e_Destroy

%BEGIN Plangegevens, idcp_voor_alles1 _CtlInfo
  dlg_plangegevens_eh(_Win,e_Control(idcp_voor_alles1,_CtrlType,_CtrlWin,_CtlInfo),0):-
	Checked = win_IsChecked(_CtrlWin),
	LbWin = win_GetCtlHandle(_Win, idcp_deelname2),
	oproepItem(1, Index, _, _, _, _, _, _),	
	lbox_SetSel(LbWin, Index, Checked),
	fail.
  dlg_plangegevens_eh(_Win,e_Control(idcp_voor_alles1,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LbWin = win_GetCtlHandle(_Win, idcp_deelname2),
	personen1(Handeling, _, Num, Cat, _),
	oproepItem(1, Index, Handeling, Num, Cat, _, _, _BW),	
	lbox_SetSel(LbWin, Index, b_True),
	!.
%END Plangegevens, idcp_voor_alles1 _CtlInfo

%BEGIN Plangegevens, idcp_voor_alles _CtlInfo
  dlg_plangegevens_eh(_Win,e_Control(idcp_voor_alles,_CtrlType,_CtrlWin,_CtlInfo),0):-
	Checked = win_IsChecked(_CtrlWin),
	LbWin = win_GetCtlHandle(_Win, idcp_deelname),
	oproepItem(0, Index, _, _, _, _, _, _),	
	lbox_SetSel(LbWin, Index, Checked),
	fail.
  dlg_plangegevens_eh(_Win,e_Control(idcp_voor_alles,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LbWin = win_GetCtlHandle(_Win, idcp_deelname),
	personen1(Handeling, _, Num, Cat, _),
	oproepItem(0, Index, Handeling, Num, Cat, _, _, _),	
	lbox_SetSel(LbWin, Index, b_True),
	!.
%END Plangegevens, idcp_voor_alles _CtlInfo

%BEGIN Plangegevens, idc_cancel _CtlInfo
  dlg_plangegevens_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	personen1(_Handeling, Tgnst, _Num, _Cat, _Slave),	
	EditWin1 = win_GetCtlHandle(_Win, idcp_custom1),
	retract(editcontroltext(idcp_custom1, TextO1, _, _, _, _, _)),
	Text1 = edit_GetText(EditWin1),
	EditWin2 = win_GetCtlHandle(_Win, idcp_custom2),
	retract(editcontroltext(idcp_custom2, TextO2, _, _, _, _,_)),
	Text2 = edit_GetText(EditWin2),
	updateText(Tgnst, Text1, TextO1, Text2, TextO2),
	win_Destroy(_Win),
	!.
%END Plangegevens, idc_cancel _CtlInfo

%BEGIN Plangegevens, idc_help _CtlInfo
  dlg_plangegevens_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Planninggegevens"),
	!.
%END Plangegevens, idc_help _CtlInfo

%BEGIN Plangegevens, idc_gewaarschuwd _CtlInfo
  dlg_plangegevens_eh(_Win,e_Control(idc_gewaarschuwd,_CtrlType,_CtrlWin,_CtlInfo),0):-
	personen1(Handeling, Tgnst, NumP, CatP, _),	
	EditWin1 = win_GetCtlHandle(_Win, idcp_custom1),
	retract(editcontroltext(idcp_custom1, TextO1, _, _, _, _, _)),
	Text1 = edit_GetText(EditWin1),
	EditWin2 = win_GetCtlHandle(_Win, idcp_custom2),
	retract(editcontroltext(idcp_custom2, TextO2, _, _, _, _, _)),
	Text2 = edit_GetText(EditWin2),
	updateText(Tgnst, Text1, TextO1, Text2, TextO2),
	loglock(mdi, "dlg_plangegevens"),
	eenling(Tgnst, Sp),
	personen1(Handeling, Tgnst, NumP, CatP, Slave),
	CtrlWin = win_GetCtlHandle(_Win, idcp_voor_alles),
	Checked = win_IsChecked(CtrlWin),
	zetOpGew(0, Handeling, Sp, Checked, NumP, CatP, Slave),
	fail.
  dlg_plangegevens_eh(_Win,e_Control(idc_gewaarschuwd,_CtrlType,_CtrlWin,_CtlInfo),0):-
	personen1(Handeling, p(Sp,_), NumP, CatP, Slave),
	CtrlWin = win_GetCtlHandle(_Win, idcp_voor_alles),
	Checked = win_IsChecked(CtrlWin),
	zetOpGew(0, Handeling, Sp, Checked, NumP, CatP, Slave),
	fail.
  dlg_plangegevens_eh(_Win,e_Control(idc_gewaarschuwd,_CtrlType,_CtrlWin,_CtlInfo),0):-
	personen1(Handeling, p(_,Sp), NumP, CatP, Slave),
	CtrlWin = win_GetCtlHandle(_Win, idcp_voor_alles1),
	Checked = win_IsChecked(CtrlWin),
	zetOpGew(1, Handeling, Sp, Checked, NumP, CatP, Slave),
	fail.
  dlg_plangegevens_eh(_Win,e_Control(idc_gewaarschuwd,_CtrlType,_CtrlWin,_CtlInfo),0):-
	logclose(),
	assert(resultaat1(1)),
	win_Destroy(_Win),
	update_uitslagen("idc_gewaarschuwd"),
	update_situatie(),
	update_waarsch(),
	%handle
	!.
%END Plangegevens, idc_gewaarschuwd _CtlInfo

%BEGIN Plangegevens, e_Create
  dlg_plangegevens_eh(_Win,e_Create(_CreationData),0):-
	Titel = cast(string, _CreationData),
	win_SetText(_Win, Titel),
        %modem_enabled(MM, _ComP, _Vorwahl, _Speed, _ToonPuls, _Geluid),
	personen1(Handeling, Tgnst, Num, Cat, _),		% de eenling
	%metModem(Tgnst, MM, _Win),
	eenling(Tgnst, Sp),
	sp2(Sp,_,Naam,Tel,Verh,_RatingE,_RatingD,_,BegTijd, _Club, _Adres, _Woonpl, _BondsNr, _ES, _DS, _Gebdatum, Tel1, Tel2, EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Res),
	N1Win = win_GetCtlHandle(_Win, idctp_persoon1),
	win_SetText(N1Win, Naam),
	zetBericht(EMail, EmailAanw),
	smsMogelijk(Sp, SMSmogelijk),
	dialog_SetState(_Win, [
	  show(idcp_deelname2,b_False),show(idcp_persoon2,b_False), show(idcp_custom2, b_False),
	  show(idcp_voor_alles1,b_False),enable(idcp_voor_alles1,b_False),
	  show(idcp_stuuremail2, b_False), show(idctp_telefoon,b_False),
	  show(idcp_opbellen3,b_False),show(idcp_opbellen4,b_False),show(idcp_opbellen6,b_False),
	  show(idcp_plangegevens_smsr, b_False),show(idcp_afdrukken1,b_False),
	  % enable(idcp_opbellen3,b_False),enable(idcp_opbellen4,b_False),enable(idcp_opbellen6,b_False),
	  enable(idcp_stuur_email1, EmailAanw), enable(idcp_plangegevens_sms, SMSmogelijk)]),
	telButton(_Win, Tel, Tel1, Tel2, idcp_opbellen1, idcp_opbellen2, idcp_opbellen5),
	tijd_str(BegTijd,TStr,_), 
	format(TStr1, "Beschikbaar na %s op werkdagen.", TStr),
	findall(VerhStr, verhl_strl(Verh, VerhStr), Verhl),
	findall(WId,wd_planned1X(WId,Sp,alleplanning),WedstrijdList),
	qsortw(WedstrijdList, WedstrijdListS),
	speeltSList(0,WedstrijdListS,0,NieuwGaant,ja,[],OnderdelenU,0,Gaant), 
	afzeggenSList(Sp, OnderdelenU, OnderdelenUA, Gaant, Gaant1),
	saveAfzegIndexes(0, NieuwGaant, Sp),
	te_plannenList(Sp, TePlannen),
	append(OnderdelenUA, TePlannen, EnTePlannen),
	append(EnTePlannen, [TStr1|Verhl]/*Onderdelen*/, OnderdelenU1),
	zetIncl(_Win, idcp_voor_alles, Gaant1),
	D1Win = win_GetCtlHandle(_Win, idcp_deelname),
	lbox_SetTabStops(D1Win, [10, 15]),
	lbox_Add(D1Win, OnderdelenU1),
	LbWin = win_GetCtlHandle(_Win, idcp_deelname),
	oproepItem(0, Index, Handeling, Num, Cat, _, _, _),	
	lbox_SetSel(LbWin, Index, b_True),
	!.
  dlg_plangegevens_eh(_Win,e_Create(_CreationData),0):-
        %modem_enabled(MM, _ComP, _Vorwahl, _Speed, _ToonPuls, _Geluid),
	personen1(Handeling, p(Sp1,Sp2), Num, Cat, _),	% het koppel
	enableGew(Sp1,_Win, idc_gewaarschuwd),
	sp2(Sp1,_,Naam,Tel,Verh,_RatingE1,_RatingD1,_,BegTijd, _Club1, _Adres1, _Woonpl1, _BondsNr1, _ES1, _DS1, _Gebdatum1, Tel11, Tel21, EMail1, _Opm1,_Gezien1,_RangPosE1,_RangPosD1,_Incasso1,_CheckGeg1,_Log1,_Res1),
	N1Win = win_GetCtlHandle(_Win, idctp_persoon1),
	win_SetText(N1Win, Naam),
	telButton(_Win, Tel, Tel11, Tel21, idcp_opbellen1, idcp_opbellen2, idcp_opbellen5),
	tijd_str(BegTijd,TStr,_), 
	format(TStr1, "Beschikbaar na %s op werkdagen.", TStr),
	findall(VerhStr, verhl_strl(Verh, VerhStr), Verhl),
	findall(WId,wd_planned1X(WId,Sp1,alleplanning),WedstrijdList),
	qsortw(WedstrijdList, WedstrijdListS),
	zetBericht(EMail1, EmailAanw1),
	speeltSList(0,WedstrijdListS,0,NieuwGaant,ja,[],OnderdelenU,0,Gaant), 
	te_plannenList(Sp1, TePlannen1),
	afzeggenSList(Sp1, OnderdelenU, OnderdelenUA1, Gaant, Gaant3),
	saveAfzegIndexes(0, NieuwGaant, Sp1),
	append(OnderdelenUA1, TePlannen1, EnTePlannen1),
	append(EnTePlannen1, [TStr1|Verhl]/*Onderdelen*/, OnderdelenU1),
	zetIncl(_Win, idcp_voor_alles, Gaant3),
	D1Win = win_GetCtlHandle(_Win, idcp_deelname),
	lbox_SetTabStops(D1Win, [10, 15]),
	lbox_Add(D1Win, OnderdelenU1),
	sp2(Sp2,_,Naam2,Tel2,Verh2,_RatingE2,_RatingD2,_,BegTijd2, _Club2, _Adres2, _Woonpl2, _BondsNr2, _ES2, _DS2, _Gebdatum2, Tel12, Tel22, EMail2, _Opm2,_Gezien2,_RangPosE2,_RangPosD2,_Incasso2,_CheckGeg2,_Log2,_Res2),
	N2Win = win_GetCtlHandle(_Win, idcp_persoon2),
	win_SetText(N2Win, Naam2),
	telButton(_Win, Tel2, Tel12, Tel22, idcp_opbellen3, idcp_opbellen4, idcp_opbellen6),
	tijd_str(BegTijd2,TStr2,_), 
	format(TStr3, "Beschikbaar na %s op werkdagen.", TStr2),
	findall(VerhStr2, verhl_strl(Verh2, VerhStr2), Verhl2),
	findall(WIdx,wd_planned1X(WIdx,Sp2,alleplanning),WedstrijdList2),
	qsortw(WedstrijdList2, WedstrijdListS2),
	speeltSList(1,WedstrijdListS2,0,NieuwGaant1,ja,[],OnderdelenU2,0,Gaant1), 
	te_plannenList(Sp2, TePlannen2),
	afzeggenSList(Sp2, OnderdelenU2, OnderdelenUA2, Gaant1, Gaant4),
	saveAfzegIndexes(1, NieuwGaant1, Sp2),
	append(OnderdelenUA2, TePlannen2, EnTePlannen2),
	append(EnTePlannen2, [TStr3|Verhl2]/*Onderdelen*/, OnderdelenU22),
	zetBericht(EMail2, EmailAanw2),
	smsMogelijk(Sp1, SMSmogelijk1),
	smsMogelijk(Sp2, SMSmogelijk2),
	dialog_SetState(_Win, [	enable(idcp_stuur_email1, EmailAanw1),enable(idcp_stuuremail2, EmailAanw2), enable(idcp_plangegevens_sms, SMSmogelijk1), enable(idcp_plangegevens_smsr, SMSmogelijk2)]),
	zetIncl(_Win, idcp_voor_alles1, Gaant4),
	D2Win = win_GetCtlHandle(_Win, idcp_deelname2),
	lbox_SetTabStops(D2Win, [10, 15]),
	lbox_Add(D2Win, OnderdelenU22),
	LbWin1 = win_GetCtlHandle(_Win, idcp_deelname),	% selecteer initial
	oproepItem(0, Index1, Handeling, Num, Cat, _, _, _),	
	lbox_SetSel(LbWin1, Index1, b_True),
	LbWin2 = win_GetCtlHandle(_Win, idcp_deelname2),
	oproepItem(1, Index2,Handeling, Num, Cat, _, _, _),
	lbox_SetSel(LbWin2, Index2, b_True),
	!.
%END Plangegevens, e_Create

  dlg_plangegevens_eh(_,_,_):-!,fail.

%END_DLG Plangegevens

beginTijd(0, _, "") :- !.
beginTijd(_, TStr, TStr1) :-
  format(TStr1, "Beschikbaar na %s op werkdagen.\n", TStr), !.

stuurOproepBericht(Persoon, Onderwerp, Bericht) :-
	naam(Toer, _, _, _, _, _),
	format(Onderwerp, "Informatie betreffende %s", Toer),
	sp2(Persoon,_,Naam,_Tel,Verh,_RatingE1,_RatingD1,_,BegTijd, _Club1, _Adres1, _Woonpl1, _BondsNr1, _ES1, _DS1, _Gebdatum1, _Tel11, _Tel21, _EMail1, _Opm1,_Gezien1,_RangPosE1,_RangPosD1,_Incasso1,_CheckGeg1,_Log1,_Res1),
	naamscan(ja,b_False, Naam, Naam1, _Voornaam,_Landcode),
	tijd_str(BegTijd,TStr,_), 
	beginTijd(BegTijd, TStr, TStr1),
	findall(VerhStr, verhl_strl(Verh, VerhStr), Verhl),
	findall(WId,wd_planned1X(WId,Persoon,alleplanning),WedstrijdList),
	qsortw(WedstrijdList, WedstrijdListS),
	speeltSListAfdr(0,WedstrijdListS,0,_,nee,[],OnderdelenU,0,Gaant), 
	afzeggenSList(Persoon, OnderdelenU, OnderdelenUA, Gaant, _Gaant1),
	te_plannenList(Persoon, TePlannen),
	append(OnderdelenUA, TePlannen, EnTePlannen),
	list_to_string(EnTePlannen, "\n", EMsg),
	list_to_string(Verhl, "\n", Gegevens),
	format(Bericht, "Beste %s.\nHet volgende staat gepland:\n%s\nOpgegeven verhinderingen:\n%s%s\nGaarne ontvangstbevestiging!", Naam1, EMsg, TStr1, Gegevens), !.



%BEGIN_DLG Gewaarschuwd
/**************************************************************************
	Creation and event handling for dialog: Gewaarschuwd
**************************************************************************/

constants

%BEGIN Gewaarschuwd, CreateParms, 09:59:57-7.2.2000, Code automatically updated!
  dlg_gewaarschuwd_ResID = idd_gewaarschuwd
  dlg_gewaarschuwd_DlgType = wd_Modal
  dlg_gewaarschuwd_Help = idh_oproepenafzeggen
%END Gewaarschuwd, CreateParms

predicates

  dlg_gewaarschuwd_eh : EHANDLER
  dlg_gewaarschuwd_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST,integer,wedscat,integer)
  dlg_gewaarschuwd_update(DIALOG_VAL_LIST)

clauses

  dlg_gewaarschuwd_Create(Parent, Winnaars, Num, Cat, OE):-
	bitright(Num, 1, Num1),
	bitand(Num, 1, OE1),
	pos(Cat, Top, _, _ ),  		% 
	dim(Top, TopDim), !,
	dim(Num,NoDim),                  	% bepaal de ronde
	repc(nee,' ',nee,nee,nee,OE1,TopDim,NoDim,Num1,Cat,onbekend,alleplanning, Plan,_Score2,_TijdLog2,_,_,_,_),
	format(IDCT_VOLGENDE_RONDE_OP_TITLE, "Vervolgwedstrijd op %s", Plan),
	format(IDCT_IS_DE_WINNAAR_GEWAARSCHUWD_TITLE, "Is/zijn % gewaarschuwd?",Winnaars),
%MARK Gewaarschuwd, new variables

	dialog_CreateModal(Parent,dlg_gewaarschuwd_ResID,"",
  		[
%BEGIN Gewaarschuwd, ControlList, 09:59:57-7.2.2000, Code automatically updated!
		df(idct_volgende_ronde_op,statictext(IDCT_VOLGENDE_RONDE_OP_TITLE),nopr),
		df(idct_is_de_winnaar_gewaarschuwd,statictext(IDCT_IS_DE_WINNAAR_GEWAARSCHUWD_TITLE),nopr)
%END Gewaarschuwd, ControlList
		],
		dlg_gewaarschuwd_eh,0,VALLIST,ANSWER),
	dlg_gewaarschuwd_handle_answer(ANSWER,VALLIST,Num, Cat, OE).

  dlg_gewaarschuwd_handle_answer(idc_ok,VALLIST, Num, Cat, OE):-
	w2(Num,Cat,Tijd,G1,G2,Fix,Spoor, BNo),
	zetGew(OE, G1, G2, G1N, G2N), !,
	logf('A',w2(Num,Cat,Tijd,G1N,G2N,Fix,Spoor,BNo),nee),
	dlg_gewaarschuwd_update(VALLIST).
  dlg_gewaarschuwd_handle_answer(idc_cancel,_,_,_,_):-!.  % Handle Esc and Cancel here
  dlg_gewaarschuwd_handle_answer(_,_,_,_,_):-
	errorexit().

  dlg_gewaarschuwd_update(_VALLIST):-
%BEGIN Gewaarschuwd, Update controls, 09:59:57-7.2.2000, Code automatically updated!
%END Gewaarschuwd, Update controls
	true.

%MARK Gewaarschuwd, new events



  dlg_gewaarschuwd_eh(_,_,_):-!,fail.

%END_DLG Gewaarschuwd


%BEGIN_TLB Uitslagen, 14:43:55-20.2.2017, Code automatically updated!
/**************************************************************************
	Creation of toolbar: Uitslagen
**************************************************************************/

clauses

  tb_uitslagen_Create(_Parent):-
ifdef use_tbar
	toolbar_create(tb_bottom,0xC0C0C0,_Parent,
		[tb_ctrl(idts_popup,pushb,idb_menu,idb_menud,idb_grijs,"Menu;Popup menu (plannen etc.)",1,1),
		 tb_ctrl(id_Wedstrijd_uitslag_invoeren,pushb,idb_balw,idb_balwd,idb_grijs,"Uitslag invoeren (F9);Uitslag invoeren",1,1),
		 tb_ctrl(idr_schemaeen,pushb,idb_zoekopw,idb_zoekopp,idb_grijs,"Schema;Wedstrijd in schema",1,1),
		 tb_ctrl(idts_overzicht,pushb,idb_wedstrijd,idb_wedstrijdd,idb_grijs,"Wedstrijdoverzicht;Wedstrijdoverzicht",1,1),
		 tb_ctrl(idt_plan,pushb,idb_planw,idb_planap,idb_grijs,"Plan of verschuif;Plan of verschuif wedstrijd",1,1),
		 tb_ctrl(idt_spelers,pushb,idb_spelersw,idb_spelerswd,idb_planaanwijs,"personen;Personen",1,1),
		 tb_ctrl(idt_synchro,pushb,idb_synchro,idb_synchrod,idb_grijs,"synchroniseer uitslagen;synchroniseer uitslagen vanaf Internet",1,1),
		 separator,
		 tb_ctrl(id_Onderdeel_print,pushb,idb_printw,idb_printdown,idb_grijs,"Afdrukken;Agenda afdrukken",1,1),
		 separator,
		 separator,
		 separator]),
enddef
	true.
%END_TLB Uitslagen


%BEGIN_TLB Planning, 15:00:24-20.8.2016, Code automatically updated!
/**************************************************************************
	Creation of toolbar: Planning
**************************************************************************/

clauses

  tb_planning_Create(_Parent):-
ifdef use_tbar
	toolbar_create(tb_top,0xFFFFFF,_Parent,
		[tb_ctrl(idt_aanwijs,pushb,idb_planaanwijs,idb_planaanwijs,idb_planaanwijs,"wijs te plannen wedstrijd aan;wijs te plannen wedstrijd (weer) aan",1,1),
		 tb_text(idt_12,tb_static,55,0,1,10,0x80,"Betreft:"),
		 tb_textExt(idt_2,tb_static,900,0,1,10,0x0,color_White,"HE1 etc. etc.")]),
enddef
	true.
%END_TLB Planning




%BEGIN_TLB Planbottom, 14:25:35-23.8.2011, Code automatically updated!
/**************************************************************************
	Creation of toolbar: Planbottom
**************************************************************************/

clauses

  tb_planbottom_Create(_Parent):-
ifdef use_tbar
	toolbar_create(tb_bottom,0xC0C0C0,_Parent,
		[tb_ctrl(idts_popup,pushb,idb_menu,idb_menud,idb_grijs,"Menu;Popup menu (plannen etc.)",1,1),
		 tb_ctrl(id_Wedstrijd_uitslag_invoeren,pushb,idb_balw,idb_balwd,idb_grijs,"Uitslag invoeren (F9);Uitslag invoeren",1,1),
		 tb_ctrl(idts_overzicht,pushb,idb_wedstrijd,idb_wedstrijdd,idb_grijs,"Wedstrijdoverzicht (F7);Wedstrijdoverzicht (planning)",1,1),
		 tb_ctrl(idt_plan,pushb,idb_planw,idb_planwd,idb_grijs,"Plan of verschuif;Plan of verschuif wedstrijd",1,1),
		 tb_ctrl(id_Wedstrijd_wis_plan1,pushb,idb_wissenw,idb_wissenwd,idb_grijs,"Wis planning;Wis de planning van deze partij",1,1),
		 tb_ctrl(idt_set_fixeer,pushb,idb_fixeerw,idb_fixeerp,idb_grijs,"Fixeer;Markeer als gefixeerd",1,1),
		 tb_ctrl(idt_tijdstip,pushb,idb_tijdstipwbmp,idb_tijdstipwd,idb_tijdstipwd,"Maak uitwijktijdstip voor de geselecteerde dag;Voeg een extra uitwijktijdstip toe op de geselecteerde dag.",1,1),
		 separator,
		 tb_text(idt_3,tb_static,300,0,1,10,0x0,"Eerstmogelijke tijdstip is geselecteerd")]),
enddef
	true.
%END_TLB Planbottom


predicates
procedure  draw_Bezwaren(window, string Bezwaren, integer Voff, integer Height, integer Width, boolean Kleur, integer Hoogte, color Achtergrond, color Voorgrond) - (i,i,i,i,i,i,o,i,i)
procedure  kleur(window, string, integer, integer, boolean Kleur,Width, color Achtergrond, color Voorgrond)
procedure  afm_bezwaren(window Win, string BezwarenOpmerkingen, integer Hin, integer Huit, integer Vin, integer Vuit) - (i,i,i,o,i,o)
nondeterm rodetekst(string)
procedure  lijnAls(window, string Opmerkingen, integer Hoogte1, integer Hoogte2, integer Hor) - (i, i, i, o, i)
procedure lijnAlsAfm(string, integer, integer) - (i,i,o)
procedure afm_nota(window, integer, wedscat, integer, integer, integer, integer) - (i,i,i,i,o,i,o)
procedure afm_klikhier(window, integer, integer, integer, integer) - (i,i,o,i,o)
procedure draw_klikhier(window, font, integer Vert, integer Hor) 
%procedure draw_klikhier(window, 

clauses

draw_klikhier(_Win, _Font, _Vert, _Hord) :-
  boventekens(No, Cat),
  wd(No, Cat, _, _, _, _, Nota),
  fronttoken(Nota, _, _), !.
draw_klikhier(_Win, _Font, _Vert, _Hord) :-
  notaVeld, !.
draw_klikhier(_Win, Font, Vert, Hord) :-
  win_SetBackColor(_Win,color_White),
  win_SetForeColor(_Win, color_LtGray),
  %win_SetBrush(_Win,brush(pat_Solid, color_White)),
  NewFont = font_SetAttrs(Font,[],8),
  win_SetFont(_Win, NewFont),
  Vert1 = Vert + 13,
  Area = rct(2,Vert, Hord, Vert1), 
  %draw_Rect(_Win,Area),
  draw_TextInRect(_Win,Area, " klik hier voor eventuele aantekening", -1,[dtext_left]).

afm_nota(_Win, No, Cat, Hor0, Hor1, Vert, Vert1) :-
  wd(No, Cat, _, _, _, _, Nota),
  fronttoken(Nota, _, _),
  Vert1 = Vert + 70,
  grootste(Hor0, 285, Hor1), !.
afm_nota(_Win, _, _, Hor0, Hor1, Vert, Vert1) :-
  notaVeld,
  Vert1 = Vert + 70,
  grootste(Hor0, 285, Hor1), !.
afm_nota(_Win, _No, _Cat, Hor0, Hor1, Vert0, Vert0) :-
  grootste(Hor0, 275, Hor1).

afm_klikhier(_Win, Hor0, Hor0, Vert0, Vert0) :-
  boventekens(No, Cat),
  wd(No, Cat, _, _, _, _, Nota),
  fronttoken(Nota, _, _), !.
afm_klikhier(_Win, Hor0, Hor0, Vert0, Vert0) :-
  notaVeld, !.
afm_klikhier(_Win, Hor0, Hor0, Vert0, Vert1) :-
  Vert1 = Vert0 + 10.

 
afm_bezwaren(Win, Bezwaren, Hin, Huit, Vin, Vuit) :-
  fronttoken(Bezwaren, _, _),
  searchstring(Bezwaren,"\n",FoundPos),
  N = FoundPos - 1,
  frontstr(N,Bezwaren,StartString,RestString),
  win_GetTextExtent(Win, Startstring, -1, Width, Height),
  frontstr(1, RestString, _, RestString1), !,
  Vin1 = Vin + Height,
  grootste(Hin, Width, Hin1),
  afm_bezwaren(Win, Reststring1, Hin1, Huit, Vin1, Vuit).
afm_bezwaren(Win, Bezwaren, Hin, Hin1, Vin, Vin1) :-
  fronttoken(Bezwaren, _, _), !,
  win_GetTextExtent(Win, Bezwaren, -1, Width, Height),
  Vin1 = Vin + Height,
  grootste(Hin, Width, Hin1).
afm_bezwaren(_Win, _Bezwaren, Hin, Hin1, Vin, Vin1) :-
  Vin1 = Vin + 1,
  Hin1 = Hin + 2.

lijnAls(Win, Opmerkingen, Hoogte1, Hoogte2, Hor) :-
  fronttoken(Opmerkingen, _, _),
  win_SetPen(Win,pen(1,ps_Solid,color_Black)),
  draw_Line(Win,pnt(0,Hoogte1),pnt(Hor,Hoogte1)),
  Hoogte2 = Hoogte1 + 1, !.
lijnAls(_, _Opmerkingen, H, H, _Hor).

lijnAlsAfm(Str, In, Uit) :-
  fronttoken(Str, _, _),
  Uit = In + 1, !.
lijnAlsAfm(_Str, Uit, Uit).

draw_Bezwaren(Win, Bezwaren, Voff, Height,Width, Kleur, Hoogte, Achtergrond,Voorgrond) :-
  frontstr(1, Bezwaren, Spits, Rest),		% van fixeerbezwaren	% 11.12.2002
  Spits = "\n",
  draw_Bezwaren(Win, Rest, Voff, Height,Width, Kleur, Hoogte, Achtergrond,Voorgrond), !.
draw_Bezwaren(Win, Bezwaren, Voff, Height,Width, Kleur, Hoogte, Achtergrond,Voorgrond) :-
  searchstring(Bezwaren,"\n",FoundPos),
  N = FoundPos - 1,
  frontstr(N,Bezwaren,StartString,RestString),
  Boff = Voff + Height,
  kleur(Win, Startstring, Voff, Boff, Kleur, Width,Achtergrond,Voorgrond), 
  frontstr(1, RestString, _, RestString1), !,
  draw_Bezwaren(Win, Reststring1, Boff, Height,Width, Kleur, Hoogte, Achtergrond,Voorgrond).
draw_Bezwaren(Win, Bezwaren, Voff, Height,Width, Kleur, Boff,Achtergrond,Voorgrond) :-
  trim_c(Bezwaren),
  not(Bezwaren = ""),
  Boff = Voff + Height,	!,			% laatste regel
  %win_SetBackColor(Win,Achtergrond),
  kleur(Win, Bezwaren, Voff, Boff,Kleur,Width,Achtergrond,Voorgrond). 
draw_Bezwaren(_Win, _Bezwaren, Voff, _Height,_,_,Voff,_,_).

rodetekst("oorgaande ronde niet").
rodetekst("oorgaande wedstrijd(").
rodetekst("erhinderd").
rodetekst("beschikbaar").
rodetekst("geblokkeerd").

kleur(Win, String, Voff, Boff,b_True,WidthM, Achtergrond,Voorgrond) :-
  searchstring(String,"=>",FoundPos),
  F1 = FoundPos + 1,
  frontstr(F1,String,ZwarteTekst,RodeTekst),
  TextArea = rct(2, Voff, WidthM, Boff), 
  win_SetBackColor(Win,Achtergrond),
  win_SetForeColor(Win, Voorgrond),
  win_SetBrush(Win,brush(pat_Solid, Achtergrond)),
  win_SetPen(Win,pen(1,ps_Solid,Achtergrond)),
  T1 = rect_Inflate(TextArea, 2, 0),
  draw_Rect(Win,T1),
  draw_TextInRect(Win,TextArea, ZwarteTekst, -1,[dtext_left]),
  win_GetTextExtent(Win, ZwarteTekst, -1, Width, _Height),
  W1 = Width + 1, 
  RoodArea = rct(W1, Voff, WidthM, Boff),
  win_SetForeColor(Win, color_red),
  draw_TextInRect(Win,RoodArea, RodeTekst, -1,[dtext_left]),
  win_SetForeColor(Win, color_black), !.
kleur(Win, String, Voff, Boff, b_True, Width,Achtergrond,_Voorgrond) :-
  rodetekst(Tkst),
  searchstring(String,Tkst,_),
  win_SetBackColor(Win,Achtergrond),
  win_SetForeColor(Win, color_red),
  RoodArea = rct(2, Voff, Width, Boff), 
  win_SetBrush(Win,brush(pat_Solid, Achtergrond)),
  win_SetPen(Win,pen(1,ps_Solid,Achtergrond)),
  T1 = rect_Inflate(RoodArea, 2, 0),
  draw_Rect(Win,T1),
  draw_TextInRect(Win,RoodArea, String, -1,[dtext_left]),
  win_SetForeColor(Win, color_black), !.
kleur(Win, String, Voff, Boff,_,Width,Achtergrond,Voorgrond) :-
  TextArea = rct(2, Voff, Width, Boff), 
  win_SetBackColor(Win,Achtergrond),
  win_SetForeColor(Win, Voorgrond),
  win_SetBrush(Win,brush(pat_Solid, Achtergrond)),
  win_SetPen(Win,pen(1,ps_Solid,Achtergrond)),
  T1 = rect_Inflate(TextArea, 2, 0),
  draw_Rect(Win,T1),
  draw_TextInRect(Win,TextArea, String, -1,[dtext_left]), !.

%BEGIN_WIN BezwarenWin
/**************************************************************************
        Creation and event handling for window: BezwarenWin
**************************************************************************/

constants
%BEGIN BezwarenWin, CreateParms, 16:22:04-13.11.2010, Code automatically updated!
  win_bezwarenwin_WinType = w_Child
  win_bezwarenwin_Flags = [wsf_Border,wsf_Close,wsf_ClipSiblings,wsf_ClipChildren,wsf_TopMost,wsf_TitleBar,wsf_Invisible]
  win_bezwarenwin_RCT = rct(100,80,473,305)
  win_bezwarenwin_Menu = no_menu
  win_bezwarenwin_Title = "Details voor het geselecteerde tijdstip"
  win_bezwarenwin_Help = idh_adresetiketten
%END BezwarenWin, CreateParms

predicates

  win_bezwarenwin_eh : EHANDLER

clauses

/*  win_bezwarenwin_Create(_Parent, _TijdStr, _Tekst, 0, Opmerkingen, _OpmAnderen, _Opm2):-
	not(fronttoken(Opmerkingen, _, _)), 
	bezwarenwinTekst(_Win, _, _, _, _, _, _),		% delete eventueel de oude
	win_SetState(_Win, [wsf_Invisible]), !.
  win_bezwarenwin_Create(_Parent, _TijdStr, _Tekst, 0, Opmerkingen, _, _):-
	not(fronttoken(Opmerkingen, _, _)), !.		% maak geen nieuwe*/
  win_bezwarenwin_Create(_Parent, _TijdStr, _Tekst, _Result, _Opmerkingen, _Verplaatsen, _OpmAnderen):-
	bezwarenwinTekst(Win, _, _, _, _, _, _, _, _),
	trap(win_Destroy(Win),_,fail),
	fail.
  win_bezwarenwin_Create(_Parent, TijdStr, Tekst, _, Opmerkingen, Verplaatsen, OpmAnderen):-
	boventekens(No, Cat),
	NulWin = cast(window, 0),
	retractall(bezwarenwinTekst(_, _, _, _, _, _, _, _, _)),
	assert(bezwarenwinTekst(NulWin, No, Cat, TijdStr, Tekst,Opmerkingen, Verplaatsen, OpmAnderen, b_False)),
	wd(No, Cat, _, _, _, _, Nota),
	retractall(editcontroltext(idc_planning, _, _, _,_,_,_)),
        asserta(editcontroltext(idc_planning, Nota, b_True, ff_Helvetica,b_False,11,1)),
	win_Create(win_bezwarenwin_WinType,win_bezwarenwin_RCT,win_bezwarenwin_Title,
		   win_bezwarenwin_Menu,_Parent,win_bezwarenwin_Flags,win_bezwarenwin_eh,0), !.
  win_bezwarenwin_Create(_, _, _, _, _, _, _).

%BEGIN BezwarenWin, e_Create
  win_bezwarenwin_eh(_Win,e_Create(_),0):-!,
%BEGIN BezwarenWin, InitControls, 16:22:04-13.11.2010, Code automatically updated!
	win_CreateDynControl([customctl(wdef(wc_Custom,rct(4,27,342,160),"Custom",u_Pixels),"pdcedit",idc_planning,[wsf_Group,wsf_TabStop])],_Win),
%END BezwarenWin, InitControls
%BEGIN BezwarenWin, ToolbarCreate, 16:22:04-13.11.2010, Code automatically updated!
	tb_plandetails_Create(_Win),
%END BezwarenWin, ToolbarCreate
	win_SetIcon(_Win, idi_planning),
	NulWin = cast(window, 0),
	retract(bezwarenwinTekst(NulWin, No, Cat, TijdStr, Bezw, Opmerkingen, VerPlaatsen, OpmAnderen, _Changed)),
	assert(bezwarenwinTekst(_Win, No, Cat, TijdStr, Bezw, Opmerkingen, Verplaatsen, OpmAnderen, b_True)),
	Font = font_Create(ff_Helvetica, [], 9),
	win_SetFont(_Win, Font),
	win_SetState(_Win, [wsf_Visible]),
	!.
%END BezwarenWin, e_Create
%MARK BezwarenWin, new events

/*
%BEGIN BezwarenWin, e_GetFocus
  win_bezwarenwin_eh(_Win,e_GetFocus,0):-!,
	write("planning child loosefooc!"),
	!.
%END BezwarenWin, e_GetFocus
*/

%BEGIN BezwarenWin, e_LoseFocus
  win_bezwarenwin_eh(_Win,e_LoseFocus,0):-
	EWin = win_GetCtlHandle(_Win, idc_planning),
	b_True = edit_IsModified(EWin),
	TextN = edit_getText(EWin),
	trim_c(TextN),
	%fronttoken(TextN, _, _),
	boventekens(No, Cat), 
	retract(wd(No, Cat, Tg1, Tg2, U, S, _Nota)),
	%Nota <> TextN, 
	assert(wd(No, Cat, Tg1, Tg2, U, S, TextN)),
	!.
%END BezwarenWin, e_LoseFocus
/*
%BEGIN BezwarenWin, idts_overzichttop
  win_bezwarenwin_eh(_Win,e_Menu(idts_overzichttop,_ShiftCtlAlt),0):-!,
        boventekens(No, Cat),
	TaskWin = vpi_GetTaskWin(),
	write("\nsituatie!!!!"), 
	situatie(TaskWin, No, Cat, 0), 
	!.
%END BezwarenWin, idts_overzichttop
*/
%BEGIN BezwarenWin, idr_schemaplan
  win_bezwarenwin_eh(_Win,e_Menu(idr_schemaplan,_ShiftCtlAlt),0):-!,
        boventekens(No, Cat),
	select(Cat, No),
	!.
%END BezwarenWin, idr_schemaplan

%BEGIN BezwarenWin, idb_todo
  win_bezwarenwin_eh(_Win,e_Menu(idb_todo,_ShiftCtlAlt),0):-
	EWin = win_GetCtlHandle(_Win, idc_planning),
	TextN = edit_getText(EWin),
  	not(fronttoken(TextN, _, _)),
  	not(notaVeld),
  	assert(notaVeld),
  	win_Invalidate(_Win),
	win_SetFocus(EWin),
	!.
  win_bezwarenwin_eh(_Win,e_Menu(idb_todo,_ShiftCtlAlt),0):-
  	%notaVeld,
	EWin = win_GetCtlHandle(_Win, idc_planning),
	TextN = edit_getText(EWin),
  	retractall(notaVeld),
  	fronttoken(TextN, _, _),
	1 = dlg_MessageBox( "Aantekening bij wedstrijd", "aantekening wissen?", mesbox_iconquestion, mesbox_buttonsokcancel, mesbox_defaultfirst, mesbox_suspendapplication ),
	boventekens(No, Cat),
	wd(No, Cat, Tg1, Tg2, U, S, _Nota),
	logf('A', wd(No, Cat, Tg1, Tg2, U, S, ""), nee),
  	edit_PasteStr(EWin, ""),
  	win_Invalidate(_Win),
  	update_display(),
	!.
  win_bezwarenwin_eh(_Win,e_Menu(idb_todo,_ShiftCtlAlt),0):-
  	win_Invalidate(_Win),
	!.
%END BezwarenWin, idb_todo

%BEGIN BezwarenWin, id_Speler_gegevens
  win_bezwarenwin_eh(_Win,e_Menu(id_Speler_gegevens,_ShiftCtlAlt),0):-!,
	boventekens(No, Cat), 
	wd(No, Cat, Tg1, Tg2, _, _, _),
	de_spelers(No, Cat, Tg1, Tg2, Spelers),
	setSelCrit([],Spelers),
	TaskWin = vpi_GetTaskWin(),
	win_spelersel_Create(TaskWin, 16),
	spelerSelRefresh(),
	!.
%END BezwarenWin, id_Speler_gegevens

%BEGIN BezwarenWin, e_Size
  win_bezwarenwin_eh(_Win,e_Size(_Height,_Width),0):-
ifdef use_tbar
	toolbar_Resize(_Win),
enddef
	!.
%END BezwarenWin, e_Size

%BEGIN BezwarenWin, e_MouseDown
  win_bezwarenwin_eh(_Win,e_MouseDown(_PNT,_ShiftCtlAlt,_Button),0):-
	EWin = win_GetCtlHandle(_Win, idc_planning),
	TextN = edit_getText(EWin),
  	not(fronttoken(TextN, _, _)),
  	not(notaVeld),
  	assert(notaVeld),
  	win_Invalidate(_Win),
	win_SetFocus(EWin),
	!.
  win_bezwarenwin_eh(_Win,e_MouseDown(_PNT,_ShiftCtlAlt,_Button),0):-
	EWin = win_GetCtlHandle(_Win, idc_planning),
	TextN = edit_getText(EWin),
  	retractall(notaVeld),
  	fronttoken(TextN, _, _),
	1 = dlg_MessageBox( "Aantekening bij wedstrijd", "aantekening wissen?", mesbox_iconquestion, mesbox_buttonsokcancel, mesbox_defaultfirst, mesbox_suspendapplication ),
	boventekens(No, Cat),
	wd(No, Cat, Tg1, Tg2, U, S, _Nota),
	logf('A', wd(No, Cat, Tg1, Tg2, U, S, ""), nee),
  	edit_PasteStr(EWin, ""),
  	win_Invalidate(_Win),
	!.
%END BezwarenWin, e_MouseDown

%BEGIN BezwarenWin, e_Destroy
  win_bezwarenwin_eh(_Win,e_Destroy,0):-
	EWin = win_GetCtlHandle(_Win, idc_planning),
	b_True = edit_IsModified(EWin),
	TextN = edit_getText(EWin),
	trim_c(TextN),
	%fronttoken(TextN, _, _),
	bezwarenwinTekst(_Win, No, Cat, _, _, _, _, _, _),
	%boventekens(No, Cat), 
	wd(No, Cat, Tg1, Tg2, U, S, _Nota),
	%Nota <> TextN, 
	logf('A', wd(No, Cat, Tg1, Tg2, U, S, TextN), nee),
	update_display(),
	fail.
  win_bezwarenwin_eh(_Win,e_Destroy,0):-
	retractall(bezwarenwinTekst(_Win, _, _, _, _, _, _, _, _)),
        retractall(editcontroltext(idc_planning, _, _, _,_,_,_)),
        retractall(notaVeld),
	!.
%END BezwarenWin, e_Destroy

%BEGIN BezwarenWin, e_Update
  win_bezwarenwin_eh(_Win,e_Update(_UpdateRct),0):-
	bezwarenwinTekst(_Win, No, Cat, TijdStr, Bezw, Opmerkingen, Verplaatsen, OpmAnderen, _),
	%retract(bezwarenwinTekst(_Win, TijdStr, Bezw, Opmerkingen, Verplaatsen, OpmAnderen, b_True)),
	%assert(bezwarenwinTekst(_Win, TijdStr, Bezw, Opmerkingen, Verplaatsen, OpmAnderen, b_False)), %opnieuw uitrekenen
	afm_bezwaren(_Win, TijdStr, 0, Hor0, 0, Vert0),
	afm_nota(_Win, No, Cat, Hor0, Hor1, Vert0, Vert1),
	afm_klikhier(_Win, Hor1, Hor1a, Vert1, Vert1a),
	afm_bezwaren(_Win, Bezw, Hor1a, Hor2, Vert1a, Vert2),
	afm_bezwaren(_Win, Opmerkingen, Hor2, Hor3, Vert2, Vert3),
	afm_bezwaren(_Win, Verplaatsen, Hor3, Hor4, Vert3, Vert4),
	afm_bezwaren(_Win, OpmAnderen, Hor4, _Hor5, Vert4, Vert5),
	lijnAlsAfm(Bezw, Vert5, Vert6),
	lijnAlsAfm(Opmerkingen, Vert6, Vert7),
	lijnAlsAfm(Verplaatsen, Vert7, Vert8),
	lijnAlsAfm(Verplaatsen, Vert8, Vert9),
	lijnAlsAfm(OpmAnderen, Vert9, Vert10),
	toolbar_getrect(_Win, tb_bottom, MRect),
	MRect = rct(_, MT, _, MB),
	Vert81 = Vert10 + MB - MT,
	ParentWin = win_GetParent(_Win),
	Prct = win_GetClientRect( ParentWin ),
	%write("\ne_update: ", _UpdateRct),
	%write("\nparent  : ", Prct),
	Prct = rct(_, _, W, H),
	W1 = W - Hor3 - 16,
	W2 = W - 14,
	H1 = (H - Vert81) div 2,
	H2 = (H + Vert81) div 2,
	win_Move(_Win,rct(W1, H1, W2, H2)),
	WP = win_GetCtlHandle(_Win, idc_planning),  % nota
	win_move(WP, rct(1, Vert0, Hor1, Vert1)),
	fail.
  win_bezwarenwin_eh(_Win,e_Update(_UpdateRct),0):-!,
	bezwarenwinTekst(_Win, No, Cat,TijdStr, Bezw, Opmerkingen, Verplaatsen, OpmAnderen, _),
	afm_bezwaren(_Win, TijdStr, 0, Hor0, 0, Vert0),
	afm_nota(_Win, No, Cat, Hor0, Hor1, Vert0, Vert1),
	afm_klikhier(_Win, Hor1, Hor1a, Vert1, Vert1a),
	afm_bezwaren(_Win, Bezw, Hor1a, Hor2, Vert1a, Vert2),
	afm_bezwaren(_Win, Opmerkingen, Hor2, Hor3, Vert2, Vert3),
	afm_bezwaren(_Win, Verplaatsen, Hor3, Hor4, Vert3, Vert4),
	afm_bezwaren(_Win, OpmAnderen, Hor4, Hor5, Vert4, _Vert5),
	win_GetTextExtent(_Win, "Mq", -1, _Width, Height),
	Hord = Hor5 + 5,
	B = color_White,
	A = color_White, 
	Font=win_GetFont(_Win),
	NewFont = font_SetAttrs(Font,[fs_Bold],0),
	win_setFont(_Win, NewFont),
	draw_Bezwaren(_Win, TijdStr, 0, Height,Hord,b_True,_Hoogte0, color_blue,color_white),		% kleur
	draw_klikhier(_Win, Font, Vert1, Hord), 
	win_setFont(_Win, Font),
	HoogteNaNota = Vert1a + 3, 
	lijnAls(_Win, Bezw, HoogteNaNota, Hoogte00, Hord),
	draw_Bezwaren(_Win, Bezw, Hoogte00, Height,Hord,b_True,Hoogte1, A,color_black),		% kleur
	lijnAls(_Win, Opmerkingen, Hoogte1, Hoogte2, Hord),
	draw_Bezwaren(_Win, Opmerkingen, Hoogte2, Height,Hord,b_False,Hoogte3, A,color_black),	% geen kleur
	lijnAls(_Win, Verplaatsen, Hoogte3, Hoogte4, Hord),
	lijnAls(_Win, Verplaatsen, Hoogte4, Hoogte5, Hord),
	draw_Bezwaren(_Win, Verplaatsen, Hoogte5, Height,Hord,b_False,Hoogte6, B,color_black),	% geen kleur
	lijnAls(_Win, OpmAnderen, Hoogte6, Hoogte7, Hord),
	draw_Bezwaren(_Win, OpmAnderen, Hoogte7, Height,Hord,b_False,_HoogteX, B,color_black),	% geen kleur
	win_SetState(_Win, [wsf_Visible]),
	win_BringToTop(_Win),
	!.
%END BezwarenWin, e_Update


%BEGIN BezwarenWin, e_Menu, Parent window 
  win_bezwarenwin_eh(Win,e_Menu(ID,CAS),0):-!,
	PARENT = win_GetParent(Win),
	win_SendEvent(PARENT,e_Menu(ID,CAS)),
	!.
%END BezwarenWin, e_Menu, Parent window

%END_WIN BezwarenWin

%BEGIN_WIN TijdstipExtra
/**************************************************************************
        Creation and event handling for window: TijdstipExtra
**************************************************************************/

constants
%BEGIN TijdstipExtra, CreateParms, 11:52:15-12.12.2002, Code automatically updated!
  win_tijdstipextra_WinType = w_Child
  win_tijdstipextra_Flags = [wsf_Border,wsf_TitleBar,wsf_Close,wsf_ClipSiblings,wsf_ClipChildren]
  win_tijdstipextra_RCT = rct(100,80,204,360)
  win_tijdstipextra_Menu = no_menu
  win_tijdstipextra_Title = "Extra Tijdstip"
  win_tijdstipextra_Help = idh_adresetiketten
%END TijdstipExtra, CreateParms

predicates

  win_tijdstipextra_eh : EHANDLER
determ  checkExtraTijdstip(integer Tijdstip, string Naam, string UurMinS)
maakextratijdstip(window)

clauses

maakextratijdstip(Win) :-  ParentWin = win_GetParent(Win),
  LboxWin = win_GetCtlHandle(ParentWin, idci_lbagenda),
  Index = lbox_GetSelIndex(LboxWin),
  Item = lbox_GetItem(LboxWin, Index),
  fronttoken(Item, _, R1),
  fronttoken(R1, _, R2),
  fronttoken(R2, TijdS, _),
  str_int(TijdS, Tijd),
  bitright(Tijd, 7, Dag),
  dag(Dag, Naam, _),
  bitleft(Dag, 7, BeginT),
  UurWin = win_GetCtlHandle(Win, idctt_uur_1),
  MinWin = win_GetCtlHandle(Win, idctt_kwart),
  lbox_GetSel(UurWin,_ItemListU,UurIndexList),
  UurIndexList = [UurIndex|_],
  lbox_GetSel(MinWin,_ItemListM,MinIndexList),
  MinIndexList = [MinIndex|_],
  bitleft(UurIndex,2,Uur),
  Tijdstip = BeginT + Uur + MinIndex,
  tijd_str(Tijdstip, UurMinS,_),
  checkExtraTijdstip(Tijdstip, Naam, UurMinS),
  logf('Z', bn(Tijdstip, 0),nee),
  update_uitslagen("maakextratijdstip"),
  win_Destroy(Win), !.

checkExtraTijdstip(Tijdstip, Naam, UurMinS) :-
  not(bn(Tijdstip, _)),
  format(Msg, "Toevoegen:\n%s %s?", Naam, UurMinS),
  1 = dlg_MessageBox( "Tijdstip", Msg, mesbox_iconquestion, mesbox_buttonsokcancel, mesbox_defaultfirst, mesbox_suspendapplication ),
  !.
checkExtraTijdstip(Tijdstip, Naam, UurMinS) :-
  bn(Tijdstip, _),
  format(Msg, "Tijdstip bestaat al:\n%s %s!", Naam, UurMinS),
  dlg_MessageBox( "Tijdstip toevoegen", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.

win_tijdstipextra_update :-
  extratijdstip(Win),
  win_Invalidate(Win), !.
win_tijdstipextra_update.

  win_tijdstipextra_Create(_Parent):-
	extratijdstip(Win),
	win_BringToTop(Win), !.
  win_tijdstipextra_Create(_Parent):-
	NulWin = cast(window, 0),
	assert(extratijdstip(NulWin)),
	win_Create(win_tijdstipextra_WinType,win_tijdstipextra_RCT,win_tijdstipextra_Title,
		   win_tijdstipextra_Menu,_Parent,win_tijdstipextra_Flags,win_tijdstipextra_eh,0).

%BEGIN TijdstipExtra, e_Create
  win_tijdstipextra_eh(_Win,e_Create(_),0):-!,
	Font = font_Create(ff_Helvetica, [fs_bold], 10),
	Font1 = font_Create(ff_Helvetica, [], 9),
	win_SetFont(_Win, Font),
%BEGIN TijdstipExtra, InitControls, 11:52:15-12.12.2002, Code automatically updated!
	win_CreateControl(wc_PushButton,rct(62,215,98,244),"&Bij",_Win,[wsf_Default,wsf_Group,wsf_TabStop],idc_ok),
	win_CreateControl(wc_LBox,rct(15,24,54,277),"",_Win,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight],idctt_uur_1),
	win_CreateControl(wc_LBox,rct(56,102,83,174),"",_Win,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight],idctt_kwart),
	win_CreateControl(wc_Text,rct(11,5,92,25),"Dag",_Win,[wsf_AlignCenter,wsf_Invisible],idct_dag),
%END TijdstipExtra, InitControls
%BEGIN TijdstipExtra, ToolbarCreate, 11:52:15-12.12.2002, Code automatically updated!
%END TijdstipExtra, ToolbarCreate
	win_BringToTop(_Win),
	retract(extratijdstip(_)),
	assert(extratijdstip(_Win)),
	UurWin = win_GetCtlHandle(_Win, idctt_uur_1),
	win_SetFont(UurWin, Font1),
	lbox_Add(UurWin, ["0","1","2","3","4","5","6","7","8","9","10","11","12",
			"13","14","15","16","17","18","19","20","21","22","23"]),
	MinWin = win_GetCtlHandle(_Win, idctt_kwart),
	win_SetFont(MinWin, Font1),
	kwartierinterval(KI),
	lbox_Add(MinWin, KI),
	lbox_SetSel(UurWin, 15, b_True),
	lbox_SetSel(MinWin, 0, b_True),
	lbox_SetTopIndex(UurWin, 8),
	!.
%END TijdstipExtra, e_Create
%MARK TijdstipExtra, new events

%BEGIN TijdstipExtra, e_Destroy
  win_tijdstipextra_eh(_Win,e_Destroy,0):-
	retractall(extratijdstip(_)),
	!.
%END TijdstipExtra, e_Destroy

%BEGIN TijdstipExtra, idc_ok _CtlInfo
  win_tijdstipextra_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	maakextratijdstip(_Win),
	!.
%END TijdstipExtra, idc_ok _CtlInfo

%BEGIN TijdstipExtra, e_Update
  win_tijdstipextra_eh(_Win,e_Update(_UpdateRct),0):-!,
	ParentWin = win_GetParent(_Win),
	LboxWin = win_GetCtlHandle(ParentWin, idci_lbagenda),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	fronttoken(Item, _, R1),
	fronttoken(R1, _, R2),
	fronttoken(R2, TijdS, _),
	str_int(TijdS, Tijd),
	bitright(Tijd, 7, Dag),
	dag(Dag, Naam, _),
	DagWin = win_GetCtlHandle(_Win, idct_dag),
	%win_SetText(DagWin, Naam),
	RCT = win_GetOuterRect(DagWin),
	draw_TextInRect(_Win,RCT, Naam, -1,[dtext_center]),
	!.
%END TijdstipExtra, e_Update

%BEGIN TijdstipExtra, e_Size
  win_tijdstipextra_eh(_Win,e_Size(_Width,_Height),0):-!,
ifdef use_tbar
	toolbar_Resize(_Win),
enddef
	!.
%END TijdstipExtra, e_Size

%BEGIN TijdstipExtra, e_Menu, Parent window 
  win_tijdstipextra_eh(Win,e_Menu(ID,CAS),0):-!,
	PARENT = win_GetParent(Win),
	win_SendEvent(PARENT,e_Menu(ID,CAS)),
	!.
%END TijdstipExtra, e_Menu, Parent window

%END_WIN TijdstipExtra



predicates 
database - btool
determ  banentoolWin(WINDOW)
%  banentoolInit(integer, integer, short, toolbar_list, toolbar_list) 
determ  baanedit(integer)	% geselecteerd om te editen


clauses

maxbaanicoonbreedte(80, 10).

predicates
  vulBanentool(Window, integer LopendeTop, integer Current, integer Max, ilist)
procedure updatemaxbanen(integer, integer)
procedure clearbanentool()

clauses

banenToolMove(R) :-
  banenToolWin(BWin),
  RCT = win_GetOuterRect(BWin),
  WSFlags = win_GetState(BWin),
  ClientRCT = rect_GetClient(WSFlags,b_False, RCT),
  ClientRCT = rct(_, _, RC, _),
  Dh = R - RC,
  RCT1 = rect_Offset(ClientRCT, Dh, 0),
  win_Move(BWin,RCT1), !.
banenToolMove(_).

updateMaxbanen(Huidig, Huidig) :- !.
updateMaxbanen(_Huidig, Nieuw) :-
  logf('A', banentool(ja, Nieuw), nee), !.

maxbaanBreedte(Parent, _, _) :-
  %win_GetTextExtent(Parent, "Res", -1, W, _),
  assert(maxbaanicoonbreedte(23, 15)),
  baanNaam(_, _, _, Naam, _),
  win_GetTextExtent(Parent, Naam, -1, Width, Height),
  maxbaanicoonbreedte(MaxLen, _),
  Width > MaxLen,
  assert(maxbaanicoonbreedte(Width, Height)),
  fail.
maxbaanBreedte(_Parent, MaxLen, Hoog) :-
  maxbaanicoonbreedte(M1, H1),
  Hoog = H1 + 1,
  MaxLen = M1 + 8.

vulBanenTool(Win, T, Max, Max, [T] ) :- 
  maxbaanbreedte(Win, Width, H),
  L = 0,
  R = L + Width,
  B = T + H,
  RectUit = rct(L, T, R, B), 
  Kleur = vpi_GetAttrVal(attr_color_btnface),
  win_SetPen(Win,pen(1, ps_solid, Kleur)),
  win_SetBrush(Win,brush(pat_solid,Kleur)),
  draw_Rect(Win,RectUit),
  PopUp = pict_GetFromRes(idb_menu), 
  pict_Draw(Win, PopUp, pnt(0, T), rop_SrcCopy), !.
vulBanenTool(Win, T, N, Max, [B | Rest]) :-
  N1 = N + 1, 
  not(baanedit(N1)),
  baanNaamP(N1, _, _, Naam),
  selectIcon(N1, OpKleur, TekstKleur), !,
  drawMyIcon(Win, Naam, 0, T, OpKleur, TekstKleur, color_black, rct(_,_,_,B)),
  vulBanenTool(Win, B, N1, Max, Rest).
vulBanenTool(Win, T, N, Max, [B | Rest]) :-
  N1 = N + 1,
  maxbaanbreedte(Win, _Width, H),
  B = T + H, 
  vulBanenTool(Win, B, N1, Max, Rest).

clearBanentool() :-
  baantoew(No, Cat, X),
  logf('R',baantoew(No, Cat, X),ja),
  fail.
clearBanentool() :-
  spelerAanwezig(Nr, Tijd, No, Cat),
  logf('R',spelerAanwezig(Nr, Tijd, No, Cat), ja),
  fail.
clearBanentool() :-
  baannaam(Nr, Besch, ParkToekomst, Naam, Naamkort),
  logf('R',baannaam(Nr, Besch, ParkToekomst, Naam, Naamkort), ja),
  fail.
clearBanentool() :-
  assert(maxbanenM(1)),
  logclose().  
  

%BEGIN_WIN Banentool
/**************************************************************************
        Creation and event handling for window: Banentool
**************************************************************************/

constants
%BEGIN Banentool, CreateParms, 12:49:11-1.8.2004, Code automatically updated!
  win_banentool_WinType = w_Child
  win_banentool_Flags = [wsf_ClipSiblings,wsf_ClipChildren]
  win_banentool_RCT = rct(100,80,228,469)
  win_banentool_Menu = no_menu
  win_banentool_Title = "Banentool"
  win_banentool_Help = idh_adresetiketten
%END Banentool, CreateParms

predicates

  win_banentool_eh : EHANDLER

clauses

banentool_ververs() :-
  banentool(ja, _),
  banentoolWin(BWin), !,
  win_Invalidate(BWin).
banentool_ververs().

  win_banentool_Create(_Parent):-
	not(banentoolWin(_)),
	banentool(ja, _), !,
	win_Create(win_banentool_WinType,win_banentool_RCT,win_banentool_Title,
		   win_banentool_Menu,_Parent,win_banentool_Flags,win_banentool_eh,0).
  win_banentool_Create(_Parent) :-
	banentoolWin(Win),
	banentool(nee, _), !,
	win_Destroy(Win).
  win_banentool_Create(_Parent).
	   

%BEGIN Banentool, e_Create
  win_banentool_eh(_Win,e_Create(_),0):-!,
	assert(banentoolWin(_Win)),
        Font = font_Create(ff_Helvetica, [], 9),
	win_SetFont(_Win, Font),
%BEGIN Banentool, InitControls, 12:49:11-1.8.2004, Code automatically updated!
%END Banentool, InitControls
%BEGIN Banentool, ToolbarCreate, 12:49:11-1.8.2004, Code automatically updated!
%END Banentool, ToolbarCreate
	!.
%END Banentool, e_Create
%MARK Banentool, new events

%BEGIN Banentool, e_MouseDown
  win_banentool_eh(_Win,e_MouseDown(PNT,_ShiftCtlAlt,_Button),0):-
	PNT = pnt(_, H),
	maxbaanbreedte(_Win, _, Hitem),
	BaanNr = H div (Hitem - 1) + 1,
	maxBanenM(Max),
	BaanNr <= Max,
	assert(baanEdit(BaanNr)),
	win_Invalidate(_Win),
	dlg_baaninstellingen_Create(_Win, BaanNr),
	retractall(baanEdit(_)),
	Parent = win_GetParent(_Win),
	win_Invalidate(Parent),
	!.
  win_banentool_eh(_Win,e_MouseDown(PNT,_ShiftCtlAlt,_Button),0):-
	PNT = pnt(_, H),
	maxbaanbreedte(_Win, _, Hitem),
	BaanNr = H div (Hitem - 1) + 1,
	maxBanenM(Max),
	BaanNr > Max,
	Menu = dyn_menu([
	  txt(id_Uitbreiding, "&Uitbreiden/inkrimpen aantal banen", 0, b_True,0, []),
	  txt(id_BanentoolReset,"&Reset...",0,b_True,0,[])
	  ]),
	menu_PopUp(_Win,Menu, pnt(0, H), align_Right),
	!.
%END Banentool, e_MouseDown

  win_banentool_eh(_Win,e_Menu(id_BanentoolReset, _CAS),0):-
	1 = dlg_MessageBox( "Banentool", "Een nieuwe start maken met het banentool?\nALLE baantoewijzing en ALLE speleraanwezigmeldingen\nworden dan gewist.",
	     mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
	clearBanentool(),
	Parent = win_GetParent(_Win),
	win_Invalidate(Parent),
	!.
  win_banentool_eh(_Win,e_Menu(id_Uitbreiding, _CAS),0):-
	findall(Aantal, bn(_, Aantal), AL),
	sortint_c(AL),
	reverse(AL, [], ALR),
	ALR = [AMax|_],
	banentool(_,Maxbanen),
	str_int(IMax, Maxbanen),
	format(Prompt, "Min %u banen:", AMax),
	MaxBS=dlg_GetStr("uitbreiding aantal banen",Prompt,IMax),
	str_int(MaxBS, MBNew),
	MBNew >= AMax, !,
	updatemaxbanen(Maxbanen,MBNew),
	assert(maxBanenM(MBNew)),
	banentool_ververs(),
	Parent = win_GetParent(_Win),
	win_Invalidate(Parent), !.

%BEGIN Banentool, e_Destroy
  win_banentool_eh(_Win,e_Destroy,0):-!,
	retractall(banenToolWin(_)),
	retractall(baanedit(_)),
	!.
%END Banentool, e_Destroy

%BEGIN Banentool, e_Update
  win_banentool_eh(_Win,e_Update(_UpdateRct),0):-
	banentool(ja,MaxbanenM),
	findall(Aantal, bn(_, Aantal), AL),
	sortint_c(AL),
	reverse(AL, [], ALR),
	ALR = [AMax|_],
	grootste(AMax, MaxBanenM, Grootste),
	updatemaxbanen(MaxbanenM, Grootste),
	assert(maxBanenM(Grootste)),
	maxBaanBreedte(_Win, Max, Hoog),
	Hoogte = Grootste * Hoog + 32,  		% 22.2.2013  
	T = 60,
	B = T + Hoogte,
	ParentWin = win_GetParent(_Win),
	PRCT = win_GetClientRect(ParentWin),
	PRCT = rct(_, _, PR0, _),
	PR = Pr0 - 6,
	L = PR - Max,
	win_Move(_Win,rct(L,T,PR,B)),
	vulBanenTool(_Win, 1, 0, Grootste, _Lijst),
	win_BringToTop(_Win),
	!.
  win_banentool_eh(_Win,e_Update(_UpdateRct),0):-
	banentool(nee,_MaxbanenM),
	win_Destroy(_Win).
%END Banentool, e_Update

%BEGIN Banentool, e_Size
  win_banentool_eh(_Win,e_Size(_Width,_Height),0):-!,
ifdef use_tbar
	toolbar_Resize(_Win),
enddef
	!.
%END Banentool, e_Size

%BEGIN Banentool, e_Menu, Parent window 
  win_banentool_eh(Win,e_Menu(ID,CAS),0):-!,
	PARENT = win_GetParent(Win),
	win_SendEvent(PARENT,e_Menu(ID,CAS)),
	!.
%END Banentool, e_Menu, Parent window

%END_WIN Banentool

%BEGIN_DLG Baaninstellingen
/**************************************************************************
	Creation and event handling for dialog: Baaninstellingen
**************************************************************************/

constants

%BEGIN Baaninstellingen, CreateParms, 09:32:10-1.8.2004, Code automatically updated!
  dlg_baaninstellingen_ResID = idd_baaninstellingen
  dlg_baaninstellingen_DlgType = wd_Modal
  dlg_baaninstellingen_Help = idh_uitslagen_invoeren_f8
%END Baaninstellingen, CreateParms

predicates

  dlg_baaninstellingen_eh : EHANDLER
  dlg_baaninstellingen_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_baaninstellingen_update(DIALOG_VAL_LIST)

clauses

  dlg_baaninstellingen_Create(Parent, Nr):-
	baanNaamP(Nr, IDC_BESCHIKBAAR_CHECKED, _Park, IDC_BAANNAAM_VALUE),
	IDC_NR_VALUE = i(Nr),
%MARK Baaninstellingen, new variables

	dialog_CreateModal(Parent,dlg_baaninstellingen_ResID,"",
  		[
%BEGIN Baaninstellingen, ControlList, 09:32:10-1.8.2004, Code automatically updated!
		df(idc_baannaam,editstr(IDC_BAANNAAM_VALUE,[mandatory]),nopr),
		df(idc_beschikbaar,checkbox(IDC_BESCHIKBAAR_CHECKED),nopr),
		df(idc_nr,editint(IDC_NR_VALUE,[]),nopr)
%END Baaninstellingen, ControlList
		],
		dlg_baaninstellingen_eh,0,VALLIST,ANSWER),
	dlg_baaninstellingen_handle_answer(ANSWER,VALLIST).

  dlg_baaninstellingen_handle_answer(idc_ok,VALLIST):-!,
	dlg_baaninstellingen_update(VALLIST).
  dlg_baaninstellingen_handle_answer(idc_cancel,_):-!.  % Handle Esc and Cancel here
  dlg_baaninstellingen_handle_answer(_,_):-
	errorexit().

  dlg_baaninstellingen_update(_VALLIST):-
%BEGIN Baaninstellingen, Update controls, 09:32:10-1.8.2004, Code automatically updated!
	_IDC_BAANNAAM_VALUE = dialog_VLGetstr(idc_baannaam,_VALLIST),
	_IDC_BESCHIKBAAR_CHECKED = dialog_VLGetCheck(idc_beschikbaar,_VALLIST),
	_IDC_NR_VALUE = dialog_VLGetint(idc_nr,_VALLIST),
%END Baaninstellingen, Update controls
	_IDC_NR_VALUE = i(Nr),
	not(baanNaam(Nr, _IDC_BESCHIKBAAR_CHECKED, _, _IDC_BAANNAAM_VALUE, _)),
	baanNaamP(Nr, _, Park, _), 
	logf('Z',baannaam(Nr, _IDC_BESCHIKBAAR_CHECKED, Park,_IDC_BAANNAAM_VALUE, ""), nee), !,
	true.
  dlg_baaninstellingen_update(_VALLIST).

%MARK Baaninstellingen, new events

%BEGIN Baaninstellingen, e_Create
  dlg_baaninstellingen_eh(_Win,e_Create(_CreationData),0):-!,
	% dialoog zelf
	OuterRct = win_GetOuterRect(_Win),
	Wsflags = win_GetState(_Win),
	OldRCT = rect_GetClient(Wsflags,b_False,OuterRct),
	OldRct = rct(_,TopD, RB,_),
	% toolbar
	ParentWin = win_GetParent(_Win),
	POuterRct = win_GetOuterRect(ParentWin),
	WsflagsP = win_GetState(ParentWin),
	POldRCT = rect_GetClient(WsflagsP,b_False,POuterRct),
	POldRct = rct(LB, TopB, _, _),
	baanedit(Nr),
	planselect(UWin, 2, _), 
	UOuterRct = win_GetOuterRect(UWin),
	WsflagsU = win_GetState(UWin),
	UOldRCT = rect_GetClient(WsflagsU,b_False,UOuterRct),
	UOldRct = rct(LU, TopU, _, _),
	% taskwin
	TWin = vpi_GetTaskWin(),
	toolbar_GetRect(TWin,tb_left,TBRCT),
	TBRCT = rct(_,_,TR,_),
	TOuterRct = win_GetOuterRect(TWin),
	TWsflags = win_GetState(TWin),
	TOldRCT = rect_GetClient(TWsflags,b_False,TOuterRct),
	TOldRct = rct(TL,TopR, _,_),
	Dh = (LB + LU + TL + TR) - RB,
	Dv =  TopR + TopB + TopU + Nr * 15 - TopD,
	RCTU = rect_Offset(OldRCT, Dh, Dv),
	win_Move(_Win,RCTU),
	!.
%END Baaninstellingen, e_Create

  dlg_baaninstellingen_eh(_,_,_):-!,fail.

%END_DLG Baaninstellingen

%BEGIN_DLG Plaatsvoorstel
/**************************************************************************
	Creation and event handling for dialog: Plaatsvoorstel
**************************************************************************/

constants

%BEGIN Plaatsvoorstel, CreateParms, 10:32:24-24.12.2013, Code automatically updated!
  dlg_plaatsvoorstel_ResID = idd_plaatsvoorstel
  dlg_plaatsvoorstel_DlgType = wd_Modeless
  dlg_plaatsvoorstel_Help = idh_plaatsing
%END Plaatsvoorstel, CreateParms

predicates

  dlg_plaatsvoorstel_eh : EHANDLER
  dlg_plaatsvoorstel_update(window, DIALOG_VAL_LIST)
procedure  zetPlaatsing(window, integer Aantal, integer Counter, slist)
procedure try_volgorde(integer, integer)
procedure alsOvergeslagen(integer, integer)

clauses

alsOvergeslagen(AantalGeplaatsten, N) :-
  N <= Aantalgeplaatsten,
  _Answer = dlg_MessageBox( "Plaatsen", "Er wordt een uitgesloten speler overgeslagen bij het plaatsen!", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  !.
alsOvergeslagen(_, _).

zetPlaatsing(Win, Aantalgeplaatsten, N, [Elem|SortList]) :-
  N <= Aantalgeplaatsten,
  parseTabs(Elem, SList),
  SList = [_, _CatNrS, NrS | _],
  str_int(NrS, Num),
  opg_t(Win,Num,_,_,_,_,0,_,_,_,_,_),
  retract(opg_t(Win,Num,Cat,Tgnst,_Plaats,Ronde,Uitsl,Tijd,LL,Keur,TID,MT)),
  assert(opg_t(Win,Num,Cat,Tgnst,N,Ronde,Uitsl,Tijd,LL,Keur,TID,MT)),
  N1 = N + 1, !,
  zetPlaatsing(Win, Aantalgeplaatsten, N1, SortList).
zetPlaatsing(Win, Aantalgeplaatsten, N, [Elem|SortList]) :-	% 6.3.2011 de anderen worden gereset
  alsOvergeslagen(Aantalgeplaatsten, N),
  parseTabs(Elem, SList),
  SList = [_, _CatNrS, NrS | _],
  str_int(NrS, Num), 
  retract(opg_t(Win,Num,Cat,Tgnst,_Plaats,Ronde,Uitsl,Tijd,LL,Keur,TID,MT)),
  assert(opg_t(Win,Num,Cat,Tgnst,0,Ronde,Uitsl,Tijd,LL,Keur,TID,MT)), !,
  zetPlaatsing(Win, Aantalgeplaatsten, N, SortList).
zetPlaatsing(_Win, _Aantalgeplaatsten, _N, _).

try_volgorde(idcp_rangorde, idcp_rangorde) :- !.
try_volgorde(_, idcp_rating).

  dlg_plaatsvoorstel_Create(Parent, VLG):-
  	wincat(Parent, Cat), !,
  	try_volgorde(VLG, VOLGORDE),
	IDCT_AANBEVOLEN_TITLE = "Zie hiervoor/hierboven voor het aanbevolen aantal.",
	uitgeslotenT(Parent, AantalU),
        findall(Num, opg_t(Parent,Num,Cat,_Tgnst,_Plaats,_Ronde,_Uitsl,_TijdKK,_LL,_Keur,_,_), NumL),
        count(NumL, 0, AantalT),
        A = AantalT - AantalU,
	findall(ASl, juistAantalGeplaatsten(Cat, A, ASl, _), ASList),
	reverse(ASList, [], ASRev),
	ASRev = [AX|_],
	str_int(AX, AI),
	IDC_AANTAL_VALUE = i(AI),
%MARK Plaatsvoorstel, new variables

	dialog_CreateModeless(Parent,dlg_plaatsvoorstel_ResID,"",
  		[
%BEGIN Plaatsvoorstel, ControlList, 10:32:24-24.12.2013, Code automatically updated!
		df(idc_aantal,editint(IDC_AANTAL_VALUE,[]),nopr),
		df(idct_aanbevolen,statictext(IDCT_AANBEVOLEN_TITLE),nopr),
		df(VOLGORDE,radiobuttongroup([idcp_rating,idcp_rangorde]),nopr)
%END Plaatsvoorstel, ControlList
		],
		dlg_plaatsvoorstel_eh,0).

  dlg_plaatsvoorstel_update(Win, _VALLIST):-
%BEGIN Plaatsvoorstel, Update controls, 10:32:24-24.12.2013, Code automatically updated!
	_IDC_AANTAL_VALUE = dialog_VLGetint(idc_aantal,_VALLIST),
	_VOLGORDE = dialog_VLGetRadiobutton(idcp_rating,_VALLIST),
%END Plaatsvoorstel, Update controls
	wincat(Win, Cat),
	findall(Sorteerveld, opgListBase(Win, Cat, SorteerVeld, _VOLGORDE), SortList),
	sortstring_c(SortList, 1),
	%list_to_string(SortList, "\n", String),
	_IDC_AANTAL_VALUE = i(Aantalgeplaatsten),
	zetPlaatsing(Win, Aantalgeplaatsten, 1, SortList),
	%write("\n",String),
	!.
	%true.

%BEGIN Plaatsvoorstel, idc_ok _CtlInfo
  dlg_plaatsvoorstel_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-!,
	VALLIST = dialog_GetValues(_Win),
	ParentWin = win_GetParent(_Win),
	dlg_plaatsvoorstel_update(ParentWin, VALLIST),
        win_PostEvent(ParentWin,e_User(prefresh,0)),
	!,fail.
%END Plaatsvoorstel, idc_ok _CtlInfo
%MARK Plaatsvoorstel, new events

%BEGIN Plaatsvoorstel, idc_cancel _CtlInfo
  dlg_plaatsvoorstel_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	ParentWin = win_GetParent(_Win),
	trap(win_SetFocus(ParentWin), _, true),
	!, fail.
%END Plaatsvoorstel, idc_cancel _CtlInfo

%BEGIN Plaatsvoorstel, idc_help _CtlInfo
  dlg_plaatsvoorstel_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Plaatsing"),
	!.
%END Plaatsvoorstel, idc_help _CtlInfo

  dlg_plaatsvoorstel_eh(_,_,_):-!,fail.

%END_DLG Plaatsvoorstel

%BEGIN_TLB Plandetails, 20:54:43-26.7.2011, Code automatically updated!
/**************************************************************************
	Creation of toolbar: Plandetails
**************************************************************************/

clauses

  tb_plandetails_Create(_Parent):-
ifdef use_tbar
	toolbar_create(tb_bottom,0xC0C0C0,_Parent,
		[tb_ctrl(idr_schemaplan,pushb,idb_zoekopw,idb_zoekopp,idb_zoekopd,"Zie Schema;Te plannen wedstrijd in schema",1,1),
		 tb_ctrl(idts_overzichttop,pushb,idb_wedstrijd,idb_wedstrijdd,idb_beeldd,"Wedstrijdoverzicht;Wedstrijdoverzicht te plannen partij",1,1),
		 tb_ctrl(id_Speler_gegevens,pushb,idb_spelersw,idb_spelers,idb_spelerd,"Lijstje van spelers;Lijstje van spelers",1,1),
		 tb_ctrl(idb_todo,pushb,idb_todow,idb_todop,idb_tododd,"Aantekening;Maak een aantekening bij deze wedstrijd",1,1)]),
enddef
	true.
%END_TLB Plandetails












