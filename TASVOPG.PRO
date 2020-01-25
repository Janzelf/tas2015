/*****************************************************************************

		Copyright (c) 1989 - 1998 De Lint Associates

 Project:  TASVPI
 FileName: TASVOPG.PRO
 Purpose: No description
 Written by: JdL
 Comments:
******************************************************************************/

include "tasvp1.inc"
include "tasvp1.con"
include "hlptopic.con"

%__________________________ tekst van wedstrijd _____________________________  
database - agenda
  sp_tijd(integer, tijd, string Park)
  single viewtype(integer)
  single dags(string)
  single tijds(string)
  single onderdeels(string)
  single algeweestB(boolean)


predicates
  rep_wed(wl,integer,integer,string,integer,string,integer,Boolean Weblinks) 
  gewaarschuwd(tgnst,janee,janee,string)
  rep_volgende1(integer,wedscat,integer,string)
  rep_volgende2(integer,wedscat,integer,slist,string)
  rep_volgende3(string,slist)
  %rep_volgende4(integer,wedscat,integer,slist)
  check_zelfde_dag(integerlist,integer TijdNiet, string, string)
  extra_space(integer)
  procedure add_baanW(janee, integer, wedscat)
  procedure bijdrage(janee, tgnst, string, string)
  procedure bijdrage(integer, string)
  procedure dagstring(boolean LineThrough, string In, string Uit)
  procedure dagstring(string In, string Uit)
  procedure tijdstring(string, string)
  procedure uplink(wedscat, wedscat)
  procedure check_parkwissel(integerlist Spelers, string Indicator)
  nondeterm sp_park(integer, string Park)
  procedure onbekend2vervolg(tgnst, string)
  procedure metUitslag(boolean Met, slist Etik, slist Omcirkel, slist Sresult)
  determ makeOnderdeelLink(integer WdNo, wedscat Cat, string Anker, string FullLink)
procedure uitslagenableAgenda(integer Tijdstip, integer WdNo, wedscat Cat, string FullLink, string TS1, string TS2)
procedure ookgelijkspel(string, string, integer, wedscat, string)
nondeterm toernooidagenVerder(integer)
determ drietoernooidagenverder(integer)
determ drietoernooidagenverderL(ilist, integer)


clauses

dags("").
tijds("").
onderdeels("").
viewtype(0).
algeweestB(b_False).

dagstring(b_False, Dag, Uit) :-
  dagstring(Dag, Uit) , !.
dagstring(_, Dag, Uit) :-
  format(Dag1, "<span style=\"text-decoration:line-through\">%s</span>", Dag),
  dagstring(Dag1, Uit).  

dagstring(Dag, "&nbsp;") :-
  dags(Dag), !.
dagstring(Dag, Dag) :-	% was nog niet
  assert(dags(Dag)).

tijdstring(Tijd, "&nbsp;") :-
  tijds(Tijd), !.
tijdstring(Tijd, Tijd1) :-
  concat(Tijd, "&nbsp;&nbsp;", Tijd1),
  assert(tijds(Tijd)).


uplink(Cat, CatN) :-
  koplv(Cat, Cat1), !, 			% gekoppeld naar..
  uplink(Cat1, CatN).
uplink(Cat, Cat).

metUitslag(b_True, S, Omcirkel, Sresult) :-
  append(S, Omcirkel, Sresult), !.
metUitslag(_, S, _, S).

makeOnderdeelLink(WdNo, Cat, _Anker, FullLink) :-
  wc(Cat, _, _, _, _, Short, _, _, _, _, _, _, _, _, _),
  %wc(Cat,_L,_,Short,_,_,_),
  uplink(Cat, Cat1),		% zoek finale
  htmlnaam(Cat1, _ShortX, Link0), !,
  reversestr(Link0, "", Link1),
  frontchar(Link1, _, Link2),
  reversestr(Link2, "", Link),
  %format(FullLink, "<%s&amp;merk=T%ux%u#T%ux%u\" title=\"klik voor schema\">%s</a>", Link, WdNo, Cat, WdNo, Cat, Short).
  format(FullLink, "<%s&amp;merk=T%ux%u\" title=\"klik voor schema\">%s</a>", Link, WdNo, Cat, Short).

ookgelijkspel(Ticket, Jaar, WdNo, Cat, Uit) :-
  format(Uit, "<strong><SPAN class='winner' onclick=\"return uitslagvanagenda('%s',%s,%u,%u, 2)\">&laquo;&laquo;&lang;=&rang;&raquo;&raquo;</SPAN></strong>", Ticket, Jaar, WdNo, Cat),
  pos(Cat, _, _, poel), !.
ookgelijkspel(_, _, _, _, "&laquo;&laquo;&lang;0&rang;&raquo;&raquo;").

uitslagenableAgenda(Tijdstip, WdNo, Cat, FullLink, TS1, TS2) :-
  drietoernooidagenverder(Tijdstip),
  not(frontchar(TS1, '}', _)),
  not(frontchar(TS2, '}', _)),
  uitslagenable(_),
  naam(_ToernooiNaam, _Nr, _Datum, TicketI, _District, _),
  dag(Date, _, _),
  dt_dateoffset_to_str(Date, "%YL", Jaar),
  %zoekmaandag(_, _, _, Jaar),
  maketicket(TicketI, Ticket),
  ookgelijkspel(Ticket, Jaar, WdNo, Cat, Separator),
  writef("<TD class=agendan>%s<TD class='agendan' colspan=4><SPAN class='winner' onclick=\"return uitslagvanagenda('%s',%s,%u,%u, 0)\">%s</SPAN>", FullLink, Ticket, Jaar, WdNo, Cat, TS1),
  writef("&nbsp;&nbsp;&nbsp;%s&nbsp;&nbsp;&nbsp;<SPAN class='winner' onclick=\"return uitslagvanagenda('%s',%s,%u,%u, 1)\">%s</SPAN><div class='uitslag' id='uitsl%s%s%u%u'></div><TD>&nbsp;<TD>&nbsp;",
                                  Separator, Ticket, Jaar, WdNo, Cat, TS2, Ticket, Jaar, WdNo, Cat), !.
uitslagenableAgenda(Tijdstip, _WdNo, _Cat, FullLink, TS1, TS2) :-
  drietoernooidagenverder(Tijdstip),
  uitslagenable(_),
  writef("<TD class=agendan>%s<TD class=agendan colspan=4>%s&nbsp;&nbsp;&nbsp;&laquo;&laquo;&lang;0&rang;&raquo;&raquo;&nbsp;&nbsp;&nbsp;%s<TD>&nbsp;<TD>&nbsp;",
                                 FullLink, TS1, TS2), !.
uitslagenableAgenda(_Tijdstip, _WdNo, _Cat, FullLink, TS1, TS2) :-
  writef("<TD class=agendan>%s<TD class=agendan>%s<TD>&nbsp;<TD>-<TD class=agendan>%s<TD>&nbsp;<TD>&nbsp;",
                                 FullLink, TS1, TS2).
toernooidagenVerder(DagV) :-
  huidig(Dag, _),
  findall(Dx, dag(Dx,_,_), DxL),
  sorttijd_c(DxL),
  member(DagV, DxL),
  DagV > Dag,
  getbacktrack(Here),
  w2(_WdNo,_Cat,TijdN,_J1,_J2,_Geforceerd,_,_Bn), % en moet een planning op die dag zijn
  bitright(TijdN, 7, DagW2),
  Dagw2 = DagV,
  cutbacktrack(Here),
  dag(DagV, Naam, _),
  writedevice(X),
  writedevice(screen),
  write(' ', Naam),
  writedevice(X).
  
drietoernooidagenverder(_Tijdstip) :- !.
drietoernooidagenverder(Tijdstip) :-
  bitright(Tijdstip, 7, Dag1),
  findall(D, toernooidagenverder(D), DL),
  drietoernooidagenverderL(DL, Dmax),
  Dag1 < Dmax.

%drietoernooidagenverderL([_, _, D3 | _], D3) :- !.
%drietoernooidagenverderL([_, D2 | _], D2) :- !.
drietoernooidagenverderL([D1 | _], D1).

rep_wed([],N,N,_,_,_,_,_) :- !.
rep_wed([w(WdNo,Cat,_,_,_,_,_, _BaanPark)|_],_,_,_,DagP,_, Form,_) :-
  Form < 3,
  wc(Cat, _, _, _, _, Short, _, _, _, _, _, _, _, _, _),
  %wc(Cat,_,_,Short,_,_,_),
  wd(WdNo,Cat,T1,T2,Score,_,_), 
  de_spelers(WdNo,Cat,T1,T2,Spelers),
  w2(WdNo,Cat,TijdN,J1,J2,Geforceerd,_,Bn),
  baan_adm(IBaan),
  add_baan(IBaan, Bn, BaanT),
  check_zelfde_dag(Spelers,TijdN, Zon1,_),
  oproepen(W), 
  betaal_adm(B),
  gewaarschuwd(T1,W,J1,JS1),
  gewaarschuwd(T2,W,J2,JS2),
  repc(nee,'}',IBaan,W,B,0,0,1,WdNo,Cat,T1,alleplanning,TS1,_,_,_,_,_,_),% en    bepaal de tekst ervoor 
  repc(nee,'}',IBaan,W,B,1,0,1,WdNo,Cat,T2,alleplanning,TS2,_,_,_,_,_,_),% en    bepaal de tekst ervoor 
  format(FStr,"\n%s %s%s%c%33.33%1 %1%",BaanT, Short,Zon1,Geforceerd,TS1,JS1,JS2,TS2),
  write(FStr),
  rep_volgende1(WdNo,Cat,DagP,Score),
  extra_space(Form),
  fail.
rep_wed([w(WdNo,Cat,_TijdN,_,_, _,_,_)|_],_,_,DagS, DagP,TStr, 3, MetUitslagInvul) :-
  wc(Cat, _, _, _, _, Short, _, _, _, _, _, _, _, _, _),
  %wc(Cat,_,_,Short,_,_,_),
  wd(WdNo,Cat,T1,T2,Score,_,_), 
  w2(WdNo,Cat,_,J1,J2,Geforceerd,_,_), 
  oproepen(W), 
  betaal_adm(B), 
  gewaarschuwd(T1,W,J1,JS1),
  gewaarschuwd(T2,W,J2,JS2),
  baan_adm(IBaan),
  repa(ja,'}',IBaan,W,B,0,0,1,WdNo,Cat, T1,alleplanning,TS1,_,_,b_False, b_False,_,_,_,_, _, _, _),
  repa(ja,'}',IBaan,W,B,1,0,1,WdNo,Cat, T2,alleplanning,TS2,_,_,b_False, b_False,_,_,_,_, _, _, _),
  format(S1, "% % % %", DagS, TStr, Short, Geforceerd),
  concat(TS1, JS1, S2),
  concat(TS2, JS2, S3),
  rep_volgende2(WdNo,Cat,DagP,S4L,_),
  append([S1, "", S2, S3], S4L, S4T),
  rep_volgende3(Score,S5L),
  append(S4T, S5L, S5T),
  metUitslag(MetUitslagInvul, S5T, ["Omcirkel de winnaar(s)!", "", "uitslag: "], Sresult),
  etiketBuild(Sresult),
  fail.
rep_wed([w(WdNo,Cat,Tijdstip,_,_,_,_,_)|_],_,_,DagS, _DagP,TStr, 4, b_False):- % webpagina met links
  wd(WdNo,Cat,T1,T2,_Score,_,_), 
  baan_adm(IBaan),
  repa(ja,'}',IBaan,nee,nee,0,0,1,WdNo,Cat,T1,alleplanning,TS1,_,_,b_True,b_False,_,Anker,_,_,_,_,_),% onbeperkt
  repa(ja,'}',IBaan,nee,nee,1,0,1,WdNo,Cat,T2,alleplanning,TS2,_,_,b_True,b_False,_,_,_,_,_,_,_),% en bepaal de tekst ervoor 
  makeOnderdeelLink(WdNo, Cat, Anker, FullLink),
  dagstring(b_False, DagS, DagS1),
  tijdstring(TStr, TStr1),
  writef("\n<TR><TD class=agendan>%s<TD class=agendar>%s", DagS1, TStr1),
  add_baanW(IBaan, WdNo, Cat),
  uitslagenableAgenda(Tijdstip, WdNo, Cat, FullLink, TS1, TS2),	% 17.2.2011
  drietoernooidagenverder(Tijdstip),
  uitslagenable(_),
  not(T1 = onbekend),
  not(T2 = onbekend),
  not(uitslagenableE(WdNo, Cat, Tijdstip, T1, T2)),
  assert(uitslagenableE(WdNo, Cat, Tijdstip, T1, T2)),
  fail.
rep_wed([w(WdNo,Cat,_,_,_,_,_,_)|_],_,_,DagS, _DagP,TStr, 4, b_True) :-	% webpagina zonder links
  wc(Cat, _, _, _, _, Short, _, _, _, _, _, _, _, _, _),
  %wc(Cat,_L,_,Short,_,_,_),
  %uplink(Cat, Cat1),		% zoek finale
%  wc(Cat1,_,_,_,_,_,_),
  %htmlnaam(Cat1, ShortX),
  wd(WdNo,Cat,T1,T2,_Score,_,_), 
  baan_adm(IBaan),
  %add_baan(IBaan, Bn, BaanT),
  repa(ja,'}',IBaan,nee,nee,0,0,1,WdNo,Cat,T1,alleplanning,TS1,_,_,b_True,b_False,_,_,_,_,_,_,_),% onbeperkt
  repa(ja,'}',IBaan,nee,nee,1,0,1,WdNo,Cat,T2,alleplanning,TS2,_,_,b_True,b_False,_,_,_,_,_,_,_),% en bepaal de tekst ervoor 
  dagstring(b_False, DagS, DagS1),
  tijdstring(TStr, TStr1),
  %htmlnaam(Lang, ShortX),
  writef("\n<TR><TD class=agendan>%s</TD><TD class=agendar>%s</TD><TD class=agendan>%s</TD>", DagS1, TStr1, Short),
  add_baanW(IBaan, WdNo, Cat),
  writef("<TD class=agendan>%s</TD><TD class=agendan>%s</TD></TR>", TS1, TS2),
  fail.
rep_wed([w(WdNo,Cat,TijdN,_,_,_,_,Park)|_],_,_,DagS, DagP,TStr, 5, _) :-	% mailmerge
  wc(Cat, _, _, Lang, _, Short, _, _, _, _, _, _, _, _, _),
  %wc(Cat,Lang,_,Short,_,_,_),
  wd(WdNo,Cat,T1,T2,_Score,_,_), 
  de_spelers(WdNo,Cat,T1,T2,Spelers),
  check_zelfde_dag(Spelers,TijdN, _,Zelfdedag),
  check_parkwissel(Spelers, Parkwissel),
  %concat(ZelfdeDag, Parkwissel, ZP),
  %w2(WdNo,Cat,_,_,_,_Gef,_,Baan), 
  betaal_adm(IBetaal),
  bijdrage(IBetaal, T1, Bedr1a, Bedr1b),
  bijdrage(IBetaal, T2, Bedr2a, Bedr2b),
  baan_adm(IBaan),
  repc(ja,'}',IBaan,nee,nee,0,0,1,WdNo,Cat,T1,alleplanning,TS1,_,_,_,_,_,_),	% en    bepaal de tekst ervoor 
  repa(ja,'}',IBaan,nee,nee,0,0,1,WdNo,Cat,T1,alleplanning,TS1u,_,_,b_False,b_False,_,_,Naama1,Naama2,_,_,_),% onbeperkt
  repc(ja,'}',IBaan,nee,nee,1,0,1,WdNo,Cat,T2,alleplanning,TS2,_,_,_,_,_,_),	% en    bepaal de tekst ervoor 
  repa(ja,'}',IBaan,nee,nee,1,0,1,WdNo,Cat,T2,alleplanning,TS2u,_,_,b_False,b_False,_,_,Naamb1,Naamb2,_,_,_),% en bepaal de tekst ervoor 
  rep_volgende2(WdNo,Cat,DagP,_,Volgende),
  onbekend2vervolg(T1, Vervolgvan1),
  onbekend2vervolg(T2, Vervolgvan2),
  writef("\n\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c", DagS,csvconst, 
     TStr,csvconst, ""/*Baan*/,csvconst, Park, csvconst, Short,csvconst,Lang,csvconst, TS1,csvconst, 
     TS1u,csvconst,TS2,csvconst, TS2u,csvconst, Volgende, csvconst),
  writef("\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c", Bedr1a, csvconst, Bedr1b, csvconst, Bedr2a, csvconst, Bedr2b, csvconst, Zelfdedag,csvconst),
  writef("\"%s\"%c\"%s\"%c\"%s\"%c",Naama1, csvconst, Naama2, csvconst, Vervolgvan1, csvconst),
  writef("\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"",Naamb1, csvconst, Naamb2,csvconst,Vervolgvan2,csvconst,Parkwissel),
  fail.
rep_wed([_|Rest],InNo,OutNo,DagS,DagP,TStr, Form, WebLinks) :-
  N = InNo - 1,                 % trek een af van aantal banen 
  rep_wed(Rest,N,OutNo,DagS, DagP,TStr,Form,Weblinks).

onbekend2vervolg(onbekend, "{") :- !.
onbekend2vervolg(_, "").

add_baanW(ja, WdNo, Cat) :-
  w2(WdNo,Cat,_,_,_,_Geforceerd,_,Bn), 
  fronttoken(Bn, _, _), !,
  writef("<TD class=agendan>%s</TD>", Bn).
add_baanW(ja, _WdNo, _Cat) :- !,
  write("<TD>&nbsp").
add_baanW(_, _, _).

extra_space(2) :- nl, !.
extra_space(_).

gewaarschuwd(onbekend,_,_,"") :- !.
gewaarschuwd(_,ja,nee,"?") :- !.
gewaarschuwd(_,_,_,"").

check_zelfde_dag(Spelers, TijdN, "!", "d") :-
  member(Speler,Spelers),
  sp_tijd(Speler, Tijd, _Park),
  Tijd > TijdN, !.
check_zelfde_dag(_,_," ", "").

sp_park(Speler, Park) :-
  sp_tijd(Speler, _, Park),
  fronttoken(Park, _, _).

check_parkwissel(Spelers, "W") :-
  baan_adm(ja),
  member(Speler,Spelers),
  findall(Park, sp_park(Speler, Park), Parken),
  sortstring_c(Parken, 1),
  removeDubl(Parken, [], ParkenX),
  ParkenX = [_, _ | _], !.
check_parkwissel(_Spelers, "").
  
rep_volgende1(_, _, _, Score) :-
 Score <> "",
 write("\n                            (tussen-)stand: ", Score),
 fail.
rep_volgende1(WdNo, Cat, DagP, _) :-
  bitright(WdNo,1,WdNext),
  w2(WdNext,Cat,Tijd,_,_,_,_,_), 
  tijd_kwart(Dag,_,_,Tijd),
  Dag <= DagP,
  dag(Dag,DStr,_), !,
  tijd_str(Tijd,Str1,_),
  write("\n                               bij winst: ",DStr," ",Str1).
rep_volgende1(_, _, _, _).

rep_volgende2(WdNo, Cat, DagP, [Str], Str) :-
  %etiketwaarden(_,_,_,_,_,_,_AantalPK,_Hor,RegelsPE,_FontG),
  bitright(WdNo,1,WdNext),
  w2(WdNext,Cat,Tijd,_,_,_,_,_), 
  tijd_kwart(Dag,_,_,Tijd),
  Dag <= DagP,
  dag(Dag,DStr,_), !,
  tijd_str(Tijd,Str1,_),
  format(Str, "vervolg op: %s%s",DStr,Str1), !.
  %inv_print(Str,KolommenPP,10000,RegelsPE), !.
rep_volgende2(_, _, _, [], "").

rep_volgende3(Score, [Str]) :-
 Score <> "",
 concat("(tussen-)stand: ", Score, Str), !.
 %etiketwaarden(_,_,_,_,_,_,_AantalPK,_Hor,RegelsPE,_FontG), !.
 %inv_print(Str,KolommenPP,10000,RegelsPE), !.
rep_volgende3(_, []).

bijdrage(ja, s(SKeus), Bedr, "") :-
  bijdrage(SKeus, Bedr), !.
bijdrage(ja, pw(SKeus), Bedr, "") :-
  bijdrage(SKeus, Bedr), !.
bijdrage(ja, p(SKeus1,SKeus2), Bedr1, Bedr2) :-
  bijdrage(SKeus1, Bedr1),
  bijdrage(SKeus2, Bedr2), !.
bijdrage(_, _, "", "").

bijdrage(SKeus, Bedr) :-
  sp2(SKeus,_,_N1,_Tel1,_ ,_,_,nee,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  %sp1(SKeus,_Spelersrt,_Naam,_,_,_,Bet,_,_,_,_,_,_,_,_,_,_,_,_),
  findall(Cat,sp_cat(SKeus,Cat),Catten),
  not(Catten = []), !,
%  writedevice(screen),
  sum(Catten,0.0,Som,"",_String),
  format(Bedr, "%7.2f",Som).
bijdrage(_SKeus, "").

%BEGIN_DLG Agenda
/**************************************************************************
	Creation and event handling for dialog: Agenda
**************************************************************************/

constants

%BEGIN Agenda, CreateParms, 22:10:22-27.10.2011, Code automatically updated!
  dlg_agenda_DlgType = wd_Modal
  dlg_agenda_Title = "Agendaoverzicht"
  dlg_agenda_RCT = rct(40,32,285,198)
  dlg_agenda_Flags = [wsf_Close,wsf_TitleBar]
  dlg_agenda_Help = idh_agenda
  dlg_agenda_Font = "Arial"
  dlg_agenda_FSize = 10
%END Agenda, CreateParms


predicates

  dlg_agenda_eh : EHANDLER
%  nondeterm wedscat_ingedeeld(string)
%  aantalDeelnemers(rsoort, integer, integer)
%  afval_poel(rsoort, string)
  check_ok(WINDOW)
  report_on(tijdl Agendatijden,string Rapdag,integer PlanTot,integer Form,
      catl Onderdelen,BOOLEAN Vrijebanen, slist Locaties, Boolean Weblinks, integer FontGr) -(i,i,i,i,i,i,i,i,i)
  getRB(WINDOW, integer)
  nondeterm catNo(SList, wedscat)
  write_form1(integer, string, BOOLEAN, wl)
nondeterm  header(integer, string, string)
  headerbox()
  aanw(integer Form, janee Bet, janee Waarsch)
  alle_spelers(tijdl)
nondeterm  prog_wedst(wedid,tijd, catl, slist Locaties)
nondeterm  tijden(integer,tijd)
  rep_vrij(integer, integer, BOOLEAN, string DagS, string TStr)
  firstPlanDag(integer)
  firstPlanDag(integer, integer, integerlist)
  setForm(WINDOW, integer Form)	% 0 = lijst, 1 = etiket
%procedure add_baanWH(janee)
procedure locatie(integer Form, janee BaanAdm, WINDOW) 
%procedure removeDubl(slist, slist, slist)
determ zeeflocaties(string, slist)
procedure locatieUit(janee, WINDOW, slist)
determ rep_wedGespeeld(integer, wedscat,string Datum, string Onderdeel, janee IBaan) - (i,i,i,i,i)
%procedure metGeplande(integer, catl, boolean)
procedure puntenStr(wedscat, integer, string)
procedure alsNogGepland(tijdl, catl, slist Locaties)

clauses

banen_file(_,_,_,_,3,_,_,_,_,_) :-		% 3 = etiketten
  etik_reset(),
  fail. 
banen_file(_Win, _, _, File, Form, _, _, _, _WebLinks,_) :-
  Form <> 4,	% 4 = webpagina
  closefile(work),
  openwrite(work,File),
  fail.
banen_file(_Win, RapDagen,DagPS,_File,Form,CatList,IVrijeBanen, Locaties, WebLinks,FontGr) :-
  betaal_adm(BetSig),
  oproepen(Waarsch),
  %closefile(work),
  %openwrite(work,File),
  assert(dags("")),
  dag(DagP,DagPS,_),
  writedevice(work),
  aanw(Form,BetSig,Waarsch),	% bevat koptekst etc. en <table> tag
  member(DagS,RapDagen),
  dag(DagN,DagS,_),
  assert(tijds("")),
  header(Form, DagS, DagPS),
  findall(Kwart,tijden(DagN,Kwart),AgendaTijden),
  sorttijd_c(AgendaTijden),
  report_on(AgendaTijden,DagS,DagP,Form,CatList,IVrijeBanen,Locaties,WebLinks,FontGr), % DagP = planning
  fail.
banen_file(Win,_,_,_,1,_,_,[],_,_) :-		% 1 = lijst
  closefile(work), 
  dlg_rapport_Create(Win, "Agenda","Agenda",b_False), !.
banen_file(Win,_,_,_,1,_,_,[Loc],_,_) :-		% 1 = lijst
  closefile(work), 
  concat("Agenda voor ", Loc, AgTekst),
  dlg_rapport_Create(Win, AgTekst,"Agenda",b_False), !.
banen_file(Win,_,_,_,2,_,_,_,_,_) :-		% 2 = uitgerekte lijst
  closefile(work), 
  dlg_rapport_Create(Win, "Agenda","Agenda",b_False), !.
banen_file(Win,_,_,_,3,_,_,_,_,_) :-		% etiketformaat
  %sluit_af(10000),
  closefile(work), 
  %aantalEtik(Aantal),
  %format(Msg, "\tTotaal aantal etiketten: %.", Aantal),
  dlg_etiketten_print_Create(Win, Answer), 
  printetiketten(Answer, nee), !.
banen_file(_Win,_,_,_,4,_,_,_,_,_) :- 		% 4 = webpagina
  write("\n</table>"), !.
banen_file(_,_,_,_,_Form,_,_,_,_,_) :-
  closefile(work).

/*metGeplande(Dag, CatList, b_True) :-
  member(Cat, CatList),
  w2(_,Cat,Tijd,_,_,_,_,_),
  bitright(Tijd, 7, Tijd1),
  Tijd1 = Dag, !.
metGeplande(_, _, b_False).*/

alsNogGepland(_, _, _) :-	% 22.2.2012
  not(uitslagenable(_)), !.
alsNogGepland(Tijden, CatList, Locaties) :-
  algeweestB(b_False),
  member(Tijd,Tijden),
  prog_wedst(_,Tijd,CatList,Locaties),
  assert(algeweestB(b_True)),
  write("\n<TR><TD>&nbsp;<TD>&nbsp;<TD colspan='7'><br>Klik op de winnaar(s) van een partij om de uitslag te melden of er tussenin voor gelijkspel!"),!. 
alsNogGepland(_, _, _).

report_on(_Tijden,DagS, _DagP,4,CatList,_IVrijeBanen,_Locaties,_WebLinks, _FontGr) :- 
  dag(Dag, DagS, _),
  assert(dags("")),
  wc(Cat, _, _, _Long, _, Short, _, _, _, _, _, _, _, _, _),
  %wc(Cat,_Long,_,Short,_,_,_),			% in volgorde van categorie 
  member(Cat,CatList),			% en in selectie
  w3(Num, Cat, _, _, _, _, Tijd),
  bitright(Tijd, 7, Tijd1),
  Tijd1 = Dag,				% gespeelde partijen
  %metGeplande(Dag, CatList, MetGeplande),
  dagstring(b_True, DagS, DagS1),	% voor de line-through
  baan_adm(IBaan),
  rep_wedGespeeld(Num, Cat, DagS1, Short, IBaan),
  fail.
report_on(Tijden,DagS, DagP,Form,CatList,IVrijeBanen,Locaties,WebLinks,_FontGr) :- 
  alle_spelers(Tijden),	% kijk vooruit voor de rest van de dag
  alsNogGepland(Tijden, CatList, Locaties),
  member(Tijd,Tijden),
  tijd_str(Tijd,TStr,_),
  findall(WedId, prog_wedst(WedId,Tijd,CatList,Locaties), WList),
  write_form1(Form,TStr,IVrijeBanen,WList),	% alleen voor 1 en 2
  bn(Tijd,N),					% om te tellen
  rep_wed(WList, N, N1, DagS, DagP, TStr, Form, WebLinks),
  rep_vrij(N1, Form, IVrijeBanen,DagS, TStr),		% alleen voor ?
  fail.
report_on(_, _, _, _, _, _, _, _, _) :-
  retractall(sp_tijd(_, _, _)).	% nodig! 

puntenStr(Cat, 0, "") :- 
  not(pos(Cat, _, _, poel)), !.
puntenStr(_, P, PS) :-
  str_int(PS, P).

rep_wedGespeeld(WdNo,Cat,Datum,_Onderdeel,IBaan) :-
  wd(WdNo,Cat,T1,T2,Uitsl,_,_),
  w3(WdNo, Cat, T3, P1, P2, _, _), 
  not(T3 = gelijk_spel),
  winnaar(T1, T2, T3, T4, T5, P1, P2, P3, P4),
  puntenStr(Cat, P3, P3S),
  puntenStr(Cat, P4, P4S),
  repa(ja,'}',nee,nee,nee,0,2,1,WdNo,Cat,T4,alleplanning,TS1,_,_,b_True,b_False,_,Anker,_,_,_,_,_),% onbeperkt
  repa(ja,'}',nee,nee,nee,1,2,1,WdNo,Cat,T5,alleplanning,TS2,_,_,b_True,b_False,_,_,_,_,_,_,_), !,
  makeOnderdeelLink(WdNo, Cat, Anker, FullLink),
  writef("\n<TR><TD class=agendan>%s<TD>&nbsp;", Datum),
  add_baanW(IBaan, WdNo, Cat),
  writef("<TD class=agendan>%s<TD class=agendab>%s<TD class=agendak>%s<TD>-<TD class=agendan>%s<TD class=agendak>%s<TD class=agendan>&nbsp;%s", FullLink, TS1, P3S, TS2, P4S, Uitsl).
rep_wedGespeeld(WdNo,Cat,Datum,Onderdeel,IBaan) :-
  wd(WdNo,Cat,T1,T2,Uitsl,_,_),
  w3(WdNo, Cat, T3, P1, P2, _, _),
  T3 = gelijk_spel, 
  puntenStr(Cat, P1, P1S),
  puntenStr(Cat, P2, P2S),
  repc(ja,'}',nee, nee,nee,0,2,1,WdNo,Cat,T1,alleplanning,TS1,_Scorex, _TtijdChangedx,_,_,_,_),
  repc(ja,'}',nee, nee,nee,1,2,1,WdNo,Cat,T2,alleplanning,TS2,_, _, _, _, _, _), !,
  writef("<TR><TD class=agendan>%s<TD>&nbsp;",Datum),
  add_baanW(IBaan, WdNo, Cat),
  writef("<TD class=agendan>%s<TD class=agendan>%s<TD class=agendak>%s<TD>-<TD class=agendan>%s<TD class=agendak>%s<TD class=agendan>&nbsp;%s", Onderdeel, TS1, P1S, TS2, P2S, Uitsl).
rep_wedGespeeld(WdNo,Cat,Datum,Onderdeel,IBaan) :-	% 24.9.2003
  wd(WdNo,Cat,T1,T2,Uitsl,_,_),
  repc(ja,'}',nee, nee,nee,0,2,1,WdNo,Cat,T1,alleplanning,TS1,_Scorex, _TtijdChangedx,_,_,_,_),
  repc(ja,'}',nee, nee,nee,1,2,1,WdNo,Cat,T2,alleplanning,TS2,_, _, _, _, _, _), !,
  writef("<TR><TD class=agendan>%s<TD>&nbsp;", Datum),
  add_baanW(IBaan, WdNo, Cat),
  writef("<TD class=agendan>%s<TD class=agendan>%s<TD>&nbsp;<TD>-<TD class=agendan>%s<TD>&nbsp;<TD class=agendan>&nbsp;%s", Onderdeel, TS1, TS2, Uitsl).


aanw(3, _, _) :- !.
/*aanw(4, _, _) :-
  assert(algeweest(b_False)),
  uitslagenable(_),
  write("\n<table>"),
  write("<TR><TD>&nbsp;<TD>&nbsp;<TD colspan='7'>Klik op de winnaar(s) van een partij om de uitslag te melden of er tussenin voor gelijkspel!"),!. */
aanw(4, _, _) :- 
  write("\n<table>"),
  assert(algeweestB(b_False)), !.
aanw(5, _, _) :-
  write("dag",csvconst,"tijdstip",csvconst,"baan",csvconst,"locatie",csvconst,"kort",csvconst,"lang",csvconst,
  "partij",csvconst,"partijL",csvconst,"tegenp",csvconst,"tegenpL",csvconst,"volgende",csvconst,
  "bedr1a",csvconst,"bedr1b",csvconst,"bedr2a",csvconst,"bedr2b",csvconst,"zelfdedag",csvconst,
  "naama1", csvconst, "naama2", csvconst,"vervolgvan1",csvconst,"naamb1",csvconst,"naamb2",csvconst,"vervolgvan2",csvconst,"parkwissel"), !.
aanw(_, _, _) :-
  strepen(42, ' '), write("f = met de hand gepland\n"), fail.
aanw(_,ja,_) :- 
  write(" $ = nog niet betaald"), strepen(20, ' '), fail.
aanw(_,_, _) :-
  write(" ! = zelfde dag nogmaals spelen\n"), fail.
aanw(_,_,ja) :- 
  write(" ? = niet gewaarschuwd"), strepen(19, ' '), fail.
aanw(_,_, _) :-
  write(" } = voorgaande wedstrijd"), strepen(16, ' '), fail.
aanw(_, _, _) :- 
  baan_adm(ja), nl,
  write(" [..] = baanlocatie "),
  fail.
aanw(_, _, _).

write_form1(_, _, b_False, []) :- !, fail.	% geen partijen
write_form1(Form, TStr, _, _) :-
  Form < 3,
  write("\n", TStr, " --------"), !.
%write_form1(5, TStr, _, _) :-	% 8.4.2010
%  write("\n", TStr, " --------"), !.
write_form1(_, _, _, _).

header(3, _, _) :- !.		% etiketten
header(5, _, _) :- !.		% mailmerge
header(4, _, _) :- !.		% html
header(_, DagS, DagPS) :-
  headerbox,
  nl,
  writef(" ======[ Agenda voor %-9.9]=======(vervolgplanning t/m %)=====", DagS, DagPS). 
  %headerbox.
header(_, _, _) :-
  write("\n\012"), fail.		% voor iedere complete dag op de terugweg ...

headerbox :- write("\n        "), strepen(22, '_').

alle_spelers(Tijden) :-		% het eerste tijdstip kan overgeslagen
  member(Tijd, Tijden),
  w2(WNum,Cat,Tijd,_,_,_,_,Park),		% pak een geprogrammeerde wedstrijd 
  wd(WNum,Cat,Sp1,Sp2,_,_,_),		% kijk wie er meespelen
  de_spelers(WNum,Cat,Sp1,Sp2,SpelerW),
  member(SpelerNum,SpelerW),
  assert(sp_tijd(SpelerNum, Tijd, Park)),	% leg alle spelers op die tijd vast
  fail.
alle_spelers(_).

zeefLocaties(_, ["Alle locaties"]) :- !.
zeefLocaties(_, []) :- !.
zeefLocaties(Loc, Locaties) :- 
  member(Loc, Locaties), !.

prog_wedst(w(WedNo,Cat,Kwart,nee,0,onbekend,b_False, Loc),Kwart, CatList, Locaties) :-
  wc(Cat, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
  %wc(Cat,_,_,_,_,_,_),			% in volgorde van categorie 
  member(Cat,CatList),			% en in selectie
  w2(WedNo,Cat,Kwart,_,_,_,_,Loc),
  zeefLocaties(Loc, Locaties).
  %  write("\n",WedNo,",",Cat,",",Kwart).

tijden(Dag,Tijd) :-
  bn(Tijd,_),
  bitright(Tijd, 7, D),
  D = Dag.

rep_vrij(0, _, _, _, _) :- !.	%geen vrije banen
rep_vrij(_, _, 0, _, _) :- !. %geen rapportage gevraagd
rep_vrij(_, Form,_, _, _) :-
  Form > 2,
  Form <> 5, !.
rep_vrij(N, _, _, _, _) :-
  N < 0, !,
  N1 = -N,
  write("\nOpmerking: Mogelijk te veel partijen! (",N1,")").
rep_vrij(N, 1, IBanen, DagS, DagT) :-
  N1 = N - 1, !,
  write("\n ___ baan vrij"),
  rep_vrij(N1, 1, IBanen, DagS, DagT).
rep_vrij(N, Form, IBanen, DagS, DagT) :-
  not(Form = 5),
  N1 = N - 1,
  write("\n ___ baan vrij\n"), !,
  rep_vrij(N1, Form, IBanen, DagS, DagT).
rep_vrij(N, 5, IBanen, DagS, DagT) :-
  write("\n",DagS,csvconst,DagT,csvconst,"\"\"",csvconst,"\"\"",csvconst,"\"\"",csvconst,"\"\"",csvconst,
  "\"\"",csvconst,"\"\"",csvconst,"\"\"",csvconst,"\"\"",csvconst,"\"\"",csvconst,
  "\"\"",csvconst,"\"\"",csvconst,"\"\"",csvconst,"\"\"",csvconst,"\"\"",csvconst,
  "\"\"", csvconst, "\"\"", csvconst,"\"\"",csvconst,"\"\"",csvconst,"\"\"",csvconst,"\"\"",csvconst,"\"\""),
  N1 = N - 1,
  %write("\n ___ baan vrij\n"),
  rep_vrij(N1, 5, IBanen, DagS, DagT).

check_ok(Win) :-
  OndWin   = win_GetCtlHandle(Win, idca_onderdelen),
  lbox_GetSel(OndWin,_ItemListO,IndexListO),
  not(IndexListO = []),
  DagWin   = win_GetCtlHandle(Win, idca_dagen),
  lbox_GetSel(DagWin,_ItemListD,IndexListD),
  not(IndexListD = []),
  %_DagTMWin = win_GetCtlHandle(Win, idca_planningtm),
  dialog_SetState(Win, [enable(idc_ok, b_True)]), !.
check_ok(Win) :-
  CBWin   = win_GetCtlHandle(Win, idca_toon_vrije_banen),
  b_True  = win_IsChecked(CBWin),
  %_DagTMWin = win_GetCtlHandle(Win, idca_planningtm),
  dialog_SetState(Win, [enable(idc_ok, b_True)]), !.
check_ok(Win) :-
  dialog_SetState(Win, [enable(idc_ok, b_False)]), !.

getRB(Win, 1) :-
  RB1 = win_GetCtlHandle(Win, idca_rblijst),
  Checked = win_IsChecked(RB1),
  Checked = b_True, !.
getRB(Win, 2) :-
  RB2 = win_GetCtlHandle(Win, idca_rbuitgerekte_lijst),
  Checked = win_IsChecked(RB2),
  Checked = b_True, !.

catNo(CatList, CatNo) :-
  member(String, Catlist),
  searchchar(String, '\t', Pos1),
  frontstr(Pos1, String, _, Str1),
  searchchar(Str1, '\t', Pos2),
  frontstr(Pos2, Str1, _, CatName),
  wc(CatNo, _, _, Catname, _, _, _, _, _, _, _, _, _, _, _).
  %wc(CatNo,Catname,_,_,_,_,_).
  
firstPlanDag(Index) :-
  findall(DagN, dag(_, DagN,_),DagenList),
  time_stamp1(_, Dag, _),
  dagIndex(Dag, DagenList, 0, Index),
  Index > 0,  !.
firstPlanDag(Index) :-
  findall(DagN, dag(DagN,_,_),DagenList),
  firstPlanDag(0, Index, DagenList), !.
firstPlanDag(0).
  
firstPlanDag(N, N, [Dag|_]) :-
  w2(_,_Cat,Tijd,_J1,_J2,_Geforceerd,_,_Bn),
  bitright(Tijd, 7, Dag1),
  Dag1 = Dag, !.
firstPlanDag(N, Out, [_|Dagen]) :-
  N1 = N + 1,
  firstPlanDag(N1, Out, Dagen).

setForm(Win, 4) :-
  dialog_SetState(Win, [show(idca_rblijst,b_False),show(idca_rbuitgerekte_lijst,b_False),
  %show(idca_formaat,b_False), show(idca_toon_vrije_banen,b_False)]), !,
  show(idca_formaat,b_False)]), !,
  dialog_SetStr(Win, idca_toon_vrije_banen, "als wedstrijdbriefjes"),
  CtrlWin = win_GetCtlHandle(Win, idca_toon_vrije_banen),
  win_Check(CtrlWin, b_True), 
  win_SetText(Win, "Wedsrrijdbriefjes").
setForm(Win, 3) :-
  dialog_SetState(Win, [show(idca_rblijst,b_False),show(idca_rbuitgerekte_lijst,b_False),
  show(idca_etiketsoort,b_False),enable(idca_etiketsoort,b_False),
  show(idca_formaat,b_False), show(idca_toon_vrije_banen,b_True)]), !,
  win_SetText(Win, "Agendasamenvoegbestand (etiketten etc.)").
setForm(Win, 2) :-
  dialog_SetState(Win, [show(idca_rblijst,b_False),show(idca_rbuitgerekte_lijst,b_False),
  show(idca_etiketsoort,b_False),enable(idca_etiketsoort,b_False),
  show(idca_formaat,b_False), show(idca_toon_vrije_banen,b_False)]), !,
  win_SetText(Win, "Webpagina").
setForm(Win, 1) :-
  dialog_SetState(Win, [show(idca_rblijst,b_False),show(idca_rbuitgerekte_lijst,b_False),
  show(idca_formaat,b_False), show(idca_toon_vrije_banen,b_False)]), !,
  %show(idca_formaat,b_False)]), !,
  %dialog_SetStr(Win, idca_toon_vrije_banen, "als wedstrijdbriefjes"),
  win_SetText(Win, "Agenda etiketten").
setForm(_Win, 0) :-
  dialog_SetState(_Win,[show(idca_etiketsoort,b_False),enable(idca_etiketsoort,b_False)]),
  RB1 = win_GetCtlHandle(_Win, idca_rblijst),
  RB2 = win_GetCtlHandle(_Win, idca_rbuitgerekte_lijst),
  %RB3 = win_GetCtlHandle(_Win, idca_rbetiketten),
  win_CheckRadioButton(RB1, [RB1, RB2]).

/*removeDubl([H1,H1|T], In, Uit) :-
  removeDubl([H1|T], In, UIt), !.
removeDubl([H|T], In, [H|Uit]) :-
  removeDubl(T, In, Uit), !.
removeDubl(_, In, In).*/

locatieUit(ja, Win, Locatie) :-
  BWin = win_GetCtlHandle(Win, idca_parken),
  lbox_GetSel(BWin, Locatie,_IndexList), !.
locatieUit(_, _Win, []) :- !.

locatie(0, ja, Win) :-		% alleen voor lijst
  BWin = win_GetCtlHandle(Win, idca_parken),
  findall(Loc,  w2(_,_Cat,_Tijd,_J1,_J2,_Geforceerd,_,Loc), LocList),
  sortstring_c(LocList, 1),
  removeDubl(LocList, [], Locaties),
  lbox_Add(BWin, ["Alle locaties"|Locaties]),
  lbox_SetSel(BWin, 0, b_True),
  win_SetState(BWin, [wsf_Visible, wsf_Enabled]), !.
locatie(_Form, _, Win) :- 
  BWin = win_GetCtlHandle(Win, idca_parken),
  win_SetState(BWin, [wsf_Invisible, wsf_Disabled]), !.
  
  dlg_agenda_Create(Parent, LijstOfEtiket):-
	assert(viewtype(LijstOfEtiket)),
	win_CreateDynDialog(Parent,
		[
%BEGIN Agenda, WinDefList, 22:10:22-27.10.2011, Code automatically updated!
		 dlg_font(wdef(dlg_agenda_DlgType,dlg_agenda_RCT,dlg_agenda_Title,u_DlgBase),
		 	  dlg_agenda_Font,dlg_agenda_FSize,dlg_agenda_Flags),
		 ctl(wdef(wc_LBox,rct(3,10,132,148),"",u_DlgBase),idca_onderdelen,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_MultiSelect,wsf_UseTabStops]),
		 ctl(wdef(wc_LBox,rct(136,12,190,91),"",u_DlgBase),idca_dagen,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_MultiSelect]),
		 ctl(wdef(wc_LBox,rct(193,12,243,91),"",u_DlgBase),idca_planningtm,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight]),
		 ctl(wdef(wc_RadioButton,rct(140,125,188,135),"&Lijst",u_DlgBase),idca_rblijst,[wsf_Auto,wsf_TabStop,wsf_Group]),
		 ctl(wdef(wc_RadioButton,rct(140,135,198,145),"Uitgerekte lijst",u_DlgBase),idca_rbuitgerekte_lijst,[wsf_Auto,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(210,147,242,164),"&OK",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(210,133,242,144),"&Cancel",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(210,119,242,130),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(6,153,32,164),"&Alle",u_DlgBase),idca_alle,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(39,153,65,164),"&Geen",u_DlgBase),idca_geen,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(137,92,162,103),"A&lle",u_DlgBase),idca_alled,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(164,92,189,103),"G&een",u_DlgBase),idca_geend,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_CheckBox,rct(137,105,226,115),"&Indicatie van vrije banen",u_DlgBase),idca_toon_vrije_banen,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_LBoxButton,rct(74,152,139,219),"",u_DlgBase),idca_parken,[wsf_Group,wsf_TabStop,wsf_VScroll]),
		 ctl(wdef(wc_PushButton,rct(156,153,204,164),"&Etiketsoort",u_DlgBase),idca_etiketsoort,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_Text,rct(3,0,51,10),"Onderdelen",u_DlgBase),idcta_onderdelen,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(137,2,185,12),"Dag(en)",u_DlgBase),idcta_dagen,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(192,2,240,12),"Planning t/m",u_DlgBase),idcta_planning_tm,[wsf_AlignLeft]),
		 ctl(wdef(wc_GroupBox,rct(137,116,200,148),"Formaat",u_DlgBase),idca_formaat,[])
%END Agenda, WinDefList
		],dlg_agenda_eh,0).

%BEGIN Agenda, idc_ok _CtlInfo
  dlg_agenda_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	%EtiketWin = win_GetCtlHandle(_Win, idca_etiketsoort),
	viewtype(0),		% lijst
	%Wsflags = win_GetState(EtiketWin),
	%member(wsf_Disabled, Wsflags),
	DagenWin = win_GetCtlHandle(_Win, idca_dagen),
	lbox_GetSel(DagenWin,RapDagen,_),
	DagPWin = win_GetCtlHandle(_Win, idca_planningtm),
	Index = lbox_GetSelIndex(DagPWin),
	DagPS = lbox_GetItem(DagPWin, Index),
	getRB(_Win, Form),
	OndWin = win_GetCtlHandle(_Win, idca_onderdelen),
	lbox_GetSel(OndWin,CatList1,_),
	findall(CatNo, catNo(CatList1, CatNo), CatList),
	CBWin = win_GetCtlHandle(_Win, idca_toon_vrije_banen),
	VrijeBanen = win_IsChecked(CBWin),
	baan_adm(BaanA),
	locatieUit(BaanA, _Win, Locatie),
	TaskWin = vpi_GetTaskWin(),
	get_TempDir(_, TempFile),
	banen_file(TaskWin, RapDagen,DagPS,TempFile,Form,CatList,VrijeBanen, Locatie, b_False, 0),
	win_Destroy(_Win),
	!.
  dlg_agenda_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	viewtype(1),		% etiketten
	DagenWin = win_GetCtlHandle(_Win, idca_dagen),
	lbox_GetSel(DagenWin,RapDagen,_),
	DagPWin = win_GetCtlHandle(_Win, idca_planningtm),
	Index = lbox_GetSelIndex(DagPWin),
	DagPS = lbox_GetItem(DagPWin, Index),
	OndWin = win_GetCtlHandle(_Win, idca_onderdelen),
	lbox_GetSel(OndWin,CatList1,_),
	findall(CatNo, catNo(CatList1, CatNo), CatList),
	TaskWin = vpi_GetTaskWin(),
	%UitInvWin = win_GetCtlHandle(_Win, idca_toon_vrije_banen),
	%MetUitslagInvul = win_IsChecked(UitInvWin),
	get_TempDir(_, TempFile),
	banen_file(TaskWin, RapDagen,DagPS,TempFile,3,CatList,b_False, [], b_False, 0),
	win_Destroy(_Win),
	!.
  dlg_agenda_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	viewtype(2),		% webpagina
	DagenWin = win_GetCtlHandle(_Win, idca_dagen),
	lbox_GetSel(DagenWin,RapDagen,_),
	DagPWin = win_GetCtlHandle(_Win, idca_planningtm),
	Index = lbox_GetSelIndex(DagPWin),
	DagPS = lbox_GetItem(DagPWin, Index),
	OndWin = win_GetCtlHandle(_Win, idca_onderdelen),
	lbox_GetSel(OndWin,CatList1,_),
	findall(CatNo, catNo(CatList1, CatNo), CatList),
	TaskWin = vpi_GetTaskWin(),
	getFolder(Dir,_,_,_),
	filenamepath(Filenaam, Dir, "agenda.htm"),
	%format(Filenaam, "%s\agenda.htm", Dir),
	banen_file(TaskWin, RapDagen,DagPS,Filenaam,4,CatList,b_False,[], b_False,0),
	win_Destroy(_Win),
	format(Txt, "\nBestand %s.\n\nZorg dat ook de webschema's up-to-date zijn!", Filenaam),
	dlg_MessageBox( "Webpagina", Txt, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	%write("\nBestand ", Filenaam),
	!.
  dlg_agenda_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	viewtype(3),		% mailmerge
	DagenWin = win_GetCtlHandle(_Win, idca_dagen),
	lbox_GetSel(DagenWin,RapDagen,_),
	DagPWin = win_GetCtlHandle(_Win, idca_planningtm),
	Index = lbox_GetSelIndex(DagPWin),
	DagPS = lbox_GetItem(DagPWin, Index),
	OndWin = win_GetCtlHandle(_Win, idca_onderdelen),
	lbox_GetSel(OndWin,CatList1,_),
	findall(CatNo, catNo(CatList1, CatNo), CatList),
	CBWin = win_GetCtlHandle(_Win, idca_toon_vrije_banen),
	VrijeBanen = win_IsChecked(CBWin),
	TaskWin = vpi_GetTaskWin(),
	%get_Toernooidir(TDir, _, _),
	getFolder(TDir,_,_,_),
	filenamepath(Brief, TDir, agendaconst),
	File = dlg_GetFileName(Brief,["txt","*.txt","dat","*.dat"],"Mailmerge bestand",[dlgfn_Save],"",_OutListFiles),
	filenamepath(File, _Path, Name),
	%nl, write(File, " ", Name),
	Name <> fctconst,
	Name <> logconst,
	banen_file(TaskWin, RapDagen,DagPS,File,5,CatList,Vrijebanen,[], b_False,0),
	win_Destroy(_Win),
	format(Txt, "\nBestand %s.", File),
	dlg_MessageBox( "Samenvoegbestand", Txt, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	%write(Txt),
	!.
  dlg_agenda_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	viewtype(4),		% uitslagbriefjes
	DagenWin = win_GetCtlHandle(_Win, idca_dagen),
	lbox_GetSel(DagenWin,RapDagen,_),
	DagPWin = win_GetCtlHandle(_Win, idca_planningtm),
	Index = lbox_GetSelIndex(DagPWin),
	DagPS = lbox_GetItem(DagPWin, Index),
	OndWin = win_GetCtlHandle(_Win, idca_onderdelen),
	lbox_GetSel(OndWin,CatList1,_),
	findall(CatNo, catNo(CatList1, CatNo), CatList),
	TaskWin = vpi_GetTaskWin(),
	%UitInvWin = win_GetCtlHandle(_Win, idca_toon_vrije_banen),
	%MetUitslagInvul = win_IsChecked(UitInvWin),
	get_TempDir(_, TempFile),
	banen_file(TaskWin, RapDagen,DagPS,TempFile,3,CatList,b_False, [], b_True, 0),
	win_Destroy(_Win),
	!.
%END Agenda, idc_ok _CtlInfo
%MARK Agenda, new events

%BEGIN Agenda, idca_toon_vrije_banen _CtlInfo
  dlg_agenda_eh(_Win,e_Control(idca_toon_vrije_banen,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	check_ok(_Win),
	!.
%END Agenda, idca_toon_vrije_banen _CtlInfo

%BEGIN Agenda, idc_help _CtlInfo
  dlg_agenda_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Agenda"),
	!.
%END Agenda, idc_help _CtlInfo

%BEGIN Agenda, idca_etiketsoort _CtlInfo
  dlg_agenda_eh(_Win,e_Control(idca_etiketsoort,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	dlg_etikesoort_create(_Win),
	!.
%END Agenda, idca_etiketsoort _CtlInfo


%BEGIN Agenda, idca_alled _CtlInfo
  dlg_agenda_eh(_Win,e_Control(idca_alled,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	DagenWin = win_GetCtlHandle(_Win, idca_dagen),
	selectAlle(DagenWin),
	check_ok(_Win),
	!.
%END Agenda, idca_alled _CtlInfo

%BEGIN Agenda, idca_geend _CtlInfo
  dlg_agenda_eh(_Win,e_Control(idca_geend,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	%dialog_SetListBox(_Win,idca_dagen,[]),
	ListWin = win_GetCtlHandle(_Win, idca_dagen),
	selectGeen(ListWin),
	check_ok(_Win),
	!.
%END Agenda, idca_geend _CtlInfo

%BEGIN Agenda, idca_planningtm selchanged
  dlg_agenda_eh(_Win,e_Control(idca_planningtm,_CtrlType,_CtrlWin,selchanged),0):-!,
	!.
%END Agenda, idca_planningtm selchanged

%BEGIN Agenda, dagen selchanged
  dlg_agenda_eh(_Win,e_Control(idca_dagen,_CtrlType,_CtrlWin,selchanged),0):-!,
	check_ok(_Win),
	!.
%END Agenda, dagen selchanged

%BEGIN Agenda, idca_geen _CtlInfo
  dlg_agenda_eh(_Win,e_Control(idca_geen,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	ListWin = win_GetCtlHandle(_Win, idca_onderdelen),
	selectGeen(ListWin),
	check_ok(_Win),
	!.
%END Agenda, idca_geen _CtlInfo

%BEGIN Agenda, idca_alle _CtlInfo
  dlg_agenda_eh(_Win,e_Control(idca_alle,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	OndWin = win_GetCtlHandle(_Win, idca_onderdelen),
	selectAlle(OndWin),
	check_ok(_Win),
	!.
%END Agenda, idca_alle _CtlInfo

%BEGIN Agenda, onderdelen selchanged
  dlg_agenda_eh(_Win,e_Control(idca_onderdelen,_CtrlType,_CtrlWin,selchanged),0):-!,
	check_ok(_Win),
	!.
%END Agenda, onderdelen selchanged

%BEGIN Agenda, idc_cancel _CtlInfo
  dlg_agenda_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	win_Destroy(_Win),
	!.
%END Agenda, idc_cancel _CtlInfo

%BEGIN Agenda, e_Destroy
  dlg_agenda_eh(_Win,e_Destroy,0):-!,
	retractall(sp_tijd(_,_, _)),
	!.
%END Agenda, e_Destroy

%BEGIN Agenda, e_Create
  dlg_agenda_eh(_Win,e_Create(_CreationData),0):-!,
	viewtype(Form),
	%write("\n",Form),
	findall(Onderdeel, wedscat_ingedeeld(Onderdeel, b_False, _, b_False), OnderdelenIn),
	OndWin = win_GetCtlHandle(_Win, idca_onderdelen),
	lbox_setTabstops(OndWin, [8,20,30]),
	lbox_Add(OndWin, OnderdelenIn),
	NoOfOnd = lbox_CountAll(OndWin),
	items2indexes(NoOfOnd, SetOndIndexList),
	dialog_SetListBox(_Win,idca_onderdelen,SetOndIndexList),
	DagWin = win_GetCtlHandle(_Win, idca_dagen),
	Font = font_Create(ff_Fixed, [], 10),
	win_SetFont(DagWin, Font),
	findall(DagN, dag(_, DagN, _), DagenList),
	lbox_Add(DagWin, DagenList),
	firstPlanDag(DagIndex),
	lbox_SetSel(DagWin, DagIndex, b_True),
	DagTMWin = win_GetCtlHandle(_Win, idca_planningtm),
	win_SetFont(DagTMWin, Font),
	lbox_Add(DagTMWin, DagenList),
	NoOfDagTM = lbox_CountAll(DagTMWin),
	IndexTM = NoOfDagTM - 1,
	lbox_SetSel(DagTMWin, IndexTM, b_True),
	baan_adm(Baan),
	locatie(Form, Baan, _Win), 
	setForm(_Win, Form),
	!.
%END Agenda, e_Create

  dlg_agenda_eh(_,_,_):-!,fail.

%END_DLG Agenda

%___________process___________________________________________________________

predicates
  voorste(integer, integerlist, integer)
  afz1(integer, integer, wedscat, tijd, janee, janee, char, tijd, string)
  uitslag_verw(integer,integer,wedscat,rsoort)  
  volg1(integer,wedscat,integer,tgnst,rsoort)  - (i,i,i,i,i)
  volg1(integer,wedscat,tgnst,rsoort) 
nondeterm overblijvende_opgave(wedscat,tgnst, integer, string)
%  tgnst_naam(tgnst, string)
nondeterm opg_naam(wedscat, tgnst, string, tgnst)
nondeterm oude_positie(wedscat, tgnst, integer)
  verwissel_met(wedscat, tgnst, integer, rsoort)
  nieuweScore(string, tgnst, string)
nondeterm  cat(string, wedstyp, sexlist, wedscat)
procedure  cat1(rsoort, string)
%nondeterm  plaats(integer, string)
procedure  plaatsNaam(integer, string)
%determ     catAlAanAndere(wedscat, wedscat)
%determ     selecteerKoppelBron(wedscat CatNo, integer Cursor)
procedure voorstel(wedscat, vandom, tgnst)
%procedure checkAlOpgave(wedscat CatNo, tgnst)

clauses

cat(CatStr,WT,SexLEigen,CatEigen) :-         
  wc(CatNo, _, _, Cat, WT, _Short, SexL, _, _, _, _, _, _, _, _),
  %wc(CatNo,Cat,WT,_,SexL,_,_),      /* ga het lijstje van onderdeeln af    */
  not(CatNo = CatEigen),
  getbacktrack(Here),
  member(SexE, SexLEigen),
  member(Sex, SexL),
  Sex = SexE, 
  pos(CatNo,_,_,RSoort),
  cutbacktrack(Here),
  %not(catAlAanAndere(CatNo, CatEigen)),
  cat1(RSoort, Str),
%  concat(Str, Cat, CatStr).
  findall(OpgaveNum, opg(OpgaveNum,CatNo,_,_,_,_,_,_,_,_,_),OpgL),
  count(OpgL, 0, Aant),
  format(CatStr, "%s\t%3d\t%s", Str, Aant, Cat).

/*catAlAanAndere(Cat, CatNaar) :-
  kopl(Cat, _, CatN1, _),
  CatN1 <> CatNaar, !.
catAlAanAndere(Cat, _CatNaar) :-
  kopl(_C, _, Cat, _), !.
catAlAanAndere(Cat, _CatNaar) :-
  wc(_,_,_,_,_,_,Cat), !.	% niet aan verliezersronde
*/
cat1(afval, " ") :- !.
cat1(poel, "p") :- !.
cat1(nietingedeeld, "-").

/*plaats(1, "Winnaar").
plaats(2, "Runner-up (tweede plaats)").*/
%plaats(3, "Derde/vierde plaats boven").
%plaats(4, "Derde/vierde plaats beneden").

selecteerKoppelBron(CatNo, _Cursor, VanPlaats, CatVan) :-
  wc(CatNo, _, _, _, WT, _Short, SexE, _, _, _, _, _, _, _, _),
  %wc(CatNo,_,WT,_,SexE,_,_),
  findall(CatStr,cat(CatStr, WT, SexE, CatNo),OnderdelenIn),
  TaskWin = vpi_GetTaskWin(),
  dlg_onderdeel_select_Create(TaskWin, "Voorronde (oorsprong):", OnderdelenIn, 0, Selectie, VanPlaats),
  Selectie = [Sel|_],		% NB op "oorsprong" wordt getest! Niet zomaar wijzigen (zie TASVDISP)
  searchchar(Sel, '\t', P1),
  frontstr(P1,Sel,_,S1),
  searchchar(S1,'\t',P2),
  frontstr(P2,S1,_,Onderdeel),
  wc(CatVan, _, _, Onderdeel, _, _, _, _, _, _, _, _, _, _, _),
  %wc(CatVan,Onderdeel,_,_,_,_,_),
  !.

checkAlOpgave(CatNo, Tgnst) :-
  not(Tgnst = onbekend),
  not(opg(_,CatNo,Tgnst,_,_,_,_,_,_,_,_)),		% nie opgegeven ?
  date(Year,Month,Day),
  time(Hours,Minutes,_Seconds,_Hundredths),
  dt_min_to_offset(Year, Month, Day, Hours, Minutes, Nr),
  %writedevice(X),
  %writedevice(screen),
  getbacktrack(Here),				% maak nieuwe
  numb(NNum),
  OutNum = Nr + NNum - 1,
  not(opg(OutNum,CatNo,_,_,_,_,_,_,_,_,_)),		% minuut verder als hij al bestond
  cutbacktrack(Here),
  logf('A',opg(OutNum,CatNo,Tgnst,0,0,0,0,"","",na,""), nee), !.
  %trap(writedevice(X),_,true), !.	% 29.6.2013
checkAlOpgave(_, _).

process(0,_,_,_,_,_) :- !.       % #################### global
process(_,0,_,_,_,_) :- !.
process(LinksRechts,_,Cat,Wnum,_,_Schema) :-
  %pos(Cat, _, Card, afval),
  Regel = 2 * Wnum + LinksRechts - 1,
  kopl(CatOrig, _Ans, Cat, plaats(Regel)),
  wc(CatOrig, _, _, Lang, _, _Short, _, _, _, _, _, _, _, _, _),
  %wc(CatOrig,Lang,_,_,_,_,_),
  format(Msg, "met %s\nVerwijderen/vervangen?", Lang),
  logclose(),
  2 = dlg_MessageBox( "Bestaande koppeling", Msg, mesbox_iconquestion, mesbox_buttonsokcancel, mesbox_defaultsecond, mesbox_suspendapplication ),
  !.
process(LinksRechts,_,Cat,Wnum,_,_Schema) :-
  %pos(Cat, _, Card, afval),
  Regel = 2 * Wnum + LinksRechts - 1,
  kopl(CatOrig, Ans, Cat, plaats(Regel)),
  logf('R', kopl(CatOrig, Ans, Cat, plaats(Regel)), ja),
  fail.
process(LinksRechts,1,Cat,Wnum,_,Schema) :-   % verander in bye 
  upd_wd(LinksRechts, WNum, Cat, bye, Schema), !,
  logclose().
process(LinksRechts,2,Cat,Wnum,_,Schema) :- 	% verander in overblijvende
  findall(Naam,overblijvende_opgave(Cat,_, 1, Naam),Namen),
  not(Namen = []),
  sortstring_c(Namen, 1),
  Res = dlg_ListSelect(" Opgaven ",Namen,0,Gekozen,_Keus),
  Res = b_True,
  overblijvende_opgave(Cat, TgnstKeus, 1, Naam1),
  Gekozen = Naam1,
  upd_wd(LinksRechts, WNum, Cat, TgnstKeus, Schema),
  logclose(), !.
process(LinksRechts,3,Cat,Wnum,TgnOorspr,Schema) :-	% verander in opgaven 
  findall(Naam,opg_naam(Cat,_,Naam,TgnOorspr),Namen),
  not(Namen = []),
  sortstring_c(Namen, 1),
  Ans = dlg_ListSelect(" Opgaven ",Namen,0,Gekozen,_Keus),
  Ans = b_True,
  opg_naam(Cat, TgnstKeus, Naam1,TgnOorspr),	% vertaal naar TgnstKeus
  Gekozen = Naam1,
  findall(WNumO,oude_positie(Cat, TgnstKeus, WNumO), WNumOL),
  upd_wd(LinksRechts, WNum, Cat, TgnstKeus, Schema), 
  logclose(),
  member(N, WNumOL),
  voorste(N, WNumOL, Grootste),
  verwissel_met(Cat, TgnOorspr, Grootste, Schema), !.
process(LinksRechts,4,Cat,Wnum,_,Schema) :-   % verander in onbekend (koppel) 
  upd_wd(LinksRechts, WNum, Cat, onbekend, Schema), 
  logclose(),
  !.
process(LinksRechts,5,CatNo,Wnum,_,Schema) :-		% verander in koppel 
  bitleft(Wnum, 1, C0),
  Cursor = C0 + LinksRechts - 1,
  selecteerKoppelBron(CatNo, Cursor, Rang, CatVan),
  not(Rang = algemeen),
  logf('A', kopl(CatVan, Rang, CatNo, plaats(Cursor)),ja), 
  update_poule(),
  voorstel(CatVan, Rang, Tgnst),	% als er al een uitslag is
  /*selecteerKoppelBron(CatNo, Cursor),
  wc(CatNo,_,WT,_,SexE,_,_),
  findall(CatStr,cat(CatStr, WT, SexE, CatNo),OnderdelenIn),
  TaskWin = vpi_GetTaskWin(),
  dlg_onderdeel_select_Create(TaskWin, "Voorronde:", OnderdelenIn, 0, Selectie),
  Selectie = [Sel|_],
  searchchar(Sel, '\t', P1),
  frontstr(P1,Sel,_,S1),
  searchchar(S1,'\t',P2),
  frontstr(P2,S1,_,Onderdeel),
  wc(Cat,Onderdeel,_,_,_,_,_),
  format(Plaats,"Koppeling met %s:",Onderdeel),
  findall(PlaatsX, plaats(_, PlaatsX), Plaatsen),
  dlg_ListSelect(Plaats,Plaatsen,-1,StrSel,_Index),
  %Ans <> resp_default,
  plaats(Ans, StrSel),
  logf('A', kopl(Cat, Ans, CatNo, Cursor),ja),*/
  term_str(tgnst,Tgnst,StringXX),
  dlg_note("checkAlOpgave", StringXX),
  checkAlOpgave(CatNo, Tgnst),
  upd_wd(LinksRechts, WNum, CatNo, Tgnst, Schema), 
  logclose(),
  !.
process(LinksRechts,6,Cat,Wnum,TgnOorspr,Schema) :-	% verander in opgaven zonder verwisselen #########
  findall(Naam,opg_naam(Cat,_,Naam,TgnOorspr),Namen),
  not(Namen = []),
  sortstring_c(Namen, 1),
  Ans = dlg_ListSelect(" Opgaven ",Namen,0,Gekozen,_Keus),
  Ans = b_True,
  opg_naam(Cat, TgnstKeus, Naam1,TgnOorspr),	% vertaal naar TgnstKeus
  Gekozen = Naam1,
  %findall(WNumO,oude_positie(Cat, TgnstKeus, WNumO), WNumOL),
  upd_wd(LinksRechts, WNum, Cat, TgnstKeus, Schema), 
  logclose(), !.
  %member(N, WNumOL),
  %voorste(N, WNumOL, Grootste),
  %verwissel_met(Cat, TgnOorspr, Grootste, Schema), !.
%process(_,_,_,_,_,_) :-  
%        sound(30,175), sound(30,196), sound(30,233), fail.

voorstel(CatVan, Rang, Doorgaande) :-	% vandom
  pos(CatVan, _, _, PouleAfval),
  vanPoelAfval(PouleAfval, CatVan, Rang, Doorgaande, _Pt, NogNietKlaar),
  NogNietKlaar = "", !.  % is bekend!
voorstel(_CatVan, _, onbekend).

voorste(N, List, _) :-
  member(N1, List),	% ga de lijst af en kijk of er eentje nog groter is
  N < N1, !, fail.
voorste(N, _, N).        

afz1(2, No, Cat, Tijd, ja, _, _, _, _) :-	% links (nl. de andere)
  wd(No,Cat,Tgnst,_,_,_,_),
  not(Tgnst = bye),
  not(Tgnst = onbekend),
  logf('A', ws(No, Cat, Tgnst, Tijd),ja), 
  fail.
afz1(1, No, Cat, Tijd, _, ja, _, _, _) :-	% rechts
  wd(No,Cat,_,Tgnst,_,_,_),
  not(Tgnst = bye),
  not(Tgnst = onbekend),
  logf('A', ws(No, Cat, Tgnst, Tijd),ja), 
  fail.
afz1(_, WNum, Cat, Tijd, J1, J2, X, Y, Z) :-
  logf('R',w2(WNum,Cat,Tijd,J1,J2,X,Y,Z),nee),
  select(Cat, WNum).

nieuweScore(_Score, onbekend, "") :- !.
nieuweScore(_Score, bye, "") :- !.
nieuweScore(Score, _, Score).

upd_wd(_, _WNum, Cat, Tgnst, _) :-
  not(Tgnst = onbekend),            % niet bij indelen
  not(Tgnst = bye),
  not(Tgnst = gelijk_spel),
  not(Tgnst = openplaats(_)),
  not(Tgnst = pw(_)),
  not(opg(_,Cat,Tgnst,_,_,_,_,_,_,_,_)),  % maak er dan een voor de zekerheid! 	 2.1.2010
  getbacktrack(Here),
  date(Year,Month,Day),
  time(Hours,Minutes,_Seconds,_),
  dt_min_to_offset(Year, Month, Day, Hours, Minutes, Nr),
  numb(N),     
  Nummer = Nr + N - 1,
  not(opg(Nummer,Cat,_,_,_,_,_,_,_,_,_)),
  cutbacktrack(Here),
  logf('A', opg(Nummer,Cat,Tgnst,0,0,0,0,"","",na,""),nee),	% maar eentje 
  fail.
upd_wd(_, WNum, Cat, bye, _) :-  		%bye:	   haal eventuele 
  w3(WNum,Cat,W,PW,PV,STat,T),				%  uitslag weg
  logf('R',w3(WNum,Cat,W,PW,PV,Stat,T),ja),
  fail.
upd_wd(_, WNum, Cat, onbekend, _) :-  		%onbekend:	   haal eventuele 
  w3(WNum,Cat,W,PW,PV,Stat,T),				%  uitslag weg
  logf('R',w3(WNum,Cat,W,PW,PV,Stat,T),ja),
  fail.
upd_wd(L_R, WNum, Cat, bye, _) :-	 	% haal eventuele 
  w2(WNum,Cat,Tijd,J1,J2,X,Y,Z),		%  planning weg
  afz1(L_R, WNum, Cat,Tijd, J1, J2, X, Y, Z),	% vóórdat w2 verdwijnt
  fail.
upd_wd(L_R, WNum, Cat, onbekend, _) :-	 	% ___________________________ 
  w2(WNum,Cat,Tijd,J1,J2,X,Y, Z),		% bye wordt opgevuld?
  afz1(L_R, WNum, Cat,Tijd, J1, J2, X, Y, Z),	% zeg af vóórdat w2 verdwijnt
  fail.						% ____________________________
upd_wd(1, WNum, Cat, TgnstKeus, Schema) :-  		% 1 = links
  wd(Wnum,Cat,_,T2,Score,Parm1,Nota), !,
  nieuweScore(Score, TgnstKeus, ScoreN),
  select(Cat, WNum),
  logf('Z',wd(Wnum,Cat,TgnstKeus,T2,ScoreN,Parm1,Nota),ja),
  uitslag_verw(1,Wnum,Cat,Schema).
upd_wd(2, WNum, Cat, TgnstKeus, Schema) :-		% 2 = rechts
  wd(Wnum,Cat,T1,_,Score,Parm1,Nota), !,
  nieuweScore(Score, TgnstKeus, ScoreN),
  select(Cat, Wnum),
  logf('Z',wd(Wnum,Cat,T1,TgnstKeus,ScoreN,Parm1,Nota),ja),
  uitslag_verw(2,Wnum,Cat,Schema).

uitslag_verw(L_R, WNum,Cat,_) :- % check of planning kan blijven..
  w2(WNum,Cat,Tijd,J1,J2,X,Y,Z),
  check_forceer_c(0, [Tijd], Wnum, Cat, _, Result,_,_),
  Result <> 0,
  select(Cat, WNum),
  %tijd_str(Tijd,TStr1),
  %bitright(Tijd,7,Dag),
  %dag(Dag,DStr1,_),
  %openwrite(work, tmpconst),
  %writedevice(work),
  %write("Tijdstip: ",DStr1," ",TStr1),
  check_forceer_c(1, [Tijd], Wnum, Cat, _, _, Bezwaren,_), 
  %closefile(work),
  %file_str(tmpconst,Bezwaren),
  Parent = vpi_GetTaskWin(),
  dlg_bezwaren_Create(Parent, Wnum, Cat, Bezwaren, KanBlijven),
  KanBlijven <> 0,
  afz1(L_R, WNum, Cat,Tijd,J1,J2, X, Y, Z),	% haal planning weg
  fail.
uitslag_verw(_,_, _ , poel) :- !.		% voor poel niet verder 
uitslag_verw(_,No,Cat,Schema) :-      /* allebij zijn bye       */ 
  wd(No,Cat,bye,bye,_,_,_), !,
  volg1(No,Cat,bye,Schema).
uitslag_verw(_,No,Cat,Schema) :-      /* een van de twee is bye */
  wd(No,Cat,WinNo,bye,_,_,_), !,
  volg1(No,Cat,WinNo,Schema).
uitslag_verw(_,No,Cat,Schema) :-      /* de ander is bye        */
  wd(No,Cat,bye,WinNo,_,_,_), !,
  volg1(No,Cat,WinNo,Schema).
uitslag_verw(_,No,Cat,Schema) :-      /* uitslag onbekend        */
  volg1(No, Cat, onbekend, Schema).

volg1(No, Cat, Tgnst, Schema) :-
  bitright(No,1,VolgNo),
  bitand(No,1,A),
  volg1(VolgNo,Cat,A,Tgnst,Schema).

volg1(0,_,_,_,_) :- !.                  /* voor de finale is er geen */
volg1(No,Cat,1,Winnaar,_) :-            /* volgende ronde !          */
  wd(No,Cat,_,Winnaar,_,_,_), !.          /* winnaar al bekend         */ 
volg1(No,Cat,0,Winnaar,_) :-            /* volgende ronde !          */
  wd(No,Cat,Winnaar,_,_,_,_), !.          /* winnaar al bekend         */ 
volg1(No,Cat,LR,Winnaar,Schema) :-      /* volgende ronde !          */
  LofR = LR + 1,
  upd_wd(LofR, No, Cat, Winnaar, Schema), !. % en ga door als schema al bestaat
volg1(No,Cat,1,Winnaar,Schema) :-        /* nog geen registratie      */
  !, 
  logf('Z',wd(No,Cat,onbekend,Winnaar,"",0,""),ja),
  uitslag_verw(1,No,Cat,Schema).
volg1(No,Cat,0,Winnaar,Schema) :-
  !, 
  logf('Z',wd(No,Cat,Winnaar,onbekend,"",0,""),ja),
  uitslag_verw(1,No,Cat,Schema).

plaatsNaam(I, Uit) :-
  I <> 0, !,
  format(Uit, "(%u)", I).
plaatsNaam(_, "").

overblijvende_opgave(Cat,Tgnst, 1, Naam) :-
  opg(_,Cat,Tgnst,Plts,_,_,_,_,_,_,_),
  plaatsNaam(Plts,PlaatsS),
  not(wd(_,Cat,Tgnst,_,_,_,_)),
  not(wd(_,Cat,_,Tgnst,_,_,_)),
  tgnst_naam(nee, b_False, Cat, Tgnst, Naam1, _),
  format(Naam, "%s%s",Naam1,PlaatsS).
overblijvende_opgave(Cat,Tgnst, 0, "") :-
  opg(_,Cat,Tgnst,_,_,N,_,_,_,_,_),
  N <> 0.

tgnst_naam(Omdraaien, Voorltr, _, s(SpNo),NaamU,Tel) :-
  sp2(SpNo,_,Naam,Tel,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  naamscan(Omdraaien,Voorltr, Naam, NaamU,_,_Landcode), !.
tgnst_naam(Omdraaien, Voorltr, _, pw(SpNo),Text,Tel) :-
  sp2(SpNo,_,Naam,Tel,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  naamscan(Omdraaien,Voorltr, Naam, NaamU,_,_Landcode),
  format(Text, "%s zoekt partner", NaamU), !.
tgnst_naam(Omdraaien, Voorltr, _, p(Sp1,Sp2),Text,Tel) :-
  sp2(Sp1,_,N1,Tel1,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  naamscan(Omdraaien,Voorltr, N1, N1U,_,_Landcode1),
  sp2(Sp2,_,N2,Tel2,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  naamscan(Omdraaien,Voorltr, N2, N2U,_,_Landcode2),
  format(Text, "%+%", N1U, N2U), !,
  format(Tel,"%s\n%s", Tel1, Tel2). 
tgnst_naam(_, _, CatT, openplaats(I), Text, "") :- 
  CatT <> 0,
  kopl(CatV, rang(Nr), CatT, plaats(I)), 
  wc(CatV, _, _, CatS, _, _Short, _, _, _, _, _, _, _, _, _), !,
  %wc(CatV,CatS,_,_,_,_,_), !,
  format(Text, "Nr %u van %s", Nr, CatS).
tgnst_naam(_, _, CatT, openplaats(I), Text, "") :- 
  CatT <> 0, !,
  format(Text, "Open plaats %u", I).
tgnst_naam(_, _, _, _,"??","").

opg_naam(Cat, Tgnst, Naam,TgnOorspr) :-
  opg(_,Cat,Tgnst,Plts,_,_,_,_,_,_,_),
  not(Tgnst = TgnOorspr),
  tgnst_naam(nee, b_False, Cat, Tgnst, Naam1, _),
  plaatsNaam(Plts, PlaatsStr),
  format(Naam, "%s%s", Naam1, PlaatsStr).

oude_positie(_, bye, _) :- !, fail.
oude_positie(Cat, Tgnst, WNum1) :-
  wd(WNum,Cat,Tgnst,_,_,_,_),
  bitleft(WNum, 1, WNum1).
oude_positie(Cat, Tgnst, WNum2) :-
  wd(WNum,Cat,_,Tgnst,_,_,_),
  bitleft(WNum, 1, WNum1),
  WNum2 = WNum1 + 1.

verwissel_met(_, _, _, poel) :- !.
verwissel_met(Cat, TgnOorspr, Grootste, Schema) :-
  bitright(Grootste, 1, WNumN),
  bitand(Grootste, 1, LR),
  LinksRechts = LR + 1,
%  bitright(WNumN, 1, WNumN1),
  upd_wd(LinksRechts, WNumN, Cat, TgnOorspr, Schema), !,
  logclose(),
  select(Cat, WNumN).
verwissel_met(_, _, _, _) :- !.

%_____________________________ indeling ___________________________________
predicates
  %ruim_op(wedscat, slist, integer)
%nondeterm  opg_cat(string,integer)
  opg1(wedscat,string,integer)

clauses

/*opg_cat(Cat1,Functie) :-         
   wc(CatNo,Cat,_,_,_,_,_),      % ga het lijstje van onderdeeln af
   opg1(CatNo,Text,Functie),     % en voorzie van merk
%   concat(Text,Cat,Cat1).
  findall(OpgaveNum, opg(OpgaveNum,CatNo,_,_,_,_,_),OpgL),
  count(OpgL, 0, Aant),
  format(Cat1, "%s\t%3d\t%s", Text, Aant, Cat).*/
     
opg1(CatNo,"x ",_) :-            % al ingedeeld
  pos(CatNo,_,_,afval), !.
opg1(CatNo,"p ",_) :-            % al ingedeeld in poeltje
  pos(CatNo,_,_,poel), !.
opg1(_,"  ",_).


%BEGIN_DLG Onderdelen select
/**************************************************************************
	Creation and event handling for dialog: Onderdelen select
**************************************************************************/

constants

%BEGIN Onderdelen select, CreateParms, 15:35:13-2.1.2005, Code automatically updated!
  dlg_onderdelen_select_DlgType = wd_Modal
  dlg_onderdelen_select_Title = "Selecteer Onderde(e)l(en)"
  dlg_onderdelen_select_RCT = rct(40,32,241,208)
  dlg_onderdelen_select_Flags = [wsf_Close,wsf_TitleBar]
  dlg_onderdelen_select_Help = idh_selecteer_onderdelen
%END Onderdelen select, CreateParms

predicates

  dlg_onderdelen_select_eh : EHANDLER
  dlg_onderdelen_select_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST, slist)
  dlg_onderdelen_select_update(DIALOG_VAL_LIST, slist)
procedure checkOK(WINDOW)

clauses

checkOK(Win) :-
  LboxWin = win_GetCtlHandle(Win, idco_lbselect),
  lbox_GetSel(LboxWin,_,IndexList),
  not(IndexList = []),
  dialog_SetState(Win, [enable(idc_ok, b_True)]), !.
checkOK(Win) :-
  dialog_SetState(Win, [enable(idc_ok, b_False)]).

  dlg_onderdelen_select_Create(Parent, Titel, IDCO_LBSELECT_ITEMLIST, Uit):-
	Parm = cast(long,Titel),
%MARK Onderdelen select, new variables

	dialog_Create(Parent,
		[
%BEGIN Onderdelen select, WinDefList, 15:35:13-2.1.2005, Code automatically updated!
		 dlg(wdef(dlg_onderdelen_select_DlgType,dlg_onderdelen_select_RCT,dlg_onderdelen_select_Title,u_DlgBase),dlg_onderdelen_select_Flags),
		 ctl(wdef(wc_LBox,rct(0,1,164,161),"",u_DlgBase),idco_lbselect,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_MultiSelect,wsf_UseTabStops]),
		 ctl(wdef(wc_PushButton,rct(20,162,41,173),"&Alle",u_DlgBase),idco_alle,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(56,162,77,173),"&Geen",u_DlgBase),idco_geen,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(165,156,201,174),"&Volgende",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop,wsf_Disabled]),
		 ctl(wdef(wc_PushButton,rct(165,142,201,152),"&Cancel",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(165,130,201,140),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop])
%END Onderdelen select, WinDefList
		],
  		[
%BEGIN Onderdelen select, ControlList, 15:35:13-2.1.2005, Code automatically updated!
		df(idco_lbselect,listbox(IDCO_LBSELECT_ITEMLIST,[]),nopr)
%END Onderdelen select, ControlList
		],
		dlg_onderdelen_select_eh,Parm,VALLIST,ANSWER),
	dlg_onderdelen_select_handle_answer(ANSWER,VALLIST,Uit).

  dlg_onderdelen_select_handle_answer(idc_ok,_VALLIST,Uit):-!,
	dlg_onderdelen_select_update(_VALLIST,Uit).
  dlg_onderdelen_select_handle_answer(idc_cancel,_,[]):-!.  % Handle Esc and Cancel here
  dlg_onderdelen_select_handle_answer(_,_,[]):-
	errorexit().

  dlg_onderdelen_select_update(_VALLIST, _IDCO_LBSELECT_ITEMLIST):-
%BEGIN Onderdelen select, Update controls, 15:35:13-2.1.2005, Code automatically updated!
	dialog_VLGetListBox(idco_lbselect,_VALLIST,_IDCO_LBSELECT_ITEMLIST,_IDCO_LBSELECT_SELECT),
%END Onderdelen select, Update controls
	true.

%MARK Onderdelen select, new events

%BEGIN Onderdelen select, idc_help _CtlInfo
  dlg_onderdelen_select_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Selecteeronderdelen"),
	!.
%END Onderdelen select, idc_help _CtlInfo

%BEGIN Onderdelen select, idco_lbselect selchanged
  dlg_onderdelen_select_eh(_Win,e_Control(idco_lbselect,_CtrlType,_CtrlWin,selchanged),0):-!,
	checkOK(_Win),
	!.
%END Onderdelen select, idco_lbselect selchanged

%BEGIN Onderdelen select, idco_geen _CtlInfo
  dlg_onderdelen_select_eh(_Win,e_Control(idco_geen,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	OndWin = win_GetCtlHandle(_Win, idco_lbselect),
	selectGeen(OndWin),
	checkOK(_Win),
	!.
%END Onderdelen select, idco_geen _CtlInfo

%BEGIN Onderdelen select, idco_alle _CtlInfo
  dlg_onderdelen_select_eh(_Win,e_Control(idco_alle,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	OndWin = win_GetCtlHandle(_Win, idco_lbselect),
	selectAlle(OndWin),
	checkOK(_Win),
	!.
%END Onderdelen select, idco_alle _CtlInfo

%BEGIN Onderdelen select, e_Create
  dlg_onderdelen_select_eh(_Win,e_Create(_CreationData),0):-
	LboxWin = win_GetCtlHandle(_Win, idco_lbselect),
	lbox_setTabstops(LboxWin, [8,20,30]),
	checkOK(_Win),
	Titel = cast(string, _CreationData),
	Titel <> "",
	win_SetText(_Win, Titel),
	OndWin = win_GetCtlHandle(_Win, idco_lbselect),
	selectAlle(OndWin),
	checkOK(_Win), !.
%END Onderdelen select, e_Create

  dlg_onderdelen_select_eh(_,_,_):-!,fail.

%END_DLG Onderdelen select



%___________________________ opgaven _____________________________________

domains
  tl  = tgnst*
   p  = p(integer,integer)
  pplist = p* 
% wc      =  wc(wedscat,string,wedstyp,string,sexlist,real,wedscat) 
% wclist  =  wc*


database - lokalopgave

opg_data(integer,string, integer, string, integer, integer)
  %       opg#    strk    plaats  naam


predicates
/*nondeterm  opg_cat(string, integer)
  opg1(wedscat,string, integer)*/
procedure  scan_sterkte(wedscat, integer)
  maak_numlijst(integer, integerlist, wedscat)
  rep3(integer, wedscat, tgnst)
%  st_list(slist)
  in_weds1(wedscat, tgnst, integer)
%  get_st(tgnst, string, slist, string)
  aant_tekst(integer, string)
  get_mv(sexe, char)
nondeterm volgens_sterkte(integer)
nondeterm  check_numb(unsigned, integer, integer)
  get_plts(integer,string, string CSS)
  get_uitsluit(integer, string, string CSSIn, string CSS)
  get_ronde(integer, string)
%  max_st(string, string, string, slist)
  %vrij_opgl(integer, integer, integer, wedscat)
procedure  ruimOpWC(wedscat)
%  positionWC(wedscat, catl, catl)
%  evenWegWC(catl, wclist)
%  sluitAanWC(wclist)
nondeterm  leeftijden(integer)
procedure  vastgesteld(string Text, integer L, integer H)

clauses

leeftijden(L) :-	
  opg_data(_OpgaveNum, _StOpg, _PLts, _Naam, L, _),
  L <> 0.
leeftijden(L) :-
  opg_data(_OpgaveNum, _StOpg, _PLts, _Naam, _, L),
  L <> 0.

vastgesteld(_, 0, 0) :- !.
vastgesteld(Text,L, H) :-
  writef("\n %s: %3d - %3d.",Text, L, H), !.

rep_cat_opg(CatS, Volgorde) :-
  wc(Cat, _, _, CatS, _, _Short, _, _, _, _, _, _, _, _, _),
  %wc(Cat,CatS,_,_,_,_,_),
%  closefile(work),
%  closefile(nawf),
  %get_TempDir(_, TempFile),
%  getFolder(TDir,_,_,_),
%  filenamepath(FullName,TDir,"opgaven.html"),
%  openwrite(work, FullName),
%  writedevice(work),
	%webrapport(Font, Kleur),
  naam(ToernooiNaam, Nr, Datum, _, _, _),
  rapport_CSS(CSSContent),
  metaTag(Eerst, Tag),
  write(Eerst),
  write("\n<HTML><HEAD>", Tag, "<TITLE>Rapport van de opgaven</TITLE></HEAD>"),
  write("\n<STYLE>"),
  write(CSSContent),
  write("\n</STYLE><BODY>"),
  write("<PRE>"),
  write("Toernooinaam: ", Toernooinaam, " Toernooinummer: ", Nr, "\nSpeeldata: ", Datum), 
  %openwrite(work,TempFile),
  %get_toernooidir(TDir, _, _),
  %format(NawFF, "%sopgaven.txt", TDir),
%  filenamepath(NawFF, TDir, "opgaven.txt"),
%  openwrite(nawf, NawFF),
  writedevice(nawf),
  writef("\n%s%c%s%c%s%c%s%c%s%c%s%c%s%c%s%c%s%c%s%c%s%c%s%c%s%c%s%c%s%c%s%c%s%c%s%c%s%c%s%c%s%c%s", 
        /*   1   2   3   4   5   6   7   8   9   0   1   2   3   4   5   6   7   8   9   0   1   2 */
              "inschrtijd",csvconst,"plaatsing",csvconst,"uitgesloten",csvconst, "ronde",csvconst,
              "club1",csvconst,"bondsnr1",csvconst,"manvr1",csvconst,"naam1",csvconst,
              "spsterkte1",csvconst,"rating1",csvconst,"geboortedatum1",csvconst,"uitkantoor1",csvconst, "verhinderingen1",csvconst,
              "club2",csvconst,"bondsnr2",csvconst,"manvr2",csvconst,"naam2",csvconst,
              "spsterkte2",csvconst,"rating2",csvconst,"geboortedatum2",csvconst,"uitkantoor2",csvconst, "verhinderingen2"),
  writedevice(work),
  findall(Opg1,opg(Opg1,Cat,_,_,_,0,_,_,_,_,_),OpgL1),
  findall(Opg2,opg(Opg2,Cat,_,_,_,1,_,_,_,_,_),OpgL2),
  count(OpgL1,0,No1),
  count(OpgL2,0,No2),
  concat("Opgavenoverzicht van ",CatS, Str1),
  write("\n", Str1),
  write("\nAantal opgaven       : ",No1, '.'),
  write("\nAantal uitsluitingen : ",No2, '.') ,
  date(_Year,Month,Day),
  helestr_int(0, Month1, Month),
  time(Hours,Minutes,_Seconds,_Hundredths),
  helestr_int(0, Minutes1, Minutes),
  format(DatumStr, "%d-%s %d:%s", Day, Month1, Hours, Minutes1),
  write("\nDatum rapport         :", DatumStr),
  wc(Cat, _, _, CatS, _, _, _, _, _, _, _, _, _, _, _),
  %wc(Cat,CatS,_,_,_,_,_),
  %scan_sterkte(Cat, 0),	 %================================ !
  scan_sterkte(Cat, 2),	 %================================ !
  write("</pre>"),
  write("\n<TABLE border=1 cellpadding=3 cellspacing=0>"),  
  write("\n<TR><TH>Opgave<br>datum/tijd</TH><TH>Club</TH><TH>Bondsnr</TH><TH>M<br>V</TH><TH>Naam</TH><TH>Spst</TH><th>Rating</th><TH>Plts</TH><TH>Geb.<br>datum</TH><TH>Uitsl</TH><TH>Rnd</TH></TR>\n"),
  maak_numlijst(Volgorde, NumLijst, Cat),
  member(Num, NumLijst),
  opg(Num,Cat,T,_,_,_,_,_,_,_,_),
  rep3(Num,Cat,T),
  fail.
rep_cat_opg(CatN, _) :-
  retractall(_, lokalopgave),
  write("\n</table>"),
  CatN <> "",
  fail.

opgavenNaarSterkte(Cat, NumLijst) :-	% global
  scan_sterkte(Cat, 2),
  findall(Opgave, volgens_sterkte(Opgave), NumLijst),
  retractall(_, lokalopgave), !.

scan_sterkte(Cat, Aanwezig) :-
  %st_list(StList),
  opg(OpgaveNum,Cat,Tgnst,Plts,_,_,_,_,_,_,_),
  in_weds1(Cat, Tgnst, Aanwezig),
  get_st(Tgnst, StOpg, Naam, Lft1, Lft2, b_True),
  %speelsterkteOudNieuw(StOpg, StOpg1),
  assert(opg_data(OpgaveNum, StOpg, PLts, Naam, Lft1, Lft2)),
  fail. 
scan_sterkte(_, _) :-
  findall(Stx, opg_data(_, Stx, _Plts1, _Naam, _, _), StxL),
  sortstring_c(StxL, 1),
  removeDubl(StxL, [], StList),
  %st_list(StList),
  %member(St, StList),
  write("\n Sterkte : "),
  %st_list(StList),  
  member(St, StList),
  writef("%-4", St),
  fail.
scan_sterkte(_, _) :-
  findall(Stx, opg_data(_, Stx, _Plts1, _Naam, _, _), StxL),
  sortstring_c(StxL, 1),
  removeDubl(StxL, [], StList),
  write("\n Aantal  : "),
  %st_list(StList),  
  member(St, StList),  
  findall(Num, opg_data(Num, St, _, _, _, _), NumList),
  count(NumList, 0, Aantal),
  aant_tekst(Aantal, Tekst),
  writef("%-4", Tekst),
  fail.
scan_sterkte(Cat, _) :-
  wc(Cat, _, _, _, _, _Short, _, _, leeftijd(_,SpecLow), leeftijd(_,SpecHigh), _, _, _, _, _),
  %  wc(Cat,_Kop,_EDG,_Kort,_VoorWie,_Geld,_Verl),
  %wcritp(Cat, _SpSterkte, SpecLow, SpecHigh, _),
  %not(EDG = g),
  findall(Lft, leeftijden(Lft), LftL),
  sortint_c(LftL),
  LftL = [MinSt|_],
  reverse(LftL, [], LftLR),
  LftLR = [MaxSt|_],
  writef("\n Min/max leeftijd ingeschreven spelers: %3d - %3d.",MinSt, MaxSt),
  vastgesteld("Min/max leeftijd volgens regels      ",SpecLow, SpecHigh), !.
scan_sterkte(_, _).

aant_tekst(0, "") :- !.
aant_tekst(A, A1) :-
  str_int(A1, A).

maak_numlijst(0, NumLijst, Cat) :-
  findall(OpgaveNum,   opg(OpgaveNum,Cat,_,_,_,_,_,_,_,_,_), NumLijst), 
  sortint_c(NumLijst), !.
maak_numlijst(1, NumLijst, _) :-
  findall(Opgave, volgens_sterkte(Opgave), NumLijst).

  
rep3(VlgNr, Cat, s(SpNo)) :-	% NB rapport + mailmerge in parallel!!
  dt_minoffset_to_str(VlgNr, "%DD/%MD %HH:%MM", VlgS), 
  opg(_,Cat,s(SpNo),Plts,Rnd,Uits,_,_,_,_,_),
  sp2(SpNo,Srt,Naam,_,_,RatingE,_RatingD,_,UitKant, Club, _, _, KNLTB, StEnkel, _DS, Gebdat,_,_,_, _,_,_,_,_,_,_,_),
  spsrt(Srt, _, MVs, _, _, _, _, _),
  get_mv(MVs, MV),
  get_plts(Plts, Plaats, PlaatsCSS),
  get_uitsluit(Uits, Uitsl, PlaatsCSS, UitslCSS),
  trim_c(Uitsl),
  get_ronde(Rnd, Ronde),
  tijd_str(Uitkant, UStr,_),
  isLeeg(Club, Club1), isLeeg(KNLTB, KNLTB1),isLeeg(StEnkel, StEnkel1), isLeeg(Plaats, Plaats1),  isLeeg(GebDat, GebDat1), isLeeg(Uitsl, Uitsl1), isLeeg(Ronde, Ronde1),isLeeg(RatingE,RatingE1),
  writef("\n<tr %s><td class='middle'>%</td><td>%</td><td>%</td><td class='middle'>%c</td><td>%</td><td class='middle'>%</td><td class='middle'>%</td><td class='middle'>%</td><td>%</td><td class='middle'>%</td><td class='middle'>%</td></trd>", 
          UitslCSS, VlgS, Club1,KNLTB1, MV, Naam, StEnkel1, RatingE1, Plaats1, GebDat1, Uitsl1, Ronde1),

  writedevice(X),
  writedevice(nawf),
  writef("\n\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%c\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%c\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"",
          /*   1       2       3       4       5       6       7       8       9       0       1       2       3       4       5       6       7       8       9       0       1       2  */
              VlgS,csvconst,Plaats,csvconst,Uitsl,csvconst,Ronde,csvconst,
              Club,csvconst,KNLTB,csvconst,MV,csvconst,Naam,csvconst,
              StEnkel,csvconst,RatingE,csvconst,GebDat,csvconst,UStr,csvconst,"",csvconst,
              "",csvconst,"",csvconst,' ',csvconst,"",csvconst,
              "",csvconst,"",csvconst,"",csvconst,"",csvconst,""),
        /*   1   2   3   4   5   6   7   8   9   0   1   2   3   4   5   6   7   8   9   0   1   2
              "inschrtijd",csvconst,"plaatsing",csvconst,"uitgesloten",csvconst, "ronde",csvconst,
              "club1",csvconst,"bondsnr1",csvconst,"manvr1",csvconst,"naam1",csvconst,
              "spsterkte1",csvconst,"rating1",csvconst,"geboortedatum1",csvconst,"uitkantoor1",csvconst, "verhinderingen1",
              "club2",csvconst,"bondsnr2",csvconst,"manvr2",csvconst,"naam2",csvconst,
              "spsterkte2",csvconst,"rating2",csvconst,"geboortedatum2",csvconst,"uitkantoor2",csvconst, "verhinderingen2"),*/
  writedevice(X),
  !.
rep3(VlgNr, Cat, p(SpN1,SpN2)) :-
  dt_minoffset_to_str(VlgNr, "%DD/%MD %HH:%MM", VlgS), 
  opg(_,Cat,p(SpN1,SpN2),Plts,Rnd,Uits,_,_,_,_,_),
  sp2(SpN1,Srt1,Naam1,_,_,_,RatingD1,_,UitKant1, Club1, _, _, KNLTB1, _, StDbl1, Gebdat1,_,_,_, _,_,_,_,_,_,_,_),
  sp2(SpN2,Srt2,Naam2,_,_,_,RatingD2,_,UitKant2, Club2, _, _, KNLTB2, _, StDbl2, Gebdat2,_,_,_, _,_,_,_,_,_,_,_),
  spsrt(Srt1, _, MVs1, _, _, _, _, _),
  get_mv(MVs1, MV1),
  spsrt(Srt2, _, MVs2, _, _, _, _, _),
  get_mv(MVs2, MV2),
  get_plts(Plts, Plaats, PlaatsCSS),
  get_uitsluit(Uits, Uitsl, PlaatsCSS, UitslCSS), trim_c(Uitsl),
  get_ronde(Rnd, Ronde),
  tijd_str(Uitkant1, UStr1,_),
  tijd_str(Uitkant2, UStr2,_),
  isLeeg(Club1, Club11), isLeeg(KNLTB1, KNLTB11),isLeeg(StDbl1, StDbl11), isLeeg(Plaats, Plaats1),  isLeeg(GebDat1, GebDat11), isLeeg(Uitsl, Uitsl1), isLeeg(Ronde, Ronde1),
  isLeeg(Club2, Club21), isLeeg(KNLTB2, KNLTB21),isLeeg(StDbl2, StDbl21), isLeeg(GebDat2, GebDat21),isLeeg(RatingD1,RatingD11),isLeeg(RatingD2,RatingD21),
  writef("\n<tr %s><td class='middle'>%</td><td class='middle'>%<br>%</td><td class='middle'>%<br>%</td><td class='middle'>%c<br>%c</td>",
       UitslCSS, VlgS, Club11,Club21,KNLTB11,KNLTB21, MV1,MV2),
  writef("<td>%<br>%</td><td class='middle'>%<br>%</td><td class='middle'>%<br>%</td><td class='middle'>%</td><td class='middle'>%<br>%</td><td class='middle'>%</td><td class='middle'>%</td></tr>", 
       Naam1, Naam2,StDbl11, StDbl21,RatingD11,RatingD21,Plaats1, GebDat11, GebDat21, Uitsl1, Ronde1),
  writedevice(X),
  writedevice(nawf),
  writef("\n\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%c\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%c\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"",
          /*   1       2       3       4       5       6       7       8       9       0       1       2       3       4       5       6       7       8       9       0       1       2  */
              VlgS,csvconst,Plaats,csvconst,Uitsl,csvconst,Ronde,csvconst,
              Club1,csvconst,KNLTB1,csvconst,MV1,csvconst,Naam1,csvconst,
              StDbl1,csvconst,RatingD1,csvconst,GebDat1,csvconst,UStr1,csvconst,"",csvconst,
              Club2,csvconst,KNLTB2,csvconst,MV2,csvconst,Naam2,csvconst,
              StDbl2,csvconst,RatingD2,csvconst,GebDat2,csvconst,UStr2,csvconst,""),
  writedevice(X),
  !.
rep3(VlgNr, Cat, pw(SpN1)) :-
  dt_minoffset_to_str(VlgNr, "%DD/%MD %HH:%MM", VlgS), 
  opg(_,Cat,pw(SpN1),Plts,Rnd,Uits,_,_,_,_,_),
  sp2(SpN1,Srt1,Naam1,_,_,_,RatingD1,_,UitKant1, Club1, _, _, KNLTB1, _, StDbl1, Gebdat1,_,_,_, _,_,_,_,_,_,_,_),
  spsrt(Srt1, _, MVs1, _, _, _, _, _),
  get_mv(MVs1, MV1),
  get_plts(Plts, Plaats, PlaatsCSS),
  get_uitsluit(Uits, Uitsl, PlaatsCSS, UitslCSS), trim_c(Uitsl),
  get_ronde(Rnd, Ronde),
  tijd_str(Uitkant1, UStr1,_),
  isLeeg(Club1, Club11), isLeeg(KNLTB1, KNLTB11),isLeeg(StDbl1, StDbl11), isLeeg(Plaats, Plaats1),  isLeeg(GebDat1, GebDat11), isLeeg(Uitsl, Uitsl1), isLeeg(Ronde, Ronde1),isLeeg(RatingD1, RatingD11),
  writef("\n<tr %s><td class='middle'>%u</td><td>%</td><td class='middle'>%</td><td>%c</td><td>%</td><td class='middle'>%</td><td class='middle'>%</td><td>%</td><td>%</td><td class='middle'>%</td><td class='middle'>%</td></tr>", 
      UitslCSS, VlgS, Club11,KNLTB11, MV1, Naam1, StDbl11, RatingD11, Plaats1, GebDat11, Uitsl1, Ronde1),
  writedevice(X),
  writedevice(nawf),
  writef("\n\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%c\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%c\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"",
          /*   1       2       3       4       5       6       7       8       9       0       1       2       3       4       5       6       7       8       9       0       1       2  */
              VlgS,csvconst,Plaats,csvconst,Uitsl,csvconst,Ronde,csvconst,
              Club1,csvconst,KNLTB1,csvconst,MV1,csvconst,Naam1,csvconst,
              StDbl1,csvconst,RatingD1,csvconst,GebDat1,csvconst,UStr1,csvconst,"",csvconst,
              "",csvconst,"",csvconst,"",csvconst,"",csvconst,
              "",csvconst,"",csvconst,"",csvconst,"",csvconst,""),
  writedevice(X),
  !.
%st_list(["1","2","3","4","5","6","7","8","9","?"]).

in_weds1(_Cat, _Tgnst, 2) :- !.	% gewoon alle opgaven
in_weds1(Cat, Tgnst, 1) :-
  wd(_,Cat,Tgnst,_,_,_,_), !.
in_weds1(Cat, Tgnst, 1) :-
  wd(_,Cat,_, Tgnst,_,_,_), !.
in_weds1(Cat, Tgnst, 0) :-
  opg(_,Cat,Tgnst,_,_,0,_,_,_,_,_), !.

get_mv(m, 'M').
get_mv(v, 'V').

volgens_sterkte(OpgaveO) :-
  findall(Stx, opg_data(_, Stx, _Plts1, _Naam, _, _), StxL),
  sortstring_c(StxL, 1),
  removeDubl(StxL, [], StList),
  %st_list(StList),
  member(St, StList),
  getbacktrack(Here),
  numb(Plts),			% volgens sterkte EN plaatsing!
  check_numb(Here, Plts, Plts1),  
  findall(Naam,opg_data(_, St, Plts1, Naam, _, _),NamenL),
  not(NamenL = []),
  sortstring_c(NamenL,1),
  member(NaamS, NamenL),	% en dan nog alfabetisch
  opg_data(OpgaveO, St, Plts1, NaamS, _, _).
  
check_numb(_, Plts, Plts) :-	% ga door met plaatsen totdat er geen
  opg_data(_, _, P,_, _, _),		%  grotere meer is.
  P >= Plts, !.
check_numb(_, _, 0).		% doe dan alle ongeplaatsten (= 0)
check_numb(Here, _, _) :-	% en dan fail.
  cutbacktrack(Here),
  fail.  

get_plts(0, "", "") :- !.
get_plts(P, Pl, "class='plaats'") :-
  str_int(Pl, P).

get_uitsluit(0, "  ", In, In) :- !.
get_uitsluit(_, "Ja", _, "class='uitsl'").

get_ronde(0, "") :- !.
get_ronde(X, XS) :-
  str_int(XS, X).
     
positionWC(CatNo, [CatNo|Cat1L], [CatNo|Cat1L]) :- !.
positionWC(CatNo, [_|Cat1L], Cat1L1) :- !,
  positionWC(CatNo, Cat1L, Cat1L1).
positionWC(_, _, []) :- !.
  
evenWegWC([Cat|Cat1L],[wc(Cat, OnderdeeelID, N, Naam, EDG, Kort, SexL, SpStrk, LftV, LftTM, HDG, Schemasoort, Keuring, KNLTB,Geld)|Cat1LVol]) :-
  wc(Cat, OnderdeeelID, N, Naam, EDG, Kort, SexL, SpStrk, LftV, LftTM, HDG, Schemasoort, Keuring, KNLTB,Geld), !,
  logf('R',wc(Cat, OnderdeeelID, N, Naam, EDG, Kort, SexL, SpStrk, LftV, LftTM, HDG, Schemasoort, Keuring, KNLTB,Geld),ja),  % voor remote belangrijk!
  evenWegWC(Cat1L, Cat1LVol).  
evenWegWC(_, []) :- !.

sluitAanWC([wc(Cat, OnderdeeelID, N, Naam, EDG, Kort, SexL, SpStrk, LftV, LftTM, HDG, Schemasoort, Keuring, KNLTB,Geld)|Cat1L1Vol]) :-
  logf('Z', wc(Cat, OnderdeeelID, N, Naam, EDG, Kort, SexL, SpStrk, LftV, LftTM, HDG, Schemasoort, Keuring, KNLTB,Geld),ja), !,
  sluitAanWC(Cat1L1Vol).
sluitAanWC(_) :-
  logclose().


%BEGIN_DLG Splitsen
/**************************************************************************
	Creation and event handling for dialog: Splitsen
**************************************************************************/


constants

%BEGIN Splitsen, CreateParms, 10:03:42-15.10.2015, Code automatically updated!
  dlg_splitsen_DlgType = wd_Modal
  dlg_splitsen_Title = "Splitsen"
  dlg_splitsen_RCT = rct(40,32,228,306)
  dlg_splitsen_Flags = [wsf_Close,wsf_TitleBar]
  dlg_splitsen_Help = idh_splitsencombineren
  dlg_splitsen_Font = "Arial"
  dlg_splitsen_FSize = 10
%END Splitsen, CreateParms

database - splitsen
determ  splcat(wedscat)
%determ splcatuit(wedscat)
single splits(DIALOG_CONTROL_ID , integer Plaatsing , integer Verhinderingen, integer Clubs,  DIALOG_CONTROL_ID VoorDelen, DIALOG_CONTROL_ID Eindronde)
catId(string, wedscat)
determ meldinggedaan

predicates

  dlg_splitsen_eh : EHANDLER
  dlg_splitsen_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_splitsen_update(DIALOG_VAL_LIST)
  int_dialogInt(integer, DIALOG_INT)
  procedure makeTekst(WINDOW, integer)
  genereerWC(integer Current, integer Total, dbasedom Master, verhl, slist Leeg, slist OutNaamList, catl OokLeeg, catl  OutCatList) - (i, i, i,i,i,o,i,o)
  blokTijdRetract(wedscat, verhl)
  blokTijdAssert(wedscat, verhl)
nondeterm  baan_dag(integer)
nondeterm  opgeg1(wedscat, integerlist, rec, integer IsVolgensRating, integer, integer)  - (i,i,o,i,i,i)
  in_club(tgnst,string,integer) - (i,o,i)
  tgnst_verh(tgnst, tijd, verhl, integer) - (i,o,o,i)
nondeterm  niet_verh(tijd, integerlist, tijd, verhl)
  verhinderd(tijd,tijd,verhl)
  verhuisOpg(wedscat, catl, rlist)
  moveUitsl(wedscat Cat, wedscat CatVrij)
  maakKorteUniek(string ShortS, string ShortS)
  maakLangeUniek(string, string)
  extraClean(wedscat, wedscat)
%procedure pouleLetter(integer N, string LetterS)  
procedure presetVerder(window, DIALOG_CONTROL_ID, wedscat)
procedure aantalsecties(integer Opgaven, integer Secties)
procedure passendeEindronde(window, wedscat CatNo, integer EINDRONDEVORM, catl, slist, string EindrondeNaam, string Short, verhl Bloktijden) - (i,i,i,i,i,i,i,i)
procedure ranglijstOptieToevoegen(wedscat, integer)
checkAantalGeplaatsten(integer Secties, integer AantalGeplaatsten)
nondeterm geplaatst(wedscat Cat, integer N)
procedure cleanOudeKoppelingen(wedscat)
%nondeterm ladderkoppel1(string, integer, string, integer)
%procedure ladderkoppel(wedscat Header, catl EersteRonde, catl TweedeRonde, catl DerdeRonde)
%determ checkPoulePoule(integer Vorm, integer AantalOpgaven, integer AantalGeplaatsten, integer IsPoulePoule)
nondeterm opgListRating(wedscat Cat, string SorteerVeld)
procedure uitgeslotentekst(integer, string)
procedure integer toGetal(integer)
procedure presetSplitsOpties(integer RatingOfAnders, boolean _IDCS_PLAATSING_CHECKED, boolean _IDCS_VERHINDERINGEN_CHECKED, boolean _IDCS_CLUB_CHECKED, 
                  boolean PLAATSING_CHECKED, boolean VERHINDERINGEN_CHECKED, boolean CLUB_CHECKED) - (i,i,i,i,o,o,o)
procedure rekeningMetPreset(window, boolean)
procedure uitgelotentekst(integer, string)
%procedure eerstekeerPouleladder()
nondeterm namen(catl CatNList, verhl BlokL, string Naam)
procedure chop(catl, integer, integer, catl)
procedure  vrijeCatNummers1(integer, wedscat, wedscat)
%procedure writeTest(rlist)


clauses

splits(idcs_anders, b_true, b_true, b_true, idcs_poules, idcs_eindronde).

uitgeslotentekst(0, "") :- !.
uitgeslotentekst(N, Tekst) :-
  format(Tekst, "het aantal uitgesloten   = %u\n", N).

makeTekst(_Win, K) :-
  K > 0,
  splcat(CatNo), 
  findall(OpgaveNum, opg(OpgaveNum,CatNo,_,_,_,0,_,_,_,_,_),OpgL),
  findall(OpgaveNmU, opg(OpgaveNmU,CatNo,_,_,_,1,_,_,_,_,_),OpgLU),
  count(OpgLU, 0, AantU),
  count(OpgL, 0, Aant),
  Deel = Aant div K,
  Deel > 1, !,
  Rest = Aant mod K,
  K1 = K - Rest,
  D1 = Deel + 1,
  PouleAant = K1 * (Deel * (Deel - 1) div 2) + Rest * (D1 * (D1 - 1) div 2),
  AfvalAant = K1 * (Deel - 1) + Rest * (D1 - 1),
  uitgeslotentekst(AantU, Utekst),
  format(Temp,"het aantal zo ontstane partijen bij\n  poules       = %\n  afvalschemas = %u", PouleAant,AfvalAant),
  format(Tekst, "het netto aantal opgaven = %\n%s==> % delen van % en % delen van %\n%",Aant,Utekst,K1, Deel, Rest, D1, Temp),
  TWin = win_GetCtlHandle(_Win, idcts_tell),
  win_SetText(TWin,Tekst),
  dialog_SetState(_Win, [enable(idc_ok, b_True)]).
makeTekst(_Win, _K) :-
  splcat(CatNo), !,
  findall(OpgaveNum, opg(OpgaveNum,CatNo,_,_,_,0,_,_,_,_,_),OpgL),
  findall(OpgaveNmU, opg(OpgaveNmU,CatNo,_,_,_,1,_,_,_,_,_),OpgLU),
  count(OpgL, 0, Aant),
  count(OpgLU, 0, AantU),
  uitgelotentekst(AantU, UStr),
  format(Tekst, "het aantal opgaven = %u%s\n\naantal delen?",Aant, UStr),
  TWin = win_GetCtlHandle(_Win, idcts_tell),
  win_SetText(TWin,Tekst),
  dialog_SetState(_Win, [enable(idc_ok, b_False)]).
makeTekst(_Win, _K).

uitgelotentekst(0,"") :- !.
uitgelotentekst(AantU, UStr) :-
  format(UStr, "\nhet aantal uitge(s)lote(n) = % (wordt niet meegenomen)", AantU).

int_dialogInt(0, void) :- !.
int_dialogInt(X,i(X))  :- !.

ruimOpWC(CatNo) :-
  wd(No, CatNo, T1, T2, Uitsl, T, Nota), !,
  logf('T',  wd(No, CatNo, T1, T2, Uitsl, T, Nota), ja).
ruimOpWC(_).

maakLangeUniek(CatS, CatS) :-
  not(wc(_, _, _, CatS, _, _, _, _, _, _, _, _, _, _, _)), !.
  %not(wc(_,CatS,_,_,_,_,_)), !.		% is uniek
maakLangeUniek(CatS1, CatS2) :-
  concat(CatS1, "*", CatUit),
  beep(),
  maakLangeUniek(CatUit, CatS2).

maakKorteUniek(ShortS, ShortS) :-
  not(wc(_, _, _, _, _, ShortS, _, _, _, _, _, _, _, _, _)), !.	% is uniek
maakKorteUniek(ShortS1, ShortS2) :-
  concat(ShortS1, "*", ShortUit),
  beep(),
  maakKorteUniek(ShortUit, ShortS2).

pouleLetter(N, LetterS) :-
  N < 26,
  char_int('A', Basis),
  LetterI = Basis + N,
  char_int(Letter, LetterI),
  str_char(LetterS, Letter), !.
pouleLetter(N, LetterS) :-
  char_int('A', Basis),
  LetterI = Basis + N - 26,
  char_int(Letter, LetterI),
  format(LetterS, "%c%c",Letter,Letter), !.

namen(CatNList, BlokL, Naam) :-
  member(Cat, CatNList),
  %writedevice(X2),
  %writedevice(screen),
  %write("\nHierb ",Cat),
  %writedevice(X2),
  blokTijdAssert(Cat, Blokl),
  wc(Cat, _OnderdeeelID, _Nr, CatNaam, _Wedstyp, _Short, _SexL, _SpStrk, _LftV, _LftTM, _HDG, _SS, _Keuring, _KNLTB, _Geld),
  concat(" \t", CatNaam, Naam).
  %writedevice(X1),
  %writedevice(screen),
  %write("\nHiera"),
  %writedevice(X1).
  %cleanoudekoppelingen(Cat).   ....

chop([H|T], In, Aantal, [H|Uit]) :-
  In < Aantal, !,
  In1 = In + 1,
  chop(T, In1, Aantal, Uit).
chop(_, _, _, []).

koplInVolgorde(CatMaster, CatN) :-
   wc(CatN, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
   kopl(CatN, algemeen, CatMaster, algemeen).

genereerWC(N, N, _, _, CatSL, CatSL, CatL, CatL) :-
  logclose(), !.
  %str_int(NS,N),
  %dlg_note("test genereerwc0",NS),  !.
genereerWC(_N, TeSplitsenAantal, Master, BlokL, _, CatNaamList, _, CatNList) :-
  Master = wc(CatMaster, _OnderdeeelID, _Nr, _CatS, _Wedstyp, _Short, _SexL, _SpStrk, _LftV, _LftTM, _HDG, "0", _Keuring, _KNLTB, _Geld),  % AR ServIT2 
  %term_str(dbasedom, Master, MasterS),
  %dlg_note("testwc",MasterS),
  isMasterCat(CatMaster),  %@@@@@@@@@@@@@	% 29.5.2015
  getbacktrack(Here),
  kopl(_, _, CatMaster, _), % ze moeten er onder hangen
  cutbacktrack(Here),
  %findall(CatN, kopl(CatN,algemeen, CatMaster,algemeen),CatNListI),
  findall(CatN, koplInVolgorde(CatMaster, CatN),CatNListI),
  chop(CatNListI, 0, TeSplitsenAantal, CatNList),
  findall(CatNaam, namen(CatNList, BlokL, CatNaam), CatNaamList),
  writedevice(X),
  writedevice(screen),
  write("\nCatNListI(genereerWC): ",CatNListI, " TesplitsenAantal: ", TesplitsenAantal, " Chop: ",CatNList, " CatnaamList:", CatNaamList),
  writedevice(X), !.
genereerWC(N, K, Master, BlokL, CatSLIn, [CatS1A|CatSLUit], CatLIn, [CatNo|CatLUit]) :-
  N1 = N + 1,
  Master = wc(CatVrij, _OnderdeeelID, Nr, CatS, Wedstyp, Short, SexL, SpStrk, LftV, LftTM, HDG, _Schemasoort, _Keuring, _KNLTB,Geld), % CatVrij = begin nieuwe serie
  CatNo = CatVrij + N,
  trim_c(CatS),
  pouleLetter(N, LetterS),
  format(CatS1, "%s-%s", LetterS, CatS),
  maakLangeUniek(CatS1, CatS2),
  trim_c(Short),
  format(ShortS1, "%s%s", Short, LetterS),
  maakKorteUniek(ShortS1, ShortS2),  !,
  cleanoudekoppelingen(CatNo),  %###############################
  Nr1 = Nr + N,
  %format(GenWc, "Catno: %u\n%s\n%s", CatNo, CatS2, ShortS2),
  %dlg_note("test genereerwc", Genwc),
  logf('Z', wc(CatNo, "", Nr1, CatS2, Wedstyp, ShortS2, SexL, SpStrk, LftV, LftTM, HDG, "", vanzelf, "",Geld),ja),
  blokTijdAssert(CatNo, BlokL),		% alleen als niet = []
  concat(" \t", CatS1, CatS1A),
  genereerWC(N1, K, Master, BlokL, CatSLIn, CatSLUit, CatLIn, CatLUit).

blokTijdRetract(CatOrig, BlokList) :-
  bl(CatOrig, BlokList), 
  logf('R', bl(CatOrig, BlokList),ja), !.
blokTijdRetract(_, []).

blokTijdAssert(_, []) :- !.
blokTijdAssert(CatNo, BlokList) :-
  logf('Z',bl(CatNo, BlokList),ja),!.

vrijeCatNummers(_K, CatNo, CatNo) :-
  %format(Msg, "\nonderdeel %u", CatNo),
  isMastercat(CatNo), 
  wc(CatNo, _, _, _, _, _, _, _, _, _, _, SchSrt, _, _, _),
  SchSrt = "0",
  %1 = dlg_MessageBox( "isMasterCat", Msg, mesbox_iconexclamation, mesbox_buttonsokcancel, mesbox_defaultsecond, mesbox_suspendapplication ),
  !.
vrijeCatNummers(K, CatNo, CatNoOut) :-
  vrijeCatNummers1(K, CatNo, CatNoOut).
  
vrijeCatNummers1(K, CatNo, CatNoOut) :-
  wc(Cat, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
  Cat >= CatNo,		% is er een cat die in die range ligt?
  Cat < CatNo + K, !,				% ja, cut ...
  CatNoIn = CatNo + 10,				% 10 verder
  vrijeCatNummers1(K, CatNoIn, CatNoOut).	% en zoek nieuwe range
vrijeCatNummers1(K, CatNo, CatNo) :-
  CatK = CatNo + K,
  extraClean(CatNo, CatK), !.
vrijeCatNummers1(_K, CatNo, CatNo).

extraClean(CatNo, CatNo) :- !.
extraClean(CatNo, _CatK) :-
  opg(H,CatNo,Tgnst,Plts,Byes,A,B,C,TeamId,Keur,Melding),
  logf('R',  opg(H,CatNo,Tgnst,Plts,Byes,A,B,C,TeamId,Keur,Melding), nee), 
  fail.
extraClean(CatNo, CatK) :-
  CatNo1 = CatNo + 1,
  extraClean(CatNo1, CatK).

baan_dag(Tijd) :-
  bn(Tijd, N),
  N <> 0,
  bitright(Tijd,7,Dag),
  dag(DNo,_,_),
  Dag = DNo.

%nondeterm  opgeg1(wedscat, integerlist, rec, integer IsVolgensRating, integer, integer, integer, integer)  - (i,i,o,i,i,i,i,i)

opgeg1(Cat, Tijden, rec(H, Plts, Byes, RandomInt, Club, Tijden_niet_Verh,0), VolgensVerh, VolgensClub, 0) :-
  opg(H,Cat,Tgnst,Plts,Byes,0,_,_,_,_,_), 
  in_club(Tgnst,Club,VolgensClub), 
  tgnst_verh(Tgnst, Begin, Verh, VolgensVerh),
  findall(Tijd, niet_verh(Tijd, Tijden, Begin, Verh), Tijden_niet_Verh),
  random(3000,RandomInt).
opgeg1(Cat, _Tijden, rec(No, 0, 0, RandomInt, Rating, [], 0), _VolgensVerhVerh, _VolgensClub, 1) :-
  findall(Sorteerveld, opgListRating(Cat, Sorteerveld), SorteerList),
  sortstring_c(Sorteerlist, 1),
  member(Item, SorteerList),
  parsetabs(Item, Parts),
  Parts = [Rating, _Cat, H | _],
  str_int(H, No),
  random(3000,RandomInt).

opgListRating(Cat, SorteerVeld) :-
%  opg(integer,wedscat,tgnst,integer,integer,integer,integer, string)
  opg(No,Cat,s(SpNo),_Plts,_Ronde,_Uitsl,_,_,_,_,_),
  sp2(SpNo,_,Naam,_,_,RE1,_RD,_,_, _, _, _, _, ES1, _SPSt, _, _, _, _, _,_,_RangPosE1,_RangPosD1,_,_CheckGeg1,_Log1,_Res1), 
  noodrating(RE1, ES1, RE),
  formatRating(RE, RE4),
  format(SorteerVeld, "%-8s%s\t%u\t%U", RE4, Naam, Cat, No).
opgListRating(Cat, SorteerVeld) :-
  opg(No,Cat,pw(SpNo),_Plts,_Ronde,_Uitsl,_,_,_,_,_),
  sp2(SpNo,_,Naam,_,_,RE1,_RD,_,_, _, _, _, _, ES1, _SPSt, _, _, _, _, _,_,_RangPosE1,_RangPosD1,_,_CheckGeg1,_Log1,_Res1), 
  noodrating(RE1, ES1, RE),
  formatRating(RE, RE4),
  format(SorteerVeld, "%-8s%s\t%u\t%U", RE4, Naam, Cat, No).
opgListRating(Cat, SorteerVeld) :-
  opg(No,Cat,p(SpNo,Sp1),_Plts,_Ronde,_Uitsl,_,_,_,_,_),
  sp2(SpNo,_,Naam,_,_,_RE1,RD1,_,_, _, _, _, _, _ES1, SPSt, _, _, _, _, _,_,_RangPosE1,_RangPosD1,_,_CheckGeg1,_Log1,_Res1), 
  sp2(Sp1,_,_N,_,_,_RE21,RD2,_,_, _, _, _, _, _ES2, SPSt2, _, _, _, _, _,_,_RangPosE2,_RangPosD2,_,_CheckGeg2,_Log2,_Res2), 
  noodrating(RD1,SpSt,RD1x),
  noodrating(RD2,SpSt2,RD2x),
  str_real_Rating(RD1x, R1),
  str_real_Rating(RD2x, R2),
  dubbelRating(R1, R2, RD, _, _,_),
  formatrating(RD, RD4),
  format(SorteerVeld, "%-8s%s\t%u\t%U", RD4, Naam, Cat, No).

%uitgesloten(Cat, H) :-
%  opg(H,Cat,_Tgnst,_Plts,_Byes,U,_,_,_,_,_),
%  U > 0. 

in_club(_, "", _) :-
  not(openbaar(ja)), !.
in_club(_, "", 0) :- !.
  %not(member(2, Fact)), !.
in_club(s(SpNo),ClubNummer,_) :-
  sp2(SpNo,_,_,_,_Verh1,_,_,_,_Beg1,ClubNummer,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !.
in_club(pw(SpNo),ClubNummer,_) :-
  sp2(SpNo,_,_,_,_Verh1,_,_,_,_Beg1,ClubNummer,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !.
in_club(p(SpNo,_),ClubNummer,_) :-
  sp2(SpNo,_,_,_,_Verh1,_,_,_,_Beg1,ClubNummer,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !.
in_club(_,"",_).

tgnst_verh(_, 0, [], 0) :- !.
  %not(member(1,Fact)), !.
tgnst_verh(s(No), Begin, Verh, _) :-
  sp2(No,_,_,_,Verh,_,_,_,Begin,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !.
tgnst_verh(pw(No), Begin, Verh, _) :-
  sp2(No,_,_,_,Verh,_,_,_,Begin,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !.
tgnst_verh(p(No1,No2), Begin, Verh, _) :-
  sp2(No1,_,_,_,Verh1,_,_,_,Beg1,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  sp2(No2,_,_,_,Verh2,_,_,_,Beg2,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !,
  append(Verh1,Verh2,Verh),
  grootste(Beg1, Beg2, Begin).

niet_verh(Tijd, Tijden, Begin, Verh) :-
  member(Tijd, Tijden),
  not(verhinderd(Tijd, Begin, Verh)).

verhinderd(Tijd, Begin, _) :-
  bitright(Tijd,7,Dag),
  dag(Dag, _, w),
  bitand(Tijd,$7F,T1), 
  T1 < Begin, !.
verhinderd(_,_,[]) :- !, fail.
verhinderd(Tijd,_,[per(T1,T2)|_]) :-
  Tijd > T1 - 8, 
  Tijd < T2, !.
verhinderd(Tijd,Beg,[_|Rest]) :-
  verhinderd(Tijd,Beg,Rest).  

verhuisOpg( _, _, []) :- !.
verhuisOpg( Cat, CatL, [rec(Num, _, _, Nx, _, _, _)|Rest]) :-  % Nx = Offset
  getNth(Nx, CatL, CatN),
  Cat <> CatN,
  opg(Num,Cat,Tgnst,Plts,Rnd,Uits,Res,LL,TeamId,Keur,Melding), !,
  logf('R',opg(Num,Cat,Tgnst,Plts,Rnd,Uits,Res,LL,TeamId,Keur,Melding),ja),
  logf('Z',opg(Num,CatN,Tgnst,Plts,Rnd,Uits,Res,LL,TeamId,Keur,Melding),ja),
  verhuisOpg(Cat, CatL, Rest).
verhuisOpg( Cat, CatL, [_|Rest]) :- !,
  verhuisOpg(Cat, CatL, Rest).
verhuisOpg(_,_,_).

moveUitsl(Cat, Cat) :- !.
moveUitsl(Cat, CatVrij) :-
  opg(Num,Cat,Tgnst,Plts,Rnd,Uits,Res,LL,TeamId,Keur,Melding),
  logf('R',opg(Num,Cat,Tgnst,Plts,Rnd,Uits,Res,LL,TeamId,Keur,Melding),ja),
  logf('Z',opg(Num,CatVrij,Tgnst,Plts,Rnd,Uits,Res,LL,TeamId,Keur,Melding),ja),
  fail.
moveUitsl(_, _).
  

geplaatst(Cat, N) :-
  opg(N,Cat,_,P,_,_,_,_,_,_,_),
  P > 0.
  
checkAantalGeplaatsten(2, 4) :- !.
checkAantalGeplaatsten(4, 8) :- !.
checkAantalGeplaatsten(S, X) :-
  G = S * 2,
  format(Msg, "NRt. Aanbevolen:\n Aantal geplaatsten: %u\n Aantal secties: %u\n Huidige aantal geplaatsten: %u.", G, S, X),
  dlg_MessageBox( "Geplaatsten in kwalificatie", Msg, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  !.

aantalsecties(Aantal, 1) :-
  Aantal <= 8, !.
aantalsecties(Aantal, 2) :-
  Aantal <= 16, !.
aantalsecties(_, 4).

/*presetVerder(_Win, _, _Cat) :-
  comline(LineBuffer),
  upper_lower(LineBuffer, LBL),
  not(searchstring(LBL, "/x", _)),
  dialog_SetState(_Win,[enable(idcs_pouleladder,b_False)]),
  fail.*/
rekeningMetPreset(Win, Checked) :-
  dialog_SetState(Win,[enable(idcs_plaatsing,Checked), enable(idcs_verhinderingen, Checked), enable(idcs_club, Checked)]).

presetVerder(_Win, _, Cat) :-
  isKwalificatie(Cat, _),
  dialog_SetRadioButton(_Win, idcs_afvalsysteem, idcs_afvalsysteem),
  dialog_SetRadioButton(_Win, idcs_rating, idcs_anders),
  dialog_SetCheck(_Win, idcs_plaatsing, b_True),
  dialog_SetCheck(_Win, idcs_verhinderingen, b_False),
  dialog_SetCheck(_Win, idcs_club, b_False),
  dialog_SetRadioButton(_Win, idcs_eindronde, idcs_geen_eindronde),
  %dialog_SetState(_Win,[enable(idcs_eindronde_aanmaken,b_False)]),
  findall(OpgaveNumx, opg(OpgaveNumx,Cat,_,_,_,_,_,_,_,_,_),OpgLx),
  count(OpgLx, 0, Aantx),
  aantalsecties(Aantx, Secties),
  %Secties = Aantx div 8,
  str_int(Getal, Secties),
  AWin = win_GetCtlHandle(_Win, idcs_aantal),
  win_SetText(AWin, Getal), 
  findall(N, geplaatst(Cat, N), Nlist),
  count(NList, 0, AantG),
  checkAantalGeplaatsten(Secties, AantG),
  !.
presetVerder(_Win, _, _Cat) :-
  splits(RATINGOFANDERS, _IDCS_PLAATSING_CHECKED, _IDCS_VERHINDERINGEN_CHECKED, _IDCS_CLUB_CHECKED, _IDCS_VORM, _EINDRONDEVORM),
  Set = toGetal(RatingOfAnders),  % RatingOfAnders = idcs_rating of idcs_anders
  bitxor(Set,1, NotSet),
  rekeningMetPreset(_Win, NotSet),  % alleen om andere opties te enabelen (plaatsing/verhindering/clubs)
  fail.
presetVerder(_Win, _, Cat) :-
  findall(N, geplaatst(Cat, N), Nlist),
  count(NList, 0, AantG),
  AantG > 0,
  dialog_SetRadioButton(_Win, idcs_rating, idcs_anders),
  dialog_SetCheck(_Win, idcs_plaatsing, b_True),
  fail.
presetVerder(_Win, idcs_niet, _) :-
  dialog_SetRadioButton(_Win,idcs_eindronde, idcs_verzameleindronde), !.
presetVerder(_Win, _, _Cat).

passendeEindronde(Win, CatNo, idcs_eindronde, _, NewCatsL, Eindrondenaam, _, _) :-
  %TaskWin = vpi_GetTaskWin(),
  dlg_koppelingmodel_Create(Win,CatNo,Eindrondenaam, NewCatsL,[1],_ReturnCode),	% [1] = met preselect
  !.
passendeEindronde(_, CatNo, idcs_geen_eindronde, NewCatL, _, _, _, _) :-
  kopl(CatNo, _, CatHoofd, _),
  member(Cat, NewCatL),
  not(kopl(Cat, algemeen, CatHoofd, algemeen)),
  logf('Z', kopl(Cat, algemeen, CatHoofd, algemeen), ja),
  fail. 
passendeEindronde(_, CatNo, idcs_geen_eindronde, _, _, _, _, _) :-
  ruimopWc(CatNo),
  wc(CatNo, OnderdeeelID, N, Naam, EDG, Kort, SexL, SpStrk, LftV, LftTM, HDG, Schemasoort, Keuring, KNLTB,Geld),
  logf('R',wc(CatNo, OnderdeeelID, N, Naam, EDG, Kort, SexL, SpStrk, LftV, LftTM, HDG, Schemasoort, Keuring, KNLTB,Geld),ja),
  fail.
/*passendeEindronde(CatNo, idcs_verzameleindronde, _NewCatL, _, _, _, _) :-	% 11.3.2011= dummy
  kopl(CatNo, X, CatHoofd, Y),
  logf('R', kopl(CatNo, X, CatHoofd, Y), ja),
  fail.*/ 
passendeEindronde(_, CatNo, idcs_verzameleindronde, NewCatL, _, _, _, _) :-
  member(Cat, NewCatL),	% 10.3.2011 verwijder voor de zekerheid?
  not(kopl(Cat, algemeen, CatNo, algemeen)),
  logf('Z', kopl(Cat, algemeen, CatNo, algemeen), ja),
  fail. 
passendeEindronde(_, _, _, _, _, _, _, _) :-
  logclose().

ranglijstOptieToevoegen(Cat, 1) :-
  isNrt(Cat), !.
ranglijstOptieToevoegen(Cat, 1) :-
  kopl(Cat, _, BCat, _),
  isNrt(BCat), !.
ranglijstOptieToevoegen(_, 0).


cleanOudeKoppelingen(Cat) :-
  writedevice(X),
  writedevice(screen),
  write("Cleanoudekoppelingen: ",Cat,"\n"),
  writedevice(X),
  kopl(Cat, Van, CatU, IPV),
  logf('R', kopl(Cat, Van, CatU, IPV), nee),
  fail.
cleanOudeKoppelingen(CatU) :-
  kopl(Cat, Van, CatU, IPV),
  logf('R', kopl(Cat, Van, CatU, IPV), nee),
  fail.
cleanOudeKoppelingen(_).
/*
writeTest(_) :-
  comline(LineBuffer),
  upper_lower(LineBuffer, LBL),
  not(searchstring(LBL, "/x", _)), !.
writeTest([H|T]) :-
  H = rec(XH, Plts, _Byes, RandomInt, _Club, _Tijden_niet_Verh, Rating),
  write("\nXH:", XH, " Plts:", Plts, " RandomInt/CatDelta: ", RandomInt, "   ", _Club, "   ", Rating), !,
  writeTest(T).
writeTest(_) :- 
  write("\n###\n").
*/


toGetal(idcs_rating, 1) :- !.
toGetal(_, 0).

presetSplitsOpties(1, _IDCS_PLAATSING_CHECKED, _IDCS_VERHINDERINGEN_CHECKED, _IDCS_CLUB_CHECKED, 0, 0, 0) :- !.
presetSplitsOpties(_, PLAATSING_CHECKED, VERHINDERINGEN_CHECKED, CLUB_CHECKED, PLAATSING_CHECKED, VERHINDERINGEN_CHECKED, CLUB_CHECKED).

  dlg_splitsen_Create(Parent,Cat,CatUit):-
	assert(splcat(Cat)),
	IDCTS_TELL_TITLE = "",
	splits(RATINGOFANDERS, IDCS_PLAATSING_CHECKED, IDCS_VERHINDERINGEN_CHECKED, IDCS_CLUB_CHECKED, IDCS_VORM, EINDRONDEVORM),

%MARK Splitsen, new variables

	dialog_Create(Parent,
		[
%BEGIN Splitsen, WinDefList, 10:03:42-15.10.2015, Code automatically updated!
		 dlg_font(wdef(dlg_splitsen_DlgType,dlg_splitsen_RCT,dlg_splitsen_Title,u_DlgBase),
		 	  dlg_splitsen_Font,dlg_splitsen_FSize,dlg_splitsen_Flags),
		 ctl(wdef(wc_RadioButton,rct(6,9,54,19),"Loting",u_DlgBase),idcs_anders,[wsf_Auto,wsf_TabStop,wsf_Group]),
		 ctl(wdef(wc_CheckBox,rct(11,18,59,28),"&Plaatsing",u_DlgBase),idcs_plaatsing,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_CheckBox,rct(11,28,72,38),"Verhinderingen",u_DlgBase),idcs_verhinderingen,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_CheckBox,rct(11,38,59,48),"Clubs",u_DlgBase),idcs_club,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_RadioButton,rct(6,55,90,65),"&Rating of Speelsterkte",u_DlgBase),idcs_rating,[wsf_Auto,wsf_TabStop]),
		 ctl(wdef(wc_Edit,rct(87,129,102,140),"",u_DlgBase),idcs_aantal,[wsf_Group,wsf_TabStop,wsf_AutoHScroll,wsf_AlignLeft]),
		 ctl(wdef(wc_RadioButton,rct(7,197,79,207),"&Niet     (o.a. AR)",u_DlgBase),idcs_niet,[wsf_Auto,wsf_TabStop]),
		 ctl(wdef(wc_RadioButton,rct(6,175,78,185),"&Poules (o.a. jeugd)",u_DlgBase),idcs_poules,[wsf_Auto,wsf_TabStop]),
		 ctl(wdef(wc_RadioButton,rct(6,153,68,163),"&Afvalsystemen",u_DlgBase),idcs_afvalsysteem,[wsf_Auto,wsf_TabStop,wsf_Group]),
		 ctl(wdef(wc_RadioButton,rct(88,153,168,163),"Eindronde aanmaken",u_DlgBase),idcs_eindronde,[wsf_Auto,wsf_TabStop,wsf_Group]),
		 ctl(wdef(wc_RadioButton,rct(88,175,181,185),"Leeg (alleen groeperen)",u_DlgBase),idcs_verzameleindronde,[wsf_Auto,wsf_TabStop]),
		 ctl(wdef(wc_RadioButton,rct(88,197,179,207),"Geen eindronde",u_DlgBase),idcs_geen_eindronde,[wsf_Auto,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(143,238,186,268),"&OK",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(108,257,140,267),"&Cancel",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(108,238,140,255),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_Text,rct(4,69,184,128),"",u_DlgBase),idcts_tell,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(6,130,86,139),"Gewenste aantal delen?",u_DlgBase),idcts_aantal_delen,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(3,230,105,271),"NB Splitsen kun je ongedaan maken met \"Combineer\". Neem dan de eindronde als verzamelpunt.",u_DlgBase),idcts_xx,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(104,185,177,195),"(o.a. AR)",u_DlgBase),idcts_oa_ar,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(105,163,153,173),"(o.a. jeugd)",u_DlgBase),idcts_oa_jrt,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(101,206,164,216),"(o.a. NRt kw..)",u_DlgBase),idcs_splitsen_nrtkw1,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(15,162,78,172),"(o.a. NRt kw..)",u_DlgBase),idcs_splitsen_nrtkw,[wsf_AlignLeft]),
		 ctl(wdef(wc_GroupBox,rct(4,1,95,69),"Rekening houden met",u_DlgBase),idcs_rekening_houden_met,[]),
		 ctl(wdef(wc_GroupBox,rct(3,143,82,228),"Delen vast indelen?",u_DlgBase),idcs_delen_vast_indelen,[]),
		 ctl(wdef(wc_GroupBox,rct(83,143,185,228),"Eindronde?",u_DlgBase),idcs_eindrondebox,[])
%END Splitsen, WinDefList
		],
  		[
%BEGIN Splitsen, ControlList, 10:03:42-15.10.2015, Code automatically updated!
		df(idcs_plaatsing,checkbox(IDCS_PLAATSING_CHECKED),nopr),
		df(idcs_verhinderingen,checkbox(IDCS_VERHINDERINGEN_CHECKED),nopr),
		df(idcs_club,checkbox(IDCS_CLUB_CHECKED),nopr),
		df(idcs_aantal,editint(void,[mandatory,minimum(2)]),dlg_prompt(idcts_aantal_delen)),
		df(idcts_tell,statictext(IDCTS_TELL_TITLE),nopr),
		df(IDCS_VORM,radiobuttongroup([idcs_afvalsysteem,idcs_niet,idcs_poules]),nopr),
		df(EINDRONDEVORM,radiobuttongroup([idcs_eindronde,idcs_geen_eindronde,idcs_verzameleindronde]),nopr),
		df(RATINGOFANDERS,radiobuttongroup([idcs_rating,idcs_anders]),nopr)
%END Splitsen, ControlList
		],
		dlg_splitsen_eh,0,VALLIST,ANSWER),
	dlg_splitsen_handle_answer(ANSWER,VALLIST),
	retract(splcat(CatUit)).

  dlg_splitsen_handle_answer(idc_ok,_VALLIST):-!,
	dlg_splitsen_update(_VALLIST).
  dlg_splitsen_handle_answer(idc_cancel,_):-!.  % Handle Esc and Cancel here
  dlg_splitsen_handle_answer(_,_):-
	errorexit().

  dlg_splitsen_update(_VALLIST):-
%BEGIN Splitsen, Update controls, 10:03:42-15.10.2015, Code automatically updated!
	_IDCS_AANTAL_VALUE = dialog_VLGetint(idcs_aantal,_VALLIST),
	_IDCS_PLAATSING_CHECKED = dialog_VLGetCheck(idcs_plaatsing,_VALLIST),
	_IDCS_VERHINDERINGEN_CHECKED = dialog_VLGetCheck(idcs_verhinderingen,_VALLIST),
	_IDCS_CLUB_CHECKED = dialog_VLGetCheck(idcs_club,_VALLIST),
	_IDCS_VORM = dialog_VLGetRadiobutton(idcs_afvalsysteem,_VALLIST),
	_EINDRONDEVORM = dialog_VLGetRadiobutton(idcs_eindronde,_VALLIST),
	_RATINGOFANDERS = dialog_VLGetRadiobutton(idcs_rating,_VALLIST),
%END Splitsen, Update controls
	!.

%MARK Splitsen, new events


%BEGIN Splitsen, idcs_rating _CtlInfo
  dlg_splitsen_eh(_Win,e_Control(idcs_rating,_CtrlType,_CtrlWin,_CtlInfo),0):-
	Checked = win_Ischecked(_CtrlWin),
	bitxor(Checked, 1, Ch),
	rekeningMetPreset(_Win, Ch),
	Checked = b_True,
	dialog_SetRadioButton(_Win,idcs_afvalsysteem, idcs_niet),
	dialog_SetRadioButton(_Win,idcs_eindronde, idcs_verzameleindronde), 
	!.
%END Splitsen, idcs_rating _CtlInfo

%BEGIN Splitsen, idcs_anders _CtlInfo
  dlg_splitsen_eh(_Win,e_Control(idcs_anders,_CtrlType,_CtrlWin,_CtlInfo),0):-
	Checked = win_Ischecked(_CtrlWin),
	rekeningMetPreset(_Win, Checked),
	!.
%END Splitsen, idcs_anders _CtlInfo

%BEGIN Splitsen, idcs_geen_eindronde _CtlInfo
  dlg_splitsen_eh(_Win,e_Control(idcs_geen_eindronde,_CtrlType,_CtrlWin,_CtlInfo),0):-
	splcat(Cat),
	not(isKwalificatie(Cat,_)),
  	Msg1 = "Het is aan te bevelen om een (dummy) eindronde te maken,\ntenzij je precies weet wat je doet!",
  	Msg2 = "\n\nMet een (dummy) eindronde kun je\n- het makkelijkst weer 'Combineren'.\n- alle gesplitste delen tesamen publiceren.",
  	
  	Msg3 = "\n\nToch een dummy eindronde maken?",
  	format(Msg, "%s%s%s", Msg1, Msg2, Msg3), % isnrt
	1 = dlg_MessageBox( "Eindronde", Msg, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
	dialog_SetRadioButton(_Win,idcs_eindronde,idcs_verzameleindronde),
	!.
%END Splitsen, idcs_geen_eindronde _CtlInfo


%BEGIN Splitsen, idcs_poules _CtlInfo
  dlg_splitsen_eh(_Win,e_Control(idcs_poules,_CtrlType,_CtrlWin,_CtlInfo),0):-
	dialog_SetRadioButton(_Win, idcs_eindronde, idcs_eindronde),
	dialog_SetState(_Win,[enable(idcs_geen_eindronde,b_True),
			      enable(idcs_verzameleindronde,b_True),
	                      enable(idcs_eindronde,b_True)]),
	                      %enable(idcs_pouleladder,b_True)]),
	!.
%END Splitsen, idcs_poules _CtlInfo

%BEGIN Splitsen, idcs_niet _CtlInfo
  dlg_splitsen_eh(_Win,e_Control(idcs_niet,_CtrlType,_CtrlWin,_CtlInfo),0):-
	dialog_SetRadioButton(_Win, idcs_eindronde, idcs_geen_eindronde),
	dialog_SetState(_Win,[enable(idcs_geen_eindronde,b_True),
			      enable(idcs_verzameleindronde,b_True),
	                      enable(idcs_eindronde,b_False)]),
	                      %enable(idcs_pouleladder,b_False)]),
	!.
%END Splitsen, idcs_niet _CtlInfo

%BEGIN Splitsen, idcs_afvalsysteem _CtlInfo
  dlg_splitsen_eh(_Win,e_Control(idcs_afvalsysteem,_CtrlType,_CtrlWin,_CtlInfo),0):-
	dialog_SetRadioButton(_Win, idcs_eindronde, idcs_eindronde),
	dialog_SetState(_Win,[enable(idcs_geen_eindronde,b_True),
	                      enable(idcs_eindronde,b_True),
			      enable(idcs_verzameleindronde,b_True)]),
	                      %enable(idcs_pouleladder,b_False)]),
	!.
%END Splitsen, idcs_afvalsysteem _CtlInfo

%BEGIN Splitsen, idc_ok _CtlInfo
  dlg_splitsen_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	_IDCS_AANTAL_VALUE = dialog_Getint(_Win, idcs_aantal),
	_IDCS_AANTAL_VALUE = i(1),
	Msg =  "Splitsen in slechts 1 deel kan niet. Daarvoor kun je direct de knop \"Indelen/loten\" gebruiken.",
	_M = dlg_MessageBox( "Splitsen", Msg, mesbox_iconExclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), !.
  dlg_splitsen_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	_IDCS_PLAATSING_CHECKED = dialog_GetCheck(_Win, idcs_plaatsing),
	_IDCS_VERHINDERINGEN_CHECKED = dialog_GetCheck(_Win, idcs_verhinderingen),
	_IDCS_CLUB_CHECKED = dialog_GetCheck(_Win, idcs_club),
	_IDCS_AANTAL_VALUE = dialog_Getint(_Win, idcs_aantal),
	IDCS_VORM = dialog_GetRadiobutton(_Win, idcs_afvalsysteem),
	EINDRONDEVORM = dialog_GetRadiobutton(_Win, idcs_eindronde),
	%write("\n---Eindrondevorm: \n", Eindrondevorm),
	RatingOfAnders = dialog_GetRadiobutton(_Win, idcs_rating),
	cursor_SetWait(),
	retract(splcat(CatNo)),
	findall(Cat1x, wc(Cat1x, _, _, _, _, _, _, _, _, _, _, _, _, _, _), Cat1ListOrig),	% originele om daar de nieuwe onderdelen tussen te passen !
	getbacktrack(Here),
	wc(CatNo, OnderdeelID, N, EindrondeNaam, WedsTyp, Short, SexList, Speelsterkte, LeeftV, LeeftTM, HDG, Schemasoort, Keuring, Tid,Geld),  
	cutbacktrack(Here),
	%kwalip(CatNo, Kwalificatie),
	blokTijdRetract(CatNo, BlokL),
	int_dialogInt(SplitsAantal,_IDCS_AANTAL_VALUE),
	vrijeCatNummers(SplitsAantal, CatNo, CatVrij),  % CatVrij is het eerste onderdeel van de vrije (gesplitste) reeks
	Master = wc(CatVrij, OnderdeelId, N, EindrondeNaam, WedsTyp, Short, SexList, Speelsterkte, LeeftV, LeeftTM, HDG, Schemasoort, Keuring, Tid, Geld),
	genereerWC(0, SplitsAantal, Master, BlokL, [], NewCatsL, [], NewCatL),
	assert(splcat(CatVrij)),
	ranglijstOptieToevoegen(CatNo, NotLoot34),
	assert(splits(RatingOfAnders, _IDCS_PLAATSING_CHECKED, _IDCS_VERHINDERINGEN_CHECKED, _IDCS_CLUB_CHECKED, IDCS_VORM, EINDRONDEVORM)),
	findall(Baan_tijd,baan_dag(Baan_tijd),Baan_tijden),
	Rating1OfAnders0 = toGetal(RatingOfAnders),
	presetSplitsOpties(Rating1OfAnders0, _IDCS_PLAATSING_CHECKED, _IDCS_VERHINDERINGEN_CHECKED, _IDCS_CLUB_CHECKED, PLAATSING_CHECKED, VERHINDERINGEN_CHECKED, CLUB_CHECKED),
	findall(Opgegevens,opgeg1(CatNo,Baan_tijden,Opgegevens,VERHINDERINGEN_CHECKED, CLUB_CHECKED, Rating1OfAnders0),Opgli),
	%str_int(NL34, NotLoot34),
	%dlg_note(NL34),
	writedevice(X),
	writedevice(screen),
	%write("\n!!! ",SplitsAantal, ", ", VERHINDERINGEN_CHECKED, ", ", CLUB_CHECKED, ", ", PLAATSING_CHECKED, ", ", NotLoot34, ", ", Rating1OfAnders0, ",\n"),
	%writeTest(OpgLi),
	%                   0                         0            0                 1             1                 0
	splitsc(SplitsAantal, OpgLi, VERHINDERINGEN_CHECKED, CLUB_CHECKED, PLAATSING_CHECKED, NotLoot34, Rating1OfAnders0),  % NB OpgLi is input & output!
        %write("...\n"),
	%writeTest(OpgLi),
	writedevice(X),
	%save("splits.txt",splitsen),
	ruimOpWC(CatNo),
	verhuisOpg(CatNo, NewCatL,OpgLi),
	moveUitsl(CatNo, CatVrij),
	close_schemas(),

	cursor_SetWait(),
	positionWC(CatNo, Cat1ListOrig, Cat1L1),	% achterste stuk incl. catno van orig lijst
	Cat1L1 = [_|Cat1L1Min], 
	evenWegWC(Cat1L1Min, Cat1L1Vol),		% haal achterste stuk weg
	sluitAanWC(Cat1L1Vol),				% en voeg achterste stuk weer toe
	cursor_SetWait(),
	deelDelenInInPoule(NewCatL, IDCS_VORM),		% alvast
	logclose(),
	loglock(mdi, "splits"),
	win_SetState(_Win, [wsf_Disabled]),
	passendeEindronde(_Win, CatNo, EINDRONDEVORM,NewCatL,NewCatsL, EindrondeNaam, Short, BlokL),
	fail.
  dlg_splitsen_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	logclose(),
	fail.
%END Splitsen, idc_ok _CtlInfo

%BEGIN Splitsen, idc_help _CtlInfo
  dlg_splitsen_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Splitsencombineren"),
	!.
%END Splitsen, idc_help _CtlInfo

%BEGIN Splitsen, e_Create
  dlg_splitsen_eh(_Win,e_Create(_CreationData),0):-
	Font = font_Create(ff_Fixed, [fs_Bold], 10),
	TellWin = win_GetCtlHandle(_Win, idcts_tell),
	win_SetFont(TellWin, Font),
	makeTekst(_Win, 0),
	splcat(Cat),
	not(isMasterCat(Cat)),
	wc(Cat, _, _, K2, _, _, _, _, _, _, _, _SchemaSoort, _, _, _),
	%not(SchemaSoort = "0"),
	%wc(Cat,K2,_,_,_,_,_),
	%wcritp(Cat, Spst, _, _, _),	 
	format(Titel, "Splitsen van onderdeel %s", K2),
	win_SetText(_Win, Titel),
	IDCS_VORM = dialog_GetRadiobutton(_Win, idcs_afvalsysteem),
	presetVerder(_Win, IDCS_VORM, Cat),
	!.
  dlg_splitsen_eh(_Win,e_Create(_CreationData),0):-
	CW = win_GetCtlHandle(_Win, idcs_rating),
	win_SendEvent(_Win,e_Control(idcs_rating,wc_RadioButton,CW, activated())),
	!.

%END Splitsen, e_Create

%BEGIN Splitsen, idcs_aantal modified
%  dlg_splitsen_eh(_Win,e_Control(idcs_aantal,_CtrlType,_CtrlWin,modified),0):-
%	AantD = dialog_GetInt(_Win, idcs_aantal),
%	AantD = i(1),
%        dialog_SetRadioButton(_Win, idcs_eindronde, idcs_geen_eindronde),
%	fail. 
  dlg_splitsen_eh(_Win,e_Control(idcs_aantal,_CtrlType,_CtrlWin,modified),0):-
	AantD = dialog_GetInt(_Win, idcs_aantal),
	int_dialogInt(Aantal, AantD), 
	%Aantal > 1,
	makeTekst(_Win, Aantal),
	!.
%END Splitsen, idcs_aantal modified

  dlg_splitsen_eh(_,_,_):-!,fail.

%END_DLG Splitsen
%BEGIN_DLG Combineer
/**************************************************************************
	Creation and event handling for dialog: Combineer
**************************************************************************/

constants

%BEGIN Combineer, CreateParms, 09:46:32-22.12.2013, Code automatically updated!
  dlg_combineer_DlgType = wd_Modal
  dlg_combineer_Title = "Combineer"
  dlg_combineer_RCT = rct(50,40,277,200)
  dlg_combineer_Flags = [wsf_Close,wsf_TitleBar]
  dlg_combineer_Help = idh_splitsencombineren
  dlg_combineer_Font = "Arial"
  dlg_combineer_FSize = 9
%END Combineer, CreateParms

database - comb
speler(integer)
single catzelf(wedscat)
%single voortgang(integer)
%single job(integer)
single gedaan(BOOLEAN)
single inschrijfgeld(real)

predicates

  dlg_combineer_eh : EHANDLER
nondeterm  cat_soort(wedscat, string, wedstyp, sexlist, boolean IsKwalificatie)
nondeterm sexListMV(wedstyp, sexlist, sexe)
nondeterm multsel(slist, wedscat)
nondeterm opgInList(integer, catl)
procedure  zetover(catl, wedscat)
  nietAanw(wedscat Nieuw, tgnst)
nondeterm sp_num(tgnst, integer)
procedure  nieuweNaam(string, string, integer)
nondeterm  spelerCats(wedscat, catl, sex)
procedure  reduceerSpCats(sexlist, sexlist, sexlist, integer)
procedure isKwalificatie1(wedscat CatZelf, boolean)
procedure maxinschrijfgeld(real)
procedure updateInschrijfgeld(wedscat Cat, real Ins, real InsN)

clauses

catzelf(0).
%voortgang(0).
%job(0).
gedaan(b_False).
inschrijfgeld(0.0).

reduceerSpCats([], _, [], 0) :- !.
reduceerSpCats([SpCat|SpCatList], Out, [SpCat|Out1], 1) :-
  not(member(SpCat, Out)), !,
  reduceerSpCats(SpCatList, [SpCat|Out], Out1, _).
reduceerSpCats([_|SpCatList], Out, Out1, Changed) :-
  reduceerSpCats(SpCatList, Out, Out1, Changed).
  
spelerCats(CatZelf, _, SpCat):-
  wc(CatZelf, _, _, _, _, _, SpCats, _, _, _, _, _, _, _, _),
  %wc(CatZelf,_,_,_,SpCats,_,_),
  member(SpCat, SpCats).    
spelerCats(_, CatList, SpCat):-
  member(Cat, CatList),
  wc(Cat, _, _, _, _, _, SpCats, _, _, _, _, _, _, _, _),
  %wc(Cat,_,_,_,SpCats,_,_),
  member(SpCat, SpCats).    
  
nieuweNaam(CatN,CatN1,Pos) :-
  frontstr(3, CatN, Str, _),
  searchchar(Str, '-', Pos),
  Pos > 0,
  frontstr(Pos, CatN, _, CatN1), 
  not(wc(_, _, _, CatN1, _, _, _, _, _, _, _, _, _, _, _)), !.	% 7.10.2001
nieuweNaam(CatN, CatN, 0).

sp_num(s(Num), Num) :-!.
sp_num(pw(Num), Num) :-!.
sp_num(p(Num, _), Num).
sp_num(p(_, Num), Num).

nietAanw(CatNew, Tgnst) :-
  not(opg(_,CatNew,Tgnst,_,_,_,_,_,_,_,_)),	
  sp_num(Tgnst, Num),
  speler(Num), !,		% tijdelijke lijst
  sp2(Num,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  format(Msg, "\nSpeler: %-20s staat al opgegeven voor dit nummer, opgave niet overgezet.", Naam),
  write(Msg),
  fail.
nietAanw(_, _).

zetover(_, CatNew) :-		% CatList -> CatNew
  cursor_SetWait(),
  logclose(),
  %loglock(modi, "zetover"),
  opg(_,CatNew,Tgnst,_,_,_,_,_,_,_,_),
  sp_num(Tgnst, Sp),
  assert(speler(Sp)),			% kijk wie er al in CatNew zitten
  fail.
zetover(CatList, CatNew) :-
  member(Cat, CatList),						% alle categorieën
  opg(Num,Cat,Tgnst,Plts,Rnd,Uits,Res,LL,TeamId,Keur,Melding),	
  nietAanw(CatNew, Tgnst),						% weiger als al deelnemer
  logf('R', opg(Num,Cat,Tgnst,Plts,Rnd,Uits,Res,LL,TeamId,Keur,Melding),ja),
  not(opg(_,CatNew,Tgnst,_,_,_,_,_,_,_,_)),		% personen staan al opgegeven
  getbacktrack(Here),
  numb(NNum),
  OutNum = Num + NNum - 1,
  not(opg(OutNum,CatNew,_,_,_,_,_,_,_,_,_)),		% minuut verder als hij al bestond
  cutbacktrack(Here),
  logf('A',opg(OutNum,CatNew,Tgnst,Plts,Rnd,Uits,Res,LL,TeamId,Keur,Melding),ja),	% maak deel van CatNew
  sp_num(Tgnst, Sp),
  assert(speler(Sp)),
  fail.
zetover(CatList, _) :-
  member(Cat, CatList),
  ruimOpWC(Cat),						% wis indeling etc.
  fail.
zetover(CatList, CatNew) :-
  not(isMasterCat(CatNew)), % niet bij knltb AR verzamel
  member(Cat, CatList),  
  not(opg(_,Cat,_,_,_,_,_,_,_,_,_)),
  wc(Cat, OnderdeeelID, N, Naam, EDG, Kort, SexL, SpStrk, LftV, LftTM, HDG, SchS, Keuring, KNLTB,Geld),
  logf('R', wc(Cat, OnderdeeelID, N, Naam, EDG, Kort, SexL, SpStrk, LftV, LftTM, HDG, SchS, Keuring, KNLTB,Geld),ja),	% Cat kan weg
  kopl(Cat,Rang,CatZelf,Plaats),
  logf('R',kopl(Cat,Rang,CatZelf,Plaats),ja),
  fail.
zetover(CatList, CatZelf) :-
  getbacktrack(Here),
  wc(CatZelf, OnderdeeelID, N, CatN, WedsTyp, Short, _SexL, SpStrk, LftV, LftTM, HDG, "", Keuring, KNLTB,Geld),
  cutbacktrack(Here),
  nieuweNaam(CatN,CatN1,Pos),
  findall(SpCat, spelerCats(CatZelf, CatList, SpCat), SpCatList),
  reduceerSpCats(SpCatList, [], NwSpCatList, Changed),
  Pos + Changed > 0,				% als een van beide veranderd
  findall(Cat1, wc(Cat1, _, _, _, _, _, _, _, _, _, _, _, _, _, _), Cat1L),	% zelfde pos in wclijst
  positionWC(CatZelf, Cat1L, Cat1L1),
  evenWegWC(Cat1L1, Cat1L1Vol),			% haal achterste stuk weg
  logf('Z',wc(CatZelf, OnderdeeelID, N, CatN1, WedsTyp, Short, NwSpCatList, SpStrk, LftV, LftTM, HDG, "", Keuring, KNLTB,Geld),ja),
  sluitAanWC(Cat1L1Vol),			% en voeg weer toe
  fail.
zetover(CatList, _) :-
  member(Cat, CatList),
  getbacktrack(Here),
  opg(_,Cat,_,_,_,_,_,_,_,_,_),
  cutbacktrack(Here),
  wc(Cat, _, _, CatNaam, _, _, _, _, _, _, _, _, _, _, _),
  logclose(),
  format(Msg,"\nOnderdeel %s laten staan met de niet overgehevelde opgaven", CatNaam),
  writeToJournaal(Msg),
  fail.
zetover(_, _) :-
  closefile(work),
  logclose().

opgInList(OpgaveNum, CatList) :-
  member(CatNo, CatList),
  opg(OpgaveNum,CatNo,_,_,_,_,_,_,_,_,_).

maxinschrijfgeld(Ins) :-
  inschrijfgeld(I),
  Ins > I, !,
  assert(inschrijfgeld(Ins)).
maxinschrijfgeld(_Ins).

updateInschrijfgeld(Cat, Ins, InsN) :-
  Ins <> InsN,
  getbacktrack(Here),
  wc(Cat, OnderdeeelID, N, Ltext, WT, Ktext, SexL, SpStrk, LftV, LftTM, HDG, Schemasoort, Keuring, KNLTB,_Geld),
  cutbacktrack(Here),
  findall(Cat1, wc(Cat1, _, _, _, _, _, _, _, _, _, _, _, _, _, _), Cat1L),	% zelfde pos in wclijst !
  positionWC(Cat, Cat1L, Cat1L1),
  evenWegWC(Cat1L1, Cat1L1Vol),			% haal achterste stuk weg
  logf('Z',wc(Cat, OnderdeeelID, N, Ltext, WT, Ktext, SexL, SpStrk, LftV, LftTM, HDG, Schemasoort, Keuring, KNLTB,InsN), nee),
  Cat1L1Vol = [_|CatStaart],
  sluitAanWC(CatStaart), !.			% en voeg weer toe en set modified()
updateInschrijfgeld(_Cat, _, _).

multsel(CatSel, CatM) :-
  member(Keus, CatSel),
  searchchar(Keus, '\t', P1),
  frontstr(P1, Keus, _, K1),
  searchchar(K1, '\t', P2),
  frontstr(P2, K1, _, CatS),
  wc(CatM, _, _, CatS, _, _, _, _, _, _, _, _, _, _, Ins),
  %wc(CatM,CatS,_,_,_,Ins,_),
  maxinschrijfgeld(Ins).

%sexListMV(g, _, m) :- !.		% bewaak mannetje en vrouwtje
sexListMV(_, SrtL, MV) :-
  member(Sx, SrtL),
  spsrt(Sx, _, MV, _, _, _, _, _).
sexListMV(X, [_|Rest], MV) :-
  sexListMV(X, Rest, MV).
 
cat_soort(CatZelf, CatStr, _WedsTyp, _SexList, b_True) :-	% kwalificatie
  kopl(CatZelf, _, Hoofd, _),
  kopl(CatK, _, Hoofd, _),
  not(CatK = CatZelf),
  wc(CatK, _, _, Cat, _WedsTyp, _, _SL, _, _, _, _, _, _, _, _),
  %wc(CatK,Cat,_WedsTypx,_/*Short*/,_SLx,_,_),
  opg1(CatK,Text,1),
  findall(OpgaveNum, opg(OpgaveNum,CatK,_,_,_,_,_,_,_,_,_),OpgL),
  count(OpgL, 0, Aant),
  format(CatStr, "%s\t%3d\t%s", Text, Aant, Cat).
cat_soort(CatZelf, CatStr, _WedsTyp, _SexList, b_False) :-
  %voorrondes(CatZelf,b_True),
  voorronde(CatZelf, CatNo),
  CatNo <> CatZelf,
  wc(CatNo, _, _, Cat, _WedsTyp, _, _SL, _, _, _, _, _, _, _, _),
  %wc(CatNo,Cat,_WedsTypx,_/*Short*/,_SLx,_,_),
  opg1(CatNo,Text,1),
  findall(OpgaveNum, opg(OpgaveNum,CatNo,_,_,_,_,_,_,_,_,_),OpgL),
  count(OpgL, 0, Aant),
  format(CatStr, "%s\t%3d\t%s", Text, Aant, Cat).
cat_soort(CatZelf, CatStr, WedsTyp, SexList, b_False) :-
  %voorrondes(CatZelf,b_False),
  %wc(CatNo,Cat,WedsTyp,_/*Short*/,SL,_,_),
  wc(CatNo, _, _, Cat, WedsTyp, _, SL, _, _, _, _, _, _, _, _),
  CatNo <> CatZelf,
  not(isOndercat(CatNo)),	% 26.11.2001
  getbacktrack(Here),
  sexListMV(WedsTyp, SexList, MV),
  sexListMV(WedsTyp, SL, MV),
  cutbacktrack(Here),
  opg1(CatNo,Text,1),
  findall(OpgaveNum, opg(OpgaveNum,CatNo,_,_,_,_,_,_,_,_,_),OpgL),
  count(OpgL, 0, Aant),
  format(CatStr, "%s\t%3d\t%s", Text, Aant, Cat).

combineer(Win, CatZelf, Done) :-
  dlg_combineer_Create(Win, CatZelf, Done).

isKwalificatie1(CatZelf, b_True) :-
  kopl(CatZelf, _, Cat, _),
  wc(Cat, _, _, _, _, _, _, __, _, _, _, _, _, Tid, _),
  toernooiK(Tid, nrt), !.
isKwalificatie1(_CatZelf, b_False).
 

  dlg_combineer_Create(Parent, CatZelf, Gedaan):-
	retractall(speler(_), comb),
  	assert(catzelf(CatZelf)),
	assert(gedaan(b_False)),
	win_CreateDynDialog(Parent,
		[
%BEGIN Combineer, WinDefList, 09:46:32-22.12.2013, Code automatically updated!
		 dlg_font(wdef(dlg_combineer_DlgType,dlg_combineer_RCT,dlg_combineer_Title,u_DlgBase),
		 	  dlg_combineer_Font,dlg_combineer_FSize,dlg_combineer_Flags),
		 ctl(wdef(wc_PushButton,rct(182,132,222,144),"&OK",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(182,118,222,130),"&Cancel",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(182,104,222,116),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_LBox,rct(4,27,178,142),"",u_DlgBase),combineermet,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_MultiSelect,wsf_UseTabStops]),
		 ctl(wdef(wc_Text,rct(4,16,148,26),"Combineren met:",u_DlgBase),idct_combineren_met,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(4,3,189,15),"Catzelf",u_DlgBase),idct_catzelf,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(4,147,197,157),"NB Je kunt meerdere onderdelen aanklikken.",u_DlgBase),idct_nb1,[wsf_AlignLeft])
%END Combineer, WinDefList
		],dlg_combineer_eh,0),
		gedaan(Gedaan).

%BEGIN Combineer, idc_ok _CtlInfo
  dlg_combineer_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	catzelf(CatZelf),
	%retractall(modified()),
	%assert(modified()),
	%loglock(),
	cursor_SetWait(),
	assert(gedaan(b_True)),
	LboxWin = win_GetCtlHandle(_Win, combineermet),
	lbox_GetSel(LboxWin,CatSel,_IndexList),
	wc(CatZelf, _, _, Naam, _, Kort, _, _, _, _, _,_SchS, _, _, InsZ),
	assert(inschrijfgeld(InsZ)),
	findall(CatM, multsel(CatSel, CatM), CatList),
	findall(OpNum, opgInList(OpNum, CatList), Nummers),
	count(Nummers, 0, TotaalAant),
	closefile(work),
	format(String,"Combineren klaar!\nTotaal aantal: %u.\n\nNaam van het resulterende onderdeel:\n%s.\nDe korte naam is: %s\nNB Strijk deze namen weer glad indien nodig!\nKlik daarvoor op \"Onderdeelnaam\".", TotaalAant,Naam,Kort),
	zetover(CatList, CatZelf),
	inschrijfgeld(InsZN),
	updateInschrijfgeld(CatZelf, InsZ, InsZN), 
	retractall(speler(_), comb),
	%SchS = "",
	dlg_MessageBox( "Combineren", String, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	logf('T', wd(1,CatZelf,onbekend,onbekend,"",0,""), nee),
	%ruim_opT(CatZelf),
	win_Destroy(_Win),
	!.
  dlg_combineer_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	win_Destroy(_Win), !.
%END Combineer, idc_ok _CtlInfo
%MARK Combineer, new events

%BEGIN Combineer, e_Create
  dlg_combineer_eh(_Win,e_Create(_CreationData),0):-!,
	catzelf(CatZelf),
	wc(CatZelf, _, _, CatS, WedsTyp, _, SexList, _Speelsterkte, _, _, _, _, _, _Tid, _),
	%wc(CatZelf,CatS,WedsTyp,_Short,SexList,_,_),    
	%wcritP(CatZelf, Speelsterkte, _, _, _),
	TWin = win_GetCtlHandle(_Win, idct_catzelf),
	win_SetText(TWin, CatS),
	isKwalificatie1(CatZelf, IsKwali),
	findall(CatNo, cat_soort(CatZelf, CatNo, WedsTyp, SexList, IsKwali), Candidaten),
	LboxWin = win_GetCtlHandle(_Win, combineermet),
	lbox_SetTabStops(LboxWin, [8, 20]),
	lbox_Add(LboxWin, Candidaten),
	!.
%END Combineer, e_Create

%BEGIN Combineer, idc_help _CtlInfo
  dlg_combineer_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Splitsencombineren"),
	!.
%END Combineer, idc_help _CtlInfo

%BEGIN Combineer, idc_cancel _CtlInfo
  dlg_combineer_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	win_Destroy(_Win),
	!.
%END Combineer, idc_cancel _CtlInfo

  dlg_combineer_eh(_,_,_):-!,fail.

%END_DLG Combineer




database - inschr

single iafm(integer WProbl, integer WMark, integer WDAtum, integer WShort, integer WOID)  % OID = internet opgaveid 
determ inschrWin(WINDOW)
markH(tgnst, wedscat)  % handled
hoogsteopgave(integer)
%single maxlen(integer)

predicates

procedure removeDubbels(tijdl, tijdl, tijdl)
nondeterm opgaveL(string, tijdl)
 tgnst_S(tgnst, string)
procedure itemRepr(string, string, string, string, string, string, string Marked, string Uitsl, string, color, string, color, slist Nrs, slist Pnrs)
procedure partner2s(string, string, string Importnummer, slist PNrList)
procedure mark(tgnst, wedscat, integer)
procedure merkteken(string, string)
procedure findTop(integer, slist, integer) - (i,i,o)
procedure isProbleem(string Item, string Uitroepteken)
procedure uitsl(integer, string)
procedure importnummer2tekst(slist, string)
procedure resetmarkH()
procedure tgnst2firstmultopg(tgnst, color, color)
procedure ishoogsteopgave(integer, color)

clauses

setmarkH(T, Cat) :-
  retractall(markH(T, Cat)),
  assert(markH(T, Cat)).
  
resetmarkH() :-
  retractall(markH(_, _)).

iafm(0,0,0,0,0).
%preselected([]).
%uitklappen(b_True).

tgnst_S(s(Num),Str) :- 
  format(Str, "%u\t", Num), !.
tgnst_S(pw(Num),Str) :- 
  format(Str, "%u\tX", Num), !.
tgnst_S(p(N1,N2),Str) :- !,
  format(Str, "%u\t%u", N1, N2).

removeDubbels([], Uit, Uit) :- !.
removeDubbels([H], In, Uit) :-
  removeDubbels([], [H|In], Uit),  !.
removeDubbels([H,H|Rest], In, Uit) :-
  removeDubbels([H|Rest], In, Uit), !.
removeDubbels([H|Rest], In, Uit) :-
  removeDubbels(Rest, [H|In], Uit), !.

mark(Tgnst, Cat, 1) :-
  markH(Tgnst, Cat), !.
mark(_, _, 0).

opgaveL(Str, TijdenS) :-
  member(Tijd, TijdenS),
  wc(Cat, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
  %wc(Cat,_,_,_,_,_,_),
  opg(Tijd,Cat,Tgnst,_,_,_,_,_,_,_,_),
  mark(Tgnst, Cat, Mark),
  tgnst_S(Tgnst, TStr),
  tgnst2firstmultopg(Tgnst, C1, C2),
  format(Str, "%u\t%U\t%u\t%s\t%u\t%u", Mark, Tijd, Cat, TStr, C1, C2).

ishoogsteopgave(SKeus, color_Yellow) :-
  sp2(SKeus,_,_Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_Opm,_,_,_,_,_,_,NrList),
  NrList = [_, _ | _],  % minstens 2
  not(hoogsteopgave(SKeus)),
  assert(hoogsteopgave(SKeus)), !. 
ishoogsteopgave(_SKeus, color_White).

tgnst2firstmultopg(s(SKeus), Color1, color_White) :-
  ishoogsteopgave(SKeus, Color1), !.
tgnst2firstmultopg(pw(SKeus), Color1, color_White) :-
  ishoogsteopgave(SKeus, Color1), !.
tgnst2firstmultopg(p(SKeus1, SKeus2), Color1, Color2) :-
  ishoogsteopgave(SKeus1, Color1),
  ishoogsteopgave(SKeus2, Color2), !.
tgnst2firstmultopg(_, color_White, color_White).

merkteken("1", "+") :- !.
merkteken(_, "").

findTop(_, _, 0) :-
  not(markH(_,_)), !.
findTop(N, [H|_T], N) :- 
  fronttoken(H,"1",_), !.
findTop(N, [_|T], No) :-
  N1 = N + 1,
  findTop(N1, T, No), !.
findTop(_, _, 0).

uitsl(0, "") :- !.
uitsl(_, "U").

itemRepr(Str, Repr, TStr, Short, Naam, PNaam, Mark, Uitsl, NrTekst, Col1, PNrTekst, Col2, NrList, PNrList) :-
  parsetabs(Str, Vals),
  Vals = [MarkS, Tijds, CatS, SpS, S3, C1, C2 | _],
  merkteken(MarkS, Mark),
  str_int(TijdS, Tijd),
  dt_minoffset_to_str(Tijd, "%DD/%MD %HH:%MM", TStr),
  str_int(CatS, Cat),
  opg(Tijd,Cat,_Tgnst,_,_,U,_,_,_,_,_),
  uitsl(U, Uitsl),
  wc(Cat, _, _, _, _, Short, _, _, _, _, _, _, _, _, _),
  %wc(Cat,_,_,Short,_,_,_),
  str_int(SpS, SKeus),
  sp2(SKeus,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_Opm,_,_,_,_,_,_,NrList),
  importnummer2tekst(NrList, NrTekst),
  partner2S(S3, PNaam, PNrTekst, PNrList),
  str_int(C1, Col1),
  str_int(C2, Col2), !,
  format(Repr, "%s  %-7s %-15s %s", TStr, Short, Naam, PNaam).
itemRepr(_, "-", "-", "-", "-", "-", "-", "-", "-", color_White, "-", color_White, [], []).

importnummer2tekst([Nr], Nr) :- !.
importnummer2tekst(List, T) :-
  reverse(List, [], LR),
  LR = [Nr | _], !,
  concat(Nr, " ...", T).
importnummer2tekst(_, " - ").
  
partner2S(R3, PNaam, PNrT, PNrList) :-
  str_int(R3, PKeus),
  sp2(PKeus,_,PNaam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_Opm,_,_,_,_,_,_,PNrList),
  importnummer2tekst(PNrList, PNrT), !.
partner2S(R3, "zoekt partner", "", []) :-
  searchchar(R3,'X',_FoundPos), !.
partner2S(_, "", "", []).

isProbleem(Str, "!") :-
  fronttoken(Str, _MarkS, Str1),
  fronttoken(Str1,TijdS,R1),
  str_int(TijdS, Tijd),
  fronttoken(R1, CatS, _R2),
  str_int(CatS, Cat),
  opg(Tijd,Cat,Tgnst,_Plts,_Byes,_,_,_,_,_,_),  % minus uitgeslotenen!
  tgnst_num(Tgnst, Num),
  partnerCritFout(Cat, Num, b_False, b_True), 	% geen messages en fail.
  !.
isProbleem(_Str, "").


%BEGIN_DLG Opgaven
/**************************************************************************
	Creation and event handling for dialog: Opgaven
**************************************************************************/

constants

%BEGIN Opgaven, CreateParms, 14:13:49-16.4.2015, Code automatically updated!
  dlg_opgaven_DlgType = wd_Modal
  dlg_opgaven_Title = "Opgaven Control Center"
  dlg_opgaven_RCT = rct(50,40,484,303)
  dlg_opgaven_Flags = [wsf_Close,wsf_TitleBar]
  dlg_opgaven_Help = idh_opgaven
  dlg_opgaven_Font = "Arial"
  dlg_opgaven_FSize = 10
%END Opgaven, CreateParms

id_refresh = 23
id_kijk = 24
id_koppeldubbel = 25

domains
  kd = kd(wedscat, integer)
  kdlist = kd*
  hdom = opgavenS ; opgavenP ; nrS ; nrP

database - hs
  hotspotO(hdom, rct, string Itemid) 
  single maxlen(integer)
  matchSp(integer TSpno, integer SpNo)
  single tempWc(integer)
  %echteWCo(wedscat)

predicates

  dlg_opgaven_eh : EHANDLER
  subclasso_eh : EHANDLER  
  myDraw(WINDOW, integer ItemId, RCT RectItem, COLOR Voorgrond, COLOR Achtergrond, boolean Selected)
  determ boodschapaantal(ilist)
  procedure metXML(boolean, window)  % inklappen/uitklappen
  procedure markUitsl(string Uitsl, boolean Selected, window, rct)
  setbuttons(window, boolean Selected)
  %getInschrijf(slist)
  nondeterm koppeldubbel(kd, string)
  procedure selecteerEen(window)
  procedure opgavenopnieuw()
  procedure isUitgesloten(boolean, string, color, color)
  procedure updatehotspot(hdom, rct, string, string)
  procedure nieuweSelect(window, string)
  determ indexVan(slist, string, integer, integer)
  nondeterm geenpartnerformulier(integer)
  procedure maxOndKort(window, integer)
  procedure maxOpgTijd(window, integer)
  %procedure getAcceptatieresultaat(window, string Tid)
  determ tgnst2tgnst(tgnst TgnstTijdelijk, tgnst Tgnst) - (i,o)
  procedure geenPartnerFormulieren(string)
  %meldingteam2status(string,teamstatus)    
  determ ongeldig(string RatingE,string RatingD,string SterkteE,string SterkteD)
  %determ matchBovenleeftijd(integer,integer)
  determ isalclub(string, string, string, file)
  procedure status2WC(wedscat WC, teamstatus Status, wedscat WC1, integer Uitsluit)
  determ teamid2Cat(string Teamid, wedscat WC1, integer OpgNr)
  determ matchtgnst(tgnst, tgnst)
  determ vertaalsexe(sex, sex)
  procedure updateonderdeelId(string OnderdeelIDN, string OnderdeelID, dbasedom) - (i,i,i)
  determ staanalopg(wedscat, tgnst)
  procedure updateveld(string, string, string) - (i,i,o)
  %procedure cleanonderdeel(wedscat, dbasedom)
  %edgmatch(wedstyp, wedstyp)  % is nu enkel e en d
  % nondeterm spMetGeenOpgave(integer)
 
clauses

maxlen(0).
tempWc(0).

subclasso_eh(_LWin,Event,0):-
  not(Event =  e_mouseDown(_,c_Nothing,mouse_button_left)),
  Parent=win_GetParent(_LWin),
  win_SendEvent(Parent,Event), 
  fail.
subclasso_eh(LboxWin,e_mouseDown(PNT,c_Nothing,mouse_button_left),0):-
  hotspotO(opgavenS, Rect, ItemId),  
  rect_PntInside(Rect,PNT),
  itemRepr(ItemId, _Repr, _TStr, _Short, _Naam, _PNaam, _Mark, _Uitsl, _NrTekst, _Col1, _PNrTekst, _Col2, NrList, _PNrList),
  getInschrijf(NrList), 
  nieuweSelect(LboxWin, ItemId), !.
subclasso_eh(LboxWin,e_mouseDown(PNT,c_Nothing,mouse_button_left),0):-
  hotspotO(opgavenP, Rect, ItemId),  
  rect_PntInside(Rect,PNT),
  itemRepr(ItemId, _Repr, _TStr, _Short, _Naam, _PNaam, _Mark, _Uitsl, _NrTekst, _Col1, _PNrTekst, _Col2, _NrList, PNrList),
  getInschrijf(PNrList),
  nieuweSelect(LboxWin, ItemId), !.
subclasso_eh(LboxWin,e_mouseDown(PNT,c_Nothing,mouse_button_left),0):-
  hotspotO(nrS, Rect, ItemId),  
  rect_PntInside(Rect,PNT),
  itemRepr(ItemId, _Repr, _TStr, _Short, Naam, _PNaam, _Mark, _Uitsl, _NrTekst, _Col1, _PNrTekst, _Col2, _NrList, _PNrList),
  sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Res),
  SpelerIn = sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Res),
  TaskWin = vpi_GetTaskWin(),
  dlg_persoonsgegevens_Create(TaskWin, SpelerIn),
  nieuweSelect(LboxWin, ItemId), !.
subclasso_eh(LboxWin,e_mouseDown(PNT,c_Nothing,mouse_button_left),0):-
  hotspotO(nrP, Rect, ItemId),  
  rect_PntInside(Rect,PNT),
  itemRepr(ItemId, _Repr, _TStr, _Short, _SNaam, PNaam, _Mark, _Uitsl, _NrTekst, _Col1, _PNrTekst, _Col2, _NrList, _PNrList),
  sp2(SKeus,Sex,PNaam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Res),
  SpelerIn = sp2(SKeus,Sex,PNaam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Res),
  TaskWin = vpi_GetTaskWin(),
  dlg_persoonsgegevens_Create(TaskWin, SpelerIn),
  nieuweSelect(LboxWin, ItemId), !.

nieuweSelect(LboxWin, ItemID) :-
  selectgeen(LboxWin),
  LboxList = lbox_GetAll(LboxWin),
  indexVan(LboxList, ItemId, 0, Index),
  lbox_SetSel(LboxWin, Index, b_True), !.
nieuweSelect(_LboxWin, _ItemID).

indexVan([X|_], X, N, N) :- !.
indexVan([_|R], X, N, Nuit) :-
  N1 = N + 1,
  indexVan(R, X, N1, Nuit), !.
indexVan(_, _, _, 0).

updatehotspot(Type, Rect, Itemid, Content) :-
  retractall(hotspotO(Type, _, Itemid)), 
  fronttoken(Content, _, _), !,
  asserta(hotspotO(Type, Rect, Itemid)).
updatehotspot(_Type, _Rect, _Itemid, _).

koppeldubbel(kd(Num, Cat), Namen) :-
  opg(Num,Cat,Tgnst,_,_,_,_,_,_,_,_),
  Tgnst = p(Sp1, Sp2),
  getbacktrack(Here),
  opg(Num1,Cat2,Tgnst,_,_,_,_,_,_,_,_),
  Num1 <> Num,
  not(kopl(Cat, _,Cat2,_)),
  not(kopl(Cat2,_,Cat,_)), 
  cutbacktrack(Here),
  tgnst_naam(ja, b_False, Cat, p(Sp1,Sp2),Namen,_).

%zelfde(Num, Num, Cat 

markUitsl("U", _, LboxWin, RectItem) :-
  win_SetBrush(LboxWin,brush(pat_BDiag, color_Black)),
  draw_Rect(LboxWin,RectItem),
 !.
markUitsl(_, _, _, _).

boodschapAantal(IndexList) :-	% boodschap bij maar één geselecteerd
  count(IndexList, 0, Aantal),
  Aantal < 2, !,
  1 = dlg_MessageBox( "Opgavenexport (XML)", "Je kunt meerdere opgaven selecteren!\nDoorgaan?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ).
boodschapAantal(_IndexList).

getInschrijf(OIDList) :-
  providerX(Prov, _, "", _),
  AV = aanmeldenvars(),
  get_tempdir(_, Download),
  deletefile(Download),
  list_to_string(OIDList, ",", OID ),
  format(Volgend,"http://%s/getopgx2009.php?stap=eendeel",Prov), 
  Return = downloadData(Volgend, Download, ["selectie",OID|AV], _, 0),
  Return = 0,
  TaskWin = vpi_GetTaskWin(),
  format(Titel, "Formulier %s", OID),
  filenameext(FN, OID, ".txt"), 
  dlg_rapport_Create(TaskWin, Titel, FN, b_True), !.
getInschrijf(OID) :-
  not(OID = []),
  dlg_MessageBox( "Formulier", "Niet aanwezig of internetverbinding mislukt.", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ).
  
dlg_opgaven_refresh(0) :-
  inschrWin(Win),
  win_PostEvent(Win, e_User(id_refresh, 0)), !.
dlg_opgaven_refresh(1) :-
  inschrWin(Win),
  win_Invalidate(Win), !.
dlg_opgaven_refresh(_).

metXML(b_False, Win) :-
  CtrlWin = win_GetCtlHandle(Win, idc_bestanden),
  CRect = win_GetOuterRect(CtrlWin),
  CRect = rct(_,YC,_,_),
  Rect = win_GetClientRect(Win),
  Rect = rct(_,_,X0,_Y0),
  OuterRCT  = win_GetOuterRect(Win),
  WsFlags   = win_GetState(Win),
  ClientRCT = rect_GetClient(WsFlags, b_False, OuterRCT),
  ClientRCT = rct(Xi, Yi, _, _),
  X2 = Xi + X0,
  Y2 = Yi + YC,
  win_Move(Win, rct(Xi, Yi, X2, Y2)),
  fail.
metXML(b_True, Win) :-
  CtrlWin = win_GetCtlHandle(Win, idct_nb),
  CRect = win_GetOuterRect(CtrlWin),
  CRect = rct(_,_YC,_,YC1),		% neem de bottom
  Rect = win_GetClientRect(Win),
  Rect = rct(_,_,X0,_Y0),
  OuterRCT  = win_GetOuterRect(Win),
  WsFlags   = win_GetState(Win),
  ClientRCT = rect_GetClient(WsFlags, b_False, OuterRCT),
  ClientRCT = rct(Xi, Yi, _, _),
  X2 = Xi + X0,
  Y2 = Yi + YC1 + 5,
  win_Move(Win, rct(Xi, Yi, X2, Y2)),
  fail.
metXML(_Gebruiken, _Win).

selecteerEen(LboxWin) :-
  _Index = lbox_GetSelIndex(LboxWin), !.
selecteerEen(LboxWin) :-
  lbox_SetSel(LboxWin, 0, b_True).
opgavenopnieuw() :-
  pos(_Cat, _, _, _),
  dlg_MessageBox( "Opnieuw kan niet", "Er zijn al onderdelen ingedeeld!\nDie moet je eerst wissen!", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  !.
opgavenopnieuw() :-
  resetmarkH(),
  retractall(opg(_,_,_,_,_,_,_,_,_,_,_)),
  findall(Spxx,  sp2(Spxx,_,_,_,_,_,_,_,_, _, _, _, _, _, _, _, _, _, _,_,_,_,_,_,_,_,_), List),
  member(No, List),
  getbacktrack(Here),
  retract(sp2(No,Soort,N1,Telthuis,_Verh,RatingE,RatingD,_Betaald,_UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,CheckGeg,_IncassoAkkoord,_Res)),
  assertz(sp2(No,Soort,N1,Telthuis,[],RatingE,RatingD,nee,0, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, "",nee,"","","",CheckGeg,"",[])),
  cutbacktrack(Here),
  fail.
opgavenopnieuw() :-
  compress(update),
  startdbRefresh(), !.

geenPartnerformulier(SpNo) :-
  sp2(SpNo,_Sex,_Naam,_,_,_,_,_,_,_Club, _Adres, _Woonpl, _LidNr, _Stx, _SterkD, _G, _Telwerk, _Telmobiel, _EMail, _Opm,_,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_,[]),
  getbacktrack(Here),
  sp2opg(SpNo,_WC,_,_,_,Koppel), % sp2opg(SpNo,Cat,OpgNo,MaatNo,Plts,p(SpNo,MaatNo)) :-	/*      en dubbelspel            */
  opg(_,_,Koppel,_,_,_,_,_,_,Teamstatus,_),
  not(Teamstatus = geaccepteerd),
  not(Teamstatus = nietgeaccepteerd),  % dan zijn ze afkomstig van de KNLTB en moeten niet meegeteld worden!
  Koppel = p(_,_),
  tgnst_num(Koppel,Andere),
  not(Andere = SpNo),
  sp2(Andere,_,_,_,_,_,_,_,_,_, _, _, _, _, _, _, _, _, _, _,_,_,_,_,_,_,[_]),
  cutbacktrack(Here).

maxOpgTijd(Win, _) :-
  assert(maxlen(0)),
  opg(Tijd,_,_,_,_,_,_,_,_,_,_),
  dt_minoffset_to_str(Tijd, "%DD/%MD %HH:%MM", TStr),
  win_GetTextExtent(Win, TStr, -1, Width, _Height),
  maxLen(CurMaxL),
  Width > CurMaxL,
  assert(maxlen(Width)),
  fail.
maxOpgTijd(_, MaxL) :-
  maxLen(MaxL).

maxOndKort(Win, _) :-
  assert(maxlen(0)),
  wc(_Cat, _, _, _, _, Short, _, _, _, _, _, _, _, _, _),
  win_GetTextExtent(Win, Short, -1, Width, _Height),
  maxLen(CurMaxL),
  Width > CurMaxL,
  assert(maxlen(Width)),
  fail.
maxOndKort(_, MaxL) :-
  maxlen(MaxL).

loadlineAcleanup() :-
  retractall(matchSp(_,_)),
  fail.
loadlineAcleanup() :-
  logclose().

ongeldig("","","","").  

matchBovenleeftijd(0,99) :- !.
matchBovenleeftijd(99,0) :- !.
matchBovenleeftijd(X,X).

status2wc(WC, nietgeaccepteerd, WCuit, 1) :-
  kopl(WCuit, _, WC, _), !.
status2wc(WC, _, WC, 0).

teamid2Cat(Teamid, WC1, OpgNr) :-
  % x%ux%u
  frontchar(TeamId,'x',R1),
  fronttoken(R1,WC,R2),
  frontchar(R2,'x',R3),
  str_int(WC,WC1),
  str_int(R3,OpgNr), !.
  

matchtgnst(s(X), s(X)) :- !.
matchtgnst(p(X1,X2), p(X1,X2)) :- !.
matchtgnst(p(X1,X2), p(X2,X1)).	% 5.7.2014

vertaalSexe(1,Uit) :-
  spsrt(Uit, _, m, _, _, _, _, _), !. % het eerste mannetje
vertaalSexe(_,Uit) :-
  spsrt(Uit, _, v, _, _, _, _, _), !. % het eerste vrouwtje

staanAlOpg(Cat, Tgnst) :-
  opg(_,Cat,TgnstX,_,_,_,_,_,_,_,_), 
  matchtgnst(Tgnst,TgnstX), !.
staanAlOpg(Cat, Tgnst) :-
  kopl(CatV, _, Cat, _),  % kwalificatieronde?
  opg(_,CatV,TgnstX,_,_,_,_,_,_,_,_), 
  matchtgnst(Tgnst,TgnstX), !.

%updateonderdeelId(OnderdeelID, OnderdeelID,_) :- !.
updateonderdeelId(OnderdeelIDN, _,wc(WC, _, Nr, Naam, EDG, Kort, SexL, SpStrk,leeftijd(VS,LV), leeftijd(TMS,TM2), HDG, Schemasoort, OrigMsg, ToernooiId, Geld)) :-
  logf('Z',wc(WC, OnderdeelIDN, Nr, Naam, EDG, Kort, SexL, SpStrk,leeftijd(VS,LV), leeftijd(TMS,TM2), HDG, Schemasoort, OrigMsg, ToernooiId, Geld),ja), !.
updateonderdeelId(_, _,_).

/*
cleanonderdeel(WC, Fact) :-
  gebruiktOnderdeel(WC),
  logf('Z', Fact, ja), !.
cleanonderdeel(_WC, Fact) :-
  logf('R', Fact, ja).
*/
updateveld(In1, In2, In2) :-
  not(fronttoken(In1, _, _)), !.
updateveld(In1, _In2, In1).

edgmatch(g,d) :- !.
edgmatch(d,g) :- !.
edgmatch(X,X).


% voor controle etc.
% ander onderdeelid zelfde schemasoort
/*loadlineA(wc(_, OnderdeelIDN, _, _, _, _, _, _, _, _, _, _, reserve(List), _, _), _Tnr) :- 
  List = [Msg|_],
  searchstring(Msg,"0011",_FoundPos),
  wc(WC, OnderdeelIDN, Nr, Naam, EDG, Kort, SexL, SpStrk,LftV, LftTM, HDG, SchemasoortN, _, _ToernooiId, Geld),
  cleanonderdeel(WC, wc(WC, "", Nr, Naam, EDG, Kort, SexL, SpStrk,LftV, LftTM, HDG, SchemasoortN, vanzelf, "", Geld)), !.
loadlineA(wc(_, _OnderdeelIDN, _, _, _, _, _, _, _, _, _, _, reserve(List), _, _), _Tnr) :- 
  List = [Msg|_],
  searchstring(Msg,"0011",_FoundPos), !.  % is al weg
*/
loadlineA(wc(_, OnderdeelID, _N, _, EDGin, _, SexL, SpStrk, _LftV, _LftTM, HDG, _SchemasoortN, _OrigMsg, _Toernooiid, _), _) :- 
  vertaalSexNrs(SexL, SexL1),
  wc(WC, OnderdeelID, _, _Naam, EDGzelf, _Kort, SexL1, SpStrk,_, _, HDG, _, _, _, _Geld), 
  edgmatch(EDGin, EDGzelf),
  %logf('Z',wc(WC, OnderdeelID, N, Naam, EDGzelf, Kort, SexL1, SpStrk,LftV, LftTM, HDG, SchemasoortN, OrigMsg, ToernooiId, Geld),ja), % oude herhalen voor zelfde volgorde
  assert(tempwc(WC)),
  !.
/*loadlineA(wc(_, OnderdeelID, N, Naam, EDG, Kort, SexL, SpStrk, LftV, LftTM, HDG, Schemasoort, Orig, Toernooiid, Geld), _Tnr) :- 
  vertaalSexNrs(SexL, SexL1),
  uniekNaam(Naam, Kort),
  %uniekKort(Kort, Kort1),
  numb(WcX),
  WcX1 = WcX + 1,
  not(wc(WcX1, _, _, _, _, _, _, _, _, _, _, _, _, _,_)), !,
  logf('Z',wc(WcX1, OnderdeelID, N, Naam, EDG, Kort, SexL1, SpStrk, LftV, LftTM, HDG, Schemasoort, Orig, ToernooiId,Geld),ja),
  assert(tempwc(WCX1)), !.
*/
loadlineA(sp2(Tijdelijk,TSexe,_Naamx,Telefoon,_,RatingE,RatingD,_,_,Verenigingsnummer,_,_,Bondsnummer,SterkteE,SterkteD,Gebdatum,
              TelA, TelM,Email,_,_,RangE,RangD,_,_,_,_), _Tnr) :- 
  sp2(SpNo,Sexe,Naam,TelH1,Verhinderingen,RatingE1,RatingD1,Betaald,Uitkantoor0,Vernr1,Adres0,Woonpl0,Bondsnummer,SterkteE1,SterkteD1,Gebdatum1,
             TelA1,TelM1,Email1,Opm0,Tshirt,RangE1,RangD1,Bank0,Weetniet0,IncassoOK,Slist), !,
  assert(matchSp(Tijdelijk, SpNo)),
  updateveld(Telefoon, TelH1, TelHU), 
  updateveld(Email, Email1, EmailU),  
  updateveld(TelA, TelA1, TelAU),
  updateveld(TelM, TelM1, TelMU),
  updateveld(Verenigingsnummer,Vernr1,VernrU),
  updateveld(SterkteE, SterkteE1, SterkteEU),
  updateveld(SterkteD, SterkteD1, SterkteDU),
  updateveld(RatingE, RatingE1, RatingEU),
  updateveld(RatingD, RatingD1, RatingDU),
  updateveld(RangE, RangE1, RangEU), 
  updateveld(RangD, RangD1, RangDU),
  splitGeb(Gebdatum1, Gebdatum2Y, Pasp, _),
  updateveld(Gebdatum, Gebdatum2Y, Gebdatum1U),
  addPaspoort(Gebdatum1U, Pasp, Gebdatum1UY),
  vertaalSexe(TSexe,Sexe),
  not(ongeldig(RatingE,RatingD,SterkteE,SterkteD)),
  not(sp2(SpNo,Sexe,Naam,TelHU,Verhinderingen,RatingEU,RatingDU,Betaald,Uitkantoor0,VernrU,Adres0,Woonpl0,Bondsnummer,SterkteEU,SterkteDU,Gebdatum1UY,
              TelAU, TelMU,EmailU,Opm0,Tshirt,RangEU,RangDU,Bank0,WeetNiet0,IncassoOK, Slist)),
  logf('Z',sp2(SpNo,Sexe,Naam,TelHU,Verhinderingen,RatingEU,RatingDU,Betaald,Uitkantoor0,VernrU,Adres0,Woonpl0,Bondsnummer,SterkteEU,SterkteDU,Gebdatum1UY,
              TelAU, TelMU,EmailU,Opm0,Tshirt,RangEU,RangDU,Bank0,WeetNiet0,IncassoOK, Slist),ja).       
loadlineA(sp2(Tijdelijk,TSexe,Naam,Telefoon,Verhinderingen,RatingE,RatingD,BetaaldNee,Uitkantoor0,Verenigingsnummer,Adres0,Woonpl0,Bondsnummer,SterkteE,SterkteD,Gebdatum,
              TelefoonA, TelefoonM,Email,Opm0,Tshirt,RangE,RangD,Bank0,WeetNiet0,Log,_DeelnemerIdInList), _Tnr) :- 
  not(sp2(_,_,_,_,_,_,_,_,_,_,_,_,Bondsnummer,_,_,_,_,_,_,_,_,_,_,_,_,_,_)),
  vertaalSexe(TSexe,Sexe),
  getbacktrack(Here),
  loglock(lees, "Spelersel"),
  logclose(),
  repeat,
  random(30000,Y),       % bepaal vrij nummer voor nieuwe speler
  SpKeus = Y + 1,
  not(sp2(SpKeus,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)),
  cutbacktrack(Here),
  assert(matchSp(Tijdelijk, SpKeus)), !,
  logf('Z',sp2(SpKeus,Sexe,Naam,Telefoon,Verhinderingen,RatingE,RatingD,BetaaldNee,Uitkantoor0,Verenigingsnummer,Adres0,Woonpl0,Bondsnummer,SterkteE,SterkteD,Gebdatum,
              TelefoonA, TelefoonM,Email,Opm0,Tshirt,RangE,RangD,Bank0,WeetNiet0,Log, []),ja).       
loadlineA(opg(_Tijdstp,_TpWC,TgnstTijdlk,_,_,_,_,_,TeamId,Status,_), _Tnr) :-
  tgnst2tgnst(TgnstTijdlk, Tgnst),   % is ordered voor dubbel
  teamid2Cat(Teamid, WC1, Opg),
  opg(Opg,WC1,TgnstX,Plts,Ronde,Uitsluiten,EenNr,LLWC,_,_,Melding), 
  matchtgnst(Tgnst, TgnstX), !,
  not(opg(Opg,WC1,TgnstX,_,_,_,_,_,_,Status,_)),!,
  logf('Z',opg(Opg, WC1 ,TgnstX,Plts,Ronde,Uitsluiten,EenNr,LLWC,TeamId,Status,Melding),ja).
loadlineA(opg(_Tijdstip,_TmpWC,_TgnstTijdlk,_,_,_,_,_,_TeamId,msg(_, "0033", _,_, _, _),_), _Tnr) :- !. % maak geen nieuwe want is een delete
loadlineA(opg(_Tijdstip,_TmpWC,_TgnstTijdlk,_,_,_,_,_,_TeamId,msg(_, "0034", _,_, _, _),_), _Tnr) :- !. % maak geen nieuwe als al ingedeeld
%  tempWC(WC),
%  pos(WC, _,_,_), !.
loadlineA(opg(_Tijdstip,_TmpWC,TgnstTijd,_,_,_,_,_,TeamId,Status,OnderdeelId), _Tnr) :-
  tempWC(WC),
  WC <> 0,
  not(teamid2cat(TeamId, _, _)),
  tgnst2tgnst(TgnstTijd, Tgnst) ,
  not(staanalopg(WC, Tgnst)),
  getbacktrack(Here),
  date(Year,Month,Day),
  time(Hours,Minutes,_Seconds,_Hundredths),
  dt_min_to_offset(Year, Month, Day, Hours, Minutes, Nr),
  status2WC(WC, Status, WC1, Uitsluiting),
  numb(N),     
  Opg = Nr + N - 1,
  not(opg(Opg,WC1,_,_,_,_,_,_,_,_,_)),
  cutbacktrack(Here),
  logf('Z',opg(Opg, WC1 ,Tgnst,0,0,Uitsluiting,0,"",TeamId,Status,OnderdeelId),ja),!.
loadlineA(klad(N, TTijd, Tekst),_) :-
  logf('Z',klad(N,TTijd, Tekst),ja), !.
loadlineA(functionaris(FType,Tid, Naam,Email,Tel1,Tel2,Tel3),_) :-
  logf('Z',functionaris(FType,Tid, Naam,Email,Tel1,Tel2,Tel3),ja), !.
loadlineA(club(Nr,Naam,Distr),_) :-
  writedevice(X),
  trim_c(Nr),
  not(isalclub(Nr,Naam,Distr,X)), !,
  logf('Z',club(Nr,Naam,Distr),nee).
loadlineA(inschrijfcheck(_,Tid,_, Nr),_) :-	% dit is voor slag één
  api_getComputername(PCname),
  date(Year,Month,Day),
  time(Hours,Minutes,_Seconds,_Hundredths),
  dt_min_to_offset(Year, Month, Day, Hours, Minutes, Tijdstip),
  logf('A',inschrijfcheck(PCname, Tid,Tijdstip, Nr),nee), !. 

isalclub(Nr,_Naam,_Distr,_X) :-
/*  writedevice(screen),
  write("\nNr: <",Nr,"> naam: <", Naam, "> distr: <", Distr,">"),
  club(Nr, NaamX, DistrX), 
  write("\nNrX: ",Nr," naam: <", NaamX, "> distr: <", DistrX,">"),
  writedevice(X),*/
  club(Nr,_,_), !.
/*isalclub(Nr,Naam,Distr,X) :-
  writedevice(screen),
  write("\nNot  Nr: ",Nr," naam: <", Naam, "> distr: <", Distr,">"),
  writedevice(X),
  fail.*/
  
tgnst2tgnst(s(TI), s(TU)) :-
  matchSp(TI, TU), !.
tgnst2tgnst(p(T1,T2),Uit) :-
  matchSp(T1, TU1),
  matchSp(T2, TU2),
  orderedTgnst(p(TU1, TU2), Uit), !.

geenPartnerformulieren(PTekst) :-
  findall(Nr, geenPartnerFormulier(Nr), NList),
  count(NList, 0, Count),
  Count > 0, !,
  format(PTekst, "Er zijn %u zonder formulier, zie Personen, Speciale selectie.", Count).
geenPartnerformulieren("").
 
  dlg_opgaven_Create(_Parent):-
	inschrWin(_Win),
        dlg_opgaven_refresh(0), !.
  dlg_opgaven_Create(Parent):-
	win_CreateDynDialog(Parent,
		[
%BEGIN Opgaven, WinDefList, 14:13:49-16.4.2015, Code automatically updated!
		 dlg_font(wdef(dlg_opgaven_DlgType,dlg_opgaven_RCT,dlg_opgaven_Title,u_DlgBase),
		 	  dlg_opgaven_Font,dlg_opgaven_FSize,dlg_opgaven_Flags),
		 ctl(wdef(wc_LBox,rct(2,10,378,204),"",u_DlgBase),idc_opgaven,[wsf_OwnerDraw,wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_HasStrings,wsf_ExtendedSel]),
		 ctl(wdef(wc_PushButton,rct(384,80,428,92),"&(ver)Plaats",u_DlgBase),idc_plaatsen,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(384,94,428,106),"&Uitsluiten",u_DlgBase),idc_uitsluiten,[wsf_Group,wsf_TabStop,wsf_Disabled]),
		 ctl(wdef(wc_PushButton,rct(61,207,101,219),"Aantallen",u_DlgBase),idc_aantallen,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(255,207,314,219),"S&ynchroniseren",u_DlgBase),idc_ophalen,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(317,207,376,219),"Sy&noniemen",u_DlgBase),idc_maken,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(383,206,430,227),"&Sluiten",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(148,231,176,243),"&Invoer",u_DlgBase),idc_invoer,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(181,231,209,243),"&Uitvoer",u_DlgBase),idc_export,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(213,231,241,243),"&Alle",u_DlgBase),idcw_alle,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(245,231,273,243),"&Geen",u_DlgBase),idcw_geen,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(382,190,429,200),"&Doublures!",u_DlgBase),idc_doublures,[wsf_Group,wsf_TabStop,wsf_Invisible]),
		 ctl(wdef(wc_PushButton,rct(384,248,424,260),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(8,231,48,243),"Allen",u_DlgBase),idc_formall,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(296,229,363,241),"Alles &Opnieuw! ...",u_DlgBase),idc_opnieuw,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_Text,rct(28,2,53,11),"Datum",u_DlgBase),idct_datum,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(67,2,103,11),"Onderdeel",u_DlgBase),idct_onderdeel,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(147,2,172,11),"Speler",u_DlgBase),idct_naam,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(290,2,318,11),"Partner",u_DlgBase),idct_partner,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(5,211,59,219),"Totaal",u_DlgBase),idct_aantal,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(381,111,431,131),"NB Uitsluiten = in de wacht zetten.",u_DlgBase),idct_uitsluiten,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(228,210,253,218),"Klik op:",u_DlgBase),idct_klik,[wsf_AlignLeft,wsf_Invisible]),
		 ctl(wdef(wc_Text,rct(380,10,433,76),"",u_DlgBase),idct_laat,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(136,250,302,259),"NB",u_DlgBase),idct_nb,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(52,233,109,243),"(met zoekfunctie)",u_DlgBase),idct_nb1,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(381,144,429,178),".. partners",u_DlgBase),idct_partners_zonder_formulier,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(381,180,429,189),"Mogelijke",u_DlgBase),idct_er_zijn,[wsf_AlignLeft,wsf_Invisible]),
		 ctl(wdef(wc_Text,rct(116,2,126,11),"Nr",u_DlgBase),idc_opgaven_32,[wsf_AlignLeft]),
		 ctl(wdef(wc_GroupBox,rct(143,221,276,246),"Opgavebestanden uitwisselen (XML)",u_DlgBase),idc_bestanden,[]),
		 ctl(wdef(wc_GroupBox,rct(3,221,112,246),"Internetformulier(en) inkijken",u_DlgBase),idc_formulieren,[]),
		 ctl(wdef(wc_GroupBox,rct(379,134,433,203),"Partners...",u_DlgBase),idc_er_zijn,[])
%END Opgaven, WinDefList
		],dlg_opgaven_eh,0).

%MARK Opgaven, new events


%BEGIN Opgaven, idc_opnieuw _CtlInfo
  dlg_opgaven_eh(_Win,e_Control(idc_opnieuw,_CtrlType,_CtrlWin,_CtlInfo),0):-
	1 = dlg_MessageBox( "Opnieuw invoeren", "Je kunt de opgaven opnieuw invoeren.\nDe bestaande opgaven worden verwijderd,\nde spelergegevens zelf blijven bestaan.\nBetaling wordt gereset.\n\nDoorgaan?\n\nNB Handmatige invoer moet ook opnieuw!", mesbox_iconexclamation, mesbox_buttonsokcancel, mesbox_defaultfirst, mesbox_suspendapplication ),
	opgavenopnieuw(),
	dlg_opgaven_refresh(0),
	win_PostEvent(_Win, e_User(id_kijk, 0)),
	!.
%END Opgaven, idc_opnieuw _CtlInfo

%BEGIN Opgaven, idc_opgaven selchanged
  dlg_opgaven_eh(_Win,e_Control(idc_opgaven,_CtrlType,_CtrlWin,selchanged),0):-
  	setButtons(_Win, b_true),
	!.
%END Opgaven, idc_opgaven selchanged

%BEGIN Opgaven, idc_doublures _CtlInfo
  dlg_opgaven_eh(_Win,e_Control(idc_doublures,_CtrlType,_CtrlWin,_CtlInfo),0):-
	findall(Namen, koppeldubbel(_OpgId, Namen), NamenList),
	sortstring_c(NamenList, 1),
	list_to_string(NamenList, "\n", Denamen),
	write(Denamen),
	removeDubl(NamenList, [], RNamen),
	list_to_string(["Hebben samen voor meer dan één onderdeel ingeschreven:\n" |RNamen], "\n", Lijst),
	format(ResMsg, "%s\n\nDat kan een vergissing zijn. Svp checken!", Lijst),
	_A = dlg_MessageBox( "Mogelijke doublure", ResMsg, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	!.
%END Opgaven, idc_doublures _CtlInfo

%BEGIN Opgaven, e_Update
  dlg_opgaven_eh(_Win,e_Update(_UpdateRct),0):-
	Font = win_GetFont(_Win),
	_FontName = font_GetAttrs(Font, Style, Size),
	%write("\n", FontName),
	Siz1 = Size - 2,
	NewFont = font_SetAttrs(Font, Style, Siz1),
	UWin = win_GetCtlHandle(_Win, idct_uitsluiten),
	TWin = win_GetCtlHandle(_Win, idct_aantal),
	KWin = win_GetCtlHandle(_Win, idct_klik),
	NWin = win_GetCtlHandle(_Win, idct_laat),
	BWin = win_GetCtlHandle(_Win, idct_nb),
	CWin = win_GetCtlHandle(_Win, idct_nb1),
	PWin = win_GetCtlHandle(_Win, idct_partners_zonder_formulier),
	DWin = win_GetCtlHandle(_Win, idc_doublures), 
	VWin = win_GetCtlHandle(_Win, idct_er_zijn), 
	win_SetFont(UWin, NewFont),
	win_SetFont(TWin, NewFont),
	win_SetFont(KWin, NewFont),
	win_SetFont(NWin, NewFont),
	win_SetFont(BWin, NewFont),
	win_SetFont(CWin, NewFont),
	win_SetFont(PWin, NewFont),
	win_SetFont(DWin, NewFont),
	win_SetFont(VWin, NewFont),
	win_SetText(NWin, "NB Als iemand\nzich terugtrekt, verwijder dan niet de betreffende opgave(n) (deelname), maar gebruik de knop \"Uitsluiten\"."),
	win_SetText(BWin, "NB Ook spelergegevens-XML kan hier ingevoerd worden!"),
	geenPartnerformulieren(PTekst),
	win_SetText(PWin, PTekst),
	LboxWin = win_GetCtlHandle(_Win, idc_opgaven),
	NoOfItems = lbox_CountAll(LboxWin),
	AWin = win_GetCtlHandle(_Win, idct_aantal),
	format(AT, "Totaal: %u opg.", NoOfItems),
	win_SetFont(AWin, NewFont),
	win_SetText(AWin, AT),
	selecteerEen(LboxWin),
	setButtons(_Win, b_True),
	!.
%END Opgaven, e_Update

%BEGIN Opgaven, idc_invoer _CtlInfo
  dlg_opgaven_eh(_Win,e_Control(idc_invoer,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	getfolder(Dir,_,_,_),
	_Filenaam = dlg_GetFileName("*.xml",["xmlbestand","*.xml","alle","*.*"],"Selecteer een bestand met XML-Inschrijfgegevens",[dlgfn_MultiSel],Dir,FileNamen),
	%Filenamen = ["C:\\Users\\janzelf\\Documents\\inschrijvingenbond.xml"],
	xml_inschrijfInvoerExpat(FileNamen, 0),
	win_Invalidate(_Win),
	!.
%END Opgaven, idc_invoer _CtlInfo

%BEGIN Opgaven, idc_formall _CtlInfo
  dlg_opgaven_eh(_Win,e_Control(idc_formall,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	xml_controlelijst(_Win),
	!.
%END Opgaven, idc_formall _CtlInfo

%BEGIN Opgaven, idc_maken _CtlInfo
  dlg_opgaven_eh(_Win,e_Control(idc_maken,_CtrlType,_CtrlWin,_CtlInfo),0):-
	win_Destroy(_Win),
	TaskWin = vpi_GetTaskWin(),
	win_spelersel_Create(TaskWin, 0),
	dlg_MessageBox( "Synoniemen", "Voeg de gegevens samen van een speler die onder twee verschillend gespelde namen voorkomt.\nSelecteer zo'n speler en gebruik het samenvoeg (synoniemen)-icoon.", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	!.
%END Opgaven, idc_maken _CtlInfo

%BEGIN Opgaven, idc_ophalen _CtlInfo
  dlg_opgaven_eh(_Win,e_Control(idc_ophalen,_CtrlType,_CtrlWin,_CtlInfo),0):-
        xml_InschrijfInvoerNieuw(),
        dlg_opgaven_refresh(0), !.
%END Opgaven, idc_ophalen _CtlInfo

%BEGIN Opgaven, idc_aantallen _CtlInfo
  dlg_opgaven_eh(_Win,e_Control(idc_aantallen,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	financieel(),
	%write(Rapport),
	%get_TempDir(_, TempFile),
	%file_str(TempFile, Rapport),
	%dlg_rapport_Create(_Win, "Aantallen", "aant",b_False), 
	!.
%END Opgaven, idc_aantallen _CtlInfo

%BEGIN Opgaven, idc_uitsluiten _CtlInfo
  dlg_opgaven_eh(_Win,e_Control(idc_uitsluiten,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idc_opgaven),	% flip uitsluiten
	lbox_GetSel(LboxWin,ItemList,_IndexList),
	ItemList = [Str0|_],
	parsetabs(Str0, Vals),
	Vals = [_, TijdS, R2 | _],
	str_int(TijdS, Tijd),
	str_int(R2, Cat),
	opg(Tijd,Cat,Tgnst,Plts,Ronde,U,Res,LL,TeamId,Keur,Melding),
	bitxor(U, 1, U1),
	logf('Z', opg(Tijd,Cat,Tgnst,Plts,Ronde,U1,Res,LL,TeamId,Keur,Melding), nee),
	win_Invalidate(LboxWin),
	!.
%END Opgaven, idc_uitsluiten _CtlInfo

%BEGIN Opgaven, idcw_geen _CtlInfo
  dlg_opgaven_eh(_Win,e_Control(idcw_geen,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idc_opgaven),	% refresh
	selectgeen(LboxWin),
	!.
%END Opgaven, idcw_geen _CtlInfo

%BEGIN Opgaven, idcw_alle _CtlInfo
  dlg_opgaven_eh(_Win,e_Control(idcw_alle,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idc_opgaven),	% refresh
	selectalle(LboxWin),
	!.
%END Opgaven, idcw_alle _CtlInfo

%BEGIN Opgaven, e_Destroy
  dlg_opgaven_eh(_Win,e_Destroy,0):-
	updateVensterpositie(opgavenvenster, _Win),
	retractall(hotspotO(_, _, _)),
	retractall(inschrWin(_)),
	!.
%END Opgaven, e_Destroy

%BEGIN Opgaven, e_User
  dlg_opgaven_eh(_Win,e_User(id_refresh, _Ptr),0):-
	LboxWin = win_GetCtlHandle(_Win, idc_opgaven),	% refresh
	lbox_GetSel(LboxWin,_ItemList,IndexList),
	%ItemList = [Str0],
	count(IndexList, 0, AantalSelected),
	AantalSelected < 2,
	%Index0 = getSelected(IndexList),
	%IndexList = [Index0|_],	
	LboxList = lbox_GetAll(LboxWin),
	findall(Tijdx, opg(Tijdx,_,_,_,_,_,_,_,_,_,_), Tijdenx),
	sorttijd_c(Tijdenx),
	removeDubbels(Tijdenx, [], TijdenSx),
	findall(Opg, opgaveL(Opg, TijdenSx), OpgList),
	retractall(hoogsteopgave(_)),
	not(LboxList = OpgList),	% base list is veranderd
	lbox_Clear(LboxWin),
	lbox_Add(LboxWin, OpgList),
	selecteerEen(LboxWin),
	setButtons(_Win, b_True),
	!.
  dlg_opgaven_eh(_Win,e_User(id_kijk, _Ptr),0):-
	overgeslagen(List),
	%write("\n", List),
	count(List, 0, Aantal),
	%write("\nAantal = ", Aantal),
	Aantal > 0,
	KWin = win_GetCtlHandle(_Win, idct_klik),
	win_SetState(KWin, [wsf_Visible]),
	format(Msg, "Er zijn nog %u opgaven op internet niet verwerkt.\nKlik op synchroniseren om dat te starten.", Aantal),
	_Answer = dlg_MessageBox( "Opgaven", Msg, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	!.
  dlg_opgaven_eh(_Win,e_User(id_koppeldubbel, _Ptr),0):-
	findall(OpgId, koppeldubbel(OpgId, _), OpgIdList),
	not(OpgIdList = []),
	%EWin = win_GetCtlHandle(_Win, idc_er_zijn),
	%win_SetState(EWin, [wsf_Visible]),
	DWin = win_GetCtlHandle(_Win, idc_doublures),
	win_SetState(DWin, [wsf_Visible]),
	PWin = win_GetCtlHandle(_Win, idct_er_zijn),
	win_SetState(PWin, [wsf_Visible]), !.

%END Opgaven, e_User

%BEGIN Opgaven, idc_plaatsen _CtlInfo
  dlg_opgaven_eh(_Win,e_Control(idc_plaatsen,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idc_opgaven),	% details
	lbox_GetSel(LboxWin,ItemList,_IndexList),
	ItemList = [Str0|_],
	parsetabs(Str0, Vals),
	Vals = [_, TijdS, R2 | _],
	%fronttoken(Str0, _Mark, Str),
	%fronttoken(Str,TijdS,R1),
	str_int(TijdS, Tijd),
	%fronttoken(R1,R2,_R3),
	str_int(R2, Cat),
	dlg_plaatsing_Create(_Win, Cat, Tijd), %
	win_PostEvent(_Win,e_User(id_refresh, 0)),
	!.
%END Opgaven, idc_plaatsen _CtlInfo

%BEGIN Opgaven, idc_help _CtlInfo
  dlg_opgaven_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Opgaven"),
	!.
%END Opgaven, idc_help _CtlInfo

%BEGIN Opgaven, e_OwnerMeasureItem
  dlg_opgaven_eh(_Win,e_OwnerMeasureItem(_CtlType,_CtlId,_ItemId,_Data),Size):-!,
	Font = win_GetFont(_Win),
	_FontName = font_GetAttrs(Font, Style, _Size),
	NewFont = font_SetAttrs(Font, [fs_Bold | Style], 0),
	win_GetTextExtent(_Win, "JansenenTilanusg", -1, Width, He),
	Height = He,
	bitleft(Height, 16, H1),
	Size = Width + H1,
	win_SetFont(_Win, NewFont),
	win_GetTextExtent(_Win, "! U", -1, WProbl, _),
	win_SetFont(_Win, Font),
	win_GetTextExtent(_Win, " X ", -1, WMark, _),
	maxOpgTijd(_Win, WDatum),
	%win_GetTextExtent(_Win, "31/12 22:22 ", -1, WDatum, _),
	maxondkort(_Win, WShort),
	%win_GetTextExtent(_Win, "pHEAjr ", -1, WShort, _),
	win_GetTextExtent(_Win, "MMM999 ", -1, WOID, _),
	WOID1 = WOID - 6,
	assert(iafm(WProbl, WMark, WDatum, WShort, WOID1)),
	!.
%END Opgaven, e_OwnerMeasureItem

%BEGIN Opgaven, idc_export _CtlInfo
  dlg_opgaven_eh(_Win,e_Control(idc_export,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idc_opgaven),
	lbox_GetSel(LboxWin,ItemList,IndexList),
	boodschapAantal(IndexList),	% boodschap bij maaréén geselecteerd
	xml_inschrijfExp(_Win, ItemList),
	trap(CW = win_GetCtlHandle(_Win, idc_cancel),_,fail),
	win_SendEvent(_Win,e_Control(idc_cancel,wc_PushButton,CW, activated())),
	!.
%END Opgaven, idc_export _CtlInfo

%BEGIN Opgaven, e_OwnerDraw
  dlg_opgaven_eh(_Win,e_OwnerDraw(_CtlType,_CtlId,_ItemId,_ItemAction,
		ItemState,LboxWin,RectItem,_ItemData),0):-
	own_member1(ItemState, odstate_Selected), !,  % was selected
	win_SetPen(LboxWin,pen(1,ps_Solid,color_Blue)),
	win_SetBrush(LboxWin,brush(pat_Solid, color_Blue)),
	draw_Rect(LboxWin,RectItem),
	myDraw(LboxWin, _ItemId, RectItem, color_White, color_Blue, b_True),
	!.
  dlg_opgaven_eh(_Win,e_OwnerDraw(_CtlType,_CtlId,_ItemId,_ItemAction,
		_ItemState,LboxWin,RectItem,_ItemData),0):-!,
	_Itemstate = [],				% het normale geval
	win_SetPen(LboxWin,pen(1,ps_Solid,color_White)),
	draw_Rect(LboxWin,RectItem),
	myDraw(LboxWin, _ItemId, RectItem, color_Black, color_White, b_False),
	!.
%END Opgaven, e_OwnerDraw

%BEGIN Opgaven, e_Create
  dlg_opgaven_eh(_Win,e_Create(_CreationData),0):-
	retractall(hotspotO(_, _, _)),
	dialoogpositie(opgavenvenster, _Win),	% als bewaard van de vorige keer
	Font = win_GetFont(_Win),
	assert(inschrWin(_Win)),
	win_PostEvent(_Win,e_User(id_kijk, 0)),
	findall(Tijd, opg(Tijd,_,_,_,_,_,_,_,_,_,_), Tijden),
	sorttijd_c(Tijden),
	removeDubbels(Tijden, [], TijdenS),
	findall(Opg, opgaveL(Opg, TijdenS), OpgList),
	retractall(hoogsteopgave(_)),
	LboxWin = win_GetCtlHandle(_Win, idc_opgaven),
	win_SetSubclassHandler(LboxWin,subclasso_eh,b_false),	% 11.12.2002 ???
	win_SetFont(Lboxwin, Font),
	lbox_Add(LboxWin, OpgList),
	findTop(0, OpgList, Top),
	lbox_SetTopIndex( LboxWin, Top ),
	lbox_SetSel(LboxWin, Top, b_True),
	%write("\nTop= ", Top),
	setbuttons(_Win, b_True),
	fail.
  dlg_opgaven_eh(Win,e_Create(_CreationData),0):-
	win_PostEvent(Win, e_User(id_koppeldubbel, 0)),
	metXML(b_True, Win),
	!.
%END Opgaven, e_Create

%BEGIN Opgaven, idc_cancel _CtlInfo
  dlg_opgaven_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	win_Destroy(_Win),
	!.
%END Opgaven, idc_cancel _CtlInfo

  dlg_opgaven_eh(_,_,_):-!,fail.

%END_DLG Opgaven

%      Wprobl      WMark        WDatum      WShort         Rest/2     
% Uitroep    Mark      Datum        Short      Speler/OID     Partner/OID

isUitgesloten(b_False, "U", _In, color_blue) :- !.
isUitgesloten(_, _, In, In).

myDraw(LboxWin, _ItemId, RectItem, KleurVIn, KleurA, Selected) :-
	Parent = win_GetParent(LboxWin),
	RectItem = rct(_L, T, R, B),
	Item = lbox_GetItem(LboxWin, _ItemId),	% get the string
	itemRepr(Item, _Repr, Datum, Short, SNaam, PNaam, Mark, Uitsl, OIDspeler, C1, OIDpartner, C2, _SOpg, _POpg),
	isProbleem(Item, Uitroep),
	isUitgesloten(Selected, Uitsl, KleurVIn, KleurV),
	iafm(WProbl, WMark, WDatum, WShort, WOID),
	Font = win_GetFont(Parent),
	_FontName = font_GetAttrs(Font, Style, Size),
	Siz1 = Size - 3,
	NewFont = font_SetAttrs(Font, Style, Siz1),
	MarkFont = font_SetAttrs(Font, [fs_Bold | Style], 0),
	RectItem0 = rct(0, T, Wprobl, B),
	win_SetForeColor(LboxWin, color_red),
	win_SetBackColor(LboxWin, color_white),
	format(KenM, "%s %s", Uitroep, Uitsl),
	trim_c(KenM),
	win_SetFont(LboxWin, MarkFont),
	draw_TextInRect(LboxWin,RectItem0, KenM, -1, [dtext_center]),
	M0 = WProbl + WMark,
	RectItem1 = rct(WProbl, T, M0, B),
	win_SetForeColor(LboxWin, KleurV),
	win_SetBackColor(LboxWin, KleurA),
	draw_TextInRect(LboxWin,RectItem1, Mark, -1, [dtext_center]),
	M1 = M0 + WDatum + 1,
	RectItem2 = rct(M0, T, M1, B),
	win_SetFont(LboxWin, Font),
	draw_TextInRect(LboxWin,RectItem2, Datum, -1, [dtext_left]),
	M2 = M1 + Wshort + 1, 
	WNaam = ((R - M2) div 2) - WOID - 2,
	RectItem3 = rct(M1, T, M2, B), 
	markUitsl(Uitsl, Selected, LboxWin, RectItem3),
	draw_TextInRect(LboxWin,RectItem3, Short, -1, [dtext_left]),
	%
	M3 = M2  + WOID + 2,
	T1 = T + 3,
	RectItem5 = rct(M2, T1, M3, B),
	win_SetFont(LboxWin, NewFont),
	win_SetBackColor(LboxWin, C1),
	win_setForeColor(LboxWin, color_Black),
	draw_TextInRect(LboxWin,RectItem5, OIDspeler, -1, [dtext_center]),
	updateHotSpot(opgavenS, RectItem5, Item, OIDspeler),
	win_SetBackColor(LboxWin, KleurA),
	win_SetForeColor(LboxWin, KleurV),
	%
	M4 = M3 + Wnaam + 1, 
	RectItem4 = rct(M3, T, M4, B), 
	win_SetFont(Lboxwin, Font),
	draw_TextInRect(LboxWin,RectItem4, SNaam, -1, [dtext_left]),
	updateHotSpot(nrS, RectItem4, Item, SNaam),
	%
	M5 = M4 + WOID + 1,
	RectItem7 = rct(M4, T1, M5, B),
	win_SetFont(Lboxwin, NewFont),
	win_SetBackColor(LboxWin, C2),
	win_setForeColor(LboxWin, color_Black),
	draw_TextInRect(LboxWin,RectItem7, OIDpartner, -1, [dtext_center]),
	updateHotSpot(opgavenP, RectItem7, Item, OIDpartner),
	win_SetBackColor(LboxWin, KleurA),
	win_SetForeColor(LboxWin, KleurV),
	win_SetFont(Lboxwin, Font),
	%
	RectItem6 = rct(M5, T, R, B),
	win_SetFont(Lboxwin, Font),
	draw_TextInRect(LboxWin,RectItem6, PNaam, -1, [dtext_left]),
	updateHotSpot(nrP, RectItem6, Item, PNaam).

setButtons(_Win, b_true) :-
	PltsWin = win_GetCtlHandle(_Win, idc_plaatsen),
	UWin = win_GetCtlHandle(_Win, idc_uitsluiten),
	win_SetState(PltsWin, [wsf_Disabled]),
	win_SetState(UWin, [wsf_Disabled]),
	win_SetState(PltsWin, [wsf_Enabled]),
	win_SetState(UWin, [wsf_Enabled]), !.
setbuttons(_Win, _).
