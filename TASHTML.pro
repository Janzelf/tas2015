/*****************************************************************************

		Copyright (c) 1989 - 2005 De Lint Associates

 Project:  TASVP1
 FileName: TASHTML.PRO
 Purpose: No description
 Written by: JdL
 Comments:
******************************************************************************/

include "tasvp1.inc"
include "tasvp1.con"
include "hlptopic.con"
include "datecust\\date_cc.pre"
include "Browser.pre"

constants
cTijdspangrens = 20

domains
bw = direct ; bijwinst; bijverlies
kolomlr = links ; midden ; rechts

database - schemas1
  spelerprogrammaItem(integer Speler, integer WedNo, wedscat, boolean Vastgesteld, integer Tijd, bw Bijwinst, string Baan, string Eerder)
  agendaitem(integer Dag, integer TotaalAantal, integer AantalPlan, string PaginaKnop, integer Reekshoofd, string ReekshoofdKnop, string KnopSec, string RowId)
knop(string Naam, string Link, string TitelSec, string Rowid)
single eerstedagGepasseerd(boolean)
groepslid(wedscat, string LongP, string LongS, wedsex MV, string WT, string Speelsterkte, integer LeeftV, integer LeeftTM) 
single huidigeKolom(kolomlr)
single indexje(integer)
single agendadag(integer)


predicates
procedure zoekHTMLnamen(catl, boolean VerlApart)
determ bovenliggende(wedscat, wedscat)
plakshort(string, string, string)
vertaal(char, string)
determ  verliezersRondeOnder(catl, wedscat WC, boolean VerlApart)
determ  verliezersRondeBoven(wedscat WC, boolean VerlApart)
determ htmlnaamP(wedscat, string, string)  % dus niet echt procedure

clauses
plakShort("", X, X) :- 
  not(htmlnaam(_, X,_)), !.
plakshort("", X, X1) :-
  numb(N),
  str_int(NS, N),
  concat(X, NS, X1),
  not(htmlnaam(_, X1, _)), !.
plakShort(In, X, Uit) :-
  frontchar(In,Char,Rest), 
  vertaal(Char, CharVS),!,
  concat(X, CharVS, X1),
  %frontchar(X1, '-', X),
  plakShort(Rest, X1, Uit).
plakshort(In, X, Uit) :-
  frontchar(In, Char, Rest),
  str_char(CharS, Char),
  concat(X, CharS, X1),
  plakShort(Rest, X1, Uit).

vertaal(' ', "") :- !.
vertaal('+', "p") :- !.
vertaal('/', "") :- !.
vertaal(Char, CharS) :-
  Char >= 'A',
  Char <= 'Z', !,
  str_char(CharS, Char).
vertaal(Char, CharS) :-
  Char >= 'a',
  Char <= 'z', !,
  str_char(CharS, Char).
vertaal(Char, CharS) :-
  Char >= '0',
  Char <= '9', !,
  str_char(CharS, Char).
vertaal(_, "x").

verliezersRondeBoven(WC, b_True) :-	% verliezersronde boven
  kopl(WC, verliezer, _, _), !.
  %wc(_, _,_, _,_,_,WC), !.
verliezersRondeBoven(WC, _) :-		% of geen verliezersronde
  not(kopl(WC, verliezer, _, _)).
  %not(wc(_, _,_, _,_,_,WC)).
  
verliezersRondeOnder(CatL, WC, b_False) :-	% verliezersronde onder
  member(WC, CatL),				% en geselecteerd
  kopl(WC, verliezer, _, _), !.
  %wc(_, _,_, _,_,_,WC), !.
verliezersRondeOnder(_, WC, _) :-	% geen verliezersronde
  not(kopl(WC, verliezer, _, _)).
  %not(wc(_, _,_, _,_,_,WC)).

htmlnaamP(WC, Short1, Link) :-
  htmlnaam(WC, Short1, Link), !.
htmlnaamP(WC, Kort, Link) :-
  kopl(WC, verliezer, WCE, _),
  wc(WCE, _OnderdeeelID,_Orde,  _Naam, _EDG, Kort, _SexL, _SpStrk, _LftV, _LftTM, _HDG, _Schemasoort, _Keuring, _KNLTB, _Geld),
  %wc(WCE, _,_, ShortV,_,_,WC),
  htmlnaam(WCE, _, Link), !.
  
zoekHTMLnamen(_, _) :-
  retractall(htmlnaam(_,_,_)),
  wc(WC, _,_,  _, _, Kort, _, _, _, _, _, _, _, _, _),
  wc(WCX, _,_,  _, _, Kort, _, _, _, _, _, _, _, _, _),
  %wc(WC, _,_, ShortP,_,_,_),
  %wc(WCX, _,_, ShortP,_,_,_),
  WCX <> WC,
  beep(),
  write("\nKorte naam ", Kort, " niet uniek!"),
  fail.
zoekHTMLnamen(CatL, VerlApart) :-
  schemadoel(upload),
  member(WC, CatL),
  verliezersRondeBoven(WC, VerlApart),
  wc(WC, _,_,  _, _, Kort, _, _, _, _, _, _, _, _, _),
  %wc(WC, _,_, ShortP,_,_,_),
  trim_c(Kort),
  plakShort(Kort,"", Short),
  upper_lower(Short, Short1),
  format(Link, "A href=\"sheet.php?plek=xxx&amp;type=sheet&amp;ond=%s\"", Short1),
  assert(htmlnaam(WC, Short1, Link)),
  fail.
zoekHTMLnamen(CatL, VerlApart) :-
  not(schemadoel(upload)),
  member(WC, CatL),
  verliezersRondeBoven(WC, VerlApart),
  wc(WC, _,_,  _, _, ShortP, _, _, _, _, _, _, _, _, _),
  %wc(WC, _,_, ShortP,_,_,_),
  trim_c(Shortp),
  plakShort(Shortp,"", Short),
  upper_lower(Short, Short1),
  random(100, RInt),
  format(Link, "A href=\"%s.html?pr=%u\"", Short1, RInt),
  assert(htmlnaam(WC, Short1, Link)),
  fail.
zoekHTMLnamen(CatL, VerlApart) :-
  wc(WC, _,_,  _, _, _Kort, _, _, _, _, _, _, _, _, _),
  %wc(WC, _,_, _,_,_,_),
  isOndercat(WC),
  verliezersRondeOnder(CatL, WC, VerlApart),
  bovenliggende(WC, WCboven),
  htmlnaam(WCBoven, Short, Link),
  assert(htmlnaam(WC, Short, Link)), 
  fail.
zoekHTMLnamen(_, _).

%apart

bovenliggende(WC, WCB) :-
  kopl(WCB, pl3en4(_), WC, _), !.
bovenliggende(WC, WCB) :-
  koplv(WC, WCB), !.
bovenliggende(WC, WCB) :-
  kopl(WC, verliezer, WCB, _), !.
  %wc(WCB, _,_, _,_,_,WC), !.
bovenliggende(WC, WC) :-
  beep(),
  fail.


predicates
procedure scanSpelerProgramma(catl, integer TmDag, string CSSnaam, boolean ZonderWebLink, integer Tijdspan) - (i,i,i,i,i)
nondeterm splitsSpelers(slist SpelersN, integer SpelerAantal, slist SpelersN1, string VanafLetter, string TotMetLetter)
%procedure testspeler(string Id, integer Spno, slist Namen, integerlist Spelers)
nondeterm splitsSpelers1(slist SpelersN, integer N, integer Portie, slist SpelersIn, slist Uit, string VanafLetter, string TotMetLetter) - (i,i,i,i,o,o,o)
nondeterm geplandeWeds(catl, integer Dag, integer WNum)
procedure groepeer(integerlist Dagenlijst, integer Aantal, integer ReekshoofdDag, integer AfgelopenDag, integer Portie)
  knopref(integer Dag, string, integer Afgelopen, integer AantalPlan, string KnopSec, string RowId)
determ eenvoorHuidig(integer Dag, integer HDag, ulist, integer AantalPlan, boolean Pass)
procedure betrokken(integer, wedscat, tgnst, tgnst, integer, bw, string)
procedure ontdubbel(integerlist, integerlist, integerlist)
nondeterm speler2naam(integerlist, string)
procedure linkSpelers(string StartL, string EndL, string Link)
procedure doorhalen(integer AantalPlan, string In, string Uit)
procedure linkKnop(string DagN, string Tok, string Tok2, integer Rand, string Link, string RowId, String LinkTitle)
procedure eerder(integer, wedscat, tgnst, string)
procedure eerder2(integer, wedscat, tgnst, tgnst, string)
procedure publiceerSpelerProgramma1(ilist SpelerV, shselect SheetSel, catl OnderdelenR, string CSSNaam, boolean ZonderWebLink, string StartChar, string TMChar)
procedure spelernummerlijst(slist, ilist)
procedure bw2str(bw, BOOLEAN, string)
procedure dagHerhaal(integer Dag, string Uit)
nondeterm member2(integer, integerlist, BOOLEAN)
procedure metZonderLink(boolean ZonderWebLink, string DHstr, string TijdS, string OndRef, string Link, integer WNo, wedscat WC, string Shortp, string BaanExtra, string BWS, string Eerder)
procedure tijdstipweergave(integer, string)
nondeterm groepslidMetLink(string MV, wedsex WT, string ShortMetLink, integer LftOnder, integer LftBoven)
nondeterm groepslidMetLinkComp(string MV)
procedure bovenGrens(integer, integer)
determ    sheetSelectorElemAanwezig(integer LftMVan, integer LftMTot, wedsex Geslacht, string WType, string Qualifier)
procedure sheetSelectorElem(shselect, integer LftMVan, integer LftMTot, wedsex Geslacht, string WType, string Titel, boolean MetSpeelsterkte, integer DagTM)
determ    sheetSelectorElemComp(shselect, boolean MetSpeelsterkte, string Titel)
nondeterm omlijstMetTR(shselect, kolomlr)
nondeterm sheetSelectorGroep(integer, integer, wedsex V, string E,  string MeisjesEnkel, kolomlr)
determ    restsheets(shselect, kolomlr)
nondeterm spelereinde
procedure overzichtref(shselect SheetSel, boolean)
nondeterm linksRechts(kolomlr)
procedure baanExtra(string, string)
procedure kortLang(shselect, string Kort, string Lang, string Uit)
determ    ietsBekend(tgnst)
determ    finalebekend1(wedscat)
procedure publiceerBekendeFinale(wedscat WC, boolean MetSpeelsterkte, integer DagTM)
nondeterm diversen(shselect, string Link)
procedure speelsterkteKol(boolean, string Sterk1, string Bonds1, janee Echt, string Br, string Sterk2, string Bonds2, janee Echt2)
procedure combine(slist, slist)
nondeterm gespeeldA(integer)
nondeterm nietgespeeld(integer)
%procedure copyright()
procedure verzamelIndex(shselect SheetSel, catl OnderdelenR, boolean NietAlsApart, boolean MetSpeelsterkte, string IDSuffix) - (i,i,i,i,i)
procedure publiceer34(wedscat, integer DagTM)
determ    isKleinAantal(catl) % afhankelijk van het aantal onderdelen
nondeterm geslachten(wedsex)
nondeterm wtypes(string)
procedure sheetheaders(string, string, string, string, string) - (o,o,o, o, o)
nondeterm zoekOpTitels(string, string) - (o,o)
procedure publiceerFinale(WEDSCAT, integer DagTM) - (i,i)
determ    postHtmlPrime(shselect SheetSel, integer PlanSel, boolean ZonderWebLinks, boolean Spelerwijzer) - (i,i,i,i)
procedure validChar(string, string) - (i,o)
procedure styleSheet(string FileNaam)
%procedure checkStyleSheet(string FileNaam, integer FontGr) - (i,i)
procedure menustr(catl) - (i)
procedure writeStyleSheet(integer FontGr)
procedure expandeer(catl, catl In, catl Uit)
nondeterm subCats(wedscat Cat, wedscat SubCat).

clauses
eersteDagGepasseerd(b_False).
huidigeKolom(links).
indexje(0).
agendadag(0).

agendaitemStr(SDag, Dag) :-
  agendaitem(RDag, _TotaalAantal, _AantalPlan, _Knop, Dag, _RKnop, _, _),
  dag(RDag, SDag, _).

%maakMijnlink(Naam, _, Naam) :-   kan ook geen kwaad
%  not(schemadoel(upload)), !.
maakMijnlink(Naam, Nr, Naam) :-
  not(fronttoken(Nr, _, _)), !.
maakMijnlink(Naam, "&nbsp;", Naam) :- !.
maakMijnlink(Naam, Nr, NaamA) :-
    format(NaamA, "<mijnlink type=\"1\" no=%s>%s</mijnlink>", Nr, Naam). 

publiceerSpelerProgramma(_, _, _, _,  _, _) :-			% niets te melden!	 21.7.2002
  not(spelerprogrammaitem(_, _, _, _, _, _, _, _)), !.
publiceerSpelerProgramma(SheetSel, b_True, OnderdelenR, _TmDag, CSSNaam, ZonderWebLink) :-
  findall(SpR, spelerprogrammaItem(SpR, _, _, _, _, _, _, _), SpelersR),
  sortint_c(SpelersR),
  ontdubbel(SpelersR, [], Spelers),
  %testspeler("na ontdubbel",21392,[], Spelers),
  findall(NaamXy, speler2naam(Spelers, Naamxy), SpelersN),
  %testspeler("na spelernaamlijst", 21392,SpelersN,[]),
  sortstring_c(SpelersN, 1),
  count(Spelers, 0, SpelerAantal),
  %dlg_note("in publiceerspelerprogramma!"),
  splitsSpelers(SpelersN, SpelerAantal, SpelersN1, Start, End),
  spelernummerlijst(SpelersN1, SpelerV),
  publiceerSpelerProgramma1(SpelerV, SheetSel, OnderdelenR, CSSNaam, ZonderWebLink, Start, End),
  fail.
publiceerSpelerProgramma(_, _, _, _, _, _) :-
  schemadoel(upload), !.
publiceerSpelerProgramma(_, _, _, _, _, _) :-
  closefile(work).

toernooiDiv() :-
  naam(TNaam, _, _, _, _, _),
  write("<div class=toernooinaam>",TNaam,"</div>").

publiceerSpelerProgramma1(SpelerV, SheetSel, OnderdelenR, _CSSNaam, ZonderWebLink, StartChar, TMChar) :-
  get_htmlmap(Dir),				% start het html rapport
  format(FNaam, "spelers%s%s.html", StartChar, TMChar),
  filenamepath(FileNaam, Dir, FNaam),
  upper_lower(FDelU, FNaam),
  upper_lower(FileNaam, FileNaamL),
  retractall(teDeleten(FDelU)),
  upper_lower(StartChar, SClower),
  closeOpen(FilenaamL, SClower, "speler"),		% global
  metaTag(Eerst, Tag),
  writef("%s\n<HTML><HEAD>%s<META HTTP-EQUIV=\"Expires\" CONTENT=\"Tue, 24 Oct 1999 17:45:00 GMT\"><TITLE>Spelers %s t/m %s</TITLE>", Eerst, Tag, StartChar, TMChar),
  writef("\n<!-- ToernooiAssistent versie %s -->",versie),
  %styleSheet(CSSNaam),
  write("</HEAD>"),
  write("\n<BODY>"),
  logoerbijTop(),
  toernooiDiv(),
  verzamelIndex(SheetSel, OnderdelenR, b_True, b_True, ""),
  write("\n<!-- hierdescript -->\n<div id=spelers>\n"),
  sheetHeaders(_, _, _, SpH, _),
  writef("<div class=spelertitel>%s %s t/m %s</div>\n", SpH, StartChar, TMChar),
  write("\n<TABLE>"),
  member(Speler, SpelerV),
  assert(agendadag(0)),
  spelereinde(),
  sp2(Speler,_,Naam,_,_,_,_,_,_, _, _, _, _, _, _, _, _, _, _, _,_,_,_,_,_,_,_),
  writef("\n<TR><TD class=overzn style=\"padding-top: 6\">%s</TD>",Naam),
  findall(TijdR, spelerprogrammaItem(Speler, _, _, _, TijdR, _, _, _), Tijden),
  sortint_c(Tijden),
  member2(TijdStip, Tijden, ZelfdeDag),
  retract(spelerprogrammaItem(Speler, WNo, WC, _, TijdStip, BijWinst, Baan, Eerder)),
  wc(WC, _,_,  _, _, ShortP, _, _, _, _, _, _, _, _, _),
  %wc(WC, _,_, Shortp,_,_,_),
  htmlnaamP(WC,OndRef, Link),  % !!!!!!!!!! verliezersronde ??
  bitright(TijdStip, 7, Dag),
  dagherhaal(Dag, DHStr),
  tijdstipweergave(Tijdstip, TijdS),
  bw2str(BijWinst, ZelfdeDag, BWS),
  baanExtra(Baan, BaanExtra),
  metZonderLink(ZonderWebLink, DHstr, TijdS, OndRef, Link, WNo, WC, Shortp, BaanExtra, BWS, Eerder),
  fail.
publiceerSpelerProgramma1(_, SheetSel,  OnderdelenR, _, b_False, _, _) :-
  write("\n</TABLE>"),
  write("</div>"),
  verzamelIndex(SheetSel, OnderdelenR, b_True, b_True, "1"),
  postHtml(b_False),
  fail.
publiceerSpelerProgramma1(_, _,  _, _, b_True, _, _) :-
  postHtml(b_True),
  fail.
publiceerSpelerProgramma1(_, _,  _, _, _ZonderLinks, _, _) :-
  copyright(),
  write("\n</BODY>"),
  write("\n</HTML>\n"),
  fail.
publiceerSpelerProgramma1(_, _, _, _, _, _, _) :-
  schemadoel(upload), !.
publiceerSpelerProgramma1(_, _, _, _, _, _, _) :-
  closefile(work).

scanSpelerProgramma( _, _,  _, _, _) :-
  retractall(spelerprogrammaitem(_, _, _, _, _, _, _, _)),
  retractall(agendaitem(_, _, _, _, _, _, _, _)),
  schemadoel(afdruk), !. 				% geen spelerprogramma als printen
scanSpelerProgramma(CatL2, TmDag,  _, _, Tijdspan) :-
  Tijdspan <= cTijdspangrens,
  assert(eerstedaggepasseerd(b_False)),
  findall(Tijd, w2(_,_,Tijd,_,_,_,_,_), Tijden),
  sorttijd_c(Tijden),
  Tijden = [T|_], 
  bitright(T, 7, HDag),
  findall(Dx, dag(Dx, _, _), DagenX),
  dag(Dag, _, _),
    Dag <= TmDag,
    findall(X, geplandeWeds(CatL2, Dag, X), XList),
    count(XList, 0, AantalPlan),
    eerstedaggepasseerd(Pass),
    eenVoorHuidig(Dag, HDag, DagenX, AantalPlan,Pass),
    assert(eerstedaggepasseerd(b_True)),
    findall(Y, gespeeld(CatL2, Dag, Y, _), YList),
    count(YList, AantalPlan, AantalTotaal),
    AantalTotaal > 0,
    assertz(agendaitem(Dag, AantalTotaal, AantalPlan, "", 0, "", "", "")),
  fail.
scanSpelerProgramma(CatL2, TmDag,  _, _, Tijdspan) :-
  Tijdspan > cTijdspangrens,
  dag(Dag, _, _),
  Dag <= TmDag,
  findall(X, geplandeWeds(CatL2, Dag, X), XList),
  count(XList, 0, AantalPlan),
  findall(Y, gespeeld(CatL2, Dag, Y, _), YList),
  count(YList, AantalPlan, TotaalAantal),
  TotaalAantal > 0,
  assertz(agendaitem(Dag, TotaalAantal, AantalPlan, "", 0, "", "", "")),
  fail.
scanSpelerProgramma(_CatL2, _TmDag,  _, _, Tijdspan) :-
  Tijdspan > cTijdspangrens,					% langer dan twee weken
  findall(Dag,agendaitem(Dag, _, _, _, _, _, _, _), Dagen),
  findall(AantalT, agendaitem(_, AantalT, _, _, _, _, _, _), TL),
  telop(TL, 0, TotaalAantal),
  N = TotaalAantal div 50,
  Portie = (TotaalAantal + N) div (N + 1),
  %Portie = 50,
  Dagen = [StartOudeMaster|_],			% moet namelijk bestaan
  groepeer(Dagen, 100, StartOudeMaster, StartOudeMaster, Portie),
  fail.
scanSpelerProgramma(_CatL2, _TmDag,  _, _, Tijdspan) :-
  Tijdspan <= cTijdspangrens,
  findall(DagX,agendaitem(DagX, _, _, _, _, _, _, _), Dagen),
  member(Dag, Dagen),				% korter dan twee weken
  getbacktrack(Here),
  retract(agendaitem(Dag, TotaalAantal, AantalPlan, _, _Master, _, _, _)),
  cutbacktrack(Here),
  knopRef(Dag, Knop, Dag, AantalPlan, KnopSec, RowId),
  assertz(agendaitem(Dag, TotaalAantal, AantalPlan, Knop, 0, Knop, KnopSec, RowId)),
  fail.
scanSpelerProgramma(CatL2, TmDag,  _, _, _) :-
  member(Cat, CatL2),
  w2(WNum,Cat,Tijd,_,_,_Fix,_TijdLog,Baan),				% + planning bekend
  bitright(Tijd, 7, Dag),
  Dag <= TmDag, 
  wd(WNum, Cat, T1, T2,_,_,_), 
  de_spelers(WNum, Cat, T1, T2, Spelers),
  member(Speler, Spelers),
  betrokken(WNum, Cat, T1, T2, Speler, Bijwinst, Eerder),
  assert(spelerprogrammaitem(Speler, WNum, Cat, b_True, Tijd, Bijwinst, Baan, Eerder)),	% vastgesteld
  fail.
scanSpelerProgramma(CatL2, _TmDag,  _, _, _) :-
  %TmDag > 0,	% 2.2.2012
  member(Cat, CatL2),
  wd(WNum, Cat, T1, T2,_,_,_), 
  not(w3(WNum,Cat,_,_,_,_,_)), 					% moet nog gespeeld
  not(T1 = bye),
  not(T2 = bye),						% echte wedstrijd
  not(spelerprogrammaitem(_, WNum, Cat, b_True, _, _, _, _)),	% niet vastgesteld
  member(Tx, [T1, T2]),
  tgnstNum(Tx, Speler),
  not(spelerprogrammaitem(Speler, _, Cat, _, _, _, _, _)),
  assert(spelerprogrammaitem(Speler, WNum, Cat, b_False, 0, direct, "", "<TD class=overzn>&nbsp;</TD>")),
  fail.
scanSpelerProgramma(_CatL2, _TmDag,  _, _, _) :- 		% 2.9.2016
  spelerprogrammaitem(21392, WNum, Cat, _, _, _, _, _),
  writedevice(X),
  writedevice(screen),
  write("\n", WNum, " ", Cat),
  writedevice(X),
  fail.
scanSpelerProgramma(CatL2, _TmDag,  _, _, _) :- 			% 23.11.2011
  %TmDag > 0,	% 2.2.2012
  member(Cat, CatL2),
  wd(WNum, Cat, T1, T2,_,_,_), 
  %w3(WNum,Cat,_,_,_,_,_), 					% al gespeeld	% 8.7.2016
  %not(T1 = bye),
  %not(T2 = bye),						% echte wedstrijd
  %not(spelerprogrammaitem(_, WNum, Cat, b_True, _, _, _, _)),	% niet vastgesteld
  member(Tx, [T1, T2]),
  tgnstNum(Tx, Speler),
  not(spelerprogrammaitem(Speler, _, Cat, _, _, _, _, _)),
  assert(spelerprogrammaitem(Speler, WNum, Cat, b_False, 0, direct, "", "<TD class=overzn>&nbsp;</TD>")),
  fail.
scanSpelerProgramma(_CatL2, _TmDag,  _, _, _) :- 		% 2.9.2016
  spelerprogrammaitem(21392, WNum, Cat, _, _, _, _, _),
  writedevice(X),
  writedevice(screen),
  write("\n2e ", WNum, " ", Cat),
  writedevice(X),
  fail.
scanSpelerProgramma(/*b_True,*/ _, _,  _, _, _) :-
  retractall(knop(_, _, _, _)),
  findall(SpR, spelerprogrammaItem(SpR, _, _, _, _, _, _, _), SpelersR),
  %testspeler(21392, [], SpelersR),
  not(SpelersR = []),
  sortint_c(SpelersR),
  ontdubbel(SpelersR, [], Spelers),
  %testspeler(21392, [], SpelersR),
  findall(Naamxx, speler2naam(Spelers, Naamxx), SpelersN),
  %testspeler(21392, SpelersN, []),
  sortstring_c(SpelersN, 1),
  count(Spelers, 0, SpelerAantal),
  splitsSpelers(SpelersN, SpelerAantal, _SpelersN1, Start, End),
  upper_lower(Start, StartL),
  upper_lower(StartU, Start),
  upper_lower(End, EndL),
  upper_lower(EndU, End),
  linkSpelers(StartL, EndL, Link),
  format(Knop, "\n<SPAN class=spelerref style='white-space:nowrap'>%s%s t/m %s</A></SPAN>", Link, StartU, EndU),
  format(Bestand, "spelers%s%s.html", StartL, EndL),
  format(KnopSec, "%s t/m %s", StartU, EndU),
  assert(knop(Knop, Bestand, KnopSec, StartL)),
  fail.
scanSpelerProgramma(_, _,  _, _, _).

eenVoorHuidig(_, _, _, _, b_True) :- !.
eenVoorHuidig(HDag, HDag, _, _, _) :- !.
eenVoorHuidig(Dag, HDag, [Dag,HDag|_], _, _) :- !.
eenvoorHuidig(Dag, HDag, [_|Rest], AantalPlan, Pass) :-
  eenvoorHuidig(Dag, HDag, Rest, AantalPlan, Pass), !.   
eenVoorHuidig(_Dag, _, _, Aantalplan, _) :-
  AantalPlan > 0.

groepeer([DagI|Dagen], N, OudeMaster, Afgelopen, Portie) :-
  N > Portie,
  retract(agendaitem(OudeMaster, AantalOM, AantalPlanOM, KnopO, _, _, _, _)),
  knopRef(OudeMaster, KnopOM, Afgelopen, AantalPlanOM, KnopSecOM, RowIdOM),
  assertz(agendaitem(OudeMaster, AantalOM, AantalPlanOM, KnopO, 0, KnopOM, KnopSecOM, RowIdOM)),	% is nog steeds een master
  retract(agendaitem(DagI, Aantal, AantalPlan, _, _, _, _, _)),
  knopRef(DagI, Knop, DagI, AantalPlan, KnopSec, RowId),
  assertz(agendaitem(DagI, Aantal, AantalPlan, Knop, 0, Knop, KnopSec, RowId)),	% = nieuwe master
  groepeer(Dagen, Aantal, DagI, DagI, Portie), !.
groepeer([Dag1|Dagen], N, Master, _, Portie) :-
  retract(agendaitem(Dag1, Aantal, AantalPlan, _, _, _, _, _)),
  knopRef(Master, Knop, Master, AantalPlan, KnopSec, RowId),
  N1 = N + Aantal,
  assertz(agendaitem(Dag1, Aantal, AantalPlan, Knop, Master, Knop, KnopSec, RowId)),
  groepeer(Dagen, N1, Master, Dag1, Portie), !.
groepeer([_|Dagen], N, Master, Afgelopen, Portie) :-
  groepeer(Dagen, N, Master, Afgelopen, Portie), !.
groepeer(_, _, OudeMaster, Afgelopen, _Portie) :-
  retract(agendaitem(OudeMaster, AantalOM, AantalPlanOM, KnopO, _, _, _, _)), 
  knopRef(OudeMaster, KnopOM, Afgelopen, AantalPlanOM, KnopSecOM, RowIdOM), !,
  assertz(agendaitem(OudeMaster, AantalOM, AantalPlanOM, KnopO, 0, KnopOM, KnopSecOM, RowIdOM)).
groepeer(_, _, _OudeMaster, _Afgelopen, _Portie).

knopRef(Dag, Knop,Dag, AantalPlan, RowId, RowId) :-          % knop van één en dezelfde dag
  dag(Dag,DStr,_), !,
  upper_lower(DStr, DStrL),
  fronttoken(DStrL, DagN, Rest1L),
  fronttoken(Rest1L, Tok, Rest2),
  fronttoken(Rest2, _, Rest3),
  fronttoken(Rest3, Tok2, _),
  doorhalen(AantalPlan, DStr, DStr1),
  random(100,Rand),
  linkknop(DagN, Tok,Tok2, Rand, Link, RowId, DStr),
  format(Knop, "\n<SPAN class=agendaref style='white-space:nowrap'>%s%s</A></SPAN>", Link, DStr1).
knopRef(Dag, Knop,Afgelopen, AantalPlan, RowId, RowId) :-  % knop van - tot
  dag(Dag,DStr,_),
  upper_lower(DStr, DStrL),
  fronttoken(DStrL, DagN, Rest1L),
  fronttoken(Rest1L, Tok, Rest2),
  fronttoken(Rest2, _, Rest3),
  fronttoken(Rest3, Tok2, _),
  dag(Afgelopen, DStrA, _), !,
  doorhalen(AantalPlan, DStr, DStr1),
  agendaitem(Afgelopen, _, AantalPlanAfgelopen, _, _, _, _, _), !,
  doorhalen(AantalPlanAfgelopen, DStrA, DStrA1),
  random(100,Rand),
  format(Title, "%s t/m %s", DStr, DStrA),
  linkknop(DagN, Tok,Tok2, Rand, Link, RowId, Title),
  format(Knop, "\n<SPAN class=agendaref  style='white-space:nowrap'>%s%s - %s</A></SPAN>", Link, DStr1, DStrA1).

linkKnop(DagN, Tok,Tok2, _Rand, Link, DagIs, Title) :-
  schemadoel(upload), !,
  format(Link, "<A href=\"sheet.php?plek=xxx&amp;type=agenda&amp;dag=%s%s-%s\" title=\"klik voor de agenda van %s\">", DagN, Tok,Tok2, Title),
  format(DagIs, "%s%s-%s",  DagN, Tok,Tok2).
linkKnop(DagN, Tok,Tok2, Rand, Link, "", Title) :-
  format(Link, "<A href=\"agenda%s%s-%s.html?pr=%u\" title=\"klik voor de agenda van %s\">",DagN, Tok,Tok2, Rand, Title).

geplandeWeds(CatL2, Dag, WNum) :-
  member(Cat, CatL2),
  w2(WNum,Cat,Tijd,_,_,_Fix,_TijdLog,_Baan),
  bitright(Tijd, 7, Dag1),
  Dag1 = Dag.

doorhalen(0, In, Uit) :- !,
  format(Uit, "<span style='text-decoration : line-through'>%s</span>", In).
doorhalen(_, In, In).

splitsSpelers(SpelersN, SpelerAantal, SpelersN, "A", "Z") :-
  %testspeler(21392, SpelersN, []),
  SpelerAantal < 50, !.
splitsSpelers(SpelersN, SpelerAantal, SpelersU, Start, End) :-
  N = SpelerAantal div 50,
  Portie = (SpelerAantal + N) div N + 1,
  splitsSpelers1(SpelersN, 0, Portie, [], SpelersU, Start, End).
  %testspeler(21392, SpelersU, []).

/*
testspeler(Id, SpNo, Y, []) :-
  writedevice(X),
  sp2(SpNo,_,Naam,_,_,_,_,_,_, _, _, _, _, _, _, _, _, _, _, _,_,_,_,_,_,_,_),
  %dlg_note(Naam),
  member(Naam, Y),
  %writedevice(X),
  writedevice(screen),
  write("\n1 ",Id," speler ",Naam," aanwezig (string)"),
  writedevice(X), !.
testspeler(Id, SpNo, [], SpList) :-
  member(SpNo, SpList),
  sp2(SpNo,_,Naam,_,_,_,_,_,_, _, _, _, _, _, _, _, _, _, _, _,_,_,_,_,_,_,_),
  writedevice(X),
  writedevice(screen),
  write("\n2 ",Id," speler ",Naam," aanwezig (integer)"),
  writedevice(X), !.
testspeler(Id, SpNo, _Y, _) :-
  writedevice(X),
  sp2(SpNo,_,Naam,_,_,_,_,_,_, _, _, _, _, _, _, _, _, _, _, _,_,_,_,_,_,_,_),
  writedevice(screen),
  write("\n- ",Id," speler ",Naam," afwezig!!"),
  writedevice(X), !.
testspeler(Id, _SpNo, _Y, _) :-
  write("\n geen", Id).
*/

splitsSpelers1([Sp1], _N, _Portie, SpelersIn, Uit, Start, "Z") :-
  append(SpelersIn, [Sp1], Uit), !,
  SpelersIn = [Speler |_],
  frontstr(1,Speler,Start,_).
splitsSpelers1([Sp1,Sp2|_SpelersN], N, Portie, SpelersIn, Uit, Start1, End1) :-
  N >= Portie,
  frontchar(Sp1,FrontChar1,_),
  not(frontchar(Sp2,FrontChar1,_)), 
  append(SpelersIn, [Sp1], Uit),
  SpelersIn = [Speler |_],
  frontstr(1,Speler,Start,_),
  validchar(Start, Start1),
  frontstr(1,Sp1,End,_),
  validchar(End, End1).
splitsSpelers1([Sp1,Sp2|SpelersN], N, Portie, _, SpelersU, Start, End) :-
  N >= Portie,
  frontchar(Sp1,FrontChar1,_),
  not(frontchar(Sp2,FrontChar1,_)), !,
  splitsSpelers1([Sp2|SpelersN], 0, Portie, [], SpelersU, Start, End).
splitsSpelers1([X|SpelersN], N, Portie, SpelersIn, SpelersU, Start, End) :-
  N1 = N + 1, !,
  append(SpelersIn, [X], SpelersIn1),
  splitsSpelers1(SpelersN, N1, Portie, SpelersIn1, SpelersU, Start, End).
splitsSpelers1(_, _N, _Portie, SpelersIn, SpelersIn, "X", "X") :- !.

validChar(C, "a") :-
  upper_lower(C, Cl),
  Cl < "a", !.
validChar(C, "z") :-
  upper_lower(C, Cl),
  Cl > "z", !.
validChar(C, C).

betrokken(Wnum, Cat, T1, _T2, Sp, direct, Eerder) :-
  eerder(WNum, Cat, T1, Eerder),
  tgnstNum(T1, Sp), !.
betrokken(Wnum, Cat, _T1, T2, Sp, direct, Eerder) :-
  eerder(WNum, Cat, T2, Eerder),
  tgnstNum(T2, Sp), !.
betrokken(Wnum, Cat, T1, T2, _Sp, bijverlies, Eerder) :-
  kopl(_, pl3en4(_), Cat, _),
  eerder2(WNum, Cat, T1, T2, Eerder), !.
betrokken(Wnum, Cat, T1, T2, _Sp, bijwinst, Eerder) :-
  eerder2(WNum, Cat, T1, T2, Eerder), !.

ontdubbel([H,H|Tail], In, Uit) :-
  ontdubbel([H|Tail], In, Uit), !.
ontdubbel([H|Tail], In, [H|Uit]) :-
  ontdubbel(Tail, In, Uit), !.
ontdubbel(_, In, In).

speler2naam(Lijst, Naam) :-
  member(SKeus, Lijst),
  sp2(SKeus,_,Naam,_,_,_,_,_,_, _, _, _, _, _, _, _, _, _, _, _,_,_,_,_,_,_,_).

linkSpelers(StartL, EndL, Link) :-
  schemadoel(upload), !,
  format(Link, "<A href=\"sheet.php?plek=xxx&amp;type=spelers&amp;l1=%s&amp;l2=%s\" title=\"klik voor lijst van spelers\">", StartL, EndL).
linkSpelers(StartL, EndL, Link) :-
  random(99,RandomInt),
  format(Link, "<A href=\"spelers%s%s.html?pr=%u\">", StartL, EndL, RandomInt).

eerder(WNum, Cat, T, "<TD class=overzn style=\"color: red\">(!)</TD>") :-
  ws(WNum, Cat, T, _Tijd), !.	% om af te zeggen ...
eerder(_, _, _, "<TD class=overzn>&nbsp;</TD>").

eerder2(WNum, Cat, T, _, "<TD class=overzn style=\"color: red\">(!)</TD>") :-
  ws(WNum, Cat, T, _Tijd), !.	% om af te zeggen ...
eerder2(WNum, Cat, _, T, "<TD class=overzn style=\"color: red\">(!)</TD>") :-
  ws(WNum, Cat, T, _Tijd), !.	% om af te zeggen ...
eerder2(_, _, _, _, "<TD class=overzn>&nbsp;</TD>").

spelernummerlijst([Naam|Rest],[SKeus|Uit]) :-
  sp2(SKeus,_,Naam,_,_,_,_,_,_, _, _, _, _, _, _, _, _, _, _, _,_,_,_,_,_,_,_), !,
  spelernummerlijst(Rest, Uit).
spelernummerlijst([Naam|Rest],[SKeus|Uit]) :-
  sp2(SKeus,_,NaamX,_,_,_,_,_,_, _, _, _, _, _, _, _, _, _, _, _,_,_,_,_,_,_,_),
  trim_c(NaamX),		% effe zoeken
  NaamX = Naam, !,
  spelernummerlijst(Rest, Uit).
spelernummerlijst([Naam|Rest],Uit) :-
  writedevice(X),
  writedevice(screen),
  write("\n?? ", Naam), !,
  writedevice(X),
  beep(),
  spelernummerlijst(Rest, Uit).
spelernummerlijst(_, []).

bw2str(direct, b_True, "..............."):- !.
bw2str(direct, _, "&nbsp;"):- !.
bw2str(bijwinst, _, "bij winst").
bw2str(bijverlies, _, "bij verlies").

dagHerhaal(Dag, Uit) :-	
  Dag > 0,			% echte planning
  agendadag(0),			% eerste spelerregel
  dt_dateoffset_to_str(Dag, "%Dn %Dd/%Md", DagS),
  fronttoken(DagS, Weekd, DagS1),
  format(Uit, "<TD class=overzn>%s</TD><TD class=overzn>%s</TD>", Weekd, DagS1),
  assert(agendadag(Dag)), !.
dagHerhaal(Dag, Uit) :-		% volgende dag idem
  Dag > 0,			% echte planning
  not(agendadag(Dag)),
  dt_dateoffset_to_str(Dag, "%Dn %Dd/%Md", DagS),
  fronttoken(DagS, Weekd, DagS1),
  format(Uit, "</TR>\n<TR><TD>&nbsp;</TD><TD class=overzn>%s</TD><TD class=overzn>%s</TD>", Weekd, DagS1),
  assert(agendadag(Dag)), !.
dagHerhaal(Dag, "<TD class=overzn>zelfde dag:</TD>") :-			% zelfde dag nog een keer
  Dag > 0, !.			% echte planning
dagHerhaal(Dag, Uit) :-	
  agendadag(0),			% eerste spelerregel
  Dag = 0,			% geen echte planning
  format(Uit, "<TD class=overzn>%s</TD><TD class=overzn>%s</TD>", "&nbsp;", "&nbsp;"),
  assert(agendadag(1)), !.
dagHerhaal(_Dag, "<TD class=overzn>en&nbsp;</TD>").			% zelfde dag nog een keer

baanExtra(Baan, Uit) :-
  baan_adm(ja), !,
  format(Uit, "<TD class=overzn>%s</TD>", Baan).
baanExtra(_, "").

spelereinde().
spelereinde() :-
  write("</TR>"),
  fail.

member2(T1, [T1, T2 | _], b_True) :-
  bitright(T1, 7, Dag1),
  Dag1 > 0,
  bitright(T2, 7, Dag2),
  Dag1 = Dag2.
member2(T1, [T1| _], b_False).
member2(T, [_| Rest], ZelfdeDag) :-
  member2(T, Rest, ZelfdeDag).

metZonderLink(b_False, DHstr, TijdS, _OndRef, Link, WNo, WC, Shortp, BaanExtra, BWS, Eerder) :-
  reversestr(Link, "", LinkR1),	% 17.6.2006
  frontchar(LinkR1, _, LinkR2),
  reversestr(LinkR2, "", LinkZ),
  writef("\n%s<TD class=overzr>%s</TD><TD class=overzn><%s&amp;merk=T%dx%d\">%s</a></TD>%s<TD class=overzn>%s</TD>%s", DHstr, TijdS, LinkZ, WNo, WC, Shortp, BaanExtra, BWS, Eerder), !.
  %writef("\n%s<TD class=overzr>%s</TD><TD class=overzn><%s&amp;merk=T%dx%d#T%dx%d\">%s</a></TD>%s<TD class=overzn>%s</TD>%s", DHstr, TijdS, LinkZ, WNo, WC, WNo, WC, Shortp, BaanExtra, BWS, Eerder), !.
metZonderLink(_, DHstr, TijdS, _OndRef, _Link, _WNo, _WC, Shortp, BaanExtra, BWS, Eerder) :-
  writef("\n%s<TD class=overzr>%s</TD><TD class=overzn>%s</TD>%s<TD class=overzn>%s</TD>%s", DHstr, TijdS, Shortp, BaanExtra, BWS, Eerder), !.

tijdstipweergave(0, "") :- !.
tijdstipweergave(Tijdstip, TijdS) :-
  tijd_str(TijdStip,TijdS,_).

agendaref(apart, 1, b_False) :-			% verwijs naar één dagagendaitem
  schemadoel(upload),
  spelerprogrammaitem(_, _, _, b_True, TijdX, _, _, _),
  dag(Dag,DStr,_), 
  getbacktrack(Here),
  bitright(Tijdx, 7, Dagx),
  Dag = Dagx,
  cutbacktrack(Here),
  upper_lower(DStr, DStrL),
  fronttoken(DStrL, DagN, Rest1L),
  fronttoken(Rest1L, Tok, Rest2),
  fronttoken(Rest2, _, Rest3),
  fronttoken(Rest3, Tok2, _),
  writef("\n<SPAN class=agendaref><A href=\"sheet.php?plek=xxx&amp;type=agenda&amp;dag=%s%s-%s\">Dagagenda!</A></SPAN><BR>", DagN, Tok,Tok2),
  !.
agendaref(apart, 1, b_False) :-
  %spelerprogrammaitem(_, _, _, b_True, _, _, _, _),
  schemadoel(Doel),
  not(Doel = upload),
  spelerprogrammaitem(_, _, _, b_True, TijdX, _, _, _),
  dag(Dag,DStr,_), 
  getbacktrack(Here),
  bitright(Tijdx, 7, Dagx),
  Dag = Dagx,
  cutbacktrack(Here),
  upper_lower(DStr, DStrL),
  fronttoken(DStrL, DagN, Rest1L),
  fronttoken(Rest1L, Tok, Rest2),
  fronttoken(Rest2, _, Rest3),
  fronttoken(Rest3, Tok2, _),
  writef("\n<SPAN class=agendaref><A href=\"agenda%s%s-%s.html\">Dagagenda!</A></SPAN><BR>", DagN, Tok, Tok2), !.
agendaref(_, _, _).

resetspelerprogramma() :-
  retractall(spelerprogrammaItem(_, _, _, _, _, _, _, _)),
  retractall(agendaitem(_, _, _, _, _, _, _, _)),
  retractall(knop(_,_, _, _)).

spagendaref(apart, 1, b_False) :-
  spelerprogrammaitem(_, _, _, _, _, _, _, _),
  knop(Knop, _, _, _),
  write("\nSpelers ", Knop, "<BR>"), !.
spagendaref(_, _, _).

publiceerOverzicht(_SheetSel, _OnderdelenR, _CSSNaam, _ZonderLinks, Mededeling) :-
  get_htmlmap(Dir),
  filenamepath(FileNaam,Dir,"toernooi.html"),		% de frontpagina!
  closeOpen(FileNaam,"", "voorpagina"),
  retractall(teDeleten("TOERNOOI.HTML")),
  naam(TNaam, _, _, _, _, _),
  metatag(Eerst,Tag),
  writef("%s\n<HTML><HEAD>%s<META HTTP-EQUIV=\"Expires\" CONTENT=\"Tue, 24 Oct 1996 17:45:00 GMT\"><TITLE>%s</TITLE>", Eerst, Tag, TNaam),
	writef("\n<!-- ToernooiAssistent versie %s -->",versie),
  %styleSheet(CSSNaam),
  write("</HEAD>"),
  write("\n<BODY>"),
  logoErBijTop(),
  write("\n<!-- begin template voorpagina -->"),
  write("\n<H3>", TNaam, "</H3>"),
  fronttoken(Mededeling, _, _),
  toBr(Mededeling, Med),
  write("\n<span class=mededeling>",Med,"</span>"),
  fail. 
publiceerOverzicht(_SheetSel, OnderdelenR, _CSSNaam, _ZonderLinks, _Mededeling) :-
  verzamelIndex(apart, OnderdelenR, b_False, b_True, ""),
  fail.
publiceerOverzicht(_, _OnderdelenR, _, ZonderLinks, _) :-
  write("\n<!-- einde template voorpagina -->\n"),
  postHtml(ZonderLinks),
  copyright(),
  write("\n</BODY>"),
  write("\n</HTML>"),
  schemadoel(Print),
  not(Print = upload),
  closefile(work),
  fail.
publiceerOverzicht(_, _OnderdelenR, _, _ZonderLinks, _).

gespeeldA(N) :-
  w3(N,_Cat,_,_,_,_,_).

nietgespeeld(N) :-		% nog niet gespeeld
  wd(N, Cat, T1, T2,_,_,_), 
  echt(T1, T2),
  not(w3(N,Cat,_,_,_,_,_)).

finalebekend(b_True) :-
  getbacktrack(Here),
  wc(WCat, _,_,  _, _, _, _, _, _, _, _, _, _, _, _),
  %wc(WCat, _,_, _,_,_,_),
  pos(WCat, _, _, afval),	% minstens één afvalronde
  cutbacktrack(Here),
  findall(Y, gespeeldA(Y), YL),
  count(YL, 0, AY),
  findall(Z, nietgespeeld(Z), ZL),
  count(ZL, 0, AZ),
  AY > AZ,
  wc(WC, _,_,  _, _, _, _, _, _, _, _, _, _, _, _),
  %wc(WC, _,_, _Shortp,_,_,_),
  finalebekend1(WC), !.  
finalebekend(b_False).

finalebekend1(Cat) :-
  pos(Cat, _, _, afval),
  wd(1, Cat, T1, _T2,_,_,_),
  ietsBekend(T1), !.
finalebekend1(Cat) :-
  pos(Cat, _, _, afval),
  wd(1, Cat, _T1, T2,_,_,_),
  ietsBekend(T2), !.
finalebekend1(Cat) :-
  w2(1,Cat,_,_,_,_,_,_), !.
finalebekend1(Cat) :-
  pos(Cat, 1, _, afval),
  kopl(_CatVan, _Pl, Cat, plaats(2)), !.
finalebekend1(Cat) :-
  pos(Cat, 1, _, afval),
  kopl(_CatVan, _Pl, Cat, plaats(3)), !.
finalebekend1(Cat) :-
  pos(Cat, _, _, poel),
  poule_ranking1(Cat, 1, _Tgnst1, Saldo, _NK1),
  Saldo = [Punten1 | _],
  Punten1 > 2, !.
  
ietsBekend(T1) :-
  not(T1 = bye),
  not(T1 = onbekend).

copyright() :-
  frontstr(4,versie, Jaar, _), !,
  write("<SPAN class=logo> &copy;",Jaar," ToernooiAssistent</SPAN>").
copyright().

stylesheet(FileNaam) :-
  schemadoel(upload), !,
  write("\n<LINK rel=stylesheet Type=\"text/css\" href=\"sheet.php?plek=xxx&amp;type=css&amp;naam=", FileNaam, "\">").
styleSheet(FileNaam) :-
  schemadoel(uploadftp), !,
  write("\n<LINK rel=stylesheet Type=\"text/css\" href=\"", FileNaam, "\">").
styleSheet(_FileNaam) :-
  weblayout1(afdruk, _IDCW_TOERNOOICSS_DEFAULT, _Settings1 ,_Kolommen, Settings2, _IDCW_MEDEDELING_VALUE, _PreselectBew),
  Settings2 = [FontGr],
  write("\n<STYLE type=\"text/css\">"),
  writeStyleSheet(Fontgr), !,
  write("\n</STYLE>").
styleSheet(FileNaam) :-
  write("\n<LINK rel=stylesheet Type=\"text/css\" href=\"", FileNaam, "\">").

%################################
finalesAfdruk() :-
  weblayout1(afdruk, _IDCW_TOERNOOICSS_DEFAULT, Settings1 ,_Kolommen, _Settings2, _IDCW_MEDEDELING_VALUE, _PreselectBew),
  Settings1 = [],
  dlg_MessageBox( "Afdrukken", "Druk eerst een pagina af via\nOnderdeel, Print, via InternetBrowser", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  !.
finalesAfdruk() :-
  retractall(groepslid(_, _, _, _, _, _, _, _)),
  finalebekend(Bekend),
  Bekend = b_True,
  assert(schemadoel(afdruk)),
  weblayout1(afdruk, _IDCW_TOERNOOICSS_DEFAULT, Settings1 ,_Kolommen, Settings2, _IDCW_MEDEDELING_VALUE, _PreselectBew),
  Settings1 = [_IDCW_RANGLIJST_CHECKED, _IDCW_ZWARTWIT_CHECKED, _Reserve, _IDCW_AANEENGEREGEN_CHECKED,_IDCW_MET_SPEELSTERKTE_CHECKED,_IDCW_SHEET_SLECTOR_CHECKED,_IDCW_INCL_LOGOBESTANDEN_CHECKED],
  Settings2 = [FontGr],
  zetzwartwit(afdruk, _IDCW_ZWARTWIT_CHECKED),
  getFolder(Pad,_,_,_),
  %checkStyleSheet(_IDCW_TOERNOOICSS_DEFAULT, FontGr),
  filenamepath(FinaleName,Pad,"Finales.html"),
  openwrite(work, FinaleName),
  writedevice(work),
  naam(TNaam, _, _, _, _, _),
  metatag(Eerst,Tag),
  writef("%s\n<HTML><HEAD>%s<META HTTP-EQUIV=\"Expires\" CONTENT=\"Tue, 24 Oct 1996 17:45:00 GMT\"><TITLE>%s</TITLE>", Eerst, Tag, TNaam),
  writef("\n<!-- ToernooiAssistent versie %s -->",versie),
  write("\n<STYLE type=\"text/css\">"),
  writeStyleSheet(Fontgr),
  write("\n</STYLE>"),
  %styleSheet(_IDCW_TOERNOOICSS_DEFAULT),
  write("</HEAD>"),
  write("\n<BODY>"),
  logoErBijTop(),
  write("\n<H3>", TNaam, " - Finales en Poules</H3>"),
  findall(Onderdeel, wedscat_ingedeeld(_, b_True, Onderdeel,  b_True), OnderdelenIn),
  %write(OnderdelenIn),
  verzamelIndex(apartmetfinale, OnderdelenIn, b_False, _IDCW_MET_SPEELSTERKTE_CHECKED, ""),
  fail.
finalesAfdruk():-
  finalebekend(Bekend),
  Bekend = b_True,
  postHtml(b_True),
  copyright(),
  write("\n</BODY>"),
  write("\n</HTML>"),
  closefile(work), 
  getFolder(Pad,_,_,_),
  filenamepath(FinaleName,Pad,"Finales.html"),
  format(URL1, "file:\\\\%s", FinaleName),
  Null = cast(api_hwnd,0),
  api_ShellExecute(Null,"open",URL1,"","",1), !.		% open in browser
finalesAfdruk().

finalepagina(_SheetSel, _OnderdelenR, _CSSNaam, _ZonderLinks, _Mededeling, _) :-
  finalebekend(Bekend),
  Bekend = b_False, !.
finalepagina(_SheetSel, _OnderdelenR, CSSNaam, _ZonderLinks, _Mededeling, _) :-
  retractall(groepslid(_, _, _, _, _, _, _, _)),
  get_htmlmap(Dir),
  filenamepath(FileNaam,Dir,"finales.html"),
  closeOpen(FileNaam,"", "finales"),
  retractall(teDeleten("FINALES.HTML")),
  naam(TNaam, _, _, _, _, _),
  metatag(Eerst,Tag),
  writef("%s\n<HTML><HEAD>%s<META HTTP-EQUIV=\"Expires\" CONTENT=\"Tue, 24 Oct 1996 17:45:00 GMT\"><TITLE>%s</TITLE>", Eerst, Tag, TNaam),
  writef("\n<!-- ToernooiAssistent versie %s -->",versie),
  %write("\n<LINK rel=stylesheet Type=\"text/css\" href=\"sheet.php?plek=xxx&amp;type=css&amp;naam=", CSSNaam, "\">"),
  styleSheet(CSSNaam),
  write("</HEAD>"),
  write("\n<BODY>"),
  logoErBijTop(),
  toernooiDiv(),
  write("\n<Div class=spelertitel>Stand van Finales en Poules</div>\n"),
  write("<div id=sheets>"),
  fail. 
finalepagina(_SheetSel, OnderdelenR, _CSSNaam, _ZonderLinks, _Mededeling, MetSpeelsterkte) :-
  expandeer(OnderdelenR, [], OnderdelenR1),	% voeg AR subonderdelen toe
  verzamelIndex(apartmetfinale, OnderdelenR1, b_False, MetSpeelsterkte, ""),
  fail.
finalepagina(_, _OnderdelenR, _, ZonderLinks, _, _) :-
  write("</div>"),
  postHtml(ZonderLinks),
  copyright(),
  write("\n</BODY>"),
  write("\n</HTML>"),
  schemadoel(Print),
  not(Print = upload),
  closefile(work),
  fail.
finalepagina(_, _OnderdelenR, _, _ZonderLinks, _, _).

expandeer([Cat|CatL], In, Uit) :-
  findall(CatS, subCats(Cat, CatS), CatSL),
  append(In, CatSL, In1),
  expandeer(CatL, In1, Uit), !.
expandeer(_, In, In).  
  
subCats(Cat, Cat) :-
  pos(Cat,_,_,_).
subCats(Cat, CatS) :-
  not(pos(Cat, _, _, _)),
  kopl(CatS, _, Cat, _),
  pos(CatS,_,_,_).
  
%################################

sluitenMaarAgenda(ShSelect, ZonderWebLinks, CatList) :-
  overzichtref(ShSelect, Zonderweblinks),
  verzamelIndex(ShSelect,CatList, b_True, b_False,"").
sluitenMaarAgenda(ShSelect, ZonderWebLinks, CatList) :-
  overzichtref(ShSelect, Zonderweblinks),
  verzamelIndex(ShSelect,CatList, b_True, b_False, "1"),
  postHtml(ZonderWeblinks),
  copyright(),
  write("\n</BODY>"),
  write("\n</HTML>\n"),
  not(schemadoel(upload)),
  closefile(work), 		% van vorige
  fail.

menustr(_) :-
  not(schemadoel(upload)), !.
menustr(OnderdelenR) :-			/* maak een bestand met menu-elementen */
  write("***[menuitems][]***\n"), 
  member(WC, OnderdelenR),
  wc(WC, _OnderdeeelID,_Orde,  Naam, _EDG, Kort, _SexL, Speelsterkte, leeftijd(_,LeeftV), leeftijd(_,LeeftTM), HDG, _Schemasoort, _Keuring, _KNLTB, _Geld),
  %wc(WC, Ond,_, Shortp,_,_,_),
  %wcritP(WC, Speelsterkte, LeeftV, LeeftTM, _),
  xSpelsoort(WC, _MVs1, _MVdomain, WTs1, _WTdomain),
  bovenGrens(LeeftTM, LeeftMO),
  not(groepslid(WC, _, _, _, _, _, _, _)),
  zonderHekje(Kort, "", KortZ),
  zonderHekje(Naam, "", NaamZ),
  assert(groepslid(WC, KortZ, NaamZ, HDG, WTs1, Speelsterkte, LeeftV, LeeftMO)),
  fail.
menustr(OnderdelenR) :-
  not(isKleinAantal(OnderdelenR)),
  sheetSelectorGroep(LftOnder, LftBoven, MV, WType, GroepsTitel, _Kolom),
  groepslid(WC, Short, LongS, MV, WType, St, LftV, LftM),
  LftM <= LftBoven,
  LftM > LftOnder,
  retract(groepslid(WC, Short, LongS, MV, WType, St, LftV, LftM)),
  wc(WC, _OnderdeeelID,_Orde,  Lang, _EDG, Kort, _SexL, _SpStrk, _LftV, _LftTM, _HDG, _Schemasoort, _Keuring, _KNLTB, _Geld),
  %wc(WC, Lang,_, Kort,_,_,_),
  zonderHekje(Lang, "", LangZ),
  zonderHekje(Kort, "", KortZ),
  htmlNaam(WC, HTML, _),
  writef("{{#}}%s}#{%s}#{%s}#{sheet}#{%s\n", GroepsTitel, KortZ, LangZ, HTML),
  fail.
menustr(OnderdelenR) :-
  isKleinAantal(OnderdelenR),
  sheetHeaders(TO, _TA, _TM, _, _),
  retract(groepslid(WC, _Short, _, _MV, _WType, _St, _LftV, _LftM)),
  wc(WC, _OnderdeeelID,_Orde,  Lang, _EDG, Kort, _SexL, _SpStrk, _, _, _HDG, _Schemasoort, _Keuring, _KNLTB, _Geld),
  %wc(WC, Lang,_, Kort,_,_,_),
  zonderHekje(Lang, "", LangZ),
  zonderHekje(Kort, "", KortZ),
  htmlNaam(WC, HTML, _),
  writef("{{#}}%s}#{%s}#{%s}#{sheet}#{%s\n", TO, KortZ, LangZ, HTML),
  fail.
menustr(OnderdelenR) :-
  not(isKleinAantal(OnderdelenR)),
  knop(_, Besta, KnopTitel, RowId),
  writef("{{#}}Speleragenda}#{%s}#{%s}#{speler}#{%s\n", Knoptitel, Besta, RowId),
  fail.
menustr(OnderdelenR) :-
  isKleinAantal(OnderdelenR),
  sheetHeaders(_, TA, TM, _, _),
  knop(_, Besta, KnopTitel, RowId),
  writef("{{#}}%s}#{%s}#{%s}#{%s}#{%s\n", TA, Knoptitel, Besta, TM, RowId),
  fail.
menustr(_) :-
  agendaitem(_RDag, _Aantal, _AantalPlan, _Knop, Dag, _RKnop, KnopSec, RowId),
  Dag = 0,  % = alleen Master
  writef("{{#}}Dagagenda}#{%s}#{%s}#{agenda}#{%s\n", KnopSec, KnopSec, RowId),
  fail.
menustr(_) :-
  write("{{#}}Diversen}#{Voorpagina}#{Voorpagina}#{voorpagina}#{\n"),
  finaleBekend(Bekend),
  Bekend = b_True,
  write("{{#}}Diversen}#{Finales}#{Finales}#{finales}#{\n"),
  fail.
menustr(_).

isKleinAantal(OnderdelenR) :-
  count(OnderdelenR, 0, Aant),
  Aant <= 6, Aant > 0.

zoekOpTitels("team", "").
zoekOpTitels("bedrij", "").

sheetheaders(Hdr, "Teamagenda", "team", "Teams", "Agenda") :-
  naam(Naam, _, _, _,_,_),
  upper_lower(Naam, NaamL),
  zoekOpTitels(Comp, Hdr),
  searchstring(NaamL, Comp, Pos),
  Pos > 0, !.
sheetheaders("Onderdelen", "Speleragenda", "speler", "Spelers", "Dagprogramma"). 

verzamelIndex(apart, _OnderdelenR, b_True, _, _) :- !.		% niet als apart
verzamelIndex(ShSel, WCL, _, _, _) :-			% de aparte "toernooi.html" met/zonder finales
  member(WC, WCL),
  wc(WC, _OnderdeeelID,_Orde,  SelectieOnd, _EDG, Shortp, _SexL, Speelsterkte, leeftijd(_,LeeftV), leeftijd(_,LeeftTM), HDG, _Schemasoort, _Keuring, _KNLTB, _Geld),
  %wc(WC, SelectieOnd,_, Shortp,_,_,_),
  %wcritP(WC, Speelsterkte, LeeftV, LeeftTM, _),
  xSpelsoort(WC, _MVs1, _MVdomain, WTs1, _WTdomain),
  bovenGrens(LeeftTM, LeeftMO),
  kortLang(ShSel, Shortp, SelectieOnd, OnderdeelNaam),
  not(groepslid(WC, _, _, _, _, _, _, _)),
  assert(groepslid(WC, OnderdeelNaam, SelectieOnd, HDG, WTs1, Speelsterkte, LeeftV, LeeftMO)),
  fail.
verzamelIndex(apart, _OnderdelenR, _, _, _) :-
  write("\n<TABLE><TR><TD style=\"vertical-align:top\">\n<TABLE>"),
  fail.
verzamelIndex(apartmetfinale, _OnderdelenR, _, _, _) :-
  write("\n<TABLE><TR><TD style=\"vertical-align:top\">\n<TABLE>"),
  fail.
verzamelIndex(bijgevoegd, _OnderdelenR, _, _, Suffix) :-		% de "snelselectors"
  writef("\n<div id=menu%s>\n<HR><TABLE>", Suffix),
  fail.
verzamelIndex(SheetSelect, OnderdelenR, _, MetSpeelsterkte, _) :-	% het vlees	
  not(isKleinAantal(OnderdelenR)),
  assert(indexje(0)),		% ?????
  assert(huidigeKolom(links)),
  linksrechts(Kolom),
  sheetSelectorGroep(LftMVan, LftMTot, Geslacht, WType, Titel, Kolom),
  sheetSelectorElemAanwezig(LftMVan, LftMTot, Geslacht, WType, Qualifier),
  write("\n<!--Groep begin ",Titel," : ", Qualifier,"-->"),
  prplan(PlanSel),
  plantm(DagNumP),
  DagTM = PlanSel * DagNumP,
  omlijstMetTR(SheetSelect, Kolom),
  sheetSelectorElem(SheetSelect, LftMVan, LftMTot, Geslacht, WType, Titel, MetSpeelsterkte, DagTM),
  fail.
verzamelIndex(SheetSelect, OnderdelenR, _, MetSpeelsterkte, _) :-	
  isKleinAantal(OnderdelenR),
  assert(indexje(0)),		% ?????
  assert(huidigeKolom(links)),
  linksrechts(Kolom),
  sheetHeaders(TO, _, _, _, _),
  write("\n<!--Groep begin ",TO," -->"),
  omlijstMetTR(SheetSelect, Kolom),
  sheetSelectorElemComp(SheetSelect, MetSpeelsterkte, TO),
  fail.
verzamelIndex(SheetSelect, _OnderdelenR, _, _, _) :-
  not(SheetSelect = apartmetfinale),
  restsheets(SheetSelect, rechts),
  fail.
verzamelIndex(bijgevoegd, _OnderdelenR, _, _, _) :-
  indexje(I),
  I mod 2 = 0,
  write("</TR>"),
  fail.
verzamelIndex(apart, _OnderdelenR, _, _, _) :-
  write("\n</TABLE>\n</TD></TR></TABLE>").  
verzamelIndex(apartmetfinale, _OnderdelenR, _, _, _) :-
  write("\n</TABLE>\n</TD></TR></TABLE>").  
verzamelIndex(bijgevoegd, _OnderdelenR, _, _, _) :-
  write("\n</TABLE><HR>\n</div>\n").  

groepslidMetLink(ShortMetLink, MV, WT, LftOnder, LftBoven) :-	% Short = eigenlijk ShortLang
  groepslid(WC, Short, _, MV, WT, St, LftT, LftM),
  LftM <= LftBoven,
  LftM > LftOnder,
  retract(groepslid(WC, Short, Lang, MV, WT, St, LftT, LftM)),
  htmlNaam(WC, _HNaam, Link),
  format(ShortMetLink, "\n <%s title=\"klik voor %s\">%s</A>", Link, Lang, Short).

groepslidMetLinkComp(ShortMetLink) :-% Short = eigenlijk ShortLang
  wtypes(WType),
  geslachten(Geslacht),
  retract(groepslid(WC, Short, Lang, Geslacht, WType, _, _, _)),
  htmlNaam(WC, _HNaam, Link),
  format(ShortMetLink, "\n <%s title=\"klik voor %s\">%s</A>", Link, Lang, Short).

bovenGrens(0, 99) :- !.
bovenGrens(N, N).

sheetSelectorElemAanwezig(LftMVan, LftMTot, Geslacht, WType, Lang) :-
  groepslid(_, _Short, Lang, Geslacht, WType, _, _, LftM),
  LftM >= LftMVan,
  LftM <= LftMTot, !.

publiceerBekendeFinale(WC, _, DagTM) :-
  finalebekend1(WC),
  pos(WC, _, _, afval),
  publiceerFinale(WC, DagTM),
  publiceer34(WC, DagTM), !.
publiceerBekendeFinale(WC, MetSpeelsterkte, _DagTM) :-
  pos(WC, _, _, poel),
  poule_ranking1(WC, 1, Tgnst1, Saldo1, NK1),
  Saldo1 = [Punten1 | _],
  Punten1 > 0,
  poule_ranking1(WC, 2, Tgnst2, _Saldo2, NK2),
  repRowH(1, WC, 0, Tgnst1, Sterk12, Naam12, Echt12, _Kol12, Bondsnr12, _Gebdatum12, _Distr12, Sterk1, Naam1, Echt1, _Kol1, Bondsnr11, _Gebdatum11, _Distr11, Br1, []),
    plaatsingHTML(WC, Tgnst1, Kleur1, KleurEnd1, Naam12, Naam121, b_False),
    write("\n<TR><TD>&nbsp;</TD>"),
    maakMijnlink(Naam1, BondsNr11, NaamLinked1), 
    maakMijnlink(Naam121, BondsNr12, NaamLinked2), 
    writef("<TD class=pouleo style=\"text-align: left\">%s%s%s%s%s</TD>", Kleur1,  NaamLinked1, Br1, NaamLinked2,KleurEnd1),
    speelsterkteKol(MetSpeelsterkte, Sterk1, BondsNr11, Echt1, Br1, Sterk12, BondsNr12, Echt12),
    writef("\n<TH class=pouleo style=\"text-align: center;text-decoration: bold\">% <span style=\"font-weight: normal\">%s</span></TH></TR>", 1, NK1),	
  repRowH(1, WC, 0, Tgnst2, Sterk22, Naam22, Echt22, _Kol22, Bondsnr22, _Gebdatum22, _Distr22, Sterk2, Naam21, Echt2, _Kol2, Bondsnr21, _Gebdatum21, _Distr21, Br2, []),
    plaatsingHTML(WC, Tgnst2, Kleur2, KleurEnd2, Naam22, Naam221, b_False),
    write("\n<TR><TD>&nbsp;</TD>"),
    maakMijnlink(Naam21, BondsNr21, NaamLinked21), 
    maakMijnlink(Naam221, BondsNr22, NaamLinked22), 
    writef("<TD class=pouleo style=\"text-align: left\">%s%s%s%s%s</TD>", Kleur2, NaamLinked21, Br2, NaamLinked22,KleurEnd2),
    speelsterkteKol(MetSpeelsterkte, Sterk2, BondsNr21, Echt2, Br2, Sterk22, BondsNr22, Echt22),
    writef("\n<TH class=poulehr>% <span style=\"font-weight: normal\">%s</span></TH></TR>", 2, NK2),	!.
publiceerBekendeFinale(_WC, _, _).

speelsterkteKol(b_True, _Sterk2, _Bond2, _Echt2, _Br, _Sterk1, _Bond1, _Echt1) :-		% NB Omgedraaid!
  writef("<TD class=pouleo style=\"text-align: right\">"),
  fail.
speelsterkteKol(b_True, Sterk2, Bond2, Echt2, _Br, _Sterk1, _Bond1, _Echt1) :-
  linkSpelerprofiel(Sterk2, Bond2, Echt2),
  fail.
speelsterkteKol(b_True, _Sterk2, _Bond2, _Echt2, Br, _Sterk1, _Bond1, _Echt) :-
  write(Br),
  fail.
speelsterkteKol(b_True, _Sterk2, _Bond2, _Echt2, _Br, Sterk1, Bond1, Echt1) :-
  linkSpelerprofiel(Sterk1, Bond1, Echt1),
  fail.
speelsterkteKol(b_True, _Sterk2, _, _, _Br, _Sterk1, _, _) :-
  write("</TD>"), !.
speelsterkteKol(_, _Sterk1, _, _, _Br, _Sterk2, _, _).

sheetSelectorElemComp(bijgevoegd, _, Titel) :-
  findall(Short, groepslidMetLinkComp(Short), ShortList),
  not(ShortList = []),
  write("\n<TD style=\"vertical-align:top;white-space:nowrap\">",Titel,"</TD>\n<TD style=\"vertical-align:top;\"><SPAN style=\"white-space:nowrap\">"),
  list_to_string(ShortList, "</SPAN> <SPAN style=\"white-space:nowrap\">", SelList),
  write(SelList, "</SPAN>\n</TD>").
sheetSelectorElemComp(apart, _, Titel) :-
  findall(Long, groepslidMetLinkComp(Long), LongList),
  not(LongList = []),
  write("\n<TR><TD style=\"vertical-align:top\"><TABLE>"),
  write("\n<TR><TH style=\"white-space:nowrap;text-align:left\">",Titel,"</TH></TR>\n"),
  list_to_string(LongList, "</TD></TR>\n<TR><TD style=\"vertical-align:top;white-space:nowrap\">", SelList),
  write("\n<TR><TD style=\"vertical-align:top;white-space:nowrap\">",SelList, "\n</TD></TR></TABLE></TD></TR>"),
  count(LongList, 0, Aantal),
  indexje(I),
  I1 = I + 2 + Aantal,
  assert(indexje(I1)), !.
sheetSelectorElemComp(apartmetfinale, MetSpeelsterkte, Titel) :-
  schemadoel(afdruk),
  write("\n<TR><TD style=\"vertical-align:top\"><TABLE border=0 cellpadding=0 cellspacing=0>"),
  write("\n<TR><TH colspan=4 class=finales>",Titel,"</TH></TR>\n"),
  %write("\n<tr><td>&nbsp;</td></TR>"),
  wtypes(WType),
  geslachten(Geslacht),
  retract(groepslid(WC, Short, _Lang, Geslacht, WType, _, _, _)),
  %htmlNaam(WC, _HNaam, Link),
  writef("\n<TR><TD colspan=4 style=\"vertical-align:top;white-space:nowrap; text-decoration: underline; font-weight: bold\">%s</td></tr>", Short),
  prplan(PlanSel),
  plantm(DagNumP),
  DagTM = PlanSel * DagNumP,
  publiceerBekendeFinale(WC, MetSpeelsterkte, DagTM),
  write("<tr><td>&nbsp;</td></TR>"),
  indexje(I),
  I1 = I + 1,
  assert(indexje(I1)),
  fail.
sheetSelectorElemComp(apartmetfinale, MetSpeelsterkte, Titel) :-
  not(schemadoel(afdruk)),
  write("\n<TR><TD style=\"vertical-align:top\"><TABLE border=0 cellpadding=0 cellspacing=0>"),
  write("\n<TR><TH colspan=4 style=\"white-space:nowrap;text-align:center\">",Titel,"</TH></TR>\n"),
  %write("\n<tr><td>&nbsp;</td></TR>"),
  wtypes(WType),
  geslachten(Geslacht),
  retract(groepslid(WC, Short, _Lang, Geslacht, WType, _, _, _)),
  htmlNaam(WC, _HNaam, Link),
  writef("\n<TR><TD colspan=4 style=\"vertical-align:top;white-space:nowrap\"><%s>%s</A></td></tr>", Link, Short),
  prplan(PlanSel),
  plantm(DagNumP),
  DagTM = PlanSel * DagNumP,
  publiceerBekendeFinale(WC, MetSpeelsterkte, DagTM),
  write("<tr><td>&nbsp;</td></TR>"),
  indexje(I),
  I1 = I + 1,
  assert(indexje(I1)),
  fail.
sheetSelectorElemComp(apartmetfinale, _, _) :-
  write("\n</TABLE></TD></TR>"),
  indexje(I),
  I1 = I + 2,
  assert(indexje(I1)).

sheetSelectorElem(bijgevoegd, LftMVan, LftMTot, Geslacht, WType, Titel, _, _) :-
  write("\n<TD style=\"vertical-align:top;white-space:nowrap\">",Titel,"</TD>\n<TD style=\"vertical-align:top;\"><SPAN style=\"white-space:nowrap\">"),
  findall(Short, groepslidMetLink(Short, Geslacht, WType, LftMVan, LftMTot), ShortList),
  list_to_string(ShortList, "</SPAN> <SPAN style=\"white-space:nowrap\">", SelList),
  write(SelList, "</SPAN>\n</TD>").
sheetSelectorElem(apart, LftMVan, LftMTot, Geslacht, WType, Titel, _, _DagTM) :-
  write("\n<TR><TD style=\"vertical-align:top\"><TABLE>"),
  write("\n<TR><TH style=\"white-space:nowrap;text-align:left\">",Titel,"</TH></TR>\n"),
  findall(Long, groepslidMetLink(Long, Geslacht, WType, LftMVan, LftMTot), LongList),
  list_to_string(LongList, "</TD></TR>\n<TR><TD style=\"vertical-align:top;white-space:nowrap\">", SelList),
  write("\n<TR><TD style=\"vertical-align:top;white-space:nowrap\">",SelList, "\n</TD></TR></TABLE></TD></TR>"),
  count(LongList, 0, Aantal),
  indexje(I),
  I1 = I + 2 + Aantal,
  assert(indexje(I1)), !.
sheetSelectorElem(apartmetfinale, LftOnder, LftBoven, Geslacht, WType, Titel, MetSpeelsterkte, DagTM) :-
  schemadoel(afdruk),
  write("\n<TR><TD style=\"vertical-align:top\"><TABLE border=0 cellpadding=0 cellspacing=0>"),
  write("\n<TR><TH colspan=4 class=finales>",Titel,"</TH></TR>\n"),
  %write("\n<tr><td>&nbsp;</td></TR>"),
  groepslid(WC, Short, _, Geslacht, WType, St, LftT, LftM),
  LftM <= LftBoven,
  LftM > LftOnder,
  retract(groepslid(WC, Short, _, Geslacht, WType, St, LftT, LftM)),
  %htmlNaam(WC, _HNaam, Link),
  writef("\n<TR><TD colspan=4 style=\"vertical-align:top;white-space:nowrap; text-decoration: underline; font-weight: bold\">%s</td></tr>", Short),
  publiceerBekendeFinale(WC, MetSpeelsterkte, DagTM),
  write("<tr><td>&nbsp;</td></TR>"),
  indexje(I),
  I1 = I + 1,
  assert(indexje(I1)),
  fail.
sheetSelectorElem(apartmetfinale, LftOnder, LftBoven, Geslacht, WType, Titel, MetSpeelsterkte, DagTM) :-
  not(schemadoel(afdruk)),
  write("\n<TR><TD style=\"vertical-align:top\"><TABLE border=0 cellpadding=0 cellspacing=0>"),
  write("\n<TR><TH colspan=4 style=\"white-space:nowrap;text-align:center\">",Titel,"</TH></TR>\n"),
  %write("\n<tr><td>&nbsp;</td></TR>"),
  groepslid(WC, Short, _, Geslacht, WType, St, LftT, LftM),
  LftM <= LftBoven,
  LftM > LftOnder,
  retract(groepslid(WC, Short, _, Geslacht, WType, St, LftT, LftM)),
  htmlNaam(WC, _HNaam, Link),
  writef("\n<TR><TD colspan=4 style=\"vertical-align:top;white-space:nowrap\"><%s>%s</A></td></tr>", Link, Short),
  publiceerBekendeFinale(WC, MetSpeelsterkte, DagTM),
  write("<tr><td>&nbsp;</td></TR>"),
  indexje(I),
  I1 = I + 1,
  assert(indexje(I1)),
  fail.
sheetSelectorElem(apartmetfinale, _LftMVan, _LftMTot, _Geslacht, _WType, _Titel, _, _) :-
  write("\n</TABLE></TD></TR>"),
  indexje(I),
  I1 = I + 2,
  assert(indexje(I1)).

sheetSelectorGroep(0, 18, dames, "E", "Meisjes Enkel", links).
sheetSelectorGroep(0, 18, dames, "D", "Meisjes Dubbel", links).
sheetSelectorGroep(0, 18, heren, "E", "Jongens Enkel", rechts).
sheetSelectorGroep(0, 18, heren, "D", "Jongens Dubbel", rechts).
sheetSelectorGroep(18, 200, dames, "E", "Dames Enkel", links).
sheetSelectorGroep(18, 200, dames, "D", "Dames Dubbel", links).
sheetSelectorGroep(18, 200, heren, "E", "Heren Enkel", rechts).
sheetSelectorGroep(18, 200, heren, "D", "Heren Dubbel", rechts).
sheetSelectorGroep(0, 18, gemengd, "D", "Jeugd Gemengd Dubbel", rechts).
sheetSelectorGroep(19, 200, gemengd, "D", "Gemengd Dubbel", links).
sheetSelectorGroep(0, 18, hdnvt, "D", "Jeugd Dubbel", rechts).
sheetSelectorGroep(19, 200, hdnvt, "D", "Dubbelspel", links).
sheetSelectorGroep(0, 18, hdnvt, "E", "Jeugd Enkel", links).
sheetSelectorGroep(19, 200, hdnvt, "E", "Enkelspel", rechts).

geslachten(dames).
geslachten(heren).
geslachten(gemengd).
geslachten(hdnvt).

wtypes("E").
wtypes("D").

diversen(_, "<A href=\"sheet.php?plek=xxx&amp;type=sheet&amp;ond=finales\">Finales/Poules</a>") :-
  schemadoel(upload),
  finalebekend(Bekend),
  Bekend = b_True.
diversen(ShSelect, Link) :-
  schemadoel(upload),
  ShSelect = apart,
  toernooiWeekJaar(_No, _WeekNr, Jaar),
  str_int(JaarS, Jaar),
  naam(_ToernooiNaam, _Nr, _Datum, TicketI, _District, _),
  maketicket(TicketI, Ticket),
  get_Tempdir(_, TempFile),
  deletefile(Tempfile),
  %AV = aanmeldenvars(),
  NAV = ["plek", Ticket, "jaar", JaarS],
  providerX(Prov, _, "", _ProvM),
  format(URL, "http://%s/reglement.php", Prov),
  _Ret = downloadData(URL, TempFile, NAV, _, 0),
  file_str(Tempfile, Str),
  searchstring(Str, "aanwezig", _Pos),
  %write(Str),
  %format(Link, "<A href=\"reglement.php?request=ja&amp;jaar=%s\">Reglement</a>", Ticket, Jaar).
  format(Link, "<A href=\"reglement.php?plek=%s&amp;jaar=%s\">Reglement</a>", Ticket, JaarS).
diversen(apart, "<!--dagmonitor-->") :-
  banentool(ja, _),
  schemadoel(upload).
diversen(ShSelect, "<A href=\"sheet.php?plek=xxx&amp;type=voorpagina\">Voorpagina</A>") :-
  not(ShSelect = apart),
  schemadoel(upload).
diversen(_, "<A href=\"finales.html\">Finales/Poules</a>") :-
  not(schemadoel(upload)),
  finaleBekend(Bekend),
  Bekend = b_True.
diversen(ShSelect, "<A href=\"toernooi.html\">Voorpagina</a>") :-
  not(ShSelect = apart),
  not(schemadoel(upload)).
diversen(_, ShortMetLink) :-
  groepslidMetLink(ShortMetLink, _, _, 0, 99).	% volgens mij de overblijvende: meestal geen???

combine([A,B|Tail], [C|Rest]) :- !,
  format(C, "%s&nbsp;%s", A, B),
  combine(Tail, Rest).
combine(L, L).

restsheets(bijgevoegd, _) :-
  findall(ReekshoofdDag, agendaitem(_, _, _, _, 0, ReeksHoofdDag, _, _), RHList),	% alle reekshoofden
  write("\n<TR><TD style=\"vertical-align:top\">Agenda</TD><TD style=\"vertical-align:top\">"),
  list_to_string(RHList, "&nbsp; ", SelList),
  write("\n", SelList, "</TD></TR>"),
  fail.
restsheets(bijgevoegd, _) :-
  sheetheaders(_, _, _, SpH, _),
  findall(Knop, knop(Knop,_, _, _),Knoppen),
  not(Knoppen = []),
  write("\n<TR><TD style=\"vertical-align:top\">",SpH,"</TD><TD style=\"vertical-align:top\">"),
  list_to_string(Knoppen, "&nbsp; ", SelList),
  write("\n", SelList, "</TD></TR>"),
  fail.
restsheets(bijgevoegd, _) :-
  %schemadoel(upload),
  findall(Opt, diversen(bijgevoegd, Opt), OptList),
  not(OptList = []),
  write("\n<TR><TD style=\"vertical-align:top\">&nbsp;</TD><TD style=\"vertical-align:top\">"),
  combine(OptList, OptListComb),
  list_to_string(OptlistComb, "&nbsp; ", SelList),
  write("\n", SelList, "\n<!--PB-->\n</TD>"), !.  % uitbreiding van sellist &nbsp;Ranglijst
restsheets(apart, rechts) :-
  sheetheaders(_, _, _, _SpH, DagP),
  findall(ReekshoofdDag, agendaitem(_, _, _, _, 0, ReekshoofdDag, _, _), RHList),	% alle reekshoofden
  not(RHList = []),
  write("\n<TR><TD style=\"vertical-align:top\"><TABLE>"),
  write("\n<TR><TH style=\"white-space:nowrap;text-align:left\">",DagP,"</TH></TR>"),
  list_to_string(RHList, "</TD></TR>\n<TR><TD>", SelList),
  write("\n<TR><TD>", SelList, "</TD></TR></TABLE></TD></TR>"),
  fail.
restsheets(apart, links) :-
  sheetheaders(_, _, _, SpH, _),
  findall(Knop, knop(Knop,_, _, _),Knoppen),
  not(Knoppen = []),
  list_to_string(Knoppen, "</TD></TR>\n<TR><TD>", SelList),
  write("\n<TR><TD style=\"vertical-align:top\"><TABLE>"),
  write("\n<TR><TH style=\"white-space:nowrap;text-align:left\">",SpH,"</TH></TR>"),
  write("\n<TR><TD>", SelList, "</TD></TR></TABLE></TD></TR>"),
  fail.
restsheets(apart, rechts) :-
  findall(Opt, diversen(apart, Opt), List),
  not(List = []), !,
  list_to_string(List, "</TD></TR>\n<TR><TD>", SelList),
  write("\n<TR><TD style=\"vertical-align:top\"><TABLE>"),
  write("\n<TR><TH style=\"white-space:nowrap;text-align:left\">","Diversen","</TH></TR>"),
  write("\n<TR><TD>", SelList, "</TD></TR></TABLE></TD></TR>"),
  fail.
restsheets(apart, rechts) :-
  write("\n<!--PA-->"), !.
restsheets(_, _).

omlijstMetTR(apartmetfinale, rechts) :-		% hier gaat ie van links naar rechts
  huidigeKolom(links),
  assert(huidigeKolom(rechts)),
  write("\n</TABLE></TD><TD style=\"vertical-align:top\"><TABLE>"),
  fail.
omlijstMetTR(apartmetfinale, _).
omlijstMetTR(apartmetfinale, _) :- 
  fail.					% backtrack
%
omlijstMetTR(apart, rechts) :-
  huidigeKolom(links),
  restsheets(apart, links),
  fail.
omlijstMetTR(apart, rechts) :-		% hier gaat ie van links naar rechts
  huidigeKolom(links),
  assert(huidigeKolom(rechts)),
  write("\n</TABLE></TD><TD style=\"vertical-align:top\"><TABLE>"),
  fail.
omlijstMetTR(apart, _).
omlijstMetTR(apart, _) :- 
  fail.					% backtrack
%
omlijstMetTR(bijgevoegd, _) :-
  indexje(I),
  I mod 2 = 0,
  write("\n<TR>"),
  fail.
omlijstMetTR(bijgevoegd, _) :-
  indexje(I),
  I1 = I + 1,
  assert(indexje(I1)).
omlijstMetTR(bijgevoegd, _) :-
  indexje(I),
  I mod 2 = 0,
  write("</TR>"),
  fail. 

kortLang(bijgevoegd, InKort, _, InKortU) :-
  zonderHekje(InKort, "", InKortU), !.
kortLang(apartmetfinale, InKort, _, InKortU) :-
  zonderHekje(InKort, "", InKortU), !.
kortLang(_, _, InLang, InLangU) :-		% apart en apartmetLink
  zonderHekje(InLang, "", InLangU), !.

linksRechts(links).
linksRechts(rechts).

postHtmlPrime(apart, PlanSel, ZonderWebLinks, _Reserve) :-
  ZonderWebLinks = b_False,
  naam(TNaam, _, _, _, _, _),
  %write("\n</TABLE>"),
  write( "\n<TABLE BORDER=\"0\" CELLPADDING=\"0\" CELLSPACING=\"0\">"),	
  write("\n<TR><TD colspan=2>"),
  writef("<SPAN class=ondnaam>%s</SPAN>&nbsp;&nbsp;", TNaam),
  write("</TD></TR>"),
  write("\n<TR><TD width=400></TD><TD>"),
  overzichtref(apart, ZonderWebLinks),
  write("</TD></TR>"),
  write("\n<TR><TD></TD><TD>"),
  agendaref(apart, PlanSel, ZonderWebLinks),
  write("\n</TD></TR><TR><TD></TD><TD>"),
  spagendaref(apart, _Reserve, ZonderWebLinks),
  write("</TD></TR>"),
  write("</TABLE>"),
  postHtml(ZonderWebLinks), !.
postHtmlPrime(bijgevoegd, _PlanSel, b_False, _Reserve) :-
  naam(TNaam, _, _, _, _, _),
  writef("<SPAN class=ondnaam>%s</SPAN>&nbsp;&nbsp;", TNaam),
  postHtml(b_False), !.
postHtmlPrime(_, _PlanSel, _, _Reserve) :-
  naam(TNaam, _, _, _, _, _),
  write("<SPAN class=ondnaam>",TNaam,"</SPAN>&nbsp;&nbsp;"),
  postHtml(b_True), !.

overzichtref(apart, b_False) :-
  schemadoel(upload),
  write("\n<BR><SPAN id=onderdelen><A href=\"sheet.php?plek=xxx&amp;type=voorpagina\">Voorpagina</A></SPAN><BR>"), !.
overzichtref(apart, b_False) :-
  write("\n<BR><SPAN id=onderdelen><A href=\"toernooi.html\">Voorpagina</A></SPAN><BR>"), !.
overzichtref(_, _).


predicates
procedure borderColor(string Color)	% verschil tussen printen en op scherm
procedure afdrukColor(string)
%determ filenaamerr(string Naam, integer Return)

clauses

writeStyleSheet(FontGr) :-
  FG1 = FontGr + 1,
  FG3 = FontGr + 4,
  borderColor(BorderColor),
  afdrukcolor(Color1),
  writef("/*Toernooi Assistent versie %s stylesheet voor Webpagina's */", versie),
  write("\n@page {size: auto; margin: 2mm}"),
  write("\nSPAN.mededeling {font-weight: bold}"),
  write("\nDIV.spelertitel {font-weight: bold; text-align: center}"),
  write("\nDIV.toernooinaam {font-weight: bold; text-align: center}"),
  write("\nSPAN.logo { font-size:8pt; white-space: nowrap}"),
  write("\nSPAN.geplaatst { color: red}"),
  write("\n/* Afvalschema's */"),
  write("\nTD {padding-left:2; padding-right: 2; padding-top: 0; padding-bottom: 0}"),
  write("\nTD.balk { color: white; background-color: black; text-align: center; font-weight: bold; height: 15}"),
  write("\nTD.boven { vertical-align:top; white-space: nowrap; padding-left: 2; text-align: center}"),
  write("\nTD.bovenZ { vertical-align:top; white-space: nowrap; padding-left: 2; text-align: left}"),
  write("\nTD.bovenB { vertical-align:top; white-space: nowrap; border-style: solid; border-right-width: 1; border-top-width: 1; border-left-width: 0; border-bottom-width: 0; padding-left: 2; text-align: left}"),
  write("\nTD.bovenBz { vertical-align:top; white-space: nowrap; border-style: solid; border-right-width: 0; border-top-width: 1; border-left-width: 0; border-bottom-width: 0; padding-left: 2; text-align: left}"),
  write("\nTD.bovenF {text-align: center; vertical-align:top; white-space: nowrap; border-style: solid; border-right-width: 1; border-top-width: 0; border-left-width: 0; border-bottom-width: 0}"),
  write("\nTD.beneden { vertical-align:bottom; white-space: nowrap; padding-left: 2}"),
  write("\nTD.benedenR { vertical-align:bottom; white-space: nowrap; padding-left: 2; padding-right: 2; text-align: right}"),
  write("\nTD.benedenM { vertical-align:bottom; white-space: nowrap; border-style: solid; border-right-width: 1; border-top-width: 0; border-left-width: 0; border-bottom-width: 1; padding-left: 2}"),
  write("\nTD.benedenC {text-align: center; vertical-align:bottom; white-space: nowrap; border-style: solid; border-right-width: 1; border-top-width: 0; border-left-width: 0; border-bottom-width: 1; padding-left: 2}"),
  %write("\nTD.benedenF {vertical-align:bottom; white-space: nowrap; border-style: solid; border-right-width: 0; border-top-width: 0; border-left-width: 0; border-bottom-width: 1;min-width:140}"),
  write("\nTD.benedenF {vertical-align:bottom; white-space: nowrap; border-style: solid; border-right-width: 0; border-top-width: 0; border-left-width: 0; border-bottom-width: 1}"),
  write("\nTD.benedenB {text-align: center; vertical-align:bottom; white-space: nowrap; border-style: solid; border-right-width: 0; border-top-width: 0; border-left-width: 0; border-bottom-width: 1; padding-left: 1}"),
  write("\n/* Poules */"),
  write("\nTABLE.poule { border-width: 0;}"),
  write("\nCAPTION.poule {font-weight: bold; text-align: left}"),
  write("\nTH.pouleh {font-weight: bold; text-align: center; padding-left: 2; padding-right: 2; border-style: solid;  border-right-width: 1; border-top-width: 0; border-left-width: 0; border-bottom-width: 1;}"),
  write("\nTH.poulehr {font-weight: bold; text-align: center; padding-left: 2; padding-right: 2; border-style: solid; border-right-width: 0; border-top-width: 0; border-left-width: 0; border-bottom-width: 0;}"),
  write("\nTD.poulen { text-align: left; white-space: nowrap; padding-left: 2; padding-right: 2;border-style: solid; border-right-width: 1; border-top-width: 0; border-left-width: 0; border-bottom-width: 1;}"),
  write("\nTH.poulenr { text-align: center; white-space: nowrap; padding-left: 2; padding-right: 2;border-style: solid; border-right-width: 1; border-top-width: 0; border-left-width: 0; border-bottom-width: 0;}"),
  write("\nTH.pouleo { text-align: center; white-space: nowrap; padding-left: 2; padding-right: 2;border-style: solid; border-right-width: 0; border-top-width: 0; border-left-width: 0; border-bottom-width: 1;}"),
  write("\nTD.pouleu { text-align: center; vertical-align: middle; white-space: nowrap;border-style: solid; border-right-width: 1; border-top-width: 0; border-left-width: 0; border-bottom-width: 1;}"),
  write("\nTD.pouleuw { text-align: center; vertical-align: middle;border-style: solid; border-right-width: 1; border-top-width: 0; border-left-width: 0; border-bottom-width: 1;}"),
  write("\nTD.pouleus { text-align: center; vertical-align: middle; white-space: nowrap;text-decoration:line-through;border-style: solid; border-right-width: 1; border-top-width: 0; border-left-width: 0; border-bottom-width: 1;}"),
  write("\nTD.winnaar { text-align: center; vertical-align: middle; white-space: nowrap;font-weight:bold;border-style: solid; border-right-width: 1; border-top-width: 0; border-left-width: 0; border-bottom-width: 1;}"),
  write("\nTD.pouled {text-align: center; vertical-align: middle; color: lightgrey; font-size:11pt; border-color: ", BorderColor, ";"),
  write(" border-style: solid; border-right-width: 1; border-top-width: 0; border-left-width: 0; border-bottom-width: 1;}"),
  write("\nTD.pouleo { text-align: center; white-space: nowrap; padding-left: 2; padding-right: 2; border-style: solid; border-right-width: 0; border-top-width: 0; border-left-width: 0; border-bottom-width: 1;}"),
  write("\n/* Dagagenda */"),
  write("\nTD.agendab {text-align: left; white-space: nowrap; font-weight: bold; padding-left: 2; padding-right: 2}"),
  write("\nTD.agendan {text-align: left; white-space: nowrap; font-weight: normal; padding-left: 2; padding-right: 2; padding-top : 8; vertical-align : top}"),
  write("\nTD.agendar {text-align: right; white-space: nowrap; font-weight: normal; padding-left: 2; padding-right: 2; padding-top : 8; vertical-align : top}"),
  write("\nTD.agendak {font-size:8pt; text-align: left; white-space: nowrap; font-weight: normal; padding-left: 2; padding-right: 2}"),
  write("\n/* Overzicht */"),
  write("\nTD.overz {text-align: left; white-space: nowrap; font-weight: normal; padding-left: 2; padding-right: 2; vertical-align: bottom}"),
  write("\nTD.overzn {text-align: left; white-space: nowrap; font-weight: normal; padding-left: 2; padding-right: 2; vertical-align: bottom}"),
  write("\nTD.overzr {text-align: right; white-space: nowrap; font-weight: normal; padding-left: 2; padding-right: 2; vertical-align: bottom}"),
  %write("\nTABLE.agenda {}"),
  write("\n/* referenties zet op display: none als je de referenties niet benodigt */"),
  write("\nSPAN.agendaref {white-space: nowrap}"),
  write("\n   #spelerref {visibility: visible;white-space:nowrap}"),
  write("\n   #agendaref {visibility: visible;white-space:nowrap}"),
  write("\n   #onderdelen {visibility: visible;white-space:nowrap}"),
  write("\nSPAN.spelerref {white-space:nowrap}"),
  %write("\n @media print {\n  #menu {display : none}\n  #menu1 {display : none}\n}"),

  write("\nBODY {background-color: white; font-family: Arial, Verdana, Helvetica; font-size:",FontGr,"pt; color:",Color1,";}"),
  write("\nTABLE {font-size:", FontGr,"pt; border-color : ",BorderColor, ";}"),
  write("\nDIV.toernooinaam { font-size:",FG3,"}"),
  write("\nTH.finales {text-align: center; font-size:",FG3,"; font-weight: bold}"),
  write("\nTD.overz {font-size:",FG1,"pt}").


borderColor(BorderColor) :-		% op het scherm
  BorderColor = "#696969",	% dimgray
  !.

afdrukcolor("black") :-
  geenkleurPrint(), !.
afdrukcolor("navy").

/*
filenaamErr(Naam, _Ret) :-
  format(Msg, "kan %s niet openen!", Naam),
  dlg_MessageBox( "Cssbestand", Msg, mesbox_iconerror, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.
*/
closeOpen(_Filenaam, Kort, Type) :-
  schemadoel(upload),
  write("***[", Type, "][", Kort, "]***\n"), !.
closeOpen(Filenaam, _, _) :-
  closefile(work),
  %write("\n", Filenaam),
  openwrite(work,Filenaam),
  writedevice(work), !.


% ====================================================================================

database - html
single firsttime(BOOLEAN)
single eerstekeer(BOOLEAN)
  schemaItem(integer Regel, integer Kol, integer Rowspan, string RepH1, string Color1, string ColorEnd, string Anker, string CSS)

predicates
procedure eindeBestand(beeld, boolean, shselect SheetSel,catl Onderdelen)
procedure logoErBijBot()
procedure weblayoutSettings(beeld Upload, beeld Web)
procedure publiceerOnderdeel(wedscat, integer DagTM, boolean ZonderWebLinks, boolean MetSpeelsterkte, kolomTypeL, integer Niveau, boolean ZonderVerlRonde) - (i, i, i, i, i, i, i)
procedure dependOnd(wedscat, integer, boolean ZonderWebLinks, boolean MetSpeelsterkte, kolomtypeL, integer Niveau, boolean ZonderVerlRonde)
procedure publiceerHTML(WEDSCAT, integer DagTM, boolean ZonderWebLinks, boolean MetSpeelsterkte, kolomtypel, integer Niveau)
procedure publiceerPoule(WEDSCAT, integer DagTM, boolean ZonderWeblinks, boolean MetSpeelsterkte, kolomtypeL, integer Niveau)
procedure ondWijzer(wedscat, string)
procedure afdrukstijl(integer Aantal, string Stijlstring)
nondeterm opgInWeds(wedscat Cat, integer Nr)
procedure alleregels(wedscat Cat, integer Card, integer TmDag, boolean ZonderWebLinks, boolean MetSpeelsterkte,kolomtypel, boolean MetNummers, boolean HilitePlaatsing)
procedure writeregel(string Num, string ExtraKols, string Style, string Color,
  string Naam, string ClassNaam, string ColorEnd, string Sterkte, integer RegelNo, boolean ZonderWebLinks)
procedure extrakols(string Class, string Style, slist, integer, string In, string Uit) - (i,i,i,i,i,o)
procedure metSpeelsterkte(boolean, janee Echt, string Bondsnummer, string CSSclass, string, string)
procedure br2style(integer, boolean MetNummers, string, string, string, integer Num, string Boven, string Beneden)
procedure metNummer(boolean MetZonder, integer In, string Uit)
procedure blokken(wedscat Cat, integer TDim, integer TmDag, boolean HilitePlaats)
procedure blok(wedscat Cat, integer Kol, integer Dim, integer TmDag)
procedure blokfinale(wedscat WC, integer Kol, integer Dim, integer TmDag)
procedure rondeTekst(integer, integer, string)
procedure repH(wedscat, integer WedsNo, integer UpDown, string Spelers, string Uitslag, integer TmDag, string Color, string ColorEnd, string Anker, boolean HilitePl)
procedure zonderFje(string, string, string, string)
procedure geplaatstH(wedscat, integer No, integer No, tgnst Tgnst, string Tekst, string Color, string ColorEnd, boolean Hilite)
procedure geplaatstH1(wedscat, integer No, integer No, tgnst Tgnst, string Tekst, string Color, string ColorEnd)
upDown(integer, tgnst, tgnst, tgnst)
procedure movepark(janee, string, string, string)
procedure plus2br(string RepH3, string RepH31)
procedure kolomheaders(integer)
procedure kolomheader(wedscat, integer Card, integer KolomAantal)
procedure boodschap(BOOLEAN, wedscat, string)
procedure ranglijst(beeld, boolean, wedscat, kolomtypel, kolomtypel)
procedure openbestand(beeld, shselect SheetSel, boolean, wedscat, string Stylesheet, string OnderdeelSelected,catl Onderdelen)
determ    openbestand1(shselect SheetSel, string FileNaam, string OnderdeelSelected, string StyleSheet, catl Onderdelen)
htmlnaamError(string)
nondeterm   eindeAaneengeregen(beeld , boolean)
procedure closefileAls(beeld)
nondeterm expandC(catl, wedscat)
procedure deletehtml(beeld)
procedure beginaaneen(string Stylesheetnaam)
procedure writeblok(integer Regel, boolean ZonderWeblinks)
procedure voorPrint(beeld PrintOfWeb,  boolean ZonderWeblinks)
procedure   aanEenGeregen(boolean) - (o)		% individuele afdruk
determ openwriteAls(beeld, string Filenaam, string Kort)
determ outHTMLftp(boolean Noshow, string FTPadres, string Gebruiker, string Wachtwoord, catl)
procedure namenRows(wedscat, integer)
%procedure printByRonde(wedscat Hoofd, integer DagTM, boolean ZonderWebLinks, boolean MetSpeelsterkte,kolomtypel, integer Niv)
determ ftpafgelopen(integer Returncode)
nondeterm hoofdronde(boolean, catl, wedscat)
procedure invoerscript()
procedure kolommen2link(kolomtypel, beeld, kolomtypel) - (i,i,o)

clauses
firsttime(b_False).
eerstekeer(b_False).

hoofdronde(b_False, CatL, Cat) :-
  member(Cat, CatL),
  not(kopl(Cat,verliezer,_,_)).
  %not(wc(_,_,_,_,_,_,Cat)).
hoofdronde(b_True, CatL, Cat) :-
  member(Cat, CatL).

invoerscript() :-
  uitslagenable(_),
  comline(LineBuffer),
  upper_lower(LineBuffer, LBL),
  searchstring(LBL, "/x", _),		% TEST
  write("<SCRIPT LANGUAGE=\"JavaScript\" type=\"text/javascript\" src=\"jquery-1.3.2.min.js\"> \n</SCRIPT>\n"),
  fail.
invoerscript() :-
  uitslagenable(_), !,
  write("<SCRIPT LANGUAGE=\"JavaScript\" type=\"text/javascript\" SRC=\"uitslaginvoerhelper.js\">\n</SCRIPT>\n"),
  write("<STYLE TYPE=\"text/css\" MEDIA=screen>\n  .uitslag  {   background-color: #FFCCFF; }\n .winner {text-decoration: underline; cursor:pointer}\n </STYLE>\n"),
  !.
invoerscript().

kolommen2Link([bondsnummer|R], upload, Uit) :-  	% 13.10.2011  introductie van links ipv kolommen!
  kolommen2Link(R, upload, Uit), !.
kolommen2Link([H|R], Beeld, [H|Uit]) :-
  kolommen2Link(R, Beeld, Uit), !.
kolommen2Link(_, _, []).

generateHTML(Print, CatLIn) :-
  retractall(uitslagenableE(_, _, _, _, _)),	% 17.2.2011
%  uitslagendownload(b_False), 	% 25.10.2011
  assert(schemadoel(Print)),
  resetspelerprogramma(),
  assert(eerstekeer(b_True)),
  assert(firsttime(b_True)),
  voorPrint(Print, ZonderWebLinks),
  weblayoutsettings(Print, Print1),
  weblayout1(Print1, _IDCW_TOERNOOICSS_DEFAULT, Settings1,Kolommen, _Settings2, _IDCW_MEDEDELING_VALUE, _CatLXX),
  Settings1 = [_IDCW_RANGLIJST_CHECKED, _IDCW_ZWARTWIT_CHECKED, VerlRondeApart, _IDCW_AANEENGEREGEN_CHECKED,MetSpeelsterkte,_IDCW_SHEET_SLECTOR_CHECKED,_IDCW_INCL_LOGOBESTANDEN_CHECKED],
  %Settings2 = [Fontgr],
  deletehtml(Print),
  %checkStyleSheet(_IDCW_TOERNOOICSS_DEFAULT, FontGr),
  beginAaneen(_IDCW_TOERNOOICSS_DEFAULT),
  prplan(PlanSel),
  cursor_SetWait(),
  plantm(DagNumP),
  DagNumP1 = PlanSel * DagNumP,
  % scan de planning nu al om te zien of dagagenda resp. spelerwijzer relevant zijn
  findall(Cx, hoofdronde(VerlRondeApart, CatLIn, Cx), CatL),
  findall(CatL1, expandC(CatL, CatL1), CatL2),
  findall(Dagv,dag(Dagv, _, _), Dagenv),
  Dagenv = [EersteDagv | _],
  reverse(Dagenv, [], DagenR),
  DagenR = [LaatsteDagv|_],
  Tijdspan = LaatsteDagv - EersteDagv ,
  scanSpelerProgramma(CatL2, DagNumP1,_IDCW_TOERNOOICSS_DEFAULT, ZonderWeblinks, Tijdspan),
  zoekHTMLnamen(CatL, VerlRondeApart),	% 19.6.2009
  % maak een overzicht van de betreffende onderdelen
  menustr(CatL),
  publiceerOverzicht(bijgevoegd, CatL, _IDCW_TOERNOOICSS_DEFAULT, ZonderWebLinks, _IDCW_MEDEDELING_VALUE),
  finalepagina(bijgevoegd, CatL, _IDCW_TOERNOOICSS_DEFAULT, ZonderWebLinks, _IDCW_MEDEDELING_VALUE, MetSpeelsterkte),
  closefileAls(Print),
  assert(eerstekeer(b_True)),
  aaneengeregen(Aaneengeregen),
  eindeAaneengeregen(Print, Aaneengeregen),
  % en publiceer de onderdelen zelf
  member(CNo, CatL),	
    wc(CNo, _OnderdeeelID,_Orde,  OnderdeelSelected, _EDG, _Kort, _SexL, _Speelsterkte, _LeeftV, _LeefTM, _HDG, _Schemasoort, _Keuring, _KNLTB, _Geld),
    %wc(CNo,OnderdeelSelected,_,_,_,_,_),
    openBestand(Print, bijgevoegd, Aaneengeregen, CNo, _IDCW_TOERNOOICSS_DEFAULT, OnderdeelSelected, CatL),
    ranglijst(Print, _IDCW_RANGLIJST_CHECKED, CNo, Kolommen, KolommenR),
    kolommen2Link(KolommenR, Print, KolommenRL),
    publiceerOnderdeel(CNo, DagNumP1, ZonderWeblinks, MetSpeelsterkte, KolommenRL, 1, VerlRondeApart),	% of
    eindeBestand(Print, Aaneengeregen, bijgevoegd, CatL),
  fail.
generateHTML(Print, CatL) :-
  voorPrint(Print, ZonderWebLinks),
  weblayoutsettings(Print, Print1),
  weblayout1(Print1, _IDCW_TOERNOOICSS_DEFAULT, Settings1,_Kolommen, Settings2, _IDCW_MEDEDELING_VALUE, _CatLXX),
  Settings1 = [_IDCW_RANGLIJST_CHECKED, _IDCW_ZWARTWIT_CHECKED, _Reserve, _IDCW_AANEENGEREGEN_CHECKED,_MetSpeelsterkte,_IDCW_SHEET_SLECTOR_CHECKED,_IDCW_INCL_LOGOBESTANDEN_CHECKED],
  Settings2 = [Fontgr],
  findall(CatL1, expandC(CatL, CatL1), _CatL2),
  TaskWin = vpi_GetTaskWin(),
  get_htmlmap(Dir),
  cursor_SetWait(),
  closefileAls(Print),
  agendaitem(Dag, _, _, _, 0, _, _, _),  % publiceer de agenda, per reekshoofd
    findall(RDag, agendaitemStr(RDag,Dag), RDagen),
    dag(Dag,RapDag,_), 
    RDagenC = [RapDag|RDagen],
    upper_lower(RapDag, RapDagL),
    fronttoken(RapDagL, Tok, Rest1),
    fronttoken(Rest1, Tok1, Rest2),
    fronttoken(Rest2, _, Rest3),
    fronttoken(Rest3, Tok2, _),
    format(NietDeletenL, "AGENDA%s%s-%s.HTML", Tok, Tok1, Tok2),
    upper_lower(NietDeleten, NietDeletenL),
    retractall(teDeleten(NietDeleten)),
    upper_lower(NietDeletenL, FilNaam),
    filenamepath(Filenaam, Dir, FilNaam),
    %format(Filenaam, "%sagenda%s%s-%s.html", Dir, Tok, Tok1,Tok2),
    format(Kort, " %s%s-%s", Tok, Tok1, Tok2),
    trim_c(Kort),
    openwriteAls(Print, Filenaam, Kort),
    writedevice(work),			% ##############@@@@@@@@@@@@@@@@############
    metaTag(Eerst,Tag),
    writef("%s\n<HTML><HEAD>%s<META HTTP-EQUIV=\"Expires\" CONTENT=\"Tue, 24 Oct 1996 17:45:00 GMT\"><TITLE>Agenda en/of uitslagen %s</TITLE>",Eerst,Tag,RapDag),
    tasversie(Versie),
    writef("\n<!-- ToernooiAssistent versie %s -->",Versie),
    %write("\n<LINK rel=stylesheet Type=\"text/css\" href=\"sheet.php?plek=xxx&amp;type=css&amp;naam=", _IDCW_TOERNOOICSS_DEFAULT, "\">"),
    styleSheet(_IDCW_TOERNOOICSS_DEFAULT),
    invoerscript(),
    write("</HEAD>"),
    write("\n<BODY>"),
    logoErBijTop(),
    toernooiDiv(),
    write("\n<!-- #### naar sluitenMaarAgenda -->\n"),
    sluitenMaarAgenda(bijgevoegd, ZonderWebLinks, CatL),
    write("\n<!-- #### naar banen_file -->\n"),
    %banen_file(TaskWin, RDagenC,RapDag,Filenaam,4,[],b_False,[], ZonderWeblinks,Fontgr), 
    banen_file(TaskWin, RDagenC,RapDag,Filenaam,4,_CatL2,b_False,[], ZonderWeblinks,Fontgr),  %+++++++++++++++++++++++++
  fail. 
generateHTML(Print, CatL) :-
  voorPrint(Print, ZonderWebLinks),
  not(CatL = []),
  weblayoutsettings(Print, Print1),
  weblayout1(Print1, IDCW_TOERNOOICSS_DEFAULT, Settings1,_Kolommen, _Settings2, _IDCW_MEDEDELING_VALUE, _CatLXX),
  Settings1 = [_IDCW_RANGLIJST_CHECKED, _IDCW_ZWARTWIT_CHECKED, _VerliezersApart, _IDCW_AANEENGEREGEN_CHECKED,_MetSpeelsterkte,_IDCW_SHEET_SLECTOR_CHECKED,_IDCW_INCL_LOGOBESTANDEN_CHECKED],
  %prplan(PlanSel),
  %PlanSel = 1,	% 2.2.2012
  plantm(DagNumP),
  % de spelerprogramma's
  publiceerSpelerProgramma(bijgevoegd, b_True, CatL, DagNumP,IDCW_TOERNOOICSS_DEFAULT, ZonderWeblinks),
  fail.
generateHTML(Print, _CatL) :-		% opruimen
  closefile(work),
  resetspelerprogramma(),	% = retractall(agendaitem(_, _, _, _, _, _)), retractall(knop(_,_)),
  not(Print = upload),
  get_htmlmap(Dir),
  retract(teDeleten(Short)),
  filenamepath(FullName,Dir,Short),
  deletefile(FullName),
  fail.
generateHTML(_, _).


% ###################### versturen of browsen
outHTML(_, _, _) :-
  cleankoppelingen(),
  fail.
outHTML(afdruk, PreselectBew, _) :-
  %htmlnaam(X, Naam, _),
  generateHTML(afdruk, PreselectBew),
  get_tempdir(Webmap, _),
  filenamepath(Schema, Webmap, "schema.html#deze"),
  deletefile(Schema),
  _Result = vpi_ProcessEvents(),
  format(URL1, "file:\\\\\\%s", Schema),
  browser_ctl(URL1), !.
%  Null = cast(api_hwnd,0),
%  api_ShellExecute(Null,"open",URL1,"","",1), !.		% open in browser
outHTML(afdruk,  _X, _) :- 
  Null = cast(api_hwnd,0),
  get_tempdir(Webmap, _),
  filenamepath(Lijst, Webmap, "schema.html"),
  api_ShellExecute(Null,"open",Lijst,"","",1), !.		% open lijst desnoods in browser
outHTML(uploadftp, CatList, NoShow) :-
  erIsGeupload(),		% dus automatisch resetten	
  autowebP(_Regelmaat,_,_Waarschuwen,Gebruiker,Wachtwoord, FTPadres, _Pad, _Vasteverb, _Hoevaak, _BijSluiten),
  outHTML(upload, CatList, NoShow),
  %uploadboodschap(Waarschuwen,Vasteverb),
  %b_True = vragenOmverbinding(Vasteverb, b_False),
  outHTMLftp(NoShow, FTPadres, Gebruiker, Wachtwoord, CatList), !.
outHTML(upload, _PreselectBew, _NoShow) :-
  geenServer(_),
  dlg_note("geen verbinding. Start opnieuw!"), !.	% doe niets
/*outHTML(upload, PreselectBew, _NoShow) :-
  generateHTML(upload, PreselectBew),
  comline(LineBuffer),
  upper_lower(LineBuffer, LBL),
  searchstring(LBL, "/x", _),		% TEST
  dlg_note("test-geen upload"), !. */ 
outHTML(upload, PreselectBew, NoShow) :-
  generateHTML(upload, PreselectBew),
  erIsGeupload(), 		% dus automatische uploadevents resetten	
  get_tempdir(Dir, _),
  filenamepath(Bestand, Dir, "AA_Opsturen"),
  filenamepath(Report, Dir, "uitklaar.html"),
  file_str(Report,"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\"><html><head>Iets mis gegaan</head><body>Er is een fout opgetreden bij het opsturen.<br>Was de Internetverbinding OK?</body></html>"),
  not(toernooiNaamWebOngelijk(Noshow)),	
  providerX(Prov, PN, "", _),
  checkvoortgang(0, "Voortgang"),
  format(Bezig, "bezig met uploaden van de schema's naar %s", PN),
  checkvoortgang(1, Bezig),
  tracezelf("sheetupload"),
  format(URL, "http://%s/%s?stap=een&type=sheets", Prov, "webload.php"),
  Vars = aanmeldenVars(),
  str_int(NSstr, NoShow),	% 18.4.2010 autoupload nog wel nodig?
  concat(Bestand, ".gz", BestandC),
  file_compress(Bestand, BestandC),
  ReturnK = fileupload(URL, BestandC, Report, ["autoupload",NSstr | Vars]),
  write(ReturnK),
  checkvoortgang(3, "bezig met wedstrijden klaar zetten"),
  tracezelf("uitslagenableupload start"),
  uitslagenableUpload(),	% 17.2.2011
  checkvoortgang(100, ""),  % sluit weer
  tracezelf("uitslagenableupload klaar"),
  file_str(Report, Tekstt),
  write("\n",Tekstt),
  Session = visSession(Report, "BEHEER="),
  write("\nSession: ",Session),
  synchKlapper(ReturnK, Report, _),
  NoShow = b_False,
  format(KURL, "http://%s/webload.php?%s&stap=twee", Prov, Session),
         %win_editor_Create(Taskwin, Text, Titel),
  Null = cast(api_hwnd,0),
  api_ShellExecute(Null,"open",KURL,"","",1), !.		% open in browser
outHTML(_, _, _).

traceZelf(String) :-
  comline(LineBuffer),
  upper_lower(LineBuffer, LBL),
  searchstring(LBL, "/x", _),		% in TEST
  time(Hours,Minutes,Seconds,Hundredths),
  format(Msg, "\n%s om %u:%u:%u:%u", String, Hours,Minutes,Seconds,Hundredths),
  writedevice(X),
  writedevice(screen),
  write(Msg),
  _Result = vpi_ProcessEvents(),
  writedevice(X), !.
traceZelf(_).

outHTMLftp(_NoShow, FTPadres, Gebruiker, Wachtwoord, CatList) :-
  checkvoortgang(0, "Voortgang"),
  %
  generateHTML(uploadftp,CatList),
  get_htmlmap(Webmap),
  filenamepath(Wild, Webmap, "*.htm*"),
  findall(FNam, dirfiles(Wild,0x40,FNam,_,_,_,_,_,_,_,_), FList),
  weblayoutsettings(uploadftp, Print1),
  weblayout1(Print1, CSS, _Settings1,_Kolommen, _Settings2, _IDCW_MEDEDELING_VALUE, _CatLXX),
  FList1 = [CSS|FList],
  format(URLF, "ftp://%s",FTPadres),
  format(GebruikerWW,"%s:%s",Gebruiker, Wachtwoord),
  format(BezigF, "ftp naar %s..", URLF),
  checkvoortgang(2, BezigF),
  Return = uploadFTP(URLF, Webmap, FList1, "rapport.txt", GebruikerWW, 0),
  ftpafgelopen(Return),
  fail.
outHTMLftp(_, _, _, _, _).

ftpafgelopen(0) :-
  checkvoortgang(98, "Uploadftp: OK!"),
  checkvoortgang(99, ""), !.
ftpafgelopen(X) :-
  checkvoortgang(100, ""),
  format(Klaar, "Fout in FTP: %u", X),
  _A = dlg_MessageBox( "FTP fout", Klaar, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), !.

editie() :-
  date(DY, DM, DD),
  time(TH, TM, _, _),
  helestr_int(0,TMS, TM),
  sleuteltekst1(Park), 
  trim_c(Park), !,
  writef("<B>%s</B>. Editie %u/%u/%u %u:%s. ", Park, DD, DM, DY, TH, TMS).
editie().
  
eindeBestand(afdruk, _, _, _) :- 
  editie(),
  logoErBijBot(),
  copyright(), !.	% apart
eindeBestand(_, _, SheetSel, OnderdelenR ) :-
  verzamelIndex(SheetSel,OnderdelenR, b_True, b_True,""),	% 22.2.2004
  editie(),
  logoErBijBot(),
  copyright(),
  write("</BODY></HTML>\n"), !.
eindeBestand(_, _, _SheetSel, _OnderdelenR) :-
  exit().

logoErBijBot() :-
  weblayout1(web, _IDCW_TOERNOOICSS_DEFAULT, Settings1,_Kolommen, _Settings2, _IDCW_MEDEDELING_VALUE, _PreselectBew),
  Settings1 = [_IDCW_RANGLIJST_CHECKED, _IDCW_ZWARTWIT_CHECKED, _Reserve, _IDCW_AANEENGEREGEN_CHECKED,_IDCW_MET_SPEELSTERKTE_CHECKED,_IDCW_SHEET_SLECTOR_CHECKED,_IDCW_INCL_LOGOBESTANDEN_CHECKED],
  _IDCW_INCL_LOGOBESTANDEN_CHECKED = b_True,
  get_htmlmap(Dir),
  filenamepath(LogoFile, Dir, "sponsorB.txt"),
  existfile(LogoFile),
  file_str(Logofile, Str),
  write("\n", Str), !.
logoErBijBot().

weblayoutSettings(upload, web) :- !.
weblayoutSettings(uploadftp, web) :- !.
weblayoutsettings(X, X).
/*
printByRonde(CatV, DagTM, ZonderWebLinks, MetSpeelsterkte,KolomTypeL, Niv) :-
  member(R, [1,2,3]),
  writef("\n<hr><div style=\"text-align: center;text-decoration: bold;\">Ronde %u</div><hr>\n", R),
  findall(CatPx, ppRondeCats_nd(CatV, R, CatPx), Catten),
  sortint_c(Catten),
  member(Catp, Catten),
  write("<br>"),
  publiceerPoule(CatP, DagTM, ZonderWebLinks, MetSpeelsterkte,KolomTypeL, Niv),
  fail.
printByRonde(_Cat, _DagTM, _ZonderWebLinks, _MetSpeelsterkte, _KolomTypeL, _Niv).

publiceerOnderdeel(CatV, DagTM, ZonderWebLinks, MetSpeelsterkte,KolomTypeL, Niv, _ZonderVerlRonde) :-
  isPoulePoule(CatV, _),
  printByRonde(CatV, DagTM, ZonderWebLinks, MetSpeelsterkte,KolomTypeL, Niv), !.*/
publiceerOnderdeel(CatV, DagTM, ZonderWebLinks, MetSpeelsterkte,KolomTypeL, Niv, _ZonderVerlRonde) :-
  pos(CatV, _, _, afval),  
  publiceerHTML(CatV, DagTM, ZonderWebLinks, MetSpeelsterkte,KolomTypeL, Niv),
  fail.
publiceerOnderdeel(CatV, DagTM, ZonderWebLinks, MetSpeelsterkte,KolomTypeL, Niv, _ZonderVerlRonde) :-
  pos(CatV, _, _, poel),
  publiceerPoule(CatV, DagTM, ZonderWebLinks, MetSpeelsterkte,KolomTypeL, Niv),
  fail.
publiceerOnderdeel(WC, DagTM, ZonderWebLinks, MetSpeelsterkte, KolomTypeL, Niv, ZonderVerlRonde) :-
  dependOnd(WC, DagTM, ZonderWebLinks, MetSpeelsterkte, KolomTypeL, Niv, ZonderVerlRonde).

dependOnd(WC, DagTM, ZonderWebLinks, MetSpeelsterkte, KolomTypeL, Niv, ZonderVerlRonde) :-
  wc(CatV, _, _,  _, _, _, _, _, _, _, _, _, _, _, _),
  %wc(CatV, _,_, _,_,_,_),
  getbacktrack(Here),
  kopl(WC, pl3en4(_), CatV, _),
  cutbacktrack(Here),			% in principe maar een
  Niv1 = Niv + 1,
  write("<BR>"),
  publiceerOnderdeel(CatV, DagTM, ZonderWebLinks, MetSpeelsterkte,KolomTypeL, Niv1, ZonderVerlRonde),
  fail.
dependOnd(WC, DagTM, ZonderWebLinks, MetSpeelsterkte, KolomTypeL, Niv, ZonderVerlRonde) :-
  getbacktrack(HereI),
  wc(CatVI, _, _,  _, _, _, _, _, _, _, _, _, _, _, _),
  koplv(CatVI, WC),
  not(kopl(WC, pl3en4(_), CatVI, _)),
  not(kopl(CatVI, verliezer, WC, _)),
  cutbacktrack(HereI),
  assert(firsttime(b_True)),		% alle categorieën die er onder hangen
  wc(CatV, _, _,  _, _, _, _, _, _, _, _, _, _, _, _),
  %wc(CatV, _,_, _,_,_,_),
  getbacktrack(Here),
  koplv(CatV, WC),
  not(kopl(WC, pl3en4(_), CatV, _)),
  cutbacktrack(Here),
  firsttime(Eerste),
  write("<BR>"),
  boodschap(Eerste, CatV, "Voorronde(s)"),
  Niv1 = Niv + 1, 
  publiceerOnderdeel(CatV, DagTM, ZonderWebLinks, MetSpeelsterkte,KolomTypeL, Niv1, ZonderVerlRonde),
  fail.
dependOnd(WC, DagTM, ZonderWebLinks, MetSpeelsterkte, KolomTypeL, Niv, b_False) :-
  getbacktrack(HereI),
  kopl(CatVI, verliezer, WC, _),
  %wc(WC, _,_, _,_,_,CatVI),
  wc(CatVI, _, _,  _, _, _, _, _, _, _, _, _, _, _, _),
  %wc(CatvI, _,_, _,_,_,_),	% bestaat
  %htmlnaam(CatVI, _, _),	% 29.6.2008
  cutbacktrack(HereI),
  assert(firsttime(b_True)),
  kopl(CatV,verliezer,WC,_),
  %wc(WC, _,_, _,_,_,CatV),
  wc(CatV, _, _,  _, _, _, _, _, _, _, _, _, _, _, _),
  %wc(Catv, _,_, _,_,_,_),	% bestaat
  pos(Catv, _, _, _),		% en ingedeeld
  Niv1 = Niv + 1,
  write("<BR>"),
  publiceerOnderdeel(CatV, DagTM, ZonderWebLinks, Metspeelsterkte,KolomTypeL, Niv1, b_False),
  fail.
dependOnd(_, _, _, _, _, _, _).

publiceerHTML(WC, DagTM, ZonderWebLinks, MetSpeelsterkte, KolomTypeL, _Niv) :-
  wc(WC, _, _,  SelectieOnd, _, _, _, _, _, _, _, _, _, _, _),
  zonderHekje(SelectieOnd, "", SelectieOndZ), 
  %wc(WC, SelectieOnd,_, _,_,_,_),
  write("\n<TABLE BORDER=\"0\" CELLPADDING=\"0\" CELLSPACING=\"0\">\n"),
  ondWijzer(WC, Wijzer),
  writef("\n<CAPTION class=poule>%s%s&nbsp;</CAPTION>", Wijzer, SelectieOndZ),
  pos(WC, Card, _, _),
  alleregels(WC, Card, DagTM, ZonderWebLinks, MetSpeelsterkte,KolomTypeL, b_True, b_True),
  write("\n</TABLE>"), !.
publiceerHTML(_, _, _, _, _, _).

publiceerPoule(WC, DagTM, _ZonderWeblinks, MetSpeelSt, KolomTypeL, _Niv) :-
  wc(WC, _, _,  SelectieOnd, _, _, _, _, _, _, _, _, _, _, _),
  zonderHekje(SelectieOnd, "", SelectieOndZ), 
  %wc(WC, SelectieOnd,_, _,_,_,_),
  findall(Nr, opgInWeds(WC,Nr),NList),
  count(NList, 0, Aantal),
  afdrukstijl(Aantal, Stijl),
  write("\n<TABLE class=poule CELLSPACING=\"0\"",Stijl,">"),
  ondWijzer(WC, Wijzer),
  writef("\n<CAPTION class=poule>%s%s&nbsp;</CAPTION>", Wijzer, SelectieOndZ),
  pouleHTM(WC, DagTM, MetSpeelSt, KolomTypeL),		% rest in tasvpoel
  write("\n</TABLE>"),
  !.
publiceerPoule(_, _, _, _, _, _).

zonderHekje(In, Str, Uit) :-
  frontchar(In,'#',Rest),
  zonderHekje(Rest, Str, Uit), !.
zonderHekje(In, Str, Uit) :-
  frontstr(1, In, Char, Rest),
  not(Char = "#"),
  concat(Str, Char, Str1),
  zonderHekje(Rest, Str1, Uit), !.
zonderHekje(_, In, In).

ondWijzer(WC, "<A name=\"deze\"/>") :-
  schemadoel(afdruk),
  schemapresel([WC, _]), !.
ondWijzer(_WC, "").

afdrukstijl(Aantal, " style=\"width: 80%;\" ") :-
  Aantal <= 4, !. 
afdrukstijl(_, "").

opgInWeds(Cat, Nr) :-
  opg(Nr,Cat,Tgnst,_,_,_,_,_,_,_,_),
  in_weds(Cat, Tgnst).

alleregels(Cat, Card, TmDag, ZonderWebLinks, MetSpeelSterkte,KolomTypeL, MetNummers, HilitePlaatsing) :-
  retractall(schemaItem(_,_,_,_,_,_,_,_)),
  dim(Card, TDim),
  count(KolomTypeL, 0, KolomAantal),
  kolomheader(Cat, Card, KolomAantal),
  blokken(Cat, TDim, TmDag, HilitePlaatsing),		% zoek alle overige schema_items uit en zet ze klaar
  numb(X),
    X1 = X - 1,			% bij nul beginnen
    No = Card + X1,		% wedstrijdnummer
    Num = X1 * 2 + 1,		% nummertje op schema
    Regel = X1 * 4 + 1,
    wd(No, Cat, T1, T2, _, _, _),
    repRowH(No, Cat, 0, T1, Sterk2, Naam2, Echt2, Kolommen2, Bondsnr2, _V11, _V12, Sterk1, Naam1, Echt1, Kolommen1, BondsNr1, _V21, _V22, Br1, KolomTypeL),
    maakMijnlink(Naam2, BondsNr2, NaamLinked2), 
    maakMijnlink(Naam1, BondsNr1, NaamLinked1), 
    plaatsingHTML(Cat, T1, Color1, Color1End, NaamLinked2, Naam21, HilitePlaatsing),							% formeel = extra veld(en)
    metSpeelsterkte(MetSpeelSterkte, Echt1, Bondsnr1, "beneden", Sterk1, Sterk1S),
    br2style(Regel, MetNummers, Br1, _St1, Stijl2, Num, NumB, NumO),
    extrakols("benedenR", Stijl2, Kolommen1, KolomAantal, "", ExtraKols1),
    writeregel(NumB, ExtraKols1, Stijl2, Color1, NaamLinked1, "class=beneden", Color1End, Sterk1S, Regel, ZonderWebLinks),
    %
    metSpeelsterkte(MetSpeelSterkte, Echt2, BondsNr2, "beneden", Sterk2, Sterk2S),
    extrakols("benedenR", "", Kolommen2, KolomAantal, "", ExtraKols2),
    Regel1 = Regel + 1,
    writeregel(NumO, ExtraKols2, "", Color1, Naam21, "class=beneden", Color1End, Sterk2S, Regel1, ZonderWebLinks),
    %
    repRowH(No, Cat, 1, T2, Sterk4, Naam4, Echt4, Kolommen4, Bondsnr4, _V31, _V32, Sterk3, Naam3, Echt3, Kolommen3, BondsNr3, _V41, _V42, Br2, KolomTypeL),
    maakMijnlink(Naam3, BondsNr3, NaamLinked3), 
    maakMijnlink(Naam4, BondsNr4, NaamLinked4), 
    Num1 = Num + 1,
    br2style(Regel1, MetNummers, Br2, St4, _St5, Num1, Num1B, Num1O),
    plaatsingHTML(Cat, T2, Color2, Color2End, NaamLinked4, Naam41, HilitePlaatsing),							% formeel = extra veld(en)
    metSpeelsterkte(MetSpeelSterkte, Echt3, Bondsnr3, "bovenB", Sterk3, Sterk3S),
    extrakols("benedenR", "", Kolommen3, KolomAantal, "", ExtraKols3),
    Regel2 = Regel1 + 1,
    writeregel(Num1B, ExtraKols3, St4, Color2, NaamLinked3, "class=bovenBZ", Color2End, Sterk3S, Regel2, ZonderWebLinks),
    %
    metSpeelsterkte(MetSpeelSterkte, Echt4, BondsNr4, "benedenM", Sterk4, Sterk4S),
    extrakols("benedenF", "", Kolommen4, KolomAantal, "", ExtraKols4),
    Regel3 = Regel2 + 1,
    writeregel(Num1O, ExtraKols4, "", Color2, Naam41, "class=benedenF", Color2End, Sterk4S, Regel3, ZonderWebLinks),
    %
  No >= Card * 2 - 1, !.
alleregels(_Cat, _, _, _TmDag, _, _, _, _).

writeregel(_, _ExtraKols, _Style, _Color, "&nbsp;", _ClassNaam, _ColorEnd, _Sterkte, 1, _ZonderWebLinks) :-
  not(schemaItem(1, _, _, _, _, _, _, _)), !.
writeregel(Num, ExtraKols, Style, Color, Naam, ClassNaam, ColorEnd, Sterkte, RegelNo, ZonderWebLinks) :-
  write("\n<TR>"),
  writef("\n<TD class=benedenR style=\"%s\">%s</TD>", Style, Num),			% kolom met nummer
  write("\n", ExtraKols),
  writef("\n<TD %s style=\"%s\">%s%s%s</TD>", ClassNaam, Style, Color, Naam, ColorEnd),% kolom met naam
  write(Sterkte, "\n"),							% kolom met speelsterkte
  writeblok(RegelNo, ZonderWebLinks),
  write("\n</TR>").

extrakols(Class, Style, [In|Rest], N, ExtraIn, ExtraUit) :-
  N mod 2 = 1,
  fronttoken(In, _, _),
  not (In = "&nbsp;"), !,
  format(ExtraIn1, "%s\n<TD class=%s style=\"white-space:nowrap;font-style:oblique;%s\">%s</TD>", ExtraIn, Class, Style, In), 
  N1 = N - 1,
  extrakols(Class, Style, Rest, N1, ExtraIn1, ExtraUit).
extrakols(Class, Style, [In|Rest], N, ExtraIn, ExtraUit) :-
  fronttoken(In, _, _),
  not (In = "&nbsp;"), !,
  format(ExtraIn1, "%s\n<TD class=%s style=\"white-space:nowrap;%s\">%s</TD>", ExtraIn, Class, Style, In), 
  N1 = N - 1,
  extrakols(Class, Style, Rest, N1, ExtraIn1, ExtraUit).
extrakols(Class, Style, [_In|Rest], N, ExtraIn, ExtraUit) :- 
  fronttoken(Style, _, _), !,
  N1 = N - 1,
  format(ExtraIn1, "%s\n<TD class=%s style=\"%s;font-size: 3pt;\"></TD>", ExtraIn, Class, Style), 
  extrakols(Class, Style, Rest, N1, ExtraIn1, ExtraUit).
extrakols(Class, Style, [_In|Rest], N, ExtraIn, ExtraUit) :- 
  not(fronttoken(Style, _, _)), !,
  N1 = N - 1,
  format(ExtraIn1, "%s\n<TD class=%s style=\"font-size: 3pt;\"></TD>", ExtraIn, Class), 
  extrakols(Class, Style, Rest, N1, ExtraIn1, ExtraUit).
extrakols(_Class, _Style, [], N, ExtraIn, ExtraUit) :-
  N > 0,
  format(ExtraUit, "%s<TD colspan=%u></TD>", ExtraIn, N), !.
extrakols(_Class, _Style, _, _, Extra, Extra).

metSpeelsterkte(b_True, ja, "00000000", Classe, _Sterk3, Sterk3S) :-				% 00000000
  format(Sterk3S, "<TD class=%s style=\"padding-left: 0; text-align: right\"><B>00</B></TD>", Classe), !.
metSpeelsterkte(b_True, ja, Bondsnummer, Classe, _Sterk3, Sterk3S) :-				% fout of ontbrekend
  not(proef11det(Bondsnummer)),									% wel spelernaam
  format(Sterk3S, "<TD class=%s style=\"padding-left: 0; text-align: right\"><B>?</B></TD>", Classe), !.
metSpeelsterkte(b_True, _, _Bondsnummer, Classe, Sterk3, Sterk3S) :-				% show geen link (afdruk)
  schemadoel(afdruk),
  not(fronttoken(Sterk3, _, _)),
  format(Sterk3S, "<TD class=%s style=\"font-size: 3pt; padding-left: 0\">&nbsp;</TD>", Classe), !.
metSpeelsterkte(b_True, _, _Bondsnummer, Classe, Sterk3, Sterk3S) :-				% show geen link (afdruk)
  schemadoel(afdruk),
  format(Sterk3S, "<TD class=%s style=\"padding-left: 0; text-align: right\">%s</TD>", Classe, Sterk3), !.
metSpeelsterkte(b_True, ja, _Bondsnummer, Classe, Sterk3, Sterk3S) :-				% geen speelsterkte maar wel link
  not(fronttoken(Sterk3, _, _)),
  %date(Year,_Month,_Day),
  %Jaar = Year - 1,
  format(Sterk3S, "<TD class=%s style=\"padding-left: 0\">xx</TD>", Classe), !.
metSpeelsterkte(b_True, ja, Bondsnummer, Classe, Sterk3, Sterk3S) :-				% alles aanwezig
  proef11det(Bondsnummer),	% 22.6.2002
  %date(Year,_Month,_Day),
  %Jaar = Year - 1,
  format(Sterk3S, "<TD class=%s style=\"padding-left: 0; text-align: right\">%s</TD>", Classe, Sterk3), !.
metSpeelsterkte(_, _, _, Classe, _Sterk3, Sterk3S) :-
  format(Sterk3S, "<TD class=%s style=\"font-size: 3pt; padding-left: 0\">&nbsp;</TD>", Classe).

br2style(1, MetZonder, "<BR>", "", "", In, In1, "&nbsp;") :- !,
  metNummer(MetZonder, In, In1).
br2style(_, MetZonder, "<BR>", "", "padding-top: 8", In, In1, "&nbsp;") :- !,
  metNummer(MetZonder, In, In1).
br2style(_, MetZonder, _, "font-size: 2pt", "font-size: 5pt", In, "&nbsp;", In1) :-
  metNummer(MetZonder, In, In1).

metNummer(b_True, In, Uit) :- !,
  format(Uit, "%u.", In).
metNummer(_, _, "").

writeblok(2, _) :-		% verzonnen voor de finalepagina
  retract(schemaItem(2, 2, 300, Tekst, Color3, ColorEnd, Anker, CSS)),
  writef("<TD class=%s rowspan=%u style=\"min-width: 130\">%s%s%s%s</TD>", CSS, 2, Color3, Anker,Tekst, ColorEnd),
  fail. 
writeblok(Regel, b_False) :-
  retract(schemaItem(Regel, _Kol, Rowspan, Tekst, Color3, ColorEnd, Anker, CSS)),
  Rowspan > 0,
  writef("<TD class=%s rowspan=%u style=\"min-width: 60\">%s%s%s%s</TD>", CSS, Rowspan, Color3, Anker,Tekst, ColorEnd),
  fail. 
writeblok(Regel, b_True) :-		% zonder weblinks
  retract(schemaItem(Regel, _Kol, Rowspan, Tekst, Color3, ColorEnd, _Anker, CSS)),
  Rowspan > 0,
  writef("<TD class=%s rowspan=%u style=\"min-width: 60\" >%s%s%s</TD>", CSS, Rowspan, Color3, Tekst, ColorEnd),
  fail. 
writeblok(_Regel, _).

blokken(Cat, TDim, TmDag, _) :-		% ga alle kolommen langs behalve eerste
  TDim > 1,
  numb(N),
    Kol = N + 1,
    Dim = TDim - Kol + 1, 
    blok(Cat, Kol, Dim, TmDag),
  Kol > TDim - 1,		% halve finale gedaan
  blokfinale(Cat, Kol, Dim, TmDag),
  write("</TR>"), !.
blokken(Cat, 1, TmDag, _) :-
  blokfinale(Cat, 1, 1, TmDag),
  write("</TR>"), !.
blokken(Cat, 0, TmDag, HL) :-		% enkele wedstrijd??
  repH(Cat, 0, 0, RepH1, RepU1, TmDag, Color1,ColorEnd,Anker1, HL),
  plus2br(RepH1, RepH11),
  assertz(schemaItem(2, 2, 300, RepH11, Color1, ColorEnd, "", "benedenB")), 
  assertz(schemaItem(4, 2, 2, RepU1, "", "", Anker1, "boven")),  % bovenb
  !.
blokken(_Cat, _TDim, _TmDag, _).		% ga alle kolommen langs behalve eerste

blokfinale(WC, Kol0, _Dim0, TmDag) :-		% samengevouwen finale + winnaar.
  Kol = Kol0 + 1,
  Kol > 3,					% daaronder niet samenklappen
  write("<TD class=balk>Finale</TD>"),
  bitleft(1, Kol0, Rowspan),
  RowspanTop    = Rowspan + 1,
  Rowspan2      = Rowspan - 2,
  repH(WC, 1, 0, RepH3, RepU3, TmDag, Color3,ColorEnd3, Anker3, b_True),
  plus2br(RepH3, RepH31),
  assertz(schemaItem(1, Kol, RowspanTop, RepH31, Color3, ColorEnd3,"", "beneden")),
  NextRow2 = RowspanTop + 1,
  assertz(schemaItem(NextRow2, Kol, 2, RepU3, "", "", Anker3, "bovenB")),
  repH(WC, 0, 0, RepH1, RepU1, TmDag, Color1,ColorEnd1, Anker1, b_True),
  plus2br(RepH1, RepH11),
  NextRow0 = NextRow2 + 2,
  assertz(schemaItem(NextRow0, Kol, Rowspan2, RepH11, Color1, ColorEnd1, "", "benedenC")), 
  NextRow = NextRow0 + Rowspan2,
  assertz(schemaItem(NextRow, Kol, 2, RepU1, "", "", Anker1, "bovenF")),	% voor de uitslag OK
  repH(WC, 1, 1, RepH4, RepU4, TmDag, Color4,ColorEnd4, Anker4, b_True),
  plus2br(RepH4, RepH41),
  NextRow3 = NextRow + 2,
  assertz(schemaItem(NextRow3, Kol, Rowspan2, RepH41, Color4, ColorEnd4, "", "benedenM")),
  NextRow4 = NextRow3 + Rowspan2,
  assertz(schemaItem(NextRow4, Kol, 2, RepU4, "", "", Anker4, "bovenZ")),
  !.
blokfinale(Cat, Kol, Dim, TmDag) :-		% gewone finale + winnaar.
  Kol1 = Kol + 1,
  Dim1 = Dim - 1,
  blok(Cat, Kol1, Dim1, TmDag),			% gewone ronde
  write("<TD class=balk>Winnaar</TD>"),		% doe de finale nog...
  bitleft(1, Kol1, Rowspan),
  RowspanTop = Rowspan + 1,
  repH(Cat, 0, 0, RepH1, RepU1, TmDag, Color1,ColorEnd1,Anker1, b_True),
  plus2br(RepH1, RepH11),
  assertz(schemaItem(1, Kol1, RowspanTop, RepH11, Color1, ColorEnd1, "", "benedenB")), 
  NextRow = RowspanTop + 1,
  assertz(schemaItem(NextRow, Kol1, 2, RepU1, "", "", Anker1, "boven")), !.	% voor de uitslag

namenRows(WC, 2) :-
  wc(WC, _, _, _, e, _, _, _, _, _, _, _, _, _, _), !.
  %wc(WC,_,e,_,_,_,_), !.
namenRows(_WC, 3).

blok(WC, Kol, Dim, TmDag) :-  % per ronde (Kol)
  Dim >= 0,
  bitleft(1, Dim, WNo),
  rondeTekst(Kol, Dim, RTekst),
  write("<TD class=balk>", RTekst, "</TD>"),
  KolMinEen = Kol - 1,
  bitleft(1, KolMinEen, Rowspan),
  RowspanTop    = Rowspan + 1,
  RowspanBottom = Rowspan - 1,
  Rowspan2      = 2 * Rowspan - 1,
  namenRows(WC, NamenRows),
  numb(N1N),  % repeat voor alle wedstrijden
    N = N1N - 1, 			% begint bij 0
    WNo1 = WNo + N,
    NextRow1 = (N * 4 * Rowspan) + 1,
    repH(WC, WNo1, 0, RepH3, RepU3, TmDag, Color3,ColorEnd3, Anker3, b_True),
    plus2br(RepH3, RepH31),
    RowspanTop1a = RowspanTop - NamenRows,	%
    %grootste(RowspanTop1a, 1, 
    assertz(schemaItem(NextRow1, Kol, RowspanTop1a, "&nbsp;", Color3, ColorEnd3, "", "beneden")),
    NextRow1a = NextRow1 + RowspanTop - NamenRows, 
    assertz(schemaItem(NextRow1a, Kol, NamenRows, RepH31, Color3, ColorEnd3, Anker3, "beneden")),
    NextRow2 = NextRow1 + RowspanTop,
    assertz(schemaItem(NextRow2, Kol, 1, RepU3, "", "", "", "bovenB")),
    repH(WC, WNo1, 1, RepH4, RepU4, TmDag, Color4,ColorEnd4, Anker4, b_True),
    plus2br(RepH4, RepH41),
    NextRow3 = NextRow2 + 1,
    Rowspan2a = Rowspan2 - NamenRows,
    assertz(schemaItem(NextRow3, Kol, Rowspan2a, "&nbsp;", Color4, ColorEnd4, "", "bovenF")),
    NextRow3a = NextRow3 + Rowspan2a,
    assertz(schemaItem(NextRow3a, Kol, NamenRows, RepH41, Color4, ColorEnd4, Anker4, "benedenM")),
    NextRow4 = NextRow3 + Rowspan2,
    assertz(schemaItem(NextRow4, Kol, RowspanBottom, RepU4, "", "", "", "bovenZ")),
  WNo1 >= WNo * 2 - 1, !.
blok(_WC, _Kol, _Dim, _TmDag).

rondeTekst(_, 0, "&nbsp;&nbsp;Finale&nbsp;&nbsp;") :- !.
rondeTekst(_, 1, "1/2 Finale") :- !.
rondeTekst(_, 2, "1/4 Finale") :- !.
rondeTekst(Kol, _, Tekst) :-
  format(Tekst, "%ue Ronde", Kol), !.
rondeTekst(_, _, "??").

repH(WC, 0, _, Inhoud, Park, TmDag, "", "", Anker, _) :-		% eindronde (= 0)
  w2(1,WC,Tijd,_,_,_Fix,_TijdLog,Baan),				% + planning bekend
  bitright(Tijd, 7, Dag),
  Dag <= TmDag, 
  %tijd_kwart(Dag,_,_,Tijd),
  dag(Dag,DStr,_), 
  tijd_str(Tijd,Str1,_), !,
  baan_adm(IBaan),
  add_Baan(IBaan, Baan, Baan1),
  concat("&nbsp;", Baan1, Park),
  format(Inhoud, "%s %s %s ", Dstr, Str1, Baan1),
  format(Anker,"<A name=T%ux%u></A>",1, WC).
repH(WC, 0, _, Naam1, Score1, TmDag, Color, ColorEnd, Anker, HL) :-		% eindronde 
  w3(1,WC,Tgnst,_, _,_, _Tijd), 				% winnaar bekend
  baan_adm(IBaan), 
  repb(ja, ' ',IBaan,nee,nee,1,0,1,0,WC,Tgnst,TmDag,Naam,Score,_Tijd2,b_True,Anker),
  concat(Score, "&nbsp;", Score1),
  geplaatstH(WC, 1, 0, Tgnst, Tekst, Color,ColorEnd, HL),
  concat(Naam, Tekst, Naam1),
  !.
repH(_WC, 0, _, "&nbsp;", "&nbsp;", _, "", "", "", _) :- !.		% 
repH(WC, No, UpDown, Naam1, Score1, TmDag, Color, ColorEnd, Anker, HL) :- 
  pos(WC, Top, _, _),
  dim(Top, TopDim),
  dim(No,NoDim),                  	% bepaal de ronde
  uitsl_win1(No, WC, S1, S2),
  upDown(UpDown, S1, S2, S),     	 
  baan_adm(IBaan),
  repb(ja, ' ',IBaan,nee,nee,UpDown,TopDim,NoDim,No,WC,S,TmDag,Naam,Score,_LogTijd,b_True,Anker),
  movepark(IBaan, Naam, NaamP, Park), 
  concat(Score, Park, Score1),
  fronttoken(Naam, Tok, Rest), 
  zonderFje(Tok, NaamP, Rest, Naam0),
  geplaatstH(WC, TopDim, NoDim, S, Tekst, Color, ColorEnd, HL),
  concat(Naam0, Tekst, Naam1), !.
repH(WC, No, UpDown, "&nbsp;", "&nbsp;", _, "", "", Anker, _) :-
  bitleft(No, 1, No1),
  Nox = No1 + UpDown,
  format(Anker,"<A name=T%ux%u></A>",Nox, WC).

zonderFje("f", _, Rest, Rest) :- !.
zonderFje(_, Naam, _, Naam).

geplaatstH(Cat, N1, N2, Tgnst, Tekst, "", "", _HL) :-
  geenkleurPrint(), !,
  geplaatstH1(Cat, N1, N2, Tgnst, Tekst, _, _).		% kan niet schelen (zwart)
geplaatstH(Cat, N1, N2, Tgnst, Tekst, "", "", b_False) :-
  geplaatstH1(Cat, N1, N2, Tgnst, Tekst, _, _), !.		% geen hilite
geplaatstH(Cat, N1, N2, Tgnst, Tekst, Color, ColorEnd, _) :- 
  geplaatstH1(Cat, N1, N2, Tgnst, Tekst, Color, ColorEnd).	

geplaatstH1(Cat, No, No, Tgnst, Tekst, "<span class=geplaatst>", "</span>") :-	% eerste ronde
  opg(_,Cat,Tgnst,Plts,_,_,_,_,_,_,_),
  Plts > 0,
  not(isOnderCat(Cat)),							% echt schema
  plaatsText(Plts, Tekst), !.
geplaatstH1(Cat, No, No, Tgnst, Tekst, "<span class=geplaatst>", "</span>") :-	% eerste ronde
  opg(_,Cat,Tgnst,Plts,_,_,_,_,_,_,_),
  Plts > 0,
  format(Tekst,"(%u)", Plts), !.
geplaatstH1(Cat, _, _, Tgnst, "",  "<span class=geplaatst>", "</span>") :-		% volgende ronde
  opg(_,Cat,Tgnst,Plts,_,_,_,_,_,_,_),
  Plts > 0, !.
geplaatstH1(_Cat, _No, _No1, _Tgnst, "", "", "").

upDown(0, S, _, S) :- !.
upDown(1, _, S, S).

movepark(ja, Planning, PlanningU, Park) :-
  searchchar(Planning,'[',P),
  P1 = P - 1,
  frontstr(P1,Planning,PlanningU,Park), !.
movepark(_, Planning, Planning, "&nbsp;").

plus2br(Namen, NamenC) :-
  searchchar(Namen,'+',Pos),
  Pos1 = Pos - 1, 
  frontstr(Pos1,Namen,Naam1,RestString),
  str_len(Reststring, Len),
  Len > 1, 
  frontchar(RestString,_,Naam2), !,
  format(NamenC, "%s<BR>%s", Naam1, Naam2).
plus2br(Namen, Namen).

kolomheaders(0) :- !.
kolomheaders(N) :-
  writef("\n<TD colspan=%u>&nbsp;</TD>",N).

kolomheader(_Cat, Card, KolomAantal) :-
  Card <> 1,
  %dim(Card, TDim),
  %writef("\n<!-- kolomheader Card=%u TDim=%u-->", Card, TDim),
  write("\n<TR>"),
  kolomheaders(Kolomaantal),
  write("<TD colspan=3>&nbsp;</TD>"),
  write("\n<!-- blokkenbegin -->"), !.
kolomheader(_, _, _KolomAantal).

boodschap(b_False, _, _) :- !.
boodschap(_, Cat, _) :-
  bovencat(Cat, Topcat),
  not(pos(TopCat, _, _, _)),
  !.
boodschap(_, _, Msg) :-
  write("\n<TABLE BORDER=\"0\" CELLPADDING=\"2\" CELLSPACING=\"0\">"),
  writef("\n<TR><TD class=balk>%s</TD></TR>",Msg),
  write("\n</TABLE>"),
  assert(firsttime(b_False)).

ranglijst(_, b_True, WC, _Kolommen, [bondsnummer,rating, ranglijst,gebdatum,districtk]) :-
  wc(WC, _OnderdeeelID, _Orde, _Naam, _EDG, _Kort, _SexL, _SpStrk, _LftV, leeftijd(_, LeeftijdTotenMet), _HDG, _Schemasoort, _Status, _KNLTB, _Geld),
  %wcrit(WC, _Speelsterkte, _LeeftijdVanAf, LeeftijdTotenMet, _Res),
  LeeftijdTotenMet > 2,
  LeeftijdTotenMet < 20, !.
ranglijst(_, b_True, WC, _Kolommen, [rating, wildcard, bondsnummer, ranglijst]) :-
  wc(WC, _OnderdeeelID, _Orde, _Naam, _EDG, _Kort, _SexL, Speelsterkte, _LftV, _LftTM, _HDG, _Schemasoort, _Status, _KNLTB, _Geld),
  %wcrit(WC, Speelsterkte, _LeeftijdVanAf, _LeeftijdTotenMet, _Res),
  fronttoken(Speelsterkte, Speelsterkte1, _),
  Speelsterkte1 < "3", !.
ranglijst(_, _, _CNo, Kolommen, Kolommen) :- !.

openBestand(upload, _SheetSel, _, CNo, StyleSheet, OnderdeelSelected, _OnderdelenR) :-		% blijf in hetzelfde bestand als aaneengeregen
  htmlnaam(CNo, Short, _),
  format(Filenaam, "%s.html", Short),
  trim_c(Filenaam),
  write("\n***[sheet][", Short,"]***\n"),
  metaTag(Eerst, Tag),
  writef("%s\n<HTML><HEAD>%s<TITLE>%s</TITLE>", Eerst, Tag, OnderdeelSelected),
  tasversie(Versie),
  writef("\n<!-- ToernooiAssistent versie %s -->",Versie),
  %write("\n<LINK rel=stylesheet Type=\"text/css\" href=\"sheet.php?plek=xxx&amp;type=css&amp;naam=", Stylesheet, "\">"),
  stylesheet(StyleSheet),
  write("\n</HEAD><BODY>"),
  logoerbijTop(), 
  toernooidiv(),
  !.
openBestand(uploadftp, SheetSel, _, CNo, StyleSheet, OnderdeelSelected, OnderdelenR) :-		% blijf in hetzelfde bestand als aaneengeregen
  htmlnaam(CNo, Short, _),
  closefile(work),
  get_htmlmap(Dir),
  format(FN, "%s.html", Short),
  filenamepath(Filenaam, Dir, FN),
  openbestand1(SheetSel, FileNaam, OnderdeelSelected, StyleSheet, OnderdelenR),
  !.
openBestand(afdruk, _, b_True, _CNo, StyleSheet, _OnderdeelSelected, OnderdelenR) :-
  eerstekeer(b_True),
  assert(eerstekeer(b_False)),
  get_tempdir(Dir, _),
  filenamepath(Filenaam, Dir, "schema.html"),
  openbestand1(apart, FileNaam, "Afdrukken", StyleSheet, OnderdelenR),
  !.
openBestand(afdruk, _, b_False, CNo, Stylesheet, OnderdeelSelected, OnderdelenR) :-
  htmlnaam(CNo, Short, _),
  get_htmlmap(Dir),
  format(FN, "%s.html", Short),
  filenamepath(Filenaam, Dir, FN),
  openbestand1(apart, FileNaam, OnderdeelSelected, StyleSheet, OnderdelenR),
  !.
openBestand(afdruk, _, b_True, _, _, _, _) :- 
  write("\n<div style=\"page-break-before: always\" >&nbsp;</div>"),
  logoErBijTop(), !.
openBestand(_, _, _, _, _, _, _).

openbestand1(_ShSelect, FileNaam, OnderdeelSelected, StyleSheet, _OnderdelenR) :-
  closefile(work),
  filenamepath(FileNaam,_,Short),
  upper_lower(ShortU, Short),
  retractall(teDeleten(ShortU)),
  trap(openwrite(work,Filenaam),_,htmlnaamError(Filenaam)),
  writedevice(work),
  metaTag(Eerst, Tag),
  writef("%s\n<HTML><HEAD>%s<META HTTP-EQUIV=\"Expires\" CONTENT=\"Tue, 24 Oct 1996 17:45:00 GMT\"><TITLE>%s</TITLE>", Eerst, Tag, OnderdeelSelected),
  tasversie(Versie),
  writef("\n<!-- ToernooiAssistent versie %s -->",Versie),
  %write("\n<LINK rel=stylesheet Type=\"text/css\" href=\"sheet.php?plek=xxx&amp;type=css&amp;naam=", StyleSheet, "\">"),
  stylesheet(StyleSheet),
  write("\n</HEAD><BODY>"),
  toernooidiv(),
  logoerbijTop(), !. 
openbestand1(_SheetSel, _FileNaam, _OnderdeelSelected, _StyleSheet, _OnderdelenR).

htmlnaamError(Naam) :-
  writedevice(screen),
  format(Msg, "Kan bestand %s niet openen, pas de korte naam van het onderdeel aan svp!", Naam),
  writeToJournaal(Msg), !.

eindeAaneengeregen(_, _).
eindeAaneengeregen(afdruk, b_True) :-
  write("</BODY></HTML>\n"),
  fail.

closefileAls(upload) :- !.
closefileAls(_) :-
  closefile(work).

expandC(CatL, WC) :-
  wc(WC, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
  %wc(WC, _Naam,_, _,_,_,_),
  bovencat(WC, Boven),
  member(Boven, CatL).

deletehtml(upload) :- !.
deletehtml(_) :-
  get_htmlmap(Webmap),
  format(StyleSheet, "%s*.css", Webmap),
  not(trap(existfile(StyleSheet),_,true)),		% als het stylesheet niet bestaat
  trap(mkdir(Webmap), _, true),		% probeer dan directory aan te maken
  fail.
deletehtml(_) :-				% maak de directory schoon
  retractall(teDeleten(_)),
  get_htmlmap(Dir),
  format(Wild, "%s*.htm*", Dir),
  dirfiles(Wild,0x40,FNam,_,_,_,_,_,_,_,_),
  upper_lower(FNamU, FNam),
  assert(teDeleten(FNamU)),
  fail.
deletehtml(_).

beginaaneen(_Stylesheet) :-
  schemadoel(upload),
  get_tempdir(Dir, _),
  %deleteAllGZ(Dir),
  filenamepath(Filenaam, Dir, "AA_Opsturen"),
  %maakRW(Filenaam),
  openwrite(work,Filenaam),
  writedevice(work),
  trim_c(_Stylesheet),
  write("***[css][",_Stylesheet,"]***\n"), 
  %get_htmlmap(Map),
  %filenamepath(FullName,Map,_Stylesheet),
  %file_str(Fullname, String),
  String = "############################################################################",  % ????????
  write(String, "\n"), !.
beginaaneen(_).

publiceerFinale(WC, DagTM) :-
  weblayout1(web, _IDCW_TOERNOOICSS_DEFAULT, Settings1,_Kolommen, _Settings2, _IDCW_MEDEDELING_VALUE, _PreselectBew),
  Settings1 = [_IDCW_RANGLIJST_CHECKED, _IDCW_ZWARTWIT_CHECKED, _VerliezersApart, _IDCW_AANEENGEREGEN_CHECKED,_IDCW_MET_SPEELSTERKTE_CHECKED,_IDCW_SHEET_SLECTOR_CHECKED,_IDCW_INCL_LOGOBESTANDEN_CHECKED],
  alleregels(WC, 1, DagTM, b_False, _IDCW_MET_SPEELSTERKTE_CHECKED, [], b_False, b_False), !. % geen nrs, geen pltshilite
publiceerFinale(_, _).

publiceer34(WC, DagTM) :-
  getbacktrack(Here),
  kopl(WC, pl3en4(_), Cat34, _),
  cutbacktrack(Here),
  write("\n<TR style=\"font-size: 4pt\"><TD colspan=3>&nbsp;</TD></TR>"),
  write("\n<TR><TD colspan=2> 3/4 plaats:","</TD></TR>"),
  weblayout1(web, _IDCW_TOERNOOICSS_DEFAULT, Settings1,_Kolommen, _Settings2, _IDCW_MEDEDELING_VALUE, _PreselectBew),
  Settings1 = [_IDCW_RANGLIJST_CHECKED, _IDCW_ZWARTWIT_CHECKED, _VerliezersApart, _IDCW_AANEENGEREGEN_CHECKED,_IDCW_MET_SPEELSTERKTE_CHECKED,_IDCW_SHEET_SLECTOR_CHECKED,_IDCW_INCL_LOGOBESTANDEN_CHECKED],
  alleregels(Cat34, 1, DagTM, b_False, _IDCW_MET_SPEELSTERKTE_CHECKED, [], b_False, b_False), !. % geen nrs, geen pltshilite
publiceer34(_WC, _).

voorPrint(afdruk,  b_True) :- !.
voorPrint(_,  b_False).

aaneengeregen(b_False) :-
  schemadoel(upload), !.
aaneengeregen(b_False) :-
  schemadoel(uploadftp), !.
aaneengeregen(b_True).

postHtml(_) :-			% zonder weblinks
  date(DY, DM, DD),
  time(TH, TM, _, _),
  helestr_int(0,TMS, TM),
  sleuteltekst1(Park),
  trim_c(Park),
  writef("<B>%s</B>. Editie %u/%u/%u %u:%s. ", Park, DD, DM, DY, TH, TMS),
  logoErBijBot().
  
openwriteAls(upload, _Filenaam, Short) :-
  write("***[agenda][",Short,"]***\n"), !. 
openwriteAls(_, Filenaam, _) :-
  openwrite(work,Filenaam).
  %trap(openwrite(work,Filenaam),_,htmlnaamError(Filenaam)).

