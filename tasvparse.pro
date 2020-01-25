 /*****************************************************************************

		Copyright (c) 1989 - 2005 De Lint Associates

 Project:  TASVP1
 FileName: TASVPARSE.PRO
 Purpose: No description
 Written by: JdL
 Comments: bedoeld om expat te incorporeren
******************************************************************************/

include "tasvp1.inc"
include "tasvp1.con"
include "hlptopic.con"


%====================================================================================
domains
  spmaat = opgever ; maat

database - xml
  mxml(string, string, string)
  t_opg(integer, wedscat, tgnst, string InschrNr)
  cacheOpg(integer, wedscat, tgnst, teamstatus)
  t_opgvoorlijst(integer)
  nakijken(string INr)
  nakijken1(string INr)
  determ algemeld
single filteren(boolean)
determ softstop  
determ fileisopen
  candi(string, string)
determ nieuwonderdeel

predicates
  splitsXML(string FileName)
procedure xml_inschrijfInvoerInsExp1()
procedure xml_inschrijfInvoerInsExp(slist Filenamen)
procedure nakijkenProc()
procedure nakijkenInit()
procedure nakijkenMsg(slist NBList, string NBMsg)
procedure softstopmessage(string, integer Icon)
procedure nieuweonderdelenmessage(string)
determ    overgeslagenaantal(slist)

clauses

filteren(b_False).

setsoftstop() :-
  assert(softstop).

splitsXML(FileName) :-
  retractall(mxml(_, _, _)),
  retractall(nieuwonderdeel),
  openread(work, FileName),
  readdevice(work),
  repeat_f(work),
  readln(S),
  trap(term_str(xml,Fact,S),_, fail),
  assert(Fact, xml),
  fail.
splitsXML(_FileName) :-
  closefile(work).

xml_invoer_file() :-
  getfolder(Path,_,_,_),
  _Filenaam = dlg_GetFileName("*.xml",["xml", "*.xml", "all","*.*"],"Selecteer een XML-bestand",[],Path,Filenamen),
  Answer = dlg_MessageBox( "Gegevensinvoer", "Gegevens invoeren zonder verhinderingen en opmerkingen?\nM.a.w. eerst opschonen?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
  Answer > 0,
  A0 = Answer - 1,	% base 0
  bitxor(A0,1,A1),	% omdraaien
  xml_inschrijfInvoerExpat(FileNamen, A1), !.
xml_invoer_file().

overgeslagenaantal([]) :-
  checkvoortgang(2, "Geen nieuwe opgaven (of geen verbinding!)..."),
  checkvoortgang(99, "Klaar"), !,
  fail.
overgeslagenaantal(Lijst) :-
  count(Lijst, 0, Aantal),
  format(M, "Het aantal in te voeren opgaven is %u.", Aantal), 
  checkvoortgang(1, M).

xml_inschrijfInvoerNieuw() :-   % direct van internet
  assert(filteren(b_False)),	% 18.7.2009
  retractall(softstop),
  checkvoortgang(0, "Invoer van inschrijvingen"),
  checkvoortgangsetsoftstop(),
  checkvoortgang(1, "DE nog in te voeren opgaven worden bepaald..."),
  overgeslagen(OpgList),
  overgeslagenaantal(OpgList),
  checkvoortgang(2, "De in te voeren opgaven worden opgehaald..."),
  list_to_string(OpgList, "','", OpgListString),
  providerX(Prov, _, "", _),
  AV = aanmeldenvars(),
  get_tempdir(_, Download),
  deletefile(Download),
  format(Volgend,"http://%s/getopgx2009.php?stap=inschrijf3",Prov), 
  Return = downloadData(Volgend, Download, ["oids",OpgListString|AV], _, 0),
  Return = 0,
  checkvoortgang(3, "Opgaven opgehaald, invoeren kan beginnen..."),
  %file_str(Download, SS),
  %write(SS),	% 20.11.2009 !!!!!!!!!!!!!
  splitsXml(Download),
  nakijkenInit(),
  loglock(mdi, "xmlinschrijfinvoerExp1"),
  xml_inschrijfInvoerInsExp1(),
  retractall(mxml(_, _, _)),
  retractall(candi(_, _)),
  logclose(),
  checkvoortgang(100, "Klaar!"),
  nakijkenProc(),
  compress(update),
  startdbrefresh(), !. 
xml_inschrijfInvoerNieuw() :-
  retractall(mxml(_, _, _)),
  retractall(candi(_, _)),
  checkvoortgang(99, ""),
  nakijkenProc().

xml_inschrijfInvoerExpat(FileNamen, Opschonen) :- % van bestandsnaamlijst
  assert(filteren(Opschonen)),	% 18.7.2009
  retractall(softstop),
  nakijkenInit(),
  loglock(mdi, "xmlinschrijfinvoer"),
  xml_inschrijfinvoerInsExp(FileNamen),
  logclose(),
  compress(update),
  startdbrefresh(),
  nakijkenProc(), !.
xml_inschrijfInvoerExpat(_FileNamen, _Opschonen).

nakijkenInit() :-
  retractall(t_opg(_, _, _, _)),	% 25.3.2010
  retractall(algemeld()),
  retractall(nakijken(_)),
  retractall(nakijken1(_)).

softstopmessage("Invoer afgebroken",mesbox_iconexclamation) :-
  retract(softstop), !.
softstopmessage("Invoer klaar", mesbox_iconinformation).

nieuweonderdelenmessage("\nEr zijn voorlopige onderdelen bijgemaakt. Zie Onderdeel,Kenmerken") :-
  retract(nieuwonderdeel()), !.
nieuweonderdelenmessage("").

nakijkenProc() :-
  spelerselrefresh(),
  findall(NB, nakijken(NB), NBList),
  nakijkenMsg(NBList, _NBMsg),
  softstopmessage(SM, Icon),
  nieuweonderdelenmessage(MsgA),
  Msg0 = "\nDe nieuwe gegevens zijn te vinden in het opgavenoverzicht.",
  Msg3 = "\nAls je een opgave niet wilt of niet meer wilt, sluit hem dan uit.",
  Msg1 = "\nLoop de eventueel in het rapport genoemde namen na op dubbelen met behulp van het nu volgende rapport.",
  Msg2 = "\nDe spelerselectie lijst (F2) heeft een optie om te helpen gelijkbedoelde spelers samen te voegen.",
  format(Msgx, "%s%s%s%s%s%s%s", SM,MsgA, Msg0,Msg3, Msg1, Msg2), 
  dlg_MessageBox( "Inschrijvingen", MsgX, Icon, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  retractall(nakijken(_)),
  retractall(nakijken1(_)),
  retractall(algemeld()),
  TaskWin = vpi_GetTaskWin(),
  dlg_opgaven_Create(TaskWin), !.
nakijkenProc().

nakijkenMsg([], "") :- !.
nakijkenMsg(NBList, NBMsg) :-
  sortstring_c(NBList, 1),
  list_to_string(["\nHet betreft de volgende opgaven:"|NBList], "\n  ", NBMsg),
  findall(Naam, nakijken1(Naam), NaamList),
  list_to_string(["\nDe volgende namen nakijken op dubbel voorkomende spelers","doe dat UITSLUITEND via de lijst van ALLE spelers in het bestand (F2)"|NaamList], "\n  ", NMsg),
  concat(NBMsg, NMsg, Rap), 
  get_TempDir(_, TempFile),
  file_str(TempFile, Rap),
  TaskWin = vpi_GetTaskWin(),
  dlg_rapport_Create(TaskWin, "Na te kijken opgaven", "nakijken.txt", b_True), !.
%  win_editor_Create(TaskWin, NBMsg, "Na te kijken opgaven"), !.
nakijkenMsg(_, "").

database - insfacts
single t_nr(integer, string InschrijfNr)
t_wc(wedscat, integer)
determ t_speler(persno)
t_sp(persno,string MV,string,string,verhl,slist,janee,tijd,clubno,adr,wnpl,knltb,
       strks,strkd,geb,tlfn,tlfn,emailadres,string, spmaat OpgeverPartner, string Incasso, string IncassOK,
       string InschrNr, slist Import)	% 19.12.2009 Import = nieuw
t_verh(period)
t_maat(integer)
t_enkel(integer, wedscat)
t_dubbel(integer, wedscat, integer MaatNummer)
t_club(string, string)

predicates
%procedure oogst(integer Mode)
determ geslacht(string MV, string Gebdatum, string Speelst, sex) - (i, i, i, o)
determ sex(string, sexe) - (i, o)
procedure expand(string, string)
%procedure ordenogeenkeer(tgnst, tgnst)
%procedure orde1(persno, persno, string SexeS, string SexeP, string, string, tgnst)
compare(wedsex DHG, string MV, integer SKeus, wedscat Cat, string OpgaveNr)
determ aborted(tgnst)
procedure voegToe(BOOLEAN, string, string, string, string, integer Filteren) - (i, i, i, i, o, i) % Filteren = 1, doorlaten <> 1
procedure lookFor(string, string, BOOLEAN)

clauses

t_nr(0, "").

geslacht(Gesl, Geb, Ste, SexN) :-			% nog uit te breiden!!
  spsrt(SexN, _, _, L_Low, L_Up, St_Low, St_Up, _),
  check_leeft(L_Low, L_Up, Geb),			% leeftijdscategorie OK?
  expand(St_Up, St_Up1),
  check_sterk(St_Low, St_Up1, Ste),			% sterkte OK?
  upper_lower(M_VU, Gesl),
  sex(M_VU, M_V),					% sexe moet kloppen
  spsrt(SexN, _, M_V,_,_,_,_,_), !.
geslacht(Gesl, _, _, Sex) :-
  upper_lower(M_VU, Gesl),
  sex(M_VU, M_V),
  spsrt(Sex, _, M_V,_,_,_,_,_), !.
geslacht(Gesl, _, _, _) :-				% uitgefilterd
  not(algemeld()),
  assert(algemeld()),
  upper_lower(M_VU, Gesl),
  format(Msg, "Spelers uitgefilterd:\n geen passende spelersoort gevonden voor \"%s\"!\nBreid de speletabel uit.",M_VU),
  writeToJournaal(Msg, ja),
  fail.

sex("M", m) :- !.
sex("V", v) :- !.
sex(X, m) :-
  not(algemeld()),
  assert(algemeld()),
  t_nr(_, Inv),
  format(Msg, "Onbekend geslacht: %s in invoer, opgave nummer: %s.\nBlanco partner??", X, Inv),
  dlg_MessageBox( "Fout in invoer", Msg, mesbox_iconError, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), !.

expand("", "") :- !.
expand(In, Uit) :-
  str_len(In, Len),
  Len < 2,
  concat(In, "z", Uit), !.
expand(In, In).

compare(hdnvt, _, _SKeus, _Cat, _) :- !.
compare(gemengd, _, _SKeus, _Cat, _) :- !.
compare(dames, "V", _SKeus, _Cat, _) :- !.
compare(heren, "M", _SKeus, _Cat, _) :- !.
compare(_Srt, _Ltr, SKeus, Cat, Nr) :-
  t_sp(SKeus,_,SpelerNaam,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),
  wc(Cat, _, _, CatNaam, _, _, _, _, _, _, _, _, _, _, _),
  %wc(Cat, CatNaam,_, _,_,_,_),
  format(Msg, "Fout geslacht\nOnderdeeel: %s\nSpeler: %s\nOpgavenummer:%s", CatNaam, SpelerNaam, Nr),
  dlg_MessageBox( "Inschrijving", Msg, mesbox_iconexclamation, mesbox_buttonsOK, mesbox_defaultfirst, mesbox_suspendapplication ), !.

aborted(Tx) :-
  tgnst_num(Tx, SKeus),
  not(sp2(SKeus,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)), !.

voegToe(_, _, _, X, X, 1) :- !.	% filteren
voegToe(b_True, _, _, X, X, _) :- !.	% voor de opmerkingen (already in)
voegToe(_, A, B, _, X, _) :-
  concat(A, B, X).

lookfor("", _, b_True) :- !.		
lookfor(_, "", b_False) :- !.
lookfor(Opmerkingen, Res, b_True) :-
  searchstring(Res,Opmerkingen,_FoundPos), !.
lookfor(_Opmerkingen, _Res, b_False).

predicates
updateSp(slist, spmaat)
procedure string  amember2(string OudeWaarde, slist Attrs, slist Attrnaam)
procedure string amember(slist, slist Keyws) - (i,i)
procedure uitkantoortijd(tijd, slist, tijd)
procedure clubcode(string, string, slist, string)
procedure adres(string, slist, string, spmaat)
procedure woonpl(string, slist, string, spmaat)
procedure oudOfNieuw(string Datum, string Jaar, string, string)
procedure nietveranderen(integer, string, string, string)
nondeterm alSpeler(string Naam, string Knltb, string MV, integer Nr, integer Control) - (o, i, o, o, o) (o, o, o, o, o) (i, o, o, o, o)% 0 = t_sp, 1 = sp1
%procedure sex1(string, sexe) - (o, i)
procedure welGeldigKNLTB(string, string)
procedure checkEmailAdres(string SpelerNaam, string EMailOud, string EMAilNew, string Res, string ResNew, string EmailNew, spmaat)
procedure checkEmailAdres(string SpelerNaam, string EMailOud, string EMAilNew, string Res, string ResNew, string EmailNew, spmaat, string Titel, string BijCancel)
procedure overnemen(integer, string TelOud, string TelNew, string TelResult, string ResOud, string ResResult) - (i,i,i,o,i,o)
procedure extraspatie1(string Res, string Spatie)
procedure fixBondEmails(string EmailNew, string EmailNewF)
procedure checkTelefoon(string SpelerNaam, string Titel, string EMailOud, string EMAilNew, string Res, string ResNew, string TelResult, spmaat)
procedure checkTelefoon(string SpelerNaam, string Titel, string EMailOud, string EMAilNew, string Res, string ResNew, string TelResult, spmaat, string Titel, string BijCancel)
procedure alleencijfers(string Str, string In, string Uit)
determ    compLaatste7(string, string)
determ    isAKeyw(slist, string)
procedure string tekstvoorKNLTB(slist)
procedure importNummer(spmaat, slist ImpIn, slist ImpUit)
procedure voorweekend(string, string, string)
determ xml2ansi(string, string)
procedure specialxml2ansi(string, string)

clauses

voorweekend(Naam, Voorweekend, "Toernooi in het voorweekend.") :-
  fronttoken(Voorweekend, _, _),
  format(Msg, "%s speelt mogelijk een ander toernooi in het voorweekend!", Naam),
  _Answer = dlg_MessageBox( "Speler", Msg, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  !.
voorweekend(_Naam, _Voorweekend, "").

updateSp(AList, ModeSpelerMaat) :-  % van maat neem je niet alles over
  filteren(Filteren),
  retract(t_sp(SpNo,Sex,Naam,Tel,Verh,Offset,nee,UitKant,Club,Adr,Wnpl,Knltb,StS,StD,Geb,Tel1,Tel2,EMail,Resxa,_ModeAl,Incasso,IncassoOK,Ins,Import)),
  adres(Adr, AList, AdrNew, ModeSpelerMaat),
  woonpl(Wnpl, AList, WnplNew, ModeSpelerMaat),
  TelNew = amember2(Tel,AList,["telefoonhuis","telefoonthuis"]),
  checkTelefoon(Naam, "huis", Tel, TelNew, Resxa, Resxb, TelResult, ModeSpelerMaat),
  Tel1New = amember2(Tel1,AList,["telefoonwerk"]),
  checkTelefoon(Naam, "werk", Tel1, Tel1New, Resxb, Resxc, Tel1Result, ModeSpelerMaat),
  Tel2New = amember2(Tel2,AList,["telefoonmobiel"]),
  checkTelefoon(Naam, "mobiel", Tel2, Tel2New, Resxc, Resx, Tel2Result, ModeSpelerMaat),
  EMailNew = amember2(EMail,AList,["emailadres","email"]),
  fixBondEmails(EmailNew, EmailNewF),
  upper_lower(EMailNewF, EMailNewL),
  upper_lower(EMail, EMailL),
  checkEmailAdres(Naam, EMailL, EMailNewL, Resx, Res0y1,EmailResult, ModeSpelerMaat),
  Knltb1 = amember(AList, ["knltbnummer","bondsnummer"]),
  welGeldigKNLTB(Knltb1, Knltb2),
  nietVeranderen(SpNo, Knltb2, Knltb, KnltbNew),		% moet juist niet veranderd worden, alleen ingevuld
  %amember2(Knltb,AList,"knltbnummer", KnltbNew),
  StSNew = amember2(StS,AList,["speelsterkteenkel","enkelspeelsterkte"]),
  StDNew = amember2(StD,AList,["speelsterktedubbel"]),
  VWToern = amember(AList, ["vorigeweektoernooi"]),
  Voorkeur = amember(AList, ["berichtspeeltijdenvoorkeur"]),
  Incasso2 = amember2(Incasso,AList, ["banknummer"]),
  IncassoOK2 = amember2(IncassoOK,AList, ["incassoakkoord"]),
  Voorweekend = amember(AList, ["voorweekend"]),
  voorweekend(Naam, Voorweekend, VoorweekendOpm),
  lookfor(VoorweekendOpm, Res0y1, AlreadyIn0y1),
  voegtoe(AlreadyIn0y1, Res0y1, VoorweekendOpm, Res0y1, Res0, Filteren), 
  Opmerkingen = amember(AList, ["opmerkingen"]),
  lookfor(Opmerkingen, Res0, AlreadyIn1),
  voegToe(AlreadyIn1, Res0, Opmerkingen, Res0, Res1, Filteren),
  KTekst = tekstvoorKNLTB(AList),
  lookfor(Ktekst, Res1, AlreadyIn1a),
  voegToe(AlreadyIn1a, Res1, KTekst, Res1, Res1b, Filteren),
  lookfor(VWToern, Res1b, AlreadyIn2),
  format(VWToern1, " Voorafgaande speel ik %s ", VWToern),
  voegToe(AlreadyIn2, VWToern1, Res1b, Res1b, Res2, Filteren),
  lookfor(Voorkeur, Res0, AlreadyIn3),
  format(Voorkeur1, " Berichtgeving bij voorkeur per %s ", Voorkeur),
  voegtoe(AlreadyIn3, Voorkeur1, Res2, Res2, Opm, Filteren), 
  importNummer(ModeSpelerMaat, Import, ImportN),
  uitkantoortijd(UitKant, AList, UitKantNew),
  GDag = amember(AList, ["geboortedag"]),
  GMaand = amember(AList, ["geboortemaand"]),
  GJaar = amember(AList, ["geboortejaar"]),
  format(GebIn, "%s-%s-%s", GDag, GMaand, GJaar),
  oudOfNieuw(Geb, GJaar, GebIn, GebNew), 
  clubcode(Club, Naam, AList, ClubNew), !,
  %bitor(Mode, ModeAl, ModeUit),
  asserta(t_sp(SpNo,Sex,Naam,TelResult,Verh,Offset,nee,UitKantNew,ClubNew,AdrNew,WnplNew,KnltbNew,StSNew,StDNew,
  		GebNew,Tel1Result,Tel2Result,EMailResult,Opm,ModeSpelerMaat,Incasso2,IncassoOK2,Ins,ImportN)).

utf_8_to_ansi(UTF,ANSI_STR):-
   UNICODE_STR = api_MultiByteToWideChar(65001,[],UTF),
   ANSI_STR = api_WideCharToMultiByte(28591,[],UNICODE_STR).
 
amember2(_In, [KeyW1, Value | _], KeyWL, "") :-
  member(KeyW1, KeyWL),
  trim_c(Value),
  Value = "-", !.
amember2(_In, [KeyW1, Value | _], KeyWL, Uit) :-
  member(KeyW1, KeyWL),
  utf_8_to_ansi(Value,ValueA),
  specialxml2ansi(ValueA, Uit),
  trim_c(Uit),
  not(Uit = ""), !.
amember2(In, [_, _ | Rest], KeyWL, Value) :-
  Value = amember2(In, Rest, KeyWL), !.
amember2(In, _AList, _KeyW, In).

amember([Keyw1, Value | _], KeywL, Uit) :-
  member(Keyw1, KeywL),
  utf_8_to_ansi(Value,ValueA), !,
  specialxml2ansi(ValueA, Uit).
amember([ _, _ | Rest], KeywL, Value) :-
  Value = amember(Rest, KeywL), !.
amember(_, _, "").

xml2ansi("#039", "'").

specialxml2ansi(In, Uit) :-
  xml2ansi(XIn, XUit),
  searchstring(In,XIn,FoundPos),
  FoundPos > 0,
  str_len(XIn, Len),
  F0 = FoundPos - 1,
  frontstr(F0, In, Begin, Rest),
  frontstr(Len, Rest, _, Rest1),
  format(In1, "%s%s%s", Begin, XUit, Rest1), !,
  specialxml2ansi(In1, Uit).
specialxml2ansi(In, In).


isAkeyw([Keyw,_|_], Keyw) :- !.
isAkeyw([_,_|Rest], Keyw) :-
  isAkeyw(Rest, Keyw).
   
tekstvoorKNLTB(AList, Uit) :-
  member(Keyw, ["geaccepteerd","reserve"]),
  isAkeyw(AList, Keyw),
  In = amember(AList, [Keyw]),
  format(Uit, "%s=%s", Keyw, In), !.
tekstvoorKNLTB(_AList, "").
  
uitkantoortijd(_In, AList, Uit) :-
  UK = amember(AList, ["uitkantoortijd"]),
  fronttoken(UK,UrenS,Rest),
  str_int(UrenS,Uren),
  bitleft(Uren, 2, Uren2), 
  fronttoken(Rest,_,MinutenS),
  str_int(MinutenS, Minuten),
  roosterinterval(RI),
  Kwartieren = Minuten div RI, !,
  bitor(Uren2, Kwartieren, Uit).
uitkantoortijd(_In, AList, Uit) :-
  UK = amember(AList, ["uitkantoortijd"]),
  fronttoken(UK,UrenS,_Rest),
  str_int(UrenS,Uren),
  bitleft(Uren, 2, Uit), !.
uitkantoortijd(In, _AList, In).

clubcode(_OudeClub, _, AList, Clubcode) :-
  Clubcode = amember(AList, ["verenigingsnummer"]),
  club(Clubcode,_,_), !.
clubcode(_, _, AList, Clubcode) :-
  Clubcode = amember(AList, ["verenigingsnummer"]),
  not(club(Clubcode,_,_)),
  not(t_club(Clubcode,_)),
  Club = amember(AList, [" club"]),
  assert(t_club(Clubcode, Club)), !.
clubcode(_OudeClub, _, AList, Clubcode) :-
  Clubcode = amember(AList, ["clubcode"]),
  club(Clubcode,_,_), !.
clubcode(_, _, AList, Clubcode) :-
  Clubcode = amember(AList, ["clubcode"]),
  not(club(Clubcode,_,_)),
  not(t_club(Clubcode,_)),
  Club = amember(AList, ["club"]),
  assert(t_club(Clubcode, Club)), !.
clubcode(_, _, AList, Clubcode) :-
  Club = amember(AList, ["club"]),
  club(Clubcode,Club,_), !.
clubcode(_, _, AList, Clubcode) :-
  Club = amember(AList, ["club"]),
  t_club(Clubcode,Club), !.
clubcode(Club, _, AList, ClubUit) :-
  _A = amember(AList, ["club"]), !,
  ClubUit = amember2(Club, AList, ["club"]).  % ???
clubcode(Club, _, AList, ClubUit) :-
  ClubUit = amember2(Club, AList, ["club"]).
  
adres(_In, AList, Uit, opgever) :-
  Straat = amember(AList, ["adresstraat","straat"]),	% straat gegeven
  Nr = amember(AList, ["adreshuisnummer","huisnummer"]),
  format(Uit, "%s %s", Straat, Nr),
  trim_c(Uit), !.
adres(In, AList, Uit, maat) :-
  not(fronttoken(In, _, _)),
  Straat = amember(AList, ["adresstraat","straat"]),	% straat gegeven
  Nr = amember(AList, ["adreshuisnummer","huisnummer"]),
  format(Uit, "%s %s", Straat, Nr),
  trim_c(Uit), !.
adres(Uit, _AList, Uit, _).

woonpl(_In, AList, Uit, opgever) :-
  PC = amember(AList, ["adrespostcode","postcode"]),
  Plaats = amember(AList, ["adreswoonplaats","woonplaats"]),
  Land = amember(Alist, ["adresland"]),
  format(Uit, "%s %s %s", PC, Plaats, Land),
  fronttoken(Uit,_,_), !,				% enige inhoud
  trim_c(Uit).
woonpl(In, AList, Uit, maat) :-
  not(fronttoken(In, _, _)),
  PC = amember(AList, ["adrespostcode","postcode"]),
  Plaats = amember(AList, ["adreswoonplaats","woonplaats"]),
  Land = amember(Alist, ["adresland"]),
  format(Uit, "%s %s %s", PC, Plaats, Land),
  fronttoken(Uit,_,_), !,				% enige inhoud
  trim_c(Uit).
woonpl(Uit, _AList, Uit, _).

oudOfNieuw(_Geb, Jaar, GebIn, GebIn) :-
  trim_c(Jaar),
  not(Jaar = ""), !.
oudOfNieuw(Geb, _, _, Geb).

nietVeranderen(Nr, Uit, NieuweNummer, Uit) :-
  fronttoken(NieuweNummer, _, _),
  alSpeler(_Naam, NieuweNummer, _Sex, Nr1, _TofS),	% naam met hetzelfde nummer
  Nr <> Nr1, !.
nietVeranderen(_Nr, Uit, NieuweNummer, Uit) :-
  not(fronttoken(NieuweNummer, _, _)), !.
nietVeranderen(_, _, Uit, Uit).

alSpeler(Naam, Knltb, Sex, Nr, 0) :- 
  t_sp(Nr,Sex,Naam,_,_,_,_,_,_,_,_,Knltb,_,_,_,_,_,_,_,_,_,_,_,_).
alSpeler(Naam, Knltb, Sex, Nr, 1) :-
  sp2(Nr,SexC,Naam,_,_,_,_,_,_, _, _, _, Knltb, _, _, _, _, _, _, _,_,_,_,_,_,_,_),
  spsrt(SexC, _, M_V,_,_,_,_,_), 
  sex1(Sex, M_V),
  not(t_sp(Nr,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)).

sex1("M", m) :- !.
sex1("V", v) :- !.
%sex1("", nvt).

welGeldigKNLTB(In, In) :-
  proef11det(In), !.
welGeldigKNLTB(_, "").

checkEmailAdres(Naam, EMailOud, EMAilNew, ResOud, ResResult,EmailResult, OpgMaat) :- 
  checkEmailAdres(Naam, EMailOud, EMAilNew, ResOud, ResResult,EmailResult, OpgMaat,
  "EMailadresverandering", "\n\n(Bij annuleren wordt het nieuwe adres overgeslagen)\n").

checkEmailAdres(_, EMail, EMail, Res, Res,Email, _, _, _) :- !.
checkEmailAdres(_, "", EmailNew, Res, Res,EmailNew, _, _, _) :- !.
checkEmailAdres(_, Email, _EmailNew, Res, Res,Email, maat, _, _) :- !. %(negeren!)
checkEmailAdres(Naam, EMailOud, EMAilNew, ResOud, ResResult,EmailResult, opgever, TitelX, BijCancel) :-
  format(Msg, "Speler %s heeft een nieuw emailadres opgegeven:\nOud:  %s\nNieuw: %s\nHet oude bewaren in het opmerkingenveld?%s", Naam, EMailOud, EMailNew,BijCancel),
  X = dlg_MessageBox( TitelX, Msg, mesbox_iconquestion, mesbox_buttonsyesnocancel, mesbox_defaultfirst, mesbox_suspendapplication ),
  overnemen(X, EmailOud, EmailNew, EmailResult, ResOud, ResResult), !.
checkEmailAdres(_, _, EmailNew, Res, Res, EmailNew, _, _, _).

overnemen(1, TelOud, TelNew, TelNew, ResOud, ResResult) :-
  extraspatie1(ResOud, Spatie),
  format(ResResult, "%s%s(was: %s)", ResOud, Spatie, TelOud), !.
overnemen(2, _TelOud, TelNew, TelNew, ResOud, ResOud) :- !.
overnemen(_, TelOud, _TelNew, TelOud, ResOud, ResOud).

extraspatie1("", "") :- !.
extraspatie1(_, " ").

fixBondEmails(EmailNew, EmailNewF) :-
  searchchar(EmailNew, ';', Pos),
  P1 = Pos - 1,
  frontstr(P1, EmailNew, EmailNewF, _), !. 
fixBondEmails(EmailNew, EmailNew).

checkTelefoon(Naam, Titel, TelOud, TelNew, ResOud, ResResult, TelResult, OpgMaat) :- !,
  checkTelefoon(Naam, Titel, TelOud, TelNew, ResOud, ResResult, TelResult, OpgMaat,
  "Telefoonnummerinvoer", "\n\n(Bij annuleren wordt het nieuwe nummer overgeslagen)\n").

checkTelefoon(_, _, TelOud, TelNieuw, Res, Res, TelOud, _, _, _) :-
  compLaatste7(TelOud, TelNieuw),
  !.
checkTelefoon(_, _, "", TelNew, Res, Res, TelNew, _, _, _) :- !.
checkTelefoon(_, _, Tel, _TelNew, Res, Res, Tel, maat, _, _) :- !.
checkTelefoon(Naam, Titel, TelOud, TelNew, ResOud, ResResult, TelResult, opgever, TitelX, BijCancel) :-
  format(Msg, "Speler %s heeft een nieuw %s telefoonnummer opgegeven:\nOud:  %s\nNieuw: %s\nHet oude bewaren in het opmerkingenveld?%s", Naam, Titel, TelOud, TelNew, BijCancel),
  X = dlg_MessageBox( TitelX, Msg, mesbox_iconquestion, mesbox_buttonsyesnocancel, mesbox_defaultfirst, mesbox_suspendapplication ),
  overnemen(X, TelOud, TelNew, TelResult, ResOud, ResResult), !.
checkTelefoon(_, _, _, TelNew, Res, Res, TelNew, _, _, _).

alleencijfers(Str, In, Uit) :-
  frontchar(Str,Char,RestString),
  Char >= '0',
  Char <= '9',
  str_char(SChar, Char), !,
  concat(SChar, In, In1),
  alleencijfers(RestString, In1, Uit).
alleencijfers(Str, In, Uit) :-
  frontchar(Str,_,RestString), !,
  alleencijfers(RestString, In, Uit).
alleencijfers(_, In, In).

compLaatste7(In1, In1) :- !.
compLaatste7(In1, In2) :-
  alleencijfers(In1, "", In11),
  alleencijfers(In2, "", In12),
  str_len(In11, L1),
  str_len(In12, L2),
  L1 > 6,
  L2 > 6,
  frontstr(7,In11,Start1,_),
  frontstr(7,In12,Start2,_),
  Start1 = Start2, !.

importNummer(opgever, Import, ImportN) :-
  filteren(b_False),
  t_nr(_, Nr),		% referentie (niet gebruikt)
  not(member(Nr, Import)),
  append(Import, [Nr], ImportN),
  sortstring_c(ImportN, 1), !.
  %not(searchchar(Nr, '?', _)),
  %format(Import, "Import %s.", Nr),
  %lookfor(Import, OpmIn, AlreadyIn),
  %voegToe(AlreadyIn, OpmIn, Import, OpmIn, OpmUit,b_False), !.
importNummer(_, Import, Import).

database - match
  syno(string, wedscat)
determ  vorigeinschr(integer PKeus, string Vorig)

predicates
openElem1 : xml_StartElementHandler
completeerElem : xml_EndElementHandler
nondeterm candidaatOnderdeel(string NaamIn, string EDIn, wedsex MVIn, string NaamX)
%determ toStr(integer, string)
determ openElem(integer Userdata, string Naam, slist AttrPairs)
determ    checkNaamKnltb(slist, string, string Naam)
determ formTRecord(integer SpNo, integer Control)
determ taNaam(slist, string TANaam, string TANaamX)
  uniekenaam(string, string)
procedure voornaam(string, string) - (i, o)
determ deelnemer(slist,spmaat)
determ candidaten(string VolleNaam, string VolleNaamMetSpatie, string Achternaam, 
		string Voorletter, string SexIn, string KnltbN, string NaamO, string Insnr) - (i,i,i,i,i,i,o,i)
determ candidatenX(slist Namen, string VolleNaam, string VolleNaamMetSpatie, string Naam, string INr) - (i,i,i,o,i)
nondeterm candidaat(string Achternaam, string Voorletter, string SexIn, string KnltbN, string NaamO)
determ   matchNaam(string NaamL, slist Candidaten, string SelectedNaam)
determ compKnltb(string Oud, string Nieuw)
verhindering(slist)
procedure vervangEventueel(string, string)
procedure string getInschrijfnummer(string FileNaam)
procedure schoneLei
determ uniekeKorteNaam(string ED, wedsex MV, string Kort)
determ selectedOnderdeel(string NaamIn, string ED, wedsex MV, string SelectedNaam, wedscat Cat)
procedure voorwie(wedsex MVG, sexlist)
determ edgConv(string ED, string MVG, wedstyp, wedsex) - (i, o, o, i) % (i, i, o, o) oud
%procedure removespatie(string, string)
procedure writebin(string)
%procedure nieuwonderdeelnodig(string SelectedNaam)
procedure erIsEenVorige(string)
procedure handleVorig(integer)
procedure deleteOpgaven(integer)
procedure echteRef(string, string, string)
%procedure hola(string)
procedure metZichzelf(tgnst, tgnst, integer Uitsluiten, string, wedscat)
procedure removeDubbeleSpaties(string, string)
nondeterm gesl2wedsex(string, wedsex)
determ compWedsex(wedsex,wedsex)
procedure uniekeLangeNaam(string, string)

clauses

getInschrijfnummer(XFull, Nr) :-
  filenamepath(XFull,_Path,X),
  upper_lower(XU, X),
  Zoekwoord = "OPGAVE",
  searchstring(XU,Zoekwoord,FoundPos),
  FoundPos = 1,
  str_len(Zoekwoord,Length),
  frontstr(Length,X,_,X1),
  filenameext(X1, Nr, _), !.
getInschrijfnummer(_XFull, "???").

schoneLei() :-
  retractall(t_sp(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)),
  retractall(t_verh(_)),
  retractall(t_wc(_, _)),
  retractall(t_verh(_)),
  retractall(t_maat(_)),
  retractall(t_enkel(_, _)),
  retractall(t_dubbel(_, _, _)),
  retractall(t_club(_, _)),
  retractall(cacheOpg(_,_,_,_)).

xml_inschrijfInvoerInsExp(Filenamen) :-	
  member(FileNaam, Filenamen),
  Inschrijfnummer = getInschrijfnummer(FileNaam),
  assert(t_nr(1, Inschrijfnummer)),
  schoneLei(),
  file_bin(FileNaam, Bin),
  tokenizeAllExpat(Bin, _Rest), 
  softstop, !.
xml_inschrijfInvoerInsExp(_).

xml_inschrijfInvoerInsExp1() :-
  retract(mxml(Code, Nr, XML)),  % vanuit memory
  format(InschrijfNummer, "%s%s", Code, Nr),
  format(M, "Opgave %s wordt ingevoerd...", InschrijfNummer),
  checkvoortgang(8, M),
 _Result = vpi_ProcessEvents(),
  schoneLei(),
  str_len(XML, Len),
  Bin = composebinary(XML, Len),
  assert(t_nr(1, Inschrijfnummer)),
  tokenizeAllExpat(Bin, _Rest), 
  softstop, !.
xml_inschrijfInvoerInsExp1().

tokenizeAllExpat(Bin, "") :- 
  %Parser = xml_ParserCreate("UTF-8"),
  Parser = xml_ParserCreate("ISO-8859-1"),
  xml_SetStartElementHandler(Parser, openElem1),
  xml_SetEndElementHandler(Parser, completeerElem),
  Len = getBinarySize(Bin),
  Status = xml_Parse(Parser, Bin, Len, 1),
  write("\nstatus = ", Status),
  xml_ParserFree(Parser).

openElem1(UserData, Naam, Attrs) :-   % vorm de Attrs om
  not(softstop),
  AttrList = xml_GetAttributes(Attrs),
  openElem(UserData, Naam, AttrList), !. 
openElem1(_UserData, _Naam, _Attrs).

erIsEenVorige(Vorig) :-
  retractall(vorigeinschr(_, _)),
  fronttoken(Vorig, _, _),
  sp2(PKeus,_,_PNaam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_Opm,_,_,_,_,_,_,Import),
  member(Vorig, Import),
  %searchstring(Opm,Vorig,_FoundPos),
  assert(vorigeinschr(PKeus, Vorig)), !.
erIsEenVorige(_Vorig).

handleVorig(SpelerNo) :-
  vorigeinschr(SpelerNo, VorigID),
  t_sp(SpelerNo,Sex,Naam,TelResult,_Verh,Offset,Betaald,_UitKantNew,ClubNew,AdrNew,WnplNew,KnltbNew,StSNew,StDNew,
  		GebNew,Tel1Result,Tel2Result,EMailResult,Opm,ModeSpelerMaat,Incasso2,IncassoOK2,Ins,Import),
  t_nr(_, Ref),
  format(Str, "De huidige inschrijving %s voor %s \nvervangt de vorige (%s) omdat\nhet een herzien formulier is.\n\nNB Wordt geel gemarkeerd in het Opgave Control Centrum.", Ref, Naam, VorigId),
  1 = dlg_MessageBox( "Formulier-update?", Str, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), !,
  retractall(t_sp(SpelerNo,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)),
  deleteopgaven(SpelerNo),
  asserta(t_sp(SpelerNo,Sex,Naam,TelResult,[],Offset,Betaald,0,ClubNew,AdrNew,WnplNew,KnltbNew,StSNew,StDNew,
  		GebNew,Tel1Result,Tel2Result,EMailResult,Opm,ModeSpelerMaat,Incasso2,IncassoOK2,Ins,Import)).
handleVorig(_GevondenSpeler).
    
deleteOpgaven(SpNo) :-
  sp2opg(SpNo, Cat, Nr1, _, _, _),
  opg(Nr1,Cat,Tgnst,A,B,C,D,E,TeamId,Keur,Melding),
  not(in_weds(Cat, Tgnst)),
  logf('R', opg(Nr1,Cat,Tgnst,A,B,C,D,E,TeamId,Keur,Melding),nee),
  retractall(t_opg(Nr1, Cat, _, _)),
  assert(cacheOpg(Nr1, Cat, Tgnst,Keur)),
  fail.
deleteOpgaven(_).

echteRef(ERef, _, ERef) :- 
  fronttoken(ERef,_,_), !.
echteRef(_, Ref, Ref).

%hola("ILU043") :-
%  write(), !.
%hola(_).

gesl2wedsex("M", heren).
gesl2wedsex("V", dames).
gesl2wedsex("G", gemengd).
gesl2wedsex(_, hdnvt).

openElem(_, "INSCHRIJVING", AttrList) :-
  %write("\ninschrijving"),
  Dag = amember(AttrList, ["dag"]),
  Maand = amember(AttrList, ["maand"]),
  Jaar = amember(AttrList, ["jaar"]),
  Tijd = amember(AttrList, ["tijd"]),
  Vorig = amember(AttrList, ["vorig"]),
  Referentie = amember(AttrList, ["referentie"]),
  %hola(Referentie),
  erIsEenVorige(Vorig),
  format(Str, "%s-%s-%s %s", Dag, Maand, Jaar, Tijd),
  trap(dt_minstr_to_offset(Str, "%Dd-%Md-%YL %Hh:%Mm", Nr), _, true),
  t_nr(_, Ref),		% 13.12.2009  nieuw!!!  tzt komt de ref uitsluitend uit de xml
  echteRef(Referentie, Ref, ERef),
  assert(t_nr(Nr, ERef)), !.				% zet een nieuw nummer
openElem(_, "TOERNOOIONDERDEEL", AttrList) :-		% van de bondsinschrijvingen
  %Naam = amember(AttrList, ["naam"]),
  %Spelsoort = amember(AttrList, ["spelsoort"]),
  Geslacht = amember(AttrList, ["geslacht"]),
  gesl2wedsex(Geslacht, Wedsex),
  Speelsterkte = amember(AttrList, ["speelsterkte"]),
  LeeftijdVanAf = amember(AttrList, ["leeftijdvanaf"]),
  LeeftijdTotenMet = amember(AttrList, ["leeftijdtotenmet"]),
  format(Naam, "%s, Speelst: %s, lftdVanaf: %s, lftdTotenmet: %s", Geslacht, Speelsterkte, LeeftijdVanAf, LeeftijdTotenMet),
  herkenOnderdeel(Naam, "E", Wedsex, Speelsterkte, LeeftijdVanAf, LeeftijdTotenMet, Cat),
  assert(t_wc(Cat, 0)), !.
openElem(_, "ONDERDEEL", AttrList) :-
  Naam = amember(AttrList, ["naam"]),
  Spelsoort = amember(AttrList, ["spelsoort"]),
  Geslacht = amember(AttrList, ["geslacht"]),
  gesl2wedsex(Geslacht, Wedsex),
  Speelsterkte = amember(AttrList, ["speelsterkte"]),
  LeeftijdVanAf = amember(AttrList, ["leeftijdvanaf"]),
  LeeftijdTotenMet = amember(AttrList, ["leeftijdtotenmet"]),
  herkenOnderdeel(Naam, Spelsoort, Wedsex, Speelsterkte, LeeftijdVanAf, LeeftijdTotenMet, Cat),
  t_nr(Nr, _),
  assert(t_wc(Cat, Nr)), !.
openElem(_Xx, "SPELER", AttrList) :-
  %write("\nspeler"),
  deelnemer(AttrList, opgever),
  t_sp(Speler,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), !,	% kijk wie het geworden is staat vooraan
  handleVorig(Speler),		% in geval van update
  retractall(t_speler(_)),
  assert(t_Speler(Speler)).		% bewaar de speler die opgave doet
openElem(_Xx, "DEELNEMER", AttrList) :-		% werd al aan gewerkt (t_sp), zet hem dan vooraan
  deelnemer(AttrList, opgever), !.
openElem(_Xx, "PARTNER", AttrList) :-
  deelnemer(AttrList, maat), !.
openElem(_Xx, "TEAM", AList) :- !,			% van de bondsinschrijvingen
  deelnemer(AList, opgever),
  t_sp(Speler,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), !,	
  t_wc(Cat, _),
  asserta(t_opg(0, Cat, s(Speler), "")), !.
openElem(_Xx, "OPGAVE", _AList) :- !,			% #########################################
  retractall(t_speler(_)).  
openElem(_Xx, "VERHINDERING", AList) :- 
  filteren(b_False), !,
  verhindering(AList).
openElem(_,_N, _Attrs).
  
verhindering(AList) :-
  Dag = amember(AList, ["dag"]),
  str_int(Dag, Idag),
  Maand = amember(AList, ["maand"]),
  str_int(Maand, Imaand),
  Jaar = amember(AList, ["jaar"]),
  str_int(Jaar, Ijaar),
  trap(dt_date_to_offset(Ijaar, Imaand, Idag, DO), _, true),
  TijdVan = amember(AList, ["tijdvan"]),
  TijdTot = amember(AList, ["tijdtot"]),
  vervangEventueel(TijdVan, TijdVan1),
  vervangEventueel(TijdTot, TijdTot1),
  bitleft(DO, 7, DIntern),
  trap(dt_timestr_to_offset(TijdVan1, "%Hh:%Mm", TV),_,true),
  roosterinterval(RI),
  TVKwartieren = TV div RI,
  trap(dt_timestr_to_offset(TijdTot1, "%Hh:%Mm", TT),_,true),
  TTKwartieren = TT div RI,
  bitor(DIntern, TVKwartieren, TVIntern),
  bitor(DIntern, TTKwartieren, TTIntern),
  Verhindering = per(TVIntern, TTIntern),
  assert(t_verh(Verhindering)), !.
verhindering(AList) :-
  term_str(slist, AList, AListS),
  t_nr(_, INr),
  format(Msg,"Verhindering niet herkend\n%s\nOpgave: %s", AListS, INr),  
  dlg_MessageBox( "Verhindering", Msg, mesbox_iconexclamation, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
  !.

deelnemer(AttrList, ModeSpMaat) :-   % deelnemer matching met wat al bestaat
  Knltb = amember(AttrList, ["knltbnummer","bondsnummer"]),	
  not(Knltb = ""),			% niet blanko
  not(Knltb = "00000000"),
  proef11det(Knltb),			% en geldig 17.1.2009
  getbacktrack(Here),
  alSpeler(Naam, Knltb, _Sex, Nr, TofS),	% naam met hetzelfde nummer
  cutbacktrack(Here),		% 20.8.2003
  checkNaamKNLTB(AttrList, Knltb, Naam), 
  formTRecord(Nr, TofS),		% er is een bestaande met hetzelfde nummer
  updateSp(AttrList, ModeSpMaat), !.
deelnemer(AttrList, ModeSpMaat) :-
  taNaam(AttrList, VolleNaam, VollenaamX),
  ANaam = amember(AttrList, ["achternaam"]),
  VoorNaam1 = amember(AttrList, ["voornaam"]),
  voornaam(Voornaam1, Voornaam),
  trim_c(VoorNaam),
  concat(VoorNaam, " ", VoorNaamY),
  frontstr(1, VoorNaamY, VoorlY, _),
  SexIn = amember(AttrList, ["geslacht"]),
  KnltbN = amember(AttrList, ["knltbnummer","bondsnummer"]),
  t_nr(_, INr),	
  candidaten(VolleNaam, VolleNaamX, ANaam, VoorlY, SexIn, KnltbN, NaamO, INr),  % (i,i,i,i,i,i,o,i) candidaten hebben een ander nummer
  alSpeler(NaamO, _Knltb, _, Nr, TofS), % zoek het bijbehorende nummer weer op
  formTRecord(Nr, TofS),		% eentje gekozen met een ander nummer
  updateSp(AttrList, ModeSpMaat), !.
deelnemer(AttrList,ModeSpMaat) :-
  taNaam(AttrList, NaamIn, _),
  uniekeNaam(NaamIn, NaamIn1),
  getbacktrack(Here),
  SexIn = amember(AttrList, ["geslacht"]),
  repeat,
  random(30000,Y),       % bepaal vrij nummer 
  SKeus = Y + 1,
  not(t_sp(SKeus,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)),	% pak een nietbestaand nummer
  not(sp2(SKeus,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)),
  cutbacktrack(Here),
  t_nr(_, Ins),
  asserta(t_sp(SKeus,SexIn,NaamIn1,"",[],[],nee,0,"","","","","","","","","","","",maat,"","",Ins,[])), % begin met maat	! 26.3.2010
  updateSp(AttrList, ModeSpMaat), !.

candidaten(VolleNaam, VolleNaamX, Achternaam, Voorletter, SexIn, KnltbN, NaamO, INr) :- %(i,i,i,i,i,i,o,i)
  findall(Naam, candidaat(Achternaam, Voorletter, SexIn, KnltbN, Naam), Namen),
  candidatenX(Namen, VolleNaam, VolleNaamX, NaamO, Inr), !.

writebin(String) :-
  comline(LineBuffer),
  upper_lower(LineBuffer, LBL),
  searchstring(LBL, "/x", _),
  str_len(String, Len),
  Bin = composebinary(String, Len),
  writef("\n%B", Bin), !. 
writebin(_String).

candidatenX([Naam1], Naam, _, Naam1, _) :-
  trim_c(Naam1),
  trim_c(Naam),
  upper_lower(Naam1, Naam1a),
  upper_lower(Naam, Naama),
  writebin(Naam1a),
  writebin(Naama),
  Naama = Naam1a, !.
candidatenX([Naam1], _, Naam, Naam1, _) :-
  upper_lower(Naam1, Naam), !.
candidatenX([], _, _, _, _) :- !, fail.
candidatenX(Lijst, Naam, _, Vollenaam, _) :-
  member(Vollenaam, Lijst),
  upper_lower(Vollenaam, Naam), !.
candidatenX(Lijst, _, NaamX, Vollenaam, _) :-
  member(Vollenaam, Lijst),
  upper_lower(Vollenaam, NaamX), !.
candidatenX(_Lijst, VolleNaam, _, NaamO, _INr) :-
  candi(VolleNaam, NaamO), !.
candidatenX(Lijst, VolleNaam, _, NaamO, INr) :-
  Parent = vpi_GetTaskWin(),
  dlg_candidatenvergelijking_Create(Parent,Vollenaam, Lijst, NaamO, Inr),
  NaamO <> "",
  retractall(candi(VolleNaam, NaamO)),
  assert(candi(VolleNaam, NaamO)), !.
candidatenX(_Lijst, _VolleNaam, _VolleNaamX, "", _) :-
  softstop().				% bij het kiezen op cancel gedrukt.

candidaat(Achternaam, Voorletter, SexIn, KnltbN, NaamO) :-
  alSpeler(NaamO, KnltbO, SexO, _Nr, _TofS),	% loop door alle spelers
  SexIn = SexO,					% geslacht moet kloppen
  xml_naamscanNoXlate(NaamO, _, VoorlX, AchterX),
  upper_lower(Voorletter, VoorlX),
  upper_lower(Achternaam, AchterX),
  compKnltb(KnltbO, KnltbN).

compKnltb(_, "") :- !.
compKnltb("", _) :- !.
compKnltb(Knltb, Knltb) :- !.
compKnltb(Knltb1, Knltb2) :- 
  proef11det(Knltb1), 
  proef11det(Knltb2), !,
  fail.
compKnltb(_, _).

candidaatOnderdeel(_, EDIn, MVIn, NaamX) :-
  wc(Cat, _, _, NaamX, _, _, _, _, _, _, MVS, _, _, _, _),
  xSpelsoort(Cat, _MVSx, _MVd, EDS, _EDd),
  EDS = EDIn,		% spelsoort
  MVS = MVIn.
candidaatOnderdeel(NaamIn, _EDIn, _MVIn, ParkeerOnderdeel) :-
  format(ParkeerOnderdeel, "%s <= nieuw?", NaamIn).

matchNaam(NaamI, ONaamL, SelectedNaam) :-
  TaskWin = vpi_GetTaskWin(),
  dlg_onderdeelmatch_Create(TaskWin, NaamI, ONaamL, SelectedNaam).

uniekeLangenaam(Naam, NaamUit) :-
  wc(_, _, _, Naam, _, _, _, _, _, _, _, _, _, _, _), !,
  concat(Naam, "$",NaamPlus),
  uniekeLangenaam(NaamPlus,NaamUit).
uniekelangenaam(Naam, Naam).

uniekeKorteNaam(ED, MV, Kort) :-
  term_str(wedsex, MV, Str),
  numb(N),
  N1 = N + 1,
  format(Kort, "%s%s%u!", ED, Str, N1),
  not(wc(_, _, _, _, _, Kort, _, _, _, _, _, _, _, _, _)),!.

voorwie(heren, [Sex1]) :-
  spsrt(Sex1, _, m, _, _, _, _, _), !.
voorwie(dames, [Sex2]) :-
  spsrt(Sex2, _, v, _, _, _, _, _), !.
voorwie( _, [Sex1, Sex2]) :-
  spsrt(Sex1, _, m, _, _, _, _, _),
  spsrt(Sex2, _, v, _, _, _, _, _), !.
voorwie( _, []) :-
  dlg_MessageBox( "Man/vrouw spelersoorten", "Man en/of vrouw als spelersoort ontbreekt.\nZie Personen, Kenmerken.\nInvoer wordt afgebroken!", mesbox_iconerror, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  setsoftstop().

edgConv("E", "M", e, heren ) :-!.
edgConv("E", "V", e, dames) :-!.
edgConv("D", "G", d, gemengd) :-!.
edgConv(_, "G", d, hdnvt).


selectedOnderdeel(NaamIn, _ED, _MV, SelectedNaam, Cat) :-
  wc(Cat, _, _, SelectedNaam, _, _, _, _, _, _, _, _, _, _, _),
  %wc(Cat, SelectedNaam, _,_,_,_,_),
  assert(syno(NaamIn, Cat)), !.
selectedOnderdeel(NaamIn, _ED, _MV, _SelectedNaam, Cat) :-
  syno(NaamIn, Cat), !.  % hoeft denk ik niet
selectedOnderdeel(NaamIn, ED, Wedsex, _SelectedNaam, CatNo1) :- % het moet dus de nieuwe zijn
  not(softstop()),
  getbacktrack(Here),
  numb(CatNo),
  CatNo1 = CatNo + 1, 
  not(wc(CatNo1, _, _, _, _, _, _, _, _, _, _, _, _, _, _)),
  cutbacktrack(Here),
  uniekeKorteNaam(ED, Wedsex, Kort),
  uniekeLangeNaam(NaamIn, NaamUniek),
  voorwie(Wedsex, VoorWie),
  edgConv(ED, _WedsexOud, EDG, Wedsex),
  logf('A',wc(CatNo1, "", 0, NaamUniek, EDG, Kort, Voorwie, "", leeftijd("",0), leeftijd("",99), Wedsex, "", vanzelf, "", 0.0),nee),
  retractall(syno(NaamIn, CatNo1)),
  assert(syno(NaamIn, CatNo1)),
  retractall(nieuwOnderdeel),
  assert(nieuwOnderdeel), !.

removeDubbeleSpaties(In, Uit) :-
  searchstring(In,"  ",FoundPos),
  frontstr(FoundPos,In,StartString,RestString),
  frontstr(1,RestString,_,Tail), !,
  concat(StartString,Tail, I1),
  removeDubbeleSpaties(I1, Uit).
removeDubbeleSpaties(In, In).

compWedsex(_, hdnvt) :- !.
compWedsex(WS, WS).

herkenOnderdeel(NaamI, EDIn, MVIn, _Speelsterkte, _LeeftijdVanAf, _LeeftijdTotenMet, Cat) :-
  %upper_lower(Naam1, NaamI),			% herken op naam en sommige kenmerken
  wc(Cat, _, _, NaamX, _, _, _, _, _, _, WS, _, _, _, _),
  %wc(Cat, NaamX, _,_,_,_,_),
  removeDubbeleSpaties(NaamI, NaamIY),
  removeDubbeleSpaties(NaamX, NaamXY),
  upper_lower(NaamIY, NaamXY),
  xSpelsoort(Cat, _MVS, _MVd, EDS, _EDd),
  %Naam1 = Naam2,
  EDS = EDIn,		% spelsoort
  compWedsex(WS, MVIn), !.
  %WS = MVIn, !.	% geslacht
  %MVS = MVIn, !.	% geslacht
herkenOnderdeel(NaamI, EDIn, MVIn, _SpeelsterkteI, _LeeftijdVanAfI, _LeeftijdTotenMetI, CatS) :-
  findall(ONaam, candidaatOnderdeel(NaamI, EDIn, MVIn, ONaam), ONaamL),
  matchNaam(NaamI, ONaamL, SelectedNaam),
  selectedOnderdeel(NaamI, EDIn, MVIn, SelectedNaam, CatS),
  %wc(CatS, Naam, _,_,_,_,_),
  retractall(syno(NaamI, CatS)),
  assert(syno(NaamI, CatS)), !.
herkenOnderdeel(_NaamI, EDIn, MVIn, SpeelsterkteI, LeeftijdVanAfI, LeeftijdTotenMetI, Cat) :-
  wc(Cat, _, _, _, _, _, _, Speelsterkte, leeftijd(LeeftijdVanafS,_), leeftijd(LeeftijdTotenMetS,_), WSex, _, _, _, _),
  %wc(Cat, _NaamX, _,_,_,_,_), 
  xSpelsoort(Cat, _MVS, _MVd, EDS, _EDd),
  EDS = EDIn,		% spelsoort
  %WSex = MVIn,
  compWedsex(Wsex,MVIn),
  %MVS = MVIn,
  format(Kriteria, "%s%s%s", LeeftijdVanafS, LeeftijdTotenMetS, Speelsterkte),	% minstens een kriterium
  fronttoken(Kriteria, _, _),
  Speelsterkte = SpeelsterkteI,
  LeeftijdVanafS = LeeftijdVanafI,
  LeeftijdTotenMetS = LeeftijdTotenMetI,
  !.	% toch herkend !

vervangEventueel(In, Uit) :-
  searchchar(In,'.',FPos),
  FPos1 = FPos - 1,
  frontstr(FPos1,In,StartString,_),
  frontstr(FPos,In,_,Rest),
  format(Uit, "%s:%s", Startstring, Rest), !.
vervangeventueel(In, In).

checkNaamKNLTB(Alist, _Knltb, NaamIn) :-
  ANaam = amember(AList,["achternaam"]),
  VoorNaam1 = amember(AList, ["voornaam"]),
  %sp2(_Nr,_SexC,NaamIn,_,_,_,_,_,_, _, _, _, Knltb, _, _, _, _, _, _, _,_,_,_,_,_,_,_), 
  searchchar(NaamIn, ',', Pos),
  Pos > 2,
  str_len(NaamIn, Len),
  L1 = Pos - 1,
  L2 = Len - Pos,
  P1 = Pos + 1,
  substring(NaamIn,1,L1,Achternaam),
  trap(frontstr(L1, ANaam, AchternaamAccent, _), _, fail),
  upper_lower(Achternaam, AAL1),
  upper_lower(AchternaamAccent, AAL2),
  AAL1 = AAL2,			% achternaam gelijk
  substring(NaamIn,P1,L2, Voorletters),
  trim_c(Voorletters),
  upper_lower(Voorletters, VL1),
  frontchar(VL1,FC1,_),
  upper_lower(Voornaam1, VL2),
  frontchar(VL2, FC2, _),
  FC1 = FC2, !.			% eerste voorletter gelijk
checkNaamKNLTB(Alist, Knltb, Naam) :-
  t_nr(_, INr),
  taNaam(Alist, NaamIn, _),
  %sp2(_Nr,_,Naam,_,_,_,_,_,_, _, _, _, Knltb, _, _, _, _, _, _, _,_,_,_,_,_,_,_),
  format(Msg, "Opgave %s: %s\nIn bestand: %s\nBeiden hebben hetzelfde bondsnummer %s\nZijn ze dezelfde persoon?", INr, Naam, NaamIn, KNLTB), 
  1 = dlg_MessageBox( "Nieuwe persoon?", Msg, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ), !.

formTRecord(SpNo, 0) :-
  retract(t_sp(SpNo,SexT,Naam,Tel,Verh,Offset,Bet,UitKant,Club,Adr,Wnpl,Knltb,StS,StD,Geb,Tel1,Tel2,EMail,Res,Mode,Incasso,IncassoOK,Ins,Import)),
  asserta(t_sp(SpNo,SexT,Naam,Tel,Verh,Offset,Bet,UitKant,Club,Adr,Wnpl,Knltb,StS,StD,Geb,Tel1,Tel2,EMail,Res,Mode,Incasso,INcassoOK,Ins, Import)), !.
formTRecord(SpNo, 1) :-
  sp2(SpNo,SexC,NaamIn,Tel,Verh,_RatingE,_RatingD,_Betaald,Uitkant, Club, Adr, Wnpl, Knltb, StS, StD, Geb, Tel1, Tel2, EMail, Opm,_Gezien,_RangPosE,_RangPosD,Incasso,_CheckGeg,IncassoOK,Import),
  spsrt(SexC, _, M_V,_,_,_,_,_), 
  sex1(Sex, M_V),
  t_nr(_, Ins),
  asserta(t_sp(SpNo,Sex,NaamIn,Tel,Verh,[],nee,UitKant,Club,Adr,Wnpl,Knltb,StS,StD,Geb,Tel1,Tel2,EMail,Opm,maat,Incasso,IncassoOK,Ins,Import)), !.
						% begin met maat
taNaam(Alist, TANaam, TANaamX) :-
  ANaam = amember(Alist, ["achternaam"]),
  VoorNaam1 = amember(Alist, ["voornaam"]),
  voornaam(Voornaam1, Voornaam),
  TV = amember(AList, ["tussenvoegsels"]),
  trim_c(TV),
  upper_lower(TV, TVl),
  format(TANaam, "%s,%s %s", ANaam, VoorNaam, TVl),
  trim_c(TANaam),
  format(TANaamX, "%s, %s %s", ANaam, VoorNaam, TVl),
  trim_c(TANaamX), !.

uniekeNaam(NaamIn, NaamIn) :-
  not(t_sp(_,_,NaamIn,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)),
  not(sp2(_,_,NaamIn,_,_,_,_,_,_, _, _, _, _, _, _, _, _, _, _, _,_,_,_,_,_,_,_)), !.
uniekeNaam(NaamIn, NaamUit) :-
  concat(NaamIn, "?", NaamIn1),
  uniekeNaam(NaamIn1, NaamUit).

voornaam("?", "") :- !.
voornaam(V, V).

predicates
onderdeel()
inschrijving()
procedure oogst()
procedure check_opg_achteraf(wedscat Cat, tgnst T)
procedure checkKNLTB(string KNLTB, string KNLTB, string NaamNieuw, string InschrNr)
procedure resultaatVeld(spmaat ModeZelf, string N, string O, string Uit)
determ alOpg(integer, wedscat, tgnst, string Nr)
determ    partnerIs(integer, tgnst, integer P)
procedure oogstbond()
procedure usecacheOpg(integer Num, wedscat Cat, tgnst Tuit, integer NrX, teamstatus)	% 7.7.2010 hergebruik

clauses

completeerElem(_User, _Keyw) :-
 softstop,
 schonelei(), !.
completeerElem(_Xx, "TEAM") :- 
  oogstBond(), !.
completeerElem(Xx, "PARTNER") :- 
  completeerElem(Xx, "SPELER"),
  t_sp(SpNo,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),
  assert(t_maat(SpNo)), !.
completeerElem(_Xx, "SPELER") :-
  retract(t_sp(SpNo,Sex,_N,Tel,VerhO,Offset,nee,UitKant,Club,Adr,Wnpl,Knltb,StS,StD,Geb,Tel1,Tel2,EMail,Opm,Mode,Incasso,IncassoOK,Ins,Import)),
  findall(Verh, t_verh(Verh), VerhList),
  append(VerhO, VerhList, VerhNew),
  merge_verh(VerhNew, VerhNewM,_),
  retractall(t_verh(_)), !,
  asserta(t_sp(SpNo,Sex,_N,Tel,VerhNewM,Offset,nee,UitKant,Club,Adr,Wnpl,Knltb,StS,StD,Geb,Tel1,Tel2,EMail,Opm,Mode,Incasso,IncassoOK,Ins,Import)).
completeerElem(_Xx, "ONDERDEEL") :-
  onderdeel(), !.
completeerElem(_Xx, "TOERNOOIONDERDEEL") :-
  retractall(t_wc(_,_)), !.
completeerElem(_Xx, "INSCHRIJVING") :-
  inschrijving(), 
  oogst(), !.
completeerElem(_Xx, "PERSONEN") :-
  oogst(), !.
completeerElem(_, _EN).

onderdeel() :-
  t_wc(Cat, Tijd),				% dubbelspel
  wc(Cat, _, _, _, WedsTyp, _, _, _, _, _, _, _, _, _, _),
  %wc(Cat, _, WedsTyp,_,_,_,_),
  not(Wedstyp = e),
  retract(t_maat(Maat)),			% maat is er
  retract(t_wc(Cat, Tijd)),
  assert(t_dubbel(Tijd, Cat, Maat)), !.
onderdeel() :-
  t_wc(Cat, Tijd),				% partner wanted
  wc(Cat, _, _, _, Wedstyp, _, _, _, _, _, _, _, _, _, _),
  %wc(Cat, _, WedsTyp,_,_,_,_),
  not(Wedstyp = e),
  retract(t_wc(Cat, Tijd)),
  assert(t_dubbel(Tijd, Cat, -1)), !.		% eigenlijk t_pw
onderdeel() :-
  retract(t_wc(Cat,Tijd)),
  wc(Cat, _, _, _, e, _, _, _, _, _, _, _, _, _, _),
  %wc(Cat, _, e,_,_,_,_),			% enkelspel
  assert(t_enkel(Tijd, Cat)), !.
onderdeel() :-
  writeToJournaal("Opgave is niet compleet."), !.

inschrijving() :-
  retract(t_dubbel(Num, Cat, -1)),
  t_speler(Sp),
  t_nr(_, InschrNr),
  assert(t_opg(Num, Cat, pw(Sp), InschrNr)), 
  fail.
inschrijving() :-
  retract(t_dubbel(Num, Cat, Maat)),
  t_speler(Sp),
  t_nr(_, InschrNr),
  %t_sp(Sp,SexS,NaamS,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),
  %t_sp(Maat,SexP,NaamP,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),
  %orde1(Sp, Maat, SexS, SexP, NaamS, NaamP, p(Sp1, Sp2)),
  assert(t_opg(Num, Cat, p(Sp, Maat), InschrNr)), 
  fail.
inschrijving() :-
  retract(t_enkel(Num, Cat)),
  t_speler(Sp),
  t_nr(_, InschrNr),
  assert(t_opg(Num, Cat, s(Sp), InschrNr)),
  fail. 
inschrijving() :-
  retract(t_speler(_Sp)), !.

oogstBond() :-
  cursor_SetWait(),
  loglock(mdi, "oogst"),
  t_sp(SKeus,_MV,NaamN,TelN,VerhN,_,_,UitKantN,ClubN,AdrN,WnplN,KnltbN,StSN,StDN,GebN,Tel1N,Tel2N,EMailN,OpmN,Mode,IncassoN,IncassoOKN, _Ins,ImportN),
  sp2(SKeus,Sex,Naam,Tel,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adr, Wnpl, Knltb, StS, StD, Geb, Tel1, Tel2, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,IncassoOK,Import),
  resultaatVeld(Mode, AdrN, Adr, AdrR),
  resultaatVeld(Mode, WnplN, Wnpl, WnplR),
  resultaatVeld(maat, KnltbN, Knltb, KnltbO),	% de oude mits niet blanco
  update_sp(sp2(SKeus,Sex,Naam,Tel,Verh,"","",Betaald,UitKant, Club, Adr, Wnpl, Knltb, StS, StD, Geb, Tel1, Tel2, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,IncassoOK,Import),
            sp2(SKeus,Sex,NaamN,TelN,VerhN,RatingE,RatingD,Betaald,UitKantN, ClubN, AdrR, WnplR, KnltbO, StSN, StDN, GebN, Tel1N, Tel2N, EMailN, OpmN,Gezien,RangPosE,RangPosD,IncassoN,CheckGeg,IncassoOKN,ImportN)),
  fail.
oogstBond() :-
  cursor_SetWait(),
  retract(t_sp(SKeus,MV,Naam,Tel,Verh,_Offset,_,UitKant,Club,Adr,Wnpl,Knltb,StS,StD,Geb,Tel1,Tel2,EMail,Opm,_,Incasso,IncassoOK, Ins,Import)),
  not(sp2(SKeus,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)),	% nieuwe
  geslacht(MV, Geb, StS, SexN),
  trim_c(Knltb),
  checkKNLTB(KNLTB, KNLTBchecked, Naam, Ins),
  logf('A', sp2(SKeus,SexN,Naam,Tel,Verh,"","",nee,UitKant, Club, Adr, Wnpl, KnltbChecked, StS, StD, Geb, Tel1, Tel2, EMail, Opm,nee,"","",Incasso,"",IncassoOK,Import), ja),
  fail.
oogstBond() :-
  getbacktrack(Here1),
  retract(t_opg(_NrOud, Cat, Tx, _)),   %die vooraan staat
  cutbacktrack(Here1),
  not(opg(_, Cat, Tx,_,_,_,_,_,_,_,_)),
  date(Year,Month,Day),
  time(Hours,Minutes,_Seconds,_Hundredths),
  dt_min_to_offset(Year, Month, Day,Hours, Minutes,Num),
  getbacktrack(Here),
  numb(N),
  Nr1 = Num + N - 1,
  not(opg(Nr1,Cat,_,_,_,_,_,_,_,_,_)),
  cutbacktrack(Here),
  logf('A', opg(Nr1,Cat,Tx,0,0,0,0,"","",na,""),ja),
  retractall(t_opgvoorlijst(Nr1)),
  asserta(t_opgvoorlijst(Nr1)),		% voor de lijst straks
  fail.
oogstBond() :-
  logclose().

usecacheOpg(_, Cat, Tuit, Nr, TStat) :-	% 7.7.2010 hergebruik
  retract(cacheOpg(Nr, Cat, Tuit, TStat)),
  not(opg(Nr,Cat,_,_,_,_,_,_,_,_,_)), !.	% 9.7.2010 voor de zekerheid.
usecacheOpg(Num, Cat, _Tuit, Nr1, na) :-	% 7.7.2010 hergebruik
  getbacktrack(Here),
  numb(N),
  Nr1 = Num + N - 1,
  not(opg(Nr1,Cat,_,_,_,_,_,_,_,_,_)),	% 9.7.2010 uniek!	
  cutbacktrack(Here), !.
usecacheOpg(Num, _Cat, _Tuit, Num, na).	%voor de proc

oogst() :-
  softstop, !.
%oogst() :-
%  write("\nOogst:"),
%  fail.
oogst() :-
  t_opg(Nr, Cat, T, InschrNr),
  tgnstNum(T, SKeus),
  xSpelsoort(Cat, _, CatMVd, _, _),
  t_sp(SKeus,MV,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),
  not(compare(CatMVd, MV, SKeus, Cat, InschrNr)),	% check het geslacht kan alleen bij niet gemengd
  retract(t_opg(Nr, Cat, T, InschrNr)),
  fail.
oogst() :-
  cursor_SetWait(),
  loglock(mdi, "oogst"),
  t_sp(SKeus,_MV,NaamN,TelN,VerhN,_,_,UitKantN,ClubN,AdrN,WnplN,KnltbN,StSN,StDN,GebN,Tel1N,Tel2N,EMailN,OpmN,Mode,IncassoN,IncassoOKN, _Ins,ImportN),
  sp2(SKeus,Sex,Naam,Tel,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adr, Wnpl, Knltb, StS, StD, Geb, Tel1, Tel2, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,IncassoOK,Import),
  splitGeb(Geb,_,Pasp,_),
  addPaspoort(GebN,Pasp,GebNY),
  resultaatVeld(Mode, AdrN, Adr, AdrR),
  resultaatVeld(Mode, WnplN, Wnpl, WnplR),
  resultaatVeld(maat, KnltbN, Knltb, KnltbO),	% de oude mits niet blanco
  update_sp(sp2(SKeus,Sex,Naam,Tel,Verh,"","",Betaald,UitKant, Club, Adr, Wnpl, Knltb, StS, StD, Geb, Tel1, Tel2, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,IncassoOK,Import),
            sp2(SKeus,Sex,NaamN,TelN,VerhN,RatingE,RatingD,Betaald,UitKantN, ClubN, AdrR, WnplR, KnltbO, StSN, StDN, GebNY, Tel1N, Tel2N, EMailN, OpmN,Gezien,RangPosE,RangPosD,IncassoN,CheckGeg,IncassoOKN,ImportN)),
  fail.
oogst() :-
  cursor_SetWait(),
  retract(t_sp(SKeus,MV,Naam,Tel,Verh,_Offset,_,UitKant,Club,Adr,Wnpl,Knltb,StS,StD,Geb,Tel1,Tel2,EMail,Opm,_,Incasso,IncassoOK, Ins,Import)),
  not(sp2(SKeus,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)),	% nieuwe
  %anderesexe(Anders),
  geslacht(MV, Geb, StS, SexN),			% nog uit te breiden!!
  trim_c(Knltb),
  checkKNLTB(KNLTB, KNLTBchecked, Naam, Ins),
  logf('A', sp2(SKeus,SexN,Naam,Tel,Verh,"","",nee,UitKant, Club, Adr, Wnpl, KnltbChecked, StS, StD, Geb, Tel1, Tel2, EMail, Opm,nee,"","",Incasso,"",IncassoOK,Import), ja),
  fail.
oogst() :-					% voor de dubbels
  loglock(mdi, "oogst"),
  wc(Cat, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
  %wc(Cat, _, _,_,_,_,_),
  findall(I, t_opg(I, Cat, _, _),IList),
  member(Num, IList),
  getbacktrack(Herex),
  retract(t_opg(Num, Cat, Tx1, Nr)),
  metZichzelf(Tx1, Tx, Uitsl, Nr, Cat),
  setmarkH(Tx, Cat),  % voor de lijst straks
  cutbacktrack(Herex),
  orderedTgnst(Tx, Tuit),		% 
  not(alOpg(Num, Cat, Tuit, Nr)),
  not(aborted(Tx)),
  check_opg_achteraf(Cat, Tuit),
  usecacheOpg(Num, Cat, Tuit, NrX, TStat),			% 7.7.2010 hergebruik
  logf('A', opg(NrX,Cat,Tuit,0,0,Uitsl,0,"","",TStat,""),ja),
  fail.
oogst() :-
  retract(cacheOpg(Nr, Cat, Tuit, TStat)),
  not(opg(_,Cat,Tuit,_,_,_,_,_,_,_,_)),	 	% 22.6.2011 voor de zekerheid.
  tgnst_naam(nee, b_False, Cat, Tuit, Naam1, _),
  not(Naam1 = "??"),
  wc(Cat, _, _, CNaam, _, _, _, _, _, _, _, _, _, _, _),
  %wc(Cat, CNaam, _,_,_,_,_),
  format(Msg, "De opgave van %s\nvoor %s\n is misschien afgezegd!\nWordt gemarkeerd als uitgesloten.\nKijk deze na svp!", Naam1, CNaam),
  dlg_MessageBox( "Opgave?", Msg, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  not(opg(Nr,Cat,_,_,_,_,_,_,_,_,_)),
  logf('A', opg(Nr,Cat,Tuit,0,0,1,0,"","",TStat,""),ja),
  fail.
oogst() :-
  retract(t_club(Code, Club)),
  logf('Z', club(Code, Club, ""),ja),
  fail.
oogst() :-
  logclose().

metZichzelf(p(S,S), pw(S), 1, Ins, Cat) :- 	% 31.5.2012 verander in partner wanted
  sp2(S,_,Naam,_,_,_,_,_,_, _, _, _, _, _, _, _, _, _, _, _,_,_,_,_,_,_,_),
  wc(Cat, _, _, CNaam, _, _, _, _, _, _, _, _, _, _, _),
  %wc(Cat, CNaam, _,_,_,_,_),
  format(Msg, "%s heeft met zichzelf opgegeven voor %s.\nOpgave omgezet in 'partner wanted' en uitgesloten.\n(Opgavenummer: %s).",Naam, CNaam, Ins),
  dlg_MessageBox( "Nieuwe opgave", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  !.
metZichzelf(Tgnst, Tgnst, 0, _, _).

check_opg_achteraf(Cat, T) :-
  checkOpgegCrit(Cat, T, b_True), !.
check_opg_achteraf(_Cat, _T).

checkKNLTB(KNLTB, "", NaamNieuw, INr) :-
  not(KNLTB = ""),
  sp2(_Nr,_SexC,Naam,_,_,_,_,_,_, _, _, _, Knltb, _, _, _, _, _, _, _,_,_,_,_,_,_,_),
  format(Msg, "Voor de nieuwe persoon %s\nis hetzelfde bondsnummer (%s)opgegeven\nals voor %s.\nNummer niet overgenomen (Opgave: %s).",NaamNieuw, KNLTB,Naam,INr), 
  assert(nakijken(INr)),
  assert(nakijken1(Naam)),
  assert(nakijken1(NaamNieuw)),
  dlg_MessageBox( "Nieuwe persoon", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  writeToJournaal(Msg),
  !.
checkKNLTB(KNLTB, KNLTB, _, _).

resultaatVeld(_, InN, InO, InN) :-
  not(fronttoken(InO, _, _)), !. 	% als  de oude niets heeft sowieso de nieuwe
resultaatVeld(opgever, InN, _, InN) :- !.	% als het de speler zelf is ook de nieuwe
resultaatVeld(_, _, InO, InO). 		% anders de oude

alopg(_, _, p(S,S),_) :- !.	% met zichzelf, moet niet kunnen!
alopg(Num, Cat, TX, _Ins) :-
  opg(_,Cat,TX,_,_,_,_,_,_,_,_),
  asserta(t_opgvoorlijst(Num)), !.		% voor de lijst straks
alopg(_Num, Cat, pw(X), _Ins) :-
  opg(_,Cat,TX,_,_,_,_,_,_,_,_),
  tgnstNum(TX, Sp),
  Sp = X,
  sp2(Sp,_,Naam,_,_,_,_,_,_, _, _, _, _, _, _, _, _, _, _, _,_,_,_,_,_,_,_),
  wc(Cat, _, _, _, _, NKort, _, _, _, _, _, _, _, _, _),
  %wc(Cat, _CatNaam,_, NKort,_,_,_),
  partnerIs(Sp, TX, NPartner),
  sp2(NPartner,_,NaamP,_,_,_,_,_,_, _, _, _, _, _, _, _, _, _, _, _,_,_,_,_,_,_,_),
  format(Msg, "%s: opgave 'zoekt partner' van %s genegeerd want partner is al %s!", NKort, Naam, NaamP),
  dlg_MessageBox( "Inschrijving", Msg, mesbox_iconinformation, mesbox_buttonsOK, mesbox_defaultfirst, mesbox_suspendapplication ), !.
alopg(_Num, Cat, p(X, Y), _Ins) :-
  tgnstnum(p(X,Y), Z),
  opg(Nr1,Cat,pw(Z),A,B,C,D,E,TeamId,Keur,Melding),
  logf('R', opg(Nr1,Cat,pw(Z),A,B,C,D,E,TeamId,Keur,Melding),ja),
  fail.
alopg(_, Cat, TX, Ins) :-
  tgnstnum(TX, Sp1),
  opg(_,Cat,TY,_,_,_,_,_,_,_,_),
  tgnstnum(TY, Sp2),
  Sp1 = Sp2,
  partnerIs(Sp1, TX, NPartner),
  partnerIs(Sp2, TY, Partner),
  sp2(Sp1,_,Naam,_,_,_,_,_,_, _, _, _, _, _, _, _, _, _, _, _,_,_,_,_,_,_,_),
  sp2(Partner,_,NaamP,_,_,_,_,_,_, _, _, _, _, _, _, _, _, _, _, _,_,_,_,_,_,_,_),
  %sp2(NPartner,_,NaamNP,_,_,_,_,_,_, _, _, _, _, _, _, _, _, _, _, _,_,_,_,_,_,_,_),
  sp2(Npartner,Sex,NaamNP,Tel,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adr, Wnpl, Knltb, StS, StD, Geb, Tel1, Tel2, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,IncassoOK,Res),
  wc(Cat, _, _, CatNaam, _, NKort, _, _, _, _, _, _, _, _, _),
  %wc(Cat, CatNaam,_, NKort,_,_,_),
  assert(nakijken(Ins)),
  assert(nakijken1(Naam)),
  assert(nakijken1(NaamP)), 
  assert(nakijken1(NaamNP)),
  format(Msg, "%s\n%s staat al opgegeven met %s.\nNieuwe opgave met %s genegeerd!\nNeem svp zelf actie (Opgave: %s).",CatNaam, Naam, NaamP, NaamNP, Ins),
  dlg_MessageBox( "Nieuwe opgave", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  %	 16.6.2008
  format(OpmGeneg, " Opg voor %s met %s genegeerd!", NKort, Naam), 
  lookfor(OpmGeneg, Opm, AlreadyIn),
  voegToe(AlreadyIn, Opm, OpmGeneg, Opm, OpmN, 2),
  update_sp(sp2(Npartner,Sex,NaamNP,Tel,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adr, Wnpl, Knltb, StS, StD, Geb, Tel1, Tel2, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,IncassoOK,Res),
            sp2(Npartner,Sex,NaamNP,Tel,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adr, Wnpl, Knltb, StS, StD, Geb, Tel1, Tel2, EMail, OpmN,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,IncassoOK,Res)),
  %
  !.

partnerIs(No, p(No,P), P) :- !.
partnerIs(No, p(P,No), P).

xml_controlelijst(_Window) :-
  providerX(Prov, _, "", _),
  AV = aanmeldenvars(),
  get_tempdir(_, Download),
  deletefile(Download),
  format(Volgend,"http://%s/getopgx2009.php?stap=allerap",Prov), 
  Return = downloadData(Volgend, Download, AV, _, 0),
  verbindingMislukt(Return), !.
xml_controlelijst(_Window) :-
  get_tempdir(_, Download),
  file_str(Download, Str),
  not(fronttoken(Str, _, _)),
  file_str(Download, "geen items gevonden."),
  fail.  
xml_controlelijst(Window) :-
  dlg_rapport_Create(Window, "de opgaven", "opgaven.dat", b_False),
  !.  
xml_controlelijst(_). 



%BEGIN_DLG Onderdeelmatch
/**************************************************************************
	Creation and event handling for dialog: Onderdeelmatch
**************************************************************************/

constants

%BEGIN Onderdeelmatch, CreateParms, 09:58:39-13.4.2014, Code automatically updated!
  dlg_onderdeelmatch_DlgType = wd_Modal
  dlg_onderdeelmatch_Title = "Onderdeelmatch"
  dlg_onderdeelmatch_RCT = rct(50,40,429,174)
  dlg_onderdeelmatch_Flags = [wsf_Close,wsf_TitleBar]
  dlg_onderdeelmatch_Help = idh_onderdeeleigenschappen
  dlg_onderdeelmatch_Font = "Arial"
  dlg_onderdeelmatch_FSize = 9
%END Onderdeelmatch, CreateParms

database - matchdia
single  waarden(string In, slist Candidaten)
single oselected(string Uit)

predicates

  dlg_onderdeelmatch_eh : EHANDLER
  determ positie(integer, integer Uit, string SelectedNaam, slist Candidaten)

clauses

waarden("", []).
oselected("").


  dlg_onderdeelmatch_Create(Parent, OnderdeelIn, Candidaten, Selected):-
  	assert(waarden(OnderdeelIn, Candidaten)),
  	removeDubbeleSpaties(OnderdeelIn, _OD),
	win_CreateDynDialog(Parent,
		[
%BEGIN Onderdeelmatch, WinDefList, 09:58:39-13.4.2014, Code automatically updated!
		 dlg_font(wdef(dlg_onderdeelmatch_DlgType,dlg_onderdeelmatch_RCT,dlg_onderdeelmatch_Title,u_DlgBase),
		 	  dlg_onderdeelmatch_Font,dlg_onderdeelmatch_FSize,dlg_onderdeelmatch_Flags),
		 ctl(wdef(wc_LBox,rct(187,9,378,117),"",u_DlgBase),idc_onderdeellijst,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight]),
		 ctl(wdef(wc_PushButton,rct(339,119,379,132),"&OK",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop,wsf_Disabled]),
		 ctl(wdef(wc_PushButton,rct(197,120,251,132),"&Stop import",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_Text,rct(3,21,185,39),"Onderdeel",u_DlgBase),idct_onderdeel,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(3,11,161,21),"Dit toernooi kent geen onderdeel van de naam:",u_DlgBase),idct_de_ta_kent_geen_onderdeel_van_de_naam,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(5,47,183,127),"",u_DlgBase),idct_onderdeelmatch_1,[wsf_AlignLeft])
%END Onderdeelmatch, WinDefList
		],dlg_onderdeelmatch_eh,0),
		oselected(Selected).

%BEGIN Onderdeelmatch, idc_ok _CtlInfo
  dlg_onderdeelmatch_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-!,
  	LBoxWin = win_getCtlHandle(_Win, idc_onderdeellijst),
	lbox_GetSel(LboxWin,ItemList,_IndexList),
	ItemList = [Sel | _],
	assert(oselected(Sel)),
	win_Destroy(_Win),
	!.
%END Onderdeelmatch, idc_ok _CtlInfo
%MARK Onderdeelmatch, new events

%BEGIN Onderdeelmatch, e_Update
  dlg_onderdeelmatch_eh(_Win,e_Update(_UpdateRct),0):-!,
  	OWin = win_getCtlHandle(_Win, idct_onderdeel),
	Font = win_GetFont(_Win),
	_FontName = font_GetAttrs(Font, Style, Size),
	NewFont = font_SetAttrs(Font, [fs_Bold|Style], Size),
	win_SetFont(OWin, NewFont),
	!.
%END Onderdeelmatch, e_Update

%BEGIN Onderdeelmatch, e_CloseRequest
  dlg_onderdeelmatch_eh(_Win,e_CloseRequest,0):-
	assert(softstop),
	fail.
%END Onderdeelmatch, e_CloseRequest

%BEGIN Onderdeelmatch, idc_cancel _CtlInfo
  dlg_onderdeelmatch_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-
	win_Destroy(_Win),
	assert(softstop), !.
%END Onderdeelmatch, idc_cancel _CtlInfo

%BEGIN Onderdeelmatch, idc_onderdeellijst selchanged
  dlg_onderdeelmatch_eh(_Win,e_Control(idc_onderdeellijst,_CtrlType,_CtrlWin,selchanged),0):-!,
	OKWin = win_getCtlHandle(_Win, idc_ok),
	win_SetState(OKWin, [wsf_Enabled]), !.
%END Onderdeelmatch, idc_onderdeellijst selchanged

%BEGIN Onderdeelmatch, e_Create
  dlg_onderdeelmatch_eh(_Win,e_Create(_CreationData),0):-
  	LBoxWin = win_getCtlHandle(_Win, idc_onderdeellijst),
	waarden(OnderdeelIn, Candidaten),  	
	lbox_Add(LboxWin, Candidaten),
  	OWin = win_getCtlHandle(_Win, idct_onderdeel),
	win_SetText(OWin, OnderdeelIn),
	HWin = win_getCtlHandle(_Win, idct_onderdeelmatch_1),
	Tekst0 = "Deze onderdeelnaam komt niet overeen met een bestaand onderdeel.",
	Tekst1 = "Kies tussen\n- toch een bestaand onderdeel\n- een nieuw onderdeel aanmaken\n- stop de import.",
	Tekst2 = "NB Het kan een klein verschil zijn zoals een extra spatie!",
	format(T, "%s\n%s\n%s", Tekst0, Tekst1, Tekst2),
	win_SetText(HWin, T),
	syno(OnderdeelIn, Cat),
	wc(Cat, _, _, SelectedNaam, _, _, _, _, _, _, _, _, _, _, _),
	%wc(Cat, SelectedNaam, _,_,_,_,_),
	positie(0, Uit, SelectedNaam, Candidaten),
	lbox_SetSel(LboxWin, Uit, b_True),
	OKWin = win_getCtlHandle(_Win, idc_ok),
	win_SetState(OKWin, [wsf_Enabled]), !.
%END Onderdeelmatch, e_Create

  dlg_onderdeelmatch_eh(_,_,_):-!,fail.

%END_DLG Onderdeelmatch

  positie(Val, Val, SelectedNaam, [SelectedNaam | _]) :- !.
  positie(In, Uit, SelectedNaam, [_ | Rest]) :-
    In1 = In + 1,
    positie(In1, Uit, SelectedNaam, Rest).
  
    
