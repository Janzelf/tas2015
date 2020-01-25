/*****************************************************************************

		Copyright (c) 1989 - 1998 De Lint Associates

 Project:  TASVPI
 FileName: TASSPSEL.PRO
 Purpose: Selectie van personen
 Written by: JdL
 Comments:
******************************************************************************/

include "tasvp1.inc"
include "tasvp1.con"
include "hlptopic.con"

%BEGIN_DLG Spelersselect
/**************************************************************************
	Creation and event handling for dialog: Spelersselect
**************************************************************************/

constants

%BEGIN Spelersselect, CreateParms, 22:13:57-27.10.2011, Code automatically updated!
  dlg_spelersselect_ResID = idd_spelersselect
  dlg_spelersselect_DlgType = wd_Modal
  dlg_spelersselect_Help = idh_personen_checken
%END Spelersselect, CreateParms
%upd_toolb = 21

predicates

  dlg_spelersselect_eh : EHANDLER
  dlg_spelersselect_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST,SLIST Selectie)
  dlg_spelersselect_update(DIALOG_VAL_LIST, SLIST Selectie)
  %itemPos(string, integer, string)

clauses

  dlg_spelersselect_Create(_Parent, Title, [], []):- !,
	dlg_MessageBox( Title, "Lijst is leeg...",mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	fail.
  dlg_spelersselect_Create(Parent, Title, SPELERLIJST_ITEMLIST, Selectie):-
	%frontstr(3,Title,StartString,_RestString),
	%itemPos(StartString, CharPos,IDCT_AANSTREPEN_TITLE),
	%sortstring_c(SPELERLIJST_ITEMLIST, CharPos),
	SPELERLIJST_SELECT = [],
	TitleC = cast(long, Title),
%MARK Spelersselect, new variables

	dialog_CreateModal(Parent,dlg_spelersselect_ResID,"",
  		[
%BEGIN Spelersselect, ControlList, 22:13:57-27.10.2011, Code automatically updated!
		df(idc_spelerlijst,listbox(SPELERLIJST_ITEMLIST,SPELERLIJST_SELECT),nopr)
%END Spelersselect, ControlList
		],
		dlg_spelersselect_eh,TitleC,VALLIST,ANSWER),
	dlg_spelersselect_handle_answer(ANSWER,VALLIST, Selectie).

  dlg_spelersselect_handle_answer(idc_ok,VALLIST,Selectie):-!,
	dlg_spelersselect_update(VALLIST,Selectie).
  dlg_spelersselect_handle_answer(idc_cancel,_,[]):-!.  % Handle Esc and Cancel here
  dlg_spelersselect_handle_answer(_,_,_):-
	errorexit().

  dlg_spelersselect_update(_VALLIST,_SPELERLIJST_ITEMLIST):-
%BEGIN Spelersselect, Update controls, 22:13:57-27.10.2011, Code automatically updated!
	dialog_VLGetListBox(idc_spelerlijst,_VALLIST,_SPELERLIJST_ITEMLIST,_SPELERLIJST_SELECT),
%END Spelersselect, Update controls
	true.

%MARK Spelersselect, new events

%BEGIN Spelersselect, idc_spelerlijst activated
  dlg_spelersselect_eh(_Win,e_Control(idc_spelerlijst,_CtrlType,_CtrlWin,activated),0):-!,
	trap(CW = win_GetCtlHandle(_Win, idc_ok),_,fail),
	win_SendEvent(_Win,e_Control(idc_ok,wc_PushButton,CW, activated())),
	!.
%END Spelersselect, idc_spelerlijst activated

%BEGIN Spelersselect, idc_help _CtlInfo
  dlg_spelersselect_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Spelerselectie"),
	!.
%END Spelersselect, idc_help _CtlInfo

%BEGIN Spelersselect, e_Create
  dlg_spelersselect_eh(_Win,e_Create(_CreationData),0):-!,
        Text = cast(string, _CreationData),
	win_SetText(_Win, Text),
	LboxWin = win_GetCtlHandle(_Win, idc_spelerlijst),
	lbox_SetTabStops(LboxWin, [25, 1000]),	
	!.
%END Spelersselect, e_Create

  dlg_spelersselect_eh(_,_,_):-!,fail.

%END_DLG Spelersselect

predicates
  sumO(catl,real,real,string,string,catl)
  
clauses

  
%nietbetaald(nee) :- !.
%nietbetaald(omheteven).

contributie(SpNo, Som, String, CatL) :-
  findall(Cat,sp_catX(SpNo,Cat),CatGL),	% 9.6.2014
  not(CatGL = []),
  zeeffamilie(CatGL, [], Catten),
  sumO(Catten,0.0,Som,"",String, Catl).

sp_catX(SpNo,Cat) :-
  opg(_,Cat,s(SpNo),_,_,0,_,_,_,_,_).	% opgegeven en niet uitgesloten
sp_catX(SpNo,Cat) :-
  opg(_,Cat,pw(SpNo),_,_,0,_,_,_,_,_).
sp_catX(SpNo,Cat) :-
  opg(_,Cat,p(SpNo,_),_,_,0,_,_,_,_,_).
sp_catX(SpNo,Cat) :-
  opg(_,Cat,p(_,SpNo),_,_,0,_,_,_,_,_).

/*contributieX(SpNo, Som, String, CatL) :-
  findall(Cat,sp_catX(SpNo,Cat),CatGL),
  sumO(CatGL,0.0,Som,"",String, Catl).
*/
sumO([],Som,Som,String,String, _) :- !.
sumO([H|Tail],InSom,UitSom,InString,UitString,[]) :-
  wc(H, _, _, _Naam, _EDG, Abbr, _, _, _, _, _, _, _, _, Contr), !,
  %wc(H,_,_,Abbr,_,Contr,_), !,
  InSom1 = Contr + InSom,
  format(InString1, "%s %s", InString, Abbr),
  sumO(Tail,InSom1,UitSom,InString1,UitString,[]).
sumO([H|Tail],InSom,UitSom,InString,UitString, CatL) :-
  member(H, CatL),
  wc(H, _, _, _Naam, _EDG, Abbr, _, _, _, _, _, _, _, _, Contr), !,
  %wc(H,_,_,Abbr,_,Contr,_), !,
  InSom1 = Contr + InSom,
  format(InString1, "%s  %s", InString, Abbr),
  sumO(Tail,InSom1,UitSom,InString1,UitString,CatL).
sumO([_|Tail],InSom,UitSom,InString,UitString, CatL) :-
  sumO(Tail,InSom,UitSom,InString,UitString,CatL).

spelerBetaal(Win) :-
  loglock(lees, "spelerbetaal"),
  logclose(),
  win_betalingen_Create(Win), !.
spelerBetaal(_).

database - check
single aantal(integer)

predicates
nondeterm   lid_sp(string, integer Mode)
nondeterm  niet_sp(string, string NaamMetGeslacht)
  check_nos(integer, slist, slist, slist)
nondeterm spTeChecken(string, integer Mode)
  spTeChecken(janee, integer SpNo, integer Mode, string,janee,integer, string, string, string, string, string)
  spOK(string, string, string, string, string, string)
  eenSterkte(string, string)
determ ongeldig(string)
determ ookOpgegeven(integer No, integer Mode)


clauses

aantal(0).

spTeChecken(SpNaam1, Mode) :-
  sp2(No,Sex,SpNaam,Tel,_,_RE,_RD,_Betaald,UitKant, Club, _, _, BondsNr, ES, DS, Gebdatum, Twerk, Tmob, _, _,FormGezien,_RPosE,_RPosD,_,_,_,_),
  openbaar(Open),
  format(Telthuis, "%s%s%s", Tel, Twerk, Tmob),
  trim_c(Telthuis),
  spTeChecken(Open, No, Mode, Telthuis, FormGezien, UitKant, Club, BondsNr, ES, DS, Gebdatum),
  spsrt(Sex, _StrSel, MV, _, _, _, _, _), 
  mv2string(MV, MVS),
  format(SpNaam1, "%s\t%s", MVS, SpNaam).
  
%spTeChecken(nee,_,3,"",_,0, _, _, _, _,_) :- !.		% geen telefoon en uitkant
spTeChecken(ja,No,3,Tel,_,_, Club, LidNr, Sterkte, SterkD, Geb) :- 
  getbacktrack(Here),
  sp2opg(No,_,_,_,_,_),
  cutbacktrack(Here), 
  not(spOK(Tel, Club, LidNr, Sterkte, SterkD, Geb)), !. 
spTeChecken(_,No,4,_,nee,_,_,_,_,_,_):-
  sp2opg(No,_,_,_,_,_), !. 

spOK(Tel, _Club, LidNr, Sterkte, SterkD, Geb) :-
  openbaar(ja),
  not(Tel = ""),
  trim_c(LidNr),
  not(LidNr = ""),
  trim_c(Sterkte),
  trim_c(SterkD),
  eenSterkte(Sterkte, SterkD),
  trim_c(Geb),
  not(Geb = ""), !.
spOK(Tel, _Club, _LidNr, Sterkte, SterkD, Geb) :-
  openbaar(nee),
  not(Tel = ""),
  trim_c(Sterkte),
  trim_c(SterkD),
  eenSterkte(Sterkte, SterkD),
  trim_c(Geb),
  not(Geb = "").

eenSterkte(Enkel, _) :- 
  not(Enkel = ""), !.
eenSterkte(_, Dubbel) :-
  not(Dubbel = ""), !. 

ookOpgegeven(_No, 5) :- !. % alles
ookOpgegeven(_No, 7) :- !.
ookOpgegeven(No, _) :-
  sp2opg(No,_,_,_,_,_), !.
  
lid_sp(LidNo_SpNo, N) :-
  sp2(No,_,_,_,_ ,_,_,_,_,_,_, _,LidNr,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  ookOpgegeven(No, N),
  format(LidNo_SpNo, "%s\t%d", LidNr, No).

check_nos(7, [T1 | Rest], In, [Naam1 | Out]) :- 		% NIET GELDIG
  searchchar(T1,'\t',FoundPos),
  Pos = FoundPos - 1,
  Pos >= 0,
  frontstr(Pos, T1, LidNummer, _),
  ongeldig(LidNummer),
  frontstr(FoundPos, T1, _, N1),
  trap(term_str(ulong, Sp1, N1), _, fail),
  sp2(Sp1,_,Naam1,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !,
  check_nos(7, Rest, In, Out).
check_nos(6, [T1 | Rest], In, [Naam1 | Out]) :-			% ontbrekend
  searchchar(T1,'\t',FoundPos),
  frontstr(FoundPos, T1, LidNummer, N1),
  not(fronttoken(LidNummer,_,_)), 
  str_int(N1, Sp1),
  sp2(Sp1,_,Naam1,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !,
  check_nos(6, Rest, In, Out).
check_nos(5, [T1, T2 | Rest], In, [Naam1,Naam2 | Out]) :-	% dubbel
  searchchar(T1,'\t',FP1),
  %FP1a = FP1 - 1,
  searchchar(T2,'\t',FP2),
  %FP2a = FP2 - 1,
  frontstr(FP1, T1, L1, N1),
  frontstr(FP2, T2, L2, N2),
  fronttoken(L1,L1a,_),
  fronttoken(L2,L2a,_),
  L1a = L2a, 
  str_int(N1, Sp1),
  str_int(N2, Sp2),
  sp2(Sp1,_,Naam1,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  sp2(Sp2,_,Naam2,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !,
  check_nos(5, Rest, In, Out).
check_nos(Keus, [_ | List], In, Out) :- !,
  check_nos(Keus, List, In, Out).
check_nos(_, _, X, X).

ongeldig("00000000") :- !. 
ongeldig("") :- !. 
ongeldig(LN) :-
  not(proef11det(LN)). 

/*niet_spelers(Parent, 2) :-		% 2 = verwijderen
  findall(SpNaam, niet_sp(SpNaam, _), Spelers),
  count(Spelers, 0, Aantal),
  format(Msg, "% niet-spelers.\n(Kan even duren)", Aantal),
  1 = dlg_MessageBox( "Verwijderen?", Msg, mesbox_iconquestion, mesbox_buttonsokcancel, mesbox_defaultsecond, mesbox_suspendapplication ),
  assert(aantal(Aantal)),
  cursor_Set( Parent, cursor_Wait),
  member(Naamx, Spelers),
  sp2(SKeus,Sex,Naamx,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
  SpelerIn = sp2(SKeus,Sex,Naamx,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
  logf('R',SpelerIn,ja), 
  fail.
niet_spelers(Parent, 2) :-
  cursor_Set(Parent, cursor_Arrow),
  logclose(), 
  %time(Hours,Minutes,_Seconds,_Hundredths),
  %helestr_int(0, MS, Minutes),
  aantal(N),
  format(Msg, " % niet-spelers uit bestand verwijderd.", N),
  %dlg_MessageBox( "Opruimen", Msg, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  writeToJournaal(Msg, ja), !. */

niet_sp(Naam, SpNaam) :-
  sp2(Sp,Sex,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  %sp1(Sp,Sex,Naam,_,_,_,_,_, _, _, _, _, _, _, _, _, _, _, _), 
  not(sp2opg(Sp,_,_,_,_,_)),
  spsrt(Sex, _StrSel, MV, _, _, _, _, _), 
  mv2string(MV, MVS),
  format(SpNaam, "%s\t%s", MVS, Naam).
  % not(wedstrijd(_,_,Sp,_)).	% 24.7.2000

%_______________________________________________________________________
spelers_uit():-
  TaskWin = vpi_GetTaskWin(),
  dlg_spelergegevensuitvoer_Create(TaskWin, [], eerste).

%BEGIN_DLG Spelergegevensuitvoer
/**************************************************************************
	Creation and event handling for dialog: Spelergegevensuitvoer
**************************************************************************/

constants

%BEGIN Spelergegevensuitvoer, CreateParms, 11:41:29-23.11.2016, Code automatically updated!
  dlg_spelergegevensuitvoer_DlgType = wd_Modal
  dlg_spelergegevensuitvoer_Title = "Maak lijst van spelers"
  dlg_spelergegevensuitvoer_RCT = rct(40,32,477,239)
  dlg_spelergegevensuitvoer_Flags = [wsf_Close,wsf_TitleBar]
  dlg_spelergegevensuitvoer_Help = idh_spelerselectie
  dlg_spelergegevensuitvoer_Font = "Arial"
  dlg_spelergegevensuitvoer_FSize = 9
%END Spelergegevensuitvoer, CreateParms

database - uit
%cl_sp(integer)
recno(integer, sex, integer)
single recno1(integer)
single planIndex(integer)
%single planAantal(integer)
single planGepland(integer)
single planTeplan(integer)
single planSelect(integer)
single planRect(RCT)
 preselC(catl, selectieniveau, window)
 namenSelectieKrit(selectieniveau, slist Onderdelen, slist Soorten, integer HeeftNietOpg, 
		integer Betalen, integer LftVan, integer LftTot, slist Clubs, slist Sterktes, slist StDbl,
		boolean Missend, boolean Opmerkingen, string OpmFilter, boolean NogIn,boolean UitL, boolean Zonderemail, 
		tijdl DagenList, ilist UrenL, slist Lokaties, integerlist Spelers, boolean Inc, boolean GeenEigenForm, boolean AlGemeld)
%determ selectieWin(window)
  spWeds(integer)
single eersteSelectie(ilist).
determ eerstehint  

predicates

  dlg_spelergegevensuitvoer_eh : EHANDLER
  %sp_out(BOOLEAN Komma, BOOLEAN Kol, BOOLEAN XML, slist Namen)
  sp_out1(BOOLEAN, BOOLEAN, BOOLEAN, integer, sex, string, string, dbasedom) 
%  init_recno
  inc_recno(integer, sex)
nondeterm  spu_result(string)
  som(integerlist, integer, integer)
  uitvoerbestand(BOOLEAN, BOOLEAN, BOOLEAN, string Bestandsnaam, string Titel)
verwerk(integer Mode, slist NaamList, BOOLEAN Kom, BOOLEAN Kol, BOOLEAN XML) % 0 = bestand 1 = etiketten 2 = mailmerge
determ getSelKrit(selectieniveau, slist OndList, slist SrtList, boolean HeeftNO, boolean Betalen, integer LftVan, integer LftTot,
  slist VerList, slist SterktesE, slist SterktesD, boolean Missend, boolean Opmerkingen, string OpmTekst, boolean NogIn, boolean Uitloot, 
  boolean ZonderEmail, tijdl Dagen, ilist Uren, slist Lokaties, integerlist Spelers, boolean IncassoOKSpelers, boolean GeenEigenForm, boolean AlGemeld)
determ updateStatus(window Win)
procedure updateStatus1(window Win, integer Aantal, selectieniveau)
procedure preselectC(window RubWin, slist, integer I, catl CatPresel)
nondeterm speeldag(integer, string)
procedure dagenL2DagNoL(slist, tijdl)
determ sp_in_groep1( integer SpNum, sex Sex,slist Onderdelen, slist Soorten, integer NietOpg, integer Betalen, integer Van, integer Tot, string GebDat, string Club, slist Clubs,
	 string Sterkte, slist Sterktes, string SterkD, slist SterktesD, boolean Missend, boolean Opmerkingen, string OpmTekst, 
	 boolean NogIn, boolean UitL, boolean ZonderEmail, tijdl DagenList, ilist UrenL, slist Lokaties, integerlist Spelers, boolean IncasOK, boolean GeenEigenFormulier,boolean AlGemeld)
procedure tweedeNiveauDisable(selectieniveau, window)
nondeterm adres_namen(string Naam, integer SpNo)
nondeterm adres_namenX(integer SpNum, ilist)
determ sp_in_groep(integer SpNo, sex, string Club, string St, string StD, string Geb) - (i, i, i, i, i, i)
nondeterm naamVanLijst1(string Naam) - (o)
determ cacheSpWeds(tijdl DagenList, ilist Uren, slist Lokaties)
determ cacheSpWeds1(integer Dag, tijdl DagenList, string Lok , slist Lokaties)
procedure peildatum(window, integer)
nondeterm tijdstringen2tijd(slist, integer)
procedure  selecttelefoon(string Tel1, string Tel2, string Tel3, string Teluit)
nondeterm selecttelefoon1(string, string, string, string)

clauses

recno1(0).
planIndex(0).
planGepland(0).
planTeplan(0).
planSelect(1).
planRect(rct(0,0,0,0)).
eersteselectie([]).

som([], I, I) :- !.
som([H|T], I, O) :-
  I1 = H + I,
  som(T, I1, O).

init_recno() :-
  retractall(recno(_,_,_), uit),
  spsrt(N, _, _, _, _, _, _, _),
  assert(recno(1, N, 0)),
  assert(recno(2, N, 0)),
  assert(recno(3, N, 0)),
  fail.
init_recno.

inc_recno(Type, Cat) :-
  retract(recno(Type, Cat, No)), !,
  N1 = No + 1,
  asserta(recno(Type, Cat, N1)).

wcMetSpelers(Rubriek) :-
  wc(Cat, _, _, Rubriek, _EDG, _Abbr, _, _, _, _, _, _, _, _, _Contr),
  %wc(Cat,Rubriek,_,_,_,_,_),
  getbacktrack(Here),
  opg(_,Cat,_,_,_,_,_,_,_,_,_),
  cutbacktrack(Here).

str_intVanTot(V, VI, T, TI) :-
  str_int(V, VI),
  str_int(T, TI),  !.
str_intVanTot(V, VI, _T, 99) :-
  str_int(V, VI), !.
str_intVanTot(_V, 0, T, TI) :-
  str_int(T, TI),  !.
str_intVanTot(_, 2, _, 1).		% onmogelijke combinatie

sp_out(_Komma, _Kol, b_True, _NaamList) :-
  write("<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>\n"),
  write("<PERSONEN>"),
  fail.
sp_out(Komma, Kol, XML, NaamList) :-
  %count(Naamlist, 0, Len),
  assert(recno1(0)),
  member(PNaam, NaamList),
  sp2(SKeus,Sex,PNaam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
  SpelerIn = sp2(SKeus,Sex,PNaam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
  recno1(Cur),
  Cur1 = Cur + 1,
  assert(recno1(Cur1)),
  inc_recno(1, Sex),
  sp_out1(Komma, Kol, XML, SKeus, Sex, PNaam, Telthuis, SpelerIn),
  fail.
sp_out(_Komma, _Kol, b_True, _NaamList) :-
  write("</PERSONEN>"), !.
sp_out(_, _, _, _).

sp_out1(_, _, _, _, _Sex, Naam, _, _) :-
  trim_c(Naam),
  Naam = "", !. 
sp_out1(b_True, _, _, _, _, _, _, SpelerGeg) :-
  SpelerGeg = sp2(_,Sex,Naam,Tel,_,_RE,_RD,_,_, Club, Adres, Woonpl, LidNr, StE, StD, Geb, _Telw, Telm, EMail, _Opm,_Gez,_RangE,_RangD,_,_,_,_),
  spsrt(Sex, _, M_V,_,_,_,_,_),
  writedevice(work),
  write(Sex, csvconst, "\"", M_V, "\"", csvconst, "\"",
        Naam,"\"",csvconst,"\"", Adres, "\"",csvconst,"\"", Woonpl, 
        "\"",csvconst,"\"", Tel, "\"",csvconst,"\"",Lidnr, "\"",csvconst,"\"",Club, "\"",csvconst,"\"",
        Ste, "\"",csvconst,"\"",Std, "\"",csvconst,"\"",Geb, "\"",csvconst,"\"",Email, "\"",csvconst,"\"", Telm, "\"\n"), !.
sp_out1(b_False, b_True, _, _Nr, _, _, _, SpelerGeg) :- 
  SpelerGeg = sp2(_,Sex,Naam,Tel,_,_RE,_RD,_,_, Club, Adres, Woonpl, LidNr, StE, StD, Geb, _Telw, _Telm, _EMail, _Opm,_Gez,_RangE,_RangD,_,_,_,_),
  spsrt(Sex, _, M_V, _, _, _, _, _),
  writedevice(work),
  writef("%-2d", Sex), 
  write(M_V), 
  writef("%-29s", Naam), 
  writef("%-29s%-29s%-29s%8s%5s%-2.2s%-2.2s%8s\n", 
             Adres, Woonpl, Tel, Lidnr, Club, Ste, Std, Geb), !.
sp_out1(b_False, b_False, b_True, Nr, _Sex, _Naam, _Tel, _Offset) :- 
  expSpeler("", "SPELER", Nr, 0).

spu_result(Str) :-
  Str = "Uitgevoerd:\n Spelersoort                     Aantal".
spu_result(Str) :-
  spsrt(No, Naam, _, _, _, _, _, _),
  recno(1, No, N1),
  N1 > 0,
  format(Str, " %-30.30    %4", Naam, N1).
spu_result(Str) :-
  Str = "                       Totaal    ------".
spu_result(Str) :-
  findall(N1, recno(1, _, N1), N1L), som(N1L, 0, S1),
  format(Str, " %-30    %4", "", S1).

uitvoerbestand(b_True, _, _, "Spelers.EXP", "Kommagescheiden") :- !. 
uitvoerbestand(_, b_True, _, "Spelers.EXP", "Vaste kolomformaat") :- !. 
uitvoerbestand(_, _, b_True, "Spelers.xml", "XML bestand").

verwerk(0, NaamList, Kom, Kol, XML) :- % 0 = bestand 1 = etiketten 2 = mailmerge
  %get_Toernooidir(TDir, _, _),
  getFolder(TDir,_,_,_),
  uitvoerbestand(Kom, Kol, XML, UBestand, Titel),
  filenamepath(Uitvoer,TDir,UBestand), 
  %concat(TDir, UBestand, Uitvoer),
  Fname = dlg_GetFileName(Uitvoer,["exp","*.exp","txt","*.txt","xml","*.xml"],Titel,[dlgfn_Save],"",_OutListFiles),
  filenamepath(Fname, _Path, Name),
  Name <> fctconst,
  init_recno(),
  %OndWin = win_GetCtlHandle(_Win, idcu_lbclub),
  closefile(work),
  openwrite(work, Fname),
  writedevice(work),
  sp_out(Kom, Kol, XML, NaamList),
  closefile(work),
  findall(Sx, spu_result(Sx), SxList),
  list_to_string(SxList, "\n", Msg),
  writeToJournaal(Msg, ja),
  closefile(work), !.
verwerk(Mode, NaamList, _Kom, _Kol, _XML) :- % 0 = bestand 1 = etiketten 2 = mailmerge
  etikettenAdres1(NaamList,Mode), !.

nogIn(NogIn) :-   % alléén voor Tasprnt
  getSelKrit(eerste,_,_,_,_,_, _,_,_,_,_,_,_,NogIn,_,_,_,_,_,_, _,_,_).

tijdstringen2tijd(Stringen, Tijd) :-
  member(String, Stringen),
  tijdstr2tijd(String, Tijd).

getSelKrit(Niveau,OndList,SrtList,HeeftNO, Betalen,LftVan,LftTot,VerList,SterktesE,SterktesD, Missend,Opmerkingen,OpmTekst, NogIn,UitL,ZonderEmail, DagenList, UrenList, Lokaties, Spelers, IncOK, GEF,NNM) :-
  not(preselC(_, Niveau, _)),	% er is geen window van dat niveau
  namenSelectieKrit(Niveau, OndList, SrtList, HeeftNO, Betalen, LftVan, LftTot, VerList, SterktesE, SterktesD,
		Missend, Opmerkingen, OpmTekst, NogIn,UitL, ZonderEmail, DagenList, UrenList, Lokaties, Spelers, IncOK, GEF,NNM), !.
getSelKrit(Niveau, OndList,SrtList,HeeftNO, Betalen,LftVan,LftTot,VerList,SterktesE,SterktesD,Missend,Opmerkingen,OpmTekst,NogIn,UitL,ZonderEmail, DagenList, UrenList, Lokaties, [],IncOK, GEF, NNM) :-
  preselC(_, Niveau, _Win), !,
  LftVanWin = win_GetCtlHandle(_Win, idcu_van),
  LftVanS = win_GetText(LftVanWin),
  LftTotWin = win_GetCtlHandle(_Win, idcu_totmet),
  LftTotS = win_GetText(LftTotWin),
  str_intVanTot(LftVanS, LftVan, LfTTotS, LftTot),
  %write("\nVanS: ", LftVanS, " totS: ", LftTotS, " vanU:", LftVan, " totU:", LftTot), 
  OndWin = win_GetCtlHandle(_Win, idcu_lbond),
  lbox_GetSel(OndWin,OndList,_),
  VerWin = win_GetCtlHandle(_Win, idcu_lbclub),
  lbox_GetSel(VerWin,VerList,_),
  SrtWin = win_GetCtlHandle(_Win, idcu_lbspsrt),
  lbox_GetSel(SrtWin,SrtList,_),
  NOWin = win_GetCtlHandle(_Win, idcu_niet_opgegeven_hebben),
  HeeftNO = win_IsChecked(NOWin),
  USWin = win_GetCtlHandle(_Win, idcu_uitgeloot_zijn),
  UitL = win_IsChecked(USWin),
  StWin = win_GetCtlHandle(_Win, idcu_lbspsterkte),
  lbox_getSel(StWin, SterktesE, _),
  StDWin = win_GetCtlHandle(_Win, idcu_spstdubbel),
  lbox_getSel(StDWin, SterktesD, _),
  BetWin = win_GetCtlHandle(_Win, idc_betalen),
  Betalen = win_IsChecked(BetWin),
  MisWin = win_GetCtlHandle(_Win, idcu_gegevens_missen),
  Missend = win_IsChecked(MisWin),
  OpmWin = win_GetCtlHandle(_Win, idcu_met_opmerkingen),
  Opmerkingen = win_IsChecked(OpmWin),
  OpmTWin = win_GetCtlHandle(_Win, idcu_opmtekst),
  OpmTekst = win_GetText(OpmTWin),
  NinWin = win_GetCtlHandle(_Win, idcu_nog_ergens_in_zitten),
  NogIn = win_IsChecked(NinWin),
  IncWin = win_GetCtlHandle(_Win, idcu_incasso),
  IncOK = win_IsChecked(IncWin),
  EmWin = win_GetCtlHandle(_Win, idcu_zonderemail),
  ZonderEmail = win_IsChecked(EmWin),
  DagWin = win_GetCtlHandle(_Win, speeldagen),
  lbox_GetSel(DagWin,DItemList,_IndexList),
  dagenL2DagNoL(DItemlist, DagenList),
  UurWin = win_GetCtlHandle(_Win,idcn_lbuur),
  lbox_GetSel(UurWin,UrenSList,_),
  findall(Tx, tijdstringen2tijd(UrenSList, Tx), UrenList),
  LokWin = win_GetCtlHandle(_Win, idcu_lblokaties),
  lbox_GetSel(LokWin, Lokaties, _),
  GEFWin = win_GetCtlHandle(_Win, idcu_geen_eigen),	% geen eigen formulier
  GEF = win_IsChecked(GEFWin),
  NNMWin = win_GetCtlHandle(_Win, idcu_zich_nog),	% nog niet gemeld
  NNM = win_IsChecked(NNMWin).

dagenL2DagNoL([Item|DItemlist], [Dag|DagenList]) :-
  speeldag(Dag, Item1),
  Item1 = Item, !,
  dagenL2DagNoL(DItemlist, DagenList).
dagenL2DagNoL(_, []).

naamVanLijst1(NaamR) :-
  findall(Naam, adres_namen(Naam, _),Namen),
  sortstring_c(Namen, 1),
  member(NaamR, Namen).

adres_namen(SpelerPlus, SpNum) :-
  readdevice(keyboard),
  %retractall(spWeds(_)),	%!!! ######################################3
  %assert(current(0)),
  sp2(SpNum,Sex,Naam,Tel1,_,_,_Btl,_,_,Club, _Adres, _Woonpl, _LidNr, St, SterkD, Geb, Tel2, Tel3, _EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Import),
  selecttelefoon(Tel1,Tel2,Tel3,Tel),
  %current(Cur),
  %Cur1 = Cur + 1,
  %assert(current(Cur1)),
  sp_in_groep(SpNum, Sex, Club, St, SterkD,Geb),
  spsrt(Sex, _, MV, _, _, _, _, _),
  mv2string(MV, Str),
  format(SpelerPlus, "%s\t%s\t%s", Str, Naam, Tel).

adres_namenX(SpNum, SpelerSelectie) :-
  member(SpNum, SpelerSelectie),
  readdevice(keyboard),
  %retractall(spWeds(_)),	%!!! ######################################3
  %assert(current(0)),
  sp2(SpNum,Sex,_Naam,_,_,_,_Btl,_,_,Club, _Adres, _Woonpl, _LidNr, St, SterkD, Geb, _Telwerk, _Telmobiel, _EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Import),
  %current(Cur),
  %Cur1 = Cur + 1,
  %assert(current(Cur1)),
  sp_in_groep(SpNum, Sex, Club, St, SterkD, Geb).

sp_in_groep(Sp, Sex, Club, St, SterkD, Geb) :-
  getSelKrit(tweede, OndList2,SrtList2,HeeftNO2, Betalen2,LftVan2,LftTot2, VerList2, Sterktes2,SterktesD2,Missend2,Opmerkingen2,OpmTekst2,NogIn2,UitL2,ZonderEmail2,DagenL2,UrenL2,Lokaties2,Spelers2,Incas2,GEF1,NNM1),
  getSelKrit(eerste, OndList1,SrtList1,HeeftNO1, Betalen1,LftVan1,LftTot1, VerList1, Sterktes1,SterktesD1,Missend1,Opmerkingen1,OpmTekst1,NogIn1,UitL1,ZonderEmail1,DagenL1,UrenL1,Lokaties1,Spelers1,Incas1,GEF2,NNM2),
  sp_in_groep1(Sp, Sex, OndList1,SrtList1,HeeftNO1, Betalen1,LftVan1,LftTot1,Geb, Club, VerList1,St, Sterktes1,SterkD,SterktesD1,Missend1,Opmerkingen1,OpmTekst1,NogIn1,UitL1,ZonderEmail1,DagenL1,UrenL1,Lokaties1,Spelers1,Incas1,GEF1,NNM1),
  sp_in_groep1(Sp, Sex, OndList2,SrtList2,HeeftNO2, Betalen2,LftVan2,LftTot2,Geb, Club, VerList2,St, Sterktes2,SterkD,SterktesD2,Missend2,Opmerkingen2,OpmTekst2,NogIn2,UitL2,ZonderEmail2,DagenL2,UrenL2,Lokaties2,Spelers2,Incas2,GEF2,NNM2), !.
sp_in_groep(Sp, Sex, Club, St, StD, Geb) :-
  not(getSelKrit(tweede, _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)),
  getSelKrit(eerste, OndList1,SrtList1,HeeftNO1, Betalen1,LftVan1,LftTot1, VerList1, Sterktes1,SterktesD1,Missend1,Opmerkingen1,OpmTekst1,NogIn1,UitL1,ZonderEmail1,DagenL1,UrenL1,Lokaties1,Spelers1,IncOK,GEF,NNM),
  sp_in_groep1(Sp, Sex, OndList1,SrtList1,HeeftNO1, Betalen1,LftVan1,LftTot1,Geb, Club, VerList1,St, Sterktes1,StD, SterktesD1,Missend1,Opmerkingen1,OpmTekst1,NogIn1,UitL1,ZonderEmail1,DagenL1,UrenL1,Lokaties1,Spelers1,IncOK,GEF,NNM), !.

sp_in_groep1( _, Sex,_Onderdelen,Soorten,_,_Betalen,_LftVan,_LftTot,_, _Club, _Clubs, _St, _StL, _StD, _StDL,_, _, _, _, _, _, _, _, _, _, _, _, _) :-		% spelersoort
  member(Groep,Soorten),
  spsrt(Sex,Groep,_,_,_,_,_,_), !.
sp_in_groep1( _, _Sex,_Onderdelen,_Soorten,_,_Betalen,_LftVan,_LftTot,_, _Club, _Clubs, St, StL, _StD, _StDL, _, _, _, _, _, _, _, _, _, _, _, _, _) :-		% speelsterkte
  %speelsterkteOudNieuw(St, StN),
  member(St,StL), !.
sp_in_groep1( _, _Sex,_Onderdelen,_Soorten,_,_Betalen,_LftVan,_LftTot,_, _Club, _Clubs, _St, _StL, StD, StDL, _, _, _, _, _, _, _, _, _, _, _, _, _) :-		% speelsterkte dubbel
  %speelsterkteOudNieuw(St, StN),
  member(StD,StDL), !.
sp_in_groep1(SpNum, _,Onderdelen,_Soorten,_NietOpg,_Betalen,_LftVan,_LftTot,_, _, _, _St, _StL, _StD, _StDL, _, _, _, _, _, _, _, _, _, _, _, _, _) :-		% in bepaalde rubriek
  member(Rubriek, Onderdelen),
  wc(Wc, _, _, Rubriek, _EDG, _Abbr, _, _, _, _, _, _, _, _, _Contr),
  %wc(Wc,Rubriek,_,_,_,_,_),
  sp2opg(SpNum,Wc,_,_,_,_), !.
sp_in_groep1(_SpNum, _,_Onderdelen,_Soorten,_,_,_LftVan,_LftTot,_, Club, Clubs, _St, _StL, _StD, _StDL, _, _, _, _, _, _, _, _, _, _, _, _, _) :-
  not( Clubs = []),
  club(Club, ClNaam1, _),
  member(ClNaam1, Clubs), !.
sp_in_groep1(SpNum, _,_Onderdelen,_Soorten, 1,_Betalen,_LftVan,_LftTot,_, _, _, _St, _StL, _StD, _StDL, _, _, _, _, _, _, _, _, _, _, _, _, _) :-		% niet sp
  not(sp2opg(SpNum,_,_,_,_,_)), !.
sp_in_groep1(SpNum, _,_Onderdelen,_Soorten,_,1,_LftVan,_LftTot,_, _Club, _Clubs, _St, _StL, _StD, _StDL,  _, _, _, _, _, _, _, _, _,_,_,_,_) :-	% niet betaald
  sp2(SpNum,_Sex,_Naam,_,_,_,_,nee,_,_Club, _Adres, _Woonpl, _LidNr, _St, _SterkD, _Geb, _Telwerk, _Telmobiel, _EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Import),
  %findall(Cat,sp_cat(SpNum,Cat),Catten),
  %not(Catten = []),
  %sum(Catten,0.0,Som,"",_),
  %format(Msg, "%6.2f %u %s",Som, SpNum, _Naam),
  %dlg_note(Msg),
  contributie(SpNum, Som, _, []),
  Som > 0,
  !. 
sp_in_groep1(_SpNum, _,_Onderdelen,_Soorten,_NietOpg,_,LftVan,LftTot,Geb, _, _, _St, _StL, _StD, _StDL, _, _, _, _, _, _, _, _, _,_,_,_,_) :-		% leeftijd
  leeftijd(Geb, Leeftijd,0, _),
  Leeftijd >= LftVan,
  Leeftijd <= LftTot,
  LftVan + LftTot > 0,
  %dlg_note("leeftijd"), 
   !. 
  %write("\ngeb: ", Geb, " Leeftijd: ", Leeftijd, " leeftijdVan: ", LftVan, " leeftijdTotenMet: ", LftTot).
sp_in_groep1(SpNo, _,_Onderdelen,_Soorten,_Nietopg,_,_LftVan,_LftTot,_Geb, _, _, _St, _StL, _StD, _StDL, b_True, _, _, _, _, _, _, _, _, _, _, _, _) :- % missend
  sp2(SpNo,_Sex,_Naam,Telthuis,[],_,_Btl,_,_,_Club, _Adres, _Woonpl, _LidNr, _S, _SterkD, _G, Telwerk, Telmobiel, _EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Import),
  format(Telle, "%s%s%s", Telthuis, Telwerk, Telmobiel),
  not(fronttoken(Telle, _, _)),
  %dlg_note("missend"), 
   !.
sp_in_groep1(SpNo, _,_Onderdelen,_Soorten,_,_,_LftVan,_LftTot,_Geb, _, _, _St, _StL, _StD, _StDL, b_True, _, _, _, _, _, _, _, _, _, _, _, _) :- % missend
  sp2(SpNo,_Sex,_Naam,_,_,_,_,_,_,_Club, _Adres, _Woonpl, _LidNr, _Stx, _SterkD, _G, _Telwerk, _Telmobiel, _EMail, _Opm,nee,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Import), 
  %dlg_note("Missend2"), 
  !.
sp_in_groep1(SpNo, _,_Onderdelen,_Soorten,_NietOpg,_,_LftVan,_LftTot,_Geb, _, _, _St, _StL, _StD, _StDL, _, b_True, "", _, _,_,_,_,_,_,_,_,_) :- % opm any
  sp2(SpNo,_Sex,_,_,_,_,_Btl,_,_,_Club, _Adres, _Woonpl, _LidNr, _St, _SterkD, _Geb, _Telwerk, _Telmobiel, _EMail, Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Import),
  OpmZ = opmZuiver(Opm),
  %trim_c(Opm),
  not(OpmZ = ""),
  %dlg_note("Opm any"), 
   !.   
sp_in_groep1(SpNo, _,_Onderdelen,_Soorten,_NietOpg,_,_LftVan,_LftTot,_Geb, _, _, _St, _StL, _StD, _StDL, _, b_True, Tekst, _, _, _, _, _, _, _,  _, _, _) :- % opm met tekst
  fronttoken(Tekst, _, _),
  upper_lower(Tekst, TekstL),
  sp2(SpNo,_Sex,_,_,_,_,_Btl,_,_,_Club, _Adres, _Woonpl, _LidNr, _St, _SterkD, _Geb, _Telwerk, _Telmobiel, _EMail, Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Import),
  upper_lower(Opm, OpmL),
  searchstring(OpmL,TekstL,_FoundPos),
  %dlg_note("opm tekst"), 
   !.
sp_in_groep1(SpNo, _,_Onderdelen,_Soorten,_,_,_LftVan,_LftTot,_Geb, _, _, _St, _StL, _StD, _StDL, _, _, _, b_True, _, _, _, _, _, _, _, _, _) :- % nog ergens in
  wedstrijd(No,WedsCat,SpNo,_,_,Andere),
  not(w3(No,WedsCat,_,_,_,_,_)),
  not(Andere = bye),
  %dlg_note("nog ergens in"), 
   !.
sp_in_groep1(SpNo, _,_Onderdelen,_Soorten,_NietOpg,_,_LftVan,_LftTot,_Geb, _, _, _St, _StL, _StD, _StDL, _, _, _, _, b_True, _, _, _, _, _, _, _, _) :- % uitgeloot
  sp2opg(SpNo,Cat,_Opg,_MaatNo,_,Tgnst),
  not(in_weds(Cat, Tgnst)),
  %dlg_note("uitgeloot"), 
   !.
sp_in_groep1(SpNo, _,_Onderdelen,_Soorten,_NietOpg,_,_LftVan,_LftTot,_Geb, _, _, _St, _StL, _StD,_StDL, _, _, _, _, _, b_True, _, _, _, _, _, _, _) :- % zonderEmail
  sp2(SpNo,_Sex,_Naam,_,_,_,_,_,_,_Club, _Adres, _Woonpl, _LidNr, _Stx, _SterkD, _G, _Telwerk, _Telmobiel, EMail, _Opm,_,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Import),
  not(searchchar(Email, '@', _)), 
  %dlg_note("zonderEmail"), 
  !.
sp_in_groep1(SpNo, _,_Onderdelen,_Soorten,_NietOpg,_,_LftVan,_LftTot,_Geb, _, _, _St, _StL,_StD, _StDL, _, _, _, _, _, _, DagenList, UrenList, Lokaties, _, _, _, _) :- % gepland op
  cacheSpWeds(DagenList, UrenList, Lokaties),
  spWeds(SpNo), 
  %dlg_note("gepland op"), 
  !.
sp_in_groep1(SpNo, _,_Onderdelen,_Soorten,_NietOpg,_,_LftVan,_LftTot,_Geb, _, _, _St, _StL, _StD, _StDL, _, _, _, _, _, _, _DagenList, _UrenL, _Lokaties, Spelers, _, _, _) :- % gepland op
  member(SpNo, Spelers),
  %dlg_note("gepland op 2"), 
   !.
sp_in_groep1(SpNo, _,_Onderdelen,_Soorten,_NietOpg,_,_LftVan,_LftTot,_Geb, _, _, _St, _StL, _StD,_StDL, _, _, _, _, _, _, _, _, _, _, b_true,_,_) :- % met Incasso OK
  sp2(SpNo,_Sex,_Naam,_,_,_,_,_,_,_Club, _Adres, _Woonpl, _LidNr, _Stx, _SterkD, _G, _Telwerk, _Telmobiel, _EMail, _Opm,_,_RangPosE,_RangPosD,_Incasso,_CheckGeg,Akkoord,_Import),
  fronttoken(Akkoord, "akkoord",_),
  %dlg_note("incasso OK"), 
   !.
sp_in_groep1(SpNo, _,_Onderdelen,_Soorten,_NietOpg,_,_LftVan,_LftTot,_Geb, _, _, _St, _StL, _StD,_StDL, _, _, _, _, _, _, _, _, _, _, _,b_true,_) :- % geen eigen formulier
  sp2(SpNo,_Sex,_Naam,_,_,_,_,_,_,_Club, _Adres, _Woonpl, _LidNr, _Stx, _SterkD, _G, _Telwerk, _Telmobiel, _EMail, _Opm,_,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_,[]),
  sp2opg(SpNo,_WC,_,_,_,Tgnst),   % sp2opg(SpNo,Cat,OpgNo,MaatNo,Plts,p(SpNo,MaatNo)) :-	/*      en dubbelspel            */
  opg(_,_Cat,Tgnst,_,_,_,_,_,_,Teamstatus,_),
  not(Teamstatus = geaccepteerd),
  not(Teamstatus = nietgeaccepteerd),
  %dlg_note("geen eigen form"), 
   !.
sp_in_groep1(SpNo, _,_Onderdelen,_Soorten,_NietOpg,_,_LftVan,_LftTot,_Geb, _, _, _St, _StL, _StD,_StDL, _, _, _, _, _, _, _, _, _, _,_,_,b_True) :- % nog niet gemeld
  sp2(SpNo,_Sex,_Naam,_,_,_,_,_,_,_Club, _Adres, _Woonpl, _LidNr, _Stx, _SterkD, _G, _Telwerk, _Telmobiel, _EMail, _Opm,nee,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Akkoord,_Import), 
  %dlg_note("nog niet gemeld"),
  !.

cacheSpWeds([], _, []) :- !, fail.
cacheSpWeds(_DagenList, _, _Lokaties) :-
  spWeds(_), !.
cacheSpWeds(DagenList, [], Lokaties) :-
  w2(No, WedsCat, Tijd,_,_,_,_,Lok),
  bitright(Tijd, 7, Dag),
  cacheSpWeds1(Dag, DagenList, Lok, Lokaties),
  wd(No, WedsCat, Tg1, Tg2, _, _, _),
  de_spelers(No, Wedscat, Tg1, Tg2, Spelers),
  member(Speler, Spelers),
  not(spWeds(Speler)),
  assert(spWeds(Speler)),
  fail.
cacheSpWeds(DagenList, UrenList, []) :-
  not(UrenList = []),
  w2(No, WedsCat, Tijd,_,_,_,_,_),
  member(Dag, DagenList),
  member(Uur, Urenlist),
  bitleft(Dag, 7, DagTijd),
  Tijd = DagTijd + Uur,
  %cacheSpWeds1(Dag, DagenList, Lok, Lokaties),
  wd(No, WedsCat, Tg1, Tg2, _, _, _),
  de_spelers(No, Wedscat, Tg1, Tg2, Spelers),
  member(Speler, Spelers),
  not(spWeds(Speler)),
  assert(spWeds(Speler)),
  fail.
cacheSpWeds(DagenList, UrenList, Lokaties) :-
  not(UrenList = []),
  not(Lokaties = []),
  w2(No, WedsCat, Tijd,_,_,_,_,Lok),
  member(Dag, DagenList),
  member(Uur, Urenlist),
  member(Lok, Lokaties),
  bitleft(Dag, 7, DagTijd),
  Tijd = DagTijd + Uur,
  %cacheSpWeds1(Dag, DagenList, Lok, Lokaties),
  wd(No, WedsCat, Tg1, Tg2, _, _, _),
  de_spelers(No, Wedscat, Tg1, Tg2, Spelers),
  member(Speler, Spelers),
  not(spWeds(Speler)),
  assert(spWeds(Speler)),
  fail.
cacheSpWeds(_, _, _).

cacheSpWeds1(Dag, DagenList, _, _) :-
  member(Dag, DagenList), !.
cacheSpWeds1(_, _, Lok, Lokaties) :-
  member(Lok, Lokaties), !.

updateStatus(_Win) :-
  cursor_SetWait(),
  preselC(_CatPresel, eerste, _Win), !,
  findall(SpelerPlus, adres_namen(_, SpelerPlus), Lijst),
  count(Lijst, 0, Aantal),
  findall(Xs, sp2(Xs,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),XsL),
  count(XsL,0,TotAant),
  assert(eersteSelectie(Lijst)),
  Ofwin = win_GetCtlHandle(_Win, idctu_dit),
  T1 = "NB. Dit is een +-relatie tussen alle mogelijkheden. Selecteer alléén wat je werkelijk wilt. ",
  T2 = "\nVoorbeeld: ALLE spelersoorten geeft ook ALLE spelers. Klik desgewenst \"Deelselectie\".",
  concat(T1, T2, T3),
  format(Text, "Er zijn %u personen geselecteerd van totaal %u.", Aantal, TotAant),
  win_SetText(OfWin, T3),
  OTWin = win_GetCtlHandle(_Win, idctu_totaal_geselecteerd),
  win_SetText(OTWin, Text),
  updatestatus1(_Win, Aantal, eerste).
updateStatus(_Win) :-
  preselC(_CatPresel, tweede, _Win), !,
  eersteselectie(ELijst),
  count(ELijst, 0, AantalInit),
  findall(Speler, adres_namenX(Speler, ELijst), Lijst),
  count(Lijst, 0, Aantal),
  Ofwin = win_GetCtlHandle(_Win, idctu_dit),
  T1 = "NB. Dit is een +-relatie tussen alle mogelijkheden. Selecteer alléén wat je werkelijk wilt. ",
  T2 = "\nDat wordt een deel van wat je in het vorige scherm gekozen hebt.",
  concat(T1, T2, T3),
  format(Text, "Er zijn nu %u personen geselecteerd van de eerder geselecteerde %u.", Aantal, AantalInit),
  win_SetText(OfWin, T3),
  OTWin = win_GetCtlHandle(_Win, idctu_totaal_geselecteerd),
  win_SetText(OTWin, Text),
  updatestatus1(_Win, Aantal, tweede).

updatestatus1(_Win, Aantal, eerste) :-
  %preselC(_CatPresel, eerste, _Win),
  VWin = win_GetCtlHandle(_Win, idcu_verdere_selectie),
  OWin = win_GetCtlHandle(_Win, idc_OK),
  win_SetState(VWin, [wsf_disabled]),
  win_SetState(OWin, [wsf_disabled]),
  Aantal > 0,
  win_SetState(VWin, [wsf_enabled]),
  win_SetState(OWin, [wsf_enabled]),
  fail.
updatestatus1(_Win, _Aantal, tweede) :-
  %preselC(_CatPresel, tweede, _Win),
  VWin = win_GetCtlHandle(_Win, idcu_verdere_selectie),
  win_SetState(VWin, [wsf_disabled, wsf_invisible]),
  win_SetText(_Win, "Selecteren, stap twee.."),
  fail.
updatestatus1(_Win, _Aantal, _).

preselectC(RubWin, [Rubr|Rest], I, CatPresel) :-
  wc(Cat, _, _, Rubr, _EDG, _Abbr, _, _, _, _, _, _, _, _, _Contr),
  %wc(Cat,Rubr,_,_,_,_,_),
  member(Cat, CatPresel), !,
  lbox_SetSel(RubWin, I, b_True),
  I1 = I + 1,
  preselectC(RubWin, Rest, I1, CatPresel).
preselectC(RubWin, [_|Rest], I, CatPresel) :-
  I1 = I + 1, !,
  preselectC(RubWin, Rest, I1, CatPresel).
preselectC(_RubWin, _, _, _).

speeldag(Dag, DagRepr) :-
  dag(Dag, _, _),
  getbacktrack(Here),
  w2(_, _WC, Tijd,_,_,_,_,_),
  bitright(Tijd, 7, Dag1),
  Dag = Dag1,
  cutbacktrack(Here),
  dt_dateoffset_to_str(Dag, "%Dn\t%Dd/%Md", DagRepr).

tweedeNiveauDisable(tweede, Win2) :-
  preselC(_, eerste, Win1),
  LftVanWin = win_GetCtlHandle(Win1, idcu_van),
  LftVanS = win_GetText(LftVanWin),
  LftTotWin = win_GetCtlHandle(Win1, idcu_totmet),
  LftTotS = win_GetText(LftTotWin),
  concat(LftVanS, LftTotS, Leeftijden),
  fronttoken(Leeftijden, _, _),
  LftVanWin2 = win_GetCtlHandle(Win2, idcu_van),
  win_SetState(LftVanWin2, [wsf_disabled]),
  LftTotWin2 = win_GetCtlHandle(Win2, idcu_totmet),
  win_SetState(LftTotWin2, [wsf_disabled]),
  LftSelWin = win_GetCtlHandle(Win2, idcta_leeftijdselectie),
  win_SetState(LftSelWin, [wsf_disabled]),
  LftSelVWin = win_GetCtlHandle(Win2, idctu_leeftijd),
  win_SetState(LftSelVWin, [wsf_disabled]),
  LftSelTWin = win_GetCtlHandle(Win2, idctu_tm),
  win_SetState(LftSelTWin, [wsf_disabled]),
  fail.
tweedeNiveauDisable(tweede, Win2) :-
  preselC(_, eerste, Win1),
  OndWin = win_GetCtlHandle(Win1, idcu_lbond),
  lbox_GetSel(OndWin,OndList,_),
  not(OndList = []),
  OndWin2 = win_GetCtlHandle(Win2, idcu_lbond),
  win_SetState(OndWin2, [wsf_disabled]),
  AWin2 = win_GetCtlHandle(Win2, idcu_alle),
  win_SetState(AWin2, [wsf_disabled]),
  GWin2 = win_GetCtlHandle(Win2, idcu_pbgeen),
  win_SetState(GWin2, [wsf_disabled]),
  fail.
tweedeNiveauDisable(tweede, Win2) :-
  preselC(_, eerste, Win1),
  VerWin = win_GetCtlHandle(Win1, idcu_lbclub),
  lbox_GetSel(VerWin,VerList,_),
  not(VerList = []),
  VerWin2 = win_GetCtlHandle(Win2, idcu_lbclub),
  win_SetState(VerWin2, [wsf_disabled]),
  AWin2 = win_GetCtlHandle(Win2, idcu_alleclubs),
  win_SetState(AWin2, [wsf_disabled]),
  GWin2 = win_GetCtlHandle(Win2, idcu_geenclubs),
  win_SetState(GWin2, [wsf_disabled]),
  fail.
tweedeNiveauDisable(tweede, Win2) :-
  preselC(_, eerste, Win1),
  SrtWin = win_GetCtlHandle(Win1, idcu_lbspsrt),
  lbox_GetSel(SrtWin,SrtList,_),
  not(SrtList = []),
  SrtWin2 = win_GetCtlHandle(Win2, idcu_lbspsrt),
  win_SetState(SrtWin2, [wsf_disabled]),
  AWin2 = win_GetCtlHandle(Win2, idcu_allesrt),
  win_SetState(AWin2, [wsf_disabled]),
  GWin2 = win_GetCtlHandle(Win2, idcu_geensrt),
  win_SetState(GWin2, [wsf_disabled]),
  fail.
tweedeNiveauDisable(tweede, Win2) :-
  preselC(_, eerste, Win1),
  NOWin = win_GetCtlHandle(Win1, idcu_niet_opgegeven_hebben),
  b_True = win_IsChecked(NOWin),
  NOWin2 = win_GetCtlHandle(Win2, idcu_niet_opgegeven_hebben),
  win_SetState(NOWin2, [wsf_disabled]),
  fail.
tweedeNiveauDisable(tweede, Win2) :-
  preselC(_, eerste, Win1),
  USWin = win_GetCtlHandle(Win1, idcu_uitgeloot_zijn),
  b_True = win_IsChecked(USWin),
  USWin2 = win_GetCtlHandle(Win2, idcu_uitgeloot_zijn),
  win_SetState(USWin2, [wsf_disabled]),
  fail.
tweedeNiveauDisable(tweede, Win2) :-
  preselC(_, eerste, Win1),
  StWin = win_GetCtlHandle(Win1, idcu_lbspsterkte),
  lbox_getSel(StWin, Sterktes, _),
  not(Sterktes = []),
  StWin2 = win_GetCtlHandle(Win2, idcu_lbspsterkte),
  win_SetState(StWin2, [wsf_disabled]),
  fail.
tweedeNiveauDisable(tweede, Win2) :-
  preselC(_, eerste, Win1),
  StWin = win_GetCtlHandle(Win1, idcu_spstdubbel),
  lbox_getSel(StWin, Sterktes, _),
  not(Sterktes = []),
  StWin2 = win_GetCtlHandle(Win2, idcu_spstdubbel),
  win_SetState(StWin2, [wsf_disabled]),
  fail.
tweedeNiveauDisable(tweede, Win2) :-
  preselC(_, eerste, Win1),
  BetWin = win_GetCtlHandle(Win1, idc_betalen),
  b_True = win_IsChecked(BetWin),
  BetWin2 = win_GetCtlHandle(Win2, idc_betalen),
  win_SetState(BetWin2, [wsf_disabled]),
  fail.
tweedeNiveauDisable(tweede, Win2) :-
  preselC(_, eerste, Win1),
  MisWin = win_GetCtlHandle(Win1, idcu_gegevens_missen),
  b_True = win_IsChecked(MisWin),
  MisWin2 = win_GetCtlHandle(Win2, idcu_gegevens_missen),
  win_SetState(MisWin2, [wsf_disabled]),
  fail.
tweedeNiveauDisable(tweede, Win2) :-
  preselC(_, eerste, Win1),
  OpmWin = win_GetCtlHandle(Win1, idcu_met_opmerkingen),
  b_True = win_IsChecked(OpmWin),
  OpmWin2 = win_GetCtlHandle(Win2, idcu_met_opmerkingen),
  win_SetState(OpmWin2, [wsf_disabled]),
  fail.
tweedeNiveauDisable(tweede, Win2) :-
  preselC(_, eerste, Win1),
  NinWin = win_GetCtlHandle(Win1, idcu_nog_ergens_in_zitten),
  b_True = win_IsChecked(NinWin),
  NinWin2 = win_GetCtlHandle(Win2, idcu_nog_ergens_in_zitten),
  win_SetState(NinWin2, [wsf_disabled]),
  fail.
tweedeNiveauDisable(tweede, Win2) :-
  preselC(_, eerste, Win1),
  EmWin = win_GetCtlHandle(Win1, idcu_zonderemail),
  b_True = win_IsChecked(EmWin),
  EmWin2 = win_GetCtlHandle(Win2, idcu_zonderemail),
  win_SetState(EmWin2, [wsf_disabled]),
  fail.
tweedeNiveauDisable(tweede, Win2) :-
  preselC(_, eerste, Win1),
  DagWin = win_GetCtlHandle(Win1, speeldagen),
  lbox_GetSel(DagWin,DItemList,_IndexList),
  not(DItemList = []),
  DagWin2 = win_GetCtlHandle(Win2, speeldagen),
  win_SetState(DagWin2, [wsf_disabled]),
  GWin2 = win_GetCtlHandle(Win2, idcu_geen),
  win_SetState(GWin2, [wsf_disabled]),
  LokWin2 = win_GetCtlHandle(Win2, idcu_lblokaties),
  win_SetState(LokWin2, [wsf_disabled]),
  GLWin2 = win_GetCtlHandle(Win2, idcu_geenlok),
  win_SetState(GLWin2, [wsf_disabled]),
  GPWin2 = win_GetCtlHandle(Win2, idcu_gepland_op),
  win_SetState(GPWin2, [wsf_disabled]),
  GDWin2 = win_GetCtlHandle(Win2, idctu_gepland_op),
  win_SetState(GDWin2, [wsf_disabled]),
  GLokWin2 = win_GetCtlHandle(Win2, idctu_lokaties),
  win_SetState(GLokWin2, [wsf_disabled]),
  fail.
tweedeNiveauDisable(tweede, Win2) :-
  preselC(_, eerste, Win1),
  LokWin = win_GetCtlHandle(Win1, idcu_lblokaties),
  lbox_GetSel(LokWin,DItemList,_IndexList),
  not(DItemList = []),
  DagWin2 = win_GetCtlHandle(Win2, speeldagen),
  win_SetState(DagWin2, [wsf_disabled]),
  GWin2 = win_GetCtlHandle(Win2, idcu_geen),
  win_SetState(GWin2, [wsf_disabled]),
  LokWin2 = win_GetCtlHandle(Win2, idcu_lblokaties),
  win_SetState(LokWin2, [wsf_disabled]),
  GLWin2 = win_GetCtlHandle(Win2, idcu_geenlok),
  win_SetState(GLWin2, [wsf_disabled]),
  GPWin2 = win_GetCtlHandle(Win2, idcu_gepland_op),
  win_SetState(GPWin2, [wsf_disabled]),
  GDWin2 = win_GetCtlHandle(Win2, idctu_gepland_op),
  win_SetState(GDWin2, [wsf_disabled]),
  GLokWin2 = win_GetCtlHandle(Win2, idctu_lokaties),
  win_SetState(GLokWin2, [wsf_disabled]),
  fail.
tweedeNiveauDisable(_, _).

peildatum(PWin, Jaar) :-
  getbacktrack(Here),
  dag(Dag, _, _),
  cutbacktrack(Here),
  dt_offset_to_date(Dag, Year, _, _, _, _),
  Year <> Jaar,
  win_SetState(PWin, [wsf_visible]),
  format(Tekst, "NB t.o.v. %u!", Year),
  win_SetText(PWin, Tekst), !.
peildatum(PWin, _Jaar) :-
  win_SetState(PWin, [wsf_invisible]).

setSelCrit(OndList,Spelers) :-
	retractall(namenSelectieKrit(_,_,_,_,_, _,_,_,_,_, _,_,_,_,_, _,_,_,_,_, _,_,_)),
	assert(namenSelectieKrit(eerste, OndList,[],0,0,0,0,[],[],[],b_False,b_False,"",b_False,b_False,b_False,[],[],[],Spelers,b_False,b_False,b_False)).
	%assert(namenSelectieKrit(eerste, OndList,SrtList,HeeftNO,Betalen,LftVan,LftTot,VerList,Sterktes,Missend,Opmerkingen,OpmTekst,NogIn,UitL,ZonderEmail,DagenL,Lokaties)),

  
  dlg_spelergegevensuitvoer_Create(_Parent, _CatPresel, eerste):- 
        retractall(spWeds(_)),	%!!! ######################################3
	retractall(namenSelectieKrit(_,_,_,_,_, _,_,_,_,_, _,_,_,_,_, _,_,_,_,_, _,_,_)),
	fail.
  dlg_spelergegevensuitvoer_Create(Parent, CatPresel, Niveau):- 
	asserta(preselC(CatPresel, Niveau, ~0)),
	win_CreateDynDialog(Parent,
		[
%BEGIN Spelergegevensuitvoer, WinDefList, 11:41:29-23.11.2016, Code automatically updated!
		 dlg_font(wdef(dlg_spelergegevensuitvoer_DlgType,dlg_spelergegevensuitvoer_RCT,dlg_spelergegevensuitvoer_Title,u_DlgBase),
		 	  dlg_spelergegevensuitvoer_Font,dlg_spelergegevensuitvoer_FSize,dlg_spelergegevensuitvoer_Flags),
		 ctl(wdef(wc_LBox,rct(1,10,137,120),"",u_DlgBase),idcu_lbond,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_MultiSelect]),
		 ctl(wdef(wc_PushButton,rct(20,121,41,131),"&Alle",u_DlgBase),idcu_alle,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(50,121,70,131),"&Geen",u_DlgBase),idcu_pbgeen,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_LBox,rct(145,10,224,120),"",u_DlgBase),idcu_lbclub,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_MultiSelect]),
		 ctl(wdef(wc_PushButton,rct(163,121,183,131),"A&lle",u_DlgBase),idcu_alleclubs,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(191,121,211,131),"G&een",u_DlgBase),idcu_geenclubs,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_LBox,rct(232,10,242,87),"",u_DlgBase),idcu_lbspsterkte,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_MultiSelect]),
		 ctl(wdef(wc_LBox,rct(249,10,259,87),"",u_DlgBase),idcu_spstdubbel,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_MultiSelect]),
		 ctl(wdef(wc_CheckBox,rct(267,11,356,21),"Niet opgegeven hebben",u_DlgBase),idcu_niet_opgegeven_hebben,[wsf_Auto,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_CheckBox,rct(267,22,344,32),"Nog moeten &betalen",u_DlgBase),idc_betalen,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_CheckBox,rct(267,33,338,43),"&Gegevens missen",u_DlgBase),idcu_gegevens_missen,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_CheckBox,rct(267,44,344,54),"Nog ergens in zitten",u_DlgBase),idcu_nog_ergens_in_zitten,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_CheckBox,rct(267,55,362,65),"Uitgeloot/uitgesloten zijn",u_DlgBase),idcu_uitgeloot_zijn,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_CheckBox,rct(267,66,362,76),"G&een emailadres hebben",u_DlgBase),idcu_zonderemail,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_CheckBox,rct(267,77,408,87),"&Incassomachtiging afgegeven hebben",u_DlgBase),idcu_incasso,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_CheckBox,rct(267,88,378,98),"Geen eigen &formulier hebben",u_DlgBase),idcu_geen_eigen,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_CheckBox,rct(267,99,373,109),"&Zich nog niet gemeld hebben",u_DlgBase),idcu_zich_nog,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_CheckBox,rct(267,110,358,120),"&Opmerkingen hebben",u_DlgBase),idcu_met_opmerkingen,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_Edit,rct(277,120,344,131),"",u_DlgBase),idcu_opmtekst,[wsf_Group,wsf_TabStop,wsf_AutoHScroll,wsf_AlignLeft]),
		 ctl(wdef(wc_LBox,rct(379,10,435,61),"",u_DlgBase),idcu_lbspsrt,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_MultiSelect]),
		 ctl(wdef(wc_PushButton,rct(383,62,403,72),"A&lle",u_DlgBase),idcu_allesrt,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(409,62,429,72),"G&een",u_DlgBase),idcu_geensrt,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_Edit,rct(379,134,401,146),"",u_DlgBase),idcu_van,[wsf_Group,wsf_TabStop,wsf_AutoHScroll,wsf_AlignLeft]),
		 ctl(wdef(wc_Edit,rct(414,134,434,146),"",u_DlgBase),idcu_totmet,[wsf_Group,wsf_TabStop,wsf_AutoHScroll,wsf_AlignLeft]),
		 ctl(wdef(wc_LBox,rct(27,143,73,204),"",u_DlgBase),speeldagen,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_UseTabStops,wsf_MultiSelect]),
		 ctl(wdef(wc_PushButton,rct(8,193,28,203),"Geen",u_DlgBase),idcu_geen,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_LBox,rct(88,143,120,204),"",u_DlgBase),idcn_lbuur,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_MultiSelect]),
		 ctl(wdef(wc_LBox,rct(140,143,184,204),"",u_DlgBase),idcu_lblokaties,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_UseTabStops,wsf_MultiSelect]),
		 ctl(wdef(wc_PushButton,rct(120,193,140,203),"Geen",u_DlgBase),idcu_geenlok,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(366,158,435,175),"Deelselectie",u_DlgBase),idcu_verdere_selectie,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(400,178,435,203),"&OK",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(365,193,397,203),"&Cancel",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(365,179,397,189),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_Text,rct(4,1,58,10),"ONDERDELEN",u_DlgBase),idctu_onderdelen,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(152,1,207,10),"VERENIGINGEN",u_DlgBase),idctu_clubs,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(375,1,436,10),"SPELERSOORTEN",u_DlgBase),idctu_spsrt,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(229,1,261,10),"SPEELST",u_DlgBase),idctu_speel,[wsf_AlignCenter]),
		 ctl(wdef(wc_Text,rct(363,135,379,145),"Van",u_DlgBase),idctu_leeftijd,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(400,135,414,145),"t/m",u_DlgBase),idctu_tm,[wsf_AlignCenter]),
		 ctl(wdef(wc_Text,rct(232,87,245,97),"enk",u_DlgBase),idctu_enkelspel_speelsterktes,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(187,139,355,178),"",u_DlgBase),idctu_dit,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(370,122,435,132),"LEEFTIJDSELECTIE",u_DlgBase),idcta_leeftijdselectie,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(138,60,144,68),"+",u_DlgBase),idctu_st_1,[wsf_AlignCenter]),
		 ctl(wdef(wc_Text,rct(225,54,231,62),"+",u_DlgBase),idcu_spelergegevensuitvoer_30,[wsf_AlignCenter]),
		 ctl(wdef(wc_Text,rct(267,38,273,46),"+",u_DlgBase),idcu_spelergegevensuitvoer_31,[wsf_AlignCenter]),
		 ctl(wdef(wc_Text,rct(260,54,266,62),"+",u_DlgBase),idcu_spelergegevensuitvoer_32,[wsf_AlignCenter]),
		 ctl(wdef(wc_Text,rct(356,135,362,143),"+",u_DlgBase),idcu_spelergegevensuitvoer_34,[wsf_AlignCenter]),
		 ctl(wdef(wc_Text,rct(283,1,347,10),"SPELERS DIE...",u_DlgBase),idctu_diversen,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(10,152,27,161),"DAG:",u_DlgBase),idctu_gepland_op,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(125,152,143,160),"LOK:",u_DlgBase),idctu_lokaties,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(1,164,6,174),"+",u_DlgBase),idctu_spelergegevensuitvoer_1,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(243,121,278,131),"zoektekst:",u_DlgBase),idctu_tekst,[wsf_AlignLeft,wsf_Disabled,wsf_Invisible]),
		 ctl(wdef(wc_Text,rct(381,146,431,156),"t.o.v. 2006",u_DlgBase),idctu_tov_2006,[wsf_AlignCenter]),
		 ctl(wdef(wc_Text,rct(243,42,249,50),"+",u_DlgBase),idcu_spelergegevensuitvoer_51,[wsf_AlignCenter]),
		 ctl(wdef(wc_Text,rct(247,87,260,97),"dbl",u_DlgBase),idcu_spelergegevensuitvoer_52,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(187,180,353,203),"Totaal geselecteerd",u_DlgBase),idctu_totaal_geselecteerd,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(75,152,89,161),"OM:",u_DlgBase),idcu_spelergegevensuitvoer_58,[wsf_AlignLeft]),
		 ctl(wdef(wc_GroupBox,rct(7,134,186,206),"Gepland op ...",u_DlgBase),idcu_gepland_op,[])
%END Spelergegevensuitvoer, WinDefList
		],dlg_spelergegevensuitvoer_eh,0).

%BEGIN Spelergegevensuitvoer, idc_ok _CtlInfo
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	preselC(_, Niveau, _Win),
	getSelKrit(Niveau, OndList,SrtList,HeeftNO,Betalen,LftVan,LftTot,VerList,Sterktes,SterktesD,Missend,Opmerkingen,OpmTekst,NogIn,UitL,ZonderEmail,DagenL,UrenL,Lokaties,Spelers,Incas,GEF,NNM),
	retractall(namenSelectieKrit(Niveau, _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)),
	assert(namenSelectieKrit(Niveau, OndList,SrtList,HeeftNO,Betalen,LftVan,LftTot,VerList,Sterktes,SterktesD,Missend,Opmerkingen,OpmTekst,NogIn,UitL,ZonderEmail,DagenL,UrenL,Lokaties,Spelers,Incas,GEF,NNM)),
	TaskWin = vpi_GetTaskWin(),
	win_Destroy(_Win),
	Niveau = eerste(),
	win_spelersel_Create(TaskWin, 16),
	spelerSelRefresh(),
	not(eerstehint),
	assert(eerstehint),
	dlg_MessageBox( "Spelerlijst", "Gebruik de iconen in de resulterende spelerslijst\nom de gewenste output te produceren!", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	!.
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0) :- !.
	
%END Spelergegevensuitvoer, idc_ok _CtlInfo
%MARK Spelergegevensuitvoer, new events

%BEGIN Spelergegevensuitvoer, idcn_lbuur selchanged
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(idcn_lbuur,_CtrlType,_CtrlWin,selchanged),0):-
	retractall(spWeds(_)),	%!!! ######################################3
	updateStatus(_Win),
	!.
%END Spelergegevensuitvoer, idcn_lbuur selchanged

%BEGIN Spelergegevensuitvoer, idcu_zich_nog _CtlInfo
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(idcu_zich_nog,_CtrlType,_CtrlWin,_CtlInfo),0):-
	updateStatus(_Win),	
	!.
%END Spelergegevensuitvoer, idcu_zich_nog _CtlInfo

%BEGIN Spelergegevensuitvoer, idcu_geen_eigen _CtlInfo
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(idcu_geen_eigen,_CtrlType,_CtrlWin,_CtlInfo),0):-
	updateStatus(_Win),	
	!.
%END Spelergegevensuitvoer, idcu_geen_eigen _CtlInfo

%BEGIN Spelergegevensuitvoer, idcu_incasso _CtlInfo
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(idcu_incasso,_CtrlType,_CtrlWin,_CtlInfo),0):-
	updateStatus(_Win),	
	!.
%END Spelergegevensuitvoer, idcu_incasso _CtlInfo

%BEGIN Spelergegevensuitvoer, e_Update
  dlg_spelergegevensuitvoer_eh(_Win,e_Update(_UpdateRct),0):-!,
	CtrlWin = win_GetCtlHandle(_Win, idctu_totaal_geselecteerd),
	Font = win_GetFont(_Win),
	_FontName = font_GetAttrs(Font, _Style, _Size),
	NewFont2 = font_SetAttrs(Font,[fs_Bold],0),
	win_SetFont(CtrlWin, NewFont2),
	!.
%END Spelergegevensuitvoer, e_Update

%BEGIN Spelergegevensuitvoer, idcu_spstdubbel selchanged
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(idcu_spstdubbel,_CtrlType,_CtrlWin,selchanged),0):-!,
	updateStatus(_Win),
	!.
%END Spelergegevensuitvoer, idcu_spstdubbel selchanged

%BEGIN Spelergegevensuitvoer, idcu_opmtekst modified
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(idcu_opmtekst,_CtrlType,_CtrlWin,modified),0):-
	updateStatus(_Win),
	!.
%END Spelergegevensuitvoer, idcu_opmtekst modified

%BEGIN Spelergegevensuitvoer, idcu_geenlok _CtlInfo
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(idcu_geenlok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LokWin = win_GetCtlHandle(_Win, idcu_lblokaties),
	selectGeen(LokWin),
	updateStatus(_Win),
	!.
%END Spelergegevensuitvoer, idcu_geenlok _CtlInfo

%BEGIN Spelergegevensuitvoer, idcu_lblokaties selchanged
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(idcu_lblokaties,_CtrlType,_CtrlWin,selchanged),0):-
	retractall(spWeds(_)),	%!!! ######################################3
	updateStatus(_Win),
	!.
%END Spelergegevensuitvoer, idcu_lblokaties selchanged

%BEGIN Spelergegevensuitvoer, idcu_verdere_selectie _CtlInfo
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(idcu_verdere_selectie,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	preselC(CatPresel, eerste, _Win),
	getSelKrit(eerste, OndList,SrtList,HeeftNO,Betalen,LftVan,LftTot,VerList,Sterktes,SterktesD,Missend,Opmerkingen,OpmTekst,NogIn,UitL,ZonderEmail,DagenL,UrenL, Lokaties,Spelers,Incas,GEF,NNM),
	retractall(namenSelectieKrit(eerste, _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)),
	assert(namenSelectieKrit(eerste, OndList,SrtList,HeeftNO,Betalen,LftVan,LftTot,VerList,Sterktes,SterktesD,Missend,Opmerkingen,OpmTekst,NogIn,UitL,ZonderEmail,DagenL,UrenL,Lokaties,Spelers,Incas,GEF,NNM)),
	dlg_spelergegevensuitvoer_Create(_Win, CatPresel, tweede),
	namenSelectieKrit(tweede, _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), 
	TaskWin = vpi_GetTaskWin(),
	win_Destroy(_Win),
	%Niveau = eerste(),
	win_spelersel_Create(TaskWin, 16),
	spelerSelRefresh(),
	%not(eerstehint),
	%assert(eerstehint),
	%dlg_MessageBox( "Spelerlijst", "Gebruik de iconen in de resulterende spelerslijst\nom de gewenste output te produceren!", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	!.
%END Spelergegevensuitvoer, idcu_verdere_selectie _CtlInfo

%BEGIN Spelergegevensuitvoer, speeldagen selchanged
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(speeldagen,_CtrlType,CtrlWin,selchanged),0):-
	lbox_GetSel(CtrlWin,_ItemList,_IndexList),
	_ITemList = [SelDag | _],
	speeldag(D1, DagRepr),
	DagRepr = SelDag,
	%dag(D1,SelDag,_),
	findall(TijdS, planTijd(D1, TijdS, _), TijdenS),	% kwartieren afhankelijk van de dag
	UurWin = win_GetCtlHandle(_Win, idcn_lbuur),
	lbox_Clear(UurWin),
	lbox_Add(UurWin, TijdenS),
	retractall(spWeds(_)),	%!!! ######################################3
	updateStatus(_Win),
	!.
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(speeldagen,_CtrlType,CtrlWin,selchanged),0):-
	lbox_GetSel(CtrlWin,_ItemList,_IndexList),
	_ITemList = [],
	UurWin = win_GetCtlHandle(_Win, idcn_lbuur),
	lbox_Clear(UurWin),
	retractall(spWeds(_)),	%!!! ######################################3
	updateStatus(_Win),
	!.
%END Spelergegevensuitvoer, speeldagen selchanged

%BEGIN Spelergegevensuitvoer, idcu_geen _CtlInfo
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(idcu_geen,_CtrlType,_CtrlWin,_CtlInfo),0):-
	DagWin = win_GetCtlHandle(_Win, speeldagen),
	selectGeen(DagWin),
	UurWin = win_GetCtlHandle(_Win, idcn_lbuur),
	retractall(spWeds(_)),	%!!! ######################################3
	lbox_Clear(UurWin),
	updateStatus(_Win),
	!.
%END Spelergegevensuitvoer, idcu_geen _CtlInfo

%BEGIN Spelergegevensuitvoer, idcu_zonderemail _CtlInfo
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(idcu_zonderemail,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	updatestatus(_Win),
	!.
%END Spelergegevensuitvoer, idcu_zonderemail _CtlInfo

%BEGIN Spelergegevensuitvoer, idcu_niet_opgegeven_hebben _CtlInfo
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(idcu_niet_opgegeven_hebben,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	updateStatus(_Win),
	!.
%END Spelergegevensuitvoer, idcu_niet_opgegeven_hebben _CtlInfo

%BEGIN Spelergegevensuitvoer, idcu_uitgeloot_zijn _CtlInfo
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(idcu_uitgeloot_zijn,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	updateStatus(_Win),
	!.
%END Spelergegevensuitvoer, idcu_uitgeloot_zijn _CtlInfo

%BEGIN Spelergegevensuitvoer, idcu_nog_ergens_in_zitten _CtlInfo
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(idcu_nog_ergens_in_zitten,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	updateStatus(_Win),
	!.
%END Spelergegevensuitvoer, idcu_nog_ergens_in_zitten _CtlInfo

%BEGIN Spelergegevensuitvoer, idcu_met_opmerkingen _CtlInfo
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(idcu_met_opmerkingen,_CtrlType,CtrlWin,_CtlInfo),0):-
	OpmTWin = win_GetCtlHandle(_Win, idcu_opmtekst),
	OpmTTWin = win_GetCtlHandle(_Win, idctu_tekst),
	win_SetState(OpmTWin, [wsf_disabled]),
	win_SetState(OpmTTWin, [wsf_disabled, wsf_invisible]),
	b_True = win_IsChecked(CtrlWin),
	win_SetState(OpmTWin, [wsf_enabled]),
	win_SetState(OpmTTWin, [wsf_enabled, wsf_visible]),
	fail.
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(idcu_met_opmerkingen,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	updateStatus(_Win),
	!.
%END Spelergegevensuitvoer, idcu_met_opmerkingen _CtlInfo

%BEGIN Spelergegevensuitvoer, idcu_gegevens_missen _CtlInfo
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(idcu_gegevens_missen,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	updateStatus(_Win),
	!.
%END Spelergegevensuitvoer, idcu_gegevens_missen _CtlInfo

%BEGIN Spelergegevensuitvoer, idc_betalen _CtlInfo
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(idc_betalen,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	updateStatus(_Win),
	!.
%END Spelergegevensuitvoer, idc_betalen _CtlInfo

%BEGIN Spelergegevensuitvoer, idcu_totmet modified
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(idcu_totmet,_CtrlType,_CtrlWin,modified),0):-!,
	updateStatus(_Win),
	!.
%END Spelergegevensuitvoer, idcu_totmet modified

%BEGIN Spelergegevensuitvoer, idcu_van modified
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(idcu_van,_CtrlType,_CtrlWin,modified),0):-!,
	updateStatus(_Win),
	!.
%END Spelergegevensuitvoer, idcu_van modified

%BEGIN Spelergegevensuitvoer, idcu_lbspsterkte selchanged
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(idcu_lbspsterkte,_CtrlType,_CtrlWin,selchanged),0):-!,
	updateStatus(_Win),
	!.
%END Spelergegevensuitvoer, idcu_lbspsterkte selchanged

%BEGIN Spelergegevensuitvoer, idcu_lbspsrt selchanged
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(idcu_lbspsrt,_CtrlType,_CtrlWin,selchanged),0):-!,
	updateStatus(_Win),
	!.
%END Spelergegevensuitvoer, idcu_lbspsrt selchanged

%BEGIN Spelergegevensuitvoer, idcu_lbclub selchanged
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(idcu_lbclub,_CtrlType,_CtrlWin,selchanged),0):-!,
	updateStatus(_Win),
	!.
%END Spelergegevensuitvoer, idcu_lbclub selchanged

%BEGIN Spelergegevensuitvoer, idcu_lbond selchanged
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(idcu_lbond,_CtrlType,_CtrlWin,selchanged),0):-!,
	updateStatus(_Win),
	!.
%END Spelergegevensuitvoer, idcu_lbond selchanged

%BEGIN Spelergegevensuitvoer, idcu_geensrt _CtlInfo
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(idcu_geensrt,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	SrtWin = win_GetCtlHandle(_Win, idcu_lbspsrt),
	selectGeen(SrtWin),
	updateStatus(_Win),
	!.
%END Spelergegevensuitvoer, idcu_geensrt _CtlInfo

%BEGIN Spelergegevensuitvoer, idcu_allesrt _CtlInfo
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(idcu_allesrt,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	SrtWin = win_GetCtlHandle(_Win, idcu_lbspsrt),
	selectAlle(SrtWin),
	_Result = vpi_ProcessEvents(b_True),
	updateStatus(_Win),
	!.
%END Spelergegevensuitvoer, idcu_allesrt _CtlInfo

%BEGIN Spelergegevensuitvoer, idcu_geenclubs _CtlInfo
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(idcu_geenclubs,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	VerWin = win_GetCtlHandle(_Win, idcu_lbclub),
	selectGeen(VerWin),
	updateStatus(_Win),
	!.
%END Spelergegevensuitvoer, idcu_geenclubs _CtlInfo

%BEGIN Spelergegevensuitvoer, idcu_alleclubs _CtlInfo
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(idcu_alleclubs,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	VerWin = win_GetCtlHandle(_Win, idcu_lbclub),
	selectAlle(VerWin),
	_Result = vpi_ProcessEvents(b_True),
	updateStatus(_Win),
	!.
%END Spelergegevensuitvoer, idcu_alleclubs _CtlInfo

%BEGIN Spelergegevensuitvoer, idcu_pbgeen _CtlInfo
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(idcu_pbgeen,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	OndWin = win_GetCtlHandle(_Win, idcu_lbond),
	selectGeen(OndWin),
	updateStatus(_Win),
	!.
%END Spelergegevensuitvoer, idcu_pbgeen _CtlInfo

%BEGIN Spelergegevensuitvoer, idcu_alle _CtlInfo
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(idcu_alle,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	OndWin = win_GetCtlHandle(_Win, idcu_lbond),
	selectAlle(OndWin),
	cursor_SetWait(),
	_Result = vpi_ProcessEvents(b_True),
	updateStatus(_Win),
	!.
%END Spelergegevensuitvoer, idcu_alle _CtlInfo


%BEGIN Spelergegevensuitvoer, idc_help _CtlInfo
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Spelerselectie"),
	!.
%END Spelergegevensuitvoer, idc_help _CtlInfo

%BEGIN Spelergegevensuitvoer, idc_cancel _CtlInfo
  dlg_spelergegevensuitvoer_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	preselC(_, Niveau, _Win),
	retractall(namenSelectieKrit(Niveau,_,_,_,_, _,_,_,_,_, _,_,_,_,_ ,_,_,_,_,_, _,_,_)),
	win_Destroy(_Win),
	!.
%END Spelergegevensuitvoer, idc_cancel _CtlInfo

%BEGIN Spelergegevensuitvoer, e_Destroy
  dlg_spelergegevensuitvoer_eh(_Win,e_Destroy,0):-!,
	%retractall(cl_sp(_), uit),
	retract(preselC(_, _Niveau_, _Win)),
	%retractall(recno(_,_,_), uit),
	!.
%END Spelergegevensuitvoer, e_Destroy

%BEGIN Spelergegevensuitvoer, e_Create
  dlg_spelergegevensuitvoer_eh(_Win,e_Create(_CreationData),0):-
	retract(preselC(CatPresel, Niveau, ~0)),
	asserta(preselC(CatPresel, Niveau, _Win)),
	tweedeNiveauDisable(Niveau, _Win),
	%OpmWin = win_GetCtlHandle(_Win, idcu_met_opmerkingen),
	PWin = win_GetCtlHandle(_Win, idctu_tov_2006),
	date(Y, _, _),
	peildatum(PWin, Y), 
	OpmTWin = win_GetCtlHandle(_Win, idcu_opmtekst),
	win_SetState(OpmTWin, [wsf_disabled]),
	SrtWin = win_GetCtlHandle(_Win, idcu_lbspsrt),
	findall(SpSoort, spsrt(_,SpSoort,_,_,_,_,_,_),SpSoorten),
	lbox_Add(SrtWin, SpSoorten),
	RubWin = win_GetCtlHandle(_Win, idcu_lbond),
	findall(Rubriek, wcMetSpelers(Rubriek),Rubrieken),
	lbox_Add(RubWin, Rubrieken),
	preselectC(RubWin, Rubrieken, 0, CatPresel),
	VerWin = win_GetCtlHandle(_Win, idcu_lbclub),
	findall(Naam, club(_, Naam, _), VerNamen),
	sortstring_c(VerNamen, 1),
	lbox_Add(VerWin, VerNamen),
	StWin = win_GetCtlHandle(_Win, idcu_lbspsterkte),
	lbox_Add(StWin, ["1", "2", "3", "4", "5", "6", "7", "8", "9"]),
	StDWin = win_GetCtlHandle(_Win, idcu_spstdubbel),
	lbox_Add(StDWin, ["1", "2", "3", "4", "5", "6", "7", "8", "9"]),
	findall(SpD, speeldag(_, SpD), SDList),
	DagWin = win_GetCtlHandle(_Win, speeldagen),
	lbox_SetTabStops(DagWin, [15, 30]),
	lbox_Add(DagWin, SDList),
	_UurWin = win_GetCtlHandle(_Win,idcn_lbuur),
	findlokaties(Lokaties),
	LokWin = win_GetCtlHandle(_Win, idcu_lblokaties),
	lbox_Add(LokWin, Lokaties), 
	updateStatus(_Win),
	!.
  dlg_spelergegevensuitvoer_eh(_Win,e_Create(_CreationData),0)  :- !.
%END Spelergegevensuitvoer, e_Create

  dlg_spelergegevensuitvoer_eh(_,_,_):-!,fail.

%END_DLG Spelergegevensuitvoer

predicates

  write_cl(integer, clubno, clubnaam, distrct)
  clubs_uit1(integer)
%  err_code(integer, string)

clauses

clubs_uit(Formaat) :- trap(clubs_uit1(Formaat), _Retcode, true).

clubs_uit1(F) :-
  assert(recno1(0)),
  %dos_pad(Path),
  getFolder(TDir,_,_,_),
  filenamePath(Uitvoer, TDir, "club.exp"),
  Fname = dlg_GetFileName(Uitvoer,["exp","*.exp","txt","*.txt"],"Mailmerge bestand",[dlgfn_Save],"",_OutListFiles),
  filenamepath(Fname, _Path, Name),
  Name <> fctconst,
  format(Msg, "Uitvoer clubgegevens naar %", Fname),
  writeToJournaal(Msg, ja),
  closefile(work),
  openwrite(work, Fname),
  assert(recno1(0)),
  writedevice(work),
  club(Code, Naam, District),
  recno1(No),
  No1 = No + 1,
  assert(recno1(No1)),
  write_cl(F, Code, Naam, District),
  fail.
clubs_uit1(_) :-
  closefile(work),
  recno1(N), !,
  format(Tkst," % records uitgevoerd ", N),
  writeToJournaal(Tkst, ja), !.

write_cl(1, Club, Naam, Distr) :-
  write('"', Club, '"', csvconst, '"', Naam, '"', csvconst, '"', Distr, '"'), nl, !.
write_cl(2, Club, Naam, Distr) :-
  writef("%-5s%-23.23s%-23.23s\n", Club, Naam, Distr).


database - partners
single selectedpartner(string)
single partnertitel(string)
single partnercat(sexlist)

predicates

selectSpCat(sexlist, sex)
spcatL2stringL(sexlist, SLIST)
  checkNaam(string, string)
  spzoeksubclass_eh : EHANDLER

clauses
selectedpartner("").
partnertitel("").
partnercat([]).

spzoeksubclass_eh(_LWin,e_Char(' ',_),0):-
  win_SetText(_LWin, ""),
  !.
spzoeksubclass_eh(_LWin,e_Char(k_down,X),0):-
  Win = win_GetParent(_LWin),
  LboxWin = win_GetCtlHandle(Win, idc_spelerselect_lbox),
  win_SendEvent(LboxWin,e_Keydown(k_down, X)),
  Index = lbox_GetSelIndex(LboxWin),
  I1 = Index + 1,
  lbox_SetSel(LboxWin, I1, b_True),
  Item = lbox_GetItem(LboxWin, I1),
  frontstr(2,Item,_,RestString),
  win_SetText(_LWin, RestString),
  !.
spzoeksubclass_eh(_LWin,e_Char(k_down,_),0):-
  Win = win_GetParent(_LWin),
  LboxWin = win_GetCtlHandle(Win, idc_spelerselect_lbox),
  lbox_SetSel(LboxWin, 0, b_True),
  Item = lbox_GetItem(LboxWin, 0),
  frontstr(2,Item,_,RestString),
  win_SetText(_LWin, RestString),
  !.
spzoeksubclass_eh(_LWin,e_Char(k_up,_),0):-
  Win = win_GetParent(_LWin),
  LboxWin = win_GetCtlHandle(Win, idc_spelerselect_lbox),
  Index = lbox_GetSelIndex(LboxWin),
  I1 = Index - 1,
  lbox_SetSel(LboxWin, I1, b_True),
  Item = lbox_GetItem(LboxWin, I1),
  frontstr(2,Item,_,RestString),
  win_SetText(_LWin, RestString),
  !.
spzoeksubclass_eh(_LWin,e_Char(k_up,_),0):-
  Win = win_GetParent(_LWin),
  LboxWin = win_GetCtlHandle(Win, idc_spelerselect_lbox),
  lbox_SetSel(LboxWin, 0, b_True),
  Item = lbox_GetItem(LboxWin, 0),
  frontstr(2,Item,_,RestString),
  win_SetText(_LWin, RestString),
  !.

checkNaam(PNaam, PNaam) :-
  trim_c(PNaam),
  not(PNaam = ""),
  not(sp2(_,_,PNaam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)), !.
checkNaam(PNaam, NaamO) :-
  sp2(_,_,PNaam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  %sp1(_,_,PNaam,_,_,_,_,_, _, _, _, _, _, _, _,_,_,_,_), !,
  1 = dlg_MessageBox( "Nieuwe speler", "Naam komt al voor in lijst!", mesbox_iconexclamation, mesbox_buttonsokcancel, mesbox_defaultfirst, mesbox_suspendapplication ),
  Naam1=dlg_GetStr("Nieuwe speler", "Naam van partner?",PNaam),
  checkNaam(Naam1, NaamO), !.

selectSpCat([],_) :- 
  dlg_MessageBox( "Selectie", "Spelerselectie leeg.", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.
selectSpCat([SpCat],SpCat) :- !.
selectSpCat(SpCatList, SpCat) :-
  spcatL2stringL(SpCatList, StringList),
  dlg_ListSelect("Welke spelersoort?", StringList, 1, _StrSel, _Index),
  spsrt(SpCat, _StrSel, _, _, _, _, _, _), !.

spcatL2stringL([], []) :- !.
spcatL2stringL([Top|Rest], [String|List]) :- 
  spsrt(Top, String, _, _, _, _, _, _), !, 
  spcatL2stringL(Rest, List).


%BEGIN_DLG Partnerselectie
/**************************************************************************
	Creation and event handling for dialog: Partnerselectie
**************************************************************************/

constants

%BEGIN Partnerselectie, CreateParms, 22:14:16-27.10.2011, Code automatically updated!
  dlg_partnerselectie_DlgType = wd_Modal
  dlg_partnerselectie_Title = "Partnerselectie"
  dlg_partnerselectie_RCT = rct(50,40,225,245)
  dlg_partnerselectie_Flags = [wsf_Close,wsf_TitleBar]
  dlg_partnerselectie_Help = idh_partnerselectie
  dlg_partnerselectie_Font = "Arial"
  dlg_partnerselectie_FSize = 10
%END Partnerselectie, CreateParms

predicates

  dlg_partnerselectie_eh : EHANDLER

clauses

  dlg_partnerselectie_Create(Parent, Titel, Partners, SexList, Selected):-
	PartnerL = cast(long, Partners),
	assert(partnertitel(Titel)),
	assert(selectedpartner("")),
	assert(partnercat(SexList)),
	win_CreateDynDialog(Parent,
		[
%BEGIN Partnerselectie, WinDefList, 22:14:16-27.10.2011, Code automatically updated!
		 dlg_font(wdef(dlg_partnerselectie_DlgType,dlg_partnerselectie_RCT,dlg_partnerselectie_Title,u_DlgBase),
		 	  dlg_partnerselectie_Font,dlg_partnerselectie_FSize,dlg_partnerselectie_Flags),
		 ctl(wdef(wc_Edit,rct(3,1,87,13),"",u_DlgBase),idc_partnerselect_str,[wsf_Group,wsf_TabStop,wsf_AutoHScroll,wsf_AlignLeft]),
		 ctl(wdef(wc_LBox,rct(2,15,116,204),"",u_DlgBase),idc_spelerselect_lbox,[wsf_TabStop,wsf_Group,wsf_VScroll,wsf_NoIntegralHeight,wsf_UseTabStops]),
		 ctl(wdef(wc_PushButton,rct(120,183,172,201),"Ge&selecteerd",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(124,120,167,132),"&Cancel",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(124,104,167,116),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(120,150,172,163),"&Nieuw",u_DlgBase),idc_nieuwe_speler,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(120,167,172,180),"&Gezocht",u_DlgBase),idc_wanted,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_Text,rct(119,55,158,64),"! = gezocht",u_DlgBase),idct_wanted,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(119,64,158,74),"¬ = bezet",u_DlgBase),idc_partnerselectie_9,[wsf_AlignLeft]),
		 ctl(wdef(wc_GroupBox,rct(118,139,174,204),"Partner",u_DlgBase),idc_partner,[])
%END Partnerselectie, WinDefList
		],dlg_partnerselectie_eh,PartnerL),
		assert(partnertitel("geen")),
		selectedpartner(Selected).

%BEGIN Partnerselectie, idc_ok _CtlInfo
  dlg_partnerselectie_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idc_spelerselect_lbox),
	lbox_GetSel(LboxWin,ItemList,_IndexList),
	ItemList = [Item|_],
	not(frontchar(Item,'¬',_)),
	assert(selectedpartner(Item)),
	win_Destroy(_Win),
	!.
%END Partnerselectie, idc_ok _CtlInfo
%MARK Partnerselectie, new events

%BEGIN Partnerselectie, idc_wanted _CtlInfo
  dlg_partnerselectie_eh(_Win,e_Control(idc_wanted,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	assert(selectedpartner("<wanted>")),
	win_Destroy(_Win),
	!.
%END Partnerselectie, idc_wanted _CtlInfo

%BEGIN Partnerselectie, idc_spelerselect_lbox activated
  dlg_partnerselectie_eh(_Win,e_Control(idc_spelerselect_lbox,_CtrlType,_CtrlWin,activated),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idc_spelerselect_lbox),
	lbox_GetSel(LboxWin,ItemList,_IndexList),
	ItemList = [Item|_],
	not(frontchar(Item,'¬',_)),
	assert(selectedpartner(Item)),
	win_Destroy(_Win),
	!.
%END Partnerselectie, idc_spelerselect_lbox activated

%BEGIN Partnerselectie, idc_help _CtlInfo
  dlg_partnerselectie_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Partnerselectie"),
	!.
%END Partnerselectie, idc_help _CtlInfo

%BEGIN Partnerselectie, idc_nieuwe_speler _CtlInfo
  dlg_partnerselectie_eh(_Win,e_Control(idc_nieuwe_speler,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	PNaam=dlg_GetStr("Nieuwe speler", "Naam van partner?",""),
	checkNaam(PNaam,PNaam1),
	partnercat(SpCatsList),
	selectSpCat(SpCatsList, SpCat),
	getbacktrack(Here),
	loglock(lees, "partnerselectie"),		% zorg dat nummer uniek is
	logclose(),
	repeat,
	random(30000,Y),	% bepaal vrij nummer (tot 30000 ivm 16 bits compat)
	SpKeus = Y + 1,
	not(  sp2(SpKeus,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)),
	cutbacktrack(Here),
	SpelerIn = sp2(SpKeus,SpCat,PNaam1,"",[],"","",nee,0, "", "", "", "", "", "", "", "", "", "", "",nee,"","","","","",[]),
	logf('A',SpelerIn,nee),
	concat("  ", PNaam1, Partner),
	assert(selectedpartner(Partner)),
	spelerSelRefresh(),
	win_Destroy(_Win),
	!.
%END Partnerselectie, idc_nieuwe_speler _CtlInfo

%BEGIN Partnerselectie, idc_partnerselect_str modified
  dlg_partnerselectie_eh(_Win,e_Control(idc_partnerselect_str,_CtrlType,_CtrlWin,modified),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idc_spelerselect_lbox),
	LboxList = lbox_GetAll(LboxWin),
	Text = win_GetText(_CtrlWin),
	trim_c(Text),
	searchslist_c(LboxList, Text, 3, Index),
	lbox_SetSel(LboxWin, Index, b_True),
	!.
%END Partnerselectie, idc_partnerselect_str modified

%BEGIN Partnerselectie, e_User
  dlg_partnerselectie_eh(_Win,e_User(1,_Ptr),0):-!,
	LtrWin = win_GetCtlHandle(_Win, idc_partnerselect_str),
	win_SetSubclassHandler(LtrWin,spzoeksubclass_eh,b_false),
	!.
%END Partnerselectie, e_User

%BEGIN Partnerselectie, idc_cancel _CtlInfo
  dlg_partnerselectie_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	win_Destroy(_Win),
	!.
%END Partnerselectie, idc_cancel _CtlInfo

%BEGIN Partnerselectie, e_Create
  dlg_partnerselectie_eh(_Win,e_Create(_CreationData),0):-!,
	partnertitel(Titel),
	win_SetText(_Win, Titel),
	PartnerList = cast(slist, _CreationData),
	LboxWin = win_GetCtlHandle(_Win, idc_spelerselect_lbox),
	lbox_setTabstops(LboxWin, [10,20]),
	lbox_Add(LboxWin, PartnerList),
	lbox_SetSel(LboxWin, 0, b_True),
	win_PostEvent(_Win,e_User(1,0)),
	!.
%END Partnerselectie, e_Create

  dlg_partnerselectie_eh(_,_,_):-!,fail.

%END_DLG Partnerselectie

predicates
  verder(integer,wedscat,wl)
  nog_niet_verloren(integer, wedscat, janee)
  merk(janee, string, string, rsoort)
  selectSpWeds(string, slist, string)

clauses

selecttelefoon(In1, In2, In3, Uit) :-
  findall(Tel, selecttelefoon1(In1, In2, In3, Tel), List),
  list_to_string(List, ",", Uit).

selecttelefoon1(Uit, _, _, Uit) :-
  fronttoken(Uit, _, _).
selecttelefoon1(_, Uit, _, Uit) :-
  fronttoken(Uit, _, _).
selecttelefoon1(_, _, Uit, Uit).

mv2string(m, "m"):- !.
mv2string(_, "v").

spelerVoorLijst(_, SpelerPlus, 0) :-		% alle
  sp2(_,Sex,Speler,Tel1,_ ,_,_,_,_,_ClubS,_, _,_,_,_,_, Tel2,Tel3,_,_,_,_,_,_,_,_,_),
  selecttelefoon(Tel1,Tel2,Tel3,Tel),
  spsrt(Sex, _StrSel, MV, _, _, _, _, _), 
  mv2string(MV, Str),
  format(SpelerPlus, "%s\t%s\t%s", Str, Speler, Tel).
spelerVoorLijst(_, SpelerPlus, 1) :-		% deelnemers
  sp2(SpNo,Sex,Speler,Tel1,_ ,_,_,_,_,_ClubS,_, _,_,_,_,_, Tel2,Tel3,_,_,_,_,_,_,_,_,_),
  selecttelefoon(Tel1,Tel2,Tel3,Tel),
  getbacktrack(Here),
  sp2opg(SpNo, _, _, _, _, _),
  cutbacktrack(Here),
  spsrt(Sex, _StrSel, MV, _, _, _, _, _), 
  mv2string(MV, Str),
  format(SpelerPlus, "%s\t%s\t%s", Str, Speler, Tel).
spelerVoorLijst(_, SpelerPlus, 2) :-		% niet passende soorten
 filtercheck(_, SpelerPlus).
spelerVoorLijst(_, SpelerPlus, 3) :-		% niet passende soorten
  spTeChecken(SpelerPlus, 3).
spelerVoorLijst(_, SpelerPlus, 4) :-		% formulier nog niet gezien
  spTeChecken(SpelerPlus, 4).
spelerVoorLijst(Win, SpelerPlus, N) :-		% niet passende soorten
% keus 1 = dubbel, 2 = ontbrekend, 3 = ongeldig obsolete!!
% keus 5 = dubbel, 6 = ontbrekend, 7 = ongeldig
  N >= 5,
  N <= 7,
  %Keus = N - 4,
  cursor_Set(Win, cursor_Wait),
  findall(LidNo_SpNo, lid_sp(LidNo_SpNo,N), List),
  cursor_Set(Win, cursor_Arrow),
  sortstring_c(List, 1),		% voor de dubbele
  check_nos(N, List, [], Spelers),
  member(Speler, Spelers),
  sp2(_,Sex,Speler,Tel1,_ ,_,_,_,_,_ClubS,_, _,_,_,_,_, Tel2,Tel3,_,_,_,_,_,_,_,_,_),
  selecttelefoon(Tel1,Tel2,Tel3,Tel),
  spsrt(Sex, _StrSel, MV, _, _, _, _, _), 
  mv2string(MV, Str),
  format(SpelerPlus, "%s\t%s\t%s", Str, Speler, Tel).
spelerVoorLijst(_, SpelerPlus, 8) :-		% niet-spelers?
  findall(Naam, niet_sp(_, Naam), Namen),
  sortstring_c(Namen,3),
  member(SpelerPlus, Namen).
spelerVoorLijst(_, SpelerPlus, 9) :-		% van één vereniging
  selectedClub(ClubUU),
  sp2(_,Sex,Naam,Tel1,_ ,_,_,_,_,ClubS,_, _,_,_,_,_,Tel2,Tel3,_,_,_,_,_,_,_,_,_),
  upper_lower(ClubU, ClubS),
  trim_c(ClubU),
  ClubUU = ClubU,
  spsrt(Sex, _, MV, _, _, _, _, _),
  mv2string(MV, Str),
  selecttelefoon(Tel1,Tel2,Tel3,Tel),
  format(SpelerPlus, "%s\t%s\t%s", Str, Naam, Tel).
spelerVoorLijst(_, SpelerPlus, 10) :-		% spelers van geselecteerde onderdelen
  %selectedCats(CatNoList),
  findall(Speler, selected_sp_in_cats(Speler, _), Spelers),
  sortstring_c(Spelers, 1),
  member(Naam, Spelers),
  sp2(_,Sex,Naam,Tel1,_ ,_,_,_,_,_,_Club, _,_,_,_,_, _,Tel2,Tel3,_,_,_,_,_,_,_,_),
  spsrt(Sex, _, MV, _, _, _, _, _),
  mv2string(MV, Str),
  selecttelefoon(Tel1,Tel2,Tel3,Tel),
  format(SpelerPlus, "%s\t%s\t%s", Str, Naam, Tel).
spelerVoorLijst(_, SpelerPlus, 16) :-
  %namenSelectieKrit(eerste, OndList,SrtList,HeeftNO,Betalen,LftVan,LftTot,VerList,Sterktes,Missend,Opmerkingen,NogIn,UitL,ZonderEmail,DagenList),
  naamVanLijst1(SpelerPlus).

wed_opgest(SKeus,w(WNum,Cat,0, Uitsl,LR,Zelf,b_False,"BaanPark")) :-
  wedstrijd(WNum,Cat,SKeus,LR,Zelf,_),
  nog_niet_verloren(WNum, Cat, Uitsl).
  
nog_niet_verloren(WNum, Cat, ja) :-
  pos(Cat,_,_,Srt),
  not(Srt = poel), 
  w3(WNum, Cat, _, _, _, _, _), !.
nog_niet_verloren(_, _, nee). 

merk(ja, CatS, CatS1, afval) :- !, concat("-  ", CatS, CatS1).
merk(ja, CatS, CatS1, poel) :- !, concat("- p", CatS, CatS1).
merk(nee, CatS, CatS1, afval) :- concat("   ", CatS, CatS1).
merk(nee, CatS, CatS1, poel) :- concat("  p", CatS, CatS1).

versten([],_,[],[]) :- !.
versten([w(WNum,Cat,_,_,_,_,_,_)|Rest],Wedstrijden,Out,CatL) :-
  verder(WNum,Cat,Wedstrijden), !,
  versten(Rest,Wedstrijden,Out,CatL).
versten([w(WNum,Cat,Tijd,Uitsl,_,P,BW,BaanPark)|Rest],Wedstrijden,
			[w(WNum,Cat,Tijd,ja,0,P,BW,BaanPark)|Out],[CatK1|CatL]) :-
  wc(Cat, _, _, _Naam, _EDG, CatK, _, _, _, _, _, _, _, _, _Contr),
  %wc(Cat,_,_,CatK,_,_,_),
  pos(Cat, _, _, AfvalPoel), 
  merk(Uitsl, CatK, CatK1, AfvalPoel), !,
  versten(Rest,Wedstrijden,Out,CatL).

wedstr_member(Weds,[Weds|_]).
wedstr_member(Weds,[_|Tail]) :- wedstr_member(Weds,Tail).

verder(WNum,Cat,Wedstrijden) :-  
  wedstr_member(Wedstrijd,Wedstrijden),
  Wedstrijd = w(W1, Cat1, _, _, _, _, _, _),
  WNum > W1,
  Cat = Cat1, !.

%selectSpWeds(_,[CatK],CatK) :- !.
selectSpWeds(_Naam,[], _CatK) :- !, fail.
selectSpWeds(Naam,CatL, CatK) :-
  format(Title, "% doet mee aan:",Naam),
  dlg_ListSelect(Title,CatL,0,CatK,_Index).


database - pers
determ spelerSelect(WINDOW, integer Mode)
single zoekDoor(integer TimerID, string Zoekstring)
determ synoniem(persno)
%tijdelijkesex(sex)
single teller(integer)
single tellerV(integer)

%BEGIN_WIN SpelerSel
/**************************************************************************
        Creation and event handling for window: SpelerSel
**************************************************************************/

constants
%BEGIN SpelerSel, CreateParms, 14:06:27-30.1.2017, Code automatically updated!
  win_spelersel_WinType = w_TopLevel
  win_spelersel_Flags = [wsf_SizeBorder,wsf_TitleBar,wsf_Close,wsf_ClipSiblings,wsf_ClipChildren]
  win_spelersel_RCT = rct(100,80,550,483)
  win_spelersel_Menu = no_menu
  win_spelersel_Title = "Spelerlijst"
  win_spelersel_Help = idh_spelerslijst
%END SpelerSel, CreateParms
id_spelers_bijwerken = 13115
id_spelers_geensnoeien = 13118
id_Speler_Checken_Nietspelers_verwijderen = 13117

domains
  puinS = spmin(integer SpNo, string Melding)

predicates

  win_spelersel_eh : EHANDLER
  inLijst : zoekFunc
  sOwnerdraw(WINDOW, RCT, integer Item)
  subclassS_eh : EHANDLER  
  popUpS(WINDOW, PNT)
procedure  opzoekbaar(slist, BOOLEAN)
procedure  wisbaar(slist, BOOLEAN)
procedure  sTitel(integer Mode, string Titel) - (i,o)
procedure invoerbaar1(integer Mode, BOOLEAN)
	selectSpsrt(sexe MV, sex Soort)
selectie2sexe(integer, sexe)
procedure timerstop(integer TimerId)
nondeterm lboxList2SpNo(slist LboxList, integer SpNo)
%nondeterm lboxList2SpNaam(slist LboxList, string Naam)
procedure sorteer(slist, boolean, slist)
procedure naamList2NaamListZonder(slist, slist)
procedure naamList2NaamListMet(slist, slist)
%procedure gegevens_bijwerken(slist MV, boolean Verbose)
%procedure opslagtijdelijkesex(sex)
determ gegevens_bijwerkenX(slist MV, boolean Verbose, string Tnummer, string Orgnummer) - (i, i, i, i)
%determ plausibel(string TNr)
%determ validFr(string)
procedure currentNummer(string Reststring, string TNr, string OrgCode) 
procedure handlesynoniem(window, persno, dbasedom)
nondeterm checkSamensmeltenNietMogelijk(persno SKeus1, persno SKeus2, string)
determ    checkredenen(string, string, slist)
procedure gegevenscount(persno SKeus, integer Count)
procedure bepaalMaster(persno SKeus1, persno SKeus2, persno, persno)
procedure gegevenAanwezig(string String, integer In, integer Uit)
procedure kiesGegeven(string In1, string In2, string Uit)
procedure kiesverh(verhl In, verhl , verhl Uit)
procedure kiesbetaald(janee, janee, janee)
procedure kiesuitkant(integer , integer, integer)
procedure samenvoegen(window, persno SKeus1, persno SKeus2)
procedure samenvoegenOpgaven(persno SKeus1, persno SKeus2)
%procedure orderedTgnst(tgnst In, tgnst Uit) 
procedure changeAllTgnst(wedscat Cat, tgnst Tgnst, tgnst TgnstN)
procedure zeefgebdatum(string, char, boolean CouldBeLeadingZero, string, string)
determ isLeadingZero(char, boolean)
procedure geensnoeien(slist Spelerlijst)
determ 	  erIsEenPlanning()
nondeterm wisbaarL(persno SKeus, window Lboxlist)
procedure verwijdervorige(string, string)
procedure vervang(integer Van, integer Naar, tgnst Van, tgnst Naar)
%procedure bondsnummerOfEmail(string,string,string,slist)
procedure merkenMet(string)
determ snoeiopm(puinS)

clauses

zoekDoor(0, "").
teller(0).
tellerV(0).

geensnoeien(Spelerslijst) :-
  member(Naam, SpelersLijst),
    sp2(SKeus,Spelersrt,Naam,Tel,VerhinderingenLijst,RatingE,RatingD,Betaald,WerkTijd, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,CheckGeg,Log,Res),
    verwijderVorige(Opm, Uit),
    not(Opm = Uit),
    logf('A', sp2(SKeus,Spelersrt,Naam,Tel,VerhinderingenLijst,RatingE,RatingD,Betaald,WerkTijd, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, Email, Uit,_Gezien,_RangPosE,_RangPosD,_Incasso,CheckGeg,Log,Res),ja),
  fail.
geensnoeien(_Spelerslijst) :-
  logclose(),
  compress(update),
  startDbRefresh(),
  _Answer = dlg_MessageBox( "Snoeien", "De markeringen in het opmerkingenveld zijn verwijderd!", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ).

snoeiopm(spmin(SKeus, OpmN)) :-
  sp2(SKeus,Spelersrt,Naam,Tel,VerhinderingenLijst,RatingE,RatingD,Betaald,WerkTijd, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,CheckGeg,Log,Res),
  format(Opm2, "snoei! %s..%s", OpmN, Opm),
  logf('A', sp2(SKeus,Spelersrt,Naam,Tel,VerhinderingenLijst,RatingE,RatingD,Betaald,WerkTijd, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, Email, Opm2,_Gezien,_RangPosE,_RangPosD,_Incasso,CheckGeg,Log,Res),ja),!.

snoeien(_Spelerslijst) :-
  Msg = "Misschien verouderde spelergegevens markeren met 'snoeien!' in het opmerkingenveld.\nJe kunt ze daarna met 'Speciale selectie' filteren op deze tekst en vervolgens verwijderen.\n\nDoorgaan?",
  2 = dlg_MessageBox( "Niet-spelerbestand snoeien", Msg, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
  !.
snoeien(Spelerslijst) :-
  cursor_SetWait(),
  assert(teller(0)),
  assert(tellerV(0)),
  get_tempDir(TDir, _TempBestand),
  filenamePath(InBestand, TDir, "snoeiin.txt"),
  openwrite(work, InBestand),
  writedevice(work),
  member(Naam, SpelersLijst),
    sp2(SKeus,_,Naam,_,_,_,_,_,_, _, _, _, Bondsnummer, _, _DS, _, _, _, Email, _,_,_,_,_,_,_,_),
    sp2(SKeus,Spelersrt,Naam,Tel,VerhinderingenLijst,RatingE,RatingD,Betaald,WerkTijd, Club, Adres, Woonpl, BondsNummer, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,CheckGeg,Log,Res),
    concat(Bondsnummer,Email,Tot),
    fronttoken(Tot,_,_),
    not(sp2opg(Skeus,_,_,_,_,_)),
    write("#", Bondsnummer,"%!",Email,"%!",SKeus),
    teller(N),
    N1 = N + 1,
    assert(teller(N1)),
    verwijdervorige(Opm,Opm1),
    not(Opm = Opm1),
    logf('A', sp2(SKeus,Spelersrt,Naam,Tel,VerhinderingenLijst,RatingE,RatingD,Betaald,WerkTijd, Club, Adres, Woonpl, BondsNummer, ES, DS, Gebdatum, Telwerk, Telmobiel, Email, Opm1,_Gezien,_RangPosE,_RangPosD,_Incasso,CheckGeg,Log,Res),ja),
  fail.	
snoeien(_Spelerslijst) :-
  closefile(work),
  get_tempDir(TDir, _TempBestand),
  filenamePath(InBestand, TDir, "snoeiin.txt"),
  %file_str(InBestand, Str),
  filenamePath(UitBestand, TDir, "snoeiuit.txt"),
  %write("\n",Inbestand),
  AAList = aanmeldenvars(),
  providerX(Prov, _, "", _),
  format(URL, "http://%s/tools/zeefmassamail1.php", Prov),  % nieuwe versie 2015.2
  concat(InBestand, ".gz", InBestandC),
  %file_str(InBestand, TSTr),
  %write("\n",TSTr),
  file_compress(InBestand, InBestandC),
  Return = fileupload(URL, InBestandC, Uitbestand, AAList), % compress
  %file_str(Uitbestand, Ttekst),
  %write("\n",TTekst),
  Return = 0,
  openread(work, Uitbestand),
  readdevice(work),
  repeat_f(work),
    readln(Line),
    %write("\n", Line),
    trap(term_str(puinS, Term, Line),_,fail),
    tellerV(N),
    snoeiopm(Term),
    N1 = N + 1,
    assert(tellerV(N1)),
    %write("\n- ", Naam),
  fail.
snoeien(_) :-
  closefile(work),
  logclose(),
  %teller(N),
  tellerV(N1),
  format(Msg, "Er zijn %u spelers gemarkeerd met 'snoeien!' in het opmerkingenveld.\nMaak daar een lijst van met 'Speciale selectie'\nen bekijk de opmerkingen.", N1),
  _Answer = dlg_MessageBox( "Spelerbestand schoonmaken", Msg, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  N1 > 2, !,
  compress(update),
  startDbRefresh().
snoeien(_).

verwijderVorige(Opm, Uit1) :-
  searchstring(Opm,"..",FP),
  Pos = FP + 1,
  frontstr(Pos,Opm,_StartString,Uit), 
  verwijderVorige(Uit, Uit1), !.
verwijderVorige(Opm, Uit1) :-
  searchstring(Opm,"||",FP),
  Pos = FP + 1,
  frontstr(Pos,Opm,_StartString,Uit), 
  verwijderVorige(Uit, Uit1), !.
verwijderVorige(Opm, Uit1) :-
  searchstring(Opm,"eden)",FP),
  Pos = FP + 4,
  frontstr(Pos,Opm,_StartString,Uit), 
  verwijderVorige(Uit, Uit1), !.
verwijderVorige(Opm, Opm).
   
gegevens_bijwerken([], _, _, _) :- !.
gegevens_bijwerken(NaamList, _, _, _) :-
  %autowebP(_, _, _, _, _, _, _, GeenVaste,_,_),
  %b_True = vragenOmverbinding(GeenVaste, b_False),
  checkvoortgang(0, "Spelerprofielen bijwerken"),
  checkvoortgang(1, "Versiecheck"),
  licentiecheck("BIJ", Reststring),
  currentNummer(Reststring, TNr, VerNr), !,
  gegevens_bijwerkenX(NaamList, b_False, TNr, VerNr). 
gegevens_bijwerken(_, _, _, _).

currentNummer(Tkst, TNr, VerNr) :-
  searchstring(Tkst,"toernooinr=",FoundPos),
  Fp = FoundPos + 10,
  frontstr(Fp, Tkst, _, T1),
  searchstring(T1, ";", FPos),
  Fpos1 = FPos - 1,
  frontstr(FPos1, T1, TNr, Tkst1),
  searchstring(Tkst1,"organisatie=",FoundPos1),
  Fp2 = FoundPos1 + 11,
  frontstr(Fp2, Tkst1, _, T2),
  searchstring(T2, ";", FPos2),
  Fpos3 = FPos2 - 1,
  frontstr(FPos3, T2, VerNr, _), !.
currentNummer(_, "", "").

gegevens_bijwerkenX(_NaamList, _, _TNr, _VerNr):-
	Title="Nieuwe versie",
	dlg_Note(Title,"functie werkt niet meer"), !.

sorteer(NaamList, 1, NaamListZ) :-
  naamList2NaamListMet(NaamList, NaamList1), !,
  sortstring_c(NaamList1, 1),
  naamList2NaamListZonder(NaamList1, NaamListZ).
sorteer(NaamList, _, NaamList).

naamList2NaamListMet([Naam|NaamList], [NaamMet | Uit]) :-
  sp2(_,_,Naam,_,_ ,_,_,_,_,_,_, WoonPl,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  frontstr(9, WoonPl, Postcode, _), !,
  concat(Postcode, Naam, NaamMet),
  naamList2NaamListMet(NaamList, Uit).
naamList2NaamListMet([Naam|NaamList], [NaamMet | Uit]) :-
  concat("         ", Naam, NaamMet), !,
  naamList2NaamListMet(NaamList, Uit).
naamList2NaamListMet(_, []).

naamList2NaamListZonder([Naam|NaamList], [NaamZonder|Uit]) :-
  frontstr(9, Naam, _, NaamZonder), !,
  naamList2NaamListZonder(NaamList, Uit).
naamList2NaamListZonder(_, []).

selectie2sexe(0,m):-!.
selectie2sexe(1,v).

selectSpsrt(MV,Sex) :-
  findall(Soort, spsrt(_, Soort, MV, _, _, _, _, _), Soorten),
  count(Soorten, 0, Aantal),
  Aantal > 1,
  dlg_ListSelect("Welke categorie?", Soorten, 1, StrSel, _Index),
  spsrt(Sex, StrSel, _, _, _, _, _, _), !.
selectSpsrt(MV,Sex) :-
  spsrt(Sex, _StrSel, MV, _, _, _, _, _), !.

sTitel(0, "Lijst van alle personen in bestand (F2)") :- !.
sTitel(1, "Lijst van ingeschreven deelnemers (F3)") :- !.
sTitel(2, "Niet passende spelersoorten") :- !.
sTitel(3, "Gegevens missen") :- !.
sTitel(4, "Formulier nog niet gezien") :- !.
sTitel(5, "Dubbele Knltb nummers") :- !.
sTitel(6, "Ontbrekende Knltb nummers") :- !.
sTitel(7, "Ongeldige Knltb nummers") :- !.
sTitel(8, "Niet-spelers") :- !.
sTitel(9, Titel) :- 
  selectedClub(Club),
  format(Titel, "Spelers van club %s", Club), !.
sTitel(10, "Uit geselecteerde onderdelen") :- !.
sTitel(16, "Speciale selectie") :- !.
sTitel(Mode, Titel) :-
  format(Titel, "Mode: %", Mode).

spelerselRefresh() :-
  spelerSelect(Win, _Mode1),
  win_PostEvent(Win,e_User(1, 0)), !.
spelerselRefresh().

opzoekbaar([Item|_], b_True) :-
  parsetabs(Item, List),
  List = [_,Naam | _],
  sp2(SpNo,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  sp2opg(SpNo, _, _, _, _, _),
  wedstrijd(_,_,SpNo,_,_,_), !.
opzoekbaar(_Items, b_False).

wisbaar([Item|_], b_False) :-
  parsetabs(Item, List),
  List = [_,Naam | _],
  sp2(SpNo,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  sp2opg(SpNo,_,_,_,_,_), !.
wisbaar(_Item, b_True).

invoerbaar1(0, b_True) :- !.
invoerbaar1(_, b_False).

merkenMet("Verjaarde bondsnummers merken") :-
  toernooiK(Tnr,_),
  wc(_,_,_,_,_,_,_,_,_,_,_,_,_,Tnr,_), !.
merkenMet("Verjaarde mailadressen merken").

popUpS(_Win, PNT) :-
  LboxWin = win_GetCtlHandle(_Win, idc_spelerselect_lbox),
  lbox_GetSel(LboxWin,ItemList,_IndexList),
  %ItemList = [Item|_],
  opzoekBaar(ItemList, Opz),
  spelerSelect(_Win, Mode),
  invoerbaar1(Mode, Invb),
  merkenMet(MerkTekst),
  Menu = dyn_menu([
    txt(idr_gegevensperpers,"&Gegevens",0,b_True,0,[]),
    txt(id_Speler_wedstrijden,"&Zoek op in schema",0,Opz,0,[]),
    txt(idt_synspeler,"&Speler met dezelfde bedoeling samenvoegen",0,b_True,0,[]),
    separator,
    txt(idr_Nieuwe_Speler,"&Nieuwe speler invoeren",0,Invb,0,[]),
    separator,
    %txt(id_Speler_selectie,"&Speciale selectie",0,b_True,0,[]),	% 31.7.2011
    txt(id_spelers_bijwerken,"Profielen updaten via KNLTB",0,b_True,0,[]),
    txt(id_spelers_snoeien,MerkTekst,0,b_True,0,[
      txt(id_spelers_snoeien,"Merktekens aanbrengen",0,b_True,0,[]),
      txt(id_spelers_geensnoeien,"Merktekens verwijderen",0,b_True,0,[])
    ]),
    txt(id_Speler_Checken_Nietspelers_verwijderen,"Alle spelers van deze lijst uit het bestand verwijderen tenzij deelnemer",0,b_True,0,[]),
    separator,
    txt(id_Speler_Uitvoer_email_aan_allen,"Bulk-email",0,b_True,0,[]),
    txt(idb_sms,"Bulk-SMS via email",0,b_True,0,[]),
    txt(id_Speler_lijsten,"Afdruklijst",0,b_True,0,[]),
    txt(id_Speler_Uitvoer_Adresgegevens_etiketten,"Etiketten",0,b_True,0,[]),
    txt(id_Speler_Uitvoer_Adresgegevens_mailmerge,"Mailmerge voor o.a. Excel",0,b_True,0,[]),
    txt(id_Speler_Uitvoer,"Uitvoerbestand",0,b_True,0,[])
    ]), !,
  menu_PopUp(_Win,Menu, PNT, align_Left), !.

subclassS_eh(_LWin,e_Char('\27',_ShiftCtlAlt),0):-!,
  Parent=win_GetParent(_LWin),
  win_Destroy(Parent).
subclassS_eh(_LWin,e_Char(Kar,_ShiftCtlAlt),0):-
  Parent=win_GetParent(_LWin),
  win_SendEvent(Parent,e_Char(Kar,_ShiftCtlAlt)), !.
subclassS_eh(_LWin,e_Keydown(k_ins, _X),0):-
  Parent=win_GetParent(_LWin),
  spelerSelect(Parent, Mode),
  invoerbaar1(Mode, Invb),
  Invb = b_True,
  win_SendEvent(Parent,e_Menu(idr_nieuwe_speler, 0)), !.

inLijst(Str, Letter) :-
  trap(subchar(Str,3,Char),_,true),
  upper_lower(Letter, Char).

zoekop(SKeus, Naam) :-
  findall(Wedstrijd,wed_opgest(SKeus,Wedstrijd),Wedstrijden),
  versten(Wedstrijden,Wedstrijden,Weds_V,CatL),
  sortstring_c(CatL, 1),  
  selectSpWeds(Naam,CatL, CatK),
  wedstr_member(Wedstrijd1,Weds_V),  	% kies de wedstrijd van die 
  Wedstrijd1 = w(WedNo, Cat1, _, _, _, TgnstX, _, _),
  frontstr(3, CatK, _, CatK1),		% strip merkje weer
  wc(Cat, _, _, _, _, CatK1, _, _, _, _, _, _, _, _, _),
  %wc(Cat,_CNaam,_,CatK1,_,_,_),          % categorie via backtrack
  Cat1 = Cat,
  retract(pos(Cat,Crd,_,Srt)),
  asserta(pos(Cat,Crd,WedNo,Srt)),
  select(Cat,WedNo,0,0,TgnstX), !.

zoekOp1(SKeus, Cat) :-
  findall(Wedstrijd,wed_opgest(SKeus,Wedstrijd),Wedstrijden),
  versten(Wedstrijden,Wedstrijden,Weds_V,_CatL),
  %sortstring_c(CatL, 1),  
  wedstr_member(Wedstrijd1,Weds_V),  	% kies de wedstrijd van die 
  Wedstrijd1 = w(WedNo, Cat1, _, _, _, TgnstX, _, _),
  Cat1 = Cat,
  retract(pos(Cat,Crd,_,Srt)),
  asserta(pos(Cat,Crd,WedNo,Srt)),
  select(Cat,WedNo,0,0,TgnstX), !.

timerStop(TimerId) :-
  TimerId <> 0,
  timer_Kill(TimerId), !.
timerStop(_).

save_SpSelWin() :-
  spelerSelect(Win, _Mode1),
  updateVensterpositie(spelersvenster, Win), !.
save_SpSelWin().

lijstPersonenTitel(Titel) :-
  spelerSelect(Win, _),
  Titel = win_GetText(Win), !.
lijstPersonenTitel("").

lijstPersoon(Persoon) :-
  spelerSelect(Win, _),
  LboxWin = win_GetCtlHandle(Win, idc_spelerselect_lbox),
  LboxList = lbox_GetAll(LboxWin),
  member(Item, LboxList),
  parsetabs(Item, List),
  List = [_, Persoon | _].

formulierVorig(No, I1, b_True) :-
  spelerSelect(Win, _),
  LboxWin = win_GetCtlHandle(Win, idc_spelerselect_lbox),
  Index = lbox_GetSelIndex(LboxWin),
  Index > 0,
  I1 = Index - 1,
  Item = lbox_GetItem(LboxWin, I1),
  parsetabs(Item, List),
  List = [_, Naam| _],
  sp2(No,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !.
formulierVorig(0, 0, b_False).

formulierVolgend(No, I1, b_True) :-
  spelerSelect(Win, _),
  LboxWin = win_GetCtlHandle(Win, idc_spelerselect_lbox),
  Index = lbox_GetSelIndex(LboxWin),
  NoOfItems = lbox_CountAll(LboxWin),
  Index < NoOfItems,
  I1 = Index + 1,
  Item = lbox_GetItem(LboxWin, I1),
  parsetabs(Item, List),
  List = [_, Naam | _],
  sp2(No,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !.
formulierVolgend(0, 0, b_False).

formulierSelect(Index) :-
  spelerSelect(Win, _),
  LboxWin = win_GetCtlHandle(Win, idc_spelerselect_lbox),
  lbox_SetSel(LboxWin, Index, b_True),
  win_PostEvent(Win,e_User(upd_toolb,0)), !.

lboxList2SpNo(LboxList, SpNo) :-
  member(Item, LboxList),
  parsetabs(Item, List),
  List = [_, Naam | _],
  sp2(SpNo,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_).

lboxList2SpNaam(LboxList, Naam) :-
  member(Item, LboxList),
  parsetabs(Item, List),
  List = [_, Naam | _].

checkSamensmeltenNietMogelijk(SKeus1, SKeus2, "Ze hebben verschillend geslacht.") :-
  sp2(SKeus1,Sex1,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  sp2(SKeus2,Sex2,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  spsrt(Sex1, _, Sexe1, _, _, _, _, _),  
  spsrt(Sex2, _, Sexe2, _, _, _, _, _),  
  not(Sexe1 = Sexe2).
checkSamensmeltenNietMogelijk(SKeus1, SKeus2, "Ze hebben verschillend bondsnummer.") :-
  sp2(SKeus1,_,_,_,_ ,_,_,_,_,_,_, _,BondsNr1,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  sp2(SKeus2,_,_,_,_ ,_,_,_,_,_,_, _,BondsNr2,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  proef11det(BondsNr1),
  proef11det(BondsNr2),
  BondsNr1 <> BondsNr2.
checkSamensmeltenNietMogelijk(SKeus1, SKeus2, "Ze hebben verschillende geboortedata.") :-
  sp2(SKeus1,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,GebDatum1, _,_,_,_,_,_,_,_,_,_,_),
  sp2(SKeus2,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,GebDatum2, _,_,_,_,_,_,_,_,_,_,_),
  frontchar(Gebdatum1, Char1, Rest1),
  frontchar(Gebdatum2, Char2, Rest2),
  zeefgebdatum(Rest1, Char1, b_True, "", Uit1), 
  zeefgebdatum(Rest2, Char2, b_True, "", Uit2), 
  fronttoken(Uit1, _, _),
  fronttoken(Uit2, _, _),
  Uit1 <> Uit2.

isLeadingZero('0', b_True).

zeefgebdatum(Rest, Char, Leading, In, Uit) :-
  Char <= '9',
  Char >= '0',
  not(isLeadingZero(Char, Leading)),
  frontchar(In1, Char, In),
  frontchar(Rest, Char1, Rest2), !,
  zeefgebdatum(Rest2, Char1, b_False, In1, Uit).
zeefgebdatum(Rest, _Char, _, In, Uit) :-
  frontchar(Rest, Char1, Rest2), !,
  zeefgebdatum(Rest2, Char1, b_True, In, Uit).
zeefgebdatum(_, _, _, In, In).

bepaalMaster(SKeus1, SKeus2, SKeus1, SKeus2) :- % master is met de hoogste gegevenscount
  gegevenscount(SKeus1, Count1),
  gegevenscount(SKeus2, Count2),
  Count1 >= Count2, !.
bepaalMaster(SKeus1, SKeus2, SKeus2, SKeus1).

gegevenAanwezig(String, In, Uit) :-
  fronttoken(String, _, _),
  Uit = In + 1, !.
gegevenAanwezig(_, In, In).

gegevenscount(SKeus, C6) :-
  sp2(SKeus,_Sex1,_Naam1,Telthuis,_Verh1,_RatingE1,_RatingD1,_Betaald1,_UitKant1, Club, Adres, _Woonpl1, BondsNr, _ES1, _DS1, Gebdatum, _Telwerk1, _Telmobiel1, EMail, _Opm1,_Gezien1,_RangPosE1,_RangPosD1,_Incasso1,_CheckGeg1,_Log1,_Import1),
  gegevenAanwezig(Club, 0, C1), !,
  gegevenAanwezig(BondsNr, C1, C2),
  gegevenAanwezig(Gebdatum, C2, C3),
  gegevenAanwezig(Email, C3, C4),
  gegevenAanwezig(Adres, C4, C5),
  gegevenAanwezig(Telthuis, C5, C6).
gegevenscount(_SKeus, 0).
  
checkRedenen(_, _, []) :- !.
checkRedenen(Naam1, Naam2, Redenen) :-
  list_to_string(Redenen, "\n", RedenS),
  format(Txt, "Samenvoegen %s en %s\nkan niet omdat:\n%s.", Naam1, Naam2, RedenS),
  dlg_MessageBox( "Spelers samensmelten", Txt, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.

kiesGegeven(In1, _In2, In1) :-
  fronttoken(In1, _, _), !.
kiesGegeven(_In1, In2, In2).

kiesverh(In, _, In) :-
  not(In = []), !.
kiesverh(_, In, In).

kiesbetaald(ja, _, ja) :- !.
kiesbetaald(_, ja, ja) :- !.
kiesbetaald(_, _, nee).

kiesuitkant(In1, In2, In1) :-
  In1 > In2, !.
kiesuitkant(_, In2, In2).

samenvoegen(Win, SKeus1, SKeus2) :-
  sp2(SKeus1,Sex1,Naam1,Telthuis1,Verh1,RatingE1,RatingD1,Betaald1,UitKant1, Club, Adres1, Woonpl1, BondsNr1, ES1, DS1, Gebdatum1, Telwerk1, Telmobiel1, EMail1, Opm1,Gezien1,RangPosE,RangPosD,Incasso1,CheckGeg1,Log1,Import1),
  sp2(SKeus2,_Sex2,_Naam2,Telthuis2,Verh2,RatingE2,RatingD2,Betaald2,UitKant2, _Club2, Adres2, Woonpl2, BondsNr2, ES2, DS2, Gebdatum2, Telwerk2, Telmobiel2, EMail2, Opm2,Gezien2,_RangPosE2,_RangPosD2,Incasso2,CheckGeg2,Log2,Import2),
  kiesgegeven(Telthuis1, Telthuis2, Telthuis),
  kiesverh(Verh1, Verh2, Verh),
  kiesgegeven(RatingE1, RatingE2, RatingE),
  kiesgegeven(RatingD1, RatingD2, RatingD),
  kiesbetaald(Betaald1, Betaald2, Betaald),
  kiesuitkant(Uitkant1, Uitkant2, Uitkant),
  kiesgegeven(Adres1, Adres2, Adres),
  kiesgegeven(Woonpl1, Woonpl2, Woonpl),
  kiesgegeven(BondsNr1, BondsNr2, BondsNr),
  kiesgegeven(ES1, ES2, ES),
  kiesgegeven(DS1, DS2, DS),
  kiesgegeven(Gebdatum1, Gebdatum2, Gebdatum),
  kiesgegeven(Telwerk1, Telwerk2, Telwerk),
  kiesgegeven(Telmobiel1, Telmobiel2, Telmobiel),
  kiesgegeven(Email1, Email2, Email),
  concat(Opm1, Opm2, Opm),
  kiesbetaald(Gezien1, Gezien2, Gezien),
  kiesgegeven(Incasso1, Incasso2, Incasso),
  kiesgegeven(CheckGeg1, CheckGeg2, CheckGeg),
  kiesgegeven(Log1, Log2, Log),
  %kiesgegeven(Res1, Res2, Res),
  append(Import1, Import2, Import3),
  SpelerIn = sp2(SKeus1,Sex1,Naam1,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import3),
  logf('A', sp2(SKeus1,Sex1,Naam1,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import3), nee),
  close_formulier(SKeus1),
  dlg_persoonsgegevens_Create(Win, SpelerIn ),
  not(sp2opg(SKeus2,_,_,_,_,_)),
  logf('R', sp2(SKeus2,_Sex2,_Naam2,Telthuis2,Verh2,RatingE2,RatingD2,Betaald2,UitKant2, _Club2, Adres2, Woonpl2, BondsNr2, ES2, DS2, Gebdatum2, Telwerk2, Telmobiel2, EMail2, Opm2,Gezien2,_RangPosE2,_RangPosD2,Incasso2,CheckGeg2,Log2,Import2), ja),
  spelerselrefresh(),
  !.
samenvoegen(_Win, _SKeus1, _SKeus2).

orderedTgnst(s(SKeus), s(SKeus)) :- !.
orderedTgnst(pw(SKeus), pw(SKeus)) :- !.
orderedTgnst(p(SKeus,Maat), p(T1, T2)) :- 
  sp2(SKeus,_S1,Naam1,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  sp2(Maat,_S2,Naam2,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  orde(Naam1,Naam2,SKeus,Maat, T1, T2), !.
orderedTgnst(_, onbekend).

changeAllTgnst(Cat, Tgnst, TgnstN) :-
  wd(Wnum,Cat,Tgnst,T2,Uitsl,Parm1,Note), 
  logf('Z', wd(Wnum,Cat,TgnstN,T2,Uitsl,Parm1,Note), ja),
  fail.
changeAllTgnst(Cat, Tgnst, TgnstN) :-
  wd(Wnum,Cat,T1,Tgnst,Uitsl,Parm1,Note), 
  logf('Z', wd(Wnum,Cat,T1,TgnstN,Uitsl,Parm1,Note), ja),
  fail.
changeAllTgnst(Cat, Tgnst, TgnstN) :-
  w3(WNum,Cat,Tgnst,PW,PV,Stat,T),					%  uitslag weg
  logf('Z', w3(WNum,Cat,TgnstN,PW,PV,Stat,T), ja),
  fail.
changeAllTgnst(Cat, Tgnst, TgnstN) :-
  ws(WNum, Cat, Tgnst, Tijd),
  logf('Z', ws(WNum, Cat, TgnstN, Tijd),ja),
  fail.
changeAllTgnst(_Cat, _Tgnst, _TgnstN).

vervang(SV, SN, s(SV), s(SN)) :- !.
vervang(SV, SN, p(SV, SM), p(SN, SM)) :- !.
vervang(SV, SN, p(SM, SV), p(SM, SN)) :- !.
vervang(SV, SN, pw(SV), pw(SN)) :- !.
vervang(_, _, In, In).

samenvoegenOpgaven(SKeus1, SpelerVan) :-
  sp2opg(SpelerVan,Cat,Opg,_MaatNo,_Plts,TgnstVan),
  opg(Opg,Cat,_Tgnst,Plts,Ronde,Uitsl,X,LLWC,TID,Stat,MT),
  logf('R', opg(Opg,Cat,TgnstVan,Plts,Ronde,Uitsl,X,LLWC,TID,Stat,MT), ja),
  not(sp2opg(SKeus1,Cat,_,_,_,_)),
  vervang(SpelerVan, SKeus1, TgnstVan,TgnstNaar),
  %wc(Cat, _, _, _, Mixed, _CatK1, _SexList, _, _, _, _, _, _, _, _),
  orderedTgnst(TgnstNaar, TgnstNaarUit),
  logf('Z', opg(Opg,Cat,TgnstNaarUit,Plts,Ronde,Uitsl,X,LLWC,TID,Stat,MT), ja),
  changeAllTgnst(Cat, TgnstVan, TgnstNaarUit),
  fail.
samenvoegenOpgaven(_SKeus1, SKeus2) :-
  logclose(),
  sp2opg(SKeus2,_Cat,_Opg,_MaatNo,_Plts,_Tgnst2),
  sp2(SKeus2,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  format(Txt, "Sommige opgaven van %s konden niet overgezet worden?\nDe spelergegevens zijn nog niet gewist.", Naam),
  dlg_MessageBox( "Spelers samensmelten", Txt, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  !.
samenvoegenOpgaven(_SKeus1, _SKeus2).

handlesynoniem(Win, SKeus, SpelerIn) :-
  not(synoniem(_)),
  dlg_persoonsgegevens_Create(Win, SpelerIn ),
  sp2(SKeus,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  format(Txt, "Gegevens van speler %s samenvoegen met die van een gelijkbetekenende speler (synoniem)\nomdat bij opgave dezelfde speler was bedoeld?\nKlik JA, selecteer de andere speler en klik weer op het (8+8) icoon.", Naam),
  1 = dlg_MessageBox( "Spelers samensmelten", Txt, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
  retractall(synoniem(_)),
  assert(synoniem(SKeus)), !.	
handlesynoniem(_Win, SKeus1, _SpelerIn) :-
  synoniem(SKeus2),
  SKeus1 <> SKeus2,
  sp2(SKeus1,_,Naam1,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  sp2(SKeus2,_,Naam2,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  findall(Boodschap, checkSamensmeltenNietMogelijk(SKeus1, SKeus2, Boodschap), Boodschappen),
  checkRedenen(Naam1, Naam2, Boodschappen),
  format(Txt, "Gegevens van speler \n%s\nsamensmelten met \n%s \n omdat bij opgave dezelfde speler is bedoeld?", Naam1, Naam2),
  1 = dlg_MessageBox( "Gelijkbetekenende spelers samensmelten", Txt, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
  bepaalMaster(SKeus1, SKeus2, SpelerNaar, SpelerVan),
  retractall(synoniem(_)),
  samenvoegenOpgaven(SpelerNaar, SpelerVan),
  samenvoegen(_Win, SpelerNaar, SpelerVan),
  logclose(), !.
handlesynoniem(_Win, _SKeus1, _SpelerIn) :-
  retractall(synoniem(_)).

close_spelersel() :-
  spelerselect(Win, _), !,
  win_Destroy(Win).
close_spelersel().

wisbaarL(SKeus, Lboxwin) :-
  LboxList = lbox_GetAll(LboxWin),
  member(Item, LboxList),
  parsetabs(Item, SList),
  SList = [_, Naam|_],
  sp2(SKeus,_Sex,Naam,_Telthuis,_Verh,_RatingE,_RatingD,_Betaald,_UitKant, _Club, _Adres, _Woonpl, _BondsNr, _ES, _DS, _Gebdatum, _Telwerk, _Telmobiel, _EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Import),
  not(sp2opg(SKeus,_,_,_,_,_)).

  win_spelersel_Create(_Parent, 16):-	% 0 = alle spelers 1 = deelnemers 16 = speciale selectie
	not(namenSelectieKrit(eerste,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)),
        dlg_spelergegevensuitvoer_Create(_Parent, [], eerste), !.
  win_spelersel_Create(_Parent, Mode):-	% 0 = alle spelers 1 = deelnemers 16 = speciale selectie
	spelerSelect(Win, Mode1),
	Mode = Mode1,
	win_BringToTop(Win), !.
  win_spelersel_Create(_Parent, ModeN):-
	retract(spelerSelect(Win, _)),
	assert(spelerSelect(Win, ModeN)),
	spelerselrefresh(),
	win_BringToTop(Win), !.
  win_spelersel_Create(_Parent, Mode):-
	loglock(lees, "Spelersel"),
	logclose(),
	Parm = cast(long, Mode),
	win_Create(win_spelersel_WinType,win_spelersel_RCT,win_spelersel_Title,
		   win_spelersel_Menu,_Parent,win_spelersel_Flags,win_spelersel_eh,Parm).

%BEGIN SpelerSel, e_Create
  win_spelersel_eh(_Win,e_Create(Parm),0):-
        NewFont = font_Create(ff_Helvetica, [], 10),	% 26.12.2011
  	win_SetFont(_Win, NewFont),
	Mode = cast(integer, Parm),
	assert(spelerSelect(_Win, Mode)),
%BEGIN SpelerSel, InitControls, 14:06:27-30.1.2017, Code automatically updated!
	win_CreateControl(wc_LBox,rct(94,20,310,184),"",_Win,[wsf_OwnerDraw,wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_HasStrings],idc_spelerselect_lbox),
%END SpelerSel, InitControls
%BEGIN SpelerSel, ToolbarCreate, 14:06:27-30.1.2017, Code automatically updated!
	tb_spelerselectb_Create(_Win),
	tb_spelerselect_Create(_Win),
%END SpelerSel, ToolbarCreate
	vensterpositie(spelersvenster, _Win),	% als bewaard van de vorige keer
	sTitel(Mode, Titel),
	win_SetText(_Win, Titel),
	findall(Naami, spelerVoorLijst(_Win, Naami, Mode), Personen),
	sortstring_c(Personen, 3),
	LboxWin = win_GetCtlHandle(_Win, idc_spelerselect_lbox),
  	win_SetFont(LboxWin, NewFont),
	lbox_Add(LboxWin, Personen),
	win_SetSubclassHandler(LboxWin,subclassS_eh,b_false),
	lbox_SetSel(LboxWin, 0, b_True),
	win_PostEvent(_Win,e_User(upd_toolb, 0)),
	!.
%END SpelerSel, e_Create
%MARK SpelerSel, new events

%BEGIN SpelerSel, id_Speler_Uitvoer_email_aan_allen
  win_spelersel_eh(_Win,e_Menu(id_Speler_Uitvoer_email_aan_allen,_ShiftCtlAlt),0):-
	LboxWin = win_GetCtlHandle(_Win, idc_spelerselect_lbox),
	lbox_GetSel(LboxWin,ItemList,_IndexList),
	ItemList = [Item|_],
  parsetabs(Item, List),
  List = [_,Naam | _],
        sp2(SKeus,_Sex,Naam,_Telthuis,_Verh,_RatingE,_RatingD,_Betaald,_UitKant, _Club, _Adres, _Woonpl, BondsNr, _ES, _DS, _Gebdatum, _Telwerk, _Telmobiel, EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Import),
	format(Msg, "1 mail sturen naar de geselecteerde persoon\n%s ?\nNee = naar iedereen in de lijst.", Naam),
	1 = dlg_MessageBox( "Bulkmail", Msg, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
        emailuitvoerEnkel(_Win,SKeus,Naam, Email, Bondsnr),
	!.
%END SpelerSel, id_Speler_Uitvoer_email_aan_allen

%BEGIN SpelerSel, id_Speler_Oproepen_lijst
  win_spelersel_eh(_Win,e_Menu(id_Speler_Oproepen_lijst,_ShiftCtlAlt),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idc_spelerselect_lbox),
	LboxList = lbox_GetAll(LboxWin),
	findall(SpNo, lboxList2SpNo(LboxList, SpNo), SpelerNos),
	TaskWin = vpi_GetTaskWin(),
	win_waarschuwinvoer_Create(TaskWin, SpelerNos),
	!.	% 22.10.2010 !!!
%END SpelerSel, id_Speler_Oproepen_lijst

%BEGIN SpelerSel, idb_sms
  win_spelersel_eh(_Win,e_Menu(idb_sms,_ShiftCtlAlt),0):-
  	smsbulkuitvoer(_Win),
	!.
%END SpelerSel, idb_sms

%BEGIN SpelerSel, idt_synspeler
  win_spelersel_eh(_Win,e_Menu(idt_synspeler,_ShiftCtlAlt),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idc_spelerselect_lbox),
	lbox_GetSel(LboxWin,ItemList,_IndexList),
	ItemList = [Item|_],
  parsetabs(Item, List),
  List = [_,Naam | _],
        sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
        SpelerIn = sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
	handlesynoniem(_Win,SKeus, SpelerIn),
	!.
%END SpelerSel, idt_synspeler

%BEGIN SpelerSel, idb_wis
  win_spelersel_eh(_Win,e_Menu(idb_wis,_ShiftCtlAlt),0):-
	LboxWin = win_GetCtlHandle(_Win, idc_spelerselect_lbox),
	lbox_GetSel(LboxWin,ItemList,IndexList),
	ItemList = [Item|_],
  parsetabs(Item, List),
  List = [_,Naam | _],
        sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
        mess(Import, Txt),
	1 = dlg_MessageBox( Naam, Txt, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
	close_Formulier(SKeus),
	IndexList = [Index|_],
	lbox_Delete(LboxWin, Index),
	lbox_SetSel(LboxWin, Index, b_True),
	win_PostEvent(_Win,e_User(upd_toolb, 0)),
        logf('R',sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),nee),
	!.
%END SpelerSel, idb_wis

%BEGIN SpelerSel, e_Timer
  win_spelersel_eh(_Win,e_Timer(TimerId),0):-
	timerStop(TimerId),
	assert(zoekDoor(0, "")),
	!.
%END SpelerSel, e_Timer

%BEGIN SpelerSel, e_KeyDown
  win_spelersel_eh(_Win,e_KeyDown(k_ins,c_Nothing),0):-
	spelerSelect(_Win, Mode),
	invoerbaar1(Mode, Invb),
	Invb = b_True,
	win_SendEvent(_Win,e_Menu(idr_nieuwe_speler, 0)),
  	!.
%END SpelerSel, e_KeyDown

%BEGIN SpelerSel, idr_nieuwe_speler
  win_spelersel_eh(_Win,e_Menu(idr_nieuwe_speler,_ShiftCtlAlt),0):-!,
	Answer = dlg_Ask("Nieuwe speler","Man of Vrouw?",["&Man","&Vrouw","&Cancel"]),
	selectie2sexe(Answer,MV),
	selectSpsrt(MV,Sex),
	Answer <> 2,
	getbacktrack(Here),
	loglock(lees, "Spelersel"),
	logclose(),
	repeat,
	random(30000,Y),       % bepaal vrij nummer (tot 3000 ivm 16 bits compat)
	SpKeus = Y + 1,
	not(sp2(SpKeus,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)),
	%not(sp1(SpKeus,_,_,_,_,_,_,_, _, _, _, _, _, _, _,_,_,_,_)),
	cutbacktrack(Here),
	%dlg_persoonsgegevens_Create(_Win, speler(SpKeus, Sex, "", "", [], [], omheteven, 0,
	%	    "","","","", "","","","","","","","")),
	SpelerIn = sp2(SpKeus,Sex,"","",[],"","",nee,0, "", "", "", "", "", "", "", "", "", "", "",nee,"","","","","",[]),
	dlg_persoonsgegevens_Create(_Win, SpelerIn),
	win_PostEvent(_Win,e_User(1, 0)),
	!.
%END SpelerSel, idr_nieuwe_speler

%BEGIN SpelerSel, id_help
  win_spelersel_eh(_Win,e_Menu(id_help,_ShiftCtlAlt),0):-!,
	project_ShowHelpContext("Spelerslijst"),
	!.
%END SpelerSel, id_help

%BEGIN SpelerSel, e_User
  win_spelersel_eh(_Win,e_User(1,_Ptr),0):-		% refresh
	spelerSelect(_Win, Mode),
	sTitel(Mode, Titel),
	win_SetText(_Win, Titel),
	findall(Naami, spelerVoorLijst(_Win, Naami, Mode), Personen),
	sortstring_c(Personen, 3),
	LboxWin = win_GetCtlHandle(_Win, idc_spelerselect_lbox),
	LboxList = lbox_GetAll(LboxWin),
	not(LboxList = Personen),
	findall(SpNo, lboxList2SpNo(Personen, SpNo), SpelerNos),
	updateWaarschuwVoorselectie(SpelerNos),
	lbox_GetSel(LboxWin,ItemList,_IndexList),
	lbox_Suspend(LboxWin),
	lbox_Clear(LboxWin),
	lbox_Add(LboxWin, Personen),
	lbox_Resume(LboxWin),
	win_PostEvent(_Win,e_User(upd_toolb, 0)),
	ItemList = [Item|_],
	parsetabs(Item, List),
	List = [_, Naam | _],
	searchslist_c(Personen, Naam, 3, FoundPos),
	lbox_SetSel(LboxWin, FoundPos, b_True),
	!.
    win_spelersel_eh(_Win,e_User(upd_toolb,_Ptr),0):-
	toolbar_SetValue(_Win, id_Speler_wedstrijden, ctrl_value(0,1)),
	LboxWin = win_GetCtlHandle(_Win, idc_spelerselect_lbox),
	NoOfItems = lbox_CountAll(LboxWin),
	format(No, "%u personen in de lijst", NoOfItems),
	toolbar_SetValue(_Win, idt_s1, text_value(No)),
	lbox_GetSel(LboxWin,ItemList,_IndexList),
	%ItemList = [Item|_],
	wisbaar(ItemList, Wisb),	
	toolbar_SetValue(_Win, idb_Wis, ctrl_value(Wisb,1)),
	opzoekbaar(ItemList, Opz),
	Opz = b_True,
	toolbar_SetValue(_Win, id_Speler_wedstrijden, ctrl_value(1,1)),
	fail.
  win_spelersel_eh(_Win,e_User(upd_toolb,_Ptr),0):-
	toolbar_SetValue(_Win, idr_Nieuwe_Speler, ctrl_value(0,1)),
	toolbar_SetValue(_Win, idt_synspeler, ctrl_value(0,1)),
	spelerSelect(_Win, 0),
	toolbar_SetValue(_Win, idr_Nieuwe_Speler, ctrl_value(1,1)),
	toolbar_SetValue(_Win, idt_synspeler, ctrl_value(1,1)),
	fail.
  win_spelersel_eh(_Win,e_User(upd_toolb,_Ptr),0):-
	toolbar_SetValue(_Win, id_Speler_Oproepen_lijst, ctrl_value(0,1)),
	erIsEenPlanning(),
	toolbar_SetValue(_Win, id_Speler_Oproepen_lijst, ctrl_value(1,1)),
	fail.
  win_spelersel_eh(_Win,e_User(upd_toolb,_Ptr),0):-
	toolbar_SetValue(_Win, id_Speler_Betalingen_boeken, ctrl_value(0,1)),
	betaal_adm(ja),
	getbacktrack(Here),
	opg(_N, Cat, T, _,_,_,_,_,_,_,_),
	in_weds(Cat, T),
	cutbacktrack(Here), !,
	toolbar_SetValue(_Win, id_Speler_Betalingen_boeken, ctrl_value(1,1)).
%END SpelerSel, e_User

%BEGIN SpelerSel, idc_spelerselect_lbox selchanged
  win_spelersel_eh(_Win,e_Control(idc_spelerselect_lbox,_CtrlType,_LboxWin,selchanged),0):-
	win_PostEvent(_Win,e_User(upd_toolb, 0)),
	!.
%END SpelerSel, idc_spelerselect_lbox selchanged

%BEGIN SpelerSel, e_MouseDown
  win_spelersel_eh(_Win,e_MouseDown(PNT,_ShiftCtlAlt,mouse_button_right),0):-!,
	PNT = cursor_GetPos(_Win),
	popUpS(_Win, PNT),
	!.
%END SpelerSel, e_MouseDown

%BEGIN SpelerSel, idc_spelerselect_lbox activated
  win_spelersel_eh(_Win,e_Control(idc_spelerselect_lbox,_CtrlType,_CtrlWin,activated),0):-
	spelerSelect(_Win, 1),
	LboxWin = win_GetCtlHandle(_Win, idc_spelerselect_lbox),
	lbox_GetSel(LboxWin,ItemList,_IndexList),
	%ItemList = [Item|_],
	opzoekbaar(ItemList, Opz),
	Opz = b_True,
	win_PostEvent(_Win,e_Menu(id_Speler_wedstrijden,0)),
	!.
  win_spelersel_eh(_Win,e_Control(idc_spelerselect_lbox,_CtrlType,_CtrlWin,activated),0):-
	win_PostEvent(_Win,e_Menu(idr_gegevensperpers,0)),
	!.
%END SpelerSel, idc_spelerselect_lbox activated

%BEGIN SpelerSel, idr_gegevensperpers
  win_spelersel_eh(_Win,e_Menu(idr_gegevensperpers,_ShiftCtlAlt),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idc_spelerselect_lbox),
	lbox_GetSel(LboxWin,ItemList,_IndexList),
	ItemList = [Item|_],
	parsetabs(Item, List),
	List = [_,Naam | _],
        sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
        SpelerIn = sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
	dlg_persoonsgegevens_Create(_Win, SpelerIn ),
	!.
%END SpelerSel, idr_gegevensperpers

%BEGIN SpelerSel, id_Speler_lijsten
  win_spelersel_eh(_Win,e_Menu(id_Speler_lijsten,_ShiftCtlAlt),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idc_spelerselect_lbox),
	LboxList = lbox_GetAll(LboxWin),
	findall(SpNo, lboxList2SpNo(LboxList, SpNo), SpelerNos),
	spelerselect(_Win, Mode),
	Titel = win_GetText(_Win),
	dlg_attributen_spelerlijst_Create(_Win, SpelerNos, Titel, Mode),	% 7.8.2002
	!.
%END SpelerSel, id_Speler_lijsten
  win_spelersel_eh(_Win,e_Menu(id_Speler_Uitvoer,_ShiftCtlAlt),0):-
	LboxWin = win_GetCtlHandle(_Win, idc_spelerselect_lbox),
	LboxList = lbox_GetAll(LboxWin),
	findall(Naam, lboxList2SpNaam(LboxList, Naam), NaamList),
	dlg_speleruitvoer2_Create(_Win, Kom, Kol, XML),
	verwerk(0, NaamList, Kom, Kol, XML),
	!.
  win_spelersel_eh(_Win,e_Menu(idb_sms,_ShiftCtlAlt),0):-
  	smsbulkuitvoer(_Win),
	!.
  win_spelersel_eh(_Win,e_Menu(id_Speler_Uitvoer_Adresgegevens_etiketten,_ShiftCtlAlt),0):-
	LboxWin = win_GetCtlHandle(_Win, idc_spelerselect_lbox),
	LboxList = lbox_GetAll(LboxWin),
	findall(Naam, lboxList2SpNaam(LboxList, Naam), NaamList),
	dlg_adresetiketten_Create(_Win, Orde),
	sorteer(NaamList, Orde, NaamList1),
        etikettenAdres1(NaamList1,1),
	!.
  win_spelersel_eh(_Win,e_Menu(id_Speler_Uitvoer_Adresgegevens_mailmerge,_ShiftCtlAlt),0):-
	LboxWin = win_GetCtlHandle(_Win, idc_spelerselect_lbox),
	LboxList = lbox_GetAll(LboxWin),
	findall(Naam, lboxList2SpNaam(LboxList, Naam), NaamList),
        etikettenAdres1(NaamList,2),
	!.
  win_spelersel_eh(_Win,e_Menu(id_spelers_bijwerken,_ShiftCtlAlt),0):-
	LboxWin = win_GetCtlHandle(_Win, idc_spelerselect_lbox),
	LboxList = lbox_GetAll(LboxWin),
	findall(NaamM, lboxList2SpNaam(LboxList, NaamM), NaamList),
        gegevens_bijwerken(NaamList, ja, b_False, ja), % nrdialoog, verbose, vragen om verbinding
	!.								% op twee plaatsen!
  win_spelersel_eh(_Win,e_Menu(id_spelers_snoeien,_ShiftCtlAlt),0):- % markeer verouderde spelers
	LboxWin = win_GetCtlHandle(_Win, idc_spelerselect_lbox),
	LboxList = lbox_GetAll(LboxWin),
	findall(NaamM, lboxList2SpNaam(LboxList, NaamM), NaamList),
	snoeien(NaamList), !.
  win_spelersel_eh(_Win,e_Menu(id_spelers_geensnoeien,_ShiftCtlAlt),0):- % ontmarkeer verouderde spelers
	LboxWin = win_GetCtlHandle(_Win, idc_spelerselect_lbox),
	LboxList = lbox_GetAll(LboxWin),
	findall(NaamM, lboxList2SpNaam(LboxList, NaamM), NaamList),
	geensnoeien(NaamList), !.
  win_spelersel_eh(_Win,e_Menu(id_spelers_bijwerken,_ShiftCtlAlt),0):-
	LboxWin = win_GetCtlHandle(_Win, idc_spelerselect_lbox),
	LboxList = lbox_GetAll(LboxWin),
	findall(NaamM, lboxList2SpNaam(LboxList, NaamM), NaamList),
        gegevens_bijwerken(NaamList, ja, b_False, ja), % nrdialoog, verbose, vragen om verbinding
	!.								% op twee plaatsen!
  win_spelersel_eh(_Win,e_Menu(id_Speler_Checken_Nietspelers_verwijderen,_ShiftCtlAlt),0):-
	LboxWin = win_GetCtlHandle(_Win, idc_spelerselect_lbox),
        findall(Spw, wisbaarL(Spw, Lboxwin), SwL),
        count(Swl, 0, SpwN),
        SpwN = 0, !,
        format(Txt, "Er zijn geen spelers in deze lijst die gewist kunnen worden!\nMaak eventueel een andere lijst."),
	dlg_MessageBox( "Speler wissen", Txt, mesbox_iconinformation, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ).
  win_spelersel_eh(_Win,e_Menu(id_Speler_Checken_Nietspelers_verwijderen,_ShiftCtlAlt),0):-
	LboxWin = win_GetCtlHandle(_Win, idc_spelerselect_lbox),
        findall(Spw, wisbaarL(Spw, Lboxwin), SwL),
        count(Swl, 0, SpwN),
        format(Txt, "Er zijn %u spelers in deze lijst die gewist kunnen worden omdat\nze nergens aan meedoen. Spelers en bijbehorende gegevens wissen?", SpwN),
	1 = dlg_MessageBox( "Speler wissen", Txt, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
	wisbaarL(SKeus, LboxWin),
        sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
	not(sp2opg(SKeus,_,_,_,_,_)),
	close_Formulier(SKeus),
        logf('R',sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),ja),
	fail.
  win_spelersel_eh(_Win,e_Menu(id_Speler_Checken_Nietspelers_verwijderen,_ShiftCtlAlt),0) :-
  	logclose(),
	compress(update),
  	startDbRefresh(),
        win_PostEvent(_Win,e_User(1, 0)), !.
%BEGIN SpelerSel, id_Speler_wedstrijden
  win_spelersel_eh(_Win,e_Menu(id_Speler_wedstrijden,_ShiftCtlAlt),0):-
	LboxWin = win_GetCtlHandle(_Win, idc_spelerselect_lbox),
	%ItemList = lbox_GetAll(LboxWin),
	lbox_GetSel(LboxWin,ItemList,_IndexList),
	ItemList = [Item|_],
  parsetabs(Item, List),
  List = [_,Naam | _],
        sp2(SKeus,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
	zoekop(SKeus, Naam),
	!.
  win_spelersel_eh(_Win,e_Menu(id_Speler_wedstrijden,_ShiftCtlAlt),0) :- !.
%END SpelerSel, id_Speler_wedstrijden

%BEGIN SpelerSel, idts_popup
  win_spelersel_eh(_Win,e_Menu(idts_popup,_ShiftCtlAlt),0):-!,
	PNT = cursor_GetPos(_Win),
	popUpS(_Win, PNT),
	!.
%END SpelerSel, idts_popup

%BEGIN SpelerSel, e_GetFocus
  win_spelersel_eh(_Win,e_GetFocus,0):-!,
	LboxWin = win_GetCtlHandle(_Win, idc_spelerselect_lbox),
	win_SetFocus(LboxWin),
	!.
%END SpelerSel, e_GetFocus

%BEGIN SpelerSel, e_Char
  win_spelersel_eh(_Win,e_Char(Getal,_ShiftCtlAlt),0):-
	LboxWin = win_GetCtlHandle(_Win, idc_spelerselect_lbox),
	LboxList = lbox_GetAll(LboxWin),
	%NoOfItems = lbox_CountAll(LboxWin),
	Teken = cast(char, Getal),
	str_char(TStr, Teken),
	upper_lower(TStrU, TStr),
	TStrU >= "0", %"A",
	TStrU <= "Z",
	zoekDoor(Timer, String),
	timerStop(Timer),
	NewTimerId = timer_Set(_Win, 1000),
	concat(String, TStrU, ZoekStr),
	assert(zoekDoor(NewTimerId, ZoekStr)),
	searchslist_c(LboxList, ZoekStr, 3, NewPos),
	lbox_SetSel(LboxWin, NewPos, b_True),
	win_PostEvent(_Win,e_User(upd_toolb, 0)), !.
%END SpelerSel, e_Char

%BEGIN SpelerSel, e_Destroy
  win_spelersel_eh(_Win,e_Destroy,0):-!,
	retract(spelerSelect(_Win, _)),
	updateVensterpositie(spelersvenster, _Win),
	!.
%END SpelerSel, e_Destroy

%BEGIN SpelerSel, e_OwnerMeasureItem
  win_spelersel_eh(_Win,e_OwnerMeasureItem(_CtlType,_CtlId,_ItemId,_Data),Size):-
	win_GetTextExtent(_Win, "JansenenTilanusg", -1, Width, He),
	Height = He,
	bitleft(Height, 16, H1),
	Size = Width + H1,
	!.
%END SpelerSel, e_OwnerMeasureItem

%BEGIN SpelerSel, e_OwnerDraw
  win_spelersel_eh(_Win,e_OwnerDraw(_CtlType,_CtlId,_ItemId,_ItemAction,
		_ItemState,LboxWin,_RectItem,_ItemData),0):-
	own_member1(_ItemState, odstate_Selected), !,  % was selected
	win_SetPen(LboxWin,pen(1,ps_Solid,color_Blue)),
	win_SetForeColor(LboxWin, color_White),
	win_SetBackColor(LboxWin, color_Blue),
	win_SetBrush(LboxWin,brush(pat_Solid, color_Blue)),
	sOwnerdraw(Lboxwin, _RectItem, _ItemId),
	!.
  win_spelersel_eh(_Win,e_OwnerDraw(_CtlType,_CtlId,_ItemId,_ItemAction,
		_ItemState,LboxWin,_RectItem,_ItemData),0):-
	_Itemstate = [],
	win_SetPen(LboxWin,pen(1,ps_Solid,color_White)),
	draw_Rect(LboxWin,_RectItem),
	win_SetForeColor(LboxWin, color_Black),
	win_SetBackColor(LboxWin, color_White),
	sOwnerdraw(Lboxwin, _RectItem, _ItemId),
	!.
%END SpelerSel, e_OwnerDraw

%BEGIN SpelerSel, e_Size
  win_spelersel_eh(_Win,e_Size(_Width,_Height),0):-!,
ifdef use_tbar
	toolbar_Resize(_Win),
enddef
	LboxWin = win_GetCtlHandle(_Win, idc_spelerselect_lbox),
	toolbar_GetRect(_Win,tb_left,RCTL),
	RCTL = rct(_,_,R,_),
	toolbar_GetRect(_Win,tb_bottom,RCTB),
	RCTB = rct(_,T,_,B),
	H1 = _Height - B + T,
	%RCT = win_GetClientRect( _Win ),
	%H = Height - B + T,
	win_Move(LboxWin,rct(R,0,_Width,H1)),
	win_Invalidate(LboxWin),
	!.
%END SpelerSel, e_Size

%BEGIN SpelerSel, e_Menu, Parent window 
  win_spelersel_eh(Win,e_Menu(ID,CAS),0):-!,
	PARENT = win_GetParent(Win),
	win_SendEvent(PARENT,e_Menu(ID,CAS)),
	!.
%END SpelerSel, e_Menu, Parent window

%END_WIN SpelerSel

sOwnerdraw(Lboxwin, RectItem, ItemId) :-
  draw_Rect(LboxWin,RectItem),
  Item = lbox_GetItem(LboxWin, ItemId),
  parsetabs(Item, List),
  List = [It1, It2 | _],
  win_GetTextExtent(LboxWin, "W ", -1, Width, _He),
  RectItem = rct(L, T, R, B),
  draw_TextInRect(LboxWin, rct(L, T, Width, B), It1, -1, [dtext_center]),
  draw_TextInRect(LboxWin, rct(Width, T, R, B), It2, -1, [dtext_left]),
  List = [_, _, It3 | _],
  draw_TextInRect(LboxWin, rct(200, T, R, B), It3, -1, [dtext_left]), !.
sOwnerdraw(_Lboxwin, _RectItem, _ItemId).
  

erIsEenPlanning() :-
  w2(_No,_Cat,_Tijd1,_G1,_G2,_,_,_), !.
erIsEenPlanning() :-
  ws(_No, _Cat, _Tgnst, _Tijd), !.

%BEGIN_TLB Spelerselect, 10:37:34-26.12.2011, Code automatically updated!
/**************************************************************************
	Creation of toolbar: Spelerselect
**************************************************************************/

clauses

  tb_spelerselect_Create(_Parent):-
ifdef use_tbar
	toolbar_create(tb_left,0xC0C0C0,_Parent,
		[tb_ctrl(idts_popup,pushb,idb_menu,idb_menud,idb_grijs,"Menu;gegevensuitvoer etc.",1,1),
		 tb_ctrl(idr_gegevensperpers,pushb,idb_spelerw,idb_spelerd,idb_grijs,"Spelergegevens;Selecteer spelergegevens",1,1),
		 tb_ctrl(idr_nieuwe_speler,pushb,idb_nwspw,idb_nieuwe_spelerd,idb_grijs,"Invoer nieuwe persoon (Insert);Invoer nieuwe speler",1,1),
		 tb_ctrl(idt_synspeler,pushb,idb_synspelerw,idb_syspelerd,idb_grijs,"Zelfde speler bedoeld (synoniemen);Gelijkbedoelde spelers  lijmen",1,1),
		 tb_ctrl(idb_wis,pushb,idb_wisw,idb_wiswd,idb_grijs,"Wis speler tenzij deelnemer;Wis speler tenzij deelnemer",1,1),
		 tb_ctrl(id_Speler_wedstrijden,pushb,idb_zoekopw,idb_zoekopp,idb_grijs,"Zoek speler op in schema;Wijs speler aan in schema",1,1),
		 tb_ctrl(id_Speler_Oproepen_lijst,pushb,idb_waarsw,idb_waarswd,idb_grijs,"Roep op (F6);Spelers oproepen (informeren) F6",1,1),
		 tb_ctrl(id_Speler_Uitvoer_email_aan_allen,pushb,idb_emailw,idb_emaild,idb_grijs,"Email bulk; email naar alle personen uit de lijst die een emailadres hebben",1,1),
		 tb_ctrl(idb_sms,pushb,idb_smsw,idb_smsd,idb_grijs,"Sms bulk;Bulk-sms via email",1,1),
		 tb_ctrl(id_Speler_lijsten,pushb,idb_printw,idb_printdown,idb_grijs,"Afdruk (lijst);Lijsten maken",1,1)]),
enddef
	true.
%END_TLB Spelerselect



%BEGIN_DLG Paswoord
/**************************************************************************
	Creation and event handling for dialog: Paswoord
**************************************************************************/

constants

%BEGIN Paswoord, CreateParms, 10:49:32-12.6.2002, Code automatically updated!
  dlg_paswoord_ResID = idd_paswoord
  dlg_paswoord_DlgType = wd_Modal
  dlg_paswoord_Help = idh_paswoord_wachtwoord
%END Paswoord, CreateParms

predicates

  dlg_paswoord_eh : EHANDLER
  dlg_paswoord_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_paswoord_update(DIALOG_VAL_LIST)
  verstek(string)

clauses

verstek(Wachtwoord) :-
  envsymbol("TASWW",Wachtwoord), !.

  dlg_paswoord_Create(_Parent, idc_ok):-
	not(verstek(_)), !.
  dlg_paswoord_Create(_Parent, idc_ok):-
	verstek(WW),
	not(fronttoken(WW,_,_)), !.
  dlg_paswoord_Create(_Parent, idc_ok):-
	verstek(WW),
	upper_lower(WW,"0000"), !.
  dlg_paswoord_Create(Parent, ANSWER):-

	IDC_PASWOORD_1_VALUE = "",
%MARK Paswoord, new variables

	dialog_CreateModal(Parent,dlg_paswoord_ResID,"",
  		[
%BEGIN Paswoord, ControlList, 10:49:32-12.6.2002, Code automatically updated!
		df(idc_paswoord_1,editstr(IDC_PASWOORD_1_VALUE,[]),nopr)
%END Paswoord, ControlList
		],
		dlg_paswoord_eh,0,VALLIST, ANSWER),
	dlg_paswoord_handle_answer(ANSWER,VALLIST).

  dlg_paswoord_handle_answer(idc_ok,VALLIST):-!,
	dlg_paswoord_update(VALLIST).
  dlg_paswoord_handle_answer(idc_cancel,_):-!.  % Handle Esc and Cancel here
  dlg_paswoord_handle_answer(_,_):-
	errorexit().

  dlg_paswoord_update(_VALLIST):-
%BEGIN Paswoord, Update controls, 10:49:32-12.6.2002, Code automatically updated!
	_IDC_PASWOORD_1_VALUE = dialog_VLGetstr(idc_paswoord_1,_VALLIST),
%END Paswoord, Update controls
	true.

%MARK Paswoord, new events

%BEGIN Paswoord, idc_help _CtlInfo
  dlg_paswoord_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Paswoordwachtwoord"),
	!.
%END Paswoord, idc_help _CtlInfo

%BEGIN Paswoord, idc_ok _CtlInfo
  dlg_paswoord_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	verstek(WW),
	CtrlWin = win_GetCtlHandle(_Win, idc_paswoord_1),
	WW1 = win_GetText(CtrlWin),
	WW <> WW1,
	!.
%END Paswoord, idc_ok _CtlInfo

  dlg_paswoord_eh(_,_,_):-!,fail.

%END_DLG Paswoord

%BEGIN_TLB Betalingen, 10:36:37-23.8.2011, Code automatically updated!
/**************************************************************************
	Creation of toolbar: Betalingen
**************************************************************************/

clauses

  tb_betalingen_Create(_Parent):-
ifdef use_tbar
	toolbar_create(tb_bottom,0xC0C0C0,_Parent,
		[tb_ctrl(idts_popup,pushb,idb_instellingenq,idb_instellingendown,idb_grijs,"Instellingen;Instellingen",1,1),
		 tb_ctrl(id_Onderdeel_print,pushb,idb_printw,idb_printdown,idb_grijs,"Afdruk (lijst);Betalingslijst afdrukken",1,1),
		 tb_text(idt_s1,tb_static,130,0,4,10,0x0,""),
		 tb_text(idt_2,tb_static,100,0,4,10,0x0,"Niet bevestigen")]),
enddef
	true.
%END_TLB Betalingen








%BEGIN_DLG Adresetiketten
/**************************************************************************
	Creation and event handling for dialog: Adresetiketten
**************************************************************************/

constants

%BEGIN Adresetiketten, CreateParms, 22:04:31-27.10.2011, Code automatically updated!
  dlg_adresetiketten_ResID = idd_adresetiketten
  dlg_adresetiketten_DlgType = wd_Modal
  dlg_adresetiketten_Help = idh_adresetiketten
%END Adresetiketten, CreateParms

predicates

  dlg_adresetiketten_eh : EHANDLER
  dlg_adresetiketten_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST, boolean Volgorde)
  dlg_adresetiketten_update(DIALOG_VAL_LIST, boolean Volgorde)

clauses

  dlg_adresetiketten_Create(Parent, Order):-

	ALFA = idc_alfabetisch,
%MARK Adresetiketten, new variables

	dialog_CreateModal(Parent,dlg_adresetiketten_ResID,"",
  		[
%BEGIN Adresetiketten, ControlList, 22:04:31-27.10.2011, Code automatically updated!,
		df(ALFA,radiobuttongroup([idc_alfabetisch,idc_postcode]),nopr)
%END Adresetiketten, ControlList
		],
		dlg_adresetiketten_eh,0,VALLIST,ANSWER),
	dlg_adresetiketten_handle_answer(ANSWER,VALLIST,Order).

  dlg_adresetiketten_handle_answer(idc_ok,VALLIST,Order):-!,
	dlg_adresetiketten_update(VALLIST,Order).
  dlg_adresetiketten_handle_answer(idc_cancel,_,b_False):-!.  % Handle Esc and Cancel here
  dlg_adresetiketten_handle_answer(_,_,_):-
	errorexit().

  dlg_adresetiketten_update(_VALLIST, b_false):-
%BEGIN Adresetiketten, Update controls, 22:04:31-27.10.2011, Code automatically updated!
	_ALFA = dialog_VLGetRadiobutton(idc_alfabetisch,_VALLIST),
%END Adresetiketten, Update controls
	_ALFA = idc_alfabetisch, !,
	true.
  dlg_adresetiketten_update(_VALLIST, b_True):-
	true.

%MARK Adresetiketten, new events

%BEGIN Adresetiketten, idc_etiketsoort _CtlInfo
  dlg_adresetiketten_eh(_Win,e_Control(idc_etiketsoort,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	dlg_etikesoort_Create(_Win),
	!.
%END Adresetiketten, idc_etiketsoort _CtlInfo

  dlg_adresetiketten_eh(_,_,_):-!,fail.

%END_DLG Adresetiketten

%BEGIN_DLG Speleruitvoer2
/**************************************************************************
	Creation and event handling for dialog: Speleruitvoer2
**************************************************************************/

constants

%BEGIN Speleruitvoer2, CreateParms, 21:43:06-14.7.2014, Code automatically updated!
  dlg_speleruitvoer2_ResID = idd_speleruitvoer2
  dlg_speleruitvoer2_DlgType = wd_Modal
  dlg_speleruitvoer2_Help = idh_adresetiketten
%END Speleruitvoer2, CreateParms

predicates

  dlg_speleruitvoer2_eh : EHANDLER
  dlg_speleruitvoer2_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST, boolean, boolean, boolean)
  dlg_speleruitvoer2_update(DIALOG_VAL_LIST, boolean, boolean, boolean)
procedure  dlg2Bool(integer, boolean, boolean, boolean)
  
clauses

  dlg2Bool(idcu_kommagescheiden, b_true, b_False, b_False) :- !.
  dlg2Bool(idcu_vaste_kolommen, b_False, b_True, b_False) :- !.
  dlg2Bool(_, b_False, b_False, b_true).

  dlg_speleruitvoer2_Create(Parent, Kom, Kol, XML):-

	FORMAAT = idcu_xml,
%MARK Speleruitvoer2, new variables

	dialog_CreateModal(Parent,dlg_speleruitvoer2_ResID,"",
  		[
%BEGIN Speleruitvoer2, ControlList, 21:43:06-14.7.2014, Code automatically updated!,
		df(FORMAAT,radiobuttongroup([idcu_kommagescheiden,idcu_vaste_kolommen,idcu_xml]),nopr)
%END Speleruitvoer2, ControlList
		],
		dlg_speleruitvoer2_eh,0,VALLIST,ANSWER),
	dlg_speleruitvoer2_handle_answer(ANSWER,VALLIST, Kom, Kol, XML).

  dlg_speleruitvoer2_handle_answer(idc_ok,VALLIST, Kom, Kol, XML):-!,
	dlg_speleruitvoer2_update(VALLIST, Kom, Kol, XML).
  dlg_speleruitvoer2_handle_answer(idc_cancel,_,b_False, b_False, b_False):-!.  % Handle Esc and Cancel here
  dlg_speleruitvoer2_handle_answer(_,_,_,_,_):-
	errorexit().

  dlg_speleruitvoer2_update(_VALLIST, Kom, Kol, XML):-
%BEGIN Speleruitvoer2, Update controls, 21:43:06-14.7.2014, Code automatically updated!
	_FORMAAT = dialog_VLGetRadiobutton(idcu_kommagescheiden,_VALLIST),
%END Speleruitvoer2, Update controls
	dlg2Bool(_FORMAAT, Kom, Kol, XML),
	true.

%MARK Speleruitvoer2, new events

  dlg_speleruitvoer2_eh(_,_,_):-!,fail.

%END_DLG Speleruitvoer2

%BEGIN_WIN Betalingen
/**************************************************************************
        Creation and event handling for window: Betalingen
**************************************************************************/

constants
%BEGIN Betalingen, CreateParms, 11:07:13-27.11.2014, Code automatically updated!
  win_betalingen_WinType = w_TopLevel
  win_betalingen_Flags = [wsf_SizeBorder,wsf_TitleBar,wsf_Maximize,wsf_Close,wsf_ClipSiblings]
  win_betalingen_RCT = rct(100,80,512,649)
  win_betalingen_Menu = no_menu
  win_betalingen_Title = "Nog te betalen (en ingedeeld)"
  win_betalingen_Help = idh_betalingen
%END Betalingen, CreateParms
id_betaald = 2011
id_aanwezig = 2012
id_spelergegevens = 2013
id_bevestigen = 2014
id_Speler_betaald_reset = 2015

database - betalen
  determ itemAfmB(WINDOW, integer Tab1, integer Tab2Vrij, integer Tab3Vrij, integer Tab4)
  %single ictr(string Naam, BOOLEAN)
  determ selectieBezig()
  single betaalBevestigen(janee)
  hotSpelerSpot(window LboxWin, integer SpelerNummer, rct NaamVeld, rct Vakje)
  single menucontext(integer SpelerNo)

predicates

  win_betalingen_eh : EHANDLER
  nondeterm te_betalen(string)
  determ te_betalen1(integer)
  betaalrepr(string, string, string, string, string, janee)
  bSubclass_eh : EHANDLER  
  inBLijst : zoekFunc
%  changeBetaald1(string Naam, janee Selected)
%  procedure inclLidnr(janee, string LNrIn, string LidNr, string ClubIn, string Club)
  nondeterm own_member(OD_ITEMSTATE,OD_STATE)
  bOwnerdraw(WINDOW, OD_ITEMSTATE, RCT, integer Item, BOOLEAN Selected)
  somBetalingen(slist, real, real)
  procedure betaallijst()
  %handleBetaald(integer, window, integer Index, boolean Terug)
  switch_bevestigen(window)
  procedure getpicture(janee,  resid)
  procedure setHotSpelerSpot(window LboxWin, integer SpNo, rct SpRect, rct BetRect)
  procedure hiliteAanwezig(window, integer SpelerNo)
  procedure compB(window LboxWin, slist NamenNw, slist NamenOud, integer I)
%  determ    betalingBevestigen(janee, integer SpNo, string Naam, janee, string BondsNr)
  procedure index(slist, string, integer, integer)
  procedure bevestig2checkmark(janee, integer Checkmark)
  procedure betaald2tekst(janee, string)
  procedure aanwezigMelden(integer SpNo, boolean)
  procedure checkKNLTB(string Bondsnummer)
  procedure alsLeeg(slist, slist)
  procedure qSelected(boolean , window LboxWin, rct RectItem)
  procedure resetbetalingen1()
  
clauses

menucontext(0).
betaalBevestigen(ja).

bevestig2checkmark(ja, mis_checked) :- !.
bevestig2checkmark(_, 0).

betaald2tekst(ja, "(toch) &Niet voldaan") :- !.
betaald2tekst(_, "&Voldaan").

own_member([X|_],X).
own_member([_|T],X):-own_member(T,X).

close_betaal() :-
  itemAfmB(Win, _, _, _, _),
  win_Destroy(Win), !.
close_betaal().

inBLijst(Str, Letter) :-
  frontchar(Str,Letter,_RestString), !.

te_betalen(Naam) :-
  sp2(SpNo,_Sex,Naam,_,_,_,_,nee,_, _, _, _, _, _, _, _, _, _, _, _,_,_,_,_,_,_,_),
  te_betalen1(SpNo).
  %nietbetaald(Betaald),

te_betalen1(SpNo) :-
  contributie(SpNo, Bedrag, _, []),
  Bedrag > 0, !.
te_betalen1(SpNo) :-
  banentool(ja, _),
  wedstrijd(No,WedsCat,SpNo,_,_,Andere),
  not(w3(No,WedsCat,_,_,_,_,_)),
  not(Andere = bye), !.
  
  %format(_NaamNr, "%",SpNo).

%inclLidnr(ja, LidNr, LidNr, Club, Club) :- !.
%inclLidnr(_, _, "", _, "").
aanwezigMelden(SpNo, b_true) :-
  banentool(ja, _),
  not(spelerAanwezig(SpNo, _, _, _)), !.
aanwezigMelden(_SpNo, b_False).

betaalrepr(SpNoS, BedragS, Str, "", Naam, Betaald) :-
  sp2(SpNo,Sex,Naam,_,_,_,_,Betaald,_, _ClubIn, _, _, _LidNrIn, _, _, _, _, _, _, _,_,_,_,_,_,_,_),
  str_int(SpNoS, SpNo),
  spsrt(Sex, _, MV, _, _, _, _, _), 
  mv2string(MV, Str),
  contributie(SpNo, Bedrag, _, []),
  format(BedragS, "%7.2f", Bedrag), !.

checkKNLTB(BondsNr) :-
  openbaar(ja),
  not(fronttoken(BondsNr, _, _)),
  dlg_MessageBox( "Bondsnummer", "Bondsnummer ontbreekt!", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  !.
checkKNLTB(_BondsNr).

changeBetaald1(Naam, nee) :-
  sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,nee,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
  SpelerIn = sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,ja,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
  logf('A',SpelerIn,nee), !,
  checkKNLTB(BondsNr),
  update_uitslagen("tasspsel changebetaald"),
  update_waarsch.
changeBetaald1(Naam, ja) :-
  sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,ja,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
  SpelerIn = sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,nee,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
  logf('A',SpelerIn,nee), !,
  update_uitslagen("tasspsel changebetaald"),
  update_waarsch.
changeBetaald1(_Naam, _).

index([Naam|_], Naam, I, I) :- !.
index([_| Rest], Naam, I, Uit) :-
  I1 = I + 1, !,
  index(Rest, Naam, I1, Uit).
index(_, _, I, I).

bSubclass_eh(_Win,e_Char(Getal,c_Nothing),0):-
  Teken = cast(char, Getal),
  Teken >= 'A',
  Teken <= 'z',
  LboxList = lbox_GetAll(_Win),
  NoOfItems = lbox_CountAll(_Win),
  zoekTeken(_Win, Teken, LboxList, NoOfItems, NewPos, inBLijst),
  lbox_SetTopIndex( _Win, NewPos),
  %Pos1 = cast(ulong, NewPos),
  %sendMessage(_Win, 0x019E, Pos1, 0),	% werkt !!!
  !.
bSubclass_eh(_LWin,e_KeyUp('\27',_ShiftCtlAlt),0):-!,
  Parent=win_GetParent(_LWin),
  win_Destroy(Parent).
bSubclass_eh(_LWin,e_Char(Kar,_ShiftCtlAlt),0):-!,
  Parent=win_GetParent(_LWin),
  win_SendEvent(Parent,e_Char(Kar,_ShiftCtlAlt)), !.
bSubclass_eh(LboxWin,e_MouseDown(PNT,_ShiftCtlAlt,mouse_button_right),0):-
  hotSpelerSpot(LboxWin, SKeus, RCT, _),
  rect_PntInside(RCT,PNT),
  sp2(SKeus,_Sex,Naam,_,_,_,_,_Nee,_, _, _, _, _LidNrIn, _, _, _, _, _, _, _,_,_,_,_,_,_,_),
  LboxList = lbox_GetAll(LboxWin),
  index(LboxList, Naam, 0, Index),
  lbox_SetSel(LboxWin, Index, b_True),
  sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
  SpelerIn = sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
  TaskWin = vpi_GetTaskWin(),
  dlg_persoonsgegevens_Create(TaskWin, SpelerIn ), !.
bSubclass_eh(LboxWin,e_MouseDown(PNT,_ShiftCtlAlt,mouse_button_left),0):-
  %LboxWin = win_GetCtlHandle(_Win, idcb_betaallst),
  hotSpelerSpot(LboxWin, SpNo, RCT, _),
  rect_PntInside(RCT,PNT),
  sp2(SpNo,_Sex,Naam,_,_,_,_,Betaald,_, _, _, _, _LidNrIn, _, _, _, _, _, _, _,_,_,_,_,_,_,_),
  assert(menucontext(SpNo)),
  LboxList = lbox_GetAll(LboxWin),
  index(LboxList, Naam, 0, Index),
  lbox_SetSel(LboxWin, Index, b_True),
  aanwezigMelden(SpNo, BBool),
  betaald2Tekst(Betaald, Btekst),
  Menu = dyn_menu([
    txt(id_betaald,BTekst,0,b_True,0,[]),
    txt(id_spelergegevens,"&Spelergegevens",0,b_True,0,[]),
    txt(id_aanwezig,"&Aanwezig melden",0,BBool,0,[])
    ]),
  menu_PopUp(LboxWin,Menu, PNT, align_Left),
  !.
bSubclass_eh(LboxWin,e_MouseDown(PNT,_ShiftCtlAlt,mouse_button_left),0):-
  %LboxWin = win_GetCtlHandle(_Win, idcb_betaallst),
  getbacktrack(Here),
  hotSpelerSpot(LboxWin, SKeus, _RCT1, RCT),
  rect_PntInside(RCT,PNT),
  sp2(SKeus,_Sex,Naam,_,_,_,_,Betaald,_, _, _, _, BondsNr, _, _, _, _, _, _, _,_,_,_,_,_,_,_), 
  cutbacktrack(Here),
  LboxList = lbox_GetAll(LboxWin),
  index(LboxList, Naam, 0, Index),
  lbox_SetSel(LboxWin, Index, b_True),
  betaalBevestigen(Bevestigen),
  betalingBevestigen(Bevestigen, SKeus, Naam, Betaald, BondsNr),
  changebetaald1(Naam, Betaald),
  te_informeren(SKeus),
  win_Invalidate(LboxWin, RCT),
  LboxList = lbox_GetAll(LboxWin),
  %lbox_GetSel(LboxWin,ItemList,_IndexList),
  somBetalingen(LboxList, 0.0, Som),
  format(Totaal, "Nu verwerkt %7.2f",Som),
  ParentWin = win_GetParent(LboxWin),
  toolbar_SetValue(ParentWin, idt_s1, text_value(Totaal)),
  %trap(win_SetFocus(LboxWin),_,fail),
  !.
bSubclass_eh(LboxWin,e_MouseDown(PNT,_ShiftCtlAlt,mouse_button_left),0):- 
  hotSpelerSpot(LboxWin, _SKeus, _RCT1, RCT),
  rect_PntInside(RCT,PNT), !.
bSubclass_eh(LboxWin,e_Menu(id_betaald,_),0):- 
  menucontext(SKeus),
  sp2(SKeus,_Sex,Naam,_,_,_,_,Betaald,_, _, _, _, BondsNr, _, _, _, _, _, _, _,_,_,_,_,_,_,_), 
  LboxList = lbox_GetAll(LboxWin),
  index(LboxList, Naam, 0, Index),
  lbox_SetSel(LboxWin, Index, b_True),
  betaalBevestigen(Bevestigen),
  betalingBevestigen(Bevestigen, SKeus, Naam, Betaald, BondsNr),
  changebetaald1(Naam, Betaald),
  win_Invalidate(LboxWin), !.
bSubclass_eh(_LboxWin,e_Menu(id_spelergegevens,_),0):- 
  menucontext(SKeus),
  sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
  SpelerIn = sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
  TaskWin = vpi_GetTaskWin(),
  dlg_persoonsgegevens_Create(TaskWin, SpelerIn ), !.
bSubclass_eh(_LboxWin,e_Menu(id_aanwezig,_),0):- 
  menucontext(SKeus),
  zetaanwezig(SKeus), !.

somBetalingen([Naam|Rest], In, Uit) :-
  sp2(SpNo,_,Naam,_,_,_,_,ja,_, _, _, _, _, _, _, _, _, _, _, _,_,_,_,_,_,_,_),
  contributie(SpNo, Bedrag, _, []),
  In1 = In + Bedrag, !,
  somBetalingen(Rest, In1, Uit).
somBetalingen([_|Rest], In, Uit) :-
  somBetalingen(Rest, In, Uit), !.
somBetalingen(_, In, In).

betaallijst() :-
  findall(Naam, te_betalen(Naam), Namen),
  sortstring_c(Namen, 1),
  get_TempDir(_, TempFile),
  openwrite(work,TempFile),
  writedevice(work),
  member(NaamX, Namen),
  sp2(SKeus,SpelerSrt,NaamX,Tel,_,_,_,_,_, _, _, _, _, _, _, _, Tel1, Tel2, _, _,_,_,_,_,_,_,_),
  spsrt(Spelersrt,_,_Sexe,_,_,_,_,[Aanhef|_]),
  naamscan(nee,b_False,NaamX,NaamSc,_,_Landcode),
  writef("\n% %-30s H:%-12s W:%-12s M:%-12s  ",Aanhef, NaamSc,Tel,Tel1,Tel2),
  contributie(SKeus, Som, String,[]),
  writef("\n   %-22 eur%6.2f",String,Som),
  %write(NaamX), nl,
  fail.
betaallijst() :-
  closefile(work),
  TaskWin = vpi_GetTaskWin(),
  dlg_rapport_Create(TaskWin, "Betaallijst","Betaal",b_True), !.
betaallijst().

switch_bevestigen(Win) :-
  betaalBevestigen(ja),
  assert(betaalbevestigen(nee)),
  toolbar_SetValue(Win, idt_2, text_value("Niet bevestigen")), !.
switch_bevestigen(Win) :-
  assert(betaalBevestigen(ja)),
  toolbar_SetValue(Win, idt_2, text_value("Bevestigen")), !.

/*
--------------------------------------------------------------------------------
Look at MSDN at LB_SETCARETINDEX.
Therefore in VPI you can 
1. define global predicate SendMessage, 
2. define constant lb_SETCARETINDEX
and all will be OK.
*/

setHotSpelerSpot(LboxWin, _SpNo, _SpRect, BetRect) :-
  hotSpelerSpot(LboxWin, Sp1, _, BetRect1),
  RCTI = rect_Intersect(BetRect1, BetRect),
  not(RCTI = rct(0,0,0,0)),
  %write("\n", Spno, "  ", Naam, " ", SpRect, " ", BetRect),
  retractall(hotSpelerSpot(LboxWin, Sp1, _, _)),
  fail.
setHotSpelerSpot(LboxWin, SpNo, SpRect, BetRect) :-
  SpRect = rct(_, T, _, _),
  T >= 0,
  %write("\n", Spno, "  ", Naam, " ", SpRect, " ", BetRect),
  retractall(hotSpelerSpot(LboxWin, SpNo, _, _)),
  asserta(hotSpelerSpot(LboxWin, SpNo, SpRect, BetRect)), !.
setHotSpelerSpot(_LboxWin, _No1, _A2Rect, _).

update_betalingen() :-
  itemAfmB(Win, _, _, _, _), !,	% bestaat al
  LboxWin = win_GetCtlHandle(Win, idcb_betaallst),
  NamenOud = lbox_GetAll(LboxWin),
  findall(NaamNw, te_betalen(NaamNw), NamenNw),
  sortstring_c(NamenNw, 1),
  compB(LboxWin, NamenNw, NamenOud, 0),
  win_Invalidate(Win), !.
update_betalingen().

compB(LboxWin, [N|NamenNw], [N|NamenOud], I) :- !,
  I1 = I + 1,
  compB(LboxWin, NamenNw, NamenOud, I1). 
compB(LboxWin, [NNw|NamenNw], NamenOud, I) :- 
  not(member(NNw, Namenoud)), !,
  lbox_Add(LboxWin, I, NNw),
  I1 = I + 1,
  compB(LboxWin, NamenNw, NamenOud, I1). 
compB(LboxWin, [NNw|NamenNw], [_NNO|NamenOud], I) :-
  member(NNw, NamenOud),
  %lbox_Add(LboxWin, I, NNw),
  I1 = I + 1, !,
  compB(LboxWin, [NNw|NamenNw], NamenOud, I1). 
compB(_LboxWin, _, _, _I).

betalingBevestigen(nee, _SpNo, _Naam, _Nee, _BondsNr) :- !.
betalingBevestigen(_, SpNo, Naam, nee, BondsNr) :-
  contributie(SpNo, Bedrag, String, []),
  format(Msg, "%s\nBondsnummer: %s\n%7.2f  eur%s\nVoldaan?", Naam, BondsNr, Bedrag, String),
  1 = dlg_MessageBox( "Betaling", Msg, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
  !.
betalingBevestigen(_, _SpNo, Naam, ja, _BondsNr) :-
  format(Msg,"Betaling door %s\nongedaan maken?",Naam),
  1 = dlg_MessageBox( "Betaling", Msg, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
  !.

  win_betalingen_Create(_Parent):-
	itemAfmB(Win, _, _, _, _), !,	% bestaat al
	win_Invalidate(Win),
	win_SetState(Win, [wsf_Restored]), 
	win_SetFocus(Win).
  win_betalingen_Create(_Parent):-
	assert(betaalbevestigen(nee)),
	win_Create(win_betalingen_WinType,win_betalingen_RCT,win_betalingen_Title,
		   win_betalingen_Menu,_Parent,win_betalingen_Flags,win_betalingen_eh,0).

%BEGIN Betalingen, e_Create
  win_betalingen_eh(_Win,e_Create(_),0):-!,
%BEGIN Betalingen, InitControls, 11:07:13-27.11.2014, Code automatically updated!
	win_CreateControl(wc_LBox,rct(7,3,395,557),"",_Win,[wsf_OwnerDraw,wsf_Group,wsf_TabStop,wsf_VScroll,wsf_HasStrings,wsf_NoIntegralHeight],idcb_betaallst),
%END Betalingen, InitControls
%BEGIN Betalingen, ToolbarCreate, 11:07:13-27.11.2014, Code automatically updated!
	tb_betalingen_Create(_Win),
%END Betalingen, ToolbarCreate
        NewFont = font_Create(ff_Helvetica, [], 10),	% 26.12.2011
  	win_SetFont(_Win, NewFont),
	vensterpositie(betaalvenster, _Win),
	LboxWin = win_GetCtlHandle(_Win, idcb_betaallst),
  	win_SetFont(LboxWin, NewFont),
	switch_bevestigen(_Win),
	win_PostEvent(_Win,e_user(1,0)),
	win_SetSubclassHandler(LboxWin,bSubclass_eh,b_False),
	!.
%END Betalingen, e_Create
%MARK Betalingen, new events

%BEGIN Betalingen, idc_help
  win_betalingen_eh(_Win,e_Menu(idc_help,_ShiftCtlAlt),0):-!,
	project_ShowHelpContext("Betalingen"),
	!.
%END Betalingen, idc_help

%BEGIN Betalingen, idts_popup
	win_betalingen_eh(_Win,e_Menu(idts_popup,_ShiftCtlAlt),0):-
	RCT = win_GetClientRect( _Win ),
	RCT = rct(L, _, _, B),
	L1 = L + 10,
	B1 = B - 60,
	betaalBevestigen(JaNee),
	bevestig2checkmark(JaNee,State),
	Menu = dyn_menu([
	  txt(id_bevestigen,"&Bevestigen",0,b_True,State,[])
	  ]),
	menu_PopUp(_Win,Menu, pnt(L1,B1), align_Left),
	!.
%END Betalingen, idts_popup

%BEGIN Betalingen, e_Update
  win_betalingen_eh(_Win,e_Update(_UpdateRct),0):-
	LboxWin = win_GetCtlHandle(_Win, idcb_betaallst),
	findall(NaamNr, te_betalen(NaamNr), Namen0),
        alsLeeg(Namen0, Namen1), 
	sortstring_c(Namen1, 1),
	lbox_Suspend(LboxWin),
	lbox_Clear(LboxWin),
	lbox_Add(LboxWin, Namen1),
	lbox_Resume(LboxWin),
	somBetalingen(Namen1, 0.0, Som),
	format(Totaal, "Nu verwerkt %7.2f",Som),
	toolbar_SetValue(_Win, idt_s1, text_value(Totaal)),
	!.

%END Betalingen, e_Update

%BEGIN Betalingen, e_GetFocus
  win_betalingen_eh(_Win,e_GetFocus,0):-!,
	LboxWin = win_GetCtlHandle(_Win, idcb_betaallst),
	win_SetFocus(LboxWin),
	!.
%END Betalingen, e_GetFocus


%BEGIN Betalingen, e_User
  win_betalingen_eh(_Win,e_User(1,_),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idcb_betaallst),
	win_SetFocus(LboxWin),
	%trap(win_SetFocus(LboxWin),_,fail),
%	sendMessage(LboxWin, 0x019E, Pos1, 0),	% werkt !!!
	!.
  win_betalingen_eh(_Win,e_User(2,_),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idcb_betaallst),
	RCT = win_GetClientRect( LboxWin ),
	win_Invalidate(LboxWin, RCT),
	!.
%END Betalingen, e_User


%BEGIN Betalingen, e_OwnerMeasureItem
  win_betalingen_eh(_Win,e_OwnerMeasureItem(_CtlType,_CtlId,_ItemId,_Data),Size):-
	%openbaar(nee),
	win_GetTextExtent(_Win, "JansenenTilanusg", -1, Width, He),
	Height = He,
	bitleft(Height, 16, H1),
	Size = Width + H1,
	win_GetTextExtent(_Win, "x 127.50", -1, Tab1, _),
	Tab2 = Tab1 + 20,		% de bitmap
	win_GetTextExtent(_Win, "iiX", -1, Width3, _),
	Tab3 = Tab2 + Width3,
	Tab4 = Tab3 + Width3,
	assert(itemAfmB(_Win, Tab1, Tab2, Tab3, Tab4)), 
	!.
%END Betalingen, e_OwnerMeasureItem

%BEGIN Betalingen, e_OwnerDraw
  win_betalingen_eh(_Win,e_OwnerDraw(_CtlType,_CtlId,_ItemId,_ItemAction,
		ItemState,LboxWin,RectItem,_ItemData),0):-
	own_member(ItemState, odstate_Selected), !,  % was selected
	win_SetPen(LboxWin,pen(1,ps_Solid,color_White)),
	draw_Rect(LboxWin,RectItem),
	win_SetForeColor(LboxWin, color_Black),
	win_SetBackColor(LboxWin, color_White),
	bOwnerdraw(Lboxwin, ItemState, RectItem, _ItemId,b_True),
	!.
  win_betalingen_eh(_Win,e_OwnerDraw(_CtlType,_CtlId,_ItemId,_ItemAction,
		ItemState,LboxWin,RectItem,_ItemData),0):-
	win_SetPen(LboxWin,pen(1,ps_Solid,color_White)),
	draw_Rect(LboxWin,RectItem),
	win_SetForeColor(LboxWin, color_Black),
	win_SetBackColor(LboxWin, color_White),
	bOwnerdraw(Lboxwin, ItemState, RectItem, _ItemId,b_False),
	!.
%END Betalingen, e_OwnerDraw

%BEGIN Betalingen, idcb_betaallst selchanged
  win_betalingen_eh(_Win,e_Control(idcb_betaallst,_CtrlType,CtrlWin,activated),0):-
	Index = lbox_GetSelIndex(CtrlWin),
	Naam = lbox_GetItem(CtrlWin, Index),
	sp2(SKeus,_Sex,Naam,_,_,_,_,Betaald,_, _, _, _, BondsNr, _, _, _, _, _, _, _,_,_,_,_,_,_,_),
	betaalBevestigen(Bevestigen),
	betalingBevestigen(Bevestigen, SKeus, Naam, Betaald, BondsNr),
	changebetaald1(Naam, Betaald),
	lbox_GetSel(CtrlWin,ItemList,_IndexList),
	somBetalingen(ItemList, 0.0, Som),
	format(Totaal, "Totaal verwerkt %7.2f",Som),
	toolbar_SetValue(_Win, idt_s1, text_value(Totaal)),
	trap(win_SetFocus(CtrlWin),_,fail),
	!.
%END Betalingen, idcb_betaallst selchanged

%BEGIN Betalingen, e_Destroy
  win_betalingen_eh(_Win,e_Destroy,0):-
	updateVensterpositie(betaalvenster, _Win),
	retractall(itemafmB(_,_,_,_, _)),
	LboxWin = win_GetCtlHandle(_Win, idcb_betaallst),
	retractall(hotSpelerSpot(LboxWin, _, _, _)),
	!.
%END Betalingen, e_Destroy

%BEGIN Betalingen, e_Size
  win_betalingen_eh(_Win,e_Size(_Width,_Height),0):-!,
ifdef use_tbar
	toolbar_Resize(_Win),
enddef
	LboxWin = win_GetCtlHandle(_Win, idcb_betaallst),
	toolbar_GetRect(_Win,tb_bottom,RCTB),
	RCTB = rct(_,T,_,B),
	H1 = _Height - B + T,
	%RCT = win_GetClientRect( _Win ),
	%H = Height - B + T,
	win_Move(LboxWin,rct(0,0,_Width,H1)),
	!.
%END Betalingen, e_Size

  win_betalingen_eh(_Win,e_Menu(id_Onderdeel_print,_ShftCtrlAlt),0):-
	betaallijst(),		% vaste betaallijst.
	BLWin = win_GetCtlHandle(_Win, idcb_betaallst),
	trap(win_SetFocus(BLWin),_,fail),
	!.
  win_betalingen_eh(_Win,e_Menu(id_bevestigen,_ShftCtrlAlt),0):-
	switch_bevestigen(_Win),		
	BLWin = win_GetCtlHandle(_Win, idcb_betaallst),
	trap(win_SetFocus(BLWin),_,fail),
	!.

%BEGIN Betalingen, e_Menu, Parent window 
  win_betalingen_eh(Win,e_Menu(ID,CAS),0):-!,
	PARENT = win_GetParent(Win),
	win_SendEvent(PARENT,e_Menu(ID,CAS)),
	!.
%END Betalingen, e_Menu, Parent window

%END_WIN Betalingen


resetbetalingen1() :-
  backupdirect(),
  sp2(SKeus1,Sex1,Naam1,Telthuis1,Verh1,RatingE1,RatingD1,ja,UitKant1, Club1, Adres1, Woonpl1, BondsNr1, ES1, DS1, Gebdatum1, Telwerk1, Telmobiel1, EMail1, Opm1,Gezien1,RangPosE1,RangPosD1,Incasso1,CheckGeg1,Log1,Import1),
  cursor_SetWait(),
  _Result = vpi_ProcessEvents(),
  logf('Z',sp2(SKeus1,Sex1,Naam1,Telthuis1,Verh1,RatingE1,RatingD1,nee,UitKant1, Club1, Adres1, Woonpl1, BondsNr1, ES1, DS1, Gebdatum1, Telwerk1, Telmobiel1, EMail1, Opm1,Gezien1,RangPosE1,RangPosD1,Incasso1,CheckGeg1,Log1,Import1),ja),
  fail.
resetbetalingen1() :-
  logclose(),
  compress(update),
  startdbrefresh().

resetbetalingen() :-
  TaskWin = vpi_GetTaskWin(),
  checkPaswoord(TaskWin),
  1 = dlg_MessageBox( "Betalingen resetten", "ALLE spelers worden op NIET betaald gezet!!!\n\n\nBevestigen svp!", mesbox_iconexclamation, mesbox_buttonsokcancel, mesbox_defaultsecond, mesbox_suspendapplication ),
  resetbetalingen1().

getpicture(ja, idb_betaaldja) :- !.
getpicture(_, idb_betaaldnee).

hiliteAanwezig(LboxWin, SpNo) :-
  spelerAanwezig(SpNo, _, _, _), !,
  win_SetBackColor(LboxWin, color_Yellow),
  win_SetForeColor(LboxWin, color_Black),
  !.
hiliteAanwezig(_LboxWin, _SpNo).

alsLeeg(_, ["Betaaladminstratie bijhouden is","niet aangevinkt bij 'Toernooibeleid'"]) :- betaal_adm(nee), !.
alsLeeg([], ["Alle spelers hebben betaald","of er is nog niet ingedeeld.", "Gebruik eventueel Meer, Reset ALLE betalingen"]) :- !.
alsLeeg(Namen, Namen).

qSelected(b_True, LboxWin, RectItem) :-
	win_SetBrush(LboxWin,brush(pat_Hollow, color_Yellow)),
	win_SetPen(LboxWin,pen(1,ps_Dot,color_Black)),
	draw_Rect(LboxWin,RectItem), !.
qSelected(_, _, _).

bOwnerdraw(Lboxwin, _ItemState, RectItem, ItemId, Selected) :-
	%RCT = win_GetClientRect( LboxWin ),
	%write("\n", RCT),
	RectItem = rct(_L, T, _R, B),
	Naam = lbox_GetItem(LboxWin, ItemId),
	betaalrepr(SpNoS, Bedrag, MV, _Club, Naam, Betaald),
	str_int(SpNoS, SpNo),
	itemAfmB(_Win, Tab1, Tab2, Tab3, Tab4),
	Rect1 = rct(0, T, Tab1, B),
	draw_TextInRect(LboxWin,Rect1, Bedrag, -1, [dtext_right]),
	getpicture(Betaald, Picture),
	Tab1a = Tab1 + 5,
	Tab1b = Tab2,
	Picture1 = pict_GetFromRes(Picture),
	pict_Draw(LboxWin, Picture1, pnt(Tab1a, T), rop_SrcCopy),
  	BetRect = rct(Tab1a, T, Tab1b, B), 
	draw_TextInRect(Lboxwin, rct(Tab3, T, Tab4, B), MV, -1, [dtext_left]),
	win_GetTextExtent(LboxWin, Naam, -1, NWidth, _),
	R1 = Tab4 + NWidth  + 2,
	Rect5 = rct(Tab4, T, R1, B),
	setHotSpelerSpot(LboxWin, SpNo, Rect5, BetRect), 
	hiliteAanwezig(LboxWin, SpNo),	
	draw_TextInRect(LboxWin,Rect5, Naam, -1, [dtext_left]), 
	qSelected(Selected, LboxWin, RectItem), !.
bOwnerdraw(Lboxwin, _ItemState, RectItem, ItemId, _Selected) :-
	%RCT = win_GetClientRect( LboxWin ),
	%write("\n", RCT),
	Inhoud = lbox_GetItem(LboxWin, ItemId),
	draw_TextInRect(LboxWin,Rectitem, Inhoud, -1, [dtext_left]), !. 

database - overgeslagen
  over(string ID,string Nr)
  maxwaarde(string ID, integer Max)

predicates
  nondeterm opgid(string)
  spelermetImpId(string ImpId)

clauses

opgid(Opg) :-
  retract(over(Id, Nr)),
  format(Opg, "%s%s", Id, Nr).

spelermetImpId(ImpId) :-
  sp2(_,_,_,_,_,_,_,_,_, _, _, _, _, _, _, _, _, _, _, _Opm,_,_,_,_,_,_,Imports),
  member(ImpId, Imports), !.
  %searchstring(Opm,ImpId,_FP), !.

overgeslagen([]) :-	% 13.5.2015
  naam(_, _, _, WebTicketI, _, _),
  maketicket(WebTicketI, WebTicketO), 
  not(fronttoken(WebticketO, _, _)), !.
overgeslagen([]) :-
  retractall(_, overgeslagen),
  providerX(Prov, _, "", _),
  AV = aanmeldenvars(),
  get_tempdir(_, Download),
  deletefile(Download),
  format(Volgend,"http://%s/getopgx2009.php?stap=compleetcheck",Prov), 
  Return = downloadData(Volgend, Download, AV, _, 0),
  verbindingMislukt(Return), !.
overgeslagen([]) :-
  get_tempdir(_, Download),
  %file_str(Download, _SS),
  %write(_SS),
  openread(work,Download),
  readdevice(work),
  repeat_f(work),
  %readterm(overgeslagen,Fact),
  trap(readterm(overgeslagen,Fact),_,true),
  Fact = over(Id, Nr),
  format(ImpId, "%s%s", Id, Nr),
  not(spelermetImpId(ImpId)),
  assert(Fact, overgeslagen),
  %write("\n", Fact),
  fail.
overgeslagen(OpgList) :-
  closefile(work),
  findall(OID, opgid(OID), OpgList).
