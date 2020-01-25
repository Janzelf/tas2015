/*****************************************************************************

		Copyright (c) 1989 - 1998 De Lint Associates

 Project:  TASVP1
 FileName: TASTABEL.PRO
 Purpose: No description
 Written by: JdL
 Comments:
******************************************************************************/

include "tasvp1.inc"
include "tasvp1.con"
include "hlptopic.con"
%include "treeview\\treeview.ph"

include "error.con"
include "datecust\\date_cc.pre"
include "datecust\\date_cc.pro"



%BEGIN_DLG Spelersoorten
/**************************************************************************
	Creation and event handling for dialog: Spelersoorten
**************************************************************************/

database - temp
  t_spsrt(integer, sex, string, sexe, integer, integer, string, string, slist)
single index(integer)	% NB wordt ook bij onderdelen gebruikt
single indexV(integer)
  nieuweSex(sexe, sex)

constants

%BEGIN Spelersoorten, CreateParms, 10:48:09-5.9.2009, Code automatically updated!
  dlg_spelersoorten_ResID = idd_spelersoorten
  dlg_spelersoorten_DlgType = wd_Modal
  dlg_spelersoorten_Help = idh_spelersoorten
%END Spelersoorten, CreateParms

predicates

  dlg_spelersoorten_eh : EHANDLER
  dlg_spelersoorten_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_spelersoorten_update(DIALOG_VAL_LIST)
  copySpsrt()
  mv2d_mv(sexe, dialog_control_id, string)
  lf2d_lf(integer, dialog_int, string)
  opVolgorde(integer, SLIST, SLIST)
  nieuwAt(integer Index)
  nieuwAt(integer Index, integer Current)
  deleteSrt(WINDOW)
  shiftUp(integer Vanaf)
  shiftUp(integer Current, integer Max)
  maxIndex(integer In, integer Out)
  gebruikte_soort(sex)
  saveTabel(WINDOW)
procedure  saveTabel(SLIST, SLIST)
  repr(integer, string Representation)
%determ samenvoegenSpelersoorten()
procedure combineSpelersoorten()
determ voorWieNieuw(sexlist Voorwie, sexlist VoorWieNList)
  %toonTspsrt(string Titel)	% 23.2.2013
procedure checkspsrt(sexe)

clauses

index(0).
indexV(0).

gebruikte_soort(Num) :-
  sp2(_,Num,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !,
  findall(Naamx,   sp2(Num,_,NaamX,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),Namen),
  count(Namen, 0, Aantal),
  format(Text, "%s is een van de % spelers van deze soort!",Naam, Aantal),
  dlg_Note(Text).
gebruikte_soort(Num) :-
  wc(_, _, _, Naam, _, _, List, _, _, _, _, _, _, _, _),
  %wc(_, Naam, _, _, List, _, _),
  member(Num, List), !,
  format(Text, "%s is gekoppeld aan deze soort (%)!", Naam, Num),
  dlg_Note(Text).

maxIndex(In, In) :-
  index(Max),
  In <= Max, !.
maxIndex(_, Max) :-
  index(Max).
  
deleteSrt(Win) :-
  LboxWin = win_GetCtlHandle(Win, idc_lijst),
  lbox_GetSel(LboxWin,_ItemList,IndexList),
  IndexList = [Index|_],
  t_spsrt(Index, Sex, _, _, _, _, _, _, _),
  not(gebruikte_soort(Sex)),
  retract(t_spsrt(Index, _, _, _, _, _, _, _, _)), !,
  shiftUp(Index).
  
shiftUp(Vanaf) :-
  index(Max),
  shiftUp(Vanaf, Max), !.

shiftUp(VanAf, Max) :-
  Vanaf > Max, !,
  Max1 = Max - 1,
  assert(index(Max1)).
shiftUp(I, Max) :-
  retract(t_spsrt(I, Sex, Naam, MV, LftVan, LftTot, StrkVan, StrkTot, Reserve)),
  I1 = I - 1,  
  assert(t_spsrt(I1, Sex, Naam, MV, LftVan, LftTot, StrkVan, StrkTot, Reserve)),
  I2 = I + 1, !,
  shiftUp(I2, Max).
shiftUp(I, Max) :-
  I2 = I + 1, !,
  shiftUp(I2, Max).

nieuwAt(Index) :-
  index(Max),
  nieuwAt(Index, Max),
  Max1 = Max + 1,
  assert(index(Max1)).

nieuwAt(Index , Cur) :-
  Cur < Index, 
  numb(Sex),		% zoek vrij sexnummer
  not(t_spsrt(_, Sex, _, _, _, _, _, _, _)), !,
  assert(t_spsrt(Index, Sex, "", m, 0, 0, "", "", ["Hr"])).
nieuwAt(Index , Cur) :-
  retract(t_spsrt(Cur, Sex, Naam, MV, LftVan, LftTot, StrkVan, StrkTot, Titel)),
  NCur = Cur + 1, !,
  assert(t_spsrt(NCur, Sex, Naam, MV, LftVan, LftTot, StrkVan, StrkTot, Titel)),
  Cur1 = Cur - 1,
  nieuwAt(Index, Cur1).

opVolgorde(I, Lijst, Lijst) :-
  not(t_spsrt(I, _, _, _, _, _, _, _, _)), !.
opVolgorde(I, LIn, [Repr|LUit]):-
  repr(I, Repr),
  I1 = I + 1, !,
  opVolgorde(I1, LIn, LUit).  

repr(I, Repr) :-
  t_spsrt(I, _Sex, Naam, MV, LftVan, LftTot, StrkVan, StrkTot, [Aanhef|_]),
  mv2d_mv(MV, _, MVS),
  lf2d_lf(LftVan, _, LS1),
  lf2d_lf(LftTot, _, LS2),
  format(Repr, "%s\t%s\t%-2s\t%-2s\t%s\t%s\t%s",Naam, MVS, LS1, LS2, StrkVan,StrkTot, Aanhef), !.

saveTabel(Win) :-
  cursor_Set(Win, cursor_Wait),
  opVolgorde(0, [], Lijst),
  loglock(mdi, "saveTabel"),
  findall(Naam, spsrt(Sex, Naam, MV, LftVan, LftTot, StrkVan, StrkTot, Reserve), Namen),
  saveTabel(Lijst, Namen),
  logclose(), !,
  cursor_Set(Win, cursor_Arrow).
saveTabel(Win) :-
  cursor_Set(Win, cursor_Arrow).

saveTabel([], []) :- !.
saveTabel([Repr|Lijst], []) :-
  searchchar(Repr, '\t', Pos),
  Pos1 = Pos - 1,
  frontstr(Pos1, Repr, Naam, _),
  t_spsrt(_, Sex, Naam, MV, LftVan, LftTot, StrkVan, StrkTot, Reserve), !,
  logf('Z', spsrt(Sex, Naam, MV, LftVan, LftTot, StrkVan, StrkTot, Reserve),ja),
  saveTabel(Lijst, []).
saveTabel([], [Naam|Rest]) :-
  spsrt(Sex1, Naam, MV1, LftVan1, LftTot1, StrkVan1, StrkTot1, Reserve1), !, 
  logf('R', spsrt(Sex1, Naam, MV1, LftVan1, LftTot1, StrkVan1, StrkTot1, Reserve1),ja),
  saveTabel([], Rest).
saveTabel([Repr|Lijst], [Naam|T]) :-
  searchchar(Repr, '\t', Pos),
  Pos1 = Pos - 1,
  frontstr(Pos1, Repr, Naam1, _),
  Naam = Naam1,
  t_spsrt(_, Sex, Naam, MV, LftVan, LftTot, StrkVan, StrkTot, Reserve), 
  spsrt(Sex, Naam, MV, LftVan, LftTot, StrkVan, StrkTot, Reserve), !, %  = gelijk!
  saveTabel(Lijst, T).
saveTabel(_, Namen) :-		% ongelijk; haal de rest weg
  member(H, Namen),
  spsrt(Sex1, H, MV1, LftVan1, LftTot1, StrkVan1, StrkTot1, Reserve1), 
  logf('R', spsrt(Sex1, H, MV1, LftVan1, LftTot1, StrkVan1, StrkTot1, Reserve1),ja),
  fail.
saveTabel(Lijst, _) :-
  saveTabel(Lijst, []).
  
mv2d_mv(m, idc_man, "M") :- !.
mv2d_mv(v, idc_vrouw, "V").

lf2d_lf(0, void(), ""):- !.
lf2d_lf(I, i(I), Str) :-
  str_int(Str, I).

checkSpsrt(v) :-
  not(spsrt(_, _, v, _, _, _, _, _)),
  numb(N),
  N > 0,
  not(spsrt(N,_,_,_,_,_,_,_)),
  logf('Z',spsrt(N,"dame",v,0,0,"","",["Mw"]),ja), !.
checkSpsrt(m) :-
  not(spsrt(_, _, m, _, _, _, _, _)),
  numb(N),
  N > 0,
  not(spsrt(N,_,_,_,_,_,_,_)),
  logf('Z',spsrt(N,"heer",m,0,0,"","",["Hr"]),ja), !.
checkSpsrt(_).

initspsrt() :-
  checkspsrt(m),
  checkspsrt(v),
  logclose().

copySpsrt() :-
  initspsrt(),
  assert(index(0)),
  spsrt(Sex, Naam, MV, LftVan, LftTot, StrkVan, StrkTot, Reserve),
  index(I),
  assert(t_spsrt(I, Sex, Naam, MV, LftVan, LftTot, StrkVan, StrkTot, Reserve)),
  I1 = I + 1,
  assert(index(I1)),
  fail.
copySpsrt() :-
  index(I),
  I1 = I - 1,
  assert(index(I1)).	% geef hoogste index aan

combineSpelersoorten() :-
  getbacktrack(Here),
  spsrt(Sex, _Naam, m, _LftVan, _LftTot, _StrkVan, _StrkTot, _Reserve),
  cutbacktrack(Here),
  logf('A', spsrt(Sex, "man", m, 0, 0, "", "", ["Hr"]), ja),
  assert(nieuweSex(m, Sex)),
  fail.
combineSpelersoorten() :-
  getbacktrack(Here),
  spsrt(Sex, _Naam, v, _LftVan, _LftTot, _StrkVan, _StrkTot, _Reserve),
  cutbacktrack(Here),
  logf('A', spsrt(Sex, "vrouw", v, 0, 0, "", "", ["Mw"]), ja),
  assert(nieuweSex(v, Sex)),
  fail.
combineSpelersoorten() :-
  findall(SpNo, sp2(SpNo,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), SpNoList),
  member(SKeus, SpNoList),  % een voor een
  sp2(SKeus,Sex,Naam,Telthuis,Verhinderingen,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Res),
  spsrt(Sex, _, MV, _, _, _, _, _),
  nieuweSex(MV, SexN),
  logf('A', sp2(SKeus,SexN,Naam,Telthuis,Verhinderingen,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Res), nee),
  fail.
combineSpelersoorten() :-
  %findall(WC, wc(WC,_,_,_,_,_,_), WcList),
  findall(WC, wc(WC, _, _, _, _, _, _, _, _, _, _, _, _, _, _), WcList),
  member(WCcur, WcList),
  wc(WCcur, OnderdeeelID, Orde, Naam, EDG, Kort, VoorWie, SpStrk, LftV, LftTM, HDG, Schemasoort, Status, KNLTB, Geld),
  %wc(WCcur,Naam,EDG,Kort,VoorWie,Geld,Verl),
  voorWieNieuw(VoorWie, VoorWieN),
  logf('A', wc(WCcur, OnderdeeelID, Orde, Naam, EDG, Kort, VoorWieN, SpStrk, LftV, LftTM, HDG, Schemasoort, Status, KNLTB, Geld),ja),
  fail.
combineSpelersoorten() :-
  spsrt(Sex, Naam, MV, LftVan, LftTot, StrkVan, StrkTot, Reserve),
  not(nieuweSex(_, Sex)),
  logf('R', spsrt(Sex, Naam, MV, LftVan, LftTot, StrkVan, StrkTot, Reserve), ja),
  fail.
combineSpelersoorten() :-
  logclose(),
  compress(update),
  startDbRefresh().

voorWieNieuw(Voorwie, VoorWieNList) :-
  member(VM, Voorwie),
  spsrt(VM, _, m, _, _, _, _, _),
  member(VV, Voorwie),
  spsrt(VV, _, v, _, _, _, _, _),  % beide sexen!
  findall(VoorWien, nieuweSex(_, VoorWieN), VoorWieNList), !.
voorWieNieuw(Voorwie, [SexN]) :-
  member(VM, Voorwie),
  spsrt(VM, _, m, _, _, _, _, _),
  nieuweSex(m, SexN), !.
voorWieNieuw(_Voorwie, [SexN]) :-
  nieuweSex(v, SexN), !.

samenvoegenSpelersoorten() :-
  findall(S, spsrt(S, _, _, _, _, _, _, _), SList),
  count(SList, 0, I),
  I > 2,
  1 = dlg_MessageBox( "Spelersoorten opruimen?", "Er zijn meerdere spelersoorten, tegenwoordig is dat alleen in speciale gevallen nodig!\nOpruimen? NB Dan blijven man en vrouw over, die kun je zelf weer aanpassen.", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
  combineSpelersoorten(), !.

/*  	% 23.2.2013  voor test!
toonTspsrt(TitelX) :-
  nl,
  write("\n--- ", TitelX),
  t_spsrt(I1, _Sex, Naam, MV1, LftVan, LftTot, IDC_STVAN_VALUE, IDC_STTOT_VALUE, [Titel|_]),
  write("\nIn: ",I1," ",_Sex," ",Naam," ",MV1," ",LftVan," ",LftTot," ", IDC_STVAN_VALUE, " ", IDC_STTOT_VALUE, " ", Titel),
  fail.
toonTspsrt(_).
*/

  dlg_spelersoorten_Create(_Parent):-
	samenvoegenSpelersoorten(),
	fail.
  dlg_spelersoorten_Create(Parent):-
	copySpsrt(),
	opVolgorde(0, [], LIJST_ITEMLIST),
	%findall(Naam,t_spsrt(_I, Sex, Naam, MV, LftVan, LftTot, StrkVan, StrkTot, Reserve),LIJST_ITEMLIST),
	LIJST_SELECT = [0],
	assert(indexV(0)),
%MARK Spelersoorten, new variables

	dialog_CreateModal(Parent,dlg_spelersoorten_ResID,"",
  		[
%BEGIN Spelersoorten, ControlList, 10:48:09-5.9.2009, Code automatically updated!
		df(idc_lijst,listbox(LIJST_ITEMLIST,LIJST_SELECT),nopr)
%END Spelersoorten, ControlList
		],
		dlg_spelersoorten_eh,0,VALLIST,ANSWER),
	dlg_spelersoorten_handle_answer(ANSWER,VALLIST), !.
  dlg_spelersoorten_Create(_Parent):-
	retractall(t_spsrt(_, _, _, _, _, _, _, _, _)).

  dlg_spelersoorten_handle_answer(idc_ok,VALLIST):-!,
	dlg_spelersoorten_update(VALLIST).
  dlg_spelersoorten_handle_answer(idc_cancel,_):-!.  % Handle Esc and Cancel here
  dlg_spelersoorten_handle_answer(_,_):-
	errorexit().

  dlg_spelersoorten_update(_VALLIST):-
%BEGIN Spelersoorten, Update controls, 10:48:09-5.9.2009, Code automatically updated!
	dialog_VLGetListBox(idc_lijst,_VALLIST,_LIJST_ITEMLIST,_LIJST_SELECT),
%END Spelersoorten, Update controls
	Win = vpi_GetTaskwin(),
	saveTabel(Win),
	retractall(t_spsrt(_, _, _, _, _, _, _, _, _)),
	true.

%MARK Spelersoorten, new events

%BEGIN Spelersoorten, idc_help _CtlInfo
  dlg_spelersoorten_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Spelersoorten"),
	!.
%END Spelersoorten, idc_help _CtlInfo

%BEGIN Spelersoorten, lijst activated
  dlg_spelersoorten_eh(_Win,e_Control(idc_lijst,_CtrlType,_CtrlWin,activated),0):-!,
	lbox_GetSel(_CtrlWin,_ItemList,IndexList),
	IndexList = [I1|_],
	dlg_spsoort_Create(_Win, I1),
	repr(I1, Repr),
	lbox_Delete(_CtrlWin, I1),
	lbox_Add(_CtrlWin, I1, Repr),
	lbox_SetSel(_CtrlWin, I1, b_True),
	!.
%END Spelersoorten, lijst activated

%BEGIN Spelersoorten, idc_wijzig _CtlInfo
  dlg_spelersoorten_eh(_Win,e_Control(idc_wijzig,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idc_lijst),
	I1      = lbox_GetSelIndex(LboxWin),
	dlg_spsoort_Create(_Win, I1),
	repr(I1, Repr),
	lbox_Delete(LboxWin, I1),
	lbox_Add(LboxWin, I1, Repr),
	lbox_SetSel(LboxWin, I1, b_True),
	!.
%END Spelersoorten, idc_wijzig _CtlInfo

%BEGIN Spelersoorten, idc_op _CtlInfo
  dlg_spelersoorten_eh(_Win,e_Control(idc_op,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idc_lijst),
	I1      = lbox_GetSelIndex(LboxWin),
	I1 > 0,
	I2 = I1 - 1,
	retract(t_spsrt(I1, Sex1, Naam1, DMV1, DLftVan1, DLftTot1, StrkVan1, StrkTot1, Reserve1)),
	retract(t_spsrt(I2, Sex2, Naam2, DMV2, DLftVan2, DLftTot2, StrkVan2, StrkTot2, Reserve2)),
	assert(t_spsrt(I2, Sex1, Naam1, DMV1, DLftVan1, DLftTot1, StrkVan1, StrkTot1, Reserve1)),
	assert(t_spsrt(I1, Sex2, Naam2, DMV2, DLftVan2, DLftTot2, StrkVan2, StrkTot2, Reserve2)),
	lbox_Delete(LboxWin, I1),
	repr(I2, Repr),
	lbox_Add(LboxWin, I2, Repr),
	lbox_SetSel(LboxWin, I2, b_True),
	assert(indexV(I2)),
	!.
%END Spelersoorten, idc_op _CtlInfo

%BEGIN Spelersoorten, idc_neer _CtlInfo
  dlg_spelersoorten_eh(_Win,e_Control(idc_neer,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idc_lijst),
	I1      = lbox_GetSelIndex(LboxWin),
	index(Max),
	I1 < Max,
	I2 = I1 + 1,
	retract(t_spsrt(I1, Sex1, Naam1, DMV1, DLftVan1, DLftTot1, StrkVan1, StrkTot1, Reserve1)),
	retract(t_spsrt(I2, Sex2, Naam2, DMV2, DLftVan2, DLftTot2, StrkVan2, StrkTot2, Reserve2)),
	assert(t_spsrt(I2, Sex1, Naam1, DMV1, DLftVan1, DLftTot1, StrkVan1, StrkTot1, Reserve1)),
	assert(t_spsrt(I1, Sex2, Naam2, DMV2, DLftVan2, DLftTot2, StrkVan2, StrkTot2, Reserve2)),
	lbox_Delete(LboxWin, I1),
	repr(I2, Repr),
	lbox_Add(LboxWin, I2, Repr),
	lbox_SetSel(LboxWin, I2, b_True),
	assert(indexV(I2)),
	!.
%END Spelersoorten, idc_neer _CtlInfo

%BEGIN Spelersoorten, idc_wis _CtlInfo
  dlg_spelersoorten_eh(_Win,e_Control(idc_wis,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	index(Max),
	Max > 0,	% nietLaatste(Max),
	deleteSrt(_Win),
	LboxWin = win_GetCtlHandle(_Win, idc_lijst),
	lbox_GetSel(LboxWin,_ItemList,IndexList),
	IndexList = [Index|_],
	lbox_Delete(LboxWin, Index),
	maxIndex(Index, MIndex),
	lbox_SetSel(LboxWin, MIndex, b_True),
	assert(indexV(MIndex)),
	!.
%END Spelersoorten, idc_wis _CtlInfo


%BEGIN Spelersoorten, idc_nieuw _CtlInfo
  dlg_spelersoorten_eh(_Win,e_Control(idc_nieuw,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idc_lijst),
	lbox_GetSel(LboxWin,_ItemList,IndexList),
	IndexList = [Index|_],
	Index1 = Index + 1,
	nieuwAt(Index1),
	dlg_spsoort_Create(_Win, Index1),
	repr(Index1, Repr),
	lbox_Add(LboxWin, Index1, Repr),
	lbox_SetSel(LboxWin, Index1, b_True),
	win_SetFocus(LboxWin),
	!.
  dlg_spelersoorten_eh(_Win,e_Control(idc_nieuw,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idc_lijst),
	lbox_GetSel(LboxWin,_ItemList,IndexList),
	IndexList = [Index|_],
	Index1 = Index + 1,
	shiftUp(Index1), !.
%END Spelersoorten, idc_nieuw _CtlInfo

%BEGIN Spelersoorten, e_Create
  dlg_spelersoorten_eh(_Win,e_Create(_CreationData),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idc_lijst),
	lbox_SetTabStops(LboxWin, [110, 130, 150, 170, 190, 223, 270]),
	!.
%END Spelersoorten, e_Create

%BEGIN Spelersoorten, lijst selchanged
  dlg_spelersoorten_eh(_Win,e_Control(idc_lijst,_CtrlType,_CtrlWin,selchanged),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idc_lijst),
	lbox_GetSel(LboxWin,_ItemList,_IndexList),
	!.
%END Spelersoorten, lijst selchanged

  dlg_spelersoorten_eh(_,_,_):-!,fail.

%END_DLG Spelersoorten



%BEGIN_DLG SpSoort
/**************************************************************************
	Creation and event handling for dialog: SpSoort
**************************************************************************/

constants

%BEGIN SpSoort, CreateParms, 11:17:07-24.2.2013, Code automatically updated!
  dlg_spsoort_ResID = idd_spsoort
  dlg_spsoort_DlgType = wd_Modal
  dlg_spsoort_Help = idh_spelersoorten
%END SpSoort, CreateParms

predicates

  dlg_spsoort_eh : EHANDLER
  dlg_spsoort_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST, integer Index)
  dlg_spsoort_update(DIALOG_VAL_LIST, integer Index)
  xover(string, string, string, string)

clauses

  xover(SVanI, STotI, SVanI, STotI) :-
	SVanI >= STotI, !.
  xover(SVanI, STotI, STotI, SVanI).

  dlg_spsoort_Create(Parent, I1):-
	t_spsrt(I1, _Sex, Naam, MV1, LftVan, LftTot, IDC_STVAN_VALUE, IDC_STTOT_VALUE, [Titel|_]),
	IDC_EDITFIELD_VALUE = Naam,
	mv2d_mv(MV1, MANVROUW, _),
	lf2d_lf(LftVan,IDC_LVAN_VALUE,_),
	lf2d_lf(LftTot,IDC_LTOT_VALUE,_),
	IDC_TITEL_VALUE = Titel,
%MARK SpSoort, new variables

	dialog_CreateModal(Parent,dlg_spsoort_ResID,"",
  		[
%BEGIN SpSoort, ControlList, 11:17:07-24.2.2013, Code automatically updated!
		df(idc_editfield,editstr(IDC_EDITFIELD_VALUE,[]),nopr),
		df(idc_lvan,editint(IDC_LVAN_VALUE,[range(0,99)]),nopr),
		df(idc_ltot,editint(IDC_LTOT_VALUE,[range(0,99)]),nopr),
		df(idc_stvan,editstr(IDC_STVAN_VALUE,[length(2)]),nopr),
		df(idc_sttot,editstr(IDC_STTOT_VALUE,[length(2)]),nopr),
		df(idc_titel,editstr(IDC_TITEL_VALUE,[]),nopr),
		df(MANVROUW,radiobuttongroup([idc_man,idc_vrouw]),nopr)
%END SpSoort, ControlList
		],
		dlg_spsoort_eh,0,VALLIST,ANSWER),
	dlg_spsoort_handle_answer(ANSWER,VALLIST, I1), !.

  dlg_spsoort_handle_answer(idc_ok,VALLIST, Index):- !,
	dlg_spsoort_update(VALLIST,Index).
  dlg_spsoort_handle_answer(idc_cancel,_,Index):-	% Handle Esc and Cancel here
	retract(t_spsrt(Index, _, "", m, 0, _, _, _, _Reserve)), !.
  dlg_spsoort_handle_answer(idc_cancel,_,_):- !.	% Handle Esc and Cancel here
  dlg_spsoort_handle_answer(_,_,_):-
	errorexit().

  dlg_spsoort_update(_VALLIST, Index):-
%BEGIN SpSoort, Update controls, 11:17:07-24.2.2013, Code automatically updated!
	_IDC_EDITFIELD_VALUE = dialog_VLGetstr(idc_editfield,_VALLIST),
	_IDC_LVAN_VALUE = dialog_VLGetint(idc_lvan,_VALLIST),
	_IDC_LTOT_VALUE = dialog_VLGetint(idc_ltot,_VALLIST),
	_IDC_STVAN_VALUE = dialog_VLGetstr(idc_stvan,_VALLIST),
	_IDC_STTOT_VALUE = dialog_VLGetstr(idc_sttot,_VALLIST),
	_IDC_TITEL_VALUE = dialog_VLGetstr(idc_titel,_VALLIST),
	_MANVROUW = dialog_VLGetRadiobutton(idc_man,_VALLIST),
%END SpSoort, Update controls
	retract(t_spsrt(Index, Sex1, _, _, _, _, _, _, [_|Rest])), !,
	mv2d_mv(MV, _MANVROUW, _),
	lf2d_lf(LVan,_IDC_LVAN_VALUE,_), 
	lf2d_lf(LTot,_IDC_LTOT_VALUE,_), 
	xover(_IDC_STVAN_VALUE, _IDC_STTOT_VALUE, SVan, STot),
	trim_c(_IDC_EDITFIELD_VALUE),
	assert(t_spsrt(Index, Sex1, _IDC_EDITFIELD_VALUE,
		 MV, LVan, LTot, SVan, STot, [_IDC_TITEL_VALUE|Rest])),
	true.

%MARK SpSoort, new events

%BEGIN SpSoort, idc_vrouw _CtlInfo
  dlg_spsoort_eh(_Win,e_Control(idc_vrouw,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	CtrlWin = win_GetCtlHandle(_Win, idc_titel),
	win_SetText(CtrlwIN, "Mw"),
	!.
%END SpSoort, idc_vrouw _CtlInfo


%BEGIN SpSoort, idc_help _CtlInfo
  dlg_spsoort_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Spelersoorten"),
	!.
%END SpSoort, idc_help _CtlInfo

%BEGIN SpSoort, idc_ok _CtlInfo
  dlg_spsoort_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	Naam = dialog_Getstr(_Win, idc_editfield),
	t_spsrt(_Index, _Sex1, Naam, _, _, _, _, _, _Reserve),
	t_spsrt(_, _, "", _, _, _, _, _, _),
	format(MyMsg, "%s al in gebruik!", Naam),
	dlg_MessageBox( "Soortnaam", MyMsg, mesbox_iconerror, mesbox_buttonsok, mesbox_defaultsecond, mesbox_suspendapplication ),
	%dlg_Error(MyMsg),
	!.
  dlg_spsoort_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	StVanD = dialog_Getstr(_Win, idc_stvan),
	StTotD = dialog_Getstr(_Win, idc_sttot),
	StVanD < StTotD,
	format(MyMsg, "Beide grenzen invullen"),
	dlg_MessageBox( "Speelsterktevoor deze spelersoort", MyMsg, mesbox_iconerror, mesbox_buttonsok, mesbox_defaultsecond, mesbox_suspendapplication ),
	!.
  dlg_spsoort_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LVanD = dialog_GetInt(_Win, idc_lvan),
	lf2d_lf(LVan, LVanD, _),
	LTotD = dialog_GetInt(_Win, idc_ltot),
	lf2d_lf(LTot, LTotD, _),
	LVan > LTot,
	format(MyMsg, "Beide grenzen invullen"),
	dlg_MessageBox( "Leeftijd voor deze soort", MyMsg, mesbox_iconerror, mesbox_buttonsok, mesbox_defaultsecond, mesbox_suspendapplication ),
	!.
	
%END SpSoort, idc_ok _CtlInfo

  dlg_spsoort_eh(_,_,_):-!,fail.

%END_DLG SpSoort



%BEGIN_DLG Clubstabel
/**************************************************************************
	Creation and event handling for dialog: Clubstabel
**************************************************************************/

constants

%BEGIN Clubstabel, CreateParms, 22:04:54-27.10.2011, Code automatically updated!
  dlg_clubstabel_ResID = idd_clubstabel
  dlg_clubstabel_DlgType = wd_Modal
  dlg_clubstabel_Help = idh_clubtabel
%END Clubstabel, CreateParms


database - tclub
  t_club(WINDOW, string, string, string)

predicates

  dlg_clubstabel_eh : EHANDLER
  dlg_clubstabel_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_clubstabel_update(DIALOG_VAL_LIST)
  copyClubs(WINDOW)
  nondeterm t_clubs_repr(WINDOW, string, string)
  update_clubs(WINDOW)
  nondeterm zoek_spelers_van_club(string Club, string Naam)
  destilleer(WINDOW)
  clubs_veranderd(WINDOW)
  cleanClub(string, string, string, string, string)
  printClubtabel(WINDOW)
  printClubSpelers(WINDOW)
  nondeterm clubs_repr1(WINDOW, string)
procedure  club_unf(WINDOW, string, string Found)
  nondeterm upperclub(string)
procedure clubtext(string, string)

clauses

cleanClub(Key, Key, Naam, Naam, _) :- !.
cleanClub(Key, Key, _Naam, Naam1, District) :-
  logf('A', club(Key, Naam1, District), ja), !.
cleanClub(Key, Key1, Naam, Naam1, District) :-
  logf('R', club(Key, Naam, District), ja),
  logf('A', club(Key1, Naam1, District), ja), !.
cleanClub(_, _, _, _, _).

copyClubs(Window) :-
  club(Key, Naam, District),
  Naam1 = Naam,
  trim_c(Naam1),
  upper_lower(Key1, Key),
  trim_c(Key1),
  not(Key1 = ""),
  trim_c(District),
  assert(t_club(Window, Key1,Naam1, District)),
  cleanClub(Key, Key1, Naam, Naam1, District),
  fail.
copyClubs(_) :-
  logclose().

clubs_veranderd(Win) :-
  club(Key, Naam, Distr),
  not(t_club(Win, Key, Naam, Distr)),  !. 
clubs_veranderd(Win) :-
  t_club(Win, Key, Naam, Distr),
  not(club(Key, Naam, Distr)),  !.

update_clubs(Win) :-			% wat er niet meer is
  club(Key, Naam, Distr),
  not(t_club(Win, Key, _, _)),
  logf('R', club(Key, Naam, Distr),ja),
  fail.
update_clubs(Win) :-			% volgorde niet belangrijk
  club(Key, Naam, Distr),		% alles wat hetzelfde is
  retract(t_club(Win, Key, Naam, Distr)),
  fail.
update_clubs(Win) :-	
  retract(t_club(Win, Key, Naam, Distr)),	% blijft over wat anders is
  logf('Z', club(Key, Naam, Distr),ja),
  fail.  
update_clubs(Win) :-
  logclose(),
  retractall(t_club(Win, _,_,_)).

t_clubs_repr(Win, Club, Representatie) :-
  t_club(Win, Club, Naam, District),
  club_f(Club, ClubS),
  format(Representatie,"%-5s\t%-24s\t%s", ClubS, Naam, District).

club_f(Club, ClubS) :- 		% align getallen rechts
	str_long(Club, _), !,
	str_len(Club, Clen),
	concat("00000", Club, Club1),
	frontstr(Clen, Club1, _, ClubS).
	%format(ClubS, "%5.5s", Club2). 
club_f(Club, ClubS) :- 		% align strings links
	format(ClubS, "%-5s", Club). 

club_unf(Win, ClubFound, ClubFound) :-
  t_club(Win, ClubFound, _, _), !.
club_unf(Win, ClubS, ClubFound) :-
  frontchar(ClubS,'0',ClubFound),
  t_club(Win, ClubFound, _, _), !.
club_unf(Win, ClubS, ClubFound) :-
  frontchar(ClubS,'0',Club1),
  club_unf(Win, Club1, ClubFound), !.
club_unf(_Win, ClubFound, ClubFound).

zoek_spelers_van_club(ClubUU, NaamP) :-
  sp2(_,Sex,Naam,_,_ ,_,_,_,_,ClubS,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  %sp1(_, Sex, Naam, _, _, _Pos, _, _, ClubS, _, _, _, _, _, _, _, _, _, _),
  upper_lower(ClubU, ClubS),
  trim_c(ClubU),
  ClubUU = ClubU,
  spsrt(Sex, _, MV, _, _, _, _, _),
  mv2string(MV, Str),
  format(NaamP, "%s\t%s", Str, Naam).

destilleer(Win) :-
  sp2(_,_,Naam,_,_ ,_,_,_,_,Club,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  concat(Club, "     ", Club1),
  frontstr(5, Club1, Club2, _),
  trim_c(Club2),
  not(Club2 = ""),
  upper_lower(Club3, Club2),
  not(t_club(Win, Club3, _, _)),
  trim_c(Naam),
  assert(t_club(Win,Club3, Naam, "")),
  fail.
destilleer(_).
  %closefile(nawf).

printClubtabel(Win) :-
  closefile(work),
  get_TempDir(_, TempFile),
  openwrite(work,TempFile),
  writedevice(work),
  findall(String, clubs_repr1(Win, String), Lijst),
  sortstring_c(Lijst, 4),
  write("\n Deelnemersverenigingen________________District_____"),
  member(ClubR, Lijst),
  write(ClubR),
  fail.
printClubtabel(Win) :-
  write("\n ___________________________________________________"),
  closefile(work),
  writedevice(screen),
  dlg_rapport_Create(Win, "Clubtabel","Clubs",b_False).

upperclub(ClubUp) :-
  sp2(_,_,_,_,_ ,_,_,_,_,ClubAA,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  %sp1(_,_,_,_,_,_,_,_, ClubAA, _, _, _, _, _, _, _, _, _, _),
  upper_lower(ClubUp, ClubAA).
  
clubText("", "(geen)") :- !.
clubText(Club, Club).
  
printClubSpelers(Win) :-
  closefile(work),
  get_TempDir(_, TempFile),
  openwrite(work,TempFile),
  writedevice(work),
  findall(ClubAA, upperclub(ClubAA), ClubLAA),
  sortstring_c(ClubLAA, 1),
  removeDubl(ClubLAA, [], ClubLAAS),
  member(Club, ClubLAAS),
  findall(Naam,zoek_spelers_van_club(Club, Naam), Namen),
  count(Namen, 0, Aantal),
  t_club(Win, Club, NaamCl, District),
  trim_c(NaamCl),
  clubtext(Club, ClubText),
  write("Vereniging: ", ClubText, " ", NaamCl, " ", District,"   aantal:", Aantal, "\n"),
  sortstring_c(Namen, 3),
  member(Naam, Namen),
  write(Naam, "\n"),
  fail.
printClubspelers(_) :-
  closefile(work),
  writedevice(screen).
 

clubs_repr1(Win, String) :-
  t_club(Win, Club, Naam, District),
  club_f(Club, ClubS),
  format(String, "\n | %-5s | %-24s | %-12s |", ClubS, Naam, District).

/*clubSpeler(NaamPlus) :-
  getbacktrack(Here),
  t_club(Win, _Club, _Naam, _District),
  cutbacktrack(Here),
  LboxWin = win_GetCtlHandle(Win, idc_clublijst),
  Index = lbox_GetSelIndex(LboxWin),
  Repr = lbox_GetItem(LboxWin, Index),
  searchchar(Repr, '\t', Pos),
  Pos1 = Pos - 1,
  frontstr(Pos1, Repr, Club, _),
  findall(Naamz,zoek_spelers_van_club(Club, Naamz), Namen),
  sortstring_c(Namen, 3),
  member(NaamPlus, Namen).*/


%_____________________________________________________________________________
  dlg_clubstabel_Create(Parent):-

%MARK Clubstabel, new variables

	dialog_CreateModal(Parent,dlg_clubstabel_ResID,"",
  		[
%BEGIN Clubstabel, ControlList, 22:04:54-27.10.2011, Code automatically updated!
		df(idc_clublijst,listbox([],[0]),nopr)
%END Clubstabel, ControlList
		],
		dlg_clubstabel_eh,0,VALLIST,ANSWER),
	dlg_clubstabel_handle_answer(ANSWER,VALLIST),  !.

  dlg_clubstabel_handle_answer(idc_ok,VALLIST):-!,
	dlg_clubstabel_update(VALLIST).
  dlg_clubstabel_handle_answer(idc_cancel,_):-!.  % Handle Esc and Cancel here
  dlg_clubstabel_handle_answer(_,_):-
	errorexit().

  dlg_clubstabel_update(_VALLIST):-
%BEGIN Clubstabel, Update controls, 22:04:54-27.10.2011, Code automatically updated!
	dialog_VLGetListBox(idc_clublijst,_VALLIST,_IDC_CLUBLIJST_ITEMLIST,_IDC_CLUBLIJST_SELECT),
%END Clubstabel, Update controls
	true.

%MARK Clubstabel, new events

%BEGIN Clubstabel, idc_clublijst selchanged
  dlg_clubstabel_eh(_Win,e_Control(idc_clublijst,_CtrlType,_CtrlWin,selchanged),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idc_clublijst),
	Index = lbox_GetSelIndex(LboxWin),
	Repr = lbox_GetItem(LboxWin, Index),
	searchchar(Repr, '\t', Pos),
	Pos1 = Pos - 1,
	frontstr(Pos1, Repr, Club, _ClubT),
	club_unf(_Win, Club, ClubFound),
	trim_c(ClubFound),
	upper_lower(ClubUU, ClubFound),
	retractall(selectedClub(_)),
	assert(selectedClub(ClubUU)),
	TaskWin = vpi_GetTaskWin(),
	win_spelersel_Create(TaskWin, 9),
	spelerSelRefresh(),
	!.
%END Clubstabel, idc_clublijst selchanged

%BEGIN Clubstabel, idc_afdrukken _CtlInfo
  dlg_clubstabel_eh(_Win,e_Control(idc_afdrukken,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	printClubtabel(_Win),
	!.
%END Clubstabel, idc_afdrukken _CtlInfo

%BEGIN Clubstabel, e_CloseRequest
  dlg_clubstabel_eh(_Win,e_CloseRequest,0):-!,
	clubs_veranderd(_Win),
	Ans = dlg_Ask("Clubtabel","Veranderingen bewaren?",["&OK","&Cancel", ""]),
	Ans = resp_default,
	update_clubs(_Win),
	win_Destroy(_Win),
	!.
%END Clubstabel, e_CloseRequest

%BEGIN Clubstabel, idc_cancel _CtlInfo
  dlg_clubstabel_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-
	clubs_veranderd(_Win),
	1 = dlg_MessageBox( "Clubstabel", "Veranderingen bewaren?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
	update_clubs(_Win),
	win_Destroy(_Win),
	!.
%END Clubstabel, idc_cancel _CtlInfo

%BEGIN Clubstabel, e_Destroy
  dlg_clubstabel_eh(_Win,e_Destroy,0):-!,
	updateVensterpositie(clubvenster, _Win),
	retractall(t_club(_Win, _,_,_)).
%END Clubstabel, e_Destroy

%BEGIN Clubstabel, idc_ok _CtlInfo
  dlg_clubstabel_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	update_clubs(_Win),
	win_Destroy(_Win),
	!.
%END Clubstabel, idc_ok _CtlInfo

%BEGIN Clubstabel, idc_help _CtlInfo
  dlg_clubstabel_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Clubtabel"),
	!.
%END Clubstabel, idc_help _CtlInfo

%BEGIN Clubstabel, clublijst activated
  dlg_clubstabel_eh(_Win,e_Control(idc_clublijst,_CtrlType,_CtrlWin,activated),0):-!,
	Index = lbox_GetSelIndex(_CtrlWin),	% zelfde als verander
	Repr = lbox_GetItem(_CtrlWin, Index),
	searchchar(Repr, '\t', Pos),
	Pos1 = Pos - 1,
	frontstr(Pos1, Repr, Key, _),
	trim_c(Key),
	dlg_clubgegevens_Create(_Win, Key, NKey),
	NKey <> "",
	lbox_Delete(_CtrlWin, Index),
	t_clubs_repr(_Win, Key, Representatie),
	lbox_Add(_CtrlWin, Index, Representatie), % insert
	lbox_SetSel(_CtrlWin, Index, b_True),	% en show
	!.
%END Clubstabel, clublijst activated

%BEGIN Clubstabel, idc_verander _CtlInfo
  dlg_clubstabel_eh(_Win,e_Control(idc_verander,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idc_clublijst),
	Index = lbox_GetSelIndex(LboxWin),
	Repr = lbox_GetItem(LboxWin, Index),
	searchchar(Repr, '\t', Pos),
	Pos1 = Pos - 1,
	frontstr(Pos1, Repr, Key, _),
	trim_c(Key),
	dlg_clubgegevens_Create(_Win, Key, NKey),
	NKey <> "",
	lbox_Delete(LboxWin, Index),		% update display
	t_clubs_repr(_Win, Key, Representatie),
	lbox_Add(LboxWin, Index, Representatie), % insert
	lbox_SetSel(LboxWin, Index, b_True),	% en show
	!.
%END Clubstabel, idc_verander _CtlInfo

%BEGIN Clubstabel, idc_af _CtlInfo
  dlg_clubstabel_eh(_Win,e_Control(idc_af,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idc_clublijst),
	Index = lbox_GetSelIndex(LboxWin),
	Repr = lbox_GetItem(LboxWin, Index),
	searchchar(Repr, '\t', Pos),
	Pos1 = Pos - 1,
	frontstr(Pos1, Repr, Key, _),
	trim_c(Key),
	retract(t_club(_Win, Key, _, _)),
	lbox_Delete(LboxWin, Index),
	Aantal = lbox_Countall(LboxWin),	% zet nieuwe selectie
	kleinste(Aantal, Index, Index1),
	lbox_SetSel(LboxWin, Index1, b_True), 
	!.
%END Clubstabel, idc_af _CtlInfo

%BEGIN Clubstabel, idc_bij _CtlInfo
  dlg_clubstabel_eh(_Win,e_Control(idc_bij,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idc_clublijst),
	assert(t_club(_Win,"","","")),
	dlg_clubgegevens_Create(_Win, "", Club),
	retractall(t_club(_Win, "", _, _)),	% was maar tijdelijk
	Club <> "",				% geen nieuwe
	Strings = lbox_GetAll(LboxWin),
	searchslist_c(Strings, Club, 1,NPos),	% zoek plaats voor insert
	t_clubs_repr(_Win, Club, Representatie),
	lbox_Add(LboxWin, NPos, Representatie), % insert
	lbox_SetTopIndex(LboxWin, NPos),
	lbox_SetSel(LboxWin, NPos, b_True),	% en show
	!.
%END Clubstabel, idc_bij _CtlInfo

%BEGIN Clubstabel, idc_vergaar_clubs _CtlInfo
  dlg_clubstabel_eh(_Win,e_Control(idc_vergaar_clubs,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	1 = dlg_MessageBox( "Clubs vergaren", "Pseudo clubs aanmaken van clubcodes zonder naam?\nZie help.", mesbox_iconexclamation, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
	LboxWin = win_GetCtlHandle(_Win, idc_clublijst),
	destilleer(_Win),
	findall(String, t_clubs_repr(_Win, _, String), Lijst),
	sortstring_c(Lijst, 1),
	lbox_Clear(LboxWin),
	lbox_Add(LboxWin, Lijst),
	lbox_SetSel(LboxWin, 0, b_True),
	!.
%END Clubstabel, idc_vergaar_clubs _CtlInfo

%BEGIN Clubstabel, idc_spelers_van_een_club _CtlInfo
  dlg_clubstabel_eh(_Win,e_Control(idc_spelers_van_een_club,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	printClubSpelers(_Win),
	dlg_rapport_Create(_Win, "Spelers per vereniging.","Clubs",b_False),
	!.
%END Clubstabel, idc_spelers_van_een_club _CtlInfo

%BEGIN Clubstabel, idc_wis_tabel _CtlInfo
  dlg_clubstabel_eh(_Win,e_Control(idc_wis_tabel,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idc_clublijst),
	retractall(t_club(_Win, _,_,_)),
	lbox_Clear(LboxWin),
	!.
%END Clubstabel, idc_wis_tabel _CtlInfo


%BEGIN Clubstabel, e_Create
  dlg_clubstabel_eh(_Win,e_Create(_CreationData),0):-!,
	dialoogpositie(clubvenster, _Win),
	LboxWin = win_GetCtlHandle(_Win, idc_clublijst),
	lbox_setTabStops(LboxWin, [32, 127, 137, 144, 154]),
	copyClubs(_Win),
	findall(String, t_clubs_repr(_Win, _, String), Lijst),
	sortstring_c(Lijst, 1),
	lbox_Add(LboxWin, Lijst),
	lbox_SetSel(LboxWin, 0, b_True),
	Lijst = [Repr|_],
	searchchar(Repr, '\t', Pos),
	Pos1 = Pos - 1,
	frontstr(Pos1, Repr, Club, _ClubT),
	club_unf(_Win, Club, ClubFound),
	trim_c(ClubFound),
	upper_lower(ClubUU, ClubFound),
	retractall(selectedClub(_)),
	assert(selectedClub(ClubUU)),
	TaskWin = vpi_GetTaskWin(),
	win_spelersel_Create(TaskWin, 9),
	spelerSelRefresh(),
	!.
%END Clubstabel, e_Create

  dlg_clubstabel_eh(_,_,_):-!,fail.

%END_DLG Clubstabel


%BEGIN_DLG Clubgegevens
/**************************************************************************
	Creation and event handling for dialog: Clubgegevens
**************************************************************************/

constants

%BEGIN Clubgegevens, CreateParms, 23:08:58-15.9.1999, Code automatically updated!
  dlg_clubgegevens_ResID = idd_clubgegevens
  dlg_clubgegevens_DlgType = wd_Modal
  dlg_clubgegevens_Help = idh_clubtabel
%END Clubgegevens, CreateParms

predicates

  dlg_clubgegevens_eh : EHANDLER
  dlg_clubgegevens_handle_answer(WINDOW Parent, INTEGER EndButton,DIALOG_VAL_LIST, string NKey)
  dlg_clubgegevens_update(DIALOG_VAL_LIST, string, string, string)

clauses

nieuwe_club(Win, Club) :-			% global ! niet vanuit tabel
	trim_c(Club),
	assert(t_club(Win,"",Club,"")),
	dlg_clubgegevens_Create(Win, "", ClubKey),
	retractall(t_club(Win, "" , _, "")),	% was maar tijdelijk 14.9.2000
	ClubKey <> "",
	retract(t_club(Win, ClubKey, Descr, Distr)),			% geen nieuwe
	Descr <> "",
	logf('Z',club(ClubKey, Descr, Distr),nee),
	!.

  dlg_clubgegevens_Create(Parent, IDC_CLUBKEY_VALUE, NKey):-
	club_unf(Parent, IDC_CLUBKEY_VALUE, ClubFound),
	t_club(Parent, ClubFound,IDC_CLUBNAAM_VALUE,IDC_DISTRICT_VALUE),
%MARK Clubgegevens, new variables

	dialog_CreateModal(Parent,dlg_clubgegevens_ResID,"",
  		[
%BEGIN Clubgegevens, ControlList, 23:08:58-15.9.1999, Code automatically updated!
		df(idc_clubkey,editstr(IDC_CLUBKEY_VALUE,[mandatory,length(5),upper]),dlg_prompt(idct_naamkort)),
		df(idc_clubnaam,editstr(IDC_CLUBNAAM_VALUE,[]),nopr),
		df(idc_district,editstr(IDC_DISTRICT_VALUE,[]),nopr)
%END Clubgegevens, ControlList
		],
		dlg_clubgegevens_eh,0,VALLIST,ANSWER),
	dlg_clubgegevens_handle_answer(Parent, ANSWER,VALLIST,NKey), !.

  dlg_clubgegevens_handle_answer(Parent, idc_ok,VALLIST, Key):- !,
	dlg_clubgegevens_update(VALLIST, Key, Naam, Distr),
	retractall(t_club(Parent, Key, _, _)),
	assert(t_club(Parent,Key, Naam, Distr)).
  dlg_clubgegevens_handle_answer(_,idc_cancel,_,""):-!.  % Handle Esc and Cancel here
  dlg_clubgegevens_handle_answer(_,_,_,""):-
	errorexit().

  dlg_clubgegevens_update(_VALLIST, _IDC_CLUBKEY_VALUE, _IDC_CLUBNAAM_VALUE, _IDC_DISTRICT_VALUE):-
%BEGIN Clubgegevens, Update controls, 23:08:58-15.9.1999, Code automatically updated!
	_IDC_CLUBKEY_VALUE = dialog_VLGetstr(idc_clubkey,_VALLIST),
	_IDC_CLUBNAAM_VALUE = dialog_VLGetstr(idc_clubnaam,_VALLIST),
	_IDC_DISTRICT_VALUE = dialog_VLGetstr(idc_district,_VALLIST),
%END Clubgegevens, Update controls
	trim_c(_IDC_CLUBKEY_VALUE),
	trim_c(_IDC_CLUBNAAM_VALUE),
	true.

%MARK Clubgegevens, new events

%BEGIN Clubgegevens, idc_help _CtlInfo
  dlg_clubgegevens_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Clubtabel"),
	!.
%END Clubgegevens, idc_help _CtlInfo

%BEGIN Clubgegevens, idc_ok _CtlInfo
  dlg_clubgegevens_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	Str = dialog_GetStr(_Win, idc_clubkey),
	trim_c(Str),
	ParentWin = win_GetParent(_Win),
	t_club(ParentWin,Str,_,_),
	t_club(ParentWin,"",_,_),		% en we zijn met een nieuwe bezig
	format(EMsg, "Club %s bestaat al:", Str),
	dlg_Error(EMsg),
	!.
%END Clubgegevens, idc_ok _CtlInfo

%BEGIN Clubgegevens, e_Create
  dlg_clubgegevens_eh(_Win,e_Create(_CreationData),0):-!,
	Str = dialog_GetStr(_Win, idc_clubkey),
	Str <> "",
	dialog_SetState(_Win, [enable(idc_clubkey, b_False)]),
	!.
%END Clubgegevens, e_Create

  dlg_clubgegevens_eh(_,_,_):-!,fail.

%END_DLG Clubgegevens



%BEGIN_DLG Dagen
/**************************************************************************
	Creation and event handling for dialog: Dagen
**************************************************************************/

domains 
  dagd = d(integer, string, wrk, BOOLEAN)
  dagdl = dagd*
  bnd = b(integer, integer)
  bndl = bnd*

database - tdag
  t_dag(integer, string, wrk)	% werk-copy van de dagtabel
  %t_dag1(integer, string, wrk, integer Oudedag)	% werk-copy van de dagtabel
  wisdag(integer DagNo)
  single dagnum(ulong, BOOLEAN ReadOnly)
  determ ingrijpend()		% om de gebruiker te waarschuwen
  single oudedatum(BOOLEAN)
  single dagTeller(integer)
  
  %t_sp_verh(integer, verhl)	% verhinderingen die gedelete gaan worden
  %t_bn_del(tijd)		% baanbeschikbaarheid die .. gaat worden

constants

%BEGIN Dagen, CreateParms, 22:12:03-27.10.2011, Code automatically updated!
  dlg_dagen_ResID = idd_dagen
  dlg_dagen_DlgType = wd_Modal
  dlg_dagen_Help = idh_dagen
%END Dagen, CreateParms

predicates

  dlg_dagen_eh : EHANDLER
  dlg_dagen_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_dagen_update(DIALOG_VAL_LIST)
  copyDagen
procedure update_dagen()
procedure update_dagen(ulist, ulist)
%procedure update_dagen(integerlist, integerlist)
procedure update_dagen1(ulist)
  wrk2str(wrk, string, dialog_control_id)
  nondeterm reprd(integer, string)
  repr2dagno(string, integer)
  af_niet_mogelijk(WINDOW, integer)
procedure  zeef_verhinderingen(ilist,verhl,verhl) -  (i,i,o)
  check_verhinderingen(integer, verhl)
procedure  zeef_sp(ilist)	% remove en update
procedure  zeef_bn(ilist)
nondeterm bnel(bnd)
zeef_bn(ilist, bndl)
  %zeef_bn(ilist, tijd, integer)
  dagenVeranderd()
procedure  zeeflijst(ilist)
nondeterm verpmem(integer, integer, ilist)
determ schuiftijd(tijd, ilist, tijd)
procedure schuiftijdPr(tijd, ilist, tijd)
procedure zeef_planning(ilist)
procedure zeef_was(ilist)
procedure zeef_bl(ilist)
  zeef_bl(wedscat Cat, verhl, verhl)
procedure  zeef_bloktijden(ilist Lijst, verhl, verhl, verhl)
procedure zeef_uitslag(ilist)
procedure zeef_wed(ilist)
procedure orderdagen
%nondeterm jaar(ulong, ulong)
%  fouteDagL(ulong, ulist)
%  oplopend(ulist, ulong)
  bSubclass_eh : EHANDLER  
procedure dag2Werkdag(integer DayOfWeek, wrk Werkdag)
procedure nieuwjaarsboodschap()

clauses

dagnum(0, b_False).
oudedatum(b_True).
dagTeller(0).

zeeflijst(Lijst) :-
  %write("\nZeefLijst", Lijst),
  zeef_wed(Lijst),
  zeef_bn(Lijst),
  zeef_sp(Lijst),
  zeef_planning(Lijst),
  zeef_was(Lijst),
  zeef_bl(Lijst),
  zeef_uitslag(Lijst).

copyDagen() :-
  retractall(wisdag(_)),
  retractall(t_dag(_,_,_)),
  dag(No, Str, Wrk),
  assert(t_dag(No, Str, Wrk)),
  fail.
copyDagen.   

dagenVeranderd :-
  dag(No, Tekst, Wrk),
  not(t_dag(No, Tekst, Wrk)), !.
  %write("\nDag No: ",No), !.
dagenVeranderd :-
  t_dag(No, Tekst, Wrk),
  not(dag(No, Tekst, Wrk)), !.
  %write("\nTdag No: ",No), !.

update_dagen :-
  %loglock(),
  findall(Dag, dag(Dag,_,_),Dagen),
  sortuint_c(Dagen),
  findall(Day, t_dag(Day,_,_), Days),
  sortuint_c(Days),
  %write("\nDagen: ", Dagen, "\n Days: ", Days),
  dagenVeranderd(),
  update_dagen(Dagen, Days), !.
update_dagen.

update_dagen([Dg|Dagen], [Dg|Days]) :-
  dag(Dg, Naam, Wrk),
  retract(t_dag(Dg, Naam, Wrk)), !,
  %assert(dagnum(Dg)),
  update_dagen(Dagen, Days).		% gaat gelijk op
update_dagen(Dagen, _) :-
  member(Dag, Dagen),
  dag(Dag, T, W),
  retract(dag(Dag, T, W)),
  not(t_dag(Dag, _, _)),		% komt niet meer voor
  assert(wisdag(Dag)),
  fail.
update_dagen(_Dagen, Days) :-
  update_dagen1(Days),
  zeeflijst([]),			% kan failen
  getbacktrack(Here),
  dag(Dag, _, _),
  cutbacktrack(Here),
  dt_dateoffset_to_str(Dag, "%Dd-%Md-%YL", DatumN),
  naam(TNaam, TNr, _, Ticket, District, Res),
  assert(naam(TNaam, TNr, DatumN, Ticket, District, Res)),
  compress(update),
  startDbRefresh(), !.
update_dagen(_, _).

update_dagen1([No|Days]) :-
  retract(t_dag(No, Tekst,Wrk)),
  assertz(dag(No, Tekst, Wrk)), !,
  update_dagen1(Days).
update_dagen1(Days) :-
  not(Days = []),
  writeToJournaal("\nDays niet leeg!"),
  fail.
update_dagen1(_Days).

wrk2str(w, "ja", idc_werkdag_uit_kantoortijd_geldt).
wrk2str(z, "nee", idc_vrije_dag).

reprd(No, Repr) :-
  t_dag(No, _Naam, Wrk),
  wrk2str(Wrk, WS, _),
  dt_dateoffset_to_str(No, "%Dn %DD/%MD %YL", Repr0),
  format(Repr, "%s\t%s", Repr0, WS).
  
repr2dagno(Repr, No) :-
  reprd(No, Repr1),
  Repr = Repr1, !.

verpmem(Van, Naar, [Van,Naar|_]).
verpmem(Van, Naar, [_|Rest]) :-
  verpmem(Van, Naar, Rest).

schuiftijdPr(Tijd, Lijst, NewTijd) :-	% procedure
  schuiftijd(Tijd, Lijst, NewTijd), !.
schuiftijdPr(Tijd, _, Tijd).
  

schuiftijd(Tijd, Lijst, NewTijd) :-	% kan failen
  tijd_kwart(D,_,_,Tijd), 
  verpmem(D, NewD, Lijst), !,
  bitand(Tijd, 0x7F, TijdA),
  bitleft(NewD, 7, NewDA),
  bitor(TijdA, NewDA, NewTijd).

zeef_wed(Lijst) :-
  wd(No,Cat,Opg1,Winnaar,Uitsl,Stamp,Note),
  schuiftijdPr(Stamp, Lijst, Stamp1),
  retractall(wd(No,Cat,_,_,_,_,_)),
  asserta(wd(No,Cat,Opg1,Winnaar,Uitsl,Stamp1,Note)),
  fail.
zeef_wed(_Lijst).

zeef_bl(Lijst) :-
  bl(Cat, BLijst),
  zeef_bloktijden(Lijst, BLijst, [], BLijstN),
  zeef_bl(Cat, BLijst, BLijstN),
  fail.
zeef_bl(_).

zeef_bl(_Cat, Lijst, Lijst) :- !.
zeef_bl(Cat, _Lijst, []) :-
  %logf('R',bl(Cat, Lijst),ja), !.
  retractall(bl(Cat, _)), !.
zeef_bl(Cat, _Lijst, LijstN) :-
  %logf('A', bl(Cat, LijstN),ja), !.
  retractall(bl(Cat, _)),
  asserta(bl(Cat, LijstN)), !.

zeef_bloktijden(Lijst, [per(BlT1,BlT2)|Rest], In, [per(BlT1N,BlT2N)|Uit]) :-
  schuiftijd(BlT1, Lijst, BlT1N), 
  schuiftijdPr(BlT2, Lijst, BlT2N), !,
  zeef_bloktijden(Lijst, Rest, In, Uit).
zeef_bloktijden(Lijst, [H|Rest], In, [H|Uit]) :-
  zeef_bloktijden(Lijst, Rest, In, Uit), !.
zeef_bloktijden(_, _, In, In).
  
zeef_was(_) :-
  ws(Pos, Cat, Tgnst, Tijd),
  tijd_kwart(D,_,_,Tijd), 
  wisdag(D), 
  retractall(ws(Pos, Cat, Tgnst, Tijd)),
  fail.
zeef_was(Lijst) :-
  ws(Pos, Cat, Tgnst, Tijd),
  schuiftijdPr(Tijd, Lijst, TijdN),
  retractall(ws(Pos, Cat, Tgnst, Tijd)),
  asserta(ws(Pos, Cat, Tgnst, TijdN)),
  fail.
zeef_was(_).

zeef_planning(_) :-				% verwijderen
  w2(Num,Cat,Plan,_W1,_W2,_Fixed,_TStamp,_Baan),
  tijd_kwart(D,_,_,Plan), 
  wisdag(D), 
  retractall(w2(Num,Cat,_,_,_,_,_,_)),
  fail.
zeef_planning(Verschuiflijst) :-		% verschuiven
  w2(Num,Cat,Plan,W1,W2,Fixed,TStamp,Baan),
  schuiftijdPr(Plan, VerschuifLijst, PlanNew),
  schuiftijdPr(TStamp, VerschuifLijst, TStampN),
  retractall(w2(Num,Cat,_,_,_,_,_,_)),
  asserta(w2(Num,Cat,PlanNew,W1,W2,Fixed,TStampN,Baan)),
  fail.
zeef_planning(_).

zeef_uitslag(Lijst) :-
  w3(No,Cat,Win,Punten1, Punten2, Status, Datum),	% er is een winnaar
  schuiftijdPr(Datum, Lijst, DatumN),
  retractall(w3(No, Cat, _, _, _, _, _)),
  asserta(w3(No, Cat, Win, Punten1, Punten2, Status, DatumN)),
  fail.
zeef_uitslag(_).

zeef_sp(VerschuifLijst) :-	
  sp2(SKeus,Sex,Naam,Telthuis,Verhinderingen,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Res),
  zeef_verhinderingen(VerschuifLijst,Verhinderingen,VerhNieuw),
  not(Verhinderingen = VerhNieuw),
  retractall(sp2(SKeus,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)),
  asserta(sp2(SKeus,Sex,Naam,Telthuis,VerhNieuw,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Res)),
  fail. 
zeef_sp(_).	

zeef_verhinderingen(_,[],[]) :- !.
zeef_verhinderingen(Lijst,[per(Tijd,_)|Tail],Out) :-	% verwijderen
  tijd_kwart(D,_,_,Tijd), 
  wisdag(D), !,
  zeef_verhinderingen(Lijst,Tail,Out).
zeef_verhinderingen(Lijst,[per(Tijd1,Tijd2)|Tail],[per(Tijd1B, Tijd2B)|Out]) :-
  schuiftijd(Tijd1, Lijst, Tijd1B), 			% verschuiven
  schuiftijdPr(Tijd2, Lijst, Tijd2B), !,
  zeef_verhinderingen(Lijst,Tail,Out).
zeef_verhinderingen(Lijst,[H|Tail],[H|Out]) :-
  zeef_verhinderingen(Lijst,Tail,Out).

check_verhinderingen(DagNo,[per(Tijd,_)|Tail]) :-
  tijd_kwart(D,_,_,Tijd), 
  D = DagNo, !,
  check_verhinderingen(DagNo,Tail).
check_verhinderingen(DagNo,[_|Tail]) :-
  check_verhinderingen(DagNo,Tail).

bnel(b(Tijd, Aantal)) :-
  retract(bn(Tijd, Aantal)).

zeef_bn(Lijst) :-
  findall(BNElem, bnel(BNElem), BNList),
  %member(Tijd1, Tijden),
  %write("\nZeef_bn: ", Lijst, " \n", BNList), 
  zeef_bn(Lijst, BNList),
  fail.
zeef_bn(_).

zeef_bn(Lijst,[b(Tijd, _)|Rest]) :-			% verwijderen
  bitright(Tijd,7,D),
  wisdag(D), !,
  %beep(),
  zeef_bn(Lijst,Rest).
zeef_bn(Lijst, [b(Tijd, Aantal)|Rest]) :-			% verschuiven
  schuiftijdPr(Tijd, Lijst, TijdB),
  assertz(bn(TijdB, Aantal)), !,
  zeef_bn(Lijst, Rest).
zeef_bn(_, _).
    
af_niet_mogelijk(_, DagNo) :-   	% als true gaat het niet door ... 
  w2(_,_,Tijd,_,_,_,_,_),
  bitright(Tijd,7,D1), 
  D1 = DagNo, !, 
  dlg_Note("Dag nog in gebruik","er is nog planning voor deze dag").
af_niet_mogelijk(_, DagNo) :-   	% als true gaat het niet door ... 
  ws(_,_,_,Tijd),
  bitright(Tijd,7,D1), 
  D1 = DagNo, !, 
  dlg_Note("Dag nog in gebruik","er zijn nog personen niet afgezegd voor deze dag").
af_niet_mogelijk(Win, DagNo) :-    
  bn(Tijd,_),
  bitright(Tijd,7,D),
  D = DagNo, !,
  A = dlg_Ask(Win, "Baanbeschikbaarheid",
  			"De baanbeschikbaarheid bij deze dag\nVerwijderen?",["&Ja","&Cancel"]),
  A <> resp_default, !.
af_niet_mogelijk(Win, DagNo) :-  
  sp2(_,_,_,_,Verhinderingen ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  check_verhinderingen(DagNo,Verhinderingen), 
  A = dlg_Ask(Win, "Verhinderingen voor deze dag","Verwijderen?",["&Ja","&Cancel"]),
  A <> resp_default, !.

orderDagen() :-
  findall(D, t_dag(D,_,_), DL),
  sortuint_c(DL),
  member(D1, DL),
  getbacktrack(Here),
  retract(t_dag(D1, Naam, Wrk)),
  cutbacktrack(Here),
  assertz(t_dag(D1, Naam, Wrk)),
  fail.
orderDagen.

dag2Werkdag(6, z) :- !.
dag2Werkdag(7, z) :- !.
dag2Werkdag(_, w).

bSubclass_eh(_Win,e_Keydown(k_ins,c_Nothing),0):-
	ParentWin = win_GetParent(_Win),
	trap(CW = win_GetCtlHandle(ParentWin, idc_bij),_,fail),
	win_SendEvent(ParentWin,e_Control(idc_bij,wc_PushButton,CW, activated())),
  !.
bSubclass_eh(_Win,e_Keydown(k_del,c_Nothing),0):-
	ParentWin = win_GetParent(_Win),
	trap(CW = win_GetCtlHandle(ParentWin, idc_af),_,fail),
	win_SendEvent(ParentWin,e_Control(idc_af,wc_PushButton,CW, activated())),
  !.
/*bSubclass_eh(_LWin,e_KeyUp('\27',_ShiftCtlAlt),0):-!,
  Parent=win_GetParent(_LWin),
  win_Destroy(Parent).
bSubclass_eh(_LWin,e_Char(Kar,_ShiftCtlAlt),0):-!,
  Parent=win_GetParent(_LWin),
  win_SendEvent(Parent,e_Control(Kar,_ShiftCtlAlt)), !.*/


%_____________________________________________________________________
  dlg_dagen_Create(Parent):-
	copyDagen(),
	findall(Repr, reprd(_, Repr), DAGEN_ITEMLIST),
	%DAGEN_ITEMLIST = ["\t|\t|\t|\t|"|DAGEN_ITEMLIST1],
	DAGEN_SELECT = [0],
%MARK Dagen, new variables

	dialog_CreateModal(Parent,dlg_dagen_ResID,"",
  		[
%BEGIN Dagen, ControlList, 22:12:03-27.10.2011, Code automatically updated!
		df(idc_dagen,listbox(DAGEN_ITEMLIST,DAGEN_SELECT),nopr)
%END Dagen, ControlList
		],
		dlg_dagen_eh,0,VALLIST,ANSWER),
	dlg_dagen_handle_answer(ANSWER,VALLIST).

  dlg_dagen_handle_answer(idc_ok,VALLIST):-!,
	dlg_dagen_update(VALLIST),
	assert(dagnum(0, b_True)),
	update_dagen(),
	retractall(t_dag(_,_,_)),
	retractall(wisdag(_)),
	retractall(ingrijpend()), !.
  dlg_dagen_handle_answer(idc_cancel,_):-!,  % Handle Esc and Cancel here
	assert(dagnum(0, b_True)),
	retractall(t_dag(_,_,_)),
	retractall(wisdag(_)),
	retractall(ingrijpend()), !.
  dlg_dagen_handle_answer(_,_):-
	errorexit().

nieuwjaarsboodschap() :-
  findall(T, t_dag(T, _, _), TList),
  reverse(TList, [], TListR),
  TListR = [NJ, OJ | _],
  %dt_date_to_offset(NYear1, _Month, _Day, NJ),
  dt_offset_to_date(NJ, NYr,_NM, _NW, _ND, _NDw),
  dt_offset_to_date(OJ, OYr,_OM, _OW, _OD, _ODw),
  NYr > OYr,
  _A = dlg_MessageBox( "Tip!", 
  "Kennelijk wordt de kalender aangepast aan een nieuw jaar.\nVergeet niet Toernooi, Toernooimap, Grote schoonmaak te doen!", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  !.
nieuwjaarsboodschap().

  dlg_dagen_update(_VALLIST):-
%BEGIN Dagen, Update controls, 22:12:03-27.10.2011, Code automatically updated!
	dialog_VLGetListBox(idc_dagen,_VALLIST,_DAGEN_ITEMLIST,_DAGEN_SELECT),
%END Dagen, Update controls
	true.

%MARK Dagen, new events

%BEGIN Dagen, idc_dagen selchanged
  dlg_dagen_eh(_Win,e_Control(idc_dagen,_CtrlType,LboxWin,selchanged),0):-
	CtrlWin = win_GetCtlHandle(_Win, idc_invoegen),
	win_SetState(CtrlWin, [wsf_Disabled]),
	Index = lbox_GetSelIndex(LboxWin),
	Index = lbox_GetSelIndex(LboxWin),
	Repr = lbox_GetItem(LboxWin, Index),
	repr2dagno(Repr, Dag),
	D1 = Dag - 1,
	not(t_dag(D1, _, _)),
	win_SetState(CtrlWin, [wsf_Enabled]),
	!.
%END Dagen, idc_dagen selchanged


%BEGIN Dagen, idc_invoegen _CtlInfo
  dlg_dagen_eh(_Win,e_Control(idc_invoegen,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idc_dagen),
	Index = lbox_GetSelIndex(LboxWin),
	Repr = lbox_GetItem(LboxWin, Index),
	repr2dagno(Repr, Dag),
	DagNo = Dag - 1,
	dt_offset_to_date(DagNo, _, _, _, _, DayOfWeek),
	dag2Werkdag(DayOfWeek, Werkdag),
	assertz(t_dag(DagNo, "", Werkdag)),
	lbox_Add(LboxWin, Index, ""),
	lbox_SetSel(LboxWin, Index, b_True),
	dlg_dagdefinitie_Create(_Win, b_False, DagNo, _),
	retractall(t_dag(_, "", _)),
	orderDagen(),
	findall(Reprx, reprd(_, Reprx), DAGEN_ITEMLIST),
	lbox_Suspend(LboxWin),
	lbox_Clear(LboxWin),
	lbox_Add(LboxWin, DAGEN_ITEMLIST),
	lbox_Resume(LboxWin),
	lbox_SetSel(LboxWin, Index, b_True),
	nieuwjaarsBoodschap(),
	!.
%END Dagen, idc_invoegen _CtlInfo

%BEGIN Dagen, idc_help _CtlInfo
  dlg_dagen_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Dagen"),
	!.
%END Dagen, idc_help _CtlInfo

%BEGIN Dagen, idc_dagen activated
  dlg_dagen_eh(_Win,e_Control(idc_dagen,_CtrlType,LboxWin,activated),0):-!,
	%LboxWin = win_GetCtlHandle(_Win, idc_dagen),
	Index = lbox_GetSelIndex(LboxWin),
	Repr = lbox_GetItem(LboxWin, Index),
	repr2dagno(Repr, Item),
	dlg_dagdefinitie_Create(_Win, b_True, Item, _DagNew),
	reprd(Item, NRepr),
	lbox_Delete(LboxWin, Index),
	lbox_Add(LboxWin, Index, NRepr),
	lbox_SetSel(LboxWin, Index, b_True),
	!.
%END Dagen, idc_dagen activated


%BEGIN Dagen, idc_cancel _CtlInfo
  dlg_dagen_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	dagenVeranderd(),
	Ans = dlg_Ask("Veranderingen","Bewaren?",["&Ja","&Nee"]),
	Ans = resp_default,
	update_dagen(),
	win_Destroy(_Win),
	!.
%END Dagen, idc_cancel _CtlInfo

%BEGIN Dagen, idc_verander _CtlInfo
  dlg_dagen_eh(_Win,e_Control(idc_verander,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idc_dagen),
	Index = lbox_GetSelIndex(LboxWin),
	Repr = lbox_GetItem(LboxWin, Index),
	repr2dagno(Repr, Item),
	dlg_dagdefinitie_Create(_Win, b_True, Item, _DagUit),
	orderDagen(),
	reprd(Item, NRepr),
	lbox_Delete(LboxWin, Index),
	lbox_Add(LboxWin, Index, NRepr),
	lbox_SetSel(LboxWin, Index, b_True),
	!.
%END Dagen, idc_verander _CtlInfo

%BEGIN Dagen, idc_af _CtlInfo
  dlg_dagen_eh(_Win,e_Control(idc_af,_CtrlType,_CtrlWin,_CtlInfo),0):- !,
	LboxWin = win_GetCtlHandle(_Win, idc_dagen),
	Index = lbox_GetSelIndex(LboxWin),
	Repr = lbox_GetItem(LboxWin, Index),
	repr2dagno(Repr, DagNo),
	not(af_niet_mogelijk(_Win, DagNo)),
	retract(t_dag(DagNo, _, _)),
	%Index1 = Index - 1,
	lbox_Delete(LboxWin, Index),
	NoOfItems = lbox_CountAll(LboxWin),
	EndIndex = NoOfItems - 1,
	kleinste(EndIndex, Index, NewIndex), 
	NoOfItems > 0,		% voor de delete moet er minstens 1 geweest zijn 
	lbox_SetSel(LboxWin, NewIndex, b_True),
	!.
%END Dagen, idc_af _CtlInfo

%BEGIN Dagen, idc_bij _CtlInfo
  dlg_dagen_eh(_Win,e_Control(idc_bij,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idc_dagen),
	findall(Dx, t_dag(Dx,_,_), DxL),
	sortint_c(DxL),
	reverse(DxL,[],DxRev),
	DxRev = [DagNo|_],
	DagNo1 = DagNo + 1,
	NoOfItems = lbox_CountAll(LboxWin),
	lbox_Add(LboxWin, ""),			% plak lege achteraan
	dt_offset_to_date(DagNo1, _, _, _, _, DayOfWeek),
	dag2Werkdag(DayOfWeek, Werkdag),
	assertz(t_dag(DagNo1, "", Werkdag)),
	lbox_SetSel(LboxWin, NoOfItems, b_True),
	OldIndex1 = lbox_GetSelIndex(LboxWin),
	OldIndex = OldIndex1 - 1,
	dlg_dagdefinitie_Create(_Win, b_False, DagNo1,DagNew),
	retractall(t_dag(_,"",_)),
	lbox_Delete(LboxWin, NoOfItems),
	lbox_SetSel(LboxWin, OldIndex, b_True),	!,	% zet even terug igv cancel
	t_dag(DagNew, _Naam, _),			% geen cancel want bestaat nog
	nieuwjaarsBoodschap(),
	orderDagen(),
	findall(DStr, reprd(_, DStr), DList),
	lbox_Suspend(LboxWin),
	lbox_Clear(LboxWin),
	lbox_Add(LboxWin, DList),
	lbox_Resume(LboxWin),
	lbox_SetSel(LboxWin, NoOfItems, b_True),
	nieuwjaarsBoodschap(),
	!.
  dlg_dagen_eh(_Win,e_Control(idc_bij,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idc_dagen),
	date(Year,Month,Day),
	dt_date_to_offset(Year,Month,Day,DagNo),
	lbox_Add(LboxWin, ""),		
	dt_offset_to_date(DagNo, _, _, _, _, DayOfWeek),
	dag2Werkdag(DayOfWeek, Werkdag),
	assertz(t_dag(DagNo, "", Werkdag)),
	dlg_dagdefinitie_Create(_Win, b_False, DagNo,_DagNew),
	retractall(t_dag(_, "", _)),
	findall(DStr, reprd(_, DStr), DList),
	lbox_Suspend(LboxWin),
	lbox_Clear(LboxWin),
	lbox_Add(LboxWin, DList),
	lbox_Resume(LboxWin),
	lbox_SetSel(LboxWin, 0, b_True),
	nieuwjaarsBoodschap(),
	!.
%END Dagen, idc_bij _CtlInfo

%BEGIN Dagen, e_Create
  dlg_dagen_eh(_Win,e_Create(_CreationData),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idc_dagen),
	lbox_SetTabStops(LboxWin, [39, 65, 85, 105]),
	Font = font_Create(ff_Fixed, [], 10),
	win_SetFont(LboxWin, Font),
	win_SetSubclassHandler(LboxWin,bSubclass_eh,b_False),
	!.
%END Dagen, e_Create

  dlg_dagen_eh(_,_,_):-!,fail.

%END_DLG Dagen

%BEGIN_DLG Dagdefinitie
/**************************************************************************
	Creation and event handling for dialog: Dagdefinitie
**************************************************************************/

constants

%BEGIN Dagdefinitie, CreateParms, 21:01:21-22.10.2008, Code automatically updated!
  dlg_dagdefinitie_ResID = idd_dagdefinitie
  dlg_dagdefinitie_DlgType = wd_Modal
  dlg_dagdefinitie_Help = idh_dagen
%END Dagdefinitie, CreateParms
  date_cc_class = "date_cc_class"

predicates

  dlg_dagdefinitie_eh : EHANDLER
  dlg_dagdefinitie_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST,ulong DagUit)
  dlg_dagdefinitie_update(DIALOG_VAL_LIST, ulong DagUit)
  %dagCheck(string)
  %ndexVan(integer Dag, ilist Dagen, integer, integer Index)
  date_cc_class_handler : EHANDLER
  wrk2button(wrk, integer, integer)

clauses

wrk2button(z, idc_vrije_dag, 0) :- !.
wrk2button(w, idc_werkdag_uit_kantoortijd_geldt, 1) :- !.

date_cc_class_handler(Win,e_Create(_),0):-!,		% CALL date_cc_Init ON
  dagnum(Dag,_),
  dt_mins_date_time(InitValue, Dag, 0),
  date_cc_Init(Win,InitValue,"%DD-%MD-%YL").
date_cc_class_handler(Win,EVENT,0):-!,			% CALL date_cc_HandleEvent
  date_cc_handleEvent(Win,Event).			% ON ANY OTHER EVENT
date_cc_class_handler(Win,EVENT,0):-!,			% CALL date_cc_HandleEvent
  dlg_dagdefinitie_eh(Win,Event).			% ON ANY OTHER EVENT

%__________________________________________________________________________
  dlg_dagdefinitie_Create(Parent, ReadOnly, DagNo,DagUit):-
	class_Create(date_cc_class,date_cc_class_handler),
%write("\nDagNo:", DagNo, " readonly: ", ReadOnly),
	assert(dagnum(DagNo,ReadOnly)),			% bewaar voor check
	t_dag(DagNo, _IDC_DAG_VALUE, Wrk),
	wrk2button(Wrk, KANTOORT, _),
%MARK Dagdefinitie, new variables

	dialog_CreateModal(Parent,dlg_dagdefinitie_ResID,"",
  		[
%BEGIN Dagdefinitie, ControlList, 21:01:21-22.10.2008, Code automatically updated!,
		df(KANTOORT,radiobuttongroup([idc_werkdag_uit_kantoortijd_geldt,idc_vrije_dag]),nopr)
%END Dagdefinitie, ControlList
		],
		dlg_dagdefinitie_eh,0,VALLIST,ANSWER),
	dlg_dagdefinitie_handle_answer(ANSWER,VALLIST,DagUit), 
	class_Destroy(date_cc_class), !.

  dlg_dagdefinitie_handle_answer(idc_ok,VALLIST,DagNew):-!,
	dlg_dagdefinitie_update(VALLIST,DagNew).
  dlg_dagdefinitie_handle_answer(idc_cancel,_,D):- 	% Handle Esc and Cancel here
       dagnum(D, b_True), !.
  dlg_dagdefinitie_handle_answer(idc_cancel,_,D):- 	% Handle Esc and Cancel here
       dagnum(D, b_False),
       retractall(t_dag(D, _, _)), !.
  dlg_dagdefinitie_handle_answer(_,_,_):-
	errorexit().

  dlg_dagdefinitie_update(_VALLIST, DOffset):-
%BEGIN Dagdefinitie, Update controls, 21:01:21-22.10.2008, Code automatically updated!
	_KANTOORT = dialog_VLGetRadiobutton(idc_werkdag_uit_kantoortijd_geldt,_VALLIST),
%END Dagdefinitie, Update controls
	dagnum(DOffset,_),
	true.

%MARK Dagdefinitie, new events

%BEGIN Dagdefinitie, e_Create
  dlg_dagdefinitie_eh(_Win,e_Create(_CreationData),0):-
	dagnum(_, b_False), !,
	win_PostEvent(_Win,e_User(11,0)).
  dlg_dagdefinitie_eh(_Win,e_Create(_CreationData),0):-
	dagnum(_, b_True),
	trap(DateCCWin = win_GetCtlHandle(_Win,idc_date),_,fail),
	win_SetState(DateCCWin, [wsf_Disabled]),
	trap(AanwWin = win_GetCtlHandle(_Win,idct_voorbeeld),_,fail),
	win_SetState(AanwWin, [wsf_Invisible]),
	!.
%END Dagdefinitie, e_Create

%BEGIN Dagdefinitie, e_User
  dlg_dagdefinitie_eh(_Win,e_User(11,_Ptr),0):-!,
	trap(DateCCWin = win_GetCtlHandle(_Win,idc_date),_,fail),
	win_SetFocus(DateCCWin),
	!.
%END Dagdefinitie, e_User

%BEGIN Dagdefinitie, idc_ok _CtlInfo
  %dlg_dagdefinitie_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
  dlg_dagdefinitie_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	dagnum(_,b_False),	% niet readonly
	trap(DateCCWin = win_GetCtlHandle(_Win,idc_date),_,fail),
	win_SetFocus(DateCCWin),
	MinOffset = date_cc_GetValue(DateCCWin),
	dt_mins_date_time(MinOffset, Date, _),
	t_dag(Date,Str,_),
	not(Str = ""),	
	dlg_MessageBox( "Dag", "Nieuwe toernooidag is al in de tabel!", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), !.
  dlg_dagdefinitie_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	trap(DateCCWin = win_GetCtlHandle(_Win,idc_date),_,fail),
	%win_SetFocus(DateCCWin),
	MinOffset = date_cc_GetValue(DateCCWin),
	dt_mins_date_time(MinOffset, Date, _),
	assert(dagnum(Date,b_True)),
	dt_dateoffset_to_str(Date, "%Dn %Dd/%Md", Str),
	trap(KantWin = win_GetCtlHandle(_Win,idc_werkdag_uit_kantoortijd_geldt),_,fail),
	Checked = win_IsChecked(KantWin),
	wrk2button(Wrk, _, Checked),
	retractall(t_dag(Date,_,_)),
	assert(t_dag(Date, Str, Wrk)),
	fail.
%END Dagdefinitie, idc_ok _CtlInfo


%BEGIN Dagdefinitie, idc_help _CtlInfo
  dlg_dagdefinitie_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Dagdefinitie"),
	!.
%END Dagdefinitie, idc_help _CtlInfo


  dlg_dagdefinitie_eh(_,_,_):-!,fail.

%END_DLG Dagdefinitie


%BEGIN_DLG Baanbeschikbaarheid
/**************************************************************************
	Creation and event handling for dialog: Baanbeschikbaarheid
**************************************************************************/

constants

%BEGIN Baanbeschikbaarheid, CreateParms, 10:42:04-3.10.2011, Code automatically updated!
  dlg_baanbeschikbaarheid_ResID = idd_baanbeschikbaarheid
  dlg_baanbeschikbaarheid_DlgType = wd_Modal
  dlg_baanbeschikbaarheid_Help = idh_baanbeschikbaarheid
%END Baanbeschikbaarheid, CreateParms

database - baantijden

  t_bn(tijd, integer)
single  kopietijd(tijd)
single  zdag(string)

predicates

  dlg_baanbeschikbaarheid_eh : EHANDLER
  dlg_baanbeschikbaarheid_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_baanbeschikbaarheid_update(DIALOG_VAL_LIST)
  reprb(tijd, string)
  procedure tijden2list(tijdl, slist, slist)
  repr2tijd(string, tijd)
  copyTijden
  saveTijden(WINDOW)
  compareTijden
  removeTijd(WINDOW, integer Index, tijd)
  setBaanSelect(WINDOW, integer Index)
  tijd2index(tijd, integer)
  tijd2index(tijd, tijdl, integer, integer)
  gelijkOp(tijdl, tijdl)
procedure zelfdedag(integer, string, string)
nondeterm zelfdedag(integer, integer)

clauses

kopietijd(0).
zdag("").

copyTijden :-
  loglock(mdi, "copyTijden"),
  logclose(),
  bn(Tijd, Aantal),
  bitright(Tijd, 7, Dag),
  dag(Dag, _, _),	% dag bestaat
%  Tijd > 0,
  assert(t_bn(Tijd, Aantal)),
  fail.
copyTijden.

saveTijden(Win) :-
  cursor_Set(Win, cursor_Wait),
  loglock(mdi, "saveTijden"),
  bn(Tijd, Aantal),
  not(t_bn(Tijd, _)),
  logf('R', bn(Tijd, Aantal),ja),
  fail.
saveTijden(Win) :-
  cursor_Set(Win, cursor_Wait),
  ws(No,WC, Tgnst, Tijd),
  not(ws(_, _, _, Tijd)),
  logf('R', ws(No,Wc,Tgnst,Tijd),ja),
  fail.
saveTijden(_) :-
  findall(BT, bn(BT, _), BTList),
  sorttijd_c(BTList),
  findall(BTT, t_bn(BTT, _), BTTList),
  sorttijd_c(BTTList),
  gelijkOp(BTList, BTTList),
  fail.
saveTijden(Win) :-
  logclose(),
  cursor_Set(Win, cursor_Arrow),
  fail.

gelijkOp([T|Tail], [T|Rest]) :- % ga het rijtje langs totdat verschil
  t_bn(T, Aantal),		% optreedt
  bn(T, Aantal),
  retract(t_bn(T, _)), !,
  gelijkOp(Tail, Rest).
gelijkOp(_, BTList) :-
  member(Tijd, BTList),
  retract(t_bn(Tijd, Aantal)),
  %not(bn(Tijd, Aantal)),
  logf('Z', bn(Tijd, Aantal),ja),
  fail.				% mag failen

compareTijden :-
  bn(Tijd, Aantal),
  not(t_bn(Tijd, Aantal)), !.
compareTijden :-
  t_bn(Tijd, Aantal),
  not(bn(Tijd, Aantal)), !.

setBaanSelect(LboxWin, Index) :-	% extra test voor laatste regel
  NoOfItems = lbox_CountAll(LboxWin),
  LastIndex = NoOfItems - 1,
  kleinste(Index, LastIndex, NewIndex),
  lbox_SetSel(LboxWin, NewIndex, b_True).

reprb(Tijd, Repr) :-
  t_bn(Tijd, Aantal),
  findall(WCat, w2(_WdNo,WCat,Tijd,_,_,_,_,_), Planlijst),
  count(Planlijst, 0, Vulling),
  tijd_kwart(D, Uur, Min, Tijd),
  helestr_int(0, MinS, Min),
  dag(D, Str, _Wrk),
  searchchar(Str, ' ', Pos),
  P1 = Pos - 1,
  frontstr(P1, Str, WkD, Rest),
  format(Repr, "%s\t%s\t%u:%s\t%u\t%u\t%u", WkD, Rest, Uur, MinS, Aantal, Vulling), !.

repr2tijd(Repr, Tijd) :-
  findall(Tijdx, t_bn(Tijdx, _), Tijden),
  sorttijd_c(Tijden),	% moet het wel in volgorde??
  member(Tijd, Tijden),
  reprb(Tijd, Repr1),
  Repr = Repr1, !.	% backtracken tot hij weer hetzelfde is .. gevonden
    
tijden2list([H|T], In, [Repr|Out]) :-
  reprb(H, Repr),
  tijden2list(T, In, Out), !.
tijden2list(_, In, In).

removeTijd(LboxWin, Index, Tijd) :-
  not(w2(_,_,Tijd,_,_,_,_,_)),
  not(ws(_, _, _, Tijd)),
  retract(t_bn(Tijd, _)),
  lbox_Delete(LboxWin, Index), !.
removeTijd(LboxWin, Index, Tijd) :-
  retract(t_bn(Tijd, _)), !,
  assert(t_bn(Tijd, 0)),
  _Answer = dlg_MessageBox( "Tijdstip verwijderen", "Er is nog planning (of afzegging) voor dit tijdstip dus\nalleen de beschikbaarheid wordt op 0 gezet!", mesbox_iconquestion, mesbox_buttonsokcancel, mesbox_defaultfirst, mesbox_suspendapplication ),
  reprb(Tijd, Repr),
  lbox_Delete(LboxWin, Index),
  lbox_Add(LboxWin, Index, Repr).

tijd2index(Tijd, Index) :-
  findall(Tijdx, t_bn(Tijdx, _), Tijden),
  sorttijd_c(Tijden),
  tijd2index(Tijd, Tijden, 0, Index).
  
tijd2index(_, [], _, -1) :- !.
tijd2index(Tijd, [Tijd1|_], I, I) :-
  Tijd <= Tijd1, !.
tijd2index(Tijd, [_|Tail], N, I) :-
  N1 = N + 1,
  tijd2index(Tijd, Tail, N1, I).   

zelfdedag(Dag, Aantal) :-
  t_bn(Tijd, Aantal),
  bitright(Tijd, 7, D),
  D = Dag.

zelfdedag(_Tijd, Dag, "") :-
  zdag(Dag), !.
zelfdedag(Tijd, DagS, DagU) :-
  bitright(Tijd, 7, D),
  findall(A, zelfdedag(D, A), Alist),
  count(Alist, 0, Span),
  format(DagU, "<TD rowspan=%u style=\"vertical-align:top\">%s</TD>", Span, DagS),
  assert(zdag(DagS)).
%___________________________________________________________________________________
  dlg_baanbeschikbaarheid_Create(Parent):-
	not(dag(_,_,_)),
	dlg_MessageBox( "Baanbeschikbaarheid", "Vul eerst de kalendertabel.", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	dlg_dagen_Create(Parent), !.	% eerst kalender
  dlg_baanbeschikbaarheid_Create(Parent):-
	copyTijden(),
	findall(Tijd, t_bn(Tijd, _), Tijden),
	sorttijd_c(Tijden),
	tijden2list(Tijden, [],	DAGBAANLIJST_ITEMLIST),
%MARK Baanbeschikbaarheid, new variables

	dialog_CreateModal(Parent,dlg_baanbeschikbaarheid_ResID,"",
  		[
%BEGIN Baanbeschikbaarheid, ControlList, 10:42:04-3.10.2011, Code automatically updated!
		df(dagbaanlijst,listbox(DAGBAANLIJST_ITEMLIST,[0]),nopr)
%END Baanbeschikbaarheid, ControlList
		],
		dlg_baanbeschikbaarheid_eh,0,VALLIST,ANSWER),
	dlg_baanbeschikbaarheid_handle_answer(ANSWER,VALLIST).

  dlg_baanbeschikbaarheid_handle_answer(idc_ok,VALLIST):-!,
	dlg_baanbeschikbaarheid_update(VALLIST),
	Win = vpi_GetTaskWin(),
	saveTijden(Win).
  dlg_baanbeschikbaarheid_handle_answer(idc_cancel,_):-!,	% Handle Esc and Cancel here
  	retractall(t_bn(_, _)).
  dlg_baanbeschikbaarheid_handle_answer(_,_):-
	errorexit().

  dlg_baanbeschikbaarheid_update(_VALLIST):-
%BEGIN Baanbeschikbaarheid, Update controls, 10:42:04-3.10.2011, Code automatically updated!
	dialog_VLGetListBox(dagbaanlijst,_VALLIST,_DAGBAANLIJST_ITEMLIST,_DAGBAANLIJST_SELECT),
%END Baanbeschikbaarheid, Update controls
	true.

%MARK Baanbeschikbaarheid, new events

%BEGIN Baanbeschikbaarheid, idc_afdruk _CtlInfo
  dlg_baanbeschikbaarheid_eh(_Win,e_Control(idc_afdruk,_CtrlType,_CtrlWin,_CtlInfo),0):-
	getFolder(Dir,_,_,_),
	assert(zdag("")),
	filenamepath(File, Dir, "rooster.html"),
	%format(File, "%srooster.html", Dir),
	openwrite(work, File),
	writedevice(work),
	%webrapport(Font, Kleur),
	rapport_CSS(CSSContent),
	metaTag(Eerst, Tag),
	write(Eerst),
	write("\n<HTML><HEAD>", Tag, "<TITLE>Baanbeschikbaarheid</TITLE></HEAD>"),
	write("\n<STYLE>"),
	write(CSSContent),
	write("\n</STYLE><BODY>"),
	write("\n<H3>Baanbeschikbaarheid</H3>"),
	write("\n<TABLE border=1 cellpadding=3 cellspacing=0>"),  
	write("<TR><TH>Dag</TH><TH>Tijd</TH><TH>Aantal</TH><TH>Gepland</TH></TR>\n"),
	t_bn(Tijd, Aantal),
	findall(WCat, w2(_WdNo,WCat,Tijd,_,_,_,_,_), Planlijst),
	count(Planlijst, 0, Vulling),
	tijd_kwart(D, Uur, Min, Tijd),
	helestr_int(0, MinS, Min),
	dag(D, Str, _Wrk),
	zelfdedag(Tijd, Str, WkD1),
	writef("<TR>%s<TD class=rechts>%u:%s</TD><TD class=middle>%u</TD><TD class=middle>%u</TD></TR>\n", WkD1, Uur, MinS, Aantal, Vulling),
	fail.
  dlg_baanbeschikbaarheid_eh(_Win,e_Control(idc_afdruk,_CtrlType,_CtrlWin,_CtlInfo),0):-
	write("</TABLE></BODY></HTML>"),
	closefile(work),
	Null = cast(api_hwnd,0),
	getFolder(Dir,_,_,_),
	filenamePath(File, Dir, "rooster.html"),
	api_ShellExecute(Null,"open",File,"","",1),
	!.
%END Baanbeschikbaarheid, idc_afdruk _CtlInfo

%BEGIN Baanbeschikbaarheid, idc_bij _CtlInfo
  dlg_baanbeschikbaarheid_eh(_Win,e_Control(idc_bij,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, dagbaanlijst),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	repr2tijd(Item, Tijd),
	dlg_daguurkwart_Create(_Win, Tijd),
	win_SetFocus(LboxWin),
	!.
  dlg_baanbeschikbaarheid_eh(_Win,e_Control(idc_bij,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, dagbaanlijst),
	dag(N,_,_),
	bitleft(N, 7, TN),
	dlg_daguurkwart_Create(_Win, TN),
	win_SetFocus(LboxWin),
	!.
%END Baanbeschikbaarheid, idc_bij _CtlInfo

%BEGIN Baanbeschikbaarheid, idc_dagkopie _CtlInfo
  dlg_baanbeschikbaarheid_eh(_Win,e_Control(idc_dagkopie,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	assert(kopietijd(0)),
	dlg_baandagkopie_Create(_Win),
	kopietijd(KTijd),		% kopietijd geeft terugkeer aan.
	KTijd <> 0,
	findall(Tijd, t_bn(Tijd, _), Tijden),
	sorttijd_c(Tijden),
	tijden2list(Tijden, [],	SList),
	tijd2index(KTijd, Index),
	LboxWin = win_GetCtlHandle(_Win, dagbaanlijst),
	lbox_Suspend(LboxWin),
	lbox_Clear(LboxWin),
	lbox_Add(LboxWin, SList),
	lbox_Resume(LboxWin),
	lbox_SetTopIndex( LboxWin, Index ),
	lbox_SetSel(LboxWin, Index, b_True),
	!.
%END Baanbeschikbaarheid, idc_dagkopie _CtlInfo

%BEGIN Baanbeschikbaarheid, idc_help _CtlInfo
  dlg_baanbeschikbaarheid_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Baanbeschikbaarheid"),
	!.
%END Baanbeschikbaarheid, idc_help _CtlInfo

%BEGIN Baanbeschikbaarheid, idc_cancel _CtlInfo
  dlg_baanbeschikbaarheid_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-
	compareTijden,
	ANSWER = dlg_Ask("Beschikbaarheid veranderd","bewaren?",["&Ja","&Nee"]),
	ANSWER = resp_default,
	saveTijden(_Win),
	fail.
%END Baanbeschikbaarheid, idc_cancel _CtlInfo

%BEGIN Baanbeschikbaarheid, dagbaanlijst activated
  dlg_baanbeschikbaarheid_eh(_Win,e_Control(dagbaanlijst,_CtrlType,_CtrlWin,activated),0):-!,
	LboxWin = _CtrlWin, % win_GetCtlHandle(_Win, dagbaanlijst),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	repr2tijd(Item, Tijd),
	dlg_daguurkwart_Create(_Win, Tijd),
	!.
%END Baanbeschikbaarheid, dagbaanlijst activated

%BEGIN Baanbeschikbaarheid, e_User
  dlg_baanbeschikbaarheid_eh(_Win,e_User(1,_Ptr),0):-!,	% verschuif aanwijzer
	NewTijd = cast(tijd, _Ptr),
	tijd2index(NewTijd, Index),
	LboxWin = win_GetCtlHandle(_Win, dagbaanlijst),
	lbox_SetSel(LboxWin, Index, b_True),
 	!.
  dlg_baanbeschikbaarheid_eh(_Win,e_User(2,_Ptr),0):-!,	% update
	Tijd = cast(tijd, _Ptr),
	tijd2index(Tijd, Index),
	LboxWin = win_GetCtlHandle(_Win, dagbaanlijst),
	reprb(Tijd, Repr),
	lbox_Delete(LboxWin, Index),
	lbox_Add(LboxWin, Index, Repr),
	lbox_SetSel(LboxWin, Index, b_True),
 	!.
  dlg_baanbeschikbaarheid_eh(_Win,e_User(3,_Ptr),0):-!,	% nieuw
	Tijd = cast(tijd, _Ptr),
	tijd2index(Tijd, Index),
	reprb(Tijd, Repr),
	LboxWin = win_GetCtlHandle(_Win, dagbaanlijst),
	lbox_Add(LboxWin, Index, Repr),
	lbox_SetSel(LboxWin, Index, b_True),
 	!.
%END Baanbeschikbaarheid, e_User

%BEGIN Baanbeschikbaarheid, idc_af _CtlInfo
  dlg_baanbeschikbaarheid_eh(_Win,e_Control(idc_af,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	LboxWin = win_GetCtlHandle(_Win, dagbaanlijst),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	repr2tijd(Item, Tijd),
	%write("\n", Tijd),
	removeTijd(LboxWin, Index, Tijd),
	setBaanSelect(LboxWin, Index), 
	!.
%END Baanbeschikbaarheid, idc_af _CtlInfo


%BEGIN Baanbeschikbaarheid, e_Create
  dlg_baanbeschikbaarheid_eh(_Win,e_Create(_CreationData),0):-!,
	LboxWin = win_GetCtlHandle(_Win, dagbaanlijst),
	lbox_SetTabStops(LboxWin, [18, 43, 70, 93]),
	win_SetSubclassHandler(LboxWin,bSubclass_eh,b_False),
	!.
%END Baanbeschikbaarheid, e_Create

  dlg_baanbeschikbaarheid_eh(_,_,_):-!,fail.

%END_DLG Baanbeschikbaarheid

%BEGIN_DLG Baandagkopie
/**************************************************************************
	Creation and event handling for dialog: Baandagkopie
**************************************************************************/

constants

%BEGIN Baandagkopie, CreateParms, 12:50:37-5.10.2015, Code automatically updated!
  dlg_baandagkopie_DlgType = wd_Modal
  dlg_baandagkopie_Title = "Kopieëer baanbeschikbaarheid van een dag"
  dlg_baandagkopie_RCT = rct(50,40,284,167)
  dlg_baandagkopie_Flags = [wsf_Close,wsf_TitleBar]
  dlg_baandagkopie_Help = idh_baanbeschikbaarheid
  dlg_baandagkopie_Font = "Arial"
  dlg_baandagkopie_FSize = 10
%END Baandagkopie, CreateParms

predicates

  dlg_baandagkopie_eh : EHANDLER
  nondeterm dagMetProgr(string)
  nondeterm dagNaar(string, string)
  procedure dagkopie(integer, integer)
  nondeterm bnelInVolgorde(bnd)
  procedure apply(bndl)

clauses

bnelInVolgorde(b(Tijd, Aantal)) :-
  findall(T, t_bn(T, _), Tijden),
  sortint_c(Tijden),
  member(Tijd, Tijden),
  retract(t_bn(Tijd, Aantal)).

apply([b(T,A)|Rest]) :-
  assert(t_bn(T, A)), !,
  apply(Rest).
apply(_).

dagkopie(_DagV, DagN) :-
  t_bn(Tijd, _),
  bitright(Tijd, 7, TDag),
  TDag = DagN,
  retract(t_bn(Tijd, _)),
  fail.
dagkopie(DagV, DagN) :-
  t_bn(Tijd, Aantal),
  bitright(Tijd, 7, VDag),
  VDag = DagV,
  bitand(Tijd, 0x7F, Tijd1),
  bitleft(DagN, 7, DagN1),
  bitor(DagN1, Tijd1, TijdNew),
  asserta(t_bn(TijdNew, Aantal)),
  fail.
dagkopie(_DagV, DagN) :-
  w2(_Num,_Cat,Plan,_W1,_W2,_Fixed,_TStamp,_Baan),
  bitright(Plan, 7, PDag),
  PDag = DagN,
  not(t_bn(Plan, _)),
  assert(t_bn(Plan, 0)),
  fail.
dagkopie(_, _) :-
  findall(BNElem, bnelInVolgorde(BNElem), BNList),
  apply(BNList), !.

dagNaar(Dag, Selvan) :-
  dag(No, _, _),
  dt_dateoffset_to_str(No, "%Dn %Dd/%Md %YL", Dag),
  Dag <> Selvan.

dagMetProgr(Dag) :-
  dag(No, _, _),
  getbacktrack(Here),
  t_bn(Tijd, _),
  bitright(Tijd, 7, D),
  No = D,
  cutbacktrack(Here),
  %dt_dateoffset_to_str(No, "%Dn %DD/%MD %YL", Repr0),
  dt_dateoffset_to_str(No, "%Dn %Dd/%Md %YL", Dag).

  dlg_baandagkopie_Create(Parent):-
	win_CreateDynDialog(Parent,
		[
%BEGIN Baandagkopie, WinDefList, 12:50:37-5.10.2015, Code automatically updated!
		 dlg_font(wdef(dlg_baandagkopie_DlgType,dlg_baandagkopie_RCT,dlg_baandagkopie_Title,u_DlgBase),
		 	  dlg_baandagkopie_Font,dlg_baandagkopie_FSize,dlg_baandagkopie_Flags),
		 ctl(wdef(wc_LBox,rct(2,14,85,120),"",u_DlgBase),idc_baandagkopie_van,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight]),
		 ctl(wdef(wc_LBox,rct(107,15,191,121),"",u_DlgBase),idc_baandagkopie_naar,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_MultiSelect]),
		 ctl(wdef(wc_PushButton,rct(85,56,107,66),"&>>>",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop,wsf_Disabled]),
		 ctl(wdef(wc_PushButton,rct(194,109,234,121),"&Klaar",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(193,92,233,104),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_Text,rct(17,1,60,11),"Van:",u_DlgBase),idct_van,[wsf_AlignCenter]),
		 ctl(wdef(wc_Text,rct(119,2,164,12),"Naar:",u_DlgBase),idct_naar,[wsf_AlignCenter]),
		 ctl(wdef(wc_Text,rct(193,25,226,35),"meerdere",u_DlgBase),idct_meerdere,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(193,36,226,46),"tegelijk",u_DlgBase),idct_tegelijk,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(193,47,226,57),"mogelijk!",u_DlgBase),idct_mogelijk,[wsf_AlignLeft])
%END Baandagkopie, WinDefList
		],dlg_baandagkopie_eh,0).

%BEGIN Baandagkopie, idc_ok _CtlInfo
  dlg_baandagkopie_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	NaarWin = win_GetCtlHandle(_Win, idc_baandagkopie_naar),
	lbox_GetSel(NaarWin, NaarList,_),
	member(NaarString, NaarList),
	VanWin = win_GetCtlHandle(_Win, idc_baandagkopie_van),
	lbox_GetSel(VanWin, VanList,_),
	VanList = [VanString|_],
	dt_datestr_to_offset(VanString, "%Dn %Dd/%Md %YL", DagV),
	dt_datestr_to_offset(NaarString, "%Dn %Dd/%Md %YL", DagN),
	dagkopie(DagV, DagN),
	bitleft(DagN, 7, KopieTijd),
	assert(kopietijd(KopieTijd)),
	ParentWin = win_GetParent(_Win),
        _Reply = win_SendEvent(ParentWin, e_User(1, KopieTijd)),
	fail.
	%win_Destroy(_Win),
	%!.
%END Baandagkopie, idc_ok _CtlInfo
%MARK Baandagkopie, new events

%BEGIN Baandagkopie, idc_help _CtlInfo
  dlg_baandagkopie_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Baanbeschikbaarheid"),
	!.
%END Baandagkopie, idc_help _CtlInfo

%BEGIN Baandagkopie, idc_baandagkopie_naar selchanged
  dlg_baandagkopie_eh(_Win,e_Control(idc_baandagkopie_naar,_CtrlType,_CtrlWin,selchanged),0):-!,
	CtrlWin = win_GetCtlHandle(_Win, idc_ok),
	win_SetState(CtrlWin, [wsf_Enabled]),
	!.
%END Baandagkopie, idc_baandagkopie_naar selchanged

%BEGIN Baandagkopie, idc_baandagkopie_van selchanged
  dlg_baandagkopie_eh(_Win,e_Control(idc_baandagkopie_van,_CtrlType,_CtrlWin,selchanged),0):-!,
	OKWin = win_GetCtlHandle(_Win, idc_ok),
	win_SetState(OKWin, [wsf_Disabled]),
	lbox_GetSel(_CtrlWin, ItemList,_IndexList),
	ItemList = [SelVan|_],
	LboxWin = win_GetCtlHandle(_Win, idc_baandagkopie_naar),
	findall(Dag, dagNaar(Dag, Selvan), Dagen),
	lbox_Suspend(LboxWin),
	lbox_Clear(LboxWin),
	lbox_Add(LboxWin, Dagen),
	lbox_Resume(LboxWin),
	!.
%END Baandagkopie, idc_baandagkopie_van selchanged

%BEGIN Baandagkopie, idc_cancel _CtlInfo
  dlg_baandagkopie_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	win_Destroy(_Win),
	!.
%END Baandagkopie, idc_cancel _CtlInfo

%BEGIN Baandagkopie, e_Create
  dlg_baandagkopie_eh(_Win,e_Create(_CreationData),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idc_baandagkopie_van),
	Font = font_Create(ff_Fixed, [], 11),
	win_SetFont(LboxWin, Font),
	findall(Dag, dagMetProgr(Dag), Dagen),
	lbox_Add(LboxWin, Dagen),
	LboxWin1 = win_GetCtlHandle(_Win, idc_baandagkopie_naar),
	win_SetFont(LboxWin1, Font),
	!.
%END Baandagkopie, e_Create

  dlg_baandagkopie_eh(_,_,_):-!,fail.

%END_DLG Baandagkopie

%BEGIN_DLG Onderdeelkenmerken
/**************************************************************************
	Creation and event handling for dialog: Onderdeelkenmerken
**************************************************************************/

constants

%BEGIN Onderdeelkenmerken, CreateParms, 11:17:44-1.10.2014, Code automatically updated!
  dlg_onderdeelkenmerken_ResID = idd_onderdeelkenmerken
  dlg_onderdeelkenmerken_DlgType = wd_Modal
  dlg_onderdeelkenmerken_Help = idh_onderdeeleigenschappen
%END Onderdeelkenmerken, CreateParms
c_setfocus   = 32
uitgesteld = 33
ereenbij   = 34
c_ereenaf    = 35
setenkeldubbel  = 41
setmanvrgemengd = 42

database - kenm
  determ kenmerkwin(WINDOW)
  determ waarschuwen
  single noRadio(boolean)
  single refresh(boolean)
  t_wc(dbasedom) 
  t_vink(wedscat Echtecat,wedscat catnummer)
  algeweestW(wedscat)

  %vink(wedscat)

predicates

  dlg_onderdeelkenmerken_eh : EHANDLER
  callbackO : cTreeView::callback
nondeterm  afhankelijkT(wedscat, cTreeView::tree)
nondeterm knoopT(wedscat, cTreeView::tree)
procedure hdg2mvgn(wedsex /*sexlist*/, window, slist, boolean)
determ setHDG(window, wedsex)
procedure edg2ed(wedstyp, window, boolean Nieuw)
procedure geldstr(real, string)
nondeterm spsrtInList(string SNaam, sexelst)
procedure selectedSrt(window, slist, sexlist, integer)
procedure getWedsTyp(window, wedstyp)
procedure getSex(window, wedsex)
procedure iteml2sexl(slist, sexlist)
procedure strgeld(string, real)
procedure strsterkte(string, string)
procedure getond(wedscat)
setond(wedscat)
procedure getleeft(string, integer)
nondeterm relevantesoort2(sexe, string)
determ    zijnErOpgaven(window)
procedure checkRelevanteSoorten(window SWin, sexe, slist, integer)
determ  isGelijkOndKG1(window, wedscat, catl, catl)
determ  isGelijkOndKG(window)
procedure maakgelijkOndK(window)
determ   maakgelijkOndK1(cTreeView::tree_list)
procedure treeond(cTreeView::tree_list, catl, catl)
procedure treeond(window, catl)
procedure enablecontrols(wedscat Geselecteerd)
nondeterm catlist2opg(catl, integer Opg)
nondeterm catlist2plan(catl, integer Num)
procedure opgavenVoor(window TreeWin, integer CatNr, integerlist OpgList, integerlist Planninglijst)
procedure tree2list(window Treewin, wedscat Ond, catl, catl)
procedure copyOnd()
nondeterm own_member(OD_ITEMSTATE,OD_STATE)
%procedure wcrit1(wedscat, string, integer, integer)
determ insertpoint(window, wedscat, cTreeView::insertPlace)
procedure verwisselpoint(window, wedscat In, wedscat Andere, cTreeView::insertPlace)
determ naamProbleem(wedscat)
determ sexProbleem(wedscat)
%procedure needsAttention(wedscat Cat, cTreeView::itemstate, cTreeView::itemstate)
%determ eenWaarde(string Sterk, integer Lft, integer Lft1)
determ mvInSexL(sexlist)
determ enkeldubbelMatch(wedscat Old)
determ enkeldubbelMatch(window, boolean ED, string NaamL)
damesHerenMatch(wedscat Old)
damesHerenMatch(window KWin, boolean DChecked, boolean HChecked, boolean GChecked, boolean NChecked, string NaamL)
damesHerenMatchMessage(window KWin, string NaamL, string TekstElement, string Selectie, dialog_control_id)
determ ondercatT(wedscat)
procedure deleteAllCatSelectedNodes(window Win, wedscat Cat)
determ behandelLege(wedscat)
procedure nietUpdaten(window, wedscat, wcorigine, string Tnr, string OnderdeelId) 
nondeterm soortgelijkeCat(wedscat Zelf, string KenStr, wedscat)
nondeterm teverbindenT(wedscat, string)
procedure setwedsex(window, integer)
procedure isNieuw(string, string, boolean)
determ isCatLLeeg(slist, wedscat)
procedure ontkoppelCat(wedscat)
procedure maakgelijkRest()
determ checkTvink(wedscat, wedscat, integer TreeSel) - (i,i,o)
nondeterm koplvT(wedscat CatV, wedscat Cat) - (i,o) (o,i) (i,i)
procedure rsoort2bitmapT(wedscat, cTreeView::bitmapId) - (i, o)
nondeterm voorrondeT(wedscat, wedscat) - (i, o)
nondeterm voorrondeAT(wedscat Cat, wedscat VoorrondeX)
determ checkResp(string Titel, slist CatL, string StrSel)
procedure bovencatT(wedscat, wedscat) - (i, o)

clauses
noRadio(b_False).
refresh(b_False).

own_member([X|_],X).
own_member([_|T],X):-own_member(T,X).

manVrouw(_, SrtL, gemengd) :- 
  member(Srt, SrtL),
  spsrt(Srt, _Naam, m, _LftVan, _LftTot, _StrkVan, _StrkTot, _Reserve), 
  member(Srt1, SrtL),
  spsrt(Srt1, _, v, _, _, _, _, _), !.
manVrouw(_, SrtL, heren) :-
  member(Srt, SrtL),
  spsrt(Srt, _Naam, m, _LftVan, _LftTot, _StrkVan, _StrkTot, _Reserve), !.
manVrouw(_, _, dames).
  

/*copyOnd() :-
  wc(WC,Naam,_,_,_,_,_),
  WC = 0,
  concat(Naam, " is nul!", Msg),
  dlg_MessageBox( "Categorie", Msg, mesbox_iconerror, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail. */
copyOnd() :-
  initspsrt(),
  loglock(lees, "copyOnd"),
  logclose(),
  assert(waarschuwen),
  wc(WC, OnderdeeelID, Orde, Naam, EDG, Kort, SexL, SpStrk, LftV, LftTM, HDG, Schemasoort, Status, KNLTB, Geld),
  WC <> 0,
  trim_c(Naam),
  trim_c(Kort),
  %wcrit1(WC, SpStrk, LftVan, LftTot),
  assert(t_wc(wc(WC, OnderdeeelID, Orde, Naam, EDG, Kort, SexL, SpStrk, LftV, LftTM, HDG, Schemasoort, Status, KNLTB, Geld))),
  fail.
copyOnd() :-
  kopl(Caty, Van, Catx, Naar),
  assert(t_wc(kopl(Caty, Van, Catx, Naar))),
  fail.
copyOnd(). 
  %save("databasekenm.txt",kenm).    %  !!!

spsrtInList(SrtS, [X|_]) :-
  spsrt(Srt, _SNaam, X, _LftVan, _LftTot, _StrkVan, _StrkTot, _Reserve),
  str_int(SrtS, Srt).
spsrtInList(SNaam, [_|Rest]) :-
  spsrtInList(SNaam, Rest).

selectedSrt(SrtWin, [Srt|SrtList], SexL, N) :-
  str_Int(Srt, SexI),
  member(SexI, SexL),
  lbox_SetSel(SrtWin, N, b_True),
  N1 = N + 1, !,
  selectedSrt(SrtWin, SrtList, SexL, N1).
selectedSrt(SrtWin, [_|SrtList], SexL, N) :-
  N1 = N + 1, !,
  selectedSrt(SrtWin, SrtList, SexL, N1).
selectedSrt(_, _, _, _).

checkVink(Cat, _, Nr) :-
  vink(Cat, _NrO),
  numb(N),
  Nr = N + 2000,
  not(vink(_, Nr)), !,
  assert(vink(Cat, Nr)).
checkVink(Cat, _, Cat) :-
  assert(vink(Cat, Cat)).

checkTVink(Cat, _, Nr) :-
  t_vink(Cat, _NrO),
  numb(N),
  Nr = N + 2000,
  not(t_vink(_, Nr)), !,
  assert(t_vink(Cat, Nr)).
checkTVink(Cat, _, Cat) :-
  assert(t_vink(Cat, Cat)).



%onderCatT(CatT) :-
%  kopl(_CatV, pl3en4(_), CatT, _), !.
onderCatT(Cat) :-
  koplvT(Cat, CatT),
  not(wc(CatT, _, _, _, _, _, _, _, _, _, _, _, _, _, _)),
  writedevice(XYZ),
  writedevice(screen),
  write("\n####- ondercat maar bovencat bestaat niet"),
  writedevice(XYZ),
  !.
onderCatT(Cat) :-
  koplvT(Cat, _CatT), !.

knoopT(_, _) :-
  retractall(t_vink(_, _)),
  fail.
knoopT(CatNo, tree(CatNoNr, [], BMap, BMap, Naam1, Afhankelijken)) :-
  t_wc(wc(CatNo, _, _, Naam, _, Kort, _, _, _, _, _, _, _, _, _)),
  not(ondercatT(CatNo)),
  checkTVink(CatNo, CatNo, CatNoNr),	% waarschijnlijk niet nodig?
  format(Naam1, "%s * %s", Kort, Naam),
  rsoort2bitmap(CatNo, BMap),
  findall(Afhankelijk, afhankelijkT(CatNo, Afhankelijk), Afhankelijken).

afhankelijkT(Cat, tree(CatTNr, [], BMap, BMap, Naam1, Afhankelijken)) :-
  t_wc(wc(CatT, _, _, Naam, _, Kort, _, _, _, _, _, _, _, _, _)),
  getbacktrack(Here),
  koplvT(CatT, Cat),
  cutbacktrack(Here),
  checkTVink(CatT, Cat, CatTNr),
  format(Naam1, "%s * %s", Kort, Naam),
  rsoort2bitmap(CatT, BMap),
  findall(Afhankelijk, afhankelijkT(CatT, Afhankelijk), Afhankelijken).

rsoort2bitmapT(Cat, resId(idb_plaats_34)) :-
  t_wc(kopl(_CatNo, pl3en4(_), Cat, _)), !.
rsoort2bitmapT(Cat, resId(idb_troostronde)) :-
  t_wc(kopl(Cat,verliezer,_,_)), !.
  %wc(_, _, _, _,_,_,Cat), 
  %Cat <> 0, !.	% verliezersronde
rsoort2bitmapT(Cat, BitMap) :-
  pos(Cat, _, _, RSoort), % ingedeeld
  bovenCat(Cat, TopCat),
  t_wc(wc(TopCat, _, _, _, __, _, _, _, _, _, _, _, CatKeur, _, _)),
  switchKeur(CatKeur, CatKeur1),
  findall(Voorr,voorrondeT(Cat, Voorr),Voorrondes),
  opgKeur([Cat|Voorrondes], OpgKeur),
  rsoort2bitmap1(RSoort, CatKeur1, OpgKeur, BitMap), !. 
rsoort2bitmapT(Cat, BitMap) :-
  bovenCatT(Cat, TopCat),  % niet ingedeeld
  t_wc(wc(TopCat, _, _, _, __, _, _, _, _, _, _, _, CatKeur, _, _)),
  findall(Voorr,voorrondeT(Cat, Voorr),Voorrondes),
  opgKeur([Cat|Voorrondes], OpgKeur),
  rsoort2bitmap2(CatKeur, OpgKeur, BitMap), !. 
rsoort2bitmapT(_Cat, resId(idb_geenindeling)).

bovencatT(Cat, CatBoven) :-		% zoek bovenste
  koplvT(Cat, CatB), !,
  %Rank < 3, !,	% 1 of 2 gaan door
  bovenCatT(CatB, CatBoven).
bovencatT(Cat, Cat).


koplvT(CatV, Cat) :-
  t_wc(kopl(CatV, _, Cat, _)),
  not(t_wc(kopl(CatV, pl3en4(_), Cat, _))).
koplvT(CatV, Cat) :-			% 3 en 4 is helaas andersom
  t_wc(kopl(Cat, pl3en4(3), CatV, _)).	% komen altijd paarwijze voor

voorrondeT(_Cat, _Voorronde) :-
  retractall(algeweestW(_)),
  fail.
voorrondeT(Cat, VoorrondeX) :-
  getbacktrack(Here1),
  koplvT(_, Cat),
  cutbacktrack(Here1),		% als er één is, dan allemaal in volgorde
  t_wc(wc(Voorronde, _, _, _, _, _, _, __, _, _, _, _, _, _, _)),
  getbacktrack(Here),
  koplvT(Voorronde, Cat),
  cutbacktrack(Here),
  voorrondeAT(Voorronde, VoorrondeX),
  not(algeweestW(VoorrondeX)),
  assert(algeweestW(VoorrondeX)).
voorrondeT(_Cat, _Voorronde) :-
  retractall(algeweestW(_)),
  fail.

voorrondeAT(Cat, Cat).
voorrondeAT(Cat, VoorrondeX) :-
  getbacktrack(Here1),
  koplvT(_, Cat),
  cutbacktrack(Here1),		% als er één is, dan allemaal in volgorde
  t_wc(wc(Voorronde, _, _, _, _, _, _, __, _, _, _, _, _, _, _)),
  getbacktrack(Here),
  koplvT(Voorronde, Cat),
  cutbacktrack(Here),
  voorrondeAT(Voorronde, VoorrondeX).




hdg2mvgn(_, KWin, SNamen, b_True) :-
  GWin = win_GetCtlHandle(KWin, idc_gemengd),
  win_Check(GWin, b_False),
  MWin = win_GetCtlHandle(KWin, idc_heren),  
  win_Check(MWin, b_False),  
  VWin = win_GetCtlHandle(KWin, idc_dames),  	% gemengd
  win_Check(VWin, b_False),
  NWin = win_GetCtlHandle(KWin, idc_nvt),
  win_Check(NWin, b_False),
  findall(SNaam, spsrtInList(SNaam, [m,v]), SNamen), !.
hdg2mvgn(gemengd, KWin, SNamen, _) :-
  GWin = win_GetCtlHandle(KWin, idc_gemengd),  
  MWin = win_GetCtlHandle(KWin, idc_heren),  
  VWin = win_GetCtlHandle(KWin, idc_dames),  	% gemengd
  NWin = win_GetCtlHandle(KWin, idc_nvt),
  win_CheckRadioButton(GWin, [GWin, MWin, VWin, NWin]), 
  findall(SNaam, spsrtInList(SNaam, [m,v]), SNamen), !.
hdg2mvgn(heren, KWin, SNamen, _) :-
  GWin = win_GetCtlHandle(KWin, idc_gemengd),  % man
  MWin = win_GetCtlHandle(KWin, idc_heren),  
  VWin = win_GetCtlHandle(KWin, idc_dames),  
  NWin = win_GetCtlHandle(KWin, idc_nvt),
  win_CheckRadioButton(MWin, [GWin, MWin, VWin, NWin]), 
  findall(SNaam, spsrtInList(SNaam, [m]), SNamen), !.
hdg2mvgn(dames, KWin, SNamen, _) :-
  GWin = win_GetCtlHandle(KWin, idc_gemengd),  % vrouw
  MWin = win_GetCtlHandle(KWin, idc_heren),  
  VWin = win_GetCtlHandle(KWin, idc_dames),  
  NWin = win_GetCtlHandle(KWin, idc_nvt),
  win_CheckRadioButton(VWin, [GWin, MWin, VWin, NWin]), 
  findall(SNaam, spsrtInList(SNaam, [v]), SNamen), !.
hdg2mvgn(hdnvt, KWin, SNamen, _) :-
  GWin = win_GetCtlHandle(KWin, idc_gemengd),  
  MWin = win_GetCtlHandle(KWin, idc_heren),  
  VWin = win_GetCtlHandle(KWin, idc_dames),  
  NWin = win_GetCtlHandle(KWin, idc_nvt),
  win_CheckRadioButton(NWin, [GWin, MWin, VWin, NWin]), % nvt
  findall(SNaam, spsrtInList(SNaam, [m,v]), SNamen), !.

edg2ed(e, KWin, b_True) :-
  EWin = win_GetCtlHandle(KWin, idc_enkelspel),
  win_Check(EWin, b_False),  
  DWin = win_GetCtlHandle(KWin, idc_dubbel),
  win_Check(DWin, b_False), !.  
edg2ed(e, KWin, _) :-
  EWin = win_GetCtlHandle(KWin, idc_enkelspel),  
  DWin = win_GetCtlHandle(KWin, idc_dubbel),  
  win_CheckRadioButton(EWin, [EWin, DWin]), !.
edg2ed(_, KWin, _) :-
  EWin = win_GetCtlHandle(KWin, idc_enkelspel),  
  DWin = win_GetCtlHandle(KWin, idc_dubbel),  
  win_CheckRadioButton(DWin, [EWin, DWin]), !.

geldstr(0.0, "") :- !.
geldstr(Geld, Str) :-
  format(Str, "%7.2f",Geld), !.
geldstr(_, "").

strgeld(In, Geld) :-
  searchchar(In, ',',FoundPos),
  F1 = FoundPos - 1,
  frontstr(F1,In,StartString,_),
  frontstr(FoundPos, In, _, Rest),
  format(Str, "%s%c%s", StartString, '.', Rest),
  str_real(Str, Geld), !.
strgeld(In, Geld) :-
  str_real(In, Geld), !.
strgeld(_, 0.0).

strsterkte(In, St) :-
  fronttoken(In,St,_), !.
strsterkte(_, "").

getSex(KWin, heren)  :-
  EWin = win_GetCtlHandle(KWin, idc_heren),  
  b_True = win_IsChecked(EWin), !.
getSex(KWin, dames)  :-
  GWin = win_GetCtlHandle(KWin, idc_dames),  
  b_True = win_IsChecked(GWin), !.
getSex(KWin, gemengd)  :-
  GWin = win_GetCtlHandle(KWin, idc_gemengd),  
  b_True = win_IsChecked(GWin), !.
getSex(_KWin, hdnvt).

getWedsTyp(KWin, e) :- 
  EWin = win_GetCtlHandle(KWin, idc_enkelspel),  
  b_True = win_IsChecked(EWin), !.
getWedsTyp(_KWin, d). 

iteml2sexl([Item|List], [Sexe|Uit]) :-
  str_int(Item, Sexe), !,
  iteml2sexl(List, Uit).
iteml2sexl(_, []).

getleeft(In, Uit) :-
  str_int(In, Uit), !.
getleeft(_, 0).

isNieuw("naam","kort", b_True) :- !.
isNieuw(_, _, b_False).

setOnd(CatxNr) :-
  t_vink(Catx, CatxNr),
  kenmerkWin(KWin),
  t_wc(wc(Catx, OnderdeelID, _Orde, Lang, EDG, Kort, SexL, Sterkte, leeftijd(_LeeftVanStrx,LeeftVan), leeftijd(_LeeftTotStrx,LeeftTotM), HDG, Schemasoort, Status, Tid, Geld)),
  isNieuw(Lang, Kort, Nieuw),
  NWin = win_GetCtlHandle(KWin, idc_onderdeelnaam),
  win_SetText(NWin, Lang),
  KNWin = win_GetCtlHandle(KWin, idc_kortenaam),
  win_SetText(KNWin, Kort),
  hdg2mvgn(HDG, KWin, Spelersoorten, Nieuw),
  edg2ed(EDG, KWin, Nieuw),
  geldstr(Geld, GeldStr),
  GWin = win_GetCtlHandle(KWin, idc_inschr),
  win_SetText(GWin, GeldStr),
  lf2d_lf(LeeftVan, _, LeeftVanStr),
  VWin = win_GetCtlHandle(KWin, idc_leeftvan),
  win_SetText(VWin, LeeftVanStr),
  lf2d_lf(LeeftTotM, _, LeeftTotStr),
  TWin = win_GetCtlHandle(KWin, idc_leefttm),
  win_SetText(TWin, LeeftTotStr),
  SpWin = win_GetCtlHandle(KWin, idcl_spelersoorten),
  lbox_Clear(SpWin),
  lbox_Add(SpWin, Spelersoorten),
  selectedSrt(SpWin, Spelersoorten, SexL, 0),
  StWin = win_GetCtlHandle(KWin, idc_spst),
  StLijst = lbox_GetAll(StWin),
  sterkteSel(Sterkte, StLijst, 0, StSelect),
  lbox_SetSel(StWin, StSelect, b_True),
  ScWin = win_GetCtlHandle(KWin, idc_schemasoorti),
  win_SetText(ScWin, Schemasoort),
  nietUpdaten(KWin, CatX, Status, Tid, OnderdeelId), !.
  %KMedWin = win_GetCtlHandle(KWin, idc_edit),
  %setKeurMededeling(KMedWin, Catx), !.
setOnd(_CatxNr) :-
  write("\nsetond failed tastabel 2621").

nietUpdaten(Win, Cat, vanknltb, Tnr, OnderdeelId) :-
  dialog_SetState(Win, [enable(idc_enkelspel, b_False),enable(idc_dubbel, b_False),enable(idc_dames, b_False),
    enable(idc_heren, b_False),enable(idc_gemengd, b_False),enable(idc_spst, b_False),enable(idc_leeftvan, b_False),
    enable(idc_nvt, b_False),enable(idc_leefttm, b_False),enable(idc_af, b_False)]),
  NBwin = win_GetCtlHandle(Win, idct_toelichting),
  toernooiKp(Tnr, Type),
  findall(No, opg(No,Cat,_,_,_,_,_,_,_,_,_),NoL),
  count(NoL,0,Aantal),
  format(Msg, "NB Dit onderdeel wordt bepaald door de KNLTB  en specifieke kenmerken kunnen niet gewijzigd worden (%s,%s,%u)\nEr zijn %u opgaven.", Type,OnderdeelId,Cat,Aantal),
  win_SetText(NBwin, Msg),
  !.
nietUpdaten(Win, Cat, reserve(TnrMsgX), Tnr, OnderdeelId) :-
  dialog_SetState(Win, [enable(idc_enkelspel, b_False),enable(idc_dubbel, b_False),enable(idc_dames, b_False),
    enable(idc_heren, b_False),enable(idc_gemengd, b_False),enable(idc_spst, b_False),enable(idc_leeftvan, b_False),
    enable(idc_nvt, b_False), enable(idc_leefttm, b_False),enable(idc_af, b_False)]),
  TnrMsgX = [TnrMsg|_],
  NBwin = win_GetCtlHandle(Win, idct_toelichting),
  toernooiKp(Tnr, Type),
  findall(No, opg(No,Cat,_,_,_,_,_,_,_,_,_),NoL),
  count(NoL,0,Aantal),
  format(Msg, "NB Dit onderdeel wordt bepaald door de KNLTB  en specifieke kenmerken kunnen niet gewijzigd worden (%s,%s,%u)\n%s\nEr zijn %u opgaven.", Type,OnderdeelId,Cat,TnrMsg,Aantal),
  win_SetText(NBwin, Msg),
  !.
nietUpdaten(Win, Cat, _, _, _) :-
  not(kopl(Cat,_,_,_)),
  not(kopl(_,_,Cat,_)),
  dialog_SetState(Win,[enable(idc_af, b_True)]),
  fail.
nietUpdaten(Win, Cat, _, _, _) :-
  dialog_SetState(Win, [enable(idc_enkelspel, b_True),
  enable(idc_dubbel, b_True),
  enable(idc_dames, b_True),
  enable(idc_heren, b_True),
  enable(idc_gemengd, b_True),
  enable(idc_nvt, b_True),
  enable(idc_spst, b_True),
  enable(idc_leeftvan, b_True),
  enable(idc_af, b_True),
  enable(idc_leefttm, b_True)]),
  NBwin = win_GetCtlHandle(Win, idct_toelichting),
  findall(No, opg(No,Cat,_,_,_,_,_,_,_,_,_),NoL),
  count(NoL,0,Aantal),
  format(Msg, "(%u)\nEr zijn %u opgaven.",Cat,Aantal), 
  win_SetText(NBwin, Msg). 

setHDG(KWin, gemengd) :-
  GWin = win_GetCtlHandle(KWin, idc_gemengd),
  b_True = win_IsChecked(GWin), !. 
setHDG(KWin, heren) :-
  MWin = win_GetCtlHandle(KWin, idc_heren),  
  b_True = win_IsChecked(MWin), !. 
setHDG(KWin, dames) :-
  DWin = win_GetCtlHandle(KWin, idc_dames),  	% gemengd
  b_True = win_IsChecked(DWin), !. 
setHDG(KWin, hdnvt) :-
  NWin = win_GetCtlHandle(KWin, idc_nvt),
  b_True = win_IsChecked(NWin), !. 
setHDG(_, gemengd) :-
  dlg_note("", "Selecteer de juiste optie!"),
  fail.
  
getOnd(0) :- !.
getOnd(CatxNr) :-
  t_vink(Catx, CatxNr),
  kenmerkWin(KWin),
  NWin = win_GetCtlHandle(KWin, idc_onderdeelnaam),
  Lang = win_GetText(NWin),
  fronttoken(Lang, _, _),
  trim_c(Lang),
  KNWin = win_GetCtlHandle(KWin, idc_kortenaam),
  Kort = win_GetText(KNWin),
  fronttoken(Kort, _, _),
  getSex(KWin, HDG),
  getWedsTyp(KWin, EDG),
  SpWin = win_GetCtlHandle(KWin, idcl_spelersoorten),
  lbox_GetSel(SpWin,ItemList,_IndexList),
  iteml2sexl(ItemList, SexL),
  GeldWin = win_GetCtlHandle(KWin, idc_inschr),
  GeldStr = win_GetText(GeldWin),
  strgeld(GeldStr, Geld),
  StWin = win_GetCtlHandle(KWin, idc_spst),
  StStr = win_GetText(StWin),
  strsterkte(StStr, Sterkte),
  VWin = win_GetCtlHandle(KWin, idc_leeftvan),
  LeeftVanStr = win_GetText(VWin),
  getleeft(LeeftVanStr, LeeftVan),
  TWin = win_GetCtlHandle(KWin, idc_leefttm),
  LeeftTotMStr = win_GetText(TWin),
  getleeft(LeeftTotMStr, LeeftTotM),
  ScWin = win_GetCtlHandle(KWin, idc_schemasoorti),
  Schemasoort = win_GetText(ScWin),
  setHDG(KWin, HDG),
  getbacktrack(Here),
  retract(t_wc(wc(CatX, OnderdeeelID, Orde, _, _, _, _, _, _, _, _HDG, _, Status, KNLTB, _))),
  retractall(waarschuwen),
  assert(waarschuwen),
  assert(t_wc(wc(Catx, OnderdeeelID, Orde, Lang, EDG, Kort, SexL, Sterkte, leeftijd(LeeftVanStr,LeeftVan), leeftijd(LeeftTotMStr,LeeftTotM), HDG, Schemasoort, Status, KNLTB, Geld))),
  cutbacktrack(Here),
  !.
getOnd(_).
  %  dlg_note("\getOnd failed tastabel 2767!!!").
  
zijnErOpgaven(Win) :-
  getbacktrack(Here),
  OnderdelenWin = win_GetCtlHandle(Win, idc_custom),
  cTreeView::get_selected(OnderdelenWin, CatNr),
  t_vink(Cat, CatNr),
  %write("\nCat: ", Cat),
  opg(_,Cat,_,_,_,_,_,_,_,_,_),
  cutbacktrack(Here),
  assert(noRadio(b_True)),
  An = dlg_MessageBox( "Geslacht", "Er zijn al opgaven!\nToch veranderen?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
  assert(noRadio(b_False)), !,
  An = 1.
zijnErOpgaven(_Win).

sexProbleem(Old) :-
  Old <> 0,
  kenmerkWin(KWin),
  GWin = win_GetCtlHandle(KWin, idc_gemengd),  
  b_False = win_IsChecked(GWin),
  MWin = win_GetCtlHandle(KWin, idc_heren),  
  b_False = win_IsChecked(MWin),
  VWin = win_GetCtlHandle(KWin, idc_dames),  
  b_False = win_IsChecked(VWin),
  NWin = win_GetCtlHandle(KWin, idc_nvt),  
  b_False = win_IsChecked(NWin),
  dlg_MessageBox( "Dames/heren/gemengd/nvt", "Dames/heren/gemengd/nvt\nselecteren svp!", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  win_PostEvent(KWin, e_User(c_setfocus, idc_dames)),
  !.
sexProbleem(Old) :-
  Old <> 0,
  kenmerkWin(KWin),
  %getSex(KWin, EDG),
  SpWin = win_GetCtlHandle(KWin, idcl_spelersoorten),
  %NWin = win_GetCtlHandle(KWin, idc_nvt),  
  %b_False = win_IsChecked(NWin),
  lbox_GetSel(SpWin,ItemList,_IndexList),
  ItemList = [],
  dlg_MessageBox( "Spelersoort", "Spelersoort\nselecteren svp!", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  win_PostEvent(KWin, e_User(c_setfocus, idcl_spelersoorten)), !.
sexProbleem(Old) :-
  Old <> 0,
  kenmerkWin(KWin),
  getSex(KWin, HDGN),
  HDGN = gemengd,
  SpWin = win_GetCtlHandle(KWin, idcl_spelersoorten),
  lbox_GetSel(SpWin,ItemList,_IndexList),
  iteml2sexl(ItemList, SexL),
  not(mvInSexL(SexL)),
  dlg_MessageBox( "Spelersoort", "Spelersoort\nMannelijk èn vrouwelijk selecteren svp!", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  win_PostEvent(KWin, e_User(c_setfocus, idcl_spelersoorten)), !.

mvInSexL(SexL) :-
  member(Spsrt1, SexL),
  spsrt(Spsrt1, _, v, _, _, _, _, _),
  member(Spsrt2, SexL),
  spsrt(Spsrt2, _, m, _, _, _, _, _), !.

naamProbleem(Old) :-
  Old <> 0,
  kenmerkWin(KWin),
  NWin = win_GetCtlHandle(KWin, idc_onderdeelnaam),
  Lang = win_GetText(NWin),
  not(fronttoken(Lang, _, _)),
  dlg_MessageBox( "Naam", "Naam invullen svp!", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  win_PostEvent(KWin, e_User(c_setfocus, idc_onderdeelnaam)), !.
naamProbleem(Old) :-
  Old <> 0,
  kenmerkWin(KWin),
  NWin = win_GetCtlHandle(KWin, idc_onderdeelnaam),
  Lang = win_GetText(NWin),
  searchchar(Lang,'"',_FoundPos),
  dlg_MessageBox( "Naam", "Naam mag geen \"-teken bevatten!", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  win_PostEvent(KWin, e_User(c_setfocus, idc_onderdeelnaam)), !.
naamProbleem(OldNr) :-
  OldNr <> 0,
  t_vink(Old, OldNr),
  kenmerkWin(KWin),
  NWin = win_GetCtlHandle(KWin, idc_onderdeelnaam),
  Lang = win_GetText(NWin),
  trim_c(Lang),
  t_wc(wc(Ond, _, _, Lang, _, _, _, _, _, _, _, _, _, _, _)),
  Ond <> Old,
  format(LNaam, "%s\nis niet uniek!", Lang),
  dlg_MessageBox( "Naam", LNaam, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  win_PostEvent(KWin, e_User(c_setfocus, idc_onderdeelnaam)), !.
naamProbleem(Old) :-
  Old <>0,
  kenmerkWin(KWin),
  KNWin = win_GetCtlHandle(KWin, idc_kortenaam),
  Kort = win_GetText(KNWin),
  not(fronttoken(Kort, _, _)),
  dlg_MessageBox( "Korte Naam", "Korte naam invullen svp!", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  win_PostEvent(KWin, e_User(c_setfocus, idc_kortenaam)), !.
naamProbleem(OldNr) :-
  OldNr <> 0,
  t_vink(Old, OldNr),
  kenmerkWin(KWin),
  NWin = win_GetCtlHandle(KWin, idc_kortenaam),
  Kort = win_GetText(NWin),
  trim_c(Kort),
  t_wc(wc(Ond, _, _, _, _, Kort, _, _, _, _, _, _, _, _, _)),
  Ond <> Old,
  dlg_MessageBox( "Korte Naam", "niet uniek!", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  win_PostEvent(KWin, e_User(c_setfocus, idc_kortenaam)), !.

damesHerenMatch(Old) :-
  Old <>0,
  kenmerkWin(KWin),
  DWin = win_GetCtlHandle(KWin, idc_dames),
  DChecked  = win_IsChecked(DWin),
  HWin = win_GetCtlHandle(KWin, idc_heren),
  HChecked  = win_IsChecked(HWin),
  GWin = win_GetCtlHandle(KWin, idc_gemengd),
  GChecked  = win_IsChecked(GWin),
  NWin = win_GetCtlHandle(KWin, idc_nvt),
  NChecked  = win_IsChecked(NWin),
  OWin = win_GetCtlHandle(KWin, idc_onderdeelnaam),
  Naam = win_GetText(OWin),
  upper_lower(Naam, NaamL),
  damesHerenMatch(KWin, DChecked, HChecked, GChecked, NChecked, NaamL), !.
  
damesHerenMatch(KWin, _DChecked, _HChecked, b_False, b_False, NaamL) :-
  damesHerenMatchMessage(KWin, NaamL, "gemengd", "Gemengd", idc_gemengd),  !.
damesHerenMatch(KWin, b_False, _HChecked, b_False, b_False, NaamL) :-
  damesHerenMatchMessage(KWin, NaamL, "dame", "Dames", idc_dames),  !.
damesHerenMatch(KWin, b_False, _HChecked, b_False, b_False, NaamL) :-
  damesHerenMatchMessage(KWin, NaamL, "meisje", "Dames", idc_dames),  !.
damesHerenMatch(KWin, _DChecked, b_False, b_False, b_False, NaamL) :-
  damesHerenMatchMessage(KWin, NaamL, "heren", "Heren", idc_heren),  !.
damesHerenMatch(KWin, _DChecked, b_False, b_False, b_False, NaamL) :-
  damesHerenMatchMessage(KWin, NaamL, "jongen", "Heren", idc_heren),  !.

damesHerenMatchMessage(KWin, NaamL, TekstElement, Selectie, Control) :-
  searchstring(NaamL,TekstElement,_F),
  format(Vraag, "Er komt \"%s\" in de naam voor\nmoet \"%s\" geselecteerd worden?", TekstElement, Selectie),
  1 = dlg_MessageBox( "Dames/heren/gemengd", Vraag, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
  win_PostEvent(KWin, e_User(setmanvrgemengd, Control)),
  !.

enkeldubbelMatch(Old) :-
  Old <> 0,
  kenmerkWin(KWin),
  EDWin = win_GetCtlHandle(KWin, idc_enkelspel),
  E_D  = win_IsChecked(EDWin),
  OWin = win_GetCtlHandle(KWin, idc_onderdeelnaam),
  Naam = win_GetText(OWin),
  upper_lower(Naam, NaamL),
  enkeldubbelMatch(KWin, E_D, NaamL), !.

enkeldubbelmatch(Win, b_True, NaamL) :-	% enkelattr en dubbel in de naam
  searchstring(NaamL,"dubbel",_F),
  1 = dlg_MessageBox( "Enkel of dubbel", "Er komt \"dubbel\" in de naam voor\nmoet het dan geen dubbelspel zijn?", mesbox_iconexclamation, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
  win_PostEvent(Win, e_User(setenkeldubbel, idc_dubbel)),
  !.
enkeldubbelmatch(Win, b_False, NaamL) :-	% enkelattr en dubbel in de naam
  searchstring(NaamL,"enkel",_F),
  1 = dlg_MessageBox( "Enkel of dubbel", "Er komt \"enkel\" in de naam voor\nmoet het dan geen enkelspel zijn?", mesbox_iconexclamation, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
  win_PostEvent(Win, e_User(setenkeldubbel, idc_enkelspel)),
  !.
  
%callbackO(_Win,Event):-
%  nl, write(Event), fail.
callbackO(_,keydown(45)):-			% insert
  kenmerkWin(Win),
  win_PostEvent(Win,e_User(ereenbij, 0)), !.
callbackO(_,keydown(46)):-			% delete
  kenmerkWin(Win),
  win_PostEvent(Win,e_User(c_ereenaf, 0)), !.
callbackO(_Win, selecting(Old, _)) :-
  refresh(b_True),
  naamProbleem(Old), !.
callbackO(_Win, selecting(Old, _)) :-
  refresh(b_True),
  sexProbleem(Old), !.
callbackO(_Win, selecting(Old, _)) :-
  refresh(b_True),
  enkeldubbelMatch(Old), !.
callbackO(_Win, selecting(Old, _)) :-
  refresh(b_True),
  damesHerenMatch(Old), !.
callbackO(_Win,selected(Old, Catx)):-
  getOnd(Old),
  setOnd(Catx),
  enableControls(Catx),
  close_alles(),
  refresh(b_True),
  posP(Cat, Catx, _, Pos, _),	% selecteer als ingedeeld	% 21.8.2004
  select(Cat, Pos),
  t_wc(wc(Catx, _, _, Naam, _, _, _, _, _, _, _, _, _, _, _)),
  setOndNaam(Catx, Naam),
  win_SetFocus(_Win),
  fail.

enableControls(CatSelected) :-
  kenmerkWin(Win),
  dialog_SetState(Win, [enable(idc_neer, b_False),enable(idc_op, b_False)]),
  OnderdelenWin = win_GetCtlHandle(Win, idc_custom),
  %cTreeView::get_selected(OnderdelenWin, CatSelected),
  cTreeView::get_next(OnderdelenWin, CatSelected, Next),
  Next <> 0,
  dialog_SetState(Win, [enable(idc_neer, b_True)]),
  fail.
enableControls(CatSelected) :-		% voorlopig alleen idc_af
  kenmerkWin(Win),
  OnderdelenWin = win_GetCtlHandle(Win, idc_custom),
  %cTreeView::get_selected(OnderdelenWin, CatSelected),
  cTreeView::get_previous(OnderdelenWin, CatSelected, Prev),
  Prev <> 0,
  dialog_SetState(Win, [enable(idc_op, b_True)]),
  fail.
enableControls(_Cat).

relevantesoort2(Sex, SrtStr)  :-
  spsrt(Spsrt2, _, Sex, _, _, _, _, _),
  str_int(SrtStr, Spsrt2).

checkRelevanteSoorten(SWin, Sex, [Head|Tail], N) :-
  str_int(Head, Spsrt2),
  spsrt(Spsrt2, _, Sex, _, _, _, _, _),
  lbox_SetSel(SWin, N, b_True),
  N1 = N + 1, !,
  checkRelevanteSoorten(SWin, Sex, Tail, N1).
checkRelevanteSoorten(SWin, Sex, [_|Tail], N) :-
  N1 = N + 1, !,
  checkRelevanteSoorten(SWin, Sex, Tail, N1).
checkRelevanteSoorten(_, _, _, _).

maakgelijkOndK(_Win) :-		% update het schema nog even
  OnderdelenWin = win_GetCtlHandle(_Win, idc_custom),
  cTreeView::get_selected(OnderdelenWin, CatNr),
  getOnd(CatNr),
  t_vink(Cat, CatNr),
  t_wc(wc(Cat, _OnderdeeelID, _Orde, Naam, _EDG, _Kort, _SexL, _SpStrk, _LftV, _LftTM, _HDG, _Schemasoort, _Status, _KNLTB, _Geld)),
  setOndNaam(Cat, Naam),
  fail.
maakgelijkOndK(Win) :-
  treeond(Win, Onderdelen),
  %OWin = win_GetCtlHandle(Win, idc_custom),
  %cTreeView::get_all(OWin, TreeList),	% de hele bubs
  %treeOnd(TreeList, [], Onderdelen),
  t_wc(wc(Ond, _Naam, _, _, _, _, _, _, _, _, _, _, _, _, _)),
  not(member(Ond, Onderdelen)),
  %write("\nDeleted: ", Naam),  
  retract(t_wc(wc(Ond, _, _, _, _, _, _, _, _, _, _, _, _, _, _))),
  fail.
maakgelijkOndK(_Win) :-		% gedelete wedstrijdcats
  wc(Ond, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
  not(t_wc(wc(Ond, _, _, _, _, _, _, _, _, _, _, _, _, _, _))),
  not(opg(_,Ond,_,_,_,_,_,_,_,_,_)),
  retract(wc(Ond, _, _, _, _, _, _, _, _, _, _, _, _, _, _)),
  retractall(wd(_, Ond, _, _, _, _, _)),
  retractall(w2(_, Ond, _, _, _, _, _, _)),
  retractall(w3(_, Ond, _, _, _, _, _)),
  retractall(ws(_, Ond, _, _)),
  retractall(pos(Ond, _, _, _)),
  retractall(kopl(_, _, Ond, _)),	% alleen als eindstation
  fail.
maakgelijkOndK(Win) :-		% voeg bij en pas aan op volgorde
  OWin = win_GetCtlHandle(Win, idc_custom),
  cTreeView::get_all(OWin, TreeList),	% de hele bubs
  loglock(lees, "tabond"),
  logclose(),
  maakgelijkOndK1(TreeList),
  save("kenfacts.txt", kenm),
  maakgelijkRest(), 
  %not(kompaan),
  compress(update),
  startDbRefresh(), !.
maakgelijkOndK(_).

treeond(Win, Onderdelen) :-
  OWin = win_GetCtlHandle(Win, idc_custom),
  cTreeView::get_all(OWin, TreeList),	% de hele bubs
  treeond(Treelist,[], Onderdelen).

treeond([tree(OndNr, _, _, _, _, SubList)|Rest], In, Uit) :-	% alle onderdelen in de tree
  t_vink(Ond, OndNr),
  treeond(SubList, [Ond],U1),
  append(In,U1, In1), !,
  treeond(Rest, In1, Uit).
treeond(_, In, In).

/*platslaan(O, [X|_]) :-
  X = tree(OndT, _, _, _, _, _SubList),
  t_vink(O, OndT). 
platslaan(O, [X|_]) :-
  X = tree(_, _, _, _, _, SubList),
  platslaan(O, SubList).
platslaan(O, [_|Rest]) :-
  platslaan(O, Rest).*/

maakgelijkOndK1([])  :- !.
maakgelijkOndK1([X|Tail])  :-			% filter de naamloze er uit
  X = tree(OndT, _, _, _, _, _SubList),
  t_vink(Ond, OndT), 
  t_wc(wc(Ond, _OnderdeeelID, _Orde, Naam, _, _, _, _, _, _, _, _, _, _, _)),
  trim_c(Naam),
  Naam = "",
  retractall(wc(Ond, _, _, _, _, _, _, _, _, _, _, _, _, _, _)),!,
  maakgelijkOndK1(Tail).
maakgelijkOndK1([X|Tail])  :-
  X = tree(OndT, _, _, _, _, SubList),
  t_vink(Ond, OndT), 
  t_wc(wc(Ond, OnderdeeelID, Orde, Naam, EDG, Kort, SexL, SpStrk, LftV, LftTM, HDG, Schemasoort, Status, KNLTB, Geld)),
  retractall(wc(Ond, _, _, _, _, _, _, _, _, _, _, _, _, _, _)), !,
  assertz(wc(Ond, OnderdeeelID, Orde, Naam, EDG, Kort, SexL, SpStrk, LftV, LftTM, HDG, Schemasoort, Status, KNLTB, Geld)),
  maakgelijkOndK1(SubList),
  maakgelijkOndK1(Tail).
maakgelijkOndK1([tree(Ond, _, _, _, _, _)|Tail])  :-
  format(Msg, "Ond %s niet gevonden!", Ond),
  writeToJournaal(Msg),
  vpi_Alarm(mesbox_iconerror),
  maakgelijkOndK1(Tail), !.

isGelijkOndKG(Win) :-			
  OWin = win_GetCtlHandle(Win, idc_custom),
  cTreeView::get_selected(OWin, CatNrS),
  getOnd(CatNrS),
  findall(WCOld,wc(WCOld, _, _, _, _, _, _, _, _, _, _, _, _, _, _),OldList), 
  cTreeView::get_child(OWin, 0, CatNr),
  %writedevice(X),
  %vink(Cat, CatNr),
  isGelijkOndKG1(OWin, CatNr, OldList, Rest),
  Rest = [], !.

/*
maakgelijkRest() :-
  pos(Catx, Omvang, Plaats, Soort),
  not(t_wc(pos(Catx, Omvang, Plaats, Soort))),
  retractall(pos(Catx, Omvang, Plaats, Soort)),
  fail.
maakgelijkRest() :-
  t_wc(pos(Catx, Omvang, Plaats, Soort)),
  not(pos(Catx, Omvang, Plaats, Soort)),
  assertz(pos(Catx, Omvang, Plaats, Soort)),
  fail.
maakgelijkRest() :-
  opg(TN1,Catx,Tgst,Plx,Rondex,Uitslx,Nrx,LLWCx,TeamIdx,TeamStatusx,MTx),
  not(t_wc(opg(TN1,Catx,Tgst,Plx,Rondex,Uitslx,Nrx,LLWCx,TeamIdx,TeamStatusx,MTx))),
  retractall(opg(TN1,Catx,Tgst,Plx,Rondex,Uitslx,Nrx,LLWCx,TeamIdx,TeamStatusx,MTx)),
  fail.
maakgelijkRest() :-
  t_wc(pos(Catx, Omvang, Plaats, Soort)),
  not(pos(Catx, Omvang, Plaats, Soort)),
  assertz(pos(Catx, Omvang, Plaats, Soort)),
  fail.
maakgelijkRest() :-
  wd(WdNo,Catx,T1,T2,Uitsl,Tijd,Note),
  not(t_wc(wd(WdNo,Catx,T1,T2,Uitsl,Tijd,Note))),
  retractall(wd(WdNo,Catx,T1,T2,Uitsl,Tijd,Note)),
  fail.
maakgelijkRest() :-
  t_wc(wd(WdNo,Catx,T1,T2,Uitsl,Tijd,Note)),
  not(wd(WdNo,Catx,T1,T2,Uitsl,Tijd,Note)),
  assertz(wd(WdNo,Catx,T1,T2,Uitsl,Tijd,Note)),
  fail.
maakgelijkRest() :-
  w2(WdNo,Catx,Tijd,Gew1,Gew2,Fixed,Logtijd,Park),
  not(t_wc(w2(WdNo,Catx,Tijd,Gew1,Gew2,Fixed,Logtijd,Park))),
  retractall(w2(WdNo,Catx,Tijd,Gew1,Gew2,Fixed,Logtijd,Park)),
  fail.
maakgelijkRest() :-
  t_wc(w2(WdNo,Catx,Tijd,Gew1,Gew2,Fixed,Logtijd,Park)),
  not(w2(WdNo,Catx,Tijd,Gew1,Gew2,Fixed,Logtijd,Park)),
  assertz(w2(WdNo,Catx,Tijd,Gew1,Gew2,Fixed,Logtijd,Park)),
  fail.
maakgelijkRest() :-
  w3(WdNo,Catx,Tg1,Tg2, P1, P2, Spoor),
  not(t_wc(w3(WdNo,Catx,Tg1,Tg2, P1, P2, Spoor))),
  retractall(w3(WdNo,Catx,Tg1,Tg2, P1, P2, Spoor)),
  fail.
maakgelijkRest() :-
  t_wc(w3(WdNo,Catx,Tg1,Tg2, P1, P2, Spoor)),
  not(w3(WdNo,Catx,Tg1,Tg2, P1, P2, Spoor)),
  assertz(w3(WdNo,Catx,Tg1,Tg2, P1, P2, Spoor)),
  fail.
*/
maakgelijkRest() :-
  kopl(Caty, Van, Catx, Naar),
  not(t_wc(kopl(Caty, Van, Catx, Naar))),
  retractall(kopl(Caty, Van, Catx, Naar)),
  fail.
maakgelijkRest() :-
  t_wc(kopl(Caty, Van, Catx, Naar)),
  not(kopl(Caty, Van, Catx, Naar)),
  assertz(kopl(Caty, Van, Catx, Naar)),
  fail.
maakgelijkRest.

isGelijkOndKG1(_OWin, 0, Rest, Rest)  :- !.
isGelijkOndKG1(OWin, OndNr, [Ond|Tail], TailB)  :-
  t_vink(Ond, OndNr), 
  t_wc(wc(Ond, OnderdeeelID, Orde, Naam, EDG, Kort, SexL, SpStrk, leeftijd(_, LftV), leeftijd(_,LftM), HDG, Schemasoort, Status, KNLTB, Geld)),
  wc(Ond, OnderdeeelID, Orde, Naam, EDG, Kort, SexL, SpStrk, leeftijd(_, LftV), leeftijd(_,LftM), HDG, Schemasoort, Status, KNLTB, Geld), !,
  cTreeView::get_child(OWin, OndNr, Child),
  isGelijkOndKG1(OWin, Child, Tail, TailA),
  cTreeView::get_next(OWin, OndNr, Next),
  isGelijkOndKG1(OWin, Next, TailA, TailB).
isGelijkOndKG1(_OWin, OndNr, [Ond|_Tail], _TailB)  :-
  writedevice(screen),
  %term_str(file, WD, TD),
  %dlg_note("device", TD),
  t_vink(OndX, OndNr),
  Ond <> OndX,
  %dlg_note("","\nVolgorde is anders!"),
  write("\nvolgorde is anders!"),
  fail.  
isGelijkOndKG1(_OWin, OndNr, [Ond|_Tail], _TailB)  :-
  writedevice(screen),
  %WD = screen,
  t_vink(Ond, OndNr), 
  t_wc(wc(Ond, OnderdeeelID, Orde, Naam, EDG, Kort, SexL, SpStrk, leeftijd(LVStr, LftV), leeftijd(LTStr,LftM), HDG, Schemasoort, Status, KNLTB, Geld)),
  X = wc(Ond, OnderdeeelID, Orde, Naam, EDG, Kort, SexL, SpStrk, leeftijd(LVStr, LftV), leeftijd(LTStr,LftM), HDG, Schemasoort, Status, KNLTB, Geld),
  write("\nMismatch\n", X),
  wc(Ond, OnderdeeelID1, Orde1, Naam1, EDG1, Kort1, SexL1, SpStrk1, leeftijd(LVstr1, LftV1), leeftijd(LTStr1,LftM1), HDG1, Schemasoort1, Status1, KNLTB1, Geld1),
  Y = wc(Ond, OnderdeeelID1, Orde1, Naam1, EDG1, Kort1, SexL1, SpStrk1, leeftijd(LVstr1, LftV1), leeftijd(LTStr1,LftM1), HDG1, Schemasoort1, Status1, KNLTB1, Geld1),
  write("\n", Y),
  fail.  

opgavenVoor(Win, CatNr, OpgList, NumList) :-
  cTreeView::get_child(Win, CatNr, Child),		% alles wat er onder hangt
  tree2list(Win, Child, [CatNr], CatList),
  findall(Opg, catlist2opg(CatList, Opg), OpgList),
  findall(Num, catlist2plan(CatList, Num), NumList).

tree2list(_OWin, 0, Rest, Rest)  :- !.		% zoek alle onderliggende cats op
tree2list(OWin, OndNr, In, [Ond|Out])  :-
  t_vink(Ond, OndNr),
  cTreeView::get_child(OWin, OndNr, Child),
  tree2list(OWin, Child, In, In1),
  cTreeView::get_next(OWin, OndNr, Next),
  tree2list(OWin, Next, In1, Out), !.
tree2list(_OWin, _OndNr, _, []).

catlist2opg(CatList, Opg) :-
  member(Cat, CatList),
  opg(Opg,Cat,_,_,_,_,_,_,_,_,_).

catlist2plan(CatList, Num) :-
  member(Cat, CatList),
  w2(Num,Cat,_,_,_,_,_,_).

checkPaswoord(Parent) :-
  w2(_Num,_Cat,_,_,_,_,_,_), !,		% als er planning is
  dlg_Paswoord_create(Parent, Return),
  Return = idc_ok.
checkPaswoord(_).

insertPoint(OnderdelenWin, CatSelected, after(Prev1)) :-
  cTreeView::get_previous(OnderdelenWin, CatSelected, Prev),
  Prev <> 0,
  cTreeView::get_previous(OnderdelenWin, Prev, Prev1),
  Prev1 <> 0, !.
insertPoint(OnderdelenWin, CatSelected, first()) :-
  cTreeView::get_previous(OnderdelenWin, CatSelected, Prev),
  Prev <> 0, !.

verwisselPoint(LWin, CatSelected, CatAndere, after(Prev)) :-
  cTreeView::get_previous(LWin, CatSelected, Prev),
  Prev <> 0,
  Prev <> CatAndere, !.
verwisselPoint(LWin, CatSelected, CatAndere, after(Prev1)) :-
  cTreeView::get_previous(LWin, CatSelected, Prev),
  Prev <> 0,
  Prev = CatAndere, 
  cTreeView::get_previous(LWin, Prev, Prev1),
  Prev1 <> 0, !.
verwisselPoint(_OnderdelenWin, _CatSelected, _Catandere, first()).

deleteAllCatSelectedNodes(OnderdelenWin, Cat) :-
  t_vink(CatNr, Cat),
  cTreeView::delete_item(OnderdelenWin, CatNr), 		% delete
  fail.
deleteAllCatSelectedNodes(_Win, _Cat).

closeMeteenKenmerken() :-
  waarschuwen,
  kenmerkWin(Win), 
  dlg_MessageBox( "On-line", "Het toernooibestand is door een andere gebruiker on-line veranderd!\nStart OnderdeelKenmerken opnieuw!", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  win_Destroy(Win), !.
closeMeteenKenmerken().


behandelLege(_Cat) :-
  kenmerkWin(KWin),
  NWin = win_GetCtlHandle(KWin, idc_onderdeelnaam),
  Lang = win_GetText(NWin),
  trim_c(Lang),
  Lang = "", !.
behandelLege(Cat) :-
  not(naamProbleem(Cat)),
  not(sexProbleem(Cat)),
  not(enkeldubbelMatch(Cat)).

soortgelijkeCat(Cat, KenStr, CatN) :-  % cats die in aanmerking komen
  t_wc(wc(Cat, _, _, _, EDa, _, SexL, Sterkte, leeftijd(_,_), leeftijd(_,_), HDG, _, _, _, _)),
  t_wc(wc(Catx, _OnderdeelID, _Orde, Lang, EDb, _Kort, SexL, Sterkte, leeftijd(_LeeftVanStrx,LeeftVan), leeftijd(_LeeftTotStrx,LeeftTotM), HDG, _Schemasoort, _Status, Tid, _Geld)),
  edgmatch(EDa, EDb),
  Catx <> Cat,
  fronttoken(Tid, _, _),
  not(pos(Catx,_,_,_)),
  term_str(wedsex,HDG, HDGStr),
  format(KenStr, "%s %s %-2u %-2u %s",Lang, HDGStr, LeeftVan, LeeftTotM, Tid),
  t_vink(Catx, CatN).  


verhuisWC(Catx, CatSel) :-
  retractall(waarschuwen),
  %wc(Catx, _,_,  _, _, _, _, _, _, _, _, _, _, _, Geld),
  getbacktrack(Here),
  retract(t_wc(wc(CatSel, OnderdeeelIDx, Ordex,  Naamx, WTypx, Kortx, SexList1x, SpSt1x, Londerx, Lbovenx, HDGx, Schemasoortx, Keuringx, KNLTBx, Geld))), % het KNLTB onderdeel
  retract(t_wc(wc(Catx, OnderdeeelID,Orde,  Naam, WTyp, Kort, SexList1, SpSt1, Londer, Lboven, HDG, Schemasoort, Keuring, KNLTB, Gld))),                  % Niet Knltb
  cutbacktrack(Here),
  TnewK = wc(Catx, OnderdeeelIDx, Orde,  Naam, WTypx, Kort, SexList1x, SpSt1x, Londerx, Lbovenx, HDGx, Schemasoortx, Keuringx, KNLTBx, Geld),          % het nieuwe KNLTB onderdeel
  ToldK = wc(CatSel, OnderdeeelID,Ordex,  Naamx, WTyp, Kortx, SexList1, SpSt1, Londer, Lboven, HDG, Schemasoort, Keuring, KNLTB, Gld),
  assertz(t_wc(TNewK)),
  assertz(t_wc(ToldK)),
  fail.
verhuisWC(Catx, CatSel) :-  
  %kopl(Caty, Van, Catx, Naar),
  retract(t_wc(kopl(Catx, Van, Caty, Naar))),
  assertz(t_wc(kopl(CatSel, Van, Caty, Naar))),
  fail.
verhuisWC(_Catx, _CatSel).
  %logclose(). 

teVerbindenT(Cat1, OnderdeelPlusTid) :-
  t_wc(wc(Cat1, _OnderdeeelID,_Orde,  _Naam, WTyp, _Kort, SexList1, SpSt1, leeftijd(_,Londer1), leeftijd(_,Lboven1), _HDG, _Schemasoort, _Keuring, _KNLTB, _Geld)),
  t_wc(wc(Cat2, _,_, Onderdeel2, WTyp, _, SexList2, SpStX, leeftijd(_,LonderX), leeftijd(_,LbovenX), _, _, _, Tid, _)),
  Cat1 <> Cat2,
  SpSt1 = SpStX,
  Londer1 = LonderX,
  matchbovenleeftijd(Lboven1,LbovenX),
  compsexes(SexList1, SexList2),
  format(OnderdeelPlusTid, "%s\t  %s", Onderdeel2, Tid).

setwedsex(Win, Selected) :-
  SelW = win_GetCtlHandle(Win, Selected),
  GWin = win_GetCtlHandle(Win, idc_gemengd),  
  MWin = win_GetCtlHandle(Win, idc_heren),  
  VWin = win_GetCtlHandle(Win, idc_dames),
  Nwin = win_GetCtlHandle(Win, idc_nvt),  
  win_CheckRadioButton(SelW, [GWin, MWin, VWin, NWin]).
  
ontkoppelCat(Catx) :-
  t_wc(wc(Catx, OnderdeelId, _,  Naam, _,_Kort, _, _, _, _, _, _, _, _, _)),
  not(OnderdeelId = ""),
  format(Msg, "Onderdeel %s\nloskoppelen van ServIt??\nN.B. Dan kun je eventueel vervolgens de definities aanpassen en nog een keer de juiste match proberen te vinden.", Naam),
  1 = dlg_MessageBox( "Loskoppelen", Msg, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
  retract(t_wc(wc(Catx, OnderdeelId, Orde,  Naam, WTyp,_Kort, SexList1, SpSt1, Londer, Lboven, HDG, Schemasoort, _, _, Geld))), !,
  assertz(t_wc(wc(Catx, "", Orde,  Naam, WTyp,_Kort, SexList1, SpSt1, Londer, Lboven, HDG, Schemasoort, vanzelf, "", Geld))),
  !.
ontkoppelCat(_Catx).


isCatLLeeg([], _) :-
  findall(Sx,spsrt(Sx,_,_,_,_,_,_,_),SxList),
  count( SXList, 0, Aantal),
  Aantal > 2,
  Msg = "Geen match met een KNLTB-onderdeel gevonden!\nU hebt meer dan twee spelersoorten!\nZie Personen, Kenmerken.",
  _Answer = dlg_MessageBox( "Geen match", Msg, mesbox_iconinformation, mesbox_buttonsokcancel, mesbox_defaultfirst, mesbox_suspendapplication ), !, fail.
isCatLLeeg([], Catx) :-
  concat("geen overeenkomend KNLTB-onderdeel gevonden.\nNB in sommige gevallen moet je eerst ook speelsterkte 1 of 2 overeenkomend maken. Of vraag wedstrijdtennis@knltb.nl om een definitie bij te maken.",
  "\n\nDoe voor de zekerheid Toernooinummer/certificaatbeheer > Update definities.", Msg),
  _Answer = dlg_MessageBox( "Geen match", Msg, mesbox_iconinformation, mesbox_buttonsokcancel, mesbox_defaultfirst, mesbox_suspendapplication ), !, 
  ontkoppelCat(Catx), fail.
isCatLLeeg(_, _).
    
checkResp(Titel, CatL, StrSel) :-
  _Resp = dlg_ListSelect(Titel,CatL,-1,StrSel,_Index),
  fronttoken(StrSel, _, _),  !.
checkResp(Titel, _, _) :-
  dlg_MessageBox( Titel, "U hebt geen ServIt onderdeel geselecteerd!", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.
  
  


%  t_wc(wc(Catx, _OnderdeelID, _Orde, Lang, EDG, _Kort, SexL, _Sterkte, leeftijd(_LeeftVanStrx,LeeftVan), leeftijd(_LeeftTotStrx,LeeftTotM), HDG, _Schemasoort, _Status, Tid, _Geld)),
%  t_wc(wc(Catsel, OnderdeelIDSel, _OrdeSel, NaamSel, EDGsel, _Kortsel, SexLsel, Sterktesel, leeftijd(_LeeftVanStrsel,LeeftVansel), 
%	  leeftijd(_LeeftTotStrsel,LeeftTotMsel), _HDGsel, _Schemasoortsel, _Statussel, TidSel, GeldSel)),


  dlg_onderdeelkenmerken_Create(_Parent):-
	kenmerkWin(_Win), !.
  dlg_onderdeelkenmerken_Create(Parent):-
	assert(refresh(b_True)),
	checkPaswoord(Parent),
	%alsOnline(),
	assert(noRadio(b_False)),
	cleanRelaties(),
	copyOnd,
	win_CreateResDialog(Parent,dlg_onderdeelkenmerken_DlgType,dlg_onderdeelkenmerken_ResID,dlg_onderdeelkenmerken_eh,0).

%BEGIN Onderdeelkenmerken, idc_ok _CtlInfo
  dlg_onderdeelkenmerken_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	LWin = win_GetCtlHandle(_Win, idc_custom),
	cTreeView::get_selected(LWin, CatNr),
	t_vink(Cat, CatNr),
	%behandelLege(Cat),
	not(isGelijkOndKG(_Win)), !,
        behandelLege(Cat),
	cursor_SetWait(),
	maakgelijkOndK(_Win),
	win_Destroy(_Win), !.
dlg_onderdeelkenmerken_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	win_Destroy(_Win),
	!.
%END Onderdeelkenmerken, idc_ok _CtlInfo
%MARK Onderdeelkenmerken, new events

%BEGIN Onderdeelkenmerken, idc_nvt _CtlInfo
  dlg_onderdeelkenmerken_eh(_Win,e_Control(idc_nvt,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	noRadio(b_False),
	zijnErOpgaven(_Win),
	setwedsex(_Win, idc_nvt),
	findall(Num, relevantesoort2( _, Num),SrtList),
	SWin = win_GetCtlHandle(_Win, idcl_spelersoorten),  
	lbox_Clear(SWin),
	lbox_Add(SWin, SrtList),
	checkRelevanteSoorten(SWin, m, SrtList, 0),
	checkRelevanteSoorten(SWin, v, SrtList, 0),
	!.
%END Onderdeelkenmerken, idc_nvt _CtlInfo

%BEGIN Onderdeelkenmerken, idc_groepeer _CtlInfo
  dlg_onderdeelkenmerken_eh(_Win,e_Control(idc_groepeer,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idc_custom),
	cTreeView::get_Selected(LboxWin, Cat),
	t_vink(Cat1, Cat), 
	t_wc(kopl(Cat1, algemeen, Cat2, algemeen)), !,
	t_wc(wc(Cat2, _, _, ER, _, _, _, _, _, _, _, _, _, _,_)),
	format(Msg, "Algemene koppeling met/naar %s.\nKoppeling verbreken?", ER),
	A = dlg_MessageBox( "Bevestig", Msg, mesbox_iconquestion, mesbox_buttonsokcancel, mesbox_defaultsecond, mesbox_suspendapplication ),
	A = 1,
	retract(t_wc(kopl(Cat1, algemeen, Cat2, algemeen))),
	deleteKnopen(LboxWin), !,
	findall(Knoopx, knoopT(_, Knoopx), Knopen),
	cTreeView::insert_tree_list(LboxWin, 0, first(), Knopen).
  dlg_onderdeelkenmerken_eh(_Win,e_Control(idc_groepeer,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idc_custom),
	cTreeView::get_Selected(LboxWin, Cat1Nr),
	t_vink(Cat1, Cat1Nr),
	not(kopl(Cat1, algemeen, _, _)),
	findall(Cat2, teVerbindenT(Cat1, Cat2), CatL),
	CatL = [],
	dlg_note("Test", "geen kandidaten!"), !.
  dlg_onderdeelkenmerken_eh(_Win,e_Control(idc_groepeer,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idc_custom),
	cTreeView::get_Selected(LboxWin, Cat1Nr),
	write("\n", Cat1Nr),
	t_vink(Cat1, Cat1Nr),
	not(t_wc(kopl(Cat1, algemeen, _, _))),
	findall(Cat2, teVerbindenT(Cat1, Cat2), CatL),
	b_True = dlg_ListSelect("Koppelen naar",CatL,0,CatSel,_Index),
	parsetabs(CatSel,Items),
	Items = [CatSel1|_],
	t_wc(wc(Cat1, _, _, ER, _, _, _, _, _, _, _, _, _, _,_)),
	format(Msg, "Koppeling aanleggen van \n%s\nnaar\n%s?\n(NB: %s moet eind- of verzamelronde zijn!)", ER, CatSel, CatSel),
	b_True = dlg_MessageBox( "Algemene koppeling", Msg, mesbox_iconquestion, mesbox_buttonsokcancel, mesbox_defaultsecond, mesbox_suspendapplication ),
	t_wc(wc(CatNaar, _, _, CatSel1, _, _, _, _, _, _, _, _, _, _,_)),
	handleVerKoppeling(Cat1,CatNaar),
	assertz(t_wc(kopl(Cat1, algemeen, CatNaar, algemeen))),
	deleteKnopen(LboxWin),
	findall(Knoopx, knoopT(_, Knoopx), Knopen),
	cTreeView::insert_tree_list(LboxWin, 0, first(), Knopen), !.
%END Onderdeelkenmerken, idc_groepeer _CtlInfo

%BEGIN Onderdeelkenmerken, idc_verwissel _CtlInfo
  dlg_onderdeelkenmerken_eh(_Win,e_Control(idc_verwissel,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LWin = win_GetCtlHandle(_Win, idc_custom),
	cTreeView::get_selected(LWin, Cat),
        behandelLege(Cat),
	cursor_SetWait(),
	getOnd(Cat),
	t_vink(Catx, Cat),
	t_wc(wc(Catx, _, _, Lang, _, _Kort, _SexL, _Sterkte, _Lft, _LftN, _HDG, _Schemasoort, _Status, _Tid, _Geld)),
	findall(KenStr, soortgelijkeCat(Catx,KenStr,_),CatIL),
	isCatLLeeg(CatIL, Catx),
	Msg = "Je kunt verhuizen naar een door de KNLTB voorgeprogrammeerde definitie en dus koppelen aan ServIt.\nStart met de niet-KNLTB definitie\nDoorgaan?",
	1 = dlg_MessageBox( "Verwissel onderdeeldefinitie", Msg, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
	checkResp("Verwissel met", CatIL,StrSel),
	getbacktrack(Here),
	soortgelijkeCat(Catx,Tekst,CatSel),
	Tekst = StrSel,
	verhuisWC(Catx, CatSel),
	cutbackTrack(Here),
	verwisselPoint(LWin, CatSel, Catx, PointS),
	verwisselPoint(LWin, CatX, CatSel, PointX),
	cTreeView::get_part(LWin, Catx, Knopenx),
	cTreeView::get_parent(LWin, CatSel, ParentS),
	cTreeView::get_parent(LWin, Catx, ParentX),
	cTreeView::get_part(LWin, CatSel, Knopens),
	assert(refresh(b_False)),
	%dlg_note("1"),
	cTreeView::delete_item(LWin, Catx),
	Knopenx = [ItemTreex|_],
	ItemTreex = tree(CatNoNrx, _, _BMapx, _, Naam1x, AfhankelijkenX),
	t_vink(CatNoRealx, CatNoNrx),
	rsoort2bitmapT(CatNoRealx, BMapx1),
	ItemTreex1 = tree(CatNoNrx, [], BMapx1, BmapX1, Naam1x, AfhankelijkenX),
	cTreeView::delete_item(LWin, CatSel),
	Knopens = [ItemTrees|_],
	ItemTrees = tree(CatNoNrs, _, _BMaps, _, Naam1s, AfhankelijkenS),
	t_vink(CatNoRealS, CatNoNrS),
	rsoort2bitmapT(CatNoRealS, BMapS1),
	ItemTreeS1 = tree(CatNoNrs, [], BMapS1, BmapS1, Naam1S, AfhankelijkenS),
	cTreeView::insert_tree(LWin, ParentS, PointS, ItemTreex1),
	cTreeView::insert_tree(LWin, ParentX, PointX, ItemTrees1),
	assert(refresh(b_True)),
	format(Msg1, "%s\n is gekoppeld aan ServIt!", Lang),
	_K = dlg_MessageBox( "Verwissel onderdeeldefinitie", Msg1, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	!.
%END Onderdeelkenmerken, idc_verwissel _CtlInfo

%BEGIN Onderdeelkenmerken, e_Update
  dlg_onderdeelkenmerken_eh(_Win,e_Update(_UpdateRct),0):-!,
	Font = win_GetFont(_Win),
	_FontName = font_GetAttrs(Font, Style, Size),
	Siz1 = Size - 1,
	NewFont = font_SetAttrs(Font, Style, Siz1),
	LangWin = win_GetCtlHandle(_Win, idct_voorbeeld_de_5_35_lang),
	KortWin = win_GetCtlHandle(_Win, idct_voorbeeld_5de35_kort),
	win_SetFont(KortWin, NewFont),
	win_setText(KortWin, "Voorbeeld: 5DE35+ of 1HEkwB\nkort en géén spaties svp"),
	win_SetFont(LangWin, NewFont),
	!.
%END Onderdeelkenmerken, e_Update

%BEGIN Onderdeelkenmerken, idc_leefttm modified
  dlg_onderdeelkenmerken_eh(_Win,e_Control(idc_leefttm,_CtrlType,CtrlWin,modified),0):-
	Text = win_GetText(CtrlWin),
	%fronttoken(Text, Text1, _),
	metTekst(Text),
	dlg_MessageBox( "Title", "Leeftijd t/m moet een getal zijn <= 18 of blanco.", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	!.
  dlg_onderdeelkenmerken_eh(_Win,e_Control(idc_leefttm,_CtrlType,CtrlWin,modified),0):-
	Text = win_GetText(CtrlWin),
	fronttoken(Text, Text1, _),
	str_len(Text1, Len),
	Len > 2,
	beep(),
	frontstr(2,Text1,StartString,_),
	win_SetText(CtrlWin, StartString),
	!.
%END Onderdeelkenmerken, idc_leefttm modified

%BEGIN Onderdeelkenmerken, idc_leeftvan modified
  dlg_onderdeelkenmerken_eh(_Win,e_Control(idc_leeftvan,_CtrlType,CtrlWin,modified),0):-
	Text = win_GetText(CtrlWin),
	metTekst(Text),
	%not(str_int(Text1, _)),
	dlg_MessageBox( "Title", "Leeftijd vanaf moet een getal zijn of blanco.", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	!.
  dlg_onderdeelkenmerken_eh(_Win,e_Control(idc_leeftvan,_CtrlType,CtrlWin,modified),0):-
	Text = win_GetText(CtrlWin),
	fronttoken(Text, Text1, _),
	str_len(Text1, Len),
	Len > 2,
	beep(),
	frontstr(2,Text1,StartString,_),
	win_SetText(CtrlWin, StartString),
	!.
%END Onderdeelkenmerken, idc_leeftvan modified

%BEGIN Onderdeelkenmerken, e_CloseRequest
  dlg_onderdeelkenmerken_eh(_Win,e_CloseRequest,0):-!,
	OnderdelenWin = win_GetCtlHandle(_Win, idc_custom),
	cTreeView::get_selected(OnderdelenWin, CatSelected),
	getond(CatSelected),
	not(isGelijkOndKG(_Win)),
	1 = dlg_MessageBox( "Onderdelentabel", "Tabel is veranderd.\nBewaren?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
	trap(CW = win_GetCtlHandle(_Win, idc_ok),_,fail),
	win_SendEvent(_Win,e_Control(idc_ok,wc_PushButton,CW, activated())),
	!.
%END Onderdeelkenmerken, e_CloseRequest

%BEGIN Onderdeelkenmerken, idc_help _CtlInfo
  dlg_onderdeelkenmerken_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Onderdeeleigenschappen"),
	!.
%END Onderdeelkenmerken, idc_help _CtlInfo

%BEGIN Onderdeelkenmerken, idc_kortenaam modified
  dlg_onderdeelkenmerken_eh(_Win,e_Control(idc_kortenaam,_CtrlType,KWin,modified),0):-
	NWin = win_GetCtlHandle(_Win, idc_onderdeelnaam),
	LWin = win_GetCtlHandle(_Win, idc_custom),
	cTreeView::get_selected(LWin, Cat),
	cTreeView::get_item(LWin, Cat, _Text, State, Bitmap1, Bitmap2),
	Naam = win_GetText(NWin),
	Kort = win_GetText(KWin),
	format(Tekst, "%s * %s", Kort, Naam),
	cTreeView::set_item(LWin, Cat, Tekst, State, Bitmap1, Bitmap2),
	!.
%END Onderdeelkenmerken, idc_kortenaam modified

%BEGIN Onderdeelkenmerken, idc_onderdeelnaam modified
  dlg_onderdeelkenmerken_eh(_Win,e_Control(idc_onderdeelnaam,_CtrlType,NWin,modified),0):-
	KWin = win_GetCtlHandle(_Win, idc_kortenaam),
	LWin = win_GetCtlHandle(_Win, idc_custom),
	cTreeView::get_selected(LWin, Cat),
	cTreeView::get_item(LWin, Cat, _Text, State, Bitmap1, Bitmap2),
	Naam = win_GetText(NWin),
	Kort = win_GetText(KWin),
	format(Tekst, "%s * %s", Kort, Naam),
	cTreeView::set_item(LWin, Cat, Tekst, State, Bitmap1, Bitmap2),
	!.
%END Onderdeelkenmerken, idc_onderdeelnaam modified

%BEGIN Onderdeelkenmerken, e_User
  dlg_onderdeelkenmerken_eh(_Win,e_User(c_setfocus, Control),0):-
	KWin = win_GetCtlHandle(_Win, Control),
	trap(win_SetFocus(KWin),_,fail),
	!.
  dlg_onderdeelkenmerken_eh(_Win,e_User(setenkeldubbel,Control),0):-
	EWin = win_GetCtlHandle(_Win, idc_enkelspel),
	DWin = win_GetCtlHandle(_Win, idc_dubbel),
	CWin = win_GetCtlHandle(_Win, Control),
	win_CheckRadioButton(CWin, [EWin, DWin]),
	win_SetFocus(CWin),
	!.
  dlg_onderdeelkenmerken_eh(_Win,e_User(setmanvrgemengd,Control),0):-
	DWin = win_GetCtlHandle(_Win, idc_dames),
	HWin = win_GetCtlHandle(_Win, idc_heren),
	GWin = win_GetCtlHandle(_Win, idc_gemengd),
	NWin = win_GetCtlHandle(_Win, idc_nvt),
	CWin = win_GetCtlHandle(_Win, Control),
	win_CheckRadioButton(CWin, [DWin, HWin, GWin,NWin]),
	win_SetFocus(CWin),
	!.
  dlg_onderdeelkenmerken_eh(_Win,e_User(uitgesteld,_Ptr),0):-
        %treeond(_Win, Onx),
        %%term_str(catl, Onx, Stx),
        %dlg_note(Stx),
	OnderdelenWin = win_GetCtlHandle(_Win, idc_custom),
	t_wc(wc(Cat, _, _, _, _, _, _, _, _, _, _, _, _, _, _)),
	%t_wc(Cat,_Naam,_,_,_,_,_,_,_,_),
	%write("\n", Cat, " ", _Naam),
	%not(onderCat(Cat)),
	win_SetFocus(_Win),
	%cTreeView::select(OnderdelenWin, Cat),
	trap(cTreeView::select(OnderdelenWin, Cat),_, true),
	win_SetFocus(OnderdelenWin),
	!.
  dlg_onderdeelkenmerken_eh(_Win,e_User(uitgesteld,_Ptr),0):-
	trap(CW = win_GetCtlHandle(_Win, idc_bij),_,fail),
	win_SendEvent(_Win,e_Control(idc_bij,wc_PushButton,CW, activated())), !.

  dlg_onderdeelkenmerken_eh(_Win,e_User(ereenbij,_Ptr),0):-
	OnderdelenWin = win_GetCtlHandle(_Win, idc_custom),
	cTreeView::get_selected(OnderdelenWin, Itemid),
	getbacktrack(Here),
	numb(N),
	not(t_wc(wc(N, _, _, _, _, _, _, _, _, _, _, _, _, _, _))),
	cutbacktrack(Here),
	retractall(waarschuwen),
	assert(waarschuwen),
	assert(t_wc(wc(N, "onderdeelid", 0, "naam", e, "kort", [], "", leeftijd("",0), leeftijd("",99), gemengd, "", vanzelf, "", 0.0))),
	getbacktrack(Here1),
	numb(N1),
	not(t_vink(_,N1)),
	cutbacktrack(Here1),
	assert(t_vink(N, N1)),
	cTreeView::insert_item(OnderdelenWin, N1, 0, after(ItemId), "", [bold()], resid(idb_geenindeling), resId(idb_geenindeling)),
	cTreeView::select(OnderdelenWin, N1),
	NWin = win_GetCtlHandle(_Win, idc_kortenaam),
	win_SetFocus(NWin),
	!.

  dlg_onderdeelkenmerken_eh(_Win,e_User(c_ereenaf,_Ptr),0):-
	OnderdelenWin = win_GetCtlHandle(_Win, idc_custom),
	cTreeView::get_selected(OnderdelenWin, CatSelectedNr),
	t_vink(CatSelected, CatSelectedNr),
	t_wc(wc(CatSelected, OId, _, _, _, _, _, _, _, _, _, _, vanknltb, _, _)),
	fronttoken(OId,_,_), !.
  dlg_onderdeelkenmerken_eh(_Win,e_User(c_ereenaf,_Ptr),0):-
	OnderdelenWin = win_GetCtlHandle(_Win, idc_custom),
	cTreeView::get_selected(OnderdelenWin, CatSelectedNr),
	t_vink(CatSelected, CatSelectedNr),
	%not(t_wc(wc(CatSelected, "onderdeelid", 0, "naam", e, "kort", [], "", leeftijd("",0), leeftijd("",99), gemengd, "", vanzelf, "", 0.0))),
	getond(CatSelected),
	opgavenVoor(OnderdelenWin, CatSelectedNr, Opgaven, Planning),
	count(Opgaven, 0, AantalOpgaven),
	count(Planning, 0, GeplandeWedstrijden),
	not(Opgaven = []),
	format(Msg, "Er staan nog %u personen/koppels opgegeven voor dit onderdeel\nEr staan %u wedstrijden gepland.\nWis eerst de indeling resp. opgaven!", AantalOpgaven, GeplandeWedstrijden),
	dlg_MessageBox( "Onderdeel", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultsecond, mesbox_suspendapplication ),
	win_SetFocus(_Win),
	!.
  dlg_onderdeelkenmerken_eh(_Win,e_User(c_ereenaf,_Ptr),0):-
	OnderdelenWin = win_GetCtlHandle(_Win, idc_custom),
	cTreeView::get_selected(OnderdelenWin, CatSelected),
	cTreeView::get_child(OnderdelenWin, CatSelected, Child),	% eerste child
	tree2list(OnderdelenWin, Child, [CatSelected], CatList),	% alles wat daaronder en aanhangt
	assert(refresh(b_False)),
	cTreeView::delete_item(OnderdelenWin, CatSelected), 		% delete
	retract(t_vink(Cat, CatSelected)),
	deleteAllCatSelectedNodes(OnderdelenWin, Cat),
	assert(refresh(b_True)),
	member(Cat, CatList),
	%retract(t_wc(Cat,_,_,_,_,_,_,_,_,_)),
	retract(t_wc(wc(Cat, _, _, _, _, _, _, _, _, _, _, _, _, _, _))),
	%retract(t_wc(wc(Ond, OnderdeeelID, _Orde, Naam, EDG, Kort, SexL, SpStrk, LftV, LftTM, HDG, Schemasoort, Status, KNLTB, Geld))),
	%assert(t_wc(wc(Ond, OnderdeeelID, _Orde, Naam, EDG, Kort, SexL, SpStrk, LftV, LftTM, HDG, Schemasoort, Status, KNLTB, Geld))),
	%retract(t_wc(Ond,Naam,EDG,Kort,VoorWie,Geld,Cat,SpStrk, LftVan, LftTot)),
	%assert(t_wc(Ond,Naam,EDG,Kort,VoorWie,Geld,0,SpStrk, LftVan, LftTot)),
	fail.
  dlg_onderdeelkenmerken_eh(_Win,e_User(c_ereenaf,_Ptr),0):-	% voeg er een toe als de lijst leeg wordt
	OnderdelenWin = win_GetCtlHandle(_Win, idc_custom),
	cTreeView::get_count(OnderdelenWin, Count),
	Count = 0,
	trap(CW = win_GetCtlHandle(_Win, idc_bij),_,fail),
	win_SendEvent(_Win,e_Control(idc_bij,wc_PushButton,CW, activated())),
	win_SetFocus(_Win), !.
  dlg_onderdeelkenmerken_eh(_Win,e_User(c_ereenaf,_Ptr),0):-
	win_SetFocus(_Win), !.
%END Onderdeelkenmerken, e_User

%BEGIN Onderdeelkenmerken, idc_neer _CtlInfo
  dlg_onderdeelkenmerken_eh(_Win,e_Control(idc_neer,_CtrlType,_CtrlWin,_CtlInfo),0):-
	OnderdelenWin = win_GetCtlHandle(_Win, idc_custom),
	cTreeView::get_selected(OnderdelenWin, CatSelected),
	getOnd(CatSelected),
	cTreeView::get_next(OnderdelenWin, CatSelected, Next),
	Next <> 0,
	cTreeView::get_parent(OnderdelenWin, CatSelected, Parent),
	cTreeView::get_part(OnderdelenWin, CatSelected, Knopen),
	assert(refresh(b_False)),
	cTreeView::delete_item(OnderdelenWin, CatSelected),
	%findall(Knoopx, knoopT(CatSelected, Knoopx), Knopen),
	Knopen = [ItemTree|_],
	cTreeView::insert_tree(OnderdelenWin, Parent, after(Next), ItemTree),
	assert(refresh(b_True)),
	cTreeView::select(OnderdelenWin, CatSelected),
	!.
%END Onderdeelkenmerken, idc_neer _CtlInfo

%BEGIN Onderdeelkenmerken, idc_op _CtlInfo
  dlg_onderdeelkenmerken_eh(_Win,e_Control(idc_op,_CtrlType,_CtrlWin,_CtlInfo),0):-
	OnderdelenWin = win_GetCtlHandle(_Win, idc_custom),
	cTreeView::get_selected(OnderdelenWin, CatSelected),
	getOnd(CatSelected),
	insertPoint(OnderdelenWin, CatSelected, Point),
	cTreeView::get_parent(OnderdelenWin, CatSelected, Parent),
	cTreeView::get_part(OnderdelenWin, CatSelected, Knopen),
	assert(refresh(b_False)),
	cTreeView::delete_item(OnderdelenWin, CatSelected),
	%findall(Knoopx, knoopT(CatSelected, Knoopx), Knopen),
	Knopen = [ItemTree|_],
	cTreeView::insert_tree(OnderdelenWin, Parent, Point, ItemTree),
	assert(refresh(b_True)),
	cTreeView::select(OnderdelenWin, CatSelected),
	!.
%END Onderdeelkenmerken, idc_op _CtlInfo

%BEGIN Onderdeelkenmerken, idc_af _CtlInfo
  dlg_onderdeelkenmerken_eh(_Win,e_Control(idc_af,_CtrlType,_CtrlWin,_CtlInfo),0):-
	win_PostEvent(_Win,e_User(c_ereenaf, 0)), !.
%END Onderdeelkenmerken, idc_af _CtlInfo

%BEGIN Onderdeelkenmerken, idc_bij _CtlInfo
  dlg_onderdeelkenmerken_eh(_Win,e_Control(idc_bij,_CtrlType,_CtrlWin,_CtlInfo),0):-
	win_PostEvent(_Win,e_User(ereenbij, 0)), !.
%END Onderdeelkenmerken, idc_bij _CtlInfo

%BEGIN Onderdeelkenmerken, e_OwnerDraw
  dlg_onderdeelkenmerken_eh(_Win,e_OwnerDraw(od_Lbox,idcl_spelersoorten,_ItemId,_ItemAction,
		ItemState,LboxWin,RectItem,_ItemData),0):-
	own_member(ItemState, odstate_Selected), !,  % was selected
	RectItem = rct(L, T, R, B),
	win_SetPen(LboxWin,pen(1,ps_Solid,color_Blue)),
	win_SetForeColor(LboxWin, color_White),
	win_SetBackColor(LboxWin, color_Blue),
	win_SetBrush(LboxWin,brush(pat_Solid, color_Blue)),
	draw_Rect(LboxWin,RectItem),
	Item = lbox_GetItem(LboxWin, _ItemId),	% get the string
	str_int(Item, Srt),
	spsrt(Srt, Hx, _MV, _LftVan, _LftTot, _StrkVan, _StrkTot, _Reserve), 
	win_GetTextExtent(LboxWin, "x ", -1, Width, _Height),
	R1 = L + Width,
	RCT1 = rct(L, T, R1, B),
	draw_TextInRect(LboxWin,RCT1, "x ", -1, [dtext_Left]),
	RCT2 = rct(R1, T, R, B),
	draw_TextInRect(LboxWin,RCT2, Hx, -1, [dtext_Left]),
	!.
  dlg_onderdeelkenmerken_eh(_Win,e_OwnerDraw(od_Lbox,idcl_spelersoorten,_ItemId,_ItemAction,
		ItemState,LboxWin,RectItem,_ItemData),0):-
	RectItem = rct(L, T, R, B),
	Itemstate = [],
	win_SetPen(LboxWin,pen(1,ps_Solid,color_White)),
	draw_Rect(LboxWin,RectItem),
	win_SetForeColor(LboxWin, color_Black),
	win_SetBackColor(LboxWin, color_White),
	Item = lbox_GetItem(LboxWin, _ItemId),	% get the string
	str_int(Item, Srt),
	spsrt(Srt, Hx, _MV, _LftVan, _LftTot, _StrkVan, _StrkTot, _Reserve), 
	win_GetTextExtent(LboxWin, "x ", -1, Width, _Height),
	%NH = B - T,
	R1 = L + Width,
	RCT1 = rct(L, T, R1, B),
	draw_TextInRect(LboxWin,RCT1, "  ", -1, [dtext_Left]),
	RCT2 = rct(R1, T, R, B),
	draw_TextInRect(LboxWin,RCT2, Hx, -1, [dtext_Left]),
	!.
%END Onderdeelkenmerken, e_OwnerDraw

%BEGIN Onderdeelkenmerken, e_OwnerMeasureItem
  dlg_onderdeelkenmerken_eh(_Win,e_OwnerMeasureItem(od_Lbox,idcl_spelersoorten,_ItemId,_),Size):-
	win_GetTextExtent(_Win, "JansenenTilanusg", -1, Width, He),
	Height = He ,
	bitleft(Height, 16, H1),
	Size = Width + H1,
	!.
%END Onderdeelkenmerken, e_OwnerMeasureItem

%BEGIN Onderdeelkenmerken, idc_cancel _CtlInfo
  dlg_onderdeelkenmerken_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-
	OnderdelenWin = win_GetCtlHandle(_Win, idc_custom),
	cTreeView::get_selected(OnderdelenWin, CatSelected),
	getond(CatSelected),
	not(isGelijkOndKG(_Win)),
	1 = dlg_MessageBox( "Onderdelentabel", "Tabel is veranderd.\nBewaren?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
	trap(CW = win_GetCtlHandle(_Win, idc_ok),_,fail),
	win_SendEvent(_Win,e_Control(idc_ok,wc_PushButton,CW, activated())), !.
  dlg_onderdeelkenmerken_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0) :-
       win_destroy(_Win), !.
%END Onderdeelkenmerken, idc_cancel _CtlInfo

%BEGIN Onderdeelkenmerken, idc_gemengd _CtlInfo
  dlg_onderdeelkenmerken_eh(_Win,e_Control(idc_gemengd,_CtrlType,_CtrlWin,activated()),0):-  !,
	noRadio(b_False),
	zijnErOpgaven(_Win),
	setwedsex(_Win, idc_gemengd),
	findall(Num, relevantesoort2( _, Num),SrtList),
	SWin = win_GetCtlHandle(_Win, idcl_spelersoorten),  
	lbox_Clear(SWin),
	lbox_Add(SWin, SrtList),
	checkRelevanteSoorten(SWin, m, SrtList, 0),
	checkRelevanteSoorten(SWin, v, SrtList, 0),
	!.
%END Onderdeelkenmerken, idc_gemengd _CtlInfo

%BEGIN Onderdeelkenmerken, idc_heren _CtlInfo
  dlg_onderdeelkenmerken_eh(_Win,e_Control(idc_heren,_CtrlType,_CtrlWin,activated()),0):- !,
	noRadio(b_False),
	zijnErOpgaven(_Win),
	setwedsex(_Win, idc_heren),
	findall(Num, relevantesoort2( m, Num),SrtList),
	SWin = win_GetCtlHandle(_Win, idcl_spelersoorten),  
	lbox_Clear(SWin),
	lbox_Add(SWin, SrtList),
	checkRelevanteSoorten(SWin, m, SrtList, 0),
	!.
%END Onderdeelkenmerken, idc_heren _CtlInfo

%BEGIN Onderdeelkenmerken, idc_dames _CtlInfo
  dlg_onderdeelkenmerken_eh(_Win,e_Control(idc_dames,_CtrlType,_CtrlWin,activated()),0):- 
	noRadio(b_False),
	zijnErOpgaven(_Win),
	setwedsex(_Win, idc_dames),
	findall(Num, relevantesoort2( v, Num),SrtList),
	SWin = win_GetCtlHandle(_Win, idcl_spelersoorten),  
	lbox_Clear(SWin),
	lbox_Add(SWin, SrtList),
	checkRelevanteSoorten(SWin, v, SrtList, 0) ,
	!.
%END Onderdeelkenmerken, idc_dames _CtlInfo

%BEGIN Onderdeelkenmerken, idc_dubbel _CtlInfo
  dlg_onderdeelkenmerken_eh(_Win,e_Control(idc_dubbel,_CtrlType,_CtrlWin,_CtlInfo),0):-
	noRadio(b_False),
	zijnErOpgaven(_Win),
	EWin = win_GetCtlHandle(_Win, idc_enkelspel),  
	DWin = win_GetCtlHandle(_Win, idc_dubbel),  
	win_CheckRadioButton(DWin, [EWin, DWin]), 
	!.
%END Onderdeelkenmerken, idc_dubbel _CtlInfo

%BEGIN Onderdeelkenmerken, idc_enkelspel _CtlInfo
  dlg_onderdeelkenmerken_eh(_Win,e_Control(idc_enkelspel,_CtrlType,_CtrlWin,_CtlInfo),0):-
	noRadio(b_False),
	zijnErOpgaven(_Win),
	EWin = win_GetCtlHandle(_Win, idc_enkelspel),  
	DWin = win_GetCtlHandle(_Win, idc_dubbel),  
	win_CheckRadioButton(EWin, [EWin, DWin]), 
	!.
%END Onderdeelkenmerken, idc_enkelspel _CtlInfo

%BEGIN Onderdeelkenmerken, e_Destroy
  dlg_onderdeelkenmerken_eh(_Win,e_Destroy,0):-
	updateVensterpositie(kenmerkvenster, _Win),
	retractall(kenmerkwin(_)),
	retractall(waarschuwen),
	retractall(t_wc( _)),
	retractall(t_vink(_,_)), 
	%retractall(_, kenm),
	!.
%END Onderdeelkenmerken, e_Destroy

%BEGIN Onderdeelkenmerken, e_Create
  dlg_onderdeelkenmerken_eh(_Win,e_Create(_CreationData),0):-!,
	dialoogpositie(kenmerkvenster, _Win),
	TaskWin = vpi_GetTaskWin(),
	TaskRct = win_GetOuterRect(TaskWin),
	RCT = win_GetOuterRect(_Win),
	WsFlags = win_GetState(_Win),
	ClRCT = rect_GetClient(WsFlags, b_False, RCT),
	TaskRct = rct(_, _, Xt, _),
	ClRCT = rct(_, _, Xc, _),
	OffsX = Xt - Xc - 4,
	NewClient = rect_Offset(ClRCT, OffsX, 0), 
	win_Move(_Win, NewClient), 
	assert(kenmerkWin(_Win)),
	OndWin = win_GetCtlHandle(_Win, idc_custom),
	cTreeView::init(OndWin, [tvs_hasbuttons, tvs_haslines, tvs_linesatroot, tvs_showselalways],resid(idb_teesize),callbackO),
	findall(Knoopx, knoopT(_, Knoopx), Knopen),
	cTreeView::insert_tree_list(OndWin, 0, first(), Knopen),
	SWin = win_GetCtlHandle(_Win, idc_spst),
	lbox_Add(SWin, ["","1","2","3","4","5","6","7","8","9"]),
	lbox_SetSel(SWin, 1, b_True),
	win_PostEvent(_Win, e_User(uitgesteld, 0)),
	!.
%END Onderdeelkenmerken, e_Create

  dlg_onderdeelkenmerken_eh(_,_,_):-!,fail.

%END_DLG Onderdeelkenmerken
%BEGIN_TLB SpelerselectB, 23:22:41-15.11.2010, Code automatically updated!
/**************************************************************************
	Creation of toolbar: SpelerselectB
**************************************************************************/

clauses

  tb_spelerselectb_Create(_Parent):-
ifdef use_tbar
	toolbar_create(tb_bottom,0xC0C0C0,_Parent,
		[separator,
		 separator,
		 tb_text(idt_s1,tb_static,176,0,4,10,0x0,"Totaal ?? personen in lijst")]),
enddef
	true.
%END_TLB SpelerselectB



%BEGIN_DLG DagUurKwart
/**************************************************************************
	Creation and event handling for dialog: DagUurKwart
**************************************************************************/

constants

%BEGIN DagUurKwart, CreateParms, 12:52:47-5.10.2015, Code automatically updated!
  dlg_daguurkwart_DlgType = wd_Modal
  dlg_daguurkwart_Title = "Invoer van baanbeschikbaarheid"
  dlg_daguurkwart_RCT = rct(40,32,158,180)
  dlg_daguurkwart_Flags = [wsf_Close,wsf_TitleBar]
  dlg_daguurkwart_Help = idh_baanbeschikbaarheid
  dlg_daguurkwart_Font = "Arial"
  dlg_daguurkwart_FSize = 10
%END DagUurKwart, CreateParms

predicates

  dlg_daguurkwart_eh : EHANDLER
  dagMetTab(string MetBlank, string MetTab)
  nondeterm dagRepr(integer, string)
  dagIndex1(integer, integer) - (i, o)
  dagIndex1(integer, integer, integerlist, integer) 
  staatOp(WINDOW, tijd)
  vulAantal(WINDOW, tijd)
  behandel(WINDOW, tijd, string)
%procedure  kwartierinterval(slist)

clauses

dagIndex1(D, N) :-
  findall(D1, dag(D1, _, _), D1en),
  dagIndex1(D, 0, D1en, N).
  
dagIndex1(D, N, [D|_], N) :- !.
dagIndex1(D, N, [_|T], Out) :-
  N1 = N + 1,
  dagIndex1(D, N1, T, Out).

dagMetTab(In, Uit) :-
  bound(In),
  searchchar(In, ' ', Pos),
  Pos1 = Pos - 1, !,
  frontstr(Pos1, In, FStr, _),
  frontstr(Pos, In, _, BStr),
  format(Uit, "%s\t%s", FStr, BStr).
dagMetTab(Uit, In) :-
  bound(In),
  searchchar(In, '\t', Pos),
  Pos1 = Pos - 1,
  frontstr(Pos1, In, FStr, _),
  frontstr(Pos, In, _, BStr),
  format(Uit, "%s %s", FStr, BStr).

dagRepr(DagNo, Repr) :-
  dag(DagNo, Str, _), 
  dagMetTab(Str, Repr).       

staatOp(_Win, Tijd) :-
  DagKeuzeWin = win_GetCtlHandle(_Win, idc_dagkeuze),
  UrenWin     = win_GetCtlHandle(_Win, idc_uurkeuze),
  MinWin      = win_GetCtlHandle(_Win, idc_kwartierkeuze),
  IndexD = lbox_GetSelIndex(DagKeuzeWin),
  ItemD  = lbox_GetItem(DagKeuzeWin, IndexD),
  dagRepr(DagNo, Repr), 
  ItemD = Repr, !,
  UurU  = lbox_GetSelIndex(UrenWin),
  roosterinterval(N),
  Min   = lbox_GetSelIndex(MinWin) * N,
  tijd_kwart(DagNo, UurU, Min, Tijd),
  TijdNotify = cast(long, Tijd),
  ParentWin = win_GetParent(_Win),
  _Reply = win_SendEvent(ParentWin, e_User(1, TijdNotify)).

vulAantal(Win, Tijd) :-
  t_bn(Tijd, Aantal),
  str_int(AanStr, Aantal),
  AantalWin = win_GetCtlHandle(Win, idc_aantal),
  win_SetText(AantalWin, AanStr), !,
  win_SetFocus(AantalWin).
vulAantal(Win, _Tijd) :-
  AantalWin = win_GetCtlHandle(Win, idc_aantal),
  win_SetText(AantalWin, ""), !,
  win_SetFocus(AantalWin).
  
behandel(Win, Tijd, AantalStr) :-
  str_int(AantalStr, Aantal),
  retract(t_bn(Tijd, _)),		% tijdstip update
  assert(t_bn(Tijd, Aantal)), !,
  TijdNotify = cast(long, Tijd),
  ParentWin = win_GetParent(Win),
  _Reply = win_SendEvent(ParentWin, e_User(2, TijdNotify)).
behandel(Win, Tijd, AantalStr) :-
  str_int(AantalStr, Aantal),
  assert(t_bn(Tijd, Aantal)), !,	% tijdstip inserten
  TijdNotify = cast(long, Tijd),
  ParentWin = win_GetParent(Win),
  _Reply = win_SendEvent(ParentWin, e_User(3, TijdNotify)).

kwartierInterval([":00", ":15", ":30", ":45"]) :-
  roosterinterval(15), !.
kwartierinterval([":00", ":12", ":24", ":36", ":48"]).

  dlg_daguurkwart_Create(Parent, InitTijdStip):-
  	Parm = cast(long, InitTijdStip),
	win_CreateDynDialog(Parent,
		[
%BEGIN DagUurKwart, WinDefList, 12:52:47-5.10.2015, Code automatically updated!
		 dlg_font(wdef(dlg_daguurkwart_DlgType,dlg_daguurkwart_RCT,dlg_daguurkwart_Title,u_DlgBase),
		 	  dlg_daguurkwart_Font,dlg_daguurkwart_FSize,dlg_daguurkwart_Flags),
		 ctl(wdef(wc_LBox,rct(2,22,45,126),"",u_DlgBase),idc_dagkeuze,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_UseTabStops]),
		 ctl(wdef(wc_LBox,rct(46,3,83,144),"",u_DlgBase),idc_uurkeuze,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight]),
		 ctl(wdef(wc_LBox,rct(83,7,107,49),"",u_DlgBase),idc_kwartierkeuze,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight]),
		 ctl(wdef(wc_Edit,rct(83,70,108,82),"Aantal",u_DlgBase),idc_aantal,[wsf_AlignLeft,wsf_Group,wsf_TabStop,wsf_AutoHScroll]),
		 ctl(wdef(wc_PushButton,rct(84,87,116,109),"&Bij",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(83,128,115,144),"&OK",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(4,134,36,144),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_Text,rct(84,60,105,68),"Aantal",u_DlgBase),idct_aantal,[wsf_AlignLeft])
%END DagUurKwart, WinDefList
		],dlg_daguurkwart_eh,Parm).

%BEGIN DagUurKwart, idc_ok _CtlInfo
  dlg_daguurkwart_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-!,
  	staatOp(_Win, Tijd),
	AantalWin   = win_GetCtlHandle(_Win, idc_aantal),
	AantalStr   = win_GetText(AantalWin),
	behandel(_Win, Tijd, AantalStr),
	!.
%END DagUurKwart, idc_ok _CtlInfo
%MARK DagUurKwart, new events

%BEGIN DagUurKwart, idc_help _CtlInfo
  dlg_daguurkwart_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Baanbeschikbaarheid"),
	!.
%END DagUurKwart, idc_help _CtlInfo

%BEGIN DagUurKwart, idc_kwartierkeuze selchanged
  dlg_daguurkwart_eh(_Win,e_Control(idc_kwartierkeuze,_CtrlType,_CtrlWin,selchanged),0):-!,
  	staatOp(_Win, Tijd),
  	vulAantal(_Win, Tijd),
	!.
%END DagUurKwart, idc_kwartierkeuze selchanged

%BEGIN DagUurKwart, idc_uurkeuze selchanged
  dlg_daguurkwart_eh(_Win,e_Control(idc_uurkeuze,_CtrlType,_CtrlWin,selchanged),0):-!,
  	staatOp(_Win, Tijd),
  	vulAantal(_Win, Tijd),
	!.
%END DagUurKwart, idc_uurkeuze selchanged

%BEGIN DagUurKwart, idc_dagkeuze selchanged
  dlg_daguurkwart_eh(_Win,e_Control(idc_dagkeuze,_CtrlType,_CtrlWin,selchanged),0):-!,
  	staatOp(_Win, Tijd),
  	vulAantal(_Win, Tijd),
	!.
%END DagUurKwart, idc_dagkeuze selchanged

%BEGIN DagUurKwart, idc_cancel _CtlInfo
  dlg_daguurkwart_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	win_Destroy(_Win),
	!.
%END DagUurKwart, idc_cancel _CtlInfo

%BEGIN DagUurKwart, e_Create
  dlg_daguurkwart_eh(_Win,e_Create(_CreationData),0):-!,
  	IniTijd = cast(tijd, _CreationData),
  	tijd_kwart(D, Uur, Min, IniTijd),
	findall(Dag, dagRepr(_, Dag), Dagen),
	DagKeuzeWin = win_GetCtlHandle(_Win, idc_dagkeuze),
	lbox_SetTabStops(DagKeuzeWin, [15, 25]),
	lbox_Add(DagKeuzeWin, Dagen),
	dagIndex1(D, IndexD),
	lbox_SetSel(DagKeuzeWin, IndexD, b_True),
	UrenWin = win_GetCtlHandle(_Win, idc_uurkeuze),
	lbox_Add(UrenWin, ["0 u","1 u","2 u","3 u","4 u","5 u","6 u","7 u","8 u","9 u","10u",
			"11u",	"12u", "13u", "14u", "15u", "16u", "17u", "18u", 
				"19u", "20u", "21u", "22u", "23u"]),
	lbox_SetTopIndex(UrenWin, 10),
	roosterinterval(RI),
	IndexM = Min div RI,
	lbox_SetSel(UrenWin, Uur, b_True),
	MinWin = win_GetCtlHandle(_Win, idc_kwartierkeuze),
	kwartierInterval(MinList),
	lbox_Add(MinWin, MinList),
	lbox_SetSel(MinWin, IndexM, b_True),
  	vulAantal(_Win, IniTijd),
	!.
%END DagUurKwart, e_Create

  dlg_daguurkwart_eh(_,_,_):-!,fail.

%END_DLG DagUurKwart

