/*****************************************************************************

		Copyright (c) 1989 - 1998 De Lint Associates

 Project:  TASVP1
 FileName: TASPLAN.PRO
 Purpose: No description
 Written by: JdL
 Comments:
******************************************************************************/

include "tasvp1.inc"
include "tasvp1.con"
include "hlptopic.con"


%BEGIN_DLG OndBloktijden
/**************************************************************************
	Creation and event handling for dialog: OndBloktijden
**************************************************************************/

constants

%BEGIN OndBloktijden, CreateParms, 16:59:40-9.2.2013, Code automatically updated!
  dlg_ondbloktijden_DlgType = wd_Modal
  dlg_ondbloktijden_Title = "Bloktijden per onderdeel"
  dlg_ondbloktijden_RCT = rct(40,32,367,199)
  dlg_ondbloktijden_Flags = [wsf_Close,wsf_TitleBar]
  dlg_ondbloktijden_Help = idh_bloktijden
  dlg_ondbloktijden_Font = "Arial"
  dlg_ondbloktijden_FSize = 9
%END OndBloktijden, CreateParms
  blokrefr = 21

database - tbloktijden
%single curWC(wedscat)
  t_bl(wedscat, verhl)
  t_blUniek(wedscat, verhl)

predicates

  dlg_ondbloktijden_eh : EHANDLER
  nondeterm reprOndBl(wedscat, string)
  copyBl()
  copyBl(wedscat)
  met_bloktijd(wedscat, string Naam, string Repr)
  reprPer(period, string Repr)
  %reprBlokList(verhl, slist, slist)
  saveBl
  compBl
  %updateOndList(WINDOW)
  %remove(integer, integer, verhl, verhl)
  getParentCurWC(window Win, catl)
  nondeterm getCurWc(window, wedscat, ilist IndexList)
  procedure makesel(window, ilist)

clauses
%curWC(0).

getParentCurWC(Win, WL) :-
  ParentWin = win_GetParent(Win),
  findall(W, getCurWc(ParentWin, W, _), WL).

getCurWc(Win, Wedscat, IndexList) :-
  WcWin = win_GetCtlHandle(Win, idcbl_ondlist),
  lbox_GetSel(WcWin,ItemList,IndexList),
  member(Item, ItemList),
  %Index = lbox_GetSelIndex(WcWin),
  %Item = lbox_GetItem(WcWin, Index),
  parsetabs(Item, OList),
  OList = [_, Onderdeel | _],  
  %frontstr(2, Item, _, Onderdeel),
  wc(Wedscat, _, _, Onderdeel, _, _, _, _, _, _, _, _, _, _, _).
  %wc(Wedscat,Onderdeel,_EDG,_Kort,_VoorWie,_Geld,_Verl).

copyBl :-
  wc(WC, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
  %wc(WC,_Naam,_EDG,_Kort,_VoorWie,_Geld,_Verl),
  copyBl(WC),
  fail.
copyBl.

copyBl(WC) :-		
  bl(WC, Tijden),
  assert(t_bl(WC, Tijden)), !.
copyBl(WC) :-		% maak tijdelijk voor alle 
  assert(t_bl(WC, [])).

saveBl :-
  retract(t_bl(WC, [])),
  bl(WC, Tijden),
  logf('R', bl(WC, Tijden),ja),
  fail.
saveBl :-
  bl(WC, Tijden),
  retract(t_bl(WC, Tijden)),
  fail.
saveBl :-
  retract(t_bl(WC, NewTijden)),
  logf('Z', bl(WC, NewTijden),ja),
  fail.
saveBl :-
  logclose().

compBl :-
  t_bl(WC, []),
  bl(WC, _), !.		% different
compBl :-
  t_bl(WC, List),
  not(List = []),
  not(bl(WC, List)), !.	% unequal

met_bloktijd(WC, Naam, Repr) :-
  t_bl(WC, []), !,
  format(Repr, " \t%s",Naam).
met_bloktijd(WC, Naam, Repr) :-
  t_bl(WC, _), !,
  format(Repr, "o\t%s", Naam).

reprOndBl(WC, Repr) :-
  wc(WC, _, _, Naam, _, _, _, _, _, _, _, _, _, _, _),
  %wc(WC,Naam,_EDG,_Kort,_VoorWie,_Geld,_Verl),
  met_bloktijd(WC, Naam, Repr).

reprPer(per(T1, T2), Repr) :-
  bitleft(24, 2, HeleDag),
  T2 - T1 = HeleDag,
  tijd_kwart(D, _, _, T1),
  dag(D, DStr, _), !,
  format(Repr, "%s hele dag.", DStr).
reprPer(per(T, T), Repr) :-
  tijd_kwart(D, _, _, T),
  dag(D, DStr, _), !,
  tijd_str(T, TStr, _), 
  format(Repr, "%s op %s", DStr, TStr).
reprPer(per(T1, T2), Repr) :-
  tijd_kwart(D1, _, _, T1),
  dag(D1, D1Str, _), !,
  tijd_str(T1, T1Str, _),
  tijd_str(T2, T2Str, _),
  format(Repr, "%s van %s tot %s", D1Str, T1Str, T2Str).
reprPer(per(_, _), " ????").

reprBloklist([], Uit, Uit) :- !.
reprBlokList([H|T], In, [Repr|Uit]) :-
  reprPer(H, Repr),
  reprBlokList(T, In, Uit).
/*
updateOndList(Win) :-
  LboxWin = win_GetCtlHandle(Win, idcbl_ondlist),
  Index   = lbox_GetSelIndex(LboxWin),
  Item = lbox_GetItem(LboxWin, Index),
  parsetabs(Item, OList),
  OList = [_, Kort | _],
  %curWC(WC),
  wc(WC,_Naam,_EDG,Kort,_VoorWie,_Geld,_Verl),
  met_bloktijd(WC, Kort, Repr),
  lbox_Delete(LboxWin, Index),
  lbox_Add(LboxWin, Index, Repr),
  lbox_SetSel(LboxWin, Index, b_True), !.
*/  
remove(Index, Index, [_|T], T) :- !.
remove(N, Index, [X|BlList], [X|NewList]) :-
  N1 = N + 1, !,
  remove(N1, Index, BlList, NewList). 
remove(_, _, T, T).

makesel(LboxWin, List) :-
  member(Sel, List),
  lbox_SetSel(LboxWin, Sel, b_True),
  fail.
makesel(_LboxList, _).

  dlg_ondbloktijden_Create(Parent):-
  	copyBl,
	win_CreateDynDialog(Parent,
		[
%BEGIN OndBloktijden, WinDefList, 16:59:40-9.2.2013, Code automatically updated!
		 dlg_font(wdef(dlg_ondbloktijden_DlgType,dlg_ondbloktijden_RCT,dlg_ondbloktijden_Title,u_DlgBase),
		 	  dlg_ondbloktijden_Font,dlg_ondbloktijden_FSize,dlg_ondbloktijden_Flags),
		 ctl(wdef(wc_LBox,rct(2,13,149,138),"",u_DlgBase),idcbl_ondlist,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_UseTabStops,wsf_ExtendedSel]),
		 ctl(wdef(wc_LBox,rct(151,13,288,138),"",u_DlgBase),idcbl_blokperioden,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_UseTabStops]),
		 ctl(wdef(wc_PushButton,rct(292,50,324,62),"&Bij",u_DlgBase),idcbl_bij,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(292,64,324,76),"&Af",u_DlgBase),idcbl_af,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(292,140,324,164),"&OK",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(152,144,191,156),"&Repeteer",u_DlgBase),idcbl_kopie,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(292,127,324,137),"&Cancel",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(292,113,324,123),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(198,144,237,156),"&Wissen",u_DlgBase),idcbl_ondbloktijden_wis,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_Text,rct(16,1,288,12),"",u_DlgBase),idctbl_blokonderdelen,[wsf_AlignRight]),
		 ctl(wdef(wc_Text,rct(3,139,115,166),"",u_DlgBase),idctbl_ctrlklik,[wsf_AlignLeft])
%END OndBloktijden, WinDefList
		],dlg_ondbloktijden_eh,0),
	!.

%BEGIN OndBloktijden, idc_ok _CtlInfo
  dlg_ondbloktijden_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-!,
	saveBl,
	win_Destroy(_Win),
	!.
%END OndBloktijden, idc_ok _CtlInfo
%MARK OndBloktijden, new events

%BEGIN OndBloktijden, e_Update
  dlg_ondbloktijden_eh(_Win,e_Update(_UpdateRct),0):-!,
	CtrlWin = win_GetCtlHandle(_Win, idctbl_ctrlklik),
	Font = font_Create(ff_Helvetica, [], 8),
	win_SetFont(CtrlWin, Font),
	win_SetText(CtrlWin, "o = onderdeel kent bloktijden.\nCtrl-klik  om meerdere tegelijk te selecteren."),
	!.
%END OndBloktijden, e_Update

%BEGIN OndBloktijden, idcbl_ondbloktijden_wis _CtlInfo
  dlg_ondbloktijden_eh(_Win,e_Control(idcbl_ondbloktijden_wis,_CtrlType,_CtrlWin,_CtlInfo),0):-
	getCurwC(_Win, WC,_),
	%curWC(WC),
	retract(t_bl(WC, _)),
	assert(t_bl(WC, [])),
	List = win_GetCtlHandle(_Win, idcbl_blokperioden),
	lbox_Clear(List),
	dialog_SetState(_Win, [enable(idcbl_af, b_False), enable(idcbl_ondbloktijden_wis, b_False)]),
	%--
	findall(Repr, reprOndBl(_, Repr),Lijst),		% refresh het onderdelenlijstje
	LboxWin = win_GetCtlHandle(_Win, idcbl_ondlist),
	lbox_GetSel(LboxWin,_ItemList,IndexList),
	lbox_clear(LboxWin),
	lbox_Add(LboxWin, Lijst),
	makeSel(LboxWin, IndexList),
	!.
%END OndBloktijden, idcbl_ondbloktijden_wis _CtlInfo

%BEGIN OndBloktijden, e_Destroy
  dlg_ondbloktijden_eh(_Win,e_Destroy,0):-!,
	retractall(t_bl(_, _)),
	!.
%END OndBloktijden, e_Destroy

%BEGIN OndBloktijden, idc_help _CtlInfo
  dlg_ondbloktijden_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Bloktijden"),
	!.
%END OndBloktijden, idc_help _CtlInfo

%BEGIN OndBloktijden, idcbl_blokperioden selchanged
  dlg_ondbloktijden_eh(_Win,e_Control(idcbl_blokperioden,_CtrlType,LboxWin,selchanged),0):-
	_Index = lbox_GetSelIndex(LboxWin),
	dialog_SetState(_Win, [enable(idcbl_af, b_True)]),
	!.
  dlg_ondbloktijden_eh(_Win,e_Control(idcbl_blokperioden,_CtrlType,_CtrlWin,selchanged),0):-
	dialog_SetState(_Win, [enable(idcbl_af, b_False)]),
	!.
%END OndBloktijden, idcbl_blokperioden selchanged

%BEGIN OndBloktijden, e_CloseRequest
  dlg_ondbloktijden_eh(_Win,e_CloseRequest,0):-
	compBl,
	ANSWER = dlg_Ask("Veranderingen","Bewaren?",["&Ja","&Nee"]),
	ANSWER = resp_default,
	saveBl,
	fail.
	%!.
%END OndBloktijden, e_CloseRequest

%BEGIN OndBloktijden, idcbl_ondlist selchanged
  dlg_ondbloktijden_eh(_Win,e_Control(idcbl_ondlist,_CtrlType,_LboxWin,selchanged),0):-
	%LboxWin = win_GetCtlHandle(_Win, idcbl_ondlist),
	getCurWC(_Win, WC,_),
%	Index = lbox_GetSelIndex(LboxWin),
%	Item = lbox_GetItem(LboxWin, Index),
%	parsetabs(Item, OList),
%	OList = [_, Kort | _],
	%frontstr(2, Item, _, Onderdeel),
	wc(WC, _, _, Onderdeel, _, _, _, _, _, _, _, _, _, _, _),
	%wc(WC,Onderdeel,_EDG,_Kort,_VoorWie,_Geld,_Verl),
	dialog_SetStr(_Win,idctbl_blokonderdelen,Onderdeel),
	%assert(curWC(WC)),
	t_bl(WC,BlokPerList),
	reprBlokList(BlokPerList, [], BlokStrList),
	List = win_GetCtlHandle(_Win, idcbl_blokperioden),
	lbox_Clear(List),
	lbox_Add(List, BlokStrList),
	%updateOndList(_Win),
	dialog_SetState(_Win, [enable(idcbl_af, b_False), enable(idcbl_ondbloktijden_wis, b_False)]),
	not(BlokPerList = []),
	dialog_SetState(_Win, [enable(idcbl_ondbloktijden_wis, b_True)]),
	!.
%END OndBloktijden, idcbl_ondlist selchanged


%BEGIN OndBloktijden, e_User
  dlg_ondbloktijden_eh(_Win,e_User(blokrefr,_Ptr),0):-	% refresh (van sel)
	findall(Repr, reprOndBl(_, Repr),Lijst),		% het onderdelenlijstje
	LboxWin = win_GetCtlHandle(_Win, idcbl_ondlist),
	lbox_GetSel(LboxWin,_ItemList,IndexList),
	lbox_clear(LboxWin),
	lbox_Add(LboxWin, Lijst),
	makeSel(LboxWin, IndexList),
	%---------------------------------------------------
	List = win_GetCtlHandle(_Win, idcbl_blokperioden),	% de bloktijdenlijst
	getbacktrack(Here),
	getCurWC(_Win, WC, _),
	wc(WC, _, _, Onderdeel, _, _, _, _, _, _, _, _, _, _, _),
	%wc(WC,Onderdeel,_EDG,_Kort,_VoorWie,_Geld,_Verl),	% de voorste
	cutbacktrack(Here),
	dialog_SetStr(_Win,idctbl_blokonderdelen,Onderdeel),
	%assert(curWC(WC)),
	t_bl(WC,BlokPerList),
	reprBlokList(BlokPerList, [], BlokStrList),
	lbox_Clear(List),
	lbox_Add(List, BlokStrList),
	dialog_SetState(_Win, [enable(idcbl_af, b_False), enable(idcbl_ondbloktijden_wis, b_False)]),
	not(BlokPerList = []),
	dialog_SetState(_Win, [enable(idcbl_ondbloktijden_wis, b_True)]),
	!.
%END OndBloktijden, e_User

%BEGIN OndBloktijden, idc_cancel _CtlInfo
  dlg_ondbloktijden_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-
	compBl,
	ANSWER = dlg_Ask("Veranderingen","Bewaren?",["&Ja","&Nee"]),
	ANSWER = resp_default,
	saveBl,
	win_Destroy(_Win),
	!.
  dlg_ondbloktijden_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	win_Destroy(_Win),
	!.
%END OndBloktijden, idc_cancel _CtlInfo

%BEGIN OndBloktijden, e_Create
  dlg_ondbloktijden_eh(_Win,e_Create(_CreationData),0):-!,
	findall(Repr, reprOndBl(_, Repr),Lijst),		% het onderdelenlijstje
	LboxWin = win_GetCtlHandle(_Win, idcbl_ondlist),
	lbox_SetTabStops(LboxWin, [10, 20, 30]),
	lbox_Add(LboxWin, Lijst),
	lbox_SetSel(LboxWin, 0, b_True),
	%---------------------------------------------------
	List = win_GetCtlHandle(_Win, idcbl_blokperioden),	% de bloktijdenlijst
	Font = font_create(ff_Fixed, [], 9),
	win_SetFont(List, Font),
	getbacktrack(Here),
	wc(WC, _, _, Onderdeel, _, _, _, _, _, _, _, _, _, _, _),
	%wc(WC,Onderdeel,_EDG,_Kort,_VoorWie,_Geld,_Verl),	% de voorste
	cutbacktrack(Here),
	dialog_SetStr(_Win,idctbl_blokonderdelen,Onderdeel),
	%assert(curWC(WC)),
	t_bl(WC,BlokPerList),
	reprBlokList(BlokPerList, [], BlokStrList),
	lbox_Add(List, BlokStrList),
	dialog_SetState(_Win, [enable(idcbl_af, b_False), enable(idcbl_ondbloktijden_wis, b_False)]),
	not(BlokPerList = []),
	dialog_SetState(_Win, [enable(idcbl_ondbloktijden_wis, b_True)]),
	!.
%END OndBloktijden, e_Create

%BEGIN OndBloktijden, idcbl_bij _CtlInfo
  dlg_ondbloktijden_eh(_Win,e_Control(idcbl_bij,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	dlg_ondbloktijddef_Create(_Win),
	!.
%END OndBloktijden, idcbl_bij _CtlInfo

%BEGIN OndBloktijden, idcbl_af _CtlInfo
  dlg_ondbloktijden_eh(_Win,e_Control(idcbl_af,_CtrlType,_CtrlWin,_CtlInfo),0):-
	PerListWin = win_GetCtlHandle(_Win, idcbl_blokperioden),
	Index = lbox_GetSelIndex(PerListWin),
	getCurWC(_Win, WC, IndexList),
	retract(t_bl(WC, BlList)),
	remove(0, Index, BlList, NewList),
	assert(t_bl(WC, NewList)),
	lbox_Delete(PerListWin, Index),
	% ----------
	findall(Repr, reprOndBl(_, Repr),Lijst),		% het onderdelenlijstje
	LboxWin = win_GetCtlHandle(_Win, idcbl_ondlist),
	lbox_Clear(LboxWin),
	lbox_Add(LboxWin, Lijst),
	IndexList = [Ind | _], 
	lbox_SetSel(LboxWin, Ind, b_True),
	
	!.
%END OndBloktijden, idcbl_af _CtlInfo

%BEGIN OndBloktijden, idcbl_kopie _CtlInfo
  dlg_ondbloktijden_eh(_Win,e_Control(idcbl_kopie,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	dlg_ondbloktijdensel_Create(_Win),
	!.
%END OndBloktijden, idcbl_kopie _CtlInfo

  dlg_ondbloktijden_eh(_,_,_):-!,fail.

%END_DLG OndBloktijden



%BEGIN_DLG OndBloktijdenSel
/**************************************************************************
	Creation and event handling for dialog: OndBloktijdenSel
**************************************************************************/

constants

%BEGIN OndBloktijdenSel, CreateParms, 21:20:29-8.10.2008, Code automatically updated!
  dlg_ondbloktijdensel_DlgType = wd_Modal
  dlg_ondbloktijdensel_Title = "Kopiëren vanaf:"
  dlg_ondbloktijdensel_RCT = rct(40,32,202,165)
  dlg_ondbloktijdensel_Flags = [wsf_Close,wsf_TitleBar]
  dlg_ondbloktijdensel_Help = idh_bloktijden
  dlg_ondbloktijdensel_Font = "Arial"
  dlg_ondbloktijdensel_FSize = 9
%END OndBloktijdenSel, CreateParms

predicates

  dlg_ondbloktijdensel_eh : EHANDLER
  nondeterm copyBlOnd(window, string)

clauses
copyBlOnd(Win, Naam) :-
  wc(WC, _, _, Naam, _, _, _, _, _, _, _, _, _, _, _),
  %wc(WC,Naam,_EDG,_Kort,_VoorWie,_Geld,_Verl),
  t_bl(WC, Tijden),
  not(Tijden = []),
  getParentCurWC(Win, WCL),
  not(member(WC, WCL)),
  %not(curWC(WC)),
  not(t_blUniek(_, Tijden)),
  assert(t_blUniek(WC, Tijden)).

  dlg_ondbloktijdensel_Create(Parent):-
	win_CreateDynDialog(Parent,
		[
%BEGIN OndBloktijdenSel, WinDefList, 21:20:29-8.10.2008, Code automatically updated!
		 dlg_font(wdef(dlg_ondbloktijdensel_DlgType,dlg_ondbloktijdensel_RCT,dlg_ondbloktijdensel_Title,u_DlgBase),
		 	  dlg_ondbloktijdensel_Font,dlg_ondbloktijdensel_FSize,dlg_ondbloktijdensel_Flags),
		 ctl(wdef(wc_LBox,rct(2,15,126,131),"",u_DlgBase),idcbl_ondlist,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_UseTabStops]),
		 ctl(wdef(wc_PushButton,rct(127,122,159,132),"&OK",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(127,110,159,120),"&Cancel",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(127,98,159,108),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_Text,rct(4,3,98,13),"In aanmerking komen:",u_DlgBase),idctbl_in_aanmerking_komen,[wsf_AlignLeft])
%END OndBloktijdenSel, WinDefList
		],dlg_ondbloktijdensel_eh,0).

%BEGIN OndBloktijdenSel, idc_ok _CtlInfo
  dlg_ondbloktijdensel_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idcbl_ondlist),
	lbox_GetSel(LboxWin,_ItemList,_IndexList),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	wc(WC2, _, _, Item, _, _, _, _, _, _, _, _, _, _, _),
	%wc(WC2,Item,_EDG,_Kort,_VoorWie,_Geld,_Verl),
	getParentCurWC(_Win, WCL),
	member(WC1, WCL),
	%curWC(WC1),
	%wc(WC1,Ond,_,_,_,_,_),
	%write("\n", Ond),
	t_bl(WC2, NewList),
	retractall(t_bl(WC1, _)),
	assert(t_bl(WC1, NewList)),
	fail.
  dlg_ondbloktijdensel_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	ParentWin = win_GetParent(_Win),
	_Reply = win_SendEvent(ParentWin,e_User(blokrefr, 0)),
	win_Destroy(_Win),
	!.
%END OndBloktijdenSel, idc_ok _CtlInfo
%MARK OndBloktijdenSel, new events

%BEGIN OndBloktijdenSel, e_Destroy
  dlg_ondbloktijdensel_eh(_Win,e_Destroy,0):-!,
	retractall(t_blUniek(_,_)),
	!.
%END OndBloktijdenSel, e_Destroy

%BEGIN OndBloktijdenSel, idc_help _CtlInfo
  dlg_ondbloktijdensel_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Bloktijden"),
	!.
%END OndBloktijdenSel, idc_help _CtlInfo

%BEGIN OndBloktijdenSel, idc_cancel _CtlInfo
  dlg_ondbloktijdensel_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	win_Destroy(_Win),
	!.
%END OndBloktijdenSel, idc_cancel _CtlInfo

%BEGIN OndBloktijdenSel, e_Create
  dlg_ondbloktijdensel_eh(_Win,e_Create(_CreationData),0):-!,
	findall(Repr, copyBlOnd(_Win, Repr),Lijst),
	LboxWin = win_GetCtlHandle(_Win, idcbl_ondlist),
	lbox_SetTabStops(LboxWin, [10, 20, 30]),
	lbox_Add(LboxWin, Lijst),
	!.
%END OndBloktijdenSel, e_Create

  dlg_ondbloktijdensel_eh(_,_,_):-!,fail.

%END_DLG OndBloktijdenSel


%BEGIN_DLG OndBloktijdDef
/**************************************************************************
	Creation and event handling for dialog: OndBloktijdDef
**************************************************************************/

constants

%BEGIN OndBloktijdDef, CreateParms, 22:08:38-27.10.2011, Code automatically updated!
  dlg_ondbloktijddef_DlgType = wd_Modal
  dlg_ondbloktijddef_Title = "Stel bloktijden vast"
  dlg_ondbloktijddef_RCT = rct(40,32,179,288)
  dlg_ondbloktijddef_Flags = [wsf_Close,wsf_TitleBar]
  dlg_ondbloktijddef_Help = idh_bloktijden
  dlg_ondbloktijddef_Font = "Arial"
  dlg_ondbloktijddef_FSize = 10
%END OndBloktijdDef, CreateParms

predicates

  dlg_ondbloktijddef_eh : EHANDLER
%  kwartlist(slist)
procedure   kwartlist1(integer, integer, slist, slist)
%  items2tijden(integer, ilist, string, period)
  items2tijden1(integer, tijd, ilist, string, period)
%  mergeBl(period, verhl, verhl)
  combinBl(period, period, period)
procedure meerdagen(slist, string, string)

clauses

mergeBl(Per, [], [Per]) :- !.
mergeBl(Per, [H|T], [X|T]) :-
  combinBl(Per, H, X), !.
mergeBl(per(T1, T2), [per(H1, H2), per(H3, H4) | Trail], [per(H1, H2),per(T1, T2),per(H3, H4)|Trail]) :-
  H2 < T1,
  T2 < H3, !. 
mergeBl(per(T1, T2), [per(H1, H2) | Trail], [per(T1, T2),per(H1, H2)|Trail]) :-
  T2 < H1, !. 
mergeBl(Per, [H|T], [H|Uit]) :-
  mergeBl(Per, T, Uit).  
  
combinBl(per(T1, T2), per(H1, H2),  per(H1, H2)) :-
  T1 >= H1,		% T tussen H
  T1 <= H2,
  T2 >= H1,
  T2 <= H2, !.
combinBl(per(T1, T2), per(H1, H2), per(H1, T2)) :-
  T1 >= H1,		% T1 tussen H
  T1 <= H2, !.
combinBl(per(T1, T2), per(H1, H2), per(T1, H2)) :-
  T2 >= H1,		% T2 tussen H
  T2 <= H2, !.
combinBl(per(T1, T2), per(H1, H2), per(T1, T2)) :-
  H1 >= T1,		% H tussen T
  H1 <= T2,
  H2 >= T1,
  H2 <= T2, !.

items2tijden(_Dag, [], "", per(0,0)) :- !.
items2tijden(Dag, [T], Text, per(Kw, Kw)) :- !,
  T1 = T + 1,
  bitleft(Dag, 7, D1),
  Kw = D1 + T1,
  reprPer(per(Kw, Kw), Text).
items2tijden(Dag, Selections, Text, Per) :-
  Selections = [First|Rest],
  items2tijden1(Dag, First, Rest, Text, Per).

items2tijden1(Dag, T0, [Last], Text, per(Kw1, Kw2)) :- !,
  T1 = T0 + 1,
  L1 = Last + 1,
  bitleft(Dag, 7, D1),
  Kw1 = D1 + T1,
  Kw2 = D1 + L1,
  reprPer(per(Kw1, Kw2), Text).
items2tijden1(Dag, T1, [_|Selecties], Text, Per) :-	% zoek de laatste op
  items2tijden1(Dag, T1, Selecties, Text, Per).

kwartlist(List) :-					% vulling voor kwartselectie
  kwartlist1(1, 97, [], List).

kwartlist1(End, End, List, List) :- !.
kwartlist1(N, End, In, [Kwart|Uit]) :-
  tijd_str(N, Kwart, _),
  N1 = N + 1, !,
  kwartlist1(N1, End, In, Uit).

meerdagen([_, _ | _], In, Uit) :-
  format(Uit, "%s\netcetera.", In), !.
meerdagen(_, In, In).
  
  dlg_ondbloktijddef_Create(Parent):-
	win_CreateDynDialog(Parent,
		[
%BEGIN OndBloktijdDef, WinDefList, 22:08:38-27.10.2011, Code automatically updated!
		 dlg_font(wdef(dlg_ondbloktijddef_DlgType,dlg_ondbloktijddef_RCT,dlg_ondbloktijddef_Title,u_DlgBase),
		 	  dlg_ondbloktijddef_Font,dlg_ondbloktijddef_FSize,dlg_ondbloktijddef_Flags),
		 ctl(wdef(wc_LBox,rct(3,12,51,172),"",u_DlgBase),idcb_blokt_dag,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_ExtendedSel]),
		 ctl(wdef(wc_LBox,rct(50,12,85,172),"",u_DlgBase),idcb_blokt_van,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_Sort,wsf_NoIntegralHeight,wsf_Disabled,wsf_Invisible,wsf_MultiSelect]),
		 ctl(wdef(wc_PushButton,rct(101,75,135,94),"&Hele dag",u_DlgBase),idbc_hele_dag,[wsf_Group,wsf_TabStop,wsf_Invisible]),
		 ctl(wdef(wc_PushButton,rct(101,100,135,122),"&Doe",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop,wsf_Disabled]),
		 ctl(wdef(wc_PushButton,rct(102,154,134,166),"&Stop",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(102,141,134,151),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_LBox,rct(21,187,76,254),"",u_DlgBase),idct_welkeelke,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight]),
		 ctl(wdef(wc_Text,rct(3,2,47,12),"Dag",u_DlgBase),idcbt_dag,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(49,2,79,12),"Van/tot",u_DlgBase),idcbt_vanaf,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(3,174,96,184),"Selecteer gewenste dag(en)",u_DlgBase),idcbt_klik_op_tijdstippen,[wsf_NoWrap]),
		 ctl(wdef(wc_Text,rct(97,14,135,69),"Blokperiode",u_DlgBase),idctb_blokperiode,[wsf_AlignLeft,wsf_Invisible]),
		 ctl(wdef(wc_Text,rct(4,187,21,196),"Elke:",u_DlgBase),idct_elke,[wsf_AlignLeft])
%END OndBloktijdDef, WinDefList
		],dlg_ondbloktijddef_eh,0).

%BEGIN OndBloktijdDef, idc_ok _CtlInfo
  dlg_ondbloktijddef_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	LboxWinD = win_GetCtlHandle(_Win, idcb_blokt_dag),
	lbox_GetSel(LboxWinD, ItemListD, _IndexListD),			% get dag
	LboxWinT = win_GetCtlHandle(_Win, idcb_blokt_van),
	lbox_GetSel(LboxWinT, _ItemListT, IndexListT),			% get tijden
	not(IndexListT = []),
	getParentCurWc(_Win, WedscList),
	member(Wedscat, WedscList),
  	  member(TextD, ItemListD),
	    dag(D1,TextD,_), 
	    items2tijden(D1,IndexListT, _TextT, Periode),
	    getbacktrack(Here),
	    t_bl(Wedscat,InList),
	    cutbacktrack(Here),
	    mergeBl(Periode, Inlist, OutList),
	    retractall(t_bl(Wedscat, _)),
	    assert(t_bl(Wedscat, OutList)),
	fail.
  dlg_ondbloktijddef_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	KwartListWin = win_GetCtlHandle(_Win, idcb_blokt_van),
	selectGeen(KwartListWin),
	dialog_SetStr(_Win, idctb_blokperiode, ""),
	ParentWin = win_GetParent(_Win),
	win_SendEvent(ParentWin,e_User(blokrefr, 0)),
	!.
%END OndBloktijdDef, idc_ok _CtlInfo
%MARK OndBloktijdDef, new events

%BEGIN OndBloktijdDef, idct_welkeelke selchanged
  dlg_ondbloktijddef_eh(_Win,e_Control(idct_welkeelke,_CtrlType,ElkeWin,selchanged),0):-
	LboxWin = win_GetCtlHandle(_Win, idcb_blokt_dag),
	0 = lbox_GetSelIndex(ElkeWin),		% alle werkdagen
	findall(DagX, dag(DagX, _, _), DagXL),
	elkeWerkdag(DagXL, 0, NList),
	not(NList = []),
	reverse(NList, [], NListR),		% van achter naar voren
	zetSelect(LboxWin, NListR), 
	dialog_SetState(_Win, [show(idcb_blokt_van, b_True),
				enable(idcb_blokt_van, b_True),
				show(idbc_hele_dag, b_True),
				enable(idbc_hele_dag, b_True), enable(idc_ok, b_False)]),
	KwartListWin = win_GetCtlHandle(_Win, idcb_blokt_van),
	lbox_Clear(KwartListWin),	% om eventuele selecties kwijt te raken
	kwartlist(List),
%	nl, write(List),
	lbox_Add(KwartListWin, List),
	lbox_SetTopIndex(KwartListWin, 60),
	dialog_SetStr(_Win, idctb_blokperiode, ""),
	!.
  dlg_ondbloktijddef_eh(_Win,e_Control(idct_welkeelke,_CtrlType,ElkeWin,selchanged),0):-
	LboxWin = win_GetCtlHandle(_Win, idcb_blokt_dag),
	Index = lbox_GetSelIndex(ElkeWin),
	Index <> 0,
	findall(DagX, dag(DagX, _, _), DagXL),
	findall(N, soortgelijkeDagen(Index, DagXL, 0, N), NList),
	reverse(NList, [], NListR),		% van achter naar voren
	zetSelect(LboxWin, NListR), 
	dialog_SetState(_Win, [show(idcb_blokt_van, b_True),
				enable(idcb_blokt_van, b_True),
				show(idbc_hele_dag, b_True),
				enable(idbc_hele_dag, b_True), enable(idc_ok, b_False)]),
	KwartListWin = win_GetCtlHandle(_Win, idcb_blokt_van),
	lbox_Clear(KwartListWin),	% om eventuele selecties kwijt te raken
	kwartlist(List),
%	nl, write(List),
	lbox_Add(KwartListWin, List),
	lbox_SetTopIndex(KwartListWin, 60),
	dialog_SetStr(_Win, idctb_blokperiode, ""),
	!.
%END OndBloktijdDef, idct_welkeelke selchanged

%BEGIN OndBloktijdDef, idc_help _CtlInfo
  dlg_ondbloktijddef_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Bloktijden"),
	!.
%END OndBloktijdDef, idc_help _CtlInfo

%BEGIN OndBloktijdDef, e_Create
  dlg_ondbloktijddef_eh(_Win,e_Create(_CreationData),0):-!,
	DagList = win_GetCtlHandle(_Win, idcb_blokt_dag),
	Font = font_create(ff_Fixed, [], 9),
	win_SetFont(DagList, Font),
	findall(Dag, dag(_,Dag,_), Dagen),
	lbox_Add(DagList, Dagen),
	KwartListWin = win_GetCtlHandle(_Win, idcb_blokt_van),
	win_SetFont(KwartListWin, Font),
	ElkeListWin = win_GetCtlHandle(_Win, idct_welkeelke),
	win_SetFont(ElkeListWin, Font),
	ElkeList = ["werkdag","maandag","dinsdag","woensdag","donderdag","vrijdag","zaterdag","zondag"],
	lbox_Add(ElkeListWin, ElkeList),
	!.
%END OndBloktijdDef, e_Create

%BEGIN OndBloktijdDef, idc_cancel _CtlInfo
  dlg_ondbloktijddef_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	win_Destroy(_Win),
	!.
%END OndBloktijdDef, idc_cancel _CtlInfo

%BEGIN OndBloktijdDef, idbc_hele_dag _CtlInfo
  dlg_ondbloktijddef_eh(_Win,e_Control(idbc_hele_dag,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idcb_blokt_van),
	selectAlle(LboxWin),
	LboxWinD = win_GetCtlHandle(_Win, idcb_blokt_dag),
	lbox_GetSel(LboxWinD, ItemListD, _IndexListD),			% get dag
	ItemListD = [TextD|_],
	format(TextT, "%s geblokkeerd.", TextD),
	meerdagen(ItemListD, TextT, TextT1),
	dialog_SetStr(_Win, idctb_blokperiode, TextT1),
	dialog_SetState(_Win, [show(idctb_blokperiode, b_true), enable(idc_ok, b_True)]),
	!.
%END OndBloktijdDef, idbc_hele_dag _CtlInfo

%BEGIN OndBloktijdDef, idcb_blokt_van selchanged
  dlg_ondbloktijddef_eh(_Win,e_Control(idcb_blokt_van,_CtrlType,LboxWinT,selchanged),0):-!,
	LboxWinD = win_GetCtlHandle(_Win, idcb_blokt_dag),
	lbox_GetSel(LboxWinD, ItemListD, _IndexListD),			% get dag
	ItemListD = [TextD|_],
	%LboxWinT = win_GetCtlHandle(_Win, idcb_blokt_van),
	lbox_GetSel(LboxWinT, _ItemListT, IndexListT),			% get tijden
	dag(D1,TextD,_), 
	items2tijden(D1,IndexListT, TextT, _),
	meerdagen(ItemListD, TextT, TextT1),
	dialog_SetStr(_Win, idctb_blokperiode, TextT1),
	dialog_SetState(_Win, [show(idctb_blokperiode, b_true), enable(idc_ok, b_True)]),
	!.
%END OndBloktijdDef, idcb_blokt_van selchanged

%BEGIN OndBloktijdDef, idcb_blokt_dag selchanged
  dlg_ondbloktijddef_eh(_Win,e_Control(idcb_blokt_dag,_CtrlType,LboxWin,selchanged),0):-
	Index = lbox_GetSelIndex(LboxWin),
	_Item = lbox_GetItem(LboxWin, Index),
	dialog_SetState(_Win, [show(idcb_blokt_van, b_True),
				enable(idcb_blokt_van, b_True),
				show(idbc_hele_dag, b_True),
				enable(idbc_hele_dag, b_True),
				enable(idc_ok, b_False)]),
	KwartListWin = win_GetCtlHandle(_Win, idcb_blokt_van),
	lbox_Clear(KwartListWin),	% om eventuele selecties kwijt te raken
	kwartlist(List),
%	nl, write(List),
	lbox_Add(KwartListWin, List),
	lbox_SetTopIndex(KwartListWin, 60),
	dialog_SetStr(_Win, idctb_blokperiode, ""),
	!.
%END OndBloktijdDef, idcb_blokt_dag selchanged

  dlg_ondbloktijddef_eh(_,_,_):-!,fail.

%END_DLG OndBloktijdDef

%_____________________________________________________________________________________
%#####################################################################################
%  planning van onderdelen
%
%

database - plannen
  single reedsError(integer Aantal)

predicates
  %plan_wd(WINDOW, integer,integer,wedscat,tijd,janee LogBlijfOpen) 
  plan_wedst_als(WINDOW,integer,integer,wedscat,tgnst,tgnst,tijd,janee LogBlijfOpen,integer EindTijd)-(i,i,i,i,i,i,i,i,i)
  plan_vorige(WINDOW,integer,wedscat,tijd,janee LogOpen,tijd EindTijd)-(i,i,i,i,i,i)
%determ  herplan(WINDOW, schl, janee LogBlijfOpen) - (i, i, i)
determ  checkNogEens(sch, integer, BOOLEAN MetOnbekend) - (i,i,i)
%nondeterm  split(sch,schl,schl,schl)
%	   qsort(schl,schl)
%determ	   schort_op1(integer,wedscat,schl,schl) - (i, i, i, o)
	   %na_vorige(integer,wedscat,tijd)
	   geplande_tijd(integer,wedscat,tgnst, tijd)
nondeterm  zoek_oplossing(WINDOW, integer, integer, wedscat, tgnst, tgnst, tijd, wt, janee LogOpen, tijd EindTijd)
	   %pick(integer, tijdl, integer, tijd)
%	   wtl2string(wtl, slist, tijdl)
%	   afz(janee, integer, wedscat, tijd, tgnst, janee LogBlijfOpen)
	   baan_toewijzing(tijd, string, integer, wedscat)
	   baan_toewijzing1(integer, integer, string, string)
%	check_vorige(integer)
%nondeterm  undoSchortOp(schl)
determ  plan_wd0(WINDOW, integer,integer,wedscat,tijd,tijd) - (i, i, i, i, i,i)
  nondeterm baanPlan(tijd Tijd, integer Num, wedscat Cat, string BaanS)
  nietEigen(integer Num, wedscat Cat, integer Num1, wedscat Cat1)
  eenOnbekend(wedscat,integer,tgnst, tgnst, BOOLEAN)
  planbaar1(wedscat, integer, BOOLEAN)

clauses
reedsError(0).

eenOnbekend(WC,No, onbekend, onbekend, b_True) :-	% een vd twee onbekend en niet gepland
  bitleft(No, 1, No1),
  wd(No1, WC, _T1, _T2, _, _, _), 
  not(w2(No1,WC,_,_,_,_,_,_)), !.			% eerste ronde?
eenOnbekend(WC,No, _, onbekend, b_True) :-
  bitleft(No, 1, No1),
  No2 = No1 + 1,
  wd(No2, WC, _T1, _T2, _, _, _),
  not(w2(No2,WC,_,_,_,_,_,_)), !.

planbaar1(WC, Num, MetOnbekend):-
  wd(Num, WC, T1, T2, _, _, _), 
  not(T1 = bye),
  not(T2 = bye),
  not(eenOnbekend(WC, Num, T1, T2, MetOnbekend)),
  not(w3(Num,WC,_,_,_,_,_)), !.

plan_wd0(_, _,Num,Cat,_,_):- 
  w2(Num,Cat,_,_,_,_,_,_), !.           % voor de zekerheid
plan_wd0(Win, Meerv,Num,Cat,BeginDag,EindTijd):- 
  schort_op1(Num,Cat,[],SchortList),
  undoSchortOp(SchortList),
  qsort(SchortList,SchList),
  wd(Num,Cat,T1,T2,_,_,_),			% kijk wie er meespelen
  plan_wedst_als(Win,MeerV,Num,Cat,T1,T2,BeginDag,ja,EindTijd), !,
  herplan(Win,SchList,ja).


/*************************************************************************/    
/*    de eigenlijke plan routine                                         */ 
/*************************************************************************/    
plan_wedst_als(_,_,Num,Cat,_,_,_,_,_) :-		/* wedstrijd is er niet            */
  not(wd(Num,Cat,_,_,_,_,_)), !.
plan_wedst_als(_,_,Num,Cat,_,_,_,_,_) :-		/* wedstrijd al gespeeld,          */
  w3(Num,Cat,_,_,_,_,_), !.				/*   winnaar bekend, skip          */
plan_wedst_als(_,_,Num,Cat,_,_,_,_,_) :-		/* wedstrijd al gepland            */
  w2(Num,Cat,_,_,_,_,_,_), !.			/*   skip deze ook                 */
plan_wedst_als(_,_,_,_,_,bye,_,_,_) :- !.		/* hoeft niet gepland              */
plan_wedst_als(_,_,_,_,bye,_,_,_,_) :- !.		/* hoeft niet gepland              */
plan_wedst_als(_,_,_,_,onbekend,onbekend,_,_,_) :- !.
  % not(fullfunc), !.			% voorkom verder plannen....
plan_wedst_als(_Win,enkelvoudig,Num,Cat,T1,T2,BeginT,LogOpen,EindTijd) :- 	% plan een wedstrijd
  zoek_oplossing(_Win,enkelvoudig, Num, Cat, T1, T2, BeginT, Oplossing,LogOpen,EindTijd), 
  Oplossing = wt(_, Opl, _, _, _, _), 
  Opl < EindTijd, !,
  time_stamp1(TT, _, _),
  baan_toewijzing(Opl, Baan, Num, Cat),
  logf('A',w2(Num,Cat,Opl,nee,nee,' ',TT,Baan),LogOpen),
  select(Cat,Num).
plan_wedst_als(_Win,meervoudig,_Num,_Cat,_T1,_T2,_BeginT,_LogOpen,_) :- % plan een wedstrijd met keus
  writeToJournaal("Fout in programmalogica!"), beep(),
  fail.
/*  findall(Oplossing,zoek_oplossing(_Win,meervoudig, Num, Cat, T1, T2, BeginT, Oplossing,LogOpen),Result),
  not(Result = []),  
  wtl2string(Result, StrRes, TijdRes),
  logclose(),
  dlg_ListSelect("dag      uur  banen",StrRes,1,_StrSel,Regel),
  pick(1, TijdRes, Regel, OplTijd),
  time_stamp(TT, _, _),
  baan_toewijzing(OplTijd, BaanNo, Num, Cat),
  checkNogEens(s(Num,Cat,OplTijd,nee,nee,' ',TT,BaanNo),0,nee,b_True),
  % check nog een keer...
  select(Cat,Num).*/
plan_wedst_als(_Win,enkelvoudig,_,_,_,_,_,_,_) :-
  reedsError(N),
  N1 = N + 1,
  assert(reedsError(N1)),
  %select(Cat, Num), !,
  %logclose(),
  %dlg_Note("Wedstrijd(en)","Planning is mislukt...\nZie hierna \"Nog te plannen\" (F4)"),
  fail. 

checkNogEens(s(Num,Cat,Tijd,_B,_C,_Forc,_Stamp,_Baan), VorigResult, MetOnbekend) :-
  loglock(lees, "checkNogEeens"),
  planbaar1(Cat, Num, MetOnbekend),
  check_forceer_c(0, [Tijd], Num, Cat, _, Result,_, _),
  %write("\nResult: ", Result, " Vorig result: ", VorigResult),
  bitand(Result, 0xFEFF, Result1),	% filter voorronde niet gepland uit
  bitand(VorigResult, 0xFEFF, ResultV),	% filter voorronde niet gepland uit
  Result1 = ResultV, !.
checkNogEens(_, _, _) :- 
  logclose(),
  S4 = "Situatie gewijzigd (netwerk?).\nPlan opnieuw.",
  dlg_MessageBox( "Planning", S4, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, 
  									mesbox_suspendapplication ),
  fail.

zoek_oplossing(Win,Aantal, Num, Cat, T1, T2, BeginT, wt(OplString, Tijd, ja, "BaanPark", b_False, "wasop"), LogOpen,EindTijd) :-
  plan_vorige(Win,Num, Cat, BeginT,LogOpen,EindTijd),		% plan eventuele vorige
  de_spelers(Num,Cat,T1,T2,SpelerS),
  select(Cat,Num), !,
  na_vorige(Num,Cat, T1, T2, BeginT1),
  grootste(BeginT,BeginT1,BeginTijd),
  scan_planning_c(Aantal, BeginTijd, Spelers, Num, Cat, Tijd, Over),
  tijd_kwart(D1,_,_,Tijd),
  dag(D1, DStr1, _), 
  tijd_str(Tijd, TStr1, _),
  format(OplString, "%-8s %5s  %3d", DStr1, TStr1, Over).

na_vorige(Num,Cat, Tg1, Tg2, Tijd) :-    /* vindt het tijdstip waarop vorige ronde(s) */
  bitleft(Num,1,Num1),        /* klaar is (zijn)                           */
  Num2 = Num1 + 1,
  geplande_tijd(Num1,Cat,Tg1, Tijd1),
  geplande_tijd(Num2,Cat,Tg2, Tijd2),
  %geplande_gekoppeld(Num1, Cat, T1, Tijd3),
  %geplande_gekoppeld(Num2, Cat, T2, Tijd4),
  %grootste(Tijd3, Tijd4, TG),
  grootste(Tijd1,Tijd2,Tijd).
  %grootste(TG, TP, Tijd).

/*geplande_gekoppeld(Num, Cat, onbekend, Tijd) :-
  kopl(CatV, _, Cat, plaats(Num)),	% koppeling (niet uitgevoerd)
  pos(CatV, _, _, poel), !,		% naar poule?
  findall(T, w2(_,CatV,T,_,_,_,_,_), TL),
  sortint_c(TL),
  reverse(TL, [], TLR),
  TLR = [Tijd | _].
geplande_gekoppeld(Num, Cat, openplaats(X), Tijd) :-
  kopl(CatV, _, Cat, plaats(X)),	% koppeling (niet uitgevoerd)
  pos(CatV, _, _, poel), !,		% naar poule?
  findall(T, w2(_,CatV,T,_,_,_,_,_), TL),
  sortint_c(TL),
  reverse(TL, [], TLR),
  TLR = [Tijd | _].
geplande_gekoppeld(_, _, _, 0).*/
  
geplande_tijd(Num,Cat,_, 0) :-          /* vorige wedstrijd al gespeeld      */
  w3(Num,Cat,_,_,_,_,_), !.
geplande_tijd(Num,Cat,_,Tijd) :-       /* vorige wedstrijd gepland          */
  w2(Num,Cat,Tijd,_,_,_,_,_), !.
geplande_tijd(Num, Cat, _, Tijd) :-
  kopl(CatV, _, Cat, plaats(Num)),	% koppeling (niet uitgevoerd)
  pos(CatV, _, _, poel),		% naar poule?
  findall(T, w2(_,CatV,T,_,_,_,_,_), TL),
  sortint_c(TL),
  reverse(TL, [], TLR),
  TLR = [Tijd | _], !.
geplande_tijd(_Num, Cat, openplaats(X), Tijd) :-
  kopl(CatV, _, Cat, plaats(X)),	% koppeling (niet uitgevoerd)
  pos(CatV, _, _, poel), 		% naar poule?
  findall(T, w2(_,CatV,T,_,_,_,_,_), TL),
  sortint_c(TL),
  reverse(TL, [], TLR),
  TLR = [Tijd | _], !.
geplande_tijd(Num,Cat,_, Tijd) :-       /* kijk naar voor-vorige wedstrijden */
  wd(Num,Cat,T1,T2,_,_,_), !,
  na_vorige(Num,Cat,T1,T2,Tijd).
geplande_tijd(Num,Cat,_, 0) :-          /* vorige wedstijd is er niet        */
  not(wd(Num,Cat,_,_,_,_,_)).

/*pick(Num, [Tijd|_], Num, Tijd) :- !.
pick(Cur, [_|Rest], Num, Tijd) :-
  Cur1 = Cur + 1, 
  pick(Cur1, Rest, Num, Tijd).*/
  
plan_vorige(Win,Num,Cat,BeginDag,LogOpen,EindTijd) :-
  bitleft(Num,1,N1),            /* zoek de voorgaande                 */
  wd(N1,Cat,U1,U2,_,_,_),         /* kijk wie er meespelen              */
  plan_wedst_als(Win,enkelvoudig,N1,Cat,U1,U2,BeginDag,LogOPen,EindTijd),
  N2 = N1 + 1,
  wd(N2,Cat,U3,U4,_,_,_),         /* kijk wie er meespelen              */
  plan_wedst_als(Win,enkelvoudig,N2,Cat,U3,U4,BeginDag,LogOpen,EindTijd), ! .
plan_vorige(_, _, _, _, _, _).		% er was geen vorige ...

/*wtl2string([], [], []) :- !.  
wtl2string([H|Rest], [S|OutS], [T|OutT]) :-
  H = wt(S, T, _, _),
  wtl2string(Rest, OutS, OutT).*/

nietEigen(Num, Cat, Num, Cat) :- !, fail.
nietEigen(_,_,_,_).

baanPlan(Tijd, Num, Cat, BaanS) :-
  w2(Num1,Cat1,Tijd,_,_,_,_,BaanS),
  nietEigen(Num, Cat, Num1, Cat1).
  
baan_toewijzing(Tijd, BaanS2, Num, Cat) :-
  baan_adm(ja),
  bn(Tijd, Aantal),
  findall(BaanS,baanPlan(Tijd, Num, Cat,BaanS),Banen), % zoek ze vast bij elkaar..
  numb(Baan),
  str_int(BaanS1, Baan),
  not(member(BaanS1, Banen)),
  baan_toewijzing1(Baan, Aantal, BaanS1, BaanS2), !. 
baan_toewijzing(_, "", _, _).

baan_toewijzing1(Baan, Aantal, _, "?") :-
  Baan > Aantal, !.
baan_toewijzing1(_, _, In, In).
  
/*bezwaren(Pos,Cat,Tijd,Keus) :-	% Geef de bezwaren weer
  tijd_str(Tijd,TStr1),
  bitright(Tijd,7,Dag),
  dag(Dag,DStr1,_),
  write("Tijdstip: ",DStr1," ",TStr1),
  %	na_vorige(Pos,Cat,TijdVorige),
  separatie(Separatie),
  dag_max(DagMax),
  dag_max_o(DagMaxO),
  check_forceer_c(1, [Tijd], Pos, Cat, Separatie, DagMax, DagMaxO,
						 _, _Result),
  Ans = dlg_Ask("Bezwaren vervolgpartij","Forceren???",["&Wel","&Niet"]), % 0, 1
  write("\nAns= ",Ans),
  Keus = Ans + 2, !.
bezwaren(_,_,_,2) :-	% geen bezwaar gevonden
  sleep(10).*/

/*check_vorige(Result) :-
	bitright(Result, 7, Resultv),
	ResultV > 0, !,
	write("\n  Vorige ronde vermoedelijk nog niet afgelopen!").
check_vorige(_).*/


    
%BEGIN_DLG OnderdPlanMult
/**************************************************************************
	Creation and event handling for dialog: OnderdPlanMult
**************************************************************************/

constants

%BEGIN OnderdPlanMult, CreateParms, 20:46:47-3.10.2011, Code automatically updated!
  dlg_onderdplanmult_DlgType = wd_Modal
  dlg_onderdplanmult_Title = "Plan een of meer onderdelen"
  dlg_onderdplanmult_RCT = rct(40,32,348,226)
  dlg_onderdplanmult_Flags = [wsf_Close,wsf_TitleBar]
  dlg_onderdplanmult_Help = idh_planning
  dlg_onderdplanmult_Font = "Arial"
  dlg_onderdplanmult_FSize = 10
%END OnderdPlanMult, CreateParms

database - plan
  single aantal(integer)

predicates

  dlg_onderdplanmult_eh : EHANDLER
nondeterm  onderdRepr(wedscat, string)
  merkAfvalPoel(rsoort, string)
  check_selectie(WINDOW)
nondeterm onderdSel(slist, wedscat)
%nondeterm planTijd(integer Dag, string Tijd, integer TijdSec)
%  tijdstr2tijd(string, tijd)
  dagAt(integerlist Dagen, integer Dag, integer In, integer Index, integer DagAt)
  tijdAt(integerlist Tijden, integer Tijd, integer In, integer Uit)
procedure planMultEindMessage(integer I, integer N)
procedure wedstypMark(boolean E_D, wedstyp, boolean Mark)	% b_True = dubbels
procedure wedstypMarkP(rsoort, boolean Mark)			% b_True = poule
procedure check_EDbuttons(window)
determ indexOf(string, slist, integer, integer)
determ overblijvendeDagen(window DagWin, slist DagenList2)
procedure getRestdagen(slist , integer Index, integer Index, slist)
setperiode(window)
clauses

aantal(0).

planTijd(Dag, Str, TijdSec) :-
  findall(T, bn(T, _), TList),
  sorttijd_c(TList),
  member(Tijd, TList),
  bitright(Tijd, 7, Dag1),
  Dag = Dag1,
  tijd_str(Tijd, Str, _),
  bitand(Tijd, $7F, TijdSec).

dagAt([D|_], D1, I, I, D) :-
  D >= D1,  !.
dagAt([_|R], Dag, In, Uit, D) :-
  In1 = In + 1,
  dagAt(R, Dag, In1, Uit, D). 

tijdAt([T1|_],T, I, I) :- 
  T <= T1, !.
tijdAt([], _, I, I) :- !.
tijdAt([_|Tijden],T, In, Uit) :- !,
  In1 = In + 1,
  tijdAt(Tijden, T, In1, Uit).

check_selectie(Win) :-
  OndWin  = win_GetCtlHandle(Win, idcbo_onderdelen),
  DagWin  = win_GetCtlHandle(Win, idcbo_dag1),
  TijdWin = win_GetCtlHandle(Win, idcbo_uur1),
  %lbox_GetSelIndex(OndWin),
  lbox_GetSelIndex(DagWin),
  lbox_GetSelIndex(TijdWin), 
  lbox_GetSel(OndWin,_ItemList,_IndexList),
  not(_ItemList = []), !,
  dialog_SetState(Win, [enable(idc_ok, b_True)]).
check_selectie(Win) :-  
  dialog_SetState(Win, [enable(idc_ok, b_False)]).

merkAfvalPoel(poel, "p") :- !.
merkAfvalPoel(afval, " ").

onderdRepr(WC, Repr) :-
  wc(WC, _, _, Catname, _, _, _, _, _, _, _, _, _, _, _),
  %wc(WC,Catname,_,_,_,_,_),
  pos(WC,_,_,AfvalPoel),
  planbaar1(WC, _, b_True),
  merkAfvalPoel(AfvalPoel,Merk),
  format(Repr, "%s\t%s", Merk, Catname).

onderdSel(List, WC) :-
  member(Onderdeel, List),
  frontstr(2, Onderdeel, _, Catname),
  wc(WC, _, _, Catname, _, _, _, _, _, _, _, _, _, _, _).
  %wc(WC,Catname,_,_,_,_,_).

tijdstr2tijd(TijdS, Tijd) :-
  searchchar(TijdS,'.',Pos),
  P1 = Pos - 1, !,
  frontstr(P1, TijdS, UurS, _),
  str_int(UurS, Uur),
  bitleft(Uur, 2, Uur1),
  frontstr(Pos, TijdS, _, MinS),
  str_int(MinS, Min),
  roosterinterval(RI),
  Kwartieren = Min div RI,
  Tijd = Uur1 + Kwartieren.
tijdstr2tijd(TijdS, Tijd) :-
  searchchar(TijdS,':',Pos),
  P1 = Pos - 1,
  frontstr(P1, TijdS, UurS, _),
  str_int(UurS, Uur),
  bitleft(Uur, 2, Uur1),
  frontstr(Pos, TijdS, _, MinS),
  str_int(MinS, Min),
  roosterinterval(RI),
  Kwartieren = Min div RI,
  Tijd = Uur1 + Kwartieren.

planMultEindMessage(I, N) :-
  N > 0,
  format(Msg, "%u wedstrijden gepland.\n%u wedstrijden NIET gepland (te moeilijk?).\nVerander de baanbeschikbaarheid of planningvoorwaarden.\nZie ook \"Nog te plannen\" (F4)", I, N),
  dlg_MessageBox( "Planning", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), !.
planMultEindMessage(I, _N) :-
  format(Msg, "%u wedstrijden gepland.", I),
  dlg_MessageBox( "Planning", Msg, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), !.

wedstypMark(b_True, WedsTyp, b_True) :-	% b_True = dubbels
  not(WedsTyp = e), !.
wedstypMark(b_False, e, b_True) :- !.	% b_False = enkels
wedstypMark(_, _, b_False).

wedstypMarkP(poel, b_True) :- !.
wedstypMarkP(_, b_False).

check_EDbuttons(Win) :-
  DButton = win_GetCtlHandle(Win, idco_dubbels),
  win_SetState(DButton, [wsf_disabled]),
  getbackTrack(Here),
  onderdRepr(WC, _),
  wc(WC, _, _, _, d, _, _, _, _, _, _, _, _, _, _),
  %wc(WC,_Catname,d,_,_,_,_),
  cutbackTrack(Here),
  win_SetState(DButton, [wsf_enabled]),
  fail.
check_EDbuttons(Win) :-
  DButton = win_GetCtlHandle(Win, idco_dubbels),
  win_SetState(DButton, [wsf_disabled]),
  getbackTrack(Here),
  onderdRepr(WC, _),
  wc(WC, _, _, _, g, _, _, _, _, _, _, _, _, _, _),
  %wc(WC,_Catname,d,_,_,_,_),
  cutbackTrack(Here),
  win_SetState(DButton, [wsf_enabled]),
  fail.
check_EDbuttons(Win) :-
  DButton = win_GetCtlHandle(Win, idco_enkels),
  win_SetState(DButton, [wsf_disabled]),
  onderdRepr(WC, _),
  wc(WC, _, _, _, e, _, _, _, _, _, _, _, _, _, _),
  %wc(WC,_Catname,e,_,_,_,_),
  win_SetState(DButton, [wsf_enabled]), !.
check_EDbuttons(_Win).

indexOf(String, [String|_], In, In) :- !.
indexOf(String, [_|Slist], In, Uit) :-
  In1 = In + 1,
  indexOf(String, Slist, In1, Uit).

getRestdagen([Dag|Dagen], Index, Index, [Dag | Uit]) :-
  Loper = Index + 1, !,
  getRestdagen(Dagen, Index, Loper, Uit). 
getRestdagen([Dag|Dagen], Index, Loper, [Dag | Uit]) :-
  Loper > Index, !,
  getRestdagen(Dagen, Index, Loper, Uit). 
getRestdagen([_Dag|Dagen], Index, Loper, Uit) :-
  Loper1 = Loper + 1, !,
  getRestdagen(Dagen, Index, Loper1,  Uit).
getRestdagen(_, _Index, _Loper, []).

overblijvendeDagen(DagWin, RestDagen) :-
  LboxList = lbox_GetAll(DagWin),
  Index = lbox_GetSelIndex(DagWin),
  getRestDagen(LboxList, Index, 0, RestDagen).
  
setPeriode(_Win) :-
  LboxWinD = win_GetCtlHandle(_Win, idcbo_dag1),
  lbox_GetSel(LboxWinD, ItemListD, _IndexListD),			% get dag
  ItemListD = [TextD|_],
  LboxWinT = win_GetCtlHandle(_Win, idcbo_uur1),
  lbox_GetSel(LboxWinT, ItemListT, _IndexListT),			% get tijden
  ItemListT = [TijdStr|_],
  LboxWinD2 = win_GetCtlHandle(_Win, idcbo_dag2),
  lbox_GetSel(LboxWinD2, ItemListD2, _IndexListD2),			% get dag
  ItemListD2 = [TextD2|_],
  format(TextT, "Vanaf %s %s t/m %s, daarbuiten wordt beschouwd als mislukt!", TextD, TijdStr,TextD2),
  dialog_SetStr(_Win, idcto_begintijdstip, TextT).

  
  
  dlg_onderdplanmult_Create(Parent):-
	win_CreateDynDialog(Parent,
		[
%BEGIN OnderdPlanMult, WinDefList, 20:46:47-3.10.2011, Code automatically updated!
		 dlg_font(wdef(dlg_onderdplanmult_DlgType,dlg_onderdplanmult_RCT,dlg_onderdplanmult_Title,u_DlgBase),
		 	  dlg_onderdplanmult_Font,dlg_onderdplanmult_FSize,dlg_onderdplanmult_Flags),
		 ctl(wdef(wc_LBox,rct(3,12,170,158),"",u_DlgBase),idcbo_onderdelen,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_MultiSelect,wsf_UseTabStops]),
		 ctl(wdef(wc_LBox,rct(173,12,221,123),"",u_DlgBase),idcbo_dag1,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight]),
		 ctl(wdef(wc_LBox,rct(223,12,253,123),"",u_DlgBase),idcbo_uur1,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight]),
		 ctl(wdef(wc_LBox,rct(258,12,306,123),"",u_DlgBase),idcbo_dag2,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight]),
		 ctl(wdef(wc_PushButton,rct(233,158,301,180),"&Plan",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(3,159,35,170),"Geen",u_DlgBase),idco_geen,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(37,159,69,170),"&Alle",u_DlgBase),idco_onderdselmult_alle,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(71,159,102,170),"&Enkels",u_DlgBase),idco_enkels,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(104,159,136,170),"&Dubbels",u_DlgBase),idco_dubbels,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(138,159,170,170),"&Poules",u_DlgBase),idco_onderdplanmult_poules,[wsf_Group,wsf_TabStop]),
		 customctl(wdef(wc_Custom,rct(172,155,227,172),"Progress",u_DlgBase),"progress",idco_progress,[wsf_Group,wsf_TabStop,wsf_Invisible]),
		 ctl(wdef(wc_LBox,rct(308,12,338,123),"",u_DlgBase),idcbo_uur2,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight]),
		 ctl(wdef(wc_PushButton,rct(3,177,78,190),"Planingvoorwaarden",u_DlgBase),idco_planingvoorwaarden,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(95,177,170,190),"Baanbeschikbaarheid",u_DlgBase),idco_baanbeschikbaarheid,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(193,179,225,189),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_Text,rct(2,3,116,11),"Selecteer te plannen onderdelen",u_DlgBase),idcto_selecteer_te_plannen_onderdelen,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(192,3,242,11),"Plannen vanaf",u_DlgBase),idcto_begindag_en_tijd,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(174,125,306,154),"",u_DlgBase),idcto_begintijdstip,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(259,3,304,11),"Plannen t/m",u_DlgBase),idco_onderdplanmult_plantot,[wsf_AlignCenter])
%END OnderdPlanMult, WinDefList
		],dlg_onderdplanmult_eh,0).

%BEGIN OnderdPlanMult, idc_ok _CtlInfo
  dlg_onderdplanmult_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	LboxWinD = win_GetCtlHandle(_Win, idcbo_dag1),
	lbox_GetSel(LboxWinD, ItemListD, _IndexListD),			% get dag
	ItemListD = [TextD|_],
	dag(D1,TextD,_),
	bitleft(D1, 7, D2), 
	LboxWinT = win_GetCtlHandle(_Win, idcbo_uur1),
	lbox_GetSel(LboxWinT, ItemListT, _IndexListT),			% get tijden
	ItemListT = [TijdStr|_],
	LboxWinDL = win_GetCtlHandle(_Win, idcbo_dag2),
	lbox_GetSel(LboxWinDL, ItemListDL, _IndexListDL),			% get dag
	ItemListDL = [TextDL|_],
	dag(DL,TextDL,_),
	DL1 = DL + 1,
	bitleft(DL1, 7, EindTijd),
	%format(TextT, "Vanaf %s %s", TextD, TijdStr),
	%dialog_SetStr(_Win, idcto_begintijdstip, TextT),
	tijdstr2tijd(TijdStr,TijdSel),
	BeginTijd = D2 + TijdSel,
	%bitleft(7, 7, OptimTijd),
	%Eind = BeginTijd + OptimTijd,
	PWin = win_GetCtlHandle(_Win, idco_progress),
	dialog_SetState(_Win,[show(idco_progress,b_True)]),
	OndWin = win_GetCtlHandle(_Win, idcbo_onderdelen),
	lbox_GetSel(OndWin,ItemListO,_IndexListO),
	findall(WC, onderdSel(ItemListO, WC), NewCatL),
	assert(aantal(0)),
	assert(reedsError(0)),		% 0 = nog niet gepland
	sortwed_c3(0, NewCatL, 0, Num, Cat, _, Aantal),% alle nog niet geplande
	retractall(spelerdrukte(_,_,_,_,_,_,_,_,_)),	% hoort er bij
	ParentWin = win_GetParent(_Win),
	plan_wd0(ParentWin, enkelvoudig,Num,Cat,BeginTijd,EindTijd),
	aantal(I),
	I1 = I + 1,
	progress_bar_set_value(PWin, Aantal, I1),
	vpi_ProcessEvents(b_True ),
	assert(aantal(I1)),
	fail.
  dlg_onderdplanmult_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	logclose(),
	aantal(I),
	I > 3,
	compress(update),
	startDBrefresh(),
	fail.
  dlg_onderdplanmult_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	update_display(),
	update_waarsch(),
	update_uitslagen("tasplan ondplanmult"),
	update_proj_toolbar(),
	aantal(I),
	reedsError(N),
	planMultEindMessage(I, N),
	win_Destroy(_Win),
 !.
%END OnderdPlanMult, idc_ok _CtlInfo
%MARK OnderdPlanMult, new events

%BEGIN OnderdPlanMult, idco_baanbeschikbaarheid _CtlInfo
  dlg_onderdplanmult_eh(_Win,e_Control(idco_baanbeschikbaarheid,_CtrlType,_CtrlWin,_CtlInfo),0):-
	dlg_baanbeschikbaarheid_Create(_Win),
	!.
%END OnderdPlanMult, idco_baanbeschikbaarheid _CtlInfo

%BEGIN OnderdPlanMult, idco_planingvoorwaarden _CtlInfo
  dlg_onderdplanmult_eh(_Win,e_Control(idco_planingvoorwaarden,_CtrlType,_CtrlWin,_CtlInfo),0):-
	dlg_toernooibeleid_Create(_Win),
	!.
%END OnderdPlanMult, idco_planingvoorwaarden _CtlInfo

%BEGIN OnderdPlanMult, idcbo_dag2 selchanged
  dlg_onderdplanmult_eh(_Win,e_Control(idcbo_dag2,_CtrlType,_CtrlWin,selchanged),0):-!,
	setPeriode(_Win),
	!.
%END OnderdPlanMult, idcbo_dag2 selchanged

%BEGIN OnderdPlanMult, idco_onderdplanmult_poules _CtlInfo
  dlg_onderdplanmult_eh(_Win,e_Control(idco_onderdplanmult_poules,_CtrlType,_CtrlWin,_CtlInfo),0):-
	ListWin = win_GetCtlHandle(_Win, idcbo_onderdelen),
	LboxList = lbox_GetAll(ListWin),
	member(Item, LboxList),
	indexOf(Item, LboxList, 0, Index),
	onderdRepr(WC, Repr),
	Item = Repr,
	wc(WC, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
	%wc(WC,_Catname,_WedsTyp,_,_,_,_),
	pos(WC, _, _, Poel),
	wedstypMarkP(Poel, Mark),
	lbox_SetSel(ListWin, Index, Mark),
	fail.
  dlg_onderdplanmult_eh(_Win,e_Control(idco_onderdplanmult_poules,_CtrlType,_CtrlWin,_CtlInfo),0):-
	check_Selectie(_Win),
	!.
%END OnderdPlanMult, idco_onderdplanmult_poules _CtlInfo

%BEGIN OnderdPlanMult, idco_enkels _CtlInfo
  dlg_onderdplanmult_eh(_Win,e_Control(idco_enkels,_CtrlType,_CtrlWin,_CtlInfo),0):-
	ListWin = win_GetCtlHandle(_Win, idcbo_onderdelen),
	LboxList = lbox_GetAll(ListWin),
	member(Item, LboxList),
	indexOf(Item, LboxList, 0, Index),
	onderdRepr(WC, Repr),
	Item = Repr,
	wc(WC, _, _, _, WedsTyp, _, _, _, _, _, _, _, _, _, _),
	%wc(WC,_Catname,WedsTyp,_,_,_,_),
	wedstypMark(b_False, WedsTyp, Mark),
	lbox_SetSel(ListWin, Index, Mark),
	fail.
  dlg_onderdplanmult_eh(_Win,e_Control(idco_enkels,_CtrlType,_CtrlWin,_CtlInfo),0):-
	check_Selectie(_Win),
	!.
%END OnderdPlanMult, idco_enkels _CtlInfo

%BEGIN OnderdPlanMult, idco_dubbels _CtlInfo
  dlg_onderdplanmult_eh(_Win,e_Control(idco_dubbels,_CtrlType,_CtrlWin,_CtlInfo),0):-
	ListWin = win_GetCtlHandle(_Win, idcbo_onderdelen),
	LboxList = lbox_GetAll(ListWin),
	member(Item, LboxList),
	indexOf(Item, LboxList, 0, Index),
	onderdRepr(WC, Repr),
	Item = Repr,
	wc(WC, _, _, _, WedsTyp, _, _, _, _, _, _, _, _, _, _),
	%wc(WC,_Catname,WedsTyp,_,_,_,_),
	wedstypMark(b_True, WedsTyp, Mark),
	lbox_SetSel(ListWin, Index, Mark),
	fail.
  dlg_onderdplanmult_eh(_Win,e_Control(idco_dubbels,_CtrlType,_CtrlWin,_CtlInfo),0):-
	check_Selectie(_Win),
	!.
%END OnderdPlanMult, idco_dubbels _CtlInfo

%BEGIN OnderdPlanMult, idc_help _CtlInfo
  dlg_onderdplanmult_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Planning"),
	!.
%END OnderdPlanMult, idc_help _CtlInfo

%BEGIN OnderdPlanMult, idco_geen _CtlInfo
  dlg_onderdplanmult_eh(_Win,e_Control(idco_geen,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	ListWin = win_GetCtlHandle(_Win, idcbo_onderdelen),
	selectGeen(ListWin),
	check_selectie(_Win),
	!.
%END OnderdPlanMult, idco_geen _CtlInfo

%BEGIN OnderdPlanMult, idcbo_uur1 selchanged
  dlg_onderdplanmult_eh(_Win,e_Control(idcbo_uur1,_CtrlType,_LboxWinT,selchanged),0):-!,
	check_selectie(_Win),
	setPeriode(_Win),
	!.
%END OnderdPlanMult, idcbo_uur1 selchanged

%BEGIN OnderdPlanMult, idcbo_dag1 selchanged
  dlg_onderdplanmult_eh(_Win,e_Control(idcbo_dag1,_CtrlType,LboxWinD,selchanged),0):-!,
	lbox_GetSel(LboxWinD, ItemListD, _IndexListD),			% get dag
	ItemListD = [TextD|_],
	dag(D1,TextD,_),
	findall(TijdS, planTijd(D1, TijdS, _), TijdenS),	% kwartieren afhankelijk van de dag
	LboxWinT = win_GetCtlHandle(_Win, idcbo_uur1),
	lbox_Clear(LboxWinT),
	lbox_Add(LboxWinT, TijdenS),
	check_selectie(_Win),
	!.
%END OnderdPlanMult, idcbo_dag1 selchanged

%BEGIN OnderdPlanMult, idco_onderdselmult_alle _CtlInfo
  dlg_onderdplanmult_eh(_Win,e_Control(idco_onderdselmult_alle,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	OndWin = win_GetCtlHandle(_Win, idcbo_onderdelen),
	selectAlle(OndWin),
	check_selectie(_Win),
	!.
%END OnderdPlanMult, idco_onderdselmult_alle _CtlInfo

%BEGIN OnderdPlanMult, idcbo_onderdelen selchanged
  dlg_onderdplanmult_eh(_Win,e_Control(idcbo_onderdelen,_CtrlType,_CtrlWin,selchanged),0):-!,
	check_selectie(_Win),
	!.
%END OnderdPlanMult, idcbo_onderdelen selchanged

%BEGIN OnderdPlanMult, idc_cancel _CtlInfo
  dlg_onderdplanmult_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	win_Destroy(_Win),
	!.
%END OnderdPlanMult, idc_cancel _CtlInfo

%BEGIN OnderdPlanMult, e_Create
  dlg_onderdplanmult_eh(_Win,e_Create(_CreationData),0):-!,
	OndWin = win_GetCtlHandle(_Win, idcbo_onderdelen),
	DagWin = win_GetCtlHandle(_Win, idcbo_dag1),
	%TijdWin = win_GetCtlHandle(_Win, idcbo_uur1),
	lbox_SetTabStops(OndWin, [10, 20, 100]),
	findall(Repr, onderdRepr(_, Repr), OnderdList),
	lbox_Add(OndWin, OnderdList),
	Font = font_Create(ff_Fixed, [], 10),
	win_SetFont(DagWin, Font),
	findall(DagN, dag(_, DagN, _), DagenList),
	lbox_Add(DagWin, DagenList),
	huidig(DagNo, Tijd),
	findall(DagNox, dag(DagNox, _, _), DagNoList),
	dagAt(DagNoList, DagNo, 0, IndexD, DagNoX),
	lbox_SetSel(DagWin, IndexD, b_True),
	findall(TijdS, planTijd(DagNoX, TijdS, _), TijdenS),	  % kwartieren afhankelijk van de dag
	LboxWinT = win_GetCtlHandle(_Win, idcbo_uur1),
	lbox_Clear(LboxWinT),
	lbox_Add(LboxWinT, TijdenS),
	%tijd_str(Tijd, Str),
	findall(TijdSec, planTijd(DagNoX, _, TijdSec), TijdenSec), % om de index te bepalen
	tijdAt(TijdenSec, Tijd, 0, IndexT),
	lbox_SetSel(LboxWinT, IndexT, b_True),
	PWin = win_GetCtlHandle(_Win, idco_progress),
	progress_bar_set_value(PWin, 0, 0),
	check_selectie(_Win),
	check_EDbuttons(_Win),
	% nieuw..	vanaf 27.9.2011
	DagWin2  = win_GetCtlHandle(_Win, idcbo_dag2),
	win_SetFont(DagWin2, Font),
	overblijvendeDagen(DagWin, DagenList2),
	lbox_Add(DagWin2, DagenList2),
	reverse(DagenList, [], DRev),
	DRev = [Laatste|_],
	indexof(Laatste, DagenList2, 0, Index),
	lbox_SetSel(DagWin2, Index, b_True),
	setPeriode(_Win),
	
	TijdWin2 = win_GetCtlHandle(_Win, idcbo_uur2),
	lbox_Add(TijdWin2, TijdenS),
	!.
%END OnderdPlanMult, e_Create

  dlg_onderdplanmult_eh(_,_,_):-!,fail.

%END_DLG OnderdPlanMult


%BEGIN_DLG OnderdPlanMultWis
/**************************************************************************
	Creation and event handling for dialog: OnderdPlanMultWis
**************************************************************************/

constants

%BEGIN OnderdPlanMultWis, CreateParms, 19:53:35-9.2.2013, Code automatically updated!
  dlg_onderdplanmultwis_DlgType = wd_Modal
  dlg_onderdplanmultwis_Title = "Wissen van planning in een periode"
  dlg_onderdplanmultwis_RCT = rct(40,32,321,204)
  dlg_onderdplanmultwis_Flags = [wsf_Close,wsf_TitleBar]
  dlg_onderdplanmultwis_Help = idh_planning_wissen
  dlg_onderdplanmultwis_Font = "Arial"
  dlg_onderdplanmultwis_FSize = 10
%END OnderdPlanMultWis, CreateParms
 
database - wissen
single progr(integer)
single aantgewis(integer)

predicates

  dlg_onderdplanmultwis_eh : EHANDLER
  check_ond(WINDOW)
  begintext(WINDOW, tijd, tijd)
  %eindText(WINDOW, tijd)
  volgBegin(WINDOW)
  %check_begin(WINDOW)
  %check_eind(WINDOW)
  wis_planning(WINDOW, tijd, tijd, catl, BOOLEAN InclFix, WINDOW PWin, integer Aantal)
  nondeterm onderdReprPl(string)
  items2WC(slist, catl, catl)
  nondeterm dagMetPlanning(string, catl)
  nondeterm kwartlistW(integer Dag, catl, string)
  qText(BOOLEAN, string)

clauses

progr(0).
aantgewis(0).

dagMetPlanning(DagRepr, CatL) :-
  dag(Dag, DagRepr, _),
  getbacktrack(Here),
  member(WC, CatL),
  w2(_,WC,Tijd,_,_,_,_,_),
  bitright(Tijd, 7, Dag1),
  Dag = Dag1,
  cutbacktrack(Here).
  
kwartlistW(DagNo, CatL, Kwart) :-			% vulling voor kwartselectie
  findall(Tijd, bn(Tijd, _), Tijden),
  sorttijd_c(Tijden),
  member(T1, Tijden),
  bitright(T1, 7, Dag1),
  DagNo = Dag1,
  getbacktrack(Here),
  member(WC, CatL),
  w2(_,WC,T1,_,_,_,_,_),
  cutbacktrack(Here),
  tijd_str(T1, Kwart,_).

items2WC([], In, In) :- !.
items2WC([Item|Items], In, [WC|Out]) :-
  frontstr(2, Item, _, WCStr),
  wc(WC, _, _, WCStr, _, _, _, _, _, _, _, _, _, _, _), !,
  %wc(WC,WCStr,_EDG,_Kort,_VoorWie,_Geld,_Verl), !,
  items2WC(Items, In, Out).

onderdReprPl(Repr) :-
  onderdRepr(WC, Repr),
  getbacktrack(Here),
  w2(_,WC,_Tijd,_,_,_,_,_),
  cutbacktrack(Here).

wis_planning(_Win,BegTijd,EindTijd,CatList,b_True,PWin,Aantal) :-
  member(Cat, CatList),
  progr(N),
  N1 = N + 1,
  assert(progr(N1)),
  progress_bar_set_value(PWin, Aantal, N1),
  vpi_ProcessEvents(b_True ),
  w2(No,Cat,Tijd1,_,_,_,_,_),
  Tijd1 >= BegTijd,
  Tijd1 <= EindTijd,
  aantgewis(I),
  I1 = I + 1,
  assert(aantgewis(I1)),
  w2(No,Cat,A1,A2,A3,A4,A5,A6),	 	%  wis alle planning
  logf('R',w2(No,Cat,A1,A2,A3,A4,A5,A6),ja),
  wd(No, Cat, Tg1, Tg2, _, _, _), 
  afz(A2, No, Cat, A1, Tg1, ja),
  afz(A3, No, Cat, A1, Tg2, ja),
  fail.
wis_planning(_Win,BegTijd,EindTijd,CatList,b_False,PWin,Aantal) :-
  member(Cat, CatList),
  progr(N),
  N1 = N + 1,
  assert(progr(N1)),
  progress_bar_set_value(PWin, Aantal, N1),
  vpi_ProcessEvents(b_True ),
  w2(No,Cat,Tijd,_,_,' ',_,_),
  Tijd >= BegTijd,
  Tijd <= EindTijd,
  aantgewis(I),
  I1 = I + 1,
  assert(aantgewis(I1)),
  w2(No,Cat,A1,A2,A3,A4,A5,A6),		%  wis alleen niet gefixeerde planning
  logf('R',w2(No,Cat,A1,A2,A3,A4,A5,A6),ja),
  wd(No, Cat, Tg1, Tg2, _, _, _), 
  afz(A2, No, Cat, A1, Tg1, ja),
  afz(A3, No, Cat, A1, Tg2, ja),
  fail.
wis_planning(_Win,_BegTijd,_EindTijd,_CatList,_,_,_) :-
  logclose(),
  update_waarsch(),
  %member(CatP, CatList),
  %select(CatP),
  fail.
wis_planning(_Win,BegTijd,EindTijd,CatList,_,_,_) :-
  member(CatP, CatList),
  w2(_,CatP,Tijd,_,_,'f',_,_),
  Tijd >= BegTijd,
  Tijd <= EindTijd,
  update_display(),
  update_uitslagen("tasplan wis_planningx"),
  update_waarsch(),
  aantgewis(I),
  format(Msg, "% geplande wedstrijden gewist,\ngefixeerde wedstrijden zijn niet meegewist.",I),
  dlg_Note("Wissen",Msg), !.
wis_planning(_, _, _, _, _, _, _) :-
  update_display(),
  update_uitslagen("tasplan wis_planning"),
  update_waarsch(),
  aantgewis(I),
  format(Msg, "% geplande wedstrijden gewist.",I),
  dlg_Note("Wissen", Msg), !.

beginText(Win, Tijd11, Tijd21) :-
  Dag1Win = win_GetCtlHandle(Win, idcbo_dag1),
  lbox_GetSel(Dag1Win,Item1List,_Index1List),
  Item1List = [Dag1Sel|_],
  dag(DagNo1,Dag1Sel,_),
  Uur1Win = win_GetCtlHandle(Win, idcbo_uur1),
  lbox_GetSel(Uur1Win,KwartSList1,_KwartList1),
  KwartSList1 = [Kwart1|_],
  tijdstr2tijd(Kwart1, Tijd1), 
  bitleft(DagNo1, 7, DagNum1),
  Tijd11 = DagNum1 + Tijd1,
  tijd_str(Tijd11, Str1,_),
  format(Text1, "Van %s %s tot en met ......",Dag1Sel, Str1), 
  dialog_SetStr(Win, idcto_begintijdstip, Text1), 
  Dag2Win = win_GetCtlHandle(Win, idcbo_dag2),
  lbox_GetSel(Dag2Win,Item2List,_Index2List),
  Item2List = [Dag2Sel|_],
  dag(DagNo2,Dag2Sel,_),
  Uur2Win = win_GetCtlHandle(Win, idcbo_uurtot),
  lbox_GetSel(Uur2Win,KwartSList2,_KwartList2),
  KwartSList2 = [Kwart2|_],
  tijdstr2tijd(Kwart2, Tijd2),
  bitleft(DagNo2, 7, DagNum2),
  Tijd21 = DagNum2 + Tijd2,
  tijd_str(Tijd21, Str2,_),
  format(Text, "Van %s %s tot en met %s %s.",Dag1Sel, Str1, Dag2Sel, Str2), 
  dialog_SetStr(Win, idcto_begintijdstip, Text), !.
beginText(_Win, 0, 0).
  
volgBegin(Win) :-
  Dag1Win = win_GetCtlHandle(Win, idcbo_dag1),
  Dag2Win = win_GetCtlHandle(Win, idcbo_dag2),
  lbox_GetSel(Dag1Win,_Item1List,Index1List),
  Index1List = [Index1|_],
  lbox_GetSel(Dag2Win,_Item2List,Index2List),
  Index2List = [Index2|_],
  Index2 > Index1, !.				% dagindex2 > dagindex1: niets aan de hand
volgBegin(Win) :-
  Dag1Win = win_GetCtlHandle(Win, idcbo_dag1),
  Dag2Win = win_GetCtlHandle(Win, idcbo_dag2),
  lbox_GetSel(Dag1Win,_Item1List,Index1List),
  Index1List = [Index1|_],
  lbox_GetSel(Dag2Win,_Item2List,Index2List),
  Index2List = [Index2|_],
  Index2 = Index1, !,				% dagindex2 = dagindex1: check uurindex
  Uur1Win = win_GetCtlHandle(Win, idcbo_uur1),
  Uur2Win = win_GetCtlHandle(Win, idcbo_uurtot),
  lbox_GetSel(Uur1Win,_Kwart1List,IndexU1List),
  IndexU1List = [IndexU1|_],
  lbox_GetSel(Uur2Win,_Kwart2List,IndexU2List),
  IndexU2List = [IndexU2|_],
  grootste(IndexU1, IndexU2, Grootste),
  lbox_SetSel(Uur2Win, Grootste, b_True). 
volgBegin(Win) :-
  Dag1Win = win_GetCtlHandle(Win, idcbo_dag1),
  Dag2Win = win_GetCtlHandle(Win, idcbo_dag2),
  lbox_GetSel(Dag1Win,_Item1List,Index1List),
  Index1List = [Index1|_],
  lbox_SetSel(Dag2Win, Index1, b_True),		% maak begindag en einddag minstens gelijk 
  Uur1Win = win_GetCtlHandle(Win, idcbo_uur1),
  Uur2Win = win_GetCtlHandle(Win, idcbo_uurtot),
  % update uur2win
  OndWin = win_GetCtlHandle(Win, idcbo_onderdelen),
  lbox_GetSel(OndWin,ItemList,_IndexList),
  items2WC(ItemList, [], WCList),
  lbox_GetSel(Dag2Win, DagSel2List,_IndexListD2),
  DagSel2List = [DagS2|_],
  dag(Dag2, DagS2, _),
  findall(Kw2, KwartlistW(Dag2, WCList, Kw2), KwartierenList2),
  lbox_Clear(Uur2Win),
  lbox_Add(Uur2Win,  KwartierenList2),
  lbox_GetSel(Uur1Win,_Kwart1List,IndexU1List),
  IndexU1List = [IndexU1|_], 
  lbox_SetSel(Uur2Win, IndexU1, b_True), !. 
volgBegin(_Win).

check_ond(Win) :-
  OndWin = win_GetCtlHandle(Win, idcbo_onderdelen),
  lbox_GetSel(OndWin,_ItemList,IndexList),
  IndexList = [_|_],
  dialog_SetState(Win, [enable(idcbo_dag1,b_True), show(idcbo_dag1,b_True),
  			enable(idcbo_dag2,b_True), show(idcbo_dag2,b_True),
  			enable(idcbo_uur1,b_True), show(idcbo_uur1,b_True),
  			enable(idcbo_uurtot,b_True), show(idcbo_uurtot,b_True),
  			show(idcto_begintijdstip,b_True), 
  			show(idcto_begindag_en_tijd,b_True),show(idct_totdag,b_True)
  			]) , !.
check_ond(Win) :-
  dialog_SetState(Win, [enable(idcbo_dag1,b_False), show(idcbo_dag1,b_False),
  			enable(idcbo_dag2,b_False), show(idcbo_dag2,b_False),
  			enable(idcbo_uur1,b_False), show(idcbo_uur1,b_False),
  			enable(idcbo_uurtot,b_False), show(idcbo_uurtot,b_False),
  			show(idcto_begintijdstip,b_False), 
  			show(idcto_begindag_en_tijd,b_False),show(idct_totdag,b_False)
  			]) , !.

qText(b_True, "Incl. gefixeerden wissen?") :- !.
qText(_, "Planning wissen?").
  
  dlg_onderdplanmultwis_Create(Parent):-
	win_CreateDynDialog(Parent,
		[
%BEGIN OnderdPlanMultWis, WinDefList, 19:53:35-9.2.2013, Code automatically updated!
		 dlg_font(wdef(dlg_onderdplanmultwis_DlgType,dlg_onderdplanmultwis_RCT,dlg_onderdplanmultwis_Title,u_DlgBase),
		 	  dlg_onderdplanmultwis_Font,dlg_onderdplanmultwis_FSize,dlg_onderdplanmultwis_Flags),
		 ctl(wdef(wc_LBox,rct(2,9,122,160),"",u_DlgBase),idcbo_onderdelen,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_MultiSelect,wsf_UseTabStops]),
		 ctl(wdef(wc_PushButton,rct(6,161,34,171),"&Alle",u_DlgBase),idco_onderdselmult_alle,[wsf_Default,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(46,161,75,171),"&Geen",u_DlgBase),idco_geen,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_LBox,rct(126,9,170,107),"",u_DlgBase),idcbo_dag1,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_Disabled,wsf_Invisible]),
		 ctl(wdef(wc_LBox,rct(173,9,199,107),"",u_DlgBase),idcbo_uur1,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_Disabled,wsf_Invisible]),
		 ctl(wdef(wc_LBox,rct(205,9,249,107),"",u_DlgBase),idcbo_dag2,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_Disabled,wsf_Invisible]),
		 ctl(wdef(wc_LBox,rct(252,9,278,107),"",u_DlgBase),idcbo_uurtot,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_Disabled,wsf_Invisible]),
		 ctl(wdef(wc_CheckBox,rct(127,109,262,119),"&Gefixeerde wedstrijden ook wissen",u_DlgBase),idcbo_inclusief_gefixeerden,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_PushButton,rct(243,160,275,172),"&OK",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(205,161,237,171),"&Cancel",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(167,161,199,171),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop]),
		 customctl(wdef(wc_Custom,rct(81,161,149,171),"Progress",u_DlgBase),"progress",idcbo_progress,[wsf_Group,wsf_TabStop,wsf_Invisible]),
		 ctl(wdef(wc_Text,rct(2,1,80,9),"Selecteer onderdelen",u_DlgBase),idcto_select_tewissen_periode,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(146,1,191,9),"Vanaf",u_DlgBase),idcto_begindag_en_tijd,[wsf_AlignCenter,wsf_Invisible]),
		 ctl(wdef(wc_Text,rct(125,140,280,158),"begintijdstip",u_DlgBase),idcto_begintijdstip,[wsf_AlignLeft,wsf_Invisible]),
		 ctl(wdef(wc_Text,rct(227,1,269,9),"Tot en met",u_DlgBase),idct_totdag,[wsf_AlignCenter,wsf_Invisible])
%END OnderdPlanMultWis, WinDefList
		],dlg_onderdplanmultwis_eh,0).

%BEGIN OnderdPlanMultWis, idc_ok _CtlInfo
  dlg_onderdplanmultwis_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-!,
	beginText(_Win, Beg, Eind),
	%eindText(_Win, Eind),
	CheckBox = win_GetCtlHandle(_Win, idcbo_inclusief_gefixeerden),
	InclFix = win_IsChecked(CheckBox),
	qText(InclFix, Tekst),
	Ans = dlg_MessageBox( "Planning", Tekst, mesbox_iconquestion, mesbox_buttonsokcancel, mesbox_defaultsecond, mesbox_suspendapplication ),
	Ans = 1,
	OndWin = win_GetCtlHandle(_Win, idcbo_onderdelen),
	lbox_GetSel(OndWin,ItemList,_IndexList),
	count(ItemList, 0, AantalWC),
	items2WC(ItemList, [], WCList),
	ParentWin = win_GetParent(_Win),
	PWin = win_GetCtlHandle(_Win, idcbo_progress),
	dialog_SetState(_Win, [show(idcbo_progress,b_True)]),
	assert(progr(0)),
	assert(aantgewis(0)),
	wis_planning(ParentWin, Beg, Eind, WCList, InclFix, PWin, AantalWC),
        update_proj_toolbar(),
        win_Destroy(_Win),
	!.
%END OnderdPlanMultWis, idc_ok _CtlInfo
%MARK OnderdPlanMultWis, new events

%BEGIN OnderdPlanMultWis, idc_help _CtlInfo
  dlg_onderdplanmultwis_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Planningwissen"),
	!.
%END OnderdPlanMultWis, idc_help _CtlInfo

%BEGIN OnderdPlanMultWis, idc_cancel _CtlInfo
  dlg_onderdplanmultwis_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-
	win_Destroy(_Win),
	!.
%END OnderdPlanMultWis, idc_cancel _CtlInfo


%BEGIN OnderdPlanMultWis, idcbo_uurtot selchanged
  dlg_onderdplanmultwis_eh(_Win,e_Control(idcbo_uurtot,_CtrlType,_CtrlWin,selchanged),0):-!,
	dialog_SetState(_Win, [enable(idc_ok, b_True)]),
	volgBegin(_Win),
	beginText(_Win,_, _),
	!.
%END OnderdPlanMultWis, idcbo_uurtot selchanged

%BEGIN OnderdPlanMultWis, idcbo_dag2 selchanged
  dlg_onderdplanmultwis_eh(_Win,e_Control(idcbo_dag2,_CtrlType,DagWin2,selchanged),0):-!,
	OndWin = win_GetCtlHandle(_Win, idcbo_onderdelen),
	lbox_GetSel(OndWin,ItemList,_IndexList),
	items2WC(ItemList, [], WCList),
	TijdWin2  = win_GetCtlHandle(_Win, idcbo_uurtot),
	lbox_GetSel(DagWin2, DagSel2List,_IndexListD2),
	DagSel2List = [DagS2|_],
	dag(Dag2, DagS2, _),
	findall(Kw2, KwartlistW(Dag2, WCList, Kw2), KwartierenList2),
	lbox_Clear(TijdWin2),
	lbox_Add(TijdWin2,  KwartierenList2),
	lbox_SetSel(TijdWin2, 0, b_True),
	volgBegin(_Win),
	beginText(_Win,_, _),
	dialog_SetState(_Win, [enable(idc_ok, b_True)]),
	!.
%END OnderdPlanMultWis, idcbo_dag2 selchanged


%BEGIN OnderdPlanMultWis, idco_geen _CtlInfo
  dlg_onderdplanmultwis_eh(_Win,e_Control(idco_geen,_CtrlType,_CtrlWin,_CtlInfo),0):-
	OndWin = win_GetCtlHandle(_Win, idcbo_onderdelen),
	selectGeen(OndWin),	
	check_ond(_Win),
	!.
%END OnderdPlanMultWis, idco_geen _CtlInfo

%BEGIN OnderdPlanMultWis, idcbo_uur1 selchanged
  dlg_onderdplanmultwis_eh(_Win,e_Control(idcbo_uur1,_CtrlType,_CtrlWin,selchanged),0):-!,
	volgBegin(_Win),
	beginText(_Win,_,_),
	check_ond(_Win),
	%eindText(_Win,_),
	!.
%END OnderdPlanMultWis, idcbo_uur1 selchanged

%BEGIN OnderdPlanMultWis, idcbo_dag1 selchanged
  dlg_onderdplanmultwis_eh(_Win,e_Control(idcbo_dag1,_CtrlType,DagWin1,selchanged),0):-!,
	OndWin = win_GetCtlHandle(_Win, idcbo_onderdelen),
	lbox_GetSel(OndWin,ItemList,_IndexList),
	items2WC(ItemList, [], WCList),
	TijdWin1  = win_GetCtlHandle(_Win, idcbo_uur1),
	lbox_GetSel(DagWin1, DagSel1List,_IndexListD1),
	DagSel1List = [DagS1|_],
	dag(Dag1, DagS1, _),
	findall(Kw1, KwartlistW(Dag1, WCList, Kw1), KwartierenList1),
	lbox_Clear(TijdWin1),
	lbox_Add(TijdWin1,  KwartierenList1),
	lbox_SetSel(TijdWin1, 0, b_True),
	volgBegin(_Win),
	beginText(_Win,_,_),
	%eindText(_Win,_),
	!.
%END OnderdPlanMultWis, idcbo_dag1 selchanged

%BEGIN OnderdPlanMultWis, idco_onderdselmult_alle _CtlInfo
  dlg_onderdplanmultwis_eh(_Win,e_Control(idco_onderdselmult_alle,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	OndWin = win_GetCtlHandle(_Win, idcbo_onderdelen),
	selectAlle(OndWin),
	%NoOfItems = lbox_CountAll(OndWin),
	%items2indexes(NoOfItems, SetIndexList),
	%dialog_SetListBox(_Win,idcbo_onderdelen,SetIndexList),
	DagWin1  = win_GetCtlHandle(_Win, idcbo_dag1),
	TijdWin1 = win_GetCtlHandle(_Win, idcbo_uur1),
	DagWin2  = win_GetCtlHandle(_Win, idcbo_dag2),
	TijdWin2 = win_GetCtlHandle(_Win, idcbo_uurtot),
	lbox_GetSel(OndWin,ItemList,_IndexList),
	items2WC(ItemList, [], WCList),
	findall(DagN, dagMetPlanning(DagN, WCList), DagenList),
	%------------------------------------------------------
	lbox_Clear(DagWin1),
	lbox_Clear(DagWin2),
	lbox_Add(DagWin1,  DagenList),
	lbox_SetSel(DagWin1, 0, b_True),
	lbox_GetSel(DagWin1, DagSel1List,_IndexListD1),
	DagSel1List = [DagS1|_],
	dag(Dag1, DagS1, _),
	findall(Kw1, KwartlistW(Dag1, WCList, Kw1), KwartierenList1),
	lbox_Clear(TijdWin1),
	lbox_Add(TijdWin1,  KwartierenList1),
	lbox_SetSel(TijdWin1, 0, b_True),
	%------------------------------------------------------
	lbox_Add(DagWin2, DagenList),
	lbox_SetSel(DagWin2, 0, b_True),
	lbox_GetSel(DagWin2, DagSel2List,_IndexListD2),
	DagSel2List = [DagS2|_],
	dag(Dag2, DagS2, _),
	findall(Kw2, KwartlistW(Dag2, WCList, Kw2), KwartierenList2),
	lbox_Clear(TijdWin2),
	lbox_Add(TijdWin2, KwartierenList2),
	%lbox_SetSel(TijdWin2, 0, b_True),
	beginText(_Win,_,_),
	check_ond(_Win),
	!.
%END OnderdPlanMultWis, idco_onderdselmult_alle _CtlInfo

%BEGIN OnderdPlanMultWis, idcbo_onderdelen selchanged
  dlg_onderdplanmultwis_eh(_Win,e_Control(idcbo_onderdelen,_CtrlType,OndWin,selchanged),0):-
	%OndWin = win_GetCtlHandle(_Win, idcbo_onderdelen),
	DagWin1  = win_GetCtlHandle(_Win, idcbo_dag1),
	TijdWin1 = win_GetCtlHandle(_Win, idcbo_uur1),
	DagWin2  = win_GetCtlHandle(_Win, idcbo_dag2),
	TijdWin2 = win_GetCtlHandle(_Win, idcbo_uurtot),
	lbox_GetSel(OndWin,ItemList,_IndexList),
	items2WC(ItemList, [], WCList),
	findall(DagN, dagMetPlanning(DagN, WCList), DagenList),
	%------------------------------------------------------
	lbox_Clear(DagWin1),
	lbox_Clear(DagWin2),
	lbox_Add(DagWin1,  DagenList),
	lbox_SetSel(DagWin1, 0, b_True),
	lbox_GetSel(DagWin1, DagSel1List,_IndexListD1),
	DagSel1List = [DagS1|_],
	dag(Dag1, DagS1, _),
	findall(Kw1, KwartlistW(Dag1, WCList, Kw1), KwartierenList1),
	lbox_Clear(TijdWin1),
	lbox_Add(TijdWin1,  KwartierenList1),
	lbox_SetSel(TijdWin1, 0, b_True),
	%------------------------------------------------------
	lbox_Add(DagWin2, DagenList),
	lbox_SetSel(DagWin2, 0, b_True),
	lbox_GetSel(DagWin2, DagSel2List,_IndexListD2),
	DagSel2List = [DagS2|_],
	dag(Dag2, DagS2, _),
	findall(Kw2, KwartlistW(Dag2, WCList, Kw2), KwartierenList2),
	lbox_Clear(TijdWin2),
	lbox_Add(TijdWin2, KwartierenList2),
	%lbox_SetSel(TijdWin2, 0, b_True),
	beginText(_Win,_,_),
	check_ond(_Win),
	!.
%END OnderdPlanMultWis, idcbo_onderdelen selchanged

%BEGIN OnderdPlanMultWis, e_Create
  dlg_onderdplanmultwis_eh(_Win,e_Create(_CreationData),0):-!,
	dialog_SetState(_Win, [enable(idc_ok, b_False)]),
	OndWin   = win_GetCtlHandle(_Win, idcbo_onderdelen),
	DagWin1  = win_GetCtlHandle(_Win, idcbo_dag1),
	%TijdWin1 = win_GetCtlHandle(_Win, idcbo_uur1),
	DagWin2  = win_GetCtlHandle(_Win, idcbo_dag2),
	%TijdWin2 = win_GetCtlHandle(_Win, idcbo_uurtot),
	lbox_SetTabStops(OndWin, [10, 20, 100]),
	findall(Repr, onderdReprPl(Repr), OnderdList),
	lbox_Add(OndWin, OnderdList),
	Font = font_Create(ff_Fixed, [], 10),
	win_SetFont(DagWin1, Font),
	win_SetFont(DagWin2, Font),
	%eindText(_Win,_),
	%check_selectie(_Win),
	!.
%END OnderdPlanMultWis, e_Create

  dlg_onderdplanmultwis_eh(_,_,_):-!,fail.

%END_DLG OnderdPlanMultWis

%BEGIN_DLG WedsplanWis
/**************************************************************************
	Creation and event handling for dialog: WedsplanWis
**************************************************************************/

constants

%BEGIN WedsplanWis, CreateParms, 13:48:21-4.12.2002, Code automatically updated!
  dlg_wedsplanwis_DlgType = wd_Modal
  dlg_wedsplanwis_Title = "Wis wedstrijdplan"
  dlg_wedsplanwis_RCT = rct(50,40,290,175)
  dlg_wedsplanwis_Flags = [wsf_Close,wsf_TitleBar]
  dlg_wedsplanwis_Help = idh_planning_wissen
%END WedsplanWis, CreateParms

database - bezwaren
  returnC(integer, wedscat, integer)	
  bezwTekst(WINDOW, integer, wedscat, string)
  single wisWedId(integer, wedscat)	% te gast

predicates

  dlg_wedsplanwis_eh : EHANDLER
procedure  initWaars(WINDOW, janee Oproepen, janee Gew1, tgnst T1, janee Gew2, tgnst T2)  
procedure  afz1(BOOLEAN,janee, integer, wedscat, tijd, tgnst)

clauses

wisWedId(0, 0).

afz1(b_True, _,  Pos, Cat, _Tijd, Tgnst) :-
  ws(Pos, Cat, Tgnst, T), 
  logf('R', ws(Pos, Cat, Tgnst, T), ja),
  !.			% was al
afz1(_, ja, Pos, Cat, _Tijd, Tgnst) :-
  ws(Pos, Cat, Tgnst, _), !.		% was al
afz1(b_False, ja, Pos, Cat, Tijd, Tgnst) :-
  not(Tgnst = onbekend),
  logf('A', ws(Pos, Cat, Tgnst, Tijd),ja), !.
afz1(_, _, _, _, _, _).

initWaars(_Win, nee, _G1, _T1, _G2, _T2) :- !.
initWaars(_Win, _, nee, _, nee, _) :- !.
initWaars(Win, _, _, _, _, _) :- 
  dialog_SetState(Win, [show(idc_afgezegd, b_True)]),
  fail.
initWaars(Win, ja, ja, T1, _, _) :- 
  G1Win = win_GetCtlHandle(Win, idc_sp1afgez),
  win_SetState(G1Win, [wsf_Visible]),
  tgnst_naam(nee, b_False, 0, T1, Namen1, Tel1),
  win_SetText(G1Win, Namen1),
  trim_c(Tel1),
  Tel1 <> "",
  T1Win = win_GetCtlHandle(Win, idct_tel1),
  win_SetState(T1Win, [wsf_Visible]),
  win_SetText(T1Win, Tel1),
  fail.
initWaars(Win, ja, _, _, ja, T2) :- 
  G2Win = win_GetCtlHandle(Win, idc_sp2afgez),
  win_SetState(G2Win, [wsf_Visible]),
  tgnst_naam(nee, b_False,0, T2, Namen2, Tel2),
  win_SetText(G2Win, Namen2),
  trim_c(Tel2),
  Tel2 <> "",
  T2Win = win_GetCtlHandle(Win, idct_tel2),
  win_SetState(T2Win, [wsf_Visible]),
  win_SetText(T2Win, Tel2),
  fail.
initWaars(_Win, _, _, _, _, _). 

afgelasten(_Win, Pos, Cat) :-		% wedstrijd afgelasten
  TaskWin = vpi_GetTaskWin(),
  dlg_wedsplanwis_Create(TaskWin, Pos, Cat),
  update_uitslagen("tasplan afgelasten"),
  update_proj_toolbar(), !.
/*  w2(Pos,Cat,Tijd,B,C,D,E,F),		%  wis de planning
  logf('R',w2(Pos,Cat,Tijd,B,C,D,E,F),nee),
  wd(Pos, Cat, T1, T2, _, _), 
  afz(B, Pos, Cat, Tijd, T1, nee),
  afz(C, Pos, Cat, Tijd, T2, nee),
  update_waarsch(),
  %flush_log(),
  select(Cat, Pos), !.*/

  dlg_wedsplanwis_Create(Parent, No, Cat):-
	assert(wisWedId(No, Cat)),
	win_CreateDynDialog(Parent,
		[
%BEGIN WedsplanWis, WinDefList, 13:48:21-4.12.2002, Code automatically updated!
		 dlg(wdef(dlg_wedsplanwis_DlgType,dlg_wedsplanwis_RCT,dlg_wedsplanwis_Title,u_DlgBase),dlg_wedsplanwis_Flags),
		 ctl(wdef(wc_CheckBox,rct(6,52,172,62),"Speler1 afgezegd",u_DlgBase),idc_sp1afgez,[wsf_Auto,wsf_Group,wsf_TabStop,wsf_Invisible]),
		 ctl(wdef(wc_CheckBox,rct(6,93,172,103),"Speler2 afgezegd",u_DlgBase),idc_sp2afgez,[wsf_Auto,wsf_Group,wsf_TabStop,wsf_Invisible]),
		 ctl(wdef(wc_PushButton,rct(194,116,238,133),"&Wissen",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(194,101,238,113),"&Cancel",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(194,86,238,98),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_Text,rct(14,20,173,35),"Plan op: ...",u_DlgBase),idct_plan_op,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(12,2,176,15),"Wedstrijd",u_DlgBase),idct_wedstrijd,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(24,63,173,89),"Tel1",u_DlgBase),idct_tel1,[wsf_AlignLeft,wsf_Invisible]),
		 ctl(wdef(wc_Text,rct(24,104,173,129),"Tel2",u_DlgBase),idct_tel2,[wsf_AlignLeft,wsf_Invisible]),
		 ctl(wdef(wc_GroupBox,rct(3,39,182,132),"Vinkje als afgezegd",u_DlgBase),idc_afgezegd,[wsf_Invisible])
%END WedsplanWis, WinDefList
		],dlg_wedsplanwis_eh,0).

%BEGIN WedsplanWis, idc_ok _CtlInfo
  dlg_wedsplanwis_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-!,
	G1Win = win_GetCtlHandle(_Win, idc_sp1afgez),
	G1checked = win_IsChecked(G1Win),
	G2Win = win_GetCtlHandle(_Win, idc_sp2afgez),
	G2checked = win_IsChecked(G2Win),
	wisWedId(Pos, Cat),
	w2(Pos,Cat,Tijd,B,C,D,E,F),		%  wis de planning
	logf('R',w2(Pos,Cat,Tijd,B,C,D,E,F),ja),
	wd(Pos, Cat, T1, T2, _, _, _), 
	afz1(G1Checked, B, Pos, Cat, Tijd, T1),
	afz1(G2Checked, C, Pos, Cat, Tijd, T2),
	logclose(),
	update_waarsch(),
	update_proj_toolbar(),
	update_uitslagen("tasplan wedsplanwis"),
	select(Cat, Pos), !,
	win_Destroy(_Win),
	!.
%END WedsplanWis, idc_ok _CtlInfo
%MARK WedsplanWis, new events

%BEGIN WedsplanWis, idc_help _CtlInfo
  dlg_wedsplanwis_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Planningwissen"),
	!.
%END WedsplanWis, idc_help _CtlInfo

%BEGIN WedsplanWis, e_Create
  dlg_wedsplanwis_eh(_Win,e_Create(_CreationData),0):-!,
	wisWedId(Pos, Cat),
	w2(Pos,Cat,Tijd,G1,G2,_D,_E,_F),		%  wis de planning
	tijd_str(Tijd,TStr1,_),
	bitright(Tijd,7,Dag),
	dag(Dag,DStr1,_),
	format(Tijdstip," Tijdstip: %s %s",DStr1,TStr1),
	PlWin = win_GetCtlHandle(_Win, idct_plan_op),
	win_SetText(PlWin, Tijdstip),
	wc(Cat, _, _, CNaam, _, _, _, _, _, _, _, _, _, _, _),
	%wc(Cat,CNaam,_EDG,_Kort,_VoorWie,_Geld,_Verl),
	WWin = win_GetCtlHandle(_Win, idct_wedstrijd),
	win_SetText(WWin, CNaam),
	wd(Pos, Cat, T1, T2, _, _, _),
	oproepen(Opr),
	initWaars(_Win, Opr, G1, T1, G2, T2),  
	!.
%END WedsplanWis, e_Create

%BEGIN WedsplanWis, idc_cancel _CtlInfo
  dlg_wedsplanwis_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	win_Destroy(_Win),
	!.
%END WedsplanWis, idc_cancel _CtlInfo

  dlg_wedsplanwis_eh(_,_,_):-!,fail.

%END_DLG WedsplanWis

clauses
planAangewezenWeds(_OWin, Num, Cat) :-		% global________________________
  planMetAgenda(Num, Cat), !.

%___________________ progress window opgavecheck_______________________

database - progr
 checkprogress(integer Line, string Tekst)
 determ progresswin(window)
 determ progressprogress(window)
 single uploadklaar(boolean)
 single hostIsProcessing(integer)
 
%BEGIN_DLG Opgave check
/**************************************************************************
	Creation and event handling for dialog: Opgave check
**************************************************************************/

constants

%BEGIN Opgave check, CreateParms, 13:56:29-31.7.2009, Code automatically updated!
  dlg_opgave_check_DlgType = wd_Modeless
  dlg_opgave_check_Title = "Opgave check"
  dlg_opgave_check_RCT = rct(50,40,364,193)
  dlg_opgave_check_Flags = [wsf_Close,wsf_TitleBar]
  dlg_opgave_check_Help = idh_adresetiketten
  dlg_opgave_check_Font = "Arial"
  dlg_opgave_check_FSize = 9
%END Opgave check, CreateParms

predicates

  dlg_opgave_check_eh : EHANDLER
  %checkvoortgang(integer, string)
  writeT(window, slist, integer)

clauses

uploadklaar(b_False).
hostIsProcessing(0).

%checkvoortgang(N, Tekst) :-
%  write("\n", N, " ", Tekst),
%  fail.
%##################################
% 0 = creëer
% 8 = meerdere files/opgaven
% 20 = setwait
% 38 = lange regel afbreken (gebruikt 138)
% 99 = stop na 3 sec
% 98 = stop na 6 sec

checkvoortgangsetsoftstop() :-
  progresswin(Win),
  win_SetState(Win, [wsf_Disabled]), !.
  %AWin = win_GetCtlHandle(Win, idc_afbreken),
  %win_SetState(AWin, [wsf_Visible, wsf_Enabled]), !.
checkvoortgangsetsoftstop().

%checkvoortgang(N, Tekst) :-
%  write("\n", N, " ", Tekst),
%  fail.
checkvoortgang(0, Tekst) :-		% 0 = creëer 
  assert(uploadklaar(b_False)),
  not(progresswin(_)),
  assert(checkprogress(0, Tekst)),
  Parent = vpi_GetTaskWin(),
  dlg_opgave_check_Create(Parent), !.
checkvoortgang(99, _) :-	% 99 = stop met klein delay
  progresswin(Win), !,
  _NewTimerId = timer_Set(Win, 3000).		% nog even nagenieten... het timerinterrupt zal killen
checkvoortgang(98, _) :-	% 98 = stop met wat langer delay
  progresswin(Win), !,
  _NewTimerId = timer_Set(Win, 6000).		% nog even nagenieten...
checkvoortgang(100, _) :-	% 100 = stop
  progresswin(Win), !,
  win_Destroy(Win),
  _Result = vpi_ProcessEvents( b_True ).
checkvoortgang(_N, _) :-
  not(progresswin(_)), !.	% doe niets
checkvoortgang(20, _Tekst) :-
  cursor_SetWait(),
  fail.
checkvoortgang(8, Tekst) :-	% voor meerdere files/pgaven
  retractall(checkprogress(1, _)),
  assertz(checkprogress(1, Tekst)),
  progresswin(Win),
  win_Invalidate(Win),
  _Result = vpi_ProcessEvents( b_True ),
  !.
checkvoortgang(38, Tekst) :-
  checkprogress(38, Hi),
  str_len(Hi, Len),
  Len < 72,
  retract(checkprogress(38, T)),
  concat(T, Tekst, NTekst),
  assertz(checkprogress(38, NTekst)),
  progresswin(Win),
  win_Invalidate(Win),
  _Result = vpi_ProcessEvents( b_True ),
  !.
checkvoortgang(38, Tekst) :-
  checkprogress(38, Hi),
  str_len(Hi, Len),
  Len >= 72,
  retract(checkprogress(139, T)),
  concat(T, Tekst, NTekst),
  assertz(checkprogress(139, NTekst)),
  progresswin(Win),
  win_Invalidate(Win),
  _Result = vpi_ProcessEvents( b_True ),
  !.
checkvoortgang(38, Tekst) :-
  not(checkprogress(38, _)),
  assertz(checkprogress(38, Tekst)),
  progresswin(Win),
  win_Invalidate(Win),
  _Result = vpi_ProcessEvents( b_True ),
  !.
checkvoortgang(38, Tekst) :-
  not(checkprogress(139, _)),
  assertz(checkprogress(139, Tekst)),
  progresswin(Win),
  win_Invalidate(Win),
  _Result = vpi_ProcessEvents( b_True ),
  !.
checkvoortgang(_, Tekst) :-
  assertz(checkprogress(0, Tekst)),
  %write("\n", Tekst),
  progresswin(Win),
  win_Invalidate(Win),
  _Result = vpi_ProcessEvents( b_True ),
  !.
checkvoortgang(_, _).

/*curlprogressPr(DownTotal, DownNow, X1, X2) :-
  writef("\n%f %f %f %f", DownTotal, DownNow, X1, X2),
  vpi_ProcessEvents(b_True ),
  fail.*/
curlprogressPr(_, _, _, _) :-
  not(progressprogress(_)),
  progresswin(Win),
  dialog_SetState(Win, [show(idc_progress,b_True)]),
  ProgressWin = win_GetCtlHandle(Win, idc_progress),
  assert(progressprogress(ProgressWin)),
  fail.
curlprogressPr(_, 0.0, UpTotal, UpNow) :-
  uploadklaar(b_False),
  UpTotalI = val(integer, UpTotal),
  UpNowI = val(integer, UpNow),
  UpTotalI <> UpNowI, 			% upload
  assert(hostIsProcessing(0)),
  progresswin(_),
  progressprogress(PWin),
  progress_bar_set_value(PWin, UpTotalI, UpNowI),
  vpi_ProcessEvents(b_True ), !.
curlprogressPr(_, 0.0, UpTotal, UpTotal) :-
  uploadklaar(b_False),
  assert(uploadklaar(b_True)),
  UpTotalI = val(integer, UpTotal),
  progresswin(_),
  progressprogress(PWin),
  progress_bar_set_value(PWin, UpTotalI, UpTotalI),
  vpi_ProcessEvents(b_True ), !.
curlprogressPr(_DownTotal, 0.0, _, _) :-
  progresswin(_),
  hostIsProcessing(100),
  assert(hostIsProcessing(0)),
  progressprogress(PWin),
  progress_bar_set_value(PWin, 100, 0),
  vpi_ProcessEvents(b_True ), !.
  %checkvoortgang(38, "XX"), !.
curlprogressPr(_DownTotal, 0.0, _, _) :-
  progresswin(_),
  hostIsProcessing(N),
  N1 = N + 5,
  assert(hostIsProcessing(N1)),
  progressprogress(PWin),
  progress_bar_set_value(PWin, 100, N1),
  vpi_ProcessEvents(b_True ), !.
curlprogressPr(DownTotal, DownNow, X, X) :-
  DownTotalI = val(integer, DownTotal),
  DownNowI = val(integer, DownNow),
  %DownTotalI <> DownNowI,		% download
  progresswin(_),
  progressprogress(PWin),
  progress_bar_set_value(PWin, DownTotalI, DownNowI),
  vpi_ProcessEvents(b_True ), !.
curlprogressPr(_, _, _, _).
  
/*
  write("\n", DownTotalI, " ", DownNowI, " ", UpTotal, " ", UpNow),
	_Result = vpi_ProcessEvents(), !.*/

writeT(Win, [Tekst| Rest], N) :-
  N1 = N + 1,
  T = 10 + N * 14,
  B = T + 14,
  draw_TextInRect(Win,rct(3,T,500,B), Tekst, -1,[dtext_left]), !,
  writeT(Win, Rest, N1).

  dlg_opgave_check_Create(Parent):-
	win_CreateDynDialog(Parent,
		[
%BEGIN Opgave check, WinDefList, 13:56:29-31.7.2009, Code automatically updated!
		 dlg_font(wdef(dlg_opgave_check_DlgType,dlg_opgave_check_RCT,dlg_opgave_check_Title,u_DlgBase),
		 	  dlg_opgave_check_Font,dlg_opgave_check_FSize,dlg_opgave_check_Flags),
		 customctl(wdef(wc_Custom,rct(61,139,312,151),"Custom",u_DlgBase),"progress",idc_progress,[wsf_Group,wsf_TabStop,wsf_Invisible]),
		 ctl(wdef(wc_PushButton,rct(3,138,51,150),"&Afbreken",u_DlgBase),idc_afbreken,[wsf_Group,wsf_TabStop,wsf_Disabled,wsf_Invisible])
%END Opgave check, WinDefList
		],dlg_opgave_check_eh,0).

%MARK Opgave check, new events

%BEGIN Opgave check, idc_afbreken _CtlInfo
  dlg_opgave_check_eh(_Win,e_Control(idc_afbreken,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	setsoftstop(),
	!.
%END Opgave check, idc_afbreken _CtlInfo

%BEGIN Opgave check, e_Update
  dlg_opgave_check_eh(_Win,e_Update(_UpdateRct),0):-!,
	findall(T,  checkprogress(_, T), TList),
	writeT(_Win, TList, 0), 
	!.
%END Opgave check, e_Update

%BEGIN Opgave check, e_Timer
  dlg_opgave_check_eh(_Win,e_Timer(TimerId),0):-
	timer_Kill(TimerId),
	win_Destroy(_Win),
	!.
%END Opgave check, e_Timer

%BEGIN Opgave check, e_Destroy
  dlg_opgave_check_eh(_Win,e_Destroy,0):-!,
	retractall(checkprogress(_,_)),
	retractall(progresswin(_)),
	retractall(progressprogress(_)),
	!.
%END Opgave check, e_Destroy

%BEGIN Opgave check, e_Create
  dlg_opgave_check_eh(_Win,e_Create(_CreationData),0):-!,
	assert(progresswin(_Win)),
	retract(checkprogress(_, Title)),
	win_SetText(_Win, Title),
	PWin = win_GetCtlHandle(_Win, idc_progress),
	progress_bar_set_colors(PWin,color_Gray,color_LtGray,pat_hollow),
	!.
%END Opgave check, e_Create

  dlg_opgave_check_eh(_,_,_):-!,fail.

%END_DLG Opgave check



%BEGIN_DLG Webupdateregelmaat
/**************************************************************************
	Creation and event handling for dialog: Webupdateregelmaat
**************************************************************************/

constants

%BEGIN Webupdateregelmaat, CreateParms, 13:13:44-14.7.2012, Code automatically updated!
  dlg_webupdateregelmaat_ResID = idd_regelmatige_webupdate
  dlg_webupdateregelmaat_DlgType = wd_Modal
  dlg_webupdateregelmaat_Help = idh_opsturen_naar_het_web
%END Webupdateregelmaat, CreateParms
id_setfocus =124

predicates

  dlg_webupdateregelmaat_eh : EHANDLER
  dlg_webupdateregelmaat_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_webupdateregelmaat_update(DIALOG_VAL_LIST)
  %autowebOptie2int(integer Idc_aan, integer Getal, string) 
  procedure htmlOfFTP(integer Idc_upload_naar_klapper, integer Getal, string )
  ftpOfHtml(integer, integer)
  groepHoeEnable(window, boolean)
  determ hoevaakselectie(hoevaak, integer)
  procedure metSlash(string, string)
  %procedure geenvasteverbinding(integer, integer)

clauses

autowebOptie2int(1, 1, vaak, WN) :- 
  providerX(_, PN, "", _),
  format(WN, "Vaak, naar %s", PN), !. 
autowebOptie2int(1, 1, mindervaak, WN) :- 
  providerX(_, PN, "", _),
  format(WN, "Regelmatig, naar %s", PN), !. 
autowebOptie2int(1, 1, afentoe, WN) :- 
  providerX(_, PN, "", _),
  format(WN, "Af en toe, naar %s", PN), !. 
autowebOptie2int(_, 1, _, WN) :- 
  providerX(_, PN, "", _),
  format(WN, "Uit, naar %s", PN), !. 
autowebOptie2int(1, _, vaak, "Vaak, per FTP") :- !. 
autowebOptie2int(1, _, mindervaak, "Regelmatig, per FTP") :- !. 
autowebOptie2int(1, _, afentoe, "Af en toe, per FTP") :- !. 
autowebOptie2int(_, _, _, "Uit, per FTP") :- !. 

htmlOfFTP(idc_upload_naar_klapper, 1, PU) :-
  providerX(PU, _, "", _), !.
htmlOfFTP(idc_ftp_naar_eigen_website, _, "Eigen website") :- !.

ftpOfHtml(idc_upload_naar_klapper, 1) :- !.
ftpOfHtml(idc_ftp_naar_eigen_website, 2).


autowebP(Int,HTMLFTP,Waarsch,_IDC_GEBRUIKER_VALUE, Wachtwoord, _IDC_FTP_adres_VALUE,Nietgebruikt,0,Hoevaak,BijSluiten) :-
  api_getComputername(ComputerName),
  upper_lower(PC, Computername),
  autoweb(PC, Int,HTMLFTP,Waarsch, _IDC_GEBRUIKER_VALUE, Bin, _IDC_FTP_adres_VALUE,Nietgebruikt,_Vasteverb,Hoevaak,BijSluiten),
  %geenvasteverbinding(Vasteverb, GVB),
  term_bin(string,Wachtwoord,Bin), !.
autowebP(0,1,b_True,"jg.de.lint", "Nopje1@", "home.hccnet.nl/mijnweb", "", 0, vaak, 1) :-
  comline(LineBuffer),
  upper_lower(LineBuffer, LBL),
  searchstring(LBL, "/x", _),!.	% in TEST
  %geenvasteverbinding(1, GVB).
autowebP(0,1,b_True,"", "", "", "", 0, afentoe, 1).
  %geenvasteverbinding(1, GVB).

hoevaakselectie(vaak, 0).
hoevaakselectie(mindervaak, 1).
hoevaakselectie(afentoe, 2).

groepHoeEnable(Win, TrueFalse) :-
  dialog_SetState(Win, [enable(idc_wachtwoord, TrueFalse), 
      enable(idc_gebruiker, TrueFalse), enable(idc_ftp_adres, TrueFalse),
      enable(idct_nb, TrueFalse),
      enable(idct_voorbeeld_ftpmijnbebnlmijndir, TrueFalse),
      enable(idct_incl_pad, TrueFalse), %enable(idc_ftpdirectory, TrueFalse),
      enable(idct_ftp_adres, TrueFalse), enable(idc_ftp_gegevens, TrueFalse),
      enable(idct_gebruiker, TrueFalse), enable(idct_wachtwoord, TrueFalse)]).

metSlash(In, In) :-
  reversestr(In, "", Rev),
  frontchar(Rev, '/', _),!.
metSlash(In, Uit) :-
  concat(In, "/", Uit).
  
  dlg_webupdateregelmaat_Create(Parent, MetSchemas):-
	autowebP(IDC_AAN_CHECKED,HTMLFTP,IDC_EVEN_WAARSCHUWEN_CHECKED,IDC_GEBRUIKER_VALUE,
	   IDC_WACHTWOORD_VALUE, IDC_FTP_ADRES_VALUE, _Ongebruikt,
	   _IDC_VRAGEN_OM_INTERNETVERBINDING_CHECKED, Hoevaak, IDC_SLUITEN_CHECKED),
	hoevaakselectie(Hoevaak, IDC_HOEVAAK_SELECT),
	htmlOfFTP(IDC_WEBFTP, HTMLFTP, _),
	IDC_HOEVAAK_ITEMLIST = ["vaak","minder vaak","af en toe"],
	%IDC_HOEVAAK_SELECT = 0,
	%IDC_AAN_CHECKED = b_false,
%MARK Webupdateregelmaat, new variables

	dialog_CreateModal(Parent,dlg_webupdateregelmaat_ResID,"",
  		[
%BEGIN Webupdateregelmaat, ControlList, 13:13:44-14.7.2012, Code automatically updated!
		df(idc_ftp_adres,editstr(IDC_FTP_ADRES_VALUE,[]),nopr),
		df(idc_gebruiker,editstr(IDC_GEBRUIKER_VALUE,[]),nopr),
		df(idc_wachtwoord,editstr(IDC_WACHTWOORD_VALUE,[]),nopr),
		df(idc_even_waarschuwen,checkbox(IDC_EVEN_WAARSCHUWEN_CHECKED),nopr),
		df(idc_aan,checkbox(IDC_AAN_CHECKED),nopr),
		df(idc_hoevaak,listbutton(IDC_HOEVAAK_ITEMLIST,IDC_HOEVAAK_SELECT),nopr),
		df(idc_sluiten,checkbox(IDC_SLUITEN_CHECKED),nopr),
		df(IDC_WEBFTP,radiobuttongroup([idc_ftp_naar_eigen_website,idc_upload_naar_klapper]),nopr)
%END Webupdateregelmaat, ControlList
		],
		dlg_webupdateregelmaat_eh,MetSchemas,VALLIST,ANSWER),
	dlg_webupdateregelmaat_handle_answer(ANSWER,VALLIST).

  dlg_webupdateregelmaat_handle_answer(idc_ok,VALLIST):-!,
	dlg_webupdateregelmaat_update(VALLIST).
  dlg_webupdateregelmaat_handle_answer(idc_cancel,_):-!.  % Handle Esc and Cancel here
  dlg_webupdateregelmaat_handle_answer(_,_):-
	errorexit().

  dlg_webupdateregelmaat_update(_VALLIST):-
%BEGIN Webupdateregelmaat, Update controls, 13:13:44-14.7.2012, Code automatically updated!
	_IDC_FTP_ADRES_VALUE = dialog_VLGetstr(idc_ftp_adres,_VALLIST),
	_IDC_WACHTWOORD_VALUE = dialog_VLGetstr(idc_wachtwoord,_VALLIST),
	_IDC_GEBRUIKER_VALUE = dialog_VLGetstr(idc_gebruiker,_VALLIST),
	_IDC_EVEN_WAARSCHUWEN_CHECKED = dialog_VLGetCheck(idc_even_waarschuwen,_VALLIST),
	_IDC_AAN_CHECKED = dialog_VLGetCheck(idc_aan,_VALLIST),
	dialog_VLGetListButton(idc_hoevaak,_VALLIST,_IDC_HOEVAAK_ITEMLIST,_IDC_HOEVAAK_SELECT),
	_IDC_SLUITEN_CHECKED = dialog_VLGetCheck(idc_sluiten,_VALLIST),
	_IDC_WEBFTP = dialog_VLGetRadiobutton(idc_ftp_naar_eigen_website,_VALLIST),
%END Webupdateregelmaat, Update controls
	hoevaakselectie(Hoevaak, _IDC_HOEVAAK_SELECT),
	ftpOfHtml(_IDC_WEBFTP, HTMLFTP),
	%autowebp(Int,HTMLFTP,Waarsch,_IDC_GEBRUIKER_VALUE, Wachtwoord, _IDC_FTP_adres_VALUE,Nietgebruikt,GVB,Hoevaak,BijSluiten),
	SrcStr=_IDC_WACHTWOORD_VALUE,
	StrArrayLength = sizeof(SrcStr),
	Bin=composebinary(SrcStr,StrArrayLength),
	%term_bin(string,RestoredStr,Bin),
	metslash(_IDC_FTP_adres_VALUE, FTPadres),
	api_getComputername(Computername),
	upper_lower(PC, Computername),
	logf('A', autoweb(PC, _IDC_AAN_CHECKED,HTMLFTP,_IDC_EVEN_WAARSCHUWEN_CHECKED,_IDC_GEBRUIKER_VALUE, Bin, 
	  FTPadres, "", 0, Hoevaak, _IDC_SLUITEN_CHECKED),nee).

%MARK Webupdateregelmaat, new events

%BEGIN Webupdateregelmaat, e_User
  dlg_webupdateregelmaat_eh(_Win,e_User(id_setfocus,Ctrlid),0):-!,
	CtrlWin = win_GetCtlHandle(_Win, CtrlId),
	win_SetFocus(CtrlWin),
	!.
%END Webupdateregelmaat, e_User

%BEGIN Webupdateregelmaat, idc_ok _CtlInfo
  dlg_webupdateregelmaat_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	idc_ftp_naar_eigen_website <> dialog_GetRadiobutton(_Win,idc_ftp_naar_eigen_website),
	!,
	fail.
  dlg_webupdateregelmaat_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	Str = dialog_GetStr(_Win, idc_ftp_adres),
	not(fronttoken(Str, _, _)),
	_A = dlg_MessageBox( "FTP", "FTP-adres (inclusief pad) nodig\nVoorbeeeld: ftp.mijnwebsite.nl/mijntoernooi.", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	win_PostEvent(_Win,e_User(id_setfocus, idc_ftp_adres)),
	!.
  dlg_webupdateregelmaat_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	Str = dialog_GetStr(_Win, idc_gebruiker),
	not(fronttoken(Str, _, _)),
	_A = dlg_MessageBox( "FTP", "Gebruikersnaam nodig.", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	win_PostEvent(_Win,e_User(id_setfocus, idc_gebruiker)),
	!.
  dlg_webupdateregelmaat_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	Str = dialog_GetStr(_Win, idc_wachtwoord),
	not(fronttoken(Str, _, _)),
	_A = dlg_MessageBox( "FTP", "Wachtwoord nodig.", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	win_PostEvent(_Win,e_User(id_setfocus, idc_wachtwoord)),
	!.
%END Webupdateregelmaat, idc_ok _CtlInfo

%BEGIN Webupdateregelmaat, idc_help _CtlInfo
  dlg_webupdateregelmaat_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Opsturennaarhetweb"),
	!.
%END Webupdateregelmaat, idc_help _CtlInfo

%BEGIN Webupdateregelmaat, idc_aan _CtlInfo
  dlg_webupdateregelmaat_eh(_Win,e_Control(idc_aan,_CtrlType,_CtrlWin,_CtlInfo),0):-
	Checked = win_IsChecked(_CtrlWin),
	dialog_SetState(_Win, [enable(idc_even_waarschuwen, Checked),
	  enable(idc_hoevaak, Checked), enable(idc_sluiten, Checked)]),
	!.
%END Webupdateregelmaat, idc_aan _CtlInfo

%BEGIN Webupdateregelmaat, idc_upload_naar_klapper _CtlInfo
  dlg_webupdateregelmaat_eh(_Win,e_Control(idc_upload_naar_klapper,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	groepHoeEnable(_Win, b_False),
	!.
%END Webupdateregelmaat, idc_upload_naar_klapper _CtlInfo

%BEGIN Webupdateregelmaat, idc_ftp_naar_eigen_website _CtlInfo
  dlg_webupdateregelmaat_eh(_Win,e_Control(idc_ftp_naar_eigen_website,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	groepHoeEnable(_Win, b_True),
	!.
%END Webupdateregelmaat, idc_ftp_naar_eigen_website _CtlInfo

%BEGIN Webupdateregelmaat, e_Create
  dlg_webupdateregelmaat_eh(_Win,e_Create(_CreationData),0):-
	providerX(P, _PN, "", _),
	PWin = win_GetCtlHandle(_Win, idc_upload_naar_klapper),
	win_SetText(PWin, P),
	TrueFalse = dialog_Getcheck(_Win, idc_aan),
	dialog_SetState(_Win, [enable(idc_even_waarschuwen, TrueFalse),
	 enable(idc_hoevaak, TrueFalse), enable(idc_sluiten, TrueFalse)]),
	fail.
  dlg_webupdateregelmaat_eh(_Win,e_Create(MetSchemas),0):-
  	dialog_SetState(_Win, [show(idc_aan, MetSchemas), show(idc_even_waarschuwen, MetSchemas),
  	show(idc_sluiten, MetSchemas), show(idc_hoevaak, MetSchemas)]),
  	fail.
  dlg_webupdateregelmaat_eh(_Win,e_Create(_CreationData),0):-
	groepHoeEnable(_Win, b_False),
	idc_ftp_naar_eigen_website = dialog_GetRadioButton(_Win, idc_ftp_naar_eigen_website),
	groepHoeEnable(_Win, b_True),
	!.
%END Webupdateregelmaat, e_Create

  dlg_webupdateregelmaat_eh(_,_,_):-!,fail.

%END_DLG Webupdateregelmaat

predicates
  procedure wisalleplanning()

clauses

wisplanAlle(Parent) :-  
  dlg_paswoord_Create(Parent, Return),
  Return = idc_ok,
  findall(Cat, w2(_,Cat,_,_,_,_,_,_), CL),
  count(CL, 0, Aantal),
  format(M1, "Alle planning schoon maken van %u partijen?\n\nZeker weten?\n==> Maak eerst een back-up! <==", Aantal),
  1 = dlg_MessageBox( "Alle planning wissen", M1, mesbox_iconquestion, mesbox_buttonsokcancel, mesbox_defaultsecond, mesbox_suspendapplication ),
  assert(aantgewis(0)),
  wisalleplanning,
  aantgewis(N),
  format(Msg, "Planning van %u wedstrijden gewist!", N),  
  dlg_MessageBox( "Alle planning wissen", Msg, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  !.
wisplanAlle(_).
    
wisalleplanning() :-
  w2(No,Cat,A1,A2,A3,A4,A5,A6),	 	%  wis alle planning
  logf('R',w2(No,Cat,A1,A2,A3,A4,A5,A6),ja),
  cursor_SetWait(),
  aantgewis(I),
  I1 = I + 1,
  assert(aantgewis(I1)),
  fail.
wisalleplanning() :-
  ws(Pos, Cat, Tgnst, T), 
  logf('R', ws(Pos, Cat, Tgnst, T), ja),
  fail.
wisalleplanning() :-
  update_display(),
  update_uitslagen("tasplan wis alleplanning"),
  update_waarsch(),
  update_proj_toolbar(),
  logclose().

%BEGIN_DLG Bezwaren
/**************************************************************************
	Creation and event handling for dialog: Bezwaren
**************************************************************************/

constants

%BEGIN Bezwaren, CreateParms, 12:03:35-16.11.2011, Code automatically updated!
  dlg_bezwaren_DlgType = wd_Modal
  dlg_bezwaren_Title = "Bezwaren en handhaven (vervolg)planning"
  dlg_bezwaren_RCT = rct(50,40,246,211)
  dlg_bezwaren_Flags = [wsf_Close,wsf_TitleBar]
  dlg_bezwaren_Help = idh_adresetiketten
  dlg_bezwaren_Font = "Arial"
  dlg_bezwaren_FSize = 10
%END Bezwaren, CreateParms

predicates

  dlg_bezwaren_eh : EHANDLER

clauses

  dlg_bezwaren_Create(_Parent, Nr, Cat, Bezwaren, 0):-
	retract(bezwTekst(Win, Nr, Cat, _Tekst)),
	assert(bezwTekst(Win, Nr, Cat, Bezwaren)),
	win_Invalidate(Win), !.
  dlg_bezwaren_Create(Parent, Nr, Cat, Bezwaren, Return):-
	logclose(),
	assertz(bezwTekst(~0, Nr, Cat, Bezwaren)), 
	win_CreateDynDialog(Parent,
		[
%BEGIN Bezwaren, WinDefList, 12:03:35-16.11.2011, Code automatically updated!
		 dlg_font(wdef(dlg_bezwaren_DlgType,dlg_bezwaren_RCT,dlg_bezwaren_Title,u_DlgBase),
		 	  dlg_bezwaren_Font,dlg_bezwaren_FSize,dlg_bezwaren_Flags),
		 ctl(wdef(wc_PushButton,rct(131,157,194,169),"&Ja",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(66,157,129,169),"&Nee",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(3,157,26,169),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop])
%END Bezwaren, WinDefList
		],dlg_bezwaren_eh,0),
	retract(returnC(Nr, Cat, Return)), !.
  dlg_bezwaren_Create(_, _, _, _, 0).

%BEGIN Bezwaren, idc_ok _CtlInfo
  dlg_bezwaren_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-!,
	bezwTekst(_Win, Nr, Cat, _),
	assert(returnC(Nr, Cat, 0)),   
	win_Destroy(_Win),
	!.
%END Bezwaren, idc_ok _CtlInfo
%MARK Bezwaren, new events

%BEGIN Bezwaren, e_Create
  dlg_bezwaren_eh(_Win,e_Create(_CreationData),0):-
  	retract(bezwTekst(~0, Nr, Cat, Tekst)),
  	assert(bezwTekst(_Win, Nr, Cat, Tekst)),
	!.
%END Bezwaren, e_Create

%BEGIN Bezwaren, idc_cancel _CtlInfo
  dlg_bezwaren_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_Creationdata),0):-
	bezwTekst(_Win, Nr, Cat, _),
	assert(returnC(Nr, Cat, 0)),
	win_Destroy(_Win),
	%Bezwaren = cast(string, _CreationData),
	%assert(bezwTekst(_Win, Bezwaren)),
	!.
%END Bezwaren, idc_cancel _CtlInfo

%BEGIN Bezwaren, e_Update
  dlg_bezwaren_eh(_Win,e_Update(_UpdateRct),0):-!,
	win_SetBrush(_Win,brush(pat_Solid,color_White)),
	bezwTekst(_Win, WdNo, Cat, Str),
	w2(WdNo,Cat,Tijd,_,_,Geforceerd,_,_), 
	%pos(Cat,Card,_,_), 
	wc(Cat, _, _, _, _, WedscatShort, _, _, _, _, _, _, _, _, _),
	%wc(Cat,_,_,WedsCatShort,_,_,_),
	tijd_kwart(D1,U1,M1,Tijd),
	helestr_int(0,MS1,M1),
	dag(D1, DagNaam, _),
	wd(WdNo,Cat,T1,T2,_,_,_),
	repa(nee,'}',nee, nee,nee,0,2,1,WdNo,Cat,T1,alleplanning,TS1,_Score, _TtijdChanged,b_True, b_False,_Clubs,_,_,_,_,_,_),
	repa(nee,'}',nee, nee,nee,1,2,1,WdNo,Cat,T2,alleplanning,TS2,_,_,b_True,b_False,_Clubs1,_Ankr,_,_,_,_,_),
	format(Jtekst, "%c %s/% ", Geforceerd,TS1,TS2),
	format(ITekst, "Betreft %s %s %d:%s", WedsCatShort, DagNaam, U1, MS1),
	format(Utekst, "Er zijn conflicten met de huidige planning van:\n%s\n%s\n%s\n\nDie planning handhaven??",  JTekst, ITekst, Str),
	OKWin = win_GetCtlHandle(_Win, idc_ok),
	OKrect = win_GetOuterRect(OKWin),
	OKrect = rct(_, OKTop, _, _),
	RCT = win_GetClientRect( _Win ),
	RCT = rct(L, T, R, _),
	RCT0 = rct(L, T, R, OKTop),
	RCT1 = rect_Inflate(RCT0, -5, -5),
	RCT2 = rect_Inflate(RCT0, -2, -2),
	draw_Rect(_Win, RCT2),
	%write("\n", Str),
	%draw_Text(_Win, X, Y, Str, Len),
	draw_TextInRect(_Win,RCT1, Utekst, -1,[dtext_Left]),
	!.
%END Bezwaren, e_Update

%BEGIN Bezwaren, e_Destroy
  dlg_bezwaren_eh(_Win,e_Destroy,0):-
	retractall(bezwTekst(_Win, _, _, _)),
	!.
%END Bezwaren, e_Destroy

  dlg_bezwaren_eh(_,_,_):-!,fail.

%END_DLG Bezwaren

