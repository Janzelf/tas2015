/*****************************************************************************

		Copyright (c) 1989 - 1998 De Lint Associates

 Project:  TASVPI
 FileName: TASVERH.PRO
 Purpose: Invoer van verhinderingen + spelerselectiedialoog + toernooibeleid
 Written by: JdL
 Comments:
******************************************************************************/

include "tasvp1.inc"
include "tasvp1.con"
include "hlptopic.con"
%include "treeview\\treeview.ph"
include "datecust\\date_cc.pre"

database - verhn
single currentPersoon(integer)
single tabF(integer)
single vanTot(integer, integer)

predicates

procedure sortverhinderingen(verhl,verhl)
procedure splitv(period,verhl,verhl,verhl)
procedure merge_verh1(verhl, verhl, janee)
overlap(period, period, period)
check_per(period)
procedure updateVerhinderingTekst(window)
procedure updateDagTekst(window Win, string, boolean Compleet)
procedure updateDagTekst1(slist , string, boolean Compleet )
procedure updateTijdstipTekst(window Win, string Tekst, period, boolean Compleet)
procedure checkUitKantoor(verhl Verhinderingen, integer UitKant, integer)
nondeterm verhindertijd(integer N, string TijdStr)
procedure bepaalNieuweVanTot1(integer VanO, integer TotO, integer Van, integer Mid, integer Tot, integer Van, integer Tot, integer Deselect)
procedure bepaalNieuweVanTot(window Win)
procedure kleur(integer, integer, integer, color, FONTSTYLE Fontstyle)

clauses

currentPersoon(0).
tabF(0).
vanTot(-1, -1).

check_per(Periode) :-
  currentPersoon(SKeus),
  wd_planned(SKeus,WedstrijdList,alleplanning,b_True),	%wl
  Periode = per(K1,K2),
  wedstr_member(Wedstrijd,WedstrijdList),
  Wedstrijd = w(_/*WedNo*/,_/*WedsCat*/, Tijd, _, _, _/*Persno*/, _, _),
  K1 >= 0,
  Tijd < K2,
  Tijd  + 8 > K1,
  tijd_kwart(D1,_,_,K1),
  dag(D1,DS1,_), 
  tijd_str(K1,KS1, _),
  tijd_str(K2,TS3, _),
  format(H, "% % tot %\nStrijdig met huidige planning!\ntoch boeken?", DS1, KS1, TS3), !,
  1 = dlg_MessageBox( "Verhindering", H, mesbox_iconinformation, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ).
check_per(_).

sortverhinderingen([],[]).
sortverhinderingen([H|T],S) :-
  splitv(H,T,A,B),
  sortverhinderingen(A,A1),
  sortverhinderingen(B,B1),
  append(A1,[H|B1],S), !.

splitv(_,[],[],[]).
splitv(per(H1,H2),[per(A1,A2)|X],[per(A1,A2)|Y],Z) :-
  H1 = A1,
  H2 > A2, !,
  splitv(per(H1,H2),X,Y,Z).
splitv(per(H1,H2),[per(A1,A2)|X],[per(A1,A2)|Y],Z) :-
  H1 > A1, !,
  splitv(per(H1,H2),X,Y,Z).
splitv(H,[A|X],Y,[A|Z]) :- !,
  splitv(H,X,Y,Z).

merge_verh1([], [], nee) :- !.
merge_verh1([X], [X], nee) :- !.
merge_verh1([X,Y|Rest], Uit, ja) :-
	 overlap(X,Y,Z),
	 merge_verh1([Z | Rest], Uit, _), !.
merge_verh1([X|Rest], [X|Uit], Overlap) :-
	merge_verh1(Rest, Uit, Overlap).

merge_verh(In, Uit, Overlap) :-
  sortverhinderingen(In, In1),
  merge_verh1(In1, Uit, Overlap).

overlap(per(K1,K2),per(O1,O2),per(G1,G2)) :-
  K1 <= O2,
  K1 >= O1, !,
  kleinste(K1,O1,G1),
  grootste(K2,O2,G2). 
  %write("\nK1: ",K1," K2: ",K2," O1: ",O1," O2: ",O2," Overlap1a").
overlap(per(K1,K2),per(O1,O2),per(G1,G2)) :-
  K2 <= O2,
  K2 >= O1,
  kleinste(K1,O1,G1),
  grootste(K2,O2,G2). 
  %write("\nK1: ",K1," K2: ",K2," O1: ",O1," O2: ",O2," Overlap1b").

soortgelijkeDagen(I, [Dag| _], N, N) :-
  dt_offset_to_date(Dag, _, _, _, _, DagVanDeWeek),
  DagVanDeWeek = I.
soortgelijkeDagen(I, [_| Rest], N, M) :-
  N1 = N + 1,
  soortgelijkeDagen(I, Rest, N1, M).

elkeWerkdag([Dag|Rest], N, [N | Lijst]) :-
  dag(Dag, _, w), !,
  N1 = N + 1,
  elkeWerkdag(Rest, N1, Lijst).
elkeWerkdag([_|Rest], N, Lijst) :-
  N1 = N + 1, !,
  elkeWerkdag(Rest, N1, Lijst).
elkeWerkdag(_, _, []).

zetSelect(LboxWin, NListR) :-
  member(N, NListR),
  lbox_SetSel(LboxWin, N, b_True),
  fail.
zetSelect(_LboxWin, _NListR).

bepaalNieuweVanTot1(VanO, _TotO, Van, Mid, Tot, Van, Tot, Mid) :-	% verlaag de ondergrens (Mid weg)
  Van < VanO, !.
bepaalNieuweVanTot1(_VanO, TotO, Van, Mid, Tot, Van, Tot, Mid) :-	% verhoog de bovengrens (Mid weg)
  Tot > TotO, !.
bepaalNieuweVanTot1(VanO, TotO, Van, Mid, Tot, Mid, Tot, Van) :-	% verhoog de ondergrens (Van weg)
  Mid - VanO < TotO - Mid, !.					% Mid dichterbij onder 
bepaalNieuweVanTot1(_VanO, _TotO, Van, Mid, Tot, Van, Mid, Tot).	% verlaag de bovengrens (Tot weg)
  
bepaalNieuweVanTot(Win) :-
  TijdstipWin = win_GetCtlHandle(Win, idc_verhnieuw_van),
  lbox_GetSel(TijdstipWin,_ItemList,IndexList),
  IndexList = [],			% geen geselecteerd
  assert(vanTot(-1, -1)), !.		% doe niets verder
bepaalNieuweVanTot(Win) :-
  TijdstipWin = win_GetCtlHandle(Win, idc_verhnieuw_van),
  lbox_GetSel(TijdstipWin,_ItemList,IndexList),
  IndexList = [VanTot],			% één geselecteerd
  assert(vanTot(-1, VanTot)), !.		% doe niets verder
bepaalNieuweVanTot(Win) :-
  TijdstipWin = win_GetCtlHandle(Win, idc_verhnieuw_van),
  lbox_GetSel(TijdstipWin,_ItemList,IndexList),
  IndexList = [Van, Tot],		% twee geselecteerd
  assert(vanTot(Van, Tot)), !.		% doe niets verder
bepaalNieuweVanTot(Win) :-
  TijdstipWin = win_GetCtlHandle(Win, idc_verhnieuw_van),
  lbox_GetSel(TijdstipWin,_ItemList,IndexList),
  IndexList = [Van, Mid, Tot],
  vanTot(VanO, TotO),
  bepaalNieuweVanTot1(VanO, TotO, Van, Mid, Tot, VanN, TotN, Weg),
  assert(vanTot(VanN, TotN)),
  lbox_SetSel(TijdstipWin, Weg, b_False), !.
bepaalNieuweVanTot(_Win) :-
  beep().

updateVerhinderingTekst(Win) :-
  VWin = win_GetCtlHandle(Win, idct_verhinderd),
  updateDagTekst(Win, TekstD, C1),
  updateTijdstipTekst(Win, TekstT, _, C2),
  format(Tekst, "%s\n%s", TekstD, TekstT),
  win_SetText(VWin, Tekst),
  bitand(C1, C2, Compleet),
  dialog_SetState(Win, [enable(idc_ok, Compleet)]).

updateDagTekst(Win, Tekst, Compleet) :-
  DagWin = win_GetCtlHandle(Win, idc_verhnieuw_dag),
  lbox_GetSel(DagWin,DagenList,_IndexList),
  updateDagTekst1(DagenList, Tekst, Compleet).

updateDagTekst1([], "", b_False) :-!.
updateDagTekst1([Dag], Dag, b_True) :- !.
updateDagTekst1([Dag1, Dag2], Tekst, b_True) :-
  format(Tekst, "%s en \n%s", Dag1, Dag2),  !.
updateDagTekst1([Dag1, Dag2 | _], Tekst, b_True) :-
  format(Tekst, "%s,\n%s,  etc.", Dag1, Dag2),  !.

updateTijdstipTekst(_Win, "", per(0, 0), b_False) :-
  vanTot(-1, -1), !.
updateTijdstipTekst(Win, "hele dag", per(0, 125), b_True) :-
  TijdstipWin = win_GetCtlHandle(Win, idc_verhnieuw_van),
  NoOfItems = lbox_CountAll(TijdstipWin),
  vanTot(Van, Tot),
  Van <= 0,
  Tot >= NoOfItems - 1, !.
updateTijdstipTekst(Win, Tekst, per(0, TotT), b_True) :-
  TijdstipWin = win_GetCtlHandle(Win, idc_verhnieuw_van),
  vanTot(-1, Tot),
  TotI = lbox_GetItem(TijdstipWin, Tot),
  str_int(TotI, TotT),
  tijd_str(TotT, TotS, _),
  format(Tekst, "tot %s", TotS), !.
updateTijdstipTekst(Win, Tekst, per(VanT, TotT), b_True) :-
  TijdstipWin = win_GetCtlHandle(Win, idc_verhnieuw_van),
  vanTot(Van, Tot),
  VanI = lbox_GetItem(TijdstipWin, Van),
  str_int(VanI, VanT),
  tijd_str(VanT, VanS, _),
  TotI = lbox_GetItem(TijdstipWin, Tot),
  str_int(TotI, TotT),
  tijd_str(TotT, TotS, _),
  format(Tekst, "%s tot %s", VanS, TotS), !.
updateTijdstipTekst(_Win, "??", per(0,0), b_False).

checkUitKantoor(Verhinderingen, UitKant, 0) :-
  UitKant > 0,
  getbacktrack(Here),
  periode_member(Verhindering, Verhinderingen),
  Verhindering = per(_, Tot),
  bitand(Tot, $7F, Hoelaat),
  bitright(Tot, 7, Dag),
  dag(Dag, _, w),		% uit kantoortijd geldt
  Hoelaat < UitKant,
  cutbacktrack(Here),
  1 = dlg_MessageBox( "Uit kantoor", "Er zijn verhinderingen op werkdagen\ndie eindigen vóór de uitkantoortijd!\n\nUitkantoortijd voor deze persoon wissen?",
   mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ), !.
checkUitKantoor(_Verhinderingen, UitKant, Uitkant).

verhindertijd(N, TijdStr) :-
  str_int(TijdStr, N).
verhindertijd(N, TijdStr) :-
  roosterinterval(RI),
  N < 23 * (60 div RI) + 3,	% 23 uur stop
  N1 = N + 1,
  verhindertijd(N1, Tijdstr).

kleur(_, -1, -1, color_Black, []) :- !.
kleur(ItemId, Van, _Tot, color_Black, []) :-
  ItemId < Van, !.
kleur(ItemId, _Van, Tot, color_Black, []) :-
  ItemId >  Tot, !.
kleur(_, _, _, color_Red, [fs_Bold]).

%BEGIN_DLG Verhindernieuw
/**************************************************************************
	Creation and event handling for dialog: Verhindernieuw
**************************************************************************/

CONSTANTS

%BEGIN Verhindernieuw, CreateParms, 22:11:02-4.3.2012, Code automatically updated!
  dlg_verhindernieuw_ResID = idd_verhindernieuw
  dlg_verhindernieuw_DlgType = wd_Modal
  dlg_verhindernieuw_Help = idh_verhinderingen
%END Verhindernieuw, CreateParms

PREDICATES

  dlg_verhindernieuw_eh : EHANDLER
  dlg_verhindernieuw_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST,verhl,verhl)
  dlg_verhindernieuw_update(DIALOG_VAL_LIST,verhl,verhl)
  no_handler : EHANDLER

CLAUSES

  no_handler(_,_,0):-fail.

  dlg_verhindernieuw_Create(Parent, Sp, Verhinderingen, NieuweVerhinderingen):-
	assert(currentPersoon(Sp)),
	findall(Dag, dag(_, Dag, _), Dagen),
	VERHNIEUW_DAG_SELECT = [0],
	VERHNIEUW_DAG_ITEMLIST = Dagen,
	VERHNIEUW_VAN_SELECT = [],
	findall(VerhTijd, verhindertijd(1, VerhTijd), 	VERHNIEUW_VAN_ITEMLIST),
	IDCT_WELKEELKE_ITEMLIST = ["werkdag","maandag","dinsdag","woensdag","donderdag","vrijdag","zaterdag","zondag"],
%MARK Verhindernieuw, new variables

	dialog_CreateModal(Parent,dlg_verhindernieuw_ResID,"",
  		[
%BEGIN Verhindernieuw, ControlList, 22:11:02-4.3.2012, Code automatically updated!
		df(idc_verhnieuw_dag,listbox(VERHNIEUW_DAG_ITEMLIST,VERHNIEUW_DAG_SELECT),nopr),
		df(idc_verhnieuw_van,listbox(VERHNIEUW_VAN_ITEMLIST,VERHNIEUW_VAN_SELECT),nopr),
		df(idct_welkeelke,listbox(IDCT_WELKEELKE_ITEMLIST,[]),nopr)
%END Verhindernieuw, ControlList
		],
		dlg_verhindernieuw_eh,0,VALLIST,ANSWER),
	dlg_verhindernieuw_handle_answer(ANSWER,VALLIST, 
					Verhinderingen, NieuweVerhinderingen).

  dlg_verhindernieuw_handle_answer(idc_ok,VALLIST, X, Y):-!,
	dlg_verhindernieuw_update(VALLIST,X,Y).
  dlg_verhindernieuw_handle_answer(idc_cancel,_,X,X):-!.  % Handle Esc and Cancel here
  dlg_verhindernieuw_handle_answer(_,_,X,X):-
	errorexit().

  dlg_verhindernieuw_update(_VALLIST,Verhinderingen, Verhinderingen):-
%BEGIN Verhindernieuw, Update controls, 22:11:02-4.3.2012, Code automatically updated!
	dialog_VLGetListBox(idc_verhnieuw_dag,_VALLIST,_VERHNIEUW_DAG_ITEMLIST,_VERHNIEUW_DAG_SELECT),
	dialog_VLGetListBox(idc_verhnieuw_van,_VALLIST,_VERHNIEUW_VAN_ITEMLIST,_VERHNIEUW_VAN_SELECT),
	dialog_VLGetListBox(idct_welkeelke,_VALLIST,_IDCT_WELKEELKE_ITEMLIST,_IDCT_WELKEELKE_SELECT),
%END Verhindernieuw, Update controls
	!.
  dlg_verhindernieuw_update(_VALLIST,VerhinderingenUpd, VerhinderingenUpd).

%MARK Verhindernieuw, new events

%BEGIN Verhindernieuw, e_Destroy
  dlg_verhindernieuw_eh(_Win,e_Destroy,0):-
	assert(vantot(-1,-1)),
	!.
%END Verhindernieuw, e_Destroy

%BEGIN Verhindernieuw, e_OwnerMeasureItem
  dlg_verhindernieuw_eh(_Win,e_OwnerMeasureItem(_CtlType,_CtlId,_ItemId,_Data),Size):-
	Font = font_Create(ff_Fixed, [], 8),
        FontWin = win_Create(w_Child,rct(0,0,0,0),"",no_menu,_Win,[],no_handler,0),
	win_SetFont(FontWin, Font),
	win_GetTextExtent(FontWin, "23:", -1, Width, He),
	W = Width + 2,
	assert(tabF(W)),
	win_Destroy(FontWin),
	Height =  He  - 2,
	bitleft(Height, 16, H1),
	Size = Width + H1,
	!.
%END Verhindernieuw, e_OwnerMeasureItem

%BEGIN Verhindernieuw, e_OwnerDraw
  dlg_verhindernieuw_eh(_Win,e_OwnerDraw(_CtlType,_CtlId,ItemId,_ItemAction,
		ItemState,LboxWin,RectItem,_ItemData),0):-
	own_member1(ItemState, odstate_Selected), 
	Item = lbox_GetItem(LboxWin, ItemId),		% selected
	RectItem = rct(L, T, R, B),
	B1 = B + 1,
	str_int(Item, Tijd),
	roosterinterval(RI),
	Tijd mod (60 div RI) = 0,
	Value = vpi_GetAttrVal(attr_color_highlight),
	win_SetPen(LboxWin,pen(1,ps_Solid,Value)),
	win_SetBrush(LboxWin,brush(pat_Solid,Value)),
	draw_Rect(LboxWin,rct(L, T, R, B1)),
	win_SetBackColor(LboxWin,Value),
	win_SetForeColor(LboxWin, color_White),
	Font = font_Create(ff_Fixed, [], 9),
	win_SetFont(LboxWin, Font),
	tijd_str(Tijd, TijdStr,_),
	frontstr(3, TijdStr, Uur, Min),
	tabf(W),
	draw_TextInRect(LboxWin,rct(0, T, W, B1), Uur, -1,[dtext_right, dtext_vcenter]),
	draw_TextInRect(LboxWin,rct(W, T, R, B1), Min, -1,[dtext_left, dtext_vcenter]),
	!.
  dlg_verhindernieuw_eh(_Win,e_OwnerDraw(_CtlType,_CtlId,ItemId,_ItemAction,
		ItemState,LboxWin,RectItem,_ItemData),0):-
	own_member1(ItemState, odstate_Selected),
	Item = lbox_GetItem(LboxWin, ItemId),		% selected
	RectItem = rct(L, T, R, B),
	B1 = B + 1,
	str_int(Item, Tijd),
	roosterinterval(RI),
	Tijd mod (60 div RI) <> 0,
	Value = vpi_GetAttrVal(attr_color_highlight),
	win_SetPen(LboxWin,pen(1,ps_Solid,Value)),
	win_SetBrush(LboxWin,brush(pat_Solid,Value)),
	draw_Rect(LboxWin,rct(L, T, R, B1)),
	win_SetBackColor(LboxWin,Value),
	win_SetForeColor(LboxWin, color_White),
	Font = font_Create(ff_Fixed, [], 8),
	win_SetFont(LboxWin, Font),
	tijd_str(Tijd, TijdStr,_),
	frontstr(3, TijdStr, _Uur, Min),
	tabf(W),
	draw_TextInRect(LboxWin,rct(W, T, R, B1), Min, -1,[dtext_left, dtext_vcenter]),
	!.
  dlg_verhindernieuw_eh(_Win,e_OwnerDraw(_CtlType,_CtlId,ItemId,_ItemAction,
		_ItemState,LboxWin,RectItem,_ItemData),0):-
	Item = lbox_GetItem(LboxWin, ItemId),
	RectItem = rct(L, T, R, B),
	B1 = B + 1,
	vanTot(Van, Tot),
	kleur(ItemId, Van, Tot, Kleur, FSL),
	str_int(Item, Tijd),
	roosterinterval(RI),
	Tijd mod (60 div RI) = 0,
	win_SetPen(LboxWin,pen(1,ps_Solid,color_White)),
	win_SetBrush(LboxWin,brush(pat_Solid,color_White)),
	draw_Rect(LboxWin,rct(L, T, R, B1)),
	win_SetBackColor(LboxWin,color_White),
	win_SetForeColor(LboxWin, Kleur),
	win_SetBackMode(LboxWin,bk_Transparent),
	Font = font_Create(ff_Fixed, FSL, 9),
	win_SetFont(LboxWin, Font),
	tijd_str(Tijd, TijdStr,_),
	frontstr(3, TijdStr, Uur, Min),
	tabf(W),
	draw_TextInRect(LboxWin,rct(0, T, W, B1), Uur, -1,[dtext_right, dtext_vcenter]),
	draw_TextInRect(LboxWin,rct(W, T, R, B1), Min, -1,[dtext_left, dtext_vcenter]),
	!.
  dlg_verhindernieuw_eh(_Win,e_OwnerDraw(_CtlType,_CtlId,ItemId,_ItemAction,
		_ItemState,LboxWin,RectItem,_ItemData),0):-
	Item = lbox_GetItem(LboxWin, ItemId),
	RectItem = rct(L, T, R, B),
	B1 = B + 1,
	vanTot(Van, Tot),
	kleur(ItemId, Van, Tot, Kleur, FSL),
	str_int(Item, Tijd),
	roosterinterval(RI),
	Tijd mod (60 div RI) <> 0,
	win_SetPen(LboxWin,pen(1,ps_Solid,color_White)),
	win_SetBrush(LboxWin,brush(pat_Solid,color_White)),
	draw_Rect(LboxWin,rct(L, T, R, B1)),
	win_SetBackColor(LboxWin,color_White),
	win_SetForeColor(LboxWin, Kleur),
	Font = font_Create(ff_Fixed, FSL, 8),
	win_SetFont(LboxWin, Font),
	tijd_str(Tijd, TijdStr,_),
	frontstr(3, TijdStr, _Uur, Min),
	tabf(W),
	draw_TextInRect(LboxWin,rct(W, T, R, B1), Min, -1,[dtext_left, dtext_vcenter]),
	!.
%END Verhindernieuw, e_OwnerDraw

%BEGIN Verhindernieuw, idc_alle1 _CtlInfo
  dlg_verhindernieuw_eh(_Win,e_Control(idc_alle1,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idc_verhnieuw_dag),
	selectAlle(LboxWin),
	dialog_SetState(_Win, [
		enable(idc_verhnieuw_van, b_True),
		show(idc_verhnieuw_van, b_True),
		show(idct_vanaf, b_True),
		show(idct_klik_op_tijdstippen, b_False),
		show(idc_hele_dag, b_True),
		show(idc_pbgeen, b_True)
		]),
	updateVerhinderingTekst(_Win),
	!.
%END Verhindernieuw, idc_alle1 _CtlInfo

%BEGIN Verhindernieuw, idc_geen1 _CtlInfo
  dlg_verhindernieuw_eh(_Win,e_Control(idc_geen1,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idc_verhnieuw_dag),
	selectGeen(LboxWin),
	assert(vanTot(-1,-1)),
	TWin = win_GetCtlHandle(_Win, idc_verhnieuw_van),
	selectGeen(TWin),
	dialog_SetState(_Win, [
		enable(idc_verhnieuw_van, b_False),
		show(idc_verhnieuw_van, b_False),
		show(idct_vanaf, b_False),
		show(idct_klik_op_tijdstippen, b_True),
		show(idc_hele_dag, b_False),
		show(idc_pbgeen, b_False),
		enable(idc_ok, b_False)
		]),
	updateVerhinderingTekst(_Win),
	!.
%END Verhindernieuw, idc_geen1 _CtlInfo

%BEGIN Verhindernieuw, idct_welkeelke selchanged
  dlg_verhindernieuw_eh(_Win,e_Control(idct_welkeelke,_CtrlType,ElkeWin,selchanged),0):-
	LboxWin = win_GetCtlHandle(_Win, idc_verhnieuw_dag),
	0 = lbox_GetSelIndex(ElkeWin),		% alle werkdagen
	findall(DagX, dag(DagX, _, _), DagXL),
	elkeWerkdag(DagXL, 0, NList),
	not(NList = []),
	reverse(NList, [], NListR),		% van achter naar voren
	zetSelect(LboxWin, NListR), 
	dialog_SetState(_Win, [
		enable(idc_verhnieuw_van, b_True),
		show(idc_verhnieuw_van, b_True),
		show(idct_vanaf, b_True),
		show(idct_klik_op_tijdstippen, b_False),
		show(idc_hele_dag, b_True),
		show(idc_pbgeen, b_True)
		]),
	lbox_SetSel(ElkeWin, 0, b_False),
	updateVerhinderingTekst(_Win),
	!.
  dlg_verhindernieuw_eh(_Win,e_Control(idct_welkeelke,_CtrlType,ElkeWin,selchanged),0):-
	LboxWin = win_GetCtlHandle(_Win, idc_verhnieuw_dag),
	Index = lbox_GetSelIndex(ElkeWin),
	Index <> 0,
	findall(DagX, dag(DagX, _, _), DagXL),
	I = Index,   %	5.2.2003 (werkdag toegevoegd) was + 1,  dag van de week maandag = 1
	findall(N, soortgelijkeDagen(I, DagXL, 0, N), NList),
	reverse(NList, [], NListR),		% van achter naar voren
	zetSelect(LboxWin, NListR), 
	dialog_SetState(_Win, [
		enable(idc_verhnieuw_van, b_True),
		show(idc_verhnieuw_van, b_True),
		show(idct_vanaf, b_True),
		show(idct_klik_op_tijdstippen, b_False),
		show(idc_hele_dag, b_True),
		show(idc_pbgeen, b_True)
		]),
	lbox_SetSel(ElkeWin, Index, b_False),
	updateVerhinderingTekst(_Win),
	!.
%END Verhindernieuw, idct_welkeelke selchanged

%BEGIN Verhindernieuw, idc_help _CtlInfo
  dlg_verhindernieuw_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Verhinderingen"),
	!.
%END Verhindernieuw, idc_help _CtlInfo

%BEGIN Verhindernieuw, idc_ok _CtlInfo
  dlg_verhindernieuw_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	ParentWin = win_GetParent(_Win),
	%TijdenWin = win_GetCtlHandle(_Win, idc_verhnieuw_van),
	updateTijdstipTekst(_Win, _, PeriodeD,_),
	DagenWin = win_GetCtlHandle(_Win, idc_verhnieuw_dag),
	lbox_GetSel(DagenWin,ItemList,_IndexList),
	member(Dag, ItemList),
	dag(DagNo, Dag, _),
	bitleft(DagNo, 7, DagTijd),
	PeriodeD = per(Van, Tot),	
	bitor(Van, DagTijd, VanX),
	bitor(Tot, DagTijd, TotX),
	Periode = per(VanX, TotX),
	check_per(Periode),  
	%ElkeWin = win_GetCtlHandle(_Win, idct_welkeelke),
	%selectGeen(ElkeWin),
	getbacktrack(Here),
	persoon(ParentWin, Persno, NaamN, VerhlO, Sex, ES, DS, Uitkant),
	cutbacktrack(Here),
	%sortverhinderingen([Periode|VerhlO],VerhinderingenSorted),
	merge_verh([Periode|VerhlO], VerhinderingenUpd, _Overlap),
	%write("Overlap: ", Overlap),
	getbacktrack(Daar),
	retract(persoon(ParentWin, PersNo, _, _, _, _, _, _)),
	cutbacktrack(Daar),
	assert(persoon(ParentWin, PersNo, NaamN, VerhinderingenUpd, Sex, Es, DS, Uitkant)),
	fail.
  dlg_verhindernieuw_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0) :-
	ParentWin = win_GetParent(_Win),
	retract(persoon(ParentWin, PersNo, NaamN, Verhinderingen, Sex, ES, DS, Uitkant)),
	checkUitKantoor(Verhinderingen, UitKant, UitkantNieuw),
	assert(persoon(ParentWin, PersNo, NaamN, Verhinderingen, Sex, ES, DS, UitkantNieuw)),
	win_PostEvent(ParentWin,e_User(upd_verhinderingen, 0)),
	DagenWin = win_GetCtlHandle(_Win, idc_verhnieuw_dag),
	selectGeen(DagenWin),
	updateVerhinderingTekst(_Win), !.
%	dialog_SetStr(_Win, idct_verhinderd, ""), !.
%END Verhindernieuw, idc_ok _CtlInfo

%BEGIN Verhindernieuw, idc_pbgeen _CtlInfo
  dlg_verhindernieuw_eh(_Win,e_Control(idc_pbgeen,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idc_verhnieuw_van),
	assert(vanTot(-1, -1)),
	selectGeen(LboxWin),
%	dialog_SetStr(_Win, idct_verhinderd, ""),
	updateVerhinderingTekst(_Win),
	!.
%END Verhindernieuw, idc_pbgeen _CtlInfo

%BEGIN Verhindernieuw, idc_verhnieuw_van selchanged
  dlg_verhindernieuw_eh(_Win,e_Control(idc_verhnieuw_van,_CtrlType,_CtrlWin,selchanged),0):-
	bepaalNieuweVanTot(_Win),
	updateVerhinderingTekst(_Win),
	win_Invalidate(_Win),
	!.
  dlg_verhindernieuw_eh(_Win,e_Control(idc_verhnieuw_van,_CtrlType,_CtrlWin,selchanged),0):-!,
	dialog_SetStr(_Win, idct_verhinderd, "").
%END Verhindernieuw, idc_verhnieuw_van selchanged

%BEGIN Verhindernieuw, idc_hele_dag _CtlInfo
  dlg_verhindernieuw_eh(_Win,e_Control(idc_hele_dag,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idc_verhnieuw_van),
	NoOfItems = lbox_CountAll(LboxWin),
	N = NoOfItems - 1,
	assert(vanTot(0, N)),
	updateVerhinderingTekst(_Win),
	win_Invalidate(LboxWin),
	!.
%END Verhindernieuw, idc_hele_dag _CtlInfo

%BEGIN Verhindernieuw, e_Create
  dlg_verhindernieuw_eh(_Win,e_Create(_CreationData),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idc_verhnieuw_dag),
	ElkeWin = win_GetCtlHandle(_Win, idct_welkeelke),
	Font = font_Create(ff_Fixed, [], 10),
	win_SetFont(LboxWin, Font),
	win_SetFont(ElkeWin, Font),
	lbox_SetSel(LboxWin, 0, b_False),
	dialog_SetState(_Win, [
		enable(idc_verhnieuw_van, b_False),
		show(idc_verhnieuw_van, b_False),
		show(idct_vanaf, b_False)
		]),
	VanWin = win_GetCtlHandle(_Win, idc_verhnieuw_van),
	win_SetFont(VanWin, Font),
	lbox_SetTopIndex( VanWin, 25),
	win_SetState(VanWin, [wsf_Disabled, wsf_Invisible]),
	lbox_SetTopIndex( LboxWin, 12),
	!.
%END Verhindernieuw, e_Create

%BEGIN Verhindernieuw, idc_verhnieuw_dag selchanged
  dlg_verhindernieuw_eh(_Win,e_Control(idc_verhnieuw_dag,_CtrlType,_LboxWinD,selchanged),0):-
	dialog_SetState(_Win, [
		enable(idc_verhnieuw_van, b_True),
		show(idc_verhnieuw_van, b_True),
		show(idct_vanaf, b_True),
		show(idct_klik_op_tijdstippen, b_False),
		show(idc_hele_dag, b_True),
		show(idc_pbgeen, b_True)
		]),
	updateVerhinderingTekst(_Win),
	!.
  dlg_verhindernieuw_eh(_Win,e_Control(idc_verhnieuw_dag,_CtrlType,_LboxWinD,selchanged),0):-
	updateVerhinderingTekst(_Win), !.
	% dialog_SetStr(_Win, idct_verhinderd, ""), !.
%END Verhindernieuw, idc_verhnieuw_dag selchanged

  dlg_verhindernieuw_eh(_,_,_):-!,fail.

%END_DLG Verhindernieuw




%BEGIN_DLG Toernooibeleid
/**************************************************************************
	Creation and event handling for dialog: Toernooibeleid
**************************************************************************/

constants

%BEGIN Toernooibeleid, CreateParms, 13:57:35-18.9.2016, Code automatically updated!
  dlg_toernooibeleid_ResID = idd_toernooibeleid
  dlg_toernooibeleid_DlgType = wd_Modal
  dlg_toernooibeleid_Help = idh_toernooibeleid
%END Toernooibeleid, CreateParms

predicates

  dlg_toernooibeleid_eh : EHANDLER
  dlg_toernooibeleid_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_toernooibeleid_update(DIALOG_VAL_LIST)
  nr2dialog(integer, dialog_int)
procedure  saveBet(BOOLEAN)
procedure  saveDoorpl(BOOLEAN)
procedure  saveToew(BOOLEAN)
procedure  saveWaars(BOOLEAN)
procedure  saveTijd(dialog_int, dialog_int)
procedure  saveMaxTijdTussen(dialog_int)
procedure  saveAantD(dialog_int)
procedure  saveAantO(dialog_int, integer)
procedure saveduur(dialog_int)
procedure savePouleTelling(integer, boolean, string)
%procedure saveOpenbaar(BOOLEAN)
procedure saveEnkel(dialog_int)
determ beleidNietVeranderd(boolean BetD,boolean PlD,boolean Locatie,boolean OprD, dialog_int AantDagD,dialog_int AantOndD,dialog_int DuurD,dialog_int SingelsIn,dialog_int TussenD, integer PouleT, integer PerWat, janee BanenTool)
nondeterm pouletellingMogelijkheid(integer Selectie, string Type)
%procedure pouletellingMogelijkheidD(integer Selectie, string Type)
%procedure pouletellingPresel(string, slist, integer, integer) - (i,i,i,o)
procedure saveBanentool(janee)
procedure getUitslagenableWaarden(boolean, string)
procedure saveuitslagenable(boolean, string)
procedure saveTshirtchecked(boolean, string)
procedure savedubbelvoorenkel(boolean)
procedure indexvanpouletelling(string, slist, integer, integer)
procedure
 pouletellingmogelijkheidP(integer, string)
clauses

nr2dialog(0, void()) :- !.
nr2dialog(Nr, i(Nr)) :- !.

saveBet(BetD) :-
  janee2bool(Bet, BetD),
  not(betaal_adm(Bet)),
  logf('Z', betaal_adm(Bet),nee), 
  update_betalingen(), !.
saveBet(_BetD).

saveDoorpl(PlD) :-
  janee2bool(Pl, PlD),
  not(door_plannen(Pl)),
  logf('Z', door_plannen(Pl),nee), !.
saveDoorpl(_).

saveToew(BaanD) :-
  janee2bool(Baan, BaanD),
  not(baan_adm(Baan)),
  logf('Z', baan_adm(Baan),nee), !.
saveToew(_).

saveWaars(OprD) :-
  janee2bool(Opr, OprD),
  not(oproepen(Opr)),
  logf('Z', oproepen(Opr),nee), !.
saveWaars(_).  

/*
saveOpenbaar(OprD) :-
  janee2bool(Opr, OprD),
  not(openbaar(Opr)),
  logf('Z', openbaar(Opr),nee), !.
saveOpenbaar(_).  
*/
	
saveTijd(TDuur, TussenD) :-
  nr2dialog(Duur, TDuur),
  nr2dialog(Tussen, TussenD),
  Duur > Tussen,
  not(separatie(Duur)),
  logf('Z', separatie(Duur),nee), !.
saveTijd(_TDuur, TussenD) :-
  nr2dialog(Tussen, TussenD),
  not(separatie(Tussen)),
  logf('Z', separatie(Tussen),nee), !.
saveTijd(_, _).

saveMaxTijdTussen(Tijd) :-
  nr2dialog(MTijd, Tijd),
  not(maxseparatie(MTijd)),
  logf('Z', maxseparatie(MTijd),nee), !.
saveMaxTijdTussen(_).

savedubbelvoorenkel(DvoorE) :-
  janee2bool(DVEjanee, DvoorE),
  not(dubbelVoorEnkel(DVEjanee)),
  logf('Z', dubbelvoorenkel(DVEJaNee), nee), !.
savedubbelvoorenkel(_).

saveAantD(AantDagD) :-
  nr2dialog(AantDag, AantDagD),
  AantDag1 = AantDag - 1,
  not(dag_max(AantDag1)),
  logf('Z', dag_max(AantDag1),nee), !.
saveAantD(_).
	
saveAantO(AantOndD, PerWat) :-
  nr2dialog(AantOnd, AantOndD),
  not(dag_max_o(AantOnd, PerWat)),
  logf('Z',dag_max_o(AantOnd, PerWat),nee), !.
saveAantO(_, _).

saveDuur(DuurD) :-
  nr2dialog(Duur, DuurD),
  not(duur(Duur)),
  logf('Z',duur(Duur),nee), !.
saveDuur(_).

saveEnkel(SinglesD) :-
  nr2dialog(Singles, SinglesD),
  not(singles(Singles)),
  logf('Z',singles(Singles), nee), !.
saveEnkel(_).

saveBanentool(BanenTool) :-
  banentool(_, Maxbanen),
  not(banentool(Banentool, _)),
  close_uitslaginv(),
  logf('Z', banentool(BanenTool, Maxbanen),nee), !.
saveBanentool(_BanenTool).

savePouleTelling(PouleT, Enabled, MASTERPIN) :-
  %pouletelling(TX),
  %writedevice(X),
  %writedevice(screen),
  %getbacktrack(Here),
  %pouletellingmogelijkheid(TX, TXs),
  %pouletellingmogelijkheid(PouleT, PouleTs),
  %cutbacktrack(Here),
  %writef("\nwas %d (%s), wordt %d (%s)", TX, TXs, PouleT, PouleTs),
  %writedevice(X),
  pouleTelling(PouleT), !, 
  saveUitslagenable(Enabled, MASTERPIN), !.
savePouleTelling(PouleT, Enabled, MASTERPIN) :-
  saveUitslagenable(Enabled, MASTERPIN),
  logf('Z',pouletelling(PouleT), nee), 
  w3(_, _, _, _, _, _, _),
  close_alles(),
  dlg_MessageBox( "Poule", "Puntentoekenning gewijzigd.\nReeds toegekende punten worden niet automatisch aangepast!", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  !.
savePouleTelling(_PouleT, _, _).

beleidNietVeranderd(BetD,PlD,Locatie,OprD,AantDagD,AantOndD,DuurD,SingelsIn,TussenD,PouleT, PerWat, BanenTool) :-
  janee2bool(Bet, BetD),
  betaal_adm(Bet),
  janee2bool(Pl, PlD),
  door_plannen(Pl),
  janee2bool(Baan, Locatie),
  baan_adm(Baan),
  janee2bool(Opr, OprD),
  oproepen(Opr),
  nr2dialog(AantDag, AantDagD),
  AantDag1 = AantDag - 1,
  dag_max(AantDag1),
  nr2dialog(AantOnd, AantOndD),
  dag_max_o(AantOnd, PerWat),
  nr2dialog(Duur, DuurD),
  duur(Duur),
  nr2dialog(Singles, SingelsIn),
  singles(Singles),
  banentool(BanenTool, _Maxbanen),
  nr2dialog(Duur, TussenD),
  separatie(Duur), 
  pouletelling(PouleT), !.

pouletellingMogelijkheid(0,"Standaard").
pouletellingMogelijkheid(5,"Standaard met gelijk spel").
pouletellingMogelijkheid(1,"Alternatief (zie help)").
pouletellingMogelijkheid(2,"Eigen telling").
pouletellingMogelijkheid(3,"Eigen telling (verliezer=0)").
pouleTellingMogelijkheid(4,"Alternatief (2)").

pouletellingmogelijkheidP(PT, String) :-
  pouletellingmogelijkheid(PT, String), !.
pouletellingmogelijkheidP(_, "").
/*
pouleTellingMogelijkheidD(In,Tekst) :-
  pouleTellingMogelijkheid(In,Tekst), !.
pouleTellingMogelijkheidD(_,"Standaard").

pouletellingPresel(Tekst, [Tekst|_], Presel, Presel) :- !.
pouletellingPresel(Tekst, [_|List], In, Uit) :-
  In1 = In + 1, !,
  pouletellingPresel(Tekst, List, In1, Uit).
pouletellingPresel(_, _, _, 0).
*/
getUitslagenableWaarden(b_true, Masterpin) :-
  uitslagenable(Masterpin), !.
getUitslagenableWaarden(b_False, "").

saveUitslagenable(b_true, MASTERPIN) :-
  uitslagenable(Masterpin), !.
saveUitslagenable(b_true, MASTERPIN) :-
  logf('Z', uitslagenable(Masterpin), nee), !, 
  setUitslagenableMasterrecord("aan", MasterPin).
saveUitslagenable(_, Masterpin) :-
  uitslagenable(_), !,
  logf('R', uitslagenable(Masterpin), nee),
  setUitslagenableMasterrecord("uit", "").
saveUitslagenable(_, _).

indexvanPouletelling(Entry, [Entry| _], N, N) :- !.
indexvanpouletelling(Entry, [_ | List], Nin, Nuit) :-
  N = Nin + 1, !,
  indexvanpouletelling(Entry, List, N, Nuit).
indexvanPouletelling(_, _, N, N) :- 
  dlg_note("boe"), !.

setUitslagenableMasterrecord(Op, Pin) :-
  get_tempdir(Dir1,_),
  %getfolder(TempDir,Dir1,_,_),
  filenamepath(FF, Dir1, "uitslagenable.txt"),
  file_str(FF, "doet er niet toe"),
  not(geenServer(_)),
  providerx(Provider, _, "", _),
  format(URL, "http://%s/uitslagenable.php", Provider),
  X = aanmeldenvars(),
  filenamepath(UETempfile, Dir1, "uitslagenable1.txt"),
  pouletelling(Telling),
  str_int(TellingS, Telling),
  ReturnUE = fileupload(URL, FF, UETempFile, ["pin", Pin, "op", Op, "puntentelling", TellingS | X]),
  upper_lower(OpU, Op),
  format(Msg, "Uitslaginvoer ter plaatse is %s gezet.\nVervers nu zelf de webpagina's op de ToernooiKlapper (met Alt-W).", OpU),
  dlg_MessageBox( "Uitslaginvoer ter plaatse", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  checkserver(ReturnUE), !.
setUitslagenableMasterrecord(_Op, _Pin).

  dlg_toernooibeleid_Create(Parent):-
	%checkPaswoord(Parent),
	oproepen(Opr),
	janee2bool(Opr,IDCBS_WAARSCHUWADMINISTRATIE_CHECKED),
	betaal_adm(Bet),
	janee2bool(Bet, IDCBS_BETAALADMINISTRATIE_CHECKED),
	door_plannen(Plan),
	janee2bool(Plan, IDCBS_DOORPLANNEN_CHECKED),
	baan_adm(Baan),
	janee2bool(Baan, IDCBS_BAANTOEWIJZING_CHECKED),
	separatie(TijdTussen),
	nr2dialog(TijdTussen, IDCBS_TIJDTUSSEN_VALUE),
	maxseparatie(MaxTijdTussen),
	nr2dialog(MaxTijdTussen, IDCBS_MAXTIJDTUSSEN_VALUE),
	dag_max(MaxPerDg),
	MaxPerDg1 = MaxPerDg + 1,
	nr2dialog(MaxPerDg1, IDCBS_AANTPERDAG_VALUE),
	dag_max_o(MaxPerOnd, IDCBS_WEEK),
	nr2dialog(MaxPerOnd, IDCBS_AANTPERONDPERDAG_VALUE),
	duur(Duur),
	IDCBS_WEDSDUUR = i(Duur),
	singles(Singles),
	IDCBS_SINGLESIN = i(Singles),
	pouletelling(PTis),
	pouletellingmogelijkheidP(PTis, PTstring),
	findall(PT, pouletellingMogelijkheid(_,PT), IDBCS_POULETELLINGL),
	indexvanpouletelling(PTString, IDBCS_POULETELLINGL, 0, IndexPT),
	%writedevice(XD),
	%writedevice(screen),
	%write("\nPTs: ",PTstring),
	%write("\nList: ",IDBCS_POULETELLINGL),
	%write("\nIndex: ",IndexPT,"\n"),
	%writedevice(XD),
	IDCBS_WEEK_ITEMLIST = ["per dag","per 2 dagen","per week","per 2 weken","per vier weken", "per vijf weken"],
	getUitslagenableWaarden(IDCBS_UITSLAGIVOER_CHECKED, Masterpin),
	roosterinterval(RI),
	format(IDCTBS_KW_TITLE, "x %d min.", RI),
	format(IDCBS_TOERNOOIBELEID_18_TITLE, "x %d min.", RI),
	tshirt(IDC_TSHIRTMELDING_CHECKED, IDC_TSIRTTEKST_VALUE),
	format(IDCBS_TOERNOOIBELEID_34_TITLE, "x %d min.", RI),
	dubbelvoorenkel(DvoorEjanee),
	janee2bool(DvoorEjanee, IDCBS_GEEN_DUBBEL_VOOR_ENKEL_CHECKED),
%MARK Toernooibeleid, new variables

	dialog_CreateModal(Parent,dlg_toernooibeleid_ResID,"",
  		[
%BEGIN Toernooibeleid, ControlList, 13:57:35-18.9.2016, Code automatically updated!
		df(idcbs_tijdtussen,editint(IDCBS_TIJDTUSSEN_VALUE,[mandatory,range(1,2000)]),dlg_prompt(idctbs_min_tijd_aanvang)),
		df(idcbs_wedsduur,editint(IDCBS_WEDSDUUR,[mandatory,minimum(1)]),dlg_prompt(idcbs_duur)),
		df(idcbs_aantperdag,editint(IDCBS_AANTPERDAG_VALUE,[mandatory,range(1,9)]),nopr),
		df(idcbs_aantperondperdag,editint(IDCBS_AANTPERONDPERDAG_VALUE,[mandatory,range(1,9)]),nopr),
		df(idcbs_week,listbutton(IDCBS_WEEK_ITEMLIST,IDCBS_WEEK),nopr),
		df(idcbs_singlesin,editint(IDCBS_SINGLESIN,[mandatory]),nopr),
		df(idcbs_maxtijdtussen,editint(IDCBS_MAXTIJDTUSSEN_VALUE,[]),nopr),
		df(idcbs_geen_dubbel_voor_enkel,checkbox(IDCBS_GEEN_DUBBEL_VOOR_ENKEL_CHECKED),nopr),
		df(idcbs_betaaladministratie,checkbox(IDCBS_BETAALADMINISTRATIE_CHECKED),nopr),
		df(idcbs_doorplannen,checkbox(IDCBS_DOORPLANNEN_CHECKED),nopr),
		df(idcbs_baantoewijzing,checkbox(IDCBS_BAANTOEWIJZING_CHECKED),nopr),
		df(idcbs_waarschuwadministratie,checkbox(IDCBS_WAARSCHUWADMINISTRATIE_CHECKED),nopr),
		df(idc_tshirtmelding,checkbox(IDC_TSHIRTMELDING_CHECKED),nopr),
		df(idc_tsirttekst,editstr(IDC_TSIRTTEKST_VALUE,[]),nopr),
		df(idcbs_uitslagivoer,checkbox(IDCBS_UITSLAGIVOER_CHECKED),nopr),
		df(idcbs_master_pin,editstr(MASTERPIN,[length(7)]),nopr),
		df(idbcs_pouletelling,listbutton(IDBCS_POULETELLINGL,IndexPT),nopr),
		df(idctbs_kw,statictext(IDCTBS_KW_TITLE),nopr),
		df(idcbs_toernooibeleid_18,statictext(IDCBS_TOERNOOIBELEID_18_TITLE),nopr),
		df(idcbs_toernooibeleid_34,statictext(IDCBS_TOERNOOIBELEID_34_TITLE),nopr)
%END Toernooibeleid, ControlList
		],
		dlg_toernooibeleid_eh,0,VALLIST,ANSWER),
	dlg_toernooibeleid_handle_answer(ANSWER,VALLIST).

  dlg_toernooibeleid_handle_answer(idc_ok,VALLIST):-!,
	dlg_toernooibeleid_update(VALLIST).
  dlg_toernooibeleid_handle_answer(idc_cancel,_):-!.  % Handle Esc and Cancel here
  dlg_toernooibeleid_handle_answer(_,_):-
	errorexit().

  dlg_toernooibeleid_update(_VALLIST):-
%BEGIN Toernooibeleid, Update controls, 13:57:35-18.9.2016, Code automatically updated!
	_IDCBS_BETAALADMINISTRATIE_CHECKED = dialog_VLGetCheck(idcbs_betaaladministratie,_VALLIST),
	_IDCBS_DOORPLANNEN_CHECKED = dialog_VLGetCheck(idcbs_doorplannen,_VALLIST),
	_IDCBS_BAANTOEWIJZING_CHECKED = dialog_VLGetCheck(idcbs_baantoewijzing,_VALLIST),
	_IDCBS_WAARSCHUWADMINISTRATIE_CHECKED = dialog_VLGetCheck(idcbs_waarschuwadministratie,_VALLIST),
	_IDCBS_TIJDTUSSEN_VALUE = dialog_VLGetint(idcbs_tijdtussen,_VALLIST),
	_IDCBS_AANTPERDAG_VALUE = dialog_VLGetint(idcbs_aantperdag,_VALLIST),
	_IDCBS_AANTPERONDPERDAG_VALUE = dialog_VLGetint(idcbs_aantperondperdag,_VALLIST),
	_IDCBS_WEDSDUUR = dialog_VLGetint(idcbs_wedsduur,_VALLIST),
	_IDCBS_SINGLESIN = dialog_VLGetint(idcbs_singlesin,_VALLIST),
	dialog_VLGetListButton(idbcs_pouletelling,_VALLIST,_IDBCS_POULETELLINGL,_IDBCS_POULETELLING),  % vallist, string, integer)
	dialog_VLGetListButton(idcbs_week,_VALLIST,_IDCBS_WEEK_ITEMLIST,_IDCBS_WEEK),
	_IDCBS_UITSLAGIVOER_CHECKED = dialog_VLGetCheck(idcbs_uitslagivoer,_VALLIST),
	_MASTERPIN = dialog_VLGetstr(idcbs_master_pin,_VALLIST),
	_IDC_TSHIRTMELDING_CHECKED = dialog_VLGetCheck(idc_tshirtmelding,_VALLIST),
	_IDC_TSIRTTEKST_VALUE = dialog_VLGetstr(idc_tsirttekst,_VALLIST),
	_IDCBS_MAXTIJDTUSSEN_VALUE = dialog_VLGetint(idcbs_maxtijdtussen,_VALLIST),
	_IDCBS_GEEN_DUBBEL_VOOR_ENKEL_CHECKED = dialog_VLGetCheck(idcbs_geen_dubbel_voor_enkel,_VALLIST),
%END Toernooibeleid, Update controls
	saveBet(_IDCBS_BETAALADMINISTRATIE_CHECKED),
	saveDoorpl(_IDCBS_DOORPLANNEN_CHECKED),
	saveToew(_IDCBS_BAANTOEWIJZING_CHECKED),
	saveWaars(_IDCBS_WAARSCHUWADMINISTRATIE_CHECKED),
	saveAantD(_IDCBS_AANTPERDAG_VALUE),
	saveAantO(_IDCBS_AANTPERONDPERDAG_VALUE, _IDCBS_WEEK),
	saveDuur(_IDCBS_WEDSDUUR),
	saveEnkel(_IDCBS_SINGLESIN),
	saveTijd(_IDCBS_WEDSDUUR, _IDCBS_TIJDTUSSEN_VALUE),
	saveMaxTijdTussen(_IDCBS_MAXTIJDTUSSEN_VALUE),
	savedubbelvoorenkel(_IDCBS_GEEN_DUBBEL_VOOR_ENKEL_CHECKED),
	pouletellingMogelijkheid(PT,_IDBCS_POULETELLINGL),
	savePouleTelling(PT, _IDCBS_UITSLAGIVOER_CHECKED, _MASTERPIN),
	%saveOpenBaar(_IDCE_OPENBAAR_CHECKED),
	%janee2bool(BanenToolJN, _BanenTool),
	saveBanentool(ja),
	saveUitslagenable(_IDCBS_UITSLAGIVOER_CHECKED, _MASTERPIN),
	saveTshirtchecked(_IDC_TSHIRTMELDING_CHECKED, _IDC_TSIRTTEKST_VALUE),
	update_display(),
	update_uitslagen("toernooibeleid"),
	update_waarsch(),
	update_proj_toolbar(),		% update oproepbutton
	!.
  dlg_toernooibeleid_update(_VALLIST).

%MARK Toernooibeleid, new events

%BEGIN Toernooibeleid, idcbs_betaaladministratie _CtlInfo
  dlg_toernooibeleid_eh(_Win,e_Control(idcbs_betaaladministratie,_CtrlType,_CtrlWin,_CtlInfo),0):-
	b_True = win_IsChecked(_CtrlWin),
	%dialog_setState(_Win, [enable(idcbs_banknummers, b_True)]),
	!.
  dlg_toernooibeleid_eh(_Win,e_Control(idcbs_betaaladministratie,_CtrlType,_CtrlWin,_CtlInfo),0):-
	b_False = win_IsChecked(_CtrlWin),
	%dialog_setState(_Win, [enable(idcbs_banknummers, b_False)]),
	!.
%END Toernooibeleid, idcbs_betaaladministratie _CtlInfo

/*
%BEGIN Toernooibeleid, e_Create
dlg_toernooibeleid_eh(_Win,e_Create(_CreationData),0):-
	betaal_adm(nee),
	dialog_setState(_Win, [enable(idcbs_banknummers, b_False)]),
	!.
%E%END Toernooibeleid, e_Create

%BEGIN Toernooibeleid, idcbs_banknummers _CtlInfo
  dlg_toernooibeleid_eh(_Win,e_Control(idcbs_banknummers,_CtrlType,BWin,_CtlInfo),0):-
	b_True = win_IsChecked(BWin),
	banknummers(nee),
	logf('Z', banknummers(ja), nee),
	!.
  dlg_toernooibeleid_eh(_Win,e_Control(idcbs_banknummers,_CtrlType,BWin,_CtlInfo),0):-
	b_False = win_IsChecked(BWin),
	banknummers(ja),
	logf('Z', banknummers(nee), nee),
	!.
%END Toernooibeleid, idcbs_banknummers _CtlInfo
*/
%BEGIN Toernooibeleid, idc_ok _CtlInfo
  dlg_toernooibeleid_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	_IDCBS_UITSLAGIVOER_CHECKED = dialog_GetCheck(_Win, idcbs_uitslagivoer),
	_IDCBS_UITSLAGIVOER_CHECKED = b_true,
	_MASTERPIN = dialog_Getstr(_Win, idcbs_master_pin),
	str_len(_MASTERPIN, Len),
	Len < 6,
	dlg_MessageBox( "Uitslaginvoer ter plaatse", "Het wachtwoord moet 6 - 9 tekens bevatten!\n(de spelers kunnen met hun pincode werken)", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	PinWin = win_GetCtlHandle(_Win, idcbs_master_pin),
	win_SetFocus(PinWin), !.
  dlg_toernooibeleid_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	_IDCBS_BETAALADMINISTRATIE_CHECKED = dialog_GetCheck(_Win, idcbs_betaaladministratie),
	_IDCBS_DOORPLANNEN_CHECKED = dialog_GetCheck(_Win, idcbs_doorplannen),
	_IDCBS_BAANTOEWIJZING_CHECKED = dialog_GetCheck(_Win, idcbs_baantoewijzing),
	_IDCBS_WAARSCHUWADMINISTRATIE_CHECKED = dialog_GetCheck(_Win, idcbs_waarschuwadministratie),
	_IDCBS_TIJDTUSSEN_VALUE = dialog_Getint(_Win, idcbs_tijdtussen),
	_IDCBS_AANTPERDAG_VALUE = dialog_Getint(_Win, idcbs_aantperdag),
	_IDCBS_AANTPERONDPERDAG_VALUE = dialog_Getint(_Win, idcbs_aantperondperdag),
	_IDCBS_WEDSDUUR = dialog_Getint(_Win, idcbs_wedsduur),
	_IDCBS_SINGLESIN = dialog_Getint(_Win, idcbs_singlesin),
	%_BANENTOOL = dialog_GetCheck(_Win, idcbs_banentool),
	%_IDCBS_MAXBANEN = dialog_Getint(_Win, idcbs_maxbanen),
	%janee2bool(BanenToolJN,_BANENTOOL),
	%BanentoolJN = ja,
	dialog_GetListButton(_Win, idbcs_pouletelling,_IDBCS_POULETELLINGL,_IDBCS_POULETELLING),
	dialog_GetListButton(_Win, idcbs_week,_IDCBS_WEEK_ITEMLIST,_IDCBS_WEEK),
	not(beleidNietVeranderd(_IDCBS_BETAALADMINISTRATIE_CHECKED,_IDCBS_DOORPLANNEN_CHECKED,_IDCBS_BAANTOEWIJZING_CHECKED,
	_IDCBS_WAARSCHUWADMINISTRATIE_CHECKED,_IDCBS_AANTPERDAG_VALUE,_IDCBS_AANTPERONDPERDAG_VALUE,_IDCBS_WEDSDUUR,
	_IDCBS_SINGLESIN,_IDCBS_TIJDTUSSEN_VALUE,_IDBCS_POULETELLING,_IDCBS_WEEK, ja)),
	ParentWin = win_GetParent(_Win),
	not(checkPaswoord(ParentWin)), !.
  dlg_toernooibeleid_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	TussenWin = win_GetCtlHandle(_Win, idcbs_tijdtussen),
	TussenS = win_GetText(TussenWin),
	str_int(TussenS, Tussen),
	DuurWin = win_GetCtlHandle(_Win, idcbs_wedsduur),
	DuurS = win_GetText(DuurWin),
	str_int(DuurS, Duur),
	Duur > Tussen,
	%win_SetText(TussenWin, DuurS),
	Msg = "Minimum tijd tussen twee wedstrijden van dezelfde speler\n(in kwartieren) mag niet kleiner zijn dan de ingestelde wedstrijdduur,\ndus maak de een groter of de ander kleiner.",
	dlg_MessageBox( "Tijd tussen twee wedstrijden.", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	win_SetFocus(TussenWin),
	!.
%END Toernooibeleid, idc_ok _CtlInfo

%BEGIN Toernooibeleid, idc_help _CtlInfo
  dlg_toernooibeleid_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Toernooibeleid"),
	!.
%END Toernooibeleid, idc_help _CtlInfo

  dlg_toernooibeleid_eh(_,_,_):-!,fail.

%END_DLG Toernooibeleid


%BEGIN_DLG Toernooistart
/**************************************************************************
	Creation and event handling for dialog: Toernooistart
**************************************************************************/

constants

%BEGIN Toernooistart, CreateParms, 12:25:06-9.11.2016, Code automatically updated!
  dlg_toernooistart_ResID = idd_toernooistart
  dlg_toernooistart_DlgType = wd_Modal
  dlg_toernooistart_Help = idh_grote_schoonmaak_nieuw_toernooi
%END Toernooistart, CreateParms
  date_cc_class = "date_cc_class"
  s_setfocus = 137

facts - start
single current(integer)
%single pw(string)
single startdag(integer)
 %currentWC(wedscat, string Spst, wedsex HDG, wedstyp EDG, integer LV, integer TM)
echteWc(wedscat)
basicCat(wedscat)
onderdeelnr(string)

predicates

  dlg_toernooistart_eh : EHANDLER
  dlg_toernooistart_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_toernooistart_update(DIALOG_VAL_LIST)
  procedure begin(WINDOW Progress, BOOLEAN DeleteSp)
  date_cc_class_handlerP : EHANDLER
  startMelding(Window, integer, string Titel, string Melding)
  procedure knltbrel(window, boolean)   % true is bondstoernooi
  procedure alignDagVanDeWeek(integer, integer, integer)
  procedure alignDagVanDeWeek(integer, integer)
  procedure verschuifdata(integer)
  nondeterm verschuifF(integer Offset, ilist ToernooidagenGeenVerh, integer ToernooidagGeenVerhVerschoven)
  procedure gefingeerdespelerswissen(boolean)
  opslaanOfTerug(string Toernooinaam, window, boolean) - (i,i,i)
  determ certificaatInstall(window, string Tid) 
  procedure addnr(slist, string, slist)
  determ loadline(dbasedom)  % toernooidefinitie
  determ leeftijdOndergrensMatch(integer, integer)
  determ leeftijdBovengrensMatch(integer, integer)
  %determ geencriteria(string,leeftijd,leeftijd,string Schemasoort)
  determ getNieuweStartDatum(window, integer)
  procedure cleanOnderdeelDefinities(slist TidsOverslaanList)
  %procedure maakOndercat(wedscat, string Schemasoort, string Spst, wedsex HDG, wedstyp EDG, integer LV, integer LT, string NaamProxy, string NaamEigen, string Resultnaam)
  procedure websiteIs(string, string)
  nondeterm basicCats(wedscat, string Tid)
  determ fouteStartdag(dbasedom, janee Silent,boolean Checkdate)
  procedure maakOnderCats(string Tid)
  procedure selectNaam(string SchSrt, string In, string In1, String Uit)
  %procedure edgHDGconvert(wedstyp, wedsex, wedstyp, wedsex)
  %edgmatch(wedstyp, wedstyp)
  determ dagP(string)
  procedure dag2Werkdag(integer DayOfWeek, wrk Werkdag)
  procedure resetvereniging1(string Tnr)
  determ opslaan(string, string, string)
  procedure openOfClub(string, string)
  procedure checkkwalificatie(string Tnr, janee Log)
procedure uniekNaam1(string Naam, string, string Kort, string) - (i,i,i,i)
%procedure cleanrelaties()	% 25.7.2016
  isbondstoernooi(janee)
  uniekKort(string, string) - (i,o)

clauses

wisonderdeelnr() :-
  retractall(onderdeelnr(_)).

current(0).
startdag(0).

edgHDGconvert(g, _, d, gemengd) :- !.
edgHDGconvert(EDG, HDG, EDG, HDG). 

begin(PWin, b_True) :-
  progress_bar_set_value(PWin, 0, 0),
  vpi_ProcessEvents(b_True ),
  sound(20,145),
  %retractall(sp2(_,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)),
  sp2(_,_Srtm,Naam,_,_,_,_,_,_, _, _, _, Bondsnummer, _, _, _, _, _, _, _,_,_,_,_,_,_,_),
  not(proef11det(Bondsnummer)),
  retract(sp2(_,_,Naam,_,_,_,_,_,_, _, _, _, Bondsnummer, _, _, _, _, _, _, _,_,_,_,_,_,_,_)),
  fail.
begin(PWin, _) :-
  retractall(inschrijfcheck(_,_,_, _)),
  current(Cur),
  Cur1 = Cur + 10,
  assert(current(Cur1)),
  progress_bar_set_value(PWin, 100, Cur),
  vpi_ProcessEvents(b_True ),
  sound(20,165),
  wc(V, _, _, _Naam, _, _, _, _, _, _, _, _, _, _,_),
  isOnderCat(V),
  retract(wc(V, _, _, _, _, _, _, _, _, _, _, _, _, _,_)),
  fail.
begin(_Pwin, _) :-		% clean de onderdelen
  findall(WcX, wc(WcX, _, _, _, _, _, _, _, _, _, _, _, _, _,_),WcxList),
  member(WC, WCxList),
  getbacktrack(Here),
  retract(wc(WC, _OnderdeeelID, Nr, Naam, EDG, _Kort, SexL, SpStrk, LftVan, LftTM, HDG, Schemasoort, _Afkomst, _Tnr,Geld)),
  edgHDGconvert(EDG, HDG, EDG1, HDG1),
  cutbacktrack(Here),
  asserta(wc(WC, "", Nr, Naam, EDG1, _Kort, SexL, SpStrk, LftVan, LftTM, HDG1, Schemasoort, vanzelf, "",Geld)),
  fail.
begin(PWin, _) :-
  %retractall(wc(_, _, _, _, _, _, _, _, _, _, _, _, _, _,_)),
  retractall(toernooiK(_,_)),
  naam(Naam, Nr, Datum, _Ticket, Res1, Res2),
  assert(naam(Naam, Nr, Datum, "", Res1, Res2)),	% reset ticket
  assert(tshirt(b_False,"Persoon meldt zich voor de eerste keer!")),
  banentool(_, MaxB),
  assert(banentool(ja, MaxB)),
  retractall(baannaam(_, _, _, _, _)),
  retractall(baantoew(_, _, _)),
  retractall(spelerAanwezig(_, _, _, _)),
  retractall(opg(_,_,_,_,_,_,_,_,_,_,_)),
  retractall(functionaris(_,_,_,_,_,_,_)),
  current(Cur),
  Cur1 = Cur + 10,
  Cur2 = Cur + 20,
  Cur3 = Cur + 30,
  Cur4 = Cur + 40,
  assert(current(Cur4)),
  progress_bar_set_value(PWin, 100, Cur),
  vpi_ProcessEvents(b_True ),
  sound(20,175),
  retractall(pos(_,_,_,_)),
  progress_bar_set_value(PWin, 100, Cur1),
  vpi_ProcessEvents(b_True ),
  sound(20,185),
  retractall(wd(_,_,_,_,_,_,_)),
  progress_bar_set_value(PWin, 100, Cur2),
  vpi_ProcessEvents(b_True ),
  sound(20,196),
  retractall(w2(_,_,_,_,_,_,_,_)),
  progress_bar_set_value(PWin, 100, Cur3),
  vpi_ProcessEvents(b_True ),
  sound(20,208),
  retractall(w3(_, _, _, _, _, _, _)),
  retractall(bl(_, _)),
  retractall(bllft(_, _, _)),
  sound(20,220),
  retractall(bn(_,0)),
  retractall(kopl(_,_,_,_)),
  retractall(ws(_,_,_,_)),
  retractall(klad(_,_,_)),
  progress_bar_set_value(PWin, 100, Cur4),
  vpi_ProcessEvents(b_True ),
  sound(20,240),
  findall(Spxx,  sp2(Spxx,_,_,_,_,_,_,_,_, _, _, _, _, _, _, _, _, _, _,_,_,_,_,_,_,_,_), List),
  count(List, 0, Aantal),
  CurStart = (Aantal * 2) div 3,
  Aantal1 = (Aantal * 5) div 3,
  assert(current(CurStart)),
  member(No, List),
  current(Cur101),
  Cur102 = Cur101 + 1,
  assert(current(Cur102)),
  progress_bar_set_value(PWin, Aantal1, Cur102),
  vpi_ProcessEvents(b_True ),
  getbacktrack(Here),
  retract(sp2(No,Soort,N1,Telthuis,_Verh,_RatingE,_RatingD,_Betaald,_UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, _Opm,_Gezien,_RangPosE,_RangPosD,Incasso,CheckGeg,_IncassoAkkoord,_Res)),
  splitgeb(Gebdatum, Gebdatum1, _, _),
  assertz(sp2(No,Soort,N1,Telthuis,[],"","",nee,0, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum1, Telwerk, Telmobiel, EMail, "",nee,"","",Incasso,CheckGeg,"",[])),
  cutbacktrack(Here),
  fail.
begin(_, _) :-
  assert(schemapresel([])),
  retractAll(weblayout(_, _, _, _, _, _, _)),  
  fail.
begin(_, _) :-
  getFolder(TMap,_,_,_),
  format(Wild, "%s*.xml*", TMap),
  dirfiles(Wild,0x40,FNam,_,_,_,_,_,_,_,_),
  filenamepath(FullName,TMap,FNam),
  trap(existfile(FullName),_, true),
  deletefile(FullName), 
  fail.
begin(_, _) :-
  findall(PCx, autoweb(PCx, 1,_,_, _, _, _,_,_,_,_), Lijst),
  member(PC, Lijst),
  retract(autoweb(PC, 1,_HTMLFTP,Waarsch, IDC_GEBRUIKER_VALUE, Bin, IDC_FTP_adres_VALUE,Nietgebruikt,_Vasteverb,Hoevaak,BijSluiten)),
  assert(autoweb(PC, 0,1,Waarsch, IDC_GEBRUIKER_VALUE, Bin, IDC_FTP_adres_VALUE,Nietgebruikt,_Vasteverb,Hoevaak,BijSluiten)),	% 25.9.2012 alleen html!
  fail.
begin(_, _) :-
  getFolder(_Dir, Toernooien, _, _),
  filenamepath(FullName, Toernooien, jourconst),
  filenameext(jourconst, Name, Ext),
  date(Jaar, Maand, Dag),
  format(Name1, "%s-tot-%d-%d-%d",Name, Dag, Maand, Jaar),
  filenameext(Name2,Name1,Ext),
  filenamepath(NewOSFileName, Toernooien, Name2),
  trap(renamefile(FullName,NewOSFileName),_, fail),
  writetojournaal("begin"),
  fail.
begin(_, _).

verschuifdata(0) :- !.
verschuifdata(Offset) :-
  findall(DagO, dag(DagO, _, W), Dagen),
  member(Dag, Dagen),
  retract(dag(Dag, _, W)),
  NDag = Dag + Offset,
  dt_dateoffset_to_str(NDag,"%Dn %Dd/%Md",Str),
  assertz(dag(NDag, Str, W)),
  fail. 
verschuifdata(Offset) :-
  findall(Tijdx, bn(Tijdx, _), Tijden),
  member(Tijd, Tijden),
  tijd_kwart(Dag, Uur, Min, Tijd),
  NDag = Dag + Offset,
  tijd_kwart(NDag, Uur, Min, NTijd),
  retract(bn(Tijd, Aantal)),
  assertz(bn(NTijd, Aantal)),
  fail.  
verschuifdata(Offset) :-	% verschuif het formulier mee!
  form(Catl, Uiterste, Retour, Mededeling, Maxond, Inschrgeld, ToernooidagenGeenVerh, 
    Maxverh, Uitkant, UitKantMax, VerhWijze, Voorweekend, SpelerGeg, PartnerGeg),
  UitersteN = Uiterste + Offset,
  findall(ToernooidagGeenVerhVerschoven, 
    verschuifF(Offset,ToernooidagenGeenVerh,ToernooidagGeenVerhVerschoven), ToernooidagenGeenVerhVerschoven),
  assert(form(Catl, UitersteN, Retour, Mededeling, Maxond, Inschrgeld, ToernooidagenGeenVerhVerschoven, 
    Maxverh, Uitkant, UitKantMax, VerhWijze, Voorweekend, SpelerGeg, PartnerGeg)),
  fail.
verschuifdata(_).
	
verschuifF(Offset, ToernooidagenGeenVerh,ToernooidagGeenVerhVerschoven) :-
  member(Dag, ToernooidagenGeenVerh),
  ToernooidagGeenVerhVerschoven = Dag + Offset.

date_cc_class_handlerP(Win,e_Create(_),0) :-		% CALL date_cc_Init ON
  dag(Dag, _, _), !,
  dt_mins_date_time(InitValue, Dag, 0),
  date_cc_Init(Win, InitValue, "%DD-%MD-%YL"),
  Font = font_Create(ff_Helvetica, [], 8),
  win_SetFont(Win, Font).
date_cc_class_handlerP(Win, EVENT, 0):-!,		% CALL date_cc_HandleEvent
  date_cc_handleEvent(Win, Event).			% ON ANY OTHER EVENT
date_cc_class_handlerP(Win, EVENT, 0):-!,		% CALL date_cc_HandleEvent
  dlg_toernooistart_eh(Win, Event).			% ON ANY OTHER EVENT

startMelding(Window, Control, Titel, Melding) :-
  dlg_MessageBox( Titel, Melding, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  win_PostEvent(Window, e_User(s_setfocus, Control)), !.

knltbRel(Win, TrueFalse) :-	% true = bondstoernooi
  dialog_SetState(Win, [
    enable(idcst_knltb_gerelateerd,TrueFalse), enable(idctst_nieuw_toernooinummer,TrueFalse),
    enable(idcst_05,TrueFalse), show(idcst_05,TrueFalse), 
    enable(idcst_06,TrueFalse), show(idcst_06,TrueFalse),
    show(idctst_eventueel,TrueFalse), 
    show(idctst_nieuw_toernooinummer,TrueFalse),
    enable(idctst_certificaat,TrueFalse),enable(idcst_selecteer,TrueFalse),
    enable(idcst_certificaatfile,TrueFalse),enable(idctst_wachtwoord,TrueFalse),
    enable(idcst_selecteer,TrueFalse),
    show(idcst_knltb_gerelateerd,TrueFalse), 
    show(idctst_certificaat,TrueFalse),show(idcst_selecteer,TrueFalse),
    show(idcst_certificaatfile,TrueFalse),show(idctst_wachtwoord,TrueFalse),
    show(idcst_wachtwoord,TrueFalse),show(idcst_toernooistart_5,TrueFalse),
    show(idcst_toernooistart_25,TrueFalse)
  ]),
  bitxor(TrueFalse,1,FalseTrue),
  dialog_SetState(Win, [
    show(idcst_toernooistart_22,FalseTrue),enable(idcst_toernooistart_22,FalseTrue),
    show(idcst_kalender_en_baan,FalseTrue),enable(idcst_kalender_en_baan,FalseTrue),
    show(idctst_toernooistartdatum_oud,FalseTrue),enable(idctst_toernooistartdatum_oud,FalseTrue),
    show(idctst_nieuwestart,FalseTrue),enable(idctst_nieuwestart,FalseTrue),
    show(idctst_datum,FalseTrue),enable(idctst_datum,FalseTrue),
    show(idctst_naar,FalseTrue),enable(idctst_naar,FalseTrue),
    show(idcst_datecontrol,FalseTrue),enable(idcst_datecontrol,FalseTrue),
    show(idctst_pijltjestoetsen,FalseTrue),enable(idctst_pijltjestoetsen,FalseTrue)
  ]), !.
 
knltbRel(_Win, _).

alignDagVanDeWeek(DagOud, GekozenDag, DagNieuw) :-
  dt_offset_to_date(GekozenDag,_Y2,_M2,_W2,_Dm2,Dw2),
  dt_offset_to_date(DagOud,_Y1,_M1,_W1,_Dm1,Dw1),
  Diff = Dw2 - Dw1,
  alignDagVanDeWeek(Diff, Diff1),
  DagNieuw = GekozenDag - Diff1, !.

alignDagVanDeWeek(Diff, Diff1) :-
  Diff < -3, !,
  Diff1 = Diff + 7.
alignDagVanDeWeek(Diff, Diff1) :-
  Diff > 3, !,
  Diff1 = Diff - 7.
alignDagVanDeWeek(Diff, Diff).

toernooidatum(Datum) :-
  dag(Dag, _, _),
  dt_dateoffset_to_str(Dag, "%Dd-%Md-%YL", Datum), !.

gefingeerdespelerswissen(b_True) :-
  sp2(_,_Srtm,"Zitzak,T",_,_,_,_,_,_, _, _, _, _Bondsnummer, _, _, _, _, _, _, _,_,_,_,_,_,_,_),
  1 = dlg_MessageBox( "Kopie van Demo", "Gefingeerde spelers wissen?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
  !.
gefingeerdespelerswissen(b_False).


certificaatAanwezig(_Win, Tid, "", 0) :-		% wordt voorlopig niet gebruikt!
  not(fronttoken(Tid,_,_)), !.
certificaatAanwezig(_Win, Tid, EinddatumS, NogXdagen) :-
  providerX(Prov,_,"",_),
  format(URL, "http://%s/knltbproxy2015/IsJuisteCertificaatEr.php", Prov),
  get_TempDir(_, TempFile),
  deletefile(TempFile),
  AanmeldenVars = aanmeldenvars(),
  _Return = downloadData(URL, TempFile, ["tid", Tid | AanmeldenVars], _HttpResponsecode, 0),
  file_str(TempFile, Str),
  dt_datestr_to_offset(Str, "%YL-%MD-%DD", Datum),
  dt_dateoffset_to_str(Datum, "%DD-%MD-%YL",EinddatumS),
  date(Year,Month,Day),
  dt_date_to_offset(Year, Month, Day, DatumX),
  NogXdagen = Datum - DatumX.

certificaatInstall(_Win,Tid ) :-
  not(fronttoken(Tid,_,_)), !.
certificaatInstall(Win,Tid ) :-
  b_True = dialog_GetCheck(Win, idcst_toernooi_bondsnummers),
  providerX(Prov,_,"",_),
  format(URL,"http://%s/knltbproxy2015/CertificaatImport.php",Prov),
  Password = dialog_GetStr(Win, idcst_wachtwoord),
  trim_c(Password),
  CW = win_GetCtlHandle(Win, idcst_certificaatfile),
  BestandC = win_getText(CW),
  Vars = aanmeldenVars(),
  Report = "certificaatupload.txt",
  ReturnK = fileupload(URL, BestandC, Report, ["ww",Password, "tid", Tid | Vars]),
  write("\n",ReturnK),
  file_str(Report, Str),
  write("\n",Str),
  searchstring(Str, "Geluk", _), !.
certificaatInstall(_Win, Tid) :-
  format(Msg, "Certificaat installeren voor toernooi nummer %s mislukt!\nHeb je een vers certificaat en wachtwoord van mijnKNLTB opgehaald?\nNB Certificaten kunnen verlopen!", Tid),
  _A = dlg_MessageBox( "Attentie", Msg, mesbox_iconExclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.
/*
preconditionWC(Tnr) :-
  retract(wc(Wc, OnderdeeelID, N, Naam, EDG, Kort, SexL, SpStrk, LftV, LftTM, HDG, Schemasoort, _, Tnr,Geld)),
  assertz(wc(Wc, OnderdeeelID, N, Naam, EDG, Kort, SexL, SpStrk, LftV, LftTM, HDG, Schemasoort, vanzelf, "", Geld)),
  fail.
preconditionWC(_Tnr).
*/
gebruiktOnderdeel(WC) :-
  kopl(WC,_,_,_), !.
gebruiktOnderdeel(WC) :-
  kopl(_,_,WC,_), !.
gebruiktOnderdeel(WC) :-
  opg(_,WC,_,_,_,_,_,_,_,_,_), !.

fouteStartDag(dag(0,_,_), nee, b_True) :-  % not silent and docheck date
  startdag(SD),
  findall(Dag, dag(Dag,_,_), DList),
  DList = [Dag1|_],
  SD < Dag1 - 7,
  SD > Dag1 + 7, 
  dt_dateoffset_to_str(SD,"%Dn %Dd/%Md",StartDagStr),
  dt_dateoffset_to_str(Dag1,"%Dn %Dd/%Md",EersteDagStr),
  format(Msg,"Eerste dag volgens KNLTB\n%s\nen eerste dag van de kalender\n%s\nliggen te(?) ver uit elkaar!\nDoorgaan?",StartDagStr,EerstedagStr),
  2 = dlg_MessageBox( "Attentie", Msg, mesbox_iconExclamation, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
  closefile(work),
  logclose(), !.

openOfClub("open", "open of club") :- !.
openOfClub(X, X).

getKNLTBdefinities(TNr, _, _,b_True) :-
  initspsrt(),
  not(fronttoken(Tnr,_,_)), !.
getKNLTBdefinities(TNr,_, _,_) :-
  stopdbrefresh(),  % overbodig
  backup("email", ja),
  retractall(onderdeelnr(_)),
  retractall(basicCat(_)),
  basicCats(Cat,Tnr),
  assert(basicCat(Cat)),
  fail.
getKNLTBdefinities(TNr,Silent,CheckDate,b_False) :-
  retractall(echteWc(_)),
  writedevice(X),
  writedevice(screen),
  providerx(Provider, _, "", _),
  format(URL, "http://%s/knltbproxy2015/DefinitieExportServiceN1.php", Provider),
  get_TempDir(_, TempFile),
  deletefile(TempFile),
  AanmeldenVars = aanmeldenvars(),
  _Return = downloadData(URL, TempFile, ["tid",TNr | AanmeldenVars], _HttpResponsecode, 0),
  writedevice(X),
  openread(work, TempFile),
  readdevice(work),
  repeat_f(work),
  readln(Line),
  utf_8_to_ansi(Line, Line1),
  writedevice(XY),
  writedevice(screen),
  writedevice(XY),
  trap(term_str(dbasedom, Term, Line1),Error,misser(Line, Error)),
  loadline(Term),
  fouteStartdag(Term, Silent,CheckDate), !.
getKNLTBdefinities(_Tnr,_,_,b_False) :-
  not(onderdeelnr("vereniging1")),
  closefile(work), !.	%check of valid download
getKNLTBdefinities(Tnr,_,_,_) :-
  onderdeelnr(Tnr),
  wc(WC, Oid, _, _, _, _, _, _, _, _, _, _, _, Tnr, _), % moet met Tnr
  not(onderdeelNr(Oid)),
  retract(wc(WC, Oid, Nx, Naam, EDG, Kortx, SexLx, SpStrk, LftV, LftTm, HDG, Schemasoort, _Keuring, Tnr, Geldx)), % moet met Tnr
  assert(wc(WC, "", Nx, Naam, EDG, Kortx, SexLx, SpStrk, LftV, LftTm, HDG, Schemasoort, vanzelf, "", Geldx)),
  fail.
getKNLTBdefinities(Tnr,_,_,_) :-
  %save("Vel2.txt"),
  maakOnderCats(Tnr),  % !!!!
  logclose(),
  closefile(work),
  fail.
getKNLTBdefinities(Tnr,_,_,_) :-
  logclose(),
  closefile(work),
  writedevice(screen),
  wc(Wc, _, _, _, _, _, _, _, _, _, _, _, _, Tnr,_),
  not(echteWc(Wc)),
  retract(wc(WC, Oid, Nx, Naam, EDG, Kortx, SexLx, SpStrk, LftV, LftTm, HDG, Schemasoort, _Keuring, Tnr, Geldx)), % moet met Tnr
  %gebruiktOnderdeel(WC),
  assert(wc(WC, "", Nx, Naam, EDG, Kortx, SexLx, SpStrk, LftV, LftTm, HDG, Schemasoort, vanzelf, "", Geldx)),
  format(Mess,"Onderdeel heeft tussentijds een andere onderdeelid gekregen: %u %s.",WC, Oid),
  dlg_note("Attentie",Mess),
  fail.
getKNLTBdefinities(_Tnr,_,_,b_false) :-
  retractall(echteWc(_)),
  retract(toernooiK("",_Soort1)), !,
  Msg = "ServIt erkent het certificaat niet (verlopen?) of er is een andere storing.\nProbeer opnieuw en/of ververs het certificaat vanaf mijnKNLTB.",
  _Answer = dlg_MessageBox( "Toernooisoort", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  writetojournaal(Msg), !.
getKNLTBdefinities(Tnr,nee,_,b_true) :-
  %retractall(echteWc(_)),
  toernooiK(Tnr,Soort1), !,
  term_str(toernooisoort, Soort1, Soort2),
  openOfClub(Soort2, Soort),
  Opgehaald = "De definities zijn met succes opgehaald van ServIt!",
  format(Msg, "%s\nDe TA denkt dat bij nummer %s een %s toernooi hoort.\n100%c zeker is dit niet.\nWilt u dat nakijken?\n\nNB Neem svp contact met ons op als u twijfelt (info@toernooiassistent.nl).", Opgehaald, Tnr, Soort, '%'),
  _Answer = dlg_MessageBox( "Toernooisoort", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  writetojournaal(Msg), !.
getKNLTBdefinities(Tnr,_Silent,_, b_False) :-
  not(toernooiK(Tnr,_)),
  format(Msg, "Toernooi %s niet gevonden in ServIt!\nHebt u het juiste toernooinummer en/of het juiste certificaat gebruikt?",Tnr),
  _Answer = dlg_MessageBox( "Een toernooi koppelen", Msg, mesbox_iconquestion, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  !.
getKNLTBdefinities(Tnr,_,_,b_True) :-
  toernooiK(Tnr,nrt), 
  checkkwalificatie(Tnr,nee), !.
getKNLTBdefinities(_Tnr,_,_,b_True).

/*
showWc(Wc, Wc, Nr) :-
  wc(WC, Oid, Nx, Naam, EDG, Kortx, SexLx, SpStrk, LftV, LftTm, HDG, Schemasoort, Keuring, Tnr, Geldx),
  Term = wc(WC, Oid, Nx, Naam, EDG, Kortx, SexLx, SpStrk, LftV, LftTm, HDG, Schemasoort, Keuring, Tnr, Geldx),
  term_str(dbasedom, Term, Line),
  write("\n ",Nr," -- ",Line), !.
showWc(_, _,_).

showterm(Term, Wc) :-
  %wc(WC, Oid, Nx, Naam, EDG, Kortx, SexLx, SpStrk, LftV, LftTm, HDG, Schemasoort, Keuring, Tnr, Geldx),
  Term = wc(WC, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
  term_str(dbasedom, Term, Line),
  write("\n -- ",Line), !.
showterm(_, _).
*/
checkkwalificatie(Tnr,_) :-
  not(toernooiK(Tnr,nrt)), !.
checkkwalificatie(Tnr,nee) :-
  wc(WcX, _OnderdeeelID, _N, Naam, e, Kort, SexL, "", leeftijd("",0), leeftijd("",0), HDG, "", vanknltb, Tnr,Geld),
  not(kopl(_,_,WcX,_)),
  concat("Kwal. ",Naam, Naam0),
  concat(Kort, "Kw", Kort0),
  uniekNaam(Naam0, Kort0),
  %uniekKort(Kort0, Kort1),
  getbacktrack(Here),
  numb(WcXN),
  WcX1 = WcXN + 1,
  not(wc(WcX1, _, _, _, _, _, _, _, _, _, _, _, _, _,_)),
  not(kopl(WcX1,_,_,_)),
  not(kopl(_,_,WcX1,_)), 
  cutbacktrack(Here),
  assertz(wc(WcX1, "", 99, Naam0, e, Kort0, SexL, "", leeftijd("",0), leeftijd("",0), HDG, "", vanzelf, Tnr,Geld)),
  assertz(kopl(WcX1,algemeen,WcX,algemeen)), 
  fail.
checkkwalificatie(Tnr,ja) :-
  wc(WcX, _OnderdeeelID, _N, Naam, e, Kort, SexL, "", leeftijd("",0), leeftijd("",0), HDG, "", vanknltb, Tnr,Geld),
  not(kopl(_,_,WcX,_)),
  concat("Kwal. ",Naam, Naam0),
  concat(Kort, "Kw", Kort0),
  uniekNaam(Naam0, Kort0),
  %uniekKort(Kort0, Kort1),
  getbacktrack(Here),
  numb(WcXN),
  WcX1 = WcXN + 1,
  not(wc(WcX1, _, _, _, _, _, _, _, _, _, _, _, _, _,_)), 
  not(kopl(WcX1,_,_,_)),
  not(kopl(_,_,WcX1,_)), 
  cutbacktrack(Here),
  logf('Z',wc(WcX1, "", 99, Naam0, e, Kort0, SexL, "", leeftijd("",0), leeftijd("",0), HDG, "", vanzelf, Tnr,Geld),ja),
  logf('Z',kopl(WcX1,algemeen,WcX,algemeen),nee), 
  fail.
checkkwalificatie(_Tnr, _).  

leeftijdOndergrensMatch(X, X) :- !.  
leeftijdOndergrensMatch(Xin, Xaanw) :-
  Xin < 5,
  Xaanw < 5, !.
  
leeftijdBovengrensMatch(X, X) :- !.
leeftijdBovengrensMatch(0,X) :-
 X > 85, !.
leeftijdBovengrensMatch(X,0) :-
 X > 85, !.
leeftijdBovengrensMatch(X, Y) :-
 X > 85,
 Y > 85, !.   

%geencriteria("",leeftijd("",0),leeftijd("",0),"").   


maakOnderCats(ToernooiId) :- 
  wc(WCM, _, _, _, EDG1, _, SexL, SpStrk,LftV, leeftijd(_,LftTM), HDG, "0", _, ToernooiId, _Geld),
  wc(WC, _OnderdeelId, _Nr, _Naam, EDG2, _KNaam, SexL, SpStrk,LftV, leeftijd(_LStr,LftTM1), HDG, SchS, _Origine, ToernooiId, _),
  leeftijdBovengrensmatch(LftTM,LftTM1),
  edgmatch(EDG1, EDG2),
  SchS >= "A",
  SchS <= "Z",
  not(kopl(WC,_,_,_)),
  retractall(kopl(WCM,_,WC, _)),
  %not(kopl(WCM,_,WC,_)),
  assertz(kopl(WC,algemeen,WCM,algemeen)),
  fail.
maakOnderCats(_).

selectNaam(Schsrt, In, _, In) :-
  fronttoken(SchSrt,_,_), !.
selectNaam(_, _, In, In).

%edgmatch(d,g) :- !.
%edgmatch(g,d) :- !.
%edgmatch(X,X).



loadline(wc(_, OnderdeelID, _N, Naam, EDGin, _Kort, _SexL, SpStrk, leeftijd(_SV,IV), leeftijd(_STM,ITM), HDG, Schemasoort, Origine, Tnr,_Geld)) :-
  retractall(onderdeelNr(OnderdeelID)),
  assert(onderdeelNr(OnderdeelId)),
  wc(Wedscat, _OnderdeeelIDx, Nx, Naamx, EDGzelf, Kortx, SexLx, SpStrk, leeftijd(_SVx,IVx), leeftijd(_STMx,ITMx), HDG, Schemasoort, _Keuringx, "", Geldx),
  basicCat(Wedscat),		% toponderdeel niet gekoppeld aan Tid met zelfde karakteristieken
  edgmatch(EDGin,EDGzelf), 
  leeftijdBovengrensMatch(ITM, ITMx),
  leeftijdOndergrensMatch(IV, IVx),
  retractall(wc(Wedscat, _, _, _, _, _, _, _, _, _, _, _, _, _,_)),
  retract(basicCat(Wedscat)),
  assert(echteWC(Wedscat)),
  selectNaam(Schemasoort, Naam, Naamx, NaamResult),
  uniekNaam(NaamResult, Kortx),
  %uniekkort(Kortx,KortxU),
  assertz(wc(Wedscat, OnderdeelID, Nx, NaamResult, EDGin, Kortx, SexLx, SpStrk, leeftijd(_SV,IV), leeftijd(_STM,ITM), HDG, Schemasoort, Origine, Tnr,Geldx)), !.
loadline(wc(_, OnderdeelID, _N, _Naam, EDGzelf, _Kort, _SexL, SpStrk, leeftijd(_SV,IV), leeftijd(_STM,ITM), HDG, Sch, Origine, Tnr,_Geld)) :-
  fronttoken(OnderdeelId, _, _),  % voor de zekerheid...
  wc(Wedscat, OnderdeelID, Nx, Naamx, EDGin, Kortx, SexLx, SpStrk, leeftijd(_SVx,IVx), leeftijd(_STMx,ITMx), HDG, _Schemasoort, _Keuringx, _, Geldx),
  basicCat(Wedscat),
  edgmatch(EDGin, EDGzelf),	% wel gekoppeld aan Tid met dezelfde karakteristieken
  leeftijdBovengrensMatch(ITM, ITMx),
  leeftijdOndergrensMatch(IV, IVx),
  retractall(wc(Wedscat, _, _, _, _, _, _, _, _, _, _, _, _, _,_)),
  retract(basicCat(Wedscat)),
  assert(echteWC(Wedscat)),
  uniekNaam(Naamx, Kortx),
  uniekkort(Kortx,KortxU),
  assertz(wc(Wedscat, OnderdeelID, Nx, Naamx, EDGin, KortxU, SexLx, SpStrk, leeftijd(_SV,IV), leeftijd(_STM,ITM), HDG, Sch, Origine, Tnr,Geldx)), !.
loadline(wc(_, OnderdeeelID, N, Naam, EDG, Kort, SexL, SpStrk, LftV, LftTM, HDG, Schemasoort, Origine, Tnr,Geld)) :-
  vertaalSexNrs(SexL, SexL1),	% voor het geval van vroeger
  uniekNaam(Naam, Kort),
  uniekKort(Kort, Kort1),
  %dlg_note("Hier", Naam),
  numb(WcX),
  WcX1 = WcX + 1,
  not(wc(WcX1, _, _, _, _, _, _, _, _, _, _, _, _, _,_)),
  not(kopl(WcX1,_,_,_)),	% 11.1.2015niet het begin of eindpunt van een bungelende wedscat
  not(kopl(_, _, WcX1, _)),
  %LftV = leeftijd(_, LV),
  %LFtTM = leeftijd(_, LT), 
  %maakonderCat(WcX1,Schemasoort,SpStrk, HDG, EDG, LV, LT, "","",_), !,
  assert(echteWc(WcX1)), !,
  assertz(wc(WcX1, OnderdeeelID, N, Naam, EDG, Kort1, SexL1, SpStrk, LftV, LftTM, HDG, Schemasoort, Origine, Tnr,Geld)).
loadline(vereniging1(NrIn, WLIn, WLTelefIn, ClubIn, PlaatsIn, EMailIn, BGNaamIn, BGTelIn, BGEmailIn, WebsiteIn)) :-
  assert(onderdeelnr("vereniging1")),
  vereniging1(NrAl,_,_,_,_,_,_,_,_,_),
  NrIn <> NrAl,
  websiteIs(WebsiteIn, WebsiteUit),
  assertz(vereniging1(NrIn, WLIn, WLTelefIn, ClubIn, PlaatsIn, EMailIn, BGNaamIn, BGTelIn, BGEmailIn, WebsiteUit)), !.
loadline(functionaris(FType,Tid, Naam,Email,Tel1,Tel2,Tel3)) :-
  retractall(functionaris(FType,Tid,_,_,_,_,_)),
  assertz(functionaris(FType,Tid, Naam,Email,Tel1,Tel2,Tel3)), !.
loadline(toernooiK(Tid,Type)) :-
  assert(onderdeelnr(Tid)),
  retractall(toernooiK(Tid,_)),
  assertz(toernooiK(Tid,Type)).
loadline(dag(0,Str,_)) :-
  frontstr(10,Str,Str1,_),
  dt_datestr_to_offset(Str1, "%YL-%MD-%DD", DateOffset),
  assert(startdag(DateOffset)), !.
loadline(dag(1,_Str,_)) :- !.   % doe daar nog niets mee
  %write("\neinddatum ",Str), !.

websiteIs(In, In) :-
  fronttoken(In, _, _), !.
websiteIs(_, "https://www.toernooiklapper.nl").
/*
uniekNaam(Naam, Uit) :-
  wc(_, _, _, Naam, _, _Kort, _, _, _, _, _, _, _, _,_), !,
  concat(Naam,"#",Naam1),
  uniekNaam(Naam1, Uit).
uniekNaam(Naam, Naam).
*/

uniekNaam(Naam, Kort) :-
  uniekNaam1(Naam, Naam, Kort, Kort), !.

uniekNaam1(OrigN, Naam,OrigK,Kort) :-
  wc(_WC, _, _, Naam, _, _, _, _, _, _, _, _, _, _,_), !,
  concat(Naam,"^",Naam1), !,
  uniekNaam1(OrigN, Naam1,OrigK,Kort).
uniekNaam1(OrigN, Naam,OrigK,Kort) :-
  wc(_WC, _, _, _Naam, _, Kort, _, _, _, _, _, _, _, _,_), !,
  concat(Kort,"^",Kort1), !,
  uniekNaam1(OrigN, Naam,OrigK,Kort1).
uniekNaam1(OrigN, OrigN, OrigK, OrigK) :- !. % is al uniek
uniekNaam1(OrigN, Naam,OrigK,Kort) :-
  wc(WC, OnderdeeelID, Nr, OrigN, EDG, OrigK, SexL, SpStrk, LftVan, LftTM, HDG, Schemasoort, Afkomst, Tnr,Geld),
  logf('Z',wc(WC, OnderdeeelID, Nr, Naam, EDG, Kort, SexL, SpStrk, LftVan, LftTM, HDG, Schemasoort, Afkomst, Tnr,Geld),nee), !.  
uniekNaam1(OrigN, Naam,_OrigK,Kort) :-
  wc(WC, OnderdeeelID, Nr, OrigN, EDG, _, SexL, SpStrk, LftVan, LftTM, HDG, Schemasoort, Afkomst, Tnr,Geld),
  logf('Z',wc(WC, OnderdeeelID, Nr, Naam, EDG, Kort, SexL, SpStrk, LftVan, LftTM, HDG, Schemasoort, Afkomst, Tnr,Geld),nee), !.  
uniekNaam1(_OrigN, Naam,OrigK,Kort) :-
  wc(WC, OnderdeeelID, Nr, _, EDG, OrigK, SexL, SpStrk, LftVan, LftTM, HDG, Schemasoort, Afkomst, Tnr,Geld),
  logf('Z',wc(WC, OnderdeeelID, Nr, Naam, EDG, Kort, SexL, SpStrk, LftVan, LftTM, HDG, Schemasoort, Afkomst, Tnr,Geld),nee), !.  
uniekNaam1(_Orig, _Naam, _, _).

  
uniekKort(Kort, Uit) :-
  wc(_, _, _, _, _, Kort, _, _, _, _, _, _, _, _,_), !,
  concat(Kort,"*",Kort1),
  uniekKort(Kort1, Uit).
uniekKort(Kort, Kort).

vertaalSexNrs([1],[Uit]) :-
  spsrt(Uit, _, m, _, _, _, _, _), !.
vertaalSexNrs([2],[Uit]) :-
  spsrt(Uit, _, v, _, _, _, _, _), !.
vertaalSexNrs(_, [U1,U2]) :-
  spsrt(U1, _, m, _, _, _, _, _),
  spsrt(U2, _, v, _, _, _, _, _), !.
vertaalSexNrs(_, []).

opslaan(OpslaanAls,Dir, Uit) :-
  usb(ja), !,
  Uit = dlgFileNameZeker(OpslaanAls,["Toernooibestand","*.tas"],"",[dlgfn_Save],Dir,_OutListFiles).
opslaan(Filenaam, _, FileNaam) :-
  not(existfile(Filenaam)), !.
opslaan(Filenaam, _, FileNaam) :-
  format(Msg, "Bestand %s\nbestaat al, overschrijven?",Filenaam),
  1 = dlg_MessageBox( "Toernooibestand", Msg, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ).
    
opslaanOfTerug(TNaam, _Win, b_true) :-
  getfolder(_DocDir, Dir, _,_),
  %concat(DocDir, "\\Toernooien", Dir),
  cleantoernooinaam(TNaam, ToernooiFnaam),
  concat(ToernooiFNaam, ".tas", SaveNaam),
  filenamepath(OpslaanAls, Dir, SaveNaam),
  opslaan(Opslaanals,Dir, FILENAME),
  fronttoken(FILENAME, _, _),
  filenamepath(FILENAME, NPad, _),
  filenamepath(NFile, NPad, SaveNaam),
  retractall(alleenlezen1),
  assert(toernooidir(NPad, NFile)),
  webdir(WP, _, WB),
  assert(webdir(WP, disk, WB)),
  updatetoernooidirsave(),
  VPIconf = getvpiini(),
  save(VPIconf,vpiconf),
  save(NFile),
  del_log(NFile), 
  close_alles(),
  update_proj_toolbar(),
  startDBrefresh(),
  format(Msg1, "Toernooi bestand aangemaakt in\n  %s !\nGa verder met het stappenplan:\n  1. Inrichten, Beleid instellen!",NPad),
  dlg_MessageBox( "Aangemaakt", Msg1,  
	mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  win_Destroy(_Win),
  TW = vpi_GetTaskwin(),
  win_PostEvent(TW,e_Menu(id_Internet, 0)), !.
opslaanOfTerug(_, _Win, _) :-
  win_Destroy(_Win),
  TaskWin = vpi_GetTaskWin(),
  dlg_toernooiload_Create(TaskWin, "afgeblazen", b_False, update).


cleanOnderdeeldefinities([]) :- !.
cleanOnderdeeldefinities(Nrs) :-
  writedevice(X),
  writedevice(screen),
  write("\ncleanonderdeel + functionaris: ", Nrs),
  writedevice(X),
  wc(WC, _, _, _, _, _, _, _, _, _, _, _, _, Tid,_),
    not(member(Tid, Nrs)),
    retract(wc(WC, _, _, _, _, _, _, _, _, _, _, _, _, _,_)),
  fail.
cleanOnderdeeldefinities(Nrs) :-
  functionaris(Ftype, Tid, _, _, _, _, _),
    not(member(Tid, Nrs)),
    retract(functionaris(Ftype, Tid, _, _, _, _, _)),
  fail.
cleanOnderdeeldefinities(_).


addnr(Inlist, "", InList) :- !.
addnr(Inlist, In, InList) :-
  member(In, Inlist), !.
addnr(Inlist, In, [In|Inlist]).


getNieuweStartDatum(_Win,Offset) :-
  b_False = dialog_GetCheck(_Win, idcst_toernooi_bondsnummers),
  trap(DateCCWin = win_GetCtlHandle(_Win,idcst_datecontrol),_,fail),
  MinOffset = date_cc_GetValue(DateCCWin),
  dt_mins_date_time(MinOffset,Date2,_Time),
  dag(Dag1Oud, _, _),
  alignDagVanDeWeek(Dag1Oud, Date2, StartDatum),
  Offset = StartDatum - Dag1Oud,  !.
getNieuweStartDatum(_Win,Offset) :-
  b_True = dialog_GetCheck(_Win, idcst_toernooi_bondsnummers),
  startdag(Date2),  % gezet door de service
  dag(Dag1Oud, _, _),
  alignDagVanDeWeek(Dag1Oud, Date2, StartDatum),
  Offset = StartDatum - Dag1Oud, !.
getNieuweStartDatum(_Win,0) :-
  win_Destroy(_Win),
  TaskWin = vpi_GetTaskWin(),
  dlg_toernooiload_Create(TaskWin, "afgeblazen", b_False, update),
  fail.

dag2Werkdag(6, z) :- !.
dag2Werkdag(7, z) :- !.
dag2Werkdag(_, w).

resetvereniging1(Tnr) :-
  fronttoken(Tnr, _, _),
  assert(vereniging1("", "", "", "", "", "", "", "", "", "")), !.
resetvereniging1(_Tnr).

isBondstoernooi(ja) :-
  toernooiK(_,_), !.
isBondstoernooi(nee).

dagP(_DagNaam):-
  dag(D,_Naam,_ZW),
  D < 400000, !,
  %write(D),
  dlg_MessageBox( "Kalendertabel", "De kalendertabel is helaas corrupt, hier kunt u geen nieuw toernooi van maken! Zie Toernooi inrichten, Kalender aanpassen.\n\nNog beter: open eerst een gave back-up van dit toernooi!", mesbox_iconerror, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.
dagP(DagNaam):-
  dag(Dag1, _, _),
  dt_dateoffset_to_str(Dag1,"%Dn %DD-%MD-%YL",DagNaam), !.
dagP(DagNaam):-
  date(Year,Month,Day),
  dt_date_to_offset(Year, Month, Day, Doff),
  dt_dateoffset_to_str(Doff, "%Dn %Dd/%Md", Str),
  dt_offset_to_date(Doff, _, _, _, _, DayOfWeek),
  dag2werkdag(DayOfWeek, Werkdag),
  logf('Z',dag(Doff, Str, WerkDag), nee),
  dt_dateoffset_to_str(Doff,"%Dn %DD-%MD-%YL",DagNaam).
  
%___________________________________________________________________________________
  dlg_toernooistart_Create(Parent):-
	dagP(IDCTST_DATUM_TITLE),
	class_Create(date_cc_class,date_cc_class_handlerP),
	T0 = "Maak alles in één klap gereed voor een nieuw toernooi op basis van het geopende oude toernooi. Bij voorkeur hetzelfde toernooi van verleden jaar!\n",
	T1 = "\"Schoonmaken\" van de gegevens, dus bijvoorbeeld indeling en planning worden verwijderd",
	T2 = " en verhinderingen worden gewist. Suggestie: verwerk het jaartal in de nieuwe toernooinaam!",
	format(IDCTST_STATIC_TEXT_TITLE, "%s%s%s", T0, T1, T2), 
	naam(IDCST_NAAM_VALUE, _, _, _, _, _),
	isBondstoernooi(BT),
	%openbaar(Openb),
	janee2bool(BT, IDCST_TOERNOOI_BONDSNUMMERS_CHECKED),
	IDCST_WACHTWOORD_VALUE = "",
	IDCST_05_VALUE = "",
	IDCST_06_VALUE = "",
	IDCST_CERTIFICAATFILE_VALUE = "nog geen certificaat geselecteerd",
%MARK Toernooistart, new variables

	dialog_CreateModal(Parent,dlg_toernooistart_ResID,"",
  		[
%BEGIN Toernooistart, ControlList, 12:25:06-9.11.2016, Code automatically updated!
		df(idcst_naam,editstr(IDCST_NAAM_VALUE,[]),nopr),
		df(idcst_toernooi_bondsnummers,checkbox(IDCST_TOERNOOI_BONDSNUMMERS_CHECKED),nopr),
		df(idcst_05,editstr(IDCST_05_VALUE,[]),nopr),
		df(idcst_06,editstr(IDCST_06_VALUE,[]),nopr),
		df(idcst_certificaatfile,editstr(IDCST_CERTIFICAATFILE_VALUE,[]),nopr),
		df(idcst_wachtwoord,editstr(IDCST_WACHTWOORD_VALUE,[]),nopr),
		df(idctst_static_text,statictext(IDCTST_STATIC_TEXT_TITLE),nopr),
		df(idctst_datum,statictext(IDCTST_DATUM_TITLE),nopr)
%END Toernooistart, ControlList
		],
		dlg_toernooistart_eh,0,VALLIST,ANSWER),
	class_destroy(date_cc_class),
	dlg_toernooistart_handle_answer(ANSWER,VALLIST), !.

  dlg_toernooistart_handle_answer(idc_ok,VALLIST):-!,
	dlg_toernooistart_update(VALLIST).
  dlg_toernooistart_handle_answer(idc_cancel,_):-!.  % Handle Esc and Cancel here
  dlg_toernooistart_handle_answer(_,_):-
	errorexit().

  dlg_toernooistart_update(_VALLIST):-
%BEGIN Toernooistart, Update controls, 12:25:06-9.11.2016, Code automatically updated!
	_IDCST_05_VALUE = dialog_VLGetstr(idcst_05,_VALLIST),
	_IDCST_NAAM_VALUE = dialog_VLGetstr(idcst_naam,_VALLIST),
	_IDCST_TOERNOOI_BONDSNUMMERS_CHECKED = dialog_VLGetCheck(idcst_toernooi_bondsnummers,_VALLIST),
	_IDCST_WACHTWOORD_VALUE = dialog_VLGetstr(idcst_wachtwoord,_VALLIST),
	_IDCST_06_VALUE = dialog_VLGetstr(idcst_06,_VALLIST),
	_IDCST_CERTIFICAATFILE_VALUE = dialog_VLGetstr(idcst_certificaatfile,_VALLIST),
%END Toernooistart, Update controls
	true.

%MARK Toernooistart, new events

%BEGIN Toernooistart, idcst_05 _CtlInfo
  dlg_toernooistart_eh(_Win,e_Control(idcst_05,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	!.
%END Toernooistart, idcst_05 _CtlInfo

%BEGIN Toernooistart, idcst_selecteer _CtlInfo
  dlg_toernooistart_eh(Win,e_Control(idcst_selecteer,_CtrlType,_CtrlWin,_CtlInfo),0):-
	FN = dlg_GetFileName("*.pfx",["pfx","*.pfx"],"Selecteer certificaatbestand",[],"",_OutListFiles),
	dialog_SetStr(Win, idcst_certificaatfile, FN),
	!.
%END Toernooistart, idcst_selecteer _CtlInfo

%BEGIN Toernooistart, idcst_toernooi_bondsnummers _CtlInfo
  dlg_toernooistart_eh(_Win,e_Control(idcst_toernooi_bondsnummers,_CtrlType,_CtrlWin,_CtlInfo),0):-
	Checked = dialog_GetCheck(_Win, idcst_toernooi_bondsnummers),
	knltbrel(_Win, Checked),
	fail.
  dlg_toernooistart_eh(_Win,e_Control(idcst_toernooi_bondsnummers,_CtrlType,_CtrlWin,_CtlInfo),0):-
	b_False = dialog_GetCheck(_Win, idcst_toernooi_bondsnummers),
	%write("\nhier"),
	dialog_SetState(_Win, [enable(idc_ok,b_True)]),
	!.
  dlg_toernooistart_eh(_Win,e_Control(idcst_toernooi_bondsnummers,_CtrlType,_CtrlWin,_CtlInfo),0):- !.
%END Toernooistart, idcst_toernooi_bondsnummers _CtlInfo

%BEGIN Toernooistart, e_User
  dlg_toernooistart_eh(_Win,e_User(_Id,CtrlID),0):-
	CtrlWin = win_GetCtlHandle(_Win, CtrlId),
	win_SetFocus(CtrlWin),
	!.
%END Toernooistart, e_User

%BEGIN Toernooistart, e_Create
  dlg_toernooistart_eh(_Win,e_Create(_CreationData),0):-
	Checked = dialog_GetCheck(_Win, idcst_toernooi_bondsnummers),
	knltbrel(_Win, Checked),		% zet alle KNLTB-velden aan
	!.
%END Toernooistart, e_Create

%BEGIN Toernooistart, idc_help _CtlInfo
  dlg_toernooistart_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Groteschoonmaaknieuwtoernooi"),
	!.
%END Toernooistart, idc_help _CtlInfo

%BEGIN Toernooistart, idc_ok _CtlInfo
  dlg_toernooistart_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	b_False = dialog_GetCheck(_Win, idcst_toernooi_bondsnummers),
	trap(DateCCWin = win_GetCtlHandle(_Win,idcst_datecontrol),_,fail),
	MinOffset = date_cc_GetValue(DateCCWin),
	dt_mins_date_time(MinOffset,GekozenDag,_Time),
	getbacktrack(Here),
	dag(DagOud, _, _),
	cutbacktrack(Here),
	GekozenDag = DagOud,
	format(Msg, "De nieuwe datum is gelijk aan de oude!\nIs dat de bedoeling?"),
	2 = dlg_MessageBox( "Toernooistartdatum", Msg, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
	!.
  dlg_toernooistart_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	NNaam = dialog_getstr(_Win, idcst_naam),
	naam(IDCST_NAAM_VALUE, _, _, _, _, _),
	upper_lower(NNaam, IDCST_NAAM_VALUE), 
	format(Msg, "U moet de toernooinaam veranderen!\nZorg dat er ook een jaartal in voorkomt!"),
	startMelding(_Win, idcst_naam, "ToernooiNaam", Msg), !.
  dlg_toernooistart_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	b_True = dialog_GetCheck(_Win, idcst_toernooi_bondsnummers),
	TNr = dialog_GetStr(_Win, idcst_05),
	not(fronttoken(TNr, _, _)),
	format(Msg, "invullen svp!"),
	startMelding(_Win, idcst_05, "Toernooinummer", Msg), !.
  dlg_toernooistart_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	b_True = dialog_GetCheck(_Win, idcst_toernooi_bondsnummers),
	CWin = win_GetCtlHandle(_Win, idcst_certificaatfile),	
	CBestand = win_GetText(CWin),
	not(existfile(CBestand)),
	dlg_MessageBox( "Certificaat", "Wijs het certificaatbestand aan", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), !.
  dlg_toernooistart_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	b_True = dialog_GetCheck(_Win, idcst_toernooi_bondsnummers),
	WW = dialog_GetStr(_Win, idcst_wachtwoord),
	not(fronttoken(WW, _, _)),
	dlg_MessageBox( "Wachtwoord", "Geef het wachtwoord - u ontvangt dit per mail van de KNLTB.", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), !.
  dlg_toernooistart_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0) :-
	b_True = dialog_GetCheck(_Win, idcst_toernooi_bondsnummers),
	TNr = dialog_GetStr(_Win, idcst_05),
	trim_c(Tnr),
	%not(certificaatAanwezig(_Win,Tnr,_,_)),
	not(certificaatInstall(_Win,Tnr)),
	!.
  dlg_toernooistart_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0) :-
	b_True = dialog_GetCheck(_Win, idcst_toernooi_bondsnummers),
	TNr = dialog_GetStr(_Win, idcst_06),
	trim_c(Tnr),
	%not(certificaatAanwezig(_Win,Tnr,_,_)),
	not(certificaatInstall(_Win,Tnr)),
	!.
  dlg_toernooistart_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0) :-
	stopdbrefresh(),
	TNaam = dialog_GetStr(_Win, idcst_naam),
	TNr = dialog_GetStr(_Win, idcst_05),
	trim_c(Tnr),
	TNr2 = dialog_GetStr(_Win, idcst_06),
	trim_c(Tnr2),
	naam(_, _, _, _, District, _),
	addnr([], Tnr, NrsNew),
	addnr(NrsNew, Tnr2, NrsNew1),
	vpi_Alarm(mesbox_iconquestion),
	dialog_SetState(_Win,[show(idcst_progress,b_True)]),
	PWin = win_GetCtlHandle(_Win, idcst_progress),	
	close_alles(),
	close_editorwin(),
	gefingeerdeSpelersWissen(DeleteSp),
	assert(current(0)),
	resetVereniging1(Tnr),
	begin(PWin, DeleteSp),	
	getKNLTBdefinities(Tnr,nee,b_False,Doorgaan),
	retractall(onderdeelnr(_)),
	progress_bar_set_value(PWin, 100, 60),
	getNieuweStartDatum(_Win,Offset),
	verschuifdata(Offset),
	getKNLTBdefinities(Tnr2,nee,b_True,Doorgaan1),
	progress_bar_set_value(PWin, 100, 80),
	cleanonderdeeldefinities(NrsNew1),
	vpi_ProcessEvents(b_True ),
	toernooidatum(DatumN),
	assert(naam(TNaam, NrsNew1, DatumN, "", District, 0)),
	bitand(Doorgaan,Doorgaan1,Doorgaan2),
	opslaanOfTerug(TNaam, _Win, Doorgaan2),
	setNaam(),
	!.
%END Toernooistart, idc_ok _CtlInfo

  dlg_toernooistart_eh(_,_,_):-!,fail.

%END_DLG Toernooistart
%___________________________________________________________________________________________
predicates
  opg1(wedscat,string,integer)
  tgnst_verh(tgnst, tijd, verhl)
  in_club(tgnst,string,integer LotingRating)
nondeterm  niet_verh(integer, integerlist, integer, verhl)
  verhinderd(integer,integer,verhl)
nondeterm opgegFout(wedscat Cat, integer H)
procedure lotingSterkte(string, integer)
procedure lotingsterktedubbel(string, string, integer)
nondeterm opgegdebug(wedscat, integerlist, rec, integer Gereserveerd)
nondeterm toernooikExt(string, string)
  procedure ratingrapport(slist, string Titel, string Msg)

clauses

opg_cat(CatS,WC, 2) :-  	 % Functie = 2 --> alleen bovencats met criteria
  wc(WC, _, _, Ond, _, _, _, _, _, _, _, _, _, _,_),
  %wc(WC, Ond, _, _Shortp,_,_,_), 
  not(isOnderCat(WC)),
  format(CatS, "%s\t%s\t%s", "", "", Ond).
opg_cat(CatS,CatNo, Functie) :-  % Functie = 0 --> alleen ingedeeld
  Functie <> 2,
  wc(CatNo, _, _, Cat, _, _, _, _, _, _, _, _, _, _,_), % ga het lijstje van onderdeeln af
   opg1(CatNo,Text,Functie),     % en voorzie van merk
  findall(OpgaveNum, opg(OpgaveNum,CatNo,_,_,_,_,_,_,_,_,_),OpgL),
  count(OpgL, 0, Aant),
  format(CatS, "%s\t%3d\t%s", Text, Aant, Cat).
     
opg1(CatNo,"x ",_) :-            % al ingedeeld
  pos(CatNo,_,_,afval), !.
opg1(CatNo,"p ",_) :-            % al ingedeeld in poeltje
  pos(CatNo,_,_,poel), !.
opg1(_,"  ",1).

toernooikExt(Tid, Tid) :-
  toernooiK(Tid, _),
  getbackTrack(Here),
  wc(_, _, _, _, _, _, _, _, _, _, _, _, _, Tid,_),
  cutbacktrack(Here).
toernooiKExt("", "lokaal") :-
  wc(CatNo, _, _, _Naam, _, _, _, _, _, _, _, _, _, "", _),
  not(isondercat(CatNo)), !.

indelingX() :-
  cleanRelaties(),
  TaskWin = vpi_GetTaskWin(),
  findall(Tidx, toernooiKExt(Tidx, _), Tids),
  toernooinummerselect(Tids,Tid),
  dlg_deel_in_Create(TaskWin, [Tid]), !.
indelingX().

checkOpgegCrit(Cat, Tgnst, Messages) :-
  tgnstnum(Tgnst, Num),
  partnerCritFout(Cat, Num, Messages, b_True), !,	% messages en fail.
  fail.
checkOpgegCrit(_Cat, _Tgnst, _).

opgegdebug(Cat, Tijden, rec(H, Plts, Byes, 0, Club, Tijden_niet_Verh,LotingRating), Gereserveerden) :-
  opgeg(Cat, Tijden, rec(H, Plts, Byes, 0, Club, Tijden_niet_Verh,LotingRating), Gereserveerden),
  L = rec(H, Plts, Byes, 0, Club, Tijden_niet_Verh,LotingRating),
  term_str(rec, L, Str),
  writedevice(X),
  writedevice(screen),
  write("\n+++ ",Str),
  writedevice(X).

opgeg(Cat, Tijden, rec(H, Plts, Byes, 0, Club, Tijden_niet_Verh,LotingRating), _) :-
  opg(H,Cat,Tgnst,Plts,Byes,0,_,_,_,_,_),  % minus uitgeslotenen!
  not(Tgnst = openplaats(_)),
  checkOpgegCrit(Cat, Tgnst, b_True),
  in_club(Tgnst,Club,LotingRating), 
  tgnst_verh(Tgnst, Begin, Verh),
  findall(Tijd, niet_verh(Tijd, Tijden, Begin, Verh), Tijden_niet_Verh).
  %term_str(dbasedom, opg(H,Cat,Tgnst,Plts,Byes,0,0,"","",na,""),Str),
  %writedevice(X),
  %writedevice(screen),
  %tgnst_naam(ja, b_False, Cat, Tgnst,NaamU,_Tel),
  %write("\n+!!+ ",Str,NaamU),
  %writedevice(X).
opgeg(Cat, Tijden, rec(H, Plts, Byes, 0, "", Tijden_niet_Verh,0), Gereserveerden) :-
  opg(H,Cat,openplaats(N),Plts,Byes,0,_,_,_,_,_),
  N <= Gereserveerden,
  findall(Tijd, niet_verh(Tijd, Tijden, 0, []), Tijden_niet_Verh).

opgegFout(Cat, H) :-
  opg(H,Cat,_Tgnst,_Plts,_Byes,U,_,_,_,_,_),  
  U <> 0.
opgegFout(Cat, H) :-
  opg(H,Cat,Tgnst,_Plts,_Byes,0,_,_,_,_,_),  % minus uitgeslotenen!
  not(checkOpgegCrit(Cat, Tgnst, b_False)).

lotingSterkte(In, Uit) :-
  str_int(In, Uit), !.
  %Uit = Uit1 * 1000.			% 3.6.2004 hoeft niet meer
lotingSterkte(_, 0).

lotingSterkteDubbel(In1, In2, Uit) :-
  str_int(In1, Uit1),
  str_int(In2, Uit2),
  Uit = (Uit1 + Uit2) , !. 
lotingSterkteDubbel(_, _, 0).		% 0 wordt door indmc achteraan gezet!!

in_club(_, "",0) :-
  not(openbaar(ja)), !.
in_club(s(SpNo),ClubNummer,LotingRating) :-
  sp2(SpNo,_,_,_,_,_RatingE2,_RatingD2,_,_, ClubNummer, _, _, _, STE, _, _, _, _, _, _,_,_,_,_,_,_,_),
  lotingSterkte(STE, LotingRating), !.	% 2.6.2004 dus eigenlijk geen rating meer !!!
in_club(pw(SpNo),ClubNummer,LotingRating) :-
  sp2(SpNo,_,_,_,_,_RatingE2,_RatingD2,_,_, ClubNummer, _, _, _, _STE, STD, _, _, _, _, _,_,_,_,_,_,_,_),
  lotingSterkte(STD, LotingRating), !.
in_club(p(SpNo1,SpNo2),ClubNummer, Sterkte) :-
  sp2(SpNo1,_,_,_,_,_RatingE1,_RatingD1,_,_, ClubNummer, _, _, _, _, STD1, _, _, _, _, _,_,_,_,_,_,_,_), 
  sp2(SpNo2,_,_,_,_,_RatingE2,_RatingD2,_,_, _ClubNummerX, _, _, _, _, STD2, _, _, _, _, _,_,_,_,_,_,_,_),
  lotingsterktedubbel(STD1, STD2, Sterkte), !.
in_club(_,"", 0).  % 1 = niet echt

tgnst_verh(s(No), Begin, Verh) :-
  sp2(No,_,_,_,Verh,_RatingE2,_RatingD2,_,Begin, _, _, _, _, _, _, _, _, _, _, _,_,_,_,_,_,_,_), !.
tgnst_verh(pw(No), Begin, Verh) :-
  sp2(No,_,_,_,Verh,_RatingE2,_RatingD2,_,Begin, _, _, _, _, _, _, _, _, _, _, _,_,_,_,_,_,_,_), !.
tgnst_verh(p(No1,No2), Begin, Verh) :-
  sp2(No1,_,_,_,Verh1,_RatingE1,_RatingD1,_,Beg1, _, _, _, _, _, _, _, _, _, _, _,_,_,_,_,_,_,_),
  sp2(No2,_,_,_,Verh2,_RatingE2,_RatingD2,_,Beg2, _, _, _, _, _, _, _, _, _, _, _,_,_,_,_,_,_,_), !,
  append(Verh1,Verh2,Verh),
  grootste(Beg1, Beg2, Begin).
tgnst_verh(openplaats(_), 0, []).

verhinderd(Tijd, Begin, _) :-
  bitright(Tijd,7,Dag),
  dag(Dag, _, w),
  bitand(Tijd,$7F,T1), 
  T1 < Begin, !.
verhinderd(_,_,[]) :- !, fail.
verhinderd(Tijd,_,[per(T1,T2)|_]) :-
  duur(Duur),
  Tijd > T1 - Duur, 		% 23.12.2002
  Tijd < T2, !.
verhinderd(Tijd,Beg,[_|Rest]) :-
  verhinderd(Tijd,Beg,Rest).  

niet_verh(Tijd, Tijden, Begin, Verh) :-
  member(Tijd, Tijden),
  not(verhinderd(Tijd, Begin, Verh)).

%BEGIN_DLG Deel in
/**************************************************************************
	Creation and event handling for dialog: Deel in  
**************************************************************************/

domains
  kunne = man; vrouw; gemengd

constants

%BEGIN Deel in, CreateParms, 10:33:19-3.3.2015, Code automatically updated!
  dlg_deel_in_DlgType = wd_Modal
  dlg_deel_in_Title = "Deel in"
  dlg_deel_in_RCT = rct(40,32,422,260)
  dlg_deel_in_Flags = [wsf_Close,wsf_TitleBar]
  dlg_deel_in_Help = idh_deel_in
  dlg_deel_in_Font = "Arial"
  dlg_deel_in_FSize = 9
%END Deel in, CreateParms
  maxpoeldim = 20
  drievierplaats = 21
  extrafinale = 22
  deelincreate = 2
  deelinnaamgeving = 3
  deelinrefresh = 4
  controletimerefresh = 5
  
database - deelin
determ deelinwin(WINDOW, slist TidList)
single deelInIndex(integer)	% tellertje voor progress
single inkrimpen(integer) % 0 = eerste ronde, 1 = onderste helft
  single nieuweronde(string Naam, string Kort, catL Eigen)
nondeterm refresh()
single firstSelect(wedscat)
vorigeC(wedscat)
single hartbeat(integer)
determ ischanged()
determ oddeven(integer)
geenbondsnummer(string)
single headertekst(string)

predicates

  dlg_deel_in_eh : EHANDLER
  callbackI : cTreeView::callback
  gegevens(WINDOW, wedscat, integer, boolean MetVoorrondes)
procedure   checkPlaatsing(wedscat)
nondeterm  gep(catl, integer, integer)
nondeterm  baan_dag(integerlist, integer)
nondeterm opgU(integer, catl)
nondeterm opgC(integer, catl)
nondeterm opgNorm(catl, integer)
  selectedCat(WINDOW, wedscat) - (i, o)
  selectedCat(window, wedscat, integer AantalOpg) - (i, i, i)
  indeelbaar(WINDOW, wedscat, integer)
  get_tgnst(integer,wedscat,tgnst,integer) - (i,i,o,i)
  get_tgnst1(integer,tgnst) % voor alleen skelet
  aanbrengen(wedscat, integer NumWeds, integerlist Indeling, rsoort GewensteVorm, boolean SkeletOnly)
  uitslag_verw(integer,wedscat,rsoort)  
  volg1(integer,wedscat,integer,tgnst,rsoort) - (i,i,i,i,i)
  volg1(integer,wedscat,tgnst,rsoort) 
  selectAlsIngedeeld(wedscat)
  procedure wijsaan(catl, integer Index) - (i, o)
  wijsaan(wedscat, catl, integer, integer) - (i,i,i,o)
  in_weds1(wedscat, tgnst, integer)
  onbezet(tgnst, tgnst)
procedure   mvg(sexlist, kunne)
nondeterm   soortGenoot(wedscat, string Naam)
procedure   cat1(wedscat Cat, string) 
nondeterm   algeselecteerd(wedscat CatE, string SelNaam)
procedure   preselectV(slist CatNamen, slist PNamen, integer , ilist Selected)
determ      verwijderOnderdeelNiet(wedscat)
procedure   nieuweSelect(window, wedscat, wedscat)
determ	    naamgeving(window)
determ      checkGereserveerd(window Win, wedscat Cat, rsoort GewensteVorm, integer Aantal1, integer Gereserveerd)
procedure mededeling(ilist)
determ    preForm(DIALOG_CONTROL_ID, rsoort)
%nondeterm teVerbinden(wedscat, string)  % kan gegroepeerd worden
determ    bestaandOfNieuw(window Win, wedscat Cat, wedscat BestaandNieuw)  % voor indelen
procedure  alPlanning(wedscat Cat, string tekst, integer Icon)
determ eigenvoorrondeOfVrij(wedscat, wedscat Cat)
procedure volgendeTwee(window Win, wedscat Cat)
determ eenTwee34(wedscat, string Titel, string Tekst, integer)
procedure plaats34(window, wedscat SelectedOnderdeel, integer Type)
%procedure enableDrieVier(wedscat, boolean)
%procedure enableEenTwee(wedscat, boolean)
procedure getHiddenPV(string Inhoud, slist In, slist In)
procedure openplaatsenRap()
nondeterm openPlaats(wedscat, string Tekst)
nondeterm vorigeW(wedscat Cat, wedscat CatV) - (i,o)
procedure maakPoules(wedscat TopCat, integer A, catl PouleCats) - (i,i,o)
procedure pouleCats(integer A, wedscat CatVrij, catl PouleCats)  
procedure checkGelijkeClubs(wedscat, tgl, integer, integer)
procedure clubcomp(slist Clubs, integer, integer)
procedure indelinglist2tgnstlist(wedscat, integerlist, tgl).
procedure   subcomp(string Club3, string, string, string, integer Weight2, integer In, integer Uit)
procedure byefactor(string, string, integer)
%procedure cleankoppelingen()
procedure xml_minVoornaamA(string, string)
procedure xml_voorvoegsel1A(string Voornaam, string Rest1, string In, string VoorvoegselU)
procedure xml_voorvoegselA(string Voornaam, string Rest1, string Voorvoegsel)
%nondeterm valterwattechecken(string Tid)
procedure geldigbondsnummer(string In, string Uit)
procedure getAcceptatieresultaat(window, string Tid)
procedure getControleresultaat1(string Tid)
procedure setAcceptatie()
procedure setMetServices()
nondeterm dependent(wedscat, wedscat, string Tid)
nondeterm geldigTeam(tgnst)
%nondeterm basicCats(wedscat, string Tid)
procedure ruimOp_Links(wedscat)
procedure stopdbrefreshIfNotInschrijfcontrole()
%procedure handleVerKoppeling(wedscat Van, wedscat Naar)
nondeterm siblingSchsrt(wedscat Cat1, string SchSrt, wedscat Eigen)
procedure tidList_to_tekst(slist, string)
procedure toernooitype_to_tekst(toernooisoort, string)
procedure tid2koppeltekst(string, string)
determ algespeeld(string)
determ dependOpgExists(wedscat Ond, string Tid)
determ eriseenopgaveomtechecken(string Tid)
procedure movekoppeling(wedscat,wedscat) 
procedure soort2geslacht(integer, string)
nondeterm certificaatCheck(string Tid)	% failt als geldig
nondeterm certisongeldig(string GeldigTM)
nondeterm tid_report_cat(string, string)
determ mededelingC(string)
procedure ophoudenmetchecken()

clauses

deelInIndex(0).
inkrimpen(0).
nieuweronde("", "", []).
hartbeat(3).
headertekst("").

in_weds1(Cat, Tgnst, 1) :-
  wd(_,Cat,Tgnst,_,_,_,_), !.
in_weds1(Cat, Tgnst, 1) :-
  wd(_,Cat,_, Tgnst,_,_,_), !.
in_weds1(Cat, Tgnst, 0) :-
  opg(_,Cat,Tgnst,_,_,0,_,_,_,_,_), !.

uitslag_verw(_, _ , poel) :- !.	    % voor poel niet verder 
uitslag_verw(No,Cat,Schema) :-      /* allebij zijn bye       */ 
  wd(No,Cat,bye,bye,_,_,_), !,
  volg1(No,Cat,bye,Schema).
uitslag_verw(No,Cat,Schema) :-      /* een van de twee is bye */
  wd(No,Cat,WinNo,bye,_,_,_), !,
  volg1(No,Cat,WinNo,Schema).
uitslag_verw(No,Cat,Schema) :-      /* de ander is bye        */
  wd(No,Cat,bye,WinNo,_,_,_), !,
  volg1(No,Cat,WinNo,Schema).
uitslag_verw(No,Cat,Schema) :-      /* uitslag onbekend        */
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
  uitslag_verw(No,Cat,Schema).
volg1(No,Cat,0,Winnaar,Schema) :-
  !, 
  logf('Z',wd(No,Cat,Winnaar,onbekend,"",0,""),ja),
  uitslag_verw(No,Cat,Schema).

apw(Cat,[O1,O2|Rest],WdNo,Schema, b_False) :- 
  X = WdNo * 2,
  get_tgnst(O1,Cat,T1,X),
  X1 = X + 1,
  get_tgnst(O2,Cat,T2,X1),
  logf('Z',wd(WdNo,Cat,T1,T2,"",0,""),ja),
  uitslag_verw(WdNo,Cat,Schema),
  W1 = WdNo + 1, !,
  apw(Cat,Rest,W1,Schema, b_False).
apw(Cat,[O1,O2|Rest],WdNo,Schema, b_True) :- 	% skelet only + byes
  get_tgnst1(O1,T1),				% bye = 0
  get_tgnst1(O2,T2),
  logf('Z',wd(WdNo,Cat,T1,T2,"",0,""),ja),
  uitslag_verw(WdNo,Cat,Schema),
  W1 = WdNo + 1, !,
  apw(Cat,Rest,W1,Schema, b_True).
apw(Cat,_/*[]*/,_,_, _) :- !,   /* stop de indeling + wedstijden in de database */
  retractall(opg(_,Cat,openplaats(_),_,_,_,_,_,_,_,_)), % igv poule, kan weer weg
  logclose().

get_tgnst1(0,bye) :- !.
get_tgnst1(_,onbekend).

get_tgnst(-1,_,onbekend,_) :- !.
get_tgnst(0,_,bye,_) :- !.
/*get_tgnst(H,Cat,onbekend,Plaats,b_True) :-
  isNrt(Cat),
  opg(H,Cat,openplaats(CatV),_,_,_,_,_,_,_,_),
  logf('Z', kopl(CatV,rang(1),Cat, plaats(Plaats)),ja),
  !.*/   
get_tgnst(H,Cat,Tgnst,_) :-
  opg(H,Cat,Tgnst,_,_,_,_,_,_,_,_), !.
get_tgnst(H,Cat,_,Plaats) :-
  format(XX, "\nget_tgnst failt! H:%d Cat:%D Tgnst: %s Plaats:%d",H,Cat, "--TS--",Plaats),
  _Answer = dlg_MessageBox( "Opgelet", XX, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.
  
opg_tgnst_list(_,[],[]) :- !.
opg_tgnst_list(Cat,[O1,O2|Tail],[i(T1,T2)|Out]) :- 
  get_tgnst(O1,Cat,T1,0),
  get_tgnst(O2,Cat,T2,0), !,
  opg_tgnst_list(Cat,Tail,Out).
opg_tgnst_list(_,_X,[]). % :- write("\n", X, "\n"), beep, !.

%  Aantal           Gereserveerd  Poel  Afval  Indelen
%     0                 <3          -     -       -
%     0                 >2          -     *       *
%     1                 >1          -     *       *
%     2                 0           *     -       *
%     2                 >0          *     *       *
%     3 - maxpoeldim    -           *     *       *
%     >maxpoeldim       -           -     *       *

indeelbaar(Win, Cat, 0) :-
  dialog_SetState(Win, [enable(idc_ok, b_False)]),
  kopl(_, _, Cat, _), !.	% derde/vierde plaats
indeelbaar(Win, _, _Aantal) :-
  dialog_SetState(Win, [enable(idc_ok, b_True)]),
  !.

opgU(No, CatL) :-
  member(Cat, CatL),
  opg(No, Cat, _, _, _, U, _, _,_,_,_),
  U <> 0.
  
baan_dag(Dagen, Tijd) :-
  bn(Tijd, N),
  N <> 0,
  bitright(Tijd,7,Dag),
  member(Dag, Dagen).
  
gep(CatL, Gep, Pl) :-
  member(Cat, CatL), 
  opg(Gep, Cat, _, Pl, _, 0, _, _,_,_,_),
  Pl <> 0.

opgC(Cand, CatL) :-
  member(Cat, CatL),
  opg(Cand, Cat, Tgnst, _, _, _, _, _,_,_,_),
  not(in_weds1(Cat, Tgnst, 1)).

opgNorm(CatL, Num) :-
  member(Cat, CatL),
  opg(Num, Cat, _, _, _, 0, _, _,_,_,_).


checkPlaatsing(Cat) :-
  findall(Plaats, gep([Cat], _, Plaats), PlList),
  sortint_c(PlList),
  reverse(PlList, [], PlListRev),
  PlListRev = [PlMax | _], 
  checkPlaatsing1(PlList, 1, PlMax, Missende),
  signaleerMissende(Missende), !.
checkPlaatsing(_Cat).

checkPlaatsing1([X|R], X, PlMax, Missende) :-
  N1 = X + 1, !,
  checkPlaatsing1(R, N1, PlMax, Missende).
checkPlaatsing1([X|R], N, PlMax, [N|Missende]) :-
  X > N, !,
  N1 = N + 1,
  checkPlaatsing1([X|R], N1, PlMax, Missende).
checkPlaatsing1([X|R], N, PlMax, Missende) :-
  X < N, !,
  format(Msg, "Dubbele plaatsing: %u!", X),
  dlg_MessageBox( "Plaatsing", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  checkPlaatsing1(R, N, PlMax, Missende).
checkPlaatsing1(R, N, PlMax, [N|Missende]) :-
  R = [_|_],
  N1 = N + 1, !,
  checkPlaatsing1(R, N1, PlMax, Missende).
checkPlaatsing1(_, _N, _PlMax, []).

signaleerMissende([]) :- !.
signaleerMissende(List) :-
  term_str(integerlist, List, String),
  format(Msg, "Missende (overgeslagen nummer) plaatsing: %s.\n\nNB Alleen van belang bij afvalschema.", String),
  dlg_MessageBox( "Plaatsing", Msg, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  !.
 
gegevens(Win, Cat, AantalEigen,b_True) :-
  %wc(Cat, _, _, _Ond, _, _, _, _, _, _, _, "0", _, _,_),
  isMasterCat(Cat),
  findall(NumC, opgNorm([Cat], NumC), NumCList),
  count(NumCList, 0, AantalEigen),
  findall(Num, opgNorm([Cat], Num), Numlist),	% minus uitgesloten
  findall(Cand, opgC(Cand, [Cat]), CandList),
  count(Numlist, 0, Aantal1),
  count(CandList, 0, CandAant),
  format(Zin1, "Aantal inschrijvingen  = %u.", Aantal1),
  format(Zin2, "Totaal nog kandidaat   = %.", CandAant),
       Zin3 = "Zie voor details\n de voorrondes zelf!",
  format(GegevenS, "%s\n%s\n%s", Zin1, Zin2, Zin3),
  GegWin = win_GetCtlHandle(Win, idctd_gegevens),
  win_SetText(GegWin, GegevenS), !.    
gegevens(Win, Cat, Aantal1,_) :-
  CatList = [Cat], 
  wc(Cat, _, _, _Ond, _, _, _, _, _, _, _, _, _, _,_),
  %wc(Cat,_Ond,_,_,_,_,_),
  findall(Num, opgNorm(CatList, Num), Numlist),	% minus uitgesloten
  findall(Gep, gep(CatList, Gep, _), Geplist),
  findall(Um, opgU(Um, CatList), Ulist),	% minus uitgesloten
  findall(Cand, opgC(Cand, CatList), CandList),
  count(Numlist, 0, Aantal1),
  count(Geplist, 0, Gepaant),
  count(Ulist, 0, Uaant),
  count(CandList, 0, CandAant),
  findall(Nietgek, nietgekeurdeopgaven(Cat, NietGek), NGList),
  count(NGList, 0, NGAant),
  findall(Afgek, afgekeurdeopgaven(Cat, AfGek), AGList),
  count(AGList, 0, AGAant),
  format(Zin1, "Aantal inschrijvingen  = %u.", Aantal1),
  format(Zin2, "Incl.  geplaatsten     = %.", Gepaant),
  format(Zin3, "Excl.  uitgeslotenen   = %.", Uaant),
  format(Zin4, "Excl.  nog kandidaat   = %.", CandAant),
  format(Zin5, "Niet gekeurd (nog niet)= %u.", NGAant),
  format(Zin6, "Afgekeurd              = %u.", AGAant),
  format(GegevenS, "%\n%\n%\n%\n%\n%", Zin1, Zin2, Zin3, Zin4, Zin5, Zin6),
  GegWin = win_GetCtlHandle(Win, idctd_gegevens),
  win_SetText(GegWin, GegevenS), !.    
gegevens(Win, Cat, AantalEigen,b_True) :-
  findall(NumC, opgNorm([Cat], NumC), NumCList),
  count(NumCList, 0, AantalEigen),
  findall(Catx, voorronde(Cat, Catx), CatList),
  %write("\nCatlist: ",CatList," Aantaleigen: ",AantalEigen),
  %wc(Cat, _, _, Ond, _, _, _, _, _, _, _, _, _, _,_),
  %write("\n", Ond),
  %wc(Cat,_Ond,_,_,_,_,_),
  findall(Num, opgNorm(CatList, Num), Numlist),	% minus uitgesloten
  findall(Cand, opgC(Cand, CatList), CandList),
  %write("\nOpgNorm",Numlist," opgC",CandList),
  count(Numlist, 0, Aantal1),
  count(CandList, 0, CandAant),
  format(Zin1, "Aantal inschrijvingen  = %u.", Aantal1),
  format(Zin2, "Totaal nog kandidaat   = %.", CandAant),
       Zin3 = "Zie voor details\n de voorrondes zelf!",
  format(GegevenS, "%s\n%s\n%s", Zin1, Zin2, Zin3),
  GegWin = win_GetCtlHandle(Win, idctd_gegevens),
  win_SetText(GegWin, GegevenS), !.    

selectedCat(Win, Cat) :-		%
  dialog_SetState(Win, [	% disable ze eerst allemaal...
  	enable(idcd_pbopstel,b_False),
  	enable(idcd_wis,b_False),
  	enable(idcd_pbronde,b_False),
	enable(idcd_spelers_plaatsen,b_False),
  	enable(idcd_splits,b_False),
  	enable(idcd_combineer,b_False),
  	enable(idcd_verliezer,b_False),
  	enable(idcd_34plaats,b_False),
  	enable(idcd_eindronde,b_False),
  	enable(idcd_verbindt,b_False),
  	%enable(idcd_verwerkingsverslag,b_False),
  	enable(idcp_uitloothulp,b_False)
	]),
  LboxWin = win_GetCtlHandle(Win, idcd_lbond),
  cTreeView::get_selected(LboxWin, Nr),
  vink(Cat, Nr),
  %write("\nSelectedCat: ", Cat),
  findall(Num, opg(Num, Cat, _, _, _, _, _, _,_,_,_), Numlist),	
  count(Numlist, 0, Aantal),
  PbWin = win_GetCtlHandle(Win, idcd_wis),
  win_SetText(PbWin, "Wis indeling"),
  selectedCat(Win, Cat, Aantal), !.	% en zet de juiste een voor een weer aan

selectedCat(Win,  Cat, _Opg) :-	% voor de knoppen
  pos(Cat, _, _, _Type),	% ingedeeld
  dialog_SetState(Win, [enable(idcd_pbopstel,b_True)]),
  fail.
selectedCat(Win,  Cat, Opg) :-
  not(koplv(_, Cat)),
  not(pos(Cat, _, _, _)),
  Opg = 0,
  PbWin = win_GetCtlHandle(Win, idcd_wis),
  win_SetText(PbWin, "Wis onderdeel"),
  dialog_SetState(Win, [enable(idcd_wis,b_True)]),
  fail.
selectedCat(Win,  Cat, Opg) :-	% 12.11.2003 voor finale
  not(kopl(_, algemeen, Cat, _)),
  not(kopl(_, pl3en4(_), Cat, _)),	% niet derde/vierde plaats
  kopl(CatV, rang(_), Cat, _),
  not(pos(CatV, _, _, afval)),
  Opg = 0,
  PbWin = win_GetCtlHandle(Win, idcd_wis),
  win_SetText(PbWin, "Wis onderdeel"),
  dialog_SetState(Win, [enable(idcd_wis,b_True)]),
  fail.
selectedCat(Win,  Cat, _Opg) :-	% alleen indeling wordt gewist
  not(koplv(_, Cat)),	% niet als er nog links naar zijn
  %not(kopl(_, 2, Cat, _)),
  pos(Cat, _, _, _),
  dialog_SetState(Win, [enable(idcd_wis,b_True)]),
  fail.
selectedCat(Win,  Cat, _Opg) :-	% alleen indeling wordt gewist
  kopl(_, algemeen, Cat, algemeen),
  pos(Cat, _, _, _),
  dialog_SetState(Win, [enable(idcd_wis,b_True)]),
  fail.
selectedCat(Win,  Cat, _Opg) :-
  pos(Cat, _, _, afval),	% ingedeeld in afval
  dialog_SetState(Win, [enable(idcd_pbronde,b_True)]),
  fail.
selectedCat(Win,  Cat, _Opg) :-	% 15.3.2007
  not(pos(Cat, _, _, _)),	% niet ingedeeld
  dialog_SetState(Win, [enable(idcd_pbronde,b_True)]),
  fail.
selectedCat(Win,  _Cat, Opg) :-
  Opg > 0,
  dialog_SetState(Win, [enable(idcd_spelers_plaatsen,b_True),enable(idcp_uitloothulp,b_True)]),	% 21.10.2007
  fail.
selectedCat(Win,  Cat, Opg) :-
  Opg > 1,			% meer dan één opgave
  not(kopl(_, _, Cat, _)),	% niet derde/vierde plaats
  %not(kopl(Cat, _, _, _)),
  %wc(Cat,_,_,_,_,_,Verl),
  %not(wc(Verl,_,_,_,_,_,_)),	% niet als ie een verliezersronde heeft
  not(pos(Cat,_,_,_)),
  dialog_SetState(Win, [enable(idcd_splits,b_True)]),
  fail.
selectedCat(Win,  Cat, Opg) :-
  Opg > 4,			% meer dan vier opgaven
  %wc(Cat, _, _, _Ond, _, _, _, _, _, _, _, "0", _, _,_),
  isMasterCat(Cat),
  not(pos(Cat,_,_,_)),
  dialog_SetState(Win, [enable(idcd_splits,b_True)]),
  fail.
/*selectedCat(Win,  Cat, Opg) :-
  Opg > 1,			% meer dan één opgave
  kopl(Cat, algemeen, _, algemeen),
  dialog_SetState(Win, [enable(idcd_splits,b_True)]),
  fail.*/
selectedCat(Win,  Cat, _Opg) :-
  not(isOnderCat(Cat)),
  dialog_SetState(Win, [enable(idcd_combineer,b_True)]),
  fail.
selectedCat(Win,  Cat, _Opg) :-
  isOnderCat(Cat),
  %kopl(Cat, _, Catx, _),
  %kopl(Cata, _, Catx, _),
  %not(Cata = Cat),
  dialog_SetState(Win, [enable(idcd_combineer,b_True)]),
  fail.
selectedCat(Win,  Cat, Opg) :-
  Opg > 0, 
  %not(isOndercat(Cat)),		
  pos(Cat, _, _, afval),	% ingedeeld in afval
  dialog_SetState(Win, [enable(idcd_verliezer,b_True)]),
  fail.
selectedCat(Win,  Cat, _Opg) :-
  not(isOndercat(Cat)),		
  pos(Cat, Card, _, _),		% ingedeeld
  Card > 1, 
  dialog_SetState(Win, [enable(idcd_34plaats,b_True)]),
  fail.
selectedCat(Win,  Cat, _Opg) :-
  wc(Cat, OID, _, _, _, _, _, _, _, _, _, _, _, _,_),
  fronttoken(OID, _, _),
  kopl(Cat, _, Cat0,_),		
  wc(Cat0, OID1, _, _, _, _, _, _, _, _, _, _, _, _,_),
  OID1 <> OID,
  pos(Cat, Card, _, _),		% ingedeeld
  Card > 1, 
  dialog_SetState(Win, [enable(idcd_34plaats,b_True)]),
  fail.
selectedCat(Win,  Cat, _Opg) :-
  kopl(Cat,verliezer,_,_),
  %wc(_Catx,_Naam,_,_,_,_,Cat),
  dialog_SetState(Win, [enable(idcd_34plaats,b_True)]),
  fail.
selectedCat(Win,  Cat, _Opg) :-
  not(isOndercat(Cat)),		% ingedeeld
  wc(Cat, _, _, _, _, _, _, _, _, _, _, _, _, Tid,_),
  not(fronttoken(Tid,_,_)),
  dialog_SetState(Win, [enable(idcd_eindronde,b_True)]),
  fail.
selectedCat(Win,  Cat, _Opg) :-
  isMasterCat(Cat),		% ingedeeld
  dialog_SetState(Win, [enable(idcd_eindronde,b_True)]),
  fail.
selectedCat(Win, Cat, _Opg) :-
  not(isOndercat(Cat)),
  getbacktrack(Here),
  wc(Cat, _, _, _, _, _, _, _, _, _, _, _, _, Tid,_),
  not(toernooiK(Tid,jrt)),
  teVerbinden(Cat, _Cat2),
  cutbacktrack(Here),
  dialog_SetState(Win, [enable(idcd_verbindt,b_True)]),
  fail.
selectedCat(Win, Cat, _Opg) :-
  kopl(Cat, algemeen, _, _),		% los te snijden
  dialog_SetState(Win, [enable(idcd_verbindt,b_True)]),
  fail.
selectedCat(_, _, _).

/*
enableDrieVier(Cat, b_True) :-
  pos(Cat, _, _, afval), !.	% ingedeeld in afval
enableDrieVier(Cat, b_True) :-
  kopl(Cat,verliezer,_,_), !.
enableDrieVier(_Cat, b_False).
  
enableEenTwee(Cat, b_True) :-
  pos(Cat, _, _, poel), !.	% ingedeeld in afval
enableEenTwee(_Cat, b_False).
*/

eenTwee34(Cat, "Toevoegen", "Extra finale van de poule toevoegen?", extrafinale) :-
  pos(Cat, _, _, poel), !.	% ingedeeld in afval
eenTwee34(Cat, "Toevoegen", "Wedstrijd om de 3/4 plaats?", drievierplaats) :-
  pos(Cat, _, _, afval), !.	% ingedeeld in afval
eenTwee34(Cat, "Toevoegen", "Wedstrijd om de 3/4 plaats/nin de verliezersronde?", drievierplaats) :-
  kopl(Cat,verliezer,_,_), !.

indelinglist2tgnstlist(Cat, [O | OTail], [T | TTail]) :-
  get_tgnst(O, Cat, T, 0) ,  
  indelinglist2tgnstlist(Cat, OTail, TTail),   !.
indelinglist2tgnstlist(_, _, []).

checkGelijkeClubs(Cat, [T1, T2, T3, T4 | Tail], In, Uit) :-
  in_club(T1,Club1,_), 
  in_club(T2,Club2,_), 
  in_club(T3,Club3,_), 
  in_club(T4,Club4,_),
  clubcomp([Club1, Club2, Club3, Club4], In, In1), !,
  checkGelijkeClubs(Cat,  Tail, In1, Uit).
checkGelijkeClubs(Cat, [T1, T2 | Tail], In, Uit) :-
  in_club(T1,Club1,_), 
  in_club(T2,Club2,_), 
  clubcomp([Club1, Club2], In, In1), !,
  checkGelijkeClubs(Cat,  Tail, In1, Uit).
checkGelijkeClubs(_,  _, Uit, Uit).

byefactor("", "", 2) :- !.
byefactor("", _, 1) :- !.
byefactor(_, "", 1) :- !.
byefactor(_, _, 0).

subcomp(Club1, Club2, Club3, Club4, Value, In, Uit) :-
  byefactor(Club3, Club4, BF),
  Club1 = Club2,
  fronttoken(Club1, _, _),
  Uit = Value + BF + In, !.
subcomp(_, _, _, _, _, In, In).

clubcomp([Club1, Club2, Club3, Club4], NE, Uit) :-  % 1 2
  subcomp(Club1, Club2, Club3, Club4, 2, Ne, U1),
  subcomp(Club1, Club3, Club2, Club4, 1, U1, U2),
  subcomp(Club2, Club3, Club1, Club4, 1, U2, U3),
  subcomp(Club1, Club4, Club2, Club3, 1, U3, U4),
  subcomp(Club2, Club4, Club1, Club3, 1, U4, U5),
  subcomp(Club3, Club4, Club1, Club2, 2, U5, Uit), !.
clubcomp([Club1, Club2],  In, Uit) :-
  subcomp(Club1, Club2, "-", "-", 2, In, Uit), !.
clubcomp(_, In, In).

aanbrengen(Cat,NumWeds, Indeling, GewensteVorm, SkeletOnly) :-
  pos(Cat,_,_,_),
  card(NumWeds, Card),
  alPlanning(Cat, Tekst, Icon),
  format(Msg2, "Bestaande indeling vervangen?%s", Tekst),
  1 = dlg_MessageBox( "Indeling", Msg2, Icon, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
  close_alles(),
  wd(WdNo,Cat,T1,T2,Uitsl,St,_),
  logf('T',wd(WdNo,Cat,T1,T2,Uitsl,St,""),ja),
  logf('A',pos(Cat,Card,Card,GewensteVorm),ja),
  apw(Cat, Indeling,Card, GewensteVorm, SkeletOnly),
  logclose(), !.
aanbrengen(Cat,NumWeds, Indeling, GewensteVorm, SkeletOnly) :-
  not(pos(Cat,_,_,_)),
  %indelinglist2tgnstlist(Cat, Indeling, Tgl),
  %checkGelijkeClubs(Cat, Tgl, 0, Uit),
  %Uit = 0,
  card(NumWeds, Card),
  logf('A',pos(Cat,Card,Card,GewensteVorm),ja),
  apw(Cat, Indeling,Card, GewensteVorm, SkeletOnly),
  logclose(), !.
aanbrengen(_Cat,_NumWeds, _Indeling, _GewensteVorm, _) :-
  logclose().

alPlanning(Cat, "\nBestaande planning en uitslagen gaan verloren.", mesbox_iconexclamation) :-
  w2(_, Cat, _, _, _, _, _, _), !.  
alPlanning(Cat, "\nBestaande uitslagen gaan verloren.", mesbox_iconexclamation) :-
  w3(_, Cat, _, _, _, _, _), !.
alPlanning(_Cat, "", mesbox_iconquestion).

selectAlsIngedeeld(Cat) :-
  pos(Cat, _, W, _),
  select(Cat, W), !.
selectAlsIngedeeld(_) :-
  close_alles.
  
wijsaan(CatL, Uit) :-
  pos(CatCurr, _, _, _),		% zet op de huidige
  wijsaan(CatCurr, CatL, 0, Uit), !.
wijsaan(_, 0).

wijsaan(Item, [Item|_], In, In) :- !.
wijsaan(Item, [_|Rest], In, Uit) :-
  In1 = In + 1, !,
  wijsaan(Item, Rest, In1, Uit).
wijsaan(_, _, _, 0) :- !.

deelDelenInInPoule(_CatL, idcs_niet) :- !.
deelDelenInInPoule(CatL, AfvalPoule) :-
  %count(CatL, 0, Aantal),
  assert(deelInIndex(0)),
  member(Cat, CatL),
  deelInIndex(Index),
  I1 = Index + 1,
  assert(deelInINdex(I1)),
  not(pos(Cat, _, _, _)),	% anders niet opnieuw
  findall(Opgegevens,opgeg(Cat,[],Opgegevens,0),OpgLi),
  findall(H, opgegFout(Cat, H), HList),
  mededeling(HList),
  preForm(AfvalPoule, A_P),
  %write("\n", OpgLi),
  myloting(OpgLi,0,Indeling,NumWeds,A_P),
  aanbrengen(Cat, NumWeds, Indeling, A_P, b_False),
  fail.
deelDelenInInPoule([Cat|_], _) :-
  pos(Cat, _,No,_), !,
  select(Cat, No).
deelDelenInInPoule(_, _).

preForm(idcs_poules, poel) :- !.
preForm(_, afval).

mededeling([]) :- !.
mededeling(List) :-
  count(List, 0, Aantal),
  format(Msg, "%u opgaven blijven geparkeerd (kandidaat)\nPlaats die eventueel zelf met wijzigopstelling!", Aantal),
  dlg_MessageBox( "Geparkeerd", Msg, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), !.

toernooiKP(Tid, TypeS) :-
  toernooiK(Tid, Type), !,
  term_str(toernooisoort,Type, TypeS).
toernooiKP(_, "lokaal").

callbackI(_, dblclick(_)) :-
  deelinWin(Win, _),
  TTWin = win_GetCtlHandle(Win, idcd_spelers_plaatsen),
  win_SendEvent(Win,e_Control(idcd_spelers_plaatsen,8,TTWin,activated())), !.
  %win_PostEvent(Win,e_User(deelinnaamgeving, 0)), !.
callbackI(_,selected(_, CatNr)):-
  vink(Cat, CatNr),
  not(refresh()),
  deelinWin(Win, _), 
  selectedCat(Win, _Catx),		% Catx = Cat!
  voorrondes(Cat, MetVoorrondes),
  gegevens(Win, Cat, Aantal, MetVoorrondes),
  wc(Cat, OnderdeelId, _, _K2, _, _, _, _, _, _, _, Schsrt, _, Tid,_),
  %wc(Cat,K2,_,_,_,_,_),
  %toernooiKP(Tid, TypeS),
  %concat(OnderdeelId, "      ", O1),
  %frontstr(6, O1, _TTT, Nr),
  %trim_c(Nr),
  %format(Nrx, "%s %u", Nr, Cat) ,
  %trim_c(Nrx),
  tid2koppeltekst(Tid, KT),
  format(Kenm, "%u %s", Cat, Schsrt),
  trim_c(Kenm),
  format(Titel, "%s(%s)\n%s\n%s", KT, Kenm, OnderdeelId),
  TTWin = win_GetCtlHandle(Win, idctd_datum_lcheck),
  win_SetText(TTWin, Titel),
  close_alles(),
  indeelbaar(Win, Cat, Aantal),
  posP(_, Cat, _,No,_),
  select(Cat, No),
  win_SetFocus(Win),
  fail.

tid2Koppeltekst("", "") :- !.
tid2Koppeltekst(Tis, Tekst) :-
  format(Tekst, "hoort bij dit toernooinummer: %s ", Tis).

naamgeving(_Win) :-
  LboxWin = win_GetCtlHandle(_Win, idcd_lbond),
  cTreeView::get_selected(LboxWin, Nr),
  cTreeView::get_item(LboxWin, Nr, _, _, BMap1, BMap2),
  vink(Cat, Nr),
  getbacktrack(Here),
  wc(Cat, OnderdeeelID, N, ER, EDG, KN, SexL, SpStrk, LftV, LftTM, HDG, Schemasoort, VanWie, KNLTB,Geld),
  %wc(Cat,ER,EDG,KN,VoorWie,Prijs,Verl),
  cutbacktrack(Here),   
  assert(nieuweronde(ER, KN, [Cat])),
  dlg_onderdeelgegevens_Create(_Win, Return),
  Return = 1,
  not(nieuweronde(ER, KN, [Cat])),		% ER en KN veranderd
  nieuweronde(OndNaam, KorteNaam, _),
  setOndNaam(Cat, OndNaam),
  format(NNaam, "%s * %s", KorteNaam, OndNaam),
  cTreeView::get_item(LboxWin, Nr, _, Style,_,_),
  overblijvendeOpgave(Cat, Style, State, alle),
  cTreeView::set_item(LboxWin, Nr, NNaam, [selected()|State], BMap1, BMap2), 
  findall(Cat1, wc(Cat1, _, _, _, _, _, _, _, _, _, _, _, _, _,_), Cat1L),	% zelfde pos in wclijst !
  %findall(Cat1, wc(Cat1,_,_,_,_,_,_), Cat1L),	% zelfde pos in wclijst !
  positionWC(Cat, Cat1L, Cat1L1),
  evenWegWC(Cat1L1, Cat1L1Vol),			% haal achterste stuk weg
  Cat1L1Vol = [_|Cat1L1Volx],
  trim_c(OndNaam),
  logf('Z',wc(Cat, OnderdeeelID, N, Ondnaam, EDG, KorteNaam, SexL, SpStrk, LftV, LftTM, HDG, Schemasoort, VanWie, KNLTB,Geld),ja),
  %logf('Z',wc(Cat,OndNaam,EDG,KorteNaam,VoorWie,Prijs,Verl),ja),
  sluitAanWC(Cat1L1Volx),			% en voeg weer toe
  logclose(), !.

deleteKnopen(LboxWin) :-
  cTreeView::get_all(LboxWin, List),
  reverse(List, [], RList),		% van achter naar voren
  deleteKnopen(LboxWin, RList).

deleteKnopen(LboxWin, [tree(Item, _, _, _, _, _)|T]) :-
  cTreeView::delete_item(LboxWin, Item), !,
  deleteKnopen(LboxWin, T).
deleteKnopen(_, _).

mvg(SrtList, gemengd) :-
  getbacktrack(Here),
  member(Srtm, SrtList),
  spsrt(Srtm, _SNaam, m, _LftVan, _LftTot, _StrkVan, _StrkTot, _Reserve),
  cutbacktrack(Here),
  member(Srtv, SrtList),
  spsrt(Srtv, _, v, _, _, _, _, _), !.
mvg(SrtList, man) :-
  member(Srtm, SrtList),
  spsrt(Srtm, _SNaam, m, _LftVan, _LftTot, _StrkVan, _StrkTot, _Reserve), !.
mvg(_SrtList, vrouw).

soortGenoot(Cat, CatNaam) :-
  wc(Cat, _, _, _, EDG, _, VoorWie1, _SpStrk, _LftV, _LftTM, _HDG, _Schemasoort, _Keuring, _KNLTB, _Geld),
  %wc(Cat,_Kop,EDG,_Kort,VoorWie1,_,_),
  mvg(Voorwie1, MVG1),
  wc(CatS, _, _, Naam, EDG, _, VoorWie2, _, _, _, _, _, _, _, _),
  %wc(CatS,Naam,EDG,_,Voorwie2,_,_),
  mvg(Voorwie2, MVG2),
  MVG1 = MVG2,
  eigenvoorrondeOfVrij(CatS, Cat),
  getbacktrack(Here),
  opg(_,CatS,_,_,_,_,_,_,_,_,_),	% een voorronde moet wel een opgave hebben
  cutbacktrack(Here),
  cat1(CatS, Prefix),
  format(CatNaam, "%s\t%s", Prefix, Naam).

eigenvoorrondeOfVrij(CatS, Cat) :-
  koplv(CatS, Cat), !.
eigenvoorrondeOfVrij(CatS, _Cat) :-
  not(isOndercat(CatS)).

algeselecteerd(CatE, SelNaam) :-
  wc(Cat, _, _, Naam, _, _, _, _, _, _, _, _, _, _, _),
  %wc(Cat, Naam,_,_,_,_,_),
  koplv(Cat, CatE),
  cat1(Cat, Prefix),
  format(SelNaam, "%s\t%s", Prefix, Naam).

preselectV([H|T], PNamen, N, [N|Uit]) :-
  member(P, PNamen),
  P = H, !,
  N1 = N + 1,
  preselectV(T, PNamen, N1, Uit). 
preselectV([_|T], PNamen, N, Uit) :-
  N1 = N + 1, !,
  preselectV(T, PNamen, N1, Uit). 
preselectV(_, _PNamen, _, []) :- !.

cat1(Cat, " ") :- 
  pos(Cat, _, _, afval), !.
cat1(Cat, "p") :-
  pos(Cat, _, _, poel), !.
cat1(_Cat, "-"). 

verwijderOnderdeelNiet(Cat) :-
  opg(_OpgaveNum,Cat,_,_,_,_,_,_,_,_,_), !.	% er zijn opgaven, dus niet
verwijderOnderdeelNiet(Cat) :-			% er is een afhankelijk onderdeel
  koplv(_, Cat), !.	% inclusief verliezersronde 
verwijderOnderdeelNiet(Cat) :-
  wc(Cat, _, _, _, _, _, _, _, _, _, _, _, _, KNLTB, _),
  fronttoken(KNLTB, _, _),
  _Answer = dlg_MessageBox( "Wissen", "U wist een met de KNLTB gesynchroniseerd onderdeel.\nDoe een opgavecheck!", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), !.
verwijderOnderdeelNiet(Cat) :-
  wc(Cat, OnderdeeelID, N, Naam, EDG, Kort, SexL, SpStrk, LftV, LftTM, HDG, Schemasoort, Keuring, KNLTB,Geld),
  %wc(Cat,OndNaam,EDG,KorteNaam,VoorWie,Prijs,Stamp),
  logf('R',wc(Cat, OnderdeeelID, N, Naam, EDG, Kort, SexL, SpStrk, LftV, LftTM, HDG, Schemasoort, Keuring, KNLTB,Geld),nee), 	% mag verwijderd worden
  fail.

nieuweSelect(_Win, Cat, Cat) :-
  wc(Cat, _, _, _, _, _, _, _, _, _, _, _, _, _, _), !.
nieuweSelect(Win, Cat, NCat) :-
  vink(Cat, Nr),
  cTreeView::get_next(Win, Nr, NrN),
  NrN <> 0,
  vink(NCat, NrN), !.
nieuweSelect(Win, Cat, NCat) :-
  vink(Cat, Nr),
  cTreeView::get_previous(Win, Nr, NrPrev),
  NrPrev <> 0,
  vink(NCat, NrPrev), !.
nieuweSelect(Win, Cat, NCat) :-
  vink(Cat, Nr),
  cTreeView::get_parent(Win, Nr, NrN),
  NrN <> 0,
  vink(NCat, NrN), !.
nieuweSelect(_Win, _Cat, 0).

checkGereserveerd(Win, _Cat, afval, Aantal1, Gereserveerd) :-
  Lim = Aantal1 + Gereserveerd - 2,
  Lim < 0,
  XX = - Lim,
  format(Msg, "Indelen kan wel maar dan moet u minstens %u gereserveerde plaatsen invoeren.", XX), 
  dlg_MessageBox( "Indelen", Msg, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  RWin = win_GetCtlHandle(Win, idcd_pl),
  win_SetFocus(RWin),
  !.

firstSelect(0).

teVerbinden(Cat1, OnderdeelPlusTid) :-
  wc(Cat1, _OnderdeeelID,_Orde,  _Naam, WTyp1, _Kort, SexList1, SpSt1, leeftijd(_,Londer1), leeftijd(_,Lboven1), _HDG, _Schemasoort, _Keuring, _KNLTB, _Geld),
  %wc(Cat1,_Onderdeel1,WTyp,_,Sexlist1,_,_), 
  %wcritp(Cat1, SpSt1, Londer1, Lboven1, _),
  wc(Cat2, _,_, Onderdeel2, WTyp2, _, SexList2, SpStX, leeftijd(_,LonderX), leeftijd(_,LbovenX), _, _, _, Tid, _),
  %wc(Cat2,Onderdeel2,WTyp,_,Sexlist2,_,_), 
  edgmatch(Wtyp1, WTyp2),
  Cat1 <> Cat2,
  %wcritP(Cat2, SpStX, LonderX, LbovenX, _),
  SpSt1 = SpStX,
  Londer1 = LonderX,
  matchbovenleeftijd(Lboven1,LbovenX),
  compsexes(SexList1, SexList2),
  format(OnderdeelPlusTid, "%s\t  %s", Onderdeel2, Tid).
  %pos(Cat2, _, _, _),
  %not(isOndercat(Cat2)).

pouleOpenX(_Cat, Ter, _) :-
  Ter < 1, !.
pouleOpenX(Cat, Tereserveren, poel) :-
  getbacktrack(Here),
  date(Year,Month,Day),
  time(Hours,Minutes,_Seconds,_Hundredths),
  dt_min_to_offset(Year, Month, Day, Hours, Minutes, Nr),
  numb(N),     
  Opg = Nr + N - 1,
  not(opg(Opg,Cat,_,_,_,_,_,_,_,_,_)),
  cutbacktrack(Here),
  numb(X),
  not(opg(_,Cat,openplaats(X),_,_,_,_,_,_,_,_)),
  logf('Z',opg(Opg,Cat,openplaats(X),0,0,0,0,"","",na,""),nee),
  Ter1 = Tereserveren - 1, !,
  pouleOpenX(Cat, Ter1, poel).
pouleOpenX(_Cat, _Ter, _).

xml_geboortedatum(UitgebreideGeboorteDatum, ja, EenTwee) :-
  fronttoken(UitgebreideGeboorteDatum, DagS, R1),
  fronttoken(R1, _, R2),
  fronttoken(R2, MaandS, R3),
  fronttoken(R3, _, R4),
  fronttoken(R4, JaarS, _),
  format(AttrDag, "geboortedag%u", EenTwee),
  xml_gegeven(AttrDag, DagS),
  format(AttrMaand, "geboortemaand%u", EenTwee),
  xml_gegeven(AttrMaand, MaandS),
  format(AttrJaar, "geboortejaar%u", EenTwee),
  xml_gegeven(AttrJaar, JaarS), !.
xml_geboortedatum(GeboorteDatum, nee, EenTwee) :-
  format(Attr, "geboortedatum%u", EenTwee),
  xml_gegeven(Attr, GeboorteDatum), !. 
xml_geboortedatum(_, _, _).				% doe niets


/*
lidnrVerplicht(X, X) :-
  fronttoken(X, _, _), !.
lidnrVerplicht(_, "00000000") :- !.
*/
tgnst_id(s(Id), IDS) :- !,
  str_int(IDS, Id).
tgnst_id(pw(Id), IDS) :- !, %!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  format(IDS, "%uy", Id).	% 27.12.2005
tgnst_id(p(Id1,Id2), IDS) :- !,
  format(IDS, "%ux%u", Id1, Id2).

bestaandOfNieuw(_, Cat1, Catx) :-
  findall(Cat2, teVerbinden(Cat1, Cat2), CatL),
  not(CatL = []),
  X = dlg_MessageBox( "Verliezersronde", "Bestaand (zwevend) onderdeel als verliezersronde koppelen?", mesbox_iconquestion, mesbox_buttonsyesnocancel, mesbox_defaultthird, mesbox_suspendapplication ),
  X <> 2,
  !,
  X <> 3,
  b_True = dlg_ListSelect("Verliezersronde:",CatL,0,CatSel,_Index),
  parseTabs(CatSel, Items),
  Items = [CatSel1|_],
  %wc(Catx,CatSel,_,_,_,_,_),
  wc(Catx, _, _, CatSel1, _, _, _, _, _, _, _, _, _, _, _), 
  logf('Z', kopl(Catx, verliezer, Cat1, algemeen), nee), !.
bestaandOfNieuw(_Win, Cat, N) :-
  getbacktrack(Here),
  wc(Cat, OnderdeelID, Nr, Kop, EDG, Kort, SexL, SpStrk, LftV, LftTM, HDG, Schemasoort, Status, KNLTB,Geld),
  %wc(Cat,Kop,EDG,Kort,VoorWie,Geld,_),	% verliezersronde niet gedefiniëerd
  cutbacktrack(Here),
  format(TroostNaam, "%s troostronde", Kop), 
  concat(Kort, "      ", Kort1),
  frontstr(5,Kort1,Kort2,_),
  format(TroostKort, "%sT", Kort2),
  assert(nieuweronde(TroostNaam, TroostKort,[])),
  dlg_Troostronde_create(_Win, "Naamgeving troostronde", Return),
  Return = b_True,
  getbacktrack(Here1),
  numb(N),
  not(wc(N, _, _, _, _, _, _, _, _, _, _, _, _, _, _)),
  not(kopl(N,_,_,_)),
  not(kopl(_,_,N,_)), 
  cutbacktrack(Here1),
  nieuweronde(KopN, KortN, _),
  assert(nieuweronde("", "", [])),
  findall(Cat1, wc(Cat1, _, _, _, _, _, _, _, _, _, _, _, _, _, _), Cat1L),	% zelfde pos in wclijst !
  %findall(Cat1, wc(Cat1,_,_,_,_,_,_), Cat1L),	% zelfde pos in wclijst !
  positionWC(Cat, Cat1L, Cat1L1),
  evenWegWC(Cat1L1, Cat1L1Vol),			% haal achterste stuk weg
  logf('Z',kopl(N,verliezer,Cat,algemeen),ja),
  %logf('Z',wc(Cat,Kop,EDG,Kort,VoorWie,Geld, N), ja),
  logf('Z',wc(Cat, OnderdeelID, Nr, Kop, EDG, Kort, SexL, SpStrk, LftV, LftTM, HDG, Schemasoort, Status, KNLTB,Geld),ja),
  logf('Z',wc(N, OnderdeelId, Nr, KopN, EDG, KortN, SexL, SpStrk, LftV, LftTM, HDG, Schemasoort, vanzelf, KNLTB,0.0),ja),
  Cat1L1Vol = [_|Cat1L1Volx1],
  sluitAanWC(Cat1L1Volx1),			% en voeg weer toe
  logclose(), !.					% nog nodig ???


volgendeTwee(Win, Cat) :-
  vink(Cat, CatNr),
  cTreeview::get_next(Win, CatNr, Next),
  Next <> 0,
  cTreeview::get_next(Win, Next, Next1),
  Next1 <> 0,
  cTreeview::select(Win, Next1), !.
volgendeTwee(Win, Cat) :-
  vink(Cat, CatNr),
  cTreeview::get_next(Win, CatNr, Next),
  Next <> 0,
  cTreeview::select(Win, Next), !.
volgendeTwee(_Win, _Cat).

xml_tag(Tag, Gegeven) :-
  xml_xlate(Gegeven, Gegeven1),
  writef("<ns:%s>%s</ns:%s>", Tag, Gegeven1, Tag), !.

xml_tag_opt(_Tag,"") :- !.
xml_tag_opt(_Tag,"99") :- !.  % niet relevant
xml_tag_opt(Tag,Gegeven) :-
  xml_xlate(Gegeven, Gegeven1),
  writef("<ns:%s>%s</ns:%s>\n", Tag, Gegeven1, Tag), !.

getAcceptatieresultaat(_Win, Tid) :-
  %write("\nTid: ", Tid),
  not(fronttoken(Tid,_,_)), !.
getAcceptatieresultaat(_Win, Tid) :-
  fronttoken(Tid,_,_),
  not(toernooiK(Tid, nrt)), 
  not(toernooiK(Tid, jrt)),
  Msg = "Het acceptatieresultaat speelt alleen een rol bij een ranglijst toernooi!\nToch proberen op te halen?",
  2 = dlg_MessageBox( "Acceptatieresultaat", Msg, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
  !.
getAcceptatieresultaat(_Win,Tid) :-
  checkkwalificatie(Tid, ja), 
  providerX(Prov,_,"",_),
  format(URL,"http://%s/knltbproxy2015/AcceptatieResultaatService.php",Prov), 
  get_TempDir(_, TempFile),
  %write("\n",Tempfile,"\n"),
  deletefile(TempFile),
  AanmeldenVars = aanmeldenvars(),
  _Return = downloadData(URL, TempFile, ["tid",Tid | AanmeldenVars], _HttpResponsecode, 0),
  %file_str(TempFile, Str),
  %writedevice(XX),
  %writedevice(screen),
  %write("\n", Str),
  %checkbeschikbaar(Str),
  %writedevice(XX),
  openread(work, TempFile),
  readdevice(work),
  repeat_f(work),
  cursor_SetWait(),
  readln(Line),
  utf_8_to_ansi(Line, Line1),
  trap(term_str(dbasedom, Term, Line1),Error,misser(Line, Error)),
  loadlineA(Term, Tid),
  fail.
getAcceptatieresultaat(_,Tid) :-
  loadlineAcleanup(),
  logclose(),
  dodeelinrefresh(),
  closefile(work),
  findall(I,tid_report_cat(I, Tid),IList),
  ratingrapport(IList,"Rapport aceptatieresultaat","").

dependent(Ond, Ond, _).
dependent(Ond, Uit, Tid) :-
  kopl(Uit,_,Ond,_),
  not(basicCats(Uit, Tid)).

basicCats(Cat, Tid) :-
  wc(Cat, _, _, _, _, _, _, _, _, _, _, _, _, Tid, _),
  not(isondercat(Cat)).
basicCats(Cat, Tid) :-
  wc(Cat, _, _, _, _, _, _, _, _, _, _, _SS, _, Tid, _),
  %kopl(Cat, _, CatH, _),
  %wc(CatH, _, _, _, _, _, _, _, _, _, _, "0", _, Tid, _),
  isMasterCat(Cat).
  %isondercat(Cat),
  %SS >= "A",
  %SS <= "Z".

dependOpgExists(Ond, Tid) :-
  dependent(Ond, Ondxx, Tid),
  opg(_TN1,Ondxx,_Tgst,_Plx,_Rondex,_Uitslx,_Nrx,_LLWCx,_TeamIdx,_TeamStatusx,_MTx), !.

eriseenopgaveomtechecken(Tid) :-
  basicCats(Catxy1,Tid),
  dependOpgExists(Catxy1, Tid), !.

checkStart(Tid) :-
  not(eriseenopgaveomtechecken(Tid)), !.
checkStart(Tid) :-
  api_getComputername(PCname),
  inschrijfcheck(PCname, Tid,TijdstipVorige, _Nr),  
  date(Year,Month,Day),
  time(Hours,Minutes,_Seconds,_Hundredths),
  dt_min_to_offset(Year, Month, Day, Hours, Minutes, Tijdstip),
  Minuten = Tijdstip - TijdstipVorige,
  format(Msg, "Er is al %u minuten een inschrijfcontrole bezig.\nWil je nu al een nieuwe starten?\nNB De vorige wordt normaal gesproken eerst afgemaakt.", Minuten),
  2 = dlg_MessageBox( "Inschrijfcontrole", Msg, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
  !.
checkStart(Tid) :-
  retractall(geenbondsnummer(_)),
  getFolder(TDir,_,_, _),
  %naam(_Naam, Nrsxx, _NrUit, _WT, _, _),
  %NrsXX = [NrXX|_],
  format(NaamXX, "Check_%s.xml", Tid),
  filenamepath(XmlName, TDir, NaamXX),
  %write("\ntasverh.pro ",XmlName),
  openwrite(work, XmlName),	% xml-bestand
  writedevice(work), 
  %findall(CatF,wc(CatF, _, _, _, _, _, _, _, _, _, _, _, _, Tid, _),CatLF), 
  %loadCats(CatLF, _Onderdelen, UpperCats),
  write("<ns:Toernooi>"),
  xml_tag("Toernooinummer", Tid),
  looptag("Toernooionderdelen"),
  basicCats(Ond, Tid),
    dependOpgExists(Ond, Tid),
    wc(Ond, OnderdeelId, _Nr, OnderdeelNaam, WTyp, _Kort, _SexList, SpStrk, leeftijd(LftVa,_), leeftijd(LftTM,_), HDG, _Schemasoort, _Status, _KNLTB, _),
    looptag("Toernooionderdeel"),
    retractall(algeweest(_)),
    xml_spelsoort(WTyp, SpelSoort,_),
    wedsex2geslacht(HDG, Geslacht),
    xml_tag("Geslacht", Geslacht),
    xml_tag_opt("LeeftijdTotEnMet",LftTM),
    xml_tag_opt("LeeftijdVanaf",LftVa),
    xml_xlate(Onderdeelnaam, OnderdeelnaamX),
    xml_tag("Naam", OnderdeelNaamX),
    xml_tag("OnderdeelId", OnderdeelId),
    xml_tag_opt("Speelsterkte",SpStrk),
    xml_tag("Spelsoort",Spelsoort),
    xml_tag("Status", "G"),
    looptag("ToernooiInschrijfcontroleTeams"),
    dependent(Ond, Ondx, Tid),
      opg(TN1,Ondx,Tgst,_Plx,_Rondex,_Uitslx,_Nrx,_LLWCx,_TeamIdx,_TeamStatusx,_MTx),
        not(algeweest(Tgst)),
        geldigTeam(Tgst),
        format(TeamId, "x%ux%u", Ondx, TN1),
        write("<ns:Team>"),
        tgnst2deelnemer(Tgst),
        xml_tag("Status","N"),
        xml_tag("TeamId",TeamId),
        write("</ns:Team>"), 
      fail.
checkStart(Tid) :-
  vereniging1(VNr, _WL, _WlTel, _Club, _Stad, _WlEmail,_BGNaam,_BGTel,_BGEmail,_Website),
  xml_tag("Verenigingsnummer",VNr),
  write("</ns:Toernooi>"),
  closefile(work),
  %naam(_Naam, Nrsxx, _NrUit, _WT, _, _),
  %NrsXX = [NrXX|_],
  getFolder(TDir,_,_, _),
  format(NaamXX, "Check_%s.xml", Tid),
  filenamepath(XmlName, TDir, NaamXX),
  write("\nXMLname nog een keer: ",XmlName),
  retractall(algeweest(_)),  % !
  providerX(Prov,_,"",_),
  format(URL,"http://%s/knltbproxy2015/InschrijfcontroleServiceNN.php",Prov),  % stuur op!!!
  get_TempDir(_, TempFile),
  deletefile(TempFile),
  write("\n",TempFile),
  file_str(XmlName,XML4),
  AanmeldenVars = aanmeldenvars(),
  _Return = downloadData(URL, TempFile, ["Inschrijvingen", XML4, "tid",Tid | AanmeldenVars], _HttpResponsecode, 0),
  file_str(TempFile, Str),
  write(Str),  
  loadlineAcleanup(),
  openread(work, TempFile),
  readdevice(work),
  repeat_f(work),
  readln(Line),
  utf_8_to_ansi(Line, Line1),
  trap(term_str(dbasedom, Term, Line1),Error,misser(Line, Error)),
  loadlineA(Term, Tid),   % checkstartid wordt gezet
  fail. 
checkStart(_) :-
  loadlineAcleanup(),
  logclose(),
  closefile(work),
  %backup("email", ja),    %silent back-up
  dodeelinrefresh(),
  fail.
checkStart(_) :-
  findall(Naam, geenbondsnummer(Naam), Namen),
  not(Namen = []),
  list_to_string(Namen, "\n", String),
  format(Msg, "De inschrijfcontrole is gestart, dat duurt 10 - 20 minuten.\nWanneer gereed verschijnt het controleresultaat in uw browser.\nEr zijn spelers zonder geldig bondsnummer:\n\n%s\n\nGaarne intussen verbeteren!",String),
  _Answer = dlg_MessageBox( "Inschrijfcontrole", Msg, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  retractall(geenbondsnummer(_)), !.
checkStart(_) :-
  Str = "Als de controle is afgelopen vind je een rapport in de browser.\nKlik nu op OK.",
  format(Msg, "de inschrijfcontrole is gestart.\nDat kan ongeveer 20 minuten duren.\nNB de controle geldt voor alle onderdelen van dit toernooinummer tegelijk!\n%s", Str),
  _Answer = dlg_MessageBox( "Inschrijfcontrole", Msg, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ).
 

wedsex2geslacht(heren, "M").
wedsex2geslacht(dames, "V").
wedsex2geslacht(gemengd, "G").
wedsex2geslacht(hdnvt, "N").

tgnst2deelnemer(Tgnst) :-
  algeweest(Tgnst), !.
tgnst2deelnemer(Tgnst) :-
  assert(algeweest(Tgnst)),
  fail.
tgnst2deelnemer(p(T1,T2)) :-
  write("<ns:Deelnemer1>"),
  sp2deelnemer(T1),
  write("</ns:Deelnemer1>\n"),
  write("<ns:Deelnemer2>"),
  sp2deelnemer(T2),!,
  write("</ns:Deelnemer2>\n"), !.
tgnst2deelnemer(s(T1)) :-
  write("<ns:Deelnemer1>"),
  sp2deelnemer(T1), !,
  write("\n</ns:Deelnemer1>\n").
tgnst2deelnemer(_).

geldigbondsnummer(In, In) :-
  proef11det(In), !.
geldigbondsnummer(_In, "00000000").

geldigTeam(Tgnst) :-
  tgnstnum(Tgnst, SpNo),
  sp2(SpNo,_,Naam,_,_,_,_,_,_, _, _, _, BondsNr, _, _, _, _, _, _, _,_,_,_,_,_,_,_),
  not(proef11det(BondsNr)),
  not(geenbondsnummer(Naam)), 
  assert(geenbondsnummer(Naam)),
  fail.
geldigTeam(s(SpNo)) :-
  sp2(SpNo,_,_,_Telthuis,_Verh,_RatingE,_RatingD,_Betaald,_UitKant, _Club, _Adres, _Woonpl, BondsNr, _ES, _DS, _Gebdatum, _Telwerk, _Telmobiel, _EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_IncassoAkkoord,_Res),
  proef11det(BondsNr), !.
geldigTeam(p(SpNo1,SpNo2)) :-
  sp2(SpNo1,_,_,_,_,_,_,_,_, _, _, _, BondsNr1, _, _, _, _, _, _, _,_,_,_,_,_,_,_),
  proef11det(BondsNr1),
  sp2(SpNo2,_,_,_,_,_,_,_,_, _, _, _, BondsNr2, _, _, _, _, _, _, _,_,_,_,_,_,_,_),
  proef11det(BondsNr2), !.

soort2geslacht(Soort, "M") :-
  spsrt(Soort,_,m,_,_,_,_,_), !. 
soort2geslacht(_Soort, "V").

sp2deelnemer(SpNo) :-
  comline(LineBuffer),
  upper_lower(LineBuffer, LBL),
  searchstring(LBL, "/ta", _),
  sp2(SpNo,_Soort,Naam1,_Telthuis,_Verh,_RatingE,_RatingD,_Betaald,_UitKant, _Club, _Adres, _Woonpl, BondsNr, _ES, _DS, _Gebdatum, _Telwerk, _Telmobiel, _EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_IncassoAkkoord,_Res),
  geldigbondsnummer(Bondsnr, Bondsnr1),
  xml_naamscan(Naam1, _Voornaam1X, _Voorletter1, Achternaam1),
  %xml_voorvoegselA(Voornaam1X, Voornaam1, Tussenvoegsel),
  xml_tag("Achternaam", Achternaam1),
  xml_tag("Bondsnummer", BondsNr1),
  format(DeelnemerId, "x%u", SpNo),
  xml_tag("DeelnemerId", DeelnemerId), !. 
sp2deelnemer(SpNo) :-
  sp2(SpNo,Soort,Naam1,_Telthuis,_Verh,_RatingE,_RatingD,_Betaald,_UitKant, _Club, _Adres, _Woonpl, BondsNr, _ES, _DS, _Gebdatum, _Telwerk, _Telmobiel, _EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_IncassoAkkoord,_Res),
  geldigbondsnummer(Bondsnr, Bondsnr1),
  soort2Geslacht(Soort, MV),
  xml_naamscan(Naam1, Voornaam1X, Voorletter1, Achternaam1),
  xml_voorvoegselA(Voornaam1X, Voornaam1, Tussenvoegsel),
  xml_tag("Achternaam", Achternaam1),
  xml_tag("Bondsnummer", BondsNr1),
  format(DeelnemerId, "x%u", SpNo),
  xml_tag("DeelnemerId", DeelnemerId),
  xml_tag("Voorletters", Voorletter1),
  xml_tag("Tussenvoegsel", Tussenvoegsel),
  xml_tag("Voornaam", Voornaam1),
  xml_tag("Geslacht", MV), !.

looptag(Tag) :-
  writef("<ns:%s>\n",Tag).   % openen
looptag(Tag) :-
  writef("\n</ns:%s>",Tag),  % afsluiten
  fail.    

xml_minVoornaamA("", "?") :- !.
xml_minVoornaamA(In, In).
 
xml_voorvoegselA(Voornaam, Rest1, VoorvoegselX) :-
  xml_voorvoegsel1A(Voornaam, Rest, "", Voorvoegsel),
  trim_c(Voorvoegsel),
  xml_xlate(Voorvoegsel, VoorvoegselX),
  trim_c(Rest),
  xml_minVoornaamA(Rest, Rest1), !.
  
xml_voorvoegsel1A(Voornaam, Rest1, In, VoorvoegselU) :-
  trim_c(Voornaam),
  str_len(VoorNaam, Len),
  reversestr(VoorNaam, "", NaamRev),
  searchchar(NaamRev,' ',Pos),
  P1 = Len - Pos + 1,
  frontstr(P1,VoorNaam,Rest,Voorvoegsel), 
  upper_lower(Voorvoegsel, VoorvoegselL),
  Voorvoegsel = VoorvoegselL, !,
  format(In1, "%s %s", Voorvoegsel, In),
  xml_voorvoegsel1A(Rest, Rest1, In1, VoorvoegselU).
xml_voorvoegsel1A(VoorNaam, VoorNaam, X, X).

responseOK(Filenaam, TestString,FoundPos) :-
  file_str(FileNaam, Inhoud),
  upper_lower(Inhoud, InhoudL),
  upper_lower(TestString, TestStringL),
  searchstring(InhoudL,TeststringL,FoundPos), !.
  %write("\n", Inhoud), !.
responseOK(_, _, 0).

visPostVarsUit(FileNaam, HiddenPostVars) :-
  file_str(FileNaam, Inhoud),
  getHiddenPV(Inhoud, [], HiddenPostVars).

getHiddenPV(Inhoud, In, [PstVarNaam,PstVarInh|Uit]) :-	% gepaard
  upper_lower(Inhoud, InhoudL),
  searchstring(InhoudL,"hidden",Pos),
  frontstr(Pos,Inhoud,_,R1),
  searchstring(R1,"=",Pos1),
  frontstr(Pos1,R1,_,R2),
  fronttoken(R2,PstVarNaam,R3),
  fronttoken(R3,_PstVarNaam,R4),
  fronttoken(R4,_,R5),
  fronttoken(R5,_,R6),
  searchchar(R6,'"',P6),
  L1 = P6 - 1,
  frontstr(L1,R6,PstVarInh,RestString),
  fronttoken(Reststring, _, Rest),
  getHiddenPV(Rest, In, Uit), !.
getHiddenPV(_Inhoud, In, In).

verwerkcheck(_, _, _, _, _) :-
	dlg_Note("verwerkcheck","verwijderd...").

movekoppeling(Van, Naar) :-
  findall(ToT,kopl(Van, _, ToT, _),ToTList),
  member(CatX, ToTList),
  kopl(Van, TypeV, CatX, TypeN),
  logf('R',kopl(Van, TypeV, Catx, TypeN),ja),
  logf('Z',kopl(Naar, TypeV, CatX, TypeN),ja),
  fail.
movekoppeling(_, _).

plaats34(_Win,Cat, extrafinale) :-
  LboxWin = win_GetCtlHandle(_Win, idcd_lbond),
  kopl(Cat, rang(1), CatFinale, _),	% eerste/tweede plaats
  vink(CatFinale, CatFinaleNr),
  cTreeView::select(LboxWin, CatFinaleNr),
  write("#### %!"),
  !.
plaats34(_Win,Cat, extrafinale) :-			% eerste/tweede plaats
  write("#### %%"),
  deelinWin(_Win,TidL),
  LboxWin = win_GetCtlHandle(_Win, idcd_lbond),
  getbacktrack(Here),
  wc(Cat, OnderdeelId, Nr, Kop, EDG, Kort, VoorWie, SpStrk, LftV, LftTM, HDG, Schemasoort, Keuring, KNLTB,Geld),
  cutbacktrack(Here),
  format(NaamF, "Finale %s", Kop), 
  %concat(Kort, "      ", Kort1),
  %frontstr(5,Kort1,Kort2,_),
  format(KortF, "%sf", Kort), trim_c(KortF),
  assert(nieuweronde(NaamF, KortF, [])),
  dlg_Troostronde_create(_Win, "Naamgeving extra finale", Return),
  Return = b_True,
  getbacktrack(Here1),
  numb(FCat),
  not(wc(FCat, _, _, _, _, _, _, _, _, _, _, _, _, _, _)),
  not(kopl(FCat,_,_,_)),
  not(kopl(_,_,FCat,_)), 
  cutbacktrack(Here1),
  nieuweronde(KopN, KortN, _),
  assert(nieuweronde("", "", [])),
  findall(Cat1, wc(Cat1, _, _, _, _, _, _, _, _, _, _, _, _, _, _), Cat1L),	% zelfde pos in wclijst !
  positionWC(Cat, Cat1L, Cat1L1),
  evenWegWC(Cat1L1, Cat1L1Vol),			% haal achterste stuk weg
  movekoppeling(Cat, FCat),
  logf('T', wd(1,FCat,onbekend,onbekend,"",0,""), ja), 
  Nr1 = Nr + 1,
  logf('Z',  wc(FCat, OnderdeelId, Nr1, KopN, EDG, KortN, VoorWie, SpStrk, LftV, LftTM, HDG, Schemasoort, Keuring, KNLTB,Geld),ja),
  logf('Z',wd(1,FCat,onbekend,onbekend,"",0,""),ja),
  logf('Z', pos(FCat, 1, 3, afval),ja),
  logf('Z',kopl(Cat, rang(1), FCat, plaats(2)),ja),
  logf('Z',kopl(Cat, rang(2), FCat, plaats(3)),ja),
  %logf('Z',wc(Cat, "", Nr, Kop, EDG, Kort, VoorWie, SpStrk, LftV, LftTM, HDG, "", vanzelf, "",Geld),ja),
  sluitAanWC(Cat1L1Vol), 			% en voeg weer toe
  logf('Z',wc(Cat, "", Nr, Kop, EDG, Kort, VoorWie, SpStrk, LftV, LftTM, HDG, "", vanzelf, "",Geld),ja),	% 30.5.2015 gemoved!
  logclose(), 
  vulMetKnopen(LboxWin,TidL),
  vink(Cat, CatNr),
  cTreeView::select(LboxWin, CatNr),
  win_SetFocus(LboxWin), 
  voorrondes(FCat, MetVoorrondes),
  gegevens(_Win, FCat, _AantalSel, MetVoorrondes), !.
plaats34(_Win,Cat, drievierplaats) :-
  LboxWin = win_GetCtlHandle(_Win, idcd_lbond),
  kopl(Cat, pl3en4(_), Cat34, _),	% derde/vierde plaats
  vink(Cat34, Cat34Nr),
  cTreeView::select(LboxWin, Cat34Nr),
  !.
plaats34(_Win,Cat, drievierplaats) :-
  LboxWin = win_GetCtlHandle(_Win, idcd_lbond),
  getbacktrack(Here),
  wc(Cat, _OnderdeelId, _Nr, Kop, EDG, Kort, VoorWie, SpeelSterkte, LeeftV, LeeftTM, HDG, Schemasoort, Status, KNLTB,Geld),
  %wc(Cat,Kop,EDG,Kort,VoorWie,_Geld,_Verl),
  cutbacktrack(Here),
  %wcritP(Cat, Speelsterkte, LeeftV, LeeftTM, Res),
  format(Naam34, "%s 3/4 plaats", Kop), 
  concat(Kort, "      ", Kort1),
  frontstr(5,Kort1,Kort2,_),
  format(Kort34, "%s3", Kort2), trim_c(Kort34),
  assert(nieuweronde(Naam34, Kort34, [])),
  dlg_Troostronde_create(_Win, "Naamgeving 3/4", Return),
  Return = b_True,
  getbacktrack(Here1),
  numb(N),
  not(wc(N, _, _, _, _, _, _, _, _, _, _, _, _, _, _)),
  not(kopl(N,_,_,_)),
  not(kopl(_,_,N,_)), 
  cutbacktrack(Here1),
  nieuweronde(KopN, KortN, _),
  assert(nieuweronde("", "", [])),
  format(ReprN, "%s * %s", KortN, KopN),
  assert(vink(N, N)),
  assert(refresh()),
  cTreeView::insert_item(LboxWin, N, Cat, first(), ReprN, [], resId(idb_plaats_34), resId(idb_plaats_34)),
  retractall(refresh()),
  cTreeView::select(LboxWin, N),
  findall(Cat1,wc(Cat1, _, _, _, _, _, _, _, _, _, _, _, _, _, _), Cat1L),	% zelfde pos in wclijst !
  positionWC(Cat, Cat1L, Cat1L1),
  evenWegWC(Cat1L1, Cat1L1Vol),			% haal achterste stuk weg
  Cat1L1Vol = [CatVx|Cat1L1Volx],
  sluitAanWC([CatVx]),				% een te veel
  logf('T', wd(1,N,onbekend,onbekend,"",0,""), ja), 
  retractall(kopl(Cat, pl3en4(_), _, _)),
  logf('Z',wc(N, _OnderdeelId, _Nr, KopN, EDG, KortN, VoorWie, SpeelSterkte, LeeftV, LeeftTM, HDG, Schemasoort, Status, KNLTB,Geld),ja),
  %logf('Z',wcrit(N, Speelsterkte, LeeftV, LeeftTM, Res),ja),
  logf('Z',wd(1,N,onbekend,onbekend,"",0,""),ja),
  logf('Z', pos(N, 1, 3, afval),ja),
  logf('Z',kopl(Cat, pl3en4(3), N, plaats(2)),ja),
  logf('Z',kopl(Cat, pl3en4(4), N, plaats(3)),ja),
  sluitAanWC(Cat1L1Volx),			% en voeg weer toe
  logclose(),
  win_SetFocus(LboxWin),
  voorrondes(N, MetVoorrondes),
  gegevens(_Win, N, _AantalSel, MetVoorrondes), !.
plaats34(_Win, _, _) :-
  LboxWin = win_GetCtlHandle(_Win, idcd_lbond),
  win_SetFocus(LboxWin), !.

vulMetKnopen(LboxWin, TidL):-
  cleanRelaties(),
  assert(refresh()),
  deleteKnopen(LboxWin),
  findall(Knoopx, knoop(_, Knoopx, 1, TidL), Knopen),
  cTreeView::insert_tree_list(LboxWin, 0, first(), Knopen),
  retractall(refresh()), !.

openPlaats(Cat, Tekst) :-
  opg(_H,Cat,pw(Sp),_Plts,_Byes,_,_,_,_,_,_),
  sp2(Sp,_Soort,N1,_Telthuis,_Verh,_RatingE,_RatingD,_Betaald,_UitKant, _Club, _Adres, _Woonpl, _BondsNr, _ES, _DS, _Gebdatum, _Telwerk, _Telmobiel, _EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_IncassoAkkoord,_Res),
  format(Tekst, " %s zoekt nog partner.", N1).
openPlaats(Cat, " nog open plaats(en).") :-
  pos(Cat, Card, _, afval),
  wd(N,Cat,_,onbekend,_,_,_),
  N >= Card,
  not(kopl(_, _, Cat, _)), !.
openPlaats(Cat, " nog open plaats(en).") :-
  pos(Cat, Card, _, afval),
  wd(N,Cat,onbekend,_,_,_,_),
  N >= Card,
  not(kopl(_, _, Cat, _)), !.
openPlaats(Cat, " nog open plaats(en).") :-
  pos(Cat, Card, _, poel),
  wd(N,Cat,openplaats(_),_,_,_,_),
  N >= Card,
  not(kopl(_, _, Cat, _)), !.
openPlaats(Cat, " nog open plaats(en).") :-
  pos(Cat, Card, _, poel),
  wd(N,Cat,_, openplaats(_),_,_,_),
  N >= Card,
  not(kopl(_, _, Cat, _)), !.
  
openplaatsenRap() :-
  get_TempDir(_, TempFile),
  openwrite(work,TempFile),
  writedevice(work),
  write("Open plaatsen en partner gezocht\n"),
  %wc(Cat,KopN,_EDG,_KortN,_VoorWie,_,_),
  wc(Cat, _, _, KopN, _, _, _, _, _, _, _, _, _, _,_),
  getbacktrack(Here),
  openPlaats(Cat, _),
  cutbacktrack(Here),
  write("\n",KopN, ":"),
  openPlaats(Cat, Tekst),
  write("\n",Tekst),
  fail.
openplaatsenRap() :-
  not(openPlaats(_, _)),
  write("\nIn geen enkel onderdeel open plaatsen gevonden!"),
  closefile(work),
  !.
openplaatsenRap() :-
  write("\nVervang de open plaatsen door een speler/koppel,\ndoor een bye (afval) of verwijder ze (poule)."),
  closefile(work).
  
vorigeW(Cat, CatV) :-
  kopl(CatV, _, Cat, _),
  not(vorigeC(CatV)),
  assert(vorigeC(CatV)).
vorigeW(_, _) :-
  retractall(vorigeC(_)),
  fail.

pouleCats(_A, _CatVrij, []).

maakPoules(TopCat, A, PouleCats) :-
  wc(TopCat, OnderdeelId, Nr, Eindrondenaam, Wedstyp, Short, SexList, SpeelSterkte, LeeftV, LeeftTM, HDG, Schemasoort, Keuring, KNLTB,Geld),
  %wc(TopCat, EindrondeNaam, WedsTyp, Short, SexList, Geld, Verl),
  %wcrit(TopCat, SpSt, LftV, LftT, X),
  vrijeCatNummers(A, TopCat, CatVrij),
  pouleCats(A, CatVrij, PouleCats),  
  member(Cat, PouleCats),
  format(WCNaam, "#%u : %s", Cat, EindrondeNaam),
  logf('Z', wc(Cat, OnderdeelId, Nr, WCNaam, Wedstyp, Short, SexList, SpeelSterkte, LeeftV, LeeftTM, HDG, Schemasoort, Keuring, KNLTB,Geld), ja),
  %logf('Z', wcrit(Cat, SpSt, LftV, LftT, X), ja),
  logf('Z', wd(4, Cat, openplaats(1), openplaats(3), "", 0, ""), ja),  
  logf('Z', wd(5, Cat, openplaats(2), openplaats(3), "", 0, ""), ja),  
  logf('Z', wd(6, Cat, openplaats(1), openplaats(2), "", 0, ""), ja),
  logf('Z', pos(Cat, 4, 4, poel),ja),
  logf('Z', kopl(Cat, algemeen, TopCat, algemeen), ja),
  fail.  
maakPoules(_TopCat, _A, []).

closeMeteenDeelIn() :-
  deelinWin(Win,_), 
  dlg_MessageBox( "On-line", "Het toernooibestand is door een andere gebruiker on-line veranderd!\nStart Indelen opnieuw!", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  %retractall(modified()),
  win_Destroy(Win), !.
closeMeteenDeelIn().

cleankoppelingen() :-
  kopl(Cat, Pos, CatU, PosU),
  not(wc(Cat, _, _, _, _, _, _, _, _, _, _, _, _, _,_)), 
  logf('R',kopl(Cat, Pos, CatU, PosU), ja),
  fail.
cleankoppelingen() :-
  logclose().

herhaalControle(Tid) :-  % alleen voor test
  toernooiK(Tid,TType),
  term_str(toernooisoort, TType, TypeStr),
  providerX(Prov,_,"",_),
  format(URL,"http://%s/knltbproxy2015/InschrijfcontroleServiceNN.php?herhaal=ja",Prov),  %?herhaal=ja",
  get_TempDir(_, TempFile),
  deletefile(TempFile),
  AanmeldenVars = aanmeldenvars(),
  _Return = downloadData(URL, TempFile, ["tid",Tid , "ttype", TypeStr | AanmeldenVars], _HttpResponsecode, 0),
  writedevice(WX),
  writedevice(screen),
  write("\n", Tempfile),
  writedevice(WX),
  retractall(club(_,_,_)),
  loadlineacleanup(),
  openread(work, TempFile),
  readdevice(work),
  repeat_f(work),
  readln(Line),
  fronttoken(Line,_,_),
  utf_8_to_ansi(Line, Line1),
  trap(term_str(dbasedom, Term, Line1),Error,misser(Line, Error)),
  loadlineA(Term, Tid),
  retractall(ischanged),
  assert(ischanged),
  fail.
herhaalcontrole(_) :-
  closefile(work),
  loadlineAcleanup(), 
  retract(ischanged()),
  doDeelinrefresh(), 
  fail.
herhaalcontrole(_) :-
  klad(2,TT,Tekst), !,
  logf('R',klad(2,TT,Tekst),nee),
  dlg_note("TestMessage",Tekst).
herhaalcontrole(_).

getControleresultaat() :-
  hartbeat(Bt),
  BtN = Bt + 1,
  assert(hartbeat(BtN)),
  %write("\nBtn.. ", BtN),
  BtN > 5,  % eens in de vijf keer
  %write("\nBingo! ", BtN),
  assert(hartbeat(0)),
  api_getComputername(PCname),
  inschrijfCheck(PCname,Tid,_Tx, _Cid),
  getControleResultaat1(Tid), !.
getControleresultaat() :-
  inschrijfCheck(_PCname,_,_, _), !,
  doDeelInRefresh().
getControleresultaat().

getControleresultaat1(Tid) :-  
  assert(headertekst("<p>Resultaat van de inschrijfcheck")),
  inschrijfCheck(PcName,Tid,Tx, Cid),
  toernooiK(Tid, TType),
  term_str(toernooisoort, TType, TypeStr),
  providerX(Prov,_,"",_),
  format(URL,"http://%s/knltbproxy2015/InschrijfcontroleServiceNN.php",Prov),  %?herhaal=ja",
  get_TempDir(_, TempFile),
  deletefile(TempFile),
  AanmeldenVars = aanmeldenvars(),
  _Return = downloadData(URL, TempFile, ["tid",Tid , "cid", Cid , "ttype", TypeStr | AanmeldenVars], _HttpResponsecode, 0),
  file_str(TempFile, Str),
  %write("\n", Str),
  not(searchstring(Str, "NotAvailable",_)),  % anders wachten
  logf('R',inschrijfCheck(PcName, Tid, Tx, Cid),nee),
  TaskWin = vpi_GetTaskWin(),
  toolbar_SetValue(TaskWin, idt_toolbarmsg, text_value("")),
  retractall(ischanged),
  assert(ischanged),
  not(alGespeeld(Str)),
  not(mededelingC(Str)),
  retractall(club(_,_,_)),
  loadlineAcleanup(),
  openread(work, TempFile),
  readdevice(work),
  repeat_f(work),
  readln(Line),
  fronttoken(Line,_,_),
  utf_8_to_ansi(Line, Line1),
  trap(term_str(dbasedom, Term, Line1),Error,misser(Line, Error)),
  loadlineA(Term, Tid),
  fail.
getControleresultaat1(_Tid) :-
  retract(ischanged()),
  closefile(work),
  findall(I,tid_report_cat(I, Tid),IList),
  headertekst(Header),
  ratingrapport(IList,"Rapport controleresultaat", Header),
  doDeelinrefresh(),
  loadlineAcleanup(), 
  fail.
getControleresultaat1(_Tid) :-
  setcheckwachten(),
  !.

tid_report_cat(CatS, Tid) :-
  wc(WC, _, _, Ond, _, _, _, _, _, _, _, _, _, Tid,_),
  %wc(WC, Ond, _, _Shortp,_,_,_), 
  not(isOnderCat(WC)),
  format(CatS, "%s\t%s\t%s", "", "", Ond).

alGespeeld(Str) :-
  searchstring(Str,"algespeeld!",_FoundPos),
  Med = "Het toernooi is al gespeeld en inschrijfcontrole kan niet meer!",
  _Answer = dlg_MessageBox( "Al gespeeld", Med, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  format(Tekst, "<br><SPAN class=\'rood\'>Fout bij de inschrijfcontrole:<br>%s<br>Contact de KNLTB!</SPAN>",Med),
  assert(headertekst(Tekst)).

mededelingC(Str) :-
  searchstring(Str,"!mededeling!",_FoundPos),
  str_len("!mededeling!",Pos),
  frontstr(Pos,Str,_,Med),
  _Answer = dlg_MessageBox("Fout bij de inschrijfcontrole", Med, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  format(Tekst, "<br><SPAN class=\'rood\'>Fout bij de inschrijfcontrole:<br>%s<br>Contact de KNLTB!</SPAN>",Med),
  assert(headertekst(Tekst)).

doDeelinrefresh() :-
  deelinwin(Win, _Tid),
  LboxWin = win_GetCtlHandle(Win, idcd_lbond),
  cTreeView::get_selected(LboxWin, CatNr),
  vink(Cat, CatNr),
  win_PostEvent(Win, e_user(deelinrefresh, Cat)), !.
doDeelinrefresh().

setAcceptatie() :-
  deelinWin(Win,_TidL),
  %member(Tid, TidL),
  %toernooiK(Tid,jrt),
  AWin = win_GetCtlHandle(Win, idcd_acceptatie),
  win_SetState(AWin, [wsf_Enabled]), !.
setAcceptatie() :-
  deelinWin(Win,TidL),
  member(Tid, TidL),
  toernooiK(Tid,jrt),
  AWin = win_GetCtlHandle(Win, idcd_acceptatie),
  win_SetState(AWin, [wsf_Enabled]), !.
setAcceptatie() :-
  deelinWin(Win,TidL),
  member(Tid,TidL),
  toernooiK(Tid,nrt),
  AWin = win_GetCtlHandle(Win, idcd_acceptatie),
  win_SetState(AWin, [wsf_Enabled]), !.
setAcceptatie() :-
  deelinWin(Win,_Tid),
  AWin = win_GetCtlHandle(Win, idcd_acceptatie),
  win_SetState(AWin, [wsf_Disabled]), !.
setAcceptatie().

setMetServices() :-
  deelinWin(Win, TidL),
  TidL = [""],
  AWin = win_GetCtlHandle(Win, idcd_check),
  win_SetState(AWin, [wsf_Disabled, wsf_Invisible]),
  BWin = win_GetCtlHandle(Win, idcd_acceptatie),
  win_SetState(BWin, [wsf_Disabled, wsf_Invisible]),
  CWin = win_GetCtlHandle(Win, idcd_opgavecheck),
  win_SetState(CWin, [wsf_Disabled,wsf_Invisible]),
   !.
setMetServices().

ophoudenmetchecken() :-
  %api_getComputername(PCname),
  inschrijfCheck(PCname,Tid,DatumTijd, Controleid),
  date(Year,Month,Day),
  time(Hours,Minutes,_Seconds,_Hundredths),
  dt_min_to_offset(Year, Month, Day, Hours, Minutes, Tijdstip),
  Delta = Tijdstip - DatumTijd,
  Delta > 60,  % = 1 uur
  logf('R', inschrijfCheck(PCname,Tid,DatumTijd, Controleid), nee),
  TaskWin = vpi_GetTaskWin(),
  toolbar_SetValue(TaskWin, idt_toolbarmsg, text_value("")),
  fail.
ophoudenmetchecken().

setCheckwachten() :-
  ophoudenmetchecken(),
  deelinWin(_Win,TidL),
  TidL = [Tid|_],
  WWin = win_GetCtlHandle(_Win, idctd_wachten),
  inschrijfCheck(_PCname,Tid,DatumTijd, _),
  date(Year,Month,Day),
  time(Hours,Minutes,_Seconds,_Hundredths),
  dt_min_to_offset(Year, Month, Day, Hours, Minutes, Tijdstip),
  Delta = Tijdstip - DatumTijd,
  format(Msg, "wachten op het controleresultaat van %s\nca %u minuten verstreken.",TID,Delta),
  win_SetText(WWin, Msg),
  win_invalidate(WWin),
  Bericht = "wachten op het controleresultaat",
  TaskWin = vpi_GetTaskWin(),
  toolbar_SetValue(TaskWin, idt_toolbarmsg, text_value(Bericht)),
  !.
setCheckwachten() :-
  deelinWin(_Win,TidL),
  TidL = [Tid|_],
  toernooiK(Tid,_),
  WWin = win_GetCtlHandle(_Win, idctd_wachten),
  win_SetText(WWin, "Geen inschrijfcontrole bezig."), !.
setCheckwachten() :-
  deelinWin(_Win,_),
  WWin = win_GetCtlHandle(_Win, idctd_wachten),
  win_SetText(WWin, ""), !.
setCheckwachten().

ruimOp_Links(Cat) :-
  kopl(Cat, K1, Naar, K2),
  logf('R',kopl(Cat, K1, Naar, K2), ja),
  fail.
ruimOp_Links(Cat) :-
  kopl(Van, K1, Cat, K2),
  logf('R',kopl(Van, K1, Cat, K2), ja),
  fail.
ruimOp_Links(_) :-
  logclose().

stopdbrefreshIfNotInschrijfcontrole() :-
  inschrijfcheck(_PC,_Tid,_,_), !.
stopdbrefreshIfNotInschrijfcontrole() :-
  stopdbrefresh().

siblingSchSrt(Cat, SchSrt, Cat1) :-
  kopl(CatD, _, Cat, _),
  not(CatD = Cat1),
  wc(CatD, _OnderdeeelID, _Nr, _Naam, _EDG, _Kort, _SexL, _SpStrk, _LftVan, _LftTM, _HDG, SchSrt, _Afkomst, _Tnr,_Geld),
  fronttoken(SchSrt, _, _).

handleVerKoppeling(Cat1,_CatSel) :-
  kopl(Cat1, V1, Catx, N1),
  logf('R',kopl(Cat1, V1, Catx, N1), nee),
  fail.
handleVerKoppeling(Cat1,_CatSel) :-
  kopl(Cat1, Van, Catx, Naar),
  logf('R',kopl(Cat1, Van, Catx, Naar), nee),
  fail.
handleVerKoppeling(Cat1,CatSel) :-
  %wc(CatSel, _, _, _, _, _, _, _, _, _, _, "0", _, _,_),
  isMasterCat(CatSel),
  getbacktrack(Here),
  wc(Cat1, OnderdeeelID, Nr, Naam, EDG, Kort, SexL, SpStrk, LftVan, LftTM, HDG, Schemasoort, Afkomst, Tnr,Geld),
  %write("\nSchSoort: ", SchemaSoort),
  findall(S1, siblingSchSrt(CatSel, S1, Cat1), S1List),
  %write("\n", S1List),
  member(Sdoel, ["A", "B", "C", "D", "E", "F", "G", "H"]),
  not(member(SDoel, S1List)),
  not(SDoel = Schemasoort),
  cutbacktrack(Here),
  concat(Kort, "-D", Kort1),
  concat(Naam, " D", Naam1),
  uniekNaam(Naam1, Kort1),
  %uniekKort(Kort1, Kort2),
  logf('Z', wc(Cat1, OnderdeeelID, Nr, Naam1, EDG, Kort1, SexL, SpStrk, LftVan, LftTM, HDG, SDoel, Afkomst, Tnr,Geld), ja),
  fail.
handleVerKoppeling(_Cat1,_CatSel) :-
  logclose().  

toernooitype_to_tekst(nrt, "Ranglijst toernooi") :-!.
toernooitype_to_tekst(jrt, "Ranglijst toernooi") :- !.
toernooitype_to_tekst(rolstoel, "Rolstoel toernooi") :- !.
toernooitype_to_tekst(_, "Open of clubtoernooi").

tidList_to_tekst([""], "Deel in") :- !.
tidList_to_tekst([Tid], Tekst) :-
  toernooiK(Tid, Type), !,
  toernooitype_to_tekst(Type, TTekst),
  format(Tekst, "Indelen van toernooinummer %s %s",Tid,TTekst).
tidlist_to_tekst(TidList, TitelTekst) :-
  list_to_string(TidList, ",", TidString),
  format(Titeltekst, "Indelen van meerdere toernooinummers: %s", TidString).

certificaatCheck(Tid) :-	% failt als geldig
  providerX(Prov,_,"",_),
  format(URL,"http://%s/knltbproxy2015/IsJuisteCertificaatEr.php",Prov),
  get_TempDir(_,TempFile),
  toernooiK(Tid, _Srt),
  deletefile(TempFile),
  Vars = aanmeldenVars(),
  _Return = downloadData(URL, TempFile, ["certcheck", Tid | Vars], _HttpResponsecode, 0),
  file_str(TempFile, GeldigTM),
  certIsOngeldig(GeldigTM).
  
certisongeldig(GeldigTM) :-
  upper_lower(GeldigTM, Lower),
  searchstring(Lower,"niet",_),
  _Answer = dlg_MessageBox( "Certificaatcheck", "Het certificaat is niet gevonden??", mesbox_iconerror, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), !.
certisongeldig(GeldigTM) :-
  trim_c(GeldigTM),
  dt_datestr_to_offset(GeldigTM, "%Dd-%Md-%YL", Offset),
  date(Year,Month,Day),
  dt_date_to_offset(Year,Month,Day, Huidig),
  Verschil = Offset - Huidig,
  Verschil < 100,
  _Answer = dlg_MessageBox( "Certificaatcheck", "Het certificaat moet eerst vernieuwd.\nZie Toernooinummer/Certificaat beheer.", mesbox_iconerror, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ).

cleanRelaties() :-
  kopl(Van, VanDom, Naar, IpvDom),
  not(wc(Van, _, _, _, _, _, _, _, _, _, _, _, _, _,_)),
  logf('R',kopl(Van, VanDom, Naar, IpvDom),ja),
  fail.
cleanRelaties() :-
  kopl(Van, VanDom, Naar, IpvDom),
  not(wc(Naar, _, _, _, _, _, _, _, _, _, _, _, _, _,_)),
  logf('R',kopl(Van, VanDom, Naar, IpvDom),ja),
  fail.
cleanRelaties() :-
  kopl(Van, _VanDom, Naar, _IpvDom),
  kopl(Naar, VanDomx, Van, IpvDomx),
  logf('R', kopl(Naar, VanDomx, Van, IpvDomx),ja),
  fail.
cleanRelaties() :-
  logclose().

    
  dlg_deel_in_Create(_, TidL):-
        loglock(lees, "Deelin"),
        logclose(),
	getbacktrack(Here),
	member(Tid,TidL),
	toernooiK(Tid,_),
	pos(Cat, _, _, _),
	wc(Cat, _, _, _, _, _, _, _, _, _, _, _, _, Tid,_),
	cutbacktrack(Here),
	assert(firstselect(Cat)),
	getcontroleresultaat1(Tid),
	fail.
  dlg_deel_in_Create(_,TidL):-
	deelinWin(Win,TidL), !,	% bestaat al
 	stopdbrefreshIfNotInschrijfcontrole(),
	trap(win_SetFocus(Win), _, true).
  dlg_deel_in_Create(_,_Tid):-
	deelinWin(Win,_), 
	win_destroy(Win),
	fail.
  dlg_deel_in_Create(Parent,TidL):-
        %loglock(lees,"deelin"),
        %logclose(),
 	stopdbrefreshIfNotInschrijfcontrole(),
	checkPaswoord(Parent),
	assert(deelinWin(~0, TidL)),
	%alsOnline(),
	win_CreateDynDialog(Parent,
		[
%BEGIN Deel in, WinDefList, 10:33:19-3.3.2015, Code automatically updated!
		 dlg_font(wdef(dlg_deel_in_DlgType,dlg_deel_in_RCT,dlg_deel_in_Title,u_DlgBase),
		 	  dlg_deel_in_Font,dlg_deel_in_FSize,dlg_deel_in_Flags),
		 customctl(wdef(wc_Custom,rct(1,3,188,224),"Custom",u_DlgBase),"treeView",idcd_lbond,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(192,3,250,14),"Onderdeelnaam",u_DlgBase),idcd_naamgeving,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(192,15,250,26),"(ver)&Plaats ",u_DlgBase),idcd_spelers_plaatsen,[wsf_Group,wsf_TabStop,wsf_Default]),
		 ctl(wdef(wc_PushButton,rct(192,27,250,38),"&Uitloothulp",u_DlgBase),idcp_uitloothulp,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(192,39,250,50),"Sp&lits ",u_DlgBase),idcd_splits,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(192,51,250,62),"&Combineer met ..",u_DlgBase),idcd_combineer,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(192,63,250,79),"&Indelen/loten",u_DlgBase),idc_ok,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(192,80,250,91),"&Wijzigopstelling",u_DlgBase),idcd_pbopstel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(192,92,250,103),"Wis &indeling",u_DlgBase),idcd_wis,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(192,104,250,115),"&Ronde +/- ",u_DlgBase),idcd_pbronde,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(192,116,250,127),"&Verliezersronde ",u_DlgBase),idcd_verliezer,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(192,128,250,139),"&1/2 of 3/4 plaats",u_DlgBase),idcd_34plaats,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(192,140,250,151),"&Eindronde",u_DlgBase),idcd_eindronde,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(192,152,250,163),"&Groepeer",u_DlgBase),idcd_verbindt,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(192,176,250,192),"R&apport",u_DlgBase),idcd_afdruk,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(345,207,379,225),"&Sluiten",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(285,211,317,221),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(258,130,375,142),"Inschrijfcontrole",u_DlgBase),idcd_check,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(258,143,375,155),"Acceptatieresultaat van KNLTB",u_DlgBase),idcd_acceptatie,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_Text,rct(254,12,380,69),"",u_DlgBase),idctd_gegevens,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(253,73,380,117),"",u_DlgBase),idctd_datum_lcheck,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(259,157,376,183),"",u_DlgBase),idctd_wachten,[wsf_AlignCenter]),
		 ctl(wdef(wc_GroupBox,rct(252,3,381,70),"Gegevens",u_DlgBase),idcd_gegevens,[]),
		 ctl(wdef(wc_GroupBox,rct(190,166,252,197),"Afdruk/rapport",u_DlgBase),idcd_afdruk1,[]),
		 ctl(wdef(wc_GroupBox,rct(257,121,379,186),"KNLTB diensten",u_DlgBase),idcd_opgavecheck,[])
%END Deel in, WinDefList
		],dlg_deel_in_eh,0).

%BEGIN Deel in, idc_ok _CtlInfo
  dlg_deel_in_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	dlg_deel_in_1_Create(_Win), 
	selectedCat(_Win, CatIn),
	posP(Cat, CatIn, _, No, _),
	select(Cat, No),
	LboxWin = win_GetCtlHandle(_Win, idcd_lbond),	% ververs entry
	wc(Cat, _, _, CatNaam, _, Kort, _, _, _, _, _, _, _, _,_),
	%wc(Cat,CatNaam,_,Kort,_,_,_),
	format(Repr, "%s * %s", Kort, CatNaam),   
	rsoort2bitmap(Cat, BitMap),
	vink(Cat, Nr),
	cTreeView::get_item(LboxWin, Nr, _, Style,_,_),
	overblijvendeOpgave(Cat, Style, State, alle),
	cTreeView::set_item(LboxWin, Nr, Repr, State, BitMap, BitMap), 
	selectedCat(_Win, Catxx),
	voorrondes(Catxx, MetVoorrondes),
	gegevens(_Win, Catxx, _AantalSel, MetVoorrondes),
	update_proj_toolbar(),
	win_SetFocus(LboxWin), !.
	
%END Deel in, idc_ok _CtlInfo
%MARK Deel in, new events

%BEGIN Deel in, idcd_acceptatie _CtlInfo
  dlg_deel_in_eh(_Win,e_Control(idcd_acceptatie,_CtrlType,_CtrlWin,_CtlInfo),0):-
	deelinWin(_win, TidL),
	member(Tid,TidL),
  	toernooiK(Tid, _Type),
  	%not(Type = open),
	getAcceptatieresultaat(_Win, Tid),
	!.
%END Deel in, idcd_acceptatie _CtlInfo

%BEGIN Deel in, idcp_uitloothulp _CtlInfo
  dlg_deel_in_eh(_Win,e_Control(idcp_uitloothulp,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idcd_lbond),
	cTreeView::get_Selected(LboxWin, Cat1Nr),
	vink(Cat1, Cat1Nr),
	dlg_uitloten_Create(_Win, Cat1),
	!.
%END Deel in, idcp_uitloothulp _CtlInfo

%BEGIN Deel in, idcd_check _CtlInfo
  dlg_deel_in_eh(_Win,e_Control(idcd_check,_CtrlType,_CtrlWin,_CtlInfo),0):-
	deelinWin(_Win, TidL),
	member(Tid,TidL),
	certificaatcheck(Tid), !.
  dlg_deel_in_eh(_Win,e_Control(idcd_check,_CtrlType,_CtrlWin,_CtlInfo),0):-
	deelinWin(_Win, TidL),
	member(Tid,TidL),
	getKNLTBdefinities(Tid,ja,b_False,_),
	retractall(onderdeelnr(_)),
	compress(update),
	startdbrefresh(),
	fail.
  dlg_deel_in_eh(_Win,e_Control(idcd_check,_CtrlType,_CtrlWin,_CtlInfo),0):-
        comline(LineBuffer),
        upper_lower(LineBuffer, LBL),
        searchstring(LBL, "/x", _),
	1 = dlg_MessageBox( "Test", "Attentie: herhaalzelfdecontrole?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
	write("Herhalen!"), 
	deelinWin(_Win, TidL),
	member(Tid,TidL),
        stopdbrefresh(),
        herhaalcontrole(Tid),
        startdbrefresh(), !. 
  dlg_deel_in_eh(_Win,e_Control(idcd_check,_CtrlType,_CtrlWin,_CtlInfo),0):-
	deelinWin(_Win, TidL),
	member(Tid,TidL),
	checkstart(Tid),
	startdbrefresh(),
	WWin = win_GetCtlHandle(_Win, idctd_wachten),
	format(Wmsg, "wachten op het controleresultaat van %s", Tid),
	win_SetText(WWin, Wmsg), !.
  dlg_deel_in_eh(_Win,e_Control(idcd_check,_CtrlType,_CtrlWin,_CtlInfo),0) :- !.

%END Deel in, idcd_check _CtlInfo

%BEGIN Deel in, idcd_verbindt _CtlInfo
  dlg_deel_in_eh(_Win,e_Control(idcd_verbindt,_CtrlType,_CtrlWin,_CtlInfo),0):-
	deelinWin(_Win,TidL),
	LboxWin = win_GetCtlHandle(_Win, idcd_lbond),
	cTreeView::get_Selected(LboxWin, Cat1Nr),
	vink(Cat1, Cat1Nr),
	kopl(Cat1, algemeen, Cat2, algemeen), !,
	wc(Cat2, _, _, ER, _, _, _, _, _, _, _, _, _, _,_),
	%wc(Cat2,ER,_,_,_,_,_),
	format(Msg, "Algemene koppeling met/naar %s.\nKoppeling verbreken?", ER),
	A = dlg_MessageBox( "Bevestig", Msg, mesbox_iconquestion, mesbox_buttonsokcancel, mesbox_defaultsecond, mesbox_suspendapplication ),
	%write("\n", A),
	A = 1,
	logf('R',kopl(Cat1, algemeen, Cat2, algemeen), nee),
	vulMetKnopen(LboxWin,TidL),
	nieuweselect(Lboxwin, Cat1Nr, CatNNw),
	vink(CatNNw, Cat1Nra),
	cTreeView::select(LboxWin, Cat1Nra),
	win_SetFocus(LboxWin), 
	voorrondes(Cat1, MetVoorrondes),
	gegevens(_Win, Cat1, _AantalSel, MetVoorrondes), !.
  dlg_deel_in_eh(_Win,e_Control(idcd_verbindt,_CtrlType,_CtrlWin,_CtlInfo),0):-
	deelinWin(_Win,TidL),
	LboxWin = win_GetCtlHandle(_Win, idcd_lbond),
	cTreeView::get_Selected(LboxWin, Cat1Nr),
	vink(Cat1, Cat1Nr),
	not(kopl(Cat1, algemeen, _, _)),
	findall(Cat2, teVerbinden(Cat1, Cat2), CatL),
	b_True = dlg_ListSelect("Koppelen naar",CatL,0,CatSel,_Index),
	parsetabs(CatSel,Items),
	Items = [CatSel1|_],
	wc(Cat1, _, _, ER, _, _, _, _, _, _, _, _, _, _,_),
        %wc(Cat1,ER,_,_,_,_,_),
	format(Msg, "Koppeling aanleggen van \n%s\nnaar\n%s?\n(NB: %s moet eind- of verzamelronde zijn!)", ER, CatSel, CatSel),
	b_True = dlg_MessageBox( "Algemene koppeling", Msg, mesbox_iconquestion, mesbox_buttonsokcancel, mesbox_defaultsecond, mesbox_suspendapplication ),
	%write("\nCatSel1: ", CatSel1),
	wc(CatNaar, _, _, CatSel1, _, _, _, _, _, _, _, _, _, _,_),
	%write("\nCatnaar: ",CatNaar),
	%wc(Catx,CatSel,_,_,_,_,_),
	handleVerKoppeling(Cat1,CatNaar),
	logf('Z', kopl(Cat1, algemeen, CatNaar, algemeen), nee),
	vulMetKnopen(LboxWin,TidL),
	nieuweselect(Lboxwin, Cat1Nr, CatNNw),
	vink(CatNNw, Cat1Nra),
	cTreeView::select(LboxWin, Cat1Nra),
	%cTreeView::select(LboxWin, Cat1Nr),
	win_SetFocus(LboxWin),
	voorrondes(Cat1, MetVoorrondes),
	gegevens(_Win, Cat1, _AantalSel, MetVoorrondes), 
	!.
%END Deel in, idcd_verbindt _CtlInfo

%BEGIN Deel in, idcd_naamgeving _CtlInfo
  dlg_deel_in_eh(_Win,e_Control(idcd_naamgeving,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	naamgeving(_Win),
	win_SetFocus(_Win),
	!.
%END Deel in, idcd_naamgeving _CtlInfo

%BEGIN Deel in, idcd_eindronde _CtlInfo
  dlg_deel_in_eh(_Win,e_Control(idcd_eindronde,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	deelinWin(_Win,TidL),
	LboxWin = win_GetCtlHandle(_Win, idcd_lbond),
	cTreeView::get_Selected(LboxWin, CatNr),
	vink(Cat, CatNr),
	findall(Catnaam, soortGenoot(Cat, Catnaam), CatNamen),	% 12.11.2001
	findall(CatP, algeselecteerd(Cat, CatP), PNamen),
	preselectV(CatNamen, PNamen, 0, Sellist),
	wc(Cat, _, _, ER, _, _, _, _, _, _, _, _, _, _,_),
	%wc(Cat,ER,_,_,_,_,_),   
	dlg_koppelingmodel_Create(_Win, Cat, ER, CatNamen, Sellist, ReturnCode),
	ReturnCode = 1, 
	vulMetKnopen(LboxWin,TidL),
	pos(CatN, _, _, _), !,
	vink(CatN, CatNNr),
	cTreeView::select(LboxWin, CatNNr),
	win_SetFocus(LboxWin),
	voorrondes(CatN, MetVoorrondes),
	gegevens(_Win, CatN, _AantalSel,MetVoorrondes),
	!.
%END Deel in, idcd_eindronde _CtlInfo

%BEGIN Deel in, idcd_34plaats _CtlInfo
  dlg_deel_in_eh(_Win,e_Control(idcd_34plaats,_CtrlType,_KWin,_CtlInfo),0):-
	%RCT = win_GetOuterRect(KWin),
	%RCT = rct(L, T, R, B),
	%R1 = (L + R) div 2,
	%B1 = (T + B) div 2,
	LboxWin = win_GetCtlHandle(_Win, idcd_lbond),
	cTreeView::get_Selected(LboxWin, CatNr),
	vink(Cat, CatNr),
	%enableDrieVier(Cat, Enable34),
	%enableEenTwee(Cat, Enable12),
	eenTwee34(Cat, Titel, Tekst, Type1234),
	1 = dlg_MessageBox( Titel, Tekst, mesbox_iconquestion, mesbox_buttonsokcancel, mesbox_defaultfirst, mesbox_suspendapplication ),
	plaats34(_Win, Cat, Type1234),
	!.
%END Deel in, idcd_34plaats _CtlInfo

%BEGIN Deel in, idcd_verliezer _CtlInfo
  dlg_deel_in_eh(_Win,e_Control(idcd_verliezer,_CtrlType,_CtrlWin,_CtlInfo),0):-
	deelinWin(_Win,TidL),
	LboxWin = win_GetCtlHandle(_Win, idcd_lbond),
	cTreeView::get_Selected(LboxWin, CatNr),
	vink(Cat, CatNr),
	kopl(CatT,verliezer,Cat,_),
	%getbacktrack(Here),
	%wc(Cat,Kop,EDG,Kort,VoorWie,Geld, CatT), 
	%cutbacktrack(Here),
	%CatT <> 0, 					% verliezersronde bestaat
	vink(CatT, CatTNr),
	cTreeView::select(LboxWin, CatTNr),
	1 = dlg_MessageBox( "Verliezersronde", "Losmaken van hoofdronde?\nDaarmee wordt het een zelfstandig onderdeel met dezelfde kenmerken.", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
	%findall(Cat1, wc(Cat1,_,_,_,_,_,_), Cat1L),	% zelfde pos in wclijst !
	%positionWC(Cat, Cat1L, Cat1L1),
	%evenWegWC(Cat1L1, Cat1L1Vol),			% haal achterste stuk weg
	%logf('Z',wc(Cat,Kop,EDG,Kort,VoorWie,Geld, 0),ja),
	%Cat1L1Vol = [_|Cat1L1Volx1],
	%sluitAanWC(Cat1L1Volx1),			% en voeg weer toe
	%logclose(),
	logf('R',kopl(CatT,verliezer,Cat,algemeen),nee),
	vulMetKnopen(LboxWin,TidL),
	close_alles(),
	cTreeView::select(LboxWin, CatNr),
	!.
  dlg_deel_in_eh(_Win,e_Control(idcd_verliezer,_CtrlType,_CtrlWin,_CtlInfo),0):-
	deelinWin(_Win,TidL),
	LboxWin = win_GetCtlHandle(_Win, idcd_lbond),
	cTreeView::get_Selected(LboxWin, CatNr),
	vink(Cat, CatNr),
	not(kopl(Cat,verliezer,_,_)),
	%wc(Cat,_Kop,_EDG,_Kort,_VoorWie,_Geld,0),	% verliezersronde niet gedefiniëerd
	bestaandOfNieuw(_Win, Cat, BestaandNieuw),
	vulMetKnopen(LboxWin,TidL),
	close_alles(),
	vink(BestaandNieuw, BNNr),
	cTreeView::select(LboxWin, BNNr),
	win_SetFocus(LboxWin),

	voorrondes(BestaandNieuw, MetVoorrondes),
	gegevens(_Win, BestaandNieuw, _AantalSel, MetVoorrondes),


	%gegevens(_Win, AantalSel, _),
	%zetVorm(_Win, BestaandNieuw, AantalSel),
	not(pos(BestaandNieuw, _, _, _)),		% nog niet ingedeeld
	Msg = "Als je meteen indeelt met open plaatsen worden de verliezers\ndirect opgesteld zodra ze bekend zijn.\nJe kunt ook naderhand indelen.",
	dlg_MessageBox( "Troost-/verliezersronde", Msg, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	!.
  dlg_deel_in_eh(_Win,e_Control(idcd_verliezer,_CtrlType,_CtrlWin,_CtlInfo),0):-
	logclose(), !.
%END Deel in, idcd_verliezer _CtlInfo

%BEGIN Deel in, e_User
  dlg_deel_in_eh(_Win,e_User(1,_Ptr),0):-!,		% waar wordt dit gebruikt? 
	beep(),
	win_Destroy(_Win),
	!.
  dlg_deel_in_eh(_Win,e_User(deelincreate,_Ptr),0):-!,		% extended create = 2
	win_SetFocus(_Win),
	firstselect(Cat),
	LboxWin = win_GetCtlHandle(_Win, idcd_lbond),
	vink(Cat, CatNr),
	cTreeView::select(LboxWin, CatNr),
	voorrondes(Cat, MetVoorrondes),
	gegevens(_Win, Cat, Aantal, MetVoorrondes),
	indeelbaar(_Win, Cat, Aantal), 
	win_SetFocus(_Win),
	!.
  dlg_deel_in_eh(_Win,e_User(deelinnaamgeving,_Ptr),0):-!, 	% naamgeving = 3
	naamgeving(_Win),
	!.
  dlg_deel_in_eh(_Win,e_User(deelinrefresh,CatNew),0):-         %refresh = 4
        %write("\nHier refresh"),
	deelinWin(_Win,TidL),
	%WWin = win_GetCtlHandle(_Win, idctd_wachten),
        setCheckwachten(),
	LboxWin = win_GetCtlHandle(_Win, idcd_lbond),
	update_proj_toolbar(),
	vulMetKnopen(LboxWin,TidL),
	vink(CatNew, CatNewNr),
	assert(refresh()),
	cTreeView::select(LboxWin, CatNewNr),
	retractall(refresh()),
	voorrondes(CatNew, MetVoorrondes),
	gegevens(_Win, CatNew, _AantalSel, MetVoorrondes),
	!.
  dlg_deel_in_eh(_Win,e_User(controletimerefresh,_CatNew),0):-         %refresh = 5
        setCheckwachten(), !.
%END Deel in, e_User

%BEGIN Deel in, idc_help _CtlInfo
  dlg_deel_in_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Deelin"),
	!.
%END Deel in, idc_help _CtlInfo

%BEGIN Deel in, idcd_pbopstel _CtlInfo
  dlg_deel_in_eh(_Win,e_Control(idcd_pbopstel,_CtrlType,_CtrlWin,_CtlInfo),0):-
	close_alles(),
	selectedCat(_Win, Cat),
	pos(Cat, W, _, afval),
	W1 = 2 * W,
	win_Destroy(_Win),
	select(Cat, W1, 1,0,onbekend), 	% 1 = wijzig opstellingsmode
	!.
  dlg_deel_in_eh(_Win,e_Control(idcd_pbopstel,_CtrlType,_CtrlWin,_CtlInfo),0):-
	deelinWin(_Win,TidL),
	LboxWin = win_GetCtlHandle(_Win, idcd_lbond),
	cTreeView::get_Selected(LboxWin, CatNr),
	vink(Cat, CatNr),

	%selectedCat(_Win, Cat),
	pos(Cat, _W, _Pos, poel),
	%select(Cat, Pos,0,1,onbekend),
	dlg_deel_in_poule_Create(_Win, Cat),
	%voorrondes(Cat, MetVoorrondes),
	%gegevens(_Win, Cat, _AantalSel, MetVoorrondes),
	%zetVorm(_Win, Cat, AantalSel),
	LboxWin = win_GetCtlHandle(_Win, idcd_lbond),
	%vink(Cat, Nr),
	vulMetKnopen(LboxWin,TidL),
	cTreeView::select(LboxWin, CatNr),
	win_SetFocus(LboxWin),
	voorrondes(Cat, MetVoorrondes),
	gegevens(_Win, Cat, _AantalSel, MetVoorrondes), 

	%cTreeView::get_item(LboxWin, Nr, Repr, Style, BitMap1, BitMap2),
	%overblijvendeOpgave(Cat, Style, State, alle),
	%cTreeView::set_item(LboxWin, Nr, Repr, State, BitMap1, BitMap2), 
	!.
%END Deel in, idcd_pbopstel _CtlInfo

%BEGIN Deel in, idcd_wis _CtlInfo
/*
  dlg_deel_in_eh(_Win,e_Control(idcd_wis,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idcd_lbond),
	cTreeView::get_Selected(LboxWin, CatNr),
	vink(Cat, CatNr),
	voorrondes(Cat,b_True),
	2 = dlg_MessageBox( "Indeling wissen", "De koppeling met de voorrondes wordt ook gewist!\nDoorgaan?", mesbox_iconexclamation, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
	!.
*/
  dlg_deel_in_eh(_Win,e_Control(idcd_wis,_CtrlType,_CtrlWin,_CtlInfo),0):-
	deelinWin(_Win,TidL),
	LboxWin = win_GetCtlHandle(_Win, idcd_lbond),
	cTreeView::get_Selected(LboxWin, CatNr),
	vink(Cat, CatNr),
	wc(Cat, _, _, K2, _, _, _, _, _, _, _, _, _, _,_),
	%wc(Cat,K2,_,_,_,_,_), 
	format(Titel, "wissen van\n%s?",K2),
	1 = dlg_MessageBox( "Indeling", Titel, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
	logf('T', wd(1,Cat,onbekend,onbekend,"",0,""), nee), 
	%ruim_opT(Cat),
	close_alles(),
	not(verwijderOnderdeelNiet(Cat)),
	ruimOp_Links(Cat),
	nieuweSelect(LboxWin, Cat, CatN),		% onderdeel wordt helemaal weggehaald
	vulMetKnopen(LboxWin,TidL),
	close_alles(),
	%retractall(modified()),
	%assert(modified()),
	vink(CatN, CatNNr),
	cTreeView::select(LboxWin, CatNNr),
	win_SetFocus(LboxWin),
	voorrondes(CatN, MetVoorrondes),
	gegevens(_Win, CatN, _, MetVoorrondes),
	%zetVorm(_Win, CatN, AantalSel),
	update_proj_toolbar(),
	win_SetFocus(LboxWin), !.
  dlg_deel_in_eh(_Win,e_Control(idcd_wis,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	deelinWin(_Win,TidL),
	LboxWin = win_GetCtlHandle(_Win, idcd_lbond),	% alleen de indeling is gewist
	cTreeView::get_Selected(LboxWin, CatNr),
	vink(Cat, CatNr),
	vulMetKnopen(LboxWin,TidL),
	close_alles(),
	vink(Cat, CatNrN),
	cTreeView::select(LboxWin, CatNrN),
	win_SetFocus(LboxWin),
	voorrondes(Cat, MetVoorrondes),
	gegevens(_Win, Cat, _,MetVoorrondes),
	%zetVorm(_Win, Cat, AantalSel),
	update_proj_toolbar(),
	%retractall(modified()),
	%assert(modified()),
	win_SetFocus(LboxWin),
	!.
%END Deel in, idcd_wis _CtlInfo

%BEGIN Deel in, idcd_pbronde _CtlInfo
  dlg_deel_in_eh(_Win,e_Control(idcd_pbronde,_CtrlType,_CtrlWin,_CtlInfo),0):-
	selectedCat(_Win, Cat),
	pos(Cat, _, _, afval),
	ronde_bij_af(Cat),
	select(Cat, 3),
	update_display(),
	!.
%END Deel in, idcd_pbronde _CtlInfo

%BEGIN Deel in, idcd_splits _CtlInfo
  dlg_deel_in_eh(_Win,e_Control(idcd_splits,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idcd_lbond),
	cTreeView::get_selected(LboxWin, CatNr),
	vink(Cat, CatNr),
	%write("\nTe splitsen Cat: ",Cat),
	dlg_splitsen_Create(_Win,Cat,CatNew),
	cleanRelaties(),
	win_PostEvent(_Win, e_user(deelinrefresh, CatNew)),
	!.
%END Deel in, idcd_splits _CtlInfo

%BEGIN Deel in, idcd_combineer _CtlInfo
  dlg_deel_in_eh(_Win,e_Control(idcd_combineer,_CtrlType,_CtrlWin,_CtlInfo),0):-
	deelinWin(_Win, TidL),
	LboxWin = win_GetCtlHandle(_Win, idcd_lbond),
	cTreeView::get_selected(LboxWin, CatNr),
	vink(Cat, CatNr),
	combineer(_Win, Cat, Done),	% modified set
	Done = b_True,	
	vulMetKnopen(LboxWin,TidL),
	vink(Cat, CatNrN),
	cTreeView::select(LboxWin, CatNrN),
	win_SetFocus(LboxWin),
	voorrondes(Cat, MetVoorrondes),
	gegevens(_Win, Cat, _AantalSel, MetVoorrondes),
	%zetVorm(_Win, Cat, AantalSel),
	!.
  dlg_deel_in_eh(_Win,e_Control(idcd_combineer,_CtrlType,_CtrlWin,_CtlInfo),0):-
	win_SetFocus(_Win), !.
%END Deel in, idcd_combineer _CtlInfo

%BEGIN Deel in, idcd_afdruk _CtlInfo
  dlg_deel_in_eh(_Win,e_Control(idcd_afdruk,_CtrlType,_CtrlWin,_CtlInfo),0):-
	%LboxWin = win_GetCtlHandle(_Win, idcd_lbond),
	%cTreeView::get_selected(LboxWin, CatNr),
	%vink(Cat, CatNr),
	%opg_cat(Item,Cat,1),
	TaskWin = vpi_GetTaskWin(),
	dlg_indelingsrapport_Create(Taskwin, 2, "Afdruk"),  % == opstelling!
	win_Destroy(_Win),
	!.
%END Deel in, idcd_afdruk _CtlInfo


%BEGIN Deel in, idcd_spelers_plaatsen _CtlInfo
  dlg_deel_in_eh(_Win,e_Control(idcd_spelers_plaatsen,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	deelinWin(_Win,TidL),
	LboxWin = win_GetCtlHandle(_Win, idcd_lbond),
	cTreeView::get_selected(LboxWin, CatNr),
	vink(Cat, CatNr),
	selectAlsIngedeeld(Cat),
	dlg_plaatsing_Create(_Win, Cat, 0),
	update_proj_toolbar(),
	vulMetKnopen(LboxWin,TidL),
	volgendeTwee(LboxWin, Cat),
	vink(Cat, CatNrN),
	cTreeView::select(LboxWin, CatNrN),
	win_SetFocus(LboxWin),
	voorrondes(Cat, MetVoorrondes),
	gegevens(_Win, Cat, _AantalSel, MetVoorrondes),
	%zetVorm(_Win, Cat, AantalSel),
	!.
%END Deel in, idcd_spelers_plaatsen _CtlInfo


%BEGIN Deel in, idc_cancel _CtlInfo
  dlg_deel_in_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-
	trap(win_Destroy(_Win), _, fail), !.
%END Deel in, idc_cancel _CtlInfo

%BEGIN Deel in, e_Destroy
  dlg_deel_in_eh(_Win,e_Destroy,0):-
	startdbrefresh(),
	retractall(refresh()),
	fail.
  dlg_deel_in_eh(_Win,e_Destroy,0):-
	startDBrefresh(),
	updateVensterpositie(indeelvenster, _Win),
	retractall(deelinWin(_Win,_)),
	!.
%END Deel in, e_Destroy

%BEGIN Deel in, e_Create
  dlg_deel_in_eh(_Win,e_Create(_Parm),0):-
	close_alles(),
	cleankoppelingen(),
        retract(deelinWin(~0,TidList)),
	assert(deelinWin(_Win,TidList)),
	%write("\ntidlist: ", TidList),
	tidlist_to_tekst(TidList, TitelTekst),
	win_SetText(_Win,TitelTeksT),
	dialoogpositie(indeelvenster, _Win),
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
	LboxWin = win_GetCtlHandle(_Win, idcd_lbond),
	cTreeView::init(LboxWin, [tvs_hasbuttons, tvs_haslines, tvs_linesatroot, tvs_showselalways],resid(idb_teesize),callbackI),
	vulMetKnopen(LboxWin, TidList),
	win_PostEvent(_Win,e_User(deelincreate,0)),		% extended 
	GegWin = win_GetCtlHandle(_Win, idctd_gegevens),
	Font1 = font_Create(ff_Fixed, [fs_Bold], 9),
	%CWin = win_GetCtlhandle(_Win, idctd_wachten),
	win_SetFont(GegWin, Font1),
	setCheckwachten(),
	setAcceptatie(),
	setMetServices(),
	!.
%END Deel in, e_Create

  dlg_deel_in_eh(_,_,_):-!,fail.

%END_DLG Deel in

%______________________ronde bij/af _________________________________________

predicates

%  handm_cat(wedscat, integer)
  voorronde1(integer,integer,wedscat)
  verplaatskoppelingUit(wedscat, tgnst, integer, integer LR, integer Nieuw)
  verplaatskoppelingIn(wedscat, integer)
  procedure removeKoppeling(wedscat, integer)
  procedure verplaatskoppelingInSchuin(wedscat, integer, integer)
  helftbezet(wedscat Cat, integer Onder, integer Boven)
  onecht(tgnst, tgnst)
  koppeling(wedscat, integer)
  
clauses

ronde_bij_af(CatSel) :- 
  Ans = dlg_Ask("Omvang","Keuze:",["&Cancel","&Uitbreiden","&Inkrimpen"]),
  handm_cat(CatSel,Ans), !.
ronde_bij_af(_).

helftbezet(Cat, Onder, Boven) :-
  wd(Wnum,Cat,T1,T2,_,_,_),  % als er een echte partij is met een nummer 
  Wnum >= Onder,
  Wnum < Boven,
  not(koppeling(Cat, Wnum)),
  not(onecht(T1, T2)), !.

onecht(bye, bye) :- !.
onecht(onbekend, onbekend) :- !.
onecht(bye, onbekend) :- !.
onecht(onbekend, bye) :- !.

onbezet(bye, _) :- !.
onbezet(_, bye).

handm_cat(Cat, 1) :-
  pos(Cat,Card,_,_),
  wd(Wnum,Cat,T1,T2,_,_,_),  % als er een echte partij is met een nummer 
  Wnum >= Card,
  T1 = bye,
  T2 = bye, !,
  dlg_Note("Uitbreiding","Uitbreiding is er al").
handm_cat(Cat, 1) :-
  pos(Cat,Card,_Pos,Srt),
  Card <= 128,
  bitleft(Card,1,NewCard),
  voorronde1(Card,NewCard,Cat),
  %bitleft(Pos,1,NewPos),	
  logf('A',pos(Cat,NewCard,NewCard,Srt),nee),
  dlg_Note("Uitbreiding","Schema uitgebreid"), !.
handm_cat(_, 1) :- !,
  dlg_Note("Uitbreiding","Max. schemagrootte bereikt").
handm_cat(Cat, 2) :-
  pos(Cat,1,_,_), !.
handm_cat(Cat, 2) :-
  pos(Cat,Card,_,_),
  assert(inkrimpen(0)),
  Boven = Card + Card div 2,
  %helftbezet(Cat, Card, Boven),
  Boven1 = Card * 2 + 1,
  helftbezet(Cat, Boven, Boven1), 
  wd(Wnum,Cat,T1,T2,_,_,_),  % als er een echte partij is met een nummer 
  Wnum >= Card,
  not(onbezet(T1, T2)), !,
  %not(T1 = bye),
  %not(T2 = bye), !,
  dlg_Note("Inkrimpen","Kan niet verder krimpen,\nde eerste ronde is nog nodig").
handm_cat(Cat, 2) :-
  pos(Cat,Card,_,_Schema),
  Boven = Card + Card div 2,
  helftbezet(Cat, Card, Boven),		% onderste helft kan leeg gemaakt worden
  Boven1 = Card * 2 + 1,
  not(helftbezet(Cat, Boven, Boven1)), 
  assert(inkrimpen(1)),
  getbacktrack(Here),
  numb(N),
  card(N, CardN),
  N >= CardN + CardN div 2,
  wd(N,Cat,T1,T2,U,D,Nota),  		% als er een echte partij is met een nummer 
  logf('R',wd(N,Cat,T1,T2,U,D,Nota),ja),
  removekoppeling(Cat, N),
  N >= 2 * Card - 1,
  cutbacktrack(Here),
  fail.
handm_cat(Cat, 2) :-
  inkrimpen(1),
  pos(Cat,Card,Posx,Schema),
  logf('A', pos(Cat,Card,Posx,Schema),ja), 
  %Boven = Card + Card div 2,
  %helftbezet(Cat, Card, Boven),		% bovenste helft nog steeds bezet, nu verhuizen
  Y = 2 * Card - 1,
  not(wd(Y,Cat,_,_,_,_,_)),		% benedenste helft is verdwenen
  %getbacktrack(Here),
  numb(N),
  N1 = N + 1,
  card(N1, CardN1),
  bitright(CardN1, 1, Offset), 
  Nnew = N1 - Offset,
  wdnumOud2Nieuw(Cat, N1, Nnew),
  verplaatskoppelingInSchuin(Cat, N1, Nnew),
  N1 >= 2 * Card - 1,
  %pos(Cat,Card,_Posi,Srt),
  bitright(Card,1,C1),
  logf('A',pos(Cat,C1,C1,Schema),nee),
  dlg_Note("Inkrimpen","Onderste helft gewist!"), !.
handm_cat(Cat, 2) :-			% als hele ronde kon
  inkrimpen(0),
  pos(Cat,Card,_,_),
  wd(Wnum,Cat,T1,T2,U,D,Nota),
%  wd(Wnum,Cat,_,_,_,_),    
  Wnum >= Card,
  logf('R',wd(Wnum,Cat,T1,T2,U,D,Nota),ja),
  verplaatskoppelingIn(Cat, Wnum),
  fail.
handm_cat(Cat, 2) :-
  inkrimpen(0),
  pos(Cat,Card,_Posi,Srt),
  bitright(Card,1,C1),
  C1 > 0, !,
  logf('A',pos(Cat,C1,C1,Srt),nee),
  dlg_Note("Inkrimpen","De eerste ronde gewist!").
handm_cat(Cat, 2) :-
  inkrimpen(0),
  pos(Cat,Card,Pos,Srt), 
  logf('R',pos(Cat,Card,Pos,Srt),nee), !,
  dlg_Note("Inkrimpen","Scema is opgeheven").

voorronde1(End,End,_) :- !.
voorronde1(Curr,End,Cat) :-
  wd(Curr,Cat,T1,T2,_,_,Nota), !,
  bitleft(Curr,1,N1),
  logf('Z',wd(N1,Cat,T1,bye,"",0,Nota),ja),
  verplaatskoppelingUit(Cat, T1, Curr, 0, N1),
  N2 = N1 + 1,
  logf('Z',wd(N2,Cat,bye,T2,"",0,Nota),ja),
  verplaatskoppelingUit(Cat, T2, Curr, 1, N2),
  C1 = Curr + 1,
  voorronde1(C1,End,Cat).

verplaatskoppelingUit(Cat, onbekend, Curr, LR, Nieuw) :-
  kopl(CatFr, rang(Nr), Cat, plaats(PosUit)),
  bitright(PosUit, 1, KPos),
  bitand(PosUit, 1, KLR),
  KPos = Curr,
  LR = KLR, !,
  bitleft(Nieuw, 1, NPos),
  NPos1 = NPOs + LR,
  logf('Z', kopl(CatFr, rang(Nr), Cat, plaats(NPos1)),ja).
verplaatskoppelingUit(_, _, _, _, _).

verplaatskoppelingIn(Cat, No) :-
  kopl(CatFr, rang(Nr), Cat, plaats(PosUit)),
  bitright(PosUit, 1, Pos1),
  Pos1 = No,
  logf('Z', kopl(CatFr, rang(Nr), Cat, plaats(Pos1)), ja), !.
verplaatskoppelingIn(_Cat, _No).

verplaatskoppelingInSchuin(Cat, No, NoNew) :-
  kopl(CatFr, Nr, Cat, plaats(PosUit)),
  bitright(PosUit, 1, Pos1),
  Pos1 = No,
  bitand(PosUit, 1, LR),
  NPos = NoNew + LR,
  logf('A', kopl(CatFr, Nr, Cat, plaats(NPos)), ja),
  fail.
verplaatskoppelingInSchuin(_Cat, _No, _).

koppeling(Cat, Nr) :-
  kopl(_CatFr, rang(Nr), Cat, plaats(PosUit)),
  bitright(PosUit, 1, Pos1),
  Pos1 = Nr, !.

removeKoppeling(Cat, No) :-
  kopl(CatFr, Nr, Cat, plaats(PosUit)),
  bitright(PosUit, 1, Pos1),
  Pos1 = No,
  logf('R',kopl(CatFr, Nr, Cat, plaats(PosUit)), ja),
  fail.
removeKoppeling(_Cat, _No).

%BEGIN_DLG Deel in poule
/**************************************************************************
	Creation and event handling for dialog: Deel in poule
**************************************************************************/

constants

%BEGIN Deel in poule, CreateParms, 22:07:13-27.10.2011, Code automatically updated!
  dlg_deel_in_poule_DlgType = wd_Modal
  dlg_deel_in_poule_Title = "Pouledeelname"
  dlg_deel_in_poule_RCT = rct(40,32,316,194)
  dlg_deel_in_poule_Flags = [wsf_Close,wsf_TitleBar]
  dlg_deel_in_poule_Help = idh_pouleaanpassing
  dlg_deel_in_poule_Font = "Arial"
  dlg_deel_in_poule_FSize = 10
%END Deel in poule, CreateParms

/*domains
opgavedom = opgave(integer) ; openplaats(integer) ; niet ; nul
opgavedoml = opgavedom*  */

database - poeltmp
  poelOnd(WINDOW, wedscat Onderdeel)
  deelnr(WINDOW, tgnst)
  deelnrNw(WINDOW, tgnst Oud, tgnst Nw)	% nieuwe lijst
  gegadigde(WINDOW, tgnst, boolean VerwijderdVerhuisd, wedscat)
  volgorde(WINDOW, tgl)
  t_kopl(window, wedscat CatVan, vandom, wedscat Onderdeel, ipvdom)
  
predicates

  dlg_deel_in_poule_eh : EHANDLER
  copyDeelnr(WINDOW, wedscat)
  copyGegadigde(WINDOW, wedscat Onderdeel)
  nondeterm deelnrNaam(WINDOW, string, tgnst)
  %deelnrNaam1(wedscat, tgnst, string Naam)
  nondeterm gegadigdeNaam(WINDOW, string, tgnst)
procedure   t_tgnst_naam(window, wedscat Cat, tgnst, string Naam)
  pouleVeranderd(WINDOW, tgl Lijst)
  savePoel(WINDOW, wedscat)
  savePoel(WINDOW, wedscat, tgnst, tgl)
  filter1(integer No, wedscat, tgnst, tgnst, tgnst, tgnst)
  %procedure opgevenAlsOpenPlaats(wedscat Cat,opgavedom, opgavedom)
  %procedure verwijderOpenPlaats(wedscat Cat)
  %laatste(integerlist, integer)
  voegtoe(wedscat Cat, tgnst bijNaam, tgl Bestaanden, integer HoogsteWedNo)
  nondeterm bestaandeDeelnr(WINDOW, tgnst Deelnr)
  adjustVoorDos(wedscat)
  shiftWNums(wedscat, integerlist Achterstevoren, integer Verschuiving)
  compressWNums(wedscat)
  oplopend(wedscat, integerlist Oplopend)
  %wdnumOud2Nieuw(wedscat, integer WNoOud, integer WNoNieuw)
%  tgnst_naam(tgnst, string) - (i,o)
  nietEcht(wedscat)
  checkPoule(wedscat)
  oplappen(wedscat)
  boekDeelnemer(WINDOW, tgnst)
  procedure tgnst2Plaats(tgnst, ipvdom)
  procedure checkEnableVerhuisWis(window)
  procedure verhuisd(boolean, wedscat Cat, string Naam, string Naam1)

clauses

compressWNums(Cat) :-
  pos(Cat, Card, _, _Type), !,
  findall(WNo, wd(WNo,Cat,_,_,_,_,_), WList),
  %count(WList, 0, Aantal),
  sortint_c(WList),
  Begin = Card - 1, 	% zorg dat de eerste op card terecht komt
  oplopend(Cat, [Begin|WList]).
  
oplopend(Cat, [WNo1, WNo2 | Rest]) :-
  WNo2S = WNo1 + 1,
  wdnumOud2Nieuw(Cat, WNo2, Wno2S),
  oplopend(Cat, [WNo2S | Rest]), !.
oplopend(_, _).

wdnumOud2Nieuw(_Cat, WNo, WNo) :- !.		% hoeft niet
wdnumOud2Nieuw(Cat, WNum, WnumN) :-
  wd(WNum,Cat,Tg1,Tg2,U,D,N),
  logf('R', wd(WNum,Cat,Tg1,Tg2,U,D,N),ja),
  logf('Z', wd(WNumN,Cat,Tg1,Tg2,U,D,N),ja),
  fail.
wdnumOud2Nieuw(Cat, WNum, WnumN) :-
  w2(WNum,Cat,Tijd,Gew1,Gew2,Fixeer,VTijd,Bn),
  logf('R', w2(WNum,Cat,Tijd,Gew1,Gew2,Fixeer,VTijd,Bn),ja),
  logf('Z', w2(WNumN,Cat,Tijd,Gew1,Gew2,Fixeer,VTijd,Bn),ja),
  fail.
wdnumOud2Nieuw(Cat, WNum, WnumN) :-
  w3(WNum, Cat, TgWin, P1, P2, Stat, Stamp),
  logf('R', w3(WNum, Cat, TgWin, P1, P2, Stat, Stamp),ja),
  logf('Z', w3(WNumN, Cat, TgWin, P1, P2, Stat, Stamp),ja),
  fail.
wdnumOud2Nieuw(Cat, WNum, WnumN) :-
  ws(WNum, Cat, TgWin, Stamp), 
  logf('R', ws(WNum, Cat, TgWin, Stamp),ja),
  logf('Z', ws(WNumN, Cat, TgWin, Stamp),ja),
  fail.
wdnumOud2Nieuw(_, _, _).

bestaandeDeelnr(Win, TgnstD) :-
  deelnrNw(Win, TgnstO, TgnstD),
  not(TgnstO = onbekend),	% niet
  not(TgnstD = onbekend).
  
voegtoe(_, _, [], _) :- !.
voegtoe(Cat, Tgnst, [T2|Rest], Hoogste) :-
  H1 = Hoogste + 1,
  %opg(Best,Cat,T2,_,_,_,_,_), !,
  logf('Z', wd(H1,Cat,Tgnst,T2, "", 0, ""),ja),
  voegtoe(Cat, Tgnst, Rest, H1). 

adjustVoorDos(Cat) :-
  pos(Cat, Card, _, Type),
  findall(WNum, wd(Wnum,Cat,_,_, _, _, _), WList),
  sortint_c(WList),
  reverse(WList, [], WListRev),			% van boven naar beneden!
  WListRev = [Laatste|_],
  %write("\nCard:",Card, " Laatste:", Laatste),
  Laatste >= 2 * Card,
  card(Laatste, NewCard),
  Shift = NewCard - Card,
  shiftWNums(Cat, WListRev, Shift), 
  wd(Wnum,Cat,_Tg1,_Tg2,_U,_D, _),			% willekeurige bestaande wedstrijd
  logf('A',pos(Cat, NewCard, Wnum, Type),ja),
  !.
adjustVoorDos(_Cat).

shiftWNums(_, [], _) :- !.			%  ook voor DOS
shiftWNums(Cat, [WNum|T], Shift) :-
  WNumNew = WNum + Shift,
  wdnumOud2Nieuw(Cat, WNum, WnumNew), !,
  shiftWNums(Cat, T, Shift).

filter1(_Wnum, _Cat, X, X, Y, Y) :- !.	% gelijk gebleven
filter1(WNum, Cat, _, onbekend, _, _) :-
  w2(WNum,Cat,Tijd,Gew1,Gew2,Fixeer,VTijd,Bn),
  logf('R', w2(WNum,Cat,Tijd,Gew1,Gew2,Fixeer,VTijd,Bn),ja),
  fail.
filter1(WNum, Cat, _, onbekend, _, _) :-
  w3(WNum, Cat, TgWin, P1, P2, Stat, Stamp), 
  logf('R', w3(WNum, Cat, TgWin, P1, P2, Stat, Stamp),ja),
  fail.
filter1(WNum, Cat, _, onbekend, _, _) :-
  ws(WNum, Cat, TgWin, Stamp), 
  logf('R', ws(WNum, Cat, TgWin, Stamp),ja),
  fail.
filter1(WNum, Cat, _, onbekend, _, _) :-
  wd(Wnum,Cat,T1,T2, U, D, N), !,
  logf('R', wd(Wnum,Cat,T1,T2, U, D, N),ja). 
filter1(WNum, Cat, _, _, _, onbekend) :-
  w2(WNum,Cat,Tijd,Gew1,Gew2,Fixeer,VTijd,Bn),
  logf('R', w2(WNum,Cat,Tijd,Gew1,Gew2,Fixeer,VTijd,Bn),ja),
  fail.
filter1(WNum, Cat, _, _, _, onbekend) :-
  w3(WNum, Cat, TgWin, P1, P2, Stat, Stamp), 
  logf('R', w3(WNum, Cat, TgWin, P1, P2, Stat, Stamp),ja),
  fail.
filter1(WNum, Cat, _, _, _, onbekend) :-
  ws(WNum, Cat, TgWin, Stamp), 
  logf('R', ws(WNum, Cat, TgWin, Stamp),ja),
  fail.
filter1(WNum, Cat, _, _, _, onbekend) :-
  wd(Wnum,Cat,T1,T2, U, D, N), !,
  logf('R', wd(Wnum,Cat,T1,T2, U, D, N),ja). 
filter1(WNum, Cat, _, Tg1, _, Tg2) :-  
  %opg(TN1,Cat,Tg1,_,_,_,_,_),  
  %opg(TN2,Cat,Tg2,_,_,_,_,_),  
  wd(Wnum,Cat,_T1,_T2, U, D, N), !,
  logf('Z',wd(Wnum,Cat,Tg1,Tg2, U, D, N),ja).
  %w3??????????

  
savePoel(Win, Cat) :-
  close_poules(),
  volgorde(Win, Orde),
  member(Tgnst, Orde),
  savePoel(Win, Cat, Tgnst, Orde),
  fail.					% open plaatsen hoeven geen opgave!
savePoel(Win, _Cat) :-
  retract(deelnr(Win, Tgnst)),		% moet uitbreiding zijn
  %opgevenAlsOpenPlaats(Cat, OpgNo, OpgNox1),
  assertz(deelnrNw(Win, onbekend, Tgnst)),% vervangt geen oude
  fail.
savePoel(Win, Cat) :-
  wd(Wnum,Cat,Tg1,Tg2,_U,_D,_),		% ga alle wedstrijden langs
  %opg(OpgNum1,Cat,Tg1,_,_,_,_,_),  
  deelnrNw(Win, Tg1, TN1),
  %opg(OpgNum2,Cat,Tg2,_,_,_,_,_),  
  deelnrNw(Win, Tg2, TN2),
  filter1(WNum, Cat, Tg1, TN1, Tg2, TN2),
  fail.
savePoel(Win, Cat) :-			% vul aan met nieuwelingen
  deelnrNw(Win, onbekend, Tg), 
  %opg(OpgNo,Cat,Tg,_,_,_,_,_),  
  findall(Wn, wd(Wn,Cat,_,_,_,_,_), WnList),
  sortint_c(WnList),
  reverse(WnList, [], WListRev),
  WListRev = [Hoogste|_],		% bepaal hoogste wedstrijdnummer om mee te beginnen
  %laatste(WNList, Hoogste),
  findall(TgD, bestaandeDeelnr(Win, TgD), TgDList),
  voegtoe(Cat, Tg, TgDList, Hoogste),
  retract(deelnrNw(Win, onbekend, Tg)),
  assert(deelnrNw(Win, bye, Tg)),	% uitbreiding, maar verwerkt bye = nul
  fail.
savePoel(Win, Cat) :-
  kopl(CatV, rang(Nr), Cat, plaats(Cursor)), 
  not(t_kopl(Win, CatV, rang(Nr), Cat, plaats(Cursor))),
  logf('R',kopl(CatV, rang(Nr), Cat, plaats(Cursor)),ja), 
  fail.
savePoel(Win, Cat) :-
  t_kopl(Win, CatV, rang(Nr), Cat, plaats(Cursor)),
  not(kopl(CatV, rang(Nr), Cat, plaats(Cursor))), 
  retract(t_kopl(Win, CatV, rang(Nr), Cat, plaats(Cursor))),
  logf('A', kopl(CatV, rang(Nr), Cat, plaats(Cursor)),ja), 
  fail.
savePoel(Win, Cat ) :-
  retract(gegadigde(Win, Tgst, b_True, 0)),			% opgave wissen
  opg(TN1,Cat,Tgst,Pl,Ronde,Uitsl,Nr,Str,TID,Stat,MT),  
  logf('R', opg(TN1,Cat,Tgst,Pl,Ronde,Uitsl,Nr,Str,TID,Stat,MT), ja),
  fail.
savePoel(Win, Cat) :-
  retract(gegadigde(Win, Tgst, b_True, CatNew)),			% opgave verhuis
  opg(TN1,Cat,Tgst,Pl,Ronde,Uitsl,Nr,Str,TID,Stat,MT),  
  logf('R', opg(TN1,Cat,Tgst,Pl,Ronde,Uitsl,Nr,Str,TID,Stat,MT), ja),
  getbacktrack(Here),
  numb(N),     
  OpgN = TN1 + N - 1,
  not(opg(OpgN,CatNew,_,_,_,_,_,_,_,_,_)),
  cutbacktrack(Here),
  logf('Z', opg(OpgN,CatNew,Tgst,0,0,0,Nr,"","",Stat,""), ja),
  fail.
savePoel(_Win, Cat) :-
  retractall(t_kopl(_, _, _, _, _)),
  %verwijderOpenPlaats(Cat),
  compressWNums(Cat),
  adjustVoorDos(Cat),			% misschien te ver uitgegroeid
  logclose(),
  wd(Wnum,Cat,_Tg1,_Tg2,_U,_D,_),		% willekeurige bestaande wedstrijd
  select(Cat, Wnum, 0, 1, onbekend), !.
  
savePoel(Win, _Cat, Tgst, _) :-
  retract(deelnr(Win, Tgst)),		% gelijk gebleven
  assertz(deelnrNw(Win, Tgst, Tgst)), !.
savePoel(Win, _Cat, Tgst, Orde) :-
  gegadigde(Win, Tgst, _, _),			% is nu gegadigde geworden
  deelnr(Win, Tgx),
  not(member(Tgx, Orde)),	% vervang door nieuweling als die er is
  retract(deelnr(Win, Tgx)),
  %opgevenAlsOpenPlaats(Cat, OpgNox, OpgNox1),
  assertz(deelnrNw(Win, Tgst, Tgx)), !.
savePoel(Win, _Cat, Tgst, _Orde) :-
  assertz(deelnrNw(Win, Tgst, onbekend)).	% nieuwen zijn op, oude weg

pouleVeranderd(Win, _Lijst) :-
  gegadigde(Win, _, b_True, _), !.
pouleVeranderd(Win, Lijst) :-
  member(Tgst, Lijst),
  not(deelnr(Win, Tgst)), !.		% verminderd
pouleVeranderd(Win, Lijst) :-
  deelnr(Win, Tgst),
  not(member(Tgst, Lijst)), !.		% nieuwe
pouleVeranderd(Win, _Lijst) :-
  poelond(Win, Cat),
  t_kopl(Win, CatV, rang(Nr), Cat, plaats(Cursor)),
  not(kopl(CatV, rang(Nr), Cat, plaats(Cursor))), !.
pouleVeranderd(Win, _Lijst) :-
  poelond(Win, Cat),
  kopl(CatV, rang(Nr), Cat, plaats(Cursor)), 
  not(t_kopl(Win, CatV, rang(Nr), Cat, plaats(Cursor))), !.
  
nietEcht(Cat) :-			% kijk of er wedstrijden voorkomen die er niet horen
  wd(_Nox,Cat,bye,_,_,_,_), !.
nietEcht(Cat) :-
  wd(_Nox,Cat,_,bye,_,_,_), !.
nietEcht(Cat) :-
  wd(_WNum,Cat,_T1,T2,_U,_D, _), 
  not(T2 = openplaats(_)),
  not(opg(_,Cat,T2,_,_,_,_,_,_,_,_)),  !.
nietEcht(Cat) :-
  wd(_WNum,Cat,T1,_T2,_U,_D, _), 
  not(T1 = openplaats(_)),
  not(opg(_,Cat,T1,_,_,_,_,_,_,_,_)),  !.
  
oplappen(Cat) :-			% lap een verstierde poule op
  wd(WNum,Cat,_T1,T2,_U,_D, _), 
  not(opg(_,Cat,T2,_,_,_,_,_,_,_,_)),  
  filter1(WNum, Cat, bye, onbekend, bye, bye),	% gebruik filter om op te ruimen
  %logf('R', wd(WNum,Cat,T1,T2,U,D),ja),
  fail.
oplappen(Cat) :-
  wd(WNum,Cat,T1,_T2,_U,_D, _), 
  not(opg(_,Cat,T1,_,_,_,_,_,_,_,_)),  
  filter1(WNum, Cat, bye, onbekend, bye, bye),
  %logf('R', wd(WNum,Cat,T1,T2,U,D),ja),
  fail.
oplappen(Cat) :-
  wd(WNum,Cat,bye,_T2,_U,_D, _), 
  filter1(WNum, Cat, bye, onbekend, bye, bye),
  %logf('R', wd(WNum,Cat,bye,T2,U,D),ja),
  fail.
oplappen(Cat) :-
  wd(WNum,Cat,_T1,bye,_U,_D, _), 
  filter1(WNum, Cat, bye, onbekend, bye, bye),
  %logf('R', wd(WNum,Cat,T1,bye,U,D),ja),
  fail.
oplappen(Cat) :-
  compressWnums(Cat),
  logclose().

checkPoule(Cat) :-
  nietEcht(Cat), !,
  A = dlg_MessageBox( "Poule", "structuur oplappen?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
  A = 1, 
  oplappen(Cat).
checkPoule(_).

boekDeelnemer(Win, T) :-
  %opg(OpgNum1,Cat,T,_,_,_,_,_),  
  not(deelnr(Win,T)),
  assertz(deelnr(Win, T)), !.
boekDeelnemer(_Win, _T).

copyDeelnr(Win, Cat) :-
  findall(Nox,wd(Nox,Cat,_,_,_,_,_), Nums),
  sortint_c(Nums),
  member(No, Nums),
  wd(No,Cat,T1,T2,_,_,_),
  boekDeelnemer(Win, T1),
  boekDeelnemer(Win, T2),
  fail. 
copyDeelnr(Win, _) :-
  findall(Tgnst, deelnr(Win, Tgnst), TgL), 
  assert(volgorde(Win, TgL)),
  fail. 
copyDeelNr(Win, Cat) :-
  kopl(CatV, rang(Nr), Cat, plaats(Cursor)), 
  assert(t_kopl(Win, CatV, rang(Nr), Cat, plaats(Cursor))),
  fail.
copyDeelNr(_Win, _Cat).

deelnrNaam(Win, Naam, Tgnst) :-
  poelOnd(Win, Cat),
  deelnr(Win,Tgnst),
  t_tgnst_naam(Win, Cat, Tgnst, Naam).

t_kopl_ext(CatV, Rang, Cat, Plaats) :-
  t_kopl(_Window, CatV, Rang, Cat, Plaats), !.

poulOnd_ext(Win, Cat) :-
  poelond(Win, Cat).

t_tgnst_naam(Window, Cat, openplaats(I), Text) :-
  t_kopl(Window, CatV, rang(Nr), Cat, plaats(I)),
  wc(CatV, _, _, CatS, _, _, _, _, _, _, _, _, _, _,_), !,
  %wc(CatV,CatS,_,_,_,_,_), !,
  format(Text, "Nr %u van %s", Nr, CatS).
t_tgnst_naam(_, _, s(SpNo),Naam) :-
  sp2(SpNo,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !.
t_tgnst_naam(_, _, pw(SpNo),Naam) :-
  sp2(SpNo,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !.
t_tgnst_naam(_, _, p(Sp1,Sp2),Text) :-
  sp2(Sp1,_,N1,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  sp2(Sp2,_,N2,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  format(Text, "%+%", N1, N2), !.
t_tgnst_naam(_, _CatT, openplaats(I), Text) :- 
  format(Text, "Open plaats %u", I), !.
t_tgnst_naam(_, _CatT, _, "??"). 

copyGegadigde(Win, Cat) :-
  opg(_OpgNum,Cat,T,_,_,_,_,_,_,_,_),  
  not(deelnr(Win, T)),
  assert(gegadigde(Win, T, b_false, Cat)),
  fail.
copyGegadigde(_, _).

gegadigdeNaam(Win, Naam1, Tgnst) :-
  poelOnd(Win, Cat),
  gegadigde(Win,Tgnst, Verhuisd, CatN),
  t_tgnst_naam(Win, Cat, Tgnst, Naam),
  verhuisd(Verhuisd, CatN, Naam, Naam1).  
gegadigdeNaam(_Win, "Open plaats", onbekend).

verhuisd(b_True, Cat, Naam, Naam1) :-
  wc(Cat, _, _, _, _, Kort, _, _, _, _, _, _, _, _,_),
  %wc(Cat,_,_,Kort,_,_,_),
  format(Naam1, "%s ==> %s", Naam, Kort), !.
verhuisd(b_True, 0, Naam, Naam1) :-
  format(Naam1, "%s ==> xxx!", Naam), !.
verhuisd(_, _, Naam, Naam).

tgnst2Plaats(openplaats(I), plaats(I)) :- !.
tgnst2Plaats(_, algemeen).

checkEnableVerhuisWis(Win) :-
  VWin = win_GetCtlHandle(Win, idcp_verhuizen),
  win_SetState(VWin, [wsf_disabled]),
  WWin = win_GetCtlHandle(Win, idcp_wissen),
  win_SetState(WWin, [wsf_disabled]),
  GWin = win_GetCtlHandle(Win, idcp_gegadigden),
  lbox_GetSel(GWin,ItemList,_IndexList),
  ItemList = [Item | _],
  gegadigdeNaam(Win, Naamx, Tgnst),
  not(Tgnst = onbekend),
  Item = Naamx,
  win_SetState(VWin, [wsf_enabled]),
  win_SetState(WWin, [wsf_enabled]), !.
checkEnableVerhuisWis(_Win).
  
  dlg_deel_in_poule_Create(Parent, Onderdeel):-
	checkPoule(Onderdeel),
	Parm = cast(long, Onderdeel),
	win_CreateDynDialog(Parent,
		[
%BEGIN Deel in poule, WinDefList, 22:07:13-27.10.2011, Code automatically updated!
		 dlg_font(wdef(dlg_deel_in_poule_DlgType,dlg_deel_in_poule_RCT,dlg_deel_in_poule_Title,u_DlgBase),
		 	  dlg_deel_in_poule_Font,dlg_deel_in_poule_FSize,dlg_deel_in_poule_Flags),
		 ctl(wdef(wc_PushButton,rct(242,139,274,161),"&OK",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(204,139,236,149),"&Cancel",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(204,151,236,161),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_LBox,rct(1,20,131,149),"",u_DlgBase),idcp_deelnemers,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight]),
		 ctl(wdef(wc_PushButton,rct(133,48,163,60),"<<< &Bij",u_DlgBase),idcp_bij,[wsf_Group,wsf_TabStop,wsf_Disabled]),
		 ctl(wdef(wc_PushButton,rct(133,66,163,78),"&Af >>>",u_DlgBase),idcp_af,[wsf_Group,wsf_TabStop,wsf_Disabled]),
		 ctl(wdef(wc_LBox,rct(164,20,274,117),"",u_DlgBase),idcp_gegadigden,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight]),
		 ctl(wdef(wc_PushButton,rct(76,149,131,161),"&Koppelen",u_DlgBase),idcp_koppelen,[wsf_Group,wsf_TabStop,wsf_Disabled]),
		 ctl(wdef(wc_PushButton,rct(2,149,57,161),"&Ontkoppelen",u_DlgBase),idcp_ontkoppelen,[wsf_Group,wsf_TabStop,wsf_Disabled]),
		 ctl(wdef(wc_PushButton,rct(164,117,212,129),"&Verhuizen",u_DlgBase),idcp_verhuizen,[wsf_Group,wsf_TabStop,wsf_Disabled]),
		 ctl(wdef(wc_PushButton,rct(226,117,274,129),"&Wissen",u_DlgBase),idcp_wissen,[wsf_Group,wsf_TabStop,wsf_Disabled]),
		 ctl(wdef(wc_Text,rct(37,10,85,19),"Opgesteld",u_DlgBase),idctp_deelnemers,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(197,10,245,19),"Gegadigden",u_DlgBase),idcp_optestellentit,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(64,0,189,10),"Poulenaam",u_DlgBase),idctp_poulenaam,[wsf_AlignCenter])
%END Deel in poule, WinDefList
		],dlg_deel_in_poule_eh,Parm).

%BEGIN Deel in poule, idc_ok _CtlInfo
  dlg_deel_in_poule_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	volgorde(_Win, Lijst),
	pouleVeranderd(_Win, Lijst),
	%write("\nVeranderd"),
	poelOnd(_Win, Onderdeel),
	savePoel(_Win, Onderdeel),
	logclose(),
	win_Destroy(_Win),
	!.
  dlg_deel_in_poule_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	win_Destroy(_Win),
	!.
%END Deel in poule, idc_ok _CtlInfo
%MARK Deel in poule, new events

%BEGIN Deel in poule, idcp_wissen _CtlInfo
  dlg_deel_in_poule_eh(_Win,e_Control(idcp_wissen,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	GWin = win_GetCtlHandle(_Win, idcp_gegadigden),
	lbox_GetSel(GWin,ItemList,_IndexList),
	ItemList = [Item | _],
	gegadigdeNaam(_Win, Naamx, Tgnst),
	not(Tgnst = onbekend),
	Item = Naamx,
	retract(gegadigde(_Win, Tgnst, _, _)),
	assertz(gegadigde(_Win, Tgnst, b_True, 0)), 
	win_PostEvent(_Win,e_User(1,0)),
	AWin = win_GetCtlHandle(_Win, idcp_bij),
	win_SetState(AWin, [wsf_disabled]),
	!.
%END Deel in poule, idcp_wissen _CtlInfo

%BEGIN Deel in poule, idcp_verhuizen _CtlInfo
  dlg_deel_in_poule_eh(_Win,e_Control(idcp_verhuizen,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	GWin = win_GetCtlHandle(_Win, idcp_gegadigden),
	lbox_GetSel(GWin,ItemList,_IndexList),
	ItemList = [Item | _],
	gegadigdeNaam(_Win, Naamx, Tgnst),
	not(Tgnst = onbekend),
	Item = Naamx,
	poelOnd(_Win, Cat),
	personen(Tgnst, Personen, SpCatL),
	findall(CatO, passende_categorie(Cat, CatO, Personen, SpCatL), P_Cats),
	%TaskWin = vpi_GetTaskWin(),
	%write("\nKan verhuizen naar: "),
	dlg_onderdeel_select_Create(_Win, "Te verhuizen naar:", P_Cats, 0, Selectie, _),
	win_SetFocus(_Win),
	Selectie = [CatS|_],
	wc(CatNew, _, _, CatS, _, _, _, _, _, _, _, _, _, _,_),
	%wc(CatNew,CatS,_,_,_,_,_),
	retract(gegadigde(_Win, Tgnst, _, _)),
	assertz(gegadigde(_Win, Tgnst, b_True, CatNew)), 
	AWin = win_GetCtlHandle(_Win, idcp_bij),
	win_SetState(AWin, [wsf_disabled]),
	win_PostEvent(_Win,e_User(1,0)),
	!.
%END Deel in poule, idcp_verhuizen _CtlInfo

%BEGIN Deel in poule, idcp_ontkoppelen _CtlInfo
  dlg_deel_in_poule_eh(_Win,e_Control(idcp_ontkoppelen,_CtrlType,_CtrlWin,_CtlInfo),0):-
	dialog_SetState(_Win, [enable(idcp_ontkoppelen,b_False)]),
	DWin = win_GetCtlHandle(_Win, idcp_deelnemers),
	lbox_GetSel(DWin,List,_),
	List = [Item| _],
	deelnrNaam(_Win, Naamx, openplaats(X)),
	Item = Naamx,
	poelOnd(_Win, Cat2),
	t_kopl(_Win, Cat1, Vanaf, Cat2, plaats(X)), !,
	retract(t_kopl(_Win, Cat1, Vanaf, Cat2, plaats(X))),
	win_PostEvent(_Win,e_User(1,0)),
	!.
%END Deel in poule, idcp_ontkoppelen _CtlInfo


%BEGIN Deel in poule, idcp_koppelen _CtlInfo
  dlg_deel_in_poule_eh(_Win,e_Control(idcp_koppelen,_CtrlType,_CtrlWin,_CtlInfo),0):-
	DWin = win_GetCtlHandle(_Win, idcp_deelnemers),
	lbox_GetSel(DWin,List,_),
	List = [Item| _],
	poelOnd(_Win, Onderdeel),
	str_len("Open plaats ", Len),
	frontstr(Len,Item,_,RestString),
	fronttoken(Reststring, Token, _),
	str_int(Token, Cursor),
	selecteerKoppelBron(Onderdeel, Cursor, Rang, CatVan),
	%logf('A', kopl(CatVan, rang(Rang), Onderdeel, plaats(Cursor)),ja), 
        assert(t_kopl(_Win, CatVan, Rang, Onderdeel, plaats(Cursor))),
	win_PostEvent(_Win,e_User(1,0)),
	dialog_SetState(_Win, [enable(idcp_koppelen,b_False)]),
	%logclose(),
	!.
  dlg_deel_in_poule_eh(_Win,e_Control(idcp_koppelen,_CtrlType,_CtrlWin,_CtlInfo),0):-
	DWin = win_GetCtlHandle(_Win, idcp_deelnemers),
	lbox_GetSel(DWin,List,_),
	List = [Item| _],
	poelOnd(_Win, Onderdeel),
	str_len("Nr ", Len),
	frontstr(Len,Item,_,RestString),
	fronttoken(Reststring, Token, _),
	str_int(Token, Cursor),
	selecteerKoppelBron(Onderdeel, Cursor, Rang, CatVan),
	%logf('A', kopl(CatVan, rang(Rang), Onderdeel, plaats(Cursor)),ja), 
        assert(t_kopl(_Win, CatVan, Rang, Onderdeel, plaats(Cursor))),
	win_PostEvent(_Win,e_User(1,0)),
	dialog_SetState(_Win, [enable(idcp_koppelen,b_False)]),
	%logclose(),
	!.
%END Deel in poule, idcp_koppelen _CtlInfo

%BEGIN Deel in poule, e_User
  dlg_deel_in_poule_eh(_Win,e_User(1,_Ptr),0):-!,
	poelOnd(_Win, Onderdeel),
	wc(Onderdeel, _, _, Kop, _, _, _, _, _, _, _, _, _, _,_),
	%wc(Onderdeel,Kop,_,_,_,_,_),
	KWin = win_GetCtlHandle(_Win, idctp_poulenaam),
	win_SetText(KWin, Kop), 
	findall(DeelNemer,deelnrNaam(_Win, Deelnemer, _), Deelnrs),
	DWin = win_GetCtlHandle(_Win, idcp_deelnemers), 
	lbox_Clear(DWin),
	lbox_Add(DWin, Deelnrs),
	findall(Gegadigde,gegadigdeNaam(_Win, Gegadigde, _), Gegadigden),
	GWin = win_GetCtlHandle(_Win, idcp_gegadigden), 
	lbox_Clear(GWin),
	lbox_Add(GWin, Gegadigden),
	!.
%END Deel in poule, e_User

%BEGIN Deel in poule, e_CloseRequest
  dlg_deel_in_poule_eh(_Win,e_CloseRequest,0):-!,
	volgorde(_Win, Lijst),
	pouleVeranderd(_Win, Lijst),
	Ans = dlg_Ask("","Veranderingen opslaan?",["&Ja","&Nee"]),
	Ans = resp_default,
	poelOnd(_Win, Onderdeel),
	savePoel(_Win, Onderdeel),
	logclose(), 
	!.
%END Deel in poule, e_CloseRequest

%BEGIN Deel in poule, idc_help _CtlInfo
  dlg_deel_in_poule_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Pouleaanpassing"),
	!.
%END Deel in poule, idc_help _CtlInfo

%BEGIN Deel in poule, idcp_gegadigden selchanged
  dlg_deel_in_poule_eh(_Win,e_Control(idcp_gegadigden,_CtrlType,GWin,selchanged),0):-
	checkEnableVerhuisWis(_Win),	
	NoOfItems = lbox_CountAll(GWin),
	NoOfItems <= maxpoeldim,
	dialog_SetState(_Win, [enable(idcp_bij,b_True)]),
	!.
%END Deel in poule, idcp_gegadigden selchanged

%BEGIN Deel in poule, idcp_deelnemers selchanged
  dlg_deel_in_poule_eh(_Win,e_Control(idcp_deelnemers,_CtrlType,DWin,selchanged),0):-
	dialog_SetState(_Win, [enable(idcp_koppelen,b_False)]),
	lbox_GetSel(DWin,List,_),
	List = [Item| _],
	searchstring(Item, "pen plaats",_FoundPos),
	dialog_SetState(_Win, [enable(idcp_koppelen,b_True)]),
	fail.
  dlg_deel_in_poule_eh(_Win,e_Control(idcp_deelnemers,_CtrlType,DWin,selchanged),0):-
	NoOfItems = lbox_CountAll(DWin),
	NoOfItems > 2,
	dialog_SetState(_Win, [enable(idcp_af,b_True)]),
	fail.
  dlg_deel_in_poule_eh(_Win,e_Control(idcp_deelnemers,_CtrlType,DWin,selchanged),0):-
	dialog_SetState(_Win, [enable(idcp_ontkoppelen,b_False)]),
	lbox_GetSel(DWin,List,_),
	List = [Item| _],
	searchstring(Item, "Nr ",_FoundPos),
	dialog_SetState(_Win, [enable(idcp_ontkoppelen,b_True)]),
	!.
%END Deel in poule, idcp_deelnemers selchanged


%BEGIN Deel in poule, idcp_af _CtlInfo
  dlg_deel_in_poule_eh(_Win,e_Control(idcp_af,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	DWin = win_GetCtlHandle(_Win, idcp_deelnemers), 
	GWin = win_GetCtlHandle(_Win, idcp_gegadigden), 
	lbox_GetSel(DWin,ItemList,IndexList),
	ItemList = [Naam|_],
	deelnrNaam(_Win, Naamx, Tgnst),
	Naam = Naamx,
	retract(deelnr(_Win,Tgnst)),
	poelOnd(_Win, Onderdeel),
	tgnst2Plaats(Tgnst, Plaats),
	retractall(t_kopl(_Win, _, _, Onderdeel, Plaats)),
	assert(gegadigde(_Win, Tgnst, b_False, 0)),	
	IndexList = [Index|_],
	lbox_Delete(DWin, Index), 
	t_tgnst_naam(_Win, Onderdeel, Tgnst, Text),
	lbox_Add(GWin, Text),
	checkEnableVerhuisWis(_Win),	
	dialog_SetState(_Win, [enable(idcp_af,b_False)]),
	!.
%END Deel in poule, idcp_af _CtlInfo

%BEGIN Deel in poule, idcp_bij _CtlInfo
  dlg_deel_in_poule_eh(_Win,e_Control(idcp_bij,_CtrlType,_CtrlWin,_CtlInfo),0):-
	GWin = win_GetCtlHandle(_Win, idcp_gegadigden), 
	lbox_GetSel(GWin,ItemList,_IndexList),
	ItemList = [Naam|_],
	gegadigdeNaam(_Win, Naamx, Gegadigde),
	Naam = Naamx,
	Gegadigde = onbekend,
	getbacktrack(Here),
	numb(X),
	not(deelnr(_Win, openplaats(X))),
	cutbacktrack(Here),
	assert(deelnr(_Win, openplaats(X))),	
	win_PostEvent(_Win,e_User(1,0)),
	dialog_SetState(_Win, [enable(idcp_bij,b_False)]),
	!.
  dlg_deel_in_poule_eh(_Win,e_Control(idcp_bij,_CtrlType,_CtrlWin,_CtlInfo),0):-
	GWin = win_GetCtlHandle(_Win, idcp_gegadigden), 
	lbox_GetSel(GWin,ItemList,_IndexList),
	ItemList = [Naam|_],
	gegadigdeNaam(_Win, Naamx, Gegadigde),
	Naam = Naamx,
	%Gegadigde = opg(OpgNum),
	retract(gegadigde(_Win,Gegadigde,_,_)),
	assert(deelnr(_Win, Gegadigde)),	
	win_PostEvent(_Win,e_User(1,0)),
	checkEnableVerhuisWis(_Win),	
	dialog_SetState(_Win, [enable(idcp_bij,b_False)]),
	!.
%END Deel in poule, idcp_bij _CtlInfo

%BEGIN Deel in poule, idc_cancel _CtlInfo
  dlg_deel_in_poule_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-
	volgorde(_Win, Lijst),
	pouleVeranderd(_Win, Lijst),
	Ans = dlg_Ask("","Veranderingen opslaan?",["&Ja","&Nee"]),
	Ans = resp_default,
	poelOnd(_Win, Onderdeel),
	savePoel(_Win, Onderdeel),
	logclose(), 
	% als idc_ok
	win_Destroy(_Win),
	!.
  dlg_deel_in_poule_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	win_Destroy(_Win),
	!.
%END Deel in poule, idc_cancel _CtlInfo

%BEGIN Deel in poule, e_Destroy
  dlg_deel_in_poule_eh(_Win,e_Destroy,0):-!,
	retractall(poelOnd(_Win, _)),
	retractall(deelnr(_Win, _)),
	retractall(deelnrNw(_Win, _, _)),
	retractall(gegadigde(_Win, _, _, _)),
	retractall(volgorde(_Win, _)),
	retractall(t_kopl(_Win, _, _, _, _)),
	!.
%END Deel in poule, e_Destroy

%BEGIN Deel in poule, e_Create
  dlg_deel_in_poule_eh(_Win,e_Create(_CreationData),0):-!,
	Onderdeel = cast(wedscat, _CreationData),
	assert(poelOnd(_Win, Onderdeel)),
	copyDeelnr(_Win, Onderdeel),
	copyGegadigde(_Win, Onderdeel),
	win_PostEvent(_Win,e_User(1,0)),
	!.
%END Deel in poule, e_Create

  dlg_deel_in_poule_eh(_,_,_):-!,fail.

%END_DLG Deel in poule


%BEGIN_DLG Compress
/**************************************************************************
	Creation and event handling for dialog: Compress
**************************************************************************/

constants

%BEGIN Compress, CreateParms, 22:05:33-27.10.2011, Code automatically updated!
  dlg_compress_ResID = idd_compress
  dlg_compress_DlgType = wd_Modal
  dlg_compress_Help = idh_adresetiketten
%END Compress, CreateParms

database - compr
  single updrepl(uploadtype)

predicates

  dlg_compress_eh : EHANDLER
  dlg_compress_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_compress_update(DIALOG_VAL_LIST)
  compress1(WINDOW Progress)
  %check_naw(offset, nax, nax)
  %count_changes(WINDOW Progress, integer, integer Max)

clauses

updrepl(update).

compress(UpdateReplace) :-
  loglock(lees, "Compress"),
  logclose(),
  not(alleenlezen1),
  writeToJournaal("compress heeft plaats gevonden!"),
  assert(updrepl(UpdateReplace)),
  getbacktrack(Here),
  spsrt(_, _, _, _, _, _, _, _),
  cutbacktrack(Here),
  stopDBrefresh(),
  TaskWin = vpi_GetTaskWin(),
  dlg_compress_Create(TaskWin), !.
compress(_) :-
  writetojournaal("compress niet mogelijk: alleen lezen!",ja).

compress1(PWin) :-
  get_toernooidir(_Dir, _Map, MapPlaats, TB),
  readdevice(keyboard),
  progress_bar_set_value(PWin, 100, 50),
  vpi_ProcessEvents(b_True ),
  %maakRW(Fctfile),
  date(Year,Month,Day),
  time(Hours,Minutes,Seconds,_Hundredths),
  format(Timestamp, "%u-%u-%u %u:%u:%u", Year, Month, Day, Hours, Minutes, Seconds),
  retractall(webdatpositie(_, _, _)),
  assert(webdatpositie(Timestamp, 0, 0)),
  save(TB), 
  del_log(TB),
  updrepl(UpdateReplace),
  copyNaarWebIf("obsolete", MapPlaats, UpdateReplace), % webdatpos wordt verwijderd als er iets misgegaan is bij de upload
  progress_bar_set_value(PWin, 100, 100),
  vpi_ProcessEvents(b_True ), !.

  dlg_compress_Create(Parent):-
%MARK Compress, new variables
	dialog_CreateModal(Parent,dlg_compress_ResID,"",
  		[
%BEGIN Compress, ControlList, 22:05:33-27.10.2011, Code automatically updated!
%END Compress, ControlList
		],
		dlg_compress_eh,0,VALLIST,ANSWER),
	dlg_compress_handle_answer(ANSWER,VALLIST).

  dlg_compress_handle_answer(idc_ok,VALLIST):-!,
	dlg_compress_update(VALLIST).
  dlg_compress_handle_answer(idc_cancel,_):-!.  % Handle Esc and Cancel here
  dlg_compress_handle_answer(_,_):-
	errorexit().

  dlg_compress_update(_VALLIST):-
%BEGIN Compress, Update controls, 22:05:33-27.10.2011, Code automatically updated!
%END Compress, Update controls
	true.

%MARK Compress, new events

%BEGIN Compress, e_User
  dlg_compress_eh(_Win,e_User(1,_Ptr),0):-!,
	PWin = win_GetCtlHandle(_Win, idc_progress),
	compress1(PWin),
	trap(win_Destroy(_Win),_,fail),
	!.
%END Compress, e_User

%BEGIN Compress, e_Create
  dlg_compress_eh(_Win,e_Create(_CreationData),0):-!,
	PWin = win_GetCtlHandle(_Win, idc_progress),
	progress_bar_set_value(PWin, 0, 0),
	win_PostEvent(_Win,e_User(1,0)),
	!.
%END Compress, e_Create

  dlg_compress_eh(_,_,_):-!,fail.

%END_DLG Compress




%BEGIN_DLG Toernooiinstellingen
/**************************************************************************
	Creation and event handling for dialog: Toernooiinstellingen
**************************************************************************/

constants

%BEGIN Toernooiinstellingen, CreateParms, 20:03:36-7.12.2014, Code automatically updated!
  dlg_toernooiinstellingen_ResID = idd_toernooiinstellingen
  dlg_toernooiinstellingen_DlgType = wd_Modal
  dlg_toernooiinstellingen_Help = idh_extra_instellingen
%END Toernooiinstellingen, CreateParms
zetfocus = 23

database - toerinst
single focusplaats(ilist)

predicates

  dlg_toernooiinstellingen_eh : EHANDLER
  dlg_toernooiinstellingen_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_toernooiinstellingen_update(DIALOG_VAL_LIST)
%  savePouleTel(BOOLEAN)
%procedure  saveOmdraaien(BOOLEAN) 
procedure  findSelect(slist, integer Grootte, integer In, integer Uit)
procedure  saveschemafontsize(string)
%metModem(BOOLEAN, WINDOW)
%procedure selectIndex(slist, string, integer, ilist IndexList)
%toon_puls(integer, BOOLEAN) 
%procedure formuliergezien(BOOLEAN, integer)
%procedure seconden(integer, integer)
procedure kladblokInst(BOOLEAN Enabled, integer Minuten)
procedure minuten(integer, integer)
%saveformuliergezien(BOOLEAN, integer)
saveKladblokZien(BOOLEAN, integer)
%procedure saveAndereSexe(boolean)
%procedure autobackup(boolean, string)
procedure getautodirectory(string) 
procedure roosterintervalIs(integer)
procedure saveroosterinterval(integer)

clauses


focusplaats([]).


findSelect([H|_], Grootte, In, In) :-
  str_int(H, Grootte), !.
findSelect([_|List], Grootte, In, Uit) :-
  In1 = In + 1, !,
  findSelect(List, Grootte, In1, Uit).
findSelect(_, _, _, 1).

saveschemafontsize(_LETTERGROOTTE_ITEMLIST) :-
  str_int(_LETTERGROOTTE_ITEMLIST,FontGrootte),
  not(schemafontsize(FontGrootte)),	% dan hoeft het niet
  retractall(wcdisp(_,_)),
  assert(schemafontsize(FontGrootte)),
  close_alles(),
  showfirst(), !.  
saveschemafontsize(_).

/*
selectIndex([_|Rest], "", In, [In|Uit]) :-
  In1 = In + 1, !,
  selectIndex(Rest, "", In1, Uit).
selectIndex([Comp|Rest], Comp, In, [In|Uit]) :-
  In1 = In + 1, !,
  selectIndex(Rest, Comp, In1, Uit).
selectIndex([_|Rest], Comp, In, Uit) :-
  In1 = In + 1, !,
  selectIndex(Rest, Comp, In1, Uit).
selectIndex(_, _, _, []).
*/

kladblokInst(Enabled, Minuten) :-
  kladblok(Enabled, Minuten), !.
kladblokInst(b_True, 60).

saveKladblokZien(Enabled, Minuten) :-
  kladblok(Enabled, Minuten), !.
saveKladblokZien(Enabled, Minuten) :-
  logf('Z', kladblok(Enabled, Minuten), nee), !.

%saveAndereSexe(Bool) :-
%  andereSexe(Bool), !.
%saveAndereSexe(Bool) :-
%  logf('Z', andereSexe(Bool), nee), !.
  
saveTshirtchecked(Bool, Tekst) :-
  tshirt(Bool, Tekst), !.
saveTshirtchecked(Bool, Tekst) :-
  logf('A', tshirt(Bool, Tekst), nee).  

minuten(10, 0) :- !.
minuten(30, 1) :- !.
minuten(_, 2).

autobackup(b_true, Pad1) :-
  api_getComputername(CN),
  upper_lower(PC, CN),
  autobak(PC, Pad), !,
  concat("Pad: ",Pad, Pad1).
autobackup(b_false, "niet geactiveerd").

getautodirectory(Pad) :-
  api_getComputername(CN),
  upper_lower(PC, CN),
  autobak(PC, Pad), !.
getautodirectory("").

roosterintervalIs(idc_15_minuten_rooster) :-
  roosterinterval(15), !.
roosterintervalIs(idc_12_minuten_rooster).

saveRoosterinterval(idc_15_minuten_rooster) :-
  roosterinterval(12), 
  logf('A', roosterinterval(15), nee), !.
saveRoosterinterval(idc_12_minuten_rooster) :-
  roosterinterval(15), 
  logf('A', roosterinterval(12), nee), !.
saveRoosterinterval(_) :- !.
  
  dlg_toernooiinstellingen_Create(Parent, Focus):-
	assert(focusplaats(Focus)),
	%andereSexe(IDC_GD_ANDERE_SEXE__CHECKED),
	IDC_GD_ANDERE_SEXE__CHECKED = b_True,
	LETTERGROOTTE_ITEMLIST = ["8","9","10","11","12","13"],
	schemaFontSize(Grootte),
	findSelect(LETTERGROOTTE_ITEMLIST, Grootte, 0, LETTERGROOTTE_SELECT),
	%formuliergezien(IDC_FORMULIER_CHECKED, Seconden),  % eerste keer melding in banentool
	%seconden(Seconden, _),
	kladblokInst(IDC_TOERNOOIINSTELLINGEN_KLADBLOK_CHECKED, Minuten),
	minuten(Minuten, IDC_TOERNOOIINSTELLINGEN_MINUTEN_SELECT),
	autobackup(IDC_AUTOBACKUP_CHECKED, IDCT_PAD_TITLE),
	%str_int(IDCT_NR_TITLE, Nr), 
	roosterintervalIs(IDC15),
	%tshirt(IDC_TSHIRTMELDING_CHECKED, IDC_TSIRTTEKST_VALUE),
%MARK Toernooiinstellingen, new variables

	dialog_CreateModal(Parent,dlg_toernooiinstellingen_ResID,"",
  		[
%BEGIN Toernooiinstellingen, ControlList, 20:03:36-7.12.2014, Code automatically updated!
		df(lettergrootte,listbutton(LETTERGROOTTE_ITEMLIST,LETTERGROOTTE_SELECT),nopr),
		df(idc_gd_andere_sexe_,checkbox(IDC_GD_ANDERE_SEXE__CHECKED),nopr),
		df(idc_toernooiinstellingen_kladblok,checkbox(IDC_TOERNOOIINSTELLINGEN_KLADBLOK_CHECKED),nopr),
		df(idc_toernooiinstellingen_minuten,listbutton(["10","30","60"],IDC_TOERNOOIINSTELLINGEN_MINUTEN_SELECT),nopr),
		df(idc_autobackup,checkbox(IDC_AUTOBACKUP_CHECKED),nopr),
		df(idct_pad,statictext(IDCT_PAD_TITLE),nopr),
		df(IDC15,radiobuttongroup([idc_15_minuten_rooster,idc_12_minuten_rooster]),nopr)
%END Toernooiinstellingen, ControlList
		],
		dlg_toernooiinstellingen_eh,0,VALLIST,ANSWER),
	dlg_toernooiinstellingen_handle_answer(ANSWER,VALLIST).

  dlg_toernooiinstellingen_handle_answer(idc_ok,VALLIST):-!,
	dlg_toernooiinstellingen_update(VALLIST).
  dlg_toernooiinstellingen_handle_answer(idc_cancel,_):-!.  % Handle Esc and Cancel here
  dlg_toernooiinstellingen_handle_answer(_,_):-
	errorexit().

  dlg_toernooiinstellingen_update(_VALLIST):-
%BEGIN Toernooiinstellingen, Update controls, 20:03:36-7.12.2014, Code automatically updated!
	dialog_VLGetListButton(lettergrootte,_VALLIST,_LETTERGROOTTE_ITEMLIST,_LETTERGROOTTE_SELECT),
	dialog_VLGetListButton(idc_toernooiinstellingen_minuten,_VALLIST,_IDC_TOERNOOIINSTELLINGEN_MINUTEN_ITEMLIST,_IDC_TOERNOOIINSTELLINGEN_MINUTEN_SELECT),
	_IDC_TOERNOOIINSTELLINGEN_KLADBLOK_CHECKED = dialog_VLGetCheck(idc_toernooiinstellingen_kladblok,_VALLIST),
	_IDC_GD_ANDERE_SEXE__CHECKED = dialog_VLGetCheck(idc_gd_andere_sexe_,_VALLIST),
	_IDC_AUTOBACKUP_CHECKED = dialog_VLGetCheck(idc_autobackup,_VALLIST),
	_IDC15 = dialog_VLGetRadiobutton(idc_15_minuten_rooster,_VALLIST),
%END Toernooiinstellingen, Update controls
	saveschemafontsize(_LETTERGROOTTE_ITEMLIST),
	%saveAndereSexe(_IDC_GD_ANDERE_SEXE__CHECKED),
	%str_int(_IDC_SECONDEN_ITEMLIST, SecI),
	%saveformuliergezien(_IDC_FORMULIER_CHECKED, 0),
	%saveTshirtchecked(_IDC_TSHIRTMELDING_CHECKED, _IDC_TSIRTTEKST_VALUE),
	str_int(_IDC_TOERNOOIINSTELLINGEN_MINUTEN_ITEMLIST, MinutenI),
	saveKladblokZien(_IDC_TOERNOOIINSTELLINGEN_KLADBLOK_CHECKED, MinutenI),
	saveRoosterinterval(_IDC15),
	true.

%MARK Toernooiinstellingen, new events

%BEGIN Toernooiinstellingen, idc_15_minuten_rooster _CtlInfo
  dlg_toernooiinstellingen_eh(_Win,e_Control(idc_15_minuten_rooster,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	_Answer = dlg_MessageBox( "Opgelet", "Verander het roosterinterval alleen bij nieuwe toernooien!\nMaak eerst een back-up voor de zekerheid!", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	!.
%END Toernooiinstellingen, idc_15_minuten_rooster _CtlInfo

%BEGIN Toernooiinstellingen, idc_12_minuten_rooster _CtlInfo
  dlg_toernooiinstellingen_eh(_Win,e_Control(idc_12_minuten_rooster,_CtrlType,_CtrlWin,_CtlInfo),0):-
	_Answer = dlg_MessageBox( "Opgelet", "Verander het roosterinterval alleen bij nieuwe toernooien!\nMaak eerst een back-up voor de zekerheid!", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	!.
%END Toernooiinstellingen, idc_12_minuten_rooster _CtlInfo

%BEGIN Toernooiinstellingen, idc_autobackup _CtlInfo
  dlg_toernooiinstellingen_eh(_Win,e_Control(idc_autobackup,_CtrlType,CtrlWin,_CtlInfo),0):-
	b_true = win_IsChecked(CtrlWin),
	Msg = "Selecteer in het volgende menu een auto-back-uppad.\nBij voorkeur op naar een map die snel terug te vinden is.\nMaak eerst zelf in Windows die map aan als hij nog niet bestaat!",
	dlg_MessageBox( "Intelligente auto-backup", Msg, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	getautodirectory(OldPath1),
	trap(existdir(OldPath1, OldPath),RetD,dir_error(RetD,b_True)),		% 15.6.2000 extra check
	WinT = vpi_GetTaskWin(),
	1 = dlg_GetDirectoryName(WinT,OldPath,NewPath),
	met_bslash(NewPath, Path1),
	api_getComputername(CN),
	upper_lower(PC, CN),
	logf('Z', autobak(PC, Path1), nee),
	PWin = win_GetCtlHandle(_Win, idct_pad),
	concat("Pad = ", Path1, Tekst),
	win_Settext(PWin, Tekst),
	!.
  dlg_toernooiinstellingen_eh(_Win,e_Control(idc_autobackup,_CtrlType,CtrlWin,_CtlInfo),0):-!,
	PWin = win_GetCtlHandle(_Win, idct_pad),
	win_Check(CtrlWin, b_False),
	win_Settext(PWin, "Niet geactiveerd."),
	api_getComputername(CN),
	upper_lower(PC, CN),
	autobak(PC, X),
	logf('R', autobak(PC, X), nee), !.
%END Toernooiinstellingen, idc_autobackup _CtlInfo

%BEGIN Toernooiinstellingen, e_User
  dlg_toernooiinstellingen_eh(_Win,e_User(zetfocus,_),0):-
	focusplaats([1]),
	CtrlWin = win_GetCtlHandle(_Win, idc_toernooiinstellingen_kladblok),
	win_SetFocus(CtrlWin),
	!.
%END Toernooiinstellingen, e_User

%BEGIN Toernooiinstellingen, idc_toernooiinstellingen_kladblok _CtlInfo
  dlg_toernooiinstellingen_eh(_Win,e_Control(idc_toernooiinstellingen_kladblok,_CtrlType,KladWin,_CtlInfo),0):-!,
	Enable = win_IsChecked(KladWin),
	dialog_SetState(_Win, [enable(idc_toernooiinstellingen_minuten, Enable)]),
	!.
%END Toernooiinstellingen, idc_toernooiinstellingen_kladblok _CtlInfo


%BEGIN Toernooiinstellingen, e_Create
  dlg_toernooiinstellingen_eh(_Win,e_Create(_CreationData),0):-!,
	%formuliergezien(Enable, _Tijd),
	%dialog_SetState(_Win, [enable(idc_seconden, Enable)]),
	kladblokInst(EnableK, _Minuten),
	dialog_SetState(_Win, [enable(idc_toernooiinstellingen_minuten, EnableK)]),
	win_PostEvent(_Win,e_user(zetfocus, 0)),
	%CtrlWin = win_GetCtlHandle(_Win, idc_modem_gebruiken),
	%Checked = win_IsChecked(CtrlWin),
	%metModem(Checked, _Win),
	!.
%END Toernooiinstellingen, e_Create

%BEGIN Toernooiinstellingen, idc_help _CtlInfo
  dlg_toernooiinstellingen_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Extrainstellingen"),
	!.
%END Toernooiinstellingen, idc_help _CtlInfo

  dlg_toernooiinstellingen_eh(_,_,_):-!,fail.

%END_DLG Toernooiinstellingen


%__________________________________ toon indeling ______________________


database - indeel 
  opg_data(integer OpgNum ,tgnst, string, integer, string St, integer Lft1, integer Lft2)
  determ leeftError()
  algeweestRating(tgnst)
database - indeelsingle
  %single preselected(string)
  single metRangLijst(boolean)
  verzam(string)

predicates

procedure bouw_lijst(wedscat,integer,integer,tgl)
nondeterm  weds(wedscat,integer)
procedure  scan_sterkte(wedscat, integer)
procedure scan_sterkteX(wedscat, string)
  aant_tekst(integer, string)
nondeterm  overblijvende_opgave(wedscat,tgnst, integer, string)
  rep2(integer, wedscat, tgnst)
  get_mv(sexe, char)
  get_plts(integer,string, string)
  get_pltsmethaakjes(integer,string)
  
  %tgnst_naam(tgnst, string) - (i,o)
  rapport(tgl,integer,wedscat, rsoort)
  rapportP(integerlist, wedscat)
  inIndeling(integer, tgl)
  deelnemers(integerlist, tgl, integerlist, integerlist)
  repPoel(integer, wedscat, integer LaatsteRegel)
nondeterm  leeftijden(integer)
procedure  leeftijdI(string, integer, boolean Msgs)
procedure  vastgesteld(string Text, integer L, integer H)
procedure dubbelsterkte(string S1, string S2, string D)
determ toon_rating(integer OpgNo, wedscat)
nondeterm teamIdRating(wedscat Cat, string Id)
nondeterm teamsOpVolgordeVanRating(slist, wedscat CatU, integer OpgNoU, integer Aantal)
nondeterm maakTable(wedstyp)
nondeterm maaktabel()
procedure getratingR(slist, string, string) - (i, o, o)
procedure keurTekstRat(teamstatus, string)
procedure isMetRangLijst(string, string, string)
procedure schrijfMetRangLijst(string, string, string)
procedure achteraanAls0(integer, integer)
procedure  get_uitsluit(integer, string, string CSSIn, string CSS)
procedure string uitslTekst1(integer)
procedure uitslMerk(string, integer, string)
procedure doetookMeeAan(wedscat CatZelf, integer Sp, string Tekst) 
nondeterm sp2opgDetails(integer Sp, wedscat CatZelf, string UitslMerk)

clauses

%preselected("").
metRanglijst(b_False).

rapport([],_,_,_) :- !.
rapport([T1,T2|Rest],C,Cat,afval) :-
  checkGelijkeClubs(Cat, [T1,T2], 0, _Uit),	% 4.3.2011 doen we iets mee
  rep2(C, Cat, T1),
  C1 = C + 1,
  rep2(C1, Cat, T2),
  C2 = C1 + 1, !,
  rapport(Rest,C2,Cat,afval).

rapportP([Opg], Cat) :-	% de laatste geen extra regel
  repPoel(Opg, Cat, 0), !.
rapportP([Opg|OpgList], Cat) :- !,
  repPoel(Opg, Cat, 1),
  rapportP(OpgList, Cat).
rapportP(_, _).

repPoel(VlgNo, Cat, _) :-
  opg(VlgNo,Cat,s(SpNo),Plts,_/*Rnd*/,Uitsl,_,_,_,_,_),
  sp2(SpNo,Srt,Naam,_,_ ,_,Rating,_,_,Club,_, _,KNLTB,StEnkel,_,GebDat, _,_,_,_,_,_,_,_,_,_,_),
  isLeeg(Club, Club1),
  isleeg(KNLTB, KNLTB1),
  spsrt(Srt, _, MVs,_,_,_,_,_),
  get_mv(MVs, MV),
  get_plts(Plts, Plaats, PlaatsCSS),
  get_uitsluit(Uitsl, _, PlaatsCSS, UitslCSS),
  isLeeg(StEnkel, StEnkel1),  isLeeg(Gebdat, Gebdat1), isLeeg(Rating, Rating1),
  writef("\n<tr %s><td>%s</td><td>%s</td><td class='middle'>%c</td><td>%s</td><td class='middle'>%s</td><td class='middle'>%s</td><td class='middle'>%s</td><td>%s</td><td></tr>", 
          UitslCSS, Club1,KNLTB1, MV, Naam, StEnkel1, Rating1, Plaats, GebDat1), !.
repPoel(VlgNo, Cat, _) :-
  opg(VlgNo,Cat,pw(SpNo),Plts,_/*Rnd*/,_/*Uitsl*/,_,_,_,_,_),
  sp2(SpNo,Srt,Naam,_,_ ,_,Rating,_,_,Club,_, _,KNLTB,_StEnkel,StDub,GebDat, _,_,_,_,_,_,_,_,_,_,_),
  spsrt(Srt, _, MVs,_,_,_,_,_),
  get_mv(MVs, MV),
  get_plts(Plts, Plaats, PlaatsCSS),
  isLeeg(KNLTB, KNLTB1),  isLeeg(StDub, StDub1),  isLeeg(Plaats, Plaats1),  isLeeg(Gebdat, Gebdat1),  isLeeg(Club, Club1), isLeeg(Rating, Rating1),
  writef("\n<tr %s><td>%s</td><td>%s</td><td>%c</td><td>%s</td><td class='middle'>%s</td><td class='middle'>%s</td><td class='middle'>%s</td><td class='middle'>%s</td><td></tr>", 
          PlaatsCSS, Club1,KNLTB1, MV, Naam, StDub1, Rating1, Plaats1, GebDat1), !.
repPoel(VlgNo, Cat, _Extra) :-
  opg(VlgNo,Cat,p(SpN1,SpN2),Plts,_/*Rnd*/,_/*Uitsl*/,_,_,_,_,_),
  sp2(SpN1,Srt1,Naam1,_,_ ,_,Rating1,_,_,Club1,_, _,KNLTB1,_,StDbl1,GebDat1, _,_,_,_,_,_,_,_,_,_,_),
  sp2(SpN2,Srt2,Naam2,_,_ ,_,Rating2,_,_,Club2,_, _,KNLTB2,_,StDbl2,GebDat2, _,_,_,_,_,_,_,_,_,_,_),
  spsrt(Srt1, _, MVs1, _, _, _, _, _),
  get_mv(MVs1, MV1),
  spsrt(Srt2, _, MVs2, _, _, _, _, _),
  get_mv(MVs2, MV2),
  get_plts(Plts, Plaats, PlaatsCSS),
  isLeeg(Club1, Club11),  isLeeg(Club2, Club21),  isLeeg(KNLTB1, KNLTB11),  isLeeg(KNLTB2, KNLTB21),  isLeeg(StDbl1, StDbl11),
  isLeeg(StDbl2, StDbl21),  isLeeg(GebDat1, Gebdat11),  isLeeg(Gebdat2, Gebdat21), isLeeg(Rating1,Rating11), isLeeg(Rating2, Rating21),
  writef("\n<tr %s><td>%s<br>%s</td><td>%s<br>%s</td><td>%c<br>%c</td><td>%s<br>%s</td><td class='middle'>%s<br>%s</td><td>%s<br>%s</td><td class='middle'>%s</td><td>%s<br>%s</td></tr>", 
          PlaatsCSS, Club11,Club21,KNLTB11,KNLTB21, MV1,MV2, Naam1,Naam2, StDbl11,StDbl21, Rating11,Rating21, Plaats, GebDat11, GebDat21),  !.
repPoel(_, _, _).

get_plts(0, "&nbsp;",  "") :- !.
get_plts(P, Pl, "class='plaats'") :-
  str_int(Pl, P).

get_pltsmethaakjes(0, "") :- !.
get_pltsmethaakjes(P1, P) :-
  format(P, "(%u)", P1).

get_uitsluit(0, "  ", In, In) :- !.
get_uitsluit(_, "Ja", _, "class='uitsl'").

get_mv(m, 'M').
get_mv(v, 'V').

rep2(SchNr, _, bye) :-
  writef("\n<tr><td class='middle'>%u</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><tr>", 
          SchNr,  "&nbsp;", "&nbsp;", "&nbsp;","bye", "&nbsp;",  "&nbsp;", "&nbsp;", "&nbsp;"), !.
rep2(SchNr, Cat, onbekend) :-
  pos(Cat, R, _, afval),
  Schnr1 = R * 2 + SchNr - 1,
  kopl(VCat, rang(Nr), Cat, plaats(Schnr1)),
  wc(VCat, _, _, Onderdeel, _, _, _, _, _, _, _, _, _, _,_),
  %wc(VCat, Onderdeel,_,_,_,_,_),
  format(Tekst, "Nr % van voorronde %.", Nr, Onderdeel), 
  writef("\n<tr><td>%u</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>", 
          SchNr, "&nbsp;", "&nbsp;", "&nbsp;",Tekst, "&nbsp;",  "&nbsp;", "&nbsp;", "&nbsp;"), !.
rep2(SchNr, _, onbekend) :-
  writef("\n<tr><td>%u</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>", 
          SchNr, "&nbsp;", "&nbsp;", "&nbsp;","gereserveerd", "&nbsp;",  "&nbsp;", "&nbsp;", "&nbsp;"), !.
rep2(SchNr, Cat, s(SpNo)) :-
  opg(_VlgNo,Cat,s(SpNo),Plts,_/*Rnd*/,Uitsl,_,_,_,_,_),
  sp2(SpNo,Srt,Naam,_,_ ,Rating,_,_,_,Club,_, _,KNLTB,StEnkel,_,GebDat, _,_,_,_,_,_,_,_,_,_,_),
  spsrt(Srt, _, MVs,_,_,_,_,_),
  get_mv(MVs, MV),
  get_plts(Plts, Plaats, PlaatsCSS),
  get_uitsluit(Uitsl, _, PlaatsCSS, UitslCSS),
  isLeeg(KNLTB, KNLTB1),  isLeeg(StEnkel, StEnkel1),  isLeeg(Plaats, Plaats1),  isLeeg(Gebdat, Gebdat1),  isLeeg(Club, Club1),isLeeg(Rating, Rating1),
  writef("\n<tr %s><td class='middle'>%u</td><td>%s</td><td>%s</td><td class='middle'>%c</td><td>%s</td><td class='middle'>%s</td><td class='middle'>%s</td><td class='middle'>%s</td><td>%s</td><td>", 
          UitslCSS, SchNr ,Club1,KNLTB1, MV, Naam, StEnkel1, Rating1, Plaats1, GebDat1), !.
rep2(SchNr, Cat, pw(SpNo)) :-
  opg(_VlgNo,Cat,pw(SpNo),Plts,_/*Rnd*/,Uitsl,_,_,_,_,_),
  sp2(SpNo,Srt,Naam,_,_ ,_,Rating,_,_,Club,_, _,KNLTB,_StEnkel,StDub,GebDat, _,_,_,_,_,_,_,_,_,_,_),
  spsrt(Srt, _, MVs,_,_,_,_,_),
  get_plts(Plts, Plaats, PlaatsCSS),
  get_uitsluit(Uitsl, _, PlaatsCSS, UitslCSS),
  get_mv(MVs, MV),
  isLeeg(KNLTB, KNLTB1),  isLeeg(StDub, StDub1),  isLeeg(Plaats, Plaats1),  isLeeg(Gebdat, Gebdat1),  isLeeg(Club, Club1), isLeeg(Rating, Rating1),
  writef("\n<tr %s><td class='middle'>%u</td><td>%s</td><td>%s</td><td class='middle'>%c</td><td>%s</td><td class='middle'>%s</td><td class='middle'>%s</td><td class='middle'>%s</td><td>%s</td><td>", 
          UitslCSS, SchNr,Club1,KNLTB1, MV, Naam, StDub1, Rating1, Plaats1, GebDat1), !.
rep2(SchNr, Cat, p(SpN1,SpN2)) :-
  opg(_VlgNo,Cat,p(SpN1,SpN2),Plts,_/*Rnd*/,Uitsl,_,_,_,_,_),
  sp2(SpN1,Srt1,Naam1,_,_ ,_,Rating1,_,_, Club1,_, _,KNLTB1,_,StDbl1,GebDat1, _,_,_,_,_,_,_,_,_,_,_),
  sp2(SpN2,Srt2,Naam2,_,_ ,_,Rating2,_,_,Club2,_, _,KNLTB2,_,StDbl2,GebDat2, _,_,_,_,_,_,_,_,_,_,_),
  spsrt(Srt1, _, MVs1, _, _, _, _, _),
  get_mv(MVs1, MV1),
  spsrt(Srt2, _, MVs2, _, _, _, _, _),
  get_mv(MVs2, MV2),
  get_plts(Plts, Plaats, PlaatsCSS),
  get_uitsluit(Uitsl, _, PlaatsCSS, UitslCSS),
  isLeeg(Club1, Club11),  isLeeg(Club2, Club21),  isLeeg(KNLTB1, KNLTB11),  isLeeg(KNLTB2, KNLTB21),  isLeeg(StDbl1, StDbl11),
  isLeeg(StDbl2, StDbl21),  isLeeg(GebDat1, Gebdat11),  isLeeg(Gebdat2, Gebdat21), isLeeg(Rating1, Rating11), isLeeg(Rating2, Rating21),
  writef("\n<tr %s><td class='middle'>%u</td><td class='middle'>%s<br>%s</td><td class='middle'>%s<br>%s</td>",
           UitslCSS, SchNr, Club11,Club21, KNLTB11, KNLTB21),
  writef("<td class='middle'>%c<br>%c</td><td>%s<br>%s</td><td class='middle'>%s<br>%s</td><td class='middle'>%s<br>%s</td><td class='middle'>%s</td><td>%s<br>%s</td></tr>", 
           MV1, MV2, Naam1, Naam2, StDbl11, StDbl21, Rating11, Rating21, Plaats, GebDat11,Gebdat21), !.

overblijvende_opgave(Cat,Tgnst, 1, Naam) :-
  opg(_,Cat,Tgnst,_,_,_,_,_,_,_,_),
  not(wd(_,Cat,Tgnst,_,_,_,_)),
  not(wd(_,Cat,_,Tgnst,_,_,_)),
  tgnst_naam(nee, b_False, Cat, Tgnst, Naam, _Tel).
overblijvende_opgave(Cat,Tgnst, 0, "") :-
  opg(_,Cat,Tgnst,_,_,N,_,_,_,_,_),
  N <> 0.

aant_tekst(0, "") :- !.
aant_tekst(A, A1) :-
  str_int(A1, A).
  
leeftijdI(Geb, Leeftijd, _) :-
  leeftijd(Geb, Leeftijd,0, _),
  Leeftijd > 0, !.
leeftijdI(Geb, 0, b_True) :-
  not(leeftError()),
  assert(leeftError()),
  format(Msg, "Geboortedatum %s\n niet herkenbaar!", Geb),
  dlg_MessageBox( "?", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), !.
leeftijdI(_, 0, _).

get_st(s(SpNo), StEnkel, Naam, Leeftijd, Leeftijd, Msgs) :-
  sp2(SpNo,_,Naam,_,_ ,_,_,_,_,_,_, _,_,StEnkel,_,Geb, _,_,_,_,_,_,_,_,_,_,_),
  trim_c(StEnkel),
  not(StEnkel = ""),
  leeftijdI(Geb, Leeftijd, Msgs), !.
get_st(s(SpNo), "?", Naam, 0, 0, _) :-
  sp2(SpNo,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !.
get_st(pw(SpNo), StDub, Naam, Leeftijd, Leeftijd, Msgs) :-
  sp2(SpNo,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,StDub,Geb, _,_,_,_,_,_,_,_,_,_,_),
  trim_c(StDub),
  not(StDub = ""),
  leeftijdI(Geb, Leeftijd, Msgs), !.
get_st(p(Sp1, Sp2), StDubbel, Naam, Leeftijd1, Leeftijd2, Msgs) :-
  sp2(Sp1,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,St1,Geb1, _,_,_,_,_,_,_,_,_,_,_),
  leeftijdI(Geb1, Leeftijd1, Msgs),
  sp2(Sp2,_,_,_,_ ,_,_,_,_,_,_, _,_,_,St2,Geb2, _,_,_,_,_,_,_,_,_,_,_), !,
  leeftijdI(Geb2, Leeftijd2, Msgs),
  dubbelsterkte(St1, St2, StDubbel).
get_st(p(Sp1, _), "?", Naam, 0, 0, _) :-
  sp2(Sp1,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !.
get_st(_, "?", "x", 0, 0, _).

dubbelsterkte(S1, S2, D) :-
  str_int(S1, I1),
  str_int(S2, I2),
  D0 = I1 + I2,
  Dx = D0 / 2,
  format(D, "%2.1f", Dx), 
  trim_c(D), !.
dubbelsterkte(S1, _, D) :-
  str_int(S1, _I1),
  format(D, "%s.0", S1), !.
dubbelsterkte(_, S2, D) :-
  str_int(S2, _I2),
  format(D, "%s.0", S2), !.
dubbelsterkte("", _, "?") :- !.
dubbelsterkte(X, _, X).

leeftijden(L) :-	% niet g
  opg_data(_OpgaveNum, _Tgnst, _StOpg, _PLts, _Naam, L, _),
  L <> 0.
leeftijden(L) :-
  opg_data(_OpgaveNum, _Tgnst, _StOpg, _PLts, _Naam, _, L),
  L <> 0.

vastgesteld(_, 0, 0) :- !.
vastgesteld(Text,L, H) :-
  writef("\n %s: %3d - %3d.",Text, L, H), !.

scan_sterkte(Cat, Aanwezig) :-
  retractall(opg_data(_,_,_,_,_,_,_),indeel),
  %st_list(StList),
  opg(OpgaveNum,Cat,Tgnst,Plts,_,_,_,_,_,_,_),
  in_weds1(Cat, Tgnst, Aanwezig),
  get_st(Tgnst, StOpg, Naam, Lft1, Lft2, b_True),
  not(opg_data(_, Tgnst, _, _, _, _, _)),		% 13.9.2000 extra check ivm koppelingen
  assert(opg_data(OpgaveNum, Tgnst, StOpg, PLts, Naam, Lft1, Lft2)),
  fail. 
scan_sterkte(_, _) :-
  findall(Stx, opg_data(_, _, Stx, _Plts1, _Naam, _, _), StxL),
  sortstring_c(StxL, 1),
  removeDubl(StxL, [], StList),
  write("\n Sterkte : "),
  %st_list(StList),  
  member(St, StList),
  writef("%-3", St),
  fail.
scan_sterkte(_, _) :-
  findall(Stx, opg_data(_, _, Stx, _Plts1, _Naam, _, _), StxL),
  sortstring_c(StxL, 1),
  removeDubl(StxL, [], StList),
  write("\n Aantal  : "),
  %st_list(StList),  
  member(St, StList),  
  findall(Num, opg_data(Num, _, St, _, _, _, _), NumList),
  count(NumList, 0, Aantal),
  aant_tekst(Aantal, Tekst),
  writef("%-3", Tekst),
  fail.
scan_sterkte(Cat, _) :-
  %wc(Cat, _, _, _, _, _, _, _, _, _, _, _, _, _,_),
  wc(Cat, _OnderdeeelID, _N, _Naam, _EDG, _Kort, _SexL, _SpStrk, leeftijd(_,SpecLow), leeftijd(_,SpecHigh), _HDG, _Schemasoort, _Keuring, _KNLTB, _Geld),
  %wc(Cat,_Kop,_EDG,_Kort,_VoorWie,_Geld,_Verl),
  %wcritp(Cat, _SpSterkte, SpecLow, SpecHigh, _),
  %not(EDG = g),
  findall(Lft, leeftijden(Lft), LftL),
  sortint_c(LftL),
  LftL = [MinSt|_],
  reverse(LftL, [], LftLR),
  LftLR = [MaxSt|_],
  writef("\n Min/max leeftijd ingeschreven spelers: %3d - %3d.",MinSt, MaxSt),
  vastgesteld("Min/max leeftijd volgens regels      ",SpecLow, SpecHigh), !.
scan_sterkte(_, _) :-
  retractall(opg_data(_,_,_,_,_,_,_),indeel).

scan_sterkteX(Cat,_) :-
  retractall(verzam(_)),
  retractall(opg_data(_,_,_,_,_,_,_),indeel),
  %st_list(StList),
  opg(OpgaveNum,Cat,Tgnst,Plts,_,_,_,_,_,_,_),
  in_weds1(Cat, Tgnst, 0),
  get_st(Tgnst, StOpg, Naam, Lft1, Lft2, b_True),
  not(opg_data(_, Tgnst, _, _, _, _, _)),		% 13.9.2000 extra check ivm koppelingen
  %speelsterkteOudNieuw(StOpg, StOpg1),
  assert(opg_data(OpgaveNum, Tgnst, StOpg, PLts, Naam, Lft1, Lft2)),
  fail. 
scan_sterkteX(_,_) :-
  findall(St, opg_data(_, _, St, _, _, _, _), StList),
  sortstring_c(StList, 1),
  StList = [Min | _],
  reverse(StList, [], StListR),
  StListR = [Max | _],
  format(SComb, "%s - %s", Min, Max),
  assertz(verzam(SComb)),
  fail.
scan_sterkteX(_,_) :-
  findall(St, opg_data(_, _, St, _, _, _, _), StList),
  sortstring_c(StList, 1),
  StList = [],
  assertz(verzam("-")),
  fail.
scan_sterkteX(_Cat, _) :-
%  wc(Cat,_Kop,_EDG,_Kort,_VoorWie,_Geld,_Verl),
%  wcritp(Cat, _SpSterkte, _SpecLow, _SpecHigh, _),
  %not(EDG = g),
  findall(Lft, leeftijden(Lft), LftL),
  sortint_c(LftL),
  LftL = [MinSt|_],
  reverse(LftL, [], LftLR),
  LftLR = [MaxSt|_],
  format(SComb, "%u - %u",MinSt, MaxSt),
  assertz(verzam(Scomb)),
  fail.
scan_sterkteX(_Cat, _) :-
  findall(Lft, leeftijden(Lft), LftL),
  LftL = [],
  assertz(verzam("-")),
  fail.
scan_sterkteX(_, Uit) :-
  findall(S, verzam(S), SList),
  list_to_string(SList, "</td><td class='middle'>", Uit),
  retractall(opg_data(_,_,_,_,_,_,_),indeel),
  retractall(verzam(_)).

inIndeling(OpgNo, Lijst) :-
  opg(OpgNo,_Cat,Tgnst,_Plts,_Byes,_,_,_,_,_,_),
  member(Tgnst, Lijst), !.

deelnemers([], _, [], []) :- !.
deelnemers([Deelnr|Rest], IndelingsLijst, [Deelnr|Lijst], Overblijvers) :-
  inIndeling(Deelnr, IndelingsLijst), !,
  deelnemers(Rest, IndelingsLijst, Lijst, Overblijvers).
deelnemers([Deelnr|Rest], IndelingsLijst, Lijst, [Deelnr|Overblijvers]) :-
  deelnemers(Rest, IndelingsLijst, Lijst, Overblijvers).
  
soort2string(e, heren, 	   "Enkelspel Heren")	:- !.
soort2string(e, dames,	   "Enkelspel Damesl")	:- !.
soort2string(e, gemengd,   "Enkelspel Gemengd")	:- !.
soort2string(_, heren,     "Dubbelspel Heren")	:- !.
soort2string(_, dames,	   "Dubbelspel Dames")	:- !.
soort2string(_, _,	   "Dubbelspel Gemengd").

maaktabel() :-
  write("\n<TABLE border=1 cellpadding=3 cellspacing=0>").
maaktabel() :-
  write("\n</table>"),
  fail.
  
toon_plaatsing1(Cat,Indeling,NumWeds,afval,Soort) :-
  retractall(_, indeel),
  wc(Cat, _, _, Kop, EDG, _, VoorWie, _, _, _, _, _, _, _,_),
  %wc(Cat,Kop,EDG,_Kort,VoorWie,_Geld,_Verl),
  writef("\n<h5>Indeling van afvalschema %\n</h5>", Kop), 
  manVrouw(EDG, Voorwie, Srt), 
  soort2string(EDG, Srt, Spelsoort),
  write("\n<pre>\n"),
  writef(" %s. Aantal partijen: %",Spelsoort,NumWeds),
  %leeftijdscat(Voorwie, LeeftCat),
  scan_sterkte(Cat,Soort),
  %write('\n', ' '), strepen(76, '_'),
  write("\n</pre>"),
  maaktabel(),
  write("\n<tr><th>Sch<br>ema</th><th>Club</th>"),
  write("\n<th>KNLTB<br>nr</th><th>M<br>V</th><th>Naam</th><th>Sp<br>st</th><th>Rating</th><th>-</th>"),
  write("\n<th>Geboorte<br>datum</th></tr>"),
  %write("\n|Sch|Vlg|Resul-| Club |  KNLTB |M| Naam                |Sp|Adv|Plts| Geboorte |"),
  %write("\n|ema|nr | taat |      |    nr  |V|                     |st|ies|ing |   datum  |"),
  %write('\n', ' '), strepen(76, '_'),
%          1...+....10...+....20...+....30...+....40...+....50...+....60...+....70...+
  rapport(Indeling, 1, Cat, afval),
  findall(T1, overblijvende_opgave(Cat, T1, Soort, _), T1L),
  not(T1L=[]),
  write("\n<tr><td colspan='11'>Niet opgesteld:</td></tr>"),
  overblijvende_opgave(Cat,Tgnst, Soort, _),
  rep2(0,Cat,Tgnst), 
  fail.
toon_plaatsing1(_Cat,_Indeling,_NumWeds,afval,_Soort) :-
  !.
  %write('\n', ' '), strepen(76, '_'), nl.
toon_plaatsing1(Cat,IndelingsLijst,NumWeds,poel,_Soort) :-
  wc(Cat, _, _, Kop, _, _, _, _, _, _, _, _, _, _,_),
  %wc(Cat,Kop,_,_,_,_,_),
  format(Tk1, "<h5>Deelnemers aan poule %</h5>\n", Kop), 
  write(Tk1),
  %scan_sterkte(Cat, Soort), 
  write("\n<pre>\n"),
  write(" Aantal partijen: ",NumWeds),
  opgavenNaarSterkte(Cat, OpgList),
  deelnemers(OpgList, IndelingsLijst, DeelnemerLijst, Overblijvers),
  write("\n</pre>"),
  maaktabel(),
  write("\n<tr><th>Club</th><th>KNLTB<br>nr</th><th>M<br>V</th><th>Naam</th><th>Sp<br>st</th><th>Rating</th><th>Plts<br>ing</th><th>Geboorte<br>datum</th></tr>"),
  %write('\n', ' '), strepen(61, '_')
  %write("\n| Club |  KNLTB |M| Naam                |Sp|Adv|Plts| Geboorte |"),
  %write("\n|      |    nr  |V|                     |st|ies|ing |  datum   |"),
  %write('\n', ' '), strepen(61, '_'),
%          1...+....10...+....20...+....30...+....40...+....50...+....60...+....70...+
  rapportP(DeelnemerLijst, Cat),
  %write('\n', ' '), strepen(61, '_'),
%  Soort = 1, */
  not(OverBlijvers=[]),
  write("\n<tr><td colspan='8'>Niet opgesteld:</td></tr>"),
  rapportP(Overblijvers, Cat), !, 
  write("<br>\n").
  %write('\n', ' '), strepen(61, '_'), nl.
toon_plaatsing1(_Cat,_Indeling,_NumWeds,poel,_Soort) :- !,
  write("<br>\n").

toon_indeling() :-              % global
  findall(I,opg_cat(I, _, 0),IList),
  TaskWin = vpi_GetTaskWin(),
  dlg_onderdeel_select_Create(TaskWin, "Indeling van: ", IList, 0, Selectie, _),
  Selectie = [Sel|_],
  searchchar(Sel, '\t', P1),
  frontstr(P1,Sel,_,S1),
  searchchar(S1,'\t',P2),
  frontstr(P2,S1,_,Kop),
  wc(Cat, _, _, Kop, _, _, _, _, _, _, _, _, _, _,_),
  %wc(Cat,Kop,_,_,_,_,_),
  toon_plaatsing(Cat), !.
toon_indeling().
%  retractall(opg_data(_,_,_,_,_,_,_),indeel).

toon_plaatsing(Cat) :-  	% global
  pos(Cat,Card,_,Soort), !,
  BeginW = Card,
  EindW  = 2 * Card,
  bouw_lijst(Cat,BeginW,Eindw,Lijst),
  findall(WNum,weds(Cat,WNum),WNumList),
  count(WNumList,0,NumWeds),
  toon_plaatsing1(Cat,Lijst,NumWeds,Soort,1),
  %write("<br>\n"),
  retractall(opg_data(_,_,_,_,_,_,_),indeel).
toon_plaatsing(_).

bouw_lijst(_,Eind,Eind,[]) :- !.
bouw_lijst(Cat,Curr,Eind,[T1,T2|Out]) :-
  wd(Curr,Cat,T1,T2,_,_,_), !,
  C1 = Curr + 1,
  bouw_lijst(Cat,C1,Eind,Out).
bouw_lijst(Cat,Curr,Eind,Out) :-
  C1 = Curr + 1,
  bouw_lijst(Cat,C1,Eind,Out).

weds(Cat,Num) :-
  wd(Num,Cat,X,Y,_,_,_),
  echt(X,Y).

echt(bye,_)   :- !,fail.
echt(_,bye)   :- !,fail.
echt(_,_).

getRatingR(["",""|_], "&nbsp;", "&nbsp;") :- !.
getRatingR(["",RD|_], "&nbsp;", RD) :- !.
getRatingR([RE,""|_], RE, "&nbsp;") :- !.
getRatingR([RE,RD|_], RE, RD) :- !.
getRatingR(_, "&nbsp;", "&nbsp;") :- !.

keurTekstRat(geaccepteerd, "<SPAN class=\"groen\">geaccepteerd</SPAN>") :- !.
keurTekstRat(nietgeaccepteerd,   "<SPAN class=\"rood\">niet geaccepteerd</SPAN>") :- !.
keurTekstRat(reserve,   "<SPAN class=\"rood\">reserve</SPAN>") :- !.
keurTekstRat(gekeurd,         "gekeurd") :- !.
keurTekstRat(msg(_,_,_,_,_,_), "<SPAN class=\"rood\">afkeur</SPAN>") :-
   !.
keurTekstRat(_,         "niet gekeurd").

uitslTekst1(Uitsl, "Uitgesloten") :-
  Uitsl > 0, !.
uitslTekst1(_, "&nbsp;").  

uitslMerk(KN, 0, KN) :- !.
uitslMerk(KN, _, KNM) :-
  format(KNM, "<SPAN style=\"text-decoration:line-through;color:blue\">%s</SPAN>", KN). 

sp2opgDetails(Sp, CatZelf,UitslMerk) :-
  sp2opg(Sp,Cat,OpgNo,_,_,_),
  %bovencat(Cat),
  Not(Cat = Catzelf),
  opg(OpgNo,Cat,_,_,_,Uitsl,_,_,_,_,_),
  wc(Cat, _, _, _, _, KN, _, _, _, _, _, _, _, _,_),
  %wc(Cat,_,_EDG,KN,_VoorWie,_Prijs,_Verl),
  uitslMerk(KN, Uitsl, UitslMerk).

doetookMeeAan(CatZelf, Sp, Tekst) :- 
  findall(UitsluitMerk, sp2opgDetails(Sp, CatZelf,UitsluitMerk), List),
  not(List = []), !,
  list_to_string(List, " ", Tekst).
doetookMeeAan(_CatZelf, _Sp, "&nbsp;"). 

%sp1(No,Soort,N1,Tel,_,Rating,_,BeginTijd, Club, Adres, Woonpl, LidNr, Sterkte, SterkD, Geb, Tel1, Tel2, EMail, _Opmerking)
toon_rating(NoU, Cat) :-
  opg(NoU,Cat,p(SpNo,Sp1),_Plts,_Ronde,Uitsl,_,_,_,Keur,_),
  sp2(SpNo,_,N1,_,_ ,_,RD1,_,_,_,_, _,LidNr1,_SterkteE1,_SterkteD1,_GebDat1, _,_,_,_,_,_,RangD1,_,_,_,_),
  sp2(Sp1,_,N2,_,_ ,_,RD2,_,_,_Club,_, _,LidNr2,_SterkteE2,_SterkteD2,_GebDat2, _,_,_,_,_,_,RangD2,_,_,_,_),
  bondsnr(LidNr1, Query1),
  bondsnr(LidNr2, Query2),
  %teamkeur(Cat, p(SpNo,Sp1), Keur),
  keurTekstRat(Keur, KTekstR),
  keur2tekst(Cat,[SpNo,Sp1],Keur, _KKort, KKTekst, _KKleur),
  format(KTekst, "%s %s", KTekstR, KKTekst),
  str_real_Rating(RD1, RD1real),
  str_real_Rating(RD2, RD2real),
  dubbelRating(RD1real, RD2real, Dubbelrating, _Delta, _DColor, _),
  isleeg(RD1, RD11),
  isLeeg(RD2, RD21),
  isLeeg(RangD1, RangD11),
  isLeeg(RangD2, RangD21),
  schrijfMetRangLijst(RangD11, RangD21, RangLijstDS),
  UT = uitslTekst1(Uitsl),
  doetookMeeAan(Cat, SpNo, DM1),
  doetookMeeAan(Cat, Sp1, DM2),
  format(SpNamen,"%s<BR>%s",N1,N2),
  uitslMerk(SpNamen, Uitsl, DeNamen),
  write("\n<TR><TD>",DeNamen, "</TD><TD>", Query1, "<BR>", Query2, "</TD>",RangLijstDS,"<TD>",Dubbelrating,"</TD>","<TD>",RD11,"<BR>",RD21,"</TD><TD>",UT,"</TD><TD>",DM1,"<BR>",DM2,"</TD><TD>",KTekst,"</TR>"), !.
toon_rating(NoU, Cat) :-
  opg(NoU,Cat,pw(SpNo),_Plts,_Ronde,Uitsl,_,_,_,Keur,_),
  sp2(SpNo,_,N1,_,_ ,_,RD1,_,_,_,_, _,LidNr1,_SterkteE1,_SterkteD1,_GebDat1, _,_,_,_,_,_,RangD1,_,_,_,_),
  bondsnr(LidNr1, Query1),
  %teamkeur(Cat, pw(SpNo), Keur),
  keurTekstRat(Keur, KTekst),
  trim_c(RD1),
  isleeg(RD1, RD11),
  isLeeg(RangD1, RangD11),
  schrijfMetRangLijst(RangD11, "", RangLijstDS),
  UT = uitslTekst1(Uitsl),
  uitslMerk(N1, Uitsl, DeNaam),
  doetookMeeAan(Cat, SpNo, DM1),
  write("\n<TR><TD>",DeNaam,"<BR>partner wanted</TD><TD>", Query1, "</TD>",RangLijstDS,"<TD>", RD11,"</TD><TD>",KTekst,"</TD><TD>",UT,"</TD><TD>",DM1,"</TD><TD></TD></TR>"), !.
toon_rating(No, Cat) :-
  opg(No,Cat,s(SpNo),_Plts,_Ronde,Uitsl,_,_,_,Keur,_),
  sp2(SpNo,_,N1,_,_ ,RE,_RD1,_,_,_,_, _,LidNr1,_SterkteE1,_SterkteD1,_GebDat1, _,_,_,_,_,RanglijstE,_,_,_,_,_), !,
  trim_c(RE),
  bondsnr(LidNr1, Query),
  %teamkeur(Cat, s(SpNo), Keur),
  keurTekstRat(Keur, KTekstR),
  keur2tekst(Cat,[SpNo],Keur, _KKort, KKTekst, _KKleur),
  format(KTekst, "%s %s", KTekstR, KKTekst),
  isLeeg(RangLijstE, RangLijstE1),
  isleeg(RE, RE1),
  schrijfMetRangLijst(RangLijstE1, "", RangLijstES),
  UT = uitslTekst1(Uitsl),
  doetookMeeAan(Cat, SpNo, DM1),
  uitslMerk(N1, Uitsl, DeNaam),
  write("\n<TR><TD>",DeNaam, "</TD><TD>", Query, "</TD>",RangLijstES,"<TD>", RE1, "</TD><TD>",UT,"</TD><TD>",DM1,"</TD><TD>",KTekst,"</TD></TR>").

schrijfMetRangLijst(RangLijstE, "", RangLijstES) :-
  metRangLijst(b_True), !,
  format(RangLijstES, "<TD style=\"text-align: center\">%s</TD>", RangLijstE).
schrijfMetRangLijst(RangLijstD1, RangLijstD2, RangLijstDS) :-
  metRangLijst(b_True), !,
  format(RangLijstDS, "<TD style=\"text-align: center\">%s<BR>%s</TD>", RangLijstD1,RangLijstD2).
schrijfMetRangLijst(_, _, "").

bondsnr(KNLTB, "??") :-
  not(fronttoken(KNLTB, _, _)), !.
bondsnr(KNLTB, Query) :-
  %date(Year,_Month,_Day),
  %Jaar = Year - 1,
  providerX(Prov, _, "", _),
  format(Query, "<a href=http://%s/profielKNLTB.php?bnr=%s title=\"Klik voor het spelerprofiel.\">%s</a>", Prov, KNLTB,KNLTB), !.

achteraanAls0(0, 1000) :- !. 
achteraanAls0(In, In).

teamIdRating(Cat, Id) :-
  findall(Catx,  voorronde(Cat, Catx), CatxL),		% alle voorgaande rondes 
  member(Catxx, [Cat|CatxL]),
  opg(No,Catxx,p(SpNo,Sp1),_Plts,_Ronde,_Uitsl,_,_,_,_,_),
  not(algeweestRating(p(SpNo,Sp1))),
  assert(algeweestRating(p(SpNo,Sp1))),
  sp2(SpNo,_,_Naam,_,_ ,_,RD1,_,_,_,_, _,_LidNr1,_SterkteE1,_SterkteD1,GebD1, _,_,_,_,_,_,RangLD1,_,_,_,_),
  sp2(Sp1,_,_N1,_,_ ,_,RD2,_,_,_,_, _,_LidNr2,_SterkteE2,_SterkteD2,GebD2, _,_,_,_,_,_,RanglD2,_,_,_,_),
  str_real_Rating(RD1, R1),
  str_real_Rating(RD2, R2),
  dubbelRating(R1, R2, RD, _, _, _LotingRating),
  ranglijstGeldt(Cat, GebD1, RangLD1, RPD1g),
  ranglijstGeldt(Cat, GebD2, RangLD2, RPD2g),
  str_intP(RPD1g, RLD1),
  str_intP(RPD2g, RLD2),
  achteraanAls0(RLD1, RLD1a), 
  achteraanAls0(RLD2, RLD2a), 
  DubbelRangL = RLD1a + RLD2a,
  str_intP(DRL, DubbelRangL),
  concat(RangLD1, RangLD2, EenRangL),
  isMetRangLijst(EenRangL, DRL, DRLa),
  format(Id, "%6u\t%-6s\t%u\t%U", DRLa, RD, Catxx, No).
teamIdRating(Cat, Id) :-
  findall(Catx,  voorronde(Cat, Catx), CatxL),		% alle voorgaande rondes 
  member(Catxx, [Cat|CatxL]),
  opg(No,Catxx,pw(SpNo),_Plts,_Ronde,_Uitsl,_,_,_,_,_),
  not(algeweestRating(pw(SpNo))),
  assert(algeweestRating(pw(SpNo))),
  sp2(SpNo,_,_Naam,_,_ ,_,RD1,_,_,_,_, _,_LidNr1,_SterkteE1,_SterkteD1,GebD1, _,_,_,_,_,_,RangLD1,_,_,_,_),
  str_real_Rating(RD1, R1),
  dubbelRating(R1, 0.0, RD, _, _, _LotingRating),
  ranglijstGeldt(Cat, GebD1, RangLD1, RPD1g),
  str_intP(RPD1g, RLD1),
  achteraanAls0(RLD1, RLD1a), 
  DubbelRangL = RLD1a,
  str_intP(DRL, DubbelRangL),
  concat(RangLD1, "", EenRangL),
  isMetRangLijst(EenRangL, DRL, DRLa),
  format(Id, "%6u\t%-6s\t%u\t%U", DRLa, RD, Catxx, No).
teamIdRating(Cat, Id) :-
  findall(Catx,  voorronde(Cat, Catx), CatxL),		% alle voorgaande rondes 
  member(Catxx, [Cat|CatxL]),
  opg(No,Catxx,s(SpNo),_Plts,_Ronde,_Uitsl,_,_,_,_,_),
  not(algeweestRating(s(SpNo))),
  assert(algeweestRating(s(SpNo))),
  sp2(SpNo,_,_N1,_,_ ,RE,_RD1,_,_,_,_, _,_LidNr1,_SterkteE1,_SterkteD1,GebD1, _,_,_,_,_,RLE,_,_,_,_,_),
  trim_c(RE),
  ranglijstGeldt(Cat, GebD1, RLE, RLEg),
  str_intP(RLEg, R),
  achteraanAls0(R, R1),
  str_int(RLEx, R1),
  isMetRangLijst(RLE, RLEX, RLEa),
  format(Id, "%6s\t%-6s\t%u\t%U", RLEa, RE, Catxx, No).

isMetRangLijst(EenRangL, In, In) :-
  fronttoken(EenRangL, _, _),
  assert(metRanglijst(b_True)),
  fail.
isMetRangLijst(_, _, "999999").

teamsOpVolgordeVanRating(_, 0, 0, 0) :- !,
  write("\n<TR><TH colspan=7>Geen opgaven.</TH></TR>\n"),
  fail.
teamsOpVolgordeVanRating(IdL, CatU, NoU, _) :-
  sortstring_c(IdL, 1),
  member(Str, IdL),
  parseTabs(Str, ListX),
  ListX = [_, _, CatS, NoS | _],
  str_int(CatS, CatU),
  str_int(NoS, NoU)
  .

maakTable(e) :-
  metRanglijst(b_True),
  write("\n<TABLE border=1 cellpadding=3 cellspacing=0>"),  
  write("\n<TR><TH>Spelers</TH><TH>Bondsnr</TH><TH>Ranglijst</TH><TH>Rating</TH><TH>Uitsl</TH><TH>Ook</TH><TH>Keuring</TH></TR>\n").
maakTable(e) :-
  metRanglijst(b_False),
  write("\n<TABLE border=1 cellpadding=3 cellspacing=0>"),  
  write("\n<TR><TH>Spelers</TH><TH>Bondsnr</TH><TH>Rating</TH><TH>Uitsl</TH><TH>Ook</TH><TH>Keuring</TH></TR>\n").
maakTable(EDG) :-
  not(EDG = e),
  metRanglijst(b_True),
  write("\n<TABLE border=1 cellpadding=3 cellspacing=0>"),  
  write("\n<TR><TH>Koppels</TH><TH>Bondsnr</TH><TH>Ranglijst</TH><TH>Gem<BR>Rating</TH><TH>Rating</TH><TH>Uitsl</TH><TH>Ook</TH><TH>Keuring</TH></TR>\n").
maakTable(EDG) :-
  not(EDG = e),
  metRanglijst(b_False),
  write("\n<TABLE border=1 cellpadding=3 cellspacing=0>"),  
  write("\n<TR><TH>Koppels</TH><TH>Bondsnr</TH><TH>Gem<BR>Rating</TH><TH>Rating</TH><TH>Uitsl</TH><TH>Ook</TH><TH>Keuring</TH></TR>\n").
maakTable(_) :-		% op de terugweg
  write("\n</Table>"),
  fail.

%BEGIN_DLG Indelingsrapport
/**************************************************************************
	Creation and event handling for dialog: Indelingsrapport
**************************************************************************/

constants

%BEGIN Indelingsrapport, CreateParms, 11:01:51-20.4.2015, Code automatically updated!
  dlg_indelingsrapport_DlgType = wd_Modeless
  dlg_indelingsrapport_Title = "opgaven/indelingsgsrapport"
  dlg_indelingsrapport_RCT = rct(40,32,247,244)
  dlg_indelingsrapport_Flags = [wsf_Close,wsf_TitleBar]
  dlg_indelingsrapport_Help = idh_adresetiketten
  dlg_indelingsrapport_Font = "MS Sans Serif"
  dlg_indelingsrapport_FSize = 8
%END Indelingsrapport, CreateParms

predicates

  dlg_indelingsrapport_eh : EHANDLER
  voorKlembord(slist, string, string)
  lijstVoorKlembord(tgl,integer,wedscat,rsoort,string,string)
  repK(integer, wedscat, tgnst, string, string)
  maakopgaverapport(slist Items, integer Volgorde,string MailmergeNaam, string HtmlFileNaam)
  nondeterm uppercat(wedscat)
  procedure overviewRap(catl)
  procedure overviewRap1(wedscat, string Col1)
  procedure isLeegInt(integer, string Uit)
  procedure opstellingrap(slist)
  procedure opstellingrap1(wedscat)
%  procedure laatstekeuring(string)

clauses


voorKlembord([Sel | Rest], In, String) :-
  parsetabs(Sel, Parts),
  Parts = [_, _, Kop | _],
  wc(Cat, _, _, Kop, _, _, _, _, _, _, _, _, _, _,_),
  pos(Cat,Card,_,Soort), 
  BeginW = Card,
  EindW  = 2 * Card,
  bouw_lijst(Cat,BeginW,Eindw,Lijst),
  format(In1, "%s\r\n%s", In, Kop),
  lijstVoorKlembord(Lijst, 1, Cat, Soort, In1, In2), !,
  voorKlembord(Rest, In2, String).
voorKlembord([_Sel|Rest], In, Uit) :-
  voorKlembord(Rest, In, Uit), !.
voorKlembord(_, InUit, InUit).

lijstVoorKlembord([T1,T2|Rest],C,Cat,afval,In,Uit) :-
  repK(C, Cat, T1, In, In1),
  C1 = C + 1,
  repK(C1, Cat, T2, In1, In2),
  C2 = C1 + 1, !,
  lijstVoorKlembord(Rest,C2,Cat,afval,In2,Uit).
lijstVoorKlembord([T1,T2|Rest],C,Cat,poel, In, Uit) :-
  repK(C, Cat, T1, In, In1),
  repK(C, Cat, T2, In1, In2),
  C1 = C + 1, !,
  lijstVoorKlembord(Rest,C1,Cat,poel,In2,Uit).
lijstVoorKlembord(_,_,_,_,InUit,InUit) :- !.

repK(SchNr, _, bye, In, Uit) :-
  format(Uit, "%s\r\n%u.\tbye", In, SchNr), !.
repK(SchNr, _, onbekend, In, Uit) :-
  format(Uit, "%s\r\n%u. \tgereserveerd", In, SchNr), !.
repK(SchNr, Cat, s(SpNo), In, Uit) :-
  opg(_VlgNo,Cat,s(SpNo),Plts,_/*Rnd*/,_/*Uitsl*/,_,_,_,_,_),
  sp2(SpNo,_Srt,Naam,_,_ ,_,_,_,_,_Club,_, _,_KNLTB,_StEnkel,_,_GebDat, _,_,_,_,_,_,_,_,_,_,_),
  naamscan(ja,b_False,Naam, Naam1,_, _LC),
  get_pltsmethaakjes(Plts, Plaats),
  format(Uit, "%s\r\n%u.\t%s%s", In, SchNr, Naam1, Plaats), !.
repK(SchNr, Cat, pw(SpNo), In, Uit) :-
  opg(_VlgNo,Cat,pw(SpNo),Plts,_/*Rnd*/,_/*Uitsl*/,_,_,_,_,_),
  sp2(SpNo,_Srt,Naam,_,_ ,_,_,_,_,_Club,_, _,_KNLTB,_StEnkel,_StDub,_GebDat, _,_,_,_,_,_,_,_,_,_,_),
  naamscan(ja,b_False,Naam, Naam1,_, _LC),
  get_pltsmethaakjes(Plts, Plaats),
  format(Uit, "%s\r\n%u.\t%s %s - zoekt partner", In, SchNr, Naam1, Plaats), !.
repK(SchNr, Cat, p(SpN1,SpN2), In, Uit) :-
  opg(_VlgNo,Cat,p(SpN1,SpN2),Plts,_/*Rnd*/,_/*Uitsl*/,_,_,_,_,_),
  sp2(SpN1,_Srt1,Naam1,_,_ ,_,_,_,_, _Club1,_, _,_KNLTB1,_,_StDbl1,_GebDat1, _,_,_,_,_,_,_,_,_,_,_),
  naamscan(ja,b_False,Naam1, Naam1a,_, _LC1),
  sp2(SpN2,_Srt2,Naam2,_,_ ,_,_,_,_,_Club2,_, _,_KNLTB2,_,_StDbl2,_GebDat2, _,_,_,_,_,_,_,_,_,_,_),
  naamscan(ja,b_False,Naam2, Naam2a,_, _LC2),
  get_pltsmethaakjes(Plts, Plaats),
  format(Uit, "%s\r\n%u.\t%s - %s%s", In, SchNr, Naam1a, Naam2a, Plaats), !.

maakopgaverapport(ItemList, Ans, OpgavenTxt, OpgavenHtml) :-
  closefile(work),
  closefile(nawf),
  openwrite(work, OpgavenHtml),
  openwrite(nawf, OpgavenTxt),
  writedevice(work),
  member(Sel, Itemlist),
  parsetabs(Sel, Rec),
  Rec = [_,_,Kop|_],
  %wc(Cat,Kop,_,_,_,_,_),
  rep_cat_opg(Kop, Ans), !.
maakopgaverapport(_, _, OpgavenTxt, OpgavenHtml) :-
  writedevice(screen),
  closefile(work), 
  closefile(nawf),
  format(Parallel, "%s bevat tevens een samenvoegbestand voor bijvoorbeeld Excel met gegevens.", OpgavenTxt),
  writeToJournaal(Parallel, ja),
  %write("### opgaverapport ", FullName),
  format(URL1, "file:\\\\%s", OpgavenHtml),
  browser_ctl(URL1), !.

uppercat(WC) :-
  wc(WC, _, _, _, _, _, _, _, _, _, _, _, _, _,_),
  %wc(WC, _Ond, _, _Shortp,_,_,_), 
  not(isOnderCat(WC)).

isLeegInt(0, "&nbsp;") :- !.
isLeegInt(Int, Uit) :-
  str_int(Uit, Int).

overviewRap([H|_]) :-
  overviewRap1(H, "*"),
  findall(V,voorronde(H, V),VL),
  member(V1, VL),
  overviewRap1(V1, "&nbsp;"),
  fail.
overviewRap([_ | T]) :- !,
  overviewRap(T).
overviewRap(_).

overviewrap1(H, Col1) :-
  wc(H, _, _, Ond, _, Shortp, _, Spst, leeftijd(_,LeeftV), leeftijd(_,LeeftTM), _, _, _, _, _),!,
  %wc(H, Ond, _, Shortp,_,_,_), !,
  %wcritP(H,Spst,LeeftV,LeefTM,_),
  isleeg(Spst, Spst1),
  isLeegInt(LeeftV,LeefVS),
  isLeegInt(LeeftTM,LeefTMS),
  findall(Opg1,opg(Opg1,H,_,_,_,0,_,_,_,_,_),OpgL1),	% 13.3.2011 uitbreiden tot hele hierarchie!
  findall(Opg2,opg(Opg2,H,_,_,_,1,_,_,_,_,_),OpgL2),
  count(OpgL1,0,No1),
  count(OpgL2,0,No2),
  isLeegInt(No1, No1S),
  isLeegInt(No2, No2S),
  writef("\n<tr><td class='middle'>%s</td><td>%s</td><td>%s</td>",Col1, Ond,Shortp),
  writef("\n<td class='middle'>%s</td><td class='middle'>%s</td><td class='middle'>%s</td>",Spst1,LeefVS,LeefTMS),
  writef("\n<td class='middle'>%s</td><td class='middle'>%s</td>", No1S, No2S),
  scan_sterkteX(H, Str2),
  write("<td class='middle'>",Str2,"</td></tr>").
overviewrap1(_H, _Col1).

opstellingRap([H|_]) :-
  parseTabs(H, Rec),
  Rec = [_,_,Kop|_],
  wc(Cat, _, _, Kop, _, _, _, _, _, _, _, _, _, _,_),
  %wc(Cat,Kop,_,_,_,_,_),
  opstellingRap1(Cat),
  findall(V,voorronde(Cat, V),VL),
  member(V1, VL),
  opstellingRap1(V1),
  fail.
opstellingRap([_ | T]) :- !,
  opstellingRap(T).
opstellingRap(_).

opstellingrap1(Cat) :-
  pos(Cat,Card,_,Soort), 
  BeginW = Card,
  EindW  = 2 * Card,
  bouw_lijst(Cat,BeginW,Eindw,Lijst),
  findall(WNum,weds(Cat,WNum),WNumList),
  count(WNumList,0,NumWeds),
  toon_plaatsing1(Cat,Lijst,NumWeds,Soort,1), !,
  retractall(_, indeel).
opstellingrap1(_Cat) :-
  retractall(_, indeel).

ratingrapport(ItemList, Titel, Msg) :-
	closefile(work),
	assert(metRangLijst(b_False)),
	naam(ToernooiNaam, Nrs, Datum, WT, _, _),
	term_str(slist, Nrs, NrsStr),
	getFolder(Path,_,_,_),
	format(Name, "ratings%.html", WT),
	filenamepath(FullName,Path,Name),
	openwrite(work, FullName),
	writedevice(work),
	rapport_CSS(CSSContent),
	metaTag(Eerst, Tag),
	write(Eerst),
	write("\n<HTML><HEAD>", Tag, "<TITLE>Rapport van de opgaven</TITLE></HEAD>"),
	write("\n<STYLE>"),
	write(CSSContent),
	write("\n</STYLE><BODY>"),
	write(Msg),
	write("<PRE>"),
	write("Toernooinaam: ", Toernooinaam, " Toernooinummer(s): ", NrsStr, "\nToernooidatum: ", Datum), 
	write("\nBekijk het spelerprofiel door op het bondsnummer te klikken."),
	date(_Year,Month,Day),
	helestr_int(0, Month1, Month),
	time(Hours,Minutes,_Seconds,_Hundredths),
	helestr_int(0, Minutes1, Minutes),
	format(DatumStr, "%d-%s %d:%s", Day, Month1, Hours, Minutes1),
	write("\nDatum rapport         :", DatumStr),
	write("</PRE>"),
	write("\n<H4>",Titel,"</H4>"),
	member(Sel, Itemlist),
	searchchar(Sel, '\t', P1),
	frontstr(P1,Sel,_,S1),
	searchchar(S1,'\t',P2),
	frontstr(P2,S1,_,Kop),
	wc(Cat, _, _, Kop, EDG, _, _, _, _, _, _, _, _, _,_),
	assert(metRanglijst(b_False)),
	write("\n<H4>",Kop,"</H4>"),
	retractall(algeweestRating(_)),
	findall(Id,  teamIdRating(Cat, Id), IdL),
	count(IdL, 0, Aantal),
	maakTable(EDG),
        teamsOpVolgordeVanRating(IdL, CatU, OpgNoU, Aantal),
	toon_rating(OpgNoU, CatU),
	fail.
ratingrapport(_ItemList, _Titel, _) :-
	closefile(work),
	naam(_ToernooiNaam, _, _Datum, WT, _, _),
	getFolder(Path,_,_,_),
	format(Name, "ratings%.html", WT),
	filenamepath(FullName,Path,Name),
	format(URL1, "file:\\\\%s", FullName),
	browser_ctl(URL1).

  dlg_indelingsrapport_Create(Parent, _Mode,_Titel):-
	%assert(preselected(Selected)),
	win_CreateDynDialog(Parent,
		[
%BEGIN Indelingsrapport, WinDefList, 11:01:51-20.4.2015, Code automatically updated!
		 dlg_font(wdef(dlg_indelingsrapport_DlgType,dlg_indelingsrapport_RCT,dlg_indelingsrapport_Title,u_DlgBase),
		 	  dlg_indelingsrapport_Font,dlg_indelingsrapport_FSize,dlg_indelingsrapport_Flags),
		 ctl(wdef(wc_PushButton,rct(153,142,206,157),"&Opstelling",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(169,197,201,207),"&Cancel",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_LBox,rct(3,4,150,209),"",u_DlgBase),idci_ingedeeld,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_MultiSelect,wsf_UseTabStops]),
		 ctl(wdef(wc_PushButton,rct(152,20,178,29),"&Alle",u_DlgBase),idci_alle,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(152,34,178,43),"&Geen",u_DlgBase),idci_geen,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(153,110,206,125),"&Naar Word",u_DlgBase),idci_naarplakbord,[wsf_Default,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(153,78,206,93),"&Rating",u_DlgBase),idci_rating,[wsf_Default,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(153,94,206,109),"&Inschrijvingen",u_DlgBase),idci_opgaven,[wsf_Default,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(153,126,206,141),"Open &Plaatsen",u_DlgBase),idci_open_plaatsen,[wsf_Default,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(153,62,206,77),"O&verzicht",u_DlgBase),idci_overzicht,[wsf_Default,wsf_Group,wsf_TabStop])
%END Indelingsrapport, WinDefList
		],dlg_indelingsrapport_eh,0).

%BEGIN Indelingsrapport, idc_ok _CtlInfo
  dlg_indelingsrapport_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idci_ingedeeld),
	lbox_GetSel(LboxWin,ItemList,_IndexList),
	closefile(work),
	naam(ToernooiNaam, Nrs, Datum, Ticket, _, _),
	term_str(slist, Nrs, NrList),
	getFolder(Path,_,_,_),
	format(Name, "opstelling%.html", Ticket),
	filenamepath(FullName,Path,Name),
	openwrite(work,FullName),
	writedevice(work),
	metaTag(Eerst, Tag),
	write(Eerst),
	write("\n<HTML><HEAD>", Tag, "<TITLE>Rapport van de opstelling</TITLE></HEAD>"),
	write("\n<STYLE>"),
	rapport_CSS(CSSContent),
	write(CSSContent),
	write("\n</STYLE><BODY><pre>"),
	write("Toernooinaam: ", Toernooinaam, " Toernooinummer(s): ", NrList, "\nSpeeldata: ", Datum), 
	write("</pre>"),
	opstellingrap(Itemlist),
	write("\n</body>\n</html>"),
	closefile(work),
	writedevice(screen),
	format(URL1, "file:\\\\%s", FullName),
	browser_ctl(URL1), !.
%END Indelingsrapport, idc_ok _CtlInfo
%MARK Indelingsrapport, new events


%BEGIN Indelingsrapport, idc_cancel _CtlInfo
  dlg_indelingsrapport_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-
	win_Destroy(_Win),
	!.
%END Indelingsrapport, idc_cancel _CtlInfo

%BEGIN Indelingsrapport, idci_overzicht _CtlInfo
  dlg_indelingsrapport_eh(_Win,e_Control(idci_overzicht,_CtrlType,_CtrlWin,_CtlInfo),0):-
	findall(I,uppercat(I),IList),
	naam(ToernooiNaam, Nrs, Datum, WT, _, _),
	term_str(slist, Nrs, NrL),
	getFolder(Path,_,_,_),
	%get_afdrukmap(Dir),
	format(Name, "overzicht%.html", WT),
	filenamepath(FullName,Path,Name),
%
	%get_TempDir(_, TempFile),
	openwrite(work,FullName),
	writedevice(work),
	%naam(ToernooiNaam, Nr, Datum, _, _, _),
	metaTag(Eerst, Tag),
	write(Eerst),
	write("\n<HTML><HEAD>", Tag, "<TITLE>Rapport van de opstelling</TITLE></HEAD>"),
	write("\n<STYLE>"),
	rapport_CSS(CSSContent),
	write(CSSContent),
	write("\n</STYLE><BODY><pre>"),
	write("Toernooinaam: ", Toernooinaam, " Toernooinummer(s): ", NrL, "\nSpeeldata: ", Datum), 
	%toernooinummer speeldata verenigingsnummer vereniging lotingsdatum 
	%write("\nVerenigingsnummer:            Vereniging:\n"),
	write("</pre>"),
	write("\n<table>"),
	write("\n<tr><th>Hoofd-<br>ronde</th><th>Onderdeel</th><th>Kort</th><th >Speel<br>st</th><th colspan='2'>leeftijd<br>min-max</th>"),
	write("<th>aantal<br>opg</th><th>aantal<br>uitsl</th><th>speelst<br>min-max</th><th>leeft<br>min-max</th></tr>"),
	overviewRap(IList),
	write("\n</table>"),
	write("\n</body></html>"),
	closefile(work),
	format(URL1, "file:\\\\%s", FullName),
	browser_ctl(URL1), !.
%END Indelingsrapport, idci_overzicht _CtlInfo

%BEGIN Indelingsrapport, idci_opgaven _CtlInfo
  dlg_indelingsrapport_eh(_Win,e_Control(idci_opgaven,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idci_ingedeeld),
	lbox_GetSel(LboxWin,ItemList,_IndexList),
	Itemlist = [], !.
  dlg_indelingsrapport_eh(_Win,e_Control(idci_opgaven,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idci_ingedeeld),
	lbox_GetSel(LboxWin,ItemList,_IndexList),
	Ans = dlg_Ask("Volgorde","Gewenste volgorde",["&Inschrijven","&Speelsterkte","&Cancel"]),
	Ans <> resp_3,
	getFolder(TDir,_,_,_),
	filenamepath(FullName,TDir,"opgaven.html"),
	filenamepath(NawFF, TDir, "opgaven.txt"),
	maakopgaverapport(Itemlist, Ans, NawFF, FullName), !.
  dlg_indelingsrapport_eh(_Win,e_Control(idci_opgaven,_CtrlType,_CtrlWin,_CtlInfo),0):- !.
%END Indelingsrapport, idci_opgaven _CtlInfo

%BEGIN Indelingsrapport, idci_open_plaatsen _CtlInfo
  dlg_indelingsrapport_eh(_Win,e_Control(idci_open_plaatsen,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	TaskWin = vpi_GetTaskWin(),
  	openplaatsenRap(),
	%file_str(tmpconst,Text),
	dlg_rapport_Create(TaskWin, "Open plaatsen en partner gezocht","Open plaatsen en partner gezocht",b_False),
	%win_editor_Create(Taskwin, Text, "Open plaatsen en partner gezocht"),
	%win_Destroy(_Win), 
	!.
%END Indelingsrapport, idci_open_plaatsen _CtlInfo

%BEGIN Indelingsrapport, idci_rating _CtlInfo
  dlg_indelingsrapport_eh(_Win,e_Control(idci_rating,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idci_ingedeeld),
	lbox_GetSel(LboxWin,ItemList,_IndexList),
	ratingrapport(ItemList, "Deelnemers op volgorde van Rating/Ranking","<br>Deelnemers op volgorde van Rating/Ranking<br>"),
	win_destroy(_Win),  !.
%END Indelingsrapport, idci_rating _CtlInfo


%BEGIN Indelingsrapport, idci_naarplakbord _CtlInfo
  dlg_indelingsrapport_eh(_Win,e_Control(idci_naarplakbord,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idci_ingedeeld),
	lbox_GetSel(LboxWin,ItemList,_IndexList),
	voorKlembord(Itemlist, "", String),
	cb_Putstring(String),
	_A = dlg_MessageBox( "Naar Word", "De geselecteerde onderdelen staan op het klembord\nen kunnen in bijvoorbeeld Word geplakt worden.\n\nNB Ze moeten wel ingedeeld zijn!", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	!.
%END Indelingsrapport, idci_naarplakbord _CtlInfo

%BEGIN Indelingsrapport, e_Create
  dlg_indelingsrapport_eh(_Win,e_Create(_CreationData),0):-
	Mode = 2,
	%preselected(String),
	findall(I,opg_cat(I, _, Mode),IList),
	%selectIndex(IList, String, 0, IndexL),
	win_SetText(_Win, "te rapporteren onderdelen"),
	LboxWin = win_GetCtlHandle(_Win, idci_ingedeeld),
	lbox_setTabstops(LboxWin, [8,20,30]),
	lbox_Add(LboxWin, IList),
	%IndexL = [Index | _],
	%lbox_SetSel(LboxWin, Index, b_True),
	selectAlle(LboxWin),
	!.
  dlg_indelingsrapport_eh(_Win,e_Create(_CreationData),0):-
	LboxWin = win_GetCtlHandle(_Win, idci_ingedeeld),
  	selectAlle(Lboxwin), !.
%END Indelingsrapport, e_Create

%BEGIN Indelingsrapport, idci_geen _CtlInfo
  dlg_indelingsrapport_eh(_Win,e_Control(idci_geen,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idci_ingedeeld),
	selectGeen(LboxWin),
	!.
%END Indelingsrapport, idci_geen _CtlInfo

%BEGIN Indelingsrapport, idci_alle _CtlInfo
  dlg_indelingsrapport_eh(_Win,e_Control(idci_alle,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idci_ingedeeld),
	selectAlle(LboxWin),
	!.
%END Indelingsrapport, idci_alle _CtlInfo

  dlg_indelingsrapport_eh(_,_,_):-!,fail.

%END_DLG Indelingsrapport

%BEGIN_DLG Troostronde
/**************************************************************************
	Creation and event handling for dialog: Troostronde
**************************************************************************/

constants

%BEGIN Troostronde, CreateParms, 12:11:10-16.7.2011, Code automatically updated!
  dlg_troostronde_ResID = idd_troostronde
  dlg_troostronde_DlgType = wd_Modal
  dlg_troostronde_Help = idh_adresetiketten
%END Troostronde, CreateParms

database - troost
  single troostOK(boolean Return)
  single dialoognaam(string)
  
predicates

  dlg_troostronde_eh : EHANDLER

clauses

troostOK(b_False).
dialoognaam("").

  dlg_troostronde_Create(Parent, DialoogNaam, TroostReturn):-	% ook gebruikt voor 3/4 plaats en naamgeving
	assert(troostOK(b_False)),	
	assert(dialoognaam(DialoogNaam)),
	win_CreateResDialog(Parent,dlg_troostronde_DlgType,dlg_troostronde_ResID,dlg_troostronde_eh,0),
	troostOK(TroostReturn).

%BEGIN Troostronde, idc_ok _CtlInfo
  dlg_troostronde_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	KortWin = win_GetCtlHandle(_Win, idc_tkort),
	Kort = win_GetText(KortWin),
	trim_c(Kort),
	str_len(Kort, Len),
	Len > 8,
	format(Msg, "%s\nMaximaal 8 tekens!", Kort),
	dlg_MessageBox( "Korte Naam", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	win_SetFocus(KortWin),
	!.
  dlg_troostronde_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	NaamWin = win_GetCtlHandle(_Win, idc_troostnaam),
	Naam = win_GetText(NaamWin),
	wc(WC, _, _, Naam, _, _, _, _, _, _, _, _, _, _,_),
	%wc(WC,Naam,_,_Kort,_,_,_),
	nieuweronde(_, _, Eigen),
	not(member(WC, Eigen)),
	format(Msg, "%s\nNiet uniek!", Naam),
	dlg_MessageBox( "Naam", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	win_SetFocus(NaamWin),
	!.
  dlg_troostronde_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	KortWin = win_GetCtlHandle(_Win, idc_tkort),
	Kort = win_GetText(KortWin),
	trim_c(Kort),
	wc(WC, _, _, _, _, Kort, _, _, _, _, _, _, _, _,_),
	%wc(WC,_Naam,_,Kort,_,_,_),
	nieuweronde(_, _, Eigen),
	not(member(WC, Eigen)),
	format(Msg, "%s\nNiet uniek!", Kort),
	dlg_MessageBox( "Korte Naam", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	win_SetFocus(KortWin),
	!.
  dlg_troostronde_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	NaamWin = win_GetCtlHandle(_Win, idc_troostnaam),
	KortWin = win_GetCtlHandle(_Win, idc_tkort),
	Naam = win_GetText(NaamWin),
	Kort = win_GetText(KortWin),
	trim_c(Kort),
	assert(nieuweronde(Naam, Kort, [])),
	assert(troostOK(b_True)),
	win_Destroy(_Win),
	!.
%END Troostronde, idc_ok _CtlInfo
%MARK Troostronde, new events

%BEGIN Troostronde, idc_cancel _CtlInfo
  dlg_troostronde_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-
	win_Destroy(_Win),
	!.
%END Troostronde, idc_cancel _CtlInfo

%BEGIN Troostronde, e_Create
  dlg_troostronde_eh(_Win,e_Create(_CreationData),0):-!,
	NaamWin = win_GetCtlHandle(_Win, idc_troostnaam),
	KortWin = win_GetCtlHandle(_Win, idc_tkort),
	nieuweronde(Naam, Kort, _),
	win_SetText(NaamWin, Naam),
	win_SetText(KortWin, Kort),
	dialoognaam(DNaam),
	win_SetText(_Win, DNaam),
	!.
%END Troostronde, e_Create

  dlg_troostronde_eh(_,_,_):-!,fail.

%END_DLG Troostronde

% Deel In 1 _______________________________________________________

constants 
  checkdeplaatsing = 836
  enablesplitsen = 837

predicates
  getVorm(WINDOW, rsoort) 
  getTijden(WINDOW, integerlist) 
  eerste_dagen(WINDOW, rsoort, integerlist)
  dagen2Nums(slist, integerlist)
  zetVorm(WINDOW, wedscat, integer) - (i,i,i)
  zetVorm1(WINDOW, rsoort, integer AaantalOpg) 
  determ onderhavigeCat(window Win, wedscat Cat)
  procedure openWijzigOpstelling(window, boolean SkeletOnly)
  nondeterm grootteGepl(integer, integer, integer)
  determ opgavenAfgekeurd(wedscat) % niet- of afgekeurd
  determ isSplitsen(window)
  determ tryIndeling(wedscat, rlist,integer,integerlist,integerlist, integer,integer, rsoort, boolean) - (i,i,i,i,o,i,o,i,i)

clauses

openWijzigOpstelling(Win, b_True) :-
  1 = dlg_MessageBox( "Skelet klaar", "Meteen vullen?\n(Gebruik daarbij de Insert toets)", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
  ParentWin = win_GetParent(Win),
  %win_PostEvent(ParentWin,e_menu),
  close_alles(),
  onderhavigeCat(Win, Cat),
  pos(Cat, W, _, afval),
  W1 = 2 * W,
  win_Destroy(Win),
  win_Destroy(ParentWin),
  select(Cat, W1, 1,0,onbekend), !. 	% 1 = wijzig opstellingsmode
/*openWijzigOpstelling(Win, _) :-
  onderhavigeCat(Win, Cat),
  pos(Cat, _, Pos, _), !,
  select(Cat, Pos). */  
openWijzigOpstelling(_Win, _).

isSplitsen(Win) :-
  RbSplWin = win_GetCtlHandle(Win, idc_eerst_splitsen_in_aparte_delen),
  b_True = win_IsChecked(RbSplWin),
  onderhavigeCat(Win, Cat),
  ParentWin = win_GetParent(Win),
  win_Destroy(Win),
  dlg_splitsen_Create(ParentWin,Cat,CatNew),
  win_PostEvent(ParentWin, e_User(deelinrefresh, CatNew)),
  !.

getVorm(Win, afval) :-
  RbAfvWin = win_GetCtlHandle(Win, idcd_afval),
  b_True = win_IsChecked(RbAfvWin), !.
getVorm(_, poel).

dagen2Nums([],[]) :- !.
dagen2Nums([H1|Rest],[K|Out]) :-
  dag(K,H1,_), !,
  dagen2Nums(Rest,Out).

eerste_dagen(_,poel, []) :- !.
eerste_dagen(Win, _, Dagen) :-
  DagWin = win_GetCtlHandle(Win, idcd_lbdagen),
  lbox_GetSel(DagWin,ItemList,_IndexList),
  dagen2Nums(ItemList, Dagen).
  
getTijden(Win, Baan_tijden) :-
  getVorm(Win, Vorm),
  eerste_dagen(Win, Vorm, Dagen),
  findall(Baan_tijd,baan_dag(Dagen,Baan_tijd),Baan_tijden), !.

zetVorm(Win, Cat, _) :-
  ResWin = win_GetCtlHandle(Win, idcd_pl),
  win_SetText(ResWin, ""),
  kopl(Cat,verliezer,HCat,_),
  %wc(HCat,_,_,_,_,_,Cat),	% het is een troostronde
  findall(OpgaveNum, opg(OpgaveNum,Cat,_,_,_,_,_,_,_,_,_),OpgL),
  count(OpgL, 0, AantT),	% aantal reeds in troostronde
  findall(OpgaveX, opg(OpgaveX,HCat,_,_,_,_,_,_,_,_,_),OpgLX),
  count(OpgLX, 0, AantH),	% aantal in hoofdronde
  card(AantH, Card),
  AanbevolenOpen = Card div 2 - AantT,
  AanbevolenOpen > 2,
  str_int(AanbevOpenS, AanbevolenOpen),
  win_SetText(ResWin, AanbevOpenS),
  zetVorm1(Win, afval, AantT), !.
zetVorm(Win, Cat, Aantal) :-
  pos(Cat,_Card,_,Vorm),
  zetVorm1(Win, Vorm, Aantal), !.
zetVorm(Win, _Cat, Aantal) :-
  zetVorm1(Win, afval, Aantal), !.

zetVorm1(Win, _, Aantal) :-
  update_proj_toolbar(),
  dialog_SetState(Win, [enable(idcd_poule, b_False)]),
  %Aantal >= 2,
  Aantal < maxpoeldim,
  dialog_SetState(Win, [enable(idcd_poule, b_True)]),
  fail.
zetVorm1(Win, poel, Aantal) :-
  Aantal > 2,
  Aantal < maxpoeldim,
  RbAfvWin = win_GetCtlHandle(Win, idcd_afval),
  RbPoelWin = win_GetCtlHandle(Win, idcd_poule),
  win_CheckRadioButton(RbPoelWin, [RbAfvWin,RbPoelWin]),
  SWin = win_GetCtlHandle(Win, idc_alleen_skelet),
  win_Check(SWin, b_False),
  dialog_SetState(Win, [enable(idcd_lbdagen, b_False),
  			enable(idctd_eerste_ronde_op_dagen, b_False),
  			enable(idc_rekening_houden_met, b_False),
  			enable(idct_verhinderingen, b_False),
  			enable(idc_alleen_skelet,b_False)]),
  !.
zetVorm1(Win, _, _) :-
  RbAfvWin = win_GetCtlHandle(Win, idcd_afval),
  RbPoelWin = win_GetCtlHandle(Win, idcd_poule),
  win_CheckRadioButton(RbAfvWin, [RbAfvWin,RbPoelWin]),
  dialog_SetState(Win, [enable(idcd_lbdagen, b_True),
  			enable(idctd_eerste_ronde_op_dagen, b_True),
  			enable(idc_rekening_houden_met, b_True),
  			enable(idct_verhinderingen, b_True),
  			enable(idc_alleen_skelet,b_True)]),
  !.

onderhavigeCat(Win, Cat) :-
  PWin = win_GetParent(Win),
  LboxWin = win_GetCtlHandle(PWin, idcd_lbond),
  cTreeView::get_selected(LboxWin, CatNr),
  vink(Cat, CatNr), !.

grootteGepl(65, 128, 32).		% 17.11.2004
grootteGepl(65, 128, 16).
grootteGepl(33, 64, 16).
grootteGepl(33, 64, 8).
grootteGepl(17, 32, 8).
grootteGepl(17, 32, 4).
grootteGepl(9, 16, 4).
grootteGepl(9, 16, 2).
grootteGepl(5, 8, 2).
grootteGepl(2, 4, 1).
grootteGepl(0, 1, 0).

juistAantalGeplaatsten(Cat, Aantal, "4", 4) :-		% global
  isNRt(Cat),
  Aantal <= 16, !.
juistAantalGeplaatsten(Cat, _Aantal, "8", 8) :-		% global
  isNRt(Cat), !.
juistAantalGeplaatsten(Cat, Aantal, AantalS, Count) :-		% global
  not(isNRt(Cat)),
  grootteGepl(Van, TM, Count),
  Aantal >= Van,
  Aantal <= TM,
  str_int(AantalS, Count).

opgavenAfgekeurd(Cat) :-	% nietgekeurd
  wc(Cat, _, _, _, _, _, _, _, _, _, _, _, _, Tid, _),
  toernooiK(Tid,_),
  opg(_,Cat,_Tgnst,_,_,_,_,_,_,na,_), !.
opgavenAfgekeurd(Cat) :-	% afgekeurd
  wc(Cat, _, _, _, _, _, _, _, _, _, _, _, _, Tid, _),
  toernooiK(Tid,_),
  opg(_,Cat,_Tgnst,_,_,_,_,_,_,msg(_,_,_,_,_,_),_), !.


tryIndeling(Cat, OpgLi,GereserveerdT,IndelingIn,IndelingUit,_NumWedsIn,NumWedsUit,afval, ZCvermijden) :-
  indelinglist2tgnstlist(Cat, IndelingIn, TgL),
  checkGelijkeClubs(Cat, TgL, 0, Uit),
  Uit > 0,
  ZCvermijden = b_True,
  format(Msg, "Indicatie gelijke clubs in de eerste twee rondes: %u.\nOpnieuw loten om te poberen dit getal omlaag te krijgen?", Uit),
  1 = dlg_MessageBox( "Gelijke clubs vermijden", Msg, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ), !,
  trap(myloting(OpgLi,GereserveerdT,Indeling,NumWeds,afval),_,beep()),
  tryIndeling(Cat, OpgLi,GereserveerdT,Indeling,IndelingUit, NumWeds,NumwedsUit, afval, ZCvermijden).
tryIndeling(_Cat, _OpgLi,_GereserveerdT,Indeling,Indeling, NumWeds,Numweds, _GewensteVorm, _).

%BEGIN_DLG Deel In 1
/**************************************************************************
	Creation and event handling for dialog: Deel In 1
**************************************************************************/

constants

%BEGIN Deel In 1, CreateParms, 22:06:47-27.10.2011, Code automatically updated!
  dlg_deel_in_1_DlgType = wd_Modal
  dlg_deel_in_1_Title = "Deel In Details"
  dlg_deel_in_1_RCT = rct(50,40,218,286)
  dlg_deel_in_1_Flags = [wsf_Close,wsf_TitleBar]
  dlg_deel_in_1_Help = idh_afval_of_pouleschema_maken
  dlg_deel_in_1_Font = "Arial"
  dlg_deel_in_1_FSize = 10
%END Deel In 1, CreateParms

database - deelin1
single  zelfdeclubvermijden(boolean)

predicates

  dlg_deel_in_1_eh : EHANDLER

clauses

zelfdeclubvermijden(b_True).

  dlg_deel_in_1_Create(Parent):-
	win_CreateDynDialog(Parent,
		[
%BEGIN Deel In 1, WinDefList, 22:06:47-27.10.2011, Code automatically updated!
		 dlg_font(wdef(dlg_deel_in_1_DlgType,dlg_deel_in_1_RCT,dlg_deel_in_1_Title,u_DlgBase),
		 	  dlg_deel_in_1_Font,dlg_deel_in_1_FSize,dlg_deel_in_1_Flags),
		 ctl(wdef(wc_RadioButton,rct(12,66,120,76),"Eerst splitsen in aparte delen",u_DlgBase),idc_eerst_splitsen_in_aparte_delen,[wsf_Auto,wsf_TabStop,wsf_Group]),
		 ctl(wdef(wc_RadioButton,rct(12,77,73,87),"&Afvalschema",u_DlgBase),idcd_afval,[wsf_Auto,wsf_TabStop]),
		 ctl(wdef(wc_RadioButton,rct(12,88,44,98),"&Poule",u_DlgBase),idcd_poule,[wsf_Auto,wsf_TabStop]),
		 ctl(wdef(wc_LBox,rct(57,130,108,184),"",u_DlgBase),idcd_lbdagen,[wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_ExtendedSel]),
		 ctl(wdef(wc_CheckBox,rct(5,185,63,195),"Alleen &skelet",u_DlgBase),idc_alleen_skelet,[wsf_Auto,wsf_TabStop,wsf_Group]),
		 ctl(wdef(wc_CheckBox,rct(5,196,164,206),"Zelfde club in eerste ronde(s) extra vermijden",u_DlgBase),idc_zelfde_club,[wsf_Auto,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_Edit,rct(49,212,66,224),"",u_DlgBase),idcd_pl,[wsf_AlignLeft,wsf_Group,wsf_TabStop,wsf_AutoHScroll]),
		 ctl(wdef(wc_PushButton,rct(124,226,164,245),"&OK",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(39,233,79,245),"&Cancel",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(82,233,122,245),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_Text,rct(5,10,157,51),"",u_DlgBase),idctd_gegevens,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(5,214,46,224),"&Reserveren:",u_DlgBase),idctd_reserveren,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(67,214,127,224),"open plaatsen.",u_DlgBase),idctd_plaatsen,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(14,130,57,148),"Eerste ronde op dagen:",u_DlgBase),idctd_eerste_ronde_op_dagen,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(5,2,53,12),"Gegevens",u_DlgBase),idct_gegevens,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(14,110,111,129),"Enigszins rekening houden met verhinderingen:",u_DlgBase),idct_verhinderingen,[wsf_AlignLeft]),
		 ctl(wdef(wc_GroupBox,rct(2,102,166,210),"afvalschema ...",u_DlgBase),idc_rekening_houden_met,[]),
		 ctl(wdef(wc_GroupBox,rct(2,55,166,100),"Gewenste vorm",u_DlgBase),idc_gewenste_vorm,[])
%END Deel In 1, WinDefList
		],dlg_deel_in_1_eh,0).

%BEGIN Deel In 1, idc_ok _CtlInfo
  dlg_deel_in_1_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
        isSplitsen(_Win), !.
  dlg_deel_in_1_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	openbaar(ja),
	onderhavigeCat(_Win, Cat),
        opgavenAfgekeurd(Cat),
        1 <> dlg_MessageBox( "Indelen", "Er zijn opgaven afgekeurd of niet gekeurd door de KNLTB.\nDat kan de juistheid van de indeling in gevaar brengen.\nToch doorgaan?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
	!.
  dlg_deel_in_1_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	openbaar(ja),
	onderhavigeCat(_Win, Cat),
	getVorm(_Win, GewensteVorm),
	Gewenstevorm = afval,
	gegevens(_Win, Cat, Aantal,b_False),
	not(kopl(Cat,verliezer,_,_)),
	%not(wc(_,_,_,_,_,_,Cat)),		% geen verliezersronde
	findall(Gep, gep([Cat], Gep, _), GepList),
	count(GepList, 0, Count),
	findall(AI, juistAantalGeplaatsten(Cat, Aantal, _, AI), AIList),
	not(member(Count, AIList)),
	%not(juistAantalGeplaatstenGrenzen(Cat, Count, Aantal)),
	findall(ASl, juistAantalGeplaatsten(Cat, Aantal, ASl, _), ASList),
	list_to_string(ASList, " of ", ASString),
	format(Msg, "Aantal geplaatsten %u.\nJuist aantal geplaatsten %s.\nSVP verbeteren met de optie (ver)Plaats!", Count, ASString),
	concat(Msg, "\nJa: officiëel loten (en dus eerst plaatsen),\nNee: alleen voor planningdoeleinden.",Msg1), 
	1 = dlg_MessageBox( "Open toernooi", Msg1, mesbox_iconexclamation, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ), !,
	ParentWin = win_GetParent(_Win),
        win_Destroy(_Win),
       	dlg_plaatsing_Create(ParentWin, Cat, 0),
	win_PostEvent(ParentWin, e_User(deelinrefresh, Cat)),!.
  dlg_deel_in_1_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	getVorm(_Win, GewensteVorm),
	onderhavigeCat(_Win, Cat),
	gegevens(_Win, Cat, Aantal1,b_False),
	ResWin = win_GetCtlHandle(_Win, idcd_pl),
	ResS = win_GetText(ResWin),
	str_intp(ResS, Gereserveerd),
	checkGereserveerd(_Win, Cat, GewensteVorm, Aantal1, Gereserveerd), !.
  dlg_deel_in_1_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	getVorm(_Win, GewensteVorm),
	onderhavigeCat(_Win, Cat),
        getTijden(_Win, Baan_tijden), 
        gegevens(_Win, Cat, Aantal1,b_False),
        ResWin = win_GetCtlHandle(_Win, idcd_pl),
        ResS = win_GetText(ResWin),
        ZCWin = win_GetCtlHandle(_Win, idc_zelfde_club),
        ZCvermijden = win_IsChecked(ZCWin),
        assert(zelfdeclubvermijden(ZCvermijden)),
        str_intp(ResS, Gereserveerd),
        GereserveerdMin0 = 2 - Aantal1,
        grootste(0, GereserveerdMin0, GereserveerdMin),  % niet negatief
        grootste(Gereserveerd, GereserveerdMin, GereserveerdT),
        findall(AlGereserveerd, opg(_,Cat,openplaats(Algereserveerd),_,_,_,_,_,_,_,_), AlGereserveerdList),
        count(AlgereserveerdList, 0, ARAantal),
        Tereserveren = GereserveerdT - ARAantal,
        pouleOpenX(Cat, Tereserveren, Gewenstevorm),		% genereert voldoende open plaatsen voor poules
        logclose(),
        findall(Opgegevens,opgegdebug(Cat,Baan_tijden,Opgegevens,GereserveerdT),OpgLi),
        findall(Hx, opgegFout(Cat, Hx), HxList),
        mededeling(HxList), !,
        trap(myloting(OpgLi,GereserveerdT,Indeling0,NumWeds0,GewensteVorm),_,beep()),
 	tryIndeling(Cat, OpgLi,GereserveerdT,Indeling0, Indeling,Numweds0, NumWeds,GewensteVorm,ZCvermijden),
	%
	writedevice(screen),
	SWin = win_GetCtlHandle(_Win, idc_alleen_skelet),
	SkeletOnly = win_IsChecked(SWin),
	retractall(oddEven(_)),
	aanbrengen(Cat, NumWeds, Indeling, GewensteVorm, SkeletOnly),
	%retractall(modified()),
	%assert(modified()),
	openWijzigOpstelling(_Win, SkeletOnly),
	win_Destroy(_Win),
	!.
%END Deel In 1, idc_ok _CtlInfo
%MARK Deel In 1, new events

%BEGIN Deel In 1, e_User
  dlg_deel_in_1_eh(_Win,e_User(checkdeplaatsing,_Ptr),0):-
	onderhavigeCat(_Win, Cat),
	checkPlaatsing(Cat),
	!.
  dlg_deel_in_1_eh(_Win,e_User(enablesplitsen,Aantal),0):-
  	dialog_SetState(_Win, [enable(idc_eerst_splitsen_in_aparte_delen,b_False)]),
	onderhavigeCat(_Win, Cat),
	Aantal > 1,			% meer dan één opgave
	not(kopl(_, _, Cat, _)),	% niet derde/vierde plaats
	not(kopl(Cat, _, _, _)),
	%wc(Cat,_,_,_,_,_,Verl),
	not(kopl(_,verliezer,Cat,_)),
	%not(wc(Verl,_,_,_,_,_,_)),	% niet als ie een verliezersronde heeft
  	dialog_SetState(_Win, [enable(idc_eerst_splitsen_in_aparte_delen,b_True)]),
  	fail.
  dlg_deel_in_1_eh(_Win,e_User(enablesplitsen,Aantal),0):-
	Aantal > 1,
	onderhavigeCat(_Win, Cat),
	kopl(Cat, algemeen, _, algemeen),
	dialog_SetState(_Win, [enable(idc_eerst_splitsen_in_aparte_delen,b_True)]), !.
 
%END Deel In 1, e_User

%BEGIN Deel In 1, idc_help _CtlInfo
  dlg_deel_in_1_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Afvalofpouleschemamaken"),
	!.
%END Deel In 1, idc_help _CtlInfo

%BEGIN Deel In 1, idc_alleen_skelet _CtlInfo
  dlg_deel_in_1_eh(_Win,e_Control(idc_alleen_skelet,_CtrlType,SWin,_CtlInfo),0):-
	b_True = win_IsChecked(SWin),
	dialog_SetState(_Win, [enable(idcd_lbdagen, b_False),
		enable(idctd_eerste_ronde_op_dagen, b_False)]),
	!.
  dlg_deel_in_1_eh(_Win,e_Control(idc_alleen_skelet,_CtrlType,_SWin,_CtlInfo),0):-
	dialog_SetState(_Win, [enable(idcd_lbdagen, b_True),
		enable(idctd_eerste_ronde_op_dagen, b_True)]),
	!.
%END Deel In 1, idc_alleen_skelet _CtlInfo

%BEGIN Deel In 1, idc_cancel _CtlInfo
  dlg_deel_in_1_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	win_Destroy(_Win),
	!.
%END Deel In 1, idc_cancel _CtlInfo

%BEGIN Deel In 1, e_Create
  dlg_deel_in_1_eh(_Win,e_Create(_CreationData),0):-!,
	Font = font_Create(ff_Fixed, [fs_Bold], 9),
	GegWin = win_GetCtlHandle(_Win, idctd_gegevens),
	win_SetFont(GegWin, Font),
	DagWin = win_GetCtlHandle(_Win, idcd_lbdagen),
	win_SetFont(DagWin, Font),
	Geg1Win = win_GetCtlHandle(_Win, idct_gegevens),
	win_SetFont(Geg1Win, Font),
	findall(Dag, dag(_, Dag, _), Dagen),
	lbox_Add(DagWin, Dagen),
	onderhavigeCat(_Win, Cat),
	gegevens(_Win, Cat, AantalSel,b_False),
	zetVorm(_Win, Cat, AantalSel),
	win_PostEvent(_Win,e_User(checkdeplaatsing, 0)),
        findall(Num, opg(Num, Cat, _, _, _, _, _, _, _, _, _), Numlist),	
        count(Numlist, 0, Aantal),
	win_PostEvent(_Win,e_User(enablesplitsen, Aantal)),
	zelfdeclubvermijden(ZCV),
	ZCWin = win_GetCtlHandle(_Win, idc_zelfde_club),
	win_Check(ZCWin, ZCV),
	!.
%END Deel In 1, e_Create

%BEGIN Deel In 1, idcd_poule _CtlInfo
  dlg_deel_in_1_eh(_Win,e_Control(idcd_poule,_CtrlType,PWin,_CtlInfo),0):-
	b_True = win_IsChecked(PWin),
	dialog_SetState(_Win, [enable(idcd_lbdagen, b_False),
		enable(idctd_eerste_ronde_op_dagen, b_False),
		enable(idc_alleen_skelet,b_False),
  			enable(idc_rekening_houden_met, b_False),
  			enable(idct_verhinderingen, b_False)
  		]),
	!.
%END Deel In 1, idcd_poule _CtlInfo

%BEGIN Deel In 1, idcd_afval _CtlInfo
  dlg_deel_in_1_eh(_Win,e_Control(idcd_afval,_CtrlType,AWin,_CtlInfo),0):-
	b_True = win_IsChecked(AWin),
	dialog_SetState(_Win, [enable(idcd_lbdagen, b_True),
	enable(idctd_eerste_ronde_op_dagen, b_True),
  		enable(idc_rekening_houden_met, b_True),
  		enable(idct_verhinderingen, b_True),
	enable(idc_alleen_skelet,b_True)]),
	SWin = win_GetCtlHandle(_Win, idc_alleen_skelet),
	b_True = win_IsChecked(SWin),
	dialog_SetState(_Win, [enable(idcd_lbdagen, b_False),
  			enable(idc_rekening_houden_met, b_False),
  			enable(idct_verhinderingen, b_False),
		enable(idctd_eerste_ronde_op_dagen, b_False)]),
	!.
%END Deel In 1, idcd_afval _CtlInfo

  dlg_deel_in_1_eh(_,_,_):-!,fail.

%END_DLG Deel In 1

%BEGIN_DLG Candidatenvergelijking
/**************************************************************************
	Creation and event handling for dialog: Candidatenvergelijking
**************************************************************************/

constants

%BEGIN Candidatenvergelijking, CreateParms, 15:41:41-31.7.2009, Code automatically updated!
  dlg_candidatenvergelijking_DlgType = wd_Modal
  dlg_candidatenvergelijking_Title = "Invoer van inschrijvingen"
  dlg_candidatenvergelijking_RCT = rct(50,40,319,179)
  dlg_candidatenvergelijking_Flags = [wsf_Close,wsf_TitleBar]
  dlg_candidatenvergelijking_Help = idh_adresetiketten
  dlg_candidatenvergelijking_Font = "Arial"
  dlg_candidatenvergelijking_FSize = 9
%END Candidatenvergelijking, CreateParms

database - cand
single spcand(string Speler, slist Cand)
single gekozen(string)
single inr(string)

predicates

  dlg_candidatenvergelijking_eh : EHANDLER

clauses

spcand("", []).
inr("").
gekozen("").

  dlg_candidatenvergelijking_Create(Parent,Speler, Candidaten,NaamO, INr):-
  	assert(inr(INr)),	% inschrijfnummer
	assert(spcand(Speler, Candidaten)),
	assert(gekozen("")),
	win_CreateDynDialog(Parent,
		[
%BEGIN Candidatenvergelijking, WinDefList, 15:41:41-31.7.2009, Code automatically updated!
		 dlg_font(wdef(dlg_candidatenvergelijking_DlgType,dlg_candidatenvergelijking_RCT,dlg_candidatenvergelijking_Title,u_DlgBase),
		 	  dlg_candidatenvergelijking_Font,dlg_candidatenvergelijking_FSize,dlg_candidatenvergelijking_Flags),
		 ctl(wdef(wc_PushButton,rct(223,123,263,136),"&OK",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop,wsf_Disabled]),
		 ctl(wdef(wc_PushButton,rct(137,123,187,135),"&Stop import",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(11,124,51,136),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop,wsf_Invisible]),
		 ctl(wdef(wc_LBox,rct(5,29,263,91),"",u_DlgBase),idd_lb,[wsf_OwnerDraw,wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_HasStrings]),
		 ctl(wdef(wc_Text,rct(5,2,261,30),"Inspeler",u_DlgBase),idct_inspeler,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(6,92,263,121),"Niet zeker",u_DlgBase),idct_niet_zeker,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(63,124,135,136),"Opgave: .....",u_DlgBase),idct_opgave,[wsf_AlignLeft])
%END Candidatenvergelijking, WinDefList
		],dlg_candidatenvergelijking_eh,0),
		gekozen(NaamO).

%BEGIN Candidatenvergelijking, idc_ok _CtlInfo
  dlg_candidatenvergelijking_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idd_lb),
	lbox_GetSel(LboxWin,ItemList,_IndexList),
	ItemList = [Item|_],
	assert(gekozen(Item)),
	win_Destroy(_Win),
	!.
  dlg_candidatenvergelijking_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	assert(gekozen("")),
	win_Destroy(_Win),
	!.
%END Candidatenvergelijking, idc_ok _CtlInfo
%MARK Candidatenvergelijking, new events

%BEGIN Candidatenvergelijking, idd_lb selchanged
  dlg_candidatenvergelijking_eh(_Win,e_Control(idd_lb,_CtrlType,_CtrlWin,selchanged),0):-
	OWin = win_GetCtlHandle(_Win, idc_ok),
	win_SetState(OWin, [wsf_Enabled]),
	!.
%END Candidatenvergelijking, idd_lb selchanged

%BEGIN Candidatenvergelijking, idd_lb activated
  dlg_candidatenvergelijking_eh(_Win,e_Control(idd_lb,_CtrlType,_CtrlWin,activated),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idd_lb),
	lbox_GetSel(LboxWin,ItemList,_IndexList),
	ItemList = [Item|_],
	assert(gekozen(Item)),
	win_Destroy(_Win),
	!.
%END Candidatenvergelijking, idd_lb activated

%BEGIN Candidatenvergelijking, idc_cancel _CtlInfo
  dlg_candidatenvergelijking_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-
        assert(gekozen("-- geen passsende persoon --")),
	win_Destroy(_Win),
	setsoftstop(),
	!.
%END Candidatenvergelijking, idc_cancel _CtlInfo

%BEGIN Candidatenvergelijking, e_OwnerMeasureItem
  dlg_candidatenvergelijking_eh(_Win,e_OwnerMeasureItem(_CtlType,_CtlId,_ItemId,_Data),Size):-!,
	win_GetTextExtent(_Win, "JansenenTilanusg", -1, Width, He),
	Height = He,
	bitleft(Height, 16, H1),
	Size = Width + H1,
	!.
%END Candidatenvergelijking, e_OwnerMeasureItem

%BEGIN Candidatenvergelijking, e_OwnerDraw
  dlg_candidatenvergelijking_eh(_Win,e_OwnerDraw(_CtlType,_CtlId,ItemId,_ItemAction,
		ItemState,LboxWin,RectItem,_ItemData),0):-
	own_member1(ItemState, odstate_Selected), !,  % was selected
	win_SetPen(LboxWin,pen(1,ps_Solid,color_Blue)),
	win_SetBrush(LboxWin,brush(pat_Solid, color_Blue)),
	draw_Rect(LboxWin,RectItem),
	win_SetForeColor(LboxWin, color_White),
	win_SetBackColor(LboxWin, color_Blue),
	Item = lbox_GetItem(LboxWin, ItemId),
	draw_TextInRect(LboxWin,RectItem, Item, -1,[dtext_left]),
	!.
  dlg_candidatenvergelijking_eh(_Win,e_OwnerDraw(_CtlType,_CtlId,ItemId,_ItemAction,
		_ItemState,LboxWin,RectItem,_ItemData),0):-
	Item = lbox_GetItem(LboxWin, ItemId),
	win_SetPen(LboxWin,pen(1,ps_Solid,color_White)),
	win_SetBrush(LboxWin,brush(pat_Solid, color_White)),
	draw_Rect(LboxWin,RectItem),
	win_SetForeColor(LboxWin, color_Black),
	win_SetBackColor(LboxWin, color_White),
	draw_TextInRect(LboxWin,RectItem, Item, -1,[dtext_left]),
	!.
%END Candidatenvergelijking, e_OwnerDraw

%BEGIN Candidatenvergelijking, e_Destroy
  dlg_candidatenvergelijking_eh(_Win,e_Destroy,0):-!,
	assert(spcand("", [])),
	!.
%END Candidatenvergelijking, e_Destroy

%BEGIN Candidatenvergelijking, e_Create
  dlg_candidatenvergelijking_eh(_Win,e_Create(_CreationData),0):-!,
	spcand(Speler, Cand),
	format(Aanw, "De in te voeren persoon \"%s\" zou kunnen overeen komen met een persoon reeds in het bestand! Selecteer svp!", Speler),
	TWin = win_GetCtlHandle(_Win, idct_inspeler),
	win_SetText(TWin, Aanw),
	NWin = win_GetCtlHandle(_Win, idct_niet_zeker),
	M1 = "Als je het niet zeker weet, kies dan \"geen passende persoon\" en loop de inschrijving achteraf na.\n",
	M2 = "Achteraf kun je gelijkbedoelde spelers samenvoegen via Spelerselectie (F2)",
	concat(M1, M2, Msg),
	win_SetText(NWin, Msg),
	LboxWin = win_GetCtlHandle(_Win, idd_lb),
	lbox_Add(LboxWin, Cand),
	lbox_Add(LboxWin, "-- geen passsende persoon --"),
	OWin = win_GetCtlHandle(_Win, idct_opgave),
	inr(InRef),
	format(INref1, "Opgave: %s", InRef),
	win_SetText(OWin, InRef1),
	%lbox_SetSel(LboxWin, 0, b_True),
	!.
%END Candidatenvergelijking, e_Create

  dlg_candidatenvergelijking_eh(_,_,_):-!,fail.

%END_DLG Candidatenvergelijking

%BEGIN_DLG Onderdeelgegevens
/**************************************************************************
	Creation and event handling for dialog: Onderdeelgegevens
**************************************************************************/

constants

%BEGIN Onderdeelgegevens, CreateParms, 22:21:31-31.8.2014, Code automatically updated!
  dlg_onderdeelgegevens_ResID = idd_onderdeelgegevens
  dlg_onderdeelgegevens_DlgType = wd_Modal
  dlg_onderdeelgegevens_Help = idh_adresetiketten
%END Onderdeelgegevens, CreateParms

predicates

  dlg_onderdeelgegevens_eh : EHANDLER
  ratinggrenzen(wedscat, string)

clauses

  ratinggrenzen(Cat,IdLS) :-
  	wc(Cat, OnderdeelID, _N, _Naam, _EDG, _Kort, _SexL, _SpStrk, _, _, _HDG, _Schmasoort, _Keuring, _Tnr,_Geld), !,
  	schemasoortAR(Cat, OnderdeelId),
	findall(Id,  teamIdRating(Cat, Id), IdL),
	%count(IdL, 0, Aantal),
	sortstring_c(IdL, 1),
	list_to_string(IdL, "\n", IdLS),
	write(IdLS).
  	%member(Str, IdL),
  	%parseTabs(Str, ListX),
  	%ListX = [_, _, CatS, NoS | _],
  	%str_int(CatS, CatU),
  	%str_int(NoS, NoU).

  dlg_onderdeelgegevens_Create(Parent, Return):-
	assert(troostOK(b_False)),	
	win_CreateResDialog(Parent,dlg_onderdeelgegevens_DlgType,dlg_onderdeelgegevens_ResID,dlg_onderdeelgegevens_eh,0),
	troostOK(Return).

%BEGIN Onderdeelgegevens, idc_ok _CtlInfo
  dlg_onderdeelgegevens_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	NaamWin = win_GetCtlHandle(_Win, idc_troostnaam),
	Naam = win_GetText(NaamWin),
	wc(WC, _, _, Naam, _, _, _, _, _, _, _, _, _, _,_),
	nieuweronde(_, _, Eigen),
	not(member(WC, Eigen)),
	format(Msg, "%s\nNiet uniek!", Naam),
	dlg_MessageBox( "Naam", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	win_SetFocus(NaamWin),
	!.
  dlg_onderdeelgegevens_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	KortWin = win_GetCtlHandle(_Win, idc_tkort),
	Kort = win_GetText(KortWin),
	trim_c(Kort),
	wc(WC, _, _, _, _, Kort, _, _, _, _, _, _, _, _,_),
	nieuweronde(_, _, Eigen),
	not(member(WC, Eigen)),
	format(Msg, "%s\nNiet uniek!", Kort),
	dlg_MessageBox( "Korte Naam", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	win_SetFocus(KortWin),
	!.
  dlg_onderdeelgegevens_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	NaamWin = win_GetCtlHandle(_Win, idc_troostnaam),
	KortWin = win_GetCtlHandle(_Win, idc_tkort),
	Naam = win_GetText(NaamWin),
	Kort = win_GetText(KortWin),
	trim_c(Kort),
	nieuweronde(_, _, CatL),
	assert(nieuweronde(Naam, Kort, CatL)),
	assert(troostOK(b_True)),
	win_Destroy(_Win),
	!.
%END Onderdeelgegevens, idc_ok _CtlInfo
%MARK Onderdeelgegevens, new events

%BEGIN Onderdeelgegevens, idc_cancel _CtlInfo
  dlg_onderdeelgegevens_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	win_Destroy(_Win),
	!.
%END Onderdeelgegevens, idc_cancel _CtlInfo

%BEGIN Onderdeelgegevens, e_Create
  dlg_onderdeelgegevens_eh(_Win,e_Create(_CreationData),0):-!,
	NaamWin = win_GetCtlHandle(_Win, idc_troostnaam),
	KortWin = win_GetCtlHandle(_Win, idc_tkort),
	MeldingWin = win_GetCtlHandle(_Win, idcp_edit),
	VanAfWin = win_GetCtlHandle(_Win, idc_vanaf),
	TMWin = win_GetCtlHandle(_Win, idc_tm),
	StWin = win_GetCtlHandle(_Win, idc_speelst),
	SchWin = win_GetCtlHandle(_Win, idc_schemasoort),
	nieuweronde(Naam, Kort, _),
	win_SetText(NaamWin, Naam),
	win_SetText(KortWin, Kort),
	nieuweronde(_, _, [Cat]),
	wc(Cat, _, _, _, _EDG, _Kort, _SexL, Speelsterkte, leeftijd(LeeftijdVanAf,_), leeftijd(LeeftijdTM,_), _HDG, Schemasoort, _Keuring, ToernooiId,_Geld),
	%wcrit(Cat, Speelsterkte, LeeftijdVanAf, LeeftijdTotenMet, _),
	%str_intP(LeeftijdVA, LeeftijdVanAf),
	win_SetText(VanafWin, LeeftijdVanAf),
	%str_intP(LeeftijdTM, LeeftijdTotEnMet),
	win_SetText(TMWin, LeeftijdTM),
	win_SetText(StWin, Speelsterkte),
	win_SetText(SchWin, Schemasoort),
        %toernooionderdeel_t(Cat, _Naam, Speelsterkte, _ED, _HDG, _LV, _LEnMet, _Status, MeldingOnderdeel), % kan failen!
        toernooikP(ToernooiId, Toernooisoort),
        format(Msg, "Onderdeel hoort bij %s toernooi %s",Toernooisoort, ToernooiId),
	win_SetText(MeldingWin, Msg),
	ratinggrenzen(Cat, _Out),
	!.
%END Onderdeelgegevens, e_Create

  dlg_onderdeelgegevens_eh(_,_,_):-!,fail.

%END_DLG Onderdeelgegevens

%BEGIN_DLG Uitloten
/**************************************************************************
	Creation and event handling for dialog: Uitloten
**************************************************************************/

constants

%BEGIN Uitloten, CreateParms, 11:17:46-10.1.2006, Code automatically updated!
  dlg_uitloten_DlgType = wd_Modal
  dlg_uitloten_Title = "Wizzard Uitloten"
  dlg_uitloten_RCT = rct(50,40,220,203)
  dlg_uitloten_Flags = [wsf_Close,wsf_TitleBar]
  dlg_uitloten_Help = idh_adresetiketten
%END Uitloten, CreateParms

database - uitloot
  opgaveU(integer, wedscat, tgnst, integer, real, string, string, string, string, integer Uitsl, char FormGezien, char)
  single maxleng(integer)
  single uitlootCat(wedscat)
  single metweekends(boolean)
  
predicates

  dlg_uitloten_eh : EHANDLER
  maakUitlootRapport(wedscat Eigen, string Wanneertekst, boolean WkEnd)
  procedure uniekeOpgaven(catl, boolean Avond, boolean Hele, boolean WkEnd)
  determ tgnstWaarden(tgnst, integerlist Tijden, integer MaxCats, integer Verhinderingen, string Cats, string Cats, string Naam1, string Naam2, char, char)
  nondeterm opgaveSortKey(string, integer)
  procedure filterGekoppeld(catl, catl, catl, catl) - (i,i,i,o)
  procedure catl2strl(catl, string, string CatsStr1)
  procedure dubbelkolommen(wedstyp, integer MaxL, integer, integer, string)  
  procedure uitslTekst(integer, string)
  nondeterm sp2opgNietUitsl(integer Sp, wedscat Cat)
  determ    tijdgeblokkeerd(verhl, integer Tijd, integer Duur)
  procedure bloktijdToepassen(catl, integerlist, integerlist)
  procedure lftblok(string Geb, verhl Verh, verhl VerhTot)
  nondeterm bnF(integer T, boolean Avond, boolean Hele, boolean MetWeekEnd)
  nondeterm bnF(boolean, boolean, integer Uur, wrk, boolean MetWeekend)
  procedure wanneertekst(boolean Avond, boolean Hele, string)
  procedure bet2gezien(janee, char)
  determ    checkMaxSlots(integer)
  procedure weekendsmeetellen(boolean)
  
clauses

maxleng(0).
uitlootCat(0).
metweekends(b_False).

catl2strl([Cat|Rest], In, Uit) :-
  wc(Cat, _, _, _, _, Kort, _, _, _, _, _, _, _, _,_),
  %wc(Cat,_Naam,_,Kort,_,_,_),
  format(In1, "%s %s", In, Kort), !,
  catl2strl(Rest, In1, Uit).
catl2strl(_, In, Uit) :-
  trim_c(In),
  Uit = In,
  str_len(In, Len),
  maxleng(X),
  grootste(X, Len, LenG),
  assert(maxleng(LenG)).

filterGekoppeld(Cat1All, [Cat1|Rest], In, Uit) :-
  bovenCat(Cat1, CatBoven),
  not(Cat1 = CatBoven),
  member(CatBoven, Cat1All), !,
  filterGekoppeld(Cat1All, Rest, In, Uit).
filterGekoppeld(Cat1All, [Cat1|Rest], In, [Cat1|Uit]) :- !,
  filterGekoppeld(Cat1All, Rest, In, Uit).
filterGekoppeld(_Cat1All, _, In, In).

sp2opgNietUitsl(Sp, Cat) :-
  sp2opg(Sp,Cat,OpgNo,_,_,_),
  opg(OpgNo,Cat,_,_,_,0,_,_,_,_,_).     


lftblok(Geb, Verh, VerhTot) :-
  leeftijd(Geb, Leeft, 0, _),
  blLft(LftVan, LftTot, Verhl),	% leeftijdafhankelijke blokkering
  Leeft >= LftVan,
  Leeft <= LftTot, !,
  append(Verh, Verhl, VerhTot).
lftblok(_, Verh, Verh).

bet2gezien(nee, 'N') :- !.
bet2gezien(_, ' ').

tgnstWaarden(p(Sp1, Sp2), Tijden, MaxCats, Slots, CatsStr1, CatsStr2,Naam1,Naam2,Gezien1, Gezien2) :-
  sp2(Sp1,_,Naam1,_,Verh1,_,_,_Bet1,UitKant1,_,_, _,_,_,_,Geb1, _,_,_,_,Gezien1a,_,_,_,_,_,_), 
  lftblok(Geb1, Verh1, Verh11),
  bet2gezien(Gezien1a, Gezien1),
  sp2(Sp2,_,Naam2,_,Verh2,_,_,_Bet2,UitKant2,_,_, _,_,_,_,Geb2, _,_,_,_,Gezien2a,_,_,_,_,_,_), 
  bet2gezien(Gezien2a, Gezien2),
  lftblok(Geb2, Verh2, Verh21),
  findall(Cat1x, sp2opgNietUitsl(Sp1,Cat1x),Cat1L),
  filterGekoppeld(Cat1L, Cat1L, [], Cat1LF),
  catl2strl(Cat1L, "", CatsStr1),
  count(Cat1LF, 0, CatAantal1),  
  findall(Cat2x, sp2opgNietUitsl(Sp2,Cat2x),Cat2L),
  filterGekoppeld(Cat2L, Cat2L, [], Cat2LF),
  catl2strl(Cat2L, "", CatsStr2),
  count(Cat2LF, 0, CatAantal2),
  MaxCats = Cataantal1 + Cataantal2,
  %grootste(CatAantal1, CatAantal2, MaxCats),
  grootste(UitKant1, UitKant2, UitKant),
  append(Verh11, Verh21, Verh),
  timegrid(Tijden),
  slots(Verh, UitKant, Slots),
  !.
tgnstWaarden(pw(Sp1), Tijden, MaxCats, Slots, CatsStr1, "",Naam1,"",Gezien1, ' ') :-
  sp2(Sp1,_,Naam1,_,Verh1,_,_,_Bet1,UitKant1,_,_, _,_,_,_,Geb1, _,_,_,_,Gezien1a,_,_,_,_,_,_), 
  lftblok(Geb1, Verh1, Verh11),
  bet2gezien(Gezien1a, Gezien1),
  findall(Cat1x, sp2opgNietUitsl(Sp1,Cat1x),Cat1L),
  filterGekoppeld(Cat1L, Cat1L, [], Cat1LF),
  catl2strl(Cat1L, "", CatsStr1),
  count(Cat1LF, 0, CatAantal1),  
  MaxCats = Cataantal1,
  UitKant = UitKant1,
  Verh = Verh11,
  timegrid(Tijden),
  slots(Verh, UitKant, Slots),
  !.
tgnstWaarden(s(Sp1), Tijden, CatAantal1, Slots, CatsStr1, "",Naam1,"", Gezien1, ' ') :-
  sp2(Sp1,_,Naam1,_,Verhl,_,_,_Bet1,UitKant1,_,_, _,_,_,_,Geb1, _,_,_,_,Gezien1a,_,_,_,_,_,_), !,
  lftblok(Geb1, Verhl, Verh11),
  bet2gezien(Gezien1a, Gezien1),
  findall(Cat1x, sp2opgNietUitsl(Sp1,Cat1x),Cat1L),
  filterGekoppeld(Cat1L, Cat1L, [], Cat1LF),
  catl2strl(Cat1L, "", CatsStr1),
  count(Cat1LF, 0, CatAantal1),
  timegrid(Tijden),
  slots(Verh11, UitKant1, Slots).

bloktijdToepassen([Cat], [Tijd|Tijden], TijdenF) :-
  bl(Cat, VerhList),
  duur(Duur),
  tijdgeblokkeerd(VerhList, Tijd, Duur), !,
  blokTijdToepassen([Cat], Tijden, TijdenF).
bloktijdToepassen([Cat], [Tijd|Tijden], [Tijd|TijdenF]) :- !,
  blokTijdToepassen([Cat], Tijden, TijdenF).
bloktijdToepassen(_, Tijden, Tijden).

tijdgeblokkeerd([per(Van, Tot)|_], Tijd, Duur) :-
  Van - Duur < Tijd,
  Tot > Tijd, !.
tijdgeblokkeerd([_|Rest], Tijd, Duur) :-
  tijdgeblokkeerd(Rest, Tijd, Duur).

bnF(T, Avond, Hele, WkEnd) :-
  bn(T, N),		% avond
  N > 0,		% geen uitwijktijdstip
  tijd_kwart(Dag, Uur, _Min, T),
  dag(Dag, _, Wrk),
  bnF(Avond, Hele, Uur, Wrk, WkEnd).

bnF(b_False, b_True, _Uur, z, b_True) :- !. 	% hele dag met weekenden
bnF(b_False, b_True, _Uur, w, _) :- !. 	% hele dag zonder weekenden
bnF(_Avond, _Hele, _Uur, z, b_True) :- !.	% weekend
bnF(b_True, b_False, Uur, w, _) :-
  Uur >= 18.				% avond
bnF(b_False, b_False, Uur, w, _) :-
  Uur <= 18.				% overdag

uniekeOpgaven(CatL, Avond, Hele, WkEnd) :-
  assert(maxleng(0)),
  retractall(opgaveU(_, _, _, _, _, _, _, _, _, _, _, _)),
  findall(T, bnF(T, Avond, Hele, WkEnd), Tijden),
  sortint_c(Tijden),
  bloktijdToepassen(CatL, Tijden, TijdenF),
  timegrid(TijdenF),
  slots([], 0, MaxSlots), 
  checkMaxSlots(MaxSlots), 
  member(Cat, CatL),
  opg(Opg,Cat,Tgnst,_,_,U,_,_,_,_,_),
  not(opgaveU(_, _, Tgnst, _, _, _, _, _, _, _, _, _)),
  tgnstWaarden(Tgnst, TijdenF, MaxCats, Slots, CatS1, Cats2, Naam1, Naam2, Gezien1, Gezien2),
  SlotsVerhinderd = (MaxSlots - Slots) / MaxSlots * 100,
  assert(opgaveU(Opg, Cat, Tgnst, MaxCats, SlotsVerhinderd, Cats1, Cats2, Naam1, Naam2, U, Gezien1, Gezien2)),
  fail.
uniekeOpgaven(_CatL, _, _, _).

checkMaxSlots(0) :- !,
  dlg_MessageBox( "Verhinderingen", "Vul eerst de baanbeschikbaarheid realistisch in!\nZie Toernooi, Baanbeschikbaarheid.", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.
checkMaxSlots(_).

opgaveSortKey(Keyx, 1) :-		% volgorde Verhinderfactor
  opgaveU(Opg, Cat, _Tgnst, MaxCats, Verhinderfactor, _, _, _, _, _, _, _),
  format(Keyx, "%5.2f\t%3u\t%u\t%u", Verhinderfactor, MaxCats, Opg, Cat).
opgaveSortKey(Keyx, 2) :-		% deelname andere cats
  opgaveU(Opg, Cat, _Tgnst, MaxCats, Verhinderfactor, _, _, _, _, _, _, _),
  format(Keyx, "%3u\t%5.2f\t%u\t%u", Maxcats, Verhinderfactor, Opg, Cat).

dubbelkolommen(e, _MaxL, 1, 1, "") :- !.
dubbelkolommen(_, MaxL, 18, MaxL, "Deelname").  

uitslTekst(0, "") :- !.
uitslTekst(_, "U") :- !.

wanneertekst(b_True, _Hele, "Doordeweeks wordt voornamelijk 's-avonds gespeeld.") :- !.
wanneertekst(b_False, b_False, "Doordeweeks wordt voornamelijk overdag gespeeld.") :- !.
wanneertekst(_, _, "Doordeweeks wordt zowel overdag als 's-avonds gespeeld.").

weekendsmeetellen(b_True) :-
  write("Weekends tellen mee.\n\n"), !.
weekendsmeetellen(_) :-
  write("Weekends tellen niet mee.\n\n"), !.

maakUitlootRapport(CatTop, Wanneertekst, WkEnd) :-
  closefile(work),
  get_TempDir(_, TempFile),
  openwrite(work,TempFile),
  writedevice(work),
  maxleng(MaxL0),
  str_len("Deelname", LD),
  grootste(MaxL0, LD, MaxL), 
  wc(CatTop, _, _, Naam, EDG, _, _, _, _, _, _, _, _, _,_),
  %wc(CatTop,Naam,EDG,_Kort,_,_,_),
  write("Onderdeel: ", Naam, "\n", Wanneertekst,  "\nN = formulier (nog) niet gezien.\nU = al uitgesloten.\n"),
  weekendsmeetellen(WkEnd),
  dubbelkolommen(EDG, MaxL, NaamKol, OpgKol, Onderdelen2Header),  
  format(FStr, "%%1s %%c %%-18.18s %%-%us %%c %%-%u.%us %%-%us %%3.0f%%%% %%3u\n", MaxL, NaamKol, NaamKol, OpgKol),
  format(TStr, "%%1s %%c %%-18.18s %%-%us %%c %%-%u.%us %%-%us Verh Ond\n", MaxL, NaamKol, NaamKol, OpgKol),
  write("Deel 1: Volgorde beschikbaarheid (meeste verhinderingen bovenaan).\n"),
  writef(TStr, " ", ' ', "", "Deelname", ' ', "", Onderdelen2Header),  
  findall(Keyx, opgaveSortKey(Keyx,1), KeyList),
  sortstring_c(KeyList, 1),
  reverse(KeyList, [], KeyListR),
  member(Key, KeyListR),
  parseTabs(Key, StringList),
  StringList = [_Factor1, _Factor2, NrS, CatS|_],
  str_int(NrS, OpgNr),
  str_int(CatS, Cat),
  opgaveU(OpgNr, Cat, _, MaxCats, Verhinderfactor, Cats1, Cats2, Naam1, Naam2, U, Gezien1, Gezien2),
  uitslTekst(U, Utekst),
  writef(FStr, Utekst,Gezien1,Naam1, Cats1, Gezien2, Naam2, Cats2, Verhinderfactor, MaxCats),
  fail.
maakUitlootRapport(CatTop, _, _) :-
  maxleng(MaxL0),
  str_len("Deelname", LD),
  grootste(MaxL0, LD, MaxL), 
  wc(CatTop, _, _, _, EDG, _, _, _, _, _, _, _, _, _,_),
  %wc(CatTop,_Naam,EDG,_,_,_,_),
  dubbelkolommen(EDG, MaxL, NaamKol, OpgKol, Onderdelen2Header),  
  format(FStr, "%%1s %%c %%-18.18s %%-%us %%c %%-%u.%us %%-%us %%3u  %%3.0f%%%%\n", MaxL, NaamKol, NaamKol, OpgKol),
  write("\nDeel 2: Volgorde deelname aan verschillende categoriën: \n"),
  format(TStr, "%%1s %%c %%-18.18s %%-%us %%c %%-%u.%us %%-%us  Ond Verh\n", MaxL, NaamKol, NaamKol, OpgKol),
  writef(TStr, " ", ' ', "", "Deelname", ' ', "", Onderdelen2Header),  
  findall(Keyx, opgaveSortKey(Keyx,2), KeyList),
  sortstring_c(KeyList, 1),
  reverse(KeyList, [], KeyListR),
  member(Key, KeyListR),
  parseTabs(Key, StringList),
  StringList = [_Factor1, _Factor2, NrS, CatS|_],
  str_int(NrS, OpgNr),
  str_int(CatS, Cat),
  opgaveU(OpgNr, Cat, _, MaxCats, Verhinderfactor, Cats1, Cats2, Naam1, Naam2, U, Gezien1, Gezien2),
  uitslTekst(U, Utekst),
  writef(FStr, Utekst, Gezien1, Naam1, Cats1, Gezien2, Naam2, Cats2, MaxCats, Verhinderfactor),
  fail.
maakUitlootRapport(_, _, _) :-
  closefile(work),
  retractall(opgaveU(_, _, _, _, _, _, _, _, _, _, _, _)),
  Parent = vpi_GetTaskWin(),
  dlg_rapport_Create(Parent, "Uitloothulplijsten","Uitloot",b_False).

  dlg_uitloten_Create(Parent, Cat):-
	assert(uitlootcat(Cat)),
	win_CreateDynDialog(Parent,
		[
%BEGIN Uitloten, WinDefList, 11:17:46-10.1.2006, Code automatically updated!
		 dlg(wdef(dlg_uitloten_DlgType,dlg_uitloten_RCT,dlg_uitloten_Title,u_DlgBase),dlg_uitloten_Flags),
		 ctl(wdef(wc_RadioButton,rct(5,128,162,138),"&de hele dag (volgens baanbeschikbaarheid)",u_DlgBase),idc_hele_dag,[wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_RadioButton,rct(5,138,69,149),"&overdag",u_DlgBase),idc_voor_1800_uur,[wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_RadioButton,rct(5,148,63,158),"&avond",u_DlgBase),idc_na_1800_uur,[wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_CheckBox,rct(73,138,158,148),"&Weekenden meetellen",u_DlgBase),idc_weekenden_meetellen,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_PushButton,rct(128,149,168,161),"&OK",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop,wsf_Disabled]),
		 ctl(wdef(wc_Text,rct(5,3,167,107),"Ditmenu",u_DlgBase),idct_ditmenu,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(6,109,166,127),"Doordeweeks",u_DlgBase),idct_doordeweeks,[wsf_AlignLeft])
%END Uitloten, WinDefList
		],dlg_uitloten_eh,0).

%BEGIN Uitloten, idc_ok _CtlInfo
  dlg_uitloten_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-!,
	uitlootCat(Cat),
	bovencat(Cat, Catb),
	findall(Cx, voorronde(Catb, Cx), CxL),
	CxList = [Catb|CxL],
	CBAvond = win_GetCtlHandle(_Win, idc_na_1800_uur),
	Avond = win_IsChecked(CBAvond),
	CBHele = win_GetCtlHandle(_Win, idc_hele_dag),
	Hele = win_IsChecked(CBHele),
	CBWeekend = win_GetCtlHandle(_Win, idc_weekenden_meetellen),
	WkEnd = win_IsChecked(CBWeekend),
	assert(metweekends(WkEnd)),
	uniekeOpgaven(CxList, Avond, Hele, WkEnd),
	%CBOverdag = win_GetCtlHandle(_Win, idc_voor_1800_uur),
	win_Destroy(_Win),
	wanneertekst(Avond, Hele, Wanneertekst),
	maakUitlootRapport(Catb, Wanneertekst, WkEnd),
	!.
%END Uitloten, idc_ok _CtlInfo
%MARK Uitloten, new events

%BEGIN Uitloten, idc_hele_dag _CtlInfo
  dlg_uitloten_eh(_Win,e_Control(idc_hele_dag,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	OKWin = win_GetCtlHandle(_Win, idc_ok),
	win_SetState(OKWin, [wsf_Enabled]),
	!.
%END Uitloten, idc_hele_dag _CtlInfo

%BEGIN Uitloten, idc_na_1800_uur _CtlInfo
  dlg_uitloten_eh(_Win,e_Control(idc_na_1800_uur,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	OKWin = win_GetCtlHandle(_Win, idc_ok),
	win_SetState(OKWin, [wsf_Enabled]),
	!.
%END Uitloten, idc_na_1800_uur _CtlInfo

%BEGIN Uitloten, idc_voor_1800_uur _CtlInfo
  dlg_uitloten_eh(_Win,e_Control(idc_voor_1800_uur,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	OKWin = win_GetCtlHandle(_Win, idc_ok),
	win_SetState(OKWin, [wsf_Enabled]),
	!.
%END Uitloten, idc_voor_1800_uur _CtlInfo

%BEGIN Uitloten, e_Create
  dlg_uitloten_eh(_Win,e_Create(_CreationData),0):-!,
	CBWeekend = win_GetCtlHandle(_Win, idc_weekenden_meetellen),
	metweekends(MW),
	win_Check(CBWeekend, MW),
	TWin = win_GetCtlHandle(_Win, idct_ditmenu),
	Tekst = "Uitloten aan de hand van aantal verhinderingen en/of aantal onderdelen.",
	concat(Tekst, "\nAan de hand van deze lijst (2 delen) kun je met de knop Details inschrijvingen uitsluiten om vervolgens eventueel opnieuw in te delen.", T1),
	concat(T1, "\nZorg dat er al een realistische baanbeschik- baarheid is.", T2),
	concat(T2, "\nDe verhinderingen worden uitgedrukt in % van de speelbare tijd. (0% is géén verhinderingen.)",T3),
	win_SetText(TWin, T3),
	Tekst1 = "Doordeweeks wordt dit onderdeel voornamelijk gespeeld:",
	T1Win = win_GetCtlHandle(_Win, idct_doordeweeks),
	OKWin = win_GetCtlHandle(_Win, idc_ok),
	win_SetState(OKWin, [wsf_Disabled]),
	win_SetText(T1Win, Tekst1),
	!.
%END Uitloten, e_Create

  dlg_uitloten_eh(_,_,_):-!,fail.

%END_DLG Uitloten


%BEGIN_DLG Emailsmsinstellingen
/**************************************************************************
	Creation and event handling for dialog: Emailsmsinstellingen
**************************************************************************/

constants

%BEGIN Emailsmsinstellingen, CreateParms, 10:43:19-7.3.2010, Code automatically updated!
  dlg_emailsmsinstellingen_DlgType = wd_Modal
  dlg_emailsmsinstellingen_Title = "Email -> SMS instellingen"
  dlg_emailsmsinstellingen_RCT = rct(50,40,323,187)
  dlg_emailsmsinstellingen_Flags = [wsf_Close,wsf_TitleBar]
  dlg_emailsmsinstellingen_Help = idh_informeren
  dlg_emailsmsinstellingen_Font = "Arial"
  dlg_emailsmsinstellingen_FSize = 9
%END Emailsmsinstellingen, CreateParms

predicates

  dlg_emailsmsinstellingen_eh : EHANDLER
  determ check_tegoed(string, string).

clauses

  emailsmsP(Prov, Aantal, mapinameaddr("", Email)) :-
    emailsms(Prov, Aantal, EMail, _), !.
  emailsmsP("mail-sms.com", 0, mapinameaddr("", "")).

  check_tegoed(TG, "Vul een tegoed in") :-
    not(str_int(TG, _)), !.
  check_tegoed(TG, "Vul een tegoed in") :-
    str_int(TG, Tegoed),
    Tegoed = 0, !.


  dlg_emailsmsinstellingen_Create(Parent):-
	win_CreateDynDialog(Parent,
		[
%BEGIN Emailsmsinstellingen, WinDefList, 10:43:19-7.3.2010, Code automatically updated!
		 dlg_font(wdef(dlg_emailsmsinstellingen_DlgType,dlg_emailsmsinstellingen_RCT,dlg_emailsmsinstellingen_Title,u_DlgBase),
		 	  dlg_emailsmsinstellingen_Font,dlg_emailsmsinstellingen_FSize,dlg_emailsmsinstellingen_Flags),
		 ctl(wdef(wc_PushButton,rct(181,31,231,44),"Website",u_DlgBase),idc_website,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_Edit,rct(95,75,118,87),"",u_DlgBase),idc_tegoed,[wsf_Group,wsf_TabStop,wsf_AutoHScroll,wsf_AlignLeft]),
		 ctl(wdef(wc_Edit,rct(95,89,229,102),"",u_DlgBase),idc_afzender_adres,[wsf_Group,wsf_TabStop,wsf_AutoHScroll,wsf_AlignLeft]),
		 ctl(wdef(wc_Edit,rct(82,108,141,120),"mail-sms.com",u_DlgBase),idc_mailsmsnl,[wsf_AlignLeft,wsf_Group,wsf_TabStop,wsf_AutoHScroll,wsf_ReadOnly]),
		 ctl(wdef(wc_PushButton,rct(224,131,264,143),"&OK",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(182,131,222,143),"&Cancel",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_Text,rct(38,14,195,24),"Voor email -> SMS heb je een 'tegoed' nodig.",u_DlgBase),idct_email_sms,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(38,27,182,49),"Je moet dat eerst kopen op bijvoorbeeld www.mail-sms.nl.  -->",u_DlgBase),idc_emailsmsinstellingen_4,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(40,76,93,86),"Actuele tegoed:",u_DlgBase),idc_emailsmsinstellingen_5,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(39,108,79,118),"Provider:",u_DlgBase),idc_emailsmsinstellingen_8,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(143,109,267,127),"*) voor een eventuele alternatieve provider houden wij ons aanbevolen.",u_DlgBase),idc_emailsmsinstellingen_10,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(40,91,91,101),"Afzender adres:",u_DlgBase),idct_afzender_adres,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(40,53,226,73),"Neem daarna STEEDS de volgende gegevens nauwkeurig over van je SMS-provider: ",u_DlgBase),idct_gegevens,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(8,22,32,32),"Stap 1",u_DlgBase),idct_stap,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(8,69,33,79),"Stap 2",u_DlgBase),idct_stap_2,[wsf_AlignLeft]),
		 ctl(wdef(wc_GroupBox,rct(38,48,232,104),"",u_DlgBase),idc_groupbox,[])
%END Emailsmsinstellingen, WinDefList
		],dlg_emailsmsinstellingen_eh,0).

%BEGIN Emailsmsinstellingen, idc_ok _CtlInfo
  dlg_emailsmsinstellingen_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	TWin = win_GetCtlHandle(_Win, idc_tegoed),
	TG = win_GetText(TWin),
	fronttoken(TG, TG1, _),
	check_tegoed(TG1, Msg),
	_Answer = dlg_MessageBox( "", Msg, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	!.
  dlg_emailsmsinstellingen_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	AWin = win_GetCtlHandle(_Win, idc_afzender_adres),
	Afz = win_GetText(AWin),
	emailadresfout(Afz),
	Msg = "Het afzender emailadres klopt niet!",
	_Answer = dlg_MessageBox( "", Msg, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	!.
  dlg_emailsmsinstellingen_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	TWin = win_GetCtlHandle(_Win, idc_tegoed),
	TG = win_GetText(TWin),
	fronttoken(TG, TG1, _),
	str_int(TG1, Tegoed),
	AWin = win_GetCtlHandle(_Win, idc_afzender_adres),
	Afz = win_GetText(AWin),
	emailsmsP(Prov, _, _),
	retractall(emailsms(_,_, _, _)),
	assert(emailsms(Prov, Tegoed, Afz, [])),
	win_Destroy(_Win),
	!.
%END Emailsmsinstellingen, idc_ok _CtlInfo
%MARK Emailsmsinstellingen, new events

%BEGIN Emailsmsinstellingen, idc_website _CtlInfo
  dlg_emailsmsinstellingen_eh(_Win,e_Control(idc_website,_CtrlType,_CtrlWin,_CtlInfo),0):-
	Null = cast(api_hwnd,0),
  	api_ShellExecute(Null,"open","http://www.mail-sms.nl/","","",1),
	!.
%END Emailsmsinstellingen, idc_website _CtlInfo

%BEGIN Emailsmsinstellingen, idc_cancel _CtlInfo
  dlg_emailsmsinstellingen_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-
	win_Destroy(_Win),
	!.
%END Emailsmsinstellingen, idc_cancel _CtlInfo

%BEGIN Emailsmsinstellingen, e_Create
  dlg_emailsmsinstellingen_eh(_Win,e_Create(_CreationData),0):-!,
	emailsmsP(Prov, Tegoed, mapinameaddr(_, Afzender)),
	str_int(TG, Tegoed),
	TWin = win_GetCtlHandle(_Win, idc_tegoed),
	win_SetText(TWin, TG),
	PWin = win_GetCtlHandle(_Win, idc_mailsmsnl),
	win_SetText(PWin, Prov),
	AWin = win_GetCtlHandle(_Win, idc_afzender_adres),
	win_SetText(AWin, Afzender),
	!.
%END Emailsmsinstellingen, e_Create

  dlg_emailsmsinstellingen_eh(_,_,_):-!,fail.

%END_DLG Emailsmsinstellingen

