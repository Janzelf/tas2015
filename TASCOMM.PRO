/*****************************************************************************

		Copyright (c) 1989 - 1999 De Lint Associates

 Project:  TASVP1
 FileName: TASCOMM.PRO
 Purpose: No description
 Written by: JdL
 Comments:
******************************************************************************/

include "tasvp1.inc"
include "tasvp1.con"
include "hlptopic.con"
%include "apicalls\\win\\apicalls.pro"	% standard from PDC
%include "apicalls\\win\\apicomm.pro"	% my own
include "datecust\\date_cc.pre"
%include "apicalls\\win\\SMAPI1\\smapi.pro"
include "Browser.pre"
/*
implement comPortObj

facts - portFacts
  determ comWin(WINDOW)
  single comPortHandle(OS_Handle)
  determ commConfigStruct(binary)
  single portDCB(dcb)

predicates
procedure  setupCommConfig(string PortName)
  extractAndSetDCB()
procedure  emptyIn
procedure  sound(OS_Handle, BOOLEAN)
procedure  openCom(string, OS_Handle)
procedure toonPuls(BOOLEAN, char)

clauses 

comPortHandle(0).
portDCB(dcb(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)).

toonPuls(b_True, 'T') :- !.
toonPuls(_, 'P').

sound(_, _) :- !.
sound(Handle, b_True) :-
  DStr = "ATM2\r",
  str_len(DStr, LenO1),
  emptyIn,
  api_WriteFileSync(Handle, DStr, LenO1, _LenWritten1), !.
sound(Handle, _) :-
  DStr = "ATM0\r",
  str_len(DStr, LenO1),
  emptyIn,
  api_WriteFileSync(Handle, DStr, LenO1, _LenWritten1), !.
  
openCom(PortName, Handle) :-
  DesiredAccess = api_generic_read + api_generic_write,
  ShareMode = 0,
  Handle = api_CreateFile(PortName, DesiredAccess, ShareMode, 0, api_open_existing, 0/*api_file_flag_overlapped*/, 0),
  assert(comPortHandle(Handle), portFacts), !.


new(Win) :-
  modem_enabled(_MM, ComP, _Vorwahl, _Speed, _ToonOfPuls, Geluid),
  concat("COM",ComP,PortName),
  %comPort(PortName),
  assert(comWin(Win),portFacts),
  openCom(PortName, Handle),
  api_ClearCommError(Handle, _Errorcode, _CommStat),
  setupCommConfig(PortName),
  ReadIntervalTimeout_ms      =     2,
  ReadTotalTimeoutMultiplier  =     1,
  ReadTotalTimeoutConstant    =   100,
  WriteTotalTimeoutMultiplier =     2,
  WriteTotalTimeoutConstant   =   100,
  CommTimeout = api_CommTimeouts(ReadIntervalTimeout_ms,ReadTotalTimeoutMultiplier,
	  ReadTotalTimeoutConstant,WriteTotalTimeoutMultiplier,WriteTotalTimeoutConstant),
  api_SetCommTimeouts(Handle,CommTimeout),
  EventMask = api_ev_rxchar,	% api_ev_txempty + api_ev_rxchar
  api_SetCommMask(Handle,EventMask), !,
  sound(Handle,Geluid).
  
setupCommConfig(PortName) :-
  retract(commConfigStruct(LpCC1),portFacts),
  StructSize = getbinarysize(LpCC1),
  SizeExpect = api_size(StructSize),
  b_True = api_GetDefaultCommConfig(PortName, LpCC1, SizeExpect),
  LpCC1 = LpCC2,
  assert(commConfigStruct(LpCC2)),
  extractAndSetDCB, !.
setupCommConfig(PortName) :-
  InitSize = 8,
  LpCC1 = makebinary(InitSize),
  SizeExpect = api_Size(InitSize),	% struct!
  b_False = api_GetDefaultCommConfig(PortName, LpCC1, SizeExpect),
  api_GetLastError(),
  SizeExpect = api_size(NewSize),
  LpCC2 = makebinary(NewSize),
  b_True = api_GetDefaultCommConfig(PortName, LpCC2, SizeExpect),
  assert(commConfigStruct(LpCC2),portFacts),
  extractAndSetDCB, !.
setupCommConfig(_).

extractAndSetDCB:-
  modem_enabled(_MM, _ComP, _Vorwahl, Speed, _ToonOfPuls, _Geluid),
  commConfigStruct(CommConfig_),
  DcbAdr0 = cast(dword,CommConfig_),
  CommConfig = cast(commConfig,CommConfig_),
  CommConfig = api_CommConfig(CC_P1,CC_P2,CC_P3,_P_DCB,_CC_P5,_CC_P6,_CC_P7,_CC_P8),
  DcbAdr = DcbAdr0 + sizeOf(CC_P1) + sizeOf(CC_P2) + sizeOf(CC_P3),
  DCB = cast(dcb,DcbAdr),
% setup DCB
  DCB = dcb(_P1,_P2,Flags,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15),
  FlagMask = api_rtsControlMask + api_dtrControlMask + api_outxFlowMask,
  bitnot(FlagMask,ReverseFlagMask),
  bitand(Flags,ReverseFlagMask,Flags1),
		% api_binary + 
  SetFlags = api_binary + api_outxCtsFlow + api_rtsControl_handshake + api_stripNull,

%	SetFlags = api_outxDsrFlow + api_dtrControl_handshake + api_stripNull,

%	SetFlags = api_outxDsrFlow + api_dtrControl_handshake + api_stripNull,
%		 + api_outxCtsFlow + api_rtsControl_handshake % muﬂ mit rein
%		 + api_dsrSensitivity,	% 

%	SetFlags = api_outxCtsFlow + api_rtsControl_toggle + api_stripNull,

%	SetFlags = api_stripNull,

  bitor(Flags1,SetFlags,FlagsNew),
%	ByteSize = 8,Parity = 0,StopBits = 0, ^= 8N1
%	P7		P8		P9
  DCBLength = sizeof(dcb),
  DCBNew = dcb(DCBLength,Speed,FlagsNew,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15),
%  write("\n", DCBNew),
  assert(portDCB(DCBNew),portFacts),
  comPortHandle(Handle),
  api_SetCommState(Handle,DCBNew).

emptyIn :-
  InbufLen = 100,
  str_len(Inbuf, InbufLen),
  comPortHandle(Handle),
  api_ReadFileSync(Handle, Inbuf, InbufLen, _ReceiveLen1), !.
emptyIn :-
  writeToJournaal("Emptyin failed!").
  
dial(DialString) :-
  comWin(Win),
  comPortHandle(Handle),
  modem_enabled(_MM, _ComP, Vorwahl, _Speed, ToonOfPuls, _Geluid),
  toonPuls(ToonOfPuls, TPChar),
  format(DStr, "ATD%c%s%s\r", TPChar, Vorwahl, DialString),
  str_len(DStr, LenO1),
  emptyIn,
  api_WriteFileSync(Handle, DStr, LenO1, _LenWritten1),
  dlg_dial_Create(Win, DialString),
  api_WriteFileSync(Handle, "+++", 3, _LenWritten2),
  emptyIn,
  sleep(110),		% 1,1 sec.
  Hangup = "ATH0\r",
  str_len(Hangup, HangLen),
  api_WriteFileSync(Handle, Hangup, HangLen, _LenWritten3),
  sleep(110),
  emptyIn, !.

setupCom :-
  comPort(PortName),
  comWin(Win),
  commConfigStruct(LpCC1),
  Hwnd = cast(api_Hwnd, Win),
  api_CommConfigDialog(PortName, Hwnd, LpCC1), !.
setupCom. 
  
delete :-
  emptyIn,
  comPortHandle(FileHandle),
  api_CloseHandle(FileHandle), !.
delete :-
  writeToJournaal("delete failed!").
  
endclass comPortObj
*/

domains
bw = direct ; bijwinst
%shselect = apart ; bijgevoegd ; apartmetfinale	% sheet selector types
kolomlr = links ; midden ; rechts

database - schemas
  schemaItem(integer Regel, integer Kol, integer Rowspan, string RepH1, string Color1, string ColorEnd, string Anker, string CSS)
single includelogo(boolean)
single eerstedagGepasseerd(boolean)

predicates

nondeterm ondS2cat(slist, wedscat)
procedure ap2str1(wedscat CatT, string In, string Uit, string Kin, string Kuit)
procedure ap2str2(wedscat CatT, string In, string Uit, string Kin, string Kuit)
procedure ap2str3(wedscat CatT, string In, string Uit, string Kin, string Kuit)
procedure flags(ilist, boolean Browser, boolean, boolean, boolean, boolean, boolean SheetSel, boolean Logo)
%procedure speelsterkteMoet(janee, window)
procedure voorPrint(beeld PrintOfWeb,  boolean ZonderWeblinks) - (i,o)
procedure voorselectie(window, catl)
procedure voorselectie1(window LboxWin, slist, integer N, catl VSel)

clauses

includelogo(b_False).
eersteDagGepasseerd(b_False).

logoErBijTop() :-
  includelogo(b_True),
  schemadoel(upload),
  write("\n <IMG ALIGN=\"right\" SRC=\"sheet.php?plek=xxx&type=franje&naam=logo\" >"),
  !.
logoErBijTop() :-
  includelogo(b_True),
  schemadoel(uploadftp),
  webprintlogo(LFN),  
  existfile(LFN),
  filenamepath(LFN, _, FName),
  %file_str(Logofile, Str),
  writef("\n <IMG ALIGN=\"right\" SRC=\"%s\" >",FName), !.
logoErBijTop() :-
  includelogo(b_True),
  webprintlogo(LFN),  
  existfile(LFN),
  concat("file:///",LFN, LFNB),
  %file_str(Logofile, Str),
  writef("\n <IMG ALIGN=\"right\" SRC=\"%s\" >",LFNB), !.
logoErBijTop().

ap2str(WC, afval, Uit3, Kort3) :-
  ap2str1(WC, "Afvalschema", Uit1, "A", Kort1),
  ap2str2(WC, Uit1, Uit2, Kort1, Kort2),
  ap2str3(WC, Uit2, Uit3, Kort2, Kort3), !.
ap2str(_, poel, "Poule", "P") :- !.
/*ap2str(WC, nietingedeeld, " Poule-Poule ", "P-P-P"):-
  isPoulePoule(WC, _), !. */
ap2str(WC, nietingedeeld, Uit1, Kort1) :-
  ap2str2(WC,"Niet inged.", Uit1,"N", Kort1).

ap2str1(CatT, In, Uit, Kin, Kuit) :-
  kopl(CatT, pl3en4(_), _Cat, _), !,
  format(Uit, "%s met 3/4 plaats", In),	% 3/4 plaats
  format(Kuit, "%s 34", Kin).
ap2str1(_, In, In, Kin, Kin).

ap2str2(Cat, In, Uit, Kin, Kuit) :-
  koplv(CatV, Cat),
  not(kopl(Cat, pl3en4(_), CatV, _)),
  not(kopl(CatV,verliezer,Cat,_)), !,
  format(Uit, "%s met voorronde(s)", In),
  format(Kuit, "%s V", Kin).
ap2str2(_, In, In, Kin, Kin).

ap2str3(Cat, In, Uit, Kin, Kuit) :-
  kopl(CatV, verliezer,Cat,_),
  wc(CatV, _, _, _, _, _, _, _, _, _, _, _, _, _, _), !,
  format(Uit, "%s met troostronde", In),
  format(Kuit, "%s T", Kin).
ap2str3(_, In, In, Kin, Kin).

linkSpelerprofiel(_Sterk, _, nee) :- !.
linkSpelerprofiel("open", _, _) :-		% open plaats
  write("&nbsp;"), !.
linkSpelerprofiel(_Sterk, "00000000", _) :-
  write("<B>00</B>"), !.
linkSpelerprofiel("open", _, _) :-
  write("&nbsp;"), !.
linkSpelerprofiel(_Sterk, Bond, _) :-
  not(proef11det(Bond)),
  write("&nbsp;"), !.
linkSpelerprofiel(Sterk, _Bond, _) :-
  not(fronttoken(Sterk, _, _)),
  schemadoel(afdruk),
  write("..."), !.
/*linkSpelerprofiel(Sterk, Bond, _) :-
  not(fronttoken(Sterk, _, _)),
  date(Year,_Month,_Day),
  Jaar = Year - 1,
  writef("<a href=\"http://www.knltb.nl/servit/wcspelersprofieltoon.asp?WCI=wiZoekNogmaals&amp;Bondsnummer=%s&amp;DatumVanaf=01-05-%u\" target=\"_blank\">...</a>",Bond, Jaar), !.
*/
linkSpelerprofiel(Sterk, _Bond, _) :-
  %schemadoel(afdruk), !,
  write(Sterk).
/*linkSpelerprofiel(Sterk, Bond, _) :-
  date(Year,_Month,_Day),
  Jaar = Year - 1,
  writef("<a href=\"http://www.knltb.nl/servit/wcspelersprofieltoon.asp?WCI=wiZoekNogmaals&amp;Bondsnummer=%s&amp;DatumVanaf=01-05-%u\" target=\"_blank\">%s</a>%s",Bond, Jaar, Sterk).
  */

ondS2cat(_IDW_ONDERDELEN_ITEMLIST, CNo) :-
  member(OnderdeelSel, _IDW_ONDERDELEN_ITEMLIST),
  searchchar(OnderdeelSel, '\t', Pos0),
  frontstr(Pos0,OnderdeelSel,_,Selectie),
  searchchar(Selectie, '\t', Pos),
  frontstr(Pos, Selectie, _, OnderdeelSelected),
  wc(CNo, _, _, OnderdeelSelected, _, _, _, _, _, _, _, _, _, _, _).
  %wc(CNo,OnderdeelSelected,_,_,_,_,_).

get_htmlmap(Map) :-
  schemadoel(afdruk),
  getFolder(Map,_,_,_), !.
get_htmlmap(Map) :-
  get_Webmap(Map), !.

flags([b_True, Browser, GeenVerl,Weblinks,MetSpeelsterkte,SheetSel,InclLogo], b_True, Browser, Geenverl,WebLinks,MetSpeelsterkte,SheetSel,InclLogo) :- !.
flags([b_False, Browser, Geenverl,WebLinks,MetSpeelsterkte,SheetSel,InclLogo], b_False, Browser, Geenverl,WebLinks,MetSpeelsterkte,SheetSel,InclLogo) :- !.
flags(_, b_False, b_False, b_False, b_False, b_False, b_True, b_False).

/*
speelsterkteMoet(ja, Win) :-
  dialog_SetCheck(Win, idcw_met_speelsterkte, b_True), 
  dialog_SetState(Win, [enable(idcw_met_speelsterkte, b_False)]), !.
speelsterkteMoet(_, _StWin).
*/
voorPrint(afdruk,  b_True) :- !.
voorPrint(_,  b_False).

voorselectie(LboxWin, []) :-
  selectAlle(LboxWin), !.
voorselectie(LboxWin, VSel) :-		% [zelf en boven]
  LboxList = lbox_GetAll(LboxWin),
  voorselectie1(LboxWin, LboxList, 0, VSel), !.
voorselectie(_LboxWin, _VSel).

voorselectie1(LboxWin, [H | Rest], N, VSel) :-
  parsetabs(H, List),
  List = [_,_,OnderdeelSelected|_],
  wc(CNo, _, _, OnderdeelSelected, _, _, _, _, _, _, _, _, _, _, _),
  %wc(CNo,OnderdeelSelected,_,_,_,_,_),
  member(CNo, VSel), !,
  lbox_SetSel(LboxWin, N, b_True),
  N1 = N + 1,
  voorselectie1(LboxWin, Rest, N1, VSel).
voorselectie1(LboxWin, [_|Rest], N, VSel) :-
  N1 = N + 1, !,
  voorselectie1(LboxWin, Rest, N1, VSel).
voorselectie1(_LboxWin, _, _, _).

%BEGIN_DLG Maak webpagina's
/**************************************************************************
	Creation and event handling for dialog: Maak webpagina's
**************************************************************************/

constants

%BEGIN Maak webpagina's, CreateParms, 13:16:49-28.1.2012, Code automatically updated!
  dlg_maak_webpaginas_ResID = idd_maak_webpaginas
  dlg_maak_webpaginas_DlgType = wd_Modal
  dlg_maak_webpaginas_Help = idh_webpaginas
%END Maak webpagina's, CreateParms
metPlan = 65
%afdrIni  = 66

predicates

  dlg_maak_webpaginas_eh : EHANDLER
  dlg_maak_webpaginas_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_maak_webpaginas_update(DIALOG_VAL_LIST)
procedure  titelW(window, beeld)
procedure   kolommenD(integer, boolean, kolomtypel, kolomtypel) 
procedure  fontgrootte2select(ilist VanProfile, slist Menu, integer Preselect)
procedure  index(integer, slist, integer, integer)
procedure kolommen2control(kolomtypel, integer, integer, integer, integer, integer, integer, integer, integer)
%procedure spagendaErBij(beeld, boolean, boolean)
nondeterm availableStylesheet(string SS)
procedure weblayoutSettings(beeld Upload, beeld Printformaat)
procedure ondpreselect(beeld, catl PrintPresel, catl Uploadpresel, catl Uit)
procedure setplanningTM(integer, integer)
procedure inilogo(string, string)
procedure updatepresel(catl)
procedure updateweblayout(beeld, string CSSnaam, ilist Flags, kolomtypeL, ilist FontGrootten, string Mededeling, catl Selected)
%procedure weblayout1(beeld, string CSSnaam, ilist Flags, kolomtypeL, ilist FontGrL, string Mededeling, catl Selected) - (i,o,o,o,o,o,o)
procedure alleenMededeling(slist)

clauses

titelW(Win, afdruk) :-		% de printsituatie
  dialog_setstate(Win, [ 
  	show(idctw_agenda_wordt_bijgevoegd, b_False),  %]),
	show(idctw_mededeling_op_website, b_False), show(idcw_mededeling, b_False),
  	% enable(idcw_sheet_slector, b_False), show(idcw_sheet_slector, b_False),
  	show(idcw_groupbox, b_False)
  	]),
  win_SetText(Win, "Schema's afdrukken"), !.
titelW(Win, upload) :-
  providerX(_, PN, "", _),
  format(ON, "Webpagina's publiceren (%s)", PN),
  win_SetText(Win, ON),
  dialog_setstate(Win, [ 
  	show(idcw_frekwentie, b_True),
  	show(idctw_regelmatig, b_True),
  	enable(idcw_frekw_inst, b_True), show(idcw_frekw_inst, b_True)
  	%enable(idcw_logoselect, b_False), show(idcw_logoselect, b_False)
  	]),
  fail.
titelW(Win, uploadftp) :-
  win_SetText(Win, "Webpagina's publiceren (FTP)"),
  dialog_setstate(Win, [ 
  	show(idcw_frekwentie, b_True),
  	show(idctw_regelmatig, b_True),
  	enable(idcw_frekw_inst, b_True), show(idcw_frekw_inst, b_True)
  	]),
  fail.
titelW(Win, _) :-
  dialog_setstate(Win, [ 
  	enable(idcw_zwartwit, b_False), show(idcw_zwartwit, b_False)
  	]).

weblayout1(Print, IDCW_TOERNOOICSS_VALUE, Flags, GewensteKolommen, FontGrootteList,Mededeling,Selected) :-
  weblayout(Print, IDCW_TOERNOOICSS_VALUE, Flags, GewensteKolommen, FontGrootteList,Mededeling,Selected), !.   % = "toernooi.css",
weblayout1(_, "toernooi.css", [], [bondsnummer], [], "", []) :-
  openbaar(ja), !.
weblayout1(_, "toernooi.css", [], [], [], "", []).

zetzwartwit(afdruk, b_True) :-
  retractall(geenKleurPrint()),
  assert(geenkleurPrint()), !.
zetzwartwit(_, _) :-
  retractall(geenKleurPrint()).
fontgrootte2select([X|_], Lijst, Index) :-
  index(X, Lijst, 0, Index), !.
fontgrootte2select(_, _, 3). 

index(X, [XS|_], N, N) :-
  str_int(XS, X), !.
index(X, [_|Rest], N, NOut) :-
  N1 = N + 1, !,
  index(X, Rest, N1, NOut).
index(_, _, N, N) :- !.

kolommen2control([bondsnummer|Rest], b_true, IDCW_CLUB_CHECKED, IDCW_GEB_DATUM_CHECKED, IDCW_RANGLIJSTGEG_CHECKED,
		IDCW_DISTRICT_CHECKED, IDCW_WILDCARD_CHECKED,IDCW_RANGLIJST2_CHECKED, IDCW_NATIONALITEIT_CHECKED) :- !,
  kolommen2control(Rest, _IDCW_BONDSNUMMER_CHECKED, IDCW_CLUB_CHECKED, IDCW_GEB_DATUM_CHECKED, IDCW_RANGLIJSTGEG_CHECKED,
		IDCW_DISTRICT_CHECKED, IDCW_WILDCARD_CHECKED,IDCW_RANGLIJST2_CHECKED, IDCW_NATIONALITEIT_CHECKED).

kolommen2control([gebdatum|Rest], IDCW_BONDSNUMMER_CHECKED, IDCW_CLUB_CHECKED, b_true, IDCW_RANGLIJSTGEG_CHECKED,
		IDCW_DISTRICT_CHECKED, IDCW_WILDCARD_CHECKED,IDCW_RANGLIJST2_CHECKED, IDCW_NATIONALITEIT_CHECKED) :- !,
  kolommen2control(Rest, IDCW_BONDSNUMMER_CHECKED, IDCW_CLUB_CHECKED, _IDCW_GEB_DATUM_CHECKED, IDCW_RANGLIJSTGEG_CHECKED,
		IDCW_DISTRICT_CHECKED, IDCW_WILDCARD_CHECKED,IDCW_RANGLIJST2_CHECKED, IDCW_NATIONALITEIT_CHECKED).

kolommen2control([club|Rest], IDCW_BONDSNUMMER_CHECKED, b_True, IDCW_GEB_DATUM_CHECKED, IDCW_RANGLIJSTGEG_CHECKED,
		IDCW_DISTRICT_CHECKED, IDCW_WILDCARD_CHECKED,IDCW_RANGLIJST2_CHECKED, IDCW_NATIONALITEIT_CHECKED) :- !,
  kolommen2control(Rest, IDCW_BONDSNUMMER_CHECKED, _IDCW_CLUB_CHECKED, IDCW_GEB_DATUM_CHECKED, IDCW_RANGLIJSTGEG_CHECKED,
		IDCW_DISTRICT_CHECKED, IDCW_WILDCARD_CHECKED,IDCW_RANGLIJST2_CHECKED, IDCW_NATIONALITEIT_CHECKED).

kolommen2control([districtk|Rest], IDCW_BONDSNUMMER_CHECKED, IDCW_CLUB_CHECKED, IDCW_GEB_DATUM_CHECKED, IDCW_RANGLIJSTGEG_CHECKED,
		b_true, IDCW_WILDCARD_CHECKED,IDCW_RANGLIJST2_CHECKED, IDCW_NATIONALITEIT_CHECKED) :- !,
  kolommen2control(Rest, IDCW_BONDSNUMMER_CHECKED, IDCW_CLUB_CHECKED, IDCW_GEB_DATUM_CHECKED, IDCW_RANGLIJSTGEG_CHECKED,
		_IDCW_DISTRICT_CHECKED, IDCW_WILDCARD_CHECKED,IDCW_RANGLIJST2_CHECKED, IDCW_NATIONALITEIT_CHECKED).

kolommen2control([rating|Rest], IDCW_BONDSNUMMER_CHECKED, IDCW_CLUB_CHECKED, IDCW_GEB_DATUM_CHECKED, b_true,
		IDCW_DISTRICT_CHECKED, IDCW_WILDCARD_CHECKED,IDCW_RANGLIJST2_CHECKED, IDCW_NATIONALITEIT_CHECKED) :- !,
  kolommen2control(Rest, IDCW_BONDSNUMMER_CHECKED, IDCW_CLUB_CHECKED, IDCW_GEB_DATUM_CHECKED, _IDCW_RANGLIJSTGEG_CHECKED,
		IDCW_DISTRICT_CHECKED, IDCW_WILDCARD_CHECKED,IDCW_RANGLIJST2_CHECKED, IDCW_NATIONALITEIT_CHECKED).

kolommen2control([wildcard|Rest], IDCW_BONDSNUMMER_CHECKED, IDCW_CLUB_CHECKED,IDCW_GEB_DATUM_CHECKED, IDCW_RANGLIJSTGEG_CHECKED,
		IDCW_DISTRICT_CHECKED, b_true,IDCW_RANGLIJST2_CHECKED, IDCW_NATIONALITEIT_CHECKED) :- !,
  kolommen2control(Rest, IDCW_BONDSNUMMER_CHECKED, IDCW_CLUB_CHECKED, IDCW_GEB_DATUM_CHECKED, IDCW_RANGLIJSTGEG_CHECKED,
		IDCW_DISTRICT_CHECKED, _IDCW_WILDCARD_CHECKED,IDCW_RANGLIJST2_CHECKED, IDCW_NATIONALITEIT_CHECKED).

kolommen2control([ranglijst|Rest], IDCW_BONDSNUMMER_CHECKED, IDCW_CLUB_CHECKED,IDCW_GEB_DATUM_CHECKED, IDCW_RANGLIJSTGEG_CHECKED,
		IDCW_DISTRICT_CHECKED,IDCW_WILDCARD_CHECKED, b_true, IDCW_NATIONALITEIT_CHECKED) :- !,
  kolommen2control(Rest, IDCW_BONDSNUMMER_CHECKED, IDCW_CLUB_CHECKED, IDCW_GEB_DATUM_CHECKED, IDCW_RANGLIJSTGEG_CHECKED,
		IDCW_DISTRICT_CHECKED, IDCW_WILDCARD_CHECKED,_IDCW_RANGLIJST2_CHECKED, IDCW_NATIONALITEIT_CHECKED).

kolommen2control([landcode|Rest], IDCW_BONDSNUMMER_CHECKED, IDCW_CLUB_CHECKED,IDCW_GEB_DATUM_CHECKED, IDCW_RANGLIJSTGEG_CHECKED,
		IDCW_DISTRICT_CHECKED,IDCW_WILDCARD_CHECKED, IDCW_RANGLIJST2_CHECKED, b_true) :- !,
  kolommen2control(Rest, IDCW_BONDSNUMMER_CHECKED, IDCW_CLUB_CHECKED, IDCW_GEB_DATUM_CHECKED, IDCW_RANGLIJSTGEG_CHECKED,
		IDCW_DISTRICT_CHECKED, IDCW_WILDCARD_CHECKED, IDCW_RANGLIJST2_CHECKED, _IDCW_NATIONALITEIT_CHECKED).

kolommen2control([_|Rest], IDCW_BONDSNUMMER_CHECKED, IDCW_CLUB_CHECKED, IDCW_GEB_DATUM_CHECKED, IDCW_RANGLIJSTGEG_CHECKED,
		IDCW_DISTRICT_CHECKED,IDCW_WILDCARD_CHECKED,IDCW_RANGLIJST2_CHECKED, IDCW_NATIONALITEIT_CHECKED) :- !,
  kolommen2control(Rest, IDCW_BONDSNUMMER_CHECKED, IDCW_CLUB_CHECKED, IDCW_GEB_DATUM_CHECKED, IDCW_RANGLIJSTGEG_CHECKED,
		IDCW_DISTRICT_CHECKED, IDCW_WILDCARD_CHECKED,IDCW_RANGLIJST2_CHECKED, IDCW_NATIONALITEIT_CHECKED).

kolommen2control(_, b_false, b_false, b_false, b_false, b_false, b_false, b_false, b_false).

availableStylesheet("toernooi.css").
availableStylesheet(FNam) :-
  get_htmlmap(Dir),
  filenamepath(Wild, Dir, "*.css"),
  trap(findall(FN, dirfiles(Wild,0x40,FN,_,_,_,_,_,_,_,_),FNList),_,true),
  member(FNamU, FNList),
  upper_lower(FNamU, FNam),
  not(FNam = "toernooi.css").

ondpreselect(afdruk, [Preselect], _OndPreselect, [Preselect]) :- !.
%ondpreselect(afdruk, _Preselect, _OndPreselect, Preselect) :- !.
ondpreselect(_, _Preselect, OndPreselect, OndPreselect).

setPlanningTM(1, 0) :-
  findall(D, dag(D, _, _), DL),
  reverse(DL, [], DLR),
  DLR = [LastDag | _],
  assert(plantm(LastDag)), !.
setPlanningTM(_, _).	% laat hem zoals ie is 

updatepresel(Catl) :-
  schemapresel(CatL), !.
updatepresel(CatL) :-
  logf('Z', schemapresel(CatL), nee).

alleenMededeling([]) :-
  _Answer = dlg_MessageBox( "Pagina upload", "Nog geen onderdelen ingedeeld.\nU kunt wel een voorpagina met een mededeling publiceren.", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  !.  
alleenMededeling(_).

  dlg_maak_webpaginas_Create(_Parent,_Doel,_X, _):-
	uitslagendownload(b_False), 	% 25.10.2011
	fail.
  dlg_maak_webpaginas_Create(_Parent,Doel,X, b_True):-	% noshow
	weblayoutsettings(Doel, PrintW),
  	weblayout1(PrintW, _IDCW_TOERNOOICSS_DEFAULT, Settings1 ,_Kolommen, _Settings2, _IDCW_MEDEDELING_VALUE, _PreselectBew),
	Settings1 = [], !,
	%dlg_MessageBox( "Afdrukken", "Druk eerst een pagina af via\nOnderdeel, Print, via InternetBrowser", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
        dlg_maak_webpaginas_Create(_Parent,Doel,X, b_False).	% show
  dlg_maak_webpaginas_Create(_Parent,Doel,[], b_True):-
	weblayoutsettings(Doel, PrintW),
	prplan(PlanSel),
	plantm(DagNumP),
	setPlanningTM(PlanSel, DagNumP),
	weblayout1(PrintW, _, _Flags, _Kolommen, _FontGroottes, _IDCW_MEDEDELING_VALUE, OndPreselect),
	outHTML(Doel, OndPreselect, b_True), !.
  dlg_maak_webpaginas_Create(_Parent,Doel,Preselect, b_True):-
	prplan(PlanSel),
	plantm(DagNumP),
	setPlanningTM(PlanSel, DagNumP),
	outHTML(Doel, Preselect, b_True), !.
  dlg_maak_webpaginas_Create(Parent,Doel,Preselect, b_False):-
	retractall(geenkleurPrint()),
	resetspelerprogramma(),
	findall(Onderdeel, wedscat_ingedeeld(Onderdeel, b_True, _, b_False), OnderdelenIn),
	alleenMededeling(OnderdelenIn),
	assert(schemadoel(Doel)),
	IDW_ONDERDELEN_ITEMLIST = OnderdelenIn,
	findall(Dag, dag(_, Dag, _), Dagen),
	prplan(PlanSel),
	getplansel(PlanSel, PLANNING_TM),
	plantm(DagNumP),
	findIndex(Dagen, DagNumP, 0, IDC_PLAN_TM_SELECT),
	IDC_PLAN_TM_ITEMLIST = Dagen,
	weblayoutsettings(Doel, PrintW),
	weblayout1(PrintW, IDCW_TOERNOOICSS_DEFAULT, Flags, Kolommen, FontGroottes, IDCW_MEDEDELING_VALUE, OndPreselect),   % = "toernooi.css",
        ondpreselect(Doel, Preselect, OndPreselect,OndPreselectUit),
        updatepresel(OndPreselectUit),
	flags(Flags, IDCW_RANGLIJST_CHECKED, IDCW_ZWARTWIT_CHECKED,IDCW_ZONDER_VERLIEZERRONDES_CHECKED, _IDCW_AANEENGEREGEN_CHECKED,
	   IDCW_MET_SPEELSTERKTE_CHECKED,_IDCW_SHEET_SLECTOR_CHECKED,_IDCW_INCL_LOGOBESTANDEN_CHECKED),
	!,
	kolommen2control(Kolommen, IDCW_BONDSNUMMER_CHECKED, IDCW_CLUB_CHECKED, IDCW_GEB_DATUM_CHECKED, IDCW_RANGLIJSTGEG_CHECKED,
		IDCW_DISTRICT_CHECKED, IDCW_WILDCARD_CHECKED, IDCW_RANGLIJST2_CHECKED, IDCW_NATIONALITEIT_CHECKED),
	IDCW_FONTGR_ITEMLIST = ["6","7","8","9","10","11","12","13","14"],
	fontgrootte2select(FontGroottes, IDCW_FONTGR_ITEMLIST, IDCW_FONTGR_SELECT),
	findall(SS, availableStylesheet(SS), IDCW_TOERNOOICSS_ITEMLIST),
%MARK Maak webpagina's, new variables

	dialog_CreateModal(Parent,dlg_maak_webpaginas_ResID,"",
  		[
%BEGIN Maak webpagina's, ControlList, 13:16:49-28.1.2012, Code automatically updated!
		df(idw_onderdelen,listbox(IDW_ONDERDELEN_ITEMLIST,[]),nopr),
		df(idc_plan_tm,listbutton(IDC_PLAN_TM_ITEMLIST,IDC_PLAN_TM_SELECT),nopr),
		df(idcw_met_speelsterkte,checkbox(IDCW_MET_SPEELSTERKTE_CHECKED),nopr),
		df(idcw_ranglijst,checkbox(IDCW_RANGLIJST_CHECKED),nopr),
		df(idcw_bondsnummer,checkbox(IDCW_BONDSNUMMER_CHECKED),nopr),
		df(idcw_geb_datum,checkbox(IDCW_GEB_DATUM_CHECKED),nopr),
		df(idcw_club,checkbox(IDCW_CLUB_CHECKED),nopr),
		df(idcw_district,checkbox(IDCW_DISTRICT_CHECKED),nopr),
		df(idcw_ranglijst1,checkbox(IDCW_RANGLIJSTGEG_CHECKED),nopr),
		df(idcw_wildcard,checkbox(IDCW_WILDCARD_CHECKED),nopr),
		df(idcw_ranglijst2,checkbox(IDCW_RANGLIJST2_CHECKED),nopr),
		df(idcw_nationaliteit,checkbox(IDCW_NATIONALITEIT_CHECKED),nopr),
		df(idcw_zwartwit,checkbox(IDCW_ZWARTWIT_CHECKED),nopr),
		df(idcw_incl_logobestanden,checkbox(b_false),nopr),
		df(idcw_fontgr,listbutton(IDCW_FONTGR_ITEMLIST,IDCW_FONTGR_SELECT),nopr),
		df(idcw_toernooicss,listedit(IDCW_TOERNOOICSS_ITEMLIST,IDCW_TOERNOOICSS_DEFAULT),nopr),
		df(idcw_zonder_verliezerrondes,checkbox(IDCW_ZONDER_VERLIEZERRONDES_CHECKED),nopr),
		df(idcw_mededeling,editstr(IDCW_MEDEDELING_VALUE,[]),nopr),
		df(PLANNING_TM,radiobuttongroup([idc_geen_planning,idc_tot_en_met]),nopr)
%END Maak webpagina's, ControlList
		],
		dlg_maak_webpaginas_eh,0,VALLIST,ANSWER),
	dlg_maak_webpaginas_handle_answer(ANSWER,VALLIST).

  dlg_maak_webpaginas_handle_answer(idc_ok,VALLIST):-!,
	dlg_maak_webpaginas_update(VALLIST).
  dlg_maak_webpaginas_handle_answer(idc_cancel,_):-!.  % Handle Esc and Cancel here
  dlg_maak_webpaginas_handle_answer(_,_):-
	errorexit().

kolommenD(idcw_wildcard, b_True, In, [wildcard|In]) :- !. 
kolommenD(idcw_nationaliteit, b_True, In, [landcode|In]) :- !. 
kolommenD(idcw_geb_datum, b_True, In, [gebdatum|In]) :- !. 
kolommenD(idcw_ranglijst1, b_True, In, [rating|In]) :- !. 
kolommenD(idcw_bondsnummer, b_True, In, [bondsnummer|In]) :- !. 
kolommenD(idcw_district, b_True, In, [districtk|In]) :- !. 
kolommenD(idcw_club, b_True, In, [club|In]) :- !. 
kolommenD(idcw_ranglijst2, b_True, In, [ranglijst|In]) :- !. 
kolommenD(_, _, In, In).

/*spagendaErBij(web, b_True,b_True) :- !.
spagendaErBij(_, _,b_False).*/

weblayoutSettings(upload, web) :- !.
weblayoutSettings(uploadftp, web) :- !.
weblayoutsettings(X, X).


toernooiWeekJaar(No, WeekNr, Jaar ) :-  	% 21.2.2012 weeknr uitfaseren
  dag(No, _, _), !,
  dt_week_year_to_offset(WeekNr,Jaar, No).
  
inilogo(Logo, Pad) :-
  webprintlogo(LogoP),
  trap(existfile(LogoP), _, true),
  filenamePath(LogoP, Pad, Logo), !.
inilogo("*.*", "").

updateweblayout(PrintW, _IDCW_TOERNOOICSS_DEFAULT, Checked ,Kolommen, FontGr, _IDCW_MEDEDELING_VALUE, PreselectBew) :-
  weblayout(PrintW, _IDCW_TOERNOOICSS_DEFAULT, Checked ,Kolommen, FontGr, _IDCW_MEDEDELING_VALUE, PreselectBew), !.
updateweblayout(PrintW, _IDCW_TOERNOOICSS_DEFAULT, Checked,Kolommen, FontGr, _IDCW_MEDEDELING_VALUE, PreselectBew) :-
	logf('A',weblayout(PrintW, _IDCW_TOERNOOICSS_DEFAULT, Checked,Kolommen, FontGr, _IDCW_MEDEDELING_VALUE, PreselectBew),nee).

  dlg_maak_webpaginas_update(_VALLIST):-
%BEGIN Maak webpagina's, Update controls, 13:16:49-28.1.2012, Code automatically updated!
	dialog_VLGetListBox(idw_onderdelen,_VALLIST,_IDW_ONDERDELEN_ITEMLIST,_IDW_ONDERDELEN_SELECT),
	dialog_VLGetListButton(idc_plan_tm,_VALLIST,_IDC_PLAN_TM_ITEMLIST,_IDC_PLAN_TM_SELECT),
	_IDCW_RANGLIJST_CHECKED = dialog_VLGetCheck(idcw_ranglijst,_VALLIST),
	_IDCW_MET_SPEELSTERKTE_CHECKED = dialog_VLGetCheck(idcw_met_speelsterkte,_VALLIST),
	_IDCW_ZWARTWIT_CHECKED = dialog_VLGetCheck(idcw_zwartwit,_VALLIST),
	_IDCW_BONDSNUMMER_CHECKED = dialog_VLGetCheck(idcw_bondsnummer,_VALLIST),
	_IDCW_CLUB_CHECKED = dialog_VLGetCheck(idcw_club,_VALLIST),
	_IDCW_GEB_DATUM_CHECKED = dialog_VLGetCheck(idcw_geb_datum,_VALLIST),
	_IDCW_RANGLIJSTGEG_CHECKED = dialog_VLGetCheck(idcw_ranglijst1,_VALLIST),
	_IDCW_DISTRICT_CHECKED = dialog_VLGetCheck(idcw_district,_VALLIST),
	_IDCW_WILDCARD_CHECKED = dialog_VLGetCheck(idcw_wildcard,_VALLIST),
	dialog_VLGetListButton(idcw_fontgr,_VALLIST,_IDCW_FONTGR_ITEMLIST,_IDCW_FONTGR_SELECT),
	_IDCW_TOERNOOICSS_DEFAULT = dialog_VLGetListEdit(idcw_toernooicss,_VALLIST),
	_IDCW_RANGLIJST2_CHECKED = dialog_VLGetCheck(idcw_ranglijst2,_VALLIST),
	_IDCW_MEDEDELING_VALUE = dialog_VLGetstr(idcw_mededeling,_VALLIST),
	_IDCW_ZONDER_VERLIEZERRONDES_CHECKED = dialog_VLGetCheck(idcw_zonder_verliezerrondes,_VALLIST),
	_IDCW_NATIONALITEIT_CHECKED = dialog_VLGetCheck(idcw_nationaliteit,_VALLIST),
	_IDCW_INCL_LOGOBESTANDEN_CHECKED = dialog_VLGetCheck(idcw_incl_logobestanden,_VALLIST),
	_PLANNING_TM = dialog_VLGetRadiobutton(idc_geen_planning,_VALLIST),
%END Maak webpagina's, Update controls
	%apartBijgevoegd(_IDCW_SHEET_SLECTOR_CHECKED, SheetSelector),
	%SheetSelector = bijgevoegd,
	kolommenD(idcw_wildcard, _IDCW_WILDCARD_CHECKED, [], K3), 
	kolommenD(idcw_ranglijst1, _IDCW_RANGLIJSTGEG_CHECKED, K3, K4), 
	kolommenD(idcw_district, _IDCW_DISTRICT_CHECKED, K4, K5),
	kolommenD(idcw_club, _IDCW_CLUB_CHECKED, K5, K6), 
	kolommenD(idcw_bondsnummer, _IDCW_BONDSNUMMER_CHECKED, K6, K7), 
	kolommenD(idcw_geb_datum, _IDCW_GEB_DATUM_CHECKED, K7, K8), 
	kolommenD(idcw_ranglijst2, _IDCW_RANGLIJST2_CHECKED, K8, K9), 
	kolommenD(idcw_nationaliteit, _IDCW_NATIONALITEIT_CHECKED, K9, Kolommen), 
	getplansel(PlanSel,_PLANNING_TM),
	assert(prplan(PlanSel)),
	finddag(DagNumP,_IDC_PLAN_TM_ITEMLIST), 
	assert(plantm(DagNumP)),
	assert(includelogo(_IDCW_INCL_LOGOBESTANDEN_CHECKED)),
	schemaDoel(Print),
	%voorPrint(Print,  ZonderWeblinks),
	weblayoutSettings(Print, PrintW),
        %MetSpeelsterkte = _IDCW_MET_SPEELSTERKTE_CHECKED,
        %retractall(weblayout(PrintW, _, _, _, _, _, _)),
	zetzwartwit(Print, _IDCW_ZWARTWIT_CHECKED),
	str_int(_IDCW_FONTGR_ITEMLIST, FontGr),
	findall(Cat, ondS2cat(_IDW_ONDERDELEN_ITEMLIST, Cat), PreselectBew),
	%preselectbewaren(Print, _IDW_ONDERDELEN_ITEMLIST, PreselectBew),
	updateweblayout(PrintW, _IDCW_TOERNOOICSS_DEFAULT, [_IDCW_RANGLIJST_CHECKED, _IDCW_ZWARTWIT_CHECKED,
	 _IDCW_ZONDER_VERLIEZERRONDES_CHECKED, b_True/*_IDCW_AANEENGEREGEN_CHECKED*/,
	 _IDCW_MET_SPEELSTERKTE_CHECKED,b_True,_IDCW_INCL_LOGOBESTANDEN_CHECKED],Kolommen, [FontGr], _IDCW_MEDEDELING_VALUE, PreselectBew),
	%not(_IDW_ONDERDELEN_ITEMLIST = []),
	outHTML(Print, PreselectBew, b_False),
	fail.
  dlg_maak_webpaginas_update(_).

%MARK Maak webpagina's, new events

%BEGIN Maak webpagina's, idcw_logoselect _CtlInfo
  dlg_maak_webpaginas_eh(_Win,e_Control(idcw_logoselect,_CtrlType,_CtrlWin,_CtlInfo),0):-
	schemadoel(upload),
	_Answer = dlg_MessageBox( "Logo", "Installatie van het logo voor het Web\ngaat via Internet, beheer van Toernooiklapper", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	!.
  dlg_maak_webpaginas_eh(_Win,e_Control(idcw_logoselect,_CtrlType,_CtrlWin,_CtlInfo),0):-
	schemadoel(afdruk),
	inilogo(Logo, Pad),
	FN = dlg_GetFileName(Logo,["alle logotypes","*.jpg;*.bmp;*.jpeg;*.png"],"Selecteer logobestand",[],Pad,_OutListFiles),
	%logofilesize(FN),
	retractall(webprintlogo(_)),
	assert(webprintlogo(FN)),
	!.
%END Maak webpagina's, idcw_logoselect _CtlInfo

%BEGIN Maak webpagina's, idc_ok _CtlInfo
  dlg_maak_webpaginas_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	schemadoel(upload),
	LboxWin = win_GetCtlHandle(_Win, idw_onderdelen),
	lbox_GetSel(LboxWin,_ItemList,IndexList),
	IndexList = [],
	Mededeling = dialog_Getstr(_Win, idcw_mededeling),
	fronttoken(Mededeling, _, _),
	Answer = dlg_MessageBox( "Webpagina's", "Geen onderdelen geselecteerd, alleen een voorpagina met de mededeling wordt gepubliceerd!\nDoorgaan?", mesbox_iconquestion, mesbox_buttonsokcancel, mesbox_defaultfirst, mesbox_suspendapplication ),
	Answer <> 1, !.
  dlg_maak_webpaginas_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	schemadoel(upload),
	LboxWin = win_GetCtlHandle(_Win, idw_onderdelen),
	lbox_GetSel(LboxWin,_ItemList,IndexList),
	IndexList = [],
	Mededeling = dialog_Getstr(_Win, idcw_mededeling),
	not(fronttoken(Mededeling, _, _)),
	_Answer = dlg_MessageBox( "Webpagina's", "Geen onderdelen geselecteerd en ook geen mededeling.\nGebruik indien nodig Internet, beheer van Toernooiklapper\nom alle schema's van Internet te verwijderen.", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	!.
%END Maak webpagina's, idc_ok _CtlInfo


%BEGIN Maak webpagina's, idcw_frekw_inst _CtlInfo
  dlg_maak_webpaginas_eh(_Win,e_Control(idcw_frekw_inst,_CtrlType,_CtrlWin,_CtlInfo),0):-
	dlg_webupdateregelmaat_Create(_Win, b_True),
	autowebP(Regelmaat,HTMLFtp,_,_, _, _, _, _, Hoevaak, _BijSluiten),
	autowebOptie2int(Regelmaat, HTMLFtp, Hoevaak, Tekst),
	TWin = win_GetCtlHandle(_Win, idctw_regelmatig),
	win_SetText(TWin, Tekst),
	waarnaar(HTMLFtp, Waarnaar),
	assert(schemaDoel(Waarnaar)),
	!.
%END Maak webpagina's, idcw_frekw_inst _CtlInfo

%BEGIN Maak webpagina's, idcw_ranglijst _CtlInfo
  dlg_maak_webpaginas_eh(_Win,e_Control(idcw_ranglijst,_CtrlType,RWin,_CtlInfo),0):-
	%schemaDoel(afdruk),
	Ranglijst = win_IsChecked(RWin),
	bitxor(Ranglijst, 1, Geenranglijst),
	dialog_setstate(_Win, [
  	  enable(idcw_bondsnummer, Geenranglijst),show(idcw_bondsnummer, Geenranglijst),
  	  enable(idcw_geb_datum, Geenranglijst),show(idcw_geb_datum, Geenranglijst),
  	  enable(idcw_club, Geenranglijst),show(idcw_club, Geenranglijst),
  	  enable(idcw_district, Geenranglijst),show(idcw_district, Geenranglijst),
  	  enable(idcw_ranglijst1, Geenranglijst),show(idcw_ranglijst1, Geenranglijst),
  	  enable(idcw_wildcard, Geenranglijst),show(idcw_wildcard, Geenranglijst),
	  enable(idcw_ranglijst2, Geenranglijst),show(idcw_ranglijst2, Geenranglijst)
  	]),
  	Ranglijst = b_True,
	dialog_SetCheck(_Win, idcw_bondsnummer, b_True),
	dialog_SetCheck(_Win, idcw_geb_datum, b_True),
	dialog_SetCheck(_Win, idcw_club, b_False),
	dialog_SetCheck(_Win, idcw_district, b_True),
	dialog_SetCheck(_Win, idcw_ranglijst1, b_True),
	dialog_SetCheck(_Win, idcw_wildcard, b_True),
	dialog_SetCheck(_Win, idcw_ranglijst2, b_True),
	!.
%END Maak webpagina's, idcw_ranglijst _CtlInfo


%BEGIN Maak webpagina's, e_User
  dlg_maak_webpaginas_eh(_Win,e_User(metPlan,_Ptr),0):-
	CWin = win_GetCtlHandle(_Win, idc_tot_en_met),
	Plan = win_IsChecked(CWin),
	dialog_SetState(_Win, [enable(idc_plan_tm, Plan)]),
	%win_PostEvent(_Win,e_User(afdrIni,0)),
	!.
/*  dlg_maak_webpaginas_eh(_Win,e_User(afdrIni,_Ptr),0):-
	meteenupload(),
	trap(CW = win_GetCtlHandle(_Win, idc_ok),_,fail),
	win_SendEvent(_Win,e_Control(idc_ok,wc_PushButton,CW, activated())),
	!.*/
%END Maak webpagina's, e_User

%BEGIN Maak webpagina's, idc_tot_en_met _CtlInfo
  dlg_maak_webpaginas_eh(_Win,e_Control(idc_tot_en_met,_CtrlType,_CtrlWin,_CtlInfo),0):-
	win_PostEvent(_Win,e_User(metPlan,0)),
	!.
%END Maak webpagina's, idc_tot_en_met _CtlInfo

%BEGIN Maak webpagina's, idc_geen_planning _CtlInfo
  dlg_maak_webpaginas_eh(_Win,e_Control(idc_geen_planning,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	win_PostEvent(_Win,e_User(metPlan,0)),
	!.
%END Maak webpagina's, idc_geen_planning _CtlInfo

  dlg_maak_webpaginas_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-
	schemaDoel(afdruk),
	project_ShowHelpContext("Afdrukkenvanschema's"), !.
%BEGIN Maak webpagina's, idc_help _CtlInfo
  dlg_maak_webpaginas_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Webpagina's"),
	!.
%END Maak webpagina's, idc_help _CtlInfo

%BEGIN Maak webpagina's, idcw_geen _CtlInfo
  dlg_maak_webpaginas_eh(_Win,e_Control(idcw_geen,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idw_onderdelen),
	selectGeen(LboxWin),
	!.
%END Maak webpagina's, idcw_geen _CtlInfo

%BEGIN Maak webpagina's, idcw_alle _CtlInfo
  dlg_maak_webpaginas_eh(_Win,e_Control(idcw_alle,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idw_onderdelen),
	selectAlle(LboxWin),
	!.
%END Maak webpagina's, idcw_alle _CtlInfo

%BEGIN Maak webpagina's, e_Create
  dlg_maak_webpaginas_eh(_Win,e_Create(_CreationData),0):-!,
	%enableSpelerXML(_Win),
	schemaDoel(Print),
	schemaPresel(Preselect),
	%write("\n", Preselect),
	weblayoutsettings(Print, PrintW),
	weblayout1(PrintW, _IDCW_TOERNOOICSS_VALUE, Flags, _GewensteKolommen, _FontGroottes, _Mededeling, _),   % = "toernooi.css",
	flags(Flags, RanglijstK, _IDCW_ZWARTWIT_CHECKED, _GeenVerlR, _ZonderLinks,_MetSpeelsterkte,_SheetSel, _Logo),
	prplan(PlanSel),
	%spagendaErBij(PrintW, PlanSel,SpWijs),
	dialog_SetState(_Win, [show(idctw_agenda_wordt_bijgevoegd, PlanSel)]),
	LboxWin = win_GetCtlHandle(_Win, idw_onderdelen),
	lbox_setTabstops(LboxWin, [8,20,30]),
	voorselectie(LboxWin, Preselect),
	titelW(_Win, Print),
	%openbaar(Open),
	%speelsterkteMoet(Open, _Win), 
	RWin = win_GetCtlHandle(_Win, idctw_regelmatig),
	autowebP(Regelmaat,HTMLFtp,_, _, _, _, _, _, Hoevaak, _),
	autowebOptie2int(Regelmaat, HTMLFtp, Hoevaak, Tekst),
	win_SetText(RWin, Tekst),
	bitxor(RanglijstK, 1, Geenranglijst),
	dialog_setstate(_Win, [
  	  enable(idcw_bondsnummer, Geenranglijst),
  	  enable(idcw_geb_datum, Geenranglijst),
  	  enable(idcw_club, Geenranglijst),
  	  enable(idcw_district, Geenranglijst),
  	  enable(idcw_ranglijst1, Geenranglijst),
  	  enable(idcw_wildcard, Geenranglijst),
	  enable(idcw_ranglijst2, Geenranglijst)
          ]),
	win_PostEvent(_Win,e_User(metPlan,0)),
	!.
%END Maak webpagina's, e_Create

  dlg_maak_webpaginas_eh(_,_,_):-!,fail.

%END_DLG Maak webpagina's


% #################### einde opgaveinvoer ############# nu begint export ###########
database - l1
  opgSp(wedscat Cat, integer Tijd, integer Sp, string Opm)
determ okdir()



predicates 

procedure expPartner(tgnst, wedscat Cat, integer Tijd, integer Sp)
procedure exToernooiOnderdelen(wedscat Cat, integer Tijd, integer Sp)
procedure lastBlank(string, integer, integer)
procedure xml_adres(string, string, string)
procedure xml_woonplaats(string, string, string)
%procedure xml_verhinderingen(verhl, string)
procedure int_str_sp(integer, string)
procedure terugloop(string, string, string, string, string, string, string, string, string, string) - (i,i,i,i,i,i,i,i,i,i)
procedure xml_uitkantoor(string, tijd)
procedure adjustTijd(integer, integer)
%procedure jaNee2Str(janee, string, string)
procedure geboortedatum(string, string, string, string)

clauses



%jaNee2str(ja, "Ja", "J") :- !.
%jaNee2str(_, "Nee", "N").

adjustTijd(In, Uit) :-
  In < 10000,
  Uit = In + 10000, !.
adjustTijd(In, In). 

xml_inschrijfExp(Win, List) :-
  cursor_Set( Win, cursor_Wait),
  closefile(work),
  get_TempDir(_, TempFile),
  openwrite(work,TempFile),
  member(Str, List),
  parsetabs(Str, Vals),
  Vals = [_, TijdS, R2 | _],
  %fronttoken(Str, _, Str1),	% skip de markering
  %fronttoken(Str1,TijdS,R1),
  str_int(TijdS, Tijd),
  %fronttoken(R1,R2,_R3),
  str_int(R2, Cat),
  opg(Tijd,Cat,Tgnst,_,_,_,_,_,_,_,_),
  tgnstNum(Tgnst,Sp),
  sp2(Sp,_SexN,_NaamN,_Tuis,_Verh,_RE,_RD,_Betaald,_UitK, _Club, _Adr, _Wnpl, _BondsNr, _ES, _DS, _Geb, _Twerk, _Tmob, _EMail, Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Res),
  assert(opgSp(Cat, Tijd, Sp, Opm)),
  fail.
xml_inschrijfExp(_Win, _List) :-
  date(DY, DM, DD),
  time(TH, TM, _, _),
  helestr_int(0,TMS, TM),
  sleuteltekst1(Park),
  trim_c(Park),
  writedevice(work),
  write("<?xml version=\"1.0\"  encoding=\"iso-8859-1\" ?>",
	"\n<!DOCTYPE TOERNOOI SYSTEM \"..\\inschrijving.dtd\">"),
  writef("\n<!--\nInschrijvingen uitgevoerd op %u/%u/%u %u:%s\n%s\n-->", DD, DM, DY, TH, TMS, Park),
  write("\n<TOERNOOI dtdversie=\"1.0\""),
  tasversie(Versie),
  format(Plannertype, "Toernooi Assistent versie %s", Versie),
  xml_gegeven("plannertype", Plannertype),
  naam(Naam, _, _, _, _, _),
  xml_xlate(Naam, NaamX),
  xml_gegeven("naam", NaamX),
  fail.
xml_inschrijfExp(_Win, _List) :-
  write(">\n"),
  write("<INSCHRIJVINGEN afkomst=\"zelf\">"),
  opgSp(Cat, Tijd, Sp, _Opm),
  wc(Cat, _, _, _, e, _, _, _, _, _, _, _, _, _, _),
  %wc(Cat,_,e,_,_,_,_), 			% eerst met enkelspel
  adjustTijd(Tijd, Tijd1a),
  dt_minoffset_to_str(Tijd1a, "\n<INSCHRIJVING dag=\"%DD\" maand=\"%MD\" jaar=\"%YL\" tijd=\"%HH:%MM\">", TStr),
  write(TStr),
  expSpeler("  ", "SPELER", Sp, 0),
  write("\n  <ONDERDELEN>"), 
  exToernooiOnderdelen(Cat, Tijd, Sp),
  write("\n  </ONDERDELEN>"),
  write("\n</INSCHRIJVING>"),
  fail.
xml_inschrijfExp(_Win, _List) :-
  opgSp(Cat, Tijd, Sp, _Opm),		% dan overgebleven dubbelspelen
  adjustTijd(Tijd, Tijd1a),
  dt_minoffset_to_str(Tijd1a, "\n<INSCHRIJVING  dag=\"%DD\" maand=\"%MD\" jaar=\"%YL\" tijd=\"%HH:%MM\">", TStr),
  write(TStr),
  expSpeler("  ", "SPELER", Sp, 0),
  write("\n  <ONDERDELEN>"), 
  exToernooiOnderdelen(Cat, Tijd, Sp),
  write("\n  </ONDERDELEN>"),
  write("\n</INSCHRIJVING>"),
  fail.
xml_inschrijfExp(Win, _List) :-
  write("\n</INSCHRIJVINGEN>"),
  write("\n</TOERNOOI>"),
  closefile(work),
  cursor_Set(Win, cursor_Arrow),
  retractall(opgSp(_, _, _, _)),
  get_TempDir(_, TempFile),
  file_str(TempFile, Text),
  getfolder(FDir,_,_,_),
  %get_Toernooidir(TDir, _, _),
  filenamePath(Brief, FDir, "inschrijvingen.xml"),
  Fname = dlg_GetFileName(Brief,["xml","*.xml","txt","*.txt","dat","*.dat"],"Resultaatuitvoer",[dlgfn_Save],"",_OutListFiles),
  not(Fname = ""), 
  filenamepath(Fname, _Path, Name),
  Name <> fctconst,
  Name <> logconst,
  file_str(Fname, Text), !.
xml_inschrijfExp(_, _).

int_str_sp(0, "") :- !.
int_str_sp(N, Uit) :-
  str_int(Uit, N), !.
int_str_sp(In, "") :-
  bound(In), !.
int_str_sp(0, In) :-
  bound(In), !.

/*
wcritP(Cat, Speelsterkte, LeeftV, LeeftTM, Res) :-
  wcrit(Cat, Speelsterkte, LeeftV, LeeftTM, Res), !.
wcritP(_, "", 0, 0, 0).
*/
exToernooiOnderdelen(_, _, Sp) :-
  retract(opgSp(Cat1, Tijd1, Sp, _)),
  wc(Cat1, _, _, Onderdeel, _, _, _, Speelsterkte, leeftijd(LeeftVanS,_), leeftijd(LeeftTMS,_), _, _, _, _, _),
  xml_onderdeelnaam(Onderdeel, XMLnaam),
  xSpelsoort(Cat1, Geslacht, _, Spelsoort, _),
  writef("\n  <ONDERDEEL naam=\"%s\" spelsoort=\"%s\" geslacht=\"%s\"", XMLnaam, Spelsoort, Geslacht),
  xml_gegeven("speelsterkte", Speelsterkte),
  xml_gegeven("leeftijdvanaf", LeeftVanS),
  xml_gegeven("leeftijdtotenmet", LeeftTMS),
  write(">"),
  opg(Tijd1,Cat1,Tgnst,_,_,_,_,_,_,_,_),
  expPartner(Tgnst, Cat1, Tijd1, Sp),
  write("\n  </ONDERDEEL>"), !.
exToernooiOnderdelen(_, _, _Sp).

xml_adres(In, Straat, HuisNr) :-
  lastblank(In, 0, Pos),
  frontstr(Pos, In, Straat, HuisNr), 
  str_len(HuisNr, X),
  X <= 5,
  trim_c(Straat), !.
xml_adres(In, In, "").
  
lastblank(Str, In, Uit) :-
  searchchar(Str, ' ', Pos),
  P1 = Pos, % + 1,
  frontstr(P1,Str,_,Rest),
  In1 = In + P1, !,
  lastblank(Rest, In1, Uit).
lastblank(_, In, In).

xml_woonplaats(Str, Postcode, Stad) :-
  fronttoken(Str, PC1, Rest),
  str_len(PC1, 4),
  fronttoken(Rest, PC2, Stad),
  str_len(PC2, 2),
  format(Postcode, "%s %s", PC1, PC2),
  trim_c(Stad), !.
xml_woonplaats(Str, "", Str).


/*
xml_verhinderingen([H|Rest], OffsetS) :-
  H = per(K1a, K2),
  adjustTijd(K1a, K1),
  tijd_kwart(D1,_,_,K1),
  dt_dateoffset_to_str(D1, "dag=\"%Dd\" maand=\"%Md\" jaar=\"%YL\"", DS1),
  tijd_str(K1,KS1,_),
  trim_c(KS1),
  tijd_str(K2,TS3,_), !,
  trim_c(TS3),
  writef("\n%s  <VERHINDERING %s tijdvan=\"%s\" tijdtot=\"%s\"/>", OffsetS, DS1, KS1, TS3),  
  xml_verhinderingen(Rest, OffsetS).
xml_verhinderingen(_, _).
*/
xml_uitkantoor(In, K1) :-
  K1 > 0,
  tijd_str(K1,KS1,_),
  trim_c(KS1),
  writef(" %s=\"%s\"", In, KS1), !.
xml_uitkantoor(_, _).

expPartner(p(Sp1, Sp2), Cat, Tijd, Sp1) :-
  retract(opgSp(Cat, Tijd, Sp2, _)),
  expSpeler("    ", "PARTNER", Sp2, 0), !.
expPartner(p(Sp1, Sp2), Cat, Tijd, Sp2) :-
  retract(opgSp(Cat, Tijd, Sp1, _)),
  expSpeler("    ", "PARTNER", Sp1, 0), !.
expPartner(_, _, _, _).

terugloop(OffsetS, Tel, Tel1, Tel2, EMail, Knltb,StS,StD, Geb, Club) :-
  format(Tot, "%s%s%s%s%s%s%s%s%s", Tel, Tel1, Tel2, EMail, Knltb,StS,StD, Geb, Club),
  fronttoken(Tot, _, _),
  write("\n", OffsetS),!.
terugloop(_, _, _, _, _, _, _, _, _, _).

geboortedatum(Geb, Dag, Maand, Jaar1) :-
  fronttoken(Geb, Dag, Rest),
  fronttoken(Rest, _, R1),
  fronttoken(R1, Maand, _),
  lasttoken(Geb, _, Jaar), 
  add_gebjr(Jaar, Jaar1), !.
geboortedatum(_, "", "", "").
  
expSpeler(OffsetS, Element, Sp, _) :-
  concat(OffsetS, "  ", OffsetS1),
  write(OffsetS,"\n",OffsetS,"<", Element),
  sp2(Sp,Sex,Naam,Tel,_Verh,RatingE,RatingD,_Betaald,_Uitkant, Club, Adr, Wnpl, Knltb, StS, StD, Geb, Tel1, Tel2, EMail, _Opm,_Gezien,RangPosE,RangPosD,_Incasso,_CheckGeg,_IncassoAkkoord,_Res),
  spsrt(Sex, _, MV,_,_,_,_,_), 
  sex1(MVS, MV),
  xml_gegeven("geslacht", MVS),
  xml_naamscan(Naam, Voornaam, _, Achternaam),
  xml_voorvoegsel(Voornaam, Voornaam1, " tussenvoegsels"),
  xml_gegeven("achternaam", Achternaam),
  xml_gegeven("voornaam", Voornaam1),
  xml_adres(Adr, Straat, HuisNr),
  xml_gegeven("adresstraat", Straat),
  xml_gegeven("adreshuisnummer", HuisNr ),
  xml_woonplaats(Wnpl, Postcode, Stad),
  xml_gegeven("adrespostcode", Postcode),
  xml_gegeven("adreswoonplaats", Stad),
  terugloop(OffsetS1, Tel, Tel1, Tel2, EMail, Knltb,StS,StD, Geb, Club),
  xml_gegeven("telefoonhuis", Tel),
  xml_gegeven("telefoonwerk", Tel1),
  xml_gegeven("telefoonmobiel", Tel2),
  xml_gegeven("emailadres", EMail),
  xml_gegeven("knltbnummer", Knltb),
  xml_gegeven("speelsterkteenkel", StS),		% was speelsterkteenkel
  xml_gegeven("speelsterktedubbel", StD),		% was speelsterktedubbel
  xml_gegeven("enkelrating", RatingE),
  xml_gegeven("dubbelrating", RatingD),
  xml_gegeven("enkelranglijstpositie", RangPosE),
  xml_gegeven("dubbelranglijstpositie", RangPosD),
  geboortedatum(Geb, Dag, Maand, Jaar),
  xml_gegeven("geboortedag", Dag),
  xml_gegeven("geboortemaand", Maand),
  xml_gegeven("geboortejaar", Jaar),
  fail.
expSpeler(OffsetS, _Element, Sp, 0) :-	% alleen als niet beperkt
  concat(OffsetS, "  ", OffsetS1),
  sp2(Sp,_Sex,_Naam,_Tel,Verh,_RatingE,_RatingD,_Betaald,Uitkant, Club, _Adr, _Wnpl, _Knltb, _StS, _StD, _Geb, _Tel1, _Tel2, _EMail, Opm,_Gezien,_RangPosE,_RangPosD,Incasso,_CheckGeg,IncassoAkkoord,_Res),
  xml_uitkantoor("uitkantoortijd", UitKant), 
  xml_gegeven("verenigingsnummer", Club),		% was clubcode
  xml_gegeven("banknummer", Incasso),
  xml_gegeven("incassoakkoord", IncassoAkkoord),
  xml_gegeven("opmerkingen", Opm),
  write(">"),
  not(Verh = []),
  concat(OffsetS1, "  ", OffsetS2),
  write("\n", OffsetS2),
  %write("<VERHINDERINGEN>"),
  %xml_verhinderingen(Verh, OffsetS2),
  %write("\n", OffsetS1, "</VERHINDERINGEN>"),
  fail.
expSpeler(_OffsetS, _Element, _Sp, 1) :-
  write(">"),
  fail.
expSpeler(OffsetS, Element, _Sp, _) :-
  write("\n",OffsetS,"</",Element,">"), !.

database - formu
  single enkelspelSpSt(boolean)
  single dubbelspelSpSt(boolean)
  bestandM(string Naam, date_offset)
  bestandS(string Naam, date_offset)
  single wasAlVerbinding(boolean)

predicates
nondeterm dagRepr(string, ilist) - (o, i)  %, integer DagNo)
procedure toggle1(string, string)
procedure xml_Form(WINDOW, integer)
procedure xml_Form1(WINDOW)
procedure saveCats(slist, catl)
procedure toernooidagenZonderVerhindering(slist, ilist, ilist)
procedure dagGeenVerh(integer, ilist, string)
procedure getbox(WINDOW, ilist, ilist)
 setbox(WINDOW, ilist, ilist)
 determ getPartnerGeg(ilist, ilist)
 determ getSpelerGeg(ilist, ilist)
determ verlangd(WINDOW, integer)
nondeterm  gegs(string ID, string Naam, string Verplicht, integer Indic)
nondeterm  pgegs(string ID, string Naam, string Verplicht, integer Indic)
procedure selectTab(WINDOW, integer Indicator)
%procedure xml_inschrijfgeld(BOOLEAN, real)
  getFontforDialog(WINDOW,FONT)
  error
procedure generiekE(catl, BOOLEAN)
procedure generiekD(catl, BOOLEAN)
procedure generiekL(catl, BOOLEAN)
procedure profielgeg(window)
procedure setVerhWijze(integer, window PEWin, window DDWin)
procedure xml_dagdelen(integer Dagdelen)
procedure xml_dagdelen1(string Dag)  
procedure uitkantMaxIndex(slist, string, integer, integer)
procedure verplicht(window, integer, string VerplIn, string Verpl)
procedure bool2flag(boolean, integer Wsf)
%procedure checked2disabled(boolean, integer Wsf)
procedure disableUitkant(boolean, window UitKantMax, window Tekst)
procedure bool2JN(boolean, string)
procedure enablemaxverh(boolean, window MV1Win, window MV2Win, window MV3Win, window MV4Win)
nondeterm form_prereq(string, janee Updaten)
procedure reglementAanwezig(string Formmap)
determ checkUiterste1(window, integer UitersteInDagen, integer Vandaag, integer ToernooiMaandag, boolean MetFocus)
procedure maxond(string, string)
procedure metFocus(window Win, integer Control, boolean)
determ bestandMP(string Formmap, string Bestand, date_offset In)
determ bestandSP(string Bestand, date_offset)
procedure deleteAls(string Bestandsnaam)
nondeterm knltbspeler(integer, string)
procedure spelersupload(ilist)
procedure eigenformulier(window, string)
procedure partnerwanted(window, string, string EDG)
procedure partnerdisable(WINDOW)
%procedure uploadreglement(string Fullname)
procedure weggelatenCats(slist, boolean)
procedure vervang(char, string, string)
procedure wd2s(wrk, string)
procedure addslashes(string, string)
determ    toernooiNaamWebOngelijk1(integer)
procedure copyerror()
procedure uploadForm(window, integer) %- (i,i)
procedure toernooitype(string Tid, string Type)

clauses

wasAlVerbinding(b_False).

formfileU(FileU, Best, 1) :-
  get_formmap(Dir),
  %member(Best,["reglement.html", "inschrijfform4.xml", "maakform.css"]),
  member(Best,["inschrijfform4.xml"]),
  filenamepath(FileU,Dir,Best),
  trap(existfile(FileU), _, true).
formfileU(FileU, Best, 2) :-	% FTP
  get_formmap(Dir),
  member(Best, ["maakform4.html", "reglement.html","maakform.css","maakform4.js","inschrijfform4.xml"]),
  filenamepath(FileU,Dir,Best),
  trap(existfile(FileU), _, true).

addslashes(Dir, Dir) :-
  str_len(Dir, Len),
  L1 = Len - 1,
  frontstr(L1,Dir,_In1,Tok),
  %lasttoken(In, In1, Tok),
  Tok = "\\", !.
addslashes(Dir, NDir) :-
  concat(Dir, "\\", NDir).

get_formmap(Map) :-
  getfolder(_Map0,Map,_,_).
  %format(Map, "%s\\Toernooien\\", Map0).
  
knltbSpeler(Nr, Naam) :-
  sp2(Nr,_SexC,Naam,_,_,_,_,_,_, _, _, _, Knltb, _StE, _StD, _, _, _, _, _,_,_,_,_,_,_,_),
  %fronttoken(StE, _, _),
  %fronttoken(StD, _, _),
  fronttoken(Knltb, _, _),
  proef11det(Knltb).

vervang(Char, Tser, Tser1) :-
  Char >= '0',
  Char <= '9',	% zet char er weer bij
  frontchar(Tser1,Char,Tser),  !.
vervang(_, Tser, Tser).

autoNewInternetId(NewId) :-
  naam(Naam, Nr, Datum, Kode0, Distr, OokReserve),
  makeTicket(Kode0, Kode), 
  %fronttoken(Kode, _, _),
  reversestr(Kode, "", Edok),
  frontchar(Edok,Char,Tser),
  vervang(Char, Tser, Tser1),  % haal eventueel extra char er af
  reversestr(Tser1, "", IKode),
  ParmsList = aanmeldenVars(),
  providerX(Provider, _PN, "", _),
  NParms = ["oldid", IKode | ParmsList],
  format(URL, "http://%s/inetcheck.php", Provider),
  get_Tempdir(TDir, _), 
  filenamepath(FeedbackXML, TDir, "feedback.xml"),
  %autowebP(_, _, _, _, _, _, _, GeenVaste,_,_),
  %vragenOmverbinding(GeenVaste, b_False),
  deletefile(FeedbackXML),
  0 = downloadData(URL, FeedbackXML, NParms, _, 0),
  file_str(FeedbackXML, Tekst),
  parseMsg(Tekst, MList,_),
  MList = [NewId | _],
  reserveKopie(1),
  dlg_MessageBox( "Toernooi", "InternetID wordt veranderd.\nEr is een back-up gemaakt zodat u desnoods terug kunt naar het oude ID.", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  logf('A', naam(Naam, Nr, Datum, NewId, Distr, OokReserve), nee),
  compress(update),
  startdbrefresh(), !.

toernooiNaamWebOngelijk1(2) :- !.
toernooiNaamWebOngelijk1(_) :-
  write("\nnaamdif"),
  onlinebackup("naamdif"),
  fail.

toernooiNaamWebOngelijk(b_False) :-
  %write("\nin toernooinaamwebongelijkq?"),
  ParmsList = aanmeldenVars(),
  providerX(Provider, _, "", _),
  format(URL, "http://%s/getToernooiNaam.php", Provider),
  get_tempdir(TDir, _),
  filenamepath(FeedbackXML, TDir, "feedback.xml"),
  deletefile(FeedbackXML),	% downloadData appends!!
  0 = downloadData(URL, FeedbackXML, ParmsList, _, 0),
  file_str(FeedbackXML, ToerNaam),
  not(searchstring(ToerNaam,"geen",_)),
  not(ToerNaam = ""),
  naam(NaamNieuw, _Nr, _Datum, _WebTicket, _Distr, _OokReserve),
  write("### ", ToerNaam, "/n### ", NaamNieuw),
  not(ToerNaam = NaamNieuw),
  format(Txt, "Op het punt om iets op het Web te overschrijven van een toernooi met een andere toernooinaam!\nOp Internet: %s\nNieuw:           %s\nIs dit hetzelfde toernooi?", ToerNaam, NaamNieuw), 
  RR = dlg_MessageBox( "Toernooinaamcheck", Txt, mesbox_iconexclamation, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
  toernooiNaamWebOngelijk1(RR),
  not(autoNewInternetId()), !.
toernooiNaamWebOngelijk(b_False) :-
  comline(LineBuffer),
  upper_lower(LineBuffer, LBL),
  not(searchstring(LBL, "/x", _)),		% in TEST niet
  naam(Naam, Nr, Datum, Id, Distr, OokReserve),
  Id = "",
  maketicket("", Ticket),
  logf('A', naam(Naam, Nr, Datum, Ticket, Distr, OokReserve), nee),
  compress(update),
  startdbrefresh(),
  fail.



uploadForm(_, _) :-
  backup("email", ja),
  toernooiNaamWebOngelijk(b_False),
  !.
uploadForm(Win, HTML1FTP2) :-		% maak de xml voor het formulier
  xML_Form(Win, HTML1FTP2),
  fail.
uploadForm(_, _) :-		% maak de xml van de spelergegevens
  get_formmap(Dir),
  filenamepath(FKlap, Dir, "maakformklapper.html"),
  trap(existfile(FKlap),_,true),
  filenamepath(SKlap, Dir, "spelers.xml"),
  deletefile(SKlap),
  %comline(LineBuffer),
  %upper_lower(LineBuffer, LBL),
  %not(searchstring(LBL, "/x", _)),		% in TEST niet
  findall(Sp, knltbSpeler(Sp, _), SpList),
  openwrite(work, SKlap),
  writedevice(work),
  spelersupload(SpList),
  closefile(work),
  fail.
uploadForm(_, _) :-		% upload alle bestanden
  get_tempdir(Dir, _),
  filenamepath(Outfile, Dir, "AA_Opsturen"),
  closefile(work),
  openwrite(work,Outfile),
  writedevice(work),
  formfileU(FileX,_, 1),	% 16.4.2010 nog maar 1 bestand
  filenamepath(FileX,_,Kort),
  write("***[inschrijf][", Kort, "]***\n"),
  trap(file_str(FileX, Str),_,trapmsg(FileX)),
  write(Str, "\n"),
  closefile(work),
  %write("###\n", Str, "\n"),
  fail.
uploadForm(_, HTML1FTP2) :-		% upload alle bestanden
  closefile(work),
  get_tempdir(Dir, _),
  filenamepath(Outfile, Dir, "AA_Opsturen"),
  filenamepath(Report, Dir, "uitklaar.html"),
  providerX(Prov, PN, "", _),
  checkvoortgang(0, "Voortgang"),
  format(Bezig, "bezig met uploaden van het formulier naar %s", PN),
  checkvoortgang(1, Bezig),
  format(URL, "http://%s/%s?stap=een&type=formulier", Prov, "webload.php"),
  write("\n#### ",URL,"\n"),
  Vars = aanmeldenVars(),
  %str_int(NSstr, NoShow),
  concat(OutFile, ".gz", OutFileC),
  file_compress(OutFile, OutFileC),
  _ReturnK = fileupload(URL, OutfileC, Report, Vars),
  %write(ReturnK),
  HTML1FTP2 = 1,
  checkvoortgang(98, "Klaar"),
  checkvoortgang(100, ""),
  file_str(Report, RepS),
  write(RepS),
  Session = visSession(Report, "BEHEER="),
  format(KURL, "http://%s/webload.php?%s&stap=twee&type=form", Prov, Session),
         %win_editor_Create(Taskwin, Text, Titel),
  %write("\n###KURL: ",KURL),
  Null = cast(api_hwnd,0),
  api_ShellExecute(Null,"open",KURL,"","",1),		% open in browser
  onlinebackup("formu"), !.
uploadForm(_, _) :-		% ftp ook nog indien gewenst ...
  autowebP(_Regelmaat,HTML1FTP2,_Waarschuwen,Gebruiker,Wachtwoord, FTPadres, _Pad, _Vasteverb, _Hoevaak, _BijSluiten),
  HTML1FTP2 = 2,
  wasAlVerbinding(b_True),
  findall(FileX, formfileU(_, FileX, 2), Files), 
  get_formmap(Dir),
  format(URL, "ftp://%s",FTPadres),
  format(GebruikerWW,"%s:%s",Gebruiker, Wachtwoord),
  format(Bezig, "ftp naar %s..", URL),
  checkvoortgang(12, Bezig),
  Return = uploadFTP(URL, Dir, Files, "rapport.txt", GebruikerWW, 0),
  format(Klaar, "Klaar met uploadftp: %u", Return),
  checkvoortgang(98, Klaar),
  checkvoortgang(100, ""),
  S1 = "\nTip: voeg het formulier toe aan je favorieten!",
  S2 = "\n\nNB Het formulier op toernooiklapper.nl is veelzijdiger (meerdere Browsers etc.)!\n",
  format(Boodschap, "Het webadres (URL) is waarschijnlijk een ander dan het FTP adres.\nZoek zelf maakform4.html op je website op om te checken of alles OK is!%s%s", S1, S2),
  dlg_MessageBox( "Formulier ge-upload!", Boodschap, mesbox_iconinformation, mesbox_buttonsOK, mesbox_defaultfirst, mesbox_suspendapplication ), 
  !.
uploadForm(_, _) :-
  writetojournaal( "Formulier upload mislukt of afgebroken.\nHet formulier kan best nog net wel ge¸pload zijn.\nEr zit waarschijnlijk iets fout in uw installatie!", ja).

spelersupload(SpList) :-
  write("<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>\n"),
  write("<PERSONEN>"),
  member(Sp, SpList),
  expSpeler("", "SPELER", Sp, 1),	% 1 = beperkt
  fail.
spelersupload(_SpList) :-
  write("</PERSONEN>"), !.

enkelspelSpSt(b_False).
dubbelspelSpSt(b_False).

gegs("voornaam","Voornaam","J",0).
gegs("tussenvoegsels","Tussenvoegsels","N",0).
gegs("achternaam","Achternaam","J",0).
gegs("geslacht","M/V","J",0).
gegs("adresstraat", "Straat","N", idc_adres).
gegs("adreshuisnummer", "","N", idc_adres).
gegs("adrespostcode", "", "N", idc_adres).
gegs("adreswoonplaats", "", "N", idc_adres).
gegs("telefoonhuis","Telefoon","N",idc_telefoon).
gegs("telefoonwerk","Telefoon werk","N",idc_werk).
gegs("telefoonmobiel","Telefoon mobiel","N",idc_mobiel).
gegs("emailadres","E-mailadres","J",idc_emailadres).
gegs("incasso","Bank/gironummer","J",idc_incassonr).	% 8.1.2004
gegs("incassoaccoord","Incasso accoord","N",2).		% 26.3.2004
gegs("knltbnummer","Knltb-nummer","N",idc_knltbnummer_vereist).
gegs("speelsterkteenkel","Speelsterkte enkelspel","J",idc_speelsterkte_enkelspel).
gegs("speelsterktedubbel","Speelsterkte dubbelspel","J",idc_speelsterkte_dubbelspel).
gegs("competitie","Competitieklasse","N",idc_vragen_om_competitieklasse).
gegs("geboortedatum","Geboortedatum","N",idc_geboortedatum).
gegs("uitkantoortijd","Uit kantoor om:","N",idc_uitkantoortijd).		% 1 = check maxverhinderingen speler

pgegs("voornaam","Voornaam","J",0).
pgegs("tussenvoegsels","Tussenvoegsels","N",0).
pgegs("achternaam","Achternaam","J",0).
pgegs("geslacht","M/V","J",0).
pgegs("adresstraat", "Straat","N", idc_adres1).
pgegs("adreshuisnummer", "","N",idc_adres1).
pgegs("adrespostcode", "", "N", idc_adres1).
pgegs("adreswoonplaats", "", "N", idc_adres1).
pgegs("telefoonhuis","Telefoon","N",idc_telefoon1).
pgegs("telefoonwerk","Telefoon werk","N",idc_werk1).
pgegs("telefoonmobiel","Telefoon mobiel","N",idc_mobiel1).
pgegs("emailadres","E-mailadres","N",idc_emailadres1).
pgegs("knltbnummer","Knltb-nummer","Z",idc_knltbnrpartner).
pgegs("geboortedatum","Geboortedatum","N",idc_geboortedatum1).
pgegs("speelsterkteenkel","Speelsterkte enkelspel","Z",idc_spste).
pgegs("speelsterktedubbel","Speelsterkte dubbelspel","Z",idc_spstd).

bool2flag(b_False, wsf_Disabled) :- !.
bool2flag(_, wsf_Enabled).

%checked2disabled(b_True, wsf_Disabled) :- !.
%checked2disabled(_, wsf_Enabled) :- !.

getbox(_Win, [KNLTBS,GebS,StES,StDS,AdrS,TelS,TelSW,TelSM,EMailS,EmailVerplicht,Competitie,Incasso,IncassoVerpl,Opmerkingen],
             [KNLTBP,GebP,StEP,StDP,AdrP,TelP,TelPW,TelPM,EMailP,PartnerWanted,EigenForm]) :-
	KPWin = win_GetCtlHandle(_Win, idc_knltbnrpartner),
	KNLTBP = win_IsChecked(KPWin),
	GebPWin = win_GetCtlHandle(_Win, idc_geboortedatum1),
	GebP = win_IsChecked(GebPWin),
	StEPWin = win_GetCtlHandle(_Win, idc_spste),
	StEP = win_IsChecked(StEPWin),
	StDPWin = win_GetCtlHandle(_Win, idc_spstd),
	StDP = win_IsChecked(StDPWin),
	AdrPWin = win_GetCtlHandle(_Win, idc_adres1),
	AdrP = win_IsChecked(AdrPWin),
	TelPWin = win_GetCtlHandle(_Win, idc_telefoon1),
	TelP = win_IsChecked(TelPWin),
	TelPMWin = win_GetCtlHandle(_Win, idc_mobiel1),
	TelPM = win_IsChecked(TelPMWin),
	TelPWWin = win_GetCtlHandle(_Win, idc_werk1),
	TelPW = win_IsChecked(TelPWWin),
	EMPWin = win_GetCtlHandle(_Win, idc_emailadres1),
	EMailP = win_IsChecked(EMPWin),			%
	KSWin = win_GetCtlHandle(_Win, idc_knltbnummer_vereist),
	KNLTBS = win_IsChecked(KSWin),
	GebSWin = win_GetCtlHandle(_Win, idc_geboortedatum),
	GebS = win_IsChecked(GebSWin),
	StESWin = win_GetCtlHandle(_Win, idc_speelsterkte_enkelspel),
	StES = win_IsChecked(StESWin),
	StDSWin = win_GetCtlHandle(_Win, idc_speelsterkte_dubbelspel),
	StDS = win_IsChecked(StDSWin),
	AdrSWin = win_GetCtlHandle(_Win, idc_adres),
	AdrS = win_IsChecked(AdrSWin),
	TelSWin = win_GetCtlHandle(_Win, idc_telefoon),
	TelS = win_IsChecked(TelSWin),
	TelSMWin = win_GetCtlHandle(_Win, idc_mobiel),
	TelSM = win_IsChecked(TelSMWin),
	TelSWWin = win_GetCtlHandle(_Win, idc_werk),
	TelSW = win_IsChecked(TelSWWin),
	EMSWin = win_GetCtlHandle(_Win, idc_emailadres),
	EMailS = win_IsChecked(EMSWin),
	EMSVWin = win_GetCtlHandle(_Win, idc_emailverplicht),
	EMailVerplicht = win_IsChecked(EMSVWin),
	CompWin = win_GetCtlHandle(_Win, idc_vragen_om_competitieklasse),
	Competitie = win_IsChecked(CompWin),
	IncWin = win_GetCtlHandle(_Win, idc_incassonr),
	Incasso = win_IsChecked(IncWin),
	IncVerplWin = win_GetCtlHandle(_Win, idc_incassoverplicht),
	IncassoVerpl = win_IsChecked(IncVerplWin),
	PWWin = win_GetCtlHandle(_Win, idc_partner_wanted_toestaan), 
	PartnerWanted = win_IsChecked(PWWin),
	EigWin = win_GetCtlHandle(_Win, idc_partner_moet_eigen_formulier_invullen),
	EigenForm = win_IsChecked(EigWin),
	OpmWin = win_GetCtlHandle(_Win, idc_opmerkingen),
	Opmerkingen = win_IsChecked(OpmWin), !.

getPartnerGeg([KNLTBP,GebP,StEP,StDP,AdrP,TelP,TelPW,TelPM,EMailP,PartnerWanted],
              [KNLTBP,GebP,StEP,StDP,AdrP,TelP,TelPW,TelPM,EMailP,PartnerWanted,1]) :- !.
getPartnerGeg([KNLTBP,GebP,StEP,StDP,AdrP,TelP,TelPW,TelPM,EMailP,PartnerWanted,EigenForm],
              [KNLTBP,GebP,StEP,StDP,AdrP,TelP,TelPW,TelPM,EMailP,PartnerWanted,EigenForm]).

getSpelerGeg([KNLTBS,GebS,StES,StDS,AdrS,TelS,TelSW,TelSM,EMailS,EMailVerplichtS,Competitie,Incasso,IncassoVerpl,Opmerkingen],
             [KNLTBS,GebS,StES,StDS,AdrS,TelS,TelSW,TelSM,EMailS,EMailVerplichtS,Competitie,Incasso,IncassoVerpl,Opmerkingen]) :- !.
getSpelerGeg([KNLTBS,GebS,StES,StDS,AdrS,TelS,TelSW,TelSM,EMailS,EMailVerplichtS,Competitie,Incasso,IncassoVerpl],
             [KNLTBS,GebS,StES,StDS,AdrS,TelS,TelSW,TelSM,EMailS,EMailVerplichtS,Competitie,Incasso,IncassoVerpl,1]) :- !.
getSpelerGeg([KNLTBS,GebS,StES,StDS,AdrS,TelS,TelSW,TelSM,EMailS,EMailVerplichtS,Competitie,Incasso],
             [KNLTBS,GebS,StES,StDS,AdrS,TelS,TelSW,TelSM,EMailS,EMailVerplichtS,Competitie,Incasso,1,1]).

setbox(_Win, SpelerGeg, PartnerGeg) :-
	getSpelerGeg(SpelerGeg, [KNLTBS,GebS,StES,StDS,AdrS,TelS,TelSW,TelSM,EMailS,EMailVerplichtS,Competitie,Incasso,IncassoVerpl,Opmerkingen]),
        getPartnerGeg(PartnerGeg, [KNLTBP,GebP,StEP,StDP,AdrP,TelP,TelPW,TelPM,EMailP,PartnerWanted,EigenForm]),
	KPWin = win_GetCtlHandle(_Win, idc_knltbnrpartner),
	win_Check(KPWin, KNLTBP),
	GebPWin = win_GetCtlHandle(_Win, idc_geboortedatum1),
	win_Check(GebPWin, GebP),
	StEPWin = win_GetCtlHandle(_Win, idc_spste),
	win_Check(StEPWin, StEP),
	StDPWin = win_GetCtlHandle(_Win, idc_spstd),
	win_Check(StDPWin, StDP),
	AdrPWin = win_GetCtlHandle(_Win, idc_adres1),
	win_Check(AdrPWin, AdrP),
	TelPWin = win_GetCtlHandle(_Win, idc_telefoon1),
	win_Check(TelPWin, TelP),
	TelPMWin = win_GetCtlHandle(_Win, idc_mobiel1),
	win_Check(TelPMWin, TelPM),
	TelPWWin = win_GetCtlHandle(_Win, idc_werk1),
	win_Check(TelPWWin, TelPW),
	EMPWin = win_GetCtlHandle(_Win, idc_emailadres1),
	win_Check(EMPWin, EMailP),
	KSWin = win_GetCtlHandle(_Win, idc_knltbnummer_vereist),
	win_Check(KSWin, KNLTBS),
	GebSWin = win_GetCtlHandle(_Win, idc_geboortedatum),
	win_Check(GebSWin, GebS),
	StESWin = win_GetCtlHandle(_Win, idc_speelsterkte_enkelspel),
	win_Check(StESWin, StES),
	StDSWin = win_GetCtlHandle(_Win, idc_speelsterkte_dubbelspel),
	win_Check(StDSWin, StDS),
	AdrSWin = win_GetCtlHandle(_Win, idc_adres),
	win_Check(AdrSWin, AdrS),
	TelSWin = win_GetCtlHandle(_Win, idc_telefoon),
	win_Check(TelSWin, TelS),
	TelSMWin = win_GetCtlHandle(_Win, idc_mobiel),
	win_Check(TelSMWin, TelSM),
	TelSWWin = win_GetCtlHandle(_Win, idc_werk),
	win_Check(TelSWWin, TelSW),
	EMSWin = win_GetCtlHandle(_Win, idc_emailadres),
	win_Check(EMSWin, EMailS), 
	CompWin = win_GetCtlHandle(_Win, idc_vragen_om_competitieklasse),
	win_Check(CompWin, Competitie),
	EMSVWin = win_GetCtlHandle(_Win, idc_emailverplicht),
	win_Check(EMSVWin, EMailVerplichtS),
	bool2Flag(EMailS, WsFlag),
	win_SetState(EMSVWin, [Wsflag]),
	IncWin = win_GetCtlHandle(_Win, idc_incassonr),
	win_Check(IncWin, Incasso), 
	IncVerplWin = win_GetCtlHandle(_Win, idc_incassoverplicht),
	win_Check(IncVerplWin, IncassoVerpl), 
	bool2Flag(Incasso, WsFlagI),
	win_SetState(IncVerplWin, [WsflagI]),
	OpmWin = win_GetCtlHandle(_Win, idc_opmerkingen),
	win_Check(OpmWin, Opmerkingen), 
	PWWin = win_GetCtlHandle(_Win, idc_partner_wanted_toestaan), 
	win_Check(PWWin, PartnerWanted), 
	partnerdisable(_Win),
	EigWin = win_GetCtlHandle(_Win, idc_partner_moet_eigen_formulier_invullen),
	win_Check(EigWin, EigenForm), 
	 !.
setbox(_Win, _, _).

wd2s(z, " -") :- !.
wd2s(_, "ja").

dagRepr(Dag1, NList) :-
  dag(D, _, Werkdag),  % z of w
  wd2s(Werkdag, WS),
  dagGeenVerh(D, Nlist, JN),
  dt_dateoffset_to_str(D, "%Dn %Dd-%Md-%YL", Dag),
  format(Dag1, "%s\t%s\t%s\t", Dag, JN, WS).

dagGeenVerh(D, NList, "N") :-
  member(D, NList), !.
dagGeenVerh(_, _, "J").

toggle1("J", "N") :- !.
toggle1(_, "J").

%geenCrit("", 0, 0).

catMetCrit(WC, Naam, OList) :-
  wc(WC, _, _, Naam, _, _, _, _, _, _, _, _, _, Tid, _),
  not(toernooiK(Tid, nrt)),
  not(isOnderCat(WC)),
  not(member(Naam, OList)).
  %wcrit(WC, Speelsterkte, LeeftijdVanAf, LeeftijdTotenMet, _),
  %not(geenCrit(Speelsterkte, LeefTijdVanAf, LeeftijdTotenMet)).

weggelatenCats(OList, b_True) :-
  findall(WC, catMetCrit(WC, _, OList), WCList),
  not(WCList = []), !.
weggelatenCats(_OList, b_False).

xml_dagdelen(1) :-
  write(" verhinderingen=\"dagdeel\" "), !.	% periode=default
xml_dagdelen(_).

scanRetourAdres(RText0, RText) :-
  searchchar(RText0, ';', Pos),
  P1 = Pos - 1,
  frontstr(P1, RText0, RText1, _),
  frontstr(Pos,RText0, _, RText2),
  format(RText, "%s,%s", Rtext1, RText2), !.
scanRetourAdres(RText, RText).

form_prereq("maakform4.html", nee).		% lower case please!
form_prereq("maakformklapper.html", nee).
form_prereq("maakform.css", ja).
form_prereq("maakform4.js", ja).

reglementAanwezig(Formmap) :-
  format(Reglement, "%sreglement.*", Formmap),
  existfile(Reglement), !,
  write(" reglement=\"J\"").
reglementAanwezig(_) :-
  write(" reglement=\"N\"").

bestandSP(Bestand, DateOffset) :-
  bestandS(Bestand, DateOffset), !.
bestandSP(Bestand, _) :-
  format(Msg, "Bestand %s niet gevonden!\nFormulier kan niet gemaakt worden.\nProgramma update (Help, TA update) kan helpen.", Bestand),
  dlg_MessageBox( "Formulierbestand", Msg, mesbox_iconerror, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  exit(0).

deleteAls(OldName) :-
  trap(existfile(OldName), _, true),
  trap(deletefile(OldName), _, true), !.
deleteAls(_Naam).

bestandMP(Formmap, Bestand, DateOffsetIn) :-
  bestandM(Bestand, DateOffsetM),
  DateOffsetIn > DateOffsetM,
  format(Msg, "Bestand %s vervangen door een nieuwere versie (aanbevolen)?\nHet bestaande bestand wordt gerenamed.", Bestand),
  1 = dlg_MessageBox( "Formulierbestanden verversen", Msg, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
  filenamepath(FullName, Formmap, Bestand),
  concat(FullName, ".oud", OldName),
  deleteAls(OldName),
  renamefile(FullName, OldName), !.
bestandMP(_Formmap, Bestand, _DateOffsetIn) :-
  not(bestandM(Bestand, _)).

copyerror() :-
  get_Formmap(FormMap),
  format(S1, "de toernooimap bevindt zich op een plaats %s\ndie Windows om beveiligingsredenen niet meer toestaat.\n", FormMap),
  S2 = "zet de toernooimap on-line (via toernooimapselectie; goede internetverbinding vereist), of\n",
  S3 = "sluit de TA, verhuis de toernooimap, open de TA en\nzoek de toernooimap weer op met toernooimapselectie.",
  format(S, "%s%s%s", S1, S2, S3),
  dlg_MessageBox( "upload mislukt!", S, mesbox_iconerror, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ).

toNrs(Tid1,Tid2) :-
  findall(Tid, toernooiK(Tid,_), TL),
  TL = [Tid1,Tid2], !.
toNrs(Tid, "") :-
  toernooiK(Tid,_), !.
toNrs("", "").

xml_form(_Win, 2) :-	% 12.4.2010 preqs alleen nog nodig bij FTP
  retractall(bestandM(_, _)),
  get_formmap(Formmap),
  addslashes(Formmap, FormmapM),
  format(Wild, "%s%s", FormmapM, "*.*"),
  dirfiles(Wild,0x40,FNam,_,_,_,_,Jaar,Maand,Dag,_),
  dt_date_to_offset(Jaar,Maand,Dag,DateOffset),
  upper_lower(FNam, FNamL),
  assert(bestandM(FNamL, DateOffset)),
  fail.
xml_form(_Win, 2) :-
  retractall(bestandS(_, _)),
  syspath(OSPath, _),
  form_prereq(PreReq, _),
  format(Wild, "%s\\%s", OSPath, PreReq),
  dirfiles(Wild,0x40,FNam,_,_,_,_,Jaar,Maand,Dag,_),
  dt_date_to_offset(Jaar,Maand,Dag,DateOffset),
  upper_lower(FNam, FNamL),
  assert(bestandS(FNamL, DateOffset)),
  fail.
xml_Form(_Win, 2) :-				% zorg voor de prereq-bestanden
  get_formmap(Formmap),
  syspath(OSPath, _),
  form_prereq(PreReq, _),
  bestandSP(PreReq, DateOffsetS),		% check of prereq er wel is
  bestandMP(FormMap, PreReq, DateOffsetS),	% check of prereq nieuwer is
  %format(PName, "%s%s", Formmap, PreReq),
  %format(Bron, "%s\\%s", OSPath, PreReq),
  filenamepath(PName,Formmap,PreReq),
  filenamePath(Bron, OSPath,PreReq),
  trap(copyfile(Bron,PName), _,copyerror()), 
  fail.
xml_Form(Win, _) :-
  retractall(bestandM(_, _)),
  retractall(bestandS(_, _)),
  get_formmap(Formmap),
  Name = "inschrijfform4.xml",
  filenamepath(FName, Formmap, Name),
  cursor_Set(Win, cursor_Wait),
  closefile(work),
  openwrite(work,FName),
  writedevice(work),
  write("<?xml version=\"1.0\"  encoding=\"iso-8859-1\" ?>"),
  write("\n<OPGAVEFORMULIER dtdversie=\"1.0\" "),
  BWin = win_GetCtlHandle(Win, idd_mededeling),
  BText = win_GetText(BWin),
  toBr(BText, BtextMet),
  xml_Gegeven("banner", BTextMet),
  naam(Naam, _NrS, _, _WebTicket,_,_),
  write("\n  "),
  tasversie(Versie),
  xml_gegeven("toernooiplannernaam", "ToernooiAssistent"),
  xml_gegeven("toernooiplannerversie", Versie),
  write(">\n"),
  xml_xlate(Naam, NaamX),
  writef("<TOERNOOI naam=\"%s\"", NaamX),
  reglementAanwezig(Formmap),
  write(">\n"),
  xml_Form1(Win),
  cursor_Set(Win, cursor_Arrow),
  closefile(work), !.
xml_Form(Win, _) :-
  cursor_Set(Win, cursor_Arrow).

/*
xml_inschrijfgeld(b_True, Geld) :-
  Geld > 0,
  writef(" inschrijfgeld=\"%3.2f\"", Geld), !.
xml_inschrijfgeld(_, _). */

xml_dagdelen1(Dag) :-
  dt_datestr_to_offset(Dag, "%Dn %Dd-%Md-%YL", DagNr),
  dag(DagNr, _, z),
  write(" werkdag=\"N\""), !.
xml_dagdelen1(_).

maxond("", "9") :- !.
maxond(X, X).

partnerwanted(Win, "toegestaan", Spelsoort) :-
  not(Spelsoort = "E"),
  PWWin = win_GetCtlHandle(Win, idc_partner_wanted_toestaan), 
  b_True = win_IsChecked(PWWin), !.
partnerwanted(_Win, "", _).

eigenformulier(Win, "verplicht") :-
  EigWin = win_GetCtlHandle(Win, idc_partner_moet_eigen_formulier_invullen),
  b_True = win_IsChecked(EigWin), !.
eigenformulier(_Win, "").

toernooitype(Tid, Type) :-
  toernooiK(Tid, Soort),
  term_str(toernooisoort, Soort, Type), !.
toernooitype(_, "").   

xml_Form1(Win) :-
  OWin = win_GetCtlHandle(Win, idd_onderdelen),
  OList = lbox_GetAll(OWin),
  member(Onderdeel, OList),
  wc(WC, _, _, Onderdeel, _, _, _,Speelsterkte, leeftijd(LeeftVanS,_), leeftijd(LeeftTMS,_), _, _, _, Tid, Geld),
  xml_onderdeelnaam(Onderdeel, XMLnaam),
  xSpelsoort(WC, Geslacht, _, Spelsoort, _),
  writef("  <ONDERDEEL naam=\"%s\" spelsoort=\"%s\" geslacht=\"%s\"", XMLnaam, Spelsoort, Geslacht),
  xml_gegeven("toernooiid", Tid),
  toernooitype(Tid, Type),
  xml_gegeven("toernooitype", Type), 
  xml_gegeven("speelsterkte", Speelsterkte),
  xml_gegeven("leeftijdvanaf", LeeftVanS),
  xml_gegeven("leeftijdtotenmet", LeeftTMS),
  format(GeldS, "%.2", Geld),
  xml_gegeven("bedrag", GeldS),
  partnerwanted(Win, PWOK, Spelsoort),
  xml_gegeven("partnerwanted", PWOK),
  write("/>\n"),
  fail.
xml_Form1(Win) :-
  %dlg_note("", "tascom xml_form1"),
  DagWin = win_GetCtlHandle(Win, idd_speeldagen),
  DList = lbox_GetAll(DagWin),
  member(DagS, DList),
  parsetabs(DagS, Elems),
  Elems = [Dag, JaNee, _ | _],
  write("  <SPEELDAG"),
  xml_gegeven("dag", Dag),
  xml_dagdelen1(Dag),
  xml_gegeven("verhinderingtoegestaan", JaNee, "J"),
  write(" />\n"),
  fail.
xml_Form1(Win) :-  
  OpWin = win_GetCtlHandle(Win, idc_opmerkingen),
  Opm = win_IsChecked(OpWin),
  bool2JN(Opm, OpmJN),
  MVWin = win_GetCtlHandle(Win, idd_maxverh),
  MV = win_GetText(MVWin),
  trim_c(MV),
  MOWin = win_GetCtlHandle(Win, idd_maxond),
  MO = win_GetText(MOWin),
  trim_c(MO),
  MUWin = win_GetCtlHandle(Win, idc_maxuitkantoor),
  MU = win_GetText(MUWin),
  VWWin = win_GetCtlHandle(Win, idc_voorweekend),
  VW = win_IsChecked(VWWin),
  bool2JN(VW, VWJN),
  write("  <SPELEROPGAVE "),
  xml_gegeven("maxverhinderingen",MV),
  maxond(MO, MO1),
  xml_gegeven("maxonderdelen",MO1),
  xml_gegeven("maxuitkantoortijd", MU),
  xml_gegeven("vorigeweektoernooi", VWJN),
  xml_gegeven("opmerkingen", OpmJN),
  DDWin = win_GetCtlHandle(Win, idc_dagdelen),
  Dagdelen = win_IsChecked(DDWin),
  xml_dagdelen(Dagdelen),
  write(">\n"),
  gegs(ID,Naam,Verpl,CtrlID),
  verlangd(Win, CtrlID),
  verplicht(Win, CtrlId, Verpl, Verpl1),
  write("    <GEGEVEN "),
  xml_Gegeven("identificatie",ID),
  xml_Gegeven("naam", Naam),
  xml_Gegeven("verplicht", Verpl1),	% "N"
  write("/>\n"),
  fail.
xml_Form1(Win) :-
  write("  </SPELEROPGAVE>\n"),
  write("  <PARTNEROPGAVE "),
  eigenformulier(Win, EF),
  xml_gegeven("eigenformulier", EF),
  write(">\n"),
  pgegs(ID,Naam,Verpl,CtrlID),
  verlangd(Win, CtrlID),
  write("    <GEGEVEN "),
  xml_Gegeven("identificatie",ID),
  xml_Gegeven("naam", Naam),
  xml_Gegeven("verplicht", Verpl, "N"),
  write("/>\n"),
  fail.
xml_Form1(_Win) :-
  write("  </PARTNEROPGAVE>\n"),
  write("</TOERNOOI>\n</OPGAVEFORMULIER>").

bool2JN(b_True, "J") :- !.
bool2JN(_, "N").

verlangd(_Win, 0) :- !.
verlangd(_Win, 1) :-
  MVWin = win_GetCtlHandle(_Win, idd_maxverh),
  MV = win_GetText(MVWin), !, 
  str_int(MV, I), 
  I > 0.
verlangd(_Win, 2) :-
  INWin = win_GetCtlHandle(_Win, idc_incassonr),
  IN = win_IsChecked(INWin), !, 
  IN = b_True.
verlangd(Win, CtrlID) :-
  CtrlWin = win_GetCtlHandle(Win, CtrlId),
  checkbox_on = win_IsChecked(CtrlWin), !.

verplicht(Win, idc_incassonr, _, "N") :-
  CtrlWin = win_GetCtlHandle(Win, idc_incassoverplicht),
  b_False = win_IsChecked(CtrlWin), !.
verplicht(Win, idc_emailadres, _, "N") :-
  CtrlWin = win_GetCtlHandle(Win, idc_emailverplicht),
  b_False = win_IsChecked(CtrlWin), !.
verplicht(_Win, _, Verpl, Verpl).

loadCats([WC|Rest], [Ond|Uit], [WC|UWC]) :-
  wc(WC, _, _, Ond, _, _, _, _, _, _, _, _, _, _Tid, _),
  not(isOnderCat(WC)), !,
  loadCats(Rest, Uit, UWC).
loadCats([_|Rest], Uit, UWC) :-
  loadCats(Rest, Uit, UWC), !.
loadCats(_, [], []).

saveCats([Onderdeel|Lijst], [WC|Uit]) :-
  wc(WC, _, _, Onderdeel, _, _, _, _, _, _, _, _, _, _, _), !,
  saveCats(Lijst, Uit).
saveCats(_, []).

toernooidagenZonderVerhindering([DagS|Rest], In, [DO|Uit]) :-
  parsetabs(DagS, Elems),
  Elems = [Dag, JaNee | _],
  JaNee = "N",
  trap(dt_datestr_to_offset(Dag, "%Dn %Dd-%Md-%YL", DO),_,true), !,
  toernooidagenZonderVerhindering(Rest, In, Uit).
toernooidagenZonderVerhindering([_|Rest], In, Uit) :- !,
  toernooidagenZonderVerhindering(Rest, In, Uit).
toernooidagenZonderVerhindering(_, In, In).

selectTab(Win, 0) :- 
  tabdlg_select(Win, idc_customtab, 1), !.
selectTab(Win, _) :-
  tabdlg_select(Win, idc_customtab, 2),
  dlg_MessageBox( "Onderdelen", "Er zijn onderdelen die geen deel uitmaken van het formulier.\nNakijken met 'Bij' en EVENTUEEL toevoegen!", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ).

  error.
  
  getFontforDialog(_Win,Font):-
	trap(Font=win_GetFont(_Win),_,error),!.
  getFontforDialog(_,Font):-
	Font=font_Create(ff_System,[],10).

profielGeg(Win) :-
  OWin = win_GetCtlHandle(Win, idd_onderdelen),
  OList = lbox_GetAll(OWin),
  saveCats(OList, CatL),
  generiekE(CatL, Enkel),	% zoek generieke karakteristieken  
  generiekD(CatL, Dubbel),
  generiekL(CatL, Leeftijd),
  GebSWin = win_GetCtlHandle(Win, idc_geboortedatum),
  win_Check(GebSWin, Leeftijd),
  StESWin = win_GetCtlHandle(Win, idc_speelsterkte_enkelspel),
  win_Check(StESWin, Enkel),
  StDSWin = win_GetCtlHandle(Win, idc_speelsterkte_dubbelspel),
  win_Check(StDSWin, Dubbel).

generiekE(CatL, b_True) :-
  member(WC, CatL),
  wc(WC, _, _, _, e, _, _, SpeelsterkteE, _, _, _, _, _, _, _),
  not(SpeelsterkteE = ""), !.
generiekE(_CatL, b_False).

generiekD(CatL, b_True) :-
  member(WC, CatL),
  wc(WC, _, _, _, WTyp, _, _, SpeelsterkteD, _, _, _, _, _, _, _),
  not(WTyp = e),
  not(SpeelsterkteD = ""), !.
generiekD(_CatL, b_False).

generiekL(CatL, b_True) :-
  member(WC, CatL),
  wc(WC, _, _, _, _, _, _, _, leeftijd(_,LeeftijdVanAf), _, _, _, _, _, _),
  LeeftijdVanAf > 5, !.
generiekL(CatL, b_True) :-
  member(WC, CatL),
  wc(WC, _, _, _, _, _, _, _, _, leeftijd(_,LeeftijdTM), _, _, _, _, _),
  LeeftijdTM > 0,
  LeeftijdTM < 90, !.
generiekL(_, b_False).

setVerhWijze(0, PEWin, DDWin) :-
  win_CheckRadioButton(PEWin, [PEWin, DDWin]), !.
setVerhWijze(_, PEWin, DDWin) :-
  win_CheckRadioButton(DDWin, [PEWin, DDWin]), !.

uitkantMaxIndex([UKMax|_], UKMax, N, N) :- !.
uitkantMaxIndex([_|List], UitKantMax, I, N) :-
  I1 = I + 1, !,
  uitkantMaxIndex(List, UitKantMax, I1, N).
uitkantMaxIndex(_, _UKMax, _, 0).

disableUitkant(b_True, UKMWin, UKMTWin) :-
  win_SetState(UKMWin, [wsf_enabled]),
  win_SetState(UKMTWin, [wsf_enabled]), !.
disableUitkant(_, UkMWin, UKMTWin) :-
  win_SetState(UKMWin, [wsf_disabled]),
  win_SetState(UKMTWin, [wsf_disabled]).

enablemaxverh(b_True, MV1Win, MV2Win, MV3Win, MV4Win) :-
  win_SetState(MV1Win, [wsf_enabled]),
  win_SetState(MV2Win, [wsf_enabled]),
  win_SetState(MV3Win, [wsf_disabled]),
  win_SetState(MV4Win, [wsf_disabled]), !.
enablemaxverh(_, MV1Win, MV2Win, MV3Win, MV4Win) :-
  win_SetState(MV3Win, [wsf_enabled]),
  win_SetState(MV4Win, [wsf_enabled]),
  win_SetState(MV1Win, [wsf_disabled]),
  win_SetState(MV2Win, [wsf_disabled]), !.


checkUiterste(Uiterste) :-
  dt_mins_date_time(Uiterste,UitersteInDagen,_),
  date(Year,Month,Day),
  dt_date_to_offset(Year, Month, Day, Vandaag),
  %toernooiWeekJaar(_Dag, ToernooiMaandag, _, _),
  getbacktrack(Here),
  dag(ToernooiStart, _, _),
  cutbacktrack(Here),
  NulWin = cast(window, 0),
  not(checkUiterste1(NulWin, UitersteIndagen, Vandaag, ToernooiStart,b_True)), !.

metFocus(Win, Control, b_True) :-
  trap(CWin = win_GetCtlHandle(Win, Control),_,fail), % waarom dit nodig is??
  tabdlg_select(Win, idc_customtab, 1),
  win_SetFocus(CWin),
  !.
metFocus(_Win, _Control, _).

checkUiterste1(_Win, _UitersteInDagen, _Vandaag, _ToernooiMaandag, _MetFocus) :-
  comline(LineBuffer),
  upper_lower(LineBuffer, LBL),
  searchstring(LBL, "/x", _),		% in TEST
  !, fail.  
checkUiterste1(_Win, UitersteInDagen, Vandaag, _ToernooiMaandag, MetFocus) :-
  UitersteInDagen < Vandaag,
  Msg = "De uiterste inschrijfdatum ligt in het verleden,\nkies een realistische uiterste inschrijfdatum\nbij het tabblad algemeen!",
  dlg_MessageBox( "Datum conflict", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  metFocus(_Win, idd_custom, MetFocus),
  !.
checkUiterste1(_Win, _UitersteInDagen, Vandaag, ToernooiMaandag, _MetFocus) :-
  ToernooiMaandag <= Vandaag,
  Msg = "Het toernooi is verleden tijd,\npas eerst de kalender aan! (Toernooi, Kalender)",
  dlg_MessageBox( "Datum conflict", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  !.
checkUiterste1(_Win, UitersteInDagen, _Vandaag, ToernooiMaandag, MetFocus) :-
  UitersteInDagen >= ToernooiMaandag,
  Msg = "De uiterste inschrijfdatum ligt voorbij begin van het toernooi!\n\nVerbeteren?",
  1 = dlg_MessageBox( "Datum conflict", Msg, mesbox_iconexclamation, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
  metFocus(_Win, idd_custom, MetFocus),
  !.
checkUiterste1(_Win, UitersteInDagen, Vandaag, _ToernooiMaandag, MetFocus) :-
  Toekomst = UitersteInDagen - Vandaag,
  Toekomst > 180,
  format(Msg, "De uiterste inschrijfdatum ligt %u dagen van nu,\nis dat reÎel?",Toekomst),
  2 = dlg_MessageBox( "Mogelijk een datum conflict", Msg, mesbox_iconinformation, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
  metFocus(_Win, idd_custom, MetFocus),
  !.

partnerDisable(_Win).

%BEGIN_DLG InschrAlg
/**************************************************************************
	Creation and event handling for dialog: InschrAlg
**************************************************************************/

constants

%BEGIN InschrAlg, CreateParms, 20:40:39-15.10.2011, Code automatically updated!
  dlg_inschralg_ResID = idd_inschralg
  dlg_inschralg_DlgType = wd_Modal
  dlg_inschralg_Help = idh_inschrijfformulieren
%END InschrAlg, CreateParms
  date_cc_class = "date_cc_class"
  init_verder   = 63
  
predicates

  dlg_inschralg_eh : EHANDLER
  dlg_inschralg_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_inschralg_update(DIALOG_VAL_LIST)
  date_cc_class_handler1 : EHANDLER
  determ voorweekend()
  procedure volgordeO(slist, slist)
  nondeterm volgordeO1(slist, string)
  procedure checkMailAdres(string)

clauses

date_cc_class_handler1(Win,e_Create(_),0):-
  form(_, Mins, _, _, _, _, _, _, _, _, _, _, _, _),
  Mins > 0, !,
  date_cc_Init(Win, Mins, "%DD-%MD-%YL").
date_cc_class_handler1(Win,e_Create(_),0):-!,		% CALL date_cc_Init ON
  dag(Dag, _, _), !,
  D1 = Dag - 1,			% 1 dag eerder
  dt_mins_date_time(D2, D1, 0),
  date_cc_Init(Win, D2, "%DD-%MD-%YL").
date_cc_class_handler1(Win, EVENT, 0):-			% CALL date_cc_HandleEvent
  date_cc_handleEvent(Win, Event), !.			% ON ANY OTHER EVENT
date_cc_class_handler1(Win, EVENT, 0):-!,			% CALL date_cc_HandleEvent
  dlg_inschralg_eh(Win, Event).			% ON ANY OTHER EVENT

voorweekend() :-
  findall(N, dag(N, _, _), NList),
  NList = [N1, N2 | _],	% eerste twee dagen
  member(Nx, [N1, N2]),
  dt_offset_to_date(Nx, _, _, _, _, Dwx),
  Dwx >= 6, !.

volgordeO(OList, OListUit) :-
  findall(Os, volgordeO1(OList, Os), OListUit).

volgordeO1(OList, Os) :-
  wc(_, _, _, Os, _, _, _, _, _, _, _, _, _, _, _),
  member(Os, OList).

checkMailAdres(Mail) :-
  Mail=dlg_GetStr("Mailadres toernooileider","Mailadres van de toernooileider svp!\n\nZie ook toernooikenmerken.",""),
  fronttoken(Mail,_,_),
  searchchar(Mail,'@',FP1),
  FP1 > 0,
  searchchar(Mail,'.',FP2),
  FP2 > 0, !,
  vereniging1(Vnr, Club, Aanhef, Straat,Huisnr,Plaats,Postc,Tel,_,Site),
  logf('A',vereniging1(Vnr, Club, Aanhef, Straat,Huisnr,Plaats,Postc,Tel,Mail,Site),nee).
checkMailAdres("").
  
  dlg_inschralg_Create(_Parent):-
        vereniging1(_Vnr, _Club, _Aanhef, _Straat,_Huisnr,_Plaats,_Postc,_Tel,Email,_Site),
        not(fronttoken(Email, _, _)),
        checkMailAdres(NewEmail),
        NewEmail = "", !.
  dlg_inschralg_Create(_Parent):-
	get_formmap(Formmap),
	filenamepath(Stylesheet, Formmap, "t.css"),
	%format(StyleSheet, "%st.css", Formmap),
	not(trap(existfile(StyleSheet),_,true)),	% als het stylesheet niet bestaat
	trap(mkdir(Formmap), _, true),			% probeer dan directory aan te maken
	%format(TSheet, "%st.css", Formmap),
	file_str(Stylesheet, "/*  t  */"),
	fail.
  dlg_inschralg_Create(Parent):-
	class_Create(date_cc_class,date_cc_class_handler1),
  	tabdlg_Register,
%MARK InschrAlg, new variables

	dialog_CreateModal(Parent,dlg_inschralg_ResID,"",
  		[
%BEGIN InschrAlg, ControlList, 20:40:39-15.10.2011, Code automatically updated!
%END InschrAlg, ControlList
		],
		dlg_inschralg_eh,0,VALLIST,ANSWER),
	dlg_inschralg_handle_answer(ANSWER,VALLIST),
  	tabdlg_UnRegister,
	class_destroy(date_cc_class), !.

  dlg_inschralg_handle_answer(idc_ok,VALLIST):-!,
	dlg_inschralg_update(VALLIST).
  dlg_inschralg_handle_answer(idc_cancel,_):-!.  % Handle Esc and Cancel here
  dlg_inschralg_handle_answer(_,_):-
	errorexit().

  dlg_inschralg_update(_VALLIST):-
%BEGIN InschrAlg, Update controls, 20:40:39-15.10.2011, Code automatically updated!
%END InschrAlg, Update controls
	true.

%MARK InschrAlg, new events

%BEGIN InschrAlg, idc_help _CtlInfo
  dlg_inschralg_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Inschrijfformulieren"),
	!.
%END InschrAlg, idc_help _CtlInfo

%BEGIN InschrAlg, idc_cancel _CtlInfo
  dlg_inschralg_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	win_Destroy(_Win),
	!.
%END InschrAlg, idc_cancel _CtlInfo

%BEGIN InschrAlg, idc_ok _CtlInfo
  dlg_inschralg_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-	% 3.3.2012
	alleenlezen1,
	dlg_MessageBox( "Inschrijving", "Formulier wordt niet aangemaakt want het bestand is alleen lezen!", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	!.
  dlg_inschralg_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	OWin = win_GetCtlHandle(_Win, idd_onderdelen),
	NoOfItems = lbox_CountAll(OWin),
	NoOfItems = 0,
	tabdlg_select(_Win, idc_customtab, 2),
	dlg_MessageBox( "Inschrijving", "Selecteer onderdelen.", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	win_SetFocus(OWin),
	!.
  dlg_inschralg_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	profielGeg(_Win),
	fail.
  dlg_inschralg_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	VWWin = win_GetCtlHandle(_Win, idc_voorweekend),
	b_True = win_IsChecked(VWWin),
	not(voorweekend()),
	tabdlg_select(_Win, idc_customtab, 3),
	1 <> dlg_MessageBox( "Inschrijving", "Voorweekend aangevinkt maar de kalender bevat geen voorweekend.\nEerste dag moet zaterdag of zondag zijn.\nDeze boodschap negeren?", mesbox_iconexclamation, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
	win_SetFocus(VWWin),
	!.
/*  dlg_inschralg_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	IWin = win_GetCtlHandle(_Win, idc_incassonr),
	1 = win_IsChecked(IWin),
	WWin = win_GetCtlHandle(_Win, idc_adres),
	0 = win_IsChecked(WWin),
	_A = dlg_MessageBox( "Incasso", "Bij incasso moet ook het adres opgegeven worden", mesbox_iconquestion, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), !.
*/
  dlg_inschralg_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	DWin = win_GetCtlHandle(_Win, idd_custom),
	Uiterste = date_cc_GetValue(DWin),
	not(checkUiterste(Uiterste)), !.
  dlg_inschralg_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	OWin = win_GetCtlHandle(_Win, idd_onderdelen),
	OList = lbox_GetAll(OWin),
	saveCats(OList, CatL),
	DWin = win_GetCtlHandle(_Win, idd_custom),
	Uiterste = date_cc_GetValue(DWin),
  	MedWin = win_GetCtlHandle(_Win, idd_mededeling),
	Mededeling = win_GetText(MedWin),
	MOWin = win_GetCtlHandle(_Win, idd_maxond),
	MOS = win_GetText(MOWin),
	int_str_sp(MaxOnd, MOS),
	%IGWin = win_GetCtlHandle(_Win, idc_inschrijfgeld_vermelden),
	%InschrGeld = win_IsChecked(IGWin),
	InschrGeld = 0,
	GVWin = win_GetCtlHandle(_Win, idd_speeldagen),
	GVList = lbox_GetAll(GVWin),
	toernooidagenZonderVerhindering(GVList, [], GeenVerhL),
	MVSWin = win_GetCtlHandle(_Win, idd_maxverh),
	MVS = win_GetText(MVSWin),
	int_str_sp(MaxVerh, MVS),
	UKWin = win_GetCtlHandle(_Win, idc_uitkantoortijd),	% wel of niet uitkantoortijd
	UitKantoor = win_IsChecked(UKWin),
	UKMWin = win_GetCtlHandle(_Win, idc_maxuitkantoor),	% max uitkantoortijd
	lbox_GetSel(UKMWin,MWinItemList,_IndexList),
	MWinItemList = [UitKantMax|_],
	DDWin = win_GetCtlHandle(_Win, idc_dagdelen),	% periode = 0
	Periode = win_IsChecked(DDWin),
	VWWin = win_GetCtlHandle(_Win, idc_voorweekend),
	Voorweekend = win_IsChecked(VWWin),
	getbox(_Win, SpelerGeg, PartnerGeg),
	logf('A',form(CatL, Uiterste, "wasretour", Mededeling, MaxOnd, InschrGeld, GeenVerhL, MaxVerh, UitKantoor, UitkantMax, Periode, Voorweekend, SpelerGeg, PartnerGeg),nee),
        autowebP(_Regelmaat,HTML1FTP2,_Waarschuwen,_Gebruiker,_Wachtwoord, _FTPadres, _Pad, _Vasteverb, _Hoevaak, _BijSluiten),
	xML_Form(_Win, HTML1FTP2),
	win_Destroy(_Win),
	uploadForm(_Win,HTML1FTP2),
	!.
  dlg_inschralg_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0) :- !.
%END InschrAlg, idc_ok _CtlInfo

%BEGIN InschrAlg, e_User
  dlg_inschralg_eh(_Win,e_User(init_verder,_),0):-	% init verder
  	DagWin = win_GetCtlHandle(_Win, idd_speeldagen),
	lbox_SetTabStops(DagWin, [70, 120]),
	Font = font_Create(ff_Fixed, [], 10),
	win_SetFont(DagWin, Font),
	%toNrs(Tnum, _),
	form(CatL, _Uiterste, _Leeftijdcompensatie, Mededeling, MaxOnd, _InschrGeld, GeenVerh, MaxVerhS, UitKant, UitKantMax, VerhWijze, Voorweekend, SpelerGeg, PartnerGeg),
	UKWin = win_GetCtlHandle(_Win, idc_uitkantoortijd),	% wel of niet uitkantoortijd
	win_Check(UKWin, UitKant),
	UKMWin = win_GetCtlHandle(_Win, idc_maxuitkantoor),
	UKList = ["16:00","16:30","17:00","17:30","18:00","18:30","19:00","19:30","20:00"],
	lbox_Add(UKMWin, UKList),
	uitkantMaxIndex(UKList, UitKantMax, 0, UKMIndex),
	lbox_SetSel(UKMWin, UKMIndex, b_True),
	PEWin = win_GetCtlHandle(_Win, idc_periode),	% periode
	DDWin = win_GetCtlHandle(_Win, idc_dagdelen),	% dagdeel
	setVerhWijze(VerhWijze, PEWin, DDWin),
  	MedWin = win_GetCtlHandle(_Win, idd_mededeling),
	win_SetText(MedWin, Mededeling),
	RWin = win_GetCtlHandle(_Win, idd_inschrmail),
        %vereniging(_VNr, _WL, _WLTelef, _Club, _Plaats, Email, _, _, _, _Website),
        vereniging1(_Vnr, _Club, _Aanhef, _Straat,_Huisnr,_Plaats,_Postc,_Tel,Email,_Site),
	win_SetText(RWin, Email),
	loadCats(CatL, CatSL, _UCatL),
	OWin = win_GetCtlHandle(_Win, idd_onderdelen),
	lbox_Add(OWin, CatSL),
	MOWin = win_GetCtlHandle(_Win, idd_maxond),
	int_str_sp(MaxOnd, MOS),
	win_SetText(MOWin, MOS),
	VWWin = win_GetCtlHandle(_Win, idc_voorweekend),
	win_Check(VWWin, Voorweekend),
	%IGWin = win_GetCtlHandle(_Win, idc_inschrijfgeld_vermelden),
	%win_Check(IGWin, InschrGeld),
	findall(D, dagRepr(D, GeenVerh), DagL),
	lbox_Add(DagWin, DagL),
	lbox_SetSel(DagWin, 0, b_True),	
	MVSWin = win_GetCtlHandle(_Win, idd_maxverh),
	MVS1Win = win_GetCtlHandle(_Win, idd_maxverh1),
	str_int(MVS, MaxVerhS),
	win_SetText(MVSWin, MVS),
	win_SetText(MVS1Win, MVS),
	tabdlg_select(_Win, idc_customtab, 4),		% voor initialisatie?
	setbox(_Win, SpelerGeg, PartnerGeg),
	%openbaar(Openb),
	%jaNee2str(Openb, OpenbS, _),
	%OpenbWin = win_GetCtlHandle(_Win, idc_open),
	%win_SetText(OpenbWin, OpenbS),
	weggelatenCats(CatSL, Indicator),
	selectTab(_Win, Indicator),		% van loadcats
	!.
  dlg_inschralg_eh(_Win,e_User(2,b_False),0):-	% tabblad onderdelen wordt verlaten
	profielGeg(_Win),
	fail.				% verlaten gaat door
  dlg_inschralg_eh(_Win,e_User(4,b_True),0):-	% tabblad personalia wordt geselecteerd
	profielGeg(_Win), !.
  dlg_inschralg_eh(_Win,e_User(4,b_False),0):-	% tabblad personalia wordt verlaten
	profielGeg(_Win),
	fail.
  dlg_inschralg_eh(_Win,e_User(4,b_False),0):-	% tabblad personalia wordt verlaten
	IWin = win_GetCtlHandle(_Win, idc_incassonr),
	1 = win_IsChecked(IWin),
	WWin = win_GetCtlHandle(_Win, idc_adres),
	0 = win_IsChecked(WWin),
	_A = dlg_MessageBox( "Incasso", "Bij incasso moet ook het adres opgegeven worden", mesbox_iconquestion, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	win_SetFocus(WWin), !.
  dlg_inschralg_eh(_Win,e_User(3,b_True),0):-	% tabblad verhinderingen wordt geselecteerd
	UKWin = win_GetCtlHandle(_Win, idc_uitkantoortijd),	% wel of niet uitkantoortijd
	UChecked = win_IsChecked(UKWin),
	UKMWin = win_GetCtlHandle(_Win, idc_maxuitkantoor),
	UkMTWin = win_GetCtlHandle(_Win, idct_max),
	disableUitkant(UChecked, UkMWin, UkMTWin),
	PWin = win_GetCtlHandle(_Win, idc_periode),	% periode of dagdeel
	PChecked = win_IsChecked(PWin),
	MV1Win = win_GetCtlHandle(_Win, idd_maxverhinder),
	MV2Win = win_GetCtlHandle(_Win, idd_maxverh),
	MV3Win = win_GetCtlHandle(_Win, idd_maxverh1),
	MV4Win = win_GetCtlHandle(_Win, idc_inschrverh_15),
	enablemaxverh(PChecked, MV1Win, MV2Win, MV3Win, MV4Win), !.
	
%END InschrAlg, e_User

%BEGIN InschrAlg, e_Create
  dlg_inschralg_eh(_Win,e_Create(_CreationData),0):-!,
	getFontforDialog(_Win,Font),
	DynCtlList1=
		[
		 %dlg(wdef(dlg_inschrfront_DlgType,dlg_inschrfront_RCT,dlg_inschrfront_Title,u_DlgBase),dlg_inschrfront_Flags),
		 ctl(wdef(wc_Edit,rct(69,42,292,84),"",u_DlgBase),idd_mededeling,[wsf_AlignLeft,wsf_Group,wsf_TabStop,wsf_MultiLine,wsf_AutoVScroll,wsf_VScroll,wsf_WantReturn]),
		 customctl(wdef(wc_Custom,rct(83,95,135,106),"Custom",u_DlgBase),"date_cc_class",idd_custom,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_Text,rct(5,9,92,19),"Mailadres toernooileider",u_DlgBase),idd_inschrijving_emailen_naar,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(5,43,66,78),"Extra mededeling op het formulier (hou het kort!)",u_DlgBase),iddt_mededeling,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(5,96,82,106),"Uiterste inschrijfdatum:",u_DlgBase),iddt_acceptatie_tot,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(140,96,242,106),"(gebruik de pijltjestoetsen)",u_DlgBase),idct_pijltjes,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(12,123,288,144),"Zie ook Internet, beheer van ToernooiKlapper voor verdere instellingen (reglement, logo, deelnemerslijst etc.),",u_DlgBase),idct_pagina_met_reglement,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(5,21,255,31),"NB Dit adres dient alleen voor een paar statusmeldingen (is wel belangrijk)!",u_DlgBase),idct_1_emailadres,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(93,9,291,19),"",u_DlgBase),idd_inschrmail,[wsf_AlignLeft]),
		 ctl(wdef(wc_GroupBox,rct(5,114,292,147),"Diversen",u_DlgBase),idc_reglement,[])
		],
	DynCtlList2=
		[
		 ctl(wdef(wc_LBox,rct(10,14,188,152),"",u_DlgBase),idd_onderdelen,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight]),
		 ctl(wdef(wc_PushButton,rct(192,58,220,70),"&Bij",u_DlgBase),idd_bij,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(192,71,220,83),"&Af",u_DlgBase),idd_af,[wsf_Group,wsf_TabStop,wsf_Disabled]),
		 ctl(wdef(wc_PushButton,rct(192,83,220,95),"&Op",u_DlgBase),idd_op,[wsf_Group,wsf_TabStop,wsf_Disabled]),
		 ctl(wdef(wc_PushButton,rct(192,96,220,108),"&Neer",u_DlgBase),idd_neer,[wsf_Group,wsf_TabStop,wsf_Disabled]),
		 ctl(wdef(wc_Edit,rct(270,113,287,125),"",u_DlgBase),idd_maxond,[wsf_Group,wsf_TabStop,wsf_AutoHScroll,wsf_AlignLeft]),
		 %ctl(wdef(wc_CheckBox,rct(194,141,300,151),"Inschrijfgeld meevermelden",u_DlgBase),idc_inschrijfgeld_vermelden,[wsf_Group,wsf_TabStop,wsf_Auto,wsf_Disabled]),
		 %ctl(wdef(wc_PushButton,rct(243,37,291,52),"Kenmerken",u_DlgBase),idc_kenmerken,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_Text,rct(9,3,184,13),"Inschrijven staat open voor de volgende onderdelen:",u_DlgBase),iddt_men,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(193,114,270,124),"Inschrijving maximaal ",u_DlgBase),idct_inschrijven_voor_max,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(193,125,273,135),"onderdelen per persoon.",u_DlgBase),idct_onderdelen_per_persoon,[wsf_AlignLeft]),
		 %ctl(wdef(wc_Text,rct(193,12,300,39),"Onderdelen dienen een speelsterkte of leeftijdcriterium te hebben. Zie:",u_DlgBase),idct_onderdcritt,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(222,87,260,108),")-bepaal de )-volgorde",u_DlgBase),idct_bepaal_volgorde,[wsf_AlignLeft,wsf_Disabled])
		 ],
	DynCtlList3=
		[
		 ctl(wdef(wc_LBox,rct(2,19,156,141),"",u_DlgBase),idd_speeldagen,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_ExtendedSel,wsf_UseTabStops]),
		 ctl(wdef(wc_PushButton,rct(157,129,185,141),"&Ja/Nee",u_DlgBase),idd_janee,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_RadioButton,rct(164,26,274,36),"Periode (11:00-12:15 etc.)",u_DlgBase),idc_periode,[wsf_Auto,wsf_TabStop,wsf_Group]),
		 ctl(wdef(wc_Edit,rct(230,37,241,46),"",u_DlgBase),idd_maxverh,[wsf_AlignLeft,wsf_TabStop,wsf_AutoHScroll]),
		 ctl(wdef(wc_RadioButton,rct(164,50,289,60),"Dagdeel (ochtend, middag, avond)",u_DlgBase),idc_dagdelen,[wsf_Auto,wsf_TabStop]),
		 ctl(wdef(wc_Edit,rct(230,61,241,70),"",u_DlgBase),idd_maxverh1,[wsf_Group,wsf_TabStop,wsf_AutoHScroll,wsf_AlignLeft]),
		 ctl(wdef(wc_CheckBox,rct(164,77,298,87),"Uitkantoortijd (geldt alleen op werk-",u_DlgBase),idc_uitkantoortijd,[wsf_Auto,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_LBoxButton,rct(195,95,239,174),"",u_DlgBase),idc_maxuitkantoor,[wsf_Group,wsf_TabStop,wsf_VScroll]),
		 ctl(wdef(wc_CheckBox,rct(159,114,302,124),"Vragen om toernooi in voorgaande week",u_DlgBase),idc_voorweekend,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_Text,rct(3,10,104,19),"Speeldagen         toegestaan?",u_DlgBase),iddt_speeldagen_verhindering,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(55,2,104,11),"Verhinderingen",u_DlgBase),iddt_verhinderingen,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(176,97,195,107),"Max:",u_DlgBase),idct_max,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(174,36,229,46),"Maximum aantal ",u_DlgBase),idd_maxverhinder,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(174,60,229,70),"Maximum aantal ",u_DlgBase),idc_inschrverh_15,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(112,9,146,19),"Werkdag?",u_DlgBase),idct_werkdag,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(186,131,273,141),"verhinderingen toegestaan",u_DlgBase),idct_verhinderingen,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(175,87,295,97),"dagen, zie ook Toernooi, Kalender)",u_DlgBase),idct_zie_ook,[wsf_AlignLeft]),
		 ctl(wdef(wc_GroupBox,rct(161,13,302,110),"Wijze van opgeven",u_DlgBase),idc_wijze,[])
		 ],
	DynCtlList4=
		[
		 %dlg(wdef(dlg_inschrspeler_DlgType,dlg_inschrspeler_RCT,dlg_inschrspeler_Title,u_DlgBase),dlg_inschrspeler_Flags),
		 ctl(wdef(wc_CheckBox,rct(18,26,106,36),"&KNLTB-nummer",u_DlgBase),idc_knltbnummer_vereist,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_CheckBox,rct(18,38,80,48),"Geboortedatum",u_DlgBase),idc_geboortedatum,[wsf_Auto,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_CheckBox,rct(18,50,109,60),"Speelsterkte &enkelspel",u_DlgBase),idc_speelsterkte_enkelspel,[wsf_Auto,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_CheckBox,rct(18,62,113,72),"Speelsterkte &dubbelspel",u_DlgBase),idc_speelsterkte_dubbelspel,[wsf_Auto,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_CheckBox,rct(18,74,122,84),"Vragen om competitieklasse",u_DlgBase),idc_vragen_om_competitieklasse,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_CheckBox,rct(18,86,66,96),"Adres",u_DlgBase),idc_adres,[wsf_Auto,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_CheckBox,rct(18,98,60,108),"Telefoon",u_DlgBase),idc_telefoon,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_CheckBox,rct(65,98,99,108),"Mobiel",u_DlgBase),idc_mobiel,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_CheckBox,rct(102,98,132,108),"Werk",u_DlgBase),idc_werk,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_CheckBox,rct(18,110,69,120),"Emailadres",u_DlgBase),idc_emailadres,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_CheckBox,rct(86,110,128,120),"&Verplicht",u_DlgBase),idc_emailverplicht,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_CheckBox,rct(18,122,84,132),"&Incassonummer",u_DlgBase),idc_incassonr,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_CheckBox,rct(86,122,128,132),"&Verplicht",u_DlgBase),idc_incassoverplicht,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_CheckBox,rct(168,25,256,35),"&KNLTB-nummer",u_DlgBase),idc_knltbnrpartner,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_CheckBox,rct(168,37,230,47),"Geboortedatum",u_DlgBase),idc_geboortedatum1,[wsf_Auto,wsf_Group,wsf_TabStop,wsf_Enabled]),
		 ctl(wdef(wc_CheckBox,rct(168,48,259,58),"Speelsterkte &enkelspel",u_DlgBase),idc_spste,[wsf_Auto,wsf_Group,wsf_TabStop,wsf_Enabled]),
		 ctl(wdef(wc_CheckBox,rct(168,60,263,70),"Speelsterkte &dubbelspel",u_DlgBase),idc_spstd,[wsf_Auto,wsf_Group,wsf_TabStop,wsf_Enabled]),
		 ctl(wdef(wc_CheckBox,rct(168,71,216,81),"Adres",u_DlgBase),idc_adres1,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_CheckBox,rct(168,83,216,93),"Telefoon",u_DlgBase),idc_telefoon1,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_CheckBox,rct(218,83,251,93),"Mobiel",u_DlgBase),idc_mobiel1,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_CheckBox,rct(253,83,282,93),"Werk",u_DlgBase),idc_werk1,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_CheckBox,rct(168,94,219,104),"Emailadres",u_DlgBase),idc_emailadres1,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_CheckBox,rct(160,111,299,121),"Opgeven zonder partner mogelijk",u_DlgBase),idc_partner_wanted_toestaan,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_CheckBox,rct(160,122,293,132),"Partner moet eigen formulier invullen",u_DlgBase),idc_partner_moet_eigen_formulier_invullen,[wsf_Auto,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_CheckBox,rct(18,140,113,150),"Opmerkingen  toegestaan",u_DlgBase),idc_opmerkingen,[wsf_Auto,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_Text,rct(11,14,81,24),"Verlangd bij opgave:",u_DlgBase),idct_vereist_bij_opgave,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(160,14,238,24),"Verlangd bij opgave:",u_DlgBase),idc_inschrspeler_13,[wsf_AlignLeft]),
		 ctl(wdef(wc_GroupBox,rct(2,3,143,137),"Spelergegevens",u_DlgBase),idc_spelergegevens,[]),
		 ctl(wdef(wc_GroupBox,rct(152,3,294,108),"Gegevens eventuele partner",u_DlgBase),idc_partnerbox,[])
		 ],
  	SheetList =
  		[	
  		tabdlg(1,text("Algemeen"),dyn(DynCtlList1), []),
  		tabdlg(2,text("Onderdelen"),dyn(DynCtlList2), []),
  		tabdlg(3,text("Verhinderingen"),dyn(DynCtlList3), []),
  		tabdlg(4,text("Personalia"),dyn(DynCtlList4), [])
  		],
 	tabdlg_Create(_Win,idc_customtab,Font,top,58,56,SheetList,1),
	win_PostEvent(_Win,e_User(init_verder,0)),
 	!.
%END InschrAlg, e_Create
  dlg_inschralg_eh(_Win,e_Control(idd_janee,_CtrlType,_CtrlWin,_CtlInfo),0):-
	DagWin = win_GetCtlHandle(_Win, idd_speeldagen),
	lbox_GetSel(DagWin,ItemList,IndexList),
	IndexList = [Index|_],
	ItemList = [Item | _],
	parsetabs(Item, Elems),
	Elems = [Dag, JN, Wd | _], 
	%searchchar(Item,'\t',FoundPos),
	%frontstr(FoundPos,Item,StartString,RestString),
	toggle1(JN, T),
	format(Uit, "%s\t%s\t%s", Dag, T, Wd), 
	%concat(StartString, T, Uit),
	lbox_Delete(DagWin, Index),
	lbox_Add(DagWin, Index, Uit),
	lbox_SetSel(DagWin, Index, b_True), !.
  dlg_inschralg_eh(_Win,e_Control(idd_af,_CtrlType,AfWin,_CtlInfo),0):-!,
	OWin = win_GetCtlHandle(_Win, idd_onderdelen),
	lbox_GetSel(OWin,_ItemList,IndexList),
	IndexList = [Index|_],
	lbox_Delete(OWin, Index),
	NoOfItems = lbox_CountAll(OWin),
	N1 = NoOfItems - 1,
	kleinste(Index, N1, NewIndex),
	lbox_SetSel(OWin, NewIndex, b_True),
	NoOfItems = 0,
	win_SetState(AfWin, [wsf_Disabled]),
	%AfWin = win_GetCtlHandle(_Win, idd_af),
	%win_SetState(AfWin, [wsf_Enabled]),
	OpWin = win_GetCtlHandle(_Win, idd_op),
	win_SetState(OpWin, [wsf_Disabled]),
	NeerWin = win_GetCtlHandle(_Win, idd_neer),
	win_SetState(NeerWin, [wsf_Disabled]),
	VolgWin = win_GetCtlHandle(_Win, idct_bepaal_volgorde),
	win_SetState(VolgWin, [wsf_Disabled]), !.
  dlg_inschralg_eh(_Win,e_Control(idd_bij,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	OWin = win_GetCtlHandle(_Win, idd_onderdelen),
	OList = lbox_GetAll(OWin),
	findall(T, catMetCrit(_, T, OList), SList),
	dlg_onderdelen_select_Create(_Win, "Selecteer onderdelen", Slist, ItemList),
	append(OList, ItemList, TotaalL),
	volgordeO(TotaalL, TotaalO),
	lbox_clear(OWin),
	lbox_Add(OWin, TotaalO), 	!.
  dlg_inschralg_eh(_Win,e_Control(idd_neer,_CtrlType,_CtrlWin,_CtlInfo),0):-
	OWin = win_GetCtlHandle(_Win, idd_onderdelen),
	lbox_GetSel(OWin,ItemList,IndexList),
	IndexList = [Index|_],
	NoOfItems = lbox_CountAll(OWin),
	Index < NoOfItems - 1,
	I1 = Index + 1,
	ItemList = [Item|_],
	lbox_Delete(OWin, Index),
	lbox_Add(OWin, I1, Item),
	lbox_SetSel(OWin, I1, b_True),
	!.
  dlg_inschralg_eh(_Win,e_Control(idd_op,_CtrlType,_CtrlWin,_CtlInfo),0):-
	OWin = win_GetCtlHandle(_Win, idd_onderdelen),
	lbox_GetSel(OWin,ItemList,IndexList),
	IndexList = [Index|_],
	Index > 0,
	I1 = Index - 1,
	ItemList = [Item|_],
	lbox_Delete(OWin, Index),
	lbox_Add(OWin, I1, Item),
	lbox_SetSel(OWin, I1, b_True),
	!.
  dlg_inschralg_eh(_Win,e_Control(idd_onderdelen,_CtrlType,_CtrlWin,selchanged),0):-!,
	AfWin = win_GetCtlHandle(_Win, idd_af),
	win_SetState(AfWin, [wsf_Enabled]),
	OpWin = win_GetCtlHandle(_Win, idd_op),
	win_SetState(OpWin, [wsf_Enabled]),
	NeerWin = win_GetCtlHandle(_Win, idd_neer),
	win_SetState(NeerWin, [wsf_Enabled]),
	VolgWin = win_GetCtlHandle(_Win, idct_bepaal_volgorde),
	win_SetState(VolgWin, [wsf_Enabled]),
	!.
/*  dlg_inschralg_eh(_Win,e_Control(idc_kenmerken,_CtrlType,_CtrlWin,_CtlInfo),0):-
	dlg_onderdeelkenmerken_Create(_Win),
	!. */
  dlg_inschralg_eh(_Win,e_Control(idc_emailadres,_CtrlType,CtrlWin,_CtlInfo),0):-
	Check = win_IsChecked(CtrlWin),
	bool2Flag(Check, Flag),
	%UkMTWin = win_GetCtlHandle(_Win, idct_max),
	%win_SetState(UkMTWin, [Flag]),
	EMSVWin = win_GetCtlHandle(_Win, idc_emailverplicht),
	win_SetState(EMSVWin, [Flag]),
	!.
  dlg_inschralg_eh(_Win,e_Control(idc_incassonr,_CtrlType,CtrlWin,_CtlInfo),0):-
	Check = win_IsChecked(CtrlWin),
	bool2Flag(Check, Flag),
	%UkMTWin = win_GetCtlHandle(_Win, idct_max),
	%win_SetState(UkMTWin, [Flag]),
	IncVWin = win_GetCtlHandle(_Win, idc_incassoverplicht),
	win_SetState(IncVWin, [Flag]),
	!.
  dlg_inschralg_eh(_Win,e_Control(idc_uitkantoortijd,_CtrlType,CtrlWin,_CtlInfo),0):-
	Check = win_IsChecked(CtrlWin),
	bool2Flag(Check, Flag),
	UkMTWin = win_GetCtlHandle(_Win, idct_max),
	win_SetState(UkMTWin, [Flag]),
	EMSVWin = win_GetCtlHandle(_Win, idc_maxuitkantoor),
	win_SetState(EMSVWin, [Flag]),
	!.
  dlg_inschralg_eh(_Win,e_Control(idc_dagdelen,_CtrlType,_CtrlWin,_CtlInfo),0):-
	PWin = win_GetCtlHandle(_Win, idc_periode),	% periode of dagdeel
	PChecked = win_IsChecked(PWin),
	MV1Win = win_GetCtlHandle(_Win, idd_maxverhinder),
	MV2Win = win_GetCtlHandle(_Win, idd_maxverh),
	MV3Win = win_GetCtlHandle(_Win, idd_maxverh1),
	MV4Win = win_GetCtlHandle(_Win, idc_inschrverh_15),
	enablemaxverh(PChecked, MV1Win, MV2Win, MV3Win, MV4Win), !.
  dlg_inschralg_eh(_Win,e_Control(idc_periode,_CtrlType,PWin,_CtlInfo),0):-
	PChecked = win_IsChecked(PWin),
	MV1Win = win_GetCtlHandle(_Win, idd_maxverhinder),
	MV2Win = win_GetCtlHandle(_Win, idd_maxverh),
	MV3Win = win_GetCtlHandle(_Win, idd_maxverh1),
	MV4Win = win_GetCtlHandle(_Win, idc_inschrverh_15),
	enablemaxverh(PChecked, MV1Win, MV2Win, MV3Win, MV4Win), !.
  dlg_inschralg_eh(_Win,e_Control(idd_maxverh,_CtrlType,MWin,modified()),0):-
	Wsflags = win_GetState(MWin),
	member(wsf_Enabled, Wsflags),
	Text = win_GetText(MWin),
	AWin = win_GetCtlHandle(_Win, idd_maxverh1),
	win_SetText(AWin, Text), !.
  dlg_inschralg_eh(_Win,e_Control(idd_maxverh1,_CtrlType,MWin,modified()),0):-
	Wsflags = win_GetState(MWin),
	member(wsf_Enabled, Wsflags),
	Text = win_GetText(MWin),
	AWin = win_GetCtlHandle(_Win, idd_maxverh),
	win_SetText(AWin, Text), !.
  dlg_inschralg_eh(_Win,e_Control(idc_partner_wanted_toestaan,_CtrlType,_CtrlWin,_CtlInfo),0):-
	partnerDisable(_Win), !.
  dlg_inschralg_eh(_,_,_):-!,fail.

%END_DLG InschrAlg

%BEGIN_DLG Toernooiexport
/**************************************************************************
	Creation and event handling for dialog: Toernooiexport
**************************************************************************/

constants

%BEGIN Toernooiexport, CreateParms, 21:48:58-2.8.2015, Code automatically updated!
  dlg_toernooiexport_ResID = idd_toernooiexport
  dlg_toernooiexport_DlgType = wd_Modal
  dlg_toernooiexport_Help = idh_eindresultaten
%END Toernooiexport, CreateParms

%create = 22
setfoc = 23

 
predicates

  dlg_toernooiexport_eh : EHANDLER
  dlg_toernooiexport_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_toernooiexport_update(DIALOG_VAL_LIST)
  %nrFout(string)
  %tekenNietFout(char)
%procedure saveToernooiNummer(string, string, string, string)
  %saveOpenb(janee)
  saveNaam(string, slist, string Datum, string District) 
  %toerdir(janee, string)
%nondeterm   dirfiles_2(string, string, string, string)
  %checkNew(string, slist)
%toernooidatum(string)
%zetSchermop(window, janee Openbaar, janee Streng)
%procedure zetSchermOpStreng(window, janee Streng, janee Openbaar)
%determ eenNietIngevuld(window, window VWin)
%nondeterm toonC(integer Klasse, dialog_control_id)
procedure district1P(integer DistSel, string Distr)
procedure pasbestandsnaamaan(lokatie, string)
procedure toList(string, string, slist)
procedure indexS(integer,slist,string, integer)
procedure nrs2select(slist, string, integer,string,integer) - (i,o,o,o,o)
determ toernooisoort2string(toernooisoort, string)
procedure nrIf(string)
procedure algemeenAttrs(window,string TNaam,string Tnr,string Vnr,string Website,string Aanhef,string Straat,string Huisnr,string Postcode,string Plaats,string Tel,string Email)
%procedure savetoernooiK(string, string)
%procedure aOfZ(string Nr, char AofZ)
procedure alsWaarde(string,string,string,string,string)
nondeterm toernooiKMet(string Tid)
procedure maxNaamLengte(string, string)

clauses

%exportzelf(nee).
%streng(nee).

del_log(FN) :-
  resetBelangrijkeEventCounter(),
  logclose(),
  restartlog(FN), !.

maxNaamLengte(NaamI, NaamO) :-
  str_len(NaamI, Len),
  Len > 50,
  frontstr(50,NaamI,NaamO,_), !.
maxNaamLengte(Naam, Naam).

saveNaam(Naam, Nrs, Datum, District) :-	
  maxNaamLengte(Naam, NaamL),
  naam(NaamL, NrS, Datum, _Tick, District, _Res2), !.
saveNaam(Naam, Nrs, Datum, District) :-
  naam(_,_,_,Tick,_,Res2),
  maxNaamLengte(Naam, NaamL),
  logf('Z',naam(NaamL, Nrs, Datum, Tick, District, Res2),nee),
  cleanToernooiNaam(NaamL, NaamCl),
  webdir(_,Lok, _),
  pasbestandsnaamaan(Lok, NaamCl), 
  setNaam(),
  compress(replace),
  startDbRefresh(), 
  !.
saveNaam(_,_,_,_).

pasbestandsnaamaan(disk, Naam) :-
  stopdbrefresh(),
  %write("\n in pasbestandsnaamaan ",Naam),
  toernooidir(Pad, Bestand),
  %write("\ntoernooidirbestand ", Bestand),
  filenamePath(Bestand, Path, _BNaam),
  format(BNieuw, "%s.tas", Naam),
  filenamePath(BestandN, Path, BNieuw),
  %write("\ntoernooidir nieuw bestand: ", BestandN),
  assert(toernooidir(Pad, BestandN)),
  save(BestandN),	% 10.7.2014
  updatetoernooidirsave(),
  VPIconf = getvpiini(),
  save(VPIconf,vpiconf),
  filenameext(Bestand, ZonderExt, _),
  filenameext(BestandPP, ZonderExt, ".ini"),
  existfile(BestandPP),	% pas ook de parkplanner .ini aan!
  filenameext(BestandN, NZonderext, _),
  filenameext(NBestandPP, NZonderExt, ".ini"),
  renamefile(BestandPP, NBestandPP),
  renamefile(Bestand,BestandN), !.
pasbestandsnaamaan(_, _).

saveVereniging(Nr, TOrg,Aanh,Straat,HuisNrPlus,Plaats,PostCode,Telefoon,EMail,Website) :-
  vereniging1(Nr, TOrg,Aanh,Straat,HuisNrPlus,Plaats,PostCode,Telefoon,EMail,Website), !.
saveVereniging(Nr, TOrg,Aanh,Straat,HuisNrPlus,Plaats,PostCode,Telefoon,EMail,Website) :-
  logf('Z', vereniging1(Nr, TOrg,Aanh,Straat,HuisNrPlus,Plaats,PostCode,Telefoon,EMail,Website),nee), !.

district1P(DistSel, Distr) :-
  district1(DistSel, Distr, _), !.
district1P(0, _).

toList("", "", []) :- !.
toList(Nr1, "", [Nr1]) :- !.
toList(Nr1, Nr2, [Nr1,Nr2]).

toernooisoort2string(open, "open").
toernooisoort2string(jrt, "jrt").
toernooisoort2string(nrt, "nrt").
toernooisoort2string(rolstoel, "rolstoel").

nrs2select([Tid1,Tid2], Tid1, TYPE1_SELECT,Tid2,TYPE2_SELECT) :-
  toernooiK(Tid1, V1),
  toernooisoort2string(V1, V1S),
  indexS(0,["","open","jrt","nrt","rolstoel"], V1S, TYPE1_SELECT),
  toernooiK(Tid2, V2), 
  toernooisoort2string(V2, V2S),
  indexS(0,["","open","jrt","nrt","rolstoel"], V2S, TYPE2_SELECT), !.
nrs2select([Tid1], Tid1, TYPE1_SELECT,"",0) :-
  toernooiK(Tid1, V1),
  toernooisoort2string(V1, V1S),
  indexS(0,["","open","jrt","nrt","rolstoel"], V1S, TYPE1_SELECT), !.
nrs2select(_, "", 0,"",0).

indexS(_,[],_, 0) :- !.
indexS(N,[It|_],It,N) :-  !.
indexS(N,[_|Rest],It,Uit) :-
  N1 = N + 1,
  indexS(N1, Rest, It, Uit).

nrIf(Nr) :-
  toernooiK(Nr,nrt), !.
nrIf(Nr) :-
  toernooiK(Nr,jrt), !.
nrIf(Nr) :-
  toernooiK(Nr,_), !.
nrIf("").

algemeenAttrs(_Win,TNaam,Tnr,Vnr,Website,Aanhef,Straat,Huisnr,Postcode,Plaats,Tel,Email) :-
  NaamWin = win_GetCtlHandle(_Win, idce_toereig_naam1),  %naam
  TNaam = win_GetText(NaamWin),
  NrWin = win_GetCtlHandle(_Win, idce_toereig_nr),        %nr
  TNr = win_GetText(NrWin),
  VerWin = win_GetCtlHandle(_Win, idc_toernooiexport_orgver),  % vernr
  VNr = win_GetText(VerWin),
  WebsiteWin = win_GetCtlHandle(_Win, idc_websitet),  % website
  Website = win_GetText(WebsiteWin),
  AanhefWin = win_GetCtlHandle(_Win, idc_aanhef),     % aanhef
  Aanhef = win_GetText(AanhefWin),
  StraatWin = win_GetCtlHandle(_Win, idc_adresver),   % straat
  Straat = win_GetText(StraatWin),
  HuisnrWin = win_GetCtlHandle(_Win, idc_huisnr),     % Nr
  Huisnr = win_GetText(HuisnrWin),
  PCWin = win_GetCtlHandle(_Win, idc_postcode),       % PC
  Postcode = win_GetText(PCWin),
  PlaatsWin = win_GetCtlHandle(_Win, idce_plaats),    % plaats
  PLaats = win_GetText(PlaatsWin),
  TelefWin = win_GetCtlHandle(_Win, idc_toerntelin),  % tel
  Tel = win_GetText(TelefWin),
  EmailWin = win_GetCtlHandle(_Win, idc_emailin),     % email
  Email = win_GetText(EmailWin).
/*
aOfZ(Nr, 'A') :-
  findall(Nrx, toernooiK(Nrx,_), NrL),
  NrL = [Nr|_], !.
aOfZ(_Nr, 'Z').


saveToernooiK(Nr,ToernooitypeStr) :-
  fronttoken(Nr,_,_),
  fronttoken(ToernooitypeStr,_,_),
  term_str(toernooisoort, Term, ToernooitypeStr),
  toernooiK(Nr, TypeOud),
  term_str(toernooisoort, TypeOud, ToernooiTypeStrOud),
  not(ToernooitypeStr = ToernooiTypeStrOud), 
  format(Msg, "U wilt het type van toernooi nummer %s\nveranderen van %s in %s\nWeet u dat hÈÈl zeker?",Nr,ToernooiTypeStrOud,ToernooitypeStr),
  1 = dlg_MessageBox( "Toernooitype", Msg, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
  aOfZ(Nr, AofZ),
  logf(AofZ,toernooiK(Nr,Term),nee), !.
saveToernooiK(_,_).
*/
alsWaarde(Type, Naam,Mail,Tel,Y1) :-
  fronttoken(Naam,_,_), !,
  format(Y1,"%-25 = %-15s %-15s %-15s",Type,Naam,Mail,Tel).
alsWaarde(_Type, _Naam,_Mail,_Tel,"").

toernooiKMet(Nr) :-
  toernooiK(Nr, _),
  getbacktrack(Here),
  wc(_, _, _, _, _, _, _, _, _, _, _, _, _, Nr, _),
  cutbacktrack(Here).


  dlg_toernooiexport_Create(Parent,Answer):-		% hoort bij einduitslagen (xml) bij tasprnt.pro
	%closebrowser(),	
	get_toernooidir(_StartDir, _, _, Bestand),
	vereniging1(IDC_TOERNOOIEXPORT_ORGVER_VALUE, IDCE_VERENIGING_VALUE, IDC_AANHEF_VALUE, IDC_ADRESVER_VALUE,IDC_HUISNR_VALUE,IDCE_PLAATS_VALUE,IDC_POSTCODE_VALUE,IDC_TOERNTELIN_VALUE,IDC_EMAILIN_VALUE,IDC_WEBSITET_VALUE),
	naam(IDCE_TOEREIG_NAAM1_VALUE,_NrsX,_,Kode,Distr0,_),
	nrIf(Tid),
	functionarisP(wedstrijdleider, Tid, IDCE_WLNAAM, IDCE_WLEMAIL, IDCE_WLTEL, WLTEL1, IDC_TOERNOOIEXPORT_61_VALUE),
	functionarisP(bondsgedelegeerde, Tid, IDCE_BGNAAM_VALUE, IDC_BGEMAIL_VALUE, IDC_BGTEL, IDC_BGTELAVOND_VALUE, IDC_BGTELMOB_VALUE),
	functionarisP(hoofdscheidsrechter, Tid, IDC_HSCHEIDSNAAMIN_VALUE, IDC_HSCHEIDMAILIN_VALUE, IDC_HSCHEIDSTELOV_VALUE, IDC_HOOFDSTELAVOND_VALUE, IDC_HOOFDSMOBIEL_VALUE),
	vertaaldistrict(Distr0, Distr),
	makeTicket(Kode, IDCT_DIR2_TITLE),
	format(IDCT_DIR_TITLE, "Lokatie:\n%s", Bestand),
	findall(Dis, district1(_,_,Dis), IDC_DISTR_ITEMLIST),
	district1P(IDC_DISTR_SELECT, Distr),
	findall(Nr, toernooiKMet(Nr), Nrs), 
	nrs2select(Nrs, IDCE_TOEREIG_NR_VALUE, TYPE1_SELECT,IDC_NR2_VALUE,TYPE2_SELECT),
	TYPE1_ITEMLIST = ["","open","jrt","nrt","rolstoel"],
	TYPE2_ITEMLIST = ["","open","jrt","nrt","rolstoel"],
%MARK Toernooiexport, new variables

	dialog_CreateModal(Parent,dlg_toernooiexport_ResID,"",
  		[
%BEGIN Toernooiexport, ControlList, 21:48:58-2.8.2015, Code automatically updated!
		df(idce_toereig_naam1,editstr(IDCE_TOEREIG_NAAM1_VALUE,[length(50)]),nopr),
		df(idce_toereig_nr,editstr(IDCE_TOEREIG_NR_VALUE,[upper]),nopr),
		df(idc_type1,listbutton(TYPE1_ITEMLIST,TYPE1_SELECT),nopr),
		df(idc_nr2,editstr(IDC_NR2_VALUE,[]),nopr),
		df(idc_type2,listbutton(TYPE2_ITEMLIST,TYPE2_SELECT),nopr),
		df(idce_vereniging,editstr(IDCE_VERENIGING_VALUE,[]),nopr),
		df(idc_toernooiexport_orgver,editstr(IDC_TOERNOOIEXPORT_ORGVER_VALUE,[length(6),upper]),nopr),
		df(idc_aanhef,editstr(IDC_AANHEF_VALUE,[]),nopr),
		df(idc_adresver,editstr(IDC_ADRESVER_VALUE,[]),nopr),
		df(idc_huisnr,editstr(IDC_HUISNR_VALUE,[]),nopr),
		df(idce_plaats,editstr(IDCE_PLAATS_VALUE,[]),nopr),
		df(idc_postcode,editstr(IDC_POSTCODE_VALUE,[]),nopr),
		df(idc_emailin,editstr(IDC_EMAILIN_VALUE,[]),nopr),
		df(idc_toerntelin,editstr(IDC_TOERNTELIN_VALUE,[]),nopr),
		df(idc_websitet,editstr(IDC_WEBSITET_VALUE,[]),nopr),
		df(idc_distr,listbutton(IDC_DISTR_ITEMLIST,IDC_DISTR_SELECT),nopr),
		df(idce_wlnaam,editstr(IDCE_WLNAAM,[]),nopr),
		df(idce_wlemail,editstr(IDCE_WLEMAIL,[]),nopr),
		df(idce_wltelefoon,editstr(IDCE_WLTEL,[]),nopr),
		df(idc_wltel1,editstr(WLTEL1,[]),nopr),
		df(idc_wltel2,editstr(IDC_TOERNOOIEXPORT_61_VALUE,[]),nopr),
		df(idce_bgnaam,editstr(IDCE_BGNAAM_VALUE,[]),nopr),
		df(idc_bgemail,editstr(IDC_BGEMAIL_VALUE,[]),nopr),
		df(idc_bgtelefoonov,editstr(IDC_BGTEL,[]),nopr),
		df(idc_bgtelavond,editstr(IDC_BGTELAVOND_VALUE,[]),nopr),
		df(idc_bgtelmob,editstr(IDC_BGTELMOB_VALUE,[]),nopr),
		df(idc_hscheidsnaamin,editstr(IDC_HSCHEIDSNAAMIN_VALUE,[]),nopr),
		df(idc_hscheidmailin,editstr(IDC_HSCHEIDMAILIN_VALUE,[]),nopr),
		df(idc_hscheidstelov,editstr(IDC_HSCHEIDSTELOV_VALUE,[]),nopr),
		df(idc_hoofdstelavond,editstr(IDC_HOOFDSTELAVOND_VALUE,[]),nopr),
		df(idc_hoofdsmobiel,editstr(IDC_HOOFDSMOBIEL_VALUE,[]),nopr),
		df(idct_dir,statictext(IDCT_DIR_TITLE),nopr),
		df(idct_dir2,statictext(IDCT_DIR2_TITLE),nopr)
%END Toernooiexport, ControlList
		],
		dlg_toernooiexport_eh,0,VALLIST,ANSWER),
	%newdir(NewDir),
	dlg_toernooiexport_handle_answer(ANSWER,VALLIST), !.

  dlg_toernooiexport_handle_answer(idc_ok,VALLIST):-!,
	dlg_toernooiexport_update(VALLIST).
  dlg_toernooiexport_handle_answer(idc_cancel,_):-!.  % Handle Esc and Cancel here
  dlg_toernooiexport_handle_answer(_,_):-
	errorexit().

  dlg_toernooiexport_update(_VALLIST):-
%BEGIN Toernooiexport, Update controls, 21:48:58-2.8.2015, Code automatically updated!
	_IDCE_PLAATS_VALUE = dialog_VLGetstr(idce_plaats,_VALLIST),
	_IDCE_WLTEL = dialog_VLGetstr(idce_wltelefoon,_VALLIST),
	_IDCE_VERENIGING_VALUE = dialog_VLGetstr(idce_vereniging,_VALLIST),
	_IDCE_WLEMAIL = dialog_VLGetstr(idce_wlemail,_VALLIST),
	_IDCE_WLNAAM = dialog_VLGetstr(idce_wlnaam,_VALLIST),
	_IDCE_TOEREIG_NAAM1_VALUE = dialog_VLGetstr(idce_toereig_naam1,_VALLIST),
	_IDCE_TOEREIG_NR_VALUE = dialog_VLGetstr(idce_toereig_nr,_VALLIST),
	_IDC_BGTEL = dialog_VLGetstr(idc_bgtelefoonov,_VALLIST),
	_IDC_BGEMAIL_VALUE = dialog_VLGetstr(idc_bgemail,_VALLIST),
	_IDCE_BGNAAM_VALUE = dialog_VLGetstr(idce_bgnaam,_VALLIST),
	_IDC_TOERNOOIEXPORT_ORGVER_VALUE = dialog_VLGetstr(idc_toernooiexport_orgver,_VALLIST),
	_IDC_WEBSITET_VALUE = dialog_VLGetstr(idc_websitet,_VALLIST),
	dialog_VLGetListButton(idc_distr,_VALLIST,_IDC_DISTR_ITEMLIST,_IDC_DISTR_SELECT),
	_IDC_NR2_VALUE = dialog_VLGetstr(idc_nr2,_VALLIST),
	_IDC_HSCHEIDSTELOV_VALUE = dialog_VLGetstr(idc_hscheidstelov,_VALLIST),
	_IDC_HSCHEIDMAILIN_VALUE = dialog_VLGetstr(idc_hscheidmailin,_VALLIST),
	_IDC_HSCHEIDSNAAMIN_VALUE = dialog_VLGetstr(idc_hscheidsnaamin,_VALLIST),
	_IDC_HOOFDSTELAVOND_VALUE = dialog_VLGetstr(idc_hoofdstelavond,_VALLIST),
	_IDC_HOOFDSMOBIEL_VALUE = dialog_VLGetstr(idc_hoofdsmobiel,_VALLIST),
	_IDC_BGTELAVOND_VALUE = dialog_VLGetstr(idc_bgtelavond,_VALLIST),
	_IDC_BGTELMOB_VALUE = dialog_VLGetstr(idc_bgtelmob,_VALLIST),
	_WLTEL1 = dialog_VLGetstr(idc_wltel1,_VALLIST),
	_IDC_TOERNOOIEXPORT_61_VALUE = dialog_VLGetstr(idc_wltel2,_VALLIST),
	_IDC_POSTCODE_VALUE = dialog_VLGetstr(idc_postcode,_VALLIST),
	_IDC_AANHEF_VALUE = dialog_VLGetstr(idc_aanhef,_VALLIST),
	_IDC_ADRESVER_VALUE = dialog_VLGetstr(idc_adresver,_VALLIST),
	_IDC_HUISNR_VALUE = dialog_VLGetstr(idc_huisnr,_VALLIST),
	_IDC_TOERNTELIN_VALUE = dialog_VLGetstr(idc_toerntelin,_VALLIST),
	_IDC_EMAILIN_VALUE = dialog_VLGetstr(idc_emailin,_VALLIST),
	dialog_VLGetListButton(idc_type1,_VALLIST,_TYPE1_ITEMLIST,_TYPE1_SELECT),
	dialog_VLGetListButton(idc_type2,_VALLIST,_TYPE2_ITEMLIST,_TYPE2_SELECT),
%END Toernooiexport, Update controls
	toernooidatum(Datum),
	district1(_IDC_DISTR_SELECT, Distr, _),
	toList(_IDCE_TOEREIG_NR_VALUE, _IDC_NR2_VALUE, TnrS),
	saveNaam(_IDCE_TOEREIG_NAAM1_VALUE,TnrS,Datum,Distr),
	%saveToernooiK(_IDCE_TOEREIG_NR_VALUE,_TYPE1_ITEMLIST),
	%saveToernooiK(_IDC_NR2_VALUE,_TYPE2_ITEMLIST),
	saveVereniging(_IDC_TOERNOOIEXPORT_ORGVER_VALUE, _IDCE_VERENIGING_VALUE,_IDC_AANHEF_VALUE,_IDC_ADRESVER_VALUE,_IDC_HUISNR_VALUE,
	   _IDCE_PLAATS_VALUE,_IDC_POSTCODE_VALUE,_IDC_TOERNTELIN_VALUE,_IDC_EMAILIN_VALUE,_IDC_WEBSITET_VALUE), !,
	true.

%MARK Toernooiexport, new events

%BEGIN Toernooiexport, idc_maak_reglement _CtlInfo
  dlg_toernooiexport_eh(_Win,e_Control(idc_maak_reglement,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	algemeenAttrs(_Win,TNaam,_Tnr,_Vnr,Website,Aanhef,Straat,HuisnrPlus,Postcode,Plaats,Tel,Email),
	nrIf(Tid),
	concat("Toernooinaam = ",TNaam,X1),
	concat("Nummer = ",Tid,X2),
	concat("Aanhef = ",Aanhef,X3),
	format(X4, "Adres = %s %s %s %s",Straat,HuisnrPlus,Postcode,Plaats),
	concat("Tel = ",Tel,X5),
	concat("Mail = ",Email,X6),
	concat("Website = ",Website,X7),
	List = [X1,X2,X3,X4,X5,X6,X7],
	functionarisP(wedstrijdleider, Tid, IDCE_WLNAAM, IDCE_WLEMAIL, IDCE_WLTEL, _WLTEL1, _IDC_TOERNOOIEXPORT_61_VALUE),
	functionarisP(bondsgedelegeerde, Tid, IDCE_BGNAAM_VALUE, IDC_BGEMAIL_VALUE, IDC_BGTEL, _IDC_BGTELAVOND_VALUE, _IDC_BGTELMOB_VALUE),
	functionarisP(hoofdscheidsrechter, Tid, IDC_HSCHEIDSNAAMIN_VALUE, IDC_HSCHEIDMAILIN_VALUE, IDC_HSCHEIDSTELOV_VALUE, _IDC_HOOFDSTELAVOND_VALUE, _IDC_HOOFDSMOBIEL_VALUE),
	findall(Dag, dag(Dag,_,_),DagL),
	DagL = [DateB|_],
	reverse(DagL,[],DagLR),
	DagLR = [DateE|_],
	dt_dateoffset_to_str(DateB, "Begindatum: %Dn %DD-%MD-%YL", Y4),
	dt_dateoffset_to_str(DateE, "Einddatum: %Dn %DD-%MD-%YL", Y5),
	alsWaarde("Toernooileider", IDCE_WLNAAM,IDCE_WLEMAIL,IDCE_WLTEL,Y1),
	alsWaarde("Bondsgedelegeerde", IDCE_BGNAAM_VALUE,IDC_BGEMAIL_VALUE,IDC_BGTEL,Y2),
	alsWaarde("Hoofdscheidsrechter", IDC_HSCHEIDSNAAMIN_VALUE,IDC_HSCHEIDMAILIN_VALUE,IDC_HSCHEIDSTELOV_VALUE,Y3),
	L2 = [Y4,Y5,Y1,Y2,Y3],
	append(List,L2,LTot),
	list_to_string(LTot,"\r\n",String),
	format(Msg,"De volgende informatie kunt u plakken en bewerken\nin Word of LibreOffice of dergelijke als input voor\nhet reglement (pdf):\n\n%s",String),
	_Answer = dlg_MessageBox( "Reglement-informatie", Msg, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	cb_Putstring(String),
	!.
%END Toernooiexport, idc_maak_reglement _CtlInfo

%BEGIN Toernooiexport, idc_upl _CtlInfo
  dlg_toernooiexport_eh(_Win,e_Control(idc_upl,_CtrlType,_CtrlWin,_CtlInfo),0):-
	tkalrequest1("top"),
/*  	Msg = "De op dit scherm ingevulde en verbeterde gegevens worden naar de KNLTB verzonden.\n\nNB Het gaat alleen om deze gegevens voor zover de KNLTB die accepteert. Dit is dus NIET de resultaatexport! Die vindt u bij [->",
	1 = dlg_MessageBox( "Updaten algemene gegevens", Msg, mesbox_iconinformation, mesbox_buttonsokcancel, mesbox_defaultsecond, mesbox_suspendapplication ),
	providerX(Prov,_,"",_),
	format(URL,"http://%s/knltbproxy2015/DefinitieImportService.php",Prov),
	get_TempDir(_, TempFile),
	deletefile(TempFile),
	AanmeldenVars = aanmeldenvars(),
	%naam(TNaam, TNrS, DatumN, "", District, 0),
	algemeenAttrs(_Win,TNaam,Tnr,Vnr,Website,Aanhef,Straat,HuisnrPlus,Postcode,Plaats,Tel,Email),
	%not(vereniging1(VNr,_,Aanhef,Straat,HuisNrPlus,Plaats,PostCode,Tel,EMail,Website)),    --- uitgeschakeld
	%not(naam(TNaam, _, _, _, _, _)),							--- uitgeschakeld
	xml_xlate(TNaam, TNaam1),
	xml_xlate(Website, Website1),
	xml_xlate(Aanhef, Aanhef1),
	xml_xlate(Straat, Straat1),
	xml_xlate(Plaats, Plaats1),
	xml_xlate(Email, Email1),
	XML0 = "<ns:Toernooidefinitie>\n",
	format(XML1, "%s<ns:Toernooinaam>%s</ns:Toernooinaam>\n<ns:Toernooinummer>%s</ns:Toernooinummer>\n<ns:Verenigingsnummer>%s</ns:Verenigingsnummer>\n", XML0, TNaam1, TNr, VNr),
	format(XML2, "%s<ns:Website>%s</ns:Website><ns:Aanhef>%s</ns:Aanhef><ns:Straat>%s</ns:Straat><ns:Huisnummer>%s</ns:Huisnummer>\n", XML1, Website1, Aanhef1,Straat1,HuisnrPlus),
	format(XML3, "%s<ns:Postcode>%s</ns:Postcode><ns:Plaats>%s</ns:Plaats><ns:Telefoonnummer>%s</ns:Telefoonnummer><ns:Email>%s</ns:Email>\n", XML2, Postcode, Plaats1, Tel, Email1),
	concat(XML3, "</ns:Toernooidefinitie>",XML4),  % NB concat!
	_Return = downloadData(URL, TempFile, ["Toernooidefinitie", XML4, "tid",TNr | AanmeldenVars], _HttpResponsecode, 0),
	file_str(TempFile, Str),
	write(Str),
*/	!.
%END Toernooiexport, idc_upl _CtlInfo

%BEGIN Toernooiexport, idc_bij _CtlInfo
  dlg_toernooiexport_eh(_Win,e_Control(idc_bij,_CtrlType,_CtrlWin,_CtlInfo),0):-
	dlg_MessageBox( "Toernooi", "InternetID veranderen.\nHet Internet ID wordt desgewenst aangepast bij upload\nvan schema's e.d. als de toernooinaam verschilt van die op Internet.", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	!.
%END Toernooiexport, idc_bij _CtlInfo

%BEGIN Toernooiexport, e_User
  dlg_toernooiexport_eh(_Win,e_User(setfoc,HetWin),0):-
	NWin = cast(window, HetWin),
	win_SetFocus(NWin), !.
%END Toernooiexport, e_User

%BEGIN Toernooiexport, idce_wlemail losefocus
  dlg_toernooiexport_eh(_Win,e_Control(idce_wlemail,_CtrlType,_CtrlWin,losefocus),0):-
	Adres = win_GetText(_CtrlWin),
	trim_c(Adres),
	not(Adres = ""),
	emailadresFout(Adres),
	dlg_MessageBox( "E-mailadres", "Verbeteren svp!", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	win_SetFocus(_CtrlWin),
	!.
%END Toernooiexport, idce_wlemail losefocus

%BEGIN Toernooiexport, e_Update
  dlg_toernooiexport_eh(_Win,e_Update(_UpdateRct),0):-!,
	CtrlWin = win_GetCtlHandle(_Win, idct_dir),
	Font = font_Create(ff_Helvetica, [], 7),
	win_SetFont(CtrlWin, Font),
	%NteWin = win_GetCtlHandle(_Win, idct_note),
	%win_SetFont(NteWin, Font),
	%win_SetText(NteWin, "NB: voorlopig max 2 toernooinummers."),
	KWin =  win_GetCtlHandle(_Win, idct_dir2),
	win_SetFont(KWin, Font),
	!.
%END Toernooiexport, e_Update

%BEGIN Toernooiexport, idce_bgnaam losefocus
  dlg_toernooiexport_eh(_Win,e_Control(idce_bgnaam,_CtrlType,_BWin,losefocus),0):-
	Text = dialog_Getstr(_Win,idce_bgnaam),
	trim_c(Text),
	str_len(Text, Len),
	Len > 4,
	not(searchchar(Text,',',_)),
	dlg_MessageBox( "Naam", "Formaat van de naam moet zijn Vries, J de", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	!.
%END Toernooiexport, idce_bgnaam losefocus

%BEGIN Toernooiexport, idce_wlnaam losefocus
  dlg_toernooiexport_eh(_Win,e_Control(idce_wlnaam,_CtrlType,_WWin,losefocus),0):-
	Text = dialog_Getstr(_Win,idce_wlnaam),
	trim_c(Text),
	str_len(Text, Len),
	Len > 4,
	not(searchchar(Text,',',_)),
	dlg_MessageBox( "Naam", "Formaat van de naam moet zijn Vries, J de", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	!.
%END Toernooiexport, idce_wlnaam losefocus

%BEGIN Toernooiexport, idc_ok _CtlInfo
  dlg_toernooiexport_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	TnrWin = win_GetCtlHandle(_Win, idc_websitet),
	Tekst = win_GetText(TnrWin),
	searchchar(Tekst,'@',_FoundPos),
	_A = dlg_MessageBox( "Website", "is geen websiteadres!\nEventueel blanco laten.", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	HetWin = cast(integer, TnrWin),
	win_PostEvent(_Win,e_User(setfoc,HetWin)), !.
%END Toernooiexport, idc_ok _CtlInfo


%BEGIN Toernooiexport, idc_help _CtlInfo
  dlg_toernooiexport_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("eindresultaten"),
	!.
%END Toernooiexport, idc_help _CtlInfo

  dlg_toernooiexport_eh(_,_,_):-!,fail.

%END_DLG Toernooiexport

facts - mapkopie
single oudenaam(string)

predicates
%determ nieuweToernooiMap(string)
%determ checkMapNaam(window, string In, string Naam,  integer EenOfTwee)
%determ mapNaamFout(string)
procedure mapMeteenOpenen(integer, string)
%procedure kopieNaam(string In, string Uit)

clauses

oudenaam("").

toernooiKopie(Win, NewDat) :-
  webdir(_, Lok, _),
  not(Lok = inet),
  naam(Naam, TNumr, TDat, Tick, Res1, Res2),
  assert(oudenaam(Naam)),
  Toernooinaam=dlg_GetStr(Win, "Toernooikopie","Nieuwe Toernooinaam?",Naam),
  trim_c(ToernooiNaam),
  not(ToernooiNaam = ""),
  cleantoernooinaam(Toernooinaam, ToernooinaamC),
  % checkToernooinaamGeenDuplicaat(),   ................................ nog te doen
  toernooidir(_StartDir, Huidig),
  filenamepath(Huidig, Pad, _),
  assert(naam(Toernooinaam,TNumr,TDat,Tick,Res1,Res2)),
  filenameext(FNew, ToernooinaamC, ".tas"),
  filenamePath(NewDat, Pad, FNew),
  writeToJournaal(NewDat),
  trap(save(NewDat),_,dlg_Error("Fout tijdens opslaan nieuw bestand?")),
  assert(naam(Naam, TNumr, TDat, Tick, Res1, Res2)),
  format(Titelx, "Nieuw bestand: %s.\nMeteen openen?", NewDat), 
  A = dlg_MessageBox( Toernooinaam, TitelX, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
  mapMeteenOpenen(A, NewDat), !.
toernooiKopie(_, "??") :-
  oudenaam(Naam),			% zet weer terug voor de zekerheid
  naam(_, TNumr, TDat, Ticket, Res1, Res2),
  assert(naam(Naam, TNumr, TDat, Ticket, Res1, Res2)),
  fail.

mapMeteenOpenen(1, Nieuwenaam) :-
  filenamepath(NieuweNaam, Pad, _),
  set_toernooidir(Pad, NieuweNaam),
  format(Msg, "Over naar: %s", Nieuwenaam),
  writeToJournaal(Msg),
  TaskWin = vpi_GetTaskWin(),
  dlg_toernooiload_Create(TaskWin, "mapmeteenopenen",b_False, update), !.
  %updateLicentie(), !.
mapMeteenOpenen(_, _NewDirVolledig).

/*
database - browser
  single deurl(string)
*/
predicates
%  emailenmogelijk(string)
%  replacechar1(string, string)
%  striphash(string, string)

clauses
%deurl("").

browser_ctl(URL) :- 
  Null = cast(api_hwnd,0),
  api_ShellExecute(Null,"open",URL,"","",1), !.		% open in browser
  %TaskWin = vpi_GetTaskWin(),
  %tb_browsermenu_create(TaskWin),
  %toolbar_getCtlHandle(TaskWin, tb_moveable("",pnt(0,0)), Handle),
  %win_BringToTop(Handle),
  %win_SetText(Handle, "Browserhelp"),
  %assert(deurl(URL)), !.
browser_ctl(_).
/*
emailenmogelijk(Loc1) :-
  searchstring(Loc1, "ocumen", _Pos), !.	% 2.6.2010
emailenmogelijk(Loc1) :-
  searchstring(Loc1, "rating", _Pos), !.
emailenmogelijk(Loc1) :-
  %searchstring(Loc1, "webmap", _),
  searchstring(Loc1, "status", _), !.
emailenmogelijk(Loc1) :-
  %searchstring(Loc1, "webmap", _),
  searchstring(Loc1, "status", _), !.
emailenmogelijk(Loc1) :-
  searchstring(Loc1, "htm", _), !.
emailenmogelijk(Loc1) :-
  searchstring(Loc1, "rmail", _), !,
  format(Msg, "klik op de \"Verstuur!\"-knop."),
  dlg_MessageBox( "Emailen", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.
emailenmogelijk(Loc1) :-
  format(Msg, "niet mogelijk:\n%s.",Loc1),
  dlg_MessageBox( "Emailen", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.

replacechar1(String, Uit) :-
  frontchar(String, '/', Rest), !,
  replacechar1(Rest, Uit1),
  frontchar(Uit, '\\', Uit1).
replacechar1(String, Uit) :-
  concat("%20", Rest, String), !,
  replacechar1(Rest, Uit1),
  concat(" ", Uit1, Uit).
replacechar1(String, Uit) :-
  frontchar(String, Char, Rest), !,
  replacechar1(Rest, Uit1),
  frontchar(Uit, Char, Uit1).
replacechar1(_, "").

striphash(In, "") :-
  frontchar(In,FrontChar,_),
  Frontchar = '#', !.
striphash(In, Uit) :-
  frontchar(In,FrontChar,RestString),
  striphash(Reststring, R1),
  frontchar(Uit, FrontChar, R1), !.
striphash(_, "").
*/
/*
browser_emailen() :-
  deurl(URL),
  %format(Msg, "%s emailen!", URL), 
  %dlg_MessageBox( "Browserhelp", Msg, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  upper_lower(URL1, URL),
  upper_lower(URL1, URLL),
  emailenmogelijk(URLL),
  replacechar1(URL1, Loc2),	
  frontstr(8, Loc2, _, BUNaam),
  striphash(BUNaam, BUNaam1),
  dlg_note(BUNaam1),
  existfile(BUNaam1),
  get_tempdir(Dir, _),
	%file_str(BUNaam, Str),
  filenamepath(BuNaam1,_,Naame),
  filenamepath(Outfile, Dir, Naame),
  concat(OutFile, ".gz", OutFileC),
  file_compress(BUNaam1, OutFileC),
  filenamepath(Report, Dir, "uitklaar.html"),
  providerX(Prov, _PN, "", _),
  format(URLP, "http://%s/%s?stap=een&type=rapport", Prov, "webload.php"),
  Vars = aanmeldenVars(),
  _ReturnK = fileupload(URLP, OutfileC, Report, Vars),
  Session = visSession(Report, "BEHEER="),
  format(KURL, "http://%s/webload.php?%s&stap=twee&type=rmail", Prov, Session),
  Null = cast(api_hwnd,0),
  api_ShellExecute(Null,"open",KURL,"","",1),		% open in browser
  TaskWin = vpi_GetTaskWin(),
  toolbar_getCtlHandle(TaskWin, tb_moveable("",pnt(0,0)), Handle),
  toolbar_remove(Handle), !.
browser_emailen() :-
  TaskWin = vpi_GetTaskWin(),
  toolbar_getCtlHandle(TaskWin, tb_moveable("",pnt(0,0)), Handle),
  toolbar_remove(Handle), !.
browser_emailen().
*/




