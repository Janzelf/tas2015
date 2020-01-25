/*****************************************************************************

		Copyright (c) 1989 - 1998 De Lint Associates

 Project:  TASVPI
 FileName: TASPRNT.PRO
 Purpose: No description
 Written by: JdL
 Comments:
******************************************************************************/

include "tasvp1.inc"
include "tasvp1.con"
include "hlptopic.con"

domains
  PrintResolution = res(long HDPI, long VPIE, long HRES, long VRES)

constants
 rule0 =   "+..............................................................................+"
 rule3 = "\n|Schma|Resul-|Club/    | KNLTB  |M| Naam                 |Pnt|Sp|Adv| Geboorte |"
 rule4 = "\n|nr pl| taat |   Distr.|   nr   |V|                      |   |st|ies|  datum   |"


/******************************************************
 Return the first line in a string,The returned line
 contains a linefeed character.
******************************************************/

PREDICATES
  get_line(STRING Input,CHAR NewLineChar,STRING Line,STRING Rest)
CLAUSES
  get_line(Str,Char,StartStr,Rest1):-
	searchchar(Str,Char,Pos),!,
	Len = Pos - 1,
	frontstr(Len,Str,StartStr,Rest),
	frontchar(Rest,_,Rest1).
  get_line(Str,_,Str,"").

/******************************************************
 Print a single line, if the line is longer than 80
 characters, the line wraps to the next line.
******************************************************/

PREDICATES
  print_line(WINDOW,INTEGER,INTEGER,INTEGER,STRING,INTEGER,INTEGER,INTEGER,INTEGER,STRING)

CLAUSES
  print_line(_,_,_,_,RestStrNEW,LineNo,LineNo,Delta,Delta,RestStrNEW):-!.       % At end of page
  print_line(PW,SX,SY,_LH,NEWStr,_LineNo,_MaxLineNo,_Delta1,0,""):-		% Underline
	str_len(NEWStr,LEN),
	LEN > 6,
	frontstr(6,NEWStr,Kop,_RestFROMNEW),
	Kop = "+.....", !,
	win_GetTextExtent(PW, "________________________________________________________________________________", -1, Width, Height),
	SY1 = SY - Height + 1,
	SX1 = SX + Width,
	draw_Line(PW,pnt(SX, SY1), pnt(SX1, SY1)).
  print_line(PW,SX,SY,_LH,NEWStr,_Line1,_Lines,Delta1,Delta2,""):-
	draw_Text(PW,SX,SY,NEWStr),
	Delta2 = Delta1 + 1.

/******************************************************
	Print all the lines in a page
******************************************************/
PREDICATES
  print_PageLines(WINDOW,LONG StartX,LONG StartY,LONG LineW,LONG LineH,LONG NoOfLines,STRING Str,STRING Rest)
  print_PageLines(WINDOW,LONG StartX,LONG StartY,LONG LineW,LONG LineH,LONG CurrLine,LONG NoOfLines,STRING Str,STRING Rest)
  myconcat(STRING,STRING,STRING)
CLAUSES
  print_PageLines(PW,SX,SY,LW,LH,Lines,Str,Rest):-
	print_PageLines(PW,SX,SY,LW,LH,0,Lines,Str,Rest).

  print_PageLines(_,_,_,_,_,_,_,"",""):-!.                            % At end of string
  print_PageLines(_,_,_,_,_,MaxLineNo,MaxLineNo,Str,Str):-!.  % At end of page
  print_PageLines(PW,SX,SY,LW,LH,LineNo,MaxLineNo,Str,Rest):-
	get_line(Str,'\n',StartStr,RestStr2),
	print_line(PW,SX,SY,LH,StartStr,LineNo,MaxLineNo,0,Delta,RestStr1),
	LineNo1 = LineNo + Delta,
	SYNEW = SY + LH * Delta,
	myconcat(RestStr1,RestStr2,RestStr),
	print_PageLines(PW,SX,SYNEW,LW,LH,LineNo1,MaxLineNo,RestStr,Rest).

  myconcat("",S,S):-!.
  myconcat(S,"",S):-!.
  myconcat(Str1,Str2,Result):-
	concat(Str1,Str2,Result).

/******************************************************
	Print One Single page
******************************************************/

PREDICATES
  print_Page(WINDOW,STRING,INTEGER PageNo,PrintResolution,STRING Inp,STRING Rest)

CLAUSES

  print_Page(PW,Header,PageNo,res(HRes,VRes,Height,Width),Str,RestStr):-
	rapportfontgr(FontG),
	V_SCR_RES=vpi_GetAttrVal(attr_screen_vres),
	FSIZE=cast(integer,(FontG*VRes)/V_SCR_RES),
	FONT=font_Create(ff_Fixed,[],FSIZE),
	win_SetFont(PW,FONT),
	print_StartPage(PW),
	% -1/4 inch from left and from right
	LineWidth = Width - (HRes div 2),
	StartX = HRes div 4,
	StartY = VRes div 4,
	StartU = StartX,
	StartV = Height - VRes div 3,
	sleuteltekst(Id1x),
	time(Hours,Minutes,_Seconds,_Hundredths),
 	helestr_int(0,TMS, Minutes),
	date(Year,Month,Day),
	reversestr(Id1x, "", Id),
	format(Footer,"Toernooi Assistent %  blz -%d-  %/%/%  %:%",Id,PageNo,Day,Month,Year,Hours,TMS),
	draw_Text(PW,StartX,StartY,Header),     %print header
	draw_Text(PW,StartU,StartV,Footer),
	% -1/2 inch from top and from bottom
	LineHeight=cast(long,(15*VRes)/V_SCR_RES) + 1,
	Lines = cast(long,(Height-VRes) / LineHeight),
	StartY1 = cast(long,StartY + 2 * LineHeight),
	Lines1 = Lines-2,
	print_PageLines(PW,StartX,StartY1,LineWidth,LineHeight,Lines1,Str,RestStr),
	print_EndPage(PW).

/******************************************************
  Print all the pages
*******************************************************/
predicates
  print_Pages(WINDOW,STRING FName,INTEGER PageNo,PrintResolution,STRING)

CLAUSES
  print_Pages(_,_,_,_,""):-!.
  print_Pages(PW,FName,PageNo,PrintRes,Text):-
	print_Page(PW,FName,PageNo,PrintRes,Text,RestStr),
	PageNo1=PageNo+1,
	print_Pages(PW,FName,PageNo1,PrintRes,RestStr).

/******************************************************
  Print einduitslag
*******************************************************/
predicates
  print_einduitsl(string Text, string Titel)
    
clauses

print_einduitsl(Text, Titel) :-
  loadPrintConfig1(),
  PW = print_StartJob("Rapport.."),
  Height = vpi_GetAttrVal(attr_printer_height),
  Width = vpi_GetAttrVal(attr_printer_width),
  HRes = vpi_GetAttrVal(attr_printer_hres),
  VRes = vpi_GetAttrVal(attr_printer_vres),
  PageNo=1,
  print_Pages(PW,Titel,PageNo,res(HRes,VRes,Height,Width),Text),
  print_EndJob(PW).



database - rapportdlg

 rapedit(WINDOW,RCT, PNT, string Bewaar, BOOLEAN Readonly, string Titel)
determ rapporttext(string)
single tezoeken(string, integer)

CONSTANTS

  ctrlX = 24
  ctrlC = 3
  ctrlV = 22

CLAUSES
  tezoeken("", 0).

  pdcedit_handler (_Win,e_create(_),0 ) :-
	NulWin = cast(WINDOW, 0),
	rapedit(NulWin,_,_,_Bewaar,ReadOnly,_),
	rapporttext(Text),
	Font = font_Create ( ff_Fixed, [fs_Bold], 9 ),
	edit_CreateSubClass ( _Win, "*.txt", Font, ReadOnly, b_false, b_true,Text, 1, pdcedit_handler ),	% 6.3.2010 veranderd in wrap
	!.
  pdcedit_handler (_Win,e_create(_),0 ) :-
	CtrlId = win_GetCtlId(_Win ) ,
	editcontroltext(CtrlId, Text, Wrap, FontFamily, ReadOnly, Size,_Usage),
	Font = font_Create ( FontFamily, [], Size),
	edit_CreateSubClass ( _Win, "*.txt", Font, ReadOnly, b_false, Wrap,Text, 1, pdcedit_handler ),
	!.
/*  pdcedit_handler (_Win,Event,0 ) :-
	not(Event = e_Update(_)),
	not(Event = e_MouseMove(_,_,_)),
  	write("\n ", Event),
	fail.*/
  pdcedit_handler (_Win,e_Char(_,_),0 ) :- % voor kladblok
	CtrlId = win_GetCtlId(_Win ) ,
	CtrlID = idk_edit,
  	kladbloktimeron(),
	fail.
  pdcedit_handler (_Win,e_Char(_,_),0 ) :- % voor de modellen
	CtrlId = win_GetCtlId(_Win ) ,
	CtrlID = idc_custom,
  	setEmailWinTimer(),
	fail.
  pdcedit_handler (_Win,e_Char('\27',_),0 ) :-
	ParentWin=win_GetParent(_Win), 
	win_Destroy(ParentWin), !.
  pdcedit_handler (_Win,e_Char(ctrlX,_),0 ) :-
	edit_Cut(_Win),!.
  pdcedit_handler (_Win,e_Char(ctrlC,_),0 ) :-
	edit_Copy(_Win),!.
  pdcedit_handler (_Win,e_Char(ctrlV,_),0 ) :-
	edit_Paste(_Win),!.
  pdcedit_handler ( W, e_MouseDown ( _, _, _ ), 0 ) :- !, 
	win_SetFocus ( W ), fail.

%BEGIN_DLG Rapport
/**************************************************************************
	Creation and event handling for dialog: Rapport
**************************************************************************/

CONSTANTS

%BEGIN Rapport, CreateParms, 12:17:50-23.10.2011, Code automatically updated!
  dlg_rapport_DlgType = wd_Modeless
  dlg_rapport_Title = "Rapport"
  dlg_rapport_RCT = rct(40,32,337,303)
  dlg_rapport_Flags = [wsf_Close,wsf_TitleBar,wsf_SizeBorder]
  dlg_rapport_Help = idh_rapport
%END Rapport, CreateParms

PREDICATES

  dlg_rapport_eh : EHANDLER
  indexF(integer, slist, integer, integer)
  procedure ext2header(string Ext, string HtmlHeader, string HtmlTrailer, string Titel)
  procedure displayInBowser(string Ext, string ToName, janee MetMsg)
  zoeken(window, string)

CLAUSES

indexF(I, [H|_], N, N) :-
  str_int(H, I), !.
indexF(I, [_|T], In, Uit) :-
  In1 = In + 1, !,
  indexF(I, T, In1, Uit).
indexF(_, _, N, N) :- !.

ext2header(Ext, HtmlHeader, "</pre></body></html>", Titel) :-
  upper_lower(Ext, ".html"),
  format(HtmlHeader, "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\">\n<html>\n<head>\n<title>%s\n</title>\n<body>\n<pre>",Titel),
  !.
ext2header(_, "", "", _).

displayInBowser(Ext, ToName, nee) :-
  upper_lower(Ext, ".html"),
  1 = dlg_MessageBox( "Rapport", "Openen in IE?", mesbox_iconquestion, mesbox_buttonsokcancel, mesbox_defaultfirst, mesbox_suspendapplication ), !,
  Null = cast(api_hwnd,0),
  api_ShellExecute(Null,"open",ToName,"","",1),		% open in browser
  !.
displayInBowser(Ext, _ToName, nee) :-
  upper_lower(Ext, ".html"), !.
displayInBowser(_Ext, _ToName, ja).

close_rapporten() :-
  rapedit(Win, _, _, _, _, _),
  win_Destroy(Win),
  fail.
close_rapporten().
  
zoeken(EditWin, Rest) :-
  tezoeken(Str, Vanaf),
  upper_lower(Rest, Restlow),
  searchstring(Restlow,Str,FP),
  str_len(Str, Len),
  Fp1 = FP + Vanaf,
  Fp2 = Fp1 + Len,
  edit_setSelection(EditWin, Fp1, Fp2),
  assert(tezoeken(Str,Fp2)), !.
zoeken(_EditWin, _) :-
  tezoeken(_, Pos),
  Pos <> 0, !,
  dlg_MessageBox( "Zoeken", "Einde berereikt!\nBegin weer vooraan.", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  tezoeken(Str, _),
  assert(tezoeken(Str, 0)).
zoeken(_EditWin, _) :-
  tezoeken(_, 0),
  dlg_MessageBox( "Zoeken", "Niet gevonden.", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ).

  dlg_rapport_Create(Parent,Title,Bewaar,ReadOnly):-
	NulWin = cast(WINDOW,0),
	assert(rapedit(NulWin,rct(0,0,0,0), pnt(0,0), Bewaar, ReadOnly, Title)),
	Parm = cast(long, Title),
	get_TempDir(_, TempFile),
	file_str(TempFile, Text1),
	retractall(rapporttext(_)),
	assert(rapporttext(Text1)),
	win_CreateDynDialog(Parent,
		[
%BEGIN Rapport, WinDefList, 12:17:50-23.10.2011, Code automatically updated!
		 dlg(wdef(dlg_rapport_DlgType,dlg_rapport_RCT,dlg_rapport_Title,u_DlgBase),dlg_rapport_Flags),
		 ctl(wdef(wc_PushButton,rct(3,246,30,256),"&Zoek:",u_DlgBase),idc_zoek,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_Edit,rct(30,246,93,257),"",u_DlgBase),idc_zoektekst,[wsf_Group,wsf_TabStop,wsf_AutoHScroll,wsf_AlignLeft]),
		 ctl(wdef(wc_PushButton,rct(257,246,295,268),"&Afdrukken",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop]),
		 customctl(wdef(wc_Custom,rct(3,1,296,245),"Edit control",u_DlgBase),"pdcedit",idc_edit_control,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_HScroll]),
		 ctl(wdef(wc_LBoxButton,rct(161,245,187,290),"",u_DlgBase),idct_font,[wsf_Group,wsf_TabStop,wsf_VScroll]),
		 ctl(wdef(wc_PushButton,rct(188,257,220,269),"&Cancel",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(188,245,220,257),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(223,246,255,268),"&Bewaar",u_DlgBase),idc_bewaar,[wsf_Default,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_Text,rct(2,258,188,270),"Rapporttekst",u_DlgBase),idct_rapporttekst,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(111,246,159,256),"Lettergrootte:",u_DlgBase),idct_lettergrootte,[wsf_AlignLeft])
%END Rapport, WinDefList
		],dlg_rapport_eh,Parm).

%BEGIN Rapport, idc_ok _CtlInfo
  dlg_rapport_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-!,
	FontWin = win_GetCtlHandle(_Win, idct_font),
	lbox_GetSel(FontWin,FontSelList,_IndexList),
	FontSelList = [FontS|_],
	str_int(FontS, FontI),
	assert(rapportfontgr(FontI)),
	EditWin = win_GetCtlHandle(_Win, idc_edit_control),
	Text = edit_GetText(EditWin),
	naam(Toernooinaam, TN, _ToernooiDatum, _, _, _),
	term_str(slist, TN, TNrs),
	findall(N, dag(N,_,_),NL),
	NL = [Eerstedag|_],
	reverse(NL, [], NLR),
	NLR = [Laatstedag|_],
	dt_dateoffset_to_str(EersteDag, "%Dd.%Md", VanS),
	dt_dateoffset_to_str(LaatsteDag, "%Dd.%Md.%YL", TotS),
	format(Titel,"% Nr(s): % Speeldata: %s-%s.",ToernooiNaam, TNrs, VanS, TotS),
	print_einduitsl(Text, Titel),
	win_Destroy(_Win),
	!.
%END Rapport, idc_ok _CtlInfo
%MARK Rapport, new events

%BEGIN Rapport, idc_zoektekst modified
  dlg_rapport_eh(_Win,e_Control(idc_zoektekst,_CtrlType,_CtrlWin,modified),0):-
	STWin = win_GetCtlHandle(_Win, idc_zoektekst),
	Str = win_GetText(STWin),
	not(Str = ""),
	upper_lower(Str, Strlow),
	assert(tezoeken(Strlow, 0)), 
	%write("zoektekst=", Str, "\n"),
	EditWin = win_GetCtlHandle(_Win, idc_edit_control),
	Text = edit_GetText(EditWin),
	zoeken(EditWin, Text), !.
%END Rapport, idc_zoektekst modified

%BEGIN Rapport, idc_zoek _CtlInfo
  dlg_rapport_eh(_Win,e_Control(idc_zoek,_CtrlType,_CtrlWin,_CtlInfo),0):-
	EditWin = win_GetCtlHandle(_Win, idc_edit_control),
	Text = edit_GetText(EditWin),
	tezoeken(_Str, Vanaf),
	frontstr(Vanaf,Text,_,Rest),
	zoeken(EditWin, Rest), !.
%END Rapport, idc_zoek _CtlInfo

%BEGIN Rapport, idc_help _CtlInfo
  dlg_rapport_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Rapport"),
	!.
%END Rapport, idc_help _CtlInfo

%BEGIN Rapport, idc_bewaar _CtlInfo
  dlg_rapport_eh(_Win,e_Control(idc_bewaar,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	rapedit(_Win,_,_,Bewaar,_ReadOnly, TitelX),
	%get_ToernooiDir(Dir, _, _),		% ergens bewaren
	getFolder(DocDir,_,_,_),
	ToName = dlg_GetFileName(Bewaar,["tekst","*.txt","webpagina","*.html"],"Bewaar als . .",[dlgfn_Save],DocDir,_OutListFiles),
	filenamepath(ToName, _Path, Name),
	filenameext(Name,_Main,Ext),
	Name <> fctconst,
	Name <> logconst,
	naam(Toernooinaam, TN, _ToernooiDatum, _, _, _),
	term_str(slist, TN, TNSS),
	findall(N, dag(N,_,_),NL),
	NL = [Eerstedag|_],
	reverse(NL, [], NLR),
	NLR = [Laatstedag|_],
	dt_dateoffset_to_str(EersteDag, "%Dd.%Md", VanS),
	dt_dateoffset_to_str(LaatsteDag, "%Dd.%Md.%YL", TotS),
	format(Titel,"% Nr(s): % Speeldata: %s-%s.\n",ToernooiNaam, TNSS, VanS,TotS),
	get_TempDir(_, TempFile),
	file_str(Tempfile, RapStr),
	ext2header(Ext, HtmlHeader, HtmlTrailer,TitelX),
	%concat(Titel,RapStr,RapFull),
	format(RapFull, "%s%s%s%s", HtmlHeader, Titel, RapStr, HtmlTrailer),
	file_str(ToName,RapFull),
	%copyfile(tmpconst,ToName),
	format(StringX, "Rapport bewaard in het bestand: %s.", ToName),
	displayInBowser(Ext, ToName, MetMsg),
	writeToJournaal(StringX, MetMsg),
	win_Destroy(_Win),
	!.
%END Rapport, idc_bewaar _CtlInfo

%BEGIN Rapport, e_Destroy
  dlg_rapport_eh(_Win,e_Destroy,0):-!,
        updateVensterpositie(rapportvenster, _Win),
	retract(rapedit(_Win,_,_,_,_,_)),
	!.
%END Rapport, e_Destroy

%BEGIN Rapport, e_Create
  dlg_rapport_eh(_Win,e_Create(_CreationData),0):-!,
	NulWin = cast(WINDOW, 0),
	retract(rapedit(NulWin,_,_,Bewaar,ReadOnly,Titel)),
	Title = cast(string, _CreationData),
	win_SetText(_Win, Title),
	TxtWin = win_GetCtlHandle(_Win, idct_rapporttekst),
	win_SetText(TxtWin, Title),
	%tezoeken(Str, _),
	assert(tezoeken("", 0)),
	STWin = win_GetCtlHandle(_Win, idc_zoektekst),
	win_SetText(STWin, ""),
	ClientRCT = win_GetClientRect(_Win),
	EditWin = win_GetCtlHandle(_Win, idc_edit_control),
	Rct = win_GetOuterRect(EditWin),
	ClientRCT = rct(_,_,XCl,YCl),
	Rct = rct(X0,Y0,XCt,YCt),
	X1 = XCl - XCt,
	Y1 = YCl - YCt,
	assert(rapedit(_Win,rct(X0, Y0, X1, Y1),pnt(XCl,YCl),Bewaar,ReadOnly,Titel)),
	rapportfontgr(FontGr),
	FontWin = win_GetCtlHandle(_Win, idct_font),
	FontList = ["8","9","10","11","12"],
	indexF(FontGr, FontList, 0, Index),
	lbox_Add(FontWin, FontList),
	lbox_SetSel(FontWin, Index, b_True),
	vensterpositie(rapportvenster, _Win),
	win_SetFocus(STWin),
	!.
%END Rapport, e_Create

  dlg_rapport_eh(_Win,e_Size(_N, _W),0):-!,
	CtrlWin = win_GetCtlHandle(_Win, idc_edit_control),
	retract(rapedit(_Win,rct(X0,Y0,X1,Y1),pnt(XC1,YC1),Bewaar,ReadOnly,Titel)),
	X11 = _N - X1,
	Y11 = _W - Y1,
	NewClient = rct(X0, Y0, X11, Y11),
	win_Move(CtrlWin, NewClient),
	Xd = _N - XC1,
	Yd = _W - YC1,
	PB1Win = win_GetCtlHandle(_Win, idc_ok),
	PB1Rct = win_GetOuterRect(PB1Win),
	PB1RctNew = rect_Offset(PB1Rct, Xd, Yd),
	win_Move(PB1Win, PB1RctNew),
	PB2Win = win_GetCtlHandle(_Win, idc_cancel),
	PB2Rct = win_GetOuterRect(PB2Win),
	PB2RctNew = rect_Offset(PB2Rct, Xd, Yd),
	win_Move(PB2Win, PB2RctNew),
	PB3Win = win_GetCtlHandle(_Win, idc_help),
	PB3Rct = win_GetOuterRect(PB3Win),
	PB3RctNew = rect_Offset(PB3Rct, Xd, Yd),
	win_Move(PB3Win, PB3RctNew),
	PB4Win = win_GetCtlHandle(_Win, idc_bewaar),
	PB4Rct = win_GetOuterRect(PB4Win),
	PB4RctNew = rect_Offset(PB4Rct, Xd, Yd),
	win_Move(PB4Win, PB4RctNew),
	TxtWin = win_GetCtlHandle(_Win, idct_rapporttekst),
	TxtRct = win_GetOuterRect(TxtWin),
	TxtRctNew = rect_Offset(TxtRct, Xd, Yd),
	TxTRctNew = rct(_,Tr,Rr,Br),
	TxtRctNw1 = rct(2,Tr,Rr,Br),
	win_Move(TxtWin, TxtRctNw1),
	FontWin = win_GetCtlHandle(_Win, idct_font),
	FontRct = win_GetOuterRect(FontWin),
	FontRctNew = rect_Offset(FontRct, Xd, Yd),
	win_Move(FontWin, FontRctNew),
	LtrWin = win_GetCtlHandle(_Win, idct_lettergrootte),
	LtrRct = win_GetOuterRect(LtrWin),
	LtrRctNew = rect_Offset(LtrRct, Xd, Yd),
	win_Move(LtrWin, LtrRctNew),
	ZkWin = win_GetCtlHandle(_Win, idc_zoek),
	ZkRct = win_GetOuterRect(ZkWin),
	ZkRctNew = rect_Offset(ZkRct, Xd, Yd),
	win_Move(ZkWin, ZkRctNew),
	ZkTWin = win_GetCtlHandle(_Win, idc_zoektekst),
	ZkTRct = win_GetOuterRect(ZkTWin),
	ZkTRctNew = rect_Offset(ZkTRct, Xd, Yd),
	win_Move(ZkTWin, ZkTRctNew),
	win_Invalidate(_Win),
	assert(rapedit(_Win,rct(X0,Y0,X1,Y1),pnt(_N,_W),Bewaar,ReadOnly,Titel)),
	!.

%BEGIN Rapport, idc_cancel _CtlInfo
  dlg_rapport_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	win_Destroy(_Win),
	!.
%END Rapport, idc_cancel _CtlInfo



  dlg_rapport_eh(_,_,_):-!,fail.

%END_DLG Rapport

/************************************************************************
	printen van einduitslagen
**************************************************************************/

database - lokaal
  repCat(wedscat, string XMLNaam, string Link)			% voor rapportagedoeleinden
  sterkteStat(integer Sterkte, integer Aantal)
  leeftStat(integer Leeftijd, integer Aantal)
  deelnemer(tgnst)
  team(tgnst, integer Van, integer Tot, string Id, integer Plaatsing, string Kwalificatiecode)
  gehadVoorRanking(tgnst)
  eenKeerFout
  ppRonde(wedscat, integer Ronde)
  repWed(integer, wedscat)

database - export	% 8.6.2010
  rondeL(wedscat, string Type)
  gespeeldePartij(integer, wedscat)
  sets(integer, wedscat, string)
  alGeweestK(wedscaT)
  foutKNLTBnr(string)

database - enkelen
  single  ka(integer)
  single  invalidLidnr(boolean)
  %single  validExport(boolean)
  single nietgespeeldAantal(integer)
  single preset(integer)

PREDICATES

  uitslagen1(integer, wedscat)
  ingevuld(integer)
  verliezer(integer, wedscat, tgnst, tgnst, tgnst)
  rep_pers(integer Type, string, string, tgnst, wedstyp, integer CardS, wedscat)
  rep_pers1(integer, string,  string, integer, wedstyp, integer CardS, string, integer PuntenI, real Punten, wedscat, string PlaatsBeide)
  sex_str(sexe, char)
procedure  rep_uitsl(wedscat, integer, wedstyp, integer Type)
  plaatsing(wedscat, tgnst, string)
  get_sterkte(tgnst, string)
  sterkste(string, string, string)
  find_schema(tgnst, wedscat, integer, string)
procedure uitslagen(integer Type, slist Onderdelen) - (i,i)  % 1 = deelnemers 2 = winnaars 3 = 1 t/m 4 4 = onregelmatigheden
uitslagtype(integer, integer, string)
nondeterm  cat_uitslag(string, integer Mode) - (o, i)	% 5 = XML
  cat_uitslag1(wedscat,string,string)
eigenSterkte(string St, integer Kolom, integer PuntenI, real Punten)
totaan(integer, integer, integer)
procedure print_poel_uitsl(integer Keus, wedscat)
procedure pri(integer Keus, slist Ond)
procedure eindrap(WINDOW, janee, integer Type, slist UitList, string Titel) - (i,i,i,i,i)
procedure eindrapXML(catl UitList, string Titel, string Tid) - (i,i,i)
procedure xml_1(catl)
procedure xml_teams(wedscat) - (i)
procedure  xml_scan_eindstand(tgnst, wedscat, integer Van, integer Tot, string KwalCode)
nondeterm  xml_eindstand(integer Vanaf, integer TotMet)
  xml_ronde1(wedscat Cat, integerlist Van) % - (i,i)
  xml_ronde(wedscat Cat, string Verliezersronde, integer Diepte)
procedure xml_uitslagStr(string, uitslstat, string)
  xml_nextInt(string UitslagS, string integer, string Rest)
nondeterm xml_reserved_char(char,  string)
procedure xml_xlate1(string In, integer Index, integer Lengte, string Hulp, string Uit) 
%procedure xml_error(integer)
procedure xml_lidnrcheck(string, string, string) - (i,o,i)
procedure copyCats(string Tid, catl)
procedure int_strD(integer, string)
procedure updLeeftStat(integer Leeft)
procedure updStStat(integer Sterkte)
procedure statistiekenL(boolean Aanwezig, wedscat)
procedure statistiekenS(boolean Aanwezig, wedscat)
procedure xml_uitgevoerde_onderdelen(catl)
%procedure html_speelsterkteleeft(wedscat Cat, string Speelsterkte, string LeeftV, string LeeftT)
%procedure html_speelsterkte(string, string)
%procedure uitList2catList(slist, catl, catl)
%procedure reden(wedscat, string)
nondeterm deelnemerP(catl, tgnst)	% deelnemers aan een stelsel
nondeterm walkoverND(catl, integer WedsNo)% tel walkovers
nondeterm gestaaktND(catl, integer WedsNo)% tel gestaakte wedstrijden
%nondeterm walkovernrND(catl, integer WedsNo)% tel gestaakte wedstrijden
nondeterm nietgoedgekeurdeOpgaven(catl, integer)
nondeterm allenietgoedgekeurdeOpgaven(catl, integer)
nondeterm nietgekeurdeOpgaven1(catl, integer)
nondeterm allenietgekeurdeOpgaven(catl, integer)
nondeterm tgnst2nr(tgnst, integer)
procedure markCritS(wedscat, integer, string)
procedure markCritL(wedscat, integer, string)
%nondeterm spelersMetFouteSterkte(wedscat Cat, tgl, integer SpNo)
%nondeterm spelersMetFouteLeeftijd(wedscat Cat, tgl, integer SpNo)
%procedure sterkteEnkelDubbel(tgnst, string SterkteE, string SterkteD, string SterkteE)
%determ    leeftijdFout(integer, integer, integer)
nondeterm personenP(tgl, integer Dlnr)
nondeterm resultaatPlus(string Tekst, integer Kol, integer Card) - (o,o,o)
nondeterm spelersBuitenSpel(catl, integer Omtetellen)
procedure voorrondeEindstand(wedscat Voorronde)
nondeterm wedstrijdT(tgnst T, integer No, wedscat Cat)
determ nietgespeeld1(integer WNr, wedscat Cat, tgnst, tgnst)
nondeterm nietgespeeld(catl CatL, integer WNr)
procedure writeRanking()
nondeterm naamVolgens(tgnst IPVNaam, integerlist) - (o,i)
nondeterm teamMet(integer)
procedure xml_minVoornaam(string, string)
nondeterm scheidingsteken(char)
procedure xml_voorvoegsel1(string Voornaam, string Rest1, string In, string VoorvoegselU) - (i,o,i,o)
determ opsturen(string XML, string Tid)
determ totAan(integer, integer)
determ rep_pers_poule(wedscat Cat, tgnst , integer Rank)
determ xml_uitgevoerd_onderdeel(wedscat Cat)
procedure spelerXMLuploadIf(string Stap)
determ onregelmatig(integer, wedscat, string RapTekst)
determ rep_PersH(tgnst)
procedure xml_onderdeelL(wedscat Cat)
procedure xml_teamL(wedscat)
procedure xml_rondeL(wedscat)
procedure journaalniettegroot(string, integer, string, string)
determ  xml_tgnst2(tgnst) - (i)
procedure optionalTag(string Tag, integer Leeftijd)
procedure toernooiid2type(toernooisoort, string)
procedure toernooinummerVertaal(string, string)
procedure update_team_poule(integer Rank, tgnst Tgnst, integer Plaatsing, string TeamId)
nondeterm xml_voorronde_eindstand(wedscat Cat, string Kwal, tgnst T)
determ update_team_voorronde(wedscat Cat, tgnst Tgnst, string Kwal) - (i,i,i)
determ isTeamEr(tgnst, wedscat)
procedure writeinfo(integer Diepte, string)
procedure checkonderdelen(string)
nondeterm onderdeeltechecken(string, string, wedscat)
procedure repair(wedscat, catl)
%determ topCatOrSchemasoort(wedscat CatNo)
procedure convertTnrList(slist, slist)
procedure ifMsg(slist, string, string)
procedure komtvoorbij(wedscat, string)
determ    xml_uitslag(integer No, wedscat Cat, tgnst, tgnst, string Uitslag, string Uituitslag, string Bijzonderheid) -   (i,i,i,i,i,o,o)
%determ upperMatch(integer)
procedure foutenWaarsch(tgnst, teamstatus)
determ    alRepWed(integer No, wedscat Cat)
nondeterm voorrondeAKNLTB(wedscat Cat, wedscat VoorrondeX)
nondeterm voorrondeKNLTB(wedscat, wedscat)
procedure stelselKNLTB(wedscat, catl) 
procedure vpiconfbestaat(string, string)
determ    in_echteweds(wedscat, tgnst)
nondeterm isWees(string)
procedure wezenRapport()
determ    isuitslagcat(wedscat)
clauses

ka(0).
preset(0).
invalidLidnr(b_False).
nietgespeeldaantal(0).

markCritS(Cat, 0, Out) :-
  wc(Cat, _, _, _, _, _, _, SpeelSterkte, _, _, _, _, _, _, _),
  %wcrit(Cat, Speelsterkte, _, _, _Reserve),
  trim_c(Speelsterkte),
  not(Speelsterkte = ""),
  format(Out, "<SPAN class=rood><B>?</B></SPAN>"), !.
markCritS(Cat, Sterkte, Out) :-
  wc(Cat, _, _, _, _, _, _, SpeelSterkte, _, _, _, _, _, _, _),
  %wcrit(Cat, Speelsterkte, _, _, _Reserve),
  str_int(Speelsterkte, Grens),
  Sterkte < Grens,
  format(Out, "<SPAN class=rood><B>%u</B></SPAN>", Sterkte), !.
markCritS(_Cat, Sterkte, Out) :-
  format(Out, "%u", Sterkte).
  
markCritL(Cat, Leeftijd, Out) :-
  wc(Cat, _, _, _, _, _, _, _SpeelSterkte, leeftijd(_,Londer), leeftijd(_,Lboven), _, _, _, _, _),
  %wcrit(Cat, _, Londer, Lboven, _),
  Lboven >= Londer,
  Lboven > 0, 
  Leeftijd >= Londer,
  Leeftijd <= Lboven, !,
  format(Out, "%u", Leeftijd).		% binnen grenzen
markCritL(Cat, 0, Out) :-
  wc(Cat, _, _, _, _, _, _, _SpeelSterkte, leeftijd(_,Londer), leeftijd(_,Lboven), _, _, _, _, _),
  %wcrit(Cat, _, Londer, Lboven, _),
  Lboven >= Londer,
  Lboven > 0, !,			% buiten grenzen
  format(Out, "<SPAN class=rood><B>?</B></SPAN>").
markCritL(Cat, Leeftijd, Out) :-
  wc(Cat, _, _, _, _, _, _, _SpeelSterkte, leeftijd(_,Londer), leeftijd(_,Lboven), _, _, _, _, _),
  %wcrit(Cat, _, Londer, Lboven, _),
  Lboven >= Londer,
  Lboven > 0, !,			% buiten grenzen
  format(Out, "<SPAN class=rood><B>%u</B></SPAN>", Leeftijd).
markCritL(_Cat, Leeftijd, Out) :-
  format(Out, "%u", Leeftijd).

cat_uitslag(CatN1, id_Toernooi_einduitslagen_onregelm) :-
  wc(Cat, _, _, CatN, _, _, _, _, _, _, _, _, _, _, _),
  %wc(Cat,CatN,_,_,_,_,_), 
  cat_uitslag1(Cat, CatN, CatN1).
cat_uitslag(CatN1, Typ) :-
  Typ <> id_Toernooi_einduitslagen_KNLTB,
  Typ <> id_Toernooi_einduitslagen_onregelm,
  wc(Cat, _, _, CatN, _, _, _, _, _, _, _, _, _, _, _),
  %wc(Cat,CatN,_,_,_,_,_), 
  isUitslagCat(Cat),
  cat_uitslag1(Cat, CatN, CatN1).

isUitslagCat(Cat) :-
  pos(Cat, _, _, _WTyp),
  not(isOndercat(Cat)), !.
isUitslagCat(Cat) :-
  pos(Cat,_,_,_),			% zelf igedeeld
  kopl(Cat, _, CatU, _),
  not(pos(CatU,_,_,_)), !.		% 7.8.2016 en een lege er boven

cat_uitslag1(Cat, CatN, CatN1) :-
  concat("p ", CatN, CatN1), 
  pos(Cat,_,_,poel), !.				% wèl ingedeeld en poule
cat_uitslag1(_Cat, CatN, CatN1) :-
  %pos(Cat,_,_,_),				% wèl ingedeeld en afval
  concat("  ", CatN, CatN1), !.

				% GLOBAL......
ronde(0,_,winnaar)     :- !.
ronde(1,_,finale)      :- !.
ronde(Wnum,_,halvefin) :- Wnum < 4 , !.
ronde(Wnum,_,kwartfin) :- Wnum < 8 , !.
ronde(Wnum,Card,r(1))  :- Wnum >= Card, !.
ronde(Wnum,Card,r(N))  :- bitleft(Wnum,1,W1),
                          ronde(W1,Card,RX), 
                          RX = r(N1),
                          N=N1+1.

uitslagtype(id_Toernooi_einduitslagen_deelnemers, 1, "Einduitslagen - Alle deelnemers").
uitslagtype(id_Toernooi_einduitslagen_nrs_14,     3, "Einduitslagen - Nrs 1 t/m 4").
uitslagtype(id_Toernooi_einduitslagen_onregelm,   4, "Onregelmatige toernooiuitslagen").
uitslagtype(id_Toernooi_einduitslagen_1_en_2,     6, "Eindrangorde - Nrs 1 en 2").

topCatOrSchemasoort(CatNo) :-
  not(isOnderCat(CatNo)), !.
topCatOrSchemasoort(CatNo) :-
  %SchSrt >= "A",
  %SchSrt <= "L",
  kopl(CatNo, algemeen, CatNo0,algemeen),
  %isMasterCat(CatNo0),
  wc(CatNo, Oid, _, _Nam, _, _, _, _, _, _, _, _, _, Tid, _),
  wc(CatNo0, Oid0, _, _Naam, _, _, _, _, _, _, _, _, _, Tid, _),
  fronttoken(Oid0, _, _),
  Oid <> Oid0,  !.
  %  _Answer = dlg_MessageBox( "Test", _Nam, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ).

einduitslagen(Win,TypeIn) :-
  uitslagtype(TypeIn, Type, Title),		% zonder xml
  assert(nietgespeeldaantal(0)),
  findall(Catn,cat_uitslag(Catn, TypeIn),OnderdelenIn),
  dlg_onderdelen_select_Create(Win, "Einduitslagen", OnderdelenIn, UitList),
  not(UitList = []),
  stopdbrefresh(),
  openbaar(IOpenb),
  eindrap(Win, IOpenb, Type, UitList, Title), !,
  startdbrefresh().
  %writedevice(screen).
%einduitslagen(_Win,_TypeIn) :-
%  startdbrefresh().
  
toernooinummerselect([], _) :-
  !, fail.
toernooinummerselect([X], X ):- !.
toernooinummerselect(TL, Uit) :-
  preset(Pre),
  convertTnrList(TL, TLc),
  b_True = dlg_ListSelect("Selecteer toernooinummer of lokaal!",TLc,Pre,TidSel,Index),
  assert(preset(Index)),
  toernooinummervertaal(TidSel, Uit).
  
toernooinummervertaal("niet-KNLTB", "") :- !.
toernooinummervertaal(In, In).

convertTnrList([In|Tail], [Uit|Rest]) :-
  toernooinummerVertaal(Uit, In), !,
  convertTnrList(Tail,Rest).
convertTnrList(_, []).  

onderdeelTeChecken(Tid, OnderdeelId, Cat) :-
  wc(Cat, OnderdeelId, _, _, _, _, _, _, _, _, _, _, _, Tid, _),
  not(isOndercat(Cat)),
  fronttoken(OnderdeelId,_,_).

/*
showOnderdelen(CatL) :-
  member(C1, CatL),
  wc(C1, OnderdeeelID, Orde, Naam, EDG, Kort, SpSrt, SpStrk, LftV, LftTM, HDG, Schemasoort, KNLTB, ToernooiId, Geld),
  term_str(dbasedom, wc(C1, OnderdeeelID, Orde, Naam, EDG, Kort, SpSrt, SpStrk, LftV, LftTM, HDG, Schemasoort, KNLTB, ToernooiId, Geld), S1),
  write("\n",S1),
  fail.
showOnderdelen(_).
*/

in_echteweds(Cat, Tgnst) :-
  wd(WedsNo,Cat,Tgnst,_,_,_,_),
  w3(WedsNo,Cat,_,_,_,geen,_), !.
in_echteweds(Cat, Tgnst) :-
  wd(Wedsno,Cat,_, Tgnst,_,_,_),
  w3(WedsNo,Cat,_,_,_,geen,_), !.


%checkonderdelen(_) :- !.
checkonderdelen(Tid) :-
  findall(OnderdeelId, onderdeeltechecken(Tid, OnderdeelId, _), Olist),
  removeDubl(OList,[], OListR),
  member(OId, OListR),
  findall(Cat, onderdeelTeChecken(Tid, Oid, Cat), CatL),
  CatL = [H,_|_],
  nl,
  %showonderdelen(CatL),
  CatL = [_|Rest],
  repair(H, Rest),
  fail.
checkonderdelen(_).

repair(H, [F|Rest]) :-
  logf('Z', kopl(F, algemeen, H, algemeen), nee), !,
  repair(H, Rest).
repair(_, _).

einduitslagenXML() :-
  %win_PostEvent(Win,e_menu(id_Onderdeel_Webpaginas_uploaden, 0)),	% 4.7.2014
  findall(Tid, toernooiK(Tid, _), Tids),
  toernooinummerselect(Tids, Tid),
  checkonderdelen(Tid), 
  getKNLTBdefinities(Tid,ja,b_False,Doorgaan),
  Doorgaan = b_True,
  wisonderdeelnr(),
  assert(nietgespeeldAantal(0)), !,
  retractall(repWed(_,_)),
  copyCats(Tid, CatList),
  stopdbrefresh(),
  %write("\n",CatList),
  eindrapXML(CatList, "Export (XML)", Tid),
  retractall(repCat(_, _, _)),
  retractall(repWed(_,_)),
  retractall(ppRonde(_,_)),
  startdbrefresh().
  %writedevice(screen).
einduitslagenXML() :-
  startdbrefresh(),
  _Answer = dlg_MessageBox( "Einduitslagen", "Afgebroken of er is geen ServIt-2 toernooinummer gekoppeld!", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ).

xml_gegeven(Keywoord, In) :-
  xml_gegeven(Keywoord, In, "").

xml_gegeven(_, "",_) :- !.
xml_gegeven(Keywoord, In,Default) :-
  not(In = Default),
  xml_xlate(In, In1),
  writef(" %s=\"%s\"", Keywoord, In1), !.
xml_gegeven(_, _, _).

toernooiid2type(nrt, "Nationaal Ranglijst Toernooi").
toernooiid2type(jrt, "Jeugd Ranglijst Toernooi").
toernooiid2type(open, "Open toernooi of clubtoernooi").
toernooiid2type(rolstoel, "Rolstoeltennis Toernooi").
toernooiid2type(anders, "Geen bondstoernooi?!").

eindrapXML(UitList, _Title, Tid) :-		% XML
  cursor_SetWait(),
  closefile(work),
  getFolder(TDir,_,_,_),
  retractall(foutKNLTBnr(_)),
  format(XmlName1, "exp%s.xml", Tid),
  filenamepath(XmlName, TDir, XmlName1), 
  %dlg_note(XmlName),
  openwrite(work, XmlName),	% xml-bestand
  format(FName, "TA%s.html", Tid),
  filenamePath(RapName, TDir, FName),
  openwrite(nawf, RapName),	% rapport
  writedevice(work),									% switch naar work
    naam(Naam, _TN, _ToernooiDatum, _, _, _),
    write("<ns:Toernooi>"),
    xml_xlate(Naam, NaamXml),
    xml_tag("Toernooinaam", NaamXml),
    xml_tag("Toernooinummer", Tid),
    write("\n"),
    vereniging1(VerNr, ToernooiOrg, Aanhef, Straat, Huisnr, Plaats,Postcode,Telefoon,Email,_Website),
    functionarisP(wedstrijdleider, Tid, WL, WLEmail, WLTel, _, _),
    functionarisP(bondsgedelegeerde, Tid, BGNaam, BGEmail, BGTel, _, _),
  writedevice(nawf),									%switch naar rapport
  date(Year,Month,Day),			% -------------------- gedelegeerde
  time(Hours,Minutes,_Seconds,_Hundredths),
  helestr_int(0,TMS, Minutes),
  metaTag(Eerst, Tag),
  write(Eerst),
  write("\n<HTML><HEAD>", Tag, "<TITLE>Rapport</TITLE></HEAD>"),
  write("\n<STYLE>"),
  rapport_CSS(CSSContent),
  write(CSSContent),
  write("</STYLE><BODY>"),
  write("\n<H3>", NaamXml, "</H3>"),
  write("\nToernooinummer(s): <B>",  Tid, "</B> Vereniging/organisatie: <b>", VerNr, "</b> <b>", Aanhef, "</b> <b>",ToernooiOrg, "</b>"),
  write("<br>Adres: ", Straat," ",Huisnr," ",Plaats," ",Postcode," ",Telefoon," ",Email),
  toernooiK(Tid, TTyp),toernooiid2type(TTyp, TType),
  write("<br>Ingesteld toernooitype: <B>",TType,"</B>"),
  write("\n<HR>"),
  writef("<B>Rapport</B> gegenereerd op %/%/%  %:% met de ToernooiAssistent versie %s.", Day, Month, Year, Hours, TMS, versie),
  write("\n<BR>Toernooileider&nbsp;&nbsp;: ", WL, " telefoon: ", WlTel, " emailadres: ", WLEmail),
  write("\n<BR>Bondsgedelegeerde: ", BGNaam, " telefoon: ", BGTel, " emailadres: ", BGEmail),
  %write("\n<BR>Vereniging: ", Club, " te ", Stad), 
  wezenRapport(),
  xml_uitgevoerde_onderdelen(UitList),	% rapportage
  writedevice(work),			% ---------------------				switch naar work
    write("<ns:Toernooionderdelen>\n"),
    xml_1(UitList),
    %trap(xml_1(UitList),Ret,xml_error(Ret)),
    write("</ns:Toernooionderdelen>\n"),
    xml_tag("Verenigingsnummer",VerNr),
    write("</ns:Toernooi>\n"),
  closefile(work),									% close work
  writedevice(nawf),									% switch naar rapport
  write("</HTML>"),
  closefile(nawf),									% close rapport	
  fail.
eindrapXML(_UitList, _Title, _Tid) :-		% rapport
  closefile(work),
  closefile(nawf),
  foutKNLTBnr(_),
  findall(Naam, foutKNLTBnr(Naam), Namen),
  list_to_string(Namen, "\n -", NaamList),
  format(Msg, "De volgende deelnemers hebben nog foute of ontbrekende bondsnummers:\n -%s\nDie moet u eerst herstellen.", NaamList), 
  _X = dlg_MessageBox( "Resultaat", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultsecond, mesbox_suspendapplication ),
  !.
eindrapXML(_UitList, _Title, Tid) :-		% rapport
  retractall(foutKNLTBnr(_)),
  getFolder(Pad, _, _, _),
  format(FName, "TA%s.html", Tid),
  filenamePath(RapName, Pad, FName),
  format(URL1, "file:\\\\%s", RapName),
  Null = cast(api_hwnd,0),
  api_ShellExecute(Null,"open",URL1,"","",1), 		% open in browser
  format(XmlName, "exp%s.xml", Tid),
  filenamepath(XmlNameFull, Pad, XmlName),
  %write("\n",XmlNameFull),
  1 = dlg_MessageBox( "Resultaat", "Toernooiresultaat opsturen naar de KNLTB?\nBekijk eerst het rapport zorgvuldig!", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
  opsturen(XmlNameFull,Tid),
  %
  !.
eindrapXML(_UitList, _Title, _).

parseMsg(Tkst, [Msg|Uit], RestStr) :-
  upper_lower(TU, Tkst),
  searchstring(TU,"<MELDING>",FoundPos),
  Fp = FoundPos + 8,
  frontstr(Fp, Tkst, _, T1),
  upper_lower(T1U, T1),
  searchstring(T1U,"</MELDING>",FPos),
  Fpos1 = Fpos - 1,
  frontstr(FPos1, T1, Msg, Rest),
  parseMsg(Rest, Uit, RestStr), !.
parseMsg(Rest, [], Rest).
  
ifMsg([], Best, Tid ) :- 
  format(Best, "Het resultaat voor toernooinummer %s is aangeboden aan de KNLTB.\nIs er een tweede toernooinummer?\nVergeet niet dat ook aan te bieden!", Tid), !.
ifMsg(List, H, _) :-
  list_to_string(List, "\n", H).

%opsturen(_Win, _XmlName) :-
%  _Answer = dlg_MessageBox( "Toernooiresultaat", "Stuur een back-up aan support.\nWij zorgen dan voor de rest!\n(niet vergeten svp)", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
%  !.
%  write("even niet!",Xmlname), !.
opsturen(XmlName,Tid) :-
  %naam(_Naam, Nrsxx, _NrUit, _WT, _, _),
  %write("\n",Tid, " ", Xmlname),
  %NrsXX = [NrXX|_],
  providerX(Prov,_,"",_),
  format(URL,"http://%s/knltbproxy2015/UitslagServiceN.php",Prov),
  get_TempDir(_, TempFile),
  deletefile(TempFile),
  file_str(XmlName,XML4),
  AanmeldenVars = aanmeldenvars(),
  _Return = downloadData(URL, TempFile, ["Toernooiresultaat", XML4, "tid",Tid | AanmeldenVars], _HttpResponsecode, 0),
  file_str(TempFile, Str),
  %write("\n",_Return, "\n",Str),
  parsemsg(Str, MsgList, _Rest),
  ifmsg(MsgList, Best, Tid),
  dlg_MessageBox( "Resultaatupload", Best, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ).
  
eindrap(_Win, _, 4, UitList, _Title) :-
  getFolder(Dir,_,_,_),
  filenamepath(File, Dir, "onregelm.html"),
  %format(File, "%sonregelm.html", Dir),
  openwrite(work, File),
  writedevice(work),
  %webrapport(Font, Kleur),
  %rapport_CSS(Font, Kleur, CSSContent),
  uitslagen(4, UitList),		% onregelmatige uitslagen naar webpagina
  closefile(work),
  format(URL1, "file:\\\\%s", File),
  Null = cast(api_hwnd,0),
  api_ShellExecute(Null,"open",URL1,"","",1), !.		% open in browser
eindrap(_Win, _, _Type, _UitList, _Title) :-
  disk(OSPath),
  concat(OSPAth, "\\Samenvoeg", Path),
  filenamepath(Best, Path, "winnaars.txt"), 
  not(existfile(Best)),
  trap(mkdir(Path), _, true),
  vpi_ProcessEvents(b_True),
  fail.
eindrap(_Win, _, _Type, _UitList, _Title) :-
  %Type <> 5,		% voorlopig altijd!
  disk(OSPath),
  concat(OSPAth, "\\Samenvoeg", Path),
  filenamepath(Best, Path, "winnaars.txt"), 
  closefile(nawf),
  openwrite(nawf, Best),
  writedevice(nawf),
  write("onderdeel;plaats;manvrouw;naam;bondsnummer"),
  fail.
eindrap(Win, ja, Type, UitList, Title) :-	% open toernooi
  %Type <> 5,
  closefile(work),
  get_TempDir(_, TempFile),
  openwrite(work,TempFile),
  cursor_SetWait(),
  writedevice(work),
  write("\nOrganiserende vereniging: . . . . . . . . .       Verenigingsnummer: . . .\n\n"),
  uitslagen(Type, UitList),
  closefile(work),
  closefile(nawf),
  disk(OSPath),
  format(Best, "Er is ook een samenvoegbestand gegenereerd:\n%s\\Samenvoeg\\winnaars.txt", OSPath),
  dlg_MessageBox( "Samenvoegbestand voor Word", Best, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  retractall(_, lokaal), 
  dlg_rapport_Create(Win, Title,"Uitslag",b_False), !.
eindrap(Win, nee, Type, UitList, Title) :-	% niet open toernooi
  closefile(work),
  disk(OSPath),
  format(Best, "Er is ook een samenvoegbestand gegenereerd:\n%s\\Samenvoeg\\winnaars.txt", OSPath),
  dlg_MessageBox( "Samenvoegebestand voor Word", Best, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  get_TempDir(_, TempFile),
  openwrite(work,TempFile),
  cursor_SetWait(),
  writedevice(work),
  uitslagen(Type, UitList),
  closefile(work),
  closefile(nawf),
  retractall(_, lokaal), 
  dlg_rapport_Create(Win, Title,"Uitslag",b_False), !.
eindrap(_Win, _, _Type, _UitList, _) :-
  closefile(work),
  closefile(nawf).

trapmsg(File) :-
  format(Boodsch, "Fout bij het openen van bestand %s\nStaat MS-Word nog geopend?", File),
  dlg_MessageBox( "Bestand", Boodsch, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  closefile(work),
  exit(0).

journaalniettegroot(_FullName, Len, X, X) :-
  Len < 10000, !.
journaalniettegroot(Fullname, _, _, "journaal gereset") :-
  trap(deletefile(Fullname),_, fail), !.
journaalniettegroot(_Fullname, _, _, "journaal resetten mislukt!").

vpiconfbestaat(VPICONF, StrV) :-
  existfile(VPICONF),
  file_str(VPICONF, StrV), !.
vpiconfbestaat(_, "bestaat nog niet").

emailBackUp("email", BUNaam,Silent) :-
  %closebrowser(),
  not(geenServer(_)),
  existfile(BUNaam),
  getFolder(_, Toernooien, _, _),
  filenamepath(FullName, Toernooien, jourconst),
  writetojournaal("back-up email"),	% zorg dat ie bestaat
  file_str(FullName,JString),
  str_len(JString, Len),
  journaalniettegroot(FullName, Len, JString, JString1),
  get_tempdir(Dir, TempName),
  disk(Disk),
  filenameext(TempNameC, TempName, ".gz"),
  filenameext(TempNameH, Tempname, ".html"),
  file_compress(BUNaam, TempNameC),
  VPICONF = getvpiini(),
  vpiconfbestaat(VPICONF, StrV),
  syspath(ExeStartupPath,_ProgName),
  filenamepath(FName, ExeStartupPath, licconst),
  format(DiagnoseVars, "Tasi.nst: %s\nDir: %s\nTempname: %s\nDisk: %s\nvpiconf: %s\n%s", FName, Dir, Tempname,Disk,VPICONF,StrV),
  providerX(Prov, _PN, "", _),
  format(URL, "http://%s/backupenzo.php?step=upload", Prov),
  A = aanmeldenvars(),
  Return = fileupload(URL, TempNameC, TempNameH, ["diag", DiagnoseVars, "journaal", JString1 | A]),
  checkserver(Return),
  Silent = nee,					% 17.6.2014
  format(KURL, "file:\\\\%s", TempNameH),
  %win_editor_Create(Taskwin, Text, Titel),
  Null = cast(api_hwnd,0),
  api_ShellExecute(Null,"open",KURL,"","",1), !.		% open in browser
emailBackUp(_, _, _).

%spelerXMLuploadIf("resultaat1") :-
%  xmlSpelerData("RSLT"), !.
spelerXMLuploadIf(_).

%uploadAlg1(_Script,_Stap, _Files, _, _, _, b_True, _) :- !.	% files in tempdir!
uploadAlg1(_Script,Stap, Files, _, _, _, _, _) :-	% files in tempdir!
  %closebrowser(),
  get_tempdir(Dir, _),
  %deleteAllGZ(Dir),
  not(Files=[]),
  filenamepath(Outfile, Dir, "AA_Opsturen"),
  closefile(work),
  openwrite(work,Outfile),
  writedevice(work),
  member(Filex, Files),
  filenamepath(Filex,_,Kort),
  write("***[", Stap, "][", Kort, "]***\n"),
  trap(file_str(Filex, Str),_,trapmsg(Filex)),
  write(Str, "\n"),
  fail.
uploadAlg1(_Script,_Type, Files, _, _, _, _, _) :-
  closefile(work),
  Files = [Filex],
  deletefile(Filex),
  fail.
uploadAlg1(Script,Type, _Files, TitelK, _WasAlVerbinding, _Delay, b_True, Extravars) :-  %compress uploadfile
  get_tempdir(Dir, _),
  filenamePath(Bestand, Dir, "AA_Opsturen"),
  checkvoortgang(0, TitelK),
  spelerXMLuploadIf(Type),
  checkvoortgang(11, "bezig met uploaden"),
  filenamePath(Report, Dir, "uitklaar.htm"),
  providerX(Prov, _, "", _),
  format(URL,"http://%s/%s?type=%s&stap=%s", Prov, Script,  Type, "twee"),
  TaskWin = vpi_GetTaskWin(),
  Vars = aanmeldenVars(),
  append(Vars, ExtraVars, VarsT),
  concat(Bestand,".gz",Bestand1),
  file_compress(Bestand, Bestand1),
  ReturnK = upload2klapper(URL, Bestand1, Report, VarsT),
  checkvoortgang(100, ""),  % sluit weer
  TaskWin = vpi_GetTaskWin(),
  format(KURL, "file:\\\\%s", Report),
  Null = cast(api_hwnd,0),
  api_ShellExecute(Null,"open",KURL,"","",1),		% open in browser
  synchKlapper(ReturnK, Report, _FTPnotOK), !.
uploadAlg1(_Script,_Type, _, _TitelK, _, _, b_False, _).	% ergens onderbroken


deelnemerP(CatS, _) :-
  retractall(deelnemer(_)),
  member(Cat, CatS),
  opg(_, Cat, Tgnst, _, _, _, _, _, _, _Msgs, _),
  in_weds(Cat, Tgnst),
  not(deelnemer(Tgnst)),
  assert(deelnemer(Tgnst)),
  fail.
deelnemerP(_CatS, Tgnst) :-
  deelnemer(Tgnst).
deelnemerP(_, _) :-
  retractall(deelnemer(_)),
  fail.

gestaaktND(CatL, WedsNo) :-
  member(Cat, CatL),
  w3(WedsNo,Cat,_,_,_,gestaakt,_).

walkoverND(CatL, WedsNo) :-
  member(Cat, CatL),
  w3(WedsNo,Cat,_,_,_,walkover,_).
%walkoverND(CatL, WedsNo) :-
%  member(Cat, CatL),
%  w3(WedsNo,Cat,_,_,_,walkovernr,_).



int_strD(0, "-") :- !.
int_strD(I, S) :-
  format(S, "<SPAN class=rood>%u</SPAN>",I).

tgnst2nr(p(Nr,_),Nr).
tgnst2nr(p(_,Nr),Nr).
tgnst2nr(s(Nr),Nr).
tgnst2nr(pw(Nr),Nr).

nietgoedgekeurdeOpgaven(Cats,OpgNo) :-
  member(Cat, Cats),
  opg(OpgNo, Cat, Tgnst, _, _, _, _, _,_TeamId,Keurstatus,_Melding),
  in_echteweds(Cat, Tgnst),
  Keurstatus = msg(_,_,_,_,_,_).  % !!!!

allenietgoedgekeurdeopgaven(Cats, OpgNo) :-
  member(Cat1, Cats),
  stelselKNLTB(Cat1, OnderCats),
  nietgoedgekeurdeopgaven([Cat1|OnderCats],OpgNo).

nietgekeurdeOpgaven1(Cats,OpgNo) :-
  member(Cat, Cats),
  not(kopl(Cat,verliezer,_,_)),
  opg(OpgNo, Cat, Tgnst, _, _, _, _, _,_TeamId,na,_Melding),
  in_echteweds(Cat, Tgnst).

allenietgekeurdeopgaven(Cats, OpgNo) :-
  member(Cat1, Cats),
  stelselKNLTB(Cat1, OnderCats),
  nietgekeurdeopgaven1([Cat1|OnderCats],OpgNo).

personenP(Tgl, Dlnr) :-
  member(Tgnst, Tgl),
  tgnst2nr(Tgnst, Dlnr).

isWees(Naam) :-
  wc(CatNo, _, _, Naam, _, _, _, _, _, _, _, _, _, "", _),
  not(isOndercat(CatNo)).
  
wezenRapport() :-
  findall(Wees, isWees(Wees), Wezen),
  not(Wezen = []),
  write("\n<p>Attentie: De volgende onderdelen horen niet bij een toernooinummer:<br>"),
  %write("\n<TABLE border=0 cellpadding=3 cellspacing=0>",
  list_to_string(Wezen, "\n</LI><LI>", WL),
  write("\n<UL>\n<LI>",WL,"</LI></UL>"),
  write("\nMoeten dat onderdeel of die onderdelen ook opgestuurd worden naar de KNLTB?<br>Verstuur het resultaat dan nog niet maar koppel ze eerst!<br>"),
  write("\nKlik hiervoor op <a href='https://www.toernooiklapper.nl/veel-gestelde-vragen/voor-aanvang-toernooi/'>\"Zijn alle onderdelen gekoppeld aan ServIt?\"</a>.</p>"),!.
wezenrapport().  

spelersBuitenSpel(CatL, No) :-
  member(Cat, CatL),
  opg(No, Cat, Tgnst, _, _, _, _, _, _, _, _),
  not(in_weds(Cat, Tgnst)).

nietgespeeld(CatL, WNr) :-
  member(Cat, CatL),
  wd(WNr,Cat,T1,T2,_,_,_),
  nietgespeeld1(WNr, Cat, T1, T2).
  
nietgespeeld1(_, _, onbekend, _) :- !.
nietgespeeld1(_, _, _, onbekend) :- !.
nietgespeeld1(WNr, Cat, s(_), s(_)) :-
  not(w3(WNr,Cat,_,_,_,_,_)), !.
nietgespeeld1(WNr, Cat, p(_,_), p(_,_)) :-
  not(w3(WNr,Cat,_,_,_,_,_)), !.

xml_uitgevoerde_onderdelen(CatList) :-			% geeft alleen rapport
  write("\n<TABLE border=0 cellpadding=3 cellspacing=0>",
  "\n<TR><TH>Uitgevoerde<BR>Onderdelen</TH>",
        "<TH>Spel<BR>soort</TH>",
        "<TH>Ge-<BR>slacht</TH>",
	"<TH>Struc-<BR>tuur<BR>*)</TH>",
	"<TH>Niet<BR>af</TH>",
        "<TH>W.O.</TH>",
        "<TH>Op-<BR>gave</TH>",
        "<TH>Deel<BR>ne-<BR>mers</TH>",
        "<TH>Controle-<BR>fouten</TH>",
        "<TH>Niet<BR>gekeurd</TH>",
        "<TH>Zij-<BR>lijn</TH>",
        "</TR>"),
%  "\n<TR><TH>max</TH><TH>afw</TH><TH>min</TH><TH>max</TH><TH>afw</TH></TR>"),
  member(Cat, CatList),
  xml_uitgevoerd_onderdeel(Cat),
  fail.
xml_uitgevoerde_onderdelen(CatList) :-		
  write("\n</TABLE><P>"),
  write("<TABLE cellpadding=3 cellspacing =3  style=\"border-width: 0\">"),
  write("\n<TR><TD style=\"border-width: 0\"><TABLE border=1 cellpadding=3 cellspacing=0>"),
  write("\n<TR><TH colspan=2>*)&nbsp;&nbsp;betekenis</TH></TR>"),
  write("\n<TR><TD>A</TD><TD>Afvalschema</TD></TR>"),
  write("\n<TR><TD>P</TD><TD>Poule</TD></TR>"),
  write("\n<TR><TD>N</TD><TD>Niet ingedeeld</TD></TR>"),
  write("\n<TR><TD>V</TD><TD>met voorronde(s)</TD></TR>"),
  write("\n<TR><TD>34</TD><TD>met 3/4 plaats</TD></TR>"),
  write("\n<TR><TD>T</TD><TD style=\"white-space: nowrap\">met verliezersronde</TD></TR>"),
  write("\n</TABLE></TD>"),
  %checkGoedgekeurd(CatList),
  write("\n<TD style=\"border-width: 0; vertical-align: top\">"),
  findall(FouteOpgave,allenietgekeurdeopgaven(CatList,FouteOpgave), FouteOpgaveList),
  count(FouteOpgaveList, 0, AantalFOpg),
  AantalFopg > 0, 
  write("\n<b>Er zijn niet-gekeurde opgaven. Doe alsnog een inschrijfcontrole vóór dat je deze resultaten opstuurt!</b><P>"),
  fail.
xml_uitgevoerde_onderdelen(CatList) :-		
  findall(FouteOpgave,allenietgoedgekeurdeopgaven(CatList,FouteOpgave), FouteOpgaveList),
  count(FouteOpgaveList, 0, AantalFOpg),
  AantalFopg > 0, 
  write("\nProbeer controlefouten uit de schema's te halen en genereer deze export opnieuw.<p>"),
  fail.
xml_uitgevoerde_onderdelen(_CatList) :-		
  write("\n</TD></TR></TABLE>").

xml_wedsex(heren, "M", "Heren").
xml_wedsex(dames, "V", "Dames").
xml_wedsex(gemengd,"G", "Gemengd").
xml_wedsex(hdnvt,"N", "n.v.t.").

xml_uitgevoerd_onderdeel(Cat) :-			% geeft alleen rapport
  wc(Cat, _, _, _Onderdeel, WTyp, _, _, _, _, _,HDGN, _, _, _, _),
  posP(_, Cat, _, _, AfvalPoule),
  repCat(Cat, XMLnaam, Link),
  xml_spelsoort(WTyp, _Spelsoort, SpelsoortS),
  xml_wedsex(HDGN, _Geslacht, GeslachtS),
  ap2str(Cat, Afvalpoule, _, Structuur),
  stelselKNLTB(Cat, Voorrondes),
  findall(Tgnst, deelnemerP([Cat|Voorrondes],Tgnst), Tgl),
  findall(Dlnr, personenP(Tgl, Dlnr), DList),
  count(DList, 0, AantalTgnst), 
  findall(W, walkoverND([Cat|Voorrondes],W), Wlist),
  count(Wlist, 0, WO),
  int_strD(WO, Walkovers),
  findall(G, gestaaktND([Cat|Voorrondes],G), GList),
  count(GList, 0, GS),
  int_strD(GS, Gestaakten),
  findall(WdLeeg, nietgespeeld([Cat|Voorrondes], WdLeeg), WdLList),
  count(WdLList, 0, WdLA),
  int_strD(WdLA, Nietgespeeld),
  nietgespeeldaantal(NGA),
  NGA1 = NGA + WdLA,
  assert(nietgespeeldAantal(NGA1)),
  write("\n<TR><TD><A href=\"#", Link, "\">", XMLNaam,"</A></TD>",
  	"<TD>",SpelSoortS,"</TD>",
        "<TD>",GeslachtS,"</TD>",
        "<TD class='middle'>",Structuur,"</TD>",
        "<TD class='middle'>",Nietgespeeld,"</TD>",
        "<TD class='middle'>",Walkovers,"</TD>",
        "<TD class='middle'>",Gestaakten,"</TD>",
        "<TD class='middle'>",AantalTgnst,"</TD>"),
%        "<TD class='middle'>",Fouten,"</TD>"),
  fail.
xml_uitgevoerd_onderdeel(Cat) :-
  stelselKNLTB(Cat, Voorrondes),
  findall(OpgNo, nietgoedgekeurdeOpgaven([Cat|Voorrondes],OpgNo), OpgList),
  count(OpgList, 0, SpK),
  int_strD(SpK, Fouten),
  findall(OpgNoNK, nietgekeurdeOpgaven1([Cat|Voorrondes],OpgNoNK), OpgNKList),
  count(OpgNKList, 0, SpNK),
  int_strD(SpNK, NietGekeurd),
  findall(SpB, spelersBuitenSpel([Cat|Voorrondes], SpB), SpBList),
  count(SpBList, 0, SpBA),
  int_strD(SpBA, Buitenspel),
  write("<TD class='middle'>",Fouten,"</TD>",
        "<TD class='middle'>",NietGekeurd,"</TD>",
        "<TD class='middle'>",BuitenSpel,"</TD>",
        "</TR>"), !.

stelselKNLTB(Cat, Voorrondes) :-
  findall(VCat, voorrondeKNLTB(Cat, VCat), Voorrondes), !.

voorrondeKNLTB(_Cat, _Voorronde) :-
  retractall(algeweestK(_)),
  fail.
voorrondeKNLTB(Cat, VoorrondeX) :-
  getbacktrack(Here1),
  koplv(CatV, Cat),
  not(topcatorschemasoort(CatV)),
  cutbacktrack(Here1),		% als er één is, dan allemaal in volgorde
  wc(Voorronde, _, _, _, _, _, _, __, _, _, _, _, _, _, _),
  getbacktrack(Here),
  koplv(Voorronde, Cat),
  not(topcatorschemasoort(Voorronde)),
  cutbacktrack(Here),
  voorrondeAKNLTB(Voorronde, VoorrondeX),
  not(algeweestK(VoorrondeX)),
  assert(algeweestK(VoorrondeX)).
voorrondeKNLTB(_Cat, _Voorronde) :-
  retractall(algeweestK(_)),
  fail.

voorrondeAKNLTB(Cat, Cat).
voorrondeAKNLTB(Cat, VoorrondeX) :-
  getbacktrack(Here1),
  koplv(CatV, Cat),
  not(topcatorschemasoort(CatV)),
  cutbacktrack(Here1),		% als er één is, dan allemaal in volgorde
  wc(Voorronde, _, _, _, _, _, _, __, _, _, _, _, _, _, _),
  getbacktrack(Here),
  koplv(Voorronde, Cat),
  not(topcatorschemasoort(Voorronde)),
  cutbacktrack(Here),
  voorrondeAKNLTB(Voorronde, VoorrondeX).

xml_reserved_char('<',  "&lt;"  ).
xml_reserved_char('>',  "&gt;"  ).
xml_reserved_char('&',  "&amp;" ).
xml_reserved_char('\'', "&#39;").	% 9.3.2002 was &apos;
xml_reserved_char('"',  "&quot;").
xml_reserved_char('ñ',  "&#241;").
xml_reserved_char('ë',  "&#235;").

xml_xlate(In, Uit) :-
  bound(In),
  str_len(In, Len),
  L1 = Len + 1, !,
  xml_xlate1(In, 1, L1, "", Uit).
xml_xlate(Uit, In) :-
  bound(In),
  xml_reserved_char(Char, Trans),
  searchstring(In,Trans,FoundPos),
  F1 = FoundPos - 1,
  frontstr(F1,In,StartS,RestS),
  searchchar(RestS,';',EPos),
  EPos1 = EPos, % + 1,
  frontstr(EPos1,RestS,_,Staart),
  format(In1, "%s%c%s", StartS, Char, Staart), !,
  xml_xlate(Uit, In1).
xml_xlate(In, In) :-
  bound(In), !.

xml_xlate1(_, Len, Len, Uit, Uit) :- !.
xml_xlate1(In, CharNo, Len, S, Uit) :-
  subchar(In,CharNo,Char),
  xml_reserved_char(Char, XReference), !,
  concat(S, XReference, S1),
  Next = CHarNo + 1, 
  xml_xlate1(In, Next, Len, S1, Uit).
xml_xlate1(In, CharNo, Len, S, Uit) :-
  subchar(In,CharNo,Char),
  str_char(CS, Char),
  concat(S, CS, S1),     
  Next = CHarNo + 1, 
  xml_xlate1(In, Next, Len, S1, Uit).

xml_speelsterkte(Cat, Speelsterkte, St, b_True) :-
  wc(Cat, _, _, _, _, _, _, Speelst, _LftV, _LftTM, _, _, _, _, _),
  fronttoken(Speelst, St, _), !,
  format(Speelsterkte, "<ns:Speelsterkte>%s</ns:Speelsterkte>",St).
xml_speelsterkte(_, "", "", b_False).

xml_spelsoort(e, "E", "Enkel") :- !.
xml_spelsoort(_, "D", "Dubbel").

xml_leeftijd(Cat, _SexList, LeeftVanS, LeeftTMS, LeeftijdenS, b_True) :-
  wc(Cat, _, _, _, _, _, _, _Sterkte, leeftijd(_,LeeftVan), leeftijd(_,LeeftTM), _, _, _, _, _),
  LeeftVan > 0, LeeftTM > 0,
  format(LeeftVanS, " <ns:LeeftijdVanAf>%u</ns:LeeftijdVanAf>", LeeftVan),
  format(LeeftTMS,  " <ns:LeeftijdTotEnMet>%u</ns:LeeftijdTotEnMet>", LeeftTM), !,
  format(LeeftijdenS, "Leeftijden van %u t/m %u.", LeeftVan, LeeftTM).
xml_leeftijd(Cat, _SexList, LeeftVanS, "", LeeftijdenS, b_True) :-
  wc(Cat, _, _, _, _, _, _, _Sterkte, leeftijd(_, LeeftVan), _LftTM, _, _, _, _, _),
  LeeftVan > 0, !,
  format(LeeftVanS, " <ns:LeeftijdVanAf>%u</ns:LeeftijdVanAf>", LeeftVan),
  format(LeeftijdenS, "Leeftijden vanaf %u.", LeeftVan).
xml_leeftijd(Cat, _SexList, "", LeeftTMS, LeeftijdenS, b_True) :-
  wc(Cat, _, _, _, _, _, _, _Sterkte, _LftV, leeftijd(_, LeeftTM), _, _, _, _, _),
  LeeftTM > 0, !,
  format(LeeftTMS, " <ns:LeeftijdTotEnMet>%u</ns:LeeftijdTotEnMet>", LeeftTM),
  format(LeeftijdenS, "Leeftijden t/m %u.", LeeftTM).
xml_leeftijd(_Cat, _SexList, "", "", "", b_False).

xml_plaatsing(0, "") :- !.
xml_plaatsing(N, Geplaatst) :-
  format(Geplaatst, " <ns:Geplaatst>%d</ns:Geplaatst>", N).

scheidingsteken(',').
scheidingsteken(';').

xml_naamscanNoXlate(In, VoorX1, VoorlX, Achter) :-
  scheidingsteken(Teken),
  searchchar(In, Teken, Pos),
  Pos > 2,
  str_len(In, Len),
  L1 = Pos - 1,
  L2 = Len - Pos,
  P1 = Pos + 1,
  substring(In,1,L1,Achter),
  trim_c(Achter),
  substring(In,P1,L2, Voor),
  trim_c(Voor),
  concat(Voor, " ", VoorA), 
  frontstr(1,VoorA,VoorlX1,_),
  upper_lower(VoorlX, VoorlX1),
  xml_minVoornaam(Voor, VoorX1), !.
xml_naamscanNoXlate(In, "?", "", In) :-
  not(In = ""), !.
xml_naamscanNoXlate(In, "", "", In).

xml_naamscan(In, VoorX1, VoorlX, AchterX) :-
  scheidingsteken(Teken),
  searchchar(In, Teken, Pos),
  Pos > 2,
  str_len(In, Len),
  L1 = Pos - 1,
  L2 = Len - Pos,
  P1 = Pos + 1,
  substring(In,1,L1,Achter),
  trim_c(Achter),
  substring(In,P1,L2, Voor),
  trim_c(Voor),
  concat(Voor, " ", VoorA), 
  frontstr(1,VoorA,VoorlX1,_),
  upper_lower(VoorlX, VoorlX1),
  xml_xlate(Voor, VoorX),
  xml_minVoornaam(VoorX, VoorX1),
  xml_xlate(Achter, AchterX), !.
xml_naamscan(In, "?", "", In) :-
  not(In = ""), !.
xml_naamscan(In, "", "", In).

xml_minVoornaam("", "?") :- !.
xml_minVoornaam(In, In).
 
xml_voorvoegsel(Voornaam, Rest1, Key) :-
  xml_voorvoegsel1(Voornaam, Rest, "", Voorvoegsel),
  trim_c(Voorvoegsel),
  xml_xlate(Voorvoegsel, VoorvoegselX),
  writef("%s=\"%s\"", Key, VoorvoegselX), 
  trim_c(Rest),
  xml_minVoornaam(Rest, Rest1), !.
  
xml_voorvoegsel1(Voornaam, Rest1, In, VoorvoegselU) :-
  trim_c(Voornaam),
  str_len(VoorNaam, Len),
  reversestr(VoorNaam, "", NaamRev),
  searchchar(NaamRev,' ',Pos),
  P1 = Len - Pos + 1,
  frontstr(P1,VoorNaam,Rest,Voorvoegsel), 
  upper_lower(Voorvoegsel, VoorvoegselL),
  Voorvoegsel = VoorvoegselL, !,
  format(In1, "%s %s", Voorvoegsel, In),
  xml_voorvoegsel1(Rest, Rest1, In1, VoorvoegselU).
xml_voorvoegsel1(VoorNaam, VoorNaam, X, X).

xml_lidnrcheck(LidNr, LidNr, _) :-
  trim_c(LidNr),
  not(LidNr = ""),
  not(LidNr = "00000000"),
  proef11det(LidNr), !.
xml_lidnrcheck(LidNr, "00000000", Naam) :-
  LidNr = "00000000", !,
  assert(invalidLidnr(b_True)),
  writedevice(X),
  writedevice(nawf),
  xml_xlate(Naam, Naam1),
  write(Naam1, "(",LidNr, ") "),
  writedevice(X). 
xml_lidnrcheck(_LidNr, "00000000", Naam) :-
  assert(invalidLidnr(b_True)),
  writedevice(X),
  writedevice(nawf),
  xml_xlate(Naam, Naam1),
  write(Naam1, " "),
  writedevice(X). 

%xml_error(43) :- closefile(work), !.
%xml_error(Ret) :- exit(Ret).

updStStat(Sterkte) :-
  retract(sterkteStat(Sterkte, N)),
  N1 = N +1, !,
  assert(sterkteStat(Sterkte, N1)).
updStStat(Sterkte) :-
  assert(sterkteStat(Sterkte, 1)).

updLeeftStat(Leeft) :-
  retract(leeftStat(Leeft, N)),
  N1 = N + 1,
  assert(leeftStat(Leeft, N1)), !.
updLeeftStat(Leeft) :-
  assert(leeftStat(Leeft, 1)).

bondsnummerHak(Nr, NrUit) :-
  str_len(Nr, Len),
  Len > 8,
  trap(frontstr(8, Nr, NrUit, _), _, true), !.
bondsnummerHak(Nr, Nr).

xml_tgnst2(s(No1)) :-
  sp2(No1,_Sex1,_Naam1,_,_ ,_,_,_,_,_Club1,_, _,_LidNr1,StErkte1,_StD1,Geb1, _,_,_,_,_,_Ranglijst,_,_,_,_,_),
  str_intP(Sterkte1, Sterkte),
  updStStat(Sterkte),
  leeftijdP(Geb1, Leeftijd),
  updLeeftStat(Leeftijd),
  !.
xml_tgnst2(pw(No1)) :-
  sp2(No1,_Sex1,_Naam1,_,_ ,_,_,_,_,_Club1,_, _,_LidNr1,StErkte1,_StD1,Geb1, _,_,_,_,_,_Ranglijst,_,_,_,_,_),
  str_intP(Sterkte1, Sterkte),
  updStStat(Sterkte),
  leeftijdP(Geb1, Leeftijd),
  updLeeftStat(Leeftijd),
  !.
xml_tgnst2(p(No1,No2)) :-
  sp2(No1,_Sex1,_Naam1,_,_ ,_,_,_,_,_Club1,_, _,_LidNr1,_StE1,SterkD1,Geb1, _,_,_,_,_,_,_Ranglijst1,_,_,_,_),
  str_intP(SterkD1, Sterkte1),
  updStStat(Sterkte1),
  leeftijdP(Geb1, Leeftijd1),
  updLeeftStat(Leeftijd1),
  sp2(No2,_Sex2,_Naam2,_,_ ,_,_,_,_,_Club2,_, _,_LidNr2,_StE2,SterkD2,Geb2, _,_,_,_,_,_,_Ranglijst2,_,_,_,_),
  str_intP(SterkD2, Sterkte2),
  updStStat(Sterkte2),
  leeftijdP(Geb2, Leeftijd2),
  updLeeftStat(Leeftijd2),
  !.

xml_eindstand(   1,   1).
xml_eindstand(   2,   2).
xml_eindstand(   3,   4).
xml_eindstand(   5,   8).
xml_eindstand(   9,  16).
xml_eindstand(  17,  32).
xml_eindstand(  33,  64).
xml_eindstand(  65, 128).

xml_scan_eindstand(T, Cat, 3, 3, "3-4") :-	% het gaat om afvalsystemen
  wd(No,Cat,T3,T4,_Uitsl,_,_), 
  No >= 2,	% het is een halve finale
  No < 4, 
  verliezer(No, Cat, T3, T4, T), % de verliezer is T
  kopl(Cat, pl3en4(_), Cat34, _), % en er zijn speciale een koppelingen voor
  w3(1,Cat34,T,_,_,_,_), !.
xml_scan_eindstand(T, Cat, 4, 4, "3-4") :-
  wd(No,Cat,T3,T4,_Uitsl,_,_), 
  verliezer(No, Cat, T3, T4, T), 
  No >= 2,	% No is 2 of 3 voor derde of vierde plaats
  No < 4, 
  kopl(Cat, pl3en4(_), Cat34, _),
  wd(1,Cat34,_T3x,_T4x,_Uitslvv,_,_), % er is wel een wedstrijd
  w3(1, Cat34,_,_,_,_,_),  %en gespeeld
  not(w3(1,Cat34,T,_,_,_,_)), !.	% <=== hier zit het verschil
xml_scan_eindstand(T, Cat, 3, 4, "S") :-
  wd(No,Cat,T3,T4,_Uitsl,_,_), 
  verliezer(No, Cat, T3, T4, T), 
  No >= 2,	% No is 2 of 3 voor derde of vierde plaats
  No < 4, 
  kopl(Cat, pl3en4(_), Cat34, _),
  wd(1,Cat34,_T3x,_T4x,_Uitslvv,_,_), % er is wel een wedstrijd maar niet gespeeld!
  !.
xml_scan_eindstand(T, Cat, Van, Tot, "S") :-
  wd(No,Cat,T3,T4,_Uitsl,_,_), 
  verliezer(No, Cat, T3, T4, T), % <== de wedstrijd die die verloren heeft
  xml_eindstand(Van, Tot),
  No >= Van - 1,
  No < Tot, !.
xml_scan_eindstand(T, Cat, 1, 1, "S") :-
  w3(1,Cat,T,_,_,_,_), !.
xml_scan_eindstand(T, Cat, Van, Tot, "S") :-	% T komt niet voor als verliezer
  findall(No, wedstrijdT(T, No, Cat), NList),		% en heeft ook niet gewonnen
  sortint_c(NList),					% 25.2.2002
  NList = [Noy|_],
  Nx = Noy + 1,
  xml_eindstand(Van, Tot),
  Nx >= Van,
  Nx <= Tot, !.
xml_scan_eindstand(_T, _Cat, 256, 512, "S").
  
wedstrijdT(T, No, Cat) :-
  wd(No,Cat,_,T,_Uitsl,_,_), 
  not(w3(No,Cat,_,_,_,_,_)).
wedstrijdT(T, No, Cat) :-
  wd(No,Cat,T,_,_Uitsl,_,_), 
  not(w3(No,Cat,T,_,_,_,_)).

xml_voorronde_eindstand(Cat, "Q", T) :-
  w3(1,Cat,T,_,_,_,_).  	% maar heeft wel een partij gewonnen
xml_voorronde_eindstand(Cat, "QL", T) :-
  wd(1,Cat,T3,T4,_Uitsl,_,_), 
  verliezer(1, Cat, T3, T4, T), % <== de wedstrijd die die verloren heeft
  w3(_,Cat,T,_,_,_Bijz,_).  	% maar heeft wel een partij gewonnen
xml_voorronde_eindstand(Cat, "QV", T) :-
  member(No, [2,3]),
  wd(No,Cat,T3,T4,_Uitsl,_,_), 
  verliezer(No, Cat, T3, T4, T), % <== de wedstrijd die die verloren heeft
  w3(_,Cat,T,_,_,_,_).  	% maar heeft wel een partij gewonnen

update_team_voorronde(_Cat, Tgnst, Kwal) :-
  retract(team(Tgnst, Van, Tot, TeamId, Plaatsing, _)),
  assert(team(Tgnst, Van, Tot, TeamId, Plaatsing, Kwal)), !.
update_team_voorronde(Cat, Tgnst, Kwal) :-
  opg(Nr, Cat, Tgnst, Pl, _, _, _, _, _, TStat, _),
  foutenwaarsch(Tgnst, Tstat), 
  format(TeamId, "V%dx%d", Cat, Nr),
  assert(team(Tgnst, 0, 0, TeamId, Pl, Kwal)), !.

update_team_poule(_N, Tgnst, _Plaatsing, _xTeamId) :-
  team(Tgnst, _, _, _, _, "3-4"), !.
update_team_poule(N, Tgnst, Plaatsing, _xTeamId) :-
  retract(team(Tgnst, Van, Tot, TeamId, _, _)),
  format(KC, "P%u", N),
  assert(team(Tgnst, Van, Tot, TeamId, Plaatsing, KC)), !.
update_team_poule(N, Tgnst, Plaatsing, TeamId) :-   % team bestaat nog niet
  foutenwaarsch(Tgnst, na),
  format(KC, "P%u", N),
  assert(team(Tgnst, 0, 0, TeamId, Plaatsing, KC)).
  
voorrondeEindstand(CatV) :-
  pos(CatV, _Card, _, poel),			% poule en jeugdonderdeel
  poule_ranking_alle(CatV, Rank, Tgnst, _Punten),
  opg(Nr, CatV, Tgnst, Pl, _, _, _, _, _, _, _),		% daarvan de opgaven die 
  in_weds(CatV, Tgnst),				% maar wel opgesteld staan in de voorronde
  format(TeamId, "V%dx%d", CatV, Nr),
  update_team_poule(Rank, Tgnst, Pl, TeamId),
  fail.
voorrondeEindstand(CatV) :-
  pos(CatV, _Card, _, afval),
  not(kopl(CatV, verliezer, _, _)),		% geen echte voorronde	% 3.5.2014
  xml_voorronde_eindstand(CatV, KC, Tgnst),
  update_team_voorronde(CatV, Tgnst, KC),
  fail.
voorrondeEindstand(CatV) :-
  opg(Nr, CatV, Tgnst, Pl, _, _, _, _, _, Tstat, _),
  in_weds(CatV, Tgnst),
  %not(team(Tgnst, _, _, _, _, _)),
  format(TeamId, "T%dx%d", CatV, Nr),
  not(team(Tgnst, _, _, _, _, _)),
  foutenwaarsch(Tgnst, Tstat),
  assert(team(Tgnst, 0, 0, TeamId, Pl, "QS")),
  fail.
voorrondeEindstand(_CatV).

foutenWaarsch(Tgnst, _Tstat) :-
  tgnstnum(Tgnst, SpNo),
  sp2(SpNo,_,N1,_Tel1,_ ,_,_,_,_,_,_, _,BondsNr,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  not(proef11det(Bondsnr)),
  assert(foutKNLTBnr(N1)),
  fail.
foutenWaarsch(_Tgnst, _Tstat).

isMasterCat(Cat) :-
  wc(Cat, _Oid, _, _Naam, _, _, _, _, _, _, _, "0", _, Tid, _),
  kopl(_,_,Cat,_),
  fronttoken(Tid,_,_), !. 
isMasterCat(Cat) :-
  wc(Cat, Oid, _, _Naam, _, _, _, _, _, _, _, _, _, Tid, _), 
  kopl(Cat, _, CatM, _),
  wc(CatM, Oid0, _, _Naam0, _, _, _, _, _, _, _, _, _, Tid, _),
  fronttoken(Oid,_,_),
  Oid <> Oid0, !.
  %resp_default = dlg_Ask("IsMasterCatFinal","IsMasterCatFinal"/*Prompt*/,["&OK","&Cancel"]/*ButtonTitlesList*/).

xml_teams(Cat) :-
  pos(Cat, _, _, afval),		% de hoofdrondes
  opg(Nr, Cat, Tgnst, Pl, _, _, _, _, _, TStat, _),
  not(Tgnst = pw(_)),
  %not(team(Tgnst, _, _, _, _, _)),
  in_weds(Cat, Tgnst),
  foutenWaarsch(Tgnst, Tstat),
  format(TeamId, "T%dx%d", Cat, Nr),
  xml_scan_eindstand(Tgnst, Cat, Van, Tot, KC),
  not(team(Tgnst, _, _, _, _, _)),
  assert(team(Tgnst, Van, Tot, TeamId, Pl, KC)),
  fail.
xml_teams(Cat) :-
  pos(Cat, _, _, poel),
  poule_ranking_alle(Cat, Rank, Tgnst, _Punten),
  opg(Nr, Cat, Tgnst, Pl, _, _, _, _, _, TStat, _),
  not(Tgnst = pw(_)),
  in_weds(Cat, Tgnst),
  foutenwaarsch(Tgnst,  TStat),
  format(TeamId, "T%dx%d", Cat, Nr),
  not(team(Tgnst, _, _, _, _, _)),
  assert(team(Tgnst, Rank, Rank, TeamId, Pl, "S")),
  fail.
xml_teams(Cat) :-
  wc(CatV, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
  getbacktrack(Here),  
  koplv(CatV, Cat),		% alle voorgaande rondes 
  %N < 3,
  cutbacktrack(Here),
  not(topcatorschemasoort(CatV)),
  voorrondeEindstand(CatV),
  koplv(CatVV, CatV),
  xml_teams(CatVV),  % nog een dieper ..
  fail.
xml_teams(Cat) :-			% mensen die alleen aan de verliezersronde meedoen
  kopl(CatV, verliezer, Cat, _),
  opg(Nr, CatV, Tgnst, _Pl, _, _, _, _, _, TStat, _),
  not(Tgnst = pw(_)),
  in_weds(CatV, Tgnst),
  foutenwaarsch(Tgnst, TStat),
  format(TeamId, "T%dx%d", CatV, Nr),
  not(team(Tgnst, _, _, _, _, _)),
  assert(team(Tgnst, 0, 0, TeamId, 0, "S")),
  fail. 
xml_teams(_Cat).

writeInfo(_, _) :- !.
writeInfo(Diepte, String) :-
  D1 = Diepte * 3,
  str_len(Offset, D1),
  writedevice(X),
  %writedevice(work),
  writedevice(screen),
  write("\n",X, Offset,String),
  writedevice(X).

xml_rondes(Cat, N) :-
  wc(CatV, _, _, Naam, _, _, _, _, _, _, _, _SchSrt, _, _, _),
  getbacktrack(Here),  
  koplv(CatV, Cat),
  cutbacktrack(Here),
  not(topcatorschemasoort(CatV)),
  format(Info, " xml_rondes %u Rondes %u %s", N, CatV, Naam),
  writeinfo(N, Info),
  N1 = N + 1,
  xml_ronde(CatV, "", N1),		% rapporteert een ronde van die cat
  fail.  
xml_rondes(Cat, N) :-
  kopl(CatV,verliezer,Cat,_),
  pos(CatV, _Card, _, _),
  xml_ronde(CatV, " verliezersronde=\"J\"", N),
  fail.  
xml_rondes(_Cat, _).

xml_ronde(Cat, Type, Diepte) :-		% Type = soms verliezersronde
  wc(Cat, _, _, _, _, _, _, _, _, _, _, _SchSrt, _, _, _),
  not(rondeL(Cat, _)),
  findall(No, wd(No,Cat, _, _, _,_,_), NoL),
  assertz(rondeL(Cat, Type)),
  sortint_c(NoL),
  xml_ronde1(Cat, NoL),
  xml_rondes(Cat,Diepte), 	% 1.7.2006
  fail.
xml_ronde(_Cat, _Type, _).

xml_ronde1(Cat, [No|Rest]) :- 
  wd(No,Cat,T1,T2,_,_,_), 
  w3(No,Cat,_,_,_,_,_),   % er is een uitslag 	 18.7.2014
  not(T1 = pw(_)),
  not(T2 = pw(_)),
  not(gespeeldePartij(No,Cat)),	% 20.7.2014
  assertz(gespeeldePartij(No, Cat)), !,
  xml_ronde1(Cat, Rest). 
xml_ronde1(Cat, [_No|Rest]) :- 
  xml_ronde1(Cat, Rest), !.
xml_ronde1(_, _).

xml_nextInt(Str, Tok, Rest) :-
  fronttoken(Str, Tok, Rest),
  str_int(Tok, _Int), !.
xml_nextInt(Str, Tok, Rest) :-
  fronttoken(Str, _, Rest1),
  xml_nextInt(Rest1, Tok, Rest).

xml_sets(Str, "T") :-
  xml_nextInt(Str, Thuis, Rest),
  xml_nextInt(Rest, Uit, _Rest1),
  looptag("Set"),
  xml_tag("ThuisGames", Thuis),
  xml_tag("UitGames", Uit),
  fail.
xml_sets(Str, "G") :-
  xml_nextInt(Str, Thuis, Rest),
  xml_nextInt(Rest, Uit, _Rest1),
  looptag("Set"),
  xml_tag("ThuisGames", Thuis),
  xml_tag("UitGames", Uit),
  fail.
xml_sets(Str, "U") :-
  xml_nextInt(Str, Uit, Rest),
  xml_nextInt(Rest, Thuis, _Rest1), 
  looptag("Set"),
  xml_tag("ThuisGames", Thuis),
  xml_tag("UitGames", Uit),
  fail.
xml_sets(Str, TU) :-
  xml_nextInt(Str, _Uit, Rest),
  xml_nextInt(Rest, _Thuis, Rest1), !, 
  xml_sets(Rest1, TU).
xml_sets(_, _).


%xml_uitslag(_No, _Cat, bye, _, _, "U", "") :- !.
%xml_uitslag(_No, _Cat, _, bye, _, "T", "") :- !.
xml_uitslag(No, Cat, _, _, Str, "G", Bijzonderheid) :-
  w3(No,Cat,gelijk_spel,_,_,B,_), !,
  xml_uitslagStr(Str, B, Bijzonderheid).
xml_uitslag(No, Cat, T3, _, Str, "T", Bijzonderheid) :-
  w3(No,Cat,T3,_,_,B,_), !,
  xml_uitslagStr(Str, B, Bijzonderheid).
xml_uitslag(No, Cat, _, T4, Str, "U", Bijzonderheid) :-
  w3(No,Cat,T4,_,_,B,_), !,
  xml_uitslagStr(Str, B, Bijzonderheid).
xml_uitslag(_No, _Cat, _, _, _, "N", "") :- !.

xml_uitslagStr(_Str, gestaakt, "G") :- !.		% gestaakt
xml_uitslagStr(_Str, walkover, "W") :- !.		% walk over
xml_uitslagStr(_Str, _, "").				% normale uitslag

xml_onderdeelnaam(Uit, Uit1) :-
  xml_xlate(Uit, Uit1).

statistiekenL(b_True, Cat) :-
  write("\n<TR><TD><B>Leeftijdstatistieken</B></TD><TD>"),  
  write("<TABLE border=1 cellpadding=3 cellspacing=0>"),  
  write("\n<TR><TH>Leeftijd</TH>"),
  findall(L, leeftStat(L, _), LList),
  sortint_c(LList),
  member(L1, LList),
  markCritL(Cat, L1, Lout),
  write("<TD>",Lout,"</TD>"),
  fail.
statistiekenL(b_True, _) :-
  write("</TR>\n<TR><TD>Aantal</TD>"),
  findall(L, leeftStat(L, _), LList),
  sortint_c(LList),
  member(L1, LList),
  leeftStat(L1, Aantal),
  writef("<TD align=center>%3u</TD>", Aantal),
  fail.
statistiekenL(b_True, _Cat) :-
  write("</TABLE></TD></TR>"), !,
  retractall(leeftStat(_, _)).
statistiekenL(_, _Cat).
  
statistiekenS(b_True, Cat) :-
  write("\n<TR><TD><B>Speelsterktestatistieken</B></TD><TD><TABLE border=1 cellpadding=3 cellspacing=0>"),  
  write("</TR>\n<TR><TH>Sterkte</TH>"),
  findall(L, sterkteStat(L, _), LList),
  sortint_c(LList),
  member(L1, LList),
  markCritS(Cat, L1, Out),
  write("<TH>",Out,"</TH>"),
  fail.
statistiekenS(b_True, _) :-
  write("</TR>\n<TR><TD>Aantal</TD>"),
  findall(L, sterkteStat(L, _), LList),
  sortint_c(LList),
  member(L1, LList),
  sterkteStat(L1, Aantal),
  writef("<TD align=center>%3u</TD>", Aantal),
  fail.
statistiekenS(b_True, _) :-
  write("\n</TABLE></TD></TR>"), !,
  retractall(sterkteStat(_, _)).  
statistiekenS(_, _).

optionalTag(_Tag, 0) :- !.
optionalTag(_Tag, 99) :- !.
optionalTag(Tag, Leeft) :-
  str_int(LS, Leeft),
  xml_tag(Tag, LS).

komtvoorbij(_, _) :-!.
komtvoorbij(Cat, String) :-
  member(Cat,[19]),
  format(Str, "%u komt voorbij in %s", Cat, String),  
  dlg_note("", Str), !.

copyCats(Tid,_) :-
  retractall(repCat(_,_,_)),
  wc(CatNo, _, _, Naam, _, _, _, _, _, _, _, _SchSrt, _, Tid, _),
  %member(CatNo, [4]),
  komtvoorbij(CatNo, "CopyCats"),
  topCatOrSchemasoort(CatNo),
  xml_onderdeelnaam(Naam, XMLnaam),
  format(Link, "C%u", CatNo),
  assert(repCat(CatNo, XMLNaam, Link)),
  fail.
copyCats(_, CatL) :-
  findall(Cat, repcat(Cat,_,_), CatL).

xml_1([Cat|_]) :-  % entered met file work
  wc(Cat, OnderdeelId, _, Onderdeel, WTyp, _, SexList, _, leeftijd(_,LeeftVi), leeftijd(_,LeeftTmi), HDGN, _, _, _, _),
  retractall(rondeL(_, _)),
  retractall(gespeeldePartij(_, _)),
  retractall(eenKeerFout()),
  retractall(leeftStat(_, _)),
  retractall(sterkteStat(_, _)),	% zet alle statistieken op nul
  retractall(team(_,_,_,_,_,_)),
  assert(invalidLidnr(b_False)),
  repCat(Cat, XMLnaam, Link),
  xml_speelsterkte(Cat, _Speelsterkte, _SpeelsterkteRapport, MetSpeelst),
  xml_spelsoort(WTyp, Spelsoort, SpelsoortS),
  xml_leeftijd(Cat, Sexlist, _LeeftVan, _LeeftTM, LeeftijdS, MetLeeft), 
  xml_wedsex(HDGN, Geslacht,GeslachtS),
  writedevice(work),			% ____________________________________________
  write("<ns:Toernooionderdeel>\n"),
    retractall(algeweest(_)),    % nieuwe lijst van teamids voor ieder toernooionderdeel
    xml_tag("Geslacht",Geslacht),
    optionalTag("LeeftijdTotEnMet",LeeftTmi),
    optionalTag("LeeftijdVanaf",LeeftVi),
    write("\n"),
    xml_xlate(Onderdeel, OnderdeelXl),
    xml_tag("Naam", OnderdeelXl),
    xml_tag("OnderdeelId",OnderdeelId),
    write("\n"),
  writedevice(nawf),
  write("\n<TABLE class=nieuw border=1 cellpadding=3 cellspacing=0><TR><TH colspan=2 align=left><A name=\"",Link,"\"></A>",XMLnaam,"</TR></TH>"),
  writedevice(work),
    xml_ronde(Cat, "", 0),
    xml_teams(Cat),  % hier worden ze gemaakt
    xml_onderdeelL(Cat), 	% 3.6.2010
    xml_tag("Spelsoort",Spelsoort),
    xml_teamL(Cat),
    write("</ns:Toernooionderdeel>\n"),
  writedevice(nawf),			% ____________________________________________
  write("\n<TR><TD><B>Spelsoort</B> ", SpelsoortS, " ", GeslachtS, "</TD><TD>",LeeftijdS, "&nbsp;</TD></TR>"),  
  statistiekenL(MetLeeft,Cat),
  statistiekenS(MetSpeelst, Cat),
  write("\n<TR><TD>"),
  writeRanking(),
  write("\n</TD></TR>"),
  write("\n</TABLE><P>"),  
  writedevice(work),
  fail. %!,			% ____________________________________________
  %xml_1(T).
xml_1([_|T]) :- 
  xml_1(T), !.
xml_1(_).

xml_onderdeelL(_) :-
  findall(Catx, rondeL(Catx,_),CatxL),
  term_str(catl,CatxL,CatLSt),
  format(Info, "xml_onderdeeL %s Rondes", CatLst),  
  writeinfo(9, Info),
  looptag("Rondes"),
  member(Cat, CatxL),
  pos(Cat, _, _, _),
  getbacktrack(Here),
  gespeeldePartij(_, Cat),
  cutbacktrack(Here),
  looptag("Ronde"),
  xml_rondeL(Cat),
  fail.
xml_onderdeelL(_).

isTeamEr(Tgnst, _) :-
  team(Tgnst, _, _, _Id3, _,_), !.
isTeamEr(Tgnst,Cat) :-
  wc(Cat, _, _, Naam, _, _, _, _, _, _, _, _, _, _Tid, _), 
  term_str(tgnst, Tgnst, Str),
  format(Note, "%s\n%s", Str, Naam),
  dlg_note("geen team?",Note),
  fail.
isTeamEr(bye, _) :- !, fail.
isTeamEr(Tgnst, _) :-
  term_str(tgnst, Tgnst, Str),
  format(Msg, "Team bestaat niet - %s ?",Str),
  writeinfo(2, Msg),
  fail.

alRepWed(No, Cat) :-
  repwed(No, Cat),
  wc(Cat, _, _, Naam, _, _, _, _, _, _, _, _, _, _Tid, _), 
  format(Msg, "Onderdeel %u %s dubbel gekoppeld!",Cat,Naam),
  writeToJournaal(Msg, nee), !.
alRepWed(No, Cat) :-
  assert(repwed(No, Cat)),
  fail.

xml_rondeL(Cat) :-
  looptag("Partijen"),
  retract(gespeeldePartij(No, Cat)),
  %writeinfo(1, "Hier"), 
  wd(No,Cat,T3,T4,UitslStr,_,_), 
  isteamer(T3,Cat),
  isTeamEr(T4,Cat),
  team(T3, _, _, Id3, _,_),
  team(T4, _, _, Id4, _,_),
  not(alRepWed(No, Cat)),	% 30.7.2014
  loopTag("Partij"),
  xml_uitslag(No, Cat, T3, T4, UitslStr,Uitslag, Bijzonderheid),
  xml_tag_opt("Bijzonderheid",Bijzonderheid),
  write("<ns:Sets>"),
  xml_sets(UitslStr, Uitslag),
  write("</ns:Sets>\n"),
  xml_tag("TeamThuisId", Id3),
  xml_tag("TeamUitId",Id4),
  xml_tag("Uitslag",Uitslag),
  fail. 
xml_rondeL(_).
      
xml_teamL(_Cat) :-
  looptag("Teams"),
  team(Tgnst, Van, Tot, TeamId, Pl, KC),	% 9.6.2010 is nog nodig voor de xml_partij..
    looptag("Team"),
    tgnst2deelnemer(Tgnst),
    xml_tgnst2(Tgnst), % voor de statistieken
    format(Eindstand, " <ns:EindstandTotEnMet>%d</ns:EindstandTotEnMet><ns:EindstandVanaf>%d</ns:EindstandVanaf>", Tot, Van),
    write(Eindstand), 
    xml_plaatsing(Pl, Plaatsing),
    write(Plaatsing),
    format(Kwali, "<ns:Kwalificatiecode>%s</ns:Kwalificatiecode>\n", KC),
    write(Kwali),
    xml_tag("TeamId",TeamId),
  fail. 
xml_teamL(_).

naamVolgens(Tgnst, TotL) :-
  member(Tot, TotL),
  team(Tgnst, _, Tot, _, _, _).
  %Tot > 0.

teamMet(Totx) :-   % als = 0 wordt de ranking niet getoond
  team(_, _, Totx, _, _, _).

writeRanking() :-		% gesplitst over twee kolommen
  retractall(gehadVoorRanking(_)),
  getbacktrack(Here),
    write("\n<B>Eindstand</B><TABLE>"),
    findall(Totx, teamMet(Totx), TotL),
    count(TotL, 0, Aantal),
    KolomAant = Aantal div 2 - 1,
    assert(ka(0)),
    sortint_c(TotL),
    findall(Tgnstxx, naamVolgens(Tgnstxx, TotL), TgnstL),
    member(Tgnst, TgnstL),
      tgnst_naam(nee, b_False, 0, Tgnst, Naam, _),
      ka(KAx),
      not(gehadVoorRanking(Tgnst)),
      assert(gehadVoorRanking(Tgnst)),
      team(Tgnst, Van, Tot, _, _, KC),
      %Tot > 0,
      writef("\n<TR><TD>%s</TD><TD style=\"text-align: right\">%u</TD><TD style=\"text-align: right\">%u</TD><TD style=\"text-align: right\">%s</TD></TR>", Naam, Van, Tot, KC),
      KAx1 = KAx + 1,
      assert(ka(KAx1)),
    KAx >= KolomAant,
  cutbacktrack(Here),
  fail.
writeRanking() :-
  write("\n</TABLE></TD><TD><TABLE>"),
  findall(Totx, team(_, _, Totx, _, _, _), TotL),
  sortint_c(TotL),
  Member(Tot, TotL),
    team(Tgnst, Van, Tot, _, _, KC),
    %Tot > 0, 
    not(gehadVoorRanking(Tgnst)),
    assert(gehadVoorRanking(Tgnst)),
    tgnst_naam(nee, b_False, 0, Tgnst, Naam, _),
    writef("\n<TR><TD>%s</TD><TD style=\"text-align: right\">%u</TD><TD style=\"text-align: right\">%u</TD><TD style=\"text-align: right\">%s</TD></TR>", Naam, Van, Tot, KC),
  fail.
writeRanking() :-
  write("\n</TABLE>"),
  retractall(gehadVoorRanking(_)).

pri(Keus, [Ond|Onderdelen]) :-
  frontstr(2, Ond, _, Catname1),
  wc(CatNo, _, _, Catname1, _, _, _, _, _, _, _, _, _, _, _),
  %wc(CatNo,Catname1,_,_,_,_,_),
  pos(CatNo,_,_,afval),
  uitslagen1(Keus, CatNo), !,
  pri(Keus, Onderdelen).
pri(Keus, [Ond|Onderdelen]) :-
  frontstr(2, Ond, _, Catname1),
  wc(CatNo, _, _, Catname1, _, _, _, _, _, _, _, _, _, _, _),
  %wc(CatNo,Catname1,_,_,_,_,_),
  pos(CatNo,_,_,poel),
  print_poel_uitsl(Keus, CatNo), !,
  pri(Keus, Onderdelen).
pri(Keus, [Ond|Onderdelen]) :-
  writedevice(X),
  writedevice(screen),
  write("\nfail:", Ond),
  writedevice(X), !,
  pri(Keus, Onderdelen).
pri(_, _).
  
uitslagen(Keus,Onderdelen) :-		% splits in afval en poules
  Keus <> 4,
  not(Onderdelen = []),
  ingevuld(Keus),  
  pri(Keus, Onderdelen),
  Keus <> 3,
  openbaar(ja), !,
  write("\n\n", 
  "Bondsgedelegeerde:_______________________ Paraaf:___________ Datum:_________\n").
uitslagen(4, _) :-
  %ingevuld(4),
  metaTag(Eerst, Tag),
  write(Eerst),
  write("\n<HTML><HEAD>", Tag, "<TITLE>Onregelmatige uitslagen</TITLE></HEAD>"),
  write("\n<STYLE>"),
  findall(TidX, toernooiK(Tidx,_),TidL),
  list_to_string(TidL, "\n", TidStr),
  rapport_CSS(CSSContent),
  write(CSSContent),
  write("\n</STYLE>\n<BODY>"),
  write("\n<H5>Onregelmatige uitslagen ",TidStr,"</H5>"),
  write("<table>"),
  write("<TR><TH>Onderdeel</TH><TH>Uitslag</TH><TH>Tekst</TH><TH>Ronde</TH><TH colspan=3>Winnaars(s)</TH><TH colspan=3>Verliezer(s)</TH></TR>"),
  wc(Cat, _, _, _, _WTyp, Onderdeel, _, _, _, _, _, _, _, _, _),
  pos(Cat,Card,_,_),			% afval schema niet meer
  not(kopl(Cat, verliezer, _, _)),
  wd(No,Cat,T3,T4,Uitslag,_,_), 
  onregelmatig(No, Cat, Tekst),
  ronde(No, Card, Ronde),
  w3(No,Cat,W1,_,_,_,_),
  verliezer(No, Cat, T3, T4, V1),	% degene die kennelijk opgaf
  ronde_tekst(0, Ronde,Plaats),
  write("\n<TR><TD>",Onderdeel,"<TD>", Uitslag, "<TD>", Tekst, "<TD>", Plaats),
  rep_persH(W1),
  rep_persH(V1),
  write("</TR>"),
  fail.
uitslagen(4,_) :- !,
  write("</table>\n<br>Klik op het bondsnummer voor het spelerprofiel!</body></html>"),
  nl.
uitslagen(_,_).

rep_persH(s(No)) :-  !,
  sp2(No,Sex,Naam,_,_ ,_,_,_,_,_Club,_, _,KNLTB,_StE,_StD,_GebDat, _,_,_,_,_,_,_,_,_,_,_),
  naamscan(nee,b_False,Naam, Naam1,_,_Landcode),
  spsrt(Sex,_,MV,_,_,_,_,_),
  sex_str(MV, MVstr), !,
  bondsnr(KNLTB, Link),
  writef("<TD>%c<TD>%s<TD>%s", MVStr, Naam1, Link).
rep_persH(p(N1,N2)) :- 
  sp2(N1,Sex1,Naam1,_,_ ,_,_,_,_,_,_, _,KNLTB1,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  naamscan(nee,b_False,Naam1, Naam11,_,_Landcode),
  spsrt(Sex1,_,MV1,_,_,_,_,_),
  sex_str(MV1, MVstr1),
  sp2(N2,Sex2,Naam2,_,_ ,_,_,_,_,_,_, _,KNLTB2,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  naamscan(nee,b_False,Naam2, Naam21,_,_Landcode2),
  spsrt(Sex2,_,MV2,_,_,_,_,_),
  sex_str(MV2, MVstr2), !,
  bondsnr(KNLTB1, Link1),
  bondsnr(KNLTB2, Link2),
  writef("\n<TD>%c<br>%c</TD><TD>%s<br>%s</TD><TD>%s<br>%s</TD>",  MVStr1, MVStr2,Naam11, Naam21,Link1, Link2).

rep_pers(Type,Schema, Plaats,s(No), WTyp, CardS, Cat) :-  !,
  get_sterkte(s(No), Sterkt),
  resultaatPlus(Stand1, Kolom, CardS1),
  CardS1 = CardS, !,
  eigenSterkte(Sterkt, Kolom, PuntenI, PuntenBehaald),
  rep_pers1(Type,Schema, Plaats, No, WTyp, CardS, Stand1, PuntenI, PuntenBehaald, Cat, Stand1).
rep_pers(Type,Schema, Plaats,pw(No), WTyp, CardS, Cat) :-  !,
  get_sterkte(s(No), Sterkt),
  resultaatPlus(Stand1, Kolom, CardS1),
  CardS1 = CardS, !,
  eigenSterkte(Sterkt, Kolom, PuntenI, PuntenBehaald),
  rep_pers1(Type,Schema, Plaats, No, WTyp, CardS, Stand1, PuntenI, PuntenBehaald, Cat, Stand1).
rep_pers(Type,Schema, Plaats,p(N1,N2), WTyp, CardS, Cat) :- 
  get_sterkte(p(N1,N2), Sterkt),
  resultaatPlus(Stand1, Kolom, CardS1),
  CardS1 = CardS, !,
  eigenSterkte(Sterkt, Kolom, PuntenI, PuntenBehaald),
  rep_pers1(Type,Schema, Plaats, N1, WTyp, CardS, Stand1, PuntenI, PuntenBehaald, Cat, Stand1),
  rep_pers1(Type,"","", N2, WTyp, CardS, "", PuntenI, PuntenBehaald, Cat, Stand1).

rep_pers1(2, _Schema, _Pl, _No, _WTyp, _CardS, _Stand1, 0, 0.0, _, _) :- !.
rep_pers1(_Type, _Schema, _Pl, No, _WTyp, _CardS, Stand1, _PuntenI, _PuntenBehaald, Cat, PlaatsBeide) :-
  sp2(No,Sex,Naam,_,_ ,_,_,_,_,_Club,_, _,KNLTB,_StE,_StD,_GebDat, _,_,_,_,_,_,_,_,_,_,_),
  naamscan(nee,b_False,Naam, Naam1,_,_Landcode),
  spsrt(Sex,_,MV,_,_,_,_,_),
  sex_str(MV, MVstr),
  writef("\n %-6 %c %", 
    Stand1, MVStr, Naam1),
  wc(Cat, _, _, Onderdeel, _, _, _, _, _, _, _, _, _, _, _), !,
  writedevice(X),
  writedevice(nawf),
  write("\n", Onderdeel, ";", PlaatsBeide, ";", MVStr, ";", Naam1, ";", Knltb),
  writedevice(X).

onregelmatig(No, Cat, "&nbsp;") :-
  w3(No,Cat,_,_,_,gestaakt,_), !.
onregelmatig(No, Cat, "&nbsp;") :-
  w3(No,Cat,_,_,_,walkover,_), !.

print_poel_uitsl(Keus, CatNo) :-
  wc(CatNo, _, _, H, _WTyp, _, _SexList, _, _, _, _, _, _, _, _),
  write('\n','\n',H),
  poule_ranking_alle(CatNo, Rank, Tgnst, _Punten),
  totAan(Keus, Rank),
  rep_pers_poule(CatNo, Tgnst, Rank),
  fail.
print_poel_uitsl(_, _).

totAan(1, _) :- !.
totAan(3, Rank) :-
  Rank < 5, !.
totAan(_, Rank) :-
  Rank < 3.

rep_pers_poule(Cat, p(N1,N2),Rank) :-
  format(Plaats, "%ue", Rank),
  sp2(N1,Sex,Naam,_,_ ,_,_,_,_,_Club,_, _,KNLTB,_StE,_StD,_GebDat, _,_,_,_,_,_,_,_,_,_,_),
  naamscan(nee,b_False,Naam, Naam1,_,_Landcode),
  spsrt(Sex,_,MV,_,_,_,_,_),
  sex_str(MV, MVstr),
  writef("\n %-6 %c %", Plaats, MVStr, Naam1),
  sp2(N2,Sex2,Naam2,_,_ ,_,_,_,_,_Club2,_, _,KNLTB2,_StE2,_StD2,_GebDat2, _,_,_,_,_,_,_,_,_,_,_),
  naamscan(nee,b_False,Naam2, Naam3,_,_Landcode2),
  spsrt(Sex2,_,MV2,_,_,_,_,_),
  sex_str(MV2, MVstr2),
  writef("\n %-6 %c %", "", MVStr2, Naam3),
  wc(Cat, _, _, Onderdeel, _, _, _, _, _, _, _, _, _, _, _), !,
  writedevice(X),
  writedevice(nawf),
  write("\n", Onderdeel, ";", Plaats, ";", MVStr, ";", Naam1, ";", Knltb),
  write("\n", Onderdeel, ";", Plaats, ";", MVStr2, ";", Naam2, ";", Knltb2),
  writedevice(X).
rep_pers_poule(Cat, s(N1),Rank) :-
  format(Plaats, "%ue", Rank),
  sp2(N1,Sex,Naam,_,_ ,_,_,_,_,_Club,_, _,KNLTB,_StE,_StD,_GebDat, _,_,_,_,_,_,_,_,_,_,_),
  naamscan(nee,b_False,Naam, Naam1,_,_Landcode),
  spsrt(Sex,_,MV,_,_,_,_,_),
  sex_str(MV, MVstr),
  writef("\n %-6 %c %", Plaats, MVStr, Naam1),
  wc(Cat, _, _, Onderdeel, _, _, _, _, _, _, _, _, _, _, _), !,
  writedevice(X),
  writedevice(nawf),
  write("\n", Onderdeel, ";", Plaats, ";", MVStr, ";", Naam1, ";", Knltb),
  writedevice(X).
rep_pers_poule(Cat, pw(N1),Rank) :-
  format(Plaats, "%ue", Rank),
  sp2(N1,Sex,Naam,_,_ ,_,_,_,_,_Club,_, _,KNLTB,_StE,_StD,_GebDat, _,_,_,_,_,_,_,_,_,_,_),
  naamscan(nee,b_False,Naam, Naam1,_,_Landcode),
  spsrt(Sex,_,MV,_,_,_,_,_),
  sex_str(MV, MVstr),
  writef("\n %-6 %c %", Plaats, MVStr, Naam1),
  wc(Cat, _, _, Onderdeel, _, _, _, _, _, _, _, _, _, _, _),!,
  writedevice(X),
  writedevice(nawf),
  write("\n", Onderdeel, ";", Plaats, ";", MVStr, ";", Naam1, ";", Knltb),
  writedevice(X).

totaan(1, No, Card) :-
  No >= 2 * Card - 1, !.	% tot aan eerste ronde
totaan(2, No, Card) :-
  No >= 2 * Card - 1, !.	% tot aan eerste ronde (maar met punten)
totaan(3, No, _):-		% nrs 1 t/m 4
  No > 2, !.
totaan(4, No, _):-		% nrs 1 t/m 2
  No > 0, !.
totaan(N, _No, _):-
  N > 4.
  
uitslagen1(Keus, Cat) :-
  Keus <> 5,
  wc(Cat, _, _, Onderdeel, WTyp, _, _, _, _, _, _, _, _, _, _),
  retractall(_, lokaal),
  pos(Cat,Card,_,_),				% grootte
  concat(Onderdeel, ".", Onder1),
  nl, nl, write(Onder1),
  getbacktrack(Here),		% loop terug vanaf finale door alle rondes
  numb(No),
  rep_uitsl(Cat, No,  WTyp, Keus),
  totaan(Keus, No, Card),
  cutbacktrack(Here),
  nl,
  fail. 
uitslagen1(_, _).

%   1...+....10...+....20...+....30...+....40...+....50...+....60...+....70...+


plaatsing(CatNo, Tgnst, Str) :-
  opg(_, CatNo, Tgnst, Pl, _, _Uitsl, _, _, _, _, _), 
  Pl > 0,
  str_int(Str, Pl), !.
plaatsing(_CatNo, _Tgnst, "").

find_schema(T1, Cat, Card, OutS) :-
  wd(No,Cat,T1,_,_,_,_),
  No >= Card,
  Out = 2 * (No - Card) + 1, 
  str_int(OutS, Out), !.
find_schema(T1, Cat, Card, OutS) :-
  wd(No,Cat,_,T1,_,_,_),
  No >= Card,
  Out = 2 * (No - Card) + 2, 
  str_int(OutS, Out), !.
find_schema(_T1, _Cat, _Card, "").

rep_uitsl(Cat, 1, WTyp, Keus) :-	% 1 = gewoon, 2 = onregelmatig
  w3(1,Cat,T1,_,_,_,_), 				% rapporteer de winnaar
  plaatsing(Cat, T1, Pl),
  pos(Cat,Card,_,_),
  find_schema(T1, Cat, Card, Schema),	% regel in het schema 
  rep_pers(Keus,Schema, PL, T1, WTyp, 0, Cat),
  fail.
rep_uitsl(Cat, No, _, _) :- 		% van alle andere de verliezer
  wd(No,Cat,T3,_,_,_,_),
  T3 = bye, !. 
rep_uitsl(Cat, No, _, _) :- 
  wd(No,Cat,_,T4,_,_,_),
  T4 = bye, !. 
rep_uitsl(Cat, 2, WTyp, Keus) :-
  kopl(Cat, pl3en4(3), Cat34, _),
  w3(1,Cat34,T1,_,_,_,_), 				% rapporteer de winnaar
  plaatsing(Cat, T1, Pl),
  pos(Cat,Card,_,_), 
  find_schema(T1, Cat, Card, Schema),
  rep_pers(Keus, Schema,Pl, T1, WTyp,3,Cat), !. 	
rep_uitsl(Cat, 3, WTyp, Keus) :-
  kopl(Cat, pl3en4(4), Cat34, _),
  wd(No,Cat34,T3,T4,_Uitsl,_,_), 
  verliezer(No, Cat34, T3, T4, T1),
  plaatsing(Cat, T1, Pl),
  pos(Cat,Card,_,_), 
  find_schema(T1, Cat, Card, Schema),
  rep_pers(Keus, Schema,Pl, T1, WTyp,5,Cat), !. 	
rep_uitsl(Cat, No, WTyp, Keus) :-
  card(No, CardS),
  wd(No,Cat,T3,T4,_Uitsl,_,_), 
  verliezer(No, Cat, T3, T4, V1), 
  plaatsing(Cat, V1, Pl),
  pos(Cat,Card,_,_), 
  find_schema(V1, Cat, Card, Schema),
  rep_pers(Keus, Schema,Pl, V1, WTyp,CardS, Cat), !. 	
rep_uitsl(_, _, _, _).

  
verliezer(No, Cat, T1, T2, T2) :-
  w3(No,Cat,T1,_,_,_,_), !.
verliezer(No, Cat, T1, T2, T1) :-
  w3(No,Cat,T2,_,_,_,_), !. 

eigenSterkte(_St, _Kolom, 0, 0.0).

resultaatPlus("3e", 3, 3).		% speciaal geval 
resultaatPlus("4e", 3, 5).
resultaatPlus(Stand, Kolom, CardS1) :-
  resultaat(Stand,  Kolom, CardS1).

sex_str(m, 'm').
sex_str(v, 'v').
 
ronde_tekst(0,kwartfin,"kwfin") :- !.
ronde_tekst(0,halvefin,"h fin") :- !.
ronde_tekst(0,finale,  "final") :- !.
ronde_tekst(_,r(No), Tekst) :- !,
  format(Tekst, "rnd %", No).

ingevuld(4) :- !.				% niet voor onregelm. uitsl.
ingevuld(_) :-
  wc(Cat1, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
  pos(Cat1,_,_,afval),				% wèl ingedeeld
  not(kopl(Cat1,verliezer,_,_)),
  w3(N11,Cat1,_,_,_,_,_), 				% een uitslag t/m kwart
  N11 < 8, !.                                   % finales is bekend
ingevuld(_) :-
  wc(Cat1, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
  pos(Cat1,_,_,poel),				% wèl ingedeeld
  findall(N1,w3(N1,Cat1,_,_,_,_,_),N1List),		% kijk o fer wel uitslagen zijn
  count(N1List, 0, Count),
  Count > 0, !.					% voldoende uitslagen
ingevuld(_) :-
  Title="",
  dlg_Note(Title,"Nog geen uitslagen bekend."),
  fail.

get_sterkte(s(Sp), StE) :-
  sp2(Sp,_,_,_,_,_RE1,_RD1,_,_, _, _, _, _, StE, _StD1, _, _, _, _, _,_,_,_,_,_,_,_),
  trim_c(StE),
  !.
get_sterkte(pw(Sp), StD) :-
  sp2(Sp,_,_,_,_,_RE1,_RD1,_,_, _, _, _, _, _StE, StD, _, _, _, _, _,_,_,_,_,_,_,_),
  trim_c(StD),
  !.
get_sterkte(p(Sp1,Sp2),Sterkt) :-
  sp2(Sp1,_,_,_,_,_RE1,_RD1,_,_, _, _, _, _, _, StD1, _, _, _, _, _,_,_,_,_,_,_,_),
  sp2(Sp2,_,_,_,_,_RE2,_RD2,_,_, _, _, _, _, _, StD2, _, _, _, _, _,_,_,_,_,_,_,_),
  trim_c(StD1),
  trim_c(StD2),
  sterkste(StD1, StD2, Sterkt), !.
get_sterkte(_, "").

sterkste(St1, St2, St1) :-
  St1 < St2,
  St1 <> "", !.
sterkste(_, St2, St2).

%__________________________________________________________________________

/********************************************************************/
/*    rapportage:                                                   */
/*      knelpunten                                                  */
/*      nieuwe uitslagen en nieuwe planning                         */
/********************************************************************/

database - temp
  found_vlag()
  sp_tijd(integer, tijd, string Park)
  determ tijdstip(tijd)
nondeterm   sp_sel(integer)

predicates
  gewaarschuwd(tgnst,janee,janee,string)
  y_coord(integer, integer, integer)
  check_found
  prt_verh(verhl, tijd)
  %verschil(integer, integer, string, integer)
procedure dVoorE(integer)

clauses

prt_verh([], _) :- !.
prt_verh([Periode|Rest], Tijd) :-
  Periode = per(T1,T2),
  T2 > Tijd, 			% anders geldt verhindering niet meer
  tijd_kwart(D1,U1,M1,T1),
  tijd_kwart(_,U2,M2,T2),
  dag(D1,DS1,_), !,
  helestr_int(0,MS1,M1),
  helestr_int(0,MS2,M2),
  writef("\n    verh  %-9 %2.%2 tot %2.%2",DS1,U1,MS1,U2,MS2),
  prt_verh(Rest, Tijd).
prt_verh([_|Rest], Tijd) :-
  prt_verh(Rest, Tijd).

y_coord(WdNo, Card, Y_Coordinaat) :-
  dim(Card, DC),
  dim(WdNo, Dim),
  RX = DC - Dim,
  bitleft(1, RX, TweeTotDeRonde),
  card(WdNo, RondeCard),
  Y_Coordinaat = TweeTotDeRonde * (1 + 2 * (WdNo - RondeCard)).

/*_______________________
  Controleer:
  1 = knelpunten
  2 = bezwaren
  _______________________*/

dVoorE(0) :- !.
dVoorE(_) :-
  write("Dubbel vóór Enkel melden.\n").

controleer(_, 1, _) :-
  knelpuntenrapport(), !.
controleer(_Win, _, _) :-
  cursor_SetWait(),
  %disk(OSPath),
  %write("\nOSPAth   ", OSPath),
  closefile(work),
  get_TempDir(_, TempFile),
  openwrite(work,TempFile),
  fail.
controleer(_, 2, _) :-
  planparam_c(_EersteDag, DagMaxO, _PerWat, PerWatStr, _PerDagen, MinSeparatie, MaxSeparatie, Duur, Enkel, DagMax, DvoorE),
  findall(Tijd1,bn(Tijd1,_), Tijden),
  writedevice(work),
  DM1 = DagMax + 1,
  strepen(60, '_'), nl,
  write( "Wedstrijden met bezwaren bij de volgende instelling onder Toernooi, Beleid:\n"),
  writef("Minimale tijd tussen het begin van twee wedstrijden = %d kwartieren.\n", MinSeparatie), 
  writef("Duur van een wedstrijd = %d kwartieren.\n", Duur), 
  writef("Max. %d wedstrijd(en) per persoon per dag.\n", DM1),
  writef("Max %d wedstrijd(en) per persoon per onderdeel %.\n", DagMaxO, PerWatStr),
  writef("Max %d enkelspelen per persoon per dag.\n", Enkel),
  writef("Max %d kwartieren tussen twee wedstrijden voor dezelfde persoon.\n", MaxSeparatie),
  dVoorE(DvoorE),
  strepen(60, '_'), 
  writedevice(screen),
  sorttijd_c(Tijden),
  member(Tijd, Tijden),
  w2(WdNo,Cat,Tijd,_,_,Geforceerd,_,_), 
  writedevice(screen),
  check_forceer_c(2, [Tijd], WdNo, Cat, _, Result,_,_),
  Result <> 0,
  writedevice(work),
  pos(Cat,Card,_,_), 
  wc(Cat, _, _, _, _WTyp, WedsCatShort, _, _, _, _, _, _, _, _, _),
  tijd_kwart(D1,U1,M1,Tijd),
  helestr_int(0,MS1,M1),
  dag(D1, DagNaam, _),
  y_coord(WdNo, Card, Y_Coordinaat),
  wd(WdNo,Cat,T1,T2,_,_,_),
  repa(nee,'}',nee, nee,nee,0,2,1,WdNo,Cat,T1,alleplanning,TS1,_Score, _TtijdChanged,b_True, b_False,_Clubs,_,_,_,_,_,_),
  repa(nee,'}',nee, nee,nee,1,2,1,WdNo,Cat,T2,alleplanning,TS2,_,_,b_True,b_False,_Clubs1,_Ankr,_,_,_,_,_),
  format(Str, "%c %s/% ", Geforceerd,TS1,TS2),
  writef("\n\n%s%3d %s %d:%s %1.50", WedsCatShort, Y_Coordinaat,  
  						DagNaam, U1, MS1, Str),
  check_forceer_c(3, [Tijd], WdNo, Cat, _, _, Bezw,_),
  write(Bezw),
  not(found_vlag()),
  assert(found_vlag),
  fail.
/*
controleer(_Parent, Mode, Tekst) :-
  mode(Mode, Enkel_Dubbel),	% alleen 3 of 4
  assert(found_vlag()),		% altijd... anders krijg je boodschap
  writedevice(work),
  String = " ---------------------- ",
  write("\n\n", String, Tekst, " op de zelfde dag ", String), 
  dag(DagN,_,_),				% voor iedere dag
  retractall(sp_tijd(_, _, _)),
  findall(T1, dag_tijden(T1,DagN), Tijden),	% alle tijdstippen
  sorttijd_c(Tijden),    			% gesorteerd
  alle_spelers(Tijden, Mode),	% kijk vooruit voor de rest van de dag
  member(Tijd,Tijden),
  retractall(sp_tijd(_, Tijd, _)),	% tijdstip zelf doet niet mee
  w2(WNum,Cat,Tijd,_,_,_,_,_),	% pak een geprogrammeerde wedstrijd 
  wc(Cat, _, _, _, WedsType, _, _, _, _, _, _, _, _, _, _),
  not(check_type(Mode, WedsType)),
  wd(WNum,Cat,Sp1,Sp2,_,_,_),	% kijk wie er meespelen
  de_spelers(WNum,Cat,Sp1,Sp2,SpelerW),
  member(SpelerNum,SpelerW),
  sp_tijd(SpelerNum, TijdNa, _),
  rep_wed1(WNum, Cat, Tijd, 0, nee),
  sp2(SpelerNum,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  naamscan(ja,b_False,Naam,Naam1,_,_Landcode),
  write("\n Hiervan moet ", Naam1, " later om "),
  tijd_kwart(_,U1,M1,TijdNa),
  helestr_int(0,MS1,M1),
  write(U1,".",MS1," nog een ", Enkel_Dubbel, " spelen."),
  fail.
*/
controleer(Parent, 1, Tekst) :-
  found_vlag(),
  retractall(_, temp),
  writedevice(screen),
  %write("\12"),
  closefile(work), !,
  dlg_rapport_Create(Parent, Tekst,"Knelpunt",b_False).
controleer(Parent, Mode, Tekst) :-
  Mode > 1,
  writedevice(work),
  check_found,			% always true
  retractall(_, temp),
  write("\12"),
  writedevice(screen), !,
  closefile(work),
  dlg_rapport_Create(Parent, Tekst,"Controle",b_False).
controleer(_Win, _, _) :-
  closefile(work).

check_found :-
  not(found_vlag()),
  write("\n\nGeen afwijkingen gevonden."), !.
check_found.
  
gepland(WdNo, Cat, J1, J2, Geforceerd) :-
  w2(WdNo,Cat,Tijd1,J1,J2,Geforceerd,_,_), 
  tijd_kwart(D1,U1,M1,Tijd1),
  dag(D1,DS1,_), 
  helestr_int(0,MS1,M1), !,
  write("\nGepland op ",DS1," ",U1,".",MS1,":").
gepland(_, _, ja, ja, ' ') :-
  write("\nNog niet gepland:").

gewaarschuwd(onbekend,_,_,"") :- !.
gewaarschuwd(_,ja,nee,"?") :- !.
gewaarschuwd(_,_,_,"").

%BEGIN_DLG attributen spelergegevens
/**************************************************************************
	Creation and event handling for dialog: attributen spelergegevens
**************************************************************************/

constants

%BEGIN attributen spelergegevens, CreateParms, 15:33:14-3.9.2002, Code automatically updated!
  dlg_attributen_spelergegevens_DlgType = wd_Modal
  dlg_attributen_spelergegevens_Title = "attributen spelergegevens"
  dlg_attributen_spelergegevens_RCT = rct(40,32,322,190)
  dlg_attributen_spelergegevens_Flags = [wsf_Close,wsf_TitleBar]
  dlg_attributen_spelergegevens_Help = idh_attributen_spelergegevens
%END Attributen spelergegevens, CreateParms

predicates

  %dlg_attributen_spelergegevens_eh : EHANDLER
nondeterm  kies_speler2(string,integerlist)
procedure  write_s(WINDOW,integer,integerlist,catl)
  doe_opties(WINDOW,integer,verhl,integer,tijd, janee, sex, catl)
  get_club(string, string)
  speeltijden1P(wl,wtl,catl)
  prt_speeltP(wtl,integer,integer,janee,integer, slist, slist)
  prt_gewaarsch1(janee,string,janee)
nondeterm  prt_opgaven(integer,wedscat, boolean)
  prt_opgaven1(wedscat, tgnst, boolean)
  opgesteld(integer)
  write_maatz(string,integer,integer)
nondeterm  whitesp(char)
determ newline(string, string)
procedure metBaanPark(string, string)
procedure incasso2OK(string, string)

clauses

prt_opgaven(SKeus,CatN,Rap) :-
  sp2opg(SKeus,CatN,Opg,MaatNo,_,_),
  met_maat(MaatNo, Maat),
  opg(Opg,CatN,Tgnst,Plts,Rnd,Opgesteld,_,_,_,_,_),
  prt_opgaven1(CatN, Tgnst, Rap),
  wc(CatN, _, _, _, _, Str, _, _, _, _, _, _, _, _, _),
  %wc(CatN,_,_,Str,_,_,_),
  opgesteld(Opgesteld),
  write(Str,"  "),
  write_maatz(Maat,Plts,Rnd).

opgesteld(0) :- write("\n    "), !.
opgesteld(_) :- write("\n  u ").

prt_opgaven1(_, _, b_False) :-  !.
prt_opgaven1(Cat, Tgnst, _) :-
  wd(WedsNo,Cat,Tgnst,T2,_,_,_),
  not(T2 = bye),
  not(w3(WedsNo,Cat,_,_,_,_,_)), !;
  wd(WedsNo,Cat,T1,Tgnst,_,_,_),
  not(T1 = bye),
  not(w3(WedsNo,Cat,_,_,_,_,_)), !.
  
write_maatz("",0,0) :- !.
write_maatz(Maat,0,0) :- !,
  writef("%-20",Maat).
write_maatz(Maat,X,0) :- !,
  writef("%-18 p:%",Maat,X).
write_maatz(Maat,X,Y) :-
  writef("%-18 p:% r:%",Maat,X,Y).

in_weds(Cat, Tgnst) :-
  wd(_,Cat,Tgnst,_,_,_,_), !.
in_weds(Cat, Tgnst) :-
  wd(_,Cat,_, Tgnst,_,_,_), !.


prt_gewaarsch1(nee,"?",ja) :-
  oproepen(ja), !.
prt_gewaarsch1(_," ",_).

metBaanPark(BaanPark, Uit) :-
  fronttoken(BaanPark, _, _),
  baan_adm(ja), !,
  format(Uit, " [%s]", BaanPark).
metBaanPark(_BaanPark, "").

prt_speeltP([],X,X,_,_, L, L) :- !.
prt_speeltP([wt(Type,Tijd,Gewaarsch,BaanPark,BijW, _WasOp)|Rest],In,Uit,Vraagteken,1, LIn, LUit) :-
  tijd_kwart(D1,U1,M1,Tijd),
  dag(D1,DS1,_), !,
  helestr_int(0,MS1,M1),
  prt_gewaarsch1(Gewaarsch,Gstring,Vraagteken),
  metBaanPark(BaanPark, BP),
  bijwinst(BijW, _, BWT),
  writef("\n    %-4 %-7 %2.%2%s %1 %s",Type,DS1,U1,MS1,BP,Gstring, BWT),
  In1 = In + 1,
  prt_speeltP(Rest,In1,Uit,Vraagteken,1, Lin, Luit).
prt_speeltP([wt(Type,Tijd,_,BaanPark,BijW,_WasOp)|Rest],In,Uit,Vraagteken,2, Lin, [Str| Luit]) :-
  tijd_kwart(D1,U1,M1,Tijd),
  dag(D1,DS1,_), !,
  helestr_int(0,MS1,M1),
  metBaanPark(BaanPark, BP),
  bijwinst(BijW, _, BijWT),
  format(Str, "  %-4 %-7 %2.%2%s %s %s",Type,DS1,U1,MS1,BP,BijWT),
  In1 = In + 1,
  prt_speeltP(Rest,In1,Uit,Vraagteken,2, Lin, Luit).

speeltijden1P([],[],_) :- !.   
speeltijden1P([w(_,Cat,WedsTijd,Gewaarsch, _, _, BijW, BaanPark)|Rest],
				[wt(Kort,WedsTijd,Gewaarsch,BaanPark,BijW, "")|Out],[]) :-
  wc(Cat, _, _, _, _, Kort, _, _, _, _, _, _, _, _, _),!,
  speeltijden1P(Rest,Out,[]).
speeltijden1P([w(_,Cat,WedsTijd,Gewaarsch, _, _, BijW, BaanPark)|Rest],
			[wt(Wedstyp,WedsTijd,Gewaarsch,BaanPark,BijW, "")|Out],CatNoList) :-
  member(Cat, CatNoList),
  wc(Cat, _, _, _, _, WedsTyp, _, _, _, _, _, _, _, _, _), !,
  speeltijden1P(Rest,Out,CatNoList).
speeltijden1P([_|Rest],Out,CatNoList) :-
  speeltijden1P(Rest,Out,CatNoList).

get_club(ClubNo, ClubNaam) :-
  club(ClubNo, Naam, Distr), !,
  format(ClubNaam, "%s %s", Naam, Distr).
get_club(_, "").

whitesp(' ').
whitesp('\t').

newline(In, Uit) :-
  frontchar(In,Char,RestString),
  whitesp(Char), !,
  newline(RestString, Uit).
newline(In, Rest) :-
  frontchar(In,'\n',Rest).

formatOpm(Opm, Line, In, Uit, Fudge, LL) :-		% handle eventueel opschuiven (Fudge)
  not(Fudge = ""),
  Line = "", !,
  Line1 = Fudge,
  formatOpm(Opm, Line1, In, Uit, Fudge, LL).
formatOpm(Opm, Line, In, Uit, _Fudge, _LL) :-	% handle laatste \n
  not(fronttoken(Opm, _, _)), !,
  concat(In, Line, Uit).
formatOpm(Opm, Line, In, Uit, Fudge, LL) :-		% handle nieuwe regel
  newline(Opm, Opm1), !,
  concat(Line, "\n", Voegtoe),
  concat(In, Voegtoe, In1),
  formatOpm(Opm1, "", In1, Uit, Fudge, LL).
formatOpm(Opm, Line, In, Uit, Fudge, LL) :-		% handle whitespace
  frontchar(Opm,Char,Rest),
  whitesp(Char),
  str_len(Line, Len),
  Len > LL, !,
  %Len > 70, !,
  concat(Line, "\n", Line1),
  concat(In, Line1, In1),
  formatOpm(Rest, "", In1, Uit, Fudge, LL).
formatOpm(Opm, Line, In, Uit, Fudge, LL) :-		% handle al het andere
  frontchar(Opm,Char,Rest), !,
  str_char(CharS,Char),
  concat(Line, CharS, Line1),
  formatOpm(Rest, Line1, In, Uit, Fudge, LL).
formatOpm(_, Line, In, Uit, _Fudge, _) :-		% handle laatste stukje
  concat(In, Line, Uit).

incasso2OK(IN, "incasso OK") :-
  fronttoken(In,_,_), !.
incasso2OK(_, "          ").

doe_opties(Win,_,_,SKeus,_,_,_,_) :-		% Spsrt
  CtrlWin = win_GetCtlHandle(Win, idca_spelersoort),
  b_True = win_IsChecked(CtrlWin),
  sp2(SKeus,Spelersrt,_,_T1,_,_,_,_,_,_,_, _,_,_,_,_T2, _T3,_,_,_,_,_,_,_,_,_,_),
  spsrt(Spelersrt,Spelersrtnaam,_,_,_,_,_,_),
  write(Spelersrtnaam), 
  fail.
doe_opties(Win,_,_,SKeus,_,_,_,_) :-		% Telefoon
  CtrlWin = win_GetCtlHandle(Win, idca_telefoon),
  b_True = win_IsChecked(CtrlWin),
  sp2(SKeus,_Spelersrt,_,T1,_,_,_,_,_,_,_, _,_,_,_,_, T2, T3,_,_,_,_,_,_,_,_,_),
  writef("\n% %-30s H:%-12s W:%-12s M:%-12s  ","", "",T1,T2,T3),
  fail.
doe_opties(Win,_,_,SKeus,_,_,_,_) :-		% EMail
  CtrlWin = win_GetCtlHandle(Win, idca_email),
  b_True = win_IsChecked(CtrlWin),
  sp2(SKeus,_,_,_,_ ,_,_,_,_,_,_Adres, _WoonPl,_,_,_,_, _,_,EMail,_,_,_,_,_,_,_,_),
  writef("\n% %-30s %s  ","","",EMail),
  fail.
doe_opties(Win,_,_,SKeus,_,_,_,_) :-			% NAW
  CtrlWin = win_GetCtlHandle(Win, idca_adres),
  b_True = win_IsChecked(CtrlWin),
  sp2(SKeus,_,_,_,_ ,_,_,_,_,_,Adres, WoonPl,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  concat(Adres, WoonPl, TotStr),
  fronttoken(TotStr, _, _), 				% test of blanko
  write("\n    ", Adres, "\n    ", WoonPl),
  fail. 
doe_opties(Win,_,_,SKeus,_,_,_,_) :-			% Sterkte, rating, ranking
  CtrlWin = win_GetCtlHandle(Win, idc_sterkte_rating_ranking),
  b_True = win_IsChecked(CtrlWin),
  sp2(SKeus,_,_,_,_ ,RatingE,RatingD,_,_,_Club,_, _,_KNLTB,StE,StD,GebDat, _,_,_,_,_,RankE,RankD,_,_,_,_),
  writef("\n    Sterkte E:%-2 D:%-2  Rating: E:%s D:%s  Ranglijst: E:%s D:%s  Geb.:%", 
                       StE, StD, RatingE, RatingD, RankE, RankD, GebDat),
  fail.
doe_opties(Win,_,_,SKeus,_,_,_,_) :-			% Club + geboortedatum
  CtrlWin = win_GetCtlHandle(Win, idca_knltb_nr),
  b_True = win_IsChecked(CtrlWin),
  sp2(SKeus,_,_,_,_ ,_,_,_,_,Club,_, _,KNLTB,_StE,_StD,_GebDat, _,_,_,_,_,_,_,_,_,_,_),
  get_club(Club, ClubNaam),
  writef("\n    KnltbNr:%-7s  Club:%-5s %-9.16s ", 
                      KNLTB, Club, ClubNaam),
  fail.
doe_opties(Win,_,_VerhinderingenLijst,_,WerkTijd,_,_,_) :-	% verhinderingen
  CtrlWin = win_GetCtlHandle(Win, idca_verhinderingen),
  b_True = win_IsChecked(CtrlWin),
  WerkTijd > 0,
  tijd_str(WerkTijd,TStr,_), 
  writef("\n    Werkdagen om   % uur beschikbaar", TStr),
  fail.
doe_opties(Win,_,VerhinderingenLijst,_,_WerkTijd,_,_,_) :-	% verhinderingen
  CtrlWin = win_GetCtlHandle(Win, idca_verhinderingen),
  b_True = win_IsChecked(CtrlWin),
  prt_verh(VerhinderingenLijst, 0),
  fail.
doe_opties(Win,_,_,SpelerNo,_,_,_,CatNoList) :-     % geplande wedstrijden
  CtrlWin = win_GetCtlHandle(Win, idca_geplande_wedstrijden),
  b_True = win_IsChecked(CtrlWin),
  wd_planned(SPelerNo,WedstrijdList,alleplanning,b_True),
  speeltijden1P(WedstrijdList,Speeltdn,CatNoList),
  qsortwt(Speeltdn,SpeeltdnS),
  prt_speeltP(SpeeltdnS,0,_,ja,1, [], _PrList), 
  fail.
doe_opties(Win,11,_,SpelerNo,_,_,_,_) :- 	      % opgave
  CtrlWin = win_GetCtlHandle(Win, idca_onderdelen_partner),
  b_True = win_IsChecked(CtrlWin),
  nogIn(NogIn),
  %namenSelectieKrit(_,_,_,_,_,_,_,_,_,_,NogIn,_,_,_),
  prt_opgaven(SpelerNo,_,NogIn),
  fail.
doe_opties(Win,Keusno,_,SpelerNo,_,_,_,_) :- 	      % opgave
  not(Keusno = 11),
  CtrlWin = win_GetCtlHandle(Win, idca_onderdelen_partner),
  b_True = win_IsChecked(CtrlWin),
  prt_opgaven(SpelerNo,_,b_false),
  fail.
doe_opties(_,2,_,SpNo,_,_,_,_) :-     % contributie
  contributie(SpNo, Som ,String,[]),
  writef("\n   %-22 eur%6.2f",String,Som),
  fail.
doe_opties(Win,X,_,SpNo,_,_,_,_) :-         % contributie
  X <> 2,
  CtrlWin = win_GetCtlHandle(Win, idca_inschrijfgeld),
  b_True = win_IsChecked(CtrlWin),
  contributie(SpNo, Som, String,[]),
  writef("\n   %-22 eur%6.2f",String,Som),
  fail.
doe_opties(Win,X,_,SpNo,_,_,_,_) :-         % incassonr
  X <> 2,
  CtrlWin = win_GetCtlHandle(Win, idc_incassonr),
  b_True = win_IsChecked(CtrlWin),
  sp2(SpNo,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_Opm,_,_,_,BankNr,_,IncassoOK,_),
  incasso2OK(IncassoOK, IOK),
  write("\n    ", IOK, "  ", BankNr),
  fail.
doe_opties(Win,_,_,SpNo,_,_,_,_) :-           % opmerkingen
  CtrlWin = win_GetCtlHandle(Win, idca_attributen_spelergegevens_opm),
  b_True = win_IsChecked(CtrlWin),
  sp2(SpNo,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,Opm,_,_,_,_BankNr,_,_IncassoOK,_),
  trim_c(Opm),
  formatOpm(Opm, "", "", Uit, "    ", 70),
  write("\n",Uit), !.
doe_opties(_,_,_,_,_,_,_,_).  %(i,i,i,i,i,o,o,i,i,i)

kies_speler2(Name,SpelerNos) :-
  member(No, SpelerNos),
  sp2(No,_,Name,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_).

write_s(Win,KeusNo,SpelerNos,CatNoList) :-	% lijst
  findall(Namex,kies_speler2(Namex,SpelerNos),Spelers),
  sortstring_c(Spelers, 1),
  member(Naam,Spelers),
  sp2(SKeus,Spelersrt,Naam,_Tel,VerhinderingenLijst,_RatingE,_RatingD,Betaald,WerkTijd, _Club, _Adres, _Woonpl, _BondsNr, _ES, _DS, _Gebdatum, _Telwerk, _Telmobiel, _EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Res),
  spsrt(Spelersrt,_,_Sexe,_,_,_,_,[Aanhef|_]),
  naamscan(nee,b_False,Naam,NaamSc,_,_Landcode),
  writef("\n% %s ",Aanhef, NaamSc),
  doe_opties(Win,Keusno,  
         VerhinderingenLijst,
         SKeus,
         WerkTijd, Betaald, Spelersrt,CatNoList),
  fail.
write_s(_Win,_KeusNo,_SpelerNos,_CatNoList).

%_____________________________________________________________
%  daguitslagen
%_____________________________________________________________

database - uitslt
single  voorste(janee)
uitslagdag(integer)
single selectedCats(catl)

predicates
nondeterm  cats_uitslag(string)
  check_catl(slist)
  keuzes2catl(slist, catl)
  check_gespeeld(integerlist)
  write_weds(catl, integer)
procedure dag_plus(integer,string,wrk)
  ans2voorvoeg(integer, janee, string)
  uitslag_dagen(catl, ulist, slist)
  nondeterm dagen2dagenstr(ulist, string)
  dag_min(string, integer)
  qVoornaam(integer, boolean)

clauses
voorste(ja).

weds_uitsl :-
  Win = vpi_GetTaskWin(),
  findall(Catn,cats_uitslag(Catn),Catl),
  check_catl(CatL),
  TaskWin = vpi_GetTaskWin(),
  dlg_onderdelen_select_Create(TaskWin, "Daguitslagen", CatL, CatLS),
  not(CatLS = []),
  keuzes2catl(CatLS, NewCatL),
  uitslag_dagen(NewCatL, _, Dagen),
  dlg_ListSelect("Uitslagen binnengekomen op:",Dagen,1,DagS,_Indexx),
  not(DagS = ""),
  dag_min(DagS, D),
  findall(Gesp, gespeeld(NewCatL, D, Gesp, _), Gespeelden),
  check_gespeeld(Gespeelden),
  cursor_SetWait(),
  closefile(work),
  get_TempDir(_, TempFile),
  openwrite(work,TempFile),
  writedevice(work),
  write_weds(NewCatL, D),
  writedevice(screen),
  cursor_Set(Win, cursor_Arrow),
  closefile(work), !,
  dlg_rapport_Create(TaskWin,"Daguitslagen","Krant",b_False).
weds_uitsl :- 
  closefile(work).
	
cats_uitslag(Catname) :-
  wc(Cat, _, _, CatName, _, _, _, _, _, _, _, _, _, _, _),
  getbacktrack(Here),
  w3(_, Cat, _, _,_,_,_),
  cutbacktrack(Here).
  
uitslag_dagen(CatL, _, _) :-
  member(Cat, CatL),
  w3(_, Cat, _, _,_,_,Tijd),
  bitright(Tijd, 7, D),
  not(uitslagdag(D)),
  assert(uitslagdag(D)),
  fail.
uitslag_dagen(_, Dagen, DagenS) :-
  findall(D, uitslagdag(D), Dagen),
  sortuint_c(Dagen),
  findall(DagS, dagen2dagenstr(Dagen, DagS), DagenS),
  retractall(uitslagdag(_)).

dagen2dagenstr([0|_], "Voor of na het toernooi").
dagen2dagenstr([Dag|_], DagS) :-
  Dag > 0,
  dt_dateoffset_to_str(Dag, "%Dn %DD-%MD-%YL", DagS).
dagen2dagenstr([_|Rest], DagS) :-
  dagen2dagenstr(Rest, DagS).   

check_catl([]) :- !,
  dlg_Note("Uitslagen","Niets te melden"),
  fail.
check_catl(_).

check_gespeeld([]) :- !,
  dlg_Note("Uitslagen","Niets te melden op deze dag"),
  fail.
check_gespeeld(_).

keuzes2catl([Catname|CatL], [CatNo|Out]) :-
  wc(CatNo, _, _, Catname, _WTyp, _, _SexList, _, _, _, _, _, _, _, _),!,
  keuzes2catl(CatL, Out).
keuzes2catl([], []).

gespeeld(NewCatL, Dag, Num, Cat) :-
  member(Cat, NewCatL),
  w3(Num, Cat, _, _, _, _, Tijd),
  bitright(Tijd, 7, Tijd1),
  Tijd1 = Dag.

ans2voorvoeg(resp_default, ja, " ").
ans2voorvoeg(resp_default, nee, ",").
ans2voorvoeg(resp_2, _, "\n").

qVoornaam(resp_default, b_True) :- !.
qVoornaam(_, b_False).

write_weds(CatL, Dag) :-
  dag_plus(Dag, DagNaam, _),
  trim_c(DagNaam),
  Ans = dlg_Ask("Per onderdeel op één regel?",["&Ja","&Nee"]),
  Voornaam = dlg_Ask( "Voornaam voluit of voorletter?\n\nAttentie: is een nieuwe optie!",["&Voorletter", "&Voornaam"]),
  format(Text, "Uitslagen van: %\.", DagNaam),
  write(Text, '\n'),
  str_len(Text, Len),
  strepen(Len, '_'), nl,
  member(Cat, CatL),
  wc(Cat, _, _, _, _, CatS, _, _, _, _, _, _, _, _, _),
  trim_c(CatS),
  findall(Num, gespeeld([Cat], Dag, Num, _), Nummers),
  not(Nummers = []),
  concat(CatS, ":", CatS1),
  assert(voorste(ja)),
  write("\n\n", CatS1),
  sortweds_c(Nummers),
  member(Num1, Nummers),
  voorste(First),
  ans2voorvoeg(Ans, First, Voorvoeg),
  qVoornaam(Voornaam, AlleenVoorletter),
  assert(voorste(nee)),
  rep_wed2(Voorvoeg, AlleenVoorletter, Num1, Cat, Plat, _Html),
  write(Plat),
  fail.
write_weds(_, _) :-
  write("\12").

dag_plus(N, DagNaam, w) :-
  N > 0, !,
  dt_dateoffset_to_str(N, "%Dn %DD-%MD-%YL", DagNaam).
dag_plus(_, "Voor of na het toernooi", w).

dag_min("Voor of na het toernooi", 0) :- !.
dag_min(DagNaam, N) :-
  dt_datestr_to_offset(DagNaam, "%Dn %DD-%MD-%YL", N).

rep_wed2(Voorvoeg, AlleenVoorletter, WdNo,Cat, Plat, Html) :-
  wd(WdNo,Cat,T1,T2,Uitsl,_,_),
  w3(WdNo, Cat, T3, _, _, _, _), 
  not(T3 = gelijk_spel),
  winnaar(T1, T2, T3, T4, T5,0,0,_,_),
  repa(ja,'}',nee, nee,nee,0,2,1,WdNo,Cat,T4,alleplanning,TS1,_Txx,_TijdLogxx,AlleenVoorletter/*!!*/, b_False,_,_,_Naam1,_Naam2, _Nr1, _Nr2,_),
  repa(ja,'}',nee, nee,nee,1,2,1,WdNo,Cat,T5,alleplanning,TS2,_,_,AlleenVoorletter/*!!*/, b_False,_,_,_,_, _, _, _),
  scan(TS1, TS1sc, "/"),
  scan(TS2, TS2sc, "/"), !,
  scan(TS1, TS1scH, "<br>"),
  scan(TS2, TS2scH, "<br>"), !,
  format(Plat, "%s%s-%s %s", Voorvoeg, TS1sc, TS2sc, Uitsl),
  format(Html, "<TR>%s<TD>%s</TD><TD>%s</TD><TD>%s</TD></TR>\n", Voorvoeg, TS1scH, TS2scH, Uitsl).
rep_wed2(Voorvoeg, AlleenVoorletter, WdNo,Cat, Plat, Html) :-
  wd(WdNo,Cat,T1,T2,Uitsl,_,_),
  w3(WdNo, Cat, T3, _, _, _, _),
  T3 = gelijk_spel, 
  repa(ja,'}',nee, nee,nee,0,2,1,WdNo,Cat,T1,alleplanning,TS1,_,_,AlleenVoorletter/*!!*/, b_False,_,_,_,_, _, _,_),
  repa(ja,'}',nee, nee,nee,1,2,1,WdNo,Cat,T2,alleplanning,TS2,_,_,AlleenVoorletter/*!!*/, b_False,_,_,_,_, _, _,_),
  scan(TS1, TS1sc, "/"),
  scan(TS2, TS2sc, "/"), !,
  scan(TS1, TS1scH, "<br>"),
  scan(TS2, TS2scH, "<br>"), !,
  format(Plat, "%s%s-%s %s", Voorvoeg, TS1sc, TS2sc, Uitsl), !,
  format(Html, "<TR>%s<TD>%s</TD><TD>%s</TD><TD>%s</TD></TR>\n", Voorvoeg, TS1scH, TS2scH, Uitsl).
rep_wed2(Voorvoeg, AlleenVoorletter, WdNo,Cat, Plat, Html) :-	% 24.9.2003
  wd(WdNo,Cat,T1,T2,Uitsl,_,_),
  repa(ja,'}',nee, nee,nee,0,2,1,WdNo,Cat,T1,alleplanning,TS1,_,_,AlleenVoorletter/*!!*/, b_False,_,_, _Naam1,_Naam2, _Nr1, _Nr2,_),
  repa(ja,'}',nee, nee,nee,1,2,1,WdNo,Cat,T2,alleplanning,TS2,_,_,AlleenVoorletter/*!!*/, b_False,_,_,_,_, _, _,_), !,
  scan(TS1, TS1sc, "/"),
  scan(TS2, TS2sc, "/"), !,
  scan(TS1, TS1scH, "<br>"),
  scan(TS2, TS2scH, "<br>"), !,
  format(Plat, "%s%s-%s %s", Voorvoeg, TS1sc, TS2sc, Uitsl),
  format(Html, "<TR>%s<TD>%s</TD><TD>%s</TD><TD>%s</TD></TR>\n", Voorvoeg, TS1scH, TS2scH, Uitsl).

winnaar(T1, T2, T1, T1, T2, P1, P2, P1, P2) :- !.
winnaar(T1, T2, T2, T2, T1, P1, P2, P2, P1) :- !.
winnaar(_, _, _, _, _, _, _, _, _) :- beep, fail.

scan(In, Out, Delim) :-
  trim_c(In),
  searchchar(In,'+',Pos),
  Pos > 2,
  str_len(In, Len),
  L1 = Pos - 1,
  L2 = Len - Pos,
  P1 = Pos + 1, 
  substring(In, 1, L1, In1),
  substring(In, P1, L2, In2),
  scan(In1, Out1, Delim),
  scan(In2, Out2, Delim),
  format(Out, "%s%s%s", Out1, Delim, Out2), !.
scan(A,A,_Delim). 

%_________________________________________________________________________
%   plan_nieuws
%_________________________________________________________________________
  
predicates

  ronde_tekst1(rsoort, ronde_dom, string)
nondeterm plan_nieuw(string)
nondeterm planning(catl, integer, integer)  
  check_plannen(integerlist)
  check_plan_nieuw(slist)
  write_plans(catl, integer)
  rep_plan(integer, wedscat) 
  add_baan1(janee, string, string)
  vaststel_dagen(catl, ulist, slist)

clauses

vaststel_dagen(CatL, _, _) :-
  member(Cat, CatL),
  w2(_,Cat,_,_,_,_,Tijd,_),  
  bitright(Tijd, 7, D),
  not(uitslagdag(D)),
  assert(uitslagdag(D)),
  fail.
vaststel_dagen(_, Dagen, DagenS) :-
  findall(D, uitslagdag(D), Dagen),
  sortuint_c(Dagen),
  findall(DagS, dagen2dagenstr(Dagen, DagS), DagenS),
  retractall(uitslagdag(_)).

plan_nieuw(Catname) :-
  wc(Cat, _, _, Catname, _, _, _, _, _, _, _, _, _, _, _),
  getbacktrack(Here),
  w2(_, Cat, _, _, _, _, _Tijd, _),
  cutbacktrack(Here).

planning(NewCatL, Dag, WdNo) :-
  member(Cat, NewCatL),
  w2(WdNo,Cat,_,_,_,_,Tijd,_),  
  bitright(Tijd, 7, Tijd1),
  Tijd1 = Dag.

check_plan_nieuw([]) :- !,
  dlg_Note("Planning","Niets te melden"),
	fail.
check_plan_nieuw(_).

check_plannen([]) :- !,
  dlg_Note("Planning","Niets te melden"),
  fail.
check_plannen(_).

write_plans(CatL, Dag) :-
  member(Cat, CatL),
  wc(Cat, _, _, CatS, _, _, _, _, _, _, _, _, _, _, _),
  %wc(Cat,CatS,_,_,_,_,_),
  concat(CatS, ":", CatS1),
  str_len(CatS1, L1),
  write("\n\n", CatS, '\n'),
  strepen(L1, '_'), 
  findall(Num, planning([Cat], Dag, Num), Nummers),
  sortweds_c(Nummers),
  member(Num1, Nummers),
  rep_plan(Num1, Cat),
  fail.
write_plans(_, _) :-
  write("\12").

add_baan1(nee, _, "") :- !.
add_baan1(ja, "", "") :- !.
add_baan1(_, In, Uit) :-
  format(Uit, " op baan %s", In).

rep_plan(WdNo, Cat) :- 
  w2(WdNo,Cat,Tijd1,J1,J2,Geforceerd,_,Baan), 
  pos(Cat,Card,_,AfvalOfPoel), !,
  ronde(WdNo, Card, Ronde),
  ronde_tekst1(AfvalOfPoel, Ronde, Plaats),
  y_coord(WdNo, Card, Y_Coordinaat),
  tijd_kwart(D1,U1,M1,Tijd1),
  dag_plus(D1,DS1,_), !,
  helestr_int(0,MS1,M1), 
  baan_adm(C),
  add_baan1(C, Baan, BaanT),
  write("\nGepland voor ",DS1,"  ",U1,".",MS1,BaanT,":"),
  wd(WdNo,Cat,T1,T2,_,_,_),
%	gepland(WdNo,Cat,J1,J2,Geforceerd, Tijd), 
  oproepen(W), 
  betaal_adm(B), !,
  gewaarschuwd(T1,W,J1,JS1),
  gewaarschuwd(T2,W,J2,JS2),
  repc(nee, '}',nee, W,B,0,2,1,WdNo,Cat,T1,alleplanning,TS1,_Scorex, _TtijdChangedx,_,_,_,_),
  repc(nee, '}',nee, W,B,1,2,1,WdNo,Cat,T2,alleplanning,TS2,_, _, _, _, _, _),
  writef("\n regel %-3d %-9s %c %1 %-20.20 - %1 %-20.20", Y_Coordinaat, Plaats, 
				Geforceerd,JS1,TS1,JS2,TS2).

plan_nieuws() :-
  findall(Catn,plan_nieuw(Catn),Catl),
  check_plan_nieuw(CatL),
  TaskWin = vpi_GetTaskWin(),
  dlg_onderdelen_select_Create(TaskWin, "Daguitslagen", CatL, CatLS),
  not(CatLS = []),
  keuzes2catl(CatL, NewCatL),
  vaststel_dagen(NewCatL, _, Dagen),
  dlg_ListSelect("Dag",Dagen,0,DagS,_Indexx),
  not(Dags = ""),
  dag_min(DagS, D),
  findall(Plan, planning(NewCatL, D, Plan), Plannen),
  check_plannen(Plannen), !,
  closefile(work),
  get_TempDir(_, TempFile),
  openwrite(work,TempFile),
  writedevice(work),
  trim_c(DagS),
  format(Text, "Nieuwe planning, vastgesteld op: %\.", DagS),
  write(Text, '\n'),
  str_len(Text, Len),
  strepen(Len, '_'), nl,
  write_plans(NewCatL, D),
  writedevice(screen),
  closefile(work),
  dlg_rapport_Create(TaskWin,"Nieuwe planning","Plannen",b_False).
plan_nieuws().

ronde_tekst1(afval,kwartfin,"kw fin") :- !.
ronde_tekst1(afval,halvefin,"h  fin") :- !.
ronde_tekst1(afval,finale,  "finale") :- !.
ronde_tekst1(afval,r(No), Tekst) :- !,
  format(Tekst, "%de rnd", No).
ronde_tekst1(_,_, "Poule") :- !.

%_______________________________________________________________________
% lijsten en mailmerge
%_______________________________________________________________________

%BEGIN_DLG Onderdeelspelergegevens
/**************************************************************************
	Creation and event handling for dialog: Onderdeelspelergegevens
**************************************************************************/

constants

%BEGIN Onderdeelspelergegevens, CreateParms, 14:34:44-4.9.2002, Code automatically updated!
  dlg_onderdeelspelergegevens_DlgType = wd_Modal
  dlg_onderdeelspelergegevens_Title = "Selecteer onderdelen voor lijst"
  dlg_onderdeelspelergegevens_RCT = rct(40,32,192,217)
  dlg_onderdeelspelergegevens_Flags = [wsf_Close,wsf_TitleBar]
  dlg_onderdeelspelergegevens_Help = idh_lijst_per_onderdeel
%END Onderdeelspelergegevens, CreateParms
my_lijst = 1
my_mailmerge = 2

predicates

  dlg_onderdeelspelergegevens_eh : EHANDLER
nondeterm  opg_catP(string)
  in_opg(tgnst)
  procedure write_s1(integer LijstOfMM, WINDOW,integerlist,catl)
  prt_speeltM(wtl, integer)
nondeterm partnersP(integer, catl, string)

clauses

selectedCats([]).

onderdeelMM(Win) :-
  findall(Text,opg_catP(Text),In),       
  dlg_onderdelen_select_Create(Win, "Oproep/betaal mailmergeselectie", In, Out),
  not(Out = []),
  keuzes2catl(Out, CatNoList),
  assert(selectedCats(CatNoList)),
  findall(Speler, selected_sp_in_cats(_, Speler), Spelers),
  retractall(sp_sel(_)),
  getfolder(TDir,_,_,_),
  filenamePath(Brief, TDir, briefconst),
  _FILENAME = dlg_GetFileName(Brief,["txt","*.txt","dat","*.dat"],"Mailmerge(samenvoeg) bestand",[dlgfn_Save],"",_OutListFiles),
  filenamepath(_FILENAME, _Path, Name),
  Name <> fctconst,
  Name <> logconst,
  closefile(work),
  openwrite(work, _FILENAME),
  writedevice(work),
  write_s1(my_mailmerge,Win,Spelers,CatNoList), 
  closefile(work),
  writef("\nOproep/betaal mailmergebestand aangemaakt onder de naam %s",_FILENAME),
  format(Tekst, "Bestand aangemaakt onder de naam %s",_FILENAME),
  dlg_Note("Oproep/betaal brieven",Tekst), 
  !.
  
prt_speeltM([],20) :- !,	% 6.11.2000 8 -> 20
  write(csvconst).		% geen overflow
prt_speeltM(_, 20) :- !,
  write("\"!teveel!\"",csvconst).	% overflow
prt_speeltM([],N) :-	% vul aan met komma'S tot 8
  write(csvconst),
  N1 = N + 1, !,
  prt_speeltM([], N1).
prt_speeltM([wt(Type,Tijd,_,_,_BW, _)|T],N) :-
  tijd_kwart(D1,U1,M1,Tijd),
  dag(D1,DS1,_), 
  helestr_int(0,MS1,M1),
  writef("\"%-4 %-7 %2\46%2\"%c",Type,DS1,U1,MS1,csvconst),
  N1 = N + 1, !,
  prt_speeltM(T, N1).

selected_sp_in_cats( _, _) :-
  selectedCats(CatNoList),
  retractall(sp_Sel(_)),
  member(CatN, CatNoList),
  opg(_, CatN, Tgnst, _, _, _, _, _, _, _, _),
  in_opg(Tgnst),
  fail.
selected_sp_in_cats(Naam, Sp) :-
  retract(sp_sel(Sp)),
  getbacktrack(Here),
  sp2(Sp,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  cutbacktrack(Here).

in_opg(p(T1,_)) :-
  not(sp_sel(T1)),
  assert(sp_sel(T1)),
  fail.
in_opg(p(_,T2)) :-
  not(sp_sel(T2)),
  assert(sp_sel(T2)), !.
in_opg(s(T)) :-
  not(sp_sel(T)),
  assert(sp_sel(T)).
in_opg(pw(T)) :-
  not(sp_sel(T)),
  assert(sp_sel(T)).

opg_catP(N1) :-
  wc(WCat, _, _, N1, _, _, _, _, _, _, _, _, _, _, _),
  pos(WCat,_,_,_). 

partnersP(SKeus, CatNoList, Partner) :-
  sp2opg(SKeus,Cat,_,MaatNo,_,_),
  MaatNo > 0,
  wc(Cat, _, _, _, _, Abbr, _, _, _, _, _, _, _, _, _Geld),
  member(Cat, CatNoList),
  sp2(MaatNo,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  format(Partner, "%s met %s", Abbr, Naam).

telefono([""|R], Uit) :-
  telefono(R, Uit), !.
telefono([T|R],Uit) :-
  telefono(R, Uit1),
  concat(T, " ", T1),
  concat(T1, Uit1, Uit), !. 
telefono(_,"").

write_s1(my_mailmerge, _Win,SpelerNos,CatNoList) :-
  findall(Namex,kies_speler2(Namex,SpelerNos),Spelers),
  sortstring_c(Spelers, 1),
  write("mv",csvconst,"naam",csvconst,"adres",csvconst,"woonpl",csvconst,"lidnr",csvconst,"telefoon",csvconst,"email",csvconst,"oproep1",csvconst,"oproep2",csvconst,"oproep3",csvconst,"oproep4",csvconst,"oproep5",csvconst,"oproep6",csvconst,"oproep7",csvconst),
  write("oproep8",csvconst,"oproep9",csvconst,"oproep10",csvconst,"oproep11",csvconst,"oproep12",csvconst,"oproep13",csvconst,"oproep14",csvconst,"oproep15",csvconst,"oproep16",csvconst,"oproep17",csvconst,"oproep18",csvconst,"oproep19",csvconst,"oproep20",csvconst,"oprpteveel",csvconst,"deelname",csvconst,"bedrag",csvconst,"partner1",csvconst,"partner2",csvconst,"partner3",csvconst,"partner4\n"),
  member(Naam,Spelers),
  sp2(SKeus,SpelerSrt,Naam,Telefoon,_,_RE,_RD,_Bet,_Uit, _, Adres, Woonpl, LidNr, _ES, _DS, _Gebdatum, Tel1, Tel2, EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Res),
  spsrt(Spelersrt,_,_Sexe,_,_,_,_,[Aanhef|_]),
  naamscan(ja,b_False,Naam, Naam1,_,_Landcode),
  format(TelefoonT, "%s %s %s", Telefoon, Tel1, Tel2),
  trim_c(TelefoonT),
  Csvconst = csvconst,
  writef("\"%\"%c\"%\"%c\"%\"%c\"%\"%c\"%s\"%c\"%s\"%c\"%s\"%c", 
    Aanhef, Csvconst,Naam1, Csvconst,Adres,Csvconst,WoonPl,Csvconst,Lidnr,Csvconst,TelefoonT,Csvconst,Email,Csvconst),
  findall(Partner, partnersP(SKeus, CatNoList, Partner), PartnerL),
  wd_planned(SKeus,WedstrijdList,alleplanning,b_True),	% geen planning onderdrukken
  speeltijden1P(WedstrijdList,Speeltdn,CatNoList),
  qsortwt(Speeltdn,SpeeltdnS),
  prt_speeltM(SpeeltdnS,0),
  contributie(SKeus, Contr, ContrS, CatNoList),
  trim_c(ContrS),
  writef("\"%s\"%c\"eur%7.2f\"",ContrS,Csvconst, Contr),
  prt_partnermm(PartnerL),
  nl,
  fail.
write_s1(_, _Win,_SpelerNos,_CatNoList).


  dlg_onderdeelspelergegevens_Create(Parent):-
	win_CreateDynDialog(Parent,
		[
%BEGIN Onderdeelspelergegevens, WinDefList, 14:34:44-4.9.2002, Code automatically updated!
		 dlg(wdef(dlg_onderdeelspelergegevens_DlgType,dlg_onderdeelspelergegevens_RCT,dlg_onderdeelspelergegevens_Title,u_DlgBase),dlg_onderdeelspelergegevens_Flags),
		 ctl(wdef(wc_LBox,rct(4,2,148,157),"",u_DlgBase),idco_lbond,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_MultiSelect]),
		 ctl(wdef(wc_PushButton,rct(116,159,148,181),"&OK",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(78,159,110,169),"&Cancel",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(78,171,110,181),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop])
%END Onderdeelspelergegevens, WinDefList
		],dlg_onderdeelspelergegevens_eh,0).

%BEGIN Onderdeelspelergegevens, idc_ok _CtlInfo
  dlg_onderdeelspelergegevens_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-!,
	OndWin = win_GetCtlHandle(_Win, idco_lbond),
	lbox_GetSel(OndWin,ItemList,_IndexList),
	keuzes2catl(ItemList, CatNoList),
	assert(selectedCats(CatNoList)),
	TaskWin = vpi_GetTaskWin(),
	win_spelersel_Create(TaskWin, 10),
	spelerSelRefresh(),
	dlg_MessageBox( "Spelerlijst", "Gebruik het menu-coon in de spelerslijst\nom de gewenste lijst te produceren!", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	%rap_list1(_Win),
	win_Destroy(_Win),
	!.
%END Onderdeelspelergegevens, idc_ok _CtlInfo
%MARK Onderdeelspelergegevens, new events

%BEGIN Onderdeelspelergegevens, idc_help _CtlInfo
  dlg_onderdeelspelergegevens_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Lijstperonderdeel"),
	!.
%END Onderdeelspelergegevens, idc_help _CtlInfo

%BEGIN Onderdeelspelergegevens, idco_lbond selchanged
  dlg_onderdeelspelergegevens_eh(_Win,e_Control(idco_lbond,_CtrlType,_CtrlWin,selchanged),0):-!,
	!.
%END Onderdeelspelergegevens, idco_lbond selchanged

%BEGIN Onderdeelspelergegevens, idc_cancel _CtlInfo
  dlg_onderdeelspelergegevens_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	win_Destroy(_Win),
	!.
%END Onderdeelspelergegevens, idc_cancel _CtlInfo

%BEGIN Onderdeelspelergegevens, e_Create
  dlg_onderdeelspelergegevens_eh(_Win,e_Create(_CreationData),0):-!,
	findall(Text,opg_catP(Text),SList),       
	LboxWin = win_GetCtlHandle(_Win, idco_lbond),
	lbox_Add(LboxWin, SList),
	!.
%END Onderdeelspelergegevens, e_Create

  dlg_onderdeelspelergegevens_eh(_,_,_):-!,fail.

%END_DLG Onderdeelspelergegevens

%_________________________________________________________________________
%  etiketten
%_________________________________________________________________________

predicates
  write_s_etik(WINDOW, integerlist, catl, integer Optie)
  etik_opt(integer, integer, catl, slist)

  
clauses

onderdeelEtik(Win, Optie) :-		% Optie 5 = oproep  Optie 7 = betaling
  findall(Text,opg_catP(Text),SList),       
  dlg_onderdelen_select_Create(Win, "Etiketten per onderdeel", Slist, ItemList),
  cursor_SetWait(),
  keuzes2catl(ItemList, CatNoList),
  assert(selectedCats(CatNoList)),
  findall(Speler, selected_sp_in_cats(_, Speler), Spelers),
  retractall(sp_sel(_)),
  closefile(work),
  get_TempDir(_, TempFile),
  openwrite(work,TempFile),
  writedevice(work),
  write_s_etik(Win,Spelers,CatNoList,Optie), 
  closefile(work),
  dlg_etiketten_print_Create(Win, Answer),  %dlg_rapport_Create(Win, "Oproepetiketten"),
  printetiketten(Answer, nee),
  !.

write_s_etik(_Win,SpelerNos,CatNoList,Optie) :-	% etiket
  etik_reset(),
  findall(Namex,kies_speler2(Namex,SpelerNos),Spelers),
  sortstring_c(Spelers, 1),
  member(Naam,Spelers),
  sp2(SKeus,Spelersrt,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  spsrt(Spelersrt,_,_Sexe,_,_,_,_,[Aanhef|_]),
  naamscan(ja,b_False,Naam, Naam1,_,_Landcode),
  format(Naamveld, "% %", Aanhef, Naam1),
  etik_opt(Optie, SKeus, CatNoList, List1), 
  append([NaamVeld], List1, Label),
  etiketBuild(Label),
  fail.
write_s_etik(_Win,_,_,_).

etik_opt(5, SKeus, CatNoList, PrList) :- 
  wd_planned(SKeus,WedstrijdList,alleplanning,b_True),
  speeltijden1P(WedstrijdList,Speeltdn,CatNoList),
  qsortwt(Speeltdn,SpeeltdnS),
  prt_speeltP(SpeeltdnS,0,_,ja,2, [], PrList).
etik_opt(7, SKeus, CatNoList, ["", Str]) :-
  contributie(SKeus, Contr, ContrS, CatNoList),
  format(Str,"   %-16 eur%7.2f",ContrS, Contr).

%BEGIN_DLG Attributen spelerlijst
/**************************************************************************
	Creation and event handling for dialog: Attributen spelerlijst
**************************************************************************/

constants

%BEGIN Attributen spelerlijst, CreateParms, 22:03:57-27.10.2011, Code automatically updated!
  dlg_attributen_spelerlijst_ResID = idd_afdruk_van_spelerlijst
  dlg_attributen_spelerlijst_DlgType = wd_Modal
  dlg_attributen_spelerlijst_Help = idh_afdruk_van_spelergegevens
%END Attributen spelerlijst, CreateParms

database - lijst
single lijstdata(integerlist SpelerNummers, string Titel, integer Mode)
%single optiesL(boolean, boolean, boolean, boolean,boolean, boolean, boolean, boolean)

predicates

  dlg_attributen_spelerlijst_eh : EHANDLER
  dlg_attributen_spelerlijst_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_attributen_spelerlijst_update(DIALOG_VAL_LIST)

clauses

lijstdata([], "", 0).

iniOptiesL(X, SPELERSOORT,ADRES,TELEFOON,EMAIL,KNLTBNR,VERHINDERINGEN,ONDERDELEN,INSCHRIJFGELD,INCASSO,PLANNING,OPMERKINGEN, Sterkte) :-
  optiesL(X, SPELERSOORT,ADRES,TELEFOON,EMAIL,KNLTBNR,VERHINDERINGEN,ONDERDELEN,INSCHRIJFGELD,INCASSO, PLANNING,OPMERKINGEN,Sterkte), !.
iniOptiesL(_, b_False, b_False, b_False, b_False,b_False, b_False, b_False, b_False, b_False, b_False, b_False, b_False).

  dlg_attributen_spelerlijst_Create(Parent, SpelerNos, Titel, Mode):-
	assert(lijstdata(SpelerNos, Titel, Mode)),
	iniOptiesL(Mode, SPELERSOORT,ADRES,TELEFOON,EMAIL,KNLTBNR,VERHINDERINGEN,ONDERDELEN,INSCHRIJFGELD,INCASSO,PLANNING,OPMERKINGEN,IDC_STERKTE_RATING_RANKING_CHECKED),
%MARK Attributen spelerlijst, new variables

	dialog_CreateModal(Parent,dlg_attributen_spelerlijst_ResID,"",
  		[
%BEGIN Attributen spelerlijst, ControlList, 22:03:57-27.10.2011, Code automatically updated!
		df(idca_spelersoort,checkbox(SPELERSOORT),nopr),
		df(idca_adres,checkbox(ADRES),nopr),
		df(idca_telefoon,checkbox(TELEFOON),nopr),
		df(idca_email,checkbox(EMAIL),nopr),
		df(idc_sterkte_rating_ranking,checkbox(IDC_STERKTE_RATING_RANKING_CHECKED),nopr),
		df(idca_knltb_nr,checkbox(KNLTBNR),nopr),
		df(idca_verhinderingen,checkbox(VERHINDERINGEN),nopr),
		df(idca_geplande_wedstrijden,checkbox(PLANNING),nopr),
		df(idca_onderdelen_partner,checkbox(ONDERDELEN),nopr),
		df(idca_inschrijfgeld,checkbox(INSCHRIJFGELD),nopr),
		df(idc_incassonr,checkbox(INCASSO),nopr),
		df(idca_attributen_spelergegevens_opm,checkbox(OPMERKINGEN),nopr)
%END Attributen spelerlijst, ControlList
		],
		dlg_attributen_spelerlijst_eh,0,VALLIST,ANSWER),
	dlg_attributen_spelerlijst_handle_answer(ANSWER,VALLIST).

  dlg_attributen_spelerlijst_handle_answer(idc_ok,VALLIST):-!,
	dlg_attributen_spelerlijst_update(VALLIST).
  dlg_attributen_spelerlijst_handle_answer(idc_cancel,_):-!.  % Handle Esc and Cancel here
  dlg_attributen_spelerlijst_handle_answer(_,_):-
	errorexit().

  dlg_attributen_spelerlijst_update(_VALLIST):-
%BEGIN Attributen spelerlijst, Update controls, 22:03:57-27.10.2011, Code automatically updated!
	_SPELERSOORT = dialog_VLGetCheck(idca_spelersoort,_VALLIST),
	_ADRES = dialog_VLGetCheck(idca_adres,_VALLIST),
	_KNLTBNR = dialog_VLGetCheck(idca_knltb_nr,_VALLIST),
	_VERHINDERINGEN = dialog_VLGetCheck(idca_verhinderingen,_VALLIST),
	_ONDERDELEN = dialog_VLGetCheck(idca_onderdelen_partner,_VALLIST),
	_INSCHRIJFGELD = dialog_VLGetCheck(idca_inschrijfgeld,_VALLIST),
	_PLANNING = dialog_VLGetCheck(idca_geplande_wedstrijden,_VALLIST),
	_OPMERKINGEN = dialog_VLGetCheck(idca_attributen_spelergegevens_opm,_VALLIST),
	_EMAIL = dialog_VLGetCheck(idca_email,_VALLIST),
	_TELEFOON = dialog_VLGetCheck(idca_telefoon,_VALLIST),
	_IDC_STERKTE_RATING_RANKING_CHECKED = dialog_VLGetCheck(idc_sterkte_rating_ranking,_VALLIST),
	_INCASSO = dialog_VLGetCheck(idc_incassonr,_VALLIST),
%END Attributen spelerlijst, Update controls
	lijstdata(_SpelerNos, _Titel,Mode),
	retractall(optiesL(Mode, _,_,_,_,_,_,_,_,_,_,_,_)),
	assert(optiesL(Mode, _SPELERSOORT,_ADRES,_TELEFOON,_EMAIL,_KNLTBNR,_VERHINDERINGEN,_ONDERDELEN,_INSCHRIJFGELD,_INCASSO,_PLANNING,_OPMERKINGEN, _IDC_STERKTE_RATING_RANKING_CHECKED)),
	true.

%MARK Attributen spelerlijst, new events

%BEGIN Attributen spelerlijst, idc_ok _CtlInfo
  dlg_attributen_spelerlijst_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	lijstdata(SpelerNos, Titel,Mode),
	closefile(work),
	vpi_ProcessEvents(),
	get_TempDir(_, TempFile),
	openwrite(work,TempFile),
	writedevice(work),
	write("\n", Titel),  
	write_s(_Win,Mode,SpelerNos,[]), 
	write("\n\12"),
	closefile(work),
	TaskWin = vpi_GetTaskWin(),
	dlg_rapport_Create(TaskWin, Titel,"Spelerlijst",b_False),
	fail.
%END Attributen spelerlijst, idc_ok _CtlInfo

  dlg_attributen_spelerlijst_eh(_,_,_):-!,fail.

%END_DLG Attributen spelerlijst



