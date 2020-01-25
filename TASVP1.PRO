/*****************************************************************************

		Copyright (c) 1989 - 1998 De Lint Associates

 Project:  TASVPI
 FileName: TASVP1.PRO
 Purpose: Main module
 Written by: JdL
 Comments: 
******************************************************************************/

include "tasvp1.inc"
include "tasvp1.con"
include "hlptopic.con"
%include "treeview\\treeview.ph"

constants

fudgefactor = 10
portret = 0
landschap = 1
kopies = 1
id_Speler_betaald_reset = 2015


database - onderdelens

teprinten(integer Wdscat, integerlist Hoogtes, integer Blz, rsoort AfvalPoel, BOOLEAN PrRichting)
single emptyjob(BOOLEAN)
single jobstarted(BOOLEAN)
%single printwindow(WINDOW)
determ skip_post()
determ timerid1(integer, integer Aantal)
determ timerid2(integer)
determ uploadEvents(integer)
single eerstekeer(boolean)
single dlsize(integer)
single dlretcode(integer)
%######################################## sorteren

domains
  treex = reference t(val, treex, treex)
  val  = integer
  list = integer*
  resp = ok(string Msg, string Html) ; nok(string Msg, string Html)

predicates
%  insert(integer,treex)
%  instree(list,treex)
%  nondeterm treemembers(integer,treex)
%  sortcats(list,catl)
  %etiketinit()
  sumList(integerlist, integer, integer)
%  switchDiskletter(string, string)
  %procedure trycreatedir(string)
%  procedure switch_licentie(string)
%  handlecontact(integer Resp, string)
  determ confail(integer, string)
  %determ ifTempdir(string) % de tempdir moet je niet hebben...
  
clauses

eerstekeer(b_True).
dlsize(0).
dlretcode(0).

sumList([],In,In) :- !.
sumList([H|T], In, Uit) :-
  In1 = In + H,
  sumList(T, In1, Uit). 
toernooiDirReset() :-
  assert(eerstekeer(b_True)).


confail(Nummer, String) :-
  webdir(WD, inet, TB),
  format(Msg, "on-line verbinding mislukt.\nProbeer opnieuw toernooiselectie of probeer later opnieuw.\n%d\n%s", Nummer, String),
  dlg_MessageBox( "Webmap", Msg, mesbox_iconerror, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  assert(webdir(WD, disk, TB)),	% 31.8.2008 schakel om naar disk
  fail.

handleContact(RC, _) :-
  RC <> 0,
  webdir(_, disk, _), !,
  fail.
handleContact(0, Str) :-
  trap(term_str(resp, Term, Str), _, confail(1, Str)),
  Term = ok(Str1, _),
  fronttoken(Str1, _, _),
  dlg_MessageBox( "ToernooiAssistent", Str1, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.
handleContact(0, Str) :-
  trap(term_str(resp, Term, Str), _, confail(2, Str)),
  Term = ok(_, Str2),
  fronttoken(Str2, _, _),
  get_tempdir(Tempdir, _),
  filenamepath(HtmlFile, Tempdir, "Resp.html"),
  deletefile(HtmlFile),
  file_str(HtmlFile, Str2),
  format(URL, "file:///%s", HtmlFile),
  Null = cast(api_hwnd,0),
  api_ShellExecute(Null,"open",URL,"","",1),		% open in browser
  %TaskWin = vpi_GetTaskWin(),
  %dlg_browser_Create(TaskWin, URL),
  fail.
handleContact(0, Str) :-
  trap(term_str(resp, Term, Str), _, confail(3, Str)),
  Term = nok(Str1, _),
  fronttoken(Str1, _, _),
  dlg_MessageBox( "ToernooiAssistent", Str1, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.
handleContact(0, Str) :-
  trap(term_str(resp, Term, Str), _, confail(4, Str)),
  Term = nok(_, Str2),
  fronttoken(Str2, _, _),
  get_tempdir(Tempdir, _),
  filenamepath(HtmlFile, Tempdir, "Resp.html"),
  deletefile(HtmlFile),
  file_str(HtmlFile, Str2),
  format(URL, "file:///%s", HtmlFile),
  Null = cast(api_hwnd,0),
  api_ShellExecute(Null,"open",URL,"","",1), !.		% open in browser
  %TaskWin = vpi_GetTaskWin(),
  %dlg_browser_Create(TaskWin, URL), !.

get_toernooidir("", "", disk, "") :-
  webdir(_WD, inet, _),
  eerstekeer(b_True),
  %getOpeationeel(),
  get_Tempdir(_, TempFile),
  deletefile(Tempfile),
  AV = aanmeldenvars(),
  %GoedeAntwoord = "OK",
  NAV = ["gevraagd", "check" | AV],
  providerX(Prov, _, "", _ProvM),
  format(URL, "http://%s/inetcheck.php", Prov),
  deletefile(TempFile),
  Ret = downloadData(URL, TempFile, NAV, _, 0),
  checkServer(Ret),
  file_str(Tempfile, Str),
  %write(Str),
  handleContact(Ret, Str), !.	% gebeurt niks als mis
get_toernooidir(_, _, _, _) :-
  eerstekeer(b_True),
  assert(eerstekeer(b_False)),
  webdir(WD, inet, DatFile),
  %get_Tempdir(TDir, _),			% laad daarin het toernooi
  %format(Keus1, "%s\\%s\\", TDir, WD),
  AV = aanmeldenvars(),
  NAV = ["func", "gettoernooi", "map", WD | AV],
  %filenamepath(DatFile, Keus1, "tasn.dat"),
  %trycreatedir(Keus1),
  trap(deletefile(DatFile), _, fail),
  %provider(_, _, ProvM),
  %filenameext(DatFile, Header, _),
  %filenameext(TempFile, Header, ".gz"),
  trap(deletefile(DatFile), _, fail),
  _Ret = internetGet(NAV, DatFile),  
  %file_decompress(TempFile, DatFile),
  fail.
get_toernooidir(TB, Keus0, inet, TB) :-
  %get_TempDir(TDir, _),
  webdir(Keus0, inet, TB), !.
  %format(Padmet, "%s\\%s\\", TDir, Keus0),
  %filenamepath(TB, Padmet, "tasn.dat"),
  %!.
get_toernooidir(PadMet, TKort, DiskOfInet, TLok) :-
  webdir(TKort, DiskOfInet, _BN), 
  toernooidir(PadMet, TLok).

set_toernooidir(Dir,Bestand) :-
  fronttoken(Bestand, _, _),
  webdir(DirW, _, BN),
  assert(webdir(DirW, disk, BN)),
  assert(toernooidir(Dir, Bestand)), !.
set_toernooidir(Dir,_Bestand) :-
  fronttoken(Dir, _, _),
  webdir(DirW, _, BN),
  assert(webdir(DirW, disk, BN)),
  filenamePath(TB, Dir, "tasn.dat"),
  assert(toernooidir(Dir, TB)), !.
set_toernooidir(_Di, _R) :-
  getfolder(_, TM, _, _),
  assert(toernooidir(TM, "")), 
  _A = dlg_MessageBox( "Toernooimap", "Gebruik Toernooiselectie!", mesbox_iconerror, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.

%######################################## printen
predicates

print_afvalpoel(WINDOW, FONT, rsoort AofP, wedscat Onderdeel, RCT Papierafmeting, integer Opschuiven, integer AfmVert, integer FontSize)
tasPrintSetUp()
paslijst(WINDOW PRINTWIN, integer Orientatie, SLIST Selectie, BOOLEAN PrRichting, RCT PapierAfm)
vulbladzijden(WINDOW PRINTWIN, FONT, RCT Papier, catl CatList, integer FontSize)
vulbladzijde(WINDOW PRINTWIN, RCT Papier, catl CatList, catl ListIn, catl ListUit, integer Opsch, integer Tot) - (i,i,i,i,o,i,o)
printBladzijde(WINDOW PrintWin, FONT, RCT Papier, catl CatList, integer Vulling, integer Opschuiven, integer FontSize) - (i,i,i,i,i,i,i)
bepaal_vulling(RCT Papier, integer AantalPerBlz,integer Gebruikt, integer Vulling)
laatste(integer,integerlist)
synchroniseer(integer)
positie(integer, integerlist, integer, integer)
tijd_synch(integer, tijd)
procedure huidigeOfLaatste(integer Synch, integer Laatste, integer Out)
procedure getprfont(integer, BINARY)
afval_poel(rsoort, string)
nondeterm opgesteld(wedscat, integer)
besluit()
nondeterm tot1blz(wedscat,integer Orientatie)
print_planning(integer)
aantalDeelnemers(rsoort, wedscat, integer, integer)
post()
nondeterm kopl1(wedscat, wedscat)
nondeterm deelnemer(integer Opg, wedscat)
determ kenmerk(wedscat, string Kenmerk, integer Aantal)
determ notondercat(wedscat, boolean Finales)
procedure onlineable(boolean)

clauses

updatetoernooidirsave() :- % bij sluiten van de TA
  getParmString(Item),
  toernooidir(P, F),
  not(F = Item),
  webdir(WD, Lok, WB),
  assert(webdirsave(WD, Lok, WB)),
  assert(toernooidirsave(P, F)), !.  
updatetoernooidirsave() :-
  not(getParmString(_)),
  toernooidir(P, F),
  webdir(WD, Lok, WB),
  assert(webdirsave(WD, Lok, WB)),
  assert(toernooidirsave(P, F)), !.  
updatetoernooidirsave().

synchroniseer(VanafdagIndex) :-
  time_stamp1(Stamp,_,_),		% huidige
  tijd_synch(Stamp, PlanTijd),		% eerstvolgende TAS-tijd
  bitright(PlanTijd, 7, D2),		% D2 = de bijbehorende dag
  findall(DagNum, dag(DagNum, _, _), DagNummers),
  positie(D2, DagNummers, -1, DagPositie), !,  % zoek de positie in het dagenlijstje
  grootste(DagPositie, 0, VanafDagIndex).

tijd_synch(TTot, Tijd) :-
  findall(Tijd1, bn(Tijd1, _), Tijden),
  sorttijd_c(Tijden),
  member(Tijd,Tijden),
  Tijd > TTot, !.	% eerstvolgende grotere tijdstip
tijd_synch(_, 0).  

positie(Dag, [Dag|_], Num, Num) :- !.
positie(D1, [_|Dagen], In, Out) :-
  In1 = In + 1, !,
  positie(D1, Dagen, In1, Out).
positie(_, _, _, 0) :- !.


print_planning(0) :-
  prplan(0), !.
print_planning(TmDag) :-
  plantm(TmDag).

print_afvalpoel(_Win, FontNorm, afval, CatNo, Papier, Opschuiven, AfmVert, FontSize) :- !,
	%write("\nprint echt"),
	assert(emptyjob(b_False)),
	%jobstarted(Started),
	%startjob(Started),
	paginasel(PageSel),
	prvanaf(VanafDag),
%	prrichting(Orientatie),
	waarderingI(WaarderingJaNee),
	prkleur(KleurOfZw),
	prKader(Kader),
	print_planning(TmDag),
	% write("\nPlanning t/m dag:", TmDag),
	opvullen(Opvullen),
	% write("\nVanafdag:",Vanafdag),
	minkolhor(MinKolHor),
	minkolvert(MinKolVert),
	schema_draw(_Win, FontNorm, CatNo, Papier, PageSel, AfmVert, VanafDag, TmDag, WaarderingJaNee, Opschuiven, KleurOfZw, Kader, Opvullen, FontSize, MinKolHor,MinKolVert).
print_afvalpoel(Win, FontNorm,poel, CatNo, Papier, Opschuiven, _AfmVert, FontSize) :- 
	%write("\nprint echt"),
	assert(emptyjob(b_False)),
	%jobstarted(Started),
	%startjob(Started),
	open_blzEx(Win),
	print_planning(TmDag),
	poule_draw(Win, CatNo, TmDag, Papier, Opschuiven, FontNorm, FontSize).

laatste(L,[L]):- !.
laatste(L,[_|List]) :-
	laatste(L,List), !.
laatste(0,_).

huidigeOfLaatste(0, Laatste, Laatste) :- !.
huidigeOfLaatste(Synch, _, Synch).

tasPrintSetUp :-
        synchroniseer(DagPositie),
	findall(DagNum, dag(DagNum, _, _), DagNummers),
	laatste(Laatste,DagNummers),
	assert(plantm(Laatste)),
	huidigeOfLaatste(DagPositie, Laatste, Vanaf),
	assert(prvanaf(Vanaf)),
	!.

paslijst(PRINTWIN, Orientatie,Selectie,b_False,PapierAfm):-	% afvalschema's
  Orientatie <> 1,						% portret of opvullen  
  member(Cat, Selectie),
  getbacktrack(Here),
  searchchar(Cat,'\t',FoundPos),
  frontstr(FoundPos, Cat, _, SelectieOnd),
  wc(CatNo, _, _, SelectieOnd, _, _, _, _, _, _, _, _, _, _, _),
  %wc(CatNo, SelectieOnd,_,_,_,_,_),
  pos(CatNo,_,_,afval),
  paginasel(PageSel),
  %numb(N),
  prvanaf(VanafDag),
  waarderingI(Waardering),
  opvullen(Opvullen),
  print_planning(TmDag),
  minkolhor(MinKolHor),
  minkolvert(MinKolVert),
  afmetingEx(CatNo, PRINTWIN, PageSel, VanafDag, TmDag, PapierAfm, _AfmHor, AfmVert, Blz, Waardering, Opvullen,MinKolHor,MinKolVert),
  assert(teprinten(CatNo, AfmVert, Blz, afval, portret)),
  %N >= kopies,
  cutbacktrack(Here),
  fail.
paslijst(PRINTWIN, Orientatie,Selectie,b_False,PapierAfm):-	% poules
  Orientatie <> 1,				% portret of opvullen
  member(Cat, Selectie),
  searchchar(Cat,'\t',FoundPos),
  frontstr(FoundPos, Cat, _, SelectieOnd),
  wc(CatNo, _, _, SelectieOnd, _, _, _, _, _, _, _, _, _, _, _),
  %wc(CatNo, SelectieOnd, _, _, _, _, _),
  pos(CatNo,_,_,poel),
  paginasel(PageSel),
  prvanaf(VanafDag),
  print_planning(TmDag),
  pouleAfmetingEx(PrintWin, CatNo, PageSel, VanafDag, TmDag, PapierAfm, Blz, AfmVert, _Passing),
  assert(teprinten(CatNo, AfmVert, Blz/*Passing*/,poel,portret)),
  %write("\nBlz:",Blz),
  fail.
paslijst(PRINTWIN, Orientatie,Selectie,b_True,PapierAfm):-	% afvalschema's
  Orientatie <> 0,				% landschap of opvullen  
  member(Cat, Selectie),
  getbacktrack(Here),
  searchchar(Cat,'\t',FoundPos),
  frontstr(FoundPos, Cat, _, SelectieOnd),
  wc(CatNo, _, _, SelectieOnd, _, _, _, _, _, _, _, _, _, _, _),
  %wc(CatNo, SelectieOnd,_,_,_,_,_),
  pos(CatNo,_,_,afval),
  %numb(N),
  paginasel(PageSel),
  prvanaf(VanafDag),
  waarderingI(Waardering),
  opvullen(Opvullen),
  print_planning(TmDag),
  minkolhor(MinKolHor),
  minkolvert(MinKolVert),
  afmetingEx(CatNo, PRINTWIN, PageSel, VanafDag, TmDag, PapierAfm, _AfmHor, AfmVert, AantalBlz, Waardering, Opvullen, MinKolHor, MinKolVert),
  assert(teprinten(CatNo, AfmVert, AantalBlz, afval, landschap)),
  %N >= kopies,
  cutbacktrack(Here),
  fail.
paslijst(PRINTWIN, Orientatie,Selectie,b_True,PapierAfm):-	% poules
  Orientatie <> 0,				% landschap of opvullen
  member(Cat, Selectie),
  searchchar(Cat,'\t',FoundPos),
  frontstr(FoundPos, Cat, _, SelectieOnd),
  wc(CatNo, _, _, SelectieOnd, _, _, _, _, _, _, _, _, _, _, _),
  %wc(CatNo, SelectieOnd, _, _, _, _, _),
  pos(CatNo,_,_,poel),
  paginasel(PageSel),
  prvanaf(VanafDag),
  print_planning(TmDag),
  pouleAfmetingEx(PrintWin, CatNo, PageSel, VanafDag, TmDag, PapierAfm, _Blz, AfmVert, _Passing),
  assert(teprinten(CatNo, AfmVert, _Blz/*Passing*/,poel,landschap)),
  fail.
paslijst(_PRINTWIN, _Orientatie,_Selectie,_,_).
  %write(CatList).

loadPrintConfig() :-
  printer(PrintConfig),
  print_SetConfig(PrintConfig), !.
loadPrintConfig() :-
  dlg_PrintSetup(99, _SelectionType, _NoOfCopies,_FirstPage,_LastPage),
  PrintConfig = print_GetConfig(),
  assert(printer(PrintConfig)).

loadPrintConfig1() :-
  printer1(PrintConfig),
  print_SetConfig(PrintConfig), !.
loadPrintConfig1() :-
  dlg_MessageBox( "Printerselectie", "Kies de gewenste printer en klik op 'afdrukken'", mesbox_iconquestion, mesbox_buttonsokcancel, mesbox_defaultfirst, mesbox_suspendapplication ),
  dlg_PrintSetup(99, _SelType, _NoOfCopies,_FirstPage,_LastPage),
  PrintConfig = print_GetConfig(),
  assert(printer1(PrintConfig)) .

getprfont(_, Font) :-
  prfont(Font), !.
getprfont(FontSize, Font) :-
  Font = font_Create(ff_System,[],FontSize).

printOnderdelen(Selectie) :-
  %write("\n###################### Print setup."),
  print_SetOrientation(portret),		
  VResPrinter = vpi_GetAttrVal(attr_printer_vres),
  HResPrinter = vpi_GetAttrVal(attr_printer_hres),
  VResScreen = vpi_GetAttrVal(attr_screen_vres),
%  OldFont = font_Create(ff_System,[],FontSize),
%	Font1=dlg_ChooseFont(OldFONT),
  getprfont(10, Font1),
  _FontName = font_GetAttrs(Font1, _StyleFlags, FontSize1),
  FontSize2 = cast(integer,round(FontSize1 * VResPrinter / VResScreen)),
  Font2 = font_SetAttrs(Font1, [], FontSize2),	
  FontSizeFooter = FontSize2 * 2 div 3,
  Font_footer = font_SetAttrs(Font2,[],FontSizeFooter),
  %_FontName1 = font_GetAttrs(Font2, _StyleFlags1, FontSize3),
  %write("\nSize1:",FontSize1," Size2:",FontSize2, " Size3:", Fontsize3),
  VP = vpi_GetAttrVal(attr_printer_height),
  HP = vpi_GetAttrVal(attr_printer_width),
  prrichting(Orientatie),
  startJobEx(),		% reset blz nummer
  PRINTWIN=print_StartJob("Portret"),
  %write("\nstartjob1 "),
  assert(emptyjob(b_True)),
  assert(jobstarted(b_True)),
  win_SetFont(PrintWin,Font_Footer),
  win_GetTextExtent(PrintWin,"Halleluja",-1,_Width,FooterHoogte),
  sheetmarges(Links0, Rechts0, Boven0, Onder0),
  Links1  = val(integer, Links0 * HResPrinter / 25.4),
  Rechts1 = val(integer, Rechts0 * HResPrinter / 25.4),
  Boven1  = val(integer, Boven0 * VResPrinter / 25.4),
  Onder1  = val(integer, Onder0 * VResPrinter / 25.4),
  %writef("\n L: %d R: %d B: %d O: %d", Links1, Rechts1, Boven1, Onder1),
  VP1 = VP - FooterHoogte - Onder1,
  HP1 = HP - Rechts1,
  PapierAfmPortret = rct(Links1,Boven1,HP1,VP1),
  HP2 = HP - FooterHoogte - Links1,
  VP2 = VP - Onder1,
  PapierAfmLandschap = rct(Boven1,Rechts1,VP2,HP2),
  win_SetFont(PrintWin,Font2),
  paslijst(PrintWin, Orientatie,Selectie,portret,PapierAfmPortret),
  paslijst(PrintWin, Orientatie,Selectie,landschap,PapierAfmLandschap),
  %write("\nBegin met portret"),
  %printonderdelenDebug(),
  printOnderdelen(PrintWin,Font2,portret,PapierAfmPortret,FontSize1),	% zoveel mogelijk in portret
  %getBlzNr(Nr),
  %write("\nSwitch orientatie naar landschap"),
  teprinten(_CatNo, _, _Blz,_,_),		% er is ook landschap
  print_SetOrientation(landschap),
  assert(emptyjob(b_True)),
  %write("\nStartjob2"),
  PRINTWIN1=print_StartJob("Landschap"),
  assert(jobstarted(b_True)),
  win_SetFont(PrintWin1,Font2),
  printOnderdelen(PrintWin1,Font2,landschap,PapierAfmLandschap,FontSize1), !.
  %getBlzNr(Nr1),
  %write("\nAfdruk ", Nr1, " bladzijden, waarvan ",Nr," in portret mode.").
printOnderdelen(_Selectie).

printOnderdelen(PrintWin,FontNorm,portret,Papier, FontSize) :-	% koppelingen
	koppel(b_True),
	%write("\n koppelingen #####"),
        teprinten(CatNo, _, Blz,_,portret),		% 'top' passend en portret
	%write("\nprint CatNo: ", CatNo),
        Blz <= 10,
	findall(Gekoppeld,kopl1(CatNo,Gekoppeld),InCatList),
	not(InCatList = []),
	%write(" IncatList: ", InCatList),
 	assert(emptyjob(b_False)),
	%sortCats(InCatList, CatList),
	vulbladzijden(PrintWin, FontNorm,Papier, [CatNo|InCatList],FontSize), 
	fail.
printOnderdelen(_PrintWin,_Font,_,_,_) :-	
	besluit(),		% kies hor of vert met  minste aantal blz
	fail.
printOnderdelen(PrintWin,FontNorm,portret,Papier,FontSize) :-	% portret passend
	opvullen(b_True),
	%write("\n alle die portret passen ################"),
	findall(CatNo,tot1blz(CatNo,portret),InCatList),
 	not(InCatList = []),
 	assert(emptyjob(b_False)),
	%sortCats(InCatList, CatList),
	vulbladzijden(PrintWin, FontNorm,Papier, InCatList,FontSize), 
	fail.
printOnderdelen(PrintWin,FontNorm,portret,Papier,FontSize) :-
	%write("\n alle portret rest.. #############################"),
	teprinten(CatNo, [AfmVert|_], _, AfvPoel,portret),
	not(tot1blz(CatNo,landschap)), 				% landschap & passend
	retractall(teprinten(CatNo, _, _, _, _)), 
	%write("\nprint CatNo: ", CatNo),
 	assert(emptyjob(b_False)),
 	print_afvalpoel(PrintWin,FontNorm, AfvPoel,CatNo,Papier,3, AfmVert,FontSize),
        draw_footer(PrintWin, Papier,FontSize),
	fail.
printOnderdelen(PrintWin,_Font,portret,_,_) :-
	retractall(teprinten(_, _, _,_,portret)),	% verwijder overige
 	emptyjob(b_False),
	jobstarted(b_True),
	%write("\nEnd_job"),
	assert(jobstarted(b_False)),
	print_EndJob(PrintWin), !.			%   portret probeersels
printOnderdelen(PrintWin,_Font,portret,_Papier,_) :- 
 	%emptyjob(b_False),
	jobstarted(b_True), !,
	%write("\nAbort job"),
	assert(jobstarted(b_False)),
	print_EndJob(PrintWin).		%# was abort ##########
printOnderdelen(PrintWin,FontNorm,landschap,Papier,FontSize) :-
	opvullen(b_True),
	%write("\n alle die landschap passen ################"),
	%Papier = rct(_,_,_,Vert),	% Vert blijft vert!
	%findall(AfmVert, teprinten(CatNo, AfmVert, b_True,_,_),Afmetingen),
	%som(Afmetingen, 0, Totaal),
	%Totaal > Vert div 2,	% meer dan een halve bladzijde
	findall(CatNo,tot1blz(CatNo, landschap),CList),
	%write(Clist),
	%sortCats(CList, CatList),
	vulbladzijden(PrintWin, FontNorm,Papier, CList,FontSize), 
	fail.
printOnderdelen(PrintWin,FontNorm,landschap,Papier,FontSize) :-
	%write("\n alle rest.. ######### (moet niet nodig zijn) ####################"),
	retract(teprinten(CatNo, [AfmVert|_], _, AfvPoel,_)), 
	%write("\nprint2 CatNo: ", CatNo),
	%write(CatNo),
	%retractall(teprinten(CatNo, _, _, _, _)),
 	assert(emptyjob(b_False)),
 	print_afvalpoel(PrintWin, FontNorm,AfvPoel,CatNo,Papier,3,AfmVert,FontSize),
        draw_footer(PrintWin, Papier,FontSize),
	fail.
printOnderdelen(PrintWin,_Font,landschap,Papier,FontSize) :-
 	emptyjob(b_False),
	jobstarted(b_True),
	assert(jobstarted(b_False)),
        draw_footer(PrintWin, Papier,FontSize), !,
	%write("\nEndjob"),
	print_EndJob(PrintWin).
printOnderdelen(PrintWin,_Font,landschap,_Papier,_) :-
	jobstarted(b_True),
	%write("\nAbort2"),
	assert(jobstarted(b_False)), !,
	print_EndJob(PrintWin).		%# was abort
printOnderdelen(_PrintWin,_Font,_Landschap,_Papier,_) :-
	beep().

besluit() :-
	%write("\nBesluit welke van de twee"),
	wc(CatNo, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
        %wc(CatNo, _,_,_,_,_,_),
	teprinten(CatNo, _, BlzP,_,portret),
	teprinten(CatNo, _, BlzL,_,landschap),
	BlzP < BlzL,
	retract(teprinten(CatNo, _, _,_,landschap)),
	fail.
besluit() :-
	wc(CatNo, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
        %wc(CatNo, _,_,_,_,_,_),
	teprinten(CatNo, _, _,_,portret),
	teprinten(CatNo, _, _,_,landschap),
	retract(teprinten(CatNo, _, _,_,portret)),
	fail.
%besluit().  mag failen
	

tot1blz(CatNo, Orientatie) :-
  teprinten(CatNo, _AfmVert, Blz,_,Orientatie), 
  Blz <= 10.

kopl1(CatNo, Gekoppeld) :-
  wc(Gekoppeld, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
  %wc(Gekoppeld, _,_,_,_,_,_),
  getbacktrack(Here),
  teprinten(Gekoppeld, _, _Blz,_,_),		% en te printen
  kopl(Gekoppeld, _, CatNo, _),
  cutbacktrack(Here).

vulbladzijden(PRINTWIN,FontNorm,PapierAfm,InCatList,FontSize) :-	% verdeel passenden over blz
  %write("\nVulbladzijden"),
  vulbladzijde(PRINTWIN,PapierAfm, InCatList, [],BladzijdeList, 0, Gebruikt),
  Gebruikt > 0, !,
  count(BladzijdeList, 0, AantalPerBlz),
  %write("\nNieuwe blz, gebruikt: ",Gebruikt," Aantal per blz", AantalPerBlz),
  bepaal_vulling(PapierAfm, AantalPerBlz,Gebruikt,Vulling),
  PapierAfm = rct(_, Boven, _, _),
  printBladzijde(PRINTWIN, FontNorm,PapierAfm, Bladzijdelist, Vulling, Boven,FontSize),	% go print
  vulbladzijden(PRINTWIN,FontNorm,PapierAfm, InCatList,FontSize).
vulbladzijden(_PRINTWIN,_FontNorm,_PapierAfm,_InCatList,_FontSize).


bepaal_vulling(_Papier, _AantalPerBlz,_, 20) :- !.	%tijdelijk uitgeschakeld
bepaal_vulling(_Papier, AantalPerBlz,_, 0) :-		% vulling met wit
  AantalPerBlz < 2, !.
bepaal_vulling(PapierAfm, AantalPerBlz,Gebruikt,Vulling) :-
  PapierAfm = rct(_,Boven,_,PapVert),
  Vulling = (PapVert - Gebruikt - Boven) div AantalPerBlz.
  
printBladzijde(PrintWin, _,PapierAfm, [], _Vulling, _Opschuiven,FontSize) :- !,
  %write("\nBladzijde gevuld..."),
  draw_footer(PrintWin, PapierAfm,FontSize).
printBladzijde(PrintWin, FontNorm,PapierAfm, [CatNo|List], Vulling, Opschuiven,FontSize) :-
  retract(teprinten(CatNo, AfmVertList, _, AfvPoel,_Ori)), !,
  AfmVertList = [AfmVert|_],
  sumList(AfmVertList,0,Sum),
  retractall(teprinten(CatNo, _, _, _, _)), !,	% 26.8.2000 om koppelingen te cleanen
  print_afvalpoel(PrintWin, FontNorm,AfvPoel,CatNo,PapierAfm,Opschuiven,AfmVert,FontSize),
  Opschuiven1 = Opschuiven + Sum + Vulling,
  printBladzijde(PrintWin, FontNorm, PapierAfm, List, Vulling, Opschuiven1,FontSize).

vulbladzijde(_PrintWin,_Papier, [], In, In, Gebr, Gebr) :- !.
vulbladzijde(PRINTWIN,PapierAfm, [CatNo|List], In, [CatNo|UitList],Opgebruikt,Uit) :-
  teprinten(CatNo, AfmVert, Blz, _AfvPoel,_),
  Blz <= 10,	% #####################################################
  PapierAfm = rct(_,_,_,PapVert),
  sumlist(AfmVert, 0, Sum),
  %write("\nPapVert", PapVert, " Opgebruikt: ",Opgebruikt, " Sum:", Sum),
  Opgebruikt + Sum < PapVert, !,
  %write(" Zelfde blz"),
  Opgebruikt1 = Opgebruikt + Sum,
  vulbladzijde(PRINTWIN,PapierAfm, List, In, UitList, Opgebruikt1, Uit).
vulbladzijde(PRINTWIN,PapierAfm, [_|List], In, UitList, Opgebruikt, Uit) :-
  vulbladzijde(PRINTWIN,PapierAfm, List, In, UitList, Opgebruikt, Uit),!.
  % eind van bladzijde
  
finddag(DagNum, DagNaam) :-
	dag(DagNum, DagNaam,_), !.
finddag(0, _) :- !.
finddag(_, DagNaam) :- % ?????????????
	dag(_DagNum, DagNaam,_), !.

aantalDeelnemers(afval, _, In, In) :- !.
aantalDeelnemers(poel, Cat, _In, Uit) :-
  findall(N, opg(N, Cat, _T, _,_,_,_,_,_,_,_), NList),
  count(NList, 0, Uit).

wedscat_ingedeeld(Catname1, b_false, No,_) :-	% allemaal
  wc(No, _, _, CatName, _, _, _, _, _, _, _, _, _, _, _),
  %wc(No,Catname,_,_,_,_,_),
  findall(Opg, opgesteld(No, Opg), OpgL),
  count(OpgL, 0, Size3),
  pos(No,_Size,_,AfvPoel),
  afval_poel(AfvPoel, Ext),
  format(Catname1,"%s\t%3u\t%s", Ext, Size3, Catname).
wedscat_ingedeeld(Catname1, b_true, Cat, Finales) :-	% gekoppeld
  wc(Cat, _, _, Catname, _, _, _, _, _, _, _, _, _, _, _),
  %wc(Cat,Catname,_,_,_,_,_),
  notOndercat(Cat, Finales),	
  kenmerk(Cat, Ext, Size),
  format(Catname1,"%s\t%3u\t%s", Ext, Size, Catname).

notOnderCat(Cat,_) :-
  not(isOndercat(Cat)), !.
notOnderCat(Cat,b_True) :-		% 1.9.2014
  pos(Cat,_,_,_),
  kopl(Cat,_,CatTop,_),
  not(isOnderCat(CatTop)),
  not(pos(CatTop,_,_,_)), !.
notOnderCat(Cat,_) :-
  kopl(Cat,verliezer,_,_), !. % verliezersronde


opgesteld(Cat, N) :-
  opg(N, Cat, T, _,_,_,_,_,_,_,_),
  in_weds(Cat, T).

kenmerk(Cat, "f", Aantal) :-
  kopl(VCat, _, Cat, _), 
  pos(VCat, _, _, _),
  findall(Nr, deelnemer(Nr,Cat), NrList),
  count(NrList, 0, Aantal),!.
kenmerk(Cat, "V", Aantal) :-
  kopl(Cat,verliezer,_,_),
  %wc(_,_,_,_,_,_,Cat),
  pos(Cat, _, _, _),
  findall(Nr, deelnemer(Nr,Cat), NrList),
  count(NrList, 0, Aantal),!.
kenmerk(Cat, "", Uit) :-
  pos(Cat, _, _, afval), !,
  findall(N, opg(N, Cat, _T, _,_,_,_,_,_,_,_), NList),
  count(NList, 0, Uit).
kenmerk(Cat, "p", Uit) :-
  pos(Cat, _, _, poel), !,
  findall(N, opg(N, Cat, _T, _,_,_,_,_,_,_,_), NList),
  count(NList, 0, Uit).

deelnemer(Nr, Cat) :-
  opg(Nr, Cat, _T, _,_,_,_,_,_,_,_).
deelnemer(Nr, Cat) :-
  wc(CatV, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
  %wc(CatV,_,_,_,_,_,_),
  getbacktrack(Here),
  kopl(CatV, _, Cat, _),
  cutbacktrack(Here),
  opg(Nr, CatV, _T, _,_,_,_,_,_,_,_),
  not(opg(Nr, Cat, _, _,_,_,_,_,_,_,_)).

afval_poel(afval, " ") :- !.
afval_poel(_, "p").  


/*post() :-   verhuisd naar het begin!
  not(geladen), !.
post() :- 			% ---- alleen als log te groot!
  stopdbrefresh(),
  logclose(),
  logPos(Pos),
  logposini(IniP),
  Pos - IniP > 2000, 	% ---- moet 5000 zijn
  compress(update), !. */
post.


onlineable(b_True) :-
  webdir(_Map, inet,_), !.
onlineable(b_False).

%BEGIN_WIN Task Window
/***************************************************************************
	Event handling for Task Window
***************************************************************************/

PREDICATES

  task_win_eh : EHANDLER

CONSTANTS

%BEGIN Task Window, CreateParms, 17:04:38-16.4.2015, Code automatically updated!
  task_win_Flags = [wsf_SizeBorder,wsf_TitleBar,wsf_Close,wsf_Maximize,wsf_Minimize,wsf_ClipSiblings]
  task_win_Menu  = res_menu(idr_task_menu)
  task_win_Title = "tasvpi"
  task_win_Help  = idh_inhoud
%END Task Window, CreateParms
%upd_toolb = 21
kom_boven = 33
id_banentool = 911
id_certificaat = 9398
domains  /* dit is de puin van de configfile */
puin =	toernooidir(string Dir);webdir(string, lokatie, string); 
        weblayout(beeld , string , ilist , kolomtypeL, ilist , string , catl );
        modem_enabled(BOOLEAN, string Comport, string Voorkiezen, integer Speed, BOOLEAN Toonpuls, BOOLEAN Geluid) 
	

predicates
  duble_check(WINDOW)
  vensterInit(WINDOW)
  checkTrap(WINDOW, integer)
  bepaalDatabase(WINDOW)
  wmax(WINDOW,WSFLAGS, WSFLAGS)
procedure myConfConsult()
procedure eenIngedeeld(boolean, string)
determ handle_conferr()
procedure createInfoWindow()
procedure hoevaak2events(hoevaak, integer)
determ downloadsize(integer,slist, string, string UpdateExe, slist Boodschappen)
determ downloadOK(integer DeFile, integer, integer Retcode)
procedure huidige(string StIn, slist List, integer, integer Huidige)
procedure update_toolbar(window)
procedure notOnline(boolean)
procedure skipChars(char, string In, string Uit)
determ isErAntwoord(slist)
procedure updateTA(window)
procedure setAlleenLezen(string, string Pad)
procedure upload_auto(window, integer) % - (i,i) 
procedure updateTAvers(string)

CLAUSES


huidige(In, [In|_], N, N) :- !.
huidige(In, [_|Rest], N, U) :-
  N1 = N + 1, !,
  huidige(In, Rest, N1, U).
huidige(_, _, _, 0).

wmax(_,[wsf_Maximized|_], _WsFlags) :-	% maximized save afmetingen niet
  assert(window_maximized(b_True)), !.
wmax(W,[_|Rest], WsFlags) :-
  wmax(W,Rest, WsFlags), !.
wmax(_Win, _, WsFlags) :-		% nu wel
  assert(window_maximized(b_False)),
  OuterRct = win_GetOuterRect(_Win),
  ClientRct = rect_GetClient(WsFlags, b_True, OuterRct),
  ClientRct = rct(L,T,R,B),
  T1 = T, % - 19, NT only
  ClientRct1 = rct(L,T1,R,B),
  L >= 0,
  T1 >= 0,
  R > 20,
  B > 20,
  retractall(venster(_)),
  assert(venster(ClientRct1)).

duble_check(TaskWin) :-
  ScrWin = cast(window,win_GetAttrVal(TaskWin,attr_screen_window)),
  WINLIST = win_GetChildWindows(ScrWin),
  %WinText = win_GetText(TaskWin),
  TitelDeel = titeldeelc,
  str_len(TitelDeel, MyLen),
  member(OtherTaskWin,WinList),
    OtherTaskWin <> TaskWin,
    TaskWinText = win_GetText(OtherTaskWin),
    TaskWinText <> "",
    str_len(TaskWinText, Len),
    Len > MyLen,
    frontstr(MyLen,TaskWinText,StartString,_),
    %assert(tekst(TaskWinText)),
    StartString = TitelDeel,
    get_Tempdir(_, Tempfile),
    format(CmdString, "ver >> \"%s\"", Tempfile),
    system(CmdString),
    file_str(Tempfile, QIsVista),
    not(searchstring(QIsVista, "versie 6", _)),
  !,
  dlg_MessageBox( "TA", "TA is al aktief?", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  win_Destroy(TaskWin), !.
duble_check(_).

cycleOnd(Char) :-
  schemaWin(_, CurrCat), !,
  open_NextCat(Char, CurrCat).
/*cycleOnd(Char) :-
  Window = win_GetActiveWindow(),
  current_poule(Win1, CurrCat),
  Win1 = Window, !,
  open_NextCat(Char, CurrCat).
cycleOnd(_) :-
  current_afval(Win, _Currcat),
  win_BringToTop(Win), !.
cycleOnd(_) :-
  current_poule(Win, _Currcat),
  win_BringToTop(Win), !.
cycleOnd(_) :-
  pos(Cat, WNo, _, _),
  select(Cat, WNo),  !.*/
cycleOnd(_).

preselect([Ond|_], In, In) :- 
  getbacktrack(Here),
  pos(Cat, _, _, _),			% de voorste pos
  cutbacktrack(Here),
  frontstr(2,Ond,_,Selectie),
  searchchar(Selectie, '\t', Pos),
  frontstr(Pos, Selectie, _, OnderdeelSelected),
  wc(Cat, _, _, OnderdeelSelected, _, _, _, _, _, _, _, _, _, _, _), !.
  %wc(Cat, OnderdeelSelected,_,_,_,_,_),  !.
preselect([_|Rest], In, Uit) :-
  In1 = In + 1,
  preselect(Rest, In1, Uit), !.
preselect(_, In, In).

vensterInit(Win) :-
  venster(Rect),
  win_Move(Win, Rect),
  window_maximized(b_True),
  win_SetState(Win,[wsf_Maximized]), !.
vensterInit(_).

checkTrap(Win, Err) :- 
  Err < 25,
  Str = (".eisrevomed roov tkihcseg neellA"),
  reversestr(Str, "", Str1),
  dlg_Error(Str1),
  win_Destroy(Win).

update_proj_toolbar() :-			% global
  TaskWin = vpi_GetTaskWin(),
  win_PostEvent(TaskWin,e_User(upd_toolb,0)), !.	% update taakbuttons

skipChars(Char, In, Uit) :-
  not(Char = '"'), 
  frontchar(In, Char1, Uit1), !,
  skipChars(Char1, Uit1, Uit).
skipChars(_, In, In).

getParmString(StringUit) :-
  comline(Line),  % er is een commandline-toernooibestand
  concat(" ", Line, Line0),
  frontchar(Line0, Char, _),
  skipChars(Char, Line0, Line1),
  searchchar(Line1, '"', Pos), Pos1 = Pos - 1,
  frontstr(Pos1, Line1,StringUit, _).

setAlleenLezen(Item, Dir) :-   % in een temp directory
  filenamepath(Item, Dir, Bestand),
  upper_lower(Bestand, BL),  
  searchstring(BL,"temp",_FoundPos), !,
  %writedevice(X),
  %writedevice(screen),
  %write("\nhier1"),
  %writedevice(X),
  assert(alleenlezen1).
setAlleenLezen(Item, Dir) :-   % een back-up
  filenamepath(Item, Dir, Bestand),
  upper_lower(Bestand, BL),  
  searchstring(BL,"back",_FoundPos), !,
  %writedevice(X),
  %writedevice(screen),
  %write("\nhier2"),
  %writedevice(X),
  assert(alleenlezen1).
setAlleenLezen(Item, Dir) :-   % extensie niet .tas
  filenamepath(Item, Dir, Bestand),
  filenameext(Bestand, _, Ext),
  not(upper_lower(Ext, ".tas")), !,
  assert(alleenlezen1).
setAlleenLezen(_Item, "").
% :-
%  filenamepath(Item, Dir, _Bestand).


bepaaldatabase(_) :-
  getfolder(_F,Pad,_,_),  % Toernooien
  trap(mkdir(Pad), _, fail),
  fail.
bepaaldatabase(_) :-
  getfolder(_F,_,_,Pad), % backups
  trap(mkdir(Pad), _, fail),
  fail.
bepaaldatabase(_) :-
  getfolder(_F,_, Pad, _),  % gedeeld
  trap(mkdir(Pad), _, fail),
  fail.
bepaalDatabase(_) :-
  getParmString(Item),
  %_Answer = dlg_MessageBox( "Title", Item, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  %filenameext(Item, _, Ext),
  %upper_lower(Ext, ".tas"),
  trap(existfile(Item), _, true),
  %monitoralleenlezen(),
  setAlleenLezen(Item, _Path),
  toernooidirsave(Pad, _),
  set_toernooidir(Pad, Item), !.
bepaalDatabase(_) :-
  getParmString(Item),
  format(Msg, "Het opgegeven bestand \'%s\' kan niet geopend worden!", Item),
  dlg_MessageBox( "Open een toernooi!", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.
bepaalDatabase(_) :-
  not(usb(ja)),
  webdirsave(U, disk, WB),
  toernooidirsave(_Pad, TB),
  trap(existfile(TB), _, true),
  filenamepath(TB, Path, _),
  assert(webdir(U, disk, WB)),
  set_toernooidir(Path, TB), !.	% toernooidir waarschijnlijk ongeldig
bepaalDatabase(_) :-
  usb(ja),
  webdirsave(U, disk, WB),
  toernooidirsave(_Pad, TB1),
  syspath(ExeStartupPath,_ProgName),
  frontchar(ExeStartupPath, Letter, _),
  %write("\nhere, TB1: ",TB1),
  frontchar(TB1, _, Rest),
  %write("\nook hier"),
  frontchar(TB, Letter, Rest),
  trap(existfile(TB), _, true),
  filenamepath(TB, Path, _),
  assert(webdir(U, disk, WB)),
  set_toernooidir(Path, TB), !.	% toernooidir waarschijnlijk ongeldig
bepaalDatabase(_) :-
  webdirsave(U, disk, WB),
  toernooidirsave(Path, TB),
  assert(webdir(U, disk, WB)),
  set_toernooidir(Path, TB), !.	% toernooidir waarschijnlijk ongeldig
bepaalDatabase(_) :-
  webdirsave(U, inet, WB),
  assert(webdir(U, inet, WB)),
  toernooidirsave(Pad, TB),
  assert(toernooidir(Pad, TB)), !.
bepaalDatabase(_) :-
  setnaam(),
  update_proj_toolbar(),	% update taakbuttons
  Parent = vpi_GetTaskWin(),
  dlg_toernooiselectie_Create(Parent),
  fail.

stopDBrefresh() :-
  retract(timerid1(TimerId, _)),
  timer_Kill(TimerId), !.
stopDBrefresh().

startDBrefresh() :-
  not(alleenlezen1),
  TaskWin = vpi_GetTaskWin(),
  not(timerid1(_, _)),
  TimerId = timer_Set(TaskWin, timerintervalspeedy),
  assert(timerid1(TimerId, 0)), !.
startDBrefresh().


myConfConsult() :-
  VPIconf = getvpiini(),
  existfile(VPIconf),			% ini bestaat
  openread(work, VPIconf),
  readdevice(work),
  repeat_f(work),
  trap(readterm(vpiconf,Variable ),_,handle_conferr),
  assertz(Variable, vpiconf),
  fail.
myConfConsult() :-
  closefile(work).
  
handle_conferr() :-
  readtermerror(Line, _),
  trap(term_str(puin,Term,Line),_,fail),
  Term = toernooidir(Dir),
  filenamepath(TB, Dir, "tasn.dat"),
  assert(toernooidir(Dir, TB)),
  !.
handle_conferr() :-
  readtermerror(Line, _),
  trap(term_str(puin,Term,Line),_,fail),
  Term = webdir(Dir, Lok, _),
  assert(webdir(Dir, Lok, "")),
  !.
handle_conferr() :-
  readtermerror(Line, _),
  trap(term_str(puin,Term,Line),_,fail),
  Term = weblayout(AfdrOfBrowse, CSSnaam, Flags, Kolomtypel, IList, Opmerking, CatL),  % weblayout(beeld AfdrOfBrowseofUpload, string CSSnaam, ilist Flags, kolomtypeL, ilist FontGrootten, string Mededeling, catl Selected)
  logf('A',weblayout(AfdrOfBrowse, CSSnaam, Flags, Kolomtypel, IList, Opmerking, CatL),nee),
  !.

eenIngedeeld(b_True, "&Opsturen naar Website\tAlt-W") :-
  pos(_, _, _, _), !.
eenIngedeeld(b_False, "Er zijn nog geen schema's!").

status2get([Word|Status], "", Uit) :-
  str_int(String, Word), !,
  status2get(Status, String, Uit).
status2get([Word|Status], In, Uit) :-
  str_int(String, Word),
  format(In1, "%s,%s", In, String), !,
  status2get(Status, In1, Uit).
status2get(_, In, In).

createInfoWindow() :-
  providerX(Prov, _, "", _),
  not(Prov = "www.toernooiklapper.nl"),
  not(Prov = "tas.toernooiklapper.nl"),
  msg_Create(300, "Informatie"),
  writedevice(X),
  writedevice(screen),
  MsgW = msg_GetWin(),
  write("Test met provider ",Prov,"!\nKan alleen voor test gebruikt worden!"),
  dt_date_to_offset(1601,1,1, DateOffset),
  write(DateOffset),
  writedevice(X),
  edit_SetReadOnly(MsgW, b_True), !.
createInfoWindow() :-
  comline(LineBuffer),
  upper_lower(LineBuffer, LBL),
  searchstring(LBL, "/x", _),		% in TEST
  msg_Create(300, "Informatie"),
  MsgW = msg_GetWin(),
  edit_SetReadOnly(MsgW, b_True), !.
createInfoWindow().

hoevaak2events(vaak, 10).
hoevaak2events(mindervaak, 20).
hoevaak2events(afentoe, 30).



upload_auto(_, _) :-
  autowebP(Regelmaat,_, _Waarschuwen, _, _, _, _,_Vasteverb,_Hoevaak,_BijSluiten),
  Regelmaat = 0,
  retractall(uploadEvents(_)), !.	% teller van de events op moment van upload
upload_auto(_, _) :-
  uploadEvents(P0),
  belangrijkeEventsP(P1),
  P0 > P1,				% dan synchroniseren
  retractall(uploadEvents(_)),
  assert(uploadEvents(P1)), !.
upload_auto(_, _) :-
  not(uploadEvents(_)),			% hier worden de uploadevents geset (= belangrijke events)
  belangrijkeEventsP(P),
  assert(uploadEvents(P)), !.
upload_auto(_Win, Count) :-
  Count mod 2 = 1,  % ==> niet bij Count = 0 ? 
  uploadEvents(P0),
  belangrijkeEventsP(P1),
  autowebP(_Regelmaat,HTMLFTP,_Waarschuwen,_, _, _,_,_Vasteverb,Hoevaak, _BijSluiten),
  hoevaak2events(Hoevaak, Events),
  P1 > P0 + Events,
  retractall(uploadEvents(_)),
  assert(uploadEvents(P1)),
  waarnaar(HTMLFTP, Upload), 
  dlg_maak_webpaginas_Create(_Win, Upload, [], b_True), !.	% noshow
upload_auto(_, _).

waarnaar(1, upload) :- !.
waarnaar(_, uploadftp).

erIsGeupload() :-
  retractall(uploadEvents(_)).

downloadsize(_In, ParmsList, Site, UpdateExe, _BoodschappenLijst) :-
  assert(dlretcode(0)),
  assert(dlsize(0)),
  %list_to_string(BoodschappenLijst, "\n", Boodschappen),
  %format(Tekst, "%s\n\nDownloaden?", Boodschappen),  
  %1 = dlg_MessageBox("Programma-update", Tekst, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
  checkvoortgang(0, "download"),
  NParms1 = ["step", "2" | ParmsList],
  checkvoortgang(24, "bezig met downloaden nieuwe versie ..."),
  deleteFile(UpdateExe),
  Retcode = programmaUpdate(2, Site, UpdateExe, NParms1),	% stap 2
  %Retcode = downloadData(Site, UpdateExe, NParms1, _Response, 1),	% stap 2
  checkvoortgang(25, "download compleet ..."),
  file_bin(UpdateExe, Bin),
  Size = getbinarysize(Bin),
  assert(dlsize(Size)),
  assert(dlretcode(Retcode)),
  fail.
downloadsize(In, _ParmsList, _Site, _UpdateExe, _BoodschappenLijst) :-
  dlsize(Size),
  dlretcode(Retcode),
  downloadOK(Size, In, Retcode),
  !.

downloadOK(Size, _In, Retcode) :-
  Size > 0,
  Retcode = 0, !.
downloadOK(_UpdateExe, _In, Retcode) :-
  Retcode > 0, 
  dlg_MessageBox("Programma-update","Download mislukt! Vista?\nNeem svp contact met ons op voor een update.", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultsecond, mesbox_suspendapplication ),
  !,
  fail.
downloadOK(Size, In, _) :-
  format(Msg, "Download afgebroken.\nSize: %u, In: %u", Size, In),
  dlg_MessageBox("Programma-update",Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultsecond, mesbox_suspendapplication ),
  fail.

/*updateTAvers("TAupdate2011B.php") :-
  comline(LineBuffer),
  upper_lower(LineBuffer, LBL),
  searchstring(LBL, "/x", _), !. */
updateTAvers("TAupdate2011.php").

updateTA(_Taskwin) :-
  close_alles(),
  ParmsList = aanmeldenVars(),
  providerX(Provider, _, "", _),
  updateTAvers(TAU),
  format(Site, "http://%s/%s",Provider,TAU),
  get_TempDir(TDir, _),
  filenamepath(FeedbackXML, TDir, "feedback.xml"),
	%autowebP(_, _, _, _, _, _, _, GeenVaste,_,_),
	%b_True = vragenOmverbinding(GeenVaste, b_False),
  cursor_SetWait(),
  %ParmsList1 = ["versie", Versie | Parmslist],
  N2Parms = ["step", "3" | ParmsList],
  %programmaUpdate(Site, FeedbackXML, N2Parms),	% haal de filesize op   ##################
  deletefile(FeedBackXML),
  downloadData(Site, FeedbackXML, N2Parms, _Resp, 1),	% haal de filesize op   ##################
  file_str(FeedbackXML, Tekst1),
  %rite("\n", Tekst1),
  parseMsg(Tekst1, GMList, _),
  isErAntwoord(GMList),
  GMList = [Grootte|Boodschappen],
  str_int(Grootte, Size),
  filenamepath(UpdateExe, TDir, "taververs.exe"),
  write("\n", UpdateExe),
  close_alles(),
  downloadsize(Size, ParmsList, Site, UpdateExe, Boodschappen), % stap 2
  syspath(Dir, _),
  reversestr(Dir, "", DirR),
  frontchar(DirR, _, DirR1),
  reversestr(DirR1, "", DirEcht),
  upper_lower(DE, DirEcht),
  format(Parms, "/MYPATH=\"%s\"", DE),
  %write("\n",Parms), 
  1 = dlg_MessageBox("Programma-update","Installatie starten?\nDe ToernooiAssistent zal worden gestopt.", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
  checkvoortgang(100, ""),
  _ApplicationId = vpi_CreateProcess(~0, UpdateExe, Parms, wsf_Restored ),
  win_PostEvent(_TaskWin,e_Closerequest()), !.
updateTA(_Taskwin) :-
  checkvoortgang(100, ""), !.

%update_toolbar(_Win) :-
%  dlg_note("","update_toolbar"),
%  fail.
update_toolbar(Win) :-
  toolbar_SetValue(Win, id_Toernooi_Backup_maken, ctrl_value(1,1)),
  toolbar_SetValue(Win, id_Toernooi_Bestand_maak_nieuw, ctrl_value(1,1)),
  toolbar_SetValue(Win, id_Speler_Oproepen_lijst, ctrl_value(0,1)),
  getbacktrack(Here),
  w2(_No,_Cat,_Tijd1,_G1,_G2,_,_,_), 
  cutbacktrack(Here),
  toolbar_SetValue(Win, id_Speler_Oproepen_lijst, ctrl_value(1,1)),
  fail.
update_toolbar(Win) :-
  getbacktrack(Here),
  ws(_No, _Cat, _Tgnst, _Tijd),
  cutbacktrack(Here),
  toolbar_SetValue(Win, id_Speler_Oproepen_lijst, ctrl_value(1,1)),
  fail.
update_toolbar(Win) :-
  toolbar_SetValue(Win, id_Toernooi_uitslagen, ctrl_value(0,1)),
  getbacktrack(Here),
  w2(_No,_Cat,_Tijd1,_,_,_,_,_), 
  cutbacktrack(Here),
  toolbar_SetValue(Win, id_Toernooi_uitslagen, ctrl_value(1,1)),
  fail.
update_toolbar(Win) :-
  toolbar_SetValue(Win, id_Onderdeel_Plan_een_voor_een, ctrl_value(0,1)),
  planMeteen(b_True),
  toolbar_SetValue(Win, id_Onderdeel_Plan_een_voor_een, ctrl_value(1,1)),
  fail.	
update_toolbar(Win) :-
  toolbar_SetValue(Win, idr_planoverzmin, ctrl_value(0,1)),
  getbacktrack(Here1),
  wd(Num, Cat, _, _, _, _, _), 
  planbaar(Num, Cat, Bool),
  Bool = b_True,
  cutbacktrack(Here1),
  toolbar_SetValue(Win, idr_planoverzmin, ctrl_value(1,1)),
  fail.	
update_toolbar(Win) :-
  toolbar_SetValue(Win, id_Onderdeel_vertoon, ctrl_value(0,1)),
  getbacktrack(Here),
  pos(_,_,_,_),
  cutbacktrack(Here),
  toolbar_SetValue(Win, id_Onderdeel_vertoon, ctrl_value(1,1)),
  fail.
update_toolbar(Win) :-
  toolbar_SetValue(Win, id_Speler_wedstrijden, ctrl_value(0,1)),
  getbacktrack(Here),
  opg(_, _, _, _,_,_,_,_,_,_,_),
  cutbacktrack(Here),
  toolbar_SetValue(Win, id_Speler_wedstrijden, ctrl_value(1,1)),
  fail.
update_toolbar(Win) :-
  %toolbar_SetValue(Win, idd_online, ctrl_value(0,1)),
  toolbar_SetValue(Win, idt_online, text_valueExt("lokaal",color_Black,color_Yellow)),
%
  webdir(_, inet,_),
  toolbar_SetValue(Win, idt_online, text_valueExt("gedeeld!",color_Black,color_Cyan)),
  %toolbar_SetValue(Win, idd_online, ctrl_value(1,1)),
  fail.
update_toolbar(_Win).

notOnline(b_False) :-
  webdir(_, inet,_), !.
notOnline(b_True).

isErAntwoord([_,_|_]) :- !.
isErAntwoord([M|_]) :-
  _Answer = dlg_MessageBox( "Buiten bedrijf", M, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.
isErAntwoord(_) :-
  _Answer = dlg_MessageBox( "Buiten bedrijf", "Geen antwoord van de server!", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.


%BEGIN Task Window, e_Create
  task_win_eh(_Win,e_Create(_),0):-
	%win_Clear(_Win, color_Blue),
	duble_check(_Win),
	class_Create("PDCEDIT",pdcedit_handler),
	cTreeView::register(),
	progress_register(),
%BEGIN Task Window, InitControls, 17:04:38-16.4.2015, Code automatically updated!
%END Task Window, InitControls
%BEGIN Task Window, ToolbarCreate, 17:04:38-16.4.2015, Code automatically updated!
	tb_project_bottom_Create(_Win),
	tb_project_toolbar_Create(_Win),
%END Task Window, ToolbarCreate
IFDEF use_message
	createInfoWindow(),
ENDDEF
	retractall(printer(_)),
	retractall(printer1(_)),
	getopeationeel(_Win),
	myConfConsult(),
	retractall(printer(_)),		% 30.10.2000
	retractall(printer1(_)),	% 30.10.2000 niet meer nodig
	vensterinit(_Win),
	tasPrintSetUp,
	win_PostEvent(_Win,e_user(dotoernooiload,0)),	% load data
	status(SL),
	ontsleutel(SL, Id), 
        Centie = "centie:",
        reversestr(Id, "", Id1),
        retractall(sleuteltekst(_)),
        assert(sleuteltekst(Id1)),
        reversestr(Id1, "", Id2),
	format(Id3, "%s%s%s%c%s", "L", "i", Centie, ' ', Id2),
  	toolbar_SetValue(_Win, idc_laatst_gewijzigde, text_value(Id3)), !.
%END Task Window, e_Create

%MARK Task Window, new events

%BEGIN Task Window, id_Onderdeel_Webpaginas_archiveren
  task_win_eh(_Win,e_Menu(id_Onderdeel_Webpaginas_archiveren,_ShiftCtlAlt),0):-
  	tkalrequest1("top"),
  	!. 
%##### id_Onderdeel_Webpaginas_archiveren

%END Task Window, id_Onderdeel_Webpaginas_archiveren


%BEGIN Task Window, id_help_veel_gestelde_vragen
  task_win_eh(_Win,e_Menu(id_help_veel_gestelde_vragen,_ShiftCtlAlt),0):-!,
	Null = cast(api_hwnd,0),
  	api_ShellExecute(Null,"open","https://www.toernooiklapper.nl/veel-gestelde-vragen/","","",1),
	!.
%END Task Window, id_help_veel_gestelde_vragen

%BEGIN Task Window, id_help_TA_update
  task_win_eh(_Win,e_Menu(id_help_TA_update,_ShiftCtlAlt),0):-!,
	updateTA(_Win),
	!.
%END Task Window, id_help_TA_update_nieuwe

%BEGIN Task Window, id_Toernooi_uitslagen_invoeren
  task_win_eh(_Win,e_Menu(id_Toernooi_uitslagen_invoeren,_ShiftCtlAlt),0):-
	uitsl_verwrk(_Win),
	%vandaagOfMorgen(morgen, _, IVOM),
	PNT = cursor_GetPos(_Win),
	Menu = dyn_menu([
	  txt(id_Toernooi_Agenda__lijst,"Agenda",0,b_True,0,[]),
          txt(0,"Dagstatus",0,b_True,0,[
            txt(id_Toernooi_Agenda_Dagstatus_vandaag,"Selecteer dag..",0,b_True,0,[]),
            txt(id_Toernooi_Agenda_Dagstatus_instellingen,"Lettergrootte",0,b_True,0,[])
            ])
	  ]),
	menu_PopUp(_Win,Menu, PNT, align_Left),
	!.
%END Task Window, id_Toernooi_uitslagen_invoeren

%BEGIN Task Window, idt_afronden
  task_win_eh(_Win,e_Menu(idt_afronden,_ShiftCtlAlt),0):-!,
	Ingedeeld = b_True,
	Finalebekend = b_True,
	PNT = cursor_GetPos(_Win),
	Menu = dyn_menu([
	  txt(0,"Einduitslagen => KNLTB",0,b_True,0,[
            txt(id_Toernooi_einduitslagen_KNLTB,"Stuur het resultaat naar ServIt",0,Ingedeeld,0,[])
            ]),
    	  txt(id_Toernooi_Backup_maken,"Laatste back-up maken",0,b_True,0,[]),
    	  txt(id_Onderdeel_Webpaginas_uploaden,"Laatste Upload naar Web (schema's eindstand)",0,b_True,0,[]),
    	  txt(id_Toernooi_toernooiassistentkalender,"Archiveren van de schema's op Internet (zip)",0,Ingedeeld,0,[]),
    	  txt(0,"Eindstand van",0,b_True,0,[
            txt(id_Toernooi_einduitslagen_1_en_2,"Winnaars (1 en 2)",0,Ingedeeld,0,[]),
            txt(id_Toernooi_einduitslagen_nrs_14,"Nummers 1 t/m 4",0,Ingedeeld,0,[]),
            txt(id_Toernooi_einduitslagen_deelnemers,"Deelnemers",0,Ingedeeld,0,[])
          ]),
          txt(id_Toernooi_finales1,"Finalepagina",0,Finalebekend,0,[]),
    	  txt(id_Toernooi_einduitslagen_onregelm,"Onregelmatighedenrapport",0,Ingedeeld,0,[])
          ]),
	menu_PopUp(_Win,Menu, PNT, align_Left),
	!.
%END Task Window, idt_afronden

%BEGIN Task Window, id_onderdeel
  task_win_eh(_Win,e_Menu(id_onderdeel,_ShiftCtlAlt),0):-
	win_onderdeelselectie_Create(_Win),	% 17.9.2011
	PNT = cursor_GetPos(_Win),
	Menu = dyn_menu([
          txt(id_Onderdeel_vertoon,"&Schema Openen",0,b_True,0,[]),
          txt(id_Onderdeel_Print_via_browser,"Afdrukken",0,b_True,0,[]),
          txt(id_file_herstel,"Waar is het menu gebleven?",0,b_True,0,[]),
          separator,
          txt(id_Onderdeel_Webpaginas_uploaden,"Schema's en planning publiceren\tAlt-W",0,b_True,0,[]),
          txt(idr_sheets,"Zo staat het nu op Internet",0,b_True,0,[]),
          txt(id_Toernooi_toernooiassistentkalender,"Verwijderen van Internet",0,b_True,0,[]),
          separator,
          txt(id_Onderdeel_indelen,"Indelen en loten (+ KNLTB check)\tAlt-D",0,b_True,0,[]),
      	  txt(id_Onderdeel_bloktijden,"Bloktijden per onderdeel instellen",0,b_True,0,[]),
      	  txt(idcd_afdruk,"Rapporten",0,b_True,0,[])
	  ]),
	menu_PopUp(_Win,Menu, PNT, align_Left),
	!.
%END Task Window, id_onderdeel

%BEGIN Task Window, id_Speler_Betalingen_popup
  task_win_eh(_Win,e_Menu(id_Speler_Betalingen_popup,_ShiftCtlAlt),0):-!,
	spelerBetaal(_Win),
	PNT = cursor_GetPos(_Win),
	Menu = dyn_menu([
	  %txt(id_Onderdeel_print,"&Lijst",0,b_True,0,[]),
          txt(id_Toernooi_financien, "Aantallen &inschrijvingen + financiën",0,b_True,0,[]),
	  txt(id_Speler_Betalingen_etiketten,"&Etiketten",0,b_True,0,[]),
	  txt(0,"Incasso",0,b_True,0,[
	    txt(id_Speler_Betalingen_Incasso_proef,"Incasso &proef",0,b_True,0,[]),
	    txt(id_Speler_Betalingen_Incasso_nog_niet_betaald,"I&ncasso",0,b_True,0,[]), 
	    txt(id_Speler_Betalingen_Incasso_alle,"Incasso Alle",0,b_True,0,[]) 
	  ]),
	  %txt(id_Speler_Betalingen_Incasso_proef,"&Clieop Incasso (verouderd!)",0,b_True,0,[]),
	  separator(),
	    txt(id_Speler_betaald_reset,"&Reset ALLE betalingen",0,b_True,0,[])
	]),
	menu_PopUp(_Win,Menu, PNT, align_Left),
	!.
%END Task Window, id_Speler_Betalingen_popup

%BEGIN Task Window, id_oproepen_publiceren
  task_win_eh(_Win,e_Menu(id_oproepen_publiceren,_ShiftCtlAlt),0):-
	PNT = cursor_GetPos(_Win),
	%win_waarschuwinvoer_Create(_Win, []),	% 17.3.2012 kan niet vanwege de msgbox
	eenIngedeeld(_Ingedeeld, _IText),
	Menu = dyn_menu([
% oproepen
          txt(id_Speler_Oproepen_lijst,"Oproep (met bevestiging)\tF6",0,b_True,0,[]),
          txt(id_Onderdeel_Webpaginas_uploaden,"Schema's en planning publiceren\tAlt-W",0,b_True,0,[]),
    	  separator,
  	  txt(id_Speler_Oproep_print,"Oproeplijst (Tip: vink aan \"Niet al gewaarschuwd\")",0,b_True,0,[]),
  	  txt(id_Speler_Oproepen_etiketten,"Etiketten voor oproep",0,b_True,0,[]),
  	  txt(id_Speler_Oproepen_brieven,"Mailmergebestand (Samenvoegen met Word)",0,b_True,0,[]),
  	  separator,
  	  txt(0, "Meer",0,b_True,0, [
 	    txt(idc_gewaarschuwd,"Allemaal &Gewaarschuwd boeken",0,b_True,0,[]),
     	    txt(idcp_deelname,"Allemaal nog &Niet gewaarschuwd boeken",0,b_True,0,[])
	  ])
	]),
	menu_PopUp(_Win,Menu, PNT, align_Left),
	!.
%END Task Window, id_oproepen_publiceren

%BEGIN Task Window, idt_inrichten
  task_win_eh(_Win,e_Menu(idt_inrichten,_ShiftCtlAlt),0):-
%kenmerken
	PNT = cursor_GetPos(_Win),
	Menu = dyn_menu([
	  txt(id_Internet,"Nieuw toernooi aanmaken/inrichten",0,b_True,0,[]),
	  txt(id_Toernooi_eigenschappen,"Toernooikenmerken instellen ",0,b_True,0,[]),
	  txt(idr_Servit2definities,"Toernooinummer/certificaat beheer",0,b_True,0,[]),
          txt(id_Toernooi_beleid,"Toernooibeleid instellen",0,b_True,0,[]),
          txt(id_Toernooi_kalender,"Kalender aanpassen",0,b_True,0,[]),
	  txt(id_Onderdeel_eigenschappen,"Onderdelen instellen (naam, inschrijfgeld etc.)\tAlt-K",0,b_True,0,[]),
          txt(0,"Inschrijfformulier",0,b_True,0,[
            txt(id_Toernooi_inschrijfformulier_instellen,"Instellen, uploaden en testen",0,b_True,0,[]),
            txt(idr_reglement,"Reglement toevoegen (optioneel)",0,b_True,0,[]),
            txt(idr_logo,"Eigen logo (optioneel)",0,b_True,0,[]),
            txt(idr_sheets,"Het actuele formulier op Internet",0,b_True,0,[]),
            txt(id_Toernooi_toernooiassistentkalender,"Verwijderen van Internet",0,b_True,0,[])
          ]),
          separator(),
          txt(id_Toernooi_extra_instellingen,"Extra instellingen",0,b_True,0,[])

          %txt(id_Onderdeel_indelen,"Indelen en loten (+ KNLTB check)\tAlt-D",0,b_True,0,[])
	]),
	menu_PopUp(_Win,Menu, PNT, align_Left),
	!.
%END Task Window, idt_inrichten

%BEGIN Task Window, id_PlanX
  task_win_eh(_Win,e_Menu(id_PlanX,_ShiftCtlAlt),0):-
	planMetAgenda(),
	PNT = cursor_GetPos(_Win),
	eenIngedeeld(Ingedeeld, _IText),
	Menu = dyn_menu([
          %txt(idt_agenda,"Agenda Openen",0,b_True,0,[]),
	  txt(0,"Plannen",0,Ingedeeld,0,[
	    txt(id_Onderdeel_Plan_automatisch,"Plan automatisch",0,Ingedeeld,0,[]),
	    txt(id_Onderdeel_Plan_per_poule,"Per poule",0,Ingedeeld,0,[]),
	    txt(0,"Per wedstrijd (alleen vanuit schema)\tF5",0,Ingedeeld,0,[])
	  ]),
	  txt(0,"Planning wissen",0,Ingedeeld,0,[
            txt(id_Onderdeel_Wis_plan_periode,"In een periode (regen?)",0,Ingedeeld,0,[]),
            txt(id_Onderdeel_Wis_plan_alle,"Alle planning (opnieuw?)",0,Ingedeeld,0,[]),
            txt(0,"Per wedstrijd (diverse plaatsen)",0,Ingedeeld,0,[])
          ]),
	  txt(id_Onderdeel_Plan_een_voor_een, "Nog te plannen\tF4",0,b_True,0,[]),
	  separator,
          %txt(idt_agenda,"Agenda inzien",0,b_True,0,[]),
          txt(id_Toernooi_beleid,"Planningvoorwaarden instellen",0,b_True,0,[]),
   	  txt(id_Toernooi_baanbeschikbaarheid,"Baanbeschikbaarheid instellen",0,b_True,0,[]),
    	  txt(0,"Bloktijden (algemene verhinderingen) instellen",0,b_True,0,[
      	    txt(id_Speler_bloktijden,"per leeftijdsgroep",0,b_True,0,[]),
      	    txt(id_Onderdeel_bloktijden,"per onderdeel",0,Ingedeeld,0,[])
          ]),
          %txt(id_Wedstrijd_overzicht,"-overzicht van een wedstrijd (vanuit schema)\tF7",0,b_True,0,[]),
          txt(0,"Diverse overzichten",0,b_True,0,[
            txt(id_Toernooi_voortgang,"Voortgang",0,Ingedeeld,0,[]),
            txt(id_Toernooi_Controle_knelpunten,"Knelpunten",0,Ingedeeld,0,[]),
            txt(id_Toernooi_Controle_spelerbeschikbaarheid,"Spelerbezetting",0,Ingedeeld,0,[]),
            separator,
            txt(id_Toernooi_Controle_bezwaren,"Controleren vs ingesteld beleid",0,b_True,0,[])
            %txt(id_Toernooi_Controle_enkel_na_dubbel,"Enkel na dubbel",0,Ingedeeld,0,[]),
            %txt(id_Toernooi_Controle_dubbel_na_enkel,"Dubbel na enkel",0,Ingedeeld,0,[])
            %txt(id_Onderdeel_Plan_meer_dan2uur,"te &Lang wachten",0,Ingedeeld,0,[])
          ])
        ]),
	menu_PopUp(_Win,Menu, PNT, align_Left),
	!.
%END Task Window, id_PlanX

%BEGIN Task Window, id_speler
  task_win_eh(_Win,e_Menu(id_speler,_ShiftCtlAlt),0):-
	PNT = cursor_GetPos(_Win),
	%teruggable(Terug),
	%onlineable(OL),
	Menu = dyn_menu([
          txt(id_Speler_gegevens,"Personen\tF2",k_f2,b_True,0,[]),
          txt(id_Speler_wedstrijden,"Deelnemers\tF3",k_f3,b_True,0,[]),
          txt(id_Speler_selectie,"Speciale selectie",0,b_True,0,[]),
          txt(id_Speler_opgaven,"Opgaven/Inschrijvingen Control Center\tAlt-X",0,b_True,0,[]),
          txt(id_Speler_gegevens,"Gelijkbedoelde spelers samenvoegen",0,b_True,0,[]),
	  separator,
      	  txt(id_Speler_bloktijden,"Bloktijden per leeftijdsgroep",0,b_True,0,[]),
          txt(id_Speler_verenigingen,"Verenigingen",0,b_True,0,[]),
          txt(id_Speler_Checken_lidnummers,"Bondsnummers checken",0,b_True,0,[
            txt(id_Speler_Checken_lidnummers_dubbele,"Dubbele bondsnummers",0,b_True,0,[]),
            txt(id_Speler_Checken_lidnummers_niet_geldig,"Ongeldige bondsnummers",0,b_True,0,[])
            ]),
          txt(id_Speler_eigenschappen,"Kenmerken",0,b_True,0,[])
	]),
	menu_PopUp(_Win,Menu, PNT, align_Left),
	!.
%END Task Window, id_speler


%BEGIN Task Window, idt_internet
  task_win_eh(_Win,e_Menu(idt_internet,_ShiftCtlAlt),0):-
	PNT = cursor_GetPos(_Win),
	eenIngedeeld(_Ingedeeld, IText),
	Menu = dyn_menu([
          txt(0,"- -   INTERNET   - -",0,b_True,0,[]),
	  separator,
          txt(0,"Email",0,b_True,0,[
            txt(id_Speler_Uitvoer_email_aan_allen,"Bulk",0,b_True,0,[]),
            txt(id_Speler_Oproepen_lijst,"Oproep (met bevestiging)",0,b_True,0,[])
          ]),
	  separator,
          txt(0,"Formulier",0,b_True,0,[
            txt(id_Toernooi_inschrijfformulier_instellen,"Instellen, uploaden en testen",0,b_True,0,[]),
            txt(idr_reglement,"Reglement toevoegen(optioneel)",0,b_True,0,[]),
            txt(id_Toernooi_toernooiassistentkalender,"Eigen logo toevoegen (optioneel)",0,b_True,0,[]),
            txt(idr_sheets,"Het actuele formulier op Internet",0,b_True,0,[]),
            txt(id_Toernooi_toernooiassistentkalender,"Verwijderen van Internet",0,b_True,0,[])
          ]),
	  separator,
          txt(0,"Inschrijvingen",0,b_True,0,[
            txt(id_Speler_opgaven,"Opgaven/Inschrijvingen Control Center\tAlt-X",0,b_True,0,[]),
            separator,
            txt(id_Speler_gegevens,"Gelijkbedoelde spelers samenvoegen",0,b_True,0,[])
            ]),
	  separator,
          txt(0,"Schema's en planning ",0,b_True,0,[
            txt(id_Onderdeel_Webpaginas_uploaden,IText,0,b_True,0,[]),
            txt(idr_sheetcss,"Eigen opmaak (optioneel)",0,b_True,0,[]),
            txt(idr_sheets,"De actuele sheets op Internet",0,b_True,0,[]),
            txt(id_Toernooi_toernooiassistentkalender,"Verwijderen van Internet",0,b_True,0,[])
          ]),
	  separator,
          txt(id_Toernooi_toernooiassistentkalender,"TOERNOOIKLAPPER beheer (autoinloggen)",0,b_True,0,[]),
          txt(0,"Algemeen",0,b_True,0,[
            txt(id_Onderdeel_Webpaginas_uploadinstellingen,"Uploadinstellingen",0,b_True,0,[]),
            txt(idr_logo,"Eigen logo (optioneel)",0,b_True,0,[]),
            txt(idr_reglement,"Reglement (optioneel)",0,b_True,0,[])
            ])
	  ]),
	menu_PopUp(_Win,Menu, PNT, align_Left),
	!.
%END Task Window, idt_internet

%BEGIN Task Window, id_Toernooi_Backup
  task_win_eh(_Win,e_Menu(id_Toernooi_Backup,_ShiftCtlAlt),0):-
	PNT = cursor_GetPos(_Win),
	%teruggable(Terug),
	onlineable(OL),
	Menu = dyn_menu([
          txt(0,"- -   BACKUP   - -",0,b_True,0,[]),
	  separator,
          txt(0,"maken . . .",0,b_True,0,[
            txt(id_Toernooi_Backup_maken,"naar harde schijf of memory stick",0,b_True,0,[]),
            txt(id_Toernooi_toernooiMap_backup_emailen,"gedeeld + vervolgens eventueel emailen",0,b_True,0,[]),
	    txt(idr_onlinebu,"maak zelf een on-line back-up",0,OL,0,[]),
	    separator,
            txt(id_Toernooi_extra_instellingen,"Autobackup Instellen",0,b_True,0,[])
            ]),
	  separator,
          txt(id_Toernooi_Backup_terugzetten,"terugzetten/laden . . .",0,b_True,0,[]),
	  separator
	  ]),
	menu_PopUp(_Win,Menu, PNT, align_Left),
	!.
%END Task Window, id_Toernooi_Backup

%BEGIN Task Window, id_file_herstel
  task_win_eh(_Win,e_Menu(id_file_herstel,_ShiftCtlAlt),0):-
	close_alles(),
	retractall(vensterRCT(_, _)),
	retractall(weblayout(_, _, _, _, _, _, _)),
	win_onderdeelselectie_Create(_Win),	% 17.9.2011
	!.
%END Task Window, id_file_herstel

%BEGIN Task Window, id_Toernooi_Agenda_Dagstatus_instellingen
  task_win_eh(_Win,e_Menu(id_Toernooi_Agenda_Dagstatus_instellingen,_ShiftCtlAlt),0):-!,
  	List = ["small","medium","large","x-large", "10pt","11pt","12pt","13pt","14pt"],
  	dagstatusinst(StIn, _),
  	huidige(StIn, List, 0, Huidige),
	b_True = dlg_ListSelect("groote van het dagsatus font",List,Huidige,StrSel,_Index),
	logf('Z', dagstatusinst(StrSel, 0), nee),
	!.
%END Task Window, id_Toernooi_Agenda_Dagstatus_instellingen

%BEGIN Task Window, id_Toernooi_Inschrijfformulier_opgavenoverzicht
  task_win_eh(_Win,e_Menu(id_Toernooi_Inschrijfformulier_opgavenoverzicht,_ShiftCtlAlt),0):-
	dlg_opgaven_Create(_Win),
	!.
%END Task Window, id_Toernooi_Inschrijfformulier_opgavenoverzicht

%BEGIN Task Window, id_Toernooi_Inschrijfformulier_logo
  task_win_eh(_Win,e_Menu(id_Toernooi_Inschrijfformulier_logo,_ShiftCtlAlt),0):-
	win_PostEvent(_Win,e_menu(id_Toernooi_toernooiassistentkalender, 0)),
	!.
%END Task Window, id_Toernooi_Inschrijfformulier_logo

%BEGIN Task Window, id_Toernooi_Inschrijfformulier_uploadinstellingen
  task_win_eh(_Win,e_Menu(id_Toernooi_Inschrijfformulier_uploadinstellingen,_ShiftCtlAlt),0):-
	dlg_webupdateregelmaat_Create(_Win, b_False),
	!.
%END Task Window, id_Toernooi_Inschrijfformulier_uploadinstellingen

%BEGIN Task Window, id_Speler_selectie
  task_win_eh(_Win,e_Menu(id_Speler_selectie,_ShiftCtlAlt),0):-!,
	dlg_spelergegevensuitvoer_Create(_Win, [], eerste),
	!.
%END Task Window, id_Speler_selectie

%BEGIN Task Window, id_Toernooi_Inschrijfformulier_XML_lijst_afdrukken
  task_win_eh(_Win,e_Menu(id_Toernooi_Inschrijfformulier_XML_controlelijst_afdrukken,_ShiftCtlAlt),0):-!,
	%overgeslagenRapport(),		% twee rapporten
	xml_controlelijst(_Win),
	!.
%END Task Window, id_Toernooi_Inschrijfformulier_XML_controlelijst_afdrukken

%BEGIN Task Window, id_Toernooi_Agenda_Dagstatus_vandaag
  task_win_eh(_Win,e_Menu(id_Toernooi_Agenda_Dagstatus_vandaag,_ShiftCtlAlt),0):-!,
	dagrapport(File),
	format(DagBestand, "file:\\\\%s", File),
	Null = cast(api_hwnd,0),
	api_ShellExecute(Null,"open",DagBestand,"","",1),		% open in browser
	!.
%END Task Window, id_Toernooi_Agenda_Dagstatus_vandaag

%BEGIN Task Window, id_Internet
  task_win_eh(TaskWin,e_Menu(id_Internet,_ShiftCtlAlt),0):-!,
	eenIngedeeld(Ingedeeld, IText),
	finalebekend(FinaleBekend),
	%teruggable(Terug),
	%vandaagOfMorgen(morgen, _, IVOM),
%  TaskWin = vpi_GetTaskWin(),
  PNT = cursor_GetPos(TaskWin),
%  format(VTekst, " Dit is TA versie %s", versie),
  Menu = dyn_menu([
    txt(0,"- -   STAPPENPLAN   - -",0,b_True,0,[]),
    %txt(0,Vtekst,0,b_True,0,[]),
    %txt(0,"Help: info@toernooiklapper.nl",0,b_True,0,[]),
    separator,
    txt(id_Onderdeel_indelen,"1. INRICHTEN",0,b_True,0,[
    txt(id_help_ta_update,"ToernooiAssistent updaten",0,b_True,0,[]),
    txt(id_certificaat,"Certificaat ophalen van mijnKNLTB.nl",0,b_True,0,[]),
    txt(0,"Oud toernooibestand selecteren",0,b_True,0,[
      txt(id_Toernooi_nieuwe_start,"Hergebruik het huidige geopende toernooi",0,b_True,0,[]),
      txt(id_Toernooi_Bestand_open_een_ander,"Selecteer toernooi dat lijkt",0,b_True,0,[]),
      txt(id_Toernooi_Backup_terugzetten,"Bruikbare back-up laden",0,b_True,0,[])
    ]),
    txt(id_spelers_snoeien,"Verjaarde mailadressen/bondsnummers verwijderen",0,b_True,0,[]),
    txt(id_Toernooi_nieuwe_start,"Start nieuw toernooi (+voorjaars-schoonmaak)",0,b_True,0,[]),
    txt(id_Toernooi_eigenschappen,"Toernooikenmerken ",0,b_True,0,[
      txt(id_Toernooi_eigenschappen,"Instellen ",0,b_True,0,[]),
      txt(idr_Servit2definities,"Toernooinummer/certificaat beheer",0,b_True,0,[])
    ]),
    txt(id_Toernooi_beleid,"Beleid instellen",0,b_True,0,[]),
    txt(id_Toernooi_kalender,"Kalender aanpassen",0,b_True,0,[]),
    txt(id_Onderdeel_eigenschappen,"Onderdelen instellen (naam, inschrijfgeld, etc.)\tAlt-K",0,b_True,0,[]),
    txt(0, "Deelnemer-/ledenbestand (optioneel)",0,b_True,0,[
      txt(id_Speler_knltbdownload,"Importeren (KNLTB vrngng download)",0,b_True,0,[])
      ]),
    txt(idt_internet,"Inschrijfformulier maken en => Internet",0,b_True,0,[]),
    txt(idr_reglement,"Reglement toevoegen (optioneel)",0,b_True,0,[]),
    txt(0,"Uitnodigingen sturen",0,b_True,0,[
       txt(id_Speler_selectie, "aan wie?",0,b_True,0,[
         txt(id_Speler_selectie,"&Speciale selectie maken ...",0,b_True,0,[])
       ]),
       txt(id_Speler_gegevens, "per brief (etiket of mailmerge)\tF2",0,b_True,0,[]),
       txt(id_Speler_gegevens, "per e-mail of SMS\tF2",0,b_True,0,[])
      ]),
    txt(id_Speler_Betalingen_Incasso_proef, "Alvast een proefincasso doen",0,b_True,0,[])
    ]),
    separator,
    txt(id_Onderdeel_indelen,"2. INSCHRIJVEN",0,b_True,0,[
    txt(id_Onderdeel_indelen,"Inschrijvingen",0,b_True,0,[
      txt(id_Speler_gegevens, "invoeren vanaf papier\tF2",0,b_True,0,[]),
      separator,
      txt(id_Speler_opgaven, "Inschrijvingen Control Center\tAlt-X",0,b_True,0,[])
      ]),
    txt(id_Speler_gegevens, "Spelerformulieren",0,b_True,0,[
      txt(id_Speler_gegevens,"inzien/toevoegen\tF2",0,b_True,0,[]),
      %txt(id_Speler_Checken_nog_geen_formulier,"formulier nog niet gezien",0,b_True,0,[]),
      txt(id_Speler_selectie,"eerst een spelerselectie maken",0,b_True,0,[]),
      txt(id_Speler_gegevens,"afdrukken",0,b_True,0,[])
    ]),
    txt(id_Speler_opgaven, "Inschrijvingen checken",0,b_True,0,[
      txt(idcd_afdruk, "Lijst van geboekte inschrijvingen (ratingrapport)",0,b_True,0,[]),
      txt(id_Toernooi_Inschrijfformulier_XML_controlelijst_afdrukken, "Lijst van Internet-inschrijvingen",0,b_True,0,[]),
      txt(id_Speler_gegevens, "Gelijkbedoelde spelers samenvoegen\tF2",0,b_True,0,[]),
      txt(idr_verhcheckuitleg,"Verhinderingen checken per email",0,b_True,0,[])
    ]),
    txt(id_Toernooi_financien, "Aantallen inschrijvingen + financiën",0,b_True,0,[])
    ]),
    separator,
    txt(id_Onderdeel_indelen,"3. INDELEN/LOTEN",0,b_True,0,[
    txt(idcd_afdruk,"Rapport/afdruk van inschrijvingen (ratingrapport)",0,b_True,0,[]),
    txt(id_Onderdeel_indelen,"Scherm van inschrijvingen , zie \"(ver)Plaats\"\tAlt-D",0,b_True,0,[]),
    txt(idr_verhcheckuitleg,"Verhinderingen checken per email (Alle onderdelen)",0,b_True,0,[]),
    txt(id_Toernooi_baanbeschikbaarheid,"Baanbeschikbaarheid (i.v.m. loten/uitloten)",0,b_True,0,[]),
    txt(id_Onderdeel_indelen,"Eventueel uitloten, zie \"(ver)Plaats\"\tAlt-D",0,b_True,0,[]),
    txt(id_Onderdeel_indelen,"Plaatsen, zie \"(ver)Plaats\"\tAlt-D",0,b_True,0,[]),
    txt(id_Onderdeel_indelen,"Indelen en loten (+ KNLTB check)",0,b_True,0,[
      txt(id_Onderdeel_indelen,"Indelen en loten\tAlt-D",0,b_True,0,[]),
      txt(id_Onderdeel_indelen,"3/4 plaats\tAlt-D",0,b_True,0,[]),
      txt(id_Onderdeel_indelen,"1 poule met extra finale\tAlt-D",0,b_True,0,[]),
      txt(id_Onderdeel_indelen,"splitsen in meerdere poules\tAlt-D",0,b_True,0,[]),
      txt(id_Onderdeel_indelen,"schema's veranderen (wijzigopstelling)\tAlt-D",0,b_True,0,[]),
      txt(id_Onderdeel_indelen,"verliezersronde, etc....\tAlt-D",0,b_True,0,[]),
      txt(idcd_afdruk,"open plaatsen rapport",0,b_True,0,[]),
      txt(id_Onderdeel_indelen,"open plaatsen opvullen\tAlt-D",0,b_True,0,[])
    ]),
    txt(id_onderdeel,"Schema's publiceren",0,b_True,0,[
      txt(id_Onderdeel_indelen,"Op het scherm",0,Ingedeeld,0,[]),
      txt(id_Onderdeel_Print_via_browser,"Afdrukken",0,Ingedeeld,0,[]),
      txt(id_Onderdeel_Webpaginas_uploaden,IText,0,b_True,0,[]),
      txt(id_Toernooi_toernooiassistentkalender,"Beheren op toernooiklapper",0,Ingedeeld,0,[])
      ])
    %txt(id_help_Online_documentatie_deel2,"Beschrijving op Internet",0,b_True,0,[])
    ]),
    separator,
    txt(id_Onderdeel_indelen,"-  BONDSGEDELEGEERDE",0,b_True,0,[
      txt(idcd_afdruk,"Rating-rapport (mailen)",0,b_True,0,[]),
      txt(id_Toernooi_voortgang,"Voortgangsrapport",0,Ingedeeld,0,[]),
      txt(id_Toernooi_einduitslagen_onregelm,"Rapport onregelmatige uitslagen",0,Ingedeeld,0,[]),
      txt(id_Toernooi_einduitslagen_KNLTB,"Rapport Einduitslagen => KNLTB",0,Ingedeeld,0,[])
    ]),
    separator,
    txt(id_Onderdeel_indelen,"4. PLANNEN",0,b_True,0,[
    txt(id_Toernooi_baanbeschikbaarheid,"Baanbeschikbaarheid instellen",0,b_True,0,[]),
    txt(0,"Bloktijden (algemene verhinderingen)",0,b_True,0,[
      txt(id_Speler_bloktijden,"per leeftijdsgroep",0,b_True,0,[]),
      txt(id_Onderdeel_bloktijden,"per onderdeel",0,Ingedeeld,0,[])
    ]),
      txt(id_Wedstrijd_overzicht,"-overzicht van een wedstrijd (vanuit schema)\tF7",0,b_True,0,[]),
    txt(0,"Plannen (eerste rondes)",0,b_True,0,[
      txt(id_Onderdeel_Plan_automatisch,"Auto",0,Ingedeeld,0,[]),
      txt(id_Onderdeel_Plan_per_poule,"Per poule",0,Ingedeeld,0,[]),
      txt(id_Onderdeel_bloktijden,"Per wedstrijd (vanuit schema)\tF5",0,Ingedeeld,0,[]),
      txt(0,"Planning wissen",0,Ingedeeld,0,[
        txt(id_Onderdeel_Wis_plan_periode,"In een periode (regen?)",0,Ingedeeld,0,[]),
        txt(id_Onderdeel_Wis_plan_alle,"Alle planning (opnieuw?)",0,Ingedeeld,0,[]),
        txt(0,"Per wedstrijd (diverse plaatsen)",0,Ingedeeld,0,[])
      ])
    ]),
    txt(0,"Planrapportage",0,Ingedeeld,0,[
      txt(id_Toernooi_voortgang,"Voortgang",0,Ingedeeld,0,[]),
      txt(id_Toernooi_Controle_knelpunten,"Knelpunten",0,Ingedeeld,0,[]),
      txt(id_Toernooi_Controle_spelerbeschikbaarheid,"Spelerbezetting",0,Ingedeeld,0,[])
    ]),
    txt(0,"Agenda",0,b_True,0,[
      txt(idt_agenda,"Inzien",0,b_True,0,[]),
      txt(id_Toernooi_Controle_bezwaren,"Controleren",0,b_True,0,[])
      %txt(id_Toernooi_Controle_enkel_na_dubbel,"Enkel na dubbel",0,Ingedeeld,0,[]),
      %txt(id_Toernooi_Controle_dubbel_na_enkel,"Dubbel na enkel",0,Ingedeeld,0,[])
      %txt(id_Onderdeel_Plan_meer_dan2uur,"te &Lang wachten",0,Ingedeeld,0,[])
      ]),
    txt(0,"Oproepen/waarschuwen per",0,b_True,0,[
      txt(id_Speler_Oproepen_lijst,"E-mail of SMS vanaf het scherm\tF6",0,b_True,0,[]),
      txt(id_Speler_Oproepen_brieven,"Brief",0,b_True,0,[]),
      txt(id_Speler_Oproepen_etiketten,"Etiket",0,b_True,0,[])
      ])
    %txt(id_help_Online_documentatie_deel3,"Beschrijving op Internet",0,b_True,0,[]),
    %txt(id_help_locatieplanning,"Locatieplanning",0,b_True,0,[])
    ]),
    separator,
    txt(id_Onderdeel_indelen,"-  PUBLICEREN",0,b_True,0,[
    txt(0,"Op papier",0,Ingedeeld,0,[
      txt(id_Toernooi_Agenda__lijst,"Agenda",0,Ingedeeld,0,[]),
      txt(id_Toernooi_Agenda_Dagstatus_vandaag,"Dagstatus",0,Ingedeeld,0,[]),
      txt(id_Onderdeel_Print_via_browser,"Schema's",0,Ingedeeld,0,[]),
      txt(id_Toernooi_finales1,"Finalepagina",0,Finalebekend,0,[])
      ]),
      txt(id_Onderdeel_Webpaginas_uploaden,IText,0,b_True,0,[]),
      txt(id_Onderdeel_Webpaginas_uploadinstellingen,"Auto-upload naar Web (toernooiklapper)",0,Ingedeeld,0,[]),
      txt(id_Toernooi_toernooiassistentkalender,"Beheren op toernooiklapper",0,b_True,0,[])
    ]),
    separator,
    txt(id_Onderdeel_indelen,"5. SPELEN",0,b_True,0,[
    txt(id_Toernooi_uitslagen,"Uitslagen invoeren en doorplannen\tF8",0,b_True,0,[]),
    txt(id_Onderdeel_indelen,"Schema's wijzigen na bijv. w.o. (Wijzigopstelling)",0,b_True,0,[]),
    txt(id_banentool,"Banentool\tF8",0,b_True,0,[]),
    txt(id_Speler_Oproepen_lijst,"Oproepen/waarschuwen per e-mail, SMS of telefoon\tF6",0,b_True,0,[]),
    txt(id_Onderdeel,"Schema's + planning publiceren",0,b_True,0,[
      txt(id_Onderdeel_vertoon,"Op het scherm",0,Ingedeeld,0,[]),
      txt(id_Onderdeel_Print_via_browser,"Afdrukken\tCtrl-P",0,Ingedeeld,0,[]),
      txt(id_Onderdeel_Webpaginas_uploaden,IText,0,b_True,0,[])
      ]),
    txt(id_Speler_wedstrijden, "Deelnemers opzoeken\tF3",0,b_True,0,[]),
    txt(id_Toernooi_voortgang,"Voortgangsrapport",0,Ingedeeld,0,[]),
    txt(id_Onderdeel_Plan_een_voor_een, "Wat nog gepland moet worden\tF4",0,b_True,0,[]),
    txt(id_Toernooi_baanbeschikbaarheid,"Baanbeschikbaarheid aanpassen",0,b_True,0,[])
/*    txt(0,"Beschrijving op Internet",0,b_True,0,[
      txt(id_help_Online_documentatie_deel3,"Plannen en uitslagen verwerken",0,b_True,0,[]),
      txt(id_help_Online_documentatie_deel5,"Aan de wedstrijdtafel en Banentool",0,b_True,0,[]),
      txt(id_help_Online_documentatie_deel4,"Internetinstellingen",0,b_True,0,[])
    ])*/
    ]),
    separator,
    txt(id_Onderdeel_indelen,"6. KLAAR!",0,b_True,0,[
    txt(id_Toernooi_einduitslagen_KNLTB,"Einduitslagen => KNLTB",0,b_True,0,[]),
    txt(0,"Eindstand van",0,b_True,0,[
      txt(id_Toernooi_einduitslagen_1_en_2,"Winnaars (1 en 2)",0,Ingedeeld,0,[]),
      txt(id_Toernooi_einduitslagen_nrs_14,"Nummers 1 t/m 4",0,Ingedeeld,0,[]),
      txt(id_Toernooi_einduitslagen_deelnemers,"Deelnemers",0,Ingedeeld,0,[])
    ]),
    txt(id_Toernooi_finales1,"Finalepagina",0,Finalebekend,0,[]),
    txt(id_Toernooi_einduitslagen_onregelm,"Onregelmatighedenrapport",0,Ingedeeld,0,[]),
    separator,
    txt(id_Toernooi_Backup_maken,"Laatste back-up maken",0,b_True,0,[]),
    txt(id_Onderdeel_Webpaginas_uploaden,"Laatste Upload naar Web",0,b_True,0,[]),
    txt(id_Toernooi_toernooiassistentkalender,"Archiveren (zip-bestand)",0,Ingedeeld,0,[])
    ]),
    separator,
    txt(id_Onderdeel_indelen,"-  BETALINGEN",0,b_True,0,[
    txt(0,"Voorbereiding",0,b_True,0,[
      txt(id_Toernooi_beleid,"Betalingen bijhouden",0,b_True,0,[]),
      txt(id_Onderdeel_eigenschappen,"Bedrag per onderdeel instellen\tAlt-K",0,b_True,0,[]),
      txt(id_Toernooi_financien,"Financiëel overzicht",0,b_True,0,[]),
      txt(0,"Incasso instellen",0,b_True,0,[
        txt(id_Toernooi_beleid,"Banknummers gebruiken",0,b_True,0,[]),
        txt(id_Speler_Betalingen_Incasso_proef,"Proefincasso",0,b_True,0,[])
      ])
      ]),
    txt(0,"Afrekenen ",0,b_True,0,[
      txt(id_Speler_selectie,"Lijst nog te betalen, via ...",0,b_True,0,[]),
      txt(0,"Etiketten/nota's",0,b_True,0,[
        txt(id_Speler_Betalingen_etiketten,"&Etiketten/nota's",0,b_True,0,[]),
        txt(id_Speler_Betalingen_Incasso_nog_niet_betaald,"&Incasso",0,b_True,0,[])
      ]),
      txt(id_Speler_Betalingen_boeken,"Via scherm",0,b_True,0,[]),
      txt(id_Speler_selectie,"Incasso ondertekeningslijst, via ...",0,b_True,0,[])
      %txt(id_Speler_Betalingen_Incasso_proef,"Incasso innen (Clieop)",0,b_True,0,[])
      ])
    %txt(id_help_Online_documentatie_deel3,"Beschrijving op Internet",0,b_True,0,[])
    ]),
    separator,
    txt(id_Onderdeel_indelen,"-  HUISHOUDELIJK",0,b_True,0,[
    txt(id_Toernooi,"Back-up maken",0,b_True,0,[
      txt(id_Toernooi_Backup_maken,"op diskette of harde schijf",0,b_True,0,[]),
      txt(id_Toernooi_toernooiMap_backup_emailen,"gedeeld en vervolgens eventueel emailen",0,b_True,0,[]),
      txt(id_Toernooi_extra_instellingen,"auto-back-up op diskette of schijf",0,b_True,0,[])
      ]),
    txt(id_Toernooi_Backup_terugzetten,"Back-up laden",0,b_True,0,[
      ]),
    txt(id_Toernooi_Bestand_open_een_ander,"Toernooiselectie",0,b_True,0,[]),
    separator,
    txt(id_Toernooi_beleid,"Beleid",0,b_True,0,[]),
    txt(id_Toernooi_eigenschappen,"Kenmerken",0,b_True,0,[]),
    txt(id_Toernooi_extra_instellingen,"Lettergroottes, extra instellingen",0,b_True,0,[]),
    txt(id_Toernooi_toernooiassistentkalender,"Internet toernooiinstellingen",0,b_True,0,[]),
    separator,
    txt(id_help_ta_update,"Programma-update",0,b_True,0,[])
    %txt(id_help_Online_documentatie_deel1,"Beschrijving op Internet",0,b_True,0,[])
    ]),
    separator,
    txt(0,"-  HELP! ",0,b_True,0,[
      txt(id_help_about,"- Info",0,b_True,0,[]),
      txt(0,"- Contact: info@toernooiklapper.nl",0,b_True,0,[]),
      txt(id_help_veel_gestelde_vragen, "- Informatie op Internet",0,b_True,0,[]),
      txt(id_help_contents,"-  Lokale help",0,b_True,0,[])
      ])
    ]),
  PNT = pnt(R, H), R1 = R + 20,
  menu_PopUp(TaskWin,Menu, pnt(R1, H), align_Left), !.
%END Task Window, id_Internet

  task_win_eh(_Win,e_Menu(idr_verhcheckuitleg,_ShiftCtlAlt),0):-
	Aanw = "Selecteer ALLE onderdelen in speciale selectie\nen stuur een bulkmail met model\n\"opgaven en verhinderingen checken\"\naan de resulterende lijst.\nStuur deze mail een paar dagen voordat u de loting gaat doen!",
	1 = dlg_MessageBox( "Opgaven checken", Aanw, mesbox_iconinformation, mesbox_buttonsokcancel, mesbox_defaultfirst, mesbox_suspendapplication ),
	win_PostEvent(_Win,e_menu(id_speler_selectie, 0)),
	!.
  task_win_eh(_Win,e_Menu(idcd_afdruk,_ShiftCtlAlt),0):-
	dlg_indelingsrapport_Create(_Win, 2, "Rapport"),  
	!.

%BEGIN Task Window, id_Toernooi_Agenda_uitslagbriefjes
  task_win_eh(_Win,e_Menu(id_Toernooi_Agenda_uitslagbriefjes,_ShiftCtlAlt),0):-
	dlg_agenda_Create(_Win, 4),
	!.
%END Task Window, id_Toernooi_Agenda_uitslagbriefjes

%BEGIN Task Window, id_Toernooi_finales1
  task_win_eh(_Win,e_Menu(id_Toernooi_finales1,_ShiftCtlAlt),0):-
	finalesAfdruk(),
	!.
%END Task Window, id_Toernooi_finales1

%BEGIN Task Window, id_Toernooi_finales
  task_win_eh(_Win,e_Menu(id_Toernooi_finales,_ShiftCtlAlt),0):-
	finalesAfdruk(),
	!.
%END Task Window, id_Toernooi_finales

%BEGIN Task Window, id_Onderdeel_Wis_plan_periode
  task_win_eh(_Win,e_Menu(id_Onderdeel_Wis_plan_periode,_ShiftCtlAlt),0):-
	dlg_onderdplanmultwis_Create(_Win),
	!.
%END Task Window, id_Onderdeel_Wis_plan_periode

%BEGIN Task Window, id_Onderdeel_Wis_plan_alle
  task_win_eh(_Win,e_Menu(id_Onderdeel_Wis_plan_alle,_ShiftCtlAlt),0):-!,
	wisplanAlle(_Win),
	!.
%END Task Window, id_Onderdeel_Wis_plan_alle

%BEGIN Task Window, id_Speler_Invoer_Spelergegevens_knltb_excel_bestand
  task_win_eh(_Win,e_Menu(id_Speler_Invoer_Spelergegevens_knltb_excel_bestand,_ShiftCtlAlt),0):-!,
	dlg_MessageBox( "Invoer", "een Excel-bestand (xls) moet eerst\ndmv Excel gesaved worden in csv-formaat\nen kan daarna ingelezen worden!", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	!.
%END Task Window, id_Speler_Invoer_Spelergegevens_knltb_excel_bestand

%BEGIN Task Window, id_Toernooi_einduitslagen_KNLTB
  task_win_eh(_Win,e_Menu(id_Toernooi_einduitslagen_KNLTB,_ShiftCtlAlt),0):-!,
	einduitslagenXML(),	
	!.
%END Task Window, id_Toernooi_einduitslagen_KNLTB

%BEGIN Task Window, id_Onderdeel_Webpaginas_uploadinstellingen
  task_win_eh(_Win,e_Menu(id_Onderdeel_Webpaginas_uploadinstellingen,_ShiftCtlAlt),0):-!,
	dlg_webupdateregelmaat_Create(_Win, b_True),
	!.
%END Task Window, id_Onderdeel_Webpaginas_uploadinstellingen

%BEGIN Task Window, e_EndSession
  task_win_eh(_Win,e_EndSession(_AbortPossible),0):-!,
	assert(skip_post()),
	fail.
%END Task Window, e_EndSession

%BEGIN Task Window, id_Toernooi_toernooiMap_backup_emailen
  task_win_eh(_Win,e_Menu(id_Toernooi_toernooiMap_backup_emailen,_ShiftCtlAlt),0):-
        backup("email", nee),  % niet silent
	!.
%END Task Window, id_Toernooi_toernooiMap_backup_emailen

%BEGIN Task Window, id_Onderdeel_Webpaginas_uploaden
  task_win_eh(_Win,e_Menu(id_Onderdeel_Webpaginas_uploaden,_ShiftCtlAlt),0):-
	autowebP(_Regelmaat,HTMLFTP,_Waarschuwen,_, _, _,_,_Vasteverb,_Hoevaak, _),
	waarnaar(HTMLFTP, Upload), 
	dlg_maak_webpaginas_Create(_Win, Upload, [], b_False),
	!.
%END Task Window, id_Onderdeel_Webpaginas_uploaden

%BEGIN Task Window, id_Onderdeel_Webpaginas_opsturen_logos
  task_win_eh(_Win,e_Menu(id_Onderdeel_Webpaginas_opsturen_logos,_ShiftCtlAlt),0):-!,
	win_PostEvent(_Win,e_menu(id_Toernooi_toernooiassistentkalender, 0)),
	!.
%END Task Window, id_Onderdeel_Webpaginas_opsturen_logos

%BEGIN Task Window, id_Toernooi_toernooiassistentkalender
  task_win_eh(_Win,e_Menu(id_Toernooi_toernooiassistentkalender,_ShiftCtlAlt),0):-!,
	tkalrequest1("top"),
	!.
%END Task Window, id_Toernooi_toernooiassistentkalender

%BEGIN Task Window, id_file_journaal_bekijken
  task_win_eh(_Win,e_Menu(id_file_journaal_bekijken,_ShiftCtlAlt),0):-
	getFolder(_Dir,Toernooien,_,_),
	filenamepath(FullName, Toernooien, jourconst),
	existfile(FullName),
	TaskWin = vpi_GetTaskWin(),
	file_str(FullName,Text),
	format(Titel, "Bestand: %s", FullName),
        win_editor_Create(Taskwin, Text, Titel),
	!.
%END Task Window, id_file_journaal_bekijken



%BEGIN Task Window, id_Toernooi_Controle_spelerbeschikbaarheid
  task_win_eh(_Win,e_Menu(id_Toernooi_Controle_spelerbeschikbaarheid,_ShiftCtlAlt),0):-!,
        drukteRapport(1),	% 0 = zonder rapport
	!.
%END Task Window, id_Toernooi_Controle_spelerbeschikbaarheid3

%BEGIN Task Window, id_Toernooi_inschrijfformulier_instellen
  task_win_eh(_Win,e_Menu(id_Toernooi_inschrijfformulier_instellen,_ShiftCtlAlt),0):-
	dlg_inschralg_Create(_Win),
	!.
%END Task Window, id_Toernooi_inschrijfformulier_instellen

%BEGIN Task Window, idt_agenda
  task_win_eh(_Win,e_Menu(idt_agenda,_ShiftCtlAlt),0):-!,
	planMetAgenda(),
	!.
%END Task Window, idt_agenda


%BEGIN Task Window, id_file_herstel_vensters
  task_win_eh(_Win,e_Menu(id_file_herstel,_ShiftCtlAlt),0):-!,
	close_alles(),
	retractall(vensterRCT(_, _)),
	%retractall(vensterRCT(onderdeelselectvenster,_)),
	%retractall(venster(_)),
	retractall(weblayout(_, _, _, _, _, _, _)),
	%retractall(vensterRCT(browsevenster, _)),
	!.
%END Task Window, id_file_herstel_vensters

%BEGIN Task Window, id_Onderdeel_Print_via_browser
  task_win_eh(_Win,e_Menu(id_Onderdeel_Print_via_browser,_ShiftCtlAlt),0):-!,
	dlg_maak_webpaginas_Create(_Win, afdruk, [], b_False),
	!.
%END Task Window, id_Onderdeel_Print_via_browser

%BEGIN Task Window, id_Onderdeel_Print__direct
  task_win_eh(_Win,e_Menu(id_Onderdeel_Print__direct,_ShiftCtlAlt),0):-
	dlg_print_onderdelen_Create(_Win,Selectie, []),
	not(Selectie = []),
	loadPrintConfig(),
	printOnderdelen(Selectie),
	!.
%END Task Window, id_Onderdeel_Print__direct

%BEGIN Task Window, id_Speler_knltbdownload
  task_win_eh(_Win,e_Menu(id_Speler_knltbdownload,_ShiftCtlAlt),0):-!,
        spelers_invoer(_Win, 5),  % komma-gescheiden 
	!.
%END Task Window, id_Speler_knltbdownload

%BEGIN Task Window, id_Speler_Uitvoer_email_aan_allen
  task_win_eh(_Win,e_Menu(id_Speler_Uitvoer_email_aan_allen,_ShiftCtlAlt),0):-
	emailuitvoer(_Win),
	!.
%END Task Window, id_Speler_Uitvoer_email_aan_allen

%BEGIN Task Window, id_Speler_bloktijden
  task_win_eh(_Win,e_Menu(id_Speler_bloktijden,_ShiftCtlAlt),0):-
	dlg_bloktijden_per_leeftijd_Create(_Win),
	!.
%END Task Window, id_Speler_bloktijden

%BEGIN Task Window, idb_todo
  task_win_eh(_Win,e_Menu(idb_todo,_ShiftCtlAlt),0):-
	win_kladblok_Create(_Win),
	!.
%END Task Window, idb_todo

%BEGIN Task Window, id_Speler_Checken_soorten
  task_win_eh(_Win,e_Menu(id_Speler_Checken_soorten,_ShiftCtlAlt),0):-!,
	filter(),
	!.
%END Task Window, id_Speler_Checken_soorten

%BEGIN Task Window, idr_planoverzmin
  task_win_eh(_Win,e_Menu(idr_planoverzmin,_ShiftCtlAlt),0):-!,
	win_uitslageninvoer_Create(_win, 0, 0),
	!.
%END Task Window, idr_planoverzmin


%BEGIN Task Window, id_Speler_Invoer_Club_vaste_kolommen
  task_win_eh(_Win,e_Menu(id_Speler_Invoer_Club_vaste_kolommen,_ShiftCtlAlt),0):-!,
	clubs_invoer(_Win),
	!.
%END Task Window, id_Speler_Invoer_Club_vaste_kolommen

%BEGIN Task Window, id_Speler_Invoer_Club_kommagescheiden
  task_win_eh(_Win,e_Menu(id_Speler_Invoer_Club_kommagescheiden,_ShiftCtlAlt),0):-!,
	clubs_invoer(_Win),
	!.
%END Task Window, id_Speler_Invoer_Club_kommagescheiden

%BEGIN Task Window, id_Speler_Invoer_Cl_dbf
  task_win_eh(_Win,e_Menu(id_Speler_Invoer_Cl_dbf,_ShiftCtlAlt),0):-!,
	clubs_invoer(_Win),
	!.
%END Task Window, id_Speler_Invoer_Cl_dbf

%BEGIN Task Window, id_Speler_Invoer_Sp_xml_formaat
  task_win_eh(_Win,e_Menu(id_Speler_Invoer_Sp_xml_formaat,_ShiftCtlAlt),0):-!,
	xml_invoer_file(),
        %spelers_invoer(_Win, 4),
	!.
%END Task Window, id_Speler_Invoer_Sp_xml_formaat

%BEGIN Task Window, id_Speler_Invoer_Sp_dbf_formaat
  task_win_eh(_Win,e_Menu(id_Speler_Invoer_Sp_dbf_formaat,_ShiftCtlAlt),0):-!,
        spelers_invoer(_Win, 3),
	!.
%END Task Window, id_Speler_Invoer_Sp_dbf_formaat

%BEGIN Task Window, id_Speler_Invoer_Sp_kommagescheiden
  task_win_eh(_Win,e_Menu(id_Speler_Invoer_Sp_kommagescheiden,_ShiftCtlAlt),0):-!,
        spelers_invoer(_Win,2),
	!.
%END Task Window, id_Speler_Invoer_Sp_kommagescheiden

%BEGIN Task Window, id_Speler_Invoer_Sp_vaste_kolom
  task_win_eh(_Win,e_Menu(id_Speler_Invoer_Sp_vaste_kolom,_ShiftCtlAlt),0):-!,
        spelers_invoer(_Win, 1),
	!.
%END Task Window, id_Speler_Invoer_Sp_vaste_kolom

%BEGIN Task Window, id_Speler_Uitvoer_inschrijvingen
  task_win_eh(_Win,e_Menu(id_Speler_Uitvoer_inschrijvingen,_ShiftCtlAlt),0):-!,
	dlg_opgaven_create(_Win),
	!.
%END Task Window, id_Speler_Uitvoer_inschrijvingen

%BEGIN Task Window, id_Speler_opgaven
  task_win_eh(_Win,e_Menu(id_Speler_opgaven,_ShiftCtlAlt),0):-!,
	dlg_opgaven_Create(_Win),
	!.
%END Task Window, id_Speler_opgaven

%BEGIN Task Window, id_file_printer_setup_overig
  task_win_eh(_Win,e_Menu(id_file_printer_setup_overig,_ShiftCtlAlt),0):-
	printer1(PrintConfig),
	print_SetConfig( PrintConfig ),
	dlg_PrintSetup(99, _SelectionType, _NoOfCopies,_FirstPage,_LastPage),
	PrintConfigx = print_GetConfig(),
	retractall(printer1(_)),
	assert(printer1(PrintConfigx)),
	!.
  task_win_eh(_Win,e_Menu(id_file_printer_setup_overig,_ShiftCtlAlt),0):-!,
	dlg_PrintSetup(99, _SelectionType, _NoOfCopies,_FirstPage,_LastPage),
	PrintConfigx = print_GetConfig(),
	retractall(printer1(_)),
	assert(printer1(PrintConfigx)),
	!.
%END Task Window, id_file_printer_setup_overig

%BEGIN Task Window, id_file_printer_setup
  task_win_eh(_Win,e_Menu(id_file_printer_setup,_ShiftCtlAlt),0):-
	printer1(PrintConfig),
	print_SetConfig( PrintConfig ),
	dlg_PrintSetup(99, _SelectionType, _NoOfCopies,_FirstPage,_LastPage),
	PrintConfigx = print_GetConfig(),
	retractall(printer(_)),
	assert(printer(PrintConfigx)),
	!.
  task_win_eh(_Win,e_Menu(id_file_printer_setup,_ShiftCtlAlt),0):-!,
	dlg_PrintSetup(99, _SelectionType, _NoOfCopies,_FirstPage,_LastPage),
	PrintConfig = print_GetConfig(),
	retractall(printer(_)),
	assert(printer(PrintConfig)),
	!.
%END Task Window, id_file_printer_setup

%BEGIN Task Window, id_Speler_Checken_Lidnummers_niet_geldig
  task_win_eh(_Win,e_Menu(id_Speler_Checken_Lidnummers_niet_geldig,_ShiftCtlAlt),0):-!,
	win_spelersel_Create(_Win, 7),
	%dupl_lidno(_Win, 3),
	!.
%END Task Window, id_Speler_Checken_Lidnummers_niet_geldig

%BEGIN Task Window, id_help_website
  task_win_eh(_Win,e_Menu(id_help_website,_ShiftCtlAlt),0):-
	Null = cast(api_hwnd,0),
  	api_ShellExecute(Null,"open","https://www.toernooiklapper.nl/","","",1),!.
%END Task Window, id_help_website

%BEGIN Task Window, id_Toernooi_Agenda__mailmerge
  task_win_eh(_Win,e_Menu(id_Toernooi_Agenda__mailmerge,_ShiftCtlAlt),0):-!,
	dlg_agenda_Create(_Win, 3),
	!.
%END Task Window, id_Toernooi_Agenda__mailmerge

%BEGIN Task Window, e_Timer
  task_win_eh(_Win,e_Timer(_TimerId),0):-
	retract(timerid1(_TimerId, Count)),
	NCount = Count + 1,
	assert(timerid1(_TimerId, NCount)),
	kladblokUpd(),
	%beep(),
	not(logIsOpen(_)),
	getControleresultaat(),
	back_up_auto(),
	upload_auto(_Win, NCount),
	uitslagendownload(b_false),
	loglock(lees, "timer"),
	logclose(),
	retract(dagstatusQueued(File, SDag)),
	dagstatusuploadX(File,SDag),
	!.
%END Task Window, e_Timer

%BEGIN Task Window, id_Toernooi_extra_instellingen
  task_win_eh(_Win,e_Menu(id_Toernooi_extra_instellingen,_ShiftCtlAlt),0):-!,
	dlg_toernooiinstellingen_Create(_Win, []),
	!.
%END Task Window, id_Toernooi_extra_instellingen

%BEGIN Task Window, id_Onderdeel_Plan_per_poule
  task_win_eh(_Win,e_Menu(id_Onderdeel_Plan_per_poule,_ShiftCtlAlt),0):-!,
	planOndSeq(_Win),
	!.
%END Task Window, id_Onderdeel_Plan_per_poule

%BEGIN Task Window, id_Onderdeel_Plan_een_voor_een
  task_win_eh(_Win,e_Menu(id_Onderdeel_Plan_een_voor_een,_ShiftCtlAlt),0):-!,
	win_uitslageninvoer_Create(_win, 0, 0),
	%dlg_onderdeelplanhand_Create(_Win),
	!.
%END Task Window, id_Onderdeel_Plan_een_voor_een

%BEGIN Task Window, id_Onderdeel_Plan_automatisch
  task_win_eh(_Win,e_Menu(id_Onderdeel_Plan_automatisch,_ShiftCtlAlt),0):-!,
	dlg_onderdplanmult_Create(_Win),
	!.
%END Task Window, id_Onderdeel_Plan_automatisch

%BEGIN Task Window, e_CloseRequest
  task_win_eh(_Win,e_CloseRequest,0):-
	win_SetText(_Win, "TA bezig met sluiten, svp computer niet uitzetten!"),
	stopdbrefresh(),
	close_alles(),
	%closebrowser(),
	skip_post(), !.		% meteen uit met de pret
  task_win_eh(_Win,e_CloseRequest,0):-
  	autowebP(Regelmaat,HTMLFTP, _Waarsch,  _, _, _, _, _Vasteverb, _Hoevaak, BijSluiten),
	Regelmaat = 1,
	BijSluiten = 1,
	uploadEvents(P0),
	belangrijkeEventsP(P1),
	P1 > P0 , !,
	retractall(uploadEvents(_)),
	assert(uploadEvents(P1)),
	waarnaar(HTMLFTP, Upload), 
	dlg_maak_webpaginas_Create(_Win, Upload, [], b_True).
  task_win_eh(_Win,e_CloseRequest,0):-
	save_SpSelWin(),
	save_UitslWin(),
	save_WaarschWin(),
	retractall(teprinten(_, _, _, _, _)),
	post(),
	%closefile(nawf),
	WsFlags = win_GetState(_Win),
	wmax(_Win, WsFlags, WsFlags),
	fail.
  task_win_eh(_Win,e_CloseRequest,0):-
  	geladen,
  	updateToernooidirsave(),
	VPIconf = getVPIini(),
	save(VPIconf,vpiconf),
	fail.
%END Task Window, e_CloseRequest

%BEGIN Task Window, e_User
  task_win_eh(_Win,e_User(kom_boven,_Ptr),0):-!,		% sluit de hele zaak
	win_BringToTop(_Win),
	!.
  task_win_eh(_Win,e_User(99,_Ptr),0):-!,			% sluit de hele zaak
	assert(skip_post),
	win_Destroy(_Win),
	!.
  task_win_eh(_Win,e_user(dotoernooiload,_Ptr),0):-
	introFromServer(Uit),
	1 = dlg_MessageBox( "Nieuwe versie", Uit, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
	updateTA(_Win),
	fail.
  task_win_eh(_Win,e_user(dotoernooiload,_Ptr),0):-
	toernooidirReset(),
	bepaalDatabase(_Win),					% 6.9.2008 catch error
	
	%writeToJournaal("Start programma."),
	trap(dlg_toernooiload_Create(_Win, "e_user(toernooiload", b_False,update),Err,checkTrap(_Win, Err)),
	!.
  task_win_eh(_Win,e_User(upd_toolb,_Ptr),0):-
	update_toolbar(_Win), !.
%END Task Window, e_User



%BEGIN Task Window, id_Speler_Oproep_print
  task_win_eh(_Win,e_Menu(id_Speler_Oproep_print,_ShiftCtlAlt),0):-!,
	oproeplijst(_Win),
	!.
%END Task Window, id_Speler_Oproep_print


%BEGIN Task Window, id_Onderdeel_Lijsten_oproepmailmerge
  task_win_eh(_Win,e_Menu(id_Onderdeel_Lijsten_oproepmailmerge,_ShiftCtlAlt),0):-!,
	onderdeelMM(_Win),
	!.
%END Task Window, id_Onderdeel_Lijsten_oproepmailmerge

%BEGIN Task Window, id_file_etiketten
  task_win_eh(_Win,e_Menu(id_file_etiketten,_ShiftCtlAlt),0):-!,
	dlg_etikesoort_Create(_Win),
	!.
%END Task Window, id_file_etiketten

%BEGIN Task Window, id_Onderdeel_Lijsten_opretiketten
  task_win_eh(_Win,e_Menu(id_Onderdeel_Lijsten_opretiketten,_ShiftCtlAlt),0):-!,
	onderdeelEtik(_Win, 5),
	!.
%END Task Window, id_Onderdeel_Lijsten_opretiketten

%BEGIN Task Window, id_Onderdeel_Lijsten_betetik
  task_win_eh(_Win,e_Menu(id_Onderdeel_Lijsten_betetik,_ShiftCtlAlt),0):-!,
	onderdeelEtik(_Win, 7),
	!.
%END Task Window, id_Onderdeel_Lijsten_betetik

%BEGIN Task Window, id_Onderdeel_Lijsten_rapport
  task_win_eh(_Win,e_Menu(id_Onderdeel_Lijsten_rapport,_ShiftCtlAlt),0):-!,
	dlg_onderdeelspelergegevens_Create(_Win),
	!.
%END Task Window, id_Onderdeel_Lijsten_rapport

%BEGIN Task Window, id_Onderdeel_plannieuws
  task_win_eh(_Win,e_Menu(id_Onderdeel_plannieuws,_ShiftCtlAlt),0):-!,
	plan_nieuws(),
	!.
%END Task Window, id_Onderdeel_plannieuws

%BEGIN Task Window, id_Onderdeel_uitslagen
  task_win_eh(_Win,e_Menu(id_Onderdeel_uitslagen,_ShiftCtlAlt),0):-!,
	weds_uitsl(),
	!.
%END Task Window, id_Onderdeel_uitslagen

%BEGIN Task Window, id_Speler_Uitvoer_Clubgegevens_kommagescheiden
  task_win_eh(_Win,e_Menu(id_Speler_Uitvoer_Clubgegevens_kommagescheiden,_ShiftCtlAlt),0):-!,
	clubs_uit(1),
	!.
%END Task Window, id_Speler_Uitvoer_Clubgegevens_kommagescheiden

%BEGIN Task Window, id_Speler_Uitvoer_Clubgegevens_vaste_kolommen
  task_win_eh(_Win,e_Menu(id_Speler_Uitvoer_Clubgegevens_vaste_kolommen,_ShiftCtlAlt),0):-!,
	clubs_uit(2),
	!.
%END Task Window, id_Speler_Uitvoer_Clubgegevens_vaste_kolommen


%BEGIN Task Window, id_Speler_Uitvoer_spelergegevens
  task_win_eh(_Win,e_Menu(id_Speler_Uitvoer_spelergegevens,_ShiftCtlAlt),0):-!,
	spelers_uit(),
	!.
%END Task Window, id_Speler_Uitvoer_spelergegevens

%BEGIN Task Window, id_Onderdeel_indelen
  task_win_eh(_Win,e_Menu(id_Onderdeel_indelen,_ShiftCtlAlt),0):-!,
	indelingX(),
	!.
%END Task Window, id_Onderdeel_indelen


%BEGIN Task Window, id_file_opendir
  task_win_eh(_Win,e_Menu(id_file_opendir,_ShiftCtlAlt),0):-
    writedevice(X),
    writedevice(screen),
    write("tasvp1 here"),
    writedevice(X),
    win_SendEvent(_Win,e_Menu(id_Toernooi_Bestand_open_een_ander,c_Nothing)),
	!.
%END Task Window, id_file_opendir

%BEGIN Task Window, id_Toernooi_Bestand_open_een_ander
  task_win_eh(_Win,e_Menu(id_Toernooi_Bestand_open_een_ander,_ShiftCtlAlt),0):-!,
	dlg_toernooiselectie_Create(_Win),
	%open_toernooi(_Win,1,_),
	!.
%END Task Window, id_Toernooi_Bestand_open_een_ander

%BEGIN Task Window, id_Toernooi_Bestand_maak_nieuw
  task_win_eh(_Win,e_Menu(id_Toernooi_Bestand_maak_nieuw,_ShiftCtlAlt),0):-!,
	dlg_toernooistart_Create(_Win),
	%nieuweToernooiMap(_Win, _), % cb_putstring
	!.
%END Task Window, id_Toernooi_Bestand_maak_nieuw

%BEGIN Task Window, id_Toernooi_Backup_maken
  task_win_eh(_Win,e_Menu(id_Toernooi_Backup_maken,_ShiftCtlAlt),0):-!,
	backup("", nee),  % niet silent
	!.
%END Task Window, id_Toernooi_Backup_maken

%BEGIN Task Window, id_Toernooi_Backup_terugzetten
  task_win_eh(_Win,e_Menu(id_Toernooi_Backup_terugzetten,_ShiftCtlAlt),0):-!,
	vorige(_Win),
	!.
%END Task Window, id_Toernooi_Backup_terugzetten

%BEGIN Task Window, id_Toernooi_Controle_knelpunten
  task_win_eh(_Win,e_Menu(id_Toernooi_Controle_knelpunten,_ShiftCtlAlt),0):-!,
	controleer(_Win, 1, "Knelpunten"),
	!.
%END Task Window, id_Toernooi_Controle_knelpunten

%BEGIN Task Window, id_Toernooi_Controle_enkel_na_dubbel
  task_win_eh(_Win,e_Menu(id_Toernooi_Controle_bezwaren,_ShiftCtlAlt),0):-!,
	controleer(_Win, 2, "Bezwaren"),
	!.
%END Task Window, id_Toernooi_Controle_bezwaren

%BEGIN Task Window, id_Toernooi_financien
  task_win_eh(_Win,e_Menu(id_Toernooi_financien,_ShiftCtlAlt),0):-
  	financieel(), !.
%END Task Window, id_Toernooi_financien

%BEGIN Task Window, id_Toernooi_voortgang
  task_win_eh(_Win,e_Menu(id_Toernooi_voortgang,_ShiftCtlAlt),0):-!,
	voortgang_c(Rapport),
	%write(Rapport), 
	get_TempDir(_, TempFile),
	file_str(TempFile, Rapport),
	dlg_rapport_Create(_Win, "Voortgang","Voortg",b_False), !.
%END Task Window, id_Toernooi_voortgang

%BEGIN Task Window, id_Toernooi_Agenda__etiket
  task_win_eh(_Win,e_Menu(id_Toernooi_Agenda__etiket,_ShiftCtlAlt),0):-!,
	dlg_agenda_Create(_Win, 1),
	!.
%END Task Window, id_Toernooi_Agenda__etiket

%BEGIN Task Window, id_Toernooi_Agenda__lijst
  task_win_eh(_Win,e_Menu(id_Toernooi_Agenda__lijst,_ShiftCtlAlt),0):-!,
	dlg_agenda_Create(_Win, 0),
	!.
%END Task Window, id_Toernooi_Agenda__lijst

%BEGIN Task Window, id_Speler_Oproepen_lijst
  task_win_eh(_Win,e_Menu(id_Speler_Oproepen_lijst,_ShiftCtlAlt),0):-
	%oproepen(ja),  
	win_waarschuwinvoer_Create(_Win, []),
	!.
%END Task Window, id_Speler_Oproepen_lijst

  task_win_eh(_Win,e_Menu(id_banentool,_ShiftCtlAlt),0):-
 	banentool(ja, _), 
	win_SendEvent(_Win,e_Menu(id_Toernooi_uitslagen,c_Nothing)), !.
  task_win_eh(_Win,e_Menu(id_banentool,_ShiftCtlAlt),0):-
	dlg_MessageBox( "Banentool", "Om het banentool te gebruiken moet je het aan zetten bij Toernooi, Beleid", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	!.

%BEGIN Task Window, e_InitMenu
  task_win_eh(_Win,e_InitMenu,0):-!,
	Fullfunc = b_True,
	notOnline(NotOnline),
	menu_Enable(_Win, id_Toernooi_einduitslagen_deelnemers, FullFunc),
	%menu_Enable(_Win, id_Toernooi_einduitslagen_winnaars, FullFunc),
	menu_Enable(_Win, id_Toernooi_einduitslagen_nrs_14, FullFunc),
	menu_Enable(_Win, id_Toernooi_einduitslagen_onregelm, FullFunc),
	menu_Enable(_Win, id_Toernooi_einduitslagen_KNLTB, FullFunc),
        menu_Enable(_Win, id_Toernooi_Bestand_maak_nieuw, NotOnline),
	eenIngedeeld(Ingedeeld, _),
	menu_Enable(_Win, id_Onderdeel_vertoon, Ingedeeld),
	menu_Enable(_Win, id_Speler_betalingen, b_True), % één hoger?
	menu_Enable(_Win, id_Toernooi_uitslagen, FullFunc),
	menu_Enable(_Win, id_Speler_betalingen_boeken, b_True),
	menu_Enable(_Win, id_Speler_betalingen_etiketten, b_True),
	menu_Enable(_Win, id_Speler_Betalingen_Incasso_nog_niet_betaald, b_True),
	finalebekend(Bekend),
	menu_enable(_Win, id_Toernooi_finales, Bekend),
	menu_enable(_Win, id_Toernooi_finales1, Bekend),
	menu_enable(_Win, id_Toernooi_Inschrijfformulier_downloaden_opgaven, b_true),	% 26.3.2005
	!.
%END Task Window, e_InitMenu

%BEGIN Task Window, id_Toernooi_nieuwe_start
  task_win_eh(_Win,e_Menu(id_Toernooi_nieuwe_start,_ShiftCtlAlt),0):-!,
 	dlg_toernooistart_Create(_Win),
 	win_PostEvent(_Win, e_User(upd_toolb,0)),
	spelerSelRefresh(),
	!.
%END Task Window, id_Toernooi_nieuwe_start

%BEGIN Task Window, id_Toernooi_beleid
  task_win_eh(_Win,e_Menu(id_Toernooi_beleid,_ShiftCtlAlt),0):-!,
	dlg_toernooibeleid_Create(_Win),
	!.
%END Task Window, id_Toernooi_beleid

%BEGIN Task Window, id_Toernooi_eigenschappen
  task_win_eh(_Win,e_Menu(id_Toernooi_eigenschappen,_ShiftCtlAlt),0):-!,
	dlg_toernooiexport_Create(_Win,_),
	!.
%END Task Window, id_Toernooi_eigenschappen

%BEGIN Task Window, id_Onderdeel_bloktijden
  task_win_eh(_Win,e_Menu(id_Onderdeel_bloktijden,_ShiftCtlAlt),0):-!,
	dlg_ondbloktijden_Create(_Win),
	!.
%END Task Window, id_Onderdeel_bloktijden

%BEGIN Task Window, id_Onderdeel_eigenschappen
  task_win_eh(_Win,e_Menu(id_Onderdeel_eigenschappen,_ShiftCtlAlt),0):-!,
	dlg_onderdeelkenmerken_Create(_Win),
%	dlg_onderdeeleigenschappen_Create(_Win),
	!.
%END Task Window, id_Onderdeel_eigenschappen

%BEGIN Task Window, id_Toernooi_baanbeschikbaarheid
  task_win_eh(_Win,e_Menu(id_Toernooi_baanbeschikbaarheid,_ShiftCtlAlt),0):-!,
	dlg_baanbeschikbaarheid_Create(_Win),
	!.
%END Task Window, id_Toernooi_baanbeschikbaarheid

%BEGIN Task Window, id_Toernooi_kalender
  task_win_eh(_Win,e_Menu(id_Toernooi_kalender,_ShiftCtlAlt),0):-!,
	dlg_dagen_Create(_WIN),
	!.
%END Task Window, id_Toernooi_kalender

%BEGIN Task Window, id_Speler_verenigingen
  task_win_eh(_Win,e_Menu(id_Speler_verenigingen,_ShiftCtlAlt),0):-!,
	dlg_clubstabel_Create(_Win),
	!.
%END Task Window, id_Speler_verenigingen

%BEGIN Task Window, id_Speler_eigenschappen
  task_win_eh(_Win,e_Menu(id_Speler_eigenschappen,_ShiftCtlAlt),0):-!,
	dlg_spelersoorten_Create(_Win),
	!.
%END Task Window, id_Speler_eigenschappen

%BEGIN Task Window, id_Speler_Checken_Lidnummers_dubbele
  task_win_eh(_Win,e_Menu(id_Speler_Checken_Lidnummers_dubbele,_ShiftCtlAlt),0):-!,
	win_spelersel_Create(_Win, 5),
	%dupl_lidno(_Win, 1),
	!.
%END Task Window, id_Speler_Checken_Lidnummers_dubbele

%BEGIN Task Window, id_Speler_Checken_ontbrekende_gegevens
  task_win_eh(_Win,e_Menu(id_Speler_Checken_ontbrekende_gegevens,_ShiftCtlAlt),0):-!,
 	%blanko_spelers(_Win, 1),
	win_spelersel_Create(_Win, 3),
	!.
%END Task Window, id_Speler_Checken_ontbrekende_gegevens

%BEGIN Task Window, id_Speler_wedstrijden
  task_win_eh(_Win,e_Menu(id_Speler_wedstrijden,_ShiftCtlAlt),0):-!,
	win_spelersel_Create(_Win, 1),	% Mode 1 = alleen deelnemers
	%spelerZoekOp..
	!.
%END Task Window, id_Speler_wedstrijden

%BEGIN Task Window, id_Speler_Betalingen_boeken
  task_win_eh(_Win,e_Menu(id_Speler_Betalingen_boeken,_ShiftCtlAlt),0):-!,
	spelerBetaal(_Win),
	!.
%END Task Window, id_Speler_Betalingen_boeken

  task_win_eh(_Win,e_Menu(id_Spelers_snoeien,_ShiftCtlAlt),0):-!,
	%LboxWin = win_GetCtlHandle(_Win, idc_spelerselect_lbox),
	%LboxList = lbox_GetAll(LboxWin),
	_Answer = dlg_MessageBox( "Snoeien", "klik op het menu-icoon in de nu volgende spelerlijst (F2)\nen kies \"verjaarde mailadressen/bondsnummers merken\".", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	win_PostEvent(_Win,e_menu(id_speler_gegevens, 0)),
	!.

%BEGIN Task Window, id_Speler_Uitvoer_Adresgegevens_mailmerge
  task_win_eh(_Win,e_Menu(id_Speler_Uitvoer_Adresgegevens_mailmerge,_ShiftCtlAlt),0):-!,
	dlg_spelergegevensuitvoer_Create(_Win, [], eerste),
	!.
%END Task Window, id_Speler_Uitvoer_Adresgegevens_mailmerge

%BEGIN Task Window, id_Speler_Uitvoer_Adresgegevens_etiketten
  task_win_eh(_Win,e_Menu(id_Speler_Uitvoer_Adresgegevens_etiketten,_ShiftCtlAlt),0):-!,
	dlg_spelergegevensuitvoer_Create(_Win, [], eerste),
	!.
%END Task Window, id_Speler_Uitvoer_Adresgegevens_etiketten

%BEGIN Task Window, id_Speler_Oproepen_etiketten
  task_win_eh(_Win,e_Menu(id_Speler_Oproepen_etiketten,_ShiftCtlAlt),0):-!,
	etikettenOproep(_Win),
	!.
%END Task Window, id_Speler_Oproepen_etiketten

%BEGIN Task Window, id_Speler_Betalingen_etiketten
  task_win_eh(_Win,e_Menu(id_Speler_Betalingen_etiketten,_ShiftCtlAlt),0):-!,
	etikettenBetaal(_Win),
	spelerBetaal(_Win),
	!.
%END Task Window, id_Speler_Betalingen_etiketten

%BEGIN Task Window, id_Speler_Betalingen_Incasso_nog_niet_betaald
  task_win_eh(_Win,e_Menu(id_Speler_Betalingen_Incasso_nog_niet_betaald,_ShiftCtlAlt),0):-!,
	betaalIncasso(b_false),  % alleen nog niet betaald
        spelerBetaal(_Win),
	!.
%END Task Window, id_Speler_Betalingen_Incasso_nog_niet_betaald

%BEGIN Task Window, id_Speler_Betalingen_Incasso_alle
  task_win_eh(_Win,e_Menu(id_Speler_Betalingen_Incasso_alle,_ShiftCtlAlt),0):-
	1 = dlg_MessageBox( "Title", "Een incasso voor ALLE spelers\nwaarvan het banknummer bekend is\n en waarvan incasso OK is aangevinkt?", mesbox_iconexclamation, mesbox_buttonsokcancel, mesbox_defaultfirst, mesbox_suspendapplication ),
	betaalIncasso(b_True),  % allemaal
        spelerBetaal(_Win),
	!.
%END Task Window, id_Speler_Betalingen_Incasso_alle

%BEGIN Task Window, id_Speler_Betalingen_Incasso_proef
  task_win_eh(_Win,e_Menu(id_Speler_Betalingen_Incasso_proef,_ShiftCtlAlt),0):-!,
	betaalIncassoProef(),
	!.
%END Task Window, id_Speler_Betalingen_Incasso_proef

%BEGIN Task Window, id_Speler_Oproepen_brieven
  task_win_eh(_Win,e_Menu(id_Speler_Oproepen_brieven,_ShiftCtlAlt),0):-!,
	mailmergeOproep(_Win),
	!.
%END Task Window, id_Speler_Oproepen_brieven

%BEGIN Task Window, id_file_open
  task_win_eh(_Win,e_Menu(id_file_open,_ShiftCtlAlt),0):-!,
	%get_toernooidir(Dir, _, _, _),
	writedevice(X),
	writedevice(screen),
	write("!"),
	getFolder(_,DirX,_,_),
	write("\nHier ", Dirx),
	writedevice(X),
	_FILENAME = dlgFileNameZeker("*.*",["Rapport","*.txt", "XML","*.xml"],"Open data",[],DirX,_OutListFiles),
	_OutListFiles = [FileName|_], 
	TaskWin = vpi_GetTaskWin(),
	file_str(FileName,Text),
	format(Titel, "Bestand: %s", FileName),
        win_editor_Create(Taskwin, Text, Titel),
        !.
%END Task Window, id_file_open

%BEGIN Task Window, id_Toernooi_uitslagen
  task_win_eh(_Win,e_Menu(id_Toernooi_uitslagen,_ShiftCtlAlt),0):-!,
	uitsl_verwrk(_Win),
	!.
%END Task Window, id_Toernooi_uitslagen

%BEGIN Task Window, id_Onderdeel_vertoon
  task_win_eh(_Win,e_Menu(id_Onderdeel_vertoon,_ShiftCtlAlt),0):- 
	win_onderdeelselectie_Create(_Win), !.
%END Task Window, id_Onderdeel_vertoon

%BEGIN Task Window, e_Destroy
  task_win_eh(_Win,e_Destroy,0):-
	close_alles(),
	progress_Unregister(),
	class_Destroy("PDCEDIT"), !.
  task_win_eh(_Win,e_Destroy,0).
%END Task Window, e_Destroy


%BEGIN Task Window, id_Speler_gegevens
  task_win_eh(_Win,e_Menu(id_Speler_gegevens,_ShiftCtlAlt),0):-!,
	win_spelersel_Create(_Win, 0),
	!.
%END Task Window, id_Speler_gegevens

%BEGIN Task Window, id_help_contents
  task_win_eh(_Win,e_Menu(id_help_contents,_ShiftCtlAlt),0):-!,
	%system("HH tasinet.chm::/Inhoud.htm",0,_), 
        project_ShowHelpContext("Inhoud"),
  	%vpi_ShowHelp("tasvp1.hlp"),
	!.
%END Task Window, id_help_contents

%BEGIN Task Window, id_help_about
  task_win_eh(Win,e_Menu(id_help_about,_ShiftCtlAlt),0):-
        %write("\nhier"),
	dlg_about_dialog_Create(Win),
	!.
%END Task Window, id_help_about

%BEGIN Task Window, id_file_exit
  task_win_eh(Win,e_Menu(id_file_exit,_ShiftCtlAlt),0):-
	retractall(teprinten(_, _, _, _, _)),
	post(),
	WsFlags = win_GetState(Win),
	wmax(Win, WsFlags, WsFlags),
	%maakRW(tasvpiini),
	geladen,
	updatetoernooidirsave(),
	VPIconf = getVPIini(),
	trap(save(VPIconf,vpiconf),_,true),
  	win_Destroy(Win),
	!.
  task_win_eh(Win,e_Menu(id_file_exit,_ShiftCtlAlt),0):-!,
  	win_Destroy(Win), !.
%END Task Window, id_file_exit


 
%BEGIN Task Window, e_Size
  task_win_eh(_Win,e_Size(_Width,_Height),0):-!,
IFDEF use_tbar
	toolbar_Resize(_Win),
ENDDEF
IFDEF use_message
	msg_Resize(_Win),
ENDDEF
	!.
%END Task Window, e_Size

/*  task_win_eh(_Win, e_Menu(idr_directimport, _), 0) :-
        xml_InschrijfInvoerNieuw(), !. */
  task_win_eh(_Win, e_Menu(idr_reglement, _), 0) :-
	tkalrequest1("reg"),  !.
  task_win_eh(_Win, e_Menu(idr_Servit2definities, _), 0) :-
		dlg_tidsynchronisatie_Create(_Win),
  !.
  task_win_eh(_Win, e_Menu(idr_logo, _), 0) :-
	tkalrequest1("logo"),  !.
  task_win_eh(_Win, e_Menu(idr_formcss, _), 0) :-
	tkalrequest1("fcss"),  !.
  task_win_eh(_Win, e_Menu(idr_sheetcss, _), 0) :-
	tkalrequest1("scss"),  !.
  task_win_eh(_Win, e_Menu(idr_onlinebu, _), 0) :-
	onlinebackup("eigen"), !.
  task_win_eh(_Win, e_Menu(idct_kwijt, _), 0) :-  % van on-line
	1 = dlg_MessageBox( "Toernooigegevens opvissen", "Aanwijzingen:\n\nOpen hierna een map met een naam die eindigt op een uitroepteken.\nZoek vervolgens in die map naar een bruikbaar tasn(.dat) bestand\nen open dat bij voorkeur in een nieuwe map.\n\nNB Het kan zijn dat u niets vindt!",  mesbox_iconinformation, mesbox_buttonsokcancel, mesbox_defaultfirst, mesbox_suspendapplication ),
	get_tempdir(Temp, _),
	Fn = dlgFileNameZeker("*.dat",["toernooigegevens","*.dat;*.gz"],"",[],Temp,_OutListFiles),
	writedevice(X),
	writedevice(screen),
	write(Fn),
	writedevice(X),  !.
	%vervangDatXX(_Win, Fn), !.
  task_win_eh(_Win, e_Menu(idr_sheets, _), 0) :-
	%providerx(Prov, _, "", _),
        naam(_ToernooiNaam, _Nr, _Datum, TicketI, _District, _),
        maketicket(TicketI, Ticket),
	format(URL, "https://www.toernooiklapper.nl/toernooi/%s/",Ticket),
	Null = cast(api_hwnd,0),
  	api_ShellExecute(Null,"open",URL,"","",1),
  !.
  task_win_eh(_Win,e_Menu(idc_gewaarschuwd,_ShiftCtlAlt),0):-
	1 = dlg_MessageBox( "Gewaarschuwd", "Allemaal gewaarschuwd?\nMaak voor de zekerheid eerst een back-up", mesbox_iconexclamation, mesbox_buttonsokcancel, mesbox_defaultfirst, mesbox_suspendapplication ),
	cursor_setWait(),
	vpi_ProcessEvents(),
	allemaalgewaarschuwd(), !.
  task_win_eh(_Win,e_Menu(idcp_deelname,_ShiftCtlAlt),0):-	% NB een leenconstante
	1 = dlg_MessageBox( "Gewaarschuwd", "Allemaal nog niet gewaarschuwd?\nMaak voor de zekerheid eerst een back-up", mesbox_iconexclamation, mesbox_buttonsokcancel, mesbox_defaultfirst, mesbox_suspendapplication ),
	cursor_setWait(),
	vpi_ProcessEvents(),
	nognietgewaarschuwd(), !.
  task_win_eh(_Win,e_Menu(id_Speler_betaald_reset,_ShftCtrlAlt),0):-
        resetbetalingen(),
  	update_betalingen(),
	not(sp2(_,_,_,_,_,_,_,ja,_, _, _, _, _, _, _, _, _, _, _, _,_,_,_,_,_,_,_)),
	dlg_MessageBox( "Betalingen resetten", "ALLE spelers zijn op nog NIET betaald gezet!\nEr is ook een herstelpunt (back-up) gemaakt.", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), !.
   task_win_eh(_Win,e_Menu(id_Certificaat,_ShftCtrlAlt),0):-
	dlg_MessageBox( "Certificaat", "Het certificaat is alleen nodig voor een bondstoernooi,\ndownload het en voer het in bij het aanmaken van een\nnieuw toernooi.\nNB Gebruik steeds een vers certificaat!", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),!.

%BEGIN Task Window, id_Toernooi_einduitslagen_deelnemers
  task_win_eh(_Win,e_Menu(Id,_ShiftCtlAlt),0):-	%als laatste (veiliger)
	einduitslagen(_Win, Id),
	!.
%END Task Window, id_Toernooi_einduitslagen_deelnemers

%END_WIN Task Window



/***************************************************************************
	Invoking on-line Help
***************************************************************************/

  project_ShowHelpContext(HelpTopic):-
	syspath(ExeStartupPath,_ProgName),
  	format(File, "%stasinet.chm::/%s.html", ExeStartupPath, HelpTopic),
  	%write("\n", File),
  	TW = vpi_getTaskwin(), 
	helpinvoke(TW, File, 0, 0).

	%Null = cast(api_hwnd,0),
  	%api_ShellExecute(Null,"open","HH",File,"",0).


  	%format(Cmd, "HH tasinet.chm::/%s.html", HelpTopic), 
	%system(Cmd,0,_). 
  	
  	%vpi_ShowHelpContext("tasvp1.hlp",HelpTopic).

/***************************************************************************
	Main Goal
***************************************************************************/

GOAL

IFDEF use_mdi
  vpi_SetAttrVal(attr_win_mdi,b_true),
ENDDEF
IFDEF ws_win
  IFDEF use_3dctrl
    vpi_SetAttrVal(attr_win_3dcontrols,b_true),
  ENDDEF
ENDDEF  
  vpi_Init(task_win_Flags,task_win_eh,task_win_Menu,"tasvpi",task_win_Title).

%BEGIN_TLB Project toolbar, 21:38:17-24.1.2015, Code automatically updated!
/**************************************************************************
	Creation of toolbar: Project toolbar
**************************************************************************/

clauses

  tb_project_toolbar_Create(_Parent):-
ifdef use_tbar
	toolbar_create(tb_top,0xC0C0C0,_Parent,
		[tb_ctrl(id_Toernooi_Bestand_open_een_ander,pushb,idb_openw,idb_open_dn,idb_open_up,"Toernooibestand;Toernooibestandselectie",1,1),
		 tb_ctrl(id_Toernooi_Backup,pushb,idb_loadw,idb_loadd,idb_loadd,"Back-up = herstelpunt;Back-up (herstelpunt) maken/laden",1,1),
		 tb_ctrl(idt_internet,pushb,idb_internetw,idb_web,idb_web,"Internet;Internet",1,1),
		 tb_ctrl(idb_todo,pushb,idb_todow,idb_todop,idb_tododd,"Kladblok;Kladblok",1,1),
		 separator,
		 tb_ctrl(idt_inrichten,pushb,idb_instellingenq,idb_instellingendown,idb_grijs,"Toernooi inrchten;Toernooi inrichten",1,1),
		 tb_ctrl(id_speler,pushb,idb_spelersw,idb_spelerswd,idb_grijs,"Spelergegevens (F2);Lijst van alle personen in het bestand",1,1),
		 tb_ctrl(id_onderdeel,pushb,idb_schemaw,idb_schemap,idb_grijs,"Schema's;Schema's inrichten en vertonen",1,1),
		 tb_ctrl(id_PlanX,pushb,idb_planminw,idb_planminp,idb_grijs,"Planning;Planning maken of wijzigen",1,1),
		 tb_ctrl(id_oproepen_publiceren,pushb,idb_waarsw,idb_waarswd,idb_grijs,"Oproepen/Publiceren;Oproepen of Publiceren",1,1),
		 tb_ctrl(id_Toernooi_uitslagen_invoeren,pushb,idb_balw,idb_balp,idb_grijs,"Uitslagen invoeren (F8);Invoer uitslagen",1,1),
		 tb_ctrl(id_Speler_Betalingen_popup,pushb,idb_eurow,idb_eurowd,idb_grijs,"Betalingen;Betalingen invoeren",1,1),
		 tb_ctrl(idt_afronden,pushb,idb_uitgang,idb_uitgangd,idb_grijs,"Toernooi afronden;Toernooi is klaar",1,1),
		 separator,
		 tb_ctrl(id_Internet,pushb,idb_taakw,idb_taakrood,idb_taakrood,"Stappenplan;Gids en Stappenplan",1,1),
		 separator,
		 tb_text(idt_toolbarmsg,tb_static,310,0,4,10,0x80,"")]),
enddef
	true.
%END_TLB Project toolbar


naam("",[],"","","",0).
baan_adm(nee).
dag_max_o(2, 0).	% 0 = per dag (het eerste element)
openbaar(ja).
pouletelling(0).
%omdraaienNaam(ja).
door_plannen(ja).
betaal_adm(ja).
dag_max(1).
separatie(8).
maxseparatie(60).	% 17.8.2016
dubbelvoorenkel(nee).	% 21.8.2016
duur(8).
singles(2).
banentool(ja, 0).
vereniging1("", "", "", "", "", "", "", "", "", "").
%andereSexe(b_True).	% partner in gemengd dubbelspel moet andere sexe zijn (rolstoel)
tasversie(versie).
oproepen(ja).
%lidnrcheck(b_True).
etiketwaarden(r(13.5),r(4.0),r(99.1),r(33.9),r(3.0),r(0.0),i(7),i(2),7,10).
toernooidir("", "").
webdir("", disk, "").	% 28.8.2008
prrichting(0).
opvullen(b_True).
prvanaf(0).
paginasel(1).
waarderingI(b_False).
prkleur(b_False).
prkader(b_True).
prplan(1).
plantm(0).
emptyjob(b_True).
jobstarted(b_False).
%pouletel(b_False).
koppel(b_False).
minkolhor("").
minkolvert("").
schemafontsize(10).
comPort("COM2").
%modem_enabled(b_False, "2", "", 2400, b_False, b_True).
window_maximized(b_False).
sheetmarges(2, 2, 2, 2).
rapportfontgr(8).
form([], 0, "", "", 3, 0, [], 2, 0, "", 0, 0, [0,0,0,0,0,0,0], [0,0,0,0,0,0]).
%webrapport(7, 0).
banknummers(ja).
%aantekenen(b_False).
schemadoel(afdruk).
schemapresel([]).
dagstatusinst("11pt", 0).
webdirsave("", disk, "").
toernooidirsave("", "").
roosterinterval(15).
tshirt(b_True, "").



%BEGIN_TLB Help line, 19:54:35-17.10.2008, Code automatically updated!
/**************************************************************************
	Creation of toolbar: Help line
**************************************************************************/
/*
clauses

  tb_help_line_Create(_Parent):-
ifdef use_tbar
	toolbar_create(tb_bottom,0xC0C0C0,_Parent,
		[tb_ctrl(idd_online,pushb,idb_online,idb_offline,idb_offline,"onli/offline",1,1),
		 tb_text(idt_online,tb_static,48,0,1,10,0x0,"offline"),
		 tb_text(idt_help_line,tb_context,400,0,4,10,0x0,""),
		 tb_text(idc_laatst_gewijzigde,tb_static,250,0,1,10,0x0,"verbinding maken..")]),
enddef
	true.
%END_TLB Help line
*/




%BEGIN_DLG About dialog
/**************************************************************************
	Creation and event handling for dialog: About dialog
**************************************************************************/

predicates
% items2indexes(integer Aantal, ILIST IndexList)  Global
items2indexes1(integer, integer, ILIST, ILIST)
procedure usb2txt(janee, string)

clauses

items2indexes(Tot, Out) :-
	items2indexes1(0, Tot, [], Out).

items2indexes1(N, N, In, In) :- !.
items2indexes1(N, Tot, In, [N|Out]) :-
	N1 = N + 1,
	items2indexes1(N1, Tot, In, Out).

usb2txt(ja, "(Mac && USB)") :- !.
usb2txt(_, "(PC)").

CONSTANTS

%BEGIN About dialog, CreateParms, 15:10:37-1.1.2017, Code automatically updated!
  dlg_about_dialog_ResID = idd_dlg_about
  dlg_about_dialog_DlgType = wd_Modal
  dlg_about_dialog_Help = idh_inhoud
%END About dialog, CreateParms

PREDICATES

  dlg_about_dialog_eh : EHANDLER
  %b_Ehandler :EHANDLER

CLAUSES

  dlg_about_dialog_Create(Parent):-
	win_CreateResDialog(Parent,dlg_about_dialog_DlgType,dlg_about_dialog_ResID,dlg_about_dialog_eh,0).

%BEGIN About dialog, idc_ok _CtlInfo
  dlg_about_dialog_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-!,
	win_Destroy(_Win),
	!.
%END About dialog, idc_ok _CtlInfo
%MARK About dialog, new events

%BEGIN About dialog, e_Update
  dlg_about_dialog_eh(_Win,e_Update(_UpdateRct),0):-!,
	CtrlWin = win_GetCtlHandle(_Win, idc_reset),
	Font = font_Create(ff_Helvetica, [], 7),
	win_SetFont(CtrlWin, Font),
	!.
%END About dialog, e_Update

%BEGIN About dialog, idc_reset _CtlInfo
  dlg_about_dialog_eh(_Win,e_Control(idc_reset,_CtrlType,_CtrlWin,_CtlInfo),0):-
  1 = dlg_MessageBox( "Licentie", "Reset licentie?\nStart de TA opnieuw op en houd uw licentiecode bij de hand!", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
  syspath(ExeStartupPath,_ProgName),
  filenamepath(FName, ExeStartupPath, licconst),
  file_str(FName,"\n"),  % i.p.v. deletfile wegens rechten
  usb(ja),
  file_str(FName,"usb(ja)\n"),
  !.
%END About dialog, idc_reset _CtlInfo

%BEGIN About dialog, e_Create
  dlg_about_dialog_eh(_Win,e_Create(_CreationData),0):-!,
	TextCtl = win_GetCtlHandle(_Win, idc_dlg_about_st_nomb),
	usb(USBJN),
	usb2txt(USBJN, USB),
	format(Versie, "versie % %",versie, USB), % versie is in ... tasvp1.inc
	win_SetText(TextCtl, Versie),
	EigenaarW = win_GetCtlHandle(_Win, idct_eigenaar),
	sleuteltekst(Id1), !,
	trim_c(Id1),
	reversestr(Id1, "", Id2),
	win_SetText(EigenaarW, Id2),
	%dag(D, Str, _),
	%write("\n", D, " ", Str),
	%win_SetSubclassHandler(_Win, b_Ehandler, 0 ),
	!.
%END About dialog, e_Create


  dlg_about_dialog_eh(_,_,_):-!,fail.

%END_DLG About dialog

%END About dialog, idc_help _CtlInfo

/*b_Ehandler(_,e_KeyDown(I,S),0):-
  E = e_KeyDown(I,S),
  write(E),nl.*/



%BEGIN_DLG Print onderdelen
/**************************************************************************
	Creation and event handling for dialog: Print onderdelen
**************************************************************************/
database - afdruk
single  pregeselecteerd(ilist)

PREDICATES

nondeterm wedscat_ingedeeld1(string)
setenabled(WINDOW,DIALOG_CONTROL_ID)
getorientatie(integer, DIALOG_CONTROL_ID)
getpaginasel(integer,DIALOG_CONTROL_ID)
procedure preselectP(slist PRONDERDELEN_ITEMLIST, slist Preselect, integer Index, ilist, ilist PRONDERDELEN_SELECT)
procedure bewaar_pregeselecteerd(ilist)

CLAUSES
bewaar_pregeselecteerd(ListNew) :-
  count(ListNew, 0, AantN),
  %pregeselecteerd(List),
  %count(List, 0, AantalN),
  AantN > 2,
  assert(pregeselecteerd(ListNew)), !.
bewaar_pregeselecteerd(_ListNew).

preselectP(_, [], _, _, List) :-
  pregeselecteerd(List), !. 
preselectP([H|T], Preselect, N, In, [N|Uit]) :-
  searchchar(H,'\t',Pos),
  frontstr(Pos,H,_StartString,CatS),
  member(CatS, Preselect),
  N1 = N + 1, !,
  preselectP(T, Preselect, N1, In, Uit).
preselectP([_|T], Preselect, N, In, Uit) :-
  N1 = N + 1, !,
  preselectP(T, Preselect, N1, In, Uit).
preselectP(_, _, _, In, In).


wedscat_ingedeeld1(Catname1) :-
  wc(No, _, _, Catname, _, _, _, _, _, _, _, _, _, _, _),
	%wc(No,Catname,_,_,_,_,_),
  pos(No,Size,_,AfvPoel),
  Size2 = Size * 2,
  aantalDeelnemers(AfvPoel, No, Size2, Size3),
  afval_poel(AfvPoel, Ext),
  format(Prop, "%s %u", Ext, Size3),
  trim_c(Prop),
  format(Catname1,"%s\t%s", Prop, Catname).

setenabled(_Win,idc_laatst_gewijzigde) :-
	LboxWin = win_GetCtlHandle(_Win, idc_vanaf1),
	win_SetState(LBoxWin,[wsf_Enabled]), !.
setenabled(_,_).

getorientatie(0, idc_portret).
getorientatie(1, idc_landschap).
getorientatie(2, idc_auto).

getpaginasel(1, idc_alle_paginas).
getpaginasel(2, idc_niet_blanko).
getpaginasel(3, idc_laatst_gewijzigde).

getplansel(0, idc_geen_planning).
getplansel(1, idc_tot_en_met).

findIndex([Dag|_], VanafDagNum, N, N) :-
  dag(VanafDagNum, Dag, _), !.
findIndex([_|Rest], VanafDagNum, In0, N) :-
  In1 = In0 + 1, !,
  findIndex(Rest, VanafDagNum, In1, N).
findIndex([], _VanafDagNum, _, Aant) :-
  findall(Num, dag(Num, _, _), NL),
  count(NL, -1, Aant), !. 
  

CONSTANTS

%BEGIN Print onderdelen, CreateParms, 13:02:48-10.12.2001, Code automatically updated!
  dlg_print_onderdelen_ResID = idd_print_onderdelen
  dlg_print_onderdelen_DlgType = wd_Modal
  dlg_print_onderdelen_Help = idh_print
%END Print onderdelen, CreateParms

PREDICATES

  dlg_print_onderdelen_eh : EHANDLER
  dlg_print_onderdelen_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST,SLIST Selectie)
  dlg_print_onderdelen_update(DIALOG_VAL_LIST,SLIST Selectie)


CLAUSES

pregeselecteerd([]).

  dlg_print_onderdelen_Create(Parent,Selectie,Preselect):-
	findall(Onderdeel, wedscat_ingedeeld1(Onderdeel), PRONDERDELEN_ITEMLIST),
        preselectP(PRONDERDELEN_ITEMLIST, Preselect, 0, [], PRONDERDELEN_SELECT),
	opvullen(IDC_OPVULLEN_CHECKED),
	prrichting(Ori),
	getorientatie(Ori, ORIENTATIE),
	paginasel(PSel),
	getpaginasel(PSel, PAGINASEL),
	%fontsize(Size),
	%IDC_LETTER_GROOTTE1_SELECT = Size,
	%IDC_LETTER_GROOTTE1_ITEMLIST = ["8","10","12","14","16","18"],
	findall(Dag, dag(_, Dag, _), Dagen),
	IDC_VANAF1_ITEMLIST = Dagen,
	prvanaf(VanafDagNum),
	findIndex(Dagen, VanafDagNum, 0, IDC_VANAF1_SELECT),
	waarderingI(IDC_PUNTENWAARDERING_CHECKED),
	prkleur(IDC_KLEUR_CHECKED),
	IDC_PLAN_TM_ITEMLIST = Dagen,
	prkader(IDC_KADER_CHECKED),
	prplan(PlanSel),
	getplansel(PlanSel, PLANNING_TM),
	plantm(DagNumP),
	findIndex(Dagen, DagNumP, 0, IDC_PLAN_TM_SELECT),
	getprfont(10, OldFont),
	FontName = font_GetAttrs(OldFont, _, FSize),
	format(IDCT_FSIZE_TITLE, "Lettertype: %s %u", FontName, FSize),
	minkolhor(IDC_PRINT_OND_MINLANDSCH_VALUE),
	minkolvert(IDC_PROND_MINPORTR_VALUE),
%MARK Print onderdelen, new variables

	dialog_CreateModal(Parent,dlg_print_onderdelen_ResID,"",
  		[
%BEGIN Print onderdelen, ControlList, 13:02:48-10.12.2001, Code automatically updated!
		df(pronderdelen,listbox(PRONDERDELEN_ITEMLIST,PRONDERDELEN_SELECT),nopr),
		df(idc_vanaf1,listbutton(IDC_VANAF1_ITEMLIST,IDC_VANAF1_SELECT),nopr),
		df(idc_opvullen,checkbox(IDC_OPVULLEN_CHECKED),nopr),
		df(idc_kleur,checkbox(IDC_KLEUR_CHECKED),nopr),
		df(idc_plan_tm,listbutton(IDC_PLAN_TM_ITEMLIST,IDC_PLAN_TM_SELECT),nopr),
		df(idc_kader,checkbox(IDC_KADER_CHECKED),nopr),
		df(idc_puntenwaardering,checkbox(IDC_PUNTENWAARDERING_CHECKED),nopr),
		df(idc_print_ond_minlandsch,editstr(IDC_PRINT_OND_MINLANDSCH_VALUE,[length(1)]),nopr),
		df(idc_prond_minportr,editstr(IDC_PROND_MINPORTR_VALUE,[length(1)]),nopr),
		df(idct_fsize,statictext(IDCT_FSIZE_TITLE),nopr),
		df(PAGINASEL,radiobuttongroup([idc_alle_paginas,idc_niet_blanko,idc_laatst_gewijzigde]),nopr),
		df(ORIENTATIE,radiobuttongroup([idc_landschap,idc_portret,idc_auto]),nopr),
		df(PLANNING_TM,radiobuttongroup([idc_geen_planning,idc_tot_en_met]),nopr)
%END Print onderdelen, ControlList
		],
		dlg_print_onderdelen_eh,0,VALLIST,ANSWER),
	dlg_print_onderdelen_handle_answer(ANSWER,VALLIST,Selectie), !.

  dlg_print_onderdelen_handle_answer(idc_ok,VALLIST,Selectie):-!,
	dlg_print_onderdelen_update(VALLIST,Selectie).
  dlg_print_onderdelen_handle_answer(idc_cancel,_,[]):-!.  % Handle Esc and Cancel here
  dlg_print_onderdelen_handle_answer(_,_,_):-
	errorexit().

  dlg_print_onderdelen_update(_VALLIST,Selectie):-
%BEGIN Print onderdelen, Update controls, 13:02:48-10.12.2001, Code automatically updated!
	dialog_VLGetListBox(pronderdelen,_VALLIST,_PRONDERDELEN_ITEMLIST,_PRONDERDELEN_SELECT),
	dialog_VLGetListButton(idc_vanaf1,_VALLIST,_IDC_VANAF1_ITEMLIST,_IDC_VANAF1_SELECT),
	_IDC_PUNTENWAARDERING_CHECKED = dialog_VLGetCheck(idc_puntenwaardering,_VALLIST),
	_IDC_KLEUR_CHECKED = dialog_VLGetCheck(idc_kleur,_VALLIST),
	dialog_VLGetListButton(idc_plan_tm,_VALLIST,_IDC_PLAN_TM_ITEMLIST,_IDC_PLAN_TM_SELECT),
	_IDC_KADER_CHECKED = dialog_VLGetCheck(idc_kader,_VALLIST),
	_IDC_OPVULLEN_CHECKED = dialog_VLGetCheck(idc_opvullen,_VALLIST),
	_IDC_PRINT_OND_MINLANDSCH_VALUE = dialog_VLGetstr(idc_print_ond_minlandsch,_VALLIST),
	_IDC_PROND_MINPORTR_VALUE = dialog_VLGetstr(idc_prond_minportr,_VALLIST),
	_PAGINASEL = dialog_VLGetRadiobutton(idc_alle_paginas,_VALLIST),
	_ORIENTATIE = dialog_VLGetRadiobutton(idc_landschap,_VALLIST),
	_PLANNING_TM = dialog_VLGetRadiobutton(idc_geen_planning,_VALLIST),
%END Print onderdelen, Update controls
	Selectie = _PRONDERDELEN_ITEMLIST,
	bewaar_pregeselecteerd(_PRONDERDELEN_SELECT),
	getorientatie(Ori, _ORIENTATIE),
	assert(prrichting(Ori)),
	getpaginasel(PSel,_PAGINASEL),
	assert(paginasel(PSel)),
	finddag(DagNum,_IDC_VANAF1_ITEMLIST),
	assert(prvanaf(DagNum)),
	assert(waarderingI(_IDC_PUNTENWAARDERING_CHECKED)),
	assert(prkleur(_IDC_KLEUR_CHECKED)),
	getplansel(PlanSel,_PLANNING_TM),
	assert(prplan(PlanSel)),
	assert(prkader(_IDC_KADER_CHECKED)),
	finddag(DagNumP,_IDC_PLAN_TM_ITEMLIST), !,
	assert(plantm(DagNumP)),
	assert(opvullen(_IDC_OPVULLEN_CHECKED)),
	assert(koppel(b_true)),
	assert(minkolhor(_IDC_PRINT_OND_MINLANDSCH_VALUE)),
	assert(minkolvert(_IDC_PROND_MINPORTR_VALUE)),
	true.
	
%MARK Print onderdelen, new events

%BEGIN Print onderdelen, idc_printmarges _CtlInfo
  dlg_print_onderdelen_eh(_Win,e_Control(idc_printmarges,_CtrlType,_CtrlWin,_CtlInfo),0):-
	dlg_printmarges_Create(_Win),
	!.
%END Print onderdelen, idc_printmarges _CtlInfo

%BEGIN Print onderdelen, idc_prgeen _CtlInfo
  dlg_print_onderdelen_eh(_Win,e_Control(idc_prgeen,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	LboxWin = win_GetCtlHandle(_Win, pronderdelen),
	selectGeen(LboxWin),
	AWin = win_GetCtlHandle(_Win, idct_xx_geselecteerd),
	win_SetText(AWin, "Geen onderdelen staan geselecteerd"),
	OKWin = win_GetCtlHandle(_Win, idc_ok),
	win_SetState(OKWin,[wsf_Disabled]),	
	!.
%END Print onderdelen, idc_prgeen _CtlInfo

%BEGIN Print onderdelen, idc_fontsel _CtlInfo
  dlg_print_onderdelen_eh(_Win,e_Control(idc_fontsel,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	getprfont(10, OldFont),
	Font1=dlg_ChooseFont(OldFont),
	retractall(prfont(_)),
	assert(prfont(Font1)),
	FontName = font_GetAttrs(Font1, _, FSize),
	format(IDCT_FSIZE_TITLE, "Lettertype: %s %u", FontName, FSize),
	TextWin = win_GetCtlHandle(_Win, idct_fsize),
	win_SetText(TextWin, IDCT_FSIZE_TITLE),
	!.
%END Print onderdelen, idc_fontsel _CtlInfo


%BEGIN Print onderdelen, idc_help _CtlInfo
  dlg_print_onderdelen_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Print"),
	!.
%END Print onderdelen, idc_help _CtlInfo

%BEGIN Print onderdelen, pronderdelen selchanged
  dlg_print_onderdelen_eh(_Win,e_Control(pronderdelen,_CtrlType,_CtrlWin,selchanged),0):-
	lbox_GetSel(_CtrlWin,_ItemList,IndexList),
	AWin = win_GetCtlHandle(_Win, idct_xx_geselecteerd),
	count(IndexList, 0, Aantal),
	format(Boodsch, "%u onderdelen staan geslecteerd", Aantal),
	win_SetText(AWin, Boodsch),
	not(IndexList=[]),
	LboxWin = win_GetCtlHandle(_Win, idc_ok),
	win_SetState(LBoxWin,[wsf_Enabled, wsf_Default]),	
	!.
  dlg_print_onderdelen_eh(_Win,e_Control(pronderdelen,_CtrlType,_CtrlWin,selchanged),0):-
	LboxWin = win_GetCtlHandle(_Win, idc_ok),
	win_SetState(LBoxWin,[wsf_Disabled]),	
	!.
%END Print onderdelen, pronderdelen selchanged

%BEGIN Print onderdelen, idc_alle_paginas _CtlInfo
  dlg_print_onderdelen_eh(_Win,e_Control(idc_alle_paginas,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idc_vanaf1),
	win_SetState(LBoxWin,[wsf_Disabled]),	
	!.
%END Print onderdelen, idc_alle_paginas _CtlInfo

%BEGIN Print onderdelen, idc_niet_blanko _CtlInfo
  dlg_print_onderdelen_eh(_Win,e_Control(idc_niet_blanko,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idc_vanaf1),
	win_SetState(LBoxWin,[wsf_Disabled]),	
	!.
%END Print onderdelen, idc_niet_blanko _CtlInfo

%BEGIN Print onderdelen, idc_laatst_gewijzigde _CtlInfo
  dlg_print_onderdelen_eh(_Win,e_Control(idc_laatst_gewijzigde,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	%IsChecked = win_IsChecked(_CtrlWin),
	%IsChecked = b_True,
	LboxWin = win_GetCtlHandle(_Win, idc_vanaf1),
	win_SetState(LBoxWin,[wsf_Enabled]),	
	!.
%END Print onderdelen, idc_laatst_gewijzigde _CtlInfo

%BEGIN Print onderdelen, idc_alle_ond _CtlInfo
  dlg_print_onderdelen_eh(_Win,e_Control(idc_alle_ond,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	LboxWin = win_GetCtlHandle(_Win, pronderdelen),
	selectAlle(LboxWin),
	AWin = win_GetCtlHandle(_Win, idct_xx_geselecteerd),
	NoOfItems = lbox_CountAll(LboxWin),
	format(Boodsch, "%u onderdelen staan geslecteerd", NoOfItems),
	win_SetText(AWin, Boodsch),
	OKWin = win_GetCtlHandle(_Win, idc_ok),
	win_SetState(OKWin,[wsf_Enabled, wsf_Default]),	
	!.
%END Print onderdelen, idc_alle_ond _CtlInfo

%BEGIN Print onderdelen, e_Create
  dlg_print_onderdelen_eh(_Win,e_Create(_CreationData),0):-
	LboxWin = win_GetCtlHandle(_Win, pronderdelen),
	Width = 16,
	lbox_setTabstops(LboxWin, [Width]),
	paginasel(PSEL),
        getpaginasel(PSEL, ControlId),
        setenabled(_Win,ControlId),
	LboxWin = win_GetCtlHandle(_Win, pronderdelen),
	lbox_GetSel(LboxWin,_ItemList,IndexList),
	AWin = win_GetCtlHandle(_Win, idct_xx_geselecteerd),
	count(IndexList, 0, Aantal),
	format(Boodsch, "%u onderdelen staan geslecteerd", Aantal),
	win_SetText(AWin, Boodsch),
	IndexList = [],
	OKWin = win_GetCtlHandle(_Win, idc_ok),
	win_SetState(OKWin,[wsf_Disabled, wsf_Default]),	
	!.
%END Print onderdelen, e_Create

  dlg_print_onderdelen_eh(_,_,_):-!,fail.

%END_DLG Print onderdelen


%BEGIN_WIN OnderdeelSelectie
/**************************************************************************
        Creation and event handling for window: OnderdeelSelectie
**************************************************************************/

constants
%BEGIN OnderdeelSelectie, CreateParms, 10:43:37-11.12.2002, Code automatically updated!
  win_onderdeelselectie_WinType = w_TopLevel
  win_onderdeelselectie_Flags = [wsf_SizeBorder,wsf_TitleBar,wsf_Close,wsf_ClipSiblings,wsf_ClipChildren,wsf_TopMost]
  win_onderdeelselectie_RCT = rct(100,80,300,373)
  win_onderdeelselectie_Menu = no_menu
  win_onderdeelselectie_Title = "OnderdeelSelectie"
  win_onderdeelselectie_Help = idh_adresetiketten
%END OnderdeelSelectie, CreateParms

database - ondselect
determ oselect(window)
aleenkoppeling(integer Diepte, wedscat Van, wedscat Naar)

predicates

  win_onderdeelselectie_eh : EHANDLER
  callbackS : cTreeView::callback
nondeterm  afhankelijk(integer Diepte, catl Parents, wedscat, cTreeView::tree Out, integer Inged, integer Uitgebreid, overbl)
nondeterm wedscat_inged(wedscat, string Naam, integer Inged) -  (o,o,i)
determ wedscat_inged1(wedscat)
%procedure checkForduplicaatWC()
%determ checkvinkVerl(wedscat)
  wcNaam(wedscat Cat, integer Uitgebreid, string Naam)
  treeError(integer)
  checkParents(wedscat, catl)
  rem_kopl(wedscat, wedscat)

  
clauses

catSelect(Cat) :-
  oselect(_Win),
  CtrlWin = win_GetCtlHandle(_Win, idc_custom),
  %write("\ngaat worden: ",Cat),
  cTreeView::select(CtrlWin, Cat),
  !.
catSelect(_Cat).

catUnselect() :-
  oselect(_Win),
  CtrlWin = win_GetCtlHandle(_Win, idc_custom),
  cTreeView::unselect(CtrlWin), !.
catUnselect().

overblijvendeOpgave(Cat, Style, [bold()|Style], alle) :-
  opg(_, Cat, T, _,_,_,_,_,_,_,_),
  not(in_weds(Cat, T)), !.
overblijvendeOpgave(Cat, Style, [bold()|Style], loslopend) :-
  opg(_, Cat, T, _,_,_,0,_,_,_,_),		% niet buitengesloten
  not(in_weds(Cat, T)), !.
overblijvendeOpgave(_Cat, Style, State, _) :-
  removeBold(Style, State).

removeBold([H|T], [H|Uit]) :-
  not(H = bold()), !,
  removeBold(T, Uit).
removeBold([_|T], Uit) :-
  removeBold(T, Uit), !.
removeBold(_, []). 

knoop(_, _, _, _) :-
  retractall(vink(_,_)),
  fail. 
knoop(CatNo, tree(CatNoNr, State, BMap, BMap, NaamT, Afhankelijken), Uitgebr, TidL) :-
  member(Tid, TidL),
  wc(CatNo, _, _, _, _, _, _, _, _, _, _, _, _, Tid, _),
  not(isOndercat(CatNo)),
  checkvink(CatNo, CatNo, CatNoNr),
  wcNaam(CatNo, Uitgebr, NaamT),
  rsoort2bitmap(CatNo, BMap),
  overblijvendeOpgave(CatNo, [], State, alle),
  findall(Afhankelijk, afhankelijk(1, [CatNo], CatNo, Afhankelijk,0, Uitgebr, alle), Afhankelijken).
knoop(CatNo, tree(CatNoNr, State, BMap, BMap, NaamT, Afhankelijken), Uitgebr, TidL) :-
  member(Tid, TidL),
  wc(CatNo, _, _, _, _, _, _, _, _, _, _, _, _, Tid, _),
  %wc(CatNo, _,_,_,_,_,_),	% 5.9.2004 extra test
  not(isOndercat(CatNo)),
  not(vink(CatNo, _)),   
  checkvink(CatNo, CatNo, CatNoNr),
  wcNaam(CatNo, Uitgebr, NaamT),
  rsoort2bitmap(CatNo, BMap),
  overblijvendeOpgave(CatNo, [], State, alle),
  findall(Afhankelijk, afhankelijk(1, [CatNo], CatNo, Afhankelijk,0, Uitgebr, alle), Afhankelijken).
  
%  retractall(vink(_)),
%  fail. 
  
wedscat_inged(CatNo, Naam, 0) :-
  wc(CatNo, _, _, Naam, _, _, _, _, _, _, _, _, _, _, _).
  %wc(CatNo, Naam,_,_,_,_,_).
wedscat_inged(CatNo, Naam, 1) :-
  wc(CatNo, _, _, Naam, _, _, _, _, _, _, _, _, _, _, _),
  %wc(CatNo, Naam,_,_,_,_,_),
  wedscat_inged1(CatNo).

wedscat_inged1(CatNo) :-
  pos(CatNo, _, _, _), !.
wedscat_inged1(CatNo) :-
  voorronde(CatNo, Voorronde),
  pos(Voorronde, _, _, _), !.
  
knoopIngedeeld(_) :-
  retractall(vink(_, _)),
  fail. 
knoopIngedeeld(tree(CatNoNr, State, BMap, BMap, Naam, Afhankelijken)) :-
  wedscat_inged(CatNo, Naam,1),
  not(isOndercat(CatNo)),
  checkvink(CatNo, CatNo, CatNoNr),
  rsoort2bitmap(CatNo, BMap),
  overblijvendeOpgave(CatNo, [], State, loslopend),
  findall(Afhankelijk, afhankelijk(1, [CatNo], CatNo, Afhankelijk, 1, 0, loslopend), Afhankelijken).
knoopIngedeeld(_) :-
  retractall(aleenkoppeling(_,_,_)),
  fail. 

%checkParents(Cat, Parents) :-
%  not(member(Cat, Parents)), !.
checkParents(Cat, [_|Parents]) :-
  not(member(Cat, Parents)), !.
checkParents(Cat, [_,Parent|_]) :-
  wc(Cat, _, _, Naam, _, _, _, _, _, _, _, _, _, _, _),
  wc(Parent, _, _, PNaam, _, _, _, _, _, _, _, _, _, _, _),
  %wc(Cat, Naam,_,_,_,_,_),
  %wc(Parent, PNaam,_,_,_,_,_),
  rem_kopl(Cat, Parent),
  format(Msg, "Koppeling van %s naar %s verwijderd.\nCheck svp de indeling (Alt-D)!", PNaam, Naam),  
  dlg_MessageBox( "Koppeling", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.

%rem_kopl(Cat, Parent) :-
%  wc(Parent, Naam, WT, Kort, SexL, Inschr, Cat),
%  logf('Z', wc(Parent, Naam, WT, Kort, SexL, Inschr, 0), nee),
%  fail.
rem_kopl(Cat, Parent) :-
  kopl(Cat, Van, Parent, Naar),
  not(kopl(Cat, pl3en4(_), Parent, _)),
  logf('R', kopl(Cat, Van, Parent, Naar),nee),
  fail.
rem_kopl(Cat, Parent) :-
  kopl(Parent, pl3en4(X), Cat, Naar),
  logf('R', kopl(Parent, pl3en4(X), Cat, Naar), nee),
  fail.
rem_kopl(_Cat, _Parent).

afhankelijk(Diepte, _Parents, _Cat, _, _Inged, _Uitgebr, _Overbl) :-
  retractall(aleenkoppeling(Diepte, _, _)),
  fail.
afhankelijk(Diepte, Parents, Cat, tree(CatTNr, State, BMap, BMap, Naam, Afhankelijken), Inged, Uitgebr, Overbl) :-
  Diepte < 6,
  wedscat_inged(CatT, _, Inged),	% nondeterm
  %getbacktrack(Here),
  koplv(CatT, Cat),
  %retractall(aleenkoppeling(Diepte, CatT, Cat)),  al geprobeerd
  not(aleenkoppeling(Diepte, CatT, Cat)),
  assert(aleenkoppeling(Diepte, CatT, Cat)),
  %cutbacktrack(Here),
  checkParents(Cat, Parents),
  checkvink(CatT, Cat, CatTNr),
  rsoort2bitmap(CatT, BMap),
  wcNaam(CatT, Uitgebr, Naam),
  overblijvendeOpgave(CatT, [], State, Overbl),
  D1 = Diepte + 1,
  findall(Afhankelijk, afhankelijk(D1, [CatT|Parents], CatT, Afhankelijk, Inged, Uitgebr, Overbl), Afhankelijken).	% 31.5.2014 Inged ipv 1
afhankelijk(Diepte, _CatTT, _Cat, _, _Inged, _Uitgebr, _Overbl) :-
  Diepte >= 6,
  dlg_MessageBox( "Structuur", "Afgebroken want dieper dan 5 lagen!", mesbox_iconerror, mesbox_buttonsOK, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.

wcNaam(Cat, 0, Naam) :-
  wc(Cat, _, _, Naam, _, _, _, _, _, _, _, _, _, _, _), !.
  %wc(Cat, Naam, _, _Kort,_,_,_), !.
wcNaam(Cat, _, NaamU) :-
  wc(Cat, _, _, Naam, _, Kort, _, _, _, _, _, _, _, _, _), !,
  %wc(Cat, Naam, _, Kort,_,_,_), !,
  format(NaamU, "%s * %s", Kort, Naam).

callbackS(_Win,selected(_, CatNr)):-
  %not(refresh()),
  vink(CatEcht, CatNr),
  pos(CatEcht, _, WNo, _),
  %Cat <> 0,
  %posP(CatNr, CatEcht, WNo, _, _),	% 21.8.2004
  %write("\ncatecht: ",CatEcht, " wno ", WNo),
  select(CatEcht, WNo),
  %trap(win_SetFocus(_Win),_,true),
  !. 
callbackS(_Win,selected(_, CatNr)):-
  %not(refresh()),
  vink(CatEcht, CatNr),
  not(pos(CatEcht, _, _, _)),
  %Cat <> 0,
  %posP(CatNr, CatEcht, WNo, _, _),	% 21.8.2004
  %write("\ncatecht: ",CatEcht, " wno ", WNo),
  select(CatEcht, 0),
  %trap(win_SetFocus(_Win),_,true),
  !. 

close_onderdeelselect() :-
  oselect(Win),
  win_Destroy(Win), !.
close_onderdeelselect().

/*
checkForduplicaatWC() :-
  wc(CatT, _, _, NaamM, _, _, _, _, _, _, _, _, _, _, _),
  %wc(CatT, NaamM, _, _,_,_,_), 
  %wc(CatNo, _, _, SelectieOnd_, _, _, _, _, _, _, _, _, _, _, _),
  %findall(Naam, wc(CatT, Naam, _, _,_,_,_), CatL),
  findall(Naam, wc(CatT, _, _, NaamM, _, _, _, _, _, _, _, _, _, _, _), CatL),
  count(CatL, 0, N),
  N > 1,
  list_to_string(CatL, "\t", CatLS),
  format(Spag, "Duplicaat: %s %s", NaamM, CatLS),
  writeToJournaal(Spag),
  fail.
checkForduplicaatWC().
*/
/*
  dlg_onderdeelselectstruct_eh(_Win,e_menu(2,_),0):-
	win_Destroy(_Win), !.		% de Escape key ?????
*/

  win_onderdeelselectie_Create(_Parent):-
	not(pos(_,_,_,_)), !.
  win_onderdeelselectie_Create(_Parent):-
	oselect(Win), 
	trap(win_Setfocus(Win), _, fail), !.
  win_onderdeelselectie_Create(_Parent):-
	oselect(_), !.
  win_onderdeelselectie_Create(_Parent):-
	not(oselect(_)), 
	retractall(vink(_, _)), 
	win_Create(win_onderdeelselectie_WinType,win_onderdeelselectie_RCT,win_onderdeelselectie_Title,
		   win_onderdeelselectie_Menu,_Parent,win_onderdeelselectie_Flags,win_onderdeelselectie_eh,0).

%BEGIN OnderdeelSelectie, e_Create
  win_onderdeelselectie_eh(_Win,e_Create(_),0):-!,
%BEGIN OnderdeelSelectie, InitControls, 10:43:37-11.12.2002, Code automatically updated!
	win_CreateDynControl([customctl(wdef(wc_Custom,rct(7,2,193,286),"Custom",u_Pixels),"treeView",idc_custom,[wsf_Group,wsf_TabStop])],_Win),
%END OnderdeelSelectie, InitControls
%BEGIN OnderdeelSelectie, ToolbarCreate, 10:43:37-11.12.2002, Code automatically updated!
%END OnderdeelSelectie, ToolbarCreate
	Font = font_Create(ff_Helvetica, [], 8),
	win_SetFont(_Win, Font),
	assert(oselect(_Win)),
	vensterpositie(onderdeelselectvenster, _Win),	% als bewaard van de vorige keer
	CtrlWin = win_GetCtlHandle(_Win, idc_custom),
	cTreeView::init(CtrlWin, [tvs_hasbuttons, tvs_haslines, tvs_linesatroot, tvs_showselalways],resid(idb_teesize),callbackS),
	%checkforduplicaatWC(),
	findall(Knoopx, knoopIngedeeld(Knoopx), Knopen),
	not(Knopen = []),
	trap(cTreeView::insert_tree_list(CtrlWin, 0, first(), Knopen),Ret,treeError(Ret)),
	RCT = win_GetClientRect( _Win ),
	win_Move(CtrlWin,RCT),
	pos(Cat, _, _, _),
	cTreeView::select(CtrlWin, Cat),
	!.
%END OnderdeelSelectie, e_Create
%MARK OnderdeelSelectie, new events

%BEGIN OnderdeelSelectie, e_GetFocus
  win_onderdeelselectie_eh(_Win,e_GetFocus,0):-!,
	CtrlWin = win_GetCtlHandle(_Win, idc_custom),
	win_SetFocus(CtrlWin),
	!.
%END OnderdeelSelectie, e_GetFocus

%BEGIN OnderdeelSelectie, e_Destroy
  win_onderdeelselectie_eh(_Win,e_Destroy,0):-!,
	retractall(oselect(_)),
	updateVensterpositie(onderdeelselectvenster, _Win),
	!.
%END OnderdeelSelectie, e_Destroy

%BEGIN OnderdeelSelectie, e_Size
  win_onderdeelselectie_eh(_Win,e_Size(Width,Height),0):-
ifdef use_tbar
	toolbar_Resize(_Win),
enddef
	CtrlWin = win_GetCtlHandle(_Win, idc_custom),
	win_Move(CtrlWin,rct(0,0,Width, Height)),
	!.
%END OnderdeelSelectie, e_Size

%BEGIN OnderdeelSelectie, e_Menu, Parent window 
  win_onderdeelselectie_eh(Win,e_Menu(ID,CAS),0):-!,
	PARENT = win_GetParent(Win),
	win_SendEvent(PARENT,e_Menu(ID,CAS)),
	!.
%END OnderdeelSelectie, e_Menu, Parent window

%END_WIN OnderdeelSelectie

treeError(Ret) :-
  format(Msg, "Fout van treeview nr: %d.", Ret),
  writeToJournaal(Msg).
  
%BEGIN_TLB Project bottom, 15:06:34-10.12.2014, Code automatically updated!
/**************************************************************************
	Creation of toolbar: Project bottom
**************************************************************************/

clauses

  tb_project_bottom_Create(_Parent):-
ifdef use_tbar
	toolbar_create(tb_bottom,0xC0C0C0,_Parent,
		[ % tb_ctrl(idd_online,pushb,idb_online,idb_offline,idb_offline,"online/offline",1,1),
		 tb_textExt(idt_online,tb_static,65,0,1,10,0x0,color_White,"? ? ?"),
		 tb_text(idt_help_line,tb_context,400,0,4,10,0x0,""),
		 tb_text(idc_laatst_gewijzigde,tb_static,350,0,1,10,0x0,"verbinding maken..")]),
enddef
	true.
%END_TLB Project bottom

