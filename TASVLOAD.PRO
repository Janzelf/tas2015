/*****************************************************************************

		Copyright (c) 1989 - 1998 De Lint Associates

 Project:  TASVPI
 FileName: TASVLOAD.PRO
 Purpose: Alles wat met files te maken heeft
 Written by: JdL
 Comments: checked 5/12
******************************************************************************/


include "tasvp1.inc"
include "hlptopic.con"
include "tasvp1.con"
include "iodecl.con"

%constants
%fa_subdir = 0x10

domains
  keurdom = goedgekeurd ; nietgekeurd ; afgekeurd
 puin = dos_pad(string Computername, string Pad);formgezien(boolean, integer);
        wc(wedscat,string,wedstyp,string,sexlist,real,wedscat);
        wcrit(wedscat, string Speelsterkte, integer LeeftV, integer LeeftTM, integer Reserve);
        toernooionderdeel_t(wedscat, string Naam, string Speelsterkte, wedstyp ED, wedsex HDG, string Leeftijdvanaf, 
           string LeeftijdTotEnMet, keurdom Status, string MeldingOnderdeel);
       naam(string Naam, string Nr, string Datum, string WebTicket, string Distr, integer OokReserve);
       vereniging(string Nr, string WL, string WLTelef, string Club, string Plaats, string EMail, string BGNaam, string BGTel, string BGEmail, string Website);
       opg(integer Tijdstip,wedscat,tgnst,integer Plaatsing,integer Ronde,integer Uitsluiten,integer Indelingsnummer, string LLWC);  
       team_t_algemeen(string TeamID, wedscat, tgnst, keurdom Status, string MeldingTeam);
       inschrijfcheck(integer)

database - local
determ logchanged
determ backupPos(long)
determ dataverdacht(integer)
determ remoteCompressTookplace()
determ nieuweversie
determ eerstefout(integer)

database - localsingle
single logged(integer)  % voor test
single updateNodigFlag(BOOLEAN)
%single saveNodigFlag(BOOLEAN)
single lok(lokatie)
single escaped(boolean) 
%single ordeWC(integer)

predicates

cons_err(char, long)
cons_errP(char, puin)
procedure del_term(dbasedom)
%del_logX()
int_bits(short, short, sl, sl)  
list_bits(sl, sl, sl)
list_bits(sl, sl, short, short)
bits_chars(short, sl, short, string, string)
determ  crc_check(string,ushort,ushort) - (i,i,i)
cleanfacts()
procedure check_log(long)
procedure vanKompaan(char, dbasedom)
procedure time_stampPLus(tijd, tijd) - (i,o)
%back_up2(BOOLEAN Compat, WINDOW, string)
%procedure update_pad(string)
procedure tracelog(char, dbasedom, janee IsOpen)
%behandelPadError(integer Ret, string Pad)
%space2comment(ulong, string, BOOLEAN)
  checkTerm(char Action, dbasedom)
procedure logf1(char,dbasedom,janee)
exd1(string)
exd2(string,string)
withoutBslash(string, string)
procedure index(ulong, ulist, integer, integer)
procedure auto_error()
fileopenerr(string, string, integer)
myOpenAppend1(integer Retry, file, string Naam)
myOpenModify1(integer Retry, file, string Naam)
behandelAutoerror(integer Ret, string Pad)
  sex_titel(sexe, string)
%checkOfDemo(string)
openlogvoorweb(integer AantalKeren)
determ checkLogIsOpen(openhoe)
determ charIs(char)	% 14.9.2008
procedure   wis_34plaats(wedscat)
determ checkUpload(integer)
determ getOpeationeel1(string Line)
procedure isCompressNodig(integer Size)
vorigediskfile(window) - (i)
setIniPos(long Pos)
prob(integer)
determ herhaal(integer, integer)
determ webdeltasFile(string)
procedure vervangDat(window, string FullFileName) - (i,i)
determ zetTerugenFail(long, long) - (i,i)
determ handle_BUPadError()
determ backup1(string Pad)
determ backup2(string, string, string, string)
procedure merkrefresh()
procedure wcP(wedscat,dbasedom) - (i,wc(o,o,o,o,o,o,o,o,o,o,o,o,o,o,o))
procedure addVerliezer(char, wedscat Verl, wedscat Hoofd)
%procedure leeftijd2Getal(string, integer)
%procedure domconvert(keurdom, wcorigine)
procedure sexlist2wedsex(wedsex,sexlist, wedsex) - (i,i,o)
%determ backupdirect()
procedure getnst()
procedure getusb(slist)
procedure defaultcode(string, string)

clauses

logged(0).
updateNodigFlag(b_False).
%saveNodigFlag(b_False).
lok(disk).
escaped(b_False).
sex_titel(m, "Hr").
sex_titel(v, "Mw").
%ordeWC(0).

restartlog(TB) :-		% global
  %retractall(_, local),
  retractall(logpos(_)),
  closefile(nawf),
  openread(nawf, TB),
  filepos(nawf, 0, 2),
  filepos(nawf, N, 0),
  retractall(logposini(_)),
  assert(logposini(N)),
  assert(logpos(N)),
  %write("\n ###", N),
  closefile(nawf),
  assert(updateNodigFlag(b_False)), !.

dir_error(7021, _) :-
 dlg_MessageBox( "Back-up", "Doe diskette in station.", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), !.
dir_error(Ret, b_True) :-
  exit(Ret).

vorigediskfile(TaskWin) :-
  %TaskWin = vpi_GetTaskWin(),
  dlg_backupselect_Create(TaskWin).

vorige(Window) :- 
  vorigediskfile(Window).

time_stampPlus(TT, TT) :-
  TT > 0, !.
time_stampPlus(_, TS) :-
  findall(T, w2(_,_,T,_,_,_,_,_), Tijden),
  sorttijd_c(Tijden),
  Tijden = [TS|_], !.
time_stampPlus(_, 0).

/*
update_pad(Y) :-
  api_getcomputername(PC),
  upper_lower(PCU, PC),
  not(dos_pad(PCU, Y)), !,
  logf('Z', dos_pad(PCU, Y), nee).
update_pad(_).

behandelPadError(7021,_Path1) :- !,
  format(Tekst, "Doe een diskette in het station!"),
  dlg_MessageBox( "Back-up", Tekst, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ).
behandelPadError(Ret,Path1) :-
  format(Tekst, "Fout in opgeven pad % (%)\nGeen backup!", Path1, Ret),
  dlg_MessageBox( "Back-up", Tekst, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ).
  %dlg_Note("Back-up", Tekst).*/

behandelAutoerror(Ret, Pad) :-
  not(eerstefout(_)),
  assert(eerstefout(1)),
  format(Tekst, "Back-up\nFout in opgeven pad % (%)\nGeen automatische backup!\nZie ook Toernooi, Extra instellingen.\nNB Een USB stick kan spontaan van drive letter veranderen!", Pad, Ret),
  writeToJournaal(Tekst, ja), !.
behandelAutoerror(Ret, Pad) :-
  eerstefout(N),
  N mod 4 = 0,			% niet teveel messages
  time(Hours,Minutes,_Seconds,_Hundredths),
  helestr_int(0, MinStr, Minutes),
  format(Msg,"%:% Fout in opgeven pad % (%)\nGeen backup!", Hours, MinStr, Pad, Ret),
  writeToJournaal(Msg),
  fail.
behandelAutoerror(_Ret, _Pad) :-
  retract(eerstefout(N)),
  N1 = N + 1,
  assert(eerstefout(N1)).
/*
space2comment(Space, "  Dat is te weinig!", b_False) :-
  Space < 200000, !,
  beep,beep.
space2comment(_, "", b_True).
*/
withoutBslash(In, Out) :-
  str_len(In, Len),
  Len > 3,
  L1 = Len - 1,
  frontstr(L1,In,Out,RestString),
  RestString = "\\", !.
withoutBslash(In, In).
  
existdir(Wild, "A:\\"):-
  str_len(Wild, Len),
  Len <= 3,
  frontchar(Wild,'A',_), !.
existdir(Wild, "B:\\"):-
  str_len(Wild, Len),
  Len <= 3,
  frontchar(Wild,'B',_), !.
existdir(Wild, Wild):-
  withoutBslash(Wild, Wild1),
  vpi_ProcessEvents(),
  trap(diropen(Wild1,fa_subdir,Block),_,fail),
  exd1(Block),
  dirclose(Block), !.
existdir(Wild, "A:\\"):-
  str_len(Wild, Len),
  Len > 3, !,
  format(Msg, "Directory %s bestaat niet.", Wild),
  writeToJournaal(Msg).
existdir(Wild, Wild).

exd1(Block):-
  dirmatch(Block,Name,_,_,_,_,_,_,_,_),
  exd2(Block,Name).

exd2(_,Name):- not(frontchar(Name,'.',_)), !.
exd2(Block,_):- exd1(Block).

index(Dag, [Dag|_], I, I) :- !.
index(Dag, [_|Dagen], In, Uit) :- !,
  In1 = In + 1,
  index(Dag, Dagen, In1, Uit).
index(_, _, I, I).

backup("email", Silent) :-
  %naam(ToernooiNaam, _, _,_,_,_),
  %cleantoernooinaam(Toernooinaam, ToernooinaamC),
  get_tempdir(_, TmpFile),
  trap(save(TmpFile), _, dlg_Error("(auto-)Back-up mislukt!?\nProbeer nogmaals!")),
  emailBackup("email", TmpFile,Silent), !.
backup("", _) :-
  trap(backup1(""), _, handle_BUPadError()), !. 
backup(_, _).

backupdirect() :- !,
  trap(backup1("direct"), _, handle_BUPadError()). 

handle_BUPadError() :-
  1 = dlg_MessageBox( "Back-uppad", "Pad is niet geldig!\nMisschien moet je eerst een USB-stick aanbrengen.\nErgens anders naar toe back-uppen?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
  getfolder(_,_, _, BUP),
  backup1(BUP).
  
backup1(In) :-
  naam(ToernooiNaam, _, _,_,_,_),
  cleantoernooinaam(Toernooinaam, ToernooinaamC),
  date(_Year,Month,Day),
  helestr_int(0,MonthS,Month), helestr_int(0,DayS, Day),
  getfolder(_Folder,_, _, Pad),
  getbacktrack(Here),
  numb(Num),
    helestr_int(0,NumS, Num),
    format(BNaam, "backup(%s-%s %s) %s.tas", MonthS, DayS,NumS, ToernooiNaamC),
    filenamepath(TBU, Pad, BNaam),
    %write("\nexistfile ",TBU,"? Pad:", Pad), 
  not(existfile(TBU)), 
  cutbacktrack(Here),
  backup2(In, BNaam, Pad, TBU), !.
  
backup2("", BNaam, Pad, _TBU) :-
  Filename = dlgFileNameZeker(BNaam,["toernooibackup","backup(*.tas"],"",[dlgfn_Save],Pad,_OutListFiles),
  filenamepath(Filename, Path, _),	% gebruik alleen het pad
  filenamepath(Uiteindelijk, Path, BNaam), 
  trap(save(Uiteindelijk), _, dlg_Error("Back-up mislukt!?\nProbeer nogmaals!")), !.
  %update_pad(Path), !.
backup2("direct", _BNaam, _Pad, TBU) :-
  trap(save(TBU), _, dlg_Error("Back-up mislukt!?\nProbeer nogmaals!")), !.

dlgFileNameZeker(InitSelection,FileTypeFilters,Title,Flags,StartPad,OutFiles, Filename) :- 
  assert(escaped(b_True)),
  trap(Filename = dlg_GetFileName(InitSelection,FileTypeFilters,Title,Flags,StartPad,OutFiles),_,assert(escaped(b_False))), !.
dlgFileNameZeker(InitSelection,FileTypeFilters,Title,Flags,_,OutFiles, Filename) :- 
  escaped(b_False),
  getfolder(MijnDocumenten, _, _, _),
  1 = dlg_MessageBox( "", "Pad is niet geldig!\nMisschien moet je eerst een USB-stick aanbrengen.\nDoorgaan?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
  Filename = dlg_GetFileName(InitSelection,FileTypeFilters,Title,Flags,MijnDocumenten,OutFiles).



back_up_auto() :-
  not(backupPos(_)),
  logPos(P),
  assert(backupPos(P)), !.
back_up_auto() :-
  not(logPos(_)),
  retractall(backupPos(_)), !.
back_up_auto() :-
  backupPos(P0),
  P0 > 1000,
  logPos(P1),
  P1 < 1000,
  retractall(backupPos(_)), !.
back_up_auto() :-
  api_getComputername(CN),
  upper_lower(PC, CN),
  autobak(PC, OldPath1),
  logPos(Pos),
  backupPos(Pos1),
  Pos - Pos1 > 4000,	% fuzz factor
  retractall(backupPos(_)),
  assert(backupPos(Pos)),
  trap(existdir(OldPath1, NewPath),RetD,behandelAutoerror(RetD,OldPath1)),		% 15.6.2000 extra check voor A en B
  _Win = vpi_GetTaskWin(),
  met_bslash(NewPath, Path1),
  getbacktrack(Here),
  time_stamp1(TT, _, _),
  time_stampPlus(TT, TTPlus),
  bitright(TTPlus, 7, Dag),
  findall(D, dag(D, _, _), Dagen),
  index(Dag, Dagen, 0, DagNo), 
  format(TstNaam, "%TASB%u-D%", Path1, DagNo, "9x"), 
  trap(not(existfile(TstNaam)),Ret,behandelAutoerror(Ret,Path1)), 			% kijk nogmaals of de directory bestaat
  naam(ToernooiNaam, _, _,_,_,_),
  cleantoernooinaam(Toernooinaam, ToernooinaamC),
  numb(Num),
  format(DatNaam, "%sbackup(%u %u)%s.tas", Path1, DagNo, Num, ToernooinaamC), 
  not(trap(existfile(DatNaam), _, cutbacktrack(Here))), 
  cutbacktrack(Here),
  trap(save(DatNaam),_,auto_error()),  
  %Nr1 = Nr + 1,
  %logf('Z', autobak(PC, OldPath1, Nr1), nee),
  beep(), !.
back_up_auto().

auto_error() :-
  dlg_MessageBox( "AutoBackup", "Check het pad voor auto-back-ups!\nZie Toernooi, extra instellingen.\nAuto-backup kan op dit moment niet gemaakt worden!", mesbox_iconerror, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), !.

met_bslash("", "") :- !.        % root directory
met_bslash(Pad, Pad) :-
  str_len(Pad, Lengte),
  L1 = Lengte - 1,
  frontstr(L1, Pad, _, F),	% al een \ ?
  F = "\\", !. 			% doe niets
met_bslash(Pad, Out) :-
  concat(Pad, "\\", Out).	% plak er een achter

del_logX :-
  resetBelangrijkeEventCounter(),
  cleanfacts(),			% maak geheugen schoon
  logclose(),
  retractall(_, local),		% ga opnieuw bewaken
  retractall(logpos(_)),
  retractall(logposIni(_)),
  assert(updateNodigFlag(b_False)),
  toernooidir(Dir, _),
  filenamepath(LogFile, Dir, logconst),  % bestaat niet meer!!
  trap(existfile(Logfile), _, fail),
  deletefile(Logfile), 
 !.
del_logX.

cleanfacts() :-
  retractall(sp2(_,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)),
  retractall(wd(_,_,_,_,_,_,_)),
  retractall(w2(_,_,_,_,_,_,_,_)),
  retractall(w3(_,_,_,_,_,_,_)),
  retractall(opg(_,_,_,_,_,_,_,_,_,_,_)),
  retractall(bn(_,_)),
  retractall(pos(_,_,_,_)),  
  retractall(wc(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _)),
  retractall(dag(_,_,_)),
  retractall(club(_,_,_)),
  retractall(baannaam(_, _, _, _, _)),
  retractall(baantoew(_, _, _)),
  retractall(spelerAanwezig(_, _, _, _)),
  assert(oproepen(nee)),
  assert(separatie(8)),
  assert(maxseparatie(60)),
  assert(dubbelvoorenkel(ja)), 	% 21.8.2016
  assert(duur(8)),
  assert(singles(2)),
  assert(banentool(nee, 0)),
  assert(vereniging1("", "","","","","","","","","")),
  assert(banknummers(ja)),
  %assert(andereSexe(b_True)),
  assert(roosterinterval(15)),
  assert(dag_max(1)),
  assert(betaal_adm(nee)),
  assert(door_plannen(nee)),
  assert(openbaar(nee)),
  assert(naam("naamloos",[],"","","",0)),	% single!
  assert(form([], 1053249120, "", "",0,b_False,[731429,731430],2, b_True, "19:00", 1, b_False, [],[])),  
  retractall(inschrijfcheck(_,_,_,_)),
  retractall(ws(_,_,_,_)),
  retractall(spsrt(_,_,_,_,_,_,_,_)),
  retractall(bl(_,_)),
  retractall(blLft(_, _, _)),
  assert(baan_adm(nee)),
  assert(dag_max_o(2,0)),
  retractall(kopl(_,_,_,_)),
  %retractall(formgezien(_, _)),
  retractall(kladblok(_, _)),
  retractall(klad(_, _, _)),
  retractall(webdatpositie(_, _, _)),
  assert(pouletelling(0)),
  %retractall(team_t_algemeen(_, _, _, _, _)),
  resetBelangrijkeEventCounter(),
  retractall(eerstefout(_)),
  retractall(uitslagenable(_)),
  retractall(algemeelX(_,_,_,_,_)),
  retractall(autobak(_, _)),
  retractall(incassoeigennr(_)),
  retractall(emailadreszelf(_,_,_)),
  retractall(autoweb(_,_,_,_,_,_,_,_,_,_,_)),
  %retractall(dos_pad(_,_)),
  retractall(weblayout(_, _, _, _, _, _, _)),
  retractall(functionaris(_, _, _, _, _, _, _)),  
  assert(schemapresel([])),
  assert(tshirt(b_False, "Persoon meldt zich voor de eerste keer.")),
  retractall(toernooiK(_,_)),
  retractall(dagstatusqueued(_,_)),
  retractall(dagstatusExport(_,_)).

vanKompaan(Action, Fact) :-	% report igv test
  comline(LineBuffer),
  upper_lower(LineBuffer, LBL),
  searchstring(LBL, "/x", _),
  logged(N),
  N1 = N + 1,
  assert(logged(N1)),
  term_str(dbasedom,Fact,String),
  format(Msg,"% nieuw: %c %s.",N1, Action, String),
  writedevice(X),
  writedevice(screen),
  write("\n#### vankompaan test ", Msg),
  writedevice(X), !.
  %writeToJournaal(Msg), !.
vanKompaan(_Action, _Fact).

checkTerm(_, sp2(_,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)) :- assert(updateNodigFlag(b_True)), !.	% 18.2.2001
checkTerm(_, wd(_,_,_,_,_,_,_))     :- assert(updateNodigFlag(b_True)), !.
checkTerm(_, w2(_,_,_,_,_,_,_,_)) :- assert(updateNodigFlag(b_True)), !.
checkTerm(_, w3(_,_,_,_,_,_,_))         :- assert(updateNodigFlag(b_True)), !.
checkTerm(_, ws(_,_,_,_))         :- assert(updateNodigFlag(b_True)), !.
checkTerm(_, baantoew(_, _, _))  :- assert(updateNodigFlag(b_True)), !.
checkTerm(_, speleraanwezig(_, _, _, _))  :- checkcompleet(), assert(updateNodigFlag(b_True)), !.
checkTerm(_, klad(_, _, _)) :- kladblokrefresh(), !.
checkTerm(_, _).

doUpdateVensters() :-
  updateNodigFlag(b_True),
  assert(updateNodigFlag(b_False)),
  update_waarsch(),
  update_display(),
  upd_formulier(),
  update_situatie(), 
  update_uitslagen("doUpdateVensters"),
  update_planinvoer(),
  upd_formulier(),
  spelerSelRefresh(), !.
doUpdateVensters.

fileopenerr(Titel, Mode, 7005) :-
  format(Msg, "Openen in %s mode mislukt\nCode %u",  Mode, 7005), 
  dlg_MessageBox( Titel, Msg, mesbox_iconerror, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  writetojournaal(Msg),
  exit(7005), !.
fileopenerr(Titel, _, X) :-
  %comline(LineBuffer),
  %upper_lower(LineBuffer, LBL),
  %searchstring(LBL, "/x", _),
  write("\n#### Openen lukt niet: ", Titel),
  exit(X), !.

/*myOpenRead(_, _, _) :- % test
  comline(LineBuffer),
  upper_lower(LineBuffer, LBL),
  searchstring(LBL, "/x", _),	% 10.2.2011 voor de zekerheid
  exit(7123), !.*/   
myOpenRead(20, File, Naam) :- !,	% probeer 10 keer te openen
  trap(openread(File, Naam),Code,fileopenerr(Naam, "lezen", Code)).
myOpenRead(20, _File, Naam) :- !,
%  assert(kompaan()),
  format(Titel, "Bestand %s",Naam),
  dlg_MessageBox( Titel, "Openen (read) mislukt", mesbox_iconerror, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  writetojournaal("Openen (read) mislukt"),
  fail.
myOpenRead(_Retry, File, Naam) :-
  %maakRW(Naam),
  trap(openread(File, Naam),Code,fileopenerr(Naam, "lezen", Code)), 
  !.
myOpenRead(Retry, File, Naam) :-
  Retr = Retry + 1,
  sleep(30),
  myOpenRead(Retr, File, Naam).

myOpenAppend(File, Naam) :-
  trap(myOpenAppend1(0, File, Naam),_,true), !.

myOpenAppend1(20, File, Naam) :-!,		% probeer 10 keer te openen
  trap(openappend(File, Naam),_,true).
myOpenAppend1(20, _File, Naam) :- !,
  format(Titel, "Bestand %s",Naam),
  _Ans = dlg_MessageBox( Titel, "Openen (append) mislukt", mesbox_iconerror, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.
myOpenAppend1(_Retry, File, Naam) :-
  trap(openappend(File, Naam),Code,fileopenerr(Naam, "append", Code)), !.
myOpenAppend1(Retry, File, Naam) :-
  Retr = Retry + 1,
  sleep(30),
  myOpenAppend1(Retr, File, Naam).

myOpenModify(File, Naam) :-
  alleenlezen1(),
  trap(openread(File, Naam),Code,fileopenerr(Naam, "lezen", Code)), !.
myOpenModify(File, Naam) :-
  trap(myOpenModify1(0, File, Naam),_,true), !. 
myOpenModify(_File, Naam) :-
  format(Msg, "problemen met het openen van %s", Naam),  
  dlg_MessageBox( "", Msg, mesbox_iconquestion, mesbox_buttonsokcancel, mesbox_defaultfirst, mesbox_suspendapplication ),
  writetojournaal(Msg),
  fail.

%myOpenModify(_File, _Naam) :-
%  not(kompaan()),
%  assert(kompaan()),
%  fail.

myOpenModify1(_Retry, File, Naam) :-
  trap(openmodify(File, Naam),Code,fileopenerr(Naam, "modify", Code)), !.
myOpenModify1(Retry, File, Naam) :-
  Retry < 20,
  Retr = Retry + 1,
  sleep(30),
  myOpenModify1(Retr, File, Naam).

showFirst :-		% wat het eerst gedisplayed wordt!
  pos(Cat, _, WNo, _),
  retractall(wcdisp(Cat, _)),
  select(Cat, WNo), !. 
  %Msg_win = msg_GetWin(), 
  %trap(win_BringToTop(Msg_Win),_,true), !.	% beter
showFirst().

getOpeationeel1(Line) :-
  searchstring(Line,"status",_FoundPos),
  trap(term_str(operationeelvast, Term, Line),_,fail),
  Term = status(SL),
  retractall(status(_)),
  assert(status(SL)), !.  	% 15.12.2008 in operationeel
getOpeationeel1(Line) :-
  searchstring(Line,"usb",_FoundPos),
  term_str(operationeelvast, Term, Line),
  Term = usb(JaNee),
  retractall(usb(_)),
  assert(usb(JaNee)).	% 27.1.2009

%getlicentie

getnst() :-
  syspath(ExeStartupPath,_ProgName),
  filenamepath(FName, ExeStartupPath, licconst),
  not(existfile(FName)), !.
getnst() :-
  syspath(ExeStartupPath,_ProgName),
  filenamepath(FName, ExeStartupPath, licconst),
  existfile(FName),
  openread(work, FName),
  readdevice(work),
  repeat_f(work),
  readln(Line),
  trap(getOpeationeel1(Line),_,fail),
  fail.
getnst() :-
  closefile(work).

getusb(["usb","ja"]) :-
  usb(ja), !.
getusb([]).

defaultcode("DEMO", "94267C") :- !.
defaultcode("demo", "94267C") :- !.
defaultcode("123456", "94267C") :- !.
defaultcode(Code, Code). 

getOpeationeel(_) :-
  closefile(work),
  retractall(usb(_)),
  assert(usb(nee)),
  retractall(status(_)),
  getnst(),
  not(status(_)),
  assert(nieuweversie),
  getusb(USB),
  syspath(ExeStartupPath,_ProgName),
  filenamepath(FName, ExeStartupPath, licconst),
  LC0 = dlg_GetStr("Installatie ToernooiAssistent","Vul svp uw licentiecode in\nen klik op OK.",""),
  upper_lower(LC, Lc0),
  defaultcode(LC, LCx),
  api_getComputername(CN),
  URL = "http://tas.toernooiklapper.nl/tools/maaklicentie.php",
  NAV = ["toegangscode",LCx,"computername",CN | USB],
  mydeleteFile(FName),
  _Error = downloadData(URL, FName, NAV, _, 0),
  fail.  
getOpeationeel(_) :-
  getnst(),
  comline(LineBuffer),
  upper_lower(LineBuffer, LBL),
  searchstring(LBL, "/usb", _),
  retractall(usb(_)),
  assert(usb(ja)),	% 27.1.2009
  fail.
getopeationeel(_) :-
  retract(nieuweversie),
  status(X),
  ontsleutel(X, String),
  concat("Succes! De licentie is geïnstalleerd op naam van:\n", String,Suc),
  _A = dlg_MessageBox( "Licentiebestand", Suc, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  !.
getOpeationeel(Win) :-
  not(status(_)), !,
  syspath(ExeStartupPath,_ProgName),
  filenamepath(FName, ExeStartupPath, licconst),
  file_str(FName,"\n"),
  %deletefile(FName),
  dlg_MessageBox( "ToernooiAssistent", "Installatie mislukt,\nvoer de juiste licentiecode in of\nContact: info@toernooiklapper.nl.\nIs er verbinding met Internet?", mesbox_iconerror, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  win_Destroy(Win).
getOpeationeel(_).


repeat_f(_).
repeat_f(File) :- 
  not(eof(File)), 
  repeat_f(File).

charIs('U') :- !.	% 13.2.2011 toegevoegd
charIs('Z') :- !.	% 14.9.2008 voor de zekerheid
charIs('A') :- !.
charIs('R') :- !.
charIs('T').

isCompressNodig(Size) :- 
  %writedevice(XY),
  %writedevice(screen),
  %write("\ntasvload webdatpos"),
  %writedevice(XY),
  Size > 0, !,
  retractall(compressRequired),
  assert(compressRequired).
isCompressNodig(_).

setIniPos(Pos) :-
  not(logposini(_)),
  assert(logPosIni(Pos)), !.
setIniPos(_).

cons_err(_, Pos) :-	
  readtermerror(Line, _),
  %dlg_note("2",Line),
  frontchar(Line, Char, String),
  trim_c(String),
  charIs(Char),
  setIniPos(Pos),
  trap(term_str(dbasedom,Term,String),_,fail),
  action_term(Char, Term), !.    % hier niet opnieuw log maken
cons_err(Action, _) :-
  readtermerror(Line, _),
  trap(term_str(puin,Term,Line),_,fail),
  cons_errP(Action, Term), !.
cons_err(_, _) :- 
  readtermerror(Line, Pos),
  fronttoken(Line, _, _),
  Pos > 1,
  %dlg_note("3",Line), !,
  not(dataverdacht(_)), !,
  format(Msg, "%s ? %s ?",versie, Line),
  writeToJournaal(Msg, ja),
  assert(dataverdacht(1)).
cons_err(_, _) :- 	% 11.10.2014
  readtermerror(Line, Pos),
  fronttoken(Line, _, _),
  Pos > 1,
  dataverdacht(1),
  format(Msg, "%s ? %s ?",versie, Line),
  writeToJournaal(Msg, nee),
  retractall(dataverdacht(_)),
  assert(dataverdacht(2)).

wcP(WC, wc(WC, OnderdeeelID, Orde, Naam, EDG, Kort, SexL, SpStrk, LftV, LftTM, HDG, Schemasoort, Status, KNLTB, Geld)) :-  
  wc(WC, OnderdeeelID, Orde, Naam, EDG, Kort, SexL, SpStrk, LftV, LftTM, HDG, Schemasoort, Status, KNLTB, Geld), !.
wcP(WC, wc(WC, "", 0, "Naam", e, "Kort", [], "", leeftijd("",0), leeftijd("",99), gemengd, "", vanknltb, "KNLTB", 0.0)).

sexlist2wedsex(hdnvt, _, hdnvt) :- !.
sexlist2wedsex(_, SrtL, gemengd) :-
  member(H, SrtL),
  spsrt(H,_,m,_,_,_,_,_), 
  member(H1, SrtL),
  spsrt(H1,_,v,_,_,_,_,_), !.
sexlist2wedsex(_, SrtL, heren) :-
  member(H, SrtL),
  spsrt(H,_,m,_,_,_,_,_), !.
sexlist2wedsex(_, _, dames).
  
addVerliezer(_Action,0, _) :- !.
addVerliezer(Action, WC1, WC2) :-
  action_term(Action, kopl(WC1, verliezer, WC2, algemeen)), !.

/*leeftijd2getal(Str, Int) :-
  str_int(Str, Int), !.
leeftijd2getal(_, 0).*/

getal2Leeftijd(0, "") :- !.
getal2Leeftijd(Int, Str) :-
  str_int(Str, Int), !.

cons_errP(Action, wc(WC,Naam,EDG,Kort,SexL,Geld,Verl)) :-
  wcP(WC, wc(_, OnderdeeelID, N, _, _EDG, _Kort, _SexL, SpStrk, LftV, LftTM, HDG, Schemasoort, _Status, _KNLTB, _Geld)),
  addVerliezer(Action, Verl, WC),
  % sexlist2wedsex(SexL, HDG),  kan pas als spsrt ook geladen is
  retractall(compressrequired),
  assert(compressrequired),
  action_term(Action, wc(WC, OnderdeeelID, N, Naam, EDG, Kort, SexL, SpStrk, LftV, LftTM, HDG, Schemasoort, reserve([]), "",Geld)),!.
cons_errP(Action, wcrit(WC, SpSt, LeeftVInt, LeeftTM, _Reserve)) :-
  getal2Leeftijd(LeeftVInt,LftV),   %leeftijd2Getal(LftV, LeeftVInt),
  getal2Leeftijd(LeeftTM, LftTm),   %leeftijd2Getal(LftTM, LeeftTM),
  wc(WC, OnderdeeelID,Orde,  Naam, EDG, Kort, SexL, _SpSt, _LftV, _LftTM, HDG, Schemasoort, Reserve, KNLTB, Geld),
  retractall(compressrequired),
  assert(compressrequired),
  action_term(Action, wc(WC, OnderdeeelID, Orde, Naam, EDG, Kort, SexL, SpSt, leeftijd(LftV,LeeftVInt), leeftijd(LftTM,LeeftTM), HDG, Schemasoort, Reserve, KNLTB, Geld)),!.
cons_errP(Action, naam(Naam, Nr, Datum, WebTicket, Distr, OokReserve)) :-
  retractall(compressrequired),
  assert(compressrequired),
  action_term(Action, naam(Naam, [Nr], Datum, WebTicket, Distr, OokReserve)), !.  % lijst van toernooinummers....
cons_errP(Action, vereniging(Nr, WL, WLTelef, Club, Plaats, EMail, BGNaam, BGTel, BGEmail, Website)) :-
  naam(_Naam, [Tid], _Datum, _WebTicket, _Distr, _OokReserve),
  retractall(compressrequired),
  assert(compressrequired),
  action_term(Action, vereniging1(Nr, Club, WL, "", "", Plaats, "", WLTelef, Email, Website)), 
  %action_term(Action, functionaris(wedstrijdleider, Tid, WL, Email, WLTelef, "", "")),
  action_term(Action, functionaris(bondsgedelegeerde, Tid, BGNaam, BGEmail, BGTel, "", "")), !.
cons_errP(Action, opg(Tijdstip,WC,Tgnst,Plaatsing,Ronde,Uitsluiten,Indelingsnummer, LLWC)) :-
  retractall(compressrequired),
  assert(compressrequired),
  action_term(Action, opg(Tijdstip,WC,Tgnst,Plaatsing,Ronde,Uitsluiten,Indelingsnummer, LLWC, "", na, "")), !.
cons_errP(_Action, team_t_algemeen(_TeamID, _WC, _Tgnst, _Keuring, _MeldingTeam)) :- !.
cons_errP(_Action, toernooionderdeel_t(_WC, _Naam, _Speelsterkte, _ED, _HDG, _Leeftijdvanaf, _LeeftijdTotEnMet, _Keuring, _MeldingOnderdeel)) :- !.
cons_errP(_Action, inschrijfcheck(_Tijdstip)) :-!.

tracelog(Oper, Fact, IsOpen) :-
  comline(LineBuffer),
  upper_lower(LineBuffer, LBL),
  searchstring(LBL, "/x", _),
  logged(N),
  N1 = N + 1,
  assert(logged(N1)), 
  writedevice(WX),
  writedevice(screen),
  nl,write(N1, " ", Oper," ",Fact," ",IsOpen),
  writedevice(WX),  !.
tracelog(_, _, _) :- !.  

logf(Oper,Fact,Log) :-
  logf1(Oper,Fact,Log),
  writedevice(X),
  writedevice(screen),
  write("\n",Oper,Fact),
  writedevice(X), !.
logf(_, _, _) :-
  logclose().

webdeltasFile(File) :-
  get_tempdir(DirT, _),
  webdir(Map, _inet, _), !,
  concat(Map, logconstw, Webdeltas),  
  filenamepath(File, DirT, Webdeltas).

checkLogIsOpen(Hoe) :-
  logIsOpen(Hoe), !.
checkLogIsOpen(mdi) :-
  get_toernooidir(_, _, _, TB),
  %filenamepath(Logbestand, Dir, fctconst),
  myOpenmodify(logfile, TB),
  assert(logIsOpen(mdi)),
  webdeltasFile(File),
  openWrite(nawf, File), !.
checkLogIsOpen(lees) :-
  get_toernooidir(_, _, _, TB),
  %filenamepath(Logbestand, Dir, fctconst),
  myOpenread(0, logfile, TB),
  assert(logIsOpen(lees)),
  webdeltasFile(File),
  openWrite(nawf, File), !.
checkLogIsOpen(_).

logf1(_Oper,_Fact,_Open) :-
  alleenlezen1,
  _Answer = dlg_MessageBox( "Bestand", "Opgelet: veranderingen worden niet bewaard!", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), !.
logf1(Oper,Fact,Open) :-
  loglock(mdi, "logf1"),
  checklogIsOpen(mdi),
  action_term(Oper, Fact),
  writedevice(CurrentX),
  tracelog(Oper, Fact, Open),
  not(alleenlezen1),	% 31.12.2010
  writedevice(logfile),
  filepos(logfile, 0, 2),	% achteraan
  write(Oper,Fact), nl,		% deze moet!
  %writedevice(CurrentX), 
  webdir(_, inet,_),		% 1.9.2008
  writedevice(nawf),
  write(Oper,Fact), nl,		% hier wordt de clouddelta geschreven
  writedevice(CurrentX), 
  fail.
logf1(_,_,nee) :- 	% nee = niet open laten
  logclose(), !.
logf1(_,_,_).

herhaal(E, Aantal) :-	% aantal is nu op 4 gezet	30.6.2014
  E > 0,
  Aantal > 4, !. 
herhaal(E, Aantal) :-
  E > 0, !,
  Aantal1 = Aantal + 1,
  openLogVoorWeb(Aantal1).

openLogVoorWeb(Aantal) :- 	% download logdelta from web de nawf blijft open?
  webdir(Map, inet, _WB),
  get_toernooidir(_, _, _, TB),
% read deltalog from web
  %filenamepath(Logbestand, Dir, fctconst),
  closefile(logfile),
  AList = aanmeldenvars(),
  %sessionid(Session),
  %str_int(SesString, Session),
  webdatpositie(DatTimestamp, Begin, _),  % dattimestamp geeft de versie van de 'basis'-datfile aan
  str_int(BS, Begin),
  NAV = ["map", Map, "func", "getlogdelta", "begin", BS, "dattimestamp" , DatTimestamp | AList],	% 31.8.2008
  providerX(_, _, "tabun1.php", ProvM),
  Error = downloadData(ProvM, TB, NAV, _, 0),	% logdelta wordt ge-append  3 = error
  herhaal(Error, Aantal),
  format(Msg, "Verbinding met de server stokt (%u)!\n(kan ook gebeuren als de PC in de slaapstand gaat, stel het\nenergiemanagement niet te zuinig af!)\n\nProgramma opnieuw opstarten svp!", Error),
  writeToJournaal(Msg, ja),
  TaskWin = vpi_GetTaskWin(),
  win_Destroy(TaskWin),
  fail.
openLogVoorWeb(_) :-	% open extra log voor het naar web
  webdeltasFile(LogbestandW),  % = tasbestand
  openWrite(nawf, LogbestandW), !.
openLogVoorWeb(_).

merkRefresh() :-
  comline(LineBuffer),
  upper_lower(LineBuffer, LBL),
  searchstring(LBL, "/x", _),
  writedevice(X),
  writedevice(screen),
  write("¶"),
  writedevice(X), !.
merkRefresh().

loglock(_Openhoe, _Caller) :-	% 2.1.2011 openhoe is om de bestandsdatum niet te verzieken igv alleen lezen
  not(logIsOpen(_)),
  stopdbrefresh(),
  get_toernooidir(_Dir, _Map, _, TB),
  closefile(logfile), % voor de zekerheid
  vpi_ProcessEvents( b_True),
  openLogVoorWeb(1),     % + read delta from web    	% 31.8.2008
  assert(logIsOpen(mdi)),
  myOpenmodify(logfile, TB),
  merkRefresh(),
  filepos(logfile, 0, 2),	% bepaal achteraan positie
  filepos(logfile, Pos, 0),
  readdevice(Rdx),
  readdevice(logfile),
  check_log(Pos),	% kijk of er from extern wat veranderd is
  readdevice(Rdx),
  not(remoteCompressTookPlace()),
  doUpdateVensters(), !.		% kijk of een update nodig is
loglock(_Hoe,_) :-
  not(remoteCompressTookPlace()),
  logIsOpen(_), !.
loglock(_Hoe, _) :-
  not(remoteCompressTookPlace()),
  dlg_MessageBox( "Multiuser", "Lock failed", mesbox_iconerror, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.
loglock(_, _) :-
  retract(remoteCompressTookPlace()), !,
  retractall(logIsOpen(_)),
  closefile(logfile),
  closefile(nawf),
  TaskWin = vpi_GetTaskWin(),
  win_PostEvent(TaskWin,e_user(dotoernooiload,0)),
  closeMeteenKenmerken(),
  closeMeteenDeelIn(), !.
loglock(_, _).

prob(_X) :-
  retractall(logisopen(_)),
  writedevice(Y),
  writedevice(screen),
  write("!"),
  writedevice(Y).
  %exit(X).

logclose() :-
  closefile(nawf),
  logIsOpen(_), 
  trap(filepos(logfile, 0, 2),X,prob(X)),	% bewaar achteraan positie in logpos()
  filepos(logfile, Pos, 0),
  retractall(logPos(_)),
  assert(logPos(Pos)),
  fail.
logclose() :-
  logIsOpen(OpenHoe), 
  comline(LineBuffer),
  upper_lower(LineBuffer, LBL),
  searchstring(LBL, "/x", _),
  writedevice(X),
  writedevice(screen),
  write("±", OpenHoe),  % plusmin
  %logpos(Z),
  %logposIni(Y),
  %_Delta = Z - Y,
  %write("\nPos: ", Z, "  PosIni: ", Y, "  Delta: ", Delta),
  writedevice(X),
  fail.
logclose() :-
  closefile(logfile),
  webdir(Map, inet, _TB),	% 30.8.2008
  AList = aanmeldenvars(),
  webdeltasFile(LogbestandW),  % de delta's...
  %get_Toernooidir(_Dir, DirO, _, _),
  %filenamepath(LogbestandW, Dir, logconstw),
  existfile(LogbestandW),
  file_str(LogbestandW, Str),
  str_len(Str, Len), 
  %write("\n!!! ", Len),
  Len > 0,
  AAList = ["map", Map, "func", "loadlog" | AList],  % uploadlog!
  get_tempDir(TDir, _),
  filenamePath(TempFile, TDir, "feedback.txt"),
  providerX(_, _, "tabun1.php", ProvM),
  concat(LogBestandW, ".gz", LogBestandWC),
  %file_str(LogBestandW, Test),
  %write("\n #### LogbestandW: ",LogbestandW,"\n", Test),
  file_compress(LogBestandW, LogBestandWC),
  %writedevice(XXY),
  %writedevice(screen),
  %write("\nuploadlog from: ",LogBestandW),
  %writedevice(XXY),
  Return = fileupload(ProvM, LogBestandWC, TempFile, AAList),
  checkUpload(Return),
  fail.
logclose() :-
  retractall(logIsOpen(_)), 
  startDBrefresh().

checkUpload(0) :- !.
checkUpload(Error) :-
  format(Msg, "Verbinding met Internet stokt (%u)!\n\nHet programma wordt gestopt.", Error),
  %dlg_MessageBox( "Netwerk", Msg, mesbox_iconerror, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  writeToJournaal(Msg, ja),
  TaskWin = vpi_GetTaskWin(),
  win_Destroy(TaskWin),
  fail.

zetTerugenFail(PosX, PosX) :- !.
zetTerugenFail(PosX, _) :-
  filepos(logfile, PosX, 0),
  fail.

check_log(Pos) :-  % Pos = huidige logsize
  logPos(Pos1),	   % = vorige logsize.	
  Pos1 < Pos, 			% er is iets nieuws bijgekomen
  filepos(logfile, Pos1, 0),	%  zet terug waar we gebleven waren
  repeat_f(logfile),
  readchar(Action),
  trap(readterm(dbasedom,Fact),_,cons_err(Action, Pos)),
  action_term(Action,Fact),
  checkTerm(Action, Fact),
  vanKompaan(Action, Fact),	% voor test: report externe delta's in logfile
  eof(logfile),
  %filepos(logfile, PosX, 0),
  retractall(dataverdacht(_)),
  %writedevice(Wdev),
  %writedevice(screen),
  retract(logpos(_)), !,
  filepos(logfile, 0, 2),	% achteraan
  filepos(logfile, PosN, 0),
  assert(logPos(PosN)).
  %setkompaan(),
  %writedevice(Wdev).
check_log(Pos) :-
  logPos(Pos1),
  Pos1 > Pos, !,
  closefile(logfile),
  retractall(logIsOpen(_)),
  %writeToJournaal("Toernooibestand veranderd"
  %dlg_MessageBox( "Toernooibestand", "Toernooibestand is veranderd.", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  retractall(logPos(_)),
  TaskWin = vpi_GetTaskWin(),
  win_PostEvent(TaskWin,e_user(dotoernooiload,0)),
  closeMeteenKenmerken(),
  closeMeteenDeelIn(),
  writeToJournaal("Toernooibestand veranderd. Opnieuw geladen").	% reload
check_log(_).

wis_34plaats(Cat) :-
  kopl(Cat, pl3en4(_), Cat34, _),
  retractall(kopl(Cat, pl3en4(_), _, _)),
  retractall(wc(Cat34, _, _, _, _, _, _, _, _, _, _, _, _, _, _)),
  retractall(wd(_, Cat34, _, _, _, _, _)),
  retractall(w2(_, Cat34, _, _, _, _, _, _)),
  retractall(w3(_, Cat34, _, _, _, _, _)),
  retractall(ws(_, Cat34, _, _)),
  retractall(pos(Cat34, _, _, _)),
  %retractall(kopl(_, _, Cat, _)),   % alleen als eindstation
  %ruim_opT(Cat34),
  fail.
wis_34plaats(_Cat).

action_term(_, Fact) :- 
  del_term(Fact),
  fail.
action_term('R', _) :- !.
action_term('T',wd(_, Cat, _, _, _, _, _)) :-  
  wis_34plaats(Cat),
  retractall(wd(_, Cat, _, _, _, _, _)),
  retractall(w2(_, Cat, _, _, _, _, _, _)),
  retractall(w3(_, Cat, _, _, _, _, _)),
  retractall(ws(_, Cat, _, _)),
  retractall(pos(Cat, _, _, _)),
  retractall(kopl(Cat,verliezer,_,_)),
  not(isNrt(Cat)),
  wc(Cat, _, _, _, _, _, _, _, _, _, _, "", _, _, _), !, % niet als schemasoort
  retractall(kopl(_, _, Cat, _)).   % alleen als eindstation
  %retractall(kopl(Cat, _, _, _)).
action_term('T',_) :- !. 
action_term('A',Term) :- 
  asserta(Term), !.
action_term('Z',Term) :- !,
  assertz(Term).
action_term(X, Term) :-
  write("\nAction_term ",X, " ", Term, " ??").   

del_term(webdatpositie(DatumN, _Volgorde, Compress)) :-	
  webdatpositie(D1, _N1, _),
  isCompressNodig(Compress),
  retractall(webdatpositie( _, _, _)),
  %webdir(_, inet, _),	% 18.10.2010
  DatumN <> D1,
  assert(remoteCompressTookPlace), !.  % voor een internetmap dus
del_term(webdatpositie(_DatumN, _Volgorde, _)) :- 
  retractall(webdatpositie(_, _, _)), !.
del_term(toernooiK(Nr, _)) :- !,
  retractall(toernooiK(Nr, _)).
del_term(sp2(Persno,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)) :- !,
  retractall(sp2(Persno,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)).
del_term(wd(Num, Wedscat, _, _, _, _, _)) :- !,
  retractall(wd(Num, Wedscat, _, _, _, _, _)).
del_term(w2(Num, Wedscat, _, _, _, _, _, _)) :- !,
  incrBelangrijkeEventCounter(),
  retractall(w2(Num, Wedscat, _, _, _, _, _, _)).
del_term(w3(Num, Wedscat, _, _, _, _, _)) :- !,
  incrBelangrijkeEventCounter(),
  retractall(w3(Num, Wedscat, _, _, _, _, _)).
del_term(ws(Num, Wedscat, Tgnst, _)) :- !,
  retractall(ws(Num, Wedscat, Tgnst, _)).
del_term(opg(Num, Wedscat, _, _, _, _, _, _, _, _, _)) :- !,
  retractall(opg(Num, Wedscat, _, _, _, _, _, _, _, _, _)).
del_term(bn(Tijd, _)) :- !,
  retractall(bn(Tijd, _)).
del_term(pos(Wedscat, _, _, _)) :- !,
  retractall(pos(Wedscat, _, _, _)).
del_term(wc(Wedscat, _, _, _, _, _, _, _, _, _, _, _, _, _, _)) :- !,
  retractall(wc(Wedscat, _, _, _, _, _, _, _, _, _, _, _, _, _, _)).
del_term(spsrt(Sex, _, _, _, _, _, _, _)) :- !,
  retractall(spsrt(Sex, _, _, _, _, _, _, _)).
del_term(dag(Num, _, _)) :- !,
  retractall(dag(Num, _, _)).
del_term(club(Clubno, _, _)) :- !,
  retractall(club(Clubno, _, _)).
del_term(baannaam(Letter, _, Park, _, _)) :- !,
  retractall(baannaam(Letter, _, Park, _, _)).
%del_term(A) :- 		% al eens geprobeerd
 % retractall(A), !.*/
del_term(dagstatusinst(_, _)) :- !.
del_term(oproepen(_)) :- !.	% single facts
del_term(separatie(_)) :- !.
del_term(maxseparatie(_)) :- !.
del_term(dubbelvoorenkel(_)) :- !.	% 21.8.2016
del_term(duur(_)) :- !.
del_term(singles(_)) :- !.
del_term(banentool(_, _)) :- !.
del_term(vereniging1(_,_,_,_,_,_,_,_,_,_)) :- !.
%del_term(andereSexe(_)) :- !.
del_term(roosterinterval(_)) :- !.
del_term(dag_max(_)) :- !.
del_term(dag_max_o(_, _)) :- !.
del_term(betaal_adm(_)) :- !.
del_term(baan_adm(_)) :- !.
del_term(door_plannen(_)) :- !.
del_term(openbaar(_)) :- !.
del_term(naam(_, _, _, _, _, _)) :- !.
del_term(form(_, _, _, _, _, _, _, _, _, _, _, _, _, _)) :- !.
del_term(inschrijfcheck(PC,Tid,_, _)) :- !,
  retractall(inschrijfcheck(PC,Tid,_,_)).
del_term(bl(Cat, _)) :- !,
  retractall(bl(Cat, _)).
del_term(kopl(Cat, Pos, _, _)) :- !,
  retractall(kopl(Cat, Pos, _, _)).
del_term(pouleTelling(_)) :- !.
%del_term(omdraaienNaam(_)) :- !.
%del_term(lidnrcheck(_)) :- !.
del_term(klad(Nr, _, _)) :-
  retractall(klad(Nr, _, _)), !.
del_term(kladblok( _, _)) :-
  retractall(kladblok( _, _)), !.
del_term(blLft(Van, Tot, _)) :-
  retractall(blLft(Van, Tot, _)), !.
del_term(baantoew(No, Cat, _)) :-
  retractall(baantoew(No, Cat,_)), !.
del_term(spelerAanwezig(SpNo, _, _, _)) :-
  retractall(spelerAanwezig(SpNo, _, _, _)), !.
del_term(banknummers(_)) :- !.
%del_term(team_t_algemeen(TeamID, _, _, _, _)) :-	% van de keuringen
%  retractall(team_t_algemeen(TeamID, _, _, _, _)), !.
del_term(autoweb(PC, _,_,_, _, _, _, _, _, _, _)) :-
  retractall(autoweb(PC, _,_,_, _, _, _, _, _, _, _)), !.
del_term(algemeelX(MT, _, _, _, _)) :-
  retractall(algemeelX(MT, _, _, _, _)), !.
del_term(emailadresZelf(PC, _, _)) :-
  retractall(emailadresZelf(PC, _, _)), !.  
del_term(incassoeigennr(_)) :-
  retractall(incassoeigennr(_)), !.
del_term(autobak(_, _)) :-
  api_getComputername(CN),
  upper_lower(PC, CN),
  retractall(autobak(PC, _)), !.
del_term(uitslagenable(_)) :-
  retractall(uitslagenable(_)), !.
del_term(weblayout(AfdrOfBrowse, _, _, _, _, _, _)) :-
  retractall(weblayout(AfdrOfBrowse, _, _, _, _, _, _)), !.
del_term(schemapresel(_CatL)) :- !.
del_term(functionaris(Ftype, ToernooiId, _, _, _, _, _)) :-
  retractall(functionaris(Ftype, ToernooiId, _, _, _, _, _)), !.  
del_term(tshirt(_,_)) :- !.
%  retractall(schemapresel(CatL)), !.
del_term(X) :-
  write("\ndel_term ",X," ??").

repeat.
repeat:-repeat.

ontsleutel(List, String) :-
  list_bits(List, Out, G, _),
  bits_chars(1, Out, 0, "", String),
  %write("\nG: ", G, "\n"), 
  crc_check(String, 0, G), !.
ontsleutel(_, "") :-
  exit(2).

bits_chars(7, [B1 | Rest], CharIn, StrIn, Out) :- !,
  bitleft(CharIn, 1, A1),
  bitand($00FF, A1, A2),
  bitor(A2, B1, Char),
  %write(B1),
  bitxor($002A, Char, Charx),
  char_int(C, Charx),
  %write(C),
  str_char(C1, C),
  concat(StrIn, C1, In1), 
  bits_chars(1, Rest, 0, In1, Out).
bits_chars(Curr, [B1 | Rest], CharIn, StrIn, StrOut) :- !,
  bitleft(CharIn, 1, A1),
  bitand(A1, $00FF, A2),
  bitor(A2, B1, Char),
  Char1 = cast(char, Char),
  Curr1 = cast(short,Curr + 1),
  bits_chars(Curr1, Rest, Char1, StrIn, StrOut).
bits_chars(_, _, _, Str, Str).

list_bits([G,Pos,H|List], Out, G, 0) :-       %splits de positie af
  int_bits(Pos, H, [], Last),
  list_bits(List, Last, Out).
  
list_bits([], In, In) :- !.
list_bits([H|T], In, Bits) :-
  int_bits(16, H, In, B1), 
  list_bits(T, B1, Bits).

int_bits(1, Int, BitsIn, [Bit1|BitsIn]) :- !,
  bitright(Int, 15, Bit),
  bitand(Bit, 1, Bit1).
int_bits(Curr, Int, BitsIn, [Bit1|BitsOut]) :-
  bitright(Int, 15, Bit),
  bitand(Bit, 1, Bit1),
  C1 = cast(short,Curr - 1),
  bitleft(Int, 1, I1),
  I2 = cast(short,I1),
  %bitand(I1, $FFFF,I2)
  int_bits(C1, I2, BitsIn, BitsOut).

crc_check("",X,X) :- !.   /* cyclic redundancy check */
crc_check(String,In,X) :-
  frontchar(String,Char,Rest),
  char_int(Char,Int),
  bitxor(In,Int,Out),
  bitand(Out,$0001,B),
  bitleft(B,15,B1),
  bitright(Out,1,Out1),
  bitor(Out1,B1,Out2),
  Out3 = cast(ushort,Out2),
  %write(" ", Out3),
  crc_check(Rest,Out3,X).   

database - local1

%determ  recNr(integer)
recno(integer, sex, integer)
%sp_uit(integer)
cl_sp(integer)
%single expath(string)

%__________________ Algemeen ______________________________________________

predicates
  som(integerlist, integer, integer)
  skip_blank(string, string)
  blank(char) 
  %kleinste(integer, integer, integer)
  skip_delim(string, string) 
  substring1(string, integer, integer, string) -(i,i,i,o)
determ  get_string(string, string, string, integer Maxlen) - (i, o, o, i)
determ  get_string(char,string, string, string,string) - (i, i, o, i, o)
 init_recnoIn
  inc_recno(integer, sex)
determ get_string1(string In, string String, string Out) - (i,o,o)

clauses

%expath("").	% het export pad

init_recnoIn() :-
  retractall(recno(_,_,_), local1),
  retractall(cl_sp(_), local1),
  spsrt(N, _, _, _, _, _, _, _),
  assert(recno(1, N, 0)),
  assert(recno(2, N, 0)),
  assert(recno(3, N, 0)),
  fail.
init_recnoIn().

inc_recno(Type, Cat) :-
  retract(recno(Type, Cat, No)), !,
  N1 = No + 1,
  asserta(recno(Type, Cat, N1)).

som([], I, I) :- !.
som([H|T], I, O) :-
  I1 = H + I,
  som(T, I1, O).

get_string1(In, String, Out) :-
  searchchar(In, csvconst,FoundPos),
  F1 = Foundpos - 1,
  frontstr(F1,In,String,R1),
  frontchar(R1,_FC,Out), !.
get_string1(In, In, "").

get_string(In, String, Out, MaxLen) :-
  str_len(In, Len),
  Len > 0,
  skip_delim(In, Out1), 
  skip_blank(Out1, Out2), 
  frontchar(Out2, Char, Out3), !,
  get_string(Char, Out3, Out, "", String1),
  trim_c(String1),
  substring1(String1, 1, MaxLen, String).
get_string(_, "", "", _).

get_string('"', "\"", "", InS, InS):- !.
get_string(';', "", "", InS, InS):- !.	% 20.5.2008
get_string(Ch, "", "", InS, OutS):- !,
  frontchar(OutS, Ch, InS).
get_string('"', In, Out, InS, InS):-
  frontchar(In, '"', Out),	% kom een " tegen 
  skip_blank(Out, Out2),	% gevolgd door delimiter?
  frontchar(Out2, csvconst, _), !.   % ja dan token klaar
get_string('"', In, Out, InS, OutS):-
  frontchar(In, Char, Out1), !, 
  get_string('"', Out1, Out, InS, OutS1),
  frontchar(OutS, Char, OutS1). 
get_string(csvconst, In, Out, InS, InS):-
  frontchar(Out, csvconst, In), !.
get_string(Char, In, Out, InS, OutS):-
  frontchar(In, Char1, Out1), !, 
  get_string(Char1, Out1, Out, InS, OutS1),
  frontchar(OutS, Char, OutS1). 

skip_delim("", _) :- !,
  exit(118).	% te weinig velden
skip_delim(In, Out) :-
  frontchar(In, csvconst, Out), !.
skip_delim(In, Out) :-
  skip_blank(In, Out1),
  skip_delim(Out1, Out), !.
skip_delim(_, _) :-
  exit(116).	% vreemd scheidingsteken
  
skip_blank(In, Out) :-
  frontchar(In, Char, Out1),
  blank(Char), !,
  skip_blank(Out1, Out).
skip_blank(In, In).

blank(' ').
blank('\t').

substring1(String, Beg, Len,  Out) :-
  str_len(String, Leng),
  Lg = Leng + 1,
  Beg < Lg, !,
  End = Beg + Len,
  kleinste(Lg, End, Kl),
  L1 = Kl - Beg,
  substring(String, Beg, L1, Out).
substring1(_, _, _, "").


/*
===========================================================================
	XDBASE3.PRO	

	Access a dBase 3 (V1.1) compatible file from Prolog

	The two datafiles xdbase3.dbf and xdbase3.dbt must
	be present at the default directory.

	Copyright (C) 1990 Prolog Development Center
===========================================================================
*/

/*
==========================================================================
	READEXT.PRO

	Utility predicates for import routines

	Copyright (C) 1990,1992 Prolog Development Center
==========================================================================
*/


DOMAINS
  ptrInt=struct ptrint(integer)
  ptrlong=struct ptrlong(long)
  ptrreal=struct ptrreal(real)
PREDICATES
  read_int(Integer)

ifdef platform_32bit
  read_long(Real) - (o)
elsedef
  read_long(long) - (o)
enddef

  read_strArr(Integer,String)
  read_strCtrlZ(String)
  read_str2CtrlZ(String,String)
  blockNStr(binary, integer, string)
  make_safe(integer, string,string)

  ignore(Integer)

CLAUSES

  read_int(V) :-
ifdef platform_32bit
	readchar(Low), 
	readchar(High), 
	bitleft(High,8,Vh), 
	V = Low + Vh.
elsedef
	readblock(2, PIntBin),
	Pint=cast(ptrInt, PIntBin),
	Pint=ptrInt(V).
enddef

  read_long(Long) :-
ifdef platform_32bit  
	read_int(VLow), 
	read_int(_), 	%throw away high order byte???
	Long=Vlow.
elsedef
	readblock(4, PLongBin),
	PLong=cast(ptrLong, PLongBin),
	PLong=ptrLong(Long).
enddef

  read_strArr(N,S) :-    
	filepos(work,0,1),
  	readblock(N,SBinary),  %readblock
	blockNStr(SBinary,N, S).  	

  read_strCtrlZ(Str) :- 
	read_str2CtrlZ("",Str).
  read_str2CtrlZ(Scur,Str) :-
	readchar(CH), 
	CH<>'\026', !, 	% CtrlZ => CP/M way signaling eof 
	str_char(S1,CH), 
	concat(Scur,S1,Scur2),
	read_str2CtrlZ(Scur2,Str).
  read_str2CtrlZ(S,S).

 blockNStr(Bin,N,Str):-
 	StrUnsafe=cast(string,Bin),
 	make_safe(N, StrUnsafe, Str).
 
 make_safe(N,S,S2):- trap(substring(S,1,N,S2),_,true),!. 
 make_safe(_,S,S2):- concat(S,"",S2).  % str len < N
/*************************************************************
	Ignore Characters
*************************************************************/

  ignore(0) :- !.
  ignore(N) :- 
	readdevice(Device),
	filepos(Device, N, 1).

Domains
/*************************************************************
	Prolog representation of the data base
*************************************************************/

  Dbase3RecL	= Dbase3Rec*	    % A database is a number of records 
  Dbase3Rec	= Dbase3Elem*	    % A record is a number of fields 
  Dbase3Elem	= char(String);	    % Characters 
  		  real(Real);	    % 64-bit IEEE floating point 
  		  logical(Bool);    % Logical
  		  memo(String);	    % 10 digits rep. a .DBT block no 
  		  date(String)	    % format YYYY MM DD 
  
  Bool		= Char		    % Y y N n T t F f or Space 
  
  FldDescL	= FldDesc*	    % description for each field 
  FldDesc	= flddesc(Dbase3Type,Integer)
  Dbase3Type	= ch;r;l;m;d
  
  FldNameL	= String*

PREDICATES
  /* Read predicates */
  Init_Dbase3(Real,FldNameL,FldDescL)
  rd_dbase3_DbaseHeader(Real)
  rd_dbase3_fieldDescL(FldNameL,FldDescL)
  rd_dbase3_DataRec(File,FldDescL,dBase3Rec)
  rd_dbase3_elem(File,FldDesc,dBase3Elem)

  conv_FldType(Char,dBASE3Type)

  /* Read a single record */
  rd_dbase3_DataRec1(Real,Real,File,FldDescL,dBase3Rec)
  nondeterm rd_dbase3Rec(Real,File,FldDescL,dBase3Rec)

CLAUSES
  Init_Dbase3(TotRecs,FldNameL,FldDescL):-
	rd_dbase3_DbaseHeader(TotRecs),
	rd_dbase3_fieldDescL(FldNameL,FldDescL).


/*************************************************************
	Read Dbase header
*************************************************************/

  rd_dbase3_DbaseHeader(TotRecs1):-
	ignore(4),	    % ID & Last update & record size 
	read_long(TotRecs1), % 32-bit number 
	ignore(24).	    % Header length, Record length & Reserved


/*************************************************************
	Read Field descriptors
*************************************************************/

  rd_dbase3_fieldDescL([FldName|FldNameL],[fldDesc(Type,Len1)|FldDescL]):-
	readchar(Ch), 
	Ch<>'\013',!, 		    % CR means final array field 
	read_strArr(10,Name), 	    % read rest of field name	
	frontchar(FldName,Ch,Name), % add back the starting letter
	readchar(T), 
	conv_FldType(T,Type),
	ignore(4),		    % skip data address 
	readchar(Len),
	Len1 = cast(char, Len), 
	ignore(15),		    % skip decimal count & Reserved 
	rd_dbase3_FieldDescL(FldNameL,FldDescL).

  rd_dbase3_FieldDescL([],[]):-
 % 	filepos(le_file,8,0),	% less version dependant...
  	filepos(work,8,0),	% less version dependant...
  	read_int(Datastart),
 % 	filepos(le_file,Datastart,0).  
  	filepos(work,Datastart,0).  

  conv_FldType('C',ch):-!.
  conv_FldType('N',r):-!.
  conv_FldType('L',l):-!.
  conv_FldType('M',m):-!.
  conv_FldType('D',d):-!.

  
/*************************************************************
	Read Data Records
*************************************************************/

  rd_dbase3_datarec1(Ni,No,MFP,FldDescL,Rec):-
	readchar(NotDel), 
	NotDel=' ',!,	
	No=Ni-1,
	rd_dbase3_DataRec(MFP,FldDescL,Rec).
  rd_dbase3_datarec1(Ni,No,MFP,FldDescL,Rec):-
	Ni2=Ni-1, 
	rd_dbase3_datarec(MFP,FldDescL,_),
	rd_dbase3_DataRec1(Ni2,No,MFP,FldDescL,Rec).

  rd_dbase3_DataRec(_,[],[]):-!.
  rd_dbase3_DataRec(MFP,[FldDesc|FldDescL],[Elem|ElemL]):-
	rd_dbase3_elem(MFP,FldDesc,Elem), 
	%write(" ", Elem),
	rd_dbase3_DataRec(MFP,FldDescL,ElemL).

  rd_dbase3_elem(_,fldDesc(ch,Len),char(Str)):-
	!,
	read_strArr(Len,Str).
	%write(" Len:", Len, " Str: ", Str).
  rd_dbase3_elem(_,fldDesc(l,Len),logical(Char)):-!,
	readchar(Char), 
	ToSkip=Len-1, 
	ignore(ToSkip).
  rd_dbase3_elem(_,fldDesc(r,Len),char(Str)):-!, %  rd_dbase3_elem(_,fldDesc(r,Len),real(Real)):-!,
	read_strArr(Len,Str).
	%str_real(Str,Real).
  rd_dbase3_elem(MFP,fldDesc(m,Len),memo(Memo)):-!,
	read_strArr(Len,BlkNo), 
	str_int(BlkNo,P), 
	Pos=P*512,
	readdevice(work), 
%	readdevice(le_file), 
	readdevice(MFP),
	filepos(MFP,Pos,0), 
	read_strCtrlZ(Memo),
%	readdevice(le_file).
	readdevice(work).
  rd_dbase3_elem(_,fldDesc(d,Len),char(Date)):-!,%rd_dbase3_elem(_,fldDesc(d,Len),date(Date)):-!,
	read_strArr(Len,Date).


/*************************************************************
	Read Data record Sequential
*************************************************************/

  rd_dbase3Rec(0.0,_,_,_):-!,fail.
  rd_dbase3Rec(N,MFP,FldDescL,Rec):-
	rd_dbase3_datarec1(N,_,MFP,FldDescL,Rec).
  rd_dbase3Rec(N,MFP,FldDescL,Rec):-
	N2=N-1, 
	rd_dbase3Rec(N2,MFP,FldDescL,Rec).


%_____________________________________________________________________
%			spelers_in
%_____________________________________________________________________

database - eerste
determ algedaan()
single reccount(integer)
%spelerlidno(integer SpNo, string KNLTB, string TrimmedNaam)
single lidnrlen(integer)

predicates

  check_fields(FldDescL)
  chOr(FldDesc)
  sp_in1(WINDOW Msg, WINDOW progress, string file, integer Mode, ilist Details)
  list_sp(Dbase3Rec)
  expand(string, string)
procedure metinhoud(string, string, string) - (i,i,o)
  determ_sex(string Naam, string, string, string, string, sex) - (i,i,i,i,i,o)
  procedure determ_sexPr(string Naam, string, string, string, string, sex)
  spelers_invoerITF(WINDOW Msg, WINDOW Progr, string, integer Mode, ilist Details) - (i, i, i, i, i)
  spi_result(integer)
  tokenize_handle(WINDOW Msg, string)
  %tokenize_handleITF(integer Stap, WINDOW Msg, string)
  tokenize_tabs(window Msg, string Line, ilist Details)
  procedure searchTab(string Line, string Token, string Rest)
  procedure roepnaam(ilist Details, string, string, string)
  naam_fout(naam)
  upd_sp(dbasedom)
  upd_sp1(dbasedom) - (i)
  spsoort_present(string, string, string, string, string)
  sex(string, sexe)
  lasttoken1(string, string, string, string)  - (i, i, o, o)
  bestaandSpelerNummer(knltb)
  foutboodschap(integer, string)
  procedure eerste_keer(WINDOW MsgWin, integer RecordLengte, integer LidNrLengte)
  filter1(integer)
  procedure incasso(string Bank, string Giro, string Incasso)
  procedure geslacht(string, string)
  procedure metEmail(string, string)
  procedure leeftijdComp(wedscat, paspoort, integer, integer) % voor 4Q jeugd etc
    
clauses
reccount(0).
lidnrlen(0).

spelers_invoer(Win,6) :-
  Bestand = dlgFileNameZeker("*.txt",["csv", "*.csv", "exp","*.exp","dat","*.dat","txt","*.txt","dbf","*.dbf"],"Selecteer ITF-bestand",[],"",_OutListFiles),
  %Bestand = "C:\\Users\\JanLap\\Documents\\My Dropbox\\NED 13-19 August Hoofddorp.csv",
  existfile(Bestand),
  write("\nBestaat: ",Bestand),
  %write("\nVergelijk: ", Bestand1), 
  dlg_invoer_Create(Win, Bestand, 6), !.
spelers_invoer(_Win, 4) :-
  xml_invoer_file(), !.
spelers_invoer(Win, 5) :-	% KNLTB download
  getFolder(PathIn,_,_,_),
  Filenaam = dlgFileNameZeker("*.csv",["csv", "*.csv", "exp","*.exp","dat","*.dat","txt","*.txt","dbf","*.dbf"],"Knltb-bestand, opgeslagen als csv",[],PathIn,_OutListFiles),
  %filenamepath(Filenaam, Path, _Name),
  %assert(expath(Path)),
  dlg_MessageBox( "Invoer", "Als dit bestand zojuist via een ander programma (Excel?) gemaakt is, zorg er dan voor dat dat nu gestopt is!", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  dlg_invoer_Create(Win, Filenaam, 5),
  compress(update),
  startDbRefresh(),
  !.
spelers_invoer(Win, Mode) :-
  Mode = 3,
  getfolder(PathIn,_,_,_),
  Filenaam = dlgFileNameZeker("*.dbf",["exp","*.exp","dat","*.dat","txt","*.txt","dbf","*.dbf"],"Selecteer invoerbestand",[],PathIn,_OutListFiles),
  %filenamepath(Filenaam, Path, _Name),
  %assert(expath(Path)),
  dlg_invoer_Create(Win, Filenaam, Mode), !.
spelers_invoer(Win, Mode) :-
  Mode < 3,
  getfolder(PathIn,_,_,_),
  Filenaam = dlgFileNameZeker("*.csv",["exp","*.exp","dat","*.dat","txt","*.txt","dbf","*.dbf","csv","*.csv"],"Selecteer invoerbestand",[],PathIn,_OutListFiles),
  %filenamepath(Filenaam, Path, _Name),
  %assert(expath(Path)),
  dlg_invoer_Create(Win, Filenaam, Mode), !.
spelers_invoer(_Win, _).
  
/*spelers_invoerITF(MsgWin, ProgressWin, File, 6, Details) :-	% 6= ITF-spelers
  %openread(work, File),
  myOpenread(0, work, File),
  readdevice(work),
  init_recnoIn(),
  sp_in1(MsgWin, ProgressWin, "", 6, Details), !.
spelers_invoerITF(_, _, _, 6, _) :-
  closefile(work), !. */
spelers_invoerITF(MsgWin, ProgressWin, File, 5, Details) :-	% 1= vaste kolom, 2=puntkomma, 3=dbf, 4=xml, 5=knltb(tab)
  myOpenread(0, work, File),
  init_recnoIn(),
  filenamepath(File, _Path, Name),
  filenameext(Name,_, Ext), 	% determine filetype
  upper_lower(Ext1, Ext),
  sp_in1(MsgWin, ProgressWin, Ext1, 5, Details), !.
spelers_invoerITF(_, _, _, 5, _) :-
  closefile(work),
  exit(120).  
spelers_invoerITF(MsgWin, ProgressWin, File, Mode, _) :-	% 1= vaste kolom, 2=puntkomma, 3=dbf, 4=xml, 5=knltb(tab)
%	Title="Title",
%	str_int(MS,Mode),
%	dlg_Note(Title,MS),
  Mode < 4,
  myOpenread(0, work, File),
  init_recnoIn(),
  filenamepath(File, _Path, Name),
  filenameext(Name,_, Ext), 	% determine filetype
  upper_lower(Ext1, Ext),
  sp_in1(MsgWin, ProgressWin, Ext1, Mode, []), !.
spelers_invoerITF(_, _, _, Mode, _) :-
  Mode < 4,
  closefile(work),
  exit(120).  

chOr(flddesc(ch,_)) :- !.
chOr(flddesc(r,_)) :- !.
chOr(flddesc(d,_)) :- !.

check_fields([F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11|_]) :-
%  write("\nCheck fields"),
  F1 = flddesc(ch, _),
  F2 = flddesc(ch, _),
  F3 = flddesc(ch, _),
  F4 = flddesc(ch, _),
  F5 = flddesc(ch, _),
  F6 = flddesc(ch, _),
  chOr(F7),
  chOr(F8),
  F9 = flddesc(ch, _),
  F10 = flddesc(ch, _),
  chOr(F11), !.
check_fields([_F1,_F2,_F3,_F4,_F5,_F6,_F7,_F8,_F9,_F10,_F11,_F12,_F13, _F14, _F15, _F16, _F17, _F18, _F19, _F20|_]) :- !.
check_fields(_) :-
   exit(122).

sp_in1(_Msgwin, ProgressWin, _/*".DBF"*/, 3, _Details) :-
  retractall(algedaan()),
  assert(reccount(0)),
  filemode(work,0), readdevice(work),
  init_dbase3(TotRecs,_FldNameL,FldDescL),
  format(Msg,"Aantal te lezen records: %u.",TotRecs), 
  writeToJournaal(Msg),
  check_fields(FldDescL),
  writeToJournaal("Field check OK"),
  rd_Dbase3Rec(TotRecs,work,FldDescL,Rec),
  reccount(N),
  list_sp(Rec),
  TotRecs1 = cast(ulong, TotRecs),
  N1 = N+1,
  assert(reccount(N1)),
  readdevice(work), 
  N mod 4 = 0,
  progress_bar_set_value(ProgressWin, TotRecs1, N),
  vpi_ProcessEvents(b_True ),
  fail.
sp_in1(MsgWin, ProgressWin, _Ext, Mode, _Details) :-
  Mode < 3,
  %Ext <> ".DBF",
  assert(lidnrlen(0)),
  retractall(algedaan()),
  filePos(work,1,2),
  filePos(work, Len, 0),
  filePos(work, 0, 0),
  readdevice(work),
  readln(_StringX), % write(String),
  repeat_f(work), 
  readdevice(work), 
  readln(String), % write(String),
  filePos(work, Current, 0),
  readdevice(keyboard),
  tokenize_handle(MsgWin, String),
  Current mod 2 = 0,
  progress_bar_set_value(ProgressWin, Len, Current),
  vpi_ProcessEvents(b_True ),
  fail.
sp_in1(MsgWin, ProgressWin, _Ext, 5, Details) :-		% tab en csv jeugd (KNltb)
  %Ext <> ".DBF",
  assert(lidnrlen(0)),
  retractall(algedaan()),
  filePos(work,1,2),
  filePos(work, Len, 0),
  filePos(work, 0, 0),
  readdevice(work), 
  readln(_Header), % die niet..
  repeat_f(work), 
  readdevice(work), 
  readln(String), %write(String),
  filePos(work, Current, 0),
  readdevice(keyboard),
  tokenize_tabs(MsgWin, String, Details), 
  Current mod 2 = 0,
  progress_bar_set_value(ProgressWin, Len, Current),
  vpi_ProcessEvents(b_True ),
  fail.

eerste_keer(_, _, N) :-
  lidnrlen(N),
  N > 0, !.
eerste_keer(MWin, 143, 7) :- 
  win_SetText(MWin, "Lidnummerlengte = 7"),
  assert(lidnrlen(7)), !.
eerste_keer(MWin, 145, 7) :-
  win_SetText(MWin, "Lidnummerlengte = 7"),
  assert(lidnrlen(7)), !.
eerste_keer(MWin, _, 8) :-
  win_SetText(MWin, "Lidnummerlengte = 8"),
  assert(lidnrlen(8)), !.

list_sp([_, char(Voornaam),char(Achternaam),char(Tussenv),char(Adres),char(Postcode),char(Woonplaats),
	char(Geb), char(Tel1), char(Tel2), char(Tel3), char(_T1M), char(_T2M), char(_T3M), 
	char(M_V), char(E_Mail),char(_Club), char(ClubCode), char(Ste), char(StD),_,char(LidNr)|_]) :- 
  upper_lower(Tussenv, TussenvL),		% puur op aantal kolommen wordt geselecteerd (MTCS)
  trim_c(Achternaam),
  trim_c(Voornaam),
  trim_c(TussenVL),
  format(Naam, "%s,%s %s", Achternaam, Voornaam, TussenvL),
  format(Woonpl, "%s %s", Postcode, Woonplaats),
  not(naam_fout(Naam)),
  upper_lower(Ste1, Ste),
  upper_lower(Std1, Std),
  %substring1(SexS,   1, 2, SexS1),   trim_c(SexS1),
  substring1(M_V,    1, 1, M_V1),    trim_c(M_V1),
  substring1(Naam,   1, 29, Naam1),  trim_c(Naam1),
  substring1(Adres,  1, 29, Adres1), trim_c(Adres1),
  substring1(Woonpl, 1, 29, Woonpl1),trim_c(Woonpl1),
  substring1(Tel1,    1, 29, Tel11),   trim_c(Tel11),
  substring1(Tel2,    1, 29, Tel22),   trim_c(Tel22),
  substring1(Tel3,    1, 29, Tel33),   trim_c(Tel33),
  substring1(Lidnr,  1, 8,  Lidnr1), trim_c(Lidnr1),
  substring1(ClubCode,   1, 5,  Club1),  trim_c(Club1),
  substring1(Ste1,   1, 2,  Ste2),   trim_c(Ste2),
  substring1(Std1,   1, 2,  Std2),   trim_c(Std2),
  substring1(Geb,    1, 10,  Geb2),  trim_c(Geb2),
  determ_sex(Naam1, M_V1, Ste2, Std2, Geb2, SexN),
  trim_c(E_Mail),
  trim_c(Naam1), !,			% en haal meteen de blanks weg
  upd_sp(sp2(0,SexN,Naam1,Tel11,[],"","",nee,0, Club1, Adres1, Woonpl1, LidNr1, Ste2, StD2, Geb2, Tel22, Tel33, E_Mail, "",nee,"","","","","",[])).
list_sp([char(_SexS),char(M_V),char(Naam),char(Adres),char(Woonpl),char(Tel),
          char(LidNr),char(Club),char(Ste),char(Std),char(Geb)|_]) :- 
  not(naam_fout(Naam)),
  upper_lower(Ste1, Ste),
  upper_lower(Std1, Std),
  %substring1(SexS,   1, 2, SexS1),   trim_c(SexS1),
  substring1(M_V,    1, 1, M_V1),    trim_c(M_V1),
  substring1(Naam,   1, 29, Naam1),  trim_c(Naam1),
  substring1(Adres,  1, 29, Adres1), trim_c(Adres1),
  substring1(Woonpl, 1, 29, Woonpl1),trim_c(Woonpl1),
  substring1(Tel,    1, 29, Tel1),   trim_c(Tel1),
  substring1(Lidnr,  1, 8,  Lidnr1), trim_c(Lidnr1),
  substring1(Club,   1, 5,  Club1),  trim_c(Club1),
  substring1(Ste1,   1, 2,  Ste2),   trim_c(Ste2),
  substring1(Std1,   1, 2,  Std2),   trim_c(Std2),
  substring1(Geb,    1, 10,  Geb2),  trim_c(Geb2),
  determ_sex(Naam1, M_V1, Ste2, Std2, Geb2, SexN),
  trim_c(Naam1),			% en haal meteen de blanks weg
  upd_sp(sp2(0,SexN,Naam1,Tel1,[],"","",nee,0, Club1, Adres1, Woonpl1, LidNr1, Ste2, StD2, Geb2, "", "", "", "",nee,"","","","","",[])), !.

searchTab(String, Token, Rest) :-	% laatste token niet van belang!
  searchchar(String,'\t',P),
  P0 = P - 1,
  frontstr(P0,String,Token,RestString),
  frontchar(RestString,_FrontChar,Rest), !.
searchTab(String, "", String).	% laatste token niet van belang!

roepnaam(Details, Roepnaam, Voorletters, Voornamen) :-
  member(3, Details),
  frontchar(Roepnaam,FrontChar,_),
  frontchar(Voorletters,FrontChar,Rest),
  frontchar(Rest, '.', Rest1),
  format(Voornamen, "%s %s", Roepnaam, Rest1),
  trim_c(Voornamen), !.
roepnaam(Details, Roepnaam, Voorletters, Voornamen) :-
  member(3, Details),
  frontchar(Roepnaam,FrontChar,_),
  frontchar(Voorletters,FrontChar,Rest),
  format(Voornamen, "%s %s", Roepnaam, Rest), !.
roepnaam(Details, Roepnaam, Voorletters, Voornamen) :-
  member(3, Details), !,
  format(Voornamen, "%s %s", Roepnaam, Voorletters).  
roepnaam(Details, Roepnaam, _Voorletters, Roepnaam) :-
  member(1, Details), !.
roepnaam(_, _Roepnaam, Voorletters, Voorletters).
  % member(1, Details), !. Niet nodig.

incasso("", GiroNr, Incasso) :-
  format(Incasso, "G%s", GiroNr), !.
incasso(B, _, B).

geslacht("1", "M") :- !.
geslacht("2", "V") :- !.

geslacht("Man", "M") :- !.
geslacht("man", "M") :- !.
geslacht(_, "V").

/*omdraaienGebDatum(GebDatum, DatumGeb) :-
  fronttoken(GebDatum, Dag, R1),
  frontchar(R1, '-', R2),
  fronttoken(R2, Maand, R3),
  frontchar(R3, '-', Jaar),
  format(DatumGeb, "%s-%s-%s", Jaar, Maand, Dag), !.
omdraaienGebDatum(X, X).*/

tokenize_tabs(_, String, _Details) :-
  searchchar(String, '{', Pos),
  Pos = 1, !,			% KNLTB download
  get_string1(String, _Lidmaatschap, Rest),
  get_string1(Rest, _Vereniging, Rest1),
  get_string1(Rest1, BondsNr, Rest2), 
  get_string1(Rest2, _Lidnaam, Rest3), 
  get_string1(Rest3, _Voorletters, Rest4), 
  get_string1(Rest4, Tussenvoegsels, Rest5),
  get_string1(Rest5, AchterNaam, Rest6),
  not(naam_fout(AchterNaam)),  % niet naam die niet kan
  trim_c(AchterNaam),			% en haal meteen de blanks weg
  get_string1(Rest6, Roepnaam, Rest7),
  get_string1(Rest7, Straat, Rest8),
  get_string1(Rest8, HuisNr, Rest9),
  get_string1(Rest9, Toevoeging, Rest10),
  get_string1(Rest10, Postcode, Rest11),
  get_string1(Rest11, Plaats, Rest12),
  get_string1(Rest12, _Landcode, Rest13),
  get_string1(Rest13, GebDatum, Rest14),
  get_string1(Rest14, _JunSen, Rest15),
  get_string1(Rest15, Gesl, Rest16),
  get_string1(Rest16, Email, Rest17),
  get_string1(Rest17, Telthuis, Rest18),
  %
  get_string1(Rest18, Telwerk, Rest19),
  get_string1(Rest19, Telmobiel, Rest20),
  get_string1(Rest20, IBAN, Rest21),  
  %
  get_string1(Rest21, _LidmaatschapVanaf, Rest22),
  get_string1(Rest22, _DatumOpgezegd, Rest23),
  get_string1(Rest23, _RefNr, Rest24),
  get_string1(Rest24, ES, Rest25),
  get_string1(Rest25, DS, Rest26),
  get_string1(Rest26, EnkelRating, Rest27),
  get_string1(Rest27, DubbelRating, _Rest28),
  %roepnaam(Details, Roepnaam, Voorletters, Voornamen),
  %omdraaienGebDatum(GebDatum, DatumGeb),
  format(NaamN, "%s,%s %s", AchterNaam, Roepnaam, Tussenvoegsels),
  trim_c(NaamN),
  geslacht(Gesl, MV),
  determ_sex(NaamN, MV, "1", "1", Gebdatum, SexN),	% 20.1.2014
  %nieuwOfBestaand(BondsNr, Skeus, SexN),
  format(Adres, "%s %s %s", Straat, Huisnr, Toevoeging),
  trim_c(Adres),
  format(WoonPl, "%s %s", Postcode, Plaats),
  inc_recno(3, SexN), 
  upd_sp(sp2(0,SexN,NaamN,Telthuis,[],EnkelRating,DubbelRating,nee,0, "", Adres, Woonpl, BondsNr, ES, DS, GebDatum, Telwerk, Telmobiel, Email, "",nee,"","",IBAN,"","",[])),
  !.
tokenize_tabs(_, S0, Details) :-
  str_len(S0, Len),
  Len >30,
  searchTab(S0,BondsNr,S1),
  searchTab(S1,Achternaam,S2),
  searchTab(S2,Voorletters,S3),
  searchTab(S3,Tussenvoegsels,S4),
  searchTab(S4,Roepnaam,S5),
  searchTab(S5,Straat,S6),
  searchTab(S6,Huisnr,S7),
  searchTab(S7,Huisnr_Toev,S8),
  searchTab(S8,Postcode,S9),
  searchTab(S9,Woonplaats,S10),
  searchTab(S10,_Land,S11),
  searchTab(S11,Telthuis,S12),
  searchTab(S12,_Fotonr,S13),
  searchTab(S13,MV,S14),
  searchTab(S14,Gebdatum,S15),
  searchTab(S15,_LidVanAf,S16),		% niet gebruikt
  searchTab(S16,_VerenigingsLidnr,S17),
  searchTab(S17,ES,S18),
  searchTab(S18,DS,S19),
  searchTab(S19,EnkelRating,S20),
  searchTab(S20,DubbelRating,S21),
  searchTab(S21,_EnkelOud,S22),
  searchTab(S22,_DubbelOud,S23),
  searchTab(S23,_Wedst,S24),		% niet gebruikt
  searchTab(S24,_Ten,S25),		% niet gebruikt
  searchTab(S25,_CC,S26),		% niet gebruikt
  searchTab(S26,Email,S27),
  searchTab(S27,Telwerk,S28),
  searchTab(S28,Telmobiel,S29),
  searchTab(S29,_Fax,S30),		% wordt niet gebruikt
  searchTab(S30,Banknr,S31),		% niet gebruikt
  searchTab(S31,Gironr,_),		% niet gebruikt
  roepnaam(Details, Roepnaam, Voorletters, Voornamen),
  format(NaamN, "%s,%s %s %s", Achternaam, Voornamen, Tussenvoegsels),
  trim_c(NaamN),
  determ_sex(NaamN, MV, ES, DS, Gebdatum, SexN),
  %nieuwOfBestaand(BondsNr, Skeus, SexN),
  format(Adres, "%s %s %s", Straat, Huisnr, Huisnr_Toev),
  trim_c(Adres),
  format(WoonPl, "%s %s", Postcode, Woonplaats),
  incasso(BankNr, GiroNr, Incasso),
  inc_recno(3, SexN), 
  upd_sp(sp2(0,SexN,NaamN,Telthuis,[],EnkelRating,DubbelRating,nee,0, "", Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, "",nee,"","",Incasso,"","",[])),
  !.

metEmail(String, Email) :-
  get_string(String, Email, _, 40),
  not(emailadresfout(Email)), !.
metEmail(_String, "").


tokenize_handle(_, String0) :-    % vooor zwaanshoek 
  writedevice(X),
  writedevice(screen),
  write("\n!! ", String0),
  searchchar(String0, csvconst, Pos), 
  write("\n", Pos),
  writedevice(X),
  %Pos = 4,			% test op delimited string
  concat(";",String0,String),
  %get_string(String, _SexNr, Str1, 6),
  get_string(String, M_V, Str2, 20),
  %dlg_note(M_V),
  fronttoken(M_V, _, _),
  get_string(Str2, _Voorl, Str3, 60),
  get_string(Str3, Voornaam, Str4, 50),
  get_string(Str4, Tussenvoegsel, Str5, 50),
  get_string(Str5, Naam, Str6, 50),
  get_string(Str6, Email, Str7, 50),
  get_string(Str7, _Email2, Str8, 50),
  get_string(Str8, Telhuis, Str9, 10),
  get_string(Str9, Telmob, Str10, 40),
  get_string(Str10, Gebdat, Str11, 15),
  get_string(Str11, Adres, Str12, 70),
  get_string(Str12, Huisnr, Str13, 50),
  get_string(Str13, HnrToev, Str14, 50),
  get_string(Str14, PC, Str15, 50),
  get_string(Str15, Stad, Str16, 50),
  get_string(Str16, _Land, Str17, 50),
  get_string(Str17, _Labels, Str18, 50),
  get_string(Str18, _Rollen, Str19, 50),
  get_string(Str19, _Clublidnr, Str20, 50),
  get_string(Str20, Bondsnr, Str21, 50),
  get_string(Str21, _Clublid, Str22, 50),
  get_string(Str22, _Bondslid, Str23, 50),
  get_string(Str23, _StartLid, Str24, 50),
  get_string(Str24, _Geslacht, Str25, 50),
  get_string(Str25, _Abonn, Str26, 50),
  get_string(Str26, _AbonStart, Str27, 50),
  get_string(Str27, _AbonEind, Str28, 50),
  get_string(Str28, _Extra, Str29, 50),
  get_string(Str29, StE, Str30, 50),
  get_string(Str30, StD, Str31, 50),
  get_string(Str31, _RatE, Str32, 50),
  get_string(Str32, _RatD, _RestStr1, 50),
  trim_c(Naam),
  trim_c(Adres),
  format(TotNaam, "%s,%s %s",Naam, Voornaam, Tussenvoegsel),
  %dlg_note("Naam", TotNaam),
  format(TotAdres, "%s %s %s", Adres, Huisnr, HnrToev),
  trim_c(TotAdres),
  %dlg_note(TotAdres),
  format(WoonPl, "%s %s", PC, Stad),
  trim_c(WoonPl),
  %dlg_note("tasvload regel 1991"),
  %dlg_note(M_V),
  %dlg_note(Gebdat),
  determ_sex(TotNaam, M_V, "8", "8", Gebdat, Sex),
  %StE = "",
  %StD = "",
  upd_sp(sp2(0,Sex,TotNaam,TelHuis,[],"","",nee,0, "", TotAdres, WoonPl, BondsNr,StE,StD, Gebdat, "", TelMob, Email, "",nee,"","","","","",[])), !.

tokenize_handle(_, String0) :-    % vooor vierhoeven 
  writedevice(X),
  writedevice(screen),
  %write("\n!! ", String0),
  %searchchar(String0, csvconst, Pos), 
  %write("\n", Pos),
  writedevice(X),
  %Pos = 4,			% test op delimited string
  concat(";",String0,String),
  get_string(String, ANaam, Str1, 60), !,
  get_string(Str1, Voornaam, Str2, 20),
  get_string(Str2, Tussenvoegsels, Str3, 20),
  get_string(Str3, Adres, Str4a, 50),
  get_string(Str4a, HuisNr, Str4, 50),
  get_string(Str4, Postcode, Str5, 50),
  get_string(Str5, Woonpl, Str6, 50),
  get_string(Str6, EMail, Str7, 70),
  get_string(Str7, Geb, Str8, 50),
  get_string(Str8, M_V, Str9, 20),
  fronttoken(M_V, _, _),
  get_string(Str9, LidNr, Str10, 10),
  get_string(Str10, Club, Str11, 30),
  %get_string(Str11, _Bank, Str8, 50),
  %get_string(Str12, _HetLand, Str9, 50),
  get_string(Str11, TelHuis, Str13, 50),
  get_string(Str13, TelHuis1, _Str14, 50),
  %get_string(Str15, _TelMob, Str12, 50),
  %get_string(Str16, _, Str15, 10),
  %get_string(Str17, _, Str16, 10),
  format(Naam, "%s,%s %s",ANaam, Voornaam, Tussenvoegsels),
  trim_c(Naam),
  %dlg_note("Naam", Naam),
  %dlg_note(Geb),
  format(HetAdres, "%s %s",Adres,HuisNr),
  trim_c(HetAdres),
  determ_sex(Naam, M_V, "8", "8", Geb, Sex),
  format(WnPl, "%s %s",Postcode, WoonPl),
  %TelHuis = "",
  %TelHuis1 = "",
  TelMob = "",
  upd_sp(sp2(0,Sex,Naam,TelHuis,[],"","",nee,0, Club, HetAdres, WnPl, LidNr,"","", Geb, TelHuis1, TelMob, Email, "",nee,"","","","","",[])), !.
tokenize_handle(_, String0) :-    % vooor smitshoek 
  writedevice(X),
  writedevice(screen),
  %write("\n!! ", String0),
  %searchchar(String0, csvconst, Pos), 
  %write("\n", Pos),
  writedevice(X),
  %Pos = 4,			% test op delimited string
  concat(";",String0,String),
  get_string(String, ANaam, Str1, 60),
  get_string(Str1, Voornaam, Str2, 20),
  get_string(Str2, Tussenvoegsels, Str3, 20),
  get_string(Str3, Adres, Str4, 50),
  get_string(Str4, Postcode, Str5, 50),
  get_string(Str5, Woonpl, Str6, 50),
  get_string(Str6, EMail, Str7, 70),
  get_string(Str7, Geb, Str8, 50),
  get_string(Str8, M_V, Str9, 20),
  get_string(Str9, LidNr, Str10, 10),
  get_string(Str10, Club, _Str11, 30),
  %get_string(Str11, _Bank, Str8, 50),
  %get_string(Str12, _HetLand, Str9, 50),
  %get_string(Str13, _TelHuis, Str10, 50),
  %get_string(Str14, _TelHuis1, Str11, 50),
  %get_string(Str15, _TelMob, Str12, 50),
  %get_string(Str16, _, Str15, 10),
  %get_string(Str17, _, Str16, 10),
  format(Naam, "%s,%s %s",ANaam, Voornaam, Tussenvoegsels),
  trim_c(Naam),
  
  determ_sex(Naam, M_V, "8", "8", Geb, Sex),
  format(WnPl, "%s %s",Postcode, WoonPl),
  TelHuis = "",
  TelHuis1 = "",
  TelMob = "",
  upd_sp(sp2(0,Sex,Naam,TelHuis,[],"","",nee,0, Club, Adres, WnPl, LidNr,"","", Geb, TelHuis1, TelMob, Email, "",nee,"","","","","",[])), !.
tokenize_handle(_, String0) :-    % vooor meerzicht !!!!!!!!!!!!!!!!!!
  %write("\n", String0), 
  searchchar(String0, csvconst, Pos),
  write("\n", Pos),
  Pos = 9, !,			% test op delimited string
  concat(";",String0,String),
  get_string(String, LidNr, Str1, 8),
  write("\nLidNr: ", LidNr),
  get_string(Str1, Naam, Str2, 30),
  get_string(Str2, Status, Str3, 30),
  get_string(Str3, Email, Str4, 30),
  get_string(Str4, Geslacht, _Str5, 8),
  write("\nGeslacht: ", Geslacht),
  sex(Geslacht, MV),
  spsrt(Sex, _String, MV, _Integer, _Integer1, _String1, _String2, _), !,
  upd_sp(sp2(0,Sex,Naam,"",[],"","",nee,0, "", "", "", LidNr,"","", "", "", "", Email, Status,nee,"","","","","",[])).
tokenize_handle(_, String) :-
  searchchar(String, csvconst, Pos),
  Pos < 5, !,			% test op delimited string
  frontchar(String, Char, Out3), 
  get_string(Char, Out3, Rest, "", SexS), 
  trim_c(SexS),
  spsoort_present(SexS, _SexS1, M_V, Rest, Rest2),
  get_string(Rest2, Naam, Rest3, 29),
  not(naam_fout(Naam)),
  trim_c(Naam),			% en haal meteen de blanks weg
  get_string(Rest3, Adres, Rest4, 29),
  get_string(Rest4, Woonpl, Rest5, 29),
  get_string(Rest5, Tel, Rest6, 29),
  get_string(Rest6, LidNr, Rest7, 8),
  get_string(Rest7, Club, Rest8, 5),
  get_string(Rest8, Ste, Rest9, 2),
  get_string(Rest9, Std, Rest10, 2),
  get_string(Rest10, Geb, Rest11, 10),
  metEmail(Rest11, Email), !,
  determ_sex(Naam, M_V, Ste, Std, Geb, SexN),
  upd_sp(sp2(0,SexN,Naam,Tel,[],"","",nee,0, Club, Adres, Woonpl, LidNr,Ste,Std, Geb, "", "", Email, "",nee,"","","","","",[])).
tokenize_handle(MSgWin, String) :-
  str_len(String, Len),
  Len > 1,		% zeef het eof character
  eerste_keer(MsgWin, Len, LidNrLengte),
  substring1(String, 3, 1, M_V),        trim_c(M_V),
  substring1(String, 4, 29, Naam),	trim_c(Naam),
  not(naam_fout(Naam)),
  trim_c(Naam),			% en haal meteen de blanks weg
  substring1(String, 33, 29, Adres), 	trim_c(Adres),
  substring1(String, 62, 29, Woonpl),	trim_c(Woonpl),
  substring1(String, 91, 29, Tel),	trim_c(Tel),
  substring1(String, 120, LidNrLengte, LidNr),	trim_c(LidNr),
  Of1 = 120 + LidNrLengte,
  substring1(String, Of1, 5, Club),	trim_c(Club),
  Of2 = Of1 + 5,
  substring1(String, Of2, 2, Ste),	trim_c(Ste),
  Of3 = Of2 + 2,
  substring1(String, Of3, 2, Std),	trim_c(Std),
  Of4 = Of3 + 2,
  substring1(String, Of4, 10, Geb),	trim_c(Geb),
  Of5 = Of4 + 10,
  substring1(String, Of5, 40, Email),	trim_c(Email),
  determ_sex(Naam, M_V, Ste, Std, Geb, SexN),
  upd_sp(sp2(0,SexN,Naam,Tel,[],"","",nee,0, "", Adres, Woonpl, LidNr,Ste,Std, Geb, "", "", Email, "",nee,"","","","","",[])).


spsoort_present("", "", M_V, Rest, Rest2) :-
  get_string(Rest, M_V, Rest2, 2), !.
spsoort_present(SexS, SexS, M_V, Rest, Rest2) :-
  str_int(SexS, _),
  get_string(Rest, M_V, Rest2, 3), !.
spsoort_present(M_V, "", M_V, Rest, Rest).

filter :-
  samenvoegenSpelersoorten(),
  fail.
filter :-
  Msg = "Wilt u de spelers herindelen naar spelersoort?",
  A = dlg_MessageBox( "Spelersoort", Msg, mesbox_iconQuestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
  filter1(A), !.
filter.

filter1(1) :-
  cursor_SetWait(),
  assert(reccount(0)),
  retractall(algedaan()),
  sp2(SpNo,Sex,Naam,Tel,Verh,RatingE,RatingD,Betaald,UitK, Club, Adres, Woonpl, LidNr, Sterkte, SterkD, Geb, T2, T3, EMail, Opm,Gezien,RangPosE,RangPosD,BankNr,CheckGeg,Log,Res), 
  spsrt(Sex, _, M_V,_,_,_,_,_),
  sex(M_VS, M_V),
  determ_sex(Naam, M_VS, Sterkte, SterkD, Geb, SexN),
  not(Sex = SexN),
  reccount(N),
  N1 = N + 1,
  assert(reccount(N1)),
  logf('A', sp2(SpNo,SexN,Naam,Tel,Verh,RatingE,RatingD,Betaald,UitK, Club, Adres, Woonpl, LidNr, Sterkte, SterkD, Geb, T2, T3, EMail, Opm,Gezien,RangPosE,RangPosD,BankNr,CheckGeg,Log,Res), ja),
  fail.
filter1(1) :-
  logclose(),
  reccount(N),
  format(Msg, "%d spelerrecords geconverteerd.", N),
  dlg_MessageBox( "Spelersoort", Msg, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ).

filtercheck(SpNo, Naam1) :-
  retractall(algedaan()),
  sp2(SpNo,Sex,Naam,_Tel,_Verh,_RatingE, _RatingD,_Betaald,_UitK, _Club, _Adres, _Woonpl, _LidNr, Sterkte, SterkD, Geb, _Tel1, _Tel2, _EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_BankNr,_CheckGeg,_Log,_Res), 
  spsrt(Sex, _, M_V,_,_,_,_,_),
  sex(M_VS, M_V),
  determ_sexPr(Naam, M_VS, Sterkte, SterkD, Geb, SexN),
  not(Sex = SexN),
  format(Naam1, "%s\t%s", M_VS, Naam).

determ_sexPr(Naam, M_VS, Ste, Std, Geb, SexN) :-
  determ_sex(Naam, M_VS, Ste, Std, Geb, SexN), !.
determ_sexPr(_, _M_VS, _Ste, _Std, _Geb, 101).

determ_sex(_, M_VS, Ste, _/*Std*/, Geb, SexN) :-
  spsrt(SexN, _, _, L_Low, L_Up, St_Low, St_Up, _),
  check_leeft(L_Low, L_Up, Geb),			% leeftijdscategorie OK?
  expand(St_Up, St_Up1),
  check_sterk(St_Low, St_Up1, Ste),			% sterkte OK?
  upper_lower(M_VU, M_VS),
  sex(M_VU, M_V),
  spsrt(SexN, _, M_V,_,_,_,_,_), !.
determ_sex(Naam, _M_VS, _Ste, _/*Std*/, _Geb, _SexN) :-
  not(algedaan()),
  format(Msg, "Een record valt buiten de beschikbare spelersoortcriteria!\nnaam: %s\ngeboortedatum: %s\nspeelsterkte: %s\n(er kunnen er meer zijn\nZie Personen, Kenmerken.)",Naam, _Geb, _Ste),
  dlg_MessageBox( "Persoonsgegevens", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  assert(algedaan()),
  fail.

foutboodschap(112, "Onbekende spelersoort (zie kenmerkentabel)").
foutboodschap(113, "Man/vrouw veld is blank").
foutboodschap(114, "Man/vrouwveld bevat verkeerde informatie").
foutboodschap(116, "Onbekend scheidingsteken").
foutboodschap(117, "Naam blank of < 3 tekens").
foutboodschap(118, "Te weinig velden in het record").
foutboodschap(122, "Input moet 11 velden bevatten in 'char' formaat.\nKolommen 7 en 8 mogen ook 'real' zijn.\nKolom 11 (geboortedatum) mag ook een datumveld zijn.").

spi_result(Error) :-
  logclose(),
  closefile(work),
  readdevice(keyboard),
  update_waarsch(),
  update_display(),
  update_situatie(), 
  update_uitslagen("spi_result"),
  update_planinvoer(),
  upd_formulier(),
  spelerSelRefresh(), 
  Error <> 120,
  foutboodschap(Error, Text),
  findall(N3, recno(3, _, N3), N3L), 
  som(N3L, 0, S3),
  format(Str, "Regel(record) %u\n%s", S3, Text),
  dlg_MessageBox( "Invoer afgebroken", Str, mesbox_iconerror, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.
spi_result(Err) :-
  Msg_win = msg_GetWin(),
  New_font = font_Create(ff_Fixed, [], 9),
  edit_SetFont (Msg_win, New_font),
  Err < 123,
  writeToJournaal(" Spelersoort         Nieuw Vervangen   Verwerkt"),
  spsrt(No, Naam, _, _, _, _, _, _),
  recno(1, No, N1),
  recno(2, No, N2),
  recno(3, No, N3),
  N1 + N2 + N3 > 0,
  format(Msg, " %-14.14       %4    %4         %4", Naam, N1, N2, N3),
  writeToJournaal(Msg),
  fail.
spi_result(Err) :-
  Err < 123,
  writeToJournaal("            Totaal    -------------------------"),
  findall(N1, recno(1, _, N1), N1L), 
  som(N1L, 0, S1),
  findall(N2, recno(2, _, N2), N2L), 
  som(N2L, 0, S2),
  findall(N3, recno(3, _, N3), N3L), 
  som(N3L, 0, S3),
  format(Msg," %-14.14       %4    %4         %4", "", S1, S2, S3),
  writeToJournaal(Msg),
  fail.
spi_result(_).

sex("DHR.", m) :- !.
sex("MEVR.", v) :- !.
sex("Man", m) :- !.
sex("Vrouw", v) :- !.
sex("M", m) :- !.
sex("V", v) :- !.
sex("",  m) :- !, exit(113).
sex(_,   m) :- exit(114).

bestaandSpelerNummer(Knltb) :-
  not(Knltb = ""),
  not(Knltb = "00000000"),
  not(Knltb = "00000019"),
  sp2(_,_,_,_,_,_,_,_,_,_,_,_,Knltb,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !.
 
metInhoud(_, "-", "") :- !.
metInhoud(_, I2, I2) :-
  I2 <> "", !.
metInhoud(I1, _I2, I1).
  
upd_sp(sp2(_,SexN,Naam,TelA,_V,RatingE2,RatingD2,_B,_UitK, Club, Adr, Wnpl, Knltb, StS, StD, Geb, TelB, TelC, EMail, _Opm,_Gezien,RangPosE2,RangPosD2,BankNr2,_Cx,_Lx,_Rx)) :-
  inc_recno(3, SexN),
  not(Knltb = ""),
  not(Knltb = "00000000"),
  not(Knltb = "00000019"),
  sp2(Sp,_,_Naamx,Tel1,Verh,RatingE1,RatingD1,_Betaald, UitK, ClubB, AdrB, WnplB, Knltb, StSB, StDB, GebB ,Tel2,Tel3,EMailA,Opm,_G,RangPosE1,RangPosD1,BankNr1,CheckGeg,Log,Res),  
  metinhoud(TelA, Tel1, Tel1O),
  metinhoud(TelB, Tel2, Tel2O),
  metinhoud(TelC, Tel3, Tel3O),
  metinhoud(ClubB, Club, ClubO),
  metinhoud(AdrB, Adr, AdrO),
  metinhoud(WnplB, Wnpl, WnplO),
  metinhoud(StSB, StS, StSO),
  metinhoud(StDB, StD, StDO),
  metinhoud(GebB, Geb, GebO),
  metinhoud(EMailA, EMail, EMail1),
  metinhoud(RangPosE1, RangPosE2, RangPosE),
  metinhoud(RangPosD1, RangPosD2, RangPosD),
  metinhoud(RatingE1, RatingE2, RatingE),
  metinhoud(RatingD1, RatingD2, RatingD),
  metinhoud(BankNr1, BankNr2, BankNr),
%  ratingInh(RatingB, Rating, RatingO),
  upd_sp1(sp2(Sp,SexN,Naam,Tel1O,Verh,RatingE,RatingD,nee,UitK, ClubO, AdrO, WnplO, Knltb, StSO, StDO, GebO, Tel2O, Tel3O, EMail1, Opm,nee,RangPosE,RangPosD,BankNr,CheckGeg,Log,Res)), !.
upd_sp(sp2(_,SexN,Naam,TelA,_V,RatingE2,RatingD2,_By,_UitKy, Club, Adr, Wnpl, Knltb, StS, StD, Geb, TelB, TelC, EMailA, Opm,_Gezien,RangPosE2,RangPosD2,BankNr2,_Cx,_Lx,_)) :-	% bestaande naam, geen bestaand nummer
  not(bestaandSpelerNummer(Knltb)),
  sp2(SpNo,_,Naam,Tel1,Verh,RatingE1,RatingD1,_Betaald,UitK, ClubB, AdrB, WnplB, KnltbB, StSB, StDB, GebB,Tel2,Tel3,EMail,Opm,_Gezien,RangPosE1,RangPosD1,BankNr1,CheckGeg,Log,Res),  
  metinhoud(TelA, Tel1, Tel1O),
  metinhoud(TelB, Tel2, Tel2O),
  metinhoud(TelC, Tel3, Tel3O),
  metinhoud(ClubB, Club, ClubO),
  metinhoud(AdrB, Adr, AdrO),
  metinhoud(WnplB, Wnpl, WnplO),
  metinhoud(StSB, StS, StSO),
  metinhoud(StDB, StD, StDO),
  metinhoud(GebB, Geb, GebO),
  metinhoud(KnltbB, Knltb, KnltbO),
  metinhoud(EMailA, EMail, EMail1),
  metinhoud(RangPosE1, RangPosE2, RangPosE),
  metinhoud(RangPosD1, RangPosD2, RangPosD),
  metinhoud(RatingE1, RatingE2, RatingE),
  metinhoud(RatingD1, RatingD2, RatingD),
  metinhoud(BankNr1, BankNr2, BankNr),
%  ratingInh(RatingB, Rating, RatingO),
  upd_sp1(sp2(SpNo,SexN,Naam,Tel1O,Verh,RatingE,RatingD,nee,UitK, ClubO, AdrO,WnplO,KnltbO, StSO, StDO, GebO, Tel2O, Tel3O, EMail1, Opm,nee,RangPosE,RangPosD,BankNr,CheckGeg,Log,Res)), !.
upd_sp(sp2(_,Sex,Naam,Tel1,Verh,RatingE,RatingD,Betaald,UitK, Club, Adr, Wnpl, Knltb, StS, StD, Geb, Tel2, Tel3, EMail, Opm,Gezien,RangPosE,RangPosD,BankNr,CheckGeg,Log,Res)) :-	% bestaande naam, geen bestaand nummer
  getbacktrack(Here),
  %loglock("upd_sp"),
  repeat,
  random(30000,Y),       /* bepaal vrij nummer             */
  SpNo = Y + 1,
  not(sp2(SpNo,_,_,_,_,_,_,_,_,_,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_,_,_)),
  cutbacktrack(Here),
  logf('A',sp2(SpNo,Sex,Naam,Tel1,Verh,RatingE,RatingD,Betaald,UitK, Club, Adr, Wnpl, Knltb, StS, StD, Geb,Tel2,Tel3,EMail,Opm,Gezien,RangPosE,RangPosD,BankNr,CheckGeg,Log,Res),ja),
  inc_recno(1, Sex), !.
upd_sp(sp2(_,_Sex,Naam,_Tel1,_Verh,_RatingE,_RatingD,_Betaald,_UitK, _Club, _Adr, _Wnpl, Knltb, _StS, _StD, _Geb, _Tel2, _Tel3, _EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_BankNr,_CheckGeg,_Log,_Res)) :-
  format(Str, "Gegevens van %,\nKNLTB nummer %s",Naam, Knltb),
  dlg_MessageBox( "Niet bewaard", Str, mesbox_iconerror, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), !.

upd_sp1(sp2(SpNo,SexN,Naam,Tel,Verh,RatingE,RatingD,Betaald,UitK, Club, Adr, Wnpl, Knltb, StS, StD, Geb,Tel1,Tel2,EMail,Opm,Gezien,RangPosE,RangPosD,BankNr,CheckGeg,Log,Res)) :-
  not(sp2(SpNo,SexN,Naam,Tel,Verh,RatingE,RatingD,Betaald,UitK, Club, Adr, Wnpl, Knltb, StS, StD, Geb,Tel1,Tel2,EMail,Opm,Gezien,RangPosE,RangPosD,BankNr,CheckGeg,Log,Res)), 
  logf('A',sp2(SpNo,SexN,Naam,Tel,Verh,RatingE,RatingD, Betaald,UitK, Club, Adr, Wnpl, Knltb, StS, StD, Geb,Tel1,Tel2,EMail,Opm,Gezien,RangPosE,RangPosD,BankNr,CheckGeg,Log,Res),ja), 
  inc_recno(2, SexN), !.
upd_sp1(sp2(_,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)).  

naam_fout("") :- !, exit(117).
naam_fout(Naam) :-
  str_len(Naam, Len), Len < 2, exit(117).

add_gebjr(Jr, Jr1) :-
  str_len(Jr, Len), 
  Len < 2, !,
  concat("200", Jr, Jr1).	% vb 2 --> 2002 etc.
add_gebjr(Jr, Jr1) :-
  str_len(Jr, Len), 
  Len < 3,
  str_int(Jr, Jaar),
  Jaar <= 10, !,		% vb 19 --> 2019 etc.
  concat("20", Jr, Jr1).
add_gebjr(Jr, Jr1) :-
  str_len(Jr, Len), 
  Len < 3,
  str_int(Jr, Jaar),
  Jaar > 10, !,			% vb 49 --> 1949 etc.
  concat("19", Jr, Jr1).
add_gebjr(Jr, Jr) :-
  str_len(Jr, Len),
  Len = 4, !.

leeftijdP(Geb, Leeft) :-
  leeftijd(Geb, Leeft,0, _), !.
leeftijdP(_, 0).

leeftijdComp(_, groen, _Lin, 0) :- !.
leeftijdComp(Cat, _, Lin, Luit) :-
  wc(Cat, _, _, _Naam, _, _CatShort, _, _, leeftijd(_, Vanaf), leeftijd(_,LeeftTM), _, _, _, Tid, _),
  toernooiK(Tid,jrt),
  toernooiWeekJaar(_, Wk, _),
  Wk > 42,
  Vanaf > 6,
  LeeftTM < 18,
  Luit = Lin + 1, !.
leeftijdComp(_Cat, _, Lin, Lin).

leeftijd(Geb, LeeftU, Cat, Pasp) :-
  splitGeb(Geb, _, _, Pasp),
  lasttoken(Geb, _, Token),
  str_len(Token, Len),
  Len <= 4,			% formaat 01-01-1988 
  add_gebjr(Token, GebT),	% maak 1982 van 82, 2001 van 1, etc.
  %geboortemaand(Rest, GebMnd),
  str_int(GebT, GebJ),
  toernooiweekjaar(_, _, Year),
  Leeft = Year - GebJ,
  leeftijdComp(Cat, Pasp, Leeft, LeeftU), !.
leeftijd(Geb, Leeft, _, Pasp) :-
  splitGeb(Geb, _, _, Pasp),
  lasttoken(Geb, _, Token),
  str_len(Token, Len),
  Len > 4,  % neem de achterste 4 tekens
  Begin = Len - 4,
  frontstr(Begin,Token,_StartString,TokenT),
  str_int(TokenT, GebJ),
  toernooiweekjaar(_, _, Year),
  Leeft = Year - GebJ, !.
leeftijd(Geb, 0, _, Pasp) :- 
  splitGeb(Geb, _, _, Pasp), !.

check_leeft(_, _, "") :- !.  % niet checken als gebdatum blanco
check_leeft(0, 0, _) :- !.
check_leeft(L_Low, L_Up, Geb) :-
  leeftijd(Geb, Leeft, 0, _),
  Leeft >= L_Low,
  Leeft <= L_Up, !.

expand("", "") :- !.
expand(In, Uit) :-
  str_len(In, Len),
  Len < 2,
  concat(In, "z", Uit), !.
expand(In, In).

check_sterk(_, _, "") :- !.
check_sterk("", "", _) :- !.
check_sterk("", St_Up, St) :-
  St >= St_Up, !.
check_sterk(St_Low, "", St) :-
  St <= St_Low, !.
check_sterk(St_Low, St_Up, St) :-
  St <= St_Low,
  St >= St_Up.

lasttoken(In, Body, Out) :-
  lasttoken1(In, "", Body, Out).

lasttoken1(In, Body, Body, Out) :-
  fronttoken(In, Out, Rest),
  not(fronttoken(Rest, _, _)), !.
lasttoken1(In, Bin, Bout, Out) :-
  fronttoken(In, Tok, Rest),
  concat(Bin, Tok, Bin1), 
  lasttoken1(Rest, Bin1, Bout, Out).

%______________________Clubs in ________________________________________%

database - temp2
single recnum(integer)
inetBestand(string, string ToernooiNummer, string ToernooiDatum, string Map, string ToernooiNaam, string DatDatum, string LogDatum, string Licentie)
single metToernooinaam(string TNaam, slist TNum, string TDatum, string IID, integer Toernooidag)
single metSp2(boolean)
single metOpg(boolean)
single metWd(boolean)
single metW2(boolean)
single metW3(boolean)
single metPos(boolean)

predicates
  clubs_in1(WINDOW, string)
  clubs_in2(string Extension)
  tokenize_handle_cl(string)
  list_cl(Dbase3Rec)
  check_fieldsCl(FldDescL)
  cli_result(integer)
determ  clubcode_blanco(string)
  %filerepr(string, string)
  %leeserror()
  %
determ  getToernooiNaam(boolean Verbose, string FileNaam, string TNaam, slist TNum, string TDatum, string IID, integer Toernooidag)

determ checkBuLine(string, string)
procedure twodigits(string, string)
procedure file_decompressIf(string FIn, string Fout)
nondeterm redenfout(string)
determ datumRead(string TDatum1, string TDatum, integer Toernooidag)


clauses

recnum(0).
metToernooiNaam("back-up niet leesbaar", [], "", "", 0).
metSp2(b_False).
metOpg(b_False).
metWd(b_False).
metW2(b_False).
metW3(b_False).
metPos(b_False).

twodigits(Str, Out) :-
  str_len(Str, Len),
  Len = 1,
  concat( "0", Str, Out), !.
twodigits(Out, Out).

file_decompressIf(FIn, Fout) :-
  filenameext(FIn, _, Ext),
  upper_lower(Ext, ".gz"),
  get_tempdir(_, Fout),
  file_decompress(FIn, Fout), !.
file_decompressIf(FIn, FIn).

datumRead(TDatum1, TDatum, Toernooidag) :-
  fronttoken(TDatum1, Dag1, R1),
  twodigits(Dag1, DagS),
  frontchar(R1, _, R2),
  fronttoken(R2, Maand1, R3),
  twodigits(Maand1, MaandS),
  frontchar(R3, _, JaarS),
  format(TDatum, "%s-%s-%s", JaarS, MaandS, DagS),
  str_int(JaarS, Jaar),
  str_int(MaandS, Maand),
  str_int(DagS, Dag),
  dt_date_to_offset(Jaar, Maand, Dag, Off1),
  date(Year,Month,Day),
  dt_date_to_offset(Year, Month, Day, Off2),
  Toernooidag = Off2 - Off1, !.

checkBuLine(Line, "naam") :- 
  trap(term_str(dbasedom, Term, Line), _, fail),
  Term = naam(TNaam, TNum, TDatum1,InetId,_,_),
  datumRead(TDatum1, TDatum, Toernooidag), !,
  assert(metToernooinaam(TNaam, TNum, TDatum, InetId, Toernooidag)).
checkBuLine(Line, "naam") :- 
  trap(term_str(puin, Term, Line), _, fail),
  Term = naam(TNaam, TNum1, TDatum1,InetId,_,_),
  datumRead(TDatum1, TDatum, Toernooidag), !,
  assert(metToernooinaam(TNaam, [TNum1], TDatum, InetId, Toernooidag)).
checkBuLine(_Line, "sp2") :- !,
  assert(metSp2(b_True)).
checkBuLine(_Line, "opg") :- !,
  assert(metOpg(b_True)).
checkBuLine(_Line, "wd") :- !,
  assert(metWd(b_True)).
checkBuLine(_Line, "w2") :- !,
  assert(metW2(b_True)).
checkBuLine(_Line, "w3") :- !,
  assert(metW3(b_True)).
checkBuLine(_Line, "pos") :- !,
  assert(metPos(b_True)).

redenfout("niet sp wel opg") :-
  metSp2(b_False),
  metOpg(b_True).
redenfout("niet opg wel wd") :-
  metOpg(b_False),
  metWd(b_True).
redenfout("niet wd wel w2") :-
  metWd(b_False),
  metW2(b_True).
redenfout("niet wd wel w3") :-
  metWd(b_False),
  metW3(b_True).
redenfout("wel wd niet pos") :-
  metWd(b_True),
  metPos(b_False).
  
getToernooiNaam(_, Fnam1, _, _, _, _, _) :-
  assert(metToernooiNaam("back-up beschadigd!", [], "", "", 0)),
  assert(metSp2(b_False)),
  assert(metOpg(b_False)),
  assert(metWd(b_False)),
  assert(metW2(b_False)),
  assert(metW3(b_False)),
  assert(metPos(b_False)),
  file_decompressIf(Fnam1, Fnam2),  % als gz bestand
  myOpenread(0, work, Fnam2),
  readdevice(work),
  repeat_f(work),
  readln(Line),
  fronttoken(Line, Token, _),
  checkBuLine(Line , Token),
  fail.
getToernooiNaam(Verbose, _, _, _, _, _, _) :-	% 16.6.2012 verbose
  closefile(work),
  readdevice(keyboard),
  Verbose = b_True,
  findall(R, redenfout(R), RList),
  not(RList = []),
  list_to_string(RList, "\n", Reden),
  format(Ms, "De back-up is beschadigd!\nToch laden?\n\n(%s)", Reden),
  2 = dlg_MessageBox( "Back-ups lezen", Ms, mesbox_iconexclamation, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ), !,
  fail.  
getToernooiNaam(_, _,TNaam, TNum, TDatum, InetId, Toernooidag)  :-
  metToerNooiNaam(TNaam, TNum, TDatum, InetId, Toernooidag).

/*leeserror() :-
  _A = dlg_MessageBox( "Back-ups lezen", "Fout bij het lezen van de back-ups\nDiskette onleesbaar?", mesbox_iconerror, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.
*/
clubs_invoer(Win) :-
  getfolder(Dir,_,_,_),
	%get_toernooidir(TDir, _, _, _),
	filenamepath(Invoer, Dir, "Club.exp"),
	%concat(TDir, "Club.exp", Invoer),
  Fn = dlgFileNameZeker(Invoer,["exp","*.exp","dat","*.dat","txt","*.txt","dbf","*.dbf"],"Selecteer invoerbestand",[],Dir,_OutListFiles),
  trap(clubs_in1(Win, Fn), Ret, cli_result(Ret)), !.
clubs_invoer(_Win).

clubs_in1(_Win, File) :-
  assert(recnum(0)),
  myOpenread(0, work, File), 
  filenamepath(File, _Path, Name),
  filenameext(Name,_, Ext), 	% determine filetype
  clubs_in2(Ext), !.
clubs_in1(_, _) :-
  logclose(),
  closefile(work),
  readdevice(keyboard),
  exit(120), !.

clubs_in2("DBF") :-
  filemode(work,0), readdevice(work),
  init_dbase3(TotRecs,FldNameL,FldDescL),
  write('\n', FldNameL, FldDescL), 
  check_fieldsCl(FldDescL),
  rd_Dbase3Rec(TotRecs,work,FldDescL,Rec),
  list_cl(Rec),
  fail.
clubs_in2(Ext) :-
  Ext <> "DBF",
  repeat_f(work), 
  readdevice(work), 
  readln(String), % write(String),
  readdevice(keyboard),
  tokenize_handle_cl(String),
  fail.
clubs_in2(_) :-
  logclose(),
  fail.

clubcode_blanco("") :- exit(117), !.

tokenize_handle_cl(String) :-
  searchchar(String, csvconst, Pos),
  Pos < 10, !,			% test op delimited string
  skip_blank(String, S2), 
  frontchar(S2, Char, S3), !,
  get_string(Char, S3, S4, "", ClubI),
  substring1(ClubI, 1, 5, Club),
  trim_c(Club),
  not(clubcode_blanco(Club)),
  get_string(S4, Naam, S5, 23),
  get_string(S5, Distr, _, 23),
  recnum(N),
  N1 = N + 1,
  assert(recnum(N1)),
  logf('A', club(Club, Naam, Distr),ja).
tokenize_handle_cl(String) :-
  substring1(String, 1, 5, Club),	trim_c(Club),
  not(clubcode_blanco(Club)),
  Club <> "\26",
  substring1(String, 6, 23, Naam),      trim_c(Naam),
  substring1(String, 29, 23, Distr),	trim_c(Distr),
  recnum(N),
  N1 = N + 1,
  assert(recnum(N1)),
  logf('A', club(Club, Naam, Distr),ja).

cli_result(_Error) :-
  logclose(),
  closefile(work),
  readdevice(keyboard),
  fail.
cli_result(121) :-
  dlg_MessageBox( "Clubinvoer", "dBase def error: min 3 kolommen, char formaat.\nInvoer gestopt.", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.
cli_result(117) :-
  dlg_MessageBox( "Clubinvoer", "Club met blanco code.\nInvoer gestopt.", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.
cli_result(Err) :-
  Err < 131,
  recnum(N1),
  format(Msg, " Verwerkt: %u records", N1),
  dlg_MessageBox( "Clubinvoer", Msg, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.

list_cl([char(Club),char(Naam),char(Dist)|_]) :- 
  Club <> "",
  substring1(Club, 1, 5, Club1), trim_c(Club1),
  substring1(Naam, 1, 23, Naam1), trim_c(Naam1),
  substring1(Dist, 1, 23, Dist1), trim_c(Dist1),
  not(club(Club1, Naam1, Dist1)),
  recnum(N),
  N1 = N + 1,
  assert(recnum(N1)),
  %write('\n', Club1, ' ', Naam1, ' ', Dist1),
  logf('A', club(Club1, Naam1, Dist1),ja).

check_fieldsCl([F1,F2,F3|_]) :-
	F1 = flddesc(ch, _),
	F2 = flddesc(ch, _),
	F3 = flddesc(ch, _), !.
check_fieldsCl(_) :-
 	exit(121).



%BEGIN_DLG BackupSelect
/**************************************************************************
	Creation and event handling for dialog: BackupSelect
**************************************************************************/

constants

%BEGIN BackupSelect, CreateParms, 13:36:43-12.10.2016, Code automatically updated!
  dlg_backupselect_ResID = idd_backupselect
  dlg_backupselect_DlgType = wd_Modal
  dlg_backupselect_Help = idh_selecteer_backup1
%END BackupSelect, CreateParms

predicates

  dlg_backupselect_eh : EHANDLER
  dlg_backupselect_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_backupselect_update(DIALOG_VAL_LIST)
  checkBU(string, string Toernooinaam) - (i, o)
  %checkBU1(string Bestand, string Inhoud, string Toernaam)
  %procedure visToernooiSelectie(string FileNaam)
  procedure visBackupSelectieInternet(string FileNaam)
  procedure visBackupSelectieDisk()
  determ reportErr(string, string)
  determ backupVan(window, string Nr)
  myOwnerdrawB(window Parent, window LboxWin, integer ItemId, RCT RectItem, COLOR Voorgrond, COLOR Achtergrond)
  procedure convertwijze(uploadtype, string)
  nondeterm wildcards(string, string Pad, string Type)
  selback(string, integer ForeGround, integer BackGround)
  procedure vervangdat1(lokatie, string FF, string FN, string ToernooinaamNieuw) - (i,i,o,i)
  determ nietgevonden(integer, string, string)
  determ tegrootomtedelen()
  determ ifGZ(string In, string Ext, string Uit)
  procedure naamTo(string Line, string TNaam, string TDatum, string IID)
  %determ vervanggevallen(string, integer)
  procedure vervangVraag(lokatie, string ToernooinaamNieuw, integer Ans) - (i,i,o)
  %procedure selectjson(string)

clauses

wildcards(Wild, D, "disk") :-
  getfolder(_, _, _,D),
  %concat(F, "\\Toernooien\\Backups", D),
  concat(D, "\\backup(*.tas", Wild).
wildcards(Wild, D, "zonline") :-
  getfolder(_,_,_, D),
  concat(D, "\\*.gz", Wild).

checkBU(OSFileName, TNaam) :-
  existfile(OSFileName),
  getToernooiNaam(b_True, OSFileName, TNaam, _TNum, _TDatum, _IID, _Toernooidag), !.
checkBU(OSFileName, "TV Hoorn1") :-
  %not(existfile(OSFileName)),
  format(MSg, "BU Niet in orde\nDoorgaan?\n\n(bestandsnaam: %s)", OSFileName),
  1 = dlg_MessageBox( "Back-up", Msg, mesbox_iconerror, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ).
  
visBackupSelectieInternet(FullName) :-	% 25.12.2010 backupenzo.php
  openread(work,FullName),
  readdevice(work),
  repeat_f(work),
  readln(Line),
  trap(term_str(temp2, Fact, Line), T, write("\n### error! ", T, " : ", Line)), 
  %inetBestand(string, string ToernooiNummer, string ToernooiDatum, string Map, string ToernooiNaam, string DatDatum, string LogDatum, string Licentie)
  %Fact = inetBestand(Moment, BUType, TDag, TNaam, Toernooinaam, TDatum, Tnummer, BUType),
  %concat(Moment, BUType, Key),
  %assert(inetBestand(Key, BUType, TDag, TNaam, TStamp, TDatum, Tnummer, Map),temp2),
  Fact = inetBestand(Moment, Map, TDag, TNaam, TStamp, TDatum, Tnummer, BUType),
  concat(Moment, BUType, Key),
  assert(inetBestand(Key, BUType, TDag, TNaam, TStamp, TDatum, Tnummer, Map),temp2),
  fail.
visBackupSelectieInternet(_) :-
  closefile(work).

visBackupSelectieDisk()  :-
  wildcards(Wild, Dir, Type),
  trap(dirfiles(Wild, 0x40, Fnam, _, Hour, Min, Sec, Year, Month, Day, _Size), Ret, dir_error(Ret, b_False)),
  filenamepath(FNam1, Dir, Fnam),
  helestr_int(0, MinS, Min),
  helestr_int(0, HourS, Hour),
  trap(getToernooiNaam(b_False, FNam1, TNaam, _TNum, TDatum, IID, Toernooidag), _, fail),
  str_int(TDag, Toernooidag),
  helestr_int(0, MonthS, Month),
  helestr_int(0, DayS, Day),
  helestr_int(0, SecS, Sec),
  format(TStamp, "%s-%s-%u %s:%s", DayS, MonthS, Year, HourS, MinS),  
  format(Key, "%u-%s-%s %s:%s:%s%s",Year, MonthS, DayS, HourS, MinS, SecS,Type),
  assert(inetBestand(Key, Type, TDag, TNaam, TStamp, TDatum, IID, Fnam1)),
  %comline(LineBuffer),
  %upper_lower(LineBuffer, LBL),
  %searchstring(LBL, "/x", _),
  %write("\n ### Dbackup: ", Key, " ", Fnam1),
  fail.
visBackupSelectieDisk().

convertWijze(update, "update").
convertwijze(replace, "replace").

teGrootOmtedelen() :-
  findall(SpNo, sp2(SpNo,_,_,_,_,_,_,_,_,_,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_,_,_), SpList),
  count(SpList, 0, Aantal),
  Aantal > 5000, !,
  dlg_MessageBox( "Te groot", "Het bestand is eigenlijk te groot om te delen (meer dan 5000 personen).\nWis personen die niet meedoen!", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), !.

selectJson("tabun1plusjson.php") :-
  comline(LineBuffer),
  upper_lower(LineBuffer, LBL),
  searchstring(LBL, "/json", _), !.
selectJson("tabun1.php").

copyNaarWebIf(_Map, inet, _UpdateReplace) :-
  teGrootOmtedelen(),
  fail.
copyNaarWebIf(_Map, inet, UpdateReplace) :-        % alleen voor hele toernooibestanden
  convertWijze(UpdateReplace, UpdateReplaceS),
  webdir(DirO, inet, TB),
  %get_Toernooidir(_TM, DirO, _, TB),
  get_tempDir(TDir, _),
  filenamePath(TempFile, TDir, "feedback.txt"),
  retractall(webdatpositie(_, _, _)),
  %filenamepath(TMdat, TM, fctconst),
  AList = aanmeldenvars(),
  AAList = ["reden", "compr", "map", DirO , "func", "uploadtoernooi", "wijze", UpdateReplaceS | AList],
  selectjson(TaBun),
  providerX(_, _, TaBun, ProvM),
  write("\n#### ", ProvM),
  filenamepath(TB, _, Namem),
  getfolder( _, _, Fol, _),
  format(TB1,"%s\\%s.gz", Fol, Namem),
  file_compress(TB, TB1),
  %format(NMsg, "%s\n%s\n%s", TB,TB1), %+++++++++++++++++++++++
  %dlg_note(NMsg),
  Return = fileupload(ProvM, TB1, TempFile, AAList),   
  checkupload(Return),
  file_str(TempFile, Str),
  write(Str),
  trap(term_str(dbasedom, Term, Str), _, reportErr(Str, UpdateReplaceS)),
  retractall(webdatpositie(_, _, _)),
  assert(Term),   !.
copyNaarWebIf(_Map, _Inet, _).

reportErr(Str, Wijze) :-
  format(Str1, "Upload van het bestand is mislukt.\n%s\n%s.", Wijze, Str),
  writeToJournaal(Str1, ja),
  fail.

reservekopie(1) :-
  getfolder(_, _, _, Backups),
  naam(ToernooiNaam, _, _,_,_,_),
  cleantoernooinaam(Toernooinaam, ToernooinaamC),
  date(_Year,Month,Day),
  helestr_int(0,MonthS,Month), helestr_int(0,DayS, Day),
  %concat(Folder, "\\Toernooien\\Backups",Pad),
  getbacktrack(Here),
  numb(Num),
    helestr_int(0,NumS, Num),
    format(BNaam, "backup(%s-%s %s) %s.tas", MonthS, DayS,NumS, ToernooiNaamC),
    filenamepath(TBU, Backups, BNaam),
  not(existfile(TBU)), 
  cutbacktrack(Here),
  format(Msg, "Reservekopie maken mislukt!?\n%s\n%s", Backups, TBU),
  trap(save(TBU), _, dlg_Error(Msg)),
  format(Msg1, "Reservekopie %s gemaakt.", TBU),
  writetojournaal(Msg1, ja), !.
reserveKopie(_).

vervangVraag(disk, ToernooinaamNieuw,Ans) :- 
  naam(ToernooiNaamOud, _, _,_,_,_),
  ToernooiNaamOud = ToernooinaamNieuw, 
  format(Msg, "Eerst even een extra back-up maken van de huidige gegevens?\nDaarna worden de nieuwe gegevens geladen."),
  Ans = dlg_MessageBox( "Gegevens vervangen", Msg, mesbox_iconquestion, mesbox_buttonsyesnocancel, mesbox_defaultthird, mesbox_suspendapplication ),
  cursor_SetWait(),
  %toernooidir(OudPad, OudeBestand),
  %filenamepath(FullNieuw, OudPad, FF),
  reserveKopie(Ans), !.
vervangVraag(_, _ToernooinaamNieuw,1).

vervangdat1(disk, FF, FullNieuw, _) :-
  %concat(Folder, "\\Toernooien\\",Pad),
  cursor_SetWait(),
  toernooidir(OudPad, _OudeBestand),
  filenamepath(FullNieuw, OudPad, FF),
  %alsOverschrijven(OudPad, Oudebestand, FullNieuw),
  assert(toernooidir(OudPad, FullNieuw)), !.
vervangdat1(inet, FF, FF1, _) :-
  filenamepath(FF, _, Name),
  concat("gedeeld ", Name, Name1),
  % getFolder(_, _, DeelMap, _),
  get_Tempdir(DeelMap, _),  % !!!!!!!!!	% 21.2.2013
  filenamepath(FF1, DeelMap, Name1),
  cursor_SetWait(),
  webdir(PadXX, _, _OudeBestand),
  assert(webdir(PadXX, inet, FF1)), !.
vervangdat1(_, _, "", _).

ifGZ(Item, ".gz", FF) :-
  get_TempDir(_, Unpacked),
  file_decompress(Item, Unpacked),
  checkBU(Unpacked, ToernooiNaamNieuw),	% check de kwaliteit van het bestand
  format(Msg, "Het GZ bestand bevat gegevens van het %s,\nDoorgaan?", ToernooinaamNieuw),
  1 = dlg_MessageBox( "Toernooibestand", Msg, mesbox_iconquestion, mesbox_buttonsokcancel, mesbox_defaultfirst, mesbox_suspendapplication ),
  getfolder(_Folder, _Toern,_,Backups),
  cleantoernooinaam(ToernooinaamNieuw, ToernooinaamNC),
  concat("vanGZ ", ToernooinaamNC, TNC),	% 27.5.2012
  filenameext(FF1, TNC, ".tas"),
  filenamePath(FF, Backups, FF1),
  mydeletefile(FF),
  renamefile(Unpacked,FF), !.
ifGZ(Item, Ext, Item) :-
  not(upper_lower(Ext, ".gz")).

vervangDat(_Win, FullName) :-
  %not(upper_lower(Ext, ".gz")),
  close_alles(),
  %file_str(Fullname, Str),
  %write(Str),
  checkBU(FullName, ToernooiNaamNieuw),	% check de kwaliteit van het bestand
  cleantoernooinaam(ToernooinaamNieuw, ToernooinaamNC),
  filenameext(FF, ToernooiNaamNC, ".tas"),
  webdir(_, Lok, _),
  vervangVraag(Lok, ToernooinaamNieuw,Ans),
  Ans <> 3, 
  stopDBrefresh(),
  vervangdat1(Lok, FF, FullNieuw, ToernooinaamNieuw),
  copyTextFile(FullName, FullNieuw),
  %loglock(lees, "vervangDat"),			% voorkomt ook interrupts
  %compress(replace),
  writedevice(screen),
  %win_Destroy(_Win),	% 22.12.2010
  TaskWin = vpi_GetTaskWin(),
  retractall(logIsOpen(_)), 
  %logclose(),
  del_logX(),
  retractall(alleenlezen1),
  dlg_toernooiload_Create(TaskWin, "Vervangdat", b_True, replace), !.  	% 28.5.2012 b_True = uploadto follow
vervangDat(_Win, _FullName) :-
  startdbrefresh().

nietgevonden(404, Map, Timestamp) :-
  format(Msg, "back-up niet meer aanwezig????\n%s\n%s", Map, Timestamp), 
  _Answer = dlg_MessageBox( "Bak-up", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  !, fail.
nietgevonden(_, _, _). 

backupVan(_Win, Nr) :-
  inetBestand(Nr, "disk", _TDag, _TNaam, _TStamp, _TDatum, _Tnummer, FileFF),
  %dos_padPC(Path1),
  vervangDat(_Win, FileFF), 
  %win_Destroy(_Win),	% 22.12.2010
  !.
backupVan(_Win, Nr) :-  	% 28.5.2012  deze werkt niet!
  inetBestand(Nr, "zonline", _TDag, _TNaam, _TStamp, _TDatum, _Tnummer, FileFF),
  %dos_padPC(Path1),
  %filenameext(FileFF, _, Ext),
  concat(FileFF,".gz", FF),
  vervangDat(_Win, FF),
  %win_Destroy(_Win),	% 22.12.2010
  !.
backupVan(_Win, Nr) :-
  inetBestand(Nr, "inet", _TDag, _TNaam, _T1, _TDatum, _Tnummer, _FileFF),
  providerX(Prov, _, "", _),
  cursor_SetWait(),
  frontstr(19, Nr, EchteTimestamp, _),
  VarList = aanmeldenVars(),
  VarList1 = ["step", "haalop", "T1", Echtetimestamp | VarList],
  format(URL, "http://%s/backupenzo.php", Prov),
  get_tempdir(_, Fullname),
  %_Return = getbackup(URL, FullName, VarList1),
  mydeleteFile(Fullname),
  _Return = downloadData(URL, FullName, VarList1,_NotFound,0),
  %file_str(FullName, StrX),
  %write("\n", StrX),
  vervangDat(_Win, FullName),	% 24.3.2011
  %win_Destroy(_Win),	% 22.12.2010
  !.
backupVan(_Win, Nr) :-
  %write("\n #### backup van ", Nr),
  inetBestand(Nr, "online", _TDag, _TNaam, _Timestamp, _TDatum, _Tnummer, _FileFF), !,
  frontstr(19, Nr, EchteTimestamp, _),
  providerX(_, _, "tabun1.php", ProvO),
  VarList = aanmeldenVars(),
  webdir(Map, _, _),
  cursor_SetWait(),
  VarList1 = ["func", "getbackup", "map", Map, "timestamp", EchteTimestamp | VarList],
  get_tempdir(_, FullName),
  mydeletefile(FullName),
  0 = downloadData(ProvO, FullName, VarList1, NotFound, 1),	% metprogress
  nietgevonden(NotFound, Map, EchteTimestamp),
  vervangDat(_Win, FullName), !.
backupVan(_Win, Nr) :-
  inetBestand(Nr, "archief", _TDag, _TNaam, _Timestamp, _TDatum, _Tnummer, Token), !,
  providerX(_, _, "tabun1.php", ProvO),
  VarList = aanmeldenVars(),
  VarList1 = ["func", "getarchief", "token", Token | VarList],
  %write("\n", VarList1),
  cursor_SetWait(),
  get_tempdir(_, FullName),
  mydeletefile(FullName),
  0 = downloadData(ProvO, FullName, VarList1, NotFound, 0),	% metprogress
  nietgevonden(NotFound, "", ""),
  vervangDat(_Win, FullName).
  %win_Destroy(_Win).	% 22.12.2010

selback("disk", color_Black, color_Yellow) :- !.
selback("inet", color_White, color_Blue) :-!.
selback("online", color_Black, color_Green) :-!.
selback("zonline", color_Black, color_Cyan) :-!.
selback(_,  color_White, color_Blue).

myOwnerdrawB(_Win, LboxWin, ItemId, RectItem, Voorgrond, Achtergrond) :-
  %lbox_SetTabStops(LboxWin, [80, 160, 240,320, 400],),
  win_SetForeColor(LboxWin, Voorgrond),
  win_SetBackColor(LboxWin, Achtergrond),
  win_SetPen(LboxWin,pen(1,ps_Solid,Achtergrond)),
  draw_Rect(LboxWin, RectItem),
  RectItem = rct(_L, T, R, B),
  Item = lbox_GetItem(LboxWin, ItemId),	% get the key
  %str_int(Item, Nr),
  inetBestand(Item, MapType, TDag, TNaam, TStamp, TDatum, Tnummer, _FF),
  %frontstr(16, TStamp, TStamp1, _),
  %format(Line, "%s %s%3.3s %s %s", TDatum, TStamp, TDag, TNaam, Tnummer),
  %draw_TextInRect(LboxWin,rct(0, T, 80, B), TDatum, -1, [dtext_left]),
  draw_TextInRect(LboxWin,rct(2, T, 115, B), TStamp, 16, [dtext_left]),
  win_SetBackColor(LboxWin, color_LtGray),
  draw_TextInRect(LboxWin,rct(122, T, 153, B), TDag, -1, [dtext_center]),
  win_SetBackColor(LboxWin, Achtergrond),
  %draw_TextInRect(LboxWin,rct(160, T, 500, B), TNaam, -1, [dtext_center]),
  draw_TextInRect(LboxWin,rct(160, T, 570, B), TNaam, -1, [dtext_center]),
  win_SetBackColor(LboxWin, color_LtGray),
  %draw_TextInRect(LboxWin,rct(502, T, 582, B), TDatum, -1, [dtext_left]),
  draw_TextInRect(LboxWin,rct(572, T, 652, B), TDatum, -1, [dtext_left]),
  win_SetBackColor(LboxWin, Achtergrond),
  %draw_TextInRect(LboxWin,rct(584, T, 665, B), Tnummer, -1, [dtext_left]),
  draw_TextInRect(LboxWin,rct(654, T, 738, B), Tnummer, -1, [dtext_left]),
  %write("\n### maptype: ", MapType, "  ", Item),
  selBack(Maptype, ForGr, BackGr),
  win_SetForeColor(LboxWin, ForGr),
  win_SetBackColor(LboxWin, BackGr),
  draw_TextInRect(LboxWin,rct(740, T, R, B), Maptype, 1, [dtext_center]), !.

  dlg_backupselect_Create(Parent):-
	retractall(inetBestand(_,_,_,_,_,_,_,_)),
	assert(recnum(0)),
	LB_BACKUPVERSIONS_ITEMLIST = ["bezig met opzoeken.."],
	LB_BACKUPVERSIONS_SELECT = [0],
%MARK BackupSelect, new variables

	dialog_CreateModal(Parent,dlg_backupselect_ResID,"",
  		[
%BEGIN BackupSelect, ControlList, 13:36:43-12.10.2016, Code automatically updated!
		df(lb_backupversions,listbox(LB_BACKUPVERSIONS_ITEMLIST,LB_BACKUPVERSIONS_SELECT),nopr)
%END BackupSelect, ControlList
		],
		dlg_backupselect_eh,0,VALLIST,ANSWER),
	dlg_backupselect_handle_answer(ANSWER,VALLIST).

  dlg_backupselect_handle_answer(idc_ok,VALLIST):-!,
	dlg_backupselect_update(VALLIST).
  dlg_backupselect_handle_answer(idc_cancel,_):-!.  % Handle Esc and Cancel here
  dlg_backupselect_handle_answer(_,_):-
	errorexit().

  dlg_backupselect_update(_VALLIST):-
%BEGIN BackupSelect, Update controls, 13:36:43-12.10.2016, Code automatically updated!
	dialog_VLGetListBox(lb_backupversions,_VALLIST,_LB_BACKUPVERSIONS_ITEMLIST,_LB_BACKUPVERSIONS_SELECT),
%END BackupSelect, Update controls
	true.

%MARK BackupSelect, new events

%BEGIN BackupSelect, idc_backupvanauto _CtlInfo


  dlg_backupselect_eh(_Win,e_Control(idc_backupvanauto,_CtrlType,_CtrlWin,_CtlInfo),0):-
	autobackup(Active, Dir),
	Active = b_true,
	trap(existdir(Dir, _),RetD,behandelAutoerror(RetD,Dir)),		% A en B niet meer
	SelectedItem = dlgFileNameZeker("",["Toernooiback-up","*.*"],"Zoek een auto-back-up",[],Dir,_OutListFiles),
	filenameext(SelectedItem, _, Ext),
	format(Boodsch,"autobackup teruggezet %s", SelectedItem), 
	writeToJournaal(Boodsch),
	ifGZ(SelectedItem, Ext, UnpackedItem),
	vervangDat(_Win, UnpackedItem),
	win_Destroy(_Win),	% 22.12.2010
	!.
  dlg_backupselect_eh(_Win,e_Control(idc_backupvanauto,_CtrlType,_CtrlWin,_CtlInfo),0):-
	autobackup(Active, _Dir),
	Active = b_False,
	Answer = dlg_MessageBox( "Auto-Backup", "Nog geen autoback-up ingesteld voor dit toernooi op deze computer!\nNu doen?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
	Answer = 1,
	win_Destroy(_Win),
	TaskWin = vpi_GetTaskWin(),
	dlg_toernooiinstellingen_Create(Taskwin, []),
	!.
%END BackupSelect, idc_backupvanauto _CtlInfo

%BEGIN BackupSelect, idc_zoek _CtlInfo
  dlg_backupselect_eh(_Win,e_Control(idc_zoek,_CtrlType,_CtrlWin,_CtlInfo),0):-
	getFolder(_Doc,_,_,Dir),
	%concat(Doc, "\\Toernooien\\Backups", Dir), 
	SelectedItem = dlgFileNameZeker("",["Toernooiback-up","*.tas;tas*.dat;*.gz"],"Zoek een back-up",[],Dir,_OutListFiles),
	%write("\n### ",SelectedItem),
	filenameext(SelectedItem, _, Ext),
	format(Boodsch,"backup teruggezet %s", SelectedItem), 
	writeToJournaal(Boodsch),
	ifGZ(SelectedItem, Ext, UnpackedItem),
	vervangDat(_Win, UnpackedItem),
	win_Destroy(_Win),	
	!.
%END BackupSelect, idc_zoek _CtlInfo

%BEGIN BackupSelect, e_OwnerMeasureItem
  dlg_backupselect_eh(_Win,e_OwnerMeasureItem(_CtlType,_CtlId,_ItemId,_Data),Size):-
	win_GetTextExtent(_Win, "goedgek", -1, Wstatus, Height),
	H = Height + 1,
	bitleft(H, 16, H1),
	Size = Wstatus + H1 + 1,
	!.
%END BackupSelect, e_OwnerMeasureItem

%BEGIN BackupSelect, e_OwnerDraw
  dlg_backupselect_eh(_Win,e_OwnerDraw(_CtlType,_CtlId,_ItemId,_ItemAction,
		ItemState,LboxWin,RectItem,_ItemData),0):-
	own_member1(ItemState, odstate_Selected),   % was selected
	win_SetBrush(LboxWin,brush(pat_Solid, color_Blue)),
	myOwnerdrawB(_Win, LboxWin, _ItemId, RectItem, color_white, color_blue),
	!.
  dlg_backupselect_eh(_Win,e_OwnerDraw(_CtlType,_CtlId,_ItemId,_ItemAction,
		_ItemState,LboxWin,RectItem,_ItemData),0):-
		win_SetPen(LboxWin,pen(1,ps_Solid,color_White)),
	myOwnerdrawB(_Win, LboxWin, _ItemId, RectItem, color_black, color_white), !.

%END BackupSelect, e_OwnerDraw

%BEGIN BackupSelect, e_Destroy
  dlg_backupselect_eh(_Win,e_Destroy,0):-
	%assert(newdir1("x")),
	%retractall(buBestandsnaam(_, _)),
	retractall(inetBestand(_,_,_,_,_, _, _, _)),
	!.
%END BackupSelect, e_Destroy

%BEGIN BackupSelect, lb_backupversions selchanged
  dlg_backupselect_eh(_Win,e_Control(lb_backupversions,_CtrlType,_CtrlWin,selchanged),0):-!,
	dialog_SetState(_Win, [enable(idc_ok, b_True)]),
	!.
%END BackupSelect, lb_backupversions selchanged

%BEGIN BackupSelect, idc_ok _CtlInfo
  dlg_backupselect_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	%lok(inet),
	%assert(newdir1("x")),
	dialog_GetListBox(_Win, lb_backupversions,_LB_BACKUPVERSIONS_ITEMLIST,_LB_BACKUPVERSIONS_SELECT),
	_LB_BACKUPVERSIONS_ITEMLIST = [SelectedItem], 
        %str_Int(SelectedItem, Nr),
        stopdbrefresh(),
	backupVan(_Win, SelectedItem),
	win_destroy(_Win),
	%compress(replace),
	!.
  dlg_backupselect_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0) :- !.
%END BackupSelect, idc_ok _CtlInfo

%BEGIN BackupSelect, lb_backupversions activated
  dlg_backupselect_eh(_Win,e_Control(lb_backupversions,_CtrlType,_CtrlWin,activated),0):-
	trap(CW = win_GetCtlHandle(_Win, idc_ok),_,fail),
	win_SendEvent(_Win,e_Control(idc_ok,wc_PushButton,CW, activated())),
	!.
%END BackupSelect, lb_backupversions activated

%BEGIN BackupSelect, idc_help _CtlInfo
  dlg_backupselect_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Selecteerbackup"),
	!.
%END BackupSelect, idc_help _CtlInfo

%BEGIN BackupSelect, e_User
  dlg_backupselect_eh(_Win,e_User(1,_Ptr),0):-
	%lok(disk),
	LboxWin = win_GetCtlHandle(_Win, lb_backupversions),
	%not(newdir1("x")),
	lbox_Clear(LboxWin),
	cursor_SetWait(),
	visBackupSelectieDisk(),				% van disk
	%sortBackupselectie(),
	inetBestand(NrS, _MapType, _TDag, _TNaam, _TStamp, _TDatum, _Tnummer, _Licentie),
	%str_int(NrS, Nr),
	lbox_Add(LboxWin, NrS),
	fail. 
  dlg_backupselect_eh(_Win,e_User(1,_Ptr),0):-
	%lok(inet),
	win_SetText(_Win, "back-up selecteren"),
	not(geenServer(_)),
	providerX(Prov, _, "", _URL1),
	VarList = aanmeldenVars(),
	VarList1 = ["step", "zoekN" | VarList],
	format(URL, "http://%s/backupenzo.php", Prov),	% 2.1.2011 als mail verzonden
	get_Tempdir(_, FullName),
	mydeletefile(FullName),   	% 9.12.2009
	cursor_SetWait(),
	%Return = getbackup(URL, FullName, VarList1),
	Return = downloadData(URL, FullName, VarList1,_NotFound,0),
	checkServer(Return),
	%file_str(Fullname, Str),
	%write("\n", Str),
	visBackupSelectieInternet(FullName),
	fail.
  dlg_backupselect_eh(_Win,e_User(1,_Ptr),0):-
   	not(geenServer(_)),
	providerX(_Prov, _, "tabun1.php", URL1),		% 16.6.2012 de gedeelde
	get_Tempdir(_, FullName),
	VarList = aanmeldenVars(),
	webdir(Map, _, _),
	VarList2 = ["map", Map, "func", "getbackupsN" | VarList],
	mydeletefile(FullName),
	cursor_SetWait(),
	X = downloadData(URL1, FullName, VarList2, _, 0),
	checkServer(X),
	checkvoortgang(100, ""),      % sluit weer
	visBackupSelectieInternet(FullName),
	fail.
  dlg_backupselect_eh(_Win,e_User(1,_Ptr),0):-
	LboxWin = win_GetCtlHandle(_Win, lb_backupversions),	% voeg ze allemaal toe aan de lijst in de goede volgorde
	%save("temp2.txt",temp2),
	lbox_Clear(LboxWin),
	findall(NrS, inetBestand(NrS, _MapType, _TDag, _TNaam, _TStamp, _TDatum, _Tnummer, _Licentie), NrSL),
	not(NrSL = []),
	sortstring_c(NrSL, 1),
	reverse(NrSL, [], Items1),
	lbox_Clear(LboxWin),
	lbox_Add(LboxWin, Items1), !.
  dlg_backupselect_eh(_Win,e_User(1,_Ptr),0) :-
	dlg_MessageBox( "Back-up", "Geen bestand gevonden.\nGebruik de knop Zoek..", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	!.
%END BackupSelect, e_User

%BEGIN BackupSelect, e_Create

  dlg_backupselect_eh(_Win,e_Create(_CreationData),0):-!,
	CtrlWin = win_GetCtlHandle(_Win, lb_backupversions),
	New_font = font_Create(ff_System, [], 9),
	win_SetFont(CtrlWin, New_Font),
	win_PostEvent(_Win,e_User(1,0)),
	!.
%END BackupSelect, e_Create

  dlg_backupselect_eh(_,_,_):-!,fail.

%END_DLG BackupSelect


predicates
%nondeterm   dirfiles_2(string, string, string, string)
	%checkNew(string, stringlist)
determ  getToernooiNaam1(string, string,string,string)  % voor toernooiselectie
  %setnaam(WINDOW)
  determ searchL(slist, string, integer, integer)
  %terug1()
  %terug2(string)
procedure   omdraaienDatum(string, string)
% toernDirRepr(string Repr, string TDatum1, string NewDir, string NewDir, string TNaam, string TNum, lokatie, lokatie, 
%           string DatDatum, string LogDatum, string Licentie)  - (o, i, i, i, i, i, i, i, i, i, i)

clauses 

%newdir1("").

searchL([H|_], S, I, I) :-
  searchstring(H, S, _), !, !.
searchL([_|R],S, In, Uit) :-
  In1 = In + 1, !,
  searchL(R, S, In1, Uit).
%searchL(_,_,_,0). 

naamTo(Line, TNaam, TDatum,IID) :-
  trap(term_str(dbasedom, Term, Line), _, fail),
  Term = naam(TNaam, _TNum, TDatum,IID,_,_), !.
naamTo(Line, TNaam, TDatum,IID) :-
  trap(term_str(puin, Term, Line), _, fail),
  Term = naam(TNaam, _TNum, TDatum,IID,_,_), !.
naamTo(_Line, "", "","").

getToernooiNaam1(Fnam, _TNaam,_TNum,_TDatum) :-
  not(existfile(FNam)), !, fail.
getToernooiNaam1(Fnam, TNaam,IID,TDatum) :-
  closefile(work),
  format(ErrMsg, "\n### error in gettoernooinaam: %s", FNam),
  trap(openread(work, Fnam),_, writetojournaal(ErrMsg)),
  %myOpenread(0, work, Fnam),
  readdevice(work),
  repeat_f(work),
  readln(Line),
  fronttoken(Line, T0, _),
  T0 = "naam", 		% normaal bovenaan
  naamTo(Line, TNaam, TDatum,IID),
  %trap(term_str(dbasedom, Term, Line), _, fail),
  %Term = naam(TNaam, _TNum, TDatum,IID,_,_),
  %readXX(Line, TNaam,TNum,TDatum),
  closefile(work), !,
  readdevice(keyboard).
getToernooiNaam1(_, "", "", "")  :-
  closefile(work),
  readdevice(keyboard).

setnaam() :-
  TaskWin = vpi_GetTaskWin(),
  naam(Naam, _, _,_,_,_),
  %get_toernooidir(Dir, _, _, _),
  %lastdir(Dir,_,Token),
  format(Text, "%s %s",titeldeelc, Naam),
  win_SetText(TaskWin, Text), !.
setnaam() :-
  TaskWin = vpi_GetTaskWin(),
  win_SetText(TaskWin, titeldeelc).

omdraaienDatum(In, Uit) :-
  searchchar(In, '-', Pos),
  Pos1 = Pos - 1,
  frontstr(Pos1,In,Dag,R1),
  frontchar(R1, _, R2),
  searchchar(R2, '-', P2),
  P2x = P2 - 1,
  frontstr(P2x, R2, Maand, R3),
  frontchar(R3, _, Jaar),
  str_int(Dag, DagI),
  helestr_int(0, DagS, DagI),
  str_int(Maand, MaandI),
  helestr_int(0, MaandS, MaandI), !,
  format(Uit, "%s-%s-%s", Jaar, MaandS, DagS). 
omdraaienDatum(In, In).


%BEGIN_DLG Toernooiselectie
/**************************************************************************
	Creation and event handling for dialog: Toernooiselectie
**************************************************************************/

constants

%BEGIN Toernooiselectie, CreateParms, 11:33:08-29.9.2013, Code automatically updated!
  dlg_toernooiselectie_ResID = idd_toernooiselectie
  dlg_toernooiselectie_DlgType = wd_Modal
  dlg_toernooiselectie_Help = idh_toernooimappen
%END Toernooiselectie, CreateParms

database - mappen
  single iafm(integer Item1, integer Item2, integer Item3, integer Item4, integer Debug)

predicates

  dlg_toernooiselectie_eh : EHANDLER

  myOwnerdraw2(window Parent, window LboxWin, integer ItemId, RCT RectItem, COLOR Voorgrond, COLOR Achtergrond)
  procedure kleur(window, string, rct, string, string)
  determ switchMap(string InetOfDisk, string Map, string Bestand)
  %switchMapNoreset(string Map, string Bestand)
  getSelState(window Win, string Eigen, string Web, string TKort, string TLok)
  enableDisableKnoppen(window)
  webtoernooieninfo(string)
  getInetToernooien()
  %verwijderLokalemap(string Map)
  string isCurrentToernooi(string TMap) 
  determ isOnline(lokatie)
  selecteertoernooi(window)
  determ nietgedeeld(string FileName)
  setknoppen(slist LboxList, window LboxWin, boolean)
  showtoernooidirs()


clauses

iafm(20,75,350,90, 80).

kleur(Win, "inet", Rect, _DirIn, "Internet") :-
  %win_SetBackColor(Win, 0xFFFF80),
  win_SetBrush(Win,brush(pat_Solid,0xFFFF80)),
  win_SetPen(Win, pen(1, ps_solid, 0x400040)),
  win_SetBackMode(Win, bk_Transparent),
  draw_RoundRect(Win, Rect, 8, 8),
  !. 
kleur(Win, _, Rect, InUit, InUit) :-
  %win_SetBackColor(Win, 0xFFFF80),
  win_SetBrush(Win,brush(pat_Solid,0x80FFFF)),
  win_SetPen(Win, pen(1, ps_solid, 0x400040)),
  win_SetBackMode(Win, bk_Transparent),
  draw_RoundRect(Win, Rect, 8, 8),
  !. 
kleur(_, _, _, X, X).

myOwnerdraw2(_Win, LboxWin, ItemId, RectItem, Voorgrond, Achtergrond) :-
  Parent = win_getParent(Lboxwin),
  Font = win_GetFont(Parent),
  _FontName = font_GetAttrs(Font, Style, Size),
  Siz1 = Size - 2,
  NewFont = font_SetAttrs(Font, Style, Siz1),
  NewFont2 = font_SetAttrs(Font,[fs_Bold],0),
  win_SetPen(LboxWin,pen(1,ps_Solid,Achtergrond)),
  RectItem = rct(_L, T, R, B),
  iafm(Afm1, Afm2, Afm3, Afm4, Afm5),
  draw_Rect(LboxWin, rct(Afm1, T, R, B)),
  Item = lbox_GetItem(LboxWin, ItemId),	% get the string
  parsetabs(Item, Fields),
  Fields = [GewijzigdOrde, TNaam, TNum, _TKort, File, Web, Eigen, TDatum | _],
  %Fields = [TDatum, Eigen, ToernDir, TNaam, TNum , Web| _], 
  win_SetBackColor(LboxWin, color_Green),
  win_SetForeColor(LboxWin, color_Black),
  win_SetFont(LboxWin, NewFont2),
  draw_TextInRect(LboxWin, rct(0, T, Afm1, B), Eigen, -1, [dtext_center]),
  win_SetForeColor(LboxWin, Voorgrond),
  Afm1a = Afm1 + 2,
  Afm2a = Afm1a + Afm2,
  Afm1c = Afm1a + 4,
  %Afm2aa = Afm2a - 2,
  frontstr(5, GewijzigdOrde, _, GewijzigdX),
  fronttoken(GewijzigdX, Maand, Rest1),
  fronttoken(Rest1, _, Rest2),
  fronttoken(Rest2, Dag, Rest3),
  format(GewijzigdView,"%s/%s%s",Dag,Maand,Rest3),
  %MarkFont = font_SetAttrs(Font, [fs_Bold | Style], 0),
  win_Setfont(Lboxwin, Newfont),
  win_SetBackColor(LboxWin, Achtergrond),
  T1 = T+2,
  draw_TextInRect(LboxWin, rct(Afm1c, T1, Afm2a, B), GewijzigdView, -1, [dtext_center]),
  Afm2b = Afm2a + 2,
  Afm3a = Afm2b + Afm3,
  Afm3aa = Afm3a - 2,
  Afm2c = Afm2b + 2,
  win_Setfont(Lboxwin, Font),
  win_SetForeColor(LboxWin, color_Black),
  kleur(LboxWin, Web, rct(Afm2b, T, Afm3aa, B), File, FileDisp),
  draw_TextInRect(LboxWin, rct(Afm2c, T, Afm3a, B), TNaam, -1, [dtext_left]),
  %write ("\n #####? ", TNaam), 
  win_SetForeColor(LboxWin, Voorgrond),
  win_SetBackColor(LboxWin, Achtergrond),
  Afm3b = Afm3a + 2,
  Afm4a = Afm3b + Afm4,
  draw_TextInRect(LboxWin, rct(Afm3b, T, Afm4a, B), TNum, -1, [dtext_left]),
  Afm4b = Afm4a + 2,
  Afm4c = Afm4b + Afm5,
  draw_TextInRect(LboxWin, rct(Afm4b, T, Afm4c, B), TDatum, -1, [dtext_left]),
  Afm5a = Afm4c + 2,
  win_Setfont(Lboxwin, Newfont), !,
  draw_TextInRect(LboxWin, rct(Afm5a, T, R, B), FileDisp, -1, [dtext_left]).
myOwnerdraw2(_Win, LboxWin, ItemId, RectItem, _Voorgrond, Achtergrond) :-
  Parent = win_getParent(Lboxwin),
  Font = win_GetFont(Parent),
  _FontName = font_GetAttrs(Font, Style, Size),
  Siz1 = Size - 2,
  NewFont = font_SetAttrs(Font, Style, Siz1),
  win_Setfont(Lboxwin, Newfont),
  win_SetPen(LboxWin,pen(1,ps_Solid,Achtergrond)),
  RectItem = rct(_L, T, R, B),
  iafm(Afm1, _Afm2, _fm3, _fm4, _fm5),
  draw_Rect(LboxWin, rct(Afm1, T, R, B)),
  Item = lbox_GetItem(LboxWin, ItemId),	% get the string
  draw_TextInRect(LboxWin, RectItem, Item, -1, [dtext_left]).
  

switchMap("inet", Keus0, WB) :-
  assert(webdir(Keus0, inet, WB)),
  toernooiDirReset(), !.
switchMap(_, _Keus0, TLok) :-
  webdir(WM, _, TB),
  assert(webdir(WM, disk, TB)),
  toernooidir(DirC, _),
  set_toernooidir(DirC,TLok).

getSelState(Win, Eigen, Web, Tkort, Tlokatie) :-
  LboxWin = win_GetCtlHandle(Win, idct_toernooisel),
  Index = lbox_GetSelIndex(LboxWin),
  Item = lbox_GetItem(LboxWin, Index),
  parseTabs(Item, TList),
  TList = [_Tdatum, _Tnaam, _Tnum, TKort, Tlokatie, Web, Eigen | _],
  trim_c(TKort).


%enableDisableKnoppen(Win) :- 
%  alleenlezen,
%  dialog_SetState(Win, [enable(idct_op_netwerk, b_False)]),
%  fail.
enableDisableKnoppen(Win) :- 
  dialog_SetState(Win, [enable(idct_verwijder, b_False)]),
  getSelState(Win, Eigen, _Web, _, Keus), 
  not(searchstring(Keus,"demo",_)),
  not(Eigen = ">>"),
  %not(Web = "w"),
  dialog_SetState(Win, [enable(idct_verwijder,b_True)]),
  fail.
enableDisableKnoppen(Win) :- 
  dialog_SetState(Win, [enable(idct_maak_nieuw, b_False)]),
  getSelState(Win, Eigen, _Web, _, _Tlokatie), 
  Eigen = ">>",
  %not(Web = "inet"),
  dialog_SetState(Win, [enable(idct_maak_nieuw,b_True)]),
  fail.
enableDisableKnoppen(Win) :- 
  dialog_SetState(Win, [enable(idct_nieuw, b_False)]),
  getSelState(Win, Eigen, _Web, _Keus, _TLok), 
  Eigen = ">>",
  %not(Web = "inet"),
  dialog_SetState(Win, [enable(idct_nieuw,b_True)]),
  fail.
enableDisableKnoppen(Win) :- 
  dialog_SetState(Win, [enable(idct_web, b_False)]),
  getSelState(Win, Eigen, Web, _Tkort, Tlokatie),
  Eigen = ">>",
  not(Web = "inet"),
  %not(alleenlezen),
  upper_lower(Tlokatie, TlokatieL),
  not(searchstring(TlokatieL,"demot",_)),
  dialog_SetState(Win, [enable(idct_web,b_True)]),
  fail.
/*enableDisableKnoppen(Win) :- % is deze wel nodig?
  dialog_SetState(Win, [enable(idc_ok, b_False)]),
  getSelState(Win, _Eigen, _Web, _Keus, _), 
  not(alleenlezen),
  dialog_SetState(Win, [enable(idc_ok,b_True)]),
  fail.*/ 
enableDisableKnoppen(_).

webToernooienInfo(_Bestandsnaam) :-
  geenServer(_),
  format(Msg, "Het huidige toernooibestand kan niet gedeeld worden (geen internetverbinding?)\nStart de TA opnieuw als u verbinding hebt.\n\n"),
  1 = dlg_MessageBox("Delen", Msg , mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), !.
webToernooienInfo(_Bestandsnaam) :-
  naam(ToernooiNaam, _, _,_,_,_),
  inetbestand(_, _, _, _DirOnline, Toernooinaam, _, _, _),
  format(Msg, "Toernooi met die naam:\n%s\nstaat al gedeeld!\nKan niet nogmaals delen!",Toernooinaam),
  dlg_MessageBox("Delen", Msg , mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), !.
webToernooienInfo(Bestandsnaam) :-
  date(Year,_M,_D),
  getbacktrack(Here),
  numb(N),
    char_int('a', I), I1 = I + N - 1, char_int(C, I1), 
    format(DirOnline, "%u%c", Year, C),
    not(inetbestand(_, _, _, DirOnline, _, _, _, _)),
  cutbacktrack(Here),
  format(Msg, "Het huidige toernooibestand wordt online gezet.\nJe kunt het dan delen met andere gebruikers\nmet dezelfde licentie en een internetverbinding.\n\nDoorgaan?\n\n"),
  1 = dlg_MessageBox("Delen", Msg , mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
  TaskWin = vpi_GetTaskWin(),
  win_PostEvent(TaskWin,e_User(upd_toolb,0)),
  %get_toernooidir(_, _Mapzelfde, _, Pad), 
  %compress(replace),  %+++++++++++++++++++ weg
  filenamepath(Bestandsnaam, _HPad, Naam),
  concat("gedeeld ", Naam, Naam1),
  getfolder(_, _, Gedeeld, _Backup),
  %get_tempfolder(Gedeeld, _), % !!!!
  filenamepath(GBestandsnaam, Gedeeld, Naam1),
  copyfile(Bestandsnaam,GBestandsnaam),
  backupdirect(),
  mydeletefile(Bestandsnaam),  %++++++++++++++++++++++++
  assert(webdir(DirOnline, inet, GBestandsnaam)),
  %dlg_note("compress gaat beginnen"),  %+++++++++++++++++++++++++++
  compress(replace),
  startDBrefresh(),
  AList = aanmeldenvars(),
  AAList = ["map", DirOnline , "func", "startRegistratie" | AList],
  providerX(_, _, "tabun1.php", ProvM),
  get_Tempdir(TDir, _),
  filenamePath(Html, TDir, "registratie.html"),
  mydeletefile(Html),
  _Return = downloadData(ProvM, Html, AAList, _, 0),
  Null = cast(api_hwnd,0),
  api_ShellExecute(Null,"open",Html,"","",1),		% open in browser
  dlg_MessageBox( "Op het Web", "Het toernooibestand is naar het web verhuisd en kan daar gedeeld worden.\nSelecteer daarvoor het toernooi vanuit een andere PC met dezelfde TA-licentie!\n\nEr is ook een back-up gemaakt.", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  close_alles(),
  %format(MsgN, "Over van disk: %s",Bestandsnaam),
  format(MsgM, "Over naar inet: %s %s",DirOnline, GBestandsnaam),
  update_proj_toolbar(),
  %writeToJournaal(MsgN),
  writeToJournaal(MsgM), !.
webToernooienInfo(_DirO).

getInetToernooien() :-
  not(geenServer(_)),
  retractall(inetBestand(_, _, _, _, _, _, _, _)),
  get_Tempdir(_, TempFile),
  mydeletefile(Tempfile),
  AV = aanmeldenvars(),
  NAV = ["func", "gettoernooienN" | AV],
  providerX(_, _, "tabun1.php", ProvM),
  Ret = downloadData(ProvM, TempFile, NAV, _, 0),
  checkServer(Ret),
  %Ret = 0,	% anders geen
  %file_str(TempFile, SSS),
  %write(SSS),
  closefile(work),
  openread(work,TempFile),
  readdevice(work),
  repeat_f(work),
  trap(readterm(temp2,Fact),_,true),
  recnum(N),
  assert(Fact,temp2),
  N1 = N + 1,
  assert(recnum(N1)),
  fail.
getInetToernooien() :-
  closefile(work).

isCurrentToernooi(TMap, ">>") :- 
  webdir(TMap, inet, _), !.
isCurrentToernooi(Fullname1, ">>") :- 
  webdir(_, disk, _),
  toernooidir(_, Fullname2),
  upper_lower(Fullname1, Fullname2),  !.
isCurrentToernooi(_, "").

isOnline(disk) :-
  not(alleenlezen1),
  dlg_MessageBox( "Op disk", "Maak een back-up via het back-up menu!", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.
isOnline(inet) :-
  1 = dlg_MessageBox( "Gedeeld", "Er wordt een ongedeelde kopie van het toernooibestand gemaakt (op disk).\nDoorgaan?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ), !.

close_alles() :-
  close_spelersel(),
  close_emailwin(),
  close_schemas(),
  close_poules(),
  close_rapporten(),
  close_onderdeelselect(),
  close_Formulier(),
  close_betaal(),
  close_uitslaginv(),
  close_situatie(),
  close_waarsch(),
  close_kladblok(),
  %closebrowser(),
  close_editorwin(), !.

%selecteerToernooi(_Win) :-	% 11.1.2011
%  alleenlezen, !.
selecteerToernooi(_Win) :-
  getSelState(_Win, Eigen, _Web, _TKort, _TLok), 
  Eigen = ">>", 
  win_Destroy(_Win), !.
selecteerToernooi(_Win) :-
  LboxWin = win_GetCtlHandle(_Win, idct_toernooisel),
  Index = lbox_GetSelIndex(LboxWin),
  Item = lbox_GetItem(LboxWin, Index),
  parseTabs(Item, TList),
  TList = [_, _, _, TKort, TLok, Web, _Selected| _],
  close_alles(),
  stopdbrefresh(),	% 31.8.2011
  %close_poules(),
  %get_toernooidir(_, _, _, Huidig),
  retractall(alleenlezen1),
  switchMap(Web, TKort, TLok),
  win_Destroy(_Win),
  %get_toernooidir(_,_,_,DirO),
  %toernooidir(DirO, _XLok),
  %format(MsgN, "Over van: %s\n      Over naar: %s",Huidig,DirO),
  %writeToJournaal(MsgN),
  TaskWin = vpi_GetTaskWin(),
  dlg_toernooiload_Create(TaskWin, "toernooiselectie_OK", b_False, update).

nietgedeeld(FileName) :-
  upper_lower(FileName, FFL),
  searchstring(FFL,"gedeeld",_FoundPos),
  dlg_MessageBox( "Gedeeld", "De gedeelde directory is een systeemmap!\n\nTip: Laad zo'n GEDEELD bestand alleen in noodgevallen,\nen dan als back-up!", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), !,
  fail.
nietgedeeld(_FileName).

setknoppen(LboxList, LboxWin, b_True) :-
  not(alleenlezen1),
  searchL(LboxList, ">>", 0, Index),
  lbox_SetSel(LboxWin, Index, b_True), !.
setknoppen(_LboxList, _LboxWin, b_False) :-
  alleenlezen1, !,
  write("alleenlezen staat aan!").
setknoppen(_, _, b_True). 

showtoernooidirs() :- !.  	% 24.2.2013  alleen voor debug
showtoernooidirs() :-
  write("\nGetToernooidirs:"),
  gettoernooidirs(Dir),
  write("\n", Dir),
  fail.
showtoernooidirs() :-
  write("\nToernooidirs:"),
  toernooidirs(Dir),
  write("\n", Dir),
  fail.
showtoernooidirs().

  dlg_toernooiselectie_Create(Parent):-
  showtoernooidirs(),
  %get_toernooidir(_,_,_,Huidig),
  %write("\n #### bestand: ",Huidig),
	win_CreateResDialog(Parent,dlg_toernooiselectie_DlgType,dlg_toernooiselectie_ResID,dlg_toernooiselectie_eh,0).

%MARK Toernooiselectie, new events

%BEGIN Toernooiselectie, e_Update
  dlg_toernooiselectie_eh(_Win,e_Update(_UpdateRct),0):-
	%LboxWin = win_GetCtlHandle(_Win, idct_toernooisel),
	%Index = lbox_GetSelIndex(LboxWin),
        %getSelState(_Win, Eigen, _Web, _, _Tlokatie), 
	%Eigen = ">>",
	alleenlezen1,
	ROWin = win_GetCtlHandle(_Win, idctt_huidige_is_alleen_lezen),
	win_SetState(ROWin, [wsf_Visible]),
	!.
  dlg_toernooiselectie_eh(_Win,e_Update(_UpdateRct),0):-
        %getSelState(_Win, Eigen, _Web, _, _Tlokatie), 
	%not(Eigen = ">>"),
	not(alleenlezen1),
	ROWin = win_GetCtlHandle(_Win, idctt_huidige_is_alleen_lezen),
	win_SetState(ROWin, [wsf_InVisible]), !.
%END Toernooiselectie, e_Update

%BEGIN Toernooiselectie, e_Destroy
  dlg_toernooiselectie_eh(_Win,e_Destroy,0):-
	retractall(inetbestand(_, _, _, _, _, _, _, _)),
	!.
%END Toernooiselectie, e_Destroy

%BEGIN Toernooiselectie, idct_web _CtlInfo
  dlg_toernooiselectie_eh(_Win,e_Control(idct_web,_CtrlType,_CtrlWin,_CtlInfo),0):-
	teGrootOmtedelen,
	fail.
  dlg_toernooiselectie_eh(_Win,e_Control(idct_web,_CtrlType,_CtrlWin,_CtlInfo),0):-
%	LboxWin = win_GetCtlHandle(_Win, idct_toernooisel),
%	Index = lbox_GetSelIndex(LboxWin),
%	Item = lbox_GetItem(LboxWin, Index),
%	parseTabs(Item, TList),
%	TList = [_Tdatum, _Tnaam, _Tnum, _TKort, Tlokatie, Web, _Eigen | _],
	get_Toernooidir(_, _ , _, Bestand),
%	term_str(lokatie, Lok, Web),
	webToernooienInfo(Bestand),
	win_Destroy(_Win),
	!.
%END Toernooiselectie, idct_web _CtlInfo

%BEGIN Toernooiselectie, e_OwnerMeasureItem
  dlg_toernooiselectie_eh(_Win,e_OwnerMeasureItem(_CtlType,_CtlId,_ItemId,_Data),Size):-!,
	win_GetTextExtent(_Win, "Een hele lange toernooinaam in 2013 Een hele lange toernooin", -1, Wstatus, Height),
	H = Height + 1,
	bitleft(H, 16, H1),
	Size = Wstatus + H1 + 1,
	win_GetTextExtent(_Win, " 1920-01-02 ", -1, Afm5, _),
	iafm(Afm1, Afm2, _Afm3, Afm4, _Afm5),
	assert(iafm(Afm1, Afm2, Wstatus, Afm4, Afm5)),
	% ---
	/*Parent = win_getParent(_Win),
	Font = win_GetFont(Parent),
	_FontName = font_GetAttrs(Font, Style, Size),
	Siz1 = Size - 2,
	NewFont = font_SetAttrs(Font, Style, Siz1),
	NewFont2 = font_SetAttrs(Font,[fs_Bold],0),*/
	!.
%END Toernooiselectie, e_OwnerMeasureItem

%BEGIN Toernooiselectie, e_OwnerDraw
  dlg_toernooiselectie_eh(_Win,e_OwnerDraw(_CtlType,_CtlId,_ItemId,_ItemAction,
		ItemState,LboxWin,RectItem,_ItemData),0):-
	own_member1(ItemState, odstate_Selected),   % was selected
	win_SetBrush(LboxWin,brush(pat_Solid, color_Blue)),
	myOwnerdraw2(_Win, LboxWin, _ItemId, RectItem, color_white, color_blue),
	!.
  dlg_toernooiselectie_eh(_Win,e_OwnerDraw(_CtlType,_CtlId,_ItemId,_ItemAction,
		_ItemState,LboxWin,RectItem,_ItemData),0):-!,
	%_Itemstate = [],
	win_SetPen(LboxWin,pen(1,ps_Solid,color_White)),
	myOwnerdraw2(_Win, LboxWin, _ItemId, RectItem, color_black, color_white),
	!.
%END Toernooiselectie, e_OwnerDraw

%BEGIN Toernooiselectie, idct_nieuw _CtlInfo
  dlg_toernooiselectie_eh(_Win,e_Control(idct_nieuw,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	dlg_toernooistart_Create(_Win),
	win_Destroy(_Win),	% 26.4.2004
	!.
%END Toernooiselectie, idct_nieuw _CtlInfo

%BEGIN Toernooiselectie, idct_op_netwerk _CtlInfo
  dlg_toernooiselectie_eh(_Win,e_Control(idct_op_netwerk,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	getFolder(_Doc, Dir, _, _),                   % Zoek een toernooi
	%concat(Doc, "\\Toernooien", Dir), 
	Filename = dlgFileNameZeker("",["Toernooi","*.tas;tasn.dat"],"Open een voormalige toernooimap",[],Dir,_OutListFiles),
	filenamepath(Filename, Path, _),
	nietGedeeld(Filename),
	close_alles(),
	%close_poules(),
	%set_toernooidir(Path, Filename),
	webdir(P, _, B),
	assert(webDir(P, disk, B)),
	assert(toernooidir(Path, FileName)),
	%voegtoeaantoernooidirs(Path),
	win_Destroy(_Win),
	TaskWin = vpi_GetTaskWin(),
	dlg_toernooiload_Create(TaskWin, "toernooiselectie", b_False, update),
	win_PostEvent(TaskWin,e_User(upd_toolb,0)),	% update taakbuttons
	!.
%END Toernooiselectie, idct_op_netwerk _CtlInfo

%BEGIN Toernooiselectie, idct_verwijder _CtlInfo
  dlg_toernooiselectie_eh(_Win,e_Control(idct_verwijder,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idct_toernooisel),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	parseTabs(Item, TList),
	TList = [_Tdatum, _Tnaam, _Tnum, _Tkort, TLok, DiskOrInet, _ | _],
	%TList = [_, _Eigen, Keus0, _, _, Web, _, _, _TB | _],
	DiskOrInet = "disk",
	%TList = [_, _, Keus0 | _],
	format(ZekerWeten, "Weet je zeker dat je %s wilt wissen?", TLok),
	1 = dlg_MessageBox( "Toernooibestand wissen", ZekerWeten, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
	mydeletefile(TLok),
	lbox_Delete(LboxWin, Index),
	dlg_MessageBox( "Toernooibestand wissen", "Toernooibestand gewist!", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	LboxList = lbox_GetAll(LboxWin),	% niet alfabetisch!
	searchL(LboxList, ">>", 0, Index1),
	lbox_SetSel(LboxWin, Index1, b_True),
	%dialog_SetState(_Win, [enable(idc_ok, b_False)]),
	enableDisableKnoppen(_Win),
	!.
  dlg_toernooiselectie_eh(_Win,e_Control(idct_verwijder,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idct_toernooisel),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	parseTabs(Item, TList),
	%TList = [_, _Eigen, Keus0, _, _, Web, _, _, _StatusNS | _],
	TList = [_Tdatum, Tnaam, _Tnum, Keus0, _TLok, DiskOrInet, _TB | _],
	DiskOrInet = "inet",
	trim_c(Keus0),
	format(ZekerWeten, "Weet je zeker dat je het gedeelde toernooibestand\n%s\nnaar het on-line archief wilt verhuizen?\nEr kunnen misschien nog andere gebruikers zijn.\n\nNB Maak voor de zekerheid ook EERST\neen gewone back-up op een USB-staafje!\n", TNaam),
	1 = dlg_MessageBox( "Toernooibestand verwijderen", ZekerWeten, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
	get_Tempdir(_, TempFile),
	mydeletefile(Tempfile),
	AV = aanmeldenvars(),
	NAV = ["func", "verwijdertoernooi", "mapdel", Keus0 | AV],
	providerX(_, _, "tabun1.php", ProvM),
	Ret = downloadData(ProvM, TempFile, NAV, _, 0),
	file_str(TempFile, Str),
	%write(Str),
	searchstring(Str, "OK", _), 
	Ret = 0,	% anders geen
	lbox_Delete(LboxWin, Index),
	naam(ToernooiNaamW, _, _,_,_,_),
	retractall(inetbestand(_, _, _, _, ToernooinaamW, _, _, _)),
	format(Msg, "naar het on-line archief verhuisd: %s", TNaam), 
	writetojournaal(Msg),
	dlg_MessageBox( "Toernooi verwijderen", "Het toernooibestand is verhuisd naar het archief.\nHet on-line archief is te bereiken via \"back-up laden\".", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	LboxList = lbox_GetAll(LboxWin),	% niet alfabetisch!
	searchL(LboxList, ">>", 0, Index1),
	lbox_SetSel(LboxWin, Index1, b_True),
	%dialog_SetState(_Win, [enable(idc_ok, b_False)]),
	enableDisableKnoppen(_Win),
	!.

%END Toernooiselectie, idct_verwijder _CtlInfo

%BEGIN Toernooiselectie, idc_ok _CtlInfo
  dlg_toernooiselectie_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	selecteerToernooi(_Win), !.
%END Toernooiselectie, idc_ok _CtlInfo

%BEGIN Toernooiselectie, idc_help _CtlInfo
  dlg_toernooiselectie_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Toernooimappen"),
	!.
%END Toernooiselectie, idc_help _CtlInfo

%BEGIN Toernooiselectie, idct_toernooisel selchanged
  dlg_toernooiselectie_eh(_Win,e_Control(idct_toernooisel,_CtrlType,_LboxWin,selchanged),0):-
	enableDisableKnoppen(_Win),
	!.
%END Toernooiselectie, idct_toernooisel selchanged

%BEGIN Toernooiselectie, idct_toernooisel activated
  dlg_toernooiselectie_eh(_Win,e_Control(idct_toernooisel,_CtrlType,_LboxWin,activated),0):-
	selecteerToernooi(_Win), !.
%END Toernooiselectie, idct_toernooisel activated

%BEGIN Toernooiselectie, e_User

  dlg_toernooiselectie_eh(_Win,e_User(1,_Ptr),0):-	% 28.11.2010
	cursor_SetWait(),
	LboxWin = win_GetCtlHandle(_Win, idct_toernooisel),
	lbox_Clear(LboxWin),
	alleenlezen1,
	getParmstring(FN),
	filenamepath(FN, T, _),
	filenamepath(DirSpec, T, "*.tas"),
	trap(dirfiles(DirSpec, 0x40, Name, _, Uur, Min, _Sec, Jaar, Maand, Dag, _),_,fail),
	helestr_int(0,MaandS, Maand), helestr_int(0, DagS, Dag),helestr_int(0,UurS,Uur), helestr_int(0,MinS,Min), 
	format(GewijzigdOrde, "%u-%s-%s %s:%s", Jaar, MaandS, DagS, UurS, MinS),
	filenamepath(Fullname, T, Name),
	upper_lower(FN, Fullname),
	existfile(Fullname),
        getToernooiNaam1(Fullname, TNaam,TNum,TDatum),
        omdraaienDatum(TDatum, TDatum1),
	Current = isCurrentToernooi(Fullname), 
	TMap = " -- ",
	format(Repr, "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s",GewijzigdOrde, TNaam, TNum, TMap, Fullname,"disk", Current,TDatum1),
	lbox_Add(LboxWin, Repr),
	fail.
  dlg_toernooiselectie_eh(_Win,e_User(1,_Ptr),0):-	% 28.11.2010
	cursor_SetWait(),
	LboxWin = win_GetCtlHandle(_Win, idct_toernooisel),
	getToernooiDirs(T),
	filenamepath(DirSpec, T, "*.tas"),
	trap(dirfiles(DirSpec, 0x40, Name, _, Uur, Min, _Sec, Jaar, Maand, Dag, _),_,fail),
	trap(frontstr(7, Name, Front, _),_,fail),
	not(Front = "backup("),
	not(Front = "gedeeld"),
	helestr_int(0,MaandS, Maand), helestr_int(0, DagS, Dag),helestr_int(0,UurS,Uur), helestr_int(0,MinS,Min), 
	format(GewijzigdOrde, "%u-%s-%s %s:%s", Jaar, MaandS, DagS, UurS, MinS),
	filenamepath(Fullname, T, Name),
	existfile(Fullname),
        getToernooiNaam1(Fullname, TNaam,TNum,TDatum),
        omdraaienDatum(TDatum, TDatum1),
	Current = isCurrentToernooi(Fullname), 
	TMap = " -- ",
	format(Repr, "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s",GewijzigdOrde, TNaam, TNum, TMap, Fullname,"disk", Current,TDatum1),
	lbox_Add(LboxWin, Repr),
	fail.
  dlg_toernooiselectie_eh(_Win,e_User(1,_Ptr),0):-
	cursor_SetWait(),
	LboxWin = win_GetCtlHandle(_Win, idct_toernooisel),
	toernooidir(StartDir, _),	% 28.8.2008
	met_bslash(StartDir, StartDirB),
	concat(StartDirB, "..", StartDir1),
	filenamepath(DirSpec, StartDir1, "*."),
	vpi_processEvents(),
	trap(findall(SDirx,dirfiles(DirSpec, 0x10, SDirx, _, _, _, _, _, _, _, _), SDirS),_, fail),
	assert(recnum(0)),
	member(ToernDir, SDirS),
	ToernDir <> ".",
	ToernDir <> "..",
	upper_lower(ToernDir, ToernDirL),
	not(inetbestand(_, _, _, ToernDirL, _, _, _, _)),
	lastdir(StartDir, CurDirPre, _CurDirSec),
	recnum(Cur),
	Cur1 = Cur + 1,
	assert(recnum(Cur1)),
	vpi_ProcessEvents(b_True ),
	format(NewDir, "%s%s", CurDirPre, ToernDir),
        filenamepath(SSpec, Newdir, "*.dat"),
	trap(dirfiles(SSpec, 0x40, FNam, _, Uur, Min, _Sec, Jaar, Maand, Dag, _), _, fail),
	upper_lower("tasn.dat", FNam),           % voor de tasn.dat
        filenamepath(FileSpec, NewDir, FNam),
        existfile(FileSpec),
	helestr_int(0,MaandS, Maand), helestr_int(0, DagS, Dag),helestr_int(0,UurS,Uur), helestr_int(0,MinS,Min), 
	format(GewijzigdOrde, "%u-%s-%s %s:%s", Jaar, MaandS, DagS, UurS, MinS),
        getToernooiNaam1(FileSpec, TNaam,TNum,TDatum),
	omdraaienDatum(TDatum, TDatum1),
	Current = isCurrentToernooi(Filespec), 
	format(Repr, "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s", GewijzigdOrde, TNaam, TNum, ToernDir, Filespec,"disk", Current, TDatum1),
	lbox_Add(LboxWin, Repr),
	fail.
  dlg_toernooiselectie_eh(_Win,e_User(1,_Ptr),0):-
	cursor_SetWait(),
	LboxWin = win_GetCtlHandle(_Win, idct_toernooisel),
        getInetToernooien(),
	inetbestand(_, TDatum, TNum, TMap, TNaam, _DatDatum, GewijzigdOrde, _),
	cleantoernooinaam(TNaam, TNaamC),
	get_Tempdir(DocT, _),   %!!!!!!!!!!
	concat("gedeeld ", TNaamC,TNaamD), 
	filenameext(TNaamD1, TNaamD, ".tas"),
	filenamepath(WB, DocT, TNaamD1),
	Current = isCurrentToernooi(TMap), 
	format(Repr, "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s", GewijzigdOrde, TNaam, TNum, TMap, WB,"inet", Current, TDatum),
	lbox_Add(LboxWin, Repr),
	fail.
  dlg_toernooiselectie_eh(_Win,e_User(1,_Ptr),0):-
	cursor_SetWait(),
	LboxWin = win_GetCtlHandle(_Win, idct_toernooisel),
	LboxList = lbox_GetAll(LboxWin),
	sortstring_c(LboxList, 1),  % volgens datum
	reverse(LboxList, [], LboxListR),
	lbox_Suspend(LboxWin),
	lbox_Clear(LboxWin),
	lbox_Add(LboxWin, LboxListR),
	lbox_Resume(LboxWin),
	fail.	
  dlg_toernooiselectie_eh(_Win,e_User(1,_Ptr),0):-
	LboxWin = win_GetCtlHandle(_Win, idct_toernooisel),
	LboxList = lbox_GetAll(LboxWin),
	setknoppen(LboxList, LboxWin, NotRO),
	enableDisableKnoppen(_Win),
	NotRO = b_False,
	dlg_MessageBox( "Toernooibestand", "Huidige bestand is alleen lezen, je kunt wel een kopie maken.", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
        dialog_SetState(_Win, [enable(idct_maak_nieuw,b_True)]),
	fail.
  dlg_toernooiselectie_eh(_Win,e_User(1,_Ptr),0):-
	LboxWin = win_GetCtlHandle(_Win, idct_toernooisel),
	NoOfItems = lbox_CountAll(LboxWin),
	NoOfItems < 2,
	dlg_MessageBox( "Toernooibestand", "Open het demo-toernooi of zoek een toernooi op disk of stick met \"Zoek...\" of laad een back-up.", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	!.

%END Toernooiselectie, e_User

%BEGIN Toernooiselectie, idct_maak_nieuw _CtlInfo
  dlg_toernooiselectie_eh(_Win,e_Control(idct_maak_nieuw,_CtrlType,_CtrlWin,_CtlInfo),0):-
	alleenlezen1,
	getfolder(_DocDir, Dir, _, _),
	%concat(DocDir, "\\Toernooien", Dir),
	naam(TNaam, _, _,_,_,_),
	cleantoernooinaam(TNaam, ToernooiFnaam),
	concat(ToernooiFNaam, ".tas", SaveNaam),
	filenamepath(OpslaanAls, Dir, SaveNaam),
	FILENAME = dlgFileNameZeker(OpslaanAls,["Toernooibestand","*.tas"],"",[dlgfn_Save],Dir,_OutListFiles),
	retractall(alleenlezen1),
	filenamepath(FILENAME, NPath, _),
	filenamepath(NFile, Npath, SaveNaam),
	%voegtoeaantoernooidirs(NPath),
	assert(toernooidir(NPath, NFile)),
	webdir(WP, _, _WB),
	assert(webdir(WP, disk, Filename)),
	format(SaveErr, "opslaan van bestand op %s mislukt!", NFile),
	trap(save(NFile), _, dlg_Error(SaveErr)),
	del_logX(),
	TaskWin = vpi_GetTaskWin(),
	dlg_toernooiload_Create(TaskWin, "Kopie", b_False, replace),
	update_proj_toolbar(),
	vpi_ProcessEvents(b_True ),
	startDBrefresh(),
	win_Destroy(_Win), !.
  dlg_toernooiselectie_eh(_Win,e_Control(idct_maak_nieuw,_CtrlType,_CtrlWin,_CtlInfo),0):-
	webdir(WP, Lok, WB),      % maakt in feite een kopie
	isOnline(Lok),
	%TaskWin = vpi_GetTaskWin(),
	%win_Destroy(_Win),
	stopdbrefresh(),
	getfolder(_DocDir, Dir, _, _),
	%concat(DocDir, "\\Toernooien", Dir),
	naam(TNaam, _, _,_,_,_),
	cleantoernooinaam(TNaam, ToernooiFnaam),
	concat(ToernooiFNaam, ".tas", SaveNaam),
	filenamepath(OpslaanAls, Dir, SaveNaam),
	FILENAME = dlgFileNameZeker(OpslaanAls,["Toernooibestand","*.tas"],"",[dlgfn_Save],Dir,_OutListFiles),
	retractall(alleenlezen1),
	filenamepath(FILENAME, NPath, _),
	filenamepath(NFile, Npath, SaveNaam),
	%voegtoeaantoernooidirs(NPath),
	assert(toernooidir(NPath, NFile)),
	%webdir(WP, _, WB),
	assert(webdir(WP, disk, WB)),
	format(SaveErr, "opslaan van bestand op %s mislukt!", NFile),
	trap(save(NFile), _, dlg_Error(SaveErr)),
	del_logX(),
	TaskWin = vpi_GetTaskWin(),
	dlg_toernooiload_Create(TaskWin, "Kopie", b_False, replace),
	update_proj_toolbar(),
	vpi_ProcessEvents(b_True ),
	startDBrefresh(),
	win_Destroy(_Win),
	!.
%END Toernooiselectie, idct_maak_nieuw _CtlInfo

%BEGIN Toernooiselectie, e_Create
  dlg_toernooiselectie_eh(_Win,e_Create(_CreationData),0):-!,
	%LboxWin = win_GetCtlHandle(_Win, idct_toernooisel),
	%lbox_SetTabStops(LboxWin, [50, 240, 290, 320]),
	win_PostEvent(_Win,e_User(1,0)),
	!.
%END Toernooiselectie, e_Create

%BEGIN Toernooiselectie, idc_cancel _CtlInfo
  dlg_toernooiselectie_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-
	win_Destroy(_Win),
	!.
%END Toernooiselectie, idc_cancel _CtlInfo


  dlg_toernooiselectie_eh(_,_,_):-!,fail.

%END_DLG Toernooiselectie
%_________________________________________________________________________________7


%BEGIN_DLG Invoer
/**************************************************************************
	Creation and event handling for dialog: Invoer
**************************************************************************/

constants

%BEGIN Invoer, CreateParms, 16:15:05-3.9.2003, Code automatically updated!
  dlg_invoer_DlgType = wd_Modal
  dlg_invoer_Title = "Invoer"
  dlg_invoer_RCT = rct(40,32,187,118)
  dlg_invoer_Flags = [wsf_Close,wsf_TitleBar]
  dlg_invoer_Help = idh_spelergegevensinvoer
%END Invoer, CreateParms

database - inv1
  single filenaam(string, integer Mode)
  single details(ilist)

predicates

  dlg_invoer_eh : EHANDLER
procedure  getDetails(integer Mode, window Win, ilist Details)
procedure  setDetails(integer Mode, window Win, ilist Details)

clauses

  getDetails(5, Win, [1]) :-
	CtrlWin = win_GetCtlHandle(Win, idc_voornamen_overnemen),
	b_True = win_IsChecked(CtrlWin), !.
  getDetails(5, Win, [2]) :-
	CtrlWin = win_GetCtlHandle(Win, idc_voorletters_overnemen),
	b_True = win_IsChecked(CtrlWin), !.
  getDetails(5, Win, [3]) :-
	CtrlWin = win_GetCtlHandle(Win, idc_allebei),
	b_True = win_IsChecked(CtrlWin), !.
  getDetails(_, _Win, []).

  setDetails(5, Win, [1]) :-
	CtrlWin = win_GetCtlHandle(Win, idc_voornamen_overnemen),
	win_Check(CtrlWin, b_True), !.
  setDetails(5, Win, [2]) :-
	CtrlWin = win_GetCtlHandle(Win, idc_voorletters_overnemen),
	win_Check(CtrlWin, b_True), !.
  setDetails(5, Win, [3]) :-
	CtrlWin = win_GetCtlHandle(Win, idc_allebei),
	win_Check(CtrlWin, b_True), !.
  setDetails(_, _Win, _).

filenaam("", 0).
details([3]).

  dlg_invoer_Create(Parent, Filenaam, Mode):-
	assert(filenaam(Filenaam, Mode)),
        %str_int(Str,  Mode),
	%dlg_Note("Test",Str),
	win_CreateDynDialog(Parent,
		[
%BEGIN Invoer, WinDefList, 16:15:05-3.9.2003, Code automatically updated!
		 dlg(wdef(dlg_invoer_DlgType,dlg_invoer_RCT,dlg_invoer_Title,u_DlgBase),dlg_invoer_Flags),
		 ctl(wdef(wc_PushButton,rct(63,74,95,84),"&Cancel",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop,wsf_Disabled,wsf_Invisible]),
		 ctl(wdef(wc_PushButton,rct(28,74,60,84),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop]),
		 customctl(wdef(wc_Custom,rct(16,51,123,62),"Progress",u_DlgBase),"progress",idc_progress,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(98,66,146,84),"&OK",u_DlgBase),idc_ok,[wsf_Group,wsf_TabStop,wsf_Disabled,wsf_Invisible]),
		 ctl(wdef(wc_RadioButton,rct(25,17,117,27),"&Roepnaam overnemen",u_DlgBase),idc_voornamen_overnemen,[wsf_Auto,wsf_TabStop,wsf_Disabled,wsf_Invisible,wsf_Group]),
		 ctl(wdef(wc_RadioButton,rct(25,27,117,37),"&Voorletters overnemen",u_DlgBase),idc_voorletters_overnemen,[wsf_Auto,wsf_TabStop,wsf_Disabled,wsf_Invisible]),
		 ctl(wdef(wc_RadioButton,rct(25,37,73,47),"&Allebei",u_DlgBase),idc_allebei,[wsf_Auto,wsf_TabStop,wsf_Disabled,wsf_Invisible]),
		 ctl(wdef(wc_Text,rct(10,2,129,13),"Voornaam- en voorletterdetails",u_DlgBase),idct_spersoortindeling,[wsf_AlignLeft])
%END Invoer, WinDefList
		],dlg_invoer_eh,0).

%MARK Invoer, new events

%BEGIN Invoer, idc_ok _CtlInfo
  dlg_invoer_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	win_PostEvent(_Win,e_User(11, 0)),
	!.
%END Invoer, idc_ok _CtlInfo

%BEGIN Invoer, e_User
  dlg_invoer_eh(Win,e_User(11,_Ptr),0):-
	dialog_SetState(Win, [show(idc_progress, b_True)]),
	filenaam(Filenaam, Mode),
	getDetails(Mode, Win, Details),
	assert(details(Details)),
	ProgressWin = win_GetCtlHandle(Win, idc_progress),
	MsgWin = win_GetCtlHandle(Win, idct_spersoortindeling),
	trap(spelers_invoerITF(MsgWin, ProgressWin, FileNaam, Mode, Details), Ret, spi_result(Ret)), 
	win_Destroy(Win),
	!.
  dlg_invoer_eh(_Win,e_User(11,_Ptr),0):-
	win_Destroy(_Win),
	!.
%END Invoer, e_User

%BEGIN Invoer, e_Create
  dlg_invoer_eh(_Win,e_Create(_CreationData),0):-
	ProgressWin = win_GetCtlHandle(_Win, idc_progress),
	progress_bar_set_value(ProgressWin, 0, 0),
	dialog_SetState(_Win, [show(idc_progress, b_False)]),
	win_PostEvent(_Win,e_User(11, 0)),
	!.
  dlg_invoer_eh(Win,e_Create(_CreationData),0):-!,
	ProgressWin = win_GetCtlHandle(Win, idc_progress),
	progress_bar_set_value(ProgressWin, 0, 0),
	dialog_SetState(Win, [show(idc_progress, b_False),
	                       show(idc_ok, b_True),
	                       enable(idc_ok, b_True),
	                       show(idc_cancel, b_True),
	                       enable(idc_cancel, b_True),
	                       show(idc_voornamen_overnemen, b_True),
	                       show(idc_voorletters_overnemen, b_True),
	                       show(idc_allebei, b_True),
	                       enable(idc_voornamen_overnemen, b_True),
	                       enable(idc_voorletters_overnemen, b_True),
	                       enable(idc_allebei, b_True)
	                       ]),
	details(Details),
	setDetails(5, Win, Details),
	!.
%END Invoer, e_Create


%BEGIN Invoer, idc_help _CtlInfo
  dlg_invoer_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Spelergegevensinvoer"),
	!.
%END Invoer, idc_help _CtlInfo

%BEGIN Invoer, idc_cancel _CtlInfo
  dlg_invoer_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	win_Destroy(_Win),
	!.
%END Invoer, idc_cancel _CtlInfo


  dlg_invoer_eh(_,_,_):-!,fail.

%END_DLG Invoer





%BEGIN_DLG ToernooiLoad
/**************************************************************************
	Creation and event handling for dialog: ToernooiLoad
**************************************************************************/

constants

%BEGIN ToernooiLoad, CreateParms, 16:47:33-22.1.2011, Code automatically updated!
  dlg_toernooiload_ResID = idd_toernooiload
  dlg_toernooiload_DlgType = wd_Modal
  dlg_toernooiload_Help = idh_adresetiketten
%END ToernooiLoad, CreateParms

database - veilig
nawfix(integer, integer, integer)
single uploadToFollow(boolean)
single wijze(uploadtype)
determ toernooiload

predicates

  dlg_toernooiload_eh : EHANDLER
  dlg_toernooiload_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_toernooiload_update(DIALOG_VAL_LIST)
  open_data(WINDOW, string Caller)
  procedure fix_schema(WINDOW) 
  not_real(integer, wedscat, tgnst)
  nondeterm diversen1(string)
  showMsgs(slist)
  conversieerror(integer)
  determ asserterror(dbasedom)
  determ importnummerUitOpm(slist, string, slist, string)
%  procedure cleantoernooinaam(string, string)
  procedure vertaal(char, string)
  procedure openfout(WINDOW, integer Errorcode)
  %reduceerAlgemeelX(dbasedom)
  %reduceerEmailadreszelf(dbasedom)
  %reduceerAutoweb(dbasedom)
  %reduceerAutobak(dbasedom)
  determ reduceerFacts(dbasedom)
  determ reduceerFacts1(dbasedom)
  %reduceerDosPad(dbasedom)
  determ wdMaxNo(wedscat, integer)
  gewenstevorm(wedscat, rsoort)
%determ  introfromserver(string)
procedure dagstatusexportP(string JsonW, string JsonS)
procedure myDeleteFileMsg(string, integer) 

clauses

uploadToFollow(b_False).
wijze(update).	% update of replace	

importnummerUitOpm(NL, Opm, [Nr|Rest], OpmOut) :-
  searchstring(Opm,"Import",P1),
  P0 = P1 - 1,
  P2 = P1 + 7,
  trap(substring(Opm,P2,6,Nr), _, fail),
  P3 = P2 + 6,
  frontstr(P3,Opm,_StartString,Opm1),
  frontstr(P0, Opm, Opm0, _),
  concat(Opm0, Opm1, OpmF), % OpmF = opmerking eruit gefilterd
  importnummerUitOpm(NL, OpmF, Rest, OpmOut), !.
importnummerUitOpm(Res, Opm, Res, Opm).

fix_schema(_) :-
  %loglock("fix_schema"),
  spsrt(Sex, String, Sexe, L1, L2, S1, S2, []),
  sex_titel(Sexe, Titel),
  logf('Z', spsrt(Sex, String, Sexe, L1, L2, S1, S2, [Titel]), ja),
  fail.
fix_schema(_) :-		% reparaties .....
  wd(Num, Cat, T1, T2,A, B, Nota),
  not( wc(Cat, _, _, _, _, _, _, _, _, _, _, _, _, _, _)),
  logf('T',wd(Num, Cat, T1, T2,A, B, Nota),ja),
  fail.
fix_schema(_) :-
  pos(Cat,N1,N2,N3),
  not(wd(_,Cat,_,_,_,_,_)),
  logf('R', pos(Cat, N1, N2, N3),ja),
  fail.
fix_schema(_) :-
  separatie(Sep),
  duur(Duur),
  kleinste(Sep, Duur, Duur1),
  not(duur(Duur1)),
  logf('Z',duur(Duur1),ja),
  fail.
fix_schema(_) :-
%  retractall(nawfix(_,_,_)),
  ws(Num, Cat, Tg, Tijd),
  not_real(Num, Cat, Tg),
  logf('R', ws(Num, Cat, Tg, Tijd),ja),
  fail.
fix_schema(_) :-	% 20.12.2009 verhuis het importnummer van de opmerkingen naar de lijst
  sp2(SpNo,SexN,Naam,Tel,Verh,RatingE,RatingD,Betaald,UitK, Club, Adres, Woonpl, LidNr, Sterkte, SterkD, Geb, T2, T3, EMail, Opm,Gezien,RangPosE,RangPosD,BankNr,CheckGeg,Log,Res),
  importnummerUitOpm(Res, Opm, Imports, OpmF),
  not(Res = Imports),
  sortstring_c(Imports, 1),	% 20.12.2009  compress? ja
  logf('A', sp2(SpNo,SexN,Naam,Tel,Verh,RatingE,RatingD,Betaald,UitK, Club, Adres, Woonpl, LidNr, Sterkte, SterkD, Geb, T2, T3, EMail, OpmF,Gezien,RangPosE,RangPosD,BankNr,CheckGeg,Log,Imports), ja),
  not(compressRequired),
  assert(compressRequired),
  fail.  
fix_schema(_) :-
  getbacktrack(Here),
  wc(_, "onderdeelid", _, _, _, _, _, _, _, _, _, _, _, _, _),
  cutbacktrack(Here),
  retractall(compressRequired),
  assert(compressRequired),
  findall(WcX,   wc(WcX, "onderdeelid", _, _, _, _, _, _, _, _, _, _, _, _, _),WcXL),
  member(WC, WcXL),
  getbacktrack(Here1),
  wc(WC, _, Orde, Naam, EDG, Kort, SexL, SpStrk, LftV, LftTM, HDG, Schemasoort, _, _, Geld),
  cutbacktrack(Here1),
  logf('Z',wc(WC, "", Orde, Naam, EDG, Kort, SexL, SpStrk, LftV, LftTM, HDG, Schemasoort, vanzelf, "", Geld),ja),
  fail.
fix_schema(_) :-
  getbacktrack(Here),
  wc(_, _, _, _, _, _, SexLx, _, _, _, HDGx, _, _, _, _),
  sexlist2wedsex(HDGx, SexLx, HDGxy),
  not(HDGx = HDGxy),
  writetojournaal("Attributen voor dames, heren, gemengd toegevoegd!"),
  cutbacktrack(Here),
  retractall(compressRequired),
  assert(compressRequired),
  findall(WcX,   wc(WcX, _, _, _, _, _, _, _, _, _, _, _, _, _, _),WcXL),
  member(WC, WcXL),
  getbacktrack(Here1),
  wc(WC, OnderdeeelID, Orde, Naam, EDG, Kort, SexL, SpStrk, LftV, LftTM, HDG, Schemasoort, Status, KNLTB, Geld),
  cutbacktrack(Here1),
  sexlist2wedsex(HDG, SexL, HDG1),
  logf('Z',wc(WC, OnderdeeelID, Orde, Naam, EDG, Kort, SexL, SpStrk, LftV, LftTM, HDG1, Schemasoort, Status, KNLTB, Geld),ja),
  fail.
fix_schema(_) :-
  opg(NumX,Cat,Tgnst,A,B,C,D,E,F,G,H),
  tgnstnum(Tgnst, Num),
  not(sp2(Num,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)),
  logf('R', opg(NumX,Cat,Tgnst,A,B,C,D,E,F,G,H),ja),
  format(String, "een opgave (%u) verwijst naar een niet bestaande persoon! (opgave is verwijderd)", Num),
  writetojournaal(String, ja),
  fail.
fix_schema(_) :-	% 5.2.2005
  opg(NumX,Cat,Tgnst,A,B,C,D,E,F,G,H),
  not(wc(Cat, _, _, _, _, _, _, _, _, _, _, _, _, _, _)),
  logf('R', opg(NumX,Cat,Tgnst,A,B,C,D,E,F,G,H),ja),
  fail.
fix_schema(_) :-		% 2.3.2010
  opg(NumX,Cat,Tgnst,A,B,C,D,E,F,G,H),
  not(Tgnst = p(_,_)),
  not(Tgnst = s(_)),
  not(Tgnst = pw(_)),
  logf('R', opg(NumX,Cat,Tgnst,A,B,C,D,E,F,G,H),ja),
  fail.
fix_schema(_) :-
  findall(Tid,toernooiK(Tid,_),TidL),
  knoop(_, _Tree, 0,TidL),	% check de structuur
  fail.
fix_schema(_) :-
  wc(Cat, _, _, _, _, _, _, _, _, _, _, _, _, _, _),  
  %wc(Cat,_,_,_,_,_,_),
  getbacktrack(Here),
  wd(_, Cat, _, _, _, _, _),
  cutbacktrack(Here),
  not(pos(Cat,_,_,_)),
  wdMaxNo(Cat, Max),
  card(Max, Card),
  gewenstevorm(Cat, GewensteVorm),
  assert(pos(Cat,Card,Card,GewensteVorm)),
  not(compressRequired),
  assert(compressRequired),
  fail.
fix_schema(_) :-
  logclose().

wdMaxNo(Cat, MaxNo) :-
  findall(No, wd(No, Cat, _, _, _, _, _), NoList),
  sortint_c(NoList),
  reverse(NoList, [], NLR),
  NLR = [MaxNo | _], !.

gewenstevorm(Cat, afval) :-
  wd(1, Cat, _, _, _, _, _), !.
gewenstevorm(_, poel).

conversieerror(X) :-
  format(Uit,  "fix_schema error ergens: %d", X),
  writetojournaal(Uit, ja), !.

not_real(_, _, s(T1)) :-			% wordt bij het laden gebruikt
  not(sp2(T1,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)), !.
not_real(_, _, pw(T1)) :-			% wordt bij het laden gebruikt
  not(sp2(T1,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)), !.
not_real(_, _, p(T1,_)) :-
  not(sp2(T1,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)), !.
not_real(_, _, p(_,T2)) :-
  not(sp2(T2,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)), !.
not_real(Num, Cat, _) :-
  not(wd(Num, Cat, _, _, _, _, _)), !. 

diversen1("Toernooibestand.\nHet bestand is geopend in \"alleen lezen\" mode.\n  Veranderingen worden niet bewaard!\nJe kunt via toernooiselectie een bruikbare kopie maken.") :- alleenlezen1.
diversen1("Niet standaard instelling poulepuntentoekenning!\n. . zie Toernooi, Beleid, Poulepunten.") :- pouleTelling(PT), PT > 0.
diversen1("Autowebupload van eventuele schema's staat aan!\n. . zie Internet, Algemeen, Uploadinstellingen.") :- 
  api_getcomputername(PC), upper_lower(PCU, PC), autoweb(PCU, 1,_,_, _, _, _,_,_,_,_),  % autoupload staat aan en er zijn schema's
  getbacktrack(Here), wc(No, _, _, _CatName, _, _, _, _, _, _, _, _, _, _, _), pos(No,_,_,_), cutbacktrack(Here). 
diversen1("Uitslagen invoeren ter plaatse\n. . zie Toernooi, Beleid") :- uitslagenable(_).
diversen1("Roosterinterval 12 minuten NIET STANDAARD!!\n. . zie Toernooi, Extra instellingen") :- roosterinterval(N), N <> 15.
diversen1(Msg) :- 
  Diff = toernooidag(), Diff <  -2, overgeslagen(List), count(List, 0, Aantal), Aantal > 0,
  format(Msg, "Er zijn nog %u opgaven op internet niet verwerkt (zie Alt-X).\n(dat kunnen ook nieuwe verhinderingen zijn)", Aantal).

showMsgs([]) :- !.
showMsgs(List) :- 
  list_to_string(List, "\n", String),
  dlg_MessageBox( "Opgelet!", String, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ).

asserterror(Fact) :-
  writedevice(X),
  writedevice(screen),
  write("asserterror in tasvload",Fact),
  writedevice(X),
  fail.

vertaal(Ch, St) :-
  char_int(Ch, Int),
  Int > 64,
  Int < 91, !,
  str_char(St,Ch).
vertaal(Ch, St) :-
  char_int(Ch, Int),
  Int > 96,
  Int < 123, !,
  str_char(St,Ch).
vertaal(Ch, St) :-
  char_int(Ch, Int),
  Int > 47,
  Int < 58, !,
  str_char(St,Ch).
vertaal('\\', "-") :- !.
vertaal('/', "-") :- !.
vertaal('.', "-") :- !.
vertaal('\'', "-") :- !.
vertaal(':', "-") :- !.
vertaal(_, "").

cleantoernooinaam(In, UitX) :-
  frontchar(In,Char,Rest),
  vertaal(Char, CharT), !,
  cleantoernooinaam(Rest, Uit),
  concat(CharT, Uit, UitX).
cleantoernooinaam(In, Uit) :-
  str_len(In, Len),
  Len > 50,
  frontstr(50,In,Uit,_),
  trim_c(Uit), !.
cleantoernooinaam(InUit, InUit) :-
  trim_c(InUit).

getvpiini(VPIconf) :-
  not(usb(ja)),
  existfile(tasvpiinix),
  %write("\n ###", tasvpiinix, " bestaat nog"),
  getfolder(_F,Dir,_,_),
  %format(VPIconf, "%s\\toernooien\\%s", F, tasvpiinix), 
  filenamepath(VPIconf, Dir, tasvpiinix),
  write("\n",VPIconf),
  not(existfile(VPIconf)),
  copyfile(tasvpiinix, VPIconf),
  mydeletefile(tasvpiinix),
  fail. 
getvpiini(VPIconf) :-
  getfolder(_F,Dir,_,_),
  filenamepath(VPIconf, Dir, tasvpiinix), !.
  %write("\n ###", VPIconf, " wordt gebruikt"), !.
  %format(VPIconf, "%s\\toernooien\\%s", F, tasvpiinix).

reduceerfacts(Fact) :-
  not(reduceerfacts1(Fact)), !.
reduceerfacts(Fact) :-
  not(compressRequired),
  assert(compressRequired),
  comline(LineBuffer),
  upper_lower(LineBuffer, LBL),
  searchstring(LBL, "/x", _),
  nl,write(Fact),
  fail.

reduceerfacts1(sp2(Spno,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_)) :-
  sp2(SpNo,_,_,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !.	% 6.7.2013 als er al een sp is met dat nummer...
reduceerfacts1(ws(Nr, Cat34, Tgnst, _)) :-
  ws(Nr, Cat34, Tgnst, _), !.
%reduceerfacts1(wcrit(Wedscat, _, _, _, _)) :-
%  wcrit(Wedscat, _, _, _, _), !.
%reduceerfacts1(wc(Wedscat, _, _, _, _, _, _)) :-
%  wc(Wedscat, _, _, _, _, _, _), !.
reduceerfacts1(bn(Tijd, _)) :-
  bn(Tijd, _), !.
reduceerfacts1(dag(Dag, _, _)) :-
  dag(Dag, _, _), !.
reduceerfacts1(wd(Num, Wedscat, _, _, _, _, _)) :-
  wd(Num, Wedscat, _, _, _, _, _), !.
reduceerfacts1(pos(Wedscat, _, _, _)) :-
  pos(Wedscat, _, _, _), !.
reduceerfacts1(w3(Num, Wedscat, _, _, _, _, _)) :-
  w3(Num, Wedscat, _, _, _, _, _), !.
reduceerfacts1(w2(Num, Wedscat, _, _, _, _, _, _)) :-
  w2(Num, Wedscat, _, _, _, _, _, _), !.
reduceerfacts1(opg(Num, Wedscat, _, _, _, _, _, _, _, _, _)) :-
  opg(Num, Wedscat, _, _, _, _, _, _, _, _, _), !.
reduceerfacts1(kopl(Cat, Pos, CatV, _)) :-
  kopl(Cat, Pos, CatV, _), !.
reduceerfacts1(algemeelX(MT,_,_,_,_)) :-
  algemeelX(MT,_,_,_,_), !.
reduceerfacts1(autoweb(PC, _,_,_, _, _, _, _, _, _, _)) :-
  autoweb(PC, _,_,_, _, _, _, _, _, _, _), !.
reduceerfacts1(emailadresZelf(PC, _, _)) :-
  emailadresZelf(PC, _, _), !.
reduceerfacts1(autobak(MT,_)) :-
  autobak(MT,_), !.
reduceerfacts1(club(Cl,_,_)) :-
  club(Cl,_,_), !.
reduceerfacts1(weblayout(Beeld, _, _, _, _, _, _)) :-
  weblayout(Beeld, _, _, _, _, _, _), !.
reduceerfacts1(spsrt(Sex, _, _, _, _, _, _, _)) :-
  spsrt(Sex, _, _, _, _, _, _, _), !.
reduceerfacts1(toernooiK(Nr, Type)) :-
  toernooiK(Nr, Type), !.

myDeletefile(FileNaam) :-
  trap(deletefile(FileNaam), ErrNo, myDeletefileMsg(FileNaam, Errno)), !.
myDeletefile(_FileNaam).

myDeleteFileMsg(File, Nr) :-
  format(Msg, "Error deleting %s\nErrorCode %u", File, Nr),
  dlg_note(Msg).

introfromServer(Rest1) :-
  Vars = aanmeldenVars(),
  providerX(Provider, _, "", _),
  format(URL, "http://%s/intro2016.php", Provider),
  %write("\n", URL),
  get_tempDir(TDir, _TempBestand),
  filenamePath(TempFile, TDir, "intro.txt"),
  myDeletefile(Tempfile),
  Error = downloadData(URL, Tempfile, Vars, _Retcode, 0),
  file_str(Tempfile, Str),
  %write("\n",Error, " ", Str),
  checkServer(Error),
  searchstring(Str, "NB!", Pos),
  %write(Pos),
  Pos1 = Pos + 3,
  frontstr(Pos1,Str,_,Rest),
  concat("\n", Rest, Rest1), !.
  %_Answer = dlg_MessageBox( "Programmastatus", Rest, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), !.

open_data(_Win, _Debug) :-
  cursor_SetWait(),
  closefile(work),
  closefile(nawf),
  get_toernooidir(_, _, _, TB),
  format(Msg, "Openen: %s", TB),
  updatetoernooidirsave(),
  VPIconf = getVPIini(),
  format(SaveErr, "opslaan van bestand op %s mislukt!", VPIconf),
  trap(save(VPIconf), _, dlg_Error(SaveErr)),
  writeToJournaal(Msg),
  filenamePath(TB, Path, _),
  filenamepath(Logfile, Path, logconst),	% 22.8.2010
  mydeletefile(Logfile),	% 9.2.2011
  vpi_ProcessEvents( b_True ),
  cleanfacts,	% maak geheugen schoon, single facts uitgezonderd
  retractall(_, operationeel),
  getOpeationeel(_Win), % & statusN
  %retractall(_, opgcheck),		% keuringsgegevens
  retractall(_, local), %factPos(_)),
  not(myOpenread(0, work, TB)),
  !.			% doe niks!!
open_data(_Win, _) :-
  retractall(webdatpositie(_,_, _)),	% 31.8.2008
  retractall(remoteCompressTookPlace),
  retractall(dataverdacht(_)),
  ProgressWin = win_GetCtlHandle(_Win, idcl_load_data),
  filePos(work,1,2),
  filePos(work, Len, 0),
  filePos(work, 0, 0),
  repeat_f(work),
  readdevice(work),
  filePos(work, Current, 0),
  trap(readterm(dbasedom,Fact),_,cons_err('Z', Current)),
  not(Fact = webdatpositie(_,_,_)),  % niet in base file, alleen in log
  reduceerFacts(Fact),
  trap(assert(Fact), _, asserterror(Fact)),
  Current mod 1000 < 100,
  progress_bar_set_value(ProgressWin, Len, Current),
  vpi_ProcessEvents(b_True ),
  fail.
open_data(_Win, _) :-
  closefile(work),
  assert(tasversie(versie)),
  retract(dataverdacht(_)),
  vpi_Alarm(mesbox_iconerror),
  M1 = "Opgelet!\nEr zijn fouten opgetreden bij het laden van de toernooigegevens.",
  M2 = "\nInspecteer de inhoud zorgvuldig!",
  format(Msg, "%s%s", M1, M2),
  dlg_MessageBox( "", Msg, mesbox_iconerror, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.
open_data(_Win, _Debug) :-	% 23.11.2010  Versie 2011
  get_toernooidir(_Dir, _, Mode, TB),
  Mode = disk,
  filenameext(TB, _Main, Ext),
  upper_lower(Ext, ".dat"),
  Msg = "De de toernooimap wordt naar een enkel bestand geconverteerd.",
  dlg_MessageBox( "Nieuwe versie TA", Msg, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  naam(ToernooiNaam, _, _,_,_,_),
  cleantoernooinaam(Toernooinaam, ToernooinaamC),
  filenameext(TBN, ToernooinaamC, ".tas"),
  getfolder(_Doc,DocT,_,_),
  Filename = dlgFileNameZeker(TBN,["toernooibestand","*.tas"],"Toernooibestand opslaan",[dlgfn_Save],DocT,_OutListFiles),
  filenamepath(Filename, NPad, _),
  filenamepath(NFilename, NPad, TBN),
  set_toernooidir(NPad, NFilename),
  format(SaveErr, "kopiëren naar %s mislukt!", NFilename),
  mydeletefile(NFilename),
  trap(copyfile(TB, NFilename), _, dlg_Error(SaveErr)),
  mydeletefile(TB),
  fail.
open_data(_, _) :-
  get_toernooidir(_Dir, _, Mode, TB),
  Mode = disk,
  filenamepath(TB, Path, _),
  voegtoeaantoernooidirs(Path),
  fail.
open_data(Win, _) :-
  retractall(logIsOpen(_)),
  updatetoernooidirsave(),
  VPIconf = getvpiini(),
  format(SaveErr, "kopiëren naar %s mislukt!", VPIconf),
  trap(save(VPIconf, vpiconf), _, dlg_Error(SaveErr)),
  get_toernooidir(_Dir, _, _, TB),
  %fix_schema(Win),
  trap(fix_schema(Win),X,conversieerror(X)),		% pas oude patches en nieuwe conversie toe
  myOpenModify(logfile,TB),
  filepos(logfile, 0, 2),		% achteraan
  %filepos(logfile, 0, 2),		% achteraan nog een keer
  filepos(logfile, PosX, 0),
  closefile(logfile),
  retractall(logIsOpen(_)),
  retractall(logpos(_)),
  assert(logPos(PosX)),
  setIniPos(PosX),
  cursor_SetWait(),
  findall(Msg, diversen1(Msg), MsgList),
  showMsgs(MsgList),
  update_proj_toolbar(),
  %introFromServer(),   % nieuw 	18.8.2013
  spelerSelRefresh(),
  setNaam(),
  fail.
open_data(_Win, _) :-	% 22.10.2010
  not(toernooiload),
  webdir(_, inet, _),
  not(webdatpositie(_, _, _)),
  date(Year,Month,Day),
  time(Hours,Minutes,Seconds,_Hundredths),
  format(Timestamp, "%u-%u-%u %u:%u:%u", Year, Month, Day, Hours, Minutes, Seconds),
  logf('Z',webdatpositie(Timestamp, 0, 0), nee),
  fail.
open_data(_Win, _) :-
  logPos(Pos),
  logposini(IniP),
  X = Pos - IniP,
  %write("\ndelta: ", X),
  X > 1000, 	% ---- moet 5000 zijn
  write(" gaat comprimeren!"),
  not(compressRequired),
  assert(compressRequired),
  fail.
open_data(_Win, _) :-
  %getExpatVersion(),
  retract(compressRequired), !,
  compress(update),
  startDBrefresh().
open_data(_Win, _).

openfout(Win, Errorcode) :-
  webdir(_, Lok, WB),
  toernooidir(Pad, Bestand),
  tasversie(Versie),
  format(Msg,"Er is een fout opgetreden bij het laden! (%s)\r\n\r\nHerstel de netwerkverbinding of probeer via toernooiselectie het gewenste toernooi te laden.\r\n", Versie),
  term_str(lokatie, Lok, LokStr),
  format(Msg1, "Als u deze fout blijft houden, meld dan svp aan support\r\nPad: %s\r\nToernooibestand:\r\n %s\r\nof\r\n%s\r\nLokatie: %s", Pad, Bestand, WB, LokStr), 
  concat(Msg, Msg1, Msg2),
  lasterror(_ErrorNo,FileName,_IncludeFileName,Position),
  format(Msg3, "\r\nin bestand: %s\r\nPositie: %u", Filename, Position),
  concat(Msg2, Msg3, Msg4), 
  syspath(ExeStartupPath,_ProgName),
  filenamepath(Errorfile, ExeStartupPath,"prolog.err"),  
  errormsg(Errorfile ,Errorcode,ErrorMsg,_ExtraHelpMsg),
  format(Msg5, "\r\n%s", Errormsg),
  concat(Msg4, Msg5, Mess),
  cb_PutString(Mess),
 _Answer = dlg_MessageBox( "Laden mislukt", Mess, mesbox_iconerror, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
 win_Destroy(Win).
  
  dlg_toernooiload_Create(Parent, _Debug, UploadIf, UpdateReplace):-
  	retractall(geladen),
	assert(wijze(UpdateReplace)),
	assert(uploadToFollow(UploadIf)),
	%format(DM, "dlg_toernooiload_Create opgeroepen door %s", Debug),
  	%writeToJournaal(DM),
%MARK ToernooiLoad, new variables

	dialog_CreateModal(Parent,dlg_toernooiload_ResID,"",
  		[
%BEGIN ToernooiLoad, ControlList, 16:47:33-22.1.2011, Code automatically updated!
%END ToernooiLoad, ControlList
		],
		dlg_toernooiload_eh,UploadIf,VALLIST,ANSWER),
	dlg_toernooiload_handle_answer(ANSWER,VALLIST).

  dlg_toernooiload_handle_answer(idc_ok,VALLIST):-!,
	dlg_toernooiload_update(VALLIST).
  dlg_toernooiload_handle_answer(idc_cancel,_):- !.
  dlg_toernooiload_handle_answer(_,_):-
	errorexit().

  dlg_toernooiload_update(_VALLIST):-
%BEGIN ToernooiLoad, Update controls, 16:47:33-22.1.2011, Code automatically updated!
%END ToernooiLoad, Update controls
	true.

%MARK ToernooiLoad, new events

%BEGIN ToernooiLoad, e_User
  dlg_toernooiload_eh(_Win,e_User(1,_UploadIf),0):-
	retractall(toernooiload),
	assert(toernooiload),
	stopDBrefresh(),
	trap(open_data(_Win, "ToernooiLoad"),Errcode, openfout(_Win,Errcode)),
	ProgressWin1 = win_GetCtlHandle(_Win, idcl_load_data),
	progress_bar_set_value(ProgressWin1, 100, 100),
	retract(toernooiload),
	retractall(geladen),
	assert(geladen),
	%get_toernooidir(_Dir, _, _, _),
	%format(CmdString, "attrib -R \"%s*.*\" /S", Dir),
	%write("\n", CmdString),
	%system(CmdString,0,_RetCode),
	%write("\n", _RetCode),
	startDBrefresh(),
	win_Destroy(_Win),
	showFirst(),
	assert(updateNodigFlag(b_True)),
	doUpdateVensters(), 
 	kladblokPopup(),
 	kladblokrefresh(),
 	uploadToFollow(b_True),
        webdir(Map, Lokatie, _TB),
        wijze(UpdateReplace),
        copyNaarWebIf(Map, Lokatie, UpdateReplace), !.
%        fail.
%END ToernooiLoad, e_User

%BEGIN ToernooiLoad, e_Create
  dlg_toernooiload_eh(_Win,e_Create(_CreationData),0):-!,
	ProgressWin1 = win_GetCtlHandle(_Win, idcl_load_data),
 	progress_bar_set_value(ProgressWin1, 0, 0),
	progress_bar_set_colors(ProgressWin1,color_Green,color_White,pat_Hollow),
	win_PostEvent(_Win,e_user(1,0)),
	!.
%END ToernooiLoad, e_Create

  dlg_toernooiload_eh(_,_,_):-!,fail.

%END_DLG ToernooiLoad

dagstatusexportP(JsonW,JsonS) :-
  retract(dagstatusexport(JsonW,JsonS)), !.
dagstatusexportP("","").

dagstatusUploadX("", Dag) :-
  dag(Dag,DSel,_),
  dagrapport1(DSel),
  fail.
dagstatusUploadX("", _Dag) :-
  retractall(dagstatusQueued(_,_)),
  fail.
dagstatusUploadX(_, Dag) :-
  %comline(LineBuffer),
  %upper_lower(LineBuffer, LBL),
  %not(searchstring(LBL, "/x", _)),
  get_Webmap(Dir),
  filenamepath(File, Dir, "dagstatus.html"),
  get_tempDir(TDir, _TempBestand),
  filenamePath(TempFile, TDir, "feedback.txt"),
  providerX(Provider, _, "", _),
  format(URL, "http://%s/dagmonitor.php", Provider),
  X = aanmeldenvars(),
  dt_dateoffset_to_str(Dag, "%YL-%MD-%DD",Datum),
  %dagstatusVars(VandaagOfMorgen, X, AAList),
  dagstatusexportP(InhoudW,InhoudS),
  BBList = ["dag", Datum, "refresh", "ja", "wjson", InhoudW, "sjson", InhoudS | X],
  concat(File, ".gz", Filec),
  file_compress(File, FileC),
  _Return = fileupload(URL, FileC, TempFile, BBList),  % compress
  fail.
dagstatusUploadX(_File, _).

