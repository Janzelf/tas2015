/*****************************************************************************

		Copyright (c) 1989 - 1998 De Lint Associates

 Project:  TASVP1
 FileName: TASETIK.PRO
 Purpose: No description
 Written by: JdL
 Comments:
******************************************************************************/

include "tasvp1.inc"
include "tasvp1.con"
include "hlptopic.con"
include "tools\\lbpgview.pre"
include "tools\\lbpgview.pro"

% ############# het oude tasetik0
database - temp

nondeterm incl_sp(integer)
  etiket(integer Nr, slist)
single aantal_etik(integer)
single opt_etik_oproep(integer Opt_postc,BOOLEAN Opt_z_adres,BOOLEAN Opt_z_uitslt,
	BOOLEAN Opt_nog_waars,BOOLEAN Opt_spaarstand,
	BOOLEAN Opt_vervolg_dag, BOOLEAN Opt_vervolg_alle, BOOLEAN Opt_toernooinaam,
	BOOLEAN Emailapart)
single etiketRegels(integer)
determ mailmerge(integer)	% 0 = echte mailmerge 1 = oproeplijst 
single current(integer)
single currentE(integer)

clauses

current(0).
currentE(0).
aantal_etik(0).


etik_reset :-
  assert(currentE(0)),
  retractall(etiket(_, _)),
  assert(aantal_etik(0)), !.

% ################# prnt de etiketten vanuit de tempfile

predicates
printetikettenA(WINDOW Printwin, janee TesttFrame) - (i, i)
procedure pSList(WINDOW, slist, RCT, integer)
procedure pStart(integer, integer, WINDOW, FONT)
procedure regelOverflow(integer Curr, integer Max, slist In, slist Nieuw, slist Uit, slist Rest)
procedure etikPreview(integer Aantal)
procedure nlDep(integer Regel)
procedure checkRegelsPE(integer)
procedure testFrame(WINDOW, RCT, janee)

clauses

checkRegelsPE(RegelsPE) :-
  RegelsPE < 3, !,
  dlg_MessageBox( "Attentie", "te weinig regels per etiket\nzie File, Etiketformaat", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  closefile(work),
  exit(11).
checkRegelsPE(_).
  

etiketBuild([]) :- !.
etiketBuild(SList) :-
  etiketwaarden(_, _,_,_,_,_,_,_, RegelsPE,_),
  checkRegelsPE(RegelsPE),
  currentE(N),
  N1 = N + 1,
  assert(currentE(N1)),
  regelOverflow(-1, RegelsPE, SList, [], MaxList, Rest),
  assert(etiket(N, MaxList)),
  etiketBuild(Rest).

regelOverflow(Max, Max, Rest, In, In, Rest) :- !.	% max is bereikt
regelOverflow(N, Max, [H|Rest], In, [H|Uit], RestU) :- 
  N1 = N + 1, !,
  regelOverflow(N1, Max, Rest, In, Uit, RestU).
regelOverflow(_, _, _, In, In, []) :- !.		% slist is op

pSlist(PrintWin, [H|T], PRCT, Height) :-
  draw_TextInRect(PrintWin,PRCT, H, -1, [dtext_left]),
  PRCT1 = rect_Offset(PRCT, 0, Height), !,
  pSlist(PrintWin, T, PRCT1, Height).
pSlist(_, _, _, _).

printetiketten(idc_ok, TestFrame) :- !,
  loadPrintConfig1(),
  PrintWin = print_StartJob("Etiketten"),
  printetikettenA(PrintWin, TestFrame),
  print_EndJob(PrintWin).
printetiketten(_, _).

pStart(0, _, _PrintWin, _) :- !.
pStart(_, 0, PrintWin, Font) :-
  print_EndPage(PrintWin),
  print_StartPage(PrintWin),
  win_SetFont(PrintWin, Font), !.
pStart(_, _, _PrintWin, _).

printetikettenA(PrintWin, TestFrame) :-
  print_StartPage(PrintWin),
  etiketwaarden(r(BovenMarge), r(LinkerMarge),r(Breedte),r(Hoogte),r(HorSpace),
  					r(VertSpace),i(AantalPKol),i(AantHor), _RegelsPE,FontG),
  AantalPP = AantalPKol * AantHor,
  SVRes = vpi_GetAttrVal(attr_screen_vres),
  PVres = vpi_GetAttrVal(attr_printer_vres),
  Size1 = val(integer,(FontG * PVres / SVRes)),
  Font = font_Create(ff_Times,[],Size1),
  win_SetFont(PrintWin, Font),
  win_GetTextExtent(PrintWin, "JJansenSchoonhoveng",-1, _Width,Heigth),
  PHres = vpi_GetAttrVal(attr_printer_hres),
  PHResMM = PHres / 25.4,
  X0 =  val(integer , (LinkerMarge * PHResMM)),
  Xetik =  val(integer, Breedte  * PHResMM),
  Xdelta =  val(integer, (Breedte + HorSpace)  * PHResMM),
  PVres = vpi_GetAttrVal(attr_printer_vres),
  PVResMM = PVRes / 25.4,
  Y0 = val(integer, Bovenmarge * PVResMM),
  Ydelta = val(integer,(Hoogte + VertSpace) * PVResMM),
  assert(current(0)),
  retract(etiket(_, SList)),		% ga alle etiketten langs
  current(Nr),
  Nr1 = Nr + 1,
  assert(current(Nr1)),
  A = Nr mod AantalPP,
  pStart(Nr, A, PrintWin, Font),	% nieuwe bladzijde ?
  Rij = A div AantHor,
  Y1 = Y0 + Ydelta * Rij,
  Y2 = Y1 + Heigth, 
  Kol = A mod AantHor,
  X1 = X0 + Kol * Xdelta,
  X2 = X1 + Xetik,
  PRCT = rct(X1,Y1,X2,Y2),
  YE = Y1 + val(integer, Hoogte * PVResMM),
  TRCT = rct(X1, Y1, X2, YE), 
  testFrame(PrintWin, TRCT, TestFrame),
  pSlist(PrintWin, Slist, PRCT, Heigth), % print het etiket in het vakje
  fail.
printetikettenA(PrintWin, _) :-
  print_EndPage(PrintWin), !.

testframe(PrintWin, TRCT, ja) :-
  win_SetBrush(PrintWin,brush(pat_hollow, color_Black)),
  draw_Rect(PrintWin,TRCT), !.
testframe(_PWin, _TRCT, _).

nlDep(0) :- !.
nlDep(_) :-
  write("----------------------------------\n").

etikPreview(_) :-
  closefile(work),
  get_TempDir(_, TempFile),
  openwrite(work,TempFile),
  writedevice(work),
  etiket(Nr, Slist),
  nlDep(Nr),
  member(String, Slist),
  write(String, "\n"),
  fail.
etikPreview(N) :-
  findall(Nr, etiket(Nr, _), List),
  count(List, 0, N),	
  assert(aantal_etik(N)),
  closefile(work).
  

% ########################### adresetiketten ##############################
%
% #########################################################################

PREDICATES

nondeterm  single_mem(string, slist)
	opgesteld(wedscat, tgnst)
	procedure adresMMrecords(slist Namen)
	determ isFamilie(wedscat, catl)

clauses

single_mem(Naam1, [Naam,Naam|Namen]) :-		% zeef de dubbele
  !,
  single_mem(Naam1, [Naam|Namen]).
single_mem(Naam, [Naam|_]).
single_mem(Naam, [_|Namen]) :-
  single_mem(Naam, Namen).

sum([H|Tail],InSom,UitSom,InString,UitString) :-
  wc(H, _OnderdeeelID,_Orde,  _Naam, _EDG, Abbr, _SexL, _SpStrk, _LftV, _LftTM, _HDG, _Schemasoort, _Keuring, _KNLTB, Contr), !,
  %wc(H,_,_,Abbr,_,Contr,_), !,
  InSom1 = Contr + InSom,
  concat(" ",Abbr,Abbr1),
  concat(InString,Abbr1,InString1),
  sum(Tail,InSom1,UitSom,InString1,UitString).
sum(_,Som,Som,String,String).

sp_cat(SpNo,Cat) :-
  opg(_,Cat,s(SpNo),_,_,_,_,_,_,_,_),
  opgesteld(Cat, s(SpNo)).
sp_cat(SpNo,Cat) :-
  opg(_,Cat,pw(SpNo),_,_,_,_,_,_,_,_),
  opgesteld(Cat, pw(SpNo)).
sp_cat(SpNo,Cat) :-
  opg(_,Cat,p(SpNo,_),_,_,_,_,_,_,_,_),
  opgesteld(Cat, p(SpNo,_)).
sp_cat(SpNo,Cat) :-
  opg(_,Cat,p(_,SpNo),_,_,_,_,_,_,_,_),
  opgesteld(Cat, p(_,SpNo)).

isFamilie(Cat1, CatL) :-
  wc(Cat1, _,_,  _, EDG, Abbr, SexL, SpStrk, LftV, leeftijd(_,LftTM), _, _Schemasoort1, _, _, _),
  member(Cat2, CatL),
  wc(Cat2, _,_,  _, EDG, Abbr, SexL, SpStrk, LftV, leeftijd(_,LftTM1), _, _Schemasoort2, _, _, _),
  matchBovenleeftijd(LftTm, LftTm1), !.

zeefFamilie([Top|Tail], In, CatsU) :-
  bovencat(Top, Top1),
  isFamilie(Top1, In),
  zeefFamilie(Tail, In, CatsU), !.
zeefFamilie([Top|Tail], In, CatsU) :-
  bovencat(Top, Top1),
  zeefFamilie(Tail, [Top1|In], CatsU), !.
zeefFamilie(_, CatsU, CatsU).

opgesteld(Cat, T) :-
  wd(_,Cat,T,_,_,_,_), !.
opgesteld(Cat, T) :-
  wd(_,Cat,_,T,_,_,_), !.

etikettenAdres1([],_) :- !.
etikettenAdres1(Namen,2) :-
  getFolder(Dir,_,_,_),
  Best = dlg_GetFileName("adresgegevens.txt",["txt","*.txt","alle","*.*"],"Mailmergebestand opslaan",[dlgfn_Save],Dir,_OutListFiles),  
  closefile(work),
  openwrite(work, Best),
  writedevice(work),
  adresMMrecords(Namen),
  closefile(work),
  writedevice(screen), !.
etikettenAdres1(Namen,1) :-	% echte etiketten
  etik_reset(),
  assert(current(0)),
  single_mem(Naam, Namen),
  current(Cur),
  Cur1 = Cur + 1,
  assert(current(Cur1)),
  sp2(_,SpelerSrt,Naam,_,_,_,_,_,_,_Club, Adres, Woonpl, _LidNr, _SterkS, _SterkD, _Geb, _Telwerk, _Telmobiel, _EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Res),
  naamscan(ja, b_False,Naam, Naam1,_Voornaam,_Landcode),
  spsrt(Spelersrt,_/*Spelersrtnaam*/,_Sex,_,_,_,_,[STitel|_]),
  format(NStr, " % %", STitel, Naam1),
  concat(" ", Adres, Adres1),
  concat(" ", WoonPl, WoonPL1),
  etiketBuild([NStr, "", Adres1, WoonPl1]),		% 9.3.2001
  fail.
etikettenAdres1(_Namen, 1) :-
  TaskWin = vpi_GetTaskWin(),
  dlg_etiketten_print_Create(TaskWin, Answer),  %dlg_rapport_Create(Win, "Oproepetiketten"),
  printetiketten(Answer, nee).

adresMMrecords(Namen) :-
  assert(current(0)),
  write("mv",csvconst,"naam",csvconst,"adres",csvconst,"woonpl",csvconst,"club",csvconst,
    "district",csvconst,"email",csvconst,"sterks",csvconst,"sterkd",csvconst,"geb", csvconst, "banknummer\n"),	% 21.8.2001
  single_mem(Naam, Namen),
  current(Cur),
  Cur1 = Cur + 1,
  assert(current(Cur1)),
  sp2(_,SpelerSrt,Naam,_,_,_,_,_,_,Club, Adres, Woonpl, _LidNr, SterkS, SterkD, Geb, _Telwerk, _Telmobiel, EMail, _Opm,_Gezien,_RangPosE,_RangPosD,Incasso,_CheckGeg,_Log,_Res),
  naamscan(ja,b_False,Naam, Naam1,_Voornaam,_Landcode),
  clubP(Club, ClubNaam, ClubDistrict),
  spsrt(Spelersrt,_/*Spelersrtnaam*/,_Sex,_,_,_,_,[STitel|_]),
  writef("\"%\"%c\"%\"%c", STitel, csvconst, Naam1, csvconst),
  writef("\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"\n",
    Adres, csvconst,WoonPl, csvconst,ClubNaam, csvconst,ClubDistrict, csvconst, Email,
    csvconst, SterkS, csvconst, SterkD, csvconst, Geb, csvconst, Incasso),
  nl,
  fail.
adresMMrecords(_Namen).

% ################# betaaletiketten ##############################
% 
% ################################################################
constants

doe_niet_mee_const = "Niet-spelers"
niet_betaald_const = "Inschrijfgeld openstaand"

database - temp2  
  heeftBetaald(integer, string IBAN)
  heeftGeenInschrijfgeld(string CatNaam)
  single betCount(integer)

predicates
  etikettenBetaal1(WINDOW, Slist Selectie, integer Orde)
nondeterm betaal_namen(WINDOW Progress, slist, string, integer)
procedure	zonder_postcode2(integer, string, string)
	sp_in_groep1(string, persno, sex)
	met_postcode2(integer, string, string, string)
	betaalMMrecords(slist, boolean BetIncl, integer Aantal)
nondeterm betsp(integer)
%procedure betaalMailmerge()
determ bepaalbankkode(string, string, string Naam)
procedure toString(slist, string, integer)
nondeterm bankcode(string, string)
procedure detectGeenInschrijfgeld(catl)
procedure messageGeenInschrijfgeld(slist, string)
procedure explode(string Separator, string In, slist Uit)
procedure removeheeftbetaald(slist)
determ    jaNeeBetaaldIncl(janee, boolean)
procedure testNieuw(string)

clauses

betCount(0).

etikettenBetaal(Win):-
  dlg_etiketten_betaal_Create(Win, _Selectie, _Orde), !.
etikettenBetaal(_).

etikettenBetaal1(_,[], _) :- !.
etikettenBetaal1(Progress,Gekozen_Groepen, Orde) :-
  not(mailmerge(_)),
  findall(N1, betaal_namen(Progress, Gekozen_Groepen, N1, Orde), Namen),
  sortstring_c(Namen, 1),
  etik_reset(),
  assert(current(0)),
  single_mem(NaamR, Namen),
  zonder_postcode2(Orde, NaamR, Naam),
  sp2(SKeus,SpelerSrt,Naam,_,_,_,_,nee,_, _, _, _, _, _, _, _, _, _, _, _,_,_,_,_,_,_,_),
  spsrt(Spelersrt,_,_Sex,_,_,_,_,[STitel|_]),
  naamscan(ja,b_False,Naam,Naam1,_Voornaam,_Landcode),
  format(StrN, " % %", STitel, Naam1),
  findall(Cat,sp_cat(SKeus,Cat),Catten),
  not(Catten = []),
  sum(Catten,0.0,Som,"",String),
  Som > 0.0, 
  current(Cur),
  Cur1 = Cur + 1,
  assert(current(Cur1)),
  format(BetSom, " voldaan eur %6.2f", Som),
  etiketBuild([StrN, "", String, BetSom]),
  fail.
etikettenBetaal1(Win, _Groepen, _Orde) :-
  closefile(work),
  not(mailmerge(_)),
  dlg_etiketten_print_Create(Win, MAnswer),  %dlg_rapport_Create(Win, "Oproepetiketten"),
  printetiketten(MAnswer, nee), !.
etikettenBetaal1(_Win, _Groepen, _Orde) :-
  closefile(work).

%betaalMailmerge() :-
%  _Answer = dlg_MessageBox( "Betaalincasso", "Zorg eerst dat alle bedragen goed staan in Onderdeel Kenmerken!", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
%  fail.

testnieuw("http://www.toernooiklapper.nl/inc-ng1/indexN.php").
/*testnieuw("http://www.toernooiklapper.nl/inc-ng1/indexN.php") :- 
  comline(LineBuffer),
  upper_lower(LineBuffer, LBL),
  searchstring(LBL, "/x", _), !.		% in TEST
testnieuw("http://tas.toernooiklapper.nl/inc-ng/indexN.php").*/



betaalIncassoProef() :-
  Vars = aanmeldenVars(),
  testNieuw(URL),
  get_Tempdir(_Tdir, TempBestand),
  filenameext(Tempbestand, Namexy, _),
  filenameext(HetFrame, Namexy, ".html"),
  deletefile(HetFrame),
  Vars1 = ["stap1", "x", "proefincasso", "ja" | Vars],
  downloadData(URL, HetFrame, Vars1, _HttpRetcode, 0), % stap 1 isset(vrs) 
  BeheerId = visBEHEERid(HetFrame),
  format(Session,"%s?%s", URL, BeheerId),  % stap 2 isset(BEHEER)
  Null = cast(api_hwnd,0),
  api_ShellExecute(Null,"open",Session,"","",1),
  fail.
betaalIncassoProef().

betaalIncasso(BetaaldIncl) :-
  findall(N1,   sp2(SpNum,_,N1,_,_,_,_,_,_, _, _Adres, _Woonpl, _, _, _, _, _, _, _, _,_,_,_,_,_,"akkoord",_), Spelers),
  sortstring_c(Spelers, 1),
  %get_Toernooidir(TDir, _, _),
  %getfolder(TDir,_,_,_),
  naam(TNaam, _ToernooiNrs, _, _, _, _),
  cleanToernooinaam(Tnaam, Tnaam1),
  format(FnaamIn, "betaal%s.txt",Tnaam1),
  get_webmap(Pad),
  filenamePath(Filename, Pad, FnaamIn),
  Tnaam1 <> fctconst,
  %closefile(work),
  stopdbrefresh(),
  openwrite(work, Filename),
  writedevice(work),
  trap(betaalMMrecords(Spelers,BetaaldIncl,Aantal),_, fail),
  closefile(work),
  logclose(),
  close_betaal(),
  compress(replace),
  startDbRefresh(),
  file_str(Filename, BStr),
  Vars = aanmeldenVars(), 
  testnieuw(IURL),
  %IURL = "http://tas.toernooiklapper.nl/incasso/index.php",
  get_Tempdir(_Tdir, TempBestand),
  filenameext(Tempbestand, Namexy, _),
  filenameext(HetFrame, Namexy, ".html"),
  deletefile(HetFrame),
  Vars1 = ["stap1", "x", "transacties", BStr  | Vars],
  downloadData(IURL, HetFrame, Vars1, _HttpRetcode, 0), % stap 1 isset(vrs) 
  file_str(HetFrame, Strr),
  BeheerId = visBEHEERid(HetFrame),
  explode("\n",Strr, Sexp),
  removeheeftbetaald(Sexp),
  %dlg_note("TASETIK", Strr),
  %format(Session,"http://tas.toernooiklapper.nl/incasso/index.php?%s", BeheerId),  % stap 2 isset(BEHEER)
  format(Session,"%s?%s", IURL, BeheerId),  % stap 2 isset(BEHEER)
  Null = cast(api_hwnd,0),
  api_ShellExecute(Null,"open",Session,"","",1),
  format(Tekst, "%d betaalrecords aangemaakt in bestand %s.\nDat hoef je niet te gebruiken want je wordt nu met record en al gelinkt naar ons eigen incassoprogramma in de Internet Explorer.\n",Aantal, Filename),
  %writeToJournaal(Tekst),
  findall(CatNI, heeftGeenInschrijfgeld(CatNI), CatNIL),
  messageGeenInschrijfgeld(CatNIL, Msg),
  format(Tekst1, "Als ingedeeld is vind je in het betalingenscherm welke spelers overgeslagen zijn.\n\n%s\n%s", Msg, Tekst),
  _Answer = dlg_MessageBox( "Betaalrecords", Tekst1, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.
betaalIncasso(_) :-
  retract(heeftBetaald(SKeus, _Iban)),
  sp2(SKeus,SpelerSrt,Naam,Tel,Verh,RatE,RatD,nee,UitKant, ClubNo, Adres, Woonpl, LidNr, SterkS, SterkD, Geb, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Inschr),
  cursor_SetWait(),
  logf('Z',sp2(SKeus,SpelerSrt,Naam,Tel,Verh,RatE,RatD,ja,UitKant, ClubNo, Adres, Woonpl, LidNr, SterkS, SterkD, Geb, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Inschr),ja),
  fail.
betaalIncasso(_) :-
  logclose(),
  closefile(work),
  startdbrefresh(),
  retractall(heeftBetaald(_,_)),
  retractall(heeftGeenInschrijfgeld(_)).

explode(SearchStr, In, [RStr|Uit]) :-  % explode alleen getest voor "\n" als separator
  searchstring(In,SearchStr,FoundPos),
  FP = FoundPos - 1,
  frontstr(FP,In,RStr,RestString),
  frontstr(1,RestString,_,In1), !,
  explode(SearchStr, In1, Uit).
explode(_SearchStr, In, [In]) :-
  fronttoken(In, _, _), !.
explode(_, _, []).

removeHeeftBetaald([]) :- !.
removeHeeftBetaald(List) :-
  write("\n", List, "\n"),
  count(List,-1,Count),
  format(Msg, "Er zijn %u IBAN nummers ongeldig. Die spelers blijven in de betaallijst.", Count),
  _Answer = dlg_MessageBox( "IBAN nummers", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  member(Rec, List),
  fronttoken(Rec, Iban, _),
  retract(heeftbetaald(_, Iban)),
  fail.
removeHeeftBetaald(_).

messageGeenInschrijfgeld([], "") :- !.
messageGeenInschrijfgeld([CatNaam], Msg) :-
  format(Msg, "Attentie:\nde volgende categorie heeft geen inschrijfgeld gedefiniëerd (zie Alt K):\n %s",CatNaam), !.
messageGeenInschrijfgeld(CatL, Msg) :-
  count(CatL,0, Count),
  Count < 8,
  list_to_string(CatL, "\n  ", List),
  format(Msg, "Attentie:de volgende categorieën hebben geen inschrijfgeld (zie Alt K)\n  %s",List), !.
messageGeenInschrijfgeld(_, "Attentie: er hebben veel categorieën geen inschrijfgeld gedefiniëerd! (zie Alt K)").

bankcode("RBRB","RBRBNL21").
bankcode("INGB","INGBNL2A").
bankcode("RABO","RABONL2U").
bankcode("ABNA","ABNANL2A").
bankcode("TRIO","TRIONL2U").
bankcode("SNSB","SNSBNL2A").
bankcode("FVLB","FVLBNL22").
bankcode("ASNB","ASNBNL21").
bankcode("KNAB","KNABNL2H").
bankcode("BUNQ","BUNQNL2A").
bankcode("INSI","INSINL2A"). 

bepaalbankkode(IBAN, Bic2, _) :-
  bankcode(Bic1, Bic2),
  searchstring(IBAN, Bic1, _), !.
bepaalbankkode(IBAN, "blanco",_) :-
  not(fronttoken(IBAN, _,_)), !, fail.  
/*bepaalbankkode(IBAN, "onbekend", Naam) :-
  %logclose(),
  format(Msg, "IBAN %s onbekende bankcode van %s. Overslaan of stoppen?", IBAN, Naam),
  Answer = dlg_MessageBox( "IBAN", Msg, mesbox_iconexclamation, mesbox_buttonsokcancel, mesbox_defaultfirst, mesbox_suspendapplication ),
  Answer <> 1,
  exit(100).*/

toString([In | _], In, _SKeus) :- !.
toString(_, Uit, SKeus) :-
  Rest = SKeus mod 10000,	% laatste vier cijfers?
  format(Uit, "Inschr%u", Rest). 

detectGeenInschrijfGeld(CattenU) :-
  member(Cat, CattenU),
  wc(Cat, _OnderdeeelID,_Orde,  Naam, _EDG, _Abbr, _SexL, _SpStrk, _LftV, _LftTM, _HDG, _Schemasoort, _Keuring, _KNLTB, Contr), 
  Contr = 0,
  not(heeftGeenInschrijfgeld(Naam)),
  assert(heeftGeenInschrijfgeld(Naam)),
  fail.
detectGeenInschrijfGeld(_).

jaNeeBetaaldIncl(_Betaald, b_True) :- !.
jaNeeBetaaldIncl(nee, b_False).

betaalMMrecords(Namen, BetIncl, _) :-
  retractall(heeftBetaald(_,_)),
  retractall(heeftGeenInschrijfgeld(_)),
  assert(betCount(0)),
  %naam(_TNaam, ToernooiNrs, _, _, _, _), 
  date(Year, Month,Day),
  dt_date_to_offset(Year, Month, Day, Off1),
  Off2 = Off1 - 30,
  dt_dateoffset_to_str(Off2,"%YL-%MD-%DD", DateS),  
  member(Naam, Namen),
  cursor_SetWait(),
  sp2(SKeus,SpelerSrt,Naam,_Tel,_Verh,_RatE,_RatD,Betaald,_UitKant, _ClubNo, Adres, Woonpl, _LidNr, _SterkS, _SterkD, _Geb, _Telwerk, _Telmobiel, _EMail, _Opm,_Gezien,_RangPosE,_RangPosD,Incasso,_CheckGeg,_Log,Inschr),
  jaNeeBetaaldIncl(Betaald,BetIncl),
  reverse(Inschr,[],InschrRev),
  toString(InschrRev, InschrStr,SKeus),
  %toString(ToernooiNrs,ToerStr),
  spsrt(Spelersrt,_,_Sex,_,_,_,_,[STitel|_]),
  findall(Cat,sp_catX(SKeus,Cat),Catten),  % hoeft niet in schema te staan
  zeefFamilie(Catten, [], CattenU),
  %writedevice(X),
  %writedevice(screen),
  %write("\n",Catten, ""  ,CattenU),
  %writedevice(X),
  not(CattenU = []),
  detectGeenInschrijfgeld(CattenU),
  sum(CattenU,0.0,Som,"",String),
  Som > 0.0, 
  naamscan(ja,b_False,Naam, Naam1,_Voornaam,_Landcode),
  upper_lower(IncassoU,Incasso),
  bepaalbankkode(IncassoU, Bankkode,Naam),
  cleanToernooinaam(Naam1, Naam1C),
  cleanToernooinaam(Adres, AdresC),
  cleanToernooinaam(WoonPl, WoonPlC),
  assert(heeftBetaald(SKeus, IncassoU)),
  betCount(BC),
  write(STitel," ",Naam1C,csvconst,IncassoU,csvconst,Bankkode,csvconst,"EUR",csvconst,Som,csvconst,AdresC,csvconst,WoonplC,csvconst,"NL",csvconst,"shared",csvconst,"",csvconst,String,csvconst,InschrStr,csvconst,DateS),  
  %                  0                1                 2                3             4              5              6                7              8               9            10               11                12
  nl,
  BC1 = BC + 1,
  assert(betCount(BC1)),
  fail.
betaalMMrecords(_Namen, _, BC) :-
  closefile(work),
  betCount(BC),
  logclose().
  

sp_in_groep1(Groep, _, Sex) :-	%
  spsrt(Sex,Groep,_,_,_,_,_,_), !.
sp_in_groep1(Rubriek, SpNum, _) :-		% in bepaalde rubriek
  wc(Wc, _OnderdeeelID,_Orde,  Rubriek, _EDG, _Kort, _SexL, _SpStrk, _LftV, _LftTM, _HDG, _Schemasoort, _Keuring, _KNLTB, _Geld),
  %wc(Wc,Rubriek,_,_,_,_,_),
  sp2opg(SpNum,Wc,_,_,_,_), !.
sp_in_groep1(_, SpNum, _) :-			% in bepaalde rubriek
  incl_sp(SpNum), !.
sp_in_groep1(doe_niet_mee_const, SpNum, _) :-	% niet sp
  not(sp2opg(SpNum,_,_,_,_,_)), !.
sp_in_groep1(niet_betaald_const, SpNum, _) :-	% niet betaald
  findall(Cat,sp_cat(SpNum,Cat),Catten),
  not(Catten = []),
  sum(Catten,0.0,Som,"",_),
  Som > 0.0, !. 

betsp(No) :-
  sp2(No,_,_,_,_,_,_,nee,_, _, _, _, _, _, _, _, _, _, _, _,_,_,_,_,_,_,_).

betaal_namen(Progress, Gekozen_Groepen, PostcodeEnNaam, Orde) :-
  readdevice(keyboard),
  findall(No, betsp(No),List),
  count(List, 0, Aantal),
  assert(current(0)),
  member(SpNum, List),
  sp2(SpNum,Sex,Naam,_,_,_,_,_,_, _, _Adres, Woonpl, _, _, _, _, _, _, _, _,_,_,_,_,_,_,_),
  current(Cur),
  Cur1 = Cur + 1,
  assert(current(Cur1)),
  progress_bar_set_value(Progress, Aantal, Cur1),
  vpi_ProcessEvents(b_True),
  getbacktrack(Here),
  member(Groep, Gekozen_Groepen),
  sp_in_groep1(Groep, SpNum, Sex), 
  cutbacktrack(Here),
  met_postcode2(Orde, WoonPl, Naam, PostcodeEnNaam).

zonder_postcode2(2, In, Uit) :-
  frontstr(9, In, _, Uit), !.
zonder_postcode2(_, InUit, InUit).

met_postcode2(2, WoonPl, Naam, PostcodeEnNaam) :-
  frontstr(9, WoonPl, Postcode, _), !,
  concat(Postcode, Naam, PostcodeEnNaam).
met_postcode2(2, _, Naam, PostcodeEnNaam) :-
  str_len(Str, 9), !,
  concat(Str, Naam, PostcodeEnNaam).
met_postcode2(_, _, Naam, Naam).		% opt_postc = zonder postcode (alfab)



% ################# oproepetiketten #####################################
%
% #######################################################################
constants

 opt_postc = 1
 opt_z_adres = 2
 opt_z_uitslt = 3
 opt_nog_waars = 4
 opt_boek_waars = 5
 opt_spaarstand = 6
 opt_vervolg_dag = 7
 opt_vervolg_alle = 8
 opt_toernooinaam = 9

database - temp1

  te_waars(persno, integer, wedscat, tijd, integer LR, BOOLEAN Bijwinst, string BaanPark)	% de spelers en wedstr
  niet_waars(wedscat, tijd)
  sp_inf(wedstyp, persno, perslst, perslst)		% de spelergegevens
  determ incl(BOOLEAN)

domains
  spt  = spt(wedscat, tijd, persno, BOOLEAN Bijwinst, string Baan)
  sptl = spt*

predicates

  print_etik1(WINDOW Progress, slist, tijd)
  wlterm(BOOLEAN, persno, wl, wedstyp, perslst, perslst)
  check_info(persno, perslst)
  prt_informeren(indicator, perslst, slist In, slist Uit)
  prt_informerenmm(integer, perslst)
  prt_informerenbel(perslst)
  met_postcode(integer, string, string, string, tijd)
  qsortwE(wl,wl)
  splitwE(wedid,wl,wl,wl)
  te_waarsch(integer,wl, tijd, tijd)
  prt_speeltE(sptl, slist, slist)
  prt_speelt1(sptl, slist, slist)
  prt_uitslt(stringlist, slist, slist) - (i,i,o)
  zelfde_cat(integer,wedid, tijd, tijd)
  prt_speeltmm(sptl)
  prt_speeltmm(integer, sptl)
  prt_uitsltmm(stringlist)
  prt_speeltbel(sptl)
  prt_uitsltbel(stringlist)
  prt_uitsltbel1(stringlist)
  gew(boolean EmailNiet, integer, integer, wedscat, janee, janee, janee, janee, tgnst, tgnst)
nondeterm speler_speelt(persno, spt)
nondeterm  speler_uitslt(persno, string)
  list2string(string, stringlist, string)
  niets_te_melden(stringlist, sptl) 
nondeterm  keuzes_namen(WINDOW Progress, slist, string, tijd)
  preset_opties(integer Opt_postc,BOOLEAN Opt_z_adres,BOOLEAN Opt_z_uitslt,
	BOOLEAN Opt_nog_waars,BOOLEAN Opt_spaarstand,
	BOOLEAN Opt_vervolg_dag, BOOLEAN Opt_vervolg_alle, 
	BOOLEAN Opt_toernooinaam, Boolean Emailapart)  
 etikettenOproep1(WINDOW ProgBar, slist Groepen, integer PlanTM)
nondeterm spNummer(slist Gekozen_Groepen, integer SpNum)
  checkond(wedscat)
  gepland1(integer, tijd, BOOLEAN, BOOLEAN, Tijd)
procedure  verh_str(verhl, string, string, char Scheiding)
procedure  verh_strA(verhl, string, string, char Scheiding)
procedure  uitkant_str(tijd, string)
  namen_leeg(slist)
procedure format_opm(string In, integer Len, string Out)
nondeterm nietOpgesteld(integer Sp, string Rubriek)
procedure list_to_string1(slist, string)
procedure prt_catten(catl, string, string)
nondeterm prt_catten1(catl, string, string)
%determ stuuremail(Boolean , string Naam, string Emailadres, slist Tekst)
opencheck(integer)
determ emailNiet(string Emailadres, boolean)
procedure tekstEmail(boolean, string)

clauses

opt_etik_oproep(idc_volgorde_postcode,b_false,b_false,b_false,b_false,b_false,b_false,b_false, b_False).

mailmergeOproep(Win) :-
  assert(mailmerge(0)),
  etikettenOproep(Win),
  retract(mailmerge(_)), !.
mailmergeOproep(_Win) :-
  retractall(mailmerge(_)).

oproeplijst(Win) :-
  assert(mailmerge(1)),
  dlg_etiketten_oproep_Create(Win, _Gekozen_Groepen, _PlanTM),
  retract(mailmerge(_)), !.
oproeplijst(_) :-
  retractall(mailmerge(_)).

etikettenOproep(Win) :-
  dlg_etiketten_oproep_Create(Win, _Gekozen_Groepen, _PlanTM), !.
etikettenOproep(_).

opencheck(_) :- % 7032
  dlg_MessageBox( "Bestand", "Kan bestand niet openen.\nSluit eventueel Word en probeer opnieuw.", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  !.

tekstEmail(b_False, "") :- !.
tekstEmail(_, "zonder emailadres ").

etikettenOproep1(ProgressWin, Gekozen_Groepen, PlanTM):-
  retractall(incl(_)),
  mailmerge(0),
  TotN1 = PlanTM + 1,
  bitleft(TotN1, 7, Tot), 		% maak er een tijdstip van ..
  findall(Naam, keuzes_namen(ProgressWin, Gekozen_Groepen, Naam, Tot), Namen),
  not(namen_leeg(Namen)),
  sortstring_c(Namen, 1),
  %get_Toernooidir(TDir, _, _),
  getfolder(TDir,_,_,_),
  filenamePath(Brief, TDir, briefconst),
  _FILENAME = dlg_GetFileName(Brief,["txt","*.txt","dat","*.dat"],"Mailmerge bestand",[dlgfn_Save],"",_OutListFiles),
  filenamepath(_FILENAME, _Path, Name),
  Name <> fctconst,
  closefile(work),
  trap(openwrite(work, _FILENAME),Ret, opencheck(Ret)),
  writedevice(work),
  trap(print_etik1(ProgressWin, Namen, Tot),_,true), 
  closefile(work),
  format(Msg, "Oproepmailmergebestand aangemaakt in %s.",_FILENAME),
  writeToJournaal(Msg, ja),
  assert(aantal_etik(1)),
  fail.
etikettenOproep1(ProgressWin, Gekozen_Groepen, PlanTM):-
  mailmerge(1),		% oproep lijst
  TotN1 = PlanTM + 1,
  bitleft(TotN1, 7, Tot), 		% maak er een tijdstip van ..
  findall(Naam, keuzes_namen(ProgressWin, Gekozen_Groepen, Naam, Tot), Namen),
  not(namen_leeg(Namen)),
  sortstring_c(Namen, 1),
  closefile(work),
  get_TempDir(_, TempFile),
  openwrite(work,TempFile),
  writedevice(work),
  trap(print_etik1(ProgressWin, Namen, Tot),_,true), 
  closefile(work),
  TaskWin = vpi_GetTaskWin(),
  dlg_rapport_Create(TaskWin,"Oproeplijst", "Oproep",b_False), 
  fail.
  % ######################################################
etikettenOproep1(ProgressWin, Gekozen_Groepen, PlanTM):-
  not(mailmerge(_)),
  TotN1 = PlanTM + 1,
  bitleft(TotN1, 7, Tot), 		% maak er een tijdstip van ..
  findall(Naam, keuzes_namen(ProgressWin, Gekozen_Groepen, Naam, Tot), Namen),
  not(namen_leeg(Namen)),
  sortstring_c(Namen, 1),
  etik_reset(),
  trap(print_etik1(ProgressWin, Namen, Tot),_,true),
  TaskWin = vpi_GetTaskWin(),
  dlg_etiketten_print_Create(TaskWin, Ans),
  printetiketten(Ans, nee),
  fail.
etikettenOproep1(_ProgressWin, _Gekozen_Groepen, _PlanTM) :-
  opt_etik_oproep(_,_,_,_,_,_,_,_,EmailApart),
  oproepen(ja),
  aantal_etik(N_Etik),
  N_Etik <> 0,
  tekstEmail(EmailApart, Tekst),
  format(Msg, "Spelers %sop gewaarschuwd zetten?", Tekst),
  Ans = dlg_MessageBox( "Waarschuwadministratie", Msg, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
  Ans = 1,  
  retract(te_waars(_, Num1, Cat1, _, LinksRechts, _, _)),
  w2(Num1,Cat1,Tijd,G1,G2,F,T,Bn),
  wd(Num1, Cat1, Tg1, Tg2, _, _,_),
  gew(EmailApart,LinksRechts, Num1, Cat1, G1, G2, H1, H2, Tg1, Tg2),
  logf('A',w2(Num1, Cat1, Tijd, H1, H2, F, T, Bn),ja),
  fail.
etikettenOproep1(_ProgressWin, _Gekozen_Groepen, _PlanTM) :- 
  logclose(),
  retractall(_, temp1),
  retractall(mailmerge(_)),
  opt_etik_oproep(_,_,_,_,_,_,_,_,EmailApart),
  EmailApart = b_True,
  _A = dlg_MessageBox( "Waarschuwadministratie", "Stuur emails via het Oproepmenu!", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultsecond, mesbox_suspendapplication ),
  TaskWin = vpi_GetTaskWin(),
  win_waarschuwinvoer_Create(TaskWin, []), !.
etikettenOproep1(_ProgressWin, _Gekozen_Groepen, _PlanTM).

namen_leeg([]) :- 
  dlg_MessageBox( "Oproepen", "Er valt niets op te roepen!\nGebruik eventueel Personen, Uitvoer, Adresgegevens,\nEtiketten, mailmerge etc.", mesbox_iconquestion, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ).

spNummer(Gekozen_Groepen, Nummer) :-
  member(Groep, Gekozen_Groepen),
  spsrt(Sex,Groep,_,_,_,_,_,_), 
  %sp1(Nummer,Sex,_Naam,_,_,_Offset,_,_,_,_,_,_,_,_,_,_,_,_,_).
  sp2(Nummer,Sex,_Naam,_,_,_,_,_,_, _, _Adres, _Woonpl, _, _, _, _, _, _, _, _,_,_,_,_,_,_,_).

gepland1(Num, Tot, NietAlGew, _, EersteTijd) :-
  wd_planned(Num,WedstrijdList,Tot,b_True),
  not(WedstrijdList = []),
  qsortwE(WedstrijdList, WL1),
  %write("\nNo: ", Num, " WLijst: ", WL1),
  te_waarsch(Num,WL1,0, EersteTijd),		% maakt te_waars( .. meerdere per speler
  wlterm(NietAlGew, Num, WL1, OokEnkel, [], Partners),
  assert(sp_inf(OokEnkel,Num,Partners,[])),
  current(Cur),
  Cur1 = Cur + 1,
  assert(current(Cur1)), !.
gepland1(Num, _Tot, b_False, b_True, 0) :-	% nietnietalgew, vermelding van uitsluiten
  sp2opg(Num,Cat,_,_,_,Tgnst),	% alle opgaven voor een onderdeel
  checkond(Cat),
  %pos(Cat, _, _, _),		% onderdeel bestaat
  not(in_weds(Cat, Tgnst)),	% maar speler doet er niet aan mee
  assert(sp_inf(e,Num,[],[])),
  current(Cur),
  Cur1 = Cur + 1,
  assert(current(Cur1)), !.

keuzes_namen(Progress, Gekozen_Groepen, PostcodeEnNaam, Tot):-
  opt_etik_oproep(IDC_VOLGORDE_POSTCODE,_,IDC_VERMELDING_VAN_UITSLUITEN_CHECKED,IDC_NIET_AL_GEWAARSCHUWD_CHECKED,_,_,_,_,_EmailApart),
  readdevice(keyboard),
  findall(No, spNummer(Gekozen_Groepen, No), List),
  count(List, 0, Aantal),
  assert(current(0)),
  member(Num, List),
  %sp1(Num,_,Naam,_,_,_Offset,_,_, _, _, WoonPl, _, _, _, _, _, _, _, _),
  sp2(Num,_,Naam,_,_,_,_,_,_, _, _, Woonpl, _, _, _, _, _, _, _, _,_,_,_,_,_,_,_),
  gepland1(Num, Tot, IDC_NIET_AL_GEWAARSCHUWD_CHECKED,IDC_VERMELDING_VAN_UITSLUITEN_CHECKED, EersteTijd),
  current(Cur1),
  progress_bar_set_value(Progress, Aantal, Cur1),
  vpi_ProcessEvents(b_True),
  met_postcode(IDC_VOLGORDE_POSTCODE, Woonpl, Naam, PostcodeEnNaam,EersteTijd).

partners(SKeus, Partner) :-
  sp2opg(SKeus,Cat,_,MaatNo,_,_),
  MaatNo > 0,
  wc(Cat, _OnderdeeelID,_Orde,  _Naam, _EDG, Abbr, _SexL, _SpStrk, _LftV, _LftTM, _HDG, _Schemasoort, _Keuring, _KNLTB, _Geld),
  %wc(Cat,_,_,Abbr,_,_Contr,_),
  sp2(MaatNo,_,Naam,_,_,_,_,_,_, _, _, _, _, _, _, _, _, _, _, _,_,_,_,_,_,_,_),
  naamscan(ja,b_False, Naam, Naam1, _Voornaam, _Landcode),
  format(Partner, "%s met %s", Abbr, Naam1).



prt_partnermm([P1,P2,P3,P4|_]) :- !,
  writef("%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"",csvconst,P1,csvconst,P2,csvconst,P3,csvconst,P4).
prt_partnermm([P1,P2,P3]) :- !,
  writef("%c\"%s\"%c\"%s\"%c\"%s\"%c",csvconst,P1,csvconst,P2,csvconst,P3,csvconst).
prt_partnermm([P1,P2]) :- !,
  writef("%c\"%s\"%c\"%s\"%c%c",csvconst,P1,csvconst,P2,csvconst,csvconst).
prt_partnermm([P1]) :- !,
  writef("%c\"%s\"%c%c%c",csvconst,P1,csvconst,csvconst,csvconst).
prt_partnermm(_) :-
  write(csvconst,csvconst,csvconst,csvconst).
  
verh_str([], "", "", _) :- !.
verh_str(List, In, Uit, Scheiding) :-
  verh_strA(List, In, Uit, Scheiding), !.

verh_strA([], In, Uit, _) :- 		% punt achteraf
  concat(In, ".", Uit), !.
verh_stra([Periode|Rest], "", Uit, Scheiding) :-	% de eerste zonder scheiding vooraf
  Periode = per(T1,T2),
  tijd_kwart(D1,U1,M1,T1),
  tijd_kwart(_,U2,M2,T2),
  dag(D1,DS1,_), !,
  helestr_int(0,MS1,M1),
  helestr_int(0,MS2,M2),
  format(In1, "%-9 %2.%2 tot %2.%2",DS1,U1,MS1,U2,MS2),
  verh_strA(Rest, In1, Uit, Scheiding).
verh_strA([Periode|Rest], In, Uit, Scheiding) :-
  Periode = per(T1,T2),
  tijd_kwart(D1,U1,M1,T1),
  tijd_kwart(_,U2,M2,T2),
  dag(D1,DS1,_), !,
  helestr_int(0,MS1,M1),
  helestr_int(0,MS2,M2),
  format(In1, "%s%c%-9s %2.%2 tot %2.%2",In, Scheiding, DS1,U1,MS1,U2,MS2),
  verh_strA(Rest, In1, Uit,Scheiding).
verh_strA([_|Rest], In, Uit,Scheiding) :-
  verh_strA(Rest, In, Uit, Scheiding).

uitkant_str(Tijd, Str) :-
  Tijd > 0,
  tijd_kwart(_D1,U1,M1,Tijd),
  helestr_int(0,MS1,M1), !,
  format(Str, "%2.%2", U1, MS1).
uitkant_str(_, "") :- !.

emailNiet(_Emailadres, b_False) :- !.
emailNiet(Emailadres, _) :-
  emailadresFout(Emailadres).

/*stuuremail(b_True, Ontvanger, Emailadres, Lista5) :-
  fronttoken(Emailadres, _, _),
  list_to_string(Lista5, "\n", Body),
  naam(Toernooinaam, _, _, _, _, _),
  emailSturen([mapinameaddr(Ontvanger,EMailadres)],[],[],Toernooinaam,Body,[]), !,
  fail.
stuuremail(_, _, _Emailadres, _Lista5).
*/
print_etik1(_Progress,  _, _) :-
  opt_etik_oproep(_,_,_,_,IDC_SPAARSTAND_INFORMEER_PARTNER_CHECKED,_,_,_,_Email),
  IDC_SPAARSTAND_INFORMEER_PARTNER_CHECKED = b_true,
  sp_inf(d, Sp, [Partner],_),		% voor alle alleen dubbel spelers
  getbacktrack(Here),
  sp_inf(_, Partner, _, _),		% zoek partner
  retractall(sp_inf(_, Sp, _, _)),	% delete speler
  retract(sp_inf(_, Partner, Partners, WaarsPartner)),
  cutbacktrack(Here),			% zorg dat partner label krijgt
  assert(sp_inf(d, Partner, Partners, [Sp|WaarsPartner])), % gs = partner info
  fail.
print_etik1(_Progress,  _, _) :-
  opt_etik_oproep(_,_,_,_,IDC_SPAARSTAND_INFORMEER_PARTNER_CHECKED,_,_,_,_Email),
  IDC_SPAARSTAND_INFORMEER_PARTNER_CHECKED = b_true,
  sp_inf(d, Sp, [Partner,Partner2],_),	% voor alle alleen dubbel spelers
  getbacktrack(Here),
  sp_inf(_, Partner, _, _),
  sp_inf(_, Partner2, _, _),		% zoek partner
  retractall(sp_inf(_, Sp, _, _)),	% delete speler
  retract(sp_inf(_, Partner, Partners, WaarsPartner)),
  retract(sp_inf(_, Partner2, Partners2, WaarsPartner2)),
  cutbacktrack(Here),			% zorg dat partner label krijgt
  assert(sp_inf(d, Partner, Partners, [Sp|WaarsPartner])), % g = partner info
  assert(sp_inf(d, Partner2, Partners2, [Sp|WaarsPartner2])), 
  fail.
print_etik1(Progress, Namen, _/*Tot*/) :-
  opt_etik_oproep(_,IDC_MET_ADRESETIKETTEN_CHECKED,_,_,_,_,_,_,EmailApart),
  not(mailmerge(_)),
  count(Namen, 0, Aantal),
  assert(current(0)),
  member(NaamR, Namen),
  current(Cur),
  Cur1 = Cur + 1,
  assert(current(Cur1)),
  progress_bar_set_value(Progress, Aantal, Cur1),
  vpi_ProcessEvents(b_True),
  frontstr(9, NaamR, _, Naam),
  sp2(SKeus,SpelerSrt,Naam,_,_,_,_,_,_, _, Adres, Woonpl, _, _, _, _, _, _,Emailadres, _,_,_,_,_,_,_,_),
  emailNiet(Emailadres, Emailapart),
  check_info(SKeus, WieInformeren),
  spsrt(Spelersrt,_,_Sex,_,_,_,_,[STitel|_]),
  findall(Speelt, speler_speelt(SKeus, Speelt), SpeeltL),
  findall(Uitslt, speler_uitslt(SKeus, Uitslt), UitsltL),
  not(niets_te_melden(UitsltL, SpeeltL)),
  naamscan(ja,b_False,Naam, Naam1, _, _Landcode),
  format(NStr, " % %", STitel, Naam1),
  prt_uitslt(Uitsltl, [], Lista0), 
  append([NStr], Lista0, Lista1),
  prt_speeltE(SpeeltL, [],Lista2),
  append(Lista1, Lista2, Lista3),
  prt_informeren(1, WieInformeren,[],Lista4),
  append(Lista3, Lista4, Lista5),
  %stuuremail(Emailapart, "",Emailadres, Lista5),  
% .................................................................
  etiketBuild(Lista5),
  IDC_MET_ADRESETIKETTEN_CHECKED = b_true,
  concat(" ", Adres, Adres1),
  concat(" ", WoonPl, WoonPL1),
  etiketBuild([NStr, "", Adres1, WoonPL1]),
  fail.
print_etik1(Progress, Namen, _Tot) :-
  mailmerge(0),
  opt_etik_oproep(_,_,_,_,_,_,_,_,EmailApart),
  write("mv",csvconst,"naam",csvconst,"voornaam",csvconst,"adres",csvconst,"woonpl",csvconst,"lidnr",csvconst,"telefoon",csvconst,"email",csvconst,"uitsluit1",csvconst,"uitsluit2",csvconst,"oproep1",csvconst,"oproep2",csvconst,"oproep3",csvconst,"oproep4",csvconst,"oproep5",csvconst,"oproep6",csvconst,"oproep7",csvconst,"oproep8",csvconst),
  write("oproep9",csvconst,"oproep10",csvconst,"oproep11",csvconst,"oproep12",csvconst,"oproep13",csvconst,"oproep14",csvconst,"oproep15",csvconst,"oproep16",csvconst,"oproep17",csvconst,"oproep18",csvconst,"oproep19",csvconst,"oproep20",csvconst,"oprpteveel",csvconst,"informeer1",csvconst,"informeer2",csvconst,"geld",csvconst,"partner1",csvconst,"partner2"),
  write(csvconst,"partner3",csvconst,"partner4",csvconst,"sterktes",csvconst,"sterkted",csvconst,"geboren"),
  write(csvconst,"beschikbaar",csvconst,"verhinderingen",csvconst,"deelname1",csvconst,"deelname2\n"),
  count(Namen, 0, Aantal),
  assert(current(0)),
  member(NaamR, Namen),
  current(Cur),
  Cur1 = Cur + 1,
  assert(current(Cur1)),
  progress_bar_set_value(Progress, Aantal, Cur1),
  vpi_ProcessEvents(b_True),
  frontstr(9, NaamR, _, Naam),
  sp2(SKeus,SpelerSrt,Naam,Tel0,VerhinderL,_RE,_RD,_,UitKant, _, Adres, Woonpl, LidNr, SterkS, SterkD, Geb, Tel1, Tel2, EMailadres, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Res),
  emailNiet(Emailadres, Emailapart),
  check_info(SKeus, WieInformeren),
  spsrt(Spelersrt,_,_Sex,_,_,_,_,[STitel|_]),
  findall(Speelt, speler_speelt(SKeus, Speelt), SpeeltL),
  findall(Uitslt, speler_uitslt(SKeus, Uitslt), UitsltL),
  not(niets_te_melden(UitsltL, SpeeltL)), 
  findall(Partner, partners(SKeus, Partner), PartnerL),
  naamscan(ja,b_False,Naam, Naam1, Voornaam,_Landcode),
  % -------------
  format(NStr, " % %", STitel, Naam1),
  prt_uitslt(Uitsltl, [], Lista0), 
  append([NStr], Lista0, Lista1),
  prt_speeltE(SpeeltL, [],Lista2),
  append(Lista1, Lista2, _Lista3),
  prt_informeren(1, WieInformeren,[],_Lista4),
  %append(Lista3, Lista4, Lista5),
  %stuuremail(Emailapart, "",Email, Lista5),  
  % --------------
  writef("\"%\"%c\"%\"%c\"%\"%c", STitel, csvconst,Naam1,csvconst,Voornaam,csvconst),
  format(Telefoon, "%s %s %s", Tel0, Tel1, Tel2),
  trim_c(Telefoon),
  writef("\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c",Adres, csvconst,WoonPl,csvconst,Lidnr,csvconst,Telefoon,csvconst,Emailadres,csvconst),	% 3, 4
  prt_uitsltmm(Uitsltl), 
  prt_speeltmm(SpeeltL),
  prt_informerenmm(1, WieInformeren),
  findall(Catx,sp_cat(SKeus,Catx),Catten),
  prt_catten(Catten, CattenL, CattenS),
  sum(Catten,0.0,Som,"",_),
  writef("%c\"%6.2f\"",csvconst, Som),
  prt_partnermm(PartnerL),
  uitkant_str(UitKant, Beschikbaar),
  verh_str(VerhinderL, "", Verhinderingen,csvconst),
  writef("%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"%c\"%s\"",csvconst,SterkS,csvconst,SterkD,csvconst,Geb,csvconst,Beschikbaar,csvconst,Verhinderingen,csvconst,CattenL,csvconst,CattenS),
  nl,
  fail.
print_etik1(Progress, Namen, _Tot) :-
  mailmerge(1),		% oproeplijst
  opt_etik_oproep(_,_,_,_,_,_,_,_,EmailApart),
  count(Namen, 0, Aantal),
  assert(current(0)),
  member(NaamR, Namen),
  current(Cur),
  Cur1 = Cur + 1,
  assert(current(Cur1)),
  progress_bar_set_value(Progress, Aantal, Cur1),
  vpi_ProcessEvents(b_True),
  frontstr(9, NaamR, _, Naam),
  sp2(SKeus,SpelerSrt,Naam,Tel,_,_,_,_,_, _, _, _, _, _, _, _, Tel1, Tel2, Emailadres, _,_,_,_,_,_,_,_Res),
  emailNiet(Emailadres, Emailapart),
  check_info(SKeus, WieInformeren),
  spsrt(Spelersrt,_,_Sex,_,_,_,_,[STitel|_]),
  findall(Speelt, speler_speelt(SKeus, Speelt), SpeeltL),
  findall(Uitslt, speler_uitslt(SKeus, Uitslt), UitsltL),
  not(niets_te_melden(UitsltL, SpeeltL)), 
  writef("% % \nH:%-11s W:%-11s M:%-11s\n", STitel, Naam, Tel,Tel1,Tel2),
  prt_uitsltbel(Uitsltl), 
  prt_speeltbel(SpeeltL),
  prt_informerenbel(WieInformeren),
  nl,
  fail.
print_etik1(_, _, _).

clubP(Club, ClubNaam, District) :-
  club(Club, ClubNaam, District), !.
clubP(_, "", "").

spelerdata(Sp, sp2(Sp,Spelersrt,Naam,Tel0,VerhinderL,_RatingE,_RatingD,_Bet,Uitkant, Club, Adres, Woonpl, LidNr, SterkS, SterkD, Geb, Tel1, Tel2, EMail, Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Res),Data) :-
  spsrt(Spelersrt,_,_Sex,_,_,_,_,[STitel|_]),
  naamscan(ja, b_False,Naam, Naam1, _, _Landcode),
  format(NSt, "Gegevens van  : % %", STitel, Naam1),
  format(Tel, "Telefoon      : %s %s %s", Tel0, Tel1, Tel2),
  format(Adr, "Adres         : %s %s", Adres, WoonPl),
  format(Lnr, "Lidnummer     : %s  Geboortedatum: %s", Lidnr, Geb),
  format(Ema, "Emailadres    : %s", Email),
  clubP(Club, ClubNaam, ClubDistrict),
  format(Ver, "Vereniging    : %s %s %s", Club, ClubNaam, ClubDistrict),
  format(Str, "Speelsterktes : %s %s", SterkS, Sterkd),
  uitkant_str(UitKant, Beschikbaar),
  format(Uka, "Uit kantoor   : %s", Beschikbaar),
  verh_str(VerhinderL, "", Verhinderingen, '\n'),
  format(Vrh, "Verhinderingen: \n%s", Verhinderingen),
  str_len(Opm, Olen),
  format_opm(Opm, Olen, Opo),
	wd_planned(Sp,WedstrijdList,alleplanning,b_True),
	speeltijden1(WedstrijdList,Speeltdn),
	qsortwt(Speeltdn,SpeeltdnS),
	prt_speelt(SpeeltdnS,0,_,nee,1,1,[],OnderdelenU,0,_X),
	list_to_string1(["Geplande wedstrijden:"|OnderdelenU], Ost),
  te_plannenList(Sp, TePlanList),
  list_to_string1([""|TePlanList], Tpl),
  findall(Sx, nietOpgesteld(Sp, Sx), NietOpgList),
  list_to_string1(["Opgegeven voor:"|NietOpgList], Nop),
  findall(Partner, partners(Sp, Partner), PartnerL),
  list_to_string1(["Partners:"|PartnerL], Pal), 
  format(Data, "%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s%s%s%s%s%s",
   NSt, Tel, Adr, Lnr, Ema, Ver, Str, Uka, Vrh, Opo, Ost, Tpl, Nop, Pal), !. 
spelerdata(_Sp, _, "Geen spelerdata!").

list_to_string1([_], "") :- !.
list_to_string1(List, String1) :-
  list_to_string(List, "\n", String),
  concat("\n", String, String1).

prt_catten([], "", "") :- !.
prt_catten(Catten, CattenL, CattenS) :-
  findall(CatL,prt_catten1(Catten, CatL, _), CatLList),
  findall(CatS,prt_catten1(Catten, _, CatS), CatSList),
  list_to_string(CatLList," ", CattenL),
  list_to_string(CatSList," ", CattenS).

prt_catten1(Catten, Naam, Kort) :-
  member(Cat, Catten),
  wc(Cat, _OnderdeeelID,_Orde,  Naam, _EDG, Kort, _SexL, _SpStrk, _LftV, _LftTM, _HDG, _Schemasoort, _Keuring, _KNLTB, _Geld).
  %wc(Cat,CatL,_,CatS,_,_,_).

nietOpgesteld(Sp, Naam) :-
  sp2opg(Sp,Cat,_,_,_,Tgnst),	% alle opgaven voor een onderdeel
  not(in_weds(Cat, Tgnst)),	% maar speler doet er niet aan mee
  wc(Cat, _OnderdeeelID,_Orde,  Naam, _EDG, _Kort, _SexL, _SpStrk, _LftV, _LftTM, _HDG, _Schemasoort, _Keuring, _KNLTB, _Geld).
  %wc(Cat,Rubriek,_,_,_,_,_).

format_opm(Opm, _, "") :-
  not(fronttoken(Opm, _, _)), !.
format_opm(Opm, Len, Str) :-
  Len < 70,
  format(Str, "\nOpmerkingen:\n%s",Opm), !.
format_opm(Opm, Len, Str) :-
  Len < 140,
  frontstr(70,Opm,Opm1,Opm2),
  format(Str, "\nOpmerkingen:\n%s%-70.70s", Opm1, Opm2), !.
format_opm(Opm, _Len, Str) :-
  frontstr(70,Opm,Opm1,Opmx),
  frontstr(70,OpmX,Opm2,Opm3),
  format(Str, "\nOpmerkingen:\n%s\n%s%-70.70s", Opm1, Opm2, Opm3), !.
format_opm(_, _Len, "Opm??").

prt_uitsltmm([]) :- write(csvconst,csvconst), !.	% twee uitsluitingen max 5, 6
prt_uitsltmm([U1,U2|_]) :-
  writef("\"%s\"%c\"%s\"%c",U1, csvconst,U2,csvconst), !.
prt_uitsltmm([U1]) :-
  writef("\"%s\"%c%c",U1,csvconst,csvconst).		% 25.5.2003

prt_speeltmm(SpList) :-
  prt_speeltmm(1, SpList).

prt_speeltmm(21, []) :-
  write(csvconst), !.
prt_speeltmm(21, _) :-
  write("j",csvconst), !.			% 21 en nog niet leeg -> overflow!
prt_speeltmm(N, []) :-
  write(csvconst),
  N1 = N + 1, !,
  prt_speeltmm(N1, []).
prt_speeltmm(N, [Sp|Rest]) :-
  Sp = spt(Cat, Tijd, _, BW, BaanPark),
  tijd_kwart(D1,U1,M1,Tijd),
  dag(D1,DS1,_), 
  helestr_int(0,MS1,M1),
  bijWinst(BW, BijWS, _),
  wc(Cat, _OnderdeeelID,_Orde,  _Naam, _Type, Kort, _SexL, _SpStrk, _LftV, _LftTM, _HDG, _Schemasoort, _Keuring, _KNLTB, _Geld), !,
  %wc(Cat,_,_,Type,_,_,_), !,
  writef("\"%-4 %-7 %2\46%2 %s %\"%c",Kort,DS1,U1,MS1,BaanPark,BijWS,csvconst),	% komma achteraan
  N1 = N + 1,
  prt_speeltmm(N1, Rest).

prt_informerenmm(_, []) :- write(csvconst), !.
prt_informerenmm(_, [H]) :-
  sp2(H,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !,
  naamscan(ja, b_False,Naam, Naam1, _Voornaam,_Landcode),
  writef("\"%\"%c", Naam1,csvconst).
prt_informerenmm(_, [H,I|_]) :-
  sp2(H,_,NaamH,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), 
  naamscan(ja, b_False,NaamH, NaamH1, _VoornaamH,_LandcodeH),
  sp2(I,_,NaamI,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !,
  naamscan(ja, b_False,NaamI, NaamI1, _VoornaamI,_LandcodeI),
  writef("\"%\"%c\"%s\"", NaamH1, csvconst,NaamI1).

%#################### bellen
prt_uitsltbel([]) :- !.	% geen uitsluitingen 
prt_uitsltbel(Uitsl) :-
  prt_uitsltbel1(Uitsl), !.
  
prt_uitsltbel1([U1|Rest]) :-
  writef("%s  ",U1), !,
  prt_uitsltbel1(Rest).
prt_uitsltbel1(_) :- nl.

prt_speeltbel([Sp|List]) :-
  Sp = spt(Cat, Tijd, _, BW, BaanPark),
  tijd_kwart(D1,U1,M1,Tijd),
  dag(D1,DS1,_), 
  helestr_int(0,MS1,M1),
  bijWinst(BW, BijWS,_),
  wc(Cat, _OnderdeeelID,_Orde,  _Naam, _EDG, Kort, _SexL, _SpStrk, _LftV, _LftTM, _HDG, _Schemasoort, _Keuring, _KNLTB, _Geld), !,
  %wc(Cat,_,_,Type,_,_,_), !,
  writef("%-4 %-7 %2\46%2 %s %\n",Kort,DS1,U1,MS1,BaanPark,BijWS),	
  prt_speeltbel(List).
prt_speeltbel(_).

prt_informerenbel([]) :- !.
prt_informerenbel([H]) :-
  sp2(H,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !,
  naamscan(ja, b_False,Naam, Naam1, _Voornaam,_Landcode),
  writef("Partner informeren: %\n", Naam1).
prt_informerenbel([H,I|_]) :-
  sp2(H,_,NaamH,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  naamscan(ja,b_False,NaamH, NaamH1,_, _LandcodeH),
  sp2(I,_,NaamI,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !,
  naamscan(ja, b_False,NaamI, NaamI1, _Voornaam,_LandcodeI),
  writef("Partner informeren: %s %s\n", NaamH1, NaamI1).
% ###################### bellen eind

gew(b_True, 0, _No, _Cat, _, X, _, X, Tg, _) :-
  tgnstnum(Tg, Num),
  sp2(Num,_,_,_,_,_,_,_,_,_, _, _, _, _,_,_, _, _, EMail, _,_,_,_,_,_,_,_),
  fronttoken(Email, _, _),
  emailproef(Email), !, 
  %write("\n", Tg, " ", No, " ", Cat),
  fail.
gew(b_True, 1, _No, _Cat, _, X, _, X, _, Tg) :-
  tgnstnum(Tg, Num),
  sp2(Num,_,_,_,_,_,_,_,_,_, _, _, _, _,_,_, _, _, EMail, _,_,_,_,_,_,_,_),
  fronttoken(Email, _, _),
  emailproef(Email), !, 
  %write("\n", Tg, " ", No, " ", Cat),
  fail.
gew(_, 0, No, Cat, nee, X, ja, X, Tg, _) :-
  not(Tg = onbekend),
  ws(No, Cat, Tg, Tijd),
  logf('R',ws(No, Cat, Tg, Tijd),ja), !.
gew(_, 0, _, _, nee, X, ja, X, Tg, _) :-
  not(Tg = onbekend).
gew(_, 1, No, Cat, X, nee, X, ja, _, Tg) :-
  not(Tg = onbekend),
  ws(No, Cat, Tg, Tijd),
  logf('R',ws(No, Cat, Tg, Tijd),ja), !.
gew(_, 1, _, _, X, nee, X, ja, _, Tg) :-
  not(Tg = onbekend).

met_postcode(idc_volgorde_postcode, WoonPl, Naam, PostcodeEnNaam, _) 	:-	% met postcode
  frontstr(9, WoonPl, Postcode, _), !,
  concat(Postcode, Naam, PostcodeEnNaam).
met_postcode(idc_volgorde_planning, _, Naam, PostcodeEnNaam, Plan) :-	% met postcode
  format(PostcodeEnNaam, "%9d%s",Plan,Naam), !.
met_postcode(idc_volgorde_postcode, _, Naam, PostcodeEnNaam, _) :-		% alle andere gevallen
  str_len(Str, 9), !,
  concat(Str, Naam, PostcodeEnNaam).
met_postcode(idc_volgorde_alfa, _, Naam, PostcodeEnNaam, _) :-			% alfabetisch
  str_len(Str, 9), !,
  concat(Str, Naam, PostcodeEnNaam).

speler_uitslt(Sp, Kort) :-
  opt_etik_oproep(_,_,IDC_VERMELDING_VAN_UITSLUITEN_CHECKED,_,_,_,_,_,_),
  IDC_VERMELDING_VAN_UITSLUITEN_CHECKED = b_true,
  sp2opg(Sp,Cat,_,_,_,Tgnst),	% alle opgaven voor een onderdeel
  checkond(Cat),
  not(in_weds(Cat, Tgnst)),	% maar speler doet er niet aan mee
  wc(Cat, _OnderdeeelID,_Orde,  _Naam, _EDG, Kort, _SexL, _SpStrk, _LftV, _LftTM, _HDG, _Schemasoort, _Keuring, _KNLTB, _Geld).
  %wc(Cat,_,_,Rubriek,_,_,_).

checkond(Cat) :-
  pos(Cat, _, _, _), !.		% onderdeel bestaat
checkond(_) :-			% óók de niet-bestaanden
  incl(b_True), !.
checkond(_) :-
  not(incl(_)),
  1 = dlg_MessageBox( "Bericht van uitsluiten", "Ook voor niet ingedeelde onderdelen?", mesbox_iconquestion, mesbox_buttonsYesNo, mesbox_defaultsecond, mesbox_suspendapplication ),
  assert(incl(b_True)), !.
checkond(_) :-
  not(incl(_)),
  assert(incl(b_False)).
  
niets_te_melden([], []).


wlterm(b_True, SKeus,[w(_, Cat, _, nee, _, _, _, _)|Rest],e,PartnersI, PartnersO) :-
  wc(Cat, _OnderdeeelID,_Orde,  _Naam, e, _Kort, _SexL, _SpStrk, _LftV, _LftTM, _HDG, _Schemasoort, _Keuring, _KNLTB, _Geld),
  wlterm(b_True,SKeus, Rest, _, PartnersI, PartnersO), !.
wlterm(b_False, SKeus,[w(_, Cat, _, _, _, _, _, _)|Rest],e,PartnersI, PartnersO) :-
  wc(Cat, _OnderdeeelID,_Orde,  _Naam, e, _Kort, _SexL, _SpStrk, _LftV, _LftTM, _HDG, _Schemasoort, _Keuring, _KNLTB, _Geld),
  wlterm(b_False, SKeus, Rest, _, PartnersI, PartnersO), !.
wlterm(b_True, SKeus,[w(_, Cat, _, nee, _,_,_,_)|Rest],EnkelD,PartnersI,PartnersO) :-
  wc(Cat, _OnderdeeelID,_Orde,  _Naam, _, _Kort, _SexL, _SpStrk, _LftV, _LftTM, _HDG, _Schemasoort, _Keuring, _KNLTB, _Geld),
  sp2opg(SKeus,Cat,_,MaatNo,_,_),
  MaatNo <> 0,
  not(member(MaatNo,PartnersI)),
  wlterm(b_True,SKeus, Rest, EnkelD, [MaatNo|PartnersI], PartnersO), !.
wlterm(b_False, SKeus,[w(_, Cat, _, _, _,_,_,_)|Rest],EnkelD,PartnersI,PartnersO) :-
  wc(Cat, _OnderdeeelID,_Orde,  _Naam, _, _Kort, _SexL, _SpStrk, _LftV, _LftTM, _HDG, _Schemasoort, _Keuring, _KNLTB, _Geld),
  sp2opg(SKeus,Cat,_,MaatNo,_,_),
  MaatNo <> 0,
  not(member(MaatNo,PartnersI)),
  wlterm(b_False, SKeus, Rest, EnkelD, [MaatNo|PartnersI], PartnersO), !.
wlterm(Algew, SKeus,[_|Rest],EnkelD,PartnersI, PartnersO) :-
  wlterm(Algew, SKeus, Rest, EnkelD, PartnersI, PartnersO), !.
wlterm(_,_,_,d,List,List).

check_info( _, []) :-
  opt_etik_oproep(_,_,_,_,IDC_SPAARSTAND_INFORMEER_PARTNER_CHECKED,_,_,_,_),
  IDC_SPAARSTAND_INFORMEER_PARTNER_CHECKED = b_false, !.
check_info(SKeus, W) :-		% W = te waarsch partnerlist
  sp_inf(_, SKeus, _, W), !.	% <== dan moet er een sp_inf zijn

prt_uitslt([], In, In) :- !.
prt_uitslt(List, In, [" U bent niet opgesteld voor:",String|Uit]) :-
  list2string("  ", List, String),
  prt_uitslt([], In, Uit) .

list2string(Uit, [], Uit) :- !.
list2string(In, [Top|Rest], Uit) :-
  format(In1, "% %", In, Top),
  list2string(In1, Rest, Uit), !.

prt_speeltE([], In, In) :- !.
prt_speeltE(SpList, In, [Opmaak|Uit]) :-
  opt_etik_oproep(_,_,_,_,_,_,_, IDC_MET_TOERNOOINAAM_CHECKED,_),
  IDC_MET_TOERNOOINAAM_CHECKED = b_true,
  naam(Naam, _, _, _, _, _), !,
  format(Opmaak, " Oproep %\:", Naam),
  prt_speelt1(SpList, In, Uit).
prt_speeltE(SpList, In, [" Oproep om te spelen:"|Uit]) :-
  prt_speelt1(SpList, In, Uit).

prt_speelt1([Sp|Rest], In, [WStr|Uit]) :-
  baan_adm(nee),
  Sp = spt(Cat, Tijd, _, BW, _BaanPark),
  tijd_kwart(D1,U1,M1,Tijd),
  dag(D1,DS1,_), !,
  helestr_int(0,MS1,M1),
  bijWinst(BW, BijWS,_),
  wc(Cat, _OnderdeeelID,_Orde,  _Naam, _EDG, Kort, _SexL, _SpStrk, _LftV, _LftTM, _HDG, _Schemasoort, _Keuring, _KNLTB, _Geld), !,
  %wc(Cat,_,_,Type,_,_,_), !,
  format(WStr,"   %-4 %-7 %2uu%2 %",Kort,DS1,U1,MS1,BijWS),
  prt_speelt1(Rest, In, Uit).
prt_speelt1([Sp|Rest], In, [WStr|Uit]) :-
  baan_adm(ja),
  Sp = spt(Cat, Tijd, _, BW, BaanPark),
  tijd_kwart(D1,U1,M1,Tijd),
  dag(D1,DS1,_), !,
  helestr_int(0,MS1,M1),
  bijWinst(BW, BijWS,_),
  wc(Cat, _OnderdeeelID,_Orde,  _Naam, _EDG, Kort, _SexL, _SpStrk, _LftV, _LftTM, _HDG, _Schemasoort, _Keuring, _KNLTB, _Geld), !,
  %wc(Cat,_,_,Type,_,_,_), !,
  format(WStr,"   %-4 %-7 %2uu%2 % [%]",Kort,DS1,U1,MS1,BijWS,BaanPark),
  prt_speelt1(Rest, In, Uit).
prt_speelt1(_, In, In) :- !.

prt_informeren(_, [],In, In) :- !.
prt_informeren(1, WL, In, [" Informeer svp uw partner(s):"|Uit]) :- !,
  prt_informeren(0, WL, In, Uit).
prt_informeren(_, [H|T], In, [Str|Uit]) :-
  sp2(H,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !,
  naamscan(ja, b_False,Naam, Naam1, _, _Landcode),
  format(Str, "   %.", Naam1),
  prt_informeren(0, T, In, Uit).

splitwE(_,[],[],[]).
splitwE(w(N, C,T,G,L,S,BW,BP),[w(N1,C1,T1,G1,L1,S1,BW1, BP1)|X],[w(N1, C1,T1,G1,L1,S1,BW1, BP1)|Y],Z) :-
  T > T1, !,
  splitwE(w(N, C, T, G, L, S, BW, BP),X,Y,Z).
splitwE(H,[A|X],Y,[A|Z]) :- !,
  splitwE(H,X,Y,Z).

qsortwE([],[]).
qsortwE([H|T],S) :-
  splitwE(H,T,A,B),
  qsortwE(A,A1),
  qsortwE(B,B1),
  append(A1,[H|B1],S), !.

te_waarsch(SKeus, [WD|Rest],In,Uit) :-	% zit er ergens een nog niet 
  zelfde_cat(SKeus,WD,In,Tijd), !,		%   gewaarschuwde tussen ?
  te_waarsch(SKeus,Rest,Tijd,Uit).
te_waarsch( _, _, In, In) :-
  retractall(niet_waars(_, _)). 	% alleen tijdelijk nodig!

zelfde_cat( _, w(_, Cat, Tijd, ja, _, _, _,_), In, In) :- 
  opt_etik_oproep(_,_,_,IDC_NIET_AL_GEWAARSCHUWD_CHECKED,_,_,_,_,_),
  IDC_NIET_AL_GEWAARSCHUWD_CHECKED = b_true,
  assertz(niet_waars(Cat, Tijd)), !. 	% worden opgeruimd!
zelfde_cat( _, w(_, Cat, T2, _, _, _, _, _), In, In) :-
  pos(Cat, _, _, afval), 
  niet_waars(Cat, T1),	%  voorgaande al gewaarschuwd, dus ...
  T1 < T2, !.				% er is een voorgaande en die is al .. 
zelfde_cat(SKeus, w(_, Cat, Tijd, _, _, _, _, _),In,In) :-
  opt_etik_oproep(_,_,_,_,_,_,IDC_MET_VERVOLGPARTIJEN_CHECKED,_,_),
  IDC_MET_VERVOLGPARTIJEN_CHECKED = b_false,
  % pos(Cat, _, _, afval), 
  te_waars(SKeus, _, Cat, T1, _, _, _),
  tijd_kwart(D1,_,_,Tijd),
  tijd_kwart(D2,_,_,T1),
  D1 <> D2, !.
zelfde_cat(SKeus, w(_, Cat, Tijd, _, _, _, _, _), In, In) :-
  opt_etik_oproep(_,_,_,_,_,IDC_DOORGAAN_OP_ZELFDE_DAG_MELDEN_CHECKED,IDC_MET_VERVOLGPARTIJEN_CHECKED,_,_),
  IDC_MET_VERVOLGPARTIJEN_CHECKED = b_false,
  IDC_DOORGAAN_OP_ZELFDE_DAG_MELDEN_CHECKED = b_false,
  te_waars(SKeus, _, Cat, T1, _, _, _),
  tijd_kwart(D1,_,_,Tijd),
  tijd_kwart(D2,_,_,T1),
  D1 = D2, !.
zelfde_cat(SKeus,w(WNo, Cat, Tijd, _, L_R, _, BW, BaanPark),0, Tijd) :- !,
  assertz(te_waars(SKeus,WNo, Cat, Tijd, L_R, BW, BaanPark)).
zelfde_cat(SKeus,w(WNo, Cat, Tijd, _, L_R, _, BW, BaanPark),In, In) :-
  assertz(te_waars(SKeus,WNo, Cat, Tijd, L_R, BW, BaanPark)).

speler_speelt(SpNo, spt(Cat, Tijd, SpNo, BW, BaanPark)) :-
  findall(Tijdstip, te_waars(SpNo, _, _, Tijdstip, _, _, _), Tijdstippen),
  sorttijd_c(Tijdstippen),
  member(Tijd, Tijdstippen),
  te_waars(SpNo, _, Cat, Tijd, _, BW, BaanPark).

preset_opties(VOLGORDE_POSTCO,b_True,IDC_VERMELDING_VAN_UITSLUITEN_CHECKED,
    IDC_NIET_AL_GEWAARSCHUWD_CHECKED,IDC_SPAARSTAND_INFORMEER_PARTNER_CHECKED,
    IDC_DOORGAAN_OP_ZELFDE_DAG_MELDEN_CHECKED, IDC_MET_VERVOLGPARTIJEN_CHECKED, 
    b_False,EmailApart):-
  mailmerge(_),
  opt_etik_oproep(VOLGORDE_POSTCO,_IDC_MET_ADRESETIKETTEN_CHECKED,_UITSLUITEN_CHECKED,
    IDC_NIET_AL_GEWAARSCHUWD_CHECKED,IDC_SPAARSTAND_INFORMEER_PARTNER_CHECKED,
    IDC_DOORGAAN_OP_ZELFDE_DAG_MELDEN_CHECKED, IDC_MET_VERVOLGPARTIJEN_CHECKED, 
    _IDC_MET_TOERNOOINAAM_CHECKED,EmailApart),
  bitxor(1, IDC_NIET_AL_GEWAARSCHUWD_CHECKED, IDC_VERMELDING_VAN_UITSLUITEN_CHECKED), !.
preset_opties(VOLGORDE_POSTCO,IDC_MET_ADRESETIKETTEN_CHECKED,IDC_VERMELDING_VAN_UITSLUITEN_CHECKED,
    IDC_NIET_AL_GEWAARSCHUWD_CHECKED,IDC_SPAARSTAND_INFORMEER_PARTNER_CHECKED,
    IDC_DOORGAAN_OP_ZELFDE_DAG_MELDEN_CHECKED, IDC_MET_VERVOLGPARTIJEN_CHECKED, 
    IDC_MET_TOERNOOINAAM_CHECKED,Emailapart):-
  opt_etik_oproep(VOLGORDE_POSTCO,IDC_MET_ADRESETIKETTEN_CHECKED,IDC_VERMELDING_VAN_UITSLUITEN_CHECKED,
    IDC_NIET_AL_GEWAARSCHUWD_CHECKED,IDC_SPAARSTAND_INFORMEER_PARTNER_CHECKED,
    IDC_DOORGAAN_OP_ZELFDE_DAG_MELDEN_CHECKED, IDC_MET_VERVOLGPARTIJEN_CHECKED, 
    IDC_MET_TOERNOOINAAM_CHECKED,Emailapart), !.


PREDICATES
  %ek_preview_handler : EHANDLER    Global
CLAUSES
  ek_preview_handler(PreviewWin,e_Create(_),0):-
	FormPage = 1,
	ColorPage = 16777215,
	etiketwaarden(r(TopMarg),r(LeftMarg),r(LabelWidth),r(LabelHeight),r(HorSpace),r(VertSpace),i(NoVert),i(NoHor),_,_),
	Page_Roll = 0,
	Gray = vpi_GetAttrVal(attr_color_btnface),
	preview_Init(PreviewWin,FormPage,ColorPage,LeftMarg,TopMarg,HorSpace,
		VertSpace,LabelWidth,LabelHeight,NoHor,NoVert,Page_Roll,Gray),!.

  ek_preview_handler(PreviewWin,EVENT,0):-
  	preview_HandleEvent(PreviewWin,EVENT).



%BEGIN_DLG Etiketten oproep
/**************************************************************************
	Creation and event handling for dialog: Etiketten oproep
**************************************************************************/

CONSTANTS

%BEGIN Etiketten oproep, CreateParms, 14:01:25-23.2.2005, Code automatically updated!
  dlg_etiketten_oproep_DlgType = wd_Modal
  dlg_etiketten_oproep_Title = "Oproepetiketten"
  dlg_etiketten_oproep_RCT = rct(40,32,244,186)
  dlg_etiketten_oproep_Flags = [wsf_Close,wsf_TitleBar]
  dlg_etiketten_oproep_Help = idh_etikettenoproep
%END Etiketten oproep, CreateParms

PREDICATES

  dlg_etiketten_oproep_eh : EHANDLER
  dlg_etiketten_oproep_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST, Slist Selectie, integer PlanTM)
  dlg_etiketten_oproep_update(DIALOG_VAL_LIST, Slist Selectie, integer PlanTM)
  oproepTitel(integer, string)
procedure  handle_uitsluiten(window, integer IDC_NIET_AL_GEWAARSCHUWD_CHECKED)

CLAUSES

oproepTitel(0, "Selecteer oproepgevens voor mailmergebestand") :- !.
oproepTitel(1, "Selecteer oproepgevens voor lijst") :- !.

handle_uitsluiten(Win, b_True) :- !,
  dialog_SetState(Win, [enable(idc_vermelding_van_uitsluiten, b_False)]),
  dialog_SetCheck(Win, idc_vermelding_van_uitsluiten, b_False).
handle_uitsluiten(_, _).

  dlg_etiketten_oproep_Create(Parent, Selectie, PlanTM):-
	findall(Groep,spsrt(_,Groep,_,_,_,_,_,_),IDC_ETIK_OPROEP_ITEMLIST),
	preset_opties(VOLGORDE_POSTCO,IDC_MET_ADRESETIKETTEN_CHECKED,IDC_VERMELDING_VAN_UITSLUITEN_CHECKED,
	  IDC_NIET_AL_GEWAARSCHUWD_CHECKED,IDC_SPAARSTAND_INFORMEER_PARTNER_CHECKED,
	  IDC_DOORGAAN_OP_ZELFDE_DAG_MELDEN_CHECKED, IDC_MET_VERVOLGPARTIJEN_CHECKED,
	  IDC_MET_TOERNOOINAAM_CHECKED, IDC_EMAIL_SPELERS_MET_EMAIL_CHECKED),
	IDC_ETIK_OPROEP_SELECT = [],
	findall(DagNaam, dag(_, DagNaam,_),IDC_PLAN_TM_ITEMLIST),
	count(IDC_PLAN_TM_ITEMLIST,-1,Aantal),
	IDC_PLAN_TM_SELECT = Aantal,		% default op de laatste dag
%MARK Etiketten oproep, new variables
	dialog_Create(Parent,
		[
%BEGIN Etiketten oproep, WinDefList, 14:01:25-23.2.2005, Code automatically updated!
		 dlg(wdef(dlg_etiketten_oproep_DlgType,dlg_etiketten_oproep_RCT,dlg_etiketten_oproep_Title,u_DlgBase),dlg_etiketten_oproep_Flags),
		 ctl(wdef(wc_LBox,rct(2,13,86,83),"",u_DlgBase),idc_etik_oproep,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_MultiSelect]),
		 ctl(wdef(wc_CheckBox,rct(89,10,161,20),"met &Adresetiketten",u_DlgBase),idc_met_adresetiketten,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_CheckBox,rct(89,20,181,30),"Vermelding van &uitsluiten",u_DlgBase),idc_vermelding_van_uitsluiten,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_CheckBox,rct(89,30,171,40),"&Niet al gewaarschuwd",u_DlgBase),idc_niet_al_gewaarschuwd,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_CheckBox,rct(89,40,200,50),"&Spaarstand (informeer partner)",u_DlgBase),idc_spaarstand_informeer_partner,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_CheckBox,rct(89,50,207,60),"&Doorgaan op zelfde dag melden",u_DlgBase),idc_doorgaan_op_zelfde_dag_melden,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_CheckBox,rct(89,60,180,70),"met Vervolgpartijen",u_DlgBase),idc_met_vervolgpartijen,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_CheckBox,rct(89,70,182,80),"met Toernooinaam          ",u_DlgBase),idc_met_toernooinaam,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_CheckBox,rct(89,80,202,90),"Spelers met &E-mail apart",u_DlgBase),idc_email_spelers_met_email,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_RadioButton,rct(8,110,62,120),"Postcode",u_DlgBase),idc_volgorde_postcode,[wsf_Auto,wsf_TabStop]),
		 ctl(wdef(wc_RadioButton,rct(8,120,62,130),"Alfabetisch",u_DlgBase),idc_volgorde_alfa,[wsf_Auto,wsf_TabStop]),
		 ctl(wdef(wc_LBoxButton,rct(135,98,183,153),"",u_DlgBase),idc_plan_tm,[wsf_Group,wsf_TabStop,wsf_VScroll]),
		 ctl(wdef(wc_PushButton,rct(72,116,120,128),"Etiketsoort",u_DlgBase),idc_etiketsoort,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(170,128,202,150),"&OK",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop,wsf_Disabled]),
		 ctl(wdef(wc_PushButton,rct(135,129,167,139),"&Cancel",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(135,140,167,150),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop]),
		 customctl(wdef(wc_Custom,rct(70,139,130,151),"Progress",u_DlgBase),"progress",idc_progress,[wsf_Group,wsf_TabStop,wsf_Invisible]),
		 ctl(wdef(wc_RadioButton,rct(8,130,62,140),"Planning",u_DlgBase),idc_volgorde_planning,[wsf_Auto,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(8,84,37,96),"&Alle",u_DlgBase),idc_alle,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(45,84,74,96),"&Geen",u_DlgBase),idc_geen,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_Text,rct(5,2,86,12),"Selecteer spelersoorten",u_DlgBase),idct_selecteer_spelersoorten,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(90,99,132,109),"Planning t/m",u_DlgBase),idct_planning_tm,[wsf_AlignLeft]),
		 ctl(wdef(wc_GroupBox,rct(6,101,66,143),"Volgorde",u_DlgBase),idc_volgorde,[])
%END Etiketten oproep, WinDefList
		],
  		[
%BEGIN Etiketten oproep, ControlList, 14:01:25-23.2.2005, Code automatically updated!
		df(idc_etik_oproep,listbox(IDC_ETIK_OPROEP_ITEMLIST,IDC_ETIK_OPROEP_SELECT),nopr),
		df(idc_met_adresetiketten,checkbox(IDC_MET_ADRESETIKETTEN_CHECKED),nopr),
		df(idc_vermelding_van_uitsluiten,checkbox(IDC_VERMELDING_VAN_UITSLUITEN_CHECKED),nopr),
		df(idc_niet_al_gewaarschuwd,checkbox(IDC_NIET_AL_GEWAARSCHUWD_CHECKED),nopr),
		df(idc_spaarstand_informeer_partner,checkbox(IDC_SPAARSTAND_INFORMEER_PARTNER_CHECKED),nopr),
		df(idc_doorgaan_op_zelfde_dag_melden,checkbox(IDC_DOORGAAN_OP_ZELFDE_DAG_MELDEN_CHECKED),nopr),
		df(idc_met_vervolgpartijen,checkbox(IDC_MET_VERVOLGPARTIJEN_CHECKED),nopr),
		df(idc_met_toernooinaam,checkbox(IDC_MET_TOERNOOINAAM_CHECKED),nopr),
		df(idc_email_spelers_met_email,checkbox(IDC_EMAIL_SPELERS_MET_EMAIL_CHECKED),nopr),
		df(idc_plan_tm,listbutton(IDC_PLAN_TM_ITEMLIST,IDC_PLAN_TM_SELECT),nopr),
		df(VOLGORDE_POSTCO,radiobuttongroup([idc_volgorde_postcode,idc_volgorde_alfa,idc_volgorde_planning]),nopr)
%END Etiketten oproep, ControlList
		],
		dlg_etiketten_oproep_eh,0,VALLIST,ANSWER),
	dlg_etiketten_oproep_handle_answer(ANSWER,VALLIST, Selectie, PlanTM).

  dlg_etiketten_oproep_handle_answer(idc_ok,_VALLIST, Selectie, PlanTM):-!,
	dlg_etiketten_oproep_update(_VALLIST, Selectie, PlanTM).
  dlg_etiketten_oproep_handle_answer(idc_cancel,_,[],0):-!.  % Handle Esc and Cancel here
  dlg_etiketten_oproep_handle_answer(_,_,_,_):-
	errorexit().

  dlg_etiketten_oproep_update(_VALLIST, _IDC_ETIK_OPROEP_ITEMLIST, PlanTM):-
%BEGIN Etiketten oproep, Update controls, 14:01:25-23.2.2005, Code automatically updated!
	dialog_VLGetListBox(idc_etik_oproep,_VALLIST,_IDC_ETIK_OPROEP_ITEMLIST,_IDC_ETIK_OPROEP_SELECT),
	_IDC_MET_ADRESETIKETTEN_CHECKED = dialog_VLGetCheck(idc_met_adresetiketten,_VALLIST),
	_IDC_VERMELDING_VAN_UITSLUITEN_CHECKED = dialog_VLGetCheck(idc_vermelding_van_uitsluiten,_VALLIST),
	_IDC_NIET_AL_GEWAARSCHUWD_CHECKED = dialog_VLGetCheck(idc_niet_al_gewaarschuwd,_VALLIST),
	_IDC_SPAARSTAND_INFORMEER_PARTNER_CHECKED = dialog_VLGetCheck(idc_spaarstand_informeer_partner,_VALLIST),
	_IDC_DOORGAAN_OP_ZELFDE_DAG_MELDEN_CHECKED = dialog_VLGetCheck(idc_doorgaan_op_zelfde_dag_melden,_VALLIST),
	_IDC_MET_VERVOLGPARTIJEN_CHECKED = dialog_VLGetCheck(idc_met_vervolgpartijen,_VALLIST),
	_IDC_MET_TOERNOOINAAM_CHECKED = dialog_VLGetCheck(idc_met_toernooinaam,_VALLIST),
	dialog_VLGetListButton(idc_plan_tm,_VALLIST,_IDC_PLAN_TM_ITEMLIST,_IDC_PLAN_TM_SELECT),
	_IDC_EMAIL_SPELERS_MET_EMAIL_CHECKED = dialog_VLGetCheck(idc_email_spelers_met_email,_VALLIST),
	_VOLGORDE_POSTCO = dialog_VLGetRadiobutton(idc_volgorde_postcode,_VALLIST),
%END Etiketten oproep, Update controls
	/*assert(opt_etik_oproep(_VOLGORDE_POSTCO,_IDC_MET_ADRESETIKETTEN_CHECKED,_IDC_VERMELDING_VAN_UITSLUITEN_CHECKED,
	  _IDC_NIET_AL_GEWAARSCHUWD_CHECKED,_IDC_SPAARSTAND_INFORMEER_PARTNER_CHECKED,
	  _IDC_DOORGAAN_OP_ZELFDE_DAG_MELDEN_CHECKED, _IDC_MET_VERVOLGPARTIJEN_CHECKED, _IDC_MET_TOERNOOINAAM_CHECKED, 
	  _IDC_EMAIL_SPELERS_MET_EMAIL_CHECKED)),*/
	finddag(PlanTM, _IDC_PLAN_TM_ITEMLIST),
	%_IDC_ETIK_OPROEP_ITEMLIST = [Selectie|_],
	true.

%MARK Etiketten oproep, new events


%BEGIN Etiketten oproep, idc_niet_al_gewaarschuwd _CtlInfo
  dlg_etiketten_oproep_eh(_Win,e_Control(idc_niet_al_gewaarschuwd,_CtrlType,CtrlWin,_CtlInfo),0):-
	mailmerge(_),
	IDC_NIET_AL_GEWAARSCHUWD_CHECKED = win_IsChecked(CtrlWin),
	bitxor(1,IDC_NIET_AL_GEWAARSCHUWD_CHECKED,EnableUitsluiten),
	dialog_SetCheck(_Win, idc_vermelding_van_uitsluiten, EnableUitsluiten),
	!.
  dlg_etiketten_oproep_eh(_Win,e_Control(idc_niet_al_gewaarschuwd,_CtrlType,CtrlWin,_CtlInfo),0):-
	not(mailmerge(_)),
	IDC_NIET_AL_GEWAARSCHUWD_CHECKED = win_IsChecked(CtrlWin),
	bitxor(1,IDC_NIET_AL_GEWAARSCHUWD_CHECKED,EnableUitsluiten),
	%handle_uitsluiten(IDC_NIET_AL_GEWAARSCHUWD_CHECKED, _IDC_VERMELDING_VAN_UITSLUITEN_CHECKED),
	dialog_SetState(_Win, [enable(idc_vermelding_van_uitsluiten, EnableUitsluiten)]), 
	IDC_NIET_AL_GEWAARSCHUWD_CHECKED = b_True,
	dialog_SetCheck(_Win, idc_vermelding_van_uitsluiten, b_False),
	!.
%END Etiketten oproep, idc_niet_al_gewaarschuwd _CtlInfo

%BEGIN Etiketten oproep, idc_geen _CtlInfo
  dlg_etiketten_oproep_eh(_Win,e_Control(idc_geen,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	ListWin = win_GetCtlHandle(_Win, idc_etik_oproep),
	selectGeen(ListWin),
	!.
%END Etiketten oproep, idc_geen _CtlInfo

%BEGIN Etiketten oproep, idc_alle _CtlInfo
  dlg_etiketten_oproep_eh(_Win,e_Control(idc_alle,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	ListWin = win_GetCtlHandle(_Win, idc_etik_oproep),
	selectAlle(ListWin),
	dialog_SetState(_Win, [enable(idc_OK, b_True)]),
	!.
%END Etiketten oproep, idc_alle _CtlInfo

%BEGIN Etiketten oproep, idc_ok _CtlInfo
  dlg_etiketten_oproep_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	dialog_SetState(_Win, [show(idc_progress,b_True)]),
	dialog_GetListBox(_Win, idc_etik_oproep,_IDC_ETIK_OPROEP_ITEMLIST,_IDC_ETIK_OPROEP_SELECT),
	_IDC_MET_ADRESETIKETTEN_CHECKED = dialog_GetCheck(_Win,idc_met_adresetiketten),
	_IDC_VERMELDING_VAN_UITSLUITEN_CHECKED = dialog_GetCheck(_Win, idc_vermelding_van_uitsluiten),
	_IDC_NIET_AL_GEWAARSCHUWD_CHECKED = dialog_GetCheck(_Win,idc_niet_al_gewaarschuwd),
	_IDC_SPAARSTAND_INFORMEER_PARTNER_CHECKED = dialog_GetCheck(_Win,idc_spaarstand_informeer_partner),
	_IDC_DOORGAAN_OP_ZELFDE_DAG_MELDEN_CHECKED = dialog_GetCheck(_Win,idc_doorgaan_op_zelfde_dag_melden),
	_IDC_MET_VERVOLGPARTIJEN_CHECKED = dialog_GetCheck(_Win,idc_met_vervolgpartijen),
	_IDC_MET_TOERNOOINAAM_CHECKED = dialog_GetCheck(_Win,idc_met_toernooinaam),
	dialog_GetListButton(_Win,idc_plan_tm,_IDC_PLAN_TM_ITEMLIST,_IDC_PLAN_TM_SELECT),
	_VOLGORDE_POSTCO = dialog_GetRadiobutton(_Win,idc_volgorde_postcode),
	_IDC_EMAIL_SPELERS_MET_EMAIL_CHECKED = dialog_GetCheck(_Win, idc_email_spelers_met_email),
	LboxWin = win_GetCtlHandle(_Win, idc_etik_oproep),
	lbox_GetSel(LboxWin,ItemList,_IndexList),
	assert(opt_etik_oproep(_VOLGORDE_POSTCO,_IDC_MET_ADRESETIKETTEN_CHECKED,_IDC_VERMELDING_VAN_UITSLUITEN_CHECKED,
	  _IDC_NIET_AL_GEWAARSCHUWD_CHECKED,_IDC_SPAARSTAND_INFORMEER_PARTNER_CHECKED,
	  _IDC_DOORGAAN_OP_ZELFDE_DAG_MELDEN_CHECKED, _IDC_MET_VERVOLGPARTIJEN_CHECKED,
	  _IDC_MET_TOERNOOINAAM_CHECKED, _IDC_EMAIL_SPELERS_MET_EMAIL_CHECKED)),
	finddag(PlanTM, _IDC_PLAN_TM_ITEMLIST),
	ProgressWin = win_GetCtlHandle(_Win, idc_progress),
	etikettenOproep1(ProgressWin, ItemList, PlanTM),
	%win_Destroy(_Win),
	fail.
%END Etiketten oproep, idc_ok _CtlInfo

%BEGIN Etiketten oproep, idc_help _CtlInfo
  dlg_etiketten_oproep_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Etikettenoproep"),
	!.
%END Etiketten oproep, idc_help _CtlInfo

%BEGIN Etiketten oproep, e_Create
  dlg_etiketten_oproep_eh(_Win,e_Create(_CreationData),0):-!,
	preset_opties(_,_,_IDC_VERMELDING_VAN_UITSLUITEN_CHECKED,IDC_NIET_AL_GEWAARSCHUWD_CHECKED, _,_, _,_,_),
	handle_uitsluiten(_Win, IDC_NIET_AL_GEWAARSCHUWD_CHECKED),
	%bitxor(1,IDC_NIET_AL_GEWAARSCHUWD_CHECKED,EnableUitsluiten),
	ProgressWin = win_GetCtlHandle(_Win, idc_progress),
	progress_bar_set_value(ProgressWin, 0, 0),
	mailmerge(Soort),		% alleen als mailmerge
	oproepTitel(Soort, Titel),
	win_SetText(_Win, Titel),
	dialog_SetState(_Win, [enable(idc_etiketsoort,b_False),show(idc_etiketsoort,b_False),
	enable(idc_met_adresetiketten,b_False),enable(idc_vermelding_van_uitsluiten,b_False),
	enable(idc_met_toernooinaam,b_False)]),
	!.
%END Etiketten oproep, e_Create

%BEGIN Etiketten oproep, idc_etiketsoort _CtlInfo
  dlg_etiketten_oproep_eh(_Win,e_Control(idc_etiketsoort,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	dlg_etikesoort_Create(_Win),
	!.
%END Etiketten oproep, idc_etiketsoort _CtlInfo

%BEGIN Etiketten oproep, idc_etik_oproep selchanged
  dlg_etiketten_oproep_eh(_Win,e_Control(idc_etik_oproep,_CtrlType,_CtrlWin,selchanged),0):-
	lbox_GetSel(_CtrlWin,_ItemList,IndexList),
	not(IndexList=[]),
	OKButton = win_GetCtlHandle(_Win, idc_ok),
	win_SetState(OKButton,[wsf_Enabled, wsf_Default]),	
	!.
  dlg_etiketten_oproep_eh(_Win,e_Control(idc_etik_oproep,_CtrlType,_CtrlWin,selchanged),0):-!,
	OKButton = win_GetCtlHandle(_Win, idc_ok),
	win_SetState(OKButton,[wsf_Disabled]),	
	!.
%END Etiketten oproep, idc_etik_oproep selchanged

  dlg_etiketten_oproep_eh(_,_,_):-!,fail.

%END_DLG Etiketten oproep


database - local
%determ rapporttext(string)
 rapedit(WINDOW,RCT, PNT)


%BEGIN_DLG Etiketten print
/**************************************************************************
	Creation and event handling for dialog: Etiketten print
**************************************************************************/

CONSTANTS

%BEGIN Etiketten print, CreateParms, 14:10:55-10.3.2001, Code automatically updated!
  dlg_etiketten_print_DlgType = wd_Modal
  dlg_etiketten_print_Title = "Print etiketten"
  dlg_etiketten_print_RCT = rct(40,32,222,218)
  dlg_etiketten_print_Flags = [wsf_Close,wsf_TitleBar,wsf_SizeBorder]
  dlg_etiketten_print_Help = idh_inhoud
%END Etiketten print, CreateParms

  getStartupText = 1000


PREDICATES

  dlg_etiketten_print_eh : EHANDLER
  dlg_etiketten_print_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_etiketten_print_update(DIALOG_VAL_LIST)

CLAUSES

  dlg_etiketten_print_Create(Parent, Answer):-
	etikPreview(Aantal),
	format(Tekst, "Totaal % etiketten", Aantal),
	get_TempDir(_, TempFile),
	file_str(TempFile, Text1),
	assert(editcontroltext(idc_etiketten_preview, Text1, b_False, ff_Fixed, b_True, 9, 1)),
	Parm = cast(long, Tekst),
%MARK Etiketten print, new variables

	dialog_Create(Parent,
		[
%BEGIN Etiketten print, WinDefList, 14:10:55-10.3.2001, Code automatically updated!
		 dlg(wdef(dlg_etiketten_print_DlgType,dlg_etiketten_print_RCT,dlg_etiketten_print_Title,u_DlgBase),dlg_etiketten_print_Flags),
		 customctl(wdef(wc_Custom,rct(0,3,178,156),"Etiketten preview",u_DlgBase),"pdcedit",idc_etiketten_preview,[wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(148,159,180,181),"&OK",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(111,159,143,169),"&Cancel",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(111,171,143,181),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_Text,rct(2,158,107,182),"Etiketten result",u_DlgBase),idct_etiketten_result,[wsf_AlignLeft])
%END Etiketten print, WinDefList
		],
		[
		],
		dlg_etiketten_print_eh,Parm,VALLIST,ANSWER),
	dlg_etiketten_print_handle_answer(ANSWER,VALLIST).

  dlg_etiketten_print_handle_answer(idc_ok,_VALLIST):-!,
	dlg_etiketten_print_update(_VALLIST).
  dlg_etiketten_print_handle_answer(idc_cancel,_):-!.  % Handle Esc and Cancel here
  dlg_etiketten_print_handle_answer(_,_):-
	errorexit().

  dlg_etiketten_print_update(_VALLIST):-
%BEGIN Etiketten print, Update controls, 14:10:55-10.3.2001, Code automatically updated!
%END Etiketten print, Update controls
	true.

%MARK Etiketten print, new events

%BEGIN Etiketten print, e_User
  dlg_etiketten_print_eh(_Win,e_User(_Id,_Ptr),0):-!,
	!.
%END Etiketten print, e_User

%BEGIN Etiketten print, e_Destroy
  dlg_etiketten_print_eh(_Win,e_Destroy,0):-!,
	retract(rapedit(_Win,_,_)),
	retract(editcontroltext(idc_etiketten_preview,_,_,_,_,_,_)),
	!.
%END Etiketten print, e_Destroy



%BEGIN Etiketten print, e_Create
  dlg_etiketten_print_eh(_Win,e_Create(_CreationData),0):-!,
	Tekst = cast(string, _CreationData),
	ResultWin = win_GetCtlHandle(_Win, idct_etiketten_result),
	win_SetText(ResultWin, Tekst),
	ClientRCT = win_GetClientRect(_Win),
	CtrlWin = win_GetCtlHandle(_Win, idc_etiketten_preview),
	Rct = win_GetOuterRect(CtrlWin),
	ClientRCT = rct(_,_,XCl,YCl),
	Rct = rct(X0,Y0,XCt,YCt),
	X1 = XCl - XCt,
	Y1 = YCl - YCt,
	assert(rapedit(_Win,rct(X0, Y0, X1, Y1),pnt(XCl,YCl))),
	!.
%END Etiketten print, e_Create

  dlg_etiketten_print_eh(_Win,e_Size(_N, _W),0):-!,
	CtrlWin = win_GetCtlHandle(_Win, idc_etiketten_preview),
	retract(rapedit(_Win,rct(X0,Y0,X1,Y1),pnt(XC1,YC1))),
	X11 = _N - X1,
	Y11 = _W - Y1,
	NewClient = rct(X0, Y0, X11, Y11),
	win_Move(CtrlWin, NewClient),
	Xd = _N - XC1,
	Yd = _W - YC1,
	%write("\nOffset Xd:",Xd," Yd:",Yd),
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
	PB4Win = win_GetCtlHandle(_Win, idct_etiketten_result),
	PB4Rct = win_GetOuterRect(PB4Win),
	PB4RctNew = rect_Offset(PB4Rct, Xd, Yd),
	PB4RctNew = rct(_,Tr,Rr,Br),
	win_Move(PB4Win, rct(2,Tr,Rr,Br)),
	win_Invalidate(_Win),
	assert(rapedit(_Win,rct(X0,Y0,X1,Y1),pnt(_N,_W))), !.
  dlg_etiketten_print_eh(_,_,_):-!,fail.

%END_DLG Etiketten print

predicates
procedure calc_regelsPerEtik(WINDOW, integer Aantal)
	update_preview(WINDOW)
procedure wreal2real(DIALOG_REAL, real)
procedure wint2int(DIALOG_INT, integer)
  testEtiket(integer, integer AantalRegels, slist)
  vulbladzijde(integer AantalV, integer AantalH, slist)
  vulbladzijde1(integer, slist HetEtiket)
  
clauses

etiketRegels(0).

wreal2real(r(Val), Val) :- !.
wreal2real(_, 0.0).

wint2int(i(Val),Val) :- !.
wint2int(_, 0).

calc_regelsPerEtik(_Win, Aantal) :-
	SVRes = vpi_GetAttrVal(attr_screen_vres),
	PVres = vpi_GetAttrVal(attr_printer_vres),
	etiketwaarden(_, _,_,_,_,_,_,_,_,_FontG),
	HoogteWin = win_GetCtlHandle(_Win, idc_hoogte),
	HVertS = win_GetText(HoogteWin),
	str_real(HVertS, VertSMM),
	EditListWin = win_GetCtlHandle(_Win, idc_fontgrootte),
	SizeStr = win_GetText(EditListWin),
	str_int(SizeStr, Size),
	Size1 = val(integer,(Size * PVres / SVRes)),
	FontS = win_GetFont(_Win),
	Font = font_Create(ff_Times,[],Size1),
	win_SetFont(_Win, Font),
	win_GetTextExtent(_Win, "JJansenSchoonhoveng",-1, _Width,Heigth),
	FontVInMM = Heigth * 25.4 / PVRes,
	Aantal = val(integer,(VertSMM / FontVInMM - 1)) - 1,
	assert(etiketRegels(Aantal)),
	win_SetFont(_Win, FontS), !.		% vergeet niet font weer terug te zetten..
calc_regelsPerEtik(_Win, 6).

update_preview(_Win) :-
	IDC_BOVENMARGE_VALUE = dialog_GetReal(_Win, idc_bovenmarge),
	wreal2real(IDC_BOVENMARGE_VALUE,TopMarg),
	IDC_LINKERMARGE_VALUE = dialog_Getreal(_Win, idc_linkermarge),
	wreal2real(IDC_LINKERMARGE_VALUE,LeftMarg),
	IDC_BREEDTE_VALUE = dialog_Getreal(_Win,idc_breedte),
	wreal2real(IDC_BREEDTE_VALUE,LabelWidth),
	IDC_HOOGTE_VALUE = dialog_Getreal(_Win,idc_hoogte),
	wreal2real(IDC_HOOGTE_VALUE,LabelHeight), 
	IDC_HORSPACE_VALUE = dialog_Getreal(_Win,idc_horspace),
	wreal2real(IDC_HORSPACE_VALUE,HorSpace),
	IDC_VERTSPACE_VALUE = dialog_Getreal(_Win,idc_vertspace),
	wreal2real(IDC_VERTSPACE_VALUE,VertSpace),
	IDC_VERTAANTAL_VALUE = dialog_Getint(_Win,idc_vertaantal),
	wint2int(IDC_VERTAANTAL_VALUE,NoVert),
	PreviewWin = win_GetCtlHandle(_Win, idc_custom),
	FormPage = 1,
	ColorPage = 16777215,
	IDC_HORAANT = dialog_Getint(_Win,idc_horaantal),
	wint2int(IDC_HORAANT,NoHor),
	%NoHor = 2, 	% vast
	Page_Roll = 0,
	preview_Set(PreviewWin,FormPage,ColorPage,LeftMarg,TopMarg,
		HorSpace,VertSpace,LabelWidth,
		LabelHeight,NoHor,NoVert,Page_Roll), !.

testEtiket(_, 0, ["|**|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |**|"]) :- !.
testEtiket(N, N, ["|**|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |**|"|Uit]) :-
  N1 = N - 1, !,
  testEtiket(N, N1, Uit).
testEtiket(N, AantalRegels, ["|**                                                                                       **|"|TEtiket]) :-
  AantalRegels1 = AantalRegels - 1, !,
  testEtiket(N, Aantalregels1, TEtiket).

vulbladzijde(0, _AantalH, _TEtiket) :- !.
vulbladzijde(AantalV, AantalH, TEtiket) :-
  vulbladzijde1(AantalH, TEtiket),
  AantalV1 = AantalV - 1,
  vulbladzijde(AantalV1, AantalH, TEtiket).

vulbladzijde1(0, _TEtiket) :- !.
vulbladzijde1(AantalH, TEtiket) :-
  etiketBuild(TEtiket),
  AantalH1 = AantalH - 1, !,
  vulbladzijde1(AantalH1, TEtiket).

%BEGIN_DLG Etikesoort
/**************************************************************************
	Creation and event handling for dialog: Etikesoort
**************************************************************************/

CONSTANTS

%BEGIN Etikesoort, CreateParms, 15:54:26-19.12.2003, Code automatically updated!
  dlg_etikesoort_DlgType = wd_Modal
  dlg_etikesoort_Title = "Etiketindeling"
  dlg_etikesoort_RCT = rct(40,32,255,136)
  dlg_etikesoort_Flags = [wsf_Close,wsf_TitleBar]
  dlg_etikesoort_Help = idh_etiketsoort
%END Etikesoort, CreateParms

PREDICATES

  dlg_etikesoort_eh : EHANDLER
  dlg_etikesoort_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_etikesoort_update(DIALOG_VAL_LIST)

CLAUSES

  dlg_etikesoort_Create(Parent):-

	cycleOnd('+'),			% ????
	loadPrintConfig1(),
	etiketwaarden(IDC_BOVENMARGE_VALUE,IDC_LINKERMARGE_VALUE,IDC_BREEDTE_VALUE,
		IDC_HOOGTE_VALUE,IDC_HORSPACE_VALUE,IDC_VERTSPACE_VALUE,IDC_VERTAANTAL_VALUE,IDC_HORAANTAL_VALUE,_Aantal,FontG),
	IDC_FONTGROOTTE_ITEMLIST = ["8","9","10","11","12","13","14"],	
	str_int(IDC_FONTGROOTTE_DEFAULT,FontG),
        class_Create("preview",ek_preview_handler),
%MARK Etikesoort, new variables

	dialog_Create(Parent,
		[
%BEGIN Etikesoort, WinDefList, 15:54:26-19.12.2003, Code automatically updated!
		 dlg(wdef(dlg_etikesoort_DlgType,dlg_etikesoort_RCT,dlg_etikesoort_Title,u_DlgBase),dlg_etikesoort_Flags),
		 ctl(wdef(wc_Edit,rct(50,2,71,12),"",u_DlgBase),idc_bovenmarge,[wsf_AlignLeft,wsf_Group,wsf_TabStop,wsf_AutoHScroll]),
		 ctl(wdef(wc_Edit,rct(50,12,71,22),"",u_DlgBase),idc_linkermarge,[wsf_AlignLeft,wsf_Group,wsf_TabStop,wsf_AutoHScroll]),
		 ctl(wdef(wc_Edit,rct(50,22,71,32),"",u_DlgBase),idc_breedte,[wsf_AlignLeft,wsf_Group,wsf_TabStop,wsf_AutoHScroll]),
		 ctl(wdef(wc_Edit,rct(50,32,71,42),"",u_DlgBase),idc_hoogte,[wsf_AlignLeft,wsf_Group,wsf_TabStop,wsf_AutoHScroll]),
		 ctl(wdef(wc_Edit,rct(50,42,71,52),"",u_DlgBase),idc_horspace,[wsf_AlignLeft,wsf_Group,wsf_TabStop,wsf_AutoHScroll]),
		 ctl(wdef(wc_Edit,rct(50,52,71,62),"",u_DlgBase),idc_vertspace,[wsf_AlignLeft,wsf_Group,wsf_TabStop,wsf_AutoHScroll]),
		 ctl(wdef(wc_Edit,rct(50,62,71,72),"",u_DlgBase),idc_vertaantal,[wsf_AlignLeft,wsf_Group,wsf_TabStop,wsf_AutoHScroll]),
		 ctl(wdef(wc_Edit,rct(50,72,71,82),"",u_DlgBase),idc_horaantal,[wsf_AlignLeft,wsf_Group,wsf_TabStop,wsf_AutoHScroll]),
		 ctl(wdef(wc_LBoxEdit,rct(192,13,212,77),"fontgrootte",u_DlgBase),idc_fontgrootte,[wsf_Group,wsf_TabStop,wsf_VScroll]),
		 ctl(wdef(wc_PushButton,rct(6,85,51,96),"&Beginwaarde",u_DlgBase),idc_beginwaarde,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(59,85,120,96),"&Proefvel printen",u_DlgBase),idc_proefpagina,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(180,78,212,100),"&OK",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(145,78,177,88),"&Cancel",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(145,90,177,100),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_Text,rct(6,2,50,12),"Bovenmarge",u_DlgBase),idct_bovenmarge,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(6,12,50,23),"Zijmarge",u_DlgBase),idct_linker_marge,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(6,23,50,34),"Breedte",u_DlgBase),idct_breedte,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(6,34,50,44),"Hoogte",u_DlgBase),idct_hoogte,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(6,44,50,54),"Hor ruimte",u_DlgBase),idc_hor_ruimte,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(6,54,50,64),"Vert ruimte",u_DlgBase),idc_vert_ruimte,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(6,64,50,74),"Vert aantal",u_DlgBase),idc_vert_aantal,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(6,73,50,81),"Kolommen",u_DlgBase),idct_aantal_kollommen_2,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(164,3,211,11),"lettergrootte:",u_DlgBase),idct_font_grootte,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(163,33,212,51),"x regels per etiket",u_DlgBase),idct_x_regels_per_etiket,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(72,12,85,20),"mm",u_DlgBase),idct_mm,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(72,2,85,10),"mm",u_DlgBase),idc_etikesoort_25,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(72,22,86,30),"mm",u_DlgBase),idc_etikesoort_26,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(72,32,85,40),"mm",u_DlgBase),idc_etikesoort_27,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(72,42,85,50),"mm",u_DlgBase),idc_etikesoort_28,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(72,52,86,60),"mm",u_DlgBase),idc_etikesoort_29,[wsf_AlignLeft]),
		 customctl(wdef(wc_Custom,rct(91,2,154,76),"Custom",u_DlgBase),"preview",idc_custom,[wsf_Group])
%END Etikesoort, WinDefList
		],
  		[
%BEGIN Etikesoort, ControlList, 15:54:26-19.12.2003, Code automatically updated!
		df(idc_bovenmarge,editreal(IDC_BOVENMARGE_VALUE,[mandatory]),dlg_prompt(idct_bovenmarge)),
		df(idc_linkermarge,editreal(IDC_LINKERMARGE_VALUE,[mandatory]),dlg_prompt(idct_linker_marge)),
		df(idc_breedte,editreal(IDC_BREEDTE_VALUE,[mandatory]),dlg_prompt(idct_breedte)),
		df(idc_hoogte,editreal(IDC_HOOGTE_VALUE,[mandatory]),dlg_prompt(idct_hoogte)),
		df(idc_horspace,editreal(IDC_HORSPACE_VALUE,[mandatory]),nopr),
		df(idc_vertspace,editreal(IDC_VERTSPACE_VALUE,[mandatory]),dlg_prompt(idc_vert_ruimte)),
		df(idc_vertaantal,editint(IDC_VERTAANTAL_VALUE,[mandatory]),dlg_prompt(idc_vert_aantal)),
		df(idc_horaantal,editint(IDC_HORAANTAL_VALUE,[]),nopr),
		df(idc_fontgrootte,listedit(IDC_FONTGROOTTE_ITEMLIST,IDC_FONTGROOTTE_DEFAULT),nopr)
%END Etikesoort, ControlList
		],
		dlg_etikesoort_eh,0,VALLIST,ANSWER),
	class_Destroy("preview"),
	dlg_etikesoort_handle_answer(ANSWER,VALLIST).

  dlg_etikesoort_handle_answer(idc_ok,_VALLIST):-!,
	dlg_etikesoort_update(_VALLIST).
  dlg_etikesoort_handle_answer(idc_cancel,_):-!.  % Handle Esc and Cancel here
  dlg_etikesoort_handle_answer(_,_):-
	errorexit().

  dlg_etikesoort_update(_VALLIST):-
%BEGIN Etikesoort, Update controls, 15:54:26-19.12.2003, Code automatically updated!
	_IDC_BOVENMARGE_VALUE = dialog_VLGetreal(idc_bovenmarge,_VALLIST),
	_IDC_LINKERMARGE_VALUE = dialog_VLGetreal(idc_linkermarge,_VALLIST),
	_IDC_BREEDTE_VALUE = dialog_VLGetreal(idc_breedte,_VALLIST),
	_IDC_HOOGTE_VALUE = dialog_VLGetreal(idc_hoogte,_VALLIST),
	_IDC_HORSPACE_VALUE = dialog_VLGetreal(idc_horspace,_VALLIST),
	_IDC_VERTSPACE_VALUE = dialog_VLGetreal(idc_vertspace,_VALLIST),
	_IDC_VERTAANTAL_VALUE = dialog_VLGetint(idc_vertaantal,_VALLIST),
	_IDC_FONTGROOTTE_DEFAULT = dialog_VLGetListEdit(idc_fontgrootte,_VALLIST),
	_IDC_HORAANTAL_VALUE = dialog_VLGetint(idc_horaantal,_VALLIST),
%END Etikesoort, Update controls
	%retract(etiketwaarden(_,_,_,_,_,_,_,_,_)),
	etiketRegels(Aantal),
	str_int(_IDC_FONTGROOTTE_DEFAULT,FontG),
	assert(etiketwaarden(_IDC_BOVENMARGE_VALUE,_IDC_LINKERMARGE_VALUE,_IDC_BREEDTE_VALUE,
		_IDC_HOOGTE_VALUE,_IDC_HORSPACE_VALUE,_IDC_VERTSPACE_VALUE,_IDC_VERTAANTAL_VALUE,_IDC_HORAANTAL_VALUE,Aantal,FontG)),
	true.

%MARK Etikesoort, new events

%BEGIN Etikesoort, idc_proefpagina _CtlInfo
  dlg_etikesoort_eh(_Win,e_Control(idc_proefpagina,_CtrlType,_CtrlWin,_CtlInfo),0):-
	_IDC_BOVENMARGE_VALUE = dialog_Getreal(_Win, idc_bovenmarge),
	_IDC_LINKERMARGE_VALUE = dialog_Getreal(_Win, idc_linkermarge),
	_IDC_BREEDTE_VALUE = dialog_Getreal(_Win, idc_breedte),
	_IDC_HOOGTE_VALUE = dialog_Getreal(_Win, idc_hoogte),
	_IDC_HORSPACE_VALUE = dialog_Getreal(_Win, idc_horspace),
	_IDC_VERTSPACE_VALUE = dialog_Getreal(_Win, idc_vertspace),
	_IDC_VERTAANTAL_VALUE = dialog_Getint(_Win, idc_vertaantal),
	_IDC_FONTGROOTTE_DEFAULT = dialog_GetListEdit(_Win, idc_fontgrootte),
	_IDC_HORAANTAL_VALUE = dialog_Getint(_Win, idc_horaantal),
	etiketRegels(Aantal),
	str_int(_IDC_FONTGROOTTE_DEFAULT,FontG),
	assert(etiketwaarden(_IDC_BOVENMARGE_VALUE,_IDC_LINKERMARGE_VALUE,_IDC_BREEDTE_VALUE,
		_IDC_HOOGTE_VALUE,_IDC_HORSPACE_VALUE,_IDC_VERTSPACE_VALUE,_IDC_VERTAANTAL_VALUE,_IDC_HORAANTAL_VALUE,Aantal,FontG)),
	etik_reset(),
	etiketwaarden(_,_,_,_,_,_,i(AantalV),i(AantalH),AantalRegels,_),
	testEtiket(AantalRegels, AantalRegels, TEtiket),
	vulbladzijde(AantalV, AantalH, TEtiket),
	dlg_etiketten_print_Create(_Win, Answer),
	printetiketten(Answer, ja),
	!.
%END Etikesoort, idc_proefpagina _CtlInfo

%BEGIN Etikesoort, idc_horaantal modified
  dlg_etikesoort_eh(_Win,e_Control(idc_horaantal,_CtrlType,_CtrlWin,modified),0):-!,
	update_preview(_Win),
	!.
%END Etikesoort, idc_horaantal modified

%BEGIN Etikesoort, idc_help _CtlInfo
  dlg_etikesoort_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Etiketsoort"),
	!.
%END Etikesoort, idc_help _CtlInfo

%BEGIN Etikesoort, idc_vertaantal modified
  dlg_etikesoort_eh(_Win,e_Control(idc_vertaantal,_CtrlType,_CtrlWin,modified),0):-!,
	update_preview(_Win),
	!.
%END Etikesoort, idc_vertaantal modified

%BEGIN Etikesoort, idc_vertspace modified
  dlg_etikesoort_eh(_Win,e_Control(idc_vertspace,_CtrlType,_CtrlWin,modified),0):-!,
	update_preview(_Win),
	!.
%END Etikesoort, idc_vertspace modified

%BEGIN Etikesoort, idc_horspace modified
  dlg_etikesoort_eh(_Win,e_Control(idc_horspace,_CtrlType,_CtrlWin,modified),0):-!,
	update_preview(_Win),
	!.
%END Etikesoort, idc_horspace modified

%BEGIN Etikesoort, idc_breedte modified
  dlg_etikesoort_eh(_Win,e_Control(idc_breedte,_CtrlType,_CtrlWin,modified),0):-!,
	update_preview(_Win),
	!.
%END Etikesoort, idc_breedte modified

%BEGIN Etikesoort, idc_linkermarge modified
  dlg_etikesoort_eh(_Win,e_Control(idc_linkermarge,_CtrlType,_CtrlWin,modified),0):-!,
	update_preview(_Win),
	!.
%END Etikesoort, idc_linkermarge modified

%BEGIN Etikesoort, idc_bovenmarge modified
  dlg_etikesoort_eh(_Win,e_Control(idc_bovenmarge,_CtrlType,_CtrlWin,modified),0):-!,
        update_preview(_Win),
	!.
%END Etikesoort, idc_bovenmarge modified

%BEGIN Etikesoort, e_Create
  dlg_etikesoort_eh(_Win,e_Create(_CreationData),0):-!,
	calc_regelsPerEtik(_Win, Aantal),
	format(Tekst,"%u regels per etiket",Aantal),
	TekstWin = win_GetCtlHandle(_Win, idct_x_regels_per_etiket),
	win_SetText(TekstWin, Tekst),
	!.
%END Etikesoort, e_Create

%BEGIN Etikesoort, idc_fontgrootte modified
  dlg_etikesoort_eh(_Win,e_Control(idc_fontgrootte,_CtrlType,_CtrlWin,modified),0):-!,
	win_PostEvent(_Win, e_User(1, 0)),
	!.
%END Etikesoort, idc_fontgrootte modified

%BEGIN Etikesoort, e_User
  dlg_etikesoort_eh(_Win,e_User(1,_Ptr),0):-		% vertraagde fontgrootte!
	calc_regelsPerEtik(_Win, Aantal),
	format(Tekst,"%u regels per etiket",Aantal),
	TekstWin = win_GetCtlHandle(_Win, idct_x_regels_per_etiket),
	win_SetText(TekstWin, Tekst), !.
	%update_preview(_Win), !.
%END Etikesoort, e_User

%BEGIN Etikesoort, idc_fontgrootte selchanged
  dlg_etikesoort_eh(_Win,e_Control(idc_fontgrootte,_CtrlType,_CtrlWin,selchanged),0):-!,
	win_PostEvent(_Win, e_User(1, 0)),	% indirecte update
	!.
%END Etikesoort, idc_fontgrootte selchanged

%BEGIN Etikesoort, idc_hoogte modified
  dlg_etikesoort_eh(_Win,e_Control(idc_hoogte,_CtrlType,_CtrlWin,modified),0):-!,
	update_preview(_Win),
	!.
%END Etikesoort, idc_hoogte modified


%BEGIN Etikesoort, idc_beginwaarde _CtlInfo
  dlg_etikesoort_eh(_Win,e_Control(idc_beginwaarde,_CtrlType,_CtrlWin,_CtlInfo),0):-
	% etiketwaarden(r(13.5),r(4.0),r(99.1),r(33.9),r(3.0),r(0.0),i(7)).
	dialog_SetReal(_Win,idc_bovenmarge, r(13.5)),
	dialog_SetReal(_Win, idc_linkermarge, r(4.0)),
	dialog_SetReal(_Win,idc_breedte,r(99.1)),
	dialog_SetReal(_Win,idc_hoogte,r(33.9)),
	dialog_SetReal(_Win,idc_horspace,r(3.0)),
	dialog_SetReal(_Win,idc_vertspace,r(0.0)),
        dialog_SetInt(_Win,idc_vertaantal,i(7)),
        dialog_SetInt(_Win,idc_horaantal,i(2)),
        dialog_SetListEdit(_Win,idc_fontgrootte,"10"),
        update_preview(_Win),
	calc_regelsPerEtik(_Win, Aantal),
	format(Tekst,"%u regels per etiket",Aantal),
	TekstWin = win_GetCtlHandle(_Win, idct_x_regels_per_etiket),
	win_SetText(TekstWin, Tekst),
	!.
%END Etikesoort, idc_beginwaarde _CtlInfo

  dlg_etikesoort_eh(_,_,_):-!,fail.

%END_DLG Etikesoort

% ##### betaal sevice routines
predicates
nondeterm  betaal_grp(string)
volgorde(integer, integer)

clauses
betaal_grp(Groep) :-		% 1 = betaal
  spsrt(Sex, Groep, _, _, _, _, _, _),
  getbacktrack(Here),
  %sp1(_,Sex,_,_,_,_,Bet,_,_,_,_,_,_,_,_,_,_,_,_),
  sp2(_,Sex,_,_,_ ,_,_,nee,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  cutbacktrack(Here).

volgorde(idc_alfabetisch,0).
volgorde(idc_postcode, 1).
%BEGIN_DLG Etiketten betaal
/**************************************************************************
	Creation and event handling for dialog: Etiketten betaal
**************************************************************************/

CONSTANTS

%BEGIN Etiketten betaal, CreateParms, 12:49:48-28.7.1999, Code automatically updated!
  dlg_etiketten_betaal_DlgType = wd_Modal
  dlg_etiketten_betaal_Title = "Betaaletiketten"
  dlg_etiketten_betaal_RCT = rct(40,32,173,172)
  dlg_etiketten_betaal_Flags = [wsf_Close,wsf_TitleBar]
  dlg_etiketten_betaal_Help = idh_inhoud
%END Etiketten betaal, CreateParms

PREDICATES

  dlg_etiketten_betaal_eh : EHANDLER
  dlg_etiketten_betaal_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST, SList, integer Orde)
  dlg_etiketten_betaal_update(DIALOG_VAL_LIST, Slist, integer Orde)

CLAUSES

  dlg_etiketten_betaal_Create(Parent, Selectie, Orde):-

	findall(Groep,betaal_grp(Groep),IDC_ETIK_SPSOORTEN_ITEMLIST),
	IDC_ETIK_SPSOORTEN_SELECT = [],
	%IDC_ETIK_SPSOORTEN_ITEMLIST = Groepen,
	IDCT_SELECTEER_SPELERSOORTEN_TITLE = "Selecteer spelersoorten",
	POSTCODEALFA = idc_postcode,
%MARK Etiketten betaal, new variables

	dialog_Create(Parent,
		[
%BEGIN Etiketten betaal, WinDefList, 12:49:48-28.7.1999, Code automatically updated!
		 dlg(wdef(dlg_etiketten_betaal_DlgType,dlg_etiketten_betaal_RCT,dlg_etiketten_betaal_Title,u_DlgBase),dlg_etiketten_betaal_Flags),
		 ctl(wdef(wc_LBox,rct(4,14,97,98),"",u_DlgBase),idc_etik_spsoorten,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_MultiSelect]),
		 ctl(wdef(wc_RadioButton,rct(6,111,54,121),"Postcode",u_DlgBase),idc_postcode,[wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_RadioButton,rct(6,121,54,131),"Alfabetisch",u_DlgBase),idc_alfabetisch,[wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_PushButton,rct(99,114,131,137),"&OK",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(63,126,95,136),"&Cancel",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(63,100,111,112),"Etiketsoort",u_DlgBase),idc_etiketsoort,[wsf_Group,wsf_TabStop]),
		 customctl(wdef(wc_Custom,rct(102,7,128,98),"Progress",u_DlgBase),"progress",idc_progress,[wsf_Group,wsf_TabStop,wsf_Invisible]),
		 ctl(wdef(wc_Text,rct(4,4,81,14),"Selecteer spelersoorten",u_DlgBase),idct_selecteer_spelersoorten,[wsf_AlignLeft]),
		 ctl(wdef(wc_GroupBox,rct(4,101,56,135),"Volgorde",u_DlgBase),idc_volgorde,[])
%END Etiketten betaal, WinDefList
		],
  		[
%BEGIN Etiketten betaal, ControlList, 12:49:48-28.7.1999, Code automatically updated!
		df(idc_etik_spsoorten,listbox(IDC_ETIK_SPSOORTEN_ITEMLIST,IDC_ETIK_SPSOORTEN_SELECT),nopr),
		df(idct_selecteer_spelersoorten,statictext(IDCT_SELECTEER_SPELERSOORTEN_TITLE),nopr),
		df(POSTCODEALFA,radiobuttongroup([idc_postcode,idc_alfabetisch]),nopr)
%END Etiketten betaal, ControlList
		],
		dlg_etiketten_betaal_eh,0,VALLIST,ANSWER),
	dlg_etiketten_betaal_handle_answer(ANSWER,VALLIST,Selectie,Orde).

  dlg_etiketten_betaal_handle_answer(idc_ok,_VALLIST, Selectie, Orde):-!,
	dlg_etiketten_betaal_update(_VALLIST, Selectie, Orde).
  dlg_etiketten_betaal_handle_answer(idc_cancel,_,[],0):-!.  % Handle Esc and Cancel here
  dlg_etiketten_betaal_handle_answer(_,_,_,_):-
	errorexit().

  dlg_etiketten_betaal_update(_VALLIST,_IDC_ETIK_SPSOORTEN_ITEMLIST, Orde ):-
%BEGIN Etiketten betaal, Update controls, 12:49:48-28.7.1999, Code automatically updated!
	dialog_VLGetListBox(idc_etik_spsoorten,_VALLIST,_IDC_ETIK_SPSOORTEN_ITEMLIST,_IDC_ETIK_SPSOORTEN_SELECT),
	_POSTCODEALFA = dialog_VLGetRadiobutton(idc_postcode,_VALLIST),
%END Etiketten betaal, Update controls
        volgorde(_POSTCODEALFA,Orde),
	true.

%MARK Etiketten betaal, new events

%BEGIN Etiketten betaal, idc_ok _CtlInfo
  dlg_etiketten_betaal_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	dialog_SetState(_Win, [show(idc_progress,b_True)]),
	ProgressWin = win_GetCtlHandle(_Win, idc_progress),
	progress_bar_set_value(ProgressWin, 0, 0),
	dialog_GetListBox(_Win, idc_etik_spsoorten,Gekozen_Groepen,_IDC_ETIK_SPSOORTEN_SELECT),
	_POSTCODEALFA = dialog_GetRadiobutton(_Win, idc_postcode),
        volgorde(_POSTCODEALFA,Orde),
	trap(etikettenBetaal1(ProgressWin,Gekozen_Groepen, Orde),_, true),
	win_Destroy(_Win),
	!.
%END Etiketten betaal, idc_ok _CtlInfo

%BEGIN Etiketten betaal, e_Create
  dlg_etiketten_betaal_eh(_Win,e_Create(_CreationData),0):-!,
	dialog_SetState(_Win, [enable(idc_OK, b_False)]),
	ProgressWin = win_GetCtlHandle(_Win, idc_progress),
	progress_bar_set_value(ProgressWin, 0, 0),
        %mailmerge(_),
	dialog_SetState(_Win, [enable(idc_etiketsoort, b_False),
			       show(idc_etiketsoort, b_False)]),
	win_SetText(_Win, "Selecteer betaalgegevens voor mailmerge"),
	!.
%END Etiketten betaal, e_Create

%BEGIN Etiketten betaal, idc_etik_spsoorten selchanged
  dlg_etiketten_betaal_eh(_Win,e_Control(idc_etik_spsoorten,_CtrlType,_CtrlWin,selchanged),0):-!,
	dialog_SetState(_Win, [enable(idc_OK, b_True)]),
	!.
%END Etiketten betaal, idc_etik_spsoorten selchanged

%BEGIN Etiketten betaal, idc_etiketsoort _CtlInfo
  dlg_etiketten_betaal_eh(_Win,e_Control(idc_etiketsoort,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	dlg_etikesoort_Create(_Win),
	!.
%END Etiketten betaal, idc_etiketsoort _CtlInfo

  dlg_etiketten_betaal_eh(_,_,_):-!,fail.

%END_DLG Etiketten betaal


%BEGIN_DLG internetget
/**************************************************************************
	Creation and event handling for dialog: internetget
**************************************************************************/

constants

%BEGIN internetget, CreateParms, 20:57:12-17.10.2008, Code automatically updated!
  dlg_internetget_DlgType = wd_Modal
  dlg_internetget_Title = "Niet online"
  dlg_internetget_RCT = rct(50,40,273,130)
  dlg_internetget_Flags = [wsf_Close,wsf_TitleBar]
  dlg_internetget_Help = idh_adresetiketten
  dlg_internetget_Font = "Arial"
  dlg_internetget_FSize = 9
%END internetget, CreateParms

database - internetget
  determ post(slist)
  determ file(string)
  single ret(integer)
  single resp(integer)

predicates

  dlg_internetget_eh : EHANDLER
  nietgevonden(integer)

clauses

ret(0).
resp(0).

nietGevonden(404) :-
  dlg_MessageBox( "Niet gevonden", "Bestand niet (meer) online aanwezig!\Een lokaal toernooibestand wordt in plaats daarvan geopend!\n\nKijk daarna in Backup, Laden, ...", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  webdir(M, _, B),
  assert(webdir(M, disk, B)),
  !.
nietGevonden(_).

internetGet(Post, File, Ret) :-
  selectjson(TAbun1php),
  providerX(_, _, TAbun1php, ProvM),
  deleteFile(File),
  Ret = downloadData(ProvM, File, Post,Resp, 0),
  assert(ret(Ret)),
  assert(resp(Resp)),
  Ret = 0,
  nietGevonden(Resp), !.
internetGet(Post, File, Ret) :-
  assert(post(Post)),
  assert(file(File)),
  TaskWin = vpi_GetTaskWin(),
  dlg_internetget_Create(TaskWin),
  ret(Ret).

  dlg_internetget_Create(Parent):-
	win_CreateDynDialog(Parent,
		[
%BEGIN internetget, WinDefList, 20:57:12-17.10.2008, Code automatically updated!
		 dlg_font(wdef(dlg_internetget_DlgType,dlg_internetget_RCT,dlg_internetget_Title,u_DlgBase),
		 	  dlg_internetget_Font,dlg_internetget_FSize,dlg_internetget_Flags),
		 ctl(wdef(wc_PushButton,rct(173,55,221,67),"&Afbreken",u_DlgBase),idc_ok,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(173,71,221,88),"&Doorgaan",u_DlgBase),idc_doorgaan,[wsf_Group,wsf_TabStop,wsf_Default]),
		 ctl(wdef(wc_Text,rct(1,2,171,87),"aanwijzingen",u_DlgBase),idct_aanwijzingen,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(206,2,220,12),"Ret",u_DlgBase),idct_ret,[wsf_AlignRight]),
		 ctl(wdef(wc_Text,rct(205,12,220,22),"Resp",u_DlgBase),idct_resp,[wsf_AlignRight])
%END internetget, WinDefList
		],dlg_internetget_eh,0).

%BEGIN internetget, idc_ok _CtlInfo
  dlg_internetget_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-!,
	webdir(M, _, B),
	assert(webdir(M, disk, B)),
	win_Destroy(_Win),
	!.
%END internetget, idc_ok _CtlInfo
%MARK internetget, new events

%BEGIN internetget, e_Create
  dlg_internetget_eh(_Win,e_Create(_CreationData),0):-
	ret(Ret),
	str_int(RetS, Ret),
	RWin = win_GetCtlHandle(_Win, idct_ret),
	win_SetText(RWin, RetS),
	resp(Resp),
	str_int(RespS, Resp),
	RespWin = win_GetCtlHandle(_Win, idct_resp),
	win_SetText(RespWin, RespS),
	AWin = win_GetCtlHandle(_Win, idct_aanwijzingen),
	T1 = "Kan geen online verbinding maken voor de huidige toernooimap!\n",
	T2 = "Mogelijke oorzaken:\n",
	T3 = "- Geen netwerkverbinding,\n",
	T4 = "- Blokkade door een firewall,\n",
	T5 = "- Server is down.\n\n",
	T6 = "Klik op doorgaan om opnieuw te proberen,\n",
	T7 = "of op afbreken om lokaal verder te gaan.",
	format(Text, "%s%s%s%s%s%s%s", T1, T2, T3, T4, T5, T6, T7),
	win_SetText(AWin, Text),
	!.
%END internetget, e_Create

%BEGIN internetget, e_Destroy
  dlg_internetget_eh(_Win,e_Destroy,0):-!,
	retractall(post(_)),
	retractall(file(_)),
	!.
%END internetget, e_Destroy

%BEGIN internetget, idc_doorgaan _CtlInfo
  dlg_internetget_eh(_Win,e_Control(idc_doorgaan,_CtrlType,_CtrlWin,_CtlInfo),0):-
	post(Post),
	file(File),
	deletefile(File),
	providerX(_, _, "tabun1.php", ProvM),
	not(geenServer(_)),
	Ret = downloadData(ProvM, File, Post, _, 0),
	checkServer(Ret),
	assert(ret(Ret)),
	win_Destroy(_Win),
	!.
%END internetget, idc_doorgaan _CtlInfo

  dlg_internetget_eh(_,_,_):-!,fail.

%END_DLG internetget

	% 21.8.2010 besloten dit niet af te maken vanwege het xml resultaatrapport!
database - rapporten
  single totaal(integer)
  single doenmee(integer)
  cattopcat(wedscat, wedscat)
  inschrijving(tgnst, integer Opgesteld, integer NietBetaald)
  single gTotaal(real)
  single gTotaalvoldaan(real)
  single totaalB(real)

predicates
  nondeterm deelnemer(sex, integer)
  procedure telinschrijvingen(catl, integer, integer, integer)
  procedure factorIs(wedstyp, string, integer)
  procedure isOpgesteld(catl, tgnst, integer JaIsEenNeeIsTwee)
  nondeterm betaald(tgnst, integer Betaald)

clauses

totaal(0).
totaalB(0).
doenmee(0).
gTotaal(0.0).
gTotaalvoldaan(0.0).

factorIs(e, "&nbsp;", 1) :- !.
factorIs(_, " x 2", 2).

isOpgesteld(CatList, Tgnst, 1) :-
  member(Cat, CatList),
  opgesteld(Cat, Tgnst), !.
isOpgesteld(_CatList, _Tgnst, 0).

betaald(Tgnst, 1) :-
  tgnstnum(Tgnst, SpNo),
  sp2(SpNo,_SpelerSrt,_Naam,_Tel,_Verh,_RatE,_RatD,ja,_UitKant, _ClubNo, _Adres, _Woonpl, _LidNr, _SterkS, _SterkD, _Geb, _Telwerk, _Telmobiel, _EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Inschr).

telinschrijvingen(CatList, _, _, _) :-
  member(Cat, CatList),
  opg(_Nr,Cat,Tgnst,_,_,_,_,_,_,_,_),  % niet al behandeld
  not(inschrijving(Tgnst, _, _)),
  isOpgesteld(CatList, Tgnst, JaisEen),
  findall(Uit, betaald(Tgnst, Uit), BTList),
  %write("<BR>BetaaldList: ",BTList),
  count(BTList, 0, Bet),
  assert(inschrijving(Tgnst, JaisEen, Bet)),
  fail.
telinschrijvingen(_CatList, Aantal, NietOpgesteld, Betaald) :-
  findall(T, inschrijving(T, _, _),TList),
  count(TList, 0, Aantal),
  findall(P, inschrijving(_, P, _), PList),
  telop(PList, 0, Opgesteld),
  findall(NB, inschrijving(_, _, NB), NBList),
  telop(NBList, 0, Betaald),
  NietOpgesteld = Aantal - Opgesteld,
  retractall(inschrijving(_,_, _)).
  
deelnemer(Sex, SpNo) :-
  sp2(SpNo,Sex,_Speler,_,_ ,_,_,_,_,_ClubS,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  getbacktrack(Here),
  sp2opg(SpNo, _, _, _, _, _),
  cutbacktrack(Here).

financieel() :-
  assert(totaal(0)),
  assert(doenmee(0)),
  assert(gTotaal(0.0)),
  assert(gTotaalVoldaan(0.0)),
  assert(totaalB(0.0)),
  retractall(cattopcat(_,_)),
  wc(Wc, _OnderdeeelID,_Orde,  _Naam, _EDG, _Abbr, _SexL, _SpStrk, _LftV, _LftTM, _HDG, _Schemasoort, _Keuring, _KNLTB, _Contr),
  bovencat(Wc, Top),
  assert(cattopcat(WC, Top)),
  fail.  
financieel() :-	% 20.8.2010 vervanging van financieel1 (c)
  getFolder(TDir,_,_,_),
  filenamePath(RapName, TDir, "financieelrapport.html"),
  openwrite(work, RapName),
  writedevice(work),
  rapport_css(CSSContent),
  write("\n<HTML><HEAD><TITLE>Financieel Rapport</TITLE></HEAD>"),
  write("\n<STYLE>"),
  write(CSSContent),
  write("</STYLE><BODY>"),
  write("\n<H3>Financiëel rapport</H3>"),
  write("\n<TABLE border=0 cellpadding=3 cellspacing=0>"),
  write("\n<TR><TH>Categorie</TH><TH>Aantal</TH><TH>Deelname</TH></TR>\n"),  
  %write(" Financiëel rapport\n\n Categorie           Aantal     Deelname\n ---------------------------------------"),
  spsrt(Sex, SrtNaam, _ManVr, _LftL, _LftU, _StrkL, _StrkU, _Verhl),
  findall(Sp, deelnemer(Sex, Sp), SpList),
  count(SpList, 0, Count),
  findall(SpNo, sp2(SpNo,Sex,_,_,_ ,_,_,_,_,_ClubS,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), Sp1List),
  count(Sp1List, 0, Count1),
  writef("<TR><TD>%s</TD><TD class='rechts'>%4d</TD><TD class='rechts'>%4d</TD></TR>\n",SrtNaam, Count1, Count),
  totaal(T),
  T1 = T + Count1,
  assert(totaal(T1)),
  doenmee(U),
  U1 = U + Count,
  assert(doenmee(U1)),
  fail. 
financieel()  :-
  totaal(T),
  doenmee(U),
  writef("<TR><TD>Totaal</TD><TD class='rechts'>%4d</TD><TD class='rechts'>%4d</TD</TR>\n", T, U),
  write("</TABLE><BR>\n"),
  assert(totaal(0)),
  assert(doenmee(0)),
  write("\n<TABLE border=0 cellpadding=3 cellspacing=0>"),
  write("\n<TR><TH>Onderdeel</TH><TH>Ingeschreven</TH><TH>Niet opgesteld</TH><TH>Contributie</TH><TH>Dubbel</TH><TH>Totaal</TH><TH>Voldaan</TH><TH>Begroting</TH></TR>\n"),
  wc(Wc, _OnderdeeelID,_Orde,  Naam, EDG, _Abbr, _SexL, _SpStrk, _LftV, _LftTM, _HDG, _Schemasoort, _Keuring, _KNLTB, Contr),
  not(isondercat(Wc)),
  findall(Catx, cattopcat(Catx, Wc), CatxList),
  telinschrijvingen(CatxList, Ingeschreven, NietOpgesteld, Betaald),
  factorIs(EDG, FactorS, Factor),
  Voldaan = Betaald * Contr,
  Totaal = (Ingeschreven - NietOpgesteld) * Contr * Factor,
  Begroting = Ingeschreven * Contr * Factor,
  writef("<TR><TD>%s</TD><TD class='middle'>%d</TD><TD class='middle'>%d</TD><TD class='middle'>%6.2f</TD><TD class='middle'>%s</TD><TD class='rechts'>%6.2f</TD><TD class='rechts'>%6.2f</TD><TD class='rechts'>%6.2f</TD><TR>",Naam,Ingeschreven,NietOpgesteld,Contr,FactorS, Totaal,Voldaan,Begroting),
  gTotaal(GT), GT1 = GT + Totaal, assert(gTotaal(GT1)),
  gTotaalVoldaan(GTvold), GTV = GTvold + Voldaan, assert(gTotaalVoldaan(GTv)),
  totaal(GI), GI1 = GI + Ingeschreven, assert(totaal(GI1)),
  totaalB(Beg), Beg1 = Beg + Begroting, assert(totaalB(Beg1)),
  fail.
financieel() :-
  gTotaal(GT), gTotaalVoldaan(GTV),totaal(GI),totaalB(Beg),
  writef("<TR><TD>Totaal</TD><TD class='middle'>%d</TD><TD colspan='3'></TD><TD class='rechts'>%6.2f</TD><TD class='rechts'>%6.2f</TD><TD class='rechts'>%6.2f</TD><TR>", GI,GT,GTV,Beg),
  write("</TABLE>"),
  write("NB Begroting onder de aanname dat iedere inschrijving wordt opgesteld.<BR>"),
  editie(),
  copyright(),
  write("</BODY></HTML>"),
  closefile(work),  
  getFolder(Pad, _, _, _),
  filenamePath(RapName, Pad, "financieelrapport.html"),
  format(URL1, "file:\\\\%s", RapName),
  write("\n",URL1),
  Null = cast(api_hwnd,0),
  api_ShellExecute(Null,"open",URL1,"","",1). 		% open in browser
  
  
/*%6.2f */