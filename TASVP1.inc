/*****************************************************************************

		Copyright (c) 1989 - 2010 De Lint Associates

 Project:  tasvpi
 FileName: TASVPI.INC
 Purpose:  Project's global declarations
 Written by: JdL
 Comments:
*****************************************************************************/

CONSTANTS
%  wbload = "webload.php"  
whitebackgr = 1		% voor datecontrol
versie = "2019.1A"
dlgdir_defaulttitle = "Selecteer bestaand back-up pad"
%BEGIN_CONST Project configuration constants, Code automatically updated!
  use_mdi = 1
  use_tbar = 1
  use_dlgpack = 1
  use_editor = 1
  use_message = 1
  use_progress = 1
  use_tabdlg = 1
  use_dlgdir = 1
  use_runtime = 1
%END_CONST
%  visual_project = 1

%include "vpi.con"
%include "vpi.dom"
%include "vpi.pre"
%include "vpiu.pre"
include "treeview\\treeview.ph"
IFDEF use_tabdlg
  IFNDEF use_dlgpack
    CONSTANTS
       use_dlgpack =1
  ENDDEF
ENDDEF
IFDEF use_dlgpack
  include "dialog\\dialog.dom"
  include "dialog\\dialog.pre"
ENDDEF
IFDEF use_message
  IFNDEF use_editor
    CONSTANTS
       use_editor =1
  ENDDEF
ENDDEF
IFDEF use_editor
  include "edit.dom"
  include "edit.pre"
ENDDEF
IFDEF use_message
  include "messages\\messages.pre"
ENDDEF
IFDEF use_tbar
  include "toolbar\\toolbar.dom"
  include "toolbar\\toolbar.pre"
ENDDEF
IFDEF use_tree
  include "tree\\vpitree.dom"
  include "tree\\vpitree.pre"
ENDDEF
IFDEF use_tabdlg
  include "tabdlg\\tabdlg.dom"
  include "tabdlg\\tabdlg.pre"
ENDDEF
IFDEF use_dlgdir
  IFNDEF use_ownerdraw
    CONSTANTS
       use_ownerdraw =1
  ENDDEF
  include "dlgdir\\dlgdir.pre"
ENDDEF
IFDEF use_ownerdraw
  include "owndraw\\owndraw.dom"
  include "owndraw\\owndraw.pre"
ENDDEF
IFDEF use_grid
  include "grid\\grid.dom"
  include "grid\\grid.pre"
ENDDEF
IFDEF use_date
    CONSTANTS
     language_dutch = 1
%    language_english = 1
%    language_danish  = 1
%    language_russian = 1
  ENDDEF
  %include "treeview\\treeview.ph"
IFDEF use_treebrowser
GLOBAL DOMAINS
  BROWSELIST_USERITEMKEY = INTEGER % User specified

  include "treebrws\\treebrws.dom"
  include "treebrws\\treebrws.pre"
ENDDEF
IFDEF use_listproperty
  include "property\\property.dom"
  include "property\\property.pre"
ENDDEF
IFDEF use_palette
  include "palette\\palette.dom"
  include "palette\\palette.pre"
ENDDEF
IFDEF use_progress
  include "progress\\progress.pre"
ENDDEF
IFDEF use_doc
  include "docstr.dom"
  include "html.pre"
  include "ipf.pre"
  include "rtf.pre"
  include "errhndl.pre"
ENDDEF
IFDEF use_sql
  include "sqlbind.dom"
  include "sqlbind.pre"
ENDDEF
IFDEF use_odbc
  include "odbc.dom"
  include "odbc.pre"
ENDDEF
IFDEF use_socket
  IFNDEF use_runtime
    CONSTANTS
      use_runtime = 1
  ENDDEF
ENDDEF
IFDEF use_runtime
  include "pdcrunt.dom"
  include "pdcrunt.pre"
ENDDEF
IFDEF use_socket
  include "include\\pdcsock.dom"
  include "include\\pdcsock.con"
  include "include\\pdcsock.pre"
ENDDEF

include "apicalls.dom"
include "apicalls.con"
include "apicalls.pre"
%include "apicalls\\win\\apicalls.con"
%include "apicalls\\win\\apicalls.pre"
%include "apicalls\\win\\apicomm.dom"
%include "apicalls\\win\\apicomm.con"
%include "apicalls\\win\\apicomm.pre"
include "tasapi.pre"
%  include "apicalls\\win\\smapi1\\smapi.con"
%  include "apicalls\\win\\smapi1\\smapi.dom"
%  include "apicalls\\win\\smapi1\\smapi.pre"
include "date1\\date.dom"
  CONSTANTS
     language_dutch = 1
%    language_english = 1
%    language_danish  = 1
%    language_russian = 1
  include "date1\\date.pre"

/*****************************************************************************
			User's Declarations
*****************************************************************************/
constants
  help_file_naam = "TAS.HLP"
  err_file_naam  = "PROLOG.ERR"		% 17.8.2000
  edt_file_naam  = "TASEDIT.HLP"

  
  csvconst = ';'
  titeldeelc = "ToernooiAssistent -"
  briefconst      = "tasbrief"
  etikconst       = "tasetik"
  agendaconst     = "Agendagegevens"
  tasvpiinix      = "TASVP1.INI"
  logconst        = "TASN.LOG"     % de logfile
  logconstw       = "tasweb.log"     % de logfile voor het web
  jourconst       = "journaal.txt" % de informatie
  fctconst        = "TASN.DAT"     % de factfile
  nawconst        = "TASN.NAW"     % de naw file
  %nstconst        = "TASN.NST"     % de instellingen 
  licconst        = "tasi.nst"     % voor licentie-info in root
  tmpconstx        = "tijdelijk.TMP"     % de tijdelijke file 
  iuconst         = "TASSP."	   % export/import
  iuclconst       = "CLUBS."       % export/import clubtabel
  optimconst      = $380           % optimaliseringsperiode 
  nieuwespconst   = "+ Nieuwe Speler"
  mode_binary     = 0
  mode_text       = 1
  enkelvoudig     = 1		% moet = 1 blijven!
  meervoudig      = 20		% magaangepast worden	 5.7.2002 was 10
ifdef os_os2
  achtergrond     = 113          % 7 voor zwart, 48-96 voor pictures
elsedef
  achtergrond     = 7
enddef
  inschrijven = 1
  partnerselectie = 2
  betalenboeken = 3
%mijnoproepen  = 1
%mijnafzeggen  = 2
%mijncorrectie = 3
%mijnverzet    = 4
upd_verhinderingen = 21
upd_opglist        = 22
upd_opmerkingen    = 33
alleplanning = $7FFFFFFF	% een minder in ieder geval
uitslagenvenster = 1
oproepvenster    = 2
spelersvenster   = 3
oproepcorvenster = 4
locatievenster   = 5
planvenster      = 6
fixeervenster    = 7
kladblokvenster  = 8
spelervenster    = 9
emailgroepvenster = 10
opgavenvenster = 11
onderdeelselectvenster = 12
indeelvenster = 13
kenmerkvenster = 14
clubvenster = 15
browsevenster = 16
plannenvenster = 17
banentoolvenster = 18
rapportvenster = 19
betaalvenster = 20
editorvenster = 21
situatievenster = 22
browserhelpvenster = 23
upd_toolb = 101
id_wedstrijd_plan    = 609
id_wedstrijd_wis_plan = 610
id_Wedstrijd_fixeer  = 611
id_Email = 612
id_Wedstrijd_overzicht        = 613
%timerinterval = 120000	% twee minuten
timerintervalspeedy = 30000	% halve minuut!!
timerintervalknipper = 500
id_menufuzz = 12000 % hoger dan vpi.con!
dotoernooiload = 2
  
global domains /* voor alle programma's */
  oprafztype = mijnoproepen ; mijnafzeggen ; mijncorrectie ; mijnverzet
  vehikel = direct; outlook
  meeltype = oproep ; bulk ; enkel ; sms ; smsbulk
  browserisvoor = schema ; web
  indicator   = integer
  overbl      = alle; loslopend
  hoevaak = vaak; mindervaak; afentoe
%  formaliteit = informeel %; formeel
%  jeugda      = jeugd; atoernooi
  ROW, COL, LEN, ATTR   = INTEGER
  STRINGLIST  = STRING*
  count_dom   = integer
  updown      = up; down
  INTEGERLIST = INTEGER*
  sl          = short*
  file    = printer ; work ; nawf ; logfile ; journaal % work files, logfile en nawfile (nu gebruikt als tweede workfile)
  persno  = integer
  perslst = persno*
  naam    = string
  sex     = integer
  sexlist = sex*
  sexe    = m ; v
  sexelst = sexe*
  janee   = ja ; nee ; omheteven
  tijd    = ulong     % dag, uur, kwartieren   1 dag = 0x60  of 96 decimaal
                        % bits: dddddddd duuuuukk   
  tijdl   = tijd*
  wrk     = z ; w       /* zaterdag,zondag of werkdag */
  period  = per(tijd,tijd)             /* van,tot*/
  verhl   = period* 
  wt      = wt(string,tijd,janee, string BaanPark, boolean BijWinst, string WasOp)
  wtl     = wt*
  sch     = struct s(integer,wedscat,tijd,janee,janee,char,tijd,string)
  schl    = sch*
  wedscat = integer
  catl    = wedscat*
  wcid    = wi(wedscat,string)
  wcidl   = wcid*
  lr      = integer
  wedid   = struct w(integer,wedscat,tijd,janee,lr,tgnst,boolean Vervolg, string Park)
  wl      = wedid*
  i       = i(tgnst,tgnst)
  il      = i*
  paspoort = geen ; groen
  rsoort  = poel ; afval ; nietingedeeld
  wedstyp = e  ; d ; g /* enkel - dubbel d - gemengd  Volgorde NIET veranderen */
  wtpl    = wedstyp*
  wedsex  = heren ; dames ; gemengd ; hdnvt
  ronde_dom = r(integer) ; winnaar ; finale ; kwartfin ; halvefin   
  tgnst   = s(persno) ; p(persno,persno) ; bye ; onbekend ; gelijk_spel ; openplaats(integer) ; pw(persno)
          % speler           paar 					 open plaats poules	partner wanted
  tgl     = tgnst*
  vandom  = rang(integer) ; pl3en4(integer) ; algemeen ; verliezer
  ipvdom  = plaats(integer) ; ipv(tgnst) ; algemeen	% voor koppelingen
  %opgavel = integer*
  %opgave  = opgave(integer,wedscat,tgnst,integer,integer,integer,integer)
  %opgavel = opgave*
  wps     = wp(integer,wedscat,integer)
  wpl     = wps*
  reallist = real*    
  clubno, clubnaam, distrct, adr, wnpl, knltb, geb, strks, strkd, tlfn, emailadres = string
  offset   = long  % index naar overige gegevens
  %speler   = speler(persno,sex,string,string,verhl,slist,janee,tijd,
  %		    clubno,adr,wnpl,knltb,strks,strkd,geb,tlfn,tlfn,emailadres,string,string)
%  nax      = naw(persno,clubno,adr,wnpl,knltb,strks,strkd,geb) ; nax(persno,clubno,adr,wnpl,knltb,strks,strkd,geb,tlfn,tlfn,email,string)
%  naxlist  = nax*
  rec      = struct rec(integer,   % opgavenummer
		 	integer,   % plaatsing (1,2,3, . . ., 0,0,0)
		 	integer,   % aantal byes
		 	integer,   % werkveld (=0)	% clubdeelname 
		 	string,    % club-id
		 	tijdl,	   % niet_verhinderd
		 	integer)   % voor het schudden
  rlist      = rec*  
  width      = integer
  spurg      = sp_urg(persno, integer)
  spurgl     = spurg*
  ulist      = ulong*
  beeld      = afdruk; web; upload; uploadftp
  kolomtype  = bondsnummer; speelsterkte; gebdatum; club; districtk; rating; wildcard; ranglijst; landcode
  kolomtypel = kolomtype*
  kolom      = kolom(kolomtype, string)
  koloml     = kolom*
  shselect = apart ; bijgevoegd ; apartmetfinale	% sheet selector types
  openhoe = lees ; mdi % (het log)
  %servertype = publiceren  % toernooimaponline of toernooiklapper

 zoekFunc = nondeterm (string, char) - (i, o)
 %wc      =  wc(wedscat,string,wedstyp,string,sexlist,real,wedscat) 
 %wclist  =  wc*
 dbasedomlist = dbasedom*
uitslstat = geen;walkover;gestaakt;remote
statmsg = statmsg(string Severity, string Nr, string Content)
statmsgl = statmsg*
teamstatus = geaccepteerd ; nietgeaccepteerd ; reserve ; na ; msgsnw(statmsgl) ; gekeurd ; msg(string, string, string, string, string, string) 
%teamstatus = geaccepteerd ; nietgeaccepteerd ; reserve ; na ; msg(string Severity, string Nr, string Content,string Severity2, string Nr2, string Content2) ; gekeurd
wcorigine = vanknltb ; vanzelf ; reserve(slist)
baantoew = gespeeldop(integer Baan, string Tijd) ; baan(integer Baan, string Tijd) ; reserveren(integer Baan, string Tijd) ; aanwezig(string Tijd) % string bevat tijdstip
lokatie = disk; inet; online
%dagstat = gisteren; vandaag; morgen ; overmorgen
middel = email; sms; telefoon; brief
middeltijd = mt(middel, string Boodschap, integer Tijd)
middelen = middeltijd*
oproepaan = bulk;collectie(ilist)
selectieniveau = eerste;tweede
uploadtype = update;replace
leeftijd = leeftijd(string,integer)  % dubbelgezicht
%mapplaats = disk ; web
%mb = mb(string, string) % adres, naam
ftype = bondsgedelegeerde; wedstrijdleider; hoofdscheidsrechter
toernooisoort = open ; nrt ; jrt ; anders ; rolstoel

global database
single   naam(string Naam, slist ToernooiNrs, string Datum, string WebTicket, string Distr, integer OokReserve)
toernooiK(string Nummer, toernooisoort)  % toernooitype
single tasversie(string)	% 19.12.2000 voor diagnose
   sp2(integer Spelernummer,
       sex Spelersoort,
       string Naam,
       string TelefoonA,
       verhl Verhinderingen,
       string RatingE,
       string RatingD, 
       janee Betaald,
       tijd Uitkantoor,
       string Clubno,
       string Adres,
       string Woonplaats,  % 12
       string Bondsnummer,
       string SetrkteEnkel,
       string SterkteDubbel,
       string Geboortedatum,
       string TelefoonB,
       string TelefoonC,
       string Emailadres,
       string Opm,
       janee Tshirt, 
       string RanglijstpositieE, 
       string Ranglijstpositied, 
       string BankNr, 
       string CheckGeg,
       string Log,   % incassoOK
       slist Reserve)
 /* ----- algemene partij gegevens ----- */
   /* wd#             opg#  opg#  score  spoor  */ 
   wd(integer,wedscat,tgnst,tgnst,string,tijd,string UserNote)
 /*------- geprogrammeerde wedstrijd ---------*/
%   w2(integer,wedscat,tijd,janee,janee,char,tijd) + baannummerstring
   w2(integer,wedscat,tijd,janee,janee,char,tijd,string Park)
 /*------- gespeelde wedstrijd ---------*/
   /* w3#             winn  punten1 punten2  status spoor */
   w3(integer,wedscat,tgnst,integer, integer, uitslstat, tijd)
 /*-------  opgave ----------*/
   /*  opg#    type          pltsng  ronde   uitsl  indelingsnum  LL,WC,..*/
   opg(integer Tijdstip,wedscat,tgnst,integer Plaatsing,integer Ronde,integer Uitsluiten,integer Indelingsnummer, string LLWC, string TeamID, teamstatus Status, string Reserve)
 /*------- baan rooster--*/
   /* bn#    aantal  */   
   bn(tijd,integer)
 /*--- afval/poule, omvang (cardinaliteit) & plaats in het schema ---*/
   pos(wedscat,integer,integer,rsoort)  
 /* wedstrijd categorie tabel                 verl ronde */ 
wc(wedscat, string OnderdeeelID, integer Orde, string Naam, wedstyp EDG, string Kort, sexlist SpSrt, string SpStrk, leeftijd LftV, leeftijd LftTM, wedsex HDG, string Schemasoort, wcorigine KNLTB, string ToernooiId, real Geld)  
%wc(27,"130783011",27,"Gemengd Dubbel 8 35+",g,"GD835+",[1,2],"8",leeftijd(35,"35"),leeftijd(99,"99"),gemengd,"",goedgekeurd,"tid",0.0)

%wc(wedscat,string,wedstyp,string,sexlist,real,wedscat)   % zie ook bl(...
%wcrit(wedscat, string Speelsterkte, integer LeeftV, integer LeeftTM, integer Reserve)	% 19.12.2000
%toernooionderdeel_t(wedscat, string Naam, string Speelsterkte, wedstyp ED, wedsex HDG, string Leeftijdvanaf, 
%         string LeeftijdTotEnMet, keurdom Status, string MeldingOnderdeel)
dag(ulong,string,wrk)        % kalender van het tournooi 
club(clubno,clubnaam,distrct)
single   oproepen(janee)
single   separatie(integer)
single   maxseparatie(integer)  	% 17.8.2016
single   dubbelVoorEnkel(janee)
single   dag_max(integer)
single   betaal_adm(janee)
single   door_plannen(janee)
single   openbaar(janee)
ws(integer, wedscat, tgnst, tijd)     % om af te zeggen ...
spsrt(sex, string Naam, sexe, integer LeeftV, integer LeeftT, string SpstV, string SpstT, slist Titel)
    					%slist is reserve 24.9.1999 was verhl
 % ___________ nieuw voor 3.1 ___(zie ook w2)___________________________
bl(wedscat, verhl)		% blokkeertijden voor onderdelen
blLft(integer LftVan, integer LftTot, verhl)	% leeftijdafhankelijke blokkering
single baan_adm(janee)		% baanadministratie bijhouden (= locatieplanning)
single dag_max_o(integer, integer)	% als dag_maximum, maar per onderdeel
kopl(wedscat, vandom, wedscat, ipvdom)
		% wedscat - onderdeel 	waarvanaf
		% vandom -    ,,     	plaats waarvanaf
		% wedscat -    ,,	waarnaartoe
		% ipvdom -    ,,	schemaplaats
single pouletelling(integer)
single duur(integer)		% verwachte wedstrijdduur
single singles(integer)		% max. aantal singles per persoon
single vereniging1(string Nr, string ToernooiOrg, string Aanhef, string Straat, string HuisNr, string Plaats, string Postcode, string Telefoon, string Email, string Website)
functionaris(ftype, string ToernooiId, string Naam, string Email, string MobielTel, string OverdagTel, string AvondTel)  
% aanhef
% huisnummer
% straat
% plaats
% postcode
% telefoon

determ kladblok(BOOLEAN, integer Tijd)		% kladblok popup na Tijd als er inhoud is.
klad(integer No, integer Tijdstip, string Tekst)	% kladblok inhoud
determ uitslagenable(string Pincode)
determ andereSexe(boolean)
single form(catl, integer Uiterste, string Retour, string Mededeling, 
	integer Maxond, BOOLEAN Inschrgeld, ilist ToernooidagenGeenVerh, 
	integer Maxverh, boolean Uitkant, string UitKantMax, integer VerhWijze, 
	boolean Voorweekend, ilist SpelerGeg, ilist PartnerGeg)
inschrijfcheck(string PCname, string Tid, integer DatumTijd, string ControleId)
  baantoew(integer, wedscat, baantoew)
  spelerAanwezig(integer SpNo, integer Tijdstip, integer, wedscat)
single banentool(janee, integer Maxbanen)
  baannaam(integer Nr, boolean Beschikbaar, string ParkToekomst, string BaanNaam, string NaamKort)
single banknummers(janee)  % naar 
single dagstatusinst(string Lettergrootte, integer AantalItemsPP) % naar vastewaarden?
determ webdatpositie(string DatDatum, integer WebLogPositie, integer Compress)
  autoweb(string Computername, integer,integer HtmlFtp,integer EvenWaarsch, string Userid, binary Password, string FTPadres, string Dir, integer Geenvaste, hoevaak, integer BijSluiten)
  algemeelX(meeltype, vehikel, string Onderwerp, string Template, boolean Vervolgweds)
  emailadresZelf(string Computername, string Email, string Naam)
  determ incassoeigennr(string)
  autobak(string PC, string)
  weblayout(beeld AfdrOfBrowseofUpload, string CSSnaam, ilist Flags, kolomtypeL, ilist FontGrootten, string Mededeling, catl Selected)
single schemapresel(catl)
single roosterinterval(integer)  % 15 minuten of 12 minuten (test)
single tshirt(boolean, string Tekst) 

global database - operationeel

determ  geenkleurPrint()
	bn_t(tijd)	% is global maar wordt alleen in tasplan en bn_for_c gebruikt
  wcdisp(wedscat, integer Fontsize)
%determ kompaan()			% er is een andere gebruiker gesignaleerd
determ logIsOpen(openhoe)
determ logPos(long)
determ logPosIni(long)
  oproepItem(integer LR, integer Index, oprafztype, integer WedsNo, wedscat, tgnst, integer LRinWeds, BOOLEAN vervolgweds)
determ persoon(WINDOW, integer Persno, string Naam, verhl Verhinderingen, sex Spsrt, string RatingE, string RatingD, integer Uitkantoor)
% !!!!!!
 editcontroltext(CTLID CtrlId, string, BOOLEAN wrap, FONTFAMILY, BOOLEAN ReadOnly, integer Size, integer Share)  % in tasprnt ...
determ dagstatusExport(string Wedstrijden, string Spelers)
determ kladblokcounter(integer)
%determ emailonderweg
	htmlnaam(wedscat, string, string Link)
%determ modified()	% savefacts necessary bij indelen
% namen(integer SpelerNo, integer PopupId) % in afval en poel
 vink(wedscat Echtecat,wedscat catnummer)
determ selectedClub(string Club)
determ dagstatusQueued(string File, integer Dag)
 spelerSelectieKrit(selectieniveau, slist Onderdelen, slist Soorten, integer HeeftNietOpg, 
		integer Betalen, integer LftVan, integer LftTot, slist Clubs, slist Sterktes,
		boolean Missend, boolean Opmerkingen, boolean NogIn,boolean UitL, boolean Zonderemail, tijdl DagenList)
determ schemaWin(window, wedscat)	% het huidige 
 spelerdrukte(integer SpNo, string Onderdelen, integer Afval, integer Poule, integer BijWinst, integer Plaatsing, integer Rank, integer slots, wedscat DruksteOnderdeel)
teDeleten(string FileNaam)
w2e(integer,wedscat, tgnst, integer SpNo, integer LR, tijd, middelen Verstuurd, middelen Bevestigd)	% email, telefoon etc.
determ compressRequired
determ geladen
%determ isveranderd % 
    uitslagenableE(integer, wedscat, integer Plan,tgnst, tgnst)
 geenServer(integer Status)
determ alleenlezen1
  algeweest(tgnst)


global database - operationeelvast  % tasi.nst
determ status(sl)
determ usb(janee)
determ sleuteltekst(string)
single toernooidir(string Pad, string bestand)
single webdir(string OLId, lokatie, string Bestandsnaam)


global database - vpiconf

%prconf(BINARY)
single prrichting(integer)  
%single fontsize(integer)
single opvullen(BOOLEAN)
single prvanaf(integer)
single paginasel(integer)
single waarderingI(BOOLEAN)
single prkleur(BOOLEAN)
single prkader(BOOLEAN)
single prplan(integer)	% al of niet de planning
single plantm(integer)  % de planning t/m
%single pouletel(BOOLEAN)	% obsolete
single toernooidirsave(string, string Bestand)	% 1.12.2010
toernooiDirs(string)
single webdirsave(string, lokatie, string) 
single koppel(BOOLEAN) % koppel schema's voor print
determ printer(BINARY)	% schema's obsolete
determ printer1(BINARY)	% overig obsolete
determ prfont(BINARY)
single minkolhor(string)	% min aantal kolommen in landschap mode (schema's)
single minkolvert(string)	% min aantal kolommen in portret mode (schema's)
single	etiketwaarden(DIALOG_REAL BovenMarge, DIALOG_REAL LinkerMarge,DIALOG_REAL Breedte,
	DIALOG_REAL Hoogte,DIALOG_REAL HorSpace,DIALOG_REAL VertSpace, DIALOG_INT VertAantal, DIALOG_INT HorAantal,
	integer RegelsPerEtiket,  integer FontGrootte)
determ venster(RCT)		% taskwin positie, size
single schemafontsize(integer)
single comPort(string)	% de dial poort
%single modem_enabled(BOOLEAN, string Comport, string Voorkiezen, integer Speed, BOOLEAN Toonpuls, BOOLEAN Geluid) % com#, dialinit, speed, init
single window_maximized(BOOLEAN)
single sheetmarges(integer Links, integer Rechts, integer Boven, integer Onder)
%determ taalstr(string)	% obsolete
single rapportfontgr(integer)
 vensterRCT(integer VensterID, RCT)
% weblayout(beeld AfdrOfBrowseofUpload, string CSSnaam, ilist Flags, kolomtypeL, ilist FontGrootten, string Mededeling, catl Selected)
 webprintlogo(string LogoFilename)
 %webupdate(webfrekwentie, integer LaatsteUpdate, boolean UpdateGedaan)
%single webrapport(integer Fontgrootte, integer ZWofKleur)
%single schemapresel(catl)
single schemadoel(beeld)	% dubbel!
%single aantekenen(boolean)
optiesL(integer Functie, boolean, boolean, boolean, boolean, boolean, boolean,boolean, boolean, boolean, boolean, boolean, boolean)
           % 0 is alle
           % 1 is deelnemers
           % 2 is niet passende soorten (filtercheck)
           % 3 is ontbrekende gegevens
           % 4 is formulier nog niet gezien
           % 5 is dubbel lidnummers
           % 6 is ontbrekende lidnummers
           % 7 is ongeldige lidnummers
           % 8 is niet-spelers
           % 9 is van één vereniging
           % 10 is uit onderdelen
           % 11 is die nog ergens in zitten
	   % 12 is die gegevens missen
           % 13 is die nergens aan meedoen
           % 14 is met opmerkingen
	   % 15 is met betaallijst
	   % 16 is via uitvoerselectie
determ kleurenB(integer Bezwaren, integer Urgentie)
determ emailsms(string Prov, integer Tegoed, string Afzenderemail, slist Reserve)


Global predicates

pdcedit_handler : EHANDLER 
%pdcedit_handler1: EHANDLER

procedure stopDBrefresh()
procedure startDBrefresh()
%determ	read_naw(offset, nax)           - (i,o)
%determ  store_naw(offset Oude, offset Nieuwe, nax)	- (i,o,i)
nondeterm  repeat_f(file) - (i)
procedure  action_term(char, dbasedom)	- (i, i)
nondeterm  repeat			% ga door tot het predicate niet meer fail-t
%procedure  logf(char,dbasedom,janee) - (i,i,i)	% schrijf feiten naar de log file ja = keep open
%determ flush_log()			% zorg dat log op disk staat
determ vorige(WINDOW) - (i)		% laden van een backup
procedure backup(string Email, janee Silent)			% maken van backup

nondeterm  sp2opg(integer,wedscat,integer,persno,integer,tgnst) 
			  - (i,i,o,o,o,o) (i,o,o,o,o,o) language stdcall
procedure  sortstring_c(slist,integer Begintbij1) - (i,i) language c % as "sortstr_c"

procedure file_compress(string Filenaam, string CompressedFilenaam) - (i,i) language c  % voeg zelf .gz toe aan de compressed bestandsnaam
procedure file_decompress(string Filenaam, string FnaamUit) - (i,i) language c
procedure integer upload2klapper(string URL, string Bestand, string Rapport, slist Vars) -
  (i, i, i, i) language c
procedure integer getbackup(string URL, string Bestand, slist Vars) - (i, i, i) language c
procedure integer fileupload(string URL, string Bestand, string Rapport, slist Vars) - (i, i, i, i) language c
determ  searchslist_c(slist, string, integer BeginPos, integer FoundPos) - (i, i, i, o) language c  % as "searchslist_c"
procedure  trim_c(string)                  - (i) language c   % as "_trim_c_0"
determ  trim_komma_c(string)            - (i) language c 
procedure sorttijd_c(tijdl)               - (i) language c %as "_mysort_int"
procedure  sortint_c(integerlist)          - (i) language c   %as "_mysort_int"
%procedure  sortint_c(sexlist)          - (i) language c   %as "_mysort_int"
determ  sortuint_c(ulist)                - (i) language c
%myshiftleft(integer,integer, integer) - (i,i,o) language c
%nondeterm sortwed_c(integer Scope,catl,tijd Begintijd ,tijd Eindtijd, integer Mode,integer Wedsnum,
%	wedscat,integer Urgentie,integer Aantal) - (i,i,i,i,i,o,o,o,o) language stdcall % was c
   procedure helpinvoke (window, string FileName, unsigned Command, integer Data) - (i,i,i,i) language stdcall as "_HtmlHelpA"

/*  nieuw prolog
    Scope 0 = alleen nog niet gepland
          1 = alle planbare
    Mode  b_False = alle 
          b_True  = de top 1/8
*/
nondeterm sortwed_c3(integer Scope,catl, integer Mode,integer Wedsnum,
	wedscat,integer Urgentie,integer Aantal) - (i,i,i,o,o,o,o)
determ  sortweds_c(integerlist) - (i) language c  %as "_sortweds_c_0"    /* alleen in tasprnt.pro gebruikt */
nondeterm  scan_planning_c(integer, tijd, integerlist, integer No, wedscat, tijd, integer Banenover) 
				- (i, i, i, i, i , o, o) language c
	% as "_scan_planning_c_0"
	% aantal_opl, begintijd, spelers, wedno, cat, separatie, dagmax, dagmaxo, 
	%					tijd, over
determ planparam_c(integer EersteDag, integer Hoeveel, integer PerWat, string PerWatStr, integer PerDagen,
           integer MinSeparatie, integer MaxSeparatie, integer Duur, integer MaxEnkels, integer DagMax, integer DubbelvoorEnkel) - (o, o, o, o, o, o, o, o, o, o, o) language stdcall

nondeterm  check_forceer_c(integer, tijdl, integer, wedscat, 
			string, integer, string Outstring, integer Tijdstip) - 
               (i, i, i, i, o, o, o, o) language stdcall %as "_check_forceer_c_0" was c

determ voortgang_c(string) - (o) language c
%determ financieel1(string) - (o) language c

determ splitsc(integer, rlist, unsigned Verh, unsigned Club, unsigned Plts, unsigned NotLoot34, unsigned Rating) - (i, i, i, i, i, i, i) language c 
		% vrije catnummers,  records, opties
determ
  myloting(rlist,             % records - i
			      %
	   integer,	      % gereserveerden   - i
	   integerlist,       % indeling - o
	   integer,           % aantal wedstrijden   - o
	   rsoort)            % afval of poel
	 - (i,i,o,o,i) language c %as "_myloting"

procedure LONG sendMessage(WINDOW, WORD, LONG, LONG) - (i,i,i,i) language stdcall as "_SendMessageA"
%procedure LONG    SendMessage(WINDOW, WORD, WORD, LONG) - (i,i,i,i) language stdcall
procedure  append(ilist,ilist,ilist) - (i,i,o)  
procedure  append(tgl,tgl,tgl) - (i,i,o)  
procedure  append(slist,slist,slist) - (i,i,o)  
procedure  append(verhl,verhl,verhl) - (i,i,o)  
procedure  append(integerlist,integerlist,integerlist) - (i,i,o)
procedure  append(wl,wl,wl) - (i,i,o)
procedure  append(wtl, wtl, wtl) -(i,i,o)
procedure  append(schl, schl, schl) - (i, i, o)
procedure  append(catl, catl, catl) - (i,i,o)
procedure  append(wpl, wpl, wpl) - (i,i,o)
procedure  append(menu_item_list, menu_item_list, menu_item_list) - (i, i, o)
procedure  grootste(integer,integer,integer) - (i,i,o)
procedure  kleinste(integer,integer,integer) - (i,i,o)
nondeterm  verhl_strl(verhl,string) - (i, o)
nondeterm sp_opgaven(integer, string) - (i, o)
determ met_maat(integer, string) - (i, o)
determ write_maat(string,string,integer,integer,string) - (i, i, i, i, o)
%nondeterm  member(wedstyp,wtpl) - (o,i)
nondeterm  member(sex,sexlist) - (o,i) (i,i)
nondeterm  member(persno, perslst) - (i, i) (o, i)
nondeterm  member(tijd,tijdl) - (o,i) (i,i)
nondeterm  member(integer, integerlist) - (o, i) (i,i)
nondeterm  member(string, SLIST) - (o, i) (i, i)
nondeterm  member(wedscat, catl) - (o, i) (i, i)
nondeterm  member(wsflag, wsflags) - (i, i)
nondeterm  member(sch, schl) - (o,i)
nondeterm  member(WINDOW, WINLIST) - (o, i)
nondeterm  member(integer, ilist) - (o, i) (i, i)
nondeterm  member(wps, wpl) - (o, i)
nondeterm  member(ulong, ulist) - (o, i)
nondeterm  member(wedid, wl) - (o, i)
nondeterm  member(tgnst, tgl) - (o, i) (i, i)
nondeterm  member(dbasedom, dbasedomlist) - (o, i)
%nondeterm  member(periode, verhl) - (o, i) = periode_member

nondeterm numb(integer) - (o)
procedure count(integerlist, integer, integer) - (i, i, o)
procedure count(ilist, integer, integer) - (i, i, o)
procedure count(MAPINAMEADDR_LIST, integer, integer) - (i, i, o)
procedure count(Slist, integer, integer) - (i, i, o)
procedure count(catl, integer, integer) - (i, i, o)
procedure count(tgl, integer, integer) - (i, i, o)
procedure count(kolomtypeL, integer, integer) - (i, i, o)
procedure count(wl, integer, integer) - (i, i, o)
procedure telop(ilist, integer, integer) - (i, i, o)
%determ hasp(integer, integer, integer, integer, integer, integer, integer, integer, integer) - (i,i,i,i,i,o,o,o,o) language c
procedure catSelect(wedscat) - (i)
procedure catUnselect()

include "tasvp1.pre"

include "tasvpers.pre"

include "tasvglob.pre"

include "tasverh.pre"

include "tasvopg.pre"

include "tasspsel.pre"

include "tasvdisp.pre"

include "tasvpoel.pre"

include "tasvspr.pre"

include "tasuitsl.pre"

include "tasprnt.pre"

include "tasvload.pre"
include "tasetik.pre"

include "tastabel.pre"

include "tasplan.pre"

include "tascomm.pre"

include "tashtml.pre"

include "tasvparse.pre"
