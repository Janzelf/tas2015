/*****************************************************************************

		Copyright (c) 1989 - 1998 De Lint Associates

 Project:  TASVPI
 FileName: TASVSPR.PRO
 Purpose: No description
 Written by: JdL
 Comments:
******************************************************************************/

include "tasvp1.inc"
include "tasvp1.con"
include "hlptopic.con"

%############################################################ printen
include "datecust\\date_cc.pre"

constants

%offsetx1 = 8
offsety1 = 6

database - sprint

schaal(WINDOW, integer Hoogte, integer Kolombreedte) 
%opg_data(integer OpgaveNo,string Sterkte, integer Plaats, string Naam)
single maxKolLength(integer)
single blzOpen(BOOLEAN)
single blzNr(integer)
% ###################### puntentelling
single  waard(integer Kol0Breedte, integer KolBreedte, integer RowHoogte, integer AantalRows)
single maxSterkte(string, integer)

predicates

sterkte_afm(WINDOW, wedscat Cat, BOOLEAN Waardering, integer AfmY)
scan_sterkte(WINDOW, pnt Offset, wedscat Cat, BOOLEAN Waardering)
print_sterkte(WINDOW, pnt Offset, wedscat Cat)
%st_list(slist)
%in_weds(wedscat, tgnst)
%get_stx(tgnst In, string SpeelSterkte, string Naam)
countSterkte(string, integer CurrAantal, string Sterkte, string Sterkte, integer Aantal)
sterker(string, string, string)
nondeterm opgesteld(wedscat, integer)
%nondeterm waardering(integer, integer, integer, integer)
%nondeterm resultaat(string, integer)

clauses

maxSterkte("", 0).
waard(0, 0, 0, 0).

opgesteld(Cat, OpgNo) :-
  opg(OpgNo,Cat,Tgnst,_Plts,_,_,_,_,_,_,_),
  in_weds(Cat,Tgnst).

waardering(  2,  4,  6, 1).
waardering(  5,  8, 10, 1).
waardering(  5,  8,  6, 2).
waardering(  9, 16, 15, 1).
waardering(  9, 16, 10, 2).
waardering(  9, 16,  6, 3).
waardering( 17, 32, 21, 1).
waardering( 17, 32, 15, 2).
waardering( 17, 32, 10, 3).
waardering( 17, 32,  6, 4).
waardering( 33, 64, 28, 1).
waardering( 33, 64, 21, 2).
waardering( 33, 64, 15, 3).
waardering( 33, 64, 10, 4).
waardering( 33, 64,  6, 5).
waardering( 69, 128, 36, 1).
waardering( 69, 128, 28, 2).
waardering( 69, 128, 21, 3).
waardering( 69, 128, 15, 4).
waardering( 69, 128, 10, 5).
waardering( 69, 128,  6, 6).
waardering(129, 999, 45, 1).
waardering(129, 999, 36, 2).
waardering(129, 999, 28, 3).
waardering(129, 999, 21, 4).
waardering(129, 999, 15, 5).
waardering(129, 999, 10, 6).
waardering(129, 999,  6, 7).

resultaat("1e",    1, 0).
resultaat("2e",    2, 1).
resultaat("3/4",   3, 2).
resultaat("5/8",   4, 4).
resultaat("9/16",  5, 8).
resultaat("17/32", 6, 16).
resultaat("33/64", 7, 32).

sterkte_afm(_Win, Cat, b_True, _) :-
  beep(),
  getbacktrack(Here),
  opg(_OpgaveNum,Cat,Tgnst,_Plts,_,_,_,_,_,_,_),
  in_weds(Cat, Tgnst),	
  not(get_st(Tgnst, _, _, _, _, b_True)),
  cutbacktrack(Here),
  wc(Cat, _, _, Onderdeel, _, _, _, _, _, _, _, _, _, _, _),
  %wc(Cat, Onderdeel,_,_,_,_,_),
  format(Msg, "In onderdeel %s zijn niet alle speelsterktes bekend.\nDat geeft incorrecte puntentelling.",Onderdeel),
  dlg_MessageBox( "Te halen punten", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.
sterkte_afm(Win, Cat, b_True, RowsHoogte) :-
  pos(Cat, Top, _, _), 
  Top <= 8,					% een klein schema dus extra nodig..
  opg(_OpgaveNum,Cat,Tgnst,_Plts,_,_,_,_,_,_,_),
  in_weds(Cat, Tgnst),	
  get_st(Tgnst, StOpg, _Naam, _, _, b_True),
  fronttoken(StOpg, _, _),			% er is minstens een opgave met sterkte
  %write("\nEr is een opgave met sterkte"),
  Font_norm = win_GetFont(Win),
  _Fontnaam = font_GetAttrs(Font_norm, _StyleFlags, FontSize),
  Font_footerSize = FontSize * 2 div 3,
  Font_footer = font_SetAttrs(Font_norm,[],Font_footerSize),
  win_SetFont(Win, Font_footer),
  win_GetTextExtent(Win, " Niet-categorie ", -1, _Kol0Breedte, RowH),
  win_SetFont(Win, Font_norm),
  RowsHoogte = 3 * RowH + 12, !.
sterkte_afm(_, _, _, 0).

scan_sterkte(_, _, _, b_False) :- !.	% niet nodig
scan_sterkte(_,_,Cat,_) :-
  assert(maxSterkte("", 0)),		% reset
  opg(_OpgaveNum,Cat,Tgnst,_Plts,_,_,_,_,_,_,_),
  %write("\nOpg. ", Tgnst),
  in_weds(Cat, Tgnst),	
  %write(" in weds."),
  get_st(Tgnst, StOpg, _Naam, _, _, b_True),
  %write(" sterkte: ", StOpg),
  fronttoken(StOpg, _, _),
  %write(" XX "),
  maxSterkte(CurrMax, CurrAantal),
  %write("\nSt: ", CurrMax, " #: ", CurrAantal),
  countSterkte(CurrMax, CurrAantal, StOpg, NewMax, NewAantal),
  assert(maxSterkte(NewMax, NewAantal)),
  fail.
scan_sterkte(Win,pnt(X,Y),Cat,_) :-			% stop als niet zinvol
  maxSterkte(_ST, MetAant),
  MetAant <> 0, 
  findall(OpgNo, opgesteld(Cat, OpgNo), OpgList),
  count(OpgList, 0, Aantal),
  win_GetTextExtent(Win, " Niet-categorie ", -1, Kol0Breedte, RowH),
  RowHoogte = RowH + 4,
  win_GetTextExtent(Win, " 33/64 plaats", -1, KolBreedte, _),
  %draw_Rect(Win,RCT),
  %Aantal1 = Aantal - 1,
  dim(Aantal,Rows),	% 25.8.2000
  %write("\nRows: ", Rows),
  assert(waard(Kol0Breedte, KolBreedte, RowHoogte, Rows)),
  X1 = X - Kol0Breedte - Rows * KolBreedte,
  Y1 = Y - 3 * RowHoogte,
  %write("\nPrint sterkte... pnt(",X1,",",Y1,")"),
  print_sterkte(Win, pnt(X1,Y1),Cat), !.
scan_sterkte(_,_,_,_).

countSterkte(CurrMax, CurrAantal, CurrMax, CurrMax, NewAantal) :- % lopend, lopend, huidig, wordt, wordt
  NewAantal = CurrAantal + 1, !.
countSterkte("", _, StOpg, StOpg, 1) :-
  StOpg <> "", !.
countSterkte(CurrMax, _, StOpg, StOpg, 1) :-
  sterker(CurrMax, StOpg, NewMax),
  NewMax <> CurrMax,
  %write("\nCurrMax",CurrMax," StOpg",StOpg),
  StOpg <> "", !.
  %StOpg < CurrMax, !.

sterker(StOpg, "", StOpg) :- !.
sterker("", CurrMax, CurrMax) :- !.
sterker(StOpg, CurrMax, StOpg) :-
  StOpg < CurrMax, !.
sterker(_, CurrentMax, CurrentMax).

print_sterkte(Win,pnt(X,Y),_Cat) :-		% write kolomheaders
  waard(Kol0Breedte,KolBreedte,RowHoogte,Rows),
  X1 = X + Kol0Breedte + Rows * KolBreedte, 
  Y1 = Y + 3 * RowHoogte,
  draw_Rect(Win,rct(X,Y,X1,Y1)),		% vierkant
  X10 = X + Kol0Breedte,
  draw_Line(Win,pnt(X10,Y),pnt(X10,Y1)),	% rechts van kol0
  Y10 = Y + RowHoogte,
  draw_Line(Win,pnt(X,Y10),pnt(X1,Y10)),	% onder row0
  Y11 = Y10 + RowHoogte,
  draw_Line(Win,pnt(X,Y11),pnt(X1,Y11)),	% onderrow1
  fail.
print_sterkte(Win,pnt(X,Y),_) :-		% row0
  waard(Kol0B,KolB,RowH,Rows),
  Y2 = Y + 3 * RowH,
  resultaat(Header, I, _),
  I <= Rows,
  X1 = X + Kol0B + (I - 1) * KolB,
  X2 = X1 + KolB,
  draw_TextInRect(Win,rct(X1,Y,X2,Y2), Header, -1, [dtext_center]),
  draw_Line(Win,pnt(X1,Y),pnt(X1,Y2)),		% links van kolheader
  fail.
print_sterkte(Win,pnt(X,Y),_) :-			% row met dezelfde categorie spelers
  waard(Kol0B,KolB,RowH,_Rows),
  maxSterkte(ST, MetAant),
  %write("\nCat Sterkte:",ST," met aantal:",MetAant),
  format(Str, " %s-spelers ", ST),
  X1 = X + Kol0B,
  %X2 = X1 + Kol0B,
  Y1 = Y + RowH, 
  Y2 = Y1 + RowH,
  draw_TextInRect(Win,rct(X,Y1,X1,Y2), Str, -1, [dtext_left]),
  waardering(AantalVan, AantalTot, Punten, I),
  MetAant >= AantalVan,
  MetAant <= AantalTot,  
  X3 = X1 + (I - 1) * KolB,
  X4 = X3 + KolB,
  format(PStr,"%u",Punten),
  draw_TextInRect(Win,rct(X3,Y1,X4,Y2), PStr, -1, [dtext_center]),
  %draw_Line(Win,pnt(X3,Y1),pnt(X3,Y2)),
  fail.  
print_sterkte(Win,pnt(X,Y),Cat) :-		% row met de niet-categorie-spelers
  maxSterkte(_ST, Categorie),
  findall(OpgNo, opgesteld(Cat, OpgNo), OpgList),
  count(OpgList, 0, MetAant),		% is incl totaal
  Bonus = 1 + Categorie / (MetAant), 
  %write("\nBonus = ", Bonus),
  waard(Kol0B,KolB,RowH,_Rows),
  Str = " Niet-categorie",
  %Metaant1 = MetAant - Categorie,
  %write("\nNCat ",Str, " aantal:",MetAant),
  X1 = X + Kol0B,
  %X2 = X1 + Kol0B,
  Y1 = Y + RowH + RowH, 
  Y2 = Y1 + RowH,
  draw_TextInRect(Win,rct(X,Y1,X1,Y2), Str, -1, [dtext_left]),
  waardering(AantalVan, AantalTot, Punten, I),
  MetAant >= AantalVan,
  MetAant <= AantalTot,  
  X3 = X1 + (I - 1) * KolB,
  X4 = X3 + KolB,
  P1 = Punten * Bonus,
  format(PStr,"%.2g",P1),
  %write("\n",PStr),
  draw_TextInRect(Win,rct(X3,Y1,X4,Y2), PStr, -1, [dtext_center]),
  %draw_Line(Win,pnt(X3,Y1),pnt(X3,Y2)),
  fail.
print_sterkte(_,_,_).

/*
max_st(St1, _, "?", StList) :-
  not(member(St1, StList)), !.
max_st(_, St2, "?", StList) :-
  not(member(St2, StList)), !.
max_st(St1, St2, St1, _) :-
  St1 < St2, !.
max_st(_, St2, St2, _). */

% ######################## display


predicates

papierindeling(Wedscat CatNo, WINDOW PrintWin, RCT Papier, integer AfmHor, integer AfmVert, integer OffsetX, integer OffsetY, integer RowVan, integer PapRows, integer Kolvan, integer KolDelta, integer Kols, BOOLEAN Vouwen,integer PaginaSel, integer VanafDag, integer TmDag, BOOLEAN Waardering,BOOLEAN Kleur,BOOLEAN Kader,FONT, FONT, FONT, FONT, integer FSizeO)
toon_wedstrijden(wedscat, WINDOW, RCT, PNT Offset, integer RowVan, integer PapRows, integer KolVan, integer KolDelta, BOOLEAN Vouwen,integer PaginaSel, integer VanafDag, integer TmDag, BOOLEAN Kleur,FONT, FONT)
%row(integer Top, integer WNo, integer Row) 
plpr(WINDOW, integer OE,integer WedsNo,RCT Drawrect,integer Top, integer TopDim, RCT ClipRct, PNT Offset, integer TopRow, integer Kolom, integer VanKol, integer KolDelta, BOOLEAN Vouwen)
   - (i, i, i, o, i, i, i, i, i, i, i, i, i)  
toon_wedstpr(integer Wedstrijdnummer,wedscat CatNr, WINDOW TopDim, integer Top, integer TopDim, RCT drawrect, RCT drwarect1, BOOLEAN Vouwen ,integer PaginaSel, integer VanafDag,integer TmDag,BOOLEAN Kleur,FONT, FONT)
%streepje(integer Kol0, WINDOW, integer WNum, integer X0, integer Y1, integer Y3)
%max_afm(integer, integer Max, integer)
max_kol_afm(wedscat, WINDOW, integer, RCT Papier, string MinKolsHor, string MinKolsVert) - (i,i,o,i,i,i)
regelfit(integer, integer, integer, integer NewUnit, BOOLEAN Opvullen)
%naamVanToernooi(string)
%num2Str(integer, string)
offset_rij(WINDOW, integer, integer)
%add_baan(janee, string, string)
%uitsl_win(integer, wedscat, tgnst, tgnst)
y_coord(rsoort,integer, integer, integer, integer, integer, string, string)
geplaatst(Wedscat, integer TopDim, integer NoDim, tgnst , string Tekst)
wint(integer, wedscat, tgnst)
rekKols(integer KolDelta, integer Rest, integer Max, integer Max1, integer Kols, integer SchemaKols)
afmeting(integer Top, integer KolPap, integer RegelHoogte, integer KolomBreedte, integer Hor, integer Vert, integer Rows, integer Kols, BOOLEAN gevouwen) 
           - (i, i, i, i, o, o, o, o, o)
vouwenDoen(BOOLEAN, integer Offset, integer Flag)
nondeterm wedstr_inhoud(integer WNo, wedscat Cat, integer TopDim, integer TmDag, string Inhoud, string Score, tijd Veranderd)
kol(integer TopDim, integer WNo, BOOLEAN Vouwen, integer Kol)
check_inhoud(integer PaginSel, wedscat CatNo, integer RowVan,integer RowTot,integer KolVan,integer PapKols, BOOLEAN Vouwen, integer VanafDag, integer TmDag)
kleur(WINDOW, integer PaginaSel, BOOLEAN KleurOfZw, integer VanafDag, integer LogTijd, COLOR) 
row1(integer TopDim, integer No, integer Row1, integer Row2) - (i,i,o,o)
open_blz(WINDOW PrintWin, BOOLEAN)
inspring(integer Kol, WINDOW Win, integer X0, integer X01)
positieVanPunten(WINDOW, integer KaderX, integer PapierX, integer AfmX, integer Uit, integer PapierY, integer AfmY, integer OffsetY, integer UitY)
regelhoogteExtra(integer Top, integer Hoogte, integer RegelHoogte)
kaderlijn(WINDOW, wedscat, BOOLEAN Kader, integer OffsetX, integer OffsetY, integer RowVan, integer RowTot, integer Rows, integer KolVan, integer KolTot, integer Kols, integer AfmVert)
draw_header(WINDOW, wedscat, FONT Font_Header, integer OffsetX, integer OffsetY, integer KolVan, integer PapKols, integer Kols, BOOLEAN KleurOfZw, integer Rowvan, integer Paprows, integer KopY, integer KopX)
kolsRechts(integer KolVan, integer PapKols, integer Kols, integer KolRechts)
header_opschuif(wedscat, WINDOW, integer PapKols, integer Width, integer In, integer Uit)

clauses

maxKolLength(0).

geplaatst(Cat, No, No, Tgnst, Tekst) :-
  opg(_,Cat,Tgnst,Plts,_,_,_,_,_,_,_),
  Plts > 0,
  format(Tekst, "(%u",Plts), !.
geplaatst(_Cat, _No, _No1, _Tgnst, "").

offset_rij(_, 0, 0) :- !.
offset_rij(_Win, 1, 1) :- !.
offset_rij(_Win, 2, 3) :- !.
offset_rij(_Win, Kol, Offs) :-
  Kol1 = Kol - 1 ,
  bitleft(1,Kol1,Offs1),
  Offs = (Offs1 - 1) * 2 + 1.

/*offset_rij(_, 0, 0) :- !.
offset_rij(Win, 1, Offs) :- !,
  schaal(Win, Regelhoogte, _Kolombreedte),!,
  Offs = Regelhoogte div 2.
offset_rij(Win, 2, Offs) :- !,
  schaal(Win,Regelhoogte, _Kolombreedte),!,
  Offs = Regelhoogte div 2 + Regelhoogte.
offset_rij(Win, Kol, Offs) :-
  schaal(Win, Regelhoogte, _Kolombreedte),!,
  Kol1 = Kol - 1 ,
  bitleft(1,Kol1,Offs1),
  Offs = (Offs1 - 1) * Regelhoogte + Regelhoogte div 2.*/

y_coord(afval, WdNo, Card, Dim, Dim, Offs, Naam, NaamUit) :- !,	% voegt een nummertje toe
  Coord = 2 * (WdNo - Card) + Offs +1,				% in de eerste kolom (Dim = Dim)
  format(NaamUit, " %d.%s", Coord, Naam).
y_coord(_, _, _, _, _, _, Naam, Naam1):-
  concat(" ", Naam, Naam1). 

/*add_baan(nee, _, "") :- !.
add_baan(_, "", "") :- !.
add_baan(_, Baan, Uit) :-
  format(Uit, "[%s]", Baan).*/

uitsl_win1(0, Cat, S1, S1) :-
  w3(1, Cat, S1, _, _, _, _), !. 
uitsl_win1(0, _, onbekend, onbekend) :- !.
uitsl_win1(No, Cat, S1, S2) :-
  wd(No,Cat,S1,S2,_,_,_), !.
uitsl_win1(No, Cat, S1, S2) :-
  bitleft(No, 1, No1),
  wd(No1,Cat,_,_,_,_,_),
  wint(No1, Cat, S1),
  No2 = No1 + 1,
  wint(No2, Cat, S2), !.

wint(No, Cat, gelijk_spel) :-
  w3(No, Cat, gelijk_spel, _, _, _, _), !.
wint(No, Cat, Winnaar) :-
  w3(No, Cat, Winnaar, _, _, _, _), !.
wint(_, _, onbekend).

kleur(Win,3,b_True,VanafDag,LogTijd,color_Red) :-		% highlight changes kleur
  bitright(LogTijd,7,StampDag),
  StampDag >= VanafDag, !,
  win_SetBackColor(Win, color_Yellow),
  win_SetBackMode(Win, bk_Opaque).
  %write("\nKleur").
kleur(Win,3,b_False,VanafDag,LogTijd,color_Black) :-		% highlight changes zw
  bitright(LogTijd,7,StampDag),
  StampDag >= VanafDag, !,
  Font=win_GetFont(Win),
  NewFont = font_SetAttrs(Font,[fs_Italic],0),
  win_setFont(Win, NewFont).
kleur(_,_PaginaSel,b_True,_VanafDag,_LogTijd,color_Red) :- !.
kleur(_,_PaginaSel,_KleurOfZw,_VanafDag,_LogTijd,color_Black).

%###################### wedstrinhoud
wedstr_inhoud(0, Cat, _TopDim, TmDag,Inhoud, "", TijdLog) :-  % eindronde (= 0)
  w2(1,Cat,Tijd,_,_,Fix,TijdLog,Baan),  		% + planning bekend
  TmDag1 = TmDag + 1,
  bitleft(TmDag1, 7, TmTijd),
  TmTijd > Tijd, 
  baan_adm(IBaan),
  add_baan(IBaan, Baan, BaanT),
  tijd_kwart(Dag,_,_,Tijd),
  dag(Dag,DStr,_), !,
  tijd_str(Tijd,Str1,_),
  format(Inhoud, "%c %s%s %s ", Fix, Dstr, Str1, BaanT).
wedstr_inhoud(0, Cat, _TopDim, TmDag,Inhoud, Score, Tijd) :-	% eindronde 
  w3(1,Cat,Tgnst,_, _, _, Tijd), !,					% winnaar bekend
  baan_adm(IBaan),
  repa(ja,' ',IBaan,nee,nee,1,0,1,0,Cat,Tgnst,TmDag,Inhoud,Score,_Tijd2,b_False,b_False,_Clubs,_Anker,_,_,_,_,_).
wedstr_inhoud(No, Cat, TopDim, TmDag,Inhoud, Score, Tijd):-		% gewone wedstrijd
  No <> 0,
  uitsl_win1(No, Cat, S1, _S2),     	 
  dim(No,NoDim),                  			% bepaal de ronde
  baan_adm(IBaan),
  repa(ja,' ',IBaan,nee,nee,0,TopDim,NoDim,No,Cat,S1,TmDag,Inhoud,Score,Tijd,b_False,b_False,_Clubs, _A,_,_,_,_,_).	% bovenste helft 
wedstr_inhoud(No, Cat, TopDim, TmDag,Inhoud, Score, Tijd) :-	% gewone wedstrijd
  No <> 0,
  dim(No,NoDim),                  			% bepaal de ronde
  uitsl_win1(No, Cat, _S1, S2),     
  baan_adm(IBaan),
  repa(ja,' ',IBaan,nee,nee,1,TopDim,NoDim,No,Cat,S2,TmDag,Inhoud,Score,Tijd,b_False,b_False,_Clubs,_Anker,_,_,_,_,_).	% onderste helft 
% ######################## einde wedstrinhoud

toon_wedstpr(0, Cat, Win, _Top, _, DrawRct0,_,Vouwen,_PaginaSel,_VanafDag,TmDag,_Kleur,Font_norm,_Font_score) :-        	% eindronde
  w2(1,Cat,Tijd,_,_,Fix,_,Baan),  	% + planning bekend
  TmDag1 = TmDag + 1,
  bitleft(TmDag1, 7, TmTijd),
  TmTijd > Tijd, 
  baan_adm(IBaan),
  add_baan(IBaan, Baan, BaanT),
  tijd_kwart(Dag,_,_,Tijd),
  dag(Dag,DStr,_), !,
  tijd_str(Tijd,Str1,_),
  format(Str, "%c %s%s %s ", Fix, Dstr, Str1, BaanT), 
  win_SetFont(Win, Font_norm),
  vouwenDoen(Vouwen, _, Flag),
  draw_TextInRect(Win, DrawRct0, Str, -1, [dtext_bottom, dtext_singleline, Flag]).
  %draw_Text(Win, Kolom, Rij, Str).
toon_wedstpr(0, Cat, Win, _Top, _, DrawRct0, _DrawRct1,Vouwen,_PaginaSel,_VanafDag,TmDag,_Kleur,Font_norm,Font_score) :-	% eindronde 
  w3(1,Cat,Tgnst,_,_,_,_), 			% winnaar bekend
  baan_adm(IBaan),
  repa(ja,' ',IBaan,nee,nee,1,0,1,0,Cat,Tgnst,TmDag,Naam,Score,_Tijd,b_False,b_False,_Clubs,_Anker,_,_,_,_,_Noti),
  concat(Naam, "  ", Naam1),
  draw_TextInRect(Win, DrawRct0, Naam1, -1, [dtext_bottom, dtext_singleline, dtext_right]),
  Score <> "", 
  schaal(Win,Regelhoogte, _Kolombreedte), !,
  DrawRct0 = rct(X0, _Y0, X1, Y1),
  X11 = X1 - 10,
  %Y11 = Y1 - Regelhoogte,
  Y11 = Y1,
  Y2 = Y11 + Regelhoogte,
  DrawRct = rct(X0, Y11, X11, Y2),
  %draw_Rect(Win,DrawRct),
  %win_SetForeColor(Win, color_Black),
  win_SetFont(Win, Font_score),
  vouwenDoen(Vouwen, _, _Flag),
  draw_TextInRect(Win, DrawRct, Score, -1, [dtext_singleline, dtext_top, dtext_right]),
  % draw_TextInRect(Win, DrawRct, Score, -1, [dtext_singleline, dtext_bottom, Flag]),
  win_SetFont(Win, Font_norm).
  %draw_Text(Win, Kolom, Rij, Naam).
toon_wedstpr(No, Cat, Win, Top, TopDim, DrawRct0, _DrawRct1,_,PaginaSel,VanafDag,TmDag,KleurOfZwart,Font_norm,Font_score):-% gewone wedstrijd
  No <> 0,
  uitsl_win1(No, Cat, S1, _S2),     	% 
  dim(No,NoDim),                  	% bepaal de ronde
  baan_adm(IBaan),
  repa(ja,' ',IBaan,nee,nee,0,TopDim,NoDim,No,Cat,S1,TmDag,Naam,Score,LogTijd,b_False,b_False,_Clubs,_Anker,_,_,_,_,_Noti),% bovenste helft 
  y_coord(afval,No, Top, TopDim, NoDim, 0, Naam, NaamPlus), % tellertje voor eerste kolom
  win_SetFont(Win, Font_norm),
  kleur(Win,PaginaSel,KleurOfZwart,VanafDag,LogTijd,Color),
  draw_TextInRect(Win, DrawRct0, NaamPlus, -1, [dtext_singleline, dtext_bottom]),
  win_SetBackColor(Win, color_White),
  win_SetBackMode(Win, bk_Opaque),
  %win_SetForeColor(Win, color_Black),
  win_SetFont(Win, Font_norm),
  geplaatst(Cat,TopDim, NoDim, S1, Geplaatst),
  win_SetForeColor(Win, Color),
  DrawRct0 = rct(X0, Y0, X1, Y1),
  X0a = X0 + 2,
  draw_TextInRect(Win, rct(X0a,Y0,X1,Y1), Geplaatst, -1, [dtext_singleline, dtext_bottom, dtext_right]),
  %win_SetBackColor(Win, color_White),
  win_SetBackMode(Win, bk_Transparent),
  win_SetForeColor(Win, color_Black),
  Score <> "",
  schaal(Win,Regelhoogte, _Kolombreedte),
  %Y11 = Y1 - 3,  % +++++++++++
  Y2 = Y1 + Regelhoogte,
  DrawRct = rct(X0, Y1, X1, Y2),
  %draw_Rect(Win,DrawRct),
  %win_SetForeColor(Win, color_Black),
  win_SetFont(Win, Font_score),
  draw_TextInRect(Win, DrawRct, Score, -1, [dtext_singleline, dtext_top,dtext_center]),
  win_SetFont(Win, Font_norm),
  fail.
toon_wedstpr(No, Cat, Win, Top, TopDim, _DrawRct0, DrawRct1,_,PaginaSel,VanafDag,TmDag,KleurofZwart,Font_norm,Font_score) :-% gewone wedstrijd
  No <> 0,
  dim(No,NoDim),                  	
  uitsl_win1(No, Cat, _S1, S2),     	% 
  baan_adm(IBaan),
  repa(ja,' ',IBaan,nee,nee,1,TopDim,NoDim,No,Cat,S2,TmDag,Naam1,Score,LogTijd,b_False,b_False,_Clubs,_,_,_,_,_,_),% onderste helft 
  y_coord(afval, No, Top, TopDim, NoDim, 1, Naam1, Naam1Plus), /* regelnummers!  */
  kleur(Win,PaginaSel,KleurOfZwart,VanafDag,LogTijd,Color),
  draw_TextInRect(Win, DrawRct1, Naam1Plus, -1, [dtext_singleline, dtext_bottom]),
  win_SetFont(Win, Font_norm),
  win_SetBackMode(Win, bk_Opaque),
  win_SetBackColor(Win, color_White),
  win_SetForeColor(Win, Color),
  geplaatst(Cat,TopDim, NoDim, S2, Geplaatst),
  DrawRct1 = rct(X0, Y0, X1, Y1),
  X0a = X0 + 2,
  draw_TextInRect(Win, rct(X0a,Y0,X1,Y1), Geplaatst, -1, [dtext_singleline, dtext_bottom, dtext_right]),
  win_SetBackMode(Win, bk_Transparent),
  win_SetForeColor(Win, color_Black),
  Score <> "",
  schaal(Win,Regelhoogte, _Kolombreedte),
  Y2 = Y1 + Regelhoogte,
  DrawRct = rct(X0, Y1, X1, Y2),
  %win_SetForeColor(Win, color_Black),
  %draw_Rect(Win,DrawRct),
  win_SetFont(Win, Font_score),
  draw_TextInRect(Win, DrawRct, Score, -1, [dtext_singleline, dtext_top,dtext_center]),
  win_SetFont(Win, Font_norm), !.
%toon_wedstpr(_No, _Cat, _Win, _Top, _TopDim, _DrawRct0, _DrawRct1,_,_PaginaSel,_VanafDag,_KleurOfZw,_Font_norm,_Font_score).

vouwenDoen(b_True, 0, dtext_right).
vouwenDoen(b_False, 1, dtext_left).

plpr(Win,OE,0,DrawRct,Top,TopDim,  _ClipRct,pnt(OffX,OffY),TopRow,Kol,VanKol,_KolDelta,_Vouwen) :-
  !, OE = 0,					% finale
  schaal(Win,Regelhoogte, Kolombreedte),	% OE     - opg 0 of 1 (oneven/even)
  K = (Kol - VanKol) * Kolombreedte + OffX,		
  Kol1 = TopDim + 1,
  offset_rij(Win,Kol1, Offs),
  R = (Offs * Regelhoogte) div 2 + OffY - (TopRow - Top) * 2 * Regelhoogte, 
  KolRight  = K + Kolombreedte,
  RowBottom = R + Regelhoogte,  
  DrawRct = rct(K,R,KolRight,RowBottom), !.
plpr(Win,OE,No,DrawRct,Top,_TopDim,  _ClipRct,pnt(OffX,OffY),TopRow,Kol,VanKol,_KolDelta,_Vouwen) :-
  schaal(Win,Regelhoogte, Kolombreedte),	% OE     - opg 0 of 1 (oneven/even)
  K = (Kol - VanKol) * Kolombreedte + OffX,	
  bitright(Top,Kol,KolTop),			
  Row = 2 * (No - KolTop) + OE, 		
  bitleft(1,Kol,D2delt),
  offset_rij(Win,Kol, Offs),		% in pixels
  R = (Offs * RegelHoogte) div 2 + Row * D2delt * Regelhoogte + OffY - (TopRow - Top) * 2 * Regelhoogte,
  KolRight  = K + Kolombreedte,
  RowBottom = R + Regelhoogte,
  DrawRct = rct(K,R,KolRight,RowBottom), !.

kol(TopDim, 0, Vouwen, Kol) :-
  vouwenDoen(Vouwen, KolOffs, _),
  Kol = TopDim + KolOffs, !.				% is ook output
kol(TopDim, WdNo, _Vouwen, Kol) :-
  dim(WdNo,Dim),					% No     - wedstrijdnummer           
  Kol = TopDim - Dim, !.				% R      - rij waar tekst moet komen 

% #####################################################
row1(0, 0, 1, 1) :- !.
row1(TopDim, 0, Row, Row) :-
  bitleft(1,TopDim,Top),
  bitright(Top,1,HalveTop),
  Row = Top + HalveTop - 1, !.
row1(TopDim, No1, No1, No1) :-
  bitleft(1,TopDim,Top),
  No1 >= Top, 
  !.
row1(TopDim, No1, Row, Row2) :-
  bitleft(No1, 1, No),		% rij eerder
  dim(No, Dim),
  Kol =  TopDim - Dim,
  bitleft(1,TopDim,Top),
  bitleft(1, Dim, KolTop),
  bitleft(1,Kol,D2delt),
  bitright(D2Delt, 1, HalveDelta),
  Row = (No - KolTop) * D2Delt + HalveDelta + Top,
  Row2 = Row + D2Delt,
  !.
% ######################################################


inspring(0, Win, X0, X01) :-
  win_GetTextExtent(Win,"11.",-1,Width,_Height),
  X01 = X0 + Width, !.
inspring(_Kol, _Win, X0, X0).


toon_wedstrijden(Cat,Win,ClipRct,Offset,RowVan,RowTot,KolVan,KolDelta,Vouwen,PaginaSel,VanafDag,TmDag,Kleur,Font_norm,Font_score) :-		
  win_SetForeColor(Win, color_Black),
  win_SetBackColor(Win, color_White),
  win_SetBackMode(Win, bk_Transparent),
  win_SetBrush(Win, brush(pat_Hollow, color_Black)),
  pos(Cat, Top, _, _),
  dim(Top, TopDim),
  Pen1 = pen(1,ps_Solid, color_Black),
  win_SetPen(Win, Pen1),
  wd(WNum,Cat,_, _,_,_,_),		% voor alle wedstrijden in schema
  %WNum = 1,
  row1(TopDim, WNum, Row1, Row2),
  %write("\nWNum:",WNum," Row1:",Row1," Row2:",Row2," RowVan:",RowVan," RowTot:",RowTot),
  Row2  >= RowVan,
  Row1  < RowTot,
  kol(TopDim, WNum, Vouwen, Kol),
  Kol >= KolVan,
  Kol < KolVan + KolDelta,
  %write("\nDoorgelaten, Kol:",Kol," Vouwen:",Vouwen),
  plpr(Win,0,WNum,DrawRct0,Top,TopDim, ClipRct,Offset,RowVan,Kol,KolVan,KolDelta,Vouwen), % bepaal exacte locatie v.d. ene
  plpr(Win,1,WNum,DrawRct1,Top,TopDim, ClipRct,Offset,RowVan,Kol,KolVan,KolDelta,Vouwen), % bepaal exacte locatie v.d. andere 
  DrawRct0 = rct(X0, _Y0, X1, Y1),
  %X0a = X0 + 2,
  %DrawRct0A = rct(X0a, _Y0, X1, Y1),
  DrawRct1 = rct(_X2, _Y2, _X3, Y3),
  inspring(Kol, Win, X0, X01),
  %write("\nDrawRct0:",DrawRct0," DrawRct1:",DrawRct1),
  %streepje(Kol, Win, WNum, X0, Y0, Y3),
  draw_Polyline(Win, [pnt(X01, Y1), pnt(X1,Y1), pnt(X1,Y3),pnt(X0,Y3)]),
  toon_wedstpr(WNum, Cat, Win, Top, TopDim, DrawRct0, DrawRct1,Vouwen,PaginaSel,VanafDag,TmDag,Kleur,Font_norm,Font_score),
  fail.					% herhaal voor alle wedstrijden
toon_wedstrijden(Cat,Win,ClipRct,Offset,RowVan,RowTot,KolVan,KolDelta,Vouwen,PaginaSel,VanafDag,TmDag,Kleur,Font_norm,Font_score) :-	% finale!
  win_SetForeColor(Win, color_Black),	% afhandeling van WdNo = 0 (finale)
  win_SetBackColor(Win, color_White),
  win_SetBackMode(Win, bk_Transparent),
  pos(Cat, Top, _, _),
  dim(Top, TopDim),
  WNum = 0,
  row1(TopDim, WNum, Row, _),
  Row >= RowVan,
  Row  < RowTot,
  kol(TopDim, WNum, Vouwen, Kol),
  Kol >= KolVan,
  Kol < KolVan + KolDelta,
  plpr(Win,0,WNum,DrawRct0,Top,TopDim, ClipRct,Offset,RowVan,Kol,KolVan,KolDelta,Vouwen), % bepaal exacte locatie v.d. ene
  DrawRct0 = rct(X0, _Y0, X1, Y1),
  draw_Line(Win, pnt(X0,Y1),pnt(X1,Y1)),	
  toon_wedstpr(WNum, Cat, Win, Top, TopDim, DrawRct0, DrawRct0,Vouwen,PaginaSel,VanafDag,TmDag,Kleur,Font_norm,Font_score), !.
toon_wedstrijden(_Cat,_Win,_ClipRct,_Offset,_RowVan,_RowTot,_KolVan,_KolDelta,_Vouwen,_PaginaSel,_VanafDag,_TmDag,_Kleur,_Font_norm,_Font_score).

/*max_afm(AfmHor, PapHor, PapHor) :-
  AfmHor > PapHor, !.
max_afm(AfmHor, _, AfmHor).*/

max_kol_afm(Cat, PrintWin, Width, _, _, _) :- 	% zoek max extent van alle partijen
  win_GetTextExtent(PrintWin,"NaamNaamNaamNaamNaam",-1,WidthIni,_HeightIni),
  assert(maxKolLength(WidthIni)),
  opg(_,Cat,Tgnst,_Plts,_,_,_,_,_,_,_),
  in_weds(Cat, Tgnst),
  baan_adm(IBaan),
  repa(ja,' ',IBaan,nee,nee,1,0,1,0,Cat,Tgnst,0,Naam,_Score,_Tijd,b_False,b_False,_Clubs,_,_,_,_,_,_Noti),
  win_GetTextExtent(PrintWin,Naam,-1,Width,_Height),
  maxKolLength(L),
  Width > L,
  assert(maxKolLength(Width)),
  fail.
max_kol_afm(_,_, Max, rct(PX1, _PY1, PapHor, PapVert), MinKolHor, _) :-
  fronttoken(MinKolHor,_,_),
  PapVert < PapHor,
  str_int(MinKolHor, Int),
  Max1 =  (PapHor - PX1) div Int,
  maxKolLength(Max),
  Max1 < Max, 
  assert(maxKolLength(Max1)),
  fail.
max_kol_afm(_,_, Max, rct(PX1, _PY1, PapHor, PapVert), _, MinKolVert) :-
  fronttoken(MinKolVert,_,_),
  PapVert > PapHor,
  str_int(MinKolVert, Int),
  Max1 =  (PapHor  - PX1) div Int,
  maxKolLength(Max),
  Max1 < Max, 
  assert(maxKolLength(Max1)),
  fail.
max_kol_afm(_Cat, _PrintWin, Max, _, _, _) :-
  maxKolLength(Max), !.

%num2Str(0, "") :- !.
%num2Str(Num, Str) :-
%  str_int(Str, Num).

 
sleuteltekst1(Park1) :-
  sleuteltekst(Park),
  reversestr(Park, "", Park1), !.
sleuteltekst1("").

check_inhoud(2,CatNo, RowVan,RowTot,KolVan,PapKols,Vouwen,_VanafDag,TmDag) :-	% 2 = alleen met inhoud
  pos(CatNo, Top, _, _), 
  dim(Top, TopDim),
  wd(WNum,CatNo,_, _,_,_,_),						% loop
  row1(TopDim, WNum, Row1, Row2),
  Row2  >= RowVan,
  Row1  < RowTot,
  kol(TopDim, WNum, Vouwen, Kol),
  Kol >= KolVan,
  Kol <= KolVan + PapKols - 1,
  wedstr_inhoud(WNum, CatNo, TopDim, TmDag, Inhoud, Score, _LogTijd),
  concat(Inhoud, Score, Text),
  trim_c(Text),
  Text <> "", 
  true,
  !.
check_inhoud(3,CatNo, RowVan,RowTot,KolVan,PapKols,Vouwen,VanafDag,_TmDag) :-	% 3 = veranderd sedert..
  pos(CatNo, Top, _, _), 
  dim(Top, TopDim),
  wd(WNum,CatNo,_, _,_,Spoor,_),					% loop
  row1(TopDim, WNum, Row1, Row2),
  Row2  >= RowVan,
  Row1  < RowTot,
  kol(TopDim, WNum, Vouwen, Kol),
  Kol >= KolVan,
  Kol <= KolVan + PapKols - 1,
  Spoor <> 0,
  bitright(Spoor,7,Dag),
  Dag >= VanafDag, 
  !.
check_inhoud(3,CatNo, RowVan,RowTot,KolVan,PapKols,Vouwen,VanafDag, TmDag) :-	% 3 = veranderd sedert..
  pos(CatNo, Top, _, _), 
  dim(Top, TopDim),
  wd(WNum,CatNo,_, _,_,_,_),						% loop
  row1(TopDim, WNum, Row1, Row2),
  Row1  >= RowVan,
  Row2  < RowTot,
  kol(TopDim, WNum, Vouwen, Kol),
  Kol >= KolVan,
  Kol <= KolVan + PapKols - 1,
  wedstr_inhoud(WNum, CatNo, TopDim, TmDag, Inhoud, Score, LogTijd),
  LogTijd <> 0,
  bitright(LogTijd,7,Dag),
  Dag >= VanafDag,
  concat(Inhoud, Score, Text),
  trim_c(Text),
  Text <> "", 
  !.
check_inhoud(1,_CatNo, _RowVan,_RowTot,_KolVan,_PapKols,_Vouwen,_VanafDag, _TmDag).

draw_footer(PrintWin, rct(X0,_,PapHor, PapVert),FontSizeOrig) :-
  %######## draw footer
  blzOpen(b_True), !,
  Font_norm = win_GetFont(PrintWin),
  Fontnaam = font_GetAttrs(Font_norm, _StyleFlags, FontSize),
  Font_footerSize = FontSize * 2 div 3,
  Font_footer = font_SetAttrs(Font_norm,[],Font_footerSize),
  %win_GetFontMetrics(PrintWin, _Leading, Ascent, Descent),
  %Regelhoogte = Ascent + Descent,
  naam(ToernooiNaam,_,_,_,_,_),
  date(DY, DM, DD),
  time(TH, TM, _, _),
  helestr_int(0,TMS, TM),
  sleuteltekst1(Park),
  blzNr(Nummer),
  format(BottomLine, "%s  %u/%u/%u  %u:%s  blz %u   %s",Park, DD, DM, DY, TH, TMS, Nummer, ToernooiNaam),
  format(FirmaLine, "ToernooiAssistent v% 2000 %u %s", versie, FontSizeOrig, FontNaam),
  BotVert1 = PapVert,
  BotVert2 = PapVert + 100,
  PapHor1 = PapHor + 100,
  %Font_norm = win_GetFont(PrintWin),
  win_SetFont(PrintWin, Font_footer),
  win_SetClip(PrintWin,rct(0, 0, PapHor1, BotVert2)),	% set clipping rectangle weer terug
  draw_TextInRect(PrintWin,rct(X0,BotVert1,PapHor,BotVert2),BottomLine,-1,[dtext_singleline, dtext_right, dtext_top]),
  draw_TextInRect(PrintWin,rct(X0,BotVert1,PapHor,BotVert2),FirmaLine,-1,[dtext_singleline, dtext_left, dtext_top]),
  win_SetFont(PrintWin, Font_norm),
  %write("\nEndpage"),
  print_EndPage(PRintWin),
  assert(blzOpen(b_False)).
draw_footer(_, _, _).

kaderlijn(PrintWin, CatNo, b_True,OffsetX, OffsetY, _RowVan, _RowTot, _Rows, KolVan, _PapKols, _Kols, AfmVert) :-
  Pen = pen(4 , ps_Solid, color_Black),
  win_SetPen(PrintWin,Pen),			% links
  %write("\nRowtot:",_RowTot, " RowVan:",RowVan, " Rows:",_Rows," KolVan:",KolVan," KolTot:",_KolTot," Kols:",_Kols),
  KolVan = 0,
  pos(CatNo, Top, _, _),
  X0 = OffsetX - 4,
  Y0 = OffsetY,
  schaal(PrintWin,_Regelhoogte, _Kolombreedte),	
  Y1 = AfmVert - (_RowVan - Top) * _Regelhoogte * 2 + Y0 + offsety1,
  %Y1 = (_RowTot - RowVan) * Regelhoogte * 2 + Y0 + offsety1,
  draw_Line(PrintWin,pnt(X0,Y0), pnt(X0,Y1)),
  fail.
kaderlijn(PrintWin, CatNo, b_True,OffsetX, OffsetY, _RowVan, RowTot, _Rows, KolVan, PapKols, Kols, AfmVert) :-
  %RowTot = 4 * Rows,
  pos(CatNo, Top, _, _),
  RowTot >= Top * 2,
  X0 = OffsetX - 4,				% onder
  Y0 = OffsetY,
  schaal(PrintWin,_Regelhoogte, Kolombreedte),	
%###########################################################
  kolsRechts(KolVan, PapKols, Kols, KolRechts),
  X1 = KolRechts * Kolombreedte + OffsetX ,
  %X1 = (Kols - KolVan) * Kolombreedte + X0 + X0 + OffsetX,
%############################################################
  Y1 = AfmVert - (_RowVan - Top) * _Regelhoogte * 2 + Y0 + offsety1,
  %write("\nAfmVert:",AfmVert," Y1:",Y1),
  %Y1 = (RowTot - RowVan) * Regelhoogte * 2 + Y0 + offsety1,
  draw_Line(PrintWin,pnt(X0,Y1), pnt(X1,Y1)),
  fail.
kaderlijn(PrintWin, CatNo, b_True,OffsetX, OffsetY, _RowVan, _RowTot, _Rows, KolVan, PapKols, Kols, AfmVert) :-
  PapKols + KolVan >= Kols,
  pos(CatNo, Top, _, _),
  %X0 = OffsetX - 4,				% rechts
  Y0 = OffsetY,
  schaal(PrintWin,_Regelhoogte, Kolombreedte),	
  kolsRechts(KolVan, PapKols, Kols, KolRechts),
  X1 = KolRechts * Kolombreedte + OffsetX ,
  %X1 = (Kols - KolVan) * Kolombreedte + X0 + X0 + OffsetX,
  Y1 = AfmVert - (_RowVan - Top) * _Regelhoogte * 2 + Y0 + offsety1,
  %R = _RowTot - _RowVan,
  %write("\nRowVan:",_RowVan," RowTot:",_RowTot," Rows:",_Rows, " RowDelt:", R, " Top:",Top),
  %write("\nY1 = ", Y1),
  %Y1 = (_RowTot - RowVan) * Regelhoogte * 2 + Y0 + offsety1,
  draw_Line(PrintWin,pnt(X1,Y0), pnt(X1,Y1)),
  fail.
kaderlijn(PrintWin, _Cat, _, _OffsetX, _OffsetY, _RowVan, _RowTot, _Rows, _KolVan, _KolTot, _Kols, _AfmVert) :-
  Pen = pen(1 , ps_Solid, color_Black),
  win_SetPen(PrintWin,Pen), !.

kolsRechts(KolVan, _PapKols, Kols, KolRechts) :-
  KolVan > 0, !,
  KolRechts = Kols - KolVan.
kolsRechts(KolVan, PapKols, Kols, Kols) :-
  KolVan = 0, 
  PapKols > Kols, !.
kolsRechts(_KolVan, PapKols, _Kols, PapKols).

draw_header(PrintWin, CatNo, Font_Header,OffsetX, OffsetY, KolVan, PapKols, Kols, Kleur, RowVan, _Rows, KopVertSize, Width) :-
  pos(CatNo, Top, _, _),
  RowVan = Top,
  win_SetFont(PrintWin, Font_Header),
  wc(CatNo, _, _, Onderdeel, _, _, _, _, _, _, _, _, _, _, _),
  %wc(CatNo, Onderdeel,_,_,_,_,_),
  concat(Onderdeel,"   ", OnderdeelE),
  win_GetTextExtent(PrintWin,OnderdeelE,-1,Width1,Height),
  Width = Width1 + 20,
  KopVert1 = OffsetY + 6,
  KopVert2 = KopVert1 + Height + 3,
  KopVertSize = Height + 9,
  schaal(PrintWin,_Regelhoogte, Kolombreedte),	
  kolsRechts(KolVan, PapKols, Kols, KolRechts),
  X1 = KolRechts * Kolombreedte + OffsetX ,
  X0 = X1 - Width,
  %X0 = MaxHor - Width - 7,
  kleur(PrintWin,1,Kleur,0,0,Color),
  Pen1 = pen(2,ps_Solid, Color),
  win_SetPen(PrintWin, Pen1),
  draw_Polyline(PrintWin,[pnt(X0,KopVert1), pnt(X1,KopVert1), pnt(X1,KopVert2),pnt(X0,KopVert2),pnt(X0,KopVert1)]),
  Pen2 = pen(1,ps_Solid, color_Black),
  win_SetPen(PrintWin, Pen2),
  draw_TextInRect(PrintWin,rct(X0,KopVert1/*OffsetY*/,X1,KopVert2),Onderdeel,-1,[dtext_singleline, dtext_center, dtext_vcenter]),	% 15.9.2000
  Pen3 = pen(3, ps_Solid, color_Black),
  win_SetPen(PrintWin, Pen3),
  draw_Line(PrintWin, pnt(OffsetX,OffsetY),pnt(X1,OffsetY)),
  !.
draw_header(_PrintWin, _CatNo, _Font_Header,_OffsetX, _OffsetY, _KolVan, _PapKols, _Kols, _Kleur, _RowVan, _Rows,0,10).

header_opschuif(CatNo,_, _PapKols, _, In, In) :-
  pos(CatNo, Top, _, _ ),  
  Top <= 2, !.
header_opschuif(_CatNo,_, PapKols, _, In, In) :-
  PapKols <= 2, !.
header_opschuif(_CatNo, PrintWin,3, Width, In, In) :-
  schaal(PrintWin,_Regelhoogte, Kolombreedte),	
  Width > Kolombreedte, !.
header_opschuif(CatNo, PrintWin,_, Width, In, In) :-
  pos(CatNo, Top, _, _ ),  	% terug naar bovenaan
  Top = 4, 
  schaal(PrintWin,_Regelhoogte, Kolombreedte),	
  Width > Kolombreedte, !.
header_opschuif(_, _, _PapKols, _, _In, 0).

papierindeling(CatNo, PrintWin, Papier, AfmHor, AfmVert, OffsetX, OffsetY, RowVan,PapRows,KolVan,PapKols,Kols,Vouwen,PaginaSel,VanafDag,TmDag,Waardering,Kleur,Kader,Font_header,Font_norm,Font_score,Font_footer, FontSizeO) :-
  %KolVan > 0,	% ############  test
  RowTot = RowVan + PapRows,
  check_inhoud(PaginaSel, CatNo, RowVan,RowTot,KolVan,PapKols,Vouwen,VanafDag,TmDag),
  %Papier = rct(0,0,PapHor, _PapVert),
  %max_afm(AfmHor, PapHor, _MaxHor),
  blzOpen(OpenDicht),
  open_blz(PrintWin, OpenDicht),
  %######## draw header en eventueel kader
  draw_header(PrintWin, CatNo, Font_Header,OffsetX, OffsetY, KolVan, PapKols, Kols, Kleur, RowVan, PapRows, KopY, KopX),
  header_opschuif(CatNo, PrintWin, PapKols, KopX, KopY, Opschuif),	% 18.6.2000
  OffsetY1 = OffsetY + Opschuif, 
  AfmVert1 = AfmVert, %  + Opschuif,	% 25.8.2000
  kaderlijn(PrintWin, CatNo, Kader,OffsetX, OffsetY, RowVan, RowTot, PapRows, KolVan, PapKols, Kols, AfmVert1),
  %######## draw wedstrijden
  win_SetFont(PrintWin, Font_norm),
  Offset = pnt(OffsetX,OffsetY1),
  toon_wedstrijden(CatNo,PrintWin,Papier,Offset,RowVan,RowTot,KolVan,PapKols,Vouwen,PaginaSel, VanafDag,TmDag,Kleur,Font_norm,Font_score), 
  pos(CatNo, Top, _, _),
  RowTot < Top * 2, !,		% naar bladzijde 'eronder'
  draw_footer(PrintWin, Papier,FontSizeO),
  papierindeling(CatNo, PrintWin, Papier, AfmHor, AfmVert,OffsetX, OffsetY, RowTot,PapRows,KolVan,PapKols,Kols,Vouwen,PaginaSel,VanafDag,TmDag,Waardering,Kleur,Kader,Font_header,Font_norm,Font_score,Font_footer,FontSizeO).   
papierindeling(CatNo, PrintWin, Papier, AfmHor, AfmVert,OffsetX, OffsetY, RowVan,PapRows,KolVan,PapKols,Kols,Vouwen,PaginaSel,VanafDag,TmDag,Waardering,Kleur,Kader,Font_header,Font_norm,Font_score,Font_footer,FontSizeO) :-
  RowTot = RowVan + PapRows,	% naar bladzijde 'eronder'
  pos(CatNo, Top, _, _),	% huidige was 'leeg'
  RowTot < Top * 2, !,
  draw_footer(PrintWin, Papier,FontSizeO),
  papierindeling(CatNo, PrintWin, Papier, AfmHor, AfmVert,OffsetX, OffsetY, RowTot,PapRows,KolVan,PapKols,Kols,Vouwen,PaginaSel,VanafDag,TmDag,Waardering,Kleur,Kader,Font_header,Font_norm,Font_score,Font_footer,FontSizeO).   
papierindeling(CatNo, PrintWin, Papier, AfmHor, AfmVert,OffsetX, OffsetY, _RowVan,PapRows,KolVan,PapKols,Kols,Vouwen,PaginaSel,VanafDag,TmDag, Waardering,Kleur,Kader,Font_header,Font_norm,Font_score,Font_footer,FontSizeO) :-
  KolVan1 = KolVan + PapKols,	% zelfde bladzijde 'eronder'
  KolVan1 < Kols,
  schaal(PrintWin,Regelhoogte, _Kolombreedte),	
  Papier = rct(_,_,_PapHor,PapVert),	
  OffsetY1 = OffsetY + AfmVert + Regelhoogte div 2,
  OffsetY1 + AfmVert < PapVert,
  pos(CatNo, Top, _, _ ),  !,	% terug naar bovenaan
  papierindeling(CatNo, PrintWin, Papier, AfmHor, AfmVert,OffsetX, OffsetY1, Top,PapRows,KolVan1,PapKols,Kols,Vouwen,PaginaSel,VanafDag,TmDag,Waardering,Kleur,Kader,Font_header,Font_norm,Font_score,Font_footer,FontSizeO).   
papierindeling(CatNo, PrintWin, Papier, AfmHor, AfmVert,OffsetX, OffsetY, _RowVan,PapRows,KolVan,PapKols,Kols,Vouwen,PaginaSel,VanafDag,TmDag,Waardering,Kleur,Kader,Font_header,Font_norm,Font_score,Font_footer,FontSizeO) :-
  KolVan1 = KolVan + PapKols,	% bladzijde 'ernaast'
  KolVan1 < Kols,
  pos(CatNo, Top, _, _ ),  !,	% terug naar bovenaan
  draw_footer(PrintWin, Papier,FontSizeO),
  papierindeling(CatNo, PrintWin, Papier, AfmHor, AfmVert,OffsetX, OffsetY, Top,PapRows,KolVan1,PapKols,Kols,Vouwen,PaginaSel,VanafDag,TmDag,Waardering,Kleur,Kader,Font_header,Font_norm,Font_score,Font_footer,FontSizeO).   
papierindeling(CatNo, PrintWin, PapierAfm, AfmHor, AfmVert,OffsetX, OffsetY, RowVan,PapRows,KolVan,PapKols,Kols,Vouwen,PaginaSel,VanafDag,TmDag,Waardering,_Kleur,_Kader,_Font_header,Font_norm,_Font_score,Font_footer,_FontSizeO) :-
  RowTot = RowVan + PapRows,
  check_inhoud(PaginaSel, CatNo, RowVan,RowTot,KolVan,PapKols,Vouwen,VanafDag,TmDag),
  PapierAfm = rct(_,_,PapHor,PapVert),	% eindig met eventuele puntentabel in footerfont
  win_SetFont(PrintWin, Font_footer),
  %Y_sterkte = OffsetY + AfmVert,
  %pos(CatNo, Top, _, _ ),  !,	% terug naar bovenaan
  %Y1 = AfmVert - (RowVan - Top) * _Regelhoogte * 2 + Y0 + offsety1,
  schaal(PrintWin,_Regelhoogte, Kolombreedte),	 !,
  kolsRechts(KolVan, PapKols, Kols, KolRechts),
  KaderX = KolRechts * Kolombreedte + OffsetX ,
  %KaderX = (Kols - KolVan) * Kolombreedte + 3 * OffsetX - 10,
  %write("\nKaderX: ", KaderX),
  positieVanPunten(PrintWin, KaderX, PapHor, AfmHor, AfmHorP, PapVert, AfmVert, OffsetY, AfmVertP),
  %blzNr(Nr),
  %write("\nScan_sterkte op blz:", Nr, " Paphor:", PapHor, " AfmHor:",AfmHor, " AfmHorP:", AfmHorP," OffsetX:", OffsetX),
  scan_sterkte(PRINTWIN, pnt(AfmHorP,AfmVertP), CatNo, Waardering),
  win_SetFont(PrintWin, Font_norm).

regelfit(Afm, Unit, 64, NewUnit,b_False) :-	% = wedstrijden
  MaxRegels = Afm div Unit,
  MaxRegels >= 128, 
  NewUnit = Afm div 136,
  NewUnit > Unit, !.
regelfit(Afm, Unit, 64, Unit,_) :- 		% = wedstrijden
  MaxRegels = Afm div Unit,
  MaxRegels >= 128, !.
regelfit(Afm, Unit, 32, NewUnit,b_False) :-	% = wedstrijden
  MaxRegels = Afm div Unit,
  MaxRegels >= 64, 
  NewUnit = Afm div 68,
  NewUnit > Unit, !.
regelfit(Afm, Unit, 32, Unit,_) :- 		% = wedstrijden
  MaxRegels = Afm div Unit,
  MaxRegels >= 64, !.
regelfit(Afm, Unit, 16,NewUnit,b_False) :-
  Afm div Unit >= 32,
  NewUnit = Afm div 34,
  NewUnit > Unit, !.
regelfit(Afm, Unit, 16, Unit,_) :-
  Afm div Unit >= 32, !.
regelfit(Afm, Unit, 8, NewUnit,b_False) :-
  Afm div Unit >= 16,
  NewUnit = Afm div 17,
  NewUnit > Unit, !.
regelfit(Afm, Unit, 8, Unit,_) :-
  Afm div Unit >= 16, !.
regelfit(_Afm, Unit, 4, Unit,_).

rekKols(PapKols, Rest, Max, Lengte, PapKolsU, SchemaKols) :-	% 15.9.2000 nieuw
  SchemaKols < PapKols, !,
  Lengte = Max + Max div SchemaKols + Rest div (PapKols - 1),
  PapKolsU = PapKols - 1.
rekKols(PapKols, Rest, Max, Max, PapKols, _SchemaKols) :-
  Rest > Max, !.			% doe niets
rekKols(1, Rest, Max, Lengte, 2, _) :-
  Rest > 0.8 * Max, !,
  Lengte = (Rest + Max) div 2.	
rekKols(PapKols, Rest, Max, Max1, PapKols, _) :-
  Max1 = Max + Rest div PapKols.	% rek horizontaal een beetje uit

afmeting(TopIn, PapKolsIn, RegelHoogte, KolomBreedte, Hor, Vert, Rows, Kols, b_True) :-
  dim(TopIn, TopDim),
  1 = (TopDim + 2) mod PapKolsIn,	%  precies één overflow
  TopDim >= 2, !,			% en niet te klein (voor de zekerheid)
  Kols = TopDim + 1, 			% dus vouwen
  %write("\nWordt gevouwen..."),
  Hor = Kols * Kolombreedte,
  Rows = TopIn * 2,
  Vert = Rows * Regelhoogte.
afmeting(TopIn, _PapKolsIn, RegelHoogte, KolomBreedte, Hor, Vert, Rows, Kols, b_False) :-
  dim(TopIn, TopDim), 
  Kols = TopDim + 2,
  Hor = Kols * Kolombreedte,
  Rows = TopIn * 2,
  Vert = Rows * Regelhoogte.

blzOpen(b_False).
blzNr(0).

startJobEx() :-
  assert(blzNr(0)).

getBlzNr(Nr) :-
  blzNr(Nr).

open_blzEx(PrintWin) :-
  blzOpen(Open), !,
  open_blz(PrintWin,Open).
open_blzEx(_).

open_blz(PrintWin, b_False) :-
  %write("\nStartpage"),
  print_StartPage(PrintWin), 
  blzNr(Nr),
  Nr1 = Nr + 1,
  assert(blzNr(Nr1)),
  assert(blzOpen(b_True)), !.
open_blz(_PrintWin, _).

positieVanPunten(_Win,_KaderX,PapierX, AfmX, AfmX, PapierY, AfmY, OffsetY, UitY) :-
  PapierY > AfmY,
  PapierX > AfmX,	% kleiner dan papier
  UitY = AfmY + OffsetY, !.
positieVanPunten(Win,_KaderX,PapierX, AfmX, AfmX, PapierY, _AfmY, OffsetY, UitY) :-
  schaal(Win,Regelhoogte, _Kolombreedte),
  UitY = PapierY - 2 * Regelhoogte - OffsetY,
  PapierX > AfmX, !.
positieVanPunten(_Win, KaderX, PapierX, _AfmX, KaderX, PapierY, AfmY, OffsetY, UitY) :-
  PapierY > AfmY, 
  PapierX2 = PapierX div 2,
  KaderX > PapierX2, !,	% 26.8.2000
  UitY = AfmY + OffsetY.
positieVanPunten(_Win, _KaderX, PapierX, _AfmX, PapierX, PapierY, AfmY, OffsetY, UitY) :-
  PapierY > AfmY, !,
  UitY = AfmY + OffsetY.
positieVanPunten(Win, KaderX, PapierX, _AfmX, PapierX, PapierY, _AfmY, OffsetY, UitY) :-
  PapierX2 = PapierX div 2,
  KaderX < PapierX2,	% 26.8.2000
  schaal(Win,Regelhoogte, _Kolombreedte),
  UitY = PapierY - 2 * Regelhoogte + OffsetY, !.
positieVanPunten(Win, KaderX, _PapierX, _AfmX, KaderX, PapierY, _AfmY, OffsetY, UitY) :-
  schaal(Win,Regelhoogte, _Kolombreedte),
  UitY = PapierY - 2 * Regelhoogte + OffsetY, !.

regelhoogteExtra(Top, Hoogte, RegelHoogte) :-	% de kleintjes wat uitrekken
  Top < 8, !,
  RegelHoogte = Hoogte + Hoogte div 5.
regelhoogteExtra(_, Hoogte, RegelHoogte) :-
  RegelHoogte = Hoogte + 2.

schema_draw(PRINTWIN, Font_Norm, CatNo, PapierAfm, PaginaSel,AfmVert,VanafDag,TmDag, Waardering,Opschuiven,Kleur,Kader,Opvullen,FontSizeO,MinKolsHor,MinKolsVert) :-
  win_SetFont(PrintWin, Font_Norm),
  pos(CatNo, Top, _Cursor, _ ),  		%
  dim(Top, Dimx),
  SchemaKols = Dimx + 2,
  win_GetFontMetrics(PRINTWIN, _Leading, Ascent, Descent),
  PapierAfm = rct(PX1,_PY1,PapHor,PapVert),
  max_kol_afm(CatNo, PRINTWIN, Max, PapierAfm, MinKolsHor, MinKolsVert),
  Hoogte = Ascent + Descent,
  regelhoogteExtra(Top, Hoogte, Regelhoogte),
  regelfit(PapVert, Regelhoogte, PapRows, NewRegelHoogte,Opvullen),
  PapKols1 = (PapHor - PX1) div Max,		% eigenlijk kolfit!
  %write("\nCat:",CatNo," PapKols1:",PapKols1," Max:",Max),
  Rest = (PapHor - PX1) mod Max,
  rekKols(PapKols1, Rest, Max, KolBreedte, PapKols, SchemaKols), % write(" Rest: ", Rest, " PapKols:", PapKols, " Kolbreeddte: ", KolBreedte, " Dim: ", Dimx),
  assert(schaal(PRINTWIN, NewRegelHoogte, KolBreedte)),
  afmeting(Top, PapKols,NewRegelHoogte, KolBreedte,AfmHor, _AfmVert1, _Rows, Kols, Vouwen),
  _Fontnaam = font_GetAttrs(Font_norm, _StyleFlags, FontSize),
  FontSizeHeader = FontSize + FontSize div 3,
  Font_header = font_Create(ff_Times,[fs_Bold],FontSizeHeader),
  FontsizeScore = FontSize - FontSize div 4,
  Font_score = font_SetAttrs(Font_norm,[],FontSizeScore),
  FontSizeFooter = FontSize * 2 div 3,
  Font_footer = font_SetAttrs(Font_norm,[],FontSizeFooter),
  Startrow = Top,
  OffsetX = PX1,
  OffsetY = Opschuiven,
  papierindeling(CatNo,PRINTWIN, PapierAfm, AfmHor, AfmVert,OffsetX, OffsetY, StartRow,PapRows,0,PapKols,Kols,Vouwen,PaginaSel,VanafDag,TmDag,Waardering,Kleur,Kader,Font_header,Font_norm,Font_score,Font_footer,FontSizeO),
  fail.
schema_draw(PrintWin, _, _CatNo, _PapierAfm, _, _, _, _, _Waardering, _Opschuiven, _, _, _, _,_MinKolsHor,_MinKolsVert) :-
  retractall(schaal(PrintWin,_,_)), !.

predicates
%afmetingEx(wedscat CatNo, RCT PapierAfm, integer AfmHor, integer AfmVert, BOOLEAN Passing)
kop_afm(WINDOW Win, wedscat CatNo, integer PapKols, integer Top, integer Kolbreedte, integer KopExtraHoogte)
kop_afm1(WINDOW Win, wedscat CatNo, integer KopVertSize, integer Width)
dubbel(integer, integer, integer, integer, integer, integer, integerlist, integerlist)

clauses

kop_afm(Win, CatNo, _PapKols, Top, _, KopVertSize) :-
  Top <= 2, !,
  kop_afm1(Win, CatNo, KopVertSize, _Width).
kop_afm(Win, CatNo, PapKols, _Top, _, KopVertSize) :-
  PapKols <= 2, !,
  kop_afm1(Win, CatNo, KopVertSize, _Width).
kop_afm(Win, CatNo, PapKols, _Top, Kolbreedte, KopVertSize) :-
  PapKols = 3,
  kop_afm1(Win, CatNo, KopVertSize, Width),
  Width > Kolbreedte, !.
kop_afm(Win, CatNo, _PapKols, Top, Kolbreedte, KopVertSize) :-
  Top = 4,
  kop_afm1(Win, CatNo, KopVertSize, Width),
  Width > Kolbreedte, !.
kop_afm(_Win, _CatNo, _PapKols, _Top, _, 0).

kop_afm1(Win, CatNo, KopVertSize, Width) :-
  Font_norm = win_GetFont(Win),
  _Fontnaam = font_GetAttrs(Font_norm, _StyleFlags, FontSize),
  FontSizeHeader = FontSize + FontSize div 3,
  Font_header = font_Create(ff_Times,[fs_Bold],FontSizeHeader),
  win_SetFont(Win, Font_header),
  wc(CatNo, _, _, Onderdeel, _, _, _, _, _, _, _, _, _, _, _),
  %wc(CatNo, Onderdeel,_,_,_,_,_), 
  concat(Onderdeel,"   ", OnderdeelE),
  win_GetTextExtent(Win,OnderdeelE,-1,Width,KopExtraHoogte),
  KopVertSize = KopExtraHoogte + 9,
  win_SetFont(Win, Font_norm), !.

afmetingEx(CatNo, PRINTWIN, PaginaSel, VanafDag,TmDag,PapierAfm, AfmHor, AfmVList, Blz, Waardering, Opvullen, MinKolsHor, MinKolsVert) :-
  check_inhoud(PaginaSel, CatNo, 0,99,0,99,b_False,VanafDag,TmDag),
  PapierAfm = rct(PX1,PY1,PapHor,PapVert),
  %write("\nPapierafm: ", PapierAfm),
  pos(CatNo, Top, _Cursor, _ ),  		% 
  dim(Top, Dimx),
  SchemaKols = DimX + 2,
  win_GetFontMetrics(PRINTWIN, _Leading, _Ascent, _Descent),
  Hoogte = _Ascent + _Descent,
  regelhoogteExtra(Top, Hoogte, Regelhoogte),	% voor de kleine schema's
  max_kol_afm(CatNo, PRINTWIN, Max, PapierAfm, MinKolsHor, MinKolsVert),
  PapKols1 = (PapHor - PX1) div Max,	% 
  Rest = (PapHor - PX1) mod Max,
  rekKols(PapKols1, Rest, Max, KolBreedte, PapKols, SchemaKols),
  PapVert1 = PapVert - PY1,
  regelfit(PapVert1, Regelhoogte, PapRows, NewRegelHoogte, Opvullen),	% 2 = niet rekken
  kop_afm(PRINTWIN, CatNo, PapKols, Top, Kolbreedte, KopExtraHoogte),
  afmeting(Top, PapKols,NewRegelHoogte, KolBreedte,AfmHor, AfmVert1, Rows, Kols, _Vouwen),
  %write("\nPapKols =",PapKols, " KopExtraHoogte:",KopExtraHoogte),
  sterkte_afm(PRINTWIN, CatNo, Waardering, SterkteHoogte),
  %write("\nSterktehoogte:",SterkteHoogte),
  PapR = 2 * PapRows,
  %write("\nKopExtraHoogte: ", KopExtraHoogte),
  AfmVert = AfmVert1 + SterkteHoogte + KopExtraHoogte,
  dubbel(Rows, PapR, Kols, PapKols, AfmVert, NewRegelHoogte, [], AfmVList),
  %write("\nAfmVertList:", AfmVList),
  Blz = round((Kols / PapKols) * (Rows / PapR)* 10),	% ####################################### 
  %Blz1 = (Kols / PapKols) * (Rows / PapR),
  %write("\nPapKols:",PapKols," Kols:",Kols," PapRows:",PapR," Rows:",Rows, " Blz(r):",Blz," Blz:",Blz1), 
  !.

dubbel(Rows, PapR, _Kols, _PapKols, AfMvert, _,In, [AfMvert|In]) :- 
  Rows >= PapR, !.
dubbel(_, _, Kols, PapKols, AfmVert, _, In, [AfmVert|In]) :- 
  Kols - PapKols <= 0, !.
dubbel(Rows, PapR, Kols, PapKols, AfmVert, NewRegelHoogte, In, Uit) :-
  KolsN = Kols - PapKols,
  AfmVert1 = AfmVert + NewRegelHoogte,
  dubbel(Rows, PapR, KolsN, PapKols, AfmVert, NewRegelHoogte, [AfmVert1|In], Uit).

%BEGIN_DLG Printmarges
/**************************************************************************
	Creation and event handling for dialog: Printmarges
**************************************************************************/

constants

%BEGIN Printmarges, CreateParms, 12:13:56-3.8.2001, Code automatically updated!
  dlg_printmarges_ResID = idd_printmarges
  dlg_printmarges_DlgType = wd_Modal
  dlg_printmarges_Help = idh_print
%END Printmarges, CreateParms

predicates

  dlg_printmarges_eh : EHANDLER
  dlg_printmarges_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST)
  dlg_printmarges_update(DIALOG_VAL_LIST)

clauses

  dlg_printmarges_Create(Parent):-
	sheetmarges(Links, Rechts, Boven, Onder),
	IDC_LINKS =  i(Links),
	IDC_RECHTS = i(Rechts),
	IDC_BOVEN =  i(Boven),
	IDC_ONDER =  i(Onder),
%MARK Printmarges, new variables

	dialog_CreateModal(Parent,dlg_printmarges_ResID,"",
  		[
%BEGIN Printmarges, ControlList, 12:13:56-3.8.2001, Code automatically updated!
		df(idc_links,editint(IDC_LINKS,[]),nopr),
		df(idc_rechts,editint(IDC_RECHTS,[]),nopr),
		df(idc_boven,editint(IDC_BOVEN,[]),nopr),
		df(idc_onder,editint(IDC_ONDER,[]),nopr)
%END Printmarges, ControlList
		],
		dlg_printmarges_eh,0,VALLIST,ANSWER),
	dlg_printmarges_handle_answer(ANSWER,VALLIST).

  dlg_printmarges_handle_answer(idc_ok,VALLIST):-!,
	dlg_printmarges_update(VALLIST).
  dlg_printmarges_handle_answer(idc_cancel,_):-!.  % Handle Esc and Cancel here
  dlg_printmarges_handle_answer(_,_):-
	errorexit().

  dlg_printmarges_update(_VALLIST):-
%BEGIN Printmarges, Update controls, 12:13:56-3.8.2001, Code automatically updated!
	_IDC_LINKS = dialog_VLGetint(idc_links,_VALLIST),
	_IDC_RECHTS = dialog_VLGetint(idc_rechts,_VALLIST),
	_IDC_BOVEN = dialog_VLGetint(idc_boven,_VALLIST),
	_IDC_ONDER = dialog_VLGetint(idc_onder,_VALLIST),
%END Printmarges, Update controls
	_IDC_LINKS =  i(Links),
	_IDC_RECHTS = i(Rechts),
	_IDC_BOVEN =  i(Boven),
	_IDC_ONDER =  i(Onder),
	assert(sheetmarges(Links, Rechts, Boven, Onder)),
	true.

%MARK Printmarges, new events

%BEGIN Printmarges, idc_help _CtlInfo
  dlg_printmarges_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Print"),
	!.
%END Printmarges, idc_help _CtlInfo

  dlg_printmarges_eh(_,_,_):-!,fail.

%END_DLG Printmarges

%BEGIN_WIN Kladblok
/**************************************************************************
        Creation and event handling for window: Kladblok
**************************************************************************/

constants
%BEGIN Kladblok, CreateParms, 11:11:52-26.7.2011, Code automatically updated!
  win_kladblok_WinType = w_TopLevel
  win_kladblok_Flags = [wsf_SizeBorder,wsf_TitleBar,wsf_Close,wsf_ClipSiblings,wsf_ClipChildren]
  win_kladblok_RCT = rct(100,80,570,356)
  win_kladblok_Menu = no_menu
  win_kladblok_Title = "Kladblok"
  win_kladblok_Help = idh_kladblok
%END Kladblok, CreateParms

database - kladbl
  klWin(WINDOW)
determ  kltimer(integer)
%single kladbloktekst(string)

predicates

  win_kladblok_eh : EHANDLER
procedure  kladP(integer, tijd, string) - (i, o, o)

clauses

kladblokPopup() :-
  kladblok(b_True, _),
  klad(1, _, Tekst),
  fronttoken(Tekst, _, _),
  TaskWin = vpi_GetTaskWin(),
  win_kladblok_Create(TaskWin), !.
kladblokPopup().

kladblokrefresh() :-
  klWin(Win),
  EWin = win_GetCtlHandle(Win, idk_edit),
  TextN = edit_getText(EWin),
  klad(1, _, TekstNN),
  not(searchstring(TextN, TekstNN, _)),
  edit_pastestr(EWin, TekstNN),
  edit_fileEnd(EWin),
  win_BringToTop(Win), !. 
kladblokrefresh().

kladP(Nr, Tijdstip, Tekst) :-
  klad(Nr, Tijdstip, Tekst), !.
kladP(_, 0, ""). 

kladblokUpd() :-
  kladblokCounter(I),
  kladblok(b_True, Minuten),
  %write("\n", I),
  Minuten = I * 2,
  retract(kladblokCounter(_)),
  assert(kladblokCounter(0)),
  kladblokPopup(), !.
kladblokUpd() :-
  kladblok(b_True, _Minuten),
  retract(kladblokCounter(I)),
  I1 = I + 1,
  assert(kladblokCounter(I1)), !.
kladblokUpd() :-
  kladblok(b_True, _Minuten),
  assert(kladblokCounter(0)), !.
kladblokUpd() .

close_kladblok() :-
  klWin(Win),
  win_Destroy(Win), !.
close_kladblok().

kladblokTimerOn() :-
  not(klTimer(_)),
  loglock(lees, "kladblok"),
  logclose(),
  fail.
kladblokTimerOn() :-
  retract(klTimer(TimerId)),
  timer_kill(TimerId),
  fail.
kladblokTimerOn() :-
  klWin(_Win),
  not(klTimer(_)), !,
  TimerId = timer_set(_Win, 1000),  % 1 seconde
  assert(klTimer(TimerId)).

savekladblok() :-
  klWin(Win),
  EWin = win_GetCtlHandle(Win, idk_edit),
  TextN = edit_getText(EWin),
  kladP(1, _, TextO),
  %kladP(1, _, TekstO),
  not(TextN = TeXtO),
  write("Savekladblok: ", TextN),
  logf('Z', klad(1, 0, TextN), nee), !.
savekladblok().

  win_kladblok_Create(_Parent):-
	vpi_Alarm( mesbox_iconexclamation ),
	klWin(Win),
	trap(win_BringToTop(Win),_,true), !.
	%win_PostEvent(Win,e_User(1,0)), !.	% zorg dat de focus goed komt
  win_kladblok_Create(_Parent):-
	klWin(_Win), !.
  win_kladblok_Create(_Parent):-
	kladP(1, _, Tekst),
	retractall(editcontroltext(idk_edit,_,_,_,_,_,_)),
	assert(editcontroltext(idk_edit,Tekst,b_True,ff_Helvetica,b_False,9,1)),
	win_Create(win_kladblok_WinType,win_kladblok_RCT,win_kladblok_Title,
		   win_kladblok_Menu,_Parent,win_kladblok_Flags,win_kladblok_eh,0).

%BEGIN Kladblok, e_Create
  win_kladblok_eh(_Win,e_Create(_),0):-!,
%BEGIN Kladblok, InitControls, 11:11:52-26.7.2011, Code automatically updated!
	win_CreateDynControl([customctl(wdef(wc_Custom,rct(47,55,376,232),"Custom",u_Pixels),"pdcedit",idk_edit,[wsf_Group,wsf_TabStop])],_Win),
%END Kladblok, InitControls
%BEGIN Kladblok, ToolbarCreate, 11:11:52-26.7.2011, Code automatically updated!
	tb_lifebar_Create(_Win),
	tb_kladbloktb_Create(_Win),
%END Kladblok, ToolbarCreate
	vensterpositie(kladblokvenster, _Win),	% als bewaard van de vorige keer
	assert(klWin(_Win)),
	%win_PostEvent(_Win,e_User(1,0)),
	!.
%END Kladblok, e_Create
%MARK Kladblok, new events

%BEGIN Kladblok, idk_afgewerkt
  win_kladblok_eh(_Win,e_Menu(idk_afgewerkt,_ShiftCtlAlt),0):-!,
	1 = dlg_MessageBox( "Kladblok", "Opmerkingen verwerkt?\nHet kladblok wordt geschoond.", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
	EWin = win_GetCtlHandle(_Win, idk_edit),
	edit_pastestr(EWin, ""),
	savekladblok(),
	!.
%END Kladblok, idk_afgewerkt

%BEGIN Kladblok, idts_instellingen
  win_kladblok_eh(_Win,e_Menu(idts_instellingen,_ShiftCtlAlt),0):-!,
	dlg_toernooiinstellingen_Create(_Win, [1]),
	!.
%END Kladblok, idts_instellingen

%BEGIN Kladblok, e_Timer
  win_kladblok_eh(_Win,e_Timer(TimerId),0):-
 	savekladblok(),
 	timer_Kill(TimerId),
 	retractall(kltimer(_)),
 	!.
%END Kladblok, e_Timer

%BEGIN Kladblok, id_Onderdeel_print
  win_kladblok_eh(_Win,e_Menu(id_Onderdeel_print,_ShiftCtlAlt),0):-!,
	EWin = win_GetCtlHandle(_Win, idk_edit),
	TextN = edit_GetText(EWin),
	fronttoken(TextN, _, _),
	trim_c(TextN),
        formatOpm(TextN, "", "", TextNF, "", 70),
  get_TempDir(_, TempFile),
	file_str(TempFile, TextNF),
	dlg_rapport_Create(_Win,"Afdrukken van kladblok","kladblok",b_False),
	!.
%END Kladblok, id_Onderdeel_print

%BEGIN Kladblok, e_GetFocus
  win_kladblok_eh(_Win,e_GetFocus,0):-!,
	EWin = win_GetCtlHandle(_Win, idk_edit),
	trap(win_SetFocus(EWin),_, true),
	%win_PostEvent(_Win,e_User(1,0)),
	!.
%END Kladblok, e_GetFocus

%BEGIN Kladblok, e_User
  win_kladblok_eh(_Win,e_User(1,_Ptr),0):-
	vpi_Alarm( mesbox_iconexclamation ),
	EWin = win_GetCtlHandle(_Win, idk_edit),
	trap(win_SetFocus(EWin),_, true),
	!.
%END Kladblok, e_User

%BEGIN Kladblok, idk_undo
  win_kladblok_eh(_Win,e_Menu(idk_undo,_ShiftCtlAlt),0):-
	EWin = win_GetCtlHandle(_Win, idk_edit),
	kladP(1, _, Tekst),
	edit_pastestr(EWin, Tekst),
	!.
%END Kladblok, idk_undo

%BEGIN Kladblok, idts_help
 /* win_kladblok_eh(_Win,e_Menu(idts_help,_ShiftCtlAlt),0):-!,
	project_ShowHelpContext("Kladblok"),
	!. */
%END Kladblok, idts_help

%BEGIN Kladblok, e_Destroy
  win_kladblok_eh(_Win,e_Destroy,0):-
        retractall(editcontroltext(idk_edit,_,_,_,_,_,_)),
	updateVensterpositie(kladblokvenster, _Win),
	retractall(klWin(_)),
	!.
%END Kladblok, e_Destroy

%BEGIN Kladblok, e_Size
  win_kladblok_eh(_Win,e_Size(_Width,_Height),0):-!,
ifdef use_tbar
	toolbar_Resize(_Win),
enddef
	EWin = win_GetCtlHandle(_Win, idk_edit),
	RCT0 = win_GetClientRect( _Win ),
	toolbar_GetRect(_Win,tb_bottom,TbRCTB),
	TbRCTB = rct(_, TB, _, BB),
	toolbar_GetRect(_Win,tb_left,TbRCTL),
	TbRCTL = rct(_, _, RL, _),
	RCT0 = rct(_L, T0, R, H0),
	H1 = H0 - BB + TB,	% adjust voor toolbar
	win_Move(EWin,rct(RL, T0, R, H1)),
	!.
%END Kladblok, e_Size

%BEGIN Kladblok, e_Menu, Parent window 
  win_kladblok_eh(Win,e_Menu(ID,CAS),0):-!,
	PARENT = win_GetParent(Win),
	win_SendEvent(PARENT,e_Menu(ID,CAS)),
	!.
%END Kladblok, e_Menu, Parent window

%END_WIN Kladblok

%BEGIN_TLB KladblokTB, 10:35:08-23.8.2011, Code automatically updated!
/**************************************************************************
	Creation of toolbar: KladblokTB
**************************************************************************/

clauses

  tb_kladbloktb_Create(_Parent):-
ifdef use_tbar
	toolbar_create(tb_bottom,0xC0C0C0,_Parent,
		[separator,
		 separator,
		 separator,
		 tb_ctrl(idts_instellingen,pushb,idb_instellingenq,idb_instellingendown,idb_grijs,"Instellingen;Instellingen",1,1),
		 tb_ctrl(idk_undo,pushb,idb_undow,idb_undo_dn,idb_grijs,"Undo;Undo",1,1),
		 tb_ctrl(idk_afgewerkt,pushb,idb_todow,idb_todop,idb_grijs,"Maak schoon;Maak schoon",1,1),
		 tb_ctrl(id_Onderdeel_print,pushb,idb_printw,idb_printdown,idb_print_disabled,"Afdruk;Kladbloktekst afdrukken",1,1)]),
enddef
	true.
%END_TLB KladblokTB


















%BEGIN_DLG Bloktijden per leeftijd
/**************************************************************************
	Creation and event handling for dialog: Bloktijden per leeftijd
**************************************************************************/

constants

%BEGIN Bloktijden per leeftijd, CreateParms, 20:47:55-12.8.2001, Code automatically updated!
  dlg_bloktijden_per_leeftijd_DlgType = wd_Modeless
  dlg_bloktijden_per_leeftijd_Title = "Bloktijden per leeftijd"
  dlg_bloktijden_per_leeftijd_RCT = rct(50,40,292,196)
  dlg_bloktijden_per_leeftijd_Flags = [wsf_Close,wsf_TitleBar]
  dlg_bloktijden_per_leeftijd_Help = idh_bloktijden_per_leeftijdsgroep
  dlg_bloktijden_per_leeftijd_Font = "MS Sans Serif"
  dlg_bloktijden_per_leeftijd_FSize = 8
%END Bloktijden per leeftijd, CreateParms
  blokrefr = 33

database - tbloktijden
single curLft(integer Van, integer TM)
  t_bl(integer Van, integer TM, verhl)

predicates

  dlg_bloktijden_per_leeftijd_eh : EHANDLER
  nondeterm reprBlLft(string)					% representatie van een leeftijdscat
  procedure reprBlLft(string In, integer Van, integer Tot)	% representatie van een leeftijdscat
  procedure copyBl()					% kopiëer bloktijden naar temp
  procedure sortT_Bl()
  %reprPer(period, string Repr)
  %reprBlokList(verhl, slist In, slist Uit)			% representeer een periode (delen?)
  procedure saveBl
  compBlNeq						% ongelijk?
  updateLftList(WINDOW)
  procedure lftPos(integer Van, ilist VanList, integer , integer Index)
  %remove(WINDOW, integer, integer, verhl, verhl)
  procedure zeefPer(verhl, verhl)

clauses

curLft(1,99).

copyBl() :-		
  blLft(Van,Tm, Tijden),
  not(t_bl(Van, Tm, _)),		% maar eentje
  zeefPer(Tijden, Tijden1),
  not(Tijden1=[]),
  assertz(t_bl(Van, Tm, Tijden1)),
  fail.
copyBl().

zeefPer([per(T1,T2)|Rest], [per(T1,T2)|Uit]) :-
  tijd_kwart(D, _, _, T1),
  dag(D, _DStr, _), !,		% dag moet bestaan
  zeefPer(Rest, Uit).
zeefPer([_|Rest], Uit) :-
  zeefPer(Rest, Uit), !.
zeefPer(_, []). 

saveBl() :-
  not(compBlNeq()), !.			% hoeft niets gesaved te worden
saveBl() :-
  blLft(Van, Tot, List),
  logf('R', blLft(Van, Tot, List), ja),
  fail.
saveBl() :-
  sortT_Bl(),
  retract(t_bl(Van, Tot, List)),
  not(List = []),
  logf('Z', blLft(Van, Tot, List), ja),
  fail.
saveBl() :-
  logclose().

sortT_Bl() :-
  findall(Van, blLft(Van, _, _), VanL),
  sortint_c(VanL),
  reverse(VanL, [], VanLR),
  member(Vanx, VanLR),
  getbacktrack(Here),
  retract(t_bl(Vanx, Tot, List)),
  cutbacktrack(Here),
  asserta(t_bl(VanX, Tot, List)),
  fail.
sortT_Bl().  

compBlNeq() :-				% slaagt als ongelijk
  t_bl(Van , Tot, Tijden),
  not(blLft(Van, Tot, Tijden)), !.
compBlNeq() :-				% slaagt als ongelijk
  blLft(Van , Tot, Tijden),
  not(t_bl(Van, Tot, Tijden)), !.

%blLft(integer LftVan, integer LftTot, verhl)	% leeftijdafhankelijke blokkering

reprBlLft(Rep) :-		% representatie van een leeftijdscat
  t_bl(Van , Tot, _Tijden),
  format(Rep, "%u - %u", Van, Tot).

reprBlLft(Rep, Van, Tot) :-
  t_bl(Van , Tot, _Tijden),
  format(Repx, "%u - %u", Van, Tot),
  Rep = Repx, !.
reprBlLft(_Rep, 0, 99).

updateLftList(_Win) :-
  WList = win_GetCtlHandle(_Win, idc_leeft),		% tijdstippen
  findall(Rep, reprBlLft(Rep), RepList),
  lbox_Clear(WList),
  lbox_Add(WList, RepList),
  curLft(Van, _Tot),
  findall(VanX, t_bl(VanX, _, _), VanList),
  lftPos(Van, VanList, 0, Index),
  lbox_SetSel(WList, Index, b_True), !.

lftPos(Van, [Van|_], N, N) :- !.
lftPos(Van, [_|VanList], N, Index) :-
  N1 = N + 1, !,
  lftPos(Van, VanList, N1, Index).
lftPos(_Van, _, N, N).  

  dlg_bloktijden_per_leeftijd_Create(Parent):-
	win_CreateDynDialog(Parent,
		[
%BEGIN Bloktijden per leeftijd, WinDefList, 20:47:55-12.8.2001, Code automatically updated!
		 dlg_font(wdef(dlg_bloktijden_per_leeftijd_DlgType,dlg_bloktijden_per_leeftijd_RCT,dlg_bloktijden_per_leeftijd_Title,u_DlgBase),
		 	  dlg_bloktijden_per_leeftijd_Font,dlg_bloktijden_per_leeftijd_FSize,dlg_bloktijden_per_leeftijd_Flags),
		 ctl(wdef(wc_LBox,rct(3,17,56,142),"",u_DlgBase),idc_leeft,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight]),
		 ctl(wdef(wc_LBox,rct(60,17,201,142),"",u_DlgBase),idcbl_blokperioden,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_UseTabStops]),
		 ctl(wdef(wc_PushButton,rct(206,130,238,154),"&OK",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(206,117,238,127),"&Cancel",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(206,103,238,113),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(206,54,238,66),"&Af",u_DlgBase),idcbl_af,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(206,40,238,52),"&Bij",u_DlgBase),idcbl_bij,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(5,143,53,155),"&Wissen",u_DlgBase),idcbl_lftbloktijden_wis,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_Text,rct(4,7,55,17),"Leeft van - t/m",u_DlgBase),idct_leeft_van_tm,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(64,7,200,17),"",u_DlgBase),idctbl_blokleeft,[wsf_AlignCenter])
%END Bloktijden per leeftijd, WinDefList
		],dlg_bloktijden_per_leeftijd_eh,0).

%BEGIN Bloktijden per leeftijd, idc_ok _CtlInfo
  dlg_bloktijden_per_leeftijd_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	saveBl(),
	win_Destroy(_Win),
	!.
%END Bloktijden per leeftijd, idc_ok _CtlInfo
%MARK Bloktijden per leeftijd, new events

%BEGIN Bloktijden per leeftijd, idcbl_blokperioden selchanged
  dlg_bloktijden_per_leeftijd_eh(_Win,e_Control(idcbl_blokperioden,_CtrlType,LboxWin,selchanged),0):-
	_Index = lbox_GetSelIndex(LboxWin),
	dialog_SetState(_Win, [enable(idcbl_af, b_True)]),
	!.
  dlg_bloktijden_per_leeftijd_eh(_Win,e_Control(idcbl_blokperioden,_CtrlType,_CtrlWin,selchanged),0):-
	dialog_SetState(_Win, [enable(idcbl_af, b_False)]),
	!.
%END Bloktijden per leeftijd, idcbl_blokperioden selchanged

%BEGIN Bloktijden per leeftijd, idcbl_lftbloktijden_wis _CtlInfo
  dlg_bloktijden_per_leeftijd_eh(_Win,e_Control(idcbl_lftbloktijden_wis,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idc_leeft),
	lbox_GetSel(LboxWin,ItemList,_IndexList),
	ItemList = [ReprSel|_],
	reprBlLft(ReprSel, Van, Tot),
	format(Msg, "Alle bloktijden voor %s wissen?", ReprSel), 
	1 = dlg_MessageBox( "Leeftijdscategorie", Msg , mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
	retractall(t_bl(Van, Tot, _)),
	dialog_SetState(_Win, [enable(idcbl_af, b_False)]),
	findall(Repr, reprBlLft(Repr),Lijst),			% de verschillende leeftijdscategorieën
	LboxWin = win_GetCtlHandle(_Win, idc_leeft),
	lbox_Clear(LboxWin),
	lbox_Add(LboxWin, Lijst),
	lbox_SetSel(LboxWin, 0, b_True),
	% -----------
	List = win_GetCtlHandle(_Win, idcbl_blokperioden),	% de bloktijdenlijst
	lbox_GetSel(LboxWin,Items,_),
	Items = [ReprSel1|_],
	OpschrWin = win_GetCtlHandle(_Win, idctbl_blokleeft),
	format(Opschrift, "Voor leeftijdsgroep %s jaar.", ReprSel1),
	win_SetText(OpschrWin, Opschrift),
	reprBlLft(ReprSel1, Van1, Tot1),
	t_bl(Van1, Tot1, BlokPerList1),
	reprBlokList(BlokPerList1, [], BlokStrList1),
	lbox_Clear(List),
	lbox_Add(List, BlokStrList1),
	!.
%END Bloktijden per leeftijd, idcbl_lftbloktijden_wis _CtlInfo

%BEGIN Bloktijden per leeftijd, e_User
  dlg_bloktijden_per_leeftijd_eh(_Win,e_User(_Id,_Ptr),0):-!,
	List = win_GetCtlHandle(_Win, idcbl_blokperioden),		% tijdstippen
	curLft(Van, Tot),
	t_bl(Van, Tot,BlokPerList),
	%nl, write("van e_user:", BlokPerList),
	reprBlokList(BlokPerList, [], BlokStrList),
	lbox_Clear(List),
	lbox_Add(List, BlokStrList),
	updateLftList(_Win),
	dialog_SetState(_Win, [enable(idcbl_af, b_False)]),
	!.
%END Bloktijden per leeftijd, e_User

%BEGIN Bloktijden per leeftijd, e_CloseRequest
  dlg_bloktijden_per_leeftijd_eh(_Win,e_CloseRequest,0):-
	compBlNeq,
	1 = dlg_MessageBox( "Veranderd", "Bloktijden veranderd\nBewaren?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
	saveBl(),
	!.
%END Bloktijden per leeftijd, e_CloseRequest

%BEGIN Bloktijden per leeftijd, idcbl_bij _CtlInfo
  dlg_bloktijden_per_leeftijd_eh(_Win,e_Control(idcbl_bij,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idc_leeft),
	lbox_GetSel(LboxWin,ItemList,_IndexList),
	ItemList = [ReprSel|_],
	reprBlLft(ReprSel, Van, Tot),
	assert(curLft(Van, Tot)),
	dlg_bloktijden_per_leeftijd_bij_Create(_Win),
	!.
  dlg_bloktijden_per_leeftijd_eh(_Win,e_Control(idcbl_bij,_CtrlType,_CtrlWin,_CtlInfo),0):-
	dlg_bloktijden_per_leeftijd_bij_Create(_Win),
	!.
%END Bloktijden per leeftijd, idcbl_bij _CtlInfo

%BEGIN Bloktijden per leeftijd, idcbl_af _CtlInfo
  dlg_bloktijden_per_leeftijd_eh(_Win,e_Control(idcbl_af,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idc_leeft),
	lbox_GetSel(LboxWin,ItemList,_IndexList),
	ItemList = [ReprSel|_],
	reprBlLft(ReprSel, Van, Tot),
	PerListWin = win_GetCtlHandle(_Win, idcbl_blokperioden),
	Index = lbox_GetSelIndex(PerListWin),
	retract(t_bl(Van, Tot, BlList)),
	remove(0, Index, BlList, NewList),
	assert(t_bl(Van, Tot, NewList)),
	sortT_Bl(),
	lbox_Delete(PerListWin, Index),
	!.
%END Bloktijden per leeftijd, idcbl_af _CtlInfo


%BEGIN Bloktijden per leeftijd, idc_leeft selchanged
  dlg_bloktijden_per_leeftijd_eh(_Win,e_Control(idc_leeft,_CtrlType,_CtrlWin,selchanged),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idc_leeft),
	lbox_GetSel(LboxWin,ItemList,_IndexList),
	ItemList = [ReprSel|_],
	OpschrWin = win_GetCtlHandle(_Win, idctbl_blokleeft),
	format(Opschrift, "Voor leeftijdsgroep %s jaar", ReprSel),
	win_SetText(OpschrWin, Opschrift),
	reprBlLft(ReprSel, VAn, Tot),
	List = win_GetCtlHandle(_Win, idcbl_blokperioden),	% de bloktijdenlijst
	t_bl(Van, Tot, BlokPerList),
	reprBlokList(BlokPerList, [], BlokStrList),
	lbox_Clear(List),
	lbox_Add(List, BlokStrList),
	dialog_SetState(_Win, [enable(idcbl_af, b_False)]),
	!.
%END Bloktijden per leeftijd, idc_leeft selchanged

%BEGIN Bloktijden per leeftijd, idc_help _CtlInfo
  dlg_bloktijden_per_leeftijd_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Bloktijdenperleeftijdsgroep"),
	!.
%END Bloktijden per leeftijd, idc_help _CtlInfo

%BEGIN Bloktijden per leeftijd, idc_cancel _CtlInfo
  dlg_bloktijden_per_leeftijd_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-
	compBlNeq,
	1 = dlg_MessageBox( "Veranderd", "Bloktijden zijn veranderd.\nBewaren?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
	saveBl(),
	win_Destroy(_Win),
	!.
  dlg_bloktijden_per_leeftijd_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-
	win_Destroy(_Win),
	!.
%END Bloktijden per leeftijd, idc_cancel _CtlInfo

%BEGIN Bloktijden per leeftijd, e_Create
  dlg_bloktijden_per_leeftijd_eh(_Win,e_Create(_CreationData),0):-!,
	copyBl(),
	dialog_SetState(_Win, [enable(idcbl_af, b_False)]),
	findall(Repr, reprBlLft(Repr),Lijst),			% de verschillende leeftijdscategorieën
	LboxWin = win_GetCtlHandle(_Win, idc_leeft),
	lbox_SetTabStops(LboxWin, [10, 20, 30]),
	lbox_Add(LboxWin, Lijst),
	lbox_SetSel(LboxWin, 0, b_True),
	% -----------
	List = win_GetCtlHandle(_Win, idcbl_blokperioden),	% de bloktijdenlijst
	Font = font_create(ff_Fixed, [], 9),
	win_SetFont(List, Font),
	lbox_GetSel(LboxWin,ItemList,_IndexList),
	ItemList = [ReprSel|_],
	OpschrWin = win_GetCtlHandle(_Win, idctbl_blokleeft),
	format(Opschrift, "Voor leeftijdsgroep %s jaar.", ReprSel),
	win_SetText(OpschrWin, Opschrift),
	reprBlLft(ReprSel, VAn, Tot),
	t_bl(Van, Tot, BlokPerList),
	reprBlokList(BlokPerList, [], BlokStrList),
	lbox_Add(List, BlokStrList),
	!.
%END Bloktijden per leeftijd, e_Create

%BEGIN Bloktijden per leeftijd, e_Destroy
  dlg_bloktijden_per_leeftijd_eh(_Win,e_Destroy,0):-!,
	retractall(t_bl(_ , _, _)),
	!.
%END Bloktijden per leeftijd, e_Destroy

  dlg_bloktijden_per_leeftijd_eh(_,_,_):-!,fail.

%END_DLG Bloktijden per leeftijd

%BEGIN_DLG Bloktijden per leeftijd Bij
/**************************************************************************
	Creation and event handling for dialog: Bloktijden per leeftijd Bij
**************************************************************************/

constants

%BEGIN Bloktijden per leeftijd Bij, CreateParms, 20:49:33-12.8.2001, Code automatically updated!
  dlg_bloktijden_per_leeftijd_bij_DlgType = wd_Modal
  dlg_bloktijden_per_leeftijd_bij_Title = "Bloktijden per leeftijd Bij"
  dlg_bloktijden_per_leeftijd_bij_RCT = rct(50,40,242,206)
  dlg_bloktijden_per_leeftijd_bij_Flags = [wsf_Close,wsf_TitleBar]
  dlg_bloktijden_per_leeftijd_bij_Help = idh_bloktijden_per_leeftijdsgroep
  dlg_bloktijden_per_leeftijd_bij_Font = "MS Sans Serif"
  dlg_bloktijden_per_leeftijd_bij_FSize = 8
%END Bloktijden per leeftijd Bij, CreateParms

predicates

  dlg_bloktijden_per_leeftijd_bij_eh : EHANDLER
  getGrenzen(WINDOW, integer Van, integer Tot)
procedure gett_bl(integer Van, integer Tot, verhl Inlist)
procedure setGrenzen(WINDOW Win, integer Van, integer Tot)

clauses

getGrenzen(_Win, _Van, _Tot) :-
  WinV = win_GetCtlHandle(_Win, idc_bij_van),
  VanS = win_GetText(WinV),
  not(str_int(VanS, _)),
  dlg_MessageBox( "Leeftijd", "Geef ondergrens aan", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  win_SetFocus(WinV), !,
  fail.
getGrenzen(_Win, _Van, _Tot) :-
  WinT = win_GetCtlHandle(_Win, idc_bij_tm),
  TotenMetS = win_GetText(WinT),
  not(str_int(TotenMetS, _)),
  dlg_MessageBox( "Leeftijd", "Geef bovengrens aan", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  win_SetFocus(WinT), !,
  fail.
getGrenzen(_Win, _, _) :-
  WinV = win_GetCtlHandle(_Win, idc_bij_van),
  VanS = win_GetText(WinV),
  str_int(VanS, Van),
  WinT = win_GetCtlHandle(_Win, idc_bij_tm),
  TotenMetS = win_GetText(WinT),
  str_int(TotenMetS, Tot),
  not(t_bl(Van, Tot, _)),		% het is niet een bestaande
  t_bl(A, B, _),
  Van >= A,
  Van <= B,
  dlg_MessageBox( "Leeftijd", "Onder/bovengrens overlapt met andere groep", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  win_SetFocus(WinV), !,
  fail.
getGrenzen(_Win, _, _) :-
  WinV = win_GetCtlHandle(_Win, idc_bij_van),
  VanS = win_GetText(WinV),
  str_int(VanS, Van),
  WinT = win_GetCtlHandle(_Win, idc_bij_tm),
  TotenMetS = win_GetText(WinT),
  str_int(TotenMetS, Tot),
  not(t_bl(Van, Tot, _)),
  t_bl(A, B, _),
  Tot >= A,
  Tot <= B,
  dlg_MessageBox( "Leeftijd", "Onder/bovengrens overlapt met andere groep", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  win_SetFocus(WinT), !,
  fail.
getGrenzen(_Win, _, _) :-
  WinV = win_GetCtlHandle(_Win, idc_bij_van),
  VanS = win_GetText(WinV),
  str_int(VanS, Van),
  WinT = win_GetCtlHandle(_Win, idc_bij_tm),
  TotenMetS = win_GetText(WinT),
  str_int(TotenMetS, Tot),
  not(t_bl(Van, Tot, _)),
  Tot < Van,
  dlg_MessageBox( "Leeftijd", "De ondergrens mag niet hoger zijn\ndan de bovengrens!", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  win_SetFocus(WinT), !,
  fail.
getGrenzen(_Win, Van, Tot) :-
  WinV = win_GetCtlHandle(_Win, idc_bij_van),
  VanS = win_GetText(WinV),
  str_int(VanS, Van),
  WinT = win_GetCtlHandle(_Win, idc_bij_tm),
  TmS = win_GetText(WinT),
  str_int(TmS, Tot), !.

gett_bl(Van, Tot, Inlist) :-
  t_bl(Van, Tot, Inlist), !.		% grenzen bestaan al
gett_bl(Van, Tot, []) :-
  assert(t_bl(Van, Tot, [])), !.

setGrenzen(_Win, Van, _Tot) :-
  %Van <> 1,
  str_int(VanS, Van),
  WinV = win_GetCtlHandle(_Win, idc_bij_van),
  win_SetText(WinV, VanS),
  fail.
setGrenzen(_Win, _Van, Tot) :-
  %Tot <> 99,
  str_int(TotS, Tot),
  WinT = win_GetCtlHandle(_Win, idc_bij_tm),
  win_SetText(WinT, TotS),
  fail.
setGrenzen(_Win, _Van, _Tot).

  dlg_bloktijden_per_leeftijd_bij_Create(Parent):-
	win_CreateDynDialog(Parent,
		[
%BEGIN Bloktijden per leeftijd Bij, WinDefList, 20:49:33-12.8.2001, Code automatically updated!
		 dlg_font(wdef(dlg_bloktijden_per_leeftijd_bij_DlgType,dlg_bloktijden_per_leeftijd_bij_RCT,dlg_bloktijden_per_leeftijd_bij_Title,u_DlgBase),
		 	  dlg_bloktijden_per_leeftijd_bij_Font,dlg_bloktijden_per_leeftijd_bij_FSize,dlg_bloktijden_per_leeftijd_bij_Flags),
		 ctl(wdef(wc_LBox,rct(51,12,99,152),"",u_DlgBase),idcb_blokt_dag,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight]),
		 ctl(wdef(wc_LBox,rct(99,12,145,152),"",u_DlgBase),idcb_blokt_van,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_Sort,wsf_NoIntegralHeight,wsf_Disabled,wsf_Invisible,wsf_MultiSelect]),
		 ctl(wdef(wc_PushButton,rct(153,100,187,122),"&Doe",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_Edit,rct(21,61,41,74),"",u_DlgBase),idc_bij_van,[wsf_Group,wsf_TabStop,wsf_AutoHScroll,wsf_AlignLeft]),
		 ctl(wdef(wc_Edit,rct(21,87,41,100),"",u_DlgBase),idc_bij_tm,[wsf_Group,wsf_TabStop,wsf_AutoHScroll,wsf_AlignLeft]),
		 ctl(wdef(wc_PushButton,rct(154,152,186,164),"&Stop",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(154,139,186,149),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(153,75,187,94),"&Hele dag",u_DlgBase),idbc_hele_dag,[wsf_Group,wsf_TabStop,wsf_Invisible]),
		 ctl(wdef(wc_Text,rct(52,2,96,12),"Dag",u_DlgBase),idcbt_dag,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(98,2,128,12),"Van/tot",u_DlgBase),idcbt_vanaf,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(52,154,139,164),"Selecteer gewenste dag",u_DlgBase),idcbt_klik_op_tijdstippen,[wsf_NoWrap]),
		 ctl(wdef(wc_Text,rct(148,14,191,69),"Blokperiode",u_DlgBase),idctb_blokperiode,[wsf_AlignLeft,wsf_Invisible]),
		 ctl(wdef(wc_Text,rct(3,49,47,60),"Leeftijd van",u_DlgBase),idct_leeftijd_van,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(3,75,41,86),"Leeftijd t/m",u_DlgBase),idct_leeftijd_tm,[wsf_AlignLeft])
%END Bloktijden per leeftijd Bij, WinDefList
		],dlg_bloktijden_per_leeftijd_bij_eh,0).

%BEGIN Bloktijden per leeftijd Bij, idc_ok _CtlInfo
  dlg_bloktijden_per_leeftijd_bij_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-!,
	LboxWinD = win_GetCtlHandle(_Win, idcb_blokt_dag),		% doe
	lbox_GetSel(LboxWinD, ItemListD, _IndexListD),			% get dag
	ItemListD = [TextD|_],
	LboxWinT = win_GetCtlHandle(_Win, idcb_blokt_van),
	lbox_GetSel(LboxWinT, _ItemListT, IndexListT),			% get tijden
	not(IndexListT = []),
	dag(D1,TextD,_), 
	items2tijden(D1,IndexListT, _TextT, Periode),
	getGrenzen(_Win, Van, Tot),
	assert(curLft(Van, Tot)),
	%write("\nVan: ", Van, "  Tot: ", Tot),
	gett_bl(Van, Tot, Inlist),
	mergeBl(Periode, Inlist, OutList),
	retract(t_bl(Van, Tot, _)),
	assert(t_bl(Van, Tot, OutList)),
	sortT_bl(),
	KwartListWin = win_GetCtlHandle(_Win, idcb_blokt_van),
	selectGeen(KwartListWin),
	dialog_SetStr(_Win, idctb_blokperiode, ""),
	ParentWin = win_GetParent(_Win),
	win_SendEvent(ParentWin,e_User(blokrefr, 0)),
	!.
%END Bloktijden per leeftijd Bij, idc_ok _CtlInfo
%MARK Bloktijden per leeftijd Bij, new events

%BEGIN Bloktijden per leeftijd Bij, idbc_hele_dag _CtlInfo
  dlg_bloktijden_per_leeftijd_bij_eh(_Win,e_Control(idbc_hele_dag,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idcb_blokt_van),
	selectAlle(LboxWin),
	LboxWinD = win_GetCtlHandle(_Win, idcb_blokt_dag),
	lbox_GetSel(LboxWinD, ItemListD, _IndexListD),			% get dag
	ItemListD = [TextD|_],
	format(TextT, "%s geblokkeerd.", TextD),
	dialog_SetStr(_Win, idctb_blokperiode, TextT),
	dialog_SetState(_Win, [show(idctb_blokperiode, b_true)]),
	!.
%END Bloktijden per leeftijd Bij, idbc_hele_dag _CtlInfo

%BEGIN Bloktijden per leeftijd Bij, idcb_blokt_van selchanged
  dlg_bloktijden_per_leeftijd_bij_eh(_Win,e_Control(idcb_blokt_van,_CtrlType,LboxWinT,selchanged),0):-!,
	LboxWinD = win_GetCtlHandle(_Win, idcb_blokt_dag),
	lbox_GetSel(LboxWinD, ItemListD, _IndexListD),			% get dag
	ItemListD = [TextD|_],
	%LboxWinT = win_GetCtlHandle(_Win, idcb_blokt_van),
	lbox_GetSel(LboxWinT, _ItemListT, IndexListT),			% get tijden
	dag(D1,TextD,_), 
	items2tijden(D1,IndexListT, TextT, _),
	dialog_SetStr(_Win, idctb_blokperiode, TextT),
	dialog_SetState(_Win, [show(idctb_blokperiode, b_true)]),
	!.
%END Bloktijden per leeftijd Bij, idcb_blokt_van selchanged

%BEGIN Bloktijden per leeftijd Bij, e_Create
  dlg_bloktijden_per_leeftijd_bij_eh(_Win,e_Create(_CreationData),0):-!,
	DagList = win_GetCtlHandle(_Win, idcb_blokt_dag),
	Font = font_create(ff_Fixed, [], 9),
	win_SetFont(DagList, Font),
	findall(Dag, dag(_,Dag,_), Dagen),
	lbox_Add(DagList, Dagen),
	KwartListWin = win_GetCtlHandle(_Win, idcb_blokt_van),
	win_SetFont(KwartListWin, Font),
	curLft(Van, Tot),
	%write("\nVan: ", Van, "  Tot: ", Tot),
	setGrenzen(_Win, Van, Tot),
	!.
%END Bloktijden per leeftijd Bij, e_Create


%BEGIN Bloktijden per leeftijd Bij, idcb_blokt_dag selchanged
  dlg_bloktijden_per_leeftijd_bij_eh(_Win,e_Control(idcb_blokt_dag,_CtrlType,LboxWin,selchanged),0):-
	Index = lbox_GetSelIndex(LboxWin),
	_Item = lbox_GetItem(LboxWin, Index),
	dialog_SetState(_Win, [show(idcb_blokt_van, b_True),
				enable(idcb_blokt_van, b_True),
				show(idbc_hele_dag, b_True),
				enable(idbc_hele_dag, b_True),
				show(idcbt_klik_op_tijdstippen, b_False)]),
	KwartListWin = win_GetCtlHandle(_Win, idcb_blokt_van),
	lbox_Clear(KwartListWin),	% om eventuele selecties kwijt te raken
	kwartlist(List),
%	nl, write(List),
	lbox_Add(KwartListWin, List),
	lbox_SetTopIndex(KwartListWin, 60),
	dialog_SetStr(_Win, idctb_blokperiode, ""),
	!.
%END Bloktijden per leeftijd Bij, idcb_blokt_dag selchanged

%BEGIN Bloktijden per leeftijd Bij, idc_help _CtlInfo
  dlg_bloktijden_per_leeftijd_bij_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Bloktijdenperleeftijdsgroep"),
	!.
%END Bloktijden per leeftijd Bij, idc_help _CtlInfo

%BEGIN Bloktijden per leeftijd Bij, idc_cancel _CtlInfo
  dlg_bloktijden_per_leeftijd_bij_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	win_Destroy(_Win),
	!.
%END Bloktijden per leeftijd Bij, idc_cancel _CtlInfo

  dlg_bloktijden_per_leeftijd_bij_eh(_,_,_):-!,fail.

%END_DLG Bloktijden per leeftijd Bij

domains 
  testdom = echt ; istest

database - emailb
  eAdres(integer, MAPINAMEADDR, string Bondsnummer, testdom)
  oproepsp1(integer)  

predicates
determ  lijstPersoonMetEmail(string)
determ  lijstPersoonMetSMS(string)
procedure  aantalAdressen(integer)
procedure eenAdres(integer)
procedure oproepsp(oproepaan)
nondeterm oproepspa(integer, tijd)
%determ string selectMobielNr(string, string, string, meeltype)
procedure removeAlpha(string, string, string) 
procedure emailUitvoer1(window, integer)
nondeterm emailnaamuitgebreid(string Str)
%determ checkAantal(integer)

clauses 

lijstPersoonMetEmail("") :-
  dlg_MessageBox("Email naar lijst", "Selecteer eerst een lijst met F2, F3 of Speciale selectie.", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  !, fail.
lijstPersoonMetEmail(_Titel) :-
  lijstPersoon(Naam),
  sp2(Nr,_,Naam,_,_ ,_,_,_,_,_,_, _,Bondsnummer,_,_,_, _,_,Email,_,_,_,_,_,_,_,_),
  fronttoken(Email,_,_),
  emailproef(Email),
  naamscan(ja, b_False, Naam, Naam1, _, _Landcode),
  assert(eAdres(Nr, mapinameaddr(Naam1,Email),Bondsnummer,echt)),
  fail.
lijstPersoonMetEmail(Titel) :-
  not(eAdres(_,_, _, _)),
  dlg_MessageBox( Titel, "Geen emailadressen in de lijst.", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  !, fail.
lijstPersoonMetEmail(_).

lijstPersoonMetSMS("") :-
  dlg_MessageBox("Bulk-SMS naar lijst", "Selecteer eerst een lijst met F2, F3 of Speciale selectie.", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  !, fail.
lijstPersoonMetSMS(_Titel) :-
  lijstPersoon(Naam),
  %sp2(Nr,_,Naam,_,_ ,_,_,_,_,_,_, _,Bondsnummer,_,_,_, _,_,Email,_,_,_,_,_,_,_,_),
  sp2(Nr,_Sex,Naam,Telthuis,_Verh,_RatingE,_RatingD,_Betaald,_UitKant, _Club, _Adres, _Woonpl, BondsNummer, _ES, _DS, _Gebdatum, Telwerk, Telmobiel, _EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Res),
  MobielNr = selectMobielNr(Telthuis, Telwerk, TelMobiel, smsbulk),
  frontchar(MobielNr,_FrontChar,MobNr),
  format(EmailO, "0031%s@mail-sms.com", MobNr), 	
  assert(eAdres(Nr, mapinameaddr(Naam,EmailO),Bondsnummer,echt)),
  fail.
lijstPersoonMetSMS(Titel) :-
  not(eAdres(_,_, _, _)),
  dlg_MessageBox( Titel, "Geen mobiele nummers in de lijst.", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  !, fail.
lijstPersoonMetSMS(_).

smsbulkuitvoer(Win) :-
  retractall(eAdres(_,_,_, _)),
  lijstPersonenTitel(Titel),
  lijstPersoonMetSMS(Titel),
  dlg_Email_algemeen_create(Win, smsbulk),
  %retractall(eAdres(_,_)),
  !. 
smsbulkuitvoer(_Win).

emailuitvoer(Win) :-
  retractall(eAdres(_,_,_, _)),
  lijstPersonenTitel(Titel),
  lijstPersoonMetEmail(Titel),
  %aantalAdressen(Aantal),
  %checkAantal(Aantal),
  A = dlg_Ask("Bulkemail via","Email via TA of via klembord (met namen):"/*Prompt*/,["&TA zelf","&Klembord", "&Met namen"]/*ButtonTitlesList*/),
  emailuitvoer1(Win, A),
  !. 
emailuitvoer(_).

emailnaamuitgebreid(Str) :- 
  eAdres(_, mapinameaddr(Naam, EA), _, echt),
  format(Str, "%s<%s>", Naam, EA).

emailuitvoer1(Win, resp_default) :-
  dlg_Email_algemeen_create(Win, bulk), !.
emailuitvoer1(_, resp_2) :-
  findall(EA, eAdres(_, mapinameaddr(_, EA), _, echt), EAList),
  list_to_string(EAList, "\r\n", Str),
  cb_PutString(Str),
  dlg_MessageBox( "Emailadressen", "De emailadressen staan op het klembord en kunnen bijvoorbeeld naar een nieuwe map van het adresboek van Outlook, of Windows mail geplakt worden.", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
   !.
emailuitvoer1(_, resp_3) :-
  findall(EA, emailnaamuitgebreid(EA), EAList),
  list_to_string(EAList, "\r\n", Str),
  cb_PutString(Str),
  dlg_MessageBox( "Emailadressen", "De uitgebreid emailnamen/adressen staan op het klembord en kunnen bijvoorbeeld naar het adresboek van Outlook, of Windows mail geplakt worden.", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
   !.
emailuitvoer1(_, _).

emailuitvoerEnkel(Win, Nr, Naam, Email, Bondsnummer) :-
  retractall(eAdres(_,_,_,_)),
  assert(eAdres(Nr, mapinameaddr(Naam,Email),Bondsnummer,echt)),
  dlg_Email_algemeen_create(Win, enkel), !.
emailuitvoerEnkel(_Win, _, _Naam, _Email, _Bondsnummer).

stuurBerichtenOproep(ParentWin, Aanwie) :-
  retractall(eAdres(_,_,_,_)),
  oproepsp(AanWie),
  dlg_email_Algemeen_Create(ParentWin, oproep), !.
  %retractall(eAdres(_,_,_)).
stuurBerichtenOproep(_, _) :-
  retractall(eAdres(_,_,_,_)),
  logclose().

removeAlpha("", In, In) :- !.
removeAlpha(String, In, Uit) :-
  frontstr(1,String,Char,Rest),
  Char >= "0",
  Char <= "9",
  removeAlpha(Rest, In, Uit1),
  concat(Char, Uit1, Uit), !.
removeAlpha(String, In, Uit) :-
  frontstr(1,String,_Char,Rest), !,
  removeAlpha(Rest, In, Uit).
removeAlpha(_, In, In) :- !.

selectmobielNr(_T1, _T2, M, _, M1) :-
  removeAlpha(M, "", M1),
  str_len(M1, Len),
  Len = 10, 
  frontstr(2, M1, Fr, _),
  Fr = "06", !.
selectmobielNr(T1, _T2, _M, _, T1a) :-
  removeAlpha(T1, "", T1a),
  str_len(T1a, Len),
  Len >= 10, 
  frontstr(2, T1a, Fr, _),
  Fr = "06", !.
selectmobielNr(_T1, T2, _M, _, T2a) :-
  removeAlpha(T2, "", T2a),
  str_len(T2a, Len),
  Len >= 10, 
  frontstr(2, T2a, Fr, _),
  Fr = "06", !.
selectmobielNr(_T1, _T2, _M, sms, _T2) :-
  dlg_MessageBox(" SMS ", "Geen mobiel nummer voor deze persoon!.", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.

stuurSMS(Parentwin, Sp) :-
  retractall(eAdres(_,_,_,_)),
  sp2(Sp,_Sex,Naam,Telthuis,_Verh,_RatingE,_RatingD,_Betaald,_UitKant, _Club, _Adres, _Woonpl, BondsNummer, _ES, _DS, _Gebdatum, Telwerk, Telmobiel, _EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Res),
  MobielNr = selectMobielNr(Telthuis, Telwerk, TelMobiel,sms),
  frontchar(MobielNr,_FrontChar,MobNr),
  format(EmailO, "0031%s@mail-sms.com", MobNr), 	
  assert(eAdres(Sp, mapinameaddr(Naam,EmailO),Bondsnummer,echt)),
  dlg_email_Algemeen_Create(ParentWin, sms), !.
stuurSMS(_, _) :-
  retractall(eAdres(_,_,_,_)),
  logclose().

dagnumEval(P, P) :-
  dag(P, _, _), !.
dagnumEval(_, P) :-
  findall(D, dag(D, _, _), DL),
  sortint_c(DL),
  reverse(DL, [], DLR),
  DLR = [P|_].

oproepsp(collectie(Lijst)) :-
  member(Sp, Lijst),
  sp2(Sp,_Sex,Naam,_Telthuis,_Verh,_RatingE,_RatingD,_Betaald,_UitKant, _Club, _Adres, _Woonpl, BondsNummer, _ES, _DS, _Gebdatum, _Telwerk, _Telmobiel, EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Res),
  assert(eAdres(Sp, mapinameaddr(Naam,Email),Bondsnummer,echt)),
  fail.
oproepsp(bulk) :-
  plantm(PlanTM),
  dagnumEval(PlanTM, DagNumP),
  TotN1 = DagNumP + 1,
  bitleft(TotN1, 7, Tot), 		% maak er een tijdstip van ..
  findall(Tijd, bn(Tijd, _), Tijden),
  sortint_c(Tijden),
  retractall(oproepsp1(_)),
  member(TijdX, Tijden),
  TijdX < Tot,
  oproepspa(Sp, TijdX),
  not(oproepsp1(Sp)),
  assert(oproepsp1(Sp)),
  sp2(Sp,_Sex,Naam,_Telthuis,_Verh,_RatingE,_RatingD,_Betaald,_UitKant, _Club, _Adres, _Woonpl, BondsNr, _ES, _DS, _Gebdatum, _Telwerk, _Telmobiel, EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Res),
  fronttoken(Email, _, _),
  emailproef(Email),
  assert(eAdres(Sp, mapinameaddr(Naam,Email),BondsNr,echt)),
  fail.
oproepsp(_Tot) :-
  retractall(oproepsp1(_)).

oproepspa(Sp, Tijd) :-
  ws(_Num,_Cat,T,Tijd),		% er was eens een planning
  tgnstnum(T, Sp).
oproepspa(Sp, Tijd) :-
  w2(Num,Cat,Tijd,_G1,_G2,_,_,_),	 	% 16.7.2014
  %w2(Num,Cat,Tijd,nee,_,_,_,_),	% 19.12.2006
  wd(Num,Cat,A,_,_,_,_),
  tgnstnum(A, Sp).
oproepspa(Sp, Tijd) :-
  w2(Num,Cat,Tijd,_G1,_,_,_,_),	% 	% 29.7.2013
  %w2(Num,Cat,Tijd,_G1,nee,_,_,_),	% 19.12.2006
  wd(Num,Cat,_,B,_,_,_),
  tgnstnum(B, Sp).

aantalAdressen(Aantal) :-
  findall(N, eAdres(N, _, _, echt), NL),
  count(NL, 0, Aantal).
  
eenAdres(Nr) :-
  eAdres(Nr, _, _, echt), !.
eenAdres(0).

%BEGIN_DLG Email algemeen
/**************************************************************************
	Creation and event handling for dialog: Email algemeen
**************************************************************************/

constants

%BEGIN Email algemeen, CreateParms, 12:13:21-14.9.2013, Code automatically updated!
  dlg_email_algemeen_DlgType = wd_Modeless
  dlg_email_algemeen_Title = "Emails versturen"
  dlg_email_algemeen_RCT = rct(50,40,563,336)
  dlg_email_algemeen_Flags = [wsf_Close,wsf_TitleBar]
  dlg_email_algemeen_Help = idh_email_sturen
  dlg_email_algemeen_Font = "MS Sans Serif"
  dlg_email_algemeen_FSize = 8
%END Email algemeen, CreateParms
  emailalg =56
  standaardteksten = 12120	% de basis

database - emaila
  single emailtekst(string Onderwerp, string Tekst)
  single eaantal(integer)
  determ tijdelijkModel(string)
  %determ huidigWindow(Window)
  determ emailWinTimer(Integer)
  single meelTypeF(meeltype)
  single gedaan(boolean)
  determ emailWin(window)
  %uitslagenableE(integer, wedscat, integer Plan,tgnst, tgnst)
  %single iAantekenen(integer)

predicates
  dlg_email_algemeen_eh : EHANDLER
  %aantekenen(meeltype)
  procedure individueelEmail(string Onderwerp, string Mededeling, boolean MetVervolg, testdom)
  procedure individueelEmail1(string Onderwerp, string Mededeling, boolean MetVervolg, testdom)
  nondeterm spUitloot(integer SpNo, string Onderdeel)
  procedure uitgelootvoor(integer Spno, string Tekst)
  procedure zetEmailadresZelf(string Email, string ENaam)
  procedure deelname(integer SpNo, string Tekst)
  nondeterm deelname1(integer SpNo, string Tekst)
  procedure eenbericht(integer, ilist, string Body, boolean MetVervolg, slist Teksten)
  procedure berichtvoorbeelden(window)
  procedure oproepenInvullen(integer SpNo, string Tekst, integer Ret, boolean Metvervolg)
  nondeterm afzeggenS(boolean OokAfzeggen, integer Pers, integer Tot, string Str, string Code)
  leeg(wl, slist)
procedure setAlgemeelX(vehikel, string, string, boolean)
procedure getCurrentModel(window, string Kop, string Body, boolean MetVervolg)
determ algemeelP(meeltype, vehikel, string Onderwerp, string Template, boolean MetVervolgWeds)
determ w2voorTot(integer, wedscat, integer)
determ vervang(string, string, string, string)
nondeterm standaardtekst(integer, menu_item, string)
nondeterm standaardtekstV(meeltype, integer, menu_item, string)
procedure tijdelijkModelP(string, boolean)
nondeterm invul(menu_item)
procedure personaliseer(meeltype, window Win, string Body)
  procedure verzondenOK(string)
procedure oproepEmailKlapper(string Onderwerp, string Mededeling, boolean MetVervolg, testdom IsTest)
procedure wt2str(wl, slist)
procedure enableModel(meeltype, boolean, boolean, boolean, boolean, boolean)
procedure expandeerInTeVullen(string, string)
determ versturen(window, testdom)
determ versturenNotOK(window)
determ mapinameaddr getmailbox(window, testdom)
procedure individueelEmailKlapper(string Onderwerp, string Mededeling, boolean MetVervolg, testdom)
procedure substitueer(integer SpNo, string, string, boolean MetVervolg, integer In, integer PlanInhoud)
%determ aantekenen1(integer SKeus, string EofS)
procedure skipRegel1(string, string)
procedure sluitenMaar(window)
%determ berichtAfm(string Body, boolean MetVervolg, integer Afm, integer Max)
procedure maxAfm(meeltype, integer)
determ tegoedOK(window)
procedure updateTegoed(integer Aantal)
%nondeterm emailprovider(string)
procedure speeltijdenNaHuidig(tijd, wtl,  wtl)
procedure eigenformulier(integer, string)
%procedure eenUitslagenable(integer, wedscat, integer)
procedure maakUitslagEnableUploadXML()
determ maakTeam(tgnst)
determ voegdatumtoe(dbasedom, dbasedom)
procedure removeplan(integer, wedscat)
procedure weltespelen(integer, wedscat, string, string)

clauses
emailtekst("", "").
eaantal(0).
meeltypeF(enkel).
gedaan(b_False).
%iAantekenen(b_False).

w2voorTot(Num, Cat, Tot) :-
  w2(Num,Cat,Tijd,_,_,_,_,_),
  Tijd < Tot, !.

weltespelen(Num, Cat, "Verzet:", " (nieuwe planning volgt)") :-
  wd(Num,Cat,S1,S2,_,_,_),
  echt(S1, S2),
  not(w3(Num, Cat, _, _, _, _, _)), !.
weltespelen(_, _, "Afgelast:", "").

afzeggenS(b_True, Pers, Tot, Str, Code) :-
  huidigetijdG(TH),
  ws(Num,Cat,T,Tijd),		% er was eens een planning
  tgnst_num(T, Pers),
  not(w2voorTot(Num,Cat,Tot)),	% nu geen planning voor Tot
  Tijd >= TH,	% 27.11.2009 !!!!!!!!!!!! anders hoef je niet meer af te zeggen
  Tijd < Tot,
  weltespelen(Num,Cat, PStr, WStr),
  wc(Cat, _, _, _Onderdeel, _, Type, _, _, _, _, _, _, _, _, _),
  %wc(Cat,_,_,Type,_,_,_), 
  tijd_kwart(D1,U1,M1,Tijd),
  dag(D1,DS1,_), 
  helestr_int(0,MS1,M1),
  format(Str, "%s %-5 %-7 %2\46%2 %s\n",PStr, Type,DS1,U1,MS1,WStr),
  term_str(tgnst, T, TStr),
  format(Code, "ws,%u,%u,%u,%u,%s", Num, Cat, Pers, Tijd, TStr).

leeg([], []).


spUitloot(SpNo, Onderdeel) :-
  sp2opg(SpNo,Cat,Opg,_MaatNo,_,_),
  opg(Opg,Cat,Tgnst,_,_,_,_,_,_,_,_),
  not(in_weds(Cat, Tgnst)),
  wc(Cat, _, _, Onderdeel, _, _, _, _, _, _, _, _, _, _, _).

uitgelootvoor(Spno, Tekst) :-
  findall(Onderdeel, spUitloot(SpNo, Onderdeel), Lijst),
  not(Lijst = []), !,
  list_to_string(["Je bent uitgeloot voor:"|Lijst], "\n  ", Tekst). 
uitgelootvoor(_Spno, "").

deelname(SpNo, Tekst) :-
  findall(S, deelname1(SpNo, S), SList),
  not(SList = []), !,
  list_to_string(["Opgegeven voor:"|SList], "\n  ", Tekst).
deelname(_SpNo, "").

deelname1(SpNo, Tekst) :-
  sp2opg(SpNo,CatN,_Opg,MaatNo,_,_),
  met_maat(MaatNo, Maat),
  wc(CatN, _, _, _, _, Str, _, _, _, _, _, _, _, _, _),
  %wc(CatN,_,_,Str,_,_,_),
  format(Tekst, "%s %s", Str, Maat). 

speeltijdenNaHuidig(Huidig, [wt(Wedstyp,WedsTijd,Gewaarsch,BaanPark,BW, WasOp) | Rest], [wt(Wedstyp,WedsTijd,Gewaarsch,BaanPark,BW, WasOp) | Tail]) :-
  %nl, write("huidig ",Huidig, " Wedstijd: ", Wedstijd),
  WedsTijd > Huidig,
  %nl, write("huidig ",Huidig, " Wedstijd: ", Wedstijd),
  speeltijdenNaHuidig(Huidig, Rest,  Tail), !.
speeltijdenNaHuidig(Huidig, [_ | Rest], Tail) :-
  speeltijdenNaHuidig(Huidig, Rest,  Tail), !.
speeltijdenNaHuidig(_, Rest,  Rest).

oproepenInvullen(Sp, Tekst, 1, MetVervolg) :-
  %sp2(Sp,_Sex,Naam,_Telthuis,_Verh,_RatingE,_RatingD,_Betaald,_UitKant, _Club, _Adres, _Woonpl, _BondsNr, _ES, _DS, _Gebdatum, _Telwerk, _Telmobiel, EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Res),
  plantm(PlanTM),
  TotN1 = PlanTM + 1,
  bitleft(TotN1, 7, Tot), 		% maak er een tijdstip van ..
  wd_planned(Sp,WedstrijdList,Tot,MetVervolg),
  findall(Afz, afzeggenS(b_True, Sp, Tot, Afz, _), AfzL),   % slist
  not(leeg(WedstrijdList, AfzL)),	%
  speeltijden1(WedstrijdList,Speeltdn),
  huidigetijdG(Huidig),
  %nl, write(" timestampH: ", TimestampH),
  speeltijdenNaHuidig(Huidig, Speeltdn, SpeeltdnNaH),
  qsortwt(SpeeltdnNaH,SpeeltdnS),
  prt_speelt(SpeeltdnS,0,_,nee,1,1,[],OnderdelenU,0,_X),  % nee = géén vraagteken
  list_to_string(OnderdelenU, "\n", Ost),
  list_to_string(AfzL, "", AfzegListString), !,
  format(Tekst, "%s\n%s",Ost, AfzegListString).
oproepenInvullen(_, "\nGeen oproepen!", 0, _).

eigenformulier(Sp, Tekst) :-
  sp2(Sp,_Sex,_Naam,_Telthuis,_Verh,_RatingE,_RatingD,_Betaald,_UitKant, _Club, _Adres, _Woonpl, _BondsNr, _ES, _DS, _Gebdatum, _Telwerk, _Telmobiel, _EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,Res),
  Res = [H | _],
  naam(_, _, _, WT, _, _),
  makeTicket(WT, WebTicket),
  %providerX(Prov, _, "", _),
  format(Tekst, "Inschrijving aanvullen? Klik:\nhttps://www.toernooiklapper.nl/toernooi/%sX%sX####\nof plak deze link in de browser als klik niet werkt.\n",Webticket,H), !.
eigenformulier(_SpNo, Tekst) :-
  naam(_Naam, _Nr, _Datum, WT, _Distr, _OokReserve),
  makeTicket(WT, WebTicket),
  %providerX(Prov, _, "", _),
  format(Tekst, "Inschrijven? Klik:\nhttps://www.toernooiklapper.nl/toernooi/%s\nof plak deze link in de browser als klik niet werkt.\n", WebTicket).

vervang(SString, In, Begin, Rest1) :-
  searchstring(In,SString,FoundPos),
  str_len(SString, Len),
  F0 = FoundPos - 1,
  frontstr(F0, In, Begin, Rest),
  %F1 = Len + 1,
  frontstr(Len, Rest, _, Rest1).

substitueer(SpNo, In, Uit, MetVervolg, Inh, InhUit) :-
  frontstr(2,In,StartString,R1),
  Startstring = "//",
  %searchstring(In,"\n//",FP1),
  %str_len(In, L1),
  %L1a = FP1,
  %frontstr(L1a, In, L1, R1),
  searchstring(R1, "\n", Fp2),
  frontstr(Fp2, R1, _, R2),
  concat("", R2, In1),   !,
  substitueer(SpNo, In1, Uit, MetVervolg, Inh, InhUit).
substitueer(SpNo, In, Uit, MetVervolg, Inh, InhUit) :-
  searchstring(In,"\n//",FP1),
  %str_len(In, L1),
  L1a = FP1,
  frontstr(L1a, In, L1, R1),
  searchstring(R1, "\n", Fp2),
  frontstr(Fp2, R1, _, R2),
  concat(L1, R2, In1),   !,
  substitueer(SpNo, In1, Uit, MetVervolg, Inh, InhUit).
substitueer(SpNo, In, Uit, MetVervolg, Inh, InhUit) :-
  searchstring(In,"\n//",FP1),
  %str_len(In, L1),
  L1a = FP1,
  frontstr(L1a, In, L1, R1),
  not(searchstring(R1, "\n", _)), !,
  %frontstr(Fp2, R1, _, R2),
  %concat(L1, R2, In1),   !,
  substitueer(SpNo, L1, Uit, MetVervolg, Inh, InhUit).
substitueer(SpNo, In, Uit, MetVervolg, Inh, InhUit) :-	% InhUit = 0 ==> geen individuele planninginfo
  vervang("%toernooinaam%", In, Begin, Rest1),
  naam(Naam,_NumS,_Datum,_Ticket, _, _),
  format(In1, "%s%s%s", Begin, Naam, Rest1), 
  substitueer(SpNo, In1, Uit, MetVervolg, Inh, InhUit), !.
substitueer(SpNo, In, Uit, MetVervolg, Inh, InhUit) :-
  vervang("%toernooi%", In, Begin, Rest1),
  naam(Naam,_NumS,_Datum,_Ticket, _, _),
  format(In1, "%s%s%s", Begin, Naam, Rest1), 
  substitueer(SpNo, In1, Uit, MetVervolg, Inh, InhUit), !.
substitueer(SpNo, In, Uit, MetVervolg, _Inh, InhUit) :-
  vervang("%geplandop%", In, Begin, Rest1),
  oproepenInvullen(SpNo, Tekst, Inh1, MetVervolg),
  format(In1, "%s%s%s", Begin, Tekst, Rest1), 
  substitueer(SpNo, In1, Uit, MetVervolg, Inh1, InhUit), !.
substitueer(SpNo, In, Uit, MetVervolg, Inh, InhUit) :-
  vervang("%deelname%", In, Begin, Rest1),
  deelname(SpNo, Tekst),
  format(In1, "%s%s%s", Begin, Tekst, Rest1), 
  substitueer(SpNo, In1, Uit, MetVervolg, Inh, InhUit), !.
substitueer(SpNo, In, Uit, MetVervolg, Inh, InhUit) :-
  vervang("%eigenformulier%", In, Begin, Rest1),
  eigenformulier(SpNo, Tekst),
  format(In1, "%s%s%s", Begin, Tekst, Rest1), 
  substitueer(SpNo, In1, Uit, MetVervolg, Inh, InhUit), !.
substitueer(SpNo, In, Uit,MetVervolg, Inh, InhUit) :-
  vervang("%uitgelootvoor%", In, Begin, Rest1),
  uitgelootvoor(SpNo, Tekst),
  format(In1, "%s%s%s", Begin, Tekst, Rest1), 
  substitueer(SpNo, In1, Uit, MetVervolg, Inh, InhUit), !.
substitueer(Sp, In, Uit, MetVervolg, Inh, InhUit) :-
  vervang("%voornaam%", In, Begin, Rest1),
  sp2(Sp,_Sex,Naam,_Telthuis,_Verh,_RatingE,_RatingD,_Betaald,_UitKant, _Club, _Adres, _Woonpl, _BondsNr, _ES, _DS, _Gebdatum, _Telwerk, _Telmobiel, _EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Res),
  naamscan(ja, b_False, Naam, _Naam1, Voornaam,_Landcode),
  format(In1, "%s%s%s", Begin, Voornaam, Rest1), 
  substitueer(Sp, In1, Uit, MetVervolg, Inh, InhUit), !.
substitueer(Sp, In, Uit, MetVervolg, Inh, InhUit) :-
  vervang("%bondsnummer%", In, Begin, Rest1),
  sp2(Sp,_Sex,_Naam,_Telthuis,_Verh,_RatingE,_RatingD,_Betaald,_UitKant, _Club, _Adres, _Woonpl, BondsNr, _ES, _DS, _Gebdatum, _Telwerk, _Telmobiel, _EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Res),
  %naamscan(ja, b_False, Naam, _Naam1, Voornaam,_Landcode),
  format(In1, "%s%s%s", Begin, BondsNr, Rest1), 
  substitueer(Sp, In1, Uit, MetVervolg, Inh, InhUit), !.
substitueer(Sp, In, Uit, MetVervolg, Inh, InhUit) :-  % !!!!!!!!!!!
  vervang("%inschrijfgeld%", In, Begin, Rest1),
  sp2(Sp,_Sex,_Naam,_Telthuis,_Verh,_RatingE,_RatingD,_Betaald,_UitKant, _Club, _Adres, _Woonpl, _BondsNr, _ES, _DS, _Gebdatum, _Telwerk, _Telmobiel, _EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Res),
  findall(Cat,sp_cat(Sp,Cat),Catten),
  not(Catten = []),
  sum(Catten,0.0,Som,"",_String),
  %Som > 0.0, 
  %Som1 = Som * 100,
  %Som1U = val(ulong, Som1),
  format(Som2, "EUR %-6.2f", Som),
  format(In1, "%s%s%s", Begin, Som2, Rest1), 
  substitueer(Sp, In1, Uit, MetVervolg, Inh, InhUit), !.
substitueer(Sp, In, Uit, MetVervolg, Inh, InhUit) :-
  vervang("%speler%", In, Begin, Rest1),
  sp2(Sp,_Sex,Naam,_Telthuis,_Verh,_RatingE,_RatingD,_Betaald,_UitKant, _Club, _Adres, _Woonpl, _BondsNr, _ES, _DS, _Gebdatum, _Telwerk, _Telmobiel, _EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Res),
  naamscan(ja, b_False, Naam, Naam1, _Voornaam,_Landcode),
  format(In1, "%s%s%s", Begin, Naam1, Rest1), 
  substitueer(Sp, In1, Uit, MetVervolg, Inh, InhUit), !.
substitueer(Sp, In, Uit, MetVervolg, Inh, InhUit) :-
  vervang("%uitkantoor%", In, Begin, Rest1),
  sp2(Sp,_Sex,_Naam,_Telthuis,_Verh,_RatingE,_RatingD,_Betaald,UitKant, _Club, _Adres, _Woonpl, _BondsNr, _ES, _DS, _Gebdatum, _Telwerk, _Telmobiel, _EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Res),
  Uitkant <> 0,
  tijd_kwart(_D1,U1,M1,Uitkant),
  helestr_int(0,MS1,M1),
  format(In1, "%sOp werkdagen beschikbaar vanaf: %2.%2%s", Begin, U1, MS1, Rest1), 
  substitueer(Sp, In1, Uit, MetVervolg, Inh, InhUit), !.
substitueer(Sp, In, Uit, MetVervolg, Inh, InhUit) :-
  vervang("%uitkantoor%", In, Begin, Rest1),
  sp2(Sp,_Sex,_Naam,_Telthuis,_Verh,_RatingE,_RatingD,_Betaald,UitKant, _Club, _Adres, _Woonpl, _BondsNr, _ES, _DS, _Gebdatum, _Telwerk, _Telmobiel, _EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Res),
  Uitkant = 0,
  format(In1, "%s%s", Begin, Rest1), 
  substitueer(Sp, In1, Uit, MetVervolg, Inh, InhUit), !.
substitueer(Sp, In, Uit, MetVervolg, Inh, InhUit) :-
  vervang("%verhinderingen%", In, Begin, Rest1),
  sp2(Sp,_Sex,_Naam,_Telthuis,Verh,_RatingE,_RatingD,_Betaald,_UitKant, _Club, _Adres, _Woonpl, _BondsNr, _ES, _DS, _Gebdatum, _Telwerk, _Telmobiel, _EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Res),
  findall(V, verhl_strl(Verh, V), VList),
  not(VList = []),
  list_to_string(["Verhinderd:"|VList], "\n  ", String),
  format(In1, "%s%s%s", Begin, String, Rest1), 
  substitueer(Sp, In1, Uit, MetVervolg, Inh, InhUit), !.
substitueer(Sp, In, Uit, MetVervolg, Inh, InhUit) :-
  vervang("%verhinderingen%", In, Begin, Rest1),
  sp2(Sp,_Sex,_Naam,_Telthuis,Verh,_RatingE,_RatingD,_Betaald,_UitKant, _Club, _Adres, _Woonpl, _BondsNr, _ES, _DS, _Gebdatum, _Telwerk, _Telmobiel, _EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Res),
  Verh = [],
  format(In1, "%s%s", Begin, Rest1), 
  substitueer(Sp, In1, Uit, MetVervolg, Inh, InhUit), !.
substitueer(Sp, In, Uit, MetVervolg, Inh, InhUit) :-
  vervang("%schemalink%", In, Begin, Rest1),
  naam(_Naam, _Nr, _Datum, WT, _Distr, _OokReserve),
  makeTicket(WT, WebTicket),
  %providerX(Prov, _, "", _),
  format(In1, "%shttp://www.toernooiklapper.nl/klapperx.php?tik=%s%s", Begin, WebTicket, Rest1),
  substitueer(Sp, In1, Uit, MetVervolg, Inh, InhUit), !.
substitueer(Sp, In, Uit, MetVervolg, Inh, InhUit) :-
  vervang("%toernooilink%", In, Begin, Rest1),
  %providerX(Prov, _, "", _),
  naam(_Naam, _Nr, _Datum, WT, _Distr, _OokReserve),
  makeTicket(WT, WebTicket),
  format(In1, "%shttps://www.toernooiklapper.nl/toernooi/%s/%s", Begin, WebTicket, Rest1),
  substitueer(Sp, In1, Uit, MetVervolg, Inh, InhUit), !.
substitueer(Sp, In, Uit, MetVervolg, Inh, InhUit) :-
  vervang("%bevestigingslink%", In, Begin, Rest1),
  format(In1, "%s#link#%s", Begin, Rest1),
  substitueer(Sp, In1, Uit, MetVervolg, Inh, InhUit), !.
substitueer(Sp, In, Uit, MetVervolg, Inh, InhUit) :-
  vervang("%bevestigen%", In, Begin, Rest1),
  format(In1, "%s*** [bevestigen!] ***%s", Begin, Rest1),
  substitueer(Sp, In1, Uit, MetVervolg, Inh, InhUit), !.
substitueer(Sp, In, Uit, MetVervolg, Inh, InhUit) :-
  vervang("%weeknummer%", In, Begin, Rest1),
  dag(Dag, _, _),
  dt_dateoffset_to_str(Dag,"%Wd",WS),	% 17.2.2012 1 of 2 digits
  format(In1, "%s%s%s", Begin, WS, Rest1),
  substitueer(Sp, In1, Uit, MetVervolg, Inh, InhUit), !.
substitueer(Sp, In, Uit, MetVervolg, Inh, InhUit) :-
  vervang("%pin%", In, Begin, Rest1),
  str_int(SpS, Sp),
  concat("000", SpS, SpS1),
  str_len(SpS1, L),
  P = L - 4,
  frontstr(P, SpS1, _, Pin),
  format(In1, "%s%s%s", Begin, Pin, Rest1), 
  substitueer(Sp, In1, Uit, MetVervolg, Inh, InhUit), !.
substitueer(_Sp, In, In, _, InhIn, InhIn).

individueelEmail(Onderwerp, Mededeling, MetVervolg, Test) :-
  trap(individueelEmail1(Onderwerp, Mededeling, MetVervolg, Test), _, emailfout()), !.
individueelEmail(_Onderwerp, _Mededeling, _MetVervolg, _Test).

individueelEmail1(Onderwerp, Mededeling, MetVervolg, echt) :-
  eAdres(Nr, Mapinameaddr, _, echt),
  substitueer(Nr, Onderwerp, Onderwerp1, MetVervolg, 1, _), 
  substitueer(Nr, Mededeling, Mededeling1, MetVervolg, 1, _),
  sMAPI_SendMailMessage([Mapinameaddr],[],[],Onderwerp1,Mededeling1),
  fail.
individueelEmail1(Onderwerp, Mededeling, MetVervolg, istest) :-
  eAdres(Nr, NameAddr, _, istest),
  substitueer(Nr, Onderwerp, Onderwerp1, MetVervolg, 1, _), 
  substitueer(Nr, Mededeling, Mededeling1, MetVervolg, 1, _),
  sMAPI_SendMailMessage([NameAddr],[],[],Onderwerp1,Mededeling1), !.
individueelEmail1(_Onderwerp, _Mededeling, _MetVervolg, _).

individueelEmailKlapper(Onderwerp, Mededeling, MetVervolg, Test) :-
  eAdres(Nr, mapinameaddr(Naam,Email), Bondsnummer, Test),
  substitueer(Nr, Mededeling, Mededeling1, MetVervolg, 1, _),
  substitueer(Nr, Onderwerp, Onderwerp1, MetVervolg, 1, _Inhoud),
  term_str(testdom, Test, TestStr),
  writef("\n[]%1s][%1s][%1s][%1s][%1s][%1u[]\n%s", Naam, Email, Onderwerp1, Bondsnummer, TestStr, Nr, Mededeling1),
  fail.
individueelEmailKlapper(_Onderwerp, _Mededeling, _MetVervolg, _).

oproepEmailKlapper(Onderwerp, Body, MetVervolg, Test) :-
  eAdres(Sp, mapinameaddr(Naam,Email), _Bondsnummer, Test),
  oproepenInvullen(Sp, _, Ret, MetVervolg),
  Ret = 1,			% dan pas invullen
  %sp2(Sp,_Sex,_Naam,_Telthuis,_Verh,_RatingE,_RatingD,_Betaald,_UitKant, _Club, _Adres, _Woonpl, _BondsNr, _ES, _DS, _Gebdatum, _Telwerk, _Telmobiel, _EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_Log,_Res),
  plantm(PlanTM),
  TotN1 = PlanTM + 1,
  bitleft(TotN1, 7, Tot), 
  wd_planned(Sp,WedstrijdList,Tot,MetVervolg),
  qsortw(WedstrijdList, WedstrijdList1),
  findall(Afz, afzeggenS(b_True, Sp, Tot, Afz, _), AfzL),
  findall(Code, afzeggenS(b_True, Sp, Tot, _, Code), CodeL),
  not(leeg(WedstrijdList, AfzL)),
  list_to_string(CodeL, ";", CodeString),
  wt2str(WedstrijdList1, TStrList),		% de codering
  list_to_string(TStrList, ";", TStr),
  %
  date(Year,Month,Day),
  time(Hours,Minutes,_Seconds,_Hundredths),
  dt_min_to_offset(Year, Month, Day, Hours, Minutes, Numr),
  dt_minoffset_to_str(Numr, "%YL-%MD-%DD", DatumTA),
  dt_minoffset_to_str(Numr, "%HH:%MM:00", TijdTA),
  %
  substitueer(Sp, Onderwerp, OnderwerpS, MetVervolg, 1, _),
  term_str(testdom, Test, TestStr),
  writef("[]%s}{%u}{%s}{%s}{%s}{%s}{%u}{%s}{%s}{%s[]\n", Naam, Sp, Email, OnderwerpS,TStr,CodeString,b_False,DatumTA,TijdTA,TestStr),
  %speeltijden1(WedstrijdList,Speeltdn),
  %qsortwt(Speeltdn,SpeeltdnS),
  %prt_speelt(SpeeltdnS,0,_,nee,1,1,[],OnderdelenU,0,_X),
  %list_to_string(OnderdelenU, "\n", Ost),
  substitueer(Sp, Body, BodyS, MetVervolg, 1, _),
  write(BodyS, "\n"),   % deze is echt
  fail.
oproepEmailKlapper(_Onderwerp, _Mededeling, _MetVervolg, _).

wt2str([w(No,Wc,Tijdstip,_,LR,Tgnst,b_False,_)|Rest], [Str|Uit]) :-
  term_str(tgnst, Tgnst, TStr),
  format(Str, "w2,%u,%u,%u,%u,%s", No, Wc, LR, Tijdstip,TStr),
  wt2str(Rest, Uit),
  !.
wt2str([_|Rest], Uit) :-
  wt2str(Rest, Uit), !.
wt2str(_, []).

maakTeam(s(SpNo)) :-
  T = s(SpNO),
  sp2(SpNo,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  naamscan(ja,b_True, Naam, NaamU,_,_), !,
  write("<TEAM tgnst=\"",T,"\" ","nr1=\"",SpNo,"\" nr2=\"0\" />\n"),
  write("<SPELER nr=\"",SpNo,"\" naam=\"",NaamU,"\" />\n").
maakTeam(p(SpNo1,SpNo2)) :-
  T = p(SpNo1,SpNo2),
  sp2(SpNo1,_,Naam1,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  naamscan(ja,b_True, Naam1, Naam1U,_,_),
  xml_xlate(Naam1U, Naam1UX),
  sp2(SpNo2,_,Naam2,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_),
  naamscan(ja,b_True, Naam2, Naam2U,_,_), !,
  xml_xlate(Naam2U, Naam2UX),
  write("<TEAM tgnst=\"",T,"\" ","nr1=\"",SpNo1,"\" nr2=\"",SpNo2,"\" />\n"),
  write("<SPELER nr=\"",SpNo1,"\" naam=\"",Naam1UX,"\" />\n"),
  write("<SPELER nr=\"",SpNo2,"\" naam=\"",Naam2UX,"\" />\n").

uitslagEnableUpload() :-
  not(uitslagenable(_)), !.
uitslagEnableUpload() :-
  get_TempDir(TempDir, _TempFileXX),
  %getfolder(TempDir,Dir1,_,_),
  filenamepath(FF, TempDir, "uitslagenable.xml"),
  openwrite(work, FF),
  writedevice(work),
  maakUitslagEnableUploadXML(),
  tracezelf("uitslagenable xml klaar"),
  closefile(work),
  providerx(Provider, _, "", _),
  format(URL, "http://%s/uitslagenable.php", Provider),
  X = aanmeldenvars(),
  filenamepath(UETempfile, TempDir, "uitslagenable.txt"),
  uitslagenable(Pin),
  pouletelling(Telling),
  str_int(TellingS, Telling),
  _ReturnUE = fileupload(URL, FF, UETempFile, ["pin", Pin, "op", "aan", "puntentelling", TellingS | X]),   
  file_str(UETempFile, StrUE),
  write(StrUE), !.
uitslagEnableUpload().

voegdatumtoe(Fact, _FactOut) :-
  Fact = w3(Num,Cat,WinNo,P1, P2, _, _),
  w3(Num,Cat,WinNo,P1, P2, _, _), !, % fact bestaat al dus doe niets
  fail.
voegdatumtoe(Fact, _FactOut) :-
  Fact = wd(Num,Cat,_S1,_S2, Uitslag, _, _),
  wd(Num,Cat,_,_,Uitslag,_,_), !, % fact bestaat al dus doen niets
  fail.
voegdatumtoe(Fact, FactOut) :-
  Fact = w3(Num,Cat,WinNo,P1, P2, _, _),
  w2(Num,Cat,DO,_,_,_Fix,_Tijd2,_Bn), 
  %date(Year,Month,Day),
  %dt_date_to_offset(Year,Month,Day, DO),
  %bitleft(DO, 7, DOx),
  FactOut = w3(Num,Cat,WinNo,P1, P2, geen, DO), !.
voegdatumtoe(Fact, FactOut) :-
  Fact = w3(Num,Cat,WinNo,P1, P2, _, _),
  date(Year,Month,Day),
  dt_date_to_offset(Year,Month,Day, DO),
  bitleft(DO, 7, DOx),
  FactOut = w3(Num,Cat,WinNo,P1, P2, geen, DOx), !.
voegdatumtoe(Fact, Fact).

removeplan(Num, Cat) :-
  ws(_Num,Cat,T,Tijd),		% er was eens een planning
  logf('R', ws(Num,Cat,T,Tijd), ja),
  fail.
removeplan(Num, Cat) :-
  w2(Num,Cat,Tijd,G1,G2,Fix,Tijd2,Bn), 
  logf('R', w2(Num, Cat,Tijd,G1,G2,Fix,Tijd2,Bn), ja),
  fail.
removeplan(Num,Cat) :-
  spelersblijven(Num,Cat,b_False),
  logclose().

uitslagendownload(b_true) :-
  not(uitslagenable(_)),
  dlg_MessageBox( "Uitslagen invoeren", "Om uitslagen ter plekke in te kunnen toestaan moet je\n\"Uitslaginvoer ter plekke\" aanvinken bij Toernooi, Beleid!\n(en eventueel de schema's + planning verversen op het web)", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  !.
uitslagendownload(b_false) :-
  not(uitslagenable(_)), !.
uitslagendownload(_) :-
  assert(eaantal(0)),
  providerX(Prov, _, "", _),
  format(URL,"http://%s/uitslaginvoer.php", Prov), 
  get_TempDir(_, TempFile),
  deletefile(TempFile),
  AanmeldenVars = aanmeldenvars(),
  _Return = downloadData(URL, TempFile, ["op", "uitslagen"| AanmeldenVars], _HttpResponsecode, 0),
  %stopdbrefresh(),
  %file_str(TempFile, String),
  %dlg_Note("Test",String),
  openread(work, TempFile),
  readdevice(work),
  repeat_f(work),
  readln(Line),
  trap(term_str(dbasedom, Term, Line),_,true),
  voegdatumtoe(Term, TermPlus),  % + filter al reeds gedownloade
  logf('Z',TermPlus, ja),
  TermPlus = w3(Num,Cat,_WinNo,_, _, _, _),
  eaantal(N),
  N1 = N + 1,
  assert(eaantal(N1)),
  removeplan(Num, Cat),
  uitslag_volgExtern(Num, Cat),
  fail.
uitslagendownload(_) :-
  logclose(),
  closefile(work),
  comline(LineBuffer),
  TaskWin = vpi_GetTaskWin(),
  toolbar_SetValue(TaskWin, idt_toolbarmsg, text_value("")),
  upper_lower(LineBuffer, LBL),
  searchstring(LBL, "/x", _),		% in TEST
  eaantal(N),
  %N > 0,
  format(Msg, "%d uitslag(en) gedownload.", N),
  vpi_Alarm(mesbox_iconinformation),
  toolbar_SetValue(TaskWin, idt_toolbarmsg, text_value(Msg)),
  fail.    
uitslagendownload(b_true) :-
  eaantal(N),
  N > 0,
  vpi_Alarm(mesbox_iconinformation),
  format(Msg, "%d uitslag(en) gedownload!", N),
  TaskWin = vpi_GetTaskWin(),
  toolbar_SetValue(TaskWin, idt_toolbarmsg, text_value(Msg)),
  fail.  
uitslagendownload(_) :-
  eaantal(0), !.
uitslagendownload(_) :-
  doUpdateVensters().


clauses

/*maakUitslagEnableUploadXML() :-
  comline(LineBuffer),
  upper_lower(LineBuffer, LBL),
  searchstring(LBL, "/x", _),		% in TEST
  uitslagenableE(No, Wc, Tijdstip, T1, T2),
  writedevice(X),
  writedevice(screen),
  term_str(operationeel,uitslagenableE(No, Wc, Tijdstip, T1, T2),String),
  nl,write(String),
  writedevice(X),
  fail. */    
maakUitslagEnableUploadXML() :-
  write("<?xml version=\"1.0\" encoding=\"iso-8859-1\" ?>\n"),
  write("<!DOCTYPE UITSLAGENINVOER >\n"),
  write("<UITSLAGENINVOER>\n"),
  uitslagenableE(_No, _Wc, _Tijdstip, s(SpNo1), s(SpNo2)),
  maakTeam(s(SpNo1)),
  maakTeam(s(SpNo2)),
  fail.
maakUitslagEnableUploadXML() :-
  uitslagenableE(_No, _Wc, _Tijdstip, p(Sp1,Sp2), p(Sp3, Sp4)),
  maakTeam(p(Sp1,Sp2)),
  maakTeam(p(Sp3,Sp4)),
  fail.
maakUitslagEnableUploadXML() :-
  retract(uitslagenableE(No, Wc, Tijdstip, _, _)),
  tijd_kwart(Dag, Uur, Min, Tijdstip),
  helestr_int(0, UStr, Uur),
  helestr_int(0, MStr, Min),
  dt_dateoffset_to_str(Dag, "%YL-%MD-%DD", Str),
  format(TijdstipSort, "%s %s:%s", Str, UStr, MStr),
  %dt_timeoffset_to_str(Tijdstip, "%DD/%MD %HH:%MM", TijdstipStr), 
  %dt_timeoffset_to_str(Tijdstip, "%YL-%MM-%DD %HH:%MM", TijdstipSort), % naar mysql tijdstip
  wd(No,Wc,S1,S2,_X,Y,Z),
  Term = wd(No,Wc,S1,S2,"&uitslag",Y,Z),  % (tussen-) uitslag wordt vervangen
  wc(Wc, _, _, _Onderdeel, _, Short, _, _, _, _, _, _, _, _, _),
  %wc(Wc, _Onderdeel,_,Short,_,_,_),
  pos(Wc, _, _, Type),
  term_str(dbasedom,Term,Code),
  xml_xlate(Code, Code1),
  write("<PARTIJ onderdeel=\"",Short,"\" nummer=\"",No,"\" categorie=\"",Wc,"\" tgnst1=\"",S1,"\" tgnst2=\"",S2,"\" tijdstipsort=\"",
     TijdstipSort,"\" code=\"",Code1,"\" type=\"", Type, "\"/>\n"),
  fail.
maakUitslagEnableUploadXML() :-
  write("</UITSLAGENINVOER>\n").

verzondenOK(String) :-
  not(searchstring(String, "OKcompleet", _)),
  dlg_MessageBox( "Oproepen", "Niet alle oproepen zijn uitgegaan.\nDe server kan traag zijn.\nProbeer nog een keer (desnoods later)..",
    mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  !.
verzondenOK(_).

zetEmailadresZelf(Email, ENaam) :-
  %emailadresZelfP(Email1, ENaam1),
  api_getComputername(CN),
  upper_lower(PC, CN),
  logf('Z', emailadresZelf(PC, Email, ENaam), nee),  !.

algemeelP(MeelType, Per, Onderwerp, Template, MetVervolg) :-
  algemeelX(Meeltype, Per, Onderwerp, Template, MetVervolg), !.
algemeelP(oproep, direct, "Aan %speler% van %toernooinaam%", Txt, b_True) :-
  standaardtekstV(oproep, _, _, Txt),  !.
algemeelP(bulk, direct, "%toernooinaam%", "\nSelecteer een passend Model met de knop Modellen...", b_True) :- !.
algemeelP(enkel, direct, "%toernooinaam% aan %speler% ", "Beste %voornaam%,\nje gegevens zijn als volgt:\n%deelname%\n%verhinderingen%\n%uitkantoor%", b_True).	% indivimeel
algemeelP(sms, direct, "%toernooinaam% aan %speler% ", "%voornaam%, je staat gepland voor\n%geplandop%", b_True).
algemeelP(smsbulk, direct, "%toernooinaam% aan %speler% ", "Beste %voornaam%, het %toernooinaam% toernooi\nvindt plaats op .....", b_True).

standaardtekstV(bulk, ID, txt(ID,"uitnodiging tot inschrijven",0,b_True,0,[]), Tekst) :-
  ID = standaardteksten,
  Tekst = "Beste %voornaam%,\nDit jaar begint het %toernooinaam% op ....\nJe kunt je opgeven via onderstaande link:\n%toernooilink%\nNB je bondsnummer is %bondsnummer%\n\nMet vriendelijke groeten,\nde toernooileiding.".
standaardtekstV(bulk, ID, txt(ID,"herinnering voor deelname",0,b_True,0,[]), Tekst) :-
  ID = standaardteksten + 1,
  Tekst1 = "//\n//NB! Zelf eerst de juiste selectie maken van de spelers die nog niet opgegeven hebben!\n//\nBeste %voornaam%,\nVolgens onze gegevens heb je je nog niet opgegeven voor %toernooinaam%.\n",
  Tekst2 = "Je kunt je opgeven via onderstaande link:\n%toernooilink%\n\nMet vriendelijke groeten,\nde toernooileiding.",
  concat(Tekst1, Tekst2, Tekst).
standaardtekstV(bulk, ID, txt(ID,"opgaven en verhinderingen checken",0,b_True,0,[]), Tekst) :-
  ID = standaardteksten + 2,
  format(Tekst, "Beste %%voornaam%%,\n%%deelname%%\n%%verhinderingen%%\n%%uitkantoor%%\nKijk deze gegevens even na svp!\n%%eigenformulier%%\nMet vriendelijke groeten,\nde toernooileiding.").
standaardtekstV(bulk, ID, txt(ID,"herinnering om te spelen",0,b_True,0,[]), Tekst) :-
  ID = standaardteksten + 3,
  Tekst = "Beste %voornaam%,\n\nHierbij even een herinnering dat je morgen moet spelen en wel:\n%geplandop%\nZie ook:\n%toernooilink%\nMet vriendelijke groeten\nde toernooileiding.".
standaardtekstV(bulk, ID, txt(ID,"stuur pincode",0,b_True,0,[]), Tekst) :-
  ID = standaardteksten + 4,
  Tekst = "Beste %voornaam%,\n\nHierbij even een herinnering dat je morgen moet spelen en wel:\n%geplandop%\nZie ook:\n%toernooilink%\nSvp zelf de uitslag invullen, je pincode is: %pin%\nMet vriendelijke groeten\nde toernooileiding.".
standaardtekstV(bulk, ID, txt(ID,"bedankt en tot volgend jaar",0,b_True,0,[]), Tekst) :-
  ID = standaardteksten + 5,
  Tekst = "Beste %voornaam%,\nWel bedankt voor je deelname!\nJe kunt hier de resultaten zien\n%toernooilink%\nGraag tot volgend jaar!\nMet vriendelijke groeten,\nde toernooileiding.".
standaardtekstV(oproep, ID, txt(ID,"oproepen",0,b_True,0,[]), Tekst) :-
  ID = standaardteksten + 6,
  S1 = "Beste %voornaam%, je staat gepland voor:\n%geplandop%\n%bevestigen%\n// NB toernooilink is automatisch\n",
  S2 = "Svp eventuele partner waarschuwen!\nKijk ook steeds op de schema's!\n%verhinderingen%\n%uitkantoor%\nMvg.\nde toernooileiding, tel ......",
  concat(S1, S2, Tekst).
standaardtekstV(enkel, ID, txt(ID,"individuele email",0,b_True,0,[]), Tekst) :-
  ID = standaardteksten + 7,
  Tekst = "Beste %voornaam%,\nje gegevens zijn als volgt:\n%deelname%\n%verhinderingen%\n%uitkantoor%".
standaardtekstV(sms, ID, txt(ID,"oproep sms",0,b_True,0,[]), Tekst) :-
  ID = standaardteksten + 8,
  Tekst = "%voornaam%, je staat gepland voor\n%geplandop%".
standaardtekstV(smsbulk, ID, txt(ID,"bulk sms",0,b_True,0,[]), Tekst) :-
  ID = standaardteksten + 9,
  Tekst = "Beste %voornaam%, het %toernooinaam% toernooi\nvindt plaats op .....\n// vul zelf tekst in/aan!".
standaardtekstV(bulk, ID, txt(ID, "aankondiging incasso",0,b_True,0,[]), Tekst) :-
  ID = standaardteksten + 10,
  S1 = "//NB! Svp NIET het euroteken gebruiken!\n//\n//Aanbevolen speciale selectie: Spelers die \"nog ergens in zitten\"\n//en dan Deelselectie: \"die een machtiging hebben afgegeven\".\n//",
  S2 = "Beste %voornaam%,\nbinnenkort gaan we het inschrijfgeld per incasso innen\nuiteraard alleen voor de onderdelen waar je voor ingeloot bent.",
  S3 = "Het verschuldigde bedrag is: %inschrijfgeld%.\nTer informatie het tarief is:\n EURO .. voor een enkel\n EURO .. voor een dubbel",
  S4 = "Met vriendelijke groeten, de toernooileiding",
  format(Tekst, "%s\n%s\n%s\n\n%s", S1, S2, S3, S4).

enableModel(oproep, b_True, b_False, b_False,b_False, b_False).
enableModel(bulk, b_False, b_True, b_False,b_False, b_False).
enableModel(enkel, b_False, b_False, b_True,b_False, b_False).
enableModel(sms, b_False,b_False,b_False,b_True, b_False).
enableModel(smsbulk, b_False,b_False,b_False, b_False,b_True).

standaardtekst(ID, Item, String) :-
  meeltypeF(MeelType),
  bound(ID),
  standaardTekstV(MeelType, ID, Item, String), !.
standaardtekst(0, txt(0,"voorbeelden",0,b_True,0,[
  txt(0,"oproep",0,D1,0,L1),
  txt(0,"bulk",0,D2,0,L2),
  txt(0,"individueel",0,D3,0,L3),
  txt(0,"sms",0,D4,0,L4),
  txt(0,"smsbulk",0,D5,0,L5) ]), "") :-
  meelTypeF(MeelType),
  enableModel(MeelType, D1, D2, D3, D4, D5),
  findall(El, standaardTekstV(oproep, _, El, _), L1),
  findall(E2, standaardTekstV(bulk, _, E2, _), L2),
  findall(E3, standaardTekstV(enkel, _, E3, _), L3),
  findall(E4, standaardTekstV(sms, _, E4, _), L4),
  findall(E5, standaardTekstV(smsbulk, _, E5, _), L5).
standaardtekst(ID, txt(ID,"undo model",0,Enabled,0,[]), String) :-
  bound(ID),
  ID = standaardteksten + 34,
  tijdelijkModelP(String, Enabled),
  retractall(tijdelijkModel(_)), !.
standaardtekst(ID, txt(ID,"undo model",0,Enabled,0,[]), String) :-
  ID = standaardteksten + 34,
  tijdelijkModelP(String, Enabled).
  %retractall(tijdelijkModel(_)).
standaardtekst(ID, txt(ID,"annuleren",0,b_True,0,[]), "") :-
  ID = standaardteksten + 35.

tijdelijkModelP(String, 1) :-
  tijdelijkModel(String), !.
tijdelijkModelP("", 0).

invul(txt(ID,"%voornaam%",0,b_True,0,[])) :-
  ID = standaardteksten + 20.
invul(txt(ID,"%uitgelootvoor%",0,b_True,0,[])) :-
  ID = standaardteksten+21.
invul(txt(ID,"%deelname%",0,b_True,0,[])) :-
  ID = standaardteksten+22.
invul(txt(ID,"%verhinderingen%",0,b_True,0,[])) :-
  ID = standaardteksten+23.
invul(txt(ID,"%pin%",0,b_True,0,[])) :-
  ID = standaardteksten+24.
invul(txt(ID,"%uitkantoor%",0,b_True,0,[])) :-
  ID = standaardteksten+25.
invul(txt(ID,"%speler%",0,b_True,0,[])) :- 
  ID = standaardteksten+26.
invul(txt(ID,"%geplandop%",0,b_True,0,[])) :-
  ID = standaardteksten+27.
invul(txt(ID,"%toernooinaam%",0,b_True,0,[])) :-
  ID = standaardteksten+28.
invul(txt(ID,"%weeknummer%",0,b_True,0,[])) :-
  ID = standaardteksten+29.
invul(separator).
invul(txt(ID,"%toernooilink%",0,b_True,0,[])) :-
  ID = standaardteksten+30.
invul(txt(ID,"%eigenformulier%",0,b_True,0,[])) :-
  ID = standaardteksten+31.
invul(txt(ID,"%bevestigen%",0,b_True,0,[])) :-
  ID = standaardteksten+32.
invul(txt(ID,"Eigen link",0,b_True,0,[])) :-
  ID = standaardteksten+33.
invul(txt(ID,"%bondsnummer%",0,b_True,0,[])) :-
  ID = standaardteksten+34.
invul(txt(ID,"%inschrijfgeld%",0,b_True,0,[])) :-
  ID = standaardteksten+35.

expandeerInTeVullen("%toernooilink%", "Op de volgende link kun je inschrijven en het toernooi volgen:\n%toernooilink%\n// \n// Link geldt zowel voor inschrijven als de schema's daarna!\n// \n") :- !.
expandeerInTeVullen("%bevestigen%","%bevestigen%\n// Nieuw, NIET tegelijk met de bevestigingslink gebruiken!\n") :- !.
expandeerInTeVullen("%weeknummer%", " week %weeknummer%") :- !.
expandeerInTeVullen("Eigen link", "Zie de volgende link:\nhttp://www.enelink.nl\n// NB zorg zelf voor een juiste verwijzing en TEST de email!\n") :- !.
expandeerInteVullen(X, X).

setAlgemeelX(Vehikel, Onderwerp, Template, VervolgWeds) :-	% ook onderwerp en body
  meeltypeF(MeelType),
  not(algemeelX(Meeltype, Vehikel, Onderwerp, Template, Vervolgweds)),
  logf('Z', algemeelX(Meeltype, Vehikel, Onderwerp, Template, Vervolgweds), nee), !.
setAlgemeelX(_, _Onderwerp, _Mede, _).

berichtvoorbeelden(_Win) :-
  UWin = win_GetCtlHandle(_Win, idc_undo),
  win_SetState(UWin, [wsf_disabled]),
  CWin = win_GetCtlHandle(_Win, idc_custom),
  b_True = edit_IsModified(CWin),
  win_SetState(UWin, [wsf_enabled]),
  fail.
berichtvoorbeelden(_Win) :-
  meeltypeF(MeelType),
  getCurrentModel(_Win, Kop, Body, MetVervolg),
  personaliseer(MeelType, _Win, Body),
  eenAdres(Nr),
  substitueer(Nr, Kop, Kop1, MetVervolg, 1, _),
  findall(Nrx, eAdres(Nrx, _, _, echt),Nrs),
  eenbericht(1, Nrs, Body, MetVervolg,List),
  list_to_string(List, " ______________________________________________________________ \n", Uit),
  RWin = win_GetCtlHandle(_Win, idc_resultaat),
  edit_pastestr(RWin, Uit),
  OUWin = win_GetCtlHandle(_Win, idc_onderwerp1),
  win_SetText(OUWin, Kop1),
  MWin = win_GetCtlHandle(_Win, idc_custom),
  Text = edit_getText(MWin),
  not(searchstring(Text,"%geplandop%",_)),
  GBBT = win_GetCtlHandle(_Win, idc_groupbox2),
  win_SetState(GBBT, [wsf_disabled]),
  GBPT = win_GetCtlHandle(_Win, idct_planning_tm),
  win_SetState(GBPT, [wsf_disabled]),
  GBP = win_GetCtlHandle(_Win, idc_plan_tm),
  win_SetState(GBP, [wsf_disabled]),
  GBMV = win_GetCtlHandle(_Win, idc_met_vervolg),
  win_SetState(GBMV, [wsf_disabled]), !.
berichtvoorbeelden(_Win) :-
  MWin = win_GetCtlHandle(_Win, idc_custom),
  Text = edit_getText(MWin),
  searchstring(Text,"%geplandop%",_),
  GBBT = win_GetCtlHandle(_Win, idc_groupbox2),
  win_SetState(GBBT, [wsf_enabled]),
  GBPT = win_GetCtlHandle(_Win, idct_planning_tm),
  win_SetState(GBPT, [wsf_enabled]),
  GBP = win_GetCtlHandle(_Win, idc_plan_tm),
  win_SetState(GBP, [wsf_enabled]),
  GBMV = win_GetCtlHandle(_Win, idc_met_vervolg),
  win_SetState(GBMV, [wsf_enabled]), !.
berichtvoorbeelden(_Win).
  %GBMB = win_GetCtlHandle(Win, idc_met_bevestiging),	% nog niet gebruikt

eenbericht(_, [], _Body, _, []) :- !.
eenbericht(30, _, _Body, _, ["Max. 30 voorbeelden.."]) :- !.
eenbericht(N, [Nr|Rest], Body, MetVervolg, [Tekst|Uit]) :-
  eAdres(Nr, mapinameaddr(Naam,Email), _, echt),
  substitueer(Nr, Body, Uit1, MetVervolg, 1, _),
  format(Tekst, "Aan: %s -- %s\n%s", Naam, Email, Uit1),
  N1 = N + 1, !,
  eenbericht(N1, Rest, Body, MetVervolg, Uit).
eenbericht(_, _, _Body, _, []) :- !. % procedure ....

setEmailWinTimer() :-		% global
  retract(emailWinTimer(TimerId)),
  timer_Kill(TimerId),
  fail.
setEmailWinTimer() :-
  emailWin(Win),
  NewTimerId = timer_Set(Win, 1000),
  assert(emailWinTimer(NewTimerId)), !.	
setEmailWinTimer().

getCurrentModel(_Win, Kop, Body, MetVervolg) :-
  OndWin = win_GetCtlHandle(_Win, idc_onderwerp),
  Kop = win_GetText(OndWin),
  MWin = win_GetCtlHandle(_Win, idc_custom),
  Body = edit_GetText(MWin),
  VWin = win_GetCtlHandle(_Win, idc_met_vervolg),
  MetVervolg = win_IsChecked(VWin).

personaliseer(oproep, Win, _Body) :-
  %H = win_GetCtlHandle(Win, idc_aantekenen_op_het_spelerformulier),
  %win_SetState(H, [wsf_Invisible]),
  %win_check(H, b_False),
  P = win_GetCtlHandle(Win, idc_per),
  win_SetState(P, [wsf_Invisible]),
  D = win_GetCtlHandle(Win, idc_direct),
  win_SetState(D, [wsf_Invisible]),
  O = win_GetCtlHandle(Win, idc_outlook),
  win_SetState(O, [wsf_Invisible]), !.
personaliseer(sms, Win, _Body) :-
  P = win_GetCtlHandle(Win, idc_per),
  win_SetState(P, [wsf_Invisible]),
  D = win_GetCtlHandle(Win, idc_direct),
  win_SetState(D, [wsf_Invisible]),
  O = win_GetCtlHandle(Win, idc_outlook),
  win_SetState(O, [wsf_Invisible]), !.
personaliseer(smsbulk, Win, _Body) :-
  P = win_GetCtlHandle(Win, idc_per),
  win_SetState(P, [wsf_Invisible]),
  D = win_GetCtlHandle(Win, idc_direct),
  win_SetState(D, [wsf_Invisible]),
  O = win_GetCtlHandle(Win, idc_outlook),
  win_SetState(O, [wsf_Invisible]), !.
personaliseer(_Bulk, _Win, _).
%	% oproep;bulk;enkel
%  H = win_GetCtlHandle(Win, idc_aantekenen_op_het_spelerformulier),
%  iAantekenen(Aantekenen),
%  win_check(H, Aantekenen).

getPer(Win, direct) :-
  PWin = win_GetCtlHandle(Win, idc_direct),
  b_True = win_IsChecked(PWin), !.
getPer(_Win, outlook).

vehikel2rb(_Win, direct, idc_direct) :- !.
vehikel2rb(_Win, outlook, idc_outlook) :- 
  DWin = win_GetCtlHandle(_Win, idc_afzender),
  win_SetState(DWin, [wsf_disabled]).

emailfout() :-
  dlg_MessageBox( "Emailfout", "Is het emailadres in orde?\nHeeft Outlook (Expres) de juiste opties?", mesbox_iconerror, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ).

/*rb2vehikel(idc_direct, direct) :- !.
rb2vehikel(_, outlook).*/

close_emailwin() :-
  emailWin(Win),
  win_Destroy(Win), !.
close_emailwin().
/*
berichtAfm(Body, MetVervolg, Afm, Max) :-
  eAdres(Nr, mapinameaddr(_Naam,_Email), _, echt),
  substitueer(Nr, Body, Uit, MetVervolg, 1, _),
  str_len(Uit, Afm),
  Afm > Max, !.
*/
maxAfm(sms, 160) :- !.
maxAfm(smsbulk, 160) :- !.
maxAfm(_, 1500).

emailprovider("gmail").
emailprovider("hotmail").
emailprovider("quicknet").

versturenNotOK(_Win) :-
  MWin = win_GetCtlHandle(_Win, idc_custom),
  Med = edit_GetText(MWin),
  not(fronttoken(Med, _, _)),
  dlg_MessageBox( "Email", "Vul een mededeling/model in!", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  win_SetFocus(MWin), !.
versturenNotOK(_Win) :-
  MWin = win_GetCtlHandle(_Win, idc_custom),
  win_Setfocus(MWin),
  b_True = edit_SearchStr(MWin, "..."),
  dlg_MessageBox( "Email", "Kijk svp de stippeltjes na in het MODEL!", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  win_SetFocus(MWin), !.
versturenNotOK(_Win) :-
  OWin = win_GetCtlHandle(_Win, idc_onderwerp),
  Ond = win_GetText(OWin),
  not(meeltypeF(sms)),
  not(meeltypeF(smsbulk)),
  not(fronttoken(Ond, _, _)),
  dlg_MessageBox( "Email", "Vul een onderwerp in!", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), 
  win_SetFocus(OWin), !.
versturenNotOK(_Win) :-
  meeltypeF(MT),
  maxAfm(MT, Max),
  getCurrentModel(_Win, _Kop, Body, _MetVervolg),
  str_len(Body, Afm),
  Afm > Max,
  % berichtAfm(Body, MetVervolg, Afm, Max),
  format(Msg, "het gebruikte model heeft teveel tekens (%u)\nHet maximum is %u. Maak de tekst korter!",Afm, Max), 
  dlg_MessageBox( "SMS", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), !.
versturenNotOK(_Win) :-
  getPer(_Win, Per),
  Per = direct,
  MB = getmailbox(_Win, echt),
  MB = mapinameaddr(_, EAdr),
  emailadresFout(EAdr),
  dlg_MessageBox( "Email", "Het afzender emailadres is niet geldig!\nSvp zorgvuldig invullen!\nZie ook Help.", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), !.
versturenNotOK(_Win) :-
  getPer(_Win, Per),
  Per = direct,
  MB = getmailbox(_Win, echt),
  MB = mapinameaddr(_, EAdr),
  emailprovider(EProv),
  upper_lower(Eadr, EadrL),
  searchstring(EAdrL,EProv,_FoundPos),
  1 <> dlg_MessageBox( "Email", "Het afzender emailadres-domein kan beter niet van een grote emailprovider zijn!\nDan kan de mail in de spam-box terecht komen!\nToch doorgaan?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ), !.
versturenNotOK(_Win) :-
  not(meeltypeF(oproep)),
  MWin = win_GetCtlHandle(_Win, idc_custom),
  b_True = edit_SearchStr(MWin, "#link#"),
  dlg_MessageBox( "Email", "Een #link# geldt alleen bij oproepemail.\nStuur via oproepen/afzeggen (F6).", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), 
  win_SetFocus(MWin), !.
versturenNotOK(_Win) :-
  meeltypeF(oproep),
  MWin = win_GetCtlHandle(_Win, idc_custom),
  b_False = edit_SearchStr(MWin, "%geplandop%"),
  dlg_MessageBox( "Email", "Oproepemail MOET %geplandop% bevatten.", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), 
  win_SetFocus(MWin), !.
versturenNotOK(_Win) :-
  not(meeltypeF(oproep)),
  MWin = win_GetCtlHandle(_Win, idc_custom),
  b_True = edit_SearchStr(MWin, "%bevestigingslink%"),
  dlg_MessageBox( "Email", "Een #link# geldt alleen bij oproepemail.", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), 
  win_SetFocus(MWin), !.
versturenNotOK(_Win) :-
  not(meeltypeF(oproep)),
  MWin = win_GetCtlHandle(_Win, idc_custom),
  b_True = edit_SearchStr(MWin, "%bevestigen%"),
  dlg_MessageBox( "Email", "Bevestigen geldt alleen bij oproepemail.", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), 
  win_SetFocus(MWin), !.
versturenNotOK(_Win) :-
  meeltypeF(sms),
  not(tegoedOK(_Win)), !.
versturenNotOK(_Win) :-
  meeltypeF(smsbulk),
  not(tegoedOK(_Win)), !.

getmailbox(_Win, echt, MB) :-
  meeltypeF(sms),
  emailsmsP(_Prov, _, MB), !.
getmailbox(_Win, echt, MB) :-
  meeltypeF(smsbulk),
  emailsmsP(_Prov, _, MB), !.
getmailbox(Win, echt, mapinameaddr(ENaam,Mailbox)) :-
  MBWin = win_GetCtlHandle(Win, idc_afzender),
  MailBox = win_GetText(MBWin),
  ENWin = win_GetCtlHandle(Win, idc_email_afzendernaam),
  ENaam = win_GetText(ENWin), !.
getmailbox(_, _, mapinameaddr(Naam, Mailbox)) :-
  eAdres(_, mapinameAddr(Naam, Mailbox), _, echt), !.  % de eerste

skipRegel1(In, Uit) :-
  searchstring(In,"\n",FoundPos),
  Fp1 = FoundPos,
  frontstr(Fp1,In,Head,Uit),
  searchstring(Head,"Aan",_), !.
skipRegel1(Uit, Uit).




versturen(_Win, _) :-				% save het emailadres
  not(meeltypeF(sms)),
  not(meeltypeF(smsbulk)),
  getPer(_Win, Per),
  Per = direct,
  MBWin = win_GetCtlHandle(_Win, idc_afzender),
  MB = win_GetText(MBWin),
  ENWin = win_GetCtlHandle(_Win, idc_email_afzendernaam),
  EN = win_GetText(ENWin),
  zetEmailadresZelf(MB, EN),
  fail.
versturen(_Win, echt) :-
  getPer(_Win, Veh),				% save het model
  getCurrentModel(_Win, Kop, Body, MetVervolg),
  setAlgemeelX(Veh, Kop, Body, MetVervolg),
  fail.
versturen(_Win, Test) :-				% bulk naar outlook
  assert(gedaan(b_False)),	% !!!!!
  getPer(_Win, Per),
  Per = outlook,
  meeltypeF(bulk),
  getCurrentModel(_Win, Ond, Med, MetVervolg),
  disk(Current),
  aantalAdressen(Aantal),
  format(Msg, "Naar %u adres(sen), aparte emails voor iedereen.\nKomt in de uitbak van je eigen emailprogramma.", Aantal),
  1 = dlg_MessageBox( "Email", Msg , mesbox_iconinformation, mesbox_buttonsokcancel, mesbox_defaultfirst, mesbox_suspendapplication ), 
  individueelEmail(Ond, Med, MetVervolg, Test),
  disk(Current),
  fail.
versturen(_Win, Test) :-				% bulk direct
  getPer(_Win, Per),
  Per = direct,
  meeltypeF(bulk),
  getCurrentModel(_Win, Ond, Med, MetVervolg),
  get_TempDir(_, TempFile),
  closefile(work),
  openwrite(work,TempFile),
  writedevice(work),
  MB = getmailbox(_Win, Test),
  MB = mapinameaddr(ENaam, MailBox),
  write("[]",MailBox,"[]"),
  individueelEmailKlapper(Ond, Med, MetVervolg, Test),
  closefile(work),
  uploadalg1("oproepcurl3.php", "individueel1", [TempFile], "Opsturen van e-mails", b_False,99,Gedaan, ["afzendernaam", ENaam]), % bestand bestaat al
  aantalAdressen(Aantal),
  updatetegoed(Aantal),
  assert(gedaan(Gedaan)),
  fail.
versturen(_Win, Test) :-				% sms bulk direct
  %getPer(_Win, Per),
  %Per = direct,
  meeltypeF(smsbulk),
  getCurrentModel(_Win, Ond, Med, MetVervolg),
  %get_toernooidir(Padmet, _),
  get_TempDir(_, TempFile),
  openwrite(work,TempFile),
  %filenamepath(Vollenaam, Padmet, tmpconst),
  writedevice(work),
  MB = getmailbox(_Win, Test),
  MB = mapinameaddr(ENaam, Mailbox),
  write("[]",MailBox,"[]"),
  individueelEmailKlapper(Ond, Med, MetVervolg, Test),
  closefile(work),
  uploadalg1("oproepcurl3.php", "individueel1", [TempFile], "Opsturen van SMS-e-mails", b_False,99,Gedaan,["afzendernaam", ENaam]), % bestand bestaat al
  assert(gedaan(Gedaan)),
  fail.
versturen(_Win, Test) :-				%  enkel outlook (model bypass)
  getPer(_Win, Per),
  Per = outlook,
  meeltypeF(enkel),
  getCurrentModel(_Win, _, _, MetVervolg),
  RWin = win_GetCtlHandle(_Win, idc_resultaat),
  Med = edit_GetText(RWin),
  skipRegel1(Med, Med1),
  OUWin = win_GetCtlHandle(_Win, idc_onderwerp1),
  Ond = win_GetText(OUWin),
  disk(Current),
  aantalAdressen(Aantal),
  format(Msg, "Naar %u adres(sen), aparte emails voor iedereen.\nKomt in de uitbak van je eigen mailprogramma.", Aantal),
  1 = dlg_MessageBox( "Email", Msg , mesbox_iconinformation, mesbox_buttonsokcancel, mesbox_defaultfirst, mesbox_suspendapplication ), 
  individueelEmail(Ond, Med1, MetVervolg,Test),
  disk(Current),
  fail.
versturen(_Win, Test) :-				% enkel direct
  getPer(_Win, Per),
  Per = direct,
  meeltypeF(enkel),
  getCurrentModel(_Win, _, _, MetVervolg),
  RWin = win_GetCtlHandle(_Win, idc_resultaat),
  Med = edit_GetText(RWin),
  skipRegel1(Med, Med1),
  OUWin = win_GetCtlHandle(_Win, idc_onderwerp1),
  Ond = win_GetText(OUWin),
  get_TempDir(_, TempFile),
  openwrite(work,TempFile),
  %get_toernooidir(Padmet, _),
  %ilenamepath(Vollenaam, Padmet, tmpconst),
  %openwrite(work, TempFile),
  writedevice(work),
  MB = getmailbox(_Win, Test),
  MB = mapinameaddr(ENaam, Mailbox),
  write("[]",MailBox,"[]"),
  individueelEmailKlapper(Ond, Med1, MetVervolg, Test),
  closefile(work),
  uploadalg1("oproepcurl3.php", "individueel1", [TempFile], "Opsturen van e-mails", b_False,99,Gedaan, ["afzendernaam", ENaam]), % bestand bestaat al
  assert(gedaan(Gedaan)),
  fail.
/*versturen(_Win, Test) :-				%  sms outlook (model bypass)
  getPer(_Win, Per),
  Per = outlook,
  meeltype(sms),
  getCurrentModel(_Win, _, _, MetVervolg),
  RWin = win_GetCtlHandle(_Win, idc_resultaat),
  Med = edit_GetText(RWin),
  skipRegel1(Med, Med1),
  OUWin = win_GetCtlHandle(_Win, idc_onderwerp1),
  Ond = win_GetText(OUWin),
  disk(Current),
  %aantalAdressen(Aantal),
  Msg = "SMS via email.\nKomt in de uitbak van je eigen mailprogramma.",
  1 = dlg_MessageBox( "Email", Msg , mesbox_iconinformation, mesbox_buttonsokcancel, mesbox_defaultfirst, mesbox_suspendapplication ), 
  individueelEmail(Ond, Med1, MetVervolg,Test),
  disk(Current),
  updatetegoed(1),
  fail. */
versturen(_Win, Test) :-				% sms direct
  %getPer(_Win, Per),
  %Per = direct,
  meeltypeF(sms),
  getCurrentModel(_Win, _, _, MetVervolg),
  RWin = win_GetCtlHandle(_Win, idc_resultaat),
  Med = edit_GetText(RWin),
  skipRegel1(Med, Med1),
  OUWin = win_GetCtlHandle(_Win, idc_onderwerp1),
  Ond = win_GetText(OUWin),
  get_TempDir(_, TempFile),
  openwrite(work,TempFile),
  %get_toernooidir(Padmet, _),
  %ilenamepath(Vollenaam, Padmet, tmpconst),
  %openwrite(work, TempFile),
  writedevice(work),
  MB = getmailbox(_Win, Test),
  MB = mapinameaddr(_ENaam, Mailbox),
  write("[]",MailBox,"[]"),
  individueelEmailKlapper(Ond, Med1, MetVervolg, Test),
  closefile(work),
  uploadalg1("oproepcurl3.php", "individueel1", [TempFile], "Opsturen van e-mails", b_False,99,Gedaan, []), % bestand bestaat al
  updatetegoed(1),
  assert(gedaan(Gedaan)),
  fail.
versturen(_Win, echt) :-				% aantekenen
  closefile(work), 
  gedaan(b_True),
  %MWin = win_GetCtlHandle(_Win, idc_aantekenen_op_het_spelerformulier),
  %Aan = win_IsChecked(MWin),
  %assert(iAantekenen(Aan)),
  %b_True = Aan,
  %meeltypeF(MT),
  %not(MT = oproep),
  %aantekenen(MT),
  win_Destroy(_Win),
  findall(Nr, eAdres(Nr, _, _, echt), NrList),
  count(NrList, 0, Aantal),
  Aantal > 5,
  compress(update),
  startDbRefresh(),
  !.
versturen(_Win, Test) :-				% oproepen direct
  meeltypeF(oproep),
  emailadresZelfP(Email, ENaam),
  get_TempDir(TempDir, TempFile),
  openwrite(work,TempFile),
  %get_toernooidir(Padmet, _),
  %filenamepath(Vollenaam, Padmet, tmpconst),
  getCurrentModel(_Win, Onderwerp, Mededeling, MetVervolg),
  %openwrite(work, Vollenaam),
  writedevice(work),
  write("[]",Email,"[]"),
  oproepEmailKlapper(Onderwerp, Mededeling, MetVervolg, Test),
  closefile(work),
  uitslagEnableUpload(),
  eAdres(_, _, _, Test),
  uploadalg1("oproepcurl3.php", "oproep1", [TempFile], "E-mails oproep", b_True, 57, _, ["afzendernaam", ENaam]), % bestand bestaat al
  filenamePath(Report1, TempDir, "uitklaar.htm"),
  file_str(Report1, String1),
  %write("\n ####\n",String1,"\n"),
  getbevestigingen(),	% nog een keer ...
  checkvoortgang(100, ""),
  verzondenOK(String1),
  %remoteUitslag(Test),
  Test = echt,
  _Aa = dlg_MessageBox( "Oproepmail", "Vergeet niet om de ToernooiKlapper te updaten (Alt-W)!", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  win_Destroy(_Win),
  !.
versturen(_Win, echt) :-
  meeltypeF(oproep),
  dlg_MessageBox( "Oproepemails", "Inmiddels valt er niets meer op te roepen!\nCheck eventueel \"Planning t/m\"!", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  win_Destroy(_Win),
  !.
versturen(_Win, echt) :-
  trap(win_Destroy(_Win),_,fail),
  !.
versturen(_Win, echt).

tegoedOK(_Win) :-
  emailsmsp(_, Tegoed, _),
  aantaladressen(A),
  Tegoed >= A, !.
tegoedOK(_Win) :-
  dlg_MessageBox( "SMS tegoed", "Het huidige tegoed is niet toereikend!", mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  dlg_emailsmsinstellingen_Create(_Win),
  fail.

updateTegoed(Aantal) :-
  emailsmsp(Prov, Tegoed, mapinameaddr(_, Afzender)),
  retractall(emailsms(_,_, _, _)),
  Tegoed1 = Tegoed - Aantal,
  assert(emailsms(Prov, Tegoed1, Afzender, [])).

sluitenMaar(_Win) :-
  meeltypeF(MeelType),
  getPer(_Win, Vehikel),
  getCurrentModel(_Win, Onderwerp, Template, VervolgWeds),
  not(algemeelX(Meeltype, Vehikel, Onderwerp, Template, Vervolgweds)),
  1 = dlg_MessageBox( "Sluiten", "Model en/of instellingen zijn veranderd,\nOpslaan?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
  setAlgemeelX(Vehikel, Onderwerp, Template, VervolgWeds),
  fail.
sluitenMaar(_Win) :-
  not(meeltypeF(sms)),
  not(meeltypeF(smsbulk)),
  MB = getMailbox(_Win, echt), 
  MB = mapinameaddr(_, Mailbox),
  emailadresZelfP(Mailbox1, ENaam),
  Mailbox <> Mailbox1,
  1 = dlg_MessageBox( "Sluiten", "Afzenderemailadres is veranderd,\nOpslaan?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultfirst, mesbox_suspendapplication ),
  zetEmailadresZelf(Mailbox, ENaam), !.
sluitenMaar(_Win).

emailadresZelfP(Mailbox1, ENaam) :-
  api_getComputername(CN),
  upper_lower(PC, CN),
  emailadresZelf(PC, Mailbox1, ENaam), !.
emailadresZelfP("", "").

  dlg_email_algemeen_Create(_Parent, _):-
	emailWin(_), !.
  dlg_email_algemeen_Create(Parent, MeelType):-
	assert(emailWin(~0)),
  	assert(meelTypeF(MeelType)),
	plantm(DagNum),
	dagnumEval(DagNum, DagNumP),
	assert(plantm(DagNumP)),
        retractall(editcontroltext(idc_custom,_,_,_,_,_,_)),
        retractall(editcontroltext(idc_resultaat,_,_,_,_,_,_)),
	Tekst = "",
	assert(editcontroltext(idc_custom,Tekst,b_True,ff_Helvetica,b_False,9,1)),
	assert(editcontroltext(idc_resultaat,Tekst,b_True,ff_Helvetica,b_True,8,1)),
	win_CreateDynDialog(Parent,
		[
%BEGIN Email algemeen, WinDefList, 12:13:21-14.9.2013, Code automatically updated!
		 dlg_font(wdef(dlg_email_algemeen_DlgType,dlg_email_algemeen_RCT,dlg_email_algemeen_Title,u_DlgBase),
		 	  dlg_email_algemeen_Font,dlg_email_algemeen_FSize,dlg_email_algemeen_Flags),
		 ctl(wdef(wc_Edit,rct(1,45,244,56),"",u_DlgBase),idc_onderwerp,[wsf_AlignLeft,wsf_Group,wsf_TabStop,wsf_AutoHScroll]),
		 customctl(wdef(wc_Custom,rct(1,68,249,203),"Custom",u_DlgBase),"pdcedit",idc_custom,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_LBoxButton,rct(7,227,55,307),"",u_DlgBase),idc_plan_tm,[wsf_Group,wsf_TabStop,wsf_VScroll]),
		 ctl(wdef(wc_CheckBox,rct(7,252,88,262),"&Vervolgwedstr. incl.",u_DlgBase),idc_met_vervolg,[wsf_Group,wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_PushButton,rct(99,221,166,239),"&Hele Modellen ...",u_DlgBase),idc_standaardtekst,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(99,250,166,268),"Invulvariabelen ...",u_DlgBase),idc_invulvariabelen,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(185,209,233,221),"&Undo",u_DlgBase),idc_undo,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(76,282,161,295),"&Instellingen voor SMS",u_DlgBase),idc_sms_via_email_instellingen,[wsf_Group,wsf_TabStop,wsf_Disabled,wsf_Invisible]),
		 ctl(wdef(wc_Edit,rct(263,12,511,23),"",u_DlgBase),idc_onderwerp1,[wsf_AlignLeft,wsf_Group,wsf_TabStop,wsf_AutoHScroll,wsf_ReadOnly]),
		 ctl(wdef(wc_Edit,rct(318,240,512,253),"",u_DlgBase),idc_email_afzendernaam,[wsf_AlignLeft,wsf_Group,wsf_TabStop,wsf_AutoHScroll]),
		 ctl(wdef(wc_Edit,rct(318,255,512,268),"",u_DlgBase),idc_afzender,[wsf_AlignLeft,wsf_Group,wsf_TabStop,wsf_AutoHScroll]),
		 ctl(wdef(wc_RadioButton,rct(265,273,301,283),"Direct",u_DlgBase),idc_direct,[wsf_Auto,wsf_TabStop,wsf_Group]),
		 ctl(wdef(wc_RadioButton,rct(265,283,303,293),"Outlook",u_DlgBase),idc_outlook,[wsf_Auto,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(461,268,512,280),"&Test de email",u_DlgBase),idc_test,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(461,281,512,293),"&Verstuur",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(164,282,204,294),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(207,282,247,294),"&Cancel",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_Text,rct(264,1,305,11),"Ontvangers",u_DlgBase),idct_ontvanger,[wsf_AlignLeft]),
		 ctl(wdef(wc_Edit,rct(304,1,512,12),"Lijst van emailadressen",u_DlgBase),idc_ontvanger_mail_adres,[wsf_AlignLeft,wsf_Group,wsf_AutoHScroll]),
		 ctl(wdef(wc_Text,rct(1,35,39,45),"&Onderwerp",u_DlgBase),idct_onderwerp,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(82,57,162,67),"&Model/sjabloon",u_DlgBase),idct_mededeling,[wsf_AlignCenter]),
		 ctl(wdef(wc_Text,rct(266,256,320,265),"Afzender adres",u_DlgBase),idct_afzender,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(7,217,55,227),"Planning t/m",u_DlgBase),idct_planning_tm,[wsf_AlignLeft]),
		 customctl(wdef(wc_Custom,rct(264,24,515,240),"Resultaat",u_DlgBase),"pdcedit",idc_resultaat,[wsf_Group]),
		 ctl(wdef(wc_Text,rct(22,11,226,29),"Selecteer een model, vul de gewenste tekst in en voeg de gewenste invulvariabelen toe. Rechts verschijnt het resultaat.",u_DlgBase),idct_aanw,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(252,174,260,184),">>",u_DlgBase),idct_zomoetie,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(177,233,248,263),"Houd de email/SMS svp kort en zakelijk.",u_DlgBase),idct_tekst,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(266,241,317,250),"Afzender naam",u_DlgBase),idc_email_algemeen_34,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(8,262,87,272),"NB ook binnen poules!",u_DlgBase),idct_nb_ook_poules,[wsf_AlignLeft]),
		 ctl(wdef(wc_GroupBox,rct(263,265,306,296)," Per ",u_DlgBase),idc_per,[]),
		 ctl(wdef(wc_GroupBox,rct(251,2,261,291),"",u_DlgBase),idc_groupbox,[]),
		 ctl(wdef(wc_GroupBox,rct(13,1,235,34),"Emails versturen",u_DlgBase),idc_groupbox1,[]),
		 ctl(wdef(wc_GroupBox,rct(2,205,90,280),"Oproepen voor",u_DlgBase),idc_groupbox2,[]),
		 ctl(wdef(wc_GroupBox,rct(94,205,171,280),"Invoegen",u_DlgBase),idc_invoegen,[])
%END Email algemeen, WinDefList
		],dlg_email_algemeen_eh,0).

%BEGIN Email algemeen, idc_ok _CtlInfo
  dlg_email_algemeen_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	versturenNotOK(_Win), !.
  dlg_email_algemeen_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	versturen(_Win, echt), !.
%END Email algemeen, idc_ok _CtlInfo
%MARK Email algemeen, new events

%BEGIN Email algemeen, idc_sms_via_email_instellingen _CtlInfo
  dlg_email_algemeen_eh(_Win,e_Control(idc_sms_via_email_instellingen,_CtrlType,_CtrlWin,_CtlInfo),0):-
	dlg_emailsmsinstellingen_Create(_Win),
	!.
%END Email algemeen, idc_sms_via_email_instellingen _CtlInfo

%BEGIN Email algemeen, e_CloseRequest
  dlg_email_algemeen_eh(_Win,e_CloseRequest,0):-
	sluitenMaar(_Win),
	fail.
%END Email algemeen, e_CloseRequest

%BEGIN Email algemeen, idc_test _CtlInfo
  dlg_email_algemeen_eh(_Win,e_Control(idc_test,_CtrlType,_CtrlWin,_CtlInfo),0):-
	versturenNotOK(_Win), !.
  dlg_email_algemeen_eh(_Win,e_Control(idc_test,_CtrlType,_CtrlWin,_CtlInfo),0):-
	MB = getMailbox(_Win, echt),
	MB = mapinameaddr(_, Mailbox), 
	getbacktrack(Here),
	retract(eadres(Nr, mapinameaddr(Naam, _), Bondsnummer, istest)),	% kijk nog even de bestemming na
	cutbacktrack(Here),
	asserta(eadres(Nr, mapinameaddr(Naam, Mailbox), Bondsnummer, istest)),	% 
	fail.
  dlg_email_algemeen_eh(_Win,e_Control(idc_test,_CtrlType,_CtrlWin,_CtlInfo),0):-
	MB = getmailbox(_Win, echt),
	MB = mapinameaddr(_, Mailbox),
	format(Msg, "Bij test wordt de eerste email verstuurd,\n waarbij geadresseerde en afzender worden omgewisseld.\nDus bekijk het resultaat in de mailbox van\n%s.", Mailbox),
	_Answer = dlg_MessageBox( "Test", Msg, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	versturen(_Win, istest),
	!.
%END Email algemeen, idc_test _CtlInfo

%BEGIN Email algemeen, idc_met_vervolg _CtlInfo
  dlg_email_algemeen_eh(_Win,e_Control(idc_met_vervolg,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	berichtvoorbeelden(_Win),
	!.
%END Email algemeen, idc_met_vervolg _CtlInfo

%BEGIN Email algemeen, idc_undo _CtlInfo
  dlg_email_algemeen_eh(_Win,e_Control(idc_undo,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
  	MWin = win_GetCtlHandle(_Win, idc_custom),
  	trap(edit_Undo(MWin), _, fail),
  	setEmailWinTimer(),
	!.
%END Email algemeen, idc_undo _CtlInfo

%BEGIN Email algemeen, idc_invulvariabelen _CtlInfo
  dlg_email_algemeen_eh(_Win,e_Control(idc_invulvariabelen,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	findall(Invulvariabele, invul(Invulvariabele), ItemList),
        Menu = dyn_menu(ItemList),
	%TaskWin = vpi_GetTaskWin(),
	PNT = cursor_GetPos(_Win),
	menu_PopUp(_Win,Menu, PNT, align_Left),
	!.
%END Email algemeen, idc_invulvariabelen _CtlInfo

%BEGIN Email algemeen, e_Timer
  dlg_email_algemeen_eh(_Win,e_Timer(TimerId),0):-
	timer_Kill(TimerId),
	retractall(emailWinTimer(TimerId)),
	berichtvoorbeelden(_Win),
	!.
%END Email algemeen, e_Timer

%BEGIN Email algemeen, idc_resultaat1 _CtlInfo

%BEGIN Email algemeen, e_User
  dlg_email_algemeen_eh(_Win,e_User(emailalg,_Ptr),0):-		% initverder (enkel)
	cursor_SetWait(),
	meeltypeF(enkel),
	MWin = win_GetCtlHandle(_Win, idc_resultaat),
	edit_setReadOnly(MWin, b_False),
	fail.
  dlg_email_algemeen_eh(_Win,e_User(emailalg,_Ptr),0):-		% initverder (sms)
	cursor_SetWait(),
	meeltypeF(sms),
	IWin = win_GetCtlHandle(_Win, idc_sms_via_email_instellingen),
	win_SetState(IWin, [wsf_enabled, wsf_visible]),
	MWin = win_GetCtlHandle(_Win, idc_resultaat),
	edit_setReadOnly(MWin, b_False),
	TWin = win_GetCtlHandle(_Win, idc_test),
	win_SetState(TWin, [wsf_disabled]),
	GWin = win_GetCtlHandle(_Win, idc_groupbox1),
	win_SetText(GWin, "SMS via email"),
	AWin = win_GetCtlHandle(_Win, idct_aanw), 
	win_SetText(AWin, "Je kunt de SMS in het rechterdeel nog verder aanpassen."),
	fail.
  dlg_email_algemeen_eh(_Win,e_User(emailalg,_Ptr),0):-		% initverder (smsbulk)
	cursor_SetWait(),
	meeltypeF(smsbulk),
	IWin = win_GetCtlHandle(_Win, idc_sms_via_email_instellingen),
	win_SetState(IWin, [wsf_enabled, wsf_visible]),
	TWin = win_GetCtlHandle(_Win, idc_test),
	win_SetState(TWin, [wsf_disabled]),
	GWin = win_GetCtlHandle(_Win, idc_groupbox1),
	win_SetText(GWin, "Bulk SMS via email"),
	AWin = win_GetCtlHandle(_Win, idct_aanw), 
	win_SetText(AWin, "Pas het model aan naar wens. De sms mag max 160 tekens bevatten. Zorg dat je voldoende tegoed hebt!"),
	fail.
  dlg_email_algemeen_eh(_Win,e_User(emailalg,_Ptr),0):-		% initverder voor eventuele test
	getbacktrack(Here),
	eadres(Nr, mapinameaddr(Naam, _Email), Bondsnummer, echt),
	cutbacktrack(Here),
	MB = getMailbox(_Win, echt),
	MB = mapinameaddr(_, Mailbox), 
	assert(eadres(Nr, mapinameaddr(Naam, Mailbox), Bondsnummer, istest)),	% maak een fake eadres
	fail.
  dlg_email_algemeen_eh(_Win,e_User(emailalg,_Ptr),0):-		% initverder
	cursor_SetWait(),
	meeltypeF(MeelType),
	algemeelP(MeelType, _Per, _Onderwerp, Body, _MetVervolg),
	MWin = win_GetCtlHandle(_Win, idc_custom),
	edit_pastestr(MWin, Body),
        berichtvoorbeelden(_Win), 
        fail.
  dlg_email_algemeen_eh(_Win,e_User(emailalg,_Ptr),0):-		% initverder
	meeltypeF(oproep),
	win_SetText(_Win, "Oproep-emails versturen"),
	fail.
  dlg_email_algemeen_eh(_Win,e_User(emailalg,_Ptr),0):-		% initverder
	meeltypeF(enkel),
	win_SetText(_Win, "Email naar speler"),
	fail.
  dlg_email_algemeen_eh(_Win,e_User(emailalg,_Ptr),0):-		% initverder
	meeltypeF(sms),
	win_SetText(_Win, "SMS via email"),
	OW = win_GetCtlHandle(_Win, idc_onderwerp),
	win_SetState(OW, [wsf_Invisible,wsf_Disabled]),
	O1W = win_GetCtlHandle(_Win, idct_onderwerp),
	win_SetState(O1W, [wsf_Invisible]),
	AW = win_GetCtlHandle(_Win, idct_afzender),
	win_SetState(AW, [wsf_Invisible]),
	A1W = win_GetCtlHandle(_Win, idc_afzender),
	win_SetState(A1W, [wsf_Invisible, wsf_Disabled]),
	tegoedOK(_Win),
	!.
  dlg_email_algemeen_eh(_Win,e_User(emailalg,_Ptr),0):-		% initverder
	meeltypeF(smsbulk),
	win_SetText(_Win, "Bulk-SMS via email"),
	OW = win_GetCtlHandle(_Win, idc_onderwerp),
	win_SetState(OW, [wsf_Invisible,wsf_Disabled]),
	O1W = win_GetCtlHandle(_Win, idct_onderwerp),
	win_SetState(O1W, [wsf_Invisible]),
	AW = win_GetCtlHandle(_Win, idct_afzender),
	win_SetState(AW, [wsf_Invisible]),
	A1W = win_GetCtlHandle(_Win, idc_afzender),
	win_SetState(A1W, [wsf_Invisible, wsf_Disabled]),
	tegoedOK(_Win),
	!.
  dlg_email_algemeen_eh(_Win,e_User(emailalg,_Ptr),0) :- !.
%END Email algemeen, e_User

%BEGIN Email algemeen, idc_outlook _CtlInfo
  dlg_email_algemeen_eh(_Win,e_Control(idc_outlook,_CtrlType,_CtrlWin,_CtlInfo),0):-
	DWin = win_GetCtlHandle(_Win, idc_afzender),
	win_SetState(DWin, [wsf_disabled]),
	!.
%END Email algemeen, idc_outlook _CtlInfo

%BEGIN Email algemeen, idc_direct _CtlInfo
  dlg_email_algemeen_eh(_Win,e_Control(idc_direct,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	DWin = win_GetCtlHandle(_Win, idc_afzender),
	win_SetState(DWin, [wsf_Enabled]),
	!.
%END Email algemeen, idc_direct _CtlInfo

%BEGIN Email algemeen, idc_plan_tm closeup
  dlg_email_algemeen_eh(_Win,e_Control(idc_plan_tm,_CtrlType,_CtrlWin,closeup),0):-!,
	DWin = win_GetCtlHandle(_Win, idc_plan_tm),
	Dag = win_GetText(DWin),
	finddag(DagNumP,Dag), 
	assert(plantm(DagNumP)),
	%meeltype(MeelType),
	%algemeelP(MeelType, _Per, _Onderwerp, Body, MetVervolg),
	berichtvoorbeelden(_Win),
	%MWin = win_GetCtlHandle(_Win, idc_resultaat),
	%edit_pastestr(MWin, Tekst),
	!.
%END Email algemeen, idc_plan_tm closeup

%BEGIN Email algemeen, idc_standaardtekst _CtlInfo
  dlg_email_algemeen_eh(_Win,e_Control(idc_standaardtekst,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	findall(Item, standaardtekst(_,Item,_),ItemList),
        Menu = dyn_menu(ItemList),
	PNT = cursor_GetPos(_Win),
	menu_PopUp(_Win,Menu, PNT, align_Left),
	!.
%END Email algemeen, idc_standaardtekst _CtlInfo

%BEGIN Email algemeen, idc_help _CtlInfo
  dlg_email_algemeen_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Emailsturen"),
	!.
%END Email algemeen, idc_help _CtlInfo

%BEGIN Email algemeen, idc_cancel _CtlInfo
  dlg_email_algemeen_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-
	sluitenMaar(_Win),
	win_Destroy(_Win),
	!.
%END Email algemeen, idc_cancel _CtlInfo

%BEGIN Email algemeen, e_Destroy
  dlg_email_algemeen_eh(_Win,e_Destroy,0):-!,
        retractall(editcontroltext(idc_custom,_,_,_,_,_,_)),
        retractall(editcontroltext(idc_resultaat,_,_,_,_,_,_)),
	retractall(emailWin(_)),
	updateVensterpositie(emailgroepvenster, _Win),
	retractall(eAdres(_,_,_,_)),
	!.
  dlg_email_algemeen_eh(_Win,e_Destroy,0):-
	!.
%END Email algemeen, e_Destroy


%BEGIN Email algemeen, e_Create
  dlg_email_algemeen_eh(_Win,e_Create(_CreationData),0):-!,
	dialoogpositie(emailgroepvenster, _Win),	% als bewaard van de vorige keer
	OWin = win_GetCtlHandle(_Win, idc_ontvanger_mail_adres),
	%eenAdres(Sp),
	aantalAdressen(Aantal),
	lijstPersonenTitel(Titel),
	format(Msg, "%u adressen uit %s.",Aantal, Titel),
	win_SetText(OWin, Msg),
	dialog_SetState(_Win, [enable(idc_ontvanger_mail_adres, b_False)]),
	%
	meeltypeF(MeelType),
        algemeelP(MeelType, Veh, Onderwerp, _Tekst, MetVervolg),
	%win_SetText(OWWin, Onderwerp1),
	%iAantekenen(Aant),
	%AanWin = win_GetCtlHandle(_Win, idc_aantekenen_op_het_spelerformulier),
	%win_Check(AanWin, Aant),
	OMWin = win_GetCtlHandle(_Win, idc_onderwerp),
	win_SetText(OMWin, Onderwerp),
	emailadresZelfP(Email, ENaam),
	ZWin = win_GetCtlHandle(_Win, idc_afzender),
	win_SetText(ZWin, Email),
	ENWin = win_GetCtlHandle(_Win, idc_email_afzendernaam),
	win_SetText(ENWin, ENaam),
	vehikel2rb(_Win, Veh, RBV),
	RBVWin = win_GetCtlHandle(_Win, RBV),
	DWin = win_GetCtlHandle(_Win, idc_direct),
	OLWin = win_GetCtlHandle(_Win, idc_outlook),
	win_CheckRadioButton(RBVWin, [DWin, OLWin]),
	PWin = win_GetCtlHandle(_Win, idc_plan_tm),
	findall(Dag, dag(_, Dag, _), SList),
	plantm(DagNumP),
	findIndex(Slist, DagNumP, 0, SSelect),
	lbox_Add(PWin, SList),
	lbox_SetSel(PWin, SSelect, b_True),
	%CWin = win_GetCtlHandle(_Win, idc_custom),
	%edit_SetReadOnly(CWin, b_True),
	retractall(emailWin(_)),
	assert(emailWin(_Win)),
	MVW = win_GetCtlHandle(_Win, idc_met_vervolg),
	win_Check(MVW, MetVervolg),
	win_PostEvent(_Win,e_User(emailalg,0)),
	!.
%END Email algemeen, e_Create
% EIGEN
  dlg_email_algemeen_eh(_Win, e_Menu(ID, _), 0) :-		% model invullen
	standaardTekst(ID, _, Model),
	not(Model = ""),
	OWWin = win_GetCtlHandle(_Win, idc_onderwerp),
	win_SetText(OWWin, "%toernooi%"),
  	MWin = win_GetCtlHandle(_Win, idc_custom),
	Body = edit_GetText(MWin),
	retractall(tijdelijkModel(_)),
	assert(tijdelijkModel(Body)),
	edit_pastestr(MWin, Model),
	berichtvoorbeelden(_Win), !.
  dlg_email_algemeen_eh(_Win, e_Menu(ID, _), 0) :-		% invulvariabele
	invul(Var),
        Var = txt(ID,Intevullen,_,_,_,_), !,
        expandeerInTeVullen(Intevullen, InTeVullen1),
  	MWin = win_GetCtlHandle(_Win, idc_custom),
	win_SetFocus(MWin),
	_Result = vpi_ProcessEvents(),
	format(Msg,"Op de plaats van de cursor wordt ingevoegd:\n\n%s",Intevullen1),
	1 = dlg_MessageBox( "Invulvariabele toevoegen", Msg, mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
  	Pos = edit_GetPos(MWin),
  	IV = Intevullen1,
  	edit_PasteStr(MWin, Pos, IV),
  	edit_searchStr(MWin, IV),	% om te highlighten
	berichtvoorbeelden(_Win), !.
% END_EIGEN
  dlg_email_algemeen_eh(_,_,_):-!,fail.

%END_DLG Email algemeen

domains
  large = large(integer, integer)

database - intotalen
  single bedragen(ulong)
  single nummers(large)
  single posten(ulong)
  nondeterm geboekt(integer)
  single boekdatum(MINUT_OFFSET)

predicates 
%  leadingZeros(string In, integer Len, string Uit)
%  woonplaats(string Spelernaam, string In, string Uit)
% procedure spelersMetIncasso(boolean Test, string Eigenrekening, string ToernooiNaam, ulong Posten, ulong Bedragen, MINUT_OFFSET Boekdatum, string Van, string Tot)
% procedure testVlag(boolean, string)
% procedure boekbetaald()
procedure largeAdd(large, large, large) - (i, i, o)
procedure int2large(integer, large) - (i, o)
determ str2large(string, large) - (i, o)
procedure incassoNr1(string, string, string)
% nondeterm spVanTot(string Van, string TotM, string Naamx)  % incassospelers!
% nondeterm vanTot(integer, string Van, string Tot, string BestandsLetters)
determ proef11b(string) - (i)
determ    proef11ba(string Rev, integer Pos, integer TussenSom) - (i, i, i)


clauses

bedragen(0).
nummers(large(0,0)).
posten(0).
boekdatum(0).
/*
vanTot(5, "A", "E", "AE").
vanTot(5, "F", "J", "FJ").
vanTot(5, "K", "O", "KO").
vanTot(5, "P", "T", "PT").
vanTot(5, "U", "Z", "UZ"). 
%vanTot(5, "V", "V").

vanTot(1, "A", "Z", "").
*/
largeAdd(large(A1,A2), large(B1, B2), large(C1, C2)) :-
  I2 = A2 + B2,
  C2 = I2 mod 10000,
  C1 = A1 + B1 + I2 div 10000, !.

int2large(I, large(L1, L2)) :-
  L1 = I mod 10000,
  L2 = I div 10000, !.

str2large(S, large(0, L2)) :-
  str_len(S, Len),
  Len <= 4,
  str_int(S, L2), !.
str2large(S, large(L1, L2)) :-
  str_len(S, LenT),
  LenF = LenT - 4,
  frontstr(LenF,S,S1,S2),
  str_int(S1,L1), 
  str_int(S2, L2), !.

/*
incassoTekst() :-
  retractall(geboekt(_)),	
  TaskWin = vpi_GetTaskWin(),
  checkPaswoord(TaskWin),
  dlg_MessageBox( "Incasso", "Inspecteer ook de betaallijst!\nDaar moeten de posten op voorkomen, anders eerst\nindien nodig de betalingen-resetfunctie gebruiken.\nZorg dat de onderdeelkenmerken het juiste inschrijfgeld\nbevatten.", mesbox_iconquestion, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  dlg_incasso_Create(TaskWin,EigenRekeningI,ToernooiNaam,Boekdatum,Aantaldelen),
  dt_minOffset_to_str(Boekdatum, "%DD%MD%YS", BoekdatumStr),  
  write("\n", BoekdatumStr),
  not(EigenrekeningI = ""),
  getFolder(Documents,_,_,_),
  filenamepath(FN, Documents, "clieop*.*"),
  format(Command, "attrib -R \"%s\"", FN),
  system(Command), 
  Best1 = dlg_GetFileName("Clieop03.inc",["incasso bestanden","*.inc"],"Sla het incassobestand op als",[dlgfn_Save],Documents,_OutListFiles),
  vanTot(Aantaldelen, Van, Tot, VTB),
  filenamePath(Best1, Pad, FNaam),
  filenameext(FNaam, FNaam1, Fext),
  format(FnaamU1, "%s%s", FNaam1, VTB),
  filenameext(FNaamU, FnaamU1, FExt),
  filenamePath( Best2, Pad, FNaamU),
  format(Command1, "attrib -R \"%s\"", Best2),
  system(Command1), 
  assert(bedragen(0)),
  assert(nummers(large(0,0))),
  assert(posten(0)),
  stopDBrefresh(),
  openwrite(work, Best2),
  writedevice(work),
  spelersMetIncasso(b_False, EigenrekeningI, ToernooiNaam, Posten, Bedrag, Boekdatum, Van, Tot),
  closefile(work),
  startDBrefresh(),
  writedevice(screen),
  %file_str(Best1,Text),
  TaskWin = vpi_GetTaskWin(),
  %win_editor_Create(Taskwin, Text, Best1),
  Bedrag1 = Bedrag / 100,
  %alsGeenposten(Posten),
  BetRes = "Als de incasso mislukt kunt u de functie alle betalingen resetten gebruiken, (zie Meer > reset Alle betalingen),\nverbeteringen aanbrengen in bijvoorbeeld woonplaats en het bestand opnieuw genereren.",
  format(Msg, "Totaal aantal posten: %u.\nTotaal bedrag: %7.2f.\n\nHet incassobestand (letters %s tot en met %s) staat HIER gereed: %s.\n\nDe spelers worden op betaald gezet.\n\n%s",Posten,Bedrag1,Van,Tot,Best2,BetRes),
  _Ans = dlg_MessageBox( "Incasso", Msg, mesbox_iconquestion, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  system(Command1),
  %write(Command1),
  boekbetaald(),  
  fail.

boekbetaald() :-
  %beep(),
  retract(geboekt(SKeus)),
  %sp2(SKeus,_SpelerSrt,Naam,_Tel,_,_,_,_Betaald,_, _, _Adres, _WoonPl, _LidNr, _, _, _, _Telwerk, _Telmobiel, _EMail, _Opm,_Gezien,_RangPosE,_RangPosD,_Incasso,_CheckGeg,_,_Res),
  sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,nee,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
  SpelerIn = sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,ja,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Import),
  logf('A',SpelerIn,ja),	% 8.8.2013  !!
  cursor_SetWait(),
  %_Result = vpi_ProcessEvents(),
  fail.
boekbetaald() :-
  logclose(),
  close_betaal(),
  _Ans = dlg_MessageBox( "Incasso", "Alle incasso-spelers zijn op betaald gezet!\nMaak het eventueel ongedaan met Betalingen>Meer>Reset ALLE betalingen!", mesbox_iconinformation, mesbox_buttonsOK, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.
boekbetaald() :-
  retractall(geboekt(_)).
*/
incassonr(_Naam, In, Uit) :-   % gironummer
  frontstr(1,In,Char,RestString),
  upper_lower("P", Char),
  trim_c(Reststring),
  incassoNr1(Reststring, "", Uit),
  str_len(Uit, Len),
  Len > 5,
  Len <= 9,
  not(Uit = "1234567"),
  not(Uit = "0000000"), !.
incassonr(_Naam, In, Uit) :-  % IBAN
  trim_c(In),
  str_len(In, Len),
  %writedevice(X),
  %writedevice(screen),
  %write("\nIn: ",In," ",Len),
  %writedevice(X),
  Len = 18,
  frontstr(8,In,_Char,Uit),   !.
incassonr(_Naam, In, Uit) :-  % banknummer
  frontstr(1,In,Char,_RestString),
  not(upper_lower("P", Char)),
  trim_c(In),
  incassoNr1(In, "", Uit),
  proef11b(Uit), !.
incassonr(Naam, In, "") :-
  str_len(In, Len),
  format(Str, "Speler %s heeft banknummer %s ??\nIncasso voor deze speler wordt overgeslagen.\n(aantal tekens %u)", Naam, In, Len),
  _A = dlg_MessageBox( "Fout nummer?", Str, mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),	
  fail.

proef11b(Str) :-
  %trap(term_str(ulong, _, Str),_,fail),
  str_len(Str, Len),
  Len < 10,
  reversestr(Str, "", Rev),
  proef11a(Rev, 1, 0), !.

proef11ba(String, Pos, Som) :-
  frontstr(1,String,StartString,RestString), !,
  %write("\n ", StartString),
  str_int(StartString, Int), 
  Som1 = Som + Int * Pos,
  Pos1 = Pos + 1,
  proef11a(RestString, Pos1, Som1).
proef11ba(_X, _, Som) :-
  Y = Som mod 11,
  Y = 0.


incassoNr1(Inc, In, Uit) :-
  frontchar(Inc,Char,RestString),
  Char <= '9',
  Char >= '0',
  incassoNr1(Reststring, In, In1),
  frontchar(Uit, Char, In1), !.
incassoNr1(Inc, In, Uit) :-
  frontchar(Inc,_Char,RestString), !,
  incassoNr1(RestString, In, Uit).
incassoNr1(_, In, In).
domains
  w2ed = w2ea(integer Bevestigd, integer SpNo, string EmailAddr, integer, wedscat, integer SpNo1, integer Tijd, tgnst);
         w2eo(integer Bevestigd, integer SpNo, string EmailAddr, integer, wedscat, integer LR, integer Tijd, tgnst)
/*
w2eo(0,16080,"gerdo.bakker@xs4all.nl",3,1,1,94027842,s(16080)
*/
predicates
%procedure getbevestigingen()
determ isOproepLine(w2ed)
procedure setw2e(integer No, wedscat Wc, tgnst T, integer SpNo, string Email, integer Tijd, integer Bevestigd)

clauses
getbevestigingen() :-
  retractall(w2e(_,_, _, _, _, _, _, _)),   % 15.9.2009
  providerX(Prov, _, "", _),
  format(URL,"http://%s/oproepcurl3.php", Prov), 
  get_TempDir(_, TempFile),
  deletefile(TempFile),
  AanmeldenVars = aanmeldenvars(),
  _Return = downloadData(URL, TempFile, ["type", "verstuurd"| AanmeldenVars], _HttpResponsecode, 0),
  %file_str(TempFile, String),
  %write(String),
  %dlg_Note("Test",String),
  openread(work, TempFile),
  readdevice(work),
  repeat_f(work),
  readln(Line),
  trap(term_str(w2ed, Term, Line),_,true),
  isOproepLine(Term),
  fail.
getbevestigingen() :-
  logclose(), !,
  closefile(work),
  update_waarsch().

isOproepLine(w2eo(Bev, SpNo, Email, No, Wc, LR, Tijd, _Tgnstx)) :-
  wd(No,Wc,T1,T2,_,_,_),
  getbacktrack(Here),
  w2(No,Wc,Tijd,G1,G2,Fix,Tijd2,Bn), 
  cutbacktrack(Here),
  wd(No,Wc,S1,S2,_,_,_),
  lr(Tgnst, S1, S2, LR),
  setw2e(No,Wc,Tgnst,SpNo, Email, Tijd,Bev),	% de oproep/afzeglijst
  Bev = 1,			% bevestigd
  handleLRGew(G1, G2, Gew1, Gew2,T1,T2,No,Wc,LR,_Tx),			% inclusief handle-was
  not(w2(No,Wc,Tijd,Gew1,Gew2,_,_,_)), 
  logf('A',w2(No,Wc,Tijd,Gew1,Gew2,Fix,Tijd2,Bn),ja), !.
isOproepLine(w2ea(1, SpNo, _Email, No, Wc, _Spx, Tijd, Tgnst)) :-
  ws(No,Wc,Tgnst,Tijd),		% er was eens een planning
  tgnst_num(Tgnst, SpNo),         % Sp = SpNo !!
  %not(w2(No,Wc,_,_,_,_,_,_)),	% nu geen planning
  logf('R', ws(No,Wc,Tgnst,Tijd), ja),
  retractall(w2e(No,Wc,Tgnst,SpNo,_, Tijd,_,_)), !.
isOproepLine(w2ea(0, SpNo, Email, No, Wc, _Spx, Tijd, T)) :-
  ws(No,Wc,T,Tijd),		% er was eens een planning
  not(w2e(No, Wc, T, SpNo, _, Tijd, _, _)),
  assert(w2e(No,Wc,T,SpNo, 0, Tijd,[mt(email,Email,0)],[])), !.

setw2e(No, Wc, T, SpNo, Email, Tijd, 0) :-
  retractall(w2e(No, Wc, T, SpNo, _, _, _, _)),
  assert(w2e(No,Wc, T, SpNo, 0, Tijd, [mt(email,Email,0)], [])), !.
setw2e(No, Wc, T, SpNo, Email, Tijd, 1) :-
  retractall(w2e(No, Wc, T, SpNo, _, _, _, _)),
  assert(w2e(No,Wc, T, SpNo, 0, Tijd, [mt(email,Email,0)], [mt(email,Email,0)])), !.
setw2e(_No, _Wc, _T, _, _, _Tijd, _).

%BEGIN_TLB lifebar, 10:45:52-23.8.2011, Code automatically updated!
/**************************************************************************
	Creation of toolbar: lifebar
**************************************************************************/

clauses

  tb_lifebar_Create(_Parent):-
ifdef use_tbar
	toolbar_create(tb_left,0xC0C0C0,_Parent,
		[separator,
		 separator,
		 separator,
		 tb_ctrl(id_Toernooi_Bestand_open_een_ander,pushb,idb_openw,idb_open_dn,idb_grijs,"Toernooibestanden;Toernooibestandselectie",1,1),
		 separator,
		 separator,
		 separator,
		 tb_ctrl(id_Internet,pushb,idb_taakw,idb_taakrood,idb_taakrood,"Gids;Gids en Stappenplan",1,1)]),
enddef
	true.
%END_TLB lifebar
