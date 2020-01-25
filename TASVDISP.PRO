/*****************************************************************************

		Copyright (c) 1989 - 1998 De Lint Associates

 Project:  TASVPI
 FileName: TASVDISP.PRO
 Purpose: No description
 Written by: JdL
 Comments:
******************************************************************************/

include "tasvp1.inc"
include "tasvp1.con"
include "hlptopic.con"


database - windispl

displayO(WINDOW, wedscat Onderdeel,integer OrigHor,integer OrigVert, integer Cursor)
schaal(WINDOW, integer Hoogte, integer Kolombreedte, integer ClubBreedte, integer Mode, integer Schaal) 
single maxKolLength(integer)
single maxClbLength(integer)
single metCursor(BOOLEAN, integer Id)

predicates

toon_wedst(integer Wedstrijdnummer,wedscat CatNr, WINDOW TopDim, integer Top, integer TopDim, RCT drawrect, RCT drwarect1)
procedure toon_schema(wedscat, WINDOW, RCT, PNT Origin, PNT Offset)
y_coord(rsoort,integer, integer, integer, integer, integer, string, string)
pl(WINDOW, integer OE,integer WedsNo,RCT Drawrect,integer Top, integer TopDim, RCT ClipRct, PNT Origin, PNT Offset, integer Kol)
plaats(WINDOW, integer WedsNo,RCT Drawrect, integer TotaleDimensie, PNT Origin, PNT Offset)
uitsl_win(integer, wedscat, tgnst, tgnst)
wint(integer, wedscat, tgnst)
offset_rij(WINDOW, integer, integer)
afmeting(WINDOW, integer Dimensie, integer Hor, integer Vert, integer Rows, integer Kols, integer KolPap) - (i,i,o,o,o,o,i)
draw_ronde(WINDOW Win, integer TopDim, integer Dim, PNT Origin)
procedure toon_cursor(WINDOW Win, integer Cat)
oe_win(integer OE, tgnst, tgnst, tgnst)
newXY(WINDOW, PNT Origin,RCT DrawRct,integer XN,integer YN, BOOLEAN Blijf) - (i,i,i,o,o,o)
volg(integer, integer, integer, integer, integer, integer, BOOLEAN, BOOLEAN) - (i,i,i,i,i,o,i,o)
spelerkolombreedte(WINDOW, wedscat Onderdeel, integer Size, integer Breedte, integer ClubBreedte, integer Indelingsmode)
set_kol0_pen(WINDOW, integer Kolom, PEN)
geplaatst(Wedscat, integer TopDim, integer NoDim, tgnst , string Tekst, integer WedsNo, integer Offset, color)
plaats_cursor(WINDOW, RCT CursorRct)
display_Invalidate(WINDOW, BOOLEAN Move, RCT OldCursorRect)
beweegUp(integer Card,integer Cursor,integer NewCursor)
beweegDown(integer,integer,integer)
procedure clubKolomBreedte(WINDOW, integer Mode, string Clubs)
procedure indelingsMode(integer Mode, RCT DrawRct, RCT DrawRct00, integer ClubBreedte, WINDOW Win, string Clubs)
procedure metNotitie(tgnst, string, string)
procedure plaatsOfUitslKleur(integer, integer Uitsl, color Kleur)

clauses

metCursor(b_True, 0).

update_display() :-		% niet ingedeeld
  schemaWin(_Win, Cat),
  not(pos(Cat, _, _, _)),
  close_schemas(), !.
update_display() :-		% poule
  schemaWin(_Win, Cat),
  pos(Cat, _, WdNo, poel),
  close_poules(),
  select(Cat, WdNo), !.
update_display() :-		% afval
  schemaWin(Win, Cat),
  pos(Cat, _, _WdNo, _),
  win_Invalidate(Win), !.
update_display().
  
select(0, 0) :-  
  close_schemas(), !.
select(Cat, WdNo) :-
  select(Cat, WdNo, 0, 0, onbekend), !.	% het normale geval

select(Cat, _WdNo, _Mode, _Vooraan, _) :-
  schemaWin(Win, _Cat1),
  not(pos(Cat, _, _, _)),
  win_SetState(Win, [wsf_disabled]),
  win_SetState(Win, [wsf_invisible]),
  fail.
select(Cat, _WdNo, _Mode, _Vooraan, _) :-
  schemaWin(Win, _Cat1),
  pos(Cat, _, _, _),
  win_SetState(Win, [wsf_enabled]),
  win_SetState(Win, [wsf_visible]),
  fail.
select(Cat, WdNo, Mode, _Vooraan, _) :-
  catSelect(Cat),		% synchroniseer het select window
  Parent = vpi_GetTaskWin(),
  pos(Cat, _, _, afval),
  wc(Cat, _,_,  Onderdeel, _, _, _, _, _, _, _, _, _, _, _),
  win_display_window_Create(Parent, Onderdeel, WdNo, Mode), !.
select(Cat, WdNo, _Mode, Vooraan, Tgnst) :-
  Parent = vpi_GetTaskWin(),
  pos(Cat, _, _, poel),
  wc(Cat, _,_,  Onderdeel, _, _, _, _, _, _, _, _, _, _, _),
  win_poule_window_Create(Parent, Onderdeel,WdNo,Vooraan,Tgnst), !.
select(_, _, _, _, _).

setOndNaam(Cat, Naam) :-
  displayO(Win, Cat,_X,_Y,_Cursor),
  win_SetText(Win, Naam), !.
setOndNaam(Cat, Naam) :-
  setPouleNaam(Cat, Naam).

close_schemas() :-
  close_onderdeelselect(),
  retract(schemawin(Win, _)),
  win_Destroy(Win), !.
close_schemas().
    

plaatsText(P, Str) :- !,	% 9.3.2011 alleen extern!
  format(Str, "(%u)",P).

plaatsOfUitslKleur(Plts, 0, color_Red) :-
  Plts > 0, !.
plaatsOfUitslKleur(_, Uitsl, color_Blue) :-
  Uitsl > 0, !.
plaatsOfUitslKleur(_Plts, _Uitsl, color_Black).

geplaatst(Cat, No, No, Tgnst, "", _, _, Kleur) :-
  pos(Cat, _, _, afval),
  opg(_,Cat,Tgnst,0,_,Uitsl,_,_,_,_,_),
  %Plts > 0,
  plaatsOfUitslKleur(0, Uitsl, Kleur), !.
geplaatst(Cat, No, No, Tgnst, Tekst, _, _, Kleur) :-
  pos(Cat, _, _, afval),
  opg(_,Cat,Tgnst,Plts,_,Uitsl,_,_,_,_,_),
  Plts > 0,
  plaatsOfUitslKleur(Plts, Uitsl, Kleur),
  format(Tekst, "(%u)",Plts), !.
geplaatst(Cat, DNo, DNo, onbekend, Plaats, WNo, Off, color_Black) :-
  bitleft(WNo, 1, No1),
  No = No1 + Off,
  kopl(CatVan, rang(Pl), Cat, plaats(No)),
  geplaatstePoule(CatVan, Plaats, Pl), !.
geplaatst(_Cat, _No, _No1, _Tgnst, "", _, _, color_Black).

maxKolLength(0).
maxClbLength(0).	% in indelingsmode doen clubs mee

clubKolomBreedte(Win, 1, Clubs) :-
  openbaar(ja),
  win_GetTextExtent(Win, Clubs, -1, Width, _Height),
  maxClbLength(Max),
  Width > Max,
  assert(maxClbLength(Width)), !.
clubKolomBreedte(_, _, _).

spelerkolombreedte(Win, _CatNo, _, _, _, _Mode) :-
  win_GetTextExtent(Win, "99. -- open plaats --", -1, WidthOnbek0, _),
  assert(maxKolLength(WidthOnbek0)),
  fail.
spelerkolombreedte(Win, CatNo, _, _, _, _Mode) :-
  w2(_No,CatNo,Tijd,_,_,Fix,_,Baan),  			% + planning bekend
  baan_adm(IBaan),
  add_baan(IBaan, Baan, BaanT),
  tijd_kwart(Dag,_,_,Tijd),
  dag(Dag,DStr,_), 
  tijd_str(Tijd,Str1,_),
  trim_c(Dstr),
  format(Str, "%c %s %s %s", Fix, Dstr, Str1, BaanT), 
  win_GetTextExtent(Win, Str, -1, Width, _),
  maxKolLength(Max),
  Width > Max,
  assert(maxKolLength(Width)),
  fail.
spelerkolombreedte(Win, CatNo, _, _, _, _Mode) :-
  getbacktrack(Here),
  kopl(_, _, CatNo, _),
  cutbacktrack(Here),
  win_GetTextExtent(Win, "99.         -- open plaats --       ", -1, WidthOnbek, _),
  assert(maxKolLength(WidthOnbek)),
  fail.
spelerkolombreedte(Win, CatNo, _, _, _, Mode) :-
  assert(maxClbLength(0)),
  opg(_,CatNo,Tgnst,_Plts,_,_,_,_,_,_,_),
  in_weds(CatNo, Tgnst),	% 20.9.2000
  baan_adm(IBaan),
  oproepen(Opr),
  repa(nee,' ',IBaan,Opr,nee,0,2,2,0,CatNo,Tgnst,alleplanning,Naam,_Score,_LogTijd,b_False,b_False,Clubs,_A,_,_,_,_,_),	% en    bepaal de tekst ervoor 
  concat("99.",Naam, Naam1),
  win_GetTextExtent(Win, Naam1, -1, Width, _Height),
  clubKolomBreedte(Win, Mode, Clubs),
  maxKolLength(Max),
  Width > Max,
  assert(maxKolLength(Width)),
  fail.
spelerkolombreedte(Win, _, Height, TotaalBreedte, ClubBreedte, _Mode) :-
  win_GetTextExtent(Win, "Qg", -1, _Width, Height),
  maxKolLength(Breedte),
  maxClbLength(ClubBreedte),
  TotaalBreedte = Breedte + ClubBreedte,  !.

draw_ronde(Win,TopDim, 0, pnt(X, _Y)) :-
  schaal(Win,Letterhoogte, Kolombreedte,_ClubBreedte, _,_),
  X1 = X + (TopDim) * Kolombreedte,
  X2 = X1 + Kolombreedte,
  draw_TextInRect(Win, rct(X1, 0, X2, Letterhoogte),"Finale", -1,[dtext_right, dtext_singleline]),
  !.
draw_ronde(Win, TopDim, 1, pnt(X,_Y)) :-
  schaal(Win,Letterhoogte,Kolombreedte,_ClubBreedte,_,_),
  X1 = X + (TopDim - 1) * Kolombreedte,
  X2 = X1 + Kolombreedte,
  draw_TextInRect(Win, rct(X1, 0, X2, Letterhoogte),"Halve Finale", -1,[dtext_right, dtext_singleline]),
  draw_ronde(Win, TopDim, 0, pnt(X,_Y)), !.
draw_ronde(Win, TopDim, 2, pnt(X,_Y)) :-
  schaal(Win,Letterhoogte, Kolombreedte,_ClubBreedte,_,_),
  X1 = X + (TopDim - 2) * Kolombreedte,
  X2 = X1 + Kolombreedte,
  draw_TextInRect(Win, rct(X1, 0, X2, Letterhoogte),"Kwart Finale", -1,[dtext_right, dtext_singleline]),
  draw_ronde(Win, TopDim, 1, pnt(X,_Y)), !.
draw_ronde(Win, TopDim, N, pnt(X,_Y)) :-
  schaal(Win,Letterhoogte, Kolombreedte,_ClubBreedte,_,_),
  X1 = X + (TopDim - N) * Kolombreedte,
  X2 = X1 + Kolombreedte,
  Ronde = TopDim - N + 1,
  N1 = N - 1,
  format(Text, "Ronde %d",Ronde),
  draw_TextInRect(Win, rct(X1, 0, X2, Letterhoogte),Text, -1,[dtext_right, dtext_singleline]),
  draw_ronde(Win, TopDim, N1, pnt(X,_Y)), !.

set_kol0_pen(Win, 0, PenKol0) :-
  win_SetPen(Win, PenKol0), !.
set_kol0_pen(_, _, _).

toon_schema(Cat,Win,ClipRct,Origin,Offset) :-			% Vertoon schema
  pos(Cat, Top, _, _),
  dim(Top, TopDim),
  Pen1 = pen(1,ps_Solid, color_Red),
  win_SetPen(Win, Pen1),
  Pen = pen(1,ps_Solid, color_Blue),
  win_SetPen(Win, Pen),
  PenKol0 = pen(2,ps_Solid, color_Blue),
  wd(WNum,Cat,_, _,_,_,_),					% voor alle wedstrijden in schema
  pl(Win,0,WNum,DrawRct0,Top,TopDim, ClipRct,Origin,Offset,Kol),% bepaal exacte locatie v.d. ene
  pl(Win,1,WNum,DrawRct1,Top,TopDim, ClipRct,Origin,Offset,_),  % bepaal exacte locatie v.d. andere 
  DrawRct0 = rct(X0, _Y0, X1, Y1),
  DrawRct1 = rct(X2, _Y2, X3, Y3),
  draw_Polyline(Win, [pnt(X0, Y1), pnt(X1,Y1), pnt(X3,Y3)]),
  set_kol0_pen(Win, Kol, PenKol0),
  draw_Line(Win, pnt(X3,Y3),pnt(X2,Y3)),	% de dikke lijn in eerste kolom
  win_SetPen(Win, Pen),
  toon_wedst(WNum, Cat, Win, Top, TopDim, DrawRct0,DrawRct1),
  fail.						% herhaal voor alle wedstrijden
toon_schema(Cat,Win,ClipRct,Origin,Offset) :-	% Vertoon finale
  pos(Cat, _Top, _, _),
  dim(_Top, TopDim),
  Pen = pen(1,ps_Solid, color_Blue),
  win_SetPen(Win, Pen),
  pl(Win,0,0,DrawRct0,_Top,TopDim, ClipRct,Origin,Offset,_Kol),% bepaal exacte locatie v.d. ene 
  DrawRct0 = rct(X0, _Y0, X1, Y1),
  draw_Line(Win, pnt(X0,Y1),pnt(X1,Y1)),	% de dikke lijn in eerste kolom
  toon_wedst(0, Cat, Win, _Top, TopDim, DrawRct0,DrawRct0),
  fail.						% herhaal voor alle wedstrijden
toon_schema(Cat,Win,ClipRct,Origin,pnt(OffX,_OffY)) :-
  OffX = 0,
  schaal(Win,Letterhoogte, _Kolombreedte,_ClubBreedte,_,_),
  pos(Cat, Top, _, _),			% 
  dim(Top, TopDim),
  SaveTools = win_GetDrawTools(Win),
  SaveTools = draw_tools(_Pen, _Brush, _Drwamode, _Font, _Foreground, _BackGround, _BK_MODE),
  win_SetForeColor(Win, color_White),
  win_SetBackColor(Win, color_Blue),
  win_SetBackMode(Win, bk_Opaque),
  win_SetBrush(Win,brush(pat_Solid, color_Blue)),
  ClipRct = rct(_X0, _Y0, _X1, _Y1),
  draw_Rect(Win,rct(0, 0, _X1, Letterhoogte)),
  draw_ronde(Win, TopDim, TopDim, Origin),
  win_SetForeColor(Win, _Foreground),
  win_SetBackColor(Win, _Background),
  win_SetBackMode(Win,_BK_MODE),
  fail.
toon_schema(_Cat,_Win,_ClipRct,_Origin,pnt(OffX,_OffY)):-
  OffX = 0, 
  metCursor(b_True, _), !,
  toon_cursor(_Win, _Cat).
toon_schema(_Cat,_Win,_ClipRct,_Origin,_).

display_Invalidate(Win, b_False, OldCursorRect):-
  win_Invalidate(Win, OldCursorRect),
  plaats_cursor(Win, NewRct),
  win_Invalidate(Win, NewRct), !.
display_Invalidate(Win, _, _):-
  ClipRct1 = win_GetClientRect(Win),
  toolbar_GetRect(Win,tb_bottom,TbRCT),
  TbRCT = rct(_, YC1, _, _B1),
  ClipRct1 = rct(XC0, YC0, XC1, _YC),
  %YC1 = YC - B1 + T1,
  ClipRct = rct(XC0, YC0, XC1, YC1),
  win_Invalidate(Win, ClipRct), !. 

plaats_cursor(Win, CursorRct) :-
  displayO(Win, CatNo,_X,_Y,Cursor),
  Origin = pnt(_X, _Y),
  pos(CatNo, Top, _, _), !,  		% 
  dim(Top, TopDim),
  plaats(Win, Cursor,CursorRct,TopDim, Origin,pnt(0,0)).	% Bepaal de exacte locatie
  
toon_cursor(_Win, _Cat) :-  
  displayO(_Win, _CatNo,_X,_Y,Cursor),
  Origin = pnt(_X, _Y),
  pos(_Cat, _Top, _, afval ),  		% 
  dim(_Top, TopDim),
  TopDim1 = TopDim + 1,
  dim(Cursor,NoDim),                  	% bepaal de ronde
  bitand(Cursor,1,OE),
  bitright(Cursor, 1, Cursor1),
  uitsl_win(Cursor1, _Cat, S1, S2),     % 
  oe_win(OE,S1,S2,S3),
  CursorDim = Nodim ,
  baan_adm(IBaan),
  oproepen(Opr),
  schaal(_Win,_Letterhoogte, _Kolombreedte,ClubBreedte,Mode,_),
  repa(nee,' ',IBaan,Opr,nee,OE,TopDim1,CursorDim,Cursor1,_Cat,S3,alleplanning,Naam,_,_Log,b_False,b_False,Clubs,_Anker,_,_,_,_,_Noti),% en    bepaal de tekst ervoor 
  plaats(_Win, Cursor,DrawRct,TopDim, Origin,pnt(0,0)),	% Bepaal de exacte locatie
  y_coord(afval,Cursor1, _Top, TopDim1, NoDim, OE, Naam, NaamPlus), %
  indelingsMode(Mode, DrawRct, DrawRct00, ClubBreedte, _Win, Clubs),
  SaveTools = win_GetDrawTools(_Win),
  SaveTools = draw_tools(_Pen, _Brush, _Drwamode, _Font, _Foreground, _BackGround, _BK_MODE),
  win_SetForeColor(_Win, color_White),
  win_SetBackColor(_Win, color_Blue), 	% 8.11.2010
  win_SetBackMode(_Win, bk_Opaque),
  win_SetBrush(_Win,brush(pat_Solid, color_Blue)),	% 8.11.2010
  draw_Rect(_Win,DrawRct00),
  draw_TextInRect(_Win, DrawRct00, NaamPlus, -1, [dtext_bottom, dtext_singleline]),
  win_SetForeColor(_Win, _Foreground),
  win_SetBackColor(_Win, _Background),
  win_SetBackMode(_Win,_BK_MODE),!.
toon_cursor(_,_).

plaats(Win,1,DrawRct,TopDim,pnt(OriginX,OriginY),pnt(OffX,OffY)) :-
  TopDim >= 2,
  schaal(Win, Letterhoogte, Kolombreedte,_ClubBreedte,_,_),	% plaats voor wedstrijd 1
  Kolom = TopDim + 1,
  K = (Kolom - 1) * Kolombreedte + OriginX + OffX,
  offset_rij(Win,Kolom, Offs),
  R = Offs + OriginY + OffY - 9,
  KolRight  = K + Kolombreedte,
  RowBottom = R + Letterhoogte,
  DrawRct = rct(K,R,KolRight,RowBottom),
  !.
plaats(Win,1,DrawRct,TopDim,pnt(OriginX,OriginY),pnt(OffX,OffY)) :-
  TopDim < 2,		% niet vouwen
  schaal(Win, Letterhoogte, Kolombreedte,_ClubBreedte,_,_),	% plaats voor wedstrijd 1
  Kolom = TopDim + 2,
  Kolom1 = TopDim + 1,
  K = (Kolom - 1) * Kolombreedte + OriginX + OffX,
  offset_rij(Win,Kolom1, Offs),
  R = Offs + OriginY + OffY,
  KolRight  = K + Kolombreedte,
  RowBottom = R + Letterhoogte,
  DrawRct = rct(K,R,KolRight,RowBottom),
  !.
plaats(Win,No,DrawRct,TopDim,pnt(OriginX,OriginY),pnt(OffX,OffY)) :-
  schaal(Win, Letterhoogte, Kolombreedte,_ClubBreedte,_,_),	% plaats van de andere wedstrijdnummers
  dim(No,Dim),                  
  Kolom1 = TopDim - Dim + 1,
  grootste(Kolom1, 0, Kolom), 
  card(No,KolTop),
  Row = No - KolTop, 
  K = Kolom * Kolombreedte + OriginX + OffX,
  bitleft(1,Kolom,D2delt),
  offset_rij(Win,Kolom, Offs),
  R = Offs + Row * D2delt * Letterhoogte + OriginY + OffY,
  KolRight  = K + Kolombreedte,
  RowBottom = R + Letterhoogte,
  DrawRct = rct(K,R,KolRight,RowBottom),
  !.
plaats(_, _No,_DrawRct,_TopDim,_,_) :-
  fail.

indelingsMode(1, DrawRct, DrawRct00, ClubBreedte, Win, Clubs) :-
  openbaar(ja), !,		% en openbaar
  DrawRct = rct(L,T,R,B),
  L1 = L + ClubBreedte,
  L2 = L1 + 3,
  DrawRct00 = rct(L2,T,R,B),
  DrawRct1 = rct(L,T,L1,B),
  draw_TextInRect(Win, DrawRct1, Clubs, -1, [dtext_bottom, dtext_singleline, dtext_left]).
indelingsMode(_, DrawRct00, DrawRct00, _ClubBreedte, _Win, _Clubs).

metNotitie(onbekend, Notitie, Notitie) :- !.
metNotitie(_, _, "").
  
toon_wedst(0, Cat, Win, _Top, _, DrawRct0,_) :-        	% eindronde
  w2(1,Cat,Tijd,_,_,Fix,_,Baan),  			% + planning bekend
  wd(1,Cat,_,_,Score,_,Noti),
  baan_adm(IBaan),
  add_baan(IBaan, Baan, BaanT),
  tijd_kwart(Dag,_,_,Tijd),
  dag(Dag,DStr,_), !,
  tijd_str(Tijd,Str1,_),
  schaal(Win,Letterhoogte, _Kolombreedte,_ClubBreedte,_,_),
  format(Str, "}%c %s %s %s", Fix, Dstr, Str1, BaanT), 
  draw_TextInRect(Win, DrawRct0, Str, -1, [dtext_bottom, dtext_singleline, dtext_left]),
  DrawRct0 = rct(X0, _Y0, X1, Y1),
  Y11 = Y1 - 3,
  Y2 = Y11 + Letterhoogte + 2,
  DrawRct = rct(X0, Y11, X1, Y2),
  format(OnderDeLijn, "%s%s",Score,Noti),
  fronttoken(OnderDeLijn, _, _),
  win_SetForeColor(Win, color_Red),
  draw_TextInRect(Win, DrawRct, OnderDeLijn, -1, [dtext_singleline, dtext_bottom, dtext_center]),
  win_SetForeColor(Win, color_Black), !.
toon_wedst(0, Cat, Win, _Top, _, DrawRct0, _DrawRct1) :-	% eindronde 
  w3(1,Cat,Tgnst,_,_,_,_), !,						% winnaar bekend
  baan_adm(IBaan),
  oproepen(Opr),
  repa(nee,' ',IBaan,Opr,nee,1,0,1,0,Cat,Tgnst,alleplanning,Naam,Score,_Log,b_False,b_False,_Clubs,_Anker,_,_,_,_,_),
  draw_TextInRect(Win, DrawRct0, Naam, -1, [dtext_bottom, dtext_singleline, dtext_left]),
  schaal(Win,Letterhoogte, _Kolombreedte,_ClubBreedte,_,_),
  DrawRct0 = rct(X0, _Y0, X1, Y1),
  Y11 = Y1 - 3,
  Y2 = Y11 + Letterhoogte + 2,
  DrawRct = rct(X0, Y11, X1, Y2),
  draw_TextInRect(Win, DrawRct, Score, -1, [dtext_singleline, dtext_bottom, dtext_center]), !.
toon_wedst(0, Cat, _Win, _Top, _, DrawRct0, _DrawRct1) :- 	% eindronde nog open
  wd(1,Cat,_,_,Score,_,Noti),
  draw_TextInRect(_Win, DrawRct0, "", -1, [dtext_bottom, dtext_singleline, dtext_right]),
  schaal(Win,Letterhoogte, _Kolombreedte,_ClubBreedte,_,_),
  DrawRct0 = rct(X0, _Y0, X1, Y1),
  Y11 = Y1 - 3,
  Y2 = Y11 + Letterhoogte + 2,
  DrawRct = rct(X0, Y11, X1, Y2),
  win_SetForeColor(Win, color_Red),
  format(OnderDeLijn, "%s %s", Score, Noti),
  trim_c(OnderDeLijn),
  draw_TextInRect(Win, DrawRct, OnderDeLijn, -1, [dtext_singleline, dtext_bottom, dtext_center]),
  win_SetForeColor(Win, color_Black), !.
toon_wedst(No, Cat, Win, Top, TopDim, DrawRct0, _DrawRct1) :-	% gewone wedstrijd
  uitsl_win(No, Cat, S1, _S2),     				% 
  dim(No,NoDim),                  				% bepaal de ronde
  baan_adm(IBaan),
  oproepen(Opr),
  repa(nee,' ',IBaan,Opr,nee,0,TopDim,NoDim,No,Cat,S1,alleplanning,Naam,Score,_LogTijd,b_False,b_False,Clubs,_Anker,_,_,_,_,Noti),	% en    bepaal de tekst ervoor 
  schaal(Win,Letterhoogte, _Kolombreedte,ClubBreedte,Mode,_),
  y_coord(afval,No, Top, TopDim, NoDim, 0, Naam, NaamPlus), 	% tellertje voor eerste kolom
  indelingsMode(Mode, DrawRct0, DrawRct00, ClubBreedte, Win, Clubs),
  geplaatst(Cat,TopDim, NoDim, S1, Geplaatst, No, 0, Kleur),
  win_SetForeColor(Win, Kleur),
  draw_TextInRect(Win, DrawRct00, NaamPlus, -1, [dtext_singleline, dtext_bottom]),
  win_SetBackMode(Win, bk_Opaque),
  draw_TextInRect(Win, DrawRct00, Geplaatst, -1, [dtext_singleline, dtext_bottom, dtext_right]),
  win_SetBackMode(Win, bk_Transparent),
  win_SetForeColor(Win, color_Black),
  metNotitie(S1, Noti, Noti1),
  format(OnderDeLijn, "%s%s",Score,Noti1),
  fronttoken(OnderDeLijn, _, _),
  DrawRct00 = rct(X0, _Y0, X1, Y1),
  Y11 = Y1 - 3,
  Y2 = Y11 + Letterhoogte + 2,
  DrawRct = rct(X0, Y11, X1, Y2),
  win_SetForeColor(Win, color_Red),
  Font = win_GetFont(Win),
  _FontName = font_GetAttrs(Font, Style, Size),
  Siz1 = Size - 1,
  NewFont = font_SetAttrs(Font, Style, Siz1),
  win_SetFont(Win, NewFont),
  draw_TextInRect(Win, DrawRct, OnderDeLijn, -1, [dtext_singleline, dtext_bottom, dtext_center]),
  win_SetFont(Win, Font),
  win_SetForeColor(Win, color_Black),
  fail.
toon_wedst(No, Cat, Win, Top, TopDim, _DrawRct0, DrawRct1) :-	% gewone wedstrijd
  dim(No,NoDim),                  				% bepaal de ronde
  uitsl_win(No, Cat, _S1, S2),     				% 
  baan_adm(IBaan),
  oproepen(Opr),
  repa(nee,' ',IBaan,Opr,nee,1,TopDim,NoDim,No,Cat,S2,alleplanning,Naam1,Score,_LogTijd,b_False,b_False,Clubs,_Anker,_,_,_,_,Noti),%  bepaal tekst 
  y_coord(afval, No, Top, TopDim, NoDim, 1, Naam1, Naam1Plus), 	% regelnummers!  
  schaal(Win,Letterhoogte, _Kolombreedte,ClubBreedte,Mode,_),
  indelingsMode(Mode, DrawRct1, DrawRct11, ClubBreedte, Win, Clubs),
  geplaatst(Cat,TopDim, NoDim, S2, Geplaatst, No, 1, Kleur),
  win_SetForeColor(Win, Kleur),
  draw_TextInRect(Win, DrawRct11, Naam1Plus, -1, [dtext_singleline, dtext_bottom]),
  win_SetBackMode(Win, bk_Opaque),
  draw_TextInRect(Win, DrawRct11, Geplaatst, -1, [dtext_singleline, dtext_bottom, dtext_right]),
  win_SetForeColor(Win, color_Black),
  win_SetBackMode(Win, bk_Transparent),
  metNotitie(S2, Noti, Noti1),
  format(OnderDeLijn, "%s%s",Score,Noti1),
  OnderDeLijn <> "",
  DrawRct11 = rct(X0, _Y0, X1, Y1),
  Y11 = Y1 - 3,
  Y2 = Y11 + Letterhoogte + 2,
  DrawRct = rct(X0, Y11, X1, Y2),
  win_SetForeColor(Win, color_Red),
  Font = win_GetFont(Win),
  _FontName = font_GetAttrs(Font, Style, Size),
  Siz1 = Size - 1,
  NewFont = font_SetAttrs(Font, Style, Siz1),
  win_SetFont(Win, NewFont),
  draw_TextInRect(Win, DrawRct, OnderDeLijn, -1, [dtext_singleline, dtext_bottom, dtext_center]),
  win_SetFont(Win, Font),
  win_SetForeColor(Win, color_Black),
  !.

pl(Win,0,0,DrawRct,_Top,TopDim,  _ClipRct,Origin,pnt(OffX,OffY), Kol) :-
  schaal(Win,Letterhoogte, Kolombreedte,_ClubBreedte,_,_),% het geval No = 0
  Origin = pnt(OriginX,OriginY),
  TopDim >= 2,
  Kol = TopDim,           
  K = Kol * Kolombreedte + OriginX + OffX,
  Kol1 = Kol + 1,
  offset_rij(Win,Kol1, Offs),
  R = Offs + OriginY + OffY - 9,
  KolRight  = K + Kolombreedte, % - 10
  RowBottom = R + Letterhoogte,
  DrawRct = rct(K,R,KolRight,RowBottom), !.
pl(Win,0,0,DrawRct,_Top,TopDim,  _ClipRct,Origin,pnt(OffX,OffY), Kol) :-
  schaal(Win,Letterhoogte, Kolombreedte,_ClubBreedte,_,_),% het geval No = 0
  Origin = pnt(OriginX,OriginY),
  TopDim < 2,	% 
  Kol = TopDim + 1,           
  K = Kol * Kolombreedte + OriginX + OffX,
  Kol1 = Kol,
  offset_rij(Win,Kol1, Offs),
  R = Offs + OriginY + OffY,
  KolRight  = K + Kolombreedte, % - 10
  RowBottom = R + Letterhoogte,
  DrawRct = rct(K,R,KolRight,RowBottom),  !.
pl(Win,OE,No,DrawRct,Top,TopDim,  _ClipRct,Origin,pnt(OffX,OffY), Kol) :-   
  No <> 0,				% OE     - opg 0 of 1 (oneven/even)  
  schaal(Win,Letterhoogte, Kolombreedte,_ClubBreedte,_,_),
  Origin = pnt(OriginX,OriginY),
  dim(No,Dim),                  	% No     - wedstrijdnummer 
  Kol = TopDim - Dim,           	% R      - rij waar tekst moet komen
  Kol >= 0,
  bitright(Top,Kol,KolTop),     	% K      - kolom "    "    "     "   
  Row = 2 * (No - KolTop) + OE, 	% Top    - wedstno van links boven 
  K = Kol * Kolombreedte + OriginX + OffX,	% TopDim - 'dimensie' van de top
  bitleft(1,Kol,D2delt),
  offset_rij(Win,Kol, Offs),
  R = Offs + Row * D2delt * Letterhoogte + OriginY + OffY,
  KolRight  = K + Kolombreedte, % - 10
  RowBottom = R + Letterhoogte,
  DrawRct = rct(K,R,KolRight,RowBottom), !.

offset_rij(_, 0, 0) :- !.
offset_rij(Win, 1, Offs) :- !,
  schaal(Win, Letterhoogte, _Kolombreedte,_ClubBreedte,_,_),!,
  Offs = Letterhoogte div 2.
offset_rij(Win, 2, Offs) :- !,
  schaal(Win,Letterhoogte, _Kolombreedte,_ClubBreedte,_,_),!,
  Offs = Letterhoogte div 2 + Letterhoogte.
offset_rij(Win, Kol, Offs) :-
  schaal(Win, Letterhoogte, _Kolombreedte,_ClubBreedte,_,_),!,
  Kol1 = Kol - 1 ,
  Kol1 >= 0,
  bitleft(1,Kol1,Offs1),
  Offs = Offs1 * Letterhoogte.

y_coord(afval, WdNo, Card, Dim, Dim, Offs, Naam, NaamUit) :- !,
  Coord = 2 * (WdNo - Card) + Offs +1,
  format(NaamUit, "%d.%s", Coord, Naam).
y_coord(_, _, _, _, _, _, Naam, Naam1):-
  concat(" ", Naam, Naam1). 

uitsl_win(0, Cat, S1, S1) :-
  w3(1, Cat, S1, _, _, _, _), !. 
uitsl_win(0, _, onbekend, onbekend) :- !.
uitsl_win(No, Cat, S1, S2) :-
  wd(No,Cat,S1,S2,_,_,_), !.
uitsl_win(No, Cat, S1, S2) :-
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

oe_win(0, X, _, X) :- !.
oe_win(1, _, Y, Y).

volg(CI0, CI1, DI0, DI1, Orig, Orig, Blijft, Blijft) :-	% geen verandering
  CI0 <= DI0,
  CI1 >= DI1, !. 
volg(CI0, _CI1, DI0, _DI1, Orig, New, _, b_True) :-	% invalidate nodig
  CI0 >= DI0,
  New = Orig + CI0 - DI0, !. 
volg(_CI0, CI1, _DI0, DI1, Orig, New, _, b_True) :-
  CI1 <= DI1,
  New = Orig - DI1 + CI1, !. 

newXY(Win,Origin,DrawRct,XN,YN, Blijf1) :-
  ClipRct1 = win_GetClientRect(Win),
  toolbar_GetRect(Win,tb_bottom,TbRCT),
  TbRCT = rct(_, YC1, _, _B1),
  ClipRct1 = rct(XC0, YC0, XC1, _YC),	% toolbar = bottom
  schaal(Win,Letterhoogte, _Kolombreedte,_ClubBreedte,_,_),
  Origin = pnt(X, Y),
  DrawRct = rct(XD0, YD0, XD1, YD1),
  volg(XC0, XC1, XD0, XD1, X, XN, b_False, Blijf),	% horizontaal
  YC00 = YC0 + Letterhoogte + Letterhoogte div 2,	% 22.9.1999
  YC10 = YC1 - Letterhoogte div 2,			% 22.9.1999
  volg(YC00, YC10, YD0, YD1, Y, YN, Blijf, Blijf1), !.	% verticaal

afmeting(Win,Top, Hor, Vert, Rows, Kols, PapKols) :-
  schaal(Win,Letterhoogte, Kolombreedte,_ClubBreedte,_,_),
  dim(Top, TopDim), 
  TopDim < PapKols , !,	% de kleine worden niet gevouwen (was 3)
  Kols = TopDim + 2,
  Hor = Kols * Kolombreedte,
  Rows = Top * 2,
  Vert = (Rows + 1) * Letterhoogte.
afmeting(Win,Top, Hor, Vert, Rows, TopDim, _) :-
  schaal(Win,Letterhoogte, Kolombreedte,_ClubBreedte,_,_), !,
  dim(Top, TopDim),
  Kols = TopDim + 1, 
  Hor = Kols * Kolombreedte,
  Rows = Top * 2,
  Vert = (Rows + 1) * Letterhoogte.

beweegUp(1, 1, 2) :- !.
beweegUp(2, 3, 1) :- !.
beweegUp(Card, Cursor, Cursor1) :-
  Cursor > Card,
  Cursor1 = Cursor - 1, !.

beweegDown(1, 1, 3) :- !.
beweegDown(2, 2, 1) :- !.
beweegDown(Card,Cursor,Cursor1):-
  Cursor < Card * 2 - 1,
  Cursor1 = Cursor + 1, !.
	
%BEGIN_WIN Display window
/**************************************************************************
        Creation and event handling for window: Display window
**************************************************************************/

CONSTANTS
%BEGIN Display window, CreateParms, 16:42:39-24.7.2011, Code automatically updated!
  win_display_window_WinType = w_TopLevel
  win_display_window_Flags = [wsf_SizeBorder,wsf_TitleBar,wsf_Maximize,wsf_Close,wsf_ClipSiblings]
  win_display_window_RCT = rct(100,80,727,453)
  win_display_window_Menu = no_menu
  win_display_window_Title = "Display window"
  win_display_window_Help = idh_afvalschema
%END Display window, CreateParms
id_waarschuwenp1     = 600
id_waarschuwenp2     = 601
id_waarschuwenp3     = 602
id_waarschuwenp4     = 603
id_wijzigOpst	     = 604
id_koppel            = 605
id_koppelleg         = 606
id_koppelovr         = 607
id_koppelwis         = 608
id_wijzig_bye          = 620
id_wijzig_nietinged    = 621
id_wijzig_bestaand     = 622
id_wijzig_gereserveerd = 623
id_wijzig_koppel       = 624
id_wijzig_toevoegen    = 625
id_wijzig_nietbestaand     = 626

PREDICATES

  win_display_window_eh : EHANDLER
  adapt(WINDOW, RCT ClRct, RCT TaskRct, integer HorzAfm, integer VertAfm, integer Letterhoogte) - (i,i,i,i,i,i)
  cursorIgvMode(wedscat, integer, integer, integer)
  wissel(integer, tgnst, tgnst, tgnst)
  enableWijzigBye(tgnst, BOOLEAN)
  enableWijzigNIn(wedscat, BOOLEAN)
  enableWijzigGer(WINDOW, tgnst, BOOLEAN)
  enableBestaand(wedscat, BOOLEAN)
  enableWijzigKopl(wedscat Cat,boolean)
nondeterm overblijvende_opgave(wedscat,tgnst, integer, string)
  titel(integer, string, string)
  afval_init(WINDOW Previous, WINDOW Current, integer FontSize, integer Mode)
  gewonePop(integer Mode, WINDOW)
  determ kanTerug(WINDOW)
  select_Toolbar(WINDOW, integer Mode)
determ	alleOptiesOnmogelijk(BOOLEAN PlanB, BOOLEAN Inv, BOOLEAN PlanW, BOOLEAN Beeld, BOOLEAN Bnb)
procedure koppelingen(window, BOOLEAN)

CLAUSES

alleOptiesOnmogelijk(b_False, b_False, b_False, b_False, b_False).

eindrondeVan(Cat, Cat).

enableWijzigBye(bye, b_False) :- !.
enableWijzigBye(_, b_True) :- !.

overblijvende_opgave(Cat,Tgnst, 1, Naam) :-
  opg(_,Cat,Tgnst,_,_,_,_,_,_,_,_),
  not(wd(_,Cat,Tgnst,_,_,_,_)),
  not(wd(_,Cat,_,Tgnst,_,_,_)),
  tgnst_naam(nee, b_False, Cat, Tgnst, Naam, _).

enableWijzigNIn(Cat, b_True) :-
  findall(Naam,overblijvende_opgave(Cat,_, 1, Naam),Namen),
  not(Namen = []), !.
enableWijzigNIn(_, b_False).

enableWijzigGer(_, _, b_True) :- !.

enableBestaand(Cat, b_True) :-
  opg(_,Cat,_Tgnst,_,_,_,_,_,_,_,_), !.
enableBestaand(_Cat, b_False).

enableWijzigKopl(Cat,b_False) :-
  kopl(Cat, verliezer, _, _), !.
enableWijzigKopl(_Cat,b_True).

cursorIgvMode(CatNo, Cursor, 0, Top) :- 
  pos(CatNo, Top, _, _),
  Cursor >= 2 * Top, !.
cursorIgvMode(_, Cursor, 0, Cursor) :- !.
cursorIgvMode(CatNo, _, _Mode, NCursor) :-
  pos(CatNo,Card,_,_),
  NCursor = 2 * Card, !.

koppelingen(_Win, b_True) :-
  displayO(_Win, Cat, _XN,_YN, _Num),
  koplv(Cat, _CatN), !.
koppelingen(_Win, b_True) :-
  displayO(_Win, Cat, _XN,_YN, _Num),
  kopl(Cat, pl3en4(_), _CatX, _No), !.
koppelingen(_, b_False).

gewonePop(0, _Win) :-
  displayO(_Win, Cat, _XN,_YN, Num),
  wd(Num,Cat,T1, T2,_,_,_), 		% wedstrijd bestaat
  planbaar(Num, Cat, PlanB),
  invoerbaar(Num, Cat, Inv),
  wisplanbaar(Num, Cat, PlanW),
  beeldbaar(Num, Cat, Beeld),
  baanbaar(Num, Cat, Bnb),
  koppelingen(_Win, Koppelb),
  wd(Num,Cat,T1, T2,_,_,_), 
  Menu = dyn_menu([
    txt(id_Wedstrijd_plan,"&Plan\tF5",0,PlanB,0,[]),
    txt(id_Wedstrijd_wis_plan,"&Wis plan",0,PlanW,0,[]),
    txt(id_Wedstrijd_uitslag_invoeren,"&Uitslag invoeren\tF9",0,Inv,0,[]),
    txt(idr_baannummer,"&Locatietoewijzing", 0, Bnb, 0, []),
    txt(id_Speler_lijsten,"Deelnemers in &lijst", 0, b_True, 0, []),
    separator,
    txt(id_Wedstrijd_overzicht,"Wedstrijd&overzicht\tF7",0,Beeld,0,[]),
    txt(idb_koplg,"Door naar ..",0,Koppelb,0,[]),
    separator(),
    txt(idr_copy,"&Kopiëren\tCtrl-C",0,b_True,0,[]),
    txt(lettergrootte,"&Lettergrootte",0,b_True,0,[])
    ]),
  plaats_Cursor(_Win, CursorRect),
  CursorRect = rct(L,T,R,B),
  X1 = (L + R) div 2, Y1 = (T + B) div 2, !,
  menu_PopUp(_Win,Menu, pnt(X1, Y1), align_Right).
gewonePop(0, _Win) :-
  displayO(_Win, Cat, _XN,_YN, Num),
  kopl(VCat, rang(Nr), Cat, plaats(Num)),
  wc(VCat, _,_,  Onderdeel, _, _, _, _, _, _, _, _, _, _, _),
  format(Tekst, "Nr % van &Voorronde %.", Nr, Onderdeel), 
  Menu = dyn_menu([
    separator,
    txt(idt_Terug,Tekst,0,b_True,0,[]),
    separator
    ]), !,
  plaats_Cursor(_Win, CursorRect),
  CursorRect = rct(L,T,R,B),
  X1 = (L + R) div 2, Y1 = (T + B) div 2,
  menu_PopUp(_Win,Menu, pnt(X1, Y1), align_Right).
gewonePop(1, _Win) :-
  displayO(_Win, Cat, _XN,_YN, Cursor),
  pos(Cat, Top, _, _), 
  Cursor >= 2 * Top,		% alleen kolom 1
  bitright(Cursor,1,Num),
  bitand(Cursor,1,LR),
  Helft = LR + 1,
  wd(Num,Cat,T1,T2,_,_,_),
  wissel(Helft, T1, T2, TOld),
  enableWijzigBye(TOld, VBye),
  enableWijzigNIn(Cat, VNin),
  enableWijzigGer(_Win, TOld, VGer),
  enableWijzigKopl(Cat,VKop),
  enableBestaand(Cat, VBest),
  Menu = dyn_menu([
    separator,
    txt(id_Wedstrijd_plan,"&Wijzig in/verwissel met",0, b_True, 0,
    [
      txt(id_wijzig_bye,         "&Bye",0, VBye, 0, []),
      txt(id_wijzig_nietinged,   "(nog) &Niet ingedeeld\tIns",0, VNin, 0, []),
      txt(id_wijzig_bestaand,    "&Bestaande opgave",0, Vbest, 0, []),
      txt(id_wijzig_nietbestaand,"Nieuwe &speler/koppel",0, b_True, 0, []),
      txt(id_wijzig_gereserveerd,"&Gereserveerde plaats\tDel",0, VGer, 0, []),
      txt(id_wijzig_koppel,      "&Koppeling",0, VKop, 0, []),
      txt(id_wijzig_toevoegen,   "&Toevoegen",0, b_True, 0, [])
    ]),
    separator]), !,
  plaats_Cursor(_Win, CursorRect),
  CursorRect = rct(L,_T,R,B),
  X1 = (L + R) div 2, 
  menu_PopUp(_Win,Menu, pnt(X1, B), align_Right).

wissel(1, T1, _, T1) :- !.
wissel(2, _, T2, T2) :- !.

adapt(Win, ClRct, TaskRct, Horz, Vert, Letterhoogte) :-
  TaskRct = rct(_,_,Tx1,Ty),
  Tx = Tx1 - 22,		%taskbar!
  ClRct = rct(_X0, _Y0, X, Y),
  RCT = win_GetOuterRect(Win),
  ClientRct = rect_GetClient(win_display_window_Flags, b_False, RCT), 
  Xdelta1 = (Horz - X) div 2 + 2,
  Xdelta2 = (Tx - X - 20) div 2,
  kleinste(Xdelta1, Xdelta2, Xdelta),
  Ydelta1 = (Vert - Y ) div 2 + 2,
  Ydelta2 = ((Ty - Letterhoogte * 2 - 40) - Y) div 2,
  kleinste(Ydelta1, Ydelta2, Ydelta),
  NewClient = rect_Inflate(ClientRct, Xdelta, Ydelta),
  NewClient = rct(Xn, Yn, _, _),
  %newXnCl(Xn, Xoff),
  %newXnCl(Yn, Yoff),		% zelfde voor Y
  Xoff = 10 - Xn,
  Yoff = 30 - Yn,		% menubalk!
  NewClient1 = rect_Offset(NewClient, Xoff, Yoff), 
  win_Move(Win, NewClient1), !. 
adapt(_Win, rct(_, _, _X, _Y), rct(_,_,_Tx,_Ty), _Horz, _Vert, _).

titel(0, Onderdeel, Onderdeel) :- !.		% afhankelijk van Mode
titel(1, Onderdeel, Titel) :-
  format(Titel, "%s - wijziging in opstelling mogelijk", Onderdeel), !.

afval_init(NewWin, Win, FontSize, Mode) :-
  NewFont = font_Create(ff_Helvetica, [], FontSize),
  win_SetFont(Win, NewFont),
  %win_GetFontMetrics(Win, _Leading, _Ascent, _Descent),
  retract(displayO(NewWin, CatNo,OrigX,_,Cursor)), !,
  retract(schaal(NewWin, _, _, _, Mode, _Sch)),
  spelerkolombreedte(Win, CatNo, Letterhoogte, Kolombreedte,ClubBreedte,Mode),
  %Letterhoogte = _Ascent + 2,
  assert(schaal(Win, Letterhoogte, Kolombreedte,ClubBreedte, Mode, FontSize)),
  cursorIgvMode(CatNo, Cursor, Mode, NCursor),
  pos(CatNo, Top, _, _),  		% 
  dim(Top, TopDim),
  afmeting(Win, Top, Horz, Vert1, _Rows, _Kols, 2),
  ClientRct = win_GetClientRect(Win),
  TaskWin = vpi_GetTaskWin(),
  TaskRct = win_GetClientRect(TaskWin),
  toolbar_GetRect(Win,tb_bottom,TbRCT),
  TbRCT = rct(_, T, _, B),
  Vert = Vert1 + B - T,
  adapt(Win, ClientRct, TaskRct, Horz, Vert, Letterhoogte),
  Origin = pnt(OrigX, Letterhoogte),
  plaats(Win,NCursor,DrawRct,TopDim, Origin,pnt(0,0)),
  newXY(Win, Origin,DrawRct,XN,YN,_),
  assert(displayO(Win, CatNo,XN,YN,NCursor)), !.
 
kanTerug(_Win) :-	% besturing cursor
  schaal(_Win,_Letterhoogte,_Kolombreedte,_ClubBreedte, Mode, _Schaal),
  Mode = 0,
  displayO(_Win, CatNo,_X,_Y,Cursor),
  pos(CatNo, Top, _, _),  		% 
  dim(Top, TopDim),
  dim(Cursor,Dim),                  
  Kolom = TopDim - Dim + 1,
  Kolom > 0, !.
kanTerug(_Win) :-
  schaal(_Win,_Letterhoogte,_Kolombreedte,_ClubBreedte, Mode, _Schaal),
  Mode = 0,
  displayO(_Win, CatNo,_X,_Y,Cursor),
  pos(CatNo, Top, _, _),  		% 
  dim(Top, TopDim),
  dim(Cursor,Dim),                  
  Kolom = TopDim - Dim + 1,
  Kolom = 0,
  kopl(_VCat, _Nr, CatNo, plaats(Cursor)), !.	% er is een koppeling naar toe

select_Toolbar(_Win, 0) :-
  tb_schemabar_Create(_Win), !.
select_Toolbar(_Win, _) :-
  tb_deelinbar_Create(_Win).

  win_display_window_Create(_Parent, Onderdeel, WdNo, Mode):-
	wc(CatNo, _,_,  Onderdeel, _, _, _, _, _, _, _, _, _, _, _),
	catSelect(CatNo),
	not(displayO(_, _,_,_,_)),	% geen schema
	retract(schemawin(Win, _)),
	cleanPouleWindow(Win),
	assert(schemaWin(Win, CatNo)),
	restore_toolbar(Win),
	retract(pos(CatNo, Card, _, afval)),
	asserta(pos(CatNo, Card, WdNo, afval)),		% zet vooraan
	retractall(displayO(_, _,_,_,_)),
	asserta(displayO(Win, CatNo,0,0,WdNo)),
	win_SetHandler(Win,win_display_window_eh),
	getwcdisp(CatNo, Size),				% lettergrootte
	retractall(schaal(_, _, _, _, _, _)),
	asserta(schaal(Win, 0, 0, 0, Mode, Size)),
	afval_init(Win, Win, Size,Mode),
	titel(Mode, Onderdeel, Titel),
	win_SetText(Win, Titel),
	win_Invalidate(Win), !.
  win_display_window_Create(_Parent, Onderdeel, WdNo, Mode):-
	wc(CatNo, _,_,  Onderdeel, _, _, _, _, _, _, _, _, _, _, _),
	displayO(Win, CatNo1,_,_,_),		% wel een schema...
	CatNo1 <> CatNo,			% open een ander
	cleanPouleWindow(Win),
	retract(schemawin(Win, _)),
	assert(schemaWin(Win, CatNo)),
	restore_toolbar(Win),
	retract(pos(CatNo, Card, _, afval)),
	asserta(pos(CatNo, Card, WdNo, afval)),		% zet vooraan
	retractall(displayO(_, _,_,_,_)),
	asserta(displayO(Win, CatNo,0,0,WdNo)),
	win_SetHandler(Win,win_display_window_eh),
	getwcdisp(CatNo, Size),				% lettergrootte
	retractall(schaal(_, _, _, _, _, _)),
	asserta(schaal(Win, 0, 0, 0, Mode, Size)),
	afval_init(Win, Win, Size,Mode),
	titel(Mode, Onderdeel, Titel),
	win_SetText(Win, Titel),
	win_Invalidate(Win), !.
  win_display_window_Create(_Parent, Onderdeel, WdNo, _Mode):-
	wc(CatNo, _,_,  Onderdeel, _, _, _, _, _, _, _, _, _, _, _),
	pos(CatNo, _, _Cursor, afval),  	% bestaande
	retract(displayO(Win, CatNo,A,B,_)),
	asserta(displayO(Win, CatNo,A,B,WdNo)),
	win_Invalidate(Win), !.
  win_display_window_Create(_Parent, Onderdeel, WdNo, Mode):-
	assert(metCursor(b_True, 0)),
	wc(CatNo, _,_,  Onderdeel, _, _, _, _, _, _, _, _, _, _, _),
	titel(Mode, Onderdeel, Titel),
	retract(pos(CatNo, Card, _, afval)),
	asserta(pos(CatNo, Card, WdNo, afval)),		% zet vooraan
	not(displayO(_, CatNo,_,_,_)),
	NewWin = cast(WINDOW, 0),
	retractall(schemawin(_, _)),	% 22.8.2004	   
	assert(schemawin(NewWin, CatNo)),
	asserta(displayO(NewWin, CatNo, 0, 0, WdNo)),
	getwcdisp(CatNo, Size),				% lettergrootte
	asserta(schaal(NewWin, 0, 0, 0, Mode, Size)),
	win_Create(win_display_window_WinType,win_display_window_RCT,Titel,
		   win_display_window_Menu,_Parent,win_display_window_Flags,win_display_window_eh,0),!.


%BEGIN Display window, e_Create
  win_display_window_eh(_Win,e_Create(_),0):-!,
	NewWin = cast(WINDOW, 0),	 
	retract(schemawin(NewWin, Cat)),
	assert(schemaWin( _Win, Cat)),  
	schaal(NewWin, _, _,_, Mode, Size),
	select_Toolbar(_Win, Mode),
	afval_init(NewWin, _Win, Size,Mode),
%BEGIN Display window, InitControls, 16:42:39-24.7.2011, Code automatically updated!
%END Display window, InitControls
	!.
%END Display window, e_Create
%MARK Display window, new events

%BEGIN Display window, e_MouseDbl
  win_display_window_eh(_Win,e_MouseDbl(_PNT,_ShiftCtlAlt,_Button),0):-
	displayO(_Win, CatNo,_X,_Y,Cursor),
	TaskWin = vpi_GetTaskWin(),
	situatie(TaskWin, Cursor, CatNo, 0), !.
%END Display window, e_MouseDbl

%BEGIN Display window, id_Speler_gegevens
  win_display_window_eh(_Win,e_Menu(id_Speler_gegevens,_ShiftCtlAlt),0) :-
	displayO(_Win, Cat,_X,_Y,_Cursor),
	wc(Cat, _,_,  Onderdeel, _, _, _, _, _, _, _, _, _, _, _),
	%wc(Cat, Onderdeel,_,_,_,_,_),
	setSelCrit([Onderdeel],[]),
	TaskWin = vpi_GetTaskWin(),
	win_spelersel_Create(TaskWin, 16),
	spelerSelRefresh(),
	!.
%END Display window, id_Speler_gegevens

%BEGIN Display window, idt_plan
  win_display_window_eh(_Win,e_Menu(idt_plan,_ShiftCtlAlt),0):-!,
	displayO(_Win, Cat, _XN,_YN, Num),
	planMetAgenda(Num, Cat),
	!.
%END Display window, idt_plan

%BEGIN Display window, id_Wedstrijd_uitslag_invoeren
  win_display_window_eh(_Win,e_Menu(id_Wedstrijd_uitslag_invoeren,_ShiftCtlAlt),0):-!,
	schaal(_Win,_Letterhoogte,_Kolombreedte, _ClubBreedte,Mode, _Schaal),
	Mode = 0, 
	displayO(_Win, CatNo,_X,_Y,Cursor),
	invoerbaar(Cursor, CatNo, b_True),
	dlg_uitslaginvoer1_Create(_Win, Cursor, CatNo, b_False),
	select(CatNo, Cursor),
	!.
%END Display window, id_Wedstrijd_uitslag_invoeren

%BEGIN Display window, idb_koplg
  win_display_window_eh(_Win,e_Menu(idb_koplg,_ShiftCtlAlt),0):-
	displayO(_Win, Cat,_X,_Y,_Cursor),
	kopl(Cat, algemeen, CatN, algemeen),
	pos(CatN, _, New, _),
	select(CatN, New),
	!.
  win_display_window_eh(_Win,e_Menu(idb_koplg,_ShiftCtlAlt),0):-!,
	displayO(_Win, Cat,_X,_Y,_Cursor),
	dlg_koppelingenoverzicht_Create(_Win, [Cat]),
	!.
%END Display window, idb_koplg

%BEGIN Display window, e_KeyUp
  win_display_window_eh(_Win,e_KeyUp('C', c_Control),0):-
	win_PostEvent(_Win,e_Menu(idr_copy,0)),
	!.
%END Display window, e_KeyUp

%BEGIN Display window, e_Timer
  win_display_window_eh(_Win,e_Timer(_TimerId),0):-!,
	timer_Kill(_TimerId),
	win_PostEvent(_Win,e_User(111,0)),
 
	!.
%END Display window, e_Timer

%BEGIN Display window, id_Onderdeel_print
  win_display_window_eh(_Win,e_Menu(id_Onderdeel_print,_ShiftCtlAlt),0):-
	schaal(_Win,_Letterhoogte,_Kolombreedte, _ClubBreedte,Mode, _Schaal),
	Mode = 0, 
	displayO(_Win, Cat,_X,_Y,_Cursor),
	bovencat(Cat, CatBoven),
	dlg_maak_webpaginas_Create(_Win, afdruk, [CatBoven], b_True), 	% catzelf en catboven
	!.
  win_display_window_eh(_Win,e_Menu(id_Onderdeel_print,_ShiftCtlAlt),0):-!,
	schaal(_Win,_Letterhoogte,_Kolombreedte,_ClubBreedte, Mode, _Schaal),
	Mode = 1, 	
	displayO(_Win, _Cat,_X,_Y,_Cursor),
	%opg_cat(Item,Cat,0),
	dlg_indelingsrapport_Create(_Win, 0, "Opstellingsrapport"),
	!.
%END Display window, id_Onderdeel_print


%BEGIN Display window, idts_popup
  win_display_window_eh(_Win,e_Menu(idts_popup,_ShiftCtlAlt),0):-
	schaal(_Win,_Letterhoogte,_Kolombreedte,_ClubBreedte, Mode, _Schaal),
	gewonePop(Mode, _Win), !.
%END Display window, idts_popup

%BEGIN Display window, idts_overzicht
  win_display_window_eh(_Win,e_Menu(idts_overzicht,_ShiftCtlAlt),0):-!,
	displayO(_Win, CatNo,_X,_Y,Cursor),
	beeldbaar(Cursor, CatNo, b_True),	% beeld
	TaskWin = vpi_GetTaskWin(),
	situatie(TaskWin, Cursor, CatNo, 0), !.
%END Display window, idts_overzicht

%BEGIN Display window, idt_volgende
  win_display_window_eh(_Win,e_Menu(idt_volgende,_ShiftCtlAlt),0):-
	schaal(_Win,_Letterhoogte,_Kolombreedte,_ClubBreedte, Mode, _Schaal),
	Mode = 1, !.	% niet op reageren
  win_display_window_eh(_Win,e_Menu(idt_volgende,_ShiftCtlAlt),0):-!,
	displayO(_Win, CatNo,_X,_Y,Cursor),
	Cursor > 1,
	bitright(Cursor,1,Cursor1),
	%wc(CatNo,Onderdeel,_,_,_,_,_),
	pos(CatNo, _Top, _, afval ),  		% 
	dim(_Top, TopDim),
	Origin = pnt(_X, _Y),
	plaats(_Win,Cursor1,DrawRct,TopDim, Origin,pnt(0,0)),
	newXY(_Win,Origin,DrawRct,XN,YN,Move),
	plaats_Cursor(_Win, OldCursorRect),
	retractall(displayO(_Win, _,_,_,_)),
	assert(displayO(_Win, CatNo,XN,YN,Cursor1)),
	retract(pos(CatNo,T1,_P1,AP)),
	asserta(pos(CatNo,T1,Cursor1,AP)),
	display_Invalidate(_Win, Move, OldCursorRect), 
	!.
%END Display window, idt_volgende

%BEGIN Display window, idt_terug
  win_display_window_eh(_Win,e_Menu(idt_terug,_ShiftCtlAlt),0):-
	displayO(_Win, CatNo,_X,_Y,Cursor),
	pos(CatNo, Top, _, _),  		% 
	dim(Top, TopDim),
	dim(Cursor,Dim),                  
	Kolom = TopDim - Dim + 1,
	Kolom > 0,
	bitleft(Cursor,1,Cursor1),
	Origin = pnt(_X, _Y),
	plaats(_Win,Cursor1,DrawRct,TopDim, Origin,pnt(0,0)),
	newXY(_Win,Origin,DrawRct,XN,YN,Move),
	plaats_Cursor(_Win, OldCursorRect),
	retractall(displayO(_Win, _,_,_,_)),
	assert(displayO(_Win, CatNo,XN,YN,Cursor1)),
	retract(pos(CatNo,T1,_P1,AP)),
	asserta(pos(CatNo,T1,Cursor1,AP)),
	display_Invalidate(_Win, Move, OldCursorRect), 
	!.
  win_display_window_eh(_Win,e_Menu(idt_terug,_ShiftCtlAlt),0):-
	displayO(_Win, CatNo,_X,_Y,Cursor),
	pos(CatNo, Top, _, _),  		% 
	dim(Top, TopDim),
	dim(Cursor,Dim),                  
	Kolom = TopDim - Dim + 1,
	Kolom = 1,
	_X < 0,
	beep(),
	_ClipRct = win_GetClientRect(_Win),
	Origin = pnt(_X, _Y),
	plaats(_Win,Cursor,_DrawRct,TopDim, Origin,pnt(0,0)),
	plaats_Cursor(_Win, OldCursorRect),
	retractall(displayO(_Win, _,_,_,_)),
	assert(displayO(_Win, CatNo,0,_Y,Cursor)),
	display_Invalidate(_Win, b_True, OldCursorRect), 
	!.
  win_display_window_eh(_Win,e_Menu(idt_terug,_ShiftCtlAlt),0):-
	displayO(_Win, CatNo,_X,_Y,Cursor),
	pos(CatNo, Top, _, _),  		% 
	dim(Top, TopDim),
	dim(Cursor,Dim),                  
	Kolom = TopDim - Dim + 1,
	Kolom = 0,
	kopl(VCat, _Nr, CatNo, plaats(Cursor)),
	pos(VCat, Pos, _, _),  		% 
	select(VCat, Pos),
	!.
%END Display window, idt_terug

%BEGIN Display window, idt_op
  win_display_window_eh(_Win,e_Menu(idt_op,_ShiftCtlAlt),0):-
	displayO(_Win, CatNo, _X, _Y, Cursor),
	card(Cursor, Card),
	beweegUp(Card, Cursor, Cursor1),
	pos(CatNo, _Top, _, afval),  		% 
	dim(_Top, TopDim),
	Origin = pnt(_X, _Y),
	plaats(_Win,Cursor1,DrawRct,TopDim, Origin,pnt(0,0)),
	newXY(_Win,Origin,DrawRct,XN,YN,Move),
	plaats_Cursor(_Win, OldCursorRect),
	retractall(displayO(_Win, _,_,_,_)),
	assert(displayO(_Win, CatNo,XN,YN,Cursor1)),
	retract(pos(CatNo,T1,_P1,AP)),
	asserta(pos(CatNo,T1,Cursor1,AP)),
	display_Invalidate(_Win, Move, OldCursorRect), 
	!.
%END Display window, idt_op

%BEGIN Display window, idt_neer
  win_display_window_eh(_Win,e_Menu(idt_neer,_ShiftCtlAlt),0):-!,
	displayO(_Win, CatNo,X,Y,Cursor),
	card(Cursor, Card),
	beweegDown(Card,Cursor,Cursor1),
	pos(CatNo, _Top, _, afval ),  		% 
	dim(_Top, TopDim),
	Origin = pnt(X, Y),
	plaats(_Win,Cursor1,DrawRct,TopDim, Origin,pnt(0,0)),
	newXY(_Win, Origin,DrawRct,XN,YN,Move),
	plaats_Cursor(_Win, OldCursorRect),
	retractall(displayO(_Win, _,_,_,_)),
	assert(displayO(_Win, CatNo,XN,YN,Cursor1)),
	retract(pos(CatNo,T1,_P1,AP)),
	asserta(pos(CatNo,T1,Cursor1,AP)),
	display_Invalidate(_Win, Move, OldCursorRect), 
	!.
%END Display window, idt_neer

%BEGIN Display window, idt_klein
  win_display_window_eh(_Win,e_Menu(idt_klein,_ShiftCtlAlt),0):-!,
	schaal(_Win, _, _, _,Mode, Font),		% verklein
	Font1 = Font - 1,
	afval_init(_Win, _Win, Font1,Mode),
	win_Invalidate(_Win), 
	!.
%END Display window, idt_klein

%BEGIN Display window, idt_groot
  win_display_window_eh(_Win,e_Menu(idt_groot,_ShiftCtlAlt),0):-!,
	schaal(_Win, _, _, _, Mode, Font),		% vergroot
	Font1 = Font + 1,
	afval_init(_Win, _Win, Font1,Mode),
	win_Invalidate(_Win), 
	!.
%END Display window, idt_groot

  win_display_window_eh(_Win,e_Menu(lettergrootte,_ShiftCtlAlt),0):-
	dlg_toernooiinstellingen_Create(_Win, []), !.
	
%BEGIN Display window, e_MouseDown
  win_display_window_eh(_Win,e_MouseDown(PNT,_ShiftCtlAlt,_Any_Mouse_button),0):-
	schaal(_Win,_Letterhoogte,Kolombreedte,_ClubBreedte, Mode, _Schaal),
	Mode = 1,		% igv indelingsmode
	displayO(_Win, _CatNo,X,_Y,_Cursor),
	PNT = pnt(Xp, _Yp),
	Kolom = (Xp - X) div Kolombreedte,
	Kolom > 0, !.		% doe niets
  win_display_window_eh(_Win,e_MouseDown(PNT,_ShiftCtlAlt,_Any_Mouse_button),0):-
	schaal(_Win,_Letterhoogte,Kolombreedte,_ClubBreedte, Mode, _Schaal),
	Mode = 0,		% igv gewone mode
	displayO(_Win, _CatNo,X,_Y,_Cursor),
	PNT = pnt(Xp, _Yp),
	Kolom = (Xp - X) div Kolombreedte,
	Kolom < 0, !.		% doe niets
  win_display_window_eh(_Win,e_MouseDown(PNT,_ShiftCtlAlt,_Mouse_button_left),0):-
	schaal(_Win,_Letterhoogte,_Kolombreedte,_ClubBreedte, Mode, _),	% aparte afhandeling van finale
	Mode = 1,		% igv indelingsmode 
	displayO(_Win, CatNo,_X,_Y,_Cursor),
	pos(CatNo, Top, _, _),  		% 
	dim(Top, TopDim),
	Origin = pnt(_X, _Y),
	plaats(_Win,1,DrawRct,TopDim, Origin,pnt(0,0)),
	rect_PntInside(DrawRct,PNT), !.	% doe niets!
  win_display_window_eh(_Win,e_MouseDown(_PNT,_ShiftCtlAlt,mouse_button_right),0):-
	plaats_Cursor(_Win, CursorRect),
	rect_PntInside(CursorRect,_PNT),
	schaal(_Win,_Letterhoogte,_Kolombreedte,_ClubBreedte, Mode, _Schaal), !,
	gewonePop(Mode, _Win).
  win_display_window_eh(_Win,e_MouseDown(PNT,_ShiftCtlAlt,Mouse_button),0):-
	displayO(_Win, CatNo,_X,_Y,_Cursor),
	pos(CatNo, Top, _, _),  		% 
	dim(Top, TopDim),
	Origin = pnt(_X, _Y),
	plaats(_Win,1,DrawRct,TopDim, Origin,pnt(0,0)),	% finale ??
	rect_PntInside(DrawRct,PNT),
	newXY(_Win, Origin,DrawRct,XN,YN,Move),
	plaats_Cursor(_Win, OldCursorRect),
	retractall(displayO(_Win, _,_,_,_)),
	assert(displayO(_Win, CatNo,XN,YN,1)),
	getbacktrack(Here),
	retract(pos(CatNo,T1,_P1,AP)),
	cutbacktrack(Here),
	asserta(pos(CatNo,T1,1,AP)),
	display_Invalidate(_Win, Move, OldCursorRect), 
	schaal(_Win,_Letterhoogte,_Kolombreedte,_ClubBreedte, Mode, _Schaal), !,
	Mouse_button = mouse_button_right,  
	gewonePop(Mode, _Win).
  win_display_window_eh(_Win,e_MouseDown(PNT,_ShiftCtlAlt,Mouse_button),0):-
	schaal(_Win,Letterhoogte,Kolombreedte,_ClubBreedte, _Mode, _Schaal),
	displayO(_Win, CatNo,_X,_Y,_Cursor),
	PNT = pnt(Xp, Yp),
	Kolom = (Xp - _X) div Kolombreedte,
	offset_rij(_Win,Kolom, Offs),
	Kolom >= 0,
	bitleft(1, Kolom, K2),
	Row = (Yp - Offs - _Y) div ((Letterhoogte) * K2),
	pos(CatNo, Top, _, _),  		% 
	dim(Top, TopDim),
	Dim = TopDim - Kolom,
	Dim >= 0,
	bitleft(2,Dim,Card),
	Cursor1 = Card + Row,
	Origin = pnt(_X, _Y),
	plaats(_Win,Cursor1,DrawRct,TopDim, Origin,pnt(0,0)),
	newXY(_Win, Origin,DrawRct,XN,YN,Move),
	plaats_Cursor(_Win, OldCursorRect),
	retractall(displayO(_Win, _,_,_,_)),
	assert(displayO(_Win, CatNo,XN,YN,Cursor1)),
	getbacktrack(Here),
	retract(pos(CatNo,T1,_P1,AP)),
	cutbacktrack(Here),
	asserta(pos(CatNo,T1,Cursor1,AP)),
	%beep(),
	display_Invalidate(_Win, Move, OldCursorRect), 
	schaal(_Win,_Letterhoogte,_Kolombreedte,_ClubBreedte, Mode, _Schaal), !,
	Mouse_button = mouse_button_right,  
	gewonePop(Mode, _Win).
	%win_Invalidate(_Win), 
  win_display_window_eh(_Win,e_MouseDown(_PNT,_ShiftCtlAlt,_Button),0):- !.
%END Display window, e_MouseDown

%BEGIN Display window, e_User
  win_display_window_eh(_Win,e_User(111, _Ptr),0):-	%copy to clipboard
	schaal(_Win, Letterhoogte, _Kolombreedte,_ClubBreedte, _Mode, _Schaal),
	%displayO(_Win, CatNo,X,Y,_Cursor),
	%wc(CatNo,Onderdeel,_,_,_,_,_),
	URCT = win_GetClientRect( _Win ),		% gewacht tot cursor verdwenen is
	URCT = rct(X0, _Y0, X1, _Y1),
	toolbar_GetRect(_Win,tb_bottom,TbRCT),
	TbRCT = rct(_, YC1, _, _B1),
	UpdateRct = rct(X0, Letterhoogte, X1, YC1), 
	beep(),
	Picture = pict_GetFromWin(_Win,UpdateRCT),
	cb_PutPicture( Picture ), 
	assert(metCursor(b_True, 0)),
	win_Invalidate(_Win),
	dlg_MessageBox( "Kopie", "Zichtbare stuk gekopieerd naar het klembord.\nGebruik plakken in bijvoorbeeld Word.", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ), !.
  win_display_window_eh(_Win,e_User(upd_toolb, _Ptr),0):-
	schaal(_Win,_Letterhoogte,_Kolombreedte,_ClubBreedte, Mode, _Schaal),
	Mode = 1, !,		% gewoon
	toolbar_SetValue(_Win, idts_popup, ctrl_value(0,1)),
	displayO(_Win, Cat, _XN,_YN, Cursor),
	pos(Cat, Top, _, _), 
	Cursor >= 2 * Top,		% alleen kolom 1
	toolbar_SetValue(_Win, idts_popup, ctrl_value(1,1)), 
	fail.
  win_display_window_eh(_Win,e_User(upd_toolb, _Ptr),0):-
	toolbar_SetValue(_Win, id_Wedstrijd_uitslag_invoeren, ctrl_value(0,1)),
	displayO(_Win, Cat, _XN,_YN, Cursor),
	invoerbaar(Cursor, Cat, b_True),
	toolbar_SetValue(_Win, id_Wedstrijd_uitslag_invoeren, ctrl_value(1,1)),
	fail.
  win_display_window_eh(_Win,e_User(upd_toolb, _Ptr),0):-
	toolbar_SetValue(_Win, idb_koplg, ctrl_value(0,1)),
	getbacktrack(Here),
	displayO(_Win, Cat,_X,_Y,_Cursor),
	kopl(Cat, _, _CatN, _No),
	cutbacktrack(Here),
	toolbar_SetValue(_Win, idb_koplg, ctrl_value(1,1)),
	fail.
  win_display_window_eh(_Win,e_User(upd_toolb, _Ptr),0):-
	toolbar_SetValue(_Win, idt_terug, ctrl_value(0,1)),
	kanTerug(_Win),
	toolbar_SetValue(_Win, idt_terug, ctrl_value(1,1)),
	fail.
  win_display_window_eh(_Win,e_User(upd_toolb, _Ptr),0):-
	toolbar_SetValue(_Win, idts_popup, ctrl_value(0,1)),
	displayO(_Win, Cat, _XN,_YN, Num),
	wd(Num,Cat,_, _,_,_,_), 
	planbaar(Num, Cat, PlanB),
	invoerbaar(Num, Cat, Inv),
	wisplanbaar(Num, Cat, PlanW),
	beeldbaar(Num, Cat, Beeld),
	baanbaar(Num, Cat, Bnb),
	not(alleOptiesOnmogelijk(PlanB, Inv, PlanW, Beeld, Bnb)),
	toolbar_SetValue(_Win, idts_popup, ctrl_value(1,1)),
	fail.
  win_display_window_eh(_Win,e_User(upd_toolb, _Ptr),0):-
	toolbar_SetValue(_Win, idts_overzicht, ctrl_value(0,1)),
	displayO(_Win, Cat, _XN,_YN, Num),
	wd(Num,Cat,_, _,_,_,_),
	beeldbaar(Num, Cat, Beeld),
	Beeld = b_True,
	toolbar_SetValue(_Win, idts_overzicht, ctrl_value(1,1)),
	fail.
  win_display_window_eh(_Win,e_User(upd_toolb, _Ptr),0):-
	toolbar_SetValue(_Win, idt_plan, ctrl_value(0,1)),
	displayO(_Win, Cat, _XN,_YN, Num),
	wd(Num,Cat,_, _,_,_,_), !,
	planbaar(Num, Cat, PlanBaar),
	PlanBaar = b_True,
	toolbar_SetValue(_Win, idt_plan, ctrl_value(1,1)),
	fail.
%END Display window, e_User

%BEGIN Display window, e_Char
win_display_window_eh(_Win,e_Char('c',_ShiftCtlAlt),0):-
        win_onderdeelselectie_Create(_Win), !.
  win_display_window_eh(_Win,e_Char('\27',_SCA),0):-	
	win_Destroy(_Win), !.
  win_display_window_eh(_Win,e_Char(k_enter,_SCA),0):-	
	schaal(_Win,_Letterhoogte,_Kolombreedte,_ClubBreedte, Mode, _Schaal),
	gewonePop(Mode, _Win), !.
  win_display_window_eh(_Win,e_Char(k_del,_SCA),0):-	
	schaal(_Win,_Letterhoogte,_Kolombreedte,_ClubBreedte, Mode, _Schaal),
	Mode = 1, !,		% igv indelingsmode delete -> open plaats
	displayO(_Win, CatNo,_X,_Y,Cursor),
	bitright(Cursor,1,Num),
	bitand(Cursor,1,LR),
	Helft = LR + 1,
	pos(CatNo,_,_,RSoort),
	wd(Num,CatNo,T1,T2,_,_,_),
	wissel(Helft, T1, T2, TOld),
	process(Helft,4,CatNo,Num,TOld,RSoort), 
	select(CatNo,Cursor,1,0,onbekend), !.
  win_display_window_eh(_Win,e_Char(k_ins,_SCA),0):-	
	schaal(_Win,_Letterhoogte,_Kolombreedte,_ClubBreedte, Mode, _Schaal),
	Mode = 1, !,		% igv indelingsmode insert -> overbijvende
	displayO(_Win, CatNo,_X,_Y,Cursor),
	enableWijzigNIn(CatNo, VNin),
	VNin = b_True,
	bitright(Cursor,1,Num),
	bitand(Cursor,1,LR),
	Helft = LR + 1,
	pos(CatNo,_,_,RSoort),
	wd(Num,CatNo,T1,T2,_,_,_),
	wissel(Helft, T1, T2, TOld),
	process(Helft,2,CatNo,Num,TOld,RSoort), 
	select(CatNo,Cursor,1,0,onbekend), !.
  win_display_window_eh(_Win,e_Char(k_enter,_SCA),0):-	
	schaal(_Win,_Letterhoogte,_Kolombreedte,_ClubBreedte, Mode, _Schaal),
	%Mode = 0, !,		% gewoon
	gewonePop(Mode, _Win), !.
  win_display_window_eh(_Win,e_Char('+',_SCA),0):-	
	displayO(_Win, CatNo,_XN,_YN,_Cursor1),	% volgende onderdeel
	open_nextCat('+',CatNo),
	!.
  win_display_window_eh(_Win,e_Char('-',_SCA),0):-!,
	displayO(_Win, CatNo,_XN,_YN,_Cursor1),	% vorige onderdeeel
	open_nextCat('-',CatNo),
	!.
  win_display_window_eh(_Win,e_Char('g',c_Nothing),0):-!,
	schaal(_Win, _, _, _, Mode, Font),		% vergroot
	Font1 = Font + 1,
	afval_init(_Win, _Win, Font1, Mode),
	win_Invalidate(_Win), 
	!.
  win_display_window_eh(_Win,e_Char('k',c_Nothing),0):-!,
	schaal(_Win, _, _, _, Mode, Font),		% verklein
	Font1 = Font - 1,
	afval_init(_Win, _Win, Font1,Mode),
	win_Invalidate(_Win), 
	!.
  win_display_window_eh(_Win,e_Char(k_f5,_ShiftCtlAlt),0):-
	displayO(_Win, CatNo,_X,_Y,Cursor),	% plan (opnieuw)
	planbaar(Cursor, CatNo, b_True),
	planAangewezenWeds(_Win, Cursor, CatNo), !.
  win_display_window_eh(_Win,e_Char(k_f7,_ShiftCtlAlt),0):-!,
	displayO(_Win, CatNo,_X,_Y,Cursor),
	beeldbaar(Cursor, CatNo, b_True),	% beeld
	TaskWin = vpi_GetTaskWin(),
	situatie(TaskWin, Cursor, CatNo, 0), !.
  win_display_window_eh(_Win,e_Char(k_f9,_ShiftCtlAlt),0):-!,
	displayO(_Win, CatNo,_X,_Y,Cursor),
	invoerbaar(Cursor, CatNo, b_True),
	dlg_uitslaginvoer1_Create(_Win, Cursor, CatNo, b_False),
	select(CatNo, Cursor), !.
  win_display_window_eh(_Win,e_Char(k_right,_ShiftCtlAlt),0):-
	win_SendEvent(_Win,e_Menu(idt_volgende,c_Nothing)),
	!.
  win_display_window_eh(_Win,e_Char(k_left,_ShiftCtlAlt),0):-!,
	win_SendEvent(_Win,e_Menu(idt_terug,c_Nothing)),
	!.
  win_display_window_eh(_Win,e_Char(k_prev,_ShiftCtlAlt),0):-!,
	displayO(_Win, CatNo,_X,_Y,Cursor),	% Page Up toets
	card(Cursor, Card),
	Cursor > Card,
	Cursor1a = Cursor - 8,
	grootste(Cursor1a, Card, Cursor1),
	pos(CatNo, _Top, _, afval ),  		% 
	dim(_Top, TopDim),
	Origin = pnt(_X, _Y),
	plaats(_Win,Cursor1,DrawRct,TopDim, Origin,pnt(0,0)),
	newXY(_Win,Origin,DrawRct,XN,YN,Move),
	plaats_Cursor(_Win, OldCursorRect),
	retractall(displayO(_Win, _,_,_,_)),
	assert(displayO(_Win, CatNo,XN,YN,Cursor1)),
	retract(pos(CatNo,T1,_P1,AP)),
	asserta(pos(CatNo,T1,Cursor1,AP)),
	display_Invalidate(_Win, Move, OldCursorRect), 
	!.
  win_display_window_eh(_Win,e_Char(k_up,_ShiftCtlAlt),0):-!,
	win_SendEvent(_Win,e_Menu(idt_op,c_Nothing)),
	!.
  win_display_window_eh(_Win,e_Char(k_down,_ShiftCtlAlt),0):-!,
	win_SendEvent(_Win,e_Menu(idt_neer,c_Nothing)),
	!.
  win_display_window_eh(_Win,e_Char(k_next,_ShiftCtlAlt),0):-!,
	displayO(_Win, CatNo,X,Y,Cursor),	% Page down
	card(Cursor, Card),
	CursorMax = Card * 2 - 1,
	Cursor < CursorMax,
	Cursor1a = Cursor + 8,
	kleinste(Cursor1a, CursorMax, Cursor1), 
	pos(CatNo, Top, _, _),  		% 
	dim(Top, TopDim),
	Origin = pnt(X, Y),
	plaats(_Win,Cursor1,DrawRct,TopDim, Origin,pnt(0,0)),
	newXY(_Win, Origin,DrawRct,XN,YN,Move),
	plaats_Cursor(_Win, OldCursorRect),
	retractall(displayO(_Win, _,_,_,_)),
	assert(displayO(_Win, CatNo,XN,YN,Cursor1)),
	retract(pos(CatNo,T1,_P1,AP)),
	asserta(pos(CatNo,T1,Cursor1,AP)),
	display_Invalidate(_Win, Move, OldCursorRect),
	!.
%END Display window, e_Char

%BEGIN Display window, e_Update
  win_display_window_eh(_Win,e_Update(_UpdateRct1),0):-!,
	schaal(_Win, Letterhoogte, _Kolombreedte,_ClubBreedte, _Mode, _Schaal),
	displayO(_Win, CatNo,X,Y,_Cursor),
	%wc(CatNo,Onderdeel,_,_,_,_,_),
	_UpdateRct1 = rct(X0, _Y0, X1, Y1),
	X11 = X1 + 10,
	Y11 = Y1 + 10,
	UpdateRct = rct(X0, Letterhoogte, X11, Y11), 
	toon_schema(CatNo,_Win,UpdateRct,pnt(X,Y),pnt(0,0)),
	win_PostEvent(_Win,e_User(upd_toolb, 0)),
	!.
%END Display window, e_Update

%BEGIN Display window, e_Destroy
  win_display_window_eh(_Win,e_Destroy,0):-
	%catUnselect(),
	cleanSchemaWin(_Win),
	retractall(schemawin(_, _)), !.
%END Display window, e_Destroy

%BEGIN Display window, e_Size
  win_display_window_eh(_Win,e_Size(_Width,_Height1),0):-!,
IFDEF use_tbar
	toolbar_Resize(_Win),
ENDDEF
	!.
%END Display window, e_Size
  win_display_window_eh(_Win,e_menu(idr_copy,_CAS),0):-	
	win_Invalidate(_Win),
	TimerId = timer_Set(_Win, 500),
	assert(metCursor(b_False, TimerId)), !.% start copy sequence
  win_display_window_eh(_Win,e_Menu(id_wedstrijd_plan,_CAS),0):-
	displayO(_Win, CatNo,_X,_Y,Cursor),
	TaskWin = vpi_GetTaskWin(),
	planAangewezenWeds(TaskWin, Cursor, CatNo), !.	% 15.8.2000
  win_display_window_eh(_Win,e_Menu(idr_baannummer,_CAS),0):-
	displayO(_Win, CatNo,_X,_Y,Cursor),
	TaskWin = vpi_GetTaskWin(),
	dlg_baannummer_Create(TaskWin, Cursor, CatNo), 
	select(CatNo, Cursor), !.
  win_display_window_eh(_Win,e_Menu(id_wedstrijd_wis_plan,_CAS),0):-
	displayO(_Win, CatNo,_X,_Y,Cursor),
	afgelasten(_Win,Cursor,CatNo), !.
  win_display_window_eh(_Win,e_Menu(id_Wedstrijd_uitslag_invoeren,_CAS),0):-
	displayO(_Win, CatNo,_X,_Y,Cursor),
	TaskWin = vpi_GetTaskWin(),
	dlg_uitslaginvoer1_Create(TaskWin, Cursor, CatNo, b_False),	% 15.8.2000
	update_uitslagen("tasvdisp uitsl inv"),
	update_planinvoer(),
	select(CatNo, Cursor), !.
  win_display_window_eh(_Win,e_Menu(id_Wedstrijd_overzicht,_CAS),0):-!,
	displayO(_Win, CatNo,_X,_Y,Cursor),
	TaskWin = vpi_GetTaskWin(),
	situatie(TaskWin, Cursor, CatNo, 0), !.
  win_display_window_eh(_Win,e_Menu(id_wijzig_bye,_CAS),0):-!,
	displayO(_Win, CatNo,_X,_Y,Cursor),
	bitright(Cursor,1,Num),
	bitand(Cursor,1,LR),
	Helft = LR + 1,
	pos(CatNo,_,_,RSoort),
	wd(Num,CatNo,T1,T2,_,_,_),
	wissel(Helft, T1, T2, TOld),
	process(Helft,1,CatNo,Num,TOld,RSoort),
	select(CatNo,Cursor,1,0,onbekend), !.
  win_display_window_eh(_Win,e_Menu(id_wijzig_nietinged,_CAS),0):-!,
	displayO(_Win, CatNo,_X,_Y,Cursor),
	bitright(Cursor,1,Num),
	bitand(Cursor,1,LR),
	Helft = LR + 1,
	pos(CatNo,_,_,RSoort),
	wd(Num,CatNo,T1,T2,_,_,_),
	wissel(Helft, T1, T2, TOld),
	process(Helft,2,CatNo,Num,TOld,RSoort), 
	select(CatNo,Cursor,1,0,onbekend), !.
  win_display_window_eh(_Win,e_Menu(id_wijzig_bestaand,_CAS),0):-!,
	displayO(_Win, CatNo,_X,_Y,Cursor),
	bitright(Cursor,1,Num),
	bitand(Cursor,1,LR),
	Helft = LR + 1,
	pos(CatNo,_,_,RSoort),
	wd(Num,CatNo,T1,T2,_,_,_),
	wissel(Helft, T1, T2, TOld),
	process(Helft,3,CatNo,Num,TOld,RSoort),
	select(CatNo,Cursor,1,0,onbekend), !.
  win_display_window_eh(_Win,e_Menu(id_wijzig_nietbestaand,_CAS),0):-!,
	dlg_MessageBox( "Wijzig in..", 
	  "Voeg speler/koppel eerst toe via Spelerformulier, Deelname", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	!.
  win_display_window_eh(_Win,e_Menu(id_wijzig_gereserveerd,_CAS),0):-!,
	displayO(_Win, CatNo,_X,_Y,Cursor),
	bitright(Cursor,1,Num),
	bitand(Cursor,1,LR),
	Helft = LR + 1,
	pos(CatNo,_,_,RSoort),
	wd(Num,CatNo,T1,T2,_,_,_),
	wissel(Helft, T1, T2, TOld),
	process(Helft,4,CatNo,Num,TOld,RSoort), 
	select(CatNo,Cursor,1,0,onbekend), !.
  win_display_window_eh(_Win,e_Menu(id_wijzig_koppel,_CAS),0):-!,
	displayO(_Win, CatNo,_X,_Y,Cursor),
	bitright(Cursor,1,Num),
	bitand(Cursor,1,LR),
	Helft = LR + 1,
	pos(CatNo,_,_,RSoort),
	process(Helft,5,CatNo,Num,onbekend,RSoort), 
	select(CatNo,Cursor,1,0,onbekend), !.
  win_display_window_eh(_Win,e_Menu(id_wijzig_toevoegen,_CAS),0):-!,
	beep(),
	1 = dlg_MessageBox( "Toevoegen", "Gebruik deze optie bij uitzondering.\nVerificatie is uitgeschakeld!", mesbox_iconexclamation, mesbox_buttonsokcancel, mesbox_defaultsecond, mesbox_suspendapplication ),
	displayO(_Win, CatNo,_X,_Y,Cursor),
	bitright(Cursor,1,Num),
	bitand(Cursor,1,LR),
	Helft = LR + 1,
	pos(CatNo,_,_,RSoort),
	wd(Num,CatNo,T1,T2,_,_,_),
	wissel(Helft, T1, T2, TOld),
	process(Helft,6,CatNo,Num,TOld,RSoort), 
	select(CatNo,Cursor,1,0,onbekend), !.
  win_display_window_eh(_Win,e_Menu(id_Speler_lijsten,_ShiftCtlAlt),0) :-
	displayO(_Win, Cat,_X,_Y,Cursor),
	wd(Cursor, Cat, Tg1, Tg2, _, _, _),
	de_spelers(Cursor, Cat, Tg1, Tg2, Spelers),
	setSelCrit([],Spelers),
	TaskWin = vpi_GetTaskWin(),
	win_spelersel_Create(TaskWin, 16),
	spelerSelRefresh(),
	!.
  win_display_window_eh(_Win,e_Menu(id_koppelwis,_CAS),0):-!.

%BEGIN Display window, e_Menu, Parent window 
  win_display_window_eh(Win,e_Menu(ID,CAS),0):-!,
	PARENT = win_GetParent(Win),
	win_SendEvent(PARENT,e_Menu(ID,CAS)),
	!.
%END Display window, e_Menu, Parent window

%END_WIN Display window

cleanSchemaWin(_Win) :-
	retract(schaal(_Win,_,_,_,Mode,FSize)),
	displayO(_Win, CatNo,_,_,_),
	updateWcDisp(CatNo, FSize),
	Mode = 1,
	retract(displayO(_Win, _, _, _, _Cursor)),
	retract(pos(CatNo, Dim, _, Soort)),	% zorg dat hij niet op eerste kolom staat
	asserta(pos(CatNo, Dim, Dim, Soort)), !.
cleanSchemaWin(_Win) :-
	retract(displayO(_Win, CatNo, _, _, Cursor)),
	retract(pos(CatNo, Dim, _, Soort)),
	asserta(pos(CatNo, Dim, Cursor, Soort)), !.
cleanSchemaWin(_Win).


%BEGIN_DLG Onderdeel select
/**************************************************************************
	Creation and event handling for dialog: Onderdeel select
**************************************************************************/


CONSTANTS

%BEGIN Onderdeel select, CreateParms, 12:40:05-24.9.2002, Code automatically updated!
  dlg_onderdeel_select_ResID = idd_onderdeel_select
  dlg_onderdeel_select_DlgType = wd_Modal
  dlg_onderdeel_select_Help = idh_selecteer_onderdeel
%END Onderdeel select, CreateParms

database - odeel
single vanselect(vandom)

PREDICATES

  dlg_onderdeel_select_eh : EHANDLER
  dlg_onderdeel_select_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST,slist Selectie)
  dlg_onderdeel_select_update(DIALOG_VAL_LIST, slist Selectie)
  nondeterm plaatsmogelijkheden(vandom, string)
  nondeterm plaatsmogelijkheid(vandom, string, wedscat)
  determ plaatsmogelijkheid1(vandom, string, wedscat, string)

CLAUSES

vanselect(algemeen).

plaatsmogelijkheid(Rang, Tekst1, Cat) :-
  plaatsmogelijkheden(Rang, Tekst),
  plaatsmogelijkheid1(Rang, Tekst, Cat, Tekst1). 

plaatsmogelijkheid1(Rang, Tekst, Cat, Tekst1) :-
  kopl(Cat, Rang, CatNaar, _),
  not(poulOnd_ext(_, CatNaar)), !,
  concat("¬",Tekst, Tekst1).
plaatsmogelijkheid1(Rang, Tekst, Cat, Tekst1) :-
  t_kopl_ext(Cat, Rang, _, _), !,
  concat("¬",Tekst, Tekst1).
plaatsmogelijkheid1(_Rang, Tekst, _Cat, Tekst).

plaatsmogelijkheden(rang(1), "1e plaats").
plaatsmogelijkheden(rang(2), "2e plaats").
plaatsmogelijkheden(rang(3), "3e plaats").

  dlg_onderdeel_select_Create(Parent, Titel, IDD_ONDERDEEL_SELECT_LB_ITEMLIST, Preselect, Selectie, VanPlaats):-
	logclose(),	% voor de zekerheid
	assert(vanselect(algemeen)),
	IDD_ONDERDEEL_SELECT_LB_SELECT = [Preselect],
	Parm = cast(long, Titel),
%MARK Onderdeel select, new variables

	dialog_CreateModal(Parent,dlg_onderdeel_select_ResID,"",
  		[
%BEGIN Onderdeel select, ControlList, 12:40:05-24.9.2002, Code automatically updated!
		df(idd_onderdeel_select_lb,listbox(IDD_ONDERDEEL_SELECT_LB_ITEMLIST,IDD_ONDERDEEL_SELECT_LB_SELECT),nopr)
%END Onderdeel select, ControlList
		],
		dlg_onderdeel_select_eh,Parm,VALLIST,ANSWER),
	dlg_onderdeel_select_handle_answer(ANSWER,VALLIST,Selectie),
	vanselect(VanPlaats).

  dlg_onderdeel_select_handle_answer(idc_ok,VALLIST,Selectie):-!,
	dlg_onderdeel_select_update(VALLIST,Selectie).
  dlg_onderdeel_select_handle_answer(idc_cancel,_,[""]):-!.  % Handle Esc and Cancel here
  dlg_onderdeel_select_handle_answer(_,_,_):-
	errorexit().

  dlg_onderdeel_select_update(_VALLIST,_IDD_ONDERDEEL_SELECT_LB_ITEMLIST):-
%BEGIN Onderdeel select, Update controls, 12:40:05-24.9.2002, Code automatically updated!
	dialog_VLGetListBox(idd_onderdeel_select_lb,_VALLIST,_IDD_ONDERDEEL_SELECT_LB_ITEMLIST,_IDD_ONDERDEEL_SELECT_LB_SELECT),
%END Onderdeel select, Update controls
	true.

%MARK Onderdeel select, new events

%BEGIN Onderdeel select, idc_ok _CtlInfo
  dlg_onderdeel_select_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-
	Text = win_GetText(_Win),
	searchstring(Text,"oorsprong",FoundPos),
	FoundPos > 0,
	LboxWin = win_GetCtlHandle(_Win, idd_onderdeel_select_lb),
	Index = lbox_GetSelIndex(LboxWin),
	OnderdeelSel = lbox_GetItem(LboxWin, Index),
	parsetabs(OnderdeelSel, List),
	List = [_, _, Onderdeel | _],
	wc(Cat, _,_,  Onderdeel, _, _, _, _, _, _, _, _, _, _, _),
	findall(PlaatsT, plaatsmogelijkheid(_, PlaatsT,Cat), Plaatsen),
	Ans = dlg_ListSelect(_Win, "Selecteer",Plaatsen,0,StrSel,_Index),
	plaatsmogelijkheden(Rang, StrSel),
	assert(vanselect(Rang)),
	Ans = b_False,
	assert(vanselect(algemeen)),
	!.
%END Onderdeel select, idc_ok _CtlInfo



%BEGIN Onderdeel select, idc_help _CtlInfo
  dlg_onderdeel_select_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Selecteeronderdeel"),
	!.
%END Onderdeel select, idc_help _CtlInfo

%BEGIN Onderdeel select, idd_onderdeel_select_lb activated
  dlg_onderdeel_select_eh(_Win,e_Control(idd_onderdeel_select_lb,_CtrlType,_CtrlWin,activated),0):-!,
	trap(CW = win_GetCtlHandle(_Win, idc_ok),_,fail),!,
	win_SendEvent(_Win,e_Control(idc_ok,wc_PushButton,CW, activated())),
	!.
%END Onderdeel select, idd_onderdeel_select_lb activated

%BEGIN Onderdeel select, e_Create
  dlg_onderdeel_select_eh(_Win,e_Create(_CreationData),0):-!,
	Titel   = cast(string, _CreationData),
	win_SetText(_Win, Titel),
	TaskWin = vpi_GetTaskWin(),
	RCT1 = win_GetOuterRect(TaskWin),
	RCT2 = win_GetOuterRect(_Win),
	RCT1 = rct(L1, T1, R1, B1),
	RCT2 = rct(L2, T2, R2, B2),
	Dh = (L1 + R1 - (L2 + R2)) div 2,
	Dv = (T1 + B1 - (T2 + B2)) div 2,
	RCT = rect_Offset(RCT2, Dh, Dv),
	win_Move(_Win,RCT),
	LboxWin = win_GetCtlHandle(_Win, idd_onderdeel_select_lb),
	lbox_setTabstops(LboxWin, [8,23,25]),
	!.
%END Onderdeel select, e_Create

  dlg_onderdeel_select_eh(_,_,_):-!,fail.

%END_DLG Onderdeel select

%_____________________________koppelingen________________________________________




%BEGIN_DLG Koppelingenoverzicht
/**************************************************************************
	Creation and event handling for dialog: Koppelingenoverzicht
**************************************************************************/

constants

%BEGIN Koppelingenoverzicht, CreateParms, 14:39:29-31.7.2011, Code automatically updated!
  dlg_koppelingenoverzicht_DlgType = wd_Modal
  dlg_koppelingenoverzicht_Title = "Koppelingen"
  dlg_koppelingenoverzicht_RCT = rct(40,32,331,125)
  dlg_koppelingenoverzicht_Flags = [wsf_Close,wsf_TitleBar]
  dlg_koppelingenoverzicht_Help = idh_koppelen
  dlg_koppelingenoverzicht_Font = "Arial"
  dlg_koppelingenoverzicht_FSize = 9
%END Koppelingenoverzicht, CreateParms

database - koppel

single begincatl(catl)

 
predicates

  dlg_koppelingenoverzicht_eh : EHANDLER
nondeterm koppeling(string, wedscat, vandom, wedscat, ipvdom,tgnst)
  nondeterm   koppeling1(wedscat, vandom, tgnst, string, wedscat, ipvdom) - (i,i,o,o,i,i)

  procedure enableUitvoeren(WINDOW, wedscat Van, vandom, wedscat Naar, ipvdom)
  procedure enableTerug(WINDOW, wedscat, ipvdom)
  procedure uitgevoerd(wedscat, ipvdom, string) - (i, i, o)
  procedure bepaalEindCat(WINDOW, catl CatB1, catl CatBL)
  procedure deleteGelijkeOpgaven(wedscat, tgnst)
  positieNaar(wedscat CatNo1, ipvdom, integer Regel, integer WedsNo)
  nondeterm gekoppeldeWedstrijden(wedscat CatNo1, integer PosUit, rsoort, integer P1, integer Helft, integer H1)
  procedure terugdraaien(wedscat CatNo1, tgnst Nieuwe, ipvdom Naar, rsoort Afval) - (i,i,i,i)
  procedure terugdraaien(wedscat CatNo1, tgnst Nieuwe, rsoort, ipvdom Naar, integer VrijeOpenplaats) - (i,i,i,i,i)
  
clauses

begincatl([]).

bepaalEindCat(_Win, [CatBL], [CatBL]) :- !.	
bepaalEindCat(_, _, CatL) :-
  findall(Cat, wc(Cat, _,_,_, _, _, _, _, _, _, _, _, _, _, _),CatL), !.


enableUitvoeren(Win, _Cat, _, CatNaar, Pos) :-
  koplUitgevoerd(CatNaar, Pos, _Nieuwe), !,	% al uitgevoerd
  dialog_SetState(Win, [enable(idck_uitvoeren, b_False)]).
enableUitvoeren(Win, Cat, L, _CatNaar, _Pos) :-
  pos(Cat, _, _, Soort),
  not(vanPoelAfval(Soort, Cat, L, _, _, _)), !,	% nog niet bekend
  dialog_SetState(Win, [enable(idck_uitvoeren, b_False)]).
enableUitvoeren(Win, _, _, _, _) :- 
  dialog_SetState(Win, [enable(idck_uitvoeren, b_True)]).

enableTerug(Win, Cat, Pos) :-
  koplUitgevoerd(Cat, Pos, _Nieuwe), !,	% al uitgevoerd
  dialog_SetState(Win, [enable(idck_terugdraaien, b_True)]).
enableTerug(Win, _Cat, _Pos) :-
  dialog_SetState(Win, [enable(idck_terugdraaien, b_False)]).

vanPoelAfval(poel, CatNo, rang(L), T1, Pt, NK) :- !,	%NK = niet klaar
  poule_ranking1(CatNo, L, T1, Saldo, NK),
  Saldo = [Pt | _], !.
vanPoelAfval(afval, CatNo, rang(1), T1, 1, "") :- 
  w3(1,CatNo,T1,_,_,_,_), !.
vanPoelAfval(afval, CatNo, rang(2), T1, 2, "") :- 
  w3(1,CatNo,T2,_,_,_,_),
  wd(1,CatNo,T1,T2,_,_,_), !.
vanPoelAfval(afval, CatNo, pl3en4(3), T1, 2, "") :- % 3/4 boven
  w3(2,CatNo,T2,_,_,_,_),
  wd(2,CatNo,T1,T2,_,_,_), !.
vanPoelAfval(afval, CatNo, pl3en4(3), T1, 3, "") :- 
  w3(2,CatNo,T2,_,_,_,_),
  wd(2,CatNo,T2,T1,_,_,_), !.
vanPoelAfval(afval, CatNo, pl3en4(4), T1, 2, "") :- % 3/4 beneden.
  w3(3,CatNo,T2,_,_,_,_),
  wd(3,CatNo,T1,T2,_,_,_), !.
vanPoelAfval(afval, CatNo, pl3en4(4), T1, 3, "") :- 
  w3(3,CatNo,T2,_,_,_,_),
  wd(3,CatNo,T2,T1,_,_,_), !.
  
koppeling(KopS, CatNo, L, CatNo1, PosUit, T1) :-
  begincatl(CatL),
  member(CatNo, CatL),
  kopl(CatNo, L, CatNo1, PosUit),
  koppeling1(CatNo, L, T1, KopS, CatNo1, PosUit).

uitgevoerd(CatNo, PosUit, "(gedaan)") :-
  koplUitgevoerd(CatNo, PosUit, _Nieuwe), !.	% al uitgevoerd
uitgevoerd(_CatNo, _PosUit, "").

koppeling1(CatNo, L, T1, KopS, CatNo1, Naar) :-
  uitgevoerd(CatNo1, Naar, UitS),
  pos(CatNo,_,_,Type),
  wc(CatNo, _,_,  _OnderdeelX, _, CatShort, _, _, _, _, _, _, _, _, _),
  vanPoelAfval(Type, CatNo, L, T1, _, NK),	% de aangewezen winnaar of runner-up
  repa(nee,' ',nee,nee,nee,0,0,1,0,CatNo,T1,alleplanning,Naam1,_,_Log,b_False,b_False,_Clubs,_Anker,_,_,_,_,_Noti),% en bepaal de tekst ervoor 
  wc(CatNo1, _,_,_, _, CatStr1, _, _, _, _, _, _, _, _, _),
  positieNaar(CatNo1, Naar, Regel, _), !,
  format(Naam2,"%30.30s",Naam1),
  trim_c(Naam2),
  nr2tekst(L, LTekst),
  format(KopS, "%s nr %s: %s %s\t--> r. %u %s %s", 
  	CatShort, LTekst, Naam2, NK, Regel, CatStr1, UitS).
koppeling1(CatNo, L, onbekend, KopS, CatNo1, Naar) :-
  wc(CatNo1, _,_,_, _, CatStr1, _, _, _, _, _, _, _, _, _),
  positieNaar(CatNo1, Naar, Regel,_),
  wc(CatNo, _,_,_, _, CatShort, _, _, _, _, _, _, _, _, _),
  nr2tekst(L, LTekst),
  format(KopS, "%s nr %s: %s\t--> r. %u %", 
  	CatShort, LTekst, "(nog) onbekend", Regel, CatStr1).

positieNaar(CatNo1, plaats(PosUit), Regel, PosUitX) :-
  pos(CatNo1,Card,_,afval),
  bitright(PosUit, 1, PosUitX),
  Regel = PosUit - (2 * Card) + 1, !.	% de regeling waar de koppeling naar toe is
positieNaar(CatNo1, plaats(PosUit), PosUit, P1) :- 	% niet uitgevoerde poule
  wd(P1,CatNo1,openplaats(PosUit),_,_,_,_), !.
positieNaar(CatNo1, plaats(PosUit), PosUit, P1) :- 	% niet uitgevoerde poule
  wd(P1,CatNo1,_,openplaats(PosUit),_,_,_), !.
positieNaar(CatNo1, ipv(Tgnst), 0, P1) :-
  wd(P1,CatNo1,Tgnst,_,_,_,_), !.
positieNaar(CatNo1, ipv(Tgnst), 0, P1) :-
  wd(P1,CatNo1,_,Tgnst,_,_,_), !.

deleteGelijkeOpgaven(CatNo1, Tgnst) :-
  opg(Nummer,CatNo1,Tgnst,Plts,Rondes,Uitsl,Ind,Ranking,TeamId,Keur,Melding),
  logf('R',opg(Nummer,CatNo1,Tgnst,Plts,Rondes,Uitsl,Ind,Ranking,TeamId,Keur,Melding), nee),
  fail.
deleteGelijkeOpgaven(_CatNo1, _Tgnst).

gekoppeldeWedstrijden(_CatNo1, PosUit, afval, P1, Helft, H1) :- 	% maar eentje
  bitright(PosUit, 1, P1),
  bitand(PosUit, 1, H1),
  Helft = H1 + 1, !.
gekoppeldeWedstrijden(CatNo1, PosUit, poel, P1, 1, 0) :- 		% meerder igv poule
  wd(P1,CatNo1,openplaats(PosUit),_,_,_,_).
gekoppeldeWedstrijden(CatNo1, PosUit, poel, P1, 2, 1) :- 
  wd(P1,CatNo1,_,openplaats(PosUit),_,_,_).

koppelingEchtUitvoeren(Win, CatIn, CatNo1, PosUit, Tgnst, _OTgnst) :-
  loglock(mdi, "koppelingEchtUitvoeren"),				% Win = Taskwin
  getbacktrack(Here),
  date(Year,Month,Day),
  time(Hours,Minutes,_Seconds,_Hundredths),
  dt_min_to_offset(Year, Month, Day, Hours, Minutes, Nr),
  numb(N),     
  Nummer = Nr + N - 1,
  not(opg(Nummer,CatNo1,_,_,_,_,_,_,_,_,_)),
  cutbacktrack(Here),
  opg(_,CatIn,Tgnst,Plts,_,_,_,_,TeamId,Keur,Melding),
  deleteGelijkeOpgaven(CatNo1, Tgnst),			% die er misschien al stonden
  logf('A', opg(Nummer,CatNo1,Tgnst,Plts,0,0,0,"",TeamId,Keur,Melding),nee),	% maar eentje 
  pos(CatNo1,_,_,RSoort),
  gekoppeldeWedstrijden(CatNo1, PosUit, RSoort, P1, Helft, H1), % igv poule meerdere
  upd_wd(Helft, P1, CatNo1, Tgnst, RSoort),
  close_poules(),
  select(CatNo1, P1),
  door_plannen(DoorPl),
  nieuwPlan(DoorPl,  Win, P1, CatNo1), 
  repa(nee,' ',nee,nee,nee,0,0,1,0,CatNo1,Tgnst,alleplanning,Naam1,_,_Log,b_False,b_False,_Clubs,_Anker,_,_,_,_,_),% en bepaal de tekst ervoor 
  vervolgMelding(Win, Naam1, DoorPl, P1, CatNo1, H1),	
  fail.
koppelingEchtUitvoeren(_Win, CatVan, Cat, No, Tgnst, _OTgnst) :-
  pos(Cat,_,_,poel),
  kopl(CatVan, Rang, Cat, plaats(No)),
  logf('A', kopl(CatVan, Rang, Cat, ipv(Tgnst)), nee),	% vertel waar de koppeling naar toe is gegaan
  fail.
koppelingEchtUitvoeren(_Win, _CatVan, Cat, _No, _Tgnst, OTgnst) :-
  opg(Nummer,Cat,OTgnst,Plts,Rondes,Uitsl,Ind,Ranking,TeamId,Keur,Melding),
  logf('R', opg(Nummer,Cat,OTgnst,Plts,Rondes,Uitsl,Ind,Ranking,TeamId,Keur,Melding), nee),
  fail.
koppelingEchtUitvoeren(_Win, _CatIn, _CatNo1, _PosUit, _Tgnst, _) :-
  update_planinvoer(),
  update_situatie(),
  update_waarsch(), !.

terugdraaien(CatNo1, Nieuwe, Naar, afval) :-
  terugdraaien(CatNo1, Nieuwe, afval, Naar, 0), !.
terugdraaien(CatNo1, Nieuwe, Naar, poel) :-		% zoek eerst vrije open plaats uit
  findall(Open1, wd(_,CatNo1,openplaats(Open1),_,_,_,_), Open1L),
  findall(Open2, wd(_,CatNo1,_,openplaats(Open2),_,_,_), Open2L),
  append(Open1L, Open2L, List),
  numb(VrijeOpenPlaats),
  not(member(VrijeOpenPlaats, List)), !,
  terugdraaien(CatNo1, Nieuwe, poel, Naar, VrijeOpenplaats).
terugdraaien(_, _, _, _).				% zoek eerst vrije open plaats uit

terugdraaien(CatNo1, Nieuwe, _RSoort, _Naar, _VrijeOpenplaats) :-
  opg(Nummer,CatNo1,Nieuwe,A,B,C,D,LL,TeamId,Keur,Melding),
  logf('R', opg(Nummer,CatNo1,Nieuwe,A,B,C,D,LL,TeamId,Keur,Melding),ja),
  fail.
terugdraaien(CatNo1, _, afval, plaats(PosUit), _) :-
  bitright(PosUit, 1, P1),
  bitand(PosUit, 1, H1),
  Helft = H1 + 1,
  upd_wd(Helft, P1, CatNo1, onbekend, afval), !.
terugdraaien(CatNo1, Nieuwe, poel, _, VrijeOpenPlaats) :-
  wd(P1,CatNo1,Nieuwe,T2,_Stand,B,Note),
  logf('A', wd(P1,CatNo1,openplaats(VrijeOpenPlaats),T2,"",B,Note),ja),
  fail.
terugdraaien(CatNo1, Nieuwe, poel, _, VrijeOpenPlaats) :-
  wd(P1,CatNo1,T1,Nieuwe,_Stand,B,Note),
  logf('A', wd(P1,CatNo1,T1, openplaats(VrijeOpenPlaats),"",B,Note),ja),
  fail.
terugdraaien(CatNo1, Nieuwe, poel, _, VrijeOpenPlaats) :-
  kopl(CatVan, Rang, CatNo1, ipv(Nieuwe)),
  logf('A', kopl(CatVan, Rang, CatNo1, plaats(VrijeOpenPlaats)),ja),
  fail.
terugdraaien(_CatNo1, _Nieuwe, _, _, _rijeOpenPlaats).

  dlg_koppelingenoverzicht_Create(Parent, CatL):-
	assert(beginCatL(CatL)),
	win_CreateDynDialog(Parent,
		[
%BEGIN Koppelingenoverzicht, WinDefList, 14:39:29-31.7.2011, Code automatically updated!
		 dlg_font(wdef(dlg_koppelingenoverzicht_DlgType,dlg_koppelingenoverzicht_RCT,dlg_koppelingenoverzicht_Title,u_DlgBase),
		 	  dlg_koppelingenoverzicht_Font,dlg_koppelingenoverzicht_FSize,dlg_koppelingenoverzicht_Flags),
		 ctl(wdef(wc_LBox,rct(3,3,288,70),"",u_DlgBase),idck_lbkoppel,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_UseTabStops]),
		 ctl(wdef(wc_PushButton,rct(59,74,121,89),"(alvast) &Uitvoeren",u_DlgBase),idck_uitvoeren,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(252,74,286,89),"&Eindpunt",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(216,74,250,89),"&Cancel",u_DlgBase),idc_cancel,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(179,74,213,89),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(123,74,176,89),"&Terugdraaien",u_DlgBase),idck_terugdraaien,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(3,74,56,89),"&Veranderen",u_DlgBase),idck_veranderen,[wsf_Group,wsf_TabStop])
%END Koppelingenoverzicht, WinDefList
		],dlg_koppelingenoverzicht_eh,0).

%BEGIN Koppelingenoverzicht, idc_ok _CtlInfo
  dlg_koppelingenoverzicht_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	close_alles(),
	LboxWin = win_GetCtlHandle(_Win, idck_lbkoppel),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	koppeling(String, _CatIn, _PosIn, CatNo1, Naar, _T),
	String = Item,
	positieNaar(CatNo1, Naar, _Regel,PosUit),
	select(CatNo1, PosUit, 0, 1, onbekend),
	win_Destroy(_Win),
	!.
  dlg_koppelingenoverzicht_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	win_Destroy(_Win),
	!.
%END Koppelingenoverzicht, idc_ok _CtlInfo
%MARK Koppelingenoverzicht, new events

%BEGIN Koppelingenoverzicht, idck_veranderen _CtlInfo
  dlg_koppelingenoverzicht_eh(_Win,e_Control(idck_veranderen,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idck_lbkoppel),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	koppeling(String, _CatIn, _PosIn, CatNo1, plaats(PosUit), _T),
	String = Item,
	pos(CatNo1, _, _, afval),
	checkPaswoord(_Win),
	select(CatNo1, PosUit, 1, 0, onbekend),
	select(CatNo1, PosUit, 1, 0, onbekend),		% moet twee keer (!)
	win_Destroy(_Win),
	!.
  dlg_koppelingenoverzicht_eh(_Win,e_Control(idck_veranderen,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idck_lbkoppel),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	koppeling(String, _CatIn, _PosIn, CatNo1, _PosUit, _T),
	String = Item,
	pos(CatNo1, _, _, poel),
	checkPaswoord(_Win),
	TaskWin = vpi_GetTaskWin(),
	dlg_deel_in_poule_Create(TaskWin, CatNo1),
	win_Destroy(_Win),
	!.
%END Koppelingenoverzicht, idck_veranderen _CtlInfo

%BEGIN Koppelingenoverzicht, idck_terugdraaien _CtlInfo
  dlg_koppelingenoverzicht_eh(_Win,e_Control(idck_terugdraaien,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	LboxWin = win_GetCtlHandle(_Win, idck_lbkoppel),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	koppeling(String, _CatIn, _PosIn, CatNo1, Naar, _Tgnst),
	Item = String, !,
	koplUitgevoerd(CatNo1, Naar, Nieuwe),			% al uitgevoerd
	1 = dlg_MessageBox( "Koppeling", "Terugdraaien (ongedaan maken)?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
	pos(CatNo1,_,_,RSoort),
	terugdraaien(CatNo1, Nieuwe, Naar, RSoort),
	logclose(),
	update_waarsch(),
	update_planinvoer(),
	close_poules(),
	pos(CatNo1, _, Plaats, _),
	select(CatNo1, Plaats),
	findall(KopS, koppeling(KopS, _, _, _, _, _), KopList),
	lbox_Suspend(LboxWin),
	lbox_Clear(LboxWin),
	lbox_Add(LboxWin, KopList),
	lbox_SetSel(LboxWin, Index, b_True),	% als ie nog bestaat
	lbox_Resume(LboxWin),
	IndexA = lbox_GetSelIndex(LboxWin),
	ItemA = lbox_GetItem(LboxWin, IndexA),
	koppeling(StringA, CatInA, PosInA, CatNo1A, PosUitA, _),
	ItemA = StringA,
	enableUitvoeren(_Win, CatInA, PosInA, CatNo1A, PosUitA),
	enableTerug(_Win, CatNo1A, PosUitA),
	!.
%END Koppelingenoverzicht, idck_terugdraaien _CtlInfo


%BEGIN Koppelingenoverzicht, idc_help _CtlInfo
  dlg_koppelingenoverzicht_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Koppelen"),
	!.
%END Koppelingenoverzicht, idc_help _CtlInfo

%BEGIN Koppelingenoverzicht, idck_lbkoppel selchanged
  dlg_koppelingenoverzicht_eh(_Win,e_Control(idck_lbkoppel,_CtrlType,LboxWin,selchanged),0):-!,
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	koppeling(String, CatIn, PosIn, CatNo1, PosUit, _T),
	String = Item,
	pos(CatIn, _, Pos, _),
	select(CatIn, Pos),
	enableUitvoeren(_Win, CatIn, PosIn, CatNo1, PosUit),
	enableTerug(_Win, CatNo1, PosUit),
	!.
%END Koppelingenoverzicht, idck_lbkoppel selchanged

%BEGIN Koppelingenoverzicht, idck_uitvoeren _CtlInfo
  dlg_koppelingenoverzicht_eh(_Win,e_Control(idck_uitvoeren,_CtrlType,_CtrlWin,_CtlInfo),0):-
	LboxWin = win_GetCtlHandle(_Win, idck_lbkoppel),
	Index = lbox_GetSelIndex(LboxWin),
	Item = lbox_GetItem(LboxWin, Index),
	koppeling(String, CatIn, _PosIn, CatNo1, plaats(PosUit), Tgnst),
	Item = String, !,
	1 = dlg_MessageBox( "Koppeling", "Uitvoeren?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
	TaskWin = vpi_GetTaskWin(),
	koppelingEchtUitvoeren(TaskWin, CatIn, CatNo1, PosUit, Tgnst, onbekend),
	findall(KopS, koppeling(KopS, _, _, _, _, _), KopList),
	lbox_Suspend(LboxWin),
	lbox_Clear(LboxWin),
	lbox_Add(LboxWin, KopList),
	lbox_SetSel(LboxWin, Index, b_True),	% als ie nog bestaat
	lbox_Resume(LboxWin),
	IndexA = lbox_GetSelIndex(LboxWin),
	ItemA = lbox_GetItem(LboxWin, IndexA),
	koppeling(StringA, CatInA, PosInA, CatNo1A, PosUitA, _TgnstA),
	ItemA = StringA,
	enableUitvoeren(_Win, CatInA, PosInA, CatNo1A, PosUitA),
	enableTerug(_Win, CatNo1A, PosUitA),
	logclose(),
	!.
  dlg_koppelingenoverzicht_eh(_Win,e_Control(idck_uitvoeren,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	logclose().
%END Koppelingenoverzicht, idck_uitvoeren _CtlInfo


%BEGIN Koppelingenoverzicht, idc_cancel _CtlInfo
  dlg_koppelingenoverzicht_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):- 
	win_Destroy(_Win),
	!.
%END Koppelingenoverzicht, idc_cancel _CtlInfo

%BEGIN Koppelingenoverzicht, idck_lbkoppel activated
  dlg_koppelingenoverzicht_eh(_Win,e_Control(idck_lbkoppel,_CtrlType,_CtrlWin,activated),0):-
	UWin = win_GetCtlHandle(_Win, idck_uitvoeren),
	Wsflags = win_GetState(UWin),
	not(member(wsf_disabled, Wsflags)),
	trap(CW = win_GetCtlHandle(_Win, idck_uitvoeren),_,fail),!,
	win_SendEvent(_Win,e_Control(idck_uitvoeren,wc_PushButton,CW, activated())),
	!.
  dlg_koppelingenoverzicht_eh(_Win,e_Control(idck_lbkoppel,_CtrlType,_CtrlWin,activated),0):-!,
	logclose().
%END Koppelingenoverzicht, idck_lbkoppel activated


%BEGIN Koppelingenoverzicht, e_Create
  dlg_koppelingenoverzicht_eh(_Win,e_Create(_CreationData),0):-!,
	close_alles(),
	beginCatL(CatL),
        bepaalEindCat(_Win, CatL, CatBL),
        assert(beginCatL(CatBL)),
	findall(KopS, koppeling(KopS, _, _, _, _, _), KopList),
	LboxWin = win_GetCtlHandle(_Win, idck_lbkoppel),
	lbox_SetTabStops(LboxWin, [150, 170]),
	lbox_Add(LboxWin, KopList),
	lbox_SetSel(LboxWin, 0, b_True),
	getbacktrack(Here),
	koppeling(_, CatIn, L, CatNo, P1, _T),		% de eerste
	cutbacktrack(Here),
	pos(CatIn, _, Pos, _),
	select(CatIn, Pos),
	enableUitvoeren(_Win, CatIn, L, CatNo, P1),
	enableTerug(_Win, CatNo, P1),
	!.
%END Koppelingenoverzicht, e_Create

  dlg_koppelingenoverzicht_eh(_,_,_):-!,fail.

%END_DLG Koppelingenoverzicht



%BEGIN_TLB Schemabar, 10:56:50-23.8.2011, Code automatically updated!
/**************************************************************************
	Creation of toolbar: Schemabar
**************************************************************************/

clauses

  tb_schemabar_Create(_Parent):-
ifdef use_tbar
	toolbar_create(tb_bottom,0xC0C0C0,_Parent,
		[tb_ctrl(idts_popup,pushb,idb_menu,idb_menud,idb_grijs,"Menu;Popup menu (plannen etc.)",1,1),
		 tb_ctrl(id_Speler_gegevens,pushb,idb_spelersw,idb_spelerswd,idb_grijs,"Spelergegevens (F2);Lijst van alle personen in dit onderdeel",1,1),
		 tb_ctrl(id_Wedstrijd_uitslag_invoeren,pushb,idb_balw,idb_balp,idb_grijs,"Uitslag invoeren (F9);Uitslag invoeren",1,1),
		 tb_ctrl(idt_plan,pushb,idb_planw,idb_planwd,idb_grijs,"Plan of verschuif;Plan of verschuif wedstrijd",1,1),
		 tb_ctrl(idts_overzicht,pushb,idb_wedstrijd,idb_wedstrijdd,idb_grijs,"Wedstrijdoverzicht (F7);Wedstrijdoverzicht",1,1),
		 tb_ctrl(id_Onderdeel_print,pushb,idb_printw,idb_printdown,idb_grijs,"Afdruk;Schema afdrukken",1,1),
		 separator,
		 tb_ctrl(idt_terug,pushb,idb_terugw,idb_terugd,idb_grijs,"Vorige wedstrijd (of voorronde);Selecteer vorige wedstrijd (voorronde)",1,1),
		 tb_ctrl(idt_op,pushb,idb_ow,idb_opd,idb_grijs,"Op;Op",1,1),
		 tb_ctrl(idt_neer,pushb,idb_neerw,idb_neerp,idb_grijs,"Neer;Neer",1,1),
		 tb_ctrl(idt_volgende,pushb,idb_vooruitw,idb_vooruitwd,idb_grijs,"Volgende;Selecteer volgende wedstrijd",1,1),
		 tb_ctrl(idb_koplg,pushb,idb_koplgw,idb_koplgwd,idb_grijs,"Koppelingen;Volg de koppelingen",1,1),
		 tb_ctrl(idt_groot,pushb,idb_grootw,idb_grootp,idb_grijs,"Inzoomen; Vergroot schema (g)",1,1),
		 tb_ctrl(idt_klein,pushb,idb_kleinw,idb_kleinp,idb_grijs,"Uitzoomen;Verklein het schema (k)",1,1)]),
enddef
	true.
%END_TLB Schemabar

%BEGIN_DLG Koppelingmodel
/**************************************************************************
	Creation and event handling for dialog: Koppelingmodel
**************************************************************************/

constants

%BEGIN Koppelingmodel, CreateParms, 10:56:32-7.7.2016, Code automatically updated!
  dlg_koppelingmodel_DlgType = wd_Modal
  dlg_koppelingmodel_Title = "Maak eindronde en koppelingen"
  dlg_koppelingmodel_RCT = rct(50,40,325,198)
  dlg_koppelingmodel_Flags = [wsf_TitleBar]
  dlg_koppelingmodel_Help = idh_eindronde_en_koppelingen
  dlg_koppelingmodel_Font = "Arial"
  dlg_koppelingmodel_FSize = 9
%END Koppelingmodel, CreateParms

domains
  eentweedom = een ; twee


database - koppelmod
  dlgkop(WINDOW, wedscat HCat, string Eindrondenaam, slist Voorrondes, ilist Select , integer RetCode)
  single eenOfTwee(eentweedom, slist GewensteVoorrondes, rsoort AfvalPoule)	% b_False = 1      b_True = 1 en 2
  single voorrondeNr(integer)
  regel(integer RegelNo, wedscat Voorcat, integer ET) % Eerste resp Tweede

predicates

  dlg_koppelingmodel_eh : EHANDLER
  preselect1(WINDOW, WINDOW, integer, slist)
procedure  index(string, slist, integer, integer)
procedure  filterHetType(string, slist, slist)
procedure  filterHetType(wedstyp, sexlist, slist, slist)
  rBpresel(eentweedom, WINDOW, WINDOW, WINDOW)
  aPpresel(rsoort, window, window, window)
  enableOK(WINDOW)
  langNotOK(string, wedscat Eigen, string, slist)
  kortNotOK(string, wedscat Eigen, string, slist)
  maakStelsel(WINDOW, string Ltext, string Ktext, eentweedom, slist Voorrondes, rsoort AfvalPoule)  
  checkRegel(wedscat, ipvdom)
procedure  naamvoorstel(WINDOW, string, string)
procedure preselectAlle(ilist, WINDOW)
eindrondeNr(string LText, string Ktext, wedscat Cat,string OnderdeelIDE,wcorigine KeuringE,string KNLTBE,real GeldE,string SchemasoortE)

ap2rsoort(boolean, rsoort)
determ koppelvoorrondes(rsoort, eentweedom, slist VcatList, wedscat Cat, integer Card, integerlist)

nondeterm plsJeugd(integer Card, string, integer, eentweedom, integer Aantal, integer EersteTweede) - (i,i,o,i,i,o)
determ plsJeugd1(string, integer , integer , integer, integer) - (i,o,o,o, o)
nondeterm plts12(integer EenOfTwee, string Letter, integer, integer, integer, integer, integer)
determ plts58(integer, string, integer, integer, integer, integer)
determ plts58I(string Letr, integer Aantal, integer Plaatsing)
nondeterm plts9t16(integer, string, integer, integer, integer, integer, integer, integer, integer, integer)
determ plts9t16I(string Letr, integer Aantal, integer Plaatsing)
procedure maakIndJ(integerlist, integerlist)
procedure maakHoofdrondeJ(wedscat Cat, integer Card, slist Voorrondes, integerlist Indeling, eentweedom, rsoort) - (i,i,i,o,i,i)
procedure regelvolgorde(integer Card, eentweedom)
procedure eentweedom toEenTwee(boolean) 	%EenTwee = toEenTwee(EenTweeC),
procedure eenTwee(eentweedom, integer)

clauses

eenOfTwee(een, [], afval).
voorrondeNr(0).

index(Selected, [Selected|_], In, In) :- !.
index(Selected, [_|Rest], In, Uit) :-
  In1 = In + 1, !,
  index(Selected, Rest, In1, Uit).
index(_, _, _, 0).

filterHetType(Selected, TotList, Selectie) :-
  searchchar(Selected,'\t',Pos),
  frontstr(Pos,Selected,_,Selected1),
  wc(_Cat, _,_,  Selected1, WT, _,SexL, _, _, _, _, _, _, _, _), !,
  filterHetType(WT, SexL, TotList, Selectie).
filterHetType(_, TotList, TotList).
  
filterHetType(WT, SexL, [H|T], [H|Uit]) :-
  searchchar(H,'\t',Pos),
  frontstr(Pos,H,_,Selected1),
  wc(_, _,_,  Selected1, WT1, _,SexL1, _, _, _, _, _, _, _, _),
  WT = WT1,
  SexL = SexL1, !,
  filterHetType(WT, SexL, T, Uit).
filterHetType(WT, SexL, [_|T], Uit) :-
  filterHetType(WT, SexL, T, Uit), !.
filterHetType(_WT, _SexL, _, []).

preselect1(Win, LboxWin, 0, _) :-		% geen geselecteerd, dus alles
  dlgkop(Win, _, _ERNaam, SList, _, _),
  lbox_Suspend(LboxWin),
  lbox_Clear(LboxWin),
  lbox_Add(LboxWin, SList),
  lbox_Resume(LboxWin), !.
preselect1(Win, LboxWin, 1, [Selected]) :-	% één geselecteerd, dus peers
  dlgkop(Win, _, ERNaam, Slist, _, _),
  filterHetType(Selected, Slist, Selectie),
  naamvoorstel(Win, ERNaam, Selected),
  index(Selected, Selectie, 0, Index),
  lbox_Suspend(LboxWin),
  lbox_Clear(LboxWin),
  lbox_Add(LboxWin, Selectie),
  lbox_Resume(LboxWin),
  lbox_SetSel(LboxWin, Index, b_True), !.
preselect1(_Win, _LboxWin, _, _).		% verander niets

naamvoorstel(Win, "", H) :-
  searchchar(H,'\t',Pos),
  frontstr(Pos,H,_,H1),
  fronttoken(H1,Hx,H2),
  str_len(Hx, L2),
  L2 < 3,
  fronttoken(H2,Hy,H3),
  Hy = "-",
  LNWin = win_GetCtlHandle(Win, idck_eindronde),
  win_SetText(LNWin, H3),  
  wc(_, _,_,  H1, _, Short, _, _, _, _, _, _, _, _, _),
  frontstr(4,Short,S1,_),  
  ShWin = win_GetCtlHandle(Win, idck_kort),
  win_SetText(ShWin, S1), !.
naamvoorstel(Win, "", H) :-
  searchchar(H,'\t',Pos),
  frontstr(Pos,H,_,H1),
  wc(_, _,_,  H1, _, Short, _, _, _, _, _, _, _, _, _),
  LNWin = win_GetCtlHandle(Win, idck_eindronde),
  win_SetText(LNWin, H1),  
  frontstr(4,Short,S1,_),  
  ShWin = win_GetCtlHandle(Win, idck_kort),
  win_SetText(ShWin, S1), !.
naamvoorstel(Win, ERNaam, _H) :-
  LNWin = win_GetCtlHandle(Win, idck_eindronde),
  win_SetText(LNWin, ERNaam),
  wc(_, _,_,  ERNaam, _, Kort, _, _, _, _, _, _, _, _, _),
  trim_c(Kort),  
  ShWin = win_GetCtlHandle(Win, idck_kort),
  win_SetText(ShWin, Kort), !.
naamvoorstel(_Win, _, _).

enableOK(Win) :-
  LboxWin = win_GetCtlHandle(Win, idck_voorr),
  lbox_GetSel(LboxWin,ItemList,_IndexList),
  count(ItemList, 0, Aantal),
  Aantal > 1, !,
  dialog_SetState(Win, [enable(idc_ok, b_True)]).
enableOK(Win) :-
  dialog_SetState(Win, [enable(idc_ok, b_False)]).

rBpresel(een, RB, _, RB) :- !.
rBpresel(_, _, RB, RB).

aPpresel(afval, RB, _, RB) :- !.
aPpresel(_, _, RB, RB).

ap2rsoort(b_True, afval) :- !.
ap2rsoort(_, poel) :- !.

langNotOK("", _, "Invullen svp.", _) :- !.
langNotOK(Text, Cat, Uit, _) :-
  wc(CatNo, _,_,Text, _, _, _, _, _, _, _, _, _, _, _),
  %wc(CatNo,Text,_WT1,_,_SexL1,_,_), 
  CatNo <> Cat, 
  format(Uit, "Naam  %s  al ergens in de onderdelentabel.", Text),  !.
langNotOK(Text, Cat, Uit, _) :-
  wc(CatNo, _,_, Text, _, _, _, _, _, _, _, _, _, _, _),
  %wc(CatNo,_,_,Text,_SexL1,_,_), 
  CatNo <> Cat, 
  format(Uit, "Naam  %s  al ergens in de onderdelentabel\nals korte naam.", Text),  !.
langNotOK(Text, _Cat, Uit, Voorrondes) :-
  member(VR, Voorrondes),
  searchchar(VR,'\t',PosR),
  frontstr(PosR,VR,_,VR1),
  VR1 = Text,
  format(Uit, "Naam  %s  al in gebruik als voorronde.", Text),  !.
  
kortNotOK("", _, "Korte naam invullen svp.", _) :- !.
kortNotOK(Text, Cat, Uit, _) :-
  wc(CatNo, _,_,TextV, _, _, _, _, _, _, _, _, _, _, _),
  %wc(CatNo,_,_,TextV,_SexL1,_,_), 
  trim_c(TextV),
  TextV = Text, 
  CatNo >< Cat,
  format(Uit, "Korte naam  %s  al ergens in de onderdelentabel.", Text),  !.
kortNotOK(Text, _Cat, "Korte naam al in gebruik als voorronde.", Voorrondes) :-
  member(VR, Voorrondes),
  searchchar(VR,'\t',PosR),
  frontstr(PosR,VR,_,VR1),
  wc(_, _,_,VR1, _,TextV, _, _, _, _, _, _, _, _, _),
  trim_c(TextV),
  Text = TextV, !.

eindrondeNr(LText, _Ktext, Cat,OnderdeelIDE,KeuringE,KNLTBE,GeldE,SchemasoortE) :-
  wc(Cat, OnderdeelIdE,_,LText, _, _, _, _, _, _, _, SchemasoortE, KeuringE, KNLTBE, GeldE),!.
eindrondeNr(_LText, _Ktext, Cat,"",vanzelf,"",0.0,"") :- 
  numb(Cat),
  not(wc(Cat, _,_,_, _, _, _, _, _, _, _, _, _, _, _)), !.

eenTwee(een, 1).
eenTwee(twee, 2).

maakStelsel(_Win, Ltext, Ktext, EenTwee, Voorrondes, AfvalPoule) :-	% jeugd!
  Voorrondes = [Vronde|_],	% voorrondes komen van listbox
  searchchar(Vronde,'\t',Pos),
  frontstr(Pos,Vronde,_,Vronde1),
  getbacktrack(Here),
  wc(VoorCat, _TId,Order, Vronde1, WT, _, SexL, SPSterkte, leeftijd(LLStr,LftLow), leeftijd(LHStr,LftHigh), HDG, _Schemasoort, _Keuring, _KNLTB, _Geld),
  cutbacktrack(Here),
  count(Voorrondes, 0, Aantal),
  close_alles(),
  assert(voorrondeNr(0)),			% 28.12.2002 ???
  A1 = Aantal - 1,				% 5.10.2003 grens verleggen!
  card(A1, Card0),
  eenTwee(EenTwee, ET),
  Card = Card0 * ET,
  eindrondeNr(LText, Ktext, Cat,TIdE,KeuringE,KNLTBE,GeldE,SchemasoortE),		% bewaard om hetzelfde te houden
  logf('A', wc(Cat, TIdE,Order,  Ltext, WT, Ktext, SexL, SPSterkte, leeftijd(LLStr,LftLow), leeftijd(LHStr,LftHigh), HDG, SchemasoortE, KeuringE, KNLTBE, GeldE),ja),
  logf('T', wd(1,Cat,onbekend,onbekend,"",0,""), ja),
  maakHoofdrondeJ(Cat, Card, Voorrondes, Indeling, EenTwee, AfvalPoule),
  findall(Cat1,   wc(Cat1, _,_,_, _, _, _, _, _, _, _, _, _, _, _),Cat1L),	% zelfde pos in wclijst !
  positionWC(VoorCat, Cat1L, Cat1L1),
  evenWegWC(Cat1L1, Cat1L1Vol),			% haal achterste stuk weg
  logf('Z', wc(Cat, TIdE,Order,  Ltext, WT, Ktext, SexL, SPSterkte, leeftijd(LLStr,LftLow), leeftijd(LHStr,LftHigh), HDG, SchemasoortE, KeuringE, KNLTBE, GeldE),ja),
  sluitAanWC(Cat1L1Vol),			% en voeg weer toe en set modified()
  assert(voorrondenr(0)),
  koppelvoorrondes(AfvalPoule, EenTwee, Voorrondes, Cat, Card, Indeling),	% 23.3.2017
  retractall(regel(_,_,_)), 
  fail. 
maakStelsel(_, LText, Ktext, _, _, _) :-
  logclose(),
  eindrondeNr(LText, Ktext, Cat, _, _, _, _, _),
  pos(Cat, Card, _, _),
  select(Cat, Card), !. 
maakStelsel(_, _LText, _Ktext, _, _, _).

koppelvoorrondes(afval, _EenTwee, _VcatList, Cat, Card, _Indeling) :-
  retract(regel(RegelNo, O, ET)),
  ET <> 0,
  R1 = 2* Card + RegelNo - 1,
  not(checkRegel(Cat, plaats(R1))),
  logf('Z', kopl(O, rang(ET), Cat, plaats(R1)), ja),
  fail.   
koppelvoorrondes(poel, een, Voorrondes, Cat, _Card, _) :-
  %dlg_note("hier1"),
  member(Vronde, Voorrondes),
  parsetabs(Vronde, List),
  List = [_, Vronde1 | _],
  wc(VCat, _,_, Vronde1, _, _, _, _, _, _, _, _, _, _, _),
  voorrondeNr(Nr),
  Nr1 = Nr + 1,
  assert(voorrondeNr(Nr1)),
  %dlg_note("hier2"),
  logf('Z', kopl(VCat, rang(1), Cat, plaats(Nr1)), ja),
  %TT =  kopl(VCat, rang(1), Cat, plaats(Nr1)),
  %term_str(dbasedom,TT,MString),
  %format(Msg, "%d %s", VCat, MString),
  %dlg_note("poele eindronde!",Msg),
  fail.
koppelvoorrondes(poel, twee, Voorrondes, Cat, _Card, _) :-
  member(Vronde, Voorrondes),
  parsetabs(Vronde, List),
  List = [_, Vronde1 | _],
  wc(VCat, _,_, Vronde1, _, _, _, _, _, _, _, _, _, _, _),
  voorrondeNr(Nr),
  Nr1 = Nr + 1,
  assert(voorrondeNr(Nr1)),
  Nr0 = Nr1 - 1,
  Plaats1 = Nr0 * 2 + 1,
  logf('Z', kopl(VCat, rang(1), Cat, plaats(Plaats1)), ja),
  Plaats2 = Nr1 * 2,
  logf('Z', kopl(VCat, rang(2), Cat, plaats(Plaats2)), ja),
  fail.

checkRegel(Cat, plaats(RegelNo)) :-
  kopl(_, _, Cat, plaats(RegelNo)),
  format(Msg, "Regelnummer: %u al gekoppeld!", RegelNo),
  writeToJournaal(Msg, ja), !.
checkRegel(Cat, plaats(RegelNo)) :-
  bitand(RegelNo, 1, OE),
  OE = 0,
  bitright(RegelNo, 1, WNo),
  not(wd(WNo, Cat, onbekend, _, _, _, _)),
  format(Msg, "Regelnummer: %u geen open plaats!", RegelNo),
  writeToJournaal(Msg, ja), !.
checkRegel(Cat, plaats(RegelNo)) :-
  bitand(RegelNo, 1, OE),
  OE = 1,
  bitright(RegelNo, 1, WNo),
  not(wd(WNo, Cat, _, onbekend, _, _, _)),
  format(Msg, "Regelnummer: %u geen open plaats!", RegelNo), 
  writeToJournaal(Msg, ja), !.

preselectAlle([1], LboxWin) :-	% 26.12.2002 
  selectAlle(LboxWin), !.
preselectAlle(SelList, LboxWin) :-
  member(Sel, SelList),
  lbox_SetSel(LboxWin, Sel, b_True),
  fail.
preselectAlle(_SelList, _LboxWin).

regelvolgorde(Card, _) :-
  bitleft(Card, 1, CardPlus), % twee keer zoveel
  numb(RegelNo),
  assert(regel(RegelNo, 0, 0)),
  RegelNo >= CardPlus,
  !.
regelvolgorde(_, _).

maakHoofdrondeJ(_Cat, Card, Voorrondes, _Indeling, EenTwee, afval) :-
  retractall(regel(_,_,_)),
  assert(voorrondenr(0)),
  bitleft(Card,1,Cardi1),
  regelVolgorde(Card, EenTwee),   % zet eerst alle regels op 0 (= bye)
  count(Voorrondes, 0, Aantal),
  member(VCatSSel, Voorrondes),
  voorrondenr(AZ),
  AZ1 = AZ + 1,
  assert(voorrondeNr(AZ1)),
  searchchar(VCatSSel,'\t',Pos),
  frontstr(Pos,VCatSSel,_,Selected),
  pouleLetter(AZ, AZL),
  wc(VoorCat, _,_,Selected, _, _, _, _, _, _, _, _, _, _, _), % ga alle voorcats in volgorde van selectie (= alfabetisch) af
  plsJeugd(Cardi1,AZL,Plaatsing,EenTwee, Aantal, EersteTweede),
  retractall(regel(Plaatsing, _, _)),   % verander die bye die waar een voorronde uitmondt
  assert(regel(Plaatsing, VoorCat, EersteTweede)),
  fail.
maakHoofdrondeJ(Cat, Card, _, Indeling, _EenTwee, afval) :-
  %retractall(regel(_, 0, _)),
  findall(Regel, regel(Regel, _, _), Regels),
  sortint_c(Regels),
  maakIndJ(Regels, Indeling),
  logf('A', pos(Cat,Card,Card,afval), ja),
  apw(Cat, Indeling,Card, afval, b_True), !.		%  skelet + byes!
maakHoofdrondeJ(Cat, _Cardxx, Voorrondes, [], EenTwee, poel) :-
  count(Voorrondes, 0, Gereserveerd),
  eentwee(EenTwee, ET),
  Ger = Gereserveerd * ET,
  pouleOpenX(Cat, Ger, poel),		% genereert voldoende open plaatsen voor poules
  findall(Opg, opgeg(Cat,[],Opg,Ger), OpgLi),
  %term_str(rlist, OpgLi, LLStr),
  %dlg_note("maakhoofdrondej",LLStr),
  myloting(OpgLi,Gereserveerd,Indeling,_NumWeds,poel), % = determ!
  card(Gereserveerd, Card),
  logf('A', pos(Cat,Card,Card,poel), ja),
  apw(Cat, Indeling,Card, poel, b_False),
  logclose(), !.
maakHoofdrondeJ(_Cat, _Cardxx, _voorrondes, [], _EenTwee, _).

maakIndJ([Regel|Rest], [I|Ind]) :-
  regel(Regel, I, _), !,
  maakIndJ(Rest, Ind).
maakIndJ(_, []).

plsJeugd(_, "A", 1, een, 2, 1).
plsJeugd(_, "B", 2, een, 2, 1).
plsJeugd(_, "A", 1, een, 3, 1).
plsJeugd(_, "B", 4, een, 3, 1).
plsJeugd(_, "C", 3, een, 3, 1).
plsJeugd(4, Letr, Plaatsing, een, _, 1) :-
   plsJeugd1(Letr, Plaatsing, _, _, _), 
   Plaatsing > 0, !.
plsJeugd(8, Letr, Plaatsing, een, _, 1) :-
   plsJeugd1(Letr, _, Plaatsing, _, _), 
   Plaatsing > 0, !.
plsJeugd(16, Letr, Plaatsing, een, _, 1) :-
   plsJeugd1(Letr, _, _, Plaatsing, _),
   Plaatsing > 0, !.
plsJeugd(32, Letr, Plaatsing, een, _, 1) :-
   plsJeugd1(Letr, _, _, _, Plaatsing), !.
plsJeugd(_, Letr, Plaatsing, twee, N, ET) :-
   N = 2,
   plts12(ET, Letr, Plaatsing, _, _, _, _).
plsJeugd(_, Letr, Plaatsing, twee, N, ET) :-
   N = 3,
   plts12(ET, Letr, _, Plaatsing, _, _, _).
plsJeugd(_, Letr, Plaatsing, twee, N, ET) :-
   N = 4,
   plts12(ET, Letr, _, _, Plaatsing, _, _).
plsJeugd(_, Letr, Plaatsing, twee, N, ET) :-
   N > 4, N < 9,
   plts12(ET, Letr, _, _, _, Plaatsing, _),
   Plaatsing <> 999.
plsJeugd(_, Letr, Plaatsing, twee, N, 2) :-	% moet een tweede plaats zijn
   N > 4, N < 9,
   plts12(2, Letr, _, _, _, 999, _),
   plts58I(Letr, N, Plaatsing).
plsJeugd(_, Letr, Plaatsing, twee, N, ET) :-
   N > 8, N < 17,
   plts12(ET, Letr, _, _, _, _, Plaatsing),
   Plaatsing <> 999.
plsJeugd(_, Letr, Plaatsing, twee, N, 2) :-	% moet een tweede plaats zijn
   N > 8, N < 17,
   plts12(2, Letr, _, _, _, _, 999),
   plts9t16I(Letr, N, Plaatsing).
 
%            2,3,4,5/8,9/16 poules  0 = nvt!
plts12(1, "A", 1,1,1, 1,  1).
plts12(2, "A", 3,6,6, 13, 29).
plts12(1, "B", 4,8,8, 16, 32).
plts12(2, "B", 2,3,3, 4,  4).
plts12(1, "C", 0,5,5, 5,  9).
plts12(2, "C", 0,4,2, 999,999).
plts12(1, "D", 0,0,4, 12, 24).
plts12(2, "D", 0,0,7, 999,999).
plts12(1, "E", 0,0,0, 7,  16).
plts12(2, "E", 0,0,0, 999,999).
plts12(1, "F", 0,0,0, 10, 17).
plts12(2, "F", 0,0,0, 999,999).
plts12(1, "G", 0,0,0, 14, 8).
plts12(2, "G", 0,0,0, 999,999).
plts12(1, "H", 0,0,0, 3,  25).
plts12(2, "H", 0,0,0, 15, 999).
plts12(1, "I", 0,0,0,0,   6).
plts12(2, "I", 0,0,0,0,   999).
plts12(1, "J", 0,0,0,0,   27).
plts12(2, "J", 0,0,0,0,   999).
plts12(1, "K", 0,0,0,0,   19).
plts12(2, "K", 0,0,0,0,   999).
plts12(1, "L", 0,0,0,0,   14).
plts12(2, "L", 0,0,0,0,   999).
plts12(1, "M", 0,0,0,0,   11).
plts12(2, "M", 0,0,0,0,   999).
plts12(1, "N", 0,0,0,0,   22).
plts12(2, "N", 0,0,0,0,   999).
plts12(1, "O", 0,0,0,0,   30).
plts12(2, "O", 0,0,0,0,   999).
plts12(1, "P", 0,0,0,0,   3).
plts12(2, "P", 0,0,0,0,   31).

plts58I(Letr, 5, Plaatsing) :-
  plts58(2, Letr, Plaatsing, _,_,_), !.
plts58I(Letr, 6, Plaatsing) :-
  plts58(2, Letr, _, Plaatsing, _, _), !.
plts58I(Letr, 7, Plaatsing) :-
  plts58(2, Letr, _, _, Plaatsing, _), !.
plts58I(Letr, 8, Plaatsing) :-
  plts58(2, Letr, _,_,_,Plaatsing), !.

plts58(2, "C", 14, 9, 9, 9).
plts58(2, "D", 3, 8, 8, 8).
plts58(2, "E", 10, 14, 11, 11).
plts58(2, "F", 0, 3, 6, 6).
plts58(2, "G", 0, 0, 3, 2).

plts9t16I(Letr, 9, Plaatsing) :-
  plts9t16(2, Letr, Plaatsing, _, _, _, _, _, _, _), !.
plts9t16I(Letr, 10, Plaatsing) :-
  plts9t16(2, Letr, _, Plaatsing, _, _, _, _, _, _), !.
plts9t16I(Letr, 11, Plaatsing) :-
  plts9t16(2, Letr, _, _, Plaatsing, _, _, _, _, _), !.
plts9t16I(Letr, 12, Plaatsing) :-
  plts9t16(2, Letr, _, _, _, Plaatsing, _, _, _, _), !.
plts9t16I(Letr, 13, Plaatsing) :-
  plts9t16(2, Letr, _, _, _, _, Plaatsing, _, _, _), !.
plts9t16I(Letr, 14, Plaatsing) :-
  plts9t16(2, Letr, _, _, _, _, _, Plaatsing, _, _), !.
plts9t16I(Letr, 15, Plaatsing) :-
  plts9t16(2, Letr, _, _, _, _, _, _, Plaatsing, _), !.
plts9t16I(Letr, 16, Plaatsing) :-
  plts9t16(2, Letr, _, _, _, _, _, _, _, Plaatsing), !.

plts9t16(2, "C", 30, 21, 21, 21, 21, 21, 21, 21).
plts9t16(2, "D", 3, 12, 12, 12, 12, 12, 12, 12).
plts9t16(2, "E", 22, 30, 20, 20, 20, 20, 20, 20).
plts9t16(2, "F", 11, 3, 13, 13, 13, 13, 13, 13).
plts9t16(2, "G", 19, 22, 30, 28, 28, 28, 28, 28).
plts9t16(2, "H", 14, 11, 3, 5, 5, 5, 5, 5).
plts9t16(2, "I", 27, 19, 22, 30, 26, 26, 26, 26).
plts9t16(2, "J", 0, 14, 11, 3, 7, 7, 7, 7).
plts9t16(2, "K", 0, 0, 14, 11, 3, 15, 15, 15).
plts9t16(2, "L", 0, 0, 0, 22, 30, 18, 18, 18).
plts9t16(2, "M", 0, 0, 0, 0, 22, 30, 23, 23).
plts9t16(2, "N", 0, 0, 0, 0, 0, 3, 10, 10).
plts9t16(2, "O", 0, 0, 0, 0, 0, 0, 3, 2).
%plts9t16(2, "P", 0, 0, 0, 0, 0, 0, 0, 31). niet nodig

%              4, 8,   16, 32
plsJeugd1("A", 1, 1,   1,  1)    :- !.
plsJeugd1("B", 4, 8,   16, 32)  :- !.
plsJeugd1("C", 3, 5,   5,  9)   :- !.
plsJeugd1("D", 2, 4,   12, 24)   :- !.
plsJeugd1("E", 0, 3,   7,  16)   :- !.
plsJeugd1("F", 0, 6,   10, 17)   :- !.
plsJeugd1("G", 0, 7,   14, 8)   :- !.
plsJeugd1("H", 0, 2,   3,  25)   :- !.
plsJeugd1("I", 0, 0,   4,  6)   :- !.
plsJeugd1("J", 0, 0,   13, 27)   :- !.
plsJeugd1("K", 0, 0,   9,  19)  :- !.
plsJeugd1("L", 0, 0,   8,  14)   :- !.
plsJeugd1("M", 0, 0,   11, 11)  :- !.
plsJeugd1("N", 0, 0,   6,  22)   :- !.
plsJeugd1("O", 0, 0,   15, 30)  :- !.
plsJeugd1("P", 0, 0,   2,  3)    :- !.
plsJeugd1("Q", 0, 0, 0,    4)    :- !.
plsJeugd1("R", 0, 0, 0,    29)   :- !.
plsJeugd1("S", 0, 0, 0,    21)   :- !.
plsJeugd1("T", 0, 0, 0,    12)   :- !.
plsJeugd1("U", 0, 0, 0,    13)   :- !.
plsJeugd1("V", 0, 0, 0,    20)   :- !.
plsJeugd1("W", 0, 0, 0,    28)   :- !.
plsJeugd1("X", 0, 0, 0,    5)    :- !.
plsJeugd1("Y", 0, 0, 0,    26)   :- !.
plsJeugd1("Z", 0, 0, 0,    7)   :- !.
plsJeugd1("AA", 0, 0, 0,   18)  :- !.
plsJeugd1("BB", 0, 0, 0,   15)  :- !.
plsJeugd1("CC", 0, 0, 0,   23)  :- !.
plsJeugd1("DD", 0, 0, 0,   10)  :- !.
plsJeugd1("EE", 0, 0, 0,   31)  :- !.
plsJeugd1("FF", 0, 0, 0,   2)   :- !.

toEenTwee(b_True, twee) :- !.
toEenTwee(_, een).

  dlg_koppelingmodel_Create(Parent,Cat, EindrondeNaam, VoorrondeL,SelectIn,Return):-
	NWin = cast(WINDOW, 0),
	assert(dlgkop(NWin, Cat, EindrondeNaam, VoorrondeL, SelectIn, 0)),
	win_CreateDynDialog(Parent,
		[
%BEGIN Koppelingmodel, WinDefList, 10:56:32-7.7.2016, Code automatically updated!
		 dlg_font(wdef(dlg_koppelingmodel_DlgType,dlg_koppelingmodel_RCT,dlg_koppelingmodel_Title,u_DlgBase),
		 	  dlg_koppelingmodel_Font,dlg_koppelingmodel_FSize,dlg_koppelingmodel_Flags),
		 ctl(wdef(wc_LBox,rct(3,10,136,146),"",u_DlgBase),idck_voorr,[wsf_Group,wsf_TabStop,wsf_VScroll,wsf_NoIntegralHeight,wsf_MultiSelect,wsf_UseTabStops]),
		 ctl(wdef(wc_Edit,rct(140,24,271,36),"Eindronde",u_DlgBase),idck_eindronde,[wsf_Group,wsf_TabStop,wsf_AutoHScroll,wsf_AlignLeft]),
		 ctl(wdef(wc_Edit,rct(140,51,182,63),"Kort",u_DlgBase),idck_kort,[wsf_Group,wsf_TabStop,wsf_AutoHScroll,wsf_AlignLeft]),
		 ctl(wdef(wc_RadioButton,rct(141,67,199,77),"&Afvalschema",u_DlgBase),idck_afvalschema,[wsf_Auto,wsf_TabStop,wsf_Group]),
		 ctl(wdef(wc_RadioButton,rct(141,77,189,87),"&Poule",u_DlgBase),idck_poule,[wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_RadioButton,rct(145,103,193,113),"Nrs 1",u_DlgBase),idck_nrs_1,[wsf_Auto,wsf_TabStop,wsf_Group]),
		 ctl(wdef(wc_RadioButton,rct(145,113,193,123),"Nrs 1 en 2",u_DlgBase),idck_nrs_1_en_2,[wsf_TabStop,wsf_Auto]),
		 ctl(wdef(wc_PushButton,rct(232,144,272,156),"&OK",u_DlgBase),idc_ok,[wsf_Default,wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(232,129,272,141),"&Help",u_DlgBase),idc_help,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_PushButton,rct(137,134,169,146),"&Geen",u_DlgBase),idck_geen,[wsf_Group,wsf_TabStop]),
		 ctl(wdef(wc_Text,rct(2,147,141,157),"Selecteer alle betreffende voorrondes",u_DlgBase),idctk_selecteer_de_voorrondes,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(140,14,243,24),"Onderdeelnaam (uniek):",u_DlgBase),idctk_vul_naam,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(140,41,259,51),"Korte naam (ook uniek):",u_DlgBase),idck_kort_in,[wsf_AlignLeft]),
		 ctl(wdef(wc_Text,rct(2,0,50,10),"Voorrondes:",u_DlgBase),idctk_voorrondes,[wsf_AlignLeft]),
		 ctl(wdef(wc_GroupBox,rct(138,93,199,128),"Wie gaan door?",u_DlgBase),idck_door_gaan,[]),
		 ctl(wdef(wc_GroupBox,rct(138,4,274,91),"Eindronde",u_DlgBase),idck_hoofdronde,[])
%END Koppelingmodel, WinDefList
		],dlg_koppelingmodel_eh,0),
	retract(dlgkop(NWin, _, _, _, _, Return)), !.

%BEGIN Koppelingmodel, idc_ok _CtlInfo
  dlg_koppelingmodel_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	openbaar(ja),
	NWin = win_GetCtlHandle(_Win, idck_nrs_1_en_2),
	b_True = win_IsChecked(NWin),
	dlgkop(_Win, Cat, _ERNaam, _VList, _PreSList, _Retcode),
	wc(Cat, _,_,  _, _, _, _, _, _, leeftijd(_LHStr,LftHigh), _, _, _, _, _),
	%wcrit(Cat, _SPSterkte, _LftLow, LftHigh, _Reserve),
	LftHigh > 2,		% wel gedefiniëerd maar 
	LftHigh < 17,		% jeugd
	2 = dlg_MessageBox( "Jeugdtoernooi", "Alleen de nummers één gaan door\n bij een jeugdtoernooi!\nWil je daar van afwijken?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
	!.
  dlg_koppelingmodel_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	dlgkop(_Win, Cat, _ERNaam, _VList, _PreSList, _Retcode),
	pos(Cat, _, _, _),
        wc(Cat, _,_,  Onderdeel, _, _, _, _, _, _, _, _, _, _, _),
	format(Note, "indeling van %s (%u)", Onderdeel, Cat),
	dlg_note(Note),
	2 = dlg_MessageBox( "Indeling", "Bestaande indeling van de eindronde vervangen?", mesbox_iconquestion, mesbox_buttonsyesno, mesbox_defaultsecond, mesbox_suspendapplication ),
	!.
  dlg_koppelingmodel_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	NWin = win_GetCtlHandle(_Win, idck_nrs_1_en_2),
	EenTweeC = win_IsChecked(NWin),
	EenTwee = toEenTwee(EenTweeC),
	OWin = win_GetCtlHandle(_Win, idck_voorr),
	lbox_GetSel(OWin,GewensteVoorrondes,_IndexList),
	AWin = win_GetCtlHandle(_Win, idck_afvalschema),
	AP = win_IsChecked(AWin),
	ap2rsoort(AP, AfvalPoule),
	assert(eenOfTwee(EenTwee, GewensteVoorrondes, AfvalPoule)),	% wordt hieronder verder gebruikt
	fail.
  dlg_koppelingmodel_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	eenOfTwee(_, GewensteVoorrondes,_),
	FldWin = win_GetCtlHandle(_Win, idck_eindronde),
	Text = win_GetText(FldWin),
	trim_c(Text),
	dlgkop(_Win, ERCat,_, _, _, _),
	langNotOK(Text, ERCat, Msg, GewensteVoorrondes),
	dlg_MessageBox( "Lange naam", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	win_SetFocus(FldWin),
	!.
  dlg_koppelingmodel_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	eenOfTwee(_, GewensteVoorrondes,_),
	FldWin = win_GetCtlHandle(_Win, idck_kort),
	Text = win_GetText(FldWin),
	trim_c(Text),
	dlgkop(_Win, ERCat,_, _, _, _),
	kortNotOK(Text, ERCat, Msg, GewensteVoorrondes),
	dlg_MessageBox( "Korte naam", Msg, mesbox_iconexclamation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	win_SetFocus(FldWin),
	!.
  dlg_koppelingmodel_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	LWin = win_GetCtlHandle(_Win, idck_eindronde),
	LText = win_GetText(LWin),
	trim_c(LText),
	KWin = win_GetCtlHandle(_Win, idck_kort),
	KText = win_GetText(KWin),
	trim_c(KText),
	eenOfTwee(EenTwee, _, AfvalPoule),
	OWin = win_GetCtlHandle(_Win, idck_voorr),
	lbox_GetSel(OWin,Voorrondes,_IndexList),
	maakStelsel(_Win, LText, KText, EenTwee, Voorrondes, AfvalPoule),
	retract(dlgkop(_Win, ERCat,ERNaam, VList, PreSList, _)),
	assert(dlgkop(_Win, ERCat, ERNaam, Vlist, PreSList, 1)),
	win_Destroy(_Win),
	!.
  dlg_koppelingmodel_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtrlInfo),0):-
	!.
%END Koppelingmodel, idc_ok _CtlInfo
%MARK Koppelingmodel, new events


%BEGIN Koppelingmodel, e_Destroy
  dlg_koppelingmodel_eh(_Win,e_Destroy,0):-!,
	retract(dlgkop(_Win, CAt, ERNaam, VList, PreSList, Retcode)),
	NWin = cast(WINDOW, 0),
	assert(dlgkop(NWin, Cat, ERNaam, Vlist, PreSList, Retcode)),
	!.
%END Koppelingmodel, e_Destroy

%BEGIN Koppelingmodel, idck_voorr selchanged
  dlg_koppelingmodel_eh(_Win,e_Control(idck_voorr,_CtrlType,LboxWin,selchanged),0):-!,
	lbox_GetSel(LboxWin,ItemList,_IndexList),
	count(ItemList, 0, Aantal),
	preselect1(_Win, LboxWin, Aantal, ItemList),
	enableOK(_Win),
	!.
%END Koppelingmodel, idck_voorr selchanged


%BEGIN Koppelingmodel, idck_geen _CtlInfo
  dlg_koppelingmodel_eh(_Win,e_Control(idck_geen,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	%dlgkop(_Win, Vlist, _SList),
	LboxWin = win_GetCtlHandle(_Win, idck_voorr),
	preselect1(_Win, LboxWin, 0, []),
	enableOK(_Win),
	!.
%END Koppelingmodel, idck_geen _CtlInfo

%BEGIN Koppelingmodel, idc_cancel _CtlInfo
  dlg_koppelingmodel_eh(_Win,e_Control(idc_cancel,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	win_Destroy(_Win),
	!.
%END Koppelingmodel, idc_cancel _CtlInfo


%BEGIN Koppelingmodel, idc_help _CtlInfo
  dlg_koppelingmodel_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Eindrondeenkoppelingen"),
	!.
%END Koppelingmodel, idc_help _CtlInfo

%BEGIN Koppelingmodel, e_Create
  dlg_koppelingmodel_eh(_Win,e_Create(_CreationData),0):-!,
	NWin = cast(WINDOW, 0),
	retract(dlgkop(NWin, CatIn, ERNaam, VList, PreSList, Return)),
	assert(dlgkop(_Win, CatIn, ERNaam, Vlist, PreSList, Return)),
	LboxWin = win_GetCtlHandle(_Win, idck_voorr),
	lbox_SetTabStops(LboxWin, [10]),
	lbox_Add(LboxWin, VList),
	preselectAlle(PreSList, LboxWin),
	RB1 = win_GetCtlHandle(_Win, idck_nrs_1),
	RB2 = win_GetCtlHandle(_Win, idck_nrs_1_en_2),
	eenOfTwee(EenTwee,_,AfvalPoule),
	rBpresel(EenTwee, RB1, RB2, RBsel),
	win_CheckRadioButton(RBsel, [RB1, RB2]),
	RB3 = win_GetCtlHandle(_Win, idck_afvalschema),
	RB4 = win_GetCtlHandle(_Win, idck_poule),
	aPpresel(AfvalPoule, RB3, RB4, RBSel1),
	win_CheckRadioButton(RBsel1, [RB3, RB4]),
	HItem = lbox_GetItem(LboxWin, 0),
	naamVoorstel(_Win, ERNaam, HItem),
	enableOK(_Win),
	!.
%END Koppelingmodel, e_Create

  dlg_koppelingmodel_eh(_,_,_):-!,fail.

%END_DLG Koppelingmodel

%BEGIN_TLB Deelinbar, 20:58:54-22.8.2011, Code automatically updated!
/**************************************************************************
	Creation of toolbar: Deelinbar
**************************************************************************/

clauses

  tb_deelinbar_Create(_Parent):-
ifdef use_tbar
	toolbar_create(tb_bottom,0xC0C0C0,_Parent,
		[tb_ctrl(idts_popup,pushb,idb_menu,idb_menud,idb_grijs,"Popup;Popup",1,1),
		 tb_ctrl(id_Onderdeel_print,pushb,idb_printw,idb_printdown,idb_grijs,"Afdrukken;Schema afdrukken",1,1),
		 tb_ctrl(idt_op,pushb,idb_ow,idb_opd,idb_grijs,"Op;Op",1,1),
		 tb_ctrl(idt_neer,pushb,idb_neerw,idb_neerp,idb_neerp,"Neer;Neer",1,1),
		 tb_ctrl(idt_groot,pushb,idb_grootw,idb_grootp,idb_grijs,"Inzoomen; Vergroot schema (g)",1,1),
		 tb_ctrl(idt_klein,pushb,idb_kleinw,idb_kleinp,idb_grijs,"Uitzoomen;Verklein het schema (k)",1,1),
		 separator,
		 tb_text(idts_tekst3,tb_static,168,0,4,9,0x0,"Beëindig met Alt-D of Escape")]),
enddef
	true.
%END_TLB Deelinbar



%BEGIN_WIN Onderdeel
/**************************************************************************
        Creation and event handling for window: Onderdeel
**************************************************************************/

constants
%BEGIN Onderdeel, CreateParms, 16:39:08-21.8.2004, Code automatically updated!
  win_onderdeel_WinType = w_TopLevel
  win_onderdeel_Flags = [wsf_SizeBorder,wsf_TitleBar,wsf_Maximize,wsf_Close,wsf_ClipSiblings,wsf_ClipChildren]
  win_onderdeel_RCT = rct(100,80,440,240)
  win_onderdeel_Menu = no_menu
  win_onderdeel_Title = "Onderdeel"
  win_onderdeel_Help = idh_afvalschema
%END Onderdeel, CreateParms

predicates

  win_onderdeel_eh : EHANDLER
  onderdeelWindowSize(window, wedscat, string)

clauses

onderdeelWindowSize(Win, CatNo, Onderdeel) :-
  %toolbar_remove(Win),
  write("\ntoolbar niet removed"),
  getWcDisp(CatNo, FontSize),
  Font = font_Create(ff_Helvetica, [], FontSize),
  win_SetFont(Win, Font),
  win_GetTextExtent(Win, Onderdeel, -1, Width, Height),
  Teksthoogte = 6 * (Height + 4),	% 4 regels hoog
  TekstBreedte = 3 * (Width + 2),
  %ClientRct = win_GetClientRect(Win),
  TaskWin = vpi_GetTaskWin(),
  TaskRct = win_GetClientRect(TaskWin),
  TaskRct = rct(_, _, R, B),
  B1 = B - 25,
  kleinste(TekstHoogte, B1, OH),
  R1 = R - 15,
  kleinste(TekstBreedte, R1, OB),
  RCTW = rect_Offset(rct(0,0,OB,OH), 15, 35),
  win_Move(Win,RCTW), !.

  win_onderdeel_Create(_Parent, Onderdeel):-
	%write("\nHier onderdeel !!!"),
	wc(CatNo, _,_,Onderdeel, _, _, _, _, _, _, _, _, _, _, _),
	%wc(CatNo, Onderdeel,_,_,_,_,_),
	%pos(CatNo, _, _Cursor, poel),  	% een ander wordt geopend 
	schemaWin(Win, _Cat),
	%displayP(Win, Cat,_,_,_, _),
	%Cat <> CatNo,
	win_SetHandler(Win,win_onderdeel_eh),
	win_SetText(Win, Onderdeel),
	cleanSchemaWin(Win),
	cleanPouleWindow(Win),
	retract(schemaWin(Win, _)),
	assert(schemaWin(Win, CatNo)),
	win_Clear(Win, color_Yellow),
	onderdeelwindowsize(Win, CatNo, Onderdeel),
	win_invalidate(Win),
	!.
  win_onderdeel_Create(_Parent, Onderdeel):-
	Nulwin = cast(window, 0),
	wc(CatNo, _,_, Onderdeel, _, _, _, _, _, _, _, _, _, _, _),
	catSelect(CatNo),
	retractall(schemaWin(_, _)),
	assert(schemawin(Nulwin, CatNo)),
	win_Create(win_onderdeel_WinType,win_onderdeel_RCT,win_onderdeel_Title,
		   win_onderdeel_Menu,_Parent,win_onderdeel_Flags,win_onderdeel_eh,0), !.

%BEGIN Onderdeel, e_Create
  win_onderdeel_eh(_Win,e_Create(_),0):-!,
	retract(schemaWin(_, Cat)),
	assert(schemaWin(_Win, Cat)),
	wc(Cat, _,_,Onderdeel, _, _, _, _, _, _, _, _, _, _, _),
	%wc(Cat, Onderdeel,_,_,_,_,_),
	onderdeelWindowSize(_Win, Cat, Onderdeel),
%BEGIN Onderdeel, InitControls, 16:39:08-21.8.2004, Code automatically updated!
%END Onderdeel, InitControls
%BEGIN Onderdeel, ToolbarCreate, 16:39:08-21.8.2004, Code automatically updated!
%END Onderdeel, ToolbarCreate
	!.
%END Onderdeel, e_Create
%MARK Onderdeel, new events

%BEGIN Onderdeel, e_Destroy
  win_onderdeel_eh(_Win,e_Destroy,0):-
	write("\nCatUnSelect!!! Onderdeel"),
	catUnselect(),
	retractall(schemawin(_,_)),
	!.
%END Onderdeel, e_Destroy

%BEGIN Onderdeel, e_Char
  win_onderdeel_eh(_Win,e_Char('c',_ShiftCtlAlt),0):-
        win_onderdeelselectie_Create(_Win), !.
  win_onderdeel_eh(_Win,e_Char('\27',_ShiftCtlAlt),0):-
	win_Destroy(_Win), !.
  win_onderdeel_eh(_Win,e_Char('+',_ShiftCtlAlt),0):-
	schemaWin(_, CurrCat),
	open_nextCat('+',CurrCat), !.
  win_onderdeel_eh(_Win,e_Char('-',_ShiftCtlAlt),0):-
	schemaWin(_, CurrCat),
	open_nextCat('-',CurrCat), !.
%END Onderdeel, e_Char

%BEGIN Onderdeel, e_Update
  win_onderdeel_eh(Win,e_Update(UpdateRct),0):-
	schemaWin(Win, Cat),
	wc(Cat, _,_,Onderdeel, _, _, _, _, _, _, _, _, _, _, _),
	%wc(Cat, Onderdeel,_,_,_,_,_),
	win_GetTextExtent(Win, Onderdeel, -1, Width, Height),
	UpdateRct = rct(_, _, R, _B),
	draw_TextInRect(Win,rct(1,1,Width,Height), Onderdeel, -1,[dtext_left]),
	H2 = Height + Height,
	draw_TextInRect(Win,rct(1,Height,R,H2), "(nog) Niet ingedeeld.", -1,[dtext_left]),
	!.
%END Onderdeel, e_Update

%BEGIN Onderdeel, e_Size
  win_onderdeel_eh(_Win,e_Size(_Width,_Height),0):-!,
ifdef use_tbar
	toolbar_Resize(_Win),
enddef
	!.
%END Onderdeel, e_Size

%BEGIN Onderdeel, e_Menu, Parent window 
  win_onderdeel_eh(Win,e_Menu(ID,CAS),0):-!,
	PARENT = win_GetParent(Win),
	win_SendEvent(PARENT,e_Menu(ID,CAS)),
	!.
%END Onderdeel, e_Menu, Parent window

%END_WIN Onderdeel

