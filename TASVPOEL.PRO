/*******************************************************************************
 Project:  TASVPI
 FileName: TASVPOEL.PRO
 Purpose: No description
 Written by: JdL
 Comments: 5/12
******************************************************************************/

include "tasvp1.inc"
include "tasvp1.con"
include "hlptopic.con"

% =============================================================================
%    +------+------------------------^--+
%    |      |   KolHeader Area     Row0 |
%    +------+------------------------^--+
%    |      |                           |
%    | Row- |                           |
%    |Header|                           |
%    |      |        Wedstrijd-         |
%    | Area |          Item             |
%    |      |                           |
%    |      |          Area             |
%    |      |                           |
%    |      |                           |
%    <Kolom0>  1 element: Hoogte * Kolom|  Hoogte = kolomvert * fonthoogte + 3
%    +------+---------------------------+
%
% =============================================================================

constants

kolomvert  = 3
id_zoomin  = 4
id_zoomuit = 5

domains

tel = struct t(integer, integer)
telling = tel*
uitsltoken = games(integer, integer) ; tie(string) ; tekst(string) 
uitslagL = uitsltoken*

database - winpoel
 nondeterm partijC(WINDOW, integer Volgorde, tgnst Deelnemer, ilist PuntenSaldo, integer Plaatsing, integer AantalGespeeld)
 displayP(WINDOW, wedscat Onderdeel,integer OrigHor,integer OrigVert, integer CursorRow, integer CursorKol)
 schaal(WINDOW, integer Hoogte, integer Kolombreedte, integer Kolom0, integer Row0, 
      integer Marge, integer Schaal, integer Hor, integer Vert, integer Aantal) 
 %rank(WINDOW, integer Index, string Rank)
 %ranking(WINDOW, integerlist)
 determ afdrukmode(WINDOW)
 single metCursor(BOOLEAN, integer TimerId)
  besmetTeam(WINDOW, tgnst)
  lastKol(WINDOW, integer)
 single opTgnst(tgnst)
 %setsaldo(domijn PeerALLe, tgnst, integer)
 %gameSaldo(domijn, tgnst, integer)
 %ranknieuw(window, tgnst, integer Rank, domijn PeerAlle)
 %single ookNietPeer(boolean, tgl)
single ranknr(integer)
 
predicates
  toon_poule(wedscat, WINDOW, ulong PlanTM, PNT Origin, PNT Offset,FONT FONTHeader,integer MaxKols)
  onderdeelbreedte(wedscat Onderdeel, integer Size, integer Breedte)
  repRow1(wedscat,tgnst,string,string Plaatsing, integer, color)
  poulestand_init(WINDOW, wedscat, integer Aantal)
nondeterm  wedst(integer, wedscat, tgnst, tgnst, string, BOOLEAN LinksRects,string Notitie)  
  draaiom(string, string, string)
  draaiom1(uitslagL, string, string)
  add(ilist, integer, integer)
procedure punten1(integer Pouletelling, integer, integer Verschil, integer Punten)
nondeterm  games_sets(WINDOW, tgnst, tgl, wedscat, tel) - (i,i,i,i,o)
  rep_plan(integer,wedscat,tgnst,string,string HTML,tijd,BOOLEAN,integer OffsX, janee Oproepen, string Anker)
nondeterm  rep_p(WINDOW, wedscat, tgnst, tgnst, integer Punten, string Uitslag, tijd, integer WedsNo, integer OffsX,
       janee Oproepen, string HtmlAnker, string Winnaarklasse, string HTMLuitslag, string Notitie, integer Gespeeld)
        %- (i,i,i,i,o,o,i,i,i,i,o,o,o,o,o) ,(i,i,i,o,o,o,i,o,i,i,o,o,o,o, o) %,(i,i,i,i,o,o,i,o,i,i,o,o,o,o,o)
  rep_gewaarsch(integer, wedscat, tgnst, BOOLEAN, janee, janee, string, integer OffsX, janee Oproepen)
  rep_baan(string, string)
  procedure set_uitsl(integer, integer, integer, integer, integer, integer) - (i,i,i,o,i,o)
  rep_g(wedscat, tgnst, tgnst, integer, integer, integer, integer)
  cursor_limiet(WINDOW, integer In, integer Uit, integer Kol) - (i,i,o,i)
  init_cursor(WINDOW, integer No, wedscat, integer Row, integer Kol, integer Vooraan, tgnst Speler) - (i,i,i,o,o,i,i)
  cursor_Rect(WINDOW, RCT) - (i,o)
  cursor_Rect(WINDOW, RCT, integer Kol, integer Row) - (i,o,i,i)
  adjust(integer Rn, integer R, integer Ln, integer L,  integer Kolom0, integer Kolombreedte, integer Xin, integer Xuit) 
    - (i,i,i,i,i,i,i,o)
  changed(integer K, integer K, integer R, integer R) - (i,i,i,i)
  boundaryXY(WINDOW, RCT NewRct, RCT OldRct, integer Kol, integer Row) - (i,i,i,i,i)
  incr(integer CursorK, integer CursorR, WINDOW, integer Kol, integer Row) - (i,i,i,o,o)
  decr(integer CursorK, integer CursorR, integer Kol, integer Row) - (i,i,o,o)
  waarschuwbaar(WINDOW, integer Num, wedscat Cat, BOOLEAN PlanW)
  waarschuwbaar1(tgnst, tgnst, tgnst, janee, janee, BOOLEAN)
  procedure ordeVolg(integer, integer, integer, integer) - (i,i,o,o)
%  determ stand_update(WINDOW, wedscat)
  horlijn(WINDOW, integer, integer, integer, integer)
  addTussenstand(integer WdNo, wedscat CatNo, BOOLEAN LR, string Stand, string Notitie)
  procedure besmet(tgnst T, wedscat Cat, integer WedstrijdPunten, integer WedstrijdPunten1)
  gewaarsch(integer SpNo, tgnst, tgnst, janee, janee, janee, janee, tgnst) - (i,i,i,i,i,o,o,o)
  sp2t(integer, tgnst) 
  procedure pouleNietAf(integer, integer, integer, string)
  plaatsing(wedscat Cat, tgnst T, string Plaatsing, color)
  determ krijgtPunten(WINDOW, tgnst T, tgnst T2,string Klasse, integer Gewonnen, integer Gewonnenu)
  nondeterm tgenerator(window, wedscat Cat, tgnst T, integer Plaatsing)
  procedure partijCopVolgorde(window)
procedure  tokenizeU(string In, uitslagL)
procedure uitsl_games1(uitslagL Uitslag, integer, integer, integer, integer, integer, integer, integer, integer) - (i,i,o,i,o,i,o,i,o)
procedure sortOpPunten(window, wedscat)
%procedure sortOpPunten1(window, wedscat)        % domijn is punten van alleen peers of van allemaal 
procedure sortopSets(window, wedscat, tgl PeerList, integer Punten)
nondeterm setsaldoIs(window, wedscat, tgnst, tgl, integer)
procedure addsaldo(telling, integer, integer, integer, integer)
procedure sortopGames(window, wedscat, tgl, integer Punten, integer Setsaldo)
procedure setsGamesAlle(window Win, wedscat CatNo, tgl PeerList, integer Punten, integer SetSaldo, integer Gamesaldo)
nondeterm gamesaldoIs(window, wedscat, tgnst, tgl, integer)
%procedure tgl2groep(window, tgl, integerlist)
%procedure deRank(integerlist, integer)
procedure deRankDisp(integer Rank, integer Punten, string) 
%procedure setOokNietPeer(boolean, domijn PeerAlle, tgl TgsL)
%procedure herzieRanking(window, tgl)
%procedure verplaats(window, integer Rank, integer Gedaan)
procedure qsortRank(window,tgl,tgl)	        % algehele sort 
procedure splitRank(window, tgnst, tgl, tgl, tgl)
procedure puntenGem(ilist, integer, string)
procedure puntenStr(integer In, string Uit, string Besmet)
  nondeterm nietKlaar(wedscat, tgnst, integer) - (i, i, o)
  nondeterm poulenietKlaar(wedscat, integer) - (i,o)
determ echtKoppel(tgnst)
procedure showAll(window, string)
procedure showall1(window, string)
determ isMeerdere(ilist P1, ilist P2) - (i,i)
determ isregulier(ilist Punten) - (i) % geen besmet e.d.
procedure puntendisp(integer Aantalgespeeld, ilist, slist)
procedure puntendisp1(ilist, slist)
procedure plaatsingorde(integer, integer)
determ dubbel(integerlist, integer)
procedure gaatje(integerlist, integer) 

clauses

metCursor(b_True, 0).
opTgnst(onbekend).
%ookNietPeer(b_false, []).
ranknr(1).

sp2t(No, s(No)) :- !.
sp2t(No, pw(No)) :- !.
sp2t(No, p(No, _)) :- !.
sp2t(No, p(_, No)).

gewaarsch(SpNo, T1, _, _, G2, ja, G2,T1) :-
  sp2t(SpNo, T1), !.
gewaarsch(SpNo, _, T2, G1, _, G1, ja, T2) :-
  sp2t(SpNo, T2), !.

poule_ranking_alle(Cat, Rank, Tgnst, Punten) :-	% extern nondeterm!
  NulWin = cast(WINDOW, 0),
  poulestand_init(NulWin, Cat,_),
  partijC(NulWin, Rank, Tgnst, [Punten|_], _, _).
poule_ranking_alle(_Cat, _Rank, _Tgnst, _Punten) :-	
  NulWin = cast(WINDOW, 0),
  retractall(besmetTeam(NulWin, _)),
  retractall(partijC(NulWin,_,_,_,_,_), winpoel),
  retractall(lastKol(NulWin, _)),
  fail. 

pouleNietAf(Diff,_, _, "niet geldig") :-
  Diff < 2,  !.
pouleNietAf(_,_, 0, "") :- !.
pouleNietAf(_,0, _, "") :- !.
pouleNietAf(_,_, _, "voorlopig)").

poule_ranking1(Cat, Rank, Tgnst, Saldo, NK) :-	% extern!
  NulWin = cast(WINDOW, 0),
  assert(ranknr(1)),
  retractall(partijC(NulWin,_,_,_,_,_), winpoel),
  poulestand_init(NulWin, Cat,AantalPC),
  findall(Team, besmetteam(NulWin, Team), TeamL),
  count(TeamL, 0, AantalPB),
  Diff = AantalPC - AantalPB,
  partijC(NulWin, Rank, Tgnst, Saldo, _, _), 
  retractall(partijC(NulWin,_,_,_, _,_), winpoel),
  findall(Pos, nietKlaar(Cat, Tgnst, Pos), PosList),
  findall(Pos1, poulenietklaar(Cat, Pos1), Pos1List),
  count(PosList, 0, Aantal),
  count(Pos1List, 0, Aantal1),
  pouleNietAf(Diff,Aantal, Aantal1, NK), !.

nietKlaar(Cat, T1, Pos) :-
  wd(Pos, Cat, _T2, T1, _Uitsl, _, _),
  not(w3(Pos, Cat,T1,_,_,_,_)).
nietKlaar(Cat, T2, Pos) :-
  wd(Pos, Cat, T2, _T1, _Uitsl, _, _),
  not(w3(Pos, Cat,T2,_,_,_,_)).

pouleNietKlaar(Cat, Pos) :-
  wd(Pos, Cat, _T2, _T1, _Uitsl, _, _),
  not(w3(Pos, Cat,_,_,_,_,_)).
pouleNietKlaar(Cat, Pos) :-
  wd(Pos, Cat, _T2, _T1, _Uitsl, _, _),
  not(w3(Pos, Cat,_,_,_,_,_)).

close_poules :-	% global 
  schaal(Win, _Hoogte, _Kolombreedte, _Kolom0, _Row0, _Marge, _Schaal, _, _, _),
  win_Destroy(Win),
  fail.
close_poules.

setPouleNaam(Cat, Naam) :-
  displayP(Win, Cat,_OrigX,_OrigY,_CursorR, _CursorK),
  win_SetText(Win, Naam), !.
setPouleNaam(_, _).

update_poule() :-
  displayP(Win, _CatNo,_OrigX,_OrigY,_CursorR, _CursorK),
  win_Invalidate(Win), 
  fail.
update_poule().

update_poule(CatNo) :-
  displayP(Win, CatNo,_OrigX,_OrigY,_CursorR, _CursorK),
  win_Invalidate(Win), !.
update_poule(_).

horLijn(Win, Hor, OffsX, _OriginX, Hor1) :-
  afdrukmode(Win), !,
  Hor1 = Hor + OffsX.
horLijn(_, Hor, OffsX, OriginX, Hor1) :-
  Hor1 = Hor - OffsX + OriginX.

puntenGem([Punten|_], AG, Disp) :-
  AG > 0,
  Punten >= 0,
  GemPt = Punten / AG,
  format(Disp1, "%-2.1f", GemPt),
  searchchar(Disp1,'.',FoundPos),
  FP = FoundPos - 1,
  frontstr(FP,Disp1,DispA,RestString),
  frontchar(Reststring, _, DispB), !,
  format(Disp, "%s,%s", DispA, DispB).
puntenGem(_Punten, _AG, "-").

puntenStr(In, Uit, "") :-
  In >= 0, !,
  str_int(Uit, In).  
puntenStr(In, "", "") :-
  In = -99, !.
puntenStr(_In, "-", "punten ongeldig").

puntenDisp(Gespeeld, [H|T], [ Pu | Uit]) :-
  Gespeeld > 0,
  H >= 0,
  str_int(Pu, H), !,
  puntendisp1(T, Uit).
puntenDisp(Gespeeld, [H|_], ["-"]) :-
  Gespeeld > 0,
  H < 0, !.
puntenDisp(_, _, []).  

puntenDisp1([H|T], [Pu | Uit]) :-
  str_int(Pu, H), !,
  puntendisp1(T, Uit).
puntenDisp1(_, []).  

gaatje([L], Gat ) :- 
  Gat = L + 1,!.
gaatje([A,B|_], Gat) :-
  B > A + 1, !,
  Gat = A + 1.
gaatje([_ | List], Gat) :-
  gaatje(List, Gat).  
  
dubbel([A,A|_], A) :- !.
dubbel([_| List], A) :-
  dubbel(List,A).
/*  
toon_poule(Cat,_, _, _,_,_,_) :-		% repair
  findall(Nr, wd(Nr,Cat,_T1,_T2,_Uitsl,_LogTijd,_UserNote),Nrs),
  sortint_c(Nrs),
  dubbel(Nrs, Dubl),
  gaatje(Nrs, Gat),
  wd(Dubl,Cat,T1,T2,Uitsl,LogTijd,UserNote),
  logf('R', wd(Dubl,Cat,T1,T2,Uitsl,LogTijd,UserNote), ja),
  logf('Z', wd(Gat,Cat,T1,T2,Uitsl,LogTijd,UserNote), nee),
  fail. */
toon_poule(_,Win, _PlanTM, pnt(_OriginX,OriginY),pnt(OffsX,OffsY),_,_MaxKols) :-	% rowheaders
  not(afdrukmode(Win)),
  schaal(Win, _Hoogte, _Kolombreedte, Kolom0, Row0, _Marge, _Schaal, _Hor, Vert, _),
  Vert1 = Vert + OffsY + OriginY ,
  Kolom0Offs = Kolom0 + OffsX,
  draw_Line(Win,pnt(Kolom0Offs,OffsY),pnt(Kolom0Offs,Vert1)),			% vert
  win_GetTextExtent(Win, "Pnt/Sets/Games", -1, Width, Height),
  win_GetTextExtent(Win, "Gmdld.", -1, Width2, _Height2),
  Y1h = OffsY + Row0,
  X1h = OffsX + Kolom0,
  X2k = X1h - Width,
  X1k = X2k - Width2,
  Yk = Y1h - Height,
  draw_PolyLine(Win,[pnt(X1k,Y1h),pnt(X1k,Yk),pnt(X2k,Yk),pnt(X2k,Y1h),pnt(X2k,Yk),pnt(X1h,Yk)]),	% kader voor punten
  draw_TextInRect(Win, rct(X1k, Yk, X2k, Y1h), "Gmdld.", -1, [dtext_singleline, dtext_vcenter, dtext_center]),
  draw_TextInRect(Win, rct(X2k, Yk, X1h, Y1h), "Pnt/Sets/Games", -1, [dtext_singleline, dtext_vcenter, dtext_center]),
  fail.
toon_poule(Cat,Win, _PlanTM, pnt(OriginX,OriginY),pnt(OffsX,OffsY),_,_MaxKols) :-	% rowheaders
  schaal(Win, Hoogte, _Kolombreedte, Kolom0, Row0, _Marge, _Schaal, Hor, Vert, _),
  Vert1 = Vert + OffsY + OriginY ,
  Kolom0Offs = Kolom0 + OffsX,
  draw_Line(Win,pnt(Kolom0Offs,OffsY),pnt(Kolom0Offs,Vert1)),			% vert
  win_GetTextExtent(Win, "Pnt/Sets/Games", -1, Width, Height),
  win_GetTextExtent(Win, "Gmdld.", -1, Width2, _Height2),
  X0a = OffsX ,
  X1a = Kolom0 + X0a,
  win_GetTextExtent(Win, "99", -1, Width1, _Height1),
  X0b = X0a + Width1,
  partijC(Win, No, Tg, Punten,_, AG),
    %str_int(RankDisp, AG),							% loop
    %derankdisp(No, Punten, Ranking),
    repRow1(Cat, Tg, Naam, Plaats, 0, Kleur),
    repRow1(Cat, Tg, Naam1, _, 1, _),
    Yn  = (No - 1) * Hoogte + OriginY + Row0 + OffsY,
    Yn >= Row0, %$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    Yn1 = Yn + Hoogte,
    Yn2 = Yn1 - Height,
    format(Str, "%s\n%s", Naam, Naam1),						% de namen
    str_int(NoStr, No),
    %Hor1 = Hor - OffsX + OriginX,
    horLijn(Win, Hor, OffsX, OriginX, Hor1),
    draw_Line(Win,pnt(OffsX,Yn1),pnt(Hor1,Yn1)),					% hor lijn
    draw_TextInRect(Win, rct(X0a, Yn, X0b, Yn1), NoStr, -1, [dtext_left]),
    win_SetForeColor(Win, Kleur),
    draw_TextInRect(Win, rct(X0b, Yn, X1a, Yn1), Str, -1, [dtext_left]),
    win_SetForeColor(Win, color_Black),
    draw_TextInRect(Win, rct(X0b, Yn, X1a, Yn2), Plaats, -1, [dtext_singleline, dtext_right, dtext_bottom]),
    not(afdrukmode(Win)),
    X0c = X1a - Width,	% 26.1.2017
    X0d = X0c - Width2,
    Y0c = Yn1 - Height,
    draw_PolyLine(Win,[pnt(X0d,Yn1),pnt(X0d,Y0c),pnt(X0c,Y0c),pnt(X0c,Yn1),pnt(X0c,Y0c),pnt(X1a,Y0c)]),	% kader voor punten
    puntendisp(AG, Punten, PU),
    list_to_string(PU, " ", PntS),
    draw_TextInRect(Win, rct(X0c, Y0c, X1a, Yn1), PntS, -1, [dtext_singleline, dtext_vcenter, dtext_center]),
    puntenGem(Punten, AG, RankDisp),
    %format(Saldo(
    format(Saldo, "%s", Rankdisp),
    trim_c(Saldo),
    %draw_TextInRect(Win, rct(X0d, Y0c, X0c, Yn1), RankDisp, -1, [dtext_singleline, dtext_vcenter, dtext_center]),
    draw_TextInRect(Win, rct(X0d, Y0c, X0c, Yn1), Saldo, -1, [dtext_singleline, dtext_vcenter, dtext_center]),  % Saldo
  fail.
toon_poule(_,Win, _PlanTM, pnt(OriginX,OriginY),pnt(OffsX,OffsY),_, MaxKols) :-	% kolheaders
  schaal(Win, _Hoogte, Kolombreedte, Kolom0, Row0, _Marge, _Schaal, Hor, Vert, _AantalX),
  KolOs = OriginX div Kolombreedte,
  Row0Offs = Row0 + OffsY,
  horLijn(Win, Hor, OffsX, OriginX, Hor1),
  draw_Line(Win,pnt(OffsX,Row0Offs),pnt(Hor1,Row0Offs)),		% hor één lijn
  partijC(Win, KolNo, _Tg, _, _, _),
  KolNo <= MaxKols - KolOs,
  X0a  = OriginX + Kolom0 + (KolNo - 1) * Kolombreedte + OffsX ,
  X0a >= Kolom0 + OffsX,  %&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  Y0a = OffsY,
  X1a = X0a + Kolombreedte,
  Y1a = Row0 + Y0a,
  str_int(Str, KolNo),
  Vert1 = Vert + OffsY + OriginY,
  draw_Line(Win,pnt(X1a,OffsY),pnt(X1a,Vert1)),				% verticale lijnen
  draw_TextInRect(Win, rct(X0a, Y0a, X1a, Y1a), Str, -1, [dtext_center]),
  fail.
toon_poule(_Cat,Win, _PlanTM,pnt(OrigX,OrigY),pnt(OffsX,OffsY),_,MaxKols) :-	% kolheader punten
  afdrukmode(Win),
  schaal(Win, _Hoogte, Kolombreedte, Kolom0,Row0, _Marge, _Schaal, _Hor, Vert, _),
  KolO = OrigX div Kolombreedte,	% (zijn negatief)
  lastKol(Win, KolLast),
  KolNo = KolLast + 1,
  KolNo <= MaxKols - KolO,
  X0a  = OrigX + Kolom0 + (KolNo - 1) * Kolombreedte + OffsX ,
  X0a >= Kolom0 + OffsX,  %&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  Y0a = OffsY,
  X1a = X0a + Kolombreedte,
  Y1a = Row0 + Y0a,
  Vert1 = Vert + OffsY + OrigY,
  draw_Line(Win,pnt(X1a,OffsY),pnt(X1a,Vert1)),				% verticale lijnen
  draw_TextInRect(Win, rct(X0a, Y0a, X1a, Y1a), "Punten Rangorde", -1, [dtext_center]),
  fail.
toon_poule(_,Win, _PlanTM, pnt(OriginX,OriginY),pnt(OffsX,OffsY),_, MaxKols) :-	% kolheaders
  schaal(Win, _Hoogte, Kolombreedte, Kolom0, Row0, _Marge, _Schaal, Hor, Vert, _),
  KolOs = OriginX div Kolombreedte,
  Row0Offs = Row0 + OffsY,
  horLijn(Win, Hor, OffsX, OriginX, Hor1),
  draw_Line(Win,pnt(OffsX,Row0Offs),pnt(Hor1,Row0Offs)),		% hor één lijn
  partijC(Win, KolNo, _Tg, _, _, _), 
  KolNo <= MaxKols - KolOs,
  X0a  = OriginX + Kolom0 + (KolNo - 1) * Kolombreedte + OffsX ,
  X0a >= Kolom0 + OffsX,  %&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  Y0a = OffsY,
  X1a = X0a + Kolombreedte,
  Y1a = Row0 + Y0a,
  str_int(Str, KolNo),
  Vert1 = Vert + OffsY + OriginY,
  draw_Line(Win,pnt(X1a,OffsY),pnt(X1a,Vert1)),				% verticale lijnen
  draw_TextInRect(Win, rct(X0a, Y0a, X1a, Y1a), Str, -1, [dtext_center]),
  fail.
%#######
toon_poule(Cat,Win, PlanTM,pnt(OrigX,OrigY),pnt(OffsX,OffsY),_,MaxKols) :-	% rowelementen
  Font = win_GetFont(Win),
  _FontName = font_GetAttrs(Font, Style, Size),
  Siz1 = Size - 1,
  NewFont = font_SetAttrs(Font, Style, Siz1),
  schaal(Win, Hoogte, Kolombreedte, _Kolom0B,_Row0H, _Marge, _Schaal, _Hor, _Vert, _),
  RowO = OrigY div Hoogte,				% terugrekenen naar row en kolom
  KolO = OrigX div Kolombreedte,
  partijC(Win, Row, T1, _, _, _),
    Row > -RowO,
    partijC(Win, Kol, T2, _, _, _),
      Kol > -KolO,
      Kol <= MaxKols,
      oproepen(Oproepen),
      getbacktrack(Here),
      rep_p(Win, Cat, T1, T2, WPunten, Str, PlanTM, _WdNo, OffsX,Oproepen,_,_,_,Noti,_),
      cutbacktrack(Here),
      cursor_Rect(Win, Rct, Kol, Row),
      Rct1 = rect_Inflate(Rct, -1, -1),
      Rct2 = rect_Offset(Rct1, OffsX, OffsY),
      draw_TextInRect(Win, Rct2, Str, -1, [dtext_left,dtext_wordbreak]),
      win_SetForeColor(Win, color_Red),
      win_SetFont(Win, NewFont),
      %format(NotiTest, "%s%d", Noti, WdNo),
      draw_TextInRect(Win, Rct2, Noti, -1, [dtext_bottom,dtext_singleline,dtext_left]),
      win_SetFont(Win, Font),
      win_SetForeColor(Win, color_Black),
      draw_TextInRect(Win, Rct2, "   ", -1, [dtext_bottom,dtext_singleline,dtext_right]),
      WPunten > 0,
      format(PtS,"%d ",WPunten),
      draw_TextInRect(Win, Rct2, PtS, -1, [dtext_bottom,dtext_singleline,dtext_right]),
  fail.
%###############
toon_poule(_Cat,Win, _PlanTM,pnt(OrigX,OrigY),pnt(OffsX,OffsY),_,MaxKols) :-	% print the results in last column
  afdrukmode(Win),
  schaal(Win, Hoogte, Kolombreedte, _Kolom0B,_Row0H, _Marge, _Schaal, _Hor, _Vert, _),
  RowO = OrigY div Hoogte,		% terugrekenen naar rownr en kolomnr
  KolO = OrigX div Kolombreedte,	% (zijn negatief)
  lastKol(Win, KolM),
  Kol = KolM + 1,			% de kolom met resultaten
  Kol <= MaxKols - KolO,		%####################################################################
  partijC(Win, Row, _T1, [Punten|_], _, _),	%#################################################################### derankdisp
    Row > -RowO,		
    %rankNieuw(Win, T1, _, Rank, peer) ,
    derankdisp(Row, Punten, RankDisp), 
    format(Str,"%d",Punten),
    cursor_Rect(Win, Rct, Kol, Row),
    %write(" ", RCT),
    Rct1 = rect_Inflate(Rct, -1, -1),
    Rct2 = rect_Offset(Rct1, OffsX, OffsY),
    Rct2 = rct(Links,Top,Rechts,Bottom),
    Midden = (Links + Rechts) div 2,
    RctL = rct(Links, Top, Midden, Bottom),
    RctR = rct(Midden, Top, Rechts, Bottom),
    draw_TextInRect(Win, RctL, Str, -1, [dtext_center,dtext_singleline,dtext_vcenter]),
    draw_TextInRect(Win, RctR, RankDisp, -1, [dtext_center,dtext_singleline,dtext_vcenter]),
  fail.
toon_poule(_Cat,Win,  _PlanTM, pnt(OrigX,OrigY), pnt(OffsX,OffsY), _, MaxKols) :-	% row = kol de grijze vakjes
  schaal(Win, Hoogte, Kolombreedte, _Kolom0, _Row0, _Marge, _Schaal,_Hor,_Vert, _),
  CurrentBrush = win_GetBrush(Win),
  CurrentPen = win_GetPen(Win),
  RowO = -1 * (OrigY div Hoogte),		% terugrekenen naar row en kolom
  KolO = -1 * (OrigX div Kolombreedte),
  partijC(Win, Row, _T1, [Punten|_], _, _),			% ga alle nummers langs
    Row > RowO,
    Row > KolO,
    Kol = Row,
    Kol <= MaxKols + KolO,
    win_SetBrush(Win,brush(pat_Solid,color_LtGray)),
    win_SetPen(Win, pen(1, ps_Hollow, color_Red)),
    cursor_Rect(Win, Rct2, Kol, Row),
    Rct3 = rect_Offset(Rct2, OffsX, OffsY),
    puntenstr(Punten, _, BText),
    draw_Rect(Win, Rct3),
    draw_TextInRect(Win, Rct3, BText, -1, [dtext_center,dtext_singleline,dtext_vcenter]),
    win_SetBrush(Win,CurrentBrush),
    win_SetPen(Win,CurrentPen),
  fail.
toon_poule(_Cat,Win, _PlanTM, _,pnt(OffsX,_OffsY),_, _) :-
  not(afdrukmode(Win)),
  metCursor(b_True, _),
  displayP(Win, _Cat, _OrigX, _OrigY, RowIn, Kol),
  cursor_limiet(Win, RowIn, Row, Kol), 
  partijC(Win, Row, T1, _, _, _),
  partijC(Win, Kol, T2, _, _, _),
  oproepen(Oproepen),
  getbacktrack(Here),
  rep_p(Win, _Cat, T1, T2, Punten, Str, alleplanning, _WedsNo, OffsX, Oproepen, _,_,_,Noti, _),
  cutbacktrack(Here),
  SaveTools = win_GetDrawTools(Win),
  SaveTools = draw_tools(_Pen, _Brush, _Drawmode, _Font, _Foreground, _BackGround, _BK_MODE),
  win_SetForeColor(Win, color_White),
  win_SetBackColor(Win, color_Blue),
  win_SetBackMode(Win, bk_Opaque),
  win_SetBrush(Win,brush(pat_Solid, color_Blue)),
  %getbacktrack(Here),
  cursor_Rect(Win, CursorRect),
  draw_Rect(Win, CursorRect),
  format(PtS,"%d ",Punten),		
  draw_TextInRect(Win, CursorRect, PtS, -1, [dtext_bottom,dtext_singleline,dtext_right]),
  draw_TextInRect(Win, CursorRect, Noti, -1, [dtext_bottom,dtext_singleline,dtext_left]),
  draw_TextInRect(Win, CursorRect, Str, -1, [dtext_left,dtext_wordbreak]),
  win_SetForeColor(Win, _Foreground),
  win_SetBackColor(Win, _Background),
  win_SetBackMode(Win,_BK_MODE),
  win_SetBrush(Win, _Brush), !.
toon_poule(CatNo,Win, _PlanTM,_,pnt(OffsX,OffsY),Font_header,_MaxKols) :-	% igv print een kader
  afdrukmode(Win),
  %OffsX > 0,
  SaveTools = win_GetDrawTools(Win),
  SaveTools = draw_tools(_Pen, _Brush, _Drawmode, _Font, _Foreground, _BackGround, _BK_MODE),
  %afmeting(Win, MaxKols, Hor, Vert),
  schaal(Win, _RegelHoogte, _Kolombreedte, _Kolom0, Row0,_Marge, _Schaal, Hor, Vert, _),
  X0 = OffsX - 2,
  X1 = X0 + Hor + 4,
  Y0 = OffsY - 2,
  Y1 = Y0 + Vert + 4,
  Y2 = Y0 - Row0 - 10,
  win_SetBrush(Win, brush(pat_Hollow, color_White)),
  draw_Rect(Win, rct(X0, Y2, X1, Y1)),
  %write("\nY1: ", Y1), 
  draw_Line(Win, pnt(X0, Y0), pnt(X1, Y0)),
  wc(CatNo, _, _, Onderdeel, _, _NaamKortS, _, _, _, _, _, _, _, _, _),
  %wc(CatNo, Onderdeel,_,_,_,_,_),
  Font_norm = win_GetFont(Win),
  win_SetFont(Win, Font_header),
  Yh = Y2 + Row0 + 10,
  draw_TextInRect(Win, rct(X0, Y2, X1, Yh), Onderdeel, -1, [dtext_singleline,dtext_vcenter,dtext_center]),
  win_SetFont(Win, Font_norm),
  !.
toon_poule(_,_Win,_,_,_,_,_) :- !.

cursor_Rect(Win, Rect) :-
  displayP(Win, _Cat, OrigX, OrigY, Row, Kol),		
  schaal(Win, Hoogte, Kolombreedte, Kolom0,Row0,Marge,_Schaal,_Hor,_Vert, _),
  Rect1 = rct(0, 0, KolomBreedte, Hoogte),
  Rect2 = rect_Inflate(Rect1, Marge, Marge),
  OffX = (Kol - 1) * Kolombreedte + Kolom0 + OrigX + 2,
  OffY = (Row - 1) * Hoogte + Row0 + OrigY + 1,
  Rect = rect_Offset(Rect2, OffX, OffY), !.

cursor_Rect(Win, Rect, Kol, Row) :-  		% de vlakjes
  displayP(Win, _Cat, OrigX, OrigY, _, _),
  schaal(Win, Hoogte, Kolombreedte, Kolom0,Row0,_Marge,_Schaal,_Hor,_Vert, _),
  Rect1 = rct(0, 0, KolomBreedte, Hoogte),
  Rect2 = rect_Inflate(Rect1, 0, 0),
  OffX = (Kol - 1) * Kolombreedte + Kolom0 + OrigX + 1,
  OffY = (Row - 1) * Hoogte + Row0 + OrigY + 1,
  Rect = rect_Offset(Rect2, OffX, OffY), !.
  
cursor_limiet(Win, RowIn, RowIn, Kol) :-	% - (i,i,o,i)
  lastKol(Win, Aantal),
  RowIn <= Aantal, 
  RowIn <> Kol, !.					% niet op diagonaal (x,x)
cursor_limiet(Win, RowIn, RowUit, Kol) :-
  lastKol(Win, Aantal),
  RowUit = RowIn + 1,				% dan één verder
  RowUit <= Aantal,				% niet te ver
  RowUit <> Kol, !.				% redundant
cursor_limiet(_, 1, 2, 1) :- !.			% zeker niet (1,1)
cursor_limiet(_, _, 1, _).

partijCopVolgorde(Win) :-
  assert(ranknr(1)),
  findall(Tgnst0, partijC(Win, _, Tgnst0, _, _, _), TgnstList0),
  qsortRank(Win, TgnstList0, TgnstListS),
  member(Tgnst, TgnstListS),
  ranknr(RankI),
  getbacktrack(Here),
  retract(partijC(Win, _, Tgnst, PuntenSaldo, Plaatsing, AG)), 
  cutbacktrack(Here),
  %deRankdisp(RankI,Punten, PDisp), 
  assertz(partijC(Win, RankI, Tgnst, Puntensaldo, Plaatsing, AG)),
  RNk1 = RankI + 1,
  assert(ranknr(Rnk1)),
  fail.
partijCopVolgorde(Win) :-
  ranknr(RN1),
  RN = RN1 - 1,
  assert(lastKol(Win,RN)).

tgenerator( _, Cat, T, Plts) :-
  opg(_OpgNum, Cat, T, Plts,_,_,_,_,_,_,_),
  not(T = openplaats(_)),
  in_weds(Cat, T).
tgenerator(Win, Cat, openplaats(I), 999) :-
  findall(IX, wedst(_,Cat,openplaats(IX),_,_,_,_), IXL),
  sortint_c(IXL),
  member(I, IXL),
  not(partijC(Win, _, openplaats(I), _,  _, _)).

poulestand_init(Win, Cat, _) :-				% Wie is voor dit nummer opgesteld?
  assert(ranknr(1)),
  retractall(partijC(Win,_,_,_,_,_), winpoel),
  retractall(besmetTeam(Win, _)),
  opg(_OpgNum, Cat, T, _,_,_,_,_,_,_,_),
  besmet(T, Cat, 1, N),
  N = -1,
  not(besmetTeam(Win, T)),
  assert(besmetTeam(Win, T)),
  fail.
poulestand_init(Win, Cat, _) :-				% Wie is voor dit nummer opgesteld?
  tgenerator(Win, Cat, T, Plaatsing),
  findall(Punt, rep_p(Win, Cat, T, _, Punt, _, 0, _,0,ja, _,_,_,_,_), PuntenLijst),
  findall(Gesp, rep_p(Win, Cat, T, _, _, _, 0, _,0,ja, _,_,_,_,Gesp), GespLijst),
  not(PuntenLijst = []),				% moet wel opgesteld staan
  add(PuntenLijst, 0, WedstrijdPunten),
  telop(GespLijst, 0, AG),
  besmet(T, Cat, WedstrijdPunten, WedstrijdPunten1),	% besmettest
  asserta(partijC(Win, 111, T, [WedstrijdPunten1], Plaatsing, AG)),
  fail.
poulestand_init(Win, CatNo, Aantal) :-				% zoek de ranking uit
  findall(T, partijC(Win, T, _, _, _, _), TList),
  count(TList, 0, Aantal),
  sortopPunten(Win, CatNo),	% 29.6.2011
  partijCopVolgorde(Win), !.
poulestand_init(_, _,0).

showall(Win, Tekst) :-
  writedevice(X),
  writedevice(screen),
  showall1(Win, Tekst),
  writedevice(X), !.  

showAll1(Win, Tekst) :-
  %dlg_note(Tekst),
  write("\n", Win, " ", Tekst),
  partijC(Win, Nr, Tgnst, Puntensaldo, Plaatsing, Aantalweds),
  write("\npartijC:\t",Nr, "\tTgnst: ", Tgnst, "\tPlaatsing: ",Plaatsing, "\tAantalweds: ", Aantalweds),
  member(Punten, Puntensaldo),
  write("\t",Punten),
  fail. 
showall1(Win, _) :-
  besmetTeam(Win, Team),
  write("\nbesmetTeam: ", Team),
  fail.
showall1(_Win, _).

isMeerdere([P1 | _Rest1], [P2 | _Rest2]) :-
  P1 > P2, !. 
isMeerdere([P1 | _Rest1], [P2 | _Rest2]) :-
  P1 < P2, !, fail. 
isMeerdere([_ | Rest1], [_ | Rest2]) :-
  isMeerdere(Rest1, Rest2).

isregulier([Punten|_]) :-  % geen besmet e.d.
  Punten >= 0. 

plaatsingorde(0, 99) :- !.
plaatsingorde(Pl, Pl).

splitRank(_, _, [], [], []).
splitRank(Win, Tgnst1, [Tgnst2 | X], Y, [Tgnst2 | Z]) :-          % geen wedstrijden maar geplaatst 
  partijC(Win, _, Tgnst1, PuntenSaldo1, Plts1, 0),
  isRegulier(PuntenSaldo1),
  partijC(Win, _, Tgnst2, PuntenSaldo2, Plts2, 0),
  isRegulier(PuntenSaldo2),
  plaatsingorde(Plts1, Plts1O),
  plaatsingorde(Plts2, Plts2O),
  Plts1O < Plts2O, !,
  splitRank(Win, Tgnst1, X, Y, Z).
splitRank(Win, Tgnst1, [Tgnst2 | X], [Tgnst2 | Y], Z) :-          % geen wedstrijden maar lager geplaatst 
  partijC(Win, _, Tgnst1, PuntenSaldo1, _Plts1, 0),
  isRegulier(PuntenSaldo1),
  partijC(Win, _, Tgnst2, PuntenSaldo2, _Plts2, 0),
  isRegulier(PuntenSaldo2), !,
  splitRank(Win, Tgnst1, X, Y, Z).
splitRank(Win, Tgnst1, [Tgnst2 | X], Y, [Tgnst2 | Z]) :-          % als T1, T2 regulier en winnaar (meerdere) 
  partijC(Win, _, Tgnst1, PuntenSaldo1, _, _),
  isRegulier(PuntenSaldo1),
  partijC(Win, _, Tgnst2, PuntenSaldo2, _, _),
  isRegulier(PuntenSaldo2),
  isMeerdere(PuntenSaldo1, PuntenSaldo2), !,
  splitRank(Win, Tgnst1, X, Y, Z).
splitRank(Win, Tgnst1, [Tgnst2 | X], [Tgnst2 | Y], Z) :-          % als T1, T2 reguliern 
  partijC(Win, _, Tgnst1, PuntenSaldo1, _, _),
  isRegulier(PuntenSaldo1),
  partijC(Win, _, Tgnst2, PuntenSaldo2, _, _),
  isRegulier(PuntenSaldo2), !,
  splitRank(Win, Tgnst1, X, Y, Z).
splitRank(Win, Tgnst1, [Tgnst2 | X], Y, [Tgnst2 | Z]) :-          % als T1 regulier 
  partijC(Win, _, Tgnst1, PuntenSaldo1, _, _),
  isRegulier(PuntenSaldo1), !,
  splitRank(Win, Tgnst1, X, Y, Z).
splitRank(Win, Tgnst1, [Tgnst2 | X], [Tgnst2 | Y], Z) :-          % als T2 regulier 
  partijC(Win, _, Tgnst2, PuntenSaldo2, _, _),
  isRegulier(PuntenSaldo2), !,
  splitRank(Win, Tgnst1, X, Y, Z).
splitRank(Win, Tgnst1, [Tgnst2 | X], [Tgnst2 | Y], Z) :-		% alle andere	
  writedevice(XY),
  writedevice(screen),
  write("\nonbekend geval!"),
  writedevice(XY),
  splitRank(Win, Tgnst1, X, Y, Z).

qsortRank(_Win,[],[]).	        % sorteer zó dat geplaatsten bovenaan staan bij 0 punten 
qsortRank(Win,[H|T],S) :-
  splitRank(Win,H,T,A,B),
  qsortRank(Win, A,A1),
  qsortRank(Win, B,B1),
  append(A1,[H|B1],S),  !.


sortOpPunten(Win, CatNo) :-
  findall(Puntenx, partijC(Win, _, _, [Puntenx | _], _, _), Puntenlist),
  sortint_c(Puntenlist),
  removedubl(PuntenList,[], PLRR),
  member(Punten, PLRR),
  findall(Tgnst, partijC(Win, _, Tgnst, [Punten| _], _, _),PeerList),
  not(PeerList = [_]),
  sortopSets(Win, CatNo, PeerList, Punten),
  fail.
sortOpPunten(_Win, _).

sortopSets(Win, Catno, PeerList, _) :-
  member(Tgnst, PeerList),
  getbacktrack(Here),
  retract(partijC(Win, Rowx, Tgnst, Puntensaldo, Plaatsing, Aantalgespeeld)),
  cutbacktrack(Here),
  setSaldoIs(Win, CatNo, Tgnst, PeerList, SetSaldo),
  append(Puntensaldo, [SetSaldo], PuntenSaldoN),
  assertz(partijC(Win, Rowx, Tgnst, PuntenSaldoN, Plaatsing, AantalGespeeld)),
  fail.
sortopSets(Win, Catno, _, Punten) :-
  findall(SetSaldo, partijC(Win, _, _, [Punten, SetSaldo | _], _, _), SaldoList),
  sortint_c(SaldoList),
  removedubl(SaldoList, [], SaldoRL),
  member(SaldoR, SaldoRL),
  findall(Tgnst, partijC(Win, _Rowx, Tgnst, [Punten, SaldoR | _], _, _),PeerList),
  not(PeerList = [_]),	% 25.1.2017 maar één member!
  sortopgames(Win, CatNo, PeerList, Punten, Saldor), 
  fail.
sortopSets(_Win, _Catno, _PeerList, _).  
  
setsaldoIs(Win, CatNo, T1, TgL, SetSaldo) :-
  findall(Saldo, games_sets(Win, T1, TgL, CatNo, Saldo), SaldoL),
  addsaldo(SaldoL, 0, SetSaldo, 0, _). 

sortopgames(Win, Catno, PeerList, Punten, Setsaldo) :-
  member(Tgnst, PeerList),
  getbacktrack(Here),
  retract(partijC(Win, Rowx, Tgnst, [Punten, Setsaldo | _], Plaatsing, Aantalgespeeld)),
  cutbacktrack(Here),
  gameSaldoIs(Win, CatNo, Tgnst, PeerList, GameSaldo),
  assertz(partijC(Win, Rowx, Tgnst, [Punten, SetSaldo, Gamesaldo], Plaatsing, AantalGespeeld)),
  fail.
sortopGames(Win, Catno, _Tgl, Punten, SetSaldo) :-
  findall(GameSaldo, partijC(Win, _, _, [Punten, SetSaldo, Gamesaldo | _], _, _), GameSaldoList),
  sortint_c(GameSaldoList),
  removedubl(GameSaldoList, [], SaldoRL),
  member(SaldoR, SaldoRL),
  findall(Tgnst, partijC(Win, _Rowx, Tgnst, [Punten, SetSaldo, SaldoR | _], _, _),PeerList),
  not(PeerList = [_]),	% 25.1.2017 maar één member!
  setsgamesalle(Win, CatNo, PeerList, Punten, SetSaldo, Saldor),
  fail.
sortopGames(_Win, _Catno, _PeerList, _Punten, _SetSaldo).
  
setsGamesAlle(Win, CatNo, PeerList, _Punten, _SetSaldo, _Gamesaldo) :-
  findall(TgnstA, partijC(Win, _Rowx, TgnstA, _, _, _),AlleList),
  member(Tgnst, PeerList),
  getbacktrack(Here),
  retract(partijC(Win, Rowx, Tgnst, Puntensaldo, Plaatsing, Aantalgespeeld)),
  cutbacktrack(Here),
  setSaldoIs(Win, CatNo, Tgnst, AlleList, SetSaldo),
  gamesaldoIs(Win, CatNo, Tgnst, AlleList, Gamesaldo),
  append(Puntensaldo, [SetSaldo, GameSaldo], PuntenSaldoN),
  assertz(partijC(Win, Rowx, Tgnst, PuntenSaldoN, Plaatsing, AantalGespeeld)),
  fail.
setsGamesAlle(_Win, _CatNo, _PeerList, _Punten, _SetSaldo, _Gamesaldo).
  
deRankDisp(_N, -1,  "") :- !.
deRankDisp(_N, 0,  "") :- !.
deRankDisp(N, _, Out) :-
  str_int(Out, N), !.
deRankDisp(_, _,  "").

gamesaldoIs(Win, CatNo, T1, Groep, GameSaldo) :-
  findall(Saldo, games_sets(Win, T1, Groep, CatNo, Saldo), SaldoL),
  addsaldo(SaldoL, 0, _, 0, GameSaldo).

addSaldo([t(Games,Sets)| Rest], InS, UitS, InG, UitG) :-
  addSaldo(Rest, InS, In1, InG, In2),
  UitS = Sets + In1,
  UitG = Games + In2, !.
addSaldo(_, InS, InS, InG, InG).

ordeVolg(R, K, R, K) :-		% zorg dat beneden de diagonaal gekozen wordt
  R > K, !.
ordeVolg(R, K, K, R).

andere(T, T, Ta, Ta) :- !.
andere(_, Ta, _, Ta). 

init_cursor(_Win, Pos, Cat, Kol1, Row1, 0, Tgnst) :-	% row, kol
  wd(Pos, Cat, T2, T1, _Uitsl, _, _),
  partijC(_Win, Kol1, Tgnst, _, _, _),
  andere(Tgnst, T1, T2, Ta),
  partijC(_Win, Row1, Ta, _, _, _), !. 
init_cursor(_Win, Pos, Cat, Row1, Kol1, 0, _Tgnst) :-	% row, kol
  wd(Pos, Cat, T2, T1, _Uitsl, _, _),
  partijC(_Win, Kol, T1, _, _, _),
  partijC(_Win, Row, T2, _, _, _), 
  ordeVolg(Row, Kol, Row1, Kol1), !.
init_cursor(_Win,  _, _, 2, 1, 0, _).
init_cursor(_Win, Pos, Cat, Row, 1, 1, _) :-	% vooraan Kol = 1
  wd(Pos, Cat, T2, _T1, _Uitsl, _, _),
  partijC(_Win, Row, T2, _, _, _),
  Row > 1, !. 
init_cursor(_Win, _Pos, _Cat, 1, 2, 1, _).	% vooraan Kol =  2
  
krijgtPunten(_Win, _T, _, "winnaar", 1, 1) :-
  pouleTelling(1), !.
krijgtPunten(_Win, _T, _, "pouleu", 1, 0) :-	% 2.2.2003%%%%%%%%%%%%%%
  pouleTelling(1), !.
krijgtPunten(Win, T, _, "pouleus", _, 0) :-
  pouletelling(0),
  besmetTeam(Win, T), !.	% besmet --> doorgestreept
krijgtPunten(Win, _, T2, "pouleus", _, 0) :-
  pouletelling(0),
  besmetTeam(Win, T2), !.	% andere besmet
krijgtPunten(_Win, _T, _, "winnaar", 1, 1) :-
  pouletelling(0), !.
krijgtPunten(_Win, _T, _, "pouleu", _, 0) :- 
  pouletelling(0), !.
krijgtPunten(_Win, _T, _, "winnaar", 1, 1) :- !.
krijgtPunten(_Win, _T, _, "pouleu", 0, 1) .

puntenorig(_T1, gelijk_spel, _Uitslag, 2) :-
  pouleTelling(1), !.
puntenorig(_T1, gelijk_spel, _Uitslag, 1) :-
  pouleTelling(0), !. 
puntenorig(T1, T1, Uitslag, Punten) :-
  uitsl_games(Uitslag, GamesT, GamesV, _/*SetsT*/, _/*SetsV*/),
  Games = GamesT - GamesV, !,
  punten(1, Games, Punten).
puntenorig(T1, T2, Uitsl, Punten) :-
  not(T1 = T2),
  uitsl_games(Uitsl, GamesT, GamesV, _/*SetsT*/, _/*SetsV*/),
  Games = GamesV - GamesT,
  punten(0, Games, Punten), !.
puntenorig(_T1, _T2, _Uitslag, 0).

echtKoppel(s(_)) :- !.
echtKoppel(p(_,_)).

rep_p(_, CatNo, T1, T2, _, _, _, _, _, _, _,_,_,_,_) :-
  bound(T1),
  bound(T2),
  not(T1 = T2),
  not(wedst(_, CatNo,T1,T2,_,_,_)),
  echtKoppel(T1),
  echtkoppel(T2),
  findall(WdNox,wd(WdNox,CatNo,_,_,_,_,_),WdL),
  sortint_c(WdL),
  reverse(WdL,[],WdLR),
  WdLR = [WdH|_],
  WdNw = WdH + 1,
  not(wd(WdNw,CatNo,_,_,_,_,_)), % voor de zekerheid
  logf('Z',wd(WdNw,CatNo,T1,T2,"",0,""),nee),
  wc(CatNo, _, _, Onderdeel, _, _NaamKortS, _, _, _, _, _, _, _, _, _),
  format(Msg, "missende wedstrijd van poule\n%s\ngerepareerd!",Onderdeel),
  writetojournaal(Msg),
  fail.
rep_p(_, CatNo, T1, T2, P, Uitsl1, _Tijdstip,WdNo,_,_, Anker, "pouleu",Uitsl1,Noti,1) :-	% rapporteer uitsl & punten
  %pouleTelling(1),					% fantasie telling..
  wedst(WdNo,CatNo,T1,T2,Uitsl,b_False,Noti),
  w3(WdNo,CatNo,gelijk_spel,P,_,_,_),
  format(Anker,"<A name=T%ux%u></A>",WdNo, CatNo),
  concat("(=) ", Uitsl, Uitsl1).
rep_p(_, CatNo, T1, T2, P, Uitsl1, _Tijdstip,WdNo,_,_, Anker, "pouleu",Uitsl1,Noti,1) :-	% rapporteer uitsl & punten
  %pouleTelling(1),					% fantasie telling..
  wedst(WdNo,CatNo,T1,T2,Uitsl,b_True,Noti),
  w3(WdNo,CatNo,gelijk_spel,P,_,_,_),
  format(Anker,"<A name=T%ux%u></A>",WdNo, CatNo),
  draaiom(Uitsl, "", Uitslx),
  concat("(=) ", Uitslx, Uitsl1).
rep_p(Win, CatNo, T1, T2, Punten, Uitsl, _,WdNo,_,_,Anker,Klasse,Uitsl,Noti,1) :-
  wd(WdNo,CatNo,T1,T2,Uitsl,_,Noti), 
  w3(WdNo,CatNo,T1,P,_,_,_),
  krijgtPunten(Win,T1, T2, Klasse, 1, Gewonnen),
  format(Anker,"<A name=T%ux%u></A>",WdNo, CatNo),
  Punten = Gewonnen * P.
rep_p(Win, CatNo, T1, T2, Punten, Uitsl, _,WdNo,_,_,Anker,Klasse,Uitsl, Noti,1) :-
  wd(WdNo,CatNo,T2,T1,Uitsl,_,Noti), 
  w3(WdNo,CatNo,T1,_,P,_,_),
  krijgtPunten(Win,T1, T2, Klasse, 1, Gewonnen),
  format(Anker,"<A name=T%ux%u></A>",WdNo, CatNo),
  Punten = Gewonnen * P.
rep_p(Win, CatNo, T1, T2, Punten, Uitsl1, _, WdNo,_,_,Anker,Klasse,Uitsl1,Noti,1) :-
  wd(WdNo,CatNo,T1,T2,Uitsl,_,Noti), 
  w3(WdNo,CatNo,T2,P,_,_,_),
  krijgtPunten(Win,T1,T2, Klasse,0, Gewonnen),	% 1.9.2002
  format(Anker,"<A name=T%ux%u></A>",WdNo, CatNo),
  draaiom(Uitsl, "", Uitsl1),
  trim_c(Uitsl1),
  Punten = Gewonnen * P.
rep_p(Win, CatNo, T1, T2, Punten, Uitsl1, _, WdNo,_,_,Anker,Klasse,Uitsl1,Noti,1) :-
  wd(WdNo,CatNo,T2,T1,Uitsl,_,Noti), 
  w3(WdNo,CatNo,T2,_,P,_,_),
  krijgtPunten(Win,T1,T2, Klasse,0, Gewonnen),	% 1.9.2002
  format(Anker,"<A name=T%ux%u></A>",WdNo, CatNo),
  draaiom(Uitsl, "", Uitsl1),
  trim_c(Uitsl1),
  Punten = Gewonnen * P.
rep_p(_, CatNo, T1, T2, 0, Planning, Tijdstip, WdNo, OffsX,Oproepen,Anker,"pouleuw",PlanningHTML,Noti,0) :-
  wedst(WdNo,CatNo,T1,T2,_,LR,Noti), 
  rep_plan(WdNo,CatNo,T1,Planning,PlanningHTML, Tijdstip,LR,OffsX,Oproepen,Anker), !,
  trim_c(Planning).
rep_p(_, CatNo, T1, T2, 0, Uitsl1, _, WdNo, _, _, Anker,"pouleu",Uitsl2,Noti,0) :-
  wedst(WdNo, CatNo,T1,T2,Uitsl,LR,Noti),
  LR = b_False,
  trim_c(Uitsl),
  Uitsl <> "",
  format(Anker,"<A name=T%ux%u></A>",WdNo, CatNo),
  format(Uitsl1, "(tussenstand) %s", Uitsl),
  format(Uitsl2, "(tussenstand) <span style=\"white-space:nowrap\">%s</span>", Uitsl).
rep_p(_, CatNo, T1, T2, 0, Uitsl2, _, WdNo, _, _, Anker,"pouleu",Uitsl3,Noti,0) :-
  wedst(WdNo, CatNo,T1,T2,Uitsl,LR,Noti),
  LR = b_True,
  trim_c(Uitsl),
  Uitsl <> "",
  format(Anker,"<A name=T%ux%u></A>",WdNo, CatNo),
  draaiom(Uitsl, "", Uitsl1),
  format(Uitsl2, "(tussenstand) %s", Uitsl1),
  format(Uitsl3, "(tussenstand) <span style=\"white-space:nowrap\">%s</span>", Uitsl1).
rep_p(_, CatNo, T1, T2, 0, "", _, WdNo, _, _, Anker,"pouleu","",Noti,0) :-
  wedst(WdNo, CatNo,T1,T2,Uitsl,LR,Noti),
  LR = b_True,
  trim_c(Uitsl),
  Uitsl = "",
  format(Anker,"<A name=T%ux%u></A>",WdNo, CatNo).
rep_p(_, CatNo, T1, T2, 0, "", _, WdNo, _, _, Anker,"pouleu","",Noti,0) :-
  wedst(WdNo, CatNo,T1,T2,Uitsl,LR,Noti),
  LR = b_False,
  trim_c(Uitsl),
  Uitsl = "",
  format(Anker,"<A name=T%ux%u></A>",WdNo, CatNo).
rep_p(_, _,_,onbekend,0,"",_,0, _, _, "","pouleu","","",0).
rep_p(_, _,_,bye,0,"",_,0,_,_, "","pouleu","","",0).
/*rep_p(_,T1,Test_,0,"",_,0) :-
  write("\nrep_p failt! ", T1, "  test ",Test),
  fail.*/

rep_gewaarsch(_Num, _Cat, _Tgnst, b_False, ja, _, "",_,_) :- !.
rep_gewaarsch(_Num, _Cat, _Tgnst, b_True, _, ja, "",_,_) :- !.
rep_gewaarsch(Num, Cat, Tgnst, _, _, _, Tkst,_,ja) :-
  %oproepen(ja), 			% oproepen en vorige planning
  ws(Num, Cat, Tgnst, TijdWas),
  tijd_kwart(D1,U1,M1,TijdWas),
  dag(D1,Dnaam,_), 
  helestr_int(0,MS1,M1),
  format(Tkst, "(%s %.%s)",Dnaam, U1,MS1), !.
rep_gewaarsch(_Num, _Cat, _Tgnst, b_False, nee, _, "?",0,ja) :- !. 
rep_gewaarsch(_Num, _Cat, _Tgnst, b_True, _, nee, "?",0,ja) :- !. 
rep_gewaarsch(_,_,_,_,_,_,"",_,_).

rep_baan(Baan, Tekst) :-
  baan_adm(ja),
  Baan <> "", !,
  format(Tekst,"[%s]",Baan).
rep_baan(_Baan, "").

addTussenstand(WdNo, CatNo, b_True, Stand, Noti) :-
  wedst(WdNo, CatNo,_T1,_T2,Uitsl,b_True,Noti),
  trim_c(Uitsl),
  not(Uitsl = ""),
  format(Stand, "(%s)", Uitsl), !.
addTussenstand(WdNo, CatNo, b_False, Stand, Noti) :-
  wedst(WdNo, CatNo,_T1,_T2,Uitsl,b_False, Noti),
  trim_c(Uitsl),
  not(Uitsl = ""),
  draaiom(Uitsl, "", Uitsl2),
  format(Stand, "(%s)", Uitsl2), !.
addTussenstand(_, _, _, "", "").

rep_plan(PrWed,Cat,T1,TStr,TStrHtml,Tijdstip, LR,OffsX,Oproepen,Anker) :-	% planning + oproepen + baanadm
  w2(PrWed,Cat,Tijd,G1,G2,Fix,_, BNo),
  addTussenstand(PrWed, Cat, LR, Stand,_Noti),
  rep_gewaarsch(PrWed, Cat, T1, LR, G1, G2, GTekst,OffsX,Oproepen),
  rep_baan(BNo, BaanTekst),
  Tijd < Tijdstip, 
  tijd_kwart(Dag,_,_,Tijd),
  tijd_str(Tijd,Str1,_),
  dag(Dag,DStr,_), 
  dt_dateoffset_to_str(Dag, "%Dn&nbsp;%Dd/%Md", DStrH), !,
  format(TStr, "%c %s %s %s\n%s %s", Fix, DStr, Str1, BaanTekst, GTekst, Stand),
  format(TStrHtml, "%c %s %s %s\n%s %s", Fix, DStrH, Str1, BaanTekst, GTekst, Stand),
  format(Anker,"<A name=T%ux%u></A>",PrWed, Cat).
rep_plan(PrWed,Cat,T1,TStr,TStr,_Tijdstip, LR,_,ja,"") :-	% oproepen en geen planning meer
  %oproepen(ja),
  ws(PrWed, Cat, T1, Tijd),
  addTussenstand(PrWed, Cat, LR, Stand, _Noti),
  tijd_kwart(D1,U1,M1,Tijd),
  dag(D1,Dnaam,_), !,
  helestr_int(0,MS1,M1),
  format(TStr, "(was %s %.%s)\n%s", Dnaam, U1, MS1, Stand).
%rep_plan(_,_,_,"",_, _, _, _). 


punten(Gelijkspel, Verschil, Punten) :-
  pouletelling(PT),
  punten1(PT, Gelijkspel, Verschil, Punten).

punten1(1, _, P, 4) :-
  P >= 4, !.
punten1(1, _, P, 3) :-
  P >= 2, !.
punten1(1, _, P, 2) :-
  P >= -1, !.
punten1(1, _, P, 1) :-
  P >= -3, !.
punten1(4, _, P, 8) :-
  P >= 4, !.
punten1(4, _, P, 7) :-
  P >= 3, !.
punten1(4, _, P, 6) :-
  P >= 2, !.
punten1(4, _, P, 5) :-
  P >= 1, !.
punten1(4, _, P, 3) :-
  P >= -1, !.
punten1(4, _, P, 2) :-
  P >= -2, !.
punten1(4, _, P, 1) :-
  P >= -3, !.
punten1(0, 1, _, 2) :- !.
punten1(_, _, _, 0).

draaiom(In, _, Uit) :-
  tokenizeU(In, List),
  draaiom1(List, "", Uit), !.
  
draaiom1([games(G1,G2)| T], In, Uit) :-   
  format(In1, "%s %u-%u", In, G2, G1), !,
  draaiom1(T, In1, Uit).
draaiom1([tie(TieS)| T], In, Uit) :-
  format(In1, "%s%s", In, TieS), !,
  draaiom1(T, In1, Uit).
draaiom1([tekst(Tekst)| T], In, Uit) :-
  format(In1, "%s %s", In, Tekst), !,	% 27.2.2011 het verschil is de spatie
  draaiom1(T, In1, Uit).
draaiom1(_, Uit, Uit).

skiptoken(In, Out) :-
  fronttoken(In, Token, Out1),
  not(Token = "("),
  not(str_int(Token,_)),
  skiptoken(Out1, Out), !.
skiptoken(In, In).

tokenizeU(In, [tie(TieS) | Tail]) :-
  fronttoken(In, "(", In1),
  fronttoken(In1, Getal, R2),
  str_int(Getal, _),
  fronttoken(R2, _Kaahje, R3),
  format(TieS, "(%s)", Getal), !,
  tokenizeU(R3, Tail).
tokenizeU(In, [games(G1,G2) | Tail]) :-
  fronttoken(In, Games, In1),
  str_int(Games, G1),
  fronttoken(In1, _, R1),
  fronttoken(R1, GS, R2),
  str_int(GS, G2), !,
  tokenizeU(R2, Tail).
tokenizeU(In, [tekst(Tekst) | Tail]) :-
  fronttoken(In, Tekst, R), !,
  tokenizeU(R, Tail).
tokenizeU(_In, []).

uitsl_games(Uitslag, GamesV, GamesT, SetsV, SetsT) :-
  tokenizeU(Uitslag, UitslagList),
  uitsl_games1(UitslagList, 0, GamesV, 0, GamesT, 0, SetsV, 0, SetsT).

uitsl_games1([games(GamesV,GamesT)|Tail], GamesVI, GamesVO, GamesTI, GamesTO, SetsVI, SetsVO, SetsTI, SetsTO) :-
  set_uitsl(GamesV, GamesT, SetsVI, SetsVI1, SetsTI, SetsTI1),
  GamesVI1 = GamesV + GamesVI,
  GamesTI1 = GamesT + GamesTI, !,
  uitsl_games1(Tail, GamesVI1, GamesVO, GamesTI1, GamesTO, SetsVI1, SetsVO, SetsTI1, SetsTO).
uitsl_games1([_|Tail], GamesVI, GamesVO, GamesTI, GamesTO, SetsVI, SetsVO, SetsTI, SetsTO) :- !,	
  uitsl_games1(Tail, GamesVI, GamesVO, GamesTI, GamesTO, SetsVI, SetsVO, SetsTI, SetsTO).	
uitsl_games1(_, GamesV, GamesV, GamesT, GamesT, SetsV, SetsV, SetsT, SetsT).	

set_uitsl(GamesV, GamesT, SetsVI, SetsVO, SetsT, SetsT) :-
  GamesV > GamesT, !,
  SetsVO = SetsVI + 1.
set_uitsl(GamesV, GamesT, SetsV, SetsV, SetsTI, SetsTO) :-
  GamesV < GamesT, !,
  SetsTO = SetsTI + 1.
set_uitsl(_GamesV, _GamesT, SetsVI, SetsVO, SetsTI, SetsTO) :-	% 26.1.2017 allebei een set meer
  SetsTO = SetsTI + 1,
  SetsVO = SetsVI + 1.


games_sets(Win, T1, Groep, CatNo, t(Games, Sets)) :-
  member(T2, Groep),
  partijC(Win, _Gr, T2,  _, _, _),	% ga alle meedoeners in de groep af,
  not(T1 = T2),				% die tegenpartij zijn
  rep_g(CatNo, T1, T2, GamesVoor, GamesTegen, SetsVoor, SetsTegen),
  Games = GamesVoor - GamesTegen,
  Sets = SetsVoor - SetsTegen.
  					% en kijk hoeveel games, sets voor/tegen
rep_g(CatNo, T1, T2, GamesV, GamesT, SetsV, SetsT) :-	% rapporteer games/sets t.o.v. T1
  wedst(WdNo,CatNo,T1,T2,Uitsl,_,_),
  w3(WdNo,CatNo,T1,_,_,_,_),
  uitsl_games(Uitsl, GamesV, GamesT, SetsV, SetsT), !.
rep_g(CatNo, T1, T2, GamesV, GamesT, SetsV, SetsT) :-	% rapporteer games/sets t.o.v. T1
  wedst(WdNo,CatNo,T1,T2,Uitsl,_,_),			% ook als gelijk spel
  w3(WdNo,CatNo,gelijk_spel,_,_,_,_),
  uitsl_games(Uitsl, GamesV, GamesT, SetsV, SetsT), !.
rep_g(CatNo, T1, T2, GamesV, GamesT, SetsV, SetsT) :-
  wedst(WdNo,CatNo,T2,T1,Uitsl,_,_),
  w3(WdNo,CatNo,T2,_,_,_,_),
  uitsl_games(Uitsl, GamesT, GamesV, SetsT, SetsV),!. % andersom!

add([], Out, Out) :- !.
add([H|T], In, Out) :-
  In1 = In + H,
  add(T, In1, Out).

wedst(WedsNo, CatNo, T1, T2, Uitsl, b_True, Noti) :-  
  wd(WedsNo, CatNo, T2, T1, Uitsl, _,Noti).
wedst(WedsNo, CatNo, T1, T2, Uitsl, b_False, Noti) :-  
  wd(WedsNo, CatNo, T1, T2, Uitsl, _, Noti).

besmet(T1, Cat, _, -1) :-
  pouletelling(0),
  wedst(No, Cat, T1, T2, _, _, _),
  w3(No,Cat,T2,_,_,walkover,_),	!.% de ander is de winnaar
besmet(T1, Cat, _, -1) :-
  pouletelling(0),
  wedst(No, Cat, T1, T2, _, _, _),
  w3(No,Cat,T2,_,_,gestaakt,_),	!.% de ander is de winnaar
  %not(UitslagStatus=geen), !.		% en niet regelmatig !!
besmet(_, _, P, P).	% punten geldig
  
% ################################# ranking uitzoeken
onderdeelbreedte(CatNo, Size, Breedte) :-
  wc(CatNo, _, _, _, e, _, _, _, _, _, _, _, _, _, _),
  %wc(CatNo,_,e,_,_,_,_),
  Breedte = 13 * Size, !.
onderdeelbreedte(_CatNo, Size, Breedte) :-
  Breedte = 13 * Size, !.

plaatsing(CatNo, Tgnst, Str, color_Red) :-
  opg(_, CatNo, Tgnst, Pl, _, 0, _, _, _, _, _),
  Pl > 0, 
  format(Str, "(%u)", Pl), !.
plaatsing(CatNo, Tgnst, "", color_Blue) :-
  opg(_, CatNo, Tgnst, _Pl, _, 1, _, _, _,_,_), !. 
plaatsing(_CatNo, _Tgnst, "", color_Black).

repRow1(Cat, s(No),Naam1, Plaats, 0, Kleur)  :-		% report single
  sp2(No,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), 
  naamscan(nee,b_False, Naam, Naam1,_,_Landcode), !,
  plaatsing(Cat, s(No), Plaats, Kleur).
repRow1(Cat, pw(No),Naam1, Plaats, 0, Kleur)  :-		% report pw
  sp2(No,_,Naam,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), 
  naamscan(nee,b_False, Naam, Naam1,_,_Landcode), !,
  plaatsing(Cat, pw(No), Plaats, Kleur).
repRow1(_Cat, pw(_),"zoekt partner", "", 1, color_Black) :- !.	% report pw
repRow1(Cat, p(No,No1),Naamt1, Plaats, 0, Kleur) :-		% report double
  sp2(No,_,Naamt,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !,
  naamscan(nee,b_False, Naamt, Naamt1,_,_Landcode),
  plaatsing(Cat, p(No,No1), Plaats, Kleur).
repRow1(_Cat,p(_,No1),Naam2, "", 1, color_Black) :-		% report double
  sp2(No1,_,Naam1,_,_ ,_,_,_,_,_,_, _,_,_,_,_, _,_,_,_,_,_,_,_,_,_,_), !,
  naamscan(nee,b_False, Naam1, Naam2,_,_Landcode).
repRow1(CatT,openplaats(No1),Tekst, "", 0, color_Black) :-		% open plaats + koppeling
  kopl(_CatV, rang(Nr), CatT, plaats(No1)), !,
  format(Tekst, "Nr %u van", Nr).
repRow1(CatT,openplaats(No1),CatS, "", 1, color_Black) :-		% open plaats + koppeling
  kopl(CatV, rang(_Nr), CatT, plaats(No1)), 
  wc(CatV, _, _, CatS, _, _, _, _, _, _, _, _, _, _, _), !.
repRow1(_Cat,openplaats(No1),Tekst, "", 0, color_Black) :-		% open plaats
  format(Tekst, "Open plaats %u", No1), !.
repRow1(_,_,"","",_, color_Black). 

adjust(Rn, R, _Ln, _L, _Kolom0, Kolombreedte, Xin, Xuit) :-
  Rn > R, !,
  Xuit = Xin - ((Rn - R) div Kolombreedte + 1) * Kolombreedte.	% ga rechtsaf
adjust(_Rn, _R, Ln, L, Kolom0, Kolombreedte, Xin, Xuit) :-
  Xin < 0,
  Ln < L + Kolom0, !,
  X = Xin + ((L  + Kolom0 - Ln) div Kolombreedte + 1) * Kolombreedte,% ga linksaf 
  kleinste(X, 0, Xuit), !.
adjust(_, _, _, _, _, _, X, X).

changed(K, K, R, R) :- !, fail.
changed(_, _, _, _).

boundaryXY(Win, NewRct, _OldRct, Kol, Row) :-
  displayP(Win, CatNo,OriginX,OriginY,PrevRow,PrevKol),
  changed(Kol, PrevKol, Row, PrevRow),
  RCT = win_GetClientRect(Win),
  RCT = rct(L,T,R,B),
  NewRct = rct(Ln,Tn,Rn,Bn),
  schaal(Win, Rijhoogte, Kolombreedte, Kolom0, Row0,_Marge,_Schaal,_Hor,_Vert,_),
  adjust(Rn, R, Ln, L, Kolom0, Kolombreedte, OriginX, Xuit),
  adjust(Bn, B, Tn, T, Row0, Rijhoogte, OriginY, Yuit),
  changed(OriginX, Xuit, OriginY, Yuit),
  retract(displayP(Win, _,_,_,_,_)),!,
  assert(displayP(Win,CatNo,Xuit,Yuit,Row,Kol)),
  win_Invalidate(Win).
boundaryXY(Win, NewRct, OldRct, Kol, Row) :-
  retract(displayP(Win, CatNo,OriginX,OriginY,_,_)), !,
  assert(displayP(Win,CatNo,OriginX,OriginY,Row,Kol)),
  win_Invalidate(Win, OldRct),		% zelfde
  win_Invalidate(Win, NewRct).

incr(CursorK, CursorR, Win, Kol, CursorR) :-
  lastKol(Win, Aantal),
  CursorK < Aantal,
  Kol = CursorK + 1,
  Kol <> CursorR, !.		% moet over blokje heen springen
incr(CursorK, CursorR, Win, Kol, CursorR) :-
  lastKol(Win, Aantal),
  CursorK < Aantal - 1,
  Kol = CursorK + 2, !.
incr(CursorK, _CursorR, Win, Aantal, Row) :-
  lastKol(Win, Aantal),
  CursorK = Aantal - 1, !,
  Row = Aantal - 1.

decr(CursorK, CursorR, Kol, CursorR) :-
  CursorK  > 1,
  Kol = CursorK - 1,
  Kol <> CursorR, !.		% moet over blokje heen springen
decr(CursorK, CursorR, Kol, CursorR) :-
  CursorK > 2,
  Kol = CursorK - 2, !.
%decr(2, _CursorR, 1, 2).	% in het hoekje

waarschuwbaar(Win, Num, Cat, PlanW) :-
  oproepen(ja),
  displayP(Win, _CatNo,_OriginX,_OriginY,Row,_PrevKol),
  partijC(Win,Row, T, _, _, _),
  w2(Num,Cat,_,Gew1,Gew2,_,_,_),
  wd(Num,Cat,T1,T2,_,_,_),
  waarschuwbaar1(T, T1, T2, Gew1, Gew2, PlanW), !.
waarschuwbaar(Win, Num, Cat, PlanW) :-	% 4.11.2010
  %oproepen(ja),
  displayP(Win, Cat,_OriginX,_OriginY,Row,_PrevKol),
  partijC(Win,Row, T, _, _, _),
  ws(Num, Cat, T, _TijdWas),
  wd(Num,Cat,T1,T2,_,_,_),
  waarschuwbaar1(T, T1, T2, nee, nee, PlanW), !.
waarschuwbaar(_Win, _Num, _Cat, b_False).

waarschuwbaar1(T1, T1, _, nee, _, b_True) :-!.
waarschuwbaar1(T2, _, T2, _, nee, b_True). 


% ################################## print predicate

constants

  kolom0Str = "Wj345678901234567"
  kolomStr  = "012345678901234"

predicates 
  nondeterm deelnemer(integer, wedscat)
  check_inhoud(integer PaginSel, wedscat CatNo, integer VanafDag)
  verdelen(string Onderdeel, integer AantalHor, integer AantalVert, integer Kolombreedte, integer Kolom0, integer Elementhoogte,
  	integer Row02, integer PapHor, integer PapVert, integerlist In, integerlist Vert)
  poulePart(window, integer Aantal, integer MaxKols, integer CurrentKol, wedscat CatNo,WINDOW Win,integer PlanTM,PNT Origin,
  	PNT Offset,integer FontSizeOrig, RCT Papier, FONT Header)
  newpage(WINDOW, RCT, integer, integer, integer, integer, integer)

clauses

newpage(Win, rct(PX0,PY0,PX,PY), OffsY, VertNP, Out, FontOrig, Row0) :-
  OffsY + VertNP + Row0 > PY, !,	%&&&
  Font = win_GetFont(Win),
  draw_footer(Win, rct(PX0,PY0,PX,PY), FontOrig),
  open_blzEx(Win),
  win_SetFont(Win, Font),
  Out = Row0 + 16.
newpage(_, _, OffsY, _, OffsY, _, _). 

poulePart(Win,Aantal, MaxKols, CurrentKol, CatNo,Win,PlanTM,pnt(OriginX,OriginY),pnt(OffsX,OffsY),FontOrig,Papier,FontHeader) :-
  schaal(Win, _, _, _, Row0NP, _, _, _, VertNP,_),
  Aantal - CurrentKol <= MaxKols,
  newpage(Win, Papier, OffsY, VertNP, OffsY1,FontOrig, Row0NP),
  retract(schaal(Win, ElementHoogte, Kolombreedte, Kolom0, Row0,Marge,Schaal,_,Vert,AantalXY)),
  Hor  = (Aantal - CurrentKol) * Kolombreedte + Kolom0,
  assert(schaal(Win, ElementHoogte, Kolombreedte, Kolom0, Row0,Marge,Schaal,Hor,Vert,AantalXY)),
  MaxKols1 = MaxKols + CurrentKol,
  toon_poule(CatNo,Win,PlanTm, pnt(OriginX,OriginY),pnt(OffsX,OffsY1),FontHeader, MaxKols1), !.
poulePart(Win,Aantal, MaxKols, CurrentKol, CatNo,Win,PlanTM,pnt(OriginX,OriginY),pnt(OffsX, OffsY),FontOrig,Papier,Font_Header) :-
  retract(schaal(Win, ElementHoogte, Kolombreedte, Kolom0, Row0,Marge,Schaal,_,Vert,AantalXY)),
  %Aantal <= MaxKols,
  newpage(Win, Papier, OffsY, Vert, OffsY1, FontOrig, Row0),
  Hor  = MaxKols * Kolombreedte + Kolom0,
  assert(schaal(Win, ElementHoogte, Kolombreedte, Kolom0, Row0,Marge,Schaal,Hor,Vert,AantalXY)),
  toon_poule(CatNo,Win,PlanTm, pnt(OriginX,OriginY),pnt(OffsX,OffsY1),Font_header, MaxKols), !,
  OffsY2 = OffsY1 + Vert + Row0 * 2,
  retract(displayP(Win, WC,_,OrigY,CursorR, CursorK)), !,
  OriginX1 = OriginX - (MaxKols * Kolombreedte),
  assert(displayP(Win, WC, OriginX1, OrigY, CursorR, CursorK)), %##
  %OffsX1 = OffsX + MaxKols * Kolombreedte,
  CurrentKol1 = CurrentKol + MaxKols,
  poulePart(Win,Aantal, MaxKols, CurrentKol1, CatNo,Win,PlanTM,pnt(OriginX1,OriginY),pnt(OffsX,OffsY2),FontOrig,Papier,Font_Header).

poule_draw(Win, CatNo, PlnTm, rct(A,B,PapierX,PapierY), Opschuiven, Font, FontSizeOrig) :-
  win_SetFont(Win, Font),
  PlnTM1 = PlnTM + 1,
  bitleft(PlnTM1,7,PlanTM),
  _FontName = font_GetAttrs(Font, _StyleFlags,FontSize),
  win_GetTextExtent(Win,kolomStr,-1,Kolombreedte,_),
  win_GetTextExtent(Win,kolom0Str,-1, Kolom0,HeightK0),
  ElementHoogte = HeightK0 * 3,
  wc(CatNo, _, _, Onderdeel, _, _, _, _, _, _, _, _, _, _, _),
  %wc(CatNo,Onderdeel,_,_,_,_,_),
  FS1 = FontSize + 4,
  Font_header = font_SetAttrs(Font,[fs_Bold],FS1),
  win_SetFont(Win, Font_header),
  win_GetTextExtent(Win,Onderdeel,-1,_Width,Height),
  win_SetFont(Win, Font),
  Row0 = Height + 6,
  %write("\n\npouledraw"),
  poulestand_init(Win, CatNo, Aantal0),
  Aantal = Aantal0 + 1,
  assert(displayP(Win, CatNo,0,0,1,1)), 
  StartY  = Row0 + Opschuiven + 13,
  Offset  = pnt(A,StartY),	% 31.8.2000
  MaxKols = (PapierX - A - Kolom0) div Kolombreedte,	% 25.9.1999
  Kolom01 = PapierX - A - MaxKols * Kolombreedte,
  Vert = Aantal0 * Elementhoogte + Row0,		% 28.9.1999
  assert(schaal(Win, ElementHoogte, Kolombreedte, Kolom01, Row0,5,0,0,Vert,Aantal)),
  assert(afdrukmode(Win)),
  poulePart(Win,Aantal, MaxKols,0, CatNo,Win,PlanTM,pnt(0,0),Offset,FontSizeOrig,rct(A,B,PapierX,PapierY),Font_Header),
  retractall(displayP(Win, _, _, _, _, _)),
  retractall(schaal(Win, _, _, _, _, _, _, _, _, _)), 
  %retractall(rankNieuw(Win, _, _, _)) ,
  retractall(partijC(Win, _, _, _, _,_)),
  retractall(lastKol(Win, _)), 
  fail.
poule_draw(Win, _Onderdeel, _PapierAfm, _, _, _, _) :-
  retractall(afdrukmode(Win)).	%print_EndPage(Win).

pouleAfmetingEx(Win, CatNo, PaginaSel, VanafDag, _TmDag,PapierAfm, Blz/*Hor*/, Vertlist, b_True) :-
  check_inhoud(PaginaSel,CatNo,VanafDag),
  Font = win_GetFont(Win),
  win_GetTextExtent(Win,kolomStr,-1,Kolombreedte,_),
  win_GetTextExtent(Win,kolom0Str,-1, Kolom0,HeightK0),
  ElementHoogte = HeightK0 * 3,
  _FontName = font_GetAttrs(Font, _StyleFlags,FontSize),
  FS1 = FontSize + 4,
  Font_header = font_Create(ff_Helvetica,[fs_Bold],FS1),
  win_SetFont(Win, Font_header),
  wc(CatNo, _, _, Onderdeel, _, _, _, _, _, _, _, _, _, _, _),
  %wc(CatNo,Onderdeel,_,_,_,_,_), 
  win_GetTextExtent(Win,Onderdeel,-1,_Width,Height),
  win_SetFont(Win, Font),
  Row0 = Height + 6,
  findall(Num, deelnemer(Catno,Num), Deelnemers),
  count(Deelnemers, 0, Aantal),
  Row02 = Row0 * 2,
  Hor  = (Aantal + 1) * Kolombreedte  + Kolom0,
  Vert = Aantal * Elementhoogte + Row02 + Row02,
  PapierAfm = rct(X0,Y0,PapHor,PapVert),
  PapHor1  = PapHor  - X0,
  PapVert1 = PapVert - Y0,
  verdelen(Onderdeel, Aantal, Aantal, Kolombreedte, Kolom0,Elementhoogte,Row02,PapHor1,PapVert1,[],VertList),
  %Hor <= PapHor, 
  %Vert < PapVert,
  Blz = round((Vert / PapVert) * (Hor / PapHor) * 10),	% ####################################### 
  !.	% passend
%pouleAfmetingEx(_Win, CatNo, PaginaSel, VanafDag, _TmDag,_PapierAfm, 11, 0, 0) :-
%  check_inhoud(PaginaSel,CatNo,VanafDag), !.	% 2 blz als niet past

verdelen(_, 0, _, _Kolombreedte, _Kolom0,_Elementhoogte,_Row02,_PapHor,_PapVert,In,In) :- !.
verdelen(_, AantalHor, AantalVert, Kolombreedte, Kolom0,Elementhoogte,Row02,PapHor,PapVert,In,[Vert|In]) :-
  Hor = (AantalHor + 1) * Kolombreedte + Kolom0,
  Hor <= PapHor, 
  Vert = AantalVert * Elementhoogte + Row02 + Row02,
  Vert < PapVert, !.
verdelen(Ond, AantalHor, AantalVert, Kolombreedte, Kolom0,Elementhoogte,Row02,PapHor,PapVert,In,Out) :-
  %beep(),
  Vert = AantalVert * Elementhoogte + Row02,
  Vert < PapVert,
  Horfit = (PapHor - Kolom0) div Kolombreedte,
  RestHor = (AantalHor + 1) - Horfit,
  grootste(0,RestHor,RestHor1), !,
  Vert = AantalVert * Elementhoogte + Row02,
  verdelen(Ond, RestHor1, AantalVert, Kolombreedte, Kolom0,Elementhoogte,Row02,PapHor,PapVert,[Vert|In],Out).
verdelen(Onderdeel, _, _, _, _, _, _, PapHor, PapVert,In,In) :-
  PapHor > PapVert, !,
  format(Msg, "%s loopt van het papier af\nprobeer portretmode en/of een kleiner lettertype.", Onderdeel),
  dlg_MessageBox( "Poule", Msg, mesbox_iconerror, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
  fail.
verdelen(_, _, _, _, _, _, _, _, _,In,In) :-
  %write("\npoule loopt van het papier af"),
  beep(),
  fail.

deelnemer(CatNo, OpgNum) :-
  opg(OpgNum, CatNo, T, _,_,_,_,_,_,_,_),
  getbacktrack(Here),
  wedst(_WedsNo, CatNo, T, _T2, _Uitsl,_,_),
  cutbacktrack(Here).

check_inhoud(3,CatNo,VanafDag) :-	% 3 = veranderd sedert..
  wd(_WNum,CatNo,_, _,_,Spoor,_),		% loop
  Spoor <> 0,
  bitright(Spoor,7,Dag),
  Dag >= VanafDag, 
  !.
check_inhoud(3,CatNo,VanafDag) :-	% 3 = veranderd sedert..
  wd(WNum,CatNo,_, _,_,_,_),		% loop
  w2(WNum,CatNo,_,_,_,_,Stamp,_),
  tijd_kwart(Dag,_,_,Stamp),
  Dag >= VanafDag,
  !.
check_inhoud(3,CatNo,VanafDag) :-	% 3 = veranderd sedert..
  wd(WNum,CatNo,_, _,_,_,_),		% loop
  w3(WNum,CatNo,_Tgnst,_,_,_,Stamp),
  tijd_kwart(Dag,_,_,Stamp),
  Dag >= VanafDag,
  !.
check_inhoud(1,_CatNo, _VanafDag).
check_inhoud(2,_CatNo, _VanafDag).



%BEGIN_WIN Poule window
/**************************************************************************
        Creation and event handling for window: Poule window
**************************************************************************/

CONSTANTS
%BEGIN Poule window, CreateParms, 16:42:39-24.7.2011, Code automatically updated!
  win_poule_window_WinType = w_TopLevel
  win_poule_window_Flags = [wsf_SizeBorder,wsf_TitleBar,wsf_Maximize,wsf_Close,wsf_ClipSiblings]
  win_poule_window_RCT = rct(100,80,686,414)
  win_poule_window_Menu = no_menu
  win_poule_window_Title = "Poule window"
  win_poule_window_Help = idh_pouleschema
%END Poule window, CreateParms
%id_wedstrijd_plan             = 609
%id_wedstrijd_wis_plan         = 610
%id_Wedstrijd_fixeer           = 611
%id_Wedstrijd_overzicht        = 613
id_Wedstrijd_uitslag_corrigeren = 614
id_partijen_een = 700
id_partijen_twee = 701

PREDICATES

  win_poule_window_eh : EHANDLER
  adapt(WINDOW, RCT ClRct, RCT TaskRct, integer HorzAfm, integer VertAfm, integer Letterhoogte) - (i,i,i,i,i,i)
nondeterm  cat_ingedeeld(wedscat)
%procedure  open_next(wedscat)
procedure  findNextCat(char NextPrev, catl, wedscat CurrCat, wedscat NextCat) 
poule_winit(WINDOW, WINDOW, integer FontSize, integer Aantal) - (i,i,i,i)
poule_winit(WINDOW, WINDOW, integer FontSize) - (i,i,i)
%probeerFocus(WINDOW)
koppelingen(BOOLEAN)
popupPoel(window)
nondeterm poulespeler(wedscat, integer)

CLAUSES

koppelingen(b_True) :-
  displayP(_Win, Cat,_X,_Y, _Row, _Kol),
  kopl(Cat, rang(_), _CatN, _No), !. 
  %Rang < 3, !.
koppelingen(b_False).

open_nextCat(NextPrev, CurrCat) :-
  findall(Cat, cat_ingedeeld(Cat), CatL0),
  CatL0 = [CatL1|_],
  append(CatL0, [CatL1], CatL),
  findNextCat(NextPrev, CatL, CurrCat, NextCat),
  pos(NextCat, _, WedNo, _),
  select(NextCat, WedNo), !.
open_nextCat(_,_). 

cat_ingedeeld(Cat) :-
  wc(Cat, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
  %wc(Cat,_Catname,_,_,_,_,_),
  pos(Cat,_,_WNo,_AfvPoel).

findNextCat('+',[Curr,Next|_], Curr, Next) :- !.
findNextCat('-',[Next, Curr|_], Curr, Next) :- !.
findNextCat(NP, [_|Rest], Curr, Next) :- !,
  findNextCat(NP, Rest, Curr, Next).
findNextCat(_, _, Curr, Curr).

adapt(Win, ClRct, TaskRct, Horz, Vert, Letterhoogte) :-
  TaskRct = rct(_,_,Tx1,Ty),
  Tx = Tx1 - 22,	% was 22
  ClRct = rct(_X0, _Y0, X, Y),
  RCT = win_GetOuterRect(Win),
  ClientRct = rect_GetClient(win_poule_window_Flags, b_False, RCT), 
  Xdelta1 = (Horz - X) div  + 2,
  Xdelta2 = (Tx - X - 30) div 2,
  kleinste(Xdelta1, Xdelta2, Xdelta),
  Ydelta1 = (Vert - Y ) div 2 + 2 + 15,
  Ydelta2 = ((Ty - Letterhoogte * 2 - 10) - Y) div 2,
  kleinste(Ydelta1, Ydelta2, Ydelta),
  NewClient = rect_Inflate(ClientRct, Xdelta, Ydelta),
  NewClient = rct(Xo, Yo, _, _),
  Xoff = 10 - Xo,
  Yoff = 40 - Yo,	% 3.8.2011
  NewClient1 = rect_Offset(NewClient, Xoff, Yoff), 
  win_Move(Win, NewClient1), !. 
adapt(_Win, rct(_, _, _X, _Y), rct(_,_,_Tx,_Ty), _Horz, _Vert, _).

poule_winit(NewWin, _Win, FontSize, Aantal) :-
	Font = font_Create(ff_Helvetica, [], FontSize),
	win_SetFont(_Win, Font),
	win_GetFontMetrics(_Win, _Leading, Ascent, _Descent),
	displayP(_Win, CatNo,_OrigX,_OriginY,_Row,_Col),
        onderdeelbreedte(CatNo, FontSize, Kolombreedte),
	Letterhoogte  = Ascent + 4,
	ElementHoogte = Letterhoogte * 3,
	Kolom0        = 15 * FontSize,
	Row0          = Letterhoogte + 10,  % was 10  
	retract(schaal(NewWin, _, _, _, _,_, _Schaal, _, _, _AantalXY)),
	Hor  = Aantal * Kolombreedte  + Kolom0 + 2,
	Vert = Aantal * ElementHoogte + Row0 + 8,  % was 8	% 3.8.2011
	assert(schaal(_Win, ElementHoogte, Kolombreedte, Kolom0, Row0,1,FontSize,Hor,Vert,Aantal)),
	!.

poule_winit(NewWin, _Win, FontSize) :-
	Font = font_Create(ff_Helvetica, [], FontSize),
	win_SetFont(_Win, Font),
	%_FontName = font_GetAttrs(Font, _StyleFlags,FontSize),
	win_GetFontMetrics(_Win, _Leading, Ascent, _Descent),
	displayP(_Win, CatNo,_OrigX,_OriginY,_Row,_Col),
        onderdeelbreedte(CatNo, FontSize, Kolombreedte),
	Letterhoogte  = Ascent + 4,
	ElementHoogte = Letterhoogte * 3,
	Kolom0        = 15 * FontSize,
	Row0          = Letterhoogte + 10,
	retract(schaal(NewWin, _, _, _, _,_, _Schaal, _H, _V,Aantal)),
	Hor  = Aantal * Kolombreedte  + Kolom0 + 2,
	Vert = Aantal * ElementHoogte + Row0 + 1,
	assert(schaal(_Win, ElementHoogte, Kolombreedte, Kolom0, Row0,1,FontSize,Hor,Vert,Aantal)),
	!.

/*pouleSpelersInLijst(Cat, Sp) :-
  opg(_, Cat, T, _,_,_,_,_),
  tgnstNum(T, Sp).*/
  
popupPoel(Win) :-
  getbacktrack(Here),
  displayP(Win, Cat, _, _, Row, Kol),
  partijC(Win, Row, T1, _, _, _),
  partijC(Win, Kol, T2, _, _, _),
  wedst(Num,Cat,T1,T2,_,_LR,_),	% om deze wedstrijd ging het
  cutbacktrack(Here),
  planbaar(Num, Cat, PlanB),
  invoerbaar(Num, Cat, Inv),
  %corrigeerbaar(Num, Cat, Cor),
  wisplanbaar(Num, Cat, PlanW),
  waarschuwbaar(Win, Num, Cat, Gew),
  baanbaar(Num, Cat, Bnb),
  koppelingen(KoppelB),
  %findall(Sp, pouleSpelersInLijst(Cat, Sp), SpelersList),
  %de_Spelers(Num, Cat, T1, T2, SpelersList),
  %findall(MItem, namenX(SpelersList, MItem), MenuSpelerItems),
  Menu = dyn_menu([
    txt(id_Wedstrijd_plan,"&Plan\tF5",0,PlanB,0,[]),
    txt(id_Wedstrijd_wis_plan,"&Wis plan",0,PlanW,0,[]),
    txt(id_Wedstrijd_uitslag_invoeren,"&Uitslag invoeren\tF9",0,Inv,0,[]),
    txt(idr_gewaarschuwd,"&Gewaarschuwd boeken",0,Gew,0,[]),		% lokaal afhandelen
    txt(idr_baannummer,"&Locatietoewijzing",0,Bnb,0,[]),
    txt(id_Onderdeel_Plan_per_poule,"&Plan per poule",0,b_True,0,[]),	% plan per poule
    %txt(0,"&Deelnemers", 0, b_True, 0, MenuSpelerItems),
    separator,
    txt(id_Speler_lijsten,"Deelnemers in &lijst", 0, b_True, 0, []),
    separator,
    txt(id_Wedstrijd_overzicht,"&Wedstrijdoverzicht\tF7",0,PlanB,0,[]),
    txt(idb_koplg,"&Koppelingoverzicht",0,KoppelB,0,[]),
    separator(),
    txt(idr_copy,"&Kopiëren\tCtrl-C",0,b_True,0,[]),
    txt(lettergrootte,"&Lettergrootte",0,b_True,0,[])
  ]), !,
  cursor_Rect(Win, Rect),
  Rect = rct(L,T,R,B),
  Xm = (L + R) div 2, Ym = (T + B) div 2,
  menu_PopUp(Win,Menu, pnt(Xm,Ym), align_Right).

restore_toolbar(Win) :-
  %write("\nHier restore toolbar (in tasvpoel)"),
  toolbar_GetRect(Win, tb_bottom, _), !.
restore_toolbar(Win) :-
  write("\ntoolbar_getrect failed!\nHier restore toolbar (in tasvpoel) create"),
	tb_schemabar_Create(Win).

  win_poule_window_Create(_Parent, Onderdeel, _WedsNo, _, _):-
	wc(CatNo, _, _, Onderdeel, _, _, _, _, _, _, _, _, _, _, _),
	%wc(CatNo, Onderdeel,_,_,_,_,_),
	schemawin(Win, CatNo),
	%displayP(_Win, CatNo,_X,_Y, _CursorR, _CursorK),
	not(pos(CatNo, _, _, _)),  		% inmiddels niet meer bestaand (gewist)
	win_Destroy(Win), !.
  win_poule_window_Create(_Parent, Onderdeel, WedsNo, KolomI, Tgnst):-
	wc(CatNo, _, _, Onderdeel, _, _, _, _, _, _, _, _, _, _, _),
	%wc(CatNo, Onderdeel,_,_,_,_,_),
	%pos(CatNo, _, _Cursor, poel),  	% een ander wordt geopend 
	schemaWin(Win, Cat),
	%displayP(Win, Cat,_,_,_, _),
	Cat <> CatNo,
	win_SetText(Win, Onderdeel),
	cleanSchemaWin(Win),
	retract(schemaWin(Win, _)),
	assert(schemaWin(Win, CatNo)),
	restore_toolbar(Win),			% 22.8.2004
	retract(pos(CatNo, Card, _Cursor, poel)),
	asserta(pos(CatNo, Card, WedsNo, poel)),
	retractall(displayP(Win, _,_,_,_, _)),
	retractall(partijC(Win, _, _, _, _, _)),
	%retractall(rank(Win, _, _)) ,
	%retractall(rankNieuw(Win, _, _, _)) ,
	%retractall(ranking(Win,_)),
	retractall(lastKol(Win, _)),
	win_SetHandler(Win,win_poule_window_eh),	% hergebruik
  %write("\n\npoulewindow_create"),
	poulestand_init(Win, CatNo, Aantal),  % geplaatsten eerst
	init_cursor(Win, WedsNo, CatNo, Row, Kol, KolomI, Tgnst),
	asserta(displayP(Win, CatNo,0,0,Row, Kol)),
	%schaal(Win, _, _, _, _,_, FontSize, _, _, _),
	retractall(schaal(Win, _, _, _, _,_,_,_,_,_)),	% winpoel
	getWcDisp(CatNo, FontSize),
	assert(schaal(Win, 0, 0, 0, 0,1, FontSize,0,0,0)),
	poule_winit(Win, Win, FontSize, Aantal),
	ClientRct = win_GetClientRect(Win),
	TaskWin = vpi_GetTaskWin(),
	TaskRct = win_GetClientRect(TaskWin),
	schaal(Win, _, _, _, _,_,_, Horz, Vert, _),
	adapt(Win, ClientRct, TaskRct, Horz, Vert, 30),
	win_Invalidate(Win), !.
  win_poule_window_Create(_Parent, Onderdeel, WedsNo, KolomI, Tgnst):-
	wc(CatNo, _, _, Onderdeel, _, _, _, _, _, _, _, _, _, _, _),
	%wc(CatNo, Onderdeel,_,_,_,_,_),
	retract(displayP(Win, CatNo,A,B,_, _)),
	init_cursor(Win, WedsNo, CatNo, Row, Kol, KolomI, Tgnst),
	asserta(displayP(Win, CatNo,A,B,Row, Kol)),
	%probeerFocus(Win), 
	win_Invalidate(Win), !.
  win_poule_window_Create(_Parent, Onderdeel, WedsNo, KolomI, Tgnst):-
  	assert(opTgnst(Tgnst)),
	wc(CatNo, _, _, Onderdeel, _, _, _, _, _, _, _, _, _, _, _),
	%wc(CatNo, Onderdeel,_,_,_,_,_),
	retract(pos(CatNo, Card, _Cursor, poel)),
	asserta(pos(CatNo, Card, WedsNo, poel)),
	NewWin = cast(WINDOW, 0),	   
	asserta(displayP(NewWin, CatNo,0,0,WedsNo, KolomI)),
	getWcDisp(CatNo, FSize),
	assert(schaal(NewWin, 0, 0, 0, 0,1, FSize,0,0,0)),
	win_Create(win_poule_window_WinType,win_poule_window_RCT,Onderdeel,
		   win_poule_window_Menu,_Parent,win_poule_window_Flags,win_poule_window_eh,0),
	!.

%BEGIN Poule window, e_Create
  win_poule_window_eh(_Win,e_Create(_),0):-!,
  	opTgnst(Tgnst),
	assert(metCursor(b_True, 0)),
	NewWin = cast(WINDOW, 0),	   
	retract(displayP(NewWin, CatNo,_,_,WNo, KolomI)),
	retractall(schemaWin(_, _)),
	assert(schemaWin(_Win, CatNo)),
  %write("\n\npoulewindow_create"),
	poulestand_init(_Win, CatNo, Aantal),   % geplaatsten eerst
	init_cursor(_Win, WNo, CatNo, Row, Kol, KolomI, Tgnst),
	assert(displayP(_Win, CatNo,0,0,Row, Kol)),
	schaal(NewWin, _, _, _, _,_, FontSize, _, _, _),
	%assert(schaal(NewWin, 0, 0, 0, 0,1, Mode,8)),
	poule_winit(NewWin, _Win, FontSize, Aantal),
%BEGIN Poule window, InitControls, 16:42:39-24.7.2011, Code automatically updated!
%END Poule window, InitControls
%BEGIN Poule window, ToolbarCreate, 16:42:39-24.7.2011, Code automatically updated!
	tb_schemabar_Create(_Win),
%END Poule window, ToolbarCreate
	%afmeting(_Win, Horz, Vert),
	ClientRct = win_GetClientRect(_Win),
	TaskWin = vpi_GetTaskWin(),
	TaskRct = win_GetClientRect(TaskWin),
	schaal(_Win, _, _, _, _,_,_FSize, Horz, Vert, _),
	adapt(_Win, ClientRct, TaskRct, Horz, Vert, 30),
	!.
%END Poule window, e_Create
%MARK Poule window, new events

%BEGIN Poule window, e_MouseDbl
  win_poule_window_eh(_Win,e_MouseDbl(_PNT,_ShiftCtlAlt,_Button),0):-!,
	schaal(_Win,Rijhoogte, Kolombreedte, Kolom0, Row0,_,_,Hor,Vert,_),
	displayP(_Win, Cat,OrigX,OrigY,_CursorR, _CursorK),
	_PNT = pnt(Xp, Yp),
	%afmeting(_Win, Hor, Vert),
	Xp > Kolom0,
	Xp < Hor + OrigX,
	Yp > Row0,
	Yp < Vert + OrigY,
	Kol = (Xp - OrigX - Kolom0) div Kolombreedte + 1,
	Row = (Yp - OrigY - Row0) div Rijhoogte + 1,
	Kol <> Row,
  	cursor_Rect(_Win, OldRect),
  	cursor_Rect(_Win, NewRect, Kol, Row),
	boundaryXY(_Win, NewRect, OldRect, Kol, Row),	% eerst naar Kol toe
	displayP(_Win, _,_, _, RowN, KolN),
	partijC(_Win, RowN, T1, _, _, _),
	partijC(_Win, KolN, T2, _, _, _),
	wedst(Num,Cat,T1,T2,_,_LR,_),	% om deze wedstrijd ging het
	TaskWin = vpi_GetTaskWin(),
	situatie(TaskWin, Num, Cat, 0), !.
%END Poule window, e_MouseDbl

%BEGIN Poule window, id_Speler_gegevens
  win_poule_window_eh(_Win,e_Menu(id_Speler_gegevens,_ShiftCtlAlt),0):-!,
	displayP(_Win, Cat,_X,_Y, _, _),
	findall(Sp, poulespeler(Cat, Sp), SpList),
	setSelCrit([],SpList),
	TaskWin = vpi_GetTaskWin(),
	win_spelersel_Create(TaskWin, 16),
	spelerSelRefresh(),
	!.
%END Poule window, id_Speler_gegevens

%BEGIN Poule window, idt_plan
  win_poule_window_eh(_Win,e_Menu(idt_plan,_ShiftCtlAlt),0):-!,
	getbacktrack(Here),
	displayP(_Win, Cat,_X,_Y, Row, Kol),
	partijC(_Win, Row, T1, _, _, _),
	partijC(_Win, Kol, T2, _, _, _),
	wedst(Num,Cat,T1,T2,_,_LR,_),	% om deze wedstrijd ging het
	cutbacktrack(Here),
	planMetAgenda(Num, Cat),
	!.
%END Poule window, idt_plan

%BEGIN Poule window, e_Timer
  win_poule_window_eh(_Win,e_Timer(_TimerId),0):-!,
	timer_Kill(_TimerId),
	URCT = win_GetClientRect( _Win ),		
	URCT = win_GetClientRect( _Win ),		% gewacht tot cursor verdwenen is
	URCT = rct(X0, Y0, X1, _Y1),
	toolbar_GetRect(_Win,tb_bottom,TbRCT),
	TbRCT = rct(_, YC1, _, _B1),
	draw_Line(_Win,pnt(X0,Y0),pnt(X1,Y0)),				% 
	schaal(_Win, _Hoogte, _Kolombreedte, _Kolom0, _Row0, _Marge, _Schaal, _Hor, Vert, _),
	draw_Line(_Win,pnt(X0,Y0),pnt(X0,Vert)),				% 
	Picture = pict_GetFromWin(_Win,rct(X0, Y0, X1, YC1)),
	cb_PutPicture( Picture ), 
	dlg_MessageBox( "Kopie", "Zichtbare stuk gekopieerd naar het klembord.\nGebruik plakken in bijvoorbeeld Word.", mesbox_iconinformation, mesbox_buttonsok, mesbox_defaultfirst, mesbox_suspendapplication ),
	assert(metCursor(b_True, 0)),
	win_Invalidate(_Win),
	!.
%END Poule window, e_Timer

%BEGIN Poule window, e_KeyUp
  win_poule_window_eh(_Win,e_KeyUp('C', c_Control),0):-!,
	win_PostEvent(_Win,e_Menu(idr_copy,0)),
	!.
%END Poule window, e_KeyUp

%BEGIN Poule window, idb_koplg
  win_poule_window_eh(_Win,e_Menu(idb_koplg,_ShiftCtlAlt),0):-
	displayP(_Win, Cat,_X,_Y, _Row, _Kol),
	kopl(Cat, algemeen, CatN, algemeen),
	pos(CatN, _, New, _),
	select(CatN, New), !.
  win_poule_window_eh(_Win,e_Menu(idb_koplg,_ShiftCtlAlt),0):-!,
	displayP(_Win, Cat,_X,_Y, _Row, _Kol),
	dlg_koppelingenoverzicht_Create(_Win, [Cat]),
	!.
%END Poule window, idb_koplg

%BEGIN Poule window, e_User
  win_poule_window_eh(_Win,e_User(upd_toolb,_Ptr),0):-
	toolbar_SetValue(_Win, id_Wedstrijd_uitslag_invoeren, ctrl_value(0,1)),
	getbacktrack(Here),
	displayP(_Win, Cat,_X,_Y, Row, Kol),
	partijC(_Win, Row, T1, _, _, _),
	partijC(_Win, Kol, T2, _, _, _),
	wedst(Num,Cat,T1,T2,_,_LR,_),	% om deze wedstrijd ging het
	cutbacktrack(Here),
	invoerbaar(Num, Cat, b_True),
	toolbar_SetValue(_Win, id_Wedstrijd_uitslag_invoeren, ctrl_value(1,1)),
	fail.
  win_poule_window_eh(_Win,e_User(upd_toolb,_Ptr),0):-
	toolbar_SetValue(_Win, idb_koplg, ctrl_value(0,1)),
	getbacktrack(Here),
	displayP(_Win, Cat,_X,_Y, _Row, _Kol),
	kopl(Cat, _, _CatN, _No),
	cutbacktrack(Here),
	toolbar_SetValue(_Win, idb_koplg, ctrl_value(1,1)),
	fail.
  win_poule_window_eh(_Win,e_User(upd_toolb,_Ptr),0):-
	toolbar_SetValue(_Win, idts_overzicht, ctrl_value(0,1)),
	displayP(_Win, Cat,_X,_Y, Row, Kol),
	partijC(_Win, Row, T1, _, _, _),
	partijC(_Win, Kol, T2, _, _, _),
	wedst(Num,Cat,T1,T2,_,_LR,_),	% om deze wedstrijd ging het
	beeldbaar(Num, Cat, Beeld),
	Beeld = b_True,
	toolbar_SetValue(_Win, idts_overzicht, ctrl_value(1,1)),
	fail.
  win_poule_window_eh(_Win,e_User(upd_toolb,_Ptr),0):-
	toolbar_SetValue(_Win, idt_plan, ctrl_value(0,1)),
	displayP(_Win, Cat,_X,_Y, Row, Kol),
	partijC(_Win, Row, T1, _, _, _),
	partijC(_Win, Kol, T2, _, _, _),
	wedst(Num,Cat,T1,T2,_,_LR,_),	% om deze wedstrijd ging het
	planbaar(Num, Cat, PlanBaar),
	PlanBaar = b_True,
	toolbar_SetValue(_Win, idt_plan, ctrl_value(1,1)),
	fail.
  win_poule_window_eh(_Win,e_User(_Id,_Ptr),0):-
	!.
%END Poule window, e_User

%BEGIN Poule window, id_Onderdeel_print
  win_poule_window_eh(_Win,e_Menu(id_Onderdeel_print,_ShiftCtlAlt),0):- !,
	displayP(_Win, Cat,_X,_Y, _Row, _Kol),
	bovencat(Cat, CatBoven),
	dlg_maak_webpaginas_Create(_Win, afdruk, [CatBoven], b_True), % meteen afdruk 
	!.
%END Poule window, id_Onderdeel_print


%BEGIN Poule window, idts_popup
  win_poule_window_eh(_Win,e_Menu(idts_popup,_ShiftCtlAlt),0):-
	popupPoel(_Win), !.
%END Poule window, idts_popup

%BEGIN Poule window, idts_overzicht
  win_poule_window_eh(_Win,e_Menu(idts_overzicht,_ShiftCtlAlt),0):-!,
	displayP(_Win, Cat,_X,_Y, Row, Kol),
	partijC(_Win, Row, T1, _, _, _),
	partijC(_Win, Kol, T2, _, _, _),
	wedst(Num,Cat,T1,T2,_,_LR,_),	% om deze wedstrijd ging het
	TaskWin = vpi_GetTaskWin(),
	situatie(TaskWin, Num, Cat, 0), 
	!.
%END Poule window, idts_overzicht

%BEGIN Poule window, idt_volgende
  win_poule_window_eh(_Win,e_Menu(idt_volgende,_ShiftCtlAlt),0):-!,
	displayP(_Win, _CatNo,_X,_Y, CursorR, CursorK),
	incr(CursorK, CursorR, _Win, Kol, Row),
	cursor_Rect(_Win, OldRct),
	cursor_Rect(_Win, NewRct, Kol, Row),
	boundaryXY(_Win, NewRct, OldRct, Kol, Row),
	!.
%END Poule window, idt_volgende

%BEGIN Poule window, idt_terug
  win_poule_window_eh(_Win,e_Menu(idt_terug,_ShiftCtlAlt),0):-
	displayP(_Win, _CatNo,_X,_Y, CursorR, CursorK),
	CursorK > 1,
	decr(CursorK, CursorR, Kol, Row),
	cursor_Rect(_Win, OldRct),
	cursor_Rect(_Win, NewRct, Kol, Row),
	boundaryXY(_Win, NewRct, OldRct, Kol, Row),
	!.
  win_poule_window_eh(_Win,e_Menu(idt_terug,_ShiftCtlAlt),0):-
	displayP(_Win, Cat,_X,_Y, Row, _Kol),
	partijC(_Win, Row, openplaats(T1), _, _, _),
	kopl(CatN, _, Cat, plaats(T1)),
	pos(CatN, _, New, _),
	select(CatN, New), !.
  win_poule_window_eh(_Win,e_Menu(idt_terug,_ShiftCtlAlt),0):-
	displayP(_Win, Cat,_X,_Y, Row, _Kol),
	partijC(_Win, Row, T1, _, _, _),
	kopl(VCat, _, Cat, _),
	opg(_OpgNum, VCat, T1, _,_,_,_,_,_,_,_),
	pos(VCat, _, Num, _),
	select(VCat, Num), !.
%END Poule window, idt_terug

%BEGIN Poule window, idt_op
  win_poule_window_eh(_Win,e_Menu(idt_op,_ShiftCtlAlt),0):-!,
	displayP(_Win, _CatNo,_X,_Y, CursorR, CursorK),
	decr(CursorR, CursorK, Row, Kol),
	cursor_Rect(_Win, OldRct),
	cursor_Rect(_Win, NewRct, Kol, Row),
	boundaryXY(_Win, NewRct, OldRct, Kol, Row),
	!.
%END Poule window, idt_op

%BEGIN Poule window, idt_neer
  win_poule_window_eh(_Win,e_Menu(idt_neer,_ShiftCtlAlt),0):-!,
	displayP(_Win, _CatNo,_X,_Y, CursorR, CursorK),
	incr(CursorR, CursorK, _Win, Row, Kol),
	cursor_Rect(_Win, OldRct),
	cursor_Rect(_Win, NewRct, Kol, Row),
	boundaryXY(_Win, NewRct, OldRct, Kol, Row),
	!.
%END Poule window, idt_neer

%BEGIN Poule window, idt_klein
  win_poule_window_eh(_Win,e_Menu(idt_klein,_ShiftCtlAlt),0):-!,
	schaal(_Win, _, _, _, _,_, FontSize,_,_,_),
	FontSize1 = FontSize - 1,
	poule_winit(_Win, _Win, FontSize1),
	%afmeting(_Win, Horz, Vert),
	TaskWin = vpi_GetTaskWin(),
	TaskRct = win_GetClientRect(TaskWin),
	ClientRct = win_GetClientRect(_Win),
	schaal(_Win, _, _, _, _,_, _,Horz,Vert,_),	% zijn inmiddels veranderd!
	adapt(_Win, ClientRct, TaskRct, Horz, Vert, 30),
	win_Invalidate(_Win),
	!.
%END Poule window, idt_klein

%BEGIN Poule window, idt_groot
  win_poule_window_eh(_Win,e_Menu(idt_groot,_ShiftCtlAlt),0):-!,
	schaal(_Win, _, _, _, _,_, FontSize,_,_,_),
	FontSize1 = FontSize + 1,
	poule_winit(_Win, _Win, FontSize1),
	%afmeting(_Win, Horz, Vert),
	TaskWin = vpi_GetTaskWin(),
	TaskRct = win_GetClientRect(TaskWin),
	ClientRct = win_GetClientRect(_Win),
	schaal(_Win, _, _, _, _,_, _,Horz,Vert,_),
	adapt(_Win, ClientRct, TaskRct, Horz, Vert, 30),
	win_Invalidate(_Win),
	!.
%END Poule window, idt_groot

%BEGIN Poule window, e_Size
  win_poule_window_eh(_Win,e_Size(_Width, _Height),0):-
ifdef use_tbar
	toolbar_Resize(_Win),
enddef
	!.
%END Poule window, e_Size


  win_poule_window_eh(_Win,e_Control(_,_CtrlType,_CtrlWin,_CtlInfo),0):-
	win_SetFocus(_Win), !.		% zorg dat toetsen te bedienen blijven
	
%BEGIN Poule window, e_MouseDown
  %win_poule_window_eh(_Win,e_MouseDown(_PNT,_ShiftCtlAlt,mouse_button_left),0):-
  win_poule_window_eh(_Win,e_MouseDown(_PNT,_ShiftCtlAlt,_),0):-	% 15.11.2010
	schaal(_Win,Rijhoogte, Kolombreedte, Kolom0, Row0,_,_,Hor,Vert,_),
	displayP(_Win, _CatNo,OrigX,OrigY,_CursorR, _CursorK),
	_PNT = pnt(Xp, Yp),
	%afmeting(_Win, Hor, Vert),
	Xp > Kolom0,
	Xp < Hor + OrigX,
	Yp > Row0,
	Yp < Vert + OrigY,
	Kol = (Xp - OrigX - Kolom0) div Kolombreedte + 1,
	Row = (Yp - OrigY - Row0) div Rijhoogte + 1,
	Kol <> Row,
  	cursor_Rect(_Win, OldRect),
  	cursor_Rect(_Win, NewRect, Kol, Row),
	boundaryXY(_Win, NewRect, OldRect, Kol, Row),	% eerst naar Kol toe
	fail.
  win_poule_window_eh(_Win,e_MouseDown(_PNT,_ShiftCtlAlt,mouse_button_right),0):-
	cursor_Rect(_Win, OldRect),
	rect_PntInside(OldRect,_PNT),
	popupPoel(_Win), !.
  win_poule_window_eh(_Win,e_MouseDown(_PNT,_ShiftCtlAlt,_Button),0).
%END Poule window, e_MouseDown

%BEGIN Poule window, e_Char
  win_poule_window_eh(_Win,e_Char('c',_ShiftCtlAlt),0):-
        win_onderdeelselectie_Create(_Win), !.
  win_poule_window_eh(_Win,e_Char('\27',_ShiftCtlAlt),0):-
	win_Destroy(_Win), !.
  win_poule_window_eh(_Win,e_Char(k_enter,_ShiftCtlAlt),0):-
	popupPoel(_Win), !.
  win_poule_window_eh(_Win,e_Char('g',_ShiftCtlAlt),0):-
	schaal(_Win, _, _, _, _, _, FontSize,_,_,_),
	FontSize1 = FontSize + 1,
	poule_winit(_Win, _Win, FontSize1),
	%afmeting(_Win, Horz, Vert),
	TaskWin = vpi_GetTaskWin(),
	TaskRct = win_GetClientRect(TaskWin),
	ClientRct = win_GetClientRect(_Win),
	schaal(_Win, _, _, _, _, _, _,Horz,Vert,_),
	adapt(_Win, ClientRct, TaskRct, Horz, Vert, 30),
	win_Invalidate(_Win),
	!.
  win_poule_window_eh(_Win,e_Char('k',_ShiftCtlAlt),0):-
	schaal(_Win, _, _, _, _,_, FontSize,_,_,_),
	FontSize1 = FontSize - 1,
	poule_winit(_Win, _Win, FontSize1),
	%afmeting(_Win, Horz, Vert),
	TaskWin = vpi_GetTaskWin(),
	TaskRct = win_GetClientRect(TaskWin),
	ClientRct = win_GetClientRect(_Win),
	schaal(_Win, _, _, _, _, _, _,Horz,Vert,_),
	adapt(_Win, ClientRct, TaskRct, Horz, Vert, 30),
	win_Invalidate(_Win),
	!.
  win_poule_window_eh(_Win,e_Char('+',_ShiftCtlAlt),0):-
	displayP(_Win, CurrCat, _, _, _, _),
	open_nextCat('+',CurrCat),
	!.
  win_poule_window_eh(_Win,e_Char('-',_ShiftCtlAlt),0):-
	displayP(_Win, CurrCat, _, _, _, _),
	open_nextCat('-',CurrCat),
	!.
  win_poule_window_eh(_Win,e_Char(k_f5,_ShiftCtlAlt),0):-
	displayP(_Win, Cat,_X,_Y, Row, Kol),
	partijC(_Win, Row, T1, _, _, _),
	partijC(_Win, Kol, T2, _, _, _),
	wedst(Num,Cat,T1,T2,_,_LR,_),	% om deze wedstrijd ging het
	planAangewezenWeds(_Win, Num, Cat),
	!.
  win_poule_window_eh(_Win,e_Char(k_f7,_ShiftCtlAlt),0):-
	displayP(_Win, Cat,_X,_Y, Row, Kol),
	partijC(_Win, Row, T1, _, _, _),
	partijC(_Win, Kol, T2, _, _, _),
	wedst(Num,Cat,T1,T2,_,_LR,_),	% om deze wedstrijd ging het
	TaskWin = vpi_GetTaskWin(),
	situatie(TaskWin, Num, Cat, 0),
	%write("Overzicht/beeld"),
	!.
  win_poule_window_eh(_Win,e_Char(k_f9,_ShiftCtlAlt),0):-
	displayP(_Win, Cat,_X,_Y, Row, Kol),
	partijC(_Win, Row, T1, _, _, _),
	partijC(_Win, Kol, T2, _, _, _),
	wedst(Num,Cat,T1,T2,_,_LR,_),	% om deze wedstrijd ging het
	dlg_uitslaginvoer1_Create(_Win, Num, Cat, b_False), 
	poulestand_init(_Win, Cat, _), 
	update_uitslagen("poul f9"),
	trap(win_Invalidate(_Win), _, true),
	%write("Uitslag invoer/corrigeren"),
	!.
  win_poule_window_eh(_Win,e_Char(k_right,_ShiftCtlAlt),0):-
	displayP(_Win, _CatNo,_X,_Y, CursorR, CursorK),
	incr(CursorK, CursorR, _Win, Kol, Row),
	cursor_Rect(_Win, OldRct),
	cursor_Rect(_Win, NewRct, Kol, Row),
	boundaryXY(_Win, NewRct, OldRct, Kol, Row),
	!.
  win_poule_window_eh(_Win,e_Char(k_left,_ShiftCtlAlt),0):-
	displayP(_Win, _CatNo,_X,_Y, CursorR, CursorK),
	decr(CursorK, CursorR, Kol, Row),
	cursor_Rect(_Win, OldRct),
	cursor_Rect(_Win, NewRct, Kol, Row),
	boundaryXY(_Win, NewRct, OldRct, Kol, Row),
	!.
  win_poule_window_eh(_Win,e_Char(k_down,_ShiftCtlAlt),0):-
	displayP(_Win, _CatNo,_X,_Y, CursorR, CursorK),
	incr(CursorR, CursorK, _Win, Row, Kol),
	cursor_Rect(_Win, OldRct),
	cursor_Rect(_Win, NewRct, Kol, Row),
	boundaryXY(_Win, NewRct, OldRct, Kol, Row),
	!.
  win_poule_window_eh(_Win,e_Char(k_up,_ShiftCtlAlt),0):-
	displayP(_Win, _CatNo,_X,_Y, CursorR, CursorK),
	decr(CursorR, CursorK, Row, Kol),
	cursor_Rect(_Win, OldRct),
	cursor_Rect(_Win, NewRct, Kol, Row),
	boundaryXY(_Win, NewRct, OldRct, Kol, Row),
	!.
%END Poule window, e_Char

%BEGIN Poule window, e_Destroy
  win_poule_window_eh(_Win,e_Destroy,0):-
	retractall(schemawin(_,_)),
	cleanPouleWindow(_Win),
	catUnselect(),
	!.
%END Poule window, e_Destroy

%BEGIN Poule window, e_Update
  win_poule_window_eh(_Win,e_Update(_UpdateRct),0):-!,
	displayP(_Win, CatNo,X,Y,_CursorR,_CursorK),
	Font = win_GetFont(_Win),
	findall(M, partijC(_Win, M, _, _, _, _), MList),		% staat vooraan!
	%write("\n", MaxKols),
	sortint_c(MList),
	reverse(Mlist, [], MListR),
	MListR = [MaxKols | _], 
	toon_poule(CatNo,_Win,alleplanning,pnt(X,Y),pnt(0,0),Font, MaxKols),
	win_PostEvent(_Win,e_User(upd_toolb, 0)),
	!.
%END Poule window, e_Update

  win_poule_window_eh(_Win,e_Menu(id_Onderdeel_Plan_per_poule,_ShiftCtlAlt),0):-
	displayP(_Win, Cat,_X,_Y, _Row, _Kol),
	TaskWin = vpi_GetTaskWin(),
	win_uitslageninvoer_Create(TaskWin, 3, Cat), !.

  win_poule_window_eh(_Win,e_Menu(id_Speler_lijsten,_ShiftCtlAlt),0):-
	displayP(_Win, Cat,_X,_Y, Row, Kol),
	partijC(_Win, Row, T1, _, _, _),
	partijC(_Win, Kol, T2, _, _, _),
	wedst(Num,Cat,T1,T2,_,_LR,_),	% om deze wedstrijd ging het
	wd(Num, Cat, Tg1, Tg2, _, _, _),
	de_spelers(Num, Cat, Tg1, Tg2, Spelers),
	setSelCrit([],Spelers),
	TaskWin = vpi_GetTaskWin(),
	win_spelersel_Create(TaskWin, 16),
	spelerSelRefresh(),
	!.
  win_poule_window_eh(_Win,e_Menu(lettergrootte,_ShiftCtlAlt),0):-
	dlg_toernooiinstellingen_Create(_Win, []),
	!.
/*  win_poule_window_eh(_Win,e_Menu(ID,_ShiftCtlAlt),0):-
	SKeus = ID - id_menufuzz,
	SKeus > 0,
  sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Res),
  SpelerIn = sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Res), !,
	dlg_persoonsgegevens_Create(_Win, SpelerIn ).*/
  win_poule_window_eh(_Win,e_Menu(id_wedstrijd_plan,_CAS),0):-
	displayP(_Win, Cat,_X,_Y, Row, Kol),
	partijC(_Win, Row, T1, _, _, _),
	partijC(_Win, Kol, T2, _, _, _),
	wedst(Num,Cat,T1,T2,_,_LR,_),	% om deze wedstrijd ging het
	TaskWin = vpi_GetTaskWin(),
	planAangewezenWeds(TaskWin, Num, Cat),	% 15.8.2000
	!.
  win_poule_window_eh(_Win,e_Menu(id_wedstrijd_wis_plan,_CAS),0):-
	displayP(_Win, Cat,_X,_Y, Row, Kol),
	partijC(_Win, Row, T1, _, _, _),
	partijC(_Win, Kol, T2, _, _, _),
	wedst(Num,Cat,T1,T2,_,_LR,_),	% om deze wedstrijd ging het
	afgelasten(_Win, Num,Cat),
	!.
  win_poule_window_eh(_Win,e_Menu(id_Wedstrijd_uitslag_invoeren,_CAS),0):-
	displayP(_Win, Cat,_X,_Y, Row, Kol),
	partijC(_Win, Row, T1, _, _, _),
	partijC(_Win, Kol, T2, _, _, _),
	wedst(Num,Cat,T1,T2,_,_LR,_),	% om deze wedstrijd ging het
	TaskWin = vpi_GetTaskWin(),
	dlg_uitslaginvoer1_Create(TaskWin, Num, Cat, b_False), !,	% 24.7.2002 cut verhuisd naar hier
	poulestand_init(_Win, Cat, _), 
	update_uitslagen("poelwindow uitslag invoeren"),
	trap(win_Invalidate(_Win), _, true).
  win_poule_window_eh(_Win,e_Menu(id_Wedstrijd_overzicht,_CAS),0):-
	displayP(_Win, Cat,_X,_Y, Row, Kol),
	partijC(_Win, Row, T1, _, _, _),
	partijC(_Win, Kol, T2, _, _, _),
	wedst(Num,Cat,T1,T2,_,_LR,_),	% om deze wedstrijd ging het
	TaskWin = vpi_GetTaskWin(),
	situatie(TaskWin, Num, Cat, 0), !.
  win_poule_window_eh(_Win,e_Menu(idr_baannummer,_CAS),0):-
	displayP(_Win, Cat,_X,_Y, Row, Kol),
	partijC(_Win, Row, T1, _, _, _),
	partijC(_Win, Kol, T2, _, _, _),
	wedst(Num,Cat,T1,T2,_,_LR,_),	% om deze wedstrijd ging het
	TaskWin = vpi_GetTaskWin(),
	dlg_baannummer_Create(TaskWin, Num, Cat), !,
	cursor_Rect(_Win, Rect),
	trap(win_Invalidate(_Win, Rect), _, true), !.
  win_poule_window_eh(_Win,e_Menu(idr_gewaarschuwd,_CAS),0):-!,
	displayP(_Win, Cat,_X,_Y, Row, Kol),
	partijC(_Win, Row, T1, _, _, _),
	partijC(_Win, Kol, T2, _, _, _),
	tgnst_naam(nee, b_false, Cat, T1,Naam1,_),
	tgnst_naam(nee, b_False, Cat, T2,Naam2,_),
	Menu = dyn_menu([
	  txt(id_partijen_een,Naam1,0,1,0,[]),
	  txt(id_partijen_twee,Naam2,0,1,0,[])
        ]), !,
	cursor_Rect(_Win, Rect), Rect = rct(L,T,R,B), Xm = (L + R) div 2, Ym = (T + B) div 2,
	menu_PopUp(_Win,Menu, pnt(Xm,Ym), align_Left).
  win_poule_window_eh(_Win,e_menu(idr_copy,_CAS),0):-	
	win_Invalidate(_Win),
	cursor_SetWait(),
	TimerId = timer_Set(_Win, 500),
	assert(metCursor(b_False, TimerId)), !.% start copy sequence
  win_poule_window_eh(_Win,e_menu(id_partijen_een,_CAS),0):-	
	displayP(_Win, Cat,_X,_Y, Row, Kol),
	partijC(_Win, Row, T1, _, _, _),
	partijC(_Win, Kol, T2, _, _, _),
	wedst(Num,Cat,T1,T2,_,_LR,_),	% om deze wedstrijd ging het
	dlg_plangegevens_Create(mijnoproepen, _Win, "Informeren", T1, Num, Cat, _Answer, b_False),
	cursor_Rect(_Win, Rect),
	update_waarsch(),
	update_display(),
	trap(win_Invalidate(_Win, Rect), _, true), !.
  win_poule_window_eh(_Win,e_menu(id_partijen_twee,_CAS),0):-	
	displayP(_Win, Cat,_X,_Y, Row, Kol),
	partijC(_Win, Row, T1, _, _, _),
	partijC(_Win, Kol, T2, _, _, _),
	wedst(Num,Cat,T1,T2,_,_LR,_),	% om deze wedstrijd ging het
	dlg_plangegevens_Create(mijnoproepen, _Win, "Informeren", T2, Num, Cat, _Answer, b_False),
	cursor_Rect(_Win, Rect),
	update_waarsch(),
	update_display(),
	trap(win_Invalidate(_Win, Rect), _, true), !.

%BEGIN Poule window, e_Menu, Parent window 
  win_poule_window_eh(Win,e_Menu(ID,CAS),0):-!,
	trap(PARENT = win_GetParent(Win),_, true),
	win_SendEvent(PARENT,e_Menu(ID,CAS)),
	!.
%END Poule window, e_Menu, Parent window

%END_WIN Poule window

cleanPouleWindow(_Win) :-
	%retractall(schemawin(_,_)),
	retract(schaal(_Win, _, _, _, _,_,FontSize,_,_,_)),
	retract(displayP(_Win, Cat,_X,_Y, Row, Kol)),
	partijC(_Win, Row, T1, _, _, _),
	partijC(_Win, Kol, T2, _, _, _),
	wedst(Num,Cat,T1,T2,_,_LR,_),	% om deze wedstrijd ging het
	updateWcDisp(Cat, FontSize), 
	getbacktrack(Here),
	retract(pos(Cat, Card, _NoOld, Type)),
	cutbacktrack(Here),
	asserta(pos(Cat, Card, Num, Type)),
	fail.
cleanPouleWindow(_Win) :-
	retractall(schaal(_Win, _, _, _, _,_,_,_,_,_)),	% winpoel
	retractall(displayP(_Win, _,_,_, _, _)),
	retractall(partijC(_Win, _, _, _, _, _)).
	%retractall(rank(_Win, _, _)) ,
	%retractall(rankNieuw(_Win, _, _, _)).
	%retractall(ranking(_Win,_)).

poulespeler(Cat, Sp) :-
  opg(_OpgNum, Cat, T, _Plaatsing,_,_,_,_,_,_,_),
  getbacktrack(Point),
  wedst(_,Cat,T,_,_,_,_),
  cutbacktrack(Point),
  tgnstnum(T, Sp).

%_____________________________________________________________________________

domains

  wts      = wts(integer, wedscat, string,tijd,janee, BOOLEAN Bijwinst, string BaanPark) ;
             wtw(integer, wedscat, string Kort, tijd)
  wtsl     = wts*


predicates
  synchroniseer(tijd, integer, tijd, integer)  
procedure  positie(integer, integerlist, integer, integer)
%  dagen(integer, string)
  tijd_synch(integer, tijd)
  speeltijden2(integer,wtsl)
nondeterm  speeltijden_1(integer SpNo,wts) 
  qsortwp(wtsl,wtsl)
  splitw(wts,wtsl,wtsl,wtsl)
%nondeterm  wedstr_member(wedid,wl)
  prt_gewaarsch(janee,string) 
determ  appendwtsl(wtsl, wtsl, wtsl) -(i,i,o)


clauses

synchroniseer(VanAf, DagPositie, PlanTijd, D2) :-
  time_stamp1(Stamp, _, _),
  grootste(Stamp, VanAf, GTijd),
  tijd_synch(GTijd, PlanTijd),
  bitright(PlanTijd, 7, D2),
  findall(DagNum, dag(DagNum, _, _), DagNummers),
  positie(D2, DagNummers, 0, DagPositie), !. % zoek de positie in het dagenlijstje

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
positie(_, _, _, 0).
  
speeltijden2(SKeus,SpeeltijdenS) :-
  findall(Speeltijd,speeltijden_1(SKeus,Speeltijd),Speeltijden), 
  qsortwp(Speeltijden,SpeeltijdenS).

speeltijden_1(Skeus,wts(Num,Cat,NaamKort,WedsTijd,Gewaarsch, BW, BaanPark)) :-
  wd_planned(SKeus,Wedstrijden,alleplanning,b_True),
  wedstr_member(Wedstrijd,Wedstrijden),
  Wedstrijd = w(Num, Cat, WedsTijd, Gewaarsch, _, _, BW, BaanPark),
  wc(Cat, _, _, _Onderdeel, _, NaamKort, _, _, _, _, _, _, _, _, _).
  %wc(Cat,_,_,NaamKort,_,_,_).
speeltijden_1(SKeus, wtw(Num, Cat, NaamKort, Tijd)) :-
  ws(Num, Cat, Tgnst, Tijd),
  not(w2(Num,Cat,Tijd,_,_,_,_,_)),
  tgnst_num(Tgnst, SKeus),
  wc(Cat, _, _, NaamKort, _, _, _, _, _, _, _, _, _, _, _).
  %wc(Cat,_,_,NaamKort,_,_,_).


splitw(_,[],[],[]).
splitw(wts(N,C,S,T,G,BW,BP),[wts(N1,C1,S1,T1,G1,BW1,BP1)|X],[wts(N1,C1,S1,T1,G1,BW1,BP1)|Y],Z) :-
  T > T1, !,
  splitw(wts(N,C,S,T,G,BW,BP),X,Y,Z).
splitw(wts(N,C,S,T,G,BW,BP),[wtw(N1,C1,S1,T1)|X],[wtw(N1,C1,S1,T1)|Y],Z) :-
  T > T1, !,
  splitw(wts(N,C,S,T,G,BW,BP),X,Y,Z).
splitw(wtw(N,C,S,T),[wts(N1,C1,S1,T1,G1,BW1,BP1)|X],[wts(N1,C1,S1,T1,G1,BW1,BP1)|Y],Z) :-
  T > T1, !,
  splitw(wtw(N,C,S,T),X,Y,Z).
splitw(wtw(N,C,S,T),[wtw(N1,C1,S1,T1)|X],[wtw(N1,C1,S1,T1)|Y],Z) :-  % was gepland op
  T > T1, !,
  splitw(wtw(N,C,S,T),X,Y,Z).
splitw(H,[A|X],Y,[A|Z]) :- !,
  splitw(H,X,Y,Z).

qsortwp([],[]).
qsortwp([H|T],S) :-
  splitw(H,T,A,B),
  qsortwp(A,A1),
  qsortwp(B,B1),
  appendwtsl(A1,[H|B1],S), !.

prt_gewaarsch(nee,"?") :-
  oproepen(ja), !.
prt_gewaarsch(_," ").

appendwtsl([],X,X).
appendwtsl([H|L],L1,[H|L2]) :-
  appendwtsl(L,L1,L2).

%______________________________________________________________________________

%BEGIN_WIN Situatie Window
/**************************************************************************
        Creation and event handling for window: Situatie Window
**************************************************************************/

domains
  wh = wh(integer, integer, wedscat)
  whl = wh*

constants
%BEGIN Situatie Window, CreateParms, 11:08:42-18.2.2001, Code automatically updated!
  win_situatie_window_WinType = w_TopLevel
  win_situatie_window_Flags = [wsf_SizeBorder,wsf_TitleBar,wsf_Maximize,wsf_Close,wsf_ClipSiblings]
  win_situatie_window_RCT = rct(100,80,816,366)
  win_situatie_window_Menu = no_menu
  win_situatie_window_Title = "Wedstijdplan-zicht"
  win_situatie_window_Help = idh_wedstrijdoverzicht
%END Situatie Window, CreateParms
c_speler    = 0
c_wedstrijd = 1
c_zoomin    = 2
c_zoomuit   = 3
%opmW_resize = 33

database - situatie
  fontsize(integer)
 horIndex(WINDOW, integer)
 vertIndx(WINDOW, integer)
 displayS(WINDOW, integer WedNo, wedscat Cat, tijd, integer Y)
 hotspotS(WINDOW, RCT, integer SpNo)
 hotspotW(WINDOW, RCT, integer SpNo, integer Dag, integer, wedscat, string, tijd, janee, BOOLEAN BijWinst, string BaanPark)
 cursorAt(WINDOW, BOOLEAN, integer, integer, wedscat)  % b_False = Person, b_True = Wedstrijd
single kolomsize(integer Size)
  algehad(integer)
determ opmerkingenWin(window, integer ParentWidth, integer ParentHeigth, integer No, wedscat)
 
predicates

  win_situatie_window_eh : EHANDLER
  planning(integer, wedscat, tijd, tijd)
  sitRow0(WINDOW, integer Index, RCT WindowRCT)
  dagHeaders(WINDOW, integer Index, integer IndexNa, slist, RCT DagRct, integer Breedte) 
  gridLines(WINDOW, integer X, integerlist Dagen, RCT WindowRCT, integer XStop, integer Ystop)
  prt_blok(WINDOW, wedscat, integer IndexNa, integer Kolom0, integer Y0, integer YUit, RCT UpdateRct)
  prt_blok(WINDOW, integerlist Vanafdagen, verhl, integer I, integer Y0, integer Yin, integer Yuit, RCT UpdateRCT)
  blok_tekst(WINDOW, tijd, verhl, RCT, integer, integer, integer, RCT update)
  staart(integer, integer Index, integerlist Dagen, integerlist VanafDagen)  
  write_spS(WINDOW, integerlist SpNos,tijd WedsTijd, integer Yin, integer Yuit, RCT ) % onder elkaar
  prt_dagbegin(WINDOW, integerlist VanafDagen, tijd BegTijd, RCT , integer Yin, integer Yin2, RCT Upd)- (i, i, i, i, i, o, i)
  disp_verh1(WINDOW, integer Dag, verhl, RCT, RCT) - (i,i,i,i,o)
  disp_blok(WINDOW, integer Dag, verhl, RCT, RCT) - (i,i,i,i,o)
procedure  write_matches1(WINDOW, integer SpNo, integer Dag,  wtsl, tijd Wedstijd, RCT, RCT) - (i,i,i,i,i,i,o)
procedure  prt_dagen(WINDOW, integerlist Dagen, integer SpNo, verhl Verhl, wtsl Speeltijden, tijd Wedstijd, integer Ybase, integer Yin, integer Ytop, RCT, verhl Blok)- (i,i,i,i,i,i,i,i,o,i,i)
  merk(WINDOW, integer, wedscat) % tijd, tijd)
  merkBackground(WINDOW, integer, wedscat) %tijd, tijd)
  matchTekst(integer Dag, string Type, tijd, janee Gewaarsch, BOOLEAN BijWinst, string BaanPark, string Tekst)
procedure  write_hotspot(WINDOW, BOOLEAN, integer, integer, wedscat) - (i,i,i,i,i)
  win_titel(WINDOW, integer No, wedscat Cat, tijd Tijd)
  vorige_planning(WINDOW, janee, integer, wedscat, RCT, integer Hoogte, RCT, string, string)-(i,i,i,i,i,i,o,i,o)
  zelfdeWeds(integer, wedscat, integer, wedscat, BOOLEAN)
  existCursor(WINDOW)
  handleOldCursor(WINDOW, BOOLEAN, integer SpNo, integer Num, wedscat Cat) - (i,i,i,i,i)
  betalenGevraagd(janee Betaaladm, janee Betaald, BOOLEAN, string) - (i,i,o,o) % enable pop-up
  waarschuwenGevraagd(janee, integer SpNo, tgnst, tgnst, janee Gew1, janee Gew2, BOOLEAN)
  situatie_winit(WINDOW, integer Fontsize)
  initXYsize(WINDOW, integer OldSize, integer NewSize, integer XSize)
  nextHotSpot(WINDOW, 	BOOLEAN, integer SKeus, integer Num, wedscat Cat, 
  			BOOLEAN SorWN, integer SKeusN, integer NumN, wedscat CatN,
  		RCT)
  nextSp(integer, integerlist, integer)
nondeterm  whId(WINDOW, wh)
  nextWh(integer, integer, wedscat, whl, integer, integer, wedscat) -(i,i,i,i,o,o,o)
  getfontsize(integer)
%nondeterm  persoonsItems(integer Persno, BOOLEAN, BOOLEAN, string Tel1, string Tel2, string Tel3, MENU_ITEM)
%nondeterm  planItems(catl, MENU_ITEM)
procedure maxBaanText(WINDOW, integer)
procedure leeftijdBlok(string Geboren, verhl VerhinderingenL)
nondeterm spelersInCat(wedscat Cat, integer Sp, integerlist Spelers)
procedure uitbreiding(wedscat Cat, integerlist Spelers, integerlist Spelers)
procedure linksboven(integer Links, integer Top)
procedure notaWin(window, integer No, wedscat, integer XNo, wedscat)

clauses

kolomsize(0).

getfontsize(Size) :-
  fontsize(Size), !.
getfontsize(Size) :-
  schemaFontSize(Size),
  Size1 = Size + 1,
  assert(fontsize(Size1)).

whId(Win, wh(Sp, No, Cat)) :-
  hotspotW(Win, _RCT, Sp, _, No, Cat, _Type, _Tijd, _Gewaarsch, _BijWinst, _BaanPark).
  
nextWh(Sp, No, Cat, [wh(Sp, No, Cat),wh(SkeusN, NumN, CatN)|_], SKeusN, NumN, CatN) :- !.
nextWh(Sp, No, Cat, [_|Rest], SpN, NoN, CatN) :-
  nextWh(Sp, No, Cat, Rest, SpN, NoN, CatN). 

nextSp(Curr, [Curr,Next|_], Next) :- !.
nextSp(Curr, [_|Rest],Next) :-
  nextSp(Curr,Rest,Next).

nextHotSpot(_Win, c_speler, SKeus, Num, Cat, c_speler, SKeusN, Num, Cat, RCT) :-
  findall(Sp, hotspotS(_Win, _, Sp), SpL),
  nextSp(SKeus, SpL, SKeusN), 
  hotspotS(_Win, RCT, SKeusN), !.
nextHotSpot(_Win, c_speler, _SKeus, _Num, _Cat, c_wedstrijd, SKeusN, NumN, CatN, RCT) :-
  hotspotW(_Win, RCT, SKeusN, _, NumN, CatN, _Type, _Tijd, _Gewaarsch, _BW, _BaanPark), !.
nextHotSpot(_Win, c_wedstrijd, SKeus, Num, Cat, c_wedstrijd, SKeusN, NumN, CatN, RCT) :-
  findall(Wh, whId(_Win, Wh), WhL),
  nextWh(SKeus, Num, Cat, WhL, SKeusN, NumN, CatN),
  hotspotW(_Win, RCT, SKeusN, _, NumN, CatN, _Type, _Tijd, _Gewaarsch, _BW, _BaanPark), !.
nextHotspot(_Win, _, _SKeus, Num, Cat, c_speler, SKeusN, Num, Cat, RCT) :- 
  hotspotS(_Win, RCT, SKeusN), !.

close_situatie() :-
  displayS(SWin, _No1, _Cat1, _, _),
  win_Destroy(SWin), !.
close_situatie().
  
situatie(_Win, No,Cat,Tijd) :- 		% haal andere weg
  not(displayS(_, No, Cat, Tijd, _)),
  displayS(SWin, _, _, _, _),
  win_Destroy(SWin),
  fail.
situatie(_Win, No,Cat,Tijd) :- 		% hij is er al
  displayS(SWin, No, Cat, Tijd, _),
  win_SetFocus(SWin), !.
situatie(Win, No,Cat,Tijd) :- 
  planning(No, Cat, WTijd, Tijd),
  synchroniseer(0, Dpos, _, _),
  NulWin = cast(WINDOW, 0),
  assert(horIndex(NulWin, DPos)),
  win_situatie_window_Create(Win, No, Cat, WTijd),
  Tijd = 0, !.

planning(No, Cat, WedsTijd, _) :-
  w2(No,Cat,WedsTijd,_,_,_,_,_), !.
planning(No, Cat, WedsTijd, _) :-
  ws(No,Cat,_, WedsTijd), !.
planning(_, _, 0, 0) :- !.
planning(No, Cat, WedsTijd, WedsTijd) :-  	% laat tijdelijk oude planning zien!
  assert(w2(No,Cat,WedsTijd,ja,ja,' ',0,"")), !.

dagHeaders(_Win, _, _, [], _, _) :- !.
dagHeaders(Win, I, INa, [_Dag|Rest], DagRct, Breedte) :-
  I < INa,
  I1 = I + 1, !,
  dagHeaders(Win, I1, INa, Rest, DagRct, Breedte).
dagHeaders(Win, I, INa, [Dag|Rest], DagRct, Breedte) :-
  draw_TextInRect(Win,DagRct, Dag, -1,[dtext_center]),
  DagRct1 = rect_Offset(DagRct, Breedte, 0),
  dagHeaders(Win, I, INa, Rest, DagRct1, Breedte).

sitRow0(Win, I, rct(_, _, X, _Y)) :-
  schaal(Win, _Hoogte, Kolombreedte, Kolom0, Row0,_,_,_,_,_), !,
  SaveTools = win_GetDrawTools(Win),
  SaveTools = draw_tools(_Pen, _Brush, _Drwamode, _Font, _Foreground, _BackGround, _BK_MODE),
  win_SetForeColor(Win, color_White),
  win_SetBackColor(Win, color_Blue),
  %win_SetBackMode(Win, bk_Opaque),
  win_SetBrush(Win,brush(pat_Solid, color_Blue)),
  X1 = X + 10,
  draw_Rect(Win,rct(0, 0, X1, Row0)),
  draw_TextInRect(Win, rct(2, 0, Kolom0, Row0), "Naam", -1, [dtext_left,dtext_wordbreak]), 
  findall(DagS, dag(_, DagS, _),Dagen),
  Xr = Kolom0 + Kolombreedte,
  DagRct = rct(Kolom0, 0, Xr, Row0), 
  dagHeaders(Win, 0, I, Dagen, DagRct, Kolombreedte),
  win_SetForeColor(Win, _Foreground),
  win_SetBackColor(Win, _Background).
  
gridLines(Win, X0, [], _, X1, Ystop) :- 
  schaal(Win, _Hoogte, Kolombreedte, _Kolom0, _Row0,_,_,_,_,_), 
  X1 = X0 + 2 * Kolombreedte, !,
  draw_Line(Win, pnt(X0,0), pnt(X0,Ystop)).	% verticaal
gridLines(Win, X0, _, rct(_, _, X, _Y),X1, _) :-
  schaal(Win, _Hoogte, Kolombreedte, _Kolom0, _Row0,_,_,_,_,_), 
  X1 = X0 +  Kolombreedte, 
  X0 > X, !.					
gridLines(Win, X0, [_|Rest], Rect,Xuit, Ystop) :-
  schaal(Win, _Hoogte, Kolombreedte, _Kolom0, _Row0,_,_,_,_,_), 
  Pnt0 = pnt(X0, 0),
  draw_Line(Win, Pnt0, pnt(X0,Ystop)),
  X1 = X0 + KolomBreedte, !,
  gridLines(Win, X1, Rest, Rect,Xuit, Ystop).

staart(I, I1, Staart, Staart) :- 
  I >= I1, !.
staart(I, Index, [_|Dagen], Uit) :-
  I1 = I + 1, 
  staart(I1, Index, Dagen, Uit).

prt_blok(Win, Cat, _Index, Kolom0, Y0, _YUit, UpdateRct) :-
  bl(Cat, List),
  not(List = []),
  Y1 = Y0 + 1,
  Y2 = Y0 + Y0 - 1,
  K0 = Kolom0 - 1,
  RCT = rct(1, Y1, K0, Y2),
  RctT = rect_Intersect(RCT, UpdateRct),
  not(rect_IsEmpty(RctT)),
  draw_TextInRect(Win,RCT, "blokkeertijden", -1,[dtext_left]),
  fail.
prt_blok(Win, Cat, Index, _Kolom0, Y0, YUit1, UpdateRct) :-
  bl(Cat, List),
  not(List = []),
  findall(Dag, dag(Dag, _, _), Dagen),
  %horIndex(Win, Index),
  staart(0, Index, Dagen, VanafDagen),  
  prt_blok(Win, VanafDagen, List, 0, Y0, Y0, Yuit, UpdateRct),
  Y2 = Y0 + Y0,
  grootste(Y2, Yuit, Yuit1), !.
prt_blok(_, _, _, _, Y, Y, _).
     
prt_blok(Win, [D|Dagen], List, I, Y0, Yin, Yuit, UpdateRct) :-
  schaal(Win, Hoogte, Kolombreedte, Kolom0, _Row0,_,_,_,_,_), !,
  Kolom1 = Kolom0 + I * Kolombreedte,
  Kolom2 = Kolom1 + Kolombreedte,
  bitleft(D, 7, CurrDagTijd),
  Y1 = Y0 + Hoogte,
  RCT1 = rct(Kolom1, Y0, Kolom2, Y1),
  RCT2 = rect_Inflate(RCT1, -1, -1),
  blok_tekst(Win, CurrDagTijd, List, RCT2, Y0, Hoogte, Yuit1, UpdateRct),
  grootste(Yuit1, Yin, Yin1),
  I1 = I + 1,
  prt_blok(Win, Dagen, List, I1, Y0, Yin1, Yuit, UpdateRct).
prt_blok(_, _, _, _, _, Yuit, Yuit, _).

blok_tekst(_, _, [], _, Y, _, Y, _) :- !.	% de bloktekst voor een dag
blok_tekst(Win, CDT, [per(B,E)|_], RCT, Y0, Ydelta, Yuit, UpdateRct) :-
  RctT = rect_Intersect(RCT, UpdateRct),
  not(rect_IsEmpty(RctT)),
  CDT >= B - 1,
  CDT + 96 <= E,
  Yuit = Y0 + Ydelta,
  draw_TextInRect(Win,RCT, "hele dag", -1,[dtext_left]), !.
blok_tekst(Win, CDT, [per(B,E)|Rest], RCT, Y0, Ydelta, Yuit, UpdateRct) :-
  RctT = rect_Intersect(RCT, UpdateRct),
  not(rect_IsEmpty(RctT)),
  B >= CDT,
  E <= CDT + 96,
  tijd_kwart(_,U1,M1,B),
  tijd_kwart(_,U2,M2,E),
  helestr_int(0,MS1,M1),
  helestr_int(0,MS2,M2),
  format(Tekst,"%2.2\046%2.2-%2.2\046%2s",U1,MS1,U2,MS2),
  draw_TextInRect(Win,RCT, Tekst, -1,[dtext_left]), 
  RCT1 = rect_Offset(RCT, 0, Ydelta),
  Y01 = Y0 + Ydelta,
  blok_tekst(Win, CDT, Rest, RCT1, Y01, Ydelta, Yuit, UpdateRct), !.
blok_tekst(Win, CDT, [per(B,E)|_], RCT, Y0, Ydelta, Yuit, UpdateRct) :-
  RctT = rect_Intersect(RCT, UpdateRct),
  not(rect_IsEmpty(RctT)),
  B >= CDT,  
  B < CDT + 96,
  E >= CDT + 96,
  tijd_kwart(_,U1,M1,B),
  helestr_int(0,MS1,M1),
  format(Tekst,"vanaf %2.2\046%2.2",U1, MS1), 
  Yuit = Y0 + Ydelta,
  draw_TextInRect(Win,RCT, Tekst, -1,[dtext_left]), !.
blok_tekst(Win, CDT, [per(B,E)|Rest], RCT, Y0, Ydelta, Yuit, UpdateRct) :-
  RctT = rect_Intersect(RCT, UpdateRct),
  not(rect_IsEmpty(RctT)),
  B < CDT,
  E <= CDT + 96,  
  E > CDT,
  tijd_kwart(_,U1,M1,E),
  helestr_int(0,MS1,M1),
  format(Tekst,"voor  %2.2\046%2.2",U1, MS1), 
  Y01 = Y0 + Ydelta,
  draw_TextInRect(Win,RCT, Tekst, -1,[dtext_left]),
  RCT1 = rect_Offset(RCT, 0, Ydelta),
  blok_tekst(Win, CDT, Rest, RCT1, Y01, Ydelta, Yuit, UpdateRct), !.
blok_tekst(Win, CDT, [_|Rest], RCT, Y0, Ydelta, Yuit, UpdateRct) :-
  blok_tekst(Win, CDT, Rest, RCT, Y0, Ydelta, Yuit, UpdateRct).

leeftijdBlok(Geboren, List) :-
  leeftijd(Geboren, Leeftijd,0, _),
  blLft(Van, TM, List),
  Leeftijd >= Van,
  Leeftijd <= TM, !.
leeftijdBlok(_Geboren, []).

write_spS(Win, [SpNo|Rest],WedsTijd, Yin, Yuit, UpdateRct) :-        /* per speler de gegevens */
  sp2(SpNo,_,Naam,_Telthuis,Verhl,_,_,Betaald,BegTijd,_,_,_,_,_,_,Geboren,_Telwerk, _Telmobiel,_,_,_,_,_,_,_,_,_),
  betaal_adm(JaNee),
  betalenGevraagd(JaNee, Betaald, _Boeken, Teken),
  format(Tekst, "%s%s\n%s",Teken,Naam,""),  	% was Tel
  schaal(Win, Hoogte, Kolombreedte, Kolom0, _Row0,_Marge,_,_,_,_), 
  Yin2 = Yin + Hoogte,
  NaamRct = rct(0, Yin, Kolom0, Yin2),
  NaamRct1 = rect_Inflate(NaamRct, -1, -1),
  TelRct = rect_Offset(NaamRct1, 0, Hoogte),
  HotRct = rect_Union(NaamRct1, TelRct),
  win_SetBrush(Win,brush(pat_Hollow, color_White)),
  win_SetPen(Win,pen(1,ps_Dot,color_Black)),
  draw_Rect(Win,HotRct),
  draw_TextInRect(Win,HotRct, Tekst, -1,[dtext_left]),
  assert(hotSpotS(Win, HotRct, SpNo)),
  findall(Dag, dag(Dag, _, _), Dagen),
  horIndex(Win, Index), 
  staart(0, Index, Dagen, VanafDagen), 
  X2 = Kolom0 + Kolombreedte, 
  Y2 = Yin + Hoogte,
  Rct1 = rct(Kolom0, Yin, X2, Y2),
  Rct2 = rect_Inflate(Rct1, -1, -1),
  prt_dagbegin(Win, VanafDagen, BegTijd, Rct2, 0, Yin3, UpdateRct),
  speeltijden2(SpNo,SpeeltijdenS),
  Rct3 = rect_Offset(Rct2, 0, Yin3),
  leeftijdBlok(Geboren, BlokVerh),
  prt_dagen(Win, VanafDagen, SpNo, Verhl, SpeeltijdenS, WedsTijd, 0, Yin3, Yin4, Rct3, BlokVerh), !,
  count(Vanafdagen, 0, Aantal),
  Xline = Kolom0 + Aantal * Kolombreedte,
  Ymin = Yin + 2 * Hoogte,
  grootste(Yin4, Ymin, Yin5),
  win_SetPen(Win,pen(1,ps_Solid,color_Black)),
  draw_Line(Win,pnt(0, Yin5), pnt(Xline,Yin5)),  % hor streep tot aan laatste
  write_spS(Win, Rest,WedsTijd, Yin5, Yuit, UpdateRct).
write_sps(_Win,_,_,Y,Y,_).

prt_dagen(_, [], _, _, _, _, _, Y, Y, _, _) :- !. 
prt_dagen(Win, [Dag|Dagen], SpNo, Verhl, SpeeltijdenS, WedsTijd, Ybase, Yin, Ytop, Rct, Blok) :-
  disp_Blok(Win, Dag, Blok, Rct, Rct1),
  disp_verh1(Win, Dag, Verhl, Rct1, Rct2),
  write_matches1(Win, SpNo, Dag, SpeeltijdenS, WedsTijd, Rct2, RctUit),
  RctUit = rct(_, Y2, _, _),	% kijkhoe ver die gekomen is
  grootste(Yin, Y2, Yin1),
  schaal(Win, _Hoogte, Breedte, _Kolom0, _Row0,_Marge,_,_,_,_), !,
  Rct3 = rect_Offset(Rct, Breedte, 0),
  prt_dagen(Win, Dagen, SpNo, Verhl, SpeeltijdenS, WedsTijd, Ybase, Yin1, Ytop, Rct3, Blok).
prt_dagen(_, _, _, _, _, _, _, Y, Y, _, _).

matchTekst(Dag, Type, Tijd, Gewaarsch, BW, BaanPark, Tekst) :-
  baan_adm(ja),
  not(BaanPark = ""), !,
  tijd_kwart(D1,U1,M1,Tijd),
  D1 = Dag,
  helestr_int(0,MS1,M1),
  bijWinst(BW, _, BWS),
  prt_gewaarsch(Gewaarsch,Gstring),	% print eventueel het vraagteken
  format(Tekst,"%7.7s %2.2\046%2s %s%s [%s]",Type,U1,MS1,Gstring, BWS, BaanPark).
matchTekst(Dag, Type, Tijd, Gewaarsch, BW, _BaanPark, Tekst) :-
  tijd_kwart(D1,U1,M1,Tijd),
  D1 = Dag,
  helestr_int(0,MS1,M1),
  bijWinst(BW, _, BWS),
  prt_gewaarsch(Gewaarsch,Gstring),	% print eventueel het vraagteken
  format(Tekst,"%7.7s %2.2\046%2s %s%s",Type,U1,MS1,Gstring, BWS).

vorige_planning(_Win, nee, Num, Cat, RctIn, Hoogte, RctUit, Tekstin, TekstUit) :- 
  ws(Num, Cat, _Tgnst, Tijd),		% nog doen!!
  RctUit = rect_Offset(RctIn, 0, Hoogte),
  tijd_kwart(D1,U1,M1,Tijd),
  dag(D1,Dnaam,_), !,
  helestr_int(0,MS1,M1),
  format(TekstUit,"%s\n(%s %2.2\046%2s)",TekstIn,Dnaam, U1, MS1).
vorige_planning(_, _, _Num, _Cat, RCT, _, RCT, Tekst, Tekst).

write_matches1(_, _, _, [], _, RUit, RUit) :- !.	% game, set en match
write_matches1(Win, SpNo, Dag, [wts(Num,Cat,Type,Tijd,Gewaarsch,BW,BaanPark)|Tail], WedsTijd, Rct, RctUit) :-
  matchTekst(Dag, Type, Tijd, Gewaarsch, BW, BaanPark, TekstIn),
  schaal(Win, Hoogte, _Kolombreedte, _Kolom0, _Row0,_,_,_,_,_), 
  merk(Win, Num, Cat), % WedsTijd, Tijd),		% geef hem een kleurtje
  vorige_planning(Win, Gewaarsch, Num, Cat, Rct, Hoogte, RctUitV, TekstIn, TekstUit), %####!!
  Rct2 = rect_Offset(RctUitV, 0, Hoogte),
  RctHot = rect_Union(Rct, RctUitV),
  %write(".."),
  win_SetBrush(Win,brush(pat_Hollow, color_White)),	%
  win_SetPen(Win,pen(1,ps_Dot,color_Black)),		%
  draw_Rect(Win,RctHot),				%
  draw_TextInRect(Win,RctHot, TekstUit, -1, [dtext_left]),
  win_SetForeColor(Win, color_Black),
  win_SetBackColor(Win, color_White),
  assert(hotspotW(Win, RctHot, SpNo, Dag, Num, Cat, Type, Tijd, Gewaarsch, BW, BaanPark)) , !,
  write_matches1(Win, SpNo, Dag, Tail, WedsTijd, Rct2, RctUit).
write_matches1(Win, SpNo, Dag, [wtw(Num,Cat,NaamKort,Tijd)|Tail], WedsTijd, Rct, RctUit) :-
  tijd_kwart(D1,U1,M1,Tijd),
  D1 = Dag,
  helestr_int(0,MS1,M1),
  format(TekstUit,"afgelast:\n  %7.7s %2.2\046%2s ?",NaamKort,U1,MS1),
  schaal(Win, Hoogte, _Kolombreedte, _Kolom0, _Row0,_,_,_,_,_), 
  merk(Win, Num, Cat),  %WedsTijd, Tijd),		% geef hem een kleurtje
  %vorige_planning(Win, Gewaarsch, Num, Cat, Rct, Hoogte, RctUitV, TekstIn, TekstUit), %####!!
  RctUitV = rect_Offset(Rct, 0, Hoogte),
  Rct2 = rect_Offset(RctUitV, 0, Hoogte),
  RctHot = rect_Union(Rct, RctUitV),
  %write(".."),
  win_SetBrush(Win,brush(pat_Hollow, color_White)),	%
  win_SetPen(Win,pen(1,ps_Dot,color_Black)),		%
  draw_Rect(Win,RctHot),				%
  draw_TextInRect(Win,RctHot, TekstUit, -1, [dtext_left]),
  win_SetForeColor(Win, color_Black),
  win_SetBackColor(Win, color_White),
  assert(hotspotW(Win, RctHot, SpNo, Dag, Num, Cat, NaamKort, Tijd, nee, b_False, "")) , !,
  write_matches1(Win, SpNo, Dag, Tail, WedsTijd, Rct2, RctUit).
write_matches1(Win, SpNo, Dag, [_|Tail], WedsTijd,Rct, RctUit) :- !,
  write_matches1(Win, SpNo, Dag, Tail, WedsTijd, Rct, RctUit).
write_matches1(_, _, _, _, _, RUit, RUit).

disp_verh1(_Win, _Dag, [], Rct, Rct) :- !.
disp_verh1(Win, Dag, [per(T1,T2)|Rest], Rct, RctUit) :-
  tijd_kwart(D1,U1,M1,T1),
  D1 = Dag,
  U1 = 0,
  M1 = 0,
  tijd_kwart(_,U2,_M2,T2),
  U2 >= 24,
  %M2 = 15,
  schaal(Win, Hoogte, _Kolombreedte, _Kolom0, _Row0,_,_,_,_,_), !,
  %helestr_int(0,MS1,M1),
  %helestr_int(0,MS2,M2),
  format(Tekst,"  hele dag"),
  draw_TextInRect(Win,Rct, Tekst, -1, [dtext_left]),
  Rct1 = rect_Offset(Rct, 0, Hoogte),
  disp_verh1(Win, Dag, Rest, Rct1, RctUit).
disp_verh1(Win, Dag, [per(T1,T2)|Rest], Rct, RctUit) :-
  tijd_kwart(D1,U1,M1,T1),
  D1 = Dag,
  schaal(Win, Hoogte, _Kolombreedte, _Kolom0, _Row0,_,_,_,_,_), !,
  tijd_kwart(_,U2,M2,T2),
  helestr_int(0,MS1,M1),
  helestr_int(0,MS2,M2),
  format(Tekst,"%2.2\046%2.2-%2.2\046%2s",U1,MS1,U2,MS2),
  draw_TextInRect(Win,Rct, Tekst, -1, [dtext_left]),
  Rct1 = rect_Offset(Rct, 0, Hoogte),
  disp_verh1(Win, Dag, Rest, Rct1, RctUit).
disp_verh1(Win, Dag, [_|Rest], Rct, RctUit) :-
  disp_verh1(Win, Dag, Rest, Rct, RctUit).

disp_blok(_Win, _Dag, [], Rct, Rct) :- !.
disp_blok(Win, Dag, [per(T1,T2)|Rest], Rct, RctUit) :-
  tijd_kwart(D1,U1,M1,T1),
  D1 = Dag,
  schaal(Win, Hoogte, _Kolombreedte, _Kolom0, _Row0,_,_,_,_,_), !,
  tijd_kwart(_,U2,M2,T2),
  helestr_int(0,MS1,M1),
  helestr_int(0,MS2,M2),
  format(Tekst,"%2.2\046%2.2-%2.2\046%2s Lft",U1,MS1,U2,MS2),
  draw_TextInRect(Win,Rct, Tekst, -1, [dtext_left]),
  Rct1 = rect_Offset(Rct, 0, Hoogte),
  disp_blok(Win, Dag, Rest, Rct1, RctUit).
disp_blok(Win, Dag, [_|Rest], Rct, RctUit) :-
  disp_blok(Win, Dag, Rest, Rct, RctUit).


prt_dagbegin(_Win, [], _, _, Y, Y, _) :- !.  
prt_dagbegin(Win, [CurrDag|Rest], BegTijd, Rct, _Yin, Yuit, UpdateRct) :-
  schaal(Win, Hoogte, Kolombreedte, _Kolom0, _Row0, _, _, _, _, _), 
  dag(CurrDag,_,w), !,          /* het moet een werkdag zijn */
  tijd_str(BegTijd,BegTijdStr,_),
  draw_TextInRect(Win,Rct, BegTijdStr, -1,[dtext_left]),
  Rct2 = rect_Offset(Rct, Kolombreedte, 0), 
  prt_dagbegin(Win, Rest, BegTijd, Rct2, Hoogte, Yuit, UpdateRct).
prt_dagbegin(Win, [_|Rest], BegTijd, Rct, Yin, Yuit, UpdateRct) :-	% vrijedag
  schaal(Win, _Hoogte, Kolombreedte, _Kolom0, _Row0, _, _, _, _, _),  !,
  Rct2 = rect_Offset(Rct, Kolombreedte, 0), 
  prt_dagbegin(Win, Rest, BegTijd, Rct2, Yin, Yuit, UpdateRct).

win_titel(_Win, No, Cat, Tijd) :-
  wd(No,Cat,A,B,_,_,_),
  tijd_kwart(Dag,_,_,Tijd),
  dag(Dag,DStr,_),
  tijd_str(Tijd,Str1,_),
  betaal_adm(BetSig),
  baan_adm(IBaan),
  repc(ja,'}',IBaan,nee,BetSig,0,1,2,No,Cat,A,alleplanning, Naam,_Score,_TijdLog,_,_,_,_),
  repc(ja,'}',IBaan,nee,BetSig,1,1,2,No,Cat,B,alleplanning, Naam1,_Score1,_TijdLog1,_,_,_,_),
  wc(Cat, _, _, _, _, CatNam, _, _, _, _, _, _, _, _, _),
  %wc(Cat,_,_,CatNam,_,_,_),
  format(Name, "Overzicht % %s%s %----%", CatNam, DStr, Str1, Naam, Naam1),
  win_SetText(_Win, Name), !.
win_titel(_Win, No, Cat, _Tijd) :-
  wd(No,Cat,A,B,_,_,_),
  betaal_adm(BetSig),
  baan_adm(IBaan),
  repc(ja,'}',IBaan,nee,BetSig,0,1,2,No,Cat,A,alleplanning, Naam,_Score,_TijdLog,_,_,_,_),
  repc(ja,'}',IBaan,nee,BetSig,1,1,2,No,Cat,B,alleplanning, Naam1,_Score1,_TijdLog1,_,_,_,_),
  wc(Cat, _, _, _, _, CatNam, _, _, _, _, _, _, _, _, _),
  %wc(Cat,_,_,CatNam,_,_,_),
  format(Name, "Overzicht % %s%s %----%", CatNam, "", "??", Naam, Naam1),
  win_SetText(_Win, Name), !.
win_titel(_Win, _No, _Cat, _Tijd) :-
  win_SetText(_Win, "???"), !.

zelfdeWeds(Num, Cat, Num, Cat, b_False) :- !.
zelfdeWeds(_,_,_,_, b_True).


merkBackground(Win, Num, Cat) :-
  displayS(_Win, Num, Cat, _WedsTijd, _),
  win_SetBrush(Win,brush(pat_Solid, color_Blue)),
  win_SetBackColor(Win, color_Blue),
  win_SetForeColor(Win, color_White), !.
merkBackground(Win, _, _) :-
  win_SetBrush(Win,brush(pat_Solid, color_Magenta)),
  win_SetBackColor(Win, color_Magenta),
  win_SetForeColor(Win, color_White), !.

merk(Win, Num, Cat) :- %, Tijd) :-
  displayS(_Win, Num, Cat, _WedsTijd, _),
  win_SetForeColor(Win, color_Blue), !.
merk(Win, _, _) :-
  win_SetForeColor(Win, color_Magenta), !.
%merk(_, _, _).

write_hotspot(_Win, b_True, SpNo, Num, Cat) :-  % wedstrijd
  hotspotW(_Win, RCT, SpNo, Dag, Num, Cat, NaamKort, Tijd, _, _, _),
  ws(Num, Cat, _Tgnst, Tijd),
  not(w2(Num,Cat,Tijd,_,_,_,_,_)),
  %displayS(_Win, _No, _Cat, _WedsTijd, _),
  tijd_kwart(D1,U1,M1,Tijd),
  D1 = Dag,
  helestr_int(0,MS1,M1),
  format(Tekst,"afgelast:\n  %7.7s %2.2\046%2s ?",NaamKort,U1,MS1),
  win_SetForeColor(_Win, color_White),
  merkBackground(_Win, Num, Cat),  %WedsTijd, Tijd),
  win_SetBackMode(_Win, bk_Transparent),
  draw_Rect(_Win,RCT),
  draw_TextInRect(_Win, RCT, Tekst, -1, [dtext_left]),
  win_SetPen(_Win, pen(1, ps_Solid, color_Black)),
  win_SetForeColor(_Win, color_Black),
  win_SetBackColor(_Win, color_White), !.
write_hotspot(_Win, b_True, SpNo, Num, Cat) :-  % wedstrijd
  hotspotW(_Win, RCT, SpNo, Dag, Num, Cat, Type, Tijd, Gewaarsch, BW, BaanPark),
  schaal(_Win, Hoogte, _Kolombreedte, _Kolom0, _Row0, _, _, _, _, _), 
  %displayS(_Win, _No, _Cat, _WedsTijd, _),
  matchTekst(Dag, Type, Tijd, Gewaarsch, BW, BaanPark, Tekst),
  win_SetForeColor(_Win, color_White),
  merkBackground(_Win, Num, Cat), %WedsTijd, Tijd),
  win_SetBackMode(_Win, bk_Transparent),
  draw_Rect(_Win,RCT),
  vorige_planning(_Win, Gewaarsch, Num, Cat, RCT, Hoogte, _, Tekst, TekstUit), 
  draw_TextInRect(_Win, RCT, TekstUit, -1, [dtext_left]),
  win_SetPen(_Win, pen(1, ps_Solid, color_Black)),
  win_SetForeColor(_Win, color_Black),
  win_SetBackColor(_Win, color_White), !.
write_hotspot(_Win, b_False, SKeus, _, _) :-	% speler
  hotspotS(_Win, RCT, SKeus),
  sp2(SKeus,_,Naam,_Telthuis,_,_,_,Betaald,_,_,_,_,_,_,_,_,_Telwerk, _Telmobiel,_,_,_,_,_,_,_,_,_),
  betaal_adm(JaNee),
  betalenGevraagd(JaNee, Betaald, _Boeken, Teken),
  format(Tekst, "%s%s\n%s",Teken,Naam,""),  	% was tel
  win_SetPen(_Win, pen(1, ps_Dot, color_Black)),
  win_SetForeColor(_Win, color_White),
  win_SetBackColor(_Win, color_Black),
  win_SetBackMode(_Win, bk_Opaque),
  win_SetBrush(_Win,brush(pat_Solid, color_Black)),
  draw_Rect(_Win,RCT),
  draw_TextInRect(_Win, RCT, Tekst, -1, [dtext_left]),
  win_SetBackMode(_Win, bk_Transparent),
  win_SetPen(_Win, pen(1, ps_Solid, color_Black)),
  win_SetForeColor(_Win, color_Black),
  win_SetBackColor(_Win, color_White), !.
write_hotspot(_Win, _, _SKeus, _, _). 

existCursor(_Win) :-
  cursorAt(_Win, c_wedstrijd, SpNo1, Num1, Cat1),
  hotspotW(_Win, _RCT, SpNo1, _, Num1, Cat1, _Type, _Tijd, _Gewaarsch, _BW, _BaanPark), !.
existCursor(_Win) :-
  cursorAt(_Win, c_speler, SpNo1, _Num1, _Cat1),
  hotspotS(_Win, _RCT, SpNo1), !.
existCursor(_Win) :-
  getbacktrack(Here),
  displayS(_Win, Num, Cat, _WedsTijd, _),
  %hotspotS(_Win, _RCT, SKeus),
  hotspotW(_Win, _RCT, SKeus, _, Num, Cat, _Type, _Tijd, _Gewaarsch, _BW, _BaanPark),
  cutbacktrack(Here),
  retractall(cursorAt(_Win, _, _, _,_)),
  assert(cursorAt(_Win, c_wedstrijd, SKeus, Num, Cat)),
  !.

handleOldCursor(_Win, WorS, SpNo, Num, Cat):- 
  cursorAt(_Win, WorS, SpNo, Num, Cat), !,
  fail.		% gelijk gebleven
handleOldCursor(_Win, WorS, SpNo, Num, Cat):- 
  cursorAt(_Win, c_wedstrijd, SpNo1, Num1, Cat1),
  hotspotW(_Win, RCT, SpNo1, _, Num1, Cat1, _Type, _Tijd, _Gewaarsch, _BW, _BaanPark),
  retract(cursorAt(_Win, _,_, _, _)),
  assert(cursorAt(_Win, WorS, SpNo, Num, Cat)), !,
  win_Invalidate(_Win, RCT), !.
handleOldCursor(_Win, WorS, SpNo, Num, Cat):- 
  cursorAt(_Win, c_speler, SpNo1, _Num1, _Cat1),
  hotspotS(_Win, RCT, SpNo1),
  retract(cursorAt(_Win, _,_, _, _)),
  assert(cursorAt(_Win, WorS, SpNo, Num, Cat)), !,
  win_Invalidate(_Win, RCT).

betalenGevraagd(nee, _, b_False, "") :- !.	
betalenGevraagd(ja, ja, b_False, "") :- !.
betalenGevraagd(_, _, b_True   ,"$ ").		% pop-up enabled

waarschuwenGevraagd(ja, SpNo, T1, _, nee, _, b_True) :- 
  sp2t(SpNo, T1),  !.
waarschuwenGevraagd(ja, SpNo, _, T2, _, nee, b_True) :- 
  sp2t(SpNo, T2),  !.
waarschuwenGevraagd(_, _, _, _, _, _, b_False) :- !.

maxBaanText(Win, _) :-
  assert(kolomSize(0)),
  baan_adm(ja),
  findlokaties(Locaties),
  %findall(Loc,  w2(_,_Cat,_Tijd,_J1,_J2,_Geforceerd,_,Loc), LocList),
  %sortstring_c(LocList, 1),
  %removeDublenBlank(LocList, [], Locaties),
  member(Loc, Locaties),
  win_GetTextExtent(Win, Loc, -1, Width, _Height),
  kolomSize(X),
  Width > X,
  assert(kolomSize(Width)),
  fail.
maxBaanText(_, Max) :-
  kolomSize(Max0), 
  Max = Max0 + 3, !.

situatie_winit(Win, FontSize) :-
  baan_adm(ja),
  retract(schaal(Win, _, _, _, _,Marge, _, _Hor, _Vert, _Aantal)),
  Font = font_Create(ff_Helvetica, [], FontSize),
  win_SetFont(Win, Font),
  maxBaanText(Win, BaanTextSize),
  %concat("123456789012345", Tekst, TotTekst),
  win_GetTextExtent(Win,"1234567890123451234", -1, Kolombreedte0, Elementhoogte),
  KolomBreedte = KolomBreedte0 + BaanTextSize,
  E1 = Elementhoogte + 2,
  win_GetTextExtent(Win,"HDC2V za 20.12 ? ", -1, Kolom0, Row0),
 %win_GetTextExtent(Win,"12345678901234567", -1, Kolom0, Row0),
  R1 = Row0 + 2,
  assert(schaal(Win, E1, Kolombreedte, Kolom0, R1,Marge, FontSize, _Hor, _Vert, _Aantal)), !.
situatie_winit(Win, FontSize) :-
  retract(schaal(Win, _, _, _, _,Marge, _, _Hor, _Vert, _Aantal)),
  Font = font_Create(ff_Helvetica, [], FontSize),
  win_SetFont(Win, Font),
  win_GetTextExtent(Win,"1234567890123451234", -1, Kolombreedte, Elementhoogte),
  E1 = Elementhoogte + 2,
  win_GetTextExtent(Win,"123456789012345671234", -1, Kolom0, Row0),
  R1 = Row0 + 2,
  assert(schaal(Win, E1, Kolombreedte, Kolom0, R1,Marge, FontSize, _Hor, _Vert, _Aantal)), !.

linksBoven(L, T) :-
  vensterRCT(situatievenster, RCT) ,
  RCT = rct(L, T, _, _), !.
linksBoven(L, T) :-
  TaskWin = vpi_GetTaskWin(),
  TaskRct = win_GetClientRect(TaskWin),
  Wsflags = win_GetState(TaskWin),
  ClientRct = rect_GetClient(WsFlags, b_False, TaskRct), 
  ClientRct = rct(L, T, _, _).

initXYsize(Win, 0, YSize, XStop) :-
  retract(displayS(Win, No, Cat, WedsTijd, _)),
  assert(displayS(Win, No, Cat, WedsTijd, YSize)),
  linksBoven(Lc, Tc),
  TaskWin = vpi_GetTaskWin(),
  %toolbar_GetRect(TaskWin,tb_Left,TbRect),
  toolbar_GetRect(TaskWin, tb_bottom, TbbRect),
  TbbRect = rct(_, B, R, _BBB),
  %TbRect = rct(_, _T, L, _BB),
  L = 1,
  XstopX = Lc + XStop + 5,
  YVol = TC +  YSize - 12,
  R1 = R - L - 15, %- Lc, % - 8,	% 9.11.2010 ook x-size!
  kleinste(XStopX, R1, Rxx),
  B1 = B - 8,	% 9.11.2010 was 8
  kleinste(YVol, B1, Bxx),
  LC2 = LC + 2,
  TC2 = TC + 2,
  NewRect = rct(LC2, TC2, Rxx, Bxx), 
  win_Move(Win, NewRect),
  !.
initXYsize(_, _, _, _).


update_situatie() :-
  displayS(SWin, _No, _Cat, _, _),
  win_Invalidate(SWin), !.
update_situatie().


spelersInCat(Cat, SpNum, Spelers) :-
  retractall(algehad(_)),
  wd(No, Cat, T2, T1, _Uitsl, _, _),
  not(w3(No, Cat,_,_,_,_,_)),
  member(Tx, [T2, T1]),
  tgnstNum(Tx, SpNum),
  not(member(SpNum, Spelers)),
  not(algehad(SpNum)),
  assert(algehad(SpNum)).
spelersInCat(_Cat, _Sp, _Spelers) :-
  retractall(algehad(_)),
  fail.

uitbreiding(Cat, Spelers, Spelers) :-
  not(pos(Cat, _, _, poel)), !.
uitbreiding(Cat, Spelers, SpelersMet) :-
  findall(Sp, spelersInCat(Cat, Sp, Spelers), ExtraSpelers),
  append(Spelers, ExtraSpelers, SpelersMet). 

notaWin(Win, No, Cat, No, Cat) :-
  win_wedstrijdopmerkingen_Create(Win,No,Cat), !.
notaWin(_, _No, _Cat, _, _) :-
  opmerkingenWin(OpmW, _, _, _No1, _Cat1), !,
  win_Destroy(OpmW).
notaWin(_, _No, _Cat, _, _).


%adaptS(Win, ClRct, TaskRct, _Horz, Vert) :-
%adapt(_Win, rct(_, _, _X, _Y), rct(_,_,_Tx,_Ty), _Horz, _Vert, _).
%_______________________________________________________________________________________
win_situatie_window_Create(_Parent, WedsNo, Cat, Tijd):-
	NulWin = cast(WINDOW, 0),
	assert(displayS(NulWin, WedsNo, Cat, Tijd, 0)),
	win_Create(win_situatie_window_WinType,win_situatie_window_RCT,win_situatie_window_Title,
		   win_situatie_window_Menu,_Parent,win_situatie_window_Flags,win_situatie_window_eh,0).

%BEGIN Situatie Window, e_Create
  win_situatie_window_eh(_Win,e_Create(_),0):-!,
%BEGIN Situatie Window, InitControls, 11:08:42-18.2.2001, Code automatically updated!
%END Situatie Window, InitControls
%BEGIN Situatie Window, ToolbarCreate, 11:08:42-18.2.2001, Code automatically updated!
	tb_situatiebar_Create(_Win),
%END Situatie Window, ToolbarCreate
	win_SetIcon(_Win, idi_beeld),
	NulWin = cast(WINDOW, 0),
	retract(displayS(NulWin, No, Cat, Tijd, _)),
	assert(displayS(_Win, No, Cat, Tijd, 0)),
	win_titel(_Win, No, Cat, Tijd),
	retract(horIndex(NulWin, IndxH)),
	assert(horIndex(_Win, IndxH)),
	assert(vertIndx(_Win, 0)),
	planning(No, Cat, _WTijd, Tijd),
	assert(schaal(_Win, 0, 0, 0, 0, 0, 0, 0, 0, 0)),
	getfontSize(FSize),
	situatie_winit(_Win, FSize),
	%win_wedstrijdopmerkingen_Create(_Win,No,Cat),
	%vensterpositie(situatievenster, _Win),
	!.
%END Situatie Window, e_Create
%MARK Situatie Window, new events

%BEGIN Situatie Window, id_Speler_gegevens
  win_situatie_window_eh(_Win,e_Menu(id_Speler_gegevens,_ShiftCtlAlt),0):-!,
	displayS(_Win, No, Cat, _, _),
	wd(No, Cat, Tg1, Tg2, _, _, _),
	de_spelers(No, Cat, Tg1, Tg2, Spelers),
	setSelCrit([],Spelers),
	TaskWin = vpi_GetTaskWin(),
	win_spelersel_Create(TaskWin, 16),
	spelerSelRefresh(),
	!.
%END Situatie Window, id_Speler_gegevens

%BEGIN Situatie Window, id_Speler_wedstrijden
  win_situatie_window_eh(_Win,e_Menu(id_Speler_wedstrijden,_ShiftCtlAlt),0):-!,
	cursorAt(_Win, b_False, SKeus, _Num1, _Cat1),
	sp2(SKeus,_,Naam,_,_,_,_,_,_,_,_,_,_,_,_,_,_, _,_,_,_,_,_,_,_,_,_),
	zoekOp(SKeus, Naam),
	!.
%END Situatie Window, id_Speler_wedstrijden

%BEGIN Situatie Window, e_User
  win_situatie_window_eh(_Win,e_User(upd_toolb,_Ptr),0):-
	toolbar_SetValue(_Win, idts_Popup, ctrl_value(0,1)),
	toolbar_SetValue(_Win, idts_popup, ctrl_value(1,1)),
	fail.
  win_situatie_window_eh(_Win,e_User(upd_toolb,_Ptr),0):-
	toolbar_SetValue(_Win, idts_overzicht, ctrl_value(0,1)),
	cursorAt(_Win, b_True, _SpNo1, Num, Cat),
	beeldBaar(Num, Cat, IBeeld),
	IBeeld = b_True,
	toolbar_SetValue(_Win, idts_overzicht, ctrl_value(1,1)),
	fail.
  win_situatie_window_eh(_Win,e_User(upd_toolb,_Ptr),0):-
	toolbar_SetValue(_Win, id_Speler_wedstrijden, ctrl_value(0,1)),
	cursorAt(_Win, b_False, _SpNo1, _Num1, _Cat1),
	toolbar_SetValue(_Win, id_Speler_wedstrijden, ctrl_value(1,1)),
	fail.
  win_situatie_window_eh(_Win,e_User(upd_toolb,_Ptr),0):-
	toolbar_SetValue(_Win, idr_gewaarschuwd, ctrl_value(0,1)),
	cursorAt(_Win, b_True, SpNo, Num, Cat),
	oproepen(Oproepen),
	w2(Num,Cat,_,Gew1,Gew2,_,_,_),
	wd(Num,Cat,T1,T2,_,_,_),
	waarschuwenGevraagd(Oproepen, SpNo, T1, T2, Gew1, Gew2, PlanW),
	PlanW = b_True,
	toolbar_SetValue(_Win, idr_gewaarschuwd, ctrl_value(1,1)),
	fail.
  win_situatie_window_eh(_Win,e_User(upd_toolb,_Ptr),0):-
	!.
%END Situatie Window, e_User

%BEGIN Situatie Window, idts_help
/*  win_situatie_window_eh(_Win,e_Menu(idts_help,_ShiftCtlAlt),0):-!,
	project_ShowHelpContext("Wedstrijdoverzicht"),
	!. */
%END Situatie Window, idts_help

%BEGIN Situatie Window, idts_popup
  win_situatie_window_eh(_Win,e_Menu(idts_popup,_ShiftCtlAlt),0):-!,
	win_SendEvent(_Win, e_Char(k_enter, c_Nothing)),
	!.
%END Situatie Window, idts_popup

%BEGIN Situatie Window, idts_overzicht
  win_situatie_window_eh(_Win,e_Menu(idts_overzicht,_ShiftCtlAlt),0):-!,
	cursorAt(_Win, b_True, SpNo1, Num1, Cat1),
	hotspotW(_Win, _RCT, SpNo1, _, Num1, Cat1, _Type, _Tijd, _Gewaarsch, _BW, _BaanPark),
	ParentWin = win_GetParent(_Win),
        situatie(ParentWin, Num1,Cat1,0), 
	!.
%END Situatie Window, idts_overzicht

%BEGIN Situatie Window, idt_volgende
  win_situatie_window_eh(_Win,e_Menu(idt_volgende,_ShiftCtlAlt),0):-!,
	getbacktrack(Here),
	retract(horIndex(_Win, I)),
	cutbacktrack(Here),
	I1 = I + 1,
	assert(horIndex(_Win, I1)),
	win_Invalidate(_Win),
	!.
%END Situatie Window, idt_volgende

%BEGIN Situatie Window, idt_terug
  win_situatie_window_eh(_Win,e_Menu(idt_terug,_ShiftCtlAlt),0):-!,
	horIndex(_Win,I),
	I > 0,	
	getbacktrack(Here),
	retract(horIndex(_Win,_)),
	cutbacktrack(Here),
	I1 = I - 1,
	assert(horIndex(_Win,I1)),
	win_Invalidate(_Win),
	%I1 = 0,
	%win_SetState(_CtrlWin, [wsf_disabled]),
	!.
%END Situatie Window, idt_terug

%BEGIN Situatie Window, idt_op
  win_situatie_window_eh(_Win,e_Menu(idt_op,_ShiftCtlAlt),0):-!,
	vertIndx(_Win, I),
	I > 0, 
	getbacktrack(Here),
	retract(vertIndx(_Win,_)),
	cutbacktrack(Here),
	I1 = I - 1,
	assert(vertIndx(_Win,I1)),
	win_Invalidate(_Win),
	!.
%END Situatie Window, idt_op

%BEGIN Situatie Window, idt_neer
  win_situatie_window_eh(_Win,e_Menu(idt_neer,_ShiftCtlAlt),0):-!,
	getbacktrack(Here),
	retract(vertIndx(_Win,I)),
	cutbacktrack(Here),
	I1 = I + 1,
	assert(vertIndx(_Win, I1)),
	win_Invalidate(_Win),
	!.
%END Situatie Window, idt_neer

%BEGIN Situatie Window, idt_klein
  win_situatie_window_eh(_Win,e_Menu(idt_klein,_ShiftCtlAlt),0):-!,
	schaal(_Win, _, _, _,_, _, OldFont,_,_,_),
	NewFont = OldFont - 1,
	situatie_winit(_Win, NewFont),
	retract(displayS(Win, No, Cat, WedsTijd, _)),
	assert(displayS(Win, No, Cat, WedsTijd, 0)),	% zet vert size weer op nul
	win_Invalidate(_Win),				% dan gaat ie opnieuw uitrekenen
	!.
%END Situatie Window, idt_klein

%BEGIN Situatie Window, idt_groot
  win_situatie_window_eh(_Win,e_Menu(idt_groot,_ShiftCtlAlt),0):-!,
	schaal(_Win, _, _, _,_, _, OldFont,_,_,_),
	NewFont = OldFont + 1,
	situatie_winit(_Win, NewFont),
	retract(displayS(Win, No, Cat, WedsTijd, _)),
	assert(displayS(Win, No, Cat, WedsTijd, 0)),
	win_Invalidate(_Win),
	!.
%END Situatie Window, idt_groot

%BEGIN Situatie Window, e_Char
  win_situatie_window_eh(_Win,e_Char('\27',_ShiftCtlAlt),0):-
	win_Destroy(_Win), !.
  win_situatie_window_eh(_Win,e_Char(k_down,_ShiftCtlAlt),0):-
	cursorAt(_Win, SorW, SKeus, Num, Cat),
	nextHotSpot(_Win, SorW, SKeus, Num, Cat, SorWN, SKeusN, NumN, CatN, RCT),
	handleOldCursor(_Win, SorWN, SKeusN, NumN, CatN), 
	win_Invalidate(_Win, RCT),
	!.
  win_situatie_window_eh(_Win,e_Char(k_up,_ShiftCtlAlt),0):-
	cursorAt(_Win, SorW, SKeus, Num, Cat),
	nextHotSpot(_Win, SorW, SKeus, Num, Cat, SorWN, SKeusN, NumN, CatN, RCT),
	handleOldCursor(_Win, SorWN, SKeusN, NumN, CatN), 
	win_Invalidate(_Win, RCT),
	!.
  win_situatie_window_eh(_Win,e_Char(k_left,_ShiftCtlAlt),0):-
	cursorAt(_Win, SorW, SKeus, Num, Cat),
	nextHotSpot(_Win, SorW, SKeus, Num, Cat, SorWN, SKeusN, NumN, CatN, RCT),
	handleOldCursor(_Win, SorWN, SKeusN, NumN, CatN), 
	win_Invalidate(_Win, RCT),
	!.
  win_situatie_window_eh(_Win,e_Char(k_right,_ShiftCtlAlt),0):-
	cursorAt(_Win, SorW, SKeus, Num, Cat),
	nextHotSpot(_Win, SorW, SKeus, Num, Cat, SorWN, SKeusN, NumN, CatN, RCT),
	handleOldCursor(_Win, SorWN, SKeusN, NumN, CatN), 
	win_Invalidate(_Win, RCT),
	!.
  win_situatie_window_eh(_Win,e_Char(k_enter,_ShiftCtlAlt),0):-
	cursorAt(_Win, b_False, SKeus, _Num1, _Cat1),		% persoon
	%hotspotS(_Win, RCT, SKeus),
	%RCT = rct(L,T,R,B), XX = (L + R) div 2, YY = (T + B) div 2,
	%PNT = pnt(XX, YY),
	sp2(SKeus,_,_,_Telthuis,_,_,_,Betaald,_,_,_,_,_,_,_,_,_Telwerk, _Telmobiel,_,_,_,_,_,_,_,_,_),
	betaal_adm(JaNee),
	betalenGevraagd(JaNee, Betaald, _Boeken,_Teken), !.
/*	modem_enabled(MM, _ComP, _Vorwahl, _Speed, _ToonOfPuls, _Geluid),
	findall(Item, persoonsItems(SKeus, Boeken, MM, Telthuis, Telwerk, Telmobiel, Item), MenuItems),
	Menu = dyn_menu(MenuItems), 
	menu_PopUp(_Win,Menu, PNT, align_Left),
	!. */
  win_situatie_window_eh(_Win,e_Char(k_enter,_ShiftCtlAlt),0):-
	cursorAt(_Win, b_True, SpNo, Num, Cat),			% cursor op wedstijd
	hotspotW(_Win, RCT, SpNo, _Dag, Num, Cat, _Type, _Tijd, _Gewaarsch, _BW, _BaanPark),
	RCT = rct(L,T,R,B), XX = (L + R) div 2, YY = (T + B) div 2,
	PNT = pnt(XX, YY),
	PlanB = b_True,
	displayS(_Win, Nos, Cats, _Tijds, _),
	zelfdeWeds(Num, Cat, NoS, CatS, PlanS),
	oproepen(Oproepen),
	w2(Num,Cat,_,Gew1,Gew2,_,_,_),
	wd(Num,Cat,T1,T2,_,_,_),
	waarschuwenGevraagd(Oproepen, SpNo, T1, T2, Gew1, Gew2, PlanW),
	Menu = dyn_menu([
	txt(idr_gewaarschuwd,"&Gewaarschuwd boeken",0,PlanW,0,[]),
	separator,
	txt(idr_schemaeen,"&Schema",0,PlanB,0,[]),
	txt(id_Wedstrijd_overzicht,"&Wedstrijdoverzicht\tF7",0,PlanS,0,[])
	]), 
	menu_PopUp(_Win,Menu, PNT, align_Right),
	!.
  win_situatie_window_eh(_Win,e_Char('g',_ShiftCtlAlt),0):-!,
	schaal(_Win, _, _, _,_, _, OldFont,_,_,_),
	NewFont = OldFont + 1,
	situatie_winit(_Win, NewFont),
	retract(displayS(Win, No, Cat, WedsTijd, _)),
	assert(displayS(Win, No, Cat, WedsTijd, 0)),
	win_Invalidate(_Win),
	!.
  win_situatie_window_eh(_Win,e_Char('k',_ShiftCtlAlt),0):-!,
	schaal(_Win, _, _, _,_, _, OldFont,_,_,_),
	NewFont = OldFont - 1,
	situatie_winit(_Win, NewFont),
	retract(displayS(Win, No, Cat, WedsTijd, _)),
	assert(displayS(Win, No, Cat, WedsTijd, 0)),	% zet vert size weer op nul
	win_Invalidate(_Win),				% dan gaat ie opnieuw uitrekenen
	!.
%END Situatie Window, e_Char

%BEGIN Situatie Window, e_MouseDown
  win_situatie_window_eh(_Win,e_MouseDown(_PNT,_ShiftCtlAlt,_MBany),0):-
	hotspotW(_Win, RCT, SpNo, _Dag, Num, Cat, _Type, _Tijd, _Gewaarsch, _BW, _BaanPark),
	rect_PntInside(RCT,_PNT),
	handleOldCursor(_Win, b_True, SpNo, Num, Cat), 
	win_Invalidate(_Win, RCT),
	fail.
  win_situatie_window_eh(_Win,e_MouseDown(_PNT,_ShiftCtlAlt,_MBany),0):-
	hotspotS(_Win, RCT, SKeus),
	rect_PntInside(RCT,_PNT),
	handleOldCursor(_Win, b_False, SKeus, 0, 0),
	win_Invalidate(_Win, RCT),
	fail.
  win_situatie_window_eh(_Win,e_MouseDown(_PNT,_ShiftCtlAlt,mouse_button_right),0):-
	hotspotS(_Win, RCT, SpNo),		% speler
	rect_PntInside(RCT,_PNT),
	sp2(SpNo,_,_,_Telthuis,_,_,_,Betaald,_,_,_,_,_,_,_,_,_Telwerk, _Telmobiel,_,_,_,_,_,_,_,_,_),
	betaal_adm(JaNee),
	betalenGevraagd(JaNee, Betaald, _Boeken,_Teken), !.
	%modem_enabled(MM, _ComP, _Vorwahl, _Speed, _ToonOfPuls, _Geluid),
	%findall(Item, persoonsItems(SpNo, Boeken, MM, Telthuis, Telwerk, TelMobiel, Item), MenuItems),
	%Menu = dyn_menu(MenuItems), 
	%menu_PopUp(_Win,Menu, _PNT, align_Left),
	%!.
  win_situatie_window_eh(_Win,e_MouseDown(_PNT,_ShiftCtlAlt,mouse_button_right),0):-
	hotspotW(_Win, RCT, SpNo, _Dag, Num, Cat, _Type, _Tijd, _Gewaarsch, _BW, _BaanPark),
	rect_PntInside(RCT,_PNT),		% wedstrijd
	PlanB = b_True,
	displayS(_Win, Nos, Cats, _Tijds, _),
	zelfdeWeds(Num, Cat, NoS, CatS, PlanS),
	oproepen(Oproepen),
	w2(Num,Cat,_,Gew1,Gew2,_,_,_),
	wd(Num,Cat,T1,T2,_,_,_),
	waarschuwenGevraagd(Oproepen, SpNo, T1, T2, Gew1, Gew2, PlanW),
	Menu = dyn_menu([
	txt(idr_gewaarschuwd,"&Gewaarschuwd boeken",0,PlanW,0,[]),
	separator,
	txt(idr_schemaeen,"&Schema",0,PlanB,0,[]),
	txt(id_Wedstrijd_overzicht,"&Wedstrijdoverzicht\tF7",0,PlanS,0,[])
	]), 
	menu_PopUp(_Win,Menu, _PNT, align_Right),
	!.
  win_situatie_window_eh(_Win,e_MouseDown(_PNT,_ShiftCtlAlt,mouse_button_right),0):-
	hotspotW(_Win, RCT, _SpNo, _Dag, Num, Cat, _Type, _Tijd, _Gewaarsch, _BW, _BaanPark),
	rect_PntInside(RCT,_PNT),		% wedstrijd
	PlanB = b_True,
	displayS(_Win, Nos, Cats, _Tijds, _),
	zelfdeWeds(Num, Cat, NoS, CatS, PlanS),
	%oproepen(Oproepen),
	ws(Num,Cat,_T1,_),
	Menu = dyn_menu([
	  txt(idr_gewaarschuwd,"&Gewaarschuwd boeken",0,1,0,[]),
	  separator,
	  txt(idr_schemaeen,"&Schema",0,PlanB,0,[]),
	  txt(id_Wedstrijd_overzicht,"&Wedstrijdoverzicht\tF7",0,PlanS,0,[])
	]), 
	menu_PopUp(_Win,Menu, _PNT, align_Right),
	!.
%END Situatie Window, e_MouseDown

%BEGIN Situatie Window, e_Destroy
  win_situatie_window_eh(_Win,e_Destroy,0):-
	%retract(winResize(_Win, _, _, _, _, _, _)),
	updateVensterpositie(situatievenster, _Win),
	retractall(displayS(_Win, _, _, _, _)),
	retractall(vertIndx(_Win, _)),
	retract(horIndex(_Win, _)),
	retractall(hotspotS(_Win, _, _)),
	retractall(hotspotW(_Win, _, _, _, _, _, _, _, _, _, _)),
	retractall(cursorAt(_Win, _, _, _, _)),
	retract(schaal(_Win, _, _, _, _, _, FontS, _, _,_)),
	retractall(fontsize(_)),
	assert(fontSize(FontS)),
	!.
%END Situatie Window, e_Destroy

%BEGIN Situatie Window, e_Update
  win_situatie_window_eh(_Win,e_Update(UpdateRct),0):-!,
	Rct1 = rect_Inflate(UpdateRct, 5, 5),
	Rct2 = rect_Offset(Rct1, 5, 5),		% om bij resize niet achter te lopen
	retractall(hotspotS(_Win, _, _)),
	retractall(hotspotW(_Win, _, _, _, _, _, _, _, _, _, _)),
	horIndex(_Win,Index),
	sitRow0(_Win, Index, Rct2),
	schaal(_Win, Hoogte, Kolombreedte, Kolom0, Row0, _Marge, _, _, _,_),
	findall(Dag, dag(Dag, _, _), Dagen),
	staart(0, Index, Dagen, VanafDagen),  
	displayS(_Win, No, Cat, WedsTijd, YSize),
	prt_blok(_Win, Cat, Index, Kolom0, Row0, Yuit, UpdateRct),
	wd(No,Cat,T1,T2,_,_,_), 
	de_spelers(No,Cat,T1,T2,Spelers),
	uitbreiding(Cat, Spelers, SpelersMet), 
	vertIndx(_Win, VI),
	staart(0, VI, SpelersMet, SpelersInd),
	%write("\n."),
	write_spS(_Win, SpelersInd, WedsTijd, Yuit, Yuit1, UpdateRct),	
	count(VanafDagen, 0, AantalDagen),
	XStop = Kolom0 + AantalDagen * Kolombreedte,
	gridLines(_Win, Kolom0, Vanafdagen, Rct2, _, Yuit1), 
	existCursor(_Win),
	cursorAt(_Win, WorP1, SKeus1, Num1, Cat1),
	write_hotspot(_Win, WorP1, SKeus1, Num1, Cat1),
	Yuit2 = Yuit1 + Hoogte + Hoogte + Hoogte,
	initXYsize(_Win, YSize, Yuit2, XStop),
	win_PostEvent(_Win,e_User(upd_toolb, 0)),
	notaWin(_Win, No, Cat, Num1, Cat1),
	!.
%END Situatie Window, e_Update

%BEGIN Situatie Window, e_Size
  win_situatie_window_eh(_Win,e_Size(_Width,_Height),0):-
ifdef use_tbar
	toolbar_Resize(_Win),
enddef
	opmerkingenWin(OpmW, _, _, _No, _Cat),
	win_Invalidate(OpmW),
	!.
	
%END Situatie Window, e_Size

% ____________________________ mijn eigen __________________________________
  win_situatie_window_eh(_Win,e_Menu(c_zoomin,_CAS),0):-
	schaal(_Win, _, _, _,_, _, OldFont,_,_,_),
	NewFont = OldFont + 1,
	situatie_winit(_Win, NewFont),
	retract(displayS(Win, No, Cat, WedsTijd, _)),
	assert(displayS(Win, No, Cat, WedsTijd, 0)),	% zet vert size weer op nul
	win_Invalidate(_Win),				% dan gaat ie opnieuw uitrekenen
	!.
  win_situatie_window_eh(_Win,e_Menu(c_zoomuit,_CAS),0):-
	schaal(_Win, _, _, _, _,_, OldFont,_,_,_),
	NewFont = OldFont - 1,
	situatie_winit(_Win, NewFont),
	retract(displayS(Win, No, Cat, WedsTijd, _)),
	assert(displayS(Win, No, Cat, WedsTijd, 0)),	% zet vert size weer op nul
	win_Invalidate(_Win),				% dan gaat ie opnieuw uitrekenen
	!.
  win_situatie_window_eh(_Win,e_Menu(idr_gegevensperpers,_CAS),0):-
	cursorAt(_Win, b_False, SKeus, _Num1, _Cat1),
  sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Res),
  SpelerIn = sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Res),
        dlg_persoonsgegevens_Create(_Win, SpelerIn),
       	win_Invalidate(_Win), !.
  win_situatie_window_eh(_Win,e_Menu(idr_betalenperpers,_CAS),0):-!,
	cursorAt(_Win, b_False, SKeus, _Num1, _Cat1),
  sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,_Betaald,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Res),
  SpelerIn = sp2(SKeus,Sex,Naam,Telthuis,Verh,RatingE,RatingD,ja,UitKant, Club, Adres, Woonpl, BondsNr, ES, DS, Gebdatum, Telwerk, Telmobiel, EMail, Opm,Gezien,RangPosE,RangPosD,Incasso,CheckGeg,Log,Res),
	logf('Z',SpelerIn, nee), 
	format(Tekst,"%s heeft betaald.",Naam),
	update_uitslagen("situatiewindow"),
	dlg_Note("Boeking",Tekst), !.
/*  win_situatie_window_eh(_Win,e_Menu(idr_opbellen1,_CAS),0):-!,
	modem_enabled(b_True, _ComP, _Vorwahl, _Speed, _ToonOfPuls, _Geluid),
	cursorAt(_Win, b_False, SKeus, _Num1, _Cat1),
	sp2(SKeus,_,_,Telthuis,_,_,_,_,_,_,_,_,_,_,_,_,_Telwerk, _Telmobiel,_,_,_,_,_,_,_,_,_),
	dialer(_Win, Telthuis),
	!.
  win_situatie_window_eh(_Win,e_Menu(idr_opbellen2,_CAS),0):-!,
	modem_enabled(b_True, _ComP, _Vorwahl, _Speed, _ToonOfPuls, _Geluid),
	cursorAt(_Win, b_False, SKeus, _Num1, _Cat1),
	sp2(SKeus,_,_,_Telthuis,_,_,_,_,_,_,_,_,_,_,_,_,Telwerk, _Telmobiel,_,_,_,_,_,_,_,_,_),
	dialer(_Win, Telwerk),
	!.
  win_situatie_window_eh(_Win,e_Menu(idr_opbellen3,_CAS),0):-!,
	modem_enabled(b_True, _ComP, _Vorwahl, _Speed, _ToonOfPuls, _Geluid),
	cursorAt(_Win, b_False, SKeus, _Num1, _Cat1),
	sp2(SKeus,_,_,_Telthuis,_,_,_,_,_,_,_,_,_,_,_,_,_Telwerk, Telmobiel,_,_,_,_,_,_,_,_,_),
	dialer(_Win, TelMobiel),
	!. */
  win_situatie_window_eh(_Win,e_Menu(ItemNr,_CAS),0):-
	cursorAt(_Win, b_False, SKeus, _Num1, _Cat1),
	Wc = ItemNr - 9900,
	wc(WC, _, _, _, _, _, _, _, _, _, _, _, _, _, _),
	%wc(Wc,_,_,_,_,_,_),		% moet bestaan
	pos(Wc, _, _, _),		% moet ingedeeld zijn
	te_plannen(SKeus, WNo, Wc, _),
	select(Wc, WNo),
	!.
  win_situatie_window_eh(_Win,e_Menu(idr_schemaeen,_CAS),0):-!,
	cursorAt(_Win, b_True, SpNo1, Num1, Cat1),
	hotspotW(_Win, _RCT, SpNo1, _, Num1, Cat1, _Type, _Tijd, _Gewaarsch, _BW, _BaanPark),
	select(Cat1,Num1), !.
  win_situatie_window_eh(_Win,e_Menu(id_Wedstrijd_overzicht,_CAS),0):-!,
	cursorAt(_Win, b_True, SpNo1, Num1, Cat1),
	hotspotW(_Win, _RCT, SpNo1, _, Num1, Cat1, _Type, _Tijd, _Gewaarsch, _BW, _BaanPark),
	ParentWin = win_GetParent(_Win),
        situatie(ParentWin, Num1,Cat1,0), !.
  win_situatie_window_eh(_Win,e_Menu(idr_gewaarschuwd,_CAS),0):-
	cursorAt(_Win, b_True, SpNo, Num1, Cat1),
	hotspotW(_Win, _RCT, SpNo, _, Num1, Cat1, _Type, _Tijd, _Gewaarsch, _BW, _BaanPark),
	w2(Num1,Cat1,_Tijd,G1,G2,_A,_B,_C),
	wd(Num1,Cat1,S1,S2,_,_,_),
	gewaarsch(SpNo, S1, S2, G1, G2, _H1, _H2, Tgnst),
	dlg_plangegevens_Create(mijnoproepen, _Win, "Informeren", Tgnst, Num1, Cat1, _Answer, b_False),
	win_Invalidate(_Win),
	update_waarsch(),
	update_uitslagen("tasvpoel"),
	update_display(), !.
	%fail.
%___________________________________________________________________________

%BEGIN Situatie Window, e_Menu, Parent window 
  win_situatie_window_eh(Win,e_Menu(ID,CAS),0):-!,
	PARENT = win_GetParent(Win),
	win_SendEvent(PARENT,e_Menu(ID,CAS)),
	!.
%END Situatie Window, e_Menu, Parent window


%END_WIN Situatie Window
%idr_betalenperpers
%idr_gegevensperpers
%id_Wedstrijd_overzicht
%idr_gewaarschuwd
%idr_schemaeen

%BEGIN_DLG Baannummer
/**************************************************************************
	Creation and event handling for dialog: Baannummer
**************************************************************************/

constants

%BEGIN Baannummer, CreateParms, 10:49:17-18.3.2001, Code automatically updated!
  dlg_baannummer_ResID = idd_baannummer
  dlg_baannummer_DlgType = wd_Modal
  dlg_baannummer_Help = idh_baannummer
%END Baannummer, CreateParms

database - baan
baannr(string Baan, integer Aantal)
single baanWas(integer No, wedscat)
single rowNummer(integer)

predicates

  dlg_baannummer_eh : EHANDLER
  dlg_baannummer_handle_answer(INTEGER EndButton,DIALOG_VAL_LIST,integer,wedscat,string)
  dlg_baannummer_update(DIALOG_VAL_LIST,integer,wedscat,string)
  procedure write_b1(slist, string, string, string)
  procedure removeDublenBlank(slist, slist, slist) - (i, i, o)
  nondeterm w2lok(string)

clauses

baanWas(0, 0).
rowNummer(0).

reg_baannr([H|T]) :-
  fronttoken(H,_,_),		% er staat iets in
  retract(baannr(H, N)),
  N1 = N + 1,
  assert(baannr(H, N1)), !,
  reg_baannr(T).
reg_baannr([H|T]) :-
  fronttoken(H,_,_),		% er staat iets in
  assert(baannr(H, 1)), !,
  reg_baannr(T).
reg_baannr([_|T]) :- !,
  reg_baannr(T).
reg_baannr(_).

write_b(Eigen, Uit1) :-
  findall(B, baannr(B, _), BList),
  sortstring_c(BList, 1),
  write_b1(BList, Eigen, "", Uit),
  not(Uit = ""), 
  concat("In gebruik: ", Uit, Uit1), !.
write_b(_Eigen, "").
  
write_b1([H|T], Eigen, In, Uit) :-
  H <> Eigen, 
  baannr(H, Aantal),
  Aantal > 1, 
  retract(baannr(H, _)), !,
  write_b1(T, Eigen, In, Uit1),
  format(Uit, " %ux%s %s",Aantal, H, Uit1).
write_b1([H|T], Eigen, In, Uit) :-
  H <> Eigen, 
  retract(baannr(H, _)), !,
  write_b1(T, Eigen, In, Uit1),
  format(Uit, " %s %s",H, Uit1).
write_b1([Eigen|T], Eigen, In, Uit) :-
  baannr(Eigen, Aantal),
  A1 = Aantal - 1,
  A1 > 1, 
  retract(baannr(Eigen, _)), !,
  write_b1(T, Eigen, In, Uit1),
  format(Uit, " %ux%s %s",A1, Eigen, Uit1).
write_b1([Eigen|T], Eigen, In, Uit) :-
  retract(baannr(Eigen, Aantal)),
  Aantal = 2, !,
  write_b1(T, Eigen, In, Uit1),
  format(Uit, " %s %s",Eigen, Uit1).
write_b1([_|T], Eigen, In, Uit) :- !,
  write_b1(T, Eigen, In, Uit).
write_b1(_, _, In, In).  

removeDublenBlank([H|T], In, Uit) :-
  str_int(H, _),
  removeDublenBlank(T, In, UIt), !.
removeDublenBlank([H1,H1|T], In, Uit) :-
  removeDublenBlank([H1|T], In, UIt), !.
removeDublenBlank([H|T], In, [H|Uit]) :-
  removeDublenBlank(T, In, Uit), !.
removeDublenBlank(_, In, In).

w2lok(Lok) :-
  w2(_,_Cat,_Tijd,_J1,_J2,_Geforceerd,_,Loc),
  fronttoken(Loc, _, _),
  upper_lower(Lok, Loc).

findlokaties(LokList1) :-
  findall(Lok,  w2lok(Lok), LokList),
  sortstring_c(LokList, 1),
  removeDublenBlank(LokList, [], LokList1).

  dlg_baannummer_Create(Parent, No, Cat):-
	TaskWin = vpi_GetTaskWin(),
	situatie(TaskWin, No, Cat, 0), 
	w2(No,Cat,Tijd,_,_,_,_, IDCB_NR_DEFAULT), !,
	assert(baanWas(No, Cat)),
	findall(Bno, w2(_,_,Tijd,_,_,_,_,Bno), Bnos),
	sortstring_c(Bnos, 1),
	reg_baannr(Bnos),
	write_b(IDCB_NR_DEFAULT, IDCTB_IN_GEBRUIK_TITLE),
	%write_b(Bnos, IDCB_NR_VALUE," al in gebruik.", IDCTB_IN_GEBRUIK_TITLE),
	%IDCB_NR_DEFAULT = "",
	findlokaties(IDCB_NR_ITEMLIST),
	%findall(Loc,  w2(_,_Cat,_Tijd,_J1,_J2,_Geforceerd,_,Loc), LocList),
	%sortstring_c(LocList, 1),
	%removeDublenBlank(LocList, [], IDCB_NR_ITEMLIST),
%MARK Baannummer, new variables

	dialog_CreateModal(Parent,dlg_baannummer_ResID,"",
  		[
%BEGIN Baannummer, ControlList, 10:49:17-18.3.2001, Code automatically updated!
		df(idcb_nr,listedit(IDCB_NR_ITEMLIST,IDCB_NR_DEFAULT),nopr),
		df(idctb_in_gebruik,statictext(IDCTB_IN_GEBRUIK_TITLE),nopr)
%END Baannummer, ControlList
		],
		dlg_baannummer_eh,0,VALLIST,ANSWER),
	dlg_baannummer_handle_answer(ANSWER,VALLIST,No,Cat,IDCB_NR_DEFAULT).

  dlg_baannummer_handle_answer(idc_ok,VALLIST,No,Cat,IDCB_NR_VALUE):-!,
	dlg_baannummer_update(VALLIST,No,Cat,IDCB_NR_VALUE).
  dlg_baannummer_handle_answer(idc_cancel,_,_,_,_):-!.  % Handle Esc and Cancel here
  dlg_baannummer_handle_answer(_,_,_,_,_):-
	errorexit().

  dlg_baannummer_update(_VALLIST,_No,_Cat,_OldValue):-
%BEGIN Baannummer, Update controls, 10:49:17-18.3.2001, Code automatically updated!
	_IDCB_NR_DEFAULT = dialog_VLGetListEdit(idcb_nr,_VALLIST),
%END Baannummer, Update controls
	true, !.
  dlg_baannummer_update(_VALLIST,_No,_Cat,_OldValue).

%MARK Baannummer, new events

%BEGIN Baannummer, idc_ok _CtlInfo
  dlg_baannummer_eh(_Win,e_Control(idc_ok,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	_IDCB_NR_DEFAULT = dialog_GetListEdit(_Win, idcb_nr),
	trim_c(_IDCB_NR_DEFAULT),
	baanWas(No, Cat),
	w2(No,Cat,Tijd,_G1,_G2,Fix,X, BNo), 
	not(BNo = _IDCB_NR_DEFAULT), !,
	logf('Z', w2(No,Cat,Tijd,nee,nee,Fix,X, _IDCB_NR_DEFAULT),nee),
	dlg_vervolgpartij_Create(ja, _Win, No, Cat, "Overzicht geplande wedstrijd",nee,0,_,b_False),
	update_situatie(),
	fail.
%END Baannummer, idc_ok _CtlInfo

%BEGIN Baannummer, e_Destroy
  dlg_baannummer_eh(_Win,e_Destroy,0):-
	updateVensterpositie(locatievenster, _Win),
	!.
%END Baannummer, e_Destroy

%BEGIN Baannummer, e_Create
  dlg_baannummer_eh(_Win,e_Create(_CreationData),0):-
	vensterpositie(locatievenster, _Win),	% als bewaard van de vorige keer
	!.
%END Baannummer, e_Create

%BEGIN Baannummer, idc_help _CtlInfo
  dlg_baannummer_eh(_Win,e_Control(idc_help,_CtrlType,_CtrlWin,_CtlInfo),0):-!,
	project_ShowHelpContext("Baannummer"),
	!.
%END Baannummer, idc_help _CtlInfo

  dlg_baannummer_eh(_,_,_):-!,fail.

%END_DLG Baannummer

% ######### poulehtml

predicates

procedure pouleHTM1(wedscat CatNo, WINDOW Win, integer PlanTM, boolean MetSpeelsterkte, kolomtypel) 
rowElemH(WINDOW, wedscat, tgnst, integer, integer)
diagonaal(integer, integer)
procedure remove_f(string, string)
procedure speelsterkteKol(boolean, string Sterk1, string Bonds1, janee Echt, string Br, string Sterk2, string Bonds2, janee Echt2)
procedure plaatsingHTML1(wedscat, tgnst, string, string ColorEnd, string, string, boolean Hilite) - (i, i, o, o, i, o, i)
procedure extrakols(string Class, kolomtypel, slist, slist, integer N)
procedure vul(string, string)
procedure kolomheaders(integer)
procedure extraKolStyle(kolomtype, string)

clauses

pouleHTM(CatNo, DagTM, MetSpeelsterkte, KolomTypeL) :-
  PlnTM1 = DagTM + 1,
  bitleft(PlnTM1,7,PlanTM),
  EenWin = cast(WINDOW, 1),
  poulestand_init(EenWin, CatNo, _Aantal),  % geplaatsten laatst  
  pouleHTM1(CatNo, EenWin, PlanTM, MetSpeelsterkte, KolomTypeL),
  retractall(partijC(EenWin,_,_,_, _, _), winpoel),
  retractall(lastKol(EenWin, _)).
  %retractall(rank(EenWin,_,_), winpoel),
  %retractall(rankNieuw(EenWin,_,_,_), winpoel).  % ?????
  %retractall(ranking(EenWin,_), winpoel).

plaatsingHTML(Cat, T, "", "", NIn, NUit, Hilite) :-
  geenkleurPrint(), !,
  plaatsingHTML1(Cat, T, _, _, NIn, NUit, Hilite).
plaatsingHTML(Cat, T, Style, ColorEnd, NIn, NUit, Hilite) :-
  plaatsingHTML1(Cat, T, Style, ColorEnd, NIn, NUit, Hilite).

plaatsingHTML1(Cat, T, "", "", Naam, Naam, _) :-
  opg(_, Cat, T, 0,_,_,_,_,_,_,_), !.
plaatsingHTML1(Cat, T, "<span class=geplaatst>", "</span>", Naam, Naam1, b_True) :-
  opg(_, Cat, T, Pl,_,_,_,_,_,_,_), !,
  plaatsText(Pl, Tekst),
  format(Naam1, "%s%s", Naam, Tekst).
plaatsingHTML1(Cat, T, "", "", Naam, Naam1, b_False) :-	% geen hilite
  opg(_, Cat, T, Pl,_,_,_,_,_,_,_), !,
  plaatsText(Pl, Tekst),
  format(Naam1, "%s%s", Naam, Tekst).
plaatsingHTML1(_Cat, _T, "", "", In, In, _).

speelsterkteKol(b_True, _Sterk2, _Bond2, _Echt2, _Br, _Sterk1, _Bond1, _Echt1) :- % NB Omgedraaid!
  writef("<TD class=poulen style=\"width:5\">"),
  fail.
speelsterkteKol(b_True, _Sterk2, _Bond2, nee, _Br, _Sterk1, _Bond1, nee) :- 
  writef("&nbsp;"),
  fail.
speelsterkteKol(b_True, Sterk2, Bond2, Echt2, _Br, _Sterk1, _Bond1, _Echt1) :-
  linkSpelerprofiel(Sterk2, Bond2, Echt2),
  fail.
speelsterkteKol(b_True, _Sterk2, _Bond2, _Echt2, Br, _Sterk1, _Bond1, _Echt) :-
  write(Br),
  fail.
speelsterkteKol(b_True, _Sterk2, _Bond2, _Echt2, _Br, Sterk1, Bond1, Echt1) :-
  linkSpelerprofiel(Sterk1, Bond1, Echt1),
  fail.
speelsterkteKol(b_True, _Sterk2, _, _, _Br, _Sterk1, _, _) :-
  write("</TD>"), !.
speelsterkteKol(_, _Sterk1, _, _, _Br, _Sterk2, _, _).

kolomheaders(0) :- !.
kolomheaders(N) :-
  writef("\n<TD class=pouleo colspan=%u>&nbsp;</TD>",N).

vul(In, In) :-
  fronttoken(In, _, _), !.
vul(_In, "&nbsp;").

extraKolStyle(landcode, "style=\"width:20\"") :- !.
extraKolStyle(_, "").

extrakols(Class, [KolomT|KTRest], [In1|Rest1], [In2|Rest2], N) :- !,
  vul(In1, In11),
  vul(In2, In21),
  extraKolStyle(KolomT, Style),
  writef("\n<TD class=%s %s>%s<BR>%s</TD>", Class, Style, In11, In21), 
  N1 = N - 1,
  extrakols(Class, KTRest, Rest1, Rest2, N1).
extrakols(Class, [KolomT|KTRest],  [], [In2|Rest2], N) :- !,
  extraKolStyle(KolomT, Style),
  vul(In2, In21),
  writef("\n<TD class=%s %s>%s</TD>", Class, Style, In21), 
  N1 = N - 1,
  extrakols(Class, KTRest, [], Rest2, N1).
extrakols(Class, [KolomT|KTRest],  [In1|Rest1], [], N) :- !,
  vul(In1, In11),
  extraKolStyle(KolomT, Style),
  writef("\n<TD class=%s %s>%s</TD>", Class, Style, In11), 
  N1 = N - 1,
  extrakols(Class, KTRest, Rest1, [], N1).
extrakols(_Class, _, [], [], N) :-
  N > 0,
  writef("<TD colspan=%u>&nbsp;</TD>",N), !.
extrakols(_Class, _, _, _, _).

pouleHTM1(_CatNo, _Win, _PlanTM, _, KolomTypeL) :-
  count(KolomTypeL, 0, ExtraKols),
  write("\n<TR><TD class=poulehr>&nbsp;</TD>"),		% nummerkolom
  kolomheaders(ExtraKols),
  write("<TD class=pouleo>&nbsp;</TD>"),		% naamkolom
  fail.
pouleHTM1(_CatNo, _Win, _PlanTM, b_True, _) :-	% speelsterkteaanduiding
  write("<TD class=pouleo>&nbsp;</TD>"),
  fail.
pouleHTM1(_CatNo, Win, _PlanTM, _, _KolomTypeL) :-
  partijC(Win, NoRR, _, _, _, _),	% No = kolom nummer
    writef("<TH class=pouleo>%u</TH>", NoRR),
  fail.
pouleHTM1(Cat, Win, PlanTM, SpStAand, KolomTypeL) :-
  write("\n<TH class=pouleo>Pnt</TH></TR>"),
    partijC(Win, NoR, T1, _, _, _),
    repRowH(NoR, Cat, 0, T1, Sterk2, Naam2, Echt2, Kolommen2, Bondsnr2, _Gebdatum2, _Distr2, Sterk1, Naam1, Echt1, Kolommen1, Bondsnr1, _Gebdatum1, _Distr1, Br, KolomTypeL),
    plaatsingHTML(Cat, T1, Kleur, KleurEnd, Naam2, Naam21, b_True),
    writef("\n<TR><TH class=poulenr style=\"width:20\">%</TH>", NoR),
    count(KolomTypeL, 0, NK),
    extraKols("poulen", KolomTypeL, Kolommen1, Kolommen2, NK),
    maakMijnlink(Naam1, Bondsnr1, NaamLinked1),
    maakMijnlink(Naam21, Bondsnr2, NaamLinked21),
    writef("<TD class=poulen>%s%s%s%s%s</TD>", Kleur, NaamLinked1, Br, NaamLinked21,KleurEnd),
    speelsterkteKol(SpStAand, Sterk1, BondsNr1, Echt1, Br, Sterk2, BondsNr2, Echt2),
    rowElemH(Win, Cat, T1, NoR, PlanTM), 
    write("\n</TR>"),	
  fail.
pouleHTM1(_CatNo, _Win, _, _, _).

rowElemH(Win, Cat, T1, Master, PlanTM) :- 
  partijC(Win, No1, T2, _, _, _),
  %rankNieuw(Win, T2, _, _No1S),
  diagonaal(Master, No1),
  getbacktrack(Here),
  rep_p(Win, Cat, T1, T2, _Punten, _Str, PlanTM, _, 0, nee, Anker,Klasse,StrHtml,_,_Gespeeld),
  cutbacktrack(Here),
  remove_f(StrHtml, Str1),
  writef("\n<TD class=%s width=\"100\">%s%s<BR></TD>", Klasse, Anker, Str1),
  fail.
rowElemH(Win, _, _, Master, _) :-
  partijC(Win, Master, _T1, [Punten| _], _, _AG),
  Punten >= 0,
  writef("<TH class=pouleh>%d</TH>",Punten), !.
rowElemH(_Win, _, _Master, _, _) :-
  writef("<TH class=pouleh>-</TH>").

remove_f(Str, Out) :-	% geen aanduiding van gefixeerd op web
  frontchar(Str,'f',Out), !,
  trim_c(Out).
remove_f(Str, Str).

diagonaal(Master, Master) :-
  write("\n<TD class=pouled >&bull;&nbsp;&nbsp;&bull;&nbsp;&nbsp;&bull;</TD>"), !, fail.
diagonaal(_, _).
%BEGIN_TLB Situatiebar, 10:56:50-23.8.2011, Code automatically updated!
/**************************************************************************
	Creation of toolbar: Situatiebar
**************************************************************************/

clauses

  tb_situatiebar_Create(_Parent):-
ifdef use_tbar
	toolbar_create(tb_bottom,0xC0C0C0,_Parent,
		[tb_ctrl(idts_popup,pushb,idb_menu,idb_menud,idb_grijs,"Menu;Popup menu ",1,1),
		 tb_ctrl(idts_overzicht,pushb,idb_wedstrijd,idb_wedstrijdd,idb_grijs,"Wedstrijdoverzicht;Wedstrijdoverzicht",1,1),
		 tb_ctrl(id_Speler_gegevens,pushb,idb_spelersw,idb_spelerswd,idb_grijs,"Spelers in lijst;Lijstje met spelersegevens op",1,1),
		 tb_ctrl(id_Speler_wedstrijden,pushb,idb_zoekopw,idb_zoekopp,idb_grijs,"Zoek de speler;Wijs de speler aan in een schema",1,1),
		 tb_ctrl(idr_gewaarschuwd,pushb,idb_waarsw,idb_waarswd,idb_grijs,"Gewaarschuwd;Boek als gewaarschuwd",1,1),
		 tb_text(idt_14,tb_static,96,0,3,9,0x0," bw = bij winst"),
		 tb_ctrl(idt_terug,pushb,idb_terugw,idb_terugd,idb_grijs,"Vorige dag;Verplaats kalender",1,1),
		 tb_ctrl(idt_op,pushb,idb_ow,idb_opd,idb_grijs,"Schuif omlaag;Schuif het overzicht naar beneden",1,1),
		 tb_ctrl(idt_neer,pushb,idb_neerw,idb_neerp,idb_grijs,"Schuif omhoog;Schuif het overzicht naar boven",1,1),
		 tb_ctrl(idt_volgende,pushb,idb_vooruitw,idb_vooruitwd,idb_grijs,"Volgende dag;Verplaats kalender",1,1),
		 tb_ctrl(idt_groot,pushb,idb_grootw,idb_grootp,idb_grijs,"Inzoomen; Vergroot het overzicht (g)",1,1),
		 tb_ctrl(idt_klein,pushb,idb_kleinw,idb_kleinp,idb_grijs,"Uitzoomen;Verklein het overzicht (k)",1,1)]),
enddef
	true.
%END_TLB Situatiebar


%BEGIN_WIN Wedstrijdopmerkingen
/**************************************************************************
        Creation and event handling for window: Wedstrijdopmerkingen
**************************************************************************/

constants
%BEGIN Wedstrijdopmerkingen, CreateParms, 10:14:23-19.11.2010, Code automatically updated!
  win_wedstrijdopmerkingen_WinType = w_Child
  win_wedstrijdopmerkingen_Flags = [wsf_Border,wsf_Maximize,wsf_Minimize,wsf_ClipSiblings,wsf_ClipChildren,wsf_TopMost,wsf_TitleBar]
  win_wedstrijdopmerkingen_RCT = rct(100,80,300,140)
  win_wedstrijdopmerkingen_Menu = no_menu
  win_wedstrijdopmerkingen_Title = "Wedstrijdopmerkingen"
  win_wedstrijdopmerkingen_Help = idh_adresetiketten
%END Wedstrijdopmerkingen, CreateParms


predicates

  win_wedstrijdopmerkingen_eh : EHANDLER

clauses
  useeditcontrol(Control, _Value) :-	
    retract(editcontroltext(Control, Nota, Wrap, FontF,Readonly,Size,Usage)), !,
    U1 = Usage + 1,
    assert(editcontroltext(Control, Nota, Wrap, FontF,Readonly,Size,U1)).
  useeditcontrol(Control, Value) :-	
    assert(editcontroltext(Control, Value, b_True, ff_Helvetica,b_False,11,1)).

  win_wedstrijdopmerkingen_Create(_Parent,No,Cat):-
  	opmerkingenWin(_Win,_,_,No,Cat), !.
  win_wedstrijdopmerkingen_Create(_Parent,No,Cat):-
	wd(No, Cat, _, _, _, _, Nota), !,
	retractall(opmerkingenWin(_,_,_,_,_)),
	assert(opmerkingenWin(~0,0,0, No, Cat)),
	useeditcontrol(idc_planning, Nota),
        retractall(editcontroltext(idc_planning, _, _, _, _, _,  _)), 
        asserta(editcontroltext(idc_planning, Nota, b_True, ff_Helvetica,b_False,11,1)),
	win_Create(win_wedstrijdopmerkingen_WinType,win_wedstrijdopmerkingen_RCT,win_wedstrijdopmerkingen_Title,
		   win_wedstrijdopmerkingen_Menu,_Parent,win_wedstrijdopmerkingen_Flags,win_wedstrijdopmerkingen_eh,0).
  win_wedstrijdopmerkingen_Create(_,_,_).

%BEGIN Wedstrijdopmerkingen, e_Create
  win_wedstrijdopmerkingen_eh(_Win,e_Create(_),0):-!,
%BEGIN Wedstrijdopmerkingen, InitControls, 10:14:23-19.11.2010, Code automatically updated!
	win_CreateDynControl([customctl(wdef(wc_Custom,rct(16,14,104,44),"planning",u_Pixels),"pdcedit",idc_planning,[wsf_Group,wsf_TabStop])],_Win),
%END Wedstrijdopmerkingen, InitControls
%BEGIN Wedstrijdopmerkingen, ToolbarCreate, 10:14:23-19.11.2010, Code automatically updated!
%END Wedstrijdopmerkingen, ToolbarCreate
	Font = font_Create(ff_Helvetica, [], 9),
	win_SetFont(_Win, Font),
	retract(opmerkingenWin(~0,W,H,No,Cat)),
	assert(opmerkingenWin(_Win,W,H,No,Cat)),
	!.
%END Wedstrijdopmerkingen, e_Create
%MARK Wedstrijdopmerkingen, new events

%BEGIN Wedstrijdopmerkingen, e_Destroy
  win_wedstrijdopmerkingen_eh(_Win,e_Destroy,0):-
	retract(opmerkingenWin(_Win, _W, _H, No, Cat)),
	EWin = win_GetCtlHandle(_Win, idc_planning),
	TextN = edit_getText(EWin),
	b_True = edit_IsModified(EWin),
	trim_c(TextN),
	%fronttoken(TextN, _, _),
	wd(No, Cat, Tg1, Tg2, U, S, _Nota),
	%Nota <> TextN, 
	logf('A', wd(No, Cat, Tg1, Tg2, U, S, TextN), nee),
	update_display(),
	!.
%END Wedstrijdopmerkingen, e_Destroy

%BEGIN Wedstrijdopmerkingen, e_Update
  win_wedstrijdopmerkingen_eh(_Win,e_Update(_UpdateRct),0):-
	%opmerkingenWin(_Win, W, H,_No, _Cat),
	%W > 0,
	ParentWin = win_GetParent(_Win),
	toolbar_getRect(ParentWin,tb_bottom,Rect),
	Rect = rct(_, TT, _, TB),
	Prct = win_GetClientRect( ParentWin ),
	Prct = rct(_, _, W, H),
	L = W - 288,
	T = H - 73 - TB + TT,
	W1 = W - 3,
	H1 = H - 3 - TB + TT,
	win_Move(_Win,rct(L, T, W1, H1)),
	%Prct = win_GetClientRect( _Win ),
	WP = win_GetCtlHandle(_Win, idc_planning),  % nota
	PN = win_getClientRect(_Win),
	win_move(WP, PN),
	win_BringToTop(_Win),
	!.
%END Wedstrijdopmerkingen, e_Update


%BEGIN Wedstrijdopmerkingen, e_Menu, Parent window 
  win_wedstrijdopmerkingen_eh(Win,e_Menu(ID,CAS),0):-!,
	PARENT = win_GetParent(Win),
	win_SendEvent(PARENT,e_Menu(ID,CAS)),
	!.
%END Wedstrijdopmerkingen, e_Menu, Parent window

%END_WIN Wedstrijdopmerkingen

