/******************************************************************************

		Copyright (c)1996 Prolog Development Center A/S

 Project:	Label Expert
 FileName:	LBPGVIEW.PRO
 Purpose:	Page preview custom control object
 Written by:	Sergey Alexeev
 Build:		007.970917
 Comments:
******************************************************************************/
DATABASE - preview_db
  preview_info(WINDOW GridWin,INTEGER Id,RCT,WINDOW ParentWin,INTEGER FormPage,
	COLOR ColorPage,REAL LeftMarg,REAL TopMarg,REAL HorSpace,REAL VertSpace,
	REAL LabelWidth,REAL LabelHeight,UNSIGNED NoHor,UNSIGNED NoVert,
	BOOLEAN Page_Roll,COLOR BgColor)

CLAUSES
/**GLOBAL PREDICATE*******************************************************
			     PREVIEW_INIT
 *************************************************************************/
  preview_Init(PreviewWin,FormPage,ColorPage,LeftMarg,TopMarg,HorSpace,VertSpace,
	LabelWidth,LabelHeight,NoHor,NoVert,Page_Roll,BgColor):-
	Id = win_GetCtlId(PreviewWin),
	RCT = win_GetOuterRect(PreviewWin),
	ParentWin = win_GetParent(PreviewWin),
	assert(preview_info(PreviewWin,Id,RCT,ParentWin,FormPage,ColorPage,LeftMarg,
	TopMarg,HorSpace,VertSpace,LabelWidth,LabelHeight,NoHor,NoVert,Page_Roll,BgColor)),!.

/**GLOBAL PREDICATE*******************************************************
			     PREVIEW_SET
 *************************************************************************/
  preview_set(PreviewWin,FormPage,ColorPage,LeftMarg,TopMarg,HorSpace,VertSpace,
	LabelWidth,LabelHeight,NoHor,NoVert,Page_Roll):-
	retract(preview_info(PreviewWin,Id,RCT,ParentWin,_,_,_,_,_,_,_,_,_,_,_,BgColor)),
	assert(preview_info(PreviewWin,Id,RCT,ParentWin,FormPage,ColorPage,LeftMarg,
	TopMarg,HorSpace,VertSpace,LabelWidth,LabelHeight,NoHor,NoVert,Page_Roll,BgColor)),
	win_Invalidate(PreviewWin),!.
PREDICATES
  preview_min(REAL,REAL,REAL)
  %preview_min1(INTEGER,INTEGER,INTEGER)
  preview_max(REAL,REAL,REAL)
  preview_max1(INTEGER,INTEGER,INTEGER)
CLAUSES
  preview_min(X1,X2,X1):-
	X1 < X2,!.
  preview_min(_,X2,X2).
  %preview_min1(X1,X2,X1):-
  %	X1 < X2,!.
  %preview_min1(_,X2,X2).
  preview_max(X1,X2,X2):-
  	X1 < X2,!.
  preview_max(X1,_,X1).
  preview_max1(X1,X2,X2):-
	X1 < X2,!.
  preview_max1(X1,_,X1).
PREDICATES
  preview_get_rect(WINDOW,REAL LeftMarg,REAL TopMarg,REAL HorSpace,REAL VertSpace,
	REAL LabelWidth,REAL LabelHeight,UNSIGNED NoHor,UNSIGNED NoVert,
	BOOLEAN Page_Roll,RCT,REAL D,UNSIGNED NoHor1,UNSIGNED NoVert1)
CLAUSES
  preview_get_rect(Win,LeftMarg,TopMarg,HorSpace,VertSpace,LabelWidth,LabelHeight,
	NoHor,NoVert,b_false,Rect,D,NoHor,NoVert2):-
	NoVert = 0,
	Rect = win_GetClientRect(Win),
	Rect = rct(L,T,R,B),
	NH1 = NoHor - 1,
	preview_max1(NH1,0,NH),
	Width0 = LeftMarg * 2 + LabelWidth * NoHor + HorSpace * NH,
	preview_max(Width0,1.0,Width),
	Width1 = R - L,
	D = Width1 / Width,
	NoVert1 = (B - T - TopMarg * D) / ((LabelHeight + VertSpace) * D),
	NoVert2 = val(integer,NoVert1) + 1,!.
  preview_get_rect(Win,LeftMarg,TopMarg,HorSpace,VertSpace,LabelWidth,LabelHeight,
	NoHor,NoVert,b_false,Rect,D,NoHor2,NoVert):-
	NoHor = 0,
	Rect = win_GetClientRect(Win),
	Rect = rct(L,T,R,B),
	NV1 = NoVert - 1,
	preview_max1(NV1,0,NV),
	Height0 = TopMarg * 2 + LabelHeight * NoVert + VertSpace * NV,
	preview_max(Height0,1.0,Height),
	Height1 = B - T,
	D = Height1 / Height,
	NoHor1 = (R - L - LeftMarg * D) / ((LabelWidth + HorSpace) * D),
	NoHor2 = val(integer,NoHor1) + 1,!.
  preview_get_rect(Win,LeftMarg,TopMarg,HorSpace,VertSpace,LabelWidth,LabelHeight,
	NoHor,NoVert,_,OutRect,D,NoHor,NoVert):-
	Rect = win_GetClientRect(Win),
	Rect = rct(L,T,R,B),
	NH1 = NoHor - 1,
	preview_max1(NH1,0,NH),
	Width0 = LeftMarg * 2 + LabelWidth * NoHor + HorSpace * NH,
	preview_max(Width0,1.0,Width),
	NV1 = NoVert - 1,
	preview_max1(NV1,0,NV),
	Height0 = TopMarg * 2 + LabelHeight * NoVert + VertSpace * NV,
	preview_max(Height0,1.0,Height),
	Width1 = R - L, Height1 = B - T,
	DWidth = Width1 / Width, DHeight = Height1 / Height,
	preview_min(DWidth,DHeight,D),
	L1 = cast(integer,R - LeftMarg * 2 * D - LabelWidth * NoHor * D - HorSpace * NH * D),
	T1 = cast(integer,B - TopMarg * 2 * D - LabelHeight * NoVert * D - VertSpace * NV * D),
	OutRect = rct(L1,T1,R,B),!.
PREDICATES
  preview_draw(WINDOW,REAL L,REAL T,INTEGER FormPage,REAL HorSpace,REAL VertSpace,
	REAL LabelWidth,REAL LabelHeight,UNSIGNED NoHor,UNSIGNED NoVert)
  preview_draw1(WINDOW,REAL L,REAL R,REAL T,INTEGER FormPage,REAL VertSpace,
	REAL LabelHeight,UNSIGNED NoVert)
  preview_draw2(WINDOW,RCT,INTEGER FormPage)
CLAUSES
  preview_draw(_,_,_,_,_,_,_,_,0,_):-!.
  preview_draw(Win,L,T,FormPage,HorSpace,VertSpace,LabelWidth,LabelHeight,NoHor,NoVert):-
	R = L + LabelWidth,
	preview_draw1(Win,L,R,T,FormPage,VertSpace,LabelHeight,NoVert),
	L1 = R + HorSpace,
	NoHor1 = NoHor - 1,!,
	preview_draw(Win,L1,T,FormPage,HorSpace,VertSpace,LabelWidth,LabelHeight,NoHor1,NoVert).

  preview_draw1(_,_,_,_,_,_,_,0):-!.
  preview_draw1(Win,L,R,T,FormPage,VertSpace,LabelHeight,NoVert):-
	B = T + LabelHeight,
	LR = val(integer,L), TR = val(integer,T),
	RR = val(integer,R), BR = val(integer,B),
	preview_draw2(Win,rct(LR,TR,RR,BR),FormPage),
	T1 = B + VertSpace,
	NoVert1 = NoVert - 1,!,
	preview_draw1(Win,L,R,T1,FormPage,VertSpace,LabelHeight,NoVert1).

  preview_draw2(Win,Rect,1):-
	draw_Rect(Win,Rect),!.
  preview_draw2(Win,Rect,2):-
	Rect = rct(L,_,R,_),
	RR = (R - L) div 5,
	draw_RoundRect(Win,Rect,RR,RR),!.
  preview_draw2(Win,Rect,3):-
	draw_Ellipse(Win,Rect),!.
/*************************************************************************
			     EVENT HANDLER
 *************************************************************************/
  preview_HandleEvent(Win,e_Destroy):-
	retract(preview_info(Win,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)),!.

  preview_HandleEvent(_,e_EraseBackGround()):-!.

  preview_HandleEvent(Win,e_Update(Rect)):-
	preview_info(Win,_,_,_,FormPage,ColorPage,LeftMarg,
	TopMarg,HorSpace,VertSpace,LabelWidth,LabelHeight,NoHor,NoVert,Page_Roll,BgColor),
	win_Clear(Win,Rect,BgColor),
	preview_get_rect(Win,LeftMarg,TopMarg,HorSpace,VertSpace,LabelWidth,LabelHeight,
	NoHor,NoVert,Page_Roll,rct(L,T,R,B),D,NoHor1,NoVert1),
	win_SetPen(Win,pen(1,ps_Solid,color_Black)),
	win_SetBrush(Win,brush(pat_Solid,color_Yellow)),
	draw_Rect(Win,rct(L,T,R,B)),
	L1 = L + LeftMarg * D, T1 = T + TopMarg * D,
	win_SetPen(Win,pen(1,ps_Solid,color_Black)),
	win_SetBrush(Win,brush(pat_Solid,ColorPage)),
	HorSpace0 = HorSpace * D, VertSpace0 = VertSpace * D,
	LabelWidth0 = LabelWidth * D, LabelHeight0 = LabelHeight * D,
	preview_draw(Win,L1,T1,FormPage,HorSpace0,VertSpace0,
	LabelWidth0,LabelHeight0,NoHor1,NoVert1),!.
