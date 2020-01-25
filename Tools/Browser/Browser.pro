/*****************************************************************************

		Copyright (c)2000 Prolog Development Center A/S

 Project:	Web Browser tool
 FileName:	Browser.pro
 Purpose:	Implementation of Web Browser custom control
 Written by:	Alexander Butovsky
 Date:		25.08.2000
 Tested with:	Visual Prolog 5.2 build 594
 Comments:


Disclaimer!

This is an illustrative example with weight on principles,
rather than an attempt to create a robust application.
In a real application error handling should be much more elaborate.

******************************************************************************/
% ----------------------------------------------------------------------------%
% Datatypes declaration of external predicates imported from layer WBCtrl.dll %
%           PBr - pointer to Web Browser object                               %
% ----------------------------------------------------------------------------%

domains
  ie5_Init = procedure ulong /*PBr*/ (window Parent,rct ClientRect,integer Id) - (i,i,i) language c
  ie5_GetWin = procedure window (ulong PBr) - (i) language c
  ie5_Close = procedure (ulong PBr) - (i) language c
  ie5_OpenURL = procedure (ulong PBr,string URL) - (i,i) language c
  ie5_Back = procedure (ulong PBr) - (i)  language c
  ie5_Stop = procedure (ulong PBr) - (i)  language c
  ie5_Forward = procedure (ulong PBr) - (i)  language c
  ie5_Reload = procedure (ulong PBr) - (i)  language c
  ie5_Home = procedure (ulong PBr) - (i) language c
  ie5_GetLocationURL = procedure string /*URL*/ (ulong PBr) - (i) language c
  ie5_GetLocationName = procedure string /*Name*/ (ulong PBr) - (i) language c
  ie5_GetPath = procedure string /*Path*/ (ulong PBr) - (i) language c
  ie5_GetFullName = procedure string /*FullName*/ (ulong PBr) - (i) language c
  ie5_Print = procedure (ulong PBr, integer Cmd) - (i,i) language c

database - browser_internal_info
  single nocopy dll_handle(long DllHandle)
  browser_win(window CustomWin,window BrowserWin,ulong PBr)

clauses
  dll_handle(0).  % single fact initialization


/*********************************************************************************
  Small utility predicate
*********************************************************************************/
predicates
  determ get_object_and_dll_handle(window CustomCtrlWin,ulong BrowserObjectPointer,long DllHandle) - (i,o,o)

clauses
  get_object_and_dll_handle(Win,PBr,DllHandle):-
	browser_win(Win,_,PBr),
	!,
	dll_handle(DllHandle),
	not(DllHandle = 0).

/*** GLOBAL ******************************************************************
  browser_Register/0 -  register custom control class and dynamically load Web 
			Browser layer dll

	Returns: b_true  if dll loading successfull
		 b_false if dll loading failed
	Note:    Call to this predicate should be placed in Task window
		 e_Create event handler. This will guarantee that registration
		 will occure only once.
*****************************************************************************/
predicates
  browser_eh : ehandler

clauses
  browser_Register(b_true):-	% Browser has already been registered
	dll_handle(DllHandle),
	not(DllHandle = 0),
	!.
  browser_Register(b_true):-	% Load dll and register browser class
	trap(
	  DllHandle = vpi_LoadDll("WBCtrl.dll"),
	  _,
	  fail),
	!,
	assert(dll_handle(DllHandle)),
	class_Create(web_browser_class_name,browser_eh).
  browser_Register(b_false).

/***  GLOBAL *****************************************************************
  browser_Unregister/0 - unregister custom control class and close Web Browser 
			 layer dll

	Note:    Call to this predicate should be placed in Task window
		 e_Destroy event handler. This will guarantee that 
		 unloading will occure only once.
*****************************************************************************/
  browser_Unregister():-
	dll_handle(DllHandle),
	not(DllHandle = 0),
	!,
	vpi_FreeDll(DllHandle),
	class_Destroy(web_browser_class_name),
	assert(dll_handle(0)).
  browser_Unregister().

/*** GLOBAL ******************************************************************
  browser_OpenURL/2 - display web document in browser

	Arguments: Win     - Web Browser custom control window
		   URL     - String representing the address of the document
		   	     to be shown in URL format.
*****************************************************************************/
  browser_OpenURL(Win,URL):-
	get_object_and_dll_handle(Win,PBr,DllHandle),
	!,
	OpenURLProc = cast(ie5_OpenURL,vpi_GetDllProc(DllHandle,"ie5_OpenURL")),
	OpenURLProc(PBr,URL).
  browser_OpenURL(_,_).

/*** GLOBAL *****************************************************************
  browser_Back/1 - display previous document in the history list

	Arguments: Win - Web Browser custom control window
	Note:      If previous document in the history list doesn't exist
		   predicate does nothing.
*****************************************************************************/
  browser_Back(Win):-
	get_object_and_dll_handle(Win,PBr,DllHandle),
	!,
	BackProc = cast(ie5_Back,vpi_GetDllProc(DllHandle,"ie5_Back")),
  	BackProc(PBr).
  browser_Back(_).

/*** GLOBAL ******************************************************************
  browser_Stop/1 - stops opening a file

	Arguments: Browser - Web Browser custom control window
	Note:      Stops opening a file.
*****************************************************************************/
  browser_Stop(Win):-
	get_object_and_dll_handle(Win,PBr,DllHandle),
	!,
	BackProc = cast(ie5_Stop,vpi_GetDllProc(DllHandle,"ie5_Stop")),
  	BackProc(PBr).
  browser_Stop(_).

/*** GLOBAL ******************************************************************
  browser_Forward/1 - display the next document in the history list

	Arguments: Win - Web Browser custom control window
	Note:      If the next document in the history list doesn't exist
		   predicate does nothing.
*****************************************************************************/
  browser_Forward(Win):-
	get_object_and_dll_handle(Win,PBr,DllHandle),
	!,
	ForwardProc = cast(ie5_Forward,vpi_GetDllProc(DllHandle,"ie5_Forward")),
  	ForwardProc(PBr).
  browser_Forward(_).

/*** GLOBAL ******************************************************************
  browser_Reload/1 - force browser to reload current document

	Arguments: Win - Web Browser custom control window
*****************************************************************************/
  browser_Reload(Win):-
	get_object_and_dll_handle(Win,PBr,DllHandle),
	!,
	ReloadProc = cast(ie5_Reload,vpi_GetDllProc(DllHandle,"ie5_Reload")),
  	ReloadProc(PBr).
  browser_Reload(_).

/*** GLOBAL ******************************************************************
  browser_Home/1 - force browser to display default document

	Arguments: Win - Web Browser custom control window
*****************************************************************************/
  browser_Home(Win):-
	get_object_and_dll_handle(Win,PBr,DllHandle),
	!,
	HomeProc = cast(ie5_Home,vpi_GetDllProc(DllHandle,"ie5_Home")),
  	HomeProc(PBr).
  browser_Home(_).

/*** GLOBAL ******************************************************************
  browser_GetLocationURL/1 - return current web document URL

	Arguments: Browser - Web Browser custom control window
*****************************************************************************/
  browser_GetLocationURL(Win,URL):-
	get_object_and_dll_handle(Win,PBr,DllHandle),
	!,
	GetLocationURLProc = cast(ie5_GetLocationURL,vpi_GetDllProc(DllHandle,"ie5_GetLocationURL")),
  	URL = GetLocationURLProc(PBr).
  browser_GetLocationURL(_,"").

/*** GLOBAL ******************************************************************
  browser_GetLocationName/1 - return current web document Name

	Arguments: Browser - Web Browser custom control window
*****************************************************************************/
  browser_GetLocationName(Win,Name):-
	get_object_and_dll_handle(Win,PBr,DllHandle),
	!,
	GetLocationNameProc = cast(ie5_GetLocationName,vpi_GetDllProc(DllHandle,"ie5_GetLocationName")),
  	Name = GetLocationNameProc(PBr).
  browser_GetLocationName(_,"").

/*** GLOBAL ******************************************************************
  browser_GetPath/1 - return the path to the container application using Web
  		      Browser custom control

	Arguments: Browser - Web Browser custom control window
*****************************************************************************/
  browser_GetPath(Win,Path):-
	get_object_and_dll_handle(Win,PBr,DllHandle),
	!,
	GetPathProc = cast(ie5_GetPath,vpi_GetDllProc(DllHandle,"ie5_GetPath")),
  	Path = GetPathProc(PBr).
  browser_GetPath(_,"").

/*** GLOBAL ******************************************************************
  browser_GetFullName/1 - return the full name (including path) of the contai-
			  ner application using Web Browser custom control.

	Arguments: Browser - Web Browser custom control window
*****************************************************************************/
  browser_GetFullName(Win,Name):-
	get_object_and_dll_handle(Win,PBr,DllHandle),
	!,
	GetFullNameProc = cast(ie5_GetFullName,vpi_GetDllProc(DllHandle,"ie5_GetFullName")),
  	Name = GetFullNameProc(PBr).
  browser_GetFullName(_,"").

/*** GLOBAL ******************************************************************
  browser_Print/1 - prints

	Arguments: Browser - Web Browser custom control window
	Note:      Prints the browser content
*****************************************************************************/
  browser_Print(Win, Cmd):-
	get_object_and_dll_handle(Win,PBr,DllHandle),
	!,
	BackProc = cast(ie5_Print,vpi_GetDllProc(DllHandle,"ie5_Print")),
  	BackProc(PBr, Cmd).
  browser_Print(_, _).

/*****************************************************************************
  Web Browser custom control window event handler
*****************************************************************************/
  browser_eh(Win,e_Create(_),0):-
	CtlId = win_GetCtlId(Win),
	RCT = win_GetClientRect(Win),
	dll_handle(DllHandle),
	not(DllHandle = 0),
	InitProc = cast(ie5_Init,vpi_GetDllProc(DllHandle,"ie5_Init")),
	PBr = InitProc(Win,RCT,CtlId),		% Returns object pointer
	GetWinProc = cast(ie5_GetWin,vpi_GetDllProc(DllHandle,"ie5_GetWin")),
	BrWin = GetWinProc(PBr),		% Retrieve window handle
	assert(browser_win(Win,BrWin,PBr)).	% Store obj pointer and win handle for future use

  browser_eh(Win,e_Destroy(),0):-
	dll_handle(DllHandle),
	not(DllHandle = 0),
	retract(browser_win(Win,_,PBr)),
	!,
	CloseProc = cast(ie5_Close,vpi_GetDllProc(DllHandle,"ie5_Close")),
	CloseProc(PBr).

  browser_eh(Win,e_Size(Width,Height),0):-
	browser_win(Win,BrWin,_),
	!,
	win_Move(BrWin,rct(0,0,Width,Height)).

  browser_eh(_,e_EraseBackground(),0):-
    !.
