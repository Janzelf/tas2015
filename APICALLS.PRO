/*****************************************************************************

		Copyright (c) 1984 - 2000 Prolog Development Center A/S

 Project:
 FileName: APICALLS.PRO
 Purpose: A Visual Prolog interface to OS API calls
 Written by: Konstantin Ivanov & Compiler Group
 Comments:
******************************************************************************/

ifndef ws_win
  ifdef platform_32bit
    ifndef os_os2
      #error Unsupported platform
    enddef
  elsedef
    #error Unsupported platform
  enddef
enddef

ifndef apicalls_con_included
  #Error apicalls.con must be included
enddef
ifndef apicalls_dom_included
  #Error apicalls.dom must be included
enddef
ifndef apicalls_pre_included
  #Error apicalls.pre must be included
enddef

  constants

    apicalls_pro_included = 1
use_omf_objformat = 1
			/************************/
			/* Names of API calls  */
			/************************/
  constants

    ifdef platform_16bit

      callKind = pascal

    elsedef

      callKind = stdcall

    enddef

    ifdef os_os2

    elsedef

      ifdef platform_32bit

	ifdef use_omf_objformat

	  %kernel32.lib
	  api_GetWindowsDirectoryA_FNS		= "GetWindowsDirectoryA"
	  api_LoadLibraryA_FNS			= "LoadLibraryA"
	  api_FreeLibrary_FNS			= "FreeLibrary"
	  api_GetProcAddress_FNS		= "GetProcAddress"
	  api_GetProfileStringA_FNS		= "GetProfileStringA"
	  api_GetProfileIntA_FNS		= "GetProfileIntA"

	  %advapi32.lib
	  api_RegisterEventSourceA_FNS		= "RegisterEventSourceA"
	  api_DeregisterEventSource_FNS		= "DeregisterEventSource"
	  api_ReportEventA_FNS			= "ReportEventA"
	  api_GetComputerNameA_FNS		= "GetComputerNameA"

	  %advapi32.lib
	  api_RegOpenKeyA_FNS			= "RegOpenKeyA"
	  api_RegCloseKey_FNS			= "RegCloseKey"
	  api_RegEnumKeyA_FNS			= "RegEnumKeyA"
	  api_RegQueryValueA_FNS		= "RegQueryValueA"

	  %shell32.lib
	  api_ShellExecuteA_FNS			= "ShellExecuteA"

        elsedef
	% deze worden gebruikt!
	  api_GetWindowsDirectoryA_FNS		= "_GetWindowsDirectoryA"
	  api_RegisterEventSourceA_FNS		= "_RegisterEventSourceA"
	  api_DeregisterEventSource_FNS		= "_DeregisterEventSource"
	  api_ReportEventA_FNS			= "_ReportEventA"
	  api_LoadLibraryA_FNS			= "_LoadLibraryA"
	  api_FreeLibrary_FNS			= "_FreeLibrary"
	  api_GetProcAddress_FNS		= "_GetProcAddress"
	  api_RegOpenKeyA_FNS			= "_RegOpenKeyA"
	  api_RegCloseKey_FNS			= "_RegCloseKey"
	  api_RegEnumKeyA_FNS			= "_RegEnumKeyA"
	  api_RegQueryValueA_FNS		= "_RegQueryValueA"
	  api_GetProfileStringA_FNS		= "_GetProfileStringA"
	  api_GetProfileIntA_FNS		= "_GetProfileIntA"
	  api_ShellExecuteA_FNS			= "_ShellExecuteA"
	  api_GetComputerNameA_FNS		= "_GetComputerNameA"

        enddef

      enddef

      ifdef platform_16bit

      	  api_AnsiToOem_FNS			= "ANSITOOEM"
      	  api_OemToAnsi_FNS			= "OEMTOANSI"
	  api_LoadLibraryA_FNS			= "LOADLIBRARY"
	  api_FreeLibrary_FNS			= "FREELIBRARY"
	  api_GetProcAddress_FNS		= "GETPROCADDRESS"
	  api_RegOpenKeyA_FNS			= "REGOPENKEY"
	  api_RegCloseKey_FNS			= "REGCLOSEKEY"
	  api_RegEnumKeyA_FNS			= "REGENUMKEY"
	  api_RegQueryValueA_FNS		= "REGQUERYVALUE"
	  api_GetProfileStringA_FNS		= "GETPROFILESTRING"
	  api_GetProfileIntA_FNS		= "GETPROFILEINT"
	  api_ShellExecuteA_FNS			= "SHELLEXECUTE"
	  api_GetWindowsDirectoryA_FNS		= "GETWINDOWSDIRECTORY"

      enddef

    enddef

  ifdef os_nt

    constants

      nt_error_offset = 7000

  enddef

  global predicates

      procedure OS_HANDLE loadLibraryA( STRING )				- (i)		language callKind as api_LoadLibraryA_FNS
      procedure DWORD	getProcAddress( OS_HANDLE, STRING )			- (i,i)		language callKind as api_GetProcAddress_FNS
      procedure LONG	regQueryValueA( HKEY, STRING, STRING, DWORD )		- (i,i,i,i)	language callKind as api_RegQueryValueA_FNS
      procedure LONG	regCloseKey( HKEY )					- (i)		language callKind as api_RegCloseKey_FNS
      procedure LONG	regEnumKeyA( HKEY, DWORD ,STRING, DWORD )		- (i,i,i,i)	language callKind as api_RegEnumKeyA_FNS
      procedure	UNSIGNED getProfileIntA( STRING, STRING, INTEGER )		- (i,i,i)	language callKind as api_GetProfileIntA_FNS
      procedure OS_HANDLE shellExecute( API_HWND, STRING, STRING, STRING,
      					STRING, INTEGER )			- (i,i,i,i,i,i) language callKind as api_ShellExecuteA_FNS
      procedure UNSIGNED getWindowsDirectoryA( STRING, UNSIGNED )		- (i,i)		language callKind as api_GetWindowsDirectoryA_FNS

    ifdef platform_32bit

      procedure BOOLEAN	freeLibrary( OS_HANDLE )				- (i)		language stdcall as api_FreeLibrary_FNS
      procedure LONG	regOpenKeyA( HKEY, STRING, BINARY )			- (i,i,i)	language callKind as api_RegOpenKeyA_FNS
      procedure DWORD	getProfileStringA( STRING, STRING, STRING, STRING, DWORD )- (i,i,i,i,i)	language callKind as api_GetProfileStringA_FNS

    elsedef

      procedure 	freeLibrary( OS_HANDLE )				- (i)		language pascal as api_FreeLibrary_FNS
      procedure LONG	regOpenKeyA( HKEY, STRING, HKEY )			- (i,i,o)	language callKind as api_RegOpenKeyA_FNS
      procedure INTEGER	getProfileStringA( STRING, STRING, STRING, STRING,
      					   INTEGER )				- (i,i,i,i,i)	language callKind as api_GetProfileStringA_FNS

    enddef

    ifdef os_nt

      procedure BOOLEAN	GetUserNameA( STRING, DWORD )				- (i,i) 	language stdcall
      procedure BOOLEAN	GetComputerNameA( STRING, DWORD )			- (i,i) 	language stdcall
      procedure BOOLEAN	GetVersionExA( BINARY )					- (i) 		language stdcall
      procedure BOOLEAN CloseHandle( OS_HANDLE Handle )	 			- (i)		language stdcall
      procedure UNSIGNED GetSystemDirectoryA( STRING, UNSIGNED )		- (i,i)		language stdcall
      procedure DWORD	SuspendThread( OS_HANDLE Thread )			- (i) 		language stdcall
      procedure DWORD 	ResumeThread( OS_HANDLE Thread )			- (i) 		language stdcall
      procedure BOOLEAN	TerminateThread( OS_HANDLE Thread, DWORD ExitCode ) 	- (i,i) 	language stdcall
      procedure INTEGER GetThreadPriority( OS_HANDLE Thread )			- (i) 		language stdcall
      procedure BOOLEAN	SetThreadPriority( OS_HANDLE Thread, INTEGER Priority ) - (i,i) 	language stdcall
      procedure BOOLEAN GetExitCodeThread( OS_HANDLE Thread, DWORD ExitCode ) 	- (i,o) 	language stdcall
      procedure DWORD 	WaitForSingleObject( OS_HANDLE Handle, DWORD Timeout ) 	- (i,i) 	language stdcall
      procedure DWORD 	WaitForMultipleObjects( DWORD Count, BINARY HandlesArray,
      					      BOOLEAN WaitAll,DWORD Timeout ) 	- (i,i,i,i) 	language stdcall
      procedure DWORD	GetShortPathNameA( STRING, STRING, DWORD )		- (i,i,i) 	language stdcall
      procedure		GetSystemTime( BINARY )					- (i) 		language stdcall
      procedure BOOLEAN	GetProcessTimes( OS_HANDLE, BINARY,
      					 BINARY, BINARY, BINARY )		- (i,i,i,i,i) 	language stdcall
      procedure BOOLEAN	GetThreadTimes( OS_HANDLE, BINARY,
      					 BINARY, BINARY, BINARY )		- (i,i,i,i,i) 	language stdcall
      procedure BOOLEAN	FileTimeToLocalFileTime( FILETIME, BINARY )		- (i,i) 	language stdcall
      procedure BOOLEAN	FileTimeToSystemTime( FILETIME, BINARY )		- (i,i) 	language stdcall
      procedure OS_HANDLE RegisterEventSourceA( STRING, STRING )		- (i,i) 	language stdcall
      procedure BOOLEAN DeregisterEventSource( OS_HANDLE )			- (i) 		language stdcall
      procedure BOOLEAN ReportEventA( OS_HANDLE, WORD, WORD, DWORD, DWORD,
      				     DWORD, DWORD, BINARY, BINARY)		- (i,i,i,i,i,i,i,i,i) language stdcall
      procedure OS_HANDLE FindFirstChangeNotificationA( STRING, BOOLEAN, DWORD )- (i,i,i) 	language stdcall
      procedure BOOLEAN FindNextChangeNotification( OS_HANDLE )			- (i) 		language stdcall
      procedure BOOLEAN FindCloseChangeNotification( OS_HANDLE )		- (i) 		language stdcall
      procedure OS_HANDLE GetStdHandle( DWORD )					- (i) 		language stdcall
      procedure 	CharToOemA( STRING Src, STRING Dst )			- (i,i) 	language stdcall
      procedure 	OemToCharA( STRING Src, STRING Dst )			- (i,i) 	language stdcall
      procedure INTEGER MultiByteToWideChar( UNSIGNED, DWORD, STRING, INTEGER,
      					     STRING, INTEGER) 			- (i,i,i,i,i,i) language stdcall
      procedure INTEGER WideCharToMultiByte( UNSIGNED, DWORD, STRING, INTEGER,
      					     STRING, INTEGER, STRING, LONG ) 	- (i,i,i,i,i,i,i,i) language stdcall
      %procedure INTEGER lstrlenW( STRING )					- (i) 		language stdcall

      procedure INTEGER GetNumberFormatA( LCID, DWORD, STRING, DWORD, STRING,
      					 INTEGER )				- (i,i,i,i,i,i)	language stdcall
      procedure INTEGER GetLocaleInfoA( LCID, LCTYPE, STRING, INTEGER )		- (i,i,i,i)	language stdcall
      procedure OS_HANDLE CreateEventA( BINARY, BOOLEAN, BOOLEAN, STRING )	- (i,i,i,i) language stdcall
      procedure BOOLEAN	ResetEvent( OS_HANDLE )					- (i) 	language stdcall
      procedure BOOLEAN	SetEvent( OS_HANDLE )					- (i) 	language stdcall

    enddef

    ifdef platform_16bit

      procedure 	ansiToOem( STRING Src, STRING Dst )			- (i,i) 	language stdcall as api_AnsiToOem_FNS
      procedure 	oemToAnsi( STRING Src, STRING Dst )			- (i,i) 	language stdcall as api_OemToAnsi_FNS

    enddef


  predicates

    procedure check_HInstance( UNSIGNED )				- (i)

    ifdef platform_32bit
      procedure count_some( OS_HANDLE_LIST, DWORD HandlesNumber ) 	- (i,o)
      procedure count_some( SLIST, DWORD StringsNumber )	 	- (i,o)
      procedure fill_SomeArray( OS_HANDLE_LIST,
      				BINARY HandlesArray, DWORD Index )	- (i,i,i)
      procedure fill_SomeArray( SLIST, BINARY StringsArray,
      				DWORD Index )				- (i,i,i)
      procedure check_RC_Invalid( OS_HANDLE Handle ) 			- (i)
      procedure check_RC_Bool( BOOLEAN ) 				- (i)
      procedure check_RC_Null( OS_HANDLE Handle ) 			- (i)
      procedure check_RC_Null( INTEGER NumberOfBytes ) 			- (i)
      procedure check_RC_Null( DWORD FuncHandle ) 			- (i)
      procedure make_filetime_structures( BINARY, BINARY,
      					  BINARY, BINARY ) 		- (o,o,o,o)
      procedure fill_filetime_structures( BINARY, BINARY, BINARY, BINARY,
      					  FILETIME, FILETIME, FILETIME,
      					  FILETIME ) 			- (i,i,i,i,o,o,o,o)
      procedure list_or( DWORD_LIST, DWORD )				- (i,o)

    elsedef

      procedure check_RC_Null16( DWORD Handle ) 			- (i)
      procedure check_RC_Null16( UNSIGNED Handle ) 			- (i)

    enddef

  ifndef pdcrunt_pre_included

    global predicates

      ERRONEOUS run_Error( UNSIGNED Errorno ) 			 - (i) language c as "_RUN_Error"
      PROCEDURE movmem( ULONG Source, ULONG Dest, UNSIGNED Len ) - (i,i,i) language c as "_MEM_MovMem"

  enddef

  clauses

    check_HInstance( RC ):-
    	RC < api_HInstance_error,
	RC <> 31,
    	RC <> 2, !,	% 24.7.2007 !!!!!!
    	write(RC),
    	run_error( RC ).
    check_HInstance( _ ).

  ifdef platform_32bit

    count_some( [], 0 ) :- !.
    count_some( [_|L], Cnt ) :- !,
      	count_some( L, C ),
      	Cnt = C + 1.
    count_some( _, 0 ).    %dummy

    fill_SomeArray( [], _, _ ) :- !.
    fill_SomeArray( [El|L], Array, Index ) :-
    	El_dword = cast( DWORD, El ),
	setdwordentry( Array, Index, El_dword ),
	NewIndex = Index + 1, !,
	fill_SomeArray( L, Array, NewIndex ).
    fill_SomeArray( _, _, _ ).		%dummy

    check_RC_Bool( b_false ) :- !,
	NTLastError = api_GetLastError(),
	LastError = NTLastError + nt_error_offset,
	run_Error( LastError ).
    check_RC_Bool( _ ).

    check_RC_Invalid( 0xFFFFFFFF ) :- !,
	NTLastError = api_GetLastError(),
	LastError = NTLastError + nt_error_offset,
	run_Error( LastError ).
    check_RC_Invalid( _ ).

    check_RC_Null( 0 ) :- !,
	NTLastError = api_GetLastError(),
	LastError = NTLastError + nt_error_offset,
	run_Error( LastError ).
    check_RC_Null( _ ).

    make_filetime_structures( CT, ET, KT, UT ):-
 	FILETIME_size = sizeof( FILETIME ),
	CT = makebinary( FILETIME_size ),
      	ET = makebinary( FILETIME_size ),
      	KT = makebinary( FILETIME_size ),
      	UT = makebinary( FILETIME_size ).

    fill_filetime_structures( CT, ET, KT, UT, filetime(CL,CH), filetime(EL,EH), filetime(KL,KH), filetime(UL,UH) ):-
	CL = getdwordentry( CT, 0 ),
	CH = getdwordentry( CT, 1 ),
	EL = getdwordentry( ET, 0 ),
	EH = getdwordentry( ET, 1 ),
	KL = getdwordentry( KT, 0 ),
	KH = getdwordentry( KT, 1 ),
	UL = getdwordentry( UT, 0 ),
	UH = getdwordentry( UT, 1 ).

   list_or( [], 0 ):-!.
   list_or( [El|L], Result ):-!,
	list_or( L, TmpRes ),
      	bitor( TmpRes, El, Result ).
   list_or( _, 0 ).	%dummy

  elsedef

    check_RC_Null16( 0 ) :- !,
	run_Error( 0 ).
    check_RC_Null16( _ ).

  enddef

  clauses

      api_GetWindowsDirectory( Str ) :-
		Size = 1000,
		str_len( Str, Size ),
		RC = getWindowsDirectoryA( Str, Size ),
		ifdef platform_32bit
		  check_RC_Bool( RC ).
		elsedef
		  check_RC_Null16( RC ).
		enddef


     api_LoadLibrary( Name, Handle ):-
     		Handle = loadLibraryA( Name ),
     		ifdef platform_32bit
     		  check_RC_Null( Handle ).
     		elsedef
     		  check_HInstance( Handle ).
     		enddef

     api_FreeLibrary( Handle ):-
     		ifdef platform_32bit
     		  RC = freeLibrary( Handle ),
     		  check_RC_Bool( RC ).
     		elsedef
     		  freeLibrary( Handle).
     		enddef

     api_GetProcAddress( LibHandle, FuncName, FuncHandle ):-
    		FuncHandle = getProcAddress( LibHandle, FuncName ),
    		ifdef platform_32bit
    		  check_RC_Null( FuncHandle ).
    		elsedef
    		  check_RC_Null16( FuncHandle ).
    		enddef

     api_RegOpenKey( Key, SubKey, Result, RC ):-
     		ifdef platform_32bit
     		  Key_size = sizeof( HKEY ),
     		  Key_bin = makebinary( Key_size ),
     		  RC = regOpenKeyA( Key, SubKey, Key_bin ),
		  Result = getdwordentry( Key_bin, 0 ).
		elsedef
		  RC = regOpenKeyA( Key, SubKey, Result ).
		enddef

     api_RegCloseKey( Key, RC ):-
     		RC = regCloseKey( Key ).

     api_RegEnumKey( Key, Index, SubKey, RC ):-
     		Subkey_size = api_MAX_PATH + 1,
     		str_len( SubKey, Subkey_size ),
     		RC = regEnumKeyA( Key, Index, Subkey, Subkey_size ).

     api_RegQueryValue( Key, Subkey, Value, RC ):-
		str_len( Value, 1000 ),
		Dword_size = sizeof( DWORD ),
		Valuesize_bin = makebinary( Dword_size ),
		setdwordentry( Valuesize_bin, 0, 1000 ),
		Valuesize_ulong = cast( ULONG, Valuesize_bin ),
     		RC = regQueryValueA( Key, Subkey, Value, Valuesize_ulong ).

     api_GetProfileString( AppName, KeyName, Default, Result ):-
     		str_len( Result, 1000 ),
     		_NumberOfChars = getProfileStringA( AppName, KeyName, Default, Result, 1000 ).

     api_GetProfileInt( AppName, KeyName, Default, Result ):-
     		Result = getProfileIntA( AppName, KeyName, Default ).

     api_ShellExecute( Win, Operation, File, Params, Dir, ShowCmd ):-
     		RC = shellExecute( Win, Operation, File, Params, Dir, ShowCmd ),
     		check_HInstance( RC ).

    ifdef os_nt

      api_GetVersionEx( osversioninfo(Size, Major, Minor, Build, PlatID, CSDVersion_str ) ):-
      		OSVERSIONINFO_size = sizeof( OSVERSIONINFO ),
      		Full_OSVERSIONINFO_size = OSVERSIONINFO_size + 124,		% A size of STRING = 4 => 128-4 = 124
		OSVERSIONINFO_stru = makebinary( Full_OSVERSIONINFO_size ),
      		setdwordentry( OSVERSIONINFO_stru, 0, Full_OSVERSIONINFO_size ),
      		RC = getVersionExA( OSVERSIONINFO_stru ),
      		check_RC_Bool( RC ),
      		Size = getdwordentry( OSVERSIONINFO_stru, 0),
      		Major = getdwordentry( OSVERSIONINFO_stru, 1),
      		Minor = getdwordentry( OSVERSIONINFO_stru, 2),
      		Build = getdwordentry( OSVERSIONINFO_stru, 3),
      		PlatID = getdwordentry( OSVERSIONINFO_stru, 4),
      		CSDVersion = makebinary( 128 ),
		OSVERSIONINFO_stru_begaddr = cast( ULONG, OSVERSIONINFO_stru ),
		OSVERSIONINFO_dwstru_lastaddr = OSVERSIONINFO_stru_begaddr + sizeof( DWORD ) * 5,
		CSDVersion_addr = cast( ULONG, CSDVersion ),
		movmem( OSVERSIONINFO_dwstru_lastaddr, CSDVersion_addr, 128 ),
		CSDVersion_str = cast( STRING, CSDVersion ).

      api_CloseHandle( Handle ) :-
		RC = closeHandle( Handle ),
 		check_RC_Bool( RC ).

      api_GetSystemDirectory( Str ) :-
		Size = 1000,
		str_len( Str, Size ),
		RC = getSystemDirectoryA( Str, Size ),
 		check_RC_Bool( RC ).

      api_SuspendThread( Hndl, RC ) :-
		RC = suspendThread( Hndl ),
 		check_RC_Invalid( RC ).

      api_ResumeThread( Hndl, RC ) :-
		RC = resumeThread( Hndl ),
 		check_RC_Invalid( RC ).

      api_TerminateThread( Hndl, ExitCode ) :-
		RC = terminateThread( Hndl, ExitCode ),
 		check_RC_Bool( RC ).

      api_GetThreadPriority( Hndl, Priority ) :-
		Priority = getThreadPriority( Hndl ),
		Priority <> api_thread_priority_error_return,!.
      api_GetThreadPriority( _Hndl, api_thread_priority_error_return ) :-
		NTLastError = api_GetLastError(),
		LastError = NTLastError + nt_error_offset,
		run_Error( LastError ).

      api_SetThreadPriority( Hndl, Priority ) :-
		RC = setThreadPriority( Hndl, Priority ),
 		check_RC_Bool( RC ).

      api_GetExitCodeThread( Hndl, ExitCode ) :-
		RC = getExitCodeThread( Hndl, ExitCode ),
 		check_RC_Bool( RC ).

      api_WaitForSingleObject( Hndl, Timeout, RC ) :-
      		RC = waitForSingleObject( Hndl, Timeout),
      		RC <> api_wait_failed,!.
      api_WaitForSingleObject( _, _, api_wait_failed ) :-
      		NTLastError = api_GetLastError(),
		LastError = NTLastError + nt_error_offset,
		run_Error( LastError ).

      api_WaitForMultipleObjects( Handles, WaitAll, TimeOut, RC ):-
      		count_some( Handles, Count ),
      		ArraySize = Count * sizeof( DWORD ),
		HandlesArray = makebinary( ArraySize ),
		fill_SomeArray( Handles, HandlesArray, 0 ),
		RC = waitForMultipleObjects( Count, HandlesArray, WaitAll, TimeOut),
		RC <> api_wait_failed,!.
      api_WaitForMultipleObjects( _, _, _, api_wait_failed ) :-
      		NTLastError = api_GetLastError(),
		LastError = NTLastError + nt_error_offset,
		run_Error( LastError ).

      api_GetShortPathName( LongName, Buffer ) :-
      		BufLen = 1000,
		str_len( Buffer, BufLen ),
		RC = getShortPathNameA( LongName, Buffer, BufLen ),
		check_RC_Null( RC ).

      api_GetSystemTime( systemtime( Year, Month, DayOfWeek, Day, Hour, Minute, Second, Milliseconds ) ):-
      		SYSTEMTIME_size = sizeof( SYSTEMTIME ),
      		ST = makebinary( SYSTEMTIME_size ),
      		getSystemTime( ST ),
      		Year = getwordentry( ST, 0 ),
		Month = getwordentry( ST, 1 ),
		DayOfWeek = getwordentry( ST, 2 ),
		Day = getwordentry( ST, 3 ),
		Hour = getwordentry( ST, 4 ),
		Minute = getwordentry( ST, 5 ),
		Second = getwordentry( ST, 6 ),
		Milliseconds = getwordentry( ST, 7 ).

      api_GetProcessTimes( Handle, CreateTime, ExitTime, KernelTime, UserTime ):-
      		make_filetime_structures( CT, ET, KT, UT ),
      		RC = getProcessTimes( Handle, CT, ET, KT, UT ),
      		check_RC_Bool( RC ),
      		fill_filetime_structures( CT, ET, KT, UT, CreateTime, ExitTime, KernelTime, UserTime ).

      api_GetThreadTimes( Handle, CreateTime, ExitTime, KernelTime, UserTime ):-
      		make_filetime_structures( CT, ET, KT, UT ),
      		RC = getThreadTimes( Handle, CT, ET, KT, UT ),
      		check_RC_Bool( RC ),
      		fill_filetime_structures( CT, ET, KT, UT, CreateTime, ExitTime, KernelTime, UserTime ).

      api_FileTimeToLocalFileTime( FileTime, filetime(LT_Low,LT_High) ):-
      		FILETIME_size = sizeof( FILETIME ),
      		LT = makebinary( FILETIME_size ),
      		RC = fileTimeToLocalFileTime( FileTime, LT ),
      		check_RC_Bool( RC ),
      		LT_Low = getdwordentry( LT, 0 ),
		LT_High = getdwordentry( LT, 1 ).

      api_FileTimeToSystemTime( FileTime, systemtime( Year, Month, DayOfWeek, Day, Hour, Minute, Second, Milliseconds ) ):-
      		SYSTEMTIME_size = sizeof( SYSTEMTIME ),
      		ST = makebinary( SYSTEMTIME_size ),
      		RC = fileTimeToSystemTime( FileTime, ST ),
      		check_RC_Bool( RC ),
		Year = getwordentry( ST, 0 ),
		Month = getwordentry( ST, 1 ),
		DayOfWeek = getwordentry( ST, 2 ),
		Day = getwordentry( ST, 3 ),
		Hour = getwordentry( ST, 4 ),
		Minute = getwordentry( ST, 5 ),
		Second = getwordentry( ST, 6 ),
		Milliseconds = getwordentry( ST, 7 ).

     api_RegisterEventSource( "", SourceName, Handle ):-
     		NullServerName = cast( STRING, 0 ),
     		Handle = registerEventSourceA( NullServerName, SourceName ),
     		check_RC_Null( Handle ),!.
     api_RegisterEventSource( UNCServerName, SourceName, Handle ):-
     		Handle = registerEventSourceA( UNCServerName, SourceName ),
     		check_RC_Null( Handle ).

     api_DeregisterEventSource( EventLog_handle ):-
     		RC = deregisterEventSource( EventLog_handle ),
     		check_RC_Bool( RC ).

     api_ReportEvent( EventLog_handle, Type, Category, EventID, Strings, RawData ):-
     		count_some( Strings, StringsNumber ),
     		DWORD_size = sizeof( DWORD ),
     		PtrStrArr_size = DWORD_size*StringsNumber,
		PtrStrArr = makebinary( PtrStrArr_size ),
     		fill_SomeArray( Strings, PtrStrArr, 0 ),
		RawData_size = getbinarysize( RawData ),
     		RC = reportEventA( EventLog_handle, Type, Category, EventID, 0, StringsNumber, RawData_size, PtrStrArr, RawData),
     		check_RC_Bool( RC ).

     api_FindFirstChangeNotification( PathName, WatchSubtree, NotifyFilter_list, Handle ):-
     		list_or( NotifyFilter_list, NotifyFilter ),
     		Handle = findFirstChangeNotificationA( PathName, WatchSubtree, NotifyFilter ),
     		check_RC_Invalid( Handle ).

     api_FindNextChangeNotification( ChangeHandle ):-
     		RC = findNextChangeNotification( ChangeHandle ),
     		check_RC_Bool( RC ).

     api_FindCloseChangeNotification( ChangeHandle ):-
     		RC = findCloseChangeNotification( ChangeHandle ),
     		check_RC_Bool( RC ).

     api_GetStdHandle( StdHandle, Result ):-
     		Result = getStdHandle( StdHandle ),
     		check_RC_Invalid( Result ).

     api_CharToOem( Src, Dst ) :-
		str_len( Src, Len ),
		str_len( Dst, Len ),
		charToOemA( Src, Dst ).

     api_OemToChar( Src, Dst ) :-
		str_len( Src, Len ),
		str_len( Dst, Len ),
		oemToCharA( Src, Dst ).

     api_MultiByteToWideChar( CodePage, Flags_list, Src, Dst ):-
		list_or( Flags_list, Flags ),
		NullDst = cast( STRING, 0 ),
		RC = multiByteToWideChar( CodePage, Flags, Src, -1, NullDst, 0 ),
		check_RC_Bool( RC ),
		RCC = RC * 2,				% wide char twice larger than common string
		str_len( Dst, RCC ),
		RC1 = multiByteToWideChar( CodePage, Flags, Src, -1, Dst, RC ),
		check_RC_Bool( RC1 ).

     api_WideCharToMultiByte( CodePage, Flags_list, Src, Dst ):-
		list_or( Flags_list, Flags ),
		NullDst = cast( STRING, 0 ),
		NullDefaultChar = cast( STRING, 0 ),
		NullUsedDefaultChar = cast( LONG, 0 ),
		RC = wideCharToMultiByte( CodePage, Flags, Src, -1, NullDst, 0, NullDefaultChar, NullUsedDefaultChar ),
		check_RC_Bool( RC ),
		str_len( Dst, RC ),
		RC1 = wideCharToMultiByte( CodePage, Flags, Src, -1, Dst, RC, NullDefaultChar, NullUsedDefaultChar ),
		check_RC_Bool( RC1 ).
     api_WideCharToMultiByte( CodePage, Flags_list, Src, DefaultChar, UsedDefaultChar, Dst ):-
     		list_or( Flags_list, Flags ),
		NullDst = cast( STRING, 0 ),
		LPUsedDefaultChar = cast( LONG, UsedDefaultChar ),
		RC = wideCharToMultiByte( CodePage, Flags, Src, -1, NullDst, 0, DefaultChar, LPUsedDefaultChar ),
		check_RC_Bool( RC ),
		str_len( Dst, RC ),
		RC1 = wideCharToMultiByte( CodePage, Flags, Src, -1, Dst, RC, DefaultChar, LPUsedDefaultChar ),
		check_RC_Bool( RC1 ).

    % api_lstrlen( WideCharString, Length ):-
    % 		Length = lstrlenW( WideCharString ).

     api_GetNumberFormat( Locale, Flags_list, Value, ValueFormat, Buffer ):-
     		list_or( Flags_list, Flags ),
     		str_len( Buffer, 1000 ),
     		RC = getNumberFormatA( Locale, Flags, Value, ValueFormat, Buffer, 1000 ),
     		check_RC_Null( RC ).

     api_GetLocaleInfo( Locale, LCType, Buffer ):-
     		str_len( Buffer, 1000 ),
     		RC = getLocaleInfoA( Locale, LCType, Buffer, 1000 ),
     		check_RC_Null( RC ).

     api_GetUserName( UserName ):-
     		str_len( UserName, 1000 ),
     		Dword_size = sizeof( DWORD ),
		Valuesize_bin = makebinary( Dword_size ),
		setdwordentry( Valuesize_bin, 0, 1000 ),
		Valuesize_ulong = cast( ULONG, Valuesize_bin ),
     		RC = getUserNameA( UserName, Valuesize_ulong ),
     		check_RC_Bool( RC ).

     api_GetComputerName( UserName ):-
     		str_len( UserName, 1000 ),
     		Dword_size = sizeof( DWORD ),
		Valuesize_bin = makebinary( Dword_size ),
		setdwordentry( Valuesize_bin, 0, 1000 ),
		Valuesize_ulong = cast( ULONG, Valuesize_bin ),
     		RC = getComputerNameA( UserName, Valuesize_ulong ),
     		check_RC_Bool( RC ).

     api_CreateEvent( ManualReset, InitialState, Name, Handle ) :-
		Security = cast( BINARY, 0 ),	/* ### */
		Handle = createEventA( Security,
				       ManualReset,
				       InitialState,
				       Name ),
		check_RC_Null( Handle ),
		NTLastError = api_GetLastError(),
		NTLastError = 0, !.
     api_CreateEvent( _, _, _, api_invalid_handle ) :-
		NTLastError = api_GetLastError(),
		LastError = NTLastError + nt_error_offset,
		run_Error( LastError ).

     api_ResetEvent( EventHandle ) :-
		RC = resetEvent( EventHandle ),
     		check_RC_Bool( RC ).

     api_SetEvent( EventHandle ) :-
		RC = setEvent( EventHandle ),
     		check_RC_Bool( RC ).

    enddef	/* os_nt */

    ifdef platform_16bit

      api_AnsiToOem( Src, Dst ):-
      		str_len( Src, Len ),
		str_len( Dst, Len ),
		ansiToOem( Src, Dst ).

      api_OemToAnsi( Src, Dst ) :-
		str_len( Src, Len ),
		str_len( Dst, Len ),
		oemToAnsi( Src, Dst ).

    enddef
