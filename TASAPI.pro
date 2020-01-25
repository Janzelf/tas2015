/*****************************************************************************

		Copyright (c) 1989 - 2000 De Lint Associates

 Project:  TASVP1
 FileName: TASAPI.PRO
 Purpose: No description
 Written by: JdL
 Comments: changed apicalls
******************************************************************************/

include "tasvp1.inc"
include "tasvp1.con"
%include "tasapi.pre"
include "apicalls.pro"
%include "apicalls\\win\\SMAPI1\\smapi.pro"

ifndef ws_win
  #error Unsupported platform
enddef

ifndef smapi_con_included
  #Error : smapi.con must be included
enddef
ifndef smapi_dom_included
  #Error : smapi.dom must be included
enddef
ifndef smapi_pre_included
  #Error : smapi.pre must be included
enddef

ifndef smapi_pro_included

  constants

    smapi_pro_included = 1

    ifdef platform_32bit
      
      mapilib_name 		= "mapi32.dll"
      mapiSendMail_name		= "MAPISendMail"
      mapiAddress_name		= "MAPIAddress"
      mapiFreeBuffer_name	= "MAPIFreeBuffer"
      mapiDeleteMail_name	= "MAPIDeleteMail"
      mapiDetails_name		= "MAPIDetails"
      mapiFindNext_name		= "MAPIFindNext"
      mapiLogoff_name		= "MAPILogoff"
      mapiLogon_name		= "MAPILogon"
      mapiReadMail_name		= "MAPIReadMail"
      mapiResolveName_name	= "MAPIResolveName"
      mapiSaveMail_name		= "MAPISaveMail"
      mapiSendDocuments_name	= "MAPISendDocuments"
      
    elsedef
      
      mapilib_name 		= "mapi.dll"
      mapiSendMail_name		= "MAPISENDMAIL"
      mapiAddress_name		= "MAPIADDRESS"
      mapiFreeBuffer_name	= "MAPIFREEBUFFER"
      mapiDeleteMail_name	= "MAPIDELETEMAIL"
      mapiDetails_name		= "MAPIDETAILS"
      mapiFindNext_name		= "MAPIFINDNEXT"
      mapiLogoff_name		= "MAPILOGOFF"
      mapiLogon_name		= "MAPILOGON"
      mapiReadMail_name		= "MAPIREADMAIL"
      mapiResolveName_name	= "MAPIRESOLVENAME"
      mapiSaveMail_name		= "MAPISAVEMAIL"
      mapiSendDocuments_name	= "MAPISENDDOCUMENTS"
      
    enddef

  ifndef pdcrunt_pre_included

    global predicates
	
      procedure movmem( BINARY Src, BINARY Dst, UNSIGNED Bytes ) 		- (i,i,i) 	language c as "_MEM_MovMem"
      procedure movmem( ULONG Source, ULONG Dest, UNSIGNED Len)	 		- ( i, i, i ) 	language c as "_MEM_MovMem"    
      procedure run_Error( UNSIGNED errorno ) 					- ( i ) 	language c as "_RUN_Error"    

  enddef
  ifndef apicalls_pre_included

    constants

      ifdef platform_16bit
        callKind = pascal
      elsedef
        callKind = stdcall
      enddef
      
      ifdef platform_16bit
        hinstance_error = 32
      enddef

      error_offset = 7000

    ifdef platform_32bit

      ifdef use_omf_objformat

	api_LoadLibraryA_FNS			= "LoadLibraryA"
	api_FreeLibrary_FNS			= "FreeLibrary"
	api_GetProcAddress_FNS			= "GetProcAddress"
	api_GetLastError_FNS			= "GetLastError"

      elsedef

	api_LoadLibraryA_FNS			= "_LoadLibraryA"
	api_FreeLibrary_FNS			= "_FreeLibrary"
	api_GetProcAddress_FNS			= "_GetProcAddress"
	api_GetLastError_FNS			= "_GetLastError"

      enddef

    elsedef

      api_LoadLibraryA_FNS			= "LOADLIBRARY"
      api_FreeLibrary_FNS			= "FREELIBRARY"
      api_GetProcAddress_FNS			= "GETPROCADDRESS"

    enddef
    
  global predicates

   %procedure helpinvoke (window, string FileName, unsigned Command, integer Data) - (i,i,i,i) language stdcall as "_HtmlHelpA@16"

    procedure OS_HANDLE loadLibraryA( STRING )					- (i)		language callKind as api_LoadLibraryA_FNS
    procedure BOOLEAN	freeLibrary( OS_HANDLE )				- (i)		language callKind as api_FreeLibrary_FNS
    procedure DWORD	getProcAddress( OS_HANDLE, STRING )			- (i,i)		language callKind as api_GetProcAddress_FNS
    ifdef platform_32bit    
      procedure DWORD	api_GetLastError()		 			- 		language callKind as api_GetLastError_FNS
    enddef

  predicates

    procedure OS_HANDLE api_LoadLibrary( STRING LibraryName )			- (i)
    procedure		api_FreeLibrary( OS_HANDLE LibraryHandle )		- (i)
    procedure DWORD	api_GetProcAddress( OS_HANDLE LibraryHandle,
    					    STRING FuncHandle )			- (i,i)
    ifdef platform_32bit
      procedure 		check_RC_Bool( BOOLEAN ) 			- (i)
    elsedef    
      procedure 		check_RC_Handle( OS_HANDLE ) 			- (i)    
    enddef    
    
    procedure 		check_RC_Null( ULONG )				 	- (i)
    					    
  clauses

    api_LoadLibrary( Name, Handle ):-
 	Handle = loadLibraryA( Name ),
        ifdef platform_32bit 	
 	  check_RC_Null( Handle ).
        elsedef
 	  check_RC_Handle( Handle ).
        enddef
     		
    api_FreeLibrary( Handle ):-
        ifdef platform_32bit
     	  RC = freeLibrary( Handle ),
     	  check_RC_Bool( RC ).
        elsedef
	  freeLibrary( Handle ).
        enddef
    
    api_GetProcAddress( LibHandle, FuncName, FuncHandle ):-
  	FuncHandle = getProcAddress( LibHandle, FuncName ),
    	check_RC_Null( FuncHandle ).

    ifdef platform_32bit

    check_RC_Bool( b_false ) :- !,
	NTLastError = api_GetLastError(),
	LastError = NTLastError + error_offset,
	run_Error( LastError ).
    check_RC_Bool( _ ).

    check_RC_Null( 0 ) :- !,
	NTLastError = api_GetLastError(),
	LastError = NTLastError + error_offset,
	run_Error( LastError ).
    check_RC_Null( _ ).
    
    elsedef

    check_RC_Handle( Err ) :- 
    	Err < hinstance_error,!,
	run_Error( Err ).
    check_RC_Handle( _ ).

    check_RC_Null( 0 ) :- !,
	run_Error( 0 ).
    check_RC_Null( _ ).
    
    enddef

  enddef	/*apicalls_pre_included*/

			/************************/
			/* Names of API calls  */
			/************************/

  domains

    MAPISendMail	= procedure ULONG ( LHANDLE, ULONG, MAPIMESSAGE, ULONG, ULONG )		- (i,i,i,i,i) language callKind
    MAPIAddress		= procedure ULONG ( LHANDLE, ULONG, STRING, ULONG, STRING, ULONG, 
      			 		    MAPIRECIPDESC_ARRAY, ULONG, ULONG, ULONG, ULONG )	- (i,i,i,i,i,i,i,i,i,o,o) language callKind
    MAPIFreeBuffer	= procedure ULONG ( ULONG ) 						- (i) language callKind
    MAPIDeleteMail	= procedure ULONG ( LHANDLE, ULONG, STRING, ULONG, ULONG ) 		- (i,i,i,i,i) language callKind
    MAPIDetails		= procedure ULONG ( LHANDLE, ULONG, MAPIRECIPDESC, ULONG, ULONG ) 	- (i,i,i,i,i) language callKind
    MAPIFindNext	= procedure ULONG ( LHANDLE, ULONG, STRING, STRING, ULONG, ULONG, BINARY ) - (i,i,i,i,i,i,i) language callKind
    MAPILogoff		= procedure ULONG ( LHANDLE, ULONG, ULONG, ULONG )			- (i,i,i,i) language callKind
    MAPILogon		= procedure ULONG ( ULONG, STRING, STRING, ULONG, ULONG, LHANDLE ) 	- (i,i,i,i,i,o) language callKind
    MAPIReadMail	= procedure ULONG ( LHANDLE, ULONG, STRING, ULONG, ULONG, ULONG )	- (i,i,i,i,i,o) language callKind
    MAPIResolveName	= procedure ULONG ( LHANDLE, ULONG, STRING, ULONG, ULONG, ULONG )	- (i,i,i,i,i,o) language callKind
    MAPISaveMail	= procedure ULONG ( LHANDLE, ULONG, MAPIMESSAGE, ULONG, ULONG, BINARY ) - (i,i,i,i,i,i) language callKind
    MAPISendDocuments	= procedure ULONG ( ULONG, STRING, STRING, STRING, ULONG )		- (i,i,i,i,i) language callKind

  database - smapi_db
      
    determ smapi( OS_HANDLE )

  predicates

    procedure ULONG 	sMAPI_FreeBuffer( ULONG ) - (i)

    procedure 	getMAPIHandle( OS_HANDLE Handle )					- (o)
    procedure 	list_or1( DWORD_LIST, DWORD )						- (i,o)
    procedure 	count_some1( MAPIRECIPDESC_LIST, UNSIGNED DescNumber )			- (i,o)
    procedure 	count_some1( MAPIFILEDESC_LIST, UNSIGNED DescNumber )			- (i,o)
    procedure 	fill_SomeArrayFromLst( MAPIRECIPDESC_LIST, BINARY DescsArray, DWORD Index )	- (i,i,i)
    procedure 	fill_SomeArrayFromLst( MAPIFILEDESC_LIST, BINARY DescsArray, DWORD Index )	- (i,i,i)
    procedure 	fill_RecipLstFromArray( ULONG, ULONG, BINARY, MAPIRECIPDESC_LIST )	- (i,i,i,o)
    procedure 	fill_FileLstFromArray( ULONG, ULONG, BINARY, MAPIFILEDESC_LIST )	- (i,i,i,o)      
    procedure 	check_RC_MAPIAddress( ULONG, ULONG, ULONG, MAPIRECIPDESC_ARRAY )	- (i,i,i,o)
    procedure 	check_RC_MAPIResolveName( ULONG, ULONG,  MAPIRECIPDESC )		- (i,i,o)
    procedure 	check_RC_MAPIReadMail( ULONG, ULONG,  MAPIMESSAGE )			- (i,i,o)
    procedure	check_RC_SendMailMessage( ULONG )					- (i)
    procedure 	form_stringSendDocs( SLIST, STRING, STRING )				- (i,i,o)
%    procedure	form_Recips( MAPIRECIPDESC_LIST, MAPINAMEADDR_LIST, ULONG, MAPIRECIPDESC_LIST )	- (i,i,i,o)
    procedure   zeefNaam(string, string, string)
    procedure   goedChar(char, string)

  clauses

    getMAPIHandle( Handle ):- smapi( Handle ),!.
    getMAPIHandle( Handle ):-
	Handle = api_LoadLibrary( mapilib_name ),
    	assert(smapi( Handle )).
    
    list_or1( [], 0 ):-!.
    list_or1( [El|L], Result ):-!,
	list_or1( L, TmpRes ),
ifndef	platform_32bit
	TmpRes_u = cast( UNSIGNED, TmpRes ),
	El_u = cast( UNSIGNED, El ),
	bitor( TmpRes_u, El_u, Result_u ),
	Result = cast( ULONG, Result_u ).
elsedef
	bitor( TmpRes, El, Result ).
enddef
    list_or1( _, 0 ).		%dummy

    count_some1( [], 0 ) :- !.
    count_some1( [_|L], Cnt ) :- !,
	count_some1( L, C ),
	Cnt = C + 1.
    count_some1( _, 0 ).	%dummy

    fill_SomeArrayFromLst( [], _, _ ) :- !.
    fill_SomeArrayFromLst( [El|L], Array, Offset ) :-
	El_size = sizeof( El ),
	Array_begaddr = cast( ULONG, Array ),
	El_begaddr = cast( ULONG, El ),
	El_array_addr = Array_begaddr + Offset,
	movmem( El_begaddr, El_array_addr, El_size ),
	NextOffset = Offset + El_size,!,
	fill_SomeArrayFromLst( L, Array, NextOffset ).
    fill_SomeArrayFromLst( _, _, _ ).		%dummy	

    fill_RecipLstFromArray( Limit, Limit,  _, [] ):-!.
    fill_RecipLstFromArray( Offset, Limit, Array, [El|L] ):-
    	El_size = sizeof( MAPIRECIPDESC ),
	El_bin = makebinary( El_size ),
	El_begaddr = cast( ULONG, El_bin ),
	Array_begaddr = cast( ULONG, Array ),
	El_array_addr = Array_begaddr + Offset,
	movmem( El_array_addr, El_begaddr, El_size ),
	El = cast( MAPIRECIPDESC, El_bin ),
    	NewOffset = Offset + El_size,!,
    	fill_RecipLstFromArray( NewOffset, Limit, Array, L ).
    fill_RecipLstFromArray( _, _,  _, [] ).	%dummy    	
     
    fill_FileLstFromArray( Limit, Limit,  _, [] ):-!.
    fill_FileLstFromArray( Offset, Limit, Array, [El|L] ):-
    	El_size = sizeof( MAPIFILEDESC ),
	El_bin = makebinary( El_size ),
	El_begaddr = cast( ULONG, El_bin ),
	Array_begaddr = cast( ULONG, Array ),
	El_array_addr = Array_begaddr + Offset,
	movmem( El_array_addr, El_begaddr, El_size ),
	El = cast( MAPIFILEDESC, El_bin ),
     	NewOffset = Offset + El_size,!,
     	fill_FileLstFromArray( NewOffset, Limit, Array, L ).

    check_RC_MAPIAddress( sMAPI_SUCCESS_SUCCESS, 0, _, Null_bin ):-
    	Null_bin = makebinary( 0 ),!.
    check_RC_MAPIAddress( sMAPI_SUCCESS_SUCCESS, Number, NewRecip_ptr,  NewRecip_stru):-
	NewRecip_stru_size = Number * sizeof( MAPIRECIPDESC ),
ifndef 	platform_32bit
	NewRecip_stru_size_u = cast( UNSIGNED, NewRecip_stru_size ),
	NewRecip_stru = makebinary( NewRecip_stru_size_u ),	
elsedef
	NewRecip_stru = makebinary( NewRecip_stru_size ),
enddef
	NewRecip_stru_begaddr = cast( ULONG, NewRecip_stru ),
ifndef 	platform_32bit	
	movmem( NewRecip_ptr, NewRecip_stru_begaddr, NewRecip_stru_size_u ),
elsedef
	movmem( NewRecip_ptr, NewRecip_stru_begaddr, NewRecip_stru_size ),
enddef	
	_RC = sMAPI_FreeBuffer( NewRecip_ptr ),
     	!.
    check_RC_MAPIAddress( _, _, _, Null_bin ):-
     	Null_bin = makebinary( 0 ),!.
     
    check_RC_MAPIResolveName( sMAPI_SUCCESS_SUCCESS, RecipDesc_ptr, RecipDesc_stru_s ):-
     	RecipDesc_stru_size = sizeof( MAPIRECIPDESC ),
	RecipDesc_stru = makebinary( RecipDesc_stru_size ),
	RecipDesc_stru_begaddr = cast( ULONG, RecipDesc_stru ),
	movmem( RecipDesc_ptr, RecipDesc_stru_begaddr, RecipDesc_stru_size ),
	RecipDesc_stru_s = cast( MAPIRECIPDESC, RecipDesc_stru ),
	_RC = sMAPI_FreeBuffer( RecipDesc_ptr ),
	!.
    check_RC_MAPIResolveName( _, _, mapirecipdesc( 0, 0, "", "", 0, Null_bin) ):-			% an error
     	Null_bin = makebinary( 0 ),!.
     
    check_RC_MAPIReadMail( sMAPI_SUCCESS_SUCCESS, Message_ptr, Message_stru_s ):-
     	Message_stru_size = sizeof( MAPIMESSAGE ),
	Message_stru = makebinary( Message_stru_size ),
	Message_stru_begaddr = cast( ULONG, Message_stru ),
	movmem( Message_ptr, Message_stru_begaddr, Message_stru_size ),
	Message_stru_s = cast( MAPIMESSAGE, Message_stru ),
	_RC = sMAPI_FreeBuffer( Message_ptr),!.
    check_RC_MAPIReadMail( _, _, mapimessage( 0, "", "", "", "", "", 0, Null_bin, 0, Null_bin, 0, Null_b ) ):-% an error
    	Null_bin = makebinary( 0 ),
    	Null_b = cast( MAPIFILEDESC_ARRAY, Null_bin ),
    	!.

    form_stringSendDocs( [], _, "" ):-!.
    form_stringSendDocs( [El|L], DelimChar, Str_out ):-!,
    	form_stringSendDocs( L, DelimChar, Str ),
    	format( Str_out, "%s%s%s", El, DelimChar, Str ).
    form_stringSendDocs( _, _, "" ).	%dummy

    form_Recips( Prev_list, [], _, Prev_list ):-!.
    form_Recips( Prev_list, [mapinameaddr(Name,Address)|L], AddrType, [mapirecipdesc(0,AddrType,Naam,SMTP_addr,0,NULL_EntryID)|Recips] ):-
        zeefNaam(Name, "", Naam),
  	format( SMTP_addr, "SMTP:%s", Address ),
  	NULL_EntryID = cast( BINARY, 0 ),!,
  	form_Recips( Prev_list, L, AddrType, Recips ).
    form_Recips( Prev_list, _, _, Prev_list ):-!.

    check_RC_SendMailMessage( sMAPI_SUCCESS_SUCCESS ):-!.
    check_RC_SendMailMessage( ErrorCode ):-
    	ifdef platform_32bit
		run_Error( ErrorCode ).
	elsedef
		ErrorCode_u = cast( UNSIGNED, ErrorCode ),
		run_Error( ErrorCode_u ).
	enddef

  clauses
    
    sMAPI_Init():-smapi(_),!.
    sMAPI_Init():-
	Handle = api_LoadLibrary( mapilib_name ),
     	assert(smapi( Handle )).
     
    sMAPI_Terminate():- not(smapi(_)),!.
    sMAPI_Terminate():-
    	smapi( Handle ),
    	api_FreeLibrary( Handle ),
    	retract(smapi( Handle )),!.
    sMAPI_Terminate().

    sMAPI_ConvRecipDesc_Array( [], NullArray, 0 ):-
    	free( NullArray ),
    	NullArray = cast( BINARY, 0 ),!.
    sMAPI_ConvRecipDesc_Array( [], NullArray, 0 ):-
    	bound( NullArray ),!.
    sMAPI_ConvRecipDesc_Array( Descs_list, DescsArray, Count ):-
    	bound( Descs_list ),
    	free( DescsArray ),
    	free( Count ),
    	count_some1( Descs_list, Count ),
    	ArraySize = Count * sizeof( MAPIRECIPDESC ),
	DescsArray = makebinary( ArraySize ),
	fill_SomeArrayFromLst( Descs_list, DescsArray, 0 ),!.
    sMAPI_ConvRecipDesc_Array( Descs_list, DescsArray, Count ):-
    	free( Descs_list ),
    	bound( Count ),
    	bound( DescsArray ),
    	Limit = Count * sizeof( MAPIRECIPDESC ),
	fill_RecipLstFromArray( 0, Limit, DescsArray, Descs_list ).
		
    sMAPI_ConvFileDesc_Array( [], NullArray, 0 ):-
    	free( NullArray ),
    	NullArray = cast( BINARY, 0 ),!.
    sMAPI_ConvFileDesc_Array( [], NullArray, 0 ):-
    	bound( NullArray ),!.
    sMAPI_ConvFileDesc_Array( Descs_list, DescsArray, Count ):-
    	bound( Descs_list ),
    	free( DescsArray ),
    	free( Count ),     		
    	count_some1( Descs_list, Count ),
    	ArraySize = Count * sizeof( MAPIFILEDESC ),
	DescsArray = makebinary( ArraySize ),
	fill_SomeArrayFromLst( Descs_list, DescsArray, 0 ),!.
    sMAPI_ConvFileDesc_Array( Descs_list, DescsArray, Count ):-
    	free( Descs_list ),
    	bound( Count ),
    	bound( DescsArray ),     		
    	Limit = Count * sizeof( MAPIFILEDESC ),
	fill_FileLstFromArray( 0, Limit, DescsArray, Descs_list ).     		

    sMAPI_FreeBuffer( Data, RC ):-
    	getMAPIHandle( Handle ),
    	MAPIFreeBuffer_ptr = cast( MAPIFreeBuffer, api_GetProcAddress( Handle, mapiFreeBuffer_name )),
    	RC = MAPIFreeBuffer_ptr( Data ).

    sMAPI_SendMail( SessHandle, Hwnd, Message, Flag_list, RC ):-
    	getMAPIHandle( Handle ),
    	list_or1( Flag_list, Flags ),
    	MAPISendMail_ptr = cast( MAPISendMail, api_GetProcAddress( Handle, mapiSendMail_name )),
    	RC = MAPISendMail_ptr( SessHandle, Hwnd, Message, Flags, 0 ).
     
    sMAPI_Address( SessHandle, Hwnd, Caption, EditFields, Labels, RecipsNumb, Recips, Flag_list, NewRecipsNumb, NewRecips, RC ):-
    	getMAPIHandle( Handle ),
    	list_or1( Flag_list, Flags ),
    	MAPIAddress_ptr = cast( MAPIAddress, api_GetProcAddress( Handle, mapiAddress_name )),
    	RC = MAPIAddress_ptr( SessHandle, Hwnd, Caption, EditFields, Labels, RecipsNumb, Recips, Flags, 0, 
     						    NewRecipsNumb, NewRecips_tmp ),
    	check_RC_MAPIAddress( RC, NewRecipsNumb, NewRecips_tmp, NewRecips ).
     
    sMAPI_DeleteMail( SessHandle, Hwnd, MessageID, RC ):-
    	getMAPIHandle( Handle ),
    	MAPIDeleteMail_ptr = cast( MAPIDeleteMail, api_GetProcAddress( Handle, mapiDeleteMail_name )),
    	RC = MAPIDeleteMail_ptr( SessHandle, Hwnd, MessageID, 0, 0 ).
     		
    sMAPI_Details( SessHandle, Hwnd, Recip, Flags_list, RC ):-
    	getMAPIHandle( Handle ),
    	list_or1( Flags_list, Flags ),
    	MAPIDetails_ptr = cast( MAPIDetails, api_GetProcAddress( Handle, mapiDetails_name )),
     	RC = MAPIDetails_ptr( SessHandle, Hwnd, Recip, Flags, 0 ).
     		
    sMAPI_FindNext( SessHandle, Hwnd, MessageType, SeedMessageID, Flags_list, MessageID, RC ):-
    	getMAPIHandle( Handle ),
    	list_or1( Flags_list, Flags ),
    	MessageID_bin = makebinary( 512 ),
    	MAPIFindNext_ptr = cast( MAPIFindNext, api_GetProcAddress( Handle, mapiFindNext_name )),
     	RC = MAPIFindNext_ptr( SessHandle, Hwnd, MessageType, SeedMessageID, Flags, 0, MessageID_bin ),
     	MessageID = cast( STRING, MessageID_bin ).
     
    sMAPI_Logoff( SessHandle, Hwnd, RC ):-
    	getMAPIHandle( Handle ),
    	MAPILogoff_ptr = cast( MAPILogoff, api_GetProcAddress( Handle, mapiLogoff_name )),
    	RC = MAPILogoff_ptr( SessHandle, Hwnd, 0, 0 ).
     		
    sMAPI_Logon( Hwnd, ProfileName, Password, Flags_list, SessHandle, RC ):-
    	getMAPIHandle( Handle ),
    	list_or1( Flags_list, Flags ),
    	MAPILogon_ptr = cast( MAPILogon, api_GetProcAddress( Handle, mapiLogon_name )),
     	RC = MAPILogon_ptr(  Hwnd, ProfileName, Password, Flags, 0, SessHandle ).
     		
    sMAPI_ReadMail( SessHandle, Hwnd, MessageID, Flags_list, Message_stru, RC ):-
    	getMAPIHandle( Handle ),
    	list_or1( Flags_list, Flags ),
    	MAPIReadMail_ptr = cast( MAPIReadMail, api_GetProcAddress( Handle, mapiReadMail_name )),
     	RC = MAPIReadMail_ptr(  SessHandle, Hwnd, MessageID, Flags, 0, Message_ptr ),
     	check_RC_MAPIReadMail( RC, Message_ptr, Message_stru ).

    sMAPI_ResolveName( SessHandle, Hwnd, Name, Flags_list, RecipDesc, RC ):-
    	getMAPIHandle( Handle ),
    	list_or1( Flags_list, Flags ),
    	MAPIResolveName_ptr = cast( MAPIResolveName, api_GetProcAddress( Handle, mapiResolveName_name )),
     	RC = MAPIResolveName_ptr(  SessHandle, Hwnd, Name, Flags, 0, RecipDesc_ptr ),
     	check_RC_MAPIResolveName( RC, RecipDesc_ptr, RecipDesc ).      		
      		
    sMAPI_SaveMail( SessHandle, Hwnd, Message, Flags_list, MessageID, RC ):-
    	bound( MessageID ),
    	getMAPIHandle( Handle ),
    	list_or1( Flags_list, Flags ),
    	MessageID_bin = cast( BINARY, MessageID ),
    	MAPISaveMail_ptr = cast( MAPISaveMail, api_GetProcAddress( Handle, mapiSaveMail_name )),
    	RC = MAPISaveMail_ptr(  SessHandle, Hwnd, Message, Flags, 0, MessageID_bin ),!.
    sMAPI_SaveMail( SessHandle, Hwnd, Message, Flags_list, MessageID, RC ):-
    	free( MessageID ),
    	getMAPIHandle( Handle ),
    	list_or1( Flags_list, Flags ),
    	MessageID_bin = makebinary( 512 ),
    	MAPISaveMail_ptr = cast( MAPISaveMail, api_GetProcAddress( Handle, mapiSaveMail_name )),
    	RC = MAPISaveMail_ptr(  SessHandle, Hwnd, Message, Flags, 0, MessageID_bin ),
    	MessageID = cast( STRING, MessageID_bin ).
      
    sMAPI_SendDocuments( Hwnd, DelimChar, FullPaths_list, FileNames_list, RC ):-
    	getMAPIHandle( Handle ),
    	form_stringSendDocs( FullPaths_list, DelimChar, FullPaths ),
    	form_stringSendDocs( FileNames_list, DelimChar, FileNames ),
    	MAPISendDocuments_ptr = cast( MAPISendDocuments, api_GetProcAddress( Handle, mapiSendDocuments_name )),
    	RC = MAPISendDocuments_ptr(  Hwnd, DelimChar, FullPaths, FileNames, 0 ).
    	
    sMAPI_SendMailMessage( To_list, CC_list, BCC_list, Subject, MessageText ):-
    	form_Recips( [], To_list, sMAPI_TO, Lst1 ),
    	form_Recips( Lst1, CC_list, sMAPI_CC, Lst2 ),
    	form_Recips( Lst2, BCC_list, sMAPI_BCC, Recips_list),
  	sMAPI_ConvRecipDesc_Array( Recips_list, Recips_ptr, Count ),
  	NULL_originator = cast( MAPIRECIPDESC_ARRAY, 0 ),
  	NULL_files = cast( MAPIFILEDESC_ARRAY, 0 ),
  	RC = sMAPI_SendMail( 0, 0, mapimessage(0, Subject, MessageText, "", "", "", 0, NULL_originator,Count,Recips_ptr,0,NULL_files), [ sMAPI_Logon_UI ] ),
  	check_RC_SendMailMessage( RC ).

goedChar(Char, Uit) :-
  Char >= 'a',
  Char <= 'z',
  str_char(Uit, Char), !.
goedChar(Char, Uit) :-
  Char >= 'A',
  Char <= 'Z',
  str_char(Uit, Char), !.
goedchar('ë', "e") :- !.
goedchar('é', "e") :- !.
goedchar('ï', "i") :- !.
goedchar(' ', " ") :- !.
goedchar(_, "").

zeefNaam(Naam, In, Uit) :-
  frontchar(Naam,FrontChar,RestString),
  goedChar(FrontChar, GChar), !,
  concat(In, GChar, In1),
  zeefNaam(RestString, In1, Uit).
zeefNaam(_, In, In).
     		
enddef


ifndef apicomm_pro_included

CONSTANTS

  apicomm_pro_included = 1

GLOBAL PREDICATES

  procedure OS_Handle CreateFile(string,dword DesiredAccess,dword ShareMode,
		dword SecurityAttributes,dword CreationDistribution,
		dword FlagsAndAttributes,OS_Handle TemplateFile)
		- (i,i,i,i,i,i,i)  language stdcall as api_CreateFile_FNS

  procedure boolean ReadFileSync(OS_Handle,string,dword StrLen,dword RecLen,dword)
		- (i,i,i,o,i) language stdcall as api_ReadFile_FNS

  procedure boolean WriteFileSync(OS_Handle,string,dword StrLen,dword RecLen,dword)
		- (i,i,i,o,i) language stdcall as api_WriteFile_FNS

  procedure OS_Handle CreateThread(dword SecurityAttributes,dword StackSize,functionAddress StartAdr,
		dword Parameter,dword Flags,OS_THREAD_ID ThreadID)
		- (i,i,i,i,i,o)  language stdcall as api_CreateThread_FNS

  procedure OS_Handle CreateMutex(dword SecurityAttributes,boolean InitialOwner,string MutexName)
		- (i,i,i) language stdcall as api_CreateMutex_FNS

  procedure boolean ReleaseMutex(OS_Handle) - (i) language stdcall as api_ReleaseMutex_FNS

  procedure boolean SetDefaultCommConfig(string,binary LpCC,dword Size)
		- (i,i,i) language stdcall as api_SetDefaultCommConfig_FNS

  procedure boolean SetCommState(OS_Handle HComm,dcb CommDCB)
  		- (i,i) language stdcall as api_SetCommState_FNS

  procedure boolean GetCommState(OS_Handle HComm,dcb CommDCB)
		- (i,dcb(o,o,o,o,o,o,o,o,o,o,o,o,o,o,o)) language stdcall as api_GetCommState_FNS

  procedure boolean GetCommMask(OS_Handle,dword Mask)
		- (i,o) language stdcall as api_GetCommMask_FNS

  procedure boolean SetCommMask(OS_Handle,dword Mask)
		- (i,i) language stdcall as api_SetCommMask_FNS

  procedure boolean SetCommTimeouts(OS_Handle,commTimeouts Timeouts)
		- (i,i) language stdcall as api_SetCommTimeouts_FNS

  procedure boolean PurgeComm(OS_Handle,dword Flags)
		- (i,i) language stdcall as api_PurgeComm_FNS

  procedure boolean ClearCommError(OS_Handle,dword,commStat)
		- (i,o,api_CommStat(o,o,o)) language stdcall as api_ClearCommError_FNS

  procedure boolean GetOverlappedResult(OS_Handle,overlapped,dword,boolean)
		- (i,i,o,i) language stdcall as api_GetOverlappedResult_FNS


CLAUSES

  api_CreateFile(Name,DesiredAccess,ShareMode,SecurityAttributes,CreationDistribution,
  		FlagsAndAttributes,TemplateFile,Handle):-
	Handle = CreateFile(Name,DesiredAccess,ShareMode,SecurityAttributes,CreationDistribution,
  		FlagsAndAttributes,TemplateFile),
	ifdef platform_32bit
	  check_RC_Invalid(Handle).
	elsedef
	  check_HInstance( Handle ).
	enddef

  api_ReadFileSync(Handle,Str,StrLen,RecLen):-
	RC = ReadFileSync(Handle,Str,StrLen,RecLen,0),
	check_RC_Bool(RC).

  api_WriteFileSync(Handle,Str,StrLen,RecLen):-
	RC = WriteFileSync(Handle,Str,StrLen,RecLen,0),
	check_RC_Bool(RC).

  api_CreateThread(Security_attributes,StackSize,StartAdr,Parameter,Flags,ThreadID,Handle):-
	Handle = CreateThread(Security_attributes,StackSize,StartAdr,Parameter,Flags,ThreadID),
ifdef platform_32bit
	check_RC_Null(Handle).
elsedef
	check_HInstance(Handle).
enddef

  api_CreateMutex(Security_attributes,InitialOwner,MutexName,Handle):-
	Handle = CreateMutex(Security_attributes,InitialOwner,MutexName),
ifdef platform_32bit
	check_RC_Null(Handle).
elsedef
	check_HInstance(Handle).
enddef

  api_ReleaseMutex(Handle):-
	RC = ReleaseMutex(Handle),
	check_RC_Bool(RC).

  api_SetDefaultCommConfig(Str,LpCC,Size):-
	RC = SetDefaultCommConfig(Str,LpCC,Size),
	check_RC_Bool(RC).

  api_SetCommState(Handle,DCB):-
	RC = SetCommState(Handle,DCB),
	check_RC_Bool(RC).

  api_GetCommState(Handle,DCB):-
	RC = GetCommState(Handle,DCB),
	check_RC_Bool(RC).

  api_GetCommMask(Handle,Mask):-
	RC = GetCommMask(Handle,Mask),
	check_RC_Bool(RC).

  api_SetCommMask(Handle,Mask):-
	RC = SetCommMask(Handle,Mask),
	check_RC_Bool(RC).

  api_SetCommTimeouts(Handle,Timeouts):-
	RC = SetCommTimeouts(Handle,Timeouts),
	check_RC_Bool(RC).

  api_ClearCommError(Handle,ErrorFlags,CommStat):-
	RC = ClearCommError(Handle,ErrorFlags,CommStat),
	check_RC_Bool(RC).

  api_PurgeComm(Handle,Flags):-
	RC = PurgeComm(Handle,Flags),
	check_RC_Bool(RC).

  api_GetOverlappedResult(Handle,OverlappedStruct,Number,WaitFlag):-
	RC = GetOverlappedResult(Handle,OverlappedStruct,Number,WaitFlag),
	check_RC_Bool(RC).

enddef	% apicomm_pro_included