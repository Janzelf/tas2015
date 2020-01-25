/*****************************************************************************

               Copyright (c) 1984 - 2000 Prolog Development Center A/S

 FileName: DATENEXT.PRO
 Purpose: Date and Time handling predicates
 Written by: Sergey Alexeev
 Build:		012.990119
 Tested with:   Visual Prolog v.5.1 build 555a
 Comments:
******************************************************************************/
clauses
/**global PREDICATE*******************************************************
                             DT_DATE_GET_ERROR
 *************************************************************************/
  dt_date_get_error(ErrCode,ErrStr):-
	dt_parse_error(ErrCode,ErrStr),!.
  dt_date_get_error(0,"No Errors"):-!.

/**global PREDICATE*******************************************************
                             DT_DATESTR_TO_OFFSET/2
 *************************************************************************/
  dt_datestr_to_offset("",0) :- !.
  dt_datestr_to_offset(DateStr,Off):-
	frontstr(2,DateStr,Dat,Rest),
	frontstr(2,Rest,Mo,Year),
	str_int(Dat,D),
	str_int(Mo,M),
	str_int(Year,Y),
	dt_date_to_offset(Y,M,D,Off),!.
  dt_datestr_to_offset(_,0) :-
	errorexit(error_vpi_package_bad_data).

/**global PREDICATE*******************************************************
                             DT_DATESTR_TO_OFFSET/3
 *************************************************************************/
  dt_datestr_to_offset(InString,FormatString,Off):-
	dt_minstr_to_offset(InString,FormatString,Min),
	dt_mins_date_time(Min,Off,_),!.
  dt_datestr_to_offset(_,_,0) :-
	errorexit(error_vpi_package_bad_data).

/**global PREDICATE*******************************************************
                             DT_GET_DATE
 *************************************************************************/
  dt_get_date(DateStr):-
	date(Y,M,D),
	dt_to_2_digit_str(D,D1),
	dt_to_2_digit_str(M,M1),
	dt_to_4_digit_str(Y,Y1),
	format(Datestr,"%s%s%s",D1,M1,Y1).
  dt_get_date(DateStr,FormatString):-	% LJP
	date(Y,M,D),
	dt_date_to_offset(Y,M,D,DATE_OFFSET),
	trap(dt_dateoffset_to_str(DATE_OFFSET,FormatString,DateStr),_,fail),!.
  dt_get_date("",_):-!.


/**global PREDICATE*******************************************************
                             DT_MINS_DATE_TIME
 *************************************************************************/
  dt_mins_date_time(M,D,T):-
	bound(M),!,
	D = M div dt_mins_in_day,
	T = M mod dt_mins_in_day.
  dt_mins_date_time(M,D,T) :-
	bound(D),bound(T),
	M = D * dt_mins_in_day + T.

/**global PREDICATE*******************************************************
                             DT_MINS_DATE_TIME_STR
 *************************************************************************/
  dt_mins_date_time_str(Mins,Date_str,Time_str):-
	bound(Mins),!,
	dt_mins_date_time(Mins,Date,Time),
	dt_dateoffset_to_str(Date,Date_str),
	dt_timeoffset_to_str(Time,Time_str).
  dt_mins_date_time_str(Mins,Date_str,Time_str):-
	bound(Date_str), 
	bound(Time_str),
	dt_datestr_to_offset(Date_str,Date),
	dt_timestr_to_offset(Time_str,Time),
	dt_mins_date_time(Mins,Date,Time),!.

/**global PREDICATE*******************************************************
                             DT_WEEK_YEAR_TO_OFFSET
 *************************************************************************/
predicates
procedure  dt_correct_year(INTEGER Year1,INTEGER M,INTEGER Week,INTEGER Year)
clauses
  dt_week_year_to_offset(0,1,0):-!.
  dt_week_year_to_offset(Week,Year,Offset) :-
	bound(Week), bound(Year),
	dt_date_to_offset(Year,2,1,Off),
	dt_offset_to_date(Off,_,_,W,_,Dw),
	Offset = Off + (Week - W) * 7 -(Dw - 1),!.
  dt_week_year_to_offset(Week,Year,Offset) :-
	bound(Offset),
	dt_offset_to_date(Offset,Year1,M,Week,_,_),
	dt_correct_year(Year1,M,Week,Year),!.

  dt_correct_year(Year1,1,Week,Year):-
	Week > 50,
	Year = Year1 - 1,!.
  dt_correct_year(Year1,Month,1,Year):-
	Month > 1,
	Year = Year1 + 1,!.
  dt_correct_year(Year,_,_,Year).

/**global PREDICATE*******************************************************
                             DT_DATE_MONTH_LIST
 *************************************************************************/
predicates
procedure  dt_set_month_names_def()
procedure  dt_set_month_shortnames_def()
determ dt_set_month_names(SLIST ListOfNames,INTEGER Number)
determ dt_set_month_shortnames(SLIST ListOfNames,INTEGER Number)
determ dt_get_month_names(INTEGER Numb,SLIST List,SLIST ListOfNames)
determ dt_get_month_shortnames(INTEGER Numb,SLIST List,SLIST ListOfNames)

clauses
  dt_date_month_list(ListOfNames):-
	free(ListOfNames),
	dt_get_month_names(12,[],ListOfNames),!.
  dt_date_month_list(ListOfNames):-
	bound(ListOfNames),
	dt_length_of_list(ListOfNames,N),
	N = 12,!,
	dt_set_month_names(ListOfNames,1).
  dt_date_month_list(ListOfNames):-
	bound(ListOfNames),
	dt_set_month_names_def(),!.

/**global PREDICATE*******************************************************
                             DT_DATE_SHORTMONTH_LIST
 *************************************************************************/
  dt_date_shortmonth_list(ListOfNames):-
	free(ListOfNames),
	dt_get_month_shortnames(12,[],ListOfNames),!.
  dt_date_shortmonth_list(ListOfNames):-
	bound(ListOfNames),
	dt_length_of_list(ListOfNames,N),
	N = 12,!,
	dt_set_month_shortnames(ListOfNames,1).
  dt_date_shortmonth_list(ListOfNames):-
	bound(ListOfNames),
	dt_set_month_shortnames_def(),!.

  dt_set_month_names([],_):-!.
  dt_set_month_names([Name|Rest],N):-
	retract(dt_month_name(N,_)),!,
	assert(dt_month_name(N,Name)),
	N1 = N + 1,
	dt_set_month_names(Rest,N1).

  dt_set_month_shortnames([],_):-!.
  dt_set_month_shortnames([Name|Rest],N):-
	retract(dt_month_shortname(N,_)),!,
	assert(dt_month_shortname(N,Name)),
	N1 = N + 1,
	dt_set_month_shortnames(Rest,N1).

  dt_get_month_names(0,ListOfNames,ListOfNames):-!.
  dt_get_month_names(Numb,List,ListOfNames):-
	dt_month_name(Numb,Name),!,
	N = Numb - 1,
	dt_get_month_names(N,[Name|List],ListOfNames).

  dt_get_month_shortnames(0,ListOfNames,ListOfNames):-!.
  dt_get_month_shortnames(Numb,List,ListOfNames):-
	dt_month_shortname(Numb,Name),!,
	N = Numb - 1,
	dt_get_month_shortnames(N,[Name|List],ListOfNames).

  dt_set_month_names_def():-
	retractall(dt_month_name(_,_)),
	assert(dt_month_name(1,dt_month_name_1)),
	assert(dt_month_name(2,dt_month_name_2)),
	assert(dt_month_name(3,dt_month_name_3)),
	assert(dt_month_name(4,dt_month_name_4)),
	assert(dt_month_name(5,dt_month_name_5)),
	assert(dt_month_name(6,dt_month_name_6)),
	assert(dt_month_name(7,dt_month_name_7)),
	assert(dt_month_name(8,dt_month_name_8)),
	assert(dt_month_name(9,dt_month_name_9)),
	assert(dt_month_name(10,dt_month_name_10)),
	assert(dt_month_name(11,dt_month_name_11)),
	assert(dt_month_name(12,dt_month_name_12)).

  dt_set_month_shortnames_def():-
	retractall(dt_month_shortname(_,_)),
	assert(dt_month_shortname(1,dt_month_shortname_1)),
	assert(dt_month_shortname(2,dt_month_shortname_2)),
	assert(dt_month_shortname(3,dt_month_shortname_3)),
	assert(dt_month_shortname(4,dt_month_shortname_4)),
	assert(dt_month_shortname(5,dt_month_shortname_5)),
	assert(dt_month_shortname(6,dt_month_shortname_6)),
	assert(dt_month_shortname(7,dt_month_shortname_7)),
	assert(dt_month_shortname(8,dt_month_shortname_8)),
	assert(dt_month_shortname(9,dt_month_shortname_9)),
	assert(dt_month_shortname(10,dt_month_shortname_10)),
	assert(dt_month_shortname(11,dt_month_shortname_11)),
	assert(dt_month_shortname(12,dt_month_shortname_12)).

/**global PREDICATE*******************************************************
                             DT_DATE_DAY_LIST
 *************************************************************************/
predicates
procedure  dt_set_day_names_def()
procedure  dt_set_day_shortnames_def()
determ dt_set_day_names(SLIST ListOfNames,INTEGER Number)
determ dt_set_day_shortnames(SLIST ListOfNames,INTEGER Number)
determ dt_get_day_names(INTEGER Numb,SLIST List,SLIST ListOfNames)
determ dt_get_day_shortnames(INTEGER Numb,SLIST List,SLIST ListOfNames)

clauses
  dt_date_day_list(ListOfNames):-
	free(ListOfNames),
	dt_get_day_names(7,[],ListOfNames),!.
  dt_date_day_list(ListOfNames):-
	bound(ListOfNames),
	dt_length_of_list(ListOfNames,N),
	N = 7,!,
	dt_set_day_names(ListOfNames,1).
  dt_date_day_list(ListOfNames):-
	bound(ListOfNames),
	dt_set_day_names_def(),!.

/**global PREDICATE*******************************************************
                             DT_DATE_SHORTDAY_LIST
 *************************************************************************/
  dt_date_shortday_list(ListOfNames):-
	free(ListOfNames),
	dt_get_day_shortnames(7,[],ListOfNames),!.
  dt_date_shortday_list(ListOfNames):-
	bound(ListOfNames),
	dt_length_of_list(ListOfNames,N),
	N = 7,!,
	dt_set_day_shortnames(ListOfNames,1).
  dt_date_shortday_list(ListOfNames):-
	bound(ListOfNames),
	dt_set_day_shortnames_def(),!.

  dt_set_day_names([],_):-!.
  dt_set_day_names([Name|Rest],N):-
	retract(dt_day_name(N,_)),!,
	assert(dt_day_name(N,Name)),
	N1 = N + 1,
	dt_set_day_names(Rest,N1).

  dt_set_day_shortnames([],_):-!.
  dt_set_day_shortnames([Name|Rest],N):-
	retract(dt_day_shortname(N,_)),!,
	assert(dt_day_shortname(N,Name)),
	N1 = N + 1,
	dt_set_day_shortnames(Rest,N1).

  dt_get_day_names(0,ListOfNames,ListOfNames):-!.
  dt_get_day_names(Numb,List,ListOfNames):-
	dt_day_name(Numb,Name),!,
	N = Numb - 1,
	dt_get_day_names(N,[Name|List],ListOfNames).

  dt_get_day_shortnames(0,ListOfNames,ListOfNames):-!.
  dt_get_day_shortnames(Numb,List,ListOfNames):-
	dt_day_shortname(Numb,Name),!,
	N = Numb - 1,
	dt_get_day_shortnames(N,[Name|List],ListOfNames).

  dt_set_day_names_def():-
	retractall(dt_day_name(_,_)),
	assert(dt_day_name(1,dt_day_name_1)),
	assert(dt_day_name(2,dt_day_name_2)),
	assert(dt_day_name(3,dt_day_name_3)),
	assert(dt_day_name(4,dt_day_name_4)),
	assert(dt_day_name(5,dt_day_name_5)),
	assert(dt_day_name(6,dt_day_name_6)),
	assert(dt_day_name(7,dt_day_name_7)).

  dt_set_day_shortnames_def():-
	retractall(dt_day_shortname(_,_)),
	assert(dt_day_shortname(1,dt_day_shortname_1)),
	assert(dt_day_shortname(2,dt_day_shortname_2)),
	assert(dt_day_shortname(3,dt_day_shortname_3)),
	assert(dt_day_shortname(4,dt_day_shortname_4)),
	assert(dt_day_shortname(5,dt_day_shortname_5)),
	assert(dt_day_shortname(6,dt_day_shortname_6)),
	assert(dt_day_shortname(7,dt_day_shortname_7)).

/**global PREDICATE*******************************************************
                             DT_TIME_MARKER
 *************************************************************************/
predicates
  procedure dt_set_time_marker_am(STRING TimeMarkerStringAM)
clauses
  dt_set_time_marker_am(""):-
	retract(dt_marker_time(_,TimeMarkerStringPM)),
	assert(dt_marker_time(dt_markertime_am,TimeMarkerStringPM)),!.
  dt_set_time_marker_am(TimeMarkerStringAM):-
	retract(dt_marker_time(_,TimeMarkerStringPM)),
	assert(dt_marker_time(TimeMarkerStringAM,TimeMarkerStringPM)),!.
  dt_set_time_marker_am(_).

predicates
  procedure dt_set_time_marker_pm(STRING TimeMarkerStringAM)
clauses
  dt_set_time_marker_pm(""):-
	retract(dt_marker_time(TimeMarkerStringAM,_)),
	assert(dt_marker_time(TimeMarkerStringAM,dt_markertime_pm)),!.
  dt_set_time_marker_pm(TimeMarkerStringPM):-
	retract(dt_marker_time(TimeMarkerStringAM,_)),
	assert(dt_marker_time(TimeMarkerStringAM,TimeMarkerStringPM)),!.
  dt_set_time_marker_pm(_).

  dt_time_marker(TimeMarkerStringAM,TimeMarkerStringPM):-
	free(TimeMarkerStringAM), free(TimeMarkerStringPM),
	dt_marker_time(TimeMarkerStringAM,TimeMarkerStringPM),!.
  dt_time_marker(TimeMarkerStringAM,TimeMarkerStringPM):-
	bound(TimeMarkerStringAM), bound(TimeMarkerStringPM),
	dt_set_time_marker_am(TimeMarkerStringAM),
	dt_set_time_marker_pm(TimeMarkerStringPM),!.

/**global PREDICATE*******************************************************
                             DT_TIME_SHORTMARKER
 *************************************************************************/
predicates
  procedure dt_set_time_shortmarker_am(STRING TimeMarkerStringAM)
clauses
  dt_set_time_shortmarker_am(""):-
	retract(dt_shortmarker_time(_,TimeMarkerStringPM)),
	assert(dt_shortmarker_time(dt_shortmarkertime_am,TimeMarkerStringPM)),!.
  dt_set_time_shortmarker_am(TimeMarkerStringAM):-
	retract(dt_shortmarker_time(_,TimeMarkerStringPM)),
	assert(dt_shortmarker_time(TimeMarkerStringAM,TimeMarkerStringPM)),!.
  dt_set_time_shortmarker_am(_).

predicates
  procedure dt_set_time_shortmarker_pm(STRING TimeMarkerStringAM)
clauses
  dt_set_time_shortmarker_pm(""):-
	retract(dt_shortmarker_time(TimeMarkerStringAM,_)),
	assert(dt_shortmarker_time(TimeMarkerStringAM,dt_shortmarkertime_pm)),!.
  dt_set_time_shortmarker_pm(TimeMarkerStringPM):-
	retract(dt_shortmarker_time(TimeMarkerStringAM,_)),
	assert(dt_shortmarker_time(TimeMarkerStringAM,TimeMarkerStringPM)),!.
  dt_set_time_shortmarker_pm(_).

  dt_time_shortmarker(TimeMarkerStringAM,TimeMarkerStringPM):-
	free(TimeMarkerStringAM), free(TimeMarkerStringPM),
	dt_shortmarker_time(TimeMarkerStringAM,TimeMarkerStringPM),!.
  dt_time_shortmarker(TimeMarkerStringAM,TimeMarkerStringPM):-
	bound(TimeMarkerStringAM), bound(TimeMarkerStringPM),
	dt_set_time_shortmarker_am(TimeMarkerStringAM),
	dt_set_time_shortmarker_pm(TimeMarkerStringPM),!.

/**global PREDICATE*******************************************************
                             DT_DAY_RELATIVE_FORMAT
 *************************************************************************/
  dt_day_relative_format(MinsBeg):-
	free(MinsBeg),
	dt_relative_par_day(MinsBeg),!.
  dt_day_relative_format(MinsBeg):-
	bound(MinsBeg),!,
	retractall(dt_relative_par_day(_)),
	assert(dt_relative_par_day(MinsBeg)).

/**global PREDICATE*******************************************************
                             DT_WEEK_RELATIVE_FORMAT
 *************************************************************************/
  dt_week_relative_format(MinsBeg):-
	free(MinsBeg),
	dt_relative_par_week(MinsBeg),!.
  dt_week_relative_format(MinsBeg):-
	bound(MinsBeg),!,
	retractall(dt_relative_par_week(_)),
	assert(dt_relative_par_week(MinsBeg)).

/**global PREDICATE*******************************************************
                             DT_DATE_RELATIVE_FORMAT
 *************************************************************************/
predicates
procedure  dt_set_relative_def()

clauses
  dt_date_relative_format(MinsBeg,MinsStep):-
	free(MinsBeg), free(MinsStep),
	dt_relative_par(MinsBeg,MinsStep),!.
  dt_date_relative_format(MinsBeg,MinsStep):-
	bound(MinsBeg), bound(MinsStep), MinsBeg = 0, MinsStep = 0,!,
	dt_set_relative_def().
  dt_date_relative_format(MinsBeg,MinsStep):-
	bound(MinsBeg), bound(MinsStep),!,
	retractall(dt_relative_par(_,_)),
	assert(dt_relative_par(MinsBeg,MinsStep)).

  dt_set_relative_def():-
	retractall(dt_relative_par(_,_)),!,
	assert(dt_relative_par(0,dt_mins_in_day)),
	retractall(dt_relative_txt(_,_)).

/**global PREDICATE*******************************************************
                             DT_DATE_RELATIVE_LIST
 *************************************************************************/
  dt_date_relative_list(NamesList):-
	free(NamesList),
	dt_relative_txt(NamesList,_),!.
  dt_date_relative_list(NamesList):-
	free(NamesList),
	NamesList = [],!.
  dt_date_relative_list(NamesList):-
	bound(NamesList),
	retractall(dt_relative_txt(_,_)),
	dt_length_of_list(NamesList,AmountNames),
	AmountNames > 0,!,
	assert(dt_relative_txt(NamesList,AmountNames)).
  dt_date_relative_list(NamesList):-
	bound(NamesList),!.

/**global PREDICATE*******************************************************
                             DT_DATE_FIRSTDAYOFWEEK
 *************************************************************************/
  dt_date_firstdayofweek(FirstDayOfWeek):-
	free(FirstDayOfWeek),
	dt_firstdayofweek(FirstDayOfWeek),!.
  dt_date_firstdayofweek(FirstDayOfWeek):-
	bound(FirstDayOfWeek),
	1 <= FirstDayOfWeek, FirstDayOfWeek <=7,!,
	retractall(dt_firstdayofweek(_)),
	assert(dt_firstdayofweek(FirstDayOfWeek)).
  dt_date_firstdayofweek(FirstDayOfWeek):-
	bound(FirstDayOfWeek),!.

/**global PREDICATE*******************************************************
                             DT_LEGAL_DATE
 *************************************************************************/
predicates
determ dt_get_days(INTEGER,INTEGER,INTEGER)

clauses
  dt_legal_date(Date,Dateformat):-		% LJP
	trap(dt_datestr_to_offset(Date,Dateformat,_),_,fail),!.

  dt_legal_date(Date):-
	retractall(dt_parse_error(_,_)),
	str_len(Date,8),
	frontstr(2,Date,Ds,Rest),
	frontstr(2,Rest,Ms,Ys),
	str_int(Ys,Y),
	str_int(Ms,M),
	str_int(Ds,D),
	dt_get_days(Y,M,D),!.
  dt_legal_date(_):-
	retractall(dt_parse_error(_,_)),
	assert(dt_parse_error(14209,"Illegal source string")),!,
	fail.

  dt_get_days(Y,M,D):-
	Dm1 = dt_day_in_month(Y,M),
	1 <= D, D <= Dm1,!.

/**global PREDICATE*******************************************************
                             DT_ADD_MONTH
 *************************************************************************/
predicates
procedure  dt_add_mo1(INTEGER,INTEGER,INTEGER,INTEGER,INTEGER)  

clauses
  dt_add_month(DATESTR,MO,New_Datestr):-
     frontstr(2,DATESTR,Day,REST),
     frontstr(2,REST,Mon,Year),
     str_int(Mon,M),
     str_int(Year,Y),
     dt_add_mo1(M,MO,Y,M1,Y1),
     dt_to_2_digit_str(M1,M2),
     dt_to_4_digit_str(Y1,Y2),
     format(New_datestr,"%s%s%s",Day,M2,Y2),!.
  dt_add_month(_,_,""):-
     	errorexit(error_vpi_package_bad_data).

  dt_add_mo1(M,MO,Y,M1,Y1):-
     Y1 = Y + ((M + MO) div 12),
     M1 = (M + MO) mod 12, M1 > 0,!.
  dt_add_mo1(M,MO,Y,12,Y1):-
     Y1 = Y + ((M + MO) div 12),!.
       

/**global PREDICATE*******************************************************
                      DT_DATE_SET_TWO_DIGIT_START_YEAR
 *************************************************************************/
  dt_date_2digit_start_year(StartYear):-
	free(StartYear),
	dt_two_digit_start_year(StartYear),!.
  dt_date_2digit_start_year(StartYear):-
	free(StartYear), StartYear = 0,!,
	errorexit(error_vpi_package_bad_data). % ...else start year value is illegal.
  dt_date_2digit_start_year(StartYear):-
	bound(StartYear),
	StartYear < 100,!,  % Start year value is between 0 and 99 ...
	retractall(dt_two_digit_start_year(_)),
	assert(dt_two_digit_start_year(StartYear)).
  dt_date_2digit_start_year(StartYear):-
	bound(StartYear),!,
	errorexit(error_vpi_package_bad_data),!. % ...else start year value is illegal.

/**global PREDICATE*******************************************************
                      DT_WIN_TO_STR
 *************************************************************************/
predicates
  determ dt_convert_command(STRING WinCommand,STRING DateCommand)
clauses
  dt_convert_command("dddd","%DN"):-!.
  dt_convert_command("ddd","%Dn"):-!.
  dt_convert_command("dd","%DD"):-!.
  dt_convert_command("d","%Dd"):-!.
  dt_convert_command("MMMM","%MN"):-!.
  dt_convert_command("MMM","%Mn"):-!.
  dt_convert_command("MM","%MD"):-!.
  dt_convert_command("M","%Md"):-!.
  dt_convert_command("yyyy","%YL"):-!.
  dt_convert_command("yy","%YS"):-!.
  dt_convert_command("y","%Ys"):-!.
  dt_convert_command("mm","%MM"):-!.
  dt_convert_command("m","%Mm"):-!.
  dt_convert_command("HH","%HH"):-!.
  dt_convert_command("H","%Hh"):-!.
  dt_convert_command("hh","%hH"):-!.
  dt_convert_command("h","%hh"):-!.
  dt_convert_command("tt","%TT"):-!.
  dt_convert_command("t","%Tt"):-!.

predicates
  determ dt_search_all_command(STRING WinCommandString,SLIST PossibleCommands,INTEGER CurrPos,
	STRING CurrCommand,STRING Command)
clauses
  dt_search_all_command(_,[],10000,_,_):-!,fail.
  dt_search_all_command(_,[],_,Command,Command):-!.
  dt_search_all_command(WinCommandString,[Command|Rest],CurrPos,_,OutCommand):-
	searchstring(WinCommandString,Command,FoundPos),
	FoundPos < CurrPos,!,
	dt_search_all_command(WinCommandString,Rest,FoundPos,Command,OutCommand).
  dt_search_all_command(WinCommandString,[_|Rest],CurrPos,Command,OutCommand):-!,
	dt_search_all_command(WinCommandString,Rest,CurrPos,Command,OutCommand).

predicates
  procedure dt_search_win_command(STRING WinCommandString,STRING DateCommand,STRING RestString)
clauses
  dt_search_win_command(WinCommandString,DateCommandString,RestString):-
	dt_search_all_command(WinCommandString,["dddd","ddd","dd","d","MMMM","MMM","MM","M","yyyy","yy","y","mm","m","HH","H","hh","h","tt","t"],
		10000,"",Command),
	searchstring(WinCommandString,Command,Pos),
	str_len(Command,Length),
	dt_sub_string_my(Pos,Length,WinCommandString,BlankString,WinCommand,RestString),
	dt_convert_command(WinCommand,DateCommand),
	concat(BlankString,DateCommand,DateCommandString),!.
  dt_search_win_command(WinCommandString,WinCommandString,"").

predicates
  procedure dt_get_command(STRING WinCommandString,STRING,STRING DateCommandString)
clauses
  dt_get_command("",DateCommandString,DateCommandString):-!.
  dt_get_command(WinCommandString,CurrString,DateCommandString):-
	dt_search_win_command(WinCommandString,DateCommand,RestString),
	concat(CurrString,DateCommand,CurrString1),!,
	dt_get_command(RestString,CurrString1,DateCommandString).
  dt_get_command(WinCommandString,CurrString,DateCommandString):-
	concat(CurrString,WinCommandString,DateCommandString),!.

predicates
  procedure dt_comment_string(STRING WinFormatString,STRING,STRING DateFormatString)
clauses
  dt_comment_string("","",""):-!.
  dt_comment_string(WinFormatString,Comment,DateFormatString):-
	searchchar(WinFormatString,'\'',Pos),
	str_len(WinFormatString,Length),
	Length > Pos,
	Pos1 = Pos + 1,
	subchar(WinFormatString,Pos1,Char),
	Char = '\'',
	dt_sub_string_my(Pos,2,WinFormatString,Comment1,_Separator,WinFormatString1),
	dt_comment_string(WinFormatString1,Comment2,DateFormatString),
	concat(Comment1,"\'",Comment11),
	concat(Comment11,Comment2,Comment),!.
  dt_comment_string(WinFormatString,Comment,DateFormatString):-
	searchchar(WinFormatString,'\'',Pos),
	dt_sub_string_my(Pos,1,WinFormatString,Comment,_Separator,DateFormatString),!.
  dt_comment_string(Comment,Comment,"").

predicates
  procedure dt_command_string(STRING WinFormatString,STRING,STRING DateFormatString)
clauses
  dt_command_string("",DateFormatString,DateFormatString):-!.
  dt_command_string(WinFormatString,CurrStr,DateFormatString):-
	searchchar(WinFormatString,'\'',Pos),
	dt_sub_string_my(Pos,1,WinFormatString,CommandString,_Separator,CommentString),
	dt_get_command(CommandString,"",OutputCommand),
	concat(CurrStr,OutputCommand,CurrStr1),
	dt_comment_string(CommentString,OutputComment,RestString),
	concat(CurrStr1,OutputComment,CurrStr2),!,
	dt_command_string(RestString,CurrStr2,DateFormatString).
  dt_command_string(CommandString,CurrStr,DateFormatString):-
	dt_get_command(CommandString,"",OutputCommand),
	concat(CurrStr,OutputCommand,DateFormatString),!.

  dt_win_to_str(WinFormatString,DateFormatString):-
	dt_command_string(WinFormatString,"",DateFormatString),!.
