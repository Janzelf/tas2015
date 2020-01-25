/*****************************************************************************

               Copyright (c) 1984 - 2000 Prolog Development Center A/S

 FileName: DATEMAIN.PRO
 Purpose: Date and Time handling predicates
 Written by: Sergey Alexeev
 Build:		012.990119
 Tested with:   Visual Prolog v.5.1 build 555a
 Comments:
******************************************************************************/
ifndef date_pre_included
 #Error: this file requires include: date.pre
enddef 


database - date_db
  dt_month_name(INTEGER Month,STRING Name)
  dt_month_shortname(INTEGER Month,STRING Name)
  dt_day_name(INTEGER Day,STRING Name)
  dt_day_shortname(INTEGER Day,STRING Name)
  dt_marker_time(STRING MarkerNameAM,STRING MarkerNamePM)
  dt_shortmarker_time(STRING MarkerNameAM,STRING MarkerNamePM)
  dt_relative_par(MINUT_OFFSET MinBeg,MINUT_OFFSET MinStep)
  dt_relative_par_day(MINUT_OFFSET MinBeg)
  dt_relative_par_week(MINUT_OFFSET MinBeg)
  dt_relative_txt(SLIST Names,INTEGER AmountNames)
  dt_firstdayofweek(INTEGER FirstDayOfWeek)
  determ dt_full_date(INTEGER Y,INTEGER M,INTEGER W,INTEGER Dm,INTEGER Dw,INTEGER Hour,INTEGER Mins,INTEGER H12,INTEGER AM_PM)
  determ dt_parse_error(INTEGER ErrCode,STRING ErrStr)
  determ dt_two_digit_start_year(USHORT)

clauses
  dt_month_name(1,dt_month_name_1).
  dt_month_name(2,dt_month_name_2).
  dt_month_name(3,dt_month_name_3).
  dt_month_name(4,dt_month_name_4).
  dt_month_name(5,dt_month_name_5).
  dt_month_name(6,dt_month_name_6).
  dt_month_name(7,dt_month_name_7).
  dt_month_name(8,dt_month_name_8).
  dt_month_name(9,dt_month_name_9).
  dt_month_name(10,dt_month_name_10).
  dt_month_name(11,dt_month_name_11).
  dt_month_name(12,dt_month_name_12).
  dt_month_shortname(1,dt_month_shortname_1).
  dt_month_shortname(2,dt_month_shortname_2).
  dt_month_shortname(3,dt_month_shortname_3).
  dt_month_shortname(4,dt_month_shortname_4).
  dt_month_shortname(5,dt_month_shortname_5).
  dt_month_shortname(6,dt_month_shortname_6).
  dt_month_shortname(7,dt_month_shortname_7).
  dt_month_shortname(8,dt_month_shortname_8).
  dt_month_shortname(9,dt_month_shortname_9).
  dt_month_shortname(10,dt_month_shortname_10).
  dt_month_shortname(11,dt_month_shortname_11).
  dt_month_shortname(12,dt_month_shortname_12).
  dt_day_name(1,dt_day_name_1).
  dt_day_name(2,dt_day_name_2).
  dt_day_name(3,dt_day_name_3).
  dt_day_name(4,dt_day_name_4).
  dt_day_name(5,dt_day_name_5).
  dt_day_name(6,dt_day_name_6).
  dt_day_name(7,dt_day_name_7).
  dt_day_shortname(1,dt_day_shortname_1).
  dt_day_shortname(2,dt_day_shortname_2).
  dt_day_shortname(3,dt_day_shortname_3).
  dt_day_shortname(4,dt_day_shortname_4).
  dt_day_shortname(5,dt_day_shortname_5).
  dt_day_shortname(6,dt_day_shortname_6).
  dt_day_shortname(7,dt_day_shortname_7).
  dt_marker_time(dt_markertime_am,dt_markertime_pm).
  dt_shortmarker_time(dt_shortmarkertime_am,dt_shortmarkertime_pm).
  dt_relative_par(dt_mins_in_day,dt_mins_in_day).
  dt_relative_par_day(dt_mins_in_day).
  dt_relative_par_week(dt_mins_in_day).
  dt_firstdayofweek(1).
  dt_two_digit_start_year(dt_date_two_digit_start_year).

%*************************** TOOLS predicates ****************************
predicates
procedure   dt_minimum_of3longs(LONG,LONG,LONG)
determ      dt_member_of_list(STRING,SLIST,UNSIGNED)
procedure   dt_length_of_list(SLIST,INTEGER)
clauses
  dt_minimum_of3longs(A,B,A) :- A <= B, !.
  dt_minimum_of3longs(_,A,A).

  dt_member_of_list(S,[S|_],1):-!.
  dt_member_of_list(S,[_|L],M):-
        bound(M),!,
        N = M - 1,
        dt_member_of_list(S,L,N),!.
  dt_member_of_list(S,[_|L],M):-
        dt_member_of_list(S,L,N),!,
        M = N + 1.

  dt_length_of_list([],0):-!.
  dt_length_of_list([_|T],N1):-
	dt_length_of_list(T,N),
	N1=N+1,!.
  dt_length_of_list(_,0):-errorexit().

/**global PREDICATE*******************************************************
                             DT_OFFSET_TO_TIME
 *************************************************************************/
  dt_offset_to_time(Time,H,Min):-
	H = Time div dt_mins_in_hour,
	Min = Time mod dt_mins_in_hour,!.
  dt_offset_to_time(_,0,0):-!.

/**global PREDICATE*******************************************************
                             DT_OFFSET_TO_MIN/8
 *************************************************************************/
  dt_offset_to_min(Mins,Y,M,W,Dm,Dw,H,Min):-
	dt_mins_date_time(Mins,Date,Time),
	dt_offset_to_date(Date,Y,M,W,Dm,Dw),
	H = Time div dt_mins_in_hour,
	Min = Time mod dt_mins_in_hour,!.
  dt_offset_to_min(_,0,0,0,0,0,0,0).
	
/**global PREDICATE*******************************************************
                             DT_OFFSET_TO_MIN/9
 *************************************************************************/
predicates
  procedure dt_dec_minute(MINUT_OFFSET MinsX,DT_START_END Mode,MINUT_OFFSET Mins)
clauses
  dt_dec_minute(MinsX,end,Mins):-
	DayBorder = Minsx mod dt_mins_in_day,
	DayBorder = 0,!,
	Mins = MinsX - 1.
  dt_dec_minute(Mins,_,Mins).

predicates
  procedure dt_inc_minute(MINUT_OFFSET MinsX,DT_START_END Mode,INTEGER HX,INTEGER MinX,INTEGER H,INTEGER Min)
clauses
  dt_inc_minute(MinsX,end,_,_,24,0):-
	DayBorder = Minsx mod dt_mins_in_day,
	DayBorder = 0,!.
  dt_inc_minute(_,_,H,Min,H,Min).

  dt_offset_to_min(MinsX,Mode,Y,M,W,Dm,Dw,H,Min):-
	dt_dec_minute(MinsX,Mode,Mins),
	dt_mins_date_time(Mins,Date,Time),
	dt_offset_to_date(Date,Y,M,W,Dm,Dw),
	HX = Time div dt_mins_in_hour,
	MinX = Time mod dt_mins_in_hour,
	dt_inc_minute(MinsX,Mode,HX,MinX,H,Min),!.
  dt_offset_to_min(_,_,0,0,0,0,0,0,0).
	
/**global PREDICATE*******************************************************
                             DT_OFFSET_TO_DATE
 *************************************************************************/
predicates
procedure  dt_cal_year(DATE_OFFSET Days,DATE_OFFSET Days1,INTEGER Y,BOOLEAN L)
procedure  dt_cal_large(INTEGER Year,BOOLEAN)
procedure  dt_cal_month(DATE_OFFSET Days,BOOLEAN L,INTEGER M,INTEGER Dm)
procedure  dt_cal_week(DATE_OFFSET Days,INTEGER Y,INTEGER W)
determ     dt_cal_dayofweek(DATE_OFFSET Days,INTEGER Dw)
determ     dt_get_beginweek_days(INTEGER Y,DATE_OFFSET DaysFirst)

clauses
  dt_offset_to_date(0,1,1,0,0,0):-!.
  dt_offset_to_date(Days0,Y,M,W,Dm,Dw):-
	Days = Days0 - 1,
	dt_cal_year(Days,Days1,Y,L),
	dt_cal_month(Days1,L,M,Dm),
	dt_cal_week(Days,Y,W),
	dt_cal_dayofweek(Days,Dw),!.
  dt_offset_to_date(_,1,1,0,0,0):-!.

PREDICATES
procedure dt_correct_new_400_years(DATE_OFFSET _Y400,DATE_OFFSET Y100X,DATE_OFFSET _Y100X,DATE_OFFSET Y100,DATE_OFFSET _Y100)
CLAUSES
  dt_correct_new_400_years(_Y400,Y100X,0,Y100,dt_day_in_100_year):-
	_Y400 = dt_day_in_400_year - 1,!,
	Y100 = Y100X - 1.
  dt_correct_new_400_years(_,Y100,_Y100,Y100,_Y100).

  dt_cal_year(Days,Days1,Y,Large):-
	Y400   = Days div dt_day_in_400_year,
	_Y400  = Days mod dt_day_in_400_year,
	Y100X  = _Y400 div dt_day_in_100_year,
	_Y100X = _Y400 mod dt_day_in_100_year,
	dt_correct_new_400_years(_Y400,Y100X,_Y100X,Y100,_Y100),
	Y4    = _Y100 div dt_day_in_4_year,
	_Y4   = _Y100 mod dt_day_in_4_year,
	Y1_   = _Y4 div dt_day_in_year,
	dt_minimum_of3longs(Y1_,3,Y1),
	Days1   = _Y4 - (dt_day_in_year * Y1),
	Y = (Y400 * 400) + (Y100 * 100) + (Y4 * 4) + Y1 + 1,
	dt_cal_large(Y,Large),!.

  dt_cal_large(Y,b_true):-
	A = Y mod 400,
	A = 0,!.
  dt_cal_large(Y,b_false):-
	A = Y mod 100,
	A = 0,!.
  dt_cal_large(Y,b_true):-
	A = Y mod 4,
	A = 0,!.
  dt_cal_large(_,b_false).
	
  dt_cal_month(Days,_,1,Dm):-
	Days < dt_day_to_jan,
	Dm = Days + 1,!.
  dt_cal_month(Days,b_false,2,Dm):-
	Days < dt_day_to_feb,
	Dm = (Days - dt_day_to_jan) + 1,!.
  dt_cal_month(Days,b_false,3,Dm):-
	Days < dt_day_to_mar,
	Dm = (Days - dt_day_to_feb) + 1,!.
  dt_cal_month(Days,b_false,4,Dm):-
	Days < dt_day_to_apr,
	Dm = (Days - dt_day_to_mar) + 1,!.
  dt_cal_month(Days,b_false,5,Dm):-
	Days < dt_day_to_may,
	Dm = (Days - dt_day_to_apr) + 1,!.
  dt_cal_month(Days,b_false,6,Dm):-
	Days < dt_day_to_jun,
	Dm = (Days - dt_day_to_may) + 1,!.
  dt_cal_month(Days,b_false,7,Dm):-
	Days < dt_day_to_jul,
	Dm = (Days - dt_day_to_jun) + 1,!.
  dt_cal_month(Days,b_false,8,Dm):-
	Days < dt_day_to_aug,
	Dm = (Days - dt_day_to_jul) + 1,!.
  dt_cal_month(Days,b_false,9,Dm):-
	Days < dt_day_to_sep,
	Dm = (Days - dt_day_to_aug) + 1,!.
  dt_cal_month(Days,b_false,10,Dm):-
	Days < dt_day_to_oct,
	Dm = (Days - dt_day_to_sep) + 1,!.
  dt_cal_month(Days,b_false,11,Dm):-
	Days < dt_day_to_nov,
	Dm = (Days - dt_day_to_oct) + 1,!.
  dt_cal_month(Days,b_false,12,Dm):-
	Dm = (Days - dt_day_to_nov) + 1,!.
  dt_cal_month(Days,_,2,Dm):-
	Days < dt_day_to_feb + 1,
	Dm = Days - dt_day_to_jan + 1,!.
  dt_cal_month(Days,_,3,Dm):-
	Days < dt_day_to_mar + 1,
	Dm = Days - dt_day_to_feb,!.
  dt_cal_month(Days,_,4,Dm):-
	Days < dt_day_to_apr + 1,
	Dm = Days - dt_day_to_mar,!.
  dt_cal_month(Days,_,5,Dm):-
	Days < dt_day_to_may + 1,
	Dm = Days - dt_day_to_apr,!.
  dt_cal_month(Days,_,6,Dm):-
	Days < dt_day_to_jun + 1,
	Dm = Days - dt_day_to_may,!.
  dt_cal_month(Days,_,7,Dm):-
	Days < dt_day_to_jul + 1,
	Dm = Days - dt_day_to_jun,!.
  dt_cal_month(Days,_,8,Dm):-
	Days < dt_day_to_aug + 1,
	Dm = Days - dt_day_to_jul,!.
  dt_cal_month(Days,_,9,Dm):-
	Days < dt_day_to_sep + 1,
	Dm = Days - dt_day_to_aug,!.
  dt_cal_month(Days,_,10,Dm):-
	Days < dt_day_to_oct + 1,
	Dm = Days - dt_day_to_sep,!.
  dt_cal_month(Days,_,11,Dm):-
	Days < dt_day_to_nov + 1,
	Dm = Days - dt_day_to_oct,!.
  dt_cal_month(Days,_,12,Dm):-
	Dm = Days - dt_day_to_nov,!.

  dt_cal_week(DaysFull,Y,W):-
	dt_get_beginweek_days(Y,DaysFirst),
	Y1 = Y + 1,
	dt_get_beginweek_days(Y1,DaysLast),
	DaysFirst <= DaysFull, DaysFull < DaysLast,
	W = ((DaysFull - DaysFirst) div dt_day_in_week) + 1,!.
  dt_cal_week(DaysFull,Y,W):-
	dt_get_beginweek_days(Y,DaysFirst),
	DaysFirst > DaysFull,!,
	DaysFull1 = DaysFirst - 1, Y1 = Y - 1,
	dt_cal_week(DaysFull1,Y1,W).
  dt_cal_week(_,_,1).

  dt_get_beginweek_days(Y,DaysFirst):-
	dt_date_to_offset(Y,1,1,Days1jan0),
	Days1jan = Days1jan0 - 1,
	dt_cal_dayofweek(Days1jan,Dw1jan),
	Dw1jan > 4,
	DaysFirst = Days1jan + (dt_day_in_week - Dw1jan) + 1,!.
  dt_get_beginweek_days(Y,DaysFirst):-
	dt_date_to_offset(Y,1,1,Days1jan0),
	Days1jan = Days1jan0 - 1,
	dt_cal_dayofweek(Days1jan,Dw1jan),
	DaysFirst = (Days1jan - Dw1jan) + 1,!.

  dt_cal_dayofweek(Days,Dw):-
	dt_firstdayofweek(FirstDayOfWeek),!,
	Days1 =  Days - FirstDayOfWeek + 1,
	Dw = (Days1 mod dt_day_in_week) + 1.
/**global PREDICATE*******************************************************
                             DT_TIME_TO_OFFSET
 *************************************************************************/
  dt_time_to_offset(H,Min,TimeOffset):-
	TimeOffset = H * dt_mins_in_hour + Min.

/**global PREDICATE*******************************************************
                             DT_MIN_TO_OFFSET
 *************************************************************************/
  dt_min_to_offset(Y,M,D,H,Min,MinOffset):-
	dt_date_to_offset(Y,M,D,DayOffset),
	MinOffset = DayOffset * dt_mins_in_day + H * dt_mins_in_hour + Min.

/**global PREDICATE*******************************************************
                             DT_DATE_TO_OFFSET
 *************************************************************************/
predicates
determ dt_get_day(INTEGER Month,INTEGER Days)
determ dt_get_day1(INTEGER Month,INTEGER Days)
determ dt_adjust_date(INTEGER Y,INTEGER M,INTEGER D,INTEGER Y1,INTEGER M1,INTEGER D1)
clauses
  dt_date_to_offset(1,1,0,0):-!.
  dt_date_to_offset(Y1,M1,D1,DayOffset):-
	dt_adjust_date(Y1,M1,D1,Y,M,D),
	dt_cal_large(Y1,Leap),
	Leap = b_true,
	Leap_400 = Y div 400,
	Leap_100 = Y div 100,
	Leap_4   = Y div 4,
	dt_get_day1(M,DayInFullMonth),
	Ylong = cast(long,Y),
	DayOffset = (Ylong * dt_day_in_year) + Leap_4 - Leap_100 + Leap_400 + DayInFullMonth + D + 1,!.
  dt_date_to_offset(Y1,M1,D1,DayOffset):-
	dt_adjust_date(Y1,M1,D1,Y,M,D),
	Leap_400 = Y div 400,
	Leap_100 = Y div 100,
	Leap_4   = Y div 4,
	dt_get_day(M,DayInFullMonth),
	Ylong = cast(long,Y),
	DayOffset = (Ylong * dt_day_in_year) + Leap_4 - Leap_100 + Leap_400 + DayInFullMonth + D + 1,!.
  dt_date_to_offset(_,_,_,0):-!.

  dt_get_day(0,0):-!.
  dt_get_day(1,31):-!.
  dt_get_day(2,59):-!.
  dt_get_day(3,90):-!.
  dt_get_day(4,120):-!.
  dt_get_day(5,151):-!.
  dt_get_day(6,181):-!.
  dt_get_day(7,212):-!.
  dt_get_day(8,243):-!.
  dt_get_day(9,273):-!.
  dt_get_day(10,304):-!.
  dt_get_day(11,334):-!.
  dt_get_day(12,365).
  dt_get_day1(0,0):-!.
  dt_get_day1(1,31):-!.
  dt_get_day1(2,60):-!.
  dt_get_day1(3,91):-!.
  dt_get_day1(4,121):-!.
  dt_get_day1(5,152):-!.
  dt_get_day1(6,182):-!.
  dt_get_day1(7,213):-!.
  dt_get_day1(8,244):-!.
  dt_get_day1(9,274):-!.
  dt_get_day1(10,305):-!.
  dt_get_day1(11,335):-!.
  dt_get_day1(12,366).

  dt_adjust_date(Y,M,D,Y1,M1,D1):-
	1 <= M, M <=12,!,
	Y1 = Y - 1, M1 = M - 1, D1 = D - 1,!.
  dt_adjust_date(Y,M,D,Y1,M1,D1):-
	M < 1,!,
	M2 = 0 - M,
	Y1 = Y - ((M2 + 12) div 12) - 1, M1 = 12 - (M2 mod 12) - 1, D1 = D - 1,!.
  dt_adjust_date(Y,M,D,Y1,M1,D1):-
	M > 12,!,
	Y1 = Y + (M div 12) - 1, M1 = (M mod 12) - 1, D1 = D - 1,!.

/**global PREDICATE*******************************************************
                             DT_TIMEOFFSET_TO_STR\2
 *************************************************************************/
predicates
procedure  dt_to_2_digit_str(INTEGER,STRING)
procedure  dt_to_4_digit_str(INTEGER,STRING)
clauses
  dt_timeoffset_to_str(Off,TimeStr):-
    dt_offset_to_time(Off,H,M),
    dt_to_2_digit_str(H,H1),
    dt_to_2_digit_str(M,M1),
    format(TimeStr,"%s%s",H1,M1).

  dt_to_2_digit_str(I,S) :-
    I < 10,!,
    str_int(S1,I),
    concat("0",S1,S).
  dt_to_2_digit_str(I,S) :-
    str_int(S,I).

  dt_to_4_digit_str(I,S) :-
    I < 10,!,
    str_int(S1,I),
    concat("000",S1,S).
  dt_to_4_digit_str(I,S) :-
    I < 100,!,
    str_int(S1,I),
    concat("00",S1,S).
  dt_to_4_digit_str(I,S) :-
    I < 1000,!,
    str_int(S1,I),
    concat("0",S1,S).
  dt_to_4_digit_str(I,S) :-
    str_int(S,I).

/**global PREDICATE*******************************************************
                             DT_TIMEOFFSET_TO_STR\3
 *************************************************************************/
  dt_timeoffset_to_str(Off,FormatString,OutString):-
	dt_mins_date_time(Off1,0,Off),
	dt_minoffset_to_str(Off1,FormatString,OutString),!.
  dt_timeoffset_to_str(_,_,""):-!.

/**global PREDICATE*******************************************************
                             DT_MINOFFSET_TO_STR\2
 *************************************************************************/
  dt_minoffset_to_str(0,"            "):-!.
  dt_minoffset_to_str(Off,MinStr):-
    dt_offset_to_min(Off,Y,M,_,D,_,H,Mi),
    dt_to_2_digit_str(D,D1),
    dt_to_2_digit_str(M,M1),
    dt_to_4_digit_str(Y,Y1),
    dt_to_2_digit_str(H,H1),
    dt_to_2_digit_str(Mi,Mi1),
    format(MinStr,"%s%s%s%s%s",D1,M1,Y1,H1,Mi1),!.
  
/**global PREDICATE*******************************************************
                             DT_MINOFFSET_TO_STR\3
 *************************************************************************/
predicates
procedure  dt_ins_YS0(STRING FormatString,INTEGER Y,STRING OutStr)
procedure  dt_ins_Ys(STRING FormatString,INTEGER Y,STRING OutStr)
procedure  dt_ins_YL(STRING FormatString,INTEGER Y,STRING OutStr)
procedure  dt_ins_MD0(STRING FormatString,INTEGER M,STRING OutStr)
procedure  dt_ins_Md(STRING FormatString,INTEGER M,STRING OutStr)
determ  dt_ins_MN0(STRING FormatString,INTEGER M,STRING OutStr)
determ  dt_ins_Mn(STRING FormatString,INTEGER M,STRING OutStr)
procedure  dt_ins_WD0(STRING FormatString,INTEGER W,STRING OutStr)
procedure  dt_ins_Wd(STRING FormatString,INTEGER W,STRING OutStr)
procedure  dt_ins_DD0(STRING FormatString,INTEGER Dm,STRING OutStr)
procedure  dt_ins_Dd(STRING FormatString,INTEGER Dm,STRING OutStr)
determ  dt_ins_DN0(STRING FormatString,INTEGER Dw,STRING OutStr)
determ  dt_ins_Dn(STRING FormatString,INTEGER Dw,STRING OutStr)
procedure  dt_ins_HH0(STRING FormatString,INTEGER H,STRING OutStr)
procedure  dt_ins_Hh(STRING FormatString,INTEGER H,STRING OutStr)
procedure  dt_ins_HHH0(STRING FormatString,INTEGER H,STRING OutStr)
procedure  dt_ins_Hhh(STRING FormatString,INTEGER H,STRING OutStr)
procedure  dt_ins_MM0(STRING FormatString,INTEGER Min,STRING OutStr)
procedure  dt_ins_Mm(STRING FormatString,INTEGER Min,STRING OutStr)
determ  dt_ins_TT0(STRING FormatString,INTEGER AP,STRING OutStr)
determ  dt_ins_Tt(STRING FormatString,INTEGER AP,STRING OutStr)
determ  dt_ins_DR(STRING FormatString,MINUT_OFFSET Mins,STRING OutStr)
determ  dt_ins_AR(STRING FormatString,MINUT_OFFSET Mins,STRING OutStr)
determ  dt_ins_WR(STRING FormatString,MINUT_OFFSET Mins,STRING OutStr)
procedure  dt_ins_string(STRING SourceString,INTEGER Pos,STRING InsStr,STRING OutStr)
determ dt_get_dr_txt(LONG Dr,STRING Names)
procedure dt_get_12hour(INTEGER H,INTEGER Out)

clauses
  dt_minoffset_to_str(Off,FormatString,OutString):-
	dt_offset_to_min(Off,Y,M,W,Dm,Dw,H,Min),
	dt_get_12hour(H,H12),
	AP = H div 12,
	dt_ins_YS0(FormatString,Y,OutStr1),
	dt_ins_Ys(OutStr1,Y,OutStr2),
	dt_ins_YL(OutStr2,Y,OutStr3),
	dt_ins_MD0(OutStr3,M,OutStr4),
	dt_ins_Md(OutStr4,M,OutStr5),
	dt_ins_MN0(OutStr5,M,OutStr6),
	dt_ins_Mn(OutStr6,M,OutStr7),
	dt_ins_WD0(OutStr7,W,OutStr8),
	dt_ins_Wd(OutStr8,W,OutStr9),
	dt_ins_DD0(OutStr9,Dm,OutStr10),
	dt_ins_Dd(OutStr10,Dm,OutStr11),
	dt_ins_DN0(OutStr11,Dw,OutStr12),
	dt_ins_Dn(OutStr12,Dw,OutStr13),
	dt_ins_HH0(OutStr13,H,OutStr14),
	dt_ins_Hh(OutStr14,H,OutStr15),
	dt_ins_HHH0(OutStr15,H12,OutStr16),
	dt_ins_Hhh(OutStr16,H12,OutStr17),
	dt_ins_MM0(OutStr17,Min,OutStr18),
	dt_ins_Mm(OutStr18,Min,OutStr19),
	dt_ins_DR(OutStr19,Off,OutStr20),
	dt_ins_AR(OutStr20,Off,OutStr21),
	dt_ins_WR(OutStr21,Off,OutStr22),
	dt_ins_TT0(OutStr22,AP,OutStr23),
	dt_ins_Tt(OutStr23,AP,OutString),!.
  dt_minoffset_to_str(_,_,""):-!.

  dt_minoffset_to_str(Off,Mode,FormatString,OutString):-
	dt_offset_to_min(Off,Mode,Y,M,W,Dm,Dw,H,Min),
	dt_get_12hour(H,H12),
	AP = H div 12,
	dt_ins_YS0(FormatString,Y,OutStr1),
	dt_ins_Ys(OutStr1,Y,OutStr2),
	dt_ins_YL(OutStr2,Y,OutStr3),
	dt_ins_MD0(OutStr3,M,OutStr4),
	dt_ins_Md(OutStr4,M,OutStr5),
	dt_ins_MN0(OutStr5,M,OutStr6),
	dt_ins_Mn(OutStr6,M,OutStr7),
	dt_ins_WD0(OutStr7,W,OutStr8),
	dt_ins_Wd(OutStr8,W,OutStr9),
	dt_ins_DD0(OutStr9,Dm,OutStr10),
	dt_ins_Dd(OutStr10,Dm,OutStr11),
	dt_ins_DN0(OutStr11,Dw,OutStr12),
	dt_ins_Dn(OutStr12,Dw,OutStr13),
	dt_ins_HH0(OutStr13,H,OutStr14),
	dt_ins_Hh(OutStr14,H,OutStr15),
	dt_ins_HHH0(OutStr15,H12,OutStr16),
	dt_ins_Hhh(OutStr16,H12,OutStr17),
	dt_ins_MM0(OutStr17,Min,OutStr18),
	dt_ins_Mm(OutStr18,Min,OutStr19),
	dt_ins_DR(OutStr19,Off,OutStr20),
	dt_ins_AR(OutStr20,Off,OutStr21),
	dt_ins_WR(OutStr21,Off,OutStr22),
	dt_ins_TT0(OutStr22,AP,OutStr23),
	dt_ins_Tt(OutStr23,AP,OutString),!.
  dt_minoffset_to_str(_,_,_,""):-!.

  dt_ins_YS0(FormatString,Y,OutStr):-
	searchstring(FormatString,"%YS",Pos),!,
	Y1 = (Y div 100) * 100,
	Y2 = Y - Y1,
	dt_to_2_digit_str(Y2,Str),
	dt_ins_string(FormatString,Pos,Str,OutStr1),
	dt_ins_YS0(OutStr1,Y,OutStr).
  dt_ins_YS0(FormatString,_,FormatString):-!.
  dt_ins_Ys(FormatString,Y,OutStr):-
	searchstring(FormatString,"%Ys",Pos),!,
	Y1 = (Y div 100) * 100,
	Y2 = Y - Y1,
	str_int(Str,Y2),
	dt_ins_string(FormatString,Pos,Str,OutStr1),
	dt_ins_Ys(OutStr1,Y,OutStr).
  dt_ins_Ys(FormatString,_,FormatString):-!.
  dt_ins_YL(FormatString,Y,OutStr):-
	searchstring(FormatString,"%YL",Pos),!,
	dt_to_4_digit_str(Y,Str),
	dt_ins_string(FormatString,Pos,Str,OutStr1),
	dt_ins_YL(OutStr1,Y,OutStr).
  dt_ins_YL(FormatString,_,FormatString):-!.
  dt_ins_MD0(FormatString,M,OutStr):-
	searchstring(FormatString,"%MD",Pos),!,
	dt_to_2_digit_str(M,Str),
	dt_ins_string(FormatString,Pos,Str,OutStr1),
	dt_ins_MD0(OutStr1,M,OutStr).
  dt_ins_MD0(FormatString,_,FormatString):-!.
  dt_ins_Md(FormatString,M,OutStr):-
	searchstring(FormatString,"%Md",Pos),!,
	str_int(Str,M),
	dt_ins_string(FormatString,Pos,Str,OutStr1),
	dt_ins_Md(OutStr1,M,OutStr).
  dt_ins_Md(FormatString,_,FormatString):-!.
  dt_ins_MN0(FormatString,M,OutStr):-
	searchstring(FormatString,"%MN",Pos),!,
	dt_month_name(M,Str),!,
	dt_ins_string(FormatString,Pos,Str,OutStr1),
	dt_ins_MN0(OutStr1,M,OutStr).
  dt_ins_MN0(FormatString,_,FormatString):-!.
  dt_ins_Mn(FormatString,M,OutStr):-
	searchstring(FormatString,"%Mn",Pos),!,
	dt_month_shortname(M,Str),!,
	dt_ins_string(FormatString,Pos,Str,OutStr1),
	dt_ins_Mn(OutStr1,M,OutStr).
  dt_ins_Mn(FormatString,_,FormatString):-!.
  dt_ins_WD0(FormatString,W,OutStr):-
	searchstring(FormatString,"%WD",Pos),!,
	dt_to_2_digit_str(W,Str),
	dt_ins_string(FormatString,Pos,Str,OutStr1),
	dt_ins_WD0(OutStr1,W,OutStr).
  dt_ins_WD0(FormatString,_,FormatString):-!.
  dt_ins_Wd(FormatString,W,OutStr):-
	searchstring(FormatString,"%Wd",Pos),!,
	str_int(Str,W),
	dt_ins_string(FormatString,Pos,Str,OutStr1),
	dt_ins_Wd(OutStr1,W,OutStr).
  dt_ins_Wd(FormatString,_,FormatString):-!.
  dt_ins_DD0(FormatString,D,OutStr):-
	searchstring(FormatString,"%DD",Pos),!,
	dt_to_2_digit_str(D,Str),
	dt_ins_string(FormatString,Pos,Str,OutStr1),
	dt_ins_DD0(OutStr1,D,OutStr).
  dt_ins_DD0(FormatString,_,FormatString):-!.
  dt_ins_Dd(FormatString,D,OutStr):-
	searchstring(FormatString,"%Dd",Pos),!,
	str_int(Str,D),
	dt_ins_string(FormatString,Pos,Str,OutStr1),
	dt_ins_Dd(OutStr1,D,OutStr).
  dt_ins_Dd(FormatString,_,FormatString):-!.
  dt_ins_DN0(FormatString,D,OutStr):-
	searchstring(FormatString,"%DN",Pos),!,
	dt_day_name(D,Str),!,
	dt_ins_string(FormatString,Pos,Str,OutStr1),
	dt_ins_DN0(OutStr1,D,OutStr).
  dt_ins_DN0(FormatString,_,FormatString):-!.
  dt_ins_Dn(FormatString,D,OutStr):-
	searchstring(FormatString,"%Dn",Pos),!,
	dt_day_shortname(D,Str),!,
	dt_ins_string(FormatString,Pos,Str,OutStr1),
	dt_ins_Dn(OutStr1,D,OutStr).
  dt_ins_Dn(FormatString,_,FormatString):-!.
  dt_ins_HH0(FormatString,H,OutStr):-
	searchstring(FormatString,"%HH",Pos),!,
	dt_to_2_digit_str(H,Str),
	dt_ins_string(FormatString,Pos,Str,OutStr1),
	dt_ins_HH0(OutStr1,H,OutStr).
  dt_ins_HH0(FormatString,_,FormatString):-!.
  dt_ins_Hh(FormatString,H,OutStr):-
	searchstring(FormatString,"%Hh",Pos),!,
	str_int(Str,H),
	dt_ins_string(FormatString,Pos,Str,OutStr1),
	dt_ins_Hh(OutStr1,H,OutStr).
  dt_ins_Hh(FormatString,_,FormatString):-!.
  dt_ins_HHH0(FormatString,H,OutStr):-
	searchstring(FormatString,"%hH",Pos),!,
	dt_to_2_digit_str(H,Str),
	dt_ins_string(FormatString,Pos,Str,OutStr1),
	dt_ins_HHH0(OutStr1,H,OutStr).
  dt_ins_HHH0(FormatString,_,FormatString):-!.
  dt_ins_Hhh(FormatString,H,OutStr):-
	searchstring(FormatString,"%hh",Pos),!,
	str_int(Str,H),
	dt_ins_string(FormatString,Pos,Str,OutStr1),
	dt_ins_Hhh(OutStr1,H,OutStr).
  dt_ins_Hhh(FormatString,_,FormatString):-!.
  dt_ins_MM0(FormatString,M,OutStr):-
	searchstring(FormatString,"%MM",Pos),!,
	dt_to_2_digit_str(M,Str),
	dt_ins_string(FormatString,Pos,Str,OutStr1),
	dt_ins_MM0(OutStr1,M,OutStr).
  dt_ins_MM0(FormatString,_,FormatString):-!.
  dt_ins_Mm(FormatString,M,OutStr):-
	searchstring(FormatString,"%Mm",Pos),!,
	str_int(Str,M),
	dt_ins_string(FormatString,Pos,Str,OutStr1),
	dt_ins_Mm(OutStr1,M,OutStr).
  dt_ins_Mm(FormatString,_,FormatString):-!.
  dt_ins_DR(FormatString,Mins,OutStr):-
	searchstring(FormatString,"%DR",Pos),!,
	dt_relative_par(MinsBegin,MinsStep),!,
	Scale = Mins - MinsBegin,
	Dr = Scale div MinsStep,
	dt_get_dr_txt(Dr,Str),
	dt_ins_string(FormatString,Pos,Str,OutStr1),
	dt_ins_DR(OutStr1,Mins,OutStr).
  dt_ins_DR(FormatString,_,FormatString):-!.
  dt_ins_AR(FormatString,Mins,OutStr):-
	searchstring(FormatString,"%AR",Pos),!,
	dt_relative_par_day(MinsBegin),!,
	Scale = Mins - MinsBegin,
	Dr = (Scale div dt_mins_in_day) + 1,
	trap(term_str(long,Dr,Str),_,fail),
	dt_ins_string(FormatString,Pos,Str,OutStr1),
	dt_ins_AR(OutStr1,Mins,OutStr).
  dt_ins_AR(FormatString,_,FormatString):-!.
  dt_ins_WR(FormatString,Mins,OutStr):-
	searchstring(FormatString,"%WR",Pos),!,
	dt_relative_par_week(MinsBegin),!,
	Scale = Mins - MinsBegin,
	Dr = (Scale div dt_mins_in_week) + 1,
	trap(term_str(long,Dr,Str),_,fail),
	dt_ins_string(FormatString,Pos,Str,OutStr1),
	dt_ins_WR(OutStr1,Mins,OutStr).
  dt_ins_WR(FormatString,_,FormatString):-!.
  dt_ins_TT0(FormatString,0,OutStr):-
	searchstring(FormatString,"%TT",Pos),!,
	dt_marker_time(Str,_),!,
	dt_ins_string(FormatString,Pos,Str,OutStr1),
	dt_ins_TT0(OutStr1,0,OutStr).
  dt_ins_TT0(FormatString,1,OutStr):-
	searchstring(FormatString,"%TT",Pos),!,
	dt_marker_time(_,Str),!,
	dt_ins_string(FormatString,Pos,Str,OutStr1),
	dt_ins_TT0(OutStr1,1,OutStr).
  dt_ins_TT0(FormatString,_,FormatString):-!.
  dt_ins_Tt(FormatString,0,OutStr):-
	searchstring(FormatString,"%Tt",Pos),!,
	dt_shortmarker_time(Str,_),!,
	dt_ins_string(FormatString,Pos,Str,OutStr1),
	dt_ins_Tt(OutStr1,0,OutStr).
  dt_ins_Tt(FormatString,1,OutStr):-
	searchstring(FormatString,"%Tt",Pos),!,
	dt_shortmarker_time(_,Str),!,
	dt_ins_string(FormatString,Pos,Str,OutStr1),
	dt_ins_Tt(OutStr1,1,OutStr).
  dt_ins_Tt(FormatString,_,FormatString):-!.

  dt_get_dr_txt(Dr,Names):-
	dt_relative_txt(NamesList,AmountNames),!,
	Number = (Dr mod AmountNames) + 1,
	Numb = cast(integer,Number),
	dt_member_of_list(Names,NamesList,Numb),!.
  dt_get_dr_txt(Dr,Names):-
	Dr1 = Dr + 1,
	trap(term_str(long,Dr1,Names),_,fail).

  dt_ins_string(SourceString,Pos,InsStr,OutStr):-!,
	Pos1 = Pos - 1,
	substring(SourceString,1,Pos1,Part1),
	Pos2 = Pos + 3,
	str_len(SourceString,Length),
	L = Length - Pos2 + 1,
	substring(SourceString,Pos2,L,Part2),
	concat(Part1,InsStr,OutStr1),
	concat(OutStr1,Part2,OutStr).

  dt_get_12hour(0,12):-!.
  dt_get_12hour(12,12):-!.
  dt_get_12hour(H,H1):-
	H1 = H mod 12,!.

/**global PREDICATE*******************************************************
                             DT_DATEOFFSET_TO_STR\2
 *************************************************************************/
  dt_dateoffset_to_str(0,"01010001"):-!.
  dt_dateoffset_to_str(Off,DateStr):-
    dt_offset_to_date(Off,Y,M,_,D,_),
    dt_to_2_digit_str(D,D1),
    dt_to_2_digit_str(M,M1),
    dt_to_4_digit_str(Y,Y1),
    format(DateStr,"%s%s%s",D1,M1,Y1),!.
 
/**global PREDICATE*******************************************************
                             DT_DATEOFFSET_TO_STR\3
 *************************************************************************/
  dt_dateoffset_to_str(Off,FormatString,OutString):-
	dt_mins_date_time(Off1,Off,0),
	dt_minoffset_to_str(Off1,FormatString,OutString),!.
  dt_dateoffset_to_str(_,_,""):-!.

/**global PREDICATE*******************************************************
                             DT_TIMESTR_TO_OFFSET/2
 *************************************************************************/
  dt_timestr_to_offset("",0) :- !.
  dt_timestr_to_offset(Str,0):-  
	str_int(Str,0),!.
  dt_timestr_to_offset(TimeStr,Off):-
	frontstr(2,TimeStr,Hour,Min),
	str_int(Hour,H),
	str_int(Min,Mi),
	dt_time_to_offset(H,Mi,Off),!.
  dt_timestr_to_offset(_,0):-
	errorexit(error_vpi_package_bad_data).

/**global PREDICATE*******************************************************
                             DT_TIMESTR_TO_OFFSET/3
 *************************************************************************/
  dt_timestr_to_offset(InString,FormatString,Off):-
	dt_minstr_to_offset(InString,FormatString,Min),
	dt_mins_date_time(Min,_,Off),!.
  dt_timestr_to_offset(_,_,0):-
	errorexit(error_vpi_package_bad_data).

/**global PREDICATE*******************************************************
                             DT_MINSTR_TO_OFFSET/2
 *************************************************************************/
  dt_minstr_to_offset("",0) :- !.
  dt_minstr_to_offset(TimeStr,Off):-
	frontstr(2,TimeStr,Dat,Rest),
	frontstr(2,Rest,Mo,Rest1),
	frontstr(4,Rest1,Year,Rest2),
	frontstr(2,Rest2,Hour,Min),
	str_int(Dat,D),
	str_int(Mo,M),
	str_int(YEAR,Y),
	str_int(Hour,H),
	str_int(Min,Mi),
	dt_min_to_offset(Y,M,D,H,Mi,Off),!.
  dt_minstr_to_offset(_,0) :-
	errorexit(error_vpi_package_bad_data).

/**global PREDICATE*******************************************************
                             DT_MINSTR_TO_OFFSET\3
 *************************************************************************/
predicates
determ dt_work_str(STRING InString,STRING FormatString)
determ dt_get_item(STRING InString,INTEGER Pos,STRING Command,STRING IS)
determ dt_get_value(STRING InString,INTEGER Pos,INTEGER Length,INTEGER Value,STRING IS)
determ dt_get_value1(STRING InString,INTEGER Pos,INTEGER Value,STRING IS)
procedure  dt_get_year(INTEGER Value1,INTEGER Value)
determ dt_get_monthname(STRING InString,INTEGER Pos,INTEGER Value,STRING IS)
determ dt_get_monthshortname(STRING InString,INTEGER Pos,INTEGER Value,STRING IS)
determ dt_get_dayname(STRING InString,INTEGER Pos,INTEGER Value,STRING IS)
determ dt_get_dayshortname(STRING InString,INTEGER Pos,INTEGER Value,STRING IS)
determ dt_get_time_marker(STRING InString,INTEGER Pos,INTEGER Value,STRING IS)
determ dt_get_time_shortmarker(STRING InString,INTEGER Pos,INTEGER Value,STRING IS)
determ dt_sub_string_my(INTEGER Pos,INTEGER Length,STRING InStr,STRING Separate,STRING Value,STRING RestStr)
determ dt_assert_full()
determ dt_sep_cmp(STRING InString,STRING Separator)
determ INTEGER dt_day_in_month(INTEGER Y,INTEGER M)
determ  dt_check_date()

clauses
  dt_minstr_to_offset(InString,FormatString,Min):-
	retractall(dt_full_date(_,_,_,_,_,_,_,_,_)),
	assert(dt_full_date(-1,-1,-1,-1,-1,-1,-1,-1,-1)),
	dt_work_str(InString,FormatString),
	dt_assert_full(),
	dt_check_date(),
	dt_full_date(Y,M,_,Dm,_,Hour,Mins,_,_),
	dt_min_to_offset(Y,M,Dm,Hour,Mins,Min),!.
  dt_minstr_to_offset(_,_,0) :-
	errorexit(error_vpi_package_bad_data).

  dt_check_date():-
	dt_full_date(_,_,_,_,_,_,Mins,_,_),
	Mins < 0,
	retractall(dt_parse_error(_,_)),
	assert(dt_parse_error(14208,"Illegal value (Minut)")),!,
	fail.
  dt_check_date():-
	dt_full_date(_,_,_,_,_,_,Mins,_,_),
	Mins > 59,
	retractall(dt_parse_error(_,_)),
	assert(dt_parse_error(14208,"Illegal value (Minut)")),!,
	fail.
  dt_check_date():-
	dt_full_date(_,_,_,_,_,Hour,_,_,_),
	Hour < 0,
	retractall(dt_parse_error(_,_)),
	assert(dt_parse_error(14208,"Illegal value (Hour)")),!,
	fail.
  dt_check_date():-
	dt_full_date(_,_,_,_,_,Hour,_,_,_),
	Hour > 23,
	retractall(dt_parse_error(_,_)),
	assert(dt_parse_error(14208,"Illegal value (Hour)")),!,
	fail.
  dt_check_date():-
	dt_full_date(_,M,_,_,_,_,_,_,_),
	M < 1,
	retractall(dt_parse_error(_,_)),
	assert(dt_parse_error(14208,"Illegal value (Month)")),!,
	fail.
  dt_check_date():-
	dt_full_date(_,M,_,_,_,_,_,_,_),
	M > 12,
	retractall(dt_parse_error(_,_)),
	assert(dt_parse_error(14208,"Illegal value (Month)")),!,
	fail.
  dt_check_date():-
	dt_full_date(_,_,_,Dm,_,_,_,_,_),
	Dm < 1,
	retractall(dt_parse_error(_,_)),
	assert(dt_parse_error(14208,"Illegal value (Day)")),!,
	fail.
  dt_check_date():-
	dt_full_date(Y,M,_,Dm,_,_,_,_,_),
	Dm1 = dt_day_in_month(Y,M),
	Dm > Dm1,
	retractall(dt_parse_error(_,_)),
	assert(dt_parse_error(14208,"Illegal value (Day)")),!,
	fail.
  dt_check_date():-
	dt_full_date(_,_,_,Dm,_,_,_,_,_),
	Dm > 31,
	retractall(dt_parse_error(_,_)),
	assert(dt_parse_error(14208,"Illegal value (Day)")),!,
	fail.
  dt_check_date().

  dt_day_in_month(_,1,dt_day_in_jan).
  dt_day_in_month(Y,2,dt_day_in_feb1):-
	Y1 = Y mod 4,
	Y1 = 0,!.
  dt_day_in_month(_,2,dt_day_in_feb).
  dt_day_in_month(_,3,dt_day_in_mar).
  dt_day_in_month(_,4,dt_day_in_apr).
  dt_day_in_month(_,5,dt_day_in_may).
  dt_day_in_month(_,6,dt_day_in_jun).
  dt_day_in_month(_,7,dt_day_in_jul).
  dt_day_in_month(_,8,dt_day_in_aug).
  dt_day_in_month(_,9,dt_day_in_sep).
  dt_day_in_month(_,10,dt_day_in_oct).
  dt_day_in_month(_,11,dt_day_in_nov).
  dt_day_in_month(_,12,dt_day_in_dec).

  dt_work_str(InString,FormatString):-
	retractall(dt_parse_error(_,_)),
	searchchar(FormatString,'%',Pos),!,
	dt_sub_string_my(Pos,3,FormatString,S,Command,FS),
	dt_sep_cmp(InString,S),
	dt_get_item(InString,Pos,Command,IS),
	dt_work_str(IS,FS).
  dt_work_str(S,S):-!.
  dt_work_str(_,_):-
	retractall(dt_parse_error(_,_)),
	assert(dt_parse_error(14200,"Syntax error")),!,
	fail.

  dt_sep_cmp(InString,S):-
	str_len(S,Length),
	frontstr(Length,InString,S1,_),
	S = S1,!.
  dt_sep_cmp(_,S):-
	retractall(dt_parse_error(_,_)),
	format(Str,"Expected separator: '%s'",S),
	assert(dt_parse_error(14206,Str)),!,
	fail.

  dt_get_item(InString,Pos,"%YS",IS):-!,
	dt_get_value(InString,Pos,2,Value1,IS),
	dt_get_year(Value1,Value),
	retract(dt_full_date(_,M,W,Dm,Dw,Hour,Mins,H12,AP)),!,
	assert(dt_full_date(Value,M,W,Dm,Dw,Hour,Mins,H12,AP)).
  dt_get_item(InString,Pos,"%Ys",IS):-!,
	dt_get_value1(InString,Pos,Value1,IS),
	dt_get_year(Value1,Value),
	retract(dt_full_date(_,M,W,Dm,Dw,Hour,Mins,H12,AP)),!,
	assert(dt_full_date(Value,M,W,Dm,Dw,Hour,Mins,H12,AP)).
  dt_get_item(InString,Pos,"%YL",IS):-!,
	dt_get_value(InString,Pos,4,Value,IS),
	retract(dt_full_date(_,M,W,Dm,Dw,Hour,Mins,H12,AP)),!,
	assert(dt_full_date(Value,M,W,Dm,Dw,Hour,Mins,H12,AP)).
  dt_get_item(InString,Pos,"%MD",IS):-!,
	dt_get_value(InString,Pos,2,Value,IS),
	retract(dt_full_date(Y,_,W,Dm,Dw,Hour,Mins,H12,AP)),!,
	assert(dt_full_date(Y,Value,W,Dm,Dw,Hour,Mins,H12,AP)).
  dt_get_item(InString,Pos,"%Md",IS):-!,
	dt_get_value1(InString,Pos,Value,IS),
	retract(dt_full_date(Y,_,W,Dm,Dw,Hour,Mins,H12,AP)),!,
	assert(dt_full_date(Y,Value,W,Dm,Dw,Hour,Mins,H12,AP)).
  dt_get_item(InString,Pos,"%MN",IS):-!,
	dt_get_monthname(InString,Pos,Value,IS),
	retract(dt_full_date(Y,_,W,Dm,Dw,Hour,Mins,H12,AP)),!,
	assert(dt_full_date(Y,Value,W,Dm,Dw,Hour,Mins,H12,AP)).
  dt_get_item(InString,Pos,"%Mn",IS):-!,
	dt_get_monthshortname(InString,Pos,Value,IS),
	retract(dt_full_date(Y,_,W,Dm,Dw,Hour,Mins,H12,AP)),!,
	assert(dt_full_date(Y,Value,W,Dm,Dw,Hour,Mins,H12,AP)).
  dt_get_item(InString,Pos,"%WD",IS):-!,
	dt_get_value(InString,Pos,2,Value,IS),
	retract(dt_full_date(Y,M,_,Dm,Dw,Hour,Mins,H12,AP)),!,
	assert(dt_full_date(Y,M,Value,Dm,Dw,Hour,Mins,H12,AP)).
  dt_get_item(InString,Pos,"%Wd",IS):-!,
	dt_get_value1(InString,Pos,Value,IS),
	retract(dt_full_date(Y,M,_,Dm,Dw,Hour,Mins,H12,AP)),!,
	assert(dt_full_date(Y,M,Value,Dm,Dw,Hour,Mins,H12,AP)).
  dt_get_item(InString,Pos,"%DD",IS):-!,
	dt_get_value(InString,Pos,2,Value,IS),
	retract(dt_full_date(Y,M,W,_,Dw,Hour,Mins,H12,AP)),!,
	assert(dt_full_date(Y,M,W,Value,Dw,Hour,Mins,H12,AP)).
  dt_get_item(InString,Pos,"%Dd",IS):-!,
	dt_get_value1(InString,Pos,Value,IS),
	retract(dt_full_date(Y,M,W,_,Dw,Hour,Mins,H12,AP)),!,
	assert(dt_full_date(Y,M,W,Value,Dw,Hour,Mins,H12,AP)).
  dt_get_item(InString,Pos,"%DN",IS):-!,
	dt_get_dayname(InString,Pos,Value,IS),
	retract(dt_full_date(Y,M,W,Dm,_,Hour,Mins,H12,AP)),!,
	assert(dt_full_date(Y,M,W,Dm,Value,Hour,Mins,H12,AP)).
  dt_get_item(InString,Pos,"%Dn",IS):-!,
	dt_get_dayshortname(InString,Pos,Value,IS),
	retract(dt_full_date(Y,M,W,Dm,_,Hour,Mins,H12,AP)),!,
	assert(dt_full_date(Y,M,W,Dm,Value,Hour,Mins,H12,AP)).
  dt_get_item(InString,Pos,"%HH",IS):-!,
	dt_get_value(InString,Pos,2,Value,IS),
	retract(dt_full_date(Y,M,W,Dm,Dw,_,Mins,H12,AP)),!,
	assert(dt_full_date(Y,M,W,Dm,Dw,Value,Mins,H12,AP)).
  dt_get_item(InString,Pos,"%Hh",IS):-!,
	dt_get_value1(InString,Pos,Value,IS),
	retract(dt_full_date(Y,M,W,Dm,Dw,_,Mins,H12,AP)),!,
	assert(dt_full_date(Y,M,W,Dm,Dw,Value,Mins,H12,AP)).
  dt_get_item(InString,Pos,"%MM",IS):-!,
	dt_get_value(InString,Pos,2,Value,IS),
	retract(dt_full_date(Y,M,W,Dm,Dw,Hour,_,H12,AP)),!,
	assert(dt_full_date(Y,M,W,Dm,Dw,Hour,Value,H12,AP)).
  dt_get_item(InString,Pos,"%Mm",IS):-!,
	dt_get_value1(InString,Pos,Value,IS),
	retract(dt_full_date(Y,M,W,Dm,Dw,Hour,_,H12,AP)),!,
	assert(dt_full_date(Y,M,W,Dm,Dw,Hour,Value,H12,AP)).
  dt_get_item(InString,Pos,"%hH",IS):-!,
	dt_get_value(InString,Pos,2,Value,IS),
	retract(dt_full_date(Y,M,W,Dm,Dw,Hour,Mins,_,AP)),!,
	assert(dt_full_date(Y,M,W,Dm,Dw,Hour,Mins,Value,AP)).
  dt_get_item(InString,Pos,"%hh",IS):-!,
	dt_get_value1(InString,Pos,Value,IS),
	retract(dt_full_date(Y,M,W,Dm,Dw,Hour,Mins,_,AP)),!,
	assert(dt_full_date(Y,M,W,Dm,Dw,Hour,Mins,Value,AP)).
  dt_get_item(InString,Pos,"%TT",IS):-!,
	dt_get_time_marker(InString,Pos,Value,IS),
	retract(dt_full_date(Y,M,W,Dm,Dw,Hour,Mins,H12,_)),!,
	assert(dt_full_date(Y,M,W,Dm,Dw,Hour,Mins,H12,Value)).
  dt_get_item(InString,Pos,"%Tt",IS):-!,
	dt_get_time_shortmarker(InString,Pos,Value,IS),
	retract(dt_full_date(Y,M,W,Dm,Dw,Hour,Mins,H12,_)),!,
	assert(dt_full_date(Y,M,W,Dm,Dw,Hour,Mins,H12,Value)).
  dt_get_item(IS,_,Command,IS):-
	retractall(dt_parse_error(_,_)),
	format(Str,"Unknow command: '%s'",Command),
	assert(dt_parse_error(14202,Str)),!,
	fail.

predicates
determ dt_check_StrInt(STRING ValueStr)
clauses	
  dt_get_value(InString,Pos,Length,Value,IS):-
	dt_sub_string_my(Pos,Length,InString,_,ValueStr,IS),
	dt_check_StrInt(ValueStr),!,
	str_int(ValueStr,Value).
  dt_get_value(InString,Pos,Length,0,IS):-
	dt_sub_string_my(Pos,Length,InString,_,ValueStr,IS),
	retractall(dt_parse_error(_,_)),
	format(Str,"Cannot convert: '%s'",ValueStr),
	assert(dt_parse_error(14203,Str)),!,
	fail.

  dt_get_value1(InString,Pos,Value,IS):-
	dt_sub_string_my(Pos,2,InString,_,ValueStr,IS),
	dt_check_StrInt(ValueStr),!,
	str_int(ValueStr,Value).
  dt_get_value1(InString,Pos,Value,IS):-
	dt_sub_string_my(Pos,1,InString,_,ValueStr,IS),
	dt_check_StrInt(ValueStr),!,
	str_int(ValueStr,Value).
  dt_get_value1(InString,Pos,0,IS):-
	dt_sub_string_my(Pos,1,InString,_,ValueStr,IS),
	retractall(dt_parse_error(_,_)),
	format(Str,"Cannot convert: '%s'",ValueStr),
	assert(dt_parse_error(14203,Str)),!,
	fail.

  dt_check_StrInt(""):-!.
  dt_check_StrInt(ValueStr):-
	frontchar(ValueStr,Char,VS),!,
	48 <= Char, Char <= 57,
	dt_check_StrInt(VS).
	
  dt_get_year(Value1,Value):-
	dt_two_digit_start_year(StYear),
	Value1 >= StYear,!,
	Value = Value1 + 1900.
  dt_get_year(Value1,Value):-
	Value = Value1 + 2000.

predicates
determ dt_str_cmp(STRING,STRING)
clauses
  dt_get_monthname(InString,Pos,Value,IS):-
	dt_month_name(Value,Name),
	str_len(Name,Length),
	dt_sub_string_my(Pos,Length,InString,_,Name1,IS),
	dt_str_cmp(Name,Name1),!.
  dt_get_monthname(_,_,0,""):-
	retractall(dt_parse_error(_,_)),
	assert(dt_parse_error(14204,"Bad month name")),!,
	fail.

  dt_get_monthshortname(InString,Pos,Value,IS):-
	dt_month_shortname(Value,Name),
	str_len(Name,Length),
	dt_sub_string_my(Pos,Length,InString,_,Name1,IS),
	dt_str_cmp(Name,Name1),!.
  dt_get_monthshortname(_,_,0,""):-
	retractall(dt_parse_error(_,_)),
	assert(dt_parse_error(14204,"Bad month name")),!,
	fail.

  dt_get_dayname(InString,Pos,Value,IS):-
	dt_day_name(Value,Name),
	str_len(Name,Length),
	dt_sub_string_my(Pos,Length,InString,_,Name1,IS),
	dt_str_cmp(Name,Name1),!.
  dt_get_dayname(_,_,0,""):-
	retractall(dt_parse_error(_,_)),
	assert(dt_parse_error(14205,"Bad day name")),!,
	fail.

  dt_get_dayshortname(InString,Pos,Value,IS):-
	dt_day_shortname(Value,Name),
	str_len(Name,Length),
	dt_sub_string_my(Pos,Length,InString,_,Name1,IS),
	dt_str_cmp(Name,Name1),!.
  dt_get_dayshortname(_,_,0,""):-
	retractall(dt_parse_error(_,_)),
	assert(dt_parse_error(14205,"Bad day name")),!,
	fail.

  dt_get_time_marker(InString,Pos,0,IS):-
	dt_marker_time(Name,_),
	str_len(Name,Length),
	dt_sub_string_my(Pos,Length,InString,_,Name1,IS),
	dt_str_cmp(Name,Name1),!.
  dt_get_time_marker(InString,Pos,1,IS):-
	dt_marker_time(_,Name),
	str_len(Name,Length),
	dt_sub_string_my(Pos,Length,InString,_,Name1,IS),
	dt_str_cmp(Name,Name1),!.
  dt_get_time_marker(_,_,0,""):-
	retractall(dt_parse_error(_,_)),
	assert(dt_parse_error(14210,"Bad time marker")),!,
	fail.

  dt_get_time_shortmarker(InString,Pos,0,IS):-
	dt_shortmarker_time(Name,_),
	str_len(Name,Length),
	dt_sub_string_my(Pos,Length,InString,_,Name1,IS),
	dt_str_cmp(Name,Name1),!.
  dt_get_time_shortmarker(InString,Pos,1,IS):-
	dt_shortmarker_time(_,Name),
	str_len(Name,Length),
	dt_sub_string_my(Pos,Length,InString,_,Name1,IS),
	dt_str_cmp(Name,Name1),!.
  dt_get_time_shortmarker(_,_,0,""):-
	retractall(dt_parse_error(_,_)),
	assert(dt_parse_error(14210,"Bad time marker")),!,
	fail.

  dt_str_cmp("",""):-!.
  dt_str_cmp("",_):-!,fail.
  dt_str_cmp(_,""):-!,fail.
  dt_str_cmp(Name,Name1):-
	frontchar(Name,Char,N),
	frontchar(Name1,Char1,N1),
	Char = Char1,!,
	dt_str_cmp(N,N1).
  dt_str_cmp(Name,Name1):-
	frontchar(Name,Char,N),
	frontchar(Name1,Char1,N1),
	upper_lower(Char1,Char2),
	Char = Char2,!,
	dt_str_cmp(N,N1).
  dt_str_cmp(Name,Name1):-
	frontchar(Name,Char,N),
	frontchar(Name1,Char1,N1),
	upper_lower(Char2,Char1),
	Char = Char2,!,
	dt_str_cmp(N,N1).

  dt_sub_string_my(Pos,Length,InStr,Separator,Value,RestStr):-
	P = Pos - 1,
	frontstr(P,InStr,Separator,RStr),
	frontstr(Length,RStr,Value,RestStr),!.
  dt_sub_string_my(_,_,_,"","",""):-
	retractall(dt_parse_error(_,_)),
	assert(dt_parse_error(14207,"Not enough information in the source string")),!,
	fail.

predicates
  dt_get_hour12(INTEGER H,INTEGER AP,INTEGER Out)
clauses
  dt_get_hour12(12,0,0):-!.
  dt_get_hour12(12,1,12):-!.
  dt_get_hour12(H,AP,Out):-
	Out = H + AP * 12,!.

%   Hour and Mins
  dt_assert_full():-
	dt_full_date(Y,M,W,Dm,Dw,-1,Mins,H12,AP),
	H12 <> -1, AP <> -1,
	retractall(dt_full_date(_,_,_,_,_,_,_,_,_)),
	dt_get_hour12(H12,AP,Hour),
	assert(dt_full_date(Y,M,W,Dm,Dw,Hour,Mins,H12,AP)),
	fail.
  dt_assert_full():-
	dt_full_date(Y,M,W,Dm,Dw,-1,Mins,H12,-1),
	H12 <> -1,
	retractall(dt_full_date(_,_,_,_,_,_,_,_,_)),
	dt_get_hour12(H12,0,Hour),
	assert(dt_full_date(Y,M,W,Dm,Dw,Hour,Mins,H12,-1)),
	fail.
  dt_assert_full():-
	dt_full_date(Y,M,W,Dm,Dw,-1,-1,H12,AP),
	retractall(dt_full_date(_,_,_,_,_,_,_,_,_)),
	time(Hour,Mins,_,_),
	assert(dt_full_date(Y,M,W,Dm,Dw,Hour,Mins,H12,AP)),
	fail.
  dt_assert_full():-
	dt_full_date(Y,M,W,Dm,Dw,-1,Mins,H12,AP),
	retractall(dt_full_date(_,_,_,_,_,_,_,_,_)),
	time(Hour,_,_,_),
	assert(dt_full_date(Y,M,W,Dm,Dw,Hour,Mins,H12,AP)),
	fail.
  dt_assert_full():-
	dt_full_date(Y,M,W,Dm,Dw,Hour,-1,H12,AP),
	retractall(dt_full_date(_,_,_,_,_,_,_,_,_)),
	assert(dt_full_date(Y,M,W,Dm,Dw,Hour,0,H12,AP)),
	fail.
%   Year, month and day of month
  dt_assert_full():-
	dt_full_date(-1,-1,-1,-1,-1,Hour,Mins,H12,AP),
	retractall(dt_full_date(_,_,_,_,_,_,_,_,_)),
	date(Y,M,Dm,_),
	assert(dt_full_date(Y,M,-1,Dm,-1,Hour,Mins,H12,AP)),!.
  dt_assert_full():-
	dt_full_date(Y,-1,-1,-1,-1,Hour,Mins,H12,AP),
	retractall(dt_full_date(_,_,_,_,_,_,_,_,_)),
	assert(dt_full_date(Y,1,-1,1,-1,Hour,Mins,H12,AP)),!.
  dt_assert_full():-
	dt_full_date(-1,M,W,Dm,Dw,Hour,Mins,H12,AP),
	retractall(dt_full_date(_,_,_,_,_,_,_,_,_)),
	date(Y,_,_,_),
	assert(dt_full_date(Y,M,W,Dm,Dw,Hour,Mins,H12,AP)),
	fail.
  dt_assert_full():-
	dt_full_date(Y,M,-1,-1,-1,Hour,Mins,H12,AP),
	retractall(dt_full_date(_,_,_,_,_,_,_,_,_)),
	assert(dt_full_date(Y,M,-1,1,-1,Hour,Mins,H12,AP)),!.
  dt_assert_full():-
	dt_full_date(Y,-1,-1,Dm,-1,Hour,Mins,H12,AP),
	retractall(dt_full_date(_,_,_,_,_,_,_,_,_)),
	date(_,M,_,_),
	assert(dt_full_date(Y,M,-1,Dm,-1,Hour,Mins,H12,AP)),!.
  dt_assert_full():-
	dt_full_date(Y,-1,W,-1,-1,Hour,Mins,H12,AP),
	retractall(dt_full_date(_,_,_,_,_,_,_,_,_)),
	dt_week_year_to_offset(W,Y,DateOff),
	dt_offset_to_date(DateOff,Y1,M,_,Dm,_),
	assert(dt_full_date(Y1,M,-1,Dm,-1,Hour,Mins,H12,AP)),!.
  dt_assert_full():-
	dt_full_date(Y,-1,W,-1,Dw,Hour,Mins,H12,AP),
	retractall(dt_full_date(_,_,_,_,_,_,_,_,_)),
	dt_week_year_to_offset(W,Y,DateOff1),
	DateOff = DateOff1 + Dw - 1,
	dt_offset_to_date(DateOff,Y1,M,_,Dm,_),
	assert(dt_full_date(Y1,M,-1,Dm,-1,Hour,Mins,H12,AP)),!.
  dt_assert_full():-
	dt_full_date(Y,M,_,Dm,_,Hour,Mins,_,_),
	Y <> -1, M <> -1, Dm <> -1, Hour <> -1, Mins <> -1,!.
  dt_assert_full():-
	retractall(dt_parse_error(_,_)),
	assert(dt_parse_error(14201,"Contradictory information")),!,
	fail.
