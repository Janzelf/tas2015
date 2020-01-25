/******************************************************************************

               Copyright (c) 1984 - 2000 Prolog Development Center A/S

 FileName:	DATE.PRO
 PURPOSE:	Date module
 Written by:	Alexander Butovsky
 Version:	1.0
 Build:		012.990119
 Tested with:   Visual Prolog v.5.1 build 555a
 Comments:	

******************************************************************************/
ifdef use_date
 #Error: Date module was alredy included by Visual Prolog Application Expert
enddef 

ifdef platform_16bit
  code = 5000
enddef

% Replace PROJECT with actual name of project:
include "PROJECT.inc"

include "date.pre"

% Replace DATE_DIR with actual path to DATE tool:
include "DATE_DIR\\date.con"
include "DATE_DIR\\datemain.pro"
include "DATE_DIR\\datenext.pro"
