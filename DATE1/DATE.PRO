/*****************************************************************************

               Copyright (c) 1984 - 2000 Prolog Development Center A/S

 FileName: DATE.PRO
 Purpose: Date and Time handling predicates
 Written by: Sergey Alexeev
 Build:		012.990119
 Tested with:   Visual Prolog v.5.1 build 555a
 Comments:
******************************************************************************/

include "tasvp1.inc"
include "error.con"

ifndef date_pre_included
 #Error: this file requires include: date.pre
enddef 

include "date1\\date1.con"
include "date1\\datemain.pro"
include "date1\\datenext.pro"
