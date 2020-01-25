/******************************************************************************

		Copyright (c)2000 Prolog Development Center A/S

 FileName:	Browser.pro
 Purpose:	Browser tool template file
 Written by:	Alexander Butovsky
 Date:		25.08.2000
 Tested with:	Visual Prolog 5.2 build 594
 Comments:
******************************************************************************/
ifdef platform_16bit
  code = 5000
enddef

% Replace PROJECT with actual name of project:
include "tasvp1.inc"
include "tasvp1.con"
include "hlptopic.con"

include "Browser.pre"

include "Tools\\Browser\\Browser.pro"

