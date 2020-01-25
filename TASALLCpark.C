/**********************************************************
  overal gebruikte c routines:
  1.sorteren van stringlist (ongevoelig voor hoofd/kleine letters)
 **********************************************************/

#include <stdlib.h>
#include <string.h>
/*#include <dos.h>*/

/*#define MLINEAR*/

#include <pdcrunt.h>

#define listfno 1
#define nilfno  2

#define mydebug21
/*int _searchslist_c(strlist * inlist, char * searchstr, int begin_pos);*/

typedef struct node {
  unsigned char functor;  /* the type                   */
  char *value;            /* a string pointer           */
  struct node *next;      /* pointer to next struc node */
} strlist;

strlist * xml_GetAttributes(const char **attr)
{
  int i;
  strlist *attrPairs;
  for (i = 0; attr[i]; i += 2) {
  }
  attrPairs = (strlist *) MEM_AllocGStack(++i*sizeof(strlist));
  for (i = 0; attr[i]; i += 2) {
    (attrPairs+i)->functor = listfno;
    attrPairs[i].value = (char *) attr[i];
    attrPairs[i].next = attrPairs+i+1;
    attrPairs[i+1].functor = listfno;
    attrPairs[i+1].value = (char *) attr[i+1];
    attrPairs[i+1].next = attrPairs+i+2;
  }
  attrPairs[i].functor = nilfno;
  return attrPairs;
}

/**********************************************************
  het sorteren van een stringlist, ongevoelig voor case
 **********************************************************/

static int  mycomp (const void *node1, const void *node2)
{
  return(stricmp(*(char **)node1, *(char **)node2));
}

void sortstring_c(strlist *inlist, int begin_pos)
{
  strlist *savelist = inlist;
  char **array;
  int i,n;

  begin_pos--;
  for(i=0; inlist->functor==listfno; inlist=inlist->next)
  {
    i++;
  }
  n = i;
  array = (char **) MEM_AllocGStack(i*sizeof(char *));
  inlist = savelist;
  for (i = 0; inlist->functor==listfno; inlist=inlist->next)
    array[i++] = (inlist->value) + begin_pos;
  qsort(array, n, sizeof(array[0]),  mycomp);

  inlist = savelist;
  for (i = 0; inlist->functor==listfno; inlist=inlist->next)
    inlist->value = array[i++] - begin_pos;

}

void searchslist_c(strlist * inlist, char * searchstr, unsigned int begin_pos, int *FoundPos)
{
  /*strlist *savelist = inlist;*/
  int index;
  /*IO_Writef("\nBegin_pos:%5d ", begin_pos),*/
  
  *FoundPos = 0;
  begin_pos--;
  for (index = 0; inlist->functor==listfno; inlist=inlist->next) {
    if (strlen(inlist->value) >= begin_pos) {
      if (strnicmp((inlist->value)+begin_pos, searchstr, strlen(searchstr)) >= 0) {
        *FoundPos = index;
        return;
        }
      }
    index++;
    }
  return;
}

void  trim_c(char * Text)
{
char * Current;
char * C1;


if (!*Text) return;
for (Current = Text ; *Current ; Current++) if (*Current == '\xA0') *Current = ' ';
				/* zoek eerste non-blank, of end-of-string */
for (Current = Text ; *Current == ' ' && *Current; Current++) ;
if (!*Current) {    		/* als end-of-string -> string = 0         */
 *Text = 0 ;
 return    ;
}
				/* copy alles naar voren                   */
				/* tot end-of-string                       */
for (C1 = Text ; *Current ; C1++, Current++) *C1 = *Current;
				/* C1 staat nu op end-of-string		   */
				/* snoep de achterkant af                  */
for (C1-- ; *C1 == ' ' ; C1--) ;
*++C1 = 0 ;
}


void  trim_komma_c(char * Text)
{
char * Current;
char * C1;

if (!*Text) return;
for (C1 = Text ; *C1 ; C1++)
  if(*C1 == ',' && *(C1+1) == ' ')
    for(Current = C1 + 1 ; *Current ; Current++)
      *Current = *(Current + 1);
}

/**********************************************************
  sorteren van integers
 **********************************************************/


typedef struct integerelem {  /* het prolog integerlist domain */
  unsigned char functor;
  int getal;
  struct integerelem *next;
} getallist;

static int mycomp_int(const void *int1, const void *int2)
{
  return(**(int **)int1 - **(int **)int2);
} ;

void sortint_c(getallist *inlist)
{
  getallist *savelist = inlist;
  int  *array_int;
  int  **array_intp;
  int i,n;

  for(i=0; inlist->functor==listfno; inlist=inlist->next)
  {
    i++;
  }
  n = i;
  array_int =  MEM_AllocGStack(i*sizeof(int));

  array_intp = MEM_AllocGStack(i*sizeof(int *));
  inlist = savelist;
  for (i = 0; inlist->functor==listfno; inlist=inlist->next, i++) {
    array_int[i]  = inlist->getal;
    array_intp[i] = &(array_int[i]);
  } ;
  qsort(array_intp, n, sizeof(array_intp[0]), mycomp_int);

  inlist = savelist;
  for (i = 0; inlist->functor==listfno; inlist=inlist->next) {
    inlist->getal = *(array_intp[i++]);
  } ;

}
void sorttijd_c(getallist *inlist)
{
  sortint_c(inlist);
}

void sortuint_c(getallist *inlist)
{
  sortint_c(inlist);
}



/* sorteer plan-prioriteit en geef de wedstrijden nondeterm weer door */



/*******************************************************/
/*            de PDC C-ondersteuning                   */
/*     zie PDC Prolog Professional User's Guide        */
/*******************************************************/

/*******************************************************/
/*    function-prototypes                              */
/*    uit de PROLOG code gegenereerd met de            */
/*    PRO2C utility                                    */
/*******************************************************/


#define VARFNO		0
#define CONSTFNO	1
#define LISTFNO		1
#define NILLFNO		2

#define INTEGER		int
#define CHAR		char
#define STRING		char*
#define SYMBOL		char*

typedef INTEGER	ATTR;

typedef INTEGER	PERSNO;

typedef INTEGER	SEX;

typedef INTEGER	TIJD;


typedef INTEGER	WEDSCAT;

typedef STRING	CLUBNO;

typedef STRING	ADR;

typedef STRING	WNPL;

typedef STRING	KNLTB;

typedef STRING	GEB;

typedef STRING	STERK;

typedef STRING	COMMENT;

typedef
  struct key_struct {
	unsigned char fno;
	union {
		INTEGER fkey;
		CHAR char_0;
	} u;
  } KEY;


typedef
  struct perslst_struct {
	unsigned char fno;
	PERSNO SpNum;
	struct perslst_struct *next;
  } PERSLST;

typedef
  struct verhl_struct {
	unsigned char fno;
	struct period_struct *period;
	struct verhl_struct *next;
  } VERHL;

#define period_per		1

typedef
  struct period_struct {
	unsigned char fno;
	TIJD VanTijd;
	TIJD TotTijd;
  } PERIOD;


typedef
  struct stringlist_struct {
	unsigned char fno;
	STRING string;
	struct stringlist_struct *next;
  } STRINGLIST;

#define naw_naw			1

typedef
  struct naw_struct {
	unsigned char fno;
	PERSNO persno;
	CLUBNO clubno;
	ADR adr;
	WNPL wnpl;
	KNLTB knltb;
	GEB geb;
	STERK sterk;
	COMMENT comment;
  } NAW;


#define janee_ja		1
#define janee_nee		2
#define janee_omheteven		3

typedef
  struct janee_struct {
	unsigned char fno;
  } JANEE;

#define sexe_m			1
#define sexe_v			2

typedef
  struct sexe_struct {
	unsigned char fno;
  } SEXE;

#define tgnst_s			1
#define tgnst_p			2
#define tgnst_bye		3
#define tgnst_onbekend		4
#define tgnst_gelijk_spel       5
#define tgnst_openplaats        6
#define tgnst_pw                7

typedef
  struct tgnst_struct {
	unsigned char fno;
	union {
		PERSNO s;
		struct {
			PERSNO persno;
			PERSNO persno_0;
		} p;
		PERSNO pw;
	} u;
  } TGNST;

#define wedstyp_e		1
#define wedstyp_d		2
#define wedstyp_g		3

typedef
  struct wedstyp_struct {
	unsigned char fno;
  } WEDSTYP;

typedef
  struct sexlist_struct {
	unsigned char fno;
	SEX sex;
	struct sexlist_struct *next;
  } SEXLIST;

typedef
  struct rlist_struct {
	unsigned char fno;
	struct rec_struct *rec;
	struct rlist_struct *next;
  } RLIST;

#define rec_rec			1

typedef
  struct rec_struct {
	unsigned char fno;
	INTEGER integer;
	INTEGER integer_0;
	INTEGER integer_1;
	INTEGER integer_2;
	STRING string;
	struct tijdl_struct *tijdl;
  } REC;

typedef
  struct tijdl_struct {
	unsigned char fno;
	TIJD tijd;
	struct tijdl_struct *next;
  } TIJDL;

typedef
  struct integerlist_struct {
	unsigned char fno;
	INTEGER integer;
	struct integerlist_struct *next;
  } INTEGERLIST;

#define rsoort_poel		1
#define rsoort_afval		2

typedef
  struct rsoort_struct {
	unsigned char fno;
  } RSOORT;

typedef
  struct wpl_struct {
	unsigned char fno;
	struct wps_struct *wps;
	struct wpl_struct *next;
  } WPL;

#define wps_wp			1

typedef
  struct wps_struct {
	unsigned char fno;
	INTEGER wedsno;
	WEDSCAT wedscat;
	INTEGER prioriteit;
  } WPS;

typedef
  struct wcidl_struct {
	unsigned char fno;
	struct wcid_struct *wcid;
	struct wcidl_struct *next;
  } WCIDL;

#define wcid_wi			1

typedef
  struct wcid_struct {
	unsigned char fno;
	WEDSCAT wedscat;
	STRING string;
  } WCID;

typedef
  struct catl_struct {
	unsigned char fno;
	WEDSCAT wedscat;
	struct catl_struct *next;
  } CATL;

typedef
  struct dagcl_struct {
	unsigned char fno;
	struct dagc_struct *dagc;
	struct dagcl_struct *next;
  } DAGCL;

#define dagc_dagc		1

typedef
  struct dagc_struct {
	unsigned char fno;
	INTEGER integer;
	STRING string;
	struct wrk_struct *wrk;
  } DAGC;

#define wrk_z			1
#define wrk_w			2

typedef
  struct wrk_struct {
	unsigned char fno;
  } WRK;

typedef
  struct bncl_struct {
	unsigned char fno;
	struct bnc_struct *bnc;
	struct bncl_struct *next;
  } BNCL;

#define bnc_bnc			1

typedef
  struct bnc_struct {
	unsigned char fno;
	TIJD tijd;
	INTEGER integer;
  } BNC;

typedef
  struct t_struct  {
  TIJD     Tijdstip;
  INTEGER  BanenOver;
  } TIJDSTR;

void readkey_0(KEY **key);
/*
void _stdcall hasp22(INTEGER integer,INTEGER integer_0,INTEGER *integer_1,INTEGER *integer_2,INTEGER *integer_3,INTEGER *integer_4,INTEGER *integer_5,INTEGER *integer_6);
*/
void sortstr_c(STRINGLIST *stringlist,INTEGER integer);

void my_print_c(INTEGER integer,INTEGER integer_0);

void prt_header(STRING *string, INTEGER integer);

void _stdcall sp_for_c(PERSNO *persno,SEX *sex,SYMBOL *symbol,STRING *string,VERHL **verhl,long *offset,JANEE **janee,TIJD *tijd);

void _stdcall sp_for_c_tel(PERSNO persno,SEX *sex,SYMBOL *symbol,STRING *string,STRING *tel2, STRING *tel3, VERHL **verhl,long *offset,JANEE **janee,TIJD *tijd, INTEGER *Leeftijd);

void _stdcall spsrt_for_c(SEX *sex,STRING *string,SEXE **sexe, INTEGER *lft1, INTEGER *lft2, STRING *strk1, STRING *strk2, VERHL **verhl);

void _stdcall opg_for_c_0(INTEGER *integer,WEDSCAT *wedscat,TGNST **tgnst,INTEGER *integer_0,INTEGER *integer_1,INTEGER *integer_2,TIJD *tijd);

void _stdcall opg_for_c_1(INTEGER *integer,WEDSCAT wedscat,TGNST **tgnst,INTEGER *integer_0,INTEGER *integer_1,INTEGER *integer_2,TIJD *tijd);

void _stdcall wc_for_c_0(WEDSCAT *wedscat,STRING *string,WEDSTYP **wedstyp,STRING *string_0,SEXLIST **sexlist,double *real,WEDSCAT *wedscat_0);

void _stdcall wc_for_c_1(WEDSCAT wedscat,STRING *string,WEDSTYP **wedstyp,STRING *string_0,SEXLIST **sexlist,double *real,WEDSCAT *wedscat_0);

void _stdcall pos_for_c(WEDSCAT wedscat,INTEGER *card,INTEGER *pos,
							RSOORT **rsoort);

void myloting(RLIST *rlist,INTEGERLIST **integerlist,INTEGER *integer,RSOORT *rsoort);

void mysort_wed(WPL *wpl,INTEGER *integer,WEDSCAT *wedscat);
			/* i, o, o */
void mysort_int(TIJDL *tijdl);

void _stdcall sp2opg_0(INTEGER integer,WEDSCAT wedscat,INTEGER *integer_0,
		PERSNO *persno,INTEGER *integer_1,TGNST **tgnst);

void _stdcall sp2opg_1(INTEGER integer,WEDSCAT *wedscat,INTEGER *integer_0,
		PERSNO *persno,INTEGER *integer_1,TGNST **tgnst);


void _stdcall wd_for_c_0(INTEGER *integer,WEDSCAT wedscat,TGNST **tgnst,TGNST **tgnst_0,STRING *string,TIJD *tijd);
			/* (o, i, o, o, o, o) */
void _stdcall wd_for_c_1(INTEGER *integer,WEDSCAT wedscat,TGNST *tgnst,TGNST **tgnst_0,STRING *string,TIJD *tijd);
			/* (o, i, i, o, o, o) */
void _stdcall wd_for_c_2(INTEGER *integer,WEDSCAT wedscat,TGNST **tgnst,TGNST *tgnst_0,STRING *string,TIJD *tijd);
			/* (o, i, o, i, o, o) */
void _stdcall wd_for_c_3(INTEGER integer,WEDSCAT wedscat,TGNST **tgnst,TGNST **tgnst_0,char **string,TIJD *tijd);
			/* (i, i, o, o, o, o) */
void _stdcall w2_for_c_0(INTEGER *integer,WEDSCAT wedscat,TIJD *tijd,JANEE **janee,JANEE **janee_0,CHAR *char_0,TIJD *tijd_0);
			/* (o, i, o, o, o, o, o) */
void _stdcall w2_for_c_1(INTEGER *integer,WEDSCAT *wedscat,TIJD *tijd,JANEE **janee,JANEE **janee_0,CHAR *char_0,TIJD *tijd_0);
			/* (o, o, o, o, o, o, o) */
void _stdcall w2_for_c_2(INTEGER integer,WEDSCAT wedscat,TIJD *tijd,JANEE **janee,JANEE **janee_0,CHAR *char_0,TIJD *tijd_0);
			/* (i, i, o, o, o, o, o) */
void _stdcall w3_for_c_0(INTEGER *integer,WEDSCAT wedscat,TGNST **tgnst,TIJD *tijd);
			/* (o, i, o, o) */
void _stdcall w3_for_c_1(INTEGER integer,WEDSCAT wedscat,TGNST **tgnst,TIJD *tijd);
			/* (i, i, o, o) */
void _stdcall bn_for_c(TIJD *tijd,INTEGER *integer);

void _stdcall dag_for_c_0(INTEGER *integer,STRING *string,WRK **wrk);
			/* (o, o, o) */
void _stdcall dag_for_c_1(INTEGER integer,STRING *string,WRK **wrk);
			/* (i, o, o) */
void betaal_adm_for_c_0(JANEE **BetaalAdm);

void _stdcall blok_for_c(WEDSCAT wedscat, VERHL **verhl);
void _stdcall blLft_for_c(INTEGER leeftijd, VERHL **verhl);

void _stdcall planparam_c(INTEGER *EersteDag, INTEGER *Hoeveel, INTEGER *PerWat, STRING *PerWatStr, INTEGER *PerDagen,
              INTEGER *Separatie, INTEGER *Duur, INTEGER *Enkel, INTEGER *DagMax);

void de_spelers(INTEGER integer,WEDSCAT wedscat,TGNST *tgnst,TGNST *tgnst_0,PERSLST **perslst);
void gekoppeld4c(TIJD BeginT, INTEGER Num, WEDSCAT wedscat, TGNST *t1, TGNST *t2, TIJD *tijd); /*- (i,i,i,i,i,o)*/
void tijd_kwart_1(INTEGER *integer,INTEGER *integer_0,INTEGER *integer_1,TIJD tijd);

void helestr_int_0(INTEGER integer,STRING *string,INTEGER integer_0);

void myshiftleft(INTEGER getal, INTEGER Aantalschuif, INTEGER *resultaat); 

void print_tijd(TIJD Tijd)
{
INTEGER Dag1;
INTEGER Uur1;
INTEGER Minuten1;
STRING  Minuten1S;
	    tijd_kwart_1(&Dag1, &Uur1, &Minuten1, Tijd);
	    helestr_int_0(0,&Minuten1S,Minuten1);
	    IO_Writef("%2d %2d.%-2s ",
				     Dag1, Uur1, Minuten1S);
}

static TIJDSTR *binsearch(TIJD Tijd, TIJDSTR *Low, TIJDSTR *High)
{
TIJDSTR *Mid;

  if (Tijd < Low->Tijdstip) return Low;
  while (Low < High) {
    Mid = Low + (unsigned int) (High - Low) / 2;
    if (Tijd < Mid->Tijdstip)
      High = Mid;
    else if (Tijd > Mid->Tijdstip)
      Low = Mid + 1;
    else
      return Mid;
  }
  return Low;
}

static int mycomp_tijd(const void *T1, const void *T2)

{
  return(((TIJDSTR *)T1)->Tijdstip - ((TIJDSTR *)T2)->Tijdstip);
}

/*********** vindt gaatjes om een wedstrijd te schedulen *******************/

INTEGER static dimensie(INTEGER WedsNo)
{
INTEGER Dimensie = 0;

while(WedsNo > 1) {
  WedsNo >>= 1;
  Dimensie++;
}
return Dimensie;
}


/**********************************************************
  sorteren van wedstrijden naar ronde wordt alleen nog
  gebruikt in tasprnt.pro voor de daguitslagen
 **********************************************************/

static int mycomp_weds(const void *int1, const void *int2)
{
  INTEGER dim;

  dim = dimensie(*(int *)int2) - dimensie(*(int *)int1);
  if (dim) {		/* NB kronkel van mij -- dimensie hoeft */
    return dim;         /* eigenlijk niet erbij betrokken       */
  }
  return(*(int *)int1 - *(int *)int2);
}


void sortweds_c(getallist *inlist)
{
  getallist *savelist = inlist;
  int  *array_int;
  int i,n;

  for(i=0; inlist->functor==LISTFNO; inlist=inlist->next) i++;
  n = i;
  array_int =  MEM_AllocGStack(i*sizeof(int));
  inlist = savelist;
  for (i = 0; inlist->functor==LISTFNO; inlist=inlist->next, i++) {
    array_int[i]  = inlist->getal;
  }
  qsort(array_int, n, sizeof(array_int[0]), mycomp_weds);

  inlist = savelist;
  for (i = 0; inlist->functor==LISTFNO; inlist=inlist->next) {
    inlist->getal = array_int[i++];
  }
}



void _cdecl scan_planning_c(INTEGER Aantal_Opl, TIJD BeginTijd,
	   PERSLST *Spelers, INTEGER No, WEDSCAT Cat, TIJD *Tijd, INTEGER *Banen_Over)
{   /* NB er wordt nog maar één oplossing verlangd! Redesign ??! */
TIJD     Plan;
JANEE    *Gewaarsch_1;
JANEE    *Gewaarsch_2;
CHAR     Fixed;
TIJD     Vrij;
TIJD     DagBegin;
TIJD     BanenTijd;
TIJD     OudePlan = 0;
INTEGER  BanenTal;
TIJDSTR  *TijdArray, *TijdCurr, *TijdEnd;
INTEGER  AantalTijds = 0;
PERSLST  *TPers;
SEX	 Sex;
SYMBOL   Naam;
STRING   Telefoon;
  STRING   Telefoon2;
  STRING   Telefoon3;
VERHL    *Verhinderd;
long	 Offset;
JANEE	 *Betaald;
TIJD	 UitKant;
INTEGER  Leeftijd;
TIJD     UitKantMax;
INTEGER  DagNo;
INTEGER  DagNoC, DagNoC1, DagNoC2, PerBeginC; /* begin- en einddag van periode */
STRING   Datum;
WRK      *DagType;
unsigned char DagTypeC;
unsigned Top;
INTEGER  WedsNo;
TGNST    *Tgnst1;
TGNST    *Tgnst2;
STRING   Uitslag;
char     Vlag;
TIJD     Tijd1;
INTEGER  AantalWeds; 	/* max aantal per dag               */
INTEGER  AantalWperO;   /* max aantal per dag per onderdeel */
INTEGER  AantalSingles; /* max aantal singles               */
WEDSCAT  Wedscat;
PERSLST  *Deelnemers;
PERSLST  *Dlnmr;
struct Speler_Plan {
  INTEGER SpNo;
  INTEGER DagNo;
  INTEGER WedsNo;
  WEDSCAT Cat;
  unsigned char EDG;		/* enkel of dubbel */
  unsigned char Afv_Poel;
  } *SpPlan;
INTEGER ISpeler = 0;
INTEGER ISp1;
INTEGER ISpMax;
INTEGER Card;
INTEGER Pos;
RSOORT  *Afval_of_Poel;
unsigned char Enkel_of_dubbel;
double   Geld;
STRING   Onderdeel;
WEDSTYP  *Wedstyp;
STRING   KorteNaam;
SEXLIST  *Sexlist;
WEDSCAT  VerliezersR;
INTEGER  EersteDag, DagMaxO, PerWat, PerDagen, Separatie, Duur, Enkel, DagMax;
STRING   PerWatStr;

  planparam_c(&EersteDag, &DagMaxO, &PerWat, &PerWatStr, &PerDagen, &Separatie, &Duur, &Enkel, &DagMax);
  TijdArray = MEM_AllocGStack(3000 * sizeof(TijdArray[0]));
					/* genoeg voor 3000 tijdstippen */

  TijdCurr = TijdArray;			/* vul het array met tijdstippen */
  if (!RUN_StackBTrack()) {
  bn_for_c(&BanenTijd,&BanenTal);
    AantalTijds++;
    TijdCurr->Tijdstip      = BanenTijd;
    (TijdCurr++)->BanenOver = (BanenTijd >= BeginTijd ? BanenTal : 0);
  RUN_Fail();
  };
  TijdEnd = TijdCurr;

  /*______________________________________________________________________*/
  /*  sorteer om een linear search mogelijk te maken                      */
  /*______________________________________________________________________*/
  qsort(TijdArray, AantalTijds, sizeof(TijdArray[0]), mycomp_tijd);

  /*______________________________________________________________________*/
  /*kijk of er blokkeringen voor deze categorie bestaan                   */
  /*______________________________________________________________________*/
  Top = GETSP;
  if (!RUN_StackBTrack()) {
  blok_for_c(Cat,&Verhinderd);
    for(TijdCurr = TijdArray; TijdCurr != TijdEnd ; TijdCurr++) {
      while (Verhinderd->fno == LISTFNO &&
	     TijdCurr->Tijdstip > Verhinderd->period->TotTijd)
	Verhinderd = Verhinderd->next;
      if(Verhinderd->fno != LISTFNO) break;
      if(TijdCurr->Tijdstip >= Verhinderd->period->VanTijd &&
	 TijdCurr->Tijdstip < Verhinderd->period->TotTijd)
	   TijdCurr->BanenOver = 0;
      }
    RUN_Cut(Top);
    }
  wc_for_c_1(Cat, &Onderdeel, &Wedstyp, &KorteNaam, &Sexlist, &Geld, &VerliezersR);
  Enkel_of_dubbel = Wedstyp->fno;

  /*  for(TijdCurr = TijdArray; TijdCurr != TijdEnd ; TijdCurr++) {
      print_tijd(TijdCurr->Tijdstip);
      IO_Writef(" %d\t", TijdCurr->BanenOver);
    }; */
  /*______________________________________________________________________*/
  /* loop door alle spelers                                               */
  /*______________________________________________________________________*/
  SpPlan = MEM_AllocGStack(500 * sizeof(struct Speler_Plan));
					/* schrap de verhinderde tijden */
  UitKantMax = 0;
  for (TPers = Spelers; TPers->fno==LISTFNO; TPers=TPers->next) {
    sp_for_c_tel(TPers->SpNum,&Sex,&Naam,&Telefoon,&Telefoon2,&Telefoon3,&Verhinderd,&Offset,&Betaald,&UitKant,&Leeftijd);
    /*IO_Writef("\nSpeler: %d %s", TPers->SpNum, Naam);*/
    for( ; Verhinderd->fno==LISTFNO ; Verhinderd = Verhinderd->next) {
      TijdCurr = binsearch(Verhinderd->period->VanTijd - Duur + 1, TijdArray, TijdEnd); /* duur */
      for (; TijdCurr != TijdEnd && TijdCurr->Tijdstip < Verhinderd->period->TotTijd ; TijdCurr++) {
	TijdCurr->BanenOver = 0;
      }
    };
    if (UitKant > UitKantMax) UitKantMax = UitKant;  /* bewaar max uitkant */
    /*______________________________________________________________________*/
    /*kijk of er blokkeringen voor deze leeftijd bestaan                   */
    /*______________________________________________________________________*/
    Top = GETSP;
    if (!RUN_StackBTrack()) {
    blLft_for_c(Leeftijd,&Verhinderd);
      for(TijdCurr = TijdArray; TijdCurr != TijdEnd ; TijdCurr++) {
        while (Verhinderd->fno == LISTFNO &&
	     TijdCurr->Tijdstip > Verhinderd->period->TotTijd)
	  Verhinderd = Verhinderd->next;
        if(Verhinderd->fno != LISTFNO) break;
        if(TijdCurr->Tijdstip >= Verhinderd->period->VanTijd &&
	   TijdCurr->Tijdstip < Verhinderd->period->TotTijd)
	   TijdCurr->BanenOver = 0;
        }
      RUN_Cut(Top);
      }
  };  /* volgende speler */
  DagNoC = (TijdArray->Tijdstip) >> 7;
  DagNoC--;				/* init op niet bestaande waarde */
  for(TijdCurr = TijdArray; TijdCurr != TijdEnd ; TijdCurr++) { /* ga alle tijdstippen af */
    if (!TijdCurr->BanenOver) continue;
    if ((TijdCurr->Tijdstip >> 7) != DagNoC) {	/* nieuwe dag */
      Top          = GETSP;
      dag_for_c_1((TijdCurr->Tijdstip >> 7),&Datum,&DagType);
      DagTypeC     = DagType->fno;
      DagNoC       = (TijdCurr->Tijdstip >> 7);
      RUN_Cut(Top);
    }
    if (DagTypeC == wrk_w && (TijdCurr->Tijdstip & 0x7F) < UitKantMax)
      TijdCurr->BanenOver = 0;			/* schrap i.g.v. werkdag en vóór uitkant */
  }
/*_______________________________________ alle planning _____________________________________*/
  if (!RUN_StackBTrack()) {             
    w2_for_c_1(&WedsNo,&Wedscat,&Plan,&Gewaarsch_1,&Gewaarsch_2,&Fixed,&Vrij);
    if (Wedscat == Cat && WedsNo == No) {
      OudePlan = Plan;
      RUN_Fail();		/* eventuele eigen oude planning doet niet mee */
      }
    TijdCurr = binsearch(Plan, TijdArray, TijdEnd); /* zoek tijdst zelf   */
    if (TijdCurr->Tijdstip == Plan && TijdCurr->BanenOver > 0) {
      TijdCurr->BanenOver--; /* alleen als  */
    };
       
    Top = GETSP;                                       /*    gevonden */
    wd_for_c_3(WedsNo,Wedscat,&Tgnst1,&Tgnst2,&Uitslag,&Tijd1);
    de_spelers(WedsNo,Wedscat,Tgnst1,Tgnst2,&Deelnemers);
    pos_for_c(Wedscat, &Card, &Pos, &Afval_of_Poel);
    wc_for_c_1(Wedscat, &Onderdeel, &Wedstyp, &KorteNaam, &Sexlist, &Geld, &VerliezersR);
    RUN_Cut(Top);
    Vlag = 0;          		/* ga checken wie deelneemt */
    for (TPers = Spelers; TPers->fno==LISTFNO; TPers=TPers->next) {
      /*IO_Writef("\nSpeler: %d", TPers->SpNum);*/
      for (Dlnmr = Deelnemers; Dlnmr->fno==LISTFNO; Dlnmr=Dlnmr->next) {
	if (TPers->SpNum == Dlnmr->SpNum) {
	  (SpPlan + ISpeler)->SpNo       = TPers->SpNum;
	  (SpPlan + ISpeler)->DagNo      = Plan >> 7;
	  (SpPlan + ISpeler)->WedsNo     = WedsNo;
	  (SpPlan + ISpeler)->Cat        = Wedscat;
	  (SpPlan + ISpeler)->EDG        = Wedstyp->fno;
	  (SpPlan + ISpeler++)->Afv_Poel = Afval_of_Poel->fno;
	  Vlag++;       /* één van de spelers speelt al elders */
	  if (ISpeler > 500) RUN_Exit(212); /* te veel ... */
	};
      };
    };
    if (Vlag) {		/* zet tijdstippen eromheen op verhinderd*/
      TijdCurr = binsearch(Plan - Separatie + 1, TijdArray, TijdEnd);   /* de separatie */
      for(; (TijdCurr->Tijdstip < (Plan + Separatie)) &&		/* de separatie */
						(TijdCurr != TijdEnd);
					TijdCurr++) {
	  TijdCurr->BanenOver = 0;
      };
    };
    RUN_Fail();		/* ga alle planning langs */
  };
/*__________________________________ einde alle planning _____________________________________*/
  ISpMax = ISpeler;
  /*__________________________________________________________________
  for(ISpeler = 0 ; ISpeler < ISpMax ; ISpeler++)   spelerafsprakenarray??
    IO_Writef("\n %d %d %d", (SpPlan + ISpeler)->SpNo, (SpPlan + ISpeler)->DagNo,  (SpPlan + ISpeler)->Cat) ;
    __________________________________________________________________*/
  if (!RUN_StackBTrack()) {
    dag_for_c_0(&DagNo,&Datum,&DagType);
    DagNoC1 = DagNo - (DagNo - EersteDag) % PerDagen;
    DagNoC2 = DagNoC1 + PerDagen;
    for (TPers = Spelers; TPers->fno==LISTFNO; TPers=TPers->next) {
      /*IO_Writef("Sp %d ", TPers->SpNum);*/
      AantalWeds  = 0;
      AantalWperO = 1;	/* voor iedere speler */
      AantalSingles = 0;
      if (Enkel_of_dubbel == wedstyp_e) AantalSingles = 1;
      for(ISpeler = 0 ; ISpeler < ISpMax ; ISpeler++) { /* spelerafsprakenarray??*/
        if ((SpPlan + ISpeler)->SpNo != TPers->SpNum) continue;
	if ((SpPlan + ISpeler)->DagNo == DagNo          ) {	/* zelfde speler en zelfde dag */
	  AantalWeds++;
	  ISp1 = ISpeler;
	  while (++ISp1 < ISpMax) {
	    if ((SpPlan + ISp1)->SpNo       == TPers->SpNum 		&&
		(SpPlan + ISp1)->DagNo      == DagNo 			&&
		(SpPlan + ISp1)->Cat        == (SpPlan + ISpeler)->Cat) {
	    	if ((SpPlan + ISp1++)->Afv_Poel == rsoort_afval)
		  AantalWeds++;		/* afval-wedsrijd (zelfde soort) telt dubbel */
  	    } /* if */
	  } /* while */
   	} /* if */
      /*IO_Writef("\n DagNoC1: %d DagNoC2: %d DagNo: %d TPers: %d AantalWperO: %d DagMaxO: %d", DagNoC1, DagNoC2, DagNo, TPers->SpNum, AantalWperO, DagMaxO);*/
   	if (AantalSingles && (SpPlan + ISpeler)->EDG == wedstyp_e && (SpPlan+ISpeler)->DagNo == DagNo ) AantalSingles++;
	if ((SpPlan + ISpeler)->Cat == Cat && (SpPlan+ISpeler)->DagNo == DagNo) {		/* te plannen wedstrijd meetellen */
	   if ((SpPlan + ISpeler)->Afv_Poel == rsoort_afval)
		 AantalWeds++; 					  /* idem */
	} /* if */
	if ((SpPlan + ISpeler)->Cat == Cat && (SpPlan+ISpeler)->DagNo == DagNo) {		/* te plannen wedstrijd meetellen */
	   if (PerWat == 0) AantalWperO++;		/* 23.6.2005 */
        }
        if (PerWat != 0 &&
	    (SpPlan + ISpeler)->DagNo >= DagNoC1 &&
	    (SpPlan + ISpeler)->DagNo <  DagNoC2 &&
    	    (SpPlan + ISpeler)->Cat   == Cat) { /* zelfde speler en periode en Cat */
            AantalWperO++;
      /*IO_Writef("\n++  DagNoC1: %d DagNoC2: %d DagNo: %d TPers: %d AantalWperO: %d DagMaxO: %d", DagNoC1, DagNoC2, DagNo, TPers->SpNum, AantalWperO, DagMaxO);*/
        } /* if */
      }; /* for */					/* het hele spelerplan */
      if (AantalWeds > DagMax || AantalWperO > DagMaxO || AantalSingles > Enkel) {
	DagBegin = DagNo << 7;
	TijdCurr = binsearch(DagBegin, TijdArray, TijdEnd);
	for(DagBegin += 0x80; TijdCurr->Tijdstip < DagBegin && TijdCurr != TijdEnd; TijdCurr++)
          TijdCurr->BanenOver = 0;
      };
    };						/* alle personen       */
    /*for(TijdCurr = TijdArray; TijdCurr != TijdEnd ; TijdCurr++) {
        print_tijd(TijdCurr->Tijdstip);
      IO_Writef(" %2d", TijdCurr->BanenOver);
    };*/
    RUN_Fail();			              	/* ga alle dagen langs */
  };
  for (TijdCurr = TijdArray; TijdCurr != TijdEnd; TijdCurr++)
  {                                  /* nondeterm tot Aantal_Opl oplossingen */
    if(TijdCurr->BanenOver <= 0) continue;
    if(TijdCurr->Tijdstip == OudePlan) continue; /* sla eventueel bestaand plan over */
    if(!Aantal_Opl--) break;
    if(RUN_StackBTrack()) continue;
    *Tijd       = TijdCurr->Tijdstip;
    *Banen_Over = TijdCurr->BanenOver;
    RUN_JmpReturn();
  }
  RUN_Fail();
}


#define reden_verhinderd  0x01
#define reden_beschikbaar 0x02
#define reden_speelt_enk  0x04
#define reden_geen_baan   0x08
#define reden_teveel_weds 0x10
#define reden_dag_max_per 0x20
#define reden_speelt_dub  0x40
#define reden_speelt_gd   0x80
#define reden_vorige_niet 0x0100
#define reden_vorige_laat 0x0200
#define reden_cat_blok    0x0400
#define reden_koppel      0x0800
#define reden_singles     0x1000


void _stdcall check_forceer_c(INTEGER Mode, TIJDL *Tijden,
		  INTEGER WdNo, WEDSCAT Cat,  /* de wedstr zelf */
		/*  INTEGER Separatie, INTEGER Duur, INTEGER Singles, INTEGER DagMax, INTEGER DagMaxO,*/
		  STRING *TijdenStr, INTEGER *ResultP, STRING *Outstring, INTEGER *ResTijdP)

/*___________________________________________________________*/
/* check of er wat mankeert en geef Result <> 0              */
/*      als er wat mankeert                                  */
/* als mode = 1, schrijf dan ook waarom naar het venster     */
/*___________________________________________________________*/

{
unsigned Top;
unsigned Top1;
PERSLST  *TPers;
PERSLST  *Spelers;
SEX	 Sex;
SYMBOL   Naam;
STRING   Telefoon;
  STRING   Telefoon2;
  STRING   Telefoon3;
VERHL    *Verhinderd;
long	 Offset;
JANEE	 *Betaald;
TIJD	 UitKant;
INTEGER  Leeftijd;
INTEGER  DagNo, DagNoC1, DagNoC2;
INTEGER  DagNo1;
STRING   Datum;
WRK      *DagType;
TIJD     BanenTijd;
INTEGER  BanenTal = 0;  /* voor het geval dat er geen baanplanning is */
TIJD     Plan;
TIJD     VorigPlan1;
TIJD	 VorigPlan2;
TIJD     KoppelTijd;
INTEGER  VorigWeds;
JANEE    *Gewaarsch_1;
JANEE    *Gewaarsch_2;
CHAR     Fixed;
TIJD     Vrij;
INTEGER  WedsNo;
WEDSCAT  Wedscat;
PERSLST  *Deelnemers;
PERSLST  *Dlnmr;
TGNST    *Tgnst1;
TGNST    *Tgnst2;
TGNST    *Tgnsta;
TGNST    *Tgnstb;
STRING   Uitslag;
TIJD     Tijd1;
INTEGER  AantalWeds;
INTEGER  AantalWedO;	/* max aantal wedstrijden per dag per onderdeel */
INTEGER  AantalSingles; /* aantal enkelspelen                           */
struct Speler_Plan {
  INTEGER SpNo;
  INTEGER DagNo;
  INTEGER WedsNo;
  WEDSCAT Cat;
  unsigned char    EDG;
  } *SpPlan;
TIJDL   *TTijden;
struct Plan_struct {
  TIJD    Plan_Tijd;
  INTEGER Plan_Banen;
  INTEGER Plan_Bezwaren;
  } *PlanP;
INTEGER PlanInd;
INTEGER naamnodig;
INTEGER PlanAant;
INTEGER ISpeler = 0;
INTEGER ISp1;
INTEGER ISpMax;
INTEGER Dag1;
INTEGER Uur1;
INTEGER Minuten1;
STRING  Minuten1S;
INTEGER Uur2;
INTEGER Minuten2;
STRING  Minuten2S;
STRING  Wedsnaam;
WEDSTYP *Wedstyp;
STRING  Naamkort;
SEXLIST *Sexlist;
double  Geld;   /* plaats voor real */
WEDSCAT Verl;
INTEGER Write_Mode;
INTEGER Baan_Count;
TIJD    Upper, Lower;
unsigned char EDG_eigen;
STRING   Area;
INTEGER  EersteDag, DagMaxO, PerWat, PerDagen, Separatie, Duur, Enkel, DagMax;
STRING   PerWatStr;

  planparam_c(&EersteDag, &DagMaxO, &PerWat, &PerWatStr, &PerDagen, &Separatie, &Duur, &Enkel, &DagMax);
  Write_Mode = Mode & 1; /* bit 1 is fixeer, bit 2 = controleer (bezwarenrapport; geen banen */
  Baan_Count = !(Mode & 2);
  Area = MEM_AllocGStack(10000);
  *Area = 0;
  *Outstring = Area;
  /*  if (Mode & 2) Baan_Count = 0; else Baan_Count = 1;*/
  for(PlanAant = 0, TTijden = Tijden ; /* tel het aantal te checken tijdst. */
		TTijden->fno==LISTFNO; TTijden = TTijden->next, PlanAant++);
  PlanP  = MEM_AllocGStack(PlanAant * sizeof(PlanP[0]));
  for(TTijden = Tijden, PlanInd = 0 ; TTijden->fno==LISTFNO ;
			       TTijden = TTijden->next, PlanInd++) {
    (PlanP+PlanInd)->Plan_Tijd     = TTijden->tijd;
    (PlanP+PlanInd)->Plan_Banen    = 0;
    (PlanP+PlanInd)->Plan_Bezwaren = 0;
  }
  DagNo = PlanP->Plan_Tijd >> 7;  /* alle tijden van dezelfde dag! !!!! */
  SpPlan = MEM_AllocGStack(500 * sizeof(SpPlan[0]));
 /*_____________________________________________________________________*/
  if (!RUN_StackBTrack()) {
    blok_for_c(Cat,&Verhinderd);
    for(PlanInd = 0; PlanInd < PlanAant ; PlanInd++) {
      while (Verhinderd->fno == LISTFNO &&
	     (PlanP+PlanInd)->Plan_Tijd > Verhinderd->period->TotTijd)
	Verhinderd = Verhinderd->next;
      if(Verhinderd->fno != LISTFNO) break;
      if((PlanP+PlanInd)->Plan_Tijd >= Verhinderd->period->VanTijd &&
	   (PlanP+PlanInd)->Plan_Tijd < Verhinderd->period->TotTijd) {
	(PlanP+PlanInd)->Plan_Bezwaren |= reden_cat_blok;
	if (Write_Mode) Area += IO_SPrintf(Area,"\nTijd geblokkeerd voor dit onderdeel.");
	}
      }
    RUN_Fail();
    }
  /*_____________________________________________________________________*/
  Top = GETSP;
  VorigPlan1 = 0;
  VorigPlan2 = 0;
  VorigWeds  = 0;
  wd_for_c_3(WdNo,Cat,&Tgnst1,&Tgnst2,&Uitslag,&Tijd1);
  if (Tgnst1->fno == tgnst_onbekend) {
    WedsNo = WdNo << 1;
    if (!RUN_StackBTrack()) {	/* is er een vorig wedstrijd? */
      wd_for_c_3(WedsNo,Cat,&Tgnsta,&Tgnstb,&Uitslag,&Tijd1);
      VorigWeds = 1;
      RUN_Cut(Top);
      }
    if (VorigWeds) {  
      if (!RUN_StackBTrack()) {	/* is er een vorig plan? */
        w2_for_c_2(WedsNo,Cat,&VorigPlan1,&Gewaarsch_1,&Gewaarsch_2,&Fixed,&Vrij);
        RUN_Cut(Top);
        }
      else VorigPlan1 = -1;	/* mist */  
      }
    }
  if (Tgnst2->fno == tgnst_onbekend) {
    VorigWeds = 0;
    WedsNo = (WdNo << 1) + 1;
    if (!RUN_StackBTrack()) {	/* is er een vorig wedstrijd? */
      wd_for_c_3(WedsNo,Cat,&Tgnsta,&Tgnstb,&Uitslag,&Tijd1);
      VorigWeds = 1;
      RUN_Cut(Top);
      }
    if (VorigWeds) {  
      if (!RUN_StackBTrack()) {	/* is er een vorig plan? */
        w2_for_c_2(WedsNo,Cat,&VorigPlan2,&Gewaarsch_1,&Gewaarsch_2,&Fixed,&Vrij);
        RUN_Cut(Top);
        }
      else VorigPlan2 = -1;
      }
    }
  if (!RUN_StackBTrack()) {	/* is er gekoppeld? */
    KoppelTijd = 0;
    gekoppeld4c(0, WdNo, Cat, Tgnst1, Tgnst2, &KoppelTijd);
    RUN_Cut(Top);
    }
  for(PlanInd = 0; PlanInd < PlanAant ; PlanInd++) {
    if (VorigPlan1 == -1 || VorigPlan2 == -1) {
      (PlanP+PlanInd)->Plan_Bezwaren |= reden_vorige_niet;
      if (Write_Mode) Area += IO_SPrintf(Area,"\nVoorgaande ronde niet gepland.");
      }
    if (  (PlanP+PlanInd)->Plan_Tijd < VorigPlan1 + Separatie		/* separatie */
       || (PlanP+PlanInd)->Plan_Tijd < VorigPlan2 + Separatie) {	/* separatie */
      (PlanP+PlanInd)->Plan_Bezwaren |= reden_vorige_laat;
      if (Write_Mode) Area += IO_SPrintf(Area,"\nVoorgaande wedstrijd(en) niet afgelopen.");
      }
    if ((PlanP+PlanInd)->Plan_Tijd < KoppelTijd) {
      (PlanP+PlanInd)->Plan_Bezwaren |= reden_koppel;
      if (Write_Mode) Area += IO_SPrintf(Area,"\nVoorronde niet afgelopen.");
      }
    }
  /*_____________________________________________________________________*/
  de_spelers(WdNo,Cat,Tgnst1,Tgnst2,&Spelers);
  RUN_Cut(Top);
  for (TPers = Spelers; TPers->fno==LISTFNO; TPers=TPers->next) {
    sp_for_c_tel(TPers->SpNum,&Sex,&Naam,&Telefoon,&Telefoon2,&Telefoon3,&Verhinderd,&Offset,
							&Betaald,&UitKant,&Leeftijd);
    naamnodig = 0;
    /*if (Write_Mode) Area += IO_SPrintf(Area,"\n%-20.20s tel %s %s %s", Naam, Telefoon, Telefoon2,Telefoon3);*/
    for( ; Verhinderd->fno==LISTFNO ; Verhinderd = Verhinderd->next) {
      for(PlanInd = 0 ; PlanInd < PlanAant ; PlanInd++) {
	if ((PlanP+PlanInd)->Plan_Tijd > Verhinderd->period->VanTijd
							       - Duur &&	/* duur OK */
	    (PlanP+PlanInd)->Plan_Tijd < Verhinderd->period->TotTijd) {
	  if (Write_Mode) {
	    tijd_kwart_1(&Dag1, &Uur1, &Minuten1, Verhinderd->period->VanTijd);
	    helestr_int_0(0,&Minuten1S,Minuten1);
	    tijd_kwart_1(&Dag1, &Uur2, &Minuten2, Verhinderd->period->TotTijd);
	    helestr_int_0(0,&Minuten2S,Minuten2);
            if (naamnodig == 0) {
              Area += IO_SPrintf(Area,"\n%-20.20s", Naam);
              naamnodig = 1;
              }
	    Area += IO_SPrintf(Area,
		   "\n%2d.%-2s tot %2d.%-2s verhinderd.",
		     Uur1, Minuten1S, Uur2, Minuten2S, DagNo);
	  } ;
	  (PlanP+PlanInd)->Plan_Bezwaren |= reden_verhinderd;
	};
      };
    };
    if (!RUN_StackBTrack()) {
    dag_for_c_0(&DagNo1, &Datum, &DagType);
      for(PlanInd = 0 ; PlanInd < PlanAant ; PlanInd++) {
	if (DagNo1 == DagNo &&
	    DagType->fno == wrk_w &&
	    ((PlanP+PlanInd)->Plan_Tijd & 0x7F) < UitKant) {
	  if (Write_Mode) {
	    if (naamnodig == 0) {
              Area += IO_SPrintf(Area,"\n%-20.20s", Naam);
              naamnodig = 1;
            }
            Area += IO_SPrintf(Area, "\nNog niet beschikbaar.");
          }
	  (PlanP+PlanInd)->Plan_Bezwaren |= reden_beschikbaar;
	}
      }
    RUN_Fail();
    }
    /*_____________________________leeftijdsblokkade 12.8.2001_________*/
    if (!RUN_StackBTrack()) {
      blLft_for_c(Leeftijd,&Verhinderd);
      for(PlanInd = 0; PlanInd < PlanAant ; PlanInd++) {
        while (Verhinderd->fno == LISTFNO &&
	       (PlanP+PlanInd)->Plan_Tijd > Verhinderd->period->TotTijd)
	  Verhinderd = Verhinderd->next;
        if(Verhinderd->fno != LISTFNO) break;
         if((PlanP+PlanInd)->Plan_Tijd >= Verhinderd->period->VanTijd &&
	     (PlanP+PlanInd)->Plan_Tijd < Verhinderd->period->TotTijd) {
	  (PlanP+PlanInd)->Plan_Bezwaren |= reden_cat_blok;
	  if (Write_Mode) Area += IO_SPrintf(Area,"\nLeeftijd geblokkeerd.");
	}
      }
    RUN_Fail();
    }
  /*_____________________________________________________________________*/
  }				/* einde loop door de spelerslijst */
  /*_____________________________________________________________________*/
  if (!RUN_StackBTrack()) {
  bn_for_c(&BanenTijd,&BanenTal);
    for(PlanInd = 0 ; PlanInd < PlanAant ; PlanInd++) {
      if (BanenTijd == (PlanP+PlanInd)->Plan_Tijd)
			(PlanP+PlanInd)->Plan_Banen = BanenTal;
    }
  RUN_Fail();
  }
  wc_for_c_1(Cat,&Wedsnaam,&Wedstyp,&Naamkort,&Sexlist,&Geld,&Verl);
  EDG_eigen = Wedstyp->fno;
  /*_____________________________________________________________________*/
    DagNoC1 = DagNo - (DagNo - EersteDag) % PerDagen;
    DagNoC2 = DagNoC1 + PerDagen;
  if (!RUN_StackBTrack()) {             /* alle planning ...           	 */
    w2_for_c_1(&WedsNo,&Wedscat,&Plan,&Gewaarsch_1,&Gewaarsch_2,&Fixed,&Vrij);
    if (PerWat == 0)
      if ((Plan >> 7) != DagNo)             RUN_Fail();	/* ignore die    */
      else ;
    else 
      if ((Plan >> 7) < DagNoC1 || (Plan >> 7) >= DagNoC2) RUN_Fail();
    if (WedsNo == WdNo && Wedscat == Cat) RUN_Fail();   /* die ook       */
    Lower = Plan - Separatie;	/* separatie */
    if (!Baan_Count) Lower = Plan - 1;     /* smerig maar bevalt het best .. -1 op 30.8.2007 voor dubbele boeking */
    Upper = Plan + Separatie;	/* separatie */
    Top = GETSP;
    if (!RUN_StackBTrack()) {
      wd_for_c_3(WedsNo,Wedscat,&Tgnst1,&Tgnst2,&Uitslag,&Tijd1);
      de_spelers(WedsNo,Wedscat,Tgnst1,Tgnst2,&Deelnemers);
      for (TPers = Spelers; TPers->fno==LISTFNO; TPers=TPers->next) {
	for (Dlnmr = Deelnemers; Dlnmr->fno==LISTFNO; Dlnmr=Dlnmr->next) {
	  if (TPers->SpNum == Dlnmr->SpNum) {
	    (SpPlan + ISpeler)->SpNo     = TPers->SpNum;
	    (SpPlan + ISpeler)->DagNo    = Plan >> 7; /* 	% 1.12.2002 !!!!!! */
	    /*	      (SpPlan + ISpeler)->WedsNo   = WedsNo;	*/
	    (SpPlan + ISpeler)->Cat    = Wedscat;
 	    Top1 = GETSP;
  	    wc_for_c_1(Wedscat,&Wedsnaam,&Wedstyp,&Naamkort,&Sexlist,&Geld,&Verl);
	    RUN_Cut(Top1);
  	    (SpPlan + ISpeler)->EDG = Wedstyp->fno;
  	    ISpeler++;
	    for(PlanInd = 0 ; PlanInd < PlanAant ; PlanInd++) {
	      if ((PlanP+PlanInd)->Plan_Tijd > Lower &&
		       (PlanP+PlanInd)->Plan_Tijd < Upper) {
		sp_for_c_tel(TPers->SpNum,&Sex,&Naam,&Telefoon,&Telefoon2,&Telefoon3,&Verhinderd,&Offset,&Betaald,&UitKant,&Leeftijd);
		/*wc_for_c_1(Wedscat,&Wedsnaam,&Wedstyp,&Naamkort,&Sexlist,&Geld,&Verl);*/
		if (Write_Mode) {
		  tijd_kwart_1(&Dag1, &Uur1, &Minuten1, Plan);
		  helestr_int_0(0,&Minuten1S,Minuten1);
		  Area += IO_SPrintf(Area,"\n%-20.20s => overlapt %s om %2d.%s.",
					    Naam, Naamkort, Uur1, Minuten1S);
		}
		switch (Wedstyp->fno) {
		case (wedstyp_e):
		  (PlanP+PlanInd)->Plan_Bezwaren |= reden_speelt_enk;
		  break;
		case (wedstyp_d):
		  (PlanP+PlanInd)->Plan_Bezwaren |= reden_speelt_dub;
		  break;
		case (wedstyp_g):
		  (PlanP+PlanInd)->Plan_Bezwaren |= reden_speelt_gd;
		}
	      }
	    }
	  }
	};
      };
      RUN_Cut(Top);
    };
    for(PlanInd = 0 ; PlanInd < PlanAant ; PlanInd++) {
      if (Plan == (PlanP+PlanInd)->Plan_Tijd) {
    /*	if ((PlanP+PlanInd)->Plan_Banen) {  */
	  --(PlanP+PlanInd)->Plan_Banen;
    /*	};   				      */
      break;
      };
    };
    RUN_Fail();		/* ga alle planning langs */
  };
  ISpMax = ISpeler;
  if (Baan_Count) {
    for(PlanInd = 0 ; PlanInd < PlanAant ; PlanInd++) {
      if ((PlanP+PlanInd)->Plan_Banen <= 0) {
	if (Write_Mode)
	  Area += IO_SPrintf(Area,"\nGeen baan beschikbaar.");
	(PlanP+PlanInd)->Plan_Bezwaren |= reden_geen_baan;
      }
    }
  }
  for (TPers = Spelers; TPers->fno==LISTFNO; TPers=TPers->next) {
    AantalWeds = 0;
    AantalWedO = 1;
    AantalSingles = 0;
    if (EDG_eigen == wedstyp_e) AantalSingles = 1;
    for(ISpeler = 0 ; ISpeler < ISpMax ; ISpeler++)
      if ((SpPlan + ISpeler)->SpNo == TPers->SpNum) {
	if ((SpPlan + ISpeler)->DagNo == DagNo) AantalWeds++;
	if ((SpPlan + ISpeler)->Cat == Cat) AantalWedO++;
	if ((SpPlan + ISpeler)->EDG == wedstyp_e && (SpPlan + ISpeler)->DagNo == DagNo) AantalSingles++;
	for (ISp1 = ISpeler + 1; ISp1 < ISpMax; ISp1++) {
	  /*if ((SpPlan + ISp1)->SpNo    == TPers->SpNum &&
	      (SpPlan + ISp1)->DagNo   == DagNo &&
	      (SpPlan + ISp1)->Cat     == (SpPlan + ISpeler)->Cat) {
	    AantalWeds++; 	 wedsrijd (zelfde soort) telt dubbel 
	    AantalWedO++;
	    }*/
	  }
	};
    if (AantalWeds > DagMax) {
      sp_for_c_tel(TPers->SpNum,&Sex,&Naam,&Telefoon,&Telefoon2,&Telefoon3,&Verhinderd,&Offset,
							&Betaald,&UitKant,&Leeftijd);
      if (Write_Mode) Area += IO_SPrintf(Area,"\n%-20.20s => meer dan %u weds. op één dag.", Naam, ++DagMax);
      DagMax--;
      for(PlanInd = 0 ; PlanInd < PlanAant ; PlanInd++) {
	(PlanP+PlanInd)->Plan_Bezwaren |= reden_teveel_weds;
	}
      }
    if (AantalWedO > DagMaxO) {
      sp_for_c_tel(TPers->SpNum,&Sex,&Naam,&Telefoon,&Telefoon2,&Telefoon3,&Verhinderd,&Offset,
							&Betaald,&UitKant,&Leeftijd);
      wc_for_c_1(Cat,&Wedsnaam,&Wedstyp,&Naamkort,&Sexlist,&Geld,&Verl);
      if (Write_Mode) {
	Area += IO_SPrintf(Area,"\n%-20.20s => meer dan %u %s %s.",  Naam, DagMaxO, Naamkort, PerWatStr);
	}
      for(PlanInd = 0 ; PlanInd < PlanAant ; PlanInd++) {
	(PlanP+PlanInd)->Plan_Bezwaren |= reden_dag_max_per;
	}
      }
    if (AantalSingles > Enkel) {
      sp_for_c_tel(TPers->SpNum,&Sex,&Naam,&Telefoon,&Telefoon2,&Telefoon3,&Verhinderd,&Offset,
							&Betaald,&UitKant,&Leeftijd);
      if (Write_Mode) {
	Area += IO_SPrintf(Area,"\n%-20.20s => meer dan %u enkelspelen op één dag.", Naam, Enkel);
	}
      for(PlanInd = 0 ; PlanInd < PlanAant ; PlanInd++) {
	(PlanP+PlanInd)->Plan_Bezwaren |= reden_singles;
	}
      }
    }


  *TijdenStr = MEM_AllocGStack(100);
  for(PlanInd = 0 ; PlanInd < PlanAant ; PlanInd++) {
    if(RUN_StackBTrack()) continue;
    tijd_kwart_1(&Dag1, &Uur1, &Minuten1, (PlanP+PlanInd)->Plan_Tijd);
    helestr_int_0(0,&Minuten1S,Minuten1);
    /*IO_SPrintf(*TijdenStr, " %2d.%s   %3d          ", Uur1, Minuten1S,
				       (PlanP+PlanInd)->Plan_Banen);*/
    IO_SPrintf(*TijdenStr, " %2d.%s  %3d           ", Uur1, Minuten1S,
				       (PlanP+PlanInd)->Plan_Banen);
    if (!(PlanP+PlanInd)->Plan_Banen) (*TijdenStr)[10] = '-';
    DagNo = 13;		/* gebruik als index */
    if ((PlanP+PlanInd)->Plan_Bezwaren & reden_verhinderd)
				        (*TijdenStr)[DagNo++] = 'v';
    if ((PlanP+PlanInd)->Plan_Bezwaren & reden_beschikbaar)
					(*TijdenStr)[DagNo++] = 'b';
    if ((PlanP+PlanInd)->Plan_Bezwaren & reden_speelt_enk)
					(*TijdenStr)[DagNo++] = 'e';
    if ((PlanP+PlanInd)->Plan_Bezwaren & reden_speelt_dub)
					(*TijdenStr)[DagNo++] = 'd';
    if ((PlanP+PlanInd)->Plan_Bezwaren & reden_speelt_gd)
					(*TijdenStr)[DagNo++] = 'g';
    if ((PlanP+PlanInd)->Plan_Bezwaren & reden_teveel_weds)
					(*TijdenStr)[DagNo++] = 't';
    if ((PlanP+PlanInd)->Plan_Bezwaren & reden_dag_max_per)
					(*TijdenStr)[DagNo++] = 'm';
    if ((PlanP+PlanInd)->Plan_Bezwaren & reden_cat_blok)
					(*TijdenStr)[DagNo++] = 'o';
    if ((PlanP+PlanInd)->Plan_Bezwaren & reden_koppel)
					(*TijdenStr)[DagNo++] = '<';
    if ((PlanP+PlanInd)->Plan_Bezwaren & reden_vorige_laat)
					(*TijdenStr)[DagNo++] = '<';
    if ((PlanP+PlanInd)->Plan_Bezwaren & reden_vorige_niet)
					(*TijdenStr)[DagNo++] = '<';
    if ((PlanP+PlanInd)->Plan_Bezwaren & reden_singles)
					(*TijdenStr)[DagNo++] = 's';
    if (!(PlanP+PlanInd)->Plan_Bezwaren &&
	   (PlanP+PlanInd)->Plan_Banen != 0) {        /* schrijf OK */
		(*TijdenStr)[DagNo++] = 'O';
		(*TijdenStr)[DagNo]   = 'k';
    };
    *ResultP = (PlanP+PlanInd)->Plan_Bezwaren;
    *ResTijdP = (PlanP+PlanInd)->Plan_Tijd;
    RUN_JmpReturn();
  }
  RUN_Fail();

}
/*******************************************************/
/*       het eigenlijke programma                      */
/*******************************************************/

/* als truc om behandelen van doubles (reals in PDC) te vermijden,
   wordt iedere real door een array van 4 integers vervangen       */


static void _stdcall opgesteld(WEDSCAT Wedscat, TGNST *Tgnst)
{
  INTEGER  WedsNo;
  TGNST    *Tgnst1;
  STRING   Uitslag;
  TIJD     Tijd;

  unsigned Top = GETSP;
  if (!RUN_StackBTrack()) {
    wd_for_c_1(&WedsNo,Wedscat,Tgnst,&Tgnst1,&Uitslag,&Tijd);
    RUN_Cut(Top);
    return;
  } ;
  if (!RUN_StackBTrack()) {
    wd_for_c_2(&WedsNo,Wedscat,&Tgnst1,Tgnst,&Uitslag,&Tijd);
    RUN_Cut(Top);
    return;
  } ;
  RUN_Fail();
}

void oproepen_for_c(JANEE **Waarschuwen);

void time_stamp1(TIJD *Stamp, INTEGER *integer, TIJD *hoelaat);

void _stdcall ws_for_c(INTEGER *integer,WEDSCAT wedscat,TGNST **tgnst,
								TIJD *tijd);

void _stdcall w3_for_c_2(INTEGER *integer,WEDSCAT *wedscat,TGNST **tgnst,
								TIJD *tijd);


void financieel1(STRING *Outstring)
{
  INTEGER  i;
  unsigned top;
/*  STRING   Text;*/
  STRING   Area;
  SEXE     *Dummy;
  SEX      SpelerCategorie;
  STRING   SpCatNaam;
  INTEGER  LftL;
  INTEGER  LftU;
  STRING   StL;
  STRING   StU;
  VERHL    *Vh;
  PERSNO   Persno;
  SEX      Sex;
  SYMBOL   Naam;
  STRING   Telefoon;
  STRING   Telefoon2;
  STRING   Telefoon3;
  VERHL    *Verhl;
  long     Offset;
  JANEE    *Betaald;
  TIJD     UitKantoor;
  INTEGER  Leeftijd;
  WEDSCAT  Wedscat;
  INTEGER  OpgaveNo;
  PERSNO   Partner;
  INTEGER  Plaatsing;
  TGNST    *Tgnst1;
  STRING   Kop;
  WEDSTYP  *Wedstyp;
  STRING   String_0;
  SEXLIST  *Sexlist;
  WEDSCAT  Verliezers_ronde;
  TGNST    *Tgnst;
  INTEGER  Integer_0;
  INTEGER  Integer_1;
  INTEGER  Uitsluit;
  TIJD     Tijd;
  JANEE    *BetaalAdm;
  INTEGER  Voldaan;
  INTEGER  Aantal, Totaal, DoenMee, DoenMeeTot;
  double   Contributie, ContrTotaal, ContrTot, VoldaanFl, VoldaanFlTemp, VoldaanFlTot;

  /*makestatus_0(112,"\tbezig . . (deelname)");*/
  Area = MEM_AllocGStack(30000);
  *Area = 0;
  *Outstring = Area;
/*  prt_header(&Text, 0); */
  Area += IO_SPrintf(Area, " NB Gebruik aan het eind van het toernooi niet dit rapport\n maar het resultaatexport rapport!\n\n");
  Area += IO_SPrintf(Area,
		   " Financiëel rapport"
	       "\n\n Categorie           Aantal     Deelname"
		 "\n ---------------------------------------");
  Totaal = 0; DoenMeeTot = 0;
  if (!RUN_StackBTrack()) {             /* voor alle spelers categorie‰n  */
    spsrt_for_c(&SpelerCategorie,&SpCatNaam,&Dummy,&LftL,&LftU,&StL,&StU,&Vh);
    Aantal = 0; DoenMee = 0;
    if (!RUN_StackBTrack()) {                /* voor alle spelers opgaven */
      sp_for_c(&Persno,&Sex,&Naam,&Telefoon,&Verhl,&Offset,&Betaald,
								&UitKantoor);
      if (Sex  == SpelerCategorie) {    /* doet speler mee aan categorie? */
	Aantal++;
        top = GETSP;
	sp2opg_1(Persno,&Wedscat,&OpgaveNo,&Partner,&Plaatsing,&Tgnst1);
						       /* fail-t als niet */
	DoenMee++;                        /* ja -> increment count        */
	RUN_Cut(top);                     /* en cut                       */
      };
      RUN_Fail();                         /* loop voor volgende speler    */
    };
    Totaal     += Aantal;
    DoenMeeTot += DoenMee;                /* rapporteer deze categorie:   */
    Area += IO_SPrintf(Area, "\n %-20.20s %4d        %4d",
						SpCatNaam,Aantal,DoenMee);
    RUN_Fail();                           /* loop voor volgende categorie */
  };
  Area += IO_SPrintf(Area,                /* rapporteer totalen           */
		"\n                      ----        ----"
		"\n Totaal               %4d        %4d\n",
					Totaal, DoenMeeTot);

  /* --------------------------------------------------------------------- */
  /* deel 2, de contributie                                                */
  /* --------------------------------------------------------------------- */

  /*changestatus_0("\tbezig . . (contributie)");*/
  betaal_adm_for_c_0(&BetaalAdm);
  Area += IO_SPrintf(Area, "\n Onderdeel            Ingeschreven - "
					"Niet opgesteld    Contributie");
  if (BetaalAdm->fno==janee_ja) Area += IO_SPrintf(Area, "  Voldaan");
  Area += IO_SPrintf(Area, "\n-------------------------------------"
					"------------------------------");
  if (BetaalAdm->fno==janee_ja) Area += IO_SPrintf(Area, "---------");
  Totaal = 0;
  ContrTotaal = 0;
  VoldaanFlTot = 0;
  if (!RUN_StackBTrack()) {               /* voor alle wedstr categorie‰n */
    wc_for_c_0(&Wedscat,&Kop,&Wedstyp,&String_0,&Sexlist,
					    &Contributie,&Verliezers_ronde);
    Aantal = 0; DoenMee = 0; Voldaan = 0;
    if (!RUN_StackBTrack()) {
      opg_for_c_1(&OpgaveNo,Wedscat,&Tgnst,&Integer_0,
					      &Integer_1,&Uitsluit,&Tijd);
      Aantal++;
      opgesteld(Wedscat, Tgnst);	/* failt als niet opgesteld */
      DoenMee++;
      if (BetaalAdm->fno==janee_ja) {
	if (Tgnst->fno==tgnst_s) {
	  sp_for_c_tel(Tgnst->u.s,&Sex,&Naam,&Telefoon,&Telefoon2,&Telefoon3,
                                         &Verhl,&Offset,&Betaald,&UitKantoor,&Leeftijd);
	  if (Betaald->fno==janee_ja) Voldaan++;
	} else
	if (Tgnst->fno==tgnst_pw) {
	  sp_for_c_tel(Tgnst->u.pw,&Sex,&Naam,&Telefoon,&Telefoon2,&Telefoon3,
                                         &Verhl,&Offset,&Betaald,&UitKantoor,&Leeftijd);
	  if (Betaald->fno==janee_ja) Voldaan++;
	} else
	if (Tgnst->fno==tgnst_p) {
	  sp_for_c_tel(Tgnst->u.p.persno,&Sex,&Naam,&Telefoon,&Telefoon2,&Telefoon3,
					 &Verhl,&Offset,&Betaald,&UitKantoor,&Leeftijd);
	  if (Betaald->fno==janee_ja) Voldaan++;
	  sp_for_c_tel(Tgnst->u.p.persno_0,&Sex,&Naam,&Telefoon,&Telefoon2,&Telefoon3,
                                         &Verhl,&Offset,&Betaald,&UitKantoor,&Leeftijd);
	  if (Betaald->fno==janee_ja) Voldaan++;
	} ;
      } ;
      RUN_Fail();
    };
    ContrTot = Contributie * DoenMee * ((Wedstyp->fno == wedstyp_e) ? 1 : 2);
    /*mult_for_c_0(Contributie, (DoenMee * ((Wedstyp->fno == wedstyp_e) ? 1 : 2)),
								   &ContrTot);*/
    VoldaanFl = Contributie * Voldaan;
    /*mult_for_c_0(ContrTot, Voldaan, &VoldaanFl);*/
    Area += IO_SPrintf(Area,
		 "\n %-30.30s%3d - %2d      à %5.2f %s = %7.2f",
	 Kop,Aantal,Aantal - DoenMee, Contributie,
	 ((Wedstyp->fno==wedstyp_e) ? "   " : "x 2"), ContrTot);
    if (BetaalAdm->fno==janee_ja)
			Area += IO_SPrintf(Area, "  %7.2f",VoldaanFl);
    Totaal += Aantal;
    Contributie = ContrTotaal + ContrTot;
    VoldaanFlTemp = VoldaanFlTot + VoldaanFl;
    /*add_for_c_0(  ContrTotaal,  ContrTot,  &Contributie);
      add_for_c_0(  VoldaanFlTot, VoldaanFl, &VoldaanFlTemp);*/
    ContrTotaal = Contributie;
    VoldaanFlTot = VoldaanFlTemp;
    RUN_Fail();
  };
  Area += IO_SPrintf(Area,
     "\n                           -------                         -------");
  if (BetaalAdm->fno==janee_ja) Area += IO_SPrintf(Area, "  -------");
  Area += IO_SPrintf(Area,
     "\n Totaal                        %3d                        %8.2f",
					 Totaal,     ContrTotaal);
  if (BetaalAdm->fno==janee_ja)
			Area += IO_SPrintf(Area, " %8.2f" ,VoldaanFlTot);
  /*Area += */IO_SPrintf(Area,"\n\f");
  /*removestatus_0();*/

}

/*******************************************************/
/*       Voortgangs-rapport                            */
/*******************************************************/

void voortgang_c(STRING *Outstring)
{
  STRING   Area;
/*  STRING   Text;  */
  TGNST    *Tgnst_1;
  TGNST    *Tgnst_2;
  TGNST    *TgnstX;
  WEDSCAT  Wedscat;
  STRING   Kop;
  WEDSTYP  *Wedstyp;
  STRING   String_0;
  SEXLIST  *Sexlist;
  WEDSCAT  Verliezers_ronde;
  INTEGER  WedsNo;
  STRING   Uitslag;
  TIJD     LogTijd;
  TIJD     Vrij;
  TIJD     Plan;
  TIJD     BanenTijd;
  TIJD     HTijd;
  JANEE    *Gew_1;
  JANEE    *Gew_2;
  CHAR     Fixed;
  INTEGER  DagNo;
  STRING   Datum;
  STRING   Token;
  STRING   Rest;
  WRK      *DagType;
  INTEGER  BanenTal;
  PERSNO   Persno;
  SEX      Sex;
  SYMBOL   Naam;
  STRING   Telefoon;
  VERHL    *Verhl;
  long     Offset;
  JANEE    *Betaald;
  TIJD     UitKantoor;
  JANEE    *Oproepen;
  unsigned Top;
  INTEGER  Aantal;
  INTEGER  AantalGepl;
  INTEGER  AantalTeWaars;
  INTEGER  AantalAfz;
  INTEGER  AantalGesp;
  INTEGER  Gepl_t = 0, Aant_t = 0, Gesp_t = 0, TeWaars_t = 0, Afz_t = 0;
  double  Contributie;
  struct {
    INTEGER DagNum;
  /*  unsigned char DagType;*/
    INTEGER Voor5Gepland;
    INTEGER Na5Gepland;
    INTEGER Voor5Capac;
    INTEGER Na5Capac;
    INTEGER AlGespeeld;
    } *DagPlan;
  INTEGER  Index, AantalDagen = 0; /*, AantalCats = 0;*/
  INTEGER Dag1;
  INTEGER Uur1;
  INTEGER Minuten1;
  INTEGER Card;
  INTEGER Pos;
  RSOORT  *Afval_of_Poel;
  STRING  Minuten1S;
  CHAR    A_P;
  CHAR    Oproep = 0;
  CHAR    WChar, AChar;

  /*makestatus_0(112,"\tbezig . . ");*/
  if (!RUN_StackBTrack()) {             /* voor alle dagen                  */
    dag_for_c_0(&DagNo,&Datum,&DagType);
    AantalDagen++;
    RUN_Fail();
  }                                    /* einde voor alle dagen            */
  oproepen_for_c(&Oproepen);
  if (Oproepen->fno == janee_ja) Oproep = 1;
  Area = MEM_AllocGStack(40000);
  *Area = 0;
  *Outstring = Area;
/*_________________________________________________________________________*/
  time_stamp1(&HTijd, &Dag1, &UitKantoor);/* laatste twee zijn dummy */
  if (!HTijd) {
    TIJD CTijd = 23767;
    if (!RUN_StackBTrack()) {            /* voor alle geplande wedstr.     */
      w2_for_c_1(&WedsNo,&Wedscat,&Plan,&Gew_1,&Gew_2,&Fixed,&Vrij);
      if (CTijd > Plan) CTijd = Plan;    /* zoek de vroegst geplande       */
      RUN_Fail();
      }                                  /* einde voor alle geplande wedst.*/
    if (CTijd != 23767) HTijd = CTijd;
  }
  HTijd >>= 7;	                         /* maak er dagnummer van          */
  if (!RUN_StackBTrack()) {              /* zoek eerstvolgende dag         */
    Top = GETSP;
    dag_for_c_0(&DagNo,&Datum,&DagType);
    if (DagNo > HTijd) {
      HTijd = DagNo;
      RUN_Cut(Top);
      }
    RUN_Fail();
    }
  HTijd++;
  HTijd <<= 7;		/* verander weer in tijd (einde van die dag + 1)   */
/*_________________________________________________________________________*/
  Area += IO_SPrintf(Area,
	   "Voortgangsrapport"
         "\n                            Totaal     Al       Al    Nog te  Af te   Rest"
	 "\n   Onderdeel               Partijen Gespeeld Gepland  Waarsch Zeggen Partijen"
	 "\n   --------------------------------------------------------------------------"
				 );
  if (!RUN_StackBTrack()) {               /* voor alle categorie‰n         */
    WChar = AChar = ' ';
    wc_for_c_0(&Wedscat,&Kop,&Wedstyp,&String_0,&Sexlist,
					    &Contributie,&Verliezers_ronde);
    Aantal = 0; AantalGepl = 0; AantalGesp = 0;
    AantalTeWaars = 0; AantalAfz = 0;
    if (!RUN_StackBTrack()) {             /* voor alle echte wedstrijden   */
      wd_for_c_0(&WedsNo,Wedscat,&Tgnst_1,&Tgnst_2,&Uitslag,&Vrij);
      if (Tgnst_1->fno != tgnst_bye && Tgnst_2->fno != tgnst_bye) Aantal++;
      RUN_Fail();
    };                                    /* einde voor alle echte wedstr.  */
    if (!RUN_StackBTrack()) {             /* voor alle geplande wedstr.     */
      w2_for_c_0(&WedsNo,Wedscat,&Plan,&Gew_1,&Gew_2,&Fixed,&Vrij);
      AantalGepl++;
      if (Gew_1->fno == janee_nee || Gew_2->fno == janee_nee) {
	wd_for_c_3(WedsNo,Wedscat,&Tgnst_1,&Tgnst_2,&Uitslag,&Vrij);
        if (Gew_1->fno == janee_nee && Tgnst_1->fno != tgnst_onbekend) {
	  AantalTeWaars++;
	  if (Plan < HTijd) WChar = '!';
	  }
        else if (Gew_2->fno == janee_nee && Tgnst_2->fno != tgnst_onbekend) {
	  AantalTeWaars++;
	  if (Plan < HTijd) WChar = '!';
	  }
	}
      RUN_Fail();
    };                                    /* einde voor alle geplande wedst.*/
    if (!RUN_StackBTrack()) {             /* voor alle af te zeggen wedstr. */
      ws_for_c(&WedsNo,Wedscat,&TgnstX,&Vrij);
      /*Top = GETSP;                      Toch maar weg, die persoon is zowel af te zeggen als te waarschuwen...
      if (!RUN_StackBTrack()) {            
        w2_for_c_2(WedsNo,Wedscat,&Plan,&Gew_1,&Gew_2,&Fixed,&Vrij);
        RUN_Cut(Top);
        RUN_Fail();
        }*/
      wd_for_c_3(WedsNo,Wedscat,&Tgnst_1,&Tgnst_2,&Uitslag,&Vrij);
      if (TgnstX->fno != Tgnst_1->fno && TgnstX->fno != Tgnst_2->fno) RUN_Fail(); /* moet nog uitgebreid worden */
      if (TgnstX->fno == Tgnst_1->fno && TgnstX->fno == tgnst_s && (TgnstX->u.s != Tgnst_1->u.s) &&
         TgnstX->fno == Tgnst_2->fno  && (TgnstX->u.s != Tgnst_2->u.s)  )  RUN_Fail();
      if (TgnstX->fno == Tgnst_1->fno && TgnstX->fno == tgnst_pw && (TgnstX->u.pw != Tgnst_1->u.pw) &&
         TgnstX->fno == Tgnst_2->fno  && (TgnstX->u.pw != Tgnst_2->u.pw)  )  RUN_Fail();
      AantalAfz++;
      if (Vrij < HTijd) AChar = '!';
      RUN_Fail();
    };                                    /* einde voor alle af te zeggen w.*/
    if (!RUN_StackBTrack()) {             /* voor alle gespeelde wedstr.    */
      w3_for_c_0(&WedsNo,Wedscat,&Tgnst_1,&Vrij);
      AantalGesp++;
      RUN_Fail();
    };                                    /* einde voor alle gespeelde wedst*/
    if (Aantal > 0) {
      Top = GETSP;
      pos_for_c(Wedscat, &Card, &Pos, &Afval_of_Poel);
      RUN_Cut(Top);
      A_P = (Afval_of_Poel->fno == rsoort_poel ? 'p' : ' ');
      Area += IO_SPrintf(Area, "\n %c %-27.27s%3d %7d",
				A_P, Kop, Aantal, AantalGesp);
      if (Aantal - AantalGesp)
	Area += IO_SPrintf(Area, " %7d", AantalGepl);
      else
	Area += IO_SPrintf(Area, " %7s", "-");
      if (AantalGepl && Oproep)
	Area += IO_SPrintf(Area, " %7d%c", AantalTeWaars, WChar);
      else
	Area += IO_SPrintf(Area, " %7s ", "-");
      if (AantalAfz && Oproep)
	Area += IO_SPrintf(Area, "%7d%c", AantalAfz, AChar);
      else
	Area += IO_SPrintf(Area, "%7s ", "-");
      Area += IO_SPrintf(Area, "%7d", 	Aantal - AantalGesp - AantalGepl);
    };
    Aant_t += Aantal; Gepl_t += AantalGepl ; Gesp_t += AantalGesp;
    Afz_t += AantalAfz; TeWaars_t += AantalTeWaars;
    /*AantalCats++;*/
    RUN_Fail();
  };                                      /* einde voor alle categorie‰n    */
  Area += IO_SPrintf(Area,
"\n                          -----------------------------------------------+"
"\n   %-23s%7d %7d %7d", "Totaal", Aant_t, Gesp_t, Gepl_t);
  if (Oproep)
    Area += IO_SPrintf(Area,  " %7d %7d", TeWaars_t, Afz_t);
  else
    Area += IO_SPrintf(Area,  " %7s %7s", "-", "-");
  Area += IO_SPrintf(Area,  " %7d",   Aant_t - Gesp_t - Gepl_t);
  /*------------------------------------------------------------------------*/
  Area += IO_SPrintf(Area, "\n\n ============");
  for(Index = 0; Index < AantalDagen; Index++) {
    Area += IO_SPrintf(Area, "====");
    };                    
  Area += IO_SPrintf(Area, "\n             ");
  if (!RUN_StackBTrack()) {             /* voor alle dagen                  */
    dag_for_c_0(&DagNo,&Datum,&DagType);
    /*fronttoken(Datum, &Token, &Rest);*/
    for(Index = 0; Datum[Index] != ' ' && Datum[Index] != 0; Index++) ;
    Datum[Index] = 0;
    /*Area += IO_SPrintf(Area, "%4s" , Token);*/
    Area += IO_SPrintf(Area, "%4s" , Datum);
    RUN_Fail();
  };                                    /* einde voor alle dagen            */
  /*Index = 0;*/
  /* bepaal gemiddelde uit-kantoor-tijd */
  Vrij   = 0;
  Aantal = 0;
  if (!RUN_StackBTrack()) {             /* voor alle spelers opgaven */
    Top = GETSP;
    sp_for_c(&Persno,&Sex,&Naam,&Telefoon,&Verhl,&Offset,&Betaald,
								&UitKantoor);
    if (UitKantoor > 64) {
      Vrij += UitKantoor;
      Aantal++;
      if (Aantal == 64) RUN_Cut(Top);   /* en cut                         */
    };
    RUN_Fail();                         /* loop voor volgende speler      */
  };
  if (Aantal == 64) Vrij >>= 6; else Vrij = 68; /* gemiddelde uit-kantoor-tijd is bekend */

  DagPlan = MEM_AllocGStack(4 * AantalDagen * sizeof(DagPlan[0]));

  Index = -1;
  if (!RUN_StackBTrack()) {             /* voor alle dagen                */
  dag_for_c_0(&DagNo,&Datum,&DagType);  /* initialiseer array             */
    Index++;
    (DagPlan + Index)->DagNum       = DagNo;
  /*  (DagPlan + Index)->DagType      = DagType->fno; */
    (DagPlan + Index)->Voor5Gepland = 0;
    (DagPlan + Index)->Na5Gepland   = 0;
    (DagPlan + Index)->Voor5Capac   = 0;
    (DagPlan + Index)->Na5Capac     = 0;
    (DagPlan + Index)->AlGespeeld   = 0;
  RUN_Fail();
  };                                    /* einde voor alle dagen         */
  if (!RUN_StackBTrack()) {             /* voor alle dagen               */
    w2_for_c_1(&WedsNo,&Wedscat,&Plan,&Gew_1,&Gew_2,&Fixed,&UitKantoor);
    for(Index = 0 ; Index < AantalDagen ; Index++) {
      if ((Plan >> 7) == (DagPlan + Index)->DagNum) {
	if ((Plan & 0x7F) >= Vrij){
	 ((DagPlan + Index)->Na5Gepland)++;
	  }
	else {
	  ((DagPlan + Index)->Voor5Gepland)++;
	  }
	break;
      };
    };
    RUN_Fail();
  };                                    /* einde voor alle dagen          */
  if (!RUN_StackBTrack()) {             /* tel de baan-beschikbaarheid   */
    bn_for_c(&BanenTijd,&BanenTal);
    for(Index = 0 ; Index < AantalDagen ; Index++) {
      if ((BanenTijd >> 7) == (DagPlan  + Index)->DagNum) {
	if((BanenTijd & 0x7F) < Vrij)
	  ((DagPlan  + Index)->Voor5Capac)    += BanenTal;
	  else ((DagPlan  + Index)->Na5Capac) += BanenTal;
	break;
      }
    }
    RUN_Fail();
  };                                    /* einde baan-beschikb.          */
  if (!RUN_StackBTrack()) {             /* voor alle gespeelde wedstr.    */
    w3_for_c_2(&WedsNo,&Wedscat,&Tgnst_1,&LogTijd);
    /*if (LogTijd != 0) Area += IO_SPrintf(Area, "\n %d %D %D", WedsNo, LogTijd, (LogTijd >> 7));*/
    for(Index = 0 ; Index < AantalDagen ; Index++) {
      if((LogTijd >> 7) == (DagPlan + Index)->DagNum) {
	(DagPlan + Index)->AlGespeeld++;
      }
    }
    RUN_Fail();
  };                                  /* einde voor alle gespeelde wedst*/
  tijd_kwart_1(&Dag1, &Uur1, &Minuten1, Vrij);
  helestr_int_0(0,&Minuten1S,Minuten1);
  /*----------------------------------------------------------------------*/
  Area += IO_SPrintf(Area, "\n VOOR %2du%s -----", Uur1, Minuten1S);
  for(Index = 1 ; Index < AantalDagen ; Index++) {
    Area += IO_SPrintf(Area, "----");
  };
  Area += IO_SPrintf(Area, "\n  Gepland   :");
  for(Index = 0 ; Index < AantalDagen ; Index++) {
    Area += IO_SPrintf(Area, "%4d" , (DagPlan + Index)->Voor5Gepland);
  };
  Area += IO_SPrintf(Area, "\n  Capaciteit:");
  for(Index = 0 ; Index < AantalDagen ; Index++) {
    Area += IO_SPrintf(Area, "%4d" , (DagPlan + Index)->Voor5Capac);
  };
  /*---------------------------------------------------------------------*/
  Area += IO_SPrintf(Area, "\n NA %2du%s -------", Uur1, Minuten1S);
  for(Index = 1 ; Index < AantalDagen ; Index++) {
    Area += IO_SPrintf(Area, "----");
  };
  Area += IO_SPrintf(Area, "\n  Gepland   :");
  for(Index = 0 ; Index < AantalDagen ; Index++) {
    Area += IO_SPrintf(Area, "%4d" , (DagPlan + Index)->Na5Gepland);
  };
  Area += IO_SPrintf(Area, "\n  Capaciteit:");
  for(Index = 0 ; Index < AantalDagen ; Index++) {
    Area += IO_SPrintf(Area, "%4d" , (DagPlan + Index)->Na5Capac);
  };
  /*--------------------------------------------------------------------*/
  Area += IO_SPrintf(Area, "\n TOTAAL ---------");
  for(Index = 1 ; Index < AantalDagen ; Index++) {
    Area += IO_SPrintf(Area, "----");
  };
  Area += IO_SPrintf(Area, "\n  Te spelen :");
  Aant_t -= Gesp_t;
  for(Index = 0 ; Index < AantalDagen ; Index++) {
    Area += IO_SPrintf(Area, "%4d" , Aant_t);
    Aant_t -= (DagPlan + Index)->Na5Gepland + (DagPlan + Index)->Voor5Gepland;
  };
  Area += IO_SPrintf(Area, "\n  Gespeeld  :");
  for(Index = 0 ; Index < AantalDagen ; Index++) {
    Area += IO_SPrintf(Area, "%4d",  (DagPlan + Index)->AlGespeeld);
  };
  Area += IO_SPrintf(Area, "\n  Gepland   :");
  for(Index = 0 ; Index < AantalDagen ; Index++) {
    Area += IO_SPrintf(Area, "%4d" ,
	(DagPlan + Index)->Na5Gepland + (DagPlan + Index)->Voor5Gepland);
  };
  Area += IO_SPrintf(Area, "\n  Capaciteit:");
  for(Index = 0 ; Index < AantalDagen ; Index++) {
    Area += IO_SPrintf(Area, "%4d" ,
	(DagPlan + Index)->Na5Capac + (DagPlan + Index)->Voor5Capac);
  };
  Aantal = 0;
  Area += IO_SPrintf(Area, "\n..cumulatief:");
  for(Index = 0 ; Index < AantalDagen ; Index++) {
    Aantal += (DagPlan + Index)->Na5Capac + (DagPlan + Index)->Voor5Capac;
    Area += IO_SPrintf(Area, "%4d" , Aantal);
	;
  };
  /*---------------------------------------------------------------------*/
  Area += IO_SPrintf(Area, "\n ================");
  for(Index = 1 ; Index < AantalDagen ; Index++) {
    Area += IO_SPrintf(Area, "====");
  };
  IO_SPrintf(Area, "\n\f");
  /*removestatus_0();*/
}
/* =====================================================================*/


/* =====================================================================*/
/*                     splits de opgaven in groepen                     */
/* =====================================================================*/

typedef int tijd;

typedef struct tlist {
  unsigned char functor;
  tijd baantijd;
  struct tlist *next;
} tijdlist;



typedef struct {
    int OpgaveNo;
    int Plaatsing;
    int Clubdeelname;
    int CatDelta;     /* werkveld */
    char *Club;
    tijdlist *Tijden;
} record;

typedef struct jlist {	  /* lijst met indelings-gegevens */
  unsigned char functor;
  record *recp;
  struct jlist *next;
} rlist;

int static beschikbaarheid(record *r1[], record *r5, int Aantal)
{
int done=0, n, imax, i=0, nlow=999, nhigh=0, ndoorsnee=0;
tijdlist *ptemp, **p;
if (!Aantal) return(0);
p = MEM_AllocGStack(Aantal * sizeof(tijdlist *));
for (n=0; n<Aantal; n++)  p[i++] = r1[n]->Tijden;
p[i++] = r5->Tijden;
imax = i;
for (i=0; i<imax; i++) {
  for (n=0, ptemp=p[i];ptemp->functor==listfno;ptemp=ptemp->next, n++);
  nlow  = min(n,nlow);
  nhigh = max(n,nhigh);
  }
if (nhigh==0) return(0);   /* nog niemand in part               */
while (!done) {
  for (i=1; i<imax; i++)   /* verzet alles naar zelfde tijdstip */
    while (p[i]->functor==listfno && p[i]->baantijd<p[0]->baantijd)
      p[i]=p[i]->next;
  for (i=0; i<imax; i++)
    if (p[i]->functor!=listfno || p[i]->baantijd!=p[0]->baantijd) {
      i = 0;
      break;
      }
  if (i==imax) ndoorsnee++;
  for (i=0; i<imax; i++)
    if (p[i]->functor!=listfno) {
      done = 1;
      break;
      }
  if (!done) p[0]=p[0]->next;
  }
if (ndoorsnee < nlow - 10) {
  return(-3);
}
if (ndoorsnee < nlow) {
  return (-1);
}
return(0);
}

int static mycomp_plaatsing(const void *recp1, const void *recp2)
{
  return ((*(record **)recp1)->Plaatsing - (*(record **)recp2)->Plaatsing);
}

int static mycomp_random(const void *recp1, const void *recp2)
{
  return ((*(record **)recp1)->CatDelta - (*(record **)recp2)->CatDelta);
}

/* random(integer,integer) -- (i,o) */
#define random_1 INTG_Random
extern void random_1(int, int*);

void splitsc(int Ndelen, rlist *inlist, unsigned int OptVerh, unsigned int OptClub, unsigned int OptPlts, 
             unsigned int NotLoot34, unsigned int OptRating, unsigned int IsPoulePoule)
{
signed int length, Nsize, NsizeM, Nrest, Nwork;
signed int *partCount, *partEval, k;
signed i,j;
rlist *savelist = inlist;
record **opgaven, **resultaat, *temp;
unsigned int maxI, aantalGeplaatsten;
signed int maxEval;
#ifdef mydebug2
  IO_Writef("\nOptVerh: %d OptClub: %d OptPlts: %d", OptVerh, OptClub, OptPlts);
#endif
  /* __________________tel de records _______________________________*/
  for(length=0; inlist->functor==listfno; inlist=inlist->next) {
    length++;
    }
  /* ------------------- deel in in groepen -------------------------*/
Nsize = length / Ndelen;
Nrest = length % Ndelen;
NsizeM = Nsize + 1;    /* max lengte van ieder deel */
/*if (Nrest) Nsize++;	*/	/* de lengte van ieder deel */
partCount = MEM_AllocGStack(Ndelen * sizeof(int));
partEval  = MEM_AllocGStack(Ndelen * sizeof(int));
for (i=0;i<Ndelen;i++) {
  partCount[i] = 0;
  partEval[i]  = 0;
  }
opgaven = MEM_AllocGStack((length + Ndelen + Ndelen) * sizeof(record *));
resultaat = MEM_AllocGStack((length + Ndelen + Ndelen) * sizeof(record *));

/* maak een lijst van pointers naar de opgaven */
aantalGeplaatsten = 0;
for(i=0,inlist=savelist; inlist->functor==listfno; inlist=inlist->next) {
  opgaven[i++] = inlist->recp;
  if (!(inlist->recp->Plaatsing)) inlist->recp->Plaatsing = 999; /* achteraan sorteren! */
  else aantalGeplaatsten++;
  }
/*tel_clubdeelname(opgaven, length);
*/
#ifdef mydebug2
IO_Writef("\nNdelen: %d length: %d size: %d rest: %d geplaatst: %d", Ndelen, length, Nsize, Nrest, aantalGeplaatsten);
#endif
if (OptRating) {

  for(k=0;k<length;k++) {
    i = k % Ndelen;
    ++partCount[i];
    }
  k=0;
  for (i=0; i<Ndelen; i++) {
    for (j=0; j<partCount[i]; j++) {
      opgaven[k++]->CatDelta = i;  /* hier wordt het deel aangewezen */
      }
    }
  return;
  }

   qsort(opgaven, length, sizeof(opgaven[0]), mycomp_random);  /* eigenlijk schudden */
/*schudt(opgaven, length);*/
#ifdef mydebug1
 IO_Writef("\nGeschud!");
#endif
k = 0;
if (OptPlts) { 		/* sorteer op plaatsing */
  qsort(opgaven, length, sizeof(opgaven[0]), mycomp_plaatsing);
#ifdef mydebug2
  IO_Writef("\nGesorteerd:");
  for (i = 0; i <aantalGeplaatsten;i++) {
    IO_Writef("\n Plaatsing: %d CatDelta: %d", opgaven[i]->Plaatsing, opgaven[i]->CatDelta);
    }
#endif
  if (IsPoulePoule == 1) {
    qsort(&opgaven[3], 3, sizeof(opgaven[0]), mycomp_random);
    qsort(&opgaven[6], 3, sizeof(opgaven[0]), mycomp_random);   
    for (i=0; i<Ndelen; i++) {
      for (j=0; j<partCount[i]; j++) {
        resultaat[i*NsizeM+j]->CatDelta = i;  /* hier wordt het deel aangewezen */
        }
      }
   return;
  }

  if ( NotLoot34 == 0) {
    random_1(2, &j);
    if (j && length > 3 && Ndelen > 3 ) { temp = opgaven[2]; opgaven[2] = opgaven[3]; opgaven[3] = temp;};/* loot 3 en 4 */
    for(i=4;i+4<length;i+=4) {		/* loot in groepjes van 4 */
      if (i+4 >= length) break;
      if (i >= 16) break;
      random_1(98, &j);
      opgaven[i]->CatDelta = 111;
      if(opgaven[i]->Plaatsing != 999) opgaven[i]->CatDelta = j;
      random_1(98, &j);
      opgaven[i+1]->CatDelta = 111;
      if(opgaven[i+1]->Plaatsing != 999) opgaven[i+1]->CatDelta = j;
      random_1(98, &j);
      opgaven[i+2]->CatDelta = 111;
      if(opgaven[i+2]->Plaatsing != 999) opgaven[i+2]->CatDelta = j;
      random_1(98, &j);
      opgaven[i+3]->CatDelta = 111;
      if(opgaven[i+3]->Plaatsing != 999) opgaven[i+3]->CatDelta = j;
/*      IO_Writef("\ni = :%d",i);
      IO_Writef("\n Plaatsing: %d CatDelta: %d", opgaven[i]->Plaatsing, opgaven[i]->CatDelta);
      IO_Writef("\n Plaatsing: %d CatDelta: %d", opgaven[i+1]->Plaatsing, opgaven[i+1]->CatDelta);
      IO_Writef("\n Plaatsing: %d CatDelta: %d", opgaven[i+2]->Plaatsing, opgaven[i+2]->CatDelta);
      IO_Writef("\n Plaatsing: %d CatDelta: %d", opgaven[i+3]->Plaatsing, opgaven[i+3]->CatDelta);*/
      qsort(&opgaven[i], 4, sizeof(opgaven[0]), mycomp_random);
/*      IO_Writef("\ni = :%u nog steeds",i);
      IO_Writef("\n Plaatsing: %d CatDelta: %d", opgaven[i]->Plaatsing, opgaven[i]->CatDelta);
      IO_Writef("\n Plaatsing: %d CatDelta: %d", opgaven[i+1]->Plaatsing, opgaven[i+1]->CatDelta);
      IO_Writef("\n Plaatsing: %d CatDelta: %d", opgaven[i+2]->Plaatsing, opgaven[i+2]->CatDelta);
      IO_Writef("\n Plaatsing: %d CatDelta: %d", opgaven[i+3]->Plaatsing, opgaven[i+3]->CatDelta);*/
      }
#ifdef mydebug1
  IO_Writef("\nGeplaatst (na 8):");
  for (i = 0; i <length;i++) {
    IO_Writef("\n Plaatsing: %d CatDelta: %d", opgaven[i]->Plaatsing, opgaven[i]->CatDelta);
    }
#endif
    for(;i+8<length;i+=8) {		/* loot in groepjes van 8 */
      if (i+8 >= length) break;
      random_1(98, &j);
      opgaven[i]->CatDelta = 111;
      if(opgaven[i]->Plaatsing != 999) opgaven[i]->CatDelta = j;
      random_1(98, &j);
      opgaven[i+1]->CatDelta = 111;
      if(opgaven[i+1]->Plaatsing != 999) opgaven[i+1]->CatDelta = j;
      random_1(98, &j);
      opgaven[i+2]->CatDelta = 111;
      if(opgaven[i+2]->Plaatsing != 999) opgaven[i+2]->CatDelta = j;
      random_1(98, &j);
      opgaven[i+3]->CatDelta = 111;
      if(opgaven[i+3]->Plaatsing != 999) opgaven[i+3]->CatDelta = j;
      random_1(98, &j);
      opgaven[i+4]->CatDelta = 111;
      if(opgaven[i+4]->Plaatsing != 999) opgaven[i+4]->CatDelta = j;
      random_1(98, &j);
      opgaven[i+5]->CatDelta = 111;
      if(opgaven[i+5]->Plaatsing != 999) opgaven[i+5]->CatDelta = j;
      random_1(98, &j);
      opgaven[i+6]->CatDelta = 111;
      if(opgaven[i+6]->Plaatsing != 999) opgaven[i+6]->CatDelta = j;
      random_1(98, &j);
      opgaven[i+7]->CatDelta = 111;
      if(opgaven[i+7]->Plaatsing != 999) opgaven[i+7]->CatDelta = j;
      qsort(&opgaven[i], 8, sizeof(opgaven[0]), mycomp_random);
      }
    }
  else {   /* NotLoot34 == 1 --> ranglijst */
    if (length > 3 && Ndelen < 3 ) {   /* twee delen */
      random_1(2, &i);
      if (i) { temp = opgaven[2]; opgaven[2] = opgaven[3]; opgaven[3] = temp;};/* loot 3 en 4 */
      }
    if (length > 7 && Ndelen > 3) {    /* vier delen */
      temp = opgaven[4]; opgaven[4] = opgaven[6]; opgaven[6] = temp;  /* verwissel eerst 5 en 6 naar 7 en 8 */
      temp = opgaven[5]; opgaven[5] = opgaven[7]; opgaven[7] = temp;
      random_1(2, &i);   /* vier delen,  loot 3 en 4 niet */
      if (i) { temp = opgaven[4]; opgaven[4] = opgaven[5]; opgaven[5] = temp;};/* loot 5 en 6 (eigenlijk 7 en 8, die komen bij 1 en 2 */
      random_1(2, &i);
      if (i) { temp = opgaven[6]; opgaven[6] = opgaven[7]; opgaven[7] = temp;};/* loot 6 en 7 */
      }
    }  
#ifdef mydebug1
  IO_Writef("\nGeplaatst:");
  for (i = 0; i <length;i++) {
    IO_Writef("\n Plaatsing: %d CatDelta: %d", opgaven[i]->Plaatsing, opgaven[i]->CatDelta);
    }
#endif
  for(k=0;k<length;k++) {
    if (opgaven[k]->Plaatsing >= 990) break;
    i = k % Ndelen;		/* verdeel de geplaatsten over de groepjes */
    resultaat[(i*NsizeM + partCount[i])] = opgaven[k];    /* goed ??? */
    ++partCount[i];
    }
  }  /* einde sorteer op plaatsing */
for(;k<length;k++) {		/* voor iedere opgave.. ga door met k */
  for (i=0;i<Ndelen;i++){	/* ga alle delen langs */
    partEval[i] = 0;		/* bepaal factor per deel */
    if (OptVerh)
      partEval[i] -= beschikbaarheid(&resultaat[i*NsizeM],
					opgaven[k],partCount[i]);
    if (OptClub) {
      j = 0;
      while (j<partCount[i]) {
	if (!stricmp(resultaat[i*NsizeM+j]->Club,opgaven[k]->Club)
	    && *(opgaven[k]->Club)) {
	  partEval[i] -= 10;
	  }
	j++;
	}
      }
    }
  Nwork = Nsize;
  if (k >= length - Nrest) {
    Nwork++;		/* verdeel de rest over de achterste poules */
    OptVerh = 0;
    OptClub = 0;
    }
  random_1(2, &i);
  maxI = 0;
  maxEval = -999;
#ifdef mydebug2
  IO_Writef("\nk: %d, length - Nrest: %d", k, length - Nrest);
#endif
  if (i && (k < length - Nrest)) {			/* van links naar rechts	*/
#ifdef mydebug2
  IO_Writef(" links");
#endif
    for (i=0; i<Ndelen; i++) {
      if (partCount[i] >= Nwork) continue; /* is vol .. */
      if (partEval[i] > maxEval) {
	maxEval = partEval[i];
	maxI = i;
	}
      }
    }
  else {				/* van rechts naar links, ook 'rest' */
#ifdef mydebug2
  IO_Writef(" rechts");
#endif
    for (i=Ndelen-1; i>=0; i--) {
      if (partCount[i] >= Nwork) continue;
      if (!OptVerh && !OptClub) partEval[i] = 0; /* terugzetten voor restbestand */
      if (partEval[i] > maxEval) {
	maxEval = partEval[i];
	maxI = i;
	}
      }
    }  
#ifdef mydebug
  IO_Writef("\nmaxEval: %d, maxI: %d", maxEval, maxI);
  IO_Writef("\npartCount[%d]: %d", maxI, partCount[maxI]);
  IO_Writef("\nmaxI*NsizeM+partCount[maxI]= %d",maxI*NsizeM+partCount[maxI]);
#endif
  resultaat[maxI*NsizeM+partCount[maxI]] = opgaven[k];
  ++(partCount[maxI]);
#ifdef mydebug
  IO_Writef("\nloop, maxI: %d", maxI);
#endif
#ifdef mydebug2
  IO_Writef("\nNwork: %d ::", Nwork);
  for(i=0;i<Ndelen;i++) {
    IO_Writef(" %d", partCount[i]);
    }
#endif
  }  /* loop klaar */
#ifdef mydebug
  IO_Writef("\nloop klaar");
#endif
for (i=0; i<Ndelen; i++) {
  for (j=0; j<partCount[i]; j++) {
    resultaat[i*NsizeM+j]->CatDelta = i;  /* hier wordt het deel aangewezen */
    }
  }
}
