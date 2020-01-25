
/********************************************************************
  indelingsprogramma in C
  hoort bij TASMACHI.PRO
*********************************************************************/

#include <stdlib.h>
#include <string.h>
/*#include <stdio.h>
#include <dos.h>*/
/*#define MLINEAR*/

#include <pdcrunt.h>


/*#define mydebug*/
#define string      char*

/* Generates a fail */
void RUN_Fail(void);

/* readchar(char) -- (o) */
#define readchar IO_RdCh
extern void readchar(char*);

/* random(integer,integer) -- (i,o) */
#define random_1 INTG_Random
extern void random_1(int, int*);

/*void changestatus_0(char *status);*/

void cdecl IO_Writef(char *format,...);

/* beep() -- () */
#define beep OS_Beep
extern void beep(void);


/*** alles uit de prolog global domains ***/


#define listfno 1
#define nilfno  2

/* het prolog stringlist type */
typedef struct node {
  unsigned char functor;  /* the type                   */
  char *value;            /* a string pointer           */
  struct node *next;      /* pointer to next struc node */
} strlist;



/* integerlist */
typedef struct ilist {
  unsigned char functor;
  int Value;
  struct ilist *next;
} integerlist;

typedef int tijd;

typedef struct tlist {
  unsigned char functor;
  tijd baantijd;
  struct tlist *next;
} tijdlist;

typedef struct {
  unsigned char functor;
  tijd begin;
  tijd eind;
} periode;

typedef struct vlist {	  /* verhinderingen lijst */
  unsigned char functor;
  periode *verhindering;
  struct vlist *next;
} verhl;

typedef struct {
 /*   unsigned char functor;  is struct geworden !!*/
    int OpgaveNo;
    int Plaatsing;
    int Byes;
    int Clubdeelname;     /* werkveld voor sorteren */
    char *Club;
    tijdlist *Tijden;
    int LRating;
    int Random;
} record;

typedef struct jlist {	  /* lijst met indelings-gegevens */
  unsigned char functor;
  record *recp;
  struct jlist *next;
} rlist;

/* het rsoort domain */
#define poel    1
#define afval   2

typedef struct {
  unsigned char functor;
} rsoort;

#define janee_ja		1
#define janee_nee		2

typedef
  struct janee_struct {
	unsigned char fno;
  } JANEE;

#define nietgeplaatst 999
#define bye     0
#define open    -1

/****************************************************************
       dit is de C versie van de indeling...
 ****************************************************************/


extern void zwf(char *format, ...);

typedef struct eval_struct {
  signed int plaats;
  signed int evaluatie;
  record *geplaatste;
} eeval;


/*void far cdecl menu_0(int no, strlist *keuzen, char *titel,
	int begin, int *keuze, char *keuzestr[], JANEE *DirectDial);*/
/*void far cdecl menu_0(INTEGER integer,STRINGLIST *Keuzen,STRING Titel,
     INTEGER Begin,INTEGER *Keuze,STRING *Keuzestr,JANEE *DirectDial);*/


int static mycomp_plaatsing(const void *recp1, const void *recp2)
{
  return ((*(record **)recp1)->Plaatsing - (*(record **)recp2)->Plaatsing);
}

int static mycomp_random(const void *recp1, const void *recp2) /* ipv schudt */
{
  return ((*(record **)recp1)->Random - (*(record **)recp2)->Random);
}

int static mycomp_rating(const void *recp1, const void *recp2) /* rating % 6.11.2003 */
{
  if ((*(record **)recp1)->LRating == 0) return 99999-(*(record **)recp2)->LRating;
  if ((*(record **)recp2)->LRating == 0) return (*(record **)recp1)->LRating-99999;
  return ((*(record **)recp1)->LRating - (*(record **)recp2)->LRating);
}

int static mycomp_club(const void *recp1, const void *recp2)
{
  return(stricmp((*(record **)recp1)->Club, (*(record **)recp2)->Club));
}

int static mycomp_deelname(const void *recp1, const void *recp2)
{
  if ((*(record **)recp2)->Clubdeelname == (*(record **)recp1)->Clubdeelname)
   return(stricmp((*(record **)recp1)->Club, (*(record **)recp2)->Club));
  else
    return((*(record **)recp2)->Clubdeelname -
					(*(record **)recp1)->Clubdeelname);
}

/*
int static mycomp_int(const void *i1, const void *i2)
{
  return (*(int *)i1 - *(int *)i2);
}

*/

int static card(int a)
{
int i,b;
for(i = 1, b=a-1 ; b > 1 ; i *= 2)
  b >>= 1;
return(a == 0 ? 0 : i);
}

int static afstand(int a)
{
int i;
for(i = 0 ; a > 0 ; i += 10)
  a >>= 1;
return(i);
}


int static mycomp_hus(const void *recp1, const void *recp2)
{
  return((*(eeval *)recp1).evaluatie - (*(eeval *)recp2).evaluatie);
}

void static hushus(int aantal, eeval *husarray)
{
  int i;
  for (i = 0; i < aantal; i++) {
    husarray[i].plaats = i;
    random_1(100, &(husarray[i].evaluatie));  
    }
  qsort(husarray, aantal, sizeof(husarray[0]), mycomp_hus); 
}

void static tel_clubdeelname(record *lijst[], int aantal)
{
  char *Clubs;
  int i, aantal_per_club;

  qsort(lijst, aantal, sizeof(lijst[0]), mycomp_club);

#ifdef mydebug
    IO_Writef("\ntel_clubdeelname 1");
#endif
			  /* tel aantal per club */
  aantal_per_club = 0;
  for (i=0, Clubs = lijst[aantal-1]->Club; i < aantal; i++) {
    if(stricmp(Clubs,lijst[i]->Club) == 0)
      lijst[i]->Clubdeelname = ++aantal_per_club;
    else
      aantal_per_club = 0;
    Clubs = lijst[i]->Club;
  }
#ifdef mydebug
    IO_Writef("\ntel_clubdeelname 2");
#endif
                          /* geef allen van dezelfde club zelfde aantal */
  Clubs = lijst[aantal-1]->Club;
#ifdef mydebug
    IO_Writef("\ntel_clubdeelname 3");
#endif
  aantal_per_club = lijst[aantal-1]->Clubdeelname;
  for (i=aantal-2 ; i>=0; i--)
    if(stricmp(Clubs,lijst[i]->Club) != 0) {
      aantal_per_club = lijst[i]->Clubdeelname;
      Clubs           = lijst[i]->Club;
      if (stricmp(lijst[i]->Club,"") == 0) aantal_per_club = 0;
    }
    else
      lijst[i]->Clubdeelname = aantal_per_club;

}

void static plaatsen_rooster(int lengte, int *plaatsen1[])
{
  int i;
/*  char plts[] = {'\001','\002','\003','\004','\073'};*/
  int *plaatsen;
  plaatsen = MEM_AllocGStack((lengte+2) * sizeof(*plaatsen));
  for (i = 0 ; i < lengte ; i++) plaatsen[i] = 0;
  switch (lengte) {
  case 0 :
  case 1 :
  case 2 :
    plaatsen[0] = 1;
    plaatsen[1]  = 2;
    break;
  case 4 :
    plaatsen[0] = 1;
    plaatsen[1] = 4;
    plaatsen[2] = 3;
    plaatsen[3] = 2;
    break;
  case 8 :
    plaatsen[0] = 1;
    plaatsen[1] = 8;
    plaatsen[2] = 5; 
    plaatsen[3] = 4;
    plaatsen[4] = 3;
    plaatsen[5] = 6;
    plaatsen[6] = 7;
    plaatsen[7] = 2;
    break;
  case 16 :
    plaatsen[0] = 1;   /* A */
    plaatsen[1] = 16;  /* B */
    plaatsen[2] =  5;  /* C */
    plaatsen[3] = 12;  /* D */
    plaatsen[4] =  7;  /* E plaatsen met bye 	% 6.11.2003 */
    plaatsen[5] = 10;  /* F                                 */
    plaatsen[6] = 14;  /* G  (E2 bij 6)                     */
    plaatsen[7]   = 3; /* H  (G2 bij 7 F2 bij 6)            */
    plaatsen[8]  = 4;   /* I B2 */
    plaatsen[9] =  13;  /* J A2 */
    plaatsen[10] =  9;  /* K C2 */
    plaatsen[11] =  8;  /* L D2 */
    plaatsen[12] =  11; /* M E2 */
    plaatsen[13] =  6;  /* N F2 */
    plaatsen[14] =  15; /* O H2 */
    plaatsen[15] =  2;  /* P G2 */
    break;
  case 32 :
    plaatsen[0] = 1;    /* A */
    plaatsen[1] = 32;   /* B */
    plaatsen[2] =  9;   /* C */
    plaatsen[3] = 24;   /* D */
    plaatsen[4] = 16;   /* E */
    plaatsen[5] = 17;   /* F */
    plaatsen[6] =  8;   /* G */
    plaatsen[7] = 25;   /* H */
    plaatsen[8] =  6;   /* I */
    plaatsen[9] = 27;   /* J */
    plaatsen[10] = 19;  /* K */
    plaatsen[11] = 14;  /* L */
    plaatsen[12] = 11;  /* M */
    plaatsen[13] = 22;  /* N */
    plaatsen[14] = 30;  /* O */
    plaatsen[15] = 3;   /* P */
    plaatsen[16] = 4;   /* Q B2 !*/
    plaatsen[17] = 29;  /* R A2 !*/
    plaatsen[18] = 21;  /* S C2 */
    plaatsen[19] = 12;  /* T D2 */
    plaatsen[20] = 13;  /* U F2 !*/
    plaatsen[21] = 20;  /* V E2 !*/
    plaatsen[22] = 28;  /* W G2 */
    plaatsen[23] =  5;  /* X  H2 */
    plaatsen[24] = 26;  /* Y I2 */
    plaatsen[25] =  7;  /* Z J2 */
    plaatsen[26] = 18;  /* AA L2 !*/
    plaatsen[27] = 15;  /* BB K2 !*/
    plaatsen[28] = 23;  /* CC M2 */
    plaatsen[29] = 10;  /* DD N2 */
    plaatsen[30] = 31;  /* EE P2 !*/
    plaatsen[31] =  2;  /* FF O2 !*/
    break;
  case 64 :
    plaatsen[0] = 1;  /* A */
    plaatsen[1] = 64; /* B */
    plaatsen[2] = 17; /* C */
    plaatsen[3] = 48; /* etc. */
    plaatsen[4] = 32;
    plaatsen[5] = 33;
    plaatsen[6] = 16;
    plaatsen[7] = 49;
    plaatsen[8] =  9;
    plaatsen[9] = 56;
    plaatsen[10] = 25;
    plaatsen[11] = 40;
    plaatsen[12] = 24;
    plaatsen[13] = 41;
    plaatsen[14] =  8;
    plaatsen[15] = 57;
    plaatsen[16] =  6; /**/
    plaatsen[17] = 59;
    plaatsen[18] = 43;
    plaatsen[19] = 22;
    plaatsen[20] = 27;
    plaatsen[21] = 38;
    plaatsen[22] = 54;
    plaatsen[23] = 11;
    plaatsen[24] = 14;
    plaatsen[25] = 51;
    plaatsen[26] = 35;
    plaatsen[27] = 30;
    plaatsen[28] = 19;
    plaatsen[29] = 46;
    plaatsen[30] = 62;

    plaatsen[33] = 61;
    plaatsen[34] = 45;
    plaatsen[35] = 20;
    plaatsen[36] = 29;
    plaatsen[37] = 36;
    plaatsen[38] = 52;
    plaatsen[39] = 13;
    plaatsen[40] = 12;
    plaatsen[41] = 53;
    plaatsen[42] = 37;
    plaatsen[43] = 28;
    plaatsen[44] = 21;
    plaatsen[45] = 44;
    plaatsen[46] = 60;
    plaatsen[47] =  5; /**/
    plaatsen[48] = 58;
    plaatsen[49] =  7;
    plaatsen[50] = 42;
    plaatsen[51] = 23;
    plaatsen[52] = 39;
    plaatsen[53] = 26;
    plaatsen[54] = 55;
    plaatsen[55] = 10;
    plaatsen[56] = 50;
    plaatsen[57] = 15;
    plaatsen[58] = 34;
    plaatsen[59] = 31;
    plaatsen[60] = 47;
    plaatsen[61] = 18;
    plaatsen[62] = 63;
    plaatsen[63] = 2;   
    break;
  case 128 : /*  128  */
    plaatsen[0] = 1;  /* A */
    plaatsen[1] = 128;
    plaatsen[2] =  33;
    plaatsen[3] =  96;
    plaatsen[4] =  64;
    plaatsen[5] =  65;
    plaatsen[6] =  32;
    plaatsen[7] =  97;
    plaatsen[8] =  17;
    plaatsen[9] = 112;
    plaatsen[10] =  49;
    plaatsen[11] =  80;
    plaatsen[12] =  48;
    plaatsen[13] =  81;
    plaatsen[14] =  16;
    plaatsen[15] = 113;
    plaatsen[16] =   9;
    plaatsen[17] = 120;
    plaatsen[18] =  41;
    plaatsen[19] =  88;
    plaatsen[20] =  56;
    plaatsen[21] =  73;
    plaatsen[22] =  24;
    plaatsen[23] = 105;
    plaatsen[24] =  25;
    plaatsen[25] = 104;
    plaatsen[26] =  57;
    plaatsen[27] =  72;
    plaatsen[28] =  40;
    plaatsen[29] =  89;
    plaatsen[30] =   8;
    plaatsen[31]   = 121; 
    plaatsen[32] =   6; /**/
    plaatsen[33] = 123;
    plaatsen[34] =  91;
    plaatsen[35] =  38;
    plaatsen[36] =  59;
    plaatsen[37] =  70;
    plaatsen[38] = 102;
    plaatsen[39] =  27;
    plaatsen[40] =  22;
    plaatsen[41] = 107;
    plaatsen[42] =  75;
    plaatsen[43] =  54;
    plaatsen[44] =  43;
    plaatsen[45] =  86;
    plaatsen[46] = 118;
    plaatsen[47] =  11;
    plaatsen[48] =  14;
    plaatsen[49] = 115;
    plaatsen[50] =  83;
    plaatsen[51] =  46;
    plaatsen[52] =  51;
    plaatsen[53] =  78;
    plaatsen[54] = 110;
    plaatsen[55] =  19;
    plaatsen[56] =  30;
    plaatsen[57] =  99;
    plaatsen[58] =  67;
    plaatsen[59] =  62;
    plaatsen[60] =  35;
    plaatsen[61] =  94;
    plaatsen[62]   = 126;
    plaatsen[127] =   2;
    plaatsen[126] = 127;
    plaatsen[125] =  34;
    plaatsen[124] =  95;
    plaatsen[123] =  63;
    plaatsen[122] =  66;
    plaatsen[121] =  31;
    plaatsen[120] =  98;
    plaatsen[119] =  18;
    plaatsen[118] = 111;
    plaatsen[117] =  50;
    plaatsen[116] =  79;
    plaatsen[115] =  47;
    plaatsen[114] =  82;
    plaatsen[113] =  15;
    plaatsen[112] = 114;
    plaatsen[111] =  10;
    plaatsen[110] = 119;
    plaatsen[109] =  42;
    plaatsen[108] =  87;
    plaatsen[107] =  55;
    plaatsen[106] =  74;
    plaatsen[105] =  23;
    plaatsen[104] = 106;
    plaatsen[103] =  26;
    plaatsen[102] = 103;
    plaatsen[101] =  58;
    plaatsen[100] =  71;
    plaatsen[99] =  39;
    plaatsen[98] =  90;
    plaatsen[97] =   7;
    plaatsen[96] = 122;
    plaatsen[95] =   5; /**/
    plaatsen[94] = 124;
    plaatsen[93] =  92;
    plaatsen[92] =  37;
    plaatsen[91] =  60;
    plaatsen[90] =  69;
    plaatsen[89] = 101;
    plaatsen[88] =  28;
    plaatsen[87] =  21;
    plaatsen[86] = 108;
    plaatsen[85] =  76;
    plaatsen[84] =  53;
    plaatsen[83] =  44;
    plaatsen[82] =  85;
    plaatsen[81] = 117;
    plaatsen[80] =  12;
    plaatsen[79] =  13;
    plaatsen[78] = 116;
    plaatsen[77] =  84;
    plaatsen[76] =  45;
    plaatsen[75] =  52;
    plaatsen[74] =  77;
    plaatsen[73] = 109;
    plaatsen[72] =  20;
    plaatsen[71] =  29;
    plaatsen[70] = 100;
    plaatsen[69] =  68;
    plaatsen[68] =  61;
    plaatsen[67] =  36;
    plaatsen[66] =  93;
    plaatsen[65]   = 125;
    break;
#ifdef case256
  case 256 :
    i = 1;
    plaatsen[0] = 1; 
    plaatsen[i++] = 256;  /* 2 */
    plaatsen[i++] =  65;
    plaatsen[i++] = 192;
    plaatsen[i++] = 128;  /* 5, 6 */
    plaatsen[i++] = 129;
    plaatsen[i++] =  64;  /* 7, 8 */
    plaatsen[i++] = 193;
    plaatsen[i++] =  33;  /* 9, 10 */
    plaatsen[i++] = 224;
    plaatsen[i++] =  97;  /* 11, 12 */
    plaatsen[i++] = 160;
    plaatsen[i++] =  96;  /* 13, 14 */
    plaatsen[i++] = 161;
    plaatsen[i++] =  32;  /* 15, 16 */
    plaatsen[i++] = 225;
    plaatsen[i++] =  17;  /* 17, 18 */
    plaatsen[i++] = 240;
    plaatsen[i++] =  81;  /* 19, 20 */
    plaatsen[i++] = 176;
    plaatsen[i++] = 112;  /* 21, 22 */
    plaatsen[i++] = 145;
    plaatsen[i++] =  48;  /* 23, 24 */
    plaatsen[i++] = 209;
    plaatsen[i++] = 208;  /* 25, 26 */
    plaatsen[i++] =  49;
    plaatsen[i++] = 113;  /* 27, 28 */
    plaatsen[i++] = 144;
    plaatsen[i++] = 177;  /* 29, 30 */
    plaatsen[i++] =  80;
    plaatsen[i++] =  16;  /* 31, 32 */
    plaatsen[i++] = 241;
    plaatsen[i++] = 248;  /* 33, 34 */
    plaatsen[i++] =   9;
    plaatsen[i++] = 184;  /* 35, 36 */
    plaatsen[i++] =  73;
    plaatsen[i++] = 120;  /* 37, 38 */
    plaatsen[i++] = 137;
    plaatsen[i++] =  56;  /* 39, 40 */
    plaatsen[i++] = 201;
    plaatsen[i++] = 216;  /* 41, 42 */
    plaatsen[i++] =  41;
    plaatsen[i++] = 105;  /* 43, 44 */
    plaatsen[i++] = 152;
    plaatsen[i++] = 169;  /* 45, 46 */
    plaatsen[i++] =  88;
    plaatsen[i++] =  24;  /* 47, 48 */
    plaatsen[i++] = 233;
    plaatsen[i++] = 232;  /* 49, 50 */
    plaatsen[i++] =  25;
    plaatsen[i++] =  89;  /* 51, 52 */
    plaatsen[i++] = 168;
    plaatsen[i++] = 153;  /* 53, 54 */
    plaatsen[i++] = 104;
    plaatsen[i++] =  40;  /* 55, 56 */
    plaatsen[i++] = 217;
    plaatsen[i++] = 200;  /* 57, 58 */
    plaatsen[i++] =  57;
    plaatsen[i++] = 121;  /* 59, 60 */
    plaatsen[i++] = 136;
    plaatsen[i++] = 185;  /* 61, 62 */
    plaatsen[i++] =  72;
    plaatsen[i++] =   8;  /* 63, 64 */
    plaatsen[i] = 249;    /* !!! */
    i = lengte - 1;
    plaatsen[i--] = 2;    /* byes 1, 2 */
    plaatsen[i--] = 255;
    plaatsen[i--] =  66;  /* 3, 4 */
    plaatsen[i--] = 191;
    plaatsen[i--] = 130;  /* 5, 6 */
    plaatsen[i--] = 127;
    plaatsen[i--] =  63;  /* 7, 8 */
    plaatsen[i--] = 194;
    plaatsen[i--] =  34;  /* 9, 10 */
    plaatsen[i--] = 223;
    plaatsen[i--] =  98;  /* 11, 12 */
    plaatsen[i--] = 159;
    plaatsen[i--] =  95;  /* 13, 14 */
    plaatsen[i--] = 162;
    plaatsen[i--] =  31;  /* 15, 16 */
    plaatsen[i--] = 226;
    plaatsen[i--] =  18;  /* 17, 18 */
    plaatsen[i--] = 239;
    plaatsen[i--] =  82;  /* 19, 20 */
    plaatsen[i--] = 175;
    plaatsen[i--] = 111;  /* 21, 22 */
    plaatsen[i--] = 146;
    plaatsen[i--] =  47;  /* 23, 24 */
    plaatsen[i--] = 210;
    plaatsen[i--] = 207;  /* 25, 26 */
    plaatsen[i--] =  50;
    plaatsen[i--] = 114;  /* 27, 28 */
    plaatsen[i--] = 143;
    plaatsen[i--] = 178;  /* 29, 30 */
    plaatsen[i--] =  79;
    plaatsen[i--] =  15;  /* 31, 32 */
    plaatsen[i--] = 242;
    plaatsen[i--] = 247;  /* 33, 34 */
    plaatsen[i--] =  10;
    plaatsen[i--] = 183;  /* 35, 36 */
    plaatsen[i--] =  74;
    plaatsen[i--] = 119;  /* 37, 38 */
    plaatsen[i--] = 138;
    plaatsen[i--] =  55;  /* 39, 40 */
    plaatsen[i--] = 202;
    plaatsen[i--] = 215;  /* 41, 42 */
    plaatsen[i--] =  42;
    plaatsen[i--] = 106;  /* 43, 44 */
    plaatsen[i--] = 151;
    plaatsen[i--] = 170;  /* 45, 46 */
    plaatsen[i--] =  87;
    plaatsen[i--] =  23;  /* 47, 48 */
    plaatsen[i--] = 234;
    plaatsen[i--] = 231;  /* 49, 50 */
    plaatsen[i--] =  26;
    plaatsen[i--] =  90;  /* 51, 52 */
    plaatsen[i--] = 167;
    plaatsen[i--] = 154;  /* 53, 54 */
    plaatsen[i--] = 103;
    plaatsen[i--] =  39;  /* 55, 56 */
    plaatsen[i--] = 218;
    plaatsen[i--] = 199;  /* 57, 58 */
    plaatsen[i--] =  58;
    plaatsen[i--] = 122;  /* 59, 60 */
    plaatsen[i--] = 135;
    plaatsen[i--] = 186;  /* 61, 62 */
    plaatsen[i--] =  71;
    plaatsen[i--] =   7;  /* 63, 64 */
    plaatsen[i--] = 250;
    plaatsen[i--] =   6;    /* 65 */
    plaatsen[i--] = 252;
    plaatsen[i--] = 188;
    plaatsen[i--] =  70;
    plaatsen[i--] = 124;
    plaatsen[i--] = 134;  /* 70 */
    plaatsen[i--] = 198;
    plaatsen[i--] =  60;
    plaatsen[i--] =  38;
    plaatsen[i--] = 220;
    plaatsen[i--] = 156;  /* 75 */
    plaatsen[i--] = 102;
    plaatsen[i--] =  92;
    plaatsen[i--] = 166;
    plaatsen[i--] = 230;
    plaatsen[i--] =  28;  /* 80 */
    plaatsen[i--] =  22;
    plaatsen[i--] = 236;
    plaatsen[i--] = 172;
    plaatsen[i--] =  86;
    plaatsen[i--] = 108;  /* 85 */
    plaatsen[i--] = 150;
    plaatsen[i--] = 214;
    plaatsen[i--] =  44;
    plaatsen[i--] =  54;
    plaatsen[i--] = 204;  /* 90 */
    plaatsen[i--] = 140;
    plaatsen[i--] = 118;
    plaatsen[i--] =  76;
    plaatsen[i--] = 182;
    plaatsen[i--] = 246;  /* 95 */
    plaatsen[i--] =  12;
    plaatsen[i--] =  14;
    plaatsen[i--] = 244;
    plaatsen[i--] = 180;
    plaatsen[i--] =  78;  /* 100 */
    plaatsen[i--] = 116;
    plaatsen[i--] = 142;
    plaatsen[i--] = 206;
    plaatsen[i--] =  52;
    plaatsen[i--] =  46;  /* 105 */
    plaatsen[i--] = 212;
    plaatsen[i--] = 148;
    plaatsen[i--] = 110;
    plaatsen[i--] =  84;
    plaatsen[i--] = 174;  /* 110 */
    plaatsen[i--] = 238;
    plaatsen[i--] =  20;
    plaatsen[i--] =  30;
    plaatsen[i--] = 228;
    plaatsen[i--] = 164;  /* 115 */
    plaatsen[i--] =  94;
    plaatsen[i--] = 100;
    plaatsen[i--] = 158;
    plaatsen[i--] = 222;
    plaatsen[i--] =  36;  /* 120 */
    plaatsen[i--] =  62;
    plaatsen[i--] = 196;
    plaatsen[i--] = 132;
    plaatsen[i--] = 126;
    plaatsen[i--] =  68;  /* 125 */
    plaatsen[i--] = 189;
    plaatsen[i] = 254;    /* geen i-- ! */
 #endif
  }
  for (i=0; i < lengte; --plaatsen[i++]);/* decr. alle entries naar base 0 */
  *plaatsen1 = plaatsen;
}

int static beschikbaarheid(record *r1[], record *r5)
{
int done=0, n, imax, i=0, nlow=999, nhigh=0, ndoorsnee=0;
tijdlist *ptemp, *p[5];

for (n=0; n<4; n++)
  if (r1[n]->OpgaveNo != bye && r1[n]->OpgaveNo != open)
    p[i++] = r1[n]->Tijden;
if (r5->OpgaveNo != bye && r5->OpgaveNo != open)
  p[i++] = r5->Tijden;
imax = i;
for (i=0; i<imax; i++) {
  for (n=0, ptemp=p[i];ptemp->functor==listfno;ptemp=ptemp->next, n++);
  nlow  = min(n,nlow);
  nhigh = max(n,nhigh);
  }
if (nhigh==0) return(0);
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

void static evalueer(record *tegenstander, record *indeling[],
							int roosterlengte)
{
int j,open_plaatsen;
signed int lokaal,m,n,m1,n1;
eeval e[64];
/*  char dummy; */
  for (j=0; j < (roosterlengte>>2); j++) {
    e[j].plaats    = j;
    e[j].evaluatie = 0;                   /* tel de open plaatsen */
    for (lokaal=0, open_plaatsen=0; lokaal<4; lokaal++) {
      if (indeling[j*4 + lokaal]->OpgaveNo==open)
	open_plaatsen++;
      }
    if (open_plaatsen       != 0       &&
	*tegenstander->Club != '\0'    &&
	tegenstander->Clubdeelname != 0  ) {
      for (m=j; m >= 0; m--) {
	for (lokaal=0; lokaal<4; lokaal++)
	  if (stricmp(indeling[m*4 + lokaal]->Club,tegenstander->Club) == 0)
	    break;
	if (lokaal<4) {              /* lokaal=4 : niet gevonden */
	  m1 = afstand(m ^ j);
	  break;
	  }
	}                       /* m = -1   : helemaal niet gevonden */
      if (m < 0) m1 = afstand(roosterlengte/4);
      for (n=j+1; n < roosterlengte/4; n++) {
	for (lokaal=0; lokaal <4; lokaal++)
	  if (stricmp(indeling[n*4 + lokaal]->Club,tegenstander->Club) == 0)
	    break;
	if (lokaal<4) {               /* lokaal=4 : niet gevonden */
	  n1 = afstand(n ^ j);
	  break;
	  }
	}                     /* n=roosterlengte/4: helemaal niet gevonden */
      if (n >= roosterlengte/4) n1 = afstand(n);
      e[j].evaluatie = min(m1,n1) + open_plaatsen;
      }
    else e[j].evaluatie = open_plaatsen; /* i.g.v. Club = "" etc. */
    if (open_plaatsen==0) e[j].evaluatie=-10;
    if (open_plaatsen!=0)
	e[j].evaluatie += beschikbaarheid(&indeling[j*4],tegenstander);
    }


  random_1(100, &j);
  if (j < 50) {
    for (j = 0, m = -8, n = 0; j < roosterlengte/4; j++) {
      if (e[j].evaluatie > m) {
	m = e[j].evaluatie;
	n = e[j].plaats;
      }
    }
  }
  else {
    for (j = roosterlengte/4 - 1, m = -8, n = 0; j >= 0; j--) {
      if (e[j].evaluatie > m) {
	m = e[j].evaluatie;
	n = e[j].plaats;
      }
    }
  }
/*  zwf(" %d",n);
  IO_Writef("\n%d ", n);
    readchar(&dummy);
    */

  random_1(100, &j);
  for (m=0 ; m<4; m++) {
    switch (m) {
      case 0: lokaal=(j < 50 ? 0 : 2); break;
      case 1: lokaal=(j < 50 ? 2 : 0); break;
      case 2: lokaal=1; break;
      default: lokaal=m;
      }
    if (indeling[n*4 + lokaal]->OpgaveNo == open) {
      m1 = lokaal;
      if (stricmp(indeling[(n*4 + lokaal) ^ 1]->Club, tegenstander->Club))
	break;
      }
    }
  indeling[n*4 + m1] = tegenstander;
}

/*_________________________________________________________________*/
/*  shift de deelnemers rond                                       */
/*  wordt gebruikt in het poule rotatie-schema                     */
/*  voorbeeld:                                                     */
/*            A  B       A  F       A  E       A  D      A  C      */
/*                                                                 */
/*            F  C  -->  E  B  -->  D  F  -->  C  E  --> B  D      */
/*                shiftp                                           */
/*            E  D       D  C       C  B       B  F      F  E      */
/*_________________________________________________________________*/
void shiftp(rlist **listar, int len)
{
rlist *temp;
int i;
len--;			   /* adjust array offset                   */
temp = listar[1];	   /* listar[0] blijft waar hij is          */
for(i = 1; i < len; i++)
  listar[i] = listar[i+1]; /* schuif alle anderen een naar voren    */
listar[len] = temp;
}

void myloting(rlist *inlist, int reserveer, integerlist **result, int *length1, rsoort *type) /* i, i, o, o, i */
{
/*  for(length=0; inlist->functor==listfno; inlist=inlist->next) {
    length++;
#ifdef mydebug
    IO_Writef("\nRecord: %d", length); 
#endif
    if (inlist->recp->Byes != 0)
      gedwongen_byes = gedwongen_byes + (1<<inlist->recp->Byes) - 1;
    } */
/*  rlist *curlist;*/
  rlist *savelist;
  rlist **listar;
  record **geplaatsten, **gewonen, **indeling;
  int i,j,k,length,length_in,roosterlengte,
	  aantal_gewonen,
	  aantal_byes,
	  aantal_geplaatsten,
	  aantal_officieel_geplaatsten,
	  max_geplaatsten,
	  bloksize;
  int aantal = 0;
  int gedwongen_byes = 0;
  record  byerec,open_plaats;
  int *plaatsen;
  unsigned char nullist = nilfno;
  integerlist *q, *p1;
  rlist reserve;
  eeval husredirect[8];
#ifdef mydebug
/*  char dummy;*/
#endif

/*  byerec.functor      = 1; ____ overgang op 'functorloos' ____*/
  byerec.OpgaveNo     = bye;
  byerec.Plaatsing    = nietgeplaatst;
  byerec.Byes         = 0;
  byerec.Clubdeelname = 0;
  byerec.Club         = "bye";               /* = 0 */
  byerec.Tijden       = (tijdlist *) &nullist;
  byerec.LRating      = 99999;
  byerec.Random       = 0;

/*  open_plaats.functor      = 1;*/
  open_plaats.OpgaveNo     = open;           /* = -1 */
  open_plaats.Plaatsing    = nietgeplaatst;
  open_plaats.Byes         = 0;
  open_plaats.Clubdeelname = 0;
  open_plaats.Club         = "open";
  open_plaats.Tijden       = (tijdlist *) &nullist;
  open_plaats.LRating      = 99999;
  open_plaats.Random       = 0;

  reserve.functor = 1;
  reserve.recp = &open_plaats;
  reserve.next = NULL;
  savelist = inlist;

			  /* tel de records  en reset de clubdeelname */
  for(length=0; inlist->functor==listfno; inlist=inlist->next) {
    inlist->recp->Clubdeelname = 0;
    length++;
#ifdef mydebug
    IO_Writef("\nRecord: %d", length); 
#endif
    if (inlist->recp->Byes != 0)
      gedwongen_byes = gedwongen_byes + (1<<inlist->recp->Byes) - 1;
    }
  if (type->functor == poel) {
#ifdef mydebug
    IO_Writef("\nPoele"); 
#endif
  /*______________________________________________________________________*/
  /*                           maak poeletje                              */
  /*______________________________________________________________________*/
    /*length += reserveer;*/
    reserveer = 0;
    *length1 = (length * (length - 1));
    listar = MEM_AllocGStack((length+1) * sizeof(* listar[0]));
    i = 0;
    for (inlist = savelist; inlist->functor==listfno; inlist=inlist->next) {
      listar[i++] = inlist;
      }
    if (reserveer) {
      for (j = 0; j < reserveer; j++) {
	listar[i++] = &reserve;
	}
      }
    if (length % 2) {   /*________________________________________________*/
      length++;         /*       vul aan tot even                         */
      listar[i] = NULL; /*________________________________________________*/
      }
    p1 = *result = MEM_AllocGStack(*length1 * sizeof(integerlist) * 2 + 100);
    for (i = 1; i < length; i++) { /* length - 1 keer */
      shiftp(listar, length);		/* shuif het array een keer rond */
      if (listar[1]) {
	p1->functor =listfno;
	p1->Value   = listar[0]->recp->OpgaveNo;
	q = p1;
	q->next = ++p1;
	p1->functor =listfno;                     /* de andere    */
	p1->Value   = listar[1]->recp->OpgaveNo;
	q = p1;
	q->next = ++p1;
	}
      for(j = 0 ; j < length / 2 - 1 ; j++) {
	if (listar[2+j] && listar[length - 1 - j]) {
	  p1->functor=listfno;
	  p1->Value = listar[2+j]->recp->OpgaveNo;
	  q = p1;
	  q->next = ++p1;
	  p1->functor=listfno;                     /* de andere    */
	  p1->Value = listar[length - 1 - j]->recp->OpgaveNo;
	  q = p1;
	  q->next = ++p1;
	  }
	}
      }
    p1->functor = nilfno;
    return;
    }
  /*______________________________________________________________________*/
  /*                     einde maak poeletje                              */
  /*______________________________________________________________________*/

  /* ------------------- maak afvalschema -------------------------*/

  *result = MEM_AllocGStack(sizeof(integerlist));
  (*result)->functor=nilfno;
  length_in = length;
  length += reserveer;
  roosterlengte    = card(length + gedwongen_byes) * 2;
  *length1 = roosterlengte - 1;	/* vast even setten */
  if (roosterlengte > 128) {
    IO_Writef("\tschema wordt te groot! (>128)"); 
    RUN_Fail();
  }
  max_geplaatsten = 2;
/* _______________________________ zo was het ______________________
  if ((length + gedwongen_byes) > 12) max_geplaatsten = 4;
  if ((length + gedwongen_byes) > 24) max_geplaatsten = 8;
  if ((length + gedwongen_byes) > 48) max_geplaatsten = 16;
  if ((length + gedwongen_byes) > 96) max_geplaatsten = 32;
  _________________ zo werd het op veler verzoek ... ______________*/
  if ((length + gedwongen_byes) > 8)  max_geplaatsten = 4;
  if ((length + gedwongen_byes) > 16) max_geplaatsten = 8;
  if ((length + gedwongen_byes) > 32) max_geplaatsten = 16;
  if ((length + gedwongen_byes) > 64) max_geplaatsten = 32;

  geplaatsten      = MEM_AllocGStack(roosterlengte * sizeof(record *));
			  /* vul het geplaatsten met recordpointers */
			  /* en zet de plaatsing op 999 als 0 */
			  /* dat sorteert makkelijker         */
  gewonen          = MEM_AllocGStack(roosterlengte * sizeof(record *));
  indeling         = MEM_AllocGStack(roosterlengte * sizeof(record *));
  for (i=0; i<roosterlengte; i++) {
    geplaatsten[i] = &open_plaats;
    gewonen[i]     = &open_plaats;
  }

  /* loop de list door en verhuis de geplaatsten
     resp. de gewonen (niet geplaatst) naar hun array */
  aantal_geplaatsten = 0;
  aantal_gewonen     = 0;
  inlist             = savelist;
  for (i=0; inlist->functor==listfno; inlist=inlist->next) {
    if (inlist->recp->Plaatsing == 0 || inlist->recp->Plaatsing == nietgeplaatst) {
      inlist->recp->Plaatsing   = nietgeplaatst;
      gewonen[aantal_gewonen++] = inlist->recp;
    }
    else
      geplaatsten[aantal_geplaatsten++] = inlist->recp;
    indeling[i++] = inlist->recp;  /* tijdelijk gebruik van indeling array */
  }
  plaatsen_rooster(roosterlengte, &plaatsen);
  aantal_officieel_geplaatsten = aantal_geplaatsten; /* we gaan nog meer doen met het geplaatsten-array */

#ifdef mydebug
  IO_Writef("\nreserveer: %d roosterlengte: %d length_in: %d\nBoekje: ", reserveer, roosterlengte, length_in);
  for (i=0 ; i < roosterlengte ; i++)
    IO_Writef("%d ", plaatsen[i]);
#endif

  tel_clubdeelname(indeling, length_in); /* het werkveld wordt opgevuld */

#ifdef mydebug
    IO_Writef("\naantal geplaatsten: %d", aantal_geplaatsten);
    IO_Writef("\ntel_clubdeelname gepasseerd");

#endif

  for (i=0; i<roosterlengte; i++)        /* veeg indeling weer schoon   */
    indeling[i]    = &open_plaats;

#ifdef mydebug
    IO_Writef("\nveeg indeling gepasseerd");
#endif
  qsort(geplaatsten, aantal_geplaatsten,
			sizeof(geplaatsten[0]), mycomp_plaatsing);

  while (aantal_geplaatsten > max_geplaatsten)  {   /* hevel over */
#ifdef mydebug
    IO_Writef("\nuit geplaatsten verwijderd %d --> gewonen", aantal_geplaatsten);
#endif
    gewonen[aantal_gewonen++] = geplaatsten[--aantal_geplaatsten];
    geplaatsten[aantal_geplaatsten] = &open_plaats;
    }

if (aantal_gewonen > 0) {
  for(i=0; i < aantal_gewonen;i++) {
    random_1(3000, &j);
    gewonen[i]->Random = j;  /* veld wordt apart gebruikt voor random */
    };
  qsort(gewonen, aantal_gewonen, sizeof(gewonen[0]), mycomp_random);
  }
  
#ifdef mydebug
  IO_Writef("\nGewonen random gesorteerd");
  for (i=0 ; i < aantal_gewonen ; i++)
    IO_Writef("\n%d %d %d", gewonen[i]->OpgaveNo, gewonen[i]->Random, gewonen[i]->LRating);
#endif

  qsort(gewonen, aantal_gewonen, sizeof(gewonen[0]), mycomp_rating); /*  6.11.2003  nieuwe regel 2004 */

#ifdef mydebug
  IO_Writef("\nGewonen volgens rating ");
  for (i=0 ; i < aantal_gewonen ; i++)
    IO_Writef("\n%d %d %d", gewonen[i]->OpgaveNo, gewonen[i]->Random, gewonen[i]->LRating);
#endif

  aantal_byes = roosterlengte-length-gedwongen_byes;

  i = aantal_geplaatsten;
  while (i < aantal_byes)  {   /* hevel over */
    geplaatsten[i++] = gewonen[0]; /* nieuwe regel om laagste rating een bye te geven */
    aantal_gewonen--;
    gewonen++;
    }


#ifdef mydebug
  IO_Writef("\nGeplaatsten: ");
  for(i = 0 ; i < aantal_geplaatsten ; i++)
		IO_Writef("%d ", geplaatsten[i]->OpgaveNo);
  IO_Writef("\nGewonen: ");
  for(i = 0 ; i < aantal_gewonen ; i++)
		IO_Writef("%d ", gewonen[i]->OpgaveNo);
#endif


		/* zet de rest op byes in het geplaatstenarray */
		/* vul die byes van onderen af in              */

#ifdef mydebug
  IO_Writef("\nroosterlengte : %d", roosterlengte);
  IO_Writef("\nlength        : %d", length);
  IO_Writef("\ngedwongen_byes: %d", gedwongen_byes);
  IO_Writef("\naantal_byes   : %d", aantal_byes);
#endif

  for(i=roosterlengte-1, j=0; j < aantal_byes; j++) {
    while(geplaatsten[roosterlengte-i-1]->Byes != 0)
      i--;      /* sla gedwongen byes over                     */
    geplaatsten[i--] = &byerec;
  }
#ifdef mydebug
  IO_Writef("\nGeplaatsten: ");
  for(i = 0 ; i < roosterlengte ; i++)
		IO_Writef("%d ", geplaatsten[i]->OpgaveNo);
#endif

			/*  loting 3/4 etc. (de eventuele bye moet mee!)*/
/*  if (aantal_officieel_geplaatsten > 2) {
      bloksize = 2;
      hushus(bloksize, husredirect);
    } */
  if (aantal_officieel_geplaatsten > 2) { /* loting in blokken */
    bloksize = 2;
    for (i=2; i < aantal_officieel_geplaatsten; i += bloksize) {
      switch (i) {
        case 4 : bloksize = 4; break;
        case 16 : bloksize = 8; break;
        default : bloksize = 2;
        } 
      hushus(bloksize, husredirect);
 #ifdef mydebug
  IO_Writef("\nHusredir: ");
  for(k = 0 ; k < bloksize ; k++) IO_Writef("%d ", husredirect[k].plaats);
  IO_Writef("\n        : ");
  for(k = 0 ; k < bloksize ; k++) IO_Writef("%d ", husredirect[k].evaluatie);
#endif
      for (j = 0; j < bloksize; j++) {
        husredirect[j].geplaatste = geplaatsten[i+j]; /* tussenopslag */
        }
      for (j = 0; j < bloksize; j++) {
        geplaatsten[i+husredirect[j].plaats] = husredirect[j].geplaatste;
        }
      for (j = 0; j < bloksize; j++) {
        husredirect[j].geplaatste = geplaatsten[roosterlengte-i-j-1]; /* tussenopslag */
        }
      for (j = 0; j < bloksize; j++) {
        geplaatsten[roosterlengte-i-husredirect[j].plaats-1] = husredirect[j].geplaatste;
        }
      }
  }
 
#ifdef mydebug
  IO_Writef("\nGehusseld:   ");
  for(i = 0 ; i < roosterlengte ; i++)
		IO_Writef("%d ", geplaatsten[i]->OpgaveNo);
#endif

  /* transfer naar indelings-array */
  for (i=0; i < roosterlengte; i++)
    if (plaatsen[i] >= 0)
      indeling[plaatsen[i]] = geplaatsten[i];

#ifdef mydebug
  IO_Writef("\nIndeling1: ");
  for(i = 0 ; i < roosterlengte ; i++)
		IO_Writef("%d ", indeling[i]->OpgaveNo);
#endif

  k = 0;  /* vul met de gedwongen byes */
  for (i=0; i<roosterlengte; i++)
    if ((j = indeling[i]->Byes) != 0) {
      aantal = 1 << j;
      k = i >> j; /* veeg laatste bitjes schoon */
      k = k << j;
      for (j = 0; j < aantal; j++,k++)
	if (k != i) {                              /* niet persoon zelf    */
	  if (indeling[k]->OpgaveNo != open &&     /* ook niet iets anders */
	      indeling[k]->OpgaveNo != bye)  {
#ifdef mydebug
	    IO_Writef("\ni = %d \nj = %d \nk = %d", i, j, k);
#endif
            IO_Writef("\taantal gedwongen byes te groot!");
	    RUN_Fail();
	  } ;
	  indeling[k] = &byerec;
	} ;
    } ;

#ifdef mydebug
  IO_Writef("\nIndeling2: ");
  for(i = 0 ; i < roosterlengte ; i++)
		IO_Writef("%d ", indeling[i]->OpgaveNo);
  IO_Writef("\nAantal gewonen: %d", aantal_gewonen);
#endif

if (aantal_gewonen > 0) {
  for(i=0; i < aantal_gewonen;i++) {
    random_1(3000, &j);
    gewonen[i]->Random = j;  
    };
  qsort(gewonen, aantal_gewonen, sizeof(gewonen[0]), mycomp_random);

#ifdef mydebug
    IO_Writef("\ngeschud");
#endif

#ifdef mydebug
    IO_Writef("\ngesorteerd: ");
    for(i = 0 ; i < aantal_gewonen ; i++)
	IO_Writef("%d ", gewonen[i]->OpgaveNo);
    IO_Writef("\n          : ");
    for(i = 0 ; i < aantal_gewonen ; i++)
	IO_Writef("%d ", gewonen[i]->Clubdeelname);
#endif

  if (roosterlengte > 3) {
    qsort(gewonen, aantal_gewonen, sizeof(gewonen[0]), mycomp_deelname);
    for (i=0; i < aantal_gewonen; i++) {
      evalueer(gewonen[i], indeling, roosterlengte);
    }
  }
  else { 
    for (i=0; i < aantal_gewonen; i++) {
      for (j = 0; j < roosterlengte; j++) {
        if (indeling[j]->OpgaveNo == open) {
	  indeling[j]  = gewonen[i];
	  break;
	  }
	}
      }	
    } 
} /* einde aantalgewonen > 0 */
  
  for (i=0; i < roosterlengte; i++) {
    (*result)->functor  = listfno;
    (*result)->Value    = indeling[i]->OpgaveNo;
/*    if ((*result)->Value == open) (*result)->Value = bye; */
    (*result)->next     = MEM_AllocGStack(sizeof(integerlist));
    result = &(*result)->next;
  }
    (*result)->functor=nilfno;
    *length1 = roosterlengte - 1;
}