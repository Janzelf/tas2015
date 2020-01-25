#include <pdcrunt.h>

/*#include <stdio.h>*/
#include <curl/curl.h>
#include <string.h>
#include <curl/types.h>
#include <curl/easy.h>
#include <memory.h>
#include <stdlib.h>
#define ZLIB_WINAPI
#define BUFLEN 5000 
#include <zlib.h>

#define listfno 1
#define nilfno  2

int first = 1;

static char buf[BUFLEN];

int gz_compress(FILE *in, gzFile out)
{
    int len;
    /*int err;*/
    for (;;) {
        len = (int)fread(buf, 1, sizeof(buf), in);
        if (ferror(in)) {
            IO_Writef("\n ###C ferror in  ");
            return 1;
        }
        if (len == 0) break;

        if (gzwrite(out, buf, (unsigned)len) != len)
        { 
          IO_Writef("\n ###C gzerror out  ");
          return 1;/*error(gzerror(out, &err));*/}
        }
    fclose(in);
    if (gzclose(out) != Z_OK){
     IO_Writef("\n ###C gzerror close  ");
     return 1;/*error("failed gzclose");*/
     }
    return 0;
}

int gz_uncompress(gzFile in, FILE *out)
{
   int len;
   for(;;) 
   {
        len = gzread(in, buf, sizeof(buf));
        if (len == 0) break;
        fwrite(buf, 1, (unsigned)len, out);/*error(gzerror(out, &err));*/
   }
   gzclose(in);
   fclose(out);
   return 0;
}

int file_decompress(char *file, char *outfile)
{
    FILE  *out;
    gzFile in;
    
    /*IO_Writef("\n#### (.c) %s", outfile);*/
    in = gzopen(file, "rb");
    if (in == NULL) {
	IO_Writef("\ngz file error: %s  ", file);
        return 1;
    }
    out = fopen(outfile, "wb");
    if (out == NULL) {
	IO_Writef("\nffile error: %s  ", outfile);
        return 2;
    }
    gz_uncompress(in, out);
    /*unlink(file);*/
    return 0;
}

int file_compress(char *infile, char *outfile)
{
    /*static char outfile[250];*/
    FILE  *in;
    gzFile out;

/*    strcpy(outfile, file);
    strcat(outfile, ".gz");*/

    in = fopen(infile, "rb");
    if (in == NULL) {
	IO_Writef("\nfile error: %s  ", infile);
        return 1;
    }
    out = gzopen(outfile, "wb6 ");
    if (out == NULL) {
	IO_Writef("\ngzfile error: %s  ", outfile);
        return 2;
    }
    gz_compress(in, out);
    /*unlink(file);*/
    return 0;
}

  struct MemoryStruct {
    char *memory;
    size_t size;
  };
  
typedef
  struct stringlist_struct {
	unsigned char fno;
	char* string;
	struct stringlist_struct *next;
  } stringlist;

size_t  WriteCallback(void *ptr, size_t size, size_t nmemb, void *File)
{
   register int realsize = size * nmemb;
   /*fprintf((FILE *)File, "-1-");*/
   fwrite(ptr, size, nmemb, (FILE *)File); 
   return realsize;
};

size_t read_callback(void *ptr, size_t size, size_t nmemb, void *stream)
{
  size_t retcode;

  /* in real-world cases, this would probably get this data differently
     as this fread() stuff is exactly what the library already would do
     by default internally */
  retcode = fread(ptr, size, nmemb, (FILE *)stream);

  /* fprintf(stderr, "*** We read %d bytes from file\n", retcode); */

  return retcode;
}
char referer[] = "toernooiassistent";
char koekfile[] = "geen.txt";

void     checkvoortgang(int, char *msg);
int responseOK(char *Bestandsnaam, char *Zoektekst);
void htmlTimer(char *Tekst);

void curlprogressPr(double, double, double, double);

int pc (void *curlprogressP,
                                      double dltotal,
                                      double dlnow,
                                      double ultotal,
                                      double ulnow) {
  curlprogressPr(dltotal, dlnow, ultotal, ulnow);
  return 0;
  }

int programmaUpdate(int Step, char *URL, char *uitBestand, stringlist *aanmeldenVars)
{
  CURLcode res;
  struct curl_httppost *formpost=NULL;
  struct curl_httppost *lastptr=NULL;
  FILE *uitF;
  CURL *curl;

  if (first) {
    curl_global_init(CURL_GLOBAL_ALL);	/* --   winsock  -- */
    first = 0;
    }
if (Step == 1) {
  curl = curl_easy_init();		/* -- get handle -- */
  if (!(uitF = fopen(uitBestand, "w"))) {
      curl_easy_cleanup(curl);
    return 1;
  };
  
  curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
  curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)uitF);
  curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, TRUE);
  curl_easy_setopt(curl, CURLOPT_REFERER, referer);

/*  varList = aanmeldenVars(&t1, &t2);*/

  for(; aanmeldenVars->fno==listfno; aanmeldenVars=aanmeldenVars->next->next) { /* gepaard! */
    curl_formadd(&formpost,
                 &lastptr,
                 CURLFORM_COPYNAME, aanmeldenVars->string,
                 CURLFORM_COPYCONTENTS, aanmeldenVars->next->string,
                 CURLFORM_END);
  }
  curl_easy_setopt(curl, CURLOPT_URL, URL);
  curl_easy_setopt(curl, CURLOPT_HTTPPOST, formpost);
  res = curl_easy_perform(curl);
  curl_formfree(formpost);
  if (res > 0) {
    fprintf(uitF, "\n<MELDING>Geen verbinding voor update. (returncode %u).</MELDING>", res);
    curl_easy_cleanup(curl);
    fclose(uitF);
    return 3;
  }
  curl_easy_cleanup(curl);
  fclose(uitF);
return 0;
}
if (Step == 2) {  /* de download zelf */
  curl = curl_easy_init();		/* -- get handle -- */
  if (!(uitF = fopen(uitBestand, "wb"))) {
      curl_easy_cleanup(curl);
    return 1;
  };
  
  curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
  curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)uitF);
  curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, TRUE);
  curl_easy_setopt(curl, CURLOPT_REFERER, referer);

/*  varList = aanmeldenVars(&t1, &t2);*/

  for(; aanmeldenVars->fno==listfno; aanmeldenVars=aanmeldenVars->next->next) { /* gepaard! */
    curl_formadd(&formpost,
                 &lastptr,
                 CURLFORM_COPYNAME, aanmeldenVars->string,
                 CURLFORM_COPYCONTENTS, aanmeldenVars->next->string,
                 CURLFORM_END);
  }
  curl_easy_setopt(curl, CURLOPT_URL, URL);
  curl_easy_setopt(curl, CURLOPT_HTTPPOST, formpost);
  curl_easy_setopt(curl, CURLOPT_NOPROGRESS, 0);
  curl_easy_setopt(curl, CURLOPT_PROGRESSFUNCTION, pc);
  res = curl_easy_perform(curl);
  curl_formfree(formpost);
  if (res > 0) {
    fprintf(uitF, "\n<MELDING>Geen verbinding voor update. (returncode %u).</MELDING>", res);
    curl_easy_cleanup(curl);
    fclose(uitF);
    return 3;
  }
  curl_easy_cleanup(curl);
  fclose(uitF);
return 0;
}
return 10;
}


int upload2klapper(char *URL, char *bestand, char *rapport, stringlist *aanmeldenVars)
{
  CURL *curl;
  CURLcode res;
  struct curl_httppost *formpost=NULL;
  struct curl_httppost *lastptr=NULL;
  /*char referer[] = "toernooiassistent";
  char koekfile[] = "geen.txt";		 cookies worden niet bewaard */
  char filenaamu[2500];
  char filenaamr[250];
  FILE *uit;
  strcpy(filenaamr, rapport);
  strcpy(filenaamu, URL);
  if (first) {
    curl_global_init(CURL_GLOBAL_ALL);	/* --   winsock  -- */
    first = 0;
    }
  if (!(uit = fopen(filenaamr, "w"))) {
    return 1;
    };
  curl = curl_easy_init();		/* -- get handle -- */
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)uit);
    curl_easy_setopt(curl, CURLOPT_NOPROGRESS, 0);
    curl_easy_setopt(curl, CURLOPT_PROGRESSFUNCTION, pc);
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, TRUE);
    curl_easy_setopt(curl, CURLOPT_REFERER, referer);
    /*htmlTimer("Begin upload");*/
    /* ########### eind van stap 1 ############ */
    /*curl_easy_setopt(curl, CURLOPT_VERBOSE, TRUE);*/

    /* Fill in the file upload field */
    curl_formadd(&formpost,
                 &lastptr,
                 CURLFORM_COPYNAME, "Bestand",
                 CURLFORM_FILE, bestand,
                 CURLFORM_END);
   /*IO_Writef("\nfile: %s  + %d + %d", filenaamb, strlen(filenaamb), filenaamb[0]);*/
    /* Fill in the filename field */
    /*curl_formadd(&formpost,
                 &lastptr,
                 CURLFORM_COPYNAME, "filenaam",
                 CURLFORM_COPYCONTENTS, bestand,
                 CURLFORM_END); */
 
  for(; aanmeldenVars->fno==listfno; aanmeldenVars=aanmeldenVars->next->next) { /* gepaard! */
    curl_formadd(&formpost,
                 &lastptr,
                 CURLFORM_COPYNAME, aanmeldenVars->string,
                 CURLFORM_COPYCONTENTS, aanmeldenVars->next->string,
                 CURLFORM_END);
  }
 
    /*                extra toegevoegd                         */
  
    /* initalize custom header list (stating that Expect: 100-continue is not
	      wanted */
    /* what URL that receives this POST */
    curl_easy_setopt(curl, CURLOPT_URL, filenaamu); 
    curl_easy_setopt(curl, CURLOPT_HTTPPOST, formpost);
    res = curl_easy_perform(curl);
    if (res > 7) checkvoortgang(11, "geen verbinding!");
    
    /* always cleanup */
    /*htmlTimer("Upload klaar");*/
    curl_easy_cleanup(curl);
 
    /* then cleanup the formpost chain */
    curl_formfree(formpost);
    /* free slist */
	/*free(&chunk);*/
    if (res > 0) {
      fprintf(uit, "upload mislukt, returncode = %u.\nIs er wel verbinding?", res);
      fclose(uit);
      }
  } else {
    fprintf(uit, "Probleem met windows!");
    fclose(uit);
    return 1;
  }
  fclose(uit);
return 0;
}

/*####################################################################################*/
int fileupload(char *URL, char *filenaam, char *rapport, stringlist *aanmeldenVars)
{
  CURL *curl;
  CURLcode res;
  stringlist *varList;
  struct curl_httppost *formpost=NULL;
  struct curl_httppost *lastptr=NULL;
  /*char koekfile[] = "geen.txt";*/
  FILE *uit;
  if (first) {
    curl_global_init(CURL_GLOBAL_ALL);	/* --   winsock  -- */
    first = 0;
    }
  curl = curl_easy_init();		/* -- get handle -- */
  if (!(uit = fopen(rapport, "w"))) {
      curl_easy_cleanup(curl);
    return 1;
  };
  if (!curl) return 1;
  if (curl) {
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)uit);
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, TRUE);
    curl_easy_setopt(curl, CURLOPT_COOKIEFILE, koekfile);
    curl_easy_setopt(curl, CURLOPT_REFERER, referer);
    /*curl_easy_setopt(curl, CURLOPT_AUTOREFERER, TRUE);*/
    curl_easy_setopt(curl, CURLOPT_URL, URL); 
    for(varList = aanmeldenVars; varList->fno==listfno; varList=varList->next->next) {  /*gepaard!*/ 
    curl_formadd(&formpost,
                 &lastptr,
                 CURLFORM_COPYNAME, varList->string,
                 CURLFORM_COPYCONTENTS, varList->next->string,
                 CURLFORM_END);
    }
    curl_formadd(&formpost,
                 &lastptr,
                 CURLFORM_COPYNAME, "Bestand",
                 CURLFORM_FILE, filenaam,
                 CURLFORM_END);
    curl_easy_setopt(curl, CURLOPT_HTTPPOST, formpost);
    res = curl_easy_perform(curl);
    curl_formfree(formpost);
    formpost = NULL;
    lastptr = NULL;
    fclose(uit);
    curl_easy_cleanup(curl);
    if (!res) return res;
    /*if (curl != 0) return 1;*/
  }
  return 0;
}    

/*####################################################################################*/
int downloadData(char *URL, char *uitBestand, stringlist *aanmeldenVars, int *Resp, int MetProgress)
{
  CURLcode res;
  struct curl_httppost *formpost=NULL;
  struct curl_httppost *lastptr=NULL;
  FILE *uitF;
  CURL *curl;

  if (first) {
    curl_global_init(CURL_GLOBAL_ALL);	/* --   winsock  -- */
    first = 0;
    }
  curl = curl_easy_init();		/* -- get handle -- */
  if (!(uitF = fopen(uitBestand, "a+"))) { /* append !!!!   */
      curl_easy_cleanup(curl);
    return 1;
  };
  
  curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
  curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)uitF);
  curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, TRUE);
  curl_easy_setopt(curl, CURLOPT_REFERER, referer);
  if (MetProgress) {
    curl_easy_setopt(curl, CURLOPT_NOPROGRESS, 0);
    curl_easy_setopt(curl, CURLOPT_PROGRESSFUNCTION, pc);
  }
/*  varList = aanmeldenVars(&t1, &t2);*/

  for(; aanmeldenVars->fno==listfno; aanmeldenVars=aanmeldenVars->next->next) { /* gepaard! */
    curl_formadd(&formpost,
                 &lastptr,
                 CURLFORM_COPYNAME, aanmeldenVars->string,
                 CURLFORM_COPYCONTENTS, aanmeldenVars->next->string,
                 CURLFORM_END);
  }
  curl_easy_setopt(curl, CURLOPT_URL, URL);
  curl_easy_setopt(curl, CURLOPT_HTTPPOST, formpost);
  if (MetProgress) checkvoortgang(11, "contact zoeken met server!");
  res = curl_easy_perform(curl);
  curl_formfree(formpost);
  curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, Resp),
  curl_easy_cleanup(curl);
  fclose(uitF);
return res;
}



/* void     checkvoortgang(int, char *msg); */
/*int responseOK(char *Bestandsnaam, char *Zoektekst);*/


int uploadFTP(char *URL, char* pad, stringlist *bestanden, char *rapport, char *gebruiker,
         int compress)
  {
    CURL *curl;
    CURLcode res;
    /*FILE *ftpfile;*/
    FILE * hd_src ;
    FILE * uit;
    /*struct curl_slist *headerlist=NULL;*/

    char completeURL[500]; 
    char lokaalbest[300];
    char voortgang[50];
     /* In windows, this will init the winsock stuff */
  /*IO_Writef("\nURL: %s", URL);*/
    if (first) {
      curl_global_init(CURL_GLOBAL_ALL);	/* --   winsock  -- */
      first = 0;
      }
  
    /* get a curl handle */
    curl = curl_easy_init();
    if(curl) {
      /* enable uploading */
      curl_easy_setopt(curl, CURLOPT_UPLOAD, TRUE) ;
  
      /* specify target */
  

   if (!(uit = fopen(rapport, "w"))) {
      curl_easy_cleanup(curl);
    return 1;
  };
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)uit);
    curl_easy_setopt(curl, CURLOPT_READFUNCTION, read_callback);
 
      /* now specify which file to upload */
  
      /* and give the size of the upload (optional) */
      /*curl_easy_setopt(curl, CURLOPT_INFILESIZE_LARGE, file_info.st_size);*/

      curl_easy_setopt(curl, CURLOPT_NOPROGRESS, 0);
      curl_easy_setopt(curl, CURLOPT_PROGRESSFUNCTION, pc);
      curl_easy_setopt(curl, CURLOPT_USERPWD, gebruiker);

    for(; bestanden->fno==listfno; bestanden=bestanden->next) {
      strcpy(lokaalbest, pad);
      strcat(lokaalbest, bestanden->string);
      /*IO_Writef("\ngebruiker: %s", gebruiker);
      IO_Writef("\nbestand: %s\n", lokaalbest);*/ 
      strcpy(voortgang, "Bezig met ");
      strcat(voortgang, bestanden->string);
      strcat(voortgang, "... ");
      checkvoortgang(8, voortgang);
      if (!(hd_src = fopen(lokaalbest, "rb"))) {
        fclose(uit);
		curl_easy_cleanup(curl);
		IO_Writef("openen mislukt.\n");
		return 1;
	  };
      curl_easy_setopt(curl, CURLOPT_READDATA, hd_src);
      strcpy(completeURL, URL);
      strcat(completeURL, bestanden->string);
      /*IO_Writef("\nURL: %s\n", completeURL);*/ 
      curl_easy_setopt(curl,CURLOPT_URL, completeURL);
      res = curl_easy_perform(curl);
      /*IO_Writef("\nres: %u\n", res);*/ 
      fclose(hd_src); /* close the local file */
      if (res != 0) {
        fclose(uit);
        curl_easy_cleanup(curl);
        IO_Writef("curl return: %u", res);
        return res;
        }    
    }
       /* always cleanup */
      fclose(uit);
      curl_easy_cleanup(curl);
    }
  
    curl_global_cleanup();
    return 0;
  }

