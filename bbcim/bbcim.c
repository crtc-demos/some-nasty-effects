/*
// bbcim.c (bbc diskbeeld manipulatie): SINGLE DENSITY, DD extensions and more.

// Copyright (c) W.H. Scholten 1996, 1997
 
// Permission to use, copy, modify, distribute, but NOT to sell this software
// and its documentation for any purpose is hereby granted without fee,
// provided that the above copyright notice appear in all copies and
// that both that copyright notice and this permission notice appear in
// supporting documentation, and that the name of the copyright holder
// not be used in advertising or publicity pertaining to distribution
// of the software without specific, written prior permission. The
// copyright holder makes no representations about the suitability of
// this software for any purpose. It is provided "as is" without express
// or implied warranty.
//
// THE COPYRIGHT HOLDER DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
// SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
// FITNESS, IN NO EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE FOR ANY
// SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
// RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
// CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
// CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
//
//
// Send comments and bug-reports to 
//
//    wouters@cistron.nl
//
// Send flames to /dev/null.
//
// It works under linux (1.2.13/GCC 2.7.0, 2.0.18/GCC 2.7.2).
*/

/* OVERVIEW OF CHANGES IN BBCIM.
   First bbcim version was 0.60 (I think), a combination of separate programs.
   Changes in 0.83 (from 0.70):
    +  xcrc implemented
    +  -ed may work on a mac (LET ME KNOW): uncomment #define MAC
    +  -e options now in any order
    +  -e, -x and -y: if a file exists, you can choose whether to overwrite or rename.
    c  seeks immediately before read/write in add-loop, otherwise segmentation faults sometimes (bug in GCC?).
    c  fixed small mistake in xcrc.

   Changes in 0.91 (from 0.83):
    +  'use' added.
    +  -s file overwrite choice.
    +  -e file {list}

    c  -x : rename file to itself fixed (no overwrite question)
    c  -xcrc: filenames < 7 char's now OK.
    c  -s : L before load address (as in xbeeb's __CATALOG__) is now acceptable.

   Changes in 0.92:
    +  -ab : adds bare files, otherwise info file is used by default.
    c : -e; b->s, info file is now added by default, b=bare.

   Changes in 0.93-beta (9-3-97):
    + code cleaned up with explicit type conversions
    + added code to replace characters in filenames (usually not needed in linux); e.g. '/' gives problems.
    + Help in english

   Changes in 0.94-beta (16-3-97):
    c w62 conversion now included in bbcim.


   Changes in 0.95-beta (30-6-97 - 4-7-97):
    c w62 conversion: if there are less than 32 files, only one diskimage is made.
    c disknames without a . didn't extract with -ed (as the dir name was equal to the diskimage name). Added a rename.
    c cleaned up 'mkdir' selection. You now have 3 choices.
    + Config additions: bbcim now compiles on other OS'es without changes.
    c bugfix: -s# works again (no idea when it got broken).


   Bugs and 'features':
    - several files in a diskimage can have the same name (can be useful)
    (i.e. -a doesn't overwrite)
    - -e: name checking in diskimage is case dependant.

   If in doubt: it's not a bug, it is a feature!
 */


#define VERSION "0.95-beta 5"



/*************** CONFIGURATION *********************/
/* Comment out for english messages: */
#define NL

#define UNIX_MKDIR

/* Make directory: UNIX_MKDIR, MAC_MKDIR, or SYSTEM_MKDIR (using system("mkdir.."); ) */


/* #define NO_COMMAND_LINE */
/* If you can't give parameters on the command line use this (useless?) */

#define REPLACE_FEW

/* Characters to be replaced in filenames:
   Most filesystems can't handle all characters from bbc filenames.
   Add whatever you need for your system.
   I've put the minimum for linux in REPLACE_FEW.
 */

#ifdef REPLACE_FEW
char filenamechar_bad[] = {'/'};
char filenamechar_replace[] = {'_'};
#endif

#ifdef REPLACE_MANY
char filenamechar_bad[]     = { '\\', '/', ':', '*', '?', '\"', '<', '>', '|', '.' };
char filenamechar_replace[] = { '_' , '_', '_', '_', '_', '_' , '_', '_', '_', '_' };
#endif

/************ END OF CONFIGURATION ***********************/





#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef MAC_MKDIR
 #include <unix.h>
#endif

#ifdef UNIX_MKDIR
 #include <sys/types.h>
 #include <fcntl.h>
 #include <unistd.h>
#endif

#ifdef SYSTEM_MKDIR
#endif

/* Should use this in various places:
#include <sys/stat.h>
struct stat file_stat;
if (stat(filename, file_stat)<0) perror("can't open file\n");
*/


/* Byte offsets in diskimage */
#define DFStitle0_7    0
#define DFStitle8_11   256
#define DFS accessct   256+0x04
#define DFSentries     256+5
#define DFSbootsec     256+6
#define DFSsectondiskL 256+0x07

/*next offsets +n*8 for each file */
#define DFSfilename 8
#define DFSdirectory 0x0f
#define DFSloadaddrL 256+8
#define DFSloadaddrM 256+9
#define DFSexecaddrL 256+0x0a
#define DFSexecaddrM 256+0x0b
#define DFSlengthL 256+0x0c
#define DFSlengthM 256+0x0d
#define DFSeellddss 256+0x0e
#define DFSstartsecL 256+0x0f



typedef unsigned char byt;
typedef unsigned int  uint;


/* NB invoer en bbcnaam mogen niet zelfde string zijn */
/*
void invoer_naar_bbcnaam(char *invoer, char *bbcnaam) {
  if (invoer[1]=='.')
     strncpy(bbcnaam, invoer,9);
  else {
     bbcnaam[0]='$'; bbcnaam[1]='.'; strncpy(bbcnaam+2, invoer, 7);
  }
  bbcnaam[9]=0;
}
*/

void reduceer_unix_naam(char *unixnaam, char *bbcnaam) {
  if (bbcnaam[0]=='$')
    strcpy(unixnaam,bbcnaam+2);
  else {
    unixnaam[0]=bbcnaam[0];
    strcpy(unixnaam+1,bbcnaam+2);
  }
}



#include "overschrijf.c"

#include "nieuw_diskb.c"
#include "diskb_add.c"
#include "diskb_del.c"
#include "expand_im.c"
#include "minimaliseer.c"

#include "intersplits.c"
#include "ddos2dfs.c"
#include "w62.c"

#include "splits_cat.c"
#include "bbc_crc.c"
#include "xbeeb.c"
#include "archief.c"


int  filenamechar_no=sizeof(filenamechar_bad);	/* number of char's to be checked for replacement */

void use() {
#ifdef NL
printf("Opties: -e[s/d,#,b,r] -a[b,r] -d -c -crc -min -40/80/max\n" \
       "        -y -icrc -s[#]\n" \
       "        -x -xcrc\n" \
       "        -splitds -interss -ddos2dfs -w62dfs\n" \
       "        -V -H\n\n");
#else
printf("Options: -e[s/d,#,b,r] -a[b,r] -d -c -crc -min -40/80/max\n" \
       "        -y -icrc -s[#]\n" \
       "        -x -xcrc\n" \
       "        -splitds -interss -ddos2dfs -w62dfs\n" \
       "        -V -H\n\n");
#endif
}



int main(int argc, char *argv[]) {
  FILE *fpdisk, *fpcat, *fpextract, *fpinfo;
  char disk[50], catalogus[50], extract[50], info_naam[50], basisnaam[50];
  char bbcdisk_naam[15], bbcfile_naam[15];
  char unix_bbcfile_naam[15];
  char testnaam[15];
  char optstring[20];

  unsigned char byte,H;

  long filesizesum=0;

  int extr=0, extr_dir=0, cat=0, expand=0, min=0, bcrc=0, short_name=0;
  int add=0, info=0 /* don't use info files */, no_bbc_dir=0, remove_file=0, remove_disk=0;
  int extr_list=0;

  int bad_option=1;
  int options=1;

  int i, j, rfiles, bootoption, antwoord, k;
  unsigned char files=0;
  unsigned int sectorsondisk;
  long file;
  int locked;
  unsigned char eellddss; 

  long startsector;
  unsigned long loadaddress, execaddress, length;

  unsigned int crc;

#ifdef UNIX_MKDIR
  mode_t dir_mode;
#endif

#if defined(MAC_MKDIR) || defined(SYSTEM_MKDIR)
  char actie[60];
#endif

#ifdef NO_COMMAND_LINE
  argc=1;
#endif


  if (argc < 2) {
    #ifdef NL
    printf("Welke optie?");
    #else
    printf("Which option?");
    #endif
    scanf("%20s", optstring); options=0;
  }
  else strcpy(optstring, argv[1]);



/* 1e SELECTIE */
  if (!strcmp(optstring,"-V")){
    printf(" bbcim %s (c) WHS 1996,1997\n\n",VERSION); exit(0);
  }

  if (!strcmp(optstring,"-H")) {
#ifdef NL
    printf("Commando overzicht\n" \
    " -c diskbeeld : toon bestanden in diskbeeld\n" \

    " -e[s/d,#,b,r] diskbeeld [{lijst}]: bestanden uit diskbeeld halen\n" \
    " -a[b,r] diskbeeld {lijst} : bestanden in diskbeeld plaatsen\n" \
    " -d diskbeeld {lijst} : verwijder bestanden uit diskbeeld\n" \
    " -40/-80/-max diskbeeld : grootte van diskbeeld wijzigen\n" \
    " -min diskbeeld : diskbeeld minimaliseren\n" \
    " -crc diskbeeld : bereken CRC's van bestanden in diskbeeld\n" \

    " -y {lijst} : archief bestanden bijwerken\n" \
    " -s[#] bestand : splits tekst bestand in .inf bestanden\n\n" \
    " -x {lijst} : archief bestanden omzetten naar xbeeb formaat\n" \

    " -icrc {lijst} : controleer CRC's van archief bestanden\n"\
    " -xcrc : controleer CRC's in xbeeb dir (de huidige dir)\n"\

    " -interss (sd/dd) B0 B1 B2 : enkelzijdig naar dubbelzijdig diskbeeld\n"\
    " -splitds (sd/dd) B : dubbelzijdig naar enkelzijdig diskbeeld\n"\

    " -ddos2dfs ddosbeeld : splits enkelzijdig DDOS diskbeeld in DFS diskbeelden\n"\
    " -w62dfs w62beeld : splits een watford 62 bestanden diskbeeld in 2 DFS diskbeelden\n"\

    " -V : toon versie\n\n");
#else
    printf("Command overview\n" \
    " -c diskimage : show files in diskimage\n" \

    " -e[s/d,#,b,r] diskimage [{list}]: extract files from diskimage\n" \
    " -a[b,r] diskimage {list} : put files in diskimage\n" \
    " -d diskimage {list} : remove files from diskimage\n" \
    " -40/-80/-max diskimage : change size of diskimage\n" \
    " -min diskimage : minimize diskimage\n" \
    " -crc diskimage : calculate CRC's of files in diskimage\n" \

    " -y {list} : cleanup archive file information\n" \
    " -s[#] file : split a text file in .inf files\n\n" \
    " -x {list} : convert archive files to xbeeb format\n" \

    " -icrc {list} : check CRC's of archive files\n"\
    " -xcrc : check CRC's in xbeeb dir (the current dir)\n"\

    " -interss (sd/dd) B0 B1 B2 : single sided to double sided diskimage conversion\n"\
    " -splitds (sd/dd) B : double sided to single sided diskimage conversion\n"\

    " -ddos2dfs ddosbeeld : split single sided DDOS diskimage in DFS diskimages\n"\
    " -w62dfs w62image : split a watford 62 file diskimage in 2 DFS diskimages\n"\

    " -V : show version\n\n");
#endif
    exit(0);
  }

  if (!strcmp(optstring,"-interss")) {interss(argc, argv, options); exit(0);}
  if (!strcmp(optstring,"-splitds")) {splitds(argc, argv, options); exit(0);}
  if (!strcmp(optstring,"-ddos2dfs")) {ddos2dfs(argc, argv, options); exit(0);}
  if (!strcmp(optstring,"-w62dfs")) {w62dfs(argc, argv, options); exit(0);}
  if (!strcmp(optstring,"-d")) {del_from_im(argc, argv, options); exit(0);}
  if (!strncmp(optstring,"-s",2)) {split_cat(argc, argv, options, optstring); exit(0);}
  if (!strcmp(optstring,"-x")) {xbeeb(argc, argv, options); exit(0);}
  if (!strcmp(optstring,"-y")) {cleanup(argc, argv, options); exit(0);}
  if (!strcmp(optstring,"-icrc")) {icrc(argc, argv, options); exit(0);}
  if (!strcmp(optstring,"-xcrc")) {xcrc(argc, argv, options); exit(0);}




  /* BEKIJK NU DE RESTERENDE OPTIES */
  if (!strcmp(optstring,"-c"))	 {cat=1; bad_option=0;}

  if (!strncmp(optstring,"-e",2)) {
    extr=1; bad_option=0; info=1;
    for (i=2; i<strlen(optstring); i++) {
      switch (optstring[i]) {
      case 'd' : extr_dir=1; break;
      case 's' : short_name=1; break;
      case 'b' : info=0; break;
      case '#' : no_bbc_dir=1; break;
      case 'r' : remove_disk=1; break;
      default : bad_option=1;
      }
      if (short_name & extr_dir) {
        #ifdef NL
	fprintf(stderr, "sd = slechte optie\n");
        #else
        fprintf(stderr, "sd = bad option\n");
        #endif
        exit(1);
      }
    } /*for*/
  }


  if (!strcmp(optstring,"-crc")) {bcrc=1; cat=1; bad_option=0;}
  if (!strcmp(optstring,"-40"))  {expand=40*10; bad_option=0;}
  if (!strcmp(optstring,"-80"))  {expand=80*10; bad_option=0;}
  if (!strcmp(optstring,"-max")) {expand=1023;  bad_option=0;}
  if (!strcmp(optstring,"-min")) {min=1; 	bad_option=0;}

  if (!strncmp(optstring,"-a", 2)) {
    add=1; bad_option=0; info=1;
    for (i=2; i<strlen(optstring); i++) {
      switch (optstring[i]) {
      case 'r' : remove_file=1; break;
      case 'b' : info=0; break;
      default  : bad_option=1;
      }
    }
  }


  if (!strcmp(optstring,"-new")) {bad_option=0;}

  /* EINDE OPTIES */

  if (bad_option) {
    #ifdef NL
    printf("Slechte optie\n");
    #else
    printf("Bad option\n");
    #endif
    use();
    exit(1);
  }

  if ((argc-options)<2) {
    #ifdef NL
    printf("Naam van het bbc diskbeeld?");
    #else
    printf("Name of the bbc disk image?");
    #endif
    scanf("%50s", disk);
  } else strcpy(disk, argv[1+options]);


  /* NU UITVOEREN VAN DE OPTIE:*/

  /* MAKE EMPTY DISKIMAGE.................. */
  if (!strcmp(optstring,"-new")) {
    new_diskim(disk, 800);
    exit(0);
  }


  strcpy(basisnaam, disk);
  basisnaam[strcspn(disk,".")]=0;
  strcpy(catalogus, basisnaam);
  strcat(catalogus,".cat");
 


  fpdisk=fopen(disk,"rb");



  /* ADD FILES TO DISKIMAGE...............
   ALLEEN VOOR ADD HOEFT HET DISKBEELD NOG NIET TE BESTAAN */
  if (add) {
    if (fpdisk==NULL) {
      new_diskim(disk, 800); /*DEFAULT size=80 track*/
      #ifdef NL
      printf("Nieuw diskbeeld gemaakt\n");
      #else
      printf("New diskimage made\n");
      #endif
    }
    else fclose(fpdisk);

    add_to_image(disk, options, argc,  argv, remove_file, info);
    exit(0);
  }
  /* EINDE ADDFILES.............. */



  /* VOOR ANDERE OPTIES MOET HET DISKBEELD BESTAAN */
  if (fpdisk==NULL) {
    #ifdef NL
    printf("Bestand %s is niet te openen\n\n",disk);
    #else
    printf("File %s cannot be opened\n\n",disk);
    #endif
    exit(1);
  }

  if (extr_dir) {
    rename_sequential(basisnaam); /* in case a file (dir too?)  already exists named basis_naam */
  #ifdef MAC_MKDIR
    strcpy(actie, ":"); /*MIGHT NEED TO BE "volume:" where volume is ? */
    strcat(actie, basisnaam);
    mkdir(actie,0);
  #endif
  #ifdef UNIX_MKDIR
    dir_mode=0777; /* CHANGE */
    mkdir(basisnaam, dir_mode);
  #endif
  #ifdef SYSTEM_MKDIR
    sprintf(actie, "mkdir %s", basisnaam);
    system(actie);
  #endif
  }

#ifdef DEBUG
  if (no_bbc_dir) {
   #ifdef NL
    printf("geen bbc dir\n");
   #else
    printf("no bbcdir\n");
   #endif
  }
#endif


  /* EXPAND diskbeeld ................. */
  if (expand) {
    fclose(fpdisk);
    expand_im(disk,expand);
    exit(0);
  }

 


  /* CRC, CAT, EXTRACT, MIN ................*/

  /* Doe alleen iets als het diskbeeld meer dan 2 sectoren bevat. */
  fseek(fpdisk,0L, SEEK_END);
  if (ftell(fpdisk)<512) {
    #ifdef NL
    printf("niets op het diskbeeld\n\n");
    #else
    printf("nothing on the diskimage\n\n");
    #endif

    exit(1);
  }

  /* MINIMALISEER */
  if (min) {
    fclose(fpdisk); min_diskim(disk);
    exit(0);
  }


  /* DISKNAAM BEPALEN (voor cat, crc, extract) */
  fseek(fpdisk,0L,SEEK_SET);
  for (i=0; i<8; i++) {
    fread (&byte,1,1,fpdisk);
    bbcdisk_naam[i]=byte;
    if (byte==0) break;
  }
  fseek(fpdisk,256L,SEEK_SET);
  for (i=0; i<4; i++) {
    fread (&byte,1,1,fpdisk);
    bbcdisk_naam[i+8]=byte;
    if (byte==0) break;
  }






  fseek(fpdisk,256+5L,SEEK_SET);
  fread(&files,1,1,fpdisk);


  /* SANITY CHECK ON DISKIMAGE: */
  if (files % 8) {
   #ifdef NL
    printf("corrupt diskbeeld (bestand-aantal byte)\n");
   #else
    printf("bad diskimage (fileno byte)\n");
   #endif
    exit(1);
  }


  rfiles=files/8;
  extr_list=(extr && (argc-options>2));


  fread(&byte,1,1,fpdisk);
  bootoption=byte >> 4;
  H=byte & 3;
  fread(&byte,1,1,fpdisk);
  sectorsondisk=byte+H*256L;


  if (extr && !extr_list) fpcat=fopen(catalogus,"w"); else fpcat=stdout;

  /*if (extr_list) fpcat=NULL;*/

  if (extr_list) goto skip_diskinfo;

 #ifdef NL
  fprintf(fpcat,"\nDiskette :%s",bbcdisk_naam);
  if (strlen(bbcdisk_naam)==0) fprintf(fpcat, "(geen naam)");
  fprintf(fpcat,"\n%d sectoren op de diskette\n",sectorsondisk);
 #else
  fprintf(fpcat,"\nDisk :%s",bbcdisk_naam);
  if (strlen(bbcdisk_naam)==0) fprintf(fpcat, "(no name)");
  fprintf(fpcat,"\n%d sectors on disk\n",sectorsondisk);
 #endif

 #ifdef NL
  fprintf(fpcat, "bootoptie: ");
 #else
  fprintf(fpcat, "bootoption: ");
 #endif

  switch(bootoption) {
  case 0 :
   #ifdef NL
    fprintf(fpcat, "geen");
   #else
    fprintf(fpcat, "none");
   #endif
    break;
  case 1 : fprintf(fpcat, "*LOAD !BOOT");break;
  case 2 : fprintf(fpcat, "*RUN !BOOT");break;
  case 3 : fprintf(fpcat, "*EXEC !BOOT");
  }
  fprintf(fpcat,"\n");

 #ifdef NL
  fprintf(fpcat, "Bestand    Laad  Start  Lengte Toegang startsector\n");
 #else
  fprintf(fpcat, "File       Load   Exec  Length Access  startsector\n");
 #endif


skip_diskinfo:

  /*Hoofdlus: alle bestanden opzoeken*/
  if (files>0) {

    int extr_list_no=0;

    for(file=0;file<files; file +=8) {

      strcpy(extract, basisnaam);
      if (extr_dir) strcat(extract,"/"); else strcat(extract,".");

      if (short_name) extract[0]=0;

      fseek(fpdisk,file+15L,SEEK_SET);
      fread(&byte,1,1,fpdisk);
      locked=byte >>7;
      bbcfile_naam[0]=(byte & 0x7F);
      bbcfile_naam[1]='.';
    

      fseek(fpdisk,file+8L,SEEK_SET);
/*    for(i=0; i<7; i++) {
        fread(&byte,1,1,fpdisk);
        bbcfile_naam[i+2]=byte;
      }
 */
      fread(bbcfile_naam+2,1,7,fpdisk);
      bbcfile_naam[9]=0;


      /* spaties aan het einde van bbc naam weghalen: geeft anders bestands namen
	 met spaties (in linux). */
      for (i=8;i>0 && (bbcfile_naam[i]==' '); i--);
      bbcfile_naam[i+1]=0;

#if DEBUG
      printf(bbcfile_naam);
#endif

      /* IF FILE LIST GIVEN, SEE IF NAME IS IN DISKIMAGE */
      if (extr_list) {
	int found=0;
	for (i=options+2; i<argc;i++) {
	  if (argv[i][1]=='.')
	    strncpy(testnaam, argv[i],9);
	  else {
	    testnaam[0]='$'; testnaam[1]='.';
	    strncpy(testnaam+2, argv[i],7);
          }
	  testnaam[9]=0;
	  if (!strcmp(bbcfile_naam, testnaam)) found=1;
	}
	if (!found) continue;
	extr_list_no++;
      }








      /* load/exec  adressen + lengte bepalen */
      fseek(fpdisk,256+file+14,SEEK_SET);

      fread(&eellddss,1,1,fpdisk);

      /* STARTSECTOR */
      fread(&byte,1,1,fpdisk);
      startsector=byte+(eellddss & 3)*256L;

      /* SANITY CHECK ON DISKIMAGE? (startsec>1 BUT DISALLOWS VIEWING DOS CAT OF
	 VOL 0A WITH STANDARD DFS ROUTINES. */


      /* LOADADDRESS */
      fseek(fpdisk,file+256L+8L,0);
      fread(&byte,1,1,fpdisk);
      fread(&H,1,1,fpdisk);
      loadaddress=H*256L+byte+(eellddss & 0x0C)*16384L;
      if (loadaddress & 0x30000) loadaddress |=0xFF0000;

      /* EXECADDRESS */
      fread(&byte,1,1,fpdisk);
      fread(&H,1,1,fpdisk);
      execaddress=H*256L+byte+(eellddss & 0xC0)*4L*256L;
      if (execaddress & 0x30000) execaddress |=0xFF0000;

      /* FILELENGTE */
      fread(&byte,1,1,fpdisk);
      fread(&H,1,1,fpdisk);
      length=H*256L+byte+(eellddss & 0x30)*16L*256L;

      filesizesum +=length;


      /* UITVOER VAN DEZE GEGEVENS NAAR CATALOGUS */
      if (!extr_list) {
	fprintf(fpcat,"%-9s %6lX %6lX %6lX", bbcfile_naam, loadaddress, execaddress, length);
	if (locked) fprintf(fpcat," Locked");
	else        fprintf(fpcat,"       "); /*VOOR CRC UITLIJNEN*/
      }

      strcpy(unix_bbcfile_naam, bbcfile_naam);

      /* Name check: replace certain characters in filenames (most filesystems can't
	 handle all characters from bbc filenames) */
      for (i=0; i<filenamechar_no; i++) {
	for (j=0; j<strlen(unix_bbcfile_naam); j++) {
	  if (unix_bbcfile_naam[j]==filenamechar_bad[i])
	    unix_bbcfile_naam[j]=filenamechar_replace[i];
	}
      }



      if (no_bbc_dir){
/*      if (bbcfile_naam[0]=='$') strcat(extract, unix_bbcfile_naam+2);
       else {
         bbcfile_naam[1]=bbcfile_naam[0]; strcat(extract, unix_bbcfile_naam+1);
       }
*/
        reduceer_unix_naam(extract+strlen(extract), unix_bbcfile_naam);
      }
      else strcat(extract, unix_bbcfile_naam);

      if (extr) {

	/* NAME CHECK: does it already exist? */
	char nieuwe_naam[55];

	strcpy(nieuwe_naam, extract);
	if ((fpextract=fopen(extract, "rb"))!=NULL) {
	  fclose(fpextract);
	  antwoord=overschrijf_vraag(extract, nieuwe_naam);

	  switch(antwoord) {
	  case OVERSCHRIJVEN:
	    break;
	  case HERNOEM_NIEUW:
	    strcpy(extract, nieuwe_naam); break;
	  case HERNOEM_BESTAAND:
	    rename(extract, nieuwe_naam);
	    /* altijd ook info bestand hernoemen: */
	    strcpy(info_naam, extract); strcat(info_naam, ".inf");
	    strcat(nieuwe_naam, ".inf"); rename(info_naam, nieuwe_naam);
	    break;
	  case SLA_OVER:
	    continue; /*to for*/
	  } /*switch */
	}
	/* END NAME CHECK */


	fpextract=fopen(extract, "wb");

	fseek(fpdisk,startsector*256, SEEK_SET);
	for (i=0; i<length; i++) {
          fread(&byte,1,1,fpdisk);
          if (feof(fpdisk)) {
            #ifdef NL
	    printf("onverwacht einde van het diskbeeld" \
               " bij bestand %s\n\n", bbcfile_naam);
            #else
	    printf("unexpected end of the diskimage" \
               " at file %s\n\n", bbcfile_naam);
            #endif
            exit(1);
	  }
	  fwrite(&byte,1,1,fpextract);
        }
        fclose(fpextract);
      }


      if (info) {
	strcpy(info_naam, extract);
	strcat(info_naam, ".inf");

	fpinfo=fopen(info_naam, "w");

	/* Evt volgende regel weg. (FILENAAM, bv $.ELITE) */
	fprintf(fpinfo, "%-9s", bbcfile_naam);

	/* I no longer include the (superfluous) file length in the INFO file. */
	fprintf(fpinfo," %6lX %6lX", loadaddress, execaddress);

	if (locked) fprintf(fpinfo," Locked");

      } /*N.B. info bestand wordt in crc deel gesloten*/


      crc=0;
      if (bcrc || info) {
	fseek(fpdisk,startsector*256, SEEK_SET);
	for (i=0; i<length; i++) {
	  fread(&byte,1,1,fpdisk);
	  if (feof(fpdisk)) {
             #ifdef NL
	    printf("onverwacht einde van het diskbeeld" \
               " bij bestand %s\n\n", bbcfile_naam);
             #else
	    printf("unexpected end of the diskimage" \
               " at file %s\n\n", bbcfile_naam);
             #endif
	    exit(1);
	  }

	  crc ^=(byte << 8);
	  for(k=0;k<8;k++) {
	    if (crc & 32768)
	      crc=(((crc ^ 0x0810) & 32767) << 1)+1;
	    else
	      crc =crc <<1;
	  }
	}

	if (info) {
	  fprintf(fpinfo, " CRC=%04X", crc); fclose(fpinfo);
	}

      }

      if (extr_list)
	printf(" %s\n",extract);
      else
	fprintf(fpcat,"   %4d", (int) startsector);

      if (bcrc)
	fprintf(fpcat, "       CRC= %04X\n", crc);
      else
	fprintf(fpcat, "\n");
    }

    /* SHOW NO OF (EXTRACTED) FILES */
    if (extr_list){
      rfiles=extr_list_no;
#ifdef NL
      printf("aantal bestanden: %d\n",rfiles);
    }
    fprintf(fpcat,"\n%d bestand%s\n", rfiles, (rfiles !=1) ? "en" : "");
 #else
      printf("number of files: %d\n",rfiles);
    }
    fprintf(fpcat,"\n%d file%s\n", rfiles, (rfiles !=1) ? "s" : "");
 #endif


    fclose(fpdisk);
    if (remove_disk && !extr_list) remove(disk);
}
#ifdef NL
fprintf(fpcat,"Totaal %ld bytes\n", filesizesum);
 #else
fprintf(fpcat,"Total %ld bytes\n",  filesizesum);
 #endif


return 0;
}
