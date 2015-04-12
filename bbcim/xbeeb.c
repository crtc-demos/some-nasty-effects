/*
  xbeeb.c
  Copyright (C) W.H.Scholten 1996
  Deel van bbcim.
  Laatste verandering: 30-6-96


  Alles werkt maar code is zeer lelijk (dubbel etc). VERBETER.
 */

#include "overschrijf.h"

void xbeeb(int argc, char *argv[], int options) {
  FILE *fparchief, *fpinfo, *fpcat, *fp_oude_cat;
  char infofile[100];

  const char* catalogus={"__CATALOG__"};
  const char* oude_catalogus={"__CATALOG__%"};


  int archief_bestand, l;
  char bbcfile_naam[15], xbeeb_naam[15];
  unsigned long loadaddress, execaddress;
  char read_line[50];
  char locked;
  char nieuwe_naam[55];
  int antwoord, len_s,i;
  long length;
  unsigned int crc;

/*CONVERSION WITHOUT COMMAND LINE LIST NOT IMPLEMENTED:
//   if ((argc-options)<3)
//    {
//     #ifdef NL
//     printf("Naam van het toe te voegen archief bestand?");
//     #else
//     printf("Name of the archive file to be added?");
//     #endif
//     scanf("%50s", addfile);
//     } else strcpy(addfile, argv[2+options]);
*/

  if ((argc-options)<2) {
     #ifdef NL
    printf("Geen lijst van archief bestanden meegegeven\n");
     #else
    printf("No archive file list given\n");
     #endif
    exit(1);
  }





  for(archief_bestand=2; archief_bestand<argc; archief_bestand++) {
    /* .inf bestanden overslaan: */
    /*int */l=strlen(argv[archief_bestand]);
    if (l>4) {
      if (!strncmp(argv[archief_bestand]+l-4,".inf",4))
	continue;
    }

    strcpy(infofile,argv[archief_bestand]);
    strcat(infofile,".inf");


    /* BESTAAT HET ARCHIEF BESTAND? */
    fparchief=fopen(argv[archief_bestand], "rb");
    if (fparchief==NULL) {
          #ifdef NL
      printf("Archief bestand %s kan niet geopend worden\n", argv[archief_bestand]);
          #else
      printf("Archive file %s cannot be opened\n", argv[archief_bestand]);
          #endif
      continue;
    }
    fclose(fparchief);

    /* BESTAAT HET .inf BESTAND? */
    fpinfo=fopen(infofile, "r");
    if (fpinfo==NULL) {
          #ifdef NL
      printf("Info bestand %s kan niet geopend worden, sla over\n", infofile);
          #else
      printf("Info file %s cannot be opened, skipping file\n", infofile);
          #endif
      continue;
    }

    /* UIT HET inf BESTAND DE BESTANDSNAAM HALEN EN DE ADRESSEN. */
#if 1
    fscanf(fpinfo,"%12s", bbcfile_naam);
    if (bbcfile_naam[1]!='.') {
      strncpy(bbcfile_naam, argv[archief_bestand], 12);
      fseek(fpinfo, 0L, SEEK_SET);
    }
    bbcfile_naam[9]=0;
    fscanf(fpinfo,"%lx %lx", &loadaddress, &execaddress);
#else
    fscanf(fpinfo,"%12s %lx %lx", bbcfile_naam, &loadaddress, &execaddress);
    bbcfile_naam[9]=0;

    if (bbcfile_naam[1]!='.') {
      strcpy(bbcfile_naam, argv[archief_bestand]);
      fseek(fpinfo, 0L, SEEK_SET);
      fscanf(fpinfo," %lx %lx", &loadaddress, &execaddress);
    }
#endif


    /* 'Locked' WORDT NU BEHANDELD: */
    fgets(read_line, 49, fpinfo);
    fclose(fpinfo);
 
    len_s=strlen(read_line);

    /*      NOTE: 'Locked' may be abbreviated to 'L':*/
    locked=' ';
    for (i=0; i<len_s; i++)
      if (read_line[i]=='L')
	{locked='L'; break;}



    /* HERNOEMEN ETC ALS NAAM AL BESTAAT (EN ALLEEN ALS NIEUWE NAAM ANDERS IS) */
    strcpy(nieuwe_naam, bbcfile_naam);

    if (strcmp(argv[archief_bestand], bbcfile_naam)) {

      if ((fparchief=fopen(bbcfile_naam, "rb"))!=NULL) {
	fclose(fparchief);
	antwoord=overschrijf_vraag(argv[archief_bestand], nieuwe_naam);
	/* IF NEW NAME HAS NO BBCDIR ASSUME $ */
	/* will work correctly if no new name is given (overwrite/skip)*/
	if (nieuwe_naam[1]!='.') {
	  for(i=12; i>1; i--)
	    nieuwe_naam[i]=nieuwe_naam[i-2];
	  nieuwe_naam[0]='$'; nieuwe_naam[1]='.';
	}
	nieuwe_naam[9]=0; /*maximale naam lengte*/

	switch(antwoord) {
           case OVERSCHRIJVEN:
                  rename(catalogus, oude_catalogus);

                  if (fp_oude_cat=fopen(oude_catalogus,"r")) { /*EXISTS ??*/
		    /* verwijder oude catalogus ingang: */
	             fpcat=fopen(catalogus,"w");

	             while (fgets(read_line, 49, fp_oude_cat)) {
		       sscanf(read_line, "%12s", xbeeb_naam);
		       if (!strcmp(bbcfile_naam, xbeeb_naam))
			 continue;
		       fputs(read_line, fpcat);
	             }
	          fclose(fpcat); fclose(fp_oude_cat);
	          } /* if (exists) */
                  break;

           case HERNOEM_NIEUW:
                  strcpy(bbcfile_naam, nieuwe_naam);
                  break;

           case HERNOEM_BESTAAND:
	         /* verander oude catalogus ingang: */
	          rename(catalogus, oude_catalogus);
	          if (fp_oude_cat=fopen(oude_catalogus,"r")) { /*exists ??? */
		    /* it might not if creating an xbeeb dir and files have xbeeb like names */
	             fpcat=fopen(catalogus,"w");
	             while(fgets(read_line, 49, fp_oude_cat)) {
	               sscanf(read_line, "%9s", xbeeb_naam);
	               if (!strcmp(bbcfile_naam, xbeeb_naam)) {
			 fprintf(fpcat, "%-9s", nieuwe_naam);
			 fputs(read_line+9, fpcat);
		       }
	               else
			 fputs(read_line, fpcat);
		     }
	             fclose(fpcat); fclose(fp_oude_cat);
		  }/*if (exists) */
                  rename(bbcfile_naam, nieuwe_naam);
                  break;

           case SLA_OVER:
                  continue;
	} /*switch*/
      } /*if*/





#ifdef NL
      printf("bestand %s hernoemd tot %s\n", argv[archief_bestand], bbcfile_naam);
#else
      printf("renamed file %s to %s\n", argv[archief_bestand], bbcfile_naam);
#endif

    } /*einde test of HERNOEM NAAM ANDERS IS.*/

    fparchief=fopen(argv[archief_bestand], "rb");
    fseek(fparchief, 0L, SEEK_END);
    length=ftell(fparchief);
    fclose(fparchief);

    rename(argv[archief_bestand], bbcfile_naam);

    remove(infofile);

/* if CRC was bad, should that one be put in the __CATALOG__ ??
   Probably. For now, I recalculate. FIX (if crc is bad say so in __CATALOG__)
 */

    crc=fcrc(bbcfile_naam);


    /* ADD FILENAME & INFORMATION TO __CATALOG__ */
    fpcat=fopen(catalogus,"a");
    fprintf(fpcat,"%-9s  %c  %6lX %6lX %6lX     CRC=%04X\n", bbcfile_naam, locked, loadaddress, execaddress, length, (uint) crc);
    fclose(fpcat);
  } /* FOR */
}
