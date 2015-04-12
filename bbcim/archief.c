/*archief.c
  Copyright (c) W.H.Scholten 1996
  Deel van bbcim >=0.80
  cleans up the info file and renames (should rename be optional?)
  last update 23/6/96.
*/

void cleanup(int argc, char *argv[], int options) {
  FILE *fpadd, *fpinfo;
  char infofile[100];
  unsigned long loadaddress, execaddress;
  int  addfiles,l, antwoord;
  char bbcfile_naam[13], unix_bbcfile_naam[13], nieuwe_info[50];
  char nieuwe_naam[55];
  char rest_string[100];
  int  found, len_s, i;
  unsigned int crc;

/*ARCHIVE FILE CONVERSION WITHOUT LIST NOT IMPLEMENTED:
//   if ((argc-options)<3)
//    {
//     #ifdef NL
//     printf("Naam van het om te zetten archief bestand?");
//     #else
//     printf("Name of the archive file to be ?");
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



  for(addfiles=2; addfiles<argc; addfiles++) {


    /*.inf bestanden overslaan:*/
    l=strlen(argv[addfiles]);
    if (l>4) {
      if (!strncmp(argv[addfiles]+l-4,".inf",4))
	continue;
    }

    strcpy(infofile,argv[addfiles]);
    strcat(infofile,".inf");



    /* BESTAAT HET INFO BESTAND? */
    fpinfo=fopen(infofile, "r");
    if (fpinfo==NULL) {
     #ifdef NL
      printf("Info bestand %s kan niet geopend worden, sla over\n", infofile);
     #else
      printf("Info file %s cannot be opened, skipping file\n", infofile);
     #endif
      continue;
    }


    /* BESTAAT HET ARCHIEF BESTAND? */
    fpadd=fopen(argv[addfiles], "rb");
    if (fpadd==NULL) {
          #ifdef NL
      printf("Archief bestand %s kan niet geopend worden\n", argv[addfiles]);
          #else
      printf("Archive file %s cannot be opened\n", argv[addfiles]);
          #endif
      continue;
    }
    fclose(fpadd);

    /* UIT HET inf BESTAND (EVT) DE BESTANDSNAAM HALEN. */
#if 0 
    fscanf(fpinfo,"%12s %lx %lx", bbcfile_naam, &loadaddress, &execaddress);

    fseek(fpinfo,0L,0);
    fgets(rest_string, 99, fpinfo);
    fclose(fpinfo);

    bbcfile_naam[9]=0;

    if (bbcfile_naam[1]!='.') {
      strcpy(bbcfile_naam, argv[addfiles]);
      fseek(fpinfo, 0L, SEEK_SET);
      fscanf(fpinfo,"%lx %lx", &loadaddress, &execaddress);
    }
#else
    fscanf(fpinfo,"%12s", bbcfile_naam);
    if (bbcfile_naam[1]!='.') {
      strncpy(bbcfile_naam, argv[addfiles],12);
      fseek(fpinfo, 0L, SEEK_SET);
    }
    bbcfile_naam[9]=0;
    fscanf(fpinfo,"%lx %lx", &loadaddress, &execaddress);

    fgets(rest_string, 99, fpinfo);
    fclose(fpinfo);
#endif

    reduceer_unix_naam(unix_bbcfile_naam, bbcfile_naam);

    /* HERNOEMEN ETC ALS NAAM AL BESTAAT */
    strcpy(nieuwe_naam, unix_bbcfile_naam);

    if (strcmp(argv[addfiles], unix_bbcfile_naam)) {
      if ((fpadd=fopen(unix_bbcfile_naam, "rb"))!=NULL) {
	fclose(fpadd);
	antwoord=overschrijf_vraag(argv[addfiles], nieuwe_naam);

	switch(antwoord) {

	case OVERSCHRIJVEN:
	  rename(argv[addfiles], unix_bbcfile_naam); break;

	case HERNOEM_NIEUW:
	  strcpy(unix_bbcfile_naam, nieuwe_naam);
	  rename(argv[addfiles], unix_bbcfile_naam); break;

	case HERNOEM_BESTAAND:
	  rename(unix_bbcfile_naam, nieuwe_naam);
	  strcat(nieuwe_naam, ".inf"); rename(infofile, nieuwe_naam);
	  rename(argv[addfiles], unix_bbcfile_naam); break;

	case SLA_OVER: continue;
	} /*switch*/

      }
      else
	rename(argv[addfiles], unix_bbcfile_naam);
    } /*if*/

    strcpy(nieuwe_info,unix_bbcfile_naam); strcat(nieuwe_info,".inf");



    found=0;
    len_s=strlen(rest_string);
    printf("Bestand %-9s (%-9s) CRC ", argv[addfiles], bbcfile_naam);
    for (i=0; i<len_s-4; i++) {
      if (!strncmp(rest_string+i,"CRC=",4)) {
	found=1;
	sscanf(rest_string+i+4, " %x", &crc);
	if (crc!=fcrc(unix_bbcfile_naam)) {
#ifdef NL
	  printf("fout, gevonden: %X", crc);
	  crc=fcrc(unix_bbcfile_naam);
	  printf(", berekend: %X", crc);
#else
	  printf("wrong, found: %X", crc);
	  crc=fcrc(unix_bbcfile_naam);
	  printf(", calculated: %X", crc);
#endif
	}

#ifdef NL
	else printf("in orde");
#else
        else printf("in orde");
#endif
        break;
      }
    }
    /*CRC toevoegen, alleen als er nog geen was: */
    if (!found) {
#ifdef NL
      printf("toegevoegd");
#else
      printf("added");
#endif
      crc=fcrc(unix_bbcfile_naam);
    }
    remove(infofile);

    fpinfo=fopen(nieuwe_info,"w");
    fprintf(fpinfo, "%-9s %6lX %6lX  CRC=%4X", bbcfile_naam, loadaddress, execaddress, (uint) crc);
    fclose(fpinfo);


    /* Did the name actually change? */
    if (strcmp(argv[addfiles], unix_bbcfile_naam)) {
         #ifdef NL
      printf(", hernoemd tot %s\n", unix_bbcfile_naam);
         #else
      printf(", renamed to %s\n", unix_bbcfile_naam);
         #endif
    }
    else printf("\n");
  } /*FOR*/
}




