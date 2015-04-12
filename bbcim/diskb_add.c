/* diskb_add.c: VOEG BESTANDEN AAN DISKBEELD TOE
 * Copyright (C) W.H.Scholten 1996
 * part of bbcim.
 */

void add_to_image(char *disk, int options, int argc, char *argv[], int remove_file, int use_infofiles) {
  FILE *fpdisk, *fpinfo, *fpadd;
  unsigned char byte,H;
  char infofile[100];
  char bbcfile_naam[15];
  unsigned char afiles=0;
  int  addfiles;
  unsigned long loadaddress, execaddress;
  char read_line[100];
  int  l,i, len_s;
  long startsector, length;
  unsigned char eellddss, locked;
  unsigned int  sectorsondisk;

  fpdisk=fopen(disk,"rb+");

/*ADDING FILES WITHOUT COMMAND LINE LIST NOT IMPLEMENTED
//   if ((argc-options)<3)
//    {
//     #ifdef NL
//     printf("Naam van het toe te voegen archief bestand?");
//     #else
//     printf("Name of the archive file to be added?");
//     #endif
//     scanf("%50s", addfile);
//     } else {strcpy(addfile, argv[2+options]); options +=1;}
*/

  if ((argc-options)<3) {
    #ifdef NL
    printf("Geen lijst van toe te voegen bestanden meegegeven\n");
    #else
    printf("No archive file list given\n");
    #endif
    exit(1);
  }



  /* AANTAL BESTANDEN (afiles=8*echt aantal): */
  fseek(fpdisk,256+5L,SEEK_SET);
  fread(&afiles,1,1,fpdisk);

  /* NU BESTANDEN TOEVOEGEN: */
  for(addfiles=3; addfiles<argc; addfiles++) {
    if (afiles==31*8) {
      #ifdef NL
      printf("Er zijn al 31 bestanden op het diskbeeld\n\n");
      #else
      printf("There are already 31 files on the diskimage\n\n");
      #endif
      exit(1);
    }

    /* .inf bestanden overslaan: */
    l=strlen(argv[addfiles]);
    if (l>4)
      if (!strncmp(argv[addfiles]+l-4,".inf",4))
	continue;

    strcpy(infofile,argv[addfiles]);
    strcat(infofile,".inf");

    /* Archief bestand & bijbehorend info bestand moeten bestaan: */
    if (use_infofiles) {
      fpinfo=fopen(infofile, "r");
      if (fpinfo==NULL) {
        #ifdef NL
	printf("Info bestand %s kan niet geopend worden, sla over\n", infofile);
        #else
	printf("Info file %s cannot be opened, skipping file\n", infofile);
        #endif
	continue;
      }
    }

    fpadd=fopen(argv[addfiles], "rb");
    if (fpadd==NULL) {
      #ifdef NL
      printf("Archief bestand %s kan niet geopend worden\n", argv[addfiles]);
      #else
      printf("Archive file %s cannot be opened\n", argv[addfiles]);
      #endif
      fclose(fpinfo);
      continue;
    }



    /* UIT HET inf BESTAND DE BESTANDSNAAM HALEN EN DE ADRESSEN. */
    locked=0;
    if (use_infofiles) {
//      fscanf(fpinfo,"%12s %x %x", bbcfile_naam, (int*) &loadaddress, (int*) &execaddress);
#if 1
    fscanf(fpinfo,"%12s", bbcfile_naam);
    if (bbcfile_naam[1]!='.') {
      strncpy(bbcfile_naam, argv[addfiles], 12);
      fseek(fpinfo, 0L, SEEK_SET);
    }
    bbcfile_naam[9]=0;
    fscanf(fpinfo,"%lx %lx", &loadaddress, &execaddress);

#else
      fscanf(fpinfo,"%12s %lx %lx", bbcfile_naam, &loadaddress, &execaddress);
      if (bbcfile_naam[1]!='.') {
	strncpy(bbcfile_naam, argv[addfiles],12);
	fseek(fpinfo, 0L, SEEK_SET);
	fscanf(fpinfo," %lx %lx", &loadaddress, &execaddress);
      }
      bbcfile_naam[9]=0;
#endif

      fgets(read_line, 99, fpinfo); len_s=strlen(read_line);
      for (i=0; i<len_s; i++)
        if (read_line[i]=='L') {
	  locked=1;
	  break;
        }

/*      const int len_locked=6;
 *      for (int i=0; i<len_s-len_locked; i++)
 *        {if (!strncmp(read_line+i, "Locked", len_locked))
 *      NOTE: 'Locked' may be abbreviated to 'L': */

      fclose(fpinfo);
    } /* use_infofiles */
    else {
      loadaddress=0; execaddress=0;
      strncpy(bbcfile_naam, argv[addfiles],12);
      bbcfile_naam[9]=0;
    }

    /* Should check that second char is '.' else use $.name */
    if (bbcfile_naam[1]!='.') {
      for (i=9; i>=0; i--) bbcfile_naam[i+2]=bbcfile_naam[i];
      bbcfile_naam[0]='$'; bbcfile_naam[1]='.';
    }
    bbcfile_naam[9]=0;


    /* GET PREVIOUS STARTSECTOR AND FILE LENGTH */
    if (afiles) {
      fseek(fpdisk,256L+14L,SEEK_SET);
      fread(&eellddss,1,1,fpdisk);
      /* STARTSECTOR */
      fread(&byte,1,1,fpdisk);
      startsector=byte+(long)(eellddss & 0x3)*256L;

      /* FILELENGTE */
      fseek(fpdisk,256L+12L,SEEK_SET);
      fread(&byte,1,1,fpdisk);
      fread(&H,1,1,fpdisk);
      length=H*256L+byte+(eellddss & 0x30)*16L*256L;

      startsector +=(length +255)>>8;
    }
    else startsector=2;


    /* NU TESTEN OF BESTANDEN BINNEN SECTORS-ON-DISK BLIJFT: */
    fseek(fpdisk,256+6L,SEEK_SET);
    fread(&byte,1,1,fpdisk);
    H=byte & 3;
    fread(&byte,1,1,fpdisk);
    sectorsondisk=byte+H*256L;

    fseek(fpadd, 0L, SEEK_END);
    length=ftell(fpadd);

    if (startsector+((length+255)>>8)> sectorsondisk) {
      fclose(fpadd);
      #ifdef NL
      printf("Niet genoeg ruimte op het diskbeeld\n");
      #else
      printf("Not enough space on the diskimage\n");
      #endif
      break;
    }




 #ifdef NL
    printf("bestand %s toegevoegd\n", argv[addfiles]);
 #else
    printf("adding file %s\n", argv[addfiles]);
 #endif


/*    fseek(fpadd, 0L, SEEK_SET); */
/*    fseek(fpdisk,startsector*256L,SEEK_SET); */

/* COPY FILE: */
{long j;
    for(j=0; j<(((length+255) >>8)*256); j++) {
      fseek(fpadd, j, SEEK_SET);
      if (!fread(&byte,1,1,fpadd)) byte=0;

      /* ZONDER DE VOLGENDE SEEK KRIJG IK SOMS SEGMENTATIE FOUTEN */
      fseek(fpdisk,startsector*256L+j,SEEK_SET);
      fwrite(&byte,1,1,fpdisk);
    }
}

    fclose(fpadd);

    if (remove_file) {
      remove(argv[addfiles]);
      remove(infofile);
    }

    /* ADD FILENAME TO CATALOGUE */
    /* EERST DE CATALOGUS INGANGEN VERPLAATSEN (8 bytes opschuiven) */
{long j;
    for (j=255-8; j>7; j--) {
      fseek(fpdisk, j, SEEK_SET);
      fread(&byte,1,1,fpdisk);
      fseek(fpdisk, j+8, SEEK_SET);
      fwrite(&byte,1,1,fpdisk);
      fseek(fpdisk, j+256L, SEEK_SET);
      fread(&byte,1,1,fpdisk);
      fseek(fpdisk, j+8+256L, SEEK_SET);
      fwrite(&byte,1,1,fpdisk);
    }
}



    byte=bbcfile_naam[0]+(locked<<7);
    fseek(fpdisk,15L,SEEK_SET);
    fwrite(&byte,1,1,fpdisk);
    
    fseek(fpdisk,8L,SEEK_SET);
    for(i=0; i<7; i++) {
      byte=bbcfile_naam[i+2];
      if (byte==0)
	{byte=' '; bbcfile_naam[i+3]=0;}
      fwrite(&byte,1,1,fpdisk);
    }



    afiles +=8;
    fseek(fpdisk,256+5L,SEEK_SET);
    fwrite(&afiles,1,1,fpdisk);


    /* load/exec  adressen + lengte schrijven */
    fseek(fpdisk,256L+14L,SEEK_SET);
    eellddss=((startsector >>8) & 0x3)+((loadaddress >>14) &0xC);
    eellddss +=((execaddress >>10) &0xC0);
    eellddss +=((length >>12) &0x30);
    fwrite(&eellddss,1,1,fpdisk);

    /* STARTSECTOR */
    byte=startsector & 0xFF;
    fwrite(&byte,1,1,fpdisk);


    /* LOADADDRESS */
    fseek(fpdisk,256L+8L,0);
    byte=loadaddress & 0xFF;
    H=(loadaddress >>8) & 0xFF;
    fwrite(&byte,1,1,fpdisk);
    fwrite(&H,1,1,fpdisk);

    /* EXECADDRESS */
    byte=execaddress & 0xFF;
    H=(execaddress >>8) & 0xFF;
    fwrite(&byte,1,1,fpdisk);
    fwrite(&H,1,1,fpdisk);


    /* FILELENGTE */
    byte= length & 0xFF;
    H=(length >>8) & 0xFF;
    fwrite(&byte,1,1,fpdisk);
    fwrite(&H,1,1,fpdisk);

  }/* FOR */
  printf("\n");
fclose(fpdisk);
}

