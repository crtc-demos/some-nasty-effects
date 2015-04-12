/*
  expand_im.c
  Copyright (C) 1996 W.H. Scholten
  Deel van bbcim.
*/

void expand_im(char *disk, int expand) {
  FILE *fpdisk;
  unsigned char byte, bootoption;
  long i, length, expand_length;

  fpdisk=fopen(disk,"ab+");
  fseek(fpdisk,0,SEEK_END);
  length=ftell(fpdisk);
  expand_length=expand*256;

  if (expand_length>length) {
    byte=0;
    for (i=0; i<(expand_length-length); i++)
      fwrite(&byte,1,1,fpdisk);

    /* SECTORS ON DISK AANPASSEN */
    fseek(fpdisk,256L+6L,SEEK_SET);
    fread(&bootoption,1,1,fpdisk); /*rescue bootoption*/
    bootoption &=3<<4;

    byte=((expand >>8)&3)|bootoption;
    fseek(fpdisk,256L+6L,SEEK_SET);
    fwrite(&byte,1,1,fpdisk);
    byte=(expand & 0xFF);
    fwrite(&byte,1,1,fpdisk);
  }

  /* NETJES AFSLUITEN */
  fclose(fpdisk);
}
