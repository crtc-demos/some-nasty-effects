static char *screen = (char *) 0x3000;
static volatile char *const palette_reg = (char *) 0xfe21;

void oswrch (char x)
{
  __asm__ __volatile__ ("jsr $ffee" : : "Aq" (x));
}

void setmode (char mode)
{
  oswrch (22);
  oswrch (mode);
}

void writepalette (char physical, char logical)
{
  *palette_reg = (physical << 4) | (logical ^ 7);
}

static char pal[] = {
3	,
3	,
1	,
2	,
6	,
1	,
6	,
2	,
6	,
4	,
5	,
5	,
4	,
6	,
6	,
0


};

static char cbyte[] = {
255,
174,
12,
8,
0,
10,
15,
45,
60,
56,
248,
242,
216,
240,
245,
255

};

int main (void)
{
  char c;
  int i;
  
  setmode (1);
  
  for (c = 0; c < 16; c++)
    writepalette (c, pal[c]);
  
  for (c = 0; c < 16; c++)
    for (i = 0; i < 20480 / 16; i++)
      screen[c * 20480 / 16 + i] = cbyte[c];
  
  return 0;
}
