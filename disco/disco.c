#include <stdbool.h>

static void
oswrch (char x)
{
  __asm__ __volatile__ ("jsr $ffee" : : "Aq" (x));
}

static void
osbyte (char x)
{
  unsigned char dma, dmx, dmy;
  __asm__ __volatile__ ("jsr $fff4" : "=Aq" (dma), "=xq" (dmx), "=yq" (dmy)
				    : "Aq" (x));
}

static void
osword (unsigned char code, unsigned char *parameters)
{
  unsigned char addr_lo = ((unsigned short) parameters) & 0xff;
  unsigned char addr_hi = (((unsigned short) parameters) >> 8) & 0xff;
  unsigned char dma, dmx, dmy;
  __asm__ __volatile__ ("jsr $fff1"
			: "=Aq" (dma), "=xq" (dmx), "=yq" (dmy)
			: "Aq" (code), "xq" (addr_lo), "yq" (addr_hi)
			: "memory");
}

static void
oscli (unsigned char *cmd)
{
  unsigned char cmd_lo = ((unsigned short) cmd) & 0xff;
  unsigned char cmd_hi = (((unsigned short) cmd) >> 8) & 0xff;
  unsigned char dma, dmx, dmy;
  __asm__ __volatile__ ("jsr $fff7"
			: "=Aq" (dma), "=xq" (dmx), "=yq" (dmy)
			: "xq" (cmd_lo), "yq" (cmd_hi)
			: "memory");
}

static unsigned char osfile_params[18];

static unsigned char
osfile (unsigned char code)
{
  unsigned char addr_lo = ((unsigned short) osfile_params) & 0xff;
  unsigned char addr_hi = (((unsigned short) osfile_params) >> 8) & 0xff;
  unsigned char aout;
  __asm__ __volatile__ ("jsr $ffdd"
			: "=Aq" (aout)
			: "Aq" (code), "xq" (addr_lo), "yq" (addr_hi)
			: "memory");
  return aout;
}

static void
osfile_load (const char *filename, void *address)
{
  osfile_params[0] = ((unsigned short) filename) & 0xff;
  osfile_params[1] = (((unsigned short) filename) >> 8) & 0xff;
  osfile_params[2] = ((unsigned short) address) & 0xff;
  osfile_params[3] = (((unsigned short) address) >> 8) & 0xff;
  osfile_params[4] = osfile_params[5] = 0;
  osfile_params[6] = osfile_params[7] = osfile_params[8] = osfile_params[9] = 0;
  osfile (255);
}

static void
setmode (char mode)
{
  oswrch (22);
  oswrch (mode);
}

static void
setpalette (unsigned char physical, unsigned char logical)
{
  static unsigned char params[5] = {0, 0, 0, 0, 0};
  params[0] = physical;
  params[1] = logical;
  osword (0xc, &params[0]);
}

void **dispatch_table = (void **) 0x2000;

static void
start_effect (void)
{
  void (*fn) (void) = dispatch_table[0];
  fn ();
}

static void
finish_effect (void)
{
  void (*fn) (void) = dispatch_table[1];
  fn ();
}

static void
wait_for_vsync (void)
{
  void (*fn) (void) = dispatch_table[2];
  fn ();
}

#define PLAYER_CODE 0x2700

static void
load_tune (void)
{
  void (*fn) (void) = (void (*) (void)) PLAYER_CODE;
  fn ();
}

static void
poll_tune (void)
{
  void (*fn) (void) = (void (*) (void)) PLAYER_CODE + 3;
  fn ();
}

static void
eventv_tune (void)
{
  void (*fn) (void) = (void (*) (void)) PLAYER_CODE + 12;
  fn ();
}

#define LHS(C) ((((C) & 0x8) << 4) | (((C) & 0x4) << 3) \
		| (((C) & 0x2) << 2) | (((C) & 0x1) << 1))
#define RHS(C) (LHS(C) >> 1)

static const unsigned char cols[2][16] =
{
  { LHS (0),  LHS (1),  LHS (2),  LHS (3),
    LHS (4),  LHS (5),  LHS (6),  LHS (7),
    LHS (8),  LHS (9),  LHS (10), LHS (11),
    LHS (12), LHS (13), LHS (14), LHS (15) },

  { RHS (0),  RHS (1),  RHS (2),  RHS (3),
    RHS (4),  RHS (5),  RHS (6),  RHS (7),
    RHS (8),  RHS (9),  RHS (10), RHS (11),
    RHS (12), RHS (13), RHS (14), RHS (15) }
};

static void
putpixel (unsigned int x, unsigned char y, unsigned char c)
{
  unsigned char *screen = (unsigned char *) 0x3000;
  unsigned char *rowptr = &screen[(y >> 3) * 640 + (y & 7)];
  unsigned int byte = (x >> 1) << 3;
  /*rowptr[byte] &= ~cols[x & 1][15];*/
  rowptr[byte] |= cols[x & 1][c];
}

static void
box (unsigned int xlhs, unsigned char ytop, unsigned char col_base)
{
  unsigned char col_plus = 0;
  unsigned char x, y;

  for (y = 0; y < 4; y++)
    {
      unsigned char yp = ytop + y;
      for (x = 0; x < 28; x++)
	{
	  unsigned int xp = xlhs + x;
	  if (x + y > 3 && (27 - x) + y > 3)
	    putpixel (xp, yp, col_base + col_plus);
	  col_plus++;
	  if (col_plus > 2)
	    col_plus = 0;
	}
    }

  for (y = 4; y < 40; y++)
    {
      unsigned char yp = ytop + y;
      for (x = 0; x < 28; x++)
	{
	  unsigned int xp = xlhs + x;
	  putpixel (xp, yp, col_base + col_plus);
	  col_plus++;
	  if (col_plus > 2)
	    col_plus = 0;
	}
    }

  for (y = 40; y < 44; y++)
    {
      unsigned char yp = ytop + y;
      for (x = 0; x < 28; x++)
	{
	  unsigned int xp = xlhs + x;
	  if (x + (43 - y) > 3 && (27 - x) + (43 - y) > 3)
	    putpixel (xp, yp, col_base + col_plus);
	  col_plus++;
	  if (col_plus > 2)
	    col_plus = 0;
	}
    }
}

static unsigned char *const palette = (unsigned char *) 0x1f00;

static void
init_palette (void)
{
  unsigned char i, j;

  for (i = 0; i < 75; i++)
    palette[i] = 0x10;
}

static char
colour_array[] =
{
  0, 0, 0,
  0, 0, 1,
  0, 1, 1,
  1, 1, 1,
  1, 1, 3,
  1, 3, 3,
  3, 3, 3,
  3, 3, 2,
  3, 2, 2,
  2, 2, 2,
  2, 2, 6,
  2, 6, 6,
  6, 6, 6,
  6, 6, 4,
  6, 4, 4,
  4, 4, 4,
  4, 4, 5,
  4, 5, 5,
  5, 5, 5,
  5, 5, 0,
  5, 0, 0,
  0
};

#define ROW(A,B,C,D,E)		\
  (((E) ? (0x7 << 12) : 0)		\
   | ((D) ? (0x7 << 9) : 0)	\
   | ((C) ? (0x7 << 6) : 0)	\
   | ((B) ? (0x7 << 3) : 0)	\
   | ((A) ? 0x7 : 0))

static unsigned short letters[][5] =
{
  {
    ROW (0, 0, 0, 0, 0),
    ROW (0, 0, 1, 1, 0),
    ROW (0, 1, 0, 0, 0),
    ROW (0, 1, 1, 1, 0),
    ROW (0, 0, 0, 0, 0)
  },
  
  {
    ROW (0, 0, 0, 0, 0),
    ROW (0, 0, 1, 1, 0),
    ROW (0, 1, 0, 0, 0),
    ROW (0, 1, 0, 0, 0),
    ROW (0, 0, 0, 0, 0)
  },
  
  {
    ROW (0, 0, 0, 0, 0),
    ROW (0, 1, 1, 1, 0),
    ROW (0, 0, 1, 0, 0),
    ROW (0, 0, 1, 0, 0),
    ROW (0, 0, 0, 0, 0)
  },
};

static void
set_palette (int offset, int offset2, bool letter, char which_letter)
{
  unsigned char i, j;

  for (i = 0; i < 5; i++)
    {
      unsigned short bit = letters[which_letter][i];
      for (j = 0; j < 15; j++)
	{
	  char colour = colour_array[(((i * offset2) >> 6) + j + offset) & 63];

	  if (letter && (bit & 1))
	    colour = 7;

	  palette[j * 5 + i] = ((j + 1) << 4) | colour;
	  bit >>= 1;
	}
    }
}

int
main (void)
{
  unsigned int startblk, xblk, yblk;
  unsigned int i = 0, q = 0;
  unsigned int frameno = 0;

  load_tune ();

  setmode (2);
  
  osfile_load ("palswch\r", (void*) 0x2000);

  for (i = 1; i < 16; i++)
    setpalette (i, i & 1);

  for (yblk = 0; yblk < 5; yblk++)
    {
      unsigned char ytop = yblk * 53;

      for (xblk = 0; xblk < 5; xblk++)
	{
	  unsigned char col_base = 1 + 3 * xblk, col_plus = 0;
          unsigned int xlhs = xblk * 33;

	  box (xlhs, ytop, col_base);
	}
    }

  init_palette ();

  start_effect ();

  for (; frameno < 700; frameno++)
    {
      poll_tune ();

      if (frameno >= 150 && frameno < 200)
	set_palette (i, q, true, 0);
      else if (frameno >= 250 && frameno < 300)
        set_palette (i, q, true, 1);
      else if (frameno >= 350 && frameno < 400)
        set_palette (i, q, true, 2);
      else if (frameno >= 450 && frameno < 500)
        set_palette (i, q, true, 0);
      else
        set_palette (i, q, false, 0);

      i++;
      q += 7;
      
      wait_for_vsync ();
      frameno++;
    }

  finish_effect ();
  
  eventv_tune ();
  oscli ("run showimg\r");

  return 0;
}
