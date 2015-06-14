static void
oswrch (char x)
{
  __asm__ __volatile__ ("jsr $ffee" : : "Aq" (x));
}

static void
setmode (char mode)
{
  oswrch (22);
  oswrch (mode);
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
  rowptr[byte] |= cols[x & 1][c];
}

static void
box (unsigned int xlhs, unsigned char ytop, unsigned char col_base)
{
  unsigned char col_plus = 0;
  unsigned char x, y;

  for (y = 0; y < 44; y++)
    {
      unsigned char yp = ytop + y;
      for (x = 0; x < 28; x++)
	{
	  unsigned int xp = xlhs + x;
	  if (x + y > 3 && (27 - x) + y > 3
	      && x + (43 - y) > 3 && (27 - x) + (43 - y) > 3)
	    putpixel (xp, yp, col_base + col_plus);
	  col_plus++;
	  if (col_plus > 2)
	    col_plus = 0;
	}
    }
}

int
main (void)
{
  unsigned int startblk, xblk, yblk;

  setmode (2);

  for (startblk = 0; startblk < 9; startblk++)
    {
      const char pos[9][2] = { { 4, 0 }, { 3, 0 }, { 2, 0 }, { 1, 0 }, { 0, 0 },
			       { 0, 1 }, { 0, 2 }, { 0, 3 }, { 0, 4 } };

      xblk = pos[startblk][0];
      yblk = pos[startblk][1];

      while (xblk < 5 && yblk < 5)
	{
	  unsigned char col_base = 1 + 3 * xblk, col_plus = 0;
          unsigned int xlhs = xblk * 33;
	  unsigned char ytop = yblk * 53;

	  box (xlhs, ytop, col_base);

	  xblk++;
	  yblk++;
	}
    }

  return 0;
}
