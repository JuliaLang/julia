/* Benchmark implementing the board logic for the game of go and
 * exercising it by playing random games. Derived from
 * http://www.lysator.liu.se/~gunnar/gtp/brown-1.0.tar.gz
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_BOARD 23

#define EMPTY 0
#define WHITE 1
#define BLACK 2

/* Used in the final_status[] array. */
#define DEAD 0
#define ALIVE 1
#define SEKI 2
#define WHITE_TERRITORY 3
#define BLACK_TERRITORY 4
#define UNKNOWN 5

/* Macro to find the opposite color. */
#define OTHER_COLOR(color) (WHITE + BLACK - (color))

static int board_size = 9;
static float komi = 0.0;

/* Board represented by a 1D array. The first board_size*board_size
 * elements are used. Vertices are indexed row by row, starting with 0
 * in the upper left corner.
 */
static int board[MAX_BOARD * MAX_BOARD];

/* Stones are linked together in a circular list for each string. */
static int next_stone[MAX_BOARD * MAX_BOARD];

/* Storage for final status computations. */
static int final_status[MAX_BOARD * MAX_BOARD];

/* Point which would be an illegal ko recapture. */
static int ko_i, ko_j;

/* Offsets for the four directly adjacent neighbors. Used for looping. */
static int deltai[4] = {-1, 1, 0, 0};
static int deltaj[4] = {0, 0, -1, 1};

/* Macros to convert between 1D and 2D coordinates. The 2D coordinate
 * (i, j) points to row i and column j, starting with (0,0) in the
 * upper left corner.
 */
#define POS(i, j) ((i) * board_size + (j))
#define I(pos) ((pos) / board_size)
#define J(pos) ((pos) % board_size)

/* xor-shift random number generator. */
static unsigned int rand_state = 2463534242U;

static void
xor_srand(unsigned int seed)
{
  rand_state = seed;
}

static unsigned int
xor_randn(unsigned int n)
{
  rand_state ^= (rand_state << 13);
  rand_state ^= (rand_state >> 17);
  rand_state ^= (rand_state << 5);
  return rand_state % n;
}

static void
clear_board()
{
  memset(board, 0, sizeof(board));
}

static void
init_brown(unsigned int seed)
{
  xor_srand(seed);
  clear_board();
}

static int
get_board(int i, int j)
{
  return board[i * board_size + j];
}

static int
pass_move(int i, int j)
{
  return i == -1 && j == -1;
}

static int
on_board(int i, int j)
{
  return i >= 0 && i < board_size && j >= 0 && j < board_size;
}

static int
legal_move(int i, int j, int color)
{
  int other = OTHER_COLOR(color);

  /* Pass is always legal. */
  if (pass_move(i, j))
    return 1;

  /* Already occupied. */
  if (get_board(i, j) != EMPTY)
    return 0;

  /* Illegal ko recapture. It is not illegal to fill the ko so we must
   * check the color of at least one neighbor.
   */
  if (i == ko_i && j == ko_j
      && ((on_board(i - 1, j) && get_board(i - 1, j) == other)
	  || (on_board(i + 1, j) && get_board(i + 1, j) == other)))
    return 0;

  return 1;
}

/* Does the string at (i, j) have any more liberty than the one at
 * (libi, libj)?
 */
static int
has_additional_liberty(int i, int j, int libi, int libj)
{
  int pos = POS(i, j);
  do {
    int ai = I(pos);
    int aj = J(pos);
    int k;
    for (k = 0; k < 4; k++) {
      int bi = ai + deltai[k];
      int bj = aj + deltaj[k];
      if (on_board(bi, bj) && get_board(bi, bj) == EMPTY
	  && (bi != libi || bj != libj))
	return 1;
    }

    pos = next_stone[pos];
  } while (pos != POS(i, j));

  return 0;
}

/* Does (ai, aj) provide a liberty for a stone at (i, j)? */
static int
provides_liberty(int ai, int aj, int i, int j, int color)
{
  /* A vertex off the board does not provide a liberty. */
  if (!on_board(ai, aj))
    return 0;

  /* An empty vertex IS a liberty. */
  if (get_board(ai, aj) == EMPTY)
    return 1;

  /* A friendly string provides a liberty to (i, j) if it currently
   * has more liberties than the one at (i, j).
   */
  if (get_board(ai, aj) == color)
    return has_additional_liberty(ai, aj, i, j);

  /* An unfriendly string provides a liberty if and only if it is
   * captured, i.e. if it currently only has the liberty at (i, j).
   */
  return !has_additional_liberty(ai, aj, i, j);
}

/* Is a move at (i, j) suicide for color? */
static int
suicide(int i, int j, int color)
{
  int k;
  for (k = 0; k < 4; k++)
    if (provides_liberty(i + deltai[k], j + deltaj[k], i, j, color))
      return 0;

  return 1;
}

/* Remove a string from the board array. There is no need to modify
 * the next_stone array since this only matters where there are
 * stones present and the entire string is removed.
 */
static int
remove_string(int i, int j)
{
  int pos = POS(i, j);
  int removed = 0;
  do {
    board[pos] = EMPTY;
    removed++;
    pos = next_stone[pos];
  } while (pos != POS(i, j));

  return removed;
}

/* Do two vertices belong to the same string. It is required that both
 * pos1 and pos2 point to vertices with stones.
 */
static int
same_string(int pos1, int pos2)
{
  int pos = pos1;
  do {
    if (pos == pos2)
      return 1;
    pos = next_stone[pos];
  } while (pos != pos1);

  return 0;
}

/* Play at (i, j) for color. No legality check is done here. We need
 * to properly update the board array, the next_stone array, and the
 * ko point.
 */
static void
play_move(int i, int j, int color)
{
  int pos = POS(i, j);
  int captured_stones = 0;
  int k;

  /* Reset the ko point. */
  ko_i = -1;
  ko_j = -1;

  /* Nothing more happens if the move was a pass. */
  if (pass_move(i, j))
    return;

  /* If the move is a suicide we only need to remove the adjacent
   * friendly stones.
   */
  if (suicide(i, j, color)) {
    for (k = 0; k < 4; k++) {
      int ai = i + deltai[k];
      int aj = j + deltaj[k];
      if (on_board(ai, aj)
	  && get_board(ai, aj) == color)
	remove_string(ai, aj);
    }
    return;
  }

  /* Not suicide. Remove captured opponent strings. */
  for (k = 0; k < 4; k++) {
    int ai = i + deltai[k];
    int aj = j + deltaj[k];
    if (on_board(ai, aj)
	&& get_board(ai, aj) == OTHER_COLOR(color)
	&& !has_additional_liberty(ai, aj, i, j))
      captured_stones += remove_string(ai, aj);
  }

  /* Put down the new stone. Initially build a single stone string by
   * setting next_stone[pos] pointing to itself.
   */
  board[pos] = color;
  next_stone[pos] = pos;

  /* If we have friendly neighbor strings we need to link the strings
   * together.
   */
  for (k = 0; k < 4; k++) {
    int ai = i + deltai[k];
    int aj = j + deltaj[k];
    int pos2 = POS(ai, aj);
    /* Make sure that the stones are not already linked together. This
     * may happen if the same string neighbors the new stone in more
     * than one direction.
     */
    if (on_board(ai, aj) && board[pos2] == color && !same_string(pos, pos2)) {
      /* The strings are linked together simply by swapping the the
       * next_stone pointers.
       */
      int tmp = next_stone[pos2];
      next_stone[pos2] = next_stone[pos];
      next_stone[pos] = tmp;
    }
  }

  /* If we have captured exactly one stone and the new string is a
   * single stone it may have been a ko capture.
   */
  if (captured_stones == 1 && next_stone[pos] == pos) {
    int ai, aj;
    /* Check whether the new string has exactly one liberty. If so it
     * would be an illegal ko capture to play there immediately. We
     * know that there must be a liberty immediately adjacent to the
     * new stone since we captured one stone.
     */
    for (k = 0; k < 4; k++) {
      ai = i + deltai[k];
      aj = j + deltaj[k];
      if (on_board(ai, aj) && get_board(ai, aj) == EMPTY)
	break;
    }

    if (!has_additional_liberty(i, j, ai, aj)) {
      ko_i = ai;
      ko_j = aj;
    }
  }
}

/* Generate a move. */
static void
generate_move(int *i, int *j, int color)
{
  int moves[MAX_BOARD * MAX_BOARD];
  int num_moves = 0;
  int move;
  int ai, aj;
  int k;

  memset(moves, 0, sizeof(moves));
  for (ai = 0; ai < board_size; ai++)
    for (aj = 0; aj < board_size; aj++) {
      /* Consider moving at (ai, aj) if it is legal and not suicide. */
      if (legal_move(ai, aj, color)
	  && !suicide(ai, aj, color)) {
	/* Further require the move not to be suicide for the opponent... */
	if (!suicide(ai, aj, OTHER_COLOR(color)))
	  moves[num_moves++] = POS(ai, aj);
	else {
	  /* ...however, if the move captures at least one stone,
           * consider it anyway.
	   */
	  for (k = 0; k < 4; k++) {
	    int bi = ai + deltai[k];
	    int bj = aj + deltaj[k];
	    if (on_board(bi, bj) && get_board(bi, bj) == OTHER_COLOR(color)) {
	      moves[num_moves++] = POS(ai, aj);
	      break;
	    }
	  }
	}
      }
    }

  /* Choose one of the considered moves randomly with uniform
   * distribution. (Strictly speaking the moves with smaller 1D
   * coordinates tend to have a very slightly higher probability to be
   * chosen, but for all practical purposes we get a uniform
   * distribution.)
   */
  if (num_moves > 0) {
    move = moves[xor_randn(num_moves)];
    *i = I(move);
    *j = J(move);
  }
  else {
    /* But pass if no move was considered. */
    *i = -1;
    *j = -1;
  }
}

/* Set a final status value for an entire string. */
static void
set_final_status_string(int pos, int status)
{
  int pos2 = pos;
  do {
    final_status[pos2] = status;
    pos2 = next_stone[pos2];
  } while (pos2 != pos);
}

/* Compute final status. This function is only valid to call in a
 * position where generate_move() would return pass for at least one
 * color.
 *
 * Due to the nature of the move generation algorithm, the final
 * status of stones can be determined by a very simple algorithm:
 *
 * 1. Stones with two or more liberties are alive with territory.
 * 2. Stones in atari are dead.
 *
 * Moreover alive stones are unconditionally alive even if the
 * opponent is allowed an arbitrary number of consecutive moves.
 * Similarly dead stones cannot be brought alive even by an arbitrary
 * number of consecutive moves.
 *
 * Seki is not an option. The move generation algorithm would never
 * leave a seki on the board.
 *
 * Comment: This algorithm doesn't work properly if the game ends with
 *          an unfilled ko. If three passes are required for game end,
 *          that will not happen.
 */
static void
compute_final_status(void)
{
  int i, j;
  int pos;
  int k;

  for (pos = 0; pos < board_size * board_size; pos++)
    final_status[pos] = UNKNOWN;

  for (i = 0; i < board_size; i++)
    for (j = 0; j < board_size; j++)
      if (get_board(i, j) == EMPTY)
	for (k = 0; k < 4; k++) {
	  int ai = i + deltai[k];
	  int aj = j + deltaj[k];
	  if (!on_board(ai, aj))
	    continue;
	  /* When the game is finished, we know for sure that (ai, aj)
           * contains a stone. The move generation algorithm would
           * never leave two adjacent empty vertices. Check the number
           * of liberties to decide its status, unless it's known
           * already.
	   *
	   * If we should be called in a non-final position, just make
	   * sure we don't call set_final_status_string() on an empty
	   * vertex.
	   */
	  pos = POS(ai, aj);
	  if (final_status[pos] == UNKNOWN) {
	    if (get_board(ai, aj) != EMPTY) {
	      if (has_additional_liberty(ai, aj, i, j))
		set_final_status_string(pos, ALIVE);
	      else
		set_final_status_string(pos, DEAD);
	    }
	  }
	  /* Set the final status of the (i, j) vertex to either black
           * or white territory.
	   */
	  if (final_status[POS(i, j)] == UNKNOWN) {
	    if ((final_status[pos] == ALIVE) ^ (get_board(ai, aj) == WHITE))
	      final_status[POS(i, j)] = BLACK_TERRITORY;
	    else
	      final_status[POS(i, j)] = WHITE_TERRITORY;
	  }
	}
}

static int
get_final_status(int i, int j)
{
  return final_status[POS(i, j)];
}

static float
compute_score()
{
  int i, j;
  float score = komi;

  compute_final_status();
  for (i = 0; i < board_size; i++) {
    for (j = 0; j < board_size; j++) {
      int status = get_final_status(i, j);
      if (status == BLACK_TERRITORY) {
	score--;
      }
      else if (status == WHITE_TERRITORY) {
	score++;
      }
      else if ((status == ALIVE) ^ (get_board(i, j) == WHITE)) {
	score--;
      }
      else {
	score++;
      }
    }
  }

  return score;
}

static void
benchmark(int num_games_per_point)
{
  int i, j;
  unsigned int random_seed = 1U;
  board_size = 9;
  komi = 0.5;

  init_brown(random_seed);

  for (i = 0; i < board_size; i++) {
    for (j = 0; j < board_size; j++) {
      int white_wins = 0;
      int black_wins = 0;
      int k;
      for (k = 0; k < num_games_per_point; k++) {
	int passes = 0;
	int num_moves = 1;
	int color = WHITE;
        clear_board();
	play_move(i, j, BLACK);
        while (passes < 3 && num_moves < 600) {
	  int m, n;
          generate_move(&m, &n, color);
          play_move(m, n, color);
          if (pass_move(m, n)) {
            passes++;
          }
          else {
            passes = 0;
          }
          num_moves++;
          color = OTHER_COLOR(color);
        }
	if (passes == 3) {
	  if (compute_score() > 0) {
	    white_wins++;
	  }
	  else {
	    black_wins++;
	  }
	}
      }
      /*
      printf("%d %d %f\n", i, j,
	     (float) black_wins / (float) (black_wins + white_wins));
      */
    }
  }
}

int
main(int argc, char **argv)
{
  int num_games_per_point = 10;
  if (argc > 1)
    num_games_per_point = atoi(argv[1]);
  benchmark(num_games_per_point);
  return EXIT_SUCCESS;
}
