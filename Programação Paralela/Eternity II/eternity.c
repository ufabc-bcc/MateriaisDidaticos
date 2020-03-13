/*
  Emilio Francesquini
  e.francesquini@ufabc.edu.br
  2020.Q1
  CC-BY-SA 4.0
*/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

typedef struct {
  unsigned int colors[4];
  unsigned char rotation;
  unsigned int id;
  int used;
} tile;

#define X_COLOR(t, s) (t->colors[(s + 4 - t->rotation) % 4])
#define N_COLOR(t) (X_COLOR(t, 0))
#define E_COLOR(t) (X_COLOR(t, 1))
#define S_COLOR(t) (X_COLOR(t, 2))
#define W_COLOR(t) (X_COLOR(t, 3))

typedef struct {
  unsigned int size;
  unsigned int tile_count; //==size^2
  tile ***board;
  tile *tiles;
} game;

game *initialize (FILE *input) {
  unsigned int bsize;
  unsigned int ncolors;
  int r = fscanf (input, "%u", &bsize);
  assert (r == 1);
  r = fscanf (input, "%u", &ncolors);
  assert (r == 1);
  assert (ncolors < 256);

  //creates an empty board
  game *g = malloc (sizeof(game));
  g->size = bsize;
  g->board = malloc (sizeof (tile**) * bsize);
  for(int i = 0; i < bsize; i++)
    g->board[i] = calloc(bsize, sizeof(tile*));
  g->tile_count = bsize * bsize;

  //loads tiles
  g->tiles = malloc(g->tile_count * sizeof(tile));
  for (unsigned int i = 0; i < g->tile_count; i++) {
    g->tiles[i].rotation = 0;
    g->tiles[i].id = i;
    g->tiles[i].used = 0;
    for (int c = 0; c < 4; c++) {
      r = fscanf(input, "%u", &g->tiles[i].colors[c]);
      assert(r == 1);
    }
  }

  return g;
}

void free_resources(game *game) {
  free(game->tiles);
  for(int i = 0; i < game->size; i++)
    free(game->board[i]);
  free(game->board);
  free(game);
}

int valid_move (game *game, unsigned int x, unsigned int y, tile *tile) {
  //The borders must be 0-colored
  if (x == 0 && W_COLOR(tile) != 0) return 0;
  if (y == 0 && N_COLOR(tile) != 0) return 0;
  if (x == game->size - 1 && E_COLOR(tile) != 0) return 0;
  if (y == game->size - 1 && S_COLOR(tile) != 0) return 0;

  //The tile must also be compatible with its existing neighbours
  if (x > 0 && game->board[x - 1][y] != NULL &&
      E_COLOR(game->board[x - 1][y]) != W_COLOR(tile))
    return 0;
  if (x < game->size - 1 && game->board[x + 1][y] != NULL &&
      W_COLOR(game->board[x + 1][y]) != E_COLOR(tile))
    return 0;
  if (y > 0 && game->board[x][y - 1] != NULL &&
      S_COLOR(game->board[x][y - 1]) != N_COLOR(tile))
    return 0;
  if (y < game->size - 1 && game->board[x][y + 1] != NULL &&
      N_COLOR(game->board[x][y + 1]) != S_COLOR(tile))
    return 0;

  return 1;
}


void print_solution (game *game) {
  for(unsigned int j = 0; j < game->size; j++)
    for(unsigned int i = 0; i < game->size; i++) {
      tile *t = game->board[i][j];
      printf("%u %u\n", t->id, t->rotation);
    }
}

int play (game *game, unsigned int x, unsigned int y) {
  for (int i = 0; i < game->tile_count; i++) {
    if (game->tiles[i].used) continue;
    tile *tile = &game->tiles[i];
    tile->used = 1;
    for (int rot = 0; rot < 4; rot++) {//tries each side
      tile->rotation = rot;
      if (valid_move(game, x, y, tile)) {
	game->board[x][y] = tile;
	unsigned int nx, ny;
	ny = nx = game->size;
	if (x < game->size - 1) {
	  nx = x + 1;
	  ny = y;
	} else if (y < game->size - 1) {
	  nx = 0;
	  ny = y + 1;
	}
	if (ny == game->size || play(game, nx, ny)) {
	  return 1;
	}
	game->board[x][y] = NULL;
      }
    }
    tile->used = 0;
  }
  //no solution was found
  return 0;
}


int main (int argc, char **argv) {
  game *g = initialize(stdin);
  if (play(g, 0, 0))
    print_solution(g);
  else
    printf("SOLUTION NOT FOUND");
  free_resources(g);
}
