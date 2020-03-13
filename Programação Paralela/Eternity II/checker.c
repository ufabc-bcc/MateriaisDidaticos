/*
  Emilio Francesquini
  e.francesquini@ufabc.edu.br
  2020.Q1

  CC-BY-SA 4.0
 */

#include <stdio.h>
#include <stdlib.h>

typedef struct {
  unsigned int colors[4];
  unsigned char rotation;
} tile;

#define X_COLOR(t, s) (t->colors[(s + 4 - t->rotation) % 4])
#define N_COLOR(t) (X_COLOR(t, 0))
#define E_COLOR(t) (X_COLOR(t, 1))
#define S_COLOR(t) (X_COLOR(t, 2))
#define W_COLOR(t) (X_COLOR(t, 3))

typedef struct {
  unsigned int size;
  tile ***board;
  tile *tiles;
} game;
  

game *initialize (FILE *input) {
  unsigned int bsize;
  unsigned int ncolors;
  int r = fscanf (input, "%u", &bsize);
  if (r != 1) exit (1);
  r = fscanf (input, "%u", &ncolors);
  if (r != 1) exit (1);

  //creates an empty board
  game *g = malloc (sizeof(game));
  g->size = bsize;
  g->board = malloc (sizeof (tile**) * bsize);
  for(int i = 0; i < bsize; i++)
  g->board[i] = calloc(bsize, sizeof(tile*));
  int tile_count = bsize * bsize;
  
  //loads tiles
  g->tiles = malloc(tile_count * sizeof(tile));
  for (unsigned int i = 0; i < tile_count; i++) {
    g->tiles[i].rotation = 0;   
    for (int c = 0; c < 4; c++) {
      r = fscanf(input, "%u", &g->tiles[i].colors[c]);
      if (r != 1) exit (1);
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

int main (int argc, char **argv) {
  if (argc != 2) {
    printf("Usage %s input_puzzle <solution\n", argv[0]);
    exit(1);
  }
  
  FILE *f = fopen (argv[1], "r");
  if (!f) {
    printf("File not found\n");
    exit(1);
  }
  
  game *g = initialize(f);

  for (int y = 0; y < g->size; y++) {
    for (int x = 0; x < g->size; x++) {
      unsigned int id, rot, read;
      read = scanf("%u", &id);
      if (read != 1) exit(1);
      read = scanf("%u", &rot);
      if (read != 1) exit(1);
      g->board[x][y] = &g->tiles[id];
      g->tiles[id].rotation = rot;
    }
  }

  for (int x = 0; x < g->size; x++) {
    for (int y = 0; y < g->size; y++) {
      tile *t = g->board[x][y];

      if (x == 0 && W_COLOR(t) != 0) return 1;
      if (x == g->size - 1 && E_COLOR(t) != 0) return 2;
      if (y == 0 && N_COLOR(t) != 0) return 3;
      if (y == g->size - 1 && S_COLOR(t) != 0) return 4;
      if (x < g->size - 1 && E_COLOR(t) != W_COLOR(g->board[x + 1][y])) return 5;
      if (y < g->size - 1 && S_COLOR(t) != N_COLOR(g->board[x][y + 1])) return 6;
    }
  }
      
  free_resources(g);
  return 0;
}
