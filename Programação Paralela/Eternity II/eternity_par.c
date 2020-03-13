/*
 * Emilio Francesquini <e.francesquini@ufabc.edu.br>
 * 2019-04-08
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <pthread.h>


int done;
pthread_mutex_t lock;

typedef struct {
    int colors[4];
    char rotation;
    int id;
    int used;
} tile;

#define X_COLOR(t, s) (t->colors[(s + 4 - t->rotation) % 4])
#define N_COLOR(t) (X_COLOR(t, 0))
#define E_COLOR(t) (X_COLOR(t, 1))
#define S_COLOR(t) (X_COLOR(t, 2))
#define W_COLOR(t) (X_COLOR(t, 3))

typedef struct {
    int size;
    int tile_count; //==size^2
    tile ***board;
    tile *tiles;
} game;


game *initialize (FILE *input, int copies) {
    int bsize;
    int ncolors;
    assert(copies >= 1);
    int r = fscanf (input, "%u", &bsize);
    assert (r == 1);
    r = fscanf (input, "%u", &ncolors);
    assert (r == 1);
    assert (ncolors < 256);

    int tile_count = bsize * bsize;

    //creates empty boards
    game *games = malloc (sizeof(game) * copies);
    for (int index = 0; index < copies; ++index) {
        game *g = games + index;
        g->size = bsize;
        g->board = malloc (sizeof (tile**) * bsize);
        for(int j = 0; j < bsize; j++)
            g->board[j] = calloc(bsize, sizeof(tile*));
        g->tile_count = tile_count;

        //loads tiles
        g->tiles = malloc(tile_count * sizeof(tile));
    }


    for (int i = 0; i < tile_count; i++)
        for (int c = 0; c < 4; c++) {
            int color;
            r = fscanf(input, "%u", &color);
            assert(r == 1);
            for (int j = 0; j < copies; j++) {
                game *g = games + j;
                g->tiles[i].colors[c] = color;
                g->tiles[i].rotation = 0;
                g->tiles[i].id = i;
                g->tiles[i].used = 0;
            }
        }

    return games;
}

void free_resources(game *game, int copies) {
    for (int g = 0; g < copies; g++) {
        free(game[g].tiles);
        for(int i = 0; i < game[g].size; i++)
            free(game[g].board[i]);
        free(game[g].board);
    }
    free(game);
}

int valid_move (game *game, int x, int y, tile *tile) {
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
    for(int j = 0; j < game->size; j++)
        for(int i = 0; i < game->size; i++) {
            tile *t = game->board[i][j];
            printf("%u %u\n", t->id, t->rotation);
        }
}



int play (game *game, int x, int y, int tile_start, int tile_step) {
    if (done != -1)
        return 0;
    for (int i = tile_start; i < game->tile_count; i += tile_step) {
        if (game->tiles[i].used) continue;
        tile *tile = &game->tiles[i];
        tile->used = 1;
        for (int rot = 0; rot < 4; rot++) {//tries each side
            tile->rotation = rot;
            if (valid_move(game, x, y, tile)) {
                game->board[x][y] = tile;
                int nx, ny;
                ny = nx = game->size;
                if (x < game->size - 1) {
                    nx = x + 1;
                    ny = y;
                } else if (y < game->size - 1) {
                    nx = 0;
                    ny = y + 1;
                }
                if (ny == game->size || play(game, nx, ny, 0, 1)) {
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

struct thread_init_param {
    game *game;
    int x, y, tile_start, tile_step;
};

void *init_thread (void* param) {
    struct thread_init_param *p = (struct thread_init_param*)param;
    if (play(p->game, p->x, p->y, p->tile_start, p->tile_step)) {
        pthread_mutex_lock(&lock);
        if (done == -1)
            done = p->tile_start;
        pthread_mutex_unlock(&lock);
    }
    return NULL;
}

int main (int argc, char *argv[]) {

    int nthreads = atoi(argv[1]);
    pthread_t threads[nthreads];
    struct thread_init_param tparams[nthreads];

    done = -1;
    pthread_mutex_init(&lock, NULL);

    game *g = initialize(stdin, nthreads);
    for (int t = 0; t < nthreads; t++) {
        tparams[t].game = &g[t];
        tparams[t].x = 0;
        tparams[t].y = 0;
        tparams[t].tile_start = t;
        tparams[t].tile_step = nthreads;
        pthread_create(&threads[t], NULL, init_thread, &tparams[t]);
    }

    for (int t = 0; t < nthreads; t++)
        pthread_join(threads[t], NULL);

    if (done != -1)
        print_solution(&g[done]);
    else
        printf("SOLUTION NOT FOUND");
    free_resources(g, nthreads);
}
