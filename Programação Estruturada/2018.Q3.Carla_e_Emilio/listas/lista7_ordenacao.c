#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

void bubbleSort(int *v, int tam);
void selectionSort(int *v, int tam);
void insertionSort(int *v, int tam);
void mergeSort(int *v, int tam);

void geraVetor(int *v, int tamanho) {
    int i;
    srand(clock());
    for (i = 0; i < tamanho; i++)
        v[i] = rand();
}

int main () {
    int tamanho, *vetorOriginal, *vetorAuxiliar;
    clock_t inicio;
    double tempo;

    scanf("%d", &tamanho);
    vetorOriginal = malloc(sizeof(int) * tamanho);
    vetorAuxiliar = malloc(sizeof(int) * tamanho);

    geraVetor(vetorOriginal, tamanho);

    memcpy(vetorAuxiliar, vetorOriginal, sizeof(int) * tamanho);
    inicio = clock();
    bubbleSort(vetorAuxiliar, tamanho);
    tempo = (double)(clock() - inicio) / CLOCKS_PER_SEC;
    printf ("Tempo BubbleSort: %lf\n", tempo);

    memcpy(vetorAuxiliar, vetorOriginal, sizeof(int) * tamanho);
    inicio = clock();
    selectionSort(vetorAuxiliar, tamanho);
    tempo = (double)(clock() - inicio) / CLOCKS_PER_SEC;
    printf ("Tempo SelectionSort: %lf\n", tempo);

    memcpy(vetorAuxiliar, vetorOriginal, sizeof(int) * tamanho);
    inicio = clock();
    insertionSort(vetorAuxiliar, tamanho);
    tempo = (double)(clock() - inicio) / CLOCKS_PER_SEC;
    printf ("Tempo InsertionSort: %lf\n", tempo);

    memcpy(vetorAuxiliar, vetorOriginal, sizeof(int) * tamanho);
    inicio = clock();
    mergeSort(vetorAuxiliar, tamanho);
    tempo = (double)(clock() - inicio) / CLOCKS_PER_SEC;
    printf ("Tempo MergeSort: %lf\n", tempo);

    free(vetorAuxiliar);
    free(vetorOriginal);
    return 0;
}
