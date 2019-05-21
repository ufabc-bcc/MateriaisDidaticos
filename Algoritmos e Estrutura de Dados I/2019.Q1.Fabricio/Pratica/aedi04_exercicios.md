---
title: "Algoritmos e Estrutura de Dados"
author: Paulo Pisani
date: 15 de Fevereiro de 2019
mainfont: DejaVu Sans
fontsize: 12pt
---

# Exercícios de Estruturas Lineares

## Exercício 01

Utilizando a estrutura de pilhas, faça uma função:

```C
void inverte_string(char * s, char * dest);
```

que inverte uma string utilizando uma estrutura de pilha. O argumento `s` representa a string a ser invertida e o argumento `dest` uma string pré-alocada que conterá a string invertida.

## Exercício 02

Uma expressão matemática pode ser escrita de forma:

- **Infixa:** $2 * 3 + 4$.
- **Préfixa:** $+ * 2 3 4$.
- **Pósfixa:** $2 3 * 4 -$.

Para avaliar uma expressão pósfixa, seguimos o seguinte algoritmo:

- Leia o próximo caractere `c`
- Se `c` for um número insere na pilha `p`
- Se `c` for um dos operadores matemáticos desempilha dois números da pilha, efetua a operação e empilha o resultado
- Se `c` for qualquer outro símbolo, ignore
- Caso ainda exista caracteres, volte ao início
- Retorne como resultado o único valor da pilha

Assumindo números de apenas um dígito e utilizando a função `isdigit` da biblioteca *ctype.h*, implemente um programa que avalie uma expressão pósfixa.

## Exercício 03

O problema das 8 rainhas consiste em colocar 8 rainhas em um tabuleiro de xadrez de tal forma que nenhum par de rainhas se ataquem.

FIG

Uma forma de encontrar uma solução para esse problema utiliza uma representação de array de 8 elementos:

```C
int sol[8] = {1, 2, 3, 4, -1, -1, -1, -1}
```

em que cada posição do vetor representa uma coluna do tabuleiro e o valor a linha correspondente em que a rainha se encontra. O valor $-1$ indica que aquela rainha ainda não foi alocada.

Dado o estado:

```C
int s0[8] = {-1, -1, -1, -1, -1, -1, -1, -1}
```

Os próximos estados podem ser:

```C
{1, -1, -1, -1, -1, -1, -1, -1}
{2, -1, -1, -1, -1, -1, -1, -1}
{3, -1, -1, -1, -1, -1, -1, -1}
{4, -1, -1, -1, -1, -1, -1, -1}
{5, -1, -1, -1, -1, -1, -1, -1}
{6, -1, -1, -1, -1, -1, -1, -1}
{7, -1, -1, -1, -1, -1, -1, -1}
{8, -1, -1, -1, -1, -1, -1, -1}
```

O algoritmo funciona da seguinte forma:

1. Insere o estado `s0` em uma fila
2. Remove o primeiro elemento da fila em `s`
3. Para cada possível próximo estado de `s`, se for uma solução termina, senão se for factível insere na fila
4. Se a fila estiver vazia, termina com erro, senão retorna ao passo 2

Para verificar se uma solução é factível utilize a função:

```C
#include <stdlib.h>

int ataca(int linha1, int col1, int linha2, int col2) {
    return linha1 == linha2 || col1 == col2
           || abs(linha1-linha2) == abs(col1-col2);
}

int factivel(int * sol) {
    int i, j;
    for (i=0; i<7; i++) {
        for (j=i+1; j<8; j++) {
	    if (sol[j] == -1) return 1;
	    if (ataca(i+1, sol[i], j+1, sol[j])) return 0;
	}
    }
    return 1;
}
```

**Não se esqueça de liberar toda a memória alocada ao final!!!**
