---
title: "Algoritmos e Estrutura de Dados"
author: Fabrício Olivetti de França
date: 02 de Fevereiro de 2019
mainfont: DejaVu Sans
fontsize: 12pt
---

# Algoritmos de Ordenação Eficientes

## Quick Sort

Uma outra forma de pensar em ordenar registros é utilizando a técnica **dividir e conquistar** conforme aplicado pelo algoritmo **Quick Sort**.

## Quick Sort

Em resumo, dividir e conquistar reduz o problema principal em problemas menores recursivamente até que seja possível resolver o problema de forma trivial.

## Quick Sort

Especificamente no problema de ordenação, imagine que estamos ordenando provas em ordem alfabética. Podemos iniciar com uma ordenação grosseira e depois refinar o resultado.

## Quick Sort

Pegamos a primeira prova da pilha de provas e criamos duas pilhas: a pilha da direita contém todas as provas cujo nome vem antes do nome atual, a da esquerda todas provas cujo nome aparecem depois.

## Quick Sort

Repetindo o processo, em dado momento teremos $n$ pilhas com uma prova cada, ao juntarmos a pilha da mais a esquerda para a mais a direita, teremos nossas provas ordenadas!

## Quick Sort

Em linguagem algorítimica:

- particionamos os registros em torno de um elemento fazendo com que ele fique na posição $p$ e tal que $i < p \implies K_i < K_p$ e $i > p \implies K_i > K_p$.
- repetimos o processo nos elementos de $0$ a $p$ e $p+1$ a $n$.

## Quick Sort

```C
void quickSort(registro *base, int n) {
    if (n > 0)
    {
        int p = partition(base, n);
        quickSort(base, p);
        quickSort(base + p + 1, n - p - 1);
    }
}
```

## Quick Sort

```C
int partition(registro *base, int n) {
    registro pivot = base[0];
    int i=1, j;    
    for (j=1; j<n; j++)
    {
        if (base[j].key < pivot.key)
        {
            swap(base+i, base+j);
            ++i;
        }
    }
    swap(base+i-1, base);
    return i-1;
}
```

## Quick Sort

`swap(x[1], x[1])`

\begin{tikzpicture}[font=\ttfamily,
array/.style={matrix of nodes,nodes={draw, minimum size=7mm, fill=green!30},column sep=-\pgflinewidth, row sep=0.5mm, nodes in empty cells,
row 1/.style={nodes={draw=none, fill=none, minimum size=5mm}},
},ampersand replacement=\&]

\matrix[array] (array) {
0 \& 1 \& 2 \& 3 \& 4 \& 5 \& 6 \& 7 \& 8 \\
88 \& 56 \& 100 \& 2 \& 25 \& 32 \& 1  \& 99  \& 21  \\};

\begin{scope}[on background layer]
\fill[green!10] (array-1-1.north west) rectangle (array-1-9.south east);
\fill[red!10] (array-2-1.north west) rectangle (array-2-1.south east);
\end{scope}

\draw (array-1-2.north)--++(90:3mm) node [above] (first) {$i, j$};

\end{tikzpicture}

## Quick Sort

\begin{tikzpicture}[font=\ttfamily,
array/.style={matrix of nodes,nodes={draw, minimum size=7mm, fill=green!30},column sep=-\pgflinewidth, row sep=0.5mm, nodes in empty cells,
row 1/.style={nodes={draw=none, fill=none, minimum size=5mm}},
},ampersand replacement=\&]

\matrix[array] (array) {
0 \& 1 \& 2 \& 3 \& 4 \& 5 \& 6 \& 7 \& 8 \\
88 \& 56 \& 100 \& 2 \& 25 \& 32 \& 1  \& 99  \& 21  \\};

\begin{scope}[on background layer]
\fill[green!10] (array-1-1.north west) rectangle (array-1-9.south east);
\fill[red!10] (array-2-1.north west) rectangle (array-2-1.south east);
\end{scope}

\draw (array-1-3.north)--++(90:3mm) node [above] (first) {$i, j$};

\end{tikzpicture}

## Quick Sort

`swap(x[2], x[3])`

\begin{tikzpicture}[font=\ttfamily,
array/.style={matrix of nodes,nodes={draw, minimum size=7mm, fill=green!30},column sep=-\pgflinewidth, row sep=0.5mm, nodes in empty cells,
row 1/.style={nodes={draw=none, fill=none, minimum size=5mm}},
},ampersand replacement=\&]

\matrix[array] (array) {
0 \& 1 \& 2 \& 3 \& 4 \& 5 \& 6 \& 7 \& 8 \\
88 \& 56 \& 100 \& 2 \& 25 \& 32 \& 1  \& 99  \& 21  \\};

\begin{scope}[on background layer]
\fill[green!10] (array-1-1.north west) rectangle (array-1-9.south east);
\fill[red!10] (array-2-1.north west) rectangle (array-2-1.south east);
\end{scope}

\draw (array-1-3.north)--++(90:3mm) node [above] (first) {$i$};
\draw (array-1-4.north)--++(90:3mm) node [above] (first) {$j$};

\end{tikzpicture}

## Quick Sort

`swap(x[3], x[4])`

\begin{tikzpicture}[font=\ttfamily,
array/.style={matrix of nodes,nodes={draw, minimum size=7mm, fill=green!30},column sep=-\pgflinewidth, row sep=0.5mm, nodes in empty cells,
row 1/.style={nodes={draw=none, fill=none, minimum size=5mm}},
},ampersand replacement=\&]

\matrix[array] (array) {
0 \& 1 \& 2 \& 3 \& 4 \& 5 \& 6 \& 7 \& 8 \\
88 \& 56 \& 2 \& 100 \& 25 \& 32 \& 1  \& 99  \& 21  \\};

\begin{scope}[on background layer]
\fill[green!10] (array-1-1.north west) rectangle (array-1-9.south east);
\fill[red!10] (array-2-1.north west) rectangle (array-2-1.south east);
\end{scope}

\draw (array-1-4.north)--++(90:3mm) node [above] (first) {$i$};
\draw (array-1-5.north)--++(90:3mm) node [above] (first) {$j$};

\end{tikzpicture}

## Quick Sort

`swap(x[4], x[5])`

\begin{tikzpicture}[font=\ttfamily,
array/.style={matrix of nodes,nodes={draw, minimum size=7mm, fill=green!30},column sep=-\pgflinewidth, row sep=0.5mm, nodes in empty cells,
row 1/.style={nodes={draw=none, fill=none, minimum size=5mm}},
},ampersand replacement=\&]

\matrix[array] (array) {
0 \& 1 \& 2 \& 3 \& 4 \& 5 \& 6 \& 7 \& 8 \\
88 \& 56 \& 2 \& 25 \& 100\& 32 \& 1  \& 99  \& 21  \\};

\begin{scope}[on background layer]
\fill[green!10] (array-1-1.north west) rectangle (array-1-9.south east);
\fill[red!10] (array-2-1.north west) rectangle (array-2-1.south east);
\end{scope}

\draw (array-1-5.north)--++(90:3mm) node [above] (first) {$i$};
\draw (array-1-6.north)--++(90:3mm) node [above] (first) {$j$};

\end{tikzpicture}

## Quick Sort

`swap(x[5], x[6])`

\begin{tikzpicture}[font=\ttfamily,
array/.style={matrix of nodes,nodes={draw, minimum size=7mm, fill=green!30},column sep=-\pgflinewidth, row sep=0.5mm, nodes in empty cells,
row 1/.style={nodes={draw=none, fill=none, minimum size=5mm}},
},ampersand replacement=\&]

\matrix[array] (array) {
0 \& 1 \& 2 \& 3 \& 4 \& 5 \& 6 \& 7 \& 8 \\
88 \& 56 \& 2 \& 25 \& 32\& 100 \& 1  \& 99  \& 21  \\};

\begin{scope}[on background layer]
\fill[green!10] (array-1-1.north west) rectangle (array-1-9.south east);
\fill[red!10] (array-2-1.north west) rectangle (array-2-1.south east);
\end{scope}

\draw (array-1-6.north)--++(90:3mm) node [above] (first) {$i$};
\draw (array-1-7.north)--++(90:3mm) node [above] (first) {$j$};

\end{tikzpicture}

## Quick Sort

\begin{tikzpicture}[font=\ttfamily,
array/.style={matrix of nodes,nodes={draw, minimum size=7mm, fill=green!30},column sep=-\pgflinewidth, row sep=0.5mm, nodes in empty cells,
row 1/.style={nodes={draw=none, fill=none, minimum size=5mm}},
},ampersand replacement=\&]

\matrix[array] (array) {
0 \& 1 \& 2 \& 3 \& 4 \& 5 \& 6 \& 7 \& 8 \\
88 \& 56 \& 2 \& 25 \& 32\& 1 \& 100  \& 99  \& 21  \\};

\begin{scope}[on background layer]
\fill[green!10] (array-1-1.north west) rectangle (array-1-9.south east);
\fill[red!10] (array-2-1.north west) rectangle (array-2-1.south east);
\end{scope}

\draw (array-1-7.north)--++(90:3mm) node [above] (first) {$i$};
\draw (array-1-8.north)--++(90:3mm) node [above] (first) {$j$};

\end{tikzpicture}

## Quick Sort

`swap(6,8)`

\begin{tikzpicture}[font=\ttfamily,
array/.style={matrix of nodes,nodes={draw, minimum size=7mm, fill=green!30},column sep=-\pgflinewidth, row sep=0.5mm, nodes in empty cells,
row 1/.style={nodes={draw=none, fill=none, minimum size=5mm}},
},ampersand replacement=\&]

\matrix[array] (array) {
0 \& 1 \& 2 \& 3 \& 4 \& 5 \& 6 \& 7 \& 8 \\
88 \& 56 \& 2 \& 25 \& 32\& 1 \& 100  \& 99  \& 21  \\};

\begin{scope}[on background layer]
\fill[green!10] (array-1-1.north west) rectangle (array-1-9.south east);
\fill[red!10] (array-2-1.north west) rectangle (array-2-1.south east);
\end{scope}

\draw (array-1-7.north)--++(90:3mm) node [above] (first) {$i$};
\draw (array-1-9.north)--++(90:3mm) node [above] (first) {$j$};

\end{tikzpicture}

## Quick Sort

`swap(0,6)`

\begin{tikzpicture}[font=\ttfamily,
array/.style={matrix of nodes,nodes={draw, minimum size=7mm, fill=green!30},column sep=-\pgflinewidth, row sep=0.5mm, nodes in empty cells,
row 1/.style={nodes={draw=none, fill=none, minimum size=5mm}},
},ampersand replacement=\&]

\matrix[array] (array) {
0 \& 1 \& 2 \& 3 \& 4 \& 5 \& 6 \& 7 \& 8 \\
88 \& 56 \& 2 \& 25 \& 32\& 1 \& 21  \& 99  \& 100  \\};

\begin{scope}[on background layer]
\fill[green!10] (array-1-1.north west) rectangle (array-1-9.south east);
\fill[red!10] (array-2-1.north west) rectangle (array-2-1.south east);
\end{scope}


\end{tikzpicture}

## Quick Sort

`return 5`

\begin{tikzpicture}[font=\ttfamily,
array/.style={matrix of nodes,nodes={draw, minimum size=7mm, fill=green!30},column sep=-\pgflinewidth, row sep=0.5mm, nodes in empty cells,
row 1/.style={nodes={draw=none, fill=none, minimum size=5mm}},
},ampersand replacement=\&]

\matrix[array] (array) {
0 \& 1 \& 2 \& 3 \& 4 \& 5 \& 6 \& 7 \& 8 \\
21 \& 56 \& 2 \& 25 \& 32\& 1 \& 88  \& 99  \& 100  \\};

\begin{scope}[on background layer]
\fill[green!10] (array-1-1.north west) rectangle (array-1-9.south east);
\fill[red!10] (array-2-1.north west) rectangle (array-2-1.south east);
\end{scope}


\end{tikzpicture}

## Quick Sort

\begin{tikzpicture}[font=\ttfamily,
array/.style={matrix of nodes,nodes={draw, minimum size=7mm, fill=green!30},column sep=-\pgflinewidth, row sep=0.5mm, nodes in empty cells,
row 1/.style={nodes={draw=none, fill=none, minimum size=5mm}},
},ampersand replacement=\&]

\matrix[array] (array) {
0 \& 1 \& 2 \& 3 \& 4 \& 5  \\
21 \& 56 \& 2 \& 25 \& 32 \& 1   \\};

\begin{scope}[on background layer]
\fill[green!10] (array-1-1.north west) rectangle (array-1-6.south east);
\fill[red!10] (array-2-1.north west) rectangle (array-2-1.south east);
\end{scope}

\draw (array-1-2.north)--++(90:3mm) node [above] (first) {$i$};
\draw (array-1-3.north)--++(90:3mm) node [above] (first) {$j$};

\end{tikzpicture}

## Quick Sort

\begin{tikzpicture}[font=\ttfamily,
array/.style={matrix of nodes,nodes={draw, minimum size=7mm, fill=green!30},column sep=-\pgflinewidth, row sep=0.5mm, nodes in empty cells,
row 1/.style={nodes={draw=none, fill=none, minimum size=5mm}},
},ampersand replacement=\&]

\matrix[array] (array) {
0 \& 1 \& 2 \& 3 \& 4 \& 5  \\
21 \& 2 \& 56 \& 25 \& 32 \& 1   \\};

\begin{scope}[on background layer]
\fill[green!10] (array-1-1.north west) rectangle (array-1-6.south east);
\fill[red!10] (array-2-1.north west) rectangle (array-2-1.south east);
\end{scope}

\draw (array-1-3.north)--++(90:3mm) node [above] (first) {$i$};
\draw (array-1-4.north)--++(90:3mm) node [above] (first) {$j$};

\end{tikzpicture}

## Quick Sort

\begin{tikzpicture}[font=\ttfamily,
array/.style={matrix of nodes,nodes={draw, minimum size=7mm, fill=green!30},column sep=-\pgflinewidth, row sep=0.5mm, nodes in empty cells,
row 1/.style={nodes={draw=none, fill=none, minimum size=5mm}},
},ampersand replacement=\&]

\matrix[array] (array) {
0 \& 1 \& 2 \& 3 \& 4 \& 5  \\
21 \& 2 \& 56 \& 25 \& 32 \& 1   \\};

\begin{scope}[on background layer]
\fill[green!10] (array-1-1.north west) rectangle (array-1-6.south east);
\fill[red!10] (array-2-1.north west) rectangle (array-2-1.south east);
\end{scope}

\draw (array-1-3.north)--++(90:3mm) node [above] (first) {$i$};
\draw (array-1-5.north)--++(90:3mm) node [above] (first) {$j$};

\end{tikzpicture}

## Quick Sort

\begin{tikzpicture}[font=\ttfamily,
array/.style={matrix of nodes,nodes={draw, minimum size=7mm, fill=green!30},column sep=-\pgflinewidth, row sep=0.5mm, nodes in empty cells,
row 1/.style={nodes={draw=none, fill=none, minimum size=5mm}},
},ampersand replacement=\&]

\matrix[array] (array) {
0 \& 1 \& 2 \& 3 \& 4 \& 5  \\
21 \& 2 \& 56 \& 25 \& 32 \& 1   \\};

\begin{scope}[on background layer]
\fill[green!10] (array-1-1.north west) rectangle (array-1-6.south east);
\fill[red!10] (array-2-1.north west) rectangle (array-2-1.south east);
\end{scope}

\draw (array-1-3.north)--++(90:3mm) node [above] (first) {$i$};
\draw (array-1-6.north)--++(90:3mm) node [above] (first) {$j$};

\end{tikzpicture}

## Quick Sort

\begin{tikzpicture}[font=\ttfamily,
array/.style={matrix of nodes,nodes={draw, minimum size=7mm, fill=green!30},column sep=-\pgflinewidth, row sep=0.5mm, nodes in empty cells,
row 1/.style={nodes={draw=none, fill=none, minimum size=5mm}},
},ampersand replacement=\&]

\matrix[array] (array) {
0 \& 1 \& 2 \& 3 \& 4 \& 5  \\
21 \& 2 \& 1 \& 25 \& 32 \& 56   \\};

\begin{scope}[on background layer]
\fill[green!10] (array-1-1.north west) rectangle (array-1-6.south east);
\fill[red!10] (array-2-1.north west) rectangle (array-2-1.south east);
\end{scope}

\draw (array-1-4.north)--++(90:3mm) node [above] (first) {$i$};
\draw (array-1-6.north)--++(90:3mm) node [above] (first) {$j$};

\end{tikzpicture}

## Quick Sort

retorna $p=3$

\begin{tikzpicture}[font=\ttfamily,
array/.style={matrix of nodes,nodes={draw, minimum size=7mm, fill=green!30},column sep=-\pgflinewidth, row sep=0.5mm, nodes in empty cells,
row 1/.style={nodes={draw=none, fill=none, minimum size=5mm}},
},ampersand replacement=\&]

\matrix[array] (array) {
0 \& 1 \& 2 \& 3 \& 4 \& 5  \\
1 \& 2 \& 21 \& 25 \& 32 \& 56   \\};

\begin{scope}[on background layer]
\fill[green!10] (array-1-1.north west) rectangle (array-1-6.south east);
\end{scope}

\draw (array-1-4.north)--++(90:3mm) node [above] (first) {$i$};
\draw (array-1-6.north)--++(90:3mm) node [above] (first) {$j$};

\end{tikzpicture}

## Quick Sort

retorna $p=7$.

\begin{tikzpicture}[font=\ttfamily,
array/.style={matrix of nodes,nodes={draw, minimum size=7mm, fill=green!30},column sep=-\pgflinewidth, row sep=0.5mm, nodes in empty cells,
row 1/.style={nodes={draw=none, fill=none, minimum size=5mm}},
},ampersand replacement=\&]

\matrix[array] (array) {
0 \& 1 \& 2  \\
 88  \& 99  \& 100  \\};

\begin{scope}[on background layer]
\fill[green!10] (array-1-1.north west) rectangle (array-1-3.south east);
\fill[red!10] (array-2-1.north west) rectangle (array-2-1.south east);
\end{scope}

\draw (array-1-2.north)--++(90:3mm) node [above] (first) {$i$};
\draw (array-1-3.north)--++(90:3mm) node [above] (first) {$j$};

\end{tikzpicture}

## Quick Sort

retorna $p=7$.

\begin{tikzpicture}[font=\ttfamily,
array/.style={matrix of nodes,nodes={draw, minimum size=7mm, fill=green!30},column sep=-\pgflinewidth, row sep=0.5mm, nodes in empty cells,
row 1/.style={nodes={draw=none, fill=none, minimum size=5mm}},
},ampersand replacement=\&]

\matrix[array] (array) {
0 \& 1 \& 2  \\
 88  \& 99  \& 100  \\};

\begin{scope}[on background layer]
\fill[green!10] (array-1-1.north west) rectangle (array-1-3.south east);
\fill[red!10] (array-2-1.north west) rectangle (array-2-1.south east);
\end{scope}

\draw (array-1-2.north)--++(90:3mm) node [above] (first) {$i$};
\draw (array-1-3.north)--++(90:3mm) node [above] (first) {$j$};

\end{tikzpicture}

## Quick Sort

retorna $p=0$.

\begin{tikzpicture}[font=\ttfamily,
array/.style={matrix of nodes,nodes={draw, minimum size=7mm, fill=green!30},column sep=-\pgflinewidth, row sep=0.5mm, nodes in empty cells,
row 1/.style={nodes={draw=none, fill=none, minimum size=5mm}},
},ampersand replacement=\&]

\matrix[array] (array) {
0 \& 1 \& 2  \\
 88  \& 99  \& 100  \\};

\begin{scope}[on background layer]
\fill[green!10] (array-1-1.north west) rectangle (array-1-3.south east);
\fill[red!10] (array-2-1.north west) rectangle (array-2-1.south east);
\end{scope}

\draw (array-1-2.north)--++(90:3mm) node [above] (first) {$i$};
\draw (array-1-3.north)--++(90:3mm) node [above] (first) {$j$};

\end{tikzpicture}

## Quick Sort

\begin{tabular}{lrrrr}
\toprule
 & Insert & Bubble & Select & Quick \\
\midrule
estável & \checkmark & \checkmark & & \\
in-place & \checkmark  & \checkmark & \checkmark & \checkmark \\
online &  \checkmark & & \\
adaptivo & \checkmark & \checkmark & &\\
\bottomrule
\end{tabular}

## Complexidade Assintótica

A primeira chamada de `partition` percorre os $n$ elementos da lista. 

## Complexidade Assintótica

No melhor caso, o particionamento vai dividir a lista em duas partes iguais, ou seja, os dois próximos particionamentos percorrerão $n/2$ elementos (em um total de $n$ elementos), cada uma das duas partições pode gerar duas chamadas de listas de tamanho $n/4$ (em um total de $n$ elementos). 

## Complexidade Assintótica

No melhor caso, temos uma complexidade $O(n \cdot k)$ sendo $k$ a altura da árvore de partições.

\begin{forest}
rounded/.style={circle, draw},
    [{n}, for tree=rounded
      [{n/2}
        [{n/4}]
        [{n/4}]
      ]
      [{n/2}
        [{n/4}]
        [{n/4}]
      ]
    ]
\end{forest}

## Complexidade Assintótica

O valor de $k$ é quantas vezes podemos dividir $n$ por $2$ até atingir $1$, ou seja, $\frac{n}{2^k} = 1$, temos então que:

$n = 2^k$

$\lg{n} = k$

Com isso nosso melhor caso é $O(n\log{n})$. 

## Complexidade Assintótica

Por outro lado, se o particionamento faz com que uma sublista tenha $n-1$ elementos e a outra nenhum, teremos uma sequência de $\sum_{i=0}^{n}{n-i}$ operações, o que leva a um pior caso de $O(n^2)$.

## Complexidade Assintótica

Para evitar o pior caso, devemos escolher um pivot que esteja aproximadamente posicionado no meio da lista. Podemos, por exemplo, calcular a mediana de uma amostra pequena da lista.

O custo extra pode compensar pelo fato de evitar o pior caso.

## Complexidade Assintótica

\begin{tabular}{lrrrr}
\toprule
 & Insert & Bubble & Select & Quick  \\
 \midrule
melhor & $O(n)$ & $O(n)$ & $O(n^2)$ & $O(n \log{n})$ \\
pior & $O(n^2)$  & $O(n^2)$ & $O(n^2)$ & $O(n^2)$  \\
médio & $O(n^2)$ & $O(n^2)$ & $O(n^2)$  & $O(n \log{n})$ \\
\bottomrule
\end{tabular}

## Merge Sort

Similar ao Quick Sort, o **Merge Sort** divide o problema de ordenação em problemas menores.

## Merge Sort

Para tanto, ele faz chamadas recursivas para a faixa de valores de $[0,n/2[$ e $[n/2,n[$, em seguida executando um procedimento chamado `merge` que concatena o resultado das duas chamadas recursiva.

## Merge Sort

Ou seja, ele ordena as sublistas da metade inicial e da metade final, e depois concatena as duas de forma eficiente.

## Merge Sort

```C
void mergeSort(registro *base, int n) {
    if (n > 1)
    {
        int middle = n/2;
        mergeSort(base, middle);
        mergeSort(base + middle, n - middle);
        merge(base, middle, n);
    }
}
```

## Merge Sort

```C
void merge(registro *base, int m, int n) {
    registro x[n];
    int j=0, k=m;
    
    for (int i=0; i<n; ++i) {
        if (j==m)       x[i] = base[k++];
        else if (k==n)  x[i] = base[j++];
        else if (base[j].key < base[k].key) 
                        x[i] = base[j++];
        else            x[i] = base[k++];
    }
    for (int i=0; i<n; i++) base[i]=x[i];
}
```
## Merge Sort

Merge!

\begin{tikzpicture}[font=\ttfamily,
array/.style={matrix of nodes,nodes={draw, minimum size=7mm, fill=green!30},column sep=-\pgflinewidth, row sep=0.5mm, nodes in empty cells,
row 1/.style={nodes={draw=none, fill=none, minimum size=5mm}},
},ampersand replacement=\&]

\matrix[array] (array) {
0 \& 1 \& 2 \& 3 \& 4 \& 5 \& 6 \& 7 \& 8 \\
2 \& 56 \& 88 \& 100 \& 1 \& 21  \& 25  \& 32  \& 99  \\
  \&    \&     \&     \&   \&    \&     \&     \&     \\
};

\begin{scope}[on background layer]
\fill[green!10] (array-1-1.north west) rectangle (array-1-9.south east);
\end{scope}

\draw (array-1-1.north)--++(90:3mm) node [above] (first) {$j$};
\draw (array-1-5.north)--++(90:3mm) node [above] (first) {$k$};

\end{tikzpicture}

## Merge Sort

Merge!

\begin{tikzpicture}[font=\ttfamily,
array/.style={matrix of nodes,nodes={draw, minimum size=7mm, fill=green!30},column sep=-\pgflinewidth, row sep=0.5mm, nodes in empty cells,
row 1/.style={nodes={draw=none, fill=none, minimum size=5mm}},
},ampersand replacement=\&]

\matrix[array] (array) {
0 \& 1 \& 2 \& 3 \& 4 \& 5 \& 6 \& 7 \& 8 \\
2 \& 56 \& 88 \& 100 \& 1 \& 21  \& 25  \& 32  \& 99  \\
1 \&    \&     \&     \&   \&    \&     \&     \&     \\
};

\begin{scope}[on background layer]
\fill[green!10] (array-1-1.north west) rectangle (array-1-9.south east);
\end{scope}

\draw (array-1-1.north)--++(90:3mm) node [above] (first) {$j$};
\draw (array-1-6.north)--++(90:3mm) node [above] (first) {$k$};

\end{tikzpicture}

## Merge Sort

Merge!

\begin{tikzpicture}[font=\ttfamily,
array/.style={matrix of nodes,nodes={draw, minimum size=7mm, fill=green!30},column sep=-\pgflinewidth, row sep=0.5mm, nodes in empty cells,
row 1/.style={nodes={draw=none, fill=none, minimum size=5mm}},
},ampersand replacement=\&]

\matrix[array] (array) {
0 \& 1 \& 2 \& 3 \& 4 \& 5 \& 6 \& 7 \& 8 \\
2 \& 56 \& 88 \& 100 \& 1 \& 21  \& 25  \& 32  \& 99  \\
1 \& 2  \&     \&     \&   \&    \&     \&     \&     \\
};

\begin{scope}[on background layer]
\fill[green!10] (array-1-1.north west) rectangle (array-1-9.south east);
\end{scope}

\draw (array-1-2.north)--++(90:3mm) node [above] (first) {$j$};
\draw (array-1-6.north)--++(90:3mm) node [above] (first) {$k$};

\end{tikzpicture}

## Merge Sort

Merge!

\begin{tikzpicture}[font=\ttfamily,
array/.style={matrix of nodes,nodes={draw, minimum size=7mm, fill=green!30},column sep=-\pgflinewidth, row sep=0.5mm, nodes in empty cells,
row 1/.style={nodes={draw=none, fill=none, minimum size=5mm}},
},ampersand replacement=\&]

\matrix[array] (array) {
0 \& 1 \& 2 \& 3 \& 4 \& 5 \& 6 \& 7 \& 8 \\
2 \& 56 \& 88 \& 100 \& 1 \& 21  \& 25  \& 32  \& 99  \\
1 \& 2  \& 21  \&     \&   \&    \&     \&     \&     \\
};

\begin{scope}[on background layer]
\fill[green!10] (array-1-1.north west) rectangle (array-1-9.south east);
\end{scope}

\draw (array-1-2.north)--++(90:3mm) node [above] (first) {$j$};
\draw (array-1-7.north)--++(90:3mm) node [above] (first) {$k$};

\end{tikzpicture}

## Merge Sort

Merge!

\begin{tikzpicture}[font=\ttfamily,
array/.style={matrix of nodes,nodes={draw, minimum size=7mm, fill=green!30},column sep=-\pgflinewidth, row sep=0.5mm, nodes in empty cells,
row 1/.style={nodes={draw=none, fill=none, minimum size=5mm}},
},ampersand replacement=\&]

\matrix[array] (array) {
0 \& 1 \& 2 \& 3 \& 4 \& 5 \& 6 \& 7 \& 8 \\
2 \& 56 \& 88 \& 100 \& 1 \& 21  \& 25  \& 32  \& 99  \\
1 \& 2  \& 21  \& 25  \&   \&    \&     \&     \&     \\
};

\begin{scope}[on background layer]
\fill[green!10] (array-1-1.north west) rectangle (array-1-9.south east);
\end{scope}

\draw (array-1-2.north)--++(90:3mm) node [above] (first) {$j$};
\draw (array-1-8.north)--++(90:3mm) node [above] (first) {$k$};

\end{tikzpicture}

## Merge Sort

Merge!

\begin{tikzpicture}[font=\ttfamily,
array/.style={matrix of nodes,nodes={draw, minimum size=7mm, fill=green!30},column sep=-\pgflinewidth, row sep=0.5mm, nodes in empty cells,
row 1/.style={nodes={draw=none, fill=none, minimum size=5mm}},
},ampersand replacement=\&]

\matrix[array] (array) {
0 \& 1 \& 2 \& 3 \& 4 \& 5 \& 6 \& 7 \& 8 \\
2 \& 56 \& 88 \& 100 \& 1 \& 21  \& 25  \& 32  \& 99  \\
1 \& 2  \& 21  \& 25  \& 32  \&    \&     \&     \&     \\
};

\begin{scope}[on background layer]
\fill[green!10] (array-1-1.north west) rectangle (array-1-9.south east);
\end{scope}

\draw (array-1-2.north)--++(90:3mm) node [above] (first) {$j$};
\draw (array-1-9.north)--++(90:3mm) node [above] (first) {$k$};

\end{tikzpicture}

## Merge Sort

Merge!

\begin{tikzpicture}[font=\ttfamily,
array/.style={matrix of nodes,nodes={draw, minimum size=7mm, fill=green!30},column sep=-\pgflinewidth, row sep=0.5mm, nodes in empty cells,
row 1/.style={nodes={draw=none, fill=none, minimum size=5mm}},
},ampersand replacement=\&]

\matrix[array] (array) {
0 \& 1 \& 2 \& 3 \& 4 \& 5 \& 6 \& 7 \& 8 \\
2 \& 56 \& 88 \& 100 \& 1 \& 21  \& 25  \& 32  \& 99  \\
1 \& 2  \& 21  \& 25  \& 32  \& 56   \&     \&     \&     \\
};

\begin{scope}[on background layer]
\fill[green!10] (array-1-1.north west) rectangle (array-1-9.south east);
\end{scope}

\draw (array-1-3.north)--++(90:3mm) node [above] (first) {$j$};
\draw (array-1-9.north)--++(90:3mm) node [above] (first) {$k$};

\end{tikzpicture}

## Merge Sort

Merge!

\begin{tikzpicture}[font=\ttfamily,
array/.style={matrix of nodes,nodes={draw, minimum size=7mm, fill=green!30},column sep=-\pgflinewidth, row sep=0.5mm, nodes in empty cells,
row 1/.style={nodes={draw=none, fill=none, minimum size=5mm}},
},ampersand replacement=\&]

\matrix[array] (array) {
0 \& 1 \& 2 \& 3 \& 4 \& 5 \& 6 \& 7 \& 8 \\
2 \& 56 \& 88 \& 100 \& 1 \& 21  \& 25  \& 32  \& 99  \\
1 \& 2  \& 21  \& 25  \& 32  \& 56   \& 88    \&     \&     \\
};

\begin{scope}[on background layer]
\fill[green!10] (array-1-1.north west) rectangle (array-1-9.south east);
\end{scope}

\draw (array-1-4.north)--++(90:3mm) node [above] (first) {$j$};
\draw (array-1-9.north)--++(90:3mm) node [above] (first) {$k$};

\end{tikzpicture}

## Merge Sort

Merge!

\begin{tikzpicture}[font=\ttfamily,
array/.style={matrix of nodes,nodes={draw, minimum size=7mm, fill=green!30},column sep=-\pgflinewidth, row sep=0.5mm, nodes in empty cells,
row 1/.style={nodes={draw=none, fill=none, minimum size=5mm}},
},ampersand replacement=\&]

\matrix[array] (array) {
0 \& 1 \& 2 \& 3 \& 4 \& 5 \& 6 \& 7 \& 8 \\
2 \& 56 \& 88 \& 100 \& 1 \& 21  \& 25  \& 32  \& 99  \\
1 \& 2  \& 21  \& 25  \& 32  \& 56   \& 88    \& 99    \&     \\
};

\begin{scope}[on background layer]
\fill[green!10] (array-1-1.north west) rectangle (array-1-9.south east);
\end{scope}

\draw (array-1-4.north)--++(90:3mm) node [above] (first) {$j$};
\draw (array-1-9.north)--++(90:3mm) node [above] (first) {$k$};

\end{tikzpicture}

## Merge Sort

Merge!

\begin{tikzpicture}[font=\ttfamily,
array/.style={matrix of nodes,nodes={draw, minimum size=7mm, fill=green!30},column sep=-\pgflinewidth, row sep=0.5mm, nodes in empty cells,
row 1/.style={nodes={draw=none, fill=none, minimum size=5mm}},
},ampersand replacement=\&]

\matrix[array] (array) {
0 \& 1 \& 2 \& 3 \& 4 \& 5 \& 6 \& 7 \& 8 \\
2 \& 56 \& 88 \& 100 \& 1 \& 21  \& 25  \& 32  \& 99  \\
1 \& 2  \& 21  \& 25  \& 32  \& 56   \& 88 \& 99 \& 100 \\
};

\begin{scope}[on background layer]
\fill[green!10] (array-1-1.north west) rectangle (array-1-9.south east);
\end{scope}

\draw (array-1-4.north)--++(90:3mm) node [above] (first) {$j$};
\draw (array-1-9.north)--++(90:3mm) node [above] (first) {$k$};

\end{tikzpicture}


## Merge Sort

\begin{tabular}{lrrrrr}
\toprule
 & Insert & Bubble & Select & Quick & Merge  \\
\midrule
estável & \checkmark & \checkmark & & & \checkmark \\
in-place & \checkmark  & \checkmark & \checkmark & \checkmark & \\
online &  \checkmark & & & \\
adaptivo & \checkmark & \checkmark & & &\\
\bottomrule
\end{tabular}

## Complexidade Assintótica

O algoritmo Merge Sort divide cada lista em duas sublistas de igual tamanho, o procedimento de merge tem complexidade $O(k)$ sendo $k$ a soma do número de elementos das duas sublistas.

Em cada nível da árvore fazemos então $n$ operações (somatória de todos os merges). Como nossa árvore é balanceada, temos uma altura de $\log{n}$. Portanto, mesmo no pior caso, a complexidade é $O(n \log{n})$.

## Complexidade Assintótica

O pior caso do Merge Sort faz cerca de $40\%$ menos comparações que o caso médio do Quick Sort, porém necessita de uma estrutura auxiliar para o procedimento de `merge`.

## Complexidade Assintótica

Geralmente ela é utilizada para casos em que os registros somente podem ser acessados de forma eficiente na sequência (arquivos externos, listas ligadas).

## Complexidade Assintótica

\begin{tabular}{lrrrrr}
\toprule
 & Insert & Bubble & Select & Quick & Merge  \\
 \midrule
melhor & $O(n)$ & $O(n)$ & $O(n^2)$ & $O(n \log{n})$ & $O(n \log{n})$ \\
pior & $O(n^2)$  & $O(n^2)$ & $O(n^2)$ & $O(n^2)$ & $O(n \log{n})$ \\
médio & $O(n^2)$ & $O(n^2)$ & $O(n^2)$  & $O(n \log{n})$ & $O(n \log{n})$ \\
\bottomrule
\end{tabular}

## Próxima aula

Na próxima aula aprenderemos os algoritmos **heap sort** e **bucket sort**.
