---
title: "Algoritmos e Estrutura de Dados I"
author: Fabrício Olivetti de França e Paulo Henrique Pisani
date: 13 de março de 2019
header-includes: \usepackage{forest}
---

# Lista de Exercícios 3

## Árvores AVL

1. Responda para a seguinte árvore AVL:

\begin{forest}
rounded/.style={circle, draw},
    [{5}, for tree=rounded
      [{3}
        [{2}
          [{1}]
          [,phantom]
        ]
        [{4}]
      ]
      [{10}
        [{7}
          [{6}]
          [{9}
            [{8}]
            [,phantom]
          ]
        ]
        [{11}
          [,phantom]
          [{12}]
        ]
      ]
    ]
\end{forest}

a. Desenhe a árvore binária de busca ao remover o nó $5$ sem fazer o rebalanceamento. Desenhe ao lado dos nós a altura e o fator de rebalanceamento (`height(left) - height(right)`)
b. Desenhe passo a passo o rebalanceamento da árvore

2. Desenhe a construção passo a passo da árvore AVL resultante da inserção dos valores na ordem indicada:

a. $9, 27, 50, 15, 2, 21, 36$
b. $1, 2, 3, 4, 5, 6, 7, 8, 9, 10$
c. $10, 9, 8, 7, 6, 5, 4, 3, 2, 1$

3. Desenhe a árvore AVL ao inserir os elementos $50, 25, 78, 15, 40, 60, 80, 35, 55, 65, 90, 62$.  Desenhe passo a passo a remoção do nó 15.

4. Desenhe uma árvore AVL de altura $4$ com o menor número possível de nós.

5. Crie um algoritmo que converta uma árvore binária de busca em uma árvore AVL.

6. Implemente o seguinte procedimento para $n = \{1, 2, 3, 4, 5, 6, 7, 8, 9, 10\}:

a. Crie $100$ árvores binárias de busca e $100$ árvores AVL com $n$ nós. A sequência de nós deve ser a mesma para ambas as árvores.
b. Calcule a média e desvio-padrão das alturas de cada tipo de árvore.
c. Compare os resultados.

7. Refaça o exercício anterior, mas agora para $n = \{10, 50, 100, 150\}$.

8. Calcule experimentalmente a relação do número de nós com a altura em uma árvore AVL. Adote um método experimental similar ao exercício anterior.
