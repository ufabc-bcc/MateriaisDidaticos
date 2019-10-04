# Simples análise e plotagem de dados

Este mini-projeto mostra todos os elementos básicos que você precisará para carregar, processsar e plotar dados contidos em um arquivo texto. 

Neste projeto utilizamos a biblioteca [Vega](https://vega.github.io/vega/) para geração de gráficos.

## Preliminares

Para iniciar compile e execute o código:

```
> stack build
> stack run
```
Após a execução será gerado um arquivo `temperaturas_anuais.html`. Abra no browser de sua preferência para ver o gráfico gerado.

Em seguida leia o código e entenda o que está ocorrendo em cada uma das funções. O código está cheio de comentários mas não deixe de perguntar ao professor caso tenha alguma dúvida


## Exercícios

- Note que as barras de erros são todas iguais. Corrija a implementação da função `desvioPadrao` para que ela devolva os valores corretos.

- Crie um novo gráfico que mostre umidades relativas e pressões médias agrupadas por décadas.

- A maneira que a média anual está sendo calculada não é muito precisa. Se vocês verificarem os arquivos de entrada perceberão que há dias que tem mais medidas que outros (em diversos horários), há dias cuja a medida de temperatura está faltando, há anos inclompletos (2019, por exemplo), etc... Uma possível melhoria seria o cálculo das médias diárias, em seguida mensais e só então as anuais. crie um novo gráfico de temperaturas (semelhante ao que foi dado) utilizando esta ideia. Para o cálculo das médias diárias utilize uma média aritmética simples e para combinar as médias diárias em mensais e a as mensais em anuais, utilize a média geométrica.
