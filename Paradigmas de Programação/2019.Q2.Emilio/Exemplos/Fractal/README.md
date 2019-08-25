# Mandelbrot em Haskell

Versão sequencial e paralela.

Para compilar:

``` shell
$ stack build
```
Para executar a versão sequencial:

``` shell
$ stack exec fractals 1024
```

Onde 1024x1024 é a resolução para a imagem gerada (por padrão em `mandel.ppm`).

Para executar a versão paralela:


``` shell
$ stack exec fractalp 1024 16
```
Onde 1024 (1024x1024) é a resolução da imagem gerada e 16 é o número de threads.
