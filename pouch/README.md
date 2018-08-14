※ nekonoshiri/pouch から nekonoshiri/sicilia/pouch へと移動したため以下のままではインストールできない

Pouch is a nano-size command line application to push/pop items.

Pouch はアイテムを push/pop するだけのコマンドラインアプリケーションです．

## Installation: インストール

### case 1. using Glide: Glide を使う場合

[Glide](https://github.com/Masterminds/glide) is a go package manager.

[Glide](https://github.com/Masterminds/glide) は go のパッケージマネージャです．

```
$ mkdir -p $GOPATH/src/github.com/nekonoshiri; cd $_
$ git clone https://github.com/nekonoshiri/pouch
$ cd pouch
$ glide install -v
$ go install
```

### case 2. using `go get`: `go get` を使う場合

```
$ go get github.com/nekonoshiri/pouch
```

## Usage: 使い方

```
$ pouch add I am serval

$ pouch list
3 item(s) in pouch
[1] I
[2] am
[3] serval

$ pouch pop 3
pop: [3] serval

$ pouch clean; pouch list
0 item(s) in pouch
```

## TODO: やりたいこと

- pouch pop --eval

- configfile の使い方とか　生成コマンドの実装
