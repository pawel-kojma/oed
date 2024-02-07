# Oed

Text editor written in Ocaml with ncurses bindings. Primary goal was to use functional programming techniques in practice like monads or GADT.
Editor still lacks a lot of features like line-wrap, displaying utf-8 characters, selection and more.<br/>Wouldn't use it to edit important files.

## Installation
You can either build it from source or install as opam package. In order to build from source, you need to have `dune` installed.
### opam package
```bash
$ opam install .
```
### source
Executable should be under `./_build/default/bin/main.exe`
```bash
$ opam install curses
$ dune build @install
```
## Usage
Editor supports `Normal` (N) and `Insert` (I) mode with following keybinds: <br/>
`Shift-s` (N) - save file<br/>
`Shift-q` (N) - quit<br/>
`Shift-i` and `Shift-a` (N) - go to start/end of a line <br/>
`u` and `r` (N) - undo and redo edition history<br/>
`Esc` and `i` (N) - enter insert mode<br/>
`Esc` (I) - enter normal mode<br/>

