# cglisp: Conway's Game of Life (CGL) in Emacs Lisp

A simple implementation of Conway's Game of Life (CGL) for Emacs, written in Emacs Lisp.

## This project is not stable yet
It doesn't work properly at this time

## Installation

Requires Emacs 24.1 or greater.

- Download src/cglisp.el
- Load it interactively with  ```M-x load / PATH/TO/cglisp.el``` 
  - or if you really like it, by adding ```(load PATH/TO/cglisp.el)``` in your ``.emacs``
#### Board size
The game board is a square of edge size 50. This is defined at top of cglisp.el and can be changed with ```(setf cgl-game-size SIZE)```

## Usage

### Start a game
``M-x cgl-start`` will open a new editable buffer in which you can write an initial CGL state.

You can then describe an initial state with spaces and "o"s, and run it step by step by pressing ``s``, or restarting by pressing ``r``.

Run automatically by pressing ``g`` -- a step will be made every 0.1 second (defined by variable ``cgl--game-speed``).

Pause it by pressing ``g`` again.

Quit by pressing ``q``.

### Describe an initial state
Live cells are noted "o", dead/empty cells " " (whitespace). Newlines are used to get to the next row of the state. An example would look like this

```
oo oo

o o o o o o


ooo
```
Exceeding ``cgl-game-size`` on a line, or using any other character than "o" or whitespace, will yield an error.

### Run a single step
``M-x cgl-step`` will make the game go through one step. It will use the state currently described in the game buffer, compute the next state and write it back in the game buffer, overwriting the previous state. In the former example, it would update this way:
```

o oooo


 o
 o
 o
```
