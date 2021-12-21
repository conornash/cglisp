# cglisp: Conway's Game of Life (CGL) in Emacs Lisp

A simple implementation of Conway's Game of Life (CGL) for Emacs, written in Emacs Lisp.

## Installation

Requires Emacs 24.1 or greater.

- Download src/cglisp.el
- Load it interactively with  ```M-x load-file PATH/TO/cglisp.el``` 
  - or if you *really* like CGL, by adding ```(load PATH/TO/cglisp.el)``` in your ``.emacs``
#### Board size
The game board is a square of edge size 40. This is defined at top of cglisp.el and can be changed with ```(setf cgl-game-size SIZE)```

## Usage

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


### Earth mode
By default, there is nothing out of the game board: cells on edges and corner thus have fewer neighbouring cells (i.e. only 3 for corners, and 5 for edges). Toggling Earth mode makes the board connected with itself: the top edge's top is the bottom edge, and the left edge's left is the right edge. Thus the board does not have any edge in this mode.

Toggle earth mode by pressing ``e`` during the game.

