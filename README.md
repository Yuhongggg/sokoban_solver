# sokoban_solver
## Summary
This is a naive sokoban solver, that I wrote in CLisp for a class object in UCLA.
My solver used the total manhatten distance of all blocks to the nearest goal as the heuristic function for A* search.

## How to run
This program can be run with the command:

(a* start-state #'goal-test #'next-states #'heuristic)

The start state is something like:

(setq s1 '( 
            
            (1 1 1 1 1) 
            (1 0 0 4 1)
            (1 0 2 0 1) 
            (1 0 3 0 1) 
            (1 0 0 0 1) 
            (1 1 1 1 1)
))

There are 22 sokoban problems included in the program, you can try them by replacing the start-state with s1 - s22.

The solution can be visualized by this command:

(printstates (a* start-state #'goal-test #'next-states #'heuristic) 0.2).

The last parameter 0.2, specifies how long it waits to print the next state.

## Mapping of square contents
| Content        | Interger           | ASCII  |
| ------------- |:-------------:| -----:|
| Blank       | 0       |   ' ' (white space) |
| Wall        | 1       |   '#' |
| Box         | 2       |   '$' |
| Keeper      | 3       |   '@' |
| Goal        | 4       |   '.' |
| Box+Goal    | 5       |   '*' |
| Keeper+Goal | 6       |   '+' |
