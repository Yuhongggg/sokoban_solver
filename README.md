# sokoban_solver
## Summary
This is a naive sokoban solver, that I wrote in CLisp for a class object in UCLA.
My solver used the total manhatten distance of all blocks to the nearest goal as the heuristic function for A* search.

## How to run
This program can be run with the command:
(a* start-state #'goal-test #'next-states #'heuristic)

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
