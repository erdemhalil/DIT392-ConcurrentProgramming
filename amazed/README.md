## Parallelism

To make the sequential algorithm take advantage of parallelism, instead of tracking the neighboring nodes in a stack and traversing these nodes in a LIFO manner, we decided to design our solution to spawn a new thread for each unvisited neighboring node. 

Each of these new threads would spawn new threads for any unvisited neighboring nodes encountered.
To make the algorithm not spawn more threads than necessary, we allowed the currently executing thread to continue exploring one of the neighbors while only spawning new threads
if more than one neighboring node is unvisited.

## Data Structures

All spawned (forked) threads are stored in a list waiting to be joined later on.

With our solution, the shared thread-safe datastructure is the set of visited nodes since we would not want to visit a node multiple times due to performance issues.

The map which contains the predecessor nodes does not need to be shared as the path is unique for every spawned thread.

We decided not to use the stack of neighbouring nodes (frontier) as it would be just an overhead. We already can get all the neighbouring nodes with a function and looping through them once is enough.

To ensure that threads stop working when a solution is find, we make use of an Atomic Boolean. We loop until that atomic boolean is set to true which happens only when a goal is found.

## Joining

Joining is done when the thread has finished its job which happens either if a goal is found, or there is nothing left to explore (no unvisited neighbouring node).

Joining is done by going through all the threads spawned from that particular one. In case one of these threads has found the goal, we return the path from start to goal.

## Path Reconstruction

To reconstruct the path we "stitch" the paths from the parent and child threads (start to child node + child to goal node).

## Running the solution

Compile our solution and run it with the tests using make:
```
make compile
```

Run our solution on small or medium map, with forkAfter equal to 3 or to 9:
```
make parallel_small_step3
make parallel_small_step9

make parallel_medium_step3
make parallel_medium_step9
```

If you do not have make, you can run the examples from the command line after compiling all the project’s source files:
```
java -cp src/main amazed.Main maps/MAP.map parallel-N
```
where MAP is small or medium for each of the maps, and N is a positive integer corresponding to the chosen value of forkAfter.

To run the the sequential (depth-first) version of the search:

```
make sequential_small
```