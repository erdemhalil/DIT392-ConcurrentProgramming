package amazed.solver;

import amazed.maze.Maze;

import java.util.List;
import java.util.ArrayList;
import java.util.Set;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * <code>ForkJoinSolver</code> implements a solver for
 * <code>Maze</code> objects using a fork/join multi-thread
 * depth-first search.
 * <p>
 * Instances of <code>ForkJoinSolver</code> should be run by a
 * <code>ForkJoinPool</code> object.
 */


public class ForkJoinSolver
    extends SequentialSolver
{
    private static AtomicBoolean found = new AtomicBoolean(false);
    private ArrayList<ForkJoinSolver> threadTracker = new ArrayList<>();

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal.
     *
     * @param maze   the maze to be searched
     */
    public ForkJoinSolver(Maze maze)
    {
        super(maze);
        visited = new ConcurrentSkipListSet<Integer>();
    }

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal, forking after a given number of visited
     * nodes.
     *
     * @param maze        the maze to be searched
     * @param forkAfter   the number of steps (visited nodes) after
     *                    which a parallel task is forked; if
     *                    <code>forkAfter &lt;= 0</code> the solver never
     *                    forks new tasks
     */
    public ForkJoinSolver(Maze maze, int forkAfter)
    {
        this(maze);
        this.forkAfter = forkAfter;
    }

    public ForkJoinSolver(Maze maze, int start, Set<Integer> visited)
    {
        this(maze);
        this.start = start;
        this.visited = visited;
    }

    /**
     * Searches for and returns the path, as a list of node
     * identifiers, that goes from the start node to a goal node in
     * the maze. If such a path cannot be found (because there are no
     * goals, or all goals are unreacheable), the method returns
     * <code>null</code>.
     *
     * @return   the list of node identifiers from the start node to a
     *           goal node in the maze; <code>null</code> if such a path cannot
     *           be found.
     */
    @Override
    public List<Integer> compute()
    {
        return parallelSearch();
    }

    private List<Integer> parallelSearch()
    {
        int current = start;
        int player = maze.newPlayer(current);
        List<Integer> pathResult = null;
        
        // found is an atomic boolean that is looped over until the player reaches the goal node.
        while(!found.get()) {
        
            // When the goal is reached, the boolean is set to true and the traversed path is saved.
            if (maze.hasGoal(current)) {
                found.set(true);
                pathResult = pathFromTo(start, current);                             
                break; 
            }
            
            /*  
                To avoid spawning more threads than needed, we use the current thread to traverse the first neighbor and only fork new threads
                on any other unvisited neighbors.
            */
            int nextMain = 0;
            boolean nextMainFlag = false;
            visited.add(current);
            
            for (int nb: maze.neighbors(current)) {
                if (visited.add(nb)) {
                    predecessor.put(nb, current);
                    if (!nextMainFlag) {
                        nextMain = nb;
                        nextMainFlag = true;
                    } else {
                    // If more than one neighbor is unvisited we fork a new thread for each unvisited neighbor and add these to threadTracker.
                    ForkJoinSolver newThread = new ForkJoinSolver(maze, nb, visited);          
                    threadTracker.add(newThread);          
                    newThread.fork();
                    }
                }
            }
            if (nextMainFlag) {
                maze.move(player, nextMain);
                current = nextMain;
            } else {
                break;
            }

        }   
        
        /*
        Loops over all threads saved to threadTracker, and joins the results from each thread. The result of Each thread is
        used to reconstruct the complete path to the goal.
        */
        for(ForkJoinSolver th: threadTracker) {
            List<Integer> result = th.join();
            if(result != null)
            {
                int first = result.remove(0);
                pathResult = pathFromTo(start, first);
                pathResult.addAll(result);
            }
        }

        return pathResult;
    } 
}