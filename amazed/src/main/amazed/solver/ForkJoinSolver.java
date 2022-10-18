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
        List<Integer> toReturn = null;
        
        while(!found.get()) {
        
            if (maze.hasGoal(current)) {
                found.set(true);
                toReturn = pathFromTo(start, current);                             
                break; 
            }
            
            int nextMain = 0;
            boolean nextMainFlag = false;
            visited.add(current);
            
            for (int nb: maze.neighbors(current)) {
                if (!visited.contains(nb)) {
                    visited.add(nb);
                    predecessor.put(nb, current);
                    if (!nextMainFlag) {
                        nextMain = nb;
                        nextMainFlag = true;
                    } else {
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
        
        for(ForkJoinSolver f: threadTracker) {
            List<Integer> result = f.join();
            if(result != null)
            {
                int first = result.remove(0);
                toReturn = pathFromTo(start, first);
                toReturn.addAll(result);
            }
        }

        return toReturn;
    } 
}