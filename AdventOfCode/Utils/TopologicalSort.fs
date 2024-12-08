module AdventOfCode.Utils.TopologicalSort

/// Represents the visitation state of a node:
/// - White: Not visited.
/// - Gray: Visited but not fully processed (currently in recursion stack).
/// - Black: Fully processed.
type NodeColor = 
    | White
    | Gray
    | Black

/// Performs a topological sort on a graph represented by a map of nodes to their successors.
/// Returns a list of nodes in topologically sorted order if the graph is acyclic.
/// Throws an exception if a cycle is detected.
let topologicalSort (graph : Map<'T, 'T list>) : 'T list =
    let mutable result = []
    // Track visitation state of each node
    let colors = System.Collections.Generic.Dictionary<'T, NodeColor>()
    
    // Initialize all nodes as White (unvisited)
    for kvp in graph do
        colors.[kvp.Key] <- White
    
    // A recursive DFS function to visit each node
    let rec visit node =
        match colors.[node] with
        | Gray ->
            // A gray node means we've hit a node that is currently in the recursion stack,
            // indicating a cycle.
            failwithf "Cycle detected at node: %A" node
        | Black -> 
            // Already processed fully; do nothing
            ()
        | White ->
            // Mark the node as being visited
            colors.[node] <- Gray
            // Visit all successors
            match graph.TryFind node with
            | Some successors ->
                for s in successors do
                    visit s
            | None -> ()
            // Mark node as fully processed
            colors.[node] <- Black
            // Prepend to result list (reversal at end not strictly needed if we prepend)
            result <- node :: result

    // Ensure that all nodes are visited – if there are disconnected parts of the graph,
    // we want to sort them as well.
    for kvp in graph do
        if colors.[kvp.Key] = White then
            visit kvp.Key

    // The result was built in reverse order due to the prepend; reverse it for final order.
    List.rev result