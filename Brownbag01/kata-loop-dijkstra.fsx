open System.IO

type Connection = {
    Start: string
    Finish: string
    Distance: int
}

type Waypoint = {
    Location: string
    Visited: string list
    TotalDistance: int
}

let loadConnections path =
    path
    |> File.ReadAllLines
    |> Array.skip 1
    |> fun rows -> [
        for row in rows do
            match row.Split(",") with
            | [|start;finish;distance|] ->
                { Start = start; Finish = finish; Distance = int distance }
                { Start = finish; Finish = start; Distance = int distance }
            | _ -> failwith $"{row} is invalid"
    ]
    |> List.groupBy (fun cn -> cn.Start)
    |> List.map (fun (loc, conns) -> loc, conns |> List.map (fun cn -> cn.Finish, cn.Distance) |> Map.ofList)
    |> Map.ofList

let calculateShortestRoute start finish (connections:Map<string,Map<string,int>>) =
    let rec searchForShortestPath current accMap =
        let visitDestinations state =
            (state, connections[current.Location])
            ||> Map.fold
                (fun acc destination distance ->
                    let newWaypoint = {
                        Location = destination
                        Visited = destination :: current.Visited
                        TotalDistance = current.TotalDistance + distance}
                    searchForShortestPath newWaypoint acc)
        match Map.tryFind current.Location accMap with
        | None -> 
            accMap |> Map.add current.Location (current.TotalDistance, current.Visited) |> visitDestinations
        | Some (shortestKnownPath, _) ->
            if current.TotalDistance < shortestKnownPath then
                accMap |> Map.add current.Location (current.TotalDistance, current.Visited) |> visitDestinations
            else 
                accMap
    searchForShortestPath {Location = start; Visited = []; TotalDistance = 0} Map.empty
    |> fun shortestPaths -> shortestPaths[finish]

let getShortestDistance start finish =
    Path.Combine(__SOURCE_DIRECTORY__, "resources", "data.csv")
    |> loadConnections
    |> calculateShortestRoute start finish
    |> fun (distance, route) -> distance, start::(route |> List.rev)
    |> printfn "%A"

getShortestDistance "Cogburg" "Leverstorm"
