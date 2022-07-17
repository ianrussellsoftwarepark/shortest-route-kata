open System.IO

type Tree<'T> =
    | Branch of 'T * Tree<'T> seq
    | Leaf of 'T

type Connection = {
    Start: string
    Finish: string
    Distance: int
}

type Waypoint = {
    Location: string
    Route: string list
    TotalDistance: int
}

let loadData path =
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
    |> List.groupBy (fun item -> item.Start)
    |> Map.ofList

let rec getLeaves tree =
    match tree with
    | Leaf x -> [ x ]
    | Branch (_,xs) -> xs |> Seq.toList |> List.collect getLeaves

let findPossibleRoutes start finish (connections:Map<string,Connection list>) =
    let rec processWaypoint current =
        let unvisited = 
            connections[current.Location]
            |> List.filter (fun cn -> current.Route |> List.exists (fun loc -> loc = cn.Finish) |> not)
            |> List.map (fun cn -> {
                Location = cn.Finish
                Route = cn.Start :: current.Route
                TotalDistance = cn.Distance + current.TotalDistance })
        if current.Location = finish || unvisited = [] then
            Leaf current
        else
            Branch (current, seq { for wp in unvisited do processWaypoint wp })
    processWaypoint { Location = start; Route = []; TotalDistance = 0 }
    |> getLeaves
    |> List.filter (fun wp -> wp.Location = finish)

let findShortestRoute start finish =
    Path.Combine(__SOURCE_DIRECTORY__, "resources", "data.csv")
    |> loadData
    |> findPossibleRoutes start finish
    |> List.minBy (fun wp -> wp.TotalDistance)
    |> fun wp -> wp.Location :: wp.Route |> List.rev, wp.TotalDistance

findShortestRoute "Cogburg" "Leverstorm"
