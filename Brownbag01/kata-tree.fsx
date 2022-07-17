open System.IO

type Tree<'T> =
    | Branch of 'T * Tree<'T> seq
    | Leaf of 'T

type Waypoint = {
    Location: string
    Route: string list
    TotalDistance: int
}

type Connection = {
    Start: string
    Finish: string
    Distance: int
}

let openConnection path =
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
    |> Map.ofList

let rec getLeaves tree =
    match tree with
    | Leaf wp -> [wp]
    | Branch (_,xs) -> xs |> Seq.toList |> List.collect getLeaves

let getPossibleRoutes start finish (connections:Map<string,Connection list>) = 
    let rec processWaypoint waypoint =
        let unvisited =
            connections[waypoint.Location]
            |> List.filter (fun cn -> waypoint.Route |> List.exists (fun loc -> loc = cn.Finish) |> not)
            |> List.map (fun cn -> {
                Location = cn.Finish
                Route = cn.Start :: waypoint.Route
                TotalDistance = waypoint.TotalDistance + cn.Distance
            })
        if waypoint.Location = finish || unvisited = [] then
            Leaf waypoint
        else 
            Branch (waypoint, seq { for next in unvisited do processWaypoint next })
    processWaypoint { Location = start; Route = []; TotalDistance = 0 }
    |> getLeaves
    |> List.filter (fun wp -> wp.Location = finish)

let findShortestRoute start finish =
    Path.Combine(__SOURCE_DIRECTORY__, "resources", "data.csv")
    |> openConnection // load connections
    |> getPossibleRoutes start finish// get posiible routes
    |> List.minBy (fun wp -> wp.TotalDistance)// determine shortest
    |> fun wp -> wp.Location :: wp.Route |> List.rev, wp.TotalDistance// display result

findShortestRoute "Cogburg" "Leverstorm"


