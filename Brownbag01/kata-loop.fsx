open System.IO

type Waypoint = {
    Location: string
    Route: string list
    TotalDistance: int
}

type Connection = {
    Start:string
    Finish: string
    Distance: int
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
    |> Map.ofList

let getUnvisited current connections =
    connections
    |> List.filter (fun cn -> current.Route |> List.exists (fun loc -> loc = cn.Finish) |> not)
    |> List.map (fun cn -> {
        Location = cn.Finish
        Route = cn.Start :: current.Route
        TotalDistance = current.TotalDistance + cn.Distance
    })

let findShortestRoute start finish (connections:Map<string,Connection list>) =
    let rec loop (possibles:Waypoint list) (shortest:Waypoint option) count = 
        // get next routes and split into thoise that have reached destination and those that haven't
        let (ended, potential) = 
            [
                for current in possibles do
                    let unvisited = getUnvisited current connections[current.Location]
                    for wp in unvisited do wp
            ]
            |> List.partition (fun wp -> wp.Location = finish)
        // find shortest from latest iteration
        let shortEnded = 
            match ended with
            | [] -> None
            | _ -> ended |> List.minBy (fun wp -> wp.TotalDistance) |> Some
        // compare current shortest with the one from latest iteration
        let currentShortest = 
            match shortest, shortEnded with
            | None, None -> None
            | None, Some se -> Some se
            | Some s, None -> Some s
            | Some s, Some se -> if s.TotalDistance < se.TotalDistance then Some s else Some se
        // filter out routes where the total distance is already greater than shortest route
        let stillInPlay = 
            match currentShortest with
            | None -> potential
            | Some cs -> potential |> List.filter (fun wp -> wp.TotalDistance < cs.TotalDistance)
        // either return shortest if nothing left to process or do another iteration
        match stillInPlay with
        | [] -> currentShortest, count
        | _ -> loop stillInPlay currentShortest (count + 1)
    loop [{ Location = start; Route = []; TotalDistance = 0 }] None 1 

let getShortestRoute start finish =
    Path.Combine(__SOURCE_DIRECTORY__, "resources", "data.csv")
    |> loadConnections // load connections
    |> findShortestRoute start finish // find possible routes
    |> function
        | None, _ -> failwith "No shortest route found"
        | Some wp, count -> wp.Location :: wp.Route |> List.rev, wp.TotalDistance, count // output data

getShortestRoute "Cogburg" "Leverstorm"


