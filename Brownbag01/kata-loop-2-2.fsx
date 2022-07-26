open System.IO

type Waypoint = {
    Location: string
    Route: (string * float) list
    TotalDuration: float
}

type Connection = {
    Start:string
    Finish: string
    Duration: float
}

let loadConnections path =
    path
    |> File.ReadAllLines
    |> Array.skip 1
    |> fun rows -> [
        for row in rows do
            match row.Split(",") with
            | [|start;finish;distance;speed|] ->
                let duration = float distance / float speed
                { Start = start; Finish = finish; Duration = duration }
                { Start = finish; Finish = start; Duration = duration }
            | _ -> failwith $"{row} is invalid"
    ]
    |> List.groupBy (fun cn -> cn.Start)
    |> Map.ofList

let getUnvisited current connections =
    connections
    |> List.filter (fun cn -> current.Route |> List.exists (fun (loc, _) -> loc = cn.Finish) |> not)
    |> List.map (fun cn -> {
        Location = cn.Finish
        Route = (cn.Start, current.TotalDuration) :: current.Route
        TotalDuration = current.TotalDuration + cn.Duration
    })

let findFastestRoute start finish (connections:Map<string,Connection list>) =
    let rec loop (possibles:Waypoint list) (currentFastest:Waypoint option) = 
        // get next routes and split into those that have reached destination and those that haven't
        let (ended, potential) = 
            [
                for current in possibles do
                    let unvisited = getUnvisited current connections[current.Location]
                    for wp in unvisited do wp
            ]
            |> List.partition (fun wp -> wp.Location = finish)
        // find fastest from latest iteration
        let fastestEnded = 
            match ended with
            | [] -> None
            | _ -> ended |> List.minBy (fun wp -> wp.TotalDuration) |> Some
        // compare current fastest with the one from latest iteration
        let fastestRoute = 
            match currentFastest, fastestEnded with
            | None, None -> None
            | None, Some se -> Some se
            | Some s, None -> Some s
            | Some s, Some se -> if s.TotalDuration < se.TotalDuration then Some s else Some se
        // filter out routes where the total duration is already greater than fastest route
        let stillInPlay = 
            match fastestRoute with
            | None -> potential
            | Some cs -> potential |> List.filter (fun wp -> wp.TotalDuration < cs.TotalDuration)
        // either return fastest if no routes left to process or do another iteration
        match stillInPlay with
        | [] -> fastestRoute
        | _ -> loop stillInPlay fastestRoute
    loop [{ Location = start; Route = []; TotalDuration = 0 }] None

let getFastestRoute start finish =
    Path.Combine(__SOURCE_DIRECTORY__, "resources", "data2.csv")
    |> loadConnections // load connections
    |> findFastestRoute start finish 
    |> function
        | None -> failwith "No fastest route found"
        | Some wp -> 
            (wp.Location, wp.TotalDuration) :: wp.Route 
            |> List.rev
            |> List.tail
            |> List.fold (fun acc (loc,time) -> acc + "\n" + $"%.2f{time}h  ARRIVE  {loc}") ($"00.00h  DEPART  {start}") 
    |> printfn "%A"

getFastestRoute "Steamdrift" "Leverstorm" //"Cogburg"


