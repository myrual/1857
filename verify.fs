module public ve
open System
open jb
open fq
open fx
let demo_open hqlist = 
        if hqlist = [] then false
        else
        let lastday = hqlist |> List.rev |> List.head in
        GetEnd lastday < 2390.0
let demo_close openday hqlist = 
        let openprice = GetEnd openday in
        let lastday = hqlist |> List.rev |> List.head in
        let lastday_price = GetEnd lastday in
        (lastday_price - openprice) > 20.0
let load_func_close func_close openday hqlist= 
        if (filtaftertime openday hqlist) = [] then false
        else (func_close openday hqlist)

let rec Verify func_open func_close wholehqlist tofindhqlist = 
        if tofindhqlist = [] then []
        else if wholehqlist = [] then []
        else if (List.exists (fun x -> func_open (filtBeforetime x wholehqlist)) tofindhqlist) then 
                let openday = List.find (fun x -> func_open (filtBeforetime x wholehqlist)) tofindhqlist in
                let this_close = load_func_close func_close openday in
                if (List.exists (fun x-> this_close (filtBeforetime x wholehqlist)) tofindhqlist) then 
                        let closeday = List.find (fun x-> this_close (filtBeforetime x wholehqlist)) tofindhqlist in
                        List.append [openday; closeday] (Verify func_open func_close wholehqlist (filtaftertime closeday tofindhqlist))
                else List.append [openday] []
             else []
