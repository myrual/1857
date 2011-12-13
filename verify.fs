module public ve
open System
open jb
open fq
open fx
let NearBollLBOpen hqlist = 
        let latestRecordprice = hqlist |> FindLatestRecord |> GetEnd in
        if (List.length hqlist) < 20 then false
        else
        let currentbolling =  Bolling hqlist in
        let ave = (Average_Boll currentbolling) in
        let lb = GetBollLB currentbolling in
        let underave = (ave - lb)/4.0 + lb
        (latestRecordprice < underave)

let func_CloseWrong openday hqlist rate = 
        let latestRecordprice = hqlist |> FindLatestRecord |> GetEnd in
        let openprice = GetEnd openday in
        let NowWrong = latestRecordprice < openprice in
        let NeedClose = ((openprice - latestRecordprice)/openprice > rate) in
        NowWrong && NeedClose
let func_CloseCorrect openday hqlist n = 
        let latestRecordprice = hqlist |> FindLatestRecord |> GetEnd in
        let topprice = hqlist |> MaxEndpRecord |> GetEnd in
        let openprice = GetEnd openday in
        let benchmarkprice = (topprice - openprice)/2.0 + openprice in
        latestRecordprice < benchmarkprice
let func_Open2NowLessThan openday hqlist n = 
        let open2now = filtaftertime openday hqlist in
        let len_open2now = List.length open2now in
        len_open2now < n
let NearBollLBClose openday hqlist = 
        let wrongcloseBy = func_CloseWrong openday hqlist in
        let open2NowLess = func_Open2NowLessThan openday hqlist in
        let closecorrect = func_CloseCorrect openday hqlist in
        if ((List.length hqlist) < 20) then false
        else if (open2NowLess 3) then false
        else if (wrongcloseBy 0.05) then true
        else if (closecorrect 2.0) then true
        else false
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
let demo_summary openday closeday = 
        let profit = (GetEnd closeday) - (GetEnd openday) in
        (profit, (openday, closeday))

let profitOf = fst

let load_func_close func_close openday hqlist= 
        if (filtaftertime openday hqlist) = [] then false
        else (func_close openday hqlist)

let rec Verify func_open func_close func_summary wholehqlist tofindhqlist = 
        if tofindhqlist = [] then []
        else if wholehqlist = [] then []
        else if (List.exists (fun x -> func_open (filtBeforetime x wholehqlist)) tofindhqlist) then 
                let openday = List.find (fun x -> func_open (filtBeforetime x wholehqlist)) tofindhqlist in
                let this_close = load_func_close func_close openday in
                if (List.exists (fun x-> this_close (filtBeforetime x wholehqlist)) tofindhqlist) then 
                        let closeday = List.find (fun x-> this_close (filtBeforetime x wholehqlist)) tofindhqlist in
                        List.append [(func_summary openday closeday)] (Verify func_open func_close func_summary wholehqlist (filtaftertime closeday tofindhqlist))
                else []
             else []
