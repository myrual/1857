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
        if NowWrong then  (((openprice - latestRecordprice)/openprice) > rate)
        else false
let func_CloseCorrect openday allhqlist minUP n= 
        let hqlist = filtaftertime openday allhqlist in
        let latestRecordprice = hqlist |> FindLatestRecord |> GetEnd in
        let topprice = hqlist |> MaxEndpRecord |> GetEnd in
        let openprice = GetEnd openday in
        let benchmarkprice = (topprice - openprice)/n + openprice in
        (latestRecordprice < benchmarkprice) && ((topprice - openprice)/openprice > minUP)
let func_Open2NowLessThan openday hqlist n = 
        let open2now = filtaftertime openday hqlist in
        let len_open2now = List.length open2now in
        len_open2now < n
let NearBollLBClose stoploss stopearn openday hqlist = 
        let wrongcloseBy = func_CloseWrong openday hqlist in
        let open2NowLess = func_Open2NowLessThan openday hqlist in
        let closecorrect = func_CloseCorrect openday hqlist stoploss in
        if ((List.length hqlist) < 20) then false
        else if (open2NowLess 3) then false
        else if (wrongcloseBy stoploss) then true
        else if (closecorrect stopearn) then true
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

let FoundBy this_func wholehqlist tofindhqlist =  List.exists (fun x-> this_func (filtBeforetime x wholehqlist)) tofindhqlist
let GetBy   this_func wholehqlist tofindhqlist =  List.find   (fun x-> this_func (filtBeforetime x wholehqlist)) tofindhqlist
let rec VerifyByTwoList func_open func_close func_summary wholehqlist tofindhqlist = 
        if tofindhqlist = [] then []
        else if wholehqlist = [] then []
        else if (FoundBy func_open wholehqlist tofindhqlist) then 
                let openday = GetBy func_open wholehqlist tofindhqlist in
                let this_close = load_func_close func_close openday in
                if (FoundBy this_close wholehqlist tofindhqlist) then 
                        let closeday = (GetBy this_close wholehqlist tofindhqlist) in
                        List.append [(func_summary openday closeday)] (VerifyByTwoList func_open func_close func_summary wholehqlist (filtaftertime closeday tofindhqlist))
                else []
             else []
let Verify func_open func_close func_summary wholehqlist = VerifyByTwoList func_open func_close func_summary wholehqlist wholehqlist
let explainResult (tradelist : ((float * 'a) list)) = 
        let earn = List.filter (fun x-> (fst x) > 0.0) tradelist in
        let loss = List.filter (fun x-> (fst x) < 0.0) tradelist in
        let earntime = List.length earn in
        let losstime = List.length loss in
        let totalearn = List.sumBy fst earn in
        let totalloss = List.sumBy fst loss in
        ((totalearn, earntime), (totalloss, losstime))
let bollverify loss win = Verify NearBollLBOpen (NearBollLBClose  loss win) demo_summary
let compare_matrix hqlist matrix = List.map (fun x-> explainResult (bollverify (fst x) (snd x) hqlist)) matrix
