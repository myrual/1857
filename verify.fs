module public ve
open System
open jb
open fq
open fx


let CalEndByHour (x: DayRecord) hour =
        if (hour < 11.0) then x.start
        else if (hour > 14.5) then x.endp
        else if (hour < 12.0) then ((x.endp-x.start)/2.0 + x.start)
        else (3.0 * (x.endp-x.start) / 4.0 + x.start)
let CopyWithHour (x:DayRecord) hour= 
        { time = x.time.AddHours(hour);
            start = x.start;
            high = x.high;
            low = x.low;
            endp = (CalEndByHour x hour);
            volume = x.volume;
            amount = x.amount;
        }
let OneFour (x) = [(CopyWithHour x 10.5); (CopyWithHour x 11.5); (CopyWithHour x 14.0); (CopyWithHour x 15.0)]
let rec OneFourS (hqlist) = 
        if hqlist = [] then []
        else
        let hd = List.head hqlist in
        let tl = List.tail hqlist in
        List.append (OneFour hd) (OneFourS tl)

let LBOpen latestRecordprice currentbolling = 
        let ave = (Average_Boll currentbolling) in
        let lb = GetBollLB currentbolling in
        let underave = (ave - lb)/4.0 + lb
        (latestRecordprice < underave)

let UBOpen latestRecordprice currentbolling = 
        let ave = (Average_Boll currentbolling) in
        let ub = GetBollUB currentbolling in
        let OnWave = (ub - ave) * 0.5 + ave
        (latestRecordprice > OnWave)

let NearBollOpenWith func_open hqlist = 
        if (List.length hqlist) < 20 then false
        else
        let latestRecordprice = hqlist |> FindLatestRecord |> GetEnd in
        let currentbolling =  Bolling hqlist in
        func_open latestRecordprice currentbolling

let NearBollLBOpen = NearBollOpenWith LBOpen
let NearBollUBOpen = NearBollOpenWith UBOpen

let TwoBOpen_Bear hqlist = 
        if (List.length hqlist) < 20 then false
        else
        let latestRecordprice = hqlist |> FindLatestRecord |> GetEnd in
        let currentbolling =  Bolling hqlist in
        let ave = (Average_Boll currentbolling) in
        let lb = GetBollLB currentbolling in
        let underave = (ave - lb)/4.0 + lb
        (latestRecordprice < underave)
let BearOpenwrong_NeedClose  openprice latestRecordprice rate = 
        let NowWrong = latestRecordprice > openprice in
        if NowWrong then  (((latestRecordprice - openprice)/openprice) > rate)
        else false
let func_CloseWrongBy func_WrongNeedClose openday hqlist rate = 
        let latestRecordprice = hqlist |> FindLatestRecord |> GetEnd in
        let openprice = GetEnd openday in
        func_WrongNeedClose openprice latestRecordprice rate
let BearOpenCorrect_NeedClose openprice hqlist minUP n= 
        let latestRecordprice = hqlist |> FindLatestRecord |> GetEnd in
        let bottom_price = hqlist |> MinPriceRecord |> GetEnd in
        let benchmarkprice = bottom_price + (openprice - bottom_price)/n  in
        (latestRecordprice > benchmarkprice) && ((openprice - bottom_price)/bottom_price > minUP)

let BullOpenCorrect_NeedClose openprice hqlist minUP n = 
        let latestRecordprice = hqlist |> FindLatestRecord |> GetEnd in
        let top_price = hqlist |> MaxEndpRecord |> GetEnd in
        let benchmarkprice = (top_price - openprice)/n + openprice in
        (latestRecordprice < benchmarkprice) && ((top_price - openprice)/openprice > minUP)


let func_Open2NowLessThan openday hqlist n = 
        let open2now = filtaftertime openday hqlist in
        let len_open2now = List.length open2now in
        len_open2now < n
let func_CloseCorrectBy func_CorrectNeedClose openday allhqlist minUP n= 
        let hqlist = filtaftertime openday allhqlist in
        let openprice = GetEnd openday in
        func_CorrectNeedClose openprice hqlist minUP n

let TwoBClose_Bear stoploss stopearn openday hqlist = 
        let wrongcloseBy = func_CloseWrongBy BearOpenwrong_NeedClose openday hqlist in
        let open2NowLess = func_Open2NowLessThan openday hqlist in
        let closecorrect = func_CloseCorrectBy BearOpenCorrect_NeedClose openday hqlist stoploss in
        if ((List.length hqlist) < 20) then false
        else if (open2NowLess 3) then false
        else if (wrongcloseBy stoploss) then true
        else if (closecorrect stopearn) then true
        else false

let BullOpenwrong_NeedClose openprice latestRecordprice rate = 
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



let General_Close func_CloseWrong func_CloseCorrect stoploss stopearn openday hqlist =
        let wrongcloseBy = func_CloseWrongBy func_CloseWrong openday hqlist in
        let open2NowLess = func_Open2NowLessThan openday hqlist in
        let closecorrect = func_CloseCorrectBy func_CloseCorrect openday hqlist stoploss in
        if ((List.length hqlist) < 20) then false
        else if (open2NowLess 3) then false
        else if (wrongcloseBy stoploss) then true
        else if (closecorrect stopearn) then true
        else false
let NearBollLBClose = General_Close BullOpenwrong_NeedClose BullOpenCorrect_NeedClose 
let NearBollUBClose = General_Close BearOpenwrong_NeedClose BearOpenCorrect_NeedClose

let demo_summary openday closeday = 
        let profit = (GetEnd closeday) - (GetEnd openday) in
        (profit, (openday, closeday))
let demo_summary_bear openday closeday = 
        let profit = (GetEnd openday) - (GetEnd closeday) in
        (profit, (openday, closeday))

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
let bollverifyBear loss win = Verify NearBollUBOpen (NearBollUBClose  loss win) demo_summary_bear
let SHbyTime startt endt = FQHQInTime "SH000001" startt endt
let SHBollInShort loss win startt endt = bollverify loss win ((SHbyTime startt endt) |> OneFourS |> List.rev)
let SHBollIn loss win startt endt = bollverify loss win ((SHbyTime startt endt) |> List.rev)
let SHBollInShortBear loss win startt endt = bollverifyBear loss win ((SHbyTime startt endt) |> OneFourS |> List.rev)
let SHBollInBear loss win startt endt = bollverifyBear loss win ((SHbyTime startt endt) |> List.rev)
let compare_matrix hqlist matrix = List.map (fun x-> explainResult (bollverify (fst x) (snd x) hqlist)) matrix
