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

let TwoBOpenWith70 distance2B today hqlist = 
        let latest70_static =  static_of (hqlist |> List.rev |> (filtLastnday 70) |> List.rev) "demo" in
        let today_price = GetEnd today in
        if latest70_static.Trend = "Bull" then false
        else
        List.head((CalcDown hqlist 1))/today_price < distance2B

let TwoBOpenWith100 distance2B today hqlist = 
        let latest140_static =  static_of (hqlist |> List.rev |> (filtLastnday 140) |> List.rev) "demo" in
        let today_price = GetEnd today in
        if latest140_static.Trend = "Bull" then false
        else
        List.head((CalcDown hqlist 1))/today_price < distance2B
let TwoBOpenWithLastNDays distance2B today hqlist N= 
        let latestNHQ = (hqlist |> List.rev |> (filtLastnday N) |> List.rev) in
        let latestN_static =  static_of (hqlist |> List.rev |> (filtLastnday N) |> List.rev) "demo" in
        let today_price = GetEnd today in
        if latestN_static.Trend = "Bull" then false
        else
        let estimateprice = List.head(CalcDown latestNHQ 1) in
        (estimateprice > today_price) && (estimateprice/today_price < distance2B)

let TwoBOpen_Bear distance2B hqlist = 
        if (List.length hqlist) < 70 then false
        else
        let today = FindLatestRecord hqlist in
        let lastN = TwoBOpenWithLastNDays distance2B today hqlist in
        lastN 70 || lastN 90 || lastN 110 || lastN 130 || lastN 150 || lastN 170 || lastN 190

let BearOpenwrong_NeedClose  openprice latestRecordprice rate = 
        let NowWrong = latestRecordprice > openprice in
        if NowWrong then  (((latestRecordprice - openprice)/openprice) > rate)
        else false
let Func_CloseWrongBy func_WrongNeedClose openday hqlist rate = 
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


let Func_Open2NowLessThan openday hqlist n = 
        let open2now = filtaftertime openday hqlist in
        let len_open2now = List.length open2now in
        len_open2now < n
let Func_CloseCorrectBy func_CorrectNeedClose openday allhqlist minUP n= 
        let hqlist = filtaftertime openday allhqlist in
        let openprice = GetEnd openday in
        func_CorrectNeedClose openprice hqlist minUP n


let BullOpenwrong_NeedClose openprice latestRecordprice rate = 
        let NowWrong = latestRecordprice < openprice in
        if NowWrong then  (((openprice - latestRecordprice)/openprice) > rate)
        else false

let General_Close func_CloseWrong func_CloseCorrect stoploss stopearn openday hqlist =
        let wrongcloseBy = Func_CloseWrongBy func_CloseWrong openday hqlist in
        let open2NowLess = Func_Open2NowLessThan openday hqlist in
        let closecorrect = Func_CloseCorrectBy func_CloseCorrect openday hqlist stoploss in
        if ((List.length hqlist) < 20) then false
        else if (open2NowLess 3) then false
        else if (wrongcloseBy stoploss) then true
        else if (closecorrect stopearn) then true
        else false

let NearBollLBClose = General_Close BullOpenwrong_NeedClose BullOpenCorrect_NeedClose 
let NearBollUBClose = General_Close BearOpenwrong_NeedClose BearOpenCorrect_NeedClose

let TwoBClose_Bear  = General_Close BearOpenwrong_NeedClose BearOpenCorrect_NeedClose
let TwoBClose_Bull  = General_Close BullOpenwrong_NeedClose BullOpenCorrect_NeedClose

let Demo_summary openday closeday = 
        let profit = (GetEnd closeday) - (GetEnd openday) in
        (profit, (openday, closeday))
let Demo_summary_bear openday closeday = 
        let profit = (GetEnd openday) - (GetEnd closeday) in
        (profit, (openday, closeday))

let Load_func_close func_close openday hqlist= 
        if (filtaftertime openday hqlist) = [] then false
        else (func_close openday hqlist)

let FoundBy this_func wholehqlist tofindhqlist =  List.exists (fun x-> this_func (filtBeforetime x wholehqlist)) tofindhqlist
let GetBy   this_func wholehqlist tofindhqlist =  List.find   (fun x-> this_func (filtBeforetime x wholehqlist)) tofindhqlist
let rec VerifyByTwoList func_open func_close func_summary wholehqlist tofindhqlist = 
        if tofindhqlist = [] then []
        else if wholehqlist = [] then []
        else if (FoundBy func_open wholehqlist tofindhqlist) then 
                let openday = GetBy func_open wholehqlist tofindhqlist in
                let today = FindLatestRecord wholehqlist in
                let this_close = Load_func_close func_close openday in
                if (FoundBy this_close wholehqlist tofindhqlist) then 
                        let closeday = (GetBy this_close wholehqlist tofindhqlist) in
                        List.append [(func_summary openday closeday)] (VerifyByTwoList func_open func_close func_summary wholehqlist (filtaftertime closeday tofindhqlist))
                else [(func_summary openday today)]
             else []
let Verify func_open func_close func_summary wholehqlist = VerifyByTwoList func_open func_close func_summary (ForcetoNatureHQlist wholehqlist) (ForcetoNatureHQlist wholehqlist)
let ExplainResult (tradelist : ((float * 'a) list)) = 
        let earn = List.filter (fun x-> (fst x) > 0.0) tradelist in
        let loss = List.filter (fun x-> (fst x) < 0.0) tradelist in
        let earntime = List.length earn in
        let losstime = List.length loss in
        let totalearn = List.sumBy fst earn in
        let totalloss = List.sumBy fst loss in
        ((totalearn, earntime), (totalloss, losstime))
let Bollverify loss win = Verify NearBollLBOpen (NearBollLBClose  loss win) Demo_summary
let BollverifyBear loss win = Verify NearBollUBOpen (NearBollUBClose  loss win) Demo_summary_bear
let TwoBverifyBear distance2B loss win = Verify (TwoBOpen_Bear distance2B) (TwoBClose_Bear loss win) Demo_summary_bear
let SHbyTime startt endt = FQHQInTime "SH000001" startt endt
let SHBollInShort loss win startt endt = Bollverify loss win ((SHbyTime startt endt) |> OneFourS |> List.rev)
let SHBollIn loss win startt endt = Bollverify loss win ((SHbyTime startt endt) |> List.rev)
let SHBollInShortBear loss win startt endt = BollverifyBear loss win ((SHbyTime startt endt) |> OneFourS |> List.rev)
let SHBollInBear loss win startt endt = BollverifyBear loss win ((SHbyTime startt endt) |> List.rev)
let SHTwoBBear distance2B loss win startt endt = TwoBverifyBear distance2B loss win ((SHbyTime startt endt) |> List.rev)
let compare_matrix hqlist matrix = List.map (fun x-> ExplainResult (Bollverify (fst x) (snd x) hqlist)) matrix
