module public jb
open FinData;; 
open System;;

type DayRecord = 
    { time:System.DateTime;
        start:double;
        high:double;
        low:double;
        endp:double;
        volume:double;
        amount:double
    }
let dzh = FinData.FxjData();;

let GStart (x:string[,]) n = System.Convert.ToDouble(x.GetValue(n, 2));;
let Ghigh (x:string[,]) n =  System.Convert.ToDouble(x.GetValue(n,3));;
let Glow (x:string[,]) n = System.Convert.ToDouble(x.GetValue(n,4));;
let Gend (x:string[,]) n = System.Convert.ToDouble(x.GetValue(n,5));;
let Gvolume (x:string[,]) n = System.Convert.ToDouble(string(x.GetValue(n,6)));;
let Gamount (x:string[,]) n = System.Convert.ToDouble(string(x.GetValue(n,7)));;
let GTime (x:string[,]) n = System.DateTime.Parse(string(x.GetValue(n, 1)));;
let OneRow (x:string[,]) n = {time = GTime x n;
             start = GStart x n;
             high = Ghigh x n;
             low = Glow x n;
             endp = Gend x n;
             volume = Gvolume x n;
             amount = Gamount x n
}
let Startof (x : DayRecord) = x.start
let Highof (x : DayRecord) = x.high
let Lowof (x : DayRecord) = x.low
let Endpof (x : DayRecord) = x.endp
let volumeof (x : DayRecord) = x.volume
let amountof (x : DayRecord) = x.amount
let rec Array2List inarray n = 
        if n < 0 then []
        else List.append (Array2List inarray (n-1)) [(OneRow inarray n)]

let printDataRecord (x:DayRecord) = 
        printf "%s" (x.time.ToString())
        printf "%f\n" x.start
        printf "%f\n" x.high
        printf "%f\n" x.low
        printf "%f\n" x.endp
        printf "%f\n" x.volume
        printf "%f\n" x.amount

let HQ (stockid:string) (n:int) = 
        let hq_handle = dzh.GetData("hqfq", stockid, n) in
        let counter = (Array2D.length1(hq_handle) - 1) in
        Array2List hq_handle counter
let GetEnd (x:DayRecord) = x.endp
let GetVolume (x:DayRecord) = x.volume
let GetAmount (x:DayRecord) = x.amount
let GetTime (x:DayRecord) = x.time
let GetHigh (x:DayRecord) = x.high
let GetLow (x:DayRecord) = x.low

let ByGreater opc (x1:DayRecord) (x2:DayRecord) = 
        if (opc x1) > (opc x2) then x1
        else x2
let ByLess opc (x1:DayRecord) (x2:DayRecord) = 
        if (opc x1) < (opc x2) then x1
        else x2
let rec FoundPeak opFunc (hq:DayRecord list) = 
    if ((List.length hq) = 1) then hq.Head
    else opFunc hq.Head (FoundPeak opFunc hq.Tail)

let MinPriceRecord = FoundPeak (ByLess GetEnd)
let MaxEndpRecord = FoundPeak (ByGreater GetEnd)
let MaxHighRecord = FoundPeak (ByGreater GetHigh)
let MinLowRecord = FoundPeak (ByLess GetLow)
let MinAmountRecord = FoundPeak (ByLess GetAmount)
let MaxAmountRecord = FoundPeak (ByGreater GetAmount)
let TwoTimeEndpCompare opc1 opc2 (hq : DayRecord list) = ((GetEnd (opc1 hq)) - (GetEnd (opc2 hq))) / (GetEnd (opc2 hq)) 
let SpecialPoint2Now = TwoTimeEndpCompare List.head
let CalcLow2Now = SpecialPoint2Now MinPriceRecord
let CalcHigh2Now = SpecialPoint2Now MaxEndpRecord
let CalcLow2High = TwoTimeEndpCompare MaxEndpRecord MinPriceRecord
let CalcLargeAmount2Now = SpecialPoint2Now MaxAmountRecord
let BullBear hqlist = 
        let maxr = MaxEndpRecord hqlist in
        let minr = MinLowRecord hqlist in 
        if maxr.time > minr.time then "Bull" else " Bear"
let t3int1t2 t1 t2 t3 = 
        if t1 > t2 then (t3 < t1) && (t3 > t2)
        else (t3 < t2) && (t3 > t1)
let ElapseTime opc1 opc2 hqlist = 
        let opc1r = opc1 hqlist in
        let opc2r = opc2 hqlist in
        let t1 = GetTime opc1r in
        let t2 = GetTime opc2r in
        let filteredlist = List.filter (fun x -> t3int1t2 t1 t2 (GetTime x)) hqlist in
        List.length filteredlist
type RecordCompare = 
    {   endp : Double;
        time: int;
    }
type StockStatic =
    {   Name : String;
        Trend: String ;
        High : DayRecord;
        Low : DayRecord;
        TopAmount : DayRecord;
        LowAmount : DayRecord;
        Low2Now : RecordCompare;
        High2Now : RecordCompare;
        Low2High : RecordCompare;
        Large2Now : RecordCompare;
    }
let csvcomma = ","
let staticheader2string  = 
        let a = "Name" + csvcomma + "Trend" + csvcomma  in
        let a1 = a + "High time" + csvcomma + "Low Time" + csvcomma in
        let b = a1 + "TopAmount Time" + csvcomma  + "LowAmount Time" + csvcomma in
        let c = b + "Low2Now amp" + csvcomma  + "Low2Now Time" + csvcomma in
        let d = c + "High2Now amp" + csvcomma  + "High2Now Time" + csvcomma in
        let e = d +  "Low2High amp" + csvcomma + "Low2High Time" + csvcomma in
        e + "Large2Now amp" + csvcomma + "Large2Now Time" + "\n"

let static2string (s : StockStatic) = 
        let a = s.Name + csvcomma + s.Trend + csvcomma  in
        let a1 = a + s.High.time.Date.ToString() + csvcomma + s.Low.time.Date.ToString() + csvcomma in
        let b = a1 + s.TopAmount.time.Date.ToString() + csvcomma  + s.LowAmount.time.Date.ToString() + csvcomma in
        let c = b + s.Low2Now.endp.ToString() + csvcomma  + s.Low2Now.time.ToString() + csvcomma in
        let d = c + s.High2Now.endp.ToString() + csvcomma  + s.High2Now.time.ToString() + csvcomma in
        let e = d +  s.Low2High.endp.ToString() + csvcomma + s.Low2High.time.ToString() + csvcomma in
        e + s.Large2Now.endp.ToString() + csvcomma + s.Large2Now.time.ToString() + "\n"
        
let static_of hq name = 
    {   Name = name
        Trend = BullBear hq;
        High = MaxEndpRecord hq;
        Low = MinLowRecord hq;
        TopAmount = MaxAmountRecord hq;
        LowAmount = MinAmountRecord hq;
        Low2Now = { endp = CalcLow2Now hq;
                        time = ElapseTime List.head MinLowRecord hq}
        High2Now = { endp = CalcHigh2Now hq;
                        time = ElapseTime List.head MaxEndpRecord hq}
        Low2High = { endp = CalcLow2High hq;
                        time = ElapseTime MaxEndpRecord MinLowRecord hq}
        Large2Now = { endp = CalcLargeAmount2Now hq;
                        time = ElapseTime List.head MaxAmountRecord hq}
    }


let timein (t1 : string) (t2 : string) (x : DayRecord) = 
        let time1 = System.DateTime.Parse(t1) in
        let time2 = System.DateTime.Parse(t2) in
        let xtime = GetTime x in
        t3int1t2 time1 time2 xtime

let filtIntime t1 t2 hqlist = List.filter (timein t1 t2) hqlist

let rec filtLastnday n record_list = 
        match (n, record_list) with
        | (_, []) -> []
        | (0, _) -> []
        | otherwise ->List.append ([List.head record_list])  (filtLastnday (n - 1) (List.tail record_list))
