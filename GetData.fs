module public pdjz
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

let GetHQ (stockid:string) (n:int) = 
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
type StockStatic =
    {   High : DayRecord;
        Low : DayRecord;
        TopAmount : DayRecord;
        BottemAmount : DayRecord;
    }
let GetStockStatic hq = 
    {   High = MaxEndpRecord hq;
        Low = MinLowRecord hq;
        TopAmount = MaxAmountRecord hq;
        BottemAmount = MinAmountRecord hq;
    }
