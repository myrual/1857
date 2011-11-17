module public ShaZhu
open FinData;; 
open System

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
let DayRecord = dzh.GetData("hq", "SH600834", 10)

let GStart (x:string[,]) n = System.Convert.ToDouble(x.GetValue(n, 2));;
let Ghigh (x:string[,]) n =  System.Convert.ToDouble(x.GetValue(n,3));;
let Glow (x:string[,]) n = System.Convert.ToDouble(x.GetValue(n,4));;
let Gend (x:string[,]) n = System.Convert.ToDouble(x.GetValue(n,5));;
let Gvolume (x:string[,]) n = System.Convert.ToDouble(string(x.GetValue(n,6)));;
let Gwhole (x:string[,]) n = System.Convert.ToDouble(string(x.GetValue(n,7)));;
let GTime (x:string[,]) n = System.DateTime.Parse(string(x.GetValue(n, 1)));;
let OneRow (x:string[,]) n = {time = GTime x n;
             start = GStart x n;
             high = Ghigh x n;
             low = Glow x n;
             endp = Gend x n;
             volume = Gvolume x n;
             amount = Gwhole x n
}
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

let yesterday = OneRow DayRecord 1
printDataRecord yesterday
let myres  = Array2List DayRecord (Array2D.length1(DayRecord) - 1);;
printfn "see me"
let a = List.map printDataRecord myres
