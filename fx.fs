module public fx
open System
open jb
open fq
let FQHQ (stockid:string) n =
        let hqlist = HQ (id2string stockid) n in
        if (FqFileIsHere stockid) then List.map (FQRecordWith_FQIndexList_Oneday (GenindexlistByID stockid)) hqlist
        else hqlist

let FQHQInTime id t1 t2 = (FQHQ id 0 )|>  (filtIntime t1 t2)
let FQHQBeforeTime id t1 = (FQHQ id 0 )|>  (List.filter (beforetime t1))
let FQHQAfterTime id t1 = (FQHQ id 0 )|>  (List.filter (aftertime t1))
let staticInTime idname t1 t2 = static_of (FQHQInTime (fst idname) t1 t2) (snd idname)

let SortByLow2HighAmp  = Low2High >> amp_Of

let GeneralFenXi idnamelist filtFunc =
    let hqList = List.map (fun x -> FQHQ (fst x) 0) idnamelist in
    let nameList = List.map snd idnamelist in
    let filtHqList = List.map filtFunc hqList in
    let hq_name = List.zip filtHqList nameList in
    let static_list = List.map (fun x -> static_of (fst x) (snd x)) hq_name in
    List.sortBy(Start2Now >> amp_Of) static_list
    

let FenXiList2String fxlist =
    List.map static2string  fxlist

let FenXiList2StringTitle fxlist (title:string)=
    let fenxistring = FenXiList2String fxlist in
    List.append [ title ] fenxistring

let fx2csv fname stringlist = 
        let result = Array.append [| staticheader2string |] (Array.ofList stringlist) in
        System.IO.File.WriteAllLines(fname+".csv", result)



let FXList2FileWithTitle fname fxlist title = 
        let fenxistring = FenXiList2StringTitle fxlist title in
        fx2csv fname fenxistring

let LastnDayFX2String idname n = 
        let fxlist = GeneralFenXi idname (filtLastnday n) in
        let title = "Last " + n.ToString() + "days" in
        FenXiList2StringTitle fxlist title

let LastnDayFX2File fname idname n = 
        let str = LastnDayFX2String idname n in
        fx2csv fname str

let InTimeFX2String idnamelist t1 t2 = 
        let fxlist = GeneralFenXi idnamelist (filtIntime t1 t2) in
        let title = t1 + " ->" + t2 in
        FenXiList2StringTitle fxlist title

let InTimeFX2File fname idnamelist t1 t2 = 
        let str = InTimeFX2String idnamelist t1 t2 in
        fx2csv fname str

let TrueOne x = if x then 1 else 0
let LowFirst st1 st2 = 
        (GetTime st1.Low).Date <= (GetTime st2.Low).Date

let Greater_Start2Now st1 st2 = 
        (st1 |> Start2Now |> amp_Of) > (st2 |> Start2Now |> amp_Of)

let Greater_Low2High st1 st2 = 
        (st1 |> Low2High |> amp_Of) > (st2 |> Low2High |> amp_Of)

/// hqlist is an FIFO list, earlier record is on head, later is on tail
let Bolling hqlist =
        if hqlist = [] then (0.0, (0.0, 0.0))
        else
        let aver = List.averageBy GetEnd hqlist in
        let delta1 = List.sumBy (fun x-> Math.Pow((GetEnd x) - aver, 2.0)) hqlist in
        let std = Math.Sqrt(delta1/(Convert.ToDouble(List.length hqlist) - 1.0)) in
        let Boll_UB = aver + std*2.0 in
        let Boll_LB = aver - std*2.0 in
        let BollingBand = (Boll_UB, Boll_LB) in
        (aver, BollingBand)

/// hqlist is earlier data first, later data last
let Boll first_lasthq n = 
        let last_firsthq = List.rev first_lasthq in
        let filtn4delta (x:DayRecord) = List.rev (filtLastnday n (filtBeforetime x  last_firsthq)) in
        List.map (fun x->(GetEnd x, Bolling (filtn4delta x))) first_lasthq
let BullThanWith a b t1 t2 = 
        let static_a = staticInTime a t1 t2 in
        let static_b = staticInTime b t1 t2 in
        let s2n = Greater_Start2Now  static_a static_b in
        let l2n = Greater_Low2High  static_a static_b in
        let low1st = LowFirst static_a static_b in
        (TrueOne s2n) + (TrueOne l2n) + (TrueOne low1st)

let id = ["SH000001"; "B$993738"; "B$991004"; "B$991034"; "B$991019"; "B$991007"; "600718"; "600797"; "002093"; "600834"; "601857"]
let name = ["shangzheng";  "yunjisuan";"jisuanji"; "Yousejinshu"; "meitanshiyou"; "fangdichan";"dongruan"; "zhedawangxin"; "guomaikeji"; "ShentongDitie"; "zhongguoshiyou" ]
let sh = ("SH000001", "Shanghai")
let yjs = ("B$993738", "Yunjisuan")
let jsj = ("B$991004", "jisuanji")
let ysjs = ("B$991034", "yousejinshu")
let mtsy = ("B$991019", "meitanshiyou")
let fdc = ("B$991007", "fangdichan")
let jycm = ("B$991032", "jiaoyuchuanmei")
let drjt = ("600718", "dongruanjituan")
let zdwx = ("600797", "zhedawangxin")
let gmkj = ("002093", "guomaikeji")
let stdt = ("600834", "shentongditie")
let tobe = [yjs; jsj; mtsy; fdc; drjt; zdwx; gmkj; stdt]
let idname = List.append [sh] tobe

let rec quicksort idnamelist t1 t2 = 
        if idnamelist = [] then []
        else 
        let hd = List.head idnamelist in
        let tl = List.tail idnamelist in
        let smaller = quicksort (List.filter (fun x -> (BullThanWith hd x t1 t2) <= 1) tl) t1 t2 in
        let bigger = quicksort (List.filter (fun x -> (BullThanWith hd x t1 t2) > 1) tl) t1 t2 in
        List.append (List.append smaller [hd]) bigger
let BullThanSH t1 t2 a = BullThanWith a sh t1 t2
let ListBullThanSH idnamelist t1 t2 = 
        let bulllist = List.map (fun x -> (snd x, (BullThanSH t1 t2 x))) idnamelist in
        List.sortBy(snd) bulllist

let shanghai_n n = static_of (FQHQ "SH000001" n) "ShangHaih"
let Shanghai22_Low = (shanghai_n 22).Low.time.Date.ToString()
let Shanghai44_Low = (shanghai_n 44).Low.time.Date.ToString()
let Shanghai66_Low = (shanghai_n 66).Low.time.Date.ToString()

let NOW = System.DateTime.Now.Date.ToString()
let Recent n = InTimeFX2File "Recent_n" idname n NOW
        
let VerifyPressline (k,d) hqlist = 
        let hd = List.head hqlist in
        not (List.exists (fun x -> (k*(System.Convert.ToDouble(TimeDiff hd x hqlist)) + d) < (GetEnd x)) hqlist)

let VerifyPressline_Up (k,d) hqlist = 
        let hd = List.head hqlist in
        not (List.exists (fun x -> (k*(System.Convert.ToDouble(TimeDiff hd x hqlist)) + d) > (GetEnd x)) hqlist)
let GetKDFrom x1 x2 hqlist = 
        let deltax = TimeDiff x1 x2 hqlist in
        let deltay = (GetEnd x2) - (GetEnd x1) in
        let k = deltay/System.Convert.ToDouble(deltax) in
        let d = (GetEnd x2) - (k * System.Convert.ToDouble(deltax)) in
        (k, d)

let FoundKD hqlist peak_func verify = 
        let high = peak_func hqlist in
        let afterhigh = filtaftertime high hqlist in
        let tail_after_high = List.tail afterhigh in
        let matched = List.find (fun x -> verify (GetKDFrom high x tail_after_high) tail_after_high) tail_after_high in
        GetKDFrom high matched hqlist in
let FoundKD_Down = (fun x -> FoundKD x MaxEndpRecord VerifyPressline)
let FoundKD_Up = (fun x -> FoundKD x MinEndpRecord VerifyPressline_Up)

let rec Calcbykdxi (k,d) initx n = 
        if n = 0 then []
        else List.append [k*initx + d] (Calcbykdxi (k,d) (initx+1.0) (n-1))

let CalcUpDown hqlist n foundkd peak = 
        let kd = foundkd hqlist in
        let low = peak hqlist in
        let latest = FindLatestRecord hqlist in
        let lowtonow = TimeDiff low latest hqlist in
        Calcbykdxi kd (System.Convert.ToDouble(lowtonow)) n

let CalcDown hqlist n = CalcUpDown hqlist n FoundKD_Down MaxEndpRecord
let CalcUp hqlist n = CalcUpDown hqlist n FoundKD_Up MinEndpRecord
