module public fx
open jb
open fq

let FQHQ (stockid:string) n = 
        let hqlist = HQ (id2string stockid) n in
        if (FqFileIsHere stockid) then List.map (FQRecordWith_FQIndexList_Oneday (GenindexlistByID stockid)) hqlist
        else hqlist




let SortByLow2HighAmp sta = 
    amp_Of sta.Low2High

let GeneralFenXi idnamelist filtFunc = 
    let hqList = List.map (fun x -> FQHQ (fst x) 0) idnamelist in
    let nameList = List.map snd idnamelist in
    let filtHqList = List.map filtFunc hqList in
    let hq_name = List.zip filtHqList nameList in
    let static_list = List.map (fun x -> static_of (fst x) (snd x)) hq_name in
    List.sortBy(fun x -> amp_Of x.Start2Now) static_list
    

let FenXiList2String fxlist = 
    List.map static2string  fxlist

let FenXiList2StringTitle fxlist ( title :string)= 
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

let id = ["B$993738"; "B$991004"; "B$991034"; "B$991019"; "B$991007"; "600718"; "600797"; "002093"; "600834"; "601857"]
let name = [ "yunjisuan";"jisuanji"; "Yousejinshu"; "meitanshiyou"; "fangdichan";"dongruan"; "zhedawangxin"; "guomaikeji"; "ShentongDitie"; "zhongguoshiyou" ]
let idname = List.zip id name

let shanghai_n n = static_of (FQHQ "SH000001" n) "ShangHaih"
let Shanghai22_Low = (shanghai_n 22).Low.time.Date.ToString()
let Shanghai44_Low = (shanghai_n 44).Low.time.Date.ToString()
let Shanghai66_Low = (shanghai_n 66).Low.time.Date.ToString()

let last5 = LastnDayFX2String  idname 5;;
let last10 = LastnDayFX2String  idname 10;; 
let last5_10 = fx2csv "Last_5_10" (List.append last5 last10)
let NOW = System.DateTime.Now.Date.ToString()
let Recent n = InTimeFX2File "Recent66" idname n NOW
