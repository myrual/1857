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
    List.sortBy(SortByLow2HighAmp) static_list
    

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

let id = ["B$993738"; "600718"; "600797"; "002093"; "600834"; "601857"]
let name = [ "yunjisuan"; "dongruan"; "zhedawangxin"; "guomaikeji"; "ShentongDitie"; "zhongguoshiyou" ]
let idname = List.zip id name

let last5 = LastnDayFX2File "last5" idname 5;;
let last10 = LastnDayFX2File "last10" idname 10;; 
let NOW = System.DateTime.Now.Date.ToString()
let Recent = InTimeFX2File "Oct10ToNow" idname "2011-10-10" NOW
