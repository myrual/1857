module public ri
open jb
open fq
open fx
let OneWaveEnd hq1 hq2 =
        let sta_hq1 = static_of hq1 "aa" in
        let sta_hq2 = static_of (List.append hq1 hq2) "aa" in
        (sta_hq1.Low = sta_hq2.Low) && (sta_hq1.High = sta_hq2.High)
let EarlierPeak sta = 
        if sta.Low.time > sta.High.time then sta.High
        else sta.Low

let rec FoundWave myhead hqseg n = 
        let fstseg = filtLastnday n hqseg in
        let sta_myhead = static_of myhead "head" in
        let hq_afterEarlierPeak = filtBeforetime (EarlierPeak sta_myhead) (List.append myhead hqseg) in
        if fstseg = [] then List.append [sta_myhead] []
        else if (filtLastnday n hq_afterEarlierPeak) = [] then List.append [sta_myhead] []
        else if (OneWaveEnd myhead fstseg) then List.append [sta_myhead] (FoundWave (filtLastnday n hq_afterEarlierPeak) (dropLastnday n hq_afterEarlierPeak) n)
        else FoundWave (List.append myhead fstseg) (dropLastnday n hqseg) n

let Wave id starttime endtime n comment= 
        let hq = FQHQInTime id starttime endtime in
        let firstseg = filtLastnday n hq in
        let followingseg = dropLastnday n hq in
        let stalist = FoundWave firstseg followingseg n in
        let stalistStr = List.map static2string stalist in
        let withtitle = List.append (List.append [comment] [(starttime + "->" + endtime)]) stalistStr in
        let all = List.append [staticheader2string] withtitle in
        System.IO.File.WriteAllLines(comment+".csv", Array.ofList all)
