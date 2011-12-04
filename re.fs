module public ri
open jb
open fq
open fx
let OneWaveEnd hq1 hq2 =
        let sta_hq1 = static_of hq1 "aa" in
        let sta_hq2 = static_of (List.append hq1 hq2) "aa" in
        (sta_hq1.Low = sta_hq2.Low) && (sta_hq1.High = sta_hq2.High)

let rec FoundWave myhead hqseg n = 
        let fstseg = filtLastnday n hqseg in
        if fstseg = [] then List.append [static_of myhead "bb"] []
        else if (OneWaveEnd myhead fstseg) then (List.append ([static_of myhead "aa"]) (FoundWave fstseg (dropLastnday n hqseg) n))
        else FoundWave (List.append myhead fstseg) (dropLastnday n hqseg) n
