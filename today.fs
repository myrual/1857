module public today
open jb
open fq
open fx
let last5 = LastnDayFX2String  idname 5;;
let last10 = LastnDayFX2String  idname 10;; 
let last5_10 = fx2csv "Last_5_10" (List.append last5 last10)
