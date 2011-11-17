#r "FinData.dll"
open FinData;;
let dzh = FinData.FxjData();;
let pufayinhang_latestTen = dzh.GetData("hq", "SH000001", 10)
