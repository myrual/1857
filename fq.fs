module public fq
open jb
type FQRecord = 
    { time : System.DateTime;
        free : double;
        gift : double;
        bonus : double;
        cheaper_price : double;
        cheaper_count : double;
    }

type FQIndex = 
    { time : System.DateTime;
      Index : double
    }
let rec findYesterday (cqinfo : FQRecord) (hqinfolist : DayRecord list) = 
        if cqinfo.time > hqinfolist.Head.time then hqinfolist.Head
        else findYesterday cqinfo hqinfolist.Tail

let findcqfile (stockid:string) = "fq\\" + stockid + ".csv"
let cqfilelines stockid = System.IO.File.ReadAllLines(findcqfile stockid)
let cleancq stockid = List.ofArray(Array.filter (fun (x:string) -> x.Length > 0) (cqfilelines stockid))
let MakeOne (x:string) = 
    let splitd = x.Split(',') in
    let datestr = splitd.[0] in
    let freestr = splitd.[1] in
    let giftstr = splitd.[2] in
    let bonusstr = splitd.[3] in
    let cheaper_pricestr = splitd.[4] in
    let cheaper_countstr = splitd.[5] in
        { time = System.DateTime.Parse(datestr.[0..3] + "-" + datestr.[4..5] + "-" + datestr.[6..7]);
            free = System.Convert.ToDouble(freestr);
            gift = System.Convert.ToDouble(giftstr);
            bonus = System.Convert.ToDouble(bonusstr);
            cheaper_price = System.Convert.ToDouble(cheaper_pricestr);
            cheaper_count = System.Convert.ToDouble(cheaper_countstr)
        }
let cqlist stockid = List.map MakeOne (cleancq stockid);;
let GenerateFQIndex (hqinfolist : DayRecord list) (cqinfo : FQRecord) = 
        let yesterday = findYesterday cqinfo hqinfolist in
        let yesterday_endprice = GetEnd yesterday in
        let fq_1 = yesterday_endprice - cqinfo.bonus in
        let fq_2 = (fq_1) /(1.0 + cqinfo.free + cqinfo.gift) in
        let fq_3 = (fq_2 + cqinfo.cheaper_price * cqinfo.cheaper_count) / (1.0 + cqinfo.cheaper_count) in
        { time = cqinfo.time;
            Index = fq_3 / yesterday_endprice
        }
let id2string (stockid:string) = 
        if stockid.[0] = '6' then ("SH" + stockid)
        else if stockid.[0] = '0' then ("SZ" + stockid)
        else stockid

let GenFQIndexList (hqlist : DayRecord list) (fqlist : FQRecord list) = 
        List.map (GenerateFQIndex hqlist) fqlist

let GenindexlistByID stockid = 
        let hqlist = HQ (id2string stockid) 0 in
        let fqlist = cqlist stockid in
        GenFQIndexList hqlist  fqlist

let rec FQIndexWith_FqIndexList_inputtime (fqindexlist : FQIndex list) (inputtime : System.DateTime) = 
        if fqindexlist = [] then 1.0
        else if inputtime > fqindexlist.Head.time then 1.0
        else fqindexlist.Head.Index * (FQIndexWith_FqIndexList_inputtime fqindexlist.Tail inputtime)

let FQRecordWith_FQIndexList_Oneday  (fqindexlist : FQIndex list) (oneday : DayRecord) =
        let fqindex = FQIndexWith_FqIndexList_inputtime fqindexlist oneday.time in
        { time =  oneday.time;
          start = oneday.start * fqindex;
          high = oneday.high * fqindex;
          low = oneday.low * fqindex;
          endp = oneday.endp * fqindex;
          volume = oneday.volume * fqindex;
          amount = oneday.amount * fqindex;
        }

let FqFileIsHere (stockid : string) = System.IO.File.Exists( findcqfile stockid)
