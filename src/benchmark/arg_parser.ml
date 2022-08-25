(**
Argument parser with support for quotes, taken from
   https://stackoverflow.com/a/58585029/11510700
*)

let rec scan s offset =
    let slen = String.length s in
    if offset >= slen then
        None
    else if s.[offset] = ' ' then
        scan s (offset + 1)
    else if s.[offset] = '"' then
        let rec qlook loff =
            if loff >= slen then
                (* Unterminated quotation *)
                let tok = String.sub s offset (slen - offset) in
                Some (tok, slen)
            else if s.[loff] = '"' then
                let tok = String.sub s offset (loff - offset + 1) in
                Some (tok, loff + 1)
            else qlook (loff + 1)
        in
        qlook (offset + 1)
    else
        let rec wlook loff =
            if loff >= slen then
                let tok = String.sub s offset (slen - offset) in
                Some (tok, slen)
            else if s.[loff] = ' ' || s.[loff] = '"' then
                let tok = String.sub s offset (loff - offset) in
                Some (tok, loff)
            else
                wlook (loff + 1)
        in
        wlook (offset + 1)

let split s =
  let rec isplit accum offset =
    match scan s offset with
    | None -> List.rev accum
    | Some (tok, offset') -> isplit (tok :: accum) offset'
  in
  isplit [] 0
