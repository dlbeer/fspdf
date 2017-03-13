// Simple PDF builder
// Copyright (C) 2016 Daniel Beer <dlbeer@gmail.com>
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice appear in all copies.
//
// THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
// WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
// MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
// ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
// WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
// ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
// OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

module PDF

open System
open System.Collections.Generic

////////////////////////////////////////////////////////////////////////
// Character classes
//
// See PDF 32000-1 2008: 7.2.2 "Character Set"
////////////////////////////////////////////////////////////////////////

type CharClass = Whitespace | Delimiter | Regular

let charType = Array.create 256 Regular

for x in [| 0; 9; 10; 12; 13; 32 |] do
    charType.[x] <- Whitespace

for x in [| 40; 41; 60; 62; 91; 93; 123; 125; 47; 37 |] do
    charType.[x] <- Delimiter

////////////////////////////////////////////////////////////////////////
// Text encoding
////////////////////////////////////////////////////////////////////////

let toASCII (s : string) =
    let out = Array.create s.Length 0uy
    for i = 0 to s.Length - 1 do
      out.[i] <- byte(s.[i])
    out

let fromASCII (s : byte[]) =
    let out = Text.StringBuilder()
    for i = 0 to s.Length - 1 do
      out.Append(char(s.[i])) |> ignore
    out.ToString()

////////////////////////////////////////////////////////////////////////
// Writer
////////////////////////////////////////////////////////////////////////

type Buffer() =
    let mutable data = Array.create 256 0uy
    let mutable size = 0

    member x.Size
      with get() = size
      and set(s: int) = size <- s

    member x.Data
      with get() = data

    member x.Push(dat: byte[], offset: int, len: int) =
      if size + len > data.Length then
        let mutable req = (size + len) * 2
        while (req &&& (req - 1)) <> 0 do
          req <- req &&& (req - 1)
        let n = Array.create req 0uy
        Array.blit data 0 n 0 size
        data <- n
      Array.blit dat offset data size len
      size <- size + len

    member x.Push(dat: byte[]) =
      x.Push(dat, 0, dat.Length)

    member x.Push(dat: byte) =
      x.Push([| dat |])

    member x.ToArray() =
      let out = Array.create size 0uy
      Array.blit data 0 out 0 size
      out

// Write raw ASCII
let wASCII (buf : Buffer) (kw : string) =
    buf.Push(toASCII kw)

// Write a newline, if the buffer doesn't already end on a new line
let wFreshLine (buf : Buffer) =
    if buf.Size > 0 && buf.Data.[buf.Size - 1] <> 10uy then
        buf.Push(10uy)

// Write a whitespace break, if the buffer doesn't end on a whitespace
// character
let wSeparate (buf : Buffer) =
    if buf.Size > 0 &&
            charType.[int(buf.Data.[buf.Size - 1])] <> Whitespace then
        buf.Push(32uy)

// Write a keyword (specified as an ASCII string)
let wKeyword (buf : Buffer) (kw : string) =
    wSeparate buf
    wASCII buf kw

// Write a PDF name
let wName (buf : Buffer) (n : string) =
    wSeparate buf
    buf.Push(byte('/'))
    for cr in n do
        let c = byte(cr)
        if charType.[int(c)] = Regular then
          buf.Push(c)
        else
          wASCII buf <| sprintf "#%02x" c

// Write a number
let wNumber (buf : Buffer) (n : float) =
    wSeparate buf
    wASCII buf <| sprintf "%g" n

// Write a byte-string
let wByteString (buf : Buffer) (n : byte[]) =
    wSeparate buf
    if Array.exists (fun b -> b > 127uy) n then
      buf.Push(byte('<'))
      for b in n do
        wASCII buf (sprintf "%02X" (int b))
      buf.Push(byte('>'))
    else
      buf.Push(byte('('))
      for ch in n do
          if charType.[int(ch)] = Delimiter || ch = byte('\\') then
            buf.Push([| byte('\\'); ch |])
          else
            buf.Push(ch)
      buf.Push(byte(')'))

// Write a PDF string
let wTextString (buf : Buffer) (n : string) =
    // Figure out an encoding
    if Seq.exists (fun x -> x > char(127)) n then
      let enc = new System.Text.UnicodeEncoding(true, true)
      let x = Array.create (enc.GetByteCount(n)) 0uy
      x.[0] <- 254uy
      x.[1] <- 255uy
      enc.GetBytes(n, 0, n.Length, x, 2) |> ignore
      wByteString buf x
    else
      toASCII n |> wByteString buf

////////////////////////////////////////////////////////////////////////
// Basic parsing
////////////////////////////////////////////////////////////////////////

exception ParseError of string

type Scanner(data: byte[], oin: int, limit: int) =
    let mutable offset = oin

    new(d: byte[]) =
      Scanner(d, 0, d.Length)

    member x.Data
      with get() = data

    member x.Limit
      with get() = limit

    member x.Offset
      with get() = offset

    member x.Remaining
      with get() = limit - offset

    member x.Dup() =
      Scanner(data, offset, limit)

    member x.Join(s: Scanner) =
      offset <- s.Offset

    member x.Skip(n: int) =
      if n > x.Remaining then
        raise (ParseError "Data underflow")
      offset <- offset + n

    member x.Sub(n: int) =
      if n > x.Remaining then
        raise (ParseError "Data underflow")
      let r = Scanner(data, offset, offset + n)
      offset <- offset + n
      r

    member x.SubFrom(n: int) =
      if (n < 0) || (n > x.Remaining) then
        raise (ParseError "Invalid offset")
      Scanner(data, offset + n, limit)

let isEOL (b : byte) =
    b = 13uy || b = 10uy

let skipSpace (s : Scanner) =
    let mutable i = s.Offset
    let mutable x = false
    while i < s.Limit && (charType.[int(s.Data.[i])] = Whitespace ||
                          s.Data.[i] = 37uy) do
      if s.Data.[i] = 37uy then
        while i < s.Limit && not (isEOL s.Data.[i]) do
          i <- i + 1
        if i + 1 < s.Limit && s.Data.[i] = 13uy && s.Data.[i + 1] = 10uy then
          i <- i + 1
      else
        i <- i + 1
    s.Skip(i - s.Offset)

let skipEOL (s : Scanner) =
    let mutable i = s.Offset
    while i < s.Limit && not (isEOL s.Data.[i]) do
      if s.Data.[i] = 37uy then
        while i < s.Limit && not (isEOL s.Data.[i]) do
          i <- i + 1
        if i + 1 < s.Limit && s.Data.[i] = 13uy && s.Data.[i + 1] = 10uy then
          i <- i + 1
      elif charType.[int(s.Data.[i])] <> Whitespace then
        raise (ParseError "Unexpected token while scanning for EOL")
      else
        i <- i + 1
    if i < s.Limit then
      if s.Data.[i] = 13uy then
        i <- i + 1
        if i < s.Limit && s.Data.[i] = 10uy then
          i <- i + 1
      else
        i <- i + 1
    s.Skip(i - s.Offset)

let rKeyword (si : Scanner) =
    let s = si.Dup()

    skipSpace s
    let j = s.Offset
    let mutable i = s.Offset

    while i < s.Limit && charType.[int(s.Data.[i])] = Regular do
      i <- i + 1
    s.Skip(i - s.Offset)

    if i <> j then
      si.Join(s)
      Some (fromASCII s.Data.[j..i-1])
    else None

let rUInt (si : Scanner) =
    let s = si.Dup()

    skipSpace s
    let j = s.Offset
    let mutable i = s.Offset

    while i < s.Limit && (s.Data.[i] >= 48uy && s.Data.[i] <= 57uy) do
      i <- i + 1
    s.Skip(i - s.Offset)

    if i <> j && charType.[int(s.Data.[i])] <> Regular then
      let text = fromASCII s.Data.[j..i-1]
      try
        let r = Convert.ToInt32(text)
        si.Join(s)
        Some r
      with
        | :? FormatException | :? OverflowException ->
          raise (ParseError ("Malformed integer" + text))
    else
      None

////////////////////////////////////////////////////////////////////////
// X-refs table
////////////////////////////////////////////////////////////////////////

type ObjectRef = int * int

type ObjectInfo = {
    gen:        int
    loc:        int
    exists:     bool }

type XRefTable() =
    let table = Dictionary<int, ObjectInfo>()
    let mutable next = 1

    do
      table.[0] <- { gen = 65535; loc = 0; exists = false }

    // Allocate a new name. This is a very simple allocation strategy!
    member x.Alloc(l: int) =
      let r = (next, 0)
      table.[next] <- { gen = 0; loc = l; exists = true }
      next <- next + 1
      r

    member x.Alloc() =
      x.Alloc(0)

    member x.Set((idx, gen): ObjectRef, loc: int) =
      table.[idx] <- { gen = gen; loc = loc; exists = true }

    member x.GetLoc((idx, gen): ObjectRef) =
      match table.TryGetValue(idx) with
      | (false, _) -> None
      | (true, info) ->
        if info.gen <> gen || not info.exists then
          None
        else
          Some info.loc

    member x.Count
      with get() = next

    // Write xrefs
    member x.Write(buf: Buffer) =
      let vals = table |> Seq.sortBy (fun kv -> kv.Key) |> Seq.toArray
      wASCII buf (sprintf "xref\r\n")
      let mutable s = 0
      while s < vals.Length do
        let mutable e = s
        while e + 1 < vals.Length && vals.[e + 1].Key = vals.[e].Key + 1 do
          e <- e + 1
        wASCII buf (sprintf "%d %d\r\n" vals.[s].Key (e - s + 1))
        for i = s to e do
          wASCII buf (sprintf "%010d %05d %c\r\n"
                        vals.[i].Value.loc vals.[i].Value.gen
                          (if vals.[i].Value.exists then 'n' else 'f'))
        s <- e + 1

    // Read xrefs
    member x.Read(si: Scanner) =
      let parseBlock () =
        let ss = rUInt si
        let sc = rUInt si
        match (ss, sc) with
        | (Some s, Some c) ->
          skipEOL si
          for i = 0 to c-1 do
            let loc = rUInt si
            let gen = rUInt si
            let state = rKeyword si
            skipEOL si
            match (gen, loc, state) with
            | (Some gen, Some loc, Some state) ->
              if not (table.ContainsKey(s+i)) then
                table.[s+i] <- { gen = gen; loc = loc; exists = state = "n" }
            | _ -> raise (ParseError "Invalid xref entry")
          true
        | _ -> false
      if rKeyword si <> Some "xref" then
        raise (ParseError "Expected xref keyword")
      while parseBlock () do ()

// Write object ref
let wObjectRef (buf : Buffer) (idx, gen) =
    wSeparate buf
    wASCII buf (sprintf "%d %d R" idx gen)

// Write beginning and end of object
let wObjectBegin (buf : Buffer) (idx, gen) =
    wFreshLine buf
    wASCII buf (sprintf "%d %d obj\n" idx gen)

let wObjectEnd (buf : Buffer) =
    wFreshLine buf
    wASCII buf "endobj\n"

////////////////////////////////////////////////////////////////////////
// Structured value types
////////////////////////////////////////////////////////////////////////

type Value =
    | Null
    | Boolean of bool
    | Number of float
    | ByteString of byte[]
    | Name of string
    | Array of Value[]
    | Dictionary of IDictionary<string, Value>
    | Stream of IDictionary<string, Value> * Scanner
    | Indirect of ObjectRef

let rec wValue (buf : Buffer) (v : Value) =
    let wDict (d : IDictionary<string, Value>) =
      wKeyword buf "<<"
      for e in d do
        wName buf e.Key
        wValue buf e.Value
      wKeyword buf ">>"
    match v with
    | Null -> wKeyword buf "null"
    | Boolean true -> wKeyword buf "true"
    | Boolean false -> wKeyword buf "false"
    | Number n -> wKeyword buf (sprintf "%g" n)
    | ByteString s -> wByteString buf s
    | Name s -> wName buf s
    | Array a ->
      wKeyword buf "["
      Array.iter (wValue buf) a
      wKeyword buf "]"
    | Dictionary d -> wDict d
    | Stream (d, s) ->
      wDict d
      wFreshLine buf
      wKeyword buf "stream\n"
      buf.Push(s.Data, s.Offset, s.Limit - s.Offset)
      wKeyword buf "\nendstream\n"
    | Indirect r -> wObjectRef buf r

let private rHexDigit (s : Scanner) =
    let b = s.Data.[s.Offset]
    if (b >= byte '0' && b <= byte '9') then
      s.Skip(1)
      b - byte('0')
    elif (b >= byte 'A' && b <= byte 'F') then
      s.Skip(1)
      b - byte('A') + 10uy
    elif (b >= byte 'a' && b <= byte 'f') then
      s.Skip(1)
      b - byte('a') + 10uy
    else
      raise (ParseError (sprintf "Invalid hex digit: %c" (char b)))

let private rHexByte (s : Scanner) =
    let hi = rHexDigit s
    let lo = rHexDigit s
    (hi <<< 4) ||| lo

let private rName (s : Scanner) =
    let out = Text.StringBuilder()
    let rec scan () =
      let ch = s.Data.[s.Offset]
      if ch = byte('#') then
        s.Skip(1)
        out.Append(char(rHexByte s)) |> ignore
        scan ()
      elif charType.[int(ch)] = Regular then
        s.Skip(1)
        out.Append(char(ch)) |> ignore
        scan ()
    if s.Data.[s.Offset] <> byte('/') then
      raise (ParseError "Expected name")
    s.Skip(1)
    scan ()
    out.ToString()

let private rEscape (s : Scanner) =
    if s.Offset >= s.Limit then
      raise (ParseError "EOF while reading escape sequence")
    let t = s.Data.[s.Offset]
    if t >= byte('0') && t <= byte('7') then
      let mutable octal = 0uy
      for i = 1 to 3 do
        let d = s.Data.[s.Offset]
        s.Skip(1)
        if (d < byte('0')) || (d > byte('7')) then
          raise (ParseError (sprintf "Invalid octal digit: %c" (char(d))))
        octal <- (octal <<< 3) ||| d
      octal
    else
      s.Skip(1)
      match char(t) with
      | 'n' -> byte('\n')
      | 'r' -> byte('\r')
      | 't' -> byte('\t')
      | 'b' -> byte('\b')
      | 'f' -> 12uy
      | _ -> t

let private rString (s : Scanner) =
    let mutable depth = 1
    let out = Buffer()
    s.Skip(1)
    while depth > 0 do
      let ch = s.Data.[s.Offset]
      s.Skip(1)

      if ch = byte('(') then
        out.Push(ch)
        depth <- depth + 1
      elif ch = byte(')') then
        depth <- depth - 1
        if depth > 0 then
          out.Push(ch)
      elif ch = byte('\\') then
        out.Push(rEscape s)
      else
        out.Push(ch)
    out.ToArray()

let private rHexString (s : Scanner) =
    let out = Buffer()
    s.Skip(1)
    while s.Offset < s.Limit && s.Data.[s.Offset] <> byte('>') do
      out.Push(rHexByte s)
    if s.Offset >= s.Limit then
      raise (ParseError "EOF while trying to read hex string")
    s.Skip(1)
    out.ToArray()

let private rIndirect (si : Scanner) =
    let s = si.Dup()
    match rUInt s with
    | None -> None
    | Some idx ->
      match rUInt s with
      | None -> None
      | Some gen ->
        if rKeyword s = Some "R" then
          si.Join(s)
          Some (idx, gen)
        else None

let private rStreamData (s : Scanner) (d : Dictionary<string, Value>)
                        (lresolve : Value -> Value) =
    let ss = s.Dup()
    if rKeyword ss = Some "stream" then
      skipEOL ss
      match d.TryGetValue("Length") with
      | (false, _) -> raise (ParseError "Missing length value")
      | (true, vv) ->
        match lresolve vv with
        | Number n ->
          let ni = int n
          if ni < 0 then
            raise (ParseError "Invalid length")
          else
            let d = ss.Sub(ni)
            if rKeyword ss <> Some "endstream" then
              raise (ParseError "Missing endstream")
            s.Join(ss)
            Some d
        | _ -> raise (ParseError "Invalid length type")
    else None

let rec rValue (s : Scanner) (lresolve : Value -> Value) =
    skipSpace s
    if s.Offset >= s.Limit then
      raise (ParseError "EOF while trying to read object")
    if s.Data.[s.Offset] = byte('/') then
      rName s |> Name
    elif s.Data.[s.Offset] = byte('(') then
      rString s |> ByteString
    elif s.Data.[s.Offset] = byte('<') then
      if s.Offset + 1 < s.Limit && s.Data.[s.Offset+1] = byte('<') then
        let out = new Dictionary<string, Value>()
        s.Skip(2)
        skipSpace s
        while s.Offset + 1 < s.Limit &&
              s.Data.[s.Offset..s.Offset+1] <> [| byte('>'); byte('>') |] do
          let k = rName s
          let v = rValue s lresolve
          out.[k] <- v
          skipSpace s
        if s.Offset + 1 >= s.Limit then
          raise (ParseError "EOF while trying to read array")
        s.Skip(2)

        match rStreamData s out lresolve with
        | None -> Dictionary out
        | Some d -> Stream (out, d)
      else
        rHexString s |> ByteString
    elif s.Data.[s.Offset] = byte('[') then
      s.Skip(1)
      skipSpace s
      let out = List<Value>()
      while s.Offset < s.Limit && s.Data.[s.Offset] <> byte(']') do
        out.Add(rValue s lresolve)
        skipSpace s
      if s.Offset >= s.Limit then
        raise (ParseError "EOF while trying to read array")
      s.Skip(1)
      out.ToArray() |> Array
    else
      match rIndirect s with
      | Some r -> Indirect r
      | _ ->
        match rKeyword s with
        | Some "null" -> Null
        | Some "true" -> Boolean true
        | Some "false" -> Boolean false
        | Some s ->
          match Double.TryParse(s) with
          | (false, _) -> raise (ParseError "Invalid number format")
          | (true, x) -> Number x
        | None -> raise (ParseError "Invalid object")

////////////////////////////////////////////////////////////////////////
// File parsing
////////////////////////////////////////////////////////////////////////

let loadVersion (s : Scanner) =
    if s.Remaining < 5 then
      raise (ParseError "Missing header")
    if s.Data.[s.Offset..s.Offset+4] <> [| 37uy; 80uy; 68uy; 70uy; 45uy |] then
      raise (ParseError "Missing header")
    let mutable i = s.Offset + 5
    while i < s.Limit && not (isEOL s.Data.[i]) do
      i <- i + 1
    fromASCII s.Data.[s.Offset+5..i-1]

let loadStartXRef (s : Scanner) =
    let lines = Array.create 3 ""
    let mutable i = s.Limit

    for n = 2 downto 0 do
      while i > s.Offset && isEOL s.Data.[i - 1] do
        i <- i - 1
      let mutable j = i
      while j > s.Offset && not (isEOL s.Data.[j - 1]) do
        j <- j - 1
      lines.[n] <- fromASCII s.Data.[j..(i-1)]
      i <- j

    if lines.[2] <> "%%EOF" then
      raise (ParseError "Missing footer")
    if lines.[0] <> "startxref" then
      raise (ParseError "Missing startxref")
    try
      Convert.ToInt32(lines.[1])
    with
      | :? FormatException | :? OverflowException ->
        raise (ParseError ("Malformed startxref: " + lines.[1]))

let loadSingleXTable (ss : Scanner) (x : XRefTable) =
    x.Read(ss)
    if rKeyword ss <> Some "trailer" then
      raise (ParseError "Missing trailer after XRefs")
    match rValue ss id with
    | Dictionary d -> d
    | _ -> raise (ParseError "Invalid trailer: expected dictionary")

let loadAllXRefs (s : Scanner) =
    let x = XRefTable()
    let rec scanBack lastptr (t : IDictionary<string, Value>) =
      match t.TryGetValue("Prev") with
      | (true, Number n) ->
        let p = int(n)
        if p < lastptr then
          loadSingleXTable (s.SubFrom(p)) x |> scanBack p
      | _ -> ()
    let ptr = loadStartXRef s
    let first = loadSingleXTable (s.SubFrom(ptr)) x
    scanBack ptr first
    (x, first)

let private loadIndirectRaw (s : Scanner) (x : XRefTable) (r : ObjectRef)
                            (lresolve : Value -> Value) =
    match x.GetLoc(r) with
    | None -> Null
    | Some loc ->
      let ss = s.SubFrom(loc)
      let (idx, gen) = r
      if rUInt ss <> Some idx then
        raise (ParseError "Invalid index in object header")
      if rUInt ss <> Some gen then
        raise (ParseError "Invalid generation in object header")
      if rKeyword ss <> Some "obj" then
        raise (ParseError "Invalid object header")
      let ret = rValue ss lresolve
      if rKeyword ss <> Some "endobj" then
        raise (ParseError "Missing endobj")
      ret

type ObjectLoader(s: Scanner, x: XRefTable) =
    let lengthCache = new Dictionary<ObjectRef, Value>()

    member self.Load(r: ObjectRef) =
      loadIndirectRaw s x r <| fun v ->
        let visited = new HashSet<ObjectRef>()
        let rec descend n =
          match n with
          | Indirect rr ->
            match lengthCache.TryGetValue(rr) with
            | (true, v) -> v
            | (false, _) ->
              if not (visited.Add(rr)) then
                raise (ParseError "Recursive loop in length resolution")
              loadIndirectRaw s x rr id |> descend
          | _ -> n
        let ret = descend v
        for i in visited do
          lengthCache.[i] <- ret
        ret

////////////////////////////////////////////////////////////////////////
// File construction
////////////////////////////////////////////////////////////////////////

let wHeader (f : Buffer) (v : string) =
    wASCII f (sprintf "%%PDF-%s\n" v)

let wTrailer (f : Buffer) (x : XRefTable)
             (trailer : IDictionary<string, Value>) =
    let startX = f.Size
    x.Write(f)
    wFreshLine f
    wASCII f "trailer\n"
    wValue f (Dictionary trailer)
    wFreshLine f
    wASCII f (sprintf "startxref\n%d\n%%%%EOF\n" startX)

let wIndirect (f : Buffer) (r : ObjectRef) (v : Value) =
    wObjectBegin f r
    wValue f v
    wObjectEnd f

let allocIndirect (f : Buffer) (x : XRefTable) (v : Value) =
    let r = x.Alloc(f.Size)
    wIndirect f r v
    r

////////////////////////////////////////////////////////////////////////
// Deep copy of objects
////////////////////////////////////////////////////////////////////////

type Copier(src: ObjectLoader, dst: Buffer, dx: XRefTable) =
    let mapping = new Dictionary<ObjectRef, ObjectRef>()

    member x.TranslateDict(d: IDictionary<string, Value>) =
      let out = new Dictionary<string, Value>()
      for e in d do
        out.[e.Key] <- x.Translate(e.Value)
      out :> IDictionary<string, Value>

    member x.Translate(r: Value) =
      match r with
      | Null -> Null
      | Number n -> Number n
      | Boolean b -> Boolean b
      | ByteString b -> ByteString b
      | Name n -> Name n
      | Array a -> a |> Array.map (fun e -> x.Translate(e)) |> Array
      | Dictionary d -> x.TranslateDict(d) |> Dictionary
      | Stream (d, s) -> Stream (x.TranslateDict(d), s)
      | Indirect r ->
        match mapping.TryGetValue(r) with
        | (true, c) -> Indirect c
        | (false, _) ->
          let v = src.Load(r)
          if v = Null then Null
          else
            let c = dx.Alloc()
            mapping.[r] <- c
            let tv = x.Translate(v)
            dx.Set(c, dst.Size)
            wIndirect dst c tv
            Indirect c
