// Adobe Font Metrics
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

module AFM

open System
open System.Collections.Generic

exception ParseError of string

let parseGlyphList (text : string) =
    let out = new Dictionary<string, uint32>()
    for l in text.Split('\n') do
      let d = l.Trim()
      if d.Length > 0 && d.[0] <> '#' then
        let p = d.Split(';')
        if p.Length >= 2 then
          try
            out.[p.[0]] <- Convert.ToUInt32(p.[1], 16)
          with
          | :? FormatException | :? OverflowException -> ()
    out

let glyphCode (l : IDictionary<string, uint32>) (n : string) =
    match l.TryGetValue(n) with
    | (false, _) -> raise (ParseError ("Bad glyph: " + n))
    | (true, x) -> x

type Metrics = {
    name:       string
    ascender:   int
    descender:  int
    width:      IDictionary<uint32, int>
    kern:       IDictionary<uint32 * uint32, int> }

let private getIntSimple (text : string) =
    try
      Convert.ToInt32(text)
    with
      | :? FormatException | :? OverflowException ->
        raise (ParseError ("Bad integer: " + text))

let parseGlyphInfo (gl : IDictionary<string, uint32>)
                   (w : IDictionary<uint32, int>)
                   (text : string) =
    let mutable (wx : int option) = None
    let mutable (name : string option) = None
    for p in text.Split(';') do
      let pp = p.Trim()
      if pp.Length >= 3 && pp.[0] = 'W' && pp.[1] = 'X' &&
            pp.[2] = ' ' then
        wx <- Some (getIntSimple pp.[3..])
      elif pp.Length >= 2 && pp.[0] = 'N' && pp.[1] = ' ' then
        name <- Some pp.[2..]
    match (name, wx) with
    | (Some n, Some x) -> w.[glyphCode gl n] <- x
    | (Some n, None) -> raise (ParseError ("Missing width: " + n))
    | (None, _) -> raise (ParseError ("Missing glyph name"))

let parseAFM (gl : IDictionary<string, uint32>) (text : string) =
    let mutable name = ""
    let mutable ascender = 0
    let mutable descender = 0
    let width = new Dictionary<uint32, int>()
    let kern = new Dictionary<uint32 * uint32, int>()

    for l in text.Split('\n') do
      let d = l.Trim()
      let parts = d.Split(' ')
      if parts.Length >= 1 then
        if parts.[0] = "C" then
          parseGlyphInfo gl width d
        elif parts.[0] = "Ascender" then
          if parts.Length < 1 then
            raise (ParseError ("Bad ascender info: " + d))
          ascender <- Convert.ToInt32(parts.[1])
        elif parts.[0] = "Descender" then
          if parts.Length < 1 then
            raise (ParseError ("Bad ascender info: " + d))
          descender <- Convert.ToInt32(parts.[1])
        elif parts.[0] = "WX" then
          if parts.Length < 4 then
            raise (ParseError ("Bad kerning entry: " + d))
          kern.[(glyphCode gl parts.[1], glyphCode gl parts.[2])] <-
            getIntSimple(parts.[3])
        elif parts.[0] = "FontName" then
          if parts.Length < 2 then
            raise (ParseError "Missing font name")
          name <- parts.[1]
    { name = name; ascender = ascender; descender = descender;
      width = width; kern = kern }

let measureChar (afm : Metrics) (ch : char) =
    match afm.width.TryGetValue(uint32 ch) with
    | (true, x) -> x
    | (false, _) -> 0

let measureKern (afm : Metrics) (a : char) (b : char) =
    match afm.kern.TryGetValue((uint32 a, uint32 b)) with
    | (true, x) -> x
    | (false, _) -> 0

let measureString (afm : Metrics) (text : string) =
    let mutable total = text |> Seq.map (measureChar afm) |> Seq.sum
    for i = 0 to text.Length - 2 do
      total <- total + measureKern afm text.[i] text.[i + 1]
    total

let unitsPerPt = 1000
