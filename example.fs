// Simple PDF builder: example use
// Copyright (C) 2017 Daniel Beer <dlbeer@gmail.com>
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

open System
open System.IO
open System.Text
open System.Collections.Generic

type private Dummy = { x: int }

let getRString name =
    use s = typeof<Dummy>.Assembly.GetManifestResourceStream(name)
    let c = Array.create (int s.Length) 0uy
    s.Read(c, 0, c.Length) |> ignore
    Encoding.UTF8.GetString(c)

let glyphs = getRString "glyphlist.txt" |> AFM.parseGlyphList
let timesRoman = getRString "Times-Roman.afm" |> AFM.parseAFM glyphs

let getKey (k : string) (d : IDictionary<string, PDF.Value>) =
    match d.TryGetValue(k) with
    | (false, _) -> failwith ("Missing key: " + k)
    | (true, v) -> v

let resolve (src : PDF.ObjectLoader) (v : PDF.Value) =
    match v with
    | PDF.Indirect i -> src.Load(i)
    | _ -> v

let getDict (v : PDF.Value) =
    match v with
    | PDF.Dictionary d -> d
    | _ -> failwith "Expected dictionary"

let getArray (v : PDF.Value) =
    match v with
    | PDF.Array a -> a
    | _ -> failwith "Expected array"

let getStream (v : PDF.Value) =
    match v with
    | PDF.Stream (d, s) -> (d, s)
    | _ -> failwith "Expected stream"

let page1ToForm (inputData : byte[])
                (out : PDF.Buffer) (outX : PDF.XRefTable) =
    // Read XRefs and trailer, and set up an object loader
    let pdf = PDF.Scanner(inputData)
    let (xin, tin) = PDF.loadAllXRefs pdf
    let src = PDF.ObjectLoader(pdf, xin)

    // Descend the tree to find page 1. Note that PDFs may contain page
    // trees nested more than one level deep, which this doesn't take
    // account of.
    let root = tin |> getKey "Root" |> resolve src |> getDict
    let pages = root |> getKey "Pages" |> resolve src |> getDict
    let plist = pages |> getKey "Kids" |> resolve src |> getArray

    if plist.Length < 1 then
      failwith "No pages in template"

    let p1 = plist.[0] |> resolve src |> getDict
    let (contentDict, contentStream) =
      p1 |> getKey "Contents" |> resolve src |> getStream

    // Copy page 1 into our document as an XObject
    let copier = PDF.Copier(src, out, outX)
    let form = Dictionary<string, PDF.Value>()
    form.["Type"] <- PDF.Name "XObject"
    form.["Subtype"] <- PDF.Name "Form"
    form.["BBox"] <- copier.Translate(p1 |> getKey "MediaBox")
    match p1.TryGetValue("Resources") with
    | (false, _) -> form.["Resources"] <- PDF.Dictionary (dict [])
    | (true, res) -> form.["Resources"] <- copier.Translate(res)

    for kv in contentDict do
      if not (form.ContainsKey(kv.Key)) then
        form.[kv.Key] <- copier.Translate(kv.Value)

    // Extract object size
    let (xw, xh) =
      match form.["BBox"] |> getArray with
      | [| _; _; PDF.Number w; PDF.Number h |] -> (w, h)
      | _ -> failwith "Malformed BBox"

    // Copy the object to the new file
    let r = PDF.Stream (form, contentStream) |> PDF.allocIndirect out outX
    (r, (xw, xh))

let buildPage (width : float, height : float)
              (xw : float, xh : float) (caption : string) =
    let pageBuf = PDF.Buffer()

    // Decide on a scale factor
    let scale = min (xw / (width * 2.0)) (xh / (height * 2.0))

    // Draw our XObject in the center of the page
    let ox = (width - xw * scale) * 0.5
    let oy = (height - xh * scale) * 0.5

    PDF.wKeyword pageBuf "q"
    [| scale; 0.0; 0.0; scale; ox; oy |] |> Array.iter (PDF.wNumber pageBuf)
    PDF.wKeyword pageBuf "cm"
    PDF.wName pageBuf "p1"
    PDF.wKeyword pageBuf "Do"
    PDF.wFreshLine pageBuf
    PDF.wKeyword pageBuf "Q"

    // Draw text at top
    let tps = 16.0
    let tw = float (AFM.measureString timesRoman caption) *
              tps / float AFM.unitsPerPt

    PDF.wKeyword pageBuf "BT"
    PDF.wName pageBuf "f1"
    PDF.wNumber pageBuf tps
    PDF.wKeyword pageBuf "Tf"
    [| 1.0; 0.0; 0.0; 1.0; ((width - tw) * 0.5); (height * 7.0 / 8.0) |] |>
      Array.iter (PDF.wNumber pageBuf)
    PDF.wKeyword pageBuf "Tm"
    PDF.wTextString pageBuf caption
    PDF.wKeyword pageBuf "Tj"
    PDF.wKeyword pageBuf "ET"

    // Draw outline
    PDF.wName pageBuf "DeviceRGB"
    PDF.wKeyword pageBuf "CS"
    [| 0.0; 0.0; 0.8 |] |> Array.iter (PDF.wNumber pageBuf)
    PDF.wKeyword pageBuf "SC"
    PDF.wNumber pageBuf ox
    PDF.wNumber pageBuf oy
    PDF.wKeyword pageBuf "m"
    [| (xw, 0.0); (xw, xh); (0.0, xh); (0.0, 0.0) |] |> Array.iter (fun (x, y) ->
      PDF.wNumber pageBuf (ox + x*scale)
      PDF.wNumber pageBuf (oy + y*scale)
      PDF.wKeyword pageBuf "l")
    PDF.wKeyword pageBuf "S"

    pageBuf

let makeDoc (inputData : byte[]) (caption : string) =
    let (width, height) = (612.0, 792.0)

    // Set up the output document
    let out = PDF.Buffer()
    let outX = PDF.XRefTable()
    PDF.wHeader out "1.3"

    // Copy page 1 from source into this document as an XObject
    let (formRef, (xw, xh)) = page1ToForm inputData out outX

    // Allocate a placeholder for the page directory
    let pageDirRef = outX.Alloc()

    // Create a content stream
    let pageBuf = buildPage (width, height) (xw, xh) caption

    // Construct page 1 metadata
    let f1 =
      dict [
        ("Type", PDF.Name "Font")
        ("Subtype", PDF.Name "Type1")
        ("Name", PDF.Name "f1")
        ("BaseFont", PDF.Name "Times-Roman") ] |>
      PDF.Dictionary |> PDF.allocIndirect out outX
    let procSet = [| "PDF"; "Text" |] |> Array.map PDF.Name
    let fonts = dict [("f1", PDF.Indirect f1)]
    let xobs = dict [("p1", PDF.Indirect formRef)]
    let res =
      dict [
        ("ProcSet", PDF.Array procSet)
        ("Font", PDF.Dictionary fonts)
        ("XObject", PDF.Dictionary xobs)]
    let content =
      ((dict [("Length", PDF.Number (float pageBuf.Size))]),
         PDF.Scanner(pageBuf.Data, 0, pageBuf.Size)) |>
         PDF.Stream |> PDF.allocIndirect out outX
    let p1 =
      dict [
        ("Type", PDF.Name "Page")
        ("Parent", PDF.Indirect pageDirRef)
        ("Contents", PDF.Indirect content)
        ("Resources", PDF.Dictionary res)] |>
      PDF.Dictionary |> PDF.allocIndirect out outX

    // Finalize page directory and the remaining structures
    let pageDir =
      dict [
        ("Type", PDF.Name "Pages")
        ("Count", PDF.Number 1.0)
        ("MediaBox", [| 0.0; 0.0; width; height |] |>
                        Array.map PDF.Number |> PDF.Array)
        ("Kids", [| PDF.Indirect p1 |] |> PDF.Array)]

    outX.Set(pageDirRef, out.Size)
    PDF.wIndirect out pageDirRef (PDF.Dictionary pageDir)

    let catalog =
      dict [
        ("Type", PDF.Name "Catalog")
        ("Pages", PDF.Indirect pageDirRef)] |>
      PDF.Dictionary |> PDF.allocIndirect out outX

    let trailer =
      dict [
        ("Root", PDF.Indirect catalog)
        ("Size", PDF.Number (float outX.Count)) ]

    PDF.wTrailer out outX trailer
    out.ToArray()

[<EntryPoint>]
let main args =
    if args.Length < 2 then
      Console.Error.WriteLine("Usage: example.exe <input.pdf> <output.pdf>")
      1
    else
      let input = File.ReadAllBytes(args.[0])
      let output = makeDoc input args.[0]
      File.WriteAllBytes(args.[1], output)
      0
