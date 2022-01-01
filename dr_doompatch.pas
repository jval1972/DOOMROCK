//------------------------------------------------------------------------------
//
//  DOOMROCK: Doom Rock Sprite Generator
//  Copyright (C) 2021-2022 by Jim Valavanis
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/doom-rock/
//------------------------------------------------------------------------------

unit dr_doompatch;

interface

uses
  Windows, Classes, SysUtils, Graphics, dr_utils;

function BmpAsPatch(const b: TBitmap; const palarray: PByteArray;
  const offsl: integer = MAXINT; const offst: integer = MAXINT): TMemoryStream;

implementation

uses
  dr_palettes;

type
  patch_t = packed record
    width: smallint; // bounding box size
    height: smallint;
    leftoffset: smallint; // pixels to the left of origin
    topoffset: smallint;  // pixels below the origin
  end;

  column_t = packed record
    topdelta: byte; // -1 is the last post in a column
    length: byte;   // length data bytes follows
  end;

function BmpAsPatch(const b: TBitmap; const palarray: PByteArray;
  const offsl: integer = MAXINT; const offst: integer = MAXINT): TMemoryStream;
var
  m: TMemoryStream;
  patch: patch_t;
  column: column_t;
  columnofs: TDNumberList;
  columndata: TDNumberList;
  x, y: integer;
  palette: TDoomPalette;
  c: LongWord;
  i: integer;

  procedure flashcolumnend;
  begin
    column.topdelta := 255;
    column.length := 0;
    m.Write(column, SizeOf(column));
  end;

  procedure flashcolumndata;
  var
    ii: integer;
    bb: byte;
  begin
    if columndata.Count > 0 then
    begin
      column.topdelta := y - columndata.Count;
      column.length := columndata.Count;
      m.Write(column, SizeOf(column));
      bb := 0;
      m.Write(bb, SizeOf(bb));
      for ii := 0 to columndata.Count - 1 do
      begin
        bb := columndata.Numbers[ii];
        m.Write(bb, SizeOf(bb));
      end;
      bb := 0;
      m.Write(bb, SizeOf(bb));
      columndata.Clear;
    end;
  end;

begin
  for i := 0 to 255 do
    palette[i] := RGB(palarray[3 * i], palarray[3 * i + 1], palarray[3 * i + 2]);

  m := TMemoryStream.Create;
  Result := TMemoryStream.Create;
  columnofs := TDNumberList.Create;
  columndata := TDNumberList.Create;
  try
    patch.width := b.Width;
    patch.height := b.Height;
    if offsl = MAXINT then
      patch.leftoffset := b.Width div 2
    else
      patch.leftoffset := offsl;
    if offst = MAXINT then
      patch.topoffset := b.Height
    else
      patch.topoffset := offst;
    Result.Write(patch, SizeOf(patch_t));

    for x := 0 to b.Width - 1 do
    begin
      columnofs.Add(m.Position + SizeOf(patch_t) + b.Width * SizeOf(integer));
      columndata.Clear;
      for y := 0 to b.Height - 1 do
      begin
        c := b.Canvas.Pixels[x, y];
        if c = 0 then
        begin
          flashcolumndata;
          continue;
        end;
        columndata.Add(DT_FindAproxColorIndex(@palette, c))
      end;
      flashcolumndata;
      flashcolumnend;
    end;

    for i := 0 to columnofs.Count - 1 do
    begin
      x := columnofs.Numbers[i];
      Result.Write(x, SizeOf(integer));
    end;

    m.Position := 0;
    Result.CopyFrom(m, m.Size);

  finally
    m.Free;
    columnofs.Free;
    columndata.Free;
  end;
end;

end.
