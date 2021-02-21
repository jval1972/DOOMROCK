//------------------------------------------------------------------------------
//
//  DOOMTREE: Doom Tree Sprite Generator
//  Copyright (C) 2021 by Jim Valavanis
//
// DESCRIPTION:
//  WAD Writer
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : https://sourceforge.net/projects/doom-tree/
//------------------------------------------------------------------------------

unit dt_wadwriter;

interface

uses
  Classes, SysUtils;

type
  TWadWriter = class(TObject)
  private
    lumps: TStringList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure AddData(const lumpname: string; const data: pointer; const size: integer);
    procedure AddString(const lumpname: string; const data: string);
    procedure AddStringList(const lumpname: string; const lst: TStringList);
    procedure AddSeparator(const lumpname: string);
    procedure SaveToStream(const strm: TStream);
    procedure SaveToFile(const fname: string);
  end;

function AddDataToWAD(const wad: TWADWriter; const lumpname, data: string): boolean;

implementation

uses
  dt_wad;

constructor TWadWriter.Create;
begin
  lumps := TStringList.Create;
  Inherited;
end;

destructor TWadWriter.Destroy;
var
  i: integer;
begin
  for i := 0 to lumps.Count - 1 do
    if lumps.Objects[i] <> nil then
      lumps.Objects[i].Free;
  lumps.Free;
  Inherited;
end;

procedure TWadWriter.Clear;
var
  i: integer;
begin
  for i := 0 to lumps.Count - 1 do
    if lumps.Objects[i] <> nil then
      lumps.Objects[i].Free;
  lumps.Clear;
end;

procedure TWadWriter.AddData(const lumpname: string; const data: pointer; const size: integer);
var
  m: TMemoryStream;
begin
  m := TMemoryStream.Create;
  m.Write(data^, size);
  lumps.AddObject(UpperCase(lumpname), m);
end;

procedure TWadWriter.AddString(const lumpname: string; const data: string);
var
  m: TMemoryStream;
  i: integer;
begin
  m := TMemoryStream.Create;
  for i := 1 to Length(data) do
    m.Write(data[i], SizeOf(char));
  lumps.AddObject(UpperCase(lumpname), m);
end;

procedure TWadWriter.AddStringList(const lumpname: string; const lst: TStringList);
var
  stmp: string;
begin
  stmp := lst.Text;
  AddString(lumpname, stmp);
end;

procedure TWadWriter.AddSeparator(const lumpname: string);
begin
  lumps.Add(UpperCase(lumpname));
end;

procedure TWadWriter.SaveToStream(const strm: TStream);
var
  h: wadinfo_t;
  la: Pfilelump_tArray;
  i: integer;
  p, ssize: integer;
  m: TMemoryStream;
begin
  p := strm.Position;
  h.identification := PWAD;
  h.numlumps := lumps.Count;
  h.infotableofs := p + SizeOf(wadinfo_t);
  strm.Write(h, SizeOf(h));
  p := strm.Position;
  GetMem(la, lumps.Count * SizeOf(filelump_t));
  strm.Write(la^, lumps.Count * SizeOf(filelump_t));

  for i := 0 to lumps.Count - 1 do
  begin
    la[i].filepos := strm.Position;
    m := lumps.Objects[i] as TMemoryStream;
    if m <> nil then
    begin
      la[i].size := m.Size;
      m.Position := 0;
      strm.Write(m.Memory^, m.Size);
    end
    else
      la[i].size := 0;
    la[i].name := stringtochar8(lumps.Strings[i]);
  end;
  ssize := strm.Position;
  strm.Position := p;
  strm.Write(la^, lumps.Count * SizeOf(filelump_t));
  FreeMem(la, lumps.Count * SizeOf(filelump_t));
  strm.Position := ssize;
end;

procedure TWadWriter.SaveToFile(const fname: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(fname, fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

function AddDataToWAD(const wad: TWADWriter; const lumpname, data: string): boolean;
begin
  if wad <> nil then
  begin
    wad.AddString(lumpname, data);
    Result := True;
  end
  else
    Result := False;
end;

end.

