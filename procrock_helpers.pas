//------------------------------------------------------------------------------
//
//  DOOMROCK: Doom Rock Sprite Generator
//  Copyright (C) 2021 by Jim Valavanis
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
// DESCRIPTION:
//  Utility functions
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/doom-rock/
//------------------------------------------------------------------------------

unit procrock_helpers;

interface

uses
  Classes, procrock;

procedure PT_SavePropertiesBinary(const p: properties_t; const s: TStream);

procedure PT_LoadPropertiesBinary(const p: properties_t; const s: TStream);

procedure PT_SaveRockToObj(const t: rock_t; const s: TStream);

implementation

uses
  SysUtils;

procedure PT_SavePropertiesBinary(const p: properties_t; const s: TStream);
begin
  with p do
  begin
    s.Write(mVScale, SizeOf(single));
    s.Write(mXScale, SizeOf(single));
    s.Write(mYScale, SizeOf(single));
    s.Write(mZScale, SizeOf(single));
    s.Write(mXDeformFactor, SizeOf(single));
    s.Write(mYDeformFactor, SizeOf(single));
    s.Write(mZDeformFactor, SizeOf(single));
    s.Write(mRDeformFactor, SizeOf(single));
    s.Write(mNumRings, SizeOf(integer));
    s.Write(mNumSegments, SizeOf(integer));
    s.Write(mXOffset, SizeOf(single));
    s.Write(mZOffset, SizeOf(single));
    s.Write(mPitRate, SizeOf(single));
    s.Write(mPitElevation, SizeOf(single));
    s.Write(mGroundLevelHeight, SizeOf(single));
    s.Write(mXCareen, SizeOf(single));
    s.Write(mYCareen, SizeOf(single));
    s.Write(mZCareen, SizeOf(single));
    s.Write(mRecalcUV, SizeOf(boolean));
    s.Write(mSeed, SizeOf(integer));
    s.Write(mRseed, SizeOf(integer));
    s.Write(mComplete, SizeOf(boolean));
    s.Write(mYOffset, SizeOf(single));
    s.Write(mXNegativeCut, SizeOf(single));
    s.Write(mXPositiveCut, SizeOf(single));
    s.Write(mYNegativeCut, SizeOf(single));
    s.Write(mYPositiveCut, SizeOf(single));
    s.Write(mZNegativeCut, SizeOf(single));
    s.Write(mZPositiveCut, SizeOf(single));
    s.Write(mUOffset, SizeOf(single));
    s.Write(mVOffset, SizeOf(single));
    s.Write(mRecessRate, SizeOf(single));
    s.Write(mRecessStrength, SizeOf(single));
  end;
end;

procedure PT_LoadPropertiesBinary(const p: properties_t; const s: TStream);
begin
  with p do
  begin
    DefaultValues(0);
    s.Read(mVScale, SizeOf(single));
    s.Read(mXScale, SizeOf(single));
    s.Read(mYScale, SizeOf(single));
    s.Read(mZScale, SizeOf(single));
    s.Read(mXDeformFactor, SizeOf(single));
    s.Read(mYDeformFactor, SizeOf(single));
    s.Read(mZDeformFactor, SizeOf(single));
    s.Read(mRDeformFactor, SizeOf(single));
    s.Read(mNumRings, SizeOf(integer));
    s.Read(mNumSegments, SizeOf(integer));
    s.Read(mXOffset, SizeOf(single));
    s.Read(mZOffset, SizeOf(single));
    s.Read(mPitRate, SizeOf(single));
    s.Read(mPitElevation, SizeOf(single));
    s.Read(mGroundLevelHeight, SizeOf(single));
    s.Read(mXCareen, SizeOf(single));
    s.Read(mYCareen, SizeOf(single));
    s.Read(mZCareen, SizeOf(single));
    s.Read(mRecalcUV, SizeOf(boolean));
    s.Read(mSeed, SizeOf(integer));
    s.Read(mRseed, SizeOf(integer));
    if s.Position < s.Size then
    begin
      s.Read(mComplete, SizeOf(boolean));
      s.Read(mYOffset, SizeOf(single));
      s.Read(mXNegativeCut, SizeOf(single));
      s.Read(mXPositiveCut, SizeOf(single));
      s.Read(mYNegativeCut, SizeOf(single));
      s.Read(mYPositiveCut, SizeOf(single));
      s.Read(mZNegativeCut, SizeOf(single));
      s.Read(mZPositiveCut, SizeOf(single));
      s.Read(mUOffset, SizeOf(single));
      s.Read(mVOffset, SizeOf(single));
      s.Read(mRecessRate, SizeOf(single));
      s.Read(mRecessStrength, SizeOf(single));
    end;
  end;
end;

procedure PT_SaveRockToObj(const t: rock_t; const s: TStream);
var
  i: integer;
  a, b, c: integer;
  buf: string;

  function F2S(const f: single): string;
  var
    x: integer;
  begin
    Result := Format('%1.16f', [f]);
    for x := 1 to Length(Result) do
      if (Result[x] = ',') or (Result[x] = DecimalSeparator) then
        Result[x] := '.';
  end;

  procedure Add(const s: string);
  begin
    buf := buf + s;
  end;

begin
  buf := '';

  Add('mtllib rock.mtl'#13#10);
  for i := 0 to t.mVertCount - 1 do
    Add('v ' + F2S(t.mVert[i].x) + ' ' + F2S(t.mVert[i].y) + ' ' +  F2S(t.mVert[i].z) + #13#10);

  for i := 0 to t.mVertCount - 1 do
    Add('vt ' + F2S(t.mVert[i].u) + ' ' + F2S(t.mVert[i].v) + #13#10);

  Add('g rock\nusemtl rock'#13#10);
  for i := 0 to t.mFaceCount - 1 do
  begin
    a := t.mFace[i].x + 1;
    b := t.mFace[i].y + 1;
    c := t.mFace[i].z + 1;
    Add(Format('f %d/%d/%d %d/%d/%d %d/%d/%d'#13#10, [a, a, a, b, b, b, c, c, c]));
  end;

  for i := 1 to Length(buf) do
    s.Write(buf[i], SizeOf(char));
end;

end.
