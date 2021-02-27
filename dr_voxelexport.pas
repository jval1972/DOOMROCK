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
//  Export rock to voxelbuffer
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/doom-rock/
//------------------------------------------------------------------------------

unit dr_voxelexport;

interface

uses
  Windows,
  Classes,
  SysUtils,
  Graphics,
  dr_voxels,
  procrock,
  dr_voxelizer;

procedure DT_CreateVoxelFromRock(const t: rock_t; const vox: voxelbuffer_p;
  const voxsize: integer; const rocktex: TBitmap);

implementation

uses
  dr_defs;

procedure DT_CreateVoxelFacesFromRock(const mVertCount, mFaceCount: integer;
  const mVert: array of fvec5_t; const mFace: array of ivec3_t;
  const scale: single; const vox: voxelbuffer_p;
  const voxsize: integer; const tex: TBitmap; const opaque: boolean);
var
  tri: meshtriangle_t;
  i: integer;
  ofs: integer;
  procedure _make_vertex(const r, g: integer);
  begin
    tri[g].x := mVert[r].x * scale + ofs;
    tri[g].y := voxsize - 1.0 - mVert[r].y * scale;
    tri[g].z := voxsize - 1.0 - mVert[r].z * scale - ofs;
    tri[g].u := mVert[r].u;
    tri[g].v := mVert[r].v;
  end;
begin
  ofs := voxsize div 2;
  for i := 0 to mFaceCount - 1 do
  begin
    _make_vertex(mFace[i].x, 0);
    _make_vertex(mFace[i].y, 1);
    _make_vertex(mFace[i].z, 2);
    DT_VoxelizeTri(@tri, tex, vox, voxsize, opaque);
  end;
end;

procedure DT_CreateVoxelFromRock(const t: rock_t; const vox: voxelbuffer_p;
  const voxsize: integer; const rocktex: TBitmap);
var
  xmin, xmax, ymin, ymax, zmin, zmax: single;
  i: integer;
  scale: single;
begin
  if t.mVertCount = 0 then
    Exit;
  xmin := t.mFace[0].x;
  xmax := t.mFace[0].x;
  ymin := t.mFace[0].y;
  ymax := t.mFace[0].y;
  zmin := t.mFace[0].z;
  zmax := t.mFace[0].z;
  for i := 1 to t.mVertCount - 1 do
  begin
    if xmin > t.mVert[i].x then
      xmin := t.mVert[i].x
    else if xmax < t.mVert[i].x then
      xmax := t.mVert[i].x;
    if ymin > t.mVert[i].y then
      ymin := t.mVert[i].y
    else if ymax < t.mVert[i].y then
      ymax := t.mVert[i].y;
    if zmin > t.mVert[i].z then
      zmin := t.mVert[i].z
    else if zmax < t.mVert[i].z then
      zmax := t.mVert[i].z;
  end;
  ZeroMemory(vox, SizeOf(voxelbuffer_t));

  scale := abs(xmin);
  if abs(xmax) > scale then
    scale := abs(xmax);
  if abs(ymin) > scale then
    scale := abs(ymin);
  if abs(ymax) > scale then
    scale := abs(ymax);
  if abs(zmin) > scale then
    scale := abs(zmin);
  if abs(zmax) > scale then
    scale := abs(zmax);
  scale := 2 * scale;

  scale := (voxsize - 1) / scale;

  DT_CreateVoxelFacesFromRock(t.mVertCount, t.mFaceCount, t.mVert, t.mFace,
    scale, vox, voxsize, rocktex, true);
end;

end.
