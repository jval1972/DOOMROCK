//------------------------------------------------------------------------------
//
//  DOOMTREE: Doom Tree Sprite Generator
//  Copyright (C) 2021 by Jim Valavanis
//
// DESCRIPTION:
//  Export tree to voxelbuffer
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : https://sourceforge.net/projects/doom-tree/
//------------------------------------------------------------------------------

unit dt_voxelexport;

interface

uses
  Windows,
  Classes,
  SysUtils,
  Graphics,
  dt_voxels,
  proctree,
  dt_voxelizer;

procedure DT_CreateVoxelFromTree(const t: tree_t; const vox: voxelbuffer_p;
  const voxsize: integer; const trunktex, twigtex: TBitmap);

implementation

uses
  dt_defs;

procedure DT_CreateVoxelFacesFromTree(const mVertCount, mFaceCount: integer;
  const mVert, mNormal: array of fvec3_t; const mUV: array of fvec2_t;
  const mFace: array of ivec3_t; const scale: single; const vox: voxelbuffer_p;
  const voxsize: integer; const tex: TBitmap; const opaque: boolean);
var
  tri: meshtriangle_t;
  i: integer;
  ofs: integer;
  procedure _make_vertex(const r, g: integer);
  begin
    tri[g].x := mVert[r].x * scale + ofs;
    tri[g].y := voxsize - 1.0 - mVert[r].y * scale;
    tri[g].z := mVert[r].z * scale + ofs;
    tri[g].u := mUV[r].u;
    tri[g].v := mUV[r].v;
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

procedure DT_CreateVoxelFromTree(const t: tree_t; const vox: voxelbuffer_p;
  const voxsize: integer; const trunktex, twigtex: TBitmap);
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
  for i := 0 to t.mTwigVertCount - 1 do
  begin
    if xmin > t.mTwigVert[i].x then
      xmin := t.mTwigVert[i].x
    else if xmax < t.mTwigVert[i].x then
      xmax := t.mTwigVert[i].x;
    if ymin > t.mTwigVert[i].y then
      ymin := t.mTwigVert[i].y
    else if ymax < t.mTwigVert[i].y then
      ymax := t.mTwigVert[i].y;
    if zmin > t.mTwigVert[i].z then
      zmin := t.mTwigVert[i].z
    else if zmax < t.mTwigVert[i].z then
      zmax := t.mTwigVert[i].z;
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

  scale := (voxsize - 1) / scale;

  DT_CreateVoxelFacesFromTree(t.mVertCount, t.mFaceCount, t.mVert, t.mNormal,
    t.mUV, t.mFace, scale, vox, voxsize, trunktex, true);

  if opt_rendertwig then
    DT_CreateVoxelFacesFromTree(t.mTwigVertCount, t.mTwigFaceCount, t.mTwigVert,
      t.mTwigNormal, t.mTwigUV, t.mTwigFace, scale, vox, voxsize, twigtex, false);
end;

end.
