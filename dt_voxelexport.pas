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
  const mFace: array of ivec3_t; const scale: double; const vox: voxelbuffer_p;
  const voxsize: integer; const tex: TBitmap; const opaque: boolean);
var
  tri: meshtriangle_t;
  i: integer;
  ofs: integer;
  procedure _make_vertex(const r, g: integer);
  begin
    tri[g].x := mVert[r].x * scale + ofs;
    tri[g].y := mVert[r].y * scale;
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
  xmin, xmax, ymin, ymax, zmin, zmax: integer;
  i: integer;
  scale: double;
begin
  if t.mVertCount = 0 then
    Exit;
  xmin := t.mFace[0].x;
  xmax := t.mFace[0].x;
  ymin := t.mFace[0].y;
  ymax := t.mFace[0].y;
  zmin := t.mFace[0].z;
  zmax := t.mFace[0].z;
  for i := 1 to t.mFaceCount - 1 do
  begin
    if xmin > t.mFace[0].x then
      xmin := t.mFace[0].x
    else if xmax < t.mFace[0].x then
      xmax := t.mFace[0].x;
    if ymin > t.mFace[0].y then
      ymin := t.mFace[0].y
    else if ymax < t.mFace[0].y then
      ymax := t.mFace[0].y;
    if xmin > t.mFace[0].x then
      xmin := t.mFace[0].x
    else if zmax < t.mFace[0].z then
      zmax := t.mFace[0].z;
  end;
  for i := 0 to t.mTwigFaceCount - 1 do
  begin
    if xmin > t.mTwigFace[0].x then
      xmin := t.mTwigFace[0].x
    else if xmax < t.mTwigFace[0].x then
      xmax := t.mTwigFace[0].x;
    if ymin > t.mTwigFace[0].y then
      ymin := t.mTwigFace[0].y
    else if ymax < t.mTwigFace[0].y then
      ymax := t.mTwigFace[0].y;
    if xmin > t.mTwigFace[0].x then
      xmin := t.mTwigFace[0].x
    else if zmax < t.mTwigFace[0].z then
      zmax := t.mTwigFace[0].z;
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

  scale := voxsize / (1.0 + scale);

  DT_CreateVoxelFacesFromTree(t.mVertCount, t.mFaceCount, t.mVert, t.mNormal,
    t.mUV, t.mFace, scale, vox, voxsize, trunktex, true);

  if opt_rendertwig then
    DT_CreateVoxelFacesFromTree(t.mTwigVertCount, t.mTwigFaceCount, t.mTwigVert,
      t.mTwigNormal, t.mTwigUV, t.mTwigFace, scale, vox, voxsize, twigtex, false);
end;

end.
