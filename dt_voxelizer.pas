//------------------------------------------------------------------------------
//
//  DOOMTREE: Doom Tree Sprite Generator
//  Copyright (C) 2021 by Jim Valavanis
//
// DESCRIPTION:
//  Voxelizer
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : https://sourceforge.net/projects/doom-tree/
//------------------------------------------------------------------------------

unit dt_voxelizer;

interface

uses
  Windows, Classes, SysUtils, Graphics, dt_voxels;

type
  meshvertex_t = record
    x, y, z: single;
    u, v: single;
  end;
  Pmeshvertex_t = ^meshvertex_t;
  meshvertex_tArray = array[0..$FF] of meshvertex_t;
  Pmeshvertex_tArray = ^meshvertex_tArray;

  meshtriangle_t = array[0..2] of meshvertex_t;
  Pmeshtriangle_t = ^meshtriangle_t;
  meshtriangle_tArray = ^meshtriangle_t;
  Pmeshtriangle_tArray = ^meshtriangle_tArray;

  vec3i_t = record
    x, y, z: integer;
  end;
  Pvec3i_t = ^vec3i_t;
  vec3i_tArray = array[0..$FF] of vec3i_t;
  Pvec3i_tArray = ^vec3i_tArray;

  tri3i_t = array[0..2] of vec3i_t;
  Ptri3i_t = ^tri3i_t;
  tri3i_tArray = array[0..$FF] of tri3i_t;
  Ptri3i_tArray = ^tri3i_tArray;


procedure DT_VoxelizeTri(const tri: Pmeshtriangle_t; const tex: TBitmap;
  const vox: voxelbuffer_p; const voxsize: integer; const opaque: boolean);

implementation

uses
  dt_utils;

type
  tri_orientation_t = (
    to_intersect,
    to_xleft,
    to_xright,
    to_yleft,
    to_yright,
    to_zleft,
    to_zright
  );

// Find the position of triangle mesh relative to the voxel
// Used to find potentian intersections with voxel
function tri_sign(const tri: Ptri3i_t; const voxsize: integer): tri_orientation_t;
begin
  if (tri[0].x < 0) and (tri[1].x < 0) and (tri[2].x < 0) then
  begin
    Result := to_xleft;
    Exit;
  end;
  if (tri[0].y < 0) and (tri[1].y < 0) and (tri[2].y < 0) then
  begin
    Result := to_yleft;
    Exit;
  end;
  if (tri[0].z < 0) and (tri[1].z < 0) and (tri[2].z < 0) then
  begin
    Result := to_zleft;
    Exit;
  end;
  if (tri[0].x >= voxsize) and (tri[1].x >= voxsize) and (tri[2].x >= voxsize) then
  begin
    Result := to_xright;
    Exit;
  end;
  if (tri[0].y >= voxsize) and (tri[1].y >= voxsize) and (tri[2].y >= voxsize) then
  begin
    Result := to_yright;
    Exit;
  end;
  if (tri[0].z >= voxsize) and (tri[1].z >= voxsize) and (tri[2].z >= voxsize) then
  begin
    Result := to_zright;
    Exit;
  end;
  Result := to_intersect; // Possible intersection with the voxel buffer
end;

function vert_sq_distance(const v1, v2: Pmeshvertex_t): double;
var
  dx, dy, dz: double;
begin
  dx := v1.x - v2.x; dx := dx * dx;
  dy := v1.y - v2.y; dy := dy * dy;
  dz := v1.z - v2.z; dz := dz * dz;
  Result := dx + dy + dz;
end;

procedure vert_half(const v1, v2: Pmeshvertex_t; var v: meshvertex_t);
begin
  v.x := (v1.x + v2.x) / 2;
  v.y := (v1.y + v2.y) / 2;
  v.z := (v1.z + v2.z) / 2;
  v.u := (v1.u + v2.u) / 2;
  v.v := (v1.v + v2.v) / 2;
end;

procedure DT_VoxelizeTri(const tri: Pmeshtriangle_t; const tex: TBitmap;
  const vox: voxelbuffer_p; const voxsize: integer; const opaque: boolean);
var
  points: tri3i_t;
  i: integer;
  iu, iv: integer;
  dist01, dist12, dist20, maxdist: double;
  maxdistidx: integer;
  tri1: meshtriangle_t;
  v: meshvertex_t;
begin
  // Convert to integer
  for i := 0 to 2 do
  begin
    points[i].x := Round(tri[i].x);
    points[i].y := Round(tri[i].y);
    points[i].z := Round(tri[i].z);
  end;

  // Check if the tri possible intersects with voxel
  if tri_sign(@points, voxsize) <> to_intersect then
    Exit;

  if (points[0].x = points[1].x) and (points[0].x = points[2].x) then
    if (points[0].y = points[1].y) and (points[0].y = points[2].y) then
      if (points[0].z = points[1].z) and (points[0].z = points[2].z) then
      begin
        // The triangle occupies excactly 1 voxel item, time to draw it!
        // First check to see if it is inside the voxel
        if IsIntInRange(points[0].x, 0, voxsize - 1) then
          if IsIntInRange(points[0].y, 0, voxsize - 1) then
            if IsIntInRange(points[0].z, 0, voxsize - 1) then
            begin
              iu := Round(tex.Width * tri[0].u) mod tex.Width;
              iv := Round(tex.Height * tri[0].v) mod tex.Height;
              vox[points[0].x, points[0].y, points[0].z] := tex.Canvas.Pixels[iu, iv];
              if opaque then
                if vox[points[0].x, points[0].y, points[0].z] = 0 then
                  vox[points[0].x, points[0].y, points[0].z] := 1;
            end;
        Exit; // Nothing else to do
      end;


  // Find the biggest triangle line and slpit it
  dist01 := vert_sq_distance(@tri[0], @tri[1]);
  dist12 := vert_sq_distance(@tri[1], @tri[2]);
  dist20 := vert_sq_distance(@tri[2], @tri[0]);

  maxdist := dist01;
  maxdistidx := 0;
  if dist12 > maxdist then
  begin
    maxdist := dist12;
    maxdistidx := 1;
  end;
  if dist20 > maxdist then
  begin
    maxdistidx := 2;
  end;

  case maxdistidx of
  0: // Split line 0-1
    begin
      vert_half(@tri[0], @tri[1], v);
      tri1[0] := tri[0];
      tri1[1] := v;
      tri1[2] := tri[2];
      DT_VoxelizeTri(@tri1, tex, vox, voxsize, opaque);
      tri1[0] := tri[2];
      tri1[1] := v;
      tri1[2] := tri[1];
      DT_VoxelizeTri(@tri1, tex, vox, voxsize, opaque);
    end;
  1: // Split line 1-2
    begin
      vert_half(@tri[1], @tri[2], v);
      tri1[0] := tri[0];
      tri1[1] := tri[1];
      tri1[1] := v;
      DT_VoxelizeTri(@tri1, tex, vox, voxsize, opaque);
      tri1[0] := tri[0];
      tri1[1] := v;
      tri1[2] := tri[2];
      DT_VoxelizeTri(@tri1, tex, vox, voxsize, opaque);
    end;
  2:  // Split line 2-0
    begin
      vert_half(@tri[2], @tri[0], v);
      tri1[0] := tri[0];
      tri1[1] := tri[1];
      tri1[1] := v;
      DT_VoxelizeTri(@tri1, tex, vox, voxsize, opaque);
      tri1[0] := tri[1];
      tri1[1] := v;
      tri1[2] := tri[2];
      DT_VoxelizeTri(@tri1, tex, vox, voxsize, opaque);
    end;
  end;
end;

end.
