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
//  MD2 model export
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/doom-rock/
//------------------------------------------------------------------------------

unit dr_md2;

interface

uses
  Windows, Classes, SysUtils, procrock;

procedure SaveRockToMD2Stream(const rock: rock_t; const strm: TStream; const name: string);

implementation

uses
  dr_triangulation;

const
  NUMVERTEXNORMALS = 162;
  r_avertexnormals: array[0..NUMVERTEXNORMALS - 1, 0..2] of single = (
    (-0.525731,  0.000000,  0.850651),
    (-0.442863,  0.238856,  0.864188),
    (-0.295242,  0.000000,  0.955423),
    (-0.309017,  0.500000,  0.809017),
    (-0.162460,  0.262866,  0.951056),
    ( 0.000000,  0.000000,  1.000000),
    ( 0.000000,  0.850651,  0.525731),
    (-0.147621,  0.716567,  0.681718),
    ( 0.147621,  0.716567,  0.681718),
    ( 0.000000,  0.525731,  0.850651),
    ( 0.309017,  0.500000,  0.809017),
    ( 0.525731,  0.000000,  0.850651),
    ( 0.295242,  0.000000,  0.955423),
    ( 0.442863,  0.238856,  0.864188),
    ( 0.162460,  0.262866,  0.951056),
    (-0.681718,  0.147621,  0.716567),
    (-0.809017,  0.309017,  0.500000),
    (-0.587785,  0.425325,  0.688191),
    (-0.850651,  0.525731,  0.000000),
    (-0.864188,  0.442863,  0.238856),
    (-0.716567,  0.681718,  0.147621),
    (-0.688191,  0.587785,  0.425325),
    (-0.500000,  0.809017,  0.309017),
    (-0.238856,  0.864188,  0.442863),
    (-0.425325,  0.688191,  0.587785),
    (-0.716567,  0.681718, -0.147621),
    (-0.500000,  0.809017, -0.309017),
    (-0.525731,  0.850651,  0.000000),
    ( 0.000000,  0.850651, -0.525731),
    (-0.238856,  0.864188, -0.442863),
    ( 0.000000,  0.955423, -0.295242),
    (-0.262866,  0.951056, -0.162460),
    ( 0.000000,  1.000000,  0.000000),
    ( 0.000000,  0.955423,  0.295242),
    (-0.262866,  0.951056,  0.162460),
    ( 0.238856,  0.864188,  0.442863),
    ( 0.262866,  0.951056,  0.162460),
    ( 0.500000,  0.809017,  0.309017),
    ( 0.238856,  0.864188, -0.442863),
    ( 0.262866,  0.951056, -0.162460),
    ( 0.500000,  0.809017, -0.309017),
    ( 0.850651,  0.525731,  0.000000),
    ( 0.716567,  0.681718,  0.147621),
    ( 0.716567,  0.681718, -0.147621),
    ( 0.525731,  0.850651,  0.000000),
    ( 0.425325,  0.688191,  0.587785),
    ( 0.864188,  0.442863,  0.238856),
    ( 0.688191,  0.587785,  0.425325),
    ( 0.809017,  0.309017,  0.500000),
    ( 0.681718,  0.147621,  0.716567),
    ( 0.587785,  0.425325,  0.688191),
    ( 0.955423,  0.295242,  0.000000),
    ( 1.000000,  0.000000,  0.000000),
    ( 0.951056,  0.162460,  0.262866),
    ( 0.850651, -0.525731,  0.000000),
    ( 0.955423, -0.295242,  0.000000),
    ( 0.864188, -0.442863,  0.238856),
    ( 0.951056, -0.162460,  0.262866),
    ( 0.809017, -0.309017,  0.500000),
    ( 0.681718, -0.147621,  0.716567),
    ( 0.850651,  0.000000,  0.525731),
    ( 0.864188,  0.442863, -0.238856),
    ( 0.809017,  0.309017, -0.500000),
    ( 0.951056,  0.162460, -0.262866),
    ( 0.525731,  0.000000, -0.850651),
    ( 0.681718,  0.147621, -0.716567),
    ( 0.681718, -0.147621, -0.716567),
    ( 0.850651,  0.000000, -0.525731),
    ( 0.809017, -0.309017, -0.500000),
    ( 0.864188, -0.442863, -0.238856),
    ( 0.951056, -0.162460, -0.262866),
    ( 0.147621,  0.716567, -0.681718),
    ( 0.309017,  0.500000, -0.809017),
    ( 0.425325,  0.688191, -0.587785),
    ( 0.442863,  0.238856, -0.864188),
    ( 0.587785,  0.425325, -0.688191),
    ( 0.688191,  0.587785, -0.425325),
    (-0.147621,  0.716567, -0.681718),
    (-0.309017,  0.500000, -0.809017),
    ( 0.000000,  0.525731, -0.850651),
    (-0.525731,  0.000000, -0.850651),
    (-0.442863,  0.238856, -0.864188),
    (-0.295242,  0.000000, -0.955423),
    (-0.162460,  0.262866, -0.951056),
    ( 0.000000,  0.000000, -1.000000),
    ( 0.295242,  0.000000, -0.955423),
    ( 0.162460,  0.262866, -0.951056),
    (-0.442863, -0.238856, -0.864188),
    (-0.309017, -0.500000, -0.809017),
    (-0.162460, -0.262866, -0.951056),
    ( 0.000000, -0.850651, -0.525731),
    (-0.147621, -0.716567, -0.681718),
    ( 0.147621, -0.716567, -0.681718),
    ( 0.000000, -0.525731, -0.850651),
    ( 0.309017, -0.500000, -0.809017),
    ( 0.442863, -0.238856, -0.864188),
    ( 0.162460, -0.262866, -0.951056),
    ( 0.238856, -0.864188, -0.442863),
    ( 0.500000, -0.809017, -0.309017),
    ( 0.425325, -0.688191, -0.587785),
    ( 0.716567, -0.681718, -0.147621),
    ( 0.688191, -0.587785, -0.425325),
    ( 0.587785, -0.425325, -0.688191),
    ( 0.000000, -0.955423, -0.295242),
    ( 0.000000, -1.000000,  0.000000),
    ( 0.262866, -0.951056, -0.162460),
    ( 0.000000, -0.850651,  0.525731),
    ( 0.000000, -0.955423,  0.295242),
    ( 0.238856, -0.864188,  0.442863),
    ( 0.262866, -0.951056,  0.162460),
    ( 0.500000, -0.809017,  0.309017),
    ( 0.716567, -0.681718,  0.147621),
    ( 0.525731, -0.850651,  0.000000),
    (-0.238856, -0.864188, -0.442863),
    (-0.500000, -0.809017, -0.309017),
    (-0.262866, -0.951056, -0.162460),
    (-0.850651, -0.525731,  0.000000),
    (-0.716567, -0.681718, -0.147621),
    (-0.716567, -0.681718,  0.147621),
    (-0.525731, -0.850651,  0.000000),
    (-0.500000, -0.809017,  0.309017),
    (-0.238856, -0.864188,  0.442863),
    (-0.262866, -0.951056,  0.162460),
    (-0.864188, -0.442863,  0.238856),
    (-0.809017, -0.309017,  0.500000),
    (-0.688191, -0.587785,  0.425325),
    (-0.681718, -0.147621,  0.716567),
    (-0.442863, -0.238856,  0.864188),
    (-0.587785, -0.425325,  0.688191),
    (-0.309017, -0.500000,  0.809017),
    (-0.147621, -0.716567,  0.681718),
    (-0.425325, -0.688191,  0.587785),
    (-0.162460, -0.262866,  0.951056),
    ( 0.442863, -0.238856,  0.864188),
    ( 0.162460, -0.262866,  0.951056),
    ( 0.309017, -0.500000,  0.809017),
    ( 0.147621, -0.716567,  0.681718),
    ( 0.000000, -0.525731,  0.850651),
    ( 0.425325, -0.688191,  0.587785),
    ( 0.587785, -0.425325,  0.688191),
    ( 0.688191, -0.587785,  0.425325),
    (-0.955423,  0.295242,  0.000000),
    (-0.951056,  0.162460,  0.262866),
    (-1.000000,  0.000000,  0.000000),
    (-0.850651,  0.000000,  0.525731),
    (-0.955423, -0.295242,  0.000000),
    (-0.951056, -0.162460,  0.262866),
    (-0.864188,  0.442863, -0.238856),
    (-0.951056,  0.162460, -0.262866),
    (-0.809017,  0.309017, -0.500000),
    (-0.864188, -0.442863, -0.238856),
    (-0.951056, -0.162460, -0.262866),
    (-0.809017, -0.309017, -0.500000),
    (-0.681718,  0.147621, -0.716567),
    (-0.681718, -0.147621, -0.716567),
    (-0.850651,  0.000000, -0.525731),
    (-0.688191,  0.587785, -0.425325),
    (-0.587785,  0.425325, -0.688191),
    (-0.425325,  0.688191, -0.587785),
    (-0.425325, -0.688191, -0.587785),
    (-0.587785, -0.425325, -0.688191),
    (-0.688191, -0.587785, -0.425325)
  );

function FindNormalIdx(x, y, z: single): integer;
var
  dist: single;
  maxdist: single;
  len: single;
  i: integer;
begin
  len := Sqrt(x * x + y * y + z * z);
  if len <> 0.0 then
  begin
    x := x / len;
    y := y / len;
    z := z / len;
  end;

  Result := 0;
  maxdist := Sqr(r_avertexnormals[0][0] - x) + Sqr(r_avertexnormals[0][1] - y) + Sqr(r_avertexnormals[0][2] - z);
  for i := 1 to NUMVERTEXNORMALS - 1 do
  begin
    dist := Sqr(r_avertexnormals[i][0] - x) + Sqr(r_avertexnormals[i][1] - y) + Sqr(r_avertexnormals[i][2] - z);
    if dist < maxdist then
    begin
      maxdist := dist;
      Result := i;
    end;
  end;
end;

const
  // Magic number that identifies MD2 files (ASCII: 'IDP2').
  MD2_MAGIC = $32504449;

type
  TMD2_Index_List = record
    a, b, c: Integer;
    a_s, a_t,
    b_s, b_t,
    c_s, c_t: Single;
  end;
  TMD2_Index_List_Array = array[0..$FFFF] of TMD2_Index_List;
  PMD2_Index_List_Array = ^TMD2_Index_List_Array;

  TMD2DstVert_T = packed record
    s: SmallInt;
    t: SmallInt;
  end;
  TMD2DstVert_TArray = array[0..$FFFF] of TMD2DstVert_T;
  PMD2DstVert_TArray = ^TMD2DstVert_TArray;

  TMD2Triangle_T = packed record
    index_xyz: packed array[0..2] of SmallInt;
    index_st: packed array[0..2] of SmallInt;
  end;

  TMD2Trivertx_T = packed record
    v: packed array[0..2] of Byte;
    lightnormalindex: byte;
  end;

  TMD2Skin_T = record
    name: packed array[0..63] of char;
  end;

  TMD2GlCmd_T = packed record
    s, t: single;
    index: integer;
  end;

  PMD2AliasFrame_T = ^TMD2AliasFrame_T;
  TMD2AliasFrame_T = packed record
    scale: array[0..2] of Single;
    translate: array[0..2] of Single;
    name: packed array[0..15] of Char;
//    verts: array[0..0] of TMD2Trivertx_T;
  end;

  TDmd2_T = record
    ident: Integer;       // magic number. must be equal to "IDP2"
    version: Integer;     // md2 version. must be equal to 8

    skinWidth: Integer;   // width of the texture
    skinHeight: Integer;  // height of the texture
    framesize: Integer;   // size of one frame in bytes

    num_skins: Integer;   // number of textures
    num_xyz: Integer;     // number of vertices
    num_st: Integer;      // number of texture coordinates
    num_tris: Integer;    // number of triangles
    num_glcmds: Integer;  // number of opengl commands
    num_frames: Integer;  // total number of frames

    ofs_skins: Integer;   // offset to skin names (64 bytes each)
    ofs_st: Integer;      // offset to s-t texture coordinates
    ofs_tris: Integer;    // offset to triangles
    ofs_frames: Integer;  // offset to frame data
    ofs_glcmds: Integer;  // offset to opengl commands
    ofs_end: Integer;     // offset to end of file
  end;

const
  SKINSIZE = 256;

procedure SaveRockToMD2Stream(const rock: rock_t; const strm: TStream; const name: string);
var
  h: TDmd2_T;
  i, j: integer;
  skin: TMD2Skin_T;
  st: TMD2DstVert_T;
  tri: TMD2Triangle_T;
  frame: TMD2AliasFrame_T;
  trivert: TMD2Trivertx_T;
  v: integer;
  glcmd: integer;
  glcmds: TMD2GlCmd_T;
  start: integer;
  minx, maxx, miny, maxy, minz, maxz: single;
  dx, dy, dz: single;
  scalex, scaley, scalez: single;
  offsetx, offsety, offsetz: single;
  bottomPoints: array of TTriangulationPoint;
  bottomPoly: TTriangulationPolygons;
  bottomTris: TTriangulationTriangles;
begin
  start := strm.Position;

  rock.generate;

  SetLength(bottomTris, 0);

  if not rock.mProperties.mComplete then
  begin
    SetLength(bottomPoly, 1);
    SetLength(bottomPoly[0], 0);
    for i := rock.mVertCount - 1 downto 0 do
      if rock.mVert[i].ring = rock.mProperties.mNumRings then
      begin
        SetLength(bottomPoly[0], Length(bottomPoly[0]) + 1);
        bottomPoly[0][Length(bottomPoly[0]) - 1].x := rock.mVert[i].x;
        bottomPoly[0][Length(bottomPoly[0]) - 1].y := rock.mVert[i].z;
      end;
    if Length(bottomPoly[0]) > 0 then
    begin
      SetLength(bottomPoly[0], Length(bottomPoly[0]) + 1);
      bottomPoly[0][Length(bottomPoly[0]) - 1] := bottomPoly[0][0];
      TriangulateDelaunayClipping(bottomPoly, bottomTris);
    end;
  end;

  ZeroMemory(@h, SizeOf(TDmd2_T));
  h.ident := MD2_MAGIC;
  h.version := 8;
  h.skinWidth := SKINSIZE;
  h.skinHeight := SKINSIZE;
  h.num_skins := 1;
  h.num_xyz := rock.mVertCount + 3 * Length(bottomTris);
  h.num_st := rock.mVertCount + 3 * Length(bottomTris);
  h.num_tris := rock.mFaceCount + Length(bottomTris);
  h.num_glcmds := (rock.mFaceCount + Length(bottomTris)) * 10 + 1;
  h.num_frames := 1;
  strm.Write(h, SizeOf(TDmd2_T));

  // Skins
  h.ofs_skins := strm.Position - start;
  ZeroMemory(@skin, SizeOf(TMD2Skin_T));
  for i := 1 to Length(name) do
  begin
    skin.name[i - 1] := name[i];
    if i = 64 then
      Break;
  end;
  strm.Write(skin, SizeOf(TMD2Skin_T));

  // s,t offsets
  h.ofs_st := strm.Position - start;
  for i := 0 to rock.mVertCount - 1 do
  begin
    st.s := Round(rock.mVert[i].u * SKINSIZE);
    st.t := Round(rock.mVert[i].v * SKINSIZE);
    strm.Write(st, SizeOf(TMD2DstVert_T));
  end;

  for i := 0 to Length(bottomTris) - 1 do
    for j := 0 to 2 do
    begin
      st.s := Round(bottomTris[i][j].x * SKINSIZE);
      st.t := Round(bottomTris[i][j].y * SKINSIZE);
      strm.Write(st, SizeOf(TMD2DstVert_T));
    end;

  // Tris
  h.ofs_tris := strm.Position - start;
  for i := 0 to rock.mFaceCount - 1 do
  begin
    tri.index_xyz[0] := rock.mFace[i].x;
    tri.index_xyz[1] := rock.mFace[i].y;
    tri.index_xyz[2] := rock.mFace[i].z;
    tri.index_st[0] := rock.mFace[i].x;
    tri.index_st[1] := rock.mFace[i].y;
    tri.index_st[2] := rock.mFace[i].z;
    strm.Write(tri, SizeOf(TMD2Triangle_T));
  end;

  for i := 0 to Length(bottomTris) - 1 do
  begin
    tri.index_xyz[0] := rock.mVertCount + i * 3;
    tri.index_xyz[1] := rock.mVertCount + i * 3 + 1;
    tri.index_xyz[2] := rock.mVertCount + i * 3 + 2;
    tri.index_st[0] := rock.mVertCount + i * 3;
    tri.index_st[1] := rock.mVertCount + i * 3 + 1;
    tri.index_st[2] := rock.mVertCount + i * 3 + 2;
    strm.Write(tri, SizeOf(TMD2Triangle_T));
  end;

  minx := 100000.0;
  maxx := -100000.0;
  miny := 100000.0;
  maxy := -100000.0;
  minz := 100000.0;
  maxz := -100000.0;
  for i := 0 to rock.mVertCount - 1 do
  begin
    if rock.mVert[i].x < minx then
      minx := rock.mVert[i].x;
    if rock.mVert[i].x > maxx then
      maxx := rock.mVert[i].x;
    if rock.mVert[i].y < miny then
      miny := rock.mVert[i].y;
    if rock.mVert[i].y > maxy then
      maxy := rock.mVert[i].y;
    if rock.mVert[i].z < minz then
      minz := rock.mVert[i].z;
    if rock.mVert[i].z > maxz then
      maxz := rock.mVert[i].z;
  end;
  dx := maxx - minx;
  dy := maxy - miny;
  dz := maxz - minz;
  scalex := 255 / dx;
  scaley := 255 / dy;
  scalez := 255 / dz;
  offsetx := minx;
  offsety := miny;
  offsetz := minz;

  // Frames
  h.ofs_frames := strm.Position - start;
  frame.scale[0] := dx;
  frame.scale[1] := dz;
  frame.scale[2] := dy;
  frame.translate[0] := offsetx * 255;
  frame.translate[1] := offsetz * 255;
  frame.translate[2] := offsety * 255;
  frame.name[0] := 'd';
  frame.name[1] := 'e';
  frame.name[2] := 'f';
  frame.name[3] := 'a';
  frame.name[4] := 'u';
  frame.name[5] := 'l';
  frame.name[6] := 't';
  frame.name[7] := '0';
  frame.name[8] := #0;
  frame.name[9] := #0;
  frame.name[10] := #0;
  frame.name[11] := #0;
  frame.name[12] := #0;
  frame.name[13] := #0;
  frame.name[14] := #0;
  frame.name[15] := #0;
  strm.Write(frame, SizeOf(TMD2AliasFrame_T));
  for i := 0 to rock.mVertCount - 1 do
  begin
    v := Round((rock.mVert[i].x - offsetx) * scalex);
    trivert.v[0] := v;
    v := Round((rock.mVert[i].z - offsetz) * scalez);
    trivert.v[1] := v;
    v := Round((rock.mVert[i].y - offsety) * scaley);
    trivert.v[2] := v;
    trivert.lightnormalindex := FindNormalIdx(rock.mVert[i].x, rock.mVert[i].z, rock.mVert[i].y);
    strm.Write(trivert, SizeOf(TMD2Trivertx_T));
  end;
  for i := 0 to Length(bottomTris) - 1 do
    for j := 0 to 2 do
    begin
      v := Round((bottomTris[i][j].x - offsetx) * scalex);
      trivert.v[0] := v;
      v := Round((bottomTris[i][j].y - offsetz) * scalez);
      trivert.v[1] := v;
      v := Round((0.0 - offsety) * scaley);
      trivert.v[2] := v;
      trivert.lightnormalindex := FindNormalIdx(bottomTris[i][j].x, bottomTris[i][j].y, 0.0);
      strm.Write(trivert, SizeOf(TMD2Trivertx_T));
    end;

  h.framesize := strm.Position - h.ofs_frames;

  h.ofs_glcmds := strm.Position - start;
  for i := 0 to rock.mFaceCount - 1 do
  begin
    glcmd := 3;
    strm.Write(glcmd, SizeOf(Integer));
    glcmds.s := rock.mVert[rock.mFace[i].x].u;
    glcmds.t := rock.mVert[rock.mFace[i].x].v;
    glcmds.index := rock.mFace[i].x;
    strm.Write(glcmds, SizeOf(TMD2GlCmd_T));
    glcmds.s := rock.mVert[rock.mFace[i].y].u;
    glcmds.t := rock.mVert[rock.mFace[i].y].v;
    glcmds.index := rock.mFace[i].y;
    strm.Write(glcmds, SizeOf(TMD2GlCmd_T));
    glcmds.s := rock.mVert[rock.mFace[i].z].u;
    glcmds.t := rock.mVert[rock.mFace[i].z].v;
    glcmds.index := rock.mFace[i].z;
    strm.Write(glcmds, SizeOf(TMD2GlCmd_T));
  end;

  for i := 0 to Length(bottomTris) - 1 do
  begin
    glcmd := 3;
    strm.Write(glcmd, SizeOf(Integer));
    for j := 0 to 2 do
    begin
      glcmds.s := bottomTris[i][j].x;
      glcmds.t := bottomTris[i][j].y;
      glcmds.index := rock.mVertCount + i * 3 + j;
      strm.Write(glcmds, SizeOf(TMD2GlCmd_T));
    end;
  end;

  glcmd := 0;
  strm.Write(glcmd, SizeOf(Integer));
  h.ofs_end := strm.Position - start;

  strm.Position := start;
  strm.Write(h, SizeOf(TDmd2_T));
  strm.Position := h.ofs_end;
end;

end.
