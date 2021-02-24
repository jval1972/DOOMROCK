//------------------------------------------------------------------------------
//
//  DOOMTREE: Doom Tree Sprite Generator
//  Copyright (C) 2021 by Jim Valavanis
//
// DESCRIPTION:
//  Software Rendering Library
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : https://sourceforge.net/projects/doom-tree/
//------------------------------------------------------------------------------

unit dt_voxelizer;

interface

uses
  Windows, Classes, SysUtils, Graphics;

const
  MAXVOXELSIZE = 256;

type
  voxelitem_t = LongWord;
  voxelitem_p = ^voxelitem_t;
  voxelbuffer_t = array[0..MAXVOXELSIZE - 1, 0..MAXVOXELSIZE - 1, 0..MAXVOXELSIZE - 1] of voxelitem_t;
  voxelbuffer_p = ^voxelbuffer_t;

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

procedure DT_VoxelizeTri(const tri: meshtriangle_t; const tex: TBitmap; const vox: voxelbuffer_p);

implementation

procedure DT_VoxelizeTri(const tri: meshtriangle_t; const tex: TBitmap;
  const vox: voxelbuffer_p; const voxsize: integer);
begin
end;

end.
 