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
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/doom-rock/
//------------------------------------------------------------------------------

program DOOMROCK;

uses
  FastMM4 in 'FastMM4.pas',
  FastMM4Messages in 'FastMM4Messages.pas',
  Forms,
  main in 'main.pas' {Form1},
  dglOpenGL in 'dglOpenGL.pas',
  procrock in 'procrock.pas',
  dr_gl in 'dr_gl.pas',
  dr_undo in 'dr_undo.pas',
  dr_binary in 'dr_binary.pas',
  dr_filemenuhistory in 'dr_filemenuhistory.pas',
  dr_utils in 'dr_utils.pas',
  pngextra in 'pngextra.pas',
  pngimage in 'pngimage.pas',
  pnglang in 'pnglang.pas',
  xTGA in 'xTGA.pas',
  zBitmap in 'zBitmap.pas',
  zlibpas in 'zlibpas.pas',
  procrock_helpers in 'procrock_helpers.pas',
  dr_slider in 'dr_slider.pas',
  dr_soft3d in 'dr_soft3d.pas',
  frm_exportsprite in 'frm_exportsprite.pas' {ExportSpriteForm},
  frm_spriteprefix in 'frm_spriteprefix.pas' {SpritePrefixForm},
  dr_palettes in 'dr_palettes.pas',
  dr_wadwriter in 'dr_wadwriter.pas',
  dr_wad in 'dr_wad.pas',
  dr_doompatch in 'dr_doompatch.pas',
  dr_defs in 'dr_defs.pas',
  dr_voxelizer in 'dr_voxelizer.pas',
  dr_voxels in 'dr_voxels.pas',
  dr_voxelexport in 'dr_voxelexport.pas',
  frm_exportvoxel in 'frm_exportvoxel.pas' {ExportVoxelForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'DOOMROCK Sprite Generator';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

