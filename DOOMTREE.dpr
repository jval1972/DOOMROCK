//------------------------------------------------------------------------------
//
//  DOOMTREE: Doom Tree Sprite Generator
//  Copyright (C) 2021 by Jim Valavanis
//
// DESCRIPTION:
//  Main programm
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : https://sourceforge.net/projects/doom-tree/
//------------------------------------------------------------------------------

program DOOMTREE;

uses
  FastMM4 in 'FastMM4.pas',
  FastMM4Messages in 'FastMM4Messages.pas',
  Forms,
  main in 'main.pas' {Form1},
  dglOpenGL in 'dglOpenGL.pas',
  proctree in 'proctree.pas',
  dt_gl in 'dt_gl.pas',
  dt_undo in 'dt_undo.pas',
  dt_binary in 'dt_binary.pas',
  dt_filemenuhistory in 'dt_filemenuhistory.pas',
  dt_utils in 'dt_utils.pas',
  pngextra in 'pngextra.pas',
  pngimage in 'pngimage.pas',
  pnglang in 'pnglang.pas',
  xTGA in 'xTGA.pas',
  zBitmap in 'zBitmap.pas',
  zlibpas in 'zlibpas.pas',
  proctree_helpers in 'proctree_helpers.pas',
  dt_slider in 'dt_slider.pas',
  dt_soft3d in 'dt_soft3d.pas',
  frm_exportsprite in 'frm_exportsprite.pas' {ExportSpriteForm},
  frm_spriteprefix in 'frm_spriteprefix.pas' {SpritePrefixForm},
  dt_palettes in 'dt_palettes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'DOOMTREE Sprite Generator';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

