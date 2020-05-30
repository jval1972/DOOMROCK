program ptree;

uses
  FastMM4 in 'FastMM4.pas',
  FastMM4Messages in 'FastMM4Messages.pas',
  Forms,
  main in 'main.pas' {Form1},
  dglOpenGL in 'dglOpenGL.pas',
  proctree in 'proctree.pas',
  pt_gl in 'pt_gl.pas',
  pt_undo in 'pt_undo.pas',
  pt_binary in 'pt_binary.pas',
  pt_filemenuhistory in 'pt_filemenuhistory.pas',
  pt_utils in 'pt_utils.pas',
  pngextra in 'pngextra.pas',
  pngimage in 'pngimage.pas',
  pnglang in 'pnglang.pas',
  xTGA in 'xTGA.pas',
  zBitmap in 'zBitmap.pas',
  zlibpas in 'zlibpas.pas',
  proctree_helpers in 'proctree_helpers.pas',
  pt_slider in 'pt_slider.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Procedural Tree Generator';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

