//------------------------------------------------------------------------------
//
//  DOOMTREE: Doom Tree Sprite Generator
//  Copyright (C) 2021 by Jim Valavanis
//
// DESCRIPTION:
//  Main Form
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : https://sourceforge.net/projects/doom-tree/
//------------------------------------------------------------------------------

unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, pngimage, xTGA, jpeg, zBitmap, ComCtrls, ExtCtrls, Buttons, Menus,
  StdCtrls, AppEvnts, ExtDlgs, clipbrd, ToolWin, dglOpenGL, proctree, dt_undo,
  dt_filemenuhistory, dt_slider;

type
  TForm1 = class(TForm)
    ColorDialog1: TColorDialog;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Open2: TMenuItem;
    Save1: TMenuItem;
    Savesa1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    ApplicationEvents1: TApplicationEvents;
    OpenDialog1: TOpenDialog;
    Undo1: TMenuItem;
    Redo1: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    Timer1: TTimer;
    StatusBar1: TStatusBar;
    Options1: TMenuItem;
    SavePictureDialog1: TSavePictureDialog;
    N4: TMenuItem;
    Export1: TMenuItem;
    ExportObjModel1: TMenuItem;
    SaveDialog1: TSaveDialog;
    N5: TMenuItem;
    N8: TMenuItem;
    Copy1: TMenuItem;
    OpenPictureDialog2: TOpenPictureDialog;
    ToolBar1: TToolBar;
    PropertiesPanel: TPanel;
    OpenGLScrollBox: TScrollBox;
    OpenGLPanel: TPanel;
    Splitter1: TSplitter;
    SaveAsButton1: TSpeedButton;
    SaveButton1: TSpeedButton;
    OpenButton1: TSpeedButton;
    NewButton1: TSpeedButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    UndoButton1: TSpeedButton;
    RedoButton1: TSpeedButton;
    ToolButton4: TToolButton;
    AboutButton1: TSpeedButton;
    N7: TMenuItem;
    HistoryItem0: TMenuItem;
    HistoryItem1: TMenuItem;
    HistoryItem2: TMenuItem;
    HistoryItem3: TMenuItem;
    HistoryItem4: TMenuItem;
    HistoryItem5: TMenuItem;
    HistoryItem6: TMenuItem;
    HistoryItem7: TMenuItem;
    HistoryItem8: TMenuItem;
    HistoryItem9: TMenuItem;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    ExportScreenshot1: TMenuItem;
    Wireframe1: TMenuItem;
    Twig1: TMenuItem;
    Renderenviroment1: TMenuItem;
    TrunkImage: TImage;
    TwigImage: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    SeedEdit: TEdit;
    SeedSpeedButton1: TSpeedButton;
    SeedSpeedButton2: TSpeedButton;
    BranchSegmentsPaintBox: TPaintBox;
    BranchSegmentsLabel: TLabel;
    BranchLevelsPaintBox: TPaintBox;
    BranchLevelsLabel: TLabel;
    TruncForksPaintBox: TPaintBox;
    TruncForksLabel: TLabel;
    TextureVMultiplierPaintBox: TPaintBox;
    TextureVMultiplierLabel: TLabel;
    TwigScalePaintBox: TPaintBox;
    TwigScaleLabel: TLabel;
    InitialLengthPaintBox: TPaintBox;
    InitialLengthLabel: TLabel;
    LenFalloffRatePaintBox: TPaintBox;
    LenFalloffRateLabel: TLabel;
    LenFalloffPowerPaintBox: TPaintBox;
    LenFalloffPowerLabel: TLabel;
    MaxClumpingPaintBox: TPaintBox;
    MaxClumpingLabel: TLabel;
    MinClumpingPaintBox: TPaintBox;
    MinClumpingLabel: TLabel;
    SymmetryPaintBox: TPaintBox;
    SymmetryLabel: TLabel;
    DropPaintBox: TPaintBox;
    DropLabel: TLabel;
    GrowthPaintBox: TPaintBox;
    GrowthLabel: TLabel;
    SweepPaintBox: TPaintBox;
    SweepLabel: TLabel;
    TruncRadiusPaintBox: TPaintBox;
    TruncRadiusLabel: TLabel;
    RadiusFalloffPaintBox: TPaintBox;
    RadiusFalloffLabel: TLabel;
    ClimbRatePaintBox: TPaintBox;
    ClimbRateLabel: TLabel;
    KinkPaintBox: TPaintBox;
    KinkLabel: TLabel;
    TaperRatePaintBox: TPaintBox;
    TaperRateLabel: TLabel;
    TwistsPaintBox: TPaintBox;
    TwistsLabel: TLabel;
    TruncLengthPaintBox: TPaintBox;
    TruncLengthLabel: TLabel;
    SaveDialog2: TSaveDialog;
    LoadTrunkBitBtn1: TBitBtn;
    LoadTwigBitBtn1: TBitBtn;
    Sprite1: TMenuItem;
    N1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure NewButton1Click(Sender: TObject);
    procedure SaveButton1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AboutButton1Click(Sender: TObject);
    procedure SaveAsButton1Click(Sender: TObject);
    procedure ExitButton1Click(Sender: TObject);
    procedure OpenButton1Click(Sender: TObject);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure OpenGLPanelResize(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure OpenGLPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenGLPanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenGLPanelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure OpenGLPanelDblClick(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure Undo1Click(Sender: TObject);
    procedure Redo1Click(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure File1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Options1Click(Sender: TObject);
    procedure Wireframe1Click(Sender: TObject);
    procedure Twig1Click(Sender: TObject);
    procedure TrunkImageDblClick(Sender: TObject);
    procedure TwigImageDblClick(Sender: TObject);
    procedure Renderenviroment1Click(Sender: TObject);
    procedure SeedSpeedButton1Click(Sender: TObject);
    procedure SeedSpeedButton2Click(Sender: TObject);
    procedure SeedEditKeyPress(Sender: TObject; var Key: Char);
    procedure SeedEditChange(Sender: TObject);
    procedure ExportObjModel1Click(Sender: TObject);
    procedure ExportScreenshot1Click(Sender: TObject);
    procedure Sprite1Click(Sender: TObject);
  private
    { Private declarations }
    ffilename: string;
    changed: Boolean;
    tree: tree_t;
    rc: HGLRC;   // Rendering Context
    dc: HDC;     // Device Context
    glpanx, glpany: integer;
    glmousedown: integer;
    undoManager: TUndoRedoManager;
    filemenuhistory: TFileMenuHistory;
    glneedsupdate: boolean;
    needsrecalc: boolean;
    BranchSegmentsSlider: TSliderHook;
    BranchLevelsSlider: TSliderHook;
    TruncForksSlider: TSliderHook;
    TextureVMultiplierSlider: TSliderHook;
    TwigScaleSlider: TSliderHook;
    InitialLengthSlider: TSliderHook;
    LenFalloffRateSlider: TSliderHook;
    LenFalloffPowerSlider: TSliderHook;
    MaxClumpingSlider: TSliderHook;
    MinClumpingSlider: TSliderHook;
    SymmetrySlider: TSliderHook;
    DropSlider: TSliderHook;
    GrowthSlider: TSliderHook;
    SweepSlider: TSliderHook;
    TruncRadiusSlider: TSliderHook;
    RadiusFalloffSlider: TSliderHook;
    ClimbRateSlider: TSliderHook;
    KinkSlider: TSliderHook;
    TaperRateSlider: TSliderHook;
    TwistsSlider: TSliderHook;
    TruncLengthSlider: TSliderHook;
    closing: boolean;
    procedure Idle(Sender: TObject; var Done: Boolean);
    function CheckCanClose: boolean;
    procedure DoNewTree(const seed: integer);
    procedure DoSaveTree(const fname: string);
    function DoLoadTree(const fname: string): boolean;
    procedure SetFileName(const fname: string);
    procedure DoSaveTreeBinaryUndo(s: TStream);
    procedure DoLoadTreeBinaryUndo(s: TStream);
    procedure SaveUndo;
    procedure UpdateStausbar;
    procedure UpdateEnable;
    procedure OnLoadTreeFileMenuHistory(Sender: TObject; const fname: string);
    procedure DoRenderGL;
    procedure Get3dPreviewBitmap(const b: TBitmap);
    procedure TreeToSliders;
    procedure SlidersToLabels;
    procedure TreeToControls;
    procedure ControlsToTree(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  dt_gl,
  dt_defs,
  dt_utils,
  proctree_helpers,
  frm_exportsprite;

{$R *.dfm}

resourcestring
  rsTitle = 'DOOMTREE Sprite Generator';

procedure TForm1.FormCreate(Sender: TObject);
var
  pfd: TPIXELFORMATDESCRIPTOR;
  pf: Integer;
  doCreate: boolean;
begin
  Randomize;
  
  pt_LoadSettingFromFile(ChangeFileExt(ParamStr(0), '.ini'));

  closing := False;

  PageControl1.ActivePageIndex := 0;
  
  undoManager := TUndoRedoManager.Create;
  undoManager.OnLoadFromStream := DoLoadTreeBinaryUndo;
  undoManager.OnSaveToStream := DoSaveTreeBinaryUndo;

  filemenuhistory := TFileMenuHistory.Create(self);
  filemenuhistory.MenuItem0 := HistoryItem0;
  filemenuhistory.MenuItem1 := HistoryItem1;
  filemenuhistory.MenuItem2 := HistoryItem2;
  filemenuhistory.MenuItem3 := HistoryItem3;
  filemenuhistory.MenuItem4 := HistoryItem4;
  filemenuhistory.MenuItem5 := HistoryItem5;
  filemenuhistory.MenuItem6 := HistoryItem6;
  filemenuhistory.MenuItem7 := HistoryItem7;
  filemenuhistory.MenuItem8 := HistoryItem8;
  filemenuhistory.MenuItem9 := HistoryItem9;
  filemenuhistory.OnOpen := OnLoadTreeFileMenuHistory;

  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory9));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory8));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory7));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory6));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory5));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory4));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory3));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory2));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory1));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory0));

  tree := tree_t.Create;

  Scaled := False;

  OpenGLPanel.Width := 3 * Screen.Width div 4;
  OpenGLPanel.Height := 3 * Screen.Height div 4;
  OpenGLPanel.DoubleBuffered := True;

  glpanx := 0;
  glpany := 0;
  glmousedown := 0;

  InitOpenGL;
  ReadExtensions;
  ReadImplementationProperties;

  // OpenGL initialisieren
  dc := GetDC(OpenGLPanel.Handle);

  // PixelFormat
  pfd.nSize := SizeOf(pfd);
  pfd.nVersion := 1;
  pfd.dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
  pfd.iPixelType := PFD_TYPE_RGBA;      // PFD_TYPE_RGBA or PFD_TYPEINDEX
  pfd.cColorBits := 32;

  pf := ChoosePixelFormat(dc, @pfd);   // Returns format that most closely matches above pixel format
  SetPixelFormat(dc, pf, @pfd);

  rc := wglCreateContext(dc);    // Rendering Context = window-glCreateContext
  wglMakeCurrent(dc, rc);        // Make the DC (Form1) the rendering Context

  // Initialize GL environment variables

  glInit;

  ResetCamera;

  OpenGLPanelResize(sender);    // sets up the perspective

  trunktexture := gld_CreateTexture(TrunkImage.Picture, False);
  twigtexture := gld_CreateTexture(TwigImage.Picture, True);

  glneedsupdate := True;

  needsrecalc := True;

  TabSheet1.DoubleBuffered := True;

  BranchSegmentsSlider := TSliderHook.Create(BranchSegmentsPaintBox);
  BranchSegmentsSlider.Min := 2;
  BranchSegmentsSlider.Max := 32;

  BranchLevelsSlider := TSliderHook.Create(BranchLevelsPaintBox);
  BranchLevelsSlider.Min := 1;
  BranchLevelsSlider.Max := 10;

  TruncForksSlider := TSliderHook.Create(TruncForksPaintBox);
  TruncForksSlider.Min := 0;
  TruncForksSlider.Max := 32;

  TextureVMultiplierSlider := TSliderHook.Create(TextureVMultiplierPaintBox);
  TextureVMultiplierSlider.Min := 0.010;
  TextureVMultiplierSlider.Max := 10.000;

  TwigScaleSlider := TSliderHook.Create(TwigScalePaintBox);
  TwigScaleSlider.Min := 0.010;
  TwigScaleSlider.Max := 2.000;

  InitialLengthSlider := TSliderHook.Create(InitialLengthPaintBox);
  InitialLengthSlider.Min := 0.010;
  InitialLengthSlider.Max := 5.000;

  LenFalloffRateSlider := TSliderHook.Create(LenFalloffRatePaintBox);
  LenFalloffRateSlider.Min := 0.010;
  LenFalloffRateSlider.Max := 1.500;

  LenFalloffPowerSlider := TSliderHook.Create(LenFalloffPowerPaintBox);
  LenFalloffPowerSlider.Min := -2.000;
  LenFalloffPowerSlider.Max := 2.000;

  MaxClumpingSlider := TSliderHook.Create(MaxClumpingPaintBox);
  MaxClumpingSlider.Min := 0.010;
  MaxClumpingSlider.Max := 10.000;

  MinClumpingSlider := TSliderHook.Create(MinClumpingPaintBox);
  MinClumpingSlider.Min := 0.010;
  MinClumpingSlider.Max := 10.000;

  SymmetrySlider := TSliderHook.Create(SymmetryPaintBox);
  SymmetrySlider.Min := 2.000;
  SymmetrySlider.Max := 4.000;

  DropSlider := TSliderHook.Create(DropPaintBox);
  DropSlider.Min := -2.000;
  DropSlider.Max := 2.000;

  GrowthSlider := TSliderHook.Create(GrowthPaintBox);
  GrowthSlider.Min := -4.000;
  GrowthSlider.Max := 4.000;

  SweepSlider := TSliderHook.Create(SweepPaintBox);
  SweepSlider.Min := -1.000;
  SweepSlider.Max := 1.000;

  TruncRadiusSlider := TSliderHook.Create(TruncRadiusPaintBox);
  TruncRadiusSlider.Min := 0.010;
  TruncRadiusSlider.Max := 0.500;

  RadiusFalloffSlider := TSliderHook.Create(RadiusFalloffPaintBox);
  RadiusFalloffSlider.Min := 0.100;
  RadiusFalloffSlider.Max := 1.000;

  ClimbRateSlider := TSliderHook.Create(ClimbRatePaintBox);
  ClimbRateSlider.Min := 0.010;
  ClimbRateSlider.Max := 1.000;

  KinkSlider := TSliderHook.Create(KinkPaintBox);
  KinkSlider.Min := -2.000;
  KinkSlider.Max := 2.000;

  TaperRateSlider := TSliderHook.Create(TaperRatePaintBox);
  TaperRateSlider.Min := 0.500;
  TaperRateSlider.Max := 2.000;

  TwistsSlider := TSliderHook.Create(TwistsPaintBox);
  TwistsSlider.Min := 0.010;
  TwistsSlider.Max := 10.000;

  TruncLengthSlider := TSliderHook.Create(TruncLengthPaintBox);
  TruncLengthSlider.Min := 0.010;
  TruncLengthSlider.Max := 5.000;

  doCreate := True;
  if ParamCount > 0 then
    if DoLoadTree(ParamStr(1)) then
      doCreate := False;

  if DoCreate then
  begin
    SetFileName('');
    changed := False;
    TreeToControls;
    glneedsupdate := True;
    needsrecalc := True;
    undoManager.Clear;
  end;

  // when the app has spare time, render the GL scene
  Application.OnIdle := Idle;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CheckCanClose;
end;

function TForm1.CheckCanClose: boolean;
var
  ret: integer;
begin
  if changed then
  begin
    ret := MessageBox(Handle, 'Do you want to save changes?', PChar(rsTitle), MB_YESNOCANCEL or MB_ICONQUESTION or MB_APPLMODAL);
    if ret = IDCANCEL	then
    begin
      Result := False;
      exit;
    end;
    if ret = IDNO	then
    begin
      Result := True;
      exit;
    end;
    if ret = IDYES then
    begin
      SaveButton1Click(self);
      Result := not changed;
      exit;
    end;
  end;
  Result := True;
end;

procedure TForm1.NewButton1Click(Sender: TObject);
begin
  if not CheckCanClose then
    Exit;

  DoNewTree(random($10000));
  ResetCamera;
end;

procedure TForm1.DoNewTree(const seed: integer);
begin
  SetFileName('');
  changed := False;
  tree.mProperties.DefaultValues(seed);
  TreeToControls;
  glneedsupdate := True;
  needsrecalc := True;
  undoManager.Clear;
end;

procedure TForm1.SetFileName(const fname: string);
begin
  ffilename := fname;
  Caption := rsTitle;
  if ffilename <> '' then
    Caption := Caption + ' - ' + MkShortName(ffilename);
end;

procedure TForm1.SaveButton1Click(Sender: TObject);
begin
  if ffilename = '' then
  begin
    if SaveDialog1.Execute then
    begin
      ffilename := SaveDialog1.FileName;
      filemenuhistory.AddPath(ffilename);
    end
    else
    begin
      Beep;
      Exit;
    end;
  end;
  BackupFile(ffilename);
  DoSaveTree(ffilename);
end;

procedure TForm1.DoSaveTree(const fname: string);
var
  fs: TFileStream;
begin
  SetFileName(fname);

  fs := TFileStream.Create(fname, fmCreate);
  try
    PT_SavePropertiesBinary(tree.mProperties, fs);
  finally
    fs.Free;
  end;

  changed := False;
end;

function TForm1.DoLoadTree(const fname: string): boolean;
var
  fs: TFileStream;
  s: string;
begin
  if not FileExists(fname) then
  begin
    s := Format('File %s does not exist!', [MkShortName(fname)]);
    MessageBox(Handle, PChar(s), PChar(rsTitle), MB_OK or MB_ICONEXCLAMATION or MB_APPLMODAL);
    Result := False;
    exit;
  end;

  undoManager.Clear;

  fs := TFileStream.Create(fname, fmOpenRead or fmShareDenyWrite);
  try
    PT_LoadPropertiesBinary(tree.mProperties, fs);
  finally
    fs.Free;
  end;

  TreeToControls;
  filemenuhistory.AddPath(fname);
  SetFileName(fname);
  glneedsupdate := True;
  needsrecalc := True;
  Result := True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  closing := True;
  Timer1.Enabled := False;
  undoManager.Free;
  wglMakeCurrent(0, 0);
  wglDeleteContext(rc);

  glDeleteTextures(1, @trunktexture);
  glDeleteTextures(1, @twigtexture);

  stringtobigstring(filemenuhistory.PathStringIdx(0), @opt_filemenuhistory0);
  stringtobigstring(filemenuhistory.PathStringIdx(1), @opt_filemenuhistory1);
  stringtobigstring(filemenuhistory.PathStringIdx(2), @opt_filemenuhistory2);
  stringtobigstring(filemenuhistory.PathStringIdx(3), @opt_filemenuhistory3);
  stringtobigstring(filemenuhistory.PathStringIdx(4), @opt_filemenuhistory4);
  stringtobigstring(filemenuhistory.PathStringIdx(5), @opt_filemenuhistory5);
  stringtobigstring(filemenuhistory.PathStringIdx(6), @opt_filemenuhistory6);
  stringtobigstring(filemenuhistory.PathStringIdx(7), @opt_filemenuhistory7);
  stringtobigstring(filemenuhistory.PathStringIdx(8), @opt_filemenuhistory8);
  stringtobigstring(filemenuhistory.PathStringIdx(9), @opt_filemenuhistory9);
  pt_SaveSettingsToFile(ChangeFileExt(ParamStr(0), '.ini'));

  filemenuhistory.Free;

  BranchSegmentsSlider.Free;
  BranchLevelsSlider.Free;
  TruncForksSlider.Free;
  TextureVMultiplierSlider.Free;
  TwigScaleSlider.Free;
  InitialLengthSlider.Free;
  LenFalloffRateSlider.Free;
  LenFalloffPowerSlider.Free;
  MaxClumpingSlider.Free;
  MinClumpingSlider.Free;
  SymmetrySlider.Free;
  DropSlider.Free;
  GrowthSlider.Free;
  SweepSlider.Free;
  TruncRadiusSlider.Free;
  RadiusFalloffSlider.Free;
  ClimbRateSlider.Free;
  KinkSlider.Free;
  TaperRateSlider.Free;
  TwistsSlider.Free;
  TruncLengthSlider.Free;

  tree.Free;
end;

resourcestring
  copyright1 = 'proctree.js Copyright (c) 2012, Paul Brunt';
  copyright2 = 'c++ port Copyright (c) 2015, Jari Komppa';
  copyright3 = 'Pascal port Copyright (c) 2018, Jim Valavanis';

procedure TForm1.AboutButton1Click(Sender: TObject);
begin
  MessageBox(
    Handle,
    PChar(Format('%s'#13#10 +
    'Version ' + I_VersionBuilt + #13#10 +
    'Copyright (c) 2021, jvalavanis@gmail.com'#13#10 +
    #13#10'A tool to create tree sprites for Doom.'#13#10#13#10 +
        copyright1 + #13#10 +
        copyright2 + #13#10 +
        copyright3,
        [rsTitle])),
    PChar(rsTitle),
    MB_OK or MB_ICONINFORMATION or MB_APPLMODAL);
end;

procedure TForm1.SaveAsButton1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    filemenuhistory.AddPath(SaveDialog1.FileName);
    BackupFile(SaveDialog1.FileName);
    DoSaveTree(SaveDialog1.FileName);
  end;
end;

procedure TForm1.ExitButton1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.OpenButton1Click(Sender: TObject);
begin
  if not CheckCanClose then
    Exit;

  if OpenDialog1.Execute then
  begin
    DoLoadTree(OpenDialog1.FileName);
    ResetCamera;
  end;
end;

procedure TForm1.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  pt: TPoint;
  r: TRect;
  z: glfloat;
begin
  pt := OpenGLPanel.Parent.ScreenToClient(MousePos);
  r := OpenGLPanel.ClientRect;
  if r.Right > OpenGLScrollBox.Width then
    r.Right := OpenGLScrollBox.Width;
  if r.Bottom > OpenGLScrollBox.Height then
    r.Bottom := OpenGLScrollBox.Height;
  if PtInRect(r, pt) then
  begin
    z := camera.z - 0.5;
    z := z / 0.99;
    camera.z := z + 0.5;
    if camera.z < -20.0 then
      camera.z := -20.0;
    glneedsupdate := True;
  end;
end;

procedure TForm1.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  pt: TPoint;
  r: TRect;
  z: glfloat;
begin
  pt := OpenGLPanel.Parent.ScreenToClient(MousePos);
  r := OpenGLPanel.ClientRect;
  if r.Right > OpenGLScrollBox.Width then
    r.Right := OpenGLScrollBox.Width;
  if r.Bottom > OpenGLScrollBox.Height then
    r.Bottom := OpenGLScrollBox.Height;
  if PtInRect(r, pt) then
  begin
    z := camera.z - 0.5;
    z := z * 0.99;
    camera.z := z + 0.5;
    if camera.z > 0.5 then
      camera.z := 0.5;
    glneedsupdate := True;
  end;
end;

procedure TForm1.OpenGLPanelResize(Sender: TObject);
begin
  glViewport(0, 0, OpenGLPanel.Width, OpenGLPanel.Height);    // Set the viewport for the OpenGL window
  glMatrixMode(GL_PROJECTION);        // Change Matrix Mode to Projection
  glLoadIdentity;                     // Reset View
  gluPerspective(45.0, OpenGLPanel.Width / OpenGLPanel.Height, 1.0, 500.0);  // Do the perspective calculations. Last value = max clipping depth

  glMatrixMode(GL_MODELVIEW);         // Return to the modelview matrix
  glneedsupdate := True;
end;

procedure TForm1.Idle(Sender: TObject; var Done: Boolean);
begin
  if closing then
    Exit;

  Sleep(1);
  UpdateEnable;

  Done := False;

  if needsrecalc then
    glneedsupdate := True;

  if not glneedsupdate then
    // jval: We don't need to render
    Exit;

  UpdateStausbar;

  DoRenderGL;

  glneedsupdate := False;
end;

procedure TForm1.ApplicationEvents1Idle(Sender: TObject;
  var Done: Boolean);
begin
  Idle(Sender, Done);
end;

procedure TForm1.OpenGLPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button in [mbLeft, mbRight] then
  begin
    glpanx := X;
    glpany := Y;
    if Button = mbLeft then
      glmousedown := 1
    else
      glmousedown := 2;
    SetCapture(OpenGLPanel.Handle);
  end;
end;

procedure TForm1.OpenGLPanelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  glmousedown := 0;
  ReleaseCapture;
end;

procedure TForm1.OpenGLPanelMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if glmousedown = 0 then
    exit;

  if glmousedown = 1 then
  begin
    camera.ay := camera.ay + (glpanx - X) ;/// OpenGLPanel.Width {* 2 * pi};
    camera.ax := camera.ax + (glpany - Y) ; // / OpenGLPanel.Height {* 2 * pi};
  end
  else
  begin
    camera.x := camera.x + (glpanx - X) / OpenGLPanel.Width * (camera.z - 1.0);/// OpenGLPanel.Width {* 2 * pi};
    if camera.x < -6.0 then
      camera.x := -6.0
    else if camera.x > 6.0 then
      camera.x := 6.0;

    camera.y := camera.y - (glpany - Y) / OpenGLPanel.Width * (camera.z - 1.0); // / OpenGLPanel.Height {* 2 * pi};
    if camera.y < -6.0 then
      camera.y := -6.0
    else if camera.y > 6.0 then
      camera.y := 6.0;
  end;

  glneedsupdate := True;

  glpanx := X;
  glpany := Y;
end;

procedure TForm1.OpenGLPanelDblClick(Sender: TObject);
begin
  ResetCamera;
  glneedsupdate := True;
end;

procedure TForm1.Edit1Click(Sender: TObject);
begin
  Undo1.Enabled := undoManager.CanUndo;
  Redo1.Enabled := undoManager.CanRedo;
end;

procedure TForm1.Undo1Click(Sender: TObject);
begin
  if undoManager.CanUndo then
  begin
    undoManager.Undo;
    glneedsupdate := True;
    needsrecalc := True;
  end;
end;

procedure TForm1.Redo1Click(Sender: TObject);
begin
  if undoManager.CanRedo then
  begin
    undoManager.Redo;
    glneedsupdate := True;
    needsrecalc := True;
  end;
end;

procedure TForm1.DoSaveTreeBinaryUndo(s: TStream);
begin
  PT_SavePropertiesBinary(tree.mProperties, s);
end;

procedure TForm1.DoLoadTreeBinaryUndo(s: TStream);
begin
  PT_LoadPropertiesBinary(tree.mProperties, s);
  TreeToControls;
  glneedsupdate := True;
  needsrecalc := True;
end;

procedure TForm1.SaveUndo;
begin
  undoManager.SaveUndo;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  glneedsupdate := True;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  glneedsupdate := True;
end;

procedure TForm1.UpdateStausbar;
begin
  StatusBar1.Panels[0].Text := Format('Camera(x=%2.2f, y=%2.2f, z=%2.2f)', [camera.x, camera.y, camera.z]);
  StatusBar1.Panels[1].Text := Format('Rendered triangles = %d', [pt_rendredtriangles]);
end;

procedure TForm1.UpdateEnable;
begin
  Undo1.Enabled := undoManager.CanUndo;
  Redo1.Enabled := undoManager.CanRedo;
  UndoButton1.Enabled := undoManager.CanUndo;
  RedoButton1.Enabled := undoManager.CanRedo;
end;

procedure TForm1.OnLoadTreeFileMenuHistory(Sender: TObject; const fname: string);
begin
  if not CheckCanClose then
    Exit;

  DoLoadTree(fname);
  ResetCamera;
end;

procedure TForm1.File1Click(Sender: TObject);
begin
  filemenuhistory.RefreshMenuItems;
end;

procedure TForm1.DoRenderGL;
begin
  if glneedsupdate then
  begin
    glBeginScene(OpenGLPanel.Width, OpenGLPanel.Height);
    try
      if needsrecalc then
      begin
        tree.generate;
        needsrecalc := False;
      end;
      glRenderEnviroment;
      glRenderTree(tree);
    finally
      glEndScene(dc);
    end;
  end;
end;

procedure TForm1.Get3dPreviewBitmap(const b: TBitmap);
type
  long_a = array[0..$FFFF] of LongWord;
  Plong_a = ^long_a;
var
  L, buf: Plong_a;
  w, h: integer;
  i, j: integer;
  idx: integer;
begin
  w := OpenGLPanel.Width;
  h := OpenGLPanel.Height;
  b.Width := w;
  b.Height := h;
  b.PixelFormat := pf32bit;

  GetMem(L, w * h * SizeOf(LongWord));
  glReadPixels(0, 0, w, h, GL_BGRA, GL_UNSIGNED_BYTE, L);

  idx := 0;
  for j := 0 to h - 1 do
  begin
    buf := b.ScanLine[h - j - 1];
    for i := 0 to w - 1 do
    begin
      buf[i] := L[idx];
      Inc(idx);
    end;
  end;

  FreeMem(L, w * h * SizeOf(LongWord));
end;

procedure TForm1.Copy1Click(Sender: TObject);
var
  b: TBitmap;
begin
  b := TBitmap.Create;
  try
    DoRenderGL; // JVAL: For some unknown reason this must be called before glReadPixels
    Get3dPreviewBitmap(b);
    Clipboard.Assign(b);
  finally
    b.Free;
  end;
end;

procedure TForm1.Options1Click(Sender: TObject);
begin
  Renderenviroment1.Checked := opt_renderevniroment;
  Wireframe1.Checked := opt_renderwireframe;
  Twig1.Checked := opt_rendertwig;
end;

procedure TForm1.Wireframe1Click(Sender: TObject);
begin
  opt_renderwireframe := not opt_renderwireframe;
  glneedsupdate := True;
end;

procedure TForm1.Twig1Click(Sender: TObject);
begin
  opt_rendertwig := not opt_rendertwig;
  glneedsupdate := True;
end;

procedure TForm1.TrunkImageDblClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    TrunkImage.Picture.LoadFromFile(OpenPictureDialog1.FileName);
    glDeleteTextures(1, @trunktexture);
    trunktexture := gld_CreateTexture(TrunkImage.Picture, False);
  end;
end;

procedure TForm1.TwigImageDblClick(Sender: TObject);
begin
  if OpenPictureDialog2.Execute then
  begin
    TwigImage.Picture.LoadFromFile(OpenPictureDialog2.FileName);
    glDeleteTextures(1, @twigtexture);
    twigtexture := gld_CreateTexture(TwigImage.Picture, True);
  end;
end;

procedure TForm1.Renderenviroment1Click(Sender: TObject);
begin
  opt_renderevniroment := not opt_renderevniroment;
  glneedsupdate := True;
end;

procedure TForm1.SeedSpeedButton1Click(Sender: TObject);
var
  x: integer;
begin
  x := StrToIntDef(SeedEdit.Text, -1);
  if x >= 0 then
    if x < MAXINT then
      SeedEdit.Text := IntToStr(x + 1);
end;

procedure TForm1.SeedSpeedButton2Click(Sender: TObject);
var
  x: integer;
begin
  x := StrToIntDef(SeedEdit.Text, -1);
  if x > 0 then
    SeedEdit.Text := IntToStr(x - 1);
end;

procedure TForm1.SeedEditKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in [#8, '0'..'9']) then
    Key := #0;
end;

procedure TForm1.TreeToSliders;
begin
  BranchSegmentsSlider.OnSliderHookChange := nil;
  BranchLevelsSlider.OnSliderHookChange := nil;
  TruncForksSlider.OnSliderHookChange := nil;
  TextureVMultiplierSlider.OnSliderHookChange := nil;
  TwigScaleSlider.OnSliderHookChange := nil;
  InitialLengthSlider.OnSliderHookChange := nil;
  LenFalloffRateSlider.OnSliderHookChange := nil;
  LenFalloffPowerSlider.OnSliderHookChange := nil;
  MaxClumpingSlider.OnSliderHookChange := nil;
  MinClumpingSlider.OnSliderHookChange := nil;
  SymmetrySlider.OnSliderHookChange := nil;
  DropSlider.OnSliderHookChange := nil;
  GrowthSlider.OnSliderHookChange := nil;
  SweepSlider.OnSliderHookChange := nil;
  TruncRadiusSlider.OnSliderHookChange := nil;
  RadiusFalloffSlider.OnSliderHookChange := nil;
  ClimbRateSlider.OnSliderHookChange := nil;
  KinkSlider.OnSliderHookChange := nil;
  TaperRateSlider.OnSliderHookChange := nil;
  TwistsSlider.OnSliderHookChange := nil;
  TruncLengthSlider.OnSliderHookChange := nil;

  BranchSegmentsSlider.Position := tree.mProperties.mSegments;
  BranchLevelsSlider.Position := tree.mProperties.mLevels;
  TruncForksSlider.Position := tree.mProperties.mTreeSteps;
  TextureVMultiplierSlider.Position := tree.mProperties.mVMultiplier;
  TwigScaleSlider.Position := tree.mProperties.mTwigScale;
  InitialLengthSlider.Position := tree.mProperties.mInitialBranchLength;
  LenFalloffRateSlider.Position := tree.mProperties.mLengthFalloffFactor;
  LenFalloffPowerSlider.Position := tree.mProperties.mLengthFalloffPower;
  MaxClumpingSlider.Position := tree.mProperties.mClumpMax;
  MinClumpingSlider.Position := tree.mProperties.mClumpMin;
  SymmetrySlider.Position := tree.mProperties.mBranchFactor;
  DropSlider.Position := tree.mProperties.mDropAmount;
  GrowthSlider.Position := tree.mProperties.mGrowAmount;
  SweepSlider.Position := tree.mProperties.mSweepAmount;
  TruncRadiusSlider.Position := tree.mProperties.mMaxRadius;
  RadiusFalloffSlider.Position := tree.mProperties.mRadiusFalloffRate;
  ClimbRateSlider.Position := tree.mProperties.mClimbRate;
  KinkSlider.Position := tree.mProperties.mTrunkKink;
  TaperRateSlider.Position := tree.mProperties.mTaperRate;
  TwistsSlider.Position := tree.mProperties.mRadiusFalloffRate;
  TruncLengthSlider.Position := tree.mProperties.mTrunkLength;

  BranchSegmentsPaintBox.Invalidate;
  BranchLevelsPaintBox.Invalidate;
  TruncForksPaintBox.Invalidate;
  TextureVMultiplierPaintBox.Invalidate;
  TwigScalePaintBox.Invalidate;
  InitialLengthPaintBox.Invalidate;
  LenFalloffRatePaintBox.Invalidate;
  LenFalloffPowerPaintBox.Invalidate;
  MaxClumpingPaintBox.Invalidate;
  MinClumpingPaintBox.Invalidate;
  SymmetryPaintBox.Invalidate;
  DropPaintBox.Invalidate;
  GrowthPaintBox.Invalidate;
  SweepPaintBox.Invalidate;
  TruncRadiusPaintBox.Invalidate;
  RadiusFalloffPaintBox.Invalidate;
  ClimbRatePaintBox.Invalidate;
  KinkPaintBox.Invalidate;
  TaperRatePaintBox.Invalidate;
  TwistsPaintBox.Invalidate;
  TruncLengthPaintBox.Invalidate;

  BranchSegmentsSlider.OnSliderHookChange := ControlsToTree;
  BranchLevelsSlider.OnSliderHookChange := ControlsToTree;
  TruncForksSlider.OnSliderHookChange := ControlsToTree;
  TextureVMultiplierSlider.OnSliderHookChange := ControlsToTree;
  TwigScaleSlider.OnSliderHookChange := ControlsToTree;
  InitialLengthSlider.OnSliderHookChange := ControlsToTree;
  LenFalloffRateSlider.OnSliderHookChange := ControlsToTree;
  LenFalloffPowerSlider.OnSliderHookChange := ControlsToTree;
  MaxClumpingSlider.OnSliderHookChange := ControlsToTree;
  MinClumpingSlider.OnSliderHookChange := ControlsToTree;
  SymmetrySlider.OnSliderHookChange := ControlsToTree;
  DropSlider.OnSliderHookChange := ControlsToTree;
  GrowthSlider.OnSliderHookChange := ControlsToTree;
  SweepSlider.OnSliderHookChange := ControlsToTree;
  TruncRadiusSlider.OnSliderHookChange := ControlsToTree;
  RadiusFalloffSlider.OnSliderHookChange := ControlsToTree;
  ClimbRateSlider.OnSliderHookChange := ControlsToTree;
  KinkSlider.OnSliderHookChange := ControlsToTree;
  TaperRateSlider.OnSliderHookChange := ControlsToTree;
  TwistsSlider.OnSliderHookChange := ControlsToTree;
  TruncLengthSlider.OnSliderHookChange := ControlsToTree;
end;

procedure TForm1.SlidersToLabels;
begin
  BranchSegmentsLabel.Caption := Format('%d', [Round(BranchSegmentsSlider.Position / 2) * 2]);
  BranchLevelsLabel.Caption := Format('%d', [Round(BranchLevelsSlider.Position)]);
  TruncForksLabel.Caption := Format('%d', [Round(TruncForksSlider.Position)]);
  TextureVMultiplierLabel.Caption := Format('%1.3f', [TextureVMultiplierSlider.Position]);
  TwigScaleLabel.Caption := Format('%1.3f', [TwigScaleSlider.Position]);
  InitialLengthLabel.Caption := Format('%1.3f', [InitialLengthSlider.Position]);
  LenFalloffRateLabel.Caption := Format('%1.3f', [LenFalloffRateSlider.Position]);
  LenFalloffPowerLabel.Caption := Format('%1.3f', [LenFalloffPowerSlider.Position]);
  MaxClumpingLabel.Caption := Format('%1.3f', [MaxClumpingSlider.Position]);
  MinClumpingLabel.Caption := Format('%1.3f', [MinClumpingSlider.Position]);
  SymmetryLabel.Caption := Format('%1.3f', [SymmetrySlider.Position]);
  DropLabel.Caption := Format('%1.3f', [DropSlider.Position]);
  GrowthLabel.Caption := Format('%1.3f', [GrowthSlider.Position]);
  SweepLabel.Caption := Format('%1.3f', [SweepSlider.Position]);
  TruncRadiusLabel.Caption := Format('%1.3f', [TruncRadiusSlider.Position]);
  RadiusFalloffLabel.Caption := Format('%1.3f', [RadiusFalloffSlider.Position]);
  ClimbRateLabel.Caption := Format('%1.3f', [ClimbRateSlider.Position]);
  KinkLabel.Caption := Format('%1.3f', [KinkSlider.Position]);
  TaperRateLabel.Caption := Format('%1.3f', [TaperRateSlider.Position]);
  TwistsLabel.Caption := Format('%1.3f', [TwistsSlider.Position]);
  TruncLengthLabel.Caption := Format('%1.3f', [TruncLengthSlider.Position]);
end;

procedure TForm1.TreeToControls;
begin
  if closing then
    Exit;

  SeedEdit.Text := IntToStr(tree.mProperties.mSeed);
  TreeToSliders;
  SlidersToLabels;
end;

procedure TForm1.ControlsToTree(Sender: TObject);
begin
  if closing then
    Exit;

  SaveUndo;
  SlidersToLabels;
  tree.mProperties.mSeed := StrToIntDef(SeedEdit.Text, 262);
  tree.mProperties.mSegments := Round(BranchSegmentsSlider.Position / 2) * 2;
  tree.mProperties.mLevels := Round(BranchLevelsSlider.Position);
  tree.mProperties.mTreeSteps := Round(TruncForksSlider.Position);
  tree.mProperties.mVMultiplier := TextureVMultiplierSlider.Position;
  tree.mProperties.mTwigScale := TwigScaleSlider.Position;
  tree.mProperties.mInitialBranchLength := InitialLengthSlider.Position;
  tree.mProperties.mLengthFalloffFactor := LenFalloffRateSlider.Position;
  tree.mProperties.mLengthFalloffPower := LenFalloffPowerSlider.Position;
  tree.mProperties.mClumpMax := MaxClumpingSlider.Position;
  tree.mProperties.mClumpMin := MinClumpingSlider.Position;
  tree.mProperties.mBranchFactor := SymmetrySlider.Position;
  tree.mProperties.mDropAmount := DropSlider.Position;
  tree.mProperties.mGrowAmount := GrowthSlider.Position;
  tree.mProperties.mSweepAmount := SweepSlider.Position;
  tree.mProperties.mMaxRadius := TruncRadiusSlider.Position;
  tree.mProperties.mRadiusFalloffRate := RadiusFalloffSlider.Position;
  tree.mProperties.mClimbRate := ClimbRateSlider.Position;
  tree.mProperties.mTrunkKink := KinkSlider.Position;
  tree.mProperties.mTaperRate := TaperRateSlider.Position;
  tree.mProperties.mRadiusFalloffRate := TwistsSlider.Position;
  tree.mProperties.mTrunkLength := TruncLengthSlider.Position;
  needsrecalc := True;
  changed := True;
end;

procedure TForm1.SeedEditChange(Sender: TObject);
begin
  SaveUndo;
  tree.mProperties.mSeed := StrToIntDef(SeedEdit.Text, 262);
  needsrecalc := True;
  changed := True;
end;

procedure TForm1.ExportObjModel1Click(Sender: TObject);
var
  fs: TFileStream;
begin
  if SaveDialog2.Execute then
  begin
    BackupFile(SaveDialog2.FileName);
    fs := TFileStream.Create(SaveDialog2.FileName, fmCreate);
    try

      PT_SaveTreeToObj(tree, fs);
    finally
      fs.Free;
    end;
  end;
end;

procedure TForm1.ExportScreenshot1Click(Sender: TObject);
var
  b: TBitmap;
begin
  if SavePictureDialog1.Execute then
  begin
    BackupFile(SavePictureDialog1.FileName);
    b := TBitmap.Create;
    try
      DoRenderGL;
      Get3dPreviewBitmap(b);
      Clipboard.Assign(b);
      b.SaveToFile(SavePictureDialog1.FileName);
    finally
      b.Free;
    end;
  end;
end;

procedure TForm1.Sprite1Click(Sender: TObject);
var
  f: TExportSpriteForm;
begin
  f := TExportSpriteForm.Create(nil);
  try
    f.tree := tree;
    f.twigtex.Canvas.StretchDraw(Rect(0, 0, f.twigtex.Width, f.twigtex.Height), TwigImage.Picture.Bitmap);
    f.trunktex.Canvas.StretchDraw(Rect(0, 0, f.trunktex.Width, f.trunktex.Height), TrunkImage.Picture.Bitmap);
    f.ShowModal;
  finally
    f.Free;
  end;
end;

end.

