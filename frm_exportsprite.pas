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
//  Export Sprite Form
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/doom-rock/
//------------------------------------------------------------------------------

unit frm_exportsprite;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, dr_soft3d, procrock, ComCtrls;

type
  TExportSpriteForm = class(TForm)
    Label1: TLabel;
    FileNameEdit: TEdit;
    SelectFileButton: TSpeedButton;
    GeneralGroupBox: TGroupBox;
    Label2: TLabel;
    SpritePrefixButton: TSpeedButton;
    PrefixEdit: TEdit;
    PatchRadioGroup: TRadioGroup;
    PreviewGroupBox: TGroupBox;
    Panel3: TPanel;
    PaintBox1: TPaintBox;
    Panel4: TPanel;
    ZoomInSpeedButton: TSpeedButton;
    ZoomOutSpeedButton: TSpeedButton;
    HourglassLabel: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    SaveDialog1: TSaveDialog;
    ZoomTrackBar: TTrackBar;
    RotateTrackBar: TTrackBar;
    Theta2IncButton1: TSpeedButton;
    Theta2DecButton1: TSpeedButton;
    ScriptParametersGroupBox: TGroupBox;
    ScriptRadioGroup: TRadioGroup;
    Label3: TLabel;
    ActorNameEdit: TEdit;
    Label4: TLabel;
    EditorNumberEdit: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    RadiusEdit: TEdit;
    HeightLabel: TLabel;
    HeightEdit: TEdit;
    Timer1: TTimer;
    VoxelGroupBox: TGroupBox;
    GenerateVoxelCheckBox: TCheckBox;
    voxRadioButton64x64: TRadioButton;
    voxRadioButton128x128: TRadioButton;
    voxRadioButton256x256: TRadioButton;
    procedure SpritePrefixButtonClick(Sender: TObject);
    procedure SelectFileButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FileNameEditChange(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure ZoomInSpeedButtonClick(Sender: TObject);
    procedure ZoomOutSpeedButtonClick(Sender: TObject);
    procedure ZoomTrackBarChange(Sender: TObject);
    procedure RotateTrackBarChange(Sender: TObject);
    procedure Theta2IncButton1Click(Sender: TObject);
    procedure Theta2DecButton1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure CheckNumericEdit(Sender: TObject; var Key: Char);
    procedure ScriptRadioGroupClick(Sender: TObject);
    procedure GenerateVoxelCheckBoxClick(Sender: TObject);
    procedure voxRadioButton64x64Click(Sender: TObject);
    procedure voxRadioButton128x128Click(Sender: TObject);
    procedure voxRadioButton256x256Click(Sender: TObject);
  private
    { Private declarations }
    needs3dupdate: boolean;
    buffer: TBitmap;
    device: device_t;
    fdevicewidth, fdeviceheight: integer;
    fviewdist, ftheta, ftheta2: float;
    procedure RenderFaces(const mVertCount, mFaceCount: integer;
      const mVert: array of fvec5_t; const mFace: array of ivec3_t; const tex: TBitmap);
    procedure DoUpdate3d;
    procedure UpdateControls;
    function CheckOKtoGO: boolean;
  public
    { Public declarations }
    rock: rock_t;
    rocktex: TBitmap;
    procedure PrepareTextures;
    procedure DoExportSpriteWAD;
  end;

implementation

{$R *.dfm}

uses
  Math,
  procrock_helpers,
  dr_defs,
  dr_utils,
  dr_wadwriter,
  dr_doompatch,
  dr_palettes,
  dr_voxels,
  dr_voxelexport,
  frm_spriteprefix;

const
  MINVIEWDIST = 1.0;
  MAXVIEWDIST = 16.0;

procedure TExportSpriteForm.SpritePrefixButtonClick(Sender: TObject);
var
  s: string;
begin
  s := PrefixEdit.Text;
  if GetSpritePrefix(s) then
    PrefixEdit.Text := s;
end;

procedure TExportSpriteForm.SelectFileButtonClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    FileNameEdit.Text := SaveDialog1.FileName;
    Button1.Enabled := True;
  end;
end;

procedure TExportSpriteForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  DoubleBuffered := True;
  for i := 0 to ComponentCount - 1 do
    if Components[i].InheritsFrom(TWinControl) then
      (Components[i] as TWinControl).DoubleBuffered := True;

  fdevicewidth := 256;
  fdeviceheight := 510;
  device_init(@device, fdevicewidth, fdeviceheight);
  buffer := TBitmap.Create;
  buffer.Width := fdevicewidth;
  buffer.Height := fdeviceheight;
  buffer.PixelFormat := pf32bit;
  needs3dupdate := True;
  rocktex := TBitmap.Create;
  rocktex.Width := 256;
  rocktex.Height := 256;
  rocktex.PixelFormat := pf32bit;

  fviewdist := opt_viewdist / OPT_TO_FLOAT;
  if fviewdist < MINVIEWDIST then
    fviewdist := MINVIEWDIST
  else if fviewdist > MAXVIEWDIST then
    fviewdist := MAXVIEWDIST;

  ftheta := opt_theta1 / OPT_TO_FLOAT;
  if ftheta < 0.0 then
    ftheta := 0.0
  else if ftheta > 2 * PI then
    ftheta := 2 * PI;

  ftheta2 := opt_theta2 / OPT_TO_FLOAT;
  if ftheta2 < 0.0 then
    ftheta2 := 0.0
  else if ftheta2 > PI / 4 then
    ftheta2 := PI / 4;

  if opt_spritepal in [0..4] then
    PatchRadioGroup.ItemIndex := opt_spritepal;

  if opt_spritescript in [0..2] then
    ScriptRadioGroup.ItemIndex := opt_spritescript;

  GenerateVoxelCheckBox.Checked := opt_dospritevox = 1;

  voxRadioButton64x64.Checked := opt_spritevox = 64;
  voxRadioButton128x128.Checked := opt_spritevox = 128;
  voxRadioButton256x256.Checked := opt_spritevox = 256;

  UpdateControls;
end;

procedure TExportSpriteForm.FormDestroy(Sender: TObject);
begin
  device_destroy(@device);
  rocktex.Free;
  buffer.Free;

  opt_viewdist := Round(fviewdist * OPT_TO_FLOAT);
  opt_theta1 := Round(ftheta * OPT_TO_FLOAT);
  opt_theta2 := Round(ftheta2 * OPT_TO_FLOAT);
  opt_spritepal := PatchRadioGroup.ItemIndex;
  opt_spritescript := ScriptRadioGroup.ItemIndex;
  if GenerateVoxelCheckBox.Checked then
    opt_dospritevox := 1
  else
    opt_dospritevox := 0;
  if voxRadioButton64x64.Checked then
    opt_spritevox := 64
  else if voxRadioButton128x128.Checked then
    opt_spritevox := 128
  else if voxRadioButton256x256.Checked then
    opt_spritevox := 256;
end;

procedure TExportSpriteForm.FileNameEditChange(Sender: TObject);
begin
  if Trim(FileNameEdit.Text) = '' then
    Button1.Enabled := False;
end;

procedure TExportSpriteForm.RenderFaces(const mVertCount, mFaceCount: integer;
  const mVert: array of fvec5_t; const mFace: array of ivec3_t; const tex: TBitmap);
var
  v1, v2, v3: vertex_t;
  i: integer;
  procedure _make_vertex(const r: integer; const p: Pvertex_t);
  begin
    p.pos.x := mVert[r].x;
    p.pos.y := mVert[r].z;
    p.pos.z := mVert[r].y;
    p.pos.w := 1.0;
    p.tc.u := mVert[r].u;
    p.tc.v := mVert[r].v;
    p.color.r := 1.0;
    p.color.g := 1.0;
    p.color.b := 1.0;
    p.rhw := 1.0;
  end;
begin
  device_set_texture(@device, tex);
  for i := 0 to mFaceCount - 1 do
  begin
    _make_vertex(mFace[i].x, @v1);
    _make_vertex(mFace[i].y, @v2);
    _make_vertex(mFace[i].z, @v3);
    device_draw_primitive(@device, @v1, @v2, @v3);
  end;
end;

procedure TExportSpriteForm.DoUpdate3d;
var
  c, m, m2: matrix_t;
begin
  device_clear(@device);
  camera_at_zero(@device, fviewdist, 0, 0);
  matrix_set_rotate(@m, 0.0, 0.0, 1.0, ftheta);
  matrix_set_rotate(@m2, 0.0, 1.0, 0.0, ftheta2);
  matrix_mul(@c, @m, @m2);
	device.transform.world := c;
	transform_update(@device.transform);
  device.render_state := RENDER_STATE_TEXTURE_SOLID;
  RenderFaces(rock.mVertCount, rock.mFaceCount, rock.mVert, rock.mFace, rocktex);

  buffer.Canvas.StretchDraw(Rect(0, 0, buffer.Width, buffer.Height), device.bframebuffer);
  needs3dupdate := False;
end;

procedure TExportSpriteForm.PaintBox1Paint(Sender: TObject);
begin
  if needs3dupdate then
    DoUpdate3d;
  PaintBox1.Canvas.Draw(0, 0, buffer);
end;

procedure TExportSpriteForm.ZoomInSpeedButtonClick(Sender: TObject);
begin
  if fviewdist > MINVIEWDIST then
  begin
    fviewdist := fviewdist - 0.5;
    if fviewdist < MINVIEWDIST then
      fviewdist := MINVIEWDIST;
    needs3dupdate := True;
    UpdateControls;
    PaintBox1.Invalidate;
  end;
end;

procedure TExportSpriteForm.ZoomOutSpeedButtonClick(Sender: TObject);
begin
  if fviewdist < MAXVIEWDIST then
  begin
    fviewdist := fviewdist + 0.5;
    if fviewdist > MAXVIEWDIST then
      fviewdist := MAXVIEWDIST;
    needs3dupdate := True;
    UpdateControls;
    PaintBox1.Invalidate;
  end;
end;

procedure TExportSpriteForm.UpdateControls;
begin
  ZoomTrackBar.Position := GetIntegerInRange(Round(fviewdist * 10), ZoomTrackBar.Min, ZoomTrackBar.Max);
  RotateTrackBar.Position := GetIntegerInRange(Round(ftheta / (2 * pi) * RotateTrackBar.Max), RotateTrackBar.Min, RotateTrackBar.Max);
  voxRadioButton64x64.Visible := GenerateVoxelCheckBox.Checked;
  voxRadioButton128x128.Visible := GenerateVoxelCheckBox.Checked;
  voxRadioButton256x256.Visible := GenerateVoxelCheckBox.Checked;
end;

procedure TExportSpriteForm.ZoomTrackBarChange(Sender: TObject);
begin
  fviewdist := ZoomTrackBar.Position / 10;
  needs3dupdate := True;
  PaintBox1.Invalidate;
end;

procedure TExportSpriteForm.RotateTrackBarChange(Sender: TObject);
begin
  ftheta := RotateTrackBar.Position / RotateTrackBar.Max * 2 * PI;
  needs3dupdate := True;
  PaintBox1.Invalidate;
end;

procedure TExportSpriteForm.Theta2IncButton1Click(Sender: TObject);
begin
  if ftheta2 < PI / 4 then
  begin
    ftheta2 := ftheta2 + PI / 32;
    if ftheta2 > PI / 4 then
      ftheta2 := PI / 4;
    needs3dupdate := True;
    UpdateControls;
    PaintBox1.Invalidate;
  end;
end;

procedure TExportSpriteForm.Theta2DecButton1Click(Sender: TObject);
begin
  if ftheta2 > 0.0 then
  begin
    ftheta2 := ftheta2 - PI / 32;
    if ftheta2 < 0.0 then
      ftheta2 := 0.0;
    needs3dupdate := True;
    UpdateControls;
    PaintBox1.Invalidate;
  end;
end;

function TExportSpriteForm.CheckOKtoGO: boolean;
var
  s: string;
begin
  if Trim(FileNameEdit.Text) = '' then
  begin
    Result := False;
    Exit;
  end;

  if Length(Trim(PrefixEdit.Text)) <> 5 then
  begin
    Result := False;
    Exit;
  end;

  s := Trim(PrefixEdit.Text);
  if Pos(s[5], 'ABCDEFGHIJKLMNOPQRSTUVWXYZ\[]') = 0 then
  begin
    Result := False;
    Exit;
  end;

  if Pos(' ', s) > 0 then
  begin
    Result := False;
    Exit;
  end;


  if Length(Trim(ActorNameEdit.Text)) = 0 then
  begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

procedure TExportSpriteForm.Timer1Timer(Sender: TObject);
begin
  Button1.Enabled := CheckOKtoGO;
end;

procedure TExportSpriteForm.CheckNumericEdit(Sender: TObject;
  var Key: Char);
begin
  if not (Key in [#8, '0'..'9']) then
  begin
    Key := #0;
    Exit;
  end;
end;

procedure TExportSpriteForm.ScriptRadioGroupClick(Sender: TObject);
begin
  ScriptParametersGroupBox.Visible := ScriptRadioGroup.ItemIndex <> 2;
  VoxelGroupBox.Visible := ScriptRadioGroup.ItemIndex = 0;
end;

procedure TExportSpriteForm.PrepareTextures;
var
  y: integer;
  x: integer;
  ln: PIUINT32Array;
  r, g, b: byte;
begin
  rocktex.PixelFormat := pf32bit;
  for y := 0 to rocktex.Height - 1 do
  begin
    ln := rocktex.ScanLine[y];
    for x := 0 to rocktex.Width - 1 do
    begin
      r := GetRValue(ln[x]);
      g := GetGValue(ln[x]);
      b := GetBValue(ln[x]);
      if r < 16 then
        r := 16;
      if g < 16 then
        g := 16;
      if b < 16 then
        b := 16;
      ln[x] := RGB(r, g, b);
    end;
  end;
end;

procedure TExportSpriteForm.DoExportSpriteWAD;
var
  wad: TWADWriter;
  script: TStringList;
  name: string;
  stmp: string;
  i: integer;
  ms: TMemoryStream;
  b: TBitmap;
  vox: voxelbuffer_p;
  voxsize: integer;
begin
  Screen.Cursor := crHourGlass;
  wad := TWADWriter.Create;
  try
    script := TStringList.Create;
    try

      ms := TMemoryStream.Create;
      try
        PT_SavePropertiesBinary(rock.mProperties, ms);
        wad.AddData('DOOMROCK', ms.Memory, ms.Size);
      finally
        ms.Free;
      end;

      if ScriptRadioGroup.ItemIndex <> 2 then
      begin
        stmp := Trim(ActorNameEdit.Text);
        name := '';
        for i := 1 to Length(stmp) do
          if stmp[i] <> ' ' then
            name := name + stmp[i];
        script.Add('ACTOR ' + stmp + ' ' + EditorNumberEdit.Text);
        script.Add('{');
        script.Add('  Health 10000');
        script.Add('  Radius ' + RadiusEdit.Text);
        script.Add('  Height ' + HeightEdit.Text);
        script.Add('  Mass 100000');
        script.Add('  +SOLID');
        script.Add('  States');
        script.Add('  {');
        script.Add('  Spawn:');
        stmp := PrefixEdit.Text;
        script.Add('    ' + stmp[1] + stmp[2] + stmp[3] + stmp[4] + ' ' + stmp[5] + ' -1');
        script.Add('  }');
        script.Add('}');
        if ScriptRadioGroup.ItemIndex = 0 then
          wad.AddStringList('ACTORDEF', script)
        else
          wad.AddStringList('DECORATE', script);
      end;
    finally
      script.Free;
    end;
    wad.AddSeparator('S_START');

    b := TBitmap.Create;
    try
      b.Width := 256;
      b.Height := 255;
      b.PixelFormat := pf32bit;
      b.Canvas.Draw(0, 0, buffer);
      for i := 0 to 255 do
        b.Canvas.Pixels[i, 0] := RGB(0, 0, 0);
      case PatchRadioGroup.ItemIndex of
      0: ms := BmpAsPatch(b, @DoomPaletteRaw);
      1: ms := BmpAsPatch(b, @HereticPaletteRaw);
      2: ms := BmpAsPatch(b, @HexenPaletteRaw);
      3: ms := BmpAsPatch(b, @StrifePaletteRaw);
      else
        ms := BmpAsPatch(b, @RadixPaletteRaw);
      end;
      wad.AddData(PrefixEdit.Text + '0', ms.Memory, ms.Size);
      ms.Free;
    finally
      b.Free;
    end;

    wad.AddSeparator('S_END');

    if ScriptRadioGroup.ItemIndex = 0 then
      if GenerateVoxelCheckBox.Checked then
      begin
        GetMem(vox, SizeOf(voxelbuffer_t));

        if voxRadioButton64x64.Checked then
          voxsize := 64
        else if voxRadioButton128x128.Checked then
          voxsize := 128
        else
          voxsize := 256;

        DT_CreateVoxelFromRock(rock, vox, voxsize, rocktex);

        if ScriptRadioGroup.ItemIndex = 0 then
        begin
          VXE_ExportVoxelToDDVOX(vox, voxsize, 'vxtmp');
          wad.AddFile(PrefixEdit.Text, 'vxtmp');
          DeleteFile('vxtmp');
          wad.AddString('PK3ENTRY', PrefixEdit.Text + '=' + PrefixEdit.Text + '.DDVOX');
          wad.AddString('VOXELDEF', 'voxeldef ' + PrefixEdit.Text + '.ddvox replace sprite ' + PrefixEdit.Text);
        end
        else
        begin
          wad.AddSeparator('VX_START');
          if voxsize = 256 then
            voxsize := 254;
          case PatchRadioGroup.ItemIndex of
          0: VXE_ExportVoxelToSlab6VOX(vox, voxsize, @DoomPaletteRaw, 'vxtmp');
          1: VXE_ExportVoxelToSlab6VOX(vox, voxsize, @HereticPaletteRaw, 'vxtmp');
          2: VXE_ExportVoxelToSlab6VOX(vox, voxsize, @HexenPaletteRaw, 'vxtmp');
          3: VXE_ExportVoxelToSlab6VOX(vox, voxsize, @StrifePaletteRaw, 'vxtmp');
          else
            VXE_ExportVoxelToSlab6VOX(vox, voxsize, @RadixPaletteRaw, 'vxtmp');
          end;
          wad.AddFile(PrefixEdit.Text, 'vxtmp');
          DeleteFile('vxtmp');
          wad.AddSeparator('VX_END');
          wad.AddString('VOXELDEF', PrefixEdit.Text + '="' + PrefixEdit.Text + '.vox"{'#13#10'}');
        end;

        FreeMem(vox, SizeOf(voxelbuffer_t));
      end;

    BackupFile(FileNameEdit.Text);
    wad.SaveToFile(FileNameEdit.Text);
  finally
    wad.Free;
  end;
  Screen.Cursor := crDefault;
end;

procedure TExportSpriteForm.GenerateVoxelCheckBoxClick(Sender: TObject);
begin
  voxRadioButton64x64.Visible := GenerateVoxelCheckBox.Checked;
  voxRadioButton128x128.Visible := GenerateVoxelCheckBox.Checked;
  voxRadioButton256x256.Visible := GenerateVoxelCheckBox.Checked;
end;

procedure TExportSpriteForm.voxRadioButton64x64Click(Sender: TObject);
begin
  voxRadioButton64x64.Checked := True;
  voxRadioButton128x128.Checked := False;
  voxRadioButton256x256.Checked := False;
end;

procedure TExportSpriteForm.voxRadioButton128x128Click(Sender: TObject);
begin
  voxRadioButton64x64.Checked := False;
  voxRadioButton128x128.Checked := True;
  voxRadioButton256x256.Checked := False;
end;

procedure TExportSpriteForm.voxRadioButton256x256Click(Sender: TObject);
begin
  voxRadioButton64x64.Checked := False;
  voxRadioButton128x128.Checked := False;
  voxRadioButton256x256.Checked := True;
end;

end.
