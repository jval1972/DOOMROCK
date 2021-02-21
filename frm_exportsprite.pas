//------------------------------------------------------------------------------
//
//  DOOMTREE: Doom Tree Sprite Generator
//  Copyright (C) 2021 by Jim Valavanis
//
// DESCRIPTION:
//  Export Sprite Form
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : https://sourceforge.net/projects/doom-tree/
//------------------------------------------------------------------------------

unit frm_exportsprite;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, dt_soft3d;

type
  TExportSpriteForm = class(TForm)
    Label1: TLabel;
    FileNameEdit: TEdit;
    SelectFileButton: TSpeedButton;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    SpritePrefixButton: TSpeedButton;
    PrefixEdit: TEdit;
    HQCheckBox: TCheckBox;
    PatchRadioGroup: TRadioGroup;
    AnglesRadioGroup: TRadioGroup;
    SpriteFormatRadioGroup: TRadioGroup;
    GroupBox3: TGroupBox;
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
    procedure SpritePrefixButtonClick(Sender: TObject);
    procedure SelectFileButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FileNameEditChange(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
  private
    { Private declarations }
    buffer: TBitmap;
    device: device_t;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  frm_spriteprefix;

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

  device_init(@device, 255, 255);
  buffer := TBitmap.Create;
  buffer.Width := 255;
  buffer.Height := 255;
  buffer.PixelFormat := pf32bit;
end;

procedure TExportSpriteForm.FormDestroy(Sender: TObject);
begin
  device_destroy(@device);
  buffer.Free;
end;

procedure TExportSpriteForm.FileNameEditChange(Sender: TObject);
begin
  Button1.Enabled := FileNameEdit.Text <> '';
end;

procedure TExportSpriteForm.PaintBox1Paint(Sender: TObject);
begin
  PaintBox1.Canvas.Draw(0, 0, buffer);
end;

end.
