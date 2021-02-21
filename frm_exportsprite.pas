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
  Dialogs, StdCtrls, ExtCtrls, Buttons;

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
    procedure SpritePrefixButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ExportSpriteForm: TExportSpriteForm;

implementation

{$R *.dfm}

procedure TExportSpriteForm.SpritePrefixButtonClick(Sender: TObject);
var
  s: string;
begin
  s := PrefixEdit.Text;
  if GetSpritePrefix(s) then
    PrefixEdit.Text := s;
end;

end.
