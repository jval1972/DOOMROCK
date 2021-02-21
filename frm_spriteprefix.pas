//------------------------------------------------------------------------------
//
//  DOOMTREE: Doom Tree Sprite Generator
//  Copyright (C) 2021 by Jim Valavanis
//
// DESCRIPTION:
//  Sprite prefix input form
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : https://sourceforge.net/projects/doom-tree/
//------------------------------------------------------------------------------

unit frm_spriteprefix;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TSpritePrefixForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    NameEdit: TEdit;
    LabelSName: TLabel;
    FrameEdit: TEdit;
    LabelSFrame: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;


function GetSpritePrefix(var sprefix: string): boolean;

implementation

{$R *.dfm}

function GetSpritePrefix(var sprefix: string): boolean;
var
  f: TSpritePrefixForm;
  sname, sframe: string;
begin
  Result := False;
  while Length(sprefix) < 5 do
    sprefix := sprefix + ' ';
  sname := sprefix[1] + sprefix[2] + sprefix[3] + sprefix[4];
  sframe := sprefix[5];
  f := TSpritePrefixForm.Create(nil);
  try
    f.NameEdit.Text := sname;
    f.FrameEdit.Text := sframe;
    f.ShowModal;
    if f.ModalResult = mrOK then
    begin
      sname := f.NameEdit.Text;
      while Length(sname) < 4 do
        sname := sname + ' ';
      sframe := f.FrameEdit.Text;
      if sframe = '' then
        sframe := ' ';
      sprefix := sname + sframe;
      Result := True;
    end;
  finally
    f.Free;
  end;
end;

end.
