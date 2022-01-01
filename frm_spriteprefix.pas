//------------------------------------------------------------------------------
//
//  DOOMROCK: Doom Rock Sprite Generator
//  Copyright (C)2021-2022 by Jim Valavanis
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
//  Sprite prefix input form
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/doom-rock/
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
