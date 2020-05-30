unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Buttons, ToolWin, StdCtrls,
  proctree;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    ToolBar1: TToolBar;
    SpeedButton1: TSpeedButton;
    ToolButton1: TToolButton;
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
    procedure printtree(const t: tree_t);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.printtree(const t: tree_t);
var
  i: integer;
  s: TStringList;
begin
  Memo1.Lines.Clear;
  s := TStringList.Create;
  s.Add('-------------------------------------------------------------------');
  s.Add(Format('mVertCount=%d', [t.mVertCount]));
  for i := 0 to t.mVertCount - 1 do
    s.Add(Format('%2.5f  %2.5f  %2.5f', [t.mVert[i].x, t.mVert[i].y, t.mVert[i].z]));
  s.Add('');
  s.Add('-------------------------------------------------------------------');
  s.Add(Format('mTwigVertCount=%d', [t.mTwigVertCount]));
  for i := 0 to t.mTwigVertCount - 1 do
    s.Add(Format('%2.5f  %2.5f  %2.5f', [t.mTwigVert[i].x, t.mTwigVert[i].y, t.mTwigVert[i].z]));
  Memo1.Lines.AddStrings(s);
  s.Free;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
var
  tree: tree_t;
begin
  tree := tree_t.Create;
  tree.generate;
  printtree(tree);
  tree.Free;
end;

end.
