object Form1: TForm1
  Left = 173
  Top = 211
  Width = 1044
  Height = 540
  Caption = 'ProcTree test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 29
    Width = 1028
    Height = 472
    Align = alClient
    Font.Charset = GREEK_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Courier'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 1028
    Height = 29
    ButtonWidth = 89
    Caption = 'ToolBar1'
    TabOrder = 1
    object ToolButton1: TToolButton
      Left = 0
      Top = 2
      Width = 8
      Caption = 'ToolButton1'
      Style = tbsSeparator
    end
    object SpeedButton1: TSpeedButton
      Left = 8
      Top = 2
      Width = 129
      Height = 22
      Caption = 'Generate tree'
      OnClick = SpeedButton1Click
    end
  end
end
