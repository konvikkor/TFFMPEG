object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 507
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 396
    Width = 635
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 0
    ExplicitWidth = 399
  end
  object Memo1: TMemo
    Left = 0
    Top = 399
    Width = 635
    Height = 89
    Align = alBottom
    TabOrder = 0
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 488
    Width = 635
    Height = 19
    Panels = <>
  end
  object MyFFMpegDisplay1: TMyFFMpegDisplay
    Left = 0
    Top = 0
    Width = 635
    Height = 396
    PopupMenu = PopupMenu1
    Align = alClient
    ExplicitTop = -3
  end
  object MyFFMpeg1: TMyFFMpeg
    Display = MyFFMpegDisplay1
    Seek = 0
    Left = 128
    Top = 144
  end
  object PopupMenu1: TPopupMenu
    Left = 248
    Top = 152
    object File1: TMenuItem
      Caption = 'File'
      object Open1: TMenuItem
        Caption = 'Open'
        OnClick = Open1Click
      end
      object Close1: TMenuItem
        Caption = 'Close'
      end
    end
    object Actions1: TMenuItem
      Caption = 'Actions'
      object Play1: TMenuItem
        Caption = 'Play'
        OnClick = Play1Click
      end
      object Stop1: TMenuItem
        Caption = 'Stop'
        OnClick = Stop1Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 128
    Top = 72
  end
end
