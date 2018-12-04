object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 325
  ClientWidth = 701
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PopupMenu = PopupMenu1
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 701
    Height = 306
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'LOG'
      object Splitter1: TSplitter
        Left = 505
        Top = 0
        Height = 278
        Align = alRight
        ExplicitLeft = 240
        ExplicitTop = 128
        ExplicitHeight = 100
      end
      object Splitter2: TSplitter
        Left = 185
        Top = 0
        Height = 278
        ExplicitLeft = 256
        ExplicitTop = 112
        ExplicitHeight = 100
      end
      object Memo1: TMemo
        Left = 188
        Top = 0
        Width = 317
        Height = 278
        Align = alClient
        Lines.Strings = (
          'Memo1')
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object Memo2: TMemo
        Left = 508
        Top = 0
        Width = 185
        Height = 278
        Align = alRight
        Lines.Strings = (
          'Memo2')
        ScrollBars = ssVertical
        TabOrder = 1
      end
      object Memo3: TMemo
        Left = 0
        Top = 0
        Width = 185
        Height = 278
        Align = alLeft
        Lines.Strings = (
          'Memo3')
        ScrollBars = ssVertical
        TabOrder = 2
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 1
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 306
    Width = 701
    Height = 19
    Panels = <
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 50
      end>
  end
  object PopupMenu1: TPopupMenu
    Left = 88
    Top = 64
    object Open1: TMenuItem
      Caption = 'Open'
      OnClick = Open1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object DecodePak1: TMenuItem
      Caption = 'Decode Pak'
      OnClick = DecodePak1Click
    end
    object Play1: TMenuItem
      Caption = 'Play'
      OnClick = Play1Click
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 176
    Top = 48
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 132
    Top = 40
  end
  object TrayIcon1: TTrayIcon
    PopupMenu = PopupMenu1
    Visible = True
    Left = 228
    Top = 64
  end
  object TaskDialog1: TTaskDialog
    Buttons = <>
    RadioButtons = <>
    Left = 252
    Top = 152
  end
end
