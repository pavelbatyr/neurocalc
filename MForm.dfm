object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = #1053#1077#1081#1088#1086#1082#1072#1083#1100#1082#1091#1083#1103#1090#1086#1088
  ClientHeight = 538
  ClientWidth = 782
  Color = clBtnFace
  Constraints.MinHeight = 250
  Constraints.MinWidth = 250
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  OnResize = FormResize
  PixelsPerInch = 120
  TextHeight = 16
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 782
    Height = 29
    Caption = 'ToolBar'
    Images = Images
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object tbNew: TToolButton
      Left = 0
      Top = 0
      Action = acNew
      ImageIndex = 0
    end
    object tbOpen: TToolButton
      Left = 23
      Top = 0
      Action = acOpen
      ImageIndex = 1
    end
    object tbSave: TToolButton
      Left = 46
      Top = 0
      Action = acSave
      ImageIndex = 2
    end
    object ToolButton1: TToolButton
      Left = 69
      Top = 0
      Width = 200
      Caption = 'ToolButton1'
      ImageIndex = 3
      Style = tbsSeparator
    end
    object btnToModel: TButton
      Left = 269
      Top = 0
      Width = 75
      Height = 22
      Caption = #1052#1086#1076#1077#1083#1100
      TabOrder = 0
      OnClick = btnToModelClick
    end
    object btnToTrain: TButton
      Left = 344
      Top = 0
      Width = 75
      Height = 22
      Caption = #1054#1073#1091#1095#1077#1085#1080#1077
      TabOrder = 1
      OnClick = btnToTrainClick
    end
  end
  object Status: TStatusBar
    Left = 0
    Top = 518
    Width = 782
    Height = 20
    Panels = <
      item
        Width = 50
      end>
  end
  object MainPanel: TPanel
    Left = 0
    Top = 29
    Width = 782
    Height = 489
    Align = alClient
    TabOrder = 2
  end
  object Images: TImageList
    Left = 544
    Top = 8
    Bitmap = {
      494C010103000800980010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000815A1DFBA282
      51F89D7C47F39D7C48F39D7C48F39D7C48F39D7C48F39D7C48F39D7C48F39D7C
      48F39D7B4AF2997544FF8C6632D6000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C1761BFFC27519FFBD6B13FFB965
      04FFB96504FFB96504FFBA6504FFBA6504FFBA6504FFBA6504FFBA6504FFBA65
      04FFBA6504FFBC690AFFB96A15FFC3791FFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A08052FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF987644E100000000657AB69F3370C2FF206DC9FF1E69
      C6FF1B67C6FF1A65C6FF1864C6FF1663C6FF1460C4FF125EC6FF105CC4FF105B
      C4FF105BC4FF105EC8FF1059C1FF5D76B5A2D5933DFFEFB736FFCDC6C0FFE9F8
      FFFFDBE5F6FFDBE8F8FFDBE8F8FFDBE8F9FFDBE8F8FFDAE7F8FFDBE7F8FFD8E4
      F5FFE9F6FFFFCDC6C0FFEAA714FFC0761DFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A68556FFFFFF
      FFFFF5F4ECFFF3F1E8FFF3F1E9FFF3F1E9FFF2F0E7FFEFE8DEFFEEE9E0FFEFEB
      E2FFF6F3EDFFF0EBE2FF987137E100000000284AA1FF80DFFFFF41BCFFFF43B8
      FFFF3FB4FFFF3BB1FFFF36AEFFFF32A9FFFF2EA6FFFF29A1FFFF259EFFFF219A
      FFFF2097FFFF209AFFFF229FFFFF063AA0FFCD9551FFE8AE3CFFDCD7D4FFECE8
      E9FFADA0A2FFA79B9EFF9E9395FF94898CFF8A8185FF83797CFF7B7276FF685F
      64FFECE8E9FFDCD7D4FFE59E20FFC77B25FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000AD9164FFFFFF
      FFFFF6F4EFFFF5F2EFFFF5F2EFFFF5F2EFFFF5F2EFFFF6F5EDFFF3F4E8FFF3F0
      EAFFFCFCF9FFEFE9E0FF987239E1000000002447A3FF79D3FFFF40B1FEFF41AE
      FCFF3FABFCFF39A7FAFF37A3F9FF33A0F9FF2F9CF9FF2C99F9FF2896F8FF2393
      F8FF1E8EF8FF1D8DF9FF1F92FFFF053CA2FFD09653FFEAB447FFDCD7D4FFEFF0
      EFFFDFDEDCFFE1E0DFFFE0DFDEFFDFE0DDFFE0DFDDFFDFDEDDFFDFE0DEFFDBD9
      D9FFEDEDEDFFDCD7D4FFE7A62BFFC9802BFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B39669FFFFFF
      FFFFF6F4EFFFF5F2EDFFF5F2EDFFF5F2EDFFF5F3EEFFF5F2EEFFF7F3EFFFF5F2
      EDFFFDFDFAFFEFE8E0FF987239E100000000254CA8FF7ED7FFFF45B7FEFF48B3
      FCFF44B0FCFF40ACFCFF3CA9FBFF39A6FAFF35A2FAFF319FFAFF2D9BFAFF2998
      F9FF2594F9FF2292FBFF2095FFFF063FA6FFD49B58FFEBB950FFDCD7D4FFECE8
      E9FFA99D9FFFA4999EFF9A9194FF92888BFF897F85FF82797CFF7A7177FF655C
      62FFECE8E9FFDCD7D4FFE8AC37FFCC8531FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B19667FFFFFF
      FFFFF6F4EFFFF5F2EDFFF5F2EDFFF5F2EDFFF7F6EFFFF6F1EEFFFCF6F4FFFAF3
      F2FFFEFBFDFFEFE9DFFF987238E100000000254CA8FF84DBFFFF4CBBFEFF4DB8
      FDFF4AB5FDFF45B1FCFF41AEFCFF3EAAFCFF3AA7FAFF36A4FAFF32A0FAFF2D9D
      FAFF2B99FAFF2998FAFF2499FFFF0642ACFFD69E5BFFEDBD5AFFDCD7D4FFFFFF
      FFFFFFFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFDCD7D4FFEAB340FFD08B34FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B19666FFFFFF
      FFFFF6F4EFFFF5F2EDFFF4F1ECFFF4F0ECFFF9F3F2FFFBFAF0FFFBF9F5FFFBF4
      F3FFFCF7F3FFEFEAE3FF98723AE1000000002554B3FF80DDFFFF4EBFFEFF50BD
      FDFF4FBAFDFF4CB6FCFF46B3FCFF43AFFCFF3FABFCFF3DA7FBFF37A4FAFF33A1
      FAFF2D9CFAFF2396FBFF259BFFFF0642ACFFD9A45EFFF0C263FFDCD7D4FFECE8
      E9FFA99D9FFFA4999EFF9A9194FF92888BFF897F85FF82797CFF7A7177FF655C
      62FFECE8E9FFDCD7D4FFEDB749FFD2903AFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B0956DFFFFFF
      FFFFF7F4EDFFF4F1ECFFF6F2EDFFF9F2F2FFFAFAF4FFFBFDF6FFFCFAF9FFF4EF
      E7FFF5F1ECFFF0EAE2FF99733BE1000000002554B3FFAAECFFFF61C9FEFF53C2
      FDFF4EBDFDFF4AB9FDFF46B5FDFF42B1FCFF3EAEFCFF3AAAFCFF36A6FBFF31A3
      FAFF36A2FBFF4DADFDFF6FC0FFFF1450B7FFD8A35CFFF0C66DFFDCD7D4FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFDCD7D4FFEEBD54FFD7963EFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B19E7CFFFFFF
      FFFFFCFEF8FFF9F9F0FFFBFAF9FFFAF9FCFFFBFCFBFFFAFAFCFFF3F1E9FFEAE5
      DDFFECE7E0FFE6D8CBFF99753BE100000000295ABCFFBEF4FFFF95DEFFFF93D9
      FEFF82D3FEFF71C9FDFF69C6FDFF67C4FDFF62C0FDFF5FBCFDFF6ABFFDFF72C2
      FDFF7EC6FDFF7CC5FEFF80CBFFFF1452BBFFDEAC69FFF9D281FFC1975CFF9A7B
      60FF95775EFF97795DFF97795DFF97795DFF97795DFF97795CFF97795CFF9577
      5EFF9A7A5EFFC19A64FFF7CA6BFFD99B44FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BDA787FFFFFF
      FFFFFDFEFCFFFAFBF8FFFAFAFCFFFAFBFEFFFCFBF9FFF5F2EAFFF0EEE7FFE9E2
      DAFFE6DDD4FFD6CBB4FF9A763DE100000000285DC2FFBDF5FFFF93DFFFFF94DC
      FEFF93DAFEFF91DAFEFF90D8FFFF8DD6FFFF8BD4FFFF87D1FFFF83CDFFFF7EC8
      FFFF76C5FFFF76C4FFFF7BCBFFFF1456C2FFDDAB67FFF6D58BFFFFD056FFC0A8
      87FFC8C5C9FFCEC6BFFFCDC6C0FFCDC6C0FFCDC6BFFFD6D0CAFFD6D3D0FFCFCE
      D4FFC0A888FFFFD25DFFF3CC75FFDCA148FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C5B190FFFFFF
      FFFFFCFDFDFFFAF9F9FFFAFBFDFFFBFDFAFFF9F7F0FFF3EEE4FFDDD4C5FFD4BE
      ABFFD0BCA1FFBEA784FF9A773EE100000000285DC2FFC0F8FFFF97E1FFFF95DF
      FEFF93DFFFFFB4EFFFFFB2EDFFFFB0EBFFFFAFE9FFFFACE6FFFFA9E4FFFFA7E2
      FFFFA5E0FFFFA7E1FFFFA9E5FFFF1B5FCAFFDCA966FFF6D993FFFBC85DFFC2B4
      A2FFD7DEEBFFDDDDDDFFDCDDDEFFDCDBDDFFE7E8EAFFC8BAA7FFA29692FFC2B4
      A2FFC6BCA9FFFBCB63FFF3D07EFFE0A74CFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C7B596FFFFFF
      FFFFF8FDFDFFFAFBFBFFFBFDFAFFF6F7EBFFF3F1E9FFE3D9CAFFC9B493FFEBE3
      DCFFE1D9C6FFB79D73FF9C763EE400000000285DC2FFBFFBFFFF99E5FFFF98E2
      FFFF9DE9FFFF4881D9FF2B60CDFF2D6CD2FF2D6BD1FF2D6BD1FF2C6AD1FF2B6A
      D0FF2D6BD0FF2D6CD1FF2E6DD3FF84A8E17CE5B973FFF6DA97FFFBCC62FFC8BA
      A7FFDDE0E9FFE1DFDDFFE0DFDEFFDFDDDCFFEFF3F9FF9F886FFFE5AF47FF9E91
      89FFC7BDB2FFFDCF6AFFF5D484FFE3AC51FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C8B599FFFFFF
      FFFFFBFDFFFFFDFDFDFFF6F7F2FFEDE7E0FFEFE4DDFFD1BEA6FFCDB99AFFFFFF
      FFFFCFBBA1FFB29569CFF9F7F40D000000002E6ED5FFD4FFFFFFA8F1FFFFA6EF
      FFFFB1F9FFFF1462D2FF00000000000000000000000000000000000000000000
      000000000000000000000000000000000000E9BC75FFF8DD9EFFFDCF69FFCEC0
      AFFFE3E7EFFFE7E5E3FFE6E5E4FFE5E4E2FFF1F6FFFFBAA386FFFFE873FFB5AB
      9EFFCAC0B8FFFFD26EFFF9DA8EFFE7B25BFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C9B89BFFFFFF
      FFFFFCFFFFFFFCF8F3FFF4EBE5FFE5DFD7FFE3D6C6FFCDB696FFBCAA89FFD6C8
      B4FFA98956D70000000000000000000000001765D7FDB6E5FAFFB3E8FBFFB2E5
      FCFFA8DEF8FF1667D9FF00000000000000000000000000000000000000000000
      000000000000000000000000000000000000EAC079FFF8E09BFFFBD165FFD3C4
      AFFFEAEEF6FFECEBE8FFECEBE9FFEBE9E6FFFBFFFFFFA28E78FFDEAF4FFFA89C
      95FFD1C7B9FFFFDA78FFF5D889FFE2A442FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CBBDA2FFFFFF
      FFFFFFFFFFFFFAF7F6FFF0E8DFFFDFD7C7FFD8C6B1FFC1AA85FF9F824CFFB195
      69D100000000000000000000000000000000ACC7F1531E6BDDEB2A71DDE32A6F
      DDE31F6BDDEBA2C0EF5D00000000000000000000000000000000000000000000
      000000000000000000000000000000000000ECC47EFFFEF4D5FFFFE290FFDCD7
      D4FFF5FFFFFFF6FEFFFFF6FEFFFFF6FDFFFFFFFFFFFFDFDDDCFFC8BAA7FFDFDD
      DCFFE5E4E2FFFFDE88FFE4AA45FFFBF4EB190000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C3B292FBCEBC
      A3F8CCBBA0F3C8B69AF3C7B599F3C4AE8CF3C2AA84F3AA9161FFA98B5DCA0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ECC681FFF0CA82FFF4CA7DFFE8C7
      88FFEFCF94FFEED397F1EDCF92FFEED092FFEED093FFF2D396FFF7D79BFFF6D6
      9BFFE6C48AFFEBB552FFFDF8F111FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00C001FFFF00000000C001000000000000
      C001000000000000C001000000000000C001000000000000C001000000000000
      C001000000000000C001000000000000C001000000000000C001000000000000
      C001000000000000C001000000000000C00103FF00000000C00703FF00000000
      C00F03FF00000000C01FFFFF0000000000000000000000000000000000000000
      000000000000}
  end
  object MainMenu: TMainMenu
    Images = Images
    Left = 648
    Top = 8
    object nFile: TMenuItem
      Caption = #1060#1072#1081#1083
      object nNew: TMenuItem
        Action = acNew
        ImageIndex = 0
      end
      object nOpen: TMenuItem
        Action = acOpen
        ImageIndex = 1
      end
      object nSave: TMenuItem
        Action = acSave
        ImageIndex = 2
      end
    end
  end
  object ActionList: TActionList
    Left = 744
    Top = 8
    object acNew: TAction
      Caption = #1053#1086#1074#1099#1081
      Hint = #1053#1086#1074#1099#1081
      OnExecute = acNewExecute
    end
    object acOpen: TAction
      Caption = #1054#1090#1082#1088#1099#1090#1100
      Hint = #1054#1090#1082#1088#1099#1090#1100
      OnExecute = acOpenExecute
    end
    object acSave: TAction
      Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100
      Hint = #1057#1086#1093#1088#1072#1085#1080#1090#1100
      OnExecute = acSaveExecute
    end
  end
  object Open: TOpenDialog
    Left = 592
    Top = 8
  end
  object Save: TSaveDialog
    Left = 696
    Top = 8
  end
end
