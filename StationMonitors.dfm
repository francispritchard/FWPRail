object StationMonitorsWindow: TStationMonitorsWindow
  Left = 0
  Top = 0
  Align = alTop
  BorderStyle = bsNone
  Caption = 'Station Monitors'
  ClientHeight = 492
  ClientWidth = 688
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = StationMonitorsFormCreate
  OnPaint = StationMonitorsFormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object StationMonitorsTcpServer: TTcpServer
    LocalHost = '192.168.0.3'
    LocalPort = '80'
    OnAccept = StationMonitorsTcpServerAccept
    Left = 584
    Top = 328
  end
end
