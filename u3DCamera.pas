unit u3DCamera;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,

  uMediaConstant,
  Winapi.GDIPOBJ, Winapi.GDIPAPI, OpenGL, System.TimeSpan,
  //Winapi.D3D11Shader, Winapi.D3D11,

  Vcl.ExtCtrls,Math,System.SyncObjs,System.Generics.Collections,

  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.DateUtils, mmsystem,
  libavcodec, libavdevice, libavfilter, libswresample, libswscale,
  libavutil, libavformat,
  sdl2, {SDL2_ttf,{sdl, {uResourcePaths,} System.Threading;

type
  T3DCamera = class(TComponent)
  private
    FCameraPoint: T3DPoint;
    FTargetPoint: T3DPoint;
  protected
    Procedure WMUpdateRender(var Message: TWMEraseBkgnd);message WM_3D_CAMERA_RENDER;
  public
    constructor Create(AOwner: TComponent); override;
    property Camera:T3DPoint Read FCameraPoint Write FCameraPoint;
    property Target:T3DPoint Read FTargetPoint Write FTargetPoint;
  published
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MY Media',[T3DCamera]);
end;

{ T3DCamera }

constructor T3DCamera.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

procedure T3DCamera.WMUpdateRender(var Message: TWMEraseBkgnd);
begin

end;

end.
