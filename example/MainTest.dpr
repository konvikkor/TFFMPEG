program MainTest;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {Form1},
  uFFMPG in '..\uFFMPG.pas',
  FFTypes in '..\FFMpeg_Header\FFTypes.pas',
  libavcodec in '..\FFMpeg_Header\libavcodec.pas',
  libavcodec_avfft in '..\FFMpeg_Header\libavcodec_avfft.pas',
  libavdevice in '..\FFMpeg_Header\libavdevice.pas',
  libavfilter in '..\FFMpeg_Header\libavfilter.pas',
  libavfilter_avcodec in '..\FFMpeg_Header\libavfilter_avcodec.pas',
  libavfilter_buffersink in '..\FFMpeg_Header\libavfilter_buffersink.pas',
  libavfilter_buffersrc in '..\FFMpeg_Header\libavfilter_buffersrc.pas',
  libavfilter_formats in '..\FFMpeg_Header\libavfilter_formats.pas',
  libavformat in '..\FFMpeg_Header\libavformat.pas',
  libavformat_avio in '..\FFMpeg_Header\libavformat_avio.pas',
  libavformat_url in '..\FFMpeg_Header\libavformat_url.pas',
  libavutil in '..\FFMpeg_Header\libavutil.pas',
  libavutil_audio_fifo in '..\FFMpeg_Header\libavutil_audio_fifo.pas',
  libavutil_avstring in '..\FFMpeg_Header\libavutil_avstring.pas',
  libavutil_bprint in '..\FFMpeg_Header\libavutil_bprint.pas',
  libavutil_buffer in '..\FFMpeg_Header\libavutil_buffer.pas',
  libavutil_channel_layout in '..\FFMpeg_Header\libavutil_channel_layout.pas',
  libavutil_common in '..\FFMpeg_Header\libavutil_common.pas',
  libavutil_cpu in '..\FFMpeg_Header\libavutil_cpu.pas',
  libavutil_dict in '..\FFMpeg_Header\libavutil_dict.pas',
  libavutil_display in '..\FFMpeg_Header\libavutil_display.pas',
  libavutil_error in '..\FFMpeg_Header\libavutil_error.pas',
  libavutil_eval in '..\FFMpeg_Header\libavutil_eval.pas',
  libavutil_fifo in '..\FFMpeg_Header\libavutil_fifo.pas',
  libavutil_file in '..\FFMpeg_Header\libavutil_file.pas',
  libavutil_frame in '..\FFMpeg_Header\libavutil_frame.pas',
  libavutil_hwcontext in '..\FFMpeg_Header\libavutil_hwcontext.pas',
  libavutil_imgutils in '..\FFMpeg_Header\libavutil_imgutils.pas',
  libavutil_log in '..\FFMpeg_Header\libavutil_log.pas',
  libavutil_mathematics in '..\FFMpeg_Header\libavutil_mathematics.pas',
  libavutil_md5 in '..\FFMpeg_Header\libavutil_md5.pas',
  libavutil_mem in '..\FFMpeg_Header\libavutil_mem.pas',
  libavutil_motion_vector in '..\FFMpeg_Header\libavutil_motion_vector.pas',
  libavutil_opt in '..\FFMpeg_Header\libavutil_opt.pas',
  libavutil_parseutils in '..\FFMpeg_Header\libavutil_parseutils.pas',
  libavutil_pixdesc in '..\FFMpeg_Header\libavutil_pixdesc.pas',
  libavutil_pixfmt in '..\FFMpeg_Header\libavutil_pixfmt.pas',
  libavutil_rational in '..\FFMpeg_Header\libavutil_rational.pas',
  libavutil_samplefmt in '..\FFMpeg_Header\libavutil_samplefmt.pas',
  libavutil_time in '..\FFMpeg_Header\libavutil_time.pas',
  libavutil_timestamp in '..\FFMpeg_Header\libavutil_timestamp.pas',
  libswresample in '..\FFMpeg_Header\libswresample.pas',
  libswscale in '..\FFMpeg_Header\libswscale.pas',
  sdl2 in '..\SDL_Header\sdl2.pas',
  sdl2_gfx in '..\SDL_Header\sdl2_gfx.pas',
  sdl2_image in '..\SDL_Header\sdl2_image.pas',
  sdl2_mixer in '..\SDL_Header\sdl2_mixer.pas',
  sdl2_net in '..\SDL_Header\sdl2_net.pas',
  sdl2_ttf in '..\SDL_Header\sdl2_ttf.pas',
  FFUtils in '..\FFMpeg_Header\FFUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
