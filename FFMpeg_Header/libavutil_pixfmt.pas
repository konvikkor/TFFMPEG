(*
 * copyright (c) 2006 Michael Niedermayer <michaelni@gmx.at>
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *)

(**
 * @file
 * pixel format definitions
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: libavutil/pixfmt.h
 * Ported by CodeCoolie@CNSW 2009/03/18 -> $Date:: 2017-05-30 #$
 *)

(*
FFmpeg Delphi/Pascal Headers and Examples License Agreement

A modified part of FFVCL - Delphi FFmpeg VCL Components.
Copyright (c) 2008-2018 DelphiFFmpeg.com
All rights reserved.
http://www.DelphiFFmpeg.com

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

This source code is provided "as is" by DelphiFFmpeg.com without
warranty of any kind, either expressed or implied, including but not
limited to the implied warranties of merchantability and/or fitness
for a particular purpose.

Please also notice the License agreement of FFmpeg libraries.
*)

unit libavutil_pixfmt;

interface

{$I CompilerDefines.inc}

{$I libversion.inc}

const
  AVPALETTE_SIZE = 1024;
  AVPALETTE_COUNT = 256;

{$IF Defined(BCB) and Defined(VER140)} // C++Builder 6
  AV_PIX_FMT_NONE=-1;
  AV_PIX_FMT_YUV420P=$0;
  AV_PIX_FMT_YUYV422=$1;
  AV_PIX_FMT_RGB24=$2;
  AV_PIX_FMT_BGR24=$3;
  AV_PIX_FMT_YUV422P=$4;
  AV_PIX_FMT_YUV444P=$5;
  AV_PIX_FMT_YUV410P=$6;
  AV_PIX_FMT_YUV411P=$7;
  AV_PIX_FMT_GRAY8=$8;
  AV_PIX_FMT_MONOWHITE=$9;
  AV_PIX_FMT_MONOBLACK=$A;
  AV_PIX_FMT_PAL8=$B;
  AV_PIX_FMT_YUVJ420P=$C;
  AV_PIX_FMT_YUVJ422P=$D;
  AV_PIX_FMT_YUVJ444P=$E;
{$IFDEF FF_API_XVMC}
  AV_PIX_FMT_XVMC_MPEG2_MC=$F;
  AV_PIX_FMT_XVMC_MPEG2_IDCT=$10;
//AV_PIX_FMT_XVMC = AV_PIX_FMT_XVMC_MPEG2_IDCT,
  _AV_PIX_FMT_DELTA=0;  
{$ELSE}
  _AV_PIX_FMT_DELTA=2;  
{$ENDIF}
  AV_PIX_FMT_UYVY422=$11-_AV_PIX_FMT_DELTA;
  AV_PIX_FMT_UYYVYY411=$12-_AV_PIX_FMT_DELTA;
  AV_PIX_FMT_BGR8=$13-_AV_PIX_FMT_DELTA;
  AV_PIX_FMT_BGR4=$14-_AV_PIX_FMT_DELTA;
  AV_PIX_FMT_BGR4_BYTE=$15-_AV_PIX_FMT_DELTA;
  AV_PIX_FMT_RGB8=$16-_AV_PIX_FMT_DELTA;
  AV_PIX_FMT_RGB4=$17-_AV_PIX_FMT_DELTA;
  AV_PIX_FMT_RGB4_BYTE=$18-_AV_PIX_FMT_DELTA;
  AV_PIX_FMT_NV12=$19-_AV_PIX_FMT_DELTA;
  AV_PIX_FMT_NV21=$1A-_AV_PIX_FMT_DELTA;
  AV_PIX_FMT_ARGB=$1B-_AV_PIX_FMT_DELTA;
  AV_PIX_FMT_RGBA=$1C-_AV_PIX_FMT_DELTA;
  AV_PIX_FMT_ABGR=$1D-_AV_PIX_FMT_DELTA;
  AV_PIX_FMT_BGRA=$1E-_AV_PIX_FMT_DELTA;
  AV_PIX_FMT_GRAY16BE=$1F-_AV_PIX_FMT_DELTA;
  AV_PIX_FMT_GRAY16LE=$20-_AV_PIX_FMT_DELTA;
  AV_PIX_FMT_YUV440P=$21-_AV_PIX_FMT_DELTA;
  AV_PIX_FMT_YUVJ440P=$22-_AV_PIX_FMT_DELTA;
  AV_PIX_FMT_YUVA420P=$23-_AV_PIX_FMT_DELTA;
{$IFDEF FF_API_VDPAU}
  AV_PIX_FMT_VDPAU_H264=$24-_AV_PIX_FMT_DELTA;
  AV_PIX_FMT_VDPAU_MPEG1=$25-_AV_PIX_FMT_DELTA;
  AV_PIX_FMT_VDPAU_MPEG2=$26-_AV_PIX_FMT_DELTA;
  AV_PIX_FMT_VDPAU_WMV3=$27-_AV_PIX_FMT_DELTA;
  AV_PIX_FMT_VDPAU_VC1=$28-_AV_PIX_FMT_DELTA;
  _AV_PIX_FMT_DELTA2=_AV_PIX_FMT_DELTA;
{$ELSE}
  _AV_PIX_FMT_DELTA2=_AV_PIX_FMT_DELTA+5;
{$ENDIF}
  AV_PIX_FMT_RGB48BE=$29-_AV_PIX_FMT_DELTA2;
  AV_PIX_FMT_RGB48LE=$2A-_AV_PIX_FMT_DELTA2;
  AV_PIX_FMT_RGB565BE=$2B-_AV_PIX_FMT_DELTA2;
  AV_PIX_FMT_RGB565LE=$2C-_AV_PIX_FMT_DELTA2;
  AV_PIX_FMT_RGB555BE=$2D-_AV_PIX_FMT_DELTA2;
  AV_PIX_FMT_RGB555LE=$2E-_AV_PIX_FMT_DELTA2;
  AV_PIX_FMT_BGR565BE=$2F-_AV_PIX_FMT_DELTA2;
  AV_PIX_FMT_BGR565LE=$30-_AV_PIX_FMT_DELTA2;
  AV_PIX_FMT_BGR555BE=$31-_AV_PIX_FMT_DELTA2;
  AV_PIX_FMT_BGR555LE=$32-_AV_PIX_FMT_DELTA2;
{$IFDEF FF_API_VAAPI}
  AV_PIX_FMT_VAAPI_MOCO=$33-_AV_PIX_FMT_DELTA2;
  AV_PIX_FMT_VAAPI_IDCT=$34-_AV_PIX_FMT_DELTA2;
  AV_PIX_FMT_VAAPI_VLD=$35-_AV_PIX_FMT_DELTA2;
  AV_PIX_FMT_VAAPI=AV_PIX_FMT_VAAPI_VLD;
  _AV_PIX_FMT_DELTA3=_AV_PIX_FMT_DELTA2;
{$ELSE}
  AV_PIX_FMT_VAAPI=$33-_AV_PIX_FMT_DELTA2;
  _AV_PIX_FMT_DELTA3=_AV_PIX_FMT_DELTA2+2;
{$ENDIF}
  AV_PIX_FMT_YUV420P16LE=$36-_AV_PIX_FMT_DELTA3;
  AV_PIX_FMT_YUV420P16BE=$37-_AV_PIX_FMT_DELTA3;
  AV_PIX_FMT_YUV422P16LE=$38-_AV_PIX_FMT_DELTA3;
  AV_PIX_FMT_YUV422P16BE=$39-_AV_PIX_FMT_DELTA3;
  AV_PIX_FMT_YUV444P16LE=$3A-_AV_PIX_FMT_DELTA3;
  AV_PIX_FMT_YUV444P16BE=$3B-_AV_PIX_FMT_DELTA3;
{$IFDEF FF_API_VDPAU}
  AV_PIX_FMT_VDPAU_MPEG4=$3C-_AV_PIX_FMT_DELTA3;
  _AV_PIX_FMT_DELTA4=_AV_PIX_FMT_DELTA3;
{$ELSE}
  _AV_PIX_FMT_DELTA4=_AV_PIX_FMT_DELTA3+1;
{$ENDIF}
  AV_PIX_FMT_DXVA2_VLD=$3D-_AV_PIX_FMT_DELTA4;
  AV_PIX_FMT_RGB444LE=$3E-_AV_PIX_FMT_DELTA4;
  AV_PIX_FMT_RGB444BE=$3F-_AV_PIX_FMT_DELTA4;
  AV_PIX_FMT_BGR444LE=$40-_AV_PIX_FMT_DELTA4;
  AV_PIX_FMT_BGR444BE=$41-_AV_PIX_FMT_DELTA4;
  AV_PIX_FMT_YA8=$42-_AV_PIX_FMT_DELTA4;
  AV_PIX_FMT_BGR48BE=$43-_AV_PIX_FMT_DELTA4;
  AV_PIX_FMT_BGR48LE=$44-_AV_PIX_FMT_DELTA4;
  AV_PIX_FMT_YUV420P9BE=$45-_AV_PIX_FMT_DELTA4;
  AV_PIX_FMT_YUV420P9LE=$46-_AV_PIX_FMT_DELTA4;
  AV_PIX_FMT_YUV420P10BE=$47-_AV_PIX_FMT_DELTA4;
  AV_PIX_FMT_YUV420P10LE=$48-_AV_PIX_FMT_DELTA4;
  AV_PIX_FMT_YUV422P10BE=$49-_AV_PIX_FMT_DELTA4;
  AV_PIX_FMT_YUV422P10LE=$4A-_AV_PIX_FMT_DELTA4;
  AV_PIX_FMT_YUV444P9BE=$4B-_AV_PIX_FMT_DELTA4;
  AV_PIX_FMT_YUV444P9LE=$4C-_AV_PIX_FMT_DELTA4;
  AV_PIX_FMT_YUV444P10BE=$4D-_AV_PIX_FMT_DELTA4;
  AV_PIX_FMT_YUV444P10LE=$4E-_AV_PIX_FMT_DELTA4;
  AV_PIX_FMT_YUV422P9BE=$4F-_AV_PIX_FMT_DELTA4;
  AV_PIX_FMT_YUV422P9LE=$50-_AV_PIX_FMT_DELTA4;
  AV_PIX_FMT_VDA_VLD=$51-_AV_PIX_FMT_DELTA4;
  _AV_PIX_FMT_DELTA5=_AV_PIX_FMT_DELTA4+4;
  AV_PIX_FMT_GBRP=$56-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_GBRP9BE=$57-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_GBRP9LE=$58-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_GBRP10BE=$59-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_GBRP10LE=$5A-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_GBRP16BE=$5B-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_GBRP16LE=$5C-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_YUVA422P=$5D-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_YUVA444P=$5E-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_YUVA420P9BE=$5F-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_YUVA420P9LE=$60-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_YUVA422P9BE=$61-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_YUVA422P9LE=$62-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_YUVA444P9BE=$63-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_YUVA444P9LE=$64-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_YUVA420P10BE=$65-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_YUVA420P10LE=$66-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_YUVA422P10BE=$67-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_YUVA422P10LE=$68-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_YUVA444P10BE=$69-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_YUVA444P10LE=$6A-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_YUVA420P16BE=$6B-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_YUVA420P16LE=$6C-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_YUVA422P16BE=$6D-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_YUVA422P16LE=$6E-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_YUVA444P16BE=$6F-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_YUVA444P16LE=$70-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_VDPAU=$71-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_XYZ12LE=$72-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_XYZ12BE=$73-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_NV16=$74-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_NV20LE=$75-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_NV20BE=$76-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_RGBA64BE=$77-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_RGBA64LE=$78-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_BGRA64BE=$79-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_BGRA64LE=$7A-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_YVYU422=$7B-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_VDA=$7C-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_YA16BE=$7D-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_YA16LE=$7E-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_GBRAP=$7F-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_GBRAP16BE=$80-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_GBRAP16LE=$81-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_QSV=$82-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_MMAL=$83-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_D3D11VA_VLD=$84-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_CUDA=$85-_AV_PIX_FMT_DELTA5;
  AV_PIX_FMT_0RGB=$123+4;
  AV_PIX_FMT_RGB0=$123+5;
  AV_PIX_FMT_0BGR=$123+6;
  AV_PIX_FMT_BGR0=$123+7;
  AV_PIX_FMT_YUV420P12BE=$123+$8;
  AV_PIX_FMT_YUV420P12LE=$123+$9;
  AV_PIX_FMT_YUV420P14BE=$123+$A;
  AV_PIX_FMT_YUV420P14LE=$123+$B;
  AV_PIX_FMT_YUV422P12BE=$123+$C;
  AV_PIX_FMT_YUV422P12LE=$123+$D;
  AV_PIX_FMT_YUV422P14BE=$123+$E;
  AV_PIX_FMT_YUV422P14LE=$123+$F;
  AV_PIX_FMT_YUV444P12BE=$123+$10;
  AV_PIX_FMT_YUV444P12LE=$123+$11;
  AV_PIX_FMT_YUV444P14BE=$123+$12;
  AV_PIX_FMT_YUV444P14LE=$123+$13;
  AV_PIX_FMT_GBRP12BE=$123+$14;
  AV_PIX_FMT_GBRP12LE=$123+$15;
  AV_PIX_FMT_GBRP14BE=$123+$16;
  AV_PIX_FMT_GBRP14LE=$123+$17;
  AV_PIX_FMT_YUVJ411P=$123+$18;
  AV_PIX_FMT_BAYER_BGGR8=$123+$19;
  AV_PIX_FMT_BAYER_RGGB8=$123+$1A;
  AV_PIX_FMT_BAYER_GBRG8=$123+$1B;
  AV_PIX_FMT_BAYER_GRBG8=$123+$1C;
  AV_PIX_FMT_BAYER_BGGR16LE=$123+$1D;
  AV_PIX_FMT_BAYER_BGGR16BE=$123+$1E;
  AV_PIX_FMT_BAYER_RGGB16LE=$123+$1F;
  AV_PIX_FMT_BAYER_RGGB16BE=$123+$20;
  AV_PIX_FMT_BAYER_GBRG16LE=$123+$21;
  AV_PIX_FMT_BAYER_GBRG16BE=$123+$22;
  AV_PIX_FMT_BAYER_GRBG16LE=$123+$23;
  AV_PIX_FMT_BAYER_GRBG16BE=$123+$24;
{$IFNDEF FF_API_XVMC}
  AV_PIX_FMT_XVMC=$123+$25;
  _AV_PIX_FMT_DELTA6=0;
{$ELSE}
  _AV_PIX_FMT_DELTA6=1;
{$ENDIF}
  AV_PIX_FMT_YUV440P10LE=$123+$26-_AV_PIX_FMT_DELTA6;
  AV_PIX_FMT_YUV440P10BE=$123+$27-_AV_PIX_FMT_DELTA6;
  AV_PIX_FMT_YUV440P12LE=$123+$28-_AV_PIX_FMT_DELTA6;
  AV_PIX_FMT_YUV440P12BE=$123+$29-_AV_PIX_FMT_DELTA6;
  AV_PIX_FMT_AYUV64LE=$123+$2A-_AV_PIX_FMT_DELTA6;
  AV_PIX_FMT_AYUV64BE=$123+$2B-_AV_PIX_FMT_DELTA6;
  AV_PIX_FMT_VIDEOTOOLBOX=$123+$2C-_AV_PIX_FMT_DELTA6;
  AV_PIX_FMT_P010LE=$123+$2D-_AV_PIX_FMT_DELTA6;
  AV_PIX_FMT_P010BE=$123+$2E-_AV_PIX_FMT_DELTA6;
  AV_PIX_FMT_GBRAP12BE=$123+$2F-_AV_PIX_FMT_DELTA6;
  AV_PIX_FMT_GBRAP12LE=$123+$30-_AV_PIX_FMT_DELTA6;
  AV_PIX_FMT_GBRAP10BE=$123+$31-_AV_PIX_FMT_DELTA6;
  AV_PIX_FMT_GBRAP10LE=$123+$32-_AV_PIX_FMT_DELTA6;
  AV_PIX_FMT_MEDIACODEC=$123+$33-_AV_PIX_FMT_DELTA6;
  AV_PIX_FMT_GRAY12BE=$123+$34-_AV_PIX_FMT_DELTA6;
  AV_PIX_FMT_GRAY12LE=$123+$35-_AV_PIX_FMT_DELTA6;
  AV_PIX_FMT_GRAY10BE=$123+$36-_AV_PIX_FMT_DELTA6;
  AV_PIX_FMT_GRAY10LE=$123+$37-_AV_PIX_FMT_DELTA6;
  AV_PIX_FMT_P016LE=$123+$38-_AV_PIX_FMT_DELTA6;
  AV_PIX_FMT_P016BE=$123+$39-_AV_PIX_FMT_DELTA6;


  AV_PIX_FMT_NB=$123+$34-_AV_PIX_FMT_DELTA6;
{$IFEND}

type
(**
 * Pixel format.
 *
 * @note
 * AV_PIX_FMT_RGB32 is handled in an endian-specific manner. An RGBA
 * color is put together as:
 *  (A << 24) | (R << 16) | (G << 8) | B
 * This is stored as BGRA on little-endian CPU architectures and ARGB on
 * big-endian CPUs.
 *
 * @par
 * When the pixel format is palettized RGB32 (AV_PIX_FMT_PAL8), the palettized
 * image data is stored in AVFrame.data[0]. The palette is transported in
 * AVFrame.data[1], is 1024 bytes long (256 4-byte entries) and is
 * formatted the same as in AV_PIX_FMT_RGB32 described above (i.e., it is
 * also endian-specific). Note also that the individual RGB32 palette
 * components stored in AVFrame.data[1] should be in the range 0..255.
 * This is important as many custom PAL8 video codecs that were designed
 * to run on the IBM VGA graphics adapter use 6-bit palette components.
 *
 * @par
 * For all the 8 bits per pixel formats, an RGB32 palette is in data[1] like
 * for pal8. This palette is filled in automatically by the function
 * allocating the picture.
 *)
  PPAVPixelFormat = ^PAVPixelFormat;
  PAVPixelFormat = ^TAVPixelFormat;
{$IF Defined(BCB) and Defined(VER140)} // C++Builder 6
  TAVPixelFormat = Integer;
{$ELSE}
  TAVPixelFormat = (
    AV_PIX_FMT_NONE = -1,
    AV_PIX_FMT_YUV420P,   ///< planar YUV 4:2:0, 12bpp, (1 Cr & Cb sample per 2x2 Y samples)
    AV_PIX_FMT_YUYV422,   ///< packed YUV 4:2:2, 16bpp, Y0 Cb Y1 Cr
    AV_PIX_FMT_RGB24,     ///< packed RGB 8:8:8, 24bpp, RGBRGB...
    AV_PIX_FMT_BGR24,     ///< packed RGB 8:8:8, 24bpp, BGRBGR...
    AV_PIX_FMT_YUV422P,   ///< planar YUV 4:2:2, 16bpp, (1 Cr & Cb sample per 2x1 Y samples)
    AV_PIX_FMT_YUV444P,   ///< planar YUV 4:4:4, 24bpp, (1 Cr & Cb sample per 1x1 Y samples)
    AV_PIX_FMT_YUV410P,   ///< planar YUV 4:1:0,  9bpp, (1 Cr & Cb sample per 4x4 Y samples)
    AV_PIX_FMT_YUV411P,   ///< planar YUV 4:1:1, 12bpp, (1 Cr & Cb sample per 4x1 Y samples)
    AV_PIX_FMT_GRAY8,     ///<        Y        ,  8bpp
    AV_PIX_FMT_MONOWHITE, ///<        Y        ,  1bpp, 0 is white, 1 is black, in each byte pixels are ordered from the msb to the lsb
    AV_PIX_FMT_MONOBLACK, ///<        Y        ,  1bpp, 0 is black, 1 is white, in each byte pixels are ordered from the msb to the lsb
    AV_PIX_FMT_PAL8,      ///< 8 bits with AV_PIX_FMT_RGB32 palette
    AV_PIX_FMT_YUVJ420P,  ///< planar YUV 4:2:0, 12bpp, full scale (JPEG), deprecated in favor of AV_PIX_FMT_YUV420P and setting color_range
    AV_PIX_FMT_YUVJ422P,  ///< planar YUV 4:2:2, 16bpp, full scale (JPEG), deprecated in favor of AV_PIX_FMT_YUV422P and setting color_range
    AV_PIX_FMT_YUVJ444P,  ///< planar YUV 4:4:4, 24bpp, full scale (JPEG), deprecated in favor of AV_PIX_FMT_YUV444P and setting color_range
{$IFDEF FF_API_XVMC}
    AV_PIX_FMT_XVMC_MPEG2_MC,///< XVideo Motion Acceleration via common packet passing
    AV_PIX_FMT_XVMC_MPEG2_IDCT,
//    AV_PIX_FMT_XVMC = AV_PIX_FMT_XVMC_MPEG2_IDCT,
{$ENDIF}
    AV_PIX_FMT_UYVY422,   ///< packed YUV 4:2:2, 16bpp, Cb Y0 Cr Y1
    AV_PIX_FMT_UYYVYY411, ///< packed YUV 4:1:1, 12bpp, Cb Y0 Y1 Cr Y2 Y3
    AV_PIX_FMT_BGR8,      ///< packed RGB 3:3:2,  8bpp, (msb)2B 3G 3R(lsb)
    AV_PIX_FMT_BGR4,      ///< packed RGB 1:2:1 bitstream,  4bpp, (msb)1B 2G 1R(lsb), a byte contains two pixels, the first pixel in the byte is the one composed by the 4 msb bits
    AV_PIX_FMT_BGR4_BYTE, ///< packed RGB 1:2:1,  8bpp, (msb)1B 2G 1R(lsb)
    AV_PIX_FMT_RGB8,      ///< packed RGB 3:3:2,  8bpp, (msb)2R 3G 3B(lsb)
    AV_PIX_FMT_RGB4,      ///< packed RGB 1:2:1 bitstream,  4bpp, (msb)1R 2G 1B(lsb), a byte contains two pixels, the first pixel in the byte is the one composed by the 4 msb bits
    AV_PIX_FMT_RGB4_BYTE, ///< packed RGB 1:2:1,  8bpp, (msb)1R 2G 1B(lsb)
    AV_PIX_FMT_NV12,      ///< planar YUV 4:2:0, 12bpp, 1 plane for Y and 1 plane for the UV components, which are interleaved (first byte U and the following byte V)
    AV_PIX_FMT_NV21,      ///< as above, but U and V bytes are swapped

    AV_PIX_FMT_ARGB,      ///< packed ARGB 8:8:8:8, 32bpp, ARGBARGB...
    AV_PIX_FMT_RGBA,      ///< packed RGBA 8:8:8:8, 32bpp, RGBARGBA...
    AV_PIX_FMT_ABGR,      ///< packed ABGR 8:8:8:8, 32bpp, ABGRABGR...
    AV_PIX_FMT_BGRA,      ///< packed BGRA 8:8:8:8, 32bpp, BGRABGRA...

    AV_PIX_FMT_GRAY16BE,  ///<        Y        , 16bpp, big-endian
    AV_PIX_FMT_GRAY16LE,  ///<        Y        , 16bpp, little-endian
    AV_PIX_FMT_YUV440P,   ///< planar YUV 4:4:0 (1 Cr & Cb sample per 1x2 Y samples)
    AV_PIX_FMT_YUVJ440P,  ///< planar YUV 4:4:0 full scale (JPEG), deprecated in favor of AV_PIX_FMT_YUV440P and setting color_range
    AV_PIX_FMT_YUVA420P,  ///< planar YUV 4:2:0, 20bpp, (1 Cr & Cb sample per 2x2 Y & A samples)
{$IFDEF FF_API_VDPAU}
    AV_PIX_FMT_VDPAU_H264,///< H.264 HW decoding with VDPAU, data[0] contains a vdpau_render_state struct which contains the bitstream of the slices as well as various fields extracted from headers
    AV_PIX_FMT_VDPAU_MPEG1,///< MPEG-1 HW decoding with VDPAU, data[0] contains a vdpau_render_state struct which contains the bitstream of the slices as well as various fields extracted from headers
    AV_PIX_FMT_VDPAU_MPEG2,///< MPEG-2 HW decoding with VDPAU, data[0] contains a vdpau_render_state struct which contains the bitstream of the slices as well as various fields extracted from headers
    AV_PIX_FMT_VDPAU_WMV3,///< WMV3 HW decoding with VDPAU, data[0] contains a vdpau_render_state struct which contains the bitstream of the slices as well as various fields extracted from headers
    AV_PIX_FMT_VDPAU_VC1, ///< VC-1 HW decoding with VDPAU, data[0] contains a vdpau_render_state struct which contains the bitstream of the slices as well as various fields extracted from headers
{$ENDIF}
    AV_PIX_FMT_RGB48BE,   ///< packed RGB 16:16:16, 48bpp, 16R, 16G, 16B, the 2-byte value for each R/G/B component is stored as big-endian
    AV_PIX_FMT_RGB48LE,   ///< packed RGB 16:16:16, 48bpp, 16R, 16G, 16B, the 2-byte value for each R/G/B component is stored as little-endian

    AV_PIX_FMT_RGB565BE,  ///< packed RGB 5:6:5, 16bpp, (msb)   5R 6G 5B(lsb), big-endian
    AV_PIX_FMT_RGB565LE,  ///< packed RGB 5:6:5, 16bpp, (msb)   5R 6G 5B(lsb), little-endian
    AV_PIX_FMT_RGB555BE,  ///< packed RGB 5:5:5, 16bpp, (msb)1X 5R 5G 5B(lsb), big-endian   , X=unused/undefined
    AV_PIX_FMT_RGB555LE,  ///< packed RGB 5:5:5, 16bpp, (msb)1X 5R 5G 5B(lsb), little-endian, X=unused/undefined

    AV_PIX_FMT_BGR565BE,  ///< packed BGR 5:6:5, 16bpp, (msb)   5B 6G 5R(lsb), big-endian
    AV_PIX_FMT_BGR565LE,  ///< packed BGR 5:6:5, 16bpp, (msb)   5B 6G 5R(lsb), little-endian
    AV_PIX_FMT_BGR555BE,  ///< packed BGR 5:5:5, 16bpp, (msb)1X 5B 5G 5R(lsb), big-endian   , X=unused/undefined
    AV_PIX_FMT_BGR555LE,  ///< packed BGR 5:5:5, 16bpp, (msb)1X 5B 5G 5R(lsb), little-endian, X=unused/undefined

{$IFDEF FF_API_VAAPI}
    (** @name Deprecated pixel formats *)
    (**@{*)
    AV_PIX_FMT_VAAPI_MOCO, ///< HW acceleration through VA API at motion compensation entry-point, Picture.data[3] contains a vaapi_render_state struct which contains macroblocks as well as various fields extracted from headers
    AV_PIX_FMT_VAAPI_IDCT, ///< HW acceleration through VA API at IDCT entry-point, Picture.data[3] contains a vaapi_render_state struct which contains fields extracted from headers
    AV_PIX_FMT_VAAPI_VLD,  ///< HW decoding through VA API, Picture.data[3] contains a VASurfaceID
    (**@}*)
    AV_PIX_FMT_VAAPI = AV_PIX_FMT_VAAPI_VLD,
{$ELSE}
    (**
     *  Hardware acceleration through VA-API, data[3] contains a
     *  VASurfaceID.
     *)
    AV_PIX_FMT_VAAPI,
{$ENDIF}

    AV_PIX_FMT_YUV420P16LE,  ///< planar YUV 4:2:0, 24bpp, (1 Cr & Cb sample per 2x2 Y samples), little-endian
    AV_PIX_FMT_YUV420P16BE,  ///< planar YUV 4:2:0, 24bpp, (1 Cr & Cb sample per 2x2 Y samples), big-endian
    AV_PIX_FMT_YUV422P16LE,  ///< planar YUV 4:2:2, 32bpp, (1 Cr & Cb sample per 2x1 Y samples), little-endian
    AV_PIX_FMT_YUV422P16BE,  ///< planar YUV 4:2:2, 32bpp, (1 Cr & Cb sample per 2x1 Y samples), big-endian
    AV_PIX_FMT_YUV444P16LE,  ///< planar YUV 4:4:4, 48bpp, (1 Cr & Cb sample per 1x1 Y samples), little-endian
    AV_PIX_FMT_YUV444P16BE,  ///< planar YUV 4:4:4, 48bpp, (1 Cr & Cb sample per 1x1 Y samples), big-endian
{$IFDEF FF_API_VDPAU}
    AV_PIX_FMT_VDPAU_MPEG4,  ///< MPEG-4 HW decoding with VDPAU, data[0] contains a vdpau_render_state struct which contains the bitstream of the slices as well as various fields extracted from headers
{$ENDIF}
    AV_PIX_FMT_DXVA2_VLD,    ///< HW decoding through DXVA2, Picture.data[3] contains a LPDIRECT3DSURFACE9 pointer

    AV_PIX_FMT_RGB444LE,  ///< packed RGB 4:4:4, 16bpp, (msb)4X 4R 4G 4B(lsb), little-endian, X=unused/undefined
    AV_PIX_FMT_RGB444BE,  ///< packed RGB 4:4:4, 16bpp, (msb)4X 4R 4G 4B(lsb), big-endian,    X=unused/undefined
    AV_PIX_FMT_BGR444LE,  ///< packed BGR 4:4:4, 16bpp, (msb)4X 4B 4G 4R(lsb), little-endian, X=unused/undefined
    AV_PIX_FMT_BGR444BE,  ///< packed BGR 4:4:4, 16bpp, (msb)4X 4B 4G 4R(lsb), big-endian,    X=unused/undefined
    AV_PIX_FMT_YA8,       ///< 8 bits gray, 8 bits alpha

    AV_PIX_FMT_BGR48BE,   ///< packed RGB 16:16:16, 48bpp, 16B, 16G, 16R, the 2-byte value for each R/G/B component is stored as big-endian
    AV_PIX_FMT_BGR48LE,   ///< packed RGB 16:16:16, 48bpp, 16B, 16G, 16R, the 2-byte value for each R/G/B component is stored as little-endian

    (**
     * The following 12 formats have the disadvantage of needing 1 format for each bit depth.
     * Notice that each 9/10 bits sample is stored in 16 bits with extra padding.
     * If you want to support multiple bit depths, then using AV_PIX_FMT_YUV420P16* with the bpp stored separately is better.
     *)
    AV_PIX_FMT_YUV420P9BE, ///< planar YUV 4:2:0, 13.5bpp, (1 Cr & Cb sample per 2x2 Y samples), big-endian
    AV_PIX_FMT_YUV420P9LE, ///< planar YUV 4:2:0, 13.5bpp, (1 Cr & Cb sample per 2x2 Y samples), little-endian
    AV_PIX_FMT_YUV420P10BE,///< planar YUV 4:2:0, 15bpp, (1 Cr & Cb sample per 2x2 Y samples), big-endian
    AV_PIX_FMT_YUV420P10LE,///< planar YUV 4:2:0, 15bpp, (1 Cr & Cb sample per 2x2 Y samples), little-endian
    AV_PIX_FMT_YUV422P10BE,///< planar YUV 4:2:2, 20bpp, (1 Cr & Cb sample per 2x1 Y samples), big-endian
    AV_PIX_FMT_YUV422P10LE,///< planar YUV 4:2:2, 20bpp, (1 Cr & Cb sample per 2x1 Y samples), little-endian
    AV_PIX_FMT_YUV444P9BE, ///< planar YUV 4:4:4, 27bpp, (1 Cr & Cb sample per 1x1 Y samples), big-endian
    AV_PIX_FMT_YUV444P9LE, ///< planar YUV 4:4:4, 27bpp, (1 Cr & Cb sample per 1x1 Y samples), little-endian
    AV_PIX_FMT_YUV444P10BE,///< planar YUV 4:4:4, 30bpp, (1 Cr & Cb sample per 1x1 Y samples), big-endian
    AV_PIX_FMT_YUV444P10LE,///< planar YUV 4:4:4, 30bpp, (1 Cr & Cb sample per 1x1 Y samples), little-endian
    AV_PIX_FMT_YUV422P9BE, ///< planar YUV 4:2:2, 18bpp, (1 Cr & Cb sample per 2x1 Y samples), big-endian
    AV_PIX_FMT_YUV422P9LE, ///< planar YUV 4:2:2, 18bpp, (1 Cr & Cb sample per 2x1 Y samples), little-endian
    AV_PIX_FMT_VDA_VLD,    ///< hardware decoding through VDA

    AV_PIX_FMT_GBRP,      ///< planar GBR 4:4:4 24bpp
//    AV_PIX_FMT_GBR24P = AV_PIX_FMT_GBRP, // alias for #AV_PIX_FMT_GBRP
    AV_PIX_FMT_GBRP9BE,   ///< planar GBR 4:4:4 27bpp, big-endian
    AV_PIX_FMT_GBRP9LE,   ///< planar GBR 4:4:4 27bpp, little-endian
    AV_PIX_FMT_GBRP10BE,  ///< planar GBR 4:4:4 30bpp, big-endian
    AV_PIX_FMT_GBRP10LE,  ///< planar GBR 4:4:4 30bpp, little-endian
    AV_PIX_FMT_GBRP16BE,  ///< planar GBR 4:4:4 48bpp, big-endian
    AV_PIX_FMT_GBRP16LE,  ///< planar GBR 4:4:4 48bpp, little-endian
    AV_PIX_FMT_YUVA422P,  ///< planar YUV 4:2:2 24bpp, (1 Cr & Cb sample per 2x1 Y & A samples)
    AV_PIX_FMT_YUVA444P,  ///< planar YUV 4:4:4 32bpp, (1 Cr & Cb sample per 1x1 Y & A samples)
    AV_PIX_FMT_YUVA420P9BE,  ///< planar YUV 4:2:0 22.5bpp, (1 Cr & Cb sample per 2x2 Y & A samples), big-endian
    AV_PIX_FMT_YUVA420P9LE,  ///< planar YUV 4:2:0 22.5bpp, (1 Cr & Cb sample per 2x2 Y & A samples), little-endian
    AV_PIX_FMT_YUVA422P9BE,  ///< planar YUV 4:2:2 27bpp, (1 Cr & Cb sample per 2x1 Y & A samples), big-endian
    AV_PIX_FMT_YUVA422P9LE,  ///< planar YUV 4:2:2 27bpp, (1 Cr & Cb sample per 2x1 Y & A samples), little-endian
    AV_PIX_FMT_YUVA444P9BE,  ///< planar YUV 4:4:4 36bpp, (1 Cr & Cb sample per 1x1 Y & A samples), big-endian
    AV_PIX_FMT_YUVA444P9LE,  ///< planar YUV 4:4:4 36bpp, (1 Cr & Cb sample per 1x1 Y & A samples), little-endian
    AV_PIX_FMT_YUVA420P10BE, ///< planar YUV 4:2:0 25bpp, (1 Cr & Cb sample per 2x2 Y & A samples, big-endian)
    AV_PIX_FMT_YUVA420P10LE, ///< planar YUV 4:2:0 25bpp, (1 Cr & Cb sample per 2x2 Y & A samples, little-endian)
    AV_PIX_FMT_YUVA422P10BE, ///< planar YUV 4:2:2 30bpp, (1 Cr & Cb sample per 2x1 Y & A samples, big-endian)
    AV_PIX_FMT_YUVA422P10LE, ///< planar YUV 4:2:2 30bpp, (1 Cr & Cb sample per 2x1 Y & A samples, little-endian)
    AV_PIX_FMT_YUVA444P10BE, ///< planar YUV 4:4:4 40bpp, (1 Cr & Cb sample per 1x1 Y & A samples, big-endian)
    AV_PIX_FMT_YUVA444P10LE, ///< planar YUV 4:4:4 40bpp, (1 Cr & Cb sample per 1x1 Y & A samples, little-endian)
    AV_PIX_FMT_YUVA420P16BE, ///< planar YUV 4:2:0 40bpp, (1 Cr & Cb sample per 2x2 Y & A samples, big-endian)
    AV_PIX_FMT_YUVA420P16LE, ///< planar YUV 4:2:0 40bpp, (1 Cr & Cb sample per 2x2 Y & A samples, little-endian)
    AV_PIX_FMT_YUVA422P16BE, ///< planar YUV 4:2:2 48bpp, (1 Cr & Cb sample per 2x1 Y & A samples, big-endian)
    AV_PIX_FMT_YUVA422P16LE, ///< planar YUV 4:2:2 48bpp, (1 Cr & Cb sample per 2x1 Y & A samples, little-endian)
    AV_PIX_FMT_YUVA444P16BE, ///< planar YUV 4:4:4 64bpp, (1 Cr & Cb sample per 1x1 Y & A samples, big-endian)
    AV_PIX_FMT_YUVA444P16LE, ///< planar YUV 4:4:4 64bpp, (1 Cr & Cb sample per 1x1 Y & A samples, little-endian)

    AV_PIX_FMT_VDPAU,     ///< HW acceleration through VDPAU, Picture.data[3] contains a VdpVideoSurface

    AV_PIX_FMT_XYZ12LE,      ///< packed XYZ 4:4:4, 36 bpp, (msb) 12X, 12Y, 12Z (lsb), the 2-byte value for each X/Y/Z is stored as little-endian, the 4 lower bits are set to 0
    AV_PIX_FMT_XYZ12BE,      ///< packed XYZ 4:4:4, 36 bpp, (msb) 12X, 12Y, 12Z (lsb), the 2-byte value for each X/Y/Z is stored as big-endian, the 4 lower bits are set to 0
    AV_PIX_FMT_NV16,         ///< interleaved chroma YUV 4:2:2, 16bpp, (1 Cr & Cb sample per 2x1 Y samples)
    AV_PIX_FMT_NV20LE,       ///< interleaved chroma YUV 4:2:2, 20bpp, (1 Cr & Cb sample per 2x1 Y samples), little-endian
    AV_PIX_FMT_NV20BE,       ///< interleaved chroma YUV 4:2:2, 20bpp, (1 Cr & Cb sample per 2x1 Y samples), big-endian

    (**
     * duplicated pixel formats for compatibility with libav.
     * FFmpeg supports these formats since Sat Sep 24 06:01:45 2011 +0200 (commits 9569a3c9f41387a8c7d1ce97d8693520477a66c3)
     * also see Fri Nov 25 01:38:21 2011 +0100 92afb431621c79155fcb7171d26f137eb1bee028
     * Libav added them Sun Mar 16 23:05:47 2014 +0100 with incompatible values (commit 1481d24c3a0abf81e1d7a514547bd5305232be30)
     *)
    AV_PIX_FMT_RGBA64BE,     ///< packed RGBA 16:16:16:16, 64bpp, 16R, 16G, 16B, 16A, the 2-byte value for each R/G/B/A component is stored as big-endian
    AV_PIX_FMT_RGBA64LE,     ///< packed RGBA 16:16:16:16, 64bpp, 16R, 16G, 16B, 16A, the 2-byte value for each R/G/B/A component is stored as little-endian
    AV_PIX_FMT_BGRA64BE,     ///< packed RGBA 16:16:16:16, 64bpp, 16B, 16G, 16R, 16A, the 2-byte value for each R/G/B/A component is stored as big-endian
    AV_PIX_FMT_BGRA64LE,     ///< packed RGBA 16:16:16:16, 64bpp, 16B, 16G, 16R, 16A, the 2-byte value for each R/G/B/A component is stored as little-endian

    AV_PIX_FMT_YVYU422,   ///< packed YUV 4:2:2, 16bpp, Y0 Cr Y1 Cb

    AV_PIX_FMT_VDA,          ///< HW acceleration through VDA, data[3] contains a CVPixelBufferRef

    AV_PIX_FMT_YA16BE,       ///< 16 bits gray, 16 bits alpha (big-endian)
    AV_PIX_FMT_YA16LE,       ///< 16 bits gray, 16 bits alpha (little-endian)

    AV_PIX_FMT_GBRAP,        ///< planar GBRA 4:4:4:4 32bpp
    AV_PIX_FMT_GBRAP16BE,    ///< planar GBRA 4:4:4:4 64bpp, big-endian
    AV_PIX_FMT_GBRAP16LE,    ///< planar GBRA 4:4:4:4 64bpp, little-endian
    (**
     *  HW acceleration through QSV, data[3] contains a pointer to the
     *  mfxFrameSurface1 structure.
     *)
    AV_PIX_FMT_QSV,
    (**
     * HW acceleration though MMAL, data[3] contains a pointer to the
     * MMAL_BUFFER_HEADER_T structure.
     *)
    AV_PIX_FMT_MMAL,

    AV_PIX_FMT_D3D11VA_VLD,  ///< HW decoding through Direct3D11, Picture.data[3] contains a ID3D11VideoDecoderOutputView pointer

    (**
     * HW acceleration through CUDA. data[i] contain CUdeviceptr pointers
     * exactly as for system memory frames.
     *)
    AV_PIX_FMT_CUDA,

    AV_PIX_FMT_0RGB=$123+4, ///< packed RGB 8:8:8, 32bpp, XRGBXRGB...   X=unused/undefined
    AV_PIX_FMT_RGB0,        ///< packed RGB 8:8:8, 32bpp, RGBXRGBX...   X=unused/undefined
    AV_PIX_FMT_0BGR,        ///< packed BGR 8:8:8, 32bpp, XBGRXBGR...   X=unused/undefined
    AV_PIX_FMT_BGR0,        ///< packed BGR 8:8:8, 32bpp, BGRXBGRX...   X=unused/undefined

    AV_PIX_FMT_YUV420P12BE, ///< planar YUV 4:2:0,18bpp, (1 Cr & Cb sample per 2x2 Y samples), big-endian
    AV_PIX_FMT_YUV420P12LE, ///< planar YUV 4:2:0,18bpp, (1 Cr & Cb sample per 2x2 Y samples), little-endian
    AV_PIX_FMT_YUV420P14BE, ///< planar YUV 4:2:0,21bpp, (1 Cr & Cb sample per 2x2 Y samples), big-endian
    AV_PIX_FMT_YUV420P14LE, ///< planar YUV 4:2:0,21bpp, (1 Cr & Cb sample per 2x2 Y samples), little-endian
    AV_PIX_FMT_YUV422P12BE, ///< planar YUV 4:2:2,24bpp, (1 Cr & Cb sample per 2x1 Y samples), big-endian
    AV_PIX_FMT_YUV422P12LE, ///< planar YUV 4:2:2,24bpp, (1 Cr & Cb sample per 2x1 Y samples), little-endian
    AV_PIX_FMT_YUV422P14BE, ///< planar YUV 4:2:2,28bpp, (1 Cr & Cb sample per 2x1 Y samples), big-endian
    AV_PIX_FMT_YUV422P14LE, ///< planar YUV 4:2:2,28bpp, (1 Cr & Cb sample per 2x1 Y samples), little-endian
    AV_PIX_FMT_YUV444P12BE, ///< planar YUV 4:4:4,36bpp, (1 Cr & Cb sample per 1x1 Y samples), big-endian
    AV_PIX_FMT_YUV444P12LE, ///< planar YUV 4:4:4,36bpp, (1 Cr & Cb sample per 1x1 Y samples), little-endian
    AV_PIX_FMT_YUV444P14BE, ///< planar YUV 4:4:4,42bpp, (1 Cr & Cb sample per 1x1 Y samples), big-endian
    AV_PIX_FMT_YUV444P14LE, ///< planar YUV 4:4:4,42bpp, (1 Cr & Cb sample per 1x1 Y samples), little-endian
    AV_PIX_FMT_GBRP12BE,    ///< planar GBR 4:4:4 36bpp, big-endian
    AV_PIX_FMT_GBRP12LE,    ///< planar GBR 4:4:4 36bpp, little-endian
    AV_PIX_FMT_GBRP14BE,    ///< planar GBR 4:4:4 42bpp, big-endian
    AV_PIX_FMT_GBRP14LE,    ///< planar GBR 4:4:4 42bpp, little-endian
    AV_PIX_FMT_YUVJ411P,    ///< planar YUV 4:1:1, 12bpp, (1 Cr & Cb sample per 4x1 Y samples) full scale (JPEG), deprecated in favor of AV_PIX_FMT_YUV411P and setting color_range

    AV_PIX_FMT_BAYER_BGGR8,    ///< bayer, BGBG..(odd line), GRGR..(even line), 8-bit samples */
    AV_PIX_FMT_BAYER_RGGB8,    ///< bayer, RGRG..(odd line), GBGB..(even line), 8-bit samples */
    AV_PIX_FMT_BAYER_GBRG8,    ///< bayer, GBGB..(odd line), RGRG..(even line), 8-bit samples */
    AV_PIX_FMT_BAYER_GRBG8,    ///< bayer, GRGR..(odd line), BGBG..(even line), 8-bit samples */
    AV_PIX_FMT_BAYER_BGGR16LE, ///< bayer, BGBG..(odd line), GRGR..(even line), 16-bit samples, little-endian */
    AV_PIX_FMT_BAYER_BGGR16BE, ///< bayer, BGBG..(odd line), GRGR..(even line), 16-bit samples, big-endian */
    AV_PIX_FMT_BAYER_RGGB16LE, ///< bayer, RGRG..(odd line), GBGB..(even line), 16-bit samples, little-endian */
    AV_PIX_FMT_BAYER_RGGB16BE, ///< bayer, RGRG..(odd line), GBGB..(even line), 16-bit samples, big-endian */
    AV_PIX_FMT_BAYER_GBRG16LE, ///< bayer, GBGB..(odd line), RGRG..(even line), 16-bit samples, little-endian */
    AV_PIX_FMT_BAYER_GBRG16BE, ///< bayer, GBGB..(odd line), RGRG..(even line), 16-bit samples, big-endian */
    AV_PIX_FMT_BAYER_GRBG16LE, ///< bayer, GRGR..(odd line), BGBG..(even line), 16-bit samples, little-endian */
    AV_PIX_FMT_BAYER_GRBG16BE, ///< bayer, GRGR..(odd line), BGBG..(even line), 16-bit samples, big-endian */
{$IFNDEF FF_API_XVMC}
    AV_PIX_FMT_XVMC,///< XVideo Motion Acceleration via common packet passing
{$ENDIF}
    AV_PIX_FMT_YUV440P10LE, ///< planar YUV 4:4:0,20bpp, (1 Cr & Cb sample per 1x2 Y samples), little-endian
    AV_PIX_FMT_YUV440P10BE, ///< planar YUV 4:4:0,20bpp, (1 Cr & Cb sample per 1x2 Y samples), big-endian
    AV_PIX_FMT_YUV440P12LE, ///< planar YUV 4:4:0,24bpp, (1 Cr & Cb sample per 1x2 Y samples), little-endian
    AV_PIX_FMT_YUV440P12BE, ///< planar YUV 4:4:0,24bpp, (1 Cr & Cb sample per 1x2 Y samples), big-endian
    AV_PIX_FMT_AYUV64LE,    ///< packed AYUV 4:4:4,64bpp (1 Cr & Cb sample per 1x1 Y & A samples), little-endian
    AV_PIX_FMT_AYUV64BE,    ///< packed AYUV 4:4:4,64bpp (1 Cr & Cb sample per 1x1 Y & A samples), big-endian

    AV_PIX_FMT_VIDEOTOOLBOX, ///< hardware decoding through Videotoolbox

    AV_PIX_FMT_P010LE, ///< like NV12, with 10bpp per component, data in the high bits, zeros in the low bits, little-endian
    AV_PIX_FMT_P010BE, ///< like NV12, with 10bpp per component, data in the high bits, zeros in the low bits, big-endian

    AV_PIX_FMT_GBRAP12BE,  ///< planar GBR 4:4:4:4 48bpp, big-endian
    AV_PIX_FMT_GBRAP12LE,  ///< planar GBR 4:4:4:4 48bpp, little-endian

    AV_PIX_FMT_GBRAP10BE,  ///< planar GBR 4:4:4:4 40bpp, big-endian
    AV_PIX_FMT_GBRAP10LE,  ///< planar GBR 4:4:4:4 40bpp, little-endian

    AV_PIX_FMT_MEDIACODEC, ///< hardware decoding through MediaCodec

    AV_PIX_FMT_GRAY12BE,   ///<        Y        , 12bpp, big-endian
    AV_PIX_FMT_GRAY12LE,   ///<        Y        , 12bpp, little-endian
    AV_PIX_FMT_GRAY10BE,   ///<        Y        , 10bpp, big-endian
    AV_PIX_FMT_GRAY10LE,   ///<        Y        , 10bpp, little-endian

    AV_PIX_FMT_P016LE, ///< like NV12, with 16bpp per component, little-endian
    AV_PIX_FMT_P016BE, ///< like NV12, with 16bpp per component, big-endian

    AV_PIX_FMT_NB         ///< number of pixel formats, DO NOT USE THIS if you want to link with shared libav* because the number of formats might differ between versions
  );
{$IFEND}

const
{$IFDEF FF_API_XVMC}
  AV_PIX_FMT_XVMC = AV_PIX_FMT_XVMC_MPEG2_IDCT;
{$ENDIF}

//  AV_PIX_FMT_Y400A   = AV_PIX_FMT_GRAY8A;
  AV_PIX_FMT_GBR24P  = AV_PIX_FMT_GBRP;
  AV_PIX_FMT_Y400A = AV_PIX_FMT_YA8; ///< alias for AV_PIX_FMT_YA8
  AV_PIX_FMT_GRAY8A= AV_PIX_FMT_YA8; ///< alias for AV_PIX_FMT_YA8

{
#if AV_HAVE_BIGENDIAN
#   define AV_PIX_FMT_NE(be, le) AV_PIX_FMT_##be
#else
#   define AV_PIX_FMT_NE(be, le) AV_PIX_FMT_##le
#endif
}
  AV_PIX_FMT_RGB32   = AV_PIX_FMT_BGRA;
  AV_PIX_FMT_RGB32_1 = AV_PIX_FMT_ABGR;
  AV_PIX_FMT_BGR32   = AV_PIX_FMT_RGBA;
  AV_PIX_FMT_BGR32_1 = AV_PIX_FMT_ARGB;
  AV_PIX_FMT_0RGB32  = AV_PIX_FMT_BGR0;
  AV_PIX_FMT_0BGR32  = AV_PIX_FMT_RGB0;

  AV_PIX_FMT_GRAY10 = AV_PIX_FMT_GRAY10LE;
  AV_PIX_FMT_GRAY12 = AV_PIX_FMT_GRAY12LE;
  AV_PIX_FMT_GRAY16 = AV_PIX_FMT_GRAY16LE;
  AV_PIX_FMT_YA16   = AV_PIX_FMT_YA16LE;
  AV_PIX_FMT_RGB48  = AV_PIX_FMT_RGB48LE;
  AV_PIX_FMT_RGB565 = AV_PIX_FMT_RGB565LE;
  AV_PIX_FMT_RGB555 = AV_PIX_FMT_RGB555LE;
  AV_PIX_FMT_RGB444 = AV_PIX_FMT_RGB444LE;
  AV_PIX_FMT_RGBA64 = AV_PIX_FMT_RGBA64LE;
  AV_PIX_FMT_BGR48  = AV_PIX_FMT_BGR48LE;
  AV_PIX_FMT_BGR565 = AV_PIX_FMT_BGR565LE;
  AV_PIX_FMT_BGR555 = AV_PIX_FMT_BGR555LE;
  AV_PIX_FMT_BGR444 = AV_PIX_FMT_BGR444LE;
  AV_PIX_FMT_BGRA64 = AV_PIX_FMT_BGRA64LE;

  AV_PIX_FMT_YUV420P9  = AV_PIX_FMT_YUV420P9LE;
  AV_PIX_FMT_YUV422P9  = AV_PIX_FMT_YUV422P9LE;
  AV_PIX_FMT_YUV444P9  = AV_PIX_FMT_YUV444P9LE;
  AV_PIX_FMT_YUV420P10 = AV_PIX_FMT_YUV420P10LE;
  AV_PIX_FMT_YUV422P10 = AV_PIX_FMT_YUV422P10LE;
  AV_PIX_FMT_YUV440P10 = AV_PIX_FMT_YUV440P10LE;
  AV_PIX_FMT_YUV444P10 = AV_PIX_FMT_YUV444P10LE;
  AV_PIX_FMT_YUV420P12 = AV_PIX_FMT_YUV420P12LE;
  AV_PIX_FMT_YUV422P12 = AV_PIX_FMT_YUV422P12LE;
  AV_PIX_FMT_YUV440P12 = AV_PIX_FMT_YUV440P12LE;
  AV_PIX_FMT_YUV444P12 = AV_PIX_FMT_YUV444P12LE;
  AV_PIX_FMT_YUV420P14 = AV_PIX_FMT_YUV420P14LE;
  AV_PIX_FMT_YUV422P14 = AV_PIX_FMT_YUV422P14LE;
  AV_PIX_FMT_YUV444P14 = AV_PIX_FMT_YUV444P14LE;
  AV_PIX_FMT_YUV420P16 = AV_PIX_FMT_YUV420P16LE;
  AV_PIX_FMT_YUV422P16 = AV_PIX_FMT_YUV422P16LE;
  AV_PIX_FMT_YUV444P16 = AV_PIX_FMT_YUV444P16LE;

  AV_PIX_FMT_GBRP9  = AV_PIX_FMT_GBRP9LE;
  AV_PIX_FMT_GBRP10 = AV_PIX_FMT_GBRP10LE;
  AV_PIX_FMT_GBRP12 = AV_PIX_FMT_GBRP12LE;
  AV_PIX_FMT_GBRP14 = AV_PIX_FMT_GBRP14LE;
  AV_PIX_FMT_GBRP16 = AV_PIX_FMT_GBRP16LE;
  AV_PIX_FMT_GBRAP10 = AV_PIX_FMT_GBRAP10LE;
  AV_PIX_FMT_GBRAP12 = AV_PIX_FMT_GBRAP12LE;
  AV_PIX_FMT_GBRAP16 = AV_PIX_FMT_GBRAP16LE;

  AV_PIX_FMT_BAYER_BGGR16 = AV_PIX_FMT_BAYER_BGGR16LE;
  AV_PIX_FMT_BAYER_RGGB16 = AV_PIX_FMT_BAYER_RGGB16LE;
  AV_PIX_FMT_BAYER_GBRG16 = AV_PIX_FMT_BAYER_GBRG16LE;
  AV_PIX_FMT_BAYER_GRBG16 = AV_PIX_FMT_BAYER_GRBG16LE;


  AV_PIX_FMT_YUVA420P9  = AV_PIX_FMT_YUVA420P9LE;
  AV_PIX_FMT_YUVA422P9  = AV_PIX_FMT_YUVA422P9LE;
  AV_PIX_FMT_YUVA444P9  = AV_PIX_FMT_YUVA444P9LE;
  AV_PIX_FMT_YUVA420P10 = AV_PIX_FMT_YUVA420P10LE;
  AV_PIX_FMT_YUVA422P10 = AV_PIX_FMT_YUVA422P10LE;
  AV_PIX_FMT_YUVA444P10 = AV_PIX_FMT_YUVA444P10LE;
  AV_PIX_FMT_YUVA420P16 = AV_PIX_FMT_YUVA420P16LE;
  AV_PIX_FMT_YUVA422P16 = AV_PIX_FMT_YUVA422P16LE;
  AV_PIX_FMT_YUVA444P16 = AV_PIX_FMT_YUVA444P16LE;

  AV_PIX_FMT_XYZ12      = AV_PIX_FMT_XYZ12LE;
  AV_PIX_FMT_NV20       = AV_PIX_FMT_NV20LE;
  AV_PIX_FMT_AYUV64     = AV_PIX_FMT_AYUV64LE;
  AV_PIX_FMT_P010       = AV_PIX_FMT_P010LE;
  AV_PIX_FMT_P016       = AV_PIX_FMT_P016LE;

{$IF Defined(BCB) and Defined(VER140)} // C++Builder 6
  AVCOL_PRI_RESERVED0   = 0;
  AVCOL_PRI_BT709       = 1;
  AVCOL_PRI_UNSPECIFIED = 2;
  AVCOL_PRI_RESERVED    = 3;
  AVCOL_PRI_BT470M      = 4;
  AVCOL_PRI_BT470BG     = 5;
  AVCOL_PRI_SMPTE170M   = 6;
  AVCOL_PRI_SMPTE240M   = 7;
  AVCOL_PRI_FILM        = 8;
  AVCOL_PRI_BT2020      = 9;
  AVCOL_PRI_SMPTE428    = 10;
  AVCOL_PRI_SMPTEST428_1= AVCOL_PRI_SMPTE428;
  AVCOL_PRI_SMPTE431    = 11;
  AVCOL_PRI_SMPTE432    = 12;
  AVCOL_PRI_JEDEC_P22   = 22;
  AVCOL_PRI_NB          = 23;

  AVCOL_TRC_RESERVED0    = 0;
  AVCOL_TRC_BT709        = 1;
  AVCOL_TRC_UNSPECIFIED  = 2;
  AVCOL_TRC_RESERVED     = 3;
  AVCOL_TRC_GAMMA22      = 4;
  AVCOL_TRC_GAMMA28      = 5;
  AVCOL_TRC_SMPTE170M    = 6;
  AVCOL_TRC_SMPTE240M    = 7;
  AVCOL_TRC_LINEAR       = 8;
  AVCOL_TRC_LOG          = 9;
  AVCOL_TRC_LOG_SQRT     = 10;
  AVCOL_TRC_IEC61966_2_4 = 11;
  AVCOL_TRC_BT1361_ECG   = 12;
  AVCOL_TRC_IEC61966_2_1 = 13;
  AVCOL_TRC_BT2020_10    = 14;
  AVCOL_TRC_BT2020_12    = 15;
  AVCOL_TRC_SMPTE2084    = 16;
  AVCOL_TRC_SMPTEST2084  = AVCOL_TRC_SMPTE2084;
  AVCOL_TRC_SMPTE428     = 17;
  AVCOL_TRC_SMPTEST428_1 = AVCOL_TRC_SMPTE428;
  AVCOL_TRC_ARIB_STD_B67 = 18;
  AVCOL_TRC_NB           = 19;

  AVCOL_SPC_RGB         = 0;
  AVCOL_SPC_BT709       = 1;
  AVCOL_SPC_UNSPECIFIED = 2;
  AVCOL_SPC_RESERVED    = 3;
  AVCOL_SPC_FCC         = 4;
  AVCOL_SPC_BT470BG     = 5;
  AVCOL_SPC_SMPTE170M   = 6;
  AVCOL_SPC_SMPTE240M   = 7;
  AVCOL_SPC_YCGCO       = 8;  ///< Used by Dirac / VC-2 and H.264 FRext, see ITU-T SG16
  AVCOL_SPC_YCOCG       = AVCOL_SPC_YCGCO;
  AVCOL_SPC_BT2020_NCL  = 9;
  AVCOL_SPC_BT2020_CL   = 10;
  AVCOL_SPC_NB          = 11;

  AVCOL_RANGE_UNSPECIFIED = 0;
  AVCOL_RANGE_MPEG        = 1;
  AVCOL_RANGE_JPEG        = 2;
  AVCOL_RANGE_NB          = 3;

  AVCHROMA_LOC_UNSPECIFIED = 0;
  AVCHROMA_LOC_LEFT        = 1;
  AVCHROMA_LOC_CENTER      = 2;
  AVCHROMA_LOC_TOPLEFT     = 3;
  AVCHROMA_LOC_TOP         = 4;
  AVCHROMA_LOC_BOTTOMLEFT  = 5;
  AVCHROMA_LOC_BOTTOM      = 6;
  AVCHROMA_LOC_NB          = 7;
{$IFEND}

type
(**
  * Chromaticity coordinates of the source primaries.
  *)
{$IF Defined(BCB) and Defined(VER140)} // C++Builder 6
  TAVColorPrimaries = Integer;
{$ELSE}
  TAVColorPrimaries = (
    AVCOL_PRI_RESERVED0   = 0,
    AVCOL_PRI_BT709       = 1,  ///< also ITU-R BT1361 / IEC 61966-2-4 / SMPTE RP177 Annex B
    AVCOL_PRI_UNSPECIFIED = 2,
    AVCOL_PRI_RESERVED    = 3,
    AVCOL_PRI_BT470M      = 4,  ///< also FCC Title 47 Code of Federal Regulations 73.682 (a)(20)

    AVCOL_PRI_BT470BG     = 5,  ///< also ITU-R BT601-6 625 / ITU-R BT1358 625 / ITU-R BT1700 625 PAL & SECAM
    AVCOL_PRI_SMPTE170M   = 6,  ///< also ITU-R BT601-6 525 / ITU-R BT1358 525 / ITU-R BT1700 NTSC
    AVCOL_PRI_SMPTE240M   = 7,  ///< functionally identical to above
    AVCOL_PRI_FILM        = 8,  ///< colour filters using Illuminant C
    AVCOL_PRI_BT2020      = 9,  ///< ITU-R BT2020
    AVCOL_PRI_SMPTE428    = 10, ///< SMPTE ST 428-1 (CIE 1931 XYZ)
    AVCOL_PRI_SMPTEST428_1= AVCOL_PRI_SMPTE428,
    AVCOL_PRI_SMPTE431    = 11, ///< SMPTE ST 431-2 (2011) / DCI P3
    AVCOL_PRI_SMPTE432    = 12, ///< SMPTE ST 432-1 (2010) / P3 D65 / Display P3
    AVCOL_PRI_JEDEC_P22   = 22, ///< JEDEC P22 phosphors
    AVCOL_PRI_NB                ///< Not part of ABI
  );
{$IFEND}

(**
 * Color Transfer Characteristic.
 *)
{$IF Defined(BCB) and Defined(VER140)} // C++Builder 6
  TAVColorTransferCharacteristic = Integer;
{$ELSE}
  TAVColorTransferCharacteristic = (
    AVCOL_TRC_RESERVED0    = 0,
    AVCOL_TRC_BT709        = 1,  ///< also ITU-R BT1361
    AVCOL_TRC_UNSPECIFIED  = 2,
    AVCOL_TRC_RESERVED     = 3,
    AVCOL_TRC_GAMMA22      = 4,  ///< also ITU-R BT470M / ITU-R BT1700 625 PAL & SECAM
    AVCOL_TRC_GAMMA28      = 5,  ///< also ITU-R BT470BG
    AVCOL_TRC_SMPTE170M    = 6,  ///< also ITU-R BT601-6 525 or 625 / ITU-R BT1358 525 or 625 / ITU-R BT1700 NTSC
    AVCOL_TRC_SMPTE240M    = 7,
    AVCOL_TRC_LINEAR       = 8,  ///< "Linear transfer characteristics"
    AVCOL_TRC_LOG          = 9,  ///< "Logarithmic transfer characteristic (100:1 range)"
    AVCOL_TRC_LOG_SQRT     = 10, ///< "Logarithmic transfer characteristic (100 * Sqrt( 10 ) : 1 range)"
    AVCOL_TRC_IEC61966_2_4 = 11, ///< IEC 61966-2-4
    AVCOL_TRC_BT1361_ECG   = 12, ///< ITU-R BT1361 Extended Colour Gamut
    AVCOL_TRC_IEC61966_2_1 = 13, ///< IEC 61966-2-1 (sRGB or sYCC)
    AVCOL_TRC_BT2020_10    = 14, ///< ITU-R BT2020 for 10-bit system
    AVCOL_TRC_BT2020_12    = 15, ///< ITU-R BT2020 for 12-bit system
    AVCOL_TRC_SMPTE2084    = 16, ///< SMPTE ST 2084 for 10-, 12-, 14- and 16-bit systems
    AVCOL_TRC_SMPTEST2084  = AVCOL_TRC_SMPTE2084,
    AVCOL_TRC_SMPTE428     = 17, ///< SMPTE ST 428-1
    AVCOL_TRC_SMPTEST428_1 = AVCOL_TRC_SMPTE428,
    AVCOL_TRC_ARIB_STD_B67 = 18, ///< ARIB STD-B67, known as "Hybrid log-gamma"
    AVCOL_TRC_NB                 ///< Not part of ABI
  );
{$IFEND}

type
(**
 * YUV colorspace type.
 *)
{$IF Defined(BCB) and Defined(VER140)} // C++Builder 6
  TAVColorSpace = Integer;
{$ELSE}
  TAVColorSpace = (
    AVCOL_SPC_RGB         = 0,  ///< order of coefficients is actually GBR, also IEC 61966-2-1 (sRGB)
    AVCOL_SPC_BT709       = 1,  ///< also ITU-R BT1361 / IEC 61966-2-4 xvYCC709 / SMPTE RP177 Annex B
    AVCOL_SPC_UNSPECIFIED = 2,
    AVCOL_SPC_RESERVED    = 3,
    AVCOL_SPC_FCC         = 4,  ///< FCC Title 47 Code of Federal Regulations 73.682 (a)(20)
    AVCOL_SPC_BT470BG     = 5,  ///< also ITU-R BT601-6 625 / ITU-R BT1358 625 / ITU-R BT1700 625 PAL & SECAM / IEC 61966-2-4 xvYCC601
    AVCOL_SPC_SMPTE170M   = 6,  ///< also ITU-R BT601-6 525 / ITU-R BT1358 525 / ITU-R BT1700 NTSC
    AVCOL_SPC_SMPTE240M   = 7,  ///< functionally identical to above
    AVCOL_SPC_YCGCO       = 8,  ///< Used by Dirac / VC-2 and H.264 FRext, see ITU-T SG16
    AVCOL_SPC_YCOCG       = AVCOL_SPC_YCGCO,
    AVCOL_SPC_BT2020_NCL  = 9,  ///< ITU-R BT2020 non-constant luminance system
    AVCOL_SPC_BT2020_CL   = 10, ///< ITU-R BT2020 constant luminance system
    AVCOL_SPC_SMPTE2085   = 11, ///< SMPTE 2085, Y'D'zD'x
    AVCOL_SPC_NB                ///< Not part of ABI
  );
{$IFEND}

(**
 * MPEG vs JPEG YUV range.
 *)
{$IF Defined(BCB) and Defined(VER140)} // C++Builder 6
  TAVColorRange = Integer;
{$ELSE}
  TAVColorRange = (
    AVCOL_RANGE_UNSPECIFIED = 0,
    AVCOL_RANGE_MPEG        = 1, ///< the normal 219*2^(n-8) "MPEG" YUV ranges
    AVCOL_RANGE_JPEG        = 2, ///< the normal     2^n-1   "JPEG" YUV ranges
    AVCOL_RANGE_NB               ///< Not part of ABI
  );
{$IFEND}

(**
 * Location of chroma samples.
 *
 * Illustration showing the location of the first (top left) chroma sample of the
 * image, the left shows only luma, the right
 * shows the location of the chroma sample, the 2 could be imagined to overlay
 * each other but are drawn separately due to limitations of ASCII
 *
 *                1st 2nd       1st 2nd horizontal luma sample positions
 *                 v   v         v   v
 *                 ______        ______
 *1st luma line > |X   X ...    |3 4 X ...     X are luma samples,
 *                |             |1 2           1-6 are possible chroma positions
 *2nd luma line > |X   X ...    |5 6 X ...     0 is undefined/unknown position
 *)
{$IF Defined(BCB) and Defined(VER140)} // C++Builder 6
  TAVChromaLocation = Integer;
{$ELSE}
  TAVChromaLocation = (
    AVCHROMA_LOC_UNSPECIFIED = 0,
    AVCHROMA_LOC_LEFT        = 1, ///< MPEG-2/4 4:2:0, H.264 default for 4:2:0
    AVCHROMA_LOC_CENTER      = 2, ///< MPEG-1 4:2:0, JPEG 4:2:0, H.263 4:2:0
    AVCHROMA_LOC_TOPLEFT     = 3, ///< ITU-R 601, SMPTE 274M 296M S314M(DV 4:1:1), mpeg2 4:2:2
    AVCHROMA_LOC_TOP         = 4,
    AVCHROMA_LOC_BOTTOMLEFT  = 5,
    AVCHROMA_LOC_BOTTOM      = 6,
    AVCHROMA_LOC_NB               ///< Not part of ABI
  );
{$IFEND}

implementation

end.
