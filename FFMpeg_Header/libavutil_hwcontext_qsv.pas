(*
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
 * An API-specific header for AV_HWDEVICE_TYPE_QSV.
 *
 * This API does not support dynamic frame pools. AVHWFramesContext.pool must
 * contain AVBufferRefs whose data pointer points to an mfxFrameSurface1 struct.
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: libavutil/hwcontext_qsv.h
 * Ported by CodeCoolie@CNSW 2017/04/23 -> $Date:: 2017-05-30 #$
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

unit libavutil_hwcontext_qsv;

interface

{$I CompilerDefines.inc}

uses
  libmfxsession,
  libmfxstructures;

{$I libversion.inc}

type
(**
 * This struct is allocated as AVHWDeviceContext.hwctx
 *)
  TAVQSVDeviceContext = record
    session: TmfxSession;
  end;

(**
 * This struct is allocated as AVHWFramesContext.hwctx
 *)
  PAVQSVFramesContext = ^TAVQSVFramesContext;
  TAVQSVFramesContext = record
    surfaces: PmfxFrameSurface1;
    nb_surfaces: Integer;

    (**
     * A combination of MFX_MEMTYPE_* describing the frame pool.
     *)
    frame_type: Integer;
  end;

implementation

end.
