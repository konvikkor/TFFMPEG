(*
 * copyright (c) 2001 Fabrice Bellard
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
 * @ingroup libavc
 * Libavcodec external API header
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: libavcodec/avcodec.h
 * Ported by CodeCoolie@CNSW 2008/03/17 -> $Date:: 2017-05-30 #$
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

unit libavcodec;

interface

{$I CompilerDefines.inc}

uses
  FFTypes,
  libavutil,
  libavutil_buffer,
  libavutil_dict,
  libavutil_frame,
  libavutil_log,
  libavutil_pixfmt,
  libavutil_rational,
  libavutil_samplefmt;

{$I libversion.inc}

(**
 * @defgroup libavc libavcodec
 * Encoding/Decoding Library
 *
 * @{
 *
 * @defgroup lavc_decoding Decoding
 * @{
 * @}
 *
 * @defgroup lavc_encoding Encoding
 * @{
 * @}
 *
 * @defgroup lavc_codec Codecs
 * @{
 * @defgroup lavc_codec_native Native Codecs
 * @{
 * @}
 * @defgroup lavc_codec_wrappers External library wrappers
 * @{
 * @}
 * @defgroup lavc_codec_hwaccel Hardware Accelerators bridge
 * @{
 * @}
 * @}
 * @defgroup lavc_internal Internal
 * @{
 * @}
 * @}
 *
 *)

(**
 * @ingroup libavc
 * @defgroup lavc_encdec send/receive encoding and decoding API overview
 * @{
 *
 * The avcodec_send_packet()/avcodec_receive_frame()/avcodec_send_frame()/
 * avcodec_receive_packet() functions provide an encode/decode API, which
 * decouples input and output.
 *
 * The API is very similar for encoding/decoding and audio/video, and works as
 * follows:
 * - Set up and open the AVCodecContext as usual.
 * - Send valid input:
 *   - For decoding, call avcodec_send_packet() to give the decoder raw
 *     compressed data in an AVPacket.
 *   - For encoding, call avcodec_send_frame() to give the encoder an AVFrame
 *     containing uncompressed audio or video.
 *   In both cases, it is recommended that AVPackets and AVFrames are
 *   refcounted, or libavcodec might have to copy the input data. (libavformat
 *   always returns refcounted AVPackets, and av_frame_get_buffer() allocates
 *   refcounted AVFrames.)
 * - Receive output in a loop. Periodically call one of the avcodec_receive_*()
 *   functions and process their output:
 *   - For decoding, call avcodec_receive_frame(). On success, it will return
 *     an AVFrame containing uncompressed audio or video data.
 *   - For encoding, call avcodec_receive_packet(). On success, it will return
 *     an AVPacket with a compressed frame.
 *   Repeat this call until it returns AVERROR(EAGAIN) or an error. The
 *   AVERROR(EAGAIN) return value means that new input data is required to
 *   return new output. In this case, continue with sending input. For each
 *   input frame/packet, the codec will typically return 1 output frame/packet,
 *   but it can also be 0 or more than 1.
 *
 * At the beginning of decoding or encoding, the codec might accept multiple
 * input frames/packets without returning a frame, until its internal buffers
 * are filled. This situation is handled transparently if you follow the steps
 * outlined above.
 *
 * In theory, sending input can result in EAGAIN - this should happen only if
 * not all output was received. You can use this to structure alternative decode
 * or encode loops other than the one suggested above. For example, you could
 * try sending new input on each iteration, and try to receive output if that
 * returns EAGAIN.
 *
 * End of stream situations. These require "flushing" (aka draining) the codec,
 * as the codec might buffer multiple frames or packets internally for
 * performance or out of necessity (consider B-frames).
 * This is handled as follows:
 * - Instead of valid input, send NULL to the avcodec_send_packet() (decoding)
 *   or avcodec_send_frame() (encoding) functions. This will enter draining
 *   mode.
 * - Call avcodec_receive_frame() (decoding) or avcodec_receive_packet()
 *   (encoding) in a loop until AVERROR_EOF is returned. The functions will
 *   not return AVERROR(EAGAIN), unless you forgot to enter draining mode.
 * - Before decoding can be resumed again, the codec has to be reset with
 *   avcodec_flush_buffers().
 *
 * Using the API as outlined above is highly recommended. But it is also
 * possible to call functions outside of this rigid schema. For example, you can
 * call avcodec_send_packet() repeatedly without calling
 * avcodec_receive_frame(). In this case, avcodec_send_packet() will succeed
 * until the codec's internal buffer has been filled up (which is typically of
 * size 1 per output frame, after initial input), and then reject input with
 * AVERROR(EAGAIN). Once it starts rejecting input, you have no choice but to
 * read at least some output.
 *
 * Not all codecs will follow a rigid and predictable dataflow; the only
 * guarantee is that an AVERROR(EAGAIN) return value on a send/receive call on
 * one end implies that a receive/send call on the other end will succeed, or
 * at least will not fail with AVERROR(EAGAIN). In general, no codec will
 * permit unlimited buffering of input or output.
 *
 * This API replaces the following legacy functions:
 * - avcodec_decode_video2() and avcodec_decode_audio4():
 *   Use avcodec_send_packet() to feed input to the decoder, then use
 *   avcodec_receive_frame() to receive decoded frames after each packet.
 *   Unlike with the old video decoding API, multiple frames might result from
 *   a packet. For audio, splitting the input packet into frames by partially
 *   decoding packets becomes transparent to the API user. You never need to
 *   feed an AVPacket to the API twice (unless it is rejected with AVERROR(EAGAIN) - then
 *   no data was read from the packet).
 *   Additionally, sending a flush/draining packet is required only once.
 * - avcodec_encode_video2()/avcodec_encode_audio2():
 *   Use avcodec_send_frame() to feed input to the encoder, then use
 *   avcodec_receive_packet() to receive encoded packets.
 *   Providing user-allocated buffers for avcodec_receive_packet() is not
 *   possible.
 * - The new API does not handle subtitles yet.
 *
 * Mixing new and old function calls on the same AVCodecContext is not allowed,
 * and will result in undefined behavior.
 *
 * Some codecs might require using the new API; using the old API will return
 * an error when calling it. All codecs support the new API.
 *
 * A codec is not allowed to return AVERROR(EAGAIN) for both sending and receiving. This
 * would be an invalid state, which could put the codec user into an endless
 * loop. The API has no concept of time either: it cannot happen that trying to
 * do avcodec_send_packet() results in AVERROR(EAGAIN), but a repeated call 1 second
 * later accepts the packet (with no other receive/flush API calls involved).
 * The API is a strict state machine, and the passage of time is not supposed
 * to influence it. Some timing-dependent behavior might still be deemed
 * acceptable in certain cases. But it must never result in both send/receive
 * returning EAGAIN at the same time at any point. It must also absolutely be
 * avoided that the current state is "unstable" and can "flip-flop" between
 * the send/receive APIs allowing progress. For example, it's not allowed that
 * the codec randomly decides that it actually wants to consume a packet now
 * instead of returning a frame, after it just returned AVERROR(EAGAIN) on an
 * avcodec_send_packet() call.
 * @}
 *)

(**
 * @defgroup lavc_core Core functions/structures.
 * @ingroup libavc
 *
 * Basic definitions, functions for querying libavcodec capabilities,
 * allocating core structures, etc.
 * @{
 *)


(**
 * Identify the syntax and semantics of the bitstream.
 * The principle is roughly:
 * Two decoders with the same ID can decode the same streams.
 * Two encoders with the same ID can encode compatible streams.
 * There may be slight deviations from the principle due to implementation
 * details.
 *
 * If you add a codec ID to this list, add it so that
 * 1. no value of an existing codec ID changes (that would break ABI),
 * 2. it is as close as possible to similar codecs
 *
 * After adding new codec IDs, do not forget to add an entry to the codec
 * descriptor list and bump libavcodec minor version.
 *)
{$IF Defined(BCB) and Defined(VER140)} // C++Builder 6
const
  AV_CODEC_ID_NONE=$0;
  AV_CODEC_ID_MPEG1VIDEO=$1;
  AV_CODEC_ID_MPEG2VIDEO=$2;
{$IFDEF FF_API_XVMC}
  AV_CODEC_ID_MPEG2VIDEO_XVMC=$3;
  _AV_ID_DELTA=0;
{$ELSE}
  _AV_ID_DELTA=1;
{$ENDIF}
  AV_CODEC_ID_H261=$4-_AV_ID_DELTA;
  AV_CODEC_ID_H263=$5-_AV_ID_DELTA;
  AV_CODEC_ID_RV10=$6-_AV_ID_DELTA;
  AV_CODEC_ID_RV20=$7-_AV_ID_DELTA;
  AV_CODEC_ID_MJPEG=$8-_AV_ID_DELTA;
  AV_CODEC_ID_MJPEGB=$9-_AV_ID_DELTA;
  AV_CODEC_ID_LJPEG=$A-_AV_ID_DELTA;
  AV_CODEC_ID_SP5X=$B-_AV_ID_DELTA;
  AV_CODEC_ID_JPEGLS=$C-_AV_ID_DELTA;
  AV_CODEC_ID_MPEG4=$D-_AV_ID_DELTA;
  AV_CODEC_ID_RAWVIDEO=$E-_AV_ID_DELTA;
  AV_CODEC_ID_MSMPEG4V1=$F-_AV_ID_DELTA;
  AV_CODEC_ID_MSMPEG4V2=$10-_AV_ID_DELTA;
  AV_CODEC_ID_MSMPEG4V3=$11-_AV_ID_DELTA;
  AV_CODEC_ID_WMV1=$12-_AV_ID_DELTA;
  AV_CODEC_ID_WMV2=$13-_AV_ID_DELTA;
  AV_CODEC_ID_H263P=$14-_AV_ID_DELTA;
  AV_CODEC_ID_H263I=$15-_AV_ID_DELTA;
  AV_CODEC_ID_FLV1=$16-_AV_ID_DELTA;
  AV_CODEC_ID_SVQ1=$17-_AV_ID_DELTA;
  AV_CODEC_ID_SVQ3=$18-_AV_ID_DELTA;
  AV_CODEC_ID_DVVIDEO=$19-_AV_ID_DELTA;
  AV_CODEC_ID_HUFFYUV=$1A-_AV_ID_DELTA;
  AV_CODEC_ID_CYUV=$1B-_AV_ID_DELTA;
  AV_CODEC_ID_H264=$1C-_AV_ID_DELTA;
  AV_CODEC_ID_INDEO3=$1D-_AV_ID_DELTA;
  AV_CODEC_ID_VP3=$1E-_AV_ID_DELTA;
  AV_CODEC_ID_THEORA=$1F-_AV_ID_DELTA;
  AV_CODEC_ID_ASV1=$20-_AV_ID_DELTA;
  AV_CODEC_ID_ASV2=$21-_AV_ID_DELTA;
  AV_CODEC_ID_FFV1=$22-_AV_ID_DELTA;
  AV_CODEC_ID_4XM=$23-_AV_ID_DELTA;
  AV_CODEC_ID_VCR1=$24-_AV_ID_DELTA;
  AV_CODEC_ID_CLJR=$25-_AV_ID_DELTA;
  AV_CODEC_ID_MDEC=$26-_AV_ID_DELTA;
  AV_CODEC_ID_ROQ=$27-_AV_ID_DELTA;
  AV_CODEC_ID_INTERPLAY_VIDEO=$28-_AV_ID_DELTA;
  AV_CODEC_ID_XAN_WC3=$29-_AV_ID_DELTA;
  AV_CODEC_ID_XAN_WC4=$2A-_AV_ID_DELTA;
  AV_CODEC_ID_RPZA=$2B-_AV_ID_DELTA;
  AV_CODEC_ID_CINEPAK=$2C-_AV_ID_DELTA;
  AV_CODEC_ID_WS_VQA=$2D-_AV_ID_DELTA;
  AV_CODEC_ID_MSRLE=$2E-_AV_ID_DELTA;
  AV_CODEC_ID_MSVIDEO1=$2F-_AV_ID_DELTA;
  AV_CODEC_ID_IDCIN=$30-_AV_ID_DELTA;
  AV_CODEC_ID_8BPS=$31-_AV_ID_DELTA;
  AV_CODEC_ID_SMC=$32-_AV_ID_DELTA;
  AV_CODEC_ID_FLIC=$33-_AV_ID_DELTA;
  AV_CODEC_ID_TRUEMOTION1=$34-_AV_ID_DELTA;
  AV_CODEC_ID_VMDVIDEO=$35-_AV_ID_DELTA;
  AV_CODEC_ID_MSZH=$36-_AV_ID_DELTA;
  AV_CODEC_ID_ZLIB=$37-_AV_ID_DELTA;
  AV_CODEC_ID_QTRLE=$38-_AV_ID_DELTA;
  AV_CODEC_ID_TSCC=$39-_AV_ID_DELTA;
  AV_CODEC_ID_ULTI=$3A-_AV_ID_DELTA;
  AV_CODEC_ID_QDRAW=$3B-_AV_ID_DELTA;
  AV_CODEC_ID_VIXL=$3C-_AV_ID_DELTA;
  AV_CODEC_ID_QPEG=$3D-_AV_ID_DELTA;
  AV_CODEC_ID_PNG=$3E-_AV_ID_DELTA;
  AV_CODEC_ID_PPM=$3F-_AV_ID_DELTA;
  AV_CODEC_ID_PBM=$40-_AV_ID_DELTA;
  AV_CODEC_ID_PGM=$41-_AV_ID_DELTA;
  AV_CODEC_ID_PGMYUV=$42-_AV_ID_DELTA;
  AV_CODEC_ID_PAM=$43-_AV_ID_DELTA;
  AV_CODEC_ID_FFVHUFF=$44-_AV_ID_DELTA;
  AV_CODEC_ID_RV30=$45-_AV_ID_DELTA;
  AV_CODEC_ID_RV40=$46-_AV_ID_DELTA;
  AV_CODEC_ID_VC1=$47-_AV_ID_DELTA;
  AV_CODEC_ID_WMV3=$48-_AV_ID_DELTA;
  AV_CODEC_ID_LOCO=$49-_AV_ID_DELTA;
  AV_CODEC_ID_WNV1=$4A-_AV_ID_DELTA;
  AV_CODEC_ID_AASC=$4B-_AV_ID_DELTA;
  AV_CODEC_ID_INDEO2=$4C-_AV_ID_DELTA;
  AV_CODEC_ID_FRAPS=$4D-_AV_ID_DELTA;
  AV_CODEC_ID_TRUEMOTION2=$4E-_AV_ID_DELTA;
  AV_CODEC_ID_BMP=$4F-_AV_ID_DELTA;
  AV_CODEC_ID_CSCD=$50-_AV_ID_DELTA;
  AV_CODEC_ID_MMVIDEO=$51-_AV_ID_DELTA;
  AV_CODEC_ID_ZMBV=$52-_AV_ID_DELTA;
  AV_CODEC_ID_AVS=$53-_AV_ID_DELTA;
  AV_CODEC_ID_SMACKVIDEO=$54-_AV_ID_DELTA;
  AV_CODEC_ID_NUV=$55-_AV_ID_DELTA;
  AV_CODEC_ID_KMVC=$56-_AV_ID_DELTA;
  AV_CODEC_ID_FLASHSV=$57-_AV_ID_DELTA;
  AV_CODEC_ID_CAVS=$58-_AV_ID_DELTA;
  AV_CODEC_ID_JPEG2000=$59-_AV_ID_DELTA;
  AV_CODEC_ID_VMNC=$5A-_AV_ID_DELTA;
  AV_CODEC_ID_VP5=$5B-_AV_ID_DELTA;
  AV_CODEC_ID_VP6=$5C-_AV_ID_DELTA;
  AV_CODEC_ID_VP6F=$5D-_AV_ID_DELTA;
  AV_CODEC_ID_TARGA=$5E-_AV_ID_DELTA;
  AV_CODEC_ID_DSICINVIDEO=$5F-_AV_ID_DELTA;
  AV_CODEC_ID_TIERTEXSEQVIDEO=$60-_AV_ID_DELTA;
  AV_CODEC_ID_TIFF=$61-_AV_ID_DELTA;
  AV_CODEC_ID_GIF=$62-_AV_ID_DELTA;
  AV_CODEC_ID_DXA=$63-_AV_ID_DELTA;
  AV_CODEC_ID_DNXHD=$64-_AV_ID_DELTA;
  AV_CODEC_ID_THP=$65-_AV_ID_DELTA;
  AV_CODEC_ID_SGI=$66-_AV_ID_DELTA;
  AV_CODEC_ID_C93=$67-_AV_ID_DELTA;
  AV_CODEC_ID_BETHSOFTVID=$68-_AV_ID_DELTA;
  AV_CODEC_ID_PTX=$69-_AV_ID_DELTA;
  AV_CODEC_ID_TXD=$6A-_AV_ID_DELTA;
  AV_CODEC_ID_VP6A=$6B-_AV_ID_DELTA;
  AV_CODEC_ID_AMV=$6C-_AV_ID_DELTA;
  AV_CODEC_ID_VB=$6D-_AV_ID_DELTA;
  AV_CODEC_ID_PCX=$6E-_AV_ID_DELTA;
  AV_CODEC_ID_SUNRAST=$6F-_AV_ID_DELTA;
  AV_CODEC_ID_INDEO4=$70-_AV_ID_DELTA;
  AV_CODEC_ID_INDEO5=$71-_AV_ID_DELTA;
  AV_CODEC_ID_MIMIC=$72-_AV_ID_DELTA;
  AV_CODEC_ID_RL2=$73-_AV_ID_DELTA;
  AV_CODEC_ID_ESCAPE124=$74-_AV_ID_DELTA;
  AV_CODEC_ID_DIRAC=$75-_AV_ID_DELTA;
  AV_CODEC_ID_BFI=$76-_AV_ID_DELTA;
  AV_CODEC_ID_CMV=$77-_AV_ID_DELTA;
  AV_CODEC_ID_MOTIONPIXELS=$78-_AV_ID_DELTA;
  AV_CODEC_ID_TGV=$79-_AV_ID_DELTA;
  AV_CODEC_ID_TGQ=$7A-_AV_ID_DELTA;
  AV_CODEC_ID_TQI=$7B-_AV_ID_DELTA;
  AV_CODEC_ID_AURA=$7C-_AV_ID_DELTA;
  AV_CODEC_ID_AURA2=$7D-_AV_ID_DELTA;
  AV_CODEC_ID_V210X=$7E-_AV_ID_DELTA;
  AV_CODEC_ID_TMV=$7F-_AV_ID_DELTA;
  AV_CODEC_ID_V210=$80-_AV_ID_DELTA;
  AV_CODEC_ID_DPX=$81-_AV_ID_DELTA;
  AV_CODEC_ID_MAD=$82-_AV_ID_DELTA;
  AV_CODEC_ID_FRWU=$83-_AV_ID_DELTA;
  AV_CODEC_ID_FLASHSV2=$84-_AV_ID_DELTA;
  AV_CODEC_ID_CDGRAPHICS=$85-_AV_ID_DELTA;
  AV_CODEC_ID_R210=$86-_AV_ID_DELTA;
  AV_CODEC_ID_ANM=$87-_AV_ID_DELTA;
  AV_CODEC_ID_BINKVIDEO=$88-_AV_ID_DELTA;
  AV_CODEC_ID_IFF_ILBM=$89-_AV_ID_DELTA;
  AV_CODEC_ID_KGV1=$8A-_AV_ID_DELTA;
  AV_CODEC_ID_YOP=$8B-_AV_ID_DELTA;
  AV_CODEC_ID_VP8=$8C-_AV_ID_DELTA;
  AV_CODEC_ID_PICTOR=$8D-_AV_ID_DELTA;
  AV_CODEC_ID_ANSI=$8E-_AV_ID_DELTA;
  AV_CODEC_ID_A64_MULTI=$8F-_AV_ID_DELTA;
  AV_CODEC_ID_A64_MULTI5=$90-_AV_ID_DELTA;
  AV_CODEC_ID_R10K=$91-_AV_ID_DELTA;
  AV_CODEC_ID_MXPEG=$92-_AV_ID_DELTA;
  AV_CODEC_ID_LAGARITH=$93-_AV_ID_DELTA;
  AV_CODEC_ID_PRORES=$94-_AV_ID_DELTA;
  AV_CODEC_ID_JV=$95-_AV_ID_DELTA;
  AV_CODEC_ID_DFA=$96-_AV_ID_DELTA;
  AV_CODEC_ID_WMV3IMAGE=$97-_AV_ID_DELTA;
  AV_CODEC_ID_VC1IMAGE=$98-_AV_ID_DELTA;
  AV_CODEC_ID_UTVIDEO=$99-_AV_ID_DELTA;
  AV_CODEC_ID_BMV_VIDEO=$9A-_AV_ID_DELTA;
  AV_CODEC_ID_VBLE=$9B-_AV_ID_DELTA;
  AV_CODEC_ID_DXTORY=$9C-_AV_ID_DELTA;
  AV_CODEC_ID_V410=$9D-_AV_ID_DELTA;
  AV_CODEC_ID_XWD=$9E-_AV_ID_DELTA;
  AV_CODEC_ID_CDXL=$9F-_AV_ID_DELTA;
  AV_CODEC_ID_XBM=$100-_AV_ID_DELTA;
  AV_CODEC_ID_ZEROCODEC=$101-_AV_ID_DELTA;
  AV_CODEC_ID_MSS1=$102-_AV_ID_DELTA;
  AV_CODEC_ID_MSA1=$103-_AV_ID_DELTA;
  AV_CODEC_ID_TSCC2=$104-_AV_ID_DELTA;
  AV_CODEC_ID_MTS2=$105-_AV_ID_DELTA;
  AV_CODEC_ID_CLLC=$106-_AV_ID_DELTA;
  AV_CODEC_ID_MSS2=$107-_AV_ID_DELTA;
  AV_CODEC_ID_VP9=$108-_AV_ID_DELTA;
  AV_CODEC_ID_AIC=$109-_AV_ID_DELTA;
  AV_CODEC_ID_ESCAPE130=$10A-_AV_ID_DELTA;
  AV_CODEC_ID_G2M=$10B-_AV_ID_DELTA;
  AV_CODEC_ID_WEBP=$10C-_AV_ID_DELTA;
  AV_CODEC_ID_HNM4_VIDEO=$10D-_AV_ID_DELTA;
  AV_CODEC_ID_HEVC=$10E-_AV_ID_DELTA;
  AV_CODEC_ID_FIC=$10F-_AV_ID_DELTA;
  AV_CODEC_ID_ALIAS_PIX=$110-_AV_ID_DELTA;
  AV_CODEC_ID_BRENDER_PIX=$111-_AV_ID_DELTA;
  AV_CODEC_ID_PAF_VIDEO=$112-_AV_ID_DELTA;
  AV_CODEC_ID_EXR=$113-_AV_ID_DELTA;
  AV_CODEC_ID_VP7=$114-_AV_ID_DELTA;
  AV_CODEC_ID_SANM=$115-_AV_ID_DELTA;
  AV_CODEC_ID_SGIRLE=$116-_AV_ID_DELTA;
  AV_CODEC_ID_MVC1=$117-_AV_ID_DELTA;
  AV_CODEC_ID_MVC2=$118-_AV_ID_DELTA;
  AV_CODEC_ID_HQX=$119-_AV_ID_DELTA;
  AV_CODEC_ID_TDSC=$11A-_AV_ID_DELTA;
  AV_CODEC_ID_HQ_HQA=$11B-_AV_ID_DELTA;
  AV_CODEC_ID_HAP=$11C-_AV_ID_DELTA;
  AV_CODEC_ID_DDS=$11D-_AV_ID_DELTA;
  AV_CODEC_ID_DXV=$11E-_AV_ID_DELTA;
  AV_CODEC_ID_SCREENPRESSO=$11F-_AV_ID_DELTA;
  AV_CODEC_ID_RSCC=$120-_AV_ID_DELTA;

  AV_CODEC_ID_Y41P=$8000;
  AV_CODEC_ID_AVRP=$8001;
  AV_CODEC_ID_012V=$8002;
  AV_CODEC_ID_AVUI=$8003;
  AV_CODEC_ID_AYUV=$8004;
  AV_CODEC_ID_TARGA_Y216=$8005;
  AV_CODEC_ID_V308=$8006;
  AV_CODEC_ID_V408=$8007;
  AV_CODEC_ID_YUV4=$8008;
  AV_CODEC_ID_AVRN=$8009;
  AV_CODEC_ID_CPIA=$800A;
  AV_CODEC_ID_XFACE=$800B;
  AV_CODEC_ID_SNOW=$800C;
  AV_CODEC_ID_SMVJPEG=$800D;
  AV_CODEC_ID_APNG=$800E;
  AV_CODEC_ID_DAALA=$800F;
  AV_CODEC_ID_CFHD=$8010;
  AV_CODEC_ID_TRUEMOTION2RT=$8011;
  AV_CODEC_ID_M101=$8012;
  AV_CODEC_ID_MAGICYUV=$8013;
  AV_CODEC_ID_SHEERVIDEO=$8014;
  AV_CODEC_ID_YLC=$8015;
  AV_CODEC_ID_PSD=$8016;
  AV_CODEC_ID_PIXLET=$8017;
  AV_CODEC_ID_SPEEDHQ=$8018;
  AV_CODEC_ID_FMVC=$8019;
  AV_CODEC_ID_SCPR=$801A;
  AV_CODEC_ID_CLEARVIDEO=$801B;
  AV_CODEC_ID_XPM=$801C;
  AV_CODEC_ID_AV1=$801D;

  AV_CODEC_ID_FIRST_AUDIO=$10000;
  AV_CODEC_ID_PCM_S16LE=$10000;
  AV_CODEC_ID_PCM_S16BE=$10001;
  AV_CODEC_ID_PCM_U16LE=$10002;
  AV_CODEC_ID_PCM_U16BE=$10003;
  AV_CODEC_ID_PCM_S8=$10004;
  AV_CODEC_ID_PCM_U8=$10005;
  AV_CODEC_ID_PCM_MULAW=$10006;
  AV_CODEC_ID_PCM_ALAW=$10007;
  AV_CODEC_ID_PCM_S32LE=$10008;
  AV_CODEC_ID_PCM_S32BE=$10009;
  AV_CODEC_ID_PCM_U32LE=$1000A;
  AV_CODEC_ID_PCM_U32BE=$1000B;
  AV_CODEC_ID_PCM_S24LE=$1000C;
  AV_CODEC_ID_PCM_S24BE=$1000D;
  AV_CODEC_ID_PCM_U24LE=$1000E;
  AV_CODEC_ID_PCM_U24BE=$1000F;
  AV_CODEC_ID_PCM_S24DAUD=$10010;
  AV_CODEC_ID_PCM_ZORK=$10011;
  AV_CODEC_ID_PCM_S16LE_PLANAR=$10012;
  AV_CODEC_ID_PCM_DVD=$10013;
  AV_CODEC_ID_PCM_F32BE=$10014;
  AV_CODEC_ID_PCM_F32LE=$10015;
  AV_CODEC_ID_PCM_F64BE=$10016;
  AV_CODEC_ID_PCM_F64LE=$10017;
  AV_CODEC_ID_PCM_BLURAY=$10018;
  AV_CODEC_ID_PCM_LXF=$10019;
  AV_CODEC_ID_S302M=$1001A;
  AV_CODEC_ID_PCM_S8_PLANAR=$1001B;
  AV_CODEC_ID_PCM_S24LE_PLANAR=$1001C;
  AV_CODEC_ID_PCM_S32LE_PLANAR=$1001D;
  AV_CODEC_ID_PCM_S16BE_PLANAR=$1001E;

  AV_CODEC_ID_PCM_S64LE=$10800;
  AV_CODEC_ID_PCM_S64BE=$10801;
  AV_CODEC_ID_PCM_F16LE=$10802;
  AV_CODEC_ID_PCM_F24LE=$10803;

  AV_CODEC_ID_ADPCM_IMA_QT=$11000;
  AV_CODEC_ID_ADPCM_IMA_WAV=$11001;
  AV_CODEC_ID_ADPCM_IMA_DK3=$11002;
  AV_CODEC_ID_ADPCM_IMA_DK4=$11003;
  AV_CODEC_ID_ADPCM_IMA_WS=$11004;
  AV_CODEC_ID_ADPCM_IMA_SMJPEG=$11005;
  AV_CODEC_ID_ADPCM_MS=$11006;
  AV_CODEC_ID_ADPCM_4XM=$11007;
  AV_CODEC_ID_ADPCM_XA=$11008;
  AV_CODEC_ID_ADPCM_ADX=$11009;
  AV_CODEC_ID_ADPCM_EA=$1100A;
  AV_CODEC_ID_ADPCM_G726=$1100B;
  AV_CODEC_ID_ADPCM_CT=$1100C;
  AV_CODEC_ID_ADPCM_SWF=$1100D;
  AV_CODEC_ID_ADPCM_YAMAHA=$1100E;
  AV_CODEC_ID_ADPCM_SBPRO_4=$1100F;
  AV_CODEC_ID_ADPCM_SBPRO_3=$11010;
  AV_CODEC_ID_ADPCM_SBPRO_2=$11011;
  AV_CODEC_ID_ADPCM_THP=$11012;
  AV_CODEC_ID_ADPCM_IMA_AMV=$11013;
  AV_CODEC_ID_ADPCM_EA_R1=$11014;
  AV_CODEC_ID_ADPCM_EA_R3=$11015;
  AV_CODEC_ID_ADPCM_EA_R2=$11016;
  AV_CODEC_ID_ADPCM_IMA_EA_SEAD=$11017;
  AV_CODEC_ID_ADPCM_IMA_EA_EACS=$11018;
  AV_CODEC_ID_ADPCM_EA_XAS=$11019;
  AV_CODEC_ID_ADPCM_EA_MAXIS_XA=$1101A;
  AV_CODEC_ID_ADPCM_IMA_ISS=$1101B;
  AV_CODEC_ID_ADPCM_G722=$1101C;
  AV_CODEC_ID_ADPCM_IMA_APC=$1101D;
  AV_CODEC_ID_ADPCM_VIMA=$1101E;
{$IFDEF FF_API_VIMA_DECODER}
  AV_CODEC_ID_VIMA=AV_CODEC_ID_ADPCM_VIMA;
{$ENDIF}

  AV_CODEC_ID_ADPCM_AFC=$11800;
  AV_CODEC_ID_ADPCM_IMA_OKI=$11801;
  AV_CODEC_ID_ADPCM_DTK=$11802;
  AV_CODEC_ID_ADPCM_IMA_RAD=$11803;
  AV_CODEC_ID_ADPCM_G726LE=$11804;
  AV_CODEC_ID_ADPCM_THP_LE=$11805;
  AV_CODEC_ID_ADPCM_PSX=$11806;
  AV_CODEC_ID_ADPCM_AICA=$11807;
  AV_CODEC_ID_ADPCM_IMA_DAT4=$11808;
  AV_CODEC_ID_ADPCM_MTAF=$11809;

  AV_CODEC_ID_AMR_NB=$12000;
  AV_CODEC_ID_AMR_WB=$12001;
  AV_CODEC_ID_RA_144=$13000;
  AV_CODEC_ID_RA_288=$13001;
  AV_CODEC_ID_ROQ_DPCM=$14000;
  AV_CODEC_ID_INTERPLAY_DPCM=$14001;
  AV_CODEC_ID_XAN_DPCM=$14002;
  AV_CODEC_ID_SOL_DPCM=$14003;
  AV_CODEC_ID_SDX2_DPCM=$14800;
  AV_CODEC_ID_MP2=$15000;
  AV_CODEC_ID_MP3=$15001;
  AV_CODEC_ID_AAC=$15002;
  AV_CODEC_ID_AC3=$15003;
  AV_CODEC_ID_DTS=$15004;
  AV_CODEC_ID_VORBIS=$15005;
  AV_CODEC_ID_DVAUDIO=$15006;
  AV_CODEC_ID_WMAV1=$15007;
  AV_CODEC_ID_WMAV2=$15008;
  AV_CODEC_ID_MACE3=$15009;
  AV_CODEC_ID_MACE6=$1500A;
  AV_CODEC_ID_VMDAUDIO=$1500B;
  AV_CODEC_ID_FLAC=$1500C;
  AV_CODEC_ID_MP3ADU=$1500D;
  AV_CODEC_ID_MP3ON4=$1500E;
  AV_CODEC_ID_SHORTEN=$1500F;
  AV_CODEC_ID_ALAC=$15010;
  AV_CODEC_ID_WESTWOOD_SND1=$15011;
  AV_CODEC_ID_GSM=$15012;
  AV_CODEC_ID_QDM2=$15013;
  AV_CODEC_ID_COOK=$15014;
  AV_CODEC_ID_TRUESPEECH=$15015;
  AV_CODEC_ID_TTA=$15016;
  AV_CODEC_ID_SMACKAUDIO=$15017;
  AV_CODEC_ID_QCELP=$15018;
  AV_CODEC_ID_WAVPACK=$15019;
  AV_CODEC_ID_DSICINAUDIO=$1501A;
  AV_CODEC_ID_IMC=$1501B;
  AV_CODEC_ID_MUSEPACK7=$1501C;
  AV_CODEC_ID_MLP=$1501D;
  AV_CODEC_ID_GSM_MS=$1501E;
  AV_CODEC_ID_ATRAC3=$1501F;
{$IFDEF FF_API_VOXWARE}
  AV_CODEC_ID_VOXWARE=$15020;
  _AV_ID_DELTA_2=0;
{$ELSE}
  _AV_ID_DELTA_2=1;
{$ENDIF}
  AV_CODEC_ID_APE=$15021-_AV_ID_DELTA_2;
  AV_CODEC_ID_NELLYMOSER=$15022-_AV_ID_DELTA_2;
  AV_CODEC_ID_MUSEPACK8=$15023-_AV_ID_DELTA_2;
  AV_CODEC_ID_SPEEX=$15024-_AV_ID_DELTA_2;
  AV_CODEC_ID_WMAVOICE=$15025-_AV_ID_DELTA_2;
  AV_CODEC_ID_WMAPRO=$15026-_AV_ID_DELTA_2;
  AV_CODEC_ID_WMALOSSLESS=$15027-_AV_ID_DELTA_2;
  AV_CODEC_ID_ATRAC3P=$15028-_AV_ID_DELTA_2;
  AV_CODEC_ID_EAC3=$15029-_AV_ID_DELTA_2;
  AV_CODEC_ID_SIPR=$1502A-_AV_ID_DELTA_2;
  AV_CODEC_ID_MP1=$1502B-_AV_ID_DELTA_2;
  AV_CODEC_ID_TWINVQ=$1502C-_AV_ID_DELTA_2;
  AV_CODEC_ID_TRUEHD=$1502D-_AV_ID_DELTA_2;
  AV_CODEC_ID_MP4ALS=$1502E-_AV_ID_DELTA_2;
  AV_CODEC_ID_ATRAC1=$1502F-_AV_ID_DELTA_2;
  AV_CODEC_ID_BINKAUDIO_RDFT=$15030-_AV_ID_DELTA_2;
  AV_CODEC_ID_BINKAUDIO_DCT=$15031-_AV_ID_DELTA_2;
  AV_CODEC_ID_AAC_LATM=$15032-_AV_ID_DELTA_2;
  AV_CODEC_ID_QDMC=$15033-_AV_ID_DELTA_2;
  AV_CODEC_ID_CELT=$15034-_AV_ID_DELTA_2;
  AV_CODEC_ID_G723_1=$15035-_AV_ID_DELTA_2;
  AV_CODEC_ID_G729=$15036-_AV_ID_DELTA_2;
  AV_CODEC_ID_8SVX_EXP=$15037-_AV_ID_DELTA_2;
  AV_CODEC_ID_8SVX_FIB=$15038-_AV_ID_DELTA_2;
  AV_CODEC_ID_BMV_AUDIO=$15039-_AV_ID_DELTA_2;
  AV_CODEC_ID_RALF=$1503A-_AV_ID_DELTA_2;
  AV_CODEC_ID_IAC=$1503B-_AV_ID_DELTA_2;
  AV_CODEC_ID_ILBC=$1503C-_AV_ID_DELTA_2;
  AV_CODEC_ID_OPUS=$1503D-_AV_ID_DELTA_2;
  AV_CODEC_ID_COMFORT_NOISE=$1503E-_AV_ID_DELTA_2;
  AV_CODEC_ID_TAK=$1503F-_AV_ID_DELTA_2;
  AV_CODEC_ID_METASOUND=$15040-_AV_ID_DELTA_2;
  AV_CODEC_ID_PAF_AUDIO=$15041-_AV_ID_DELTA_2;
  AV_CODEC_ID_ON2AVC=$15042-_AV_ID_DELTA_2;
  AV_CODEC_ID_DSS_SP=$15043-_AV_ID_DELTA_2;
  AV_CODEC_ID_FFWAVESYNTH=$15800;
  AV_CODEC_ID_SONIC=$15801;
  AV_CODEC_ID_SONIC_LS=$15802;
  AV_CODEC_ID_EVRC=$15803;
  AV_CODEC_ID_SMV=$15804;
  AV_CODEC_ID_DSD_LSBF=$15805;
  AV_CODEC_ID_DSD_MSBF=$15806;
  AV_CODEC_ID_DSD_LSBF_PLANAR=$15807;
  AV_CODEC_ID_DSD_MSBF_PLANAR=$15808;
  AV_CODEC_ID_4GV=$15809;
  AV_CODEC_ID_INTERPLAY_ACM=$1580A;
  AV_CODEC_ID_XMA1=$1580B;
  AV_CODEC_ID_XMA2=$1580C;
  AV_CODEC_ID_DST=$1580D;
  AV_CODEC_ID_ATRAC3AL=$1580E;
  AV_CODEC_ID_ATRAC3PAL$1580F;
  AV_CODEC_ID_FIRST_SUBTITLE=$17000;
  AV_CODEC_ID_DVD_SUBTITLE=$17000;
  AV_CODEC_ID_DVB_SUBTITLE=$17001;
  AV_CODEC_ID_TEXT=$17002;
  AV_CODEC_ID_XSUB=$17003;
  AV_CODEC_ID_SSA=$17004;
  AV_CODEC_ID_MOV_TEXT=$17005;
  AV_CODEC_ID_HDMV_PGS_SUBTITLE=$17006;
  AV_CODEC_ID_DVB_TELETEXT=$17007;
  AV_CODEC_ID_SRT=$17008;
  AV_CODEC_ID_MICRODVD=$17800;
  AV_CODEC_ID_EIA_608=$17801;
  AV_CODEC_ID_JACOSUB=$17802;
  AV_CODEC_ID_SAMI=$17803;
  AV_CODEC_ID_REALTEXT=$17804;
  AV_CODEC_ID_STL=$17805;
  AV_CODEC_ID_SUBVIEWER1=$17806;
  AV_CODEC_ID_SUBVIEWER=$17807;
  AV_CODEC_ID_SUBRIP=$17808;
  AV_CODEC_ID_WEBVTT=$17809;
  AV_CODEC_ID_MPL2=$1780A;
  AV_CODEC_ID_VPLAYER=$1780B;
  AV_CODEC_ID_PJS=$1780C;
  AV_CODEC_ID_ASS=$1780D;
  AV_CODEC_ID_HDMV_TEXT_SUBTITLE=$1780E;
  AV_CODEC_ID_FIRST_UNKNOWN=$18000;
  AV_CODEC_ID_TTF=$18000;
  AV_CODEC_ID_SCTE_35=$18001;
  AV_CODEC_ID_BINTEXT=$18800;
  AV_CODEC_ID_XBIN=$18801;
  AV_CODEC_ID_IDF=$18802;
  AV_CODEC_ID_OTF=$18803;
  AV_CODEC_ID_SMPTE_KLV=$18804;
  AV_CODEC_ID_DVD_NAV=$18805;
  AV_CODEC_ID_TIMED_ID3=$18806;
  AV_CODEC_ID_BIN_DATA=$18807;
  AV_CODEC_ID_PROBE=$19000;
  AV_CODEC_ID_MPEG2TS=$20000;
  AV_CODEC_ID_MPEG4SYSTEMS=$20001;
  AV_CODEC_ID_FFMETADATA=$21000;
  AV_CODEC_ID_WRAPPED_AVFRAME=$21001;
  ME_ZERO=1;
  ME_FULL=2;
  ME_LOG=3;
  ME_PHODS=4;
  ME_EPZS=5;
  ME_X1=6;
  ME_HEX=7;
  ME_UMH=8;
  ME_ITER=9;
  ME_TESA=10;

  AVDISCARD_NONE   =-16;
  AVDISCARD_DEFAULT=  0;
  AVDISCARD_NONREF =  8;
  AVDISCARD_BIDIR  = 16;
  AVDISCARD_NONKEY = 32;
  AVDISCARD_ALL    = 48;

  AV_AUDIO_SERVICE_TYPE_MAIN              = 0;
  AV_AUDIO_SERVICE_TYPE_EFFECTS           = 1;
  AV_AUDIO_SERVICE_TYPE_VISUALLY_IMPAIRED = 2;
  AV_AUDIO_SERVICE_TYPE_HEARING_IMPAIRED  = 3;
  AV_AUDIO_SERVICE_TYPE_DIALOGUE          = 4;
  AV_AUDIO_SERVICE_TYPE_COMMENTARY        = 5;
  AV_AUDIO_SERVICE_TYPE_EMERGENCY         = 6;
  AV_AUDIO_SERVICE_TYPE_VOICE_OVER        = 7;
  AV_AUDIO_SERVICE_TYPE_KARAOKE           = 8;
  AV_AUDIO_SERVICE_TYPE_NB                = 9;
{$IFEND}

{$IF Defined(BCB) and Defined(VER140)} // C++Builder 6
type
  PAVCodecID = ^TAVCodecID;
  TAVCodecID = Integer;
{$ELSE}
type
  PAVCodecID = ^TAVCodecID;
  TAVCodecID = (
    AV_CODEC_ID_NONE,

    (* video codecs *)
    AV_CODEC_ID_MPEG1VIDEO,
    AV_CODEC_ID_MPEG2VIDEO,      ///< preferred ID for MPEG-1/2 video decoding
{$IFDEF FF_API_XVMC}
    AV_CODEC_ID_MPEG2VIDEO_XVMC,
{$ENDIF}
    AV_CODEC_ID_H261,
    AV_CODEC_ID_H263,
    AV_CODEC_ID_RV10,
    AV_CODEC_ID_RV20,
    AV_CODEC_ID_MJPEG,
    AV_CODEC_ID_MJPEGB,
    AV_CODEC_ID_LJPEG,
    AV_CODEC_ID_SP5X,
    AV_CODEC_ID_JPEGLS,
    AV_CODEC_ID_MPEG4,
    AV_CODEC_ID_RAWVIDEO,
    AV_CODEC_ID_MSMPEG4V1,
    AV_CODEC_ID_MSMPEG4V2,
    AV_CODEC_ID_MSMPEG4V3,
    AV_CODEC_ID_WMV1,
    AV_CODEC_ID_WMV2,
    AV_CODEC_ID_H263P,
    AV_CODEC_ID_H263I,
    AV_CODEC_ID_FLV1,
    AV_CODEC_ID_SVQ1,
    AV_CODEC_ID_SVQ3,
    AV_CODEC_ID_DVVIDEO,
    AV_CODEC_ID_HUFFYUV,
    AV_CODEC_ID_CYUV,
    AV_CODEC_ID_H264,
    AV_CODEC_ID_INDEO3,
    AV_CODEC_ID_VP3,
    AV_CODEC_ID_THEORA,
    AV_CODEC_ID_ASV1,
    AV_CODEC_ID_ASV2,
    AV_CODEC_ID_FFV1,
    AV_CODEC_ID_4XM,
    AV_CODEC_ID_VCR1,
    AV_CODEC_ID_CLJR,
    AV_CODEC_ID_MDEC,
    AV_CODEC_ID_ROQ,
    AV_CODEC_ID_INTERPLAY_VIDEO,
    AV_CODEC_ID_XAN_WC3,
    AV_CODEC_ID_XAN_WC4,
    AV_CODEC_ID_RPZA,
    AV_CODEC_ID_CINEPAK,
    AV_CODEC_ID_WS_VQA,
    AV_CODEC_ID_MSRLE,
    AV_CODEC_ID_MSVIDEO1,
    AV_CODEC_ID_IDCIN,
    AV_CODEC_ID_8BPS,
    AV_CODEC_ID_SMC,
    AV_CODEC_ID_FLIC,
    AV_CODEC_ID_TRUEMOTION1,
    AV_CODEC_ID_VMDVIDEO,
    AV_CODEC_ID_MSZH,
    AV_CODEC_ID_ZLIB,
    AV_CODEC_ID_QTRLE,
    AV_CODEC_ID_TSCC,
    AV_CODEC_ID_ULTI,
    AV_CODEC_ID_QDRAW,
    AV_CODEC_ID_VIXL,
    AV_CODEC_ID_QPEG,
    AV_CODEC_ID_PNG,
    AV_CODEC_ID_PPM,
    AV_CODEC_ID_PBM,
    AV_CODEC_ID_PGM,
    AV_CODEC_ID_PGMYUV,
    AV_CODEC_ID_PAM,
    AV_CODEC_ID_FFVHUFF,
    AV_CODEC_ID_RV30,
    AV_CODEC_ID_RV40,
    AV_CODEC_ID_VC1,
    AV_CODEC_ID_WMV3,
    AV_CODEC_ID_LOCO,
    AV_CODEC_ID_WNV1,
    AV_CODEC_ID_AASC,
    AV_CODEC_ID_INDEO2,
    AV_CODEC_ID_FRAPS,
    AV_CODEC_ID_TRUEMOTION2,
    AV_CODEC_ID_BMP,
    AV_CODEC_ID_CSCD,
    AV_CODEC_ID_MMVIDEO,
    AV_CODEC_ID_ZMBV,
    AV_CODEC_ID_AVS,
    AV_CODEC_ID_SMACKVIDEO,
    AV_CODEC_ID_NUV,
    AV_CODEC_ID_KMVC,
    AV_CODEC_ID_FLASHSV,
    AV_CODEC_ID_CAVS,
    AV_CODEC_ID_JPEG2000,
    AV_CODEC_ID_VMNC,
    AV_CODEC_ID_VP5,
    AV_CODEC_ID_VP6,
    AV_CODEC_ID_VP6F,
    AV_CODEC_ID_TARGA,
    AV_CODEC_ID_DSICINVIDEO,
    AV_CODEC_ID_TIERTEXSEQVIDEO,
    AV_CODEC_ID_TIFF,
    AV_CODEC_ID_GIF,
    AV_CODEC_ID_DXA,
    AV_CODEC_ID_DNXHD,
    AV_CODEC_ID_THP,
    AV_CODEC_ID_SGI,
    AV_CODEC_ID_C93,
    AV_CODEC_ID_BETHSOFTVID,
    AV_CODEC_ID_PTX,
    AV_CODEC_ID_TXD,
    AV_CODEC_ID_VP6A,
    AV_CODEC_ID_AMV,
    AV_CODEC_ID_VB,
    AV_CODEC_ID_PCX,
    AV_CODEC_ID_SUNRAST,
    AV_CODEC_ID_INDEO4,
    AV_CODEC_ID_INDEO5,
    AV_CODEC_ID_MIMIC,
    AV_CODEC_ID_RL2,
    AV_CODEC_ID_ESCAPE124,
    AV_CODEC_ID_DIRAC,
    AV_CODEC_ID_BFI,
    AV_CODEC_ID_CMV,
    AV_CODEC_ID_MOTIONPIXELS,
    AV_CODEC_ID_TGV,
    AV_CODEC_ID_TGQ,
    AV_CODEC_ID_TQI,
    AV_CODEC_ID_AURA,
    AV_CODEC_ID_AURA2,
    AV_CODEC_ID_V210X,
    AV_CODEC_ID_TMV,
    AV_CODEC_ID_V210,
    AV_CODEC_ID_DPX,
    AV_CODEC_ID_MAD,
    AV_CODEC_ID_FRWU,
    AV_CODEC_ID_FLASHSV2,
    AV_CODEC_ID_CDGRAPHICS,
    AV_CODEC_ID_R210,
    AV_CODEC_ID_ANM,
    AV_CODEC_ID_BINKVIDEO,
    AV_CODEC_ID_IFF_ILBM,
    AV_CODEC_ID_KGV1,
    AV_CODEC_ID_YOP,
    AV_CODEC_ID_VP8,
    AV_CODEC_ID_PICTOR,
    AV_CODEC_ID_ANSI,
    AV_CODEC_ID_A64_MULTI,
    AV_CODEC_ID_A64_MULTI5,
    AV_CODEC_ID_R10K,
    AV_CODEC_ID_MXPEG,
    AV_CODEC_ID_LAGARITH,
    AV_CODEC_ID_PRORES,
    AV_CODEC_ID_JV,
    AV_CODEC_ID_DFA,
    AV_CODEC_ID_WMV3IMAGE,
    AV_CODEC_ID_VC1IMAGE,
    AV_CODEC_ID_UTVIDEO,
    AV_CODEC_ID_BMV_VIDEO,
    AV_CODEC_ID_VBLE,
    AV_CODEC_ID_DXTORY,
    AV_CODEC_ID_V410,
    AV_CODEC_ID_XWD,
    AV_CODEC_ID_CDXL,
    AV_CODEC_ID_XBM,
    AV_CODEC_ID_ZEROCODEC,
    AV_CODEC_ID_MSS1,
    AV_CODEC_ID_MSA1,
    AV_CODEC_ID_TSCC2,
    AV_CODEC_ID_MTS2,
    AV_CODEC_ID_CLLC,
    AV_CODEC_ID_MSS2,
    AV_CODEC_ID_VP9,
    AV_CODEC_ID_AIC,
    AV_CODEC_ID_ESCAPE130,
    AV_CODEC_ID_G2M,
    AV_CODEC_ID_WEBP,
    AV_CODEC_ID_HNM4_VIDEO,
    AV_CODEC_ID_HEVC,
    AV_CODEC_ID_FIC,
    AV_CODEC_ID_ALIAS_PIX,
    AV_CODEC_ID_BRENDER_PIX,
    AV_CODEC_ID_PAF_VIDEO,
    AV_CODEC_ID_EXR,
    AV_CODEC_ID_VP7,
    AV_CODEC_ID_SANM,
    AV_CODEC_ID_SGIRLE,
    AV_CODEC_ID_MVC1,
    AV_CODEC_ID_MVC2,
    AV_CODEC_ID_HQX,
    AV_CODEC_ID_TDSC,
    AV_CODEC_ID_HQ_HQA,
    AV_CODEC_ID_HAP,
    AV_CODEC_ID_DDS,
    AV_CODEC_ID_DXV,
    AV_CODEC_ID_SCREENPRESSO,
    AV_CODEC_ID_RSCC,

    AV_CODEC_ID_Y41P = $8000,
    AV_CODEC_ID_AVRP,
    AV_CODEC_ID_012V,
    AV_CODEC_ID_AVUI,
    AV_CODEC_ID_AYUV,
    AV_CODEC_ID_TARGA_Y216,
    AV_CODEC_ID_V308,
    AV_CODEC_ID_V408,
    AV_CODEC_ID_YUV4,
    AV_CODEC_ID_AVRN,
    AV_CODEC_ID_CPIA,
    AV_CODEC_ID_XFACE,
    AV_CODEC_ID_SNOW,
    AV_CODEC_ID_SMVJPEG,
    AV_CODEC_ID_APNG,
    AV_CODEC_ID_DAALA,
    AV_CODEC_ID_CFHD,
    AV_CODEC_ID_TRUEMOTION2RT,
    AV_CODEC_ID_M101,
    AV_CODEC_ID_MAGICYUV,
    AV_CODEC_ID_SHEERVIDEO,
    AV_CODEC_ID_YLC,
    AV_CODEC_ID_PSD,
    AV_CODEC_ID_PIXLET,
    AV_CODEC_ID_SPEEDHQ,
    AV_CODEC_ID_FMVC,
    AV_CODEC_ID_SCPR,
    AV_CODEC_ID_CLEARVIDEO,
    AV_CODEC_ID_XPM,
    AV_CODEC_ID_AV1,

    (* various PCM "codecs" *)
{$IFNDEF FPC}
    AV_CODEC_ID_FIRST_AUDIO = $10000,     ///< A dummy id pointing at the start of audio codecs
{$ENDIF}
    AV_CODEC_ID_PCM_S16LE = $10000,
    AV_CODEC_ID_PCM_S16BE,
    AV_CODEC_ID_PCM_U16LE,
    AV_CODEC_ID_PCM_U16BE,
    AV_CODEC_ID_PCM_S8,
    AV_CODEC_ID_PCM_U8,
    AV_CODEC_ID_PCM_MULAW,
    AV_CODEC_ID_PCM_ALAW,
    AV_CODEC_ID_PCM_S32LE,
    AV_CODEC_ID_PCM_S32BE,
    AV_CODEC_ID_PCM_U32LE,
    AV_CODEC_ID_PCM_U32BE,
    AV_CODEC_ID_PCM_S24LE,
    AV_CODEC_ID_PCM_S24BE,
    AV_CODEC_ID_PCM_U24LE,
    AV_CODEC_ID_PCM_U24BE,
    AV_CODEC_ID_PCM_S24DAUD,
    AV_CODEC_ID_PCM_ZORK,
    AV_CODEC_ID_PCM_S16LE_PLANAR,
    AV_CODEC_ID_PCM_DVD,
    AV_CODEC_ID_PCM_F32BE,
    AV_CODEC_ID_PCM_F32LE,
    AV_CODEC_ID_PCM_F64BE,
    AV_CODEC_ID_PCM_F64LE,
    AV_CODEC_ID_PCM_BLURAY,
    AV_CODEC_ID_PCM_LXF,
    AV_CODEC_ID_S302M,
    AV_CODEC_ID_PCM_S8_PLANAR,
    AV_CODEC_ID_PCM_S24LE_PLANAR,
    AV_CODEC_ID_PCM_S32LE_PLANAR,
    AV_CODEC_ID_PCM_S16BE_PLANAR,

    AV_CODEC_ID_PCM_S64LE = $10800,
    AV_CODEC_ID_PCM_S64BE,
    AV_CODEC_ID_PCM_F16LE,
    AV_CODEC_ID_PCM_F24LE,

    (* various ADPCM codecs *)
    AV_CODEC_ID_ADPCM_IMA_QT = $11000,
    AV_CODEC_ID_ADPCM_IMA_WAV,
    AV_CODEC_ID_ADPCM_IMA_DK3,
    AV_CODEC_ID_ADPCM_IMA_DK4,
    AV_CODEC_ID_ADPCM_IMA_WS,
    AV_CODEC_ID_ADPCM_IMA_SMJPEG,
    AV_CODEC_ID_ADPCM_MS,
    AV_CODEC_ID_ADPCM_4XM,
    AV_CODEC_ID_ADPCM_XA,
    AV_CODEC_ID_ADPCM_ADX,
    AV_CODEC_ID_ADPCM_EA,
    AV_CODEC_ID_ADPCM_G726,
    AV_CODEC_ID_ADPCM_CT,
    AV_CODEC_ID_ADPCM_SWF,
    AV_CODEC_ID_ADPCM_YAMAHA,
    AV_CODEC_ID_ADPCM_SBPRO_4,
    AV_CODEC_ID_ADPCM_SBPRO_3,
    AV_CODEC_ID_ADPCM_SBPRO_2,
    AV_CODEC_ID_ADPCM_THP,
    AV_CODEC_ID_ADPCM_IMA_AMV,
    AV_CODEC_ID_ADPCM_EA_R1,
    AV_CODEC_ID_ADPCM_EA_R3,
    AV_CODEC_ID_ADPCM_EA_R2,
    AV_CODEC_ID_ADPCM_IMA_EA_SEAD,
    AV_CODEC_ID_ADPCM_IMA_EA_EACS,
    AV_CODEC_ID_ADPCM_EA_XAS,
    AV_CODEC_ID_ADPCM_EA_MAXIS_XA,
    AV_CODEC_ID_ADPCM_IMA_ISS,
    AV_CODEC_ID_ADPCM_G722,
    AV_CODEC_ID_ADPCM_IMA_APC,
    AV_CODEC_ID_ADPCM_VIMA,

    AV_CODEC_ID_ADPCM_AFC = $11800,
    AV_CODEC_ID_ADPCM_IMA_OKI,
    AV_CODEC_ID_ADPCM_DTK,
    AV_CODEC_ID_ADPCM_IMA_RAD,
    AV_CODEC_ID_ADPCM_G726LE,
    AV_CODEC_ID_ADPCM_THP_LE,
    AV_CODEC_ID_ADPCM_PSX,
    AV_CODEC_ID_ADPCM_AICA,
    AV_CODEC_ID_ADPCM_IMA_DAT4,
    AV_CODEC_ID_ADPCM_MTAF,

    (* AMR *)
    AV_CODEC_ID_AMR_NB = $12000,
    AV_CODEC_ID_AMR_WB,

    (* RealAudio codecs*)
    AV_CODEC_ID_RA_144 = $13000,
    AV_CODEC_ID_RA_288,

    (* various DPCM codecs *)
    AV_CODEC_ID_ROQ_DPCM = $14000,
    AV_CODEC_ID_INTERPLAY_DPCM,
    AV_CODEC_ID_XAN_DPCM,
    AV_CODEC_ID_SOL_DPCM,

    AV_CODEC_ID_SDX2_DPCM = $14800,

    (* audio codecs *)
    AV_CODEC_ID_MP2 = $15000,
    AV_CODEC_ID_MP3,          ///< preferred ID for decoding MPEG audio layer 1, 2 or 3
    AV_CODEC_ID_AAC,
    AV_CODEC_ID_AC3,
    AV_CODEC_ID_DTS,
    AV_CODEC_ID_VORBIS,
    AV_CODEC_ID_DVAUDIO,
    AV_CODEC_ID_WMAV1,
    AV_CODEC_ID_WMAV2,
    AV_CODEC_ID_MACE3,
    AV_CODEC_ID_MACE6,
    AV_CODEC_ID_VMDAUDIO,
    AV_CODEC_ID_FLAC,
    AV_CODEC_ID_MP3ADU,
    AV_CODEC_ID_MP3ON4,
    AV_CODEC_ID_SHORTEN,
    AV_CODEC_ID_ALAC,
    AV_CODEC_ID_WESTWOOD_SND1,
    AV_CODEC_ID_GSM,           ///< as in Berlin toast format
    AV_CODEC_ID_QDM2,
    AV_CODEC_ID_COOK,
    AV_CODEC_ID_TRUESPEECH,
    AV_CODEC_ID_TTA,
    AV_CODEC_ID_SMACKAUDIO,
    AV_CODEC_ID_QCELP,
    AV_CODEC_ID_WAVPACK,
    AV_CODEC_ID_DSICINAUDIO,
    AV_CODEC_ID_IMC,
    AV_CODEC_ID_MUSEPACK7,
    AV_CODEC_ID_MLP,
    AV_CODEC_ID_GSM_MS, (* as found in WAV *)
    AV_CODEC_ID_ATRAC3,
{$IFDEF FF_API_VOXWARE}
    AV_CODEC_ID_VOXWARE,
{$ENDIF}
    AV_CODEC_ID_APE,
    AV_CODEC_ID_NELLYMOSER,
    AV_CODEC_ID_MUSEPACK8,
    AV_CODEC_ID_SPEEX,
    AV_CODEC_ID_WMAVOICE,
    AV_CODEC_ID_WMAPRO,
    AV_CODEC_ID_WMALOSSLESS,
    AV_CODEC_ID_ATRAC3P,
    AV_CODEC_ID_EAC3,
    AV_CODEC_ID_SIPR,
    AV_CODEC_ID_MP1,
    AV_CODEC_ID_TWINVQ,
    AV_CODEC_ID_TRUEHD,
    AV_CODEC_ID_MP4ALS,
    AV_CODEC_ID_ATRAC1,
    AV_CODEC_ID_BINKAUDIO_RDFT,
    AV_CODEC_ID_BINKAUDIO_DCT,
    AV_CODEC_ID_AAC_LATM,
    AV_CODEC_ID_QDMC,
    AV_CODEC_ID_CELT,
    AV_CODEC_ID_G723_1,
    AV_CODEC_ID_G729,
    AV_CODEC_ID_8SVX_EXP,
    AV_CODEC_ID_8SVX_FIB,
    AV_CODEC_ID_BMV_AUDIO,
    AV_CODEC_ID_RALF,
    AV_CODEC_ID_IAC,
    AV_CODEC_ID_ILBC,
    AV_CODEC_ID_OPUS,
    AV_CODEC_ID_COMFORT_NOISE,
    AV_CODEC_ID_TAK,
    AV_CODEC_ID_METASOUND,
    AV_CODEC_ID_PAF_AUDIO,
    AV_CODEC_ID_ON2AVC,
    AV_CODEC_ID_DSS_SP,

    AV_CODEC_ID_FFWAVESYNTH = $15800,
    AV_CODEC_ID_SONIC,
    AV_CODEC_ID_SONIC_LS,
    AV_CODEC_ID_EVRC,
    AV_CODEC_ID_SMV,
    AV_CODEC_ID_DSD_LSBF,
    AV_CODEC_ID_DSD_MSBF,
    AV_CODEC_ID_DSD_LSBF_PLANAR,
    AV_CODEC_ID_DSD_MSBF_PLANAR,
    AV_CODEC_ID_4GV,
    AV_CODEC_ID_INTERPLAY_ACM,
    AV_CODEC_ID_XMA1,
    AV_CODEC_ID_XMA2,
    AV_CODEC_ID_DST,
    AV_CODEC_ID_ATRAC3AL,
    AV_CODEC_ID_ATRAC3PAL,

    (* subtitle codecs *)
{$IFNDEF FPC}
    AV_CODEC_ID_FIRST_SUBTITLE = $17000,          ///< A dummy ID pointing at the start of subtitle codecs.
{$ENDIF}
    AV_CODEC_ID_DVD_SUBTITLE = $17000,
    AV_CODEC_ID_DVB_SUBTITLE,
    AV_CODEC_ID_TEXT,          ///< raw UTF-8 text
    AV_CODEC_ID_XSUB,
    AV_CODEC_ID_SSA,
    AV_CODEC_ID_MOV_TEXT,
    AV_CODEC_ID_HDMV_PGS_SUBTITLE,
    AV_CODEC_ID_DVB_TELETEXT,
    AV_CODEC_ID_SRT,

    AV_CODEC_ID_MICRODVD   = $17800,
    AV_CODEC_ID_EIA_608,
    AV_CODEC_ID_JACOSUB,
    AV_CODEC_ID_SAMI,
    AV_CODEC_ID_REALTEXT,
    AV_CODEC_ID_STL,
    AV_CODEC_ID_SUBVIEWER1,
    AV_CODEC_ID_SUBVIEWER,
    AV_CODEC_ID_SUBRIP,
    AV_CODEC_ID_WEBVTT,
    AV_CODEC_ID_MPL2,
    AV_CODEC_ID_VPLAYER,
    AV_CODEC_ID_PJS,
    AV_CODEC_ID_ASS,
    AV_CODEC_ID_HDMV_TEXT_SUBTITLE,

    (* other specific kind of codecs (generally used for attachments) *)
{$IFNDEF FPC}
    AV_CODEC_ID_FIRST_UNKNOWN = $18000,           ///< A dummy ID pointing at the start of various fake codecs.
{$ENDIF}
    AV_CODEC_ID_TTF = $18000,

    AV_CODEC_ID_SCTE_35, ///< Contain timestamp estimated through PCR of program stream.
    AV_CODEC_ID_BINTEXT    = $18800,
    AV_CODEC_ID_XBIN,
    AV_CODEC_ID_IDF,
    AV_CODEC_ID_OTF,
    AV_CODEC_ID_SMPTE_KLV,
    AV_CODEC_ID_DVD_NAV,
    AV_CODEC_ID_TIMED_ID3,
    AV_CODEC_ID_BIN_DATA,

    AV_CODEC_ID_PROBE = $19000, ///< codec_id is not known (like AV_CODEC_ID_NONE) but lavf should attempt to identify it

    AV_CODEC_ID_MPEG2TS = $20000, (**< _FAKE_ codec to indicate a raw MPEG-2 TS
                                * stream (only used by libavformat) *)
    AV_CODEC_ID_MPEG4SYSTEMS = $20001, (**< _FAKE_ codec to indicate a MPEG-4 Systems
                                * stream (only used by libavformat) *)
    AV_CODEC_ID_FFMETADATA = $21000,   ///< Dummy codec for streams containing only metadata information.
    AV_CODEC_ID_WRAPPED_AVFRAME = $21001 ///< Passthrough codec, AVFrames wrapped in AVPacket
  );
{$IFEND}

const
  AV_CODEC_ID_H265 = AV_CODEC_ID_HEVC;
  AV_CODEC_ID_IFF_BYTERUN1 = AV_CODEC_ID_IFF_ILBM;

{$IFDEF FF_API_VIMA_DECODER}
  AV_CODEC_ID_VIMA=AV_CODEC_ID_ADPCM_VIMA;
{$ENDIF}
{$IFDEF FPC}
  AV_CODEC_ID_FIRST_UNKNOWN = AV_CODEC_ID_TTF;           ///< A dummy ID pointing at the start of various fake codecs.
{$ENDIF}

(**
 * This struct describes the properties of a single codec described by an
 * AVCodecID.
 * @see avcodec_descriptor_get()
 *)
type
(**
 * AVProfile.
 *)
  PAVProfile = ^TAVProfile;
  TAVProfile = record
    profile: Integer;
    name: PAnsiChar; ///< short name for the profile
  end;

  PAVCodecDescriptor = ^TAVCodecDescriptor;
  TAVCodecDescriptor = record
    id: TAVCodecID;
    ttype: TAVMediaType;
    (**
     * Name of the codec described by this descriptor. It is non-empty and
     * unique for each codec descriptor. It should contain alphanumeric
     * characters and '_' only.
     *)
    name: PAnsiChar;
    (**
     * A more descriptive name for this codec. May be NULL.
     *)
    long_name: PAnsiChar;
    (**
     * Codec properties, a combination of AV_CODEC_PROP_* flags.
     *)
    props: Integer;
    (**
     * MIME type(s) associated with the codec.
     * May be NULL; if not, a NULL-terminated array of MIME types.
     * The first item is always non-NULL and is the preferred MIME type.
     *)
    //const char *const *mime_types;
    mime_types: PPAnsiChar;
    (**
     * If non-NULL, an array of profiles recognized for this codec.
     * Terminated with FF_PROFILE_UNKNOWN.
     *)
    profiles: PAVProfile;
  end;

const
(**
 * Codec uses only intra compression.
 * Video codecs only.
 *)
  AV_CODEC_PROP_INTRA_ONLY    = (1 shl 0);
(**
 * Codec supports lossy compression. Audio and video codecs only.
 * @note a codec may support both lossy and lossless
 * compression modes
 *)
  AV_CODEC_PROP_LOSSY         = (1 shl 1);
(**
 * Codec supports lossless compression. Audio and video codecs only.
 *)
  AV_CODEC_PROP_LOSSLESS      = (1 shl 2);
(**
 * Codec supports frame reordering. That is, the coded order (the order in which
 * the encoded packets are output by the encoders / stored / input to the
 * decoders) may be different from the presentation order of the corresponding
 * frames.
 *
 * For codecs that do not have this property set, PTS and DTS should always be
 * equal.
 *)
  AV_CODEC_PROP_REORDER       = (1 shl 3);
(**
 * Subtitle codec is bitmap based
 * Decoded AVSubtitle data can be read from the AVSubtitleRect->pict field.
 *)
  AV_CODEC_PROP_BITMAP_SUB    = (1 shl 16);
(**
 * Subtitle codec is text based.
 * Decoded AVSubtitle data can be read from the AVSubtitleRect->ass field.
 *)
  AV_CODEC_PROP_TEXT_SUB      = (1 shl 17);

(**
 * @ingroup lavc_decoding
 * Required number of additionally allocated bytes at the end of the input bitstream for decoding.
 * This is mainly needed because some optimized bitstream readers read
 * 32 or 64 bit at once and could read over the end.<br>
 * Note: If the first 23 bits of the additional bytes are not 0, then damaged
 * MPEG bitstreams could cause overread and segfault.
 *)
  AV_INPUT_BUFFER_PADDING_SIZE = 32;

(**
 * @ingroup lavc_encoding
 * minimum encoding buffer size
 * Used to avoid some checks during header writing.
 *)
  AV_INPUT_BUFFER_MIN_SIZE = 16384;

{$IFDEF FF_API_WITHOUT_PREFIX}
(**
 * @deprecated use AV_INPUT_BUFFER_PADDING_SIZE instead
 *)
  FF_INPUT_BUFFER_PADDING_SIZE = 32;

(**
 * @deprecated use AV_INPUT_BUFFER_MIN_SIZE instead
 *)
  FF_MIN_BUFFER_SIZE = 16384;
{$ENDIF} (* FF_API_WITHOUT_PREFIX *)

(**
 * @ingroup lavc_encoding
 * motion estimation type.
 * @deprecated use codec private option instead
 *)
type
{$IFDEF FF_API_MOTION_EST}
{$IF Defined(BCB) and Defined(VER140)} // C++Builder 6
  TMotion_Est_ID = Integer;
{$ELSE}
  TMotion_Est_ID = (
    ME_ZERO = 1,    ///< no search, that is use 0,0 vector whenever one is needed
    ME_FULL,
    ME_LOG,
    ME_PHODS,
    ME_EPZS,        ///< enhanced predictive zonal search
    ME_X1,          ///< reserved for experiments
    ME_HEX,         ///< hexagon based search
    ME_UMH,         ///< uneven multi-hexagon search
    ME_TESA,        ///< transformed exhaustive search algorithm
    ME_ITER=50      ///< iterative search
  );
{$IFEND}
{$ENDIF}

(**
 * @ingroup lavc_decoding
 *)
{$IF Defined(BCB) and Defined(VER140)} // C++Builder 6
  TAVDiscard = Integer;
{$ELSE}
  TAVDiscard = (
    (* We leave some space between them for extensions (drop some
     * keyframes for intra-only or drop just some bidir frames). *)
    AVDISCARD_NONE    =-16, ///< discard nothing
    AVDISCARD_DEFAULT =  0, ///< discard useless packets like 0 size packets in avi
    AVDISCARD_NONREF  =  8, ///< discard all non reference
    AVDISCARD_BIDIR   = 16, ///< discard all bidirectional frames
    AVDISCARD_NONINTRA= 24, ///< discard all non intra frames
    AVDISCARD_NONKEY  = 32, ///< discard all frames except keyframes
    AVDISCARD_ALL     = 48  ///< discard all
  );
{$IFEND}

{$IF Defined(BCB) and Defined(VER140)} // C++Builder 6
  TAVAudioServiceType = Integer;
{$ELSE}
  TAVAudioServiceType = (
    AV_AUDIO_SERVICE_TYPE_MAIN              = 0,
    AV_AUDIO_SERVICE_TYPE_EFFECTS           = 1,
    AV_AUDIO_SERVICE_TYPE_VISUALLY_IMPAIRED = 2,
    AV_AUDIO_SERVICE_TYPE_HEARING_IMPAIRED  = 3,
    AV_AUDIO_SERVICE_TYPE_DIALOGUE          = 4,
    AV_AUDIO_SERVICE_TYPE_COMMENTARY        = 5,
    AV_AUDIO_SERVICE_TYPE_EMERGENCY         = 6,
    AV_AUDIO_SERVICE_TYPE_VOICE_OVER        = 7,
    AV_AUDIO_SERVICE_TYPE_KARAOKE           = 8,
    AV_AUDIO_SERVICE_TYPE_NB                     ///< Not part of ABI
  );
{$IFEND}

(**
 * @ingroup lavc_encoding
 *)
  PRcOverride = ^TRcOverride;
  TRcOverride = record // SizeOf = 16
    start_frame: Integer;
    end_frame: Integer;
    qscale: Integer; // If this is 0 then quality_factor will be used instead.
    quality_factor: Single;
  end;

const
{$IFDEF FF_API_MAX_BFRAMES}
(**
 * @deprecated there is no libavcodec-wide limit on the number of B-frames
 *)
  FF_MAX_B_FRAMES = 16;
{$ENDIF}

(* encoding support
   These flags can be passed in AVCodecContext.flags before initialization.
   Note: Not everything is supported yet.
*)

(**
 * Allow decoders to produce frames with data planes that are not aligned
 * to CPU requirements (e.g. due to cropping).
 *)
  AV_CODEC_FLAG_UNALIGNED       = (1 shl  0);
(**
 * Use fixed qscale.
 *)
  AV_CODEC_FLAG_QSCALE          = (1 shl  1);
(**
 * 4 MV per MB allowed / advanced prediction for H.263.
 *)
  AV_CODEC_FLAG_4MV             = (1 shl  2);
(**
 * Output even those frames that might be corrupted.
 *)
  AV_CODEC_FLAG_OUTPUT_CORRUPT  = (1 shl  3);
(**
 * Use qpel MC.
 *)
  AV_CODEC_FLAG_QPEL            = (1 shl  4);
(**
 * Use internal 2pass ratecontrol in first pass mode.
 *)
  AV_CODEC_FLAG_PASS1           = (1 shl  9);
(**
 * Use internal 2pass ratecontrol in second pass mode.
 *)
  AV_CODEC_FLAG_PASS2           = (1 shl 10);
(**
 * loop filter.
 *)
  AV_CODEC_FLAG_LOOP_FILTER     = (1 shl 11);
(**
 * Only decode/encode grayscale.
 *)
  AV_CODEC_FLAG_GRAY            = (1 shl 13);
(**
 * error[?] variables will be set during encoding.
 *)
  AV_CODEC_FLAG_PSNR            = (1 shl 15);
(**
 * Input bitstream might be truncated at a random location
 * instead of only at frame boundaries.
 *)
  AV_CODEC_FLAG_TRUNCATED       = (1 shl 16);
(**
 * Use interlaced DCT.
 *)
  AV_CODEC_FLAG_INTERLACED_DCT  = (1 shl 18);
(**
 * Force low delay.
 *)
  AV_CODEC_FLAG_LOW_DELAY       = (1 shl 19);
(**
 * Place global headers in extradata instead of every keyframe.
 *)
  AV_CODEC_FLAG_GLOBAL_HEADER   = (1 shl 22);
(**
 * Use only bitexact stuff (except (I)DCT).
 *)
  AV_CODEC_FLAG_BITEXACT        = (1 shl 23);
(* Fx : Flag for H.263+ extra options *)
(**
 * H.263 advanced intra coding / MPEG-4 AC prediction
 *)
  AV_CODEC_FLAG_AC_PRED         = (1 shl 24);
(**
 * interlaced motion estimation
 *)
  AV_CODEC_FLAG_INTERLACED_ME   = (1 shl 29);
  AV_CODEC_FLAG_CLOSED_GOP      = (1 shl 31);

(**
 * Allow non spec compliant speedup tricks.
 *)
  AV_CODEC_FLAG2_FAST           = (1 shl  0);
(**
 * Skip bitstream encoding.
 *)
  AV_CODEC_FLAG2_NO_OUTPUT      = (1 shl  2);
(**
 * Place global headers at every keyframe instead of in extradata.
 *)
  AV_CODEC_FLAG2_LOCAL_HEADER   = (1 shl  3);

(**
 * timecode is in drop frame format. DEPRECATED!!!!
 *)
  AV_CODEC_FLAG2_DROP_FRAME_TIMECODE = (1 shl 13);

(**
 * Input bitstream might be truncated at a packet boundaries
 * instead of only at frame boundaries.
 *)
  AV_CODEC_FLAG2_CHUNKS         = (1 shl 15);
(**
 * Discard cropping information from SPS.
 *)
  AV_CODEC_FLAG2_IGNORE_CROP    = (1 shl 16);

(**
 * Show all frames before the first keyframe
 *)
  AV_CODEC_FLAG2_SHOW_ALL       = (1 shl 22);
(**
 * Export motion vectors through frame side data
 *)
  AV_CODEC_FLAG2_EXPORT_MVS     = (1 shl 28);
(**
 * Do not skip samples and export skip information as frame side data
 *)
  AV_CODEC_FLAG2_SKIP_MANUAL    = (1 shl 29);
(**
 * Do not reset ASS ReadOrder field on flush (subtitles decoding)
 *)
  AV_CODEC_FLAG2_RO_FLUSH_NOOP  = (1 shl 30);

(* Unsupported options :
 *              Syntax Arithmetic coding (SAC)
 *              Reference Picture Selection
 *              Independent Segment Decoding *)
(* /Fx *)
(* codec capabilities *)

(**
 * Decoder can use draw_horiz_band callback.
 *)
  AV_CODEC_CAP_DRAW_HORIZ_BAND     = (1 shl  0);
(**
 * Codec uses get_buffer() for allocating buffers and supports custom allocators.
 * If not set, it might not use get_buffer() at all or use operations that
 * assume the buffer was allocated by avcodec_default_get_buffer.
 *)
  AV_CODEC_CAP_DR1                 = (1 shl  1);
  AV_CODEC_CAP_TRUNCATED           = (1 shl  3);
(**
 * Encoder or decoder requires flushing with NULL input at the end in order to
 * give the complete and correct output.
 *
 * NOTE: If this flag is not set, the codec is guaranteed to never be fed with
 *       with NULL data. The user can still send NULL data to the public encode
 *       or decode function, but libavcodec will not pass it along to the codec
 *       unless this flag is set.
 *
 * Decoders:
 * The decoder has a non-zero delay and needs to be fed with avpkt->data=NULL,
 * avpkt->size=0 at the end to get the delayed data until the decoder no longer
 * returns frames.
 *
 * Encoders:
 * The encoder needs to be fed with NULL data at the end of encoding until the
 * encoder no longer returns data.
 *
 * NOTE: For encoders implementing the AVCodec.encode2() function, setting this
 *       flag also means that the encoder must set the pts and duration for
 *       each output packet. If this flag is not set, the pts and duration will
 *       be determined by libavcodec from the input frame.
 *)
  AV_CODEC_CAP_DELAY               = (1 shl  5);
(**
 * Codec can be fed a final frame with a smaller size.
 * This can be used to prevent truncation of the last audio samples.
 *)
  AV_CODEC_CAP_SMALL_LAST_FRAME    = (1 shl  6);

{$IFDEF FF_API_CAP_VDPAU}
(**
 * Codec can export data for HW decoding (VDPAU).
 *)
  AV_CODEC_CAP_HWACCEL_VDPAU       = (1 shl  7);
{$ENDIF}

(**
 * Codec can output multiple frames per AVPacket
 * Normally demuxers return one frame at a time, demuxers which do not do
 * are connected to a parser to split what they return into proper frames.
 * This flag is reserved to the very rare category of codecs which have a
 * bitstream that cannot be split into frames without timeconsuming
 * operations like full decoding. Demuxers carrying such bitstreams thus
 * may return multiple frames in a packet. This has many disadvantages like
 * prohibiting stream copy in many cases thus it should only be considered
 * as a last resort.
 *)
  AV_CODEC_CAP_SUBFRAMES           = (1 shl  8);
(**
 * Codec is experimental and is thus avoided in favor of non experimental
 * encoders
 *)
  AV_CODEC_CAP_EXPERIMENTAL        = (1 shl  9);
(**
 * Codec should fill in channel configuration and samplerate instead of container
 *)
  AV_CODEC_CAP_CHANNEL_CONF        = (1 shl 10);
(**
 * Codec supports frame-level multithreading.
 *)
  AV_CODEC_CAP_FRAME_THREADS       = (1 shl 12);
(**
 * Codec supports slice-based (or partition-based) multithreading.
 *)
  AV_CODEC_CAP_SLICE_THREADS       = (1 shl 13);
(**
 * Codec supports changed parameters at any point.
 *)
  AV_CODEC_CAP_PARAM_CHANGE        = (1 shl 14);
(**
 * Codec supports avctx->thread_count == 0 (auto).
 *)
  AV_CODEC_CAP_AUTO_THREADS        = (1 shl 15);
(**
 * Audio encoder supports receiving a different number of samples in each call.
 *)
  AV_CODEC_CAP_VARIABLE_FRAME_SIZE = (1 shl 16);
(**
 * Decoder is not a preferred choice for probing.
 * This indicates that the decoder is not a good choice for probing.
 * It could for example be an expensive to spin up hardware decoder,
 * or it could simply not provide a lot of useful information about
 * the stream.
 * A decoder marked with this flag should only be used as last resort
 * choice for probing.
 *)
  AV_CODEC_CAP_AVOID_PROBING       = (1 shl 17);
(**
 * Codec is intra only.
 *)
  AV_CODEC_CAP_INTRA_ONLY       = $40000000;
(**
 * Codec is lossless.
 *)
  AV_CODEC_CAP_LOSSLESS         = $80000000;


{$IFDEF FF_API_WITHOUT_PREFIX}
(**
 * Allow decoders to produce frames with data planes that are not aligned
 * to CPU requirements (e.g. due to cropping).
 *)
  CODEC_FLAG_UNALIGNED = AV_CODEC_FLAG_UNALIGNED;
  CODEC_FLAG_QSCALE = AV_CODEC_FLAG_QSCALE;
  CODEC_FLAG_4MV    = AV_CODEC_FLAG_4MV;
  CODEC_FLAG_OUTPUT_CORRUPT = AV_CODEC_FLAG_OUTPUT_CORRUPT;
  CODEC_FLAG_QPEL   = AV_CODEC_FLAG_QPEL;
{$IFDEF FF_API_GMC}
(**
 * @deprecated use the "gmc" private option of the libxvid encoder
 *)
  CODEC_FLAG_GMC    = $0020;  ///< Use GMC.
{$ENDIF}
{$IFDEF FF_API_MV0}
(**
 * @deprecated use the flag "mv0" in the "mpv_flags" private option of the
 * mpegvideo encoders
 *)
  CODEC_FLAG_MV0    = $0040;
{$ENDIF}
{$IFDEF FF_API_INPUT_PRESERVED}
(**
 * @deprecated passing reference-counted frames to the encoders replaces this
 * flag
 *)
  CODEC_FLAG_INPUT_PRESERVED = $0100;
{$ENDIF}
  CODEC_FLAG_PASS1           = AV_CODEC_FLAG_PASS1;
  CODEC_FLAG_PASS2           = AV_CODEC_FLAG_PASS2;
  CODEC_FLAG_GRAY            = AV_CODEC_FLAG_GRAY;
{$IFDEF FF_API_EMU_EDGE}
(**
 * @deprecated edges are not used/required anymore. I.e. this flag is now always
 * set.
 *)
  CODEC_FLAG_EMU_EDGE        = $4000;
{$ENDIF}
  CODEC_FLAG_PSNR            = AV_CODEC_FLAG_PSNR;
  CODEC_FLAG_TRUNCATED       = AV_CODEC_FLAG_TRUNCATED;

{$IFDEF FF_API_NORMALIZE_AQP}
(**
 * @deprecated use the flag "naq" in the "mpv_flags" private option of the
 * mpegvideo encoders
 *)
  CODEC_FLAG_NORMALIZE_AQP  = $00020000;
{$ENDIF}
  CODEC_FLAG_INTERLACED_DCT = AV_CODEC_FLAG_INTERLACED_DCT;
  CODEC_FLAG_LOW_DELAY      = AV_CODEC_FLAG_LOW_DELAY;
  CODEC_FLAG_GLOBAL_HEADER  = AV_CODEC_FLAG_GLOBAL_HEADER;
  CODEC_FLAG_BITEXACT       = AV_CODEC_FLAG_BITEXACT;
  CODEC_FLAG_AC_PRED        = AV_CODEC_FLAG_AC_PRED;
  CODEC_FLAG_LOOP_FILTER    = AV_CODEC_FLAG_LOOP_FILTER;
  CODEC_FLAG_INTERLACED_ME  = AV_CODEC_FLAG_INTERLACED_ME;
  CODEC_FLAG_CLOSED_GOP     = AV_CODEC_FLAG_CLOSED_GOP;
  CODEC_FLAG2_FAST          = AV_CODEC_FLAG2_FAST;
  CODEC_FLAG2_NO_OUTPUT     = AV_CODEC_FLAG2_NO_OUTPUT;
  CODEC_FLAG2_LOCAL_HEADER  = AV_CODEC_FLAG2_LOCAL_HEADER;
  CODEC_FLAG2_DROP_FRAME_TIMECODE = AV_CODEC_FLAG2_DROP_FRAME_TIMECODE;
  CODEC_FLAG2_IGNORE_CROP   = AV_CODEC_FLAG2_IGNORE_CROP;

  CODEC_FLAG2_CHUNKS        = AV_CODEC_FLAG2_CHUNKS;
  CODEC_FLAG2_SHOW_ALL      = AV_CODEC_FLAG2_SHOW_ALL;
  CODEC_FLAG2_EXPORT_MVS    = AV_CODEC_FLAG2_EXPORT_MVS;
  CODEC_FLAG2_SKIP_MANUAL   = AV_CODEC_FLAG2_SKIP_MANUAL;

(* Unsupported options :
 *              Syntax Arithmetic coding (SAC)
 *              Reference Picture Selection
 *              Independent Segment Decoding *)
(* /Fx *)
(* codec capabilities *)

  CODEC_CAP_DRAW_HORIZ_BAND = AV_CODEC_CAP_DRAW_HORIZ_BAND; ///< Decoder can use draw_horiz_band callback.
(**
 * Codec uses get_buffer() for allocating buffers and supports custom allocators.
 * If not set, it might not use get_buffer() at all or use operations that
 * assume the buffer was allocated by avcodec_default_get_buffer.
 *)
  CODEC_CAP_DR1             = AV_CODEC_CAP_DR1;
  CODEC_CAP_TRUNCATED       = AV_CODEC_CAP_TRUNCATED;
{$IFDEF FF_API_XVMC}
(* Codec can export data for HW decoding. This flag indicates that
 * the codec would call get_format() with list that might contain HW accelerated
 * pixel formats (XvMC, VDPAU, VAAPI, etc). The application can pick any of them
 * including raw image format.
 * The application can use the passed context to determine bitstream version,
 * chroma format, resolution etc.
 *)
  CODEC_CAP_HWACCEL         = $0010;
{$ENDIF}
(**
 * Encoder or decoder requires flushing with NULL input at the end in order to
 * give the complete and correct output.
 *
 * NOTE: If this flag is not set, the codec is guaranteed to never be fed with
 *       with NULL data. The user can still send NULL data to the public encode
 *       or decode function, but libavcodec will not pass it along to the codec
 *       unless this flag is set.
 *
 * Decoders:
 * The decoder has a non-zero delay and needs to be fed with avpkt->data=NULL,
 * avpkt->size=0 at the end to get the delayed data until the decoder no longer
 * returns frames.
 *
 * Encoders:
 * The encoder needs to be fed with NULL data at the end of encoding until the
 * encoder no longer returns data.
 *
 * NOTE: For encoders implementing the AVCodec.encode2() function, setting this
 *       flag also means that the encoder must set the pts and duration for
 *       each output packet. If this flag is not set, the pts and duration will
 *       be determined by libavcodec from the input frame.
 *)
  CODEC_CAP_DELAY           = AV_CODEC_CAP_DELAY;
(**
 * Codec can be fed a final frame with a smaller size.
 * This can be used to prevent truncation of the last audio samples.
 *)
  CODEC_CAP_SMALL_LAST_FRAME = AV_CODEC_CAP_SMALL_LAST_FRAME;
{$IFDEF FF_API_CAP_VDPAU}
(**
 * Codec can export data for HW decoding (VDPAU).
 *)
  CODEC_CAP_HWACCEL_VDPAU    = AV_CODEC_CAP_HWACCEL_VDPAU;
{$ENDIF}
(**
 * Codec can output multiple frames per AVPacket
 * Normally demuxers return one frame at a time, demuxers which do not do
 * are connected to a parser to split what they return into proper frames.
 * This flag is reserved to the very rare category of codecs which have a
 * bitstream that cannot be split into frames without timeconsuming
 * operations like full decoding. Demuxers carrying such bitstreams thus
 * may return multiple frames in a packet. This has many disadvantages like
 * prohibiting stream copy in many cases thus it should only be considered
 * as a last resort.
 *)
  CODEC_CAP_SUBFRAMES        = AV_CODEC_CAP_SUBFRAMES;
(**
 * Codec is experimental and is thus avoided in favor of non experimental
 * encoders
 *)
  CODEC_CAP_EXPERIMENTAL     = AV_CODEC_CAP_EXPERIMENTAL;
(**
 * Codec should fill in channel configuration and samplerate instead of container
 *)
  CODEC_CAP_CHANNEL_CONF     = AV_CODEC_CAP_CHANNEL_CONF;
{$IFDEF FF_API_NEG_LINESIZES}
(**
 * @deprecated no codecs use this capability
 *)
  CODEC_CAP_NEG_LINESIZES    = $0800;
{$ENDIF}
(**
 * Codec supports frame-level multithreading.
 *)
  CODEC_CAP_FRAME_THREADS    = AV_CODEC_CAP_FRAME_THREADS;
(**
 * Codec supports slice-based (or partition-based) multithreading.
 *)
  CODEC_CAP_SLICE_THREADS    = AV_CODEC_CAP_SLICE_THREADS;
(**
 * Codec supports changed parameters at any point.
 *)
  CODEC_CAP_PARAM_CHANGE     = AV_CODEC_CAP_PARAM_CHANGE;
(**
 * Codec supports avctx->thread_count == 0 (auto).
 *)
  CODEC_CAP_AUTO_THREADS     = AV_CODEC_CAP_AUTO_THREADS;
(**
 * Audio encoder supports receiving a different number of samples in each call.
 *)
  CODEC_CAP_VARIABLE_FRAME_SIZE = AV_CODEC_CAP_VARIABLE_FRAME_SIZE;
(**
 * Codec is intra only.
 *)
  CODEC_CAP_INTRA_ONLY       = AV_CODEC_CAP_INTRA_ONLY;
(**
 * Codec is lossless.
 *)
  CODEC_CAP_LOSSLESS         = AV_CODEC_CAP_LOSSLESS;

(**
 * HWAccel is experimental and is thus avoided in favor of non experimental
 * codecs
 *)
  HWACCEL_CODEC_CAP_EXPERIMENTAL     = $0200;
{$ENDIF} (* FF_API_WITHOUT_PREFIX *)

{$IFDEF FF_API_MB_TYPE}
//The following defines may change, don't expect compatibility if you use them.
  MB_TYPE_INTRA4x4   = $0001;
  MB_TYPE_INTRA16x16 = $0002; //FIXME H.264-specific
  MB_TYPE_INTRA_PCM  = $0004; //FIXME H.264-specific
  MB_TYPE_16x16      = $0008;
  MB_TYPE_16x8       = $0010;
  MB_TYPE_8x16       = $0020;
  MB_TYPE_8x8        = $0040;
  MB_TYPE_INTERLACED = $0080;
  MB_TYPE_DIRECT2    = $0100; //FIXME
  MB_TYPE_ACPRED     = $0200;
  MB_TYPE_GMC        = $0400;
  MB_TYPE_SKIP       = $0800;
  MB_TYPE_P0L0       = $1000;
  MB_TYPE_P1L0       = $2000;
  MB_TYPE_P0L1       = $4000;
  MB_TYPE_P1L1       = $8000;
  MB_TYPE_L0         = (MB_TYPE_P0L0 or MB_TYPE_P1L0);
  MB_TYPE_L1         = (MB_TYPE_P0L1 or MB_TYPE_P1L1);
  MB_TYPE_L0L1       = (MB_TYPE_L0   or MB_TYPE_L1);
  MB_TYPE_QUANT      = $00010000;
  MB_TYPE_CBP        = $00020000;
// Note bits 24-31 are reserved for codec specific use (H.264 ref0, MPEG-1 0mv, ...)
{$ENDIF}

(**
 * Pan Scan area.
 * This specifies the area which should be displayed.
 * Note there may be multiple such areas for one frame.
 *)
type
  PAVPanScan = ^TAVPanScan;
  TAVPanScan = record
    (**
     * id
     * - encoding: Set by user.
     * - decoding: Set by libavcodec.
     *)
    id: Integer;

    (**
     * width and height in 1/16 pel
     * - encoding: Set by user.
     * - decoding: Set by libavcodec.
     *)
    width: Integer;
    height: Integer;

    (**
     * position of the top left corner in 1/16 pel for up to 3 fields/frames
     * - encoding: Set by user.
     * - decoding: Set by libavcodec.
     *)
    position: array[0..2] of array[0..1] of SmallInt; // int16_t
  end;

(**
 * This structure describes the bitrate properties of an encoded bitstream. It
 * roughly corresponds to a subset the VBV parameters for MPEG-2 or HRD
 * parameters for H.264/HEVC.
 *)
  PAVCPBProperties = ^TAVCPBProperties;
  TAVCPBProperties = record
    (**
     * Maximum bitrate of the stream, in bits per second.
     * Zero if unknown or unspecified.
     *)
    max_bitrate: Integer;
    (**
     * Minimum bitrate of the stream, in bits per second.
     * Zero if unknown or unspecified.
     *)
    min_bitrate: Integer;
    (**
     * Average bitrate of the stream, in bits per second.
     * Zero if unknown or unspecified.
     *)
    avg_bitrate: Integer;

    (**
     * The size of the buffer to which the ratecontrol is applied, in bits.
     * Zero if unknown or unspecified.
     *)
    buffer_size: Integer;

    (**
     * The delay between the time the packet this structure is associated with
     * is received and the time when it should be decoded, in periods of a 27MHz
     * clock.
     *
     * UINT64_MAX when unknown or unspecified.
     *)
    vbv_delay: Int64;
  end;

const
{$IFDEF FF_API_QSCALE_TYPE}
  FF_QSCALE_TYPE_MPEG1 = 0;
  FF_QSCALE_TYPE_MPEG2 = 1;
  FF_QSCALE_TYPE_H264  = 2;
  FF_QSCALE_TYPE_VP56  = 3;
{$ENDIF}

(**
 * The decoder will keep a reference to the frame and may reuse it later.
 *)
  AV_GET_BUFFER_FLAG_REF = (1 shl 0);

  AV_PKT_FLAG_KEY     = $0001; ///< The packet contains a keyframe
  AV_PKT_FLAG_CORRUPT = $0002; ///< The packet content is corrupted
(**
 * Flag is used to discard packets which are required to maintain valid
 * decoder state but are not required for output and should be dropped
 * after decoding.
 **)
  AV_PKT_FLAG_DISCARD = $0004;

{$IFDEF FF_API_ASPECT_EXTENDED}
  // const for TAVCodecContext
  // AVCodecContext.gop_size
  FF_ASPECT_EXTENDED = 15;
{$ENDIF}

  // AVCodecContext.rc_strategy
  FF_RC_STRATEGY_XVID = 1;

  // AVCodecContext.workaround_bugs
  FF_BUG_AUTODETECT       = 1;  ///< autodetection
{$IFDEF FF_API_OLD_MSMPEG4}
  FF_BUG_OLD_MSMPEG4      = 2;
{$ENDIF}
  FF_BUG_XVID_ILACE       = 4;
  FF_BUG_UMP4             = 8;
  FF_BUG_NO_PADDING       = 16;
  FF_BUG_AMV              = 32;
{$IFDEF FF_API_AC_VLC}
  FF_BUG_AC_VLC           = 0;  ///< Will be removed, libavcodec can now handle these non-compliant files by default.
{$ENDIF}
  FF_BUG_QPEL_CHROMA      = 64;
  FF_BUG_STD_QPEL         = 128;
  FF_BUG_QPEL_CHROMA2     = 256;
  FF_BUG_DIRECT_BLOCKSIZE = 512;
  FF_BUG_EDGE             = 1024;
  FF_BUG_HPEL_CHROMA      = 2048;
  FF_BUG_DC_CLIP          = 4096;
  FF_BUG_MS               = 8192; ///< Work around various bugs in Microsoft's broken decoders.
  FF_BUG_TRUNCATED        = 16384;
  FF_BUG_IEDGE            = 32768;

  // AVCodecContext.strict_std_compliance
  FF_COMPLIANCE_VERY_STRICT  = 2; ///< Strictly conform to an older more strict version of the spec or reference software.
  FF_COMPLIANCE_STRICT       = 1; ///< Strictly conform to all the things in the spec no matter what consequences.
  FF_COMPLIANCE_NORMAL       = 0;
  FF_COMPLIANCE_UNOFFICIAL   = -1; ///< Allow unofficial extensions
  FF_COMPLIANCE_EXPERIMENTAL = -2; ///< Allow nonstandardized experimental things.

  // AVCodecContext.dct_algo
  FF_DCT_AUTO    = 0;
  FF_DCT_FASTINT = 1;
  FF_DCT_INT     = 2;
  FF_DCT_MMX     = 3;
  FF_DCT_ALTIVEC = 5;
  FF_DCT_FAAN    = 6;

  // AVCodecContext.idct_algo
  FF_IDCT_AUTO          = 0;
  FF_IDCT_INT           = 1;
  FF_IDCT_SIMPLE        = 2;
  FF_IDCT_SIMPLEMMX     = 3;
  FF_IDCT_ARM           = 7;
  FF_IDCT_ALTIVEC       = 8;
{$IFDEF FF_API_ARCH_SH4}
  FF_IDCT_SH4           = 9;
{$ENDIF}
  FF_IDCT_SIMPLEARM     = 10;
{$IFDEF FF_API_UNUSED_MEMBERS}
  FF_IDCT_IPP           = 13;
{$ENDIF} (* FF_API_UNUSED_MEMBERS *)
  FF_IDCT_XVID          = 14;
{$IFDEF FF_API_IDCT_XVIDMMX}
  FF_IDCT_XVIDMMX       = 14;
{$ENDIF} (* FF_API_IDCT_XVIDMMX *)
  FF_IDCT_SIMPLEARMV5TE = 16;
  FF_IDCT_SIMPLEARMV6   = 17;
{$IFDEF FF_API_ARCH_SPARC}
  FF_IDCT_SIMPLEVIS     = 18;
{$ENDIF}
  FF_IDCT_FAAN          = 20;
  FF_IDCT_SIMPLENEON    = 22;
{$IFDEF FF_API_ARCH_ALPHA}
  FF_IDCT_SIMPLEALPHA   = 23;
{$ENDIF}
  FF_IDCT_SIMPLEAUTO    = 128;

  // AVCodecContext.error_concealment
  FF_EC_GUESS_MVS   = 1;
  FF_EC_DEBLOCK     = 2;
  FF_EC_FAVOR_INTER = 256;

{$IFDEF FF_API_PRIVATE_OPT}
  // AVCodecContext.prediction_method
  FF_PRED_LEFT   = 0;
  FF_PRED_PLANE  = 1;
  FF_PRED_MEDIAN = 2;
{$ENDIF}

  // AVCodecContext.debug
  FF_DEBUG_PICT_INFO   = 1;
  FF_DEBUG_RC          = 2;
  FF_DEBUG_BITSTREAM   = 4;
  FF_DEBUG_MB_TYPE     = 8;
  FF_DEBUG_QP          = 16;
{$IFDEF FF_API_DEBUG_MV}
(**
 * @deprecated this option does nothing
 *)
  FF_DEBUG_MV          = 32;
{$ENDIF}
  FF_DEBUG_DCT_COEFF   = $00000040;
  FF_DEBUG_SKIP        = $00000080;
  FF_DEBUG_STARTCODE   = $00000100;
{$IFDEF FF_API_UNUSED_MEMBERS}
  FF_DEBUG_PTS         = $00000200;
{$ENDIF} (* FF_API_UNUSED_MEMBERS *)
  FF_DEBUG_ER          = $00000400;
  FF_DEBUG_MMCO        = $00000800;
  FF_DEBUG_BUGS        = $00001000;
{$IFDEF FF_API_DEBUG_MV}
  FF_DEBUG_VIS_QP      = $00002000;
  FF_DEBUG_VIS_MB_TYPE = $00004000;
{$ENDIF}
  FF_DEBUG_BUFFERS     = $00008000;
  FF_DEBUG_THREADS     = $00010000;
  FF_DEBUG_GREEN_MD    = $00800000;
  FF_DEBUG_NOMC        = $01000000;

{$IFDEF FF_API_DEBUG_MV}
  // AVCodecContext.debug_mv
  FF_DEBUG_VIS_MV_P_FOR  = $00000001; // visualize forward predicted MVs of P-frames
  FF_DEBUG_VIS_MV_B_FOR  = $00000002; // visualize forward predicted MVs of B-frames
  FF_DEBUG_VIS_MV_B_BACK = $00000004; // visualize backward predicted MVs of B-frames
{$ENDIF}

  // AVCodecContext.properties
  FF_CODEC_PROPERTY_LOSSLESS        = $00000001;
  FF_CODEC_PROPERTY_CLOSED_CAPTIONS = $00000002;

  // AVCodecContext.sub_text_format
  FF_SUB_TEXT_FMT_ASS              = 0;
{$IFDEF FF_API_ASS_TIMING}
  FF_SUB_TEXT_FMT_ASS_WITH_TIMINGS = 1;
{$ENDIF}

  // AVCodecContext.ildct_cmp
  FF_CMP_SAD        = 0;
  FF_CMP_SSE        = 1;
  FF_CMP_SATD       = 2;
  FF_CMP_DCT        = 3;
  FF_CMP_PSNR       = 4;
  FF_CMP_BIT        = 5;
  FF_CMP_RD         = 6;
  FF_CMP_ZERO       = 7;
  FF_CMP_VSAD       = 8;
  FF_CMP_VSSE       = 9;
  FF_CMP_NSSE       = 10;
  FF_CMP_W53        = 11;
  FF_CMP_W97        = 12;
  FF_CMP_DCTMAX     = 13;
  FF_CMP_DCT264     = 14;
  FF_CMP_MEDIAN_SAD = 15;
  FF_CMP_CHROMA     = 256;

{$IFDEF FF_API_AFD}
  // AVCodecContext.dtg_active_format
  FF_DTG_AFD_SAME         = 8;
  FF_DTG_AFD_4_3          = 9;
  FF_DTG_AFD_16_9         = 10;
  FF_DTG_AFD_14_9         = 11;
  FF_DTG_AFD_4_3_SP_14_9  = 13;
  FF_DTG_AFD_16_9_SP_14_9 = 14;
  FF_DTG_AFD_SP_4_3       = 15;
{$ENDIF} (* FF_API_AFD *)

  // AVCodecContext.intra_quant_bias
  FF_DEFAULT_QUANT_BIAS = 999999;

{$IFDEF FF_API_CODER_TYPE}
  // AVCodecContext.coder_type
  FF_CODER_TYPE_VLC       = 0;
  FF_CODER_TYPE_AC        = 1;
  FF_CODER_TYPE_RAW       = 2;
  FF_CODER_TYPE_RLE       = 3;
{$IFDEF FF_API_UNUSED_MEMBERS}
  FF_CODER_TYPE_DEFLATE   = 4;
{$ENDIF} (* FF_API_UNUSED_MEMBERS *)
{$ENDIF} (* FF_API_CODER_TYPE *)

  // AVCodecContext.slice_flags
  SLICE_FLAG_CODED_ORDER    = $0001; ///< draw_horiz_band() is called in coded order instead of display
  SLICE_FLAG_ALLOW_FIELD    = $0002; ///< allow draw_horiz_band() with field slices (MPEG-2 field pics)
  SLICE_FLAG_ALLOW_PLANE    = $0004; ///< allow draw_horiz_band() with 1 component at a time (SVQ1)

  // AVCodecContext.mb_decision
  FF_MB_DECISION_SIMPLE = 0;        ///< uses mb_cmp
  FF_MB_DECISION_BITS   = 1;        ///< chooses the one which needs the fewest bits
  FF_MB_DECISION_RD     = 2;        ///< rate distortion

  // AVCodecContext.profile
  FF_PROFILE_UNKNOWN  = -99;
  FF_PROFILE_RESERVED = -100;

  FF_PROFILE_AAC_MAIN = 0;
  FF_PROFILE_AAC_LOW  = 1;
  FF_PROFILE_AAC_SSR  = 2;
  FF_PROFILE_AAC_LTP  = 3;
  FF_PROFILE_AAC_HE   = 4;
  FF_PROFILE_AAC_HE_V2 = 28;
  FF_PROFILE_AAC_LD   = 22;
  FF_PROFILE_AAC_ELD  = 38;
  FF_PROFILE_MPEG2_AAC_LOW = 128;
  FF_PROFILE_MPEG2_AAC_HE  = 131;

  FF_PROFILE_DNXHD       = 0;
  FF_PROFILE_DNXHR_LB    = 1;
  FF_PROFILE_DNXHR_SQ    = 2;
  FF_PROFILE_DNXHR_HQ    = 3;
  FF_PROFILE_DNXHR_HQX   = 4;
  FF_PROFILE_DNXHR_444   = 5;

  FF_PROFILE_DTS         = 20;
  FF_PROFILE_DTS_ES      = 30;
  FF_PROFILE_DTS_96_24   = 40;
  FF_PROFILE_DTS_HD_HRA  = 50;
  FF_PROFILE_DTS_HD_MA   = 60;
  FF_PROFILE_DTS_EXPRESS = 70;

  FF_PROFILE_MPEG2_422    = 0;
  FF_PROFILE_MPEG2_HIGH   = 1;
  FF_PROFILE_MPEG2_SS     = 2;
  FF_PROFILE_MPEG2_SNR_SCALABLE = 3;
  FF_PROFILE_MPEG2_MAIN   = 4;
  FF_PROFILE_MPEG2_SIMPLE = 5;

  FF_PROFILE_H264_CONSTRAINED = (1 shl 9);  // 8+1; constraint_set1_flag
  FF_PROFILE_H264_INTRA       = (1 shl 11); // 8+3; constraint_set3_flag

  FF_PROFILE_H264_BASELINE             = 66;
  FF_PROFILE_H264_CONSTRAINED_BASELINE = (66 or FF_PROFILE_H264_CONSTRAINED);
  FF_PROFILE_H264_MAIN                 = 77;
  FF_PROFILE_H264_EXTENDED             = 88;
  FF_PROFILE_H264_HIGH                 = 100;
  FF_PROFILE_H264_HIGH_10              = 110;
  FF_PROFILE_H264_HIGH_10_INTRA        = (110 or FF_PROFILE_H264_INTRA);
  FF_PROFILE_H264_MULTIVIEW_HIGH       = 118;
  FF_PROFILE_H264_HIGH_422             = 122;
  FF_PROFILE_H264_HIGH_422_INTRA       = (122 or FF_PROFILE_H264_INTRA);
  FF_PROFILE_H264_STEREO_HIGH          = 128;
  FF_PROFILE_H264_HIGH_444             = 144;
  FF_PROFILE_H264_HIGH_444_PREDICTIVE  = 244;
  FF_PROFILE_H264_HIGH_444_INTRA       = (244 or FF_PROFILE_H264_INTRA);
  FF_PROFILE_H264_CAVLC_444            = 44;

  FF_PROFILE_VC1_SIMPLE   = 0;
  FF_PROFILE_VC1_MAIN     = 1;
  FF_PROFILE_VC1_COMPLEX  = 2;
  FF_PROFILE_VC1_ADVANCED = 3;

  FF_PROFILE_MPEG4_SIMPLE                    =  0;
  FF_PROFILE_MPEG4_SIMPLE_SCALABLE           =  1;
  FF_PROFILE_MPEG4_CORE                      =  2;
  FF_PROFILE_MPEG4_MAIN                      =  3;
  FF_PROFILE_MPEG4_N_BIT                     =  4;
  FF_PROFILE_MPEG4_SCALABLE_TEXTURE          =  5;
  FF_PROFILE_MPEG4_SIMPLE_FACE_ANIMATION     =  6;
  FF_PROFILE_MPEG4_BASIC_ANIMATED_TEXTURE    =  7;
  FF_PROFILE_MPEG4_HYBRID                    =  8;
  FF_PROFILE_MPEG4_ADVANCED_REAL_TIME        =  9;
  FF_PROFILE_MPEG4_CORE_SCALABLE             = 10;
  FF_PROFILE_MPEG4_ADVANCED_CODING           = 11;
  FF_PROFILE_MPEG4_ADVANCED_CORE             = 12;
  FF_PROFILE_MPEG4_ADVANCED_SCALABLE_TEXTURE = 13;
  FF_PROFILE_MPEG4_SIMPLE_STUDIO             = 14;
  FF_PROFILE_MPEG4_ADVANCED_SIMPLE           = 15;

  FF_PROFILE_JPEG2000_CSTREAM_RESTRICTION_0  = 1;
  FF_PROFILE_JPEG2000_CSTREAM_RESTRICTION_1  = 2;
  FF_PROFILE_JPEG2000_CSTREAM_NO_RESTRICTION = 32768;
  FF_PROFILE_JPEG2000_DCINEMA_2K             = 3;
  FF_PROFILE_JPEG2000_DCINEMA_4K             = 4;

  FF_PROFILE_VP9_0                           = 0;
  FF_PROFILE_VP9_1                           = 1;
  FF_PROFILE_VP9_2                           = 2;
  FF_PROFILE_VP9_3                           = 3;

  FF_PROFILE_HEVC_MAIN                       = 1;
  FF_PROFILE_HEVC_MAIN_10                    = 2;
  FF_PROFILE_HEVC_MAIN_STILL_PICTURE         = 3;
  FF_PROFILE_HEVC_REXT                       = 4;

  // AVCodecContext.level
  FF_LEVEL_UNKNOWN = -99;

  // AVCodecContext.thread_type
  FF_THREAD_FRAME   = 1; ///< Decode more than one frame at once
  FF_THREAD_SLICE   = 2; ///< Decode more than one part of a single frame at once

  // AVCodecContext.err_recognition
  AV_EF_CRCCHECK   = 1 shl 0;
  AV_EF_BITSTREAM  = 1 shl 1;
  AV_EF_BUFFER     = 1 shl 2;
  AV_EF_EXPLODE    = 1 shl 3;
  AV_EF_IGNORE_ERR = 1 shl 15;
  AV_EF_CAREFUL    = 1 shl 16;
  AV_EF_COMPLIANT  = 1 shl 17;
  AV_EF_AGGRESSIVE = 1 shl 18;

  // AVCodecContext.compression_level
  FF_COMPRESSION_DEFAULT = -1;

  // AVCodecContext.sub_charenc_mode
  FF_SUB_CHARENC_MODE_DO_NOTHING  = -1; ///< do nothing (demuxer outputs a stream supposed to be already in UTF-8, or the codec is bitmap for instance)
  FF_SUB_CHARENC_MODE_AUTOMATIC   = 0;  ///< libavcodec will select the mode itself
  FF_SUB_CHARENC_MODE_PRE_DECODER = 1;  ///< the AVPacket data needs to be recoded to UTF-8 before being fed to the decoder, requires iconv

  // AVFrame.decode_error_flags
  FF_DECODE_ERROR_INVALID_BITSTREAM = 1;
  FF_DECODE_ERROR_MISSING_REFERENCE = 2;

  AV_SUBTITLE_FLAG_FORCED = $00000001;

(**
 * Hardware acceleration should be used for decoding even if the codec level
 * used is unknown or higher than the maximum supported level reported by the
 * hardware driver.
 *
 * It's generally a good idea to pass this flag unless you have a specific
 * reason not to, as hardware tends to under-report supported levels.
 *)
  AV_HWACCEL_FLAG_IGNORE_LEVEL = (1 shl 0);

(**
 * Hardware acceleration can output YUV pixel formats with a different chroma
 * sampling than 4:2:0 and/or other than 8 bits per component.
 *)
  AV_HWACCEL_FLAG_ALLOW_HIGH_DEPTH = (1 shl 1);

(**
 * @defgroup lavc_packet AVPacket
 *
 * Types and functions for working with AVPacket.
 * @{
 *)
type
  TAVPacketSideDataType = (
    (**
     * An AV_PKT_DATA_PALETTE side data packet contains exactly AVPALETTE_SIZE
     * bytes worth of palette. This side data signals that a new palette is
     * present.
     *)
    AV_PKT_DATA_PALETTE,

    (**
     * The AV_PKT_DATA_NEW_EXTRADATA is used to notify the codec or the format
     * that the extradata buffer was changed and the receiving side should
     * act upon it appropriately. The new extradata is embedded in the side
     * data buffer and should be immediately used for processing the current
     * frame or packet.
     *)
    AV_PKT_DATA_NEW_EXTRADATA,

    (**
     * An AV_PKT_DATA_PARAM_CHANGE side data packet is laid out as follows:
     * @code
     * u32le param_flags
     * if (param_flags & AV_SIDE_DATA_PARAM_CHANGE_CHANNEL_COUNT)
     *     s32le channel_count
     * if (param_flags & AV_SIDE_DATA_PARAM_CHANGE_CHANNEL_LAYOUT)
     *     u64le channel_layout
     * if (param_flags & AV_SIDE_DATA_PARAM_CHANGE_SAMPLE_RATE)
     *     s32le sample_rate
     * if (param_flags & AV_SIDE_DATA_PARAM_CHANGE_DIMENSIONS)
     *     s32le width
     *     s32le height
     * @endcode
     *)
    AV_PKT_DATA_PARAM_CHANGE,

    (**
     * An AV_PKT_DATA_H263_MB_INFO side data packet contains a number of
     * structures with info about macroblocks relevant to splitting the
     * packet into smaller packets on macroblock edges (e.g. as for RFC 2190).
     * That is, it does not necessarily contain info about all macroblocks,
     * as long as the distance between macroblocks in the info is smaller
     * than the target payload size.
     * Each MB info structure is 12 bytes, and is laid out as follows:
     * @code
     * u32le bit offset from the start of the packet
     * u8    current quantizer at the start of the macroblock
     * u8    GOB number
     * u16le macroblock address within the GOB
     * u8    horizontal MV predictor
     * u8    vertical MV predictor
     * u8    horizontal MV predictor for block number 3
     * u8    vertical MV predictor for block number 3
     * @endcode
     *)
    AV_PKT_DATA_H263_MB_INFO,

    (**
     * This side data should be associated with an audio stream and contains
     * ReplayGain information in form of the AVReplayGain struct.
     *)
    AV_PKT_DATA_REPLAYGAIN,

    (**
     * This side data contains a 3x3 transformation matrix describing an affine
     * transformation that needs to be applied to the decoded video frames for
     * correct presentation.
     *
     * See libavutil/display.h for a detailed description of the data.
     *)
    AV_PKT_DATA_DISPLAYMATRIX,

    (**
     * This side data should be associated with a video stream and contains
     * Stereoscopic 3D information in form of the AVStereo3D struct.
     *)
    AV_PKT_DATA_STEREO3D,

    (**
     * This side data should be associated with an audio stream and corresponds
     * to enum AVAudioServiceType.
     *)
    AV_PKT_DATA_AUDIO_SERVICE_TYPE,

    (**
     * This side data contains quality related information from the encoder.
     * @code
     * u32le quality factor of the compressed frame. Allowed range is between 1 (good) and FF_LAMBDA_MAX (bad).
     * u8    picture type
     * u8    error count
     * u16   reserved
     * u64le[error count] sum of squared differences between encoder in and output
     * @endcode
     *)
    AV_PKT_DATA_QUALITY_STATS,

    (**
     * This side data contains an integer value representing the stream index
     * of a "fallback" track.  A fallback track indicates an alternate
     * track to use when the current track can not be decoded for some reason.
     * e.g. no decoder available for codec.
     *)
    AV_PKT_DATA_FALLBACK_TRACK,

    (**
     * This side data corresponds to the AVCPBProperties struct.
     *)
    AV_PKT_DATA_CPB_PROPERTIES,

    (**
     * Recommmends skipping the specified number of samples
     * @code
     * u32le number of samples to skip from start of this packet
     * u32le number of samples to skip from end of this packet
     * u8    reason for start skip
     * u8    reason for end   skip (0=padding silence, 1=convergence)
     * @endcode
     *)
    AV_PKT_DATA_SKIP_SAMPLES=70,

    (**
     * An AV_PKT_DATA_JP_DUALMONO side data packet indicates that
     * the packet may contain "dual mono" audio specific to Japanese DTV
     * and if it is true, recommends only the selected channel to be used.
     * @code
     * u8    selected channels (0=mail/left, 1=sub/right, 2=both)
     * @endcode
     *)
    AV_PKT_DATA_JP_DUALMONO,

    (**
     * A list of zero terminated key/value strings. There is no end marker for
     * the list, so it is required to rely on the side data size to stop.
     *)
    AV_PKT_DATA_STRINGS_METADATA,

    (**
     * Subtitle event position
     * @code
     * u32le x1
     * u32le y1
     * u32le x2
     * u32le y2
     * @endcode
     *)
    AV_PKT_DATA_SUBTITLE_POSITION,

    (**
     * Data found in BlockAdditional element of matroska container. There is
     * no end marker for the data, so it is required to rely on the side data
     * size to recognize the end. 8 byte id (as found in BlockAddId) followed
     * by data.
     *)
    AV_PKT_DATA_MATROSKA_BLOCKADDITIONAL,

    (**
     * The optional first identifier line of a WebVTT cue.
     *)
    AV_PKT_DATA_WEBVTT_IDENTIFIER,

    (**
     * The optional settings (rendering instructions) that immediately
     * follow the timestamp specifier of a WebVTT cue.
     *)
    AV_PKT_DATA_WEBVTT_SETTINGS,

    (**
     * A list of zero terminated key/value strings. There is no end marker for
     * the list, so it is required to rely on the side data size to stop. This
     * side data includes updated metadata which appeared in the stream.
     *)
    AV_PKT_DATA_METADATA_UPDATE,

    (**
     * MPEGTS stream ID, this is required to pass the stream ID
     * information from the demuxer to the corresponding muxer.
     *)
    AV_PKT_DATA_MPEGTS_STREAM_ID,

    (**
     * Mastering display metadata (based on SMPTE-2086:2014). This metadata
     * should be associated with a video stream and containts data in the form
     * of the AVMasteringDisplayMetadata struct.
     *)
    AV_PKT_DATA_MASTERING_DISPLAY_METADATA,

    (**
     * This side data should be associated with a video stream and corresponds
     * to the AVSphericalMapping structure.
     *)
    AV_PKT_DATA_SPHERICAL
  );

const
  AV_PKT_DATA_QUALITY_FACTOR = AV_PKT_DATA_QUALITY_STATS; //DEPRECATED

type
  PAVPacketSideData = ^TAVPacketSideData;
  TAVPacketSideData = record
    data: PByte;
    size: Integer;
    type_: TAVPacketSideDataType;
  end;

(**
 * This structure stores compressed data. It is typically exported by demuxers
 * and then passed as input to decoders, or received as output from encoders and
 * then passed to muxers.
 *
 * For video, it should typically contain one compressed frame. For audio it may
 * contain several compressed frames. Encoders are allowed to output empty
 * packets, with no compressed data, containing only side data
 * (e.g. to update some stream parameters at the end of encoding).
 *
 * AVPacket is one of the few structs in FFmpeg, whose size is a part of public
 * ABI. Thus it may be allocated on stack and no new fields can be added to it
 * without libavcodec and libavformat major bump.
 *
 * The semantics of data ownership depends on the buf field.
 * If it is set, the packet data is dynamically allocated and is
 * valid indefinitely until a call to av_packet_unref() reduces the
 * reference count to 0.
 *
 * If the buf field is not set av_packet_ref() would make a copy instead
 * of increasing the reference count.
 *
 * The side data is always allocated with av_malloc(), copied by
 * av_packet_ref() and freed by av_packet_unref().
 *
 * @see av_packet_ref
 * @see av_packet_unref
 *)
  PPAVPacket = ^PAVPacket;
  PAVPacket = ^TAVPacket;
  TAVPacket = record
    (**
     * A reference to the reference-counted buffer where the packet data is
     * stored.
     * May be NULL, then the packet data is not reference-counted.
     *)
    buf: PAVBufferRef;
    (**
     * Presentation timestamp in AVStream->time_base units; the time at which
     * the decompressed packet will be presented to the user.
     * Can be AV_NOPTS_VALUE if it is not stored in the file.
     * pts MUST be larger or equal to dts as presentation cannot happen before
     * decompression, unless one wants to view hex dumps. Some formats misuse
     * the terms dts and pts/cts to mean something different. Such timestamps
     * must be converted to true pts/dts before they are stored in AVPacket.
     *)
    pts: Int64;
    (**
     * Decompression timestamp in AVStream->time_base units; the time at which
     * the packet is decompressed.
     * Can be AV_NOPTS_VALUE if it is not stored in the file.
     *)
    dts: Int64;
    data: PByte;
    size: Integer;
    stream_index: Integer;
    (**
     * A combination of AV_PKT_FLAG values
     *)
    flags: Integer;
    (**
     * Additional packet data that can be provided by the container.
     * Packet can contain several types of side information.
     *)
    side_data: PAVPacketSideData;
    side_data_elems: Integer;

    (**
     * Duration of this packet in AVStream->time_base units, 0 if unknown.
     * Equals next_pts - this_pts in presentation order.
     *)
    duration: Int64;

    pos: Int64;             ///< byte position in stream, -1 if unknown

{$IFDEF FF_API_CONVERGENCE_DURATION}
    (**
     * @deprecated Same as the duration field, but as int64_t. This was required
     * for Matroska subtitles, whose duration values could overflow when the
     * duration field was still an int.
     *)
    convergence_duration: Int64;
{$ENDIF}
  end;

  TAVSideDataParamChangeFlags = (
    AV_SIDE_DATA_PARAM_CHANGE_CHANNEL_COUNT  = $0001,
    AV_SIDE_DATA_PARAM_CHANGE_CHANNEL_LAYOUT = $0002,
    AV_SIDE_DATA_PARAM_CHANGE_SAMPLE_RATE    = $0004,
    AV_SIDE_DATA_PARAM_CHANGE_DIMENSIONS     = $0008
  );
(**
 * @}
 *)

  PAVCodecInternal = ^TAVCodecInternal;
  TAVCodecInternal = record
    // need {$ALIGN 8}
    // defined in libavcodec/internal.h
  end;

  TAVFieldOrder = (
    AV_FIELD_UNKNOWN,
    AV_FIELD_PROGRESSIVE,
    AV_FIELD_TT,          //< Top coded_first, top displayed first
    AV_FIELD_BB,          //< Bottom coded first, bottom displayed first
    AV_FIELD_TB,          //< Top coded first, bottom displayed first
    AV_FIELD_BT           //< Bottom coded first, top displayed first
  );

(**
 * main external API structure.
 * New fields can be added to the end with minor version bumps.
 * Removal, reordering and changes to existing fields require a major
 * version bump.
 * You can use AVOptions (av_opt* / av_set/get*()) to access these fields from user
 * applications.
 * The name string for AVOptions options matches the associated command line
 * parameter name and can be found in libavcodec/options_table.h
 * The AVOption/command line parameter names differ in some cases from the C
 * structure field names for historic reasons or brevity.
 * sizeof(AVCodecContext) must not be used outside libav*.
 *)
  PPAVCodec = ^PAVCodec;
  PAVCodec = ^TAVCodec;
  PAVHWAccel = ^TAVHWAccel;
  PPAVCodecContext = ^PAVCodecContext;
  PAVCodecContext = ^TAVCodecContext;
  TexecuteCall = function (c2: PAVCodecContext; arg: Pointer): Integer; cdecl;
  Texecute2Call = function (c2: PAVCodecContext; arg: Pointer; jobnr, threadnr: Integer): Integer; cdecl;
  TAVCodecContext = record
    (**
     * information on struct for av_log
     * - set by avcodec_alloc_context3
     *)
    av_class: PAVClass;
    log_level_offset: Integer;

    codec_type: TAVMediaType; (* see AVMEDIA_TYPE_xxx *)
    codec: PAVCodec;
{$IFDEF FF_API_CODEC_NAME}
    (**
     * @deprecated this field is not used for anything in libavcodec
     *)
    codec_name: array[0..31] of AnsiChar;
{$ENDIF}
    codec_id: TAVCodecID; (* see AV_CODEC_ID_xxx *)

    (**
     * fourcc (LSB first, so "ABCD" -> ('D'<<24) + ('C'<<16) + ('B'<<8) + 'A').
     * This is used to work around some encoder bugs.
     * A demuxer should set this to what is stored in the field used to identify the codec.
     * If there are multiple such fields in a container then the demuxer should choose the one
     * which maximizes the information about the used codec.
     * If the codec tag field in a container is larger than 32 bits then the demuxer should
     * remap the longer ID to 32 bits with a table or other structure. Alternatively a new
     * extra_codec_tag + size could be added but for this a clear advantage must be demonstrated
     * first.
     * - encoding: Set by user, if not then the default based on codec_id will be used.
     * - decoding: Set by user, will be converted to uppercase by libavcodec during init.
     *)
    //unsigned int codec_tag;
    //codec_tag: Cardinal;
    //codec_tag: array[0..3] of AnsiChar;
    codec_tag: packed record
      case Integer of
        0: (tag: Cardinal);
        1: (fourcc: array[0..3] of AnsiChar);
        2: (fourbb: array[0..3] of Byte);
      end;

{$IFDEF FF_API_STREAM_CODEC_TAG}
    (**
     * @deprecated this field is unused
     *)
    stream_codec_tag: array[0..3] of AnsiChar;
{$ENDIF}

    priv_data: Pointer;

    (**
     * Private context used for internal data.
     *
     * Unlike priv_data, this is not codec-specific. It is used in general
     * libavcodec functions.
     *)
    internal: PAVCodecInternal;

    (**
     * Private data of the user, can be used to carry app specific stuff.
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    opaque: Pointer;

    (**
     * the average bitrate
     * - encoding: Set by user; unused for constant quantizer encoding.
     * - decoding: Set by user, may be overwritten by libavcodec
     *             if this info is available in the stream
     *)
    bit_rate: Int64;

    (**
     * number of bits the bitstream is allowed to diverge from the reference.
     *           the reference can be CBR (for CBR pass1) or VBR (for pass2)
     * - encoding: Set by user; unused for constant quantizer encoding.
     * - decoding: unused
     *)
    bit_rate_tolerance: Integer;

    (**
     * Global quality for codecs which cannot change it per frame.
     * This should be proportional to MPEG-1/2/4 qscale.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    global_quality: Integer;

    (**
     * - encoding: Set by user.
     * - decoding: unused
     *)
    compression_level: Integer;
//#define FF_COMPRESSION_DEFAULT -1

    (**
     * AV_CODEC_FLAG_*.
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    flags: Integer;

    (**
     * AV_CODEC_FLAG2_*
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    flags2: Integer;

    (**
     * some codecs need / can use extradata like Huffman tables.
     * MJPEG: Huffman tables
     * rv10: additional flags
     * MPEG-4: global headers (they can be in the bitstream or here)
     * The allocated memory should be AV_INPUT_BUFFER_PADDING_SIZE bytes larger
     * than extradata_size to avoid problems if it is read with the bitstream reader.
     * The bytewise contents of extradata must not depend on the architecture or CPU endianness.
     * - encoding: Set/allocated/freed by libavcodec.
     * - decoding: Set/allocated/freed by user.
     *)
    extradata: PByte;
    extradata_size: Integer;

    (**
     * This is the fundamental unit of time (in seconds) in terms
     * of which frame timestamps are represented. For fixed-fps content,
     * timebase should be 1/framerate and timestamp increments should be
     * identically 1.
     * This often, but not always is the inverse of the frame rate or field rate
     * for video. 1/time_base is not the average frame rate if the frame rate is not
     * constant.
     *
     * Like containers, elementary streams also can store timestamps, 1/time_base
     * is the unit in which these timestamps are specified.
     * As example of such codec time base see ISO/IEC 14496-2:2001(E)
     * vop_time_increment_resolution and fixed_vop_rate
     * (fixed_vop_rate == 0 implies that it is different from the framerate)
     *
     * - encoding: MUST be set by user.
     * - decoding: the use of this field for decoding is deprecated.
     *             Use framerate instead.
     *)
    time_base: TAVRational;

    (**
     * For some codecs, the time base is closer to the field rate than the frame rate.
     * Most notably, H.264 and MPEG-2 specify time_base as half of frame duration
     * if no telecine is used ...
     *
     * Set to time_base ticks per frame. Default 1, e.g., H.264/MPEG-2 set it to 2.
     *)
    ticks_per_frame: Integer;

    (**
     * Codec delay.
     *
     * Encoding: Number of frames delay there will be from the encoder input to
     *           the decoder output. (we assume the decoder matches the spec)
     * Decoding: Number of frames delay in addition to what a standard decoder
     *           as specified in the spec would produce.
     *
     * Video:
     *   Number of frames the decoded output will be delayed relative to the
     *   encoded input.
     *
     * Audio:
     *   For encoding, this field is unused (see initial_padding).
     *
     *   For decoding, this is the number of samples the decoder needs to
     *   output before the decoder's output is valid. When seeking, you should
     *   start decoding this many samples prior to your desired seek point.
     *
     * - encoding: Set by libavcodec.
     * - decoding: Set by libavcodec.
     *)
    delay: Integer;


    (* video only *)
    (**
     * picture width / height.
     *
     * @note Those fields may not match the values of the last
     * AVFrame output by avcodec_decode_video2 due frame
     * reordering.
     *
     * - encoding: MUST be set by user.
     * - decoding: May be set by the user before opening the decoder if known e.g.
     *             from the container. Some decoders will require the dimensions
     *             to be set by the caller. During decoding, the decoder may
     *             overwrite those values as required while parsing the data.
     *)
    width, height: Integer;

    (**
     * Bitstream width / height, may be different from width/height e.g. when
     * the decoded frame is cropped before being output or lowres is enabled.
     *
     * @note Those field may not match the value of the last
     * AVFrame output by avcodec_receive_frame() due frame
     * reordering.
     *
     * - encoding: unused
     * - decoding: May be set by the user before opening the decoder if known
     *             e.g. from the container. During decoding, the decoder may
     *             overwrite those values as required while parsing the data.
     *)
    coded_width, coded_height: Integer;

//#if FF_API_ASPECT_EXTENDED
//#define FF_ASPECT_EXTENDED 15
//#endif

    (**
     * the number of pictures in a group of pictures, or 0 for intra_only
     * - encoding: Set by user.
     * - decoding: unused
     *)
    gop_size: Integer;

    (**
     * Pixel format, see AV_PIX_FMT_xxx.
     * May be set by the demuxer if known from headers.
     * May be overridden by the decoder if it knows better.
     *
     * @note This field may not match the value of the last
     * AVFrame output by avcodec_receive_frame() due frame
     * reordering.
     *
     * - encoding: Set by user.
     * - decoding: Set by user if known, overridden by libavcodec while
     *             parsing the data.
     *)
    pix_fmt: TAVPixelFormat;

{$IFDEF FF_API_MOTION_EST}
    (**
     * This option does nothing
     * @deprecated use codec private options instead
     *)
    me_method: Integer;
{$ENDIF}

    (**
     * If non NULL, 'draw_horiz_band' is called by the libavcodec
     * decoder to draw a horizontal band. It improves cache usage. Not
     * all codecs can do that. You must check the codec capabilities
     * beforehand.
     * When multithreading is used, it may be called from multiple threads
     * at the same time; threads might draw different parts of the same AVFrame,
     * or multiple AVFrames, and there is no guarantee that slices will be drawn
     * in order.
     * The function is also used by hardware acceleration APIs.
     * It is called at least once during frame decoding to pass
     * the data needed for hardware render.
     * In that mode instead of pixel data, AVFrame points to
     * a structure specific to the acceleration API. The application
     * reads the structure and can change some fields to indicate progress
     * or mark state.
     * - encoding: unused
     * - decoding: Set by user.
     * @param height the height of the slice
     * @param y the y position of the slice
     * @param type 1->top field, 2->bottom field, 3->frame
     * @param offset offset into the AVFrame.data from which the slice should be read
     *)
    draw_horiz_band: procedure (s: PAVCodecContext;
                            const src: PAVFrame; offset: PInteger;{int offset[AV_NUM_DATA_POINTERS]}
                            y, ttype, height: Integer); cdecl;

    (**
     * callback to negotiate the pixelFormat
     * @param fmt is the list of formats which are supported by the codec,
     * it is terminated by -1 as 0 is a valid format, the formats are ordered by quality.
     * The first is always the native one.
     * @note The callback may be called again immediately if initialization for
     * the selected (hardware-accelerated) pixel format failed.
     * @warning Behavior is undefined if the callback returns a value not
     * in the fmt list of formats.
     * @return the chosen format
     * - encoding: unused
     * - decoding: Set by user, if not set the native format will be chosen.
     *)
    get_format: function(s: PAVCodecContext; const fmt: PAVPixelFormat): TAVPixelFormat; cdecl;

    (**
     * maximum number of B-frames between non-B-frames
     * Note: The output will be delayed by max_b_frames+1 relative to the input.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    max_b_frames: Integer;

    (**
     * qscale factor between IP and B-frames
     * If > 0 then the last P-frame quantizer will be used (q= lastp_q*factor+offset).
     * If < 0 then normal ratecontrol will be done (q= -normal_q*factor+offset).
     * - encoding: Set by user.
     * - decoding: unused
     *)
    b_quant_factor: Single;

{$IFDEF FF_API_RC_STRATEGY}
    (** @deprecated use codec private option instead *)
    rc_strategy: Integer;
//#define FF_RC_STRATEGY_XVID 1
{$ENDIF}

{$IFDEF FF_API_PRIVATE_OPT}
    (** @deprecated use encoder private options instead *)
    b_frame_strategy: Integer;
{$ENDIF}

    (**
     * qscale offset between IP and B-frames
     * - encoding: Set by user.
     * - decoding: unused
     *)
    b_quant_offset: Single;

    (**
     * Size of the frame reordering buffer in the decoder.
     * For MPEG-2 it is 1 IPB or 0 low delay IP.
     * - encoding: Set by libavcodec.
     * - decoding: Set by libavcodec.
     *)
    has_b_frames: Integer;

{$IFDEF FF_API_PRIVATE_OPT}
    (** @deprecated use encoder private options instead *)
    mpeg_quant: Integer;
{$ENDIF}

    (**
     * qscale factor between P- and I-frames
     * If > 0 then the last P-frame quantizer will be used (q = lastp_q * factor + offset).
     * If < 0 then normal ratecontrol will be done (q= -normal_q*factor+offset).
     * - encoding: Set by user.
     * - decoding: unused
     *)
    i_quant_factor: Single;

    (**
     * qscale offset between P and I-frames
     * - encoding: Set by user.
     * - decoding: unused
     *)
    i_quant_offset: Single;

    (**
     * luminance masking (0-> disabled)
     * - encoding: Set by user.
     * - decoding: unused
     *)
    lumi_masking: Single;

    (**
     * temporary complexity masking (0-> disabled)
     * - encoding: Set by user.
     * - decoding: unused
     *)
    temporal_cplx_masking: Single;

    (**
     * spatial complexity masking (0-> disabled)
     * - encoding: Set by user.
     * - decoding: unused
     *)
    spatial_cplx_masking: Single;

    (**
     * p block masking (0-> disabled)
     * - encoding: Set by user.
     * - decoding: unused
     *)
    p_masking: Single;

    (**
     * darkness masking (0-> disabled)
     * - encoding: Set by user.
     * - decoding: unused
     *)
    dark_masking: Single;

    (**
     * slice count
     * - encoding: Set by libavcodec.
     * - decoding: Set by user (or 0).
     *)
    slice_count: Integer;

{$IFDEF FF_API_PRIVATE_OPT}
    (** @deprecated use encoder private options instead *)
    prediction_method: Integer;
{
#define FF_PRED_LEFT   0
#define FF_PRED_PLANE  1
#define FF_PRED_MEDIAN 2
}
{$ENDIF}

    (**
     * slice offsets in the frame in bytes
     * - encoding: Set/allocated by libavcodec.
     * - decoding: Set/allocated by user (or NULL).
     *)
    slice_offset: PInteger;

    (**
     * sample aspect ratio (0 if unknown)
     * That is the width of a pixel divided by the height of the pixel.
     * Numerator and denominator must be relatively prime and smaller than 256 for some video standards.
     * - encoding: Set by user.
     * - decoding: Set by libavcodec.
     *)
    sample_aspect_ratio: TAVRational;

    (**
     * motion estimation comparison function
     * - encoding: Set by user.
     * - decoding: unused
     *)
    me_cmp: Integer;
    (**
     * subpixel motion estimation comparison function
     * - encoding: Set by user.
     * - decoding: unused
     *)
    me_sub_cmp: Integer;
    (**
     * macroblock comparison function (not supported yet)
     * - encoding: Set by user.
     * - decoding: unused
     *)
    mb_cmp: Integer;
    (**
     * interlaced DCT comparison function
     * - encoding: Set by user.
     * - decoding: unused
     *)
    ildct_cmp: Integer;
{
#define FF_CMP_SAD          0
#define FF_CMP_SSE          1
#define FF_CMP_SATD         2
#define FF_CMP_DCT          3
#define FF_CMP_PSNR         4
#define FF_CMP_BIT          5
#define FF_CMP_RD           6
#define FF_CMP_ZERO         7
#define FF_CMP_VSAD         8
#define FF_CMP_VSSE         9
#define FF_CMP_NSSE         10
#define FF_CMP_W53          11
#define FF_CMP_W97          12
#define FF_CMP_DCTMAX       13
#define FF_CMP_DCT264       14
#define FF_CMP_MEDIAN_SAD   15
#define FF_CMP_CHROMA       256
}
    (**
     * ME diamond size & shape
     * - encoding: Set by user.
     * - decoding: unused
     *)
    dia_size: Integer;

    (**
     * amount of previous MV predictors (2a+1 x 2a+1 square)
     * - encoding: Set by user.
     * - decoding: unused
     *)
    last_predictor_count: Integer;

{$IFDEF FF_API_PRIVATE_OPT}
    (** @deprecated use encoder private options instead *)
    pre_me: Integer;
{$ENDIF}

    (**
     * motion estimation prepass comparison function
     * - encoding: Set by user.
     * - decoding: unused
     *)
    me_pre_cmp: Integer;

    (**
     * ME prepass diamond size & shape
     * - encoding: Set by user.
     * - decoding: unused
     *)
    pre_dia_size: Integer;

    (**
     * subpel ME quality
     * - encoding: Set by user.
     * - decoding: unused
     *)
    me_subpel_quality: Integer;

{$IFDEF FF_API_AFD}
    (**
     * DTG active format information (additional aspect ratio
     * information only used in DVB MPEG-2 transport streams)
     * 0 if not set.
     *
     * - encoding: unused
     * - decoding: Set by decoder.
     * @deprecated Deprecated in favor of AVSideData
     *)
    dtg_active_format: Integer;
{
#define FF_DTG_AFD_SAME         8
#define FF_DTG_AFD_4_3          9
#define FF_DTG_AFD_16_9         10
#define FF_DTG_AFD_14_9         11
#define FF_DTG_AFD_4_3_SP_14_9  13
#define FF_DTG_AFD_16_9_SP_14_9 14
#define FF_DTG_AFD_SP_4_3       15
}
{$ENDIF} (* FF_API_AFD *)
    (**
     * maximum motion estimation search range in subpel units
     * If 0 then no limit.
     *
     * - encoding: Set by user.
     * - decoding: unused
     *)
    me_range: Integer;

{$IFDEF FF_API_QUANT_BIAS}
    (**
     * @deprecated use encoder private option instead
     *)
    intra_quant_bias: Integer;
//#define FF_DEFAULT_QUANT_BIAS 999999

    (**
     * @deprecated use encoder private option instead
     *)
    inter_quant_bias: Integer;
{$ENDIF}

    (**
     * slice flags
     * - encoding: unused
     * - decoding: Set by user.
     *)
    slice_flags: Integer;
{
#define SLICE_FLAG_CODED_ORDER    0x0001 ///< draw_horiz_band() is called in coded order instead of display
#define SLICE_FLAG_ALLOW_FIELD    0x0002 ///< allow draw_horiz_band() with field slices (MPEG-2 field pics)
#define SLICE_FLAG_ALLOW_PLANE    0x0004 ///< allow draw_horiz_band() with 1 component at a time (SVQ1)
}

{$IFDEF FF_API_XVMC}
    (**
     * XVideo Motion Acceleration
     * - encoding: forbidden
     * - decoding: set by decoder
     * @deprecated XvMC doesn't need it anymore.
     *)
    xvmc_acceleration: Integer;
{$ENDIF}

    (**
     * macroblock decision mode
     * - encoding: Set by user.
     * - decoding: unused
     *)
    mb_decision: Integer;
{
#define FF_MB_DECISION_SIMPLE 0        ///< uses mb_cmp
#define FF_MB_DECISION_BITS   1        ///< chooses the one which needs the fewest bits
#define FF_MB_DECISION_RD     2        ///< rate distortion
}
    (**
     * custom intra quantization matrix
     * - encoding: Set by user, can be NULL.
     * - decoding: Set by libavcodec.
     *)
    intra_matrix: PWord;

    (**
     * custom inter quantization matrix
     * - encoding: Set by user, can be NULL.
     * - decoding: Set by libavcodec.
     *)
    inter_matrix: PWord;

{$IFDEF FF_API_PRIVATE_OPT}
    (** @deprecated use encoder private options instead *)
    scenechange_threshold: Integer;

    (** @deprecated use encoder private options instead *)
    noise_reduction: Integer;
{$ENDIF}

{$IFDEF FF_API_MPV_OPT}
    (**
     * @deprecated this field is unused
     *)
    me_threshold: Integer;

    (**
     * @deprecated this field is unused
     *)
    mb_threshold: Integer;
{$ENDIF}

    (**
     * precision of the intra DC coefficient - 8
     * - encoding: Set by user.
     * - decoding: Set by libavcodec
     *)
    intra_dc_precision: Integer;

    (**
     * Number of macroblock rows at the top which are skipped.
     * - encoding: unused
     * - decoding: Set by user.
     *)
    skip_top: Integer;

    (**
     * Number of macroblock rows at the bottom which are skipped.
     * - encoding: unused
     * - decoding: Set by user.
     *)
    skip_bottom: Integer;

{$IFDEF FF_API_MPV_OPT}
    (**
     * @deprecated use encoder private options instead
     *)
    border_masking: Single;
{$ENDIF}

    (**
     * minimum MB Lagrange multiplier
     * - encoding: Set by user.
     * - decoding: unused
     *)
    mb_lmin: Integer;

    (**
     * maximum MB Lagrange multiplier
     * - encoding: Set by user.
     * - decoding: unused
     *)
    mb_lmax: Integer;

{$IFDEF FF_API_PRIVATE_OPT}
    (**
     * @deprecated use encoder private options instead
     *)
    me_penalty_compensation: Integer;
{$ENDIF}

    (**
     * - encoding: Set by user.
     * - decoding: unused
     *)
    bidir_refine: Integer;

{$IFDEF FF_API_PRIVATE_OPT}
    (** @deprecated use encoder private options instead *)
    brd_scale: Integer;
{$ENDIF}

    (**
     * minimum GOP size
     * - encoding: Set by user.
     * - decoding: unused
     *)
    keyint_min: Integer;

    (**
     * number of reference frames
     * - encoding: Set by user.
     * - decoding: Set by lavc.
     *)
    refs: Integer;

{$IFDEF FF_API_PRIVATE_OPT}
    (** @deprecated use encoder private options instead *)
    chromaoffset: Integer;
{$ENDIF}

{$IFDEF FF_API_UNUSED_MEMBERS}
    (**
     * Multiplied by qscale for each frame and added to scene_change_score.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    scenechange_factor: Integer;
{$ENDIF}

    (**
     * Note: Value depends upon the compare function used for fullpel ME.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    mv0_threshold: Integer;

{$IFDEF FF_API_PRIVATE_OPT}
    (** @deprecated use encoder private options instead *)
    b_sensitivity: Integer;
{$ENDIF}

    (**
     * Chromaticity coordinates of the source primaries.
     * - encoding: Set by user
     * - decoding: Set by libavcodec
     *)
    color_primaries: TAVColorPrimaries;

    (**
     * Color Transfer Characteristic.
     * - encoding: Set by user
     * - decoding: Set by libavcodec
     *)
    color_trc: TAVColorTransferCharacteristic;

    (**
     * YUV colorspace type.
     * - encoding: Set by user
     * - decoding: Set by libavcodec
     *)
    colorspace: TAVColorSpace;

    (**
     * MPEG vs JPEG YUV range.
     * - encoding: Set by user
     * - decoding: Set by libavcodec
     *)
    color_range: TAVColorRange;

    (**
     * This defines the location of chroma samples.
     * - encoding: Set by user
     * - decoding: Set by libavcodec
     *)
    chroma_sample_location: TAVChromaLocation;

    (**
     * Number of slices.
     * Indicates number of picture subdivisions. Used for parallelized
     * decoding.
     * - encoding: Set by user
     * - decoding: unused
     *)
    slices: Integer;

    (** Field order
     * - encoding: set by libavcodec
     * - decoding: Set by user.
     *)
    field_order: TAVFieldOrder;

    (* audio only *)
    sample_rate: Integer; ///< samples per second
    channels: Integer;    ///< number of audio channels

    (**
     * audio sample format
     * - encoding: Set by user.
     * - decoding: Set by libavcodec.
     *)
    sample_fmt: TAVSampleFormat;  ///< sample format

    (* The following data should not be initialized. *)
    (**
     * Number of samples per channel in an audio frame.
     *
     * - encoding: set by libavcodec in avcodec_open2(). Each submitted frame
     *   except the last must contain exactly frame_size samples per channel.
     *   May be 0 when the codec has AV_CODEC_CAP_VARIABLE_FRAME_SIZE set, then the
     *   frame size is not restricted.
     * - decoding: may be set by some decoders to indicate constant frame size
     *)
    frame_size: Integer;

    (**
     * Frame counter, set by libavcodec.
     *
     * - decoding: total number of frames returned from the decoder so far.
     * - encoding: total number of frames passed to the encoder so far.
     *
     *   @note the counter is not incremented if encoding/decoding resulted in
     *   an error.
     *)
    frame_number: Integer;

    (**
     * number of bytes per packet if constant and known or 0
     * Used by some WAV based audio codecs.
     *)
    block_align: Integer;

    (**
     * Audio cutoff bandwidth (0 means "automatic")
     * - encoding: Set by user.
     * - decoding: unused
     *)
    cutoff: Integer;

    (**
     * Audio channel layout.
     * - encoding: set by user.
     * - decoding: set by user, may be overwritten by libavcodec.
     *)
    channel_layout: Int64;

    (**
     * Request decoder to use this channel layout if it can (0 for default)
     * - encoding: unused
     * - decoding: Set by user.
     *)
    request_channel_layout: Int64;

    (**
     * Type of service that the audio stream conveys.
     * - encoding: Set by user.
     * - decoding: Set by libavcodec.
     *)
     audio_service_type: TAVAudioServiceType;

    (**
     * desired sample format
     * - encoding: Not used.
     * - decoding: Set by user.
     * Decoder will decode to this format if it can.
     *)
    request_sample_fmt: TAVSampleFormat;

    (**
     * This callback is called at the beginning of each frame to get data
     * buffer(s) for it. There may be one contiguous buffer for all the data or
     * there may be a buffer per each data plane or anything in between. What
     * this means is, you may set however many entries in buf[] you feel necessary.
     * Each buffer must be reference-counted using the AVBuffer API (see description
     * of buf[] below).
     *
     * The following fields will be set in the frame before this callback is
     * called:
     * - format
     * - width, height (video only)
     * - sample_rate, channel_layout, nb_samples (audio only)
     * Their values may differ from the corresponding values in
     * AVCodecContext. This callback must use the frame values, not the codec
     * context values, to calculate the required buffer size.
     *
     * This callback must fill the following fields in the frame:
     * - data[]
     * - linesize[]
     * - extended_data:
     *   * if the data is planar audio with more than 8 channels, then this
     *     callback must allocate and fill extended_data to contain all pointers
     *     to all data planes. data[] must hold as many pointers as it can.
     *     extended_data must be allocated with av_malloc() and will be freed in
     *     av_frame_unref().
     *   * otherwise extended_data must point to data
     * - buf[] must contain one or more pointers to AVBufferRef structures. Each of
     *   the frame's data and extended_data pointers must be contained in these. That
     *   is, one AVBufferRef for each allocated chunk of memory, not necessarily one
     *   AVBufferRef per data[] entry. See: av_buffer_create(), av_buffer_alloc(),
     *   and av_buffer_ref().
     * - extended_buf and nb_extended_buf must be allocated with av_malloc() by
     *   this callback and filled with the extra buffers if there are more
     *   buffers than buf[] can hold. extended_buf will be freed in
     *   av_frame_unref().
     *
     * If AV_CODEC_CAP_DR1 is not set then get_buffer2() must call
     * avcodec_default_get_buffer2() instead of providing buffers allocated by
     * some other means.
     *
     * Each data plane must be aligned to the maximum required by the target
     * CPU.
     *
     * @see avcodec_default_get_buffer2()
     *
     * Video:
     *
     * If AV_GET_BUFFER_FLAG_REF is set in flags then the frame may be reused
     * (read and/or written to if it is writable) later by libavcodec.
     *
     * avcodec_align_dimensions2() should be used to find the required width and
     * height, as they normally need to be rounded up to the next multiple of 16.
     *
     * Some decoders do not support linesizes changing between frames.
     *
     * If frame multithreading is used and thread_safe_callbacks is set,
     * this callback may be called from a different thread, but not from more
     * than one at once. Does not need to be reentrant.
     *
     * @see avcodec_align_dimensions2()
     *
     * Audio:
     *
     * Decoders request a buffer of a particular size by setting
     * AVFrame.nb_samples prior to calling get_buffer2(). The decoder may,
     * however, utilize only part of the buffer by setting AVFrame.nb_samples
     * to a smaller value in the output frame.
     *
     * As a convenience, av_samples_get_buffer_size() and
     * av_samples_fill_arrays() in libavutil may be used by custom get_buffer2()
     * functions to find the required data size and to fill data pointers and
     * linesize. In AVFrame.linesize, only linesize[0] may be set for audio
     * since all planes must be the same size.
     *
     * @see av_samples_get_buffer_size(), av_samples_fill_arrays()
     *
     * - encoding: unused
     * - decoding: Set by libavcodec, user can override.
     *)
    get_buffer2: function(s: PAVCodecContext; frame: PAVFrame; flags: Integer): Integer; cdecl;

    (**
     * If non-zero, the decoded audio and video frames returned from
     * avcodec_decode_video2() and avcodec_decode_audio4() are reference-counted
     * and are valid indefinitely. The caller must free them with
     * av_frame_unref() when they are not needed anymore.
     * Otherwise, the decoded frames must not be freed by the caller and are
     * only valid until the next decode call.
     *
     * This is always automatically enabled if avcodec_receive_frame() is used.
     *
     * - encoding: unused
     * - decoding: set by the caller before avcodec_open2().
     *)
    refcounted_frames: Integer;

    (* - encoding parameters *)
    qcompress: Single;  ///< amount of qscale change between easy & hard scenes (0.0-1.0)
    qblur: Single;      ///< amount of qscale smoothing over time (0.0-1.0)

    (**
     * minimum quantizer
     * - encoding: Set by user.
     * - decoding: unused
     *)
    qmin: Integer;

    (**
     * maximum quantizer
     * - encoding: Set by user.
     * - decoding: unused
     *)
    qmax: Integer;

    (**
     * maximum quantizer difference between frames
     * - encoding: Set by user.
     * - decoding: unused
     *)
    max_qdiff: Integer;

{$IFDEF FF_API_MPV_OPT}
    (**
     * @deprecated use encoder private options instead
     *)
    rc_qsquish: Single;

    rc_qmod_amp: Single;
    rc_qmod_freq: Integer;
{$ENDIF}

    (**
     * decoder bitstream buffer size
     * - encoding: Set by user.
     * - decoding: unused
     *)
    rc_buffer_size: Integer;

    (**
     * ratecontrol override, see RcOverride
     * - encoding: Allocated/set/freed by user.
     * - decoding: unused
     *)
    rc_override_count: Integer;
    rc_override: PRcOverride;

{$IFDEF FF_API_MPV_OPT}
    (**
     * @deprecated use encoder private options instead
     *)
    rc_eq: PAnsiChar;
{$ENDIF}

    (**
     * maximum bitrate
     * - encoding: Set by user.
     * - decoding: Set by user, may be overwritten by libavcodec.
     *)
    rc_max_rate: Int64;

    (**
     * minimum bitrate
     * - encoding: Set by user.
     * - decoding: unused
     *)
    rc_min_rate: Int64;

{$IFDEF FF_API_MPV_OPT}
    (**
     * @deprecated use encoder private options instead
     *)
    rc_buffer_aggressivity: Single;
    rc_initial_cplx: Single;
{$ENDIF}

    (**
     * Ratecontrol attempt to use, at maximum, <value> of what can be used without an underflow.
     * - encoding: Set by user.
     * - decoding: unused.
     *)
    rc_max_available_vbv_use: Single;

    (**
     * Ratecontrol attempt to use, at least, <value> times the amount needed to prevent a vbv overflow.
     * - encoding: Set by user.
     * - decoding: unused.
     *)
    rc_min_vbv_overflow_use: Single;

    (**
     * Number of bits which should be loaded into the rc buffer before decoding starts.
     * - encoding: Set by user.
     * - decoding: unused
     *)
    rc_initial_buffer_occupancy: Integer;

{$IFDEF FF_API_CODER_TYPE}
{
#define FF_CODER_TYPE_VLC       0
#define FF_CODER_TYPE_AC        1
#define FF_CODER_TYPE_RAW       2
#define FF_CODER_TYPE_RLE       3
#if FF_API_UNUSED_MEMBERS
#define FF_CODER_TYPE_DEFLATE   4
#endif /* FF_API_UNUSED_MEMBERS */
}
    (**
     * @deprecated use encoder private options instead
     *)
    coder_type: Integer;
{$ENDIF} (* FF_API_CODER_TYPE *)

{$IFDEF FF_API_PRIVATE_OPT}
    (** @deprecated use encoder private options instead *)
    context_model: Integer;
{$ENDIF}

{$IFDEF FF_API_MPV_OPT}
    (**
     * @deprecated use encoder private options instead
     *)
    lmin: Integer;

    (**
     * @deprecated use encoder private options instead
     *)
    lmax: Integer;
{$ENDIF}

{$IFDEF FF_API_PRIVATE_OPT}
    (** @deprecated use encoder private options instead *)
    frame_skip_threshold: Integer;

    (** @deprecated use encoder private options instead *)
    frame_skip_factor: Integer;

    (** @deprecated use encoder private options instead *)
    frame_skip_exp: Integer;

    (** @deprecated use encoder private options instead *)
    frame_skip_cmp: Integer;
{$ENDIF} (* FF_API_PRIVATE_OPT *)

    (**
     * trellis RD quantization
     * - encoding: Set by user.
     * - decoding: unused
     *)
    trellis: Integer;

{$IFDEF FF_API_PRIVATE_OPT}
    (** @deprecated use encoder private options instead *)
    min_prediction_order: Integer;

    (** @deprecated use encoder private options instead *)
    max_prediction_order: Integer;

    (** @deprecated use encoder private options instead *)
    timecode_frame_start: Int64;
{$ENDIF}

{$IFDEF FF_API_RTP_CALLBACK}
    (**
     * @deprecated unused
     *)
    (* The RTP callback: This function is called    *)
    (* every time the encoder has a packet to send. *)
    (* It depends on the encoder if the data starts *)
    (* with a Start Code (it should). H.263 does.   *)
    (* mb_nb contains the number of macroblocks     *)
    (* encoded in the RTP payload.                  *)
    //void (*rtp_callback)(struct AVCodecContext *avctx, void *data, int size, int mb_nb);
    rtp_callback: procedure(avctx: PAVCodecContext; data: Pointer; size, mb_nb: Integer);
{$ENDIF}

{$IFDEF FF_API_PRIVATE_OPT}
    (** @deprecated use encoder private options instead *)
    rtp_payload_size: Integer;   (* The size of the RTP payload: the coder will  *)
                            (* do its best to deliver a chunk with size     *)
                            (* below rtp_payload_size, the chunk will start *)
                            (* with a start code on some codecs like H.263. *)
                            (* This doesn't take account of any particular  *)
                            (* headers inside the transmitted RTP payload.  *)
{$ENDIF}

{$IFDEF FF_API_STAT_BITS}
    (* statistics, used for 2-pass encoding *)
    mv_bits: Integer;
    header_bits: Integer;
    i_tex_bits: Integer;
    p_tex_bits: Integer;
    i_count: Integer;
    p_count: Integer;
    skip_count: Integer;
    misc_bits: Integer;

    (** @deprecated this field is unused *)
    frame_bits: Integer;
{$ENDIF}

    (**
     * pass1 encoding statistics output buffer
     * - encoding: Set by libavcodec.
     * - decoding: unused
     *)
    stats_out: PAnsiChar;

    (**
     * pass2 encoding statistics input buffer
     * Concatenated stuff from stats_out of pass1 should be placed here.
     * - encoding: Allocated/set/freed by user.
     * - decoding: unused
     *)
    stats_in: PAnsiChar;

    (**
     * Work around bugs in encoders which sometimes cannot be detected automatically.
     * - encoding: Set by user
     * - decoding: Set by user
     *)
    workaround_bugs: Integer;
{
#define FF_BUG_AUTODETECT       1  ///< autodetection
#if FF_API_OLD_MSMPEG4
#define FF_BUG_OLD_MSMPEG4      2
#endif
#define FF_BUG_XVID_ILACE       4
#define FF_BUG_UMP4             8
#define FF_BUG_NO_PADDING       16
#define FF_BUG_AMV              32
#if FF_API_AC_VLC
#define FF_BUG_AC_VLC           0  ///< Will be removed, libavcodec can now handle these non-compliant files by default.
#endif
#define FF_BUG_QPEL_CHROMA      64
#define FF_BUG_STD_QPEL         128
#define FF_BUG_QPEL_CHROMA2     256
#define FF_BUG_DIRECT_BLOCKSIZE 512
#define FF_BUG_EDGE             1024
#define FF_BUG_HPEL_CHROMA      2048
#define FF_BUG_DC_CLIP          4096
#define FF_BUG_MS               8192 ///< Work around various bugs in Microsoft's broken decoders.
#define FF_BUG_TRUNCATED       16384
#define FF_BUG_IEDGE           32768
}

    (**
     * strictly follow the standard (MPEG-4, ...).
     * - encoding: Set by user.
     * - decoding: Set by user.
     * Setting this to STRICT or higher means the encoder and decoder will
     * generally do stupid things, whereas setting it to unofficial or lower
     * will mean the encoder might produce output that is not supported by all
     * spec-compliant decoders. Decoders don't differentiate between normal,
     * unofficial and experimental (that is, they always try to decode things
     * when they can) unless they are explicitly asked to behave stupidly
     * (=strictly conform to the specs)
     *)
    strict_std_compliance: Integer;
{
#define FF_COMPLIANCE_VERY_STRICT   2 ///< Strictly conform to an older more strict version of the spec or reference software.
#define FF_COMPLIANCE_STRICT        1 ///< Strictly conform to all the things in the spec no matter what consequences.
#define FF_COMPLIANCE_NORMAL        0
#define FF_COMPLIANCE_UNOFFICIAL   -1 ///< Allow unofficial extensions
#define FF_COMPLIANCE_EXPERIMENTAL -2 ///< Allow nonstandardized experimental things.
}

    (**
     * error concealment flags
     * - encoding: unused
     * - decoding: Set by user.
     *)
    error_concealment: Integer;
//#define FF_EC_GUESS_MVS   1
//#define FF_EC_DEBLOCK     2
//#define FF_EC_FAVOR_INTER 256

    (**
     * debug
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    debug: Integer;
{
#define FF_DEBUG_PICT_INFO   1
#define FF_DEBUG_RC          2
#define FF_DEBUG_BITSTREAM   4
#define FF_DEBUG_MB_TYPE     8
#define FF_DEBUG_QP          16
#if FF_API_DEBUG_MV
/**
 * @deprecated this option does nothing
 */
#define FF_DEBUG_MV          32
#endif
#define FF_DEBUG_DCT_COEFF   0x00000040
#define FF_DEBUG_SKIP        0x00000080
#define FF_DEBUG_STARTCODE   0x00000100
#if FF_API_UNUSED_MEMBERS
#define FF_DEBUG_PTS         0x00000200
#endif /* FF_API_UNUSED_MEMBERS */
#define FF_DEBUG_ER          0x00000400
#define FF_DEBUG_MMCO        0x00000800
#define FF_DEBUG_BUGS        0x00001000
#if FF_API_DEBUG_MV
#define FF_DEBUG_VIS_QP      0x00002000
#define FF_DEBUG_VIS_MB_TYPE 0x00004000
#endif
#define FF_DEBUG_BUFFERS     0x00008000
#define FF_DEBUG_THREADS     0x00010000
#define FF_DEBUG_GREEN_MD    0x00800000
#define FF_DEBUG_NOMC        0x01000000
}

{$IFDEF FF_API_DEBUG_MV}
    (**
     * debug
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    debug_mv: Integer;
{
#define FF_DEBUG_VIS_MV_P_FOR  0x00000001 // visualize forward predicted MVs of P-frames
#define FF_DEBUG_VIS_MV_B_FOR  0x00000002 // visualize forward predicted MVs of B-frames
#define FF_DEBUG_VIS_MV_B_BACK 0x00000004 // visualize backward predicted MVs of B-frames
}
{$ENDIF}
    (**
     * Error recognition; may misdetect some more or less valid parts as errors.
     * - encoding: unused
     * - decoding: Set by user.
     *)
    err_recognition: Integer;
{
/**
 * Verify checksums embedded in the bitstream (could be of either encoded or
 * decoded data, depending on the codec) and print an error message on mismatch.
 * If AV_EF_EXPLODE is also set, a mismatching checksum will result in the
 * decoder returning an error.
 */
#define AV_EF_CRCCHECK  (1<<0)
#define AV_EF_BITSTREAM (1<<1)          ///< detect bitstream specification deviations
#define AV_EF_BUFFER    (1<<2)          ///< detect improper bitstream length
#define AV_EF_EXPLODE   (1<<3)          ///< abort decoding on minor error detection

#define AV_EF_IGNORE_ERR (1<<15)        ///< ignore errors and continue
#define AV_EF_CAREFUL    (1<<16)        ///< consider things that violate the spec, are fast to calculate and have not been seen in the wild as errors
#define AV_EF_COMPLIANT  (1<<17)        ///< consider all spec non compliances as errors
#define AV_EF_AGGRESSIVE (1<<18)        ///< consider things that a sane encoder should not do as an error
}

    (**
     * opaque 64-bit number (generally a PTS) that will be reordered and
     * output in AVFrame.reordered_opaque
     * - encoding: unused
     * - decoding: Set by user.
     *)
    reordered_opaque: Int64;

    (**
     * Hardware accelerator in use
     * - encoding: unused.
     * - decoding: Set by libavcodec
     *)
    hwaccel: PAVHWAccel;

    (**
     * Hardware accelerator context.
     * For some hardware accelerators, a global context needs to be
     * provided by the user. In that case, this holds display-dependent
     * data FFmpeg cannot instantiate itself. Please refer to the
     * FFmpeg HW accelerator documentation to know how to fill this
     * is. e.g. for VA API, this is a struct vaapi_context.
     * - encoding: unused
     * - decoding: Set by user
     *)
    hwaccel_context: Pointer;

    (**
     * error
     * - encoding: Set by libavcodec if flags & AV_CODEC_FLAG_PSNR.
     * - decoding: unused
     *)
    error: array[0..AV_NUM_DATA_POINTERS-1] of Int64;

    (**
     * DCT algorithm, see FF_DCT_* below
     * - encoding: Set by user.
     * - decoding: unused
     *)
    dct_algo: Integer;
{
#define FF_DCT_AUTO    0
#define FF_DCT_FASTINT 1
#define FF_DCT_INT     2
#define FF_DCT_MMX     3
#define FF_DCT_ALTIVEC 5
#define FF_DCT_FAAN    6
}

    (**
     * IDCT algorithm, see FF_IDCT_* below.
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    idct_algo: Integer;
{
#define FF_IDCT_AUTO          0
#define FF_IDCT_INT           1
#define FF_IDCT_SIMPLE        2
#define FF_IDCT_SIMPLEMMX     3
#define FF_IDCT_ARM           7
#define FF_IDCT_ALTIVEC       8
#if FF_API_ARCH_SH4
#define FF_IDCT_SH4           9
#endif
#define FF_IDCT_SIMPLEARM     10
#if FF_API_UNUSED_MEMBERS
#define FF_IDCT_IPP           13
#endif /* FF_API_UNUSED_MEMBERS */
#define FF_IDCT_XVID          14
#if FF_API_IDCT_XVIDMMX
#define FF_IDCT_XVIDMMX       14
#endif /* FF_API_IDCT_XVIDMMX */
#define FF_IDCT_SIMPLEARMV5TE 16
#define FF_IDCT_SIMPLEARMV6   17
#if FF_API_ARCH_SPARC
#define FF_IDCT_SIMPLEVIS     18
#endif
#define FF_IDCT_FAAN          20
#define FF_IDCT_SIMPLENEON    22
#if FF_API_ARCH_ALPHA
#define FF_IDCT_SIMPLEALPHA   23
#endif
#define FF_IDCT_SIMPLEAUTO    128
}

    (**
     * bits per sample/pixel from the demuxer (needed for huffyuv).
     * - encoding: Set by libavcodec.
     * - decoding: Set by user.
     *)
    bits_per_coded_sample: Integer;

    (**
     * Bits per sample/pixel of internal libavcodec pixel/sample format.
     * - encoding: set by user.
     * - decoding: set by libavcodec.
     *)
    bits_per_raw_sample: Integer;

{$IFDEF FF_API_LOWRES}
    (**
     * low resolution decoding, 1-> 1/2 size, 2->1/4 size
     * - encoding: unused
     * - decoding: Set by user.
     *)
     lowres: Integer;
{$ENDIF}

{$IFDEF FF_API_CODED_FRAME}
    (**
     * the picture in the bitstream
     * - encoding: Set by libavcodec.
     * - decoding: unused
     *
     * @deprecated use the quality factor packet side data instead
     *)
    coded_frame: PAVFrame;
{$ENDIF}

    (**
     * thread count
     * is used to decide how many independent tasks should be passed to execute()
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    thread_count: Integer;

    (**
     * Which multithreading methods to use.
     * Use of FF_THREAD_FRAME will increase decoding delay by one frame per thread,
     * so clients which cannot provide future frames should not use it.
     *
     * - encoding: Set by user, otherwise the default is used.
     * - decoding: Set by user, otherwise the default is used.
     *)
    thread_type: Integer;
{
#define FF_THREAD_FRAME   1 ///< Decode more than one frame at once
#define FF_THREAD_SLICE   2 ///< Decode more than one part of a single frame at once
}

    (**
     * Which multithreading methods are in use by the codec.
     * - encoding: Set by libavcodec.
     * - decoding: Set by libavcodec.
     *)
    active_thread_type: Integer;

    (**
     * Set by the client if its custom get_buffer() callback can be called
     * synchronously from another thread, which allows faster multithreaded decoding.
     * draw_horiz_band() will be called from other threads regardless of this setting.
     * Ignored if the default get_buffer() is used.
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    thread_safe_callbacks: Integer;

    (**
     * The codec may call this to execute several independent things.
     * It will return only after finishing all tasks.
     * The user may replace this with some multithreaded implementation,
     * the default implementation will execute the parts serially.
     * @param count the number of things to execute
     * - encoding: Set by libavcodec, user can override.
     * - decoding: Set by libavcodec, user can override.
     *)
    execute: function (c: PAVCodecContext; func: TexecuteCall; arg2: Pointer; ret: PInteger; count, size: Integer): Integer; cdecl;

    (**
     * The codec may call this to execute several independent things.
     * It will return only after finishing all tasks.
     * The user may replace this with some multithreaded implementation,
     * the default implementation will execute the parts serially.
     * Also see avcodec_thread_init and e.g. the --enable-pthread configure option.
     * @param c context passed also to func
     * @param count the number of things to execute
     * @param arg2 argument passed unchanged to func
     * @param ret return values of executed functions, must have space for "count" values. May be NULL.
     * @param func function that will be called count times, with jobnr from 0 to count-1.
     *             threadnr will be in the range 0 to c->thread_count-1 < MAX_THREADS and so that no
     *             two instances of func executing at the same time will have the same threadnr.
     * @return always 0 currently, but code should handle a future improvement where when any call to func
     *         returns < 0 no further calls to func may be done and < 0 is returned.
     * - encoding: Set by libavcodec, user can override.
     * - decoding: Set by libavcodec, user can override.
     *)
    execute2: function (c: PAVCodecContext; func: Texecute2Call; arg2: Pointer; ret: PInteger; count: Integer): Integer; cdecl;

    (**
     * noise vs. sse weight for the nsse comparison function
     * - encoding: Set by user.
     * - decoding: unused
     *)
     nsse_weight: Integer;

    (**
     * profile
     * - encoding: Set by user.
     * - decoding: Set by libavcodec.
     *)
     profile: Integer;
{
#define FF_PROFILE_UNKNOWN -99
#define FF_PROFILE_RESERVED -100

#define FF_PROFILE_AAC_MAIN 0
#define FF_PROFILE_AAC_LOW  1
#define FF_PROFILE_AAC_SSR  2
#define FF_PROFILE_AAC_LTP  3
#define FF_PROFILE_AAC_HE   4
#define FF_PROFILE_AAC_HE_V2 28
#define FF_PROFILE_AAC_LD   22
#define FF_PROFILE_AAC_ELD  38
#define FF_PROFILE_MPEG2_AAC_LOW 128
#define FF_PROFILE_MPEG2_AAC_HE  131

#define FF_PROFILE_DNXHD         0
#define FF_PROFILE_DNXHR_LB      1
#define FF_PROFILE_DNXHR_SQ      2
#define FF_PROFILE_DNXHR_HQ      3
#define FF_PROFILE_DNXHR_HQX     4
#define FF_PROFILE_DNXHR_444     5

#define FF_PROFILE_DTS         20
#define FF_PROFILE_DTS_ES      30
#define FF_PROFILE_DTS_96_24   40
#define FF_PROFILE_DTS_HD_HRA  50
#define FF_PROFILE_DTS_HD_MA   60
#define FF_PROFILE_DTS_EXPRESS 70

#define FF_PROFILE_MPEG2_422    0
#define FF_PROFILE_MPEG2_HIGH   1
#define FF_PROFILE_MPEG2_SS     2
#define FF_PROFILE_MPEG2_SNR_SCALABLE  3
#define FF_PROFILE_MPEG2_MAIN   4
#define FF_PROFILE_MPEG2_SIMPLE 5

#define FF_PROFILE_H264_CONSTRAINED  (1<<9)  // 8+1; constraint_set1_flag
#define FF_PROFILE_H264_INTRA        (1<<11) // 8+3; constraint_set3_flag

#define FF_PROFILE_H264_BASELINE             66
#define FF_PROFILE_H264_CONSTRAINED_BASELINE (66|FF_PROFILE_H264_CONSTRAINED)
#define FF_PROFILE_H264_MAIN                 77
#define FF_PROFILE_H264_EXTENDED             88
#define FF_PROFILE_H264_HIGH                 100
#define FF_PROFILE_H264_HIGH_10              110
#define FF_PROFILE_H264_HIGH_10_INTRA        (110|FF_PROFILE_H264_INTRA)
#define FF_PROFILE_H264_MULTIVIEW_HIGH       118
#define FF_PROFILE_H264_HIGH_422             122
#define FF_PROFILE_H264_HIGH_422_INTRA       (122|FF_PROFILE_H264_INTRA)
#define FF_PROFILE_H264_STEREO_HIGH          128
#define FF_PROFILE_H264_HIGH_444             144
#define FF_PROFILE_H264_HIGH_444_PREDICTIVE  244
#define FF_PROFILE_H264_HIGH_444_INTRA       (244|FF_PROFILE_H264_INTRA)
#define FF_PROFILE_H264_CAVLC_444            44

#define FF_PROFILE_VC1_SIMPLE   0
#define FF_PROFILE_VC1_MAIN     1
#define FF_PROFILE_VC1_COMPLEX  2
#define FF_PROFILE_VC1_ADVANCED 3

#define FF_PROFILE_MPEG4_SIMPLE                     0
#define FF_PROFILE_MPEG4_SIMPLE_SCALABLE            1
#define FF_PROFILE_MPEG4_CORE                       2
#define FF_PROFILE_MPEG4_MAIN                       3
#define FF_PROFILE_MPEG4_N_BIT                      4
#define FF_PROFILE_MPEG4_SCALABLE_TEXTURE           5
#define FF_PROFILE_MPEG4_SIMPLE_FACE_ANIMATION      6
#define FF_PROFILE_MPEG4_BASIC_ANIMATED_TEXTURE     7
#define FF_PROFILE_MPEG4_HYBRID                     8
#define FF_PROFILE_MPEG4_ADVANCED_REAL_TIME         9
#define FF_PROFILE_MPEG4_CORE_SCALABLE             10
#define FF_PROFILE_MPEG4_ADVANCED_CODING           11
#define FF_PROFILE_MPEG4_ADVANCED_CORE             12
#define FF_PROFILE_MPEG4_ADVANCED_SCALABLE_TEXTURE 13
#define FF_PROFILE_MPEG4_SIMPLE_STUDIO             14
#define FF_PROFILE_MPEG4_ADVANCED_SIMPLE           15

#define FF_PROFILE_JPEG2000_CSTREAM_RESTRICTION_0   1
#define FF_PROFILE_JPEG2000_CSTREAM_RESTRICTION_1   2
#define FF_PROFILE_JPEG2000_CSTREAM_NO_RESTRICTION  32768
#define FF_PROFILE_JPEG2000_DCINEMA_2K              3
#define FF_PROFILE_JPEG2000_DCINEMA_4K              4

#define FF_PROFILE_VP9_0                            0
#define FF_PROFILE_VP9_1                            1
#define FF_PROFILE_VP9_2                            2
#define FF_PROFILE_VP9_3                            3

#define FF_PROFILE_HEVC_MAIN                        1
#define FF_PROFILE_HEVC_MAIN_10                     2
#define FF_PROFILE_HEVC_MAIN_STILL_PICTURE          3
#define FF_PROFILE_HEVC_REXT                        4
}
    (**
     * level
     * - encoding: Set by user.
     * - decoding: Set by libavcodec.
     *)
     level: Integer;
//#define FF_LEVEL_UNKNOWN -99

    (**
     * Skip loop filtering for selected frames.
     * - encoding: unused
     * - decoding: Set by user.
     *)
    skip_loop_filter: TAVDiscard;

    (**
     * Skip IDCT/dequantization for selected frames.
     * - encoding: unused
     * - decoding: Set by user.
     *)
    skip_idct: TAVDiscard;

    (**
     * Skip decoding for selected frames.
     * - encoding: unused
     * - decoding: Set by user.
     *)
    skip_frame: TAVDiscard;

    (**
     * Header containing style information for text subtitles.
     * For SUBTITLE_ASS subtitle type, it should contain the whole ASS
     * [Script Info] and [V4+ Styles] section, plus the [Events] line and
     * the Format line following. It shouldn't include any Dialogue line.
     * - encoding: Set/allocated/freed by user (before avcodec_open2())
     * - decoding: Set/allocated/freed by libavcodec (by avcodec_open2())
     *)
    subtitle_header: PByte;
    subtitle_header_size: Integer;

{$IFDEF FF_API_ERROR_RATE}
    (**
     * @deprecated use the 'error_rate' private AVOption of the mpegvideo
     * encoders
     *)
    error_rate: Integer;
{$ENDIF}

{$IFDEF FF_API_VBV_DELAY}
    (**
     * VBV delay coded in the last frame (in periods of a 27 MHz clock).
     * Used for compliant TS muxing.
     * - encoding: Set by libavcodec.
     * - decoding: unused.
     * @deprecated this value is now exported as a part of
     * AV_PKT_DATA_CPB_PROPERTIES packet side data
     *)
    vbv_delay: Int64;
{$ENDIF}

{$IFDEF FF_API_SIDEDATA_ONLY_PKT}
    (**
     * Encoding only and set by default. Allow encoders to output packets
     * that do not contain any encoded data, only side data.
     *
     * Some encoders need to output such packets, e.g. to update some stream
     * parameters at the end of encoding.
     *
     * @deprecated this field disables the default behaviour and
     *             it is kept only for compatibility.
     *)
    side_data_only_packets: Integer;
{$ENDIF}

    (**
     * Audio only. The number of "priming" samples (padding) inserted by the
     * encoder at the beginning of the audio. I.e. this number of leading
     * decoded samples must be discarded by the caller to get the original audio
     * without leading padding.
     *
     * - decoding: unused
     * - encoding: Set by libavcodec. The timestamps on the output packets are
     *             adjusted by the encoder so that they always refer to the
     *             first sample of the data actually contained in the packet,
     *             including any added padding.  E.g. if the timebase is
     *             1/samplerate and the timestamp of the first input sample is
     *             0, the timestamp of the first output packet will be
     *             -initial_padding.
     *)
    initial_padding: Integer;

    (**
     * - decoding: For codecs that store a framerate value in the compressed
     *             bitstream, the decoder may export it here. { 0, 1} when
     *             unknown.
     * - encoding: May be used to signal the framerate of CFR content to an
     *             encoder.
     *)
    framerate: TAVRational;

    (**
     * Nominal unaccelerated pixel format, see AV_PIX_FMT_xxx.
     * - encoding: unused.
     * - decoding: Set by libavcodec before calling get_format()
     *)
    sw_pix_fmt: TAVPixelFormat;

    (**
     * Timebase in which pkt_dts/pts and AVPacket.dts/pts are.
     * - encoding unused.
     * - decoding set by user.
     *)
    pkt_timebase: TAVRational;

    (**
     * AVCodecDescriptor
     * - encoding: unused.
     * - decoding: set by libavcodec.
     *)
    codec_descriptor: PAVCodecDescriptor;

{$IFNDEF FF_API_LOWRES}
    (**
     * low resolution decoding, 1-> 1/2 size, 2->1/4 size
     * - encoding: unused
     * - decoding: Set by user.
     *)
    lowres: Integer;
{$ENDIF}

    (**
     * Current statistics for PTS correction.
     * - decoding: maintained and used by libavcodec, not intended to be used by user apps
     * - encoding: unused
     *)
    pts_correction_num_faulty_pts: Int64; /// Number of incorrect PTS values so far
    pts_correction_num_faulty_dts: Int64; /// Number of incorrect DTS values so far
    pts_correction_last_pts: Int64;       /// PTS of the last frame
    pts_correction_last_dts: Int64;       /// DTS of the last frame

    (**
     * Character encoding of the input subtitles file.
     * - decoding: set by user
     * - encoding: unused
     *)
    sub_charenc: PAnsiChar;

    (**
     * Subtitles character encoding mode. Formats or codecs might be adjusting
     * this setting (if they are doing the conversion themselves for instance).
     * - decoding: set by libavcodec
     * - encoding: unused
     *)
    sub_charenc_mode: Integer;
{
#define FF_SUB_CHARENC_MODE_DO_NOTHING  -1  ///< do nothing (demuxer outputs a stream supposed to be already in UTF-8, or the codec is bitmap for instance)
#define FF_SUB_CHARENC_MODE_AUTOMATIC    0  ///< libavcodec will select the mode itself
#define FF_SUB_CHARENC_MODE_PRE_DECODER  1  ///< the AVPacket data needs to be recoded to UTF-8 before being fed to the decoder, requires iconv
}

    (**
     * Skip processing alpha if supported by codec.
     * Note that if the format uses pre-multiplied alpha (common with VP6,
     * and recommended due to better video quality/compression)
     * the image will look as if alpha-blended onto a black background.
     * However for formats that do not use pre-multiplied alpha
     * there might be serious artefacts (though e.g. libswscale currently
     * assumes pre-multiplied alpha anyway).
     *
     * - decoding: set by user
     * - encoding: unused
     *)
    skip_alpha: Integer;

    (**
     * Number of samples to skip after a discontinuity
     * - decoding: unused
     * - encoding: set by libavcodec
     *)
    seek_preroll: Integer;

{$IFNDEF FF_API_DEBUG_MV}
    (**
     * debug motion vectors
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    debug_mv: Integer;
//#define FF_DEBUG_VIS_MV_P_FOR  0x00000001 //visualize forward predicted MVs of P frames
//#define FF_DEBUG_VIS_MV_B_FOR  0x00000002 //visualize forward predicted MVs of B frames
//#define FF_DEBUG_VIS_MV_B_BACK 0x00000004 //visualize backward predicted MVs of B frames
{$ENDIF}

    (**
     * custom intra quantization matrix
     * - encoding: Set by user, can be NULL.
     * - decoding: unused.
     *)
    chroma_intra_matrix: PWord;

    (**
     * dump format separator.
     * can be ", " or "\n      " or anything else
     * - encoding: Set by user.
     * - decoding: Set by user.
     *)
    dump_separator: PByte;

    (**
     * ',' separated list of allowed decoders.
     * If NULL then all are allowed
     * - encoding: unused
     * - decoding: set by user
     *)
    codec_whitelist: PAnsiChar;

    (*
     * Properties of the stream that gets decoded
     * - encoding: unused
     * - decoding: set by libavcodec
     *)
    properties: Cardinal;
//#define FF_CODEC_PROPERTY_LOSSLESS        0x00000001
//#define FF_CODEC_PROPERTY_CLOSED_CAPTIONS 0x00000002

    (**
     * Additional data associated with the entire coded stream.
     *
     * - decoding: unused
     * - encoding: may be set by libavcodec after avcodec_open2().
     *)
    coded_side_data: PAVPacketSideData;
    nb_coded_side_data: Integer;

    (**
     * A reference to the AVHWFramesContext describing the input (for encoding)
     * or output (decoding) frames. The reference is set by the caller and
     * afterwards owned (and freed) by libavcodec - it should never be read by
     * the caller after being set.
     *
     * - decoding: This field should be set by the caller from the get_format()
     *             callback. The previous reference (if any) will always be
     *             unreffed by libavcodec before the get_format() call.
     *
     *             If the default get_buffer2() is used with a hwaccel pixel
     *             format, then this AVHWFramesContext will be used for
     *             allocating the frame buffers.
     *
     * - encoding: For hardware encoders configured to use a hwaccel pixel
     *             format, this field should be set by the caller to a reference
     *             to the AVHWFramesContext describing input frames.
     *             AVHWFramesContext.format must be equal to
     *             AVCodecContext.pix_fmt.
     *
     *             This field should be set before avcodec_open2() is called.
     *)
    hw_frames_ctx: PAVBufferRef;

    (**
     * Control the form of AVSubtitle.rects[N]->ass
     * - decoding: set by user
     * - encoding: unused
     *)
    sub_text_format: Integer;
//#define FF_SUB_TEXT_FMT_ASS              0
//#if FF_API_ASS_TIMING
//#define FF_SUB_TEXT_FMT_ASS_WITH_TIMINGS 1
//#endif

    (**
     * Audio only. The amount of padding (in samples) appended by the encoder to
     * the end of the audio. I.e. this number of decoded samples must be
     * discarded by the caller from the end of the stream to get the original
     * audio without any trailing padding.
     *
     * - decoding: unused
     * - encoding: unused
     *)
    trailing_padding: Integer;

    (**
     * The number of pixels per image to maximally accept.
     *
     * - decoding: set by user
     * - encoding: set by user
     *)
    max_pixels: Int64;

    (**
     * A reference to the AVHWDeviceContext describing the device which will
     * be used by a hardware encoder/decoder.  The reference is set by the
     * caller and afterwards owned (and freed) by libavcodec.
     *
     * This should be used if either the codec device does not require
     * hardware frames or any that are used are to be allocated internally by
     * libavcodec.  If the user wishes to supply any of the frames used as
     * encoder input or decoder output then hw_frames_ctx should be used
     * instead.  When hw_frames_ctx is set in get_format() for a decoder, this
     * field will be ignored while decoding the associated stream segment, but
     * may again be used on a following one after another get_format() call.
     *
     * For both encoders and decoders this field should be set before
     * avcodec_open2() is called and must not be written to thereafter.
     *
     * Note that some decoders may require this field to be set initially in
     * order to support hw_frames_ctx at all - in that case, all frames
     * contexts used must be created on the same device.
     *)
    hw_device_ctx: PAVBufferRef;

    (**
     * Bit set of AV_HWACCEL_FLAG_* flags, which affect hardware accelerated
     * decoding (if active).
     * - encoding: unused
     * - decoding: Set by user (either before avcodec_open2(), or in the
     *             AVCodecContext.get_format callback)
     *)
    hwaccel_flags: Integer;
  end;

  PAVCodecDefault = ^TAVCodecDefault;
  TAVCodecDefault = record
    // need {$ALIGN 8}
    // defined in libavcodec/internal.h
  end;

  PAVSubtitle = ^TAVSubtitle;

(**
 * AVCodec.
 *)
  TAVCodec = record
    (**
     * Name of the codec implementation.
     * The name is globally unique among encoders and among decoders (but an
     * encoder and a decoder can share the same name).
     * This is the primary way to find a codec from the user perspective.
     *)
    name: PAnsiChar;
    (**
     * Descriptive name for the codec, meant to be more human readable than name.
     * You should use the NULL_IF_CONFIG_SMALL() macro to define it.
     *)
    long_name: PAnsiChar;
    ttype: TAVMediaType;
    id: TAVCodecID;
    (**
     * Codec capabilities.
     * see AV_CODEC_CAP_*
     *)
    capabilities: Integer;
    supported_framerates: PAVRational;  ///< array of supported framerates, or NULL if any, array is terminated by {0,0}
    pix_fmts: PAVPixelFormat;           ///< array of supported pixel formats, or NULL if unknown, array is terminated by -1
    supported_samplerates: PInteger;    ///< array of supported audio samplerates, or NULL if unknown, array is terminated by 0
    sample_fmts: PAVSampleFormat;       ///< array of supported sample formats, or NULL if unknown, array is terminated by -1
    channel_layouts: PInt64;            ///< array of support channel layouts, or NULL if unknown. array is terminated by 0
    max_lowres: Byte;                   ///< maximum value for lowres supported by the decoder
    priv_class: PAVClass;               ///< AVClass for the private context
    profiles: PAVProfile;               ///< array of recognized profiles, or NULL if unknown, array is terminated by {FF_PROFILE_UNKNOWN}

    (*****************************************************************
     * No fields below this line are part of the public API. They
     * may not be used outside of libavcodec and can be changed and
     * removed at will.
     * New public fields should be added right above.
     *****************************************************************
     *)
    priv_data_size: Integer;
    next: PAVCodec;
    (**
     * @name Frame-level threading support functions
     * @{
     *)
    (**
     * If defined, called on thread contexts when they are created.
     * If the codec allocates writable tables in init(), re-allocate them here.
     * priv_data will be set to a copy of the original.
     *)
    init_thread_copy: function(ctx: PAVCodecContext): Integer; cdecl;
    (**
     * Copy necessary context variables from a previous thread context to the current one.
     * If not defined, the next thread will start automatically; otherwise, the codec
     * must call ff_thread_finish_setup().
     *
     * dst and src will (rarely) point to the same context, in which case memcpy should be skipped.
     *)
    update_thread_context: function(dst, src: PAVCodecContext): Integer; cdecl;
    (** @} *)

    (**
     * Private codec-specific defaults.
     *)
    defaults: PAVCodecDefault;

    (**
     * Initialize codec static data, called from avcodec_register().
     *)
    init_static_data: function(codec: PAVCodec): Pointer; cdecl;

    init: function(avctx: PAVCodecContext): Integer; cdecl;
    encode_sub: function(avctx: PAVCodecContext; buf: PByte; buf_size: Integer;
                      const sub: PAVSubtitle): Integer; cdecl;
    (**
     * Encode data to an AVPacket.
     *
     * @param      avctx          codec context
     * @param      avpkt          output AVPacket (may contain a user-provided buffer)
     * @param[in]  frame          AVFrame containing the raw data to be encoded
     * @param[out] got_packet_ptr encoder sets to 0 or 1 to indicate that a
     *                            non-empty packet was returned in avpkt.
     * @return 0 on success, negative error code on failure
     *)
    encode2: function(avctx: PAVCodecContext; avpkt: PAVPacket; const frame: PAVFrame;
                      got_packet_ptr: PInteger): Integer; cdecl;
    decode: function(avcctx: PAVCodecContext; outdata: Pointer; outdata_size: PInteger; avpkt: PAVPacket): Integer; cdecl;
    close: function(avcctx: PAVCodecContext): Integer; cdecl;
    (**
     * Decode/encode API with decoupled packet/frame dataflow. The API is the
     * same as the avcodec_ prefixed APIs (avcodec_send_frame() etc.), except
     * that:
     * - never called if the codec is closed or the wrong type,
     * - AVPacket parameter change side data is applied right before calling
     *   AVCodec->send_packet,
     * - if AV_CODEC_CAP_DELAY is not set, drain packets or frames are never sent,
     * - only one drain packet is ever passed down (until the next flush()),
     * - a drain AVPacket is always NULL (no need to check for avpkt->size).
     *)
    send_frame: function(avctx: PAVCodecContext; const frame: PAVFrame): Integer; cdecl;
    send_packet: function(avctx: PAVCodecContext; const avpkt: PAVPacket): Integer; cdecl;
    receive_frame: function(avctx: PAVCodecContext; frame: PAVFrame): Integer; cdecl;
    receive_packet: function(avctx: PAVCodecContext; avpkt: PAVPacket): Integer; cdecl;
    (**
     * Flush buffers.
     * Will be called when seeking
     *)
    flush: procedure(avcctx: PAVCodecContext); cdecl;
    (**
     * Internal codec capabilities.
     * See FF_CODEC_CAP_* in internal.h
     *)
    caps_internal: Integer;
  end;

  PMpegEncContext = ^TMpegEncContext;
  TMpegEncContext = record
    // need {$ALIGN 8}
    // defined in libavcodec/mpegvideo.h
  end;

(**
 * @defgroup lavc_hwaccel AVHWAccel
 * @{
 *)
  TAVHWAccel = record
    (**
     * Name of the hardware accelerated codec.
     * The name is globally unique among encoders and among decoders (but an
     * encoder and a decoder can share the same name).
     *)
    name: PAnsiChar;

    (**
     * Type of codec implemented by the hardware accelerator.
     *
     * See AVMEDIA_TYPE_xxx
     *)
    ttype: TAVMediaType;

    (**
     * Codec implemented by the hardware accelerator.
     *
     * See AV_CODEC_ID_xxx
     *)
    id: TAVCodecID;

    (**
     * Supported pixel format.
     *
     * Only hardware accelerated formats are supported here.
     *)
    pix_fmt: TAVPixelFormat;

    (**
     * Hardware accelerated codec capabilities.
     * see HWACCEL_CODEC_CAP_*
     *)
    capabilities: Integer;

    (*****************************************************************
     * No fields below this line are part of the public API. They
     * may not be used outside of libavcodec and can be changed and
     * removed at will.
     * New public fields should be added right above.
     *****************************************************************
     *)
    next: PAVHWAccel;

    (**
     * Allocate a custom buffer
     *)
    alloc_frame: function(avctx: PAVCodecContext; frame: PAVFrame): Integer; cdecl;

    (**
     * Called at the beginning of each frame or field picture.
     *
     * Meaningful frame information (codec specific) is guaranteed to
     * be parsed at this point. This function is mandatory.
     *
     * Note that buf can be NULL along with buf_size set to 0.
     * Otherwise, this means the whole frame is available at this point.
     *
     * @param avctx the codec context
     * @param buf the frame data buffer base
     * @param buf_size the size of the frame in bytes
     * @return zero if successful, a negative value otherwise
     *)
    start_frame: function(avctx: PAVCodecContext; const buf: PByte; buf_size: Cardinal): Integer; cdecl;

    (**
     * Callback for each slice.
     *
     * Meaningful slice information (codec specific) is guaranteed to
     * be parsed at this point. This function is mandatory.
     * The only exception is XvMC, that works on MB level.
     *
     * @param avctx the codec context
     * @param buf the slice data buffer base
     * @param buf_size the size of the slice in bytes
     * @return zero if successful, a negative value otherwise
     *)
    decode_slice: function(avctx: PAVCodecContext; const buf: PByte; buf_size: Cardinal): Integer; cdecl;

    (**
     * Called at the end of each frame or field picture.
     *
     * The whole picture is parsed at this point and can now be sent
     * to the hardware accelerator. This function is mandatory.
     *
     * @param avctx the codec context
     * @return zero if successful, a negative value otherwise
     *)
    end_frame: function(avctx: PAVCodecContext): Integer; cdecl;

    (**
     * Size of per-frame hardware accelerator private data.
     *
     * Private data is allocated with av_mallocz() before
     * AVCodecContext.get_buffer() and deallocated after
     * AVCodecContext.release_buffer().
     *)
    frame_priv_data_size: Integer;

    (**
     * Called for every Macroblock in a slice.
     *
     * XvMC uses it to replace the ff_mpv_decode_mb().
     * Instead of decoding to raw picture, MB parameters are
     * stored in an array provided by the video driver.
     *
     * @param s the mpeg context
     *)
    decode_mb: procedure(s: PMpegEncContext); cdecl;

    (**
     * Initialize the hwaccel private data.
     *
     * This will be called from ff_get_format(), after hwaccel and
     * hwaccel_context are set and the hwaccel private data in AVCodecInternal
     * is allocated.
     *)
    init: function(avctx: PAVCodecContext): Integer; cdecl;

    (**
     * Uninitialize the hwaccel private data.
     *
     * This will be called from get_format() or avcodec_close(), after hwaccel
     * and hwaccel_context are already uninitialized.
     *)
    uninit: function(avctx: PAVCodecContext): Integer; cdecl;

    (**
     * Size of the private data to allocate in
     * AVCodecInternal.hwaccel_priv_data.
     *)
    priv_data_size: Integer;

    (**
     * Internal hwaccel capabilities.
     *)
    caps_internal: Integer;
  end;

(**
 * @}
 *)

{$IFDEF FF_API_AVPICTURE}
(**
 * @defgroup lavc_picture AVPicture
 *
 * Functions for working with AVPicture
 * @{
 *)

(**
 * Picture data structure.
 *
 * Up to four components can be stored into it, the last component is
 * alpha.
 * @deprecated use AVFrame or imgutils functions instead
 *)
  PAVPicture = ^TAVPicture;
  TAVPicture = record
    data: array[0..AV_NUM_DATA_POINTERS-1] of PByte;        ///< pointers to the image data planes
    linesize: array[0..AV_NUM_DATA_POINTERS-1] of Integer;  ///< number of bytes per line
  end;

(**
 * @}
 *)
{$ENDIF}

  TAVSubtitleType = (
    SUBTITLE_NONE,

    SUBTITLE_BITMAP,                ///< A bitmap, pict will be set

    (**
     * Plain text, the text field must be set by the decoder and is
     * authoritative. ass and pict fields may contain approximations.
     *)
    SUBTITLE_TEXT,

    (**
     * Formatted text, the ass field must be set by the decoder and is
     * authoritative. pict and text fields may contain approximations.
     *)
    SUBTITLE_ASS
  );

//#define AV_SUBTITLE_FLAG_FORCED 0x00000001

  PPAVSubtitleRect = ^PAVSubtitleRect;
  PAVSubtitleRect = ^TAVSubtitleRect;
  TAVSubtitleRect = record
    x: Integer;         ///< top left corner  of pict, undefined when pict is not set
    y: Integer;         ///< top left corner  of pict, undefined when pict is not set
    w: Integer;         ///< width            of pict, undefined when pict is not set
    h: Integer;         ///< height           of pict, undefined when pict is not set
    nb_colors: Integer; ///< number of colors in pict, undefined when pict is not set

{$IFDEF FF_API_AVPICTURE}
    (**
     * @deprecated unused
     *)
    pict: TAVPicture;
{$ENDIF}

    (**
     * data+linesize for the bitmap of this subtitle.
     * Can be set for text/ass as well once they are rendered.
     *)
    data: array[0..3] of PByte;
    linesize: array[0..3] of Integer;

    ttype: TAVSubtitleType;

    text: PAnsiChar;                     ///< 0 terminated plain UTF-8 text

    (**
     * 0 terminated ASS/SSA compatible event line.
     * The presentation of this is unaffected by the other values in this
     * struct.
     *)
    ass: PAnsiChar;

    flags: Integer;
  end;

  TAVSubtitle = record
    format: Word; (* 0 = graphics *)
    start_display_time: Cardinal; (* relative to packet pts, in ms *)
    end_display_time: Cardinal; (* relative to packet pts, in ms *)
    num_rects: Cardinal;
    rects: PPAVSubtitleRect;
    pts: Int64;    ///< Same as packet pts, in AV_TIME_BASE
  end;

(**
 * This struct describes the properties of an encoded stream.
 *
 * sizeof(AVCodecParameters) is not a part of the public ABI, this struct must
 * be allocated with avcodec_parameters_alloc() and freed with
 * avcodec_parameters_free().
 *)
  PPAVCodecParameters = ^PAVCodecParameters;
  PAVCodecParameters = ^TAVCodecParameters;
  TAVCodecParameters = record
    (**
     * General type of the encoded data.
     *)
    codec_type: TAVMediaType;
    (**
     * Specific type of the encoded data (the codec used).
     *)
    codec_id: TAVCodecID;
    (**
     * Additional information about the codec (corresponds to the AVI FOURCC).
     *)
    //unsigned int codec_tag;
    //codec_tag: Cardinal;
    //codec_tag: array[0..3] of AnsiChar;
    codec_tag: packed record
      case Integer of
        0: (tag: Cardinal);
        1: (fourcc: array[0..3] of AnsiChar);
        2: (fourbb: array[0..3] of Byte);
      end;

    (**
     * Extra binary data needed for initializing the decoder, codec-dependent.
     *
     * Must be allocated with av_malloc() and will be freed by
     * avcodec_parameters_free(). The allocated size of extradata must be at
     * least extradata_size + AV_INPUT_BUFFER_PADDING_SIZE, with the padding
     * bytes zeroed.
     *)
    extradata: PByte;
    (**
     * Size of the extradata content in bytes.
     *)
    extradata_size: Integer;

    (**
     * - video: the pixel format, the value corresponds to enum AVPixelFormat.
     * - audio: the sample format, the value corresponds to enum AVSampleFormat.
     *)
    format: Integer;

    (**
     * The average bitrate of the encoded data (in bits per second).
     *)
    bit_rate: Int64;

    (**
     * The number of bits per sample in the codedwords.
     *
     * This is basically the bitrate per sample. It is mandatory for a bunch of
     * formats to actually decode them. It's the number of bits for one sample in
     * the actual coded bitstream.
     *
     * This could be for example 4 for ADPCM
     * For PCM formats this matches bits_per_raw_sample
     * Can be 0
     *)
    bits_per_coded_sample: Integer;

    (**
     * This is the number of valid bits in each output sample. If the
     * sample format has more bits, the least significant bits are additional
     * padding bits, which are always 0. Use right shifts to reduce the sample
     * to its actual size. For example, audio formats with 24 bit samples will
     * have bits_per_raw_sample set to 24, and format set to AV_SAMPLE_FMT_S32.
     * To get the original sample use "(int32_t)sample >> 8"."
     *
     * For ADPCM this might be 12 or 16 or similar
     * Can be 0
     *)
    bits_per_raw_sample: Integer;

    (**
     * Codec-specific bitstream restrictions that the stream conforms to.
     *)
    profile: Integer;
    level: Integer;

    (**
     * Video only. The dimensions of the video frame in pixels.
     *)
    width: Integer;
    height: Integer;

    (**
     * Video only. The aspect ratio (width / height) which a single pixel
     * should have when displayed.
     *
     * When the aspect ratio is unknown / undefined, the numerator should be
     * set to 0 (the denominator may have any value).
     *)
    sample_aspect_ratio: TAVRational;

    (**
     * Video only. The order of the fields in interlaced video.
     *)
    field_order: TAVFieldOrder;

    (**
     * Video only. Additional colorspace characteristics.
     *)
    color_range: TAVColorRange;
    color_primaries: TAVColorPrimaries;
    color_trc: TAVColorTransferCharacteristic;
    color_space: TAVColorSpace;
    chroma_location: TAVChromaLocation;

    (**
     * Video only. Number of delayed frames.
     *)
    video_delay: Integer;

    (**
     * Audio only. The channel layout bitmask. May be 0 if the channel layout is
     * unknown or unspecified, otherwise the number of bits set must be equal to
     * the channels field.
     *)
    channel_layout: Int64;
    (**
     * Audio only. The number of audio channels.
     *)
    channels: Integer;
    (**
     * Audio only. The number of audio samples per second.
     *)
    sample_rate: Integer;
    (**
     * Audio only. The number of bytes per coded audio frame, required by some
     * formats.
     *
     * Corresponds to nBlockAlign in WAVEFORMATEX.
     *)
    block_align: Integer;
    (**
     * Audio only. Audio frame size, if known. Required by some formats to be static.
     *)
    frame_size: Integer;

    (**
     * Audio only. The amount of padding (in samples) inserted by the encoder at
     * the beginning of the audio. I.e. this number of leading decoded samples
     * must be discarded by the caller to get the original audio without leading
     * padding.
     *)
    initial_padding: Integer;
    (**
     * Audio only. The amount of padding (in samples) appended by the encoder to
     * the end of the audio. I.e. this number of decoded samples must be
     * discarded by the caller from the end of the stream to get the original
     * audio without any trailing padding.
     *)
    trailing_padding: Integer;
    (**
     * Audio only. Number of samples to skip after a discontinuity.
     *)
    seek_preroll: Integer;
  end;

//TODO: API return record  function av_codec_get_pkt_timebase(const avctx: PAVCodecContext): TAVRational; cdecl; external AVCODEC_LIBNAME name _PU + 'av_codec_get_pkt_timebase';
procedure av_codec_set_pkt_timebase(avctx: PAVCodecContext; val: TAVRational); cdecl; external AVCODEC_LIBNAME name _PU + 'av_codec_set_pkt_timebase';

function av_codec_get_codec_descriptor(const avctx: PAVCodecContext): PAVCodecDescriptor; cdecl; external AVCODEC_LIBNAME name _PU + 'av_codec_get_codec_descriptor';
procedure av_codec_set_codec_descriptor(avctx: PAVCodecContext; const desc: PAVCodecDescriptor); cdecl; external AVCODEC_LIBNAME name _PU + 'av_codec_set_codec_descriptor';

function av_codec_get_codec_properties(const avctx: PAVCodecContext): Cardinal; cdecl; external AVCODEC_LIBNAME name _PU + 'av_codec_get_codec_properties';

function av_codec_get_lowres(const avctx: PAVCodecContext): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_codec_get_lowres';
procedure av_codec_set_lowres(avctx: PAVCodecContext; val: Integer); cdecl; external AVCODEC_LIBNAME name _PU + 'av_codec_set_lowres';

function av_codec_get_seek_preroll(const avctx: PAVCodecContext): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_codec_get_seek_preroll';
procedure av_codec_set_seek_preroll(avctx: PAVCodecContext; val: Integer); cdecl; external AVCODEC_LIBNAME name _PU + 'av_codec_set_seek_preroll';

function av_codec_get_chroma_intra_matrix(const avctx: PAVCodecContext): PWord; cdecl; external AVCODEC_LIBNAME name _PU + 'av_codec_get_chroma_intra_matrix';
procedure av_codec_set_chroma_intra_matrix(avctx: PAVCodecContext; val: PWord); cdecl; external AVCODEC_LIBNAME name _PU + 'av_codec_set_chroma_intra_matrix';

function av_codec_get_max_lowres(const codec: PAVCodec): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_codec_get_max_lowres';

(**
 * If c is NULL, returns the first registered codec,
 * if c is non-NULL, returns the next registered codec after c,
 * or NULL if c is the last one.
 *)
function av_codec_next(const c: PAVCodec): PAVCodec; cdecl; external AVCODEC_LIBNAME name _PU + 'av_codec_next';

(**
 * Return the LIBAVCODEC_VERSION_INT constant.
 *)
function avcodec_version: Cardinal; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_version';

(**
 * Return the libavcodec build-time configuration.
 *)
function avcodec_configuration: PAnsiChar; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_configuration';

(**
 * Return the libavcodec license.
 *)
function avcodec_license: PAnsiChar; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_license';

(**
 * Register the codec codec and initialize libavcodec.
 *
 * @warning either this function or avcodec_register_all() must be called
 * before any other libavcodec functions.
 *
 * @see avcodec_register_all()
 *)
procedure avcodec_register(codec: PAVCodec); cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_register';

(**
 * Register all the codecs, parsers and bitstream filters which were enabled at
 * configuration time. If you do not call this function you can select exactly
 * which formats you want to support, by using the individual registration
 * functions.
 *
 * @see avcodec_register
 * @see av_register_codec_parser
 * @see av_register_bitstream_filter
 *)
procedure avcodec_register_all; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_register_all';

(**
 * Allocate an AVCodecContext and set its fields to default values. The
 * resulting struct should be freed with avcodec_free_context().
 *
 * @param codec if non-NULL, allocate private data and initialize defaults
 *              for the given codec. It is illegal to then call avcodec_open2()
 *              with a different codec.
 *              If NULL, then the codec-specific defaults won't be initialized,
 *              which may result in suboptimal default settings (this is
 *              important mainly for encoders, e.g. libx264).
 *
 * @return An AVCodecContext filled with default values or NULL on failure.
 *)
function avcodec_alloc_context3(const codec: PAVCodec): PAVCodecContext; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_alloc_context3';

(**
 * Free the codec context and everything associated with it and write NULL to
 * the provided pointer.
 *)
procedure avcodec_free_context(avctx: PPAVCodecContext); cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_free_context';

{$IFDEF FF_API_GET_CONTEXT_DEFAULTS}
(**
 * @deprecated This function should not be used, as closing and opening a codec
 * context multiple time is not supported. A new codec context should be
 * allocated for each new use.
 *)
function avcodec_get_context_defaults3(s: PAVCodecContext; const codec: PAVCodec): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_get_context_defaults3';
{$ENDIF}

(**
 * Get the AVClass for AVCodecContext. It can be used in combination with
 * AV_OPT_SEARCH_FAKE_OBJ for examining options.
 *
 * @see av_opt_find().
 *)
function avcodec_get_class(): PAVClass; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_get_class';

{$IFDEF FF_API_COPY_CONTEXT}
(**
 * Get the AVClass for AVFrame. It can be used in combination with
 * AV_OPT_SEARCH_FAKE_OBJ for examining options.
 *
 * @see av_opt_find().
 *)
function avcodec_get_frame_class(): PAVClass; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_get_frame_class';

(**
 * Get the AVClass for AVSubtitleRect. It can be used in combination with
 * AV_OPT_SEARCH_FAKE_OBJ for examining options.
 *
 * @see av_opt_find().
 *)
function avcodec_get_subtitle_rect_class(): PAVClass; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_get_subtitle_rect_class';

(**
 * Copy the settings of the source AVCodecContext into the destination
 * AVCodecContext. The resulting destination codec context will be
 * unopened, i.e. you are required to call avcodec_open2() before you
 * can use this AVCodecContext to decode/encode video/audio data.
 *
 * @param dest target codec context, should be initialized with
 *             avcodec_alloc_context3(NULL), but otherwise uninitialized
 * @param src source codec context
 * @return AVERROR() on error (e.g. memory allocation error), 0 on success
 *
 * @deprecated The semantics of this function are ill-defined and it should not
 * be used. If you need to transfer the stream parameters from one codec context
 * to another, use an intermediate AVCodecParameters instance and the
 * avcodec_parameters_from_context() / avcodec_parameters_to_context()
 * functions.
 *)
function avcodec_copy_context(dest: PAVCodecContext; const src: PAVCodecContext): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_copy_context';
{$ENDIF}

(**
 * Allocate a new AVCodecParameters and set its fields to default values
 * (unknown/invalid/0). The returned struct must be freed with
 * avcodec_parameters_free().
 *)
function avcodec_parameters_alloc: PAVCodecParameters; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_parameters_alloc';

(**
 * Free an AVCodecParameters instance and everything associated with it and
 * write NULL to the supplied pointer.
 *)
procedure avcodec_parameters_free(par: PPAVCodecParameters); cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_parameters_free';

(**
 * Copy the contents of src to dst. Any allocated fields in dst are freed and
 * replaced with newly allocated duplicates of the corresponding fields in src.
 *
 * @return >= 0 on success, a negative AVERROR code on failure.
 *)
function avcodec_parameters_copy(dst: PAVCodecParameters; const src: PAVCodecParameters): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_parameters_copy';

(**
 * Fill the parameters struct based on the values from the supplied codec
 * context. Any allocated fields in par are freed and replaced with duplicates
 * of the corresponding fields in codec.
 *
 * @return >= 0 on success, a negative AVERROR code on failure
 *)
function avcodec_parameters_from_context(par: PAVCodecParameters; const codec: PAVCodecContext): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_parameters_from_context';

(**
 * Fill the codec context based on the values from the supplied codec
 * parameters. Any allocated fields in codec that have a corresponding field in
 * par are freed and replaced with duplicates of the corresponding field in par.
 * Fields in codec that do not have a counterpart in par are not touched.
 *
 * @return >= 0 on success, a negative AVERROR code on failure.
 *)
function avcodec_parameters_to_context(codec: PAVCodecContext; const par: PAVCodecParameters): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_parameters_to_context';

(**
 * Initialize the AVCodecContext to use the given AVCodec. Prior to using this
 * function the context has to be allocated with avcodec_alloc_context3().
 *
 * The functions avcodec_find_decoder_by_name(), avcodec_find_encoder_by_name(),
 * avcodec_find_decoder() and avcodec_find_encoder() provide an easy way for
 * retrieving a codec.
 *
 * @warning This function is not thread safe!
 *
 * @note Always call this function before using decoding routines (such as
 * @ref avcodec_receive_frame()).
 *
 * @code
 * avcodec_register_all();
 * av_dict_set(&opts, "b", "2.5M", 0);
 * codec = avcodec_find_decoder(AV_CODEC_ID_H264);
 * if (!codec)
 *     exit(1);
 *
 * context = avcodec_alloc_context3(codec);
 *
 * if (avcodec_open2(context, codec, opts) < 0)
 *     exit(1);
 * @endcode
 *
 * @param avctx The context to initialize.
 * @param codec The codec to open this context for. If a non-NULL codec has been
 *              previously passed to avcodec_alloc_context3() or
 *              for this context, then this parameter MUST be either NULL or
 *              equal to the previously passed codec.
 * @param options A dictionary filled with AVCodecContext and codec-private options.
 *                On return this object will be filled with options that were not found.
 *
 * @return zero on success, a negative value on error
 * @see avcodec_alloc_context3(), avcodec_find_decoder(), avcodec_find_encoder(),
 *      av_dict_set(), av_opt_find().
 *)
function avcodec_open2(avctx: PAVCodecContext; const codec: PAVCodec; options: PPAVDictionary): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_open2';

(**
 * Close a given AVCodecContext and free all the data associated with it
 * (but not the AVCodecContext itself).
 *
 * Calling this function on an AVCodecContext that hasn't been opened will free
 * the codec-specific data allocated in avcodec_alloc_context3() with a non-NULL
 * codec. Subsequent calls will do nothing.
 *
 * @note Do not use this function. Use avcodec_free_context() to destroy a
 * codec context (either open or closed). Opening and closing a codec context
 * multiple times is not supported anymore -- use multiple codec contexts
 * instead.
 *)
function avcodec_close(avctx: PAVCodecContext): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_close';

(**
 * Free all allocated data in the given subtitle struct.
 *
 * @param sub AVSubtitle to free.
 *)
procedure avsubtitle_free(sub: PAVSubtitle); cdecl; external AVCODEC_LIBNAME name _PU + 'avsubtitle_free';

(**
 * @}
 *)

(**
 * @addtogroup lavc_packet
 * @{
 *)

(**
 * Allocate an AVPacket and set its fields to default values.  The resulting
 * struct must be freed using av_packet_free().
 *
 * @return An AVPacket filled with default values or NULL on failure.
 *
 * @note this only allocates the AVPacket itself, not the data buffers. Those
 * must be allocated through other means such as av_new_packet.
 *
 * @see av_new_packet
 *)
function av_packet_alloc: PAVPacket; cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_alloc';

(**
 * Create a new packet that references the same data as src.
 *
 * This is a shortcut for av_packet_alloc()+av_packet_ref().
 *
 * @return newly created AVPacket on success, NULL on error.
 *
 * @see av_packet_alloc
 * @see av_packet_ref
 *)
function av_packet_clone(const src: PAVPacket): PAVPacket; cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_clone';

(**
 * Free the packet, if the packet is reference counted, it will be
 * unreferenced first.
 *
 * @param packet packet to be freed. The pointer will be set to NULL.
 * @note passing NULL is a no-op.
 *)
procedure av_packet_free(pkt: PPAVPacket); cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_free';

(**
 * Initialize optional fields of a packet with default values.
 *
 * Note, this does not touch the data and size members, which have to be
 * initialized separately.
 *
 * @param pkt packet
 *)
procedure av_init_packet(pkt: PAVPacket); cdecl; external AVCODEC_LIBNAME name _PU + 'av_init_packet';

(**
 * Allocate the payload of a packet and initialize its fields with
 * default values.
 *
 * @param pkt packet
 * @param size wanted payload size
 * @return 0 if OK, AVERROR_xxx otherwise
 *)
function av_new_packet(pkt: PAVPacket; size: Integer): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_new_packet';

(**
 * Reduce packet size, correctly zeroing padding
 *
 * @param pkt packet
 * @param size new size
 *)
procedure av_shrink_packet(pkt: PAVPacket; size: Integer); cdecl; external AVCODEC_LIBNAME name _PU + 'av_shrink_packet';

(**
 * Increase packet size, correctly zeroing padding
 *
 * @param pkt packet
 * @param grow_by number of bytes by which to increase the size of the packet
 *)
function av_grow_packet(pkt: PAVPacket; grow_by: Integer): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_grow_packet';

(**
 * Initialize a reference-counted packet from av_malloc()ed data.
 *
 * @param pkt packet to be initialized. This function will set the data, size,
 *        buf and destruct fields, all others are left untouched.
 * @param data Data allocated by av_malloc() to be used as packet data. If this
 *        function returns successfully, the data is owned by the underlying AVBuffer.
 *        The caller may not access the data through other means.
 * @param size size of data in bytes, without the padding. I.e. the full buffer
 *        size is assumed to be size + AV_INPUT_BUFFER_PADDING_SIZE.
 *
 * @return 0 on success, a negative AVERROR on error
 *)
function av_packet_from_data(pkt: PAVPacket; data: PByte; size: Integer): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_from_data';

{$IFDEF FF_API_AVPACKET_OLD_API}
(**
 * @warning This is a hack - the packet memory allocation stuff is broken. The
 * packet is allocated if it was not really allocated.
 *
 * @deprecated Use av_packet_ref
 *)
function av_dup_packet(pkt: PAVPacket): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_dup_packet';

(**
 * Copy packet, including contents
 *
 * @return 0 on success, negative AVERROR on fail
 *)
function av_copy_packet(dst: PAVPacket; const src: PAVPacket): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_copy_packet';

(**
 * Copy packet side data
 *
 * @return 0 on success, negative AVERROR on fail
 *)
function av_copy_packet_side_data(dst: PAVPacket; const src: PAVPacket): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_copy_packet_side_data';

(**
 * Free a packet.
 *
 * @deprecated Use av_packet_unref
 *
 * @param pkt packet to free
 *)
procedure av_free_packet(pkt: PAVPacket); cdecl; external AVCODEC_LIBNAME name _PU + 'av_free_packet';
{$ENDIF}

(**
 * Allocate new information of a packet.
 *
 * @param pkt packet
 * @param type side information type
 * @param size side information size
 * @return pointer to fresh allocated data or NULL otherwise
 *)
function av_packet_new_side_data(pkt: PAVPacket; type_: TAVPacketSideDataType;
                                 size: Integer): PByte; cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_new_side_data';

(**
 * Wrap an existing array as a packet side data.
 *
 * @param pkt packet
 * @param type side information type
 * @param data the side data array. It must be allocated with the av_malloc()
 *             family of functions. The ownership of the data is transferred to
 *             pkt.
 * @param size side information size
 * @return a non-negative number on success, a negative AVERROR code on
 *         failure. On failure, the packet is unchanged and the data remains
 *         owned by the caller.
 *)
function av_packet_add_side_data(pkt: PAVPacket; type_: TAVPacketSideDataType;
                            data: PByte; size: Cardinal): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_add_side_data';

(**
 * Shrink the already allocated side data buffer
 *
 * @param pkt packet
 * @param type side information type
 * @param size new side information size
 * @return 0 on success, < 0 on failure
 *)
function av_packet_shrink_side_data(pkt: PAVPacket; ttype: TAVPacketSideDataType;
                               size: Integer): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_shrink_side_data';

(**
 * Get side information from packet.
 *
 * @param pkt packet
 * @param type desired side information type
 * @param size pointer for side information size to store (optional)
 * @return pointer to data if present or NULL otherwise
 *)
function av_packet_get_side_data(const pkt: PAVPacket; type_: TAVPacketSideDataType;
                                 size: PInteger): PByte; cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_get_side_data';

{$IFDEF FF_API_MERGE_SD_API}
function av_packet_merge_side_data(pkt: PAVPacket): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_merge_side_data';

function av_packet_split_side_data(pkt: PAVPacket): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_split_side_data';
{$ENDIF}

function av_packet_side_data_name(type_: TAVPacketSideDataType): PAnsiChar; cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_side_data_name';

(**
 * Pack a dictionary for use in side_data.
 *
 * @param dict The dictionary to pack.
 * @param size pointer to store the size of the returned data
 * @return pointer to data if successful, NULL otherwise
 *)
function av_packet_pack_dictionary(dict: PAVDictionary; size: PInteger): PByte; cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_pack_dictionary';
(**
 * Unpack a dictionary from side_data.
 *
 * @param data data from side_data
 * @param size size of the data
 * @param dict the metadata storage dictionary
 * @return 0 on success, < 0 on failure
 *)
function av_packet_unpack_dictionary(const data: PByte; size: Integer; dict: PPAVDictionary): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_unpack_dictionary';


(**
 * Convenience function to free all the side data stored.
 * All the other fields stay untouched.
 *
 * @param pkt packet
 *)
procedure av_packet_free_side_data(pkt: PAVPacket); cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_free_side_data';

(**
 * Setup a new reference to the data described by a given packet
 *
 * If src is reference-counted, setup dst as a new reference to the
 * buffer in src. Otherwise allocate a new buffer in dst and copy the
 * data from src into it.
 *
 * All the other fields are copied from src.
 *
 * @see av_packet_unref
 *
 * @param dst Destination packet
 * @param src Source packet
 *
 * @return 0 on success, a negative AVERROR on error.
 *)
function av_packet_ref(dst: PAVPacket; const src: PAVPacket): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_ref';

(**
 * Wipe the packet.
 *
 * Unreference the buffer referenced by the packet and reset the
 * remaining packet fields to their default values.
 *
 * @param pkt The packet to be unreferenced.
 *)
procedure av_packet_unref(pkt: PAVPacket); cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_unref';

(**
 * Move every field in src to dst and reset src.
 *
 * @see av_packet_unref
 *
 * @param src Source packet, will be reset
 * @param dst Destination packet
 *)
procedure av_packet_move_ref(dst, src: PAVPacket); cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_move_ref';

(**
 * Copy only "properties" fields from src to dst.
 *
 * Properties for the purpose of this function are all the fields
 * beside those related to the packet data (buf, data, size)
 *
 * @param dst Destination packet
 * @param src Source packet
 *
 * @return 0 on success AVERROR on failure.
 *)
function av_packet_copy_props(dst: PAVPacket; const src: PAVPacket): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_copy_props';

(**
 * Convert valid timing fields (timestamps / durations) in a packet from one
 * timebase to another. Timestamps with unknown values (AV_NOPTS_VALUE) will be
 * ignored.
 *
 * @param pkt packet on which the conversion will be performed
 * @param tb_src source timebase, in which the timing fields in pkt are
 *               expressed
 * @param tb_dst destination timebase, to which the timing fields will be
 *               converted
 *)
procedure av_packet_rescale_ts(pkt: PAVPacket; tb_src, tb_dst: TAVRational); cdecl; external AVCODEC_LIBNAME name _PU + 'av_packet_rescale_ts';

(**
 * @}
 *)

(**
 * @addtogroup lavc_decoding
 * @{
 *)

(**
 * Find a registered decoder with a matching codec ID.
 *
 * @param id AVCodecID of the requested decoder
 * @return A decoder if one was found, NULL otherwise.
 *)
function avcodec_find_decoder(id: TAVCodecID): PAVCodec; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_find_decoder';

(**
 * Find a registered decoder with the specified name.
 *
 * @param name name of the requested decoder
 * @return A decoder if one was found, NULL otherwise.
 *)
function avcodec_find_decoder_by_name(const name: PAnsiChar): PAVCodec; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_find_decoder_by_name';

(**
 * The default callback for AVCodecContext.get_buffer2(). It is made public so
 * it can be called by custom get_buffer2() implementations for decoders without
 * AV_CODEC_CAP_DR1 set.
 *)
function avcodec_default_get_buffer2(s: PAVCodecContext; frame: PAVFrame; flags: Integer): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_default_get_buffer2';

{$IFDEF FF_API_EMU_EDGE}
(**
 * Return the amount of padding in pixels which the get_buffer callback must
 * provide around the edge of the image for codecs which do not have the
 * CODEC_FLAG_EMU_EDGE flag.
 *
 * @return Required padding in pixels.
 *
 * @deprecated CODEC_FLAG_EMU_EDGE is deprecated, so this function is no longer
 * needed
 *)
function avcodec_get_edge_width: Cardinal; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_get_edge_width';
{$ENDIF}

(**
 * Modify width and height values so that they will result in a memory
 * buffer that is acceptable for the codec if you do not use any horizontal
 * padding.
 *
 * May only be used if a codec with AV_CODEC_CAP_DR1 has been opened.
 *)
procedure avcodec_align_dimensions(s: PAVCodecContext; width, height: PInteger); cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_align_dimensions';

(**
 * Modify width and height values so that they will result in a memory
 * buffer that is acceptable for the codec if you also ensure that all
 * line sizes are a multiple of the respective linesize_align[i].
 *
 * May only be used if a codec with AV_CODEC_CAP_DR1 has been opened.
 *)
procedure avcodec_align_dimensions2(s: PAVCodecContext; width, height: PInteger;
                               linesize_align: PInteger{int linesize_align[AV_NUM_DATA_POINTERS]}); cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_align_dimensions2';

(**
 * Converts AVChromaLocation to swscale x/y chroma position.
 *
 * The positions represent the chroma (0,0) position in a coordinates system
 * with luma (0,0) representing the origin and luma(1,1) representing 256,256
 *
 * @param xpos  horizontal chroma sample position
 * @param ypos  vertical   chroma sample position
 *)
function avcodec_enum_to_chroma_pos(xpos, ypos: PInteger; pos: TAVChromaLocation): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_enum_to_chroma_pos';

(**
 * Converts swscale x/y chroma position to AVChromaLocation.
 *
 * The positions represent the chroma (0,0) position in a coordinates system
 * with luma (0,0) representing the origin and luma(1,1) representing 256,256
 *
 * @param xpos  horizontal chroma sample position
 * @param ypos  vertical   chroma sample position
 *)
function avcodec_chroma_pos_to_enum(xpos, ypos: Integer): TAVChromaLocation; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_chroma_pos_to_enum';

(**
 * Decode the audio frame of size avpkt->size from avpkt->data into frame.
 *
 * Some decoders may support multiple frames in a single AVPacket. Such
 * decoders would then just decode the first frame and the return value would be
 * less than the packet size. In this case, avcodec_decode_audio4 has to be
 * called again with an AVPacket containing the remaining data in order to
 * decode the second frame, etc...  Even if no frames are returned, the packet
 * needs to be fed to the decoder with remaining data until it is completely
 * consumed or an error occurs.
 *
 * Some decoders (those marked with AV_CODEC_CAP_DELAY) have a delay between input
 * and output. This means that for some packets they will not immediately
 * produce decoded output and need to be flushed at the end of decoding to get
 * all the decoded data. Flushing is done by calling this function with packets
 * with avpkt->data set to NULL and avpkt->size set to 0 until it stops
 * returning samples. It is safe to flush even those decoders that are not
 * marked with AV_CODEC_CAP_DELAY, then no samples will be returned.
 *
 * @warning The input buffer, avpkt->data must be AV_INPUT_BUFFER_PADDING_SIZE
 *          larger than the actual read bytes because some optimized bitstream
 *          readers read 32 or 64 bits at once and could read over the end.
 *
 * @note The AVCodecContext MUST have been opened with @ref avcodec_open2()
 * before packets may be fed to the decoder.
 *
 * @param      avctx the codec context
 * @param[out] frame The AVFrame in which to store decoded audio samples.
 *                   The decoder will allocate a buffer for the decoded frame by
 *                   calling the AVCodecContext.get_buffer2() callback.
 *                   When AVCodecContext.refcounted_frames is set to 1, the frame is
 *                   reference counted and the returned reference belongs to the
 *                   caller. The caller must release the frame using av_frame_unref()
 *                   when the frame is no longer needed. The caller may safely write
 *                   to the frame if av_frame_is_writable() returns 1.
 *                   When AVCodecContext.refcounted_frames is set to 0, the returned
 *                   reference belongs to the decoder and is valid only until the
 *                   next call to this function or until closing or flushing the
 *                   decoder. The caller may not write to it.
 * @param[out] got_frame_ptr Zero if no frame could be decoded, otherwise it is
 *                           non-zero. Note that this field being set to zero
 *                           does not mean that an error has occurred. For
 *                           decoders with AV_CODEC_CAP_DELAY set, no given decode
 *                           call is guaranteed to produce a frame.
 * @param[in]  avpkt The input AVPacket containing the input buffer.
 *                   At least avpkt->data and avpkt->size should be set. Some
 *                   decoders might also require additional fields to be set.
 * @return A negative error code is returned if an error occurred during
 *         decoding, otherwise the number of bytes consumed from the input
 *         AVPacket is returned.
 *
* @deprecated Use avcodec_send_packet() and avcodec_receive_frame().
 *)
function avcodec_decode_audio4(avctx: PAVCodecContext; frame: PAVFrame;
                          got_frame_ptr: PInteger; const avpkt: PAVPacket): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_decode_audio4';

(**
 * Decode the video frame of size avpkt->size from avpkt->data into picture.
 * Some decoders may support multiple frames in a single AVPacket, such
 * decoders would then just decode the first frame.
 *
 * @warning The input buffer must be FF_INPUT_BUFFER_PADDING_SIZE larger than
 * the actual read bytes because some optimized bitstream readers read 32 or 64
 * bits at once and could read over the end.
 *
 * @warning The end of the input buffer buf should be set to 0 to ensure that
 * no overreading happens for damaged MPEG streams.
 *
 * @note Codecs which have the CODEC_CAP_DELAY capability set have a delay
 * between input and output, these need to be fed with avpkt->data=NULL,
 * avpkt->size=0 at the end to return the remaining frames.
 *
 * @note The AVCodecContext MUST have been opened with @ref avcodec_open2()
 * before packets may be fed to the decoder.
 *
 * @param avctx the codec context
 * @param[out] picture The AVFrame in which the decoded video frame will be stored.
 *             Use av_frame_alloc() to get an AVFrame. The codec will
 *             allocate memory for the actual bitmap by calling the
 *             AVCodecContext.get_buffer2() callback.
 *             When AVCodecContext.refcounted_frames is set to 1, the frame is
 *             reference counted and the returned reference belongs to the
 *             caller. The caller must release the frame using av_frame_unref()
 *             when the frame is no longer needed. The caller may safely write
 *             to the frame if av_frame_is_writable() returns 1.
 *             When AVCodecContext.refcounted_frames is set to 0, the returned
 *             reference belongs to the decoder and is valid only until the
 *             next call to this function or until closing or flushing the
 *             decoder. The caller may not write to it.
 *
 * @param[in] avpkt The input AVPacket containing the input buffer.
 *            You can create such packet with av_init_packet() and by then setting
 *            data and size, some decoders might in addition need other fields like
 *            flags&AV_PKT_FLAG_KEY. All decoders are designed to use the least
 *            fields possible.
 * @param[in,out] got_picture_ptr Zero if no frame could be decompressed, otherwise, it is nonzero.
 * @return On error a negative value is returned, otherwise the number of bytes
 * used or zero if no frame could be decompressed.
 *
 * @deprecated Use avcodec_send_packet() and avcodec_receive_frame().
 *)
function avcodec_decode_video2(avctx: PAVCodecContext; picture: PAVFrame;
                         got_picture_ptr: PInteger;
                         const avpkt: PAVPacket): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_decode_video2';

(**
 * Decode a subtitle message.
 * Return a negative value on error, otherwise return the number of bytes used.
 * If no subtitle could be decompressed, got_sub_ptr is zero.
 * Otherwise, the subtitle is stored in *sub.
 * Note that AV_CODEC_CAP_DR1 is not available for subtitle codecs. This is for
 * simplicity, because the performance difference is expect to be negligible
 * and reusing a get_buffer written for video codecs would probably perform badly
 * due to a potentially very different allocation pattern.
 *
 * Some decoders (those marked with CODEC_CAP_DELAY) have a delay between input
 * and output. This means that for some packets they will not immediately
 * produce decoded output and need to be flushed at the end of decoding to get
 * all the decoded data. Flushing is done by calling this function with packets
 * with avpkt->data set to NULL and avpkt->size set to 0 until it stops
 * returning subtitles. It is safe to flush even those decoders that are not
 * marked with CODEC_CAP_DELAY, then no subtitles will be returned.
 *
 * @note The AVCodecContext MUST have been opened with @ref avcodec_open2()
 * before packets may be fed to the decoder.
 *
 * @param avctx the codec context
 * @param[out] sub The Preallocated AVSubtitle in which the decoded subtitle will be stored,
 *                 must be freed with avsubtitle_free if *got_sub_ptr is set.
 * @param[in,out] got_sub_ptr Zero if no subtitle could be decompressed, otherwise, it is nonzero.
 * @param[in] avpkt The input AVPacket containing the input buffer.
 *)
function avcodec_decode_subtitle2(avctx: PAVCodecContext; sub: PAVSubtitle;
                            got_sub_ptr: PInteger;
                            avpkt: PAVPacket): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_decode_subtitle2';

(**
 * Supply raw packet data as input to a decoder.
 *
 * Internally, this call will copy relevant AVCodecContext fields, which can
 * influence decoding per-packet, and apply them when the packet is actually
 * decoded. (For example AVCodecContext.skip_frame, which might direct the
 * decoder to drop the frame contained by the packet sent with this function.)
 *
 * @warning The input buffer, avpkt->data must be AV_INPUT_BUFFER_PADDING_SIZE
 *          larger than the actual read bytes because some optimized bitstream
 *          readers read 32 or 64 bits at once and could read over the end.
 *
 * @warning Do not mix this API with the legacy API (like avcodec_decode_video2())
 *          on the same AVCodecContext. It will return unexpected results now
 *          or in future libavcodec versions.
 *
 * @note The AVCodecContext MUST have been opened with @ref avcodec_open2()
 *       before packets may be fed to the decoder.
 *
 * @param avctx codec context
 * @param[in] avpkt The input AVPacket. Usually, this will be a single video
 *                  frame, or several complete audio frames.
 *                  Ownership of the packet remains with the caller, and the
 *                  decoder will not write to the packet. The decoder may create
 *                  a reference to the packet data (or copy it if the packet is
 *                  not reference-counted).
 *                  Unlike with older APIs, the packet is always fully consumed,
 *                  and if it contains multiple frames (e.g. some audio codecs),
 *                  will require you to call avcodec_receive_frame() multiple
 *                  times afterwards before you can send a new packet.
 *                  It can be NULL (or an AVPacket with data set to NULL and
 *                  size set to 0); in this case, it is considered a flush
 *                  packet, which signals the end of the stream. Sending the
 *                  first flush packet will return success. Subsequent ones are
 *                  unnecessary and will return AVERROR_EOF. If the decoder
 *                  still has frames buffered, it will return them after sending
 *                  a flush packet.
 *
 * @return 0 on success, otherwise negative error code:
 *      AVERROR(EAGAIN):   input is not accepted in the current state - user
 *                         must read output with avcodec_receive_frame() (once
 *                         all output is read, the packet should be resent, and
 *                         the call will not fail with EAGAIN).
 *      AVERROR_EOF:       the decoder has been flushed, and no new packets can
 *                         be sent to it (also returned if more than 1 flush
 *                         packet is sent)
 *      AVERROR(EINVAL):   codec not opened, it is an encoder, or requires flush
 *      AVERROR(ENOMEM):   failed to add packet to internal queue, or similar
 *      other errors: legitimate decoding errors
 *)
function avcodec_send_packet(avctx: PAVCodecContext; const avpkt: PAVPacket): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_send_packet';

(**
 * Return decoded output data from a decoder.
 *
 * @param avctx codec context
 * @param frame This will be set to a reference-counted video or audio
 *              frame (depending on the decoder type) allocated by the
 *              decoder. Note that the function will always call
 *              av_frame_unref(frame) before doing anything else.
 *
 * @return
 *      0:                 success, a frame was returned
 *      AVERROR(EAGAIN):   output is not available in this state - user must try
 *                         to send new input
 *      AVERROR_EOF:       the decoder has been fully flushed, and there will be
 *                         no more output frames
 *      AVERROR(EINVAL):   codec not opened, or it is an encoder
 *      other negative values: legitimate decoding errors
 *)
function avcodec_receive_frame(avctx: PAVCodecContext; frame: PAVFrame): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_receive_frame';

(**
 * Supply a raw video or audio frame to the encoder. Use avcodec_receive_packet()
 * to retrieve buffered output packets.
 *
 * @param avctx     codec context
 * @param[in] frame AVFrame containing the raw audio or video frame to be encoded.
 *                  Ownership of the frame remains with the caller, and the
 *                  encoder will not write to the frame. The encoder may create
 *                  a reference to the frame data (or copy it if the frame is
 *                  not reference-counted).
 *                  It can be NULL, in which case it is considered a flush
 *                  packet.  This signals the end of the stream. If the encoder
 *                  still has packets buffered, it will return them after this
 *                  call. Once flushing mode has been entered, additional flush
 *                  packets are ignored, and sending frames will return
 *                  AVERROR_EOF.
 *
 *                  For audio:
 *                  If AV_CODEC_CAP_VARIABLE_FRAME_SIZE is set, then each frame
 *                  can have any number of samples.
 *                  If it is not set, frame->nb_samples must be equal to
 *                  avctx->frame_size for all frames except the last.
 *                  The final frame may be smaller than avctx->frame_size.
 * @return 0 on success, otherwise negative error code:
 *      AVERROR(EAGAIN):   input is not accepted in the current state - user
 *                         must read output with avcodec_receive_packet() (once
 *                         all output is read, the packet should be resent, and
 *                         the call will not fail with EAGAIN).
 *      AVERROR_EOF:       the encoder has been flushed, and no new frames can
 *                         be sent to it
 *      AVERROR(EINVAL):   codec not opened, refcounted_frames not set, it is a
 *                         decoder, or requires flush
 *      AVERROR(ENOMEM):   failed to add packet to internal queue, or similar
 *      other errors: legitimate decoding errors
 *)
function avcodec_send_frame(avctx: PAVCodecContext; const frame: PAVFrame): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_send_frame';

(**
 * Read encoded data from the encoder.
 *
 * @param avctx codec context
 * @param avpkt This will be set to a reference-counted packet allocated by the
 *              encoder. Note that the function will always call
 *              av_frame_unref(frame) before doing anything else.
 * @return 0 on success, otherwise negative error code:
 *      AVERROR(EAGAIN):   output is not available in the current state - user
 *                         must try to send input
 *      AVERROR_EOF:       the encoder has been fully flushed, and there will be
 *                         no more output packets
 *      AVERROR(EINVAL):   codec not opened, or it is an encoder
 *      other errors: legitimate decoding errors
 *)
function avcodec_receive_packet(avctx: PAVCodecContext; avpkt: PAVPacket): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_receive_packet';


(**
 * @defgroup lavc_parsing Frame parsing
 * @{
 *)

const
  AV_PARSER_PTS_NB = 4;
  PARSER_FLAG_COMPLETE_FRAMES = $0001;
  PARSER_FLAG_ONCE            = $0002;
  PARSER_FLAG_FETCHED_OFFSET  = $0004;
  PARSER_FLAG_USE_CODEC_TS    = $1000;

type
  TAVPictureStructure = (
    AV_PICTURE_STRUCTURE_UNKNOWN,      //< unknown
    AV_PICTURE_STRUCTURE_TOP_FIELD,    //< coded as top field
    AV_PICTURE_STRUCTURE_BOTTOM_FIELD, //< coded as bottom field
    AV_PICTURE_STRUCTURE_FRAME         //< coded as frame
  );

  PAVCodecParserContext = ^TAVCodecParserContext;
  PAVCodecParser = ^TAVCodecParser;
  TAVCodecParserContext = record
    priv_data: Pointer;
    parser: PAVCodecParser;
    frame_offset: Int64; (* offset of the current frame *)
    cur_offset: Int64; (* current offset
                           (incremented by each av_parser_parse()) *)
    next_frame_offset: Int64; (* offset of the next frame *)
    (* video info *)
    pict_type: Integer; (* XXX: Put it back in AVCodecContext. *)
    (**
     * This field is used for proper frame duration computation in lavf.
     * It signals, how much longer the frame duration of the current frame
     * is compared to normal frame duration.
     *
     * frame_duration = (1 + repeat_pict) * time_base
     *
     * It is used by codecs like H.264 to display telecined material.
     *)
    repeat_pict: Integer; (* XXX: Put it back in AVCodecContext. *)
    pts: Int64;     (* pts of the current frame *)
    dts: Int64;     (* dts of the current frame *)

    (* private data *)
    last_pts: Int64;
    last_dts: Int64;
    fetch_timestamp: Integer;

//#define AV_PARSER_PTS_NB 4
    cur_frame_start_index: Integer;
    cur_frame_offset: array[0..AV_PARSER_PTS_NB-1] of Int64;
    cur_frame_pts: array[0..AV_PARSER_PTS_NB-1] of Int64;
    cur_frame_dts: array[0..AV_PARSER_PTS_NB-1] of Int64;

    flags: Integer;
//#define PARSER_FLAG_COMPLETE_FRAMES           0x0001
//#define PARSER_FLAG_ONCE                      0x0002
/// Set if the parser has a valid file offset
//#define PARSER_FLAG_FETCHED_OFFSET            0x0004
//#define PARSER_FLAG_USE_CODEC_TS              0x1000

    offset: Int64;      ///< byte offset from starting packet start
    cur_frame_end: array[0..AV_PARSER_PTS_NB-1] of Int64;

    (**
     * Set by parser to 1 for key frames and 0 for non-key frames.
     * It is initialized to -1, so if the parser doesn't set this flag,
     * old-style fallback using AV_PICTURE_TYPE_I picture type as key frames
     * will be used.
     *)
    key_frame: Integer;

{$IFDEF FF_API_CONVERGENCE_DURATION}
    (**
     * @deprecated unused
     *)
    convergence_duration: Int64;
{$ENDIF}

    // Timestamp generation support:
    (**
     * Synchronization point for start of timestamp generation.
     *
     * Set to >0 for sync point, 0 for no sync point and <0 for undefined
     * (default).
     *
     * For example, this corresponds to presence of H.264 buffering period
     * SEI message.
     *)
    dts_sync_point: Integer;

    (**
     * Offset of the current timestamp against last timestamp sync point in
     * units of AVCodecContext.time_base.
     *
     * Set to INT_MIN when dts_sync_point unused. Otherwise, it must
     * contain a valid timestamp offset.
     *
     * Note that the timestamp of sync point has usually a nonzero
     * dts_ref_dts_delta, which refers to the previous sync point. Offset of
     * the next frame after timestamp sync point will be usually 1.
     *
     * For example, this corresponds to H.264 cpb_removal_delay.
     *)
    dts_ref_dts_delta: Integer;

    (**
     * Presentation delay of current frame in units of AVCodecContext.time_base.
     *
     * Set to INT_MIN when dts_sync_point unused. Otherwise, it must
     * contain valid non-negative timestamp delta (presentation time of a frame
     * must not lie in the past).
     *
     * This delay represents the difference between decoding and presentation
     * time of the frame.
     *
     * For example, this corresponds to H.264 dpb_output_delay.
     *)
    pts_dts_delta: Integer;

    (**
     * Position of the packet in file.
     *
     * Analogous to cur_frame_pts/dts
     *)
    cur_frame_pos: array[0..AV_PARSER_PTS_NB-1] of Int64;

    (**
     * Byte position of currently parsed frame in stream.
     *)
    pos: Int64;

    (**
     * Previous frame byte position.
     *)
    last_pos: Int64;

    (**
     * Duration of the current frame.
     * For audio, this is in units of 1 / AVCodecContext.sample_rate.
     * For all other types, this is in units of AVCodecContext.time_base.
     *)
    duration: Integer;

    field_order: TAVFieldOrder;

    (**
     * Indicate whether a picture is coded as a frame, top field or bottom field.
     *
     * For example, H.264 field_pic_flag equal to 0 corresponds to
     * AV_PICTURE_STRUCTURE_FRAME. An H.264 picture with field_pic_flag
     * equal to 1 and bottom_field_flag equal to 0 corresponds to
     * AV_PICTURE_STRUCTURE_TOP_FIELD.
     *)
    picture_structure: TAVPictureStructure;

    (**
     * Picture number incremented in presentation or output order.
     * This field may be reinitialized at the first picture of a new sequence.
     *
     * For example, this corresponds to H.264 PicOrderCnt.
     *)
    output_picture_number: Integer;

    (**
     * Dimensions of the decoded video intended for presentation.
     *)
    width: Integer;
    height: Integer;

    (**
     * Dimensions of the coded video.
     *)
    coded_width: Integer;
    coded_height: Integer;

    (**
     * The format of the coded data, corresponds to enum AVPixelFormat for video
     * and for enum AVSampleFormat for audio.
     *
     * Note that a decoder can have considerable freedom in how exactly it
     * decodes the data, so the format reported here might be different from the
     * one returned by a decoder.
     *)
    format: Integer;
  end;

  TAVCodecParser = record
    codec_ids: array[0..4] of Integer; (* several codec IDs are permitted *)
    priv_data_size: Integer;
    parser_init: function(s: PAVCodecParserContext): Integer; cdecl;
    (* This callback never returns an error, a negative value means that
     * the frame start was in a previous packet. *)
    parser_parse: function(s: PAVCodecParserContext;
                        avctx: PAVCodecContext;
                        const poutbuf: PPByte; poutbuf_size: PInteger;
                        const buf: PByte; buf_size: Integer): Integer; cdecl;
    parser_close: procedure(s: PAVCodecParserContext); cdecl;
    split: function(avctx: PAVCodecContext; const buf: PByte; buf_size: Integer): Integer; cdecl;
    next: PAVCodecParser;
  end;

function av_parser_next(const c: PAVCodecParser): PAVCodecParser; cdecl; external AVCODEC_LIBNAME name _PU + 'av_parser_next';

procedure av_register_codec_parser(parser: PAVCodecParser); cdecl; external AVCODEC_LIBNAME name _PU + 'av_register_codec_parser';
function av_parser_init(codec_id: Integer): PAVCodecParserContext; cdecl; external AVCODEC_LIBNAME name _PU + 'av_parser_init';

(**
 * Parse a packet.
 *
 * @param s             parser context.
 * @param avctx         codec context.
 * @param poutbuf       set to pointer to parsed buffer or NULL if not yet finished.
 * @param poutbuf_size  set to size of parsed buffer or zero if not yet finished.
 * @param buf           input buffer.
 * @param buf_size      buffer size in bytes without the padding. I.e. the full buffer
                        size is assumed to be buf_size + AV_INPUT_BUFFER_PADDING_SIZE.
                        To signal EOF, this should be 0 (so that the last frame
                        can be output).
 * @param pts           input presentation timestamp.
 * @param dts           input decoding timestamp.
 * @param pos           input byte position in stream.
 * @return the number of bytes of the input bitstream used.
 *
 * Example:
 * @code
 *   while(in_len){
 *       len = av_parser_parse2(myparser, AVCodecContext, &data, &size,
 *                                        in_data, in_len,
 *                                        pts, dts, pos);
 *       in_data += len;
 *       in_len  -= len;
 *
 *       if(size)
 *          decode_frame(data, size);
 *   }
 * @endcode
 *)
function av_parser_parse2(s: PAVCodecParserContext;
                     avctx: PAVCodecContext;
                     poutbuf: PPByte; poutbuf_size: PInteger;
                     const buf: PByte; buf_size: Integer;
                     pts, dts,
                     pos: Int64): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_parser_parse2';

(**
 * @return 0 if the output buffer is a subset of the input, 1 if it is allocated and must be freed
 * @deprecated use AVBitStreamFilter
 *)
function av_parser_change(s: PAVCodecParserContext;
                     avctx: PAVCodecContext;
                     poutbuf: PPByte; poutbuf_size: PInteger;
                     const buf: PByte; buf_size, keyframe: Integer): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_parser_change';
procedure av_parser_close(s: PAVCodecParserContext); cdecl; external AVCODEC_LIBNAME name _PU + 'av_parser_close';

(**
 * @}
 * @}
 *)

(**
 * @addtogroup lavc_encoding
 * @{
 *)

(**
 * Find a registered encoder with a matching codec ID.
 *
 * @param id AVCodecID of the requested encoder
 * @return An encoder if one was found, NULL otherwise.
 *)
function avcodec_find_encoder(id: TAVCodecID): PAVCodec; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_find_encoder';

(**
 * Find a registered encoder with the specified name.
 *
 * @param name name of the requested encoder
 * @return An encoder if one was found, NULL otherwise.
 *)
function avcodec_find_encoder_by_name(const name: PAnsiChar): PAVCodec; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_find_encoder_by_name';

(**
 * Encode a frame of audio.
 *
 * Takes input samples from frame and writes the next output packet, if
 * available, to avpkt. The output packet does not necessarily contain data for
 * the most recent frame, as encoders can delay, split, and combine input frames
 * internally as needed.
 *
 * @param avctx     codec context
 * @param avpkt     output AVPacket.
 *                  The user can supply an output buffer by setting
 *                  avpkt->data and avpkt->size prior to calling the
 *                  function, but if the size of the user-provided data is not
 *                  large enough, encoding will fail. If avpkt->data and
 *                  avpkt->size are set, avpkt->destruct must also be set. All
 *                  other AVPacket fields will be reset by the encoder using
 *                  av_init_packet(). If avpkt->data is NULL, the encoder will
 *                  allocate it. The encoder will set avpkt->size to the size
 *                  of the output packet.
 *
 *                  If this function fails or produces no output, avpkt will be
 *                  freed using av_packet_unref().
 * @param[in] frame AVFrame containing the raw audio data to be encoded.
 *                  May be NULL when flushing an encoder that has the
 *                  AV_CODEC_CAP_DELAY capability set.
 *                  If AV_CODEC_CAP_VARIABLE_FRAME_SIZE is set, then each frame
 *                  can have any number of samples.
 *                  If it is not set, frame->nb_samples must be equal to
 *                  avctx->frame_size for all frames except the last.
 *                  The final frame may be smaller than avctx->frame_size.
 * @param[out] got_packet_ptr This field is set to 1 by libavcodec if the
 *                            output packet is non-empty, and to 0 if it is
 *                            empty. If the function returns an error, the
 *                            packet can be assumed to be invalid, and the
 *                            value of got_packet_ptr is undefined and should
 *                            not be used.
 * @return          0 on success, negative error code on failure
 *
 * @deprecated use avcodec_send_frame()/avcodec_receive_packet() instead
 *)
function avcodec_encode_audio2(avctx: PAVCodecContext; avpkt: PAVPacket;
                          const frame: PAVFrame; got_packet_ptr: PInteger): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_encode_audio2';

(**
 * Encode a frame of video.
 *
 * Takes input raw video data from frame and writes the next output packet, if
 * available, to avpkt. The output packet does not necessarily contain data for
 * the most recent frame, as encoders can delay and reorder input frames
 * internally as needed.
 *
 * @param avctx     codec context
 * @param avpkt     output AVPacket.
 *                  The user can supply an output buffer by setting
 *                  avpkt->data and avpkt->size prior to calling the
 *                  function, but if the size of the user-provided data is not
 *                  large enough, encoding will fail. All other AVPacket fields
 *                  will be reset by the encoder using av_init_packet(). If
 *                  avpkt->data is NULL, the encoder will allocate it.
 *                  The encoder will set avpkt->size to the size of the
 *                  output packet. The returned data (if any) belongs to the
 *                  caller, he is responsible for freeing it.
 *
 *                  If this function fails or produces no output, avpkt will be
 *                  freed using av_packet_unref().
 * @param[in] frame AVFrame containing the raw video data to be encoded.
 *                  May be NULL when flushing an encoder that has the
 *                  AV_CODEC_CAP_DELAY capability set.
 * @param[out] got_packet_ptr This field is set to 1 by libavcodec if the
 *                            output packet is non-empty, and to 0 if it is
 *                            empty. If the function returns an error, the
 *                            packet can be assumed to be invalid, and the
 *                            value of got_packet_ptr is undefined and should
 *                            not be used.
 * @return          0 on success, negative error code on failure
 *
 * @deprecated use avcodec_send_frame()/avcodec_receive_packet() instead
 *)
function avcodec_encode_video2(avctx: PAVCodecContext; avpkt: PAVPacket;
                          const frame: PAVFrame; got_packet_ptr: PInteger): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_encode_video2';

function avcodec_encode_subtitle(avctx: PAVCodecContext; buf: PByte; buf_size: Integer;
                            const sub: PAVSubtitle): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_encode_subtitle';


(**
 * @}
 *)

{$IFDEF FF_API_AVCODEC_RESAMPLE}
(**
 * @defgroup lavc_resample Audio resampling
 * @ingroup libavc
 * @deprecated use libswresample instead
 *
 * @{
 *)
type
  PReSampleContext = ^TReSampleContext;
  TReSampleContext = record
    // need {$ALIGN 8}
    // defined in libavcodec/resample.c
  end;
  PAVResampleContext = ^TAVResampleContext;
  TAVResampleContext = record
    // need {$ALIGN 8}
    // defined in libavcodec/resample2.c
  end;

(**
 *  Initialize audio resampling context.
 *
 * @param output_channels  number of output channels
 * @param input_channels   number of input channels
 * @param output_rate      output sample rate
 * @param input_rate       input sample rate
 * @param sample_fmt_out   requested output sample format
 * @param sample_fmt_in    input sample format
 * @param filter_length    length of each FIR filter in the filterbank relative to the cutoff frequency
 * @param log2_phase_count log2 of the number of entries in the polyphase filterbank
 * @param linear           if 1 then the used FIR filter will be linearly interpolated
                           between the 2 closest, if 0 the closest will be used
 * @param cutoff           cutoff frequency, 1.0 corresponds to half the output sampling rate
 * @return allocated ReSampleContext, NULL if error occurred
 *)
function av_audio_resample_init(output_channels, input_channels,
                                        output_rate, input_rate: Integer;
                                        sample_fmt_out, sample_fmt_in: TAVSampleFormat;
                                        filter_length, log2_phase_count,
                                        linear: Integer; cutoff: Double): PReSampleContext; cdecl; external AVCODEC_LIBNAME name _PU + 'av_audio_resample_init';
function audio_resample(s: PReSampleContext; output: PSmallInt; input: PSmallInt; nb_samples: Integer): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'audio_resample';

(**
 * Free resample context.
 *
 * @param s a non-NULL pointer to a resample context previously
 *          created with av_audio_resample_init()
 *)
procedure audio_resample_close(s: PReSampleContext); cdecl; external AVCODEC_LIBNAME name _PU + 'audio_resample_close';

(**
 * Initialize an audio resampler.
 * Note, if either rate is not an integer then simply scale both rates up so they are.
 * @param filter_length length of each FIR filter in the filterbank relative to the cutoff freq
 * @param log2_phase_count log2 of the number of entries in the polyphase filterbank
 * @param linear If 1 then the used FIR filter will be linearly interpolated
                 between the 2 closest, if 0 the closest will be used
 * @param cutoff cutoff frequency, 1.0 corresponds to half the output sampling rate
 *)
function av_resample_init(out_rate: Integer; in_rate: Integer; filter_length: Integer; log2_phase_count: Integer; linear: Integer; cutoff: Double): PAVResampleContext; cdecl; external AVCODEC_LIBNAME name _PU + 'av_resample_init';

(**
 * Resample an array of samples using a previously configured context.
 * @param src an array of unconsumed samples
 * @param consumed the number of samples of src which have been consumed are returned here
 * @param src_size the number of unconsumed samples available
 * @param dst_size the amount of space in samples available in dst
 * @param update_ctx If this is 0 then the context will not be modified, that way several channels can be resampled with the same context.
 * @return the number of samples written in dst or -1 if an error occurred
 *)
function av_resample(c: PAVResampleContext; dst: PSmallInt; src: PSmallInt; consumed: PInteger; src_size: Integer; dst_size: Integer; update_ctx: Integer): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_resample';

(**
 * Compensate samplerate/timestamp drift. The compensation is done by changing
 * the resampler parameters, so no audible clicks or similar distortions occur
 * @param compensation_distance distance in output samples over which the compensation should be performed
 * @param sample_delta number of output samples which should be output less
 *
 * example: av_resample_compensate(c, 10, 500)
 * here instead of 510 samples only 500 samples would be output
 *
 * note, due to rounding the actual compensation might be slightly different,
 * especially if the compensation_distance is large and the in_rate used during init is small
 *)
procedure av_resample_compensate(c: PAVResampleContext; sample_delta: Integer; compensation_distance: Integer); cdecl; external AVCODEC_LIBNAME name _PU + 'av_resample_compensate';
procedure av_resample_close(c: PAVResampleContext); cdecl; external AVCODEC_LIBNAME name _PU + 'av_resample_close';

(**
 * @}
 *)
{$ENDIF}

{$IFDEF FF_API_AVPICTURE}
(**
 * @addtogroup lavc_picture
 * @{
 *)

(**
 * @deprecated unused
 *)
function avpicture_alloc(picture: PAVPicture; pix_fmt: TAVPixelFormat; width, height: Integer): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'avpicture_alloc';

(**
 * @deprecated unused
 *)
procedure avpicture_free(picture: PAVPicture); cdecl; external AVCODEC_LIBNAME name _PU + 'avpicture_free';

(**
 * @deprecated use av_image_fill_arrays() instead.
 *)
function avpicture_fill(picture: PAVPicture; const ptr: PByte;
                   pix_fmt: TAVPixelFormat; width, height: Integer): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'avpicture_fill';

(**
 * @deprecated use av_image_copy_to_buffer() instead.
 *)
function avpicture_layout(const src: PAVPicture; pix_fmt: TAVPixelFormat;
                     width, height: Integer;
                     dest: PByte; dest_size: Integer): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'avpicture_layout';

(**
 * @deprecated use av_image_get_buffer_size() instead.
 *)
function avpicture_get_size(pix_fmt: TAVPixelFormat; width, height: Integer): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'avpicture_get_size';

(**
 * @deprecated av_image_copy() instead.
 *)
procedure av_picture_copy(dst: PAVPicture; const src: PAVPicture;
              pix_fmt: TAVPixelFormat; width, height: Integer); cdecl; external AVCODEC_LIBNAME name _PU + 'av_picture_copy';

(**
 * @deprecated unused
 *)
function av_picture_crop(dst: PAVPicture; const src: PAVPicture;
             pix_fmt: TAVPixelFormat; top_band, left_band: Integer): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_picture_crop';

(**
 * @deprecated unused
 *)
function av_picture_pad(dst: PAVPicture; const src: PAVPicture; height, width: Integer; pix_fmt: TAVPixelFormat{Integer};
            padtop, padbottom, padleft, padright: Integer; color: PInteger): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_picture_pad';

(**
 * @}
 *)
{$ENDIF}

(**
 * @defgroup lavc_misc Utility functions
 * @ingroup libavc
 *
 * Miscellaneous utility functions related to both encoding and decoding
 * (or neither).
 * @{
 *)

(**
 * @defgroup lavc_misc_pixfmt Pixel formats
 *
 * Functions for working with pixel formats.
 * @{
 *)

(**
 * Utility function to access log2_chroma_w log2_chroma_h from
 * the pixel format AVPixFmtDescriptor.
 *
 * This function asserts that pix_fmt is valid. See av_pix_fmt_get_chroma_sub_sample
 * for one that returns a failure code and continues in case of invalid
 * pix_fmts.
 *
 * @param[in]  pix_fmt the pixel format
 * @param[out] h_shift store log2_chroma_w
 * @param[out] v_shift store log2_chroma_h
 *
 * @see av_pix_fmt_get_chroma_sub_sample
 *)
procedure avcodec_get_chroma_sub_sample(pix_fmt: TAVPixelFormat; h_shift, v_shift: PInteger); cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_get_chroma_sub_sample';

(**
 * Return a value representing the fourCC code associated to the
 * pixel format pix_fmt, or 0 if no associated fourCC code can be
 * found.
 *)
function avcodec_pix_fmt_to_codec_tag(pix_fmt: TAVPixelFormat): Cardinal; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_pix_fmt_to_codec_tag';

(**
 * @deprecated see av_get_pix_fmt_loss()
 *)
function avcodec_get_pix_fmt_loss(dst_pix_fmt, src_pix_fmt: TAVPixelFormat;
                             has_alpha: Integer): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_get_pix_fmt_loss';

(**
 * Find the best pixel format to convert to given a certain source pixel
 * format.  When converting from one pixel format to another, information loss
 * may occur.  For example, when converting from RGB24 to GRAY, the color
 * information will be lost. Similarly, other losses occur when converting from
 * some formats to other formats. avcodec_find_best_pix_fmt_of_2() searches which of
 * the given pixel formats should be used to suffer the least amount of loss.
 * The pixel formats from which it chooses one, are determined by the
 * pix_fmt_list parameter.
 *
 *
 * @param[in] pix_fmt_list AV_PIX_FMT_NONE terminated array of pixel formats to choose from
 * @param[in] src_pix_fmt source pixel format
 * @param[in] has_alpha Whether the source pixel format alpha channel is used.
 * @param[out] loss_ptr Combination of flags informing you what kind of losses will occur.
 * @return The best pixel format to convert to or -1 if none was found.
 *)
function avcodec_find_best_pix_fmt_of_list(const pix_fmt_list: PAVPixelFormat;
                                            src_pix_fmt: TAVPixelFormat;
                                            has_alpha: Integer; loss_ptr: PInteger): TAVPixelFormat; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_find_best_pix_fmt_of_list';

(**
 * @deprecated see av_find_best_pix_fmt_of_2()
 *)
function avcodec_find_best_pix_fmt_of_2(dst_pix_fmt1, dst_pix_fmt2, src_pix_fmt: TAVPixelFormat;
                                                  has_alpha: Integer; loss_ptr: PInteger): TAVPixelFormat; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_find_best_pix_fmt_of_2';

function avcodec_find_best_pix_fmt2(dst_pix_fmt1, dst_pix_fmt2, src_pix_fmt: TAVPixelFormat;
                        has_alpha: Integer; loss_ptr: PInteger): TAVPixelFormat; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_find_best_pix_fmt2';

function avcodec_default_get_format(s: PAVCodecContext; const fmt: PAVPixelFormat): TAVPixelFormat; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_default_get_format';

(**
 * @}
 *)

{$IFDEF FF_API_SET_DIMENSIONS}
(**
 * @deprecated this function is not supposed to be used from outside of lavc
 *)
procedure avcodec_set_dimensions(s: PAVCodecContext; width, height: Integer); cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_set_dimensions';
{$ENDIF}

{$IFDEF FF_API_TAG_STRING}
(**
 * Put a string representing the codec tag codec_tag in buf.
 *
 * @param buf       buffer to place codec tag in
 * @param buf_size size in bytes of buf
 * @param codec_tag codec tag to assign
 * @return the length of the string that would have been generated if
 * enough space had been available, excluding the trailing null
 *
 * @deprecated see av_fourcc_make_string() and av_fourcc2str().
 *)
function av_get_codec_tag_string(buf: PAnsiChar; buf_size, codec_tag: Cardinal): Cardinal; cdecl; external AVCODEC_LIBNAME name _PU + 'av_get_codec_tag_string';
{$ENDIF}

procedure avcodec_string(buf: PAnsiChar; buf_size: Integer; enc: PAVCodecContext; encode: Integer); cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_string';

(**
 * Return a name for the specified profile, if available.
 *
 * @param codec the codec that is searched for the given profile
 * @param profile the profile value for which a name is requested
 * @return A name for the profile if found, NULL otherwise.
 *)
function av_get_profile_name(const codec: PAVCodec; profile: Integer): PAnsiChar; cdecl; external AVCODEC_LIBNAME name _PU + 'av_get_profile_name';

(**
 * Return a name for the specified profile, if available.
 *
 * @param codec_id the ID of the codec to which the requested profile belongs
 * @param profile the profile value for which a name is requested
 * @return A name for the profile if found, NULL otherwise.
 *
 * @note unlike av_get_profile_name(), which searches a list of profiles
 *       supported by a specific decoder or encoder implementation, this
 *       function searches the list of profiles from the AVCodecDescriptor
 *)
function avcodec_profile_name(codec_id: TAVCodecID; profile: Integer): PAnsiChar; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_profile_name';

function avcodec_default_execute(c: PAVCodecContext; func: TexecuteCall; arg: Pointer; ret: PInteger; count, size: Integer): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_default_execute';
function avcodec_default_execute2(c: PAVCodecContext; func: Texecute2Call; arg: Pointer; ret: PInteger; count: Integer): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_default_execute2';
//FIXME func typedef

(**
 * Fill AVFrame audio data and linesize pointers.
 *
 * The buffer buf must be a preallocated buffer with a size big enough
 * to contain the specified samples amount. The filled AVFrame data
 * pointers will point to this buffer.
 *
 * AVFrame extended_data channel pointers are allocated if necessary for
 * planar audio.
 *
 * @param frame       the AVFrame
 *                    frame->nb_samples must be set prior to calling the
 *                    function. This function fills in frame->data,
 *                    frame->extended_data, frame->linesize[0].
 * @param nb_channels channel count
 * @param sample_fmt  sample format
 * @param buf         buffer to use for frame data
 * @param buf_size    size of buffer
 * @param align       plane size sample alignment (0 = default)
 * @return            >=0 on success, negative error code on failure
 * @todo return the size in bytes required to store the samples in
 * case of success, at the next libavutil bump
 *)
function avcodec_fill_audio_frame(frame: PAVFrame; nb_channels: Integer;
                             sample_fmt: TAVSampleFormat; const buf: PByte;
                             buf_size, align: Integer): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_fill_audio_frame';

(**
 * Reset the internal decoder state / flush internal buffers. Should be called
 * e.g. when seeking or when switching to a different stream.
 *
 * @note when refcounted frames are not used (i.e. avctx->refcounted_frames is 0),
 * this invalidates the frames previously returned from the decoder. When
 * refcounted frames are used, the decoder just releases any references it might
 * keep internally, but the caller's reference remains valid.
 *)
procedure avcodec_flush_buffers(avctx: PAVCodecContext); cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_flush_buffers';

(**
 * Return codec bits per sample.
 *
 * @param[in] codec_id the codec
 * @return Number of bits per sample or zero if unknown for the given codec.
 *)
function av_get_bits_per_sample(codec_id: TAVCodecID): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_get_bits_per_sample';

(**
 * Return the PCM codec associated with a sample format.
 * @param be  endianness, 0 for little, 1 for big,
 *            -1 (or anything else) for native
 * @return  AV_CODEC_ID_PCM_* or AV_CODEC_ID_NONE
 *)
function av_get_pcm_codec(fmt: TAVSampleFormat; be: Integer): TAVCodecID; cdecl; external AVCODEC_LIBNAME name _PU + 'av_get_pcm_codec';

(**
 * Return codec bits per sample.
 * Only return non-zero if the bits per sample is exactly correct, not an
 * approximation.
 *
 * @param[in] codec_id the codec
 * @return Number of bits per sample or zero if unknown for the given codec.
 *)
function av_get_exact_bits_per_sample(codec_id: TAVCodecID): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_get_exact_bits_per_sample';

(**
 * Return audio frame duration.
 *
 * @param avctx        codec context
 * @param frame_bytes  size of the frame, or 0 if unknown
 * @return             frame duration, in samples, if known. 0 if not able to
 *                     determine.
 *)
function av_get_audio_frame_duration(avctx: PAVCodecContext; frame_bytes: Integer): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_get_audio_frame_duration';

(**
 * This function is the same as av_get_audio_frame_duration(), except it works
 * with AVCodecParameters instead of an AVCodecContext.
 *)
function av_get_audio_frame_duration2(par: PAVCodecParameters; frame_bytes: Integer): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_get_audio_frame_duration2';

type
{$IFDEF FF_API_OLD_BSF}
  PPAVBitStreamFilterContext = ^PAVBitStreamFilterContext;
  PAVBitStreamFilterContext = ^TAVBitStreamFilterContext;
  PAVBitStreamFilter = ^TAVBitStreamFilter;
  TAVBitStreamFilterContext = record
    priv_data: Pointer;
    filter: PAVBitStreamFilter;
    parser: PAVCodecParserContext;
    next: PAVBitStreamFilterContext;
    (**
     * Internal default arguments, used if NULL is passed to av_bitstream_filter_filter().
     * Not for access by library users.
     *)
    args: PAnsiChar;
  end;
{$ENDIF}

  PAVBSFInternal = ^TAVBSFInternal;
  TAVBSFInternal = record
    // need {$ALIGN 8}
    // defined in libavcodec/bsf.c
  end;

(**
 * The bitstream filter state.
 *
 * This struct must be allocated with av_bsf_alloc() and freed with
 * av_bsf_free().
 *
 * The fields in the struct will only be changed (by the caller or by the
 * filter) as described in their documentation, and are to be considered
 * immutable otherwise.
 *)
  PPAVBSFContext = ^PAVBSFContext;
  PAVBSFContext = ^TAVBSFContext;
  TAVBSFContext = record
    (**
     * A class for logging and AVOptions
     *)
    av_class: PAVClass;

    (**
     * The bitstream filter this context is an instance of.
     *)
    filter: PAVBitStreamFilter;

    (**
     * Opaque libavcodec internal data. Must not be touched by the caller in any
     * way.
     *)
    internal: PAVBSFInternal;

    (**
     * Opaque filter-specific private data. If filter->priv_class is non-NULL,
     * this is an AVOptions-enabled struct.
     *)
    priv_data: Pointer;

    (**
     * Parameters of the input stream. This field is allocated in
     * av_bsf_alloc(), it needs to be filled by the caller before
     * av_bsf_init().
     *)
    par_in: PAVCodecParameters;

    (**
     * Parameters of the output stream. This field is allocated in
     * av_bsf_alloc(), it is set by the filter in av_bsf_init().
     *)
    par_out: PAVCodecParameters;

    (**
     * The timebase used for the timestamps of the input packets. Set by the
     * caller before av_bsf_init().
     *)
    time_base_in: TAVRational;

    (**
     * The timebase used for the timestamps of the output packets. Set by the
     * filter in av_bsf_init().
     *)
    time_base_out: TAVRational;
  end;

  TAVBitStreamFilter = record
    name: PAnsiChar;

    (**
     * A list of codec ids supported by the filter, terminated by
     * AV_CODEC_ID_NONE.
     * May be NULL, in that case the bitstream filter works with any codec id.
     *)
    codec_ids: PAVCodecID;

    (**
     * A class for the private data, used to declare bitstream filter private
     * AVOptions. This field is NULL for bitstream filters that do not declare
     * any options.
     *
     * If this field is non-NULL, the first member of the filter private data
     * must be a pointer to AVClass, which will be set by libavcodec generic
     * code to this class.
     *)
    priv_class: PAVClass;

    (*****************************************************************
     * No fields below this line are part of the public API. They
     * may not be used outside of libavcodec and can be changed and
     * removed at will.
     * New public fields should be added right above.
     *****************************************************************
     *)

    priv_data_size: Integer;
    init: function(ctx: PAVBSFContext): Integer; cdecl;
    filter: function(ctx: PAVBSFContext; pkt: PAVPacket): Integer; cdecl;
    close: procedure(ctx: PAVBSFContext); cdecl;
  end;

{$IFDEF FF_API_OLD_BSF}
(**
 * Register a bitstream filter.
 *
 * The filter will be accessible to the application code through
 * av_bitstream_filter_next() or can be directly initialized with
 * av_bitstream_filter_init().
 *
 * @see avcodec_register_all()
 *)
procedure av_register_bitstream_filter(bsf: PAVBitStreamFilter); cdecl; external AVCODEC_LIBNAME name _PU + 'av_register_bitstream_filter';

(**
 * Create and initialize a bitstream filter context given a bitstream
 * filter name.
 *
 * The returned context must be freed with av_bitstream_filter_close().
 *
 * @param name    the name of the bitstream filter
 * @return a bitstream filter context if a matching filter was found
 * and successfully initialized, NULL otherwise
 *)
function av_bitstream_filter_init(const name: PAnsiChar): PAVBitStreamFilterContext; cdecl; external AVCODEC_LIBNAME name _PU + 'av_bitstream_filter_init';

(**
 * Filter bitstream.
 *
 * This function filters the buffer buf with size buf_size, and places the
 * filtered buffer in the buffer pointed to by poutbuf.
 *
 * The output buffer must be freed by the caller.
 *
 * @param bsfc            bitstream filter context created by av_bitstream_filter_init()
 * @param avctx           AVCodecContext accessed by the filter, may be NULL.
 *                        If specified, this must point to the encoder context of the
 *                        output stream the packet is sent to.
 * @param args            arguments which specify the filter configuration, may be NULL
 * @param poutbuf         pointer which is updated to point to the filtered buffer
 * @param poutbuf_size    pointer which is updated to the filtered buffer size in bytes
 * @param buf             buffer containing the data to filter
 * @param buf_size        size in bytes of buf
 * @param keyframe        set to non-zero if the buffer to filter corresponds to a key-frame packet data
 * @return >= 0 in case of success, or a negative error code in case of failure
 *
 * If the return value is positive, an output buffer is allocated and
 * is available in *poutbuf, and is distinct from the input buffer.
 *
 * If the return value is 0, the output buffer is not allocated and
 * should be considered identical to the input buffer, or in case
 * *poutbuf was set it points to the input buffer (not necessarily to
 * its starting address). A special case is if *poutbuf was set to NULL and
 * *poutbuf_size was set to 0, which indicates the packet should be dropped.
 *)
function av_bitstream_filter_filter(bsfc: PAVBitStreamFilterContext;
                               avctx: PAVCodecContext; const args: PAnsiChar;
                               poutbuf: PPByte; poutbuf_size: PInteger;
                               const buf: PByte; buf_size, keyframe: Integer): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_bitstream_filter_filter';

(**
 * Release bitstream filter context.
 *
 * @param bsf the bitstream filter context created with
 * av_bitstream_filter_init(), can be NULL
 *)
procedure av_bitstream_filter_close(bsf: PAVBitStreamFilterContext); cdecl; external AVCODEC_LIBNAME name _PU + 'av_bitstream_filter_close';

(**
 * If f is NULL, return the first registered bitstream filter,
 * if f is non-NULL, return the next registered bitstream filter
 * after f, or NULL if f is the last one.
 *
 * This function can be used to iterate over all registered bitstream
 * filters.
 *)
function av_bitstream_filter_next(const f: PAVBitStreamFilter): PAVBitStreamFilter; cdecl; external AVCODEC_LIBNAME name _PU + 'av_bitstream_filter_next';
{$ENDIF}

(**
 * @return a bitstream filter with the specified name or NULL if no such
 *         bitstream filter exists.
 *)
function av_bsf_get_by_name(const name: PAnsiChar): PAVBitStreamFilter; cdecl; external AVCODEC_LIBNAME name _PU + 'av_bsf_get_by_name';

(**
 * Iterate over all registered bitstream filters.
 *
 * @param opaque a pointer where libavcodec will store the iteration state. Must
 *               point to NULL to start the iteration.
 *
 * @return the next registered bitstream filter or NULL when the iteration is
 *         finished
 *)
function av_bsf_next(opaque: PPointer): PAVBitStreamFilter; cdecl; external AVCODEC_LIBNAME name _PU + 'av_bsf_next';

(**
 * Allocate a context for a given bitstream filter. The caller must fill in the
 * context parameters as described in the documentation and then call
 * av_bsf_init() before sending any data to the filter.
 *
 * @param filter the filter for which to allocate an instance.
 * @param ctx a pointer into which the pointer to the newly-allocated context
 *            will be written. It must be freed with av_bsf_free() after the
 *            filtering is done.
 *
 * @return 0 on success, a negative AVERROR code on failure
 *)
function av_bsf_alloc(const filter: PAVBitStreamFilter; ctx: PPAVBSFContext): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_bsf_alloc';

(**
 * Prepare the filter for use, after all the parameters and options have been
 * set.
 *)
function av_bsf_init(ctx: PAVBSFContext): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_bsf_init';

(**
 * Submit a packet for filtering.
 *
 * After sending each packet, the filter must be completely drained by calling
 * av_bsf_receive_packet() repeatedly until it returns AVERROR(EAGAIN) or
 * AVERROR_EOF.
 *
 * @param pkt the packet to filter. pkt must contain some payload (i.e data or
 * side data must be present in pkt). The bitstream filter will take ownership of
 * the packet and reset the contents of pkt. pkt is not touched if an error occurs.
 * This parameter may be NULL, which signals the end of the stream (i.e. no more
 * packets will be sent). That will cause the filter to output any packets it
 * may have buffered internally.
 *
 * @return 0 on success, a negative AVERROR on error.
 *)
function av_bsf_send_packet(ctx: PAVBSFContext; pkt: PAVPacket): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_bsf_send_packet';

(**
 * Retrieve a filtered packet.
 *
 * @param[out] pkt this struct will be filled with the contents of the filtered
 *                 packet. It is owned by the caller and must be freed using
 *                 av_packet_unref() when it is no longer needed.
 *                 This parameter should be "clean" (i.e. freshly allocated
 *                 with av_packet_alloc() or unreffed with av_packet_unref())
 *                 when this function is called. If this function returns
 *                 successfully, the contents of pkt will be completely
 *                 overwritten by the returned data. On failure, pkt is not
 *                 touched.
 *
 * @return 0 on success. AVERROR(EAGAIN) if more packets need to be sent to the
 * filter (using av_bsf_send_packet()) to get more output. AVERROR_EOF if there
 * will be no further output from the filter. Another negative AVERROR value if
 * an error occurs.
 *
 * @note one input packet may result in several output packets, so after sending
 * a packet with av_bsf_send_packet(), this function needs to be called
 * repeatedly until it stops returning 0. It is also possible for a filter to
 * output fewer packets than were sent to it, so this function may return
 * AVERROR(EAGAIN) immediately after a successful av_bsf_send_packet() call.
 *)
function av_bsf_receive_packet(ctx: PAVBSFContext; pkt: PAVPacket): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_bsf_receive_packet';

(**
 * Free a bitstream filter context and everything associated with it; write NULL
 * into the supplied pointer.
 *)
procedure av_bsf_free(ctx: PPAVBSFContext); cdecl; external AVCODEC_LIBNAME name _PU + 'av_bsf_free';

(**
 * Get the AVClass for AVBSFContext. It can be used in combination with
 * AV_OPT_SEARCH_FAKE_OBJ for examining options.
 *
 * @see av_opt_find().
 *)
function av_bsf_get_class: PAVClass; cdecl; external AVCODEC_LIBNAME name _PU + 'av_bsf_get_class';

type
(**
 * Structure for chain/list of bitstream filters.
 * Empty list can be allocated by av_bsf_list_alloc().
 *)
  PPAVBSFList = ^PAVBSFList;
  PAVBSFList = ^TAVBSFList;
  TAVBSFList = record
    // need {$ALIGN 8}
    // defined in libavcodec/bsf.c
  end;

(**
 * Allocate empty list of bitstream filters.
 * The list must be later freed by av_bsf_list_free()
 * or finalized by av_bsf_list_finalize().
 *
 * @return Pointer to @ref AVBSFList on success, NULL in case of failure
 *)
function av_bsf_list_alloc: PAVBSFList; cdecl; external AVCODEC_LIBNAME name _PU + 'av_bsf_list_alloc';

(**
 * Free list of bitstream filters.
 *
 * @param lst Pointer to pointer returned by av_bsf_list_alloc()
 *)
procedure av_bsf_list_free(lst: PPAVBSFList); cdecl; external AVCODEC_LIBNAME name _PU + 'av_bsf_list_free';

(**
 * Append bitstream filter to the list of bitstream filters.
 *
 * @param lst List to append to
 * @param bsf Filter context to be appended
 *
 * @return >=0 on success, negative AVERROR in case of failure
 *)
function av_bsf_list_append(lst: PAVBSFList; bsf: PAVBSFContext): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_bsf_list_append';

(**
 * Construct new bitstream filter context given it's name and options
 * and append it to the list of bitstream filters.
 *
 * @param lst      List to append to
 * @param bsf_name Name of the bitstream filter
 * @param options  Options for the bitstream filter, can be set to NULL
 *
 * @return >=0 on success, negative AVERROR in case of failure
 *)
function av_bsf_list_append2(lst: PAVBSFList; const bsf_name: PAnsiChar; options: PPAVDictionary): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_bsf_list_append2';

(**
 * Finalize list of bitstream filters.
 *
 * This function will transform @ref AVBSFList to single @ref AVBSFContext,
 * so the whole chain of bitstream filters can be treated as single filter
 * freshly allocated by av_bsf_alloc().
 * If the call is successful, @ref AVBSFList structure is freed and lst
 * will be set to NULL. In case of failure, caller is responsible for
 * freeing the structure by av_bsf_list_free()
 *
 * @param      lst Filter list structure to be transformed
 * @param[out] bsf Pointer to be set to newly created @ref AVBSFContext structure
 *                 representing the chain of bitstream filters
 *
 * @return >=0 on success, negative AVERROR in case of failure
 *)
function av_bsf_list_finalize(lst: PPAVBSFList; bsf: PPAVBSFContext): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_bsf_list_finalize';

(**
 * Parse string describing list of bitstream filters and create single
 * @ref AVBSFContext describing the whole chain of bitstream filters.
 * Resulting @ref AVBSFContext can be treated as any other @ref AVBSFContext freshly
 * allocated by av_bsf_alloc().
 *
 * @param      str String describing chain of bitstream filters in format
 *                 `bsf1[=opt1=val1:opt2=val2][,bsf2]`
 * @param[out] bsf Pointer to be set to newly created @ref AVBSFContext structure
 *                 representing the chain of bitstream filters
 *
 * @return >=0 on success, negative AVERROR in case of failure
 *)
function av_bsf_list_parse_str(const str: PAnsiChar; bsf: PPAVBSFContext): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_bsf_list_parse_str';

(**
 * Get null/pass-through bitstream filter.
 *
 * @param[out] bsf Pointer to be set to new instance of pass-through bitstream filter
 *
 * @return
 *)
function av_bsf_get_null_filter(bsf: PPAVBSFContext): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_bsf_get_null_filter';

(* memory *)

(**
 * Same behaviour av_fast_malloc but the buffer has additional
 * AV_INPUT_BUFFER_PADDING_SIZE at the end which will always be 0.
 *
 * In addition the whole buffer will initially and after resizes
 * be 0-initialized so that no uninitialized data will ever appear.
 *)
procedure av_fast_padded_malloc(ptr: Pointer; size: PCardinal; min_size: Cardinal); cdecl; external AVCODEC_LIBNAME name _PU + 'av_fast_padded_malloc';

(**
 * Same behaviour av_fast_padded_malloc except that buffer will always
 * be 0-initialized after call.
 *)
procedure av_fast_padded_mallocz(ptr: Pointer; size: PCardinal; min_size: Cardinal); cdecl; external AVCODEC_LIBNAME name _PU + 'av_fast_padded_mallocz';

(**
 * Encode extradata length to a buffer. Used by xiph codecs.
 *
 * @param s buffer to write to; must be at least (v/255+1) bytes long
 * @param v size of extradata in bytes
 * @return number of bytes written to the buffer.
 *)
function av_xiphlacing(s: PAnsiChar; v: Cardinal): Cardinal; cdecl; external AVCODEC_LIBNAME name _PU + 'av_xiphlacing';

{$IFDEF FF_API_MISSING_SAMPLE}
(**
 * Log a generic warning message about a missing feature. This function is
 * intended to be used internally by FFmpeg (libavcodec, libavformat, etc.)
 * only, and would normally not be used by applications.
 * @param[in] avc a pointer to an arbitrary struct of which the first field is
 * a pointer to an AVClass struct
 * @param[in] feature string containing the name of the missing feature
 * @param[in] want_sample indicates if samples are wanted which exhibit this feature.
 * If want_sample is non-zero, additional verbiage will be added to the log
 * message which tells the user how to report samples to the development
 * mailing list.
 * @deprecated Use avpriv_report_missing_feature() instead.
 *)
procedure av_log_missing_feature(avc: Pointer; const feature: PAnsiChar; want_sample: Integer); cdecl; external AVCODEC_LIBNAME name _PU + 'av_log_missing_feature';

(**
 * Log a generic warning message asking for a sample. This function is
 * intended to be used internally by FFmpeg (libavcodec, libavformat, etc.)
 * only, and would normally not be used by applications.
 * @param[in] avc a pointer to an arbitrary struct of which the first field is
 * a pointer to an AVClass struct
 * @param[in] msg string containing an optional message, or NULL if no message
 * @deprecated Use avpriv_request_sample() instead.
 *)
  //void av_log_ask_for_sample(void *avc, const char *msg, ...) av_printf_format(2, 3);
procedure av_log_ask_for_sample(avc: Pointer; const msg: PAnsiChar); cdecl varargs; external AVCODEC_LIBNAME name _PU + 'av_log_ask_for_sample';
{$ENDIF}

(**
 * Register the hardware accelerator hwaccel.
 *)
procedure av_register_hwaccel(hwaccel: PAVHWAccel); cdecl; external AVCODEC_LIBNAME name _PU + 'av_register_hwaccel';

(**
 * If hwaccel is NULL, returns the first registered hardware accelerator,
 * if hwaccel is non-NULL, returns the next registered hardware accelerator
 * after hwaccel, or NULL if hwaccel is the last one.
 *)
function av_hwaccel_next(const hwaccel: PAVHWAccel): PAVHWAccel; cdecl; external AVCODEC_LIBNAME name _PU + 'av_hwaccel_next';


(**
 * Lock operation used by lockmgr
 *)
type
  TAVLockOp = (
    AV_LOCK_CREATE,  ///< Create a mutex
    AV_LOCK_OBTAIN,  ///< Lock the mutex
    AV_LOCK_RELEASE, ///< Unlock the mutex
    AV_LOCK_DESTROY  ///< Free mutex resources
  );

(**
 * Register a user provided lock manager supporting the operations
 * specified by AVLockOp. The "mutex" argument to the function points
 * to a (void * ) where the lockmgr should store/get a pointer to a user
 * allocated mutex. It is NULL upon AV_LOCK_CREATE and equal to the
 * value left by the last call for all other ops. If the lock manager is
 * unable to perform the op then it should leave the mutex in the same
 * state as when it was called and return a non-zero value. However,
 * when called with AV_LOCK_DESTROY the mutex will always be assumed to
 * have been successfully destroyed. If av_lockmgr_register succeeds
 * it will return a non-negative value, if it fails it will return a
 * negative value and destroy all mutex and unregister all callbacks.
 * av_lockmgr_register is not thread-safe, it must be called from a
 * single thread before any calls which make use of locking are used.
 *
 * @param cb User defined callback. av_lockmgr_register invokes calls
 *           to this callback and the previously registered callback.
 *           The callback will be used to create more than one mutex
 *           each of which must be backed by its own underlying locking
 *           mechanism (i.e. do not use a single static object to
 *           implement your lock manager). If cb is set to NULL the
 *           lockmgr will be unregistered.
 *)
  TlockmgrcbCall = function(mutex: PPointer; op: TAVLockOp): Integer; cdecl;
function av_lockmgr_register(cb: TlockmgrcbCall): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_lockmgr_register';

(**
 * Get the type of the given codec.
 *)
function avcodec_get_type(codec_id: TAVCodecID): TAVMediaType; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_get_type';

(**
 * Get the name of a codec.
 * @return  a static string identifying the codec; never NULL
 *)
function avcodec_get_name(id: TAVCodecID): PAnsiChar; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_get_name';

(**
 * @return a positive value if s is open (i.e. avcodec_open2() was called on it
 * with no corresponding avcodec_close()), 0 otherwise.
 *)
function avcodec_is_open(s: PAVCodecContext): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_is_open';

(**
 * @return a non-zero number if codec is an encoder, zero otherwise
 *)
function av_codec_is_encoder(const codec: PAVCodec): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_codec_is_encoder';

(**
 * @return a non-zero number if codec is a decoder, zero otherwise
 *)
function av_codec_is_decoder(const codec: PAVCodec): Integer; cdecl; external AVCODEC_LIBNAME name _PU + 'av_codec_is_decoder';

(**
 * @return descriptor for given codec ID or NULL if no descriptor exists.
 *)
function avcodec_descriptor_get(id: TAVCodecID): PAVCodecDescriptor; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_descriptor_get';

(**
 * Iterate over all codec descriptors known to libavcodec.
 *
 * @param prev previous descriptor. NULL to get the first descriptor.
 *
 * @return next descriptor or NULL after the last descriptor
 *)
function avcodec_descriptor_next(const prev: PAVCodecDescriptor): PAVCodecDescriptor; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_descriptor_next';

(**
 * @return codec descriptor with the given name or NULL if no such descriptor
 *         exists.
 *)
function avcodec_descriptor_get_by_name(const name: PAnsiChar): PAVCodecDescriptor; cdecl; external AVCODEC_LIBNAME name _PU + 'avcodec_descriptor_get_by_name';

(**
 * Allocate a CPB properties structure and initialize its fields to default
 * values.
 *
 * @param size if non-NULL, the size of the allocated struct will be written
 *             here. This is useful for embedding it in side data.
 *
 * @return the newly allocated struct or NULL on failure
 *)
function av_cpb_properties_alloc(size: Cardinal): PAVCPBProperties; cdecl; external AVCODEC_LIBNAME name _PU + 'av_cpb_properties_alloc';

(**
 * @}
 *)

(*
// libavutil/internal.h
#define MAKE_ACCESSORS(str, name, type, field) \
    type av_##name##_get_##field(const str *s) { return s->field; } \
    void av_##name##_set_##field(str *s, type v) { s->field = v; }
// libavcodec/utils.c
MAKE_ACCESSORS(AVCodecContext, codec, AVRational, pkt_timebase)
MAKE_ACCESSORS(AVCodecContext, codec, const AVCodecDescriptor *, codec_descriptor)
MAKE_ACCESSORS(AVCodecContext, codec, int, lowres)
MAKE_ACCESSORS(AVCodecContext, codec, int, seek_preroll)
MAKE_ACCESSORS(AVCodecContext, codec, uint16_t*, chroma_intra_matrix)
*)

function av_codec_get_pkt_timebase(const avctx: PAVCodecContext): TAVRational; {$IFDEF USE_INLINE}inline;{$ENDIF}

implementation

function av_codec_get_pkt_timebase(const avctx: PAVCodecContext): TAVRational;
begin
  Result := avctx.pkt_timebase;
end;

end.
