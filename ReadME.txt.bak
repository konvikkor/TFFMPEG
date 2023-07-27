-----------------------------
SDL LIB/DLL
https://wiki.libsdl.org/SDL2/Installation
https://wiki.libsdl.org/SDL2/SourceCode
https://github.com/libsdl-org/SDL/releases/tag/release-2.26.3
-----------------------------
FFMPEG
https://www.gyan.dev/ffmpeg/builds/
release builds Нужно скачать полный комплект и от туда использовать DLL
https://ffmpeg.org/olddownload.html


https://github.com/libsdl-org/SDL/releases/tag/release-2.26.3
Install MSYS2 to a fixed folder (eg.: C:\Dev\msys64)
Run msys2.exe
Execute command "pacman -S make gcc diffutils" and press "Y" to install
Close msys2
Rename C:\Dev\msys64\usr\bin\link.exe to some other name (eg.: msys2_link.exe)
Copy and rename "yasm--win64.exe" to "C:\Dev\yasm.exe"
Add "C:\Dev" to environment variables PATH
Run VS2013/2015 x86 (for x86) or x64 for (x64) Command Prompt
Execute "C:\Dev\msys64\msys2_shell.cmd -msys -use-full-path"
On the msys2 window execute "which cl" and you should see the path of your VS
Execute "which link" and you should also see the path of your VS
Go to the ffmpeg source path (eg.: "cd /c/ffmpeg3.3")
Run ./configure and make


D:\+MyProjects\FFmpeg\
./configure \
    --toolchain=msvc \
    --arch=x86_64 \
    --enable-yasm \
    --enable-asm\
    --enable-shared \
    --enable-w32threads \
    --disable-programs \
    --disable-ffserver \
    --disable-doc \
    --disable-static \
    --prefix=/d/+MyProjects/TFFMPEG/out/Win64