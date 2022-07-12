const fs = require('fs');
const path = require('path');
const deleteComment = /\/\*[a-zA-Z0-9\ \(\)\{\}\[\]\?\_\*\n\r\<\>\=\+\@\^\.\;\|\:\/\,\-\"\']{1,}?\*\/\n/g;
const getInclude = /#include ["'<](.*)["'>]\n/g;
const getIncludeIndex = /(#include ["'<].*["'>])\n/g;

var dirList  = new Map(), fileList = new Map();

async function readdirlink(dirPath = './'){
    let dirRoot = fs.opendirSync(dirPath);
    for await (const item of dirRoot) {
        if (item.isDirectory()){ dirList.set(path.join(dirPath,item.name),item); await readdirlink(path.join(dirPath,item.name).toString()) };
        if (item.isFile()){ fileList.set(path.join(dirPath,item.name),item); };
    }
}

async function generate2File(srcFile,desFile){
    let dataInFile = fs.readFileSync(srcFile).toString();
    for (const item of dataInFile.match(getInclude)) {
        console.log(`match => `,item)
    }
    /*console.log(dataInFile.match(getInclude));*/
}

(async () => {
    await readdirlink();
    console.log(`dir List ==========>>>> `,dirList);
    console.log(`file List =========>>>> `,fileList);
    await generate2File('./libavutil/avutil.h','./avutil.tmp');
})();

