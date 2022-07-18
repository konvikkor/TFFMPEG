/**
 * 2022
 * @author konvikkor
 */

const EventEmitter = require('events');
const fs = require('fs');
const path = require('path');
const crypto = require("crypto");
const deleteComment = /\/\*[a-zA-Z0-9\ \(\)\{\}\[\]\?\_\*\n\r\<\>\=\+\@\^\.\;\|\:\/\,\%\-\"\'\`\\\&\#]{1,}?\*\/\n/g;
const deleteEmptyLine = /^[\r\n]$/gm;
const getListFiles = /(^#include ["'](?<localFile>.*)['"])|(^#include [\<](?<globalFile>.*)[\>])/gm
/*
Enum Step 1
(?<enum>^enum (?<name>\w{1,})[ \t\r\n]?{(?<value>[\r\n\\ A-Za-z0-9\_\!\@\#\$\%\^\&\*\(\)\_\+\|\[\]\=\,\.\<\>\/\-\;]{0,})})
Enum step 2
[ \t](?<param>\w{1,}) {0,}?\t{0,}=? {0,}?\t{0,}(?<value>\w{0,}),

MultiLine
/(?:^#define)? ?(?<name>(\w{1,}\([0-9a-zA-Z, ]{0,}\))|(\w{1,}))(?<value>[a-zA-Z0-9\,\\\_\(\)\[\]\-\= \,\.\t]{0,}\\$[ \r\nA-Za-z0-9\t_\(\)\[\],\\]{0,}[\r\n]?)/gm
SingleLine
/^#define ?(?<name>(\w{1,}\([0-9a-zA-Z, ]{0,}\))|(\w{1,})) *(?<value>.*$)(?<!\\$)/gm

if else endif DEFINED
^.+$ all lines
*/

class processHeaderFile extends EventEmitter {
    #fileData = ''; #filePath = ''; #fileDir = ''; id = crypto.randomBytes(16).toString("hex");
    includeFileList = [];
    /** @type {Map<String, String>}  */
    #dirList = new Map();
    /** @type {Map<String, processHeaderFile>} */
    #fileList = new Map();
    get filePath(){return this.#filePath}
    get data() { return this.#fileData; }
    /** @param {Map<String, String>} val */
    set dirList(val) { this.#dirList = val; }
    /** @param {Map<String, processHeaderFile>} val */
    set fileList(val) { this.#fileList = val; }
    /** @param {String} filePath */
    constructor(filePath) {super(); this.#fileData = fs.readFileSync(filePath); this.#filePath = filePath; this.#fileDir = path.dirname(filePath); }
    /** @returns {processHeaderFile} */
    delEmptyLine() {
        let tmpText = this.#fileData.toString().replace(deleteEmptyLine, '');
        this.#fileData = (new TextDecoder()).decode((new TextEncoder()).encode(tmpText));
        return this;
    }
    /** @returns {processHeaderFile} */
    delComment() {
        let tmpText = this.#fileData.toString().replace(deleteComment, '');
        console.log(`Delete Coment ${this.#filePath}`);
        this.#fileData = (new TextDecoder()).decode((new TextEncoder()).encode(tmpText));
        return this;
    }
    /** 
     * @param {Array<processHeaderFile>=} localFile
     * @param {Array<processHeaderFile>=} globalFile
     * @returns {processHeaderFile} this */
    async loadIncludeFile(includeFileList = this.includeFileList) {
        let tmpObject;
        /** Создание меток по найденым совпадениям
         * @param {RegExpExecArray} resultRegExp
         * @returns {{index:number,src:String,filename:String,global:Boolean,count:number}}*/
        function createPosition(resultRegExp) {
            return {
                index: resultRegExp.index,
                src: resultRegExp[0],
                filename: (resultRegExp?.groups?.localFile || resultRegExp?.groups?.globalFile),
                global: resultRegExp?.groups?.localFile ? true : false,
                count: 1
            }
        }
        let resultRegExp;
        getListFiles.lastIndex;
        do {
            resultRegExp = getListFiles.exec(this.#fileData.toString());
            if (!resultRegExp) { 
                getListFiles.lastIndex = resultRegExp?.index + (resultRegExp?.groups?.localFile?.length | resultRegExp?.groups?.globalFile?.length);
                break;
            }
            let tmpFileParam = createPosition(resultRegExp);
            /* Если будет найдены уже ранее загруженные то они должны будут удалиться из этого файла 
             * загрузка локальных и глобальных должна отличаться путями
             * голобальный подгружается по имени файла а локальный по относительному */
            let find = includeFileList.find((v) => { return v.filename == tmpFileParam.filename });
            if (find) { 
                let tmpText = (this.#fileData.toString()).replace(tmpFileParam.src, ''); this.#fileData = (new TextDecoder()).decode((new TextEncoder()).encode(tmpText));
                continue;
            };
            includeFileList.push(tmpFileParam);
            getListFiles.lastIndex = resultRegExp?.index + (resultRegExp?.groups?.localFile?.length | resultRegExp?.groups?.globalFile?.length);
        } while (resultRegExp?.groups);
        getListFiles.lastIndex;

        let tmpText = this.#fileData.toString();
        for (let index = 0; index < includeFileList.length; index++) {
            let item = includeFileList[index];
            let tmpPath = path.normalize(this.#fileDir + '/' + item.filename);
            console.log(`Read to ${tmpPath} path[${this.#fileDir}] file[${item.filename}] ${fs.existsSync(tmpPath)} ID:${this.id}`);
            if (fs.existsSync(tmpPath)) {
                tmpObject = this.#fileList.get(tmpPath);
                if (tmpObject.filePath != this.filePath){
                    await tmpObject.loadIncludeFile(includeFileList);
                    item.fileData = tmpObject.data.toString();
                    tmpText = tmpText.replace(item.src, '\r' + item.fileData + '\r');                    
                }
            }
        }
        this.#fileData = (new TextDecoder()).decode((new TextEncoder()).encode(tmpText));

        console.log(`localFile => `, localFile);
        console.log(`globalFile =>`, globalFile);
        return this;
    }
    save2file(fpath) {
        if (!fpath) { return }
        fs.writeFileSync(path.normalize(fpath), this.#fileData);
    }
}
/** @extends EventEmitter*/
class header2passFile extends EventEmitter {
    #listPath = ['./'];
    dirList = new Map();
    fileList = new Map();
    get listPath() { return this.listPath; };
    async indexFileList(dirPath = './') {
        let dirRoot = fs.opendirSync(dirPath);
        for await (const item of dirRoot) {
            if (item.isDirectory()) { this.dirList.set(path.join(dirPath, item.name), item.name); await this.indexFileList(path.join(dirPath, item.name).toString()) };
            if (item.isFile()) {
                let pfile = new processHeaderFile(path.join(dirPath, item.name));
                pfile.dirList = this.dirList;
                pfile.fileList = this.fileList;
                this.fileList.set(path.join(dirPath, item.name), pfile);
            };
        }
        return this;
    }
    /**Получение обработчика для файла
     * @param {String} filePath Путь до файла
     * @returns {processHeaderFile} */
    getFileProcess(filePath) { return this.fileList.get(filePath); }
    async #initPath() {
        for await (const item of this.#listPath) { await this.indexFileList(item); };
        this.emit(`init`, this);
    }
    /** @param {Array<String>} listPath */
    constructor(listPath) {
        super();
        for (const item of listPath) { this.#listPath.push(item); }
        this.#initPath();
    }
}

(async () => {
    let h2pass = new header2passFile(['C:\\Program Files (x86)\\Windows Kits\\10\\Include\\10.0.19041.0\\ucrt', `./`]);
    h2pass.on(`init`, /** @param {header2passFile} h2p */async (h2p) => {
        console.log(`dir List ==========>>>> `, h2p.dirList);
        console.log(`file List =========>>>> `, h2p.fileList);
        let pheaderFile = h2p.getFileProcess(path.normalize('./libavutil/avutil.h'));
        await pheaderFile.loadIncludeFile();
        console.log(`pheaderFile========================================================\n
        ${pheaderFile.data}`);
        pheaderFile.save2file(`./avutil.h`);
    });
    //await generate2File('./libavutil/avutil.h', './avutil.tmp');
})();

