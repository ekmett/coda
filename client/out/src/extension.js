'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const path = require("path");
const vscode_1 = require("vscode");
const vscode_languageclient_1 = require("vscode-languageclient");
function activate(context) {
    let serverPath = context.asAbsolutePath(path.join('bin', process.platform === 'win32' ? 'coda.exe' : 'coda.sh'));
    let serverOptions = {
        run: { command: serverPath, args: ['server'] },
        debug: { command: serverPath, args: ['server', '--debug'] }
    };
    let clientOptions = {
        documentSelector: ['coda'],
        synchronize: {
            configurationSection: 'coda',
            fileEvents: vscode_1.workspace.createFileSystemWatcher('**/.codarc')
        }
    };
    let disposable = new vscode_languageclient_1.LanguageClient('coda', 'coda language server', serverOptions, clientOptions).start();
    context.subscriptions.push(disposable);
}
exports.activate = activate;
//# sourceMappingURL=extension.js.map