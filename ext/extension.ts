'use strict';
import * as path from 'path';
import { workspace, Disposable, ExtensionContext } from 'vscode';
import { LanguageClient, LanguageClientOptions, SettingMonitor, ServerOptions } from 'vscode-languageclient';
export function activate(context: ExtensionContext) {
  let serverPath = context.asAbsolutePath(path.join('bin', process.platform == "win32" ? 'coda.exe' : "coda"));
  let serverOptions: ServerOptions = {
    run : { command: serverPath, args: ['--ide'] },
    debug: { command: serverPath, args: ['--ide', '--debug'] }
  };
  let clientOptions: LanguageClientOptions = {
    documentSelector: ['coda'],
    synchronize: {
      configurationSection: 'coda',
      fileEvents: workspace.createFileSystemWatcher('**/.codarc')
    }
  };
  let disposable = new LanguageClient('coda', 'coda language server', serverOptions, clientOptions).start();
  context.subscriptions.push(disposable);
}
