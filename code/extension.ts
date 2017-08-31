'use strict';
import * as path from 'path';
import { workspace, Disposable, ExtensionContext } from 'vscode';
import { LanguageClient, LanguageClientOptions, SettingMonitor, ServerOptions } from 'vscode-languageclient';
export function activate(context: ExtensionContext) {
  let serverPath = context.asAbsolutePath(path.join('bin', process.platform === 'win32' ? 'coda.exe' : 'coda.sh'));
  let serverOptions: ServerOptions = {
    run : { command: serverPath, args: ['server'] },
    debug: { command: serverPath, args: ['server', '--debug'] }
  };
  let clientOptions: LanguageClientOptions = {
    documentSelector: ['coda'],
    synchronize: {
      configurationSection: 'coda',
      fileEvents: workspace.createFileSystemWatcher('**/.codarc')
    }
  };
  let disposable = new LanguageClient('coda', 'Coda Language Server', serverOptions, clientOptions).start();
  context.subscriptions.push(disposable);
}
