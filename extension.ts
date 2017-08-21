'use strict';
import * as path from 'path';
import { workspace, Disposable, ExtensionContext } from 'vscode';
import { LanguageClient, LanguageClientOptions, SettingMonitor, ServerOptions, TransportKind } from 'vscode-languageclient';
export function activate(context: ExtensionContext) {
  let serverModule = context.asAbsolutePath(path.join('bin','coda'));
  let serverOptions: ServerOptions = {
    run : { module: serverModule, transport: TransportKind.ipc, options: { execArgv: ["--server"] } },
    debug: { module: serverModule, transport: TransportKind.ipc, options: { execArgv: ["--server","--debug"] } }
  }
  let clientOptions: LanguageClientOptions = {
    documentSelector: ['coda'],
    synchronize: {
      configurationSection: 'coda',
      fileEvents: workspace.createFileSystemWatcher('**/.codarc')
    }
  }
  let disposable = new LanguageClient('coda', 'coda language server', serverOptions, clientOptions).start();
  context.subscriptions.push(disposable);
}
